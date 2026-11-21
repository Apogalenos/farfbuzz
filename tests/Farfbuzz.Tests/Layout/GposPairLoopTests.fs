module Farfbuzz.Tests.Layout.GposPairLoopTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.Shape.SimpleRunTests

/// <summary>The number of random cases of the synthetic property.</summary>
[<Literal>]
let private SyntheticCaseCount = 20000

/// <summary>The number of random cases of the real-font property.</summary>
[<Literal>]
let private RealCaseCount = 6000

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x6A17D3UL

/// <summary>The glyph ids of a synthetic table: the first few, and ids at the edge of what a pair key holds and beyond it.</summary>
let private bigGlyphs = [| 65533u; 65534u; 65535u; 70000u |]

/// <summary>The first value format and second value format of a synthetic subtable: mostly the advance of the first glyph, which
/// the pair loops decode, then every other shape of record, with placements, vertical advances, devices and second records.</summary>
let private formats =
    [| 4us, 0us; 4us, 0us; 4us, 0us; 4us, 0us; 4us, 0us; 4us, 0us; 4us, 4us; 0us, 4us; 5us, 0us; 1us, 0us; 2us, 0us; 8us, 0us
       0x44us, 0us; 0x14us, 0us; 0x4Fus, 0x0Fus; 0us, 0us; 4us, 1us; 0x0Cus, 0us; 0xF0us, 0us; 0x4Cus, 0x44us |]

/// <summary>The lookup flags of a synthetic lookup: mostly none, then the ones that skip glyph classes.</summary>
let private lookupFlags = [| 0u; 0u; 0u; 0u; 0u; 0u; 0x0002u; 0x0004u; 0x0008u; 0x000Au; 0x0010u |]

/// <summary>The masks of the lookups of a synthetic map: single bits, a pair, and none.</summary>
let private lookupMasks = [| 0x08u; 0x08u; 0x08u; 0x10u; 0x18u; 0x20u; 0u |]

/// <summary>A hinting device, whose delta at ppem 12 is not zero.</summary>
let private device = HintingDevice (11us, 13us, 1us, ReadOnlyMemory<byte> [| 0x10uy; 0uy |])

/// <summary>A value field of a record: zero often, so zero-valued pairs appear, else small, else a limit.</summary>
/// <param name="rnd">The random source.</param>
let private fieldValue (rnd: Random) =
    match rnd.Next 12 with
    | 0 | 1 | 2 | 3 -> 0s
    | 4 -> Int16.MaxValue
    | 5 -> Int16.MinValue
    | _ -> int16 (rnd.Next(-200, 201))

/// <summary>Builds the value fields and devices of a number of records of the two formats.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="format1">The first value format.</param>
/// <param name="format2">The second value format.</param>
/// <param name="records">The number of records.</param>
/// <returns>The fields, flat in file order, and the devices parallel to them.</returns>
let private recordFields (rnd: Random) (format1: uint16) (format2: uint16) (records: int) =
    let length = GposTables.valueLength format1 + GposTables.valueLength format2
    let deviceField (field: int) =
        let bits = if field < GposTables.valueLength format1 then format1 else format2
        let position = if field < GposTables.valueLength format1 then field else field - GposTables.valueLength format1
        let named = [| 1us; 2us; 4us; 8us; 0x10us; 0x20us; 0x40us; 0x80us |] |> Array.filter (fun bit -> bits &&& bit <> 0us)
        named[position] >= 0x10us
    let fields = Array.init (records * length) (fun index -> if deviceField (index % length) then (if rnd.Next 2 = 0 then 8s else 0s) else fieldValue rnd)
    let devices = fields |> Array.mapi (fun index value -> if deviceField (index % length) && value <> 0s then device else NoDevice)
    struct (ImmutableArray.CreateRange fields, ImmutableArray.CreateRange devices)

/// <summary>A direct table over a coverage or a class definition that answers what it answers, or an empty one that sends every
/// query to the structure.</summary>
/// <param name="answers">The answer for each glyph from zero to the last one the structure names.</param>
/// <param name="complete">Whether the table is complete.</param>
let private direct (answers: uint16 array) (complete: bool) =
    match complete with
    | true -> { Table = ImmutableArray.CreateRange answers; Complete = true }
    | false -> { Table = ImmutableArray<uint16>.Empty; Complete = false }

/// <summary>Builds a coverage of sorted distinct glyphs, as a list or as ranges.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <param name="ranges">Whether to write it as ranges.</param>
let private coverageOf (glyphs: uint32 array) (ranges: bool) =
    match ranges with
    | false -> CoverageGlyphs (ImmutableArray.CreateRange glyphs)
    | true -> CoverageRanges (ImmutableArray.CreateRange (glyphs |> Array.mapi (fun index glyph -> { First = glyph; Last = glyph; Value = uint32 index })))

/// <summary>The direct answers of a coverage whose glyphs are all below 4096, none otherwise.</summary>
/// <param name="glyphs">The covered glyphs.</param>
let private coverageAnswers (glyphs: uint32 array) =
    match glyphs.Length > 0 && Array.max glyphs < 4096u with
    | false -> [||]
    | true -> Array.init (int (Array.max glyphs) + 1) (fun glyph -> match Array.IndexOf(glyphs, uint32 glyph) with | -1 -> 0xFFFFus | index -> uint16 index)

/// <summary>Builds one random pair subtable over the glyphs of an alphabet.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs the subtable is made of.</param>
/// <param name="plain">Whether the subtable is one the lean loop applies: records that are one advance, sets in order, direct tables.</param>
let private pairSubtable (rnd: Random) (alphabet: uint32 array) (plain: bool) =
    let (format1, format2) = match plain with | true -> (4us, 0us) | false -> formats[rnd.Next formats.Length]
    let covered = Array.init (rnd.Next(1, alphabet.Length + 1)) (fun _ -> alphabet[rnd.Next alphabet.Length]) |> Array.distinct |> Array.sort
    let coverage = coverageOf covered (rnd.Next 4 = 0)
    let answers = coverageAnswers covered
    let withDirect = answers.Length > 0 && (plain || rnd.Next 6 <> 0)
    match rnd.Next 2 with
    | 0 ->
        let sets =
            covered
            |> Array.map (fun _ ->
                let seconds = Array.init (rnd.Next(0, alphabet.Length + 1)) (fun _ -> alphabet[rnd.Next alphabet.Length]) |> Array.distinct |> Array.sort
                match plain, rnd.Next 14 with
                | false, 0 when seconds.Length > 1 -> Array.rev seconds
                | false, 1 when seconds.Length > 0 -> Array.append seconds [| seconds[seconds.Length - 1] |]
                | _ -> seconds)
        let starts = Array.scan (fun total (set: uint32 array) -> total + set.Length) 0 sets
        let seconds = Array.concat sets
        let struct (values, devices) = recordFields rnd format1 format2 seconds.Length
        PairPosGlyphs (coverage, format1, format2, ImmutableArray.CreateRange starts, ImmutableArray.CreateRange seconds, values, devices, Digest.full, direct answers withDirect)
    | _ ->
        let class1Count = rnd.Next(1, 5)
        let class2Count = rnd.Next(1, 5)
        let classes (count: int) = Array.init (int (Array.max alphabet |> min 4095u) + 1) (fun _ -> if rnd.Next 3 = 0 then 0us else uint16 (rnd.Next(0, count + 2)))
        let first = classes class1Count
        let second = classes class2Count
        let classDef (answers: uint16 array) =
            match rnd.Next 3 with
            | 0 -> ClassDefArray (0u, ImmutableArray.CreateRange (answers |> Array.map uint32))
            | 1 -> ClassDefRanges (ImmutableArray.CreateRange (answers |> Array.mapi (fun glyph klass -> { First = uint32 glyph; Last = uint32 glyph; Value = uint32 klass }) |> Array.filter (fun record -> record.Value <> 0u)))
            | _ -> ClassDefNull
        let struct (values, devices) = recordFields rnd format1 format2 (class1Count * class2Count)
        let definitions1 = classDef first
        let definitions2 = classDef second
        let answersOf (definition: ClassDef) = Array.init (int (Array.max alphabet |> min 4095u) + 1) (fun glyph -> uint16 (ClassDef.classOf definition (uint32 glyph)))
        PairPosClasses (coverage, format1, format2, definitions1, definitions2, class1Count, class2Count, values, devices, Digest.full, direct answers withDirect, direct (answersOf definitions1) (plain || rnd.Next 6 <> 0), direct (answersOf definitions2) (plain || rnd.Next 6 <> 0))

/// <summary>Builds one random lookup of pair subtables, of every format.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs the subtables are made of.</param>
/// <param name="plain">Whether the lookup is one the lean loop applies.</param>
let private pairLookup (rnd: Random) (alphabet: uint32 array) (plain: bool) =
    let subtables = ImmutableArray.CreateRange (Array.init (rnd.Next(1, 4)) (fun _ -> pairSubtable rnd alphabet plain))
    let flags = match plain with | true -> 0u | false -> lookupFlags[rnd.Next lookupFlags.Length]
    LayoutAccel.build GposTables.subtableDigest GposTables.secondDigest
        { LookupType = 2
          Props = flags
          IsReverse = false
          Subtables = subtables }

/// <summary>Builds a single-adjustment subtable, which the pair loops leave to the general loop.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The glyphs.</param>
let private singleLookup (rnd: Random) (alphabet: uint32 array) =
    let covered = Array.init (rnd.Next(1, alphabet.Length + 1)) (fun _ -> alphabet[rnd.Next alphabet.Length]) |> Array.distinct |> Array.sort
    let struct (values, devices) = recordFields rnd 4us 0us covered.Length
    let subtable = SinglePos (coverageOf covered false, 4us, covered.Length, values, devices, direct (coverageAnswers covered) true)
    LayoutAccel.build GposTables.subtableDigest GposTables.secondDigest
        { LookupType = 1
          Props = 0u
          IsReverse = false
          Subtables = ImmutableArray.Create subtable }

/// <summary>Builds a whole synthetic case from one seed: a table of pair lookups, the map and stages that apply them, a font
/// of some scale and ppem, and a run of glyphs the lookups meet often.</summary>
/// <param name="seed">The seed.</param>
let private syntheticCase (seed: int) =
    let rnd = Random seed
    let alphabet = Array.append [| 1u .. uint32 (rnd.Next(3, 16)) |] (if rnd.Next 4 = 0 then [| bigGlyphs[rnd.Next bigGlyphs.Length] |] else [||])
    let lookupCount = rnd.Next(1, 5)
    let plainCase = rnd.Next 5 < 3
    let lookups = Array.init lookupCount (fun _ -> if rnd.Next 12 = 0 && not plainCase then singleLookup rnd alphabet else pairLookup rnd alphabet (plainCase && rnd.Next 8 <> 0))
    let table =
        { Version = 0x00010000u
          Scripts = ImmutableArray.Empty
          Features = ImmutableArray.Empty
          Lookups = lookups |> Array.map (fun lookup -> lazy lookup) |> ImmutableArray.CreateRange
          FeatureVariations = ReadOnlyMemory.Empty }
    let mapCount = match rnd.Next 24 with | 0 -> rnd.Next(65, 71) | _ -> rnd.Next(1, 7)
    let maps =
        Array.init mapCount (fun _ ->
            { LookupIndex = rnd.Next lookupCount
              AutoZwnj = rnd.Next 3 = 0
              AutoZwj = rnd.Next 3 = 0
              Random = false
              PerSyllable = false
              Mask = (match plainCase with | true -> lookupMasks[rnd.Next (lookupMasks.Length - 1)] | false -> lookupMasks[rnd.Next lookupMasks.Length])
              FeatureTag = Tag.make 'k' 'e' 'r' 'n' })
    let stages =
        let cuts = Array.init (rnd.Next(1, 3)) (fun _ -> rnd.Next(0, mapCount + 1)) |> Array.sort
        Array.append cuts [| (if rnd.Next 8 = 0 then rnd.Next(0, mapCount + 1) else mapCount) |] |> Array.map (fun last -> { LastLookup = last; Pause = PauseKind.NoPause })
    let globalMask = maps |> Array.fold (fun bits map -> bits ||| map.Mask) 0u |> fun bits -> if rnd.Next 6 = 0 then bits &&& 0x08u else bits
    let leanRun = rnd.Next 3 <> 0
    let length = match rnd.Next 8 with | 0 -> rnd.Next(0, 3) | 1 -> rnd.Next(30, 90) | _ -> rnd.Next(2, 24)
    let clusters = Array.scan (fun total _ -> total + uint32 (rnd.Next(0, 3))) 0u (Array.zeroCreate length) |> Array.truncate length
    let glyphPool = Array.append alphabet (if rnd.Next 5 = 0 then [| 0u; 99u; 5000u |] else [||])
    let infos =
        Sealed.init length (fun index ->
            { Codepoint = glyphPool[rnd.Next glyphPool.Length]
              Mask = (if leanRun then globalMask else (if rnd.Next 3 = 0 then uint32 (rnd.Next()) else uint32 (rnd.Next()) &&& globalMask))
              Cluster = if rnd.Next 5 = 0 then uint32 index else clusters[index]
              Var1 = 0u
              Var2 = 0u })
    let props =
        Sealed.init length (fun _ ->
            { Unchecked.defaultof<GlyphProps> with
                UnicodeProps = (if not leanRun && rnd.Next 8 = 0 then 0x20us else 0us)
                GlyphClass = [| GlyphPropsBits.Base; GlyphPropsBits.Base; GlyphPropsBits.Mark; GlyphPropsBits.Ligature; 0us |][rnd.Next 5] })
    let nearLimit = rnd.Next 5 = 0
    let positions =
        Sealed.init length (fun index ->
            { XAdvance = (if nearLimit then (match rnd.Next 3 with | 0 -> Int32.MaxValue - rnd.Next 40 | 1 -> Int32.MinValue + rnd.Next 40 | _ -> rnd.Next(-500, 500)) else 500 + index)
              YAdvance = 0
              XOffset = rnd.Next(-3, 4)
              YOffset = 0
              Var = uint32 (rnd.Next 4) })
    let direction = [| Direction.Ltr; Direction.Ltr; Direction.Ltr; Direction.Rtl; Direction.Ttb |][rnd.Next 5]
    let run =
        { GlyphRun.empty with
            Infos = infos
            Props = props
            Positions = positions
            HavePositions = rnd.Next 30 <> 0
            ContentType = GlyphContent
            Flags = LanguagePrimitives.EnumOfValue<uint32, BufferFlags> (uint32 (if rnd.Next 3 = 0 then 0x2 else if rnd.Next 3 = 0 then rnd.Next 0x100 else 0))
            ClusterLevel = [| ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes |][rnd.Next 4]
            Successful = rnd.Next 40 <> 0
            SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = Language.cLocale }
            ScratchFlags = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> (uint32 (rnd.Next 0x100))
            MaxOps = (match rnd.Next 8 with | 0 -> rnd.Next(0, 12) | 1 -> rnd.Next(0, 400) | _ -> GlyphRun.MaxOpsDefault) }
    let scale = [| 1000; 1000; 2048; 17; 4096 |][rnd.Next 5]
    let ppem = [| 0u; 0u; 12u; 100u |][rnd.Next 4]
    let font = Font.create Face.empty |> Font.withScale scale scale |> Font.withPpem ppem ppem
    struct (font, table, ImmutableArray.CreateRange maps, ImmutableArray.CreateRange stages, globalMask, run, leanRun && not (Array.exists (fun (props: GlyphProps) -> props.UnicodeProps <> 0us) (props.AsSpan().ToArray())))

/// <summary>What one case exercised, for the report.</summary>
[<Struct>]
type private Outcome =
    { Changed: bool
      Lean: bool
      LeanRan: bool
      Complex: bool
      Wide: bool
      Flagged: bool }

/// <summary>Compares the general loop, the pair loop and the lean loop on one run, in every field.</summary>
/// <param name="what">What the case is, for the failure message.</param>
/// <param name="font">The font.</param>
/// <param name="table">The table.</param>
/// <param name="maps">The map's lookups.</param>
/// <param name="stages">The map's stages.</param>
/// <param name="globalMask">The global mask.</param>
/// <param name="run">The run.</param>
/// <param name="leanContract">Whether the run keeps the promises the lean loop asks of its caller.</param>
/// <returns>What the case exercised.</returns>
let private compareLoops (what: string) (font: Font) (table: LayoutTable<PosSubtable>) (maps: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (globalMask: uint32) (run: GlyphRun) (leanContract: bool) =
    let gdef = font.Ot.Gdef.Value
    let covered = Engine.gposLookupCoveredOf table maps
    let glyphLookups = Engine.gposGlyphLookupsOf table maps
    let pairs = Engine.gposPairsOf table gdef maps stages globalMask covered glyphLookups
    let reference = Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty run
    let paired = Engine.positionTable font table gdef maps stages covered glyphLookups (Engine.pairsForFont font pairs).Tables run
    expectSameRunAll $"{what}: pair loop" reference paired
    Expect.equal (obj.ReferenceEquals(paired, run)) (obj.ReferenceEquals(reference, run)) $"{what}: the pair loop returns the run itself exactly when the general loop does"
    let usable = run.Successful && run.HavePositions && run.Positions.Length = run.Infos.Length && run.Props.Length = run.Infos.Length
    let horizontal = Direction.isHorizontal run.SegmentProperties.Direction
    let leanRan =
        match leanContract && usable && horizontal with
        | false -> false
        | true ->
            let infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
            let positions = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray())
            match Engine.positionOwned pairs font infos positions run.Flags run.ClusterLevel run.MaxOps with
            | ValueNone ->
                Expect.sequenceEqual (infos.AsSpan().ToArray()) (run.Infos.AsSpan().ToArray()) $"{what}: a run the lean loop refuses is not written (infos)"
                Expect.sequenceEqual (positions.AsSpan().ToArray()) (run.Positions.AsSpan().ToArray()) $"{what}: a run the lean loop refuses is not written (positions)"
                false
            | ValueSome left ->
                Expect.sequenceEqual (infos.AsSpan().ToArray()) (reference.Infos.AsSpan().ToArray()) $"{what}: lean infos"
                Expect.sequenceEqual (positions.AsSpan().ToArray()) (reference.Positions.AsSpan().ToArray()) $"{what}: lean positions"
                Expect.equal left reference.MaxOps $"{what}: lean operation budget"
                Expect.isTrue reference.Successful $"{what}: lean run succeeds"
                true
    { Changed = not (obj.ReferenceEquals(reference, run))
      Lean = pairs.Lean <> 0UL
      LeanRan = leanRan
      Complex = (Engine.pairsForFont font pairs).Tables |> Seq.exists (fun tables -> tables |> Seq.exists (fun table -> table.Kind = Sealed.PairKindGeneral || not table.Plain))
      Wide = maps.Length > 64
      Flagged = maps |> Seq.exists (fun map -> Gdef.sanitizeLookupProps gdef table.Lookups[map.LookupIndex].Value.Props <> 0u && not pairs.Tables[map.LookupIndex].IsEmpty) }

/// <summary>Checks one synthetic case and, when it fails, prints the seed that replays it.</summary>
/// <param name="seed">The seed.</param>
let private checkSynthetic (seed: int) =
    try
        let struct (font, table, maps, stages, globalMask, run, leanContract) = syntheticCase seed
        compareLoops $"synthetic seed {seed}" font table maps stages globalMask run leanContract
    with error ->
        printfn $"FAILED synthetic seed {seed} {error.Message}"
        reraise ()

/// <summary>One random case on a real font: a text shaped without GPOS, then positioned by every loop.</summary>
[<Struct>]
type private RealCase =
    { FontIndex: int
      Text: uint32 array
      Flags: BufferFlags
      Level: ClusterLevel
      Clusters: uint32 array
      Ops: int
      Direction: Direction }

/// <summary>Generates a real-font case: a font with layout tables and a text of it, mostly Latin.</summary>
let private genRealCase =
    gen {
        let! fontIndex = Gen.frequency [ 10, Gen.elements [ 0; 2; 4; 6 ]; 3, Gen.choose (FirstLookupFont, fontSources.Length - 1); 1, Gen.choose (0, FirstLookupFont - 1) ]
        let! text = genText fontIndex
        let! flags = genFlags
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! clusters = genClusters text.Length
        let! ops = Gen.frequency [ 18, Gen.constant GlyphRun.MaxOpsDefault; 2, Gen.choose (0, 60) ]
        let! direction = Gen.frequency [ 17, Gen.constant Direction.Ltr; 2, Gen.constant Direction.Rtl; 1, Gen.constant Direction.Ttb ]
        return { FontIndex = fontIndex; Text = text; Flags = flags; Level = level; Clusters = clusters; Ops = ops; Direction = direction }
    }

/// <summary>Checks one real-font case.</summary>
/// <param name="case">The case.</param>
/// <returns>What the case exercised, none when the font has no plan.</returns>
let private checkReal (case: RealCase) =
    let font = fonts.Value[case.FontIndex]
    let segment = { Direction = case.Direction; Script = Scripts.Latin; Language = Language.cLocale }
    match Execute.createPlan font segment ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> ValueNone
    | Ok plan ->
        match plan.Ot with
        | ValueNone -> ValueNone
        | ValueSome otPlan ->
            let source =
                { GlyphRun.empty with
                    Infos = Sealed.init case.Text.Length (fun index -> { Codepoint = case.Text[index]; Mask = 0u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u }) }
                |> GlyphRun.withSegment segment
            let withoutGpos = { plan with GposCovered = ImmutableArray.Empty }
            match Execute.run withoutGpos font ImmutableArray<Feature>.Empty source with
            | Error _ -> ValueNone
            | Ok shaped ->
                let run =
                    { shaped with
                        Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan())
                        Flags = case.Flags
                        ClusterLevel = case.Level
                        MaxOps = case.Ops }
                let run =
                    match run.Infos.Length = case.Clusters.Length with
                    | true -> { run with Infos = Sealed.init run.Infos.Length (fun index -> { run.Infos[index] with Cluster = case.Clusters[index] }) }
                    | false -> run
                let noIgnorables = run.Props.AsSpan().ToArray() |> Array.forall (fun props -> props.UnicodeProps &&& 0x20us = 0us)
                let usable = run.HavePositions && run.Positions.Length = run.Infos.Length
                let covered = plan.GposLookupCovered
                let reference = Engine.positionPlannedReference otPlan font covered plan.GposGlyphLookups run
                let paired = Engine.positionPlanned otPlan font covered plan.GposGlyphLookups plan.GposPairs run
                expectSameRunAll $"real font {case.FontIndex}: pair loop" reference paired
                Expect.equal (obj.ReferenceEquals(paired, run)) (obj.ReferenceEquals(reference, run)) "the pair loop returns the run itself exactly when the general loop does"
                let leanRan =
                    match usable && noIgnorables && Direction.isHorizontal case.Direction with
                    | false -> false
                    | true ->
                        let infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
                        let positions = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray())
                        match Engine.positionOwned plan.GposPairs font infos positions run.Flags run.ClusterLevel run.MaxOps with
                        | ValueNone ->
                            Expect.sequenceEqual (infos.AsSpan().ToArray()) (run.Infos.AsSpan().ToArray()) "a run the lean loop refuses is not written"
                            false
                        | ValueSome left ->
                            Expect.sequenceEqual (infos.AsSpan().ToArray()) (reference.Infos.AsSpan().ToArray()) $"real font {case.FontIndex}: lean infos"
                            Expect.sequenceEqual (positions.AsSpan().ToArray()) (reference.Positions.AsSpan().ToArray()) $"real font {case.FontIndex}: lean positions"
                            Expect.equal left reference.MaxOps $"real font {case.FontIndex}: lean operation budget"
                            true
                ValueSome
                    { Changed = not (obj.ReferenceEquals(reference, run))
                      Lean = plan.GposPairs.Lean <> 0UL
                      LeanRan = leanRan
                      Complex = false
                      Wide = false
                      Flagged = false }

/// <summary>Checks one real-font case and, when it fails, prints what replays it.</summary>
/// <param name="case">The case.</param>
let private checkRealReporting (case: RealCase) =
    try
        checkReal case
    with error ->
        printfn $"FAILED real font {case.FontIndex} text {case.Text} flags {case.Flags} level {case.Level} ops {case.Ops} direction {case.Direction} {error.Message}"
        reraise ()

/// <summary>The bytes one call allocates, after a warm-up call.</summary>
/// <param name="action">The call.</param>
let private measureAlloc (action: unit -> unit) =
    action ()
    let before = GC.GetAllocatedBytesForCurrentThread()
    action ()
    GC.GetAllocatedBytesForCurrentThread() - before

/// <summary>The pair loops of GPOS held to the general loop they fold.</summary>
[<Tests>]
let gposPairLoopTests =
    testList "gpos pair loops" [
        test "the pair loop and the lean loop equal the general loop in every field over random pair tables" {
            let seeds = Gen.sampleWithSeed (Rnd Seed) 30 SyntheticCaseCount (Gen.choose (0, Int32.MaxValue))
            let results = seeds |> Seq.map checkSynthetic |> Seq.toArray
            let count predicate = results |> Array.filter predicate |> Array.length
            printfn $"gpos pair loops synthetic property: {results.Length} cases, {count (fun outcome -> outcome.Changed)} changed the run, {count (fun outcome -> outcome.Lean)} had a lean position, {count (fun outcome -> outcome.LeanRan)} ran the lean loop, {count (fun outcome -> outcome.Complex)} had a table the pair loops leave to the general loop, {count (fun outcome -> outcome.Wide)} had more than 64 lookups, {count (fun outcome -> outcome.Flagged)} had a pair lookup whose flags skip glyphs"
            Expect.isGreaterThan (count (fun outcome -> outcome.Changed)) 5000 "many cases change the run"
            Expect.isGreaterThan (count (fun outcome -> outcome.LeanRan)) 3000 "many cases run the lean loop"
            Expect.isGreaterThan (count (fun outcome -> outcome.Complex)) 1000 "many cases have a table that is not plain or not compiled"
            Expect.isGreaterThan (count (fun outcome -> outcome.Wide)) 300 "some cases have more than 64 lookups"
            Expect.isGreaterThan (count (fun outcome -> outcome.Flagged)) 4000 "many cases have a pair lookup whose flags skip glyphs, which the pair loop applies through skipped glyphs"
        }

        test "the pair loop and the lean loop equal the general loop in every field over random runs of real fonts" {
            let cases = Gen.sampleWithSeed (Rnd Seed) 30 RealCaseCount genRealCase
            let results = cases |> Seq.map checkRealReporting |> Seq.choose ValueOption.toOption |> Seq.toArray
            let count predicate = results |> Array.filter predicate |> Array.length
            printfn $"gpos pair loops real-font property: {cases.Length} cases, {results.Length} with a plan, {count (fun outcome -> outcome.Changed)} changed the run, {count (fun outcome -> outcome.LeanRan)} ran the lean loop"
            Expect.isGreaterThan (count (fun outcome -> outcome.Changed)) 1000 "many cases change the run"
            Expect.isGreaterThan (count (fun outcome -> outcome.LeanRan)) 1500 "many cases run the lean loop"
        }

        test "the lean loop writes a kerned line in place and allocates nothing, and a line with no kerned pair is left as it is" {
            let font = fonts.Value[0]
            let segment = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
            match Execute.createPlan font segment ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> failtest "the plan"
            | Ok plan ->
                let otPlan = match plan.Ot with | ValueSome otPlan -> otPlan | ValueNone -> failtest "the ot plan"
                let withoutGpos = { plan with GposCovered = ImmutableArray.Empty }
                let prepare (text: string) =
                    let bytes = Text.Encoding.UTF8.GetBytes text
                    let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
                    match Execute.run withoutGpos font ImmutableArray<Feature>.Empty run with
                    | Ok shaped -> { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                    | Error _ -> failtest "the run"
                let lean (run: GlyphRun) =
                    let infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
                    let positions = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray())
                    let bytes = measureAlloc (fun () -> Engine.positionOwned plan.GposPairs font infos positions run.Flags run.ClusterLevel run.MaxOps |> ignore)
                    struct (bytes, infos, positions)
                let kerned = prepare (String.replicate 500 "AV")
                let struct (keptBytes, infos, positions) = lean kerned
                Expect.equal keptBytes 0L "a kerned line allocates nothing in the lean loop: the arrays are the result"
                Expect.notEqual (positions.AsSpan().ToArray()) (kerned.Positions.AsSpan().ToArray()) "the line was kerned"
                Expect.notEqual (infos.AsSpan().ToArray()) (kerned.Infos.AsSpan().ToArray()) "and flagged"
                let covered glyph = Sealed.anyGlyphIn plan.GposCovered (ReadOnlySpan [| { Codepoint = glyph; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u } |]) |> Farfbuzz.Harness.TailCalls.settle
                let uncovered = [| 0x20u .. 0x24Fu |] |> Array.find (fun codepoint -> match Font.nominalGlyph font codepoint with | ValueSome glyph -> not (covered glyph) | ValueNone -> false)
                let idle = prepare (String.replicate 500 (string (char uncovered)))
                let struct (idleBytes, idleInfos, idlePositions) = lean idle
                Expect.equal idleBytes 0L "a line no lookup covers allocates nothing"
                Expect.sequenceEqual (idleInfos.AsSpan().ToArray()) (idle.Infos.AsSpan().ToArray()) "and is left as it is (infos)"
                Expect.sequenceEqual (idlePositions.AsSpan().ToArray()) (idle.Positions.AsSpan().ToArray()) "and is left as it is (positions)"
                let general (run: GlyphRun) = Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run |> ignore
                let generalBytes = measureAlloc (fun () -> general kerned)
                let glyphs = int64 kerned.Infos.Length
                Expect.isLessThanOrEqual generalBytes (2L * (glyphs * 20L + 24L) + 256L) "the pair loop allocates one positions array and one infos array and a constant"
                let idleGeneral = measureAlloc (fun () -> general idle)
                Expect.equal idleGeneral 0L "the pair loop allocates nothing for a line no lookup covers"
                let nothingKerned = [| 'o'; 'e'; 'a'; 'c' |] |> Array.collect (fun a -> [| 'o'; 'e'; 'a'; 'c' |] |> Array.map (fun b -> prepare (String [| a; b |]))) |> Array.tryFind (fun run -> obj.ReferenceEquals(Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run, run))
                match nothingKerned with
                | None -> ()
                | Some run ->
                    Expect.isTrue (Sealed.anyGlyphIn plan.GposCovered (run.Infos.AsSpan())) "the lookups reach the line"
                    Expect.equal (measureAlloc (fun () -> general run)) 0L "a line the lookups reach but kern nowhere allocates nothing, and comes back as it went in"
        }
    ]
