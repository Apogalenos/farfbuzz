module Farfbuzz.Tests.AcceleratorTests

open System
open System.Buffers
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.Shape.ShapeManyTests

/// <summary>The file extensions of the font files of the shaping suites and the perf fonts.</summary>
let private fontExtensions = [| ".ttf"; ".otf"; ".ttc"; ".dfont" |]

/// <summary>The font files of the three shaping suites and of upstream's shaping benchmark, sorted.</summary>
let private fontPaths =
    lazy (
        let upstream = RepositoryPaths.upstream ()
        let directories =
            (Suites.suiteNames |> List.map (fun suite -> Path.Combine(upstream, "test", "shape", "data", suite, "fonts")))
            @ [ Path.Combine(upstream, "perf", "fonts") ]
        directories
        |> List.collect (fun directory ->
            Directory.EnumerateFiles directory
            |> Seq.filter (fun path -> Array.contains ((Path.GetExtension path |> nonNull).ToLowerInvariant()) fontExtensions)
            |> List.ofSeq)
        |> List.sort
        |> List.toArray)

/// <summary>Every face of every test font that sanitises, read from disk one file at a time.</summary>
let private allFaces () =
    seq {
        for path in fontPaths.Value do
            let bytes = ReadOnlyMemory (File.ReadAllBytes path)
            for index in 0 .. Face.count bytes - 1 do
                yield struct ($"{Path.GetFileName path}#{index}", bytes, uint32 index)
    }

/// <summary>The coverages of a context subtable.</summary>
/// <param name="rules">The rules.</param>
/// <returns>The coverages, in the order the subtable holds them.</returns>
let private contextCoverages (rules: ContextRules) =
    match rules with
    | GlyphRules (coverage, _) -> [ coverage ]
    | ClassRules (coverage, _, _, _) -> [ coverage ]
    | CoverageRule (coverages, _) -> List.ofSeq coverages

/// <summary>The coverages of a chained-context subtable.</summary>
/// <param name="rules">The rules.</param>
/// <returns>The coverages, in the order the subtable holds them.</returns>
let private chainCoverages (rules: ChainRules) =
    match rules with
    | ChainGlyphRules (coverage, _) -> [ coverage ]
    | ChainClassRules (coverage, _, _, _, _, _) -> [ coverage ]
    | ChainCoverageRule (backtrack, input, lookahead, _) -> List.ofSeq backtrack @ List.ofSeq input @ List.ofSeq lookahead

/// <summary>The coverages of a GSUB subtable.</summary>
/// <param name="subtable">The subtable.</param>
/// <returns>Every coverage it holds.</returns>
let private substCoverages (subtable: SubstSubtable) =
    match subtable with
    | SingleDelta (coverage, _, _) -> [ coverage ]
    | SingleList (coverage, _) -> [ coverage ]
    | Multiple (coverage, _) -> [ coverage ]
    | Alternate (coverage, _) -> [ coverage ]
    | LigatureSubst (coverage, _, _) -> [ coverage ]
    | SubstContext rules -> contextCoverages rules
    | SubstChainContext rules -> chainCoverages rules
    | ReverseChainSingle (coverage, backtrack, lookahead, _) -> coverage :: (List.ofSeq backtrack @ List.ofSeq lookahead)
    | SubstPending _ -> []

/// <summary>The class definitions of a GSUB subtable.</summary>
/// <param name="subtable">The subtable.</param>
/// <returns>Every class definition it holds.</returns>
let private substClassDefs (subtable: SubstSubtable) =
    match subtable with
    | SubstContext (ClassRules (_, classDef, _, _)) -> [ classDef ]
    | SubstChainContext (ChainClassRules (_, backtrack, input, lookahead, _, _)) -> [ backtrack; input; lookahead ]
    | SubstContext (GlyphRules _) -> []
    | SubstContext (CoverageRule _) -> []
    | SubstChainContext (ChainGlyphRules _) -> []
    | SubstChainContext (ChainCoverageRule _) -> []
    | SingleDelta _ -> []
    | SingleList _ -> []
    | Multiple _ -> []
    | Alternate _ -> []
    | LigatureSubst _ -> []
    | ReverseChainSingle _ -> []
    | SubstPending _ -> []

/// <summary>Every subtable of a layout table, with the table's lookup index.</summary>
/// <param name="table">The table.</param>
/// <returns>The lookup index, the lookup and one subtable entry per subtable.</returns>
let private subtablesOf (table: LayoutTable<'S>) =
    seq {
        for lookupIndex in 0 .. table.Lookups.Length - 1 do
            let accel = table.Lookups[lookupIndex].Value
            for entry in accel.Subtables do
                yield struct (lookupIndex, accel, entry)
    }

/// <summary>The glyph ids a comparison asks of a face: every glyph, then ids past the glyph count and far out of range.</summary>
/// <param name="glyphCount">The face's glyph count.</param>
/// <returns>The glyph ids.</returns>
let private probesOf (glyphCount: int) =
    Array.concat
        [ Array.init (glyphCount + 2) uint32
          [| uint32 glyphCount + 100u; 0x3FFFu; 0x4000u; 0xFFFFu; 0x10000u; 0xFFFFFFu; 0x1000000u; UInt32.MaxValue - 1u; UInt32.MaxValue |] ]

/// <summary>Collects the failures of a per-face check over every test face and fails when there is one.</summary>
/// <param name="check">Checks one face and returns its failures and the number of items it compared.</param>
/// <returns>The total number of items compared.</returns>
let private overEveryFace (check: string -> OtFace -> string list * int) =
    let failures = ResizeArray<string>()
    let mutable compared = 0
    for struct (name, bytes, index) in allFaces () do
        let otFace = OtFace.create (Face.create bytes index)
        let found, count = check name otFace
        failures.AddRange found
        compared <- compared + count
    Expect.isEmpty failures (String.concat "\n" (Seq.truncate 20 failures))
    compared

/// <summary>The failures of the digests of one layout table: a glyph a subtable can start at that its digest or its lookup's digest
/// rejects, and a lookup digest that is not the union of its subtables'.</summary>
/// <param name="name">The face's name.</param>
/// <param name="tableName">The table's name.</param>
/// <param name="glyphsOf">The glyphs a subtable can start at.</param>
/// <param name="table">The table.</param>
/// <returns>The failures and the number of glyphs checked.</returns>
let private digestFailures (name: string) (tableName: string) (glyphsOf: 'S -> ImmutableArray<uint32>) (table: LayoutTable<'S>) =
    let rejected =
        subtablesOf table
        |> Seq.collect (fun (struct (lookupIndex, accel, entry)) ->
            glyphsOf entry.Subtable
            |> Seq.filter (fun glyph -> not (Digest.mayHaveCall glyph entry.SubtableDigest && Digest.mayHaveCall glyph accel.LookupDigest))
            |> Seq.map (fun glyph -> $"{name} {tableName} lookup {lookupIndex}: glyph {glyph} is rejected by a digest"))
        |> List.ofSeq
    let notUnion =
        [ for lookupIndex in 0 .. table.Lookups.Length - 1 do
              let accel = table.Lookups[lookupIndex].Value
              let union = accel.Subtables |> Seq.fold (fun digest entry -> Digest.union digest entry.SubtableDigest) Digest.empty
              if union <> accel.LookupDigest then yield $"{name} {tableName} lookup {lookupIndex}: the lookup digest is not the union of its subtables'" ]
    let checkedGlyphs = subtablesOf table |> Seq.sumBy (fun (struct (_, _, entry)) -> (glyphsOf entry.Subtable).Length)
    rejected @ notUnion, checkedGlyphs

/// <summary>The failures of the digests of one face's GSUB and GPOS.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of glyphs checked.</returns>
let private faceDigestFailures (name: string) (otFace: OtFace) =
    let gsubFailures, gsubChecked = digestFailures name "GSUB" LookupGlyphs.subst otFace.Gsub.Value
    let gposFailures, gposChecked = digestFailures name "GPOS" LookupGlyphs.pos otFace.Gpos.Value
    gsubFailures @ gposFailures, gsubChecked + gposChecked

/// <summary>The failures of every GSUB coverage of a face against its digest: a covered glyph the digest rejects.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of coverages checked.</returns>
let private coverageDigestFailures (name: string) (otFace: OtFace) =
    let glyphCount = otFace.Face.GlyphCount
    let coverages = subtablesOf otFace.Gsub.Value |> Seq.collect (fun (struct (_, _, entry)) -> substCoverages entry.Subtable) |> Seq.toArray
    let failures =
        coverages
        |> Seq.collect (fun coverage ->
            let digest = Coverage.digest coverage
            probesOf glyphCount
            |> Seq.filter (fun glyph ->
                match Coverage.index coverage glyph with
                | Covered _ -> not (Digest.mayHaveCall glyph digest)
                | NotCovered -> false)
            |> Seq.map (fun glyph -> $"{name}: covered glyph {glyph} is rejected by the coverage digest"))
        |> List.ofSeq
    failures, coverages.Length

/// <summary>The failures of the second-glyph digest of a face's GSUB ligature lookups: a ligature whose second glyph the lookup's
/// second digest rejects, and a ligature with no second glyph when the second digest is not full.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of ligatures checked.</returns>
let private secondDigestFailures (name: string) (otFace: OtFace) =
    let ligatures =
        subtablesOf otFace.Gsub.Value
        |> Seq.collect (fun (struct (lookupIndex, accel, entry)) ->
            match entry.Subtable with
            | LigatureSubst (_, sets, _) -> sets |> Seq.collect id |> Seq.map (fun ligature -> struct (lookupIndex, accel, ligature))
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> Seq.empty)
        |> Seq.toArray
    let failures =
        ligatures
        |> Seq.choose (fun (struct (lookupIndex, accel, ligature)) ->
            match ligature.Components.IsEmpty with
            | true when accel.SecondDigest <> Digest.full -> Some $"{name} lookup {lookupIndex}: a ligature with one component but a second digest that is not full"
            | true -> None
            | false when not (Digest.mayHaveCall ligature.Components[0] accel.SecondDigest) -> Some $"{name} lookup {lookupIndex}: the second glyph {ligature.Components[0]} is rejected"
            | false -> None)
        |> List.ofSeq
    failures, ligatures.Length

/// <summary>The digest of a buffer's glyphs, <c>Digest.ofSpan</c> over infos.</summary>
/// <param name="run">The glyph ids.</param>
/// <returns>The digest.</returns>
let private bufferDigest (run: uint32 array) =
    let infos = run |> Array.map (fun glyph -> { Codepoint = glyph; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
    Digest.ofSpan (ReadOnlySpan infos) |> TailCalls.settle

/// <summary>A lookup's accelerator and the glyphs its subtables can start at.</summary>
type private StartSet<'S> = { Accel: LookupAccel<'S>; Starts: HashSet<uint32> }

/// <summary>The start sets of every lookup of a table.</summary>
/// <param name="glyphsOf">The glyphs a subtable can start at.</param>
/// <param name="table">The table.</param>
/// <returns>One start set per lookup.</returns>
let private startSetsOf (glyphsOf: 'S -> ImmutableArray<uint32>) (table: LayoutTable<'S>) =
    table.Lookups
    |> Seq.map (fun lazyAccel ->
        let accel = lazyAccel.Value
        { Accel = accel; Starts = accel.Subtables |> Seq.collect (fun entry -> glyphsOf entry.Subtable) |> HashSet })
    |> Seq.toArray

/// <summary>Whether every lookup that can start at a glyph of the run is admitted by the run's digest.</summary>
/// <param name="sets">The start sets of a table's lookups.</param>
/// <param name="run">The run's glyph ids.</param>
/// <returns>True when no lookup that can start in the run is skipped.</returns>
let private runAdmitsEveryStarter (sets: StartSet<'S> array) (run: uint32 array) =
    let digest = bufferDigest run
    sets |> Array.forall (fun set -> not (Array.exists set.Starts.Contains run) || Digest.mayIntersectCall set.Accel.LookupDigest digest)

/// <summary>A lookup-heavy face with the start sets of its two tables.</summary>
type private DigestFace = { Face: OtFace; GsubSets: StartSet<SubstSubtable> array; GposSets: StartSet<PosSubtable> array }

/// <summary>The faces the run-digest property draws from: lookup-heavy fonts of the perf set and the shaping suites.</summary>
let private digestFaces =
    lazy (
        [ "perf/fonts/Roboto-Regular.ttf"
          "perf/fonts/Amiri-Regular.ttf"
          "perf/fonts/NotoNastaliqUrdu-Regular.ttf"
          "perf/fonts/NotoSansDuployan-Regular.otf"
          "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf" ]
        |> List.map (fun path ->
            let otFace = OtFace.create (Face.create (UpstreamFiles.read path) 0u)
            { Face = otFace
              GsubSets = startSetsOf LookupGlyphs.subst otFace.Gsub.Value
              GposSets = startSetsOf LookupGlyphs.pos otFace.Gpos.Value })
        |> List.toArray)

/// <summary>A run of glyph ids of a face: some drawn from anywhere in the face, some from the glyphs a lookup of it starts at.</summary>
/// <param name="digestFace">The face.</param>
/// <returns>The generator.</returns>
let private genRun (digestFace: DigestFace) =
    let starters = digestFace.GsubSets |> Seq.collect (fun set -> set.Starts) |> Seq.truncate 4000 |> Seq.toArray
    let anywhere = Gen.choose (0, max 1 digestFace.Face.Face.GlyphCount - 1) |> Gen.map uint32
    let element =
        match starters.Length with
        | 0 -> anywhere
        | _ -> Gen.frequency [ 3, anywhere; 1, Gen.elements starters ]
    gen {
        let! length = Gen.choose (1, 30)
        return! Gen.arrayOfLength length element
    }

/// <summary>The P9.S08 properties of the digests the accelerators filter with.</summary>
[<Tests>]
let p9s08Tests =
    testList "P9.S08" [
        test "P9.S08 no lookup or subtable digest rejects a glyph the subtable can start at, in every lookup of every test font" {
            let compared = overEveryFace faceDigestFailures
            Expect.isGreaterThan compared 100000 "glyphs checked over every font"
        }
        test "P9.S08 no coverage digest rejects a covered glyph, in every GSUB coverage of every test font" {
            let compared = overEveryFace coverageDigestFailures
            Expect.isGreaterThan compared 1000 "coverages checked over every font"
        }
        test "P9.S08 a ligature lookup's second digest keeps the second glyph of every ligature, in every test font" {
            let compared = overEveryFace secondDigestFailures
            Expect.isGreaterThan compared 100 "ligatures checked over every font"
        }
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 300 }
            "P9.S08 the run digest admits every lookup that can start in the run, GSUB and GPOS, on five lookup-heavy fonts"
            (Prop.forAll (Arb.fromGen (gen {
                let! faceIndex = Gen.choose (0, digestFaces.Value.Length - 1)
                let! run = genRun digestFaces.Value[faceIndex]
                return struct (faceIndex, run)
            })) (fun (struct (faceIndex, run)) ->
                let digestFace = digestFaces.Value[faceIndex]
                runAdmitsEveryStarter digestFace.GsubSets run && runAdmitsEveryStarter digestFace.GposSets run))
        Farfbuzz.Tests.Layout.CommonTests.tests
    ]

/// <summary>The direct tables of every GSUB coverage and class definition of a face, answered against their source for every
/// glyph id.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of direct tables checked.</returns>
let private directFailures (name: string) (otFace: OtFace) =
    let glyphCount = otFace.Face.GlyphCount
    let probes = probesOf glyphCount
    let subtables = subtablesOf otFace.Gsub.Value |> Seq.map (fun (struct (_, _, entry)) -> entry.Subtable) |> Seq.toArray
    let coverages = subtables |> Array.collect (substCoverages >> List.toArray)
    let classDefs = subtables |> Array.collect (substClassDefs >> List.toArray)
    let coverageFailures =
        coverages
        |> Seq.collect (fun coverage ->
            match coverage with
            | CoverageDirect (_, _, source) ->
                probes
                |> Seq.filter (fun glyph -> Coverage.index coverage glyph <> Coverage.index source glyph)
                |> Seq.map (fun glyph -> $"{name}: the direct coverage differs from its source at glyph {glyph}")
            | CoverageNull | CoverageGlyphs _ | CoverageRanges _ -> Seq.empty)
        |> List.ofSeq
    let classFailures =
        classDefs
        |> Seq.collect (fun classDef ->
            match classDef with
            | ClassDefDirect (_, _, source) ->
                probes
                |> Seq.filter (fun glyph -> ClassDef.classOf classDef glyph <> ClassDef.classOf source glyph)
                |> Seq.map (fun glyph -> $"{name}: the direct class definition differs from its source at glyph {glyph}")
            | ClassDefNull | ClassDefArray _ | ClassDefRanges _ -> Seq.empty)
        |> List.ofSeq
    let direct =
        (coverages |> Array.filter (fun coverage -> match coverage with | CoverageDirect _ -> true | CoverageNull | CoverageGlyphs _ | CoverageRanges _ -> false)).Length
        + (classDefs |> Array.filter (fun classDef -> match classDef with | ClassDefDirect _ -> true | ClassDefNull | ClassDefArray _ | ClassDefRanges _ -> false)).Length
    coverageFailures @ classFailures, direct

/// <summary>The number of entries of the table a direct coverage was built from.</summary>
/// <param name="coverage">The source coverage.</param>
/// <returns>The glyph count of an array, the range count of ranges.</returns>
let private coverageEntries (coverage: Coverage) =
    match coverage with
    | CoverageGlyphs glyphs -> glyphs.Length
    | CoverageRanges ranges -> ranges.Length
    | CoverageNull -> 0
    | CoverageDirect _ -> 0

/// <summary>The failures of the size rules of the direct tables of a face: at most 16,384 slots, at most 16 for each glyph or range
/// of a coverage and 64 for each range of a class definition, and none for a table of fewer than 8 entries.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of direct tables checked.</returns>
let private directSizeFailures (name: string) (otFace: OtFace) =
    let subtables = subtablesOf otFace.Gsub.Value |> Seq.map (fun (struct (_, _, entry)) -> entry.Subtable) |> Seq.toArray
    let coverageFailures =
        subtables
        |> Seq.collect substCoverages
        |> Seq.choose (fun coverage ->
            match coverage with
            | CoverageDirect (slots, _, source) ->
                let entries = coverageEntries source
                match slots.Length <= 16384 && slots.Length <= 16 * entries && entries >= 8 with
                | true -> None
                | false -> Some $"{name}: a direct coverage of {slots.Length} slots for {entries} entries"
            | CoverageNull | CoverageGlyphs _ | CoverageRanges _ -> None)
        |> List.ofSeq
    let classFailures =
        subtables
        |> Seq.collect substClassDefs
        |> Seq.choose (fun classDef ->
            match classDef with
            | ClassDefDirect (slots, _, ClassDefRanges ranges) ->
                match slots.Length <= 16384 && slots.Length <= 64 * ranges.Length && ranges.Length >= 8 with
                | true -> None
                | false -> Some $"{name}: a direct class definition of {slots.Length} slots for {ranges.Length} ranges"
            | ClassDefDirect _ -> Some $"{name}: a direct class definition built from something other than ranges"
            | ClassDefNull | ClassDefArray _ | ClassDefRanges _ -> None)
        |> List.ofSeq
    coverageFailures @ classFailures, coverageFailures.Length + classFailures.Length

/// <summary>The P9.S09 equivalence of the direct coverage and class tables with the searches they replace.</summary>
[<Tests>]
let p9s09Tests =
    testList "P9.S09" [
        test "P9.S09 a direct coverage or class table answers every glyph id as its source does, in every GSUB subtable of every test font" {
            let direct = overEveryFace directFailures
            Expect.isGreaterThan direct 100 "direct tables met over every font"
        }
        test "P9.S09 a direct table is built only above eight entries and within its slot allowance, in every test font" {
            overEveryFace directSizeFailures |> ignore
        }
        Farfbuzz.Tests.Layout.DirectTableTests.tests
    ]

/// <summary>The props of a glyph by the rule of <c>GDEF::get_glyph_props</c>, read from the classes themselves.</summary>
/// <param name="gdef">The GDEF.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The props.</returns>
let private referenceProps (gdef: Gdef) (glyph: uint32) =
    match ClassDef.classOf gdef.GlyphClasses glyph with
    | 1u -> GlyphPropsBits.Base
    | 2u -> GlyphPropsBits.Ligature
    | 3u -> GlyphPropsBits.Mark ||| uint16 (ClassDef.classOf gdef.MarkAttachClasses glyph <<< 8)
    | _ -> 0us

/// <summary>The failures of a face's GDEF glyph props table: a glyph whose table entry differs from the classes, or a table not as
/// long as the glyph count.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of glyphs checked.</returns>
let private glyphPropsFailures (name: string) (otFace: OtFace) =
    let gdef = otFace.Gdef.Value
    let glyphCount = otFace.Face.GlyphCount
    let lengthFailure =
        match gdef.GlyphProps.Length = glyphCount with
        | true -> []
        | false -> [ $"{name}: the props table has {gdef.GlyphProps.Length} entries for {glyphCount} glyphs" ]
    let failures =
        probesOf glyphCount
        |> Seq.filter (fun glyph -> Gdef.glyphProps gdef glyph <> referenceProps gdef glyph)
        |> Seq.map (fun glyph -> $"{name}: the props of glyph {glyph} differ from the classes'")
        |> List.ofSeq
    lengthFailure @ failures, glyphCount

/// <summary>The failures of a face's mark glyph sets: a glyph whose answer differs from the members of its set, and an invalid set
/// index that covers a glyph.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of sets checked.</returns>
let private markSetFailures (name: string) (otFace: OtFace) =
    let gdef = otFace.Gdef.Value
    let glyphCount = otFace.Face.GlyphCount
    let probes = probesOf glyphCount
    let setFailures =
        [ for set in 0 .. gdef.MarkGlyphSets.Length - 1 do
              let members = HashSet (Coverage.glyphs gdef.MarkGlyphSets[set])
              for glyph in probes do
                  if Gdef.markSetCovers gdef set glyph <> members.Contains glyph then
                      yield $"{name}: mark set {set} answers wrongly for glyph {glyph}" ]
    let invalidFailures =
        [ for set in [ -1; gdef.MarkGlyphSets.Length; gdef.MarkGlyphSets.Length + 5; Int32.MaxValue ] do
              for glyph in [ 0u; 1u; 5u ] do
                  if Gdef.markSetCovers gdef set glyph then yield $"{name}: invalid mark set {set} covers glyph {glyph}" ]
    setFailures @ invalidFailures, gdef.MarkGlyphSets.Length

/// <summary>The lookup flag combinations a glyph property check is tried with.</summary>
let private lookupPropsSamples =
    [| 0u
       LookupFlag.IgnoreBaseGlyphs
       LookupFlag.IgnoreLigatures
       LookupFlag.IgnoreMarks
       LookupFlag.IgnoreFlags
       0x0100u
       0x0200u
       LookupFlag.UseMarkFilteringSet
       LookupFlag.UseMarkFilteringSet ||| (1u <<< 16)
       LookupFlag.UseMarkFilteringSet ||| (7u <<< 16)
       LookupFlag.IgnoreBaseGlyphs ||| LookupFlag.UseMarkFilteringSet ||| (2u <<< 16) |]

/// <summary>The glyph filter of <c>check_glyph_property</c>, written from the classes and the mark sets without the props table.</summary>
/// <param name="gdef">The GDEF.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="lookupProps">The lookup props.</param>
/// <returns>True when the glyph is not filtered out.</returns>
let private referenceKept (gdef: Gdef) (glyph: uint32) (lookupProps: uint32) =
    let glyphClass = ClassDef.classOf gdef.GlyphClasses glyph
    let ignored =
        (glyphClass = 1u && lookupProps &&& LookupFlag.IgnoreBaseGlyphs <> 0u)
        || (glyphClass = 2u && lookupProps &&& LookupFlag.IgnoreLigatures <> 0u)
        || (glyphClass = 3u && lookupProps &&& LookupFlag.IgnoreMarks <> 0u)
    match ignored, glyphClass with
    | true, _ -> false
    | false, 3u ->
        match lookupProps &&& LookupFlag.UseMarkFilteringSet <> 0u, lookupProps &&& LookupFlag.MarkAttachmentType <> 0u with
        | true, _ ->
            let set = int (lookupProps >>> 16)
            set >= 0 && set < gdef.MarkGlyphSets.Length && (match Coverage.index gdef.MarkGlyphSets[set] glyph with | Covered _ -> true | NotCovered -> false)
        | false, true -> (lookupProps &&& LookupFlag.MarkAttachmentType) = (ClassDef.classOf gdef.MarkAttachClasses glyph <<< 8)
        | false, false -> true
    | false, _ -> true

/// <summary>The failures of a face's glyph filter against the reference filter, for every glyph and every sampled lookup flag.</summary>
/// <param name="name">The face's name.</param>
/// <param name="otFace">The face.</param>
/// <returns>The failures and the number of checks.</returns>
let private filterFailures (name: string) (otFace: OtFace) =
    let gdef = otFace.Gdef.Value
    let glyphCount = otFace.Face.GlyphCount
    let failures =
        [ for lookupProps in lookupPropsSamples do
              for glyph in 0 .. glyphCount - 1 do
                  let glyph = uint32 glyph
                  if Matching.checkGlyphProperty gdef glyph (Gdef.glyphProps gdef glyph) lookupProps <> referenceKept gdef glyph lookupProps then
                      yield $"{name}: the filter of lookup flags {lookupProps} differs at glyph {glyph}" ]
    failures, glyphCount * lookupPropsSamples.Length

/// <summary>Runs a check over the faces that have a GDEF, and fails when there is a failure.</summary>
/// <param name="check">The check of one face.</param>
/// <returns>The number of items compared.</returns>
let private overFacesWithGdef (check: string -> OtFace -> string list * int) =
    overEveryFace (fun name otFace ->
        match otFace.Gdef.Value.Version with
        | 0u -> [], 0
        | _ -> check name otFace)

/// <summary>The P9.S10 equivalence of the glyph props table and the mark-set answers with their definitions.</summary>
[<Tests>]
let p9s10Tests =
    testList "P9.S10" [
        test "P9.S10 the glyph props table equals the props the classes give, for every glyph of every test font with a GDEF" {
            let compared = overFacesWithGdef glyphPropsFailures
            Expect.isGreaterThan compared 10000 "glyphs checked over every font"
        }
        test "P9.S10 a mark filtering set answers as its members do for every glyph, and an invalid set index covers nothing" {
            let sets = overFacesWithGdef markSetFailures
            Expect.isGreaterThan sets 0 "mark glyph sets met over every font"
        }
        test "P9.S10 the glyph filter equals the filter written from the classes and the sets, for every glyph and lookup flag sampled" {
            let compared = overFacesWithGdef filterFailures
            Expect.isGreaterThan compared 100000 "checks over every font"
        }
        test "P9.S10 an out-of-range mark set index is cleared from the lookup flags" {
            let gdef = (OtFace.create (Face.create (UpstreamFiles.read "perf/fonts/Roboto-Regular.ttf") 0u)).Gdef.Value
            let count = uint32 gdef.MarkGlyphSets.Length
            let props = LookupFlag.UseMarkFilteringSet ||| (count <<< 16)
            Expect.equal (Gdef.sanitizeLookupProps gdef props) (props &&& ~~~LookupFlag.UseMarkFilteringSet) "set index at the count is cleared"
            Expect.equal (Gdef.sanitizeLookupProps gdef LookupFlag.IgnoreMarks) LookupFlag.IgnoreMarks "other flags are kept"
        }
    ]

/// <summary>The identity of a lookup accelerator that a test compares: its type, flags, digests, and each subtable's digest and
/// start glyphs.</summary>
/// <param name="glyphsOf">The glyphs a subtable can start at.</param>
/// <param name="accel">The accelerator.</param>
/// <returns>The projection.</returns>
let private projectLookup (glyphsOf: 'S -> ImmutableArray<uint32>) (accel: LookupAccel<'S>) =
    struct (accel.LookupType, accel.Props, accel.IsReverse, accel.LookupDigest, accel.SecondDigest,
            accel.Subtables |> Seq.map (fun entry -> struct (entry.SubtableDigest, Seq.toArray (glyphsOf entry.Subtable))) |> Seq.toArray)

/// <summary>The projections of every lookup of a table, forcing each one first in the given order.</summary>
/// <param name="glyphsOf">The glyphs a subtable can start at.</param>
/// <param name="table">The table.</param>
/// <param name="order">The order the lookups are forced in.</param>
/// <returns>The projections by lookup index.</returns>
let private projectForced (glyphsOf: 'S -> ImmutableArray<uint32>) (table: LayoutTable<'S>) (order: int array) =
    for lookupIndex in order do
        table.Lookups[lookupIndex].Value |> ignore
    table.Lookups |> Seq.map (fun lazyAccel -> projectLookup glyphsOf lazyAccel.Value) |> Seq.toArray

/// <summary>The orders a table's lookups are forced in: file order, reversed, and rotated from the middle.</summary>
let private forcingOrders : (int -> int array) list =
    [ (fun count -> Array.init count id)
      (fun count -> Array.init count (fun at -> count - 1 - at))
      (fun count -> Array.init count (fun at -> (at + count / 2) % max 1 count)) ]

/// <summary>Runs work on threads that all start together and returns what each returned.</summary>
/// <param name="threads">The number of threads.</param>
/// <param name="work">The work, given the thread's number.</param>
/// <returns>The results by thread.</returns>
let private onThreadsTogether (threads: int) (work: int -> 'T) =
    use barrier = new Barrier(threads)
    let tasks =
        Array.init threads (fun number ->
            Task.Factory.StartNew(
                (fun () ->
                    barrier.SignalAndWait()
                    work number),
                TaskCreationOptions.LongRunning))
    Task.WaitAll(tasks |> Array.map (fun task -> task :> Task))
    tasks |> Array.map (fun task -> task.Result)

/// <summary>The number of lookups in both layout tables of a face.</summary>
/// <param name="otFace">The face.</param>
/// <returns>The lookup count.</returns>
let private lookupCountOf (otFace: OtFace) = otFace.Gsub.Value.Lookups.Length + otFace.Gpos.Value.Lookups.Length

/// <summary>The faces with the most lookups, the ones where building accelerators lazily matters.</summary>
let private lookupHeavyFaces =
    lazy (
        allFaces ()
        |> Seq.map (fun (struct (name, bytes, index)) -> struct (name, lookupCountOf (OtFace.create (Face.create bytes index)), bytes, index))
        |> Seq.sortByDescending (fun (struct (_, count, _, _)) -> count)
        |> Seq.truncate 8
        |> Seq.toArray)

/// <summary>The number of threads of the cold-start race.</summary>
let private raceThreads = max 8 (Environment.ProcessorCount + 2)

/// <summary>The number of fresh cold faces each lookup-heavy font is raced over.</summary>
[<Literal>]
let private RaceRounds = 12

/// <summary>The projections of both tables of a face, forcing every lookup in the given orders for GSUB and GPOS.</summary>
/// <param name="otFace">The face.</param>
/// <param name="orderOf">The order for a table of the given lookup count.</param>
/// <returns>The GSUB and GPOS projections.</returns>
let private projectFace (otFace: OtFace) (orderOf: int -> int array) =
    projectForced LookupGlyphs.subst otFace.Gsub.Value (orderOf otFace.Gsub.Value.Lookups.Length),
    projectForced LookupGlyphs.pos otFace.Gpos.Value (orderOf otFace.Gpos.Value.Lookups.Length)

/// <summary>The bytes the load of a face's layout tables allocates, with no lookup forced.</summary>
/// <param name="bytes">The font file.</param>
/// <param name="index">The face index.</param>
let private loadOnly (bytes: ReadOnlyMemory<byte>) (index: uint32) =
    let otFace = OtFace.create (Face.create bytes index)
    otFace.Gsub.Value |> ignore
    otFace.Gpos.Value |> ignore

/// <summary>The bytes the load of a face's layout tables and the forcing of every lookup allocate.</summary>
/// <param name="bytes">The font file.</param>
/// <param name="index">The face index.</param>
let private loadAndForce (bytes: ReadOnlyMemory<byte>) (index: uint32) =
    let otFace = OtFace.create (Face.create bytes index)
    projectFace otFace (fun count -> Array.init count id) |> ignore

/// <summary>The P9.S12 tests of the lazy construction of lookup accelerators.</summary>
[<Tests>]
let p9s12Tests =
    testList "P9.S12" [
        test "P9.S12 accelerators forced in file order, in reverse and from the middle are equal, for every test font" {
            let failures = ResizeArray<string>()
            let mutable lookups = 0
            for struct (name, bytes, index) in allFaces () do
                let fresh () = OtFace.create (Face.create bytes index)
                lookups <- lookups + lookupCountOf (fresh ())
                let projections = forcingOrders |> List.map (fun orderOf -> projectFace (fresh ()) orderOf)
                match projections with
                | first :: rest ->
                    for other in rest do
                        if other <> first then failures.Add $"{name}: forcing the lookups in another order gave other accelerators"
                | [] -> ()
            Expect.isEmpty failures (String.concat "\n" failures)
            Expect.isGreaterThan lookups 1000 "lookups compared over every font"
        }
        test "P9.S12 accelerators forced from many threads at once on a cold face equal the sequential ones, for the eight lookup-heaviest fonts" {
            for struct (name, count, bytes, index) in lookupHeavyFaces.Value do
                let expected = projectFace (OtFace.create (Face.create bytes index)) (fun lookupCount -> Array.init lookupCount id)
                for round in 1 .. RaceRounds do
                    let otFace = OtFace.create (Face.create bytes index)
                    let results =
                        onThreadsTogether raceThreads (fun number ->
                            projectFace otFace (fun lookupCount -> Array.init lookupCount (fun at -> (at + number * 37) % max 1 lookupCount)))
                    Expect.isTrue (results |> Array.forall (fun result -> result = expected)) $"{name} ({count} lookups) round {round}: a thread saw other accelerators than the sequential run"
                    let shared = otFace.Gsub.Value.Lookups |> Seq.forall (fun lazyAccel -> lazyAccel.IsValueCreated)
                    Expect.isTrue shared $"{name} round {round}: a lookup was left unbuilt after every thread forced it"
        }
        test "P9.S12 every lookup is built once: two reads of a lazy accelerator are the same object, however many threads asked" {
            for struct (name, _, bytes, index) in lookupHeavyFaces.Value |> Array.truncate 3 do
                let otFace = OtFace.create (Face.create bytes index)
                let table = otFace.Gsub.Value
                let seen =
                    onThreadsTogether raceThreads (fun _ -> table.Lookups |> Seq.map (fun lazyAccel -> lazyAccel.Value) |> Seq.toArray)
                for other in seen do
                    Expect.isTrue (Array.forall2 (fun (a: LookupAccel<SubstSubtable>) b -> Object.ReferenceEquals(a, b)) seen[0] other) $"{name}: threads read different accelerator objects"
        }
        test "P9.S12 loading a face's layout tables builds no accelerator and allocates a small fraction of building them all" {
            for struct (name, count, bytes, index) in lookupHeavyFaces.Value do
                let otFace = OtFace.create (Face.create bytes index)
                let untouched =
                    otFace.Gsub.Value.Lookups |> Seq.forall (fun lazyAccel -> not lazyAccel.IsValueCreated)
                    && otFace.Gpos.Value.Lookups |> Seq.forall (fun lazyAccel -> not lazyAccel.IsValueCreated)
                Expect.isTrue untouched $"{name}: an accelerator was built by the load"
                let loaded = bytesAllocatedBy (fun () -> loadOnly bytes index)
                let forced = bytesAllocatedBy (fun () -> loadAndForce bytes index)
                Expect.isLessThan (loaded * 4L) forced $"{name} ({count} lookups): the load allocates {loaded} bytes, loading and forcing every lookup {forced}"
        }
    ]

/// <summary>A glyph infos array and its properties for the skipping iterator: the glyph ids cycle through a face's glyphs and the
/// properties are the face's own glyph props.</summary>
/// <param name="gdef">The GDEF.</param>
/// <param name="count">The number of glyphs.</param>
/// <returns>The infos and the properties.</returns>
let private runOfFace (gdef: Gdef) (count: int) =
    let glyphs = Array.init count (fun index -> uint32 ((index * 7) % max 1 gdef.GlyphProps.Length))
    let infos = glyphs |> Array.map (fun glyph -> { Codepoint = glyph; Mask = 0xFFFFFFFFu; Cluster = 0u; Var1 = 0u; Var2 = 0u })
    let props =
        glyphs
        |> Array.map (fun glyph ->
            { UnicodeProps = 0us
              GlyphClass = Gdef.glyphProps gdef glyph
              LigProps = 0uy
              Syllable = 0uy
              ShaperCategory = 0uy
              ShaperAuxiliary = 0uy
              GlyphIndex = 0u })
    infos, props

/// <summary>Counts the glyphs a lookup flag keeps, over a range of glyph ids.</summary>
/// <param name="gdef">The GDEF.</param>
/// <param name="lookupProps">The lookup props.</param>
/// <param name="glyph">The next glyph.</param>
/// <param name="last">The last glyph.</param>
/// <param name="kept">The count so far.</param>
/// <returns>The number of glyphs kept.</returns>
[<TailCall>]
let rec private countKept (gdef: Gdef) (lookupProps: uint32) (glyph: uint32) (last: uint32) (kept: int) =
    match glyph > last with
    | true -> kept
    | false ->
        let kept = match Matching.checkGlyphProperty gdef glyph (Gdef.glyphProps gdef glyph) lookupProps with | true -> kept + 1 | false -> kept
        countKept gdef lookupProps (glyph + 1u) last kept

/// <summary>Counts the glyph ids a coverage covers, over a range.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="glyph">The next glyph.</param>
/// <param name="last">The last glyph.</param>
/// <param name="hits">The count so far.</param>
/// <returns>The number of covered glyphs.</returns>
[<TailCall>]
let rec private countCovered (coverage: Coverage) (glyph: uint32) (last: uint32) (hits: int) =
    match glyph > last with
    | true -> hits
    | false ->
        let hits = match Coverage.index coverage glyph with | Covered _ -> hits + 1 | NotCovered -> hits
        countCovered coverage (glyph + 1u) last hits

/// <summary>Walks the skipping iterator over a run from every position and counts the hits.</summary>
/// <param name="matcher">The matcher.</param>
/// <param name="gdef">The GDEF.</param>
/// <param name="components">The glyph ids the match function compares with.</param>
/// <param name="infos">The glyph infos.</param>
/// <param name="props">The glyph properties.</param>
/// <param name="index">The next start position.</param>
/// <param name="hits">The count so far.</param>
/// <returns>The number of positions that found a following match.</returns>
[<TailCall>]
let rec private walkAll (matcher: Matcher) (gdef: Gdef) (components: ImmutableArray<uint32>) (infos: GlyphInfo array) (props: GlyphProps array) (index: int) (hits: int) =
    match index >= infos.Length with
    | true -> hits
    | false ->
        let probe = Matching.next matcher gdef (GlyphIds components) 0 (ReadOnlySpan infos) (ReadOnlySpan props) index infos.Length |> TailCalls.settle
        let hits = match probe with | Hit _ -> hits + 1 | Miss _ -> hits
        walkAll matcher gdef components infos props (index + 1) hits

/// <summary>The P5.S16 allocation tests of the layout primitives.</summary>
[<Tests>]
let p5s16Tests =
    testList "P5.S16" [
        test "P5.S16 checkGlyphProperty allocates 0 bytes over every glyph and sampled lookup flag of a face with mark sets" {
            let gdef = (OtFace.create (Face.create (UpstreamFiles.read "perf/fonts/NotoNastaliqUrdu-Regular.ttf") 0u)).Gdef.Value
            let last = uint32 gdef.GlyphProps.Length - 1u
            for lookupProps in lookupPropsSamples do
                countKept gdef lookupProps 0u last 0 |> ignore
                Expect.equal (bytesAllocatedBy (fun () -> countKept gdef lookupProps 0u last 0 |> ignore)) 0L $"lookup flags {lookupProps}"
            let ignoring = countKept gdef LookupFlag.IgnoreMarks 0u last 0
            Expect.isLessThan ignoring (countKept gdef 0u 0u last 0) "ignoring marks keeps fewer glyphs: the filter is exercised"
        }
        test "P5.S16 Coverage.index allocates 0 bytes on a direct coverage, an array coverage and a range coverage of a real font" {
            let otFace = OtFace.create (Face.create (UpstreamFiles.read "perf/fonts/NotoNastaliqUrdu-Regular.ttf") 0u)
            let coverages = subtablesOf otFace.Gsub.Value |> Seq.collect (fun (struct (_, _, entry)) -> substCoverages entry.Subtable) |> Seq.toArray
            let pick matcher = coverages |> Array.tryFind matcher
            let direct = pick (fun coverage -> match coverage with | CoverageDirect _ -> true | CoverageNull | CoverageGlyphs _ | CoverageRanges _ -> false)
            let array = pick (fun coverage -> match coverage with | CoverageGlyphs glyphs -> glyphs.Length > 0 | CoverageNull | CoverageRanges _ | CoverageDirect _ -> false)
            let ranges = pick (fun coverage -> match coverage with | CoverageRanges _ -> true | CoverageNull | CoverageGlyphs _ | CoverageDirect _ -> false)
            let last = uint32 otFace.Face.GlyphCount
            let measured =
                [ direct; array; ranges ]
                |> List.choose id
                |> List.map (fun coverage ->
                    Expect.isGreaterThan (countCovered coverage 0u last 0) 0 "the coverage covers something"
                    bytesAllocatedBy (fun () -> countCovered coverage 0u last 0 |> ignore))
            Expect.isGreaterThanOrEqual measured.Length 1 "at least one kind of coverage found in the font"
            Expect.isTrue (measured |> List.forall (fun bytes -> bytes = 0L)) $"bytes allocated by the lookups: {measured}"
        }
        test "P5.S16 Matching.next allocates 0 bytes walking a run with a glyph filter and a match function" {
            let gdef = (OtFace.create (Face.create (UpstreamFiles.read "perf/fonts/NotoNastaliqUrdu-Regular.ttf") 0u)).Gdef.Value
            let infos, props = runOfFace gdef 2000
            let target = Array.findIndex (fun position -> position > 3 && props[position].GlyphClass &&& GlyphPropsBits.Mark = 0us && props[position].GlyphClass &&& 0x000Eus = 0us) [| 0 .. infos.Length - 1 |]
            let components = ImmutableArray.Create infos[target].Codepoint
            let matcher = Matching.matcherFor 0 false LookupFlag.IgnoreMarks 0xFFFFFFFFu false false false 0uy
            let hits = walkAll matcher gdef components infos props 0 0
            Expect.isGreaterThan hits 0 "some walks find a match"
            Expect.isLessThan hits infos.Length "and some do not"
            Expect.equal (bytesAllocatedBy (fun () -> walkAll matcher gdef components infos props 0 0 |> ignore)) 0L "bytes allocated by the walk"
        }
    ]

/// <summary>The code points of UTF-16 text by the BCL's rune decoder, one replacement for each ill-formed unit, with the index of
/// the unit each starts at.</summary>
/// <param name="text">The text.</param>
/// <param name="at">The index to read from.</param>
/// <param name="found">The code points so far, newest first.</param>
/// <returns>The code points with their unit indexes.</returns>
let rec private runeCodepoints (text: char array) (at: int) (found: (uint32 * int) list) =
    match at >= text.Length with
    | true -> List.rev found
    | false ->
        let mutable rune = Rune.ReplacementChar
        let mutable consumed = 0
        let status = Rune.DecodeFromUtf16(ReadOnlySpan(text, at, text.Length - at), &rune, &consumed)
        let value = match status with | OperationStatus.Done -> uint32 rune.Value | _ -> 0xFFFDu
        runeCodepoints text (at + max 1 consumed) ((value, at) :: found)

/// <summary>The code points and clusters the UTF-16 entry reads from units.</summary>
/// <param name="units">The units.</param>
/// <returns>The code points with their clusters.</returns>
let private utf16Entry (units: uint16 array) =
    let run = GlyphRun.addUtf16 (ReadOnlySpan units) 0 units.Length GlyphRun.empty |> TailCalls.settle
    [ for info in run.Infos -> info.Codepoint, int info.Cluster ]

/// <summary>The code points the UTF-8 entry reads from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The code points with their clusters.</returns>
let private utf8Entry (bytes: byte array) =
    let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> TailCalls.settle
    [ for info in run.Infos -> info.Codepoint, int info.Cluster ]

/// <summary>The code points the Latin-1 entry reads from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The code points with their clusters.</returns>
let private latin1Entry (bytes: byte array) =
    let run = GlyphRun.addLatin1 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> TailCalls.settle
    [ for info in run.Infos -> info.Codepoint, int info.Cluster ]

/// <summary>The characters a UTF-16 text is drawn from: ASCII, Latin-1, the rest of the BMP, and lone and paired surrogates.</summary>
let private genUnit =
    Gen.frequency
        [ 6, Gen.choose (0x20, 0x7E)
          2, Gen.choose (0x80, 0xFF)
          2, Gen.choose (0x100, 0xD7FF)
          2, Gen.choose (0xD800, 0xDBFF)
          2, Gen.choose (0xDC00, 0xDFFF)
          1, Gen.choose (0xE000, 0xFFFF) ]

/// <summary>The first lines of a text file of the perf set, with the offset and length of each.</summary>
/// <param name="relative">The path relative to upstream/harfbuzz.</param>
/// <param name="count">The number of lines.</param>
/// <returns>The text and its line ranges.</returns>
let private firstLines (relative: string) (count: int) =
    let text = (UpstreamFiles.read relative).ToArray()
    let ranges =
        Seq.unfold (fun start ->
            match start >= text.Length with
            | true -> None
            | false ->
                let newline = Array.IndexOf(text, '\n'B, start)
                match newline < 0 with
                | true -> None
                | false -> Some (struct (start, newline - start), newline + 1)) 0
        |> Seq.truncate count
        |> Seq.toArray
    text, ranges

/// <summary>Whether the UTF-8 document entry equals decoding, guessing and shaping with the cache entry, for the first lines of a text.</summary>
/// <param name="fontPath">The font path relative to upstream/harfbuzz.</param>
/// <param name="textPath">The text path relative to upstream/harfbuzz.</param>
/// <param name="count">The number of lines.</param>
/// <returns>The number of lines compared and the number that differed.</returns>
let private entryAgainstGeneral (fontPath: string) (textPath: string) (count: int) =
    let font = Font.create (Face.create (UpstreamFiles.read fontPath) 0u)
    let text, ranges = firstLines textPath count
    let shaped =
        ranges
        |> Array.map (fun (struct (start, length)) ->
            let run =
                GlyphRun.addUtf8 (ReadOnlySpan text) start length GlyphRun.empty
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            let struct (general, _) = Shape.shapeCached PlanCache.empty font ImmutableArray<Feature>.Empty ValueNone run
            let struct (entry, _) =
                Shape.shapeUtf8 PlanCache.empty font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory text) start length
            sameResult general entry)
    ranges.Length, shaped |> Array.filter not |> Array.length

/// <summary>The P9.S13 equivalence of the ASCII and Latin fast paths with the general route.</summary>
[<Tests>]
let p9s13Tests =
    testList "P9.S13" [
        test "P9.S13 the Latin-1 entry and the UTF-8 entry's ASCII path read every byte as the checking decoder does" {
            for value in 0 .. 255 do
                Expect.equal (latin1Entry [| byte value |]) [ (uint32 value, 0) ] $"Latin-1 byte {value}"
            for value in 0 .. 127 do
                let bytes = [| byte value |]
                Expect.equal (Utf.next8 (ReadOnlySpan bytes) 0 0xFFFDu) (struct (uint32 value, 1)) $"checking decoder, ASCII byte {value}"
                Expect.equal (utf8Entry bytes) [ (uint32 value, 0) ] $"UTF-8 entry, ASCII byte {value}"
        }
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000 }
            "P9.S13 an ASCII text through the UTF-8 entry equals the checking decoder over the same bytes"
            (Prop.forAll (Arb.fromGen (Gen.arrayOf (Gen.choose (0, 127) |> Gen.map byte))) (fun bytes ->
                let expected = bytes |> Array.mapi (fun at value -> uint32 value, at) |> List.ofArray
                utf8Entry bytes = expected))
        test "P9.S13 the UTF-16 entry reads every one of the 65,536 units as the BCL rune decoder does" {
            let failures =
                [ for value in 0 .. 0xFFFF do
                      let expected = runeCodepoints [| char value |] 0 [] |> List.map (fun (codepoint, at) -> codepoint, at)
                      if utf16Entry [| uint16 value |] <> expected then yield value ]
            Expect.isEmpty failures $"units read differently: {Seq.truncate 10 failures |> Seq.toList}"
        }
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 10000 }
            "P9.S13 a UTF-16 text with lone and paired surrogates reads as the BCL rune decoder does, with its clusters"
            (Prop.forAll (Arb.fromGen (Gen.arrayOf (genUnit |> Gen.map uint16))) (fun units ->
                utf16Entry units = runeCodepoints (units |> Array.map char) 0 []))
        test "P9.S13 the Latin property table equals the general computation for U+0000 to U+00FF, and the fast path for every code point" {
            for value in 0 .. 0xFF do
                Expect.equal UnicodeProps.latinTable[value] (UnicodeProps.compute (uint32 value)) $"table U+{value:X4}"
            let differing = [ for value in 0u .. 0x10FFFFu do if UnicodeProps.computeFast value <> UnicodeProps.compute value then yield value ]
            Expect.isEmpty differing $"computeFast differs from compute at {Seq.truncate 10 differing |> Seq.toList}"
        }
        test "P9.S13 the UTF-8 document entry equals decoding and the cache entry on the first 400 lines of both Roboto texts" {
            for textPath in [ "perf/texts/en-thelittleprince.txt"; "perf/texts/en-words.txt" ] do
                let compared, differing = entryAgainstGeneral "perf/fonts/Roboto-Regular.ttf" textPath 400
                Expect.isGreaterThan compared 300 $"{textPath}: lines compared"
                Expect.equal differing 0 $"{textPath}: lines whose entry result differs from the general route"
        }
    ]
