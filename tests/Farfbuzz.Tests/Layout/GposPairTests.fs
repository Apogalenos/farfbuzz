module Farfbuzz.Tests.Layout.GposPairTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>A minimal font whose units per em and scale are 1000, so a value scales to itself.</summary>
/// <param name="ppem">The pixels per em, zero for none.</param>
/// <returns>The font.</returns>
let private font (ppem: uint32) =
    Font.create (Face.create (ReadOnlyMemory (sfnt 0x00010000u [])) 0u)
    |> Font.withScale 1000 1000
    |> Font.withPpem ppem ppem

/// <summary>The environment of a lookup that skips nothing, on a font and a direction.</summary>
/// <param name="font">The font.</param>
/// <param name="horizontal">Whether the direction is horizontal.</param>
/// <returns>The environment.</returns>
let private envOf (font: Font) (horizontal: bool) =
    { Font = font
      Direction = (match horizontal with | true -> Direction.Ltr | false -> Direction.Ttb)
      Horizontal = horizontal
      Matcher = Matching.matcherFor 1 false 0u 1u false false false 0uy
      Gdef = font.Ot.Gdef.Value }

/// <summary>Builds the glyphs of a run, each with the lookup mask and its own cluster.</summary>
/// <param name="glyphs">The glyph ids.</param>
/// <returns>The infos.</returns>
let private infosOf (glyphs: uint32 array) =
    glyphs |> Array.mapi (fun index glyph -> { Codepoint = glyph; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })

/// <summary>Builds the properties of a run's glyphs: plain base glyphs.</summary>
/// <param name="count">The number of glyphs.</param>
/// <returns>The properties.</returns>
let private propsOf (count: int) =
    Array.create count { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Base }

/// <summary>Builds default positions for a run, each glyph 500 wide.</summary>
/// <param name="count">The number of glyphs.</param>
/// <returns>The positions.</returns>
let private positionsOf (count: int) =
    Array.create count { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }

/// <summary>A direct table that says nothing, so the structure is asked.</summary>
let private noDirect = { Table = ImmutableArray<uint16>.Empty; Complete = false }

/// <summary>A pair subtable of format 1: glyph 1 followed by glyph 2 has one record.</summary>
/// <param name="format1">The first value format.</param>
/// <param name="format2">The second value format.</param>
/// <param name="values">The value fields of the record.</param>
/// <returns>The subtable.</returns>
let private pairOfGlyphs (format1: uint16) (format2: uint16) (values: int16 array) =
    PairPosGlyphs (CoverageGlyphs (ImmutableArray.Create 1u), format1, format2, ImmutableArray.Create (0, 1), ImmutableArray.Create 2u, ImmutableArray.CreateRange values, ImmutableArray<Device>.Empty, Digest.full, noDirect)

/// <summary>Applies a pair subtable at the first glyph of a run of glyphs 1 and 2.</summary>
/// <param name="env">The environment.</param>
/// <param name="subtable">The subtable.</param>
/// <returns>The outcome.</returns>
let private applyAtFirst (env: PosEnv) (subtable: PosSubtable) =
    let infos = infosOf [| 1u; 2u; 3u |]
    GposLookups.applySubtable env subtable (ReadOnlySpan infos) (ReadOnlySpan (propsOf 3)) (ReadOnlySpan (positionsOf 3)) 0 |> Farfbuzz.Harness.TailCalls.settle

/// <summary>Random glyphs below a bound: mostly a few hundred, now and then up to past the last glyph id a font can have.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The bound.</returns>
let private glyphBound (rnd: Random) =
    match rnd.Next 24 with
    | 0 -> rnd.Next(60000, 70000)
    | 1 -> rnd.Next(4000, 9000)
    | _ -> rnd.Next(2, 400)

/// <summary>A random coverage of every shape the direct table meets: glyph arrays and range arrays, in order and apart or not, with
/// duplicates, overlaps, ranges that end before they start, and indices that do not fit a table entry.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The coverage.</returns>
let private randomCoverage (rnd: Random) =
    let bound = glyphBound rnd
    let inOrder = rnd.Next 3 <> 0
    match rnd.Next 3 with
    | 0 ->
        let glyphs = Array.init (rnd.Next(0, 80)) (fun _ -> uint32 (rnd.Next bound)) |> Array.sort
        match inOrder with
        | true -> CoverageGlyphs (ImmutableArray.CreateRange (Array.distinct glyphs))
        | false -> CoverageGlyphs (ImmutableArray.CreateRange (Array.sortBy (fun _ -> rnd.Next()) glyphs))
    | _ ->
        let edges = Array.init (2 * rnd.Next(0, 40)) (fun _ -> uint32 (rnd.Next bound)) |> Array.sort
        let ranges =
            Array.init (edges.Length / 2) (fun index ->
                { RangeRecord.First = edges[2 * index]
                  Last = edges[2 * index + 1]
                  Value = (match rnd.Next 20 with | 0 -> uint32 (rnd.Next(60000, 70000)) | _ -> uint32 (rnd.Next 200) * 3u) })
        let disturbed =
            match inOrder with
            | true -> ranges |> Array.filter (fun (range: RangeRecord) -> range.First <= range.Last) |> Array.fold (fun kept (range: RangeRecord) -> match kept with | (previous: RangeRecord) :: _ when previous.Last >= range.First -> kept | _ -> range :: kept) [] |> List.rev |> List.toArray
            | false -> (match rnd.Next 2 with | 0 -> Array.sortBy (fun _ -> rnd.Next()) ranges | _ -> Array.map (fun (range: RangeRecord) -> match rnd.Next 6 with | 0 -> { range with Last = range.First / 2u } | _ -> range) ranges)
        CoverageRanges (ImmutableArray.CreateRange disturbed)

/// <summary>A random class definition of every shape the direct table meets: an array of classes from a first glyph, and ranges, in order and
/// apart or not, with classes beyond what a table entry holds.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The class definition.</returns>
let private randomClassDef (rnd: Random) =
    let bound = glyphBound rnd
    let largest = match rnd.Next 12 with | 0 -> 70000 | _ -> 9
    match rnd.Next 3 with
    | 0 -> ClassDefArray (uint32 (rnd.Next bound), ImmutableArray.CreateRange (Array.init (rnd.Next(0, 120)) (fun _ -> uint32 (rnd.Next(0, largest + 1)))))
    | kind ->
        let edges = Array.init (2 * rnd.Next(0, 40)) (fun _ -> uint32 (rnd.Next bound)) |> Array.sort
        let ranges = Array.init (edges.Length / 2) (fun index -> { RangeRecord.First = edges[2 * index]; Last = edges[2 * index + 1]; Value = uint32 (rnd.Next(0, largest + 1)) })
        let ordered = ranges |> Array.fold (fun kept (range: RangeRecord) -> match kept with | (previous: RangeRecord) :: _ when previous.Last >= range.First -> kept | _ -> range :: kept) [] |> List.rev |> List.toArray
        match kind with
        | 1 -> ClassDefRanges (ImmutableArray.CreateRange ordered)
        | _ -> ClassDefRanges (ImmutableArray.CreateRange (Array.sortBy (fun _ -> rnd.Next()) ranges))

/// <summary>Compares two direct tables, entry by entry and in whether they are complete.</summary>
/// <param name="what">What the case is.</param>
/// <param name="actual">The table written in one pass.</param>
/// <param name="expected">The table read from the structure glyph by glyph.</param>
let private sameDirect (what: string) (actual: DirectIndex) (expected: DirectIndex) =
    Expect.equal actual.Complete expected.Complete $"{what}: complete"
    Expect.sequenceEqual (actual.Table.AsSpan().ToArray()) (expected.Table.AsSpan().ToArray()) $"{what}: table"

/// <summary>The positioning and the pair tests of P5.S10.</summary>
[<Tests>]
let gposPairTests =
    testList "gpos pair" [
        test "P5.S10 a field that is zero is not worked unless its device offset is read, which needs a ppem" {
            let hinting = HintingDevice (11us, 13us, 1us, ReadOnlyMemory<byte> [| 0x00uy; 0x00uy |])
            let values = ImmutableArray.Create (0s, 7s)
            let devices = ImmutableArray.Create (NoDevice, hinting)
            let position = { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
            let withoutPpem = GposLookups.applyValue (envOf (font 0u) true) 0x0044us values devices 0 position
            let withPpem = GposLookups.applyValue (envOf (font 12u) true) 0x0044us values devices 0 position
            let struct (movedWithout, workedWithout) = withoutPpem
            let struct (movedWith, workedWith) = withPpem
            Expect.isFalse workedWithout "no ppem: the device is never read, the zero advance is not worked"
            Expect.equal movedWithout position "no ppem: nothing moves"
            Expect.isTrue workedWith "ppem 12: the device offset is non-zero, so the value is worked even if its delta is zero"
            Expect.equal movedWith.XAdvance 100 "a zero delta moves nothing"
        }

        test "P5.S10 a pair that applies is unsafe to break to its second glyph, and a zero pair is unsafe to concat" {
            let env = envOf (font 0u) true
            let kerned = applyAtFirst env (pairOfGlyphs 4us 0us [| -50s |])
            Expect.isTrue kerned.Applied "the pair applies"
            Expect.equal kerned.Next 1 "the second glyph is the next first glyph"
            Expect.equal kerned.Advance -50 "the first glyph's advance shrinks by the scaled value"
            Expect.equal kerned.Break1To 2 "unsafe to break from the first glyph to the second inclusive"
            Expect.equal kerned.ConcatTo 0 "no concat range"
            let zero = applyAtFirst env (pairOfGlyphs 4us 0us [| 0s |])
            Expect.isTrue zero.Applied "a zero-valued record still applies"
            Expect.equal zero.Break1To 0 "it did not work, so it is not unsafe to break"
            Expect.equal zero.ConcatTo 2 "even a zero-valued pair record is a concat hazard"
            let both = applyAtFirst env (pairOfGlyphs 4us 4us [| -50s; 30s |])
            Expect.equal both.Next 2 "a second record consumes the second glyph"
            Expect.equal both.Break1To 2 "unsafe to break to the second glyph"
            Expect.equal both.Break2To 3 "and once more past it"
            Expect.equal both.SecondIndex 1 "the second record goes to the second glyph"
            Expect.equal both.Second.XAdvance 530 "the second glyph's advance grows"
        }

        test "P5.S10 a class pair whose class is out of range does not apply and is unsafe to concat" {
            let classDef1 = ClassDefArray (1u, ImmutableArray.Create 1u)
            let classDef2 = ClassDefArray (2u, ImmutableArray.Create 3u)
            let subtable =
                PairPosClasses (CoverageGlyphs (ImmutableArray.Create 1u), 4us, 0us, classDef1, classDef2, 2, 2, ImmutableArray.Create (0s, 0s, 0s, 0s), ImmutableArray<Device>.Empty, Digest.full, noDirect, noDirect, noDirect)
            let outcome = applyAtFirst (envOf (font 0u) true) subtable
            Expect.isFalse outcome.Applied "class 3 of the second glyph is not below the class count 2"
            Expect.equal outcome.ConcatTo 2 "the attempt is recorded as unsafe to concat up to the second glyph"
            let inRange = PairPosClasses (CoverageGlyphs (ImmutableArray.Create 1u), 4us, 0us, classDef1, ClassDefArray (2u, ImmutableArray.Create 1u), 2, 2, ImmutableArray.Create (0s, 0s, 0s, -20s), ImmutableArray<Device>.Empty, Digest.full, noDirect, noDirect, noDirect)
            let applied = applyAtFirst (envOf (font 0u) true) inRange
            Expect.isTrue applied.Applied "classes 1 and 1 are in range"
            Expect.equal applied.Advance -20 "the value of the class pair at row 1 column 1"
        }

        test "P5.S10 a hinting device moves a value at ppem 12 and not at ppem 0" {
            let device = HintingDevice (11us, 13us, 1us, ReadOnlyMemory<byte> [| 0x10uy; 0uy |])
            let values = ImmutableArray.Create (0s, 7s)
            let devices = ImmutableArray.Create (NoDevice, device)
            let position = { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
            let struct (atZero, _) = GposLookups.applyValue (envOf (font 0u) true) 0x0044us values devices 0 position
            let struct (atTwelve, worked) = GposLookups.applyValue (envOf (font 12u) true) 0x0044us values devices 0 position
            Expect.equal atZero.XAdvance 100 "ppem 0 reads no device"
            Expect.isTrue worked "ppem 12 reads the device"
            Expect.equal atTwelve.XAdvance (100 + 1000 / 12) "one pixel at ppem 12 is scale / ppem"
        }

        test "P5.S10 the vertical advance is negated and the horizontal advance ignored in a vertical run" {
            let position = { XAdvance = 0; YAdvance = -500; XOffset = 0; YOffset = 0; Var = 0u }
            let values = ImmutableArray.Create (100s, 40s)
            let struct (vertical, worked) = GposLookups.applyValue (envOf (font 0u) false) 0x000Cus values ImmutableArray<Device>.Empty 0 position
            Expect.isTrue worked "the vertical field is non-zero"
            Expect.equal vertical.XAdvance 0 "the horizontal advance is not applied in a vertical run"
            Expect.equal vertical.YAdvance (-500 - 40) "the vertical advance is subtracted"
            let struct (horizontal, _) = GposLookups.applyValue (envOf (font 0u) true) 0x000Cus values ImmutableArray<Device>.Empty 0 { position with XAdvance = 10 }
            Expect.equal horizontal.XAdvance 110 "the horizontal advance is added in a horizontal run"
            Expect.equal horizontal.YAdvance -500 "the vertical advance is ignored in a horizontal run"
        }

        test "GPOS direct tables answer exactly what the coverage and the class definitions answer" {
            let sources = [| "perf/fonts/Roboto-Regular.ttf"; "test/api/fonts/OpenSans-Regular.ttf"; "test/api/fonts/NotoSans-Bold.ttf" |]
            let mutable checkedTables = 0
            for source in sources do
                let table = (Font.create (Face.create (read source) 0u)).Ot.Gpos.Value
                for lookup in table.Lookups do
                    for accel in lookup.Value.Subtables do
                        let covered coverage (direct: DirectIndex) =
                            for glyph in 0u .. 5000u do
                                let expected = match Coverage.index coverage glyph with | Covered index -> index | NotCovered -> -1
                                let actual =
                                    match glyph < uint32 direct.Table.Length with
                                    | true -> (match direct.Table[int glyph] with | 0xFFFFus -> -1 | answer -> int answer)
                                    | false -> (match direct.Complete with | true -> -1 | false -> expected)
                                Expect.equal actual expected $"coverage of glyph {glyph} in {source}"
                        let classed classDef (direct: DirectIndex) =
                            for glyph in 0u .. 5000u do
                                let expected = ClassDef.classOf classDef glyph
                                let actual =
                                    match glyph < uint32 direct.Table.Length with
                                    | true -> uint32 direct.Table[int glyph]
                                    | false -> (match direct.Complete with | true -> 0u | false -> expected)
                                Expect.equal actual expected $"class of glyph {glyph} in {source}"
                        match accel.Subtable with
                        | SinglePos (coverage, _, _, _, _, direct) -> covered coverage direct; checkedTables <- checkedTables + 1
                        | PairPosGlyphs (coverage, _, _, _, _, _, _, _, direct) -> covered coverage direct; checkedTables <- checkedTables + 1
                        | PairPosClasses (coverage, _, _, classDef1, classDef2, _, _, _, _, _, direct, first, second) ->
                            covered coverage direct
                            classed classDef1 first
                            classed classDef2 second
                            checkedTables <- checkedTables + 1
                        | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosContext _ | PosChainContext _ | PosPending _ -> ()
            Expect.isGreaterThan checkedTables 3 "the fonts hold single and pair subtables"
        }

        test "GPOS a lookup that covers nothing in a run allocates nothing, and a kerned run allocates its positions and flags only" {
            let font = Font.create (Face.create (read "perf/fonts/Roboto-Regular.ttf") 0u)
            let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
            match Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
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
                let position (run: GlyphRun) = Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run
                let bytesOf (run: GlyphRun) =
                    position run |> ignore
                    let before = GC.GetAllocatedBytesForCurrentThread()
                    let result = position run
                    struct (GC.GetAllocatedBytesForCurrentThread() - before, result)
                let covered glyph = Sealed.anyGlyphIn plan.GposCovered (ReadOnlySpan [| { Codepoint = glyph; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u } |]) |> Farfbuzz.Harness.TailCalls.settle
                let uncovered = [| 0x20u .. 0x24Fu |] |> Array.find (fun codepoint -> match Font.nominalGlyph font codepoint with | ValueSome glyph -> not (covered glyph) | ValueNone -> false)
                let nothing = prepare (String.replicate 1000 (string (char uncovered)))
                let struct (idleBytes, idle) = bytesOf nothing
                Expect.equal idleBytes 0L "a run no positioning lookup covers allocates nothing"
                Expect.isTrue (obj.ReferenceEquals(idle, nothing)) "and comes back as it went in"
                let kerned = prepare (String.replicate 500 "AV")
                let struct (keptBytes, result) = bytesOf kerned
                let glyphs = int64 kerned.Infos.Length
                let positionsArray = glyphs * 20L + 24L
                let infosArray = glyphs * 20L + 24L
                Expect.isLessThanOrEqual keptBytes (positionsArray + infosArray + 256L) $"{keptBytes} B for {glyphs} kerned glyphs: one positions array and one infos array"
                Expect.isFalse ((result.Positions.AsSpan().ToArray()) = (kerned.Positions.AsSpan().ToArray())) "the run was kerned"
        }

        test "a direct table of a coverage written in one pass equals the table read glyph by glyph, for every shape of coverage" {
            let inOrder =
                Array.init 6000 (fun seed ->
                    let coverage = randomCoverage (Random (0x5EED + seed))
                    sameDirect $"coverage seed {seed}" (GposTables.directCoverage coverage) (GposTables.directCoverageBySearch coverage)
                    match coverage with
                    | CoverageGlyphs glyphs -> Sealed.directOfGlyphs (glyphs.AsSpan()) 1 0xFFFFus |> ValueOption.isSome
                    | CoverageRanges ranges -> Sealed.rangesOrdered ((Sealed.map (fun (range: RangeRecord) -> range.First) (ranges.AsSpan())).AsSpan()) ((Sealed.map (fun (range: RangeRecord) -> range.Last) (ranges.AsSpan())).AsSpan()) |> Farfbuzz.Harness.TailCalls.settle
                    | CoverageNull -> false
                    | CoverageDirect _ -> false)
            let ordered = inOrder |> Array.filter id |> Array.length
            Expect.isGreaterThan ordered 2000 "many coverages are in order, which the pass writes"
            Expect.isLessThan ordered 5000 "and many are not, which are read glyph by glyph"
        }

        test "a direct table of a class definition written in one pass equals the table read glyph by glyph, for every shape of class definition" {
            for seed in 0 .. 5999 do
                let classDef = randomClassDef (Random (0xC1A55 + seed))
                sameDirect $"class definition seed {seed}" (GposTables.directClasses classDef) (GposTables.directClassesBySearch classDef)
        }

        test "a direct table reaches every glyph a font can have, so that the pair tables of a large font are complete" {
            let glyphs = ImmutableArray.CreateRange [| 3u; 4100u; 30000u; 65535u |]
            let direct = GposTables.directCoverage (CoverageGlyphs glyphs)
            Expect.isTrue direct.Complete "a coverage that ends at the last glyph id is complete"
            Expect.equal direct.Table.Length 65536 "and holds an entry for every glyph id"
            Expect.equal direct.Table[30000] 2us "the index of a glyph past the old limit"
            Expect.equal direct.Table[29999] 0xFFFFus "and no index for a glyph that is not covered"
            let classes = GposTables.directClasses (ClassDefRanges (ImmutableArray.Create { RangeRecord.First = 5000u; Last = 40000u; Value = 7u }))
            Expect.isTrue classes.Complete "a class definition that ends past the old limit is complete"
            Expect.equal classes.Table[39999] 7us "with the class of a glyph past the old limit"
            Expect.equal classes.Table[40000] 7us "and of its last glyph"
        }
    ]
