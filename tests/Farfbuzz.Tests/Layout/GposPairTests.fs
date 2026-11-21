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
    ]
