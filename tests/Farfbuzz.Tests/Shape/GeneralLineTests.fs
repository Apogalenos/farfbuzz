module Farfbuzz.Tests.Shape.GeneralLineTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.Shape.SimpleRunTests

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x6E4E41UL

/// <summary>Generates a code point of a right-to-left Latin line: plain Latin, with the brackets and quotation marks that
/// right-to-left text mirrors and the Latin-1 and extended letters.</summary>
let private genMirroredCodepoint =
    Gen.frequency
        [ 80, Gen.choose (0x20, 0x7E)
          8, Gen.elements [ 0x28; 0x29; 0x3C; 0x3E; 0x5B; 0x5D; 0x7B; 0x7D; 0xAB; 0xBB; 0x2039; 0x203A ]
          9, Gen.choose (0xA1, 0xFF)
          3, Gen.choose (0x100, 0x17F) ]
    |> Gen.map uint32

/// <summary>Generates a right-to-left run of a script that is left to right: its text, its fields and a font.</summary>
let private genRtlCase =
    gen {
        let! fontIndex = Gen.choose (0, fontSources.Length - 1)
        let! kind = Gen.frequency [ 4, Gen.constant 0; 3, Gen.constant 1; (if fontIndex >= FirstLookupFont then 6 else 0), Gen.constant 2 ]
        let! length = Gen.frequency [ 4, Gen.choose (1, 3); 6, Gen.choose (4, 40); 1, Gen.choose (41, 150) ]
        let mapped = mappedCodepoints.Value[fontIndex]
        let! text =
            match kind with
            | 0 -> Gen.arrayOfLength length genMirroredCodepoint
            | 1 -> genText fontIndex
            | _ -> Gen.arrayOfLength length (Gen.frequency [ 9, Gen.elements mapped; 1, genMirroredCodepoint ])
        let! script = Gen.elements [ Scripts.Latin; Scripts.Latin; Scripts.Latin; Scripts.Cyrillic; Scripts.Greek; Scripts.Common; Scripts.Hebrew ]
        let! features = genFeatures fontIndex
        let! flags = genFlags
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! clusters = genClusters text.Length
        let! wildMasks = Gen.frequency [ 4, Gen.constant false; 1, Gen.constant true ]
        let! garbage = Gen.arrayOfLength text.Length (Gen.choose (0, Int32.MaxValue))
        let! successful = Gen.frequency [ 24, Gen.constant true; 1, Gen.constant false ]
        let! ops = Gen.frequency [ 5, Gen.constant GlyphRun.MaxOpsDefault; 1, Gen.choose (0, 5000) ]
        let! invisible = Gen.elements [ 0u; 0u; 3u; 77u ]
        let infos =
            Sealed.init text.Length (fun index ->
                { Codepoint = text[index]
                  Mask = if wildMasks then uint32 garbage[index] else 0u
                  Cluster = clusters[index]
                  Var1 = if wildMasks then uint32 garbage[index] else 0u
                  Var2 = 0u })
        let run =
            { GlyphRun.empty with
                Infos = infos
                ContentType = UnicodeContent
                SegmentProperties = { Direction = Direction.Rtl; Script = script; Language = Language.cLocale }
                Flags = flags
                ClusterLevel = level
                Invisible = invisible
                Successful = successful
                MaxOps = ops }
        return struct (fontIndex, features, run)
    }

/// <summary>Whether the kernel took a run.</summary>
/// <param name="font">The font.</param>
/// <param name="features">The user features.</param>
/// <param name="run">The run.</param>
/// <returns>True when the kernel shaped the run.</returns>
let private tookKernel (font: Font) (features: ImmutableArray<Feature>) (run: GlyphRun) =
    match Execute.createPlan font run.SegmentProperties features ValueNone with
    | Error _ -> false
    | Ok plan ->
        match SimpleRun.tryShapePath plan font run with
        | ValueSome _ -> true
        | ValueNone -> false

/// <summary>Shapes a run through the entry and the general pipeline and compares every field.</summary>
/// <param name="font">The font.</param>
/// <param name="features">The user features.</param>
/// <param name="run">The run.</param>
let private expectSame (font: Font) (features: ImmutableArray<Feature>) (run: GlyphRun) =
    match Execute.createPlan font run.SegmentProperties features ValueNone with
    | Error _ -> ()
    | Ok plan ->
        match Execute.runGeneral plan font features run, Execute.run plan font features run with
        | Ok expected, Ok actual -> expectSameRunAll "run" expected actual
        | Error expected, Error actual -> Expect.equal actual expected "errors"
        | Ok _, Error _ -> failtest "the entry refused a run the general pipeline shaped"
        | Error _, Ok _ -> failtest "the entry shaped a run the general pipeline refused"

/// <summary>The fonts of the normaliser property: Latin fonts with GPOS marks, a font with Hebrew and one with Thai.</summary>
let private normalizerFonts =
    lazy
        ([| "perf/fonts/Roboto-Regular.ttf"
            "test/api/fonts/OpenSans-Regular.ttf"
            "test/api/fonts/NotoSans-Bold.ttf"
            "test/subset/data/fonts/NotoIKEAHebrewLatin-Regular.ttf"
            "test/fuzzing/fonts/kanit.ttf"
            "test/api/fonts/Mplus1p-Regular.ttf"
            "test/subset/data/fonts/Amiri-Regular.ttf" |]
         |> Array.map (fun path -> Font.create (Face.create (UpstreamFiles.read path) 0u)))

/// <summary>The code points of a mark-heavy line: letters, marks of every combining class, points, Thai vowels, joiners and the
/// code points that decompose.</summary>
let private genMarkCodepoint =
    Gen.frequency
        [ 24, Gen.choose (0x61, 0x7A)
          6, Gen.choose (0x41, 0x5A)
          3, Gen.elements [ 0x20; 0x2C; 0x2E; 0x30; 0x31 ]
          20, Gen.choose (0x300, 0x36F)
          4, Gen.elements [ 0x34F; 0x34F; 0x200D; 0x200C; 0xFE0F; 0xFE00; 0xAD; 0xA0; 0x2009 ]
          6, Gen.choose (0xC0, 0x17F)
          5, Gen.choose (0x1EA0, 0x1EF9)
          4, Gen.elements [ 0x1F82; 0x1E9B; 0x212B; 0x2126; 0x0958; 0x0344; 0x0F73; 0xFB1D; 0xFB2A; 0xFB4B; 0x01C4 ]
          12, Gen.choose (0x5D0, 0x5EA)
          18, Gen.choose (0x5B0, 0x5C7)
          3, Gen.elements [ 0x5B7; 0x5B8; 0x5B0; 0x5B4; 0x5BD; 0x5C1; 0x5C2; 0x5BC; 0x5BF ]
          10, Gen.choose (0xE01, 0xE2E)
          12, Gen.choose (0xE30, 0xE5B)
          3, Gen.elements [ 0xE33; 0xE4D; 0xE40; 0xE41; 0x0E31; 0xE34; 0xE38 ]
          3, Gen.choose (0x620, 0x65F) ]
    |> Gen.map uint32

/// <summary>Generates a chunk of a mark-heavy line: one code point, or a letter followed by one to three marks that compose with it
/// or order after it.</summary>
let private genChunk =
    Gen.frequency
        [ 5, genMarkCodepoint |> Gen.map (fun codepoint -> [| codepoint |])
          5,
          gen {
              let! letter = Gen.elements [ 0x61u; 0x65u; 0x6Fu; 0x75u; 0x69u; 0x41u; 0x45u; 0x4Fu; 0x55u; 0x63u; 0x6Eu; 0x73u; 0x5D0u; 0x5E9u; 0x5D1u; 0xE01u; 0xE02u ]
              let! count = Gen.frequency [ 5, Gen.constant 1; 3, Gen.constant 2; 1, Gen.constant 3 ]
              let! marks = Gen.arrayOfLength count (Gen.elements [ 0x300u; 0x301u; 0x302u; 0x303u; 0x308u; 0x323u; 0x31Bu; 0x327u; 0x328u; 0x306u; 0x30Cu; 0x5B7u; 0x5B4u; 0x5B0u; 0x5BDu; 0x5BCu; 0x5C1u; 0xE48u; 0xE38u; 0xE34u ])
              return Array.append [| letter |] marks
          } ]

/// <summary>Generates a mark-heavy run with every field varied, for a font and a script.</summary>
let private genMarkCase =
    gen {
        let! fontIndex = Gen.choose (0, 6)
        let! length = Gen.frequency [ 4, Gen.choose (1, 3); 8, Gen.choose (2, 12); 2, Gen.choose (13, 30) ]
        let! chunks = Gen.arrayOfLength length genChunk
        let text = Array.concat chunks
        let! script = Gen.elements [ Scripts.Latin; Scripts.Latin; Scripts.Hebrew; Scripts.Hebrew; Scripts.Thai; Scripts.Thai; Scripts.Arabic; Scripts.Common ]
        let! direction = Gen.elements [ Direction.Ltr; Direction.Ltr; Direction.Rtl ]
        let! flags = genFlags
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! clusters = genClusters text.Length
        let! wild = Gen.frequency [ 4, Gen.constant false; 1, Gen.constant true ]
        let! garbage = Gen.arrayOfLength text.Length (Gen.choose (0, Int32.MaxValue))
        let! successful = Gen.frequency [ 24, Gen.constant true; 1, Gen.constant false ]
        let! ops = Gen.frequency [ 8, Gen.constant GlyphRun.MaxOpsDefault; 3, Gen.choose (0, 400); 1, Gen.choose (0, 5000) ]
        let! invisible = Gen.elements [ 0u; 0u; 3u; 77u ]
        let! notFound = Gen.elements [ 0u; 0u; 5u ]
        let! scratch = Gen.frequency [ 6, Gen.constant 0; 1, Gen.choose (0, 0xFF) ]
        let infos =
            Sealed.init text.Length (fun index ->
                { Codepoint = text[index]
                  Mask = if wild then uint32 garbage[index] else 0u
                  Cluster = clusters[index]
                  Var1 = if wild then uint32 garbage[index] else 0u
                  Var2 = 0u })
        let run =
            { GlyphRun.empty with
                Infos = infos
                ContentType = UnicodeContent
                SegmentProperties = { Direction = direction; Script = script; Language = Language.cLocale }
                Flags = flags
                ClusterLevel = level
                Invisible = invisible
                NotFound = notFound
                Successful = successful
                ScratchFlags = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> (uint32 scratch)
                MaxOps = ops }
        return struct (fontIndex, run)
    }

/// <summary>Prepares a run as the pipeline does up to the normaliser: properties, dotted circle, clusters, native direction, the
/// shaper's pre-processing and the rotation of characters.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="run">The run.</param>
/// <returns>The run the normaliser reads.</returns>
let private prepared (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
    GlyphRun.enter run
    |> OtProps.setUnicodePropsFused plan.Map.GlobalMask
    |> OtProps.insertDottedCircle font
    |> OtProps.formClusters
    |> OtProps.ensureNativeDirection
    |> ShaperHooks.preprocess plan font
    |> OtSubstitute.rotateChars plan font plan.Props.Direction

/// <summary>Checks one mark-heavy run: the lean normaliser equals the out-buffer passes in every field.</summary>
/// <param name="case">The font index and the run.</param>
/// <returns>Whether composition shortened the run, whether the marks were reordered without shortening, and whether the shaper is
/// one the lean normaliser reads.</returns>
let private checkNormalized (case: struct (int * GlyphRun)) =
    let struct (fontIndex, run) = case
    let font = normalizerFonts.Value[fontIndex]
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> struct (false, false, false)
    | Ok shapePlan ->
        match shapePlan.Ot with
        | ValueNone -> struct (false, false, false)
        | ValueSome plan ->
            let propped =
                { GlyphRun.enter run with MaxOps = run.MaxOps }
                |> OtProps.setUnicodePropsFused plan.Map.GlobalMask
                |> OtProps.insertDottedCircle font
            expectSameRunAll "clusters" (OtProps.formClustersPass propped) (OtProps.formClusters propped)
            let entered = { GlyphRun.enter run with MaxOps = run.MaxOps }
            expectSameRunAll "unicode props" (OtProps.setUnicodeProps plan.Map.GlobalMask entered) (OtProps.setUnicodePropsFused plan.Map.GlobalMask entered)
            let input = { prepared plan font { run with Successful = true } with MaxOps = run.MaxOps }
            let expected = Normalize.runPass plan font input
            let actual = Normalize.run plan font input
            try
                expectSameRunAll "normalized" expected actual
            with error ->
                let hexText = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint.ToString "X"))
                printfn $"FAILED font {fontIndex} ops {run.MaxOps} flags {run.Flags} level {run.ClusterLevel} props {run.SegmentProperties} text {hexText} notfound {run.NotFound} {error.Message}"
                reraise ()
            let shortened = actual.Infos.Length < input.Infos.Length
            let moved = not shortened && actual.Infos.AsSpan().ToArray() <> input.Infos.AsSpan().ToArray()
            struct (shortened, moved, ShaperHooks.leanNormalisation plan)

/// <summary>Whether two compiled pair subtables are the same in every field.</summary>
/// <param name="a">The first table.</param>
/// <param name="b">The second table.</param>
/// <returns>True when every field is equal.</returns>
let private samePairTable (a: PairTable) (b: PairTable) =
    let same (x: ImmutableArray<'T>) (y: ImmutableArray<'T>) = x.AsSpan().SequenceEqual(y.AsSpan())
    a.Kind = b.Kind
    && a.Plain = b.Plain
    && a.Shift = b.Shift
    && a.FilterShift = b.FilterShift
    && a.BeyondColumn = b.BeyondColumn
    && a.Width = b.Width
    && same a.Cover b.Cover
    && same a.Keys b.Keys
    && same a.Slots b.Slots
    && same a.Filter b.Filter
    && same a.Columns b.Columns
    && same a.Values b.Values

/// <summary>Whether two nested arrays of compiled pair subtables are the same.</summary>
/// <param name="a">The first tables.</param>
/// <param name="b">The second tables.</param>
/// <returns>True when every lookup's tables are equal.</returns>
let private samePairTables (a: ImmutableArray<ImmutableArray<PairTable>>) (b: ImmutableArray<ImmutableArray<PairTable>>) =
    a.Length = b.Length
    && Seq.forall2 (fun (x: ImmutableArray<PairTable>) (y: ImmutableArray<PairTable>) -> x.Length = y.Length && Seq.forall2 samePairTable x y) a b

/// <summary>The union of the glyph sets of a map's lookups, computed from the sets the generic builders make from the subtables.</summary>
/// <param name="sets">The glyph set of every lookup of the table, the lookups outside the map empty.</param>
/// <returns>The words of the union.</returns>
let private unionOf (sets: ImmutableArray<ImmutableArray<uint64>>) =
    let length = sets |> Seq.fold (fun longest set -> max longest set.Length) 0
    Array.init length (fun word -> sets |> Seq.fold (fun bits set -> bits ||| (match word < set.Length with | true -> set[word] | false -> 0UL)) 0UL)

/// <summary>Checks that the sets and tables a plan takes from the face's caches are those the subtables make, for a font, a script,
/// a direction and some features.</summary>
/// <param name="font">The font.</param>
/// <param name="props">The segment properties.</param>
/// <param name="features">The user features.</param>
/// <returns>Whether the plan had a GSUB or GPOS lookup.</returns>
let private checkPlanSets (font: Font) (props: SegmentProperties) (features: ImmutableArray<Feature>) =
    let plan = OtPlan.create font props features
    let gsub = font.Ot.Gsub.Value
    let gpos = font.Ot.Gpos.Value
    let gsubSets = Engine.lookupSetsOf gsub plan.Map.GsubLookups
    let gposSets = Engine.gposLookupCoveredOf gpos plan.Map.GposLookups
    let sameSets (what: string) (actual: ImmutableArray<ImmutableArray<uint64>>) (expected: ImmutableArray<ImmutableArray<uint64>>) =
        Expect.equal actual.Length expected.Length $"{what}: lookups"
        Seq.iter2 (fun (x: ImmutableArray<uint64>) (y: ImmutableArray<uint64>) -> Expect.sequenceEqual (x.AsSpan().ToArray()) (y.AsSpan().ToArray()) what) actual expected
    sameSets "GSUB lookup sets" (Engine.gsubLookupCovered plan font) gsubSets
    sameSets "GPOS lookup sets" (Engine.gposLookupCovered plan font) gposSets
    Expect.sequenceEqual ((Engine.gsubCovered plan font).AsSpan().ToArray()) (unionOf gsubSets) "GSUB union"
    Expect.sequenceEqual ((Engine.gposCovered plan font).AsSpan().ToArray()) (unionOf gposSets) "GPOS union"
    let gsubGlyphs = Engine.gsubGlyphLookups plan font
    let gposGlyphs = Engine.gposGlyphLookups plan font
    Expect.sequenceEqual (gsubGlyphs.AsSpan().ToArray()) ((Engine.glyphLookupsOf gsub plan.Map.GsubLookups).AsSpan().ToArray()) "GSUB glyph lookups"
    Expect.sequenceEqual (gposGlyphs.AsSpan().ToArray()) ((Engine.gposGlyphLookupsOf gpos plan.Map.GposLookups).AsSpan().ToArray()) "GPOS glyph lookups"
    let covered = Engine.gposLookupCovered plan font
    let actual = Engine.gposPairs plan font covered gposGlyphs
    let expected = Engine.gposPairsOf gpos font.Ot.Gdef.Value plan.Map.GposLookups plan.Map.GposStages plan.Map.GlobalMask covered gposGlyphs
    Expect.isTrue (samePairTables actual.Tables expected.Tables) "pair tables"
    Expect.isTrue (samePairTables actual.PositionTables expected.PositionTables) "position tables"
    sameSets "position covered" actual.PositionCovered expected.PositionCovered
    Expect.equal actual.Enabled expected.Enabled "pairs enabled"
    Expect.equal actual.Lean expected.Lean "lean positions"
    Expect.equal actual.LeanOnly expected.LeanOnly "lean only"
    Expect.equal actual.Idle expected.Idle "idle positions"
    Expect.sequenceEqual (actual.Glyphs.AsSpan().ToArray()) (expected.Glyphs.AsSpan().ToArray()) "pair glyphs"
    not plan.Map.GsubLookups.IsEmpty || not plan.Map.GposLookups.IsEmpty

/// <summary>Generates a Latin line of letters, spaces and marks that compose with them, so that the normalizer leaves no mark
/// and the kernel's tails can take the line; a few code points the kernel refuses are mixed in.</summary>
let private genComposingCase =
    gen {
        let! fontIndex = Gen.elements [ 0; 0; 1; 2 ]
        let! length = Gen.frequency [ 3, Gen.choose (1, 3); 8, Gen.choose (4, 40); 2, Gen.choose (41, 120) ]
        let! chunks =
            Gen.arrayOfLength length
                (Gen.frequency
                    [ 10, Gen.elements [ 0x20u; 0x2Cu; 0x2Eu ] |> Gen.map (fun codepoint -> [| codepoint |])
                      30, Gen.choose (0x61, 0x7A) |> Gen.map (fun codepoint -> [| uint32 codepoint |])
                      30,
                      gen {
                          let! letter = Gen.elements [ 0x61u; 0x65u; 0x6Fu; 0x75u; 0x69u; 0x41u; 0x45u; 0x4Fu; 0x55u; 0x63u; 0x6Eu; 0x73u; 0x79u ]
                          let! mark = Gen.elements [ 0x300u; 0x301u; 0x302u; 0x303u; 0x308u; 0x323u; 0x327u; 0x328u; 0x306u; 0x30Cu; 0x31Bu ]
                          return [| letter; mark |]
                      }
                      6,
                      gen {
                          let! letter = Gen.elements [ 0x61u; 0x65u; 0x6Fu; 0x75u ]
                          let! first = Gen.elements [ 0x302u; 0x306u; 0x31Bu; 0x323u ]
                          let! second = Gen.elements [ 0x301u; 0x300u; 0x303u; 0x323u; 0x309u ]
                          return [| letter; first; second |]
                      }
                      5, Gen.elements [ 0xE9u; 0xE8u; 0xF4u; 0x1EBFu; 0x1EA1u; 0x1EE9u; 0xC0u ] |> Gen.map (fun codepoint -> [| codepoint |])
                      1, Gen.elements [ 0x34Fu; 0x200Du; 0xADu; 0x2044u; 0xA0u; 0xFE0Fu ] |> Gen.map (fun codepoint -> [| codepoint |]) ])
        let text = Array.concat chunks
        let! direction = Gen.elements [ Direction.Ltr; Direction.Ltr; Direction.Rtl ]
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! flags = Gen.frequency [ 6, Gen.constant BufferFlags.Default; 2, genFlags ]
        let! clusters = genClusters text.Length
        let! features = genFeatures fontIndex
        let infos =
            Sealed.init text.Length (fun index ->
                { Codepoint = text[index]; Mask = 0u; Cluster = clusters[index]; Var1 = 0u; Var2 = 0u })
        let run =
            { GlyphRun.empty with
                Infos = infos
                ContentType = UnicodeContent
                SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = Language.cLocale }
                Flags = flags
                ClusterLevel = level }
        return struct (fontIndex, features, run)
    }

/// <summary>Shapes a run through the lean general pipeline and the reference general pipeline and compares every field.</summary>
/// <param name="font">The font.</param>
/// <param name="features">The user features.</param>
/// <param name="run">The run.</param>
/// <returns>None when the run has no ot plan, else whether the kernel's tails shaped the line.</returns>
let private leanAgainstReference (font: Font) (features: ImmutableArray<Feature>) (run: GlyphRun) =
    match Execute.createPlan font run.SegmentProperties features ValueNone with
    | Error _ -> ValueNone
    | Ok plan ->
        match plan.Ot with
        | ValueNone -> ValueNone
        | ValueSome otPlan ->
            match run.Infos.Length with
            | 0 -> ValueNone
            | _ ->
                let reference = Execute.runGeneral plan font features run
                let struct (handedOff, lean) = SimpleRun.shapeGeneralPath plan otPlan font run
                try
                    match reference, lean with
                    | Ok expected, Ok actual -> expectSameRunAll "lean" expected actual
                    | Error expected, Error actual -> Expect.equal actual expected "errors"
                    | Ok _, Error _ -> failtest "the lean pipeline refused a run the reference shaped"
                    | Error _, Ok _ -> failtest "the lean pipeline shaped a run the reference refused"
                with error ->
                    let hexText = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint.ToString "X"))
                    printfn $"FAILED lean: ops {run.MaxOps} flags {run.Flags} level {run.ClusterLevel} props {run.SegmentProperties} text {hexText} {error.Message}"
                    reraise ()
                ValueSome handedOff

/// <summary>The lines the kernel widens to and the general pipeline's lean forms, equal to the general pipeline in every field.</summary>
[<Tests>]
let generalLineTests =
    testList "general lines" [
        test "a right-to-left run of a left-to-right script takes the kernel and equals the general pipeline in every field" {
            let cases = Gen.sampleWithSeed (Rnd Seed) 30 6000 genRtlCase
            let check (struct (fontIndex, features, run: GlyphRun)) =
                try
                    expectSame fonts.Value[fontIndex] features run
                    tookKernel fonts.Value[fontIndex] features run
                with error ->
                    printfn $"FAILED font {fontIndex} features {features.Length} ops {run.MaxOps} flags {run.Flags} level {run.ClusterLevel} props {run.SegmentProperties} text {run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)} {error.Message}"
                    reraise ()
            let taken = cases |> Seq.map check |> Seq.filter id |> Seq.length
            printfn $"right-to-left property: {cases.Length} cases, {taken} took the kernel"
            Expect.isGreaterThan taken 800 "right-to-left Latin lines take the kernel"
        }

        test "the lean normaliser equals the out-buffer passes in every field over mark-heavy runs" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 1UL)) 30 12000 genMarkCase
            let results = cases |> Seq.map checkNormalized |> Seq.toArray
            let count pick = results |> Array.filter pick |> Array.length
            let composed = count (fun (struct (shortened, _, _)) -> shortened)
            let leaned = count (fun (struct (_, _, lean)) -> lean)
            let changed = count (fun (struct (_, moved, _)) -> moved)
            printfn $"normaliser property: {cases.Length} cases, {leaned} with a shaper the lean normaliser reads, {composed} shortened by composition, {changed} reordered without shortening"
            Expect.isGreaterThan composed 1500 "many runs compose"
            Expect.isGreaterThan changed 100 "many runs are reordered"
            Expect.isGreaterThan leaned 5000 "the lean normaliser is taken often"
        }

        test "the face keeps the glyph lookup tables of a GSUB map and of a GPOS map apart when both list the same lookups" {
            let font = fonts.Value[0]
            let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
            let plan = OtPlan.create font props ImmutableArray<Feature>.Empty
            Expect.isFalse plan.Map.GposLookups.IsEmpty "the plan has GPOS lookups"
            let shared = { plan with Map = { plan.Map with GsubLookups = plan.Map.GposLookups } }
            let gsub = Engine.gsubGlyphLookups shared font
            let gpos = Engine.gposGlyphLookups shared font
            Expect.sequenceEqual (gsub.AsSpan().ToArray()) ((Engine.glyphLookupsOf font.Ot.Gsub.Value shared.Map.GsubLookups).AsSpan().ToArray()) "GSUB table of the shared lookups"
            Expect.sequenceEqual (gpos.AsSpan().ToArray()) ((Engine.gposGlyphLookupsOf font.Ot.Gpos.Value shared.Map.GposLookups).AsSpan().ToArray()) "GPOS table of the shared lookups"
            Expect.isFalse (gsub.AsSpan().SequenceEqual(gpos.AsSpan())) "the two tables differ, so that one served for the other would be seen"
            Expect.isTrue ((Engine.gsubGlyphLookups shared font).Equals(Engine.gsubGlyphLookups shared font)) "the face keeps the table: asking again gives the same array"
        }

        test "a plan built from the face's cached sets and pair tables equals the plan built from the subtables" {
            let cases = 6000
            let rnd = Random 4242
            let scripts = [| Scripts.Latin; Scripts.Latin; Scripts.Latin; Scripts.Cyrillic; Scripts.Greek; Scripts.Hebrew; Scripts.Thai; Scripts.Common |]
            let directions = [| Direction.Ltr; Direction.Ltr; Direction.Rtl |]
            let featureSets = Gen.sampleWithSeed (Rnd (Seed + 2UL)) 30 cases (Gen.choose (0, fontSources.Length - 1) |> Gen.bind (fun index -> genFeatures index |> Gen.map (fun features -> struct (index, features))))
            let withLookups =
                featureSets
                |> Seq.filter (fun (struct (index, features)) ->
                    let props = { Direction = directions[rnd.Next directions.Length]; Script = scripts[rnd.Next scripts.Length]; Language = Language.cLocale }
                    checkPlanSets fonts.Value[index] props features)
                |> Seq.length
            printfn $"plan sets property: {cases} cases, {withLookups} with a GSUB or GPOS lookup"
            Expect.isGreaterThan withLookups 2000 "most plans have lookups"
        }

        test "a second plan on a face reuses the face's sets and pair tables and allocates a fraction of the first" {
            let font = fonts.Value[0]
            let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
            let withFeature tag value = ImmutableArray.Create { Tag = Tag.ofString tag; Value = value; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            let build features =
                OtPlan.create font props features
            let first = build ImmutableArray.Empty
            let second = build (withFeature "liga" 0u)
            let firstCovered = Engine.gposLookupCovered first font
            let secondCovered = Engine.gposLookupCovered second font
            let reused =
                first.Map.GposLookups
                |> Seq.filter (fun lookup -> second.Map.GposLookups |> Seq.exists (fun other -> other.LookupIndex = lookup.LookupIndex))
                |> Seq.filter (fun lookup -> firstCovered[lookup.LookupIndex].Equals secondCovered[lookup.LookupIndex] && not firstCovered[lookup.LookupIndex].IsEmpty)
                |> Seq.length
            Expect.isGreaterThan reused 5 "the lookups two plans share have one glyph set between them"
            Execute.createPlan font props (withFeature "kern" 0u) ValueNone |> ignore
            let measure () =
                let before = GC.GetAllocatedBytesForCurrentThread()
                Execute.createPlan font props (withFeature "kern" 1u) ValueNone |> ignore
                GC.GetAllocatedBytesForCurrentThread() - before
            measure () |> ignore
            let bytes = measure ()
            printfn $"second plan on Roboto: {bytes} B"
            Expect.isLessThan bytes 160000L "a second plan on a warm face allocates the map and the two glyph tables, not the sets and pair tables"
        }

        test "the lean general pipeline equals the reference general pipeline in every field over mark-heavy runs, the kernel's tails taking the lines whose marks compose" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 3UL)) 30 12000 genMarkCase
            let mutable handedOff = 0
            let mutable compared = 0
            for struct (fontIndex, run: GlyphRun) in cases do
                let font = normalizerFonts.Value[fontIndex]
                let features = ImmutableArray<Feature>.Empty
                let outcome = leanAgainstReference font features run
                compared <- compared + (match outcome with | ValueSome _ -> 1 | ValueNone -> 0)
                handedOff <- handedOff + (match outcome with | ValueSome true -> 1 | _ -> 0)
            printfn $"lean general property: {cases.Length} cases, {compared} with a plan, {handedOff} handed to the kernel's tails"
            Expect.isGreaterThan handedOff 100 "lines whose marks compose are handed to the kernel's tails"
        }

        test "the lean general pipeline hands lines whose marks compose to the kernel's tails and equals the reference in every field" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 4UL)) 30 8000 genComposingCase
            let outcomes = cases |> Seq.map (fun (struct (fontIndex, features, run)) -> leanAgainstReference normalizerFonts.Value[fontIndex] features run) |> Seq.toArray
            let handedOff = outcomes |> Array.filter (fun outcome -> outcome = ValueSome true) |> Array.length
            printfn $"composing lines property: {cases.Length} cases, {handedOff} handed to the kernel's tails"
            Expect.isGreaterThan handedOff 800 "lines whose marks compose are handed to the kernel's tails"
        }
    ]

/// <summary>The fonts of the marked-line properties: the mark-heavy fonts, two Latin fonts stripped of their layout tables, and a font
/// whose only GPOS lookup is a mark-to-base attachment.</summary>
let private markedFonts =
    lazy (Array.concat [ normalizerFonts.Value; [| fonts.Value[1]; fonts.Value[3]; fonts.Value[22] |] ])

/// <summary>Generates the code points of a line that keeps its marks: Hebrew letters with points and cantillation, Thai and Lao
/// consonants with vowels, tone marks and SARA AM, Arabic marks, and Latin letters with the marks that do not compose.</summary>
let private genMarkedCodepoint =
    Gen.frequency
        [ 14, Gen.choose (0x5D0, 0x5EA)
          10, Gen.choose (0x5B0, 0x5C7)
          6, Gen.choose (0x591, 0x5AF)
          10, Gen.choose (0xE01, 0xE2E)
          12, Gen.elements [ 0xE31; 0xE34; 0xE35; 0xE36; 0xE37; 0xE38; 0xE39; 0xE3A; 0xE47; 0xE48; 0xE49; 0xE4A; 0xE4B; 0xE4C; 0xE4D; 0xE4E; 0xE33 ]
          6, Gen.choose (0xE81, 0xEAE)
          8, Gen.elements [ 0xEB1; 0xEB4; 0xEB5; 0xEB6; 0xEB7; 0xEB8; 0xEB9; 0xEBB; 0xEBC; 0xEC8; 0xEC9; 0xECA; 0xECB; 0xECC; 0xECD; 0xEB3 ]
          6, Gen.choose (0x64B, 0x65F)
          14, Gen.choose (0x61, 0x7A)
          6, Gen.choose (0x300, 0x36F)
          4, Gen.elements [ 0x20; 0x20; 0x2C; 0x2E; 0xA0; 0x200D; 0x200C; 0x34F ]
          2, Gen.choose (0x1100, 0x11FF) ]
    |> Gen.map uint32

/// <summary>Generates a chunk of a line of one script family that keeps its marks: one code point, or a letter followed by one to
/// three marks of its script.</summary>
/// <param name="family">0 Hebrew, 1 Thai, 2 Lao, 3 Latin, 4 Arabic, anything else mixed.</param>
let private genMarkedChunk (family: int) =
    let letters, marks =
        match family with
        | 0 -> Gen.choose (0x5D0, 0x5EA), Gen.frequency [ 9, Gen.choose (0x5B0, 0x5BC); 2, Gen.choose (0x5BD, 0x5C7); 1, Gen.choose (0x591, 0x5AF) ]
        | 1 -> Gen.choose (0xE01, 0xE2E), Gen.elements [ 0xE31; 0xE34; 0xE35; 0xE36; 0xE37; 0xE38; 0xE39; 0xE47; 0xE48; 0xE49; 0xE4A; 0xE4B; 0xE4C; 0xE4D; 0xE33 ]
        | 2 -> Gen.choose (0xE81, 0xEAE), Gen.elements [ 0xEB1; 0xEB4; 0xEB5; 0xEB8; 0xEC8; 0xEC9; 0xECA; 0xECD; 0xEB3 ]
        | 3 -> Gen.choose (0x61, 0x7A), Gen.choose (0x300, 0x36F)
        | 4 -> Gen.choose (0x628, 0x64A), Gen.choose (0x64B, 0x65F)
        | _ -> Gen.frequency [ 4, Gen.choose (0x5D0, 0x5EA); 4, Gen.choose (0xE01, 0xE2E); 2, Gen.choose (0x61, 0x7A); 2, Gen.choose (0x628, 0x64A) ], Gen.frequency [ 5, Gen.choose (0x5B0, 0x5C7); 5, Gen.choose (0xE31, 0xE4E); 2, Gen.choose (0x300, 0x36F); 2, Gen.choose (0x64B, 0x65F) ]
    Gen.frequency
        [ 3, genMarkedCodepoint |> Gen.map (fun codepoint -> [| codepoint |])
          9,
          gen {
              let! letter = letters
              let! count = Gen.frequency [ 5, Gen.constant 1; 3, Gen.constant 2; 1, Gen.constant 3 ]
              let! following = Gen.arrayOfLength count marks
              return Array.append [| uint32 letter |] (Array.map uint32 following)
          } ]

/// <summary>Generates a line that keeps its marks, every field of the run varied.</summary>
let private genMarkedCase =
    gen {
        let! fontIndex = Gen.frequency [ 6, Gen.constant 3; 6, Gen.constant 4; 2, Gen.constant 0; 1, Gen.constant 1; 1, Gen.constant 2; 1, Gen.constant 5; 2, Gen.constant 6; 1, Gen.constant 7; 1, Gen.constant 8; 3, Gen.constant 9 ]
        let! family =
            match fontIndex with
            | 3 -> Gen.frequency [ 9, Gen.constant 0; 1, Gen.constant 5 ]
            | 4 -> Gen.frequency [ 7, Gen.constant 1; 2, Gen.constant 2; 1, Gen.constant 5 ]
            | 6 -> Gen.frequency [ 6, Gen.constant 4; 2, Gen.constant 3; 1, Gen.constant 5 ]
            | _ -> Gen.frequency [ 5, Gen.constant 3; 2, Gen.constant 0; 2, Gen.constant 1; 1, Gen.constant 5 ]
        let! length = Gen.frequency [ 3, Gen.choose (1, 2); 8, Gen.choose (3, 14); 2, Gen.choose (15, 40) ]
        let! chunks = Gen.arrayOfLength length (genMarkedChunk family)
        let text = Array.concat chunks
        let script =
            match family with
            | 0 -> Scripts.Hebrew
            | 1 -> Scripts.Thai
            | 2 -> Scripts.Lao
            | 3 -> Scripts.Latin
            | 4 -> Scripts.Arabic
            | _ -> Scripts.Common
        let! script = Gen.frequency [ 9, Gen.constant script; 1, Gen.elements [ Scripts.Latin; Scripts.Common; Scripts.Hebrew; Scripts.Thai ] ]
        let! direction = Gen.elements [ Direction.Ltr; Direction.Rtl; Direction.Rtl; Direction.Ltr ]
        let! flags = genFlags
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! clusters = genClusters text.Length
        let! ops = Gen.frequency [ 8, Gen.constant GlyphRun.MaxOpsDefault; 3, Gen.choose (0, 300); 1, Gen.choose (0, 5000) ]
        let! successful = Gen.frequency [ 24, Gen.constant true; 1, Gen.constant false ]
        let infos =
            Sealed.init text.Length (fun index -> { Codepoint = text[index]; Mask = 0u; Cluster = clusters[index]; Var1 = 0u; Var2 = 0u })
        let run =
            { GlyphRun.empty with
                Infos = infos
                ContentType = UnicodeContent
                SegmentProperties = { Direction = direction; Script = script; Language = Language.cLocale }
                Flags = flags
                ClusterLevel = level
                Successful = successful
                MaxOps = ops }
        return struct (fontIndex, run)
    }

/// <summary>Builds the run after the GSUB engine, the state the positioning stages read, for a plan and a run.</summary>
/// <param name="plan">The ot plan.</param>
/// <param name="font">The font.</param>
/// <param name="run">The run.</param>
/// <returns>The substituted run.</returns>
let private substitutedOf (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
    let normalized = OtShaper.normalised plan font (GlyphRun.enter run)
    match OtSubstitute.afterNormalize plan font normalized with
    | Ok substituted -> substituted
    | Error _ -> normalized

/// <summary>Whether two outcomes of an attachment subtable are the same in every field.</summary>
/// <param name="a">The first outcome.</param>
/// <param name="b">The second outcome.</param>
/// <returns>True when every field is equal.</returns>
let private sameOutcome (a: AttachOutcome) (b: AttachOutcome) =
    a.Applied = b.Applied && a.Kind = b.Kind && a.Next = b.Next && a.Position = b.Position && a.Attached = b.Attached
    && a.BreakFrom = b.BreakFrom && a.BreakTo = b.BreakTo && a.ConcatFrom = b.ConcatFrom && a.Partner = b.Partner
    && a.ExitX = b.ExitX && a.ExitY = b.ExitY && a.EntryX = b.EntryX && a.EntryY = b.EntryY
    && a.Direction = b.Direction && a.RightToLeft = b.RightToLeft && a.LastBase = b.LastBase && a.LastBaseUntil = b.LastBaseUntil

/// <summary>Offers every glyph of a substituted run to every attachment subtable of a plan, through the general reader and through
/// the compiled table, with a fresh cache of the last base and with a random one, and compares the outcomes.</summary>
/// <param name="rnd">The source of the caches.</param>
/// <param name="case">The font index and the run.</param>
/// <returns>The number of visits compared and the number of those that applied through a compiled table.</returns>
let private compareCompiledVisits (rnd: Random) (case: struct (int * GlyphRun)) =
    let struct (fontIndex, run) = case
    let font = markedFonts.Value[fontIndex]
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> struct (0, 0)
    | Ok shapePlan ->
        match shapePlan.Ot with
        | ValueNone -> struct (0, 0)
        | ValueSome plan ->
            let substituted = substitutedOf plan font { run with Successful = true }
            let length = substituted.Infos.Length
            let positions = Font.hAdvancePositions font (substituted.Infos.AsSpan())
            let mutable visits = 0
            let mutable applied = 0
            match plan.ApplyGpos && length > 0 with
            | false -> struct (0, 0)
            | true ->
                let table = font.Ot.Gpos.Value
                let gdef = font.Ot.Gdef.Value
                for map in plan.Map.GposLookups do
                    let accel = table.Lookups[map.LookupIndex].Value
                    let props = Gdef.sanitizeLookupProps gdef accel.Props
                    let matcher = Matching.matcherFor 1 false props map.Mask map.AutoZwnj map.AutoZwj false 0uy
                    let env = { Font = font; Direction = substituted.SegmentProperties.Direction; Horizontal = Direction.isHorizontal substituted.SegmentProperties.Direction; Matcher = matcher; Gdef = gdef }
                    let compiled = MarkTables.ofLookup accel.Subtables
                    for k in 0 .. accel.Subtables.Length - 1 do
                        let subtable = accel.Subtables[k].Subtable
                        for index in 0 .. length - 1 do
                            for lastBase, lastBaseUntil in [ struct (-1, 0); struct (rnd.Next(-1, length), rnd.Next(0, length + 1)) ] |> List.map (fun (struct (a, b)) -> a, b) do
                                let infos = substituted.Infos.AsSpan()
                                let glyphProps = substituted.Props.AsSpan()
                                let general = GposAttach.applySubtable env subtable infos glyphProps (positions.AsSpan()) index lastBase lastBaseUntil
                                let fast = GposAttach.applyCompiled env subtable compiled[k] infos glyphProps (positions.AsSpan()) index lastBase lastBaseUntil
                                Expect.isTrue (sameOutcome general fast) $"attach outcome of subtable {k} of lookup {map.LookupIndex} at {index} with cache ({lastBase}, {lastBaseUntil})"
                                visits <- visits + 1
                                match compiled[k], fast.Applied with
                                | ValueSome _, true -> applied <- applied + 1
                                | _ -> ()
                struct (visits, applied)

[<Tests>]
let markedLineTests =
    testList "marked lines" [
        test "composing through the hash tables equals the binary searches over every pair of the tables and over random pairs" {
            let small = UcdTables.dm2U32 |> Seq.map (fun value -> struct (value >>> 21, ((value >>> 14) &&& 0x7Fu) ||| 0x300u)) |> Seq.toArray
            let wide = UcdTables.dm2U64 |> Seq.map (fun value -> struct (uint32 (value >>> 42), uint32 ((value >>> 21) &&& 0x1FFFFFUL))) |> Seq.toArray
            let rnd = Random 77
            let random =
                Array.init 40000 (fun index ->
                    match index % 4 with
                    | 0 -> struct (uint32 (rnd.Next(0, 0x30000)), uint32 (rnd.Next(0x2F0, 0x3A0)))
                    | 1 -> struct (uint32 (rnd.Next(0x41, 0x1F0)), uint32 (rnd.Next(0x300, 0x380)))
                    | 2 -> struct (uint32 (rnd.Next(0x5D0, 0x5EB)), uint32 (rnd.Next(0x5B0, 0x5C8)))
                    | _ -> struct (uint32 (rnd.Next()), uint32 (rnd.Next())))
            let neighbours = Array.append small wide |> Array.collect (fun (struct (a, b)) -> [| struct (a, b + 1u); struct (a + 1u, b); struct (b, a); struct (a, b) |])
            let all = Array.concat [ small; wide; random; neighbours ]
            for struct (a, b) in all do
                Expect.equal (Unicode.compose a b) (Unicode.composeBySearch a b) $"compose U+{a:X4} U+{b:X4}"
            printfn $"compose property: {all.Length} pairs"
            Expect.isGreaterThan all.Length 5000 "many pairs"
        }

        test "the low tables of the Unicode properties and of every font's cmap equal the lookups they are built from" {
            let mutable checkedProps = 0
            for u in 0 .. UnicodeProps.LowLimit - 1 do
                Expect.equal UnicodeProps.lowTable[u] (UnicodeProps.compute (uint32 u)) $"properties of U+{u:X4}"
                checkedProps <- checkedProps + 1
            let mutable checkedGlyphs = 0
            for font in markedFonts.Value do
                let cmap = font.Ot.Cmap.Value
                for u in 0u .. 0x2200u do
                    let expected = Cmap.nominalGlyphGeneral cmap.Table cmap.Best cmap.Remap u
                    Expect.equal (Cmap.nominalGlyph cmap u) expected $"glyph of U+{u:X4}"
                    checkedGlyphs <- checkedGlyphs + 1
            printfn $"low tables property: {checkedProps} property entries, {checkedGlyphs} glyph lookups"
            Expect.isGreaterThan (checkedProps + checkedGlyphs) 5000 "many entries"
        }

        test "the properties loop over the low code points equals the unfolded properties in every field" {
            let gen =
                gen {
                    let! length = Gen.frequency [ 2, Gen.choose (1, 3); 8, Gen.choose (4, 40); 1, Gen.choose (41, 200) ]
                    let! text = Gen.arrayOfLength length (Gen.frequency [ 30, genMarkedCodepoint; 6, Gen.choose (0, 0xFFF) |> Gen.map uint32; 4, Gen.choose (0x2000, 0x20FF) |> Gen.map uint32; 1, Gen.elements [ 0x200Du; 0x2044u; 0x2019u; 0x201Cu; 0x2014u ]; 1, Gen.choose (0x1000, 0x1FFFF) |> Gen.map uint32 ])
                    let! scratch = Gen.frequency [ 5, Gen.constant 0; 1, Gen.choose (0, 0xFF) ]
                    let! mask = Gen.elements [ 0u; 1u; 0x80000000u; 0xFFFFFFFFu ]
                    return struct (text, scratch, mask)
                }
            let cases = Gen.sampleWithSeed (Rnd (Seed + 10UL)) 30 8000 gen
            let mutable lowOnly = 0
            for struct (text, scratch, mask) in cases do
                let infos = Sealed.init text.Length (fun index -> { Codepoint = text[index]; Mask = uint32 index; Cluster = uint32 index; Var1 = 7u; Var2 = 9u })
                let run =
                    { GlyphRun.empty with Infos = infos; ContentType = UnicodeContent; ScratchFlags = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> (uint32 scratch) }
                expectSameRunAll "properties" (OtProps.setUnicodeProps mask run) (OtProps.setUnicodePropsFused mask run)
                match text |> Array.forall (fun codepoint -> codepoint < uint32 UnicodeProps.LowLimit) with
                | true -> lowOnly <- lowOnly + 1
                | false -> ()
            printfn $"low properties property: {cases.Length} cases, {lowOnly} of low code points only"
            Expect.isGreaterThan lowOnly 2500 "many lines are of low code points only"
        }

        test "an attachment subtable's compiled table gives the outcome of the general reader in every field, for every glyph and every cache" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 11UL)) 30 3000 genMarkedCase
            let rnd = Random 4711
            let totals = cases |> Seq.map (compareCompiledVisits rnd) |> Seq.toArray
            let visits = totals |> Array.sumBy (fun (struct (visits, _)) -> visits)
            let applied = totals |> Array.sumBy (fun (struct (_, applied)) -> applied)
            printfn $"compiled attachment property: {cases.Length} cases, {visits} visits compared, {applied} applied through a compiled table"
            Expect.isGreaterThan visits 20000 "many visits are compared"
            Expect.isGreaterThan applied 3000 "many attachments go through compiled tables"
        }

        test "the marked-line positioning equals the reference general pipeline in every field over lines of Hebrew, Thai, Lao and Latin that keep their marks" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 12UL)) 30 12000 genMarkedCase
            let mutable compared = 0
            let mutable marked = 0
            for struct (fontIndex, run: GlyphRun) in cases do
                let font = markedFonts.Value[fontIndex]
                let outcome = leanAgainstReference font ImmutableArray<Feature>.Empty run
                compared <- compared + (match outcome with | ValueSome _ -> 1 | ValueNone -> 0)
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Ok shapePlan ->
                    match shapePlan.Ot with
                    | ValueSome plan when run.Infos.Length > 0 ->
                        let substituted = substitutedOf plan font (GlyphRun.enter run)
                        marked <- marked + (match SimpleRun.isMarkedLine plan substituted with | true -> 1 | false -> 0)
                    | _ -> ()
                | Error _ -> ()
            printfn $"marked line property: {cases.Length} cases, {compared} with a plan, {marked} positioned by the marked-line path"
            Expect.isGreaterThan marked 2500 "many lines take the marked-line path"
        }

        test "the planned GPOS pass, with its compiled marks and its futile lookups skipped, equals the general loop in every field, with and without unsafe-to-concat" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 13UL)) 30 6000 genMarkedCase
            let mutable compared = 0
            let mutable concat = 0
            for struct (fontIndex, run: GlyphRun) in cases do
                let font = markedFonts.Value[fontIndex]
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Error _ -> ()
                | Ok shapePlan ->
                    match shapePlan.Ot with
                    | ValueNone -> ()
                    | ValueSome plan ->
                        match plan.ApplyGpos && run.Infos.Length > 0 with
                        | false -> ()
                        | true ->
                            let substituted = substitutedOf plan font (GlyphRun.enter run)
                            let defaulted = { substituted with Positions = Font.hAdvancePositions font (substituted.Infos.AsSpan()); HavePositions = true }
                            let reference = Engine.positionPlannedReference plan font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups defaulted
                            let planned = Engine.positionPlanned plan font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups shapePlan.GposPairs defaulted
                            expectSameRunAll "planned" reference planned
                            Expect.equal planned.ScratchFlags reference.ScratchFlags "scratch flags"
                            Expect.equal planned.MaxOps reference.MaxOps "operation budget"
                            Expect.equal planned.Successful reference.Successful "success"
                            compared <- compared + 1
                            concat <- concat + (match run.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default with | true -> 1 | false -> 0)
            printfn $"planned GPOS property: {cases.Length} cases, {compared} compared, {concat} asking for unsafe-to-concat"
            Expect.isGreaterThan compared 2500 "many runs are compared"
            Expect.isGreaterThan concat 500 "many runs ask for unsafe-to-concat"
        }

        test "a Hebrew line with points, a Thai line and a French line with combining marks allocate no more than a quarter above the reference pipeline, and the bytes per code point of a general line are pinned" {
            let measure (font: Font) (text: string) script =
                let bytes = Text.Encoding.UTF8.GetBytes text
                let run =
                    GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty
                    |> GlyphRun.guessSegmentProperties Language.cLocale
                let run = { run with SegmentProperties = { run.SegmentProperties with Script = script } }
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Error _ -> failtest "no plan"
                | Ok plan ->
                    let lean = Allocation.bytesAllocatedAtBest (fun () -> Execute.runLean plan font ImmutableArray<Feature>.Empty run |> ignore)
                    let reference = Allocation.bytesAllocatedAtBest (fun () -> Execute.runGeneral plan font ImmutableArray<Feature>.Empty run |> ignore)
                    struct (lean, reference, run.Infos.Length)
            let hebrew = String.Join (" ", [ for _ in 1 .. 12 -> "\u05E9\u05B8\u05C1\u05DC\u05D5\u05B9\u05DD \u05D1\u05B0\u05BC\u05E8\u05B5\u05D0\u05E9\u05B4\u05C1\u05D9\u05EA" ])
            let thai = String.Join (" ", [ for _ in 1 .. 12 -> "\u0E2A\u0E27\u0E31\u0E2A\u0E14\u0E35\u0E04\u0E23\u0E31\u0E1A \u0E19\u0E49\u0E33\u0E43\u0E08" ])
            let struct (hebrewLean, hebrewReference, hebrewGlyphs) = measure normalizerFonts.Value[3] hebrew Scripts.Hebrew
            let struct (thaiLean, thaiReference, thaiGlyphs) = measure normalizerFonts.Value[4] thai Scripts.Thai
            printfn $"allocation: Hebrew line of {hebrewGlyphs} code points {hebrewLean} B lean, {hebrewReference} B reference; Thai line of {thaiGlyphs} code points {thaiLean} B lean, {thaiReference} B reference"
            Expect.isLessThan (hebrewLean * 4L) (hebrewReference * 5L) "the Hebrew line allocates less than a quarter above the reference"
            Expect.isLessThan (thaiLean * 4L) (thaiReference * 5L) "the Thai line allocates less than a quarter above the reference"
            let french = String.Join (" ", [ for _ in 1 .. 12 -> "Le cafe\u0301 de l'e\u0301te\u0301 a\u0300 Paris, a\u0302ge u\u0300" ])
            let struct (frenchLean, frenchReference, frenchGlyphs) = measure normalizerFonts.Value[0] french Scripts.Latin
            printfn $"allocation: French line with combining marks of {frenchGlyphs} code points {frenchLean} B lean, {frenchReference} B reference"
            Expect.isLessThan hebrewLean (int64 hebrewGlyphs * 60L) "the Hebrew line allocates under 60 bytes per code point"
            Expect.isLessThan thaiLean (int64 thaiGlyphs * 180L) "the Thai line allocates under 180 bytes per code point"
            Expect.isLessThan frenchLean (int64 frenchGlyphs * 110L) "the French line allocates under 110 bytes per code point"
        }
    ]

/// <summary>Shapes a run on the working set and through the reference general pipeline and compares every field.</summary>
/// <param name="font">The font.</param>
/// <param name="features">The user features.</param>
/// <param name="run">The run.</param>
/// <returns>Whether the working set shaped the run, none when the run has no ot plan.</returns>
let private ownedAgainstReference (font: Font) (features: ImmutableArray<Feature>) (run: GlyphRun) =
    match Execute.createPlan font run.SegmentProperties features ValueNone with
    | Error _ -> ValueNone
    | Ok plan ->
        match plan.Ot with
        | ValueNone -> ValueNone
        | ValueSome otPlan ->
            match run.Infos.Length with
            | 0 -> ValueNone
            | _ ->
                let reference = Execute.runGeneral plan font features run
                let before = struct (run.Infos.AsSpan().ToArray(), run.Props.AsSpan().ToArray(), run.Positions.AsSpan().ToArray())
                let owned = GeneralLine.tryShape plan otPlan font run
                let lean = Execute.runLean plan font features run
                try
                    let struct (infos, props, positions) = before
                    Expect.sequenceEqual (run.Infos.AsSpan().ToArray()) infos "the input infos are untouched"
                    Expect.sequenceEqual (run.Props.AsSpan().ToArray()) props "the input props are untouched"
                    Expect.sequenceEqual (run.Positions.AsSpan().ToArray()) positions "the input positions are untouched"
                    match reference, lean with
                    | Ok expected, Ok actual -> expectSameRunAll "lean" expected actual
                    | Error expected, Error actual -> Expect.equal actual expected "errors"
                    | Ok _, Error _ -> failtest "the lean route refused a run the reference shaped"
                    | Error _, Ok _ -> failtest "the lean route shaped a run the reference refused"
                    match reference, owned with
                    | Ok expected, ValueSome (Ok actual) -> expectSameRunAll "working set" expected actual
                    | Error expected, ValueSome (Error actual) -> Expect.equal actual expected "errors"
                    | _, ValueNone -> ()
                    | Ok _, ValueSome (Error _) -> failtest "the working set refused a run the reference shaped"
                    | Error _, ValueSome (Ok _) -> failtest "the working set shaped a run the reference refused"
                with error ->
                    let hexText = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint.ToString "X"))
                    printfn $"FAILED working set: ops {run.MaxOps} flags {run.Flags} level {run.ClusterLevel} props {run.SegmentProperties} successful {run.Successful} text {hexText} {error.Message}"
                    reraise ()
                ValueSome owned.IsSome

/// <summary>Compares the outcome of a chained-context subtable matched through its dense sets with the outcome of the general match, in every field.</summary>
/// <param name="what">What was matched.</param>
/// <param name="expected">The general outcome.</param>
/// <param name="actual">The outcome through the dense sets.</param>
let private expectSameContext (what: string) (expected: ContextOutcome) (actual: ContextOutcome) =
    Expect.equal actual.Matched expected.Matched $"{what}: matched"
    Expect.equal actual.Chain expected.Chain $"{what}: chain"
    Expect.equal actual.InputCount expected.InputCount $"{what}: input count"
    Expect.equal actual.MatchEnd expected.MatchEnd $"{what}: match end"
    Expect.equal actual.Records expected.Records $"{what}: records"
    Expect.equal actual.StartIndex expected.StartIndex $"{what}: start index"
    Expect.equal actual.EndIndex expected.EndIndex $"{what}: end index"
    Expect.equal actual.Owed expected.Owed $"{what}: owed"
    Expect.equal actual.AfterConcat expected.AfterConcat $"{what}: after concat"

/// <summary>Matches every chained-context subtable of the third format of a font through its dense sets and through the general
/// match, on random runs drawn from the glyphs its coverages hold, with random glyph classes, masks and lookup flags.</summary>
/// <param name="rnd">The random generator.</param>
/// <param name="font">The font.</param>
/// <returns>The number of subtables compared and the number of matches that matched.</returns>
let private compareDenseChains (rnd: Random) (font: Font) =
    let table = font.Ot.Gsub.Value
    let gdef = font.Ot.Gdef.Value
    let mutable compared = 0
    let mutable matched = 0
    let mutable refused = 0
    for lookupIndex in 0 .. table.Lookups.Length - 1 do
        let accel = table.Lookups[lookupIndex].Value
        for subtable in 0 .. accel.Subtables.Length - 1 do
            match accel.Subtables[subtable].Subtable, ChainSets.ofSubtable accel.Subtables[subtable] with
            | SubstChainContext (ChainCoverageRule (backtrack, input, lookahead, records) as rules), ValueSome sets ->
                let pool =
                    Array.concat [ for coverage in Seq.concat [ Seq.ofArray (backtrack.AsSpan().ToArray()); Seq.ofArray (input.AsSpan().ToArray()); Seq.ofArray (lookahead.AsSpan().ToArray()) ] -> (Coverage.glyphs coverage).AsSpan().ToArray() ]
                    |> Array.truncate 400
                for _ in 1 .. 40 do
                    let length = rnd.Next(1, 10)
                    let glyphs = Array.init length (fun _ -> match pool.Length > 0 && rnd.Next 10 < 8 with | true -> pool[rnd.Next pool.Length] | false -> uint32 (rnd.Next 600))
                    let infos = Array.init length (fun index -> { Codepoint = glyphs[index]; Mask = (match rnd.Next 6 with | 0 -> 2u | _ -> 1u ||| 2u); Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
                    let props = Array.init length (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = (match rnd.Next 4 with | 0 -> GlyphPropsBits.Mark | 1 -> GlyphPropsBits.Ligature | _ -> GlyphPropsBits.Base) })
                    let lookupProps = match rnd.Next 3 with | 0 -> accel.Props | 1 -> 0u | _ -> uint32 (rnd.Next 16)
                    let matcher = Matching.matcherFor 0 false lookupProps 1u false false false 0uy
                    let contextMatcher = Matching.matcherFor 0 true lookupProps 1u false false false 0uy
                    let index = rnd.Next length
                    let expectedPositions = Array.zeroCreate<int> 64
                    let actualPositions = Array.zeroCreate<int> 64
                    let expected = ContextApply.matchChain rules true gdef matcher contextMatcher (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan infos) (ReadOnlySpan props) length index (Span expectedPositions)
                    let actual = ContextApply.matchChainDense sets records matcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan infos) (ReadOnlySpan props) length index (Span actualPositions)
                    expectSameContext $"lookup {lookupIndex} subtable {subtable} index {index}" expected actual
                    Expect.sequenceEqual actualPositions expectedPositions "match positions"
                    let cannot = ContextApply.chainDenseCannotMatch sets matcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan infos) (ReadOnlySpan props) index
                    match cannot with
                    | true -> Expect.isFalse expected.Matched $"lookup {lookupIndex} subtable {subtable} index {index}: a subtable that cannot match matched"
                    | false -> ()
                    compared <- compared + 1
                    matched <- matched + (match expected.Matched with | true -> 1 | false -> 0)
                    refused <- refused + (match cannot with | true -> 1 | false -> 0)
            | _, _ -> ()
    struct (compared, matched, refused)

/// <summary>The font of the Duployan pair of the perf corpus, whose chained-context subtables have backtracks of up to nine glyphs.</summary>
let private duployanFont =
    lazy (Font.create (Face.create (UpstreamFiles.read "perf/fonts/NotoSansDuployan-Regular.otf") 0u))

/// <summary>The Unicode properties bits of a format character.</summary>
let private formatCategory = (UnicodeProps.setGeneralCategory GeneralCategory.Format Unchecked.defaultof<GlyphProps>).UnicodeProps

/// <summary>The properties of a random glyph: a base, a mark of an attachment class, a ligature, with default ignorables, joiners, hidden glyphs and syllables.</summary>
/// <param name="rnd">The random generator.</param>
let private randomGlyphProps (rnd: Random) =
    let klass =
        match rnd.Next 6 with
        | 0 | 1 -> GlyphPropsBits.Mark ||| (uint16 (rnd.Next 4) <<< 8)
        | 2 -> GlyphPropsBits.Ligature
        | 3 -> 0us
        | _ -> GlyphPropsBits.Base
    let unicode =
        match rnd.Next 12 with
        | 0 -> UnicodeProps.Ignorable
        | 1 -> UnicodeProps.Ignorable ||| UnicodeProps.Hidden
        | 2 -> UnicodeProps.Ignorable ||| UnicodeProps.CfZwj ||| formatCategory
        | 3 -> UnicodeProps.Ignorable ||| UnicodeProps.CfZwnj ||| formatCategory
        | _ -> 0us
    { Unchecked.defaultof<GlyphProps> with GlyphClass = klass; UnicodeProps = unicode; Syllable = byte (rnd.Next 3) }

/// <summary>Matches every dense chained-context subtable of a font at random visits through the walks shared among the subtables of a
/// lookup and through each subtable's own walks: the outcome and the decision to refuse are the same, in every field, with every lookup
/// flag, mark filtering sets, mark attachment classes, syllables, joiners, default ignorables and masks.</summary>
/// <param name="rnd">The random generator.</param>
/// <param name="font">The font.</param>
/// <returns>The number of subtables compared, matched and refused, and the number of visits whose share could not tell.</returns>
let private compareSharedChains (rnd: Random) (font: Font) =
    let table = font.Ot.Gsub.Value
    let gdef = font.Ot.Gdef.Value
    let mutable compared = 0
    let mutable matched = 0
    let mutable refused = 0
    let mutable undecided = 0
    let mutable sharedRefused = 0
    for lookupIndex in 0 .. table.Lookups.Length - 1 do
        let accel = table.Lookups[lookupIndex].Value
        let chains = ChainSets.ofLookup accel.Subtables
        let depths = ContextApply.shareDepthsOf chains 0 0 0 0 0
        let pool =
            Array.concat [ for subtable in accel.Subtables.AsSpan().ToArray() do
                             match subtable.Subtable with
                             | SubstChainContext (ChainCoverageRule (backtrack, input, lookahead, _)) ->
                                 for coverage in Seq.concat [ Seq.ofArray (backtrack.AsSpan().ToArray()); Seq.ofArray (input.AsSpan().ToArray()); Seq.ofArray (lookahead.AsSpan().ToArray()) ] -> (Coverage.glyphs coverage).AsSpan().ToArray()
                             | _ -> () ]
            |> Array.truncate 300
        match (depths.BackDepth > 0 || depths.ForwardDepth > 0) && pool.Length > 0 with
        | false -> ()
        | true ->
            for _ in 1 .. 60 do
                let outLen = rnd.Next(0, 40)
                let inLen = rnd.Next(1, 12)
                let glyph () = match rnd.Next 10 < 8 with | true -> pool[rnd.Next pool.Length] | false -> uint32 (rnd.Next 600)
                let mask () = match rnd.Next 6 with | 0 -> 2u | 1 -> 0u | _ -> 1u ||| 2u
                let outInfos = Array.init outLen (fun index -> { Codepoint = glyph (); Mask = mask (); Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
                let outProps = Array.init outLen (fun _ -> randomGlyphProps rnd)
                let infos = Array.init inLen (fun index -> { Codepoint = glyph (); Mask = mask (); Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
                let props = Array.init inLen (fun _ -> randomGlyphProps rnd)
                let lookupProps =
                    match rnd.Next 4 with
                    | 0 -> accel.Props
                    | 1 -> uint32 (rnd.Next 16)
                    | 2 -> LookupFlag.UseMarkFilteringSet ||| (uint32 (rnd.Next(0, gdef.MarkGlyphSets.Length + 2)) <<< 16) ||| uint32 (rnd.Next 16)
                    | _ -> uint32 (rnd.Next 4) <<< 8
                let autoZwnj = rnd.Next 2 = 0
                let autoZwj = rnd.Next 2 = 0
                let perSyllable = rnd.Next 3 = 0
                let index = rnd.Next inLen
                let syllable = props[index].Syllable
                let inputMatcher = Matching.matcherFor 0 false lookupProps 1u autoZwnj autoZwj perSyllable syllable
                let contextMatcher = Matching.matcherFor 0 true lookupProps 1u autoZwnj autoZwj perSyllable syllable
                let sharedPositions = Array.zeroCreate<int> ContextApply.ShareBufferLength
                let share = ContextApply.shareVisit depths inputMatcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan outInfos) (ReadOnlySpan outProps) index (Span sharedPositions)
                let heads = ContextApply.visitHeads inputMatcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan outInfos) (ReadOnlySpan outProps) index
                undecided <- undecided + (match share.BackStop with | ContextApply.ShareEnough -> 1 | ContextApply.ShareEnd -> 0)
                for subtable in 0 .. accel.Subtables.Length - 1 do
                    match accel.Subtables[subtable].Subtable, chains[subtable] with
                    | SubstChainContext (ChainCoverageRule (_, _, _, records)), ValueSome sets ->
                        let expectedPositions = Array.zeroCreate<int> ContextApply.ShareBufferLength
                        let actualPositions = Array.copy sharedPositions
                        let expected = ContextApply.matchChainDense sets records inputMatcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan outInfos) (ReadOnlySpan outProps) outLen index (Span expectedPositions)
                        let actual = ContextApply.matchChainDenseShared sets records inputMatcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan outInfos) (ReadOnlySpan outProps) outLen index (Span actualPositions) share
                        let what = $"lookup {lookupIndex} subtable {subtable} index {index} flags {lookupProps:X}"
                        expectSameContext what expected actual
                        Expect.sequenceEqual (Array.sub actualPositions 0 64) (Array.sub expectedPositions 0 64) $"{what}: match positions"
                        let cannot = ContextApply.chainDenseCannotMatch sets inputMatcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan outInfos) (ReadOnlySpan outProps) index
                        let cannotWith = ContextApply.chainDenseCannotMatchWith heads sets inputMatcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan outInfos) (ReadOnlySpan outProps) index
                        Expect.equal cannotWith cannot $"{what}: the decision to refuse"
                        let cannotShared = ContextApply.chainDenseCannotMatchShared heads share sets inputMatcher contextMatcher gdef (ReadOnlySpan infos) (ReadOnlySpan props) (ReadOnlySpan outInfos) (ReadOnlySpan outProps) index (Span sharedPositions)
                        match cannotShared with
                        | true -> Expect.isFalse expected.Matched $"{what}: a subtable the share refuses matched"
                        | false -> ()
                        sharedRefused <- sharedRefused + (match cannotShared && not cannot with | true -> 1 | false -> 0)
                        compared <- compared + 1
                        matched <- matched + (match expected.Matched with | true -> 1 | false -> 0)
                        refused <- refused + (match cannot with | true -> 1 | false -> 0)
                    | _, _ -> ()
    struct (compared, matched, refused, undecided, sharedRefused)

/// <summary>The general line on one working set: every stage that keeps the length of the line writes into the arrays its first stage
/// made, and the result is that of the reference pipeline in every field.</summary>
[<Tests>]
let workingSetTests =
    testList "general line working set" [
        test "a line shaped on one working set equals the reference general pipeline in every field over marked lines, mark-heavy lines and composing lines" {
            let marked = Gen.sampleWithSeed (Rnd (Seed + 20UL)) 30 12000 genMarkedCase |> Seq.toArray
            let heavy = Gen.sampleWithSeed (Rnd (Seed + 21UL)) 30 8000 genMarkCase |> Seq.toArray
            let composing = Gen.sampleWithSeed (Rnd (Seed + 22UL)) 30 3000 genComposingCase |> Seq.toArray
            let mutable owned = 0
            let mutable compared = 0
            let mutable concat = 0
            let mutable budgets = 0
            let count (outcome: bool voption) (run: GlyphRun) =
                match outcome with
                | ValueNone -> ()
                | ValueSome took ->
                    compared <- compared + 1
                    owned <- owned + (match took with | true -> 1 | false -> 0)
                    concat <- concat + (match run.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default with | true -> 1 | false -> 0)
                    budgets <- budgets + (match run.MaxOps < GlyphRun.MaxOpsDefault with | true -> 1 | false -> 0)
            let lengths = [| GlyphRun.MaxLenDefault; GlyphRun.MaxLenDefault; GlyphRun.MaxLenDefault; 0; 1; 3; 8; 20; 65536; 100 |]
            let limited (index: int) (run: GlyphRun) = { run with MaxLen = lengths[index % lengths.Length] }
            marked |> Array.iteri (fun index (struct (fontIndex, run)) -> count (ownedAgainstReference markedFonts.Value[fontIndex] ImmutableArray<Feature>.Empty (limited index run)) run)
            heavy |> Array.iteri (fun index (struct (fontIndex, run)) -> count (ownedAgainstReference normalizerFonts.Value[fontIndex] ImmutableArray<Feature>.Empty (limited index run)) run)
            composing |> Array.iteri (fun index (struct (fontIndex, features, run)) -> count (ownedAgainstReference normalizerFonts.Value[fontIndex] features (limited index run)) run)
            printfn $"working set property: {compared} cases compared, {owned} shaped on the working set, {concat} asking for unsafe-to-concat, {budgets} with a limited operation budget"
            Expect.isGreaterThan owned 8000 "many lines are shaped on the working set"
            Expect.isGreaterThan concat 1500 "many lines ask for unsafe-to-concat"
            Expect.isGreaterThan budgets 2000 "many lines have a limited operation budget"
        }

        test "a line shaped without unsafe-to-concat equals the same line shaped with it, the unsafe-to-concat marks apart, over marked lines of every font, flag, direction, cluster level and operation budget" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 30UL)) 30 9000 genMarkedCase
            let mutable compared = 0
            let mutable changed = 0
            let unmarked (infos: ImmutableArray<GlyphInfo>) = infos.AsSpan().ToArray() |> Array.map (fun info -> { info with Mask = info.Mask &&& ~~~(uint32 GlyphFlags.UnsafeToConcat) })
            for struct (fontIndex, run: GlyphRun) in cases do
                let font = markedFonts.Value[fontIndex]
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Error _ -> ()
                | Ok plan ->
                    let without = { run with Flags = run.Flags &&& ~~~BufferFlags.ProduceUnsafeToConcat }
                    let withFlag = { run with Flags = run.Flags ||| BufferFlags.ProduceUnsafeToConcat }
                    match Execute.runGeneral plan font ImmutableArray<Feature>.Empty without, Execute.runGeneral plan font ImmutableArray<Feature>.Empty withFlag with
                    | Ok off, Ok on ->
                        Expect.sequenceEqual (unmarked off.Infos) (unmarked on.Infos) "infos but for the unsafe-to-concat marks"
                        Expect.sequenceEqual (off.Props.AsSpan().ToArray()) (on.Props.AsSpan().ToArray()) "props"
                        Expect.sequenceEqual (off.Positions.AsSpan().ToArray()) (on.Positions.AsSpan().ToArray()) "positions"
                        Expect.equal off.MaxOps on.MaxOps "max ops"
                        Expect.equal off.Successful on.Successful "successful"
                        Expect.equal off.ScratchFlags on.ScratchFlags "scratch flags"
                        compared <- compared + 1
                        changed <- changed + (match on.Infos.AsSpan().ToArray() |> Array.exists (fun info -> info.Mask &&& uint32 GlyphFlags.UnsafeToConcat <> 0u) with | true -> 1 | false -> 0)
                    | Error _, Error _ -> ()
                    | Ok _, Error _ -> failtest "shaping with unsafe-to-concat failed where it succeeded without"
                    | Error _, Ok _ -> failtest "shaping without unsafe-to-concat failed where it succeeded with"
            printfn $"fast refusal property: {compared} lines compared, {changed} with unsafe-to-concat marks"
            Expect.isGreaterThan compared 5000 "many lines are compared"
            Expect.isGreaterThan changed 500 "many lines carry unsafe-to-concat marks"
        }

        test "a chained-context subtable of the third format matched through its dense sets gives the general outcome in every field, with random glyphs, classes, masks and lookup flags" {
            let rnd = Random 90210
            let totals = normalizerFonts.Value |> Array.map (fun font -> compareDenseChains rnd font)
            let compared = totals |> Array.sumBy (fun (struct (compared, _, _)) -> compared)
            let matched = totals |> Array.sumBy (fun (struct (_, matched, _)) -> matched)
            let refused = totals |> Array.sumBy (fun (struct (_, _, refused)) -> refused)
            printfn $"dense chain property: {compared} visits compared, {matched} matched, {refused} refused at once"
            Expect.isGreaterThan compared 5000 "many visits are compared"
            Expect.isGreaterThan matched 300 "many visits match"
            Expect.isGreaterThan refused 1000 "many visits are refused without the match"
        }

        test "the dense chained-context subtables of a lookup matched through the walks they share equal the general match in every field, over random visits with every lookup flag, mark filtering set, attachment class, syllable, joiner, default ignorable and mask" {
            let rnd = Random 4711
            let fontsOf = Array.append normalizerFonts.Value [| duployanFont.Value |]
            let totals = fontsOf |> Array.map (fun font -> compareSharedChains rnd font)
            let compared = totals |> Array.sumBy (fun (struct (compared, _, _, _, _)) -> compared)
            let matched = totals |> Array.sumBy (fun (struct (_, matched, _, _, _)) -> matched)
            let refused = totals |> Array.sumBy (fun (struct (_, _, refused, _, _)) -> refused)
            let sharedRefused = totals |> Array.sumBy (fun (struct (_, _, _, _, sharedRefused)) -> sharedRefused)
            let undecided = totals |> Array.sumBy (fun (struct (_, _, _, undecided, _)) -> undecided)
            printfn $"shared chain property: {compared} subtable visits compared, {matched} matched, {refused} refused at once, {undecided} visits whose backward share stopped short, {sharedRefused} subtables only the input walk of the share refuses"
            Expect.isGreaterThan compared 5000 "many visits are compared"
            Expect.isGreaterThan matched 100 "many visits match"
            Expect.isGreaterThan refused 1000 "many visits are refused without the match"
            Expect.isGreaterThan sharedRefused 200 "the input walk of the share refuses subtables the first step lets through"
        }

        test "the in-place loops of the working set equal the stage functions they replace over random arrays: flag propagation, mark and ignorable zeroing, attachment offsets and reversal" {
            let rnd = Random 31337
            let mutable cases = 0
            for _ in 1 .. 6000 do
                let length = rnd.Next(1, 40)
                let clusters = Array.init length (fun _ -> 0u)
                let mutable cluster = 0u
                for index in 0 .. length - 1 do
                    cluster <- cluster + (match rnd.Next 3 with | 0 -> 0u | _ -> 1u)
                    clusters[index] <- cluster
                let infos = Sealed.init length (fun index -> { Codepoint = uint32 (rnd.Next 300); Mask = uint32 (rnd.Next 64) ||| (match rnd.Next 4 with | 0 -> 0xFFFFFFF0u | _ -> 0u); Cluster = clusters[index]; Var1 = uint32 index; Var2 = 0u })
                let props = Sealed.init length (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = (match rnd.Next 3 with | 0 -> GlyphPropsBits.Mark | 1 -> GlyphPropsBits.Base | _ -> 0us); UnicodeProps = (match rnd.Next 4 with | 0 -> UnicodeProps.Ignorable | _ -> 0us) })
                let positions =
                    Sealed.init length (fun _ ->
                        let chain = match rnd.Next 3 with | 0 -> 0 | _ -> rnd.Next(-3, 4)
                        { XAdvance = rnd.Next(-50, 500); YAdvance = rnd.Next(-5, 5); XOffset = rnd.Next(-30, 30); YOffset = rnd.Next(-30, 30); Var = (uint32 (rnd.Next 4) <<< 16) ||| uint32 (uint16 chain) })
                let flags = Seq.fold (fun (acc: BufferFlags) flag -> if rnd.Next 2 = 0 then acc ||| flag else acc) BufferFlags.Default [ BufferFlags.ProduceUnsafeToConcat; BufferFlags.ProduceSafeToInsertTatweel; BufferFlags.PreserveDefaultIgnorables; BufferFlags.RemoveDefaultIgnorables ]
                let direction = match rnd.Next 2 with | 0 -> Direction.Ltr | _ -> Direction.Rtl
                let run =
                    { GlyphRun.empty with
                        Infos = infos
                        Props = props
                        Positions = positions
                        HavePositions = true
                        Flags = flags
                        ScratchFlags = ScratchFlags.HasDefaultIgnorables ||| ScratchFlags.HasGposAttachment
                        SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = Language.cLocale } }
                let copy (items: ImmutableArray<'T>) = Sealed.ofSpan (items.AsSpan())
                let propagated = copy infos
                Sealed.propagateFlagsOwned propagated (Flags.andMaskOf flags) (flags &&& BufferFlags.ProduceSafeToInsertTatweel <> BufferFlags.Default)
                Expect.sequenceEqual (propagated.AsSpan().ToArray()) ((Flags.propagate run).Infos.AsSpan().ToArray()) "flag propagation"
                let zeroed = copy positions
                Sealed.zeroMarkAdvancesOwned props zeroed
                let expectedZeroed = Sealed.zeroMarkAdvances (props.AsSpan()) (positions.AsSpan())
                Expect.sequenceEqual (zeroed.AsSpan().ToArray()) (expectedZeroed.AsSpan().ToArray()) "mark advances"
                let ignorables = copy positions
                let horizontal = Direction.isHorizontal direction
                match flags &&& (BufferFlags.PreserveDefaultIgnorables ||| BufferFlags.RemoveDefaultIgnorables) = BufferFlags.Default with
                | true -> Sealed.zeroIgnorablesOwned props UnicodeProps.Ignorable horizontal ignorables
                | false -> ()
                let expectedIgnorables = (OtPosition.zeroWidthDefaultIgnorables run).Positions
                Expect.sequenceEqual (ignorables.AsSpan().ToArray()) (expectedIgnorables.AsSpan().ToArray()) "ignorable widths"
                let attached = copy positions
                let forward = not (Direction.isBackward direction)
                Sealed.propagateAttachmentsOwned attached forward horizontal
                let expectedAttached = Sealed.propagateAttachments positions forward horizontal
                Expect.sequenceEqual (attached.AsSpan().ToArray()) (expectedAttached.AsSpan().ToArray()) "attachment offsets"
                let reversedInfos = copy infos
                let reversedProps = copy props
                let reversedPositions = copy positions
                Sealed.reverseOwned reversedInfos reversedProps reversedPositions
                let reversed = GlyphRun.reverse run
                Expect.sequenceEqual (reversedInfos.AsSpan().ToArray()) (reversed.Infos.AsSpan().ToArray()) "reversed infos"
                Expect.sequenceEqual (reversedProps.AsSpan().ToArray()) (reversed.Props.AsSpan().ToArray()) "reversed props"
                Expect.sequenceEqual (reversedPositions.AsSpan().ToArray()) (reversed.Positions.AsSpan().ToArray()) "reversed positions"
                cases <- cases + 1
            printfn $"in-place loops property: {cases} cases"
            Expect.isGreaterThan cases 5000 "many cases"
        }
    ]

/// <summary>The visits of the positioning kernel on marked lines: mark attachment straight over the compiled tables, and pairs under a lookup flag that skips marks.</summary>
[<Tests>]
let visitTests =
    testList "kernel visits" [
        test "the dense attachment loop and the pair loop through skipped glyphs equal the general loop in every field over lines of real fonts, with every flag, direction, cluster level and operation budget" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 20UL)) 30 12000 genMarkedCase
            let mutable compared = 0
            let mutable attached = 0
            let mutable concat = 0
            let mutable exhausted = 0
            for struct (fontIndex, run: GlyphRun) in cases do
                let font = markedFonts.Value[fontIndex]
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Error _ -> ()
                | Ok shapePlan ->
                    match shapePlan.Ot with
                    | ValueNone -> ()
                    | ValueSome plan ->
                        match plan.ApplyGpos && run.Infos.Length > 0 with
                        | false -> ()
                        | true ->
                            let substituted = substitutedOf plan font (GlyphRun.enter run)
                            let defaulted = { substituted with Positions = Font.hAdvancePositions font (substituted.Infos.AsSpan()); HavePositions = true }
                            let reference = Engine.positionPlannedReference plan font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups defaulted
                            let planned = Engine.positionPlanned plan font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups shapePlan.GposPairs defaulted
                            expectSameRunAll "dense" reference planned
                            Expect.equal planned.ScratchFlags reference.ScratchFlags "scratch flags"
                            Expect.equal planned.MaxOps reference.MaxOps "operation budget"
                            Expect.equal planned.Successful reference.Successful "success"
                            Expect.equal planned.Positions.Length reference.Positions.Length "positions"
                            for index in 0 .. planned.Positions.Length - 1 do
                                Expect.equal planned.Positions[index].Var reference.Positions[index].Var $"var of {index}"
                            compared <- compared + 1
                            attached <- attached + (match planned.ScratchFlags &&& ScratchFlags.HasGposAttachment <> ScratchFlags.Default with | true -> 1 | false -> 0)
                            concat <- concat + (match run.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default with | true -> 1 | false -> 0)
                            exhausted <- exhausted + (match planned.Successful with | true -> 0 | false -> 1)
            printfn $"kernel visits property: {cases.Length} cases, {compared} compared, {attached} attached marks, {concat} asking for unsafe-to-concat, {exhausted} that ran out of operations"
            Expect.isGreaterThan compared 5000 "many runs are compared"
            Expect.isGreaterThan attached 2000 "many runs attach marks"
            Expect.isGreaterThan concat 1000 "many runs ask for unsafe-to-concat"
            Expect.isGreaterThan exhausted 50 "some runs run out of operations"
        }

        test "positioning a Hebrew line with points and a Thai line allocates the working copies and nothing for each visit" {
            let measure (font: Font) (text: string) script =
                let bytes = Text.Encoding.UTF8.GetBytes text
                let run =
                    GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty
                    |> GlyphRun.guessSegmentProperties Language.cLocale
                let run = { run with SegmentProperties = { run.SegmentProperties with Script = script } }
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Error _ -> failtest "no plan"
                | Ok shapePlan ->
                    match shapePlan.Ot with
                    | ValueNone -> failtest "no ot plan"
                    | ValueSome plan ->
                        let substituted = substitutedOf plan font (GlyphRun.enter run)
                        let defaulted = { substituted with Positions = Font.hAdvancePositions font (substituted.Infos.AsSpan()); HavePositions = true }
                        let positioned () = Engine.positionPlanned plan font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups shapePlan.GposPairs defaulted |> ignore
                        positioned ()
                        let allocated = Allocation.bytesAllocatedAtBest positioned
                        struct (allocated, defaulted.Infos.Length)
            let hebrew = String.Join (" ", [ for _ in 1 .. 12 -> "שָׁלוֹם בְּרֵאשִׁית" ])
            let thai = String.Join (" ", [ for _ in 1 .. 12 -> "สวัสดีครับ น้ำใจ" ])
            let struct (hebrewBytes, hebrewGlyphs) = measure normalizerFonts.Value[3] hebrew Scripts.Hebrew
            let struct (thaiBytes, thaiGlyphs) = measure normalizerFonts.Value[4] thai Scripts.Thai
            printfn $"visit allocation: Hebrew {hebrewBytes} B for {hebrewGlyphs} glyphs, Thai {thaiBytes} B for {thaiGlyphs} glyphs"
            Expect.isLessThan hebrewBytes (int64 hebrewGlyphs * 64L) "the Hebrew line allocates the working copies, under 64 bytes per glyph"
            Expect.isLessThan thaiBytes (int64 thaiGlyphs * 64L) "the Thai line allocates the working copies, under 64 bytes per glyph"
        }
    ]

/// <summary>Generates a long line of mostly starters with a mark now and then, so that the normaliser meets long stretches of
/// glyphs it moves in one step, and a length limit now and then, so that it meets the stretches it must move one by one.</summary>
let private genStretchCase =
    gen {
        let! fontIndex = Gen.choose (0, 6)
        let! length = Gen.frequency [ 2, Gen.choose (2, 6); 6, Gen.choose (7, 40); 3, Gen.choose (41, 120) ]
        let! text =
            Gen.arrayOfLength length
                (Gen.frequency
                    [ 40, Gen.choose (0x61, 0x7A) |> Gen.map uint32
                      3, Gen.elements [ 0x20u; 0x2Cu; 0xADu ]
                      18, Gen.elements [ 0xE9u; 0xC0u; 0x1EA0u; 0xFB2Au; 0x1F82u; 0x212Bu; 0x958u; 0x1EBFu ]
                      6, Gen.elements [ 0x300u; 0x301u; 0x308u; 0x323u; 0x31Bu; 0x34Fu ]
                      12, Gen.choose (0x5D0, 0x5EA) |> Gen.map uint32
                      6, Gen.choose (0x5B0, 0x5BD) |> Gen.map uint32
                      10, Gen.choose (0xE01, 0xE2E) |> Gen.map uint32
                      4, Gen.elements [ 0xE31u; 0xE34u; 0xE38u; 0xE48u; 0xE33u ] ])
        let! script = Gen.elements [ Scripts.Latin; Scripts.Latin; Scripts.Hebrew; Scripts.Thai ]
        let! direction = Gen.elements [ Direction.Ltr; Direction.Ltr; Direction.Rtl ]
        let! clusters = genClusters text.Length
        let! maxLength = Gen.frequency [ 5, Gen.constant GlyphRun.MaxLenDefault; 3, Gen.choose (text.Length, text.Length + 6); 2, Gen.choose (text.Length, text.Length * 2) ]
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let infos = Sealed.init text.Length (fun index -> { Codepoint = text[index]; Mask = 0u; Cluster = clusters[index]; Var1 = 0u; Var2 = 0u })
        let run =
            { GlyphRun.empty with
                Infos = infos
                ContentType = UnicodeContent
                SegmentProperties = { Direction = direction; Script = script; Language = Language.cLocale }
                ClusterLevel = level
                MaxLen = maxLength }
        return struct (fontIndex, run)
    }

/// <summary>Generates a line of starters with isolated marks after some of them, a mark now and then after a mark or first, and a small
/// operation budget now and then, so that the normaliser meets the pairs it composes in place, the lines it declines, and the budgets that
/// run out while the clusters of the pairs are merged.</summary>
let private genPairCase =
    gen {
        let! fontIndex = Gen.elements [ 0; 1; 2; 5; 6 ]
        let! length = Gen.frequency [ 2, Gen.choose (2, 6); 6, Gen.choose (7, 40); 3, Gen.choose (41, 100) ]
        let! text =
            Gen.arrayOfLength length
                (Gen.frequency
                    [ 40, Gen.choose (0x61, 0x7A) |> Gen.map uint32
                      4, Gen.elements [ 0x20u; 0x2Cu ]
                      6, Gen.elements [ 0xE9u; 0xC0u; 0x1EA0u; 0x1EBFu ]
                      28, Gen.elements [ 0x300u; 0x301u; 0x308u; 0x323u; 0x302u; 0x327u ] ])
        let! clusters = genClusters text.Length
        let! maxOps = Gen.frequency [ 5, Gen.constant GlyphRun.MaxOpsDefault; 4, Gen.choose (0, 12); 2, Gen.choose (0, 60) ]
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let infos = Sealed.init text.Length (fun index -> { Codepoint = text[index]; Mask = 0u; Cluster = clusters[index]; Var1 = 0u; Var2 = 0u })
        let run =
            { GlyphRun.empty with
                Infos = infos
                ContentType = UnicodeContent
                SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
                ClusterLevel = level
                MaxOps = maxOps }
        return struct (fontIndex, run)
    }

/// <summary>The lean normaliser's stretch moves.</summary>
[<Tests>]
let stretchTests =
    testList "normaliser stretches" [
        test "the lean normaliser, on shared and on owned arrays, equals the out-buffer passes in every field over long stretches and under length limits" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 7UL)) 60 12000 genStretchCase
            let mutable shortened = 0
            let mutable limited = 0
            let mutable composed = 0
            let mutable grown = 0
            for struct (fontIndex, run) in cases do
                let font = normalizerFonts.Value[fontIndex]
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Error _ -> ()
                | Ok shapePlan ->
                    match shapePlan.Ot with
                    | ValueNone -> ()
                    | ValueSome plan ->
                        let input = { prepared plan font { run with Successful = true } with MaxLen = run.MaxLen }
                        let expected = Normalize.runPass plan font input
                        expectSameRunAll "shared" expected (Normalize.run plan font input)
                        let owned = { input with Infos = ImmutableArray.CreateRange (input.Infos.AsSpan().ToArray()); Props = ImmutableArray.CreateRange (input.Props.AsSpan().ToArray()) }
                        expectSameRunAll "owned" expected (Normalize.runOwned plan font owned)
                        match expected.Successful with
                        | false -> limited <- limited + 1
                        | true -> ()
                        match expected.Infos.Length < input.Infos.Length with
                        | true -> composed <- composed + 1
                        | false -> ()
                        match expected.Infos.Length > input.Infos.Length with
                        | true -> grown <- grown + 1
                        | false -> ()
                        shortened <- shortened + 1
            printfn $"stretch property: {cases.Length} cases, {shortened} compared, {limited} ran into the length limit, {grown} grew by decomposition, {composed} shortened by composition"
            Expect.isGreaterThan shortened 8000 "most cases have a plan"
            Expect.isGreaterThan limited 20 "the length limit is met"
            Expect.isGreaterThan grown 2000 "decomposition makes the arrays grow"
            Expect.isGreaterThan composed 300 "marks compose"
        }

        test "composing the isolated pairs of a line in place equals the out-buffer passes in every field, under small operation budgets and at every cluster level" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 11UL)) 60 12000 genPairCase
            let mutable compared = 0
            let mutable shortened = 0
            let mutable exhausted = 0
            for struct (fontIndex, run) in cases do
                let font = normalizerFonts.Value[fontIndex]
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Error _ -> ()
                | Ok shapePlan ->
                    match shapePlan.Ot with
                    | ValueNone -> ()
                    | ValueSome plan ->
                        let input = { prepared plan font { run with Successful = true } with MaxOps = run.MaxOps }
                        let expected = Normalize.runPass plan font input
                        let owned = { input with Infos = ImmutableArray.CreateRange (input.Infos.AsSpan().ToArray()); Props = ImmutableArray.CreateRange (input.Props.AsSpan().ToArray()) }
                        expectSameRunAll "owned" expected (Normalize.runOwned plan font owned)
                        let ownedAgain = { input with Infos = ImmutableArray.CreateRange (input.Infos.AsSpan().ToArray()); Props = ImmutableArray.CreateRange (input.Props.AsSpan().ToArray()) }
                        let set = GlyphRun.setOf ownedAgain
                        expectSameRunAll "set" expected (GlyphRun.runOf (Normalize.runOwnedSet plan font set))
                        compared <- compared + 1
                        shortened <- shortened + (match expected.Infos.Length < input.Infos.Length with | true -> 1 | false -> 0)
                        exhausted <- exhausted + (match expected.Successful with | true -> 0 | false -> 1)
            let mutable charged = 0
            for pairs in 1 .. 4 do
                for budget in 0 .. 9 do
                    for (level, markOwn) in [ for level in [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ] do for markOwn in [ false; true ] -> (level, markOwn) ] do
                        let text = Array.concat [ for _ in 1 .. pairs -> [| 0x65u; 0x301u; 0x61u |] ]
                        let infos = Sealed.init text.Length (fun index -> { Codepoint = text[index]; Mask = 0u; Cluster = uint32 (index / 3 * 2 + (match index % 3, markOwn with | 2, _ | 1, true -> 1 | _ -> 0)); Var1 = 0u; Var2 = 0u })
                        let run =
                            { GlyphRun.empty with
                                Infos = infos
                                ContentType = UnicodeContent
                                SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
                                ClusterLevel = level }
                        let font = normalizerFonts.Value[0]
                        match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                        | Error _ -> ()
                        | Ok shapePlan ->
                            match shapePlan.Ot with
                            | ValueNone -> ()
                            | ValueSome plan ->
                                let input = { prepared plan font { run with Successful = true } with MaxOps = budget }
                                let expected = Normalize.runPass plan font input
                                let owned = { input with Infos = ImmutableArray.CreateRange (input.Infos.AsSpan().ToArray()); Props = ImmutableArray.CreateRange (input.Props.AsSpan().ToArray()) }
                                expectSameRunAll "budget" expected (Normalize.runOwned plan font owned)
                                charged <- charged + (match expected.MaxOps < budget || not expected.Successful with | true -> 1 | false -> 0)
            Expect.isGreaterThan charged 40 "composing charges the operation budget, and some budgets run out"
            printfn $"isolated pairs property: {cases.Length} cases, {compared} compared, {shortened} shortened by composition, {exhausted} ran out of operations"
            Expect.isGreaterThan compared 8000 "most cases have a plan"
            Expect.isGreaterThan shortened 2000 "pairs compose"
            Expect.isGreaterThan exhausted 100 "the operation budget runs out"
        }
    ]

/// <summary>The second-code-point set of the composition.</summary>
[<Tests>]
let compositionSecondTests =
    testList "composition second set" [
        test "composing equals the binary searches for every second code point against the first code points of every pair and some more" {
            let firsts =
                Array.concat
                    [ UcdTables.dm2U32 |> Seq.map (fun value -> value >>> 21) |> Seq.toArray
                      UcdTables.dm2U64 |> Seq.map (fun value -> uint32 (value >>> 42)) |> Seq.toArray
                      [| 0x41u; 0x61u; 0x65u; 0x5D0u; 0x5E9u; 0xE01u; 0x1100u; 0x1161u; 0xAC00u; 0xAC01u; 0x3B1u; 0x0u; 0xFFFFFFFFu |] ]
                |> Array.distinct
            let sampled = firsts |> Array.indexed |> Array.filter (fun (index, _) -> index % 37 = 0 || index >= firsts.Length - 13) |> Array.map snd
            let mutable pairs = 0
            let mutable wrong = 0
            for a in sampled do
                for b in 0u .. 0x11FFFFu do
                    match Unicode.compose a b = Unicode.composeBySearch a b with
                    | true -> ()
                    | false -> wrong <- wrong + 1
                    pairs <- pairs + 1
            Expect.equal wrong 0 "pairs on which the set and the searches differ"
            printfn $"composition second set: {sampled.Length} first code points, {pairs} pairs"
            Expect.isGreaterThan pairs 5000 "many pairs"
        }
    ]

/// <summary>The scratch-flag scan of the lean normaliser and the two table shortcuts of its decisions.</summary>
[<Tests>]
let scratchScanTests =
    testList "lean normaliser shortcuts" [
        test "the quick scratch-flag scan equals the scan of every glyph over random arrays of every packed category, flag bit and special code point" {
            let rnd = Random 4242
            let special = [| 0x34Fu; 0x2044u; 0x200Du; 0x200Cu; 0xFE0Fu; 0x20u; 0xA0u; 0x2003u; 0x300u; 0x61u; 0xE31u; 0x5B0u |]
            let mutable cases = 0
            let mutable flagged = 0
            for _ in 1 .. 30000 do
                let length = rnd.Next(0, 24)
                let infos = Array.init length (fun _ -> { Codepoint = (match rnd.Next 3 with | 0 -> special[rnd.Next special.Length] | _ -> uint32 (rnd.Next 0x2100)); Mask = 1u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
                let props =
                    Array.init length (fun _ ->
                        let bits = match rnd.Next 4 with | 0 -> 0us | _ -> uint16 (rnd.Next 0x10000)
                        { Unchecked.defaultof<GlyphProps> with UnicodeProps = bits })
                let start = ScratchFlags.Default
                let expected = Normalize.scanScratchFrom (ReadOnlySpan infos) (ReadOnlySpan props) 0 start
                let actual = Normalize.scanScratchQuick (ReadOnlySpan infos) (ReadOnlySpan props) 0 start |> TailCalls.settle
                Expect.equal actual expected "scratch flags"
                cases <- cases + 1
                flagged <- flagged + (match expected = start with | true -> 0 | false -> 1)
            Expect.isGreaterThan flagged 5000 "many arrays carry flags"
            Expect.equal cases 30000 "cases"
        }

        test "no code point below U+00C0 decomposes, and the low table of Unicode properties equals the computation below U+2100 and on a sample above" {
            for codepoint in 0u .. 0xBFu do
                Expect.isTrue (Unicode.decompose codepoint |> ValueOption.isNone) $"U+{codepoint:X4} has no decomposition"
            for codepoint in 0u .. 0x20FFu do
                Expect.equal (UnicodeProps.computeLow codepoint) (UnicodeProps.compute codepoint) $"U+{codepoint:X4}"
            for codepoint in 0x2100u .. 0x3FFFFu do
                Expect.equal (UnicodeProps.computeLow codepoint) (UnicodeProps.compute codepoint) $"U+{codepoint:X4}"
        }
    ]
