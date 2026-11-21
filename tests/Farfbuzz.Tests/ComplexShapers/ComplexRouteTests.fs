module Farfbuzz.Tests.ComplexShapers.ComplexRouteTests

open System
open System.Buffers.Binary
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
let private Seed = 0x434F4DUL

/// <summary>The cases each property draws for one script.</summary>
[<Literal>]
let private CaseCount = 5000

/// <summary>The layout-table tags a stripped copy of a font renames away.</summary>
let private layoutTags =
    [ "GSUB"; "GPOS"; "GDEF"; "kern"; "morx"; "mort"; "kerx"; "trak"; "ankr"; "feat" ]
    |> List.map (fun text -> uint32 (Tag.ofString text))
    |> Set.ofList

/// <summary>Renames the layout tables of a font so shaping reads none of them.</summary>
/// <param name="source">The font bytes.</param>
/// <returns>The stripped copy.</returns>
let private stripped (source: ReadOnlyMemory<byte>) =
    let bytes = source.ToArray()
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
    for index in 0 .. count - 1 do
        let record = 12 + index * 16
        if record + 4 <= bytes.Length && layoutTags.Contains (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4))) then
            bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'
    ReadOnlyMemory bytes

/// <summary>Renames the <c>morx</c> table of a font, so that the plan of its script reads the OpenType tables and not the AAT ones; the
/// new tag keeps the table directory in order.</summary>
/// <param name="source">The font bytes.</param>
/// <returns>The copy without a <c>morx</c> table.</returns>
let private withoutMorx (source: ReadOnlyMemory<byte>) =
    let bytes = source.ToArray()
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
    for index in 0 .. count - 1 do
        let record = 12 + index * 16
        if record + 4 <= bytes.Length && BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4)) = uint32 (Tag.ofString "morx") then
            bytes[record + 1] <- byte 't'
    ReadOnlyMemory bytes

/// <summary>A script under test: a real font, the script, and weighted pools of code points drawn from the script's categories,
/// so that runs are valid syllables and broken ones alike.</summary>
type private Subject =
    { Name: string
      Font: string
      OpenType: bool
      Script: Script
      Pools: (int * (int * int) list) list }

/// <summary>The subjects: every shaper the lean normaliser reads and the general line serves beyond the default one.</summary>
let private subjects =
    let shared = [ (3, [ (0x41, 0x5A); (0x61, 0x7A) ]); (3, [ (0x20, 0x20); (0x30, 0x39) ]); (2, [ (0x25CC, 0x25CC) ]); (3, [ (0x200C, 0x200D) ]); (1, [ (0x300, 0x30F) ]) ]
    let hangulPools =
        [ (20, [ (0xAC00, 0xD7A3) ]); (10, [ (0x1100, 0x1112) ]); (10, [ (0x1161, 0x1175) ]); (8, [ (0x11A8, 0x11C2) ]); (4, [ (0x1113, 0x115F) ]); (4, [ (0x1176, 0x11A7) ]); (4, [ (0x11C3, 0x11FF) ])
          (3, [ (0xA960, 0xA97C) ]); (3, [ (0xD7B0, 0xD7FB) ]); (4, [ (0x302E, 0x302F) ]); (3, [ (0x3131, 0x318E) ]); (2, [ (0x4E00, 0x4E20) ])
          (2, [ (0x1F3FB, 0x1F3FF); (0x1F1E6, 0x1F1FF); (0xFF9E, 0xFF9F); (0xE0020, 0xE007F) ]); (2, [ (0x1F466, 0x1F469); (0x2640, 0x2642) ]) ] @ shared
    [ { Name = "Devanagari"; Font = "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf"; OpenType = false; Script = Scripts.Devanagari
        Pools = [ (30, [ (0x915, 0x939) ]); (14, [ (0x93E, 0x94C) ]); (14, [ (0x94D, 0x94D) ]); (5, [ (0x93C, 0x93C) ]); (6, [ (0x904, 0x914) ]); (4, [ (0x900, 0x903) ])
                  (6, [ (0x958, 0x95F); (0x929, 0x929); (0x931, 0x931); (0x934, 0x934) ]); (3, [ (0x951, 0x957) ]); (2, [ (0x966, 0x96F) ]) ] @ shared }
      { Name = "Telugu"; Font = "test/subset/data/fonts/SreeKrushnadevaraya-Regular.ttf"; OpenType = false; Script = Scripts.Telugu
        Pools = [ (30, [ (0xC15, 0xC39) ]); (14, [ (0xC3E, 0xC4C); (0xC55, 0xC56) ]); (14, [ (0xC4D, 0xC4D) ]); (6, [ (0xC05, 0xC14) ]); (4, [ (0xC00, 0xC03) ]) ] @ shared }
      { Name = "Kannada"; Font = "test/shape/data/text-rendering-tests/fonts/NotoSerifKannada-Regular.ttf"; OpenType = false; Script = Scripts.Kannada
        Pools = [ (30, [ (0xC95, 0xCB9) ]); (14, [ (0xCBE, 0xCCC); (0xCD5, 0xCD6) ]); (14, [ (0xCCD, 0xCCD) ]); (6, [ (0xC85, 0xC94) ]); (4, [ (0xC82, 0xC83) ]) ] @ shared }
      { Name = "Bengali"; Font = "test/subset/data/fonts/IndicTestHowrah-Regular.ttf"; OpenType = false; Script = Scripts.Bengali
        Pools = [ (30, [ (0x995, 0x9B9) ]); (14, [ (0x9BE, 0x9CC) ]); (14, [ (0x9CD, 0x9CD) ]); (5, [ (0x9BC, 0x9BC) ]); (6, [ (0x985, 0x994) ]); (4, [ (0x981, 0x983) ]); (6, [ (0x9DC, 0x9DF) ]) ] @ shared }
      { Name = "Gurmukhi"; Font = "test/subset/data/fonts/IndicTestJalandhar-Regular.ttf"; OpenType = false; Script = Scripts.Gurmukhi
        Pools = [ (30, [ (0xA15, 0xA39) ]); (14, [ (0xA3E, 0xA4C) ]); (14, [ (0xA4D, 0xA4D) ]); (5, [ (0xA3C, 0xA3C) ]); (6, [ (0xA05, 0xA14) ]); (4, [ (0xA01, 0xA03); (0xA70, 0xA71) ]); (4, [ (0xA33, 0xA36); (0xA59, 0xA5E) ]) ] @ shared }
      { Name = "Khmer"; Font = "test/subset/data/fonts/Khmer.ttf"; OpenType = false; Script = Scripts.Khmer
        Pools = [ (30, [ (0x1780, 0x17A2) ]); (5, [ (0x17A3, 0x17B3) ]); (15, [ (0x17B6, 0x17C5) ]); (8, [ (0x17C6, 0x17D1) ]); (14, [ (0x17D2, 0x17D2) ]); (3, [ (0x17D3, 0x17D3); (0x17DD, 0x17DD) ]); (4, [ (0x17BE, 0x17C0); (0x17C4, 0x17C5) ]) ] @ shared }
      { Name = "Myanmar"; Font = "test/subset/data/fonts/NotoSerifMyanmar-Regular.otf"; OpenType = false; Script = Scripts.Myanmar
        Pools = [ (30, [ (0x1000, 0x1021) ]); (10, [ (0x102B, 0x1032) ]); (8, [ (0x103B, 0x103E) ]); (5, [ (0x103A, 0x103A) ]); (8, [ (0x1039, 0x1039) ]); (4, [ (0x1036, 0x1037) ]); (4, [ (0x1025, 0x1029) ]); (2, [ (0x1040, 0x1049) ]) ] @ shared }
      { Name = "Newa (USE)"; Font = "test/subset/data/fonts/NotoSansNewa-Regular.ttf"; OpenType = false; Script = Scripts.Newa
        Pools = [ (30, [ (0x11400, 0x11434) ]); (15, [ (0x11435, 0x11446) ]); (10, [ (0x11442, 0x11442) ]); (3, [ (0x11447, 0x1144A) ]) ] @ shared }
      { Name = "Balinese (USE)"; Font = "test/shape/data/text-rendering-tests/fonts/NotoSansBalinese-Regular.ttf"; OpenType = false; Script = Scripts.Balinese
        Pools = [ (30, [ (0x1B13, 0x1B33) ]); (15, [ (0x1B35, 0x1B43) ]); (10, [ (0x1B44, 0x1B44) ]); (4, [ (0x1B00, 0x1B04) ]); (4, [ (0x1B05, 0x1B12) ]) ] @ shared }
      { Name = "Duployan (USE)"; Font = "perf/fonts/NotoSansDuployan-Regular.otf"; OpenType = false; Script = Scripts.Duployan
        Pools = [ (30, [ (0x1BC00, 0x1BC5B) ]); (14, [ (0x1BC5C, 0x1BC7C) ]); (4, [ (0x1BC80, 0x1BC88) ]); (4, [ (0x1BC9C, 0x1BC9F) ]); (4, [ (0x1BCA0, 0x1BCA3) ]); (3, [ (0x34F, 0x34F) ]) ] @ shared }
      { Name = "Khmer (OpenType)"; Font = "test/subset/data/fonts/Khmer.ttf"; OpenType = true; Script = Scripts.Khmer
        Pools = [ (30, [ (0x1780, 0x17A2) ]); (5, [ (0x17A3, 0x17B3) ]); (15, [ (0x17B6, 0x17C5) ]); (8, [ (0x17C6, 0x17D1) ]); (14, [ (0x17D2, 0x17D2) ]); (3, [ (0x17D3, 0x17D3); (0x17DD, 0x17DD) ]); (4, [ (0x17BE, 0x17C0); (0x17C4, 0x17C5) ]) ] @ shared }
      { Name = "Malayalam"; Font = "test/shape/data/in-house/fonts/55e2910dbc9ef5dd89f4e146e7e0152169545b6a.ttf"; OpenType = false; Script = Scripts.Malayalam
        Pools = [ (30, [ (0xD15, 0xD3A) ]); (14, [ (0xD3E, 0xD4C); (0xD57, 0xD57) ]); (14, [ (0xD4D, 0xD4D) ]); (8, [ (0xD4E, 0xD4E) ]); (6, [ (0xD05, 0xD14) ]); (4, [ (0xD00, 0xD03) ]); (5, [ (0xD7A, 0xD7F) ]) ] @ shared }
      { Name = "Oriya"; Font = "test/subset/data/fonts/NotoSansOriya-subset.ttf"; OpenType = false; Script = Scripts.Oriya
        Pools = [ (30, [ (0xB15, 0xB39) ]); (14, [ (0xB3E, 0xB4C); (0xB56, 0xB57) ]); (14, [ (0xB4D, 0xB4D) ]); (5, [ (0xB3C, 0xB3C) ]); (6, [ (0xB05, 0xB14) ]); (4, [ (0xB01, 0xB03) ]) ] @ shared }
      { Name = "Latin (Roboto, combining marks)"; Font = "perf/fonts/Roboto-Regular.ttf"; OpenType = false; Script = Scripts.Latin
        Pools = [ (30, [ (0x41, 0x5A); (0x61, 0x7A) ]); (10, [ (0xC0, 0x17F) ]); (22, [ (0x300, 0x36F) ]); (3, [ (0x1E00, 0x1EFF) ]); (2, [ (0x20, 0x20); (0x30, 0x39) ]); (2, [ (0x200C, 0x200D); (0x34F, 0x34F) ]) ] }
      { Name = "Hebrew"; Font = "test/subset/data/fonts/NotoIKEAHebrewLatin-Regular.ttf"; OpenType = false; Script = Scripts.Hebrew
        Pools = [ (30, [ (0x5D0, 0x5EA) ]); (14, [ (0x5B0, 0x5BC); (0x5BF, 0x5C2) ]); (6, [ (0x591, 0x5AF) ]); (4, [ (0x5BD, 0x5BE); (0x5C4, 0x5C7) ]); (3, [ (0xFB1D, 0xFB4F) ]) ] @ shared }
      { Name = "Thai"; Font = "test/fuzzing/fonts/kanit.ttf"; OpenType = false; Script = Scripts.Thai
        Pools = [ (30, [ (0xE01, 0xE2E) ]); (10, [ (0xE40, 0xE44) ]); (14, [ (0xE31, 0xE31); (0xE34, 0xE3A) ]); (10, [ (0xE47, 0xE4E) ]); (4, [ (0xE33, 0xE33) ]); (2, [ (0xE50, 0xE59) ]) ] @ shared }
      { Name = "Hangul (AdobeBlank)"; Font = "test/subset/data/fonts/AdobeBlank-Regular.ttf"; OpenType = false; Script = Scripts.Hangul
        Pools = hangulPools }
      { Name = "Hangul (in-house)"; Font = "test/shape/data/in-house/fonts/600387433d01cd5799e421dad6510a54c862f56b.ttf"; OpenType = false; Script = Scripts.Hangul
        Pools = hangulPools }
      { Name = "Hangul (65,535 glyphs)"; Font = "test/shape/data/text-rendering-tests/fonts/FDArrayTest65535.otf"; OpenType = false; Script = Scripts.Hangul
        Pools = hangulPools }
      { Name = "Arabic (Amiri)"; Font = "test/subset/data/fonts/Amiri-Regular.ttf"; OpenType = false; Script = Scripts.Arabic
        Pools = [ (30, [ (0x621, 0x64A) ]); (14, [ (0x64B, 0x65F) ]); (6, [ (0x654, 0x655); (0x658, 0x658); (0x6DC, 0x6DC); (0x6E3, 0x6E3); (0x6E7, 0x6E8); (0x8CA, 0x8CF); (0x8D3, 0x8D3); (0x8F3, 0x8F3) ])
                  (5, [ (0x670, 0x670); (0x6D6, 0x6ED) ]); (5, [ (0x610, 0x61A) ]); (4, [ (0x640, 0x640) ]); (8, [ (0x66E, 0x6D3) ]); (6, [ (0x622, 0x626); (0x6C0, 0x6C0); (0x6C2, 0x6C2); (0x6D3, 0x6D3) ]); (2, [ (0x660, 0x669) ]) ] @ shared }
      { Name = "Arabic (Nastaliq)"; Font = "perf/fonts/NotoNastaliqUrdu-Regular.ttf"; OpenType = false; Script = Scripts.Arabic
        Pools = [ (30, [ (0x621, 0x64A) ]); (14, [ (0x64B, 0x65F) ]); (6, [ (0x654, 0x655); (0x658, 0x658); (0x6DC, 0x6DC); (0x6E3, 0x6E3); (0x6E7, 0x6E8) ]); (8, [ (0x66E, 0x6D3) ]); (3, [ (0x6F0, 0x6F9) ]) ] @ shared }
      { Name = "Syriac (stch)"; Font = "test/shape/data/in-house/fonts/d9b8bc10985f24796826c29f7ccba3d0ae11ec02.ttf"; OpenType = false; Script = Scripts.Syriac
        Pools = [ (20, [ (0x710, 0x72C) ]); (14, [ (0x70F, 0x70F) ]); (10, [ (0x718, 0x718) ]); (6, [ (0x730, 0x74A) ]); (4, [ (0x2E, 0x2E) ]) ] @ shared }
      { Name = "Arabic (raised stch)"; Font = "test/shape/data/in-house/fonts/507637795ce4f2975593da54d12b46f76c7cc4cc.ttf"; OpenType = false; Script = Scripts.Arabic
        Pools = [ (20, [ (0x660, 0x669) ]); (12, [ (0x891, 0x891); (0x8E2, 0x8E2) ]); (8, [ (0x66B, 0x66B) ]); (10, [ (0x621, 0x64A) ]); (4, [ (0x64B, 0x65F) ]) ] @ shared } ]

/// <summary>Whether the plans of a subject have no pause that runs a function, so that its lines have no pause to compare.</summary>
/// <param name="subject">The subject.</param>
/// <returns>True for Hangul, Hebrew, Thai and Latin.</returns>
let private pauseless (subject: Subject) =
    subject.Script = Scripts.Hangul || subject.Script = Scripts.Hebrew || subject.Script = Scripts.Thai || subject.Script = Scripts.Latin

/// <summary>The fonts of the subjects, with their layout tables and without, loaded once.</summary>
let private fontsOf =
    lazy
        (subjects
         |> List.map (fun subject ->
             let bytes = UpstreamFiles.read subject.Font
             let plain = if subject.OpenType then withoutMorx bytes else bytes
             struct (Font.create (Face.create plain 0u), Font.create (Face.create (stripped bytes) 0u)))
         |> List.toArray)

/// <summary>Generates one code point of a subject from its weighted pools.</summary>
/// <param name="subject">The subject.</param>
/// <returns>The generator.</returns>
let private genCodepoint (subject: Subject) =
    subject.Pools
    |> List.map (fun (weight, ranges) -> weight, Gen.oneof (ranges |> List.map (fun (first, last) -> Gen.choose (first, last))))
    |> Gen.frequency
    |> Gen.map uint32

/// <summary>Generates a run of a subject with every field of the run varied, the length limit too when asked, so that a budget can fail
/// inside any stage.</summary>
/// <param name="limited">Whether the length limit of the run is varied.</param>
/// <param name="subject">The subject.</param>
/// <returns>The generator of the font variant and the run.</returns>
let private genCaseWith (limited: bool) (subject: Subject) =
    gen {
        let! strip = Gen.frequency [ 3, Gen.constant false; 1, Gen.constant true ]
        let! length = Gen.frequency [ 4, Gen.choose (1, 4); 8, Gen.choose (4, 16); 2, Gen.choose (17, 80); 1, Gen.choose (81, 200) ]
        let! text = Gen.arrayOfLength length (genCodepoint subject)
        let! direction = Gen.frequency [ 12, Gen.constant Direction.Ltr; 1, Gen.constant Direction.Rtl ]
        let! flags = genFlags
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! clusters = genClusters text.Length
        let! wild = Gen.frequency [ 8, Gen.constant false; 1, Gen.constant true ]
        let! garbage = Gen.arrayOfLength text.Length (Gen.choose (0, Int32.MaxValue))
        let! successful = Gen.frequency [ 24, Gen.constant true; 1, Gen.constant false ]
        let! ops = Gen.frequency [ 8, Gen.constant GlyphRun.MaxOpsDefault; 3, Gen.choose (0, 400); 1, Gen.choose (0, 5000) ]
        let! invisible = Gen.elements [ 0u; 0u; 3u; 77u ]
        let! notFound = Gen.elements [ 0u; 0u; 5u ]
        let! maxLen =
            match limited with
            | true -> Gen.frequency [ 3, Gen.constant GlyphRun.MaxLenDefault; 2, Gen.choose (0, 60) ]
            | false -> Gen.constant GlyphRun.MaxLenDefault
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
                SegmentProperties = { Direction = direction; Script = subject.Script; Language = Language.cLocale }
                Flags = flags
                ClusterLevel = level
                Invisible = invisible
                NotFound = notFound
                Successful = successful
                MaxLen = maxLen
                MaxOps = ops }
        return struct (strip, run)
    }

/// <summary>Generates a run of a subject with every field of the run varied.</summary>
/// <param name="subject">The subject.</param>
/// <returns>The generator of the font variant and the run.</returns>
let private genCase (subject: Subject) = genCaseWith false subject

/// <summary>Runs the stages before the normaliser, as the pipeline does.</summary>
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

/// <summary>The run a caller that decoded a line itself would hand to the route that writes into the decoded glyphs: glyphs of its own
/// and no properties array.</summary>
/// <param name="run">The run.</param>
/// <returns>The run with a copy of its glyphs and no properties.</returns>
let private freshOf (run: GlyphRun) =
    { run with Infos = Sealed.ofSpan (run.Infos.AsSpan()); Props = ImmutableArray.Empty }

/// <summary>What a checked run showed: whether the lean normaliser took it, whether it shortened or reordered, and whether the
/// working-set route shaped the whole line.</summary>
[<Struct>]
type private Outcome =
    { Lean: bool
      Changed: bool
      WorkingSet: bool }

/// <summary>Checks one run: the normaliser equals the out-buffer passes, and the entry equals the stage-by-stage pipeline, in
/// every field of the run.</summary>
/// <param name="features">The user features of the plan.</param>
/// <param name="index">The subject's index.</param>
/// <param name="case">The font variant and the run.</param>
/// <returns>What the run showed.</returns>
let private checkWith (features: ImmutableArray<Feature>) (index: int) (case: struct (bool * GlyphRun)) =
    let struct (strip, run) = case
    let struct (plain, bare) = fontsOf.Value[index]
    let font = if strip then bare else plain
    match Execute.createPlan font run.SegmentProperties features ValueNone with
    | Error _ -> { Lean = false; Changed = false; WorkingSet = false }
    | Ok shapePlan ->
        match shapePlan.Ot with
        | ValueNone -> { Lean = false; Changed = false; WorkingSet = false }
        | ValueSome plan ->
            let input = { prepared plan font { run with Successful = true } with MaxOps = run.MaxOps }
            let expected = Normalize.runPass plan font input
            let actual = Normalize.run plan font input
            try
                let entered = { run with Successful = true }
                expectSameRunAll "before the normaliser" (prepared plan font entered) (GeneralLine.beforeNormaliser plan font entered)
                expectSameRunAll "before the normaliser, in the working set" (prepared plan font entered) (GlyphRun.runOf (GeneralLine.beforeNormaliserSet plan font entered))
                expectSameRunAll "normalised" expected actual
                match Execute.runGeneral shapePlan font features run, Execute.run shapePlan font features run with
                | Ok reference, Ok entry ->
                    match GeneralLine.tryShape shapePlan plan font run with
                    | ValueSome (Ok route) -> expectSameRunAll "shaped by the working-set route" reference route
                    | ValueSome (Error _) -> failtest "the working-set route refused a run the general pipeline shaped"
                    | ValueNone -> ()
                    expectSameRunAll "shaped" reference entry
                    match Execute.tryRunFresh shapePlan font (freshOf run) with
                    | ValueSome (Ok route) -> expectSameRunAll "shaped from a run decoded for the stages to write into" reference route
                    | ValueSome (Error _) -> failtest "the route that writes into the decoded glyphs refused a run the general pipeline shaped"
                    | ValueNone -> ()
                | Error reference, Error entry ->
                    Expect.equal entry reference "errors"
                    match Execute.tryRunFresh shapePlan font (freshOf run) with
                    | ValueSome (Ok _) -> failtest "the route that writes into the decoded glyphs shaped a run the general pipeline refused"
                    | ValueSome (Error error) -> Expect.equal error reference "errors of the route that writes into the decoded glyphs"
                    | ValueNone -> ()
                | Ok _, Error _ -> failtest "the entry refused a run the general pipeline shaped"
                | Error _, Ok _ -> failtest "the entry shaped a run the general pipeline refused"
            with error ->
                let hex = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint.ToString "X"))
                let level = sprintf "%A" run.ClusterLevel
                let direction = sprintf "%A" run.SegmentProperties.Direction
                let clusters = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> string info.Cluster))
                printfn $"FAILED {subjects[index].Name} strip {strip} ops {run.MaxOps} len {run.MaxLen} flags {run.Flags} level {level} dir {direction} ok {run.Successful} invisible {run.Invisible} notfound {run.NotFound} text {hex} clusters {clusters} {error.Message}"
                reraise ()
            let worked =
                match GeneralLine.tryShape shapePlan plan font run with
                | ValueSome _ -> true
                | ValueNone -> false
            { Lean = ShaperHooks.leanNormalisation plan
              Changed = actual.Infos.Length <> input.Infos.Length || actual.Infos.AsSpan().ToArray() <> input.Infos.AsSpan().ToArray()
              WorkingSet = worked }

/// <summary>Checks one run with no user feature.</summary>
/// <param name="index">The subject's index.</param>
/// <param name="case">The font variant and the run.</param>
/// <returns>What the run showed.</returns>
let private check (index: int) (case: struct (bool * GlyphRun)) = checkWith ImmutableArray<Feature>.Empty index case

/// <summary>The user features the runs with features are shaped with, one set for each remainder of the run's length: features that turn
/// lookups off or on and ranges of clusters, so that the masks of the user features are set for a part of the line.</summary>
let private featureSets =
    let parsed (texts: string list) =
        texts
        |> List.map (fun text -> match Feature.parse text with | Ok feature -> feature | Error _ -> failwith $"feature {text}")
        |> List.toArray
    [| parsed [ "liga=0" ]
       parsed [ "ccmp=0"; "locl=0" ]
       parsed [ "kern=0"; "mark=0" ]
       parsed [ "abvs[1:4]=0"; "blws[2:]=0" ]
       parsed [ "pres=0"; "psts[0:3]=0"; "haln=0" ]
       parsed [ "init=0"; "medi=0"; "fina=0" ]
       parsed [ "calt[3:6]=0"; "rlig=0" ] |]
    |> Array.map ImmutableArray.CreateRange

/// <summary>Checks one run with the user features that its length selects.</summary>
/// <param name="index">The subject's index.</param>
/// <param name="case">The font variant and the run.</param>
/// <returns>What the run showed.</returns>
let private checkFeatured (index: int) (case: struct (bool * GlyphRun)) =
    let struct (_, run) = case
    checkWith featureSets[run.Infos.Length % featureSets.Length] index case

/// <summary>Copies the glyphs and the properties of a run, so that a pause may write into the copy.</summary>
/// <param name="run">The run.</param>
/// <returns>The run with arrays of its own.</returns>
let private copied (run: GlyphRun) =
    { run with Infos = Sealed.ofSpan (run.Infos.AsSpan()); Props = Sealed.ofSpan (run.Props.AsSpan()) }

/// <summary>Copies the arrays of a run into a working set of their own, so that a stage may write into the copy.</summary>
/// <param name="run">The run.</param>
/// <returns>The working set.</returns>
let private copiedSet (run: GlyphRun) = GlyphRun.setOf (copied run)

/// <summary>The code points of every decomposition and composition kind, with plain letters, spaces and the joiners: canonical
/// singletons, multi-step decompositions, Hangul syllables and jamo, composition exclusions, marks of equal and of different combining
/// classes, variation selectors, the zero width joiners and the combining grapheme joiner.</summary>
let private kindPools =
    [ (6, [ (0x41, 0x5A); (0x61, 0x7A) ]); (2, [ (0x20, 0x20); (0x30, 0x39) ]); (3, [ (0x200C, 0x200D); (0x34F, 0x34F) ])
      (10, [ (0x300, 0x36F) ]); (3, [ (0x340, 0x341); (0x343, 0x344); (0x374, 0x374); (0x37E, 0x37E); (0x387, 0x387); (0x1F71, 0x1F71); (0x2126, 0x2126); (0x212A, 0x212B) ])
      (6, [ (0xC0, 0x17F); (0x1E00, 0x1EFF) ]); (4, [ (0x1F00, 0x1FFF) ]); (2, [ (0x385, 0x385); (0x1FED, 0x1FED); (0xF900, 0xF90F); (0x2F800, 0x2F81F) ])
      (5, [ (0xAC00, 0xAC30); (0xB09A, 0xB09A); (0xD7A3, 0xD7A3) ]); (3, [ (0x1100, 0x1112); (0x1161, 0x1175); (0x11A8, 0x11C2) ])
      (4, [ (0x958, 0x95F); (0xFB1D, 0xFB4E); (0x2ADC, 0x2ADC); (0x9DC, 0x9DF); (0xA33, 0xA36); (0xA59, 0xA5E) ])
      (4, [ (0xF43, 0xF43); (0xF4D, 0xF4D); (0xF52, 0xF52); (0xF57, 0xF57); (0xF5C, 0xF5C); (0xF69, 0xF69); (0xF71, 0xF84) ])
      (5, [ (0x483, 0x489); (0x591, 0x5C7); (0x64B, 0x65F); (0x93C, 0x93C); (0x94D, 0x94D); (0xE31, 0xE3A); (0xE47, 0xE4E) ])
      (3, [ (0x5D0, 0x5EA); (0x915, 0x939); (0xE01, 0xE2E); (0xF40, 0xF6C); (0x1780, 0x17A2) ])
      (2, [ (0xFE00, 0xFE0F); (0x180B, 0x180D); (0xE0100, 0xE01EF) ]) ]

/// <summary>The subjects of the normaliser's kinds: the same fonts as some of the subjects above, with the pools of
/// <see cref="kindPools"/>.</summary>
let private kindSubjects =
    let kinds (name: string) (font: string) (script: Script) = { Name = name; Font = font; OpenType = false; Script = script; Pools = kindPools }
    [ kinds "Latin (Roboto)" "perf/fonts/Roboto-Regular.ttf" Scripts.Latin
      kinds "Latin (blank)" "test/subset/data/fonts/AdobeBlank-Regular.ttf" Scripts.Latin
      kinds "Greek (Roboto)" "perf/fonts/Roboto-Regular.ttf" Scripts.Greek
      kinds "Hebrew" "test/subset/data/fonts/NotoIKEAHebrewLatin-Regular.ttf" Scripts.Hebrew
      kinds "Thai" "test/fuzzing/fonts/kanit.ttf" Scripts.Thai
      kinds "Devanagari" "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf" Scripts.Devanagari
      kinds "Bengali" "test/subset/data/fonts/IndicTestHowrah-Regular.ttf" Scripts.Bengali
      kinds "Khmer" "test/subset/data/fonts/Khmer.ttf" Scripts.Khmer
      kinds "Myanmar" "test/subset/data/fonts/NotoSerifMyanmar-Regular.otf" Scripts.Myanmar
      kinds "Tibetan (USE, blank)" "test/subset/data/fonts/AdobeBlank-Regular.ttf" Scripts.Tibetan
      kinds "Newa (USE)" "test/subset/data/fonts/NotoSansNewa-Regular.ttf" Scripts.Newa
      kinds "Arabic (Amiri)" "test/subset/data/fonts/Amiri-Regular.ttf" Scripts.Arabic ]

/// <summary>The fonts of <see cref="kindSubjects"/>, loaded once.</summary>
let private kindFonts =
    lazy (kindSubjects |> List.map (fun subject -> Font.create (Face.create (UpstreamFiles.read subject.Font) 0u)) |> List.toArray)

/// <summary>Checks the normaliser on one run of <see cref="kindPools"/>: the lean normaliser, on a run and on a working set, equals the
/// out-buffer passes in every field.</summary>
/// <param name="index">The subject's index.</param>
/// <param name="case">The font variant, unused, and the run.</param>
/// <returns>Whether the plan was an OpenType one and the normaliser ran.</returns>
let private checkKinds (index: int) (case: struct (bool * GlyphRun)) =
    let struct (_, run) = case
    let font = kindFonts.Value[index]
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> false
    | Ok shapePlan ->
        match shapePlan.Ot with
        | ValueNone -> false
        | ValueSome plan ->
            let input = { prepared plan font { run with Successful = true } with MaxOps = run.MaxOps }
            let expected = Normalize.runPass plan font input
            let hex = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint.ToString "X"))
            try
                expectSameRunAll "normalised on a run" expected (Normalize.run plan font input)
                expectSameRunAll "normalised on a run with arrays of its own" expected (Normalize.runOwned plan font (copied input))
                expectSameRunAll "normalised on a working set" expected (GlyphRun.runOf (Normalize.runOwnedSet plan font (copiedSet input)))
            with error ->
                printfn $"FAILED {kindSubjects[index].Name} ops {run.MaxOps} len {run.MaxLen} text {hex} {error.Message}"
                reraise ()
            true

/// <summary>Compares one stage of the working-set route with the reference stage, both from the same run: the stage writes into a copy of
/// the run in a working set, the reference publishes, and every field of the two runs must be equal.</summary>
/// <param name="name">The stage's name.</param>
/// <param name="reference">The reference stage, on a run.</param>
/// <param name="owned">The stage that writes in place, on a working set.</param>
/// <param name="input">The run both stages read.</param>
/// <returns>The run the reference stage answers.</returns>
let private compareStage (name: string) (reference: GlyphRun -> GlyphRun) (owned: WorkingSet -> WorkingSet) (input: GlyphRun) =
    let expected = reference (copied input)
    expectSameRunAll name expected (GlyphRun.runOf (owned (copiedSet input)))
    expected

/// <summary>Checks one run stage by stage up to the glyphs and classes: each stage of the front half that writes into the working set
/// equals the reference stage on the run the reference stage before it answered.</summary>
/// <param name="index">The subject's index.</param>
/// <param name="case">The font variant and the run.</param>
/// <returns>Whether the plan was an OpenType one and the stages ran.</returns>
let private checkFront (index: int) (case: struct (bool * GlyphRun)) =
    let struct (strip, run) = case
    let struct (plain, bare) = fontsOf.Value[index]
    let font = if strip then bare else plain
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> false
    | Ok shapePlan ->
        match shapePlan.Ot with
        | ValueNone -> false
        | ValueSome plan ->
            try
                let propped = OtProps.setUnicodePropsEntered plan.Map.GlobalMask run
                expectSameRunAll "unicode properties" propped (GlyphRun.runOf (OtProps.setUnicodePropsEnteredSet plan.Map.GlobalMask run))
                let circled = compareStage "dotted circle" (OtProps.insertDottedCircle font) (GeneralLine.insertDottedCircleSet font) propped
                let clustered = compareStage "form clusters" OtProps.formClusters GeneralLine.formClustersSet circled
                let native = compareStage "native direction" OtProps.ensureNativeDirection GeneralLine.ensureNativeDirectionSet clustered
                let prepared = compareStage "preprocess" (ShaperHooks.preprocess plan font) (ShaperHooks.preprocessSet plan font) native
                let rotated = compareStage "rotate chars" (OtSubstitute.rotateChars plan font plan.Props.Direction) (GeneralLine.rotateCharsSet plan font) prepared
                let normalised = compareStage "normalise" (Normalize.run plan font) (Normalize.runOwnedSet plan font) rotated
                let glyphsReference (normalised: GlyphRun) =
                    let masked = OtSubstitute.setupMasks plan normalised
                    let recategorised =
                        match plan.FallbackMarkPositioning with
                        | true -> OtLayout.recategorizeMarks masked
                        | false -> masked
                    let started = OtSubstitute.substituteStart font (Normalize.mapGlyphsFast recategorised)
                    match plan.FallbackGlyphClasses with
                    | true -> OtProps.synthesizeGlyphClasses started
                    | false -> started
                let synthesised = compareStage "masks, glyphs and classes" glyphsReference (GeneralLine.glyphsOfSet plan font) normalised
                match plan.ApplyMorx with
                | true -> ()
                | false ->
                    let substituted = compareStage "GSUB lookups and pauses" (OtLayout.substitute plan font) (GeneralLine.lookupsSet shapePlan plan font) synthesised
                    match GeneralLine.positionedSet shapePlan plan font run.SegmentProperties.Direction (copiedSet substituted), OtShaper.fromSubstituted plan font run.SegmentProperties.Direction (copied substituted) with
                    | ValueSome (Ok positioned), Ok reference ->
                        expectSameRunAll "positioning and finishing" { reference with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u } (GlyphRun.runOf positioned)
                    | ValueSome (Error _), Error _ | ValueNone, _ -> ()
                    | ValueSome (Ok _), Error _ | ValueSome (Error _), Ok _ -> failtest "the positioning and the reference disagree on the error"
                true
            with error ->
                let hex = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint.ToString "X"))
                printfn $"FAILED STAGE {subjects[index].Name} strip {strip} ops {run.MaxOps} len {run.MaxLen} flags {run.Flags} level {run.ClusterLevel} dir {run.SegmentProperties.Direction} ok {run.Successful} text {hex} {error.Message}"
                reraise ()

/// <summary>The name of a pause, for the failure messages.</summary>
/// <param name="pause">The pause.</param>
/// <returns>The name.</returns>
let private pauseName (pause: PauseKind) =
    match pause with
    | NoPause -> "NoPause"
    | ClearSyllables -> "ClearSyllables"
    | KhmerSetupSyllables -> "KhmerSetupSyllables"
    | KhmerReorder -> "KhmerReorder"
    | IndicSetupSyllables -> "IndicSetupSyllables"
    | IndicInitialReordering -> "IndicInitialReordering"
    | IndicFinalReordering -> "IndicFinalReordering"
    | ClearSubstitutionFlags -> "ClearSubstitutionFlags"
    | UseSetupSyllables -> "UseSetupSyllables"
    | UseRecordRphf -> "UseRecordRphf"
    | UseRecordPref -> "UseRecordPref"
    | UseReorder -> "UseReorder"
    | MyanmarSetupSyllables -> "MyanmarSetupSyllables"
    | MyanmarReorder -> "MyanmarReorder"
    | ArabicRecordStch -> "ArabicRecordStch"
    | ArabicFallbackShape -> "ArabicFallbackShape"
    | ArabicReleaseAction -> "ArabicReleaseAction"

/// <summary>Compares the lookups of one window run three ways from the same run, in every field: the window cut out of the map for every
/// line (the reference), the window the plan cut once with its table of the lookups that cover each glyph, and that window written in
/// place on a copy.</summary>
/// <param name="shapePlan">The shape plan.</param>
/// <param name="font">The font.</param>
/// <param name="window">The window's ordinal.</param>
/// <param name="first">The first lookup of the window.</param>
/// <param name="last">The position after the last lookup of the window.</param>
/// <param name="plan">The ot plan.</param>
/// <param name="run">The run, with owned arrays.</param>
/// <returns>The run after the window, from the reference.</returns>
let private compareWindow (shapePlan: ShapePlan) (plan: OtShapePlan) (font: Font) window first last (run: GlyphRun) =
    let reference = OtLayout.substituteWindowIn shapePlan.GsubLookupCovered plan font first last run
    let uncoveredRun = OtLayout.substituteWindowIn ImmutableArray.Empty plan font first last run
    expectSameRunAll $"window {window} walked over every lookup" uncoveredRun reference
    let windowed = OtLayout.substituteWindowOf shapePlan.GsubLookupCovered font shapePlan.GsubWindows[window] run
    let owned = OtLayout.substituteWindowOwned shapePlan.GsubLookupCovered font shapePlan.GsubWindows[window] (copied run)
    expectSameRunAll $"window {window} with its table of covering lookups" reference windowed
    expectSameRunAll $"window {window} written in place" reference owned
    reference

/// <summary>Runs the stages of a plan one after another, and at each pause that runs a function compares the run the pause writes in
/// place with the run the reference function publishes, both from the same run, in every field; each window of lookups is compared the
/// same way.</summary>
/// <param name="shapePlan">The shape plan.</param>
/// <param name="plan">The ot plan.</param>
/// <param name="font">The font.</param>
/// <param name="stages">The compiled stages.</param>
/// <param name="stage">The next stage.</param>
/// <param name="window">The ordinal of the next window.</param>
/// <param name="first">The first lookup of the window.</param>
/// <param name="run">The run, with owned arrays.</param>
/// <param name="compared">The pauses compared so far.</param>
/// <returns>The number of pauses compared.</returns>
[<TailCall>]
let rec private comparePauses (shapePlan: ShapePlan) (plan: OtShapePlan) (font: Font) (stages: ImmutableArray<StageMap>) stage window first (run: GlyphRun) compared =
    match stage < stages.Length with
    | false ->
        compareWindow shapePlan plan font window first stages[stages.Length - 1].LastLookup run |> ignore
        compared
    | true ->
        let pause = stages[stage].Pause
        match PauseKind.runsFunction pause with
        | false -> comparePauses shapePlan plan font stages (stage + 1) window first run compared
        | true ->
            let applied = compareWindow shapePlan plan font window first stages[stage].LastLookup run
            let reference = ShaperHooks.applyPause pause plan font (copied applied)
            let owned = GlyphRun.runOf (ShaperHooks.applyPauseSet pause plan font (copiedSet applied))
            expectSameRunAll $"after the pause {pauseName pause}" reference owned
            comparePauses shapePlan plan font stages (stage + 1) (window + 1) stages[stage].LastLookup reference (compared + 1)

/// <summary>Checks one run at pause granularity: the run after each pause of the in-place form equals the run after the same pause of the
/// reference.</summary>
/// <param name="index">The subject's index.</param>
/// <param name="case">The font variant and the run.</param>
/// <returns>The number of pauses compared.</returns>
let private checkPauses (index: int) (case: struct (bool * GlyphRun)) =
    let struct (strip, run) = case
    let struct (plain, bare) = fontsOf.Value[index]
    let font = if strip then bare else plain
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> 0
    | Ok shapePlan ->
        match shapePlan.Ot with
        | ValueNone -> 0
        | ValueSome plan ->
            match plan.ApplyMorx || not plan.Map.HasPauses with
            | true -> 0
            | false ->
                try
                    let normalized = Normalize.runOwnedSet plan font (GeneralLine.beforeNormaliserSet plan font { run with Successful = true })
                    let synthesized = GlyphRun.runOf (GeneralLine.glyphsOfSet plan font normalized)
                    comparePauses shapePlan plan font plan.Map.GsubStages 0 0 0 synthesized 0
                with error ->
                    let hex = String.Join (" ", run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint.ToString "X"))
                    printfn $"FAILED PAUSE {subjects[index].Name} strip {strip} len {run.MaxLen} ops {run.MaxOps} flags {run.Flags} level {run.ClusterLevel} dir {run.SegmentProperties.Direction} text {hex} {error.Message}"
                    reraise ()

/// <summary>A pause with the run it receives.</summary>
[<Struct>]
type private PauseInput =
    { Pause: PauseKind
      Plan: OtShapePlan
      Font: Font
      Run: GlyphRun }

/// <summary>Runs the stages of a plan and collects every pause that runs a function with the run it receives, when that run has not failed:
/// a failed run takes the reference pause, which copies the run.</summary>
/// <param name="shapePlan">The shape plan.</param>
/// <param name="plan">The ot plan.</param>
/// <param name="font">The font.</param>
/// <param name="stages">The compiled stages.</param>
/// <param name="stage">The next stage.</param>
/// <param name="first">The first lookup of the window.</param>
/// <param name="run">The run, with owned arrays.</param>
/// <param name="found">The pauses collected so far, newest first.</param>
/// <returns>The pauses, newest first.</returns>
[<TailCall>]
let rec private collectPauses (shapePlan: ShapePlan) (plan: OtShapePlan) (font: Font) (stages: ImmutableArray<StageMap>) stage first (run: GlyphRun) (found: PauseInput list) =
    match stage < stages.Length with
    | false -> found
    | true ->
        let pause = stages[stage].Pause
        match PauseKind.runsFunction pause with
        | false -> collectPauses shapePlan plan font stages (stage + 1) first run found
        | true ->
            let applied = OtLayout.substituteWindowIn shapePlan.GsubLookupCovered plan font first stages[stage].LastLookup run
            let after = ShaperHooks.applyPause pause plan font (copied applied)
            let kept =
                match applied.Successful with
                | true -> { Pause = pause; Plan = plan; Font = font; Run = applied } :: found
                | false -> found
            collectPauses shapePlan plan font stages (stage + 1) stages[stage].LastLookup after kept

/// <summary>The pauses of the successful lines of a subject, each with the run it receives.</summary>
/// <param name="index">The subject's index.</param>
/// <param name="cases">The cases.</param>
/// <returns>The pauses.</returns>
let private pausesOf (index: int) (cases: struct (bool * GlyphRun) seq) =
    cases
    |> Seq.collect (fun (struct (strip, run)) ->
        let struct (plain, bare) = fontsOf.Value[index]
        let font = if strip then bare else plain
        match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
        | Error _ -> []
        | Ok shapePlan ->
            match shapePlan.Ot with
            | ValueNone -> []
            | ValueSome plan ->
                match plan.ApplyMorx || not plan.Map.HasPauses || not run.Successful with
                | true -> []
                | false ->
                    let normalized = Normalize.runOwnedSet plan font (GeneralLine.beforeNormaliserSet plan font run)
                    collectPauses shapePlan plan font plan.Map.GsubStages 0 0 (GlyphRun.runOf (GeneralLine.glyphsOfSet plan font normalized)) [])
    |> Seq.toArray

/// <summary>The bytes a function allocates over the runs of a group of pauses, less what copying their arrays allocates.</summary>
/// <param name="apply">The function that runs the pause on a copy of the run.</param>
/// <param name="inputs">The pauses.</param>
/// <returns>The bytes the pauses themselves allocate.</returns>
let private pauseBytes (apply: PauseInput -> WorkingSet -> unit) (inputs: PauseInput array) =
    let copying = Allocation.bytesAllocatedAtBest (fun () -> for input in inputs do copiedSet input.Run |> ignore)
    let applying = Allocation.bytesAllocatedAtBest (fun () -> for input in inputs do apply input (copiedSet input.Run))
    applying - copying

/// <summary>The most bytes a glyph a pause that writes in place may allocate: the categories of the universal machine's filtered input, the dotted
/// circles of broken syllables, and the arrays a would-substitute test builds; the reference pauses allocate several times as much.</summary>
/// <param name="pause">The pause.</param>
/// <returns>The bound.</returns>
let private ownedBytesPerGlyph (pause: PauseKind) =
    match pause with
    | IndicSetupSyllables | KhmerSetupSyllables | MyanmarSetupSyllables -> 1.0
    | UseSetupSyllables -> 20.0
    | IndicInitialReordering -> 45.0
    | IndicFinalReordering | KhmerReorder -> 5.0
    | MyanmarReorder -> 55.0
    | UseReorder -> 40.0
    | ClearSubstitutionFlags | UseRecordRphf | UseRecordPref -> 1.0
    | NoPause | ClearSyllables | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction -> Double.MaxValue

/// <summary>A run of the code points of a text, as the pipeline receives it: UTF-8, with the segment properties guessed.</summary>
/// <param name="text">The text.</param>
/// <returns>The run.</returns>
let private runOf (text: string) =
    let bytes = Text.Encoding.UTF8.GetBytes text
    GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty)

/// <summary>Shapes a text with the entry of the pipeline and returns the code points the glyphs were made from.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <returns>The glyphs, or none when the plan cannot be made.</returns>
let private shapedGlyphs (font: Font) (text: string) =
    let run = runOf text
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> [||]
    | Ok shapePlan ->
        match Execute.run shapePlan font ImmutableArray<Feature>.Empty run with
        | Error _ -> [||]
        | Ok shaped -> shaped.Infos.AsSpan().ToArray() |> Array.map _.Codepoint

/// <summary>Compares the lookups the plan of a case keeps ready for the kernel, in its whole map and in each of its windows, with the lookups
/// built afresh, over the glyphs the font maps for the case's code points.</summary>
/// <param name="index">The subject's index.</param>
/// <param name="case">The font variant and the run.</param>
/// <returns>The lookups compared and the lookups that differ.</returns>
let private checkReady (index: int) (case: struct (bool * GlyphRun)) =
    let struct (strip, run) = case
    let struct (plain, bare) = fontsOf.Value[index]
    let font = if strip then bare else plain
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> struct (0, 0)
    | Ok shapePlan ->
        match shapePlan.Ot with
        | ValueNone -> struct (0, 0)
        | ValueSome plan ->
            let infos =
                run.Infos.AsSpan().ToArray()
                |> Array.map (fun info -> match Font.nominalGlyph font info.Codepoint with | ValueSome glyph -> { info with Codepoint = glyph } | ValueNone -> info)
            let glyphs = { run with Infos = ImmutableArray.CreateRange infos; Props = Sealed.init infos.Length (fun _ -> Unchecked.defaultof<GlyphProps>) }
            let struct (whole, wholeDiffering) = Engine.readyDifferences plan font shapePlan.GsubLookupCovered shapePlan.GsubGlyphLookups glyphs
            let windows = [ for window in shapePlan.GsubWindows -> Engine.windowReadyDifferences shapePlan.GsubLookupCovered window font glyphs ]
            struct (whole + (windows |> List.sumBy (fun (struct (compared, _)) -> compared)), wholeDiffering + (windows |> List.sumBy (fun (struct (_, differing)) -> differing)))

/// <summary>The tests: for each script of a complex shaper, the lean normaliser and the working-set route equal the references.</summary>
[<Tests>]
let tests =
    testList "complex shaper routes" [
        test "Malayalam: the dotted circle of a broken syllable that begins with a repha goes after the repha, as HarfBuzz shapes U+0D4E U+0D4D U+200D (in-house cluster.tests)" {
            let font = Font.create (Face.create (UpstreamFiles.read "test/shape/data/in-house/fonts/fd07ea46e4d8368ada1776208c07fd596f727852.ttf") 0u)
            let glyph codepoint = match Font.nominalGlyph font codepoint with | ValueSome glyph -> glyph | ValueNone -> failtest "the font maps the code point"
            let glyphs = shapedGlyphs font "\u0D4E\u0D4D\u200D"
            Expect.equal glyphs[0..1] [| glyph 0xD4Eu; glyph 0x25CCu |] "the repha, then the dotted circle"
            let index = subjects |> List.findIndex (fun subject -> subject.Name = "Malayalam")
            let compared = checkPauses index (struct (false, runOf "\u0D4E\u0D4D\u200D"))
            Expect.isGreaterThan compared 0 "the pauses of the run are compared"
        }
        test "Hangul: a run the budget has already failed maps no glyph in the normaliser's simple path either, as the round that runs only while the buffer succeeds" {
            let index = subjects |> List.findIndex (fun subject -> subject.Name = "Hangul (65,535 glyphs)")
            let struct (plain, _) = fontsOf.Value[index]
            let text = [| 0x200Du; 0x1100u; 0xB09Au; 0xAEB0u; 0xC9C4u; 0x11B5u; 0xA972u; 0x116Fu; 0xA96Au; 0x1163u; 0xB9F4u; 0x1120u; 0xCCE5u; 0xD7B8u; 0xBBDAu; 0xCA2Cu; 0x3142u; 0xD7C7u |]
            let infos = Sealed.init text.Length (fun position -> { Codepoint = text[position]; Mask = 0u; Cluster = uint32 position; Var1 = 0u; Var2 = 0u })
            let run = { GlyphRun.empty with Infos = infos; ContentType = UnicodeContent; SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Hangul; Language = Language.cLocale }; MaxOps = 3 }
            match Execute.createPlan plain run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> failtest "the plan is made"
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> failtest "the plan has an ot plan"
                | ValueSome plan ->
                    let input = { prepared plan plain { run with Successful = true } with MaxOps = 3 }
                    Expect.isFalse input.Successful "the three operations of the budget are spent before the normaliser"
                    expectSameRunAll "normalised" (Normalize.runPass plan plain input) (Normalize.run plan plain input)
        }
        test "Devanagari: a map that shares the cell of another map keeps the lookups ready only for the lookups it was built from" {
            let index = subjects |> List.findIndex (fun subject -> subject.Name = "Devanagari")
            let struct (font, _) = fontsOf.Value[index]
            let run = runOf "\u0915\u094D\u0937\u093F \u0915\u094D\u0937\u093F"
            match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> failtest "the plan is made"
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> failtest "the plan has an ot plan"
                | ValueSome plan ->
                    let lookups = plan.Map.GsubLookups
                    Expect.isGreaterThan lookups.Length 2 "the map has lookups to reorder"
                    let glyphs = { run with Props = Sealed.init run.Infos.Length (fun _ -> Unchecked.defaultof<GlyphProps>) }
                    let struct (compared, differing) = Engine.readyDifferences plan font shapePlan.GsubLookupCovered shapePlan.GsubGlyphLookups glyphs
                    Expect.isGreaterThan compared 0 "the map's own lookups are kept ready"
                    Expect.equal differing 0 "and equal the lookups built afresh"
                    let reversed = { plan with Map = { plan.Map with GsubLookups = Sealed.init lookups.Length (fun position -> lookups[lookups.Length - 1 - position]) } }
                    let struct (sharedCompared, _) = Engine.readyDifferences reversed font shapePlan.GsubLookupCovered shapePlan.GsubGlyphLookups glyphs
                    Expect.equal sharedCompared 0 "a map with other lookups does not take the lookups kept for the first"
        }
        for index in 0 .. kindSubjects.Length - 1 do
            let subject = kindSubjects[index]
            test $"{subject.Name}: the lean normaliser equals the out-buffer passes on code points of every decomposition and composition kind" {
                let cases = Gen.sampleWithSeed (Rnd (Seed + 900UL + uint64 index)) 30 CaseCount (genCase subject)
                let compared = cases |> Seq.map (checkKinds index) |> Seq.filter id |> Seq.length
                printfn $"{subject.Name}: {compared} cases compared"
                Expect.isGreaterThan compared (CaseCount * 9 / 10) "the normaliser ran on the cases"
            }
            test $"{subject.Name}: the lean normaliser equals the out-buffer passes on such code points with the budgets and the length limit varied" {
                let cases = Gen.sampleWithSeed (Rnd (Seed + 950UL + uint64 index)) 30 CaseCount (genCaseWith true subject)
                cases |> Seq.iter (fun case -> checkKinds index case |> ignore)
            }
        for index in 0 .. subjects.Length - 1 do
            let subject = subjects[index]
            test $"{subject.Name}: the lean normaliser and the working-set route equal the references in every field" {
                let cases = Gen.sampleWithSeed (Rnd (Seed + uint64 index)) 30 CaseCount (genCase subject)
                let results = cases |> Seq.map (check index) |> Seq.toArray
                let count pick = results |> Array.filter pick |> Array.length
                let lean = count _.Lean
                let changed = count _.Changed
                let working = count _.WorkingSet
                printfn $"{subject.Name}: {cases.Length} cases, {lean} with the lean normaliser, {changed} changed by normalisation, {working} on the working set"
                match subject.Script = Scripts.Hangul with
                | true -> Expect.equal lean 0 "the lean normaliser does not read Hangul, whose pass is the out-buffer one"
                | false -> Expect.isGreaterThan lean (CaseCount * 9 / 10) "the lean normaliser reads this shaper"
                Expect.isGreaterThan working (CaseCount / 4) "the working-set route shapes many lines"
            }
            test $"{subject.Name}: every stage of the front half that writes into the working set equals the reference stage, with the budgets failing inside any stage" {
                let cases = Gen.sampleWithSeed (Rnd (Seed + 300UL + uint64 index)) 30 CaseCount (genCaseWith true subject)
                let compared = cases |> Seq.map (checkFront index) |> Seq.filter id |> Seq.length
                printfn $"{subject.Name}: {compared} cases compared stage by stage"
                Expect.isGreaterThan compared (CaseCount / 2) "the stages are compared"
            }
            test $"{subject.Name}: the working-set route equals the references with user features that set masks for a range of clusters" {
                let cases = Gen.sampleWithSeed (Rnd (Seed + 600UL + uint64 index)) 30 (CaseCount / 2) (genCase subject)
                cases |> Seq.iter (fun case -> checkFeatured index case |> ignore)
            }
            test $"{subject.Name}: the working-set route equals the references with the length limit varied" {
                let cases = Gen.sampleWithSeed (Rnd (Seed + 400UL + uint64 index)) 30 CaseCount (genCaseWith true subject)
                let diagnose (case: struct (bool * GlyphRun)) =
                    try
                        checkFront index case |> ignore
                        printfn "every stage up to the positioning equals the reference stage on this run"
                    with stage -> printfn $"the first stage that differs: {stage.Message}"
                    try
                        printfn $"windows and pauses compared on this run: {checkPauses index case}"
                    with windows -> printfn $"the first window or pause that differs: {windows.Message}"
                for case in cases do
                    try
                        check index case |> ignore
                    with error ->
                        diagnose case
                        raise error
            }
            test $"{subject.Name}: the lookups the plan keeps ready equal the lookups built afresh, and admit what they admit" {
                let cases = Gen.sampleWithSeed (Rnd (Seed + 800UL + uint64 index)) 30 (CaseCount / 5) (genCase subject)
                let results = cases |> Seq.map (checkReady index) |> Seq.toArray
                let compared = results |> Array.sumBy (fun (struct (compared, _)) -> compared)
                let differing = results |> Array.sumBy (fun (struct (_, differing)) -> differing)
                printfn $"{subject.Name}: {compared} lookups compared in {cases.Length} cases, {differing} differ"
                Expect.equal differing 0 "every lookup kept ready equals the lookup built afresh"
                match subject.Name with
                | "Hangul (65,535 glyphs)" | "Hangul (AdobeBlank)" -> Expect.equal compared 0 "the font has no lookups"
                | _ -> Expect.isGreaterThan compared 0 "lookups are compared"
            }
            test $"{subject.Name}: every length limit from 0 to the length plus 12 raises nothing, and the stages and the route equal the references" {
                let cases =
                    Gen.sampleWithSeed (Rnd (Seed + 700UL + uint64 index)) 30 150 (genCase subject)
                    |> Seq.filter (fun (struct (_, run)) -> run.Infos.Length <= 24)
                    |> Seq.truncate 40
                    |> Seq.toArray
                for case in cases do
                    let struct (strip, run) = case
                    for limit in 0 .. run.Infos.Length + 12 do
                        let limited = struct (strip, { run with MaxLen = limit })
                        checkFront index limited |> ignore
                        check index limited |> ignore
                Expect.isGreaterThan cases.Length 10 "the sweep has cases to run"
            }
            if not (pauseless subject) then
                test $"{subject.Name}: the pauses that write in place allocate next to nothing, where the reference pauses copy the run" {
                    let cases = Gen.sampleWithSeed (Rnd (Seed + 200UL + uint64 index)) 30 600 (genCase subject) |> Seq.filter (fun (struct (_, run)) -> run.Successful && run.Flags = BufferFlags.Default)
                    let inputs = pausesOf index cases
                    let kinds = inputs |> Array.map _.Pause |> Array.distinct
                    for kind in kinds do
                        let group = inputs |> Array.filter (fun input -> input.Pause = kind)
                        let glyphs = group |> Array.sumBy (fun input -> input.Run.Infos.Length)
                        let reference = pauseBytes (fun input set -> ShaperHooks.applyPause input.Pause input.Plan input.Font (GlyphRun.runOf set) |> ignore) group
                        let owned = pauseBytes (fun input set -> ShaperHooks.applyPauseSet input.Pause input.Plan input.Font set |> ignore) group
                        let perGlyph (bytes: int64) = float bytes / float glyphs
                        printfn $"{subject.Name} {pauseName kind}: {glyphs} glyphs, reference {perGlyph reference:F1} B/glyph, in place {perGlyph owned:F1} B/glyph"
                        Expect.isLessThanOrEqual (perGlyph owned) (ownedBytesPerGlyph kind) $"the pause {pauseName kind} allocates at most {ownedBytesPerGlyph kind} bytes a glyph"
                }
                test $"{subject.Name}: every window and pause that writes in place equals the reference, with the length limit varied" {
                    let cases = Gen.sampleWithSeed (Rnd (Seed + 500UL + uint64 index)) 30 CaseCount (genCaseWith true subject)
                    let compared = cases |> Seq.map (checkPauses index) |> Seq.sum
                    printfn $"{subject.Name}: {compared} pauses compared in {cases.Length} cases with the length limit varied"
                    Expect.isGreaterThan compared (CaseCount / 4) "the pauses are compared"
                }
                test $"{subject.Name}: every pause that writes in place equals the reference pause, run after run" {
                    let cases = Gen.sampleWithSeed (Rnd (Seed + 100UL + uint64 index)) 30 CaseCount (genCase subject)
                    let compared = cases |> Seq.map (checkPauses index) |> Seq.sum
                    printfn $"{subject.Name}: {compared} pauses compared in {cases.Length} cases"
                    Expect.isGreaterThan compared (CaseCount / 4) "the pauses are compared"
                }
    ]
