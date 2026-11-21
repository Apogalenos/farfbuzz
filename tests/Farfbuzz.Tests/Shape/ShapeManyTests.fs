module Farfbuzz.Tests.Shape.ShapeManyTests

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq
open System.Text
open System.Threading
open System.Threading.Tasks
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Whether two immutable arrays hold equal elements in the same order.</summary>
/// <param name="a">The first array.</param>
/// <param name="b">The second array.</param>
/// <returns>True when the elements are equal pairwise.</returns>
let private sameArray (a: ImmutableArray<'T>) (b: ImmutableArray<'T>) : bool =
    a.AsEnumerable().SequenceEqual(b.AsEnumerable())

/// <summary>Whether two runs are equal on every field, the arrays element by element.</summary>
/// <param name="a">The first run.</param>
/// <param name="b">The second run.</param>
/// <returns>True when every field is equal.</returns>
let sameRun (a: GlyphRun) (b: GlyphRun) : bool =
    sameArray a.Infos b.Infos
    && sameArray a.Positions b.Positions
    && sameArray a.Props b.Props
    && sameArray a.ContextBefore b.ContextBefore
    && sameArray a.ContextAfter b.ContextAfter
    && a.ContentType = b.ContentType
    && a.SegmentProperties = b.SegmentProperties
    && a.Flags = b.Flags
    && a.ClusterLevel = b.ClusterLevel
    && a.Replacement = b.Replacement
    && a.Invisible = b.Invisible
    && a.NotFound = b.NotFound
    && a.NotFoundVariationSelector = b.NotFoundVariationSelector
    && a.RandomState = b.RandomState
    && a.HavePositions = b.HavePositions
    && a.Successful = b.Successful
    && a.Serial = b.Serial
    && a.ScratchFlags = b.ScratchFlags
    && a.MaxLen = b.MaxLen
    && a.MaxOps = b.MaxOps

/// <summary>Whether two shaping results are equal: the same run on every field, or the same error.</summary>
/// <param name="a">The first result.</param>
/// <param name="b">The second result.</param>
/// <returns>True when both are equal.</returns>
let sameResult (a: Result<GlyphRun, ShapeError>) (b: Result<GlyphRun, ShapeError>) : bool =
    match a, b with
    | Ok x, Ok y -> sameRun x y
    | Ok _, Error _ -> false
    | Error _, Ok _ -> false
    | Error (OutlinesNotPorted x), Error (OutlinesNotPorted y) -> x = y
    | Error (NoShaperSucceeded x), Error (NoShaperSucceeded y) -> sameArray x y
    | Error (VerifyFailed (checkX, runX)), Error (VerifyFailed (checkY, runY)) -> checkX = checkY && sameRun runX runY
    | Error (OutlinesNotPorted _), Error _ -> false
    | Error (NoShaperSucceeded _), Error _ -> false
    | Error (VerifyFailed _), Error _ -> false

/// <summary>Whether two plan caches hold the same keys in the same order.</summary>
/// <param name="a">The first cache.</param>
/// <param name="b">The second cache.</param>
/// <returns>True when both caches hold equal keys in the same order.</returns>
let private sameCache (a: PlanCache) (b: PlanCache) : bool =
    a.Entries.Length = b.Entries.Length
    && Seq.forall2 (fun (x: PlanEntry) (y: PlanEntry) -> EqualityComparer<ShapePlanKey>.Default.Equals(x.Key, y.Key)) a.Entries b.Entries

/// <summary>Shapes runs one after the other with <c>shapeCached</c>, threading the cache: the reference <c>shapeMany</c> must equal.</summary>
/// <param name="cache">The plan cache.</param>
/// <param name="font">The font.</param>
/// <param name="features">The user features.</param>
/// <param name="shapers">The requested shapers.</param>
/// <param name="runs">The runs.</param>
/// <returns>The results in order and the final cache.</returns>
let sequentialReference (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (runs: GlyphRun array) =
    let results = Array.zeroCreate runs.Length
    let mutable kept = cache
    for index in 0 .. runs.Length - 1 do
        let struct (result, next) = Shape.shapeCached kept font features shapers runs[index]
        results[index] <- result
        kept <- next
    struct (results, kept)

/// <summary>Reads a font file of the perf corpus.</summary>
/// <param name="name">The file name.</param>
/// <returns>The file bytes.</returns>
let private perfFont (name: string) = read $"perf/fonts/{name}"

/// <summary>The faces the property shapes with.</summary>
let private fonts =
    lazy
        [| Font.create (Face.create (perfFont "Roboto-Regular.ttf") 0u)
           Font.create (Face.create (perfFont "Amiri-Regular.ttf") 0u)
           Font.create (Face.create (read "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf") 0u) |]

/// <summary>Code point ranges of the scripts the documents mix: Latin, Greek, Hebrew, Thai, Devanagari, Arabic (whose shaper may be
/// ported), Myanmar (ported) and neutral digits and punctuation.</summary>
let private ranges =
    [| struct (0x41, 0x7A); struct (0x391, 0x3C9); struct (0x5D0, 0x5EA); struct (0xE01, 0xE3A)
       struct (0x905, 0x939); struct (0x627, 0x64A); struct (0x1000, 0x1021); struct (0x20, 0x40) |]

/// <summary>One random line: its code points and the direction forced on it, if any.</summary>
let private lineGen : Gen<struct (uint32 array * Direction)> =
    gen {
        let! length = Gen.frequency [ (1, Gen.constant 0); (1, Gen.constant 1); (8, Gen.choose (2, 40)) ]
        let! mixed = Gen.choose (0, 4)
        let! primary = Gen.elements ranges
        let! forced = Gen.elements [| Direction.Invalid; Direction.Invalid; Direction.Invalid; Direction.Ltr; Direction.Rtl |]
        let pick =
            gen {
                let! range = match mixed with | 0 -> Gen.elements ranges | _ -> Gen.constant primary
                let struct (low, high) = range
                let! code = Gen.choose (low, high)
                return uint32 code
            }
        let! text = Gen.arrayOfLength length pick
        return struct (text, forced)
    }

/// <summary>One random document and everything that shapes it.</summary>
type private DocumentCase =
    { FontIndex: int
      Features: ImmutableArray<Feature>
      Shapers: ImmutableArray<string> voption
      Verify: bool
      Degree: int
      Lines: (struct (uint32 array * Direction)) array
      Warm: bool }

/// <summary>The feature lists the documents use.</summary>
let private featureSets =
    [| ImmutableArray<Feature>.Empty
       ImmutableArray.Create { Tag = Tag.ofString "kern"; Value = 0u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
       ImmutableArray.Create { Tag = Tag.ofString "liga"; Value = 0u; Start = 2u; End = 7u }
       ImmutableArray.Create { Tag = Tag.ofString "smcp"; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd } |]

/// <summary>The shaper lists the documents use: the default, the ot shaper, the fallback shaper and one that names no shaper.</summary>
let private shaperSets =
    [| ValueNone; ValueSome (ImmutableArray.Create "ot"); ValueSome (ImmutableArray.Create "fallback"); ValueSome (ImmutableArray.Create "nothing") |]

/// <summary>A random document: no lines, one line, a few, many, or enough glyphs to be shaped on several threads.</summary>
let private documentGen : Gen<DocumentCase> =
    gen {
        let! fontIndex = Gen.choose (0, 2)
        let! features = Gen.elements featureSets
        let! shapers = Gen.frequency [ (6, Gen.constant shaperSets[0]); (3, Gen.constant shaperSets[1]); (2, Gen.constant shaperSets[2]); (1, Gen.constant shaperSets[3]) ]
        let! verify = Gen.elements [| false; true |]
        let! degree = Gen.elements [| 1; 2; 4; Shape.AllCores |]
        let! lineCount = Gen.frequency [ (3, Gen.choose (0, 1)); (6, Gen.choose (2, 5)); (6, Gen.choose (6, 60)); (2, Gen.choose (150, 300)) ]
        let! lines = Gen.arrayOfLength lineCount lineGen
        let! warm = Gen.elements [| false; true |]
        return { FontIndex = fontIndex; Features = features; Shapers = shapers; Verify = verify; Degree = degree; Lines = lines; Warm = warm }
    }

/// <summary>Builds the run of one line, as a caller would: its code points, the forced direction, the verification flag, then
/// the guessed segment properties.</summary>
/// <param name="verify">Whether the verification flag is set.</param>
/// <param name="line">The code points and the forced direction.</param>
/// <returns>The run.</returns>
let private runOf (verify: bool) (line: struct (uint32 array * Direction)) : GlyphRun =
    let struct (text, direction) = line
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
        |> GlyphRun.withDirection direction
        |> TailCalls.settle
    let flagged =
        match verify with
        | true -> { run with Flags = BufferFlags.Verify }
        | false -> run
    GlyphRun.guessSegmentProperties Language.cLocale flagged

/// <summary>Whether <c>shapeMany</c> equals the sequential map of <c>shapeCached</c> on a document, in every result and in the cache.</summary>
/// <param name="case">The document and its settings.</param>
/// <returns>True when both agree.</returns>
let private manyEqualsSequential (case: DocumentCase) : bool =
    let font = fonts.Value[case.FontIndex]
    let runs = case.Lines |> Array.map (runOf case.Verify)
    let start =
        match case.Warm, runs with
        | true, [||] -> PlanCache.empty
        | true, _ ->
            let struct (_, kept) = Shape.shapeCached PlanCache.empty font case.Features case.Shapers runs[0]
            kept
        | false, _ -> PlanCache.empty
    let struct (expected, expectedCache) = sequentialReference start font case.Features case.Shapers runs
    let struct (actual, actualCache) = Shape.shapeMany case.Degree start font case.Features case.Shapers (ImmutableArray.CreateRange runs)
    actual.Length = expected.Length
    && Seq.forall2 sameResult actual expected
    && sameCache actualCache expectedCache

/// <summary>The lines of a perf text: the first ones, as shaped with the rest of the text after them.</summary>
/// <param name="relative">The text path under the upstream checkout.</param>
/// <param name="count">The number of lines.</param>
/// <returns>The line runs.</returns>
let private textRuns (relative: string) (count: int) : GlyphRun array =
    let bytes = (read relative).ToArray()
    let text = Encoding.UTF8.GetString bytes
    text.Split('\n')
    |> Array.take count
    |> Array.map (fun line ->
        let units = Encoding.UTF8.GetBytes line
        GlyphRun.addUtf8 (ReadOnlySpan units) 0 -1 GlyphRun.empty
        |> GlyphRun.guessSegmentProperties Language.cLocale)

/// <summary>A face, built from its bytes at every use so that nothing of it is warm, and the runs that exercise it.</summary>
type private ColdCase =
    { Name: string
      Bytes: unit -> ReadOnlyMemory<byte>
      Runs: GlyphRun array
      Variation: float32 voption }

/// <summary>The cold-face cases: layout tables with GSUB and GPOS, a complex script, a variable font and a face whose script has no
/// shaper on this line.</summary>
let private coldCases =
    lazy
        [| { Name = "Roboto"; Bytes = (fun () -> perfFont "Roboto-Regular.ttf"); Runs = textRuns "perf/texts/en-thelittleprince.txt" 24; Variation = ValueNone }
           { Name = "Devanagari"; Bytes = (fun () -> read "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf"); Runs = textRuns "perf/texts/hi-words.txt" 60; Variation = ValueNone }
           { Name = "SourceSerifVariable"; Bytes = (fun () -> read "test/subset/data/fonts/SourceSerifVariable-Roman.ttf"); Runs = textRuns "perf/texts/react-dom.txt" 24; Variation = ValueSome 650f }
           { Name = "Amiri"; Bytes = (fun () -> perfFont "Amiri-Regular.ttf"); Runs = textRuns "perf/texts/fa-thelittleprince.txt" 12; Variation = ValueNone } |]

/// <summary>The threads of a stress round: more than the cores, so that threads preempt one another inside a first use.</summary>
let private stressThreads = max 12 (Environment.ProcessorCount + 4)

/// <summary>The fresh faces each case is shaped from in the stress test.</summary>
[<Literal>]
let private ColdRounds = 200

/// <summary>Opens a font on a face, with the case's variation.</summary>
/// <param name="case">The case.</param>
/// <param name="face">The face.</param>
/// <returns>The font.</returns>
let private fontOn (case: ColdCase) (face: Face) : Font =
    let font = Font.create face
    match case.Variation with
    | ValueSome value -> Font.setVariation (Tag.ofString "wght") value font
    | ValueNone -> font

/// <summary>Runs one piece of work on several dedicated threads that start together and returns each thread's result.</summary>
/// <param name="threads">The number of threads.</param>
/// <param name="work">The work, given the thread's number.</param>
/// <returns>The results by thread.</returns>
let private onThreads (threads: int) (work: int -> 'T) : 'T array =
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

/// <summary>Shapes every run of a case with <c>shapeCached</c> from an empty cache.</summary>
/// <param name="font">The font.</param>
/// <param name="runs">The runs.</param>
/// <returns>The results.</returns>
let private shapeAllRuns (font: Font) (runs: GlyphRun array) =
    let struct (results, _) = sequentialReference PlanCache.empty font ImmutableArray.Empty ValueNone runs
    results

/// <summary>The content of the arrays of a run, copied: what shaping must leave as it was.</summary>
/// <param name="run">The run.</param>
/// <returns>Copies of the infos, positions and properties.</returns>
let private snapshotOf (run: GlyphRun) =
    struct (run.Infos.ToArray(), run.Positions.ToArray(), run.Props.ToArray())

/// <summary>Whether a run still holds the content it had when it was copied.</summary>
/// <param name="run">The run.</param>
/// <param name="snapshot">The copies.</param>
/// <returns>True when nothing changed.</returns>
let private unchanged (run: GlyphRun) (snapshot: struct (GlyphInfo array * GlyphPosition array * GlyphProps array)) =
    let struct (infos, positions, props) = snapshot
    run.Infos.AsEnumerable().SequenceEqual infos && run.Positions.AsEnumerable().SequenceEqual positions && run.Props.AsEnumerable().SequenceEqual props

/// <summary>Shapes a case on a fresh cold face from many threads sharing one font and checks every result against the sequential
/// one; a thread that finds the shared font's lazy tables mid-build must still see the one value. The input runs, shared by
/// every thread, must come out of all the rounds as they went in.</summary>
/// <param name="case">The case.</param>
/// <param name="expected">The sequential results, from another face.</param>
let private stressShaping (case: ColdCase) (expected: Result<GlyphRun, ShapeError> array) =
    let bytes = case.Bytes ()
    let before = case.Runs |> Array.map snapshotOf
    for round in 1 .. ColdRounds do
        let font = fontOn case (Face.create bytes 0u)
        let outcomes = onThreads stressThreads (fun _ -> Seq.forall2 sameResult (shapeAllRuns font case.Runs) expected)
        Expect.isTrue (Array.forall id outcomes) $"{case.Name}: round {round}: a thread's results differ from the sequential ones"
    Expect.isTrue (Array.forall2 unchanged case.Runs before) $"{case.Name}: shaping changed an input run"

/// <summary>Makes each thread open its own font on the same cold face, so that the face's accelerator cell is raced for, then shape.</summary>
/// <param name="case">The case.</param>
/// <param name="expected">The sequential results, from another face.</param>
let private stressFaceCell (case: ColdCase) (expected: Result<GlyphRun, ShapeError> array) =
    let bytes = case.Bytes ()
    for round in 1 .. ColdRounds do
        let face = Face.create bytes 0u
        let outcomes = onThreads stressThreads (fun _ -> Seq.forall2 sameResult (shapeAllRuns (fontOn case face) case.Runs) expected)
        Expect.isTrue (Array.forall id outcomes) $"{case.Name}: round {round}: a thread's results differ from the sequential ones"

/// <summary>Every font function on the first glyphs and code points of a font, as boxed values to compare.</summary>
/// <param name="font">The font.</param>
/// <returns>The values.</returns>
let private fontDigest (font: Font) : (obj | null) array =
    let glyphs = min font.Face.GlyphCount 120
    [| yield box (Font.hExtents font)
       yield box (Font.vExtents font)
       for glyph in 0 .. glyphs - 1 do
           yield box (Font.hAdvance font (uint32 glyph))
           yield box (Font.vAdvance font (uint32 glyph))
           yield box (Font.glyphName font (uint32 glyph))
           yield box (Extents.glyph font (uint32 glyph))
       for codepoint in 0x20 .. 0x7E do
           yield box (Font.nominalGlyph font (uint32 codepoint))
       for codepoint in 0x905 .. 0x93F do
           yield box (Font.nominalGlyph font (uint32 codepoint)) |]

/// <summary>Whether two digests are equal.</summary>
/// <param name="a">The first digest.</param>
/// <param name="b">The second digest.</param>
/// <returns>True when every value is equal.</returns>
let private sameDigest (a: (obj | null) array) (b: (obj | null) array) : bool =
    a.Length = b.Length && Array.forall2 (fun (x: obj | null) (y: obj | null) -> Object.Equals(x, y)) a b

/// <summary>Calls the font functions on a fresh cold face from many threads, each opening its own font on the face.</summary>
/// <param name="case">The case.</param>
let private stressFontFunctions (case: ColdCase) =
    let bytes = case.Bytes ()
    let expected = fontDigest (fontOn case (Face.create bytes 0u))
    for round in 1 .. ColdRounds do
        let face = Face.create bytes 0u
        let outcomes = onThreads stressThreads (fun _ -> sameDigest (fontDigest (fontOn case face)) expected)
        Expect.isTrue (Array.forall id outcomes) $"{case.Name}: round {round}: a thread's font values differ from the sequential ones"

/// <summary>The document-level parallel shaping tests (P9.S14).</summary>
[<Tests>]
let tests =
    testList "P9.S14" [
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "P9.S14 shapeMany equals the sequential map of shapeCached"
            (Prop.forAll (Arb.fromGen documentGen) manyEqualsSequential)
        testCase "the generated documents reach the parallel path, the empty and the one-line cases" (fun () ->
            let sample = Gen.sample 600 documentGen |> List.ofSeq
            let glyphs (case: DocumentCase) = case.Lines |> Array.sumBy (fun (struct (text, _)) -> text.Length)
            let parallelDocuments = sample |> List.filter (fun case -> case.Degree <> 1 && glyphs case >= Shape.SequentialBelowGlyphs)
            Expect.isGreaterThan parallelDocuments.Length 30 "documents large enough to be shaped on several threads"
            Expect.isGreaterThan (sample |> List.filter (fun case -> case.Lines.Length = 0) |> List.length) 5 "empty documents"
            Expect.isGreaterThan (sample |> List.filter (fun case -> case.Lines.Length = 1) |> List.length) 5 "one-line documents"
            Expect.isGreaterThan (sample |> List.filter (fun case -> case.Verify) |> List.length) 100 "documents with the verification flag")
        testCase "an empty document gives no results and keeps the cache" (fun () ->
            let font = fonts.Value[0]
            let struct (results, cache) = Shape.shapeMany Shape.AllCores PlanCache.empty font ImmutableArray.Empty ValueNone ImmutableArray.Empty
            Expect.equal results.Length 0 "no results"
            Expect.equal cache.Entries.Length 0 "no plans")
        testCase "a failing line holds its error and leaves the others as they would be alone" (fun () ->
            let font = fonts.Value[0]
            let none = ImmutableArray<Feature>.Empty
            let good = runOf false (struct ([| 0x48u; 0x69u |], Direction.Invalid))
            let empty = runOf false (struct ([||], Direction.Invalid))
            let runs = [| good; empty; good |]
            let struct (alone, _) = sequentialReference PlanCache.empty font none ValueNone runs
            let struct (together, _) = Shape.shapeMany 2 PlanCache.empty font none (ValueSome (ImmutableArray.Create "nothing")) (ImmutableArray.CreateRange runs)
            Expect.isTrue (Array.forall id (Array.map (fun result -> match result with | Ok _ -> true | Error _ -> false) alone)) "shapes alone"
            match together[0], together[1], together[2] with
            | Error (NoShaperSucceeded _), Ok kept, Error (NoShaperSucceeded _) -> Expect.equal kept.Infos.Length 0 "the empty line comes back as it was"
            | _ -> failtest "the unshapeable lines are errors in their slots and the empty line is untouched")
        testCase "a degree of one is the sequential path" (fun () ->
            let font = fonts.Value[0]
            let runs = Array.init 400 (fun index -> runOf true (struct (Array.init 12 (fun at -> 0x41u + uint32 ((index + at) % 26)), Direction.Invalid)))
            let struct (expected, _) = sequentialReference PlanCache.empty font ImmutableArray.Empty ValueNone runs
            let struct (one, _) = Shape.shapeMany 1 PlanCache.empty font ImmutableArray.Empty ValueNone (ImmutableArray.CreateRange runs)
            Expect.isTrue (Seq.forall2 sameResult one expected) "equal")
        testCase "chunk starts cover every item once, in order" (fun () ->
            let costs = Array.init 1000 (fun index -> 1 + (index * 7919) % 53)
            let starts = ParallelFill.chunkStarts (ReadOnlySpan costs) 500
            Expect.equal starts[0] 0 "first chunk starts at the first item"
            Expect.equal starts[starts.Length - 1] costs.Length "the sentinel is the count"
            Expect.isTrue (Seq.forall2 (fun a b -> a < b) (Seq.take (starts.Length - 1) starts) (Seq.skip 1 starts)) "chunks are not empty and ascend")
        testCase "stress: shaping from a cold shared font, 12 threads, 200 fresh faces each" (fun () ->
            for case in coldCases.Value do
                let expected = shapeAllRuns (fontOn case (Face.create (case.Bytes ()) 0u)) case.Runs
                stressShaping case expected)
        testCase "stress: every thread opens its own font on one cold face, 200 fresh faces each" (fun () ->
            for case in coldCases.Value do
                let expected = shapeAllRuns (fontOn case (Face.create (case.Bytes ()) 0u)) case.Runs
                stressFaceCell case expected)
        testCase "stress: font functions on a cold face from 12 threads, 200 fresh faces each" (fun () ->
            for case in coldCases.Value do
                stressFontFunctions case)
    ]
