module Farfbuzz.Bench.BatchShape

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Runtime
open System.Text
open System.Threading.Tasks
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.NativeShaping

/// <summary>A document: a font, its UTF-8 text and the lines of the text.</summary>
type private Document =
    { Name: string
      FontPath: string
      Text: byte array
      Lines: struct (int * int) array }

/// <summary>Builds the run of every line of a text: UTF-8 decoding and segment-property guessing, the work a caller does before shaping.</summary>
[<Struct>]
type private RunBuilder =
    { Text: byte array
      Lines: struct (int * int) array }

    interface IIndexedWorker<GlyphRun> with
        member this.At index =
            let struct (start, length) = this.Lines[index]
            let run = GlyphRun.addUtf8 (ReadOnlySpan(this.Text, start, this.Text.Length - start)) 0 length GlyphRun.empty
            GlyphRun.guessSegmentProperties Language.cLocale run

/// <summary>Builds the runs of the lines of a text on up to a number of threads, in chunks of similar byte counts.</summary>
/// <param name="degree">The most threads at once; one builds on the calling thread.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The runs, in line order.</returns>
let private buildRuns (degree: int) (text: byte array) (lines: struct (int * int) array) =
    let worker = { RunBuilder.Text = text; Lines = lines }
    let costs = Sealed.init lines.Length (fun index -> let struct (_, length) = lines[index] in length)
    let total = ParallelFill.totalCost (costs.AsSpan())
    match degree = 1 || total < int64 Shape.SequentialBelowGlyphs with
    | true -> ParallelFill.sequential lines.Length worker
    | false ->
        let chunkCost = int (max (int64 ParallelFill.MinChunkCost) (total / int64 (degree * ParallelFill.ChunksPerThread)))
        ParallelFill.chunked degree lines.Length (ParallelFill.chunkStarts (costs.AsSpan()) chunkCost) worker

/// <summary>The glyphs of the shaped runs.</summary>
/// <param name="results">The results.</param>
/// <returns>The total glyph count.</returns>
let private glyphsOf (results: ImmutableArray<Result<GlyphRun, ShapeError>>) =
    let mutable glyphs = 0
    for result in results do
        match result with
        | Ok run -> glyphs <- glyphs + run.Infos.Length
        | Error _ -> ()
    glyphs

/// <summary>Shapes every line sequentially with Farfbuzz, the plan cache kept from one pass to the next, the runs built in the loop.</summary>
/// <param name="cache">The cache of the previous pass.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count and the cache to keep.</returns>
let private farfbuzzSequential (cache: PlanCache) (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    let mutable kept = cache
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (shaped, next) = Shape.shapeCached kept font ImmutableArray<Feature>.Empty ValueNone run
        kept <- next
        match shaped with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    struct (glyphs, kept)

/// <summary>Shapes every line with Farfbuzz's document entry on up to a number of threads, the runs built on the same threads.</summary>
/// <param name="degree">The most threads at once.</param>
/// <param name="cache">The cache of the previous pass.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count and the cache to keep.</returns>
let private farfbuzzMany (degree: int) (cache: PlanCache) (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let runs = buildRuns degree text lines
    let struct (results, kept) = Shape.shapeMany degree cache font ImmutableArray<Feature>.Empty ValueNone runs
    struct (glyphsOf results, kept)

/// <summary>The glyphs of a shaped document.</summary>
/// <param name="document">The document.</param>
/// <returns>The glyph count.</returns>
let private glyphsOfDocument (document: ShapedDocument) =
    let mutable glyphs = 0
    for line in document.Lines do
        match line with
        | Arena (_, length) -> glyphs <- glyphs + length
        | Alone detached -> glyphs <- glyphs + detached.LineInfos.Length
        | Refused _ -> ()
    glyphs

/// <summary>Shapes every line with the document entry that decodes the text into one array per document, on up to a number of threads.</summary>
/// <param name="degree">The most threads at once.</param>
/// <param name="cache">The cache of the previous pass.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count and the cache to keep.</returns>
let private farfbuzzLines (degree: int) (cache: PlanCache) (font: Font) (text: ReadOnlyMemory<byte>) (lines: ImmutableArray<struct (int * int)>) =
    let struct (document, kept) = LineShape.utf8 degree cache font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty text lines
    struct (glyphsOfDocument document, kept)

/// <summary>Shapes prebuilt runs with the document entry: the shaping alone.</summary>
/// <param name="degree">The most threads at once.</param>
/// <param name="cache">The cache of the previous pass.</param>
/// <param name="font">The font.</param>
/// <param name="runs">The runs.</param>
/// <returns>The glyph count and the cache to keep.</returns>
let private farfbuzzPrebuilt (degree: int) (cache: PlanCache) (font: Font) (runs: ImmutableArray<GlyphRun>) =
    let struct (results, kept) = Shape.shapeMany degree cache font ImmutableArray<Feature>.Empty ValueNone runs
    struct (glyphsOf results, kept)

/// <summary>Builds, shapes and reduces to its glyph count the line at an index, keeping nothing: the diagnostic that shows what
/// keeping every shaped run costs.</summary>
[<Struct>]
type private ReducingWorker =
    { Text: byte array
      Lines: struct (int * int) array
      Font: Font
      Plans: struct (SegmentProperties * ShapePlan) array }

    interface IIndexedWorker<int> with
        member this.At index =
            let struct (start, length) = this.Lines[index]
            let run = GlyphRun.addUtf8 (ReadOnlySpan(this.Text, start, this.Text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (_, plan) = this.Plans |> Array.find (fun struct (props, _) -> props = run.SegmentProperties)
            match Shape.shapeWithPlan this.Font plan ImmutableArray<Feature>.Empty run with
            | Ok shaped -> shaped.Infos.Length
            | Error _ -> 0

/// <summary>The plans of every distinct segment-properties value of a document's lines.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The plans by segment properties.</returns>
let private plansOf (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let runs = buildRuns 1 text lines
    Seq.toArray runs
    |> Array.map _.SegmentProperties
    |> Array.distinct
    |> Array.choose (fun props ->
        match ShapePlan.create font props ImmutableArray<Feature>.Empty ValueNone with
        | Ok plan -> Some (struct (props, plan))
        | Error _ -> None)

/// <summary>Builds, shapes and reduces every line on up to a number of threads, retaining nothing but the counts.</summary>
/// <param name="degree">The most threads at once.</param>
/// <param name="worker">The worker.</param>
/// <returns>The glyph count.</returns>
let private farfbuzzReduced (degree: int) (worker: ReducingWorker) =
    let costs = Sealed.init worker.Lines.Length (fun index -> let struct (_, length) = worker.Lines[index] in length)
    let total = ParallelFill.totalCost (costs.AsSpan())
    let counts =
        match degree = 1 with
        | true -> ParallelFill.sequential worker.Lines.Length worker
        | false ->
            let chunkCost = int (max (int64 ParallelFill.MinChunkCost) (total / int64 (degree * ParallelFill.ChunksPerThread)))
            ParallelFill.chunked degree worker.Lines.Length (ParallelFill.chunkStarts (costs.AsSpan()) chunkCost) worker
    Seq.sum counts

/// <summary>Shapes every line with native HarfBuzz sequentially, reusing one buffer.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count.</returns>
let private harfBuzzSequential (font: HarfBuzzSharp.Font) (text: byte array) (lines: struct (int * int) array) =
    use buffer = new HarfBuzzSharp.Buffer()
    shapeRange font buffer text lines lines.Length 0 0

/// <summary>Shapes every line with native HarfBuzz on up to a number of threads: the same chunks as Farfbuzz, one buffer per worker
/// thread, a font shared and immutable.</summary>
/// <param name="degree">The most threads at once.</param>
/// <param name="starts">The chunk starts.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count.</returns>
let private harfBuzzParallel (degree: int) (starts: int array) (font: HarfBuzzSharp.Font) (text: byte array) (lines: struct (int * int) array) =
    let counts = Array.zeroCreate<int> (starts.Length - 1)
    let options = ParallelOptions(MaxDegreeOfParallelism = degree)
    Parallel.For(
        0,
        starts.Length - 1,
        options,
        (fun () -> new HarfBuzzSharp.Buffer()),
        (fun chunk (_: ParallelLoopState) (buffer: HarfBuzzSharp.Buffer) ->
            counts[chunk] <- shapeRange font buffer text lines starts[chunk + 1] starts[chunk] 0
            buffer),
        (fun (buffer: HarfBuzzSharp.Buffer) -> buffer.Dispose()))
    |> ignore
    Array.sum counts

/// <summary>The chunk starts both engines use for a document at a degree of parallelism: Farfbuzz's own rule over the line lengths.</summary>
/// <param name="degree">The threads.</param>
/// <param name="lines">The lines.</param>
/// <returns>The chunk starts.</returns>
let private chunksFor (degree: int) (lines: struct (int * int) array) =
    let costs = Sealed.init lines.Length (fun index -> let struct (_, length) = lines[index] in length)
    let total = ParallelFill.totalCost (costs.AsSpan())
    let chunkCost = int (max (int64 ParallelFill.MinChunkCost) (total / int64 (degree * ParallelFill.ChunksPerThread)))
    ParallelFill.chunkStarts (costs.AsSpan()) chunkCost |> TailCalls.settle

/// <summary>What one pass of an engine took.</summary>
[<Struct>]
type private Pass =
    { Milliseconds: float
      Glyphs: int
      Bytes: int64
      Gen0: int
      Gen1: int
      Gen2: int
      PauseMilliseconds: float }

/// <summary>Runs one pass, measuring wall time, managed allocation on every thread, collections and pause time.</summary>
/// <param name="work">The pass, returning its glyph count.</param>
/// <returns>The measurements.</returns>
let private measure (work: unit -> int) =
    let bytes = GC.GetTotalAllocatedBytes true
    let gen0 = GC.CollectionCount 0
    let gen1 = GC.CollectionCount 1
    let gen2 = GC.CollectionCount 2
    let pause = GC.GetTotalPauseDuration()
    let started = Stopwatch.GetTimestamp()
    let glyphs = work ()
    let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds
    { Milliseconds = elapsed
      Glyphs = glyphs
      Bytes = GC.GetTotalAllocatedBytes true - bytes
      Gen0 = GC.CollectionCount 0 - gen0
      Gen1 = GC.CollectionCount 1 - gen1
      Gen2 = GC.CollectionCount 2 - gen2
      PauseMilliseconds = (GC.GetTotalPauseDuration() - pause).TotalMilliseconds }

/// <summary>The load average of the machine, read from the system.</summary>
/// <returns>The text the system prints.</returns>
let private loadAverage () =
    let start = ProcessStartInfo("sysctl", "-n vm.loadavg", RedirectStandardOutput = true)
    match Process.Start start with
    | null -> String.Empty
    | proc ->
        use proc = proc
        proc.StandardOutput.ReadToEnd().Trim()

/// <summary>The median of a list of numbers.</summary>
/// <param name="values">The values.</param>
/// <returns>The median.</returns>
let private median (values: float array) =
    let sorted = Array.sort values
    sorted[sorted.Length / 2]

/// <summary>The line splitting of two texts alternated, line by line, into one text.</summary>
/// <param name="first">The first text.</param>
/// <param name="second">The second text.</param>
/// <returns>The merged text.</returns>
let private interleave (first: byte array) (second: byte array) =
    let a = Corpus.lines first
    let b = Corpus.lines second
    use output = new MemoryStream()
    for index in 0 .. (min a.Length b.Length) - 1 do
        let struct (aStart, aLength) = a[index]
        let struct (bStart, bLength) = b[index]
        output.Write(first, aStart, aLength)
        output.WriteByte '\n'B
        output.Write(second, bStart, bLength)
        output.WriteByte '\n'B
    output.ToArray()

/// <summary>The documents of one factor: the corpus texts, repeated that many times.</summary>
/// <param name="filter">A substring the document names must contain, or empty for all.</param>
/// <returns>The base documents.</returns>
let private baseDocuments (filter: string) =
    let roboto, prince = Corpus.load "Roboto/en-thelittleprince"
    let _, words = Corpus.load "Roboto/en-words"
    let devanagari, hindi = Corpus.load "NotoSansDevanagari/hi-words"
    let mixed = interleave hindi prince
    let strippedRoboto = LayoutShape.stripFont roboto
    [| { Name = "Little Prince, Roboto with layout"; FontPath = roboto; Text = prince; Lines = Corpus.lines prince }
       { Name = "word list, Roboto with layout"; FontPath = roboto; Text = words; Lines = Corpus.lines words }
       { Name = "Little Prince, Roboto stripped"; FontPath = strippedRoboto; Text = prince; Lines = Corpus.lines prince }
       { Name = "word list, Roboto stripped"; FontPath = strippedRoboto; Text = words; Lines = Corpus.lines words }
       { Name = "Hindi words, Noto Sans Devanagari"; FontPath = devanagari; Text = hindi; Lines = Corpus.lines hindi }
       { Name = "mixed: Hindi words and Little Prince lines, Noto Sans Devanagari"; FontPath = devanagari; Text = mixed; Lines = Corpus.lines mixed } |]
    |> Array.filter (fun document -> filter.Length = 0 || document.Name.Contains(filter, StringComparison.OrdinalIgnoreCase))

/// <summary>A document's lines repeated a number of times.</summary>
/// <param name="factor">The number of repetitions.</param>
/// <param name="lines">The lines.</param>
/// <returns>The repeated lines.</returns>
let private repeated (factor: int) (lines: struct (int * int) array) =
    Array.init (lines.Length * factor) (fun index -> lines[index % lines.Length])

/// <summary>The thread counts to measure: 2, 4, 8 and every logical core, unless the environment names others.</summary>
let private threadCounts =
    match Environment.GetEnvironmentVariable "BATCH_SHAPE_THREADS" with
    | null -> [ 2; 4; 8; Environment.ProcessorCount ] |> List.distinct |> List.filter (fun count -> count <= Environment.ProcessorCount)
    | text when text.Length = 0 -> [ 2; 4; 8; Environment.ProcessorCount ] |> List.distinct
    | text -> text.Split(',') |> Array.map int |> List.ofArray

/// <summary>Reads an environment variable, empty when unset.</summary>
/// <param name="name">The variable.</param>
/// <returns>The value.</returns>
let private env (name: string) =
    match Environment.GetEnvironmentVariable name with
    | null -> String.Empty
    | value -> value

/// <summary>The time one document may take over all its rounds, a guide for how many rounds to run.</summary>
[<Literal>]
let private RoundBudgetMilliseconds = 40000.0

/// <summary>The engines of one table: a label, a thread count, and a pass.</summary>
type private Engine =
    { Label: string
      Engine: string
      Threads: int
      Pass: unit -> int }

/// <summary>Measures one document at one factor: every engine in rounds, alternated within the round, and prints its tables.</summary>
/// <param name="document">The document.</param>
/// <param name="factor">The repetitions of the text.</param>
let private runDocument (document: Document) (factor: int) =
    let lines = repeated factor document.Lines
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes document.FontPath)) 0u)
    use face = new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile document.FontPath, 0)
    use hbFont = new HarfBuzzSharp.Font(face)
    hbFont.SetFunctionsOpenType()
    face.MakeImmutable()
    let cache = ref PlanCache.empty
    let prebuilt = buildRuns 1 document.Text lines
    let sequentialPass () =
        let struct (glyphs, kept) = farfbuzzSequential cache.Value font document.Text lines
        cache.Value <- kept
        glyphs
    let manyPass (threads: int) () =
        let struct (glyphs, kept) = farfbuzzMany threads cache.Value font document.Text lines
        cache.Value <- kept
        glyphs
    let linesText = ReadOnlyMemory document.Text
    let linesArray = ImmutableArray.CreateRange lines
    let linesPass (threads: int) () =
        let struct (glyphs, kept) = farfbuzzLines threads cache.Value font linesText linesArray
        cache.Value <- kept
        glyphs
    let prebuiltPass (threads: int) () =
        let struct (glyphs, kept) = farfbuzzPrebuilt threads cache.Value font prebuilt
        cache.Value <- kept
        glyphs
    let farfbuzzGlyphs = sequentialPass ()
    let nativeGlyphs = harfBuzzSequential hbFont document.Text lines
    let engines =
        [ yield { Label = "Farfbuzz sequential (shapeCached loop)"; Engine = "F"; Threads = 1; Pass = sequentialPass }
          for threads in threadCounts do
              yield { Label = "Farfbuzz shapeMany"; Engine = "F"; Threads = threads; Pass = manyPass threads }
          yield { Label = "Farfbuzz LineShape.utf8, one array per document"; Engine = "L"; Threads = 1; Pass = linesPass 1 }
          for threads in threadCounts do
              yield { Label = "Farfbuzz LineShape.utf8, one array per document"; Engine = "L"; Threads = threads; Pass = linesPass threads }
          yield { Label = "HarfBuzz sequential (one buffer)"; Engine = "H"; Threads = 1; Pass = fun () -> harfBuzzSequential hbFont document.Text lines }
          for threads in threadCounts do
              let starts = chunksFor threads lines
              yield { Label = "HarfBuzz parallel (one buffer per worker)"; Engine = "H"; Threads = threads; Pass = fun () -> harfBuzzParallel threads starts hbFont document.Text lines }
          if env "BATCH_SHAPE_REDUCED" <> "" then
              let worker = { ReducingWorker.Text = document.Text; Lines = lines; Font = font; Plans = plansOf font document.Text lines }
              for threads in 1 :: threadCounts do
                  yield { Label = "diagnostic: Farfbuzz, build and shape in the worker, keep only the glyph count"; Engine = "D"; Threads = threads; Pass = fun () -> farfbuzzReduced threads worker }
          if env "BATCH_SHAPE_PREBUILT" <> "" then
              for threads in 1 :: threadCounts do
                  yield { Label = "Farfbuzz shapeMany, runs prebuilt"; Engine = "P"; Threads = threads; Pass = prebuiltPass threads } ]
        |> Array.ofList
    engines |> Array.iter (fun engine -> engine.Pass () |> ignore)
    let first = measure engines[0].Pass
    let maximumRounds = match env "BATCH_SHAPE_ROUNDS" with | "" -> 9 | text -> int text
    let affordable = int (RoundBudgetMilliseconds / (first.Milliseconds * float engines.Length / 3.0))
    let rounds = max 3 (min maximumRounds affordable)
    let passes = Array.init engines.Length (fun _ -> ResizeArray<Pass>())
    for _ in 1 .. rounds do
        for index in 0 .. engines.Length - 1 do
            passes[index].Add(measure engines[index].Pass)
    let best (index: int) = passes[index] |> Seq.minBy _.Milliseconds
    let medianOf (index: int) = passes[index] |> Seq.map _.Milliseconds |> Array.ofSeq |> median
    let sequentialOf (engine: string) =
        engines |> Array.findIndex (fun candidate -> candidate.Engine = engine && candidate.Threads = 1)
    let harfbuzzAt (threads: int) =
        engines |> Array.findIndex (fun candidate -> candidate.Engine = "H" && candidate.Threads = threads)
    Console.WriteLine ""
    Console.WriteLine $"#### {document.Name}, {factor} x the text: {lines.Length:N0} lines, {nativeGlyphs:N0} glyphs (Farfbuzz {farfbuzzGlyphs:N0}), {rounds} rounds, load average {loadAverage ()}"
    Console.WriteLine ""
    Console.WriteLine "| engine | threads | best ms per document | median ms per document | million glyphs per second (best) | speed-up over own sequential (best) | Farfbuzz / HarfBuzz time at equal threads (best) |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|"
    for index in 0 .. engines.Length - 1 do
        let engine = engines[index]
        let ownSequential = sequentialOf engine.Engine
        let ratio =
            match engine.Engine with
            | "F" -> $"{(best index).Milliseconds / (best (harfbuzzAt engine.Threads)).Milliseconds:N2}"
            | "L" -> $"{(best index).Milliseconds / (best (harfbuzzAt engine.Threads)).Milliseconds:N2}"
            | _ -> String.Empty
        let glyphs = match engine.Engine with | "H" -> nativeGlyphs | _ -> farfbuzzGlyphs
        let ms = (best index).Milliseconds
        Console.WriteLine $"| {engine.Label} | {engine.Threads} | {ms:N1} | {medianOf index:N1} | {float glyphs / ms / 1000.0:N2} | {(best ownSequential).Milliseconds / ms:N2} | {ratio} |"
    Console.WriteLine ""
    Console.WriteLine "| Farfbuzz engine | threads | managed bytes per glyph | gen 0 collections per pass | gen 1 | gen 2 | GC pause ms per pass | pause share of the pass, percent |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|"
    for index in 0 .. engines.Length - 1 do
        let engine = engines[index]
        if engine.Engine <> "H" then
            let pass = best index
            let share = pass.PauseMilliseconds * 100.0 / pass.Milliseconds
            Console.WriteLine $"| {engine.Label} | {engine.Threads} | {float pass.Bytes / float pass.Glyphs:N0} | {pass.Gen0} | {pass.Gen1} | {pass.Gen2} | {pass.PauseMilliseconds:N1} | {share:N1} percent |"

/// <summary>Measures where parallel shaping starts to pay: documents of growing line counts cut from a text, shaped sequentially
/// and in parallel with the threshold lifted, so that the chunking and the thread pool are really used.</summary>
let breakEven () =
    NativeShaping.oracle.Force() |> ignore
    let roboto, prince = Corpus.load "Roboto/en-thelittleprince"
    let _, words = Corpus.load "Roboto/en-words"
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes roboto)) 0u)
    Console.WriteLine $"Break-even of shapeMany over the sequential loop, {Environment.ProcessorCount} logical cores, GC server {GCSettings.IsServerGC}, load average {loadAverage ()}"
    Console.WriteLine ""
    Console.WriteLine "| text | lines | glyphs | sequential microseconds | shapeMany microseconds at 2 threads | 4 | 8 | all logical cores | fastest parallel / sequential |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|---:|"
    for name, text in [ "Little Prince", prince; "word list", words ] do
        let all = Corpus.lines text
        for count in [ 1; 2; 3; 5; 8; 13; 20; 30; 50; 100; 150; 200; 300; 500; 1000; 2000 ] do
            if count <= all.Length then
                let lines = Array.sub all 0 count
                let runs = buildRuns 1 text lines
                let glyphs = runs |> Seq.sumBy _.Infos.Length
                let cache =
                    let struct (_, kept) = Shape.shapeMany 1 PlanCache.empty font ImmutableArray.Empty ValueNone runs
                    kept
                let timeIt (degree: int) =
                    let work () =
                        let struct (results, _) = Shape.shapeManyFrom 0 degree cache font ImmutableArray.Empty ValueNone runs
                        results.Length
                    for _ in 1 .. 20 do work () |> ignore
                    let samples = Array.init 60 (fun _ ->
                        let started = Stopwatch.GetTimestamp()
                        work () |> ignore
                        Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0)
                    Array.min samples
                let sequential = timeIt 1
                let parallelTimes = [ 2; 4; 8; Environment.ProcessorCount ] |> List.map timeIt
                Console.WriteLine $"| {name} | {count} | {glyphs} | {sequential:N1} | {parallelTimes[0]:N1} | {parallelTimes[1]:N1} | {parallelTimes[2]:N1} | {parallelTimes[3]:N1} | {(List.min parallelTimes) / sequential:N2} |"

/// <summary>The managed bytes the calling thread allocates running a piece of work.</summary>
/// <param name="work">The work.</param>
/// <returns>The bytes.</returns>
let private allocated (work: unit -> int) : int64 =
    work () |> ignore
    let before = GC.GetAllocatedBytesForCurrentThread()
    work () |> ignore
    GC.GetAllocatedBytesForCurrentThread() - before

/// <summary>The plan cache after shaping the runs once, built where no result outlives the call.</summary>
/// <param name="font">The font.</param>
/// <param name="runs">The runs.</param>
/// <returns>The cache.</returns>
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private warmCache (font: Font) (runs: ImmutableArray<GlyphRun>) =
    let struct (_, cache) = Shape.shapeMany 1 PlanCache.empty font ImmutableArray<Feature>.Empty ValueNone runs
    cache

/// <summary>The managed bytes the results of shaping the runs keep alive: the heap after a full collection with the results held,
/// less the heap before they were made.</summary>
/// <param name="cache">The plan cache.</param>
/// <param name="font">The font.</param>
/// <param name="runs">The runs.</param>
/// <returns>The retained bytes.</returns>
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private retainedBytes (cache: PlanCache) (font: Font) (runs: ImmutableArray<GlyphRun>) =
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
    let before = GC.GetTotalMemory true
    let struct (results, _) = Shape.shapeMany 1 cache font ImmutableArray<Feature>.Empty ValueNone runs
    let after = GC.GetTotalMemory true
    GC.KeepAlive results
    after - before

/// <summary>The managed bytes the results of shaping a document with the document entry keep alive.</summary>
/// <param name="cache">The plan cache.</param>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The retained bytes.</returns>
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private retainedLinesBytes (cache: PlanCache) (font: Font) (text: ReadOnlyMemory<byte>) (lines: ImmutableArray<struct (int * int)>) =
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
    let before = GC.GetTotalMemory true
    let struct (document, _) = LineShape.utf8 1 cache font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty text lines
    let after = GC.GetTotalMemory true
    GC.KeepAlive document
    after - before

/// <summary>Where the allocation of a shaped line goes, per glyph: building the run, the plan key and lookup of the cached entry,
/// shaping with a plan in hand, and what a retained result keeps alive.</summary>
let allocation () =
    Console.WriteLine $"Sizes: GlyphInfo {System.Runtime.CompilerServices.Unsafe.SizeOf<GlyphInfo>()} bytes, GlyphPosition {System.Runtime.CompilerServices.Unsafe.SizeOf<GlyphPosition>()} bytes, GlyphProps {System.Runtime.CompilerServices.Unsafe.SizeOf<GlyphProps>()} bytes"
    Console.WriteLine ""
    Console.WriteLine "| document | glyphs | building the run, B per glyph | plan key and cache lookup, B per glyph | shaping with the plan in hand, B per glyph | Result and the shaped run kept by the caller, B per glyph | LineShape.utf8 over the text, one pass, B per glyph | LineShape.utf8 result kept by the caller, B per glyph | LineShape.utf8 objects kept per line |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|---:|"
    for document in baseDocuments String.Empty do
        let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes document.FontPath)) 0u)
        let runs = buildRuns 1 document.Text document.Lines
        let glyphs = float (runs |> Seq.sumBy _.Infos.Length)
        let plans = plansOf font document.Text document.Lines
        let none = ImmutableArray<Feature>.Empty
        let build =
            allocated (fun () ->
                let worker = { RunBuilder.Text = document.Text; Lines = document.Lines }
                (ParallelFill.sequential document.Lines.Length worker).Length)
        let cached =
            allocated (fun () ->
                let mutable cache = PlanCache.empty
                let mutable total = 0
                for run in runs do
                    let struct (shaped, next) = Shape.shapeCached cache font none ValueNone run
                    cache <- next
                    match shaped with
                    | Ok shaped -> total <- total + shaped.Infos.Length
                    | Error _ -> ()
                total)
        let planned =
            allocated (fun () ->
                let mutable total = 0
                for run in runs do
                    let struct (_, plan) = plans |> Array.find (fun struct (props, _) -> props = run.SegmentProperties)
                    match Shape.shapeWithPlan font plan none run with
                    | Ok shaped -> total <- total + shaped.Infos.Length
                    | Error _ -> ()
                total)
        let cache = warmCache font runs
        let kept = retainedBytes cache font runs
        let linesText = ReadOnlyMemory document.Text
        let linesArray = ImmutableArray.CreateRange document.Lines
        let linesBytes = allocated (fun () -> let struct (shaped, _) = LineShape.utf8 1 cache font none ValueNone Language.cLocale GlyphRun.empty linesText linesArray in glyphsOfDocument shaped)
        let linesKept = retainedLinesBytes cache font linesText linesArray
        Console.WriteLine $"| {document.Name} | {glyphs:N0} | {float build / glyphs:N1} | {float (cached - planned) / glyphs:N1} | {float planned / glyphs:N1} | {float kept / glyphs:N1} | {float linesBytes / glyphs:N1} | {float linesKept / glyphs:N1} | 0 |"

/// <summary>The code points of a line that the font has no glyph for, and its code points.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <param name="line">The line.</param>
/// <returns>The code points and the misses.</returns>
let private cmapMisses (font: Font) (text: byte array) (line: struct (int * int)) =
    let struct (start, length) = line
    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
    let cmap = font.Ot.Cmap.Value
    let misses = run.Infos |> Seq.filter (fun info -> match Cmap.nominalGlyph cmap info.Codepoint with | ValueNone -> true | ValueSome _ -> false) |> Seq.length
    struct (run.Infos.Length, misses)

/// <summary>Where the lines of a document go, grouped by the script their segment properties get: the plan, the code points the
/// font has no glyph for, and the time and bytes of the document entry and of HarfBuzz on each group alone.</summary>
let lineRoutes () =
    NativeShaping.oracle.Force() |> ignore
    for document in baseDocuments (env "BATCH_SHAPE_DOCS") do
        let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes document.FontPath)) 0u)
        use face = new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile document.FontPath, 0)
        use hbFont = new HarfBuzzSharp.Font(face)
        hbFont.SetFunctionsOpenType()
        face.MakeImmutable()
        let scriptOf (line: struct (int * int)) =
            let struct (start, length) = line
            let run = GlyphRun.addUtf8 (ReadOnlySpan(document.Text, start, document.Text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            run.SegmentProperties
        let groups = document.Lines |> Array.groupBy scriptOf
        Console.WriteLine $"#### {document.Name}"
        for props, lines in groups do
            let run = GlyphRun.addUtf8 (ReadOnlySpan(document.Text, 0, document.Text.Length)) 0 0 GlyphRun.empty
            let plan = ShapePlan.create font props ImmutableArray<Feature>.Empty ValueNone
            let facts =
                match plan with
                | Error error -> $"plan error {error}"
                | Ok plan ->
                    match plan.Ot with
                    | ValueNone -> "no ot plan"
                    | ValueSome otPlan ->
                        let probe = { run with SegmentProperties = props }
                        $"shaper {otPlan.Shaper.Kind}, simple plan {SimpleRun.isSimplePlan otPlan probe}, GSUB covered words {plan.GsubCovered.Length}, GPOS covered words {plan.GposCovered.Length}, GSUB lookup-covered entries {plan.GsubLookupCovered.Length}, GPOS lookup-covered entries {plan.GposLookupCovered.Length}"
            let counted = lines |> Array.map (cmapMisses font document.Text)
            let codepoints = counted |> Array.sumBy (fun struct (count, _) -> count)
            let misses = counted |> Array.sumBy (fun struct (_, miss) -> miss)
            let linesImmutable = ImmutableArray.CreateRange lines
            let text = ReadOnlyMemory document.Text
            let mutable cache = PlanCache.empty
            let pass () =
                let struct (glyphs, kept) = farfbuzzLines 1 cache font text linesImmutable
                cache <- kept
                glyphs
            let warmed = Stopwatch.StartNew()
            while warmed.ElapsedMilliseconds < 2000L do
                pass () |> ignore
                harfBuzzSequential hbFont document.Text lines |> ignore
            let farfbuzz = Array.init 9 (fun _ -> measure pass) |> Array.minBy _.Milliseconds
            let nativeBest = Array.init 9 (fun _ -> let started = Stopwatch.GetTimestamp() in harfBuzzSequential hbFont document.Text lines |> ignore; Stopwatch.GetElapsedTime(started).TotalMilliseconds) |> Array.min
            match plan with
            | Error _ -> ()
            | Ok plan ->
                let cmap = font.Ot.Cmap.Value
                let covered = System.Collections.Generic.Dictionary<uint32, int>()
                let total = System.Collections.Generic.Dictionary<uint32, int>()
                for struct (start, length) in lines do
                    let run = GlyphRun.addUtf8 (ReadOnlySpan(document.Text, start, document.Text.Length - start)) 0 length GlyphRun.empty
                    for info in Array.ofSeq run.Infos do
                        let glyph = match Cmap.nominalGlyph cmap info.Codepoint with | ValueSome glyph -> glyph | ValueNone -> 0u
                        let single = [| { info with Codepoint = glyph } |]
                        total[info.Codepoint] <- (match total.TryGetValue info.Codepoint with | true, n -> n + 1 | false, _ -> 1)
                        if Sealed.anyGlyphIn plan.GsubCovered (ReadOnlySpan<GlyphInfo> single) then
                            covered[info.Codepoint] <- (match covered.TryGetValue info.Codepoint with | true, n -> n + 1 | false, _ -> 1)
                let lineHits =
                    lines |> Array.filter (fun struct (start, length) ->
                        let run = GlyphRun.addUtf8 (ReadOnlySpan(document.Text, start, document.Text.Length - start)) 0 length GlyphRun.empty
                        let glyphs = run.Infos |> Seq.map (fun info -> { info with Codepoint = (match Cmap.nominalGlyph cmap info.Codepoint with | ValueSome glyph -> glyph | ValueNone -> 0u) }) |> Array.ofSeq
                        Sealed.anyGlyphIn plan.GsubCovered (ReadOnlySpan<GlyphInfo> glyphs) |> TailCalls.settle)
                let list = covered |> Seq.sortByDescending _.Value |> Seq.truncate 12 |> Seq.map (fun pair -> $"U+{pair.Key:X4} x{pair.Value}") |> String.concat ", "
                Console.WriteLine $"    code points in the GSUB glyph set: {covered.Count} distinct, {Seq.sum covered.Values} occurrences of {Seq.sum total.Values}; lines with a hit {lineHits.Length} of {lines.Length}; most frequent: {list}"
            Console.WriteLine $"script {props.Script}: {lines.Length} lines, {codepoints} code points, {misses} without a glyph in the font; {facts}"
            Console.WriteLine $"    Farfbuzz LineShape.utf8, one thread: {farfbuzz.Milliseconds:N1} ms, {float farfbuzz.Bytes / float farfbuzz.Glyphs:N0} bytes per glyph; HarfBuzz: {nativeBest:N1} ms; ratio {farfbuzz.Milliseconds / nativeBest:N2}"
        let whole = ImmutableArray.CreateRange document.Lines
        let wholeText = ReadOnlyMemory document.Text
        let mutable wholeCache = PlanCache.empty
        let wholePass () =
            let struct (glyphs, kept) = farfbuzzLines 1 wholeCache font wholeText whole
            wholeCache <- kept
            glyphs
        let wholeWarm = Stopwatch.StartNew()
        while wholeWarm.ElapsedMilliseconds < 2000L do
            wholePass () |> ignore
            harfBuzzSequential hbFont document.Text document.Lines |> ignore
        let wholeFarfbuzz = Array.init 9 (fun _ -> measure wholePass) |> Array.minBy _.Milliseconds
        let wholeNative = Array.init 9 (fun _ -> let started = Stopwatch.GetTimestamp() in harfBuzzSequential hbFont document.Text document.Lines |> ignore; Stopwatch.GetElapsedTime(started).TotalMilliseconds) |> Array.min
        Console.WriteLine $"    whole document, {document.Lines.Length} lines in their order: Farfbuzz {wholeFarfbuzz.Milliseconds:N1} ms, {float wholeFarfbuzz.Bytes / float wholeFarfbuzz.Glyphs:N0} bytes per glyph; HarfBuzz {wholeNative:N1} ms; ratio {wholeFarfbuzz.Milliseconds / wholeNative:N2}"

/// <summary>Times Farfbuzz's document entry against native HarfBuzz on whole documents, sequentially and on several threads.</summary>
let run () =
    NativeShaping.oracle.Force() |> ignore
    let server = env "DOTNET_gcServer"
    let concurrent = env "DOTNET_gcConcurrent"
    let gen0 = env "DOTNET_GCgen0size"
    Console.WriteLine $"Machine: {Environment.ProcessorCount} logical cores; server GC {GCSettings.IsServerGC}, latency mode {GCSettings.LatencyMode}, DOTNET_gcServer=[{server}] DOTNET_gcConcurrent=[{concurrent}] DOTNET_GCgen0size=[{gen0}]; load average at start {loadAverage ()}"
    let factors = match env "BATCH_SHAPE_FACTORS" with | "" -> [ 1; 10; 100 ] | text -> text.Split(',') |> Array.map int |> List.ofArray
    for document in baseDocuments (env "BATCH_SHAPE_DOCS") do
        for factor in factors do
            runDocument document factor
    Console.WriteLine ""
    Console.WriteLine $"Load average at end {loadAverage ()}"
