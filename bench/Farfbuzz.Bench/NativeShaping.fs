module Farfbuzz.Bench.NativeShaping

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open BenchmarkDotNet.Attributes
open Farfbuzz.Oracle
open Farfbuzz.Bench.Corpus

/// <summary>Loads the HarfBuzz library chosen by FARFBUZZ_ORACLE once per benchmark process, before any native call.</summary>
let oracle =
    lazy (
        match OracleLibrary.ensureLoaded () with
        | Ok choice -> choice
        | Error reason -> failwith reason)

/// <summary>Shapes one line with a reused buffer, as perf/benchmark-shape.cc does.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The buffer, cleared first.</param>
/// <param name="remaining">The text from the line's start to the end of the text: the rest is post-context.</param>
/// <param name="length">The line's length in bytes.</param>
let shapeLine (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (remaining: ReadOnlySpan<byte>) (length: int) =
    buffer.ClearContents()
    buffer.AddUtf8(remaining, 0, length)
    buffer.GuessSegmentProperties()
    font.Shape(buffer)

/// <summary>Shapes the lines from an index up to an exclusive bound.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <param name="upper">The line after the last one to shape.</param>
/// <param name="index">The next line.</param>
/// <param name="glyphs">The glyphs produced so far.</param>
/// <returns>The total number of glyphs.</returns>
[<TailCall>]
let rec shapeRange (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (text: byte array) (lines: struct (int * int) array) (upper: int) (index: int) (glyphs: int) =
    match index < upper with
    | false -> glyphs
    | true ->
        let struct (start, length) = lines[index]
        shapeLine font buffer (ReadOnlySpan(text, start, text.Length - start)) length
        shapeRange font buffer text lines upper (index + 1) (glyphs + buffer.Length)

/// <summary>Shapes every line of a text.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The total number of glyphs.</returns>
let shapeAll font buffer text (lines: struct (int * int) array) = shapeRange font buffer text lines lines.Length 0 0

/// <summary>Upstream's ten font/text pairs shaped line by line by native HarfBuzz, the time Farfbuzz has to beat.</summary>
[<MemoryDiagnoser>]
type Shaping() =
    let mutable text: byte array = [||]
    let mutable lines: struct (int * int) array = [||]
    let mutable font: HarfBuzzSharp.Font = Unchecked.defaultof<HarfBuzzSharp.Font>
    let mutable buffer: HarfBuzzSharp.Buffer = Unchecked.defaultof<HarfBuzzSharp.Buffer>

    /// <summary>The pair to shape.</summary>
    [<ParamsSource("Pairs")>]
    member val Pair = String.Empty with get, set

    /// <summary>The pairs, for BenchmarkDotNet.</summary>
    member _.Pairs = names

    /// <summary>Loads the font and text, and shapes once to warm up, as upstream does.</summary>
    [<GlobalSetup>]
    member this.Setup() =
        oracle.Force() |> ignore
        let path, bytes = load this.Pair
        text <- bytes
        lines <- Corpus.lines bytes
        font <- new HarfBuzzSharp.Font(new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile path, 0))
        font.SetFunctionsOpenType()
        buffer <- new HarfBuzzSharp.Buffer()
        shapeAll font buffer text lines |> ignore

    /// <summary>Shapes every line of the text.</summary>
    /// <returns>The number of glyphs, so the work cannot be elided.</returns>
    [<Benchmark>]
    member _.ShapeText() = shapeAll font buffer text lines

/// <summary>A whole document's lines shaped by native HarfBuzz sequentially and concurrently, with a shared font and one buffer per worker.</summary>
[<MemoryDiagnoser>]
type DocumentThroughput() =
    let mutable text: byte array = [||]
    let mutable lines: struct (int * int) array = [||]
    let mutable font: HarfBuzzSharp.Font = Unchecked.defaultof<HarfBuzzSharp.Font>

    /// <summary>The document: the largest text of the corpus.</summary>
    [<Params("SourceSerifVariable/react-dom", "Roboto/en-words")>]
    member val Pair = String.Empty with get, set

    /// <summary>Loads the font and text.</summary>
    [<GlobalSetup>]
    member this.Setup() =
        oracle.Force() |> ignore
        let path, bytes = load this.Pair
        text <- bytes
        lines <- Corpus.lines bytes
        font <- new HarfBuzzSharp.Font(new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile path, 0))
        font.SetFunctionsOpenType()

    /// <summary>One buffer, every line in order.</summary>
    /// <returns>The number of glyphs.</returns>
    [<Benchmark(Baseline = true)>]
    member _.Sequential() =
        use buffer = new HarfBuzzSharp.Buffer()
        shapeAll font buffer text lines

    /// <summary>Lines spread over the thread pool in ranges, one buffer per range.</summary>
    /// <returns>The number of glyphs.</returns>
    [<Benchmark>]
    member _.Concurrent() =
        let ranges = Partitioner.Create(0, lines.Length)
        let totals = ConcurrentBag<int>()
        Parallel.ForEach(ranges, fun (first: int, after: int) ->
            use buffer = new HarfBuzzSharp.Buffer()
            totals.Add(shapeRange font buffer text lines after first 0)) |> ignore
        Seq.sum totals
