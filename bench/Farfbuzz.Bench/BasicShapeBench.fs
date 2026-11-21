module Farfbuzz.Bench.BasicShapeBench

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Runtime.Intrinsics
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.Corpus
open Farfbuzz.Bench.NativeShaping

/// <summary>The layout-table tags a no-layout benchmark renames out of a font.</summary>
let private layoutTags =
    [ "GSUB"; "GPOS"; "GDEF"; "kern"; "morx"; "mort"; "kerx"; "trak"; "ankr"; "feat" ]
    |> List.map (fun text -> uint32 (Tag.ofString text))
    |> Set.ofList

/// <summary>Renames the layout-table tags of one sfnt offset-table directory so shaping reads none of them.</summary>
/// <param name="bytes">The font bytes, mutated in place.</param>
let private stripDirectory (bytes: byte array) =
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
    let mutable index = 0
    while index < count do
        let record = 12 + index * 16
        if record + 4 <= bytes.Length && layoutTags.Contains (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4))) then
            bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'
        index <- index + 1

/// <summary>Writes a layout-stripped copy of a font beside the corpus and returns its path.</summary>
/// <param name="fontPath">The original font path.</param>
/// <returns>The stripped font's path.</returns>
let stripFont (fontPath: string) =
    let bytes = File.ReadAllBytes fontPath
    stripDirectory bytes
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "no-layout")
    Directory.CreateDirectory directory |> ignore
    let target = Path.Combine(directory, $"bench-{Path.GetFileName fontPath}#no-layout")
    File.WriteAllBytes (target, bytes)
    target

/// <summary>The Farfbuzz buffer flags selected by the verify switch.</summary>
/// <param name="verify">Non-zero to verify.</param>
/// <returns>The flags.</returns>
let private farfbuzzFlags (verify: int) : BufferFlags =
    match verify with
    | 0 -> BufferFlags.Default
    | _ -> BufferFlags.Verify

/// <summary>The HarfBuzzSharp buffer flags selected by the verify switch.</summary>
/// <param name="verify">Non-zero to verify.</param>
/// <returns>The flags.</returns>
let private hbFlags (verify: int) : HarfBuzzSharp.BufferFlags =
    match verify with
    | 0 -> HarfBuzzSharp.BufferFlags.Default
    | _ -> HarfBuzzSharp.BufferFlags.Verify

/// <summary>Shapes every line of a text with Farfbuzz, building one run per line as hb-shape does.</summary>
/// <param name="font">The font.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count, so the work is not elided.</returns>
let private shapeFarfbuzzText (font: Font) (flags: BufferFlags) (text: byte array) (lines: struct (int * int) array) : int =
    let mutable glyphs = 0
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let run = GlyphRun.withFlags flags run
        match Shape.shapeFull font ImmutableArray<Feature>.Empty ValueNone run with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    glyphs

/// <summary>Shapes every line of a text with Farfbuzz, reusing the plan while its key is unchanged.</summary>
/// <param name="font">The font.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count, so the work is not elided.</returns>
let private shapeFarfbuzzTextReused (font: Font) (flags: BufferFlags) (text: byte array) (lines: struct (int * int) array) : int =
    let mutable glyphs = 0
    let mutable lastKey = ValueNone
    let mutable lastPlan = Unchecked.defaultof<ShapePlan>
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let run = GlyphRun.withFlags flags run
        let key = ShapePlan.key font run.SegmentProperties ReadOnlySpan<Feature>.Empty "ot"
        let plan =
            match lastKey, key with
            | ValueSome prev, ValueSome current when prev = current -> lastPlan
            | _, _ ->
                match ShapePlan.create font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Ok plan ->
                    lastKey <- key
                    lastPlan <- plan
                    plan
                | Error _ -> lastPlan
        match Shape.shapeWithPlan font plan ImmutableArray<Feature>.Empty run with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    glyphs

/// <summary>Shapes every line of a text with Farfbuzz, threading a plan cache so each distinct plan is built once.</summary>
/// <param name="font">The font.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count, so the work is not elided.</returns>
let private shapeFarfbuzzTextCached (font: Font) (flags: BufferFlags) (text: byte array) (lines: struct (int * int) array) : int =
    let mutable glyphs = 0
    let mutable cache = PlanCache.empty
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let run = GlyphRun.withFlags flags run
        let struct (shaped, next) = Shape.shapeCached cache font ImmutableArray<Feature>.Empty ValueNone run
        cache <- next
        match shaped with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    glyphs

/// <summary>Shapes every line of a text with Farfbuzz through a plan cache that the caller keeps from one text to the
/// next, as a caller that shapes many texts with one font does and as HarfBuzz's per-face cache lets the native job.</summary>
/// <param name="cache">The plan cache from the previous text.</param>
/// <param name="font">The font.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count and the cache to keep.</returns>
let private shapeFarfbuzzTextWarm (cache: PlanCache) (font: Font) (flags: BufferFlags) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    let mutable kept = cache
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let run = GlyphRun.withFlags flags run
        let struct (shaped, next) = Shape.shapeCached kept font ImmutableArray<Feature>.Empty ValueNone run
        kept <- next
        match shaped with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    struct (glyphs, kept)

/// <summary>Shapes every line of a text with native HarfBuzz, reusing one buffer as benchmark-shape.cc does.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count, so the work is not elided.</returns>
let private shapeHarfBuzzText (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (flags: HarfBuzzSharp.BufferFlags) (text: byte array) (lines: struct (int * int) array) : int =
    let mutable glyphs = 0
    for line in lines do
        let struct (start, length) = line
        buffer.ClearContents()
        buffer.Flags <- flags
        buffer.AddUtf8(ReadOnlySpan(text, start, text.Length - start), 0, length)
        buffer.GuessSegmentProperties()
        font.Shape(buffer)
        glyphs <- glyphs + buffer.Length
    glyphs

/// <summary>Shapes the Roboto text pairs on their layout-stripped font with Farfbuzz against native HarfBuzz,
/// with and without <c>hb_shape_full</c>'s verify flag.</summary>
[<MemoryDiagnoser>]
type BasicShape() =
    let mutable font: Font = Unchecked.defaultof<Font>
    let mutable text: byte array = [||]
    let mutable lines: struct (int * int) array = [||]
    let mutable hbFont: HarfBuzzSharp.Font = Unchecked.defaultof<HarfBuzzSharp.Font>
    let mutable hbBuffer: HarfBuzzSharp.Buffer = Unchecked.defaultof<HarfBuzzSharp.Buffer>
    let mutable warmCache = PlanCache.empty

    /// <summary>The font and text pair to shape.</summary>
    [<Params("Roboto/en-thelittleprince", "Roboto/en-words")>]
    member val Pair = String.Empty with get, set

    /// <summary>Non-zero to shape with the verify flag set.</summary>
    [<Params(0, 1)>]
    member val Verify = 0 with get, set

    /// <summary>Strips the font, loads both engines and warms them once, as benchmark-shape.cc does.</summary>
    [<GlobalSetup>]
    member this.Setup() =
        NativeShaping.oracle.Force() |> ignore
        let originalPath, bytes = Corpus.load this.Pair
        let strippedPath = stripFont originalPath
        text <- bytes
        lines <- Corpus.lines bytes
        font <- Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes strippedPath)) 0u)
        hbFont <- new HarfBuzzSharp.Font(new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile strippedPath, 0))
        hbFont.SetFunctionsOpenType()
        hbBuffer <- new HarfBuzzSharp.Buffer()
        shapeFarfbuzzText font (farfbuzzFlags this.Verify) text lines |> ignore
        let struct (_, warmed) = shapeFarfbuzzTextWarm PlanCache.empty font (farfbuzzFlags this.Verify) text lines
        warmCache <- warmed
        shapeHarfBuzzText hbFont hbBuffer (hbFlags this.Verify) text lines |> ignore

    /// <summary>Shapes every line with Farfbuzz.</summary>
    /// <returns>The total glyph count.</returns>
    [<Benchmark>]
    member this.FarfbuzzShape() = shapeFarfbuzzText font (farfbuzzFlags this.Verify) text lines

    /// <summary>Shapes every line with Farfbuzz, reusing the plan while its key is unchanged.</summary>
    /// <returns>The total glyph count.</returns>
    [<Benchmark>]
    member this.FarfbuzzShapeReuse() = shapeFarfbuzzTextReused font (farfbuzzFlags this.Verify) text lines

    /// <summary>Shapes every line with Farfbuzz, threading a plan cache as hb-shape's per-face cache does.</summary>
    /// <returns>The total glyph count.</returns>
    [<Benchmark>]
    member this.FarfbuzzShapeCached() = shapeFarfbuzzTextCached font (farfbuzzFlags this.Verify) text lines

    /// <summary>Shapes every line with Farfbuzz, keeping the plan cache from one operation to the next.</summary>
    /// <returns>The total glyph count.</returns>
    [<Benchmark>]
    member this.FarfbuzzShapeWarmCache() =
        let struct (glyphs, kept) = shapeFarfbuzzTextWarm warmCache font (farfbuzzFlags this.Verify) text lines
        warmCache <- kept
        glyphs

    /// <summary>Shapes every line with native HarfBuzz.</summary>
    /// <returns>The total glyph count.</returns>
    [<Benchmark(Baseline = true)>]
    member this.HarfBuzzShape() = shapeHarfBuzzText hbFont hbBuffer (hbFlags this.Verify) text lines

/// <summary>The broadcast boundary of the "all code points below 0x80" scans.</summary>
let private asciiBoundary = Vector128.Create(0x80u)

/// <summary>The scalar twin: true when every code point is below 0x80, as <c>info[i].codepoint &lt; 0x80</c>.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>True when the whole span is ASCII.</returns>
let scalarAllAscii (codepoints: ReadOnlySpan<uint32>) : bool =
    let mutable index = 0
    let mutable ascii = true
    while ascii && index < codepoints.Length do
        if codepoints[index] >= 0x80u then ascii <- false
        index <- index + 1
    ascii

/// <summary>The Vector128 candidate: true when every code point is below 0x80.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>True when the whole span is ASCII.</returns>
let vectorAllAscii (codepoints: ReadOnlySpan<uint32>) : bool =
    let mutable index = 0
    let mutable high = 0u
    while index + 4 <= codepoints.Length do
        let chunk = Vector128.Create<uint32>(codepoints.Slice(index, 4))
        high <- high ||| (Vector128.GreaterThanOrEqual(chunk, asciiBoundary)).ExtractMostSignificantBits()
        index <- index + 4
    let mutable tailHigh = false
    while index < codepoints.Length do
        if codepoints[index] >= 0x80u then tailHigh <- true
        index <- index + 1
    high = 0u && not tailHigh

/// <summary>The two Vector128 candidates of the phase-4 design: a scalar loop against a Vector128 scan that
/// answers "every code point in the run is below 0x80".</summary>
[<MemoryDiagnoser>]
type AsciiScan() =
    let mutable ascii: uint32 array = [||]
    let mutable mixed: uint32 array = [||]

    /// <summary>Builds the inputs and checks the scalar and vector answers agree.</summary>
    [<GlobalSetup>]
    member _.Setup() =
        ascii <- Array.init 1000 (fun index -> uint32 (index % 128))
        mixed <- Array.init 1000 (fun index -> if index = 500 then 0x1F600u else uint32 (index % 128))
        if scalarAllAscii (ReadOnlySpan ascii) <> vectorAllAscii (ReadOnlySpan ascii) then failwith "ascii scan disagrees"
        if scalarAllAscii (ReadOnlySpan mixed) <> vectorAllAscii (ReadOnlySpan mixed) then failwith "mixed scan disagrees"

    /// <summary>The scalar loop over an all-ASCII run.</summary>
    /// <returns>True.</returns>
    [<Benchmark(Baseline = true)>]
    member _.ScalarAllAscii() = scalarAllAscii (ReadOnlySpan ascii) |> TailCalls.settle

    /// <summary>The Vector128 scan over an all-ASCII run.</summary>
    /// <returns>True.</returns>
    [<Benchmark>]
    member _.Vector128AllAscii() = vectorAllAscii (ReadOnlySpan ascii) |> TailCalls.settle
