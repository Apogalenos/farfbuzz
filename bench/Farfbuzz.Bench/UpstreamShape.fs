module Farfbuzz.Bench.UpstreamShape

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open System.Text
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Bench.NativeShaping

/// <summary>The FNV-1a 64-bit offset basis, so the output hash is stable across runs and runtimes.</summary>
[<Literal>]
let private FnvOffsetBasis = 14695981039346656037UL

/// <summary>The FNV-1a 64-bit prime.</summary>
[<Literal>]
let private FnvPrime = 1099511628211UL

/// <summary>A loaded pair: the bytes, the line ranges, three ready engines, the glyph count per pass and the output
/// hashes of the Farfbuzz and native engines.</summary>
type PairCase =
    { Pair: UpstreamPairs.Pair
      Text: byte array
      TextPtr: nativeint
      TextHandle: GCHandle
      Lines: ImmutableArray<struct (int * int)>
      GlyphsPerPass: int
      NativeHash: uint64
      FarfbuzzHash: uint64
      NativeFont: nativeint
      NativeBuffer: nativeint
      Shapers: string array
      HbsFont: HarfBuzzSharp.Font
      HbsBuffer: HarfBuzzSharp.Buffer
      FarfbuzzFont: Font }
    override case.ToString() = $"{Path.GetFileName case.Pair.FontPath}/{Path.GetFileName case.Pair.TextPath}"

/// <summary>The name of a pair, as upstream's benchmark spells it.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The name.</returns>
let private pairName (pair: UpstreamPairs.Pair) = $"{Path.GetFileName pair.FontPath}/{Path.GetFileName pair.TextPath}"

/// <summary>Throws naming the pair and the line on the first difference, so a wrong output is never measured.</summary>
/// <param name="pair">The pair.</param>
/// <param name="line">The line index.</param>
/// <param name="detail">The difference.</param>
let private differ (pair: UpstreamPairs.Pair) (line: int) (detail: string) =
    raise (InvalidOperationException($"pair {pairName pair}, line {line}: {detail}"))

/// <summary>Shapes one line through native HarfBuzz, reusing the buffer as benchmark-shape.cc does.</summary>
/// <param name="font">The font handle.</param>
/// <param name="buffer">The buffer handle, cleared first.</param>
/// <param name="shapers">The shaper list.</param>
/// <param name="textPtr">A pointer to the first byte of the text.</param>
/// <param name="totalLength">The text length in bytes.</param>
/// <param name="start">The line's first byte.</param>
/// <param name="length">The line's length in bytes.</param>
let private nativeShapeLine (font: nativeint) (buffer: nativeint) (shapers: string array) (textPtr: nativeint) (totalLength: int) (start: int) (length: int) =
    Native.hb_buffer_clear_contents buffer
    Native.hb_buffer_add_utf8(buffer, IntPtr.Add(textPtr, start), totalLength - start, 0u, length)
    Native.hb_buffer_guess_segment_properties buffer
    Native.hb_shape_full(font, buffer, Unchecked.defaultof<Feature array>, 0u, shapers) |> ignore

/// <summary>Copies the infos and positions out of a native buffer.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <returns>The infos and positions.</returns>
let private nativeShaped (buffer: nativeint) =
    let mutable infoLength = 0u
    let infos = NativeText.copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(buffer, &infoLength)) (int infoLength)
    let mutable positionLength = 0u
    let positions = NativeText.copyFrom<GlyphPosition> (Native.hb_buffer_get_glyph_positions(buffer, &positionLength)) (int positionLength)
    struct (infos, positions)

/// <summary>Shapes one line through HarfBuzzSharp, reusing the buffer.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The buffer.</param>
/// <param name="text">The whole text; the line's bytes are the post-context.</param>
/// <param name="start">The line's first byte.</param>
/// <param name="length">The line's length in bytes.</param>
let private hbsShapeLine (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (text: byte array) (start: int) (length: int) =
    buffer.ClearContents()
    buffer.AddUtf8(ReadOnlySpan(text, start, text.Length - start), 0, length)
    buffer.GuessSegmentProperties()
    font.Shape(buffer)

/// <summary>Shapes one line through Farfbuzz, threading the plan cache.</summary>
/// <param name="cache">The plan cache from the previous line.</param>
/// <param name="font">The font.</param>
/// <param name="text">The whole text; the line's bytes are the post-context.</param>
/// <param name="start">The line's first byte.</param>
/// <param name="length">The line's length in bytes.</param>
/// <returns>The shaped run and the cache to keep.</returns>
let private farfbuzzShapeLine (cache: PlanCache) (font: Font) (text: byte array) (start: int) (length: int) =
    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    Shape.shapeCached cache font ImmutableArray<Feature>.Empty ValueNone run

/// <summary>The text serialisation of glyphs, with no glyph names or extents, the format the comparison reads.</summary>
/// <param name="infos">The glyphs.</param>
/// <param name="positions">The positions.</param>
/// <returns>The text.</returns>
let private serializeGlyphs (infos: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) =
    Serialize.glyphs SerializeFlags.Default (fun _ -> ValueNone) (fun _ -> ValueNone) infos positions |> TailCalls.settle

/// <summary>FNV-1a 64-bit hash over bytes, feeding the running hash.</summary>
/// <param name="hash">The running hash.</param>
/// <param name="bytes">The bytes.</param>
/// <returns>The updated hash.</returns>
let private foldHash (hash: uint64) (bytes: ReadOnlySpan<byte>) =
    let mutable value = hash
    for index in 0 .. bytes.Length - 1 do
        value <- (value ^^^ uint64 bytes[index]) * FnvPrime
    value

/// <summary>FNV-1a 64-bit hash over the UTF-8 bytes of a text, feeding the running hash.</summary>
/// <param name="hash">The running hash.</param>
/// <param name="text">The text.</param>
/// <returns>The updated hash.</returns>
let private foldText (hash: uint64) (text: string) =
    foldHash hash (ReadOnlySpan (Encoding.UTF8.GetBytes text)) |> TailCalls.settle

/// <summary>Builds one case: loads the font and text, builds the three engines, shapes every line once through
/// Farfbuzz and native HarfBuzz comparing their serialised output, hashes both, and warms up HarfBuzzSharp.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The case.</returns>
let private buildCase (pair: UpstreamPairs.Pair) =
    let upstream = RepositoryPaths.upstream ()
    let fontPath = Path.Combine(upstream, pair.FontPath)
    let fontBytes = File.ReadAllBytes fontPath
    let textBytes = File.ReadAllBytes(Path.Combine(upstream, pair.TextPath))
    let lines = UpstreamPairs.lineRanges (ReadOnlySpan textBytes)
    let textHandle = GCHandle.Alloc(textBytes, GCHandleType.Pinned)
    let textPtr = textHandle.AddrOfPinnedObject()

    let blob = Native.hb_blob_create(fontBytes, uint32 fontBytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let nativeFont = Native.hb_font_create(face)
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    Native.hb_ot_font_set_funcs nativeFont
    let nativeBuffer = Native.hb_buffer_create ()
    let shapers = [| "ot"; Unchecked.defaultof<string> |]

    let hbsFont = new HarfBuzzSharp.Font(new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile fontPath, 0))
    hbsFont.SetFunctionsOpenType()
    let hbsBuffer = new HarfBuzzSharp.Buffer()

    let farfbuzzFont = Font.create (Face.create (ReadOnlyMemory fontBytes) 0u)

    let mutable cache = PlanCache.empty
    let mutable nativeHash = FnvOffsetBasis
    let mutable farfbuzzHash = FnvOffsetBasis
    let mutable glyphs = 0
    for index in 0 .. lines.Length - 1 do
        let struct (start, length) = lines[index]
        let struct (shaped, next) = farfbuzzShapeLine cache farfbuzzFont textBytes start length
        cache <- next
        let ours =
            match shaped with
            | Ok shaped -> serializeGlyphs (shaped.Infos.AsSpan()) (shaped.Positions.AsSpan())
            | Error _ -> differ pair index "Farfbuzz shaping failed"
        nativeShapeLine nativeFont nativeBuffer shapers textPtr textBytes.Length start length
        let struct (nativeInfos, nativePositions) = nativeShaped nativeBuffer
        let theirs = serializeGlyphs (ReadOnlySpan nativeInfos) (ReadOnlySpan nativePositions)
        match ours = theirs with
        | true -> ()
        | false -> differ pair index "serialised glyphs differ"
        nativeHash <- foldText nativeHash (theirs + "\n")
        farfbuzzHash <- foldText farfbuzzHash (ours + "\n")
        glyphs <- glyphs + nativeInfos.Length

    for index in 0 .. lines.Length - 1 do
        let struct (start, length) = lines[index]
        hbsShapeLine hbsFont hbsBuffer textBytes start length

    { Pair = pair
      Text = textBytes
      TextPtr = textPtr
      TextHandle = textHandle
      Lines = lines
      GlyphsPerPass = glyphs
      NativeHash = nativeHash
      FarfbuzzHash = farfbuzzHash
      NativeFont = nativeFont
      NativeBuffer = nativeBuffer
      Shapers = shapers
      HbsFont = hbsFont
      HbsBuffer = hbsBuffer
      FarfbuzzFont = farfbuzzFont }

/// <summary>Every case, built once with the Farfbuzz-against-native comparison already run.</summary>
let allCases =
    oracle.Force() |> ignore
    UpstreamPairs.all |> Seq.map buildCase |> Seq.toList

/// <summary>The upstream shape benchmark, Farfbuzz against native HarfBuzz 15.1.0 and HarfBuzzSharp 14.4.0.</summary>
[<MemoryDiagnoser>]
type UpstreamShape() =
    /// <summary>The cases, one per pair, for BenchmarkDotNet.</summary>
    /// <returns>The cases.</returns>
    member _.Cases() = allCases |> Seq.map (fun case -> case :> obj)

    /// <summary>Shapes every line through native HarfBuzz.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The number of glyphs, so the work cannot be elided.</returns>
    [<Benchmark(Baseline = true); ArgumentsSource("Cases")>]
    member _.Native(case: PairCase) =
        let mutable glyphs = 0
        for index in 0 .. case.Lines.Length - 1 do
            let struct (start, length) = case.Lines[index]
            nativeShapeLine case.NativeFont case.NativeBuffer case.Shapers case.TextPtr case.Text.Length start length
            glyphs <- glyphs + int (Native.hb_buffer_get_length case.NativeBuffer)
        glyphs

    /// <summary>Shapes every line through HarfBuzzSharp 14.4.0.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The number of glyphs, so the work cannot be elided.</returns>
    [<Benchmark; ArgumentsSource("Cases")>]
    member _.HarfBuzzSharp(case: PairCase) =
        let mutable glyphs = 0
        for index in 0 .. case.Lines.Length - 1 do
            let struct (start, length) = case.Lines[index]
            hbsShapeLine case.HbsFont case.HbsBuffer case.Text start length
            glyphs <- glyphs + case.HbsBuffer.Length
        glyphs

    /// <summary>Shapes every line through Farfbuzz, reusing the plan across lines as HarfBuzz's per-face cache does.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The number of glyphs, so the work cannot be elided.</returns>
    [<Benchmark; ArgumentsSource("Cases")>]
    member _.Farfbuzz(case: PairCase) =
        let mutable glyphs = 0
        let mutable cache = PlanCache.empty
        for index in 0 .. case.Lines.Length - 1 do
            let struct (start, length) = case.Lines[index]
            let struct (shaped, next) = farfbuzzShapeLine cache case.FarfbuzzFont case.Text start length
            cache <- next
            match shaped with
            | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
            | Error _ -> ()
        glyphs

/// <summary>Prints one row per pair and its output hashes; the cases' build already compares Farfbuzz and native, so
/// the two hashes agree on every row it reaches.</summary>
let tally () =
    oracle.Force() |> ignore
    Console.WriteLine "| Pair | Lines | Bytes | Glyphs/pass | Native hash | Farfbuzz hash |"
    Console.WriteLine "|---|---:|---:|---:|---|---|"
    for case in allCases do
        Console.WriteLine($"| {case} | {case.Lines.Length} | {case.Text.Length} | {case.GlyphsPerPass} | {case.NativeHash:x16} | {case.FarfbuzzHash:x16} |")
