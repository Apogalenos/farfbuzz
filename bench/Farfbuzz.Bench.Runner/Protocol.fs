module Farfbuzz.Bench.Runner.Protocol

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Runtime
open System.Runtime.Intrinsics

/// <summary>Anchors the assembly that embeds the fonts and texts.</summary>
type Marker = class end

/// <summary>The fewest seconds of measured passes of one pair, as in the protocol of P9.S05.</summary>
[<Literal>]
let MinimumSeconds = 2.0

/// <summary>The milliseconds in a second.</summary>
[<Literal>]
let MillisecondsPerSecond = 1000.0

/// <summary>The FNV-1a 64-bit offset basis, the start of every output hash.</summary>
[<Literal>]
let FnvOffsetBasis = 14695981039346656037UL

/// <summary>The FNV-1a 64-bit prime.</summary>
[<Literal>]
let FnvPrime = 1099511628211UL

/// <summary>The line terminator byte of the corpus texts.</summary>
[<Literal>]
let private Newline = '\n'B

/// <summary>The embedded folder of the fonts.</summary>
[<Literal>]
let private FontFolder = "fonts/"

/// <summary>The embedded folder of the texts.</summary>
[<Literal>]
let private TextFolder = "texts/"

/// <summary>The files of one pair of <c>perf/benchmark-shape.cc</c>, by embedded file name.</summary>
type PairSpec = { FontFile: string; TextFile: string }

/// <summary>A loaded pair: label, bytes and the lines upstream shapes.</summary>
type Pair =
    { Label: string
      Font: byte array
      Text: byte array
      Lines: struct (int * int) array }

/// <summary>One implementation: how to prepare a pair, shape every line once, and shape every line once more hashing the output.</summary>
type Impl =
    { Name: string
      Prepare: Pair -> obj
      Pass: obj -> int
      Hash: obj -> struct (uint64 * int) }

/// <summary>The result of measuring one implementation on one pair.</summary>
type Measurement =
    { Label: string
      Impl: string
      Lines: int
      Glyphs: int
      Passes: int
      MedianMilliseconds: float
      MinimumMilliseconds: float
      BytesPerPass: int64
      Consistent: bool
      Hash: uint64
      WarmUpGlyphs: int
      WarmUpHash: uint64 }

/// <summary>What one pass cost and how many glyphs it shaped.</summary>
type Sample = { Milliseconds: float; Bytes: int64; Glyphs: int }

/// <summary>Returns its argument through a call the compiler cannot inline, so that the call before it that passed a
/// span is not the last call of its function (PITFALLS.md).</summary>
/// <param name="value">The result of the call that passed a span.</param>
/// <returns>The same value.</returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let settle value = value

/// <summary>The ten pairs of <c>perf/benchmark-shape.cc</c> at HarfBuzz 15.1.0, in upstream order.</summary>
let specs =
    [ { FontFile = "Roboto-Regular.ttf"; TextFile = "en-thelittleprince.txt" }
      { FontFile = "Roboto-Regular.ttf"; TextFile = "en-words.txt" }
      { FontFile = "SourceSerifVariable-Roman.ttf"; TextFile = "react-dom.txt" }
      { FontFile = "NotoSansDevanagari-Regular.ttf"; TextFile = "hi-words.txt" }
      { FontFile = "Amiri-Regular.ttf"; TextFile = "fa-thelittleprince.txt" }
      { FontFile = "NotoNastaliqUrdu-Regular.ttf"; TextFile = "fa-thelittleprince.txt" }
      { FontFile = "NotoNastaliqUrdu-Regular.ttf"; TextFile = "fa-words.txt" }
      { FontFile = "Gulzar-Regular.ttf"; TextFile = "fa-thelittleprince.txt" }
      { FontFile = "Gulzar-Regular.ttf"; TextFile = "fa-words.txt" }
      { FontFile = "NotoSansDuployan-Regular.otf"; TextFile = "duployan.txt" } ]

/// <summary>Reads one embedded resource.</summary>
/// <param name="name">The logical name of the resource.</param>
/// <returns>The bytes, or why the resource is missing.</returns>
let private resource (name: string) =
    match typeof<Marker>.Assembly.GetManifestResourceStream name with
    | null -> Error $"resource {name} is not embedded"
    | stream ->
        use stream = stream
        use copy = new MemoryStream()
        stream.CopyTo copy
        Ok (copy.ToArray())

/// <summary>The lines of a text as upstream shapes them: each newline-terminated line, a final line without a terminator skipped.</summary>
/// <param name="text">The UTF-8 text.</param>
/// <param name="start">The first byte of the line being found.</param>
/// <param name="found">The lines found so far, last first.</param>
/// <returns>One (start, byteLength) per line, the newline excluded.</returns>
[<TailCall>]
let rec private linesFrom (text: byte array) (start: int) (found: struct (int * int) list) =
    match Array.IndexOf(text, Newline, start) with
    | -1 -> found |> List.rev |> List.toArray
    | newline -> linesFrom text (newline + 1) (struct (start, newline - start) :: found)

/// <summary>Loads a pair from the embedded resources.</summary>
/// <param name="spec">The pair's files.</param>
/// <returns>The loaded pair, or why it could not be loaded.</returns>
let load (spec: PairSpec) =
    match resource (FontFolder + spec.FontFile), resource (TextFolder + spec.TextFile) with
    | Ok font, Ok text -> Ok { Label = $"{spec.FontFile}/{spec.TextFile}"; Font = font; Text = text; Lines = linesFrom text 0 [] }
    | Error problem, _ -> Error problem
    | _, Error problem -> Error problem

/// <summary>Folds bytes into an FNV-1a 64-bit hash.</summary>
/// <param name="hash">The running hash.</param>
/// <param name="bytes">The bytes.</param>
/// <param name="index">The next byte.</param>
/// <returns>The updated hash.</returns>
[<TailCall>]
let rec foldBytes (hash: uint64) (bytes: byte array) (index: int) =
    match index < bytes.Length with
    | false -> hash
    | true -> foldBytes ((hash ^^^ uint64 bytes[index]) * FnvPrime) bytes (index + 1)

/// <summary>Runs passes until both the minimum count and the minimum time are reached, timing each pass alone.</summary>
/// <param name="impl">The implementation.</param>
/// <param name="state">The prepared pair.</param>
/// <param name="minimum">The fewest passes.</param>
/// <param name="samples">The samples so far, last first.</param>
/// <param name="elapsed">The milliseconds measured so far.</param>
/// <returns>Every sample.</returns>
[<TailCall>]
let rec private collect (impl: Impl) (state: obj) (minimum: int) (samples: Sample list) (elapsed: float) =
    match samples.Length >= minimum && elapsed >= MinimumSeconds * MillisecondsPerSecond with
    | true -> samples
    | false ->
        let before = GC.GetTotalAllocatedBytes true
        let started = Stopwatch.GetTimestamp()
        let glyphs = impl.Pass state
        let milliseconds = Stopwatch.GetElapsedTime(started).TotalMilliseconds
        let after = GC.GetTotalAllocatedBytes true
        collect impl state minimum ({ Milliseconds = milliseconds; Bytes = after - before; Glyphs = glyphs } :: samples) (elapsed + milliseconds)

/// <summary>The median of sorted values: the middle one, or the mean of the two middle ones.</summary>
/// <param name="sorted">The values in ascending order, at least one.</param>
/// <returns>The median.</returns>
let private median (sorted: float array) =
    let upper = sorted.Length / 2
    match sorted.Length % 2 with
    | 0 -> (sorted[upper - 1] + sorted[upper]) / 2.0
    | _ -> sorted[upper]

/// <summary>Measures one implementation on one pair: the first pass of the prepared pair, which shapes and hashes every line, then one
/// warm-up pass, then passes until both the minimum count and two seconds are reached, the median and the minimum in milliseconds, the bytes
/// allocated per pass, and a last hashing pass. The result is consistent only when every pass shaped the glyph count of the first one and the
/// last hashing pass gave the first one's hash, so that a pass that differs from the first on any runtime is never taken for a good run.</summary>
/// <param name="impl">The implementation.</param>
/// <param name="pair">The pair.</param>
/// <param name="passes">The fewest measured passes.</param>
/// <returns>The measurement.</returns>
let measure (impl: Impl) (pair: Pair) (passes: int) =
    let state = impl.Prepare pair
    let struct (firstHash, firstGlyphs) = impl.Hash state
    let warmUpGlyphs = impl.Pass state
    let samples = collect impl state passes [] 0.0
    let struct (lastHash, lastGlyphs) = impl.Hash state
    let times = samples |> List.map _.Milliseconds |> List.toArray |> Array.sort
    let bytes = samples |> List.map (fun sample -> float sample.Bytes) |> List.toArray |> Array.sort
    { Label = pair.Label
      Impl = impl.Name
      Lines = pair.Lines.Length
      Glyphs = lastGlyphs
      Passes = samples.Length
      MedianMilliseconds = median times
      MinimumMilliseconds = times[0]
      BytesPerPass = int64 (median bytes)
      Consistent =
        firstHash = lastHash
        && firstGlyphs = lastGlyphs
        && warmUpGlyphs = lastGlyphs
        && samples |> List.forall (fun sample -> sample.Glyphs = lastGlyphs)
      Hash = lastHash
      WarmUpGlyphs = firstGlyphs
      WarmUpHash = firstHash }

/// <summary>The WebAssembly flavour this build was compiled as.</summary>
/// <returns>True when the project defined <c>WASM_AOT</c>.</returns>
let private wasmAot () =
#if WASM_AOT
    true
#else
    false
#endif

/// <summary>Whether the process has compiled any method with a just-in-time compiler: true on CoreCLR, false on NativeAOT.
/// <c>RuntimeFeature.IsDynamicCodeCompiled</c> cannot tell, because a project that sets <c>PublishAot</c> switches dynamic
/// code off in the configuration of its CoreCLR build too.</summary>
/// <returns>True when the process has a JIT.</returns>
let private jitted () = JitInfo.GetCompiledMethodCount false > 0L

/// <summary>Names the runtime the process runs on in the words of the report's tables.</summary>
/// <returns>CoreCLR, NativeAOT, WASM-interp or WASM-AOT.</returns>
let runtimeLabel () =
    match OperatingSystem.IsBrowser(), wasmAot () with
    | true, true -> "WASM-AOT"
    | true, false -> "WASM-interp"
    | false, _ -> if jitted () then "CoreCLR" else "NativeAOT"

/// <summary>Quotes a string for JSON.</summary>
/// <param name="text">The text.</param>
/// <returns>The quoted text.</returns>
let private quoted (text: string) =
    "\"" + text.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

/// <summary>Formats a number for JSON, invariantly.</summary>
/// <param name="value">The number.</param>
/// <returns>The text.</returns>
let private number (value: float) = value.ToString("R", CultureInfo.InvariantCulture)

/// <summary>A boolean as JSON.</summary>
/// <param name="value">The value.</param>
/// <returns>true or false.</returns>
let private flag (value: bool) =
    match value with
    | true -> "true"
    | false -> "false"

/// <summary>One measurement as a JSON object.</summary>
/// <param name="measurement">The measurement.</param>
/// <returns>The object text.</returns>
let private measurementJson (measurement: Measurement) =
    $"""{{"pair":{quoted measurement.Label},"lines":{measurement.Lines},"glyphsPerPass":{measurement.Glyphs},"passes":{measurement.Passes},"medianMs":{number measurement.MedianMilliseconds},"minMs":{number measurement.MinimumMilliseconds},"bytesPerPass":{measurement.BytesPerPass},"consistent":{flag measurement.Consistent},"hash":"{measurement.Hash:x16}","firstPassGlyphs":{measurement.WarmUpGlyphs},"firstPassHash":"{measurement.WarmUpHash:x16}"}}"""

/// <summary>The JSON document of a run: the runtime labels and one result per pair.</summary>
/// <param name="impl">The implementation name.</param>
/// <param name="results">The measurements.</param>
/// <returns>The document text.</returns>
let toJson (impl: string) (results: Measurement list) =
    let accelerated = flag Vector128.IsHardwareAccelerated
    let architecture = string RuntimeInformation.ProcessArchitecture
    let browser = flag (OperatingSystem.IsBrowser())
    let dynamicCode = flag RuntimeFeature.IsDynamicCodeCompiled
    let items = String.Join(",", results |> List.map measurementJson)
    $"""{{"impl":{quoted impl},"runtime":{quoted (runtimeLabel ())},"framework":{quoted RuntimeInformation.FrameworkDescription},"browser":{browser},"dynamicCode":{dynamicCode},"architecture":{quoted architecture},"vector128":{accelerated},"processors":{Environment.ProcessorCount},"results":[{items}]}}"""
