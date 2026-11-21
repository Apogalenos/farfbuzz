module Farfbuzz.Diff.HarfBuzzSharpShaper

open System
open Farfbuzz.Oracle
open System.Collections.Concurrent
open System.IO
open System.Runtime
open System.Text
open Farfbuzz
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness
open Farfbuzz.Harness.Shaper
open Farfbuzz.Oracle.NativeText

/// <summary>HarfBuzzSharp's blob, under a name that does not clash with Farfbuzz's types.</summary>
type HbBlob = HarfBuzzSharp.Blob
/// <summary>HarfBuzzSharp's face.</summary>
type HbFace = HarfBuzzSharp.Face
/// <summary>HarfBuzzSharp's font.</summary>
type HbFont = HarfBuzzSharp.Font
/// <summary>HarfBuzzSharp's buffer.</summary>
type HbBuffer = HarfBuzzSharp.Buffer

/// <summary>A string's UTF-8 bytes, or null for an absent option, as hb-shape passes it to the *_from_string functions.</summary>
/// <param name="value">The option value.</param>
/// <returns>The bytes, or null.</returns>
let utf8 (value: string voption) : byte array =
    match value with
    | ValueSome text -> Encoding.UTF8.GetBytes text
    | ValueNone -> Unchecked.defaultof<byte array>

/// <summary>The byte length to pass with <see cref="utf8"/>: -1 for null, which HarfBuzz treats as invalid.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The length.</returns>
let lengthOf (bytes: byte array) = if obj.ReferenceEquals(bytes, null) then -1 else bytes.Length

/// <summary>Splits a list option at commas and spaces, as hb-shape's <c>strpbrk(p, ", ")</c> loops do.</summary>
/// <param name="text">The argument.</param>
/// <returns>The items, empty ones included as hb-shape tries them.</returns>
let splitList (text: string) = text.Split([| ','; ' ' |]) |> Array.toList

/// <summary>Parses --features as hb-shape does: matching outer quotes stripped, invalid items dropped.</summary>
/// <param name="raw">The argument.</param>
/// <returns>The features.</returns>
let features (raw: string voption) : Feature array =
    match raw with
    | ValueNone -> [||]
    | ValueSome text ->
        let unquoted =
            match text.Length >= 1 && text[0] = text[text.Length - 1] && (text[0] = '"' || text[0] = '\'') with
            | true -> text.Substring(1, max 0 (text.Length - 2))
            | false -> text
        match unquoted.Length with
        | 0 -> [||]
        | _ -> splitList unquoted |> List.choose (fun item -> featureFromString item |> ValueOption.toOption) |> List.toArray

/// <summary>Parses --variations as hb-shape does: invalid items dropped.</summary>
/// <param name="raw">The argument.</param>
/// <returns>The variations.</returns>
let variations (raw: string voption) : Variation array =
    match raw with
    | ValueNone -> [||]
    | ValueSome text when text.Length = 0 -> [||]
    | ValueSome text -> splitList text |> List.choose (fun item -> variationFromString item |> ValueOption.toOption) |> List.toArray

/// <summary>What a second look at a suspicious result says.</summary>
type Verdict =
    | Deterministic  // every pass gives the same glyphs: the font really produces them
    | Transient      // a second pass on the same face differs: the buffer or the shaping call was disturbed
    | DamagedFace    // the cached face repeats the glyphs and a fresh face does not: the face or its blob is damaged

/// <summary>What a look at an answer says once the tests are over.</summary>
type Late =
    | Overwritten   // the array the caller holds no longer holds what it held: memory was written behind its back
    | Unrepeatable  // the array is as it was, and shaping the request again gives other glyphs: the answer was wrong when given

/// <summary>An answer the adapter gave, kept so that it can be looked at again when the run ends.</summary>
type Handed =
    { Request: ShapeRequest
      Live: GlyphInfo array  // the array the caller holds
      Copy: GlyphInfo array  // what it held when it was handed out
      Ordinal: int           // 1 for the first answer of the process
      Thread: int
      At: TimeSpan }         // since the process started

/// <summary>
/// Catches the failure seen once on 2026-11-10 (glyph numbers no font could hold, from a run that passes every
/// other time) at the moment it happens, and writes down what tells its causes apart.
/// </summary>
[<RequireQualifiedAccess>]
module Tripwire =
    /// <summary>The reports written in this process: the test run fails when it is not empty.</summary>
    let reports = ConcurrentQueue<string>()

    /// <summary>Every answer handed out in this process, for the look after the tests.</summary>
    let handed = ConcurrentQueue<Handed>()

    /// <summary>At most this many late reports are written in one run; the count of answers that differ is still exact.</summary>
    [<Literal>]
    let MaxLateReports = 20

    let private started = Diagnostics.Process.GetCurrentProcess().StartTime
    let mutable private issued = 0

    /// <summary>Where reports go; <c>artifacts</c> is never committed.</summary>
    /// <returns>The directory.</returns>
    let directory () = Path.Combine(RepositoryPaths.root (), "artifacts", "flaky")

    /// <summary>Whether a result holds a glyph the face does not have, or a cluster outside the text.</summary>
    /// <param name="glyphCount">The number of glyphs in the face.</param>
    /// <param name="textLength">The number of code points shaped.</param>
    /// <param name="infos">The shaped glyphs.</param>
    /// <returns>True when the result cannot be taken at face value.</returns>
    let suspicious (glyphCount: uint32) (textLength: int) (infos: GlyphInfo array) =
        infos |> Array.exists (fun info -> info.Codepoint >= glyphCount || info.Cluster >= uint32 (max 1 textLength))

    /// <summary>Compares the first pass with a second pass on the same face and a pass on a fresh face.</summary>
    /// <param name="first">The glyphs under suspicion.</param>
    /// <param name="again">The same request on the same face.</param>
    /// <param name="fresh">The same request on a face loaded again from the file.</param>
    /// <returns>The verdict.</returns>
    let verdict (first: GlyphInfo array) (again: GlyphInfo array) (fresh: GlyphInfo array) =
        match first = again, first = fresh with
        | true, true -> Deterministic
        | false, _ -> Transient
        | true, false -> DamagedFace

    /// <summary>Spells glyphs as <c>glyph=cluster</c>, the first 64 of them.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <returns>The spelling.</returns>
    let spell (infos: GlyphInfo array) =
        let shown = infos |> Seq.truncate 64 |> Seq.map (fun info -> $"{info.Codepoint}={info.Cluster}") |> String.concat "|"
        $"{infos.Length} glyphs [{shown}]"

    /// <summary>Writes a report and remembers it.</summary>
    /// <param name="sink">Where the report's path is remembered.</param>
    /// <param name="folder">The directory to write into.</param>
    /// <param name="request">The request that was shaped.</param>
    /// <param name="glyphCount">The number of glyphs in the face.</param>
    /// <param name="outcome">The verdict.</param>
    /// <param name="first">The glyphs under suspicion.</param>
    /// <param name="again">The second pass on the same face.</param>
    /// <param name="fresh">The pass on a fresh face.</param>
    /// <returns>The path of the report.</returns>
    let write (sink: ConcurrentQueue<string>) (folder: string) (request: ShapeRequest) (glyphCount: uint32) (outcome: Verdict) (first: GlyphInfo array) (again: GlyphInfo array) (fresh: GlyphInfo array) =
        Directory.CreateDirectory folder |> ignore
        let stamp = DateTime.UtcNow.ToString "yyyyMMdd-HHmmss-fffffff"
        let path = Path.Combine(folder, $"adapter-{stamp}-thread{Environment.CurrentManagedThreadId}.txt")
        let text = request.Text |> Array.map (fun codepoint -> $"U+{codepoint:X4}") |> String.concat ","
        let meaning =
            match outcome with
            | Deterministic -> "every pass agrees: the font really produces these glyphs"
            | Transient -> "a second pass on the same face differs: the buffer or the shaping call was disturbed, the face is sound"
            | DamagedFace -> "the cached face repeats the glyphs and a fresh face does not: the cached face or its blob is damaged"
        let lines =
            [ $"HarfBuzz adapter tripwire, {DateTime.UtcNow:O}"
              $"verdict: %A{outcome} ({meaning})"
              $"font: {request.FontPath} (face {request.FaceIndex}, {glyphCount} glyphs)"
              $"text: {text}"
              $"features: %A{request.Features}; direction: %A{request.Direction}; script: %A{request.Script}; language: {request.Language}"
              $"shaper: %A{request.Shaper}; font funcs: %A{request.FontFuncs}; cluster level: {request.ClusterLevel}; flags: %A{request.Flags}"
              $"first pass:  {spell first}"
              $"second pass: {spell again}"
              $"fresh face:  {spell fresh}"
              $"thread: {Environment.CurrentManagedThreadId}; processors: {Environment.ProcessorCount}; server GC: {GCSettings.IsServerGC}; latency: %A{GCSettings.LatencyMode}"
              $"collections: gen0 {GC.CollectionCount 0}, gen1 {GC.CollectionCount 1}, gen2 {GC.CollectionCount 2}; managed bytes: {GC.GetTotalMemory false}"
              $"process: {Environment.ProcessId}, up {(DateTime.Now - Diagnostics.Process.GetCurrentProcess().StartTime).TotalSeconds:F1} s"
              $"oracle: {Native.version ()}" ]
        File.WriteAllLines(path, lines)
        sink.Enqueue path
        path

    /// <summary>Keeps an answer and a copy of what it holds at this moment.</summary>
    /// <param name="queue">Where the answer is kept.</param>
    /// <param name="request">The request that was shaped.</param>
    /// <param name="infos">The array being handed to the caller.</param>
    let remember (queue: ConcurrentQueue<Handed>) (request: ShapeRequest) (infos: GlyphInfo array) =
        queue.Enqueue
            { Request = request
              Live = infos
              Copy = Array.copy infos
              Ordinal = Threading.Interlocked.Increment &issued
              Thread = Environment.CurrentManagedThreadId
              At = DateTime.Now - started }

    /// <summary>What the look after the tests says about one answer.</summary>
    /// <param name="answer">The answer.</param>
    /// <param name="replay">The same request shaped again, on a face loaded afresh.</param>
    /// <returns>The finding, or none when the answer stands.</returns>
    let late (answer: Handed) (replay: GlyphInfo array) : Late voption =
        match answer.Live = answer.Copy, answer.Copy = replay with
        | false, _ -> ValueSome Overwritten
        | true, false -> ValueSome Unrepeatable
        | true, true -> ValueNone

    /// <summary>Writes a report on an answer that changed or cannot be repeated, and remembers it.</summary>
    /// <param name="sink">Where the report's path is remembered.</param>
    /// <param name="folder">The directory to write into.</param>
    /// <param name="answer">The answer.</param>
    /// <param name="outcome">The finding.</param>
    /// <param name="replay">The same request shaped again.</param>
    /// <returns>The path of the report.</returns>
    let writeLate (sink: ConcurrentQueue<string>) (folder: string) (answer: Handed) (outcome: Late) (replay: GlyphInfo array) =
        Directory.CreateDirectory folder |> ignore
        let stamp = DateTime.UtcNow.ToString "yyyyMMdd-HHmmss-fffffff"
        let path = Path.Combine(folder, $"adapter-late-{stamp}-answer{answer.Ordinal}.txt")
        let request = answer.Request
        let text = request.Text |> Array.map (fun codepoint -> $"U+{codepoint:X4}") |> String.concat ","
        let meaning =
            match outcome with
            | Overwritten -> "the array the caller holds changed after it was handed out: something wrote into managed memory"
            | Unrepeatable -> "the array is as it was handed out, and shaping again gives other glyphs: the answer was wrong when it was given"
        let lines =
            [ $"HarfBuzz adapter, look after the tests, {DateTime.UtcNow:O}"
              $"finding: %A{outcome} ({meaning})"
              $"answer {answer.Ordinal} of this process, given {answer.At.TotalSeconds:F3} s after it started, on thread {answer.Thread}"
              $"font: {request.FontPath} (face {request.FaceIndex})"
              $"text: {text}"
              $"features: %A{request.Features}; direction: %A{request.Direction}; script: %A{request.Script}; language: {request.Language}"
              $"shaper: %A{request.Shaper}; font funcs: %A{request.FontFuncs}; cluster level: {request.ClusterLevel}; flags: %A{request.Flags}"
              $"handed out: {spell answer.Copy}"
              $"held now:   {spell answer.Live}"
              $"shaped now: {spell replay}"
              $"processors: {Environment.ProcessorCount}; server GC: {GCSettings.IsServerGC}; latency: %A{GCSettings.LatencyMode}"
              $"collections: gen0 {GC.CollectionCount 0}, gen1 {GC.CollectionCount 1}, gen2 {GC.CollectionCount 2}; managed bytes: {GC.GetTotalMemory false}"
              $"process: {Environment.ProcessId}; oracle: {Native.version ()}" ]
        File.WriteAllLines(path, lines)
        sink.Enqueue path
        path

    /// <summary>Looks again at every answer kept: is the array as it was, and does shaping again give the same glyphs?</summary>
    /// <param name="queue">The answers kept.</param>
    /// <param name="sink">Where a report's path is remembered.</param>
    /// <param name="folder">The directory to write into.</param>
    /// <param name="replay">Shapes a request again on a face loaded afresh.</param>
    /// <returns>How many answers were looked at and how many differ.</returns>
    let recheck (queue: ConcurrentQueue<Handed>) (sink: ConcurrentQueue<string>) (folder: string) (replay: ShapeRequest -> GlyphInfo array) : struct (int * int) =
        let answers = queue.ToArray()
        let found =
            answers
            |> Array.choose (fun answer ->
                let again = replay answer.Request
                match late answer again with
                | ValueSome outcome -> Some (answer, outcome, again)
                | ValueNone -> None)
        found |> Array.truncate MaxLateReports |> Array.iter (fun (answer, outcome, again) -> writeLate sink folder answer outcome again |> ignore)
        struct (answers.Length, found.Length)

    /// <summary>Looks twice at a suspicious result and reports it unless every pass agrees.</summary>
    /// <param name="sink">Where a report's path is remembered.</param>
    /// <param name="folder">The directory to write into.</param>
    /// <param name="request">The request that was shaped.</param>
    /// <param name="glyphCount">The number of glyphs in the face.</param>
    /// <param name="first">The shaped glyphs.</param>
    /// <param name="again">Shapes the request again on the same face.</param>
    /// <param name="fresh">Shapes the request on a face loaded again from the file.</param>
    /// <returns>The verdict, or none when nothing was suspicious.</returns>
    let examine (sink: ConcurrentQueue<string>) (folder: string) (request: ShapeRequest) (glyphCount: uint32) (first: GlyphInfo array) (again: unit -> GlyphInfo array) (fresh: unit -> GlyphInfo array) : Verdict voption =
        match suspicious glyphCount request.Text.Length first with
        | false -> ValueNone
        | true ->
            let second = again ()
            let third = fresh ()
            let outcome = verdict first second third
            match outcome with
            | Deterministic -> ()
            | Transient | DamagedFace -> write sink folder request glyphCount outcome first second third |> ignore
            ValueSome outcome

/// <summary>Shapes a request once on a face, with a font and a buffer of its own.</summary>
/// <param name="face">The face.</param>
/// <param name="request">The request.</param>
/// <returns>The glyphs, the positions and the font; none when shaping failed; an error when the request cannot run.</returns>
let shapeOnce (face: HbFace) (request: ShapeRequest) : Result<struct (GlyphInfo array * GlyphPosition array * HbFont) voption, string> =
    let font = new HbFont(face)
    let upem = float (Native.hb_face_get_upem face.Handle)
    let struct (sizeX, sizeY) = match request.FontSize with ValueSome size -> size | ValueNone -> struct (upem, upem)
    let struct (boldX, boldY) = request.Embolden
    Native.hb_font_set_ptem(font.Handle, float32 request.Ptem)
    Native.hb_font_set_synthetic_bold(font.Handle, float32 boldX, float32 boldY, 0)
    Native.hb_font_set_synthetic_slant(font.Handle, float32 request.Slant)
    Native.hb_font_set_scale(font.Handle, int (float32 sizeX), int (float32 sizeY))
    let coordinates = variations request.Variations
    Native.hb_font_set_variations(font.Handle, coordinates, uint32 coordinates.Length)
    let funcsSet = match request.FontFuncs with ValueSome funcs -> Native.hb_font_set_funcs_using(font.Handle, funcs) <> 0 | ValueNone -> true
    match funcsSet with
    | false -> Error "unknown font functions"
    | true ->
        use buffer = new HbBuffer()
        let add (codepoints: uint32 array) (offset: int) (length: int) = Native.hb_buffer_add_codepoints(buffer.Handle, codepoints, codepoints.Length, uint32 offset, length)
        if request.Before.Length > 0 then add request.Before request.Before.Length 0
        add request.Text 0 request.Text.Length
        if request.After.Length > 0 then add request.After 0 0
        let direction = utf8 request.Direction
        let script = utf8 request.Script
        let language = Encoding.UTF8.GetBytes request.Language
        Native.hb_buffer_set_direction(buffer.Handle, Native.hb_direction_from_string(direction, lengthOf direction))
        Native.hb_buffer_set_script(buffer.Handle, Native.hb_script_from_string(script, lengthOf script))
        Native.hb_buffer_set_language(buffer.Handle, Native.hb_language_from_string(language, language.Length))
        Native.hb_buffer_set_flags(buffer.Handle, uint32 request.Flags)
        Native.hb_buffer_set_not_found_variation_selector_glyph(buffer.Handle, request.NotFoundVariationSelectorGlyph)
        Native.hb_buffer_set_cluster_level(buffer.Handle, request.ClusterLevel)
        Native.hb_buffer_guess_segment_properties buffer.Handle
        let userFeatures = features request.Features
        let shapers : string array =
            match request.Shaper with
            | ValueSome list -> Array.append (list.Split(',')) [| Unchecked.defaultof<string> |]
            | ValueNone -> Unchecked.defaultof<string array>
        match Native.hb_shape_full(font.Handle, buffer.Handle, userFeatures, uint32 userFeatures.Length, shapers) with
        | 0 -> Ok ValueNone
        | _ ->
            let mutable length = 0u
            let infos = copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(buffer.Handle, &length)) (int length)
            let positions = copyFrom<GlyphPosition> (Native.hb_buffer_get_glyph_positions(buffer.Handle, &length)) (int length)
            Ok (ValueSome (struct (infos, positions, font)))

/// <summary>The glyphs of one more pass, for the tripwire: empty when the pass fails.</summary>
/// <param name="face">The face.</param>
/// <param name="request">The request.</param>
/// <returns>The glyphs.</returns>
let glyphsOnce (face: HbFace) (request: ShapeRequest) : GlyphInfo array =
    match shapeOnce face request with
    | Ok (ValueSome (struct (infos, _, font))) ->
        font.Dispose()
        infos
    | Ok ValueNone | Error _ -> [||]

/// <summary>Builds the HarfBuzz engine for the harness: HarfBuzzSharp objects configured exactly as hb-shape configures them.</summary>
/// <returns>The engine; faces are cached per font file and index for the life of the engine, as hb-shape's batch mode does.</returns>
let create () : Shaper =
    let faces = ConcurrentDictionary<struct (string * int), HbFace>()
    let faceOf (path: string) (index: int) = faces.GetOrAdd(struct (path, index), fun _ -> new HbFace(HbBlob.FromFile path, index))
    let shape (request: ShapeRequest) : Result<ShapeOutcome, string> =
        let face = faceOf request.FontPath request.FaceIndex
        match shapeOnce face request with
        | Error reason -> Error reason
        | Ok ValueNone -> Ok ShapingFailed
        | Ok (ValueSome (struct (infos, positions, font))) ->
            let again () = glyphsOnce face request
            let fresh () =
                use blob = HbBlob.FromFile request.FontPath
                use reloaded = new HbFace(blob, request.FaceIndex)
                glyphsOnce reloaded request
            match Tripwire.examine Tripwire.reports (Tripwire.directory ()) request (Native.hb_face_get_glyph_count face.Handle) infos again fresh with
            | ValueSome (Transient | DamagedFace as outcome) -> Console.Error.WriteLine $"HarfBuzz adapter tripwire: %A{outcome}, report in {Tripwire.directory ()}"
            | ValueSome Deterministic | ValueNone -> ()
            Tripwire.remember Tripwire.handed request infos
            Ok (Shaped
                    { Infos = infos
                      Positions = positions
                      GlyphName = fun glyph -> match font.TryGetGlyphName glyph with | true, name -> ValueSome name | false, _ -> ValueNone
                      Extents =
                        fun glyph ->
                            match font.TryGetGlyphExtents glyph with
                            | true, box -> ValueSome { XBearing = box.XBearing; YBearing = box.YBearing; Width = box.Width; Height = box.Height }
                            | false, _ -> ValueNone
                      GlyphFromName = fun name -> match font.TryGetGlyphFromName name with | true, glyph -> ValueSome glyph | false, _ -> ValueNone
                      NominalGlyph = fun codepoint -> match font.TryGetNominalGlyph codepoint with | true, glyph -> ValueSome glyph | false, _ -> ValueNone })
    { Name = $"HarfBuzz {Native.version ()}"
      Shapers = Native.stringList (Native.hb_shape_list_shapers ())
      FaceLoaders = [ "ot" ]
      FontFuncs = Native.stringList (Native.hb_font_list_funcs ())
      Shape = shape }

/// <summary>
/// The look after the tests: every answer of this run is compared with its copy and shaped again on a face loaded
/// afresh, with nothing else running. An answer that changed or cannot be repeated gets a report.
/// </summary>
/// <returns>How many answers were looked at and how many differ.</returns>
let recheck () : struct (int * int) =
    let faces = ConcurrentDictionary<struct (string * int), HbFace>()
    let faceOf (path: string) (index: int) = faces.GetOrAdd(struct (path, index), fun _ -> new HbFace(HbBlob.FromFile path, index))
    Tripwire.recheck Tripwire.handed Tripwire.reports (Tripwire.directory ()) (fun request -> glyphsOnce (faceOf request.FontPath request.FaceIndex) request)
