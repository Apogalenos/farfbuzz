module Farfbuzz.Bench.FontOps

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.OpenType
open Farfbuzz.Oracle
open Farfbuzz.Bench.NativeShaping

/// <summary>The <c>wght</c> axis tag of perf/benchmark-font.cc:7-10.</summary>
let private wghtTag = Tag.make 'w' 'g' 'h' 't'

/// <summary>The single variation benchmark-font.cc applies, <c>wght=500</c>.</summary>
let private wghtVariation = { Tag = wghtTag; Value = 500f }

/// <summary>The fonts of benchmark-font.cc and the CFF font of the extents note, each variable one with and without wght=500.</summary>
let private fontVariants =
    [ "test/subset/data/fonts/Roboto-Regular.ttf", false
      "test/subset/data/fonts/RobotoFlex-Variable.ttf", false
      "test/subset/data/fonts/RobotoFlex-Variable.ttf", true
      "test/subset/data/fonts/SourceSansPro-Regular.otf", false
      "test/subset/data/fonts/SourceSansPro-Regular.otf", true
      "test/subset/data/fonts/AdobeVFPrototype.otf", false
      "test/subset/data/fonts/AdobeVFPrototype.otf", true
      "test/subset/data/fonts/SourceSerifVariable-Roman.ttf", false
      "test/subset/data/fonts/SourceSerifVariable-Roman.ttf", true
      "test/subset/data/fonts/Comfortaa-Regular-new.ttf", false
      "test/subset/data/fonts/NotoNastaliqUrdu-Regular.ttf", false
      "test/subset/data/fonts/NotoSerifMyanmar-Regular.otf", false
      "perf/fonts/NotoSansDuployan-Regular.otf", false ]

/// <summary>Builds the Farfbuzz font of a file, applying wght=500 when requested.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="variable">Whether to apply wght=500.</param>
/// <returns>The font.</returns>
let private farfbuzzFontOf bytes variable =
    let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
    match variable with
    | false -> font
    | true -> Font.setVariations (ImmutableArray.Create wghtVariation) font

/// <summary>Builds the oracle font of a file, applying wght=500 when requested.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="variable">Whether to apply wght=500.</param>
/// <returns>The font handle.</returns>
let private nativeFontOf (bytes: byte array) variable =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let font = Native.hb_font_create(face)
    Native.hb_face_destroy(face)
    Native.hb_blob_destroy(blob)
    Native.hb_ot_font_set_funcs(font)
    match variable with
    | true -> Native.hb_font_set_variations(font, [| wghtVariation |], 1u)
    | false -> ()
    font

/// <summary>Every code point the font's cmap maps, expanded into one array.</summary>
/// <param name="font">The font.</param>
/// <returns>The code points.</returns>
let private unicodesOf (font: Font) =
    let ranges = Cmap.collectUnicodes font.Ot.Cmap.Value font.Face.GlyphCount
    [| for range in ranges do
        for codepoint in range.First .. range.Last do
            yield codepoint |]

/// <summary>One measured operation: the calls one pass makes and both engines' passes.</summary>
type OpCase =
    { Name: string
      Font: string
      Calls: int
      Native: unit -> int
      Farfbuzz: unit -> int }

/// <summary>The load-and-shape and face-load passes over the file bytes.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The native face-load pass.</returns>
let private nativeFaceLoad (bytes: byte array) =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let font = Native.hb_font_create(face)
    Native.hb_face_destroy(face)
    Native.hb_blob_destroy(blob)
    Native.hb_ot_font_set_funcs(font)
    let glyphOut = [| 0u |]
    Native.hb_font_get_nominal_glyphs(font, 1u, [| 0x20u |], 4u, glyphOut, 4u)
    Native.hb_font_destroy(font)
    int glyphOut[0]

/// <summary>The Farfbuzz face-load pass: a face, a font, then the first queries that load its tables.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>A checksum.</returns>
let private farfbuzzFaceLoad (bytes: byte array) =
    let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
    let glyph =
        match Font.nominalGlyph font 0x20u with
        | ValueSome glyph -> glyph
        | ValueNone -> 0u
    int glyph

/// <summary>The cases of one font.</summary>
/// <param name="path">The font path under the upstream checkout.</param>
/// <param name="variable">Whether wght=500 is applied.</param>
/// <returns>The cases.</returns>
let private casesOf (path: string) variable =
    let bytes = File.ReadAllBytes (Path.Combine(RepositoryPaths.upstream (), path))
    let font = farfbuzzFontOf bytes variable
    let native = nativeFontOf bytes variable
    let name = path.Substring(path.LastIndexOf('/') + 1) + (match variable with | true -> "/wght=500" | false -> String.Empty)
    let glyphs = Array.init font.Face.GlyphCount uint32
    let unicodes = unicodesOf font
    let glyphOut = Array.zeroCreate unicodes.Length
    let advanceOut = Array.zeroCreate glyphs.Length
    let advanceInto = Array.zeroCreate<int> glyphs.Length
    let originX = Array.zeroCreate glyphs.Length
    let originY = Array.zeroCreate glyphs.Length
    [ { Name = "nominal_glyph"
        Font = name
        Calls = unicodes.Length
        Native = fun () ->
            Native.hb_font_get_nominal_glyphs(native, uint32 unicodes.Length, unicodes, 4u, glyphOut, 4u)
            int glyphOut[unicodes.Length / 2]
        Farfbuzz = fun () ->
            let mutable total = 0
            for index in 0 .. unicodes.Length - 1 do
                match Font.nominalGlyph font unicodes[index] with
                | ValueSome glyph -> total <- total + int glyph
                | ValueNone -> ()
            total }
      { Name = "h_advance"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_h_advances(native, uint32 glyphs.Length, glyphs, 4u, advanceOut, 4u)
            advanceOut[glyphs.Length / 2]
        Farfbuzz = fun () ->
            let glyphs = glyphs
            let mutable total = 0
            for index in 0 .. glyphs.Length - 1 do
                total <- total + Font.hAdvance font glyphs[index]
            total }
      { Name = "h_advances (batch array)"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_h_advances(native, uint32 glyphs.Length, glyphs, 4u, advanceOut, 4u)
            advanceOut[glyphs.Length / 2]
        Farfbuzz = fun () ->
            let advances = Font.hAdvances font (ReadOnlySpan glyphs) |> Farfbuzz.Harness.TailCalls.settle
            advances[glyphs.Length / 2] }
      { Name = "h_advances (caller array)"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_h_advances(native, uint32 glyphs.Length, glyphs, 4u, advanceOut, 4u)
            advanceOut[glyphs.Length / 2]
        Farfbuzz = fun () ->
            Font.hAdvancesInto font (ReadOnlySpan glyphs) (Span advanceInto) |> Farfbuzz.Harness.TailCalls.settle |> ignore
            advanceInto[glyphs.Length / 2] }
      { Name = "v_advance"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_v_advances(native, uint32 glyphs.Length, glyphs, 4u, advanceOut, 4u)
            advanceOut[glyphs.Length / 2]
        Farfbuzz = fun () ->
            let glyphs = glyphs
            let mutable total = 0
            for index in 0 .. glyphs.Length - 1 do
                total <- total + Font.vAdvance font glyphs[index]
            total }
      { Name = "v_advances (batch array)"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_v_advances(native, uint32 glyphs.Length, glyphs, 4u, advanceOut, 4u)
            advanceOut[glyphs.Length / 2]
        Farfbuzz = fun () ->
            let advances = Font.vAdvances font (ReadOnlySpan glyphs) |> Farfbuzz.Harness.TailCalls.settle
            advances[glyphs.Length / 2] }
      { Name = "v_advances (caller array)"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_v_advances(native, uint32 glyphs.Length, glyphs, 4u, advanceOut, 4u)
            advanceOut[glyphs.Length / 2]
        Farfbuzz = fun () ->
            Font.vAdvancesInto font (ReadOnlySpan glyphs) (Span advanceInto) |> Farfbuzz.Harness.TailCalls.settle |> ignore
            advanceInto[glyphs.Length / 2] }
      { Name = "v_origins (batch array)"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_v_origins(native, uint32 glyphs.Length, glyphs, 4u, originX, 4u, originY, 4u)
            originX[glyphs.Length / 2] + originY[glyphs.Length / 2]
        Farfbuzz = fun () ->
            match Origins.vOrigins font (ReadOnlySpan glyphs) |> Farfbuzz.Harness.TailCalls.settle with
            | Ok origins ->
                let struct (x, y) = origins[glyphs.Length / 2]
                x + y
            | Error _ -> 0 }
      { Name = "v_origin"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            Native.hb_font_get_glyph_v_origins(native, uint32 glyphs.Length, glyphs, 4u, originX, 4u, originY, 4u)
            originX[glyphs.Length / 2] + originY[glyphs.Length / 2]
        Farfbuzz = fun () ->
            let mutable total = 0
            for index in 0 .. glyphs.Length - 1 do
                match Origins.vOrigin font glyphs[index] with
                | Ok (struct (x, y)) -> total <- total + x + y
                | Error _ -> ()
            total }
      { Name = "glyph_extents"
        Font = name
        Calls = glyphs.Length
        Native = fun () ->
            let mutable total = 0
            let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
            for index in 0 .. glyphs.Length - 1 do
                Native.hb_font_get_glyph_extents(native, glyphs[index], &extents) |> ignore
                total <- total + extents.XBearing + extents.YBearing + extents.Width + extents.Height
            total
        Farfbuzz = fun () ->
            let mutable total = 0
            for index in 0 .. glyphs.Length - 1 do
                match Extents.glyph font glyphs[index] with
                | Ok (ValueSome extents) -> total <- total + extents.XBearing + extents.YBearing + extents.Width + extents.Height
                | Ok ValueNone -> ()
                | Error _ -> ()
            total }
      { Name = "face_load (face, font, one lookup)"
        Font = name
        Calls = 1
        Native = fun () -> nativeFaceLoad bytes
        Farfbuzz = fun () -> farfbuzzFaceLoad bytes } ]

/// <summary>Every case.</summary>
let private allCases =
    oracle.Force() |> ignore
    [ for (path, variable) in fontVariants do
        yield! casesOf path variable ]

/// <summary>Times one pass function, repeated, in nanoseconds a pass.</summary>
/// <param name="pass">The pass.</param>
/// <param name="repeats">The passes in one sample.</param>
/// <returns>The nanoseconds of one sample.</returns>
let private sample (pass: unit -> int) repeats =
    let mutable sink = 0
    let start = Stopwatch.GetTimestamp()
    for _ in 1 .. repeats do
        sink <- sink + pass ()
    let stop = Stopwatch.GetTimestamp()
    GC.KeepAlive sink
    float (stop - start) * 1e9 / float Stopwatch.Frequency

/// <summary>Measures one case: warm both engines, then alternate samples and take each engine's minimum.</summary>
/// <param name="case">The case.</param>
/// <returns>The native ns a call, the Farfbuzz ns a call and the Farfbuzz bytes a call.</returns>
let private measure case =
    for _ in 1 .. 3 do
        case.Native () |> ignore
        case.Farfbuzz () |> ignore
    let one = sample case.Farfbuzz 1
    let repeats = max 1 (int (4e6 / max one 1.0))
    let warmUntil = Stopwatch.GetTimestamp() + Stopwatch.Frequency / 2L
    while Stopwatch.GetTimestamp() < warmUntil do
        sample case.Native repeats |> ignore
        sample case.Farfbuzz repeats |> ignore
    let before = GC.GetAllocatedBytesForCurrentThread()
    case.Farfbuzz () |> ignore
    let bytes = GC.GetAllocatedBytesForCurrentThread() - before
    let mutable nativeBest = Double.MaxValue
    let mutable farfbuzzBest = Double.MaxValue
    for _ in 1 .. 15 do
        nativeBest <- min nativeBest (sample case.Native repeats)
        farfbuzzBest <- min farfbuzzBest (sample case.Farfbuzz repeats)
    let calls = float (repeats * case.Calls)
    struct (nativeBest / calls, farfbuzzBest / calls, float bytes / float case.Calls)

/// <summary>Prints the table of every operation and font: HarfBuzz time a call, Farfbuzz time a call, ratio, bytes a call.</summary>
/// <param name="filter">A substring the case name and font must contain, or empty.</param>
let run (filter: string) =
    Console.WriteLine("operation | font | HarfBuzz ns/call | Farfbuzz ns/call | ratio | Farfbuzz B/call")
    for case in allCases do
        match (case.Name + " " + case.Font).Contains(filter, StringComparison.OrdinalIgnoreCase) with
        | false -> ()
        | true ->
            let struct (native, farfbuzz, bytes) = measure case
            Console.WriteLine(String.Format("{0} | {1} | {2:F1} | {3:F1} | {4:F2} | {5:F1}", case.Name, case.Font, native, farfbuzz, farfbuzz / native, bytes))

/// <summary>Runs one engine's pass of the first case whose name and font contain the filters, for a sampling profiler.</summary>
/// <param name="operation">A substring of the operation name.</param>
/// <param name="font">A substring of the font name.</param>
/// <param name="native">Whether to run the HarfBuzz pass.</param>
/// <param name="seconds">How long to run.</param>
let loop (operation: string) (font: string) native seconds =
    let case = allCases |> List.find (fun case -> case.Name.Contains(operation) && case.Font.Contains(font))
    let pass = match native with | true -> case.Native | false -> case.Farfbuzz
    Console.WriteLine(String.Format("pid {0}, {1} / {2}, {3} calls a pass", Environment.ProcessId, case.Name, case.Font, case.Calls))
    let stop = Stopwatch.GetTimestamp() + int64 seconds * Stopwatch.Frequency
    let mutable sink = 0
    while Stopwatch.GetTimestamp() < stop do
        sink <- sink + pass ()
    GC.KeepAlive sink

/// <summary>Times the parts of loading one face, in nanoseconds a load.</summary>
/// <param name="path">The font path under the upstream checkout.</param>
let faceParts (path: string) =
    let bytes = File.ReadAllBytes (Path.Combine(RepositoryPaths.upstream (), path))
    let time (name: string) (action: unit -> int) =
        for _ in 1 .. 3000 do action () |> ignore
        let mutable best = Double.MaxValue
        for _ in 1 .. 15 do
            best <- min best (sample action 300 / 300.0)
        Console.WriteLine(String.Format("{0}: {1:F0} ns", name, best))
    time "Face.create" (fun () -> (Face.create (ReadOnlyMemory bytes) 0u).GlyphCount)
    time "Face.create + OtFace.create" (fun () -> (OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)).Face.GlyphCount)
    time "Face.create + Font.create" (fun () -> (Font.create (Face.create (ReadOnlyMemory bytes) 0u)).XScale)
    time "Face.create + Os2" (fun () -> int (Os2.fontPage (OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)).Os2.Value))
    time "Face.create + Font.create + Cmap" (fun () -> (Font.create (Face.create (ReadOnlyMemory bytes) 0u)).Ot.Cmap.Value.Latin.Length)
    let face = Face.create (ReadOnlyMemory bytes) 0u
    time "OtFace.create alone" (fun () -> (OtFace.create face).Face.GlyphCount)
    time "Cff2.load" (fun () -> (Cff2.load face).NumGlyphs)
    time "Fvar.load" (fun () -> (Fvar.load face).Axes.Length)
    time "Avar.load" (fun () -> ignore (Avar.load face); 0)
    time "Hvar.loadHvar" (fun () -> if (Hvar.loadHvar face).HasData then 1 else 0)
    time "Hvar.loadVvar" (fun () -> if (Hvar.loadVvar face).HasData then 1 else 0)
    time "Mvar.load" (fun () -> ignore (Mvar.load face); 0)
    time "FaceVariations.load" (fun () -> (Farfbuzz.Var.FaceVariations.load face).Fvar.Axes.Length)
    time "OtFace + Variations.Value" (fun () -> (OtFace.create face).Variations.Value.Fvar.Axes.Length)
