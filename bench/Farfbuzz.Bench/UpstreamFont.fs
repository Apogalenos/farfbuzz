module Farfbuzz.Bench.UpstreamFont

open System
open System.Collections.Immutable
open System.IO
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.OpenType
open Farfbuzz.Oracle
open Farfbuzz.Bench.NativeShaping

/// <summary>One font operation of perf/benchmark-font.cc.</summary>
type FontOp =
    | NominalGlyphs = 0
    | HAdvances = 1
    | VAdvances = 2
    | VOrigins = 3
    | Extents = 4
    | LoadFaceAndShape = 5

/// <summary>The <c>wght</c> axis tag of perf/benchmark-font.cc:7-10.</summary>
let private wghtTag = Tag.make 'w' 'g' 'h' 't'

/// <summary>The single variation benchmark-font.cc applies, <c>wght=500</c>.</summary>
let private wghtVariation = { Tag = wghtTag; Value = 500f }

/// <summary>The text benchmark-font.cc shapes for load_face_and_shape, a single space.</summary>
let private spaceText = [| 0x20uy |]

/// <summary>The code point of <see cref="spaceText"/>, added through <c>hb_buffer_add_codepoints</c>, which HarfBuzzSharp's own native library exports where the oracle shims do not exist.</summary>
let private spaceCodepoints = [| 0x20u |]

/// <summary>The six operations, in benchmark-font.cc order (draw and paint are out of scope).</summary>
let private allOps =
    [ FontOp.NominalGlyphs
      FontOp.HAdvances
      FontOp.VAdvances
      FontOp.VOrigins
      FontOp.Extents
      FontOp.LoadFaceAndShape ]

/// <summary>The eight fonts of benchmark-font.cc:19-29, each variable one measured with and without wght=500.</summary>
let private fontVariants =
    [ "Roboto-Regular.ttf", false
      "RobotoFlex-Variable.ttf", false
      "RobotoFlex-Variable.ttf", true
      "SourceSansPro-Regular.otf", false
      "SourceSansPro-Regular.otf", true
      "AdobeVFPrototype.otf", false
      "AdobeVFPrototype.otf", true
      "SourceSerifVariable-Roman.ttf", false
      "SourceSerifVariable-Roman.ttf", true
      "Comfortaa-Regular-new.ttf", false
      "NotoNastaliqUrdu-Regular.ttf", false
      "NotoSerifMyanmar-Regular.otf", false ]

/// <summary>The operation name of a case, as benchmark-font.cc spells it.</summary>
/// <param name="op">The operation.</param>
/// <returns>The name.</returns>
let private opName (op: FontOp) : string =
    match op with
    | FontOp.NominalGlyphs -> "nominal_glyphs"
    | FontOp.HAdvances -> "glyph_h_advances"
    | FontOp.VAdvances -> "glyph_v_advances"
    | FontOp.VOrigins -> "glyph_v_origins"
    | FontOp.Extents -> "glyph_extents"
    | FontOp.LoadFaceAndShape -> "load_face_and_shape"
    | _ -> op.ToString()

/// <summary>The full path of one benchmark font under the upstream checkout.</summary>
/// <param name="file">The file name.</param>
/// <returns>The path.</returns>
let private fontPath (file: string) : string =
    Path.Combine(RepositoryPaths.upstream (), "test", "subset", "data", "fonts", file)

/// <summary>One measured font, variation and operation.</summary>
type FontCase =
    { Font: string
      Variable: bool
      Op: FontOp
      NaReason: string voption
      Bytes: byte array
      FarfbuzzFont: Font
      NativeFont: nativeint
      GlyphCount: int
      Unicodes: uint32 array
      Glyphs: uint32 array
      GlyphOut: uint32 array
      AdvanceOut: int array
      OriginX: int array
      OriginY: int array }
    override case.ToString() = $"{opName case.Op}/{case.Font}" + (if case.Variable then "/var" else "")

/// <summary>The name of an outline source the core cannot read yet.</summary>
/// <param name="source">The source.</param>
/// <returns>The name.</returns>
let private outlineName (source: OutlineSource) : string =
    match source with
    | ColrOutlines -> "COLR"
    | VarcOutlines -> "VARC"
    | SyntheticOutlines -> "synthetic outlines"

/// <summary>Builds the default Farfbuzz font for a file, applying wght=500 when requested.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="variable">Whether to apply wght=500.</param>
/// <returns>The font.</returns>
let private farfbuzzFontOf (bytes: byte array) (variable: bool) : Font =
    let face = Face.create (ReadOnlyMemory bytes) 0u
    let font = Font.create face
    match variable with
    | false -> font
    | true -> Font.setVariations (ImmutableArray.Create wghtVariation) font

/// <summary>Builds the OT-func oracle font for a file, applying wght=500 when requested.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="variable">Whether to apply wght=500.</param>
/// <returns>The font handle.</returns>
let private nativeFontOf (bytes: byte array) (variable: bool) : nativeint =
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

/// <summary>Expands the code points a cmap maps into one array, for the nominal-glyph pass.</summary>
/// <param name="font">The font.</param>
/// <param name="glyphCount">The face glyph count.</param>
/// <returns>The code points.</returns>
let private collectUnicodes (font: Font) (glyphCount: int) : uint32 array =
    let ranges = Cmap.collectUnicodes font.Ot.Cmap.Value glyphCount
    let total = ranges |> Seq.sumBy (fun range -> int (range.Last - range.First + 1u))
    let result = Array.zeroCreate total
    let mutable index = 0
    for range in ranges do
        for codepoint in range.First .. range.Last do
            result[index] <- codepoint
            index <- index + 1
    result

/// <summary>Why the core cannot measure an operation, or none when it can: only an outline source the core does not read.</summary>
/// <param name="op">The operation.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <returns>The reason, or none.</returns>
let private naReason (op: FontOp) (font: Font) =
    let unreadable () =
        match Extents.source font with
        | ValueSome source -> ValueSome $"outlines not ported: {outlineName source}"
        | ValueNone -> ValueNone
    match op with
    | FontOp.NominalGlyphs -> ValueNone
    | FontOp.HAdvances -> ValueNone
    | FontOp.VAdvances -> ValueNone
    | FontOp.VOrigins ->
        match font.Ot.Vorg.Value.Present with
        | true -> ValueNone
        | false -> unreadable ()
    | FontOp.Extents -> unreadable ()
    | FontOp.LoadFaceAndShape -> ValueNone
    | _ -> ValueNone

/// <summary>Throws naming the font and the difference.</summary>
/// <param name="file">The font file name.</param>
/// <param name="op">The operation.</param>
/// <param name="detail">The difference.</param>
let private differ (file: string) (op: string) (detail: string) =
    raise (InvalidOperationException($"font {file}: {op} differs: {detail}"))

/// <summary>Checks nominal glyphs agree for every code point.</summary>
/// <param name="file">The font file name.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
/// <param name="unicodes">The code points.</param>
let private checkNominalGlyphs (file: string) (font: Font) (native: nativeint) (unicodes: uint32 array) : unit =
    let glyphs = Array.zeroCreate unicodes.Length
    Native.hb_font_get_nominal_glyphs(native, uint32 unicodes.Length, unicodes, 4u, glyphs, 4u)
    for index in 0 .. unicodes.Length - 1 do
        let ours =
            match Font.nominalGlyph font unicodes[index] with
            | ValueSome glyph -> glyph
            | ValueNone -> 0u
        match ours = glyphs[index] with
        | true -> ()
        | false -> differ file "nominal glyphs" $"code point {unicodes[index]} Farfbuzz {ours} HarfBuzz {glyphs[index]}"

/// <summary>Checks horizontal advances agree for every glyph.</summary>
/// <param name="file">The font file name.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
/// <param name="glyphs">The glyph ids.</param>
let private checkHAdvances (file: string) (font: Font) (native: nativeint) (glyphs: uint32 array) : unit =
    let advances = Array.zeroCreate glyphs.Length
    Native.hb_font_get_glyph_h_advances(native, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    let ours = Font.hAdvances font (ReadOnlySpan glyphs)
    for index in 0 .. glyphs.Length - 1 do
        match ours[index] = advances[index] with
        | true -> ()
        | false -> differ file "h advances" $"glyph {glyphs[index]} Farfbuzz {ours[index]} HarfBuzz {advances[index]}"

/// <summary>Checks vertical advances agree for every glyph.</summary>
/// <param name="file">The font file name.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
/// <param name="glyphs">The glyph ids.</param>
let private checkVAdvances (file: string) (font: Font) (native: nativeint) (glyphs: uint32 array) : unit =
    let advances = Array.zeroCreate glyphs.Length
    Native.hb_font_get_glyph_v_advances(native, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    let ours = Font.vAdvances font (ReadOnlySpan glyphs)
    for index in 0 .. glyphs.Length - 1 do
        match ours[index] = advances[index] with
        | true -> ()
        | false -> differ file "v advances" $"glyph {glyphs[index]} Farfbuzz {ours[index]} HarfBuzz {advances[index]}"

/// <summary>Checks vertical origins agree for every glyph.</summary>
/// <param name="file">The font file name.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
/// <param name="glyphs">The glyph ids.</param>
let private checkVOrigins (file: string) (font: Font) (native: nativeint) (glyphs: uint32 array) : unit =
    let xs = Array.zeroCreate glyphs.Length
    let ys = Array.zeroCreate glyphs.Length
    Native.hb_font_get_glyph_v_origins(native, uint32 glyphs.Length, glyphs, 4u, xs, 4u, ys, 4u)
    match Origins.vOrigins font (ReadOnlySpan glyphs) with
    | Error _ -> differ file "v origins" "unexpectedly not ported"
    | Ok ours ->
        for index in 0 .. glyphs.Length - 1 do
            let struct (x, y) = ours[index]
            match x = xs[index] && y = ys[index] with
            | true -> ()
            | false -> differ file "v origins" $"glyph {glyphs[index]} Farfbuzz ({x},{y}) HarfBuzz ({xs[index]},{ys[index]})"

/// <summary>Checks extents agree for every glyph.</summary>
/// <param name="file">The font file name.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
/// <param name="glyphs">The glyph ids.</param>
let private checkExtents (file: string) (font: Font) (native: nativeint) (glyphs: uint32 array) : unit =
    for index in 0 .. glyphs.Length - 1 do
        let glyph = glyphs[index]
        let mutable nativeExtents = Unchecked.defaultof<Native.OracleGlyphExtents>
        let present = Native.hb_font_get_glyph_extents(native, glyph, &nativeExtents)
        match Extents.glyph font glyph with
        | Error _ -> differ file "extents" $"glyph {glyph} unexpectedly not ported"
        | Ok ValueNone ->
            match present = 0 with
            | true -> ()
            | false -> differ file "extents" $"glyph {glyph} Farfbuzz none HarfBuzz present"
        | Ok (ValueSome ours) ->
            match present <> 0
                  && ours.XBearing = nativeExtents.XBearing
                  && ours.YBearing = nativeExtents.YBearing
                  && ours.Width = nativeExtents.Width
                  && ours.Height = nativeExtents.Height with
            | true -> ()
            | false -> differ file "extents" $"glyph {glyph} Farfbuzz ({ours.XBearing},{ours.YBearing},{ours.Width},{ours.Height}) HarfBuzz ({nativeExtents.XBearing},{nativeExtents.YBearing},{nativeExtents.Width},{nativeExtents.Height})"

/// <summary>Checks shaping one space agrees.</summary>
/// <param name="file">The font file name.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
let private checkShape (file: string) (font: Font) (native: nativeint) : unit =
    let run = GlyphRun.addUtf8 (ReadOnlySpan spaceText) 0 1 GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    let ours =
        match Shape.shapeFull font ImmutableArray<Feature>.Empty ValueNone run with
        | Ok shaped -> shaped
        | Error _ -> differ file "load_face_and_shape" "shaping failed"
    let buffer = Native.hb_buffer_create ()
    Native.hb_buffer_add_codepoints(buffer, spaceCodepoints, 1, 0u, 1)
    Native.hb_buffer_guess_segment_properties(buffer)
    Native.hb_shape_full(native, buffer, Unchecked.defaultof<Feature array>, 0u, Unchecked.defaultof<string array>) |> ignore
    let mutable length = 0u
    let infos = NativeText.copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(buffer, &length)) (int length)
    let positions = NativeText.copyFrom<GlyphPosition> (Native.hb_buffer_get_glyph_positions(buffer, &length)) (int length)
    Native.hb_buffer_destroy(buffer)
    match ours.Infos.Length = infos.Length with
    | false -> differ file "load_face_and_shape" $"glyph count Farfbuzz {ours.Infos.Length} HarfBuzz {infos.Length}"
    | true ->
        for index in 0 .. ours.Infos.Length - 1 do
            let ourInfo = ours.Infos[index]
            let ourPos = ours.Positions[index]
            let nativeInfo = infos[index]
            let nativePos = positions[index]
            match ourInfo.Codepoint = nativeInfo.Codepoint
                  && ourPos.XAdvance = nativePos.XAdvance
                  && ourPos.YAdvance = nativePos.YAdvance
                  && ourPos.XOffset = nativePos.XOffset
                  && ourPos.YOffset = nativePos.YOffset with
            | true -> ()
            | false -> differ file "load_face_and_shape" $"glyph {index} Farfbuzz gid {ourInfo.Codepoint} pos ({ourPos.XAdvance},{ourPos.YAdvance}) HarfBuzz gid {nativeInfo.Codepoint} pos ({nativePos.XAdvance},{nativePos.YAdvance})"

/// <summary>Runs the equality check for an operation, when it is supported.</summary>
/// <param name="file">The font file name.</param>
/// <param name="op">The operation.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
/// <param name="unicodes">The code points.</param>
/// <param name="glyphs">The glyph ids.</param>
let private checkOp (file: string) (op: FontOp) (font: Font) (native: nativeint) (unicodes: uint32 array) (glyphs: uint32 array) : unit =
    match op with
    | FontOp.NominalGlyphs -> checkNominalGlyphs file font native unicodes
    | FontOp.HAdvances -> checkHAdvances file font native glyphs
    | FontOp.VAdvances -> checkVAdvances file font native glyphs
    | FontOp.VOrigins -> checkVOrigins file font native glyphs
    | FontOp.Extents -> checkExtents file font native glyphs
    | FontOp.LoadFaceAndShape -> checkShape file font native
    | _ -> ()

/// <summary>Builds one case, running the equality check when the operation is supported.</summary>
/// <param name="file">The font file name.</param>
/// <param name="variable">Whether wght=500 is applied.</param>
/// <param name="op">The operation.</param>
/// <param name="bytes">The file bytes.</param>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="native">The oracle font.</param>
/// <param name="unicodes">The code points.</param>
/// <param name="glyphs">The glyph ids.</param>
/// <returns>The case.</returns>
let private buildCase (file: string) (variable: bool) (op: FontOp) (bytes: byte array) (font: Font) (native: nativeint) (unicodes: uint32 array) (glyphs: uint32 array) =
    let reason = naReason op font
    match reason with
    | ValueNone -> checkOp file op font native unicodes glyphs
    | ValueSome _ -> ()
    { Font = file
      Variable = variable
      Op = op
      NaReason = reason
      Bytes = bytes
      FarfbuzzFont = font
      NativeFont = native
      GlyphCount = glyphs.Length
      Unicodes = unicodes
      Glyphs = glyphs
      GlyphOut = Array.zeroCreate unicodes.Length
      AdvanceOut = Array.zeroCreate glyphs.Length
      OriginX = Array.zeroCreate glyphs.Length
      OriginY = Array.zeroCreate glyphs.Length }

/// <summary>Every case, built once with the equality check already run.</summary>
let private allCases : FontCase list =
    oracle.Force() |> ignore
    [ for (file, variable) in fontVariants do
        let bytes = File.ReadAllBytes (fontPath file)
        let font = farfbuzzFontOf bytes variable
        let native = nativeFontOf bytes variable
        let glyphs = Array.init font.Face.GlyphCount uint32
        let unicodes = collectUnicodes font font.Face.GlyphCount
        for op in allOps do
            yield buildCase file variable op bytes font native unicodes glyphs ]

/// <summary>Sums an integer array, so the measured work cannot be elided.</summary>
/// <param name="values">The values.</param>
/// <returns>The sum.</returns>
let private sumInts (values: int array) : int =
    let mutable total = 0
    for index in 0 .. values.Length - 1 do
        total <- total + values[index]
    total

/// <summary>Sums a uint32 array, so the measured work cannot be elided.</summary>
/// <param name="values">The values.</param>
/// <returns>The sum.</returns>
let private sumUInt32s (values: uint32 array) : int =
    let mutable total = 0
    for index in 0 .. values.Length - 1 do
        total <- total + int values[index]
    total

/// <summary>Sums an immutable integer array, so the measured work cannot be elided.</summary>
/// <param name="values">The values.</param>
/// <returns>The sum.</returns>
let private sumImmutableInts (values: ImmutableArray<int>) : int =
    let mutable total = 0
    for index in 0 .. values.Length - 1 do
        total <- total + values[index]
    total

/// <summary>Shapes one space from file bytes already in memory, the load_face_and_shape operation.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The number of glyphs.</returns>
let private nativeShapeOnce (bytes: byte array) : int =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let font = Native.hb_font_create(face)
    Native.hb_face_destroy(face)
    Native.hb_blob_destroy(blob)
    Native.hb_ot_font_set_funcs(font)
    let buffer = Native.hb_buffer_create ()
    Native.hb_buffer_add_codepoints(buffer, spaceCodepoints, 1, 0u, 1)
    Native.hb_buffer_guess_segment_properties(buffer)
    Native.hb_shape_full(font, buffer, Unchecked.defaultof<Feature array>, 0u, Unchecked.defaultof<string array>) |> ignore
    let glyphs = int (Native.hb_buffer_get_length(buffer))
    Native.hb_buffer_destroy(buffer)
    Native.hb_font_destroy(font)
    glyphs

/// <summary>Shapes one space from file bytes already in memory with Farfbuzz.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The number of glyphs.</returns>
let private farfbuzzShapeOnce (bytes: byte array) : int =
    let face = Face.create (ReadOnlyMemory bytes) 0u
    let font = Font.create face
    let run = GlyphRun.addUtf8 (ReadOnlySpan spaceText) 0 1 GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    match Shape.shapeFull font ImmutableArray<Feature>.Empty ValueNone run with
    | Ok shaped -> shaped.Infos.Length
    | Error _ -> 0

/// <summary>Runs one native operation over a case.</summary>
/// <param name="case">The case.</param>
/// <returns>The checksum, so the work cannot be elided.</returns>
let private runNative (case: FontCase) : int =
    match case.Op with
    | FontOp.NominalGlyphs ->
        Native.hb_font_get_nominal_glyphs(case.NativeFont, uint32 case.Unicodes.Length, case.Unicodes, 4u, case.GlyphOut, 4u)
        sumUInt32s case.GlyphOut
    | FontOp.HAdvances ->
        Native.hb_font_get_glyph_h_advances(case.NativeFont, uint32 case.GlyphCount, case.Glyphs, 4u, case.AdvanceOut, 4u)
        sumInts case.AdvanceOut
    | FontOp.VAdvances ->
        Native.hb_font_get_glyph_v_advances(case.NativeFont, uint32 case.GlyphCount, case.Glyphs, 4u, case.AdvanceOut, 4u)
        sumInts case.AdvanceOut
    | FontOp.VOrigins ->
        Native.hb_font_get_glyph_v_origins(case.NativeFont, uint32 case.GlyphCount, case.Glyphs, 4u, case.OriginX, 4u, case.OriginY, 4u)
        sumInts case.OriginX + sumInts case.OriginY
    | FontOp.Extents ->
        let mutable total = 0
        let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
        for index in 0 .. case.GlyphCount - 1 do
            Native.hb_font_get_glyph_extents(case.NativeFont, case.Glyphs[index], &extents) |> ignore
            total <- total + extents.XBearing + extents.YBearing + extents.Width + extents.Height
        total
    | FontOp.LoadFaceAndShape -> nativeShapeOnce case.Bytes
    | _ -> 0

/// <summary>Runs one Farfbuzz operation over a case.</summary>
/// <param name="case">The case.</param>
/// <returns>The checksum, so the work cannot be elided.</returns>
let private runFarfbuzz (case: FontCase) : int =
    match case.Op with
    | FontOp.NominalGlyphs ->
        let mutable total = 0
        for index in 0 .. case.Unicodes.Length - 1 do
            match Font.nominalGlyph case.FarfbuzzFont case.Unicodes[index] with
            | ValueSome glyph -> total <- total + int glyph
            | ValueNone -> ()
        total
    | FontOp.HAdvances ->
        sumImmutableInts (Font.hAdvances case.FarfbuzzFont (ReadOnlySpan case.Glyphs))
    | FontOp.VAdvances ->
        sumImmutableInts (Font.vAdvances case.FarfbuzzFont (ReadOnlySpan case.Glyphs))
    | FontOp.VOrigins ->
        match Origins.vOrigins case.FarfbuzzFont (ReadOnlySpan case.Glyphs) with
        | Error _ -> 0
        | Ok origins ->
            let mutable total = 0
            for index in 0 .. origins.Length - 1 do
                let struct (x, y) = origins[index]
                total <- total + x + y
            total
    | FontOp.Extents ->
        let mutable total = 0
        for index in 0 .. case.GlyphCount - 1 do
            match Extents.glyph case.FarfbuzzFont case.Glyphs[index] with
            | Ok (ValueSome extents) -> total <- total + extents.XBearing + extents.YBearing + extents.Width + extents.Height
            | Ok ValueNone | Error _ -> ()
        total
    | FontOp.LoadFaceAndShape -> farfbuzzShapeOnce case.Bytes
    | _ -> 0

/// <summary>The font operations of perf/benchmark-font.cc, Farfbuzz against native HarfBuzz 15.1.0.</summary>
[<MemoryDiagnoser>]
type UpstreamFont() =
    /// <summary>The cases, one per font, variation and operation.</summary>
    member _.Cases() : seq<obj> = allCases |> Seq.map (fun case -> case :> obj)

    /// <summary>Runs one operation through native HarfBuzz.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The checksum.</returns>
    [<Benchmark(Baseline = true); ArgumentsSource("Cases")>]
    member _.Native(case: FontCase) : int =
        match case.NaReason with
        | ValueSome reason -> raise (NotSupportedException(reason))
        | ValueNone -> runNative case

    /// <summary>Runs one operation through Farfbuzz.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The checksum.</returns>
    [<Benchmark; ArgumentsSource("Cases")>]
    member _.Farfbuzz(case: FontCase) : int =
        match case.NaReason with
        | ValueSome reason -> raise (NotSupportedException(reason))
        | ValueNone -> runFarfbuzz case
