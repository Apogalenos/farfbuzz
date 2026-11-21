module Farfbuzz.HarfBuzzSharp.Equivalence.PublicApiShaper

open System
open System.Collections.Concurrent
open System.IO
open System.Runtime.InteropServices
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper

/// <summary>The faces made so far, one per file and index; a face is made once and shared.</summary>
let private faces = ConcurrentDictionary<struct (string * int), Lazy<HarfBuzzSharp.Face>>()

/// <summary>A face over a font file, made once and shared; the blob copies the bytes, so the pinned memory lives for the construction only.</summary>
/// <param name="path">The file.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (path: string) (index: int) =
    faces.GetOrAdd(struct (path, index), fun _ ->
        lazy
            (let bytes = File.ReadAllBytes path
             let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
             use blob = new HarfBuzzSharp.Blob(handle.AddrOfPinnedObject(), bytes.Length, HarfBuzzSharp.MemoryMode.Duplicate)
             handle.Free()
             new HarfBuzzSharp.Face(blob, index))).Value

/// <summary>The facade's direction of the core's.</summary>
/// <param name="direction">The core direction.</param>
/// <returns>The public API's direction.</returns>
let private directionOf (direction: Direction) =
    enum<HarfBuzzSharp.Direction> (int (Direction.value direction))

/// <summary>The features of a request, parsed as hb-shape does: matching outer quotes stripped, invalid items dropped.</summary>
/// <param name="raw">The argument.</param>
/// <returns>The features.</returns>
let private featuresOf (raw: string voption) =
    match raw with
    | ValueNone -> [||]
    | ValueSome text ->
        let unquoted =
            match text.Length >= 1 && text[0] = text[text.Length - 1] && (text[0] = '"' || text[0] = '\'') with
            | true -> text.Substring(1, max 0 (text.Length - 2))
            | false -> text
        match unquoted.Length with
        | 0 -> [||]
        | _ ->
            unquoted.Split([| ','; ' ' |])
            |> Array.choose (fun item -> match HarfBuzzSharp.Feature.TryParse item with | true, feature -> Some feature | false, _ -> None)

/// <summary>The variations of a request, parsed as hb-shape does: split at every comma and space, invalid items dropped.</summary>
/// <param name="raw">The argument.</param>
/// <returns>The variations.</returns>
let private variationsOf (raw: string) =
    raw.Split([| ','; ' ' |])
    |> Array.choose (fun item ->
        match Variation.parse item with
        | Ok variation -> Some (HarfBuzzSharp.Variation(Tag = uint32 variation.Tag, Value = variation.Value))
        | Error _ -> None)

/// <summary>Shapes a request the way Avalonia drives HarfBuzzSharp: a font at the request's scale and variations, the text in up to three adds, segment properties, guess, shape.</summary>
/// <param name="request">The request.</param>
/// <returns>The outcome, or why the request cannot run.</returns>
let shape (request: ShapeRequest) =
    match Dump.unsupported request with
    | ValueSome reason -> Error $"unsupported: {reason}"
    | ValueNone ->
        let face = faceOf request.FontPath request.FaceIndex
        let font = new HarfBuzzSharp.Font(face)
        let upem = float face.UnitsPerEm
        let struct (sizeX, sizeY) = match request.FontSize with | ValueSome size -> size | ValueNone -> struct (upem, upem)
        font.SetScale(int (float32 sizeX), int (float32 sizeY))
        match request.Variations with
        | ValueSome raw -> font.SetVariations(ReadOnlySpan (variationsOf raw))
        | ValueNone -> ()
        use buffer = new HarfBuzzSharp.Buffer()
        buffer.ClusterLevel <- enum<HarfBuzzSharp.ClusterLevel> request.ClusterLevel
        buffer.Flags <- enum<HarfBuzzSharp.BufferFlags> (int (uint32 request.Flags))
        match request.Before.Length > 0 with
        | true -> buffer.AddCodepoints(ReadOnlySpan<uint32> request.Before, request.Before.Length, 0)
        | false -> ()
        buffer.AddCodepoints(ReadOnlySpan<uint32> request.Text, 0, request.Text.Length)
        match request.After.Length > 0 with
        | true -> buffer.AddCodepoints(ReadOnlySpan<uint32> request.After, 0, 0)
        | false -> ()
        match request.Direction with
        | ValueSome text -> buffer.Direction <- directionOf (Direction.ofString text)
        | ValueNone -> ()
        match request.Script with
        | ValueSome text -> buffer.Script <- HarfBuzzSharp.Script.Parse text
        | ValueNone -> ()
        use language = new HarfBuzzSharp.Language(request.Language)
        buffer.Language <- language
        buffer.GuessSegmentProperties()
        let shapers = match request.Shaper with | ValueSome text -> text.Split ',' | ValueNone -> Unchecked.defaultof<string array>
        font.Shape(buffer, featuresOf request.Features, shapers)
        match buffer.ContentType = HarfBuzzSharp.ContentType.Glyphs with
        | false -> Ok ShapingFailed
        | true ->
            let infos = buffer.GlyphInfos
            let positions = buffer.GlyphPositions
            let name (glyph: uint32) =
                match font.TryGetGlyphName glyph with
                | true, text -> ValueSome text
                | false, _ -> ValueNone
            let extents (glyph: uint32) =
                match font.TryGetGlyphExtents glyph with
                | true, found -> ValueSome { XBearing = found.XBearing; YBearing = found.YBearing; Width = found.Width; Height = found.Height }
                | false, _ -> ValueNone
            let fromName (text: string) =
                match font.TryGetGlyphFromName text with
                | true, glyph -> ValueSome glyph
                | false, _ -> ValueNone
            let nominal (codepoint: uint32) =
                match font.TryGetNominalGlyph codepoint with
                | true, glyph -> ValueSome glyph
                | false, _ -> ValueNone
            Ok (Shaped
                    { Infos = MemoryMarshal.Cast<HarfBuzzSharp.GlyphInfo, GlyphInfo>(ReadOnlySpan<HarfBuzzSharp.GlyphInfo> infos).ToArray()
                      Positions = MemoryMarshal.Cast<HarfBuzzSharp.GlyphPosition, GlyphPosition>(ReadOnlySpan<HarfBuzzSharp.GlyphPosition> positions).ToArray()
                      GlyphName = name
                      Extents = extents
                      GlyphFromName = fromName
                      NominalGlyph = nominal })

/// <summary>The shaper of this implementation for the harness.</summary>
let shaper =
    { Name = "HarfBuzzSharp public API"
      Shapers = []
      FaceLoaders = []
      FontFuncs = []
      Shape = shape }
