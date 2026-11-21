module Farfbuzz.Harness.FarfbuzzShaper

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open Farfbuzz
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper

/// <summary>Parses --features as hb-shape does: outer quotes stripped, invalid items dropped.</summary>
/// <param name="raw">The argument.</param>
/// <returns>The features.</returns>
let private features (raw: string voption) : ImmutableArray<Feature> =
    match raw with
    | ValueNone -> ImmutableArray.Empty
    | ValueSome text ->
        let unquoted =
            match text.Length >= 1 && text[0] = text[text.Length - 1] && (text[0] = '"' || text[0] = '\'') with
            | true -> text.Substring(1, max 0 (text.Length - 2))
            | false -> text
        match unquoted.Length with
        | 0 -> ImmutableArray.Empty
        | _ ->
            unquoted.Split([| ','; ' ' |])
            |> Array.choose (fun item -> match Feature.parse item with | Ok feature -> Some feature | Error _ -> None)
            |> ImmutableArray.CreateRange

/// <summary>Parses --variations as hb-shape does: split at every comma and space, invalid items dropped
/// (<c>util/font-options.hh:158-199</c>).</summary>
/// <param name="raw">The argument.</param>
/// <returns>The variations, in order.</returns>
let private variations (raw: string voption) : ImmutableArray<Variation> =
    match raw with
    | ValueNone -> ImmutableArray.Empty
    | ValueSome text ->
        text.Split([| ','; ' ' |])
        |> Array.choose (fun item -> match Variation.parse item with | Ok variation -> Some variation | Error _ -> None)
        |> ImmutableArray.CreateRange

/// <summary>Splits a --shaper list at commas, as hb-shape's shaper loop does.</summary>
/// <param name="raw">The argument.</param>
/// <returns>The shaper names, or none for the default list.</returns>
let private shapers (raw: string voption) : ImmutableArray<string> voption =
    match raw with
    | ValueNone -> ValueNone
    | ValueSome text -> ValueSome (ImmutableArray.CreateRange (text.Split [| ',' |]))

/// <summary>Maps the harness's cluster-level integer to the library's cluster level.</summary>
/// <param name="level">The integer.</param>
/// <returns>The cluster level.</returns>
let private clusterLevel (level: int) : ClusterLevel =
    match level with
    | 1 -> ClusterLevel.MonotoneCharacters
    | 2 -> ClusterLevel.Characters
    | 3 -> ClusterLevel.Graphemes
    | _ -> ClusterLevel.MonotoneGraphemes

/// <summary>Spells a shaping error for the harness's failure text.</summary>
/// <param name="error">The error.</param>
/// <returns>The text.</returns>
let private describeError (error: ShapeError) : string =
    match error with
    | OutlinesNotPorted source -> $"outlines not ported: %A{source}"
    | NoShaperSucceeded requested -> $"no shaper succeeded: %A{requested}"
    | VerifyFailed (check, _) -> $"verify failed: %A{check}"

/// <summary>Shapes one request, building a face and font of its own as hb-shape does.</summary>
/// <param name="faceOf">Loads a face by path and index.</param>
/// <param name="request">The request.</param>
/// <returns>The shaped run, the reason shaping failed, or why the request cannot run.</returns>
let private shapeOnce (faceOf: string -> int -> Face) (request: ShapeRequest) : Result<ShapeOutcome, string> =
    let face = faceOf request.FontPath request.FaceIndex
    let font = Font.create face
    let upem = float face.Upem
    let struct (sizeX, sizeY) = match request.FontSize with ValueSome size -> size | ValueNone -> struct (upem, upem)
    let struct (boldX, boldY) = request.Embolden
    let font =
        font
        |> Font.withPtem (float32 request.Ptem)
        |> Font.withSyntheticBold (float32 boldX) (float32 boldY) false
        |> Font.withSyntheticSlant (float32 request.Slant)
        |> Font.withScale (int (float32 sizeX)) (int (float32 sizeY))
        |> Font.setVariations (variations request.Variations)
    let run =
        match request.Before.Length > 0 with
        | false -> GlyphRun.empty
        | true -> GlyphRun.addCodepoints (ReadOnlySpan request.Before) request.Before.Length 0 GlyphRun.empty
    let run = GlyphRun.addCodepoints (ReadOnlySpan request.Text) 0 request.Text.Length run
    let run =
        match request.After.Length > 0 with
        | false -> run
        | true -> GlyphRun.addCodepoints (ReadOnlySpan request.After) 0 0 run
    let direction = match request.Direction with | ValueSome text -> Direction.ofString text | ValueNone -> Direction.Invalid
    let script = match request.Script with | ValueSome text -> Script.ofString text | ValueNone -> Scripts.Invalid
    let language = Language.ofString request.Language
    let flags : Farfbuzz.BufferFlags = LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.BufferFlags> (uint32 request.Flags)
    let run =
        run
        |> GlyphRun.withSegment { Direction = direction; Script = script; Language = language }
        |> GlyphRun.withFlags flags
        |> GlyphRun.withNotFoundVariationSelector request.NotFoundVariationSelectorGlyph
        |> GlyphRun.withClusterLevel (clusterLevel request.ClusterLevel)
        |> GlyphRun.guessSegmentProperties Language.cLocale
    match Shape.shapeFull font (features request.Features) (shapers request.Shaper) run with
    | Error error -> Error (describeError error)
    | Ok result ->
        let shapedRun =
            { Infos = result.Infos.AsSpan().ToArray()
              Positions = result.Positions.AsSpan().ToArray()
              GlyphName = fun glyph -> Font.glyphName font glyph
              Extents = fun glyph -> match Extents.glyph font glyph with | Ok extents -> extents | Error _ -> ValueNone
              GlyphFromName = fun name -> Font.glyphFromString font name
              NominalGlyph = fun codepoint -> Font.nominalGlyph font codepoint }
        Ok (Shaped shapedRun)

/// <summary>
/// Farfbuzz as a harness engine: the one place later phases edit to wire the shaper in. It offers the backends
/// HarfBuzz's own amalgamated build offers: the ot and fallback shapers, the ot face loader and ot font functions.
/// </summary>
let shaper : Shaper =
    let fileBytes = ConcurrentDictionary<string, byte[]>()
    let faces = ConcurrentDictionary<struct (string * int), Face>()
    let faceOf (path: string) (index: int) =
        faces.GetOrAdd(struct (path, index), fun _ ->
            let bytes = fileBytes.GetOrAdd(path, File.ReadAllBytes)
            Face.create (ReadOnlyMemory bytes) (uint32 index))
    { Name = "Farfbuzz"
      Shapers = [ "ot"; "fallback" ]
      FaceLoaders = [ "ot" ]
      FontFuncs = [ "ot" ]
      Shape = shapeOnce faceOf }
