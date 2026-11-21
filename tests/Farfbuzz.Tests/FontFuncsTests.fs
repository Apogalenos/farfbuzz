module Farfbuzz.Tests.FontFuncsTests

open System
open System.Collections.Immutable
open System.IO
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper

/// <summary>Shapes one request the way <c>FarfbuzzShaper</c> does, with the font's functions replaced by those of a builder.</summary>
/// <param name="funcsOf">Makes the functions from the finished font.</param>
/// <param name="request">The request.</param>
/// <returns>The glyph infos and positions, or why the run is not comparable.</returns>
let private shapeWith (funcsOf: Font -> FontFuncs) (request: ShapeRequest) =
    let face = Face.create (ReadOnlyMemory (File.ReadAllBytes request.FontPath)) (uint32 request.FaceIndex)
    let upem = float face.Upem
    let struct (sizeX, sizeY) = match request.FontSize with | ValueSome size -> size | ValueNone -> struct (upem, upem)
    let struct (boldX, boldY) = request.Embolden
    let variations =
        match request.Variations with
        | ValueNone -> ImmutableArray<Variation>.Empty
        | ValueSome text -> text.Split([| ','; ' ' |]) |> Array.choose (fun item -> match Variation.parse item with | Ok variation -> Some variation | Error _ -> None) |> ImmutableArray.CreateRange
    let font =
        Font.create face
        |> Font.withPtem (float32 request.Ptem)
        |> Font.withSyntheticBold (float32 boldX) (float32 boldY) false
        |> Font.withSyntheticSlant (float32 request.Slant)
        |> Font.withScale (int (float32 sizeX)) (int (float32 sizeY))
        |> Font.setVariations variations
    let withFuncs = { font with Funcs = ValueSome (funcsOf font) }
    let run =
        match request.Before.Length > 0 with
        | false -> GlyphRun.empty
        | true -> GlyphRun.addCodepoints (ReadOnlySpan request.Before) request.Before.Length 0 GlyphRun.empty
    let run = GlyphRun.addCodepoints (ReadOnlySpan request.Text) 0 request.Text.Length run
    let run =
        match request.After.Length > 0 with
        | false -> run
        | true -> GlyphRun.addCodepoints (ReadOnlySpan request.After) 0 0 run
    let level =
        match request.ClusterLevel with
        | 1 -> ClusterLevel.MonotoneCharacters
        | 2 -> ClusterLevel.Characters
        | 3 -> ClusterLevel.Graphemes
        | _ -> ClusterLevel.MonotoneGraphemes
    let direction = match request.Direction with | ValueSome text -> Direction.ofString text | ValueNone -> Direction.Invalid
    let script = match request.Script with | ValueSome text -> Script.ofString text | ValueNone -> Scripts.Invalid
    let run =
        run
        |> GlyphRun.withSegment { Direction = direction; Script = script; Language = Language.ofString request.Language }
        |> GlyphRun.withFlags (LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.BufferFlags> (uint32 request.Flags))
        |> GlyphRun.withNotFoundVariationSelector request.NotFoundVariationSelectorGlyph
        |> GlyphRun.withClusterLevel level
        |> GlyphRun.guessSegmentProperties Language.cLocale
    let features =
        match request.Features with
        | ValueNone -> ImmutableArray<Feature>.Empty
        | ValueSome text -> text.Trim('"', '\'').Split([| ','; ' ' |]) |> Array.choose (fun item -> match Feature.parse item with | Ok feature -> Some feature | Error _ -> None) |> ImmutableArray.CreateRange
    Shape.shapeFull withFuncs features (match request.Shaper with | ValueSome text -> ValueSome (ImmutableArray.CreateRange (text.Split ',')) | ValueNone -> ValueNone) run |> Result.map (fun shaped -> shaped.Infos.AsSpan().ToArray(), shaped.Positions.AsSpan().ToArray())

/// <summary>The P10.S12 cases: replaceable font functions.</summary>
[<Tests>]
let tests =
    testList "P10.S12" [
        testCase "a record whose functions wrap the defaults shapes every suite run as the font without functions does" (fun () ->
            let requests =
                Suites.registered (RepositoryPaths.upstream ())
                |> List.collect (fun suite -> suite.Files)
                |> List.collect (fun entry -> Runner.plan FarfbuzzShaper.shaper entry)
                |> List.choose (fun spec ->
                    match spec.Kind with
                    | Runner.Execute (path, unicodes, options, extra, _) ->
                        match ShapeRequest.build path unicodes options extra with
                        | Ok request when request.SinglePar || not (Array.contains 0x0Au request.Text) -> Some (Runner.displayName spec.Id, request)
                        | Ok _ | Error _ -> None
                    | Runner.Skipped _ | Runner.Unrunnable _ -> None)
            let compared = ref 0
            let differences =
                requests
                |> List.choose (fun (name, request) ->
                    match FarfbuzzShaper.shaper.Shape request with
                    | Ok (Shaped plain) ->
                        compared.Value <- compared.Value + 1
                        match shapeWith FontFuncsOf.defaults request with
                        | Ok (infos, positions) when infos = plain.Infos && positions = plain.Positions -> None
                        | Ok _ -> Some $"{name}: glyphs differ"
                        | Error error -> Some $"{name}: {error}"
                    | Ok ShapingFailed | Error _ -> None)
            printfn $"P10.S12: {compared.Value} suite runs shaped with a default-wrapping record; {differences.Length} differ"
            Expect.isGreaterThan compared.Value 5000 "the suites"
            Expect.isEmpty (List.truncate 10 differences) (String.concat "\n" (List.truncate 10 differences)))
        test "a nominal-glyph function that answers glyph 3 for every code point yields only glyph 3" {
            let path = Path.Combine(RepositoryPaths.upstream (), "perf", "fonts", "Roboto-Regular.ttf")
            let request = (ShapeRequest.build path "U+0061,U+0062,U+0063,U+0020" [] [] |> Result.defaultWith failwith)
            let allThree (font: Font) = { FontFuncsOf.defaults font with Nominal = fun _ -> ValueSome 3u }
            match shapeWith allThree request with
            | Ok (infos, _) -> Expect.equal (infos |> Array.map (fun info -> info.Codepoint) |> Array.distinct) [| 3u |] "only glyph 3"
            | Error _ -> failtest "shaping failed"
        }
        test "an advance function decides the advances" {
            let path = Path.Combine(RepositoryPaths.upstream (), "perf", "fonts", "Roboto-Regular.ttf")
            let request = (ShapeRequest.build path "U+0061,U+0062" [] [] |> Result.defaultWith failwith)
            let fixedAdvance (font: Font) = { FontFuncsOf.defaults font with HAdvance = fun _ -> 777 }
            match shapeWith fixedAdvance request with
            | Ok (_, positions) -> Expect.equal (positions |> Array.map (fun position -> position.XAdvance)) [| 777; 777 |] "advances"
            | Error _ -> failtest "shaping failed"
        }
        test "a sub-font converts the parent's advances to its own scale and keeps the parent's glyphs" {
            let path = Path.Combine(RepositoryPaths.upstream (), "perf", "fonts", "Roboto-Regular.ttf")
            let face = Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u
            let parent = Font.create face
            let glyph = ValueOption.get (Font.nominalGlyph parent 0x61u)
            let sub = FontFuncsOf.subFont parent (parent.XScale * 2) (parent.YScale * 2)
            Expect.equal (Font.hAdvance sub glyph) (2 * Font.hAdvance parent glyph) "doubled advance"
            Expect.equal (Font.nominalGlyph sub 0x61u) (Font.nominalGlyph parent 0x61u) "same glyph"
            Expect.equal (ValueOption.get (Font.hExtents sub)).Ascender (2 * (ValueOption.get (Font.hExtents parent)).Ascender) "doubled ascender"
        }
    ]
