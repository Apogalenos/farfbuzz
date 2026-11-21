module Farfbuzz.HarfBuzzSharp.Tests.FontFunctionsTests

open System
open Expecto
open Farfbuzz.Diff
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>A font's scale.</summary>
/// <param name="font">The font.</param>
/// <returns>The horizontal and vertical scale.</returns>
let private scaleOf (font: HarfBuzzSharp.Font) =
    let mutable x = 0
    let mutable y = 0
    font.GetScale(&x, &y)
    x, y

/// <summary>Font functions whose delegates answer from another, plain font.</summary>
/// <param name="source">The plain font the delegates ask.</param>
/// <returns>The functions.</returns>
let private wrapping (source: HarfBuzzSharp.Font) =
    let functions = new HarfBuzzSharp.FontFunctions()
    functions.SetNominalGlyphDelegate(HarfBuzzSharp.NominalGlyphDelegate(fun _ _ unicode glyph -> let found, value = source.TryGetNominalGlyph unicode in glyph <- value; found))
    functions.SetVariationGlyphDelegate(HarfBuzzSharp.VariationGlyphDelegate(fun _ _ unicode selector glyph -> let found, value = source.TryGetVariationGlyph(unicode, selector) in glyph <- value; found))
    functions.SetHorizontalGlyphAdvanceDelegate(HarfBuzzSharp.GlyphAdvanceDelegate(fun _ _ glyph -> source.GetHorizontalGlyphAdvance glyph))
    functions.SetVerticalGlyphAdvanceDelegate(HarfBuzzSharp.GlyphAdvanceDelegate(fun _ _ glyph -> source.GetVerticalGlyphAdvance glyph))
    functions.SetGlyphExtentsDelegate(HarfBuzzSharp.GlyphExtentsDelegate(fun _ _ glyph extents -> let found, value = source.TryGetGlyphExtents glyph in extents <- value; found))
    functions.SetHorizontalFontExtentsDelegate(HarfBuzzSharp.FontExtentsDelegate(fun _ _ extents -> let found, value = source.TryGetHorizontalFontExtents() in extents <- value; found))
    functions.SetVerticalFontExtentsDelegate(HarfBuzzSharp.FontExtentsDelegate(fun _ _ extents -> let found, value = source.TryGetVerticalFontExtents() in extents <- value; found))
    functions.SetGlyphNameDelegate(HarfBuzzSharp.GlyphNameDelegate(fun _ _ glyph name -> let found, value = source.TryGetGlyphName glyph in name <- value; found))
    functions.SetGlyphFromNameDelegate(HarfBuzzSharp.GlyphFromNameDelegate(fun _ _ name glyph -> let found, value = source.TryGetGlyphFromName name in glyph <- value; found))
    functions

/// <summary>The P10.S13 cases: font functions and the remaining font queries.</summary>
[<Tests>]
let tests =
    let roboto () = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
    testList "P10.S13" [
        testCase "delegates that return the defaults reproduce default shaping on 200 suite cases" (fun () ->
            let requests =
                ShapeTests.suiteRequests ()
                |> List.filter (fun (_, request) -> ValueOption.isNone (ShapeTests.unsupported request) && ValueOption.isNone request.Variations)
                |> List.indexed
                |> List.filter (fun (index, _) -> index % 27 = 0)
                |> List.map snd
                |> List.truncate 200
            Expect.isGreaterThan requests.Length 150 "cases"
            let differences =
                requests
                |> List.choose (fun (name, request) ->
                    let face = FacadeFonts.faceOf request.FontPath request.FaceIndex
                    use plain = new HarfBuzzSharp.Font(face)
                    let expected = ShapeTests.shapeUsing face plain request
                    use source = new HarfBuzzSharp.Font(face)
                    let sx, sy = scaleOf plain
                    source.SetScale(sx, sy)
                    use functions = wrapping source
                    use replaced = new HarfBuzzSharp.Font(face)
                    replaced.SetFontFunctions(functions, null, null)
                    let actual = try Ok (ShapeTests.shapeUsing face replaced request) with error -> Error error.Message
                    match actual = Ok expected with
                    | true -> None
                    | false -> Some name)
            Expect.isEmpty differences (String.concat "\n" (List.truncate 10 differences)))
        test "a nominal-glyph delegate that answers glyph 3 yields only glyph 3" {
            let face = FacadeFonts.faceOf (roboto ()) 0
            use font = new HarfBuzzSharp.Font(face)
            use functions = new HarfBuzzSharp.FontFunctions()
            functions.SetNominalGlyphDelegate(HarfBuzzSharp.NominalGlyphDelegate(fun _ _ _ glyph -> glyph <- 3u; true))
            font.SetFontFunctions functions
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.AddUtf16 "abc def"
            buffer.GuessSegmentProperties()
            font.Shape buffer
            Expect.equal (buffer.GlyphInfos |> Array.map (fun info -> info.Codepoint) |> Array.distinct) [| 3u |] "only glyph 3"
        }
        test "destroy callbacks run once, when the functions are replaced or the font is disposed" {
            let face = FacadeFonts.faceOf (roboto ()) 0
            let destroyed = ref 0
            let font = new HarfBuzzSharp.Font(face)
            use first = new HarfBuzzSharp.FontFunctions()
            use second = new HarfBuzzSharp.FontFunctions()
            font.SetFontFunctions(first, null, HarfBuzzSharp.ReleaseDelegate(fun () -> destroyed.Value <- destroyed.Value + 1))
            Expect.equal destroyed.Value 0 "nothing yet"
            font.SetFontFunctions(second, null, HarfBuzzSharp.ReleaseDelegate(fun () -> destroyed.Value <- destroyed.Value + 10))
            Expect.equal destroyed.Value 1 "replaced"
            font.Dispose()
            font.Dispose()
            Expect.equal destroyed.Value 11 "disposed once"
        }
        test "FontFunctions checks its arguments and freezes when a font takes it" {
            let functions = new HarfBuzzSharp.FontFunctions()
            Expect.throwsT<ArgumentNullException> (fun () -> functions.SetNominalGlyphDelegate(Unchecked.defaultof<HarfBuzzSharp.NominalGlyphDelegate>)) "null delegate"
            Expect.isFalse functions.IsImmutable "mutable"
            use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf (roboto ()) 0)
            font.SetFontFunctions functions
            Expect.isTrue functions.IsImmutable "frozen by the font"
            let error = try functions.SetNominalGlyphDelegate(HarfBuzzSharp.NominalGlyphDelegate(fun _ _ _ _ -> false)); None with :? InvalidOperationException as error -> Some error.Message
            Expect.equal error (Some "FontFunctions is immutable and can't be changed.") "immutable"
            Expect.isTrue HarfBuzzSharp.FontFunctions.Empty.IsImmutable "Empty is immutable"
            functions.Dispose()
        }
        test "origins, kerning, contour points, names and advances for a direction" {
            let named (path: string) =
                let font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
                match font.TryGetNominalGlyph 0x41u with
                | true, glyph -> fst (font.TryGetGlyphName glyph)
                | false, _ -> false
            let path = TestFonts.files () |> List.map snd |> List.find named
            let face = FacadeFonts.faceOf path 0
            use font = new HarfBuzzSharp.Font(face)
            let found, glyph = font.TryGetNominalGlyph 0x41u
            Expect.isTrue found "A"
            Expect.equal (font.TryGetHorizontalGlyphOrigin glyph) (true, 0, 0) "OpenType horizontal origin"
            Expect.equal (font.GetHorizontalGlyphKerning(glyph, glyph)) 0 "OpenType kerning"
            Expect.isFalse (let f, _, _ = font.TryGetGlyphContourPoint(glyph, 0u) in f) "no contour point from OpenType functions"
            let named, name = font.TryGetGlyphName glyph
            Expect.isTrue named "name"
            Expect.equal (font.TryGetGlyphFromName name) (true, glyph) "round trip"
            Expect.equal (font.GlyphToString glyph) name "GlyphToString"
            Expect.equal (font.TryGetGlyphFromString $"gid{glyph}") (true, glyph) "gid spelling"
            Expect.equal (font.GlyphToString 60000u) "gid60000" "unnamed glyph"
            let x, y = (let x, y = font.GetGlyphAdvanceForDirection(glyph, HarfBuzzSharp.Direction.LeftToRight) in x, y)
            Expect.equal (x, y) (font.GetHorizontalGlyphAdvance glyph, 0) "horizontal"
            let vx, vy = (let x, y = font.GetGlyphAdvanceForDirection(glyph, HarfBuzzSharp.Direction.TopToBottom) in x, y)
            Expect.equal (vx, vy) (0, font.GetVerticalGlyphAdvance glyph) "vertical"
            Expect.equal (font.GetGlyphAdvancesForDirection(ReadOnlySpan<uint32> [| glyph |], HarfBuzzSharp.Direction.LeftToRight)) [| font.GetHorizontalGlyphAdvance glyph |] "advances for a direction"
        }
        test "delegates for origins, kerning, contour points and extents are called with the font and its data" {
            let face = FacadeFonts.faceOf (roboto ()) 0
            use font = new HarfBuzzSharp.Font(face)
            use functions = new HarfBuzzSharp.FontFunctions()
            functions.SetHorizontalGlyphOriginDelegate(HarfBuzzSharp.GlyphOriginDelegate(fun _ data _ x y -> x <- (data :?> int); y <- 2; true))
            functions.SetVerticalGlyphOriginDelegate(HarfBuzzSharp.GlyphOriginDelegate(fun _ _ _ x y -> x <- 5; y <- 6; true))
            functions.SetHorizontalGlyphKerningDelegate(HarfBuzzSharp.GlyphKerningDelegate(fun _ _ l r -> int (l + r)))
            functions.SetGlyphContourPointDelegate(HarfBuzzSharp.GlyphContourPointDelegate(fun _ _ _ index x y -> x <- int index * 10; y <- 7; true))
            font.SetFontFunctions(functions, box 41, null)
            Expect.equal (font.TryGetHorizontalGlyphOrigin 9u) (true, 41, 2) "origin with the data"
            Expect.equal (font.GetHorizontalGlyphKerning(3u, 4u)) 7 "kerning"
            Expect.equal (font.TryGetGlyphContourPoint(1u, 3u)) (true, 30, 7) "contour point"
            Expect.equal (font.TryGetGlyphContourPointForOrigin(1u, 3u, HarfBuzzSharp.Direction.LeftToRight)) (true, 30 - 41, 7 - 2) "for the horizontal origin"
            Expect.equal (font.TryGetGlyphContourPointForOrigin(1u, 3u, HarfBuzzSharp.Direction.TopToBottom)) (true, 25, 1) "for the vertical origin"
        }
    ]
