module Farfbuzz.HarfBuzzSharp.Tests.GlyphQueryTests

open System
open System.Collections.Generic
open Expecto
open Farfbuzz.Diff
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>The scales the queries run at: the units per em, SKShaper's 512 and Avalonia-like 12 by 64 (not square).</summary>
let private scales (upem: int) = [ upem, upem; 512, 512; 12 * 64, 12 * 64 / 3 ]

/// <summary>The code points a face maps, read through the oracle.</summary>
/// <param name="face">The oracle face.</param>
/// <returns>The code points, ascending.</returns>
let private oracleCoverage (face: nativeint) =
    let set = Native.hb_set_create ()
    Native.hb_face_collect_unicodes(face, set)
    let found = List<uint32>()
    let mutable current = UInt32.MaxValue
    while Native.hb_set_next(set, &current) <> 0 do
        found.Add current
    Native.hb_set_destroy set
    found.ToArray()

/// <summary>An oracle font at a scale with the OpenType font functions.</summary>
/// <param name="face">The oracle face.</param>
/// <param name="x">The horizontal scale.</param>
/// <param name="y">The vertical scale.</param>
/// <returns>The font handle, for <c>hb_font_destroy</c>.</returns>
let private oracleFont (face: nativeint) (x: int) (y: int) =
    let font = Native.hb_font_create face
    Native.hb_ot_font_set_funcs font
    Native.hb_font_set_scale(font, x, y)
    font

/// <summary>The oracle's advances for a run of glyphs.</summary>
/// <param name="vertical">Whether the vertical advances are wanted.</param>
/// <param name="font">The oracle font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The advances.</returns>
let private oracleAdvances (vertical: bool) (font: nativeint) (glyphs: uint32 array) =
    let advances = Array.zeroCreate<int> glyphs.Length
    match vertical with
    | false -> Native.hb_font_get_glyph_h_advances(font, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    | true -> Native.hb_font_get_glyph_v_advances(font, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    advances

/// <summary>Runs a check on every face of the corpus and lists the faces that fail it.</summary>
/// <param name="check">The check on a font file, face index and oracle face; it returns a description of a difference, or none.</param>
/// <returns>The faces with their differences.</returns>
let private perFace (check: string -> int -> nativeint -> string option) =
    OracleFaces.all ()
    |> Seq.choose (fun (struct (font, index)) ->
        match check font.Path font.Index (OracleFaces.face font index) with
        | Some difference -> Some $"{font.Path}#{font.Index}: {difference}"
        | None -> None)
    |> Seq.truncate 10
    |> Seq.toList

/// <summary>The P10.S09 cases: glyph queries and metrics against the oracle, face by face.</summary>
[<Tests>]
let tests =
    testList "P10.S09" [
        testCase "nominal glyphs of every cmap code point equal the oracle" (fun () ->
            OracleSession.ensure ()
            let differences =
                perFace (fun path index oracleFace ->
                    use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path index)
                    let codepoints = oracleCoverage oracleFace
                    let oracle = oracleFont oracleFace 1000 1000
                    let expected = Array.zeroCreate<uint32> codepoints.Length
                    let oracleFound = Array.zeroCreate<bool> codepoints.Length
                    let one = Array.zeroCreate<uint32> 1
                    for position in 0 .. codepoints.Length - 1 do
                        oracleFound[position] <- Shims.farfbuzz_oracle_nominal_glyph_range(oracle, codepoints[position], 1u, one) = 1u
                        expected[position] <- (match oracleFound[position] with | true -> one[0] | false -> 0u)
                    Native.hb_font_destroy oracle
                    let results = codepoints |> Array.map (fun codepoint -> font.TryGetNominalGlyph codepoint)
                    let actual = results |> Array.map snd
                    let foundAgrees = Array.forall2 (fun (found, _) oracleSaw -> found = oracleSaw) results oracleFound
                    match actual = expected && foundAgrees with
                    | true -> None
                    | false ->
                        let first = [ 0 .. codepoints.Length - 1 ] |> List.find (fun position -> results[position] <> (oracleFound[position], expected[position]))
                        Some $"{codepoints.Length} code points; first difference U+{codepoints[first]:X}: facade {results[first]}, oracle {oracleFound[first]} {expected[first]}")
            Expect.isEmpty differences (String.concat "\n" differences))
        testCase "variation glyphs of every cmap 14 pair equal the oracle" (fun () ->
            OracleSession.ensure ()
            let mutable pairs = 0
            let differences =
                perFace (fun path index oracleFace ->
                    use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path index)
                    let oracle = oracleFont oracleFace 1000 1000
                    let selectors = Native.hb_set_create ()
                    Native.hb_face_collect_variation_selectors(oracleFace, selectors)
                    let collect (set: nativeint) =
                        let found = List<uint32>()
                        let mutable current = UInt32.MaxValue
                        while Native.hb_set_next(set, &current) <> 0 do
                            found.Add current
                        found.ToArray()
                    let mismatches =
                        [ for selector in collect selectors do
                            let unicodes = Native.hb_set_create ()
                            Native.hb_face_collect_variation_unicodes(oracleFace, selector, unicodes)
                            for codepoint in collect unicodes do
                                pairs <- pairs + 1
                                let mutable expected = 0u
                                let found = Native.hb_font_get_variation_glyph(oracle, codepoint, selector, &expected) <> 0
                                match font.TryGetVariationGlyph(codepoint, selector), font.TryGetGlyph(codepoint, selector) with
                                | (actualFound, actual), (generalFound, general) when actualFound = found && generalFound = found && (not found || (actual = expected && general = expected)) -> ()
                                | _ -> yield $"U+{codepoint:X} U+{selector:X}"
                            Native.hb_set_destroy unicodes ]
                    Native.hb_set_destroy selectors
                    Native.hb_font_destroy oracle
                    match mismatches with
                    | [] -> None
                    | first :: _ -> Some first)
            printfn $"P10.S09: {pairs} variation pairs compared"
            Expect.isGreaterThan pairs 10 "the corpus has variation sequences"
            Expect.isEmpty differences (String.concat "\n" differences))
        testCase "horizontal and vertical advances of every glyph equal the oracle at three scales" (fun () ->
            OracleSession.ensure ()
            let differences =
                perFace (fun path index oracleFace ->
                    use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path index)
                    let count = int (Native.hb_face_get_glyph_count oracleFace)
                    let glyphs = Array.init count uint32
                    scales (int (Native.hb_face_get_upem oracleFace))
                    |> List.tryPick (fun (x, y) ->
                        font.SetScale(x, y)
                        let oracle = oracleFont oracleFace x y
                        let horizontal = oracleAdvances false oracle glyphs
                        let vertical = oracleAdvances true oracle glyphs
                        Native.hb_font_destroy oracle
                        let singles = glyphs |> Array.map (fun glyph -> font.GetHorizontalGlyphAdvance glyph)
                        let ok = font.GetHorizontalGlyphAdvances(ReadOnlySpan<uint32> glyphs) = horizontal && singles = horizontal && font.GetVerticalGlyphAdvances(ReadOnlySpan<uint32> glyphs) = vertical && (glyphs |> Array.forall (fun glyph -> font.GetVerticalGlyphAdvance glyph = vertical[int glyph]))
                        match ok with
                        | true -> None
                        | false -> Some $"scale {x}x{y}"))
            Expect.isEmpty differences (String.concat "\n" differences))
        testCase "font extents, extents by direction and glyph extents equal the oracle at three scales" (fun () ->
            OracleSession.ensure ()
            let mutable unported = 0
            let mutable compared = 0
            let differences =
                perFace (fun path index oracleFace ->
                    use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path index)
                    let count = int (Native.hb_face_get_glyph_count oracleFace)
                    scales (int (Native.hb_face_get_upem oracleFace))
                    |> List.tryPick (fun (x, y) ->
                        font.SetScale(x, y)
                        let oracle = oracleFont oracleFace x y
                        let mutable horizontal = Unchecked.defaultof<Native.OracleFontExtents>
                        let mutable vertical = Unchecked.defaultof<Native.OracleFontExtents>
                        let hasHorizontal = Native.hb_font_get_h_extents(oracle, &horizontal) <> 0
                        let hasVertical = Native.hb_font_get_v_extents(oracle, &vertical) <> 0
                        let facadeH = font.TryGetHorizontalFontExtents()
                        let facadeV = font.TryGetVerticalFontExtents()
                        let heads (extents: HarfBuzzSharp.FontExtents) = extents.Ascender, extents.Descender, extents.LineGap
                        let fontOk =
                            fst facadeH = hasHorizontal && fst facadeV = hasVertical
                            && (not hasHorizontal || heads (snd facadeH) = (horizontal.Ascender, horizontal.Descender, horizontal.LineGap))
                            && (not hasVertical || heads (snd facadeV) = (vertical.Ascender, vertical.Descender, vertical.LineGap))
                        let forDirection = font.GetFontExtentsForDirection HarfBuzzSharp.Direction.LeftToRight
                        let directionOk = not hasHorizontal || heads forDirection = (horizontal.Ascender, horizontal.Descender, horizontal.LineGap)
                        let glyphMismatch =
                            [ 0 .. count - 1 ]
                            |> List.tryFind (fun glyph ->
                                let mutable expected = Unchecked.defaultof<Native.OracleGlyphExtents>
                                let found = Native.hb_font_get_glyph_extents(oracle, uint32 glyph, &expected) <> 0
                                try
                                    let actualFound, actual = font.TryGetGlyphExtents(uint32 glyph)
                                    compared <- compared + 1
                                    actualFound <> found || (found && (actual.XBearing, actual.YBearing, actual.Width, actual.Height) <> (expected.XBearing, expected.YBearing, expected.Width, expected.Height))
                                with :? NotSupportedException ->
                                    unported <- unported + 1
                                    false)
                        Native.hb_font_destroy oracle
                        match fontOk, directionOk, glyphMismatch with
                        | true, true, None -> None
                        | false, _, _ -> Some $"font extents at {x}x{y}"
                        | _, false, _ -> Some $"extents for direction at {x}x{y}"
                        | _, _, Some glyph -> Some $"glyph {glyph} extents at {x}x{y}"))
            printfn $"P10.S09: {compared} glyph extents compared, {unported} the core reports as unported outlines"
            Expect.isEmpty differences (String.concat "\n" differences))
        testCase "OpenType metrics positions equal the oracle for every tag at three scales, and the variations equal the core" (fun () ->
            OracleSession.ensure ()
            let tags = Enum.GetValues<HarfBuzzSharp.OpenTypeMetricsTag>()
            let differences =
                perFace (fun path index oracleFace ->
                    use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path index)
                    let core = Farfbuzz.Font.create (Farfbuzz.Face.create (ReadOnlyMemory (FacadeFonts.bytesOf path)) (uint32 index))
                    scales (int (Native.hb_face_get_upem oracleFace))
                    |> List.tryPick (fun (x, y) ->
                        font.SetScale(x, y)
                        let scaled = Farfbuzz.Font.withScale x y core
                        let oracle = oracleFont oracleFace x y
                        let bad =
                            tags
                            |> Array.tryFind (fun tag ->
                                let mutable expected = 0
                                let found = Native.hb_ot_metrics_get_position(oracle, uint32 tag, &expected) <> 0
                                let actualFound, actual = font.OpenTypeMetrics.TryGetPosition tag
                                let coreTag = LanguagePrimitives.UInt32WithMeasure<Farfbuzz.tag> (uint32 tag)
                                actualFound <> found
                                || (found && actual <> expected)
                                || font.OpenTypeMetrics.GetVariation tag <> Farfbuzz.Font.metricsVariation scaled coreTag
                                || font.OpenTypeMetrics.GetXVariation tag <> Farfbuzz.Font.metricsXVariation scaled coreTag
                                || font.OpenTypeMetrics.GetYVariation tag <> Farfbuzz.Font.metricsYVariation scaled coreTag)
                        Native.hb_font_destroy oracle
                        bad |> Option.map (fun tag -> $"{tag} at {x}x{y}")))
            Expect.isEmpty differences (String.concat "\n" differences))
        test "queries on a table-delegate face equal those on the file face" {
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            use fromFile = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
            use fromTables = new HarfBuzzSharp.Font(FacadeFonts.callbackFaceOf path 0)
            let glyphs = [| 0u .. 400u |]
            Expect.equal (fromTables.GetHorizontalGlyphAdvances(ReadOnlySpan<uint32> glyphs)) (fromFile.GetHorizontalGlyphAdvances(ReadOnlySpan<uint32> glyphs)) "advances"
            Expect.equal (glyphs |> Array.map (fun glyph -> fromTables.TryGetGlyphExtents glyph)) (glyphs |> Array.map (fun glyph -> fromFile.TryGetGlyphExtents glyph)) "extents, which need the outline tables"
            Expect.equal (fromTables.TryGetNominalGlyph 0x41u) (fromFile.TryGetNominalGlyph 0x41u) "nominal glyph"
        }
        test "the IntPtr overloads wrap the pointer for the call and the int overloads convert" {
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
            let glyphs = [| 36u; 37u; 38u |]
            let handle = Runtime.InteropServices.GCHandle.Alloc(glyphs, Runtime.InteropServices.GCHandleType.Pinned)
            let viaPointer = font.GetHorizontalGlyphAdvances(handle.AddrOfPinnedObject(), 3)
            handle.Free()
            Expect.equal viaPointer (font.GetHorizontalGlyphAdvances(ReadOnlySpan<uint32> glyphs)) "pointer and span"
            Expect.equal (font.TryGetNominalGlyph(0x41)) (font.TryGetNominalGlyph(0x41u)) "int and uint"
            Expect.equal (font.TryGetGlyph(0x41)) (font.TryGetGlyph(0x41u, 0u)) "TryGetGlyph arities"
        }
    ]
