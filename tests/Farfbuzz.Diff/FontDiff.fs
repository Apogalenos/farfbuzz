module Farfbuzz.Diff.FontDiff

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Creates a temporary oracle font with the requested state and destroys it after use.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="xScale">The horizontal scale.</param>
/// <param name="yScale">The vertical scale.</param>
/// <param name="xEmbolden">The horizontal strength.</param>
/// <param name="yEmbolden">The vertical strength.</param>
/// <param name="inPlace">Whether advances stay unchanged.</param>
/// <param name="action">The measurement.</param>
/// <returns>The measurement result.</returns>
let private withOracleFont font index xScale yScale xEmbolden yEmbolden inPlace action =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_synthetic_bold(handle, xEmbolden, yEmbolden, match inPlace with | true -> 1 | false -> 0)
    Native.hb_font_set_scale(handle, xScale, yScale)
    let result = action handle
    Native.hb_font_destroy handle
    result

/// <summary>Builds a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Reads one horizontal advance from the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The advance.</returns>
let private hAdvance (font: nativeint) (glyph: uint32) =
    let glyphs = [| glyph |]
    let advances = Array.zeroCreate 1
    Native.hb_font_get_glyph_h_advances(font, 1u, glyphs, 4u, advances, 4u)
    advances[0]

/// <summary>Reads one vertical advance from the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The advance.</returns>
let private vAdvance (font: nativeint) (glyph: uint32) =
    let glyphs = [| glyph |]
    let advances = Array.zeroCreate 1
    Native.hb_font_get_glyph_v_advances(font, 1u, glyphs, 4u, advances, 4u)
    advances[0]

/// <summary>Reads horizontal extents from the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <returns>Whether they exist and their values.</returns>
let private oracleHExtents (font: nativeint) =
    let mutable extents = Unchecked.defaultof<Native.OracleFontExtents>
    let present = Native.hb_font_get_h_extents(font, &extents)
    struct (present <> 0, extents.Ascender, extents.Descender, extents.LineGap)

/// <summary>Reads vertical extents from the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <returns>Whether they exist and their values.</returns>
let private oracleVExtents (font: nativeint) =
    let mutable extents = Unchecked.defaultof<Native.OracleFontExtents>
    let present = Native.hb_font_get_v_extents(font, &extents)
    struct (present <> 0, extents.Ascender, extents.Descender, extents.LineGap)

/// <summary>Reads Farfbuzz horizontal extents.</summary>
/// <param name="font">The Farfbuzz font.</param>
/// <returns>Whether they exist and their values.</returns>
let private farfbuzzHExtents font =
    match Font.hExtents font with
    | ValueSome extents -> struct (true, extents.Ascender, extents.Descender, extents.LineGap)
    | ValueNone -> struct (false, 0, 0, 0)

/// <summary>Reads Farfbuzz vertical extents.</summary>
/// <param name="font">The Farfbuzz font.</param>
/// <returns>Whether they exist and their values.</returns>
let private farfbuzzVExtents font =
    match Font.vExtents font with
    | ValueSome extents -> struct (true, extents.Ascender, extents.Descender, extents.LineGap)
    | ValueNone -> struct (false, 0, 0, 0)

/// <summary>Builds the glyph sequence from 0 through the face's last glyph.</summary>
/// <param name="count">The face glyph count.</param>
/// <returns>The glyphs.</returns>
let private glyphsOf (count: int) : uint32 array =
    Array.init (count + 1) uint32

/// <summary>The P2.S03 font-state differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let faceList = faces |> Seq.toList
    let scaleGen =
        Gen.frequency
            [ 8, Gen.choose (-4096, 4096)
              1, Gen.elements [ 1000; 1500; 2048; 4000; 1 <<< 24; -(1 <<< 24) ] ]
    let emboldenGen = Gen.elements [ 0f; 0.02f; 0.05f; 0.1f; -0.1f ]
    let caseGen =
        gen {
            let! struct (font, index) = Gen.elements faceList
            let glyphCount = int (Native.hb_face_get_glyph_count (OracleFaces.face font index))
            let! glyph = Gen.choose (0, glyphCount)
            let! xScale = scaleGen
            let! yScale = scaleGen
            let! xEmbolden = emboldenGen
            let! yEmbolden = emboldenGen
            let! inPlace = Gen.elements [ true; false ]
            return struct (font, index, uint32 glyph, xScale, yScale, xEmbolden, yEmbolden, inPlace)
        }
    let sizes =
        [ struct (1000, 1000); struct (1500, 1500); struct (4000, 4000); struct (2000, 2000)
          struct (1000, 1500); struct (-1000, -1000) ]
    let bolds = [ 0f; 0.1f ]
    let grid =
        [ for struct (font, index) in faces do
              for struct (xSize, ySize) in sizes do
                  for bold in bolds do
                      yield struct (font, index, xSize, ySize, bold) ]
    let reviewFace =
        { Path = System.IO.Path.Combine(RepositoryPaths.upstream (), "test", "api", "fonts", "Cantarell.A.otf")
          Suite = "api"
          Index = 0 }
    let p2s03 =
        testList "P2.S03" [
        sampled "P2.S03 scaled advances" caseGen 20_000
            (fun struct (font, index, glyph, xScale, yScale, xEmbolden, yEmbolden, inPlace) ->
                let farfbuzzFont =
                    Font.create (faceOf font index)
                    |> Font.withSyntheticBold xEmbolden yEmbolden inPlace
                    |> Font.withScale xScale yScale
                struct (Font.hAdvance farfbuzzFont glyph, Font.vAdvance farfbuzzFont glyph,
                        farfbuzzHExtents farfbuzzFont, farfbuzzVExtents farfbuzzFont))
            (fun struct (font, index, glyph, xScale, yScale, xEmbolden, yEmbolden, inPlace) ->
                withOracleFont font index xScale yScale xEmbolden yEmbolden inPlace (fun handle ->
                    struct (hAdvance handle glyph, vAdvance handle glyph,
                            oracleHExtents handle, oracleVExtents handle)))
        exhaustive "P2.S03 hb-shape option grid" grid
            (fun struct (font, index, xSize, ySize, bold) ->
                let face = faceOf font index
                let farfbuzzFont =
                    Font.create face
                    |> Font.withSyntheticBold bold bold false
                    |> Font.withScale xSize ySize
                let glyphs = glyphsOf face.GlyphCount
                struct (Font.hAdvances farfbuzzFont (ReadOnlySpan glyphs) |> Seq.toArray,
                        glyphs |> Array.map (Font.vAdvance farfbuzzFont),
                        farfbuzzHExtents farfbuzzFont, farfbuzzVExtents farfbuzzFont))
            (fun struct (font, index, xSize, ySize, bold) ->
                withOracleFont font index xSize ySize bold bold false (fun handle ->
                    let count = int (Native.hb_face_get_glyph_count (OracleFaces.face font index))
                    let glyphs = glyphsOf count
                    let h = Array.zeroCreate glyphs.Length
                    let v = Array.zeroCreate glyphs.Length
                    Native.hb_font_get_glyph_h_advances(handle, uint32 glyphs.Length, glyphs, 4u, h, 4u)
                    Native.hb_font_get_glyph_v_advances(handle, uint32 glyphs.Length, glyphs, 4u, v, 4u)
                    struct (h, v, oracleHExtents handle, oracleVExtents handle)))
    ]
    let p2r01 =
        testList "P2.R01" [
        exhaustive "P2.R01 Int32.MinValue scale with emboldening" [ struct (reviewFace, 0u) ]
            (fun struct (font, index) ->
                let farfbuzzFont =
                    Font.create (faceOf font index)
                    |> Font.withSyntheticBold 0.1f 0.1f false
                    |> Font.withScale Int32.MinValue Int32.MinValue
                struct (Font.hAdvance farfbuzzFont 1u, Font.vAdvance farfbuzzFont 1u,
                        farfbuzzHExtents farfbuzzFont, farfbuzzVExtents farfbuzzFont))
            (fun struct (font, index) ->
                withOracleFont font index Int32.MinValue Int32.MinValue 0.1f 0.1f false (fun handle ->
                    struct (hAdvance handle 1u, vAdvance handle 1u,
                            oracleHExtents handle, oracleVExtents handle)))
    ]
    testList "Font" [ p2s03; p2r01 ]
