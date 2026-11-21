module Farfbuzz.Tests.Glyph.BitmapExtentsTests

open System
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a valid 54-byte head table.</summary>
/// <param name="upem">Units per em.</param>
/// <returns>The bytes.</returns>
let private head (upem: int) : byte array =
    Array.concat
        [ be32 0x00010000u
          be32 0x00010000u
          be32 0u
          be32 0x5F0F3CF5u
          be16 0u
          be16 (uint32 upem)
          be32 0u; be32 0u
          be32 0u; be32 0u
          be16 0u; be16 0u
          be16 0u; be16 0u
          be16 0u
          be16 0u
          be16 0u
          be16 0u
          be16 0u ]

/// <summary>Builds a version 0.5 maxp table.</summary>
/// <param name="glyphCount">The number of glyphs.</param>
/// <returns>The bytes.</returns>
let private maxp (glyphCount: int) : byte array =
    Array.concat [ be32 0x00005000u; be16 (uint32 glyphCount) ]

/// <summary>Builds a 29-byte PNG header with the given dimensions.</summary>
/// <param name="width">The pixel width.</param>
/// <param name="height">The pixel height.</param>
/// <returns>The bytes.</returns>
let private png (width: uint32) (height: uint32) : byte array =
    Array.concat
        [ [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]
          be32 13u
          be32 0x49484452u
          be32 width
          be32 height
          [| 8uy; 6uy; 0uy; 0uy; 0uy |] ]

/// <summary>Builds one SBIXGlyph: offsets, graphic type and data.</summary>
/// <param name="xOffset">The x offset.</param>
/// <param name="yOffset">The y offset.</param>
/// <param name="graphicType">The graphic type.</param>
/// <param name="data">The graphic data.</param>
/// <returns>The glyph bytes.</returns>
let private sbixGlyph (xOffset: int16) (yOffset: int16) (graphicType: Tag) (data: byte array) : byte array =
    Array.concat
        [ be16 (uint32 (int xOffset &&& 0xFFFF))
          be16 (uint32 (int yOffset &&& 0xFFFF))
          be32 (uint32 graphicType)
          data ]

/// <summary>Builds a PNG glyph.</summary>
/// <param name="xOffset">The x offset.</param>
/// <param name="yOffset">The y offset.</param>
/// <param name="width">The pixel width.</param>
/// <param name="height">The pixel height.</param>
/// <returns>The glyph bytes.</returns>
let private pngGlyph (xOffset: int16) (yOffset: int16) (width: uint32) (height: uint32) : byte array =
    sbixGlyph xOffset yOffset (Tag.ofString "png ") (png width height)

/// <summary>Builds a dupe glyph naming a target glyph id.</summary>
/// <param name="target">The named glyph id.</param>
/// <returns>The glyph bytes.</returns>
let private dupeGlyph (target: uint16) : byte array =
    sbixGlyph 0s 0s (Tag.ofString "dupe") (be16 (uint32 target))

/// <summary>Builds one strike: its ppem and its glyphs, with the image offsets computed.</summary>
/// <param name="ppem">The strike ppem.</param>
/// <param name="glyphs">The glyph bytes, one per glyph.</param>
/// <returns>The strike bytes.</returns>
let private strikeBytes (ppem: uint16) (glyphs: byte array array) : byte array =
    let offsets = Array.zeroCreate (glyphs.Length + 1)
    let mutable offset = 4 + 4 * (glyphs.Length + 1)
    for i in 0 .. glyphs.Length - 1 do
        offsets[i] <- offset
        offset <- offset + glyphs[i].Length
    offsets[glyphs.Length] <- offset
    Array.concat
        [ be16 (uint32 ppem)
          be16 0u
          offsets |> Array.collect (fun o -> be32 (uint32 o))
          glyphs |> Array.concat ]

/// <summary>Builds an sbix table from its strikes.</summary>
/// <param name="strikes">The strikes, each a ppem and its glyphs.</param>
/// <returns>The table bytes.</returns>
let private sbixTable (strikes: (uint16 * byte array array) array) : byte array =
    let bodies = strikes |> Array.map (fun (ppem, glyphs) -> strikeBytes ppem glyphs)
    let headerSize = 8 + 4 * bodies.Length
    let offsets = Array.zeroCreate bodies.Length
    let mutable cursor = headerSize
    for i in 0 .. bodies.Length - 1 do
        offsets[i] <- cursor
        cursor <- cursor + bodies[i].Length
    Array.concat
        [ be16 1u
          be16 0u
          be32 (uint32 bodies.Length)
          offsets |> Array.collect (fun o -> be32 (uint32 o))
          bodies |> Array.concat ]

/// <summary>Builds a face with an sbix table.</summary>
/// <param name="glyphCount">The face's glyph count.</param>
/// <param name="strikes">The strikes.</param>
/// <returns>The face.</returns>
let private sbixFace (glyphCount: int) (strikes: (uint16 * byte array array) array) : Face =
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Head, head 1000; Tags.Maxp, maxp glyphCount; Tags.Sbix, sbixTable strikes ])) 0u

/// <summary>Builds a face with one CBDT strike of the given image format and metrics.</summary>
/// <param name="imageFormat">The image format, 17 or 18.</param>
/// <param name="height">The glyph height.</param>
/// <param name="width">The glyph width.</param>
/// <param name="bearingX">The x bearing.</param>
/// <param name="bearingY">The y bearing.</param>
/// <returns>The face.</returns>
let private cbdtFace (imageFormat: uint16) (height: byte) (width: byte) (bearingX: sbyte) (bearingY: sbyte) : Face =
    let metricBlock =
        match imageFormat with
        | 17us -> [| height; width; byte bearingX; byte bearingY; 0uy |]
        | _ -> [| height; width; byte bearingX; byte bearingY; 0uy; 0uy; 0uy; 0uy |]
    let imageData = Array.concat [ metricBlock; be32 0u ]
    let cbdt = Array.concat [ be32 0x00030000u; imageData ]
    let indexSubtable =
        Array.concat [ be16 1u; be16 (uint32 imageFormat); be32 4u; be32 0u; be32 (uint32 imageData.Length) ]
    let indexSubtableArray = Array.concat [ be16 0u; be16 0u; be32 8u ]
    let sizeTable =
        Array.concat
            [ be32 56u
              be32 0u
              be32 1u
              be32 0u
              Array.zeroCreate 12
              Array.zeroCreate 12
              be16 0u
              be16 0u
              [| 100uy; 100uy; 32uy; 0uy |] ]
    let cblc = Array.concat [ be32 0x00030000u; be32 1u; sizeTable; indexSubtableArray; indexSubtable ]
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000
                  Tags.Maxp, maxp 1
                  Tags.Cblc, cblc
                  Tags.Cbdt, cbdt ])) 0u

/// <summary>The sbix bitmap extents tests, P7.S18.</summary>
[<Tests>]
let tests =
    testList "P7.S18" [
        test "sbix strike choice picks the smallest large enough else the largest" {
            let glyphs = [| pngGlyph 0s 0s 10u 20u |]
            let face = sbixFace 1 [| 50us, glyphs; 100us, glyphs; 200us, glyphs |]
            let sbix = Sbix.load face
            let ppemOf xPpem yPpem =
                match Sbix.glyphBitmap sbix xPpem yPpem 0u with
                | ValueSome (struct (_, ppem)) -> ppem
                | ValueNone -> -1
            Expect.equal (ppemOf 0u 0u) 200 "ppem 0 chooses the largest strike"
            Expect.equal (ppemOf 40u 40u) 50 "below the smallest strike chooses the smallest"
            Expect.equal (ppemOf 150u 150u) 200 "between two strikes chooses the smallest large enough"
            Expect.equal (ppemOf 300u 300u) 200 "above the largest strike chooses the largest"
        }
        test "sbix dupe chain of 8 resolves and of 9 does not" {
            let chain8 =
                [| for i in 0 .. 6 -> dupeGlyph (uint16 (i + 1))
                   yield dupeGlyph 8us
                   yield pngGlyph 0s 0s 10u 20u |]
            let face8 = sbixFace 9 [| 100us, chain8 |]
            match Sbix.glyphBitmap (Sbix.load face8) 100u 100u 0u with
            | ValueSome _ -> ()
            | ValueNone -> failtest "a dupe chain of 8 resolves"
            let chain9 =
                [| for i in 0 .. 8 -> dupeGlyph (uint16 (i + 1))
                   yield pngGlyph 0s 0s 10u 20u |]
            let face9 = sbixFace 10 [| 100us, chain9 |]
            match Sbix.glyphBitmap (Sbix.load face9) 100u 100u 0u with
            | ValueNone -> ()
            | ValueSome _ -> failtest "a dupe chain of 9 does not resolve"
        }
        test "an sbix PNG of width 65536 has no extents" {
            let face = sbixFace 1 [| 100us, [| pngGlyph 0s 0s 65536u 10u |] |]
            match Sbix.glyphBitmap (Sbix.load face) 100u 100u 0u with
            | ValueNone -> ()
            | ValueSome _ -> failtest "a 65536-pixel-wide PNG has no extents"
        }
        test "an sbix strike of ppem 0 has no extents" {
            let face = sbixFace 1 [| 0us, [| pngGlyph 0s 0s 10u 20u |] |]
            match Sbix.glyphBitmap (Sbix.load face) 0u 0u 0u with
            | ValueNone -> ()
            | ValueSome _ -> failtest "a zero-ppem strike has no extents"
        }
        test "CBDT format 17 reads the small metrics" {
            let face = cbdtFace 17us 20uy 10uy -2y 3y
            match Cbdt.glyphMetrics (Cbdt.load face) 0u 0u 0u with
            | ValueSome (struct (raw, ppemX, ppemY)) ->
                Expect.equal (raw.XBearing, raw.YBearing, raw.Width, raw.Height) (-2, 3, 10, -20) "format 17 metrics"
                Expect.equal (ppemX, ppemY) (100, 100) "the strike ppems"
            | ValueNone -> failtest "expected format 17 metrics"
        }
        test "CBDT format 18 reads the small metrics" {
            let face = cbdtFace 18us 20uy 10uy -2y 3y
            match Cbdt.glyphMetrics (Cbdt.load face) 0u 0u 0u with
            | ValueSome (struct (raw, ppemX, ppemY)) ->
                Expect.equal (raw.XBearing, raw.YBearing, raw.Width, raw.Height) (-2, 3, 10, -20) "format 18 metrics"
                Expect.equal (ppemX, ppemY) (100, 100) "the strike ppems"
            | ValueNone -> failtest "expected format 18 metrics"
        }
        test "sbix extents queries allocate nothing" {
            let face = sbixFace 1 [| 100us, [| pngGlyph 0s 0s 10u 20u |] |]
            let font = Font.create face
            let query () =
                for glyph in 0u .. 999u do
                    Extents.glyph font glyph |> ignore
            Expect.equal (bytesAllocatedAtBest query) 0L "1,000 queries allocate nothing"
        }
    ]
