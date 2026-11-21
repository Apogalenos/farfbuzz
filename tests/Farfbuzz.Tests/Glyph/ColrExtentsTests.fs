module Farfbuzz.Tests.Glyph.ColrExtentsTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a valid 54-byte head table with the short loca format.</summary>
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

/// <summary>Builds a version 1.0 maxp table.</summary>
/// <param name="glyphCount">The number of glyphs.</param>
/// <returns>The bytes.</returns>
let private maxp (glyphCount: int) : byte array =
    Array.concat [ be32 0x00010000u; be16 (uint32 glyphCount); Array.zeroCreate 26 ]

/// <summary>Builds the glyf bytes of a rectangle with four on-curve points.</summary>
/// <param name="xMin">The left edge.</param>
/// <param name="yMin">The bottom edge.</param>
/// <param name="xMax">The right edge.</param>
/// <param name="yMax">The top edge.</param>
/// <returns>The glyph bytes.</returns>
let private rectangle (xMin: int) (yMin: int) (xMax: int) (yMax: int) : byte array =
    let word (value: int) = be16 (uint32 (value &&& 0xFFFF))
    Array.concat
        [ word 1
          word xMin; word yMin; word xMax; word yMax
          word 3
          word 0
          [| 1uy; 1uy; 1uy; 1uy |]
          word xMin; word (xMax - xMin); word 0; word (xMin - xMax)
          word yMin; word 0; word (yMax - yMin); word 0 ]

/// <summary>Builds the glyf and short loca tables of glyphs, padding each to an even length.</summary>
/// <param name="glyphs">The glyph bytes, empty for an empty glyph.</param>
/// <returns>The glyf and loca bytes.</returns>
let private glyfAndLoca (glyphs: byte array array) : byte array * byte array =
    let padded = glyphs |> Array.map (fun bytes -> match bytes.Length % 2 with | 0 -> bytes | _ -> Array.append bytes [| 0uy |])
    let offsets = padded |> Array.scan (fun offset bytes -> offset + bytes.Length) 0
    Array.concat padded, offsets |> Array.collect (fun offset -> be16 (uint32 (offset / 2)))

/// <summary>Builds a font with the rectangles of glyphs 1 and 2, glyph 0 and 3 empty, and a COLR table.</summary>
/// <param name="colr">The COLR table bytes.</param>
/// <returns>The face.</returns>
let private colrFace (colr: byte array) : Face =
    let glyf, loca = glyfAndLoca [| [||]; rectangle 0 0 500 700; rectangle 0 100 300 900; [||] |]
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000
                  Tags.Maxp, maxp 4
                  Tags.Loca, loca
                  Tags.Glyf, glyf
                  Tags.Colr, colr ])) 0u

/// <summary>Builds a version 0 COLR table with one base glyph, 3, and layers painted by the given glyphs.</summary>
/// <param name="layers">The layer glyphs.</param>
/// <returns>The table bytes.</returns>
let private colrV0 (layers: uint32 array) : byte array =
    Array.concat
        [ be16 0u
          be16 1u
          be32 14u
          be32 20u
          be16 (uint32 layers.Length)
          be16 3u; be16 0u; be16 (uint32 layers.Length)
          layers |> Array.collect (fun glyph -> Array.append (be16 glyph) (be16 0u)) ]

/// <summary>Builds a PaintSolid.</summary>
let private solid : byte array = Array.concat [ [| 2uy |]; be16 0u; be16 0x4000u ]

/// <summary>Builds a PaintGlyph over a child paint.</summary>
/// <param name="glyph">The glyph.</param>
/// <param name="child">The child paint.</param>
/// <returns>The paint bytes.</returns>
let private paintGlyph (glyph: uint32) (child: byte array) : byte array =
    Array.concat [ [| 10uy |]; be24 6u; be16 glyph; child ]

/// <summary>Builds a PaintTranslate over a child paint.</summary>
/// <param name="dx">The x shift.</param>
/// <param name="dy">The y shift.</param>
/// <param name="child">The child paint.</param>
/// <returns>The paint bytes.</returns>
let private paintTranslate (dx: int) (dy: int) (child: byte array) : byte array =
    Array.concat [ [| 14uy |]; be24 8u; be16 (uint32 (dx &&& 0xFFFF)); be16 (uint32 (dy &&& 0xFFFF)); child ]

/// <summary>Builds a PaintScale over a child paint.</summary>
/// <param name="scale">The scale as a 2.14 value.</param>
/// <param name="child">The child paint.</param>
/// <returns>The paint bytes.</returns>
let private paintScale (scale: uint32) (child: byte array) : byte array =
    Array.concat [ [| 16uy |]; be24 8u; be16 scale; be16 scale; child ]

/// <summary>Builds a PaintComposite of a source over a backdrop.</summary>
/// <param name="mode">The composite mode.</param>
/// <param name="source">The source paint.</param>
/// <param name="backdrop">The backdrop paint.</param>
/// <returns>The paint bytes.</returns>
let private paintComposite (mode: byte) (source: byte array) (backdrop: byte array) : byte array =
    Array.concat [ [| 32uy |]; be24 8u; [| mode |]; be24 (uint32 (8 + source.Length)); source; backdrop ]

/// <summary>Builds a clip list with one record for glyph 3 and a format 1 box.</summary>
/// <param name="xMin">The left edge.</param>
/// <param name="yMin">The bottom edge.</param>
/// <param name="xMax">The right edge.</param>
/// <param name="yMax">The top edge.</param>
/// <returns>The clip list bytes.</returns>
let private clipList (xMin: int) (yMin: int) (xMax: int) (yMax: int) : byte array =
    let word (value: int) = be16 (uint32 (value &&& 0xFFFF))
    Array.concat
        [ [| 1uy |]
          be32 1u
          be16 3u; be16 3u; be24 12u
          [| 1uy |]
          word xMin; word yMin; word xMax; word yMax ]

/// <summary>Builds a version 1 COLR table painting glyph 3 with a paint, and an optional clip list.</summary>
/// <param name="paint">The paint.</param>
/// <param name="clips">The clip list, empty for none.</param>
/// <returns>The table bytes.</returns>
let private colrV1 (paint: byte array) (clips: byte array) : byte array =
    let clipOffset = match clips.Length with | 0 -> 0u | _ -> uint32 (44 + paint.Length)
    Array.concat
        [ be16 1u
          be16 0u
          be32 34u
          be32 34u
          be16 0u
          be32 34u
          be32 0u
          be32 clipOffset
          be32 0u
          be32 0u
          be32 1u
          be16 3u; be32 10u
          paint
          clips ]

/// <summary>The extents of glyph 3 of a font at a scale.</summary>
/// <param name="colr">The COLR table.</param>
/// <param name="scale">The scale.</param>
/// <returns>The extents as bearing x, bearing y, width and height.</returns>
let private extentsAt (colr: byte array) (scale: int) =
    let font = Font.withScale scale scale (Font.create (colrFace colr))
    match Extents.glyph font 3u with
    | Ok (ValueSome extents) -> struct (extents.XBearing, extents.YBearing, extents.Width, extents.Height)
    | Ok ValueNone -> failtest "glyph 3 must have extents"
    | Error error -> failtest $"extents failed: %A{error}"

/// <summary>Generates a non-empty box with whole-number corners.</summary>
let private boxGen =
    gen {
        let! x = Gen.choose (-1000, 1000)
        let! y = Gen.choose (-1000, 1000)
        let! width = Gen.choose (1, 500)
        let! height = Gen.choose (1, 500)
        return { XMin = float32 x; YMin = float32 y; XMax = float32 (x + width); YMax = float32 (y + height) }
    }

/// <summary>The COLR extents tests, P7.COLR.</summary>
[<Tests>]
let tests =
    testList "P7.COLR" [
        test "a version 0 glyph is the union of its layers" {
            Expect.equal (extentsAt (colrV0 [| 1u; 2u |]) 1000) (struct (0, 900, 500, -900)) "the rectangles unite"
        }
        test "a version 0 glyph scales with the font" {
            Expect.equal (extentsAt (colrV0 [| 1u |]) 2000) (struct (0, 1400, 1000, -1400)) "the outline is drawn at the font's scale"
        }
        test "a version 0 glyph whose layers have no outline has zero extents" {
            Expect.equal (extentsAt (colrV0 [| 0u |]) 1000) (struct (0, 0, 0, 0)) "nothing is drawn"
        }
        test "a version 1 glyph filled with a solid colour is its outline" {
            Expect.equal (extentsAt (colrV1 (paintGlyph 1u solid) [||]) 1000) (struct (0, 700, 500, -700)) "the clip of the glyph"
        }
        test "a translation moves a version 1 glyph" {
            let paint = paintTranslate 50 -30 (paintGlyph 1u solid)
            Expect.equal (extentsAt (colrV1 paint [||]) 1000) (struct (50, 670, 500, -700)) "the box is translated"
        }
        test "a scale shrinks a version 1 glyph" {
            let paint = paintScale 0x2000u (paintGlyph 1u solid)
            Expect.equal (extentsAt (colrV1 paint [||]) 1000) (struct (0, 350, 250, -350)) "half the size"
        }
        test "a version 1 glyph at a larger scale is drawn at that scale" {
            Expect.equal (extentsAt (colrV1 (paintGlyph 1u solid) [||]) 2000) (struct (0, 1400, 1000, -1400)) "the font transform scales the clip"
        }
        test "a clip box is the extents of its glyph" {
            let colr = colrV1 (paintGlyph 1u solid) (clipList -10 -20 600 800)
            Expect.equal (extentsAt colr 1000) (struct (-10, 800, 610, -820)) "the clip box and not the paint"
        }
        test "a clip box is scaled to the font" {
            let colr = colrV1 (paintGlyph 1u solid) (clipList 0 0 100 200)
            Expect.equal (extentsAt colr 3000) (struct (0, 600, 300, -600)) "the clip box times three"
        }
        test "a composite source in a backdrop keeps their intersection" {
            let paint = paintComposite 5uy (paintGlyph 2u solid) (paintGlyph 1u solid)
            Expect.equal (extentsAt (colrV1 paint [||]) 1000) (struct (0, 700, 300, -600)) "the shared part of the rectangles"
        }
        test "a composite source over a backdrop keeps their union" {
            let paint = paintComposite 3uy (paintGlyph 2u solid) (paintGlyph 1u solid)
            Expect.equal (extentsAt (colrV1 paint [||]) 1000) (struct (0, 900, 500, -900)) "both rectangles"
        }
        test "an unknown composite mode clears the backdrop" {
            let paint = paintComposite 99uy (paintGlyph 2u solid) (paintGlyph 1u solid)
            Expect.equal (extentsAt (colrV1 paint [||]) 1000) (struct (0, 0, 0, 0)) "an emptied group has no extents"
        }
        test "a paint without a clip is unbounded and has zero extents" {
            Expect.equal (extentsAt (colrV1 solid [||]) 1000) (struct (0, 0, 0, 0)) "an unbounded glyph answers zeros"
        }
        test "a truncated table has no data" {
            let colr = colrV1 (paintGlyph 1u solid) [||]
            let face = colrFace (Array.sub colr 0 (colr.Length - 1))
            Expect.isFalse (Colr.hasData (Colr.load face)) "the last paint no longer fits"
        }
        test "an offset past the end has no data" {
            let colr = colrV1 (paintGlyph 1u solid) [||]
            let broken = Array.copy colr
            Array.blit (be32 0xFFFFu) 0 broken 14 4
            Expect.isFalse (Colr.hasData (Colr.load (colrFace broken))) "the base glyph list lies outside"
        }
        test "paints nest to 64 and no further" {
            let chain (translations: int) = Seq.fold (fun paint _ -> paintTranslate 1 1 paint) solid [ 1 .. translations ]
            Expect.isTrue (Colr.hasData (Colr.load (colrFace (colrV1 (chain 62) [||])))) "a chain of 63 paints passes"
            Expect.isFalse (Colr.hasData (Colr.load (colrFace (colrV1 (chain 64) [||])))) "a chain of 65 paints fails"
        }
        test "unsorted base glyph records are found as the binary search finds them" {
            let colr =
                Array.concat
                    [ be16 0u; be16 3u; be32 14u; be32 32u; be16 3u
                      be16 3u; be16 0u; be16 1u
                      be16 1u; be16 1u; be16 1u
                      be16 2u; be16 2u; be16 1u
                      be16 1u; be16 0u; be16 2u; be16 0u; be16 3u; be16 0u ]
            let face = colrFace colr
            let colr = Colr.load face
            Expect.isTrue (ValueOption.isNone (Colr.baseGlyphRecord colr 3u)) "the search never probes the first record"
            Expect.isTrue (ValueOption.isSome (Colr.baseGlyphRecord colr 1u)) "the middle record is probed first"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "the union of two boxes holds both" <|
            Prop.forAll (Arb.fromGen (Gen.zip boxGen boxGen)) (fun (first, second) ->
                let united = PaintGeometry.union first second
                united.XMin <= first.XMin && united.XMin <= second.XMin && united.XMax >= first.XMax && united.XMax >= second.XMax
                && united.YMin <= first.YMin && united.YMin <= second.YMin && united.YMax >= first.YMax && united.YMax >= second.YMax)
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "the intersection of two boxes lies in both" <|
            Prop.forAll (Arb.fromGen (Gen.zip boxGen boxGen)) (fun (first, second) ->
                let shared = PaintGeometry.intersect first second
                PaintGeometry.isVoid shared
                || (shared.XMin >= first.XMin && shared.XMin >= second.XMin && shared.XMax <= first.XMax && shared.XMax <= second.XMax
                    && shared.YMin >= first.YMin && shared.YMin >= second.YMin && shared.YMax <= first.YMax && shared.YMax <= second.YMax))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "the identity transform leaves a box alone and is neutral for multiplication" <|
            Prop.forAll (Arb.fromGen boxGen) (fun box ->
                let t = { XX = 2.0f; YX = 0.0f; XY = 0.0f; YY = 3.0f; X0 = 5.0f; Y0 = -7.0f }
                PaintGeometry.transformBox PaintGeometry.identity box = box
                && PaintGeometry.multiply PaintGeometry.identity t = t
                && PaintGeometry.multiply t PaintGeometry.identity = t)
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "transforming a box by a product equals transforming it twice for whole translations and scales" <|
            Prop.forAll (Arb.fromGen (gen {
                let! box = boxGen
                let! s = Gen.choose (1, 4)
                let! dx = Gen.choose (-50, 50)
                let! dy = Gen.choose (-50, 50)
                return struct (box, float32 s, float32 dx, float32 dy) })) (fun (struct (box, s, dx, dy)) ->
                let scale = { PaintGeometry.identity with XX = s; YY = s }
                let shift = { PaintGeometry.identity with X0 = dx; Y0 = dy }
                let once = PaintGeometry.transformBox (PaintGeometry.multiply shift scale) box
                let twice = PaintGeometry.transformBox shift (PaintGeometry.transformBox scale box)
                once = twice)
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "a box of whole numbers rounds to itself" <|
            Prop.forAll (Arb.fromGen boxGen) (fun box ->
                let extents = PaintGeometry.toGlyphExtents box
                extents.XBearing = int box.XMin && extents.YBearing = int box.YMax
                && extents.Width = int (box.XMax - box.XMin) && extents.Height = int (box.YMin - box.YMax))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "bounds: an unbounded clip is neutral for intersection and an empty group for union" <|
            Prop.forAll (Arb.fromGen boxGen) (fun box ->
                let bounded = PaintGeometry.boundsOf box
                let unbounded = { Status = Unbounded; Box = PaintGeometry.voidBox }
                let empty = { Status = Empty; Box = PaintGeometry.voidBox }
                PaintGeometry.intersectBounds bounded unbounded = bounded
                && PaintGeometry.unionBounds empty bounded = bounded
                && (PaintGeometry.intersectBounds unbounded bounded).Status = Bounded
                && (PaintGeometry.unionBounds bounded unbounded).Status = Unbounded)
    ]
