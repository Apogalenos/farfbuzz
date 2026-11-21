module Farfbuzz.Tests.Layout.GdefTests

open System
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Builds a face from a single GDEF table.</summary>
/// <param name="gdef">The GDEF bytes.</param>
/// <returns>The face.</returns>
let private faceWithGdef (gdef: byte array) =
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tag.make 'G' 'D' 'E' 'F', gdef ])) 0u

/// <summary>A minimal GSUB or GPOS table of a given length whose version 1.0 sanitizes.</summary>
/// <param name="length">The table length.</param>
/// <returns>The table bytes.</returns>
let private validLayoutTable (length: int) =
    Array.concat [ [| 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]; Array.zeroCreate (length - 10) ]

/// <summary>A 442-byte GDEF with one glyph-class range, glyphs 0 to 5 in class 1.</summary>
/// <returns>The GDEF bytes.</returns>
let private blocklistGdef () =
    let classDef = [| 0x00uy; 0x02uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x05uy; 0x00uy; 0x01uy |]
    Array.concat [ [| 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x0Cuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]; classDef; Array.zeroCreate (442 - 12 - classDef.Length) ]

/// <summary>The NotoNastaliq face, scaled as <c>test-ot-ligature-carets.c</c> does.</summary>
/// <returns>The font.</returns>
let private nastaliq () =
    let face = Face.create (read "test/api/fonts/NotoNastaliqUrdu-Regular.ttf") 0u
    Font.withScale (face.Upem * 2) (face.Upem * 4) (Font.create face)

/// <summary>The P5.S02 GDEF cases.</summary>
[<Tests>]
let tests =
    testList "P5.S02" [
        test "test_ot_layout_get_ligature_carets_ot_gdef" {
            let font = nastaliq ()
            Expect.equal (List.ofSeq (LayoutApi.getLigatureCarets font Direction.Ltr 188u)) [] "188 has no carets"
            Expect.equal (List.ofSeq (LayoutApi.getLigatureCarets font Direction.Ltr 1020u)) [ 2718; 5438; 8156 ] "1020 has three carets"
            Expect.equal (List.ofSeq (LayoutApi.getLigatureCarets font Direction.Ltr 1021u)) [] "1021 has no carets"
            Expect.equal (List.ofSeq (LayoutApi.getLigatureCarets font Direction.Ltr 1022u)) [ 3530 ] "1022 has one caret"
            Expect.equal (List.ofSeq (LayoutApi.getLigatureCarets font Direction.Ltr 1023u)) [ 2352; 4706 ] "1023 has two carets"
        }
        test "test_ot_layout_get_ligature_carets_empty" {
            let face = Face.empty
            let font = Font.withScale (face.Upem * 2) (face.Upem * 4) (Font.create face)
            Expect.equal (List.ofSeq (LayoutApi.getLigatureCarets font Direction.Rtl 1024u)) [] "the empty face has no carets"
        }
        test "test_ot_layout_gdef_unsupported_version" {
            let face = faceWithGdef [| 0x87uy; 0x00uy; 0x00uy; 0x06uy; 0x30uy |]
            Expect.isFalse (Gdef.hasGlyphClasses (Gdef.load face)) "an unsupported major version has no classes"
        }
        test "test_ot_layout_gdef_1_4_offset2" {
            let gdef =
                [| 0x00uy; 0x01uy; 0x00uy; 0x04uy
                   0x00uy; 0x26uy
                   0x00uy; 0x00uy
                   0x00uy; 0x00uy
                   0x00uy; 0x00uy
                   0x00uy; 0x00uy
                   0x00uy; 0x00uy; 0x00uy; 0x00uy
                   0x00uy; 0x00uy; 0x00uy; 0x2Euy
                   0x00uy; 0x00uy; 0x00uy; 0x00uy
                   0x00uy; 0x00uy; 0x00uy; 0x00uy
                   0x00uy; 0x00uy; 0x00uy; 0x00uy
                   0x00uy; 0x00uy; 0x00uy; 0x00uy
                   0x00uy; 0x01uy; 0x00uy; 0x05uy
                   0x00uy; 0x01uy; 0x00uy; 0x01uy
                   0x00uy; 0x01uy; 0x00uy; 0x05uy
                   0x00uy; 0x01uy; 0x00uy; 0x03uy |]
            let gdef = Gdef.load (faceWithGdef gdef)
            Expect.isTrue (Gdef.hasGlyphClasses gdef) "1.4 with glyphClassDefOffset2 has classes"
            Expect.equal (Gdef.glyphClass gdef 5u) 3u "glyph 5 is a mark through the 32-bit class definition"
        }
        test "a blocklisted triple empties the GDEF" {
            let bytes =
                sfnt 0x00010000u
                    [ Tag.make 'G' 'D' 'E' 'F', blocklistGdef ()
                      Tag.make 'G' 'S' 'U' 'B', validLayoutTable 2874
                      Tag.make 'G' 'P' 'O' 'S', validLayoutTable 42038 ]
            let gdef = Gdef.load (Face.create (ReadOnlyMemory bytes) 0u)
            Expect.isFalse (Gdef.hasGlyphClasses gdef) "the blocklisted GDEF is emptied"
            Expect.equal (Gdef.glyphClass gdef 3u) 0u "the blocklisted class is zero"
        }
        test "a face whose GSUB fails its sanitizer is not blocklisted" {
            let bytes =
                sfnt 0x00010000u
                    [ Tag.make 'G' 'D' 'E' 'F', blocklistGdef ()
                      Tag.make 'G' 'S' 'U' 'B', Array.zeroCreate 2874
                      Tag.make 'G' 'P' 'O' 'S', validLayoutTable 42038 ]
            let gdef = Gdef.load (Face.create (ReadOnlyMemory bytes) 0u)
            Expect.isTrue (Gdef.hasGlyphClasses gdef) "a GSUB that fails sanitise is not blocklisted"
            Expect.equal (Gdef.glyphClass gdef 3u) 1u "the class survives"
        }
        test "an out-of-range mark filtering set index is cleared" {
            let gdef =
                [| 0x00uy; 0x01uy; 0x00uy; 0x02uy
                   0x00uy; 0x00uy
                   0x00uy; 0x00uy
                   0x00uy; 0x00uy
                   0x00uy; 0x00uy
                   0x00uy; 0x0Euy
                   0x00uy; 0x01uy
                   0x00uy; 0x01uy
                   0x00uy; 0x00uy; 0x00uy; 0x00uy |]
            let gdef = Gdef.load (faceWithGdef gdef)
            Expect.equal (Gdef.sanitizeLookupProps gdef 0x0010u) 0x0010u "an in-range set keeps the flag"
            Expect.equal (Gdef.sanitizeLookupProps gdef (0x0010u ||| (1u <<< 16))) 0x10000u "an out-of-range set clears the flag"
        }
    ]
