module Farfbuzz.Tests.GlyphTests

open System.Runtime.CompilerServices
open Expecto
open Farfbuzz

/// <summary>Layout tests for the structs shared with HarfBuzz's C API, P1.Glyph.</summary>
[<Tests>]
let tests =
    testList "P1.Glyph" [
        test "GlyphInfo has the 20-byte layout of hb_glyph_info_t" { Expect.equal (Unsafe.SizeOf<GlyphInfo>()) 20 "size" }
        test "GlyphPosition has the 20-byte layout of hb_glyph_position_t" { Expect.equal (Unsafe.SizeOf<GlyphPosition>()) 20 "size" }
        test "Feature has the 16-byte layout of hb_feature_t" { Expect.equal (Unsafe.SizeOf<Feature>()) 16 "size" }
        test "Variation has the 8-byte layout of hb_variation_t" { Expect.equal (Unsafe.SizeOf<Variation>()) 8 "size" }
        test "GlyphExtents has the 16-byte layout of hb_glyph_extents_t" { Expect.equal (Unsafe.SizeOf<GlyphExtents>()) 16 "size" }
        test "glyph flag values equal HarfBuzz's" {
            Expect.equal (uint32 GlyphFlags.UnsafeToBreak, uint32 GlyphFlags.UnsafeToConcat, uint32 GlyphFlags.SafeToInsertTatweel, uint32 GlyphFlags.Defined) (1u, 2u, 4u, 7u) "flags"
        }
    ]
