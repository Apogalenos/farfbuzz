module Farfbuzz.Tests.MetricsTests

open System
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Face creation, metrics tables, OtFace and the default font, P2.S02.</summary>
[<Tests>]
let tests =
    testList "P2.S02" [
        test "maxp and hmtx give advances up to the bearings count" {
            let maxp = [| 0uy; 0uy; 0x50uy; 0uy; 0uy; 5uy |]
            let hhea =
                Array.concat
                    [ be32 0x00010000u
                      be16 0x0200u
                      be16 0x0010u
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 0x0100u
                      Array.zeroCreate 16
                      be16 2u ]
            let hmtx =
                [| 0uy; 1uy; 0uy; 2uy; 0uy; 3uy; 0uy; 4uy; 0uy; 5uy
                   0uy; 6uy; 0uy; 7uy; 0uy; 8uy; 0uy; 9uy |]
            let file = sfnt 0x00010000u [ Tags.Maxp, maxp; Tags.Hhea, hhea; Tags.Hmtx, hmtx ]
            let font = Font.create (Face.create (ReadOnlyMemory file) 0u)
            let advances = [ for glyph in 0u .. 4u -> Font.hAdvance font glyph ]
            Expect.equal advances [ 1; 3; 3; 3; 3 ] "long metrics"
            Expect.equal (Font.hAdvance font 8u) 0 "beyond the bearings"
            Expect.equal (Font.hAdvance font 10u) 0 "beyond the face glyph count"
        }
        test "cpal-v0 horizontal ascender is 1000" {
            let font = Font.create (Face.create (read "test/api/fonts/cpal-v0.ttf") 0u)
            match Font.hExtents font with
            | ValueSome extents -> Expect.equal extents.Ascender 1000 "ascender"
            | ValueNone -> Expect.isTrue false "horizontal extents must exist"
        }
        test "Roboto has horizontal extents but no vertical metrics" {
            let font = Font.create (Face.create (read "test/api/fonts/Roboto-Regular.abc.ttf") 0u)
            match Font.hExtents font with
            | ValueSome extents ->
                Expect.isTrue (ValueOption.isNone (Font.vExtents font)) "no vertical metrics"
                Expect.equal (Font.hExtentsWithFallback font) extents "fallback-free extents"
            | ValueNone -> Expect.isTrue false "horizontal extents must exist"
        }
    ]

/// <summary>The P2.R03 metrics pairing cases.</summary>
[<Tests>]
let reviewTests =
    let maxp = [| 0uy; 0uy; 0x50uy; 0uy; 0uy; 5uy |]
    let hmtx =
        [| 0uy; 1uy; 0uy; 2uy; 0uy; 3uy; 0uy; 4uy; 0uy; 5uy
           0uy; 6uy; 0uy; 7uy; 0uy; 8uy; 0uy; 9uy |]
    let hheaBase =
        Array.concat
            [ be32 0x00010000u; be16 0x0200u; be16 0x0010u; be16 0u
              be16 0u; be16 0u; be16 0u; be16 0x0100u; Array.zeroCreate 16 ]
    testList "P2.R03" [
        test "a padded hhea does not become a 32-bit count" {
            let hhea = Array.concat [ hheaBase; be16 2u; be16 0u ]
            let file = sfnt 0x00010000u [ Tags.Maxp, maxp; Tags.Hhea, hhea; Tags.Hmtx, hmtx ]
            let font = Font.create (Face.create (ReadOnlyMemory file) 0u)
            Expect.equal (Font.hAdvance font 2u) 3 "glyph 2 uses the second long metric"
        }
        test "a short HHEA has no long metrics" {
            let hhea = Array.concat [ hheaBase; be16 5u ]
            let hmtx = [| 0uy; 7uy; 0uy; 8uy |]
            let file = sfnt 0x00010000u [ Tags.Maxp, maxp; Tags.UpperHhea, hhea; Tags.UpperHmtx, hmtx ]
            let font = Font.create (Face.create (ReadOnlyMemory file) 0u)
            Expect.equal (Font.hAdvance font 0u) 500 "falls back to upem / 2"
        }
        test "a 32-bit HHEA count clamps instead of going negative" {
            let hhea = Array.concat [ be32 0x00010000u; Array.zeroCreate 30; be32 0x80000000u ]
            let hmtx = [| 0uy; 7uy; 0uy; 8uy |]
            let file = sfnt 0x00010000u [ Tags.Maxp, maxp; Tags.UpperHhea, hhea; Tags.UpperHmtx, hmtx ]
            let font = Font.create (Face.create (ReadOnlyMemory file) 0u)
            Expect.equal (Font.hAdvance font 0u) 7 "first long metric"
        }
        test "the fallback branch uses the font scale" {
            let font = Font.create Face.empty
            Expect.isTrue (ValueOption.isNone (Font.hExtents font)) "no source extents"
            let extents = Font.hExtentsWithFallback font
            Expect.equal (extents.Ascender, extents.Descender, extents.LineGap) (800, -200, 0) "fallback"
        }
        test "fonts made from one face share its accelerators" {
            let face = Face.create (read "test/api/fonts/Roboto-Regular.abc.ttf") 0u
            let first = Font.create face
            let second = Font.create face
            Expect.isTrue (Object.ReferenceEquals(first.Ot, second.Ot)) "shared OtFace"
        }
        test "the batch vertical path matches the single-glyph path" {
            let font = Font.create (Face.create (read "test/api/fonts/Roboto-Regular.abc.ttf") 0u)
            let glyphs = Array.init (font.Face.GlyphCount + 1) uint32
            Expect.equal (Font.vAdvances font (ReadOnlySpan glyphs) |> Seq.toArray) (glyphs |> Array.map (Font.vAdvance font)) "same advances"
        }
    ]
