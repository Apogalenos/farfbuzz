module Farfbuzz.Tests.Shape.FallbackTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format-12 cmap font mapping each code point to a glyph.</summary>
/// <param name="pairs">The code point and glyph pairs.</param>
/// <returns>The font.</returns>
let private font12 (pairs: (uint32 * uint32) list) : Font =
    let groups = pairs |> List.map (fun (codepoint, glyph) -> codepoint, codepoint, glyph)
    let body = groups |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ]) |> Array.concat
    let subtable = Array.concat [ be16 12u; be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]
    let cmap = Array.concat [ be16 0u; be16 1u; be16 3u; be16 1u; be32 12u; subtable ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmap ])) 0u |> Font.create

/// <summary>Builds a 54-byte head table.</summary>
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

/// <summary>A face with a VORG table: default 880, glyph 2 at 600, glyph 5 at 700.</summary>
/// <returns>The face.</returns>
let private vorgFace () : Face =
    let records =
        [| 2u, 600s; 5u, 700s |]
        |> Array.collect (fun (glyph, y) -> Array.concat [ be16 glyph; be16 (uint32 (int y &&& 0xFFFF)) ])
    let vorg = Array.concat [ be32 0x00010000u; be16 (uint32 (880 &&& 0xFFFF)); be16 2u; records ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Head, head 1000; Tags.Maxp, maxp 10; Tags.Vorg, vorg ])) 0u

/// <summary>Shapes code points with the fallback shaper.</summary>
/// <param name="font">The font.</param>
/// <param name="direction">The direction.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The shaped run.</returns>
let private shapeFallback (font: Font) (direction: Direction) (codepoints: uint32 array) : GlyphRun =
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
        |> GlyphRun.withDirection direction
        |> TailCalls.settle
    match Shape.shapeFull font ImmutableArray.Empty (ValueSome (ImmutableArray.Create "fallback")) run with
    | Error error -> failtest $"expected Ok, got {error}"
    | Ok shaped -> shaped

/// <summary>The P4.S08 fallback shaper tests, including <c>test_shape_list</c>.</summary>
[<Tests>]
let tests =
    testList "P4.S08" [
        test "test_shape_list" {
            let shapers = Shape.listShapers ()
            Expect.isGreaterThan shapers.Length 1 "more than one shaper"
            Expect.equal shapers[shapers.Length - 1] "fallback" "fallback is last"
        }
        test "a default ignorable becomes the space glyph with zero advances" {
            let font = font12 [ 0x20u, 10u; 0x41u, 20u ]
            let shaped = shapeFallback font Direction.Ltr [| 0x41u; 0x00ADu |]
            Expect.equal shaped.Infos.Length 2 "two glyphs"
            Expect.equal shaped.Infos[0].Codepoint 20u "the letter maps to its glyph"
            Expect.equal shaped.Infos[1].Codepoint 10u "the ignorable becomes the space glyph"
            Expect.equal shaped.Positions[1].XAdvance 0 "x advance zeroed"
            Expect.equal shaped.Positions[1].YAdvance 0 "y advance zeroed"
            Expect.equal (shaped.Infos[0].Mask &&& uint32 GlyphFlags.Defined) 0u "glyph flags cleared"
        }
        test "a backward run is reversed" {
            let font = font12 [ 0x41u, 20u; 0x42u, 21u; 0x43u, 22u ]
            let shaped = shapeFallback font Direction.Rtl [| 0x41u; 0x42u; 0x43u |]
            Expect.equal shaped.Infos.Length 3 "three glyphs"
            Expect.equal [| for info in shaped.Infos -> info.Codepoint |] [| 22u; 21u; 20u |] "reversed glyph order"
        }
        test "vOrigins returns every glyph's vertical origin" {
            let font = Font.create (vorgFace ())
            match Origins.vOrigins font (ReadOnlySpan [| 0u; 2u; 5u |]) with
            | Ok origins ->
                Expect.equal origins.Length 3 "three origins"
                Expect.equal origins[0] (struct (250, 880)) "default origin"
                Expect.equal origins[1] (struct (250, 600)) "first override"
                Expect.equal origins[2] (struct (250, 700)) "second override"
            | Error error -> failtest $"expected origins, got {error}"
        }
    ]
