module Farfbuzz.Tests.Glyph.GlyphOutlineTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>A 54-byte head table with a given upem and loca format.</summary>
/// <param name="upem">Units per em.</param>
/// <param name="indexToLocFormat">0 for short, 1 for long loca.</param>
/// <returns>The bytes.</returns>
let private head (upem: int) (indexToLocFormat: uint32) : byte array =
    Array.concat
        [ be32 0x00010000u; be32 0x00010000u; be32 0u; be32 0x5F0F3CF5u
          be16 0u; be16 (uint32 upem); be32 0u; be32 0u; be32 0u; be32 0u
          be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 indexToLocFormat; be16 0u ]

/// <summary>A version 0.5 maxp table.</summary>
/// <param name="glyphCount">The glyph count.</param>
/// <returns>The bytes.</returns>
let private maxp (glyphCount: int) : byte array =
    Array.concat [ be32 0x00005000u; be16 (uint32 glyphCount) ]

/// <summary>A 36-byte hea table with a long-metric count.</summary>
/// <param name="longMetricCount">The number of long metrics.</param>
/// <returns>The bytes.</returns>
let private hea (longMetricCount: int) : byte array =
    Array.concat
        [ be32 0x00010000u
          be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u
          be16 0u; be16 0u; be16 0u
          be16 0u; be16 0u; be16 0u; be16 0u
          be16 0u
          be16 (uint32 longMetricCount) ]

/// <summary>An hmtx/vmtx table from advance and bearing pairs.</summary>
/// <param name="metrics">The pairs.</param>
/// <returns>The bytes.</returns>
let private mtx (metrics: (int * int) array) : byte array =
    metrics |> Array.collect (fun (advance, bearing) -> Array.concat [ be16 (uint32 advance); be16 (uint32 (bearing &&& 0xFFFF)) ])

/// <summary>Builds a short loca table from byte offsets.</summary>
/// <param name="offsets">The byte offsets.</param>
/// <returns>The bytes.</returns>
let private shortLoca (offsets: int array) : byte array =
    offsets |> Array.collect (fun offset -> be16 (uint32 (offset / 2)))

/// <summary>The synthetic composite of four USE_MY_METRICS copies of a 65536-point simple glyph, from
/// <c>test/api/test-ot-metrics-tt-var.c:276-312</c>.</summary>
/// <returns>The face.</returns>
let private useMyMetricsFace () : Face =
    let glyf =
        Array.concat
            [ [| 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
              [| 0x02uy; 0x22uy; 0uy; 1uy; 0uy; 0uy |]
              [| 0x02uy; 0x22uy; 0uy; 1uy; 0uy; 0uy |]
              [| 0x02uy; 0x22uy; 0uy; 1uy; 0uy; 0uy |]
              [| 0x02uy; 0x02uy; 0uy; 1uy; 0uy; 0uy |]
              [| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
              [| 0xFFuy; 0xFFuy |]
              [| 0uy; 0uy |] ]
    let fvar =
        Array.concat
            [ be16 1u; be16 0u; be16 16u; be16 2u; be16 1u; be16 20u; be16 0u; be16 8u
              be32 (uint32 (Tag.make 'w' 'g' 'h' 't'))
              be32 (BitConverter.SingleToUInt32Bits 100.0f)
              be32 (BitConverter.SingleToUInt32Bits 400.0f)
              be32 (BitConverter.SingleToUInt32Bits 900.0f)
              be16 0u
              be16 256u ]
    let gvar =
        Array.concat
            [ be16 1u
              be16 0u
              be16 1u
              be16 0u
              be32 26u
              be16 2u
              be16 0u
              be32 26u
              [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |] ]
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000 0u
                  Tags.Maxp, maxp 2
                  Tags.Loca, shortLoca [| 0; 34; 48 |]
                  Tags.Glyf, glyf
                  Tags.Hhea, hea 2
                  Tags.Hmtx, mtx [| 600, 0; 600, 0 |]
                  Tags.Fvar, fvar
                  Tags.Gvar, gvar ])) 0u

/// <summary>A face whose glyph 1 is a simple glyph whose coordinate data is truncated.</summary>
/// <returns>The face.</returns>
let private truncatedFace () : Face =
    let glyf =
        Array.concat
            [ Array.zeroCreate 10
              [| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |] ]
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000 0u
                  Tags.Maxp, maxp 2
                  Tags.Loca, shortLoca [| 0; 10; 20 |]
                  Tags.Glyf, glyf
                  Tags.Hhea, hea 2
                  Tags.Hmtx, mtx [| 500, 0; 600, 30 |] ])) 0u

/// <summary>Builds a composite chain: glyph i references glyph i+1 until the last, which is empty.</summary>
/// <param name="glyphCount">The number of glyphs in the chain.</param>
/// <returns>The face.</returns>
let private chainFace (glyphCount: int) : Face =
    let compositeHeader = [| 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
    let empty = Array.zeroCreate 10
    let glyphs = ResizeArray<byte array>()
    let offsets = ResizeArray<int>()
    offsets.Add 0
    for i in 0 .. glyphCount - 1 do
        if i = glyphCount - 1 then
            glyphs.Add empty
            offsets.Add (offsets[offsets.Count - 1] + 10)
        else
            let gid = uint32 (i + 1)
            glyphs.Add (Array.concat [ compositeHeader; [| 0x00uy; 0x02uy; byte (gid >>> 8); byte gid; 0uy; 0uy |] ])
            offsets.Add (offsets[offsets.Count - 1] + 16)
    let glyf = Array.concat (glyphs |> Seq.toArray)
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000 0u
                  Tags.Maxp, maxp glyphCount
                  Tags.Loca, shortLoca (offsets |> Seq.toArray)
                  Tags.Glyf, glyf
                  Tags.Hhea, hea glyphCount
                  Tags.Hmtx, mtx (Array.init glyphCount (fun _ -> 500, 0)) ])) 0u

/// <summary>The P7.S10 glyph-outline unit tests.</summary>
[<Tests>]
let tests =
    testList "P7.S10" [
        test "last use_my_metrics copy wins in phantom-only descent" {
            let face = useMyMetricsFace ()
            let font = Font.create face
            Expect.equal (Font.hAdvance font 0u) 600 "advance at the default instance"
            let varied = Font.setVarCoordsNormalized (ImmutableArray.CreateRange [| 0x4000 |]) font
            Expect.equal (Font.hAdvance varied 0u) 600 "advance from the phantoms at a varied instance"
        }
        test "failure fallback returns half upem horizontally and upem vertically" {
            let face = truncatedFace ()
            let glyf = GlyfFace.load face
            let gvar = Gvar.load face
            let coords = NormalizedCoords.empty
            Expect.equal (GlyphOutline.advanceWithVariations glyf gvar coords face.Upem 1u false) (face.Upem / 2) "horizontal fallback"
            Expect.equal (GlyphOutline.advanceWithVariations glyf gvar coords face.Upem 1u true) face.Upem "vertical fallback"
            Expect.equal (GlyphOutline.vOriginWithVariations glyf gvar coords face.Upem 1u) (float32 face.Upem) "origin fallback"
        }
        test "depth limit: 65 glyphs reach depth 64, 66 glyphs fail" {
            let within = chainFace 65
            let glyfWithin = GlyfFace.load within
            let gvarWithin = Gvar.load within
            match GlyphOutline.points glyfWithin gvarWithin NormalizedCoords.empty 0u false with
            | ValueNone -> failtest "a 65-glyph chain must stay within the nesting limit"
            | ValueSome _ -> ()
            let over = chainFace 66
            let glyfOver = GlyfFace.load over
            let gvarOver = Gvar.load over
            match GlyphOutline.points glyfOver gvarOver NormalizedCoords.empty 0u false with
            | ValueNone -> ()
            | ValueSome _ -> failtest "a 66-glyph chain must exceed the nesting limit"
        }
        test "advances do not reload gvar" {
            let face = Face.create (read "test/api/fonts/SourceSansVariable-Roman-nohvar-41,C1.ttf") 0u
            let font = Font.setVarCoordsNormalized (ImmutableArray.CreateRange [| 0x4000 |]) (Font.create face)
            Font.hAdvance font 1u |> ignore
            let allocated = bytesAllocatedBy (fun () -> for _ in 1 .. 1000 do Font.hAdvance font 1u |> ignore)
            let perCall = allocated / 1000L
            Expect.isLessThan perCall 2400L $"bytes per advance: {perCall} (reloading gvar and the glyf accelerator adds ~200 more)"
        }
    ]
