module Farfbuzz.Tests.MetricsTtVarTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Unwraps a glyph's extents, failing the test where HarfBuzz would answer false.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private extents (font: Font) (glyph: uint32) : GlyphExtents =
    match Extents.glyph font glyph with
    | Ok (ValueSome extents) -> extents
    | Ok ValueNone -> failtest "glyph extents must exist"
    | Error _ -> failtest "glyph extents must exist"

/// <summary>The synthetic composite of four USE_MY_METRICS copies of a 65536-point simple glyph, from
/// <c>test/api/test-ot-metrics-tt-var.c:274-328</c>; maxp is absent, so the glyph count is set the way
/// <c>hb_face_set_glyph_count</c> sets it.</summary>
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
    let hhea =
        Array.concat
            [ be32 0x00010000u
              be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u
              be16 0u; be16 0u; be16 0u
              be16 0u; be16 0u; be16 0u; be16 0u
              be16 0u
              be16 2u ]
    let fvar =
        Array.concat
            [ be16 1u; be16 0u; be16 16u; be16 2u; be16 1u; be16 20u; be16 0u; be16 8u
              be32 0x77676874u // 'wght'
              be32 0x00640000u // min 100.0
              be32 0x01900000u // default 400.0
              be32 0x03840000u // max 900.0
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
    Face.withGlyphCount 2
        (Face.create
            (ReadOnlyMemory
                (sfnt 0x00010000u
                    [ Tags.Glyf, glyf
                      Tags.Loca, Array.concat [ be16 0u; be16 17u; be16 24u ]
                      Tags.Hhea, hhea
                      Tags.Hmtx, [| 0x02uy; 0x58uy; 0uy; 0uy; 0x02uy; 0x58uy; 0uy; 0uy |]
                      Tags.Fvar, fvar
                      Tags.Gvar, gvar ])) 0u)

/// <summary>The P7.S15 port of <c>test/api/test-ot-metrics-tt-var.c</c>: advances and extents of TrueType variable fonts.</summary>
[<Tests>]
let tests =
    testList "P7.S15" [
        test "extents tt var" {
            let face = Face.create (read "test/api/fonts/SourceSansVariable-Roman-nohvar-41,C1.ttf") 0u
            let font = Font.create face
            let defaultExtents = extents font 2u
            Expect.equal defaultExtents.XBearing 10 "x bearing at the default instance" // test/api/test-ot-metrics-tt-var.c:45
            Expect.equal defaultExtents.YBearing 846 "y bearing at the default instance" // test/api/test-ot-metrics-tt-var.c:46
            Expect.equal defaultExtents.Width 500 "width at the default instance" // test/api/test-ot-metrics-tt-var.c:47
            Expect.equal defaultExtents.Height -846 "height at the default instance" // test/api/test-ot-metrics-tt-var.c:48
            let varied = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 500.0f |]) font
            let variedExtents = extents varied 2u
            Expect.equal variedExtents.XBearing 0 "x bearing at wght 500" // test/api/test-ot-metrics-tt-var.c:55
            Expect.equal variedExtents.YBearing 874 "y bearing at wght 500" // test/api/test-ot-metrics-tt-var.c:56
            Expect.equal variedExtents.Width 551 "width at wght 500" // test/api/test-ot-metrics-tt-var.c:57
            Expect.equal variedExtents.Height -874 "height at wght 500" // test/api/test-ot-metrics-tt-var.c:58
        }
        test "advance tt var nohvar" {
            let face = Face.create (read "test/api/fonts/SourceSansVariable-Roman-nohvar-41,C1.ttf") 0u
            let font = Font.create face
            Expect.equal (Font.hAdvance font 2u) 520 "horizontal advance at the default instance" // test/api/test-ot-metrics-tt-var.c:75
            Expect.equal (Font.vAdvance font 2u) -1257 "vertical advance at the default instance" // test/api/test-ot-metrics-tt-var.c:81
            let varied = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 500.0f |]) font
            Expect.equal (Font.hAdvance varied 2u) 551 "horizontal advance at wght 500" // test/api/test-ot-metrics-tt-var.c:87
            Expect.equal (Font.vAdvance varied 2u) -1257 "vertical advance at wght 500" // test/api/test-ot-metrics-tt-var.c:93
        }
        test "advance tt var hvarvvar" {
            let face = Face.create (read "test/api/fonts/SourceSerifVariable-Roman-VVAR.abc.ttf") 0u
            let font = Font.create face
            Expect.equal (Font.hAdvance font 1u) 508 "horizontal advance at the default instance" // test/api/test-ot-metrics-tt-var.c:110
            Expect.equal (Font.vAdvance font 1u) -1000 "vertical advance at the default instance" // test/api/test-ot-metrics-tt-var.c:116
            let varied = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 700.0f |]) font
            Expect.equal (Font.hAdvance varied 1u) 531 "horizontal advance at wght 700" // test/api/test-ot-metrics-tt-var.c:122
            Expect.equal (Font.vAdvance varied 1u) -1012 "vertical advance at wght 700" // test/api/test-ot-metrics-tt-var.c:128
        }
        test "advance tt var anchor" {
            let face = Face.create (read "test/api/fonts/SourceSansVariable-Roman.anchor.ttf") 0u
            let font = Font.create face
            let defaultExtents = extents font 2u
            Expect.equal defaultExtents.XBearing 56 "x bearing at the default instance" // test/api/test-ot-metrics-tt-var.c:146
            Expect.equal defaultExtents.YBearing 672 "y bearing at the default instance" // test/api/test-ot-metrics-tt-var.c:147
            Expect.equal defaultExtents.Width 556 "width at the default instance" // test/api/test-ot-metrics-tt-var.c:148
            Expect.equal defaultExtents.Height -684 "height at the default instance" // test/api/test-ot-metrics-tt-var.c:149
            let varied = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 500.0f |]) font
            let variedExtents = extents varied 2u
            Expect.equal variedExtents.XBearing 50 "x bearing at wght 500" // test/api/test-ot-metrics-tt-var.c:156
            Expect.equal variedExtents.YBearing 667 "y bearing at wght 500" // test/api/test-ot-metrics-tt-var.c:157
            Expect.equal variedExtents.Width 592 "width at wght 500" // test/api/test-ot-metrics-tt-var.c:158
            Expect.equal variedExtents.Height -679 "height at wght 500" // test/api/test-ot-metrics-tt-var.c:159
        }
        test "extents tt var comp" {
            let face = Face.create (read "test/api/fonts/SourceSansVariable-Roman.modcomp.ttf") 0u
            let font = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 800.0f |]) (Font.create face)
            let cedilla = extents font 2u
            Expect.equal cedilla.XBearing 19 "Ccedilla x bearing" // test/api/test-ot-metrics-tt-var.c:181
            Expect.equal cedilla.YBearing 663 "Ccedilla y bearing" // test/api/test-ot-metrics-tt-var.c:182
            Expect.equal cedilla.Width 519 "Ccedilla width" // test/api/test-ot-metrics-tt-var.c:183
            Expect.equal cedilla.Height -895 "Ccedilla height" // test/api/test-ot-metrics-tt-var.c:184
            let acute = extents font 3u
            Expect.equal acute.XBearing 19 "Cacute x bearing" // test/api/test-ot-metrics-tt-var.c:189
            Expect.equal acute.YBearing 909 "Cacute y bearing" // test/api/test-ot-metrics-tt-var.c:190
            Expect.equal acute.Width 519 "Cacute width" // test/api/test-ot-metrics-tt-var.c:191
            Expect.equal acute.Height -921 "Cacute height" // test/api/test-ot-metrics-tt-var.c:192
            let caron = extents font 4u
            Expect.equal caron.XBearing 19 "Ccaron x bearing" // test/api/test-ot-metrics-tt-var.c:197
            Expect.equal caron.YBearing 866 "Ccaron y bearing" // test/api/test-ot-metrics-tt-var.c:198
            Expect.equal caron.Width 519 "Ccaron width" // test/api/test-ot-metrics-tt-var.c:199
            Expect.equal caron.Height -878 "Ccaron height" // test/api/test-ot-metrics-tt-var.c:200
        }
        test "advance tt var comp v" {
            let face = Face.create (read "test/api/fonts/SourceSansVariable-Roman.modcomp.ttf") 0u
            let font = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 800.0f |]) (Font.create face)
            Expect.equal (Font.vAdvance font 2u) -991 "vertical advance of the Ccedilla composite" // test/api/test-ot-metrics-tt-var.c:221
            match Origins.vOrigin font 2u with
            | Ok (struct (x, y)) ->
                Expect.equal x 292 "vertical origin x" // test/api/test-ot-metrics-tt-var.c:225
                Expect.equal y 1013 "vertical origin y" // test/api/test-ot-metrics-tt-var.c:226
            | Error _ -> failtest "vertical origin must exist"
        }
        test "advance tt var gvar infer" {
            let face = Face.create (read "test/api/fonts/TestGVAREight.ttf") 0u
            let font = Font.setVarCoordsNormalized (ImmutableArray.CreateRange [| 100; 0; 0; 0; 0; 0 |]) (Font.create face)
            match Extents.glyph font 4u with
            | Ok (ValueSome _) -> () // test/api/test-ot-metrics-tt-var.c:242
            | _ -> failtest "glyph 4 extents must exist after delta inference"
        }
        test "advance tt var hvar long deltas" {
            let face = Face.create (read "test/api/fonts/hvar-long-deltas.ttf") 0u
            let font = Font.create face
            Expect.equal (Font.hAdvance font 1u) 500 "advance at the default location" // test/api/test-ot-metrics-tt-var.c:263
            let varied = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 1.0f |]) font
            Expect.equal (Font.hAdvance varied 1u) -1 "the summed delta saturates to unsigned max" // test/api/test-ot-metrics-tt-var.c:268
        }
        test "advance tt var comp use my metrics" {
            let face = useMyMetricsFace ()
            let font = Font.create face
            Expect.equal (Font.hAdvance font 0u) 600 "advance at the default instance" // test/api/test-ot-metrics-tt-var.c:321
            let varied = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 500.0f |]) font
            Expect.equal (Font.hAdvance varied 0u) 600 "advance from the phantoms alone" // test/api/test-ot-metrics-tt-var.c:325
        }
        test "advance tt var comp use my metrics last" {
            let face = Face.create (read "test/api/fonts/a80ae54f8ad6577653d6bd370d6a81b37bdd7e79.ttf") 0u
            let font = Font.create face
            let defaultAdvances = [| for gid in 1u .. 6u -> Font.hAdvance font gid |]
            Expect.equal defaultAdvances [| 700; 300; 500; 500; 500; 800 |] "advances at the default instance" // test/api/test-ot-metrics-tt-var.c:346
            let varied = Font.setVariations (ImmutableArray.CreateRange [| { Tag = Tag.make 'w' 'g' 'h' 't'; Value = 900.0f } |]) font
            let advances = [| for gid in 1u .. 6u -> Font.hAdvance varied gid |]
            Expect.equal advances [| 700; 300; 300; 700; 700; 800 |] "the last use-my-metrics component wins at wght 900" // test/api/test-ot-metrics-tt-var.c:355
        }
    ]
