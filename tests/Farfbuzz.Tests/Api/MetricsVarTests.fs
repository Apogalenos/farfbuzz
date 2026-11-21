module Farfbuzz.Tests.MetricsVarTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Tests.UpstreamFiles

/// <summary>The x-height metric tag, <c>HB_OT_METRICS_TAG_X_HEIGHT</c>.</summary>
let private xHeight = Tag.make 'x' 'h' 'g' 't'

/// <summary>The horizontal-ascender metric tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_ASCENDER</c>.</summary>
let private horizontalAscender = Tag.make 'h' 'a' 's' 'c'

/// <summary>The P7.S15 port of the variation parts of <c>test/api/test-ot-metrics.c</c>.</summary>
[<Tests>]
let tests =
    testList "P7.S15" [
        test "ot metrics get var" {
            let face = Face.create (read "test/api/fonts/TestCFF2VF.otf") 0u
            let font = Font.create face
            Expect.equal (Metrics.getPosition font xHeight) (ValueSome 486) "x-height position" // test/api/test-ot-metrics.c:55-56
            Expect.equal (Font.metricsXVariation font horizontalAscender) 0 "horizontal-ascender x variation" // test/api/test-ot-metrics.c:57
            Expect.equal (Font.metricsYVariation font horizontalAscender) 0 "horizontal-ascender y variation" // test/api/test-ot-metrics.c:58
            Expect.equal (Font.metricsXVariation font xHeight) 0 "x-height variation at the default instance" // test/api/test-ot-metrics.c:59
            let varied = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 100.0f |]) font
            Expect.equal (Metrics.getPosition varied xHeight) (ValueSome 478) "x-height position at wght 100" // test/api/test-ot-metrics.c:62-63
            Expect.equal (Font.metricsXVariation varied horizontalAscender) 0 "horizontal-ascender x variation at wght 100" // test/api/test-ot-metrics.c:64
            Expect.equal (Font.metricsYVariation varied horizontalAscender) 0 "horizontal-ascender y variation at wght 100" // test/api/test-ot-metrics.c:65
            Expect.equal (Font.metricsXVariation varied xHeight) -8 "x-height variation at wght 100" // test/api/test-ot-metrics.c:66
        }
        test "ot metrics get no var x-height" {
            let face = Face.create (read "test/api/fonts/cpal-v0.ttf") 0u
            let font = Font.create face
            Expect.equal (Font.metricsXVariation font xHeight) 0 "a font without MVAR has no x-height variation" // test/api/test-ot-metrics.c:43
        }
    ]
