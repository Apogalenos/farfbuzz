module Farfbuzz.Tests.FontTests

open System
open Expecto
open Farfbuzz
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Font scale, ppem, ptem and synthetic state, P2.S03.</summary>
[<Tests>]
let tests =
    testList "P2.S03" [
        test "scale, ppem, ptem, slant and embolden round-trip" {
            let face = Face.create (read "test/api/fonts/Cantarell.A.otf") 0u
            let font = Font.create face
            Expect.equal font.XScale face.Upem "default x scale"
            Expect.equal font.YScale face.Upem "default y scale"
            Expect.equal font.XPpem 0u "default x ppem"
            Expect.equal font.YPpem 0u "default y ppem"
            Expect.equal font.Ptem 0f "default ptem"
            let scaled = Font.withScale 17 19 font
            Expect.equal (scaled.XScale, scaled.YScale) (17, 19) "scale"
            let pixed = Font.withPpem 17u 19u scaled
            Expect.equal (pixed.XPpem, pixed.YPpem) (17u, 19u) "ppem"
            let pointed = Font.withPtem 42f pixed
            Expect.equal pointed.Ptem 42f "ptem"
            let bold = Font.withSyntheticBold 0.05f -0.1f false pointed
            Expect.equal (bold.XEmbolden, bold.YEmbolden, bold.EmboldenInPlace) (0.05f, -0.1f, false) "bold"
            Expect.equal bold.XStrength 1 "x strength"
            Expect.equal bold.YStrength -2 "y strength"
            let slanted = Font.withSyntheticSlant 0.2f bold
            Expect.equal slanted.Slant 0.2f "slant"
            Expect.equal slanted.SlantXY (0.2f * 17f / 19f) "slant xy"
            Expect.isTrue (Object.ReferenceEquals(slanted, Font.withScale slanted.XScale slanted.YScale slanted)) "equal scale returns input"
            Expect.isTrue (Object.ReferenceEquals(slanted, Font.withPpem slanted.XPpem slanted.YPpem slanted)) "equal ppem returns input"
            Expect.isTrue (Object.ReferenceEquals(slanted, Font.withPtem slanted.Ptem slanted)) "equal ptem returns input"
            Expect.isTrue (Object.ReferenceEquals(slanted, Font.withSyntheticBold slanted.XEmbolden slanted.YEmbolden slanted.EmboldenInPlace slanted)) "equal bold returns input"
            Expect.isTrue (Object.ReferenceEquals(slanted, Font.withSyntheticSlant slanted.Slant slanted)) "equal slant returns input"
        }
        test "a negative scale mirrors a positive one" {
            let face = Face.create (read "test/api/fonts/Cantarell.A.otf") 0u
            let scale = int (64.0 * 1024.0 * 96.0 / 72.0)
            let positive = Font.withScale scale scale (Font.create face)
            let negative = Font.withScale -scale -scale (Font.create face)
            Expect.equal (Font.hAdvance positive 1u) (- Font.hAdvance negative 1u) "mirrored advance"
        }
    ]

/// <summary>The P2.R01 Int32.MinValue scale case, checked against the oracle.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R01" [
        test "Int32.MinValue scale with emboldening gives HarfBuzz's advances" {
            let face = Face.create (read "test/api/fonts/Cantarell.A.otf") 0u
            let font =
                Font.create face
                |> Font.withSyntheticBold 0.1f 0.1f false
                |> Font.withScale Int32.MinValue Int32.MinValue
            Expect.equal (Font.hAdvance font 1u) -1129576459 "horizontal"
            Expect.equal (Font.vAdvance font 1u) -1717986816 "vertical"
        }
    ]
