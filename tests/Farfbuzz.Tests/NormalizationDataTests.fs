module Farfbuzz.Tests.NormalizationDataTests

open Expecto
open Farfbuzz

/// <summary>The P3.S05 normalization data cases.</summary>
[<Tests>]
let tests =
    testList "P3.S05" [
        test "canonical composition and decomposition" {
            Expect.equal (Unicode.compose 0x41u 0x301u) (ValueSome 0xC1u) "A plus acute"
            Expect.equal (Unicode.decompose 0xC1u) (ValueSome (struct (0x41u, 0x301u))) "A acute"
            Expect.equal (Unicode.compose 0x0u 0x301u) ValueNone "zero"
            Expect.equal (Unicode.compose 0x1100u 0x1161u) (ValueSome 0xAC00u) "Hangul L V"
            Expect.equal (Unicode.decompose 0xAC00u) (ValueSome (struct (0x1100u, 0x1161u))) "Hangul LV"
            Expect.equal (Unicode.decompose 0xAC01u) (ValueSome (struct (0xAC00u, 0x11A8u))) "Hangul LVT"
        }
        test "compatibility decomposition is empty" {
            Expect.isTrue (Unicode.decomposeCompatibility 0xFB01u).IsEmpty "fi ligature"
        }
    ]
