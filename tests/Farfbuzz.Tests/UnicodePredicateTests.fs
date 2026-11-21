module Farfbuzz.Tests.UnicodePredicateTests

open Expecto
open Farfbuzz

/// <summary>The P3.S06 Unicode predicate cases.</summary>
[<Tests>]
let tests =
    testList "P3.S06" [
        test "modified combining classes" {
            Expect.equal (Unicode.modifiedCombiningClass 0x1A60u) 254uy "sakot"
            Expect.equal (Unicode.modifiedCombiningClass 0x0FC6u) 254uy "padma"
            Expect.equal (Unicode.modifiedCombiningClass 0x0F39u) 127uy "tsa phru"
            Expect.equal (Unicode.modifiedCombiningClass 0x05B0u) 22uy "sheva"
            Expect.equal (Unicode.modifiedCombiningClass 0x0651u) 27uy "shadda"
        }
        test "default ignorables, variation selectors and spaces" {
            Expect.isTrue (Unicode.isDefaultIgnorable 0x200Du) "ZWJ"
            Expect.isFalse (Unicode.isDefaultIgnorable 0x115Fu) "Hangul filler excluded"
            Expect.isTrue (Unicode.isVariationSelector 0xFE0Fu) "VS16"
            Expect.isTrue (Unicode.isVariationSelector 0xE0100u) "VS17"
            Expect.equal (Unicode.spaceFallbackType 0x2002u) SpaceFallback.SpaceEm2 "en space"
            Expect.equal (Unicode.spaceFallbackType 0x3000u) SpaceFallback.SpaceEm "ideographic"
        }
        test "vertical forms and emoji" {
            Expect.equal (Unicode.verticalCharFor 0x3001u) 0xFE11u "comma"
            Expect.equal (Unicode.verticalCharFor 0x41u) 0x41u "unchanged"
            Expect.isTrue (Unicode.isExtendedPictographic 0x1F600u) "grinning"
            Expect.isFalse (Unicode.isExtendedPictographic 0x41u) "A"
        }
        test "unicode properties" {
            Expect.equal (UnicodeProps.compute 0x200Du) (struct (0x0121us, ScratchFlags.HasDefaultIgnorables)) "ZWJ"
            Expect.equal (UnicodeProps.compute 0x034Fu) (struct (0x00ECus, ScratchFlags.HasDefaultIgnorables ||| ScratchFlags.HasCgj ||| ScratchFlags.HasContinuations)) "CGJ"
            Expect.equal (UnicodeProps.compute 0x0301u) (struct (0xE68Cus, ScratchFlags.HasContinuations)) "acute"
            Expect.equal (UnicodeProps.compute 0x180Bu) (struct (0x00ECus, ScratchFlags.HasDefaultIgnorables ||| ScratchFlags.HasContinuations)) "FVS1"
            Expect.equal (UnicodeProps.compute 0xE0041u) (struct (0x0061us, ScratchFlags.HasDefaultIgnorables)) "tag A"
        }
    ]

/// <summary>The PERF6 predicate case: the direct bit test of <see cref="UnicodeProps.isUnicodeMark"/> equals the
/// general-category check for every packed property value.</summary>
[<Tests>]
let perf6PredicateTests =
    testList "PERF6" [
        test "isUnicodeMark equals the general-category check for every property value" {
            for value in 0 .. 0xFFFF do
                let props = { Unchecked.defaultof<GlyphProps> with UnicodeProps = uint16 value }
                let fast = UnicodeProps.isUnicodeMark props
                let reference =
                    match UnicodeProps.generalCategory props with
                    | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark | GeneralCategory.NonSpacingMark -> true
                    | _ -> false
                Expect.equal fast reference $"value 0x{value:X4}"
        }
        test "the Latin property table equals compute for U+0000 to U+00FF" {
            for value in 0 .. 255 do
                Expect.equal (UnicodeProps.latinTable[value]) (UnicodeProps.compute (uint32 value)) $"U+{value:X4}"
        }
    ]
