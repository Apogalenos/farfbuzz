module Farfbuzz.Tests.CommonTests

open System
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Tests.Allocation

/// <summary>The P3.S02 common-type cases, ported from test/api/test-common.c.</summary>
[<Tests>]
let tests =
    testList "P3.S02" [
        test "the public glyph structs keep their native sizes" {
            Expect.equal (Marshal.SizeOf<GlyphInfo>()) 20 "GlyphInfo"
            Expect.equal (Marshal.SizeOf<GlyphPosition>()) 20 "GlyphPosition"
        }
        test "direction values and predicates" {
            Expect.equal (Direction.value Direction.Invalid) 0u "invalid"
            Expect.equal (Direction.value Direction.Ltr) 4u "ltr"
            Expect.equal (Direction.value Direction.Rtl) 5u "rtl"
            Expect.equal (Direction.value Direction.Ttb) 6u "ttb"
            Expect.equal (Direction.value Direction.Btt) 7u "btt"
            Expect.isTrue (Direction.isHorizontal Direction.Ltr) "h ltr"
            Expect.isTrue (Direction.isHorizontal Direction.Rtl) "h rtl"
            Expect.isFalse (Direction.isHorizontal Direction.Ttb) "h ttb"
            Expect.isTrue (Direction.isBackward Direction.Rtl) "b rtl"
            Expect.isTrue (Direction.isBackward Direction.Btt) "b btt"
            Expect.isFalse (Direction.isBackward Direction.Ltr) "b ltr"
            Expect.equal (Direction.reverse Direction.Ltr) Direction.Rtl "reverse ltr"
            Expect.equal (Direction.reverse Direction.Rtl) Direction.Ltr "reverse rtl"
            Expect.equal (Direction.reverse Direction.Ttb) Direction.Btt "reverse ttb"
            Expect.equal (Direction.reverse Direction.Btt) Direction.Ttb "reverse btt"
            Expect.equal (Direction.reverse Direction.Invalid) Direction.Invalid "reverse invalid"
        }
        test "direction strings are loose" {
            Expect.equal (Direction.ofString String.Empty) Direction.Invalid "empty"
            Expect.equal (Direction.ofString "x") Direction.Invalid "x"
            Expect.equal (Direction.ofString "r") Direction.Rtl "r"
            Expect.equal (Direction.ofString "RtL") Direction.Rtl "RtL"
            Expect.equal (Direction.ofString "right-to-left") Direction.Rtl "phrase"
            Expect.equal (Direction.ofString "ttb") Direction.Ttb "ttb"
            Expect.equal (Direction.ofString "btt") Direction.Btt "btt"
            Expect.equal (Direction.toString Direction.Ltr) "ltr" "ltr text"
            Expect.equal (Direction.toString Direction.Btt) "btt" "btt text"
            Expect.equal (Direction.toString Direction.Invalid) "invalid" "invalid text"
        }
        test "tag spellings" {
            Expect.equal (uint32 (Tag.ofString "aBcDe")) 0x61426344u "truncate"
            Expect.equal (uint32 (Tag.ofString "aBc")) 0x61426320u "pad"
            Expect.equal (uint32 (Tag.ofString "aB")) 0x61422020u "pad two"
            Expect.equal (uint32 (Tag.ofString "a")) 0x61202020u "pad one"
            Expect.equal (uint32 (Tag.ofString String.Empty)) 0u "empty"
        }
        test "script aliases, unknown scripts and pattern scripts" {
            Expect.equal (Script.ofString String.Empty) Scripts.Invalid "empty"
            Expect.equal (Script.ofString "arab") Scripts.Arabic "arab"
            Expect.equal (Script.ofString "ARAB") Scripts.Arabic "ARAB"
            Expect.equal (Script.ofString "Arabic") Scripts.Arabic "Arabic prefix"
            Expect.equal (Script.ofString "x123") Scripts.Unknown "unknown"
            Expect.equal (Script.ofString "wWyZ") (LanguagePrimitives.UInt32WithMeasure 0x5777797Au) "pattern"
            Expect.equal (Script.ofString "Hrkt") (LanguagePrimitives.UInt32WithMeasure 0x48726B74u) "Hrkt"
            Expect.equal (Script.ofString "Aran") Scripts.Arabic "Aran"
            Expect.equal (Script.ofString "Cyrs") Scripts.Cyrillic "Cyrs"
            Expect.equal (Script.ofString "Geok") Scripts.Georgian "Geok"
            Expect.equal (Script.ofString "Hans") Scripts.Han "Hans"
            Expect.equal (Script.ofString "Jamo") Scripts.Hangul "Jamo"
            Expect.equal (Script.ofString "Latf") Scripts.Latin "Latf"
            Expect.equal (Script.ofString "Syrj") Scripts.Syriac "Syrj"
        }
        test "horizontal directions" {
            Expect.equal (Script.horizontalDirection Scripts.Latin) Direction.Ltr "latin"
            Expect.equal (Script.horizontalDirection Scripts.Arabic) Direction.Rtl "arabic"
            Expect.equal (Script.horizontalDirection Scripts.OldItalic) Direction.Invalid "old italic"
            Expect.equal (Script.horizontalDirection (Script.ofString "wWyZ")) Direction.Ltr "pattern"
        }
        test "languages canonicalise and match" {
            let fa = Language.ofString "fa"
            let faIr = Language.ofString "fa_IR"
            Expect.equal faIr (Language.ofString "fa-ir") "case and separator"
            Expect.equal (Language.ofString "en_US.UTF-8") (Language.Bcp47 "en-us") "dot stops"
            Expect.equal (Language.ofString ".") (Language.Bcp47 String.Empty) "empty canonical"
            Expect.equal (Language.ofString String.Empty) Language.NoLanguage "empty input"
            Expect.equal (Language.toString fa) (ValueSome "fa") "string"
            Expect.equal (Language.toString Language.NoLanguage) ValueNone "no string"
            Expect.isTrue (Language.matches fa faIr) "specific"
            Expect.isFalse (Language.matches faIr fa) "not broader"
            Expect.isFalse (Language.matches fa (Language.ofString "en")) "different"
            Expect.isTrue (Language.matches Language.NoLanguage Language.NoLanguage) "both none"
            Expect.isFalse (Language.matches Language.NoLanguage fa) "one none"
            Expect.equal Language.cLocale (Language.Bcp47 "c") "c locale"
        }
        test "a leading NUL is no language" {
            Expect.equal (Language.ofString "\000c") Language.NoLanguage "leading NUL"
        }
        test "the generated script list has every script" {
            Expect.equal Scripts.all.Length 179 "count"
        }
        test "language strings truncate at 63 bytes" {
            let text = "en-" + String('a', 100)
            match Language.toString (Language.ofString text) with
            | ValueSome canonical -> Expect.equal canonical.Length 63 "length"
            | ValueNone -> Expect.isTrue false "missing language"
        }
    ]

/// <summary>The P3.R01 allocation case.</summary>
[<Tests>]
let reviewTests =
    testList "P3.R01" [
        test "horizontalDirection allocates nothing" {
            Expect.equal (bytesAllocatedBy (fun () -> Script.horizontalDirection Scripts.Arabic |> ignore)) 0L "bytes allocated"
        }
    ]
