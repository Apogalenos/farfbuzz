module Farfbuzz.Tests.UnicodeTests

open Expecto
open Farfbuzz

/// <summary>The P3.S04 Unicode property cases.</summary>
[<Tests>]
let tests =
    testList "P3.S04" [
        test "general categories, classes and mirroring" {
            Expect.equal (Unicode.generalCategory 0x41u) GeneralCategory.UppercaseLetter "A"
            Expect.equal (Unicode.generalCategory 0x61u) GeneralCategory.LowercaseLetter "a"
            Expect.equal (Unicode.combiningClass 0x0301u) 230uy "acute"
            Expect.equal (Unicode.mirroring 0x28u) 0x29u "left parenthesis"
            Expect.equal (Unicode.mirroring 0x41u) 0x41u "no mirror"
        }
        test "scripts and east Asian width" {
            Expect.equal (Unicode.script 0x0627u) Scripts.Arabic "Arabic"
            Expect.equal (Unicode.script 0x41u) Scripts.Latin "Latin"
            Expect.equal (Unicode.eastAsianWidth 0x41u) 1u "default width"
            Expect.equal (Unicode.eastAsianWidth 0x110000u) 1u "outside width"
        }
    ]
