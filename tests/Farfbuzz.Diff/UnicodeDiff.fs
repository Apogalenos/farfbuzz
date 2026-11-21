module Farfbuzz.Diff.UnicodeDiff

open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The categories in upstream order.</summary>
let private categories =
    [| GeneralCategory.Control; GeneralCategory.Format; GeneralCategory.Unassigned; GeneralCategory.PrivateUse
       GeneralCategory.Surrogate; GeneralCategory.LowercaseLetter; GeneralCategory.ModifierLetter
       GeneralCategory.OtherLetter; GeneralCategory.TitlecaseLetter; GeneralCategory.UppercaseLetter
       GeneralCategory.SpacingMark; GeneralCategory.EnclosingMark; GeneralCategory.NonSpacingMark
       GeneralCategory.DecimalNumber; GeneralCategory.LetterNumber; GeneralCategory.OtherNumber
       GeneralCategory.ConnectPunctuation; GeneralCategory.DashPunctuation; GeneralCategory.ClosePunctuation
       GeneralCategory.FinalPunctuation; GeneralCategory.InitialPunctuation; GeneralCategory.OtherPunctuation
       GeneralCategory.OpenPunctuation; GeneralCategory.CurrencySymbol; GeneralCategory.ModifierSymbol
       GeneralCategory.MathSymbol; GeneralCategory.OtherSymbol; GeneralCategory.LineSeparator
       GeneralCategory.ParagraphSeparator; GeneralCategory.SpaceSeparator |]

/// <summary>Every Unicode code point plus the out-of-range sentinels.</summary>
let private inputs =
    seq {
        for codepoint in 0 .. 0x10FFFF do yield uint32 codepoint
        yield 0x110000u
        yield 0x7FFFFFFFu
        yield 0xFFFFFFFFu
    }

/// <summary>The P3.S04 Unicode differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S04" [
        exhaustive "general category" inputs
            Unicode.generalCategory
            (fun codepoint -> categories[int (Native.farfbuzz_oracle_general_category codepoint)])
        exhaustive "combining class" inputs Unicode.combiningClass (fun codepoint -> byte (Native.farfbuzz_oracle_combining_class codepoint))
        exhaustive "mirroring" inputs Unicode.mirroring Native.farfbuzz_oracle_mirroring
        exhaustive "script" inputs (fun codepoint -> uint32 (Unicode.script codepoint)) Native.farfbuzz_oracle_script
        exhaustive "east Asian width" inputs Unicode.eastAsianWidth Native.farfbuzz_oracle_east_asian_width
    ]
