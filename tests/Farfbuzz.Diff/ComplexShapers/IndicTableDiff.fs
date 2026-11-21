module Farfbuzz.Diff.ComplexShapers.IndicTableDiff

open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>Every Unicode code point plus the out-of-range sentinels.</summary>
let private inputs =
    seq {
        for codepoint in 0 .. 0x10FFFF do yield uint32 codepoint
        yield 0x110000u
        yield 0x7FFFFFFFu
        yield 0xFFFFFFFFu
    }

/// <summary>The P6.S05 Indic category differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P6.S05" [
        exhaustive "indic categories" inputs
            (fun codepoint -> uint32 (IndicTable.categories codepoint))
            Shims.farfbuzz_oracle_indic_categories
    ]
