module Farfbuzz.Diff.UnicodePredicateDiff

open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The input domain and out-of-range sentinels.</summary>
let private inputs =
    seq {
        for codepoint in 0 .. 0x110000 do yield uint32 codepoint
        yield 0x7FFFFFFFu
        yield 0xFFFFFFFFu
    }

/// <summary>The numeric value of a space fallback.</summary>
/// <param name="space">The fallback.</param>
/// <returns>The upstream value.</returns>
let private spaceValue (space: SpaceFallback) =
    match space with
    | SpaceFallback.NotSpace -> 0
    | SpaceFallback.SpaceEm -> 1
    | SpaceFallback.SpaceEm2 -> 2
    | SpaceFallback.SpaceEm3 -> 3
    | SpaceFallback.SpaceEm4 -> 4
    | SpaceFallback.SpaceEm5 -> 5
    | SpaceFallback.SpaceEm6 -> 6
    | SpaceFallback.SpaceEm16 -> 16
    | SpaceFallback.Space4Em18 -> 17
    | SpaceFallback.Space -> 18
    | SpaceFallback.SpaceFigure -> 19
    | SpaceFallback.SpacePunctuation -> 20
    | SpaceFallback.SpaceNarrow -> 21

/// <summary>The P3.S06 Unicode predicate differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S06" [
        exhaustive "modified combining class" inputs
            Unicode.modifiedCombiningClass
            (fun codepoint -> byte (Native.farfbuzz_oracle_modified_ccc codepoint))
        exhaustive "default ignorable" inputs
            Unicode.isDefaultIgnorable
            (fun codepoint -> Native.farfbuzz_oracle_is_default_ignorable codepoint <> 0u)
        exhaustive "variation selector" inputs
            Unicode.isVariationSelector
            (fun codepoint -> Native.farfbuzz_oracle_is_variation_selector codepoint <> 0u)
        exhaustive "space fallback" inputs
            (Unicode.spaceFallbackType >> spaceValue)
            (fun codepoint -> int (Native.farfbuzz_oracle_space_fallback codepoint))
        exhaustive "vertical character" inputs
            Unicode.verticalCharFor
            Native.farfbuzz_oracle_vertical_char
        exhaustive "extended pictographic" inputs
            Unicode.isExtendedPictographic
            (fun codepoint -> Shims.farfbuzz_oracle_is_extended_pictographic codepoint <> 0u)
        exhaustive "unicode props" inputs
            (fun codepoint ->
                let struct (props, scratch) = UnicodeProps.compute codepoint
                struct (uint32 props, uint32 scratch))
            (fun codepoint ->
                let mutable scratch = 0u
                let props = Native.farfbuzz_oracle_unicode_props(codepoint, &scratch)
                struct (props, scratch))
    ]
