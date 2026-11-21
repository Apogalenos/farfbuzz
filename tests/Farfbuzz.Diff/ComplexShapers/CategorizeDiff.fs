module Farfbuzz.Diff.ComplexShapers.CategorizeDiff

open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The 181 script values of the categorize domain: <c>Scripts.all</c> (179) plus Invalid and the
/// Myanmar Zawgyi pseudo-tag 'Qaag'.</summary>
let private scripts =
    seq {
        yield! Scripts.all
        yield Scripts.Invalid
        yield 0x51616167u<script> // HB_SCRIPT_MYANMAR_ZAWGYI
    }

/// <summary>The five directions of the categorize domain.</summary>
let private directions = [ Direction.Invalid; Direction.Ltr; Direction.Rtl; Direction.Ttb; Direction.Btt ]

/// <summary>The twelve GSUB script tags of the categorize domain.</summary>
let private gsubTags =
    [ Tags.Dflt
      Tags.Latn
      Tags.Mymr
      Tag.ofString "mym2"
      Tag.ofString "deva"
      Tag.ofString "dev2"
      Tag.ofString "dev3"
      Tag.ofString "knd3"
      Tag.ofString "arab"
      Tag.ofString "syrc"
      Tag.ofString "khmr"
      Tags.Empty ]

/// <summary>The shim index of a shaper kind, in <c>HB_OT_SHAPERS_IMPLEMENT_SHAPERS</c> order
/// (arabic, default, dumber, hangul, hebrew, indic, khmer, myanmar, myanmar_zawgyi, thai, use).</summary>
/// <param name="kind">The shaper kind.</param>
/// <returns>The index.</returns>
let private index (kind: ShaperKind) =
    match kind with
    | ArabicShaper -> 0u
    | DefaultShaper -> 1u
    | DumberShaper -> 2u
    | HangulShaper -> 3u
    | HebrewShaper -> 4u
    | IndicShaper -> 5u
    | KhmerShaper -> 6u
    | MyanmarShaper -> 7u
    | MyanmarZawgyiShaper -> 8u
    | ThaiShaper -> 9u
    | UseShaper -> 10u

/// <summary>The P6.S01 shaper-categorization differential.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P6.S01" [
        exhaustive "categorize" (seq {
            for script in scripts do
                for direction in directions do
                    for gsubScript in gsubTags do
                        yield struct (script, direction, gsubScript)
        })
            (fun (struct (script, direction, gsubScript)) -> Categorize.shaper script direction gsubScript |> index)
            (fun (struct (script, direction, gsubScript)) ->
                Shims.farfbuzz_oracle_shaper_categorize (uint32 script, Direction.value direction, uint32 gsubScript))
    ]
