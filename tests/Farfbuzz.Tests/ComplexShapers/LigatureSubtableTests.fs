module Farfbuzz.Tests.ComplexShapers.LigatureSubtableTests

open Expecto
open Farfbuzz
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>The GSUB kernel's handling of a ligature lookup with several subtables, which the Indic differential found: a
/// subtable that covers the glyph but has no ligature that applies hands the glyph to the next subtable, as
/// <c>hb_ot_layout_lookup_accelerator_t::apply</c> does.</summary>
[<Tests>]
let tests =
    testList "P6.S17 ligature subtables" [
        testCase "a ligature lookup whose first subtable covers the glyph but ligates nothing goes on to its next subtable" (fun _ ->
            let font =
                fontWithGsub [ 0x61u, 1u; 0x62u, 2u; 0x63u, 3u ] 100u
                    (gsubOf "DFLT" [ "liga", [ 0u ] ] [ ligatureLookupOf [ ligatureSubtable [ 1u ] [ 3u ] 10u; ligatureSubtable [ 1u ] [ 2u ] 11u ] ])
            Expect.equal (codepointsOf (shaped font Scripts.Latin [| 0x61u; 0x63u |])) [| 10u |] "the first subtable"
            Expect.equal (codepointsOf (shaped font Scripts.Latin [| 0x61u; 0x62u |])) [| 11u |] "the second subtable, after the first covered the glyph and ligated nothing"
            Expect.equal (codepointsOf (shaped font Scripts.Latin [| 0x61u; 0x61u |])) [| 1u; 1u |] "neither")
    ]
