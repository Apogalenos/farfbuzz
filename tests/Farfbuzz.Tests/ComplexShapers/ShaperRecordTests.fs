module Farfbuzz.Tests.ComplexShapers.ShaperRecordTests

open Expecto
open Farfbuzz

/// <summary>The P6.S01 default and dumber shaper-record cases, every field of <c>_hb_ot_shaper_default</c> and
/// <c>_hb_ot_shaper_dumber</c> (src/hb-ot-shaper-default.cc:34-71) checked against its cited value.</summary>
[<Tests>]
let tests =
    testList "P6.S01" [
        testCase "default shaper record" (fun _ ->
            let record = Categorize.profile DefaultShaper
            Expect.equal record.Kind DefaultShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationAuto "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksByGdefLate "zero-width marks"
            Expect.isTrue record.FallbackPosition "fallback position")
        testCase "dumber shaper record" (fun _ ->
            let record = Categorize.profile DumberShaper
            Expect.equal record.Kind DumberShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationAuto "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksNone "zero-width marks"
            Expect.isFalse record.FallbackPosition "fallback position")
    ]
