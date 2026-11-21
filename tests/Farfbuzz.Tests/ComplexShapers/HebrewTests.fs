module Farfbuzz.Tests.ComplexShapers.HebrewTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>The dagesh presentation form of each letter U+05D0 to U+05EA, zero for a letter without one, copied from
/// <c>sDageshForms</c> (src/hb-ot-shaper-hebrew.cc:45-73).</summary>
let private dageshForms : uint32 list =
    [ 0xFB30u; 0xFB31u; 0xFB32u; 0xFB33u; 0xFB34u; 0xFB35u; 0xFB36u; 0x0000u; 0xFB38u
      0xFB39u; 0xFB3Au; 0xFB3Bu; 0xFB3Cu; 0x0000u; 0xFB3Eu; 0x0000u; 0xFB40u; 0xFB41u
      0x0000u; 0xFB43u; 0xFB44u; 0x0000u; 0xFB46u; 0xFB47u; 0xFB48u; 0xFB49u; 0xFB4Au ]

/// <summary>Every presentation-form pair of <c>compose_hebrew</c>'s <c>switch (b)</c> (src/hb-ot-shaper-hebrew.cc:84-157), as the
/// first code point, the second code point and the form.</summary>
let private presentationPairs : (uint32 * uint32 * uint32) list =
    [ 0x05D9u, 0x05B4u, 0xFB1Du
      0x05F2u, 0x05B7u, 0xFB1Fu
      0x05D0u, 0x05B7u, 0xFB2Eu
      0x05D0u, 0x05B8u, 0xFB2Fu
      0x05D5u, 0x05B9u, 0xFB4Bu
      0xFB2Au, 0x05BCu, 0xFB2Cu
      0xFB2Bu, 0x05BCu, 0xFB2Du
      0x05D1u, 0x05BFu, 0xFB4Cu
      0x05DBu, 0x05BFu, 0xFB4Du
      0x05E4u, 0x05BFu, 0xFB4Eu
      0x05E9u, 0x05C1u, 0xFB2Au
      0xFB49u, 0x05C1u, 0xFB2Cu
      0x05E9u, 0x05C2u, 0xFB2Bu
      0xFB49u, 0x05C2u, 0xFB2Du ]

/// <summary>Pairs no table of <c>compose_hebrew</c> composes.</summary>
let private uncomposable : (uint32 * uint32) list =
    [ 0x05D0u, 0x05B4u; 0x05D9u, 0x05B7u; 0x05D1u, 0x05B8u; 0x05D0u, 0x05B9u; 0x05D2u, 0x05BFu; 0x05D0u, 0x05C1u
      0x05D0u, 0x05C2u; 0x05D7u, 0x05BCu; 0x05E2u, 0x05BCu; 0x05EBu, 0x05BCu; 0x05CFu, 0x05BCu; 0xFB2Cu, 0x05BCu ]

/// <summary>The plan of the Hebrew shaper over a font with no layout tables.</summary>
let private hebrewPlan = planFor HebrewShaper Scripts.Hebrew

/// <summary>The plan of the Hebrew shaper over a font that positions marks with GPOS <c>mark</c>.</summary>
let private markedPlan = { hebrewPlan with HasGposMark = true }

/// <summary>A font that maps a letter, the dagesh and its presentation form.</summary>
let private dageshFont = fontMapping [ 0x05BCu, 2u; 0x05D0u, 1u; 0xFB30u, 3u ]

/// <summary>A font that maps a letter and the dagesh but no presentation form.</summary>
let private plainFont = fontMapping [ 0x05BCu, 2u; 0x05D0u, 1u ]

/// <summary>A font mapping the letter alef, the three marks of a reordered triple and a below mark.</summary>
let private markFont =
    fontMapping [ 0x05D0u, 1u; 0x05B7u, 2u; 0x05B8u, 3u; 0x05B0u, 4u; 0x05B4u, 5u; 0x05BDu, 6u; 0x0316u, 7u; 0x05C1u, 8u ]

/// <summary>The P6.S14 Hebrew shaper tests: the record, the composition tables, the mark reordering, the hooks through
/// the normalizer and the upstream conformance lines.</summary>
[<Tests>]
let tests =
    testList "P6.S14" [
        testCase "hebrew shaper record" (fun _ ->
            let record = Categorize.profile HebrewShaper
            Expect.equal record.Kind HebrewShaper "kind"
            Expect.equal record.GposTag (Tag.ofString "hebr") "gpos tag"
            Expect.equal record.Normalization NormalizationAuto "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksByGdefLate "zero-width marks"
            Expect.isTrue record.FallbackPosition "fallback position")
        testCase "a plan for the hebrew shaper is compiled with its GPOS tag" (fun _ ->
            Expect.equal hebrewPlan.Shaper.Kind HebrewShaper "kind"
            Expect.equal hebrewPlan.Shaper.GposTag (Tag.ofString "hebr") "gpos tag")
        testCase "every dagesh form of the letters U+05D0 to U+05EA" (fun _ ->
            dageshForms
            |> List.iteri (fun index form ->
                let letter = 0x05D0u + uint32 index
                let expected = match form with | 0u -> ValueNone | form -> ValueSome form
                Expect.equal (ShaperHooks.compose hebrewPlan letter 0x05BCu) expected $"U+{letter:X4} with dagesh"))
        testCase "every presentation form pair" (fun _ ->
            presentationPairs
            |> List.iter (fun (a, b, form) ->
                Expect.equal (ShaperHooks.compose hebrewPlan a b) (ValueSome form) $"U+{a:X4} U+{b:X4}"))
        testCase "pairs outside the tables do not compose" (fun _ ->
            uncomposable
            |> List.iter (fun (a, b) -> Expect.equal (ShaperHooks.compose hebrewPlan a b) ValueNone $"U+{a:X4} U+{b:X4}"))
        testCase "a plan with GPOS mark never uses the presentation forms" (fun _ ->
            presentationPairs
            |> List.iter (fun (a, b, _) -> Expect.equal (ShaperHooks.compose markedPlan a b) ValueNone $"U+{a:X4} U+{b:X4}"))
        testCase "Unicode composition comes first and ignores GPOS mark" (fun _ ->
            Expect.equal (ShaperHooks.compose hebrewPlan 0x61u 0x0301u) (ValueSome 0xE1u) "a with acute"
            Expect.equal (ShaperHooks.compose markedPlan 0x61u 0x0301u) (ValueSome 0xE1u) "a with acute, GPOS mark")
        testCase "the default and Thai shapers have no presentation forms" (fun _ ->
            Expect.equal (ShaperHooks.compose (planFor DefaultShaper Scripts.Hebrew) 0x05D0u 0x05BCu) ValueNone "default"
            Expect.equal (ShaperHooks.compose (planFor ThaiShaper Scripts.Thai) 0x05D0u 0x05BCu) ValueNone "thai")
        testCase "the normalizer composes a letter and a dagesh into the presentation form the font has" (fun _ ->
            let run = Normalize.run hebrewPlan dageshFont (runOf [| 0x05D0u; 0x05BCu |])
            Expect.equal (codepointsOf run) [| 0xFB30u |] "composed"
            Expect.equal [| for props in run.Props -> props.GlyphIndex |] [| 3u |] "glyph"
            Expect.equal (clustersOf run) [| 0u |] "cluster")
        testCase "the normalizer leaves a presentation form the font lacks decomposed" (fun _ ->
            let run = Normalize.run hebrewPlan plainFont (runOf [| 0x05D0u; 0x05BCu |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05BCu |] "not composed"
            Expect.equal (clustersOf run) [| 0u; 1u |] "clusters")
        testCase "the normalizer does not compose with GPOS mark although the font has the form" (fun _ ->
            let run = Normalize.run markedPlan dageshFont (runOf [| 0x05D0u; 0x05BCu |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05BCu |] "not composed")
        testCase "the default shaper's normalizer does not compose the presentation form" (fun _ ->
            let run = Normalize.run (planFor DefaultShaper Scripts.Hebrew) dageshFont (runOf [| 0x05D0u; 0x05BCu |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05BCu |] "not composed")
        testCase "an empty run normalizes to itself" (fun _ ->
            let run = Normalize.run hebrewPlan dageshFont (runOf [||])
            Expect.equal run.Infos.Length 0 "empty")
        testCase "a mark alone is left where it is" (fun _ ->
            let run = Normalize.run hebrewPlan markFont (runOf [| 0x05B7u |])
            Expect.equal (codepointsOf run) [| 0x05B7u |] "mark")
        testCase "patah, sheva and meteg have the last two marks swapped and their clusters merged" (fun _ ->
            let run = Normalize.run hebrewPlan markFont (runOf [| 0x05D0u; 0x05B7u; 0x05B0u; 0x05BDu |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05B7u; 0x05BDu; 0x05B0u |] "swapped"
            Expect.equal (clustersOf run) [| 0u; 1u; 2u; 2u |] "merged clusters")
        testCase "qamats, hiriq and a mark below are reordered the same way" (fun _ ->
            let run = Normalize.run hebrewPlan markFont (runOf [| 0x05D0u; 0x05B8u; 0x05B4u; 0x0316u |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05B8u; 0x0316u; 0x05B4u |] "swapped"
            Expect.equal (clustersOf run) [| 0u; 1u; 2u; 2u |] "merged clusters")
        testCase "the marks are sorted first, then reordered" (fun _ ->
            let run = Normalize.run hebrewPlan markFont (runOf [| 0x05D0u; 0x05BDu; 0x05B0u; 0x05B7u |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05B7u; 0x05BDu; 0x05B0u |] "sorted and swapped"
            Expect.equal (clustersOf run) [| 0u; 1u; 1u; 1u |] "clusters")
        testCase "two marks are never reordered" (fun _ ->
            let run = Normalize.run hebrewPlan markFont (runOf [| 0x05D0u; 0x05B7u; 0x05B0u |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05B7u; 0x05B0u |] "unchanged")
        testCase "marks that do not end in a meteg or a mark below stay" (fun _ ->
            let run = Normalize.run hebrewPlan markFont (runOf [| 0x05D0u; 0x05B7u; 0x05B0u; 0x05B4u |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05B7u; 0x05B0u; 0x05B4u |] "unchanged"
            Expect.equal (clustersOf run) [| 0u; 1u; 2u; 3u |] "clusters")
        testCase "the default shaper does not reorder the triple" (fun _ ->
            let run = Normalize.run (planFor DefaultShaper Scripts.Hebrew) markFont (runOf [| 0x05D0u; 0x05B7u; 0x05B0u; 0x05BDu |])
            Expect.equal (codepointsOf run) [| 0x05D0u; 0x05B7u; 0x05B0u; 0x05BDu |] "unchanged")
        Conformance.files "conformance" [ "in-house", "hebrew-diacritics.tests" ]
    ]
