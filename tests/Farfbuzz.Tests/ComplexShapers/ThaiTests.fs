module Farfbuzz.Tests.ComplexShapers.ThaiTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>The Windows PUA forms of the Thai PUA shaping, the <c>win_pua</c> column of every table of
/// <c>thai_pua_shape</c> (src/hb-ot-shaper-thai.cc:98-134).</summary>
let private windowsForms : uint32 list =
    [ 0xF70Au; 0xF70Bu; 0xF70Cu; 0xF70Du; 0xF70Eu; 0xF718u; 0xF719u; 0xF71Au
      0xF705u; 0xF706u; 0xF707u; 0xF708u; 0xF709u
      0xF713u; 0xF714u; 0xF715u; 0xF716u; 0xF717u; 0xF710u; 0xF701u; 0xF702u; 0xF703u; 0xF704u; 0xF712u; 0xF711u
      0xF70Fu; 0xF700u ]

/// <summary>The Mac PUA forms, the <c>mac_pua</c> column of the same tables.</summary>
let private macForms : uint32 list =
    [ 0xF88Bu; 0xF88Eu; 0xF891u; 0xF894u; 0xF897u; 0xF89Bu; 0xF89Cu; 0xF89Du
      0xF88Cu; 0xF88Fu; 0xF892u; 0xF895u; 0xF898u
      0xF88Au; 0xF88Du; 0xF890u; 0xF893u; 0xF896u; 0xF884u; 0xF885u; 0xF886u; 0xF887u; 0xF888u; 0xF889u; 0xF899u
      0xF89Au; 0xF89Eu ]

/// <summary>A font that maps every Windows PUA form.</summary>
let private windowsFont = fontMapping (windowsForms |> List.mapi (fun index form -> form, uint32 (index + 1)))

/// <summary>A font that maps every Mac PUA form and no Windows one.</summary>
let private macFont = fontMapping (macForms |> List.mapi (fun index form -> form, uint32 (index + 1)))

/// <summary>A font that maps both families.</summary>
let private bothFont = fontMapping ((windowsForms @ macForms) |> List.sort |> List.mapi (fun index form -> form, uint32 (index + 1)))

/// <summary>A font that maps no PUA form.</summary>
let private bareFont = fontMapping [ 0x0E01u, 1u ]

/// <summary>The plan of the Thai shaper for Thai text in a font with no GSUB script.</summary>
let private thaiPlan = planFor ThaiShaper Scripts.Thai

/// <summary>The plan of the Thai shaper for Lao text in a font with no GSUB script.</summary>
let private laoPlan = planFor ThaiShaper Scripts.Lao

/// <summary>The plan of the Thai shaper for Thai text in a font with a Thai GSUB script.</summary>
let private gsubPlan = { thaiPlan with Map = { thaiPlan.Map with FoundGsubScript = true } }

/// <summary>The global mask the flag tests start from, with no glyph flag set.</summary>
[<Literal>]
let private GlobalMask = 0x80000000u

/// <summary>Prepares code points through the Thai hook and returns the code points.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The prepared code points.</returns>
let private prepared (plan: OtShapePlan) (font: Font) (codepoints: uint32 array) : uint32 array =
    ShaperHooks.preprocess plan font (runOf codepoints) |> codepointsOf

/// <summary>The P6.S15 Thai and Lao shaper tests: the record, the SARA AM decomposition and reordering, the cluster merges,
/// the PUA shaping and the upstream conformance lines.</summary>
[<Tests>]
let tests =
    testList "P6.S15" [
        testCase "thai shaper record" (fun _ ->
            let record = Categorize.profile ThaiShaper
            Expect.equal record.Kind ThaiShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationAuto "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksByGdefLate "zero-width marks"
            Expect.isFalse record.FallbackPosition "fallback position")
        testCase "an empty run is returned as it is" (fun _ ->
            let run = runOf [||]
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.preprocess thaiPlan windowsFont run, run)) "same run")
        testCase "a run without sara am or mark is returned as it is" (fun _ ->
            let run = runOf [| 0x0E01u; 0x0E02u |]
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.preprocess thaiPlan windowsFont run, run)) "same run")
        testCase "the default and Hebrew shapers leave Thai text alone" (fun _ ->
            let run = runOf [| 0x0E01u; 0x0E33u |]
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.preprocess (planFor DefaultShaper Scripts.Thai) windowsFont run, run)) "default"
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.preprocess (planFor HebrewShaper Scripts.Hebrew) windowsFont run, run)) "hebrew")
        testCase "a sara am alone becomes nikhahit and sara aa" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runOf [| 0x0E33u |])
            Expect.equal (codepointsOf run) [| 0x0E4Du; 0x0E32u |] "decomposed"
            Expect.equal (clustersOf run) [| 0u; 0u |] "clusters")
        testCase "the nikhahit is a non-spacing continuation mark and the sara aa keeps the sara am properties" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runOf [| 0x0E33u |])
            Expect.equal (UnicodeProps.generalCategory run.Props[0]) GeneralCategory.NonSpacingMark "nikhahit category"
            Expect.isTrue (UnicodeProps.isContinuation run.Props[0]) "nikhahit continuation"
            Expect.equal (UnicodeProps.generalCategory run.Props[1]) GeneralCategory.OtherLetter "sara aa category"
            Expect.isFalse (UnicodeProps.isContinuation run.Props[1]) "sara aa continuation"
            Expect.notEqual (run.ScratchFlags &&& ScratchFlags.HasContinuations) ScratchFlags.Default "scratch flag")
        testCase "a consonant and a sara am merge their grapheme clusters" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runOf [| 0x0E01u; 0x0E33u |])
            Expect.equal (codepointsOf run) [| 0x0E01u; 0x0E4Du; 0x0E32u |] "decomposed"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u |] "clusters")
        testCase "a tone mark between a consonant and a sara am moves behind the nikhahit" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runOf [| 0x0E14u; 0x0E4Bu; 0x0E33u |])
            Expect.equal (codepointsOf run) [| 0x0E14u; 0x0E4Du; 0x0E4Bu; 0x0E32u |] "reordered"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u; 0u |] "clusters")
        testCase "every mark above the base is moved over, the mai han-akat and the phinthu variant included" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runOf [| 0x0E01u; 0x0E31u; 0x0E48u; 0x0E33u |])
            Expect.equal (codepointsOf run) [| 0x0E01u; 0x0E4Du; 0x0E31u; 0x0E48u; 0x0E32u |] "reordered")
        testCase "a mark below the base is not moved over" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runOf [| 0x0E01u; 0x0E38u; 0x0E33u |])
            Expect.equal (codepointsOf run) [| 0x0E01u; 0x0E38u; 0x0E4Du; 0x0E32u |] "unchanged order")
        testCase "two sara am in a row are each decomposed and the second merges with the first" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runOf [| 0x0E33u; 0x0E33u |])
            Expect.equal (codepointsOf run) [| 0x0E4Du; 0x0E32u; 0x0E4Du; 0x0E32u |] "decomposed"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u; 0u |] "clusters")
        testCase "a Lao sara am becomes the Lao nikhahit and sara aa" (fun _ ->
            let run = ShaperHooks.preprocess laoPlan bareFont (runOf [| 0x0E81u; 0x0EC8u; 0x0EB3u |])
            Expect.equal (codepointsOf run) [| 0x0E81u; 0x0ECDu; 0x0EC8u; 0x0EB2u |] "reordered"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u; 0u |] "clusters")
        testCase "a lone Lao sara am decomposes" (fun _ ->
            Expect.equal (prepared laoPlan bareFont [| 0x0EB3u |]) [| 0x0ECDu; 0x0EB2u |] "decomposed")
        testCase "the characters level merges neither the move nor the grapheme" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runWith ClusterLevel.Characters GlobalMask [| 0x0E14u; 0x0E4Bu; 0x0E33u |])
            Expect.equal (codepointsOf run) [| 0x0E14u; 0x0E4Du; 0x0E4Bu; 0x0E32u |] "reordered"
            Expect.equal (clustersOf run) [| 0u; 2u; 1u; 2u |] "clusters")
        testCase "the monotone characters level merges the move but not the grapheme" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runWith ClusterLevel.MonotoneCharacters GlobalMask [| 0x0E14u; 0x0E4Bu; 0x0E33u |])
            Expect.equal (clustersOf run) [| 0u; 1u; 1u; 1u |] "clusters")
        testCase "the graphemes level merges the grapheme" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan bareFont (runWith ClusterLevel.Graphemes GlobalMask [| 0x0E01u; 0x0E33u |])
            Expect.equal (clustersOf run) [| 0u; 0u; 0u |] "clusters")
        testCase "a font with a Thai GSUB script gets the sara am decomposition but no PUA shaping" (fun _ ->
            Expect.equal (prepared gsubPlan windowsFont [| 0x0E01u; 0x0E48u |]) [| 0x0E01u; 0x0E48u |] "mark kept"
            Expect.equal (prepared gsubPlan windowsFont [| 0x0E33u |]) [| 0x0E4Du; 0x0E32u |] "sara am decomposed")
        testCase "Lao text never gets PUA shaping" (fun _ ->
            Expect.equal (prepared laoPlan windowsFont [| 0x0E01u; 0x0E48u |]) [| 0x0E01u; 0x0E48u |] "mark kept")
        testCase "a font without PUA forms keeps the marks" (fun _ ->
            Expect.equal (prepared thaiPlan bareFont [| 0x0E01u; 0x0E48u |]) [| 0x0E01u; 0x0E48u |] "mark kept"
            Expect.equal (prepared thaiPlan bareFont [| 0x0E0Du; 0x0E38u |]) [| 0x0E0Du; 0x0E38u |] "base kept")
        testCase "a tone mark on a plain consonant shifts down" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E01u; 0x0E48u |]) [| 0x0E01u; 0xF70Au |] "SD")
        testCase "a tone mark on a consonant with an ascender shifts down and left" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E1Bu; 0x0E48u |]) [| 0x0E1Bu; 0xF705u |] "SDL")
        testCase "an above vowel on a consonant with an ascender shifts left" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E1Bu; 0x0E31u |]) [| 0x0E1Bu; 0xF710u |] "SL")
        testCase "an above vowel then a tone mark on a consonant with an ascender both shift left" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E1Fu; 0x0E34u; 0x0E48u |]) [| 0x0E1Fu; 0xF701u; 0xF713u |] "SL then SL")
        testCase "an above vowel then a tone mark on a plain consonant leave the tone mark alone" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E01u; 0x0E34u; 0x0E48u |]) [| 0x0E01u; 0x0E34u; 0x0E48u |] "unchanged")
        testCase "a below vowel on a removable-descender consonant removes the descender from the base" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E0Du; 0x0E38u |]) [| 0xF70Fu; 0x0E38u |] "RD yo ying"
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E10u; 0x0E39u |]) [| 0xF700u; 0x0E39u |] "RD tho than")
        testCase "a below vowel on a strict-descender consonant shifts down" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E0Eu; 0x0E38u |]) [| 0x0E0Eu; 0xF718u |] "SD")
        testCase "a below vowel on a code point that is no consonant shifts down, a tone mark does not" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x61u; 0x0E38u |]) [| 0x61u; 0xF718u |] "SD"
            Expect.equal (prepared thaiPlan windowsFont [| 0x61u; 0x0E48u |]) [| 0x61u; 0x0E48u |] "unchanged")
        testCase "a mark alone is not shifted" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E48u |]) [| 0x0E48u |] "unchanged")
        testCase "the Mac form is used when the font lacks the Windows one" (fun _ ->
            Expect.equal (prepared thaiPlan macFont [| 0x0E01u; 0x0E48u |]) [| 0x0E01u; 0xF88Bu |] "SD"
            Expect.equal (prepared thaiPlan macFont [| 0x0E0Du; 0x0E38u |]) [| 0xF89Au; 0x0E38u |] "RD")
        testCase "the Windows form is preferred when the font has both" (fun _ ->
            Expect.equal (prepared thaiPlan bothFont [| 0x0E01u; 0x0E48u |]) [| 0x0E01u; 0xF70Au |] "SD")
        testCase "the sara am is decomposed before the PUA shaping reads the marks" (fun _ ->
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E01u; 0x0E48u; 0x0E33u |]) [| 0x0E01u; 0x0E4Du; 0x0E48u; 0x0E32u |] "plain base: the nikhahit is an above vowel, the tone mark follows it and stays"
            Expect.equal (prepared thaiPlan windowsFont [| 0x0E1Bu; 0x0E33u |]) [| 0x0E1Bu; 0xF711u; 0x0E32u |] "ascender base: the nikhahit shifts left")
        testCase "a mark that is shaped makes the glyphs from its base unsafe to break" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan windowsFont (runWith ClusterLevel.MonotoneGraphemes GlobalMask [| 0x0E01u; 0x0E34u; 0x0E48u |])
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask ||| 3u; GlobalMask |] "flags on the glyph after the first cluster of the range")
        testCase "a mark next to its base makes nothing unsafe to break" (fun _ ->
            let run = ShaperHooks.preprocess thaiPlan windowsFont (runWith ClusterLevel.MonotoneGraphemes GlobalMask [| 0x0E01u; 0x0E48u |])
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask |] "no flags")
        Conformance.files "conformance" [ "in-house", "sara-am.tests" ]
    ]
