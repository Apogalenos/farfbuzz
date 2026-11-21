module Farfbuzz.Tests.Shape.PlanTests

open System.Collections.Immutable
open Expecto
open Farfbuzz

/// <summary>Builds one categorize test case.</summary>
/// <param name="name">The script name.</param>
/// <param name="script">The script.</param>
/// <param name="expected">The expected kind.</param>
let private shaperCase (name: string) (script: Script) (expected: ShaperKind) =
    testCase name (fun _ ->
        Expect.equal (Categorize.shaper script Direction.Ltr Tags.Empty) expected $"shaper for {name}")

/// <summary>The 109 switch cases of <c>hb_ot_shaper_categorize</c>, one per <c>HB_SCRIPT_*</c> label.</summary>
let private switchCases =
    [ "Arabic", Scripts.Arabic, ArabicShaper
      "Syriac", Scripts.Syriac, ArabicShaper
      "Thai", Scripts.Thai, ThaiShaper
      "Lao", Scripts.Lao, ThaiShaper
      "Hangul", Scripts.Hangul, HangulShaper
      "Hebrew", Scripts.Hebrew, HebrewShaper
      "Bengali", Scripts.Bengali, IndicShaper
      "Devanagari", Scripts.Devanagari, IndicShaper
      "Gujarati", Scripts.Gujarati, IndicShaper
      "Gurmukhi", Scripts.Gurmukhi, IndicShaper
      "Kannada", Scripts.Kannada, IndicShaper
      "Malayalam", Scripts.Malayalam, IndicShaper
      "Oriya", Scripts.Oriya, IndicShaper
      "Tamil", Scripts.Tamil, IndicShaper
      "Telugu", Scripts.Telugu, IndicShaper
      "Khmer", Scripts.Khmer, KhmerShaper
      "Myanmar", Scripts.Myanmar, MyanmarShaper
      "Myanmar Zawgyi", Script.ofIso15924 (Tag.ofString "Qaag"), MyanmarZawgyiShaper
      "Tibetan", Scripts.Tibetan, UseShaper
      "Mongolian", Scripts.Mongolian, UseShaper
      "Sinhala", Scripts.Sinhala, UseShaper
      "Buhid", Scripts.Buhid, UseShaper
      "Hanunoo", Scripts.Hanunoo, UseShaper
      "Tagalog", Scripts.Tagalog, UseShaper
      "Tagbanwa", Scripts.Tagbanwa, UseShaper
      "Limbu", Scripts.Limbu, UseShaper
      "Tai Le", Scripts.TaiLe, UseShaper
      "Buginese", Scripts.Buginese, UseShaper
      "Kharoshthi", Scripts.Kharoshthi, UseShaper
      "Syloti Nagri", Scripts.SylotiNagri, UseShaper
      "Tifinagh", Scripts.Tifinagh, UseShaper
      "Balinese", Scripts.Balinese, UseShaper
      "Nko", Scripts.Nko, UseShaper
      "Phags Pa", Scripts.PhagsPa, UseShaper
      "Cham", Scripts.Cham, UseShaper
      "Kayah Li", Scripts.KayahLi, UseShaper
      "Lepcha", Scripts.Lepcha, UseShaper
      "Rejang", Scripts.Rejang, UseShaper
      "Saurashtra", Scripts.Saurashtra, UseShaper
      "Sundanese", Scripts.Sundanese, UseShaper
      "Egyptian Hieroglyphs", Scripts.EgyptianHieroglyphs, UseShaper
      "Javanese", Scripts.Javanese, UseShaper
      "Kaithi", Scripts.Kaithi, UseShaper
      "Meetei Mayek", Scripts.MeeteiMayek, UseShaper
      "Tai Tham", Scripts.TaiTham, UseShaper
      "Tai Viet", Scripts.TaiViet, UseShaper
      "Batak", Scripts.Batak, UseShaper
      "Brahmi", Scripts.Brahmi, UseShaper
      "Mandaic", Scripts.Mandaic, UseShaper
      "Chakma", Scripts.Chakma, UseShaper
      "Miao", Scripts.Miao, UseShaper
      "Sharada", Scripts.Sharada, UseShaper
      "Takri", Scripts.Takri, UseShaper
      "Duployan", Scripts.Duployan, UseShaper
      "Grantha", Scripts.Grantha, UseShaper
      "Khojki", Scripts.Khojki, UseShaper
      "Khudawadi", Scripts.Khudawadi, UseShaper
      "Mahajani", Scripts.Mahajani, UseShaper
      "Manichaean", Scripts.Manichaean, UseShaper
      "Modi", Scripts.Modi, UseShaper
      "Pahawh Hmong", Scripts.PahawhHmong, UseShaper
      "Psalter Pahlavi", Scripts.PsalterPahlavi, UseShaper
      "Siddham", Scripts.Siddham, UseShaper
      "Tirhuta", Scripts.Tirhuta, UseShaper
      "Ahom", Scripts.Ahom, UseShaper
      "Multani", Scripts.Multani, UseShaper
      "Adlam", Scripts.Adlam, UseShaper
      "Bhaiksuki", Scripts.Bhaiksuki, UseShaper
      "Marchen", Scripts.Marchen, UseShaper
      "Newa", Scripts.Newa, UseShaper
      "Masaram Gondi", Scripts.MasaramGondi, UseShaper
      "Soyombo", Scripts.Soyombo, UseShaper
      "Zanabazar Square", Scripts.ZanabazarSquare, UseShaper
      "Dogra", Scripts.Dogra, UseShaper
      "Gunjala Gondi", Scripts.GunjalaGondi, UseShaper
      "Hanifi Rohingya", Scripts.HanifiRohingya, UseShaper
      "Makasar", Scripts.Makasar, UseShaper
      "Medefaidrin", Scripts.Medefaidrin, UseShaper
      "Old Sogdian", Scripts.OldSogdian, UseShaper
      "Sogdian", Scripts.Sogdian, UseShaper
      "Elymaic", Scripts.Elymaic, UseShaper
      "Nandinagari", Scripts.Nandinagari, UseShaper
      "Nyiakeng Puachue Hmong", Scripts.NyiakengPuachueHmong, UseShaper
      "Wancho", Scripts.Wancho, UseShaper
      "Chorasmian", Scripts.Chorasmian, UseShaper
      "Dives Akuru", Scripts.DivesAkuru, UseShaper
      "Khitan Small Script", Scripts.KhitanSmallScript, UseShaper
      "Yezidi", Scripts.Yezidi, UseShaper
      "Cypro Minoan", Scripts.CyproMinoan, UseShaper
      "Old Uyghur", Scripts.OldUyghur, UseShaper
      "Tangsa", Scripts.Tangsa, UseShaper
      "Toto", Scripts.Toto, UseShaper
      "Vithkuqi", Scripts.Vithkuqi, UseShaper
      "Kawi", Scripts.Kawi, UseShaper
      "Nag Mundari", Scripts.NagMundari, UseShaper
      "Garay", Scripts.Garay, UseShaper
      "Gurung Khema", Scripts.GurungKhema, UseShaper
      "Kirat Rai", Scripts.KiratRai, UseShaper
      "Ol Onal", Scripts.OlOnal, UseShaper
      "Sunuwar", Scripts.Sunuwar, UseShaper
      "Todhri", Scripts.Todhri, UseShaper
      "Tulu Tigalari", Scripts.TuluTigalari, UseShaper
      "Beria Erfe", Scripts.BeriaErfe, UseShaper
      "Sidetic", Scripts.Sidetic, UseShaper
      "Tai Yo", Scripts.TaiYo, UseShaper
      "Tolong Siki", Scripts.TolongSiki, UseShaper
      "Jurchen", Scripts.Jurchen, UseShaper
      "Proto Cuneiform", Scripts.ProtoCuneiform, UseShaper
      "Seal", Scripts.Seal, UseShaper ]

/// <summary>Compiles a default-shaper plan with empty capabilities, the phase-4 baseline.</summary>
/// <returns>The plan.</returns>
let private defaultPlan () =
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    OtPlan.compile PlanCapabilities.none props ImmutableArray.Empty OtMap.empty DefaultShaper

/// <summary>Compiles a plan with morx active, the case where the shaper is swapped for the dumber one.</summary>
/// <param name="kind">The categorized shaper kind.</param>
/// <returns>The plan.</returns>
let private morxPlan (kind: ShaperKind) =
    let capabilities = { PlanCapabilities.none with HasMorx = true }
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    OtPlan.compile capabilities props ImmutableArray.Empty OtMap.empty kind

/// <summary>Asserts the four mark-related plan flags of a morx-active compile.</summary>
/// <param name="kind">The categorized shaper kind.</param>
/// <param name="zeroMarks">The expected zero-marks flag.</param>
/// <param name="fallback">The expected fallback flag.</param>
let private assertMorxFlags (kind: ShaperKind) (zeroMarks: bool) (fallback: bool) =
    let plan = morxPlan kind
    Expect.equal plan.ZeroMarks zeroMarks "zero marks"
    Expect.isFalse plan.ApplyKern "apply kern"
    Expect.equal plan.ApplyFallbackKern fallback "apply fallback kern"
    Expect.equal plan.FallbackMarkPositioning fallback "fallback mark positioning"

/// <summary>Builds one morx-active plan-flag test case.</summary>
/// <param name="name">The shaper kind name.</param>
/// <param name="kind">The shaper kind.</param>
/// <param name="zeroMarks">The expected zero-marks flag.</param>
/// <param name="fallback">The expected fallback flag.</param>
let private morxCase (name: string) (kind: ShaperKind) (zeroMarks: bool) (fallback: bool) =
    testCase $"morx {name} keeps its mark behaviour" (fun _ -> assertMorxFlags kind zeroMarks fallback)

/// <summary>The morx-active compile cases: one per shaper kind, its pre-swap mark behaviour.</summary>
let private morxCases =
    [ "default", DefaultShaper, true, true
      "dumber", DumberShaper, false, false
      "arabic", ArabicShaper, true, true
      "thai", ThaiShaper, true, false
      "hangul", HangulShaper, false, true
      "hebrew", HebrewShaper, true, true
      "indic", IndicShaper, false, false
      "khmer", KhmerShaper, false, false
      "myanmar", MyanmarShaper, true, false
      "myanmar zawgyi", MyanmarZawgyiShaper, false, false
      "use", UseShaper, true, false ]

/// <summary>The P4.S01 shaping types, categorization and plan-skeleton tests.</summary>
[<Tests>]
let tests =
    testList "P4.S01" [
        yield! switchCases |> List.map (fun (name, script, expected) -> shaperCase name script expected)
        testCase "Arabic TTB is default" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Arabic Direction.Ttb Tags.Empty) DefaultShaper "vertical Arabic")
        testCase "Syriac with DFLT is default" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Syriac Direction.Ltr Tags.Dflt) DefaultShaper "Syriac DFLT")
        testCase "Arabic with DFLT is Arabic" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Arabic Direction.Ltr Tags.Dflt) ArabicShaper "Arabic DFLT")
        testCase "Devanagari with DFLT is default" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Devanagari Direction.Ltr Tags.Dflt) DefaultShaper "Devanagari DFLT")
        testCase "Devanagari with latn is default" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Devanagari Direction.Ltr Tags.Latn) DefaultShaper "Devanagari latn")
        testCase "Devanagari with dev3 is USE" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Devanagari Direction.Ltr (Tag.ofString "dev3")) UseShaper "Devanagari dev3")
        testCase "Myanmar with mymr is default" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Myanmar Direction.Ltr Tags.Mymr) DefaultShaper "Myanmar mymr")
        testCase "Tibetan with latn is default" (fun _ ->
            Expect.equal (Categorize.shaper Scripts.Tibetan Direction.Ltr Tags.Latn) DefaultShaper "Tibetan latn")
        testCase "Hebrew under morx is dumbered" (fun _ ->
            let capabilities = { PlanCapabilities.none with HasMorx = true }
            let props = { Direction = Direction.Ltr; Script = Scripts.Hebrew; Language = NoLanguage }
            Expect.equal (OtPlan.compile capabilities props ImmutableArray.Empty OtMap.empty HebrewShaper).Shaper.Kind DumberShaper "dumbered kind")
        yield! morxCases |> List.map (fun (name, kind, zeroMarks, fallback) -> morxCase name kind zeroMarks fallback)
        testCase "default plan has fallback glyph classes" (fun _ ->
            Expect.isTrue (defaultPlan ()).FallbackGlyphClasses "fallback glyph classes")
        testCase "default plan zeroes marks" (fun _ ->
            Expect.isTrue (defaultPlan ()).ZeroMarks "zero marks")
        testCase "default plan falls back mark positioning" (fun _ ->
            Expect.isTrue (defaultPlan ()).FallbackMarkPositioning "fallback mark positioning")
        testCase "default plan applies fallback kern only" (fun _ ->
            let plan = defaultPlan ()
            Expect.isTrue plan.ApplyFallbackKern "apply fallback kern"
            Expect.isFalse plan.ApplyGpos "apply gpos"
            Expect.isFalse plan.ApplyKern "apply kern"
            Expect.isFalse plan.ApplyKerx "apply kerx"
            Expect.isFalse plan.ApplyMorx "apply morx"
            Expect.isFalse plan.ApplyTrak "apply trak")
    ]
