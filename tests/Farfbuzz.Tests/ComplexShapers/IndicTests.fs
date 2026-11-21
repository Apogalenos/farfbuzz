module Farfbuzz.Tests.ComplexShapers.IndicTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>Devanagari letter ka, <c>U+0915</c>.</summary>
[<Literal>]
let private Ka = 0x0915u

/// <summary>Devanagari letter kha, <c>U+0916</c>.</summary>
[<Literal>]
let private Kha = 0x0916u

/// <summary>Devanagari letter ra, <c>U+0930</c>.</summary>
[<Literal>]
let private Ra = 0x0930u

/// <summary>Devanagari letter ya, <c>U+092F</c>.</summary>
[<Literal>]
let private Ya = 0x092Fu

/// <summary>Devanagari sign virama, <c>U+094D</c>.</summary>
[<Literal>]
let private Virama = 0x094Du

/// <summary>Devanagari sign nukta, <c>U+093C</c>.</summary>
[<Literal>]
let private Nukta = 0x093Cu

/// <summary>Devanagari vowel sign i, a left matra, <c>U+093F</c>.</summary>
[<Literal>]
let private MatraI = 0x093Fu

/// <summary>Devanagari vowel sign aa, a right matra, <c>U+093E</c>.</summary>
[<Literal>]
let private MatraAa = 0x093Eu

/// <summary>Devanagari sign anusvara, <c>U+0902</c>.</summary>
[<Literal>]
let private Anusvara = 0x0902u

/// <summary>Devanagari letter a, <c>U+0905</c>.</summary>
[<Literal>]
let private LetterA = 0x0905u

/// <summary>The zero width joiner, <c>U+200D</c>.</summary>
[<Literal>]
let private Zwj = 0x200Du

/// <summary>The zero width non-joiner, <c>U+200C</c>.</summary>
[<Literal>]
let private Zwnj = 0x200Cu

/// <summary>The dotted circle, <c>U+25CC</c>.</summary>
[<Literal>]
let private DottedCircle = 0x25CCu

/// <summary>Kannada letter ka, <c>U+0C95</c>.</summary>
[<Literal>]
let private KannadaKa = 0x0C95u

/// <summary>Kannada letter ra, <c>U+0CB0</c>.</summary>
[<Literal>]
let private KannadaRa = 0x0CB0u

/// <summary>Kannada sign virama, <c>U+0CCD</c>.</summary>
[<Literal>]
let private KannadaVirama = 0x0CCDu

/// <summary>The global mask every glyph of the tests starts with.</summary>
[<Literal>]
let private GlobalMask = 0x80000000u

/// <summary>The glyph each code point of the tests has in the test fonts.</summary>
let private mapping : (uint32 * uint32) list =
    [ Ka, 1u; Kha, 2u; Ra, 3u; Ya, 4u; Virama, 5u; Nukta, 6u; MatraI, 7u; MatraAa, 8u; Anusvara, 9u; DottedCircle, 11u
      Zwj, 12u; Zwnj, 13u; LetterA, 14u; KannadaKa, 21u; KannadaRa, 22u; KannadaVirama, 23u ]

/// <summary>The glyph of the reph the test fonts form.</summary>
[<Literal>]
let private RephGlyph = 40u

/// <summary>The glyph of the below-form of ra the test fonts form.</summary>
[<Literal>]
let private BelowRaGlyph = 41u

/// <summary>A font with the dotted circle and the letters of the tests and no layout table.</summary>
let private plainFont = fontMapping mapping

/// <summary>The GSUB table of the layout font: for the script <c>dev2</c>, <c>rphf</c> forms the reph from ra and virama and
/// <c>blwf</c> forms the below-form of ra from virama and ra.</summary>
let private layoutGsub : byte array =
    gsubOf "dev2" [ "rphf", [ 0u ]; "blwf", [ 1u ] ] [ ligatureLookup [ 3u ] [ 5u ] RephGlyph; ligatureLookup [ 5u ] [ 3u ] BelowRaGlyph ]

/// <summary>A font whose layout forms the reph and the below-form of ra.</summary>
let private layoutFont = fontWithGsub mapping 100u layoutGsub

/// <summary>The plan of the Indic shaper for a script over an empty map, as a font with no GSUB script would give it.</summary>
/// <param name="script">The script.</param>
/// <returns>The plan.</returns>
let private indicPlanFor (script: Script) = planFor IndicShaper script

/// <summary>The plan data the reordering tests use: the masks of the features the reordering sets, the lookups of
/// <c>rphf</c> and <c>blwf</c> of the layout font, and the new specification.</summary>
let private newSpecPlan : IndicPlan =
    { Config = Indic.config Scripts.Devanagari
      IsOldSpec = false
      RphfFeature = { Lookups = ImmutableArray.Create 0; ZeroContext = true; Prepared = Memo<obj>() }
      PrefFeature = { Lookups = ImmutableArray<int>.Empty; ZeroContext = true; Prepared = Memo<obj>() }
      BlwfFeature = { Lookups = ImmutableArray.Create 1; ZeroContext = true; Prepared = Memo<obj>() }
      PstfFeature = { Lookups = ImmutableArray<int>.Empty; ZeroContext = true; Prepared = Memo<obj>() }
      VatuFeature = { Lookups = ImmutableArray<int>.Empty; ZeroContext = true; Prepared = Memo<obj>() }
      RphfMask = 0x4u
      PrefMask = 0x8u
      BlwfMask = 0x20u
      AbvfMask = 0x40u
      HalfMask = 0x10u
      PstfMask = 0x80u
      InitMask = 0x100u
      Positions = Memo<obj>() }

/// <summary>The plan data with no lookup for any feature: no reph is formed and no consonant has a below-form.</summary>
let private emptyLookupsPlan : IndicPlan =
    { newSpecPlan with
        RphfFeature = { Lookups = ImmutableArray<int>.Empty; ZeroContext = true; Prepared = Memo<obj>() }
        BlwfFeature = { Lookups = ImmutableArray<int>.Empty; ZeroContext = true; Prepared = Memo<obj>() } }

/// <summary>A run of the given code points with its categories and positions set.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private categorized (codepoints: uint32 array) : GlyphRun =
    Indic.setupMasks (runWith ClusterLevel.MonotoneGraphemes GlobalMask codepoints)

/// <summary>Replaces the code points of a run by the glyphs a font maps them to, as the normalizer has done by the time the
/// pauses run.</summary>
/// <param name="font">The font.</param>
/// <param name="run">The run.</param>
/// <returns>The run with glyphs.</returns>
let private asGlyphs (font: Font) (run: GlyphRun) : GlyphRun =
    let infos = Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Codepoint = (Font.nominalGlyph font info.Codepoint |> ValueOption.defaultValue 0u) }) (run.Infos.AsSpan())
    { run with Infos = infos }

/// <summary>A run of the given code points with its syllables set, as the glyphs of a font, ready for the reordering.</summary>
/// <param name="font">The font that maps the code points to glyphs.</param>
/// <param name="script">The script of the run.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private syllabled (font: Font) (script: Script) (codepoints: uint32 array) : GlyphRun =
    GlyphRun.withSegment { Direction = Direction.Ltr; Script = script; Language = NoLanguage } (Indic.setupSyllables (asGlyphs font (categorized codepoints)))

/// <summary>Runs the initial reordering on the given code points.</summary>
/// <param name="plan">The plan data.</param>
/// <param name="font">The font.</param>
/// <param name="script">The script.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The reordered run.</returns>
let private initiallyReordered (plan: IndicPlan) (font: Font) (script: Script) (codepoints: uint32 array) : GlyphRun =
    IndicInitial.reorder plan font (syllabled font script codepoints)

/// <summary>Reads the syllable byte of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The syllable bytes.</returns>
let private syllablesOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.Syllable |]

/// <summary>Reads the position of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The positions.</returns>
let private positionsOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.ShaperAuxiliary |]

/// <summary>Reads the position of one glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <param name="index">The glyph.</param>
/// <returns>The position.</returns>
let private positionAt (run: GlyphRun) index : byte = run.Props[index].ShaperAuxiliary

/// <summary>A run of glyphs as the basic features leave them, for the final reordering: one syllable, each glyph with its
/// glyph, category and position, the first flagged as ligated (a reph the font formed).</summary>
/// <param name="ligatedFirst">Whether the first glyph ligated.</param>
/// <param name="rows">The glyph, category and position of each glyph.</param>
/// <returns>The run.</returns>
let private afterBasicFeatures (ligatedFirst: bool) (rows: (uint32 * byte * byte) list) : GlyphRun =
    let run = runWith ClusterLevel.MonotoneGraphemes GlobalMask (Array.create rows.Length 0x41u)
    let infos = Sealed.mapi (fun index (info: GlyphInfo) -> match rows[index] with | glyph, _, _ -> { info with Codepoint = glyph }) (run.Infos.AsSpan())
    let props =
        Sealed.mapi (fun index (props: GlyphProps) ->
            let _, category, position = rows[index]
            let flagged = match index = 0 && ligatedFirst with | true -> props.GlyphClass ||| GlyphPropsBits.Ligated | false -> props.GlyphClass
            { props with Syllable = 0x10uy; ShaperCategory = category; ShaperAuxiliary = position; GlyphClass = flagged }) (run.Props.AsSpan())
    { run with Infos = infos; Props = props }

/// <summary>Reorders a run of the basic features' glyphs for a script.</summary>
/// <param name="script">The script.</param>
/// <param name="rows">The glyph, category and position of each glyph, the first a ligated reph.</param>
/// <returns>The glyphs after the final reordering.</returns>
let private rephPlacedFor (script: Script) (rows: (uint32 * byte * byte) list) : uint32 array =
    let plan = { newSpecPlan with Config = Indic.config script }
    codepointsOf (IndicFinal.reorder plan plainFont (afterBasicFeatures true rows))

/// <summary>The stage of a feature among the features a builder collected.</summary>
/// <param name="collected">The builder.</param>
/// <param name="name">The feature tag.</param>
/// <returns>The stage.</returns>
let private stageOf (collected: MapBuilder) (name: string) : int =
    collected.Features |> Seq.find (fun info -> Tag.toString info.InfoTag = name) |> fun info -> info.GsubStage

/// <summary>The flags of a feature among the features a builder collected.</summary>
/// <param name="collected">The builder.</param>
/// <param name="name">The feature tag.</param>
/// <returns>The flags.</returns>
let private flagsOf (collected: MapBuilder) (name: string) : uint32 =
    collected.Features |> Seq.find (fun info -> Tag.toString info.InfoTag = name) |> fun info -> info.InfoFlags

/// <summary>The files of the shaping suites whose lines the three Indic slices make pass: the in-house Indic files and
/// the Shan, Kannada, Devanagari and Bengali test of the text rendering suite.</summary>
let private conformanceFiles : (string * string) list =
    [ "in-house", "indic-consonant-with-stacker.tests"; "in-house", "indic-decompose.tests"; "in-house", "indic-feature-order.tests"
      "in-house", "indic-init.tests"; "in-house", "indic-joiner-candrabindu.tests"; "in-house", "indic-malayalam-dot-reph.tests"
      "in-house", "indic-misc.tests"; "in-house", "indic-old-spec.tests"; "in-house", "indic-pref-blocking.tests"
      "in-house", "indic-script-extensions.tests"; "in-house", "indic-special-cases.tests"; "in-house", "indic-syllable.tests"
      "in-house", "indic-unicode18.tests"; "in-house", "indic-vowel-letter-spoofing.tests"; "text-rendering-tests", "SHKNDA-1.tests" ]

/// <summary>The P6.S17, P6.S18 and P6.S19 Indic shaper tests: the record, the configuration of each script, the features and
/// pauses, the plan data, the categories, the syllables, the dotted circles, the initial and the final reordering, the
/// decomposition and the upstream conformance lines.</summary>
[<Tests>]
let tests =
    testList "P6.S17 P6.S18 P6.S19" [
        testCase "indic shaper record" (fun _ ->
            let record = Categorize.profile IndicShaper
            Expect.equal record.Kind IndicShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationComposedDiacriticsNoShortCircuit "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksNone "zero-width marks"
            Expect.isFalse record.FallbackPosition "fallback position")
        testCase "the configuration of each script is that of indic_configs" (fun _ ->
            let expected =
                [ "devanagari", Scripts.Devanagari, 0x094Du, RephBeforePost, RephImplicit, BlwfPreAndPost
                  "bengali", Scripts.Bengali, 0x09CDu, RephAfterSub, RephImplicit, BlwfPreAndPost
                  "gurmukhi", Scripts.Gurmukhi, 0x0A4Du, RephBeforeSub, RephImplicit, BlwfPreAndPost
                  "gujarati", Scripts.Gujarati, 0x0ACDu, RephBeforePost, RephImplicit, BlwfPreAndPost
                  "oriya", Scripts.Oriya, 0x0B4Du, RephAfterMain, RephImplicit, BlwfPreAndPost
                  "tamil", Scripts.Tamil, 0x0BCDu, RephAfterPost, RephImplicit, BlwfPreAndPost
                  "telugu", Scripts.Telugu, 0x0C4Du, RephAfterPost, RephExplicit, BlwfPostOnly
                  "kannada", Scripts.Kannada, 0x0CCDu, RephAfterPost, RephImplicit, BlwfPostOnly
                  "malayalam", Scripts.Malayalam, 0x0D4Du, RephAfterMain, RephLogRepha, BlwfPreAndPost ]
            for name, script, virama, reph, mode, blwf in expected do
                let config = Indic.config script
                Expect.isTrue config.HasOldSpec $"{name} has an old specification"
                Expect.equal config.Virama virama $"{name} virama"
                Expect.equal config.RephPosition reph $"{name} reph position"
                Expect.equal config.RephMode mode $"{name} reph mode"
                Expect.equal config.BlwfMode blwf $"{name} blwf mode")
        testCase "a script the shaper takes that has no entry has the default configuration" (fun _ ->
            let config = Indic.config Scripts.Sinhala
            Expect.isFalse config.HasOldSpec "no old specification"
            Expect.equal config.Virama 0u "no virama"
            Expect.equal config.RephPosition RephBeforePost "reph position"
            Expect.equal config.RephMode RephImplicit "reph mode"
            Expect.equal config.BlwfMode BlwfPreAndPost "blwf mode")
        testCase "collect_features sets up the syllables, applies locl and ccmp, reorders, applies each basic feature in a stage of its own, reorders again and applies the rest" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Devanagari; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props IndicShaper ImmutableArray.Empty
            let pauses = [ for pause in collected.GsubPauses -> pause.StagePause ]
            let basic = List.replicate 11 NoPause
            Expect.equal pauses ([ NoPause; IndicSetupSyllables; IndicInitialReordering ] @ basic @ [ IndicFinalReordering; ClearSyllables ]) "pauses in order"
            Expect.equal (stageOf collected "rvrn") 0 "rvrn before every pause"
            Expect.equal (stageOf collected "locl") 2 "locl after the setup of the syllables"
            Expect.equal (stageOf collected "ccmp") 2 "ccmp"
            let basicStages = [ "nukt"; "akhn"; "rphf"; "rkrf"; "pref"; "blwf"; "abvf"; "half"; "pstf"; "vatu"; "cjct" ] |> List.map (stageOf collected)
            Expect.equal basicStages [ 3 .. 13 ] "each basic feature has a stage of its own after the initial reordering"
            let otherStages = [ "init"; "pres"; "abvs"; "blws"; "psts"; "haln" ] |> List.map (stageOf collected)
            Expect.equal otherStages (List.replicate 6 15) "the other features share the stage after the final reordering"
            Expect.isFalse collected.IsSimple "a hook makes the map non-simple")
        testCase "the flags of the features are those of indic_features" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Devanagari; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props IndicShaper ImmutableArray.Empty
            let global' = FeatureFlags.GlobalManualJoiners ||| FeatureFlags.PerSyllable
            let masked = FeatureFlags.ManualJoiners ||| FeatureFlags.PerSyllable
            for name in [ "nukt"; "akhn"; "rkrf"; "vatu"; "cjct"; "pres"; "abvs"; "blws"; "psts"; "haln" ] do
                Expect.equal (flagsOf collected name) global' $"{name} is global, keeps the joiners and stays in the syllable"
            for name in [ "rphf"; "pref"; "blwf"; "abvf"; "half"; "pstf"; "init" ] do
                Expect.equal (flagsOf collected name) masked $"{name} is not global, keeps the joiners and stays in the syllable"
            Expect.equal (flagsOf collected "locl") (FeatureFlags.Global ||| FeatureFlags.PerSyllable) "locl"
            Expect.equal (flagsOf collected "ccmp") (FeatureFlags.Global ||| FeatureFlags.PerSyllable) "ccmp")
        testCase "override_features turns liga off after the user features and clears the syllables last" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Devanagari; Language = NoLanguage }
            let user = ImmutableArray.Create { Tag = Tag.ofString "kern"; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            let collected = ShaperHooks.collectFeatures props IndicShaper user
            let last = collected.Features |> Seq.last
            Expect.equal (Tag.toString last.InfoTag, last.MaxValue) ("liga", 0u) "liga off is the last feature"
            Expect.equal (collected.GsubPauses[collected.GsubPauses.Length - 1].StagePause) ClearSyllables "the last pause clears the syllables")
        testCase "data_create reads the masks of the features the reordering sets and none for the global features" (fun _ ->
            let mask1 (tag: Tag) =
                match Tag.toString tag with
                | "rphf" -> 1u
                | "pref" -> 2u
                | "blwf" -> 4u
                | "abvf" -> 8u
                | "half" -> 16u
                | "pstf" -> 32u
                | "init" -> 64u
                | _ -> 0u
            let props = { Direction = Direction.Ltr; Script = Scripts.Devanagari; Language = NoLanguage }
            match ShaperHooks.createData IndicShaper mask1 props OtMap.empty with
            | IndicData data ->
                Expect.equal [ data.RphfMask; data.PrefMask; data.BlwfMask; data.AbvfMask; data.HalfMask; data.PstfMask; data.InitMask ] [ 1u; 2u; 4u; 8u; 16u; 32u; 64u ] "masks"
            | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> failtest "expected Indic data")
        testCase "a font is old-spec unless its chosen script tag ends in 2, and only the scripts with an old specification have one" (fun _ ->
            let isOldSpec (script: Script) (tag: string) =
                let map = { OtMap.empty with ChosenGsubScript = Tag.ofString tag }
                match ShaperHooks.createData IndicShaper (fun _ -> 0u) { Direction = Direction.Ltr; Script = script; Language = NoLanguage } map with
                | IndicData data -> data.IsOldSpec
                | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> failtest "expected Indic data"
            Expect.isTrue (isOldSpec Scripts.Devanagari "deva") "deva"
            Expect.isFalse (isOldSpec Scripts.Devanagari "dev2") "dev2"
            Expect.isTrue (isOldSpec Scripts.Malayalam "mlym") "mlym"
            Expect.isFalse (isOldSpec Scripts.Malayalam "mlm2") "mlm2"
            Expect.isTrue (isOldSpec Scripts.Devanagari "DFLT") "a font with no Indic script is old-spec"
            Expect.isFalse (isOldSpec Scripts.Sinhala "sinh") "a script with one specification is never old-spec")
        testCase "zero-context matching is for the new specification of every script but Malayalam" (fun _ ->
            let zeroContext (script: Script) (tag: string) =
                let map = { OtMap.empty with ChosenGsubScript = Tag.ofString tag }
                match ShaperHooks.createData IndicShaper (fun _ -> 0u) { Direction = Direction.Ltr; Script = script; Language = NoLanguage } map with
                | IndicData data -> [ data.RphfFeature; data.PrefFeature; data.BlwfFeature; data.PstfFeature; data.VatuFeature ] |> List.map (fun feature -> feature.ZeroContext)
                | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> failtest "expected Indic data"
            Expect.equal (zeroContext Scripts.Devanagari "dev2") (List.replicate 5 true) "new-spec Devanagari"
            Expect.equal (zeroContext Scripts.Devanagari "deva") (List.replicate 5 false) "old-spec Devanagari"
            Expect.equal (zeroContext Scripts.Malayalam "mlm2") (List.replicate 5 false) "new-spec Malayalam allows context"
            Expect.equal (zeroContext Scripts.Sinhala "sinh") (List.replicate 5 true) "a script with one specification")
        testCase "the lookups of a feature are those of its stage, the other features of the stage included" (fun _ ->
            let lookup index mask : LookupMap = { LookupIndex = index; AutoZwnj = false; AutoZwj = false; Random = false; PerSyllable = false; Mask = mask; FeatureTag = Tags.Empty }
            let feature tag stage : FeatureMap =
                { Tag = Tag.ofString tag; GsubIndex = 0u; GposIndex = 0u; GsubStage = stage; GposStage = 0; Shift = 0; Mask = 1u; Mask1 = 1u; NeedsFallback = false; AutoZwnj = false; AutoZwj = false; Random = false; PerSyllable = false }
            let map =
                { OtMap.empty with
                    Features = ImmutableArray.CreateRange [ feature "akhn" 1; feature "half" 2; feature "locl" 0 ]
                    GsubLookups = ImmutableArray.CreateRange [ lookup 7 1u; lookup 3 1u; lookup 9 2u; lookup 4 2u; lookup 8 2u ]
                    GsubStages = ImmutableArray.CreateRange [ { LastLookup = 0; Pause = NoPause }; { LastLookup = 2; Pause = NoPause }; { LastLookup = 5; Pause = NoPause } ] }
            Expect.equal (MapBuilder.stageLookups map (Tag.ofString "akhn")) (ImmutableArray.CreateRange [ 7; 3 ]) "the lookups of the second stage"
            Expect.equal (MapBuilder.stageLookups map (Tag.ofString "half")) (ImmutableArray.CreateRange [ 9; 4; 8 ]) "the lookups of the last stage"
            Expect.equal (MapBuilder.stageLookups map (Tag.ofString "locl")) ImmutableArray<int>.Empty "a stage with no lookups"
            Expect.equal (MapBuilder.stageLookups map (Tag.ofString "vatu")) ImmutableArray<int>.Empty "a feature the map does not have")
        testCase "setup_masks saves the category and the position of every glyph and changes nothing else" (fun _ ->
            let run = categorized [| Ka; Virama; MatraI; MatraAa; Anusvara; Nukta; Ra; Zwj; 0x41u |]
            Expect.equal [| for props in run.Props -> props.ShaperCategory |] [| 1uy; 4uy; 7uy; 7uy; 8uy; 3uy; 15uy; 6uy; 0uy |] "categories: C, H, M, M, SM, N, Ra, ZWJ, X"
            Expect.equal (positionAt run 2) 2uy "the left matra is before the matra position"
            Expect.equal (masksOf run) (Array.create 9 GlobalMask) "masks")
        testCase "an empty run has no syllable and no category" (fun _ ->
            let run = Indic.setupSyllables (Indic.setupMasks (runOf [||]))
            Expect.equal run.Infos.Length 0 "no glyph"
            Expect.equal run.ScratchFlags ScratchFlags.Default "no broken syllable")
        testCase "a consonant with a halant and a consonant is one consonant syllable, the serial counts the syllables and wraps" (fun _ ->
            let run = Indic.setupSyllables (categorized [| Ka; Virama; Kha; MatraAa; Ka |])
            Expect.equal (syllablesOf run) [| 0x10uy; 0x10uy; 0x10uy; 0x10uy; 0x20uy |] "two consonant syllables"
            Expect.equal (run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "no broken syllable"
            let many = Indic.setupSyllables (categorized (Array.create 17 Ka))
            Expect.equal (syllablesOf many |> Array.item 14) 0xF0uy "the serial of the fifteenth syllable"
            Expect.equal (syllablesOf many |> Array.item 15) 0x10uy "the serial of the sixteenth syllable wraps to one"
            Expect.equal (syllablesOf many |> Array.item 16) 0x20uy "and goes on from there")
        testCase "a vowel with no letter before is a vowel syllable and a lone matra is a broken cluster that sets the scratch flag" (fun _ ->
            let vowel = Indic.setupSyllables (categorized [| LetterA; MatraAa |])
            Expect.equal (syllablesOf vowel) [| 0x11uy; 0x11uy |] "vowel syllable"
            let broken = Indic.setupSyllables (categorized [| MatraAa |])
            Expect.equal (syllablesOf broken) [| 0x14uy |] "broken cluster"
            Expect.notEqual (broken.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "flag")
        testCase "a letter that is not Indic is a non-Indic cluster and a dotted circle starts a standalone cluster" (fun _ ->
            let run = Indic.setupSyllables (categorized [| 0x41u; DottedCircle; MatraAa |])
            Expect.equal (syllablesOf run) [| 0x15uy; 0x22uy; 0x22uy |] "non-Indic cluster, then a standalone cluster")
        testCase "every syllable is marked unsafe to break where its clusters differ and the syllable pass changes no glyph" (fun _ ->
            let run = categorized [| Ka; Virama; Kha; Ka |]
            let result = Indic.setupSyllables run
            Expect.equal (masksOf result) [| GlobalMask; GlobalMask ||| 3u; GlobalMask ||| 3u; GlobalMask |] "the glyphs after the first cluster of the syllable"
            Expect.equal (codepointsOf result) (codepointsOf run) "same glyphs")
        testCase "a left matra moves in front of its consonant in the initial reordering and the half-form masks go with the glyphs before the base" (fun _ ->
            let run = initiallyReordered newSpecPlan plainFont Scripts.Devanagari [| Ka; MatraI |]
            Expect.equal (codepointsOf run) [| 7u; 1u |] "the matra first"
            Expect.equal (clustersOf run) [| 1u; 0u |] "the clusters are merged by the final reordering, not by this one"
            Expect.equal (masksOf run) [| GlobalMask ||| 0x33u; GlobalMask |] "half and below-base forms for the glyph before the base, with the unsafe-to-break flags the syllable pass set on the matra"
            Expect.equal (positionsOf run) [| 2uy; 4uy |] "left matra position, base position")
        testCase "the old specification gives the glyphs before the base the half forms only" (fun _ ->
            let run = initiallyReordered { newSpecPlan with IsOldSpec = true } plainFont Scripts.Devanagari [| Ka; MatraI |]
            Expect.equal (masksOf run) [| GlobalMask ||| 0x13u; GlobalMask |] "half form only")
        testCase "a reph the font would form is marked with the reph mask and stays at the start of the syllable" (fun _ ->
            let run = initiallyReordered newSpecPlan layoutFont Scripts.Devanagari [| Ra; Virama; Ka |]
            Expect.equal (codepointsOf run) [| 3u; 5u; 1u |] "same order"
            Expect.equal (positionAt run 0) 1uy "the reph position"
            Expect.equal (masksOf run) [| GlobalMask ||| 0x34u; GlobalMask ||| 0x37u; GlobalMask ||| 3u |] "reph, half and below-base masks on the ra and the halant")
        testCase "without a lookup that forms the reph the ra and the halant get no reph mask" (fun _ ->
            let run = initiallyReordered emptyLookupsPlan plainFont Scripts.Devanagari [| Ra; Virama; Ka |]
            Expect.equal (masksOf run) [| GlobalMask ||| 0x30u; GlobalMask ||| 0x33u; GlobalMask ||| 3u |] "half and below-base masks only")
        testCase "a consonant the font forms below the base is a below-base consonant and the glyphs from it get the post-base masks" (fun _ ->
            let run = initiallyReordered newSpecPlan layoutFont Scripts.Devanagari [| Ka; Virama; Ra |]
            Expect.equal (codepointsOf run) [| 1u; 5u; 3u |] "same order, ka stays the base"
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask ||| 0xE3u; GlobalMask ||| 0xE3u |] "post-base masks")
        testCase "the positions of the consonants come from the lookups of blwf, vatu, pstf and pref, with the halant on either side" (fun _ ->
            let font = fontWithGsub mapping 100u (gsubOf "dev2" [ "blwf", [ 0u ]; "vatu", [ 1u ]; "pstf", [ 2u ]; "pref", [ 3u ] ] [ ligatureLookup [ 5u ] [ 1u ] 50u; ligatureLookup [ 2u ] [ 5u ] 51u; ligatureLookup [ 5u ] [ 3u ] 52u; ligatureLookup [ 4u ] [ 5u ] 53u ])
            let plan =
                { newSpecPlan with
                    BlwfFeature = { Lookups = ImmutableArray.Create 0; ZeroContext = false; Prepared = Memo<obj>() }
                    VatuFeature = { Lookups = ImmutableArray.Create 1; ZeroContext = false; Prepared = Memo<obj>() }
                    PstfFeature = { Lookups = ImmutableArray.Create 2; ZeroContext = false; Prepared = Memo<obj>() }
                    PrefFeature = { Lookups = ImmutableArray.Create 3; ZeroContext = false; Prepared = Memo<obj>() } }
            let run = syllabled font Scripts.Devanagari [| Ka; Kha; Ra; Ya; Nukta |]
            let positions = positionsOf (IndicInitial.updateConsonantPositions plan font run)
            Expect.equal positions[0] 8uy "ka: blwf has virama then ka, below the base"
            Expect.equal positions[1] 8uy "kha: vatu has kha then virama, below the base"
            Expect.equal positions[2] 11uy "ra: pstf has virama then ra, after the base"
            Expect.equal positions[3] 11uy "ya: pref has ya then virama, the other order, after the base"
            Expect.equal positions[4] 14uy "a nukta is not a consonant and keeps its position")
        testCase "a script with no virama or a font that does not map it leaves the positions alone" (fun _ ->
            let bare = fontMapping [ Ka, 1u ]
            let run = syllabled bare Scripts.Devanagari [| Ka |]
            Expect.equal (positionsOf (IndicInitial.updateConsonantPositions newSpecPlan bare run)) (positionsOf run) "no virama glyph"
            let noVirama = { newSpecPlan with Config = Indic.config Scripts.Sinhala }
            Expect.equal (positionsOf (IndicInitial.updateConsonantPositions noVirama plainFont run)) (positionsOf run) "no virama in the configuration")
        testCase "a broken cluster gets a dotted circle in front and the reordering reads it as the base" (fun _ ->
            let run = initiallyReordered newSpecPlan plainFont Scripts.Devanagari [| MatraAa |]
            Expect.equal (codepointsOf run) [| 11u; 8u |] "the dotted circle first"
            Expect.equal (syllablesOf run) [| 0x14uy; 0x14uy |] "same syllable"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster"
            Expect.equal (positionAt run 0) 4uy "the dotted circle is the base")
        testCase "the dotted circle of a broken cluster that starts with a repha goes after the repha" (fun _ ->
            let font = fontMapping [ 0x0D4Eu, 1u; 0x0D3Eu, 2u; DottedCircle, 11u ]
            let plan = { newSpecPlan with Config = Indic.config Scripts.Malayalam }
            let run = initiallyReordered plan font Scripts.Malayalam [| 0x0D4Eu; 0x0D3Eu |]
            Expect.equal (codepointsOf run) [| 1u; 11u; 2u |] "repha, dotted circle, matra")
        testCase "a broken cluster gets no dotted circle in a font without the glyph or when the buffer asks for none" (fun _ ->
            let bare = fontMapping [ MatraAa, 8u ]
            Expect.equal (codepointsOf (initiallyReordered newSpecPlan bare Scripts.Devanagari [| MatraAa |])) [| 8u |] "no glyph"
            let flagged = IndicInitial.reorder newSpecPlan plainFont (GlyphRun.withFlags BufferFlags.DoNotInsertDottedCircle (syllabled plainFont Scripts.Devanagari [| MatraAa |]))
            Expect.equal (codepointsOf flagged) [| 8u |] "flag")
        testCase "in Kannada ra, halant and ZWJ become ra, ZWJ and halant, with their clusters merged" (fun _ ->
            let plan = { newSpecPlan with Config = Indic.config Scripts.Kannada }
            let run = initiallyReordered plan plainFont Scripts.Kannada [| KannadaRa; KannadaVirama; Zwj; KannadaKa |]
            Expect.equal (codepointsOf run) [| 22u; 12u; 23u; 21u |] "the ZWJ before the halant"
            Expect.equal (clustersOf run) [| 0u; 1u; 1u; 3u |] "the clusters of the two swapped glyphs merged")
        testCase "a ZWJ after a halant stops the search for the base, which stays the end of the syllable, and requests a half form" (fun _ ->
            let run = initiallyReordered newSpecPlan layoutFont Scripts.Devanagari [| Ra; Virama; Zwj |]
            Expect.equal (positionsOf run) [| 3uy; 3uy; 3uy |] "no glyph is the base, each is before it"
            Expect.equal (masksOf run) [| GlobalMask ||| 0x30u; GlobalMask ||| 0x33u; GlobalMask ||| 0x33u |] "half and below-base masks on all three")
        testCase "a ZWNJ clears the half-form mask of the glyphs before it, back to the previous consonant" (fun _ ->
            let run = initiallyReordered newSpecPlan plainFont Scripts.Devanagari [| Ka; Zwnj; Virama; Kha |]
            Expect.equal (masksOf run) [| GlobalMask ||| 0x20u; GlobalMask ||| 0x33u; GlobalMask ||| 0x33u; GlobalMask ||| 3u |] "half cleared from the consonant only, below-base kept")
        testCase "the initial reordering of an empty run is the run" (fun _ ->
            let run = syllabled plainFont Scripts.Devanagari [||]
            Expect.equal (IndicInitial.reorder newSpecPlan plainFont run).Infos.Length 0 "empty")
        testCase "a reph that ligated moves to after the base in Devanagari and its cluster merges with what it passes" (fun _ ->
            let run = shaped layoutFont Scripts.Devanagari [| Ra; Virama; Ka |]
            Expect.equal (codepointsOf run) [| 1u; RephGlyph |] "the base, then the reph"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "the reph of Bengali goes after the subjoined consonants and so after the base here" (fun _ ->
            let mappingBengali = [ 0x0995u, 1u; 0x09B0u, 3u; 0x09CDu, 5u ]
            let font = fontWithGsub mappingBengali 100u (gsubOf "bng2" [ "rphf", [ 0u ] ] [ ligatureLookup [ 3u ] [ 5u ] RephGlyph ])
            let run = shaped font Scripts.Bengali [| 0x09B0u; 0x09CDu; 0x0995u |]
            Expect.equal (codepointsOf run) [| 1u; RephGlyph |] "the base, then the reph")
        testCase "a left matra stays before the consonant after the final reordering, its cluster merged with the consonant's" (fun _ ->
            let run = shaped layoutFont Scripts.Devanagari [| Ka; MatraI |]
            Expect.equal (codepointsOf run) [| 7u; 1u |] "the matra first"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "a left matra after a halant that follows another consonant moves to after the halant" (fun _ ->
            let run = shaped layoutFont Scripts.Devanagari [| Ka; Virama; Kha; MatraI |]
            Expect.equal (codepointsOf run) [| 1u; 5u; 7u; 2u |] "the matra between the halant and the base")
        testCase "a ZWJ after the halant keeps the left matra at the start of the syllable" (fun _ ->
            let run = shaped layoutFont Scripts.Devanagari [| Ka; Virama; Zwj; Kha; MatraI |]
            Expect.equal (codepointsOf run) [| 7u; 1u; 5u; 2u |] "the matra first, the ZWJ deleted at the end like any default ignorable")
        testCase "a left matra that starts a word gets the init feature and one after a letter does not" (fun _ ->
            let font = fontWithGsub mapping 100u (gsubOf "dev2" [ "init", [ 0u ] ] [ singleLookup [ 7u ] 50u ])
            Expect.equal (codepointsOf (shaped font Scripts.Devanagari [| Ka; MatraI |])) [| 57u; 1u |] "the matra at the start of the text is substituted"
            Expect.equal (codepointsOf (shaped font Scripts.Devanagari [| 0x41u; Ka; MatraI |])) [| 0u; 7u; 1u |] "a letter before: not a word start"
            Expect.equal (codepointsOf (shaped font Scripts.Devanagari [| 0x20u; Ka; MatraI |])) [| 0u; 57u; 1u |] "a space before: a word start")
        testCase "a vowel followed by a vowel sign that the script prohibits gets a dotted circle between them" (fun _ ->
            let plan = indicPlanFor Scripts.Devanagari
            let font = fontMapping [ LetterA, 1u; 0x093Au, 2u; DottedCircle, 11u ]
            let script = { Direction = Direction.Ltr; Script = Scripts.Devanagari; Language = NoLanguage }
            let run = ShaperHooks.preprocess plan font (GlyphRun.withSegment script (runOf [| LetterA; 0x093Au |]))
            Expect.equal (codepointsOf run) [| LetterA; DottedCircle; 0x093Au |] "the dotted circle before the second code point"
            let flagged = ShaperHooks.preprocess plan font (GlyphRun.withFlags BufferFlags.DoNotInsertDottedCircle (GlyphRun.withSegment script (runOf [| LetterA; 0x093Au |])))
            Expect.equal (codepointsOf flagged) [| LetterA; 0x093Au |] "no dotted circle when the buffer asks for none")
        testCase "the letters whose decomposition would split what fonts treat as one are kept whole" (fun _ ->
            let plan = indicPlanFor Scripts.Devanagari
            for letter in [ 0x0931u; 0x09DCu; 0x09DDu; 0x0B94u ] do
                Expect.equal (ShaperHooks.decompose plan plainFont letter) ValueNone $"U+{letter:X4}"
            Expect.equal (ShaperHooks.decompose plan plainFont 0x09CBu) (ValueSome (struct (0x09C7u, 0x09BEu))) "a split matra decomposes as Unicode says"
            Expect.equal (ShaperHooks.decompose plan plainFont 0x0958u) (Unicode.decompose 0x0958u) "the others are Unicode's")
        testCase "a mark is never composed with what follows it, the Bengali yya is composed though Unicode excludes it" (fun _ ->
            let plan = indicPlanFor Scripts.Bengali
            Expect.equal (ShaperHooks.compose plan 0x09C7u 0x09BEu) ValueNone "a split matra stays apart"
            Expect.equal (ShaperHooks.compose plan 0x09AFu 0x09BCu) (ValueSome 0x09DFu) "ya and nukta"
            Expect.equal (Unicode.compose 0x09AFu 0x09BCu) ValueNone "Unicode excludes it"
            Expect.equal (ShaperHooks.compose plan 0x0928u 0x093Cu) (Unicode.compose 0x0928u 0x093Cu) "the others are Unicode's")
        testCase "the reph goes where its script puts it: after the main consonant, after the subjoined ones, or at the end" (fun _ ->
            let rows = [ 40u, IndicMachine.Ra, IndicPosition.PosRaToBecomeReph; 1u, IndicMachine.C, IndicPosition.PosBaseC; 2u, IndicMachine.C, IndicPosition.PosPostC ]
            Expect.equal (rephPlacedFor Scripts.Oriya rows) [| 1u; 40u; 2u |] "Oriya: after the main consonant"
            Expect.equal (rephPlacedFor Scripts.Bengali rows) [| 1u; 40u; 2u |] "Bengali: before the first post-base consonant"
            Expect.equal (rephPlacedFor Scripts.Devanagari rows) [| 1u; 2u; 40u |] "Devanagari: at the end of the syllable"
            Expect.equal (rephPlacedFor Scripts.Tamil rows) [| 1u; 2u; 40u |] "Tamil: after the post-base consonants")
        testCase "a reph that ends the syllable goes before its syllable modifier signs" (fun _ ->
            let rows = [ 40u, IndicMachine.Ra, IndicPosition.PosRaToBecomeReph; 1u, IndicMachine.C, IndicPosition.PosBaseC; 9u, IndicMachine.SM, IndicPosition.PosSmvd ]
            Expect.equal (rephPlacedFor Scripts.Devanagari rows) [| 1u; 40u; 9u |] "before the anusvara")
        testCase "a reph that would end after a matra and a halant goes before the halant, so that it can interact with the matra" (fun _ ->
            let rows =
                [ 40u, IndicMachine.Ra, IndicPosition.PosRaToBecomeReph; 1u, IndicMachine.C, IndicPosition.PosBaseC
                  8u, IndicMachine.M, IndicPosition.PosAfterPost; 5u, IndicMachine.H, IndicPosition.PosAfterPost ]
            Expect.equal (rephPlacedFor Scripts.Devanagari rows) [| 1u; 8u; 40u; 5u |] "after the matra, before the halant"
            let plain = [ 40u, IndicMachine.Ra, IndicPosition.PosRaToBecomeReph; 1u, IndicMachine.C, IndicPosition.PosBaseC; 5u, IndicMachine.H, IndicPosition.PosAfterPost ]
            Expect.equal (rephPlacedFor Scripts.Devanagari plain) [| 1u; 5u; 40u |] "a halant after a consonant, with no matra, keeps the reph after it")
        testCase "the base found again goes back over the halants and nuktas that follow the real base" (fun _ ->
            let rows = [ 1u, IndicMachine.C, IndicPosition.PosPreC; 7u, IndicMachine.M, IndicPosition.PosPreM; 5u, IndicMachine.H, IndicPosition.PosBaseC ]
            let run = IndicFinal.reorder newSpecPlan plainFont (afterBasicFeatures false rows)
            Expect.equal (codepointsOf run) [| 1u; 7u; 5u |] "no glyph moves"
            Expect.equal (clustersOf run) [| 0u; 1u; 2u |] "the left matra is before the base, which is the glyph before the halant, so no cluster merges")
        testCase "a base lost to a ZWJ at the end is the glyph before it, and a halant then has the joiner moved after it" (fun _ ->
            let rows =
                [ 40u, IndicMachine.Ra, IndicPosition.PosRaToBecomeReph; 1u, IndicMachine.C, IndicPosition.PosPreC
                  5u, IndicMachine.H, IndicPosition.PosPreC; 12u, IndicMachine.ZWJ, IndicPosition.PosPreC ]
            Expect.equal (rephPlacedFor Scripts.Devanagari rows) [| 1u; 5u; 40u; 12u |] "the reph goes after the halant and before the ZWJ")
        testCase "in Malayalam the base skips over a below-form the font did not form, and a left matra goes in front of it" (fun _ ->
            let rows = [ 7u, IndicMachine.M, IndicPosition.PosPreM; 1u, IndicMachine.C, IndicPosition.PosBaseC; 5u, IndicMachine.H, IndicPosition.PosBaseC; 3u, IndicMachine.C, IndicPosition.PosBelowC ]
            let reordered (script: Script) =
                let run = GlyphRun.withSegment { Direction = Direction.Ltr; Script = script; Language = NoLanguage } (afterBasicFeatures false rows)
                codepointsOf (IndicFinal.reorder { newSpecPlan with Config = Indic.config script } plainFont run)
            Expect.equal (reordered Scripts.Malayalam) [| 1u; 5u; 7u; 3u |] "the matra before the below-form"
            Expect.equal (reordered Scripts.Devanagari) [| 7u; 1u; 5u; 3u |] "other scripts do not skip it")
        testCase "in old-spec Devanagari a ra and halant before the base get the below-base forms, the eyelash ra, unless a ZWJ follows" (fun _ ->
            let plan = { newSpecPlan with IsOldSpec = true }
            let plain = initiallyReordered plan plainFont Scripts.Devanagari [| Ya; Virama; Ra; Virama; Ka |]
            Expect.equal (masksOf plain) [| GlobalMask ||| 0x10u; GlobalMask ||| 0x13u; GlobalMask ||| 0x33u; GlobalMask ||| 0x33u; GlobalMask ||| 3u |] "below-base on the ra and its halant"
            let forced = initiallyReordered plan plainFont Scripts.Devanagari [| Ya; Virama; Ra; Virama; Zwj; Ka |]
            Expect.equal (masksOf forced) [| GlobalMask ||| 0x10u; GlobalMask ||| 0x13u; GlobalMask ||| 0x13u; GlobalMask ||| 0x13u; GlobalMask ||| 0x13u; GlobalMask ||| 3u |] "half forms only")
        testCase "the staged runner runs the three pauses even when the font has no GSUB table" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Devanagari; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props IndicShaper ImmutableArray.Empty
            let stages = ImmutableArray.CreateRange [ for pause in collected.GsubPauses -> { LastLookup = 0; Pause = pause.StagePause } ]
            let map = { OtMap.empty with GsubStages = stages; HasPauses = true }
            let plan = { indicPlanFor Scripts.Devanagari with Map = map; Data = IndicData newSpecPlan }
            let run = OtLayout.substitute plan plainFont (asGlyphs plainFont (categorized [| Ka; MatraI |]))
            Expect.equal (codepointsOf run) [| 7u; 1u |] "reordered by the pauses"
            Expect.equal (clustersOf run) [| 0u; 0u |] "merged by the final reordering"
            Expect.equal (masksOf run) [| GlobalMask ||| 0x130u; GlobalMask |] "the left matra has the half, below-base and init masks, the merge having cleared its flags")
        testCase "the pauses of the shaper return the run when the plan carries no Indic data" (fun _ ->
            let run = categorized [| Ka; MatraI |]
            let plan = { indicPlanFor Scripts.Devanagari with Data = NoShaperData }
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.applyPause IndicInitialReordering plan plainFont run, run)) "initial"
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.applyPause IndicFinalReordering plan plainFont run, run)) "final")
        Conformance.files "conformance" conformanceFiles
    ]
