module Farfbuzz.Tests.ComplexShapers.KhmerTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>The consonant ka, <c>U+1780</c>.</summary>
[<Literal>]
let private Ka = 0x1780u

/// <summary>The consonant kha, <c>U+1781</c>.</summary>
[<Literal>]
let private Kha = 0x1781u

/// <summary>The consonant ro, <c>U+179A</c>.</summary>
[<Literal>]
let private Ro = 0x179Au

/// <summary>The sign coeng, <c>U+17D2</c>.</summary>
[<Literal>]
let private Coeng = 0x17D2u

/// <summary>The vowel sign e, a pre-base vowel, <c>U+17C1</c>.</summary>
[<Literal>]
let private VowelE = 0x17C1u

/// <summary>The vowel sign i, an above-base vowel, <c>U+17B7</c>.</summary>
[<Literal>]
let private VowelI = 0x17B7u

/// <summary>The dotted circle, <c>U+25CC</c>.</summary>
[<Literal>]
let private DottedCircle = 0x25CCu

/// <summary>The glyph of the dotted circle in the test font.</summary>
[<Literal>]
let private DottedCircleGlyph = 99u

/// <summary>A font with a dotted circle and no layout table.</summary>
let private circleFont = fontMapping [ DottedCircle, DottedCircleGlyph; Ka, 1u ]

/// <summary>A font without a dotted circle.</summary>
let private bareFont = fontMapping [ Ka, 1u ]

/// <summary>The masks of the plan data the reordering tests use: <c>pref</c>, <c>blwf</c>, <c>abvf</c>, <c>pstf</c>, <c>cfar</c>.</summary>
let private masks : KhmerPlan = { Pref = 0x10u; Blwf = 0x20u; Abvf = 0x40u; Pstf = 0x80u; Cfar = 0x100u }

/// <summary>The mask the post-base features give together.</summary>
[<Literal>]
let private PostBase = 0xE0u

/// <summary>The global mask every glyph of the tests starts with.</summary>
[<Literal>]
let private GlobalMask = 0x80000000u

/// <summary>The plan of the Khmer shaper.</summary>
let private khmerPlan = planFor KhmerShaper Scripts.Khmer

/// <summary>A run of the given code points with its categories set, ready for the syllable pause.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private categorized (codepoints: uint32 array) : GlyphRun =
    Khmer.setupMasks (runWith ClusterLevel.MonotoneGraphemes GlobalMask codepoints)

/// <summary>A run of the given code points with its categories and syllables set, ready for the reordering pause.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private syllabled (codepoints: uint32 array) : GlyphRun =
    Khmer.setupSyllables (categorized codepoints)

/// <summary>Reorders the syllables of the given code points.</summary>
/// <param name="font">The font.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The reordered run.</returns>
let private reordered (font: Font) (codepoints: uint32 array) : GlyphRun =
    Khmer.reorder masks font (syllabled codepoints)

/// <summary>Reads the syllable byte of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The syllable bytes.</returns>
let private syllablesOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.Syllable |]

/// <summary>The stages of the Khmer map: the stage of <c>rvrn</c> ends in the common pause, then the two pauses of the
/// shaper, then the basic features and the pause that clears the syllables, then the rest, none holding a lookup.</summary>
let private khmerStages : ImmutableArray<StageMap> =
    ImmutableArray.CreateRange
        [ { LastLookup = 0; Pause = NoPause }
          { LastLookup = 0; Pause = KhmerSetupSyllables }
          { LastLookup = 0; Pause = KhmerReorder }
          { LastLookup = 0; Pause = ClearSyllables }
          { LastLookup = 0; Pause = NoPause } ]

/// <summary>The P6.S20 Khmer shaper tests: the record, the features and pauses, the plan data, the syllables, the dotted
/// circles, the reordering, the split vowels, the staged runner and the upstream conformance lines.</summary>
[<Tests>]
let tests =
    testList "P6.S20" [
        testCase "khmer shaper record" (fun _ ->
            let record = Categorize.profile KhmerShaper
            Expect.equal record.Kind KhmerShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationComposedDiacriticsNoShortCircuit "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksNone "zero-width marks"
            Expect.isFalse record.FallbackPosition "fallback position")
        testCase "collect_features adds the two pauses before any lookup, the basic features, the pause that clears the syllables and the others" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Khmer; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props KhmerShaper ImmutableArray.Empty
            let pauses = [ for pause in collected.GsubPauses -> pause.StagePause ]
            Expect.equal pauses [ NoPause; KhmerSetupSyllables; KhmerReorder; ClearSyllables ] "pauses in order"
            let staged = [ for info in collected.Features -> Tag.toString info.InfoTag, info.GsubStage ]
            let stageOf (name: string) = staged |> List.find (fun (tag, _) -> tag = name) |> snd
            Expect.equal (stageOf "rvrn") 0 "rvrn before every pause"
            Expect.equal (stageOf "locl") 3 "locl after the two pauses of the shaper"
            Expect.equal (stageOf "ccmp") 3 "ccmp"
            Expect.equal ([ "pref"; "blwf"; "abvf"; "pstf"; "cfar" ] |> List.map stageOf) [ 3; 3; 3; 3; 3 ] "basic features"
            Expect.equal ([ "pres"; "abvs"; "blws"; "psts" ] |> List.map stageOf) [ 4; 4; 4; 4 ] "features after the pause that clears the syllables"
            Expect.isFalse collected.IsSimple "a hook makes the map non-simple")
        testCase "the basic features keep joiners and stay within a syllable, the others are global and keep joiners" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Khmer; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props KhmerShaper ImmutableArray.Empty
            let flagsOf (name: string) = collected.Features |> Seq.find (fun info -> Tag.toString info.InfoTag = name) |> fun info -> info.InfoFlags
            Expect.equal (flagsOf "pref") (FeatureFlags.ManualJoiners ||| FeatureFlags.PerSyllable) "pref"
            Expect.equal (flagsOf "cfar") (FeatureFlags.ManualJoiners ||| FeatureFlags.PerSyllable) "cfar"
            Expect.equal (flagsOf "psts") FeatureFlags.GlobalManualJoiners "psts"
            Expect.equal (flagsOf "locl") (FeatureFlags.Global ||| FeatureFlags.PerSyllable) "locl")
        testCase "override_features turns clig on and liga off, after the user features" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Khmer; Language = NoLanguage }
            let user = ImmutableArray.Create { Tag = Tag.ofString "kern"; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            let collected = ShaperHooks.collectFeatures props KhmerShaper user
            let tail = [ for info in collected.Features -> Tag.toString info.InfoTag, info.MaxValue ] |> List.rev |> List.take 2 |> List.rev
            Expect.equal tail [ "clig", 1u; "liga", 0u ] "clig on, liga off")
        testCase "data_create reads the five basic masks and none for the global features" (fun _ ->
            let mask1 (tag: Tag) =
                match Tag.toString tag with
                | "pref" -> 1u
                | "blwf" -> 2u
                | "abvf" -> 4u
                | "pstf" -> 8u
                | "cfar" -> 16u
                | _ -> 0u
            Expect.equal (ShaperHooks.createData KhmerShaper mask1 { Direction = Direction.Ltr; Script = Scripts.Khmer; Language = NoLanguage } OtMap.empty) (KhmerData { Pref = 1u; Blwf = 2u; Abvf = 4u; Pstf = 8u; Cfar = 16u }) "masks")
        testCase "setup_masks saves the category of every glyph and changes nothing else" (fun _ ->
            let run = categorized [| Ka; Coeng; Ro; VowelE; VowelI; 0x41u |]
            Expect.equal [| for props in run.Props -> props.ShaperCategory |] [| 1uy; 4uy; 15uy; 22uy; 20uy; 0uy |] "categories"
            Expect.equal (masksOf run) (Array.create 6 GlobalMask) "masks")
        testCase "an empty run has no syllable and no category" (fun _ ->
            let run = Khmer.setupSyllables (Khmer.setupMasks (runOf [||]))
            Expect.equal run.Infos.Length 0 "no glyph"
            Expect.equal run.ScratchFlags ScratchFlags.Default "no broken syllable")
        testCase "a consonant with a coeng and ro is one consonant syllable" (fun _ ->
            let run = syllabled [| Ka; Coeng; Ro |]
            Expect.equal (syllablesOf run) [| 0x10uy; 0x10uy; 0x10uy |] "serial one, consonant syllable"
            Expect.equal (run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "no broken syllable")
        testCase "a letter that is not Khmer is a non-Khmer cluster and the serial counts the syllables" (fun _ ->
            let run = syllabled [| 0x41u; Ka; Ka |]
            Expect.equal (syllablesOf run) [| 0x12uy; 0x20uy; 0x30uy |] "non-Khmer cluster, then two consonant syllables")
        testCase "a vowel sign with no consonant is a broken cluster and sets the scratch flag" (fun _ ->
            let run = syllabled [| VowelI |]
            Expect.equal (syllablesOf run) [| 0x11uy |] "broken cluster"
            Expect.notEqual (run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "flag")
        testCase "every syllable is marked unsafe to break where its clusters differ" (fun _ ->
            let run = syllabled [| Ka; Coeng; Ro; Kha |]
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask ||| 3u; GlobalMask ||| 3u; GlobalMask |] "the glyphs after the first cluster of the syllable")
        testCase "a syllable of one glyph is not marked" (fun _ ->
            Expect.equal (masksOf (syllabled [| Ka; Kha |])) [| GlobalMask; GlobalMask |] "no flag")
        testCase "the syllable pass changes no glyph" (fun _ ->
            let run = categorized [| Ka; VowelE; Coeng; Ro |]
            Expect.equal (codepointsOf (Khmer.setupSyllables run)) (codepointsOf run) "same glyphs")
        testCase "a pre-base vowel moves in front of its consonant and the clusters merge" (fun _ ->
            let run = reordered circleFont [| Ka; VowelE |]
            Expect.equal (codepointsOf run) [| VowelE; Ka |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster"
            Expect.equal (masksOf run) [| GlobalMask ||| PostBase; GlobalMask |] "the post-base masks went with the vowel")
        testCase "a coeng and ro move in front of the base with the pref mask, the glyphs after them take cfar" (fun _ ->
            let run = reordered circleFont [| Ka; Coeng; Ro; Coeng; Kha |]
            Expect.equal (codepointsOf run) [| Coeng; Ro; Ka; Coeng; Kha |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u; 3u; 4u |] "the clusters passed over merge"
            Expect.equal (masksOf run) [| GlobalMask ||| PostBase ||| 0x10u; GlobalMask ||| PostBase ||| 0x10u; GlobalMask; GlobalMask ||| PostBase ||| 0x100u ||| 3u; GlobalMask ||| PostBase ||| 0x100u ||| 3u |] "masks: the glyphs whose clusters merged lose the unsafe-to-break flags, the others keep them")
        testCase "a coeng that is not followed by ro is not moved" (fun _ ->
            let run = reordered circleFont [| Ka; Coeng; Kha |]
            Expect.equal (codepointsOf run) [| Ka; Coeng; Kha |] "unchanged"
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask ||| PostBase ||| 3u; GlobalMask ||| PostBase ||| 3u |] "post-base masks, and the syllable's unsafe-to-break flags")
        testCase "three coengs are allowed: the third coeng and ro still move" (fun _ ->
            let run = reordered circleFont [| Ka; Coeng; Kha; Coeng; Kha; Coeng; Ro |]
            Expect.equal (codepointsOf run) [| Coeng; Ro; Ka; Coeng; Kha; Coeng; Kha |] "moved"
            let fourth = reordered circleFont [| Ka; Coeng; Kha; Coeng; Kha; Coeng; Kha; Coeng; Ro |]
            Expect.equal (codepointsOf fourth) [| Ka; Coeng; Kha; Coeng; Kha; Coeng; Kha; Coeng; Ro |] "the fourth coeng does not open another")
        testCase "a coeng at the end of the run is left where it is" (fun _ ->
            let run = reordered circleFont [| Ka; Coeng |]
            Expect.equal (codepointsOf run |> Array.filter (fun glyph -> glyph <> DottedCircleGlyph)) [| Ka; Coeng |] "unchanged")
        testCase "a pre-base vowel and a coeng and ro in one syllable are both moved, the vowel last" (fun _ ->
            let run = reordered circleFont [| Ka; Coeng; Ro; VowelE |]
            Expect.equal (codepointsOf run) [| VowelE; Coeng; Ro; Ka |] "moved")
        testCase "a consonant alone is not reordered and a non-Khmer cluster is left alone" (fun _ ->
            let run = reordered circleFont [| 0x41u; Ka |]
            Expect.equal (codepointsOf run) [| 0x41u; Ka |] "unchanged"
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask |] "no mask")
        testCase "a broken cluster gets a dotted circle in front, then the reordering reads it as the base" (fun _ ->
            let run = reordered circleFont [| VowelE |]
            Expect.equal (codepointsOf run) [| VowelE; DottedCircleGlyph |] "the vowel moved in front of the dotted circle"
            Expect.equal (syllablesOf run) [| 0x11uy; 0x11uy |] "same syllable"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "a broken cluster gets no dotted circle in a font without the glyph or when the buffer asks for none" (fun _ ->
            Expect.equal (codepointsOf (reordered bareFont [| VowelI |])) [| VowelI |] "no glyph"
            let flagged = Khmer.reorder masks circleFont (GlyphRun.withFlags BufferFlags.DoNotInsertDottedCircle (syllabled [| VowelI |]))
            Expect.equal (codepointsOf flagged) [| VowelI |] "flag")
        testCase "a syllable longer than the limit is reordered in chunks of the limit" (fun _ ->
            let run = syllabled (Array.concat [ [| Ka |]; Array.concat (Array.create 35 [| Coeng; Kha |]); [| VowelE |] ])
            let chunks = SyllableEdit.chunks run.Props |> Seq.map (fun (struct (start, finish)) -> finish - start) |> Seq.toList
            Expect.equal chunks [ 64; 8 ] "one syllable of 72 glyphs, chunks of at most 64"
            let result = Khmer.reorder masks circleFont run
            Expect.equal result.Infos[0].Codepoint Ka "the first chunk keeps its consonant first"
            Expect.equal result.Infos[64].Codepoint VowelE "the pre-base vowel moved to the start of its chunk, not of the syllable")
        testCase "the split vowels decompose into the vowel sign e and themselves" (fun _ ->
            for vowel in [ 0x17BEu; 0x17BFu; 0x17C0u; 0x17C4u; 0x17C5u ] do
                Expect.equal (ShaperHooks.decompose khmerPlan circleFont vowel) (ValueSome (struct (0x17C1u, vowel))) $"U+{vowel:X4}"
            Expect.equal (ShaperHooks.decompose khmerPlan circleFont 0x17C1u) (Unicode.decompose 0x17C1u) "other code points are Unicode's"
            Expect.equal (ShaperHooks.decompose khmerPlan circleFont 0xE1u) (Unicode.decompose 0xE1u) "a Latin one")
        testCase "a mark is never composed with what follows it, anything else is Unicode's" (fun _ ->
            Expect.equal (ShaperHooks.compose khmerPlan 0x0301u 0x0301u) ValueNone "a mark first"
            Expect.equal (ShaperHooks.compose khmerPlan 0x61u 0x0301u) (Unicode.compose 0x61u 0x0301u) "a letter first")
        testCase "the staged runner runs the pause functions even when the font has no GSUB table" (fun _ ->
            let map = { OtMap.empty with GsubStages = khmerStages; HasPauses = true }
            let plan = { khmerPlan with Map = map; Data = KhmerData masks }
            let run = OtLayout.substitute plan circleFont (categorized [| Ka; VowelE; VowelI |])
            Expect.equal (codepointsOf run) [| VowelE; Ka; VowelI |] "reordered by the pause"
            Expect.equal (syllablesOf run) [| 0x10uy; 0x10uy; 0x10uy |] "syllables set by the pause")
        testCase "a map whose pauses run no function is applied in one pass and leaves the run as it is" (fun _ ->
            let map = { OtMap.empty with GsubStages = ImmutableArray.CreateRange [ { LastLookup = 0; Pause = NoPause }; { LastLookup = 0; Pause = ClearSyllables } ] }
            let plan = { khmerPlan with Map = map }
            let run = categorized [| Ka; VowelE |]
            Expect.isTrue (obj.ReferenceEquals(OtLayout.substitute plan circleFont run, run)) "same run")
        testCase "the pause that clears the syllables is a stage boundary only" (fun _ ->
            let run = categorized [| Ka; VowelE |]
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.applyPause ClearSyllables khmerPlan circleFont run, run)) "same run"
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.applyPause NoPause khmerPlan circleFont run, run)) "no pause")
        testCase "a per-syllable matcher takes the syllable of the glyph the lookup is applied at, any other matcher is unchanged" (fun _ ->
            let run = syllabled [| Ka; Kha; Ka |]
            let perSyllable = Matching.matcherFor 0 false 0u 1u false false true 0uy
            let plain = Matching.matcherFor 0 false 0u 1u false false false 0uy
            let syllableAt index matcher =
                let applied = Matching.atSyllable matcher (run.Props.AsSpan()) index
                applied.Syllable
            Expect.equal [ for index in 0 .. 2 -> syllableAt index perSyllable ] [ 0x10uy; 0x20uy; 0x30uy ] "the syllable of the glyph at the index"
            Expect.equal [ for index in 0 .. 2 -> syllableAt index plain ] [ 0uy; 0uy; 0uy ] "a lookup that is not per syllable keeps its matcher")
        Conformance.files "conformance" [ "in-house", "khmer-mark-order.tests"; "in-house", "khmer-misc.tests" ]
    ]
