module Farfbuzz.Tests.ComplexShapers.MyanmarTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>The letter ka, <c>U+1000</c>.</summary>
[<Literal>]
let private Ka = 0x1000u

/// <summary>The letter ra, <c>U+101B</c>, the reph's first glyph.</summary>
[<Literal>]
let private Ra = 0x101Bu

/// <summary>The sign asat, <c>U+103A</c>, the reph's second glyph.</summary>
[<Literal>]
let private Asat = 0x103Au

/// <summary>The sign virama, <c>U+1039</c>, the reph's third glyph.</summary>
[<Literal>]
let private Virama = 0x1039u

/// <summary>The vowel sign e, a pre-base vowel, <c>U+1031</c>.</summary>
[<Literal>]
let private VowelE = 0x1031u

/// <summary>The vowel sign u, a below-base vowel, <c>U+102F</c>.</summary>
[<Literal>]
let private VowelU = 0x102Fu

/// <summary>The sign dot below, <c>U+1037</c>.</summary>
[<Literal>]
let private DotBelow = 0x1037u

/// <summary>The sign anusvara, <c>U+1036</c>, the above-base mark of the <c>A</c> category.</summary>
[<Literal>]
let private Anusvara = 0x1036u

/// <summary>The consonant sign medial ra, <c>U+103C</c>.</summary>
[<Literal>]
let private MedialRa = 0x103Cu

/// <summary>The variation selector one, <c>U+FE00</c>.</summary>
[<Literal>]
let private Vs1 = 0xFE00u

/// <summary>The dotted circle, <c>U+25CC</c>.</summary>
[<Literal>]
let private DottedCircle = 0x25CCu

/// <summary>The glyph of the dotted circle in the test font.</summary>
[<Literal>]
let private DottedCircleGlyph = 99u

/// <summary>A font with a dotted circle and no layout table, mapping every code point the tests use.</summary>
let private circleFont =
    fontMapping [ DottedCircle, DottedCircleGlyph; Ka, 1u; Ra, 2u; Asat, 3u; Virama, 4u; VowelE, 5u; VowelU, 6u; DotBelow, 7u; Anusvara, 8u; MedialRa, 9u; Vs1, 10u ]

/// <summary>A font without a dotted circle.</summary>
let private bareFont = fontMapping [ Ka, 1u; VowelE, 2u ]

/// <summary>The global mask every glyph of the tests starts with.</summary>
[<Literal>]
let private GlobalMask = 0x80000000u

/// <summary>The plan of the Myanmar shaper.</summary>
let private myanmarPlan = planFor MyanmarShaper Scripts.Myanmar

/// <summary>A run of the given code points with its categories set, ready for the syllable pause.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private categorized (codepoints: uint32 array) : GlyphRun =
    Myanmar.setupMasks (runWith ClusterLevel.MonotoneGraphemes GlobalMask codepoints)

/// <summary>A run of the given code points with its categories and syllables set, ready for the reordering pause.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private syllabled (codepoints: uint32 array) : GlyphRun =
    Myanmar.setupSyllables (categorized codepoints)

/// <summary>Reorders the syllables of the given code points.</summary>
/// <param name="font">The font.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The reordered run.</returns>
let private reordered (font: Font) (codepoints: uint32 array) : GlyphRun =
    Myanmar.reorder font (syllabled codepoints)

/// <summary>Reads the syllable byte of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The syllable bytes.</returns>
let private syllablesOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.Syllable |]

/// <summary>Reads the position of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The position bytes.</returns>
let private positionsOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.ShaperAuxiliary |]

/// <summary>The stages of the Myanmar map: the common pause after <c>rvrn</c>, the two pauses of the shaper, a pause after
/// each of the four basic features, the pause that clears the syllables, then the rest, none holding a lookup.</summary>
let private myanmarStages : ImmutableArray<StageMap> =
    ImmutableArray.CreateRange
        [ { LastLookup = 0; Pause = NoPause }
          { LastLookup = 0; Pause = MyanmarSetupSyllables }
          { LastLookup = 0; Pause = MyanmarReorder }
          { LastLookup = 0; Pause = NoPause }
          { LastLookup = 0; Pause = NoPause }
          { LastLookup = 0; Pause = NoPause }
          { LastLookup = 0; Pause = NoPause }
          { LastLookup = 0; Pause = ClearSyllables }
          { LastLookup = 0; Pause = NoPause } ]

/// <summary>The P6.S21 Myanmar shaper tests: the records, the features and pauses, the categories, the syllables, the
/// reordering with its reph, medial ra, left matra and below-base rules, the dotted circles, the staged runner and the
/// upstream conformance lines.</summary>
[<Tests>]
let tests =
    testList "P6.S21" [
        testCase "myanmar shaper record" (fun _ ->
            let record = Categorize.profile MyanmarShaper
            Expect.equal record.Kind MyanmarShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationComposedDiacriticsNoShortCircuit "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksByGdefEarly "zero-width marks"
            Expect.isFalse record.FallbackPosition "fallback position")
        testCase "myanmar zawgyi shaper record" (fun _ ->
            let record = Categorize.profile MyanmarZawgyiShaper
            Expect.equal record.Kind MyanmarZawgyiShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationNone "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksNone "zero-width marks"
            Expect.isFalse record.FallbackPosition "fallback position")
        testCase "collect_features adds the two pauses, one pause after each basic feature and the pause that clears the syllables" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Myanmar; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props MyanmarShaper ImmutableArray.Empty
            let pauses = [ for pause in collected.GsubPauses -> pause.StagePause ]
            Expect.equal pauses [ NoPause; MyanmarSetupSyllables; MyanmarReorder; NoPause; NoPause; NoPause; NoPause; ClearSyllables ] "pauses in order"
            let staged = [ for info in collected.Features -> Tag.toString info.InfoTag, info.GsubStage ]
            let stageOf (name: string) = staged |> List.find (fun (tag, _) -> tag = name) |> snd
            Expect.equal (stageOf "rvrn") 0 "rvrn before every pause"
            Expect.equal (stageOf "locl") 2 "locl after the two pauses of the shaper"
            Expect.equal (stageOf "ccmp") 2 "ccmp"
            Expect.equal ([ "rphf"; "pref"; "blwf"; "pstf" ] |> List.map stageOf) [ 3; 4; 5; 6 ] "one basic feature per stage"
            Expect.equal ([ "pres"; "abvs"; "blws"; "psts" ] |> List.map stageOf) [ 8; 8; 8; 8 ] "features after the pause that clears the syllables"
            Expect.isFalse collected.IsSimple "a hook makes the map non-simple")
        testCase "the basic features keep joiners and stay within a syllable, the others are global and keep joiners" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Myanmar; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props MyanmarShaper ImmutableArray.Empty
            let flagsOf (name: string) = collected.Features |> Seq.find (fun info -> Tag.toString info.InfoTag = name) |> fun info -> info.InfoFlags
            Expect.equal (flagsOf "rphf") (FeatureFlags.Global ||| FeatureFlags.ManualZwj ||| FeatureFlags.PerSyllable) "rphf"
            Expect.equal (flagsOf "psts") (FeatureFlags.Global ||| FeatureFlags.ManualZwj) "psts"
            Expect.equal (flagsOf "locl") (FeatureFlags.Global ||| FeatureFlags.PerSyllable) "locl")
        testCase "setup_masks saves the category of every glyph and changes nothing else" (fun _ ->
            let run = categorized [| Ka; Asat; Virama; VowelE; VowelU; DotBelow; Anusvara; MedialRa; Vs1; 0x41u |]
            Expect.equal [| for props in run.Props -> props.ShaperCategory |] [| 1uy; 32uy; 4uy; 22uy; 21uy; 3uy; 9uy; 36uy; 40uy; 0uy |] "categories"
            Expect.equal (masksOf run) (Array.create 10 GlobalMask) "masks")
        testCase "an empty run has no syllable and no category" (fun _ ->
            let run = Myanmar.setupSyllables (Myanmar.setupMasks (runOf [||]))
            Expect.equal run.Infos.Length 0 "no glyph"
            Expect.equal run.ScratchFlags ScratchFlags.Default "no broken syllable")
        testCase "a consonant is one consonant syllable" (fun _ ->
            let run = syllabled [| Ka |]
            Expect.equal (syllablesOf run) [| 0x10uy |] "serial one, consonant syllable"
            Expect.equal (run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "no broken syllable")
        testCase "a letter that is not Myanmar is a non-Myanmar cluster and the serial counts the syllables" (fun _ ->
            let run = syllabled [| 0x41u; Ka; Ka |]
            Expect.equal (syllablesOf run) [| 0x12uy; 0x20uy; 0x30uy |] "non-Myanmar cluster, then two consonant syllables")
        testCase "a vowel sign with no consonant is a broken cluster and sets the scratch flag" (fun _ ->
            let run = syllabled [| VowelE |]
            Expect.equal (syllablesOf run) [| 0x11uy |] "broken cluster"
            Expect.notEqual (run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "flag")
        testCase "every syllable is marked unsafe to break where its clusters differ" (fun _ ->
            let run = syllabled [| Ka; VowelE; Ka |]
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask ||| 3u; GlobalMask |] "the glyph after the first cluster of the syllable")
        testCase "a consonant alone is not reordered" (fun _ ->
            let run = reordered circleFont [| Ka |]
            Expect.equal (codepointsOf run) [| Ka |] "unchanged"
            Expect.equal (positionsOf run) [| 4uy |] "the base position")
        testCase "a pre-base vowel moves in front of its base and the clusters merge" (fun _ ->
            let run = reordered circleFont [| Ka; VowelE |]
            Expect.equal (codepointsOf run) [| VowelE; Ka |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster"
            Expect.equal (masksOf run) [| GlobalMask; GlobalMask |] "the unsafe-to-break flags went with the merge")
        testCase "a medial ra moves in front of the base" (fun _ ->
            let run = reordered circleFont [| Ka; MedialRa |]
            Expect.equal (codepointsOf run) [| MedialRa; Ka |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "a variation selector keeps the position of the pre-base vowel it follows" (fun _ ->
            let run = reordered circleFont [| Ka; VowelE; Vs1 |]
            Expect.equal (codepointsOf run) [| VowelE; Vs1; Ka |] "the vowel and its selector move together")
        testCase "a reph of ra, asat and virama moves after the base" (fun _ ->
            let run = reordered circleFont [| Ra; Asat; Virama; Ka |]
            Expect.equal (codepointsOf run) [| Ka; Ra; Asat; Virama |] "the reph moved after the base"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u; 0u |] "one cluster")
        testCase "a below-base vowel takes the below position and an above-base mark moves before it" (fun _ ->
            let run = reordered circleFont [| Ka; VowelU; Anusvara |]
            Expect.equal (codepointsOf run) [| Ka; Anusvara; VowelU |] "the above-base mark moved before the below-base vowel"
            Expect.equal (positionsOf run) [| 4uy; 7uy; 8uy |] "base, before sub, below")
        testCase "a dot below does not move before the below-base vowel" (fun _ ->
            let run = reordered circleFont [| Ka; VowelU; DotBelow |]
            Expect.equal (codepointsOf run) [| Ka; VowelU; DotBelow |] "the dot below stays after the below-base vowel")
        testCase "a broken cluster gets a dotted circle in front, then the reordering reads it as the base" (fun _ ->
            let run = reordered circleFont [| VowelE |]
            Expect.equal (codepointsOf run) [| VowelE; DottedCircleGlyph |] "the vowel moved in front of the dotted circle"
            Expect.equal (syllablesOf run) [| 0x11uy; 0x11uy |] "same syllable"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "a broken cluster gets no dotted circle in a font without the glyph or when the buffer asks for none" (fun _ ->
            Expect.equal (codepointsOf (reordered bareFont [| VowelE |])) [| VowelE |] "no glyph"
            let flagged = Myanmar.reorder circleFont (GlyphRun.withFlags BufferFlags.DoNotInsertDottedCircle (syllabled [| VowelE |]))
            Expect.equal (codepointsOf flagged) [| VowelE |] "flag")
        testCase "the staged runner runs the pause functions even when the font has no GSUB table" (fun _ ->
            let map = { OtMap.empty with GsubStages = myanmarStages; HasPauses = true }
            let plan = { myanmarPlan with Map = map }
            let run = OtLayout.substitute plan circleFont (categorized [| Ka; VowelE; VowelU |])
            Expect.equal (codepointsOf run) [| VowelE; Ka; VowelU |] "reordered by the pause"
            Expect.equal (syllablesOf run) [| 0x10uy; 0x10uy; 0x10uy |] "syllables set by the pause")
        Conformance.files "conformance" [ "in-house", "myanmar-misc.tests"; "in-house", "myanmar-syllable.tests"; "in-house", "myanmar-zawgyi.tests" ]
    ]
