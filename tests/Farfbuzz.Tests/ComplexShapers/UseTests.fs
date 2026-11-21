module Farfbuzz.Tests.ComplexShapers.UseTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport
open Farfbuzz.Tests.UpstreamFiles

/// <summary>The original indices of the glyphs the machine_index filter keeps, by its definition (src/hb-ot-shaper-use-machine.hh:909-924):
/// no CGJ, and no ZWNJ whose next glyph that is not a CGJ is a Unicode mark.</summary>
/// <param name="categories">The categories.</param>
/// <param name="marks">The Unicode-mark flags.</param>
/// <returns>The original indices of the kept glyphs.</returns>
let private keptByDefinition (categories: byte array) (marks: bool array) : int array =
    let followedByMark (index: int) =
        match Array.tryFindIndex (fun category -> category <> UseMachine.CGJ) categories[index + 1 ..] with
        | Some offset -> marks[index + 1 + offset]
        | None -> false
    [| for index in 0 .. categories.Length - 1 do
        match categories[index] with
        | category when category = UseMachine.CGJ -> ()
        | category when category = UseMachine.ZWNJ && followedByMark index -> ()
        | _ -> yield index |]

/// <summary>A category run with mark flags over the categories the filter tells apart.</summary>
let private filterCase : Gen<byte array * bool array> =
    gen {
        let! length = Gen.choose (0, 60)
        let! categories = Gen.arrayOfLength length (Gen.elements [ UseMachine.CGJ; UseMachine.ZWNJ; 0uy; 1uy; 2uy ])
        let! marks = Gen.arrayOfLength length (Gen.elements [ false; true ])
        return categories, marks
    }

/// <summary>The filtered run of the filter as the kept categories and the original indices.</summary>
/// <param name="categories">The categories.</param>
/// <param name="marks">The Unicode-mark flags.</param>
/// <returns>The kept categories and their original indices.</returns>
let private filtered (categories: byte array) (marks: bool array) : byte array * int array =
    let struct (kept, positions) = Ragel.useFilter (Sealed.ofSpan (ReadOnlySpan categories)) (Sealed.ofSpan (ReadOnlySpan marks))
    kept.AsSpan().ToArray(), positions.AsSpan().ToArray()

/// <summary>The Javanese letter ka, <c>U+A98F</c>, of category <c>B</c>.</summary>
[<Literal>]
let private Ka = 0xA98Fu

/// <summary>The Javanese vowel sign taling, <c>U+A9BA</c>, of category <c>VPre</c>.</summary>
[<Literal>]
let private Taling = 0xA9BAu

/// <summary>The Javanese vowel sign pepet, <c>U+A9BC</c>, of category <c>VAbv</c>.</summary>
[<Literal>]
let private Pepet = 0xA9BCu

/// <summary>The Javanese consonant sign pangkon, <c>U+A9C0</c>, of category <c>H</c>.</summary>
[<Literal>]
let private Pangkon = 0xA9C0u

/// <summary>The Javanese sign layar, <c>U+A982</c>, of category <c>FAbv</c>.</summary>
[<Literal>]
let private Layar = 0xA982u

/// <summary>The Sharada sign candrabindu-like repha, <c>U+111C2</c>, of category <c>R</c>.</summary>
[<Literal>]
let private Repha = 0x111C2u

/// <summary>The Egyptian hieroglyph, <c>U+13000</c>, of category <c>G</c>.</summary>
[<Literal>]
let private Hieroglyph = 0x13000u

/// <summary>The Mongolian letter a, <c>U+1820</c>, a dual-joining letter.</summary>
[<Literal>]
let private MongolianA = 0x1820u

/// <summary>The Mongolian free variation selector one, <c>U+180B</c>, a transparent mark.</summary>
[<Literal>]
let private FreeVariationSelector = 0x180Bu

/// <summary>The dotted circle, <c>U+25CC</c>.</summary>
[<Literal>]
let private DottedCircle = 0x25CCu

/// <summary>The glyph of the dotted circle in the test font.</summary>
[<Literal>]
let private DottedCircleGlyph = 99u

/// <summary>A font with a dotted circle and no layout table.</summary>
let private circleFont = fontMapping [ DottedCircle, DottedCircleGlyph; Ka, 1u ]

/// <summary>The global mask every glyph of the tests starts with.</summary>
[<Literal>]
let private GlobalMask = 0x80000000u

/// <summary>The mask of <c>rphf</c> in the plan data of the tests.</summary>
[<Literal>]
let private RphfMask = 0x10u

/// <summary>The masks of the topographical features in the plan data of the tests, <c>isol</c>, <c>init</c>, <c>medi</c>, <c>fina</c>.</summary>
let private topographical : ImmutableArray<uint32> = ImmutableArray.CreateRange [ 0x100u; 0x200u; 0x400u; 0x800u ]

/// <summary>The plan data of the tests that are not about a script that joins.</summary>
let private data : UsePlan = { RphfMask = RphfMask; TopographicalMasks = topographical; Arabic = ValueNone }

/// <summary>The plan data of the tests of a script that joins: the masks of the seven actions, <c>isol</c> 0x100, <c>fina</c> 0x200,
/// <c>fin2</c> 0x400, <c>fin3</c> 0x800, <c>medi</c> 0x1000, <c>med2</c> 0x2000, <c>init</c> 0x4000, and none for the action that selects none.</summary>
let private joiningData : UsePlan =
    { data with Arabic = ValueSome { MaskArray = ImmutableArray.CreateRange [ 0x100u; 0x200u; 0x400u; 0x800u; 0x1000u; 0x2000u; 0x4000u; 0u ]; HasStch = false; DoFallback = false } }

/// <summary>A run of the given code points with its categories set, ready for the syllable pause.</summary>
/// <param name="plan">The plan data.</param>
/// <param name="script">The script.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private categorized (plan: UsePlan) (script: Script) (codepoints: uint32 array) : GlyphRun =
    Use.setupMasks script plan (runWith ClusterLevel.MonotoneGraphemes GlobalMask codepoints)

/// <summary>A run of the given code points with its categories and syllables set, ready for the pauses that follow.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private syllabled (codepoints: uint32 array) : GlyphRun =
    Use.setupSyllables data (categorized data Scripts.Javanese codepoints)

/// <summary>Reorders the syllables of the given code points.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The reordered run.</returns>
let private reordered (codepoints: uint32 array) : GlyphRun =
    Use.reorder circleFont (syllabled codepoints)

/// <summary>Reads the syllable byte of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The syllable bytes.</returns>
let private syllablesOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.Syllable |]

/// <summary>Reads the category of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The categories.</returns>
let private categoriesOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.ShaperCategory |]

/// <summary>Reads the auxiliary byte of each glyph of a run, the action of the Arabic joining.</summary>
/// <param name="run">The run.</param>
/// <returns>The bytes.</returns>
let private actionsOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.ShaperAuxiliary |]

/// <summary>A run with the glyph at an index marked as substituted.</summary>
/// <param name="index">The index.</param>
/// <param name="run">The run.</param>
/// <returns>The run.</returns>
let private substitutedAt (index: int) (run: GlyphRun) : GlyphRun =
    { run with Props = run.Props.SetItem(index, { run.Props[index] with GlyphClass = run.Props[index].GlyphClass ||| GlyphPropsBits.Substituted }) }

/// <summary>The stage of a feature in the collected features, by its tag.</summary>
/// <param name="collected">The collected features.</param>
/// <param name="name">The feature tag.</param>
/// <returns>The stage.</returns>
let private stageOf (collected: MapBuilder) (name: string) : int =
    collected.Features |> Seq.find (fun info -> Tag.toString info.InfoTag = name) |> fun info -> info.GsubStage

/// <summary>The flags of a feature in the collected features, by its tag.</summary>
/// <param name="collected">The collected features.</param>
/// <param name="name">The feature tag.</param>
/// <returns>The flags.</returns>
let private flagsOf (collected: MapBuilder) (name: string) : uint32 =
    collected.Features |> Seq.find (fun info -> Tag.toString info.InfoTag = name) |> fun info -> info.InfoFlags

/// <summary>The conformance files that run on the universal shaping engine. Lines 19 to 21 of <c>use-syllable.tests</c> ask for the FreeType
/// font functions (<c>--font-funcs=ft</c>), which the harness does not provide, so the runner skips them: the tests leave them out.</summary>
let private conformanceFiles : (string * string) list =
    [ "in-house", "arabic-like-joining.tests"; "in-house", "arabic-phags-pa.tests"; "in-house", "glyph-props-no-gdef.tests"
      "in-house", "mongolian-variation-selector.tests"; "in-house", "sinhala.tests"; "in-house", "tibetan-contractions-1.tests"
      "in-house", "tibetan-contractions-2.tests"; "in-house", "tibetan-vowels.tests"; "in-house", "use-indic3.tests"
      "in-house", "use-javanese.tests"; "in-house", "use-marchen.tests"; "in-house", "use-syllable.tests"
      "in-house", "use-vowel-letter-spoofing.tests"; "in-house", "use.tests"; "text-rendering-tests", "SHBALI-3.tests" ]

/// <summary>The P6.S22 and P6.S23 tests of the universal shaping engine: the record, the features and pauses, the plan data, the
/// categories, the syllables, the masks of the repha and of the joining forms, the recording of the repha and the pre-base
/// consonant, the reordering, the composition, the Arabic joining and the upstream conformance lines.</summary>
[<Tests>]
let tests =
    testList "P6.S22 P6.S23" [
        testCase "use shaper record" (fun _ ->
            let record = Categorize.profile UseShaper
            Expect.equal record.Kind UseShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationComposedDiacriticsNoShortCircuit "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksByGdefEarly "zero-width marks"
            Expect.isFalse record.FallbackPosition "fallback position")
        testCase "collect_features sets up the syllables, records the repha and the pre-base consonant around the substitution flags, reorders, clears the syllables and ends with a pause of no function" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Javanese; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props UseShaper ImmutableArray.Empty
            let pauses = [ for pause in collected.GsubPauses -> pause.StagePause ]
            Expect.equal pauses [ NoPause; UseSetupSyllables; ClearSubstitutionFlags; UseRecordRphf; ClearSubstitutionFlags; UseRecordPref; UseReorder; ClearSyllables; NoPause ] "pauses in order"
            Expect.equal (stageOf collected "rvrn") 0 "rvrn before every pause"
            Expect.equal ([ "locl"; "ccmp"; "nukt"; "akhn" ] |> List.map (stageOf collected)) [ 2; 2; 2; 2 ] "the default glyph pre-processing group after the syllables are set up"
            Expect.equal (stageOf collected "rphf") 3 "rphf between the pauses that clear the substitution flags"
            Expect.equal (stageOf collected "pref") 5 "pref before the pause that records it"
            Expect.equal ([ "rkrf"; "abvf"; "blwf"; "half"; "pstf"; "vatu"; "cjct" ] |> List.map (stageOf collected)) (List.replicate 7 6) "the basic features before the reordering"
            Expect.equal ([ "isol"; "init"; "medi"; "fina" ] |> List.map (stageOf collected)) (List.replicate 4 8) "the topographical features after the syllables are cleared"
            Expect.equal ([ "abvs"; "blws"; "haln"; "pres"; "psts" ] |> List.map (stageOf collected)) (List.replicate 5 9) "the other features after the last pause"
            Expect.isFalse collected.IsSimple "a hook makes the map non-simple")
        testCase "the flags of the features are those of collect_features_use" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Javanese; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props UseShaper ImmutableArray.Empty
            let perSyllable = FeatureFlags.ManualZwj ||| FeatureFlags.PerSyllable
            for name in [ "locl"; "ccmp"; "nukt" ] do
                Expect.equal (flagsOf collected name) (FeatureFlags.Global ||| FeatureFlags.PerSyllable) $"{name} is global and stays in the syllable"
            for name in [ "akhn"; "pref"; "rkrf"; "abvf"; "blwf"; "half"; "pstf"; "vatu"; "cjct" ] do
                Expect.equal (flagsOf collected name) (FeatureFlags.Global ||| perSyllable) $"{name} is global, keeps ZWJ and stays in the syllable"
            Expect.equal (flagsOf collected "rphf") perSyllable "rphf is not global"
            for name in [ "isol"; "init"; "medi"; "fina" ] do
                Expect.equal (flagsOf collected name) FeatureFlags.None $"{name} has no flags"
            for name in [ "abvs"; "blws"; "haln"; "pres"; "psts" ] do
                Expect.equal (flagsOf collected name) (FeatureFlags.Global ||| FeatureFlags.ManualZwj) $"{name} is global and keeps ZWJ")
        testCase "data_create reads the masks of rphf and of the topographical features, none for one that is global, and the Arabic masks only for a script that joins" (fun _ ->
            let mask1 (tag: Tag) =
                match Tag.toString tag with
                | "rphf" -> 1u
                | "isol" -> 2u
                | "init" -> 4u
                | "medi" -> 8u
                | "fina" -> GlobalMask
                | "fin2" -> 16u
                | _ -> 0u
            let props = { Direction = Direction.Ltr; Script = Scripts.Javanese; Language = NoLanguage }
            let map = { OtMap.empty with GlobalMask = GlobalMask }
            match ShaperHooks.createData UseShaper mask1 props map with
            | UseData plan ->
                Expect.equal plan.RphfMask 1u "rphf"
                Expect.equal (Seq.toList plan.TopographicalMasks) [ 2u; 4u; 8u; 0u ] "isol, init, medi, and none for fina, which is global"
                Expect.equal plan.Arabic ValueNone "no Arabic plan for Javanese"
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> failtest "expected data of the universal shaping engine"
            match ShaperHooks.createData UseShaper mask1 { props with Script = Scripts.Mongolian } map with
            | UseData plan -> Expect.equal (plan.Arabic |> ValueOption.map (fun arabic -> Seq.toList arabic.MaskArray)) (ValueSome [ 2u; GlobalMask; 16u; 0u; 8u; 0u; 4u; 0u ]) "the masks of isol, fina, fin2, fin3, medi, med2, init, and none"
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> failtest "expected data of the universal shaping engine")
        testCase "setup_masks saves the category of every glyph and changes no mask" (fun _ ->
            let run = categorized data Scripts.Javanese [| Ka; Taling; Pangkon; Pepet; Layar; 0x41u |]
            Expect.equal (categoriesOf run) [| 1uy; 22uy; 12uy; 33uy; 24uy; 0uy |] "B, VPre, H, VAbv, FAbv and O"
            Expect.equal (masksOf run) (Array.create 6 GlobalMask) "masks")
        testCase "an empty run has no syllable" (fun _ ->
            let run = Use.setupSyllables data (Use.setupMasks Scripts.Javanese data (runOf [||]))
            Expect.equal run.Infos.Length 0 "no glyph"
            Expect.equal run.ScratchFlags ScratchFlags.Default "no broken syllable")
        testCase "a consonant with its vowel signs is one standard cluster and the serial counts the syllables" (fun _ ->
            let run = syllabled [| Ka; Pepet; Layar; Ka; 0x41u |]
            Expect.equal (syllablesOf run) [| 0x12uy; 0x12uy; 0x12uy; 0x22uy; 0x35uy |] "standard clusters, then a symbol cluster"
            Expect.equal (run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "no broken syllable")
        testCase "a vowel sign with no consonant is a broken cluster and sets the scratch flag" (fun _ ->
            let run = syllabled [| Pepet |]
            Expect.equal (syllablesOf run) [| 0x17uy |] "broken cluster"
            Expect.notEqual (run.ScratchFlags &&& ScratchFlags.HasBrokenSyllable) ScratchFlags.Default "flag")
        testCase "a hieroglyph is a hieroglyph cluster" (fun _ ->
            Expect.equal (syllablesOf (syllabled [| Hieroglyph |])) [| 0x16uy |] "hieroglyph cluster")
        testCase "a zero width non-joiner before a mark is left out of the machine's input and takes the syllable it stands in" (fun _ ->
            let run = syllabled [| Ka; 0x200Cu; Pepet |]
            Expect.equal (syllablesOf run) [| 0x12uy; 0x12uy; 0x12uy |] "the mark joins the consonant and the glyph between them is painted with the syllable"
            let leading = syllabled [| 0x034Fu; Ka |]
            Expect.equal (syllablesOf leading) [| 0x00uy; 0x12uy |] "a glyph left out before the first syllable keeps the value it had")
        testCase "every syllable is marked unsafe to break where its clusters differ" (fun _ ->
            let run = syllabled [| Ka; Pepet; Layar; Ka |]
            Expect.equal (masksOf run |> Array.map (fun mask -> mask &&& 3u)) [| 0u; 3u; 3u; 0u |] "the glyphs after the first cluster of the syllable")
        testCase "the first three glyphs of a syllable get the rphf mask, one when the syllable begins with a repha" (fun _ ->
            let three = syllabled [| Ka; Pangkon; Ka; Pepet |]
            Expect.equal (masksOf three |> Array.map (fun mask -> mask &&& RphfMask)) [| RphfMask; RphfMask; RphfMask; 0u |] "three glyphs"
            let repha = syllabled [| Repha; Ka; Pepet |]
            Expect.equal (masksOf repha |> Array.map (fun mask -> mask &&& RphfMask)) [| RphfMask; 0u; 0u |] "one glyph")
        testCase "a plan without an rphf mask gives no glyph the mask" (fun _ ->
            let none = { data with RphfMask = 0u }
            let run = Use.setupSyllables none (categorized none Scripts.Javanese [| Ka; Pangkon; Ka |])
            Expect.equal (masksOf run |> Array.map (fun mask -> mask &&& RphfMask)) [| 0u; 0u; 0u |] "no mask")
        testCase "the syllable pass changes no glyph" (fun _ ->
            let run = categorized data Scripts.Javanese [| Taling; Ka; Pangkon; Repha |]
            Expect.equal (codepointsOf (Use.setupSyllables data run)) (codepointsOf run) "same glyphs")
        testCase "syllables that join take the initial, medial and final forms, and a syllable alone the isolated form" (fun _ ->
            let masksAfter (codepoints: uint32 array) = masksOf (syllabled codepoints) |> Array.map (fun mask -> mask &&& 0xF00u)
            Expect.equal (masksAfter [| Ka; Ka; Ka |]) [| 0x200u; 0x400u; 0x800u |] "init, medi, fina"
            Expect.equal (masksAfter [| Ka |]) [| 0x100u |] "isol"
            Expect.equal (masksAfter [| Ka; Ka |]) [| 0x200u; 0x800u |] "init, fina")
        testCase "a hieroglyph cluster or a non cluster does not join and breaks the run of joining syllables" (fun _ ->
            let masksAfter (codepoints: uint32 array) = masksOf (syllabled codepoints) |> Array.map (fun mask -> mask &&& 0xF00u)
            Expect.equal (masksAfter [| Ka; Hieroglyph; Ka |]) [| 0x100u; 0u; 0x100u |] "isol, none, isol"
            Expect.equal (masksAfter [| Ka; Hieroglyph; Ka; Ka |]) [| 0x100u; 0u; 0x200u; 0x800u |] "isol, none, init, fina"
            Expect.equal (masksAfter [| Ka; 0x41u; Ka |]) [| 0x200u; 0x400u; 0x800u |] "a symbol cluster joins")
        testCase "a joining form replaces the masks of the forms the glyph had and keeps the others" (fun _ ->
            let run = Use.setupSyllables data (categorized data Scripts.Javanese [| Ka; Ka |])
            let marked = { run with Infos = run.Infos.SetItem(0, { run.Infos[0] with Mask = run.Infos[0].Mask ||| 0xF00u ||| 0x1000u }) }
            let again = Use.setupSyllables data marked
            Expect.equal (masksOf again |> Array.map (fun mask -> mask &&& 0x1F00u)) [| 0x1200u; 0x800u |] "the old forms are gone, the other bits stay")
        testCase "a plan whose topographical masks are all zero, or a script that joins, sets no joining form" (fun _ ->
            let none = { data with TopographicalMasks = ImmutableArray.CreateRange [ 0u; 0u; 0u; 0u ] }
            let run = Use.setupSyllables none (categorized none Scripts.Javanese [| Ka; Ka |])
            Expect.equal (masksOf run |> Array.map (fun mask -> mask &&& 0xF00u)) [| 0u; 0u |] "no forms"
            let joiningRun = categorized joiningData Scripts.Mongolian [| MongolianA; MongolianA |]
            let joined = Use.setupSyllables joiningData joiningRun
            Expect.equal (masksOf joined |> Array.map (fun mask -> mask &&& ~~~(7u ||| RphfMask))) (masksOf joiningRun |> Array.map (fun mask -> mask &&& ~~~(7u ||| RphfMask))) "the Arabic plan has set the joining masks, the topographical ones are not set")
        testCase "a long syllable is processed in chunks of at most 64 glyphs" (fun _ ->
            let run = syllabled (Array.append [| Ka |] (Array.create 70 Pepet))
            Expect.equal (masksOf run |> Array.map (fun mask -> mask &&& 0xF00u)) (Array.append (Array.create 64 0x200u) (Array.create 7 0x800u)) "the chunk of 64 glyphs is the initial form, the rest the final form")
        testCase "clear_substitution_flags clears the substituted bit of every glyph and no other" (fun _ ->
            let run = substitutedAt 1 (categorized data Scripts.Javanese [| Ka; Ka |])
            let run = { run with Props = run.Props.SetItem(0, { run.Props[0] with GlyphClass = GlyphPropsBits.Ligated ||| GlyphPropsBits.Base }) }
            let cleared = Use.clearSubstitutionFlags run
            Expect.equal [| for props in cleared.Props -> props.GlyphClass |] [| GlyphPropsBits.Ligated ||| GlyphPropsBits.Base; 0us |] "only the substituted bit goes"
            Expect.equal [| for props in (Use.clearSubstitutionFlags cleared).Props -> props.GlyphClass |] [| GlyphPropsBits.Ligated ||| GlyphPropsBits.Base; 0us |] "a run with none is left as it is")
        testCase "record_rphf makes the first substituted glyph among the leading masked glyphs the repha" (fun _ ->
            let run = substitutedAt 1 (syllabled [| Ka; Pangkon; Ka; Pepet |])
            Expect.equal (categoriesOf (Use.recordRphf data run)) [| 1uy; 18uy; 1uy; 33uy |] "the pangkon is the repha")
        testCase "record_rphf looks only at the glyphs that have the rphf mask" (fun _ ->
            let run = substitutedAt 3 (syllabled [| Ka; Pangkon; Ka; Pepet |])
            Expect.equal (categoriesOf (Use.recordRphf data run)) [| 1uy; 12uy; 1uy; 33uy |] "the fourth glyph has no mask")
        testCase "record_rphf does nothing without an rphf mask" (fun _ ->
            let run = substitutedAt 1 (syllabled [| Ka; Pangkon; Ka |])
            Expect.isTrue (obj.ReferenceEquals(Use.recordRphf { data with RphfMask = 0u } run, run)) "returned as it is")
        testCase "record_pref makes the first substituted glyph of a syllable a pre-base vowel, in each syllable" (fun _ ->
            let run = substitutedAt 3 (substitutedAt 1 (syllabled [| Ka; Pangkon; Ka; Pepet |]))
            Expect.equal (categoriesOf (Use.recordPref run)) [| 1uy; 22uy; 1uy; 33uy |] "the first substituted glyph")
        testCase "a repha moves to the end of the syllable when no glyph after the base stands after it" (fun _ ->
            let run = reordered [| Repha; Ka |]
            Expect.equal (codepointsOf run) [| Ka; Repha |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "a repha moves before the first post-base glyph" (fun _ ->
            let run = reordered [| Repha; Ka; Pepet; Layar |]
            Expect.equal (codepointsOf run) [| Ka; Repha; Pepet; Layar |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u; 2u; 3u |] "the clusters passed over merge")
        testCase "a repha before a halant moves before it" (fun _ ->
            let run = reordered [| Repha; Ka; Pangkon; Ka |]
            Expect.equal (codepointsOf run) [| Ka; Repha; Pangkon; Ka |] "before the halant")
        testCase "a repha alone is a broken cluster: the dotted circle follows it and the repha moves past the circle" (fun _ ->
            Expect.equal (codepointsOf (reordered [| Repha |])) [| DottedCircleGlyph; Repha |] "the circle, then the repha")
        testCase "a pre-base vowel moves in front of its consonant and the clusters merge" (fun _ ->
            let run = reordered [| Ka; Taling |]
            Expect.equal (codepointsOf run) [| Taling; Ka |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "a pre-base vowel moves back only as far as the last halant" (fun _ ->
            let run = reordered [| Ka; Pangkon; Ka; Taling |]
            Expect.equal (codepointsOf run) [| Ka; Pangkon; Taling; Ka |] "after the halant"
            Expect.equal (clustersOf run) [| 0u; 1u; 2u; 2u |] "only the glyphs it passed over merge")
        testCase "a halant that was ligated does not stop a pre-base vowel" (fun _ ->
            let run = syllabled [| Ka; Pangkon; Ka; Taling |]
            let ligated = { run with Props = run.Props.SetItem(1, { run.Props[1] with GlyphClass = run.Props[1].GlyphClass ||| GlyphPropsBits.Ligated }) }
            Expect.equal (codepointsOf (Use.reorder circleFont ligated)) [| Taling; Ka; Pangkon; Ka |] "to the start")
        testCase "only the first component of a multiple substitution moves" (fun _ ->
            let run = syllabled [| Ka; Taling |]
            let multiplied = { run with Props = run.Props.SetItem(1, GlyphPropsBits.setLigPropsForComponent 1uy run.Props[1]) }
            Expect.equal (codepointsOf (Use.reorder circleFont multiplied)) [| Ka; Taling |] "not moved")
        testCase "a recorded pre-base consonant moves like a pre-base vowel" (fun _ ->
            let run = Use.reorder circleFont (Use.recordPref (substitutedAt 1 (syllabled [| Ka; Pepet |])))
            Expect.equal (codepointsOf run) [| Pepet; Ka |] "the substituted glyph is in front"
            Expect.equal (categoriesOf run) [| 22uy; 1uy |] "recorded as a pre-base vowel")
        testCase "a symbol cluster is reordered like a standard cluster: its pre-base vowel moves in front of the symbol" (fun _ ->
            Expect.equal (syllablesOf (syllabled [| 0x41u; Taling |])) [| 0x15uy; 0x15uy |] "one symbol cluster"
            Expect.equal (codepointsOf (reordered [| 0x41u; Taling |])) [| Taling; 0x41u |] "the vowel sign goes in front")
        testCase "a hieroglyph cluster is not reordered" (fun _ ->
            Expect.equal (codepointsOf (reordered [| Hieroglyph |])) [| Hieroglyph |] "unchanged")
        testCase "a broken cluster gets a dotted circle, which a pre-base vowel moves in front of, and after a repha" (fun _ ->
            Expect.equal (codepointsOf (reordered [| Taling |])) [| Taling; DottedCircleGlyph |] "the vowel sign goes back before the circle"
            Expect.equal (codepointsOf (reordered [| Repha; Pepet |])) [| DottedCircleGlyph; Repha; Pepet |] "the circle follows the repha, which then moves before the first post-base glyph")
        testCase "no dotted circle is inserted when the font has none" (fun _ ->
            Expect.equal (codepointsOf (Use.reorder (fontMapping [ Ka, 1u ]) (syllabled [| Taling |]))) [| Taling |] "unchanged")
        testCase "compose never recomposes a mark and composes the rest as Unicode does" (fun _ ->
            Expect.equal (Use.compose 0x0B47u 0x0B3Eu) ValueNone "the Oriya vowel sign e is a mark: it is not recomposed with aa"
            Expect.equal (Unicode.compose 0x0B47u 0x0B3Eu) (ValueSome 0x0B4Bu) "Unicode composes them"
            Expect.equal (Use.compose 0x41u 0x0301u) (ValueSome 0xC1u) "A with acute")
        testCase "a dual-joining letter between two others takes the medial form, the first the initial and the last the final" (fun _ ->
            let run = categorized joiningData Scripts.Mongolian [| MongolianA; MongolianA; MongolianA |]
            Expect.equal (actionsOf run) [| 6uy; 4uy; 1uy |] "init, medi, fina"
            Expect.equal (masksOf run |> Array.map (fun mask -> mask &&& ~~~7u)) [| GlobalMask ||| 0x4000u; GlobalMask ||| 0x1000u; GlobalMask ||| 0x200u |] "masks of the actions")
        testCase "a letter alone takes the isolated form" (fun _ ->
            let run = categorized joiningData Scripts.Mongolian [| MongolianA |]
            Expect.equal (actionsOf run) [| 0uy |] "isol"
            Expect.equal (masksOf run) [| GlobalMask ||| 0x100u |] "mask")
        testCase "the pre-context starts the state machine: a letter after a joining letter takes the final form" (fun _ ->
            let joined = Use.setupMasks Scripts.Mongolian joiningData { runWith ClusterLevel.MonotoneGraphemes GlobalMask [| MongolianA |] with ContextBefore = ImmutableArray.Create MongolianA }
            Expect.equal (actionsOf joined) [| 1uy |] "fina")
        testCase "the post-context gives the last letter the initial form" (fun _ ->
            let joined = Use.setupMasks Scripts.Mongolian joiningData { runWith ClusterLevel.MonotoneGraphemes GlobalMask [| MongolianA |] with ContextAfter = ImmutableArray.Create MongolianA }
            Expect.equal (actionsOf joined) [| 6uy |] "init")
        testCase "a Mongolian variation selector takes the action of the letter before it" (fun _ ->
            let run = categorized joiningData Scripts.Mongolian [| MongolianA; FreeVariationSelector; MongolianA |]
            Expect.equal (actionsOf run) [| 6uy; 6uy; 1uy |] "init, init, fina"
            let other = categorized joiningData Scripts.Javanese [| MongolianA; FreeVariationSelector; MongolianA |]
            Expect.equal (actionsOf other) [| 6uy; 7uy; 1uy |] "another script leaves the selector with no action")
        testCase "an Arabic plan sets the joining forms before the categories" (fun _ ->
            let run = categorized joiningData Scripts.Mongolian [| MongolianA; MongolianA |]
            Expect.equal (categoriesOf run) [| UseTable.category MongolianA; UseTable.category MongolianA |] "the categories are those of the table")
        testCase "a chained class rule set whose digest refuses the first glyph flags the glyphs it looked at before it reads a skippable second glyph" (fun _ ->
            let font = Face.create (read "test/shape/data/in-house/fonts/f70f345188472b93f565d1d7fae8c668dd6a3244.ttf") 0u |> Font.create
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x204Du; 0xFE0Bu; 0xA9BFu |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withSegment { Direction = Direction.Ltr; Script = Scripts.Javanese; Language = NoLanguage }
                |> GlyphRun.withFlags BufferFlags.ProduceUnsafeToConcat
                |> GlyphRun.withClusterLevel ClusterLevel.MonotoneGraphemes
                |> TailCalls.settle
            match Shape.shape font ImmutableArray<Feature>.Empty run with
            | Ok shaped -> Expect.equal [| for info in shaped.Infos -> info.Mask &&& uint32 GlyphFlags.UnsafeToConcat |] [| 2u; 2u; 2u |] "hb-shape --script=Java --unsafe-to-concat gives [cakra=0+238#2|.notdef=0+600#2|space=0+0#2]"
            | Error error -> failtest $"expected a shaped run, got {error}")
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 } "the filter keeps the glyphs of its definition, with their original indices"
            (Prop.forAll (Arb.fromGen filterCase) (fun (categories, marks) ->
                let kept, positions = filtered categories marks
                positions = keptByDefinition categories marks && kept = (positions |> Array.map (fun index -> categories[index]))))
        testCase "a filter over 20,000 glyphs gives the kept indices of the definition in ascending order, from one cursor over the run" (fun _ ->
            let count = 20_000
            let categories = Array.init count (fun index -> match index % 5 with | 0 -> UseMachine.CGJ | 1 -> UseMachine.ZWNJ | _ -> byte (index % 3))
            let marks = Array.init count (fun index -> index % 7 = 0)
            let kept, positions = filtered categories marks
            Expect.equal positions (keptByDefinition categories marks) "the kept indices of the definition"
            Expect.isLessThanOrEqual positions.Length count "no glyph is kept twice"
            Expect.isTrue (Seq.forall2 (<) positions (Seq.skip 1 positions)) "the indices ascend"
            Expect.equal kept.Length positions.Length "one category per kept glyph")
        Conformance.filesExcept "conformance" conformanceFiles [ "in-house", "use-syllable.tests", 19; "in-house", "use-syllable.tests", 20; "in-house", "use-syllable.tests", 21 ]
    ]
