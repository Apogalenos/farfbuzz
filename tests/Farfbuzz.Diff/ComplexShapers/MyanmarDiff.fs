module Farfbuzz.Diff.ComplexShapers.MyanmarDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Diff
open Farfbuzz.Diff.HarfBuzzSharpShaper
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.ComplexShapers
open Farfbuzz.Diff.ComplexShapers.HebrewThaiDiff
open Farfbuzz.Diff.ComplexShapers.HangulKhmerDiff

/// <summary>The cases of each trace differential.</summary>
[<Literal>]
let private CasesPerTrace = 3_000

/// <summary>The cases of the Zawgyi shape differential on each face.</summary>
[<Literal>]
let private CasesPerZawgyi = 3_000

/// <summary>The options of the Myanmar differentials: each cluster level, each direction, a language, user features that
/// switch the basic features off, the buffer flags of the beginning and end of the text and of the default ignorables, and
/// no script, which HarfBuzz guesses.</summary>
let private myanmarOptions : string array =
    [| "--script=Mymr --direction=ltr --cluster-level=0"; "--script=Mymr --direction=ltr --cluster-level=1"
       "--script=Mymr --direction=ltr --cluster-level=2"; "--script=Mymr --direction=ltr --cluster-level=3"
       "--script=Mymr --direction=rtl --cluster-level=1"; "--script=Mymr --language=my"; "--script=Mymr --features=-rphf"
       "--script=Mymr --features=pref=0,blwf=0"; "--script=Mymr --features=pstf[1:4]=0"; "--script=Mymr --direction=ttb"
       "--script=Mymr --bot --eot"; "--script=Mymr --remove-default-ignorables --cluster-level=2"; "--script=Mymr --preserve-default-ignorables --cluster-level=3"; String.Empty |]

/// <summary>The options of the Zawgyi differential: the Zawgyi script tag in each direction and cluster level.</summary>
let private zawgyiOptions : string array =
    [| "--script=Qaag --direction=ltr"; "--script=Qaag --direction=rtl"; "--script=Qaag --direction=ttb"
       "--script=Qaag --cluster-level=1"; "--script=Qaag --cluster-level=2"; "--script=Qaag --cluster-level=3"; "--script=Qaag --bot --eot" |]

/// <summary>The generator of Myanmar texts for a face: consonant clusters with medials, vowel signs, asat, virama and
/// dot-below marks, broken clusters that need a dotted circle, variation selectors, joiners and Latin.</summary>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The generator of texts.</returns>
let private myanmarText (covered: Set<uint32>) : Gen<uint32 array> =
    let pick = pick covered
    let consonant = pick (spanning [ 0x1000u, 0x1020u ])
    let independent = pick (spanning [ 0x1021u, 0x102Au ])
    let vowel = pick (spanning [ 0x102Bu, 0x1035u ])
    let medial = pick (spanning [ 0x103Bu, 0x103Eu ])
    let asat = pick [ 0x103Au ]
    let virama = pick [ 0x1039u ]
    let dotBelow = pick [ 0x1037u ]
    let sign = pick [ 0x1036u; 0x1038u ]
    let ra = pick [ 0x101Bu ]
    let nga = pick [ 0x1004u ]
    let extended = pick (spanning [ 0xAA60u, 0xAA7Fu ])
    let joiner = Gen.elements [ 0x200Cu; 0x200Du ]
    let vs = pick [ 0xFE00u ]
    let maybe (probability: int) (item: Gen<uint32 list>) = Gen.frequency [ probability, item; 10 - probability, Gen.constant [] ]
    let cluster =
        gen {
            let! baseGlyph = Gen.frequency [ 8, consonant; 2, independent ]
            let! medials =
                Gen.frequency
                    [ 5, Gen.constant []
                      4, gen { let! m = medial in return [ m ] }
                      1, gen { let! a = medial in let! b = medial in return [ a; b ] } ]
            let! joined = maybe 2 (gen { let! j = joiner in return [ j ] })
            let! vowels = Gen.frequency [ 4, Gen.constant []; 4, gen { let! v = vowel in return [ v ] }; 2, gen { let! a = vowel in let! b = vowel in return [ a; b ] } ]
            let! signs = maybe 3 (gen { let! s = Gen.frequency [ 3, dotBelow; 1, sign ] in return [ s ] })
            let! tail = maybe 3 (gen { let! t = asat in return [ t ] })
            return [ baseGlyph ] @ medials @ joined @ vowels @ signs @ tail
        }
    let reph = gen { let! r = ra in let! a = asat in let! h = virama in return [ r; a; h ] }
    let token =
        Gen.frequency
            [ 24, cluster
              4, gen { let! r = reph in let! c = consonant in return r @ [ c ] }
              3, gen { let! v = vowel in return [ v ] }
              3, gen { let! m = medial in return [ m ] }
              2, gen { let! c = virama in return [ c ] }
              2, gen { let! c = asat in return [ c ] }
              2, gen { let! k = consonant in let! c = virama in return [ k; c ] }
              2, gen { let! j = joiner in return [ j ] }
              3, gen { let! k = Gen.frequency [ 3, consonant; 2, nga; 1, independent ] in let! s = vs in return [ k; s ] }
              3, gen { let! c = pick (0x25CCu :: latin) in return [ c ] }
              2, gen { let! d = extended in return [ d ] }
              2, Gen.constant [ 0x25CCu ] ]
    textOf 5 token


/// <summary>The code points every synthetic Myanmar font starts from.</summary>
let private myanmarCodepoints : uint32 list =
    spanning [ 0x1000u, 0x109Fu; 0xAA60u, 0xAA7Fu; 0xFE00u, 0xFE00u; 0x200Cu, 0x200Du; 0x25CCu, 0x25CCu ] @ latin

/// <summary>The GSUB table of the synthetic Myanmar fonts: one lookup per feature of the shaper, each adding a delta of its
/// own to the glyphs of a group, the groups made to overlap so that the order of the lookups and the masks show.</summary>
/// <param name="script">The script tag of the table.</param>
/// <param name="mapping">The code points and glyphs of the font.</param>
/// <returns>The table.</returns>
let private myanmarGsub (script: string) (mapping: (uint32 * uint32) list) : byte array =
    let group (ranges: (uint32 * uint32) list) (remainder: uint32) =
        mapping |> List.filter (fun (codepoint, _) -> ranges |> List.exists (fun (first, last) -> codepoint >= first && codepoint <= last) && codepoint % 3u = remainder) |> List.map snd
    let consonants = [ 0x1000u, 0x1020u ]
    gsubTable script
        [ "locl", [ 0 ]; "ccmp", [ 1; 13 ]; "rphf", [ 2 ]; "pref", [ 3 ]; "blwf", [ 4 ]; "pstf", [ 5 ]
          "pres", [ 6 ]; "abvs", [ 7 ]; "blws", [ 8 ]; "psts", [ 9 ] ]
        [ singleSubstitution (glyphsIn mapping [ 0x41u, 0x5Au; 0x30u, 0x39u ]) 30000
          singleSubstitution (glyphsIn mapping [ 0x1021u, 0x102Au ]) 31000
          singleSubstitution (group consonants 2u) 1000
          singleSubstitution (group consonants 0u) 2000
          singleSubstitution (group consonants 1u) 3000
          singleSubstitution (group consonants 0u @ group consonants 1u) 4000
          singleSubstitution (group consonants 1u) 5000
          singleSubstitution (group consonants 2u) 6000
          singleSubstitution (group consonants 0u) 7000
          singleSubstitution (group consonants 1u @ group consonants 2u) 8000
          singleSubstitution (group consonants 1u @ group consonants 2u) 9000
          singleSubstitution (glyphsIn mapping [ 0xAA60u, 0xAA7Fu ]) 10000
          singleSubstitution (glyphsIn mapping [ 0x1000u, 0x1020u ]) 11000
          ligatureSubstitution (glyphsIn mapping consonants) (glyphsIn mapping consonants) (List.head (glyphsIn mapping [ 0x41u, 0x41u ])) ]

/// <summary>Builds a synthetic font: the base face with a cmap of distinct glyphs and optional other tables, over the
/// Myanmar code points.</summary>
/// <param name="name">The file name.</param>
/// <param name="mapping">The code points and glyphs.</param>
/// <param name="script">The script tag of the GSUB table, none for a font without layout tables.</param>
/// <returns>The face.</returns>
let private syntheticMyanmar (name: string) (mapping: (uint32 * uint32) list) (script: string voption) : TestFont =
    match script with
    | ValueSome tag -> syntheticFont name mapping [ tagOf "GSUB", myanmarGsub tag mapping ]
    | ValueNone -> syntheticFont name mapping []

/// <summary>The synthetic Myanmar fonts: one with every feature under the new specification's script tag, one with no layout
/// table, one without the dotted circle, one with half of the code points, and one under the old script tag, which sends the
/// text to the default shaper.</summary>
/// <returns>The fonts, each with its name.</returns>
let private myanmarFonts () : (string * TestFont) list =
    let full = distinctMapping myanmarCodepoints
    let without (drop: uint32 -> bool) = full |> List.filter (fun (codepoint, _) -> not (drop codepoint))
    let noCircle = without (fun u -> u = 0x25CCu)
    let half = without (fun u -> u >= 0x1000u && u <= 0x109Fu && (u * 11u) % 7u < 3u)
    [ "myanmar-features", syntheticMyanmar "myanmar-gsub.otf" full (ValueSome "mym2")
      "myanmar-no-layout", syntheticMyanmar "myanmar-full.otf" full ValueNone
      "myanmar-no-dotted-circle", syntheticMyanmar "myanmar-no-circle.otf" noCircle (ValueSome "mym2")
      "myanmar-half", syntheticMyanmar "myanmar-half.otf" half (ValueSome "mym2")
      "myanmar-old-tag", syntheticMyanmar "myanmar-old-tag.otf" full (ValueSome "mymr") ]

/// <summary>The Myanmar faces of the corpus.</summary>
let private myanmarFaces : Lazy<TestFont list> = lazy (corpusFaces [ 0x1000u, 0x109Fu; 0xAA60u, 0xAA7Fu ])

/// <summary>The synthetic Myanmar fonts, written once.</summary>
let private myanmarSynthetic : Lazy<(string * TestFont) list> = lazy (myanmarFonts ())

/// <summary>The Zawgyi faces: the corpus face the upstream test uses, and the synthetic fonts.</summary>
let private zawgyiFont : Lazy<TestFont> = lazy (TestFonts.named "ab14b4eb9d7a67e293f51d30d719add06c9d6e06.ttf")

/// <summary>The Myanmar shaping differential: Farfbuzz against HarfBuzz on random Myanmar text, on every corpus face that
/// maps some Myanmar, each with its layout, without layout tables and without GPOS, and on the synthetic fonts.</summary>
/// <returns>The tests.</returns>
let myanmarTests () : Test =
    testList "P6.S21 myanmar shape" (
        sampledTexts "myanmar" myanmarFaces.Value variants myanmarOptions myanmarText CasesPerScript
        @ sampledSynthetic "myanmar synthetic" myanmarSynthetic.Value myanmarOptions myanmarText)

/// <summary>The Zawgyi shaping differential: Farfbuzz against HarfBuzz on random Myanmar text shaped with the Zawgyi script,
/// on the Zawgyi corpus face and the Myanmar faces, each with its layout and stripped, and on the synthetic fonts.</summary>
/// <returns>The tests.</returns>
let zawgyiTests () : Test =
    let faces = zawgyiFont.Value :: myanmarFaces.Value |> List.distinct
    testList "P6.S21 zawgyi shape" (
        sampledTexts "zawgyi" faces variants zawgyiOptions myanmarText CasesPerZawgyi
        @ sampledSynthetic "zawgyi synthetic" myanmarSynthetic.Value zawgyiOptions myanmarText)

/// <summary>The plan differential: the feature tags and the GSUB and GPOS lookup sets Farfbuzz and HarfBuzz compile for the
/// Myanmar faces and the synthetic fonts, in each direction, and the Zawgyi plan.</summary>
/// <returns>The tests.</returns>
let planTests () : Test =
    let faces = myanmarFaces.Value @ (myanmarSynthetic.Value |> List.map snd)
    testList "P6.S21 plan" [
        PlanDiff.featuresAgainst "myanmar plan" (planCases faces [ "--script=Mymr"; "--script=Mymr --language=my" ])
        PlanDiff.featuresAgainst "zawgyi plan" (planCases (zawgyiFont.Value :: faces |> List.distinct) [ "--script=Qaag" ])
    ]

/// <summary>The snapshots HarfBuzz takes at the Myanmar pause points: the start of the reordering, the end of the insertion of
/// dotted circles, the end of the reordering, and around the whole GSUB table: its start and its end.</summary>
let private myanmarMessages : string list =
    [ "start reordering myanmar"; "end inserting dotted-circles"; "end reordering myanmar"; "start table GSUB"; "end table GSUB" ]

/// <summary>Forgets the syllable byte of the snapshot at the end of the GSUB table. The syllables are released by the pause that
/// clears them, and HarfBuzz's class cache (<c>get_class_cached</c>, src/hb-ot-layout-gsubgpos.hh:1856-1865) then keeps its
/// cached class of a glyph in the same byte, so the byte holds an unrelated value that shaping never reads.</summary>
/// <param name="snapshots">The snapshots, in the order of <see cref="myanmarMessages"/>.</param>
/// <returns>The snapshots with the last one's syllable bytes zeroed.</returns>
let private withoutReleasedSyllables (snapshots: TracedGlyph array voption list) : TracedGlyph array voption list =
    let last = snapshots.Length - 1
    snapshots |> List.mapi (fun index snapshot ->
        match index = last with
        | true -> snapshot |> ValueOption.map (Array.map (fun (glyph, mask, cluster, _, category) -> glyph, mask, cluster, 0uy, category))
        | false -> snapshot)

/// <summary>Runs the stages of the plan's GSUB map up to the pause of the Myanmar reordering, as <c>hb_ot_map_t::apply</c>
/// does, but for that pause's own function.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="stage">The next stage.</param>
/// <param name="first">The first lookup of the window.</param>
/// <param name="run">The run.</param>
/// <returns>The run just before the reordering pause.</returns>
let rec private untilReorder (plan: OtShapePlan) (font: Font) stage first (run: GlyphRun) : GlyphRun =
    let current = plan.Map.GsubStages[stage]
    match current.Pause with
    | MyanmarReorder -> OtLayout.substituteWindow plan font first current.LastLookup run
    | NoPause | ClearSyllables | KhmerSetupSyllables | KhmerReorder | IndicSetupSyllables | IndicInitialReordering | IndicFinalReordering
    | ClearSubstitutionFlags | UseSetupSyllables | UseRecordRphf | UseRecordPref | UseReorder | MyanmarSetupSyllables | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction ->
        match PauseKind.runsFunction current.Pause with
        | false -> untilReorder plan font (stage + 1) first run
        | true ->
            let windowed = OtLayout.substituteWindow plan font first current.LastLookup run
            untilReorder plan font (stage + 1) current.LastLookup (ShaperHooks.applyPause current.Pause plan font windowed)

/// <summary>The snapshots Farfbuzz has at the Myanmar pause points, by running the pipeline up to the substitution and the
/// stages up to and through the reordering pause.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>One snapshot per message of <see cref="myanmarMessages"/>, none for a message HarfBuzz would not send.</returns>
let private farfbuzzTrace (fontFile: string) (request: ShapeRequest) : TracedGlyph array voption list =
    let struct (font, plan, synthesized) = beforeSubstitution fontFile request
    let tableSnapshots =
        match plan.ApplyMorx with
        | true -> [ ValueNone; ValueNone ]
        | false -> [ ValueSome (tracedRun synthesized); ValueSome (tracedRun (OtLayout.substitute plan font synthesized)) ]
    let myanmarSnapshots =
        match plan.Shaper.Kind = MyanmarShaper with
        | false -> [ ValueNone; ValueNone; ValueNone ]
        | true ->
            let before = untilReorder plan font 0 0 synthesized
            let struct (circled, inserted) = SyllableEdit.insertDottedCircles font 1uy MyanmarMachine.DOTTEDCIRCLE ValueNone ValueNone before
            let after = ShaperHooks.applyPause MyanmarReorder plan font before
            [ ValueSome (tracedRun before)
              (match inserted with | true -> ValueSome (tracedRun circled) | false -> ValueNone)
              ValueSome (tracedRun after) ]
    myanmarSnapshots @ tableSnapshots

/// <summary>A trace differential on random Myanmar text: the glyphs, clusters, masks, syllables and categories at each pause
/// point of the Myanmar shaper agree with HarfBuzz's, so a difference is found at its stage.</summary>
/// <param name="name">The test name.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="covered">The code points the face maps.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let private myanmarTrace (name: string) (fontFile: string) (covered: Set<uint32>) (count: int) : Test =
    Differential.sampled name (ShapeDiff.casesOfTexts fontFile myanmarOptions (myanmarText covered)) count
        (fun request -> farfbuzzTrace fontFile request |> withoutReleasedSyllables)
        (fun request -> TraceDiff.traceMessages fontFile request myanmarMessages |> List.map tracedOracle |> withoutReleasedSyllables)

/// <summary>The Myanmar trace differentials: at the start of the reordering (after the syllables are found), after the dotted
/// circles are inserted, at the end of the reordering and around the GSUB table, on the corpus Myanmar faces in each variant
/// and on the synthetic Myanmar fonts.</summary>
/// <returns>The tests.</returns>
let myanmarTraceTests () : Test =
    let corpus = myanmarFaces.Value |> List.collect (fun font -> variants font |> List.map (fun (kind, variant) -> $"myanmar trace {label font} {kind}", variant))
    let synthetic = myanmarSynthetic.Value |> List.map (fun (name, font) -> $"myanmar trace {name}", font)
    Console.WriteLine $"myanmar trace: {myanmarFaces.Value.Length} corpus faces ({names myanmarFaces.Value}) in {corpus.Length} variants, {synthetic.Length} synthetic fonts, {CasesPerTrace} cases each, {(corpus.Length + synthetic.Length) * CasesPerTrace} cases"
    testList "P6.S21 myanmar trace" [ for name, font in corpus @ synthetic -> myanmarTrace name font.Path (coverageOf font) CasesPerTrace ]

/// <summary>The fixed case the Myanmar differential found in the default shaper: a font under the old script tag <c>mymr</c> sends the
/// text to the default shaper, which gives a mark after a zero-width joiner a fallback offset as HarfBuzz does.</summary>
/// <returns>The tests.</returns>
let foundCases () : Test =
    let ours = FarfbuzzShaper.shaper
    let oracle = create ()
    let font = myanmarSynthetic.Value |> List.find (fun (name, _) -> name = "myanmar-old-tag") |> snd
    testList "P6.S21 found cases" [
        for options in [ "--script=Mymr"; String.Empty; "--script=Mymr --direction=rtl"; "--script=Mymr --cluster-level=2"; "--script=Mymr --direction=ttb" ] ->
            testCase $"a mark after a joiner under the old script tag [{options}]" (fun () ->
                let request = ShapeDiff.withFlags (ShapeDiff.request font.Path options [| 0x103Bu; 0x200Du; 0x103Au |])
                Expect.equal (ShapeDiff.serialize (ours.Shape request)) (ShapeDiff.serialize (oracle.Shape request)) "Farfbuzz shapes the text as HarfBuzz does")
    ]
