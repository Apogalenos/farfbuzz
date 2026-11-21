module Farfbuzz.Diff.ComplexShapers.IndicDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Diff
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.ComplexShapers
open Farfbuzz.Diff.ComplexShapers.HebrewThaiDiff
open Farfbuzz.Diff.ComplexShapers.HangulKhmerDiff

/// <summary>The cases generated per synthetic font.</summary>
[<Literal>]
let private CasesPerSyntheticFont = 1_500

/// <summary>The cases the trace differential generates per face.</summary>
[<Literal>]
let private CasesPerTrace = 1_000

/// <summary>An Indic script the differential covers: its name, the option that selects it, its Unicode ranges, a language
/// of the script and the script tags of the new and the old specification.</summary>
type private IndicScript =
    { Name: string
      Option: string
      Ranges: (uint32 * uint32) list
      Language: string
      NewTag: string
      OldTag: string }

/// <summary>The scripts the Indic shaper takes.</summary>
let private indicScripts : IndicScript list =
    [ { Name = "devanagari"; Option = "Deva"; Ranges = [ 0x0900u, 0x097Fu; 0xA8E0u, 0xA8FFu; 0x1CD0u, 0x1CFFu ]; Language = "hi"; NewTag = "dev2"; OldTag = "deva" }
      { Name = "bengali"; Option = "Beng"; Ranges = [ 0x0980u, 0x09FFu ]; Language = "bn"; NewTag = "bng2"; OldTag = "beng" }
      { Name = "gurmukhi"; Option = "Guru"; Ranges = [ 0x0A00u, 0x0A7Fu ]; Language = "pa"; NewTag = "gur2"; OldTag = "guru" }
      { Name = "gujarati"; Option = "Gujr"; Ranges = [ 0x0A80u, 0x0AFFu ]; Language = "gu"; NewTag = "gjr2"; OldTag = "gujr" }
      { Name = "oriya"; Option = "Orya"; Ranges = [ 0x0B00u, 0x0B7Fu ]; Language = "or"; NewTag = "ory2"; OldTag = "orya" }
      { Name = "tamil"; Option = "Taml"; Ranges = [ 0x0B80u, 0x0BFFu ]; Language = "ta"; NewTag = "tml2"; OldTag = "taml" }
      { Name = "telugu"; Option = "Telu"; Ranges = [ 0x0C00u, 0x0C7Fu ]; Language = "te"; NewTag = "tel2"; OldTag = "telu" }
      { Name = "kannada"; Option = "Knda"; Ranges = [ 0x0C80u, 0x0CFFu ]; Language = "kn"; NewTag = "knd2"; OldTag = "knda" }
      { Name = "malayalam"; Option = "Mlym"; Ranges = [ 0x0D00u, 0x0D7Fu ]; Language = "ml"; NewTag = "mlm2"; OldTag = "mlym" } ]

/// <summary>The code points outside the blocks that the Indic categories give a category: the no-break space and the
/// punctuation that act as placeholders, the joiners and the dotted circle.</summary>
let private sharedCodepoints : uint32 list = spanning [ 0x00A0u, 0x00BFu; 0x2000u, 0x206Fu; 0x25CCu, 0x25CCu ]

/// <summary>The options of the Indic differentials for a script: each cluster level, each direction, a language, user
/// features that switch the features of the shaper off and on, the buffer flags of the beginning and end of the text and of
/// the default ignorables, and no script at all, so the script is guessed from the text: one that starts with a Latin letter is
/// shaped by the default shaper.</summary>
/// <param name="script">The script.</param>
/// <returns>The option fields.</returns>
let private optionsFor (script: IndicScript) : string array =
    let prefix = $"--script={script.Option}"
    [| $"{prefix} --direction=ltr --cluster-level=0"; $"{prefix} --direction=ltr --cluster-level=1"
       $"{prefix} --direction=ltr --cluster-level=2"; $"{prefix} --direction=ltr --cluster-level=3"
       $"{prefix} --direction=rtl --cluster-level=1"; $"{prefix} --language={script.Language}"
       $"{prefix} --language={script.Language} --cluster-level=2"; $"{prefix} --features=-rphf"
       $"{prefix} --features=-half,-blwf,-pref"; $"{prefix} --features=-cjct,-akhn,-nukt"
       $"{prefix} --features=+liga,+init"; $"{prefix} --features=half[1:3]=0"; $"{prefix} --features=pstf=0,vatu=0,abvf=0"
       $"{prefix} --direction=ttb"; $"{prefix} --bot --eot"; $"{prefix} --remove-default-ignorables --cluster-level=2"
       $"{prefix} --preserve-default-ignorables --cluster-level=3"; String.Empty |]

/// <summary>The code points of a script by their Indic category.</summary>
/// <param name="script">The script.</param>
/// <returns>The code points of each category the script has.</returns>
let private classesOf (script: IndicScript) : Map<byte, uint32 list> =
    spanning script.Ranges @ sharedCodepoints @ [ 0x200Cu; 0x200Du ]
    |> List.map (fun codepoint -> IndicTable.category codepoint, codepoint)
    |> List.filter (fun (category, _) -> category <> IndicMachine.X)
    |> List.groupBy fst
    |> List.map (fun (category, pairs) -> category, pairs |> List.map snd |> List.distinct)
    |> Map.ofList

/// <summary>The generator of Indic texts of a script for a face: syllables made of the categories in valid and broken
/// orders, consonants, halant chains, nuktas, joiners, matras, modifiers, vedic marks, standalone clusters, placeholders,
/// dotted circles, lone characters of every category and Latin.</summary>
/// <param name="classes">The code points of each category.</param>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The generator of texts.</returns>
let private indicText (classes: Map<byte, uint32 list>) (covered: Set<uint32>) : Gen<uint32 array> =
    let pick = pick covered
    let one (category: byte) : Gen<uint32 list> =
        match Map.tryFind category classes with
        | Some (_ :: _ as candidates) -> gen { let! codepoint = pick candidates in return [ codepoint ] }
        | Some [] | None -> Gen.constant []
    let maybe (probability: int) (item: Gen<uint32 list>) = Gen.frequency [ probability, item; 10 - probability, Gen.constant [] ]
    let joined (parts: Gen<uint32 list> list) : Gen<uint32 list> =
        parts |> List.fold (fun (accumulated: Gen<uint32 list>) part -> gen { let! before = accumulated in let! next = part in return before @ next }) (Gen.constant [])
    let consonant = Gen.frequency [ 12, one IndicMachine.C; 4, one IndicMachine.Ra; 1, one IndicMachine.CS; 1, one IndicMachine.V ]
    let halant = one IndicMachine.H
    let joiner = Gen.elements [ [ 0x200Cu ]; [ 0x200Du ] ]
    let link =
        gen {
            let! before = maybe 2 joiner
            let! after = Gen.frequency [ 6, Gen.constant []; 2, Gen.constant [ 0x200Du ]; 1, Gen.constant [ 0x200Cu ] ]
            let! target = consonant
            let! nukta = maybe 2 (one IndicMachine.N)
            let! halantGlyph = halant
            return before @ halantGlyph @ after @ target @ nukta
        }
    let links =
        Gen.frequency
            [ 8, Gen.constant []
              7, link
              3, gen { let! a = link in let! b = link in return a @ b }
              2, gen { let! a = link in let! b = link in let! c = link in return a @ b @ c } ]
    let reph =
        Gen.frequency
            [ 12, Gen.constant []
              5, gen { let! ra = one IndicMachine.Ra in let! h = halant in return ra @ h }
              2, gen { let! ra = one IndicMachine.Ra in let! h = halant in return ra @ h @ [ 0x200Du ] }
              2, one IndicMachine.Repha
              1, gen { let! ra = one IndicMachine.Ra in let! h = halant in let! z = joiner in return ra @ h @ z } ]
    let matra =
        Gen.frequency
            [ 12, one IndicMachine.M
              2, gen { let! sm = one IndicMachine.SM in let! mp = one IndicMachine.MPst in return sm @ mp }
              2, one IndicMachine.MPst
              1, joined [ joiner; one IndicMachine.M ] ]
    let matras =
        Gen.frequency
            [ 4, Gen.constant []
              6, matra
              3, gen { let! a = matra in let! b = matra in return a @ b }
              1, gen { let! a = matra in let! b = matra in let! c = matra in return a @ b @ c } ]
    let ending =
        Gen.frequency
            [ 12, Gen.constant []
              3, halant
              3, joined [ halant; Gen.constant [ 0x200Cu ] ]
              1, joined [ halant; Gen.constant [ 0x200Du ]; one IndicMachine.N ]
              1, Gen.constant [ 0x200Cu ] ]
    let tail =
        Gen.frequency
            [ 5, Gen.constant []
              4, one IndicMachine.SM
              1, joined [ one IndicMachine.SM; one IndicMachine.SM ]
              2, one IndicMachine.A
              1, joined [ one IndicMachine.SM; one IndicMachine.A; one IndicMachine.A ]
              1, one IndicMachine.SMPst ]
    let cluster =
        joined
            [ reph; consonant; maybe 3 (one IndicMachine.N); links; maybe 2 (one IndicMachine.CM); matras
              maybe 2 (one IndicMachine.N); ending; tail ]
    let vowelSyllable = joined [ reph; one IndicMachine.V; maybe 2 (one IndicMachine.N); links; matras; ending; tail ]
    let standalone =
        joined
            [ Gen.frequency [ 4, one IndicMachine.PLACEHOLDER; 4, one IndicMachine.DOTTEDCIRCLE; 1, joined [ one IndicMachine.CS; one IndicMachine.PLACEHOLDER ] ]
              maybe 2 (one IndicMachine.N); links; matras; ending; tail ]
    let symbol = joined [ one IndicMachine.Symbol; maybe 3 (one IndicMachine.N); tail ]
    let anyCategory = Gen.elements (classes |> Map.toList |> List.map fst) |> Gen.bind one
    let broken = Gen.frequency [ 1, joined [ anyCategory; anyCategory ]; 1, joined [ anyCategory; anyCategory; anyCategory ]; 3, anyCategory ]
    let token =
        Gen.frequency
            [ 36, cluster
              8, vowelSyllable
              4, standalone
              3, symbol
              14, broken
              3, gen { let! j = joiner in return j }
              6, gen { let! c = pick latin in return [ c ] }
              2, Gen.constant [ 0x25CCu ] ]
    textOf 5 token

/// <summary>Whether the universal shaping engine takes a face for a script: its GSUB table has the script under a tag of the
/// third generation, <c>dev3</c> and the like, so the Indic shaper is not the one that shapes it.</summary>
/// <param name="script">The script.</param>
/// <param name="font">The face.</param>
/// <returns>True when the face is shaped by the universal shaping engine.</returns>
let private shapedByUse (script: IndicScript) (font: TestFont) : bool =
    let face = Font.create (Face.create (ReadOnlyMemory (System.IO.File.ReadAllBytes font.Path)) (uint32 font.Index))
    let props = { Direction = Direction.Ltr; Script = Script.ofString script.Option; Language = NoLanguage }
    Categorize.shaper props.Script props.Direction (MapBuilder.chosenGsubScript face.Ot props) = UseShaper

/// <summary>The faces of the corpus that map at least one code point of a script and that the Indic shaper takes.</summary>
/// <param name="script">The script.</param>
/// <returns>The faces.</returns>
let private facesOf (script: IndicScript) : TestFont list =
    corpusFaces script.Ranges |> List.filter (fun font -> not (shapedByUse script font))

/// <summary>The synthetic fonts of a script, written once: the font of the new specification, the font of the old
/// specification with the same lookups, the font with no layout table, the font without the dotted circle and the font
/// without the virama.</summary>
/// <param name="script">The script.</param>
/// <returns>The fonts, each with its name.</returns>
let private syntheticFonts (script: IndicScript) : (string * TestFont) list =
    let classes = classesOf script
    let codepointsOf (category: byte) = Map.tryFind category classes |> Option.defaultValue []
    let virama = codepointsOf IndicMachine.H
    let mapped = spanning script.Ranges @ [ 0x200Cu; 0x200Du; 0x25CCu; 0xA0u ] @ latin |> List.distinct |> List.sort
    let full = distinctMapping mapped
    let without (drop: uint32 -> bool) = full |> List.filter (fun (codepoint, _) -> not (drop codepoint))
    let glyphsOfCategory (mapping: (uint32 * uint32) list) (category: byte) = codepointsOf category |> List.choose (glyphOf mapping)
    let ligatureBase = 60_000u
    let gsub (tag: string) (mapping: (uint32 * uint32) list) =
        let ra = glyphsOfCategory mapping IndicMachine.Ra
        let halants = glyphsOfCategory mapping IndicMachine.H
        let consonants = glyphsOfCategory mapping IndicMachine.C
        let matras = glyphsOfCategory mapping IndicMachine.M
        let group (remainder: uint32) = consonants |> List.filter (fun glyph -> glyph % 3u = remainder)
        let letters = glyphsIn mapping [ 0x41u, 0x5Au; 0x30u, 0x39u ]
        let single (glyphs: uint32 list) (delta: int) = singleSubstitution glyphs delta
        let ligature (firsts: uint32 list) (seconds: uint32 list) (index: uint32) = ligatureSubstitution firsts seconds (ligatureBase + index)
        gsubTable tag
            [ "locl", [ 0 ]; "ccmp", [ 1 ]; "nukt", [ 2 ]; "akhn", [ 3 ]; "rphf", [ 4 ]; "rkrf", [ 5 ]; "pref", [ 6 ]; "blwf", [ 7; 8 ]
              "abvf", [ 9 ]; "half", [ 10 ]; "pstf", [ 11 ]; "vatu", [ 12; 13 ]; "cjct", [ 14 ]; "init", [ 15 ]; "pres", [ 16 ]
              "abvs", [ 17 ]; "blws", [ 18 ]; "psts", [ 19 ]; "haln", [ 20 ]; "liga", [ 21 ] ]
            [ single letters 30000
              single (glyphsOfCategory mapping IndicMachine.V) 31000
              single (glyphsOfCategory mapping IndicMachine.N) 1000
              ligature (group 0u) (group 1u) 1u
              ligature ra halants 2u
              single (glyphsOfCategory mapping IndicMachine.Ra) 3000
              ligature halants ra 4u
              ligature halants (group 2u) 5u
              ligature (group 2u) halants 6u
              single (glyphsOfCategory mapping IndicMachine.A) 4000
              single (group 0u @ group 1u) 5000
              ligature halants (group 0u) 7u
              ligature halants (group 1u @ group 2u) 8u
              ligature (group 1u @ group 2u) halants 9u
              ligature consonants consonants 10u
              single matras 6000
              single (matras @ group 0u) 7000
              single (glyphsOfCategory mapping IndicMachine.SM) 8000
              single (glyphsOfCategory mapping IndicMachine.CM @ group 2u) 9000
              single (glyphsOfCategory mapping IndicMachine.SM @ matras) 10000
              single (halants @ group 1u) 11000
              ligature consonants halants 11u ]
    let noCircle = without (fun codepoint -> codepoint = 0x25CCu)
    let noVirama = without (fun codepoint -> List.contains codepoint virama)
    let withGsub (tag: string) (mapping: (uint32 * uint32) list) = [ tagOf "GSUB", gsub tag mapping ]
    [ $"{script.Name}-new-spec", syntheticFont $"indic-{script.Name}-new.otf" full (withGsub script.NewTag full)
      $"{script.Name}-old-spec", syntheticFont $"indic-{script.Name}-old.otf" full (withGsub script.OldTag full)
      $"{script.Name}-no-layout", syntheticFont $"indic-{script.Name}-plain.otf" full []
      $"{script.Name}-no-dotted-circle", syntheticFont $"indic-{script.Name}-no-circle.otf" noCircle (withGsub script.NewTag noCircle)
      $"{script.Name}-no-virama", syntheticFont $"indic-{script.Name}-no-virama.otf" noVirama (withGsub script.NewTag noVirama) ]

/// <summary>The synthetic fonts of every script, written once.</summary>
let private syntheticByScript : Lazy<(IndicScript * (string * TestFont) list) list> =
    lazy [ for script in indicScripts -> script, syntheticFonts script ]

/// <summary>The Indic shaping differential of one script: Farfbuzz against HarfBuzz on random text of the script, on every
/// corpus face that maps some of it, each with its layout, without layout tables and without GPOS, and on the synthetic fonts.</summary>
/// <param name="script">The script.</param>
/// <returns>The tests.</returns>
let private shapeTests (script: IndicScript) : Test =
    let classes = classesOf script
    let options = optionsFor script
    let synthetic = syntheticByScript.Value |> List.find (fun (candidate, _) -> candidate.Name = script.Name) |> snd
    let listed = synthetic |> List.map fst |> String.concat " "
    Console.WriteLine $"indic {script.Name} synthetic: {synthetic.Length} fonts ({listed}), {CasesPerSyntheticFont} cases each, {synthetic.Length * CasesPerSyntheticFont} cases"
    testList $"P6.S17 P6.S18 P6.S19 indic {script.Name} shape" (
        sampledTexts $"indic {script.Name}" (facesOf script) variants options (indicText classes) CasesPerScript
        @ [ for name, font in synthetic -> ShapeDiff.againstTexts $"indic {script.Name} synthetic {name}" font.Path options (indicText classes (coverageOf font)) CasesPerSyntheticFont ])

/// <summary>The cases a random run found once in a hundred: Devanagari text on a Kannada face without GPOS is shaped by the
/// default shaper, whose fallback mark positioning takes a joiner between two marks as the base of the second. A text that
/// starts with a mark, a joiner and a mark, with the other characters before, between and after, in both directions.</summary>
/// <returns>The tests.</returns>
let private fallbackJoinerTests () : Test =
    let kannada = TestFonts.named "NotoSerifKannada-Regular.ttf"
    let copy = withoutGpos kannada |> ValueOption.defaultValue kannada
    let texts =
        [ [| 0x0901u; 0x200Cu; 0x094Du |]; [| 0x0901u; 0x200Cu; 0x094Du; 0x1CF5u |]; [| 0x0901u; 0x200Du; 0x094Du |]; [| 0x0901u; 0x2060u; 0x0902u |]
          [| 0x0041u; 0x0901u; 0x200Cu; 0x094Du |]; [| 0x0901u; 0x200Cu; 0x094Du; 0x200Cu; 0x0902u |]; [| 0x094Du; 0x200Cu; 0x0901u; 0x200Cu; 0x094Du |] ]
    let options = [| "--script=Deva"; "--script=Deva --features=+liga,+init"; "--script=Deva --direction=rtl"; "--script=Deva --cluster-level=2" |]
    testList "P6.S17 default shaper fallback marks around a joiner" [
        ShapeDiff.againstTexts "devanagari marks around a joiner on Noto Serif Kannada without GPOS" copy.Path options (Gen.elements texts) 600
    ]

/// <summary>The Indic shaping differentials, one list per script.</summary>
/// <returns>The tests.</returns>
let shapeTestsOfAllScripts () : Test =
    testList "P6.S17 P6.S18 P6.S19 indic shape" ([ for script in indicScripts -> shapeTests script ] @ [ fallbackJoinerTests () ])

/// <summary>The plan differential: the feature tags and the GSUB and GPOS lookup sets Farfbuzz and HarfBuzz compile for the
/// corpus faces of each script and for the synthetic fonts, in each direction.</summary>
/// <returns>The tests.</returns>
let planTests () : Test =
    testList "P6.S17 plan" [
        for script in indicScripts do
            let synthetic = syntheticByScript.Value |> List.find (fun (candidate, _) -> candidate.Name = script.Name) |> snd |> List.map snd
            yield PlanDiff.featuresAgainst $"indic {script.Name} plan" (planCases (facesOf script @ synthetic) [ $"--script={script.Option}"; $"--script={script.Option} --language={script.Language}" ])
    ]

/// <summary>What the trace differential compares of one glyph at a pause point: the glyph, its mask with the glyph flags, its
/// cluster, its syllable byte, its category and its position.</summary>
type private IndicTraced = uint32 * uint32 * uint32 * byte * byte * byte

/// <summary>The snapshots HarfBuzz takes at the pause points of the Indic shaper: the start of the initial reordering (after
/// the syllables are found), the end of the insertion of dotted circles, the end of the initial reordering, the start and the
/// end of the final reordering, and the end of the GSUB table.</summary>
let private indicMessages : string list =
    [ "start reordering indic initial"; "end inserting dotted-circles"; "end reordering indic initial"
      "start reordering indic final"; "end reordering indic final"; "end table GSUB" ]

/// <summary>Reads the glyphs of a HarfBuzz snapshot: the syllable is the high byte of <c>var1</c>, the category the third byte
/// of <c>var2</c> and the position its fourth.</summary>
/// <param name="infos">The glyph infos.</param>
/// <returns>The glyphs.</returns>
let private tracedOfOracle (infos: GlyphInfo array voption) : IndicTraced array voption =
    infos |> ValueOption.map (Array.map (fun info -> info.Codepoint, info.Mask, info.Cluster, byte (info.Var1 >>> 24), byte (info.Var2 >>> 16), byte (info.Var2 >>> 24)))

/// <summary>Reads the glyphs of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyphs.</returns>
let private tracedOfRun (run: GlyphRun) : IndicTraced array =
    Array.init run.Infos.Length (fun index ->
        run.Infos[index].Codepoint, run.Infos[index].Mask, run.Infos[index].Cluster, run.Props[index].Syllable, run.Props[index].ShaperCategory, run.Props[index].ShaperAuxiliary)

/// <summary>The runs Farfbuzz has at the pause points of the Indic shaper.</summary>
type private IndicSnapshots =
    { StartInitial: GlyphRun voption
      Circled: GlyphRun voption
      EndInitial: GlyphRun voption
      StartFinal: GlyphRun voption
      EndFinal: GlyphRun voption }

/// <summary>Runs the stages of the plan's GSUB map as <c>hb_ot_map_t::apply</c> does, taking the run at each pause of the
/// Indic reordering.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="stage">The next stage.</param>
/// <param name="first">The first lookup of the window.</param>
/// <param name="run">The run.</param>
/// <param name="snapshots">The snapshots taken so far.</param>
/// <returns>The run after the last stage and the snapshots.</returns>
let rec private walkStages (plan: OtShapePlan) (font: Font) stage first (run: GlyphRun) (snapshots: IndicSnapshots) : struct (GlyphRun * IndicSnapshots) =
    match stage < plan.Map.GsubStages.Length with
    | false -> struct (OtLayout.substituteWindow plan font first plan.Map.GsubStages[plan.Map.GsubStages.Length - 1].LastLookup run, snapshots)
    | true ->
        let current = plan.Map.GsubStages[stage]
        match PauseKind.runsFunction current.Pause with
        | false -> walkStages plan font (stage + 1) first run snapshots
        | true ->
            let windowed = OtLayout.substituteWindow plan font first current.LastLookup run
            let recorded =
                match current.Pause, plan.Data with
                | IndicInitialReordering, IndicData data ->
                    let struct (circled, inserted) =
                        SyllableEdit.insertDottedCircles font 4uy IndicMachine.DOTTEDCIRCLE (ValueSome IndicMachine.Repha) (ValueSome IndicPosition.PosEnd) (IndicInitial.updateConsonantPositions data font windowed)
                    let applied = ShaperHooks.applyPause current.Pause plan font windowed
                    { snapshots with StartInitial = ValueSome windowed; Circled = (match inserted with | true -> ValueSome circled | false -> ValueNone); EndInitial = ValueSome applied }
                | IndicFinalReordering, IndicData _ ->
                    { snapshots with StartFinal = ValueSome windowed; EndFinal = ValueSome (ShaperHooks.applyPause current.Pause plan font windowed) }
                | (NoPause | ClearSyllables | KhmerSetupSyllables | KhmerReorder | IndicSetupSyllables | IndicInitialReordering | IndicFinalReordering | ClearSubstitutionFlags | UseSetupSyllables | UseRecordRphf | UseRecordPref | UseReorder
                  | MyanmarSetupSyllables | MyanmarReorder | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction), (NoShaperData | HangulData _ | KhmerData _ | IndicData _ | UseData _ | ArabicData _) -> snapshots
            walkStages plan font (stage + 1) current.LastLookup (ShaperHooks.applyPause current.Pause plan font windowed) recorded

/// <summary>The snapshots Farfbuzz has at the Indic pause points and at the end of the GSUB table, for a text that is not empty.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>One snapshot per message of <see cref="indicMessages"/>, none for a message HarfBuzz would not send.</returns>
let private tracedText (fontFile: string) (request: ShapeRequest) : IndicTraced array voption list =
    let struct (font, plan, synthesized) = beforeSubstitution fontFile request
    let none = { StartInitial = ValueNone; Circled = ValueNone; EndInitial = ValueNone; StartFinal = ValueNone; EndFinal = ValueNone }
    let struct (final, snapshots) =
        match plan.ApplyMorx with
        | true -> struct (synthesized, none)
        | false -> walkStages plan font 0 0 synthesized none
    let table = match plan.ApplyMorx with | true -> ValueNone | false -> ValueSome (tracedOfRun final)
    [ snapshots.StartInitial; snapshots.Circled; snapshots.EndInitial; snapshots.StartFinal; snapshots.EndFinal ]
    |> List.map (ValueOption.map tracedOfRun)
    |> fun runs -> runs @ [ table ]

/// <summary>The snapshots Farfbuzz has at the Indic pause points: none for an empty text, for which HarfBuzz shapes nothing and
/// sends no message.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>One snapshot per message of <see cref="indicMessages"/>.</returns>
let private farfbuzzTrace (fontFile: string) (request: ShapeRequest) : IndicTraced array voption list =
    match request.Text.Length with
    | 0 -> List.replicate indicMessages.Length ValueNone
    | _ -> tracedText fontFile request

/// <summary>A trace differential on random text of a script: the glyphs, clusters, masks, syllables, categories and positions
/// at each pause point of the Indic shaper agree with HarfBuzz's, so a difference is found at its stage.</summary>
/// <param name="name">The test name.</param>
/// <param name="script">The script.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="covered">The code points the face maps.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let private trace (name: string) (script: IndicScript) (fontFile: string) (covered: Set<uint32>) (count: int) : Test =
    Differential.sampled name (ShapeDiff.casesOfTexts fontFile (optionsFor script) (indicText (classesOf script) covered)) count
        (fun request -> farfbuzzTrace fontFile request)
        (fun request -> TraceDiff.traceMessages fontFile request indicMessages |> List.map tracedOfOracle)

/// <summary>The Indic trace differentials: after the syllables are found, after the dotted circles, after the initial and the
/// final reordering and at the end of the GSUB table, on the corpus faces of each script with their layout and on the
/// synthetic fonts.</summary>
/// <returns>The tests.</returns>
let traceTests () : Test =
    let forScript (script: IndicScript) =
        let corpus = facesOf script |> List.map (fun font -> $"indic {script.Name} trace {label font}", font)
        let synthetic = syntheticByScript.Value |> List.find (fun (candidate, _) -> candidate.Name = script.Name) |> snd |> List.map (fun (name, font) -> $"indic trace {name}", font)
        Console.WriteLine $"indic {script.Name} trace: {corpus.Length} corpus faces, {synthetic.Length} synthetic fonts, {CasesPerTrace} cases each, {(corpus.Length + synthetic.Length) * CasesPerTrace} cases"
        testList $"P6.S17 P6.S18 P6.S19 indic {script.Name} trace" [ for name, font in corpus @ synthetic -> trace name script font.Path (coverageOf font) CasesPerTrace ]
    testList "P6.S17 P6.S18 P6.S19 indic trace" [ for script in indicScripts -> forScript script ]
