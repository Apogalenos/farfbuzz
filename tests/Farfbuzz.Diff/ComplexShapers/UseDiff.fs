module Farfbuzz.Diff.ComplexShapers.UseDiff

open System
open System.Collections.Immutable
open System.Text
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

/// <summary>The cases the trace differential generates per face variant.</summary>
[<Literal>]
let private CasesPerTrace = 400

/// <summary>The cases each synthetic font gets in the shaping differential.</summary>
[<Literal>]
let private CasesPerSyntheticFont = 2_000

/// <summary>The code points of the Unicode blocks the categories of the engine give a category outside the scripts: the spaces and
/// punctuation, the joiners, the dotted circle, the combining marks and the variation selectors.</summary>
let private sharedCodepoints : uint32 list =
    spanning [ 0x00A0u, 0x00BFu; 0x0300u, 0x036Fu; 0x2000u, 0x206Fu; 0x25CCu, 0x25CCu; 0xFE00u, 0xFE0Fu; 0x200Cu, 0x200Du; 0x034Fu, 0x034Fu ]

/// <summary>The code points of every script, found once: the script of each code point of the first two planes.</summary>
let private codepointsByScript : Lazy<Map<Script, uint32 list>> =
    lazy (
        [ 0u .. 0x1FFFFu ]
        |> List.groupBy (fun codepoint -> Unicode.script codepoint)
        |> Map.ofList)

/// <summary>A script the universal shaping engine shapes on at least one corpus face: its code points, and the faces.</summary>
type private UseScript =
    { Script: Script
      Name: string
      Option: string
      Codepoints: uint32 list
      Faces: TestFont list }

/// <summary>The four-letter name of a script.</summary>
/// <param name="script">The script.</param>
/// <returns>The ISO 15924 name.</returns>
let private nameOf (script: Script) : string =
    Tag.toString (LanguagePrimitives.UInt32WithMeasure (uint32 script))

/// <summary>Whether the universal shaping engine takes a face for a script: the shaper is chosen from the script and the script
/// the face's GSUB table has.</summary>
/// <param name="script">The script.</param>
/// <param name="font">The face.</param>
/// <returns>True when the engine shapes the script on the face.</returns>
let private shapedByUse (script: Script) (font: TestFont) : bool =
    let face = Font.create (Face.create (ReadOnlyMemory (System.IO.File.ReadAllBytes font.Path)) (uint32 font.Index))
    let props = { Direction = Direction.Ltr; Script = script; Language = NoLanguage }
    Categorize.shaper script Direction.Ltr (MapBuilder.chosenGsubScript face.Ot props) = UseShaper

/// <summary>The scripts the engine shapes on some corpus face, each with the faces that map some code point of it and that the
/// engine takes.</summary>
let private useScripts : Lazy<UseScript list> =
    lazy (
        let faces = TestFonts.all () |> List.map (fun font -> font, covered font |> Seq.map (fun codepoint -> Unicode.script codepoint) |> Set.ofSeq)
        let candidates =
            codepointsByScript.Value
            |> Map.toList
            |> List.filter (fun (script, _) -> script <> Scripts.Common && script <> Scripts.Inherited && script <> Scripts.Unknown && script <> Scripts.Invalid)
        let scripts =
            [ for script, codepoints in candidates do
                let covering = faces |> List.filter (fun (_, scripts) -> Set.contains script scripts) |> List.map fst
                let taking = covering |> List.filter (fun font -> shapedByUse script font)
                match taking with
                | [] -> ()
                | _ -> yield { Script = script; Name = nameOf script; Option = $"--script={nameOf script}"; Codepoints = codepoints; Faces = taking } ]
        scripts |> List.sortBy (fun script -> script.Name))

/// <summary>The code points of a script and of the shared blocks by their universal shaping engine category, leaving out those
/// that have no category of their own.</summary>
/// <param name="script">The script.</param>
/// <returns>The code points of each category.</returns>
let private classesOf (script: UseScript) : Map<byte, uint32 list> =
    script.Codepoints @ sharedCodepoints
    |> List.map (fun codepoint -> UseTable.category codepoint, codepoint)
    |> List.groupBy fst
    |> List.map (fun (category, pairs) -> category, pairs |> List.map snd |> List.distinct)
    |> Map.ofList

/// <summary>Whether a script joins like Arabic, so that its glyphs take the masks of the joining forms.</summary>
/// <param name="script">The script.</param>
/// <returns>True for the scripts of the Arabic joining list.</returns>
let private joins (script: UseScript) : bool = ArabicJoiningList.hasArabicJoining script.Script

/// <summary>The generator of texts of a script for a face: syllables made of the categories in the orders of the engine's grammar,
/// valid and broken, number and hieroglyph clusters, symbols, joiners, the dotted circle and Latin.</summary>
/// <param name="classes">The code points of each category.</param>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The generator of texts.</returns>
let private useText (classes: Map<byte, uint32 list>) (covered: Set<uint32>) : Gen<uint32 array> =
    let pick = pick covered
    let one (category: byte) : Gen<uint32 list> =
        match Map.tryFind category classes with
        | Some (_ :: _ as candidates) -> gen { let! codepoint = pick candidates in return [ codepoint ] }
        | Some [] | None -> Gen.constant []
    let joined (parts: Gen<uint32 list> list) : Gen<uint32 list> =
        parts |> List.fold (fun (accumulated: Gen<uint32 list>) part -> gen { let! before = accumulated in let! next = part in return before @ next }) (Gen.constant [])
    let maybe (probability: int) (item: Gen<uint32 list>) = Gen.frequency [ probability, item; 10 - probability, Gen.constant [] ]
    let many (item: Gen<uint32 list>) = Gen.frequency [ 6, Gen.constant []; 3, item; 2, joined [ item; item ]; 1, joined [ item; item; item ] ]
    let halant = Gen.frequency [ 6, one UseMachine.H; 2, one UseMachine.HVM; 2, one UseMachine.IS; 1, one UseMachine.Sk ]
    let consonant = Gen.frequency [ 10, one UseMachine.B; 2, one UseMachine.GB ]
    let consonantModifiers =
        let stacked = Gen.frequency [ 3, joined [ halant; one UseMachine.B ]; 2, one UseMachine.SUB ]
        joined [ many (one UseMachine.CMAbv); many (one UseMachine.CMBlw); many (joined [ stacked; many (one UseMachine.CMAbv) ]) ]
    let medial = joined [ maybe 2 (one UseMachine.MPre); maybe 2 (one UseMachine.MAbv); maybe 2 (one UseMachine.MBlw); maybe 2 (one UseMachine.MPst) ]
    let dependentVowels =
        Gen.frequency
            [ 9, joined [ many (one UseMachine.VPre); many (one UseMachine.VAbv); many (one UseMachine.VBlw); many (one UseMachine.VPst) ]
              1, halant ]
    let vowelModifiers = joined [ maybe 2 (one UseMachine.HVM); many (one UseMachine.VMPre); many (one UseMachine.VMAbv); many (one UseMachine.VMBlw); many (one UseMachine.VMPst) ]
    let finalConsonants = joined [ many (one UseMachine.FAbv); many (one UseMachine.FBlw); many (one UseMachine.FPst) ]
    let finalModifiers = Gen.frequency [ 5, Gen.constant []; 3, joined [ many (one UseMachine.FMAbv); many (one UseMachine.FMBlw) ]; 2, one UseMachine.FMPst ]
    let start = joined [ maybe 3 (Gen.frequency [ 3, one UseMachine.R; 1, one UseMachine.CS ]); consonant ]
    let middle = joined [ consonantModifiers; medial; dependentVowels; vowelModifiers; maybe 2 (joined [ one UseMachine.Sk; one UseMachine.B ]) ]
    let tail = joined [ middle; finalConsonants; finalModifiers ]
    let terminator = Gen.frequency [ 4, one UseMachine.IS; 2, one UseMachine.RK; 3, one UseMachine.Sk ]
    let symbolTail = Gen.frequency [ 3, joined [ many (one UseMachine.SMAbv); many (one UseMachine.SMBlw) ]; 1, one UseMachine.SMBlw ]
    let numberTail = joined [ many (joined [ one UseMachine.HN; one UseMachine.N ]); one UseMachine.HN ]
    let cluster =
        Gen.frequency
            [ 8, joined [ start; tail ]
              3, joined [ start; consonantModifiers; terminator ]
              2, joined [ start; middle; one UseMachine.Sk ] ]
    let hieroglyph =
        let element = joined [ many (one UseMachine.SB); one UseMachine.G; maybe 2 (one UseMachine.HR); maybe 2 (one UseMachine.HM); many (one UseMachine.SE) ]
        joined [ element; many (joined [ one UseMachine.J; element ]) ]
    let numeral = joined [ one UseMachine.N; Gen.frequency [ 3, numberTail; 2, joined [ one UseMachine.HN; one UseMachine.N ]; 1, Gen.constant [] ] ]
    let symbol = joined [ Gen.frequency [ 3, one UseMachine.O; 2, one UseMachine.GB; 1, one UseMachine.SB ]; maybe 3 (Gen.frequency [ 3, symbolTail; 2, tail ]) ]
    let anyCategory = Gen.elements (classes |> Map.toList |> List.map fst) |> Gen.bind one
    let broken = Gen.frequency [ 1, joined [ anyCategory; anyCategory ]; 1, joined [ anyCategory; anyCategory; anyCategory ]; 3, anyCategory; 2, joined [ maybe 4 (one UseMachine.R); tail ] ]
    let ending = Gen.frequency [ 6, Gen.constant []; 2, one UseMachine.ZWNJ; 1, one UseMachine.CGJ; 1, joined [ one UseMachine.CGJ; one UseMachine.ZWNJ ] ]
    let token =
        Gen.frequency
            [ 30, joined [ cluster; ending ]
              6, joined [ hieroglyph; ending ]
              6, joined [ numeral; ending ]
              4, joined [ symbol; ending ]
              3, joined [ one UseMachine.N; numberTail; ending ]
              2, one UseMachine.FMPst
              14, joined [ broken; ending ]
              4, one UseMachine.WJ
              5, gen { let! c = pick latin in return [ c ] }
              2, Gen.constant [ 0x25CCu ] ]
    textOf 5 token

/// <summary>The option fields for the context of a text: the code points before and after, of the joining types.</summary>
/// <param name="script">The script.</param>
/// <returns>The option fields, empty for a script that does not join.</returns>
let private contextOptions (script: UseScript) : string array =
    match joins script with
    | false -> [||]
    | true ->
        let letters = script.Codepoints |> List.filter (fun codepoint -> ArabicTable.joiningType codepoint = 3uy) |> List.truncate 3
        let transparent = script.Codepoints |> List.filter (fun codepoint -> ArabicTable.joiningType codepoint = 6uy) |> List.truncate 1
        let spelled (codepoints: uint32 list) = codepoints |> List.map (fun codepoint -> $"U+{codepoint:X4}") |> String.concat ","
        [| for letter in letters do
            yield $"{script.Option} --unicodes-before={spelled [ letter ]}"
            yield $"{script.Option} --unicodes-after={spelled [ letter ]} --direction=rtl"
            yield $"{script.Option} --unicodes-before={spelled (transparent @ [ letter ])} --unicodes-after={spelled [ letter ]}" |]

/// <summary>The options of the differentials for a script: each cluster level, each direction, a language, user features that
/// switch the features of the shaper off and on, the topographical features, the buffer flags of the beginning and end of the
/// text and of the default ignorables, the contexts of a script that joins, and no script at all.</summary>
/// <param name="script">The script.</param>
/// <param name="withContext">Whether the options with a context are included.</param>
/// <returns>The option fields.</returns>
let private optionsFor (script: UseScript) (withContext: bool) : string array =
    let prefix = script.Option
    let plain =
        [| $"{prefix} --direction=ltr --cluster-level=0"; $"{prefix} --direction=ltr --cluster-level=1"
           $"{prefix} --direction=ltr --cluster-level=2"; $"{prefix} --direction=ltr --cluster-level=3"
           $"{prefix} --direction=rtl --cluster-level=0"; $"{prefix} --direction=rtl --cluster-level=1"
           $"{prefix} --direction=rtl --cluster-level=2"; $"{prefix} --direction=rtl --cluster-level=3"
           $"{prefix} --language=en"; $"{prefix} --features=-rphf"; $"{prefix} --features=-pref,-akhn"
           $"{prefix} --features=-locl,-ccmp,-nukt"; $"{prefix} --features=+isol,+init"; $"{prefix} --features=-fina,-medi,+init"
           $"{prefix} --features=rkrf=0,abvf=0,blwf=0,half[1:3]=0"; $"{prefix} --features=abvs=0,blws=0,haln=0,pres=0,psts=0"
           $"{prefix} --features=rphf=0"; $"{prefix} --features=pstf=0,vatu=0,cjct=0"; $"{prefix} --direction=ttb"
           $"{prefix} --bot --eot"; $"{prefix} --remove-default-ignorables --cluster-level=2"
           $"{prefix} --preserve-default-ignorables --cluster-level=3"; String.Empty |]
    match withContext with
    | true -> Array.append plain (contextOptions script)
    | false -> plain

/// <summary>Builds a multiple substitution lookup that replaces each glyph it covers by an inserted glyph and the glyph itself.</summary>
/// <param name="glyphs">The glyphs it covers.</param>
/// <param name="inserted">The glyph put before each.</param>
/// <returns>The lookup table.</returns>
let private multipleSubstitution (glyphs: uint32 list) (inserted: uint32) : byte array =
    let sorted = glyphs |> List.distinct |> List.sort
    let sequences = sorted |> List.map (fun glyph -> Array.concat [ be16 2; be16 (int inserted); be16 (int glyph) ])
    let header = 6 + 2 * sorted.Length
    let offsets = sequences |> List.scan (fun offset sequence -> offset + sequence.Length) header
    let subtable =
        Array.concat (
            [ be16 1; be16 (List.last offsets); be16 sorted.Length ]
            @ (List.take sorted.Length offsets |> List.map be16)
            @ sequences
            @ [ coverageTable sorted ])
    Array.concat [ be16 2; be16 0; be16 1; be16 8; subtable ]

/// <summary>The generic GSUB table of the synthetic fonts of a script: one or two lookups for each feature of the shaper, over the
/// glyphs of the categories each feature acts on, with a ligature of consonants and a multiple substitution that puts a pre-base
/// vowel before them, so that the substituted repha and pre-base consonant, the ligated halant and the components of a substitution
/// are all met.</summary>
/// <param name="tag">The GSUB script tag.</param>
/// <param name="mapping">The code points and glyphs of the font.</param>
/// <returns>The table.</returns>
let private useGsub (tag: string) (mapping: (uint32 * uint32) list) : byte array =
    let glyphsOf (categories: byte list) = mapping |> List.filter (fun (codepoint, _) -> List.contains (UseTable.category codepoint) categories) |> List.map snd
    let single (glyphs: uint32 list) (delta: int) = singleSubstitution glyphs delta
    let ligature (firsts: uint32 list) (seconds: uint32 list) (index: uint32) = ligatureSubstitution firsts seconds (60_000u + index)
    let bases = glyphsOf [ UseMachine.B ]
    let group (remainder: uint32) = bases |> List.filter (fun glyph -> glyph % 3u = remainder)
    let halants = glyphsOf [ UseMachine.H; UseMachine.IS; UseMachine.HVM; UseMachine.Sk ]
    let letters = glyphsIn mapping [ 0x41u, 0x5Au; 0x30u, 0x39u ]
    let preBase = glyphsOf [ UseMachine.VPre; UseMachine.VMPre ]
    let inserted = match preBase with | glyph :: _ -> glyph | [] -> 1u
    gsubTable tag
        [ "locl", [ 0 ]; "ccmp", [ 1; 23 ]; "nukt", [ 2 ]; "akhn", [ 3 ]; "rphf", [ 4; 5 ]; "pref", [ 6; 7 ]; "rkrf", [ 8 ]; "abvf", [ 9 ]
          "blwf", [ 10 ]; "half", [ 11 ]; "pstf", [ 12 ]; "vatu", [ 13 ]; "cjct", [ 14 ]; "isol", [ 15 ]; "init", [ 16 ]; "medi", [ 17 ]
          "fina", [ 18 ]; "abvs", [ 19 ]; "blws", [ 20 ]; "haln", [ 21 ]; "pres", [ 22 ]; "psts", [ 24 ] ]
        [ single letters 30000
          single (glyphsOf [ UseMachine.B ]) 1000
          single (glyphsOf [ UseMachine.N; UseMachine.O ]) 2000
          ligature (group 0u) (group 1u) 1u
          ligature (glyphsOf [ UseMachine.R ]) halants 2u
          single (glyphsOf [ UseMachine.R ]) 3000
          ligature halants (group 2u) 3u
          single (glyphsOf [ UseMachine.SUB; UseMachine.MPre ]) 4000
          single (glyphsOf [ UseMachine.CMAbv; UseMachine.CMBlw ]) 5000
          ligature (group 1u) (glyphsOf [ UseMachine.VAbv ]) 4u
          ligature halants (group 0u @ group 1u) 5u
          single (group 0u @ group 1u) 6000
          single (glyphsOf [ UseMachine.VPst; UseMachine.FPst ]) 7000
          ligature (group 2u) (group 2u) 6u
          ligature bases (glyphsOf [ UseMachine.SUB ]) 7u
          single bases 8000
          single (group 0u) 9000
          single (group 1u) 10000
          single (group 2u @ glyphsOf [ UseMachine.VAbv ]) 11000
          single (glyphsOf [ UseMachine.VAbv; UseMachine.VMAbv ]) 12000
          single (glyphsOf [ UseMachine.VBlw; UseMachine.VMBlw ]) 13000
          single halants 14000
          single (preBase @ glyphsOf [ UseMachine.MPre ]) 15000
          multipleSubstitution (List.init 6 (fun index -> 60_001u + uint32 index)) inserted
          single (glyphsOf [ UseMachine.VPst; UseMachine.VMPst ]) 16000 ]

/// <summary>The synthetic fonts of a script, written once: a font of the shaper's features, a font with no layout table, a font
/// without the dotted circle, and a font without the invisible stackers and halants.</summary>
/// <param name="script">The script.</param>
/// <param name="tag">The GSUB script tag of the fonts.</param>
/// <returns>The fonts, each with its name.</returns>
let private syntheticFonts (script: UseScript) (tag: string) : (string * TestFont) list =
    let name = script.Name.ToLowerInvariant()
    let mapped = script.Codepoints @ sharedCodepoints @ [ 0x25CCu; 0xA0u ] @ latin |> List.distinct |> List.sort
    let full = distinctMapping mapped
    let without (drop: uint32 -> bool) = full |> List.filter (fun (codepoint, _) -> not (drop codepoint))
    let noCircle = without (fun codepoint -> codepoint = 0x25CCu)
    let noHalants = without (fun codepoint -> List.contains (UseTable.category codepoint) [ UseMachine.H; UseMachine.IS; UseMachine.HVM; UseMachine.Sk ])
    let withGsub (mapping: (uint32 * uint32) list) = [ tagOf "GSUB", useGsub tag mapping ]
    [ $"{name}-features", syntheticFont $"use-{name}-features.otf" full (withGsub full)
      $"{name}-no-layout", syntheticFont $"use-{name}-plain.otf" full []
      $"{name}-no-dotted-circle", syntheticFont $"use-{name}-no-circle.otf" noCircle (withGsub noCircle)
      $"{name}-no-halants", syntheticFont $"use-{name}-no-halants.otf" noHalants (withGsub noHalants) ]

/// <summary>The scripts that get synthetic fonts: those the corpus faces cover, so that the differential reaches each fallback of the
/// features with fonts that have the features of the shaper whatever the face.</summary>
/// <param name="script">The script.</param>
/// <returns>The GSUB script tag the fonts of the script carry.</returns>
let private gsubTagOf (script: UseScript) : string =
    match script.Name with
    | "Deva" -> "dev3"
    | "Beng" -> "bng3"
    | "Guru" -> "gur3"
    | "Gujr" -> "gjr3"
    | "Orya" -> "ory3"
    | "Taml" -> "tml3"
    | "Telu" -> "tel3"
    | "Knda" -> "knd3"
    | "Mlym" -> "mlm3"
    | name -> name.ToLowerInvariant()

/// <summary>The synthetic fonts of every script, written once, those the engine shapes: a font with no layout table of a script that
/// has an Indic shaper when its GSUB table does not have the script under a tag of the third generation is shaped by the Indic
/// shaper.</summary>
let private syntheticByScript : Lazy<(UseScript * (string * TestFont) list) list> =
    lazy [ for script in useScripts.Value -> script, syntheticFonts script (gsubTagOf script) |> List.filter (fun (_, font) -> shapedByUse script.Script font) ]

/// <summary>The labels of the faces of a script.</summary>
/// <param name="script">The script.</param>
/// <returns>The labels.</returns>
let private facesOf (script: UseScript) : TestFont list = script.Faces

/// <summary>The shaping differential of one script: Farfbuzz against HarfBuzz on random text of the script, on every corpus face
/// that maps some of it and that the engine takes, each with its layout, without layout tables, without GPOS and without GSUB, and
/// on the synthetic fonts.</summary>
/// <param name="script">The script.</param>
/// <param name="synthetic">The synthetic fonts of the script.</param>
/// <returns>The tests.</returns>
let private shapeTests (script: UseScript) (synthetic: (string * TestFont) list) : Test =
    let classes = classesOf script
    let options = optionsFor script true
    let faces = facesOf script
    let listed = synthetic |> List.map fst |> String.concat " "
    Console.WriteLine $"use {script.Name} synthetic: {synthetic.Length} fonts ({listed}), {CasesPerSyntheticFont} cases each, {synthetic.Length * CasesPerSyntheticFont} cases"
    testList $"P6.S22 P6.S23 use {script.Name} shape" (
        sampledTexts $"use {script.Name}" faces variants options (useText classes) CasesPerScript
        @ [ for name, font in synthetic -> ShapeDiff.againstTexts $"use {script.Name} synthetic {name}" font.Path options (useText classes (coverageOf font)) CasesPerSyntheticFont ])

/// <summary>The shaping differentials, one list per script.</summary>
/// <returns>The tests.</returns>
let shapeTestsOfAllScripts () : Test =
    testList "P6.S22 P6.S23 use shape" [ for script, synthetic in syntheticByScript.Value -> shapeTests script synthetic ]

/// <summary>The plan differential: the feature tags and the GSUB and GPOS lookup sets Farfbuzz and HarfBuzz compile for the corpus
/// faces of each script and for the synthetic fonts, in each direction.</summary>
/// <returns>The tests.</returns>
let planTests () : Test =
    testList "P6.S22 plan" [
        for script, synthetic in syntheticByScript.Value do
            yield PlanDiff.featuresAgainst $"use {script.Name} plan" (planCases (facesOf script @ (synthetic |> List.map snd)) [ script.Option; $"{script.Option} --language=en" ])
    ]

/// <summary>What the trace differential compares of one glyph: the glyph, its mask with the glyph flags, its cluster, its syllable
/// byte, its category and its auxiliary byte, the action of the Arabic joining, its glyph properties (the class, the substituted,
/// ligated and multiplied bits) and its ligature properties.</summary>
type private UseTraced = uint32 * uint32 * uint32 * byte * byte * byte * uint16 * byte

/// <summary>The fixed snapshots HarfBuzz takes of the engine's pipeline: the start of the reordering (after the syllables are found
/// and the repha and the pre-base consonant recorded), the end of the insertion of dotted circles, the end of the reordering, and
/// the start and the end of the GSUB table.</summary>
let private useMessages : string list =
    [ "start reordering USE"; "end inserting dotted-circles"; "end reordering USE"; "start table GSUB"; "end table GSUB" ]

/// <summary>Reads the glyphs of a HarfBuzz snapshot: the glyph properties are the low half of <c>var1</c>, the ligature properties its
/// third byte and the syllable its high byte, the category the third byte of <c>var2</c> and the auxiliary byte its fourth.</summary>
/// <param name="infos">The glyph infos.</param>
/// <returns>The glyphs.</returns>
let private tracedOfOracle (infos: GlyphInfo array voption) : UseTraced array voption =
    infos |> ValueOption.map (Array.map (fun info -> info.Codepoint, info.Mask, info.Cluster, byte (info.Var1 >>> 24), byte (info.Var2 >>> 16), byte (info.Var2 >>> 24), uint16 info.Var1, byte (info.Var1 >>> 16)))

/// <summary>Reads the glyphs of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyphs.</returns>
let private tracedOfRun (run: GlyphRun) : UseTraced array =
    Array.init run.Infos.Length (fun index ->
        run.Infos[index].Codepoint, run.Infos[index].Mask, run.Infos[index].Cluster, run.Props[index].Syllable, run.Props[index].ShaperCategory, run.Props[index].ShaperAuxiliary, run.Props[index].GlyphClass, run.Props[index].LigProps)

/// <summary>The snapshots Farfbuzz has: at the start of each lookup, by the index of the lookup in the font, and at the pause points.</summary>
type private UseSnapshots =
    { Lookups: Map<int, UseTraced array>
      Start: GlyphRun voption
      Circled: GlyphRun voption
      End: GlyphRun voption }

/// <summary>Runs one lookup of the plan's map on its own, taking the run at its start the first time the lookup runs.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="position">The position of the lookup in the map.</param>
/// <param name="run">The run.</param>
/// <param name="snapshots">The snapshots so far.</param>
/// <returns>The run after the lookup and the snapshots.</returns>
let private walkLookup (plan: OtShapePlan) (font: Font) position (run: GlyphRun) (snapshots: UseSnapshots) : struct (GlyphRun * UseSnapshots) =
    let lookup = plan.Map.GsubLookups[position]
    let applies = lookup.Mask <> 0u && run.Infos.Length > 0
    let recorded =
        match applies && not (Map.containsKey lookup.LookupIndex snapshots.Lookups) with
        | true -> { snapshots with Lookups = Map.add lookup.LookupIndex (tracedOfRun run) snapshots.Lookups }
        | false -> snapshots
    struct (OtLayout.substituteWindow plan font position (position + 1) run, recorded)

/// <summary>Runs the lookups of a stage one by one, as <c>hb_ot_map_t::apply</c> does.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="position">The next lookup.</param>
/// <param name="last">The end of the stage's lookups.</param>
/// <param name="run">The run.</param>
/// <param name="snapshots">The snapshots so far.</param>
/// <returns>The run after the stage's lookups and the snapshots.</returns>
[<TailCall>]
let rec private walkStageLookups (plan: OtShapePlan) (font: Font) position last (run: GlyphRun) (snapshots: UseSnapshots) : struct (GlyphRun * UseSnapshots) =
    match position < last with
    | false -> struct (run, snapshots)
    | true ->
        let struct (applied, recorded) = walkLookup plan font position run snapshots
        walkStageLookups plan font (position + 1) last applied recorded

/// <summary>Runs the stages of the plan's GSUB map, a lookup at a time, taking the run at each pause of the reordering.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="stage">The next stage.</param>
/// <param name="first">The first lookup of the stage.</param>
/// <param name="run">The run.</param>
/// <param name="snapshots">The snapshots taken so far.</param>
/// <returns>The run after the last stage and the snapshots.</returns>
[<TailCall>]
let rec private walkStages (plan: OtShapePlan) (font: Font) stage first (run: GlyphRun) (snapshots: UseSnapshots) : struct (GlyphRun * UseSnapshots) =
    match stage < plan.Map.GsubStages.Length with
    | false -> struct (run, snapshots)
    | true ->
        let current = plan.Map.GsubStages[stage]
        let struct (windowed, walked) = walkStageLookups plan font first current.LastLookup run snapshots
        match PauseKind.runsFunction current.Pause with
        | false -> walkStages plan font (stage + 1) current.LastLookup windowed walked
        | true ->
            let applied = ShaperHooks.applyPause current.Pause plan font windowed
            let recorded =
                match current.Pause with
                | UseReorder ->
                    let struct (circled, inserted) = SyllableEdit.insertDottedCircles font 7uy UseMachine.B (ValueSome UseMachine.R) ValueNone windowed
                    { walked with Start = ValueSome windowed; Circled = (match inserted with | true -> ValueSome circled | false -> ValueNone); End = ValueSome applied }
                | NoPause | ClearSyllables | KhmerSetupSyllables | KhmerReorder | IndicSetupSyllables | IndicInitialReordering | IndicFinalReordering
                | ClearSubstitutionFlags | UseSetupSyllables | UseRecordRphf | UseRecordPref
                | MyanmarSetupSyllables | MyanmarReorder | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction -> walked
            walkStages plan font (stage + 1) current.LastLookup applied recorded

/// <summary>The font's lookup indices of the plan's map, in the order the messages name them.</summary>
/// <param name="plan">The plan.</param>
/// <returns>The distinct lookup indices.</returns>
let private lookupIndicesOf (plan: OtShapePlan) : int list =
    [ for lookup in plan.Map.GsubLookups -> lookup.LookupIndex ] |> List.distinct

/// <summary>The messages HarfBuzz sends at the start of each lookup of a plan.</summary>
/// <param name="plan">The plan.</param>
/// <returns>The messages.</returns>
let private lookupMessages (plan: OtShapePlan) : string list =
    lookupIndicesOf plan |> List.map (fun index -> $"start lookup {index}")

/// <summary>The font lookups whose first run is after the pause that clears the syllables: from then on HarfBuzz keeps the cached
/// class of a glyph for the class-based context lookups in the byte of the syllable (<c>context_cache_func</c>,
/// src/hb-ot-layout-gsubgpos.hh:3077-3085), so the byte is not compared.</summary>
/// <param name="plan">The plan.</param>
/// <returns>The lookup indices.</returns>
let private afterClear (plan: OtShapePlan) : Set<int> =
    let cleared = plan.Map.GsubStages |> Seq.tryFind (fun stage -> stage.Pause = ClearSyllables)
    match cleared with
    | None -> Set.empty
    | Some stage ->
        let before = [ for position in 0 .. stage.LastLookup - 1 -> plan.Map.GsubLookups[position].LookupIndex ] |> Set.ofList
        [ for position in stage.LastLookup .. plan.Map.GsubLookups.Length - 1 -> plan.Map.GsubLookups[position].LookupIndex ]
        |> List.filter (fun index -> not (Set.contains index before))
        |> Set.ofList

/// <summary>Leaves the syllable byte out of the glyphs of a snapshot.</summary>
/// <param name="snapshot">The snapshot.</param>
/// <returns>The snapshot with the syllable bytes zero.</returns>
let private withoutSyllables (snapshot: UseTraced array voption) : UseTraced array voption =
    snapshot |> ValueOption.map (Array.map (fun (glyph, mask, cluster, _, category, auxiliary, props, ligature) -> glyph, mask, cluster, 0uy, category, auxiliary, props, ligature))

/// <summary>Leaves the syllable byte out of the snapshots HarfBuzz keeps it garbled in: all of them when the plan is not the engine's
/// (a text of a script of another shaper, whose syllable byte is the class cache of the lookups), and else the end of the GSUB table
/// and the lookups that first run after the syllables are cleared.</summary>
/// <param name="plan">The plan.</param>
/// <param name="snapshots">The snapshots, in the order of the fixed messages and then of the lookups.</param>
/// <returns>The snapshots.</returns>
let private comparable (plan: OtShapePlan) (snapshots: UseTraced array voption list) : UseTraced array voption list =
    let garbled = afterClear plan
    let lookups = lookupIndicesOf plan
    let shapedByUse = plan.Map.GsubStages |> Seq.exists (fun stage -> stage.Pause = UseSetupSyllables)
    snapshots
    |> List.mapi (fun index snapshot ->
        match index with
        | _ when not shapedByUse -> withoutSyllables snapshot
        | 4 -> withoutSyllables snapshot
        | fixedIndex when fixedIndex < useMessages.Length -> snapshot
        | lookupSlot -> match Set.contains lookups[lookupSlot - useMessages.Length] garbled with | true -> withoutSyllables snapshot | false -> snapshot)

/// <summary>The snapshots Farfbuzz has for a text that is not empty: one per message of <see cref="useMessages"/> and then one per
/// lookup of the map, none for a message HarfBuzz would not send.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>The snapshots.</returns>
let private farfbuzzTrace (fontFile: string) (request: ShapeRequest) : UseTraced array voption list =
    let struct (font, plan, synthesized) = beforeSubstitution fontFile request
    let none = { Lookups = Map.empty; Start = ValueNone; Circled = ValueNone; End = ValueNone }
    let struct (final, snapshots) =
        match plan.ApplyMorx with
        | true -> struct (synthesized, none)
        | false -> walkStages plan font 0 0 synthesized none
    let table = match plan.ApplyMorx with | true -> ValueNone | false -> ValueSome (tracedOfRun synthesized)
    let finished = match plan.ApplyMorx with | true -> ValueNone | false -> ValueSome (tracedOfRun final)
    let fixedSnapshots =
        [ snapshots.Start; snapshots.Circled; snapshots.End ] |> List.map (ValueOption.map tracedOfRun)
    let lookups = lookupMessages plan |> List.map (fun message -> Map.tryFind (int (message.Substring "start lookup ".Length)) snapshots.Lookups |> ValueOption.ofOption)
    comparable plan (fixedSnapshots @ [ table; finished ] @ lookups)

/// <summary>The snapshots HarfBuzz sends for the messages of a request, the plan being Farfbuzz's.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>The snapshots.</returns>
let private oracleTrace (fontFile: string) (request: ShapeRequest) : UseTraced array voption list =
    match request.Text.Length with
    | 0 -> List.replicate useMessages.Length ValueNone
    | _ ->
        let struct (_, plan, _) = beforeSubstitution fontFile request
        comparable plan (TraceDiff.traceMessages fontFile request (useMessages @ lookupMessages plan) |> List.map tracedOfOracle)

/// <summary>Both sides' snapshots for an empty text, which HarfBuzz does not shape, or for a text whose plan has no lookup message.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>Farfbuzz's snapshots.</returns>
let private tracedFarfbuzz (fontFile: string) (request: ShapeRequest) : UseTraced array voption list =
    match request.Text.Length with
    | 0 -> List.replicate useMessages.Length ValueNone
    | _ -> farfbuzzTrace fontFile request

/// <summary>A trace differential on random text of a script: the glyphs, clusters, masks, syllables, categories and joining actions
/// at the start of the GSUB table, at the start of every lookup, and at each point of the reordering agree with HarfBuzz's, so a
/// difference is found at its stage.</summary>
/// <param name="name">The test name.</param>
/// <param name="script">The script.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="covered">The code points the face maps.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let private trace (name: string) (script: UseScript) (fontFile: string) (covered: Set<uint32>) (count: int) : Test =
    Differential.sampled name (ShapeDiff.casesOfTexts fontFile (optionsFor script false) (useText (classesOf script) covered)) count
        (fun request -> tracedFarfbuzz fontFile request)
        (fun request -> oracleTrace fontFile request)

/// <summary>The trace differentials: after the syllables are found and the repha and pre-base consonant recorded, around the
/// reordering and at the start of every lookup, on the corpus faces of each script with their layout and on the synthetic fonts.</summary>
/// <returns>The tests.</returns>
let traceTests () : Test =
    let forScript (script: UseScript, synthetic: (string * TestFont) list) =
        let corpus = facesOf script |> List.collect (fun font -> variants font |> List.filter (fun (kind, _) -> kind <> "stripped") |> List.map (fun (kind, variant) -> $"use {script.Name} trace {label font} {kind}", font, variant))
        let synthetics = synthetic |> List.map (fun (name, font) -> $"use trace {name}", font, font)
        let each = max 100 (CasesPerTrace * 50 / max 50 (corpus.Length + synthetics.Length))
        Console.WriteLine $"use {script.Name} trace: {corpus.Length} corpus variants, {synthetics.Length} synthetic fonts, {each} cases each, {(corpus.Length + synthetics.Length) * each} cases"
        testList $"P6.S22 P6.S23 use {script.Name} trace" [ for name, font, variant in corpus @ synthetics -> trace name script variant.Path (coverageOf font) each ]
    testList "P6.S22 P6.S23 use trace" [ for entry in syntheticByScript.Value -> forScript entry ]
