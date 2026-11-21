module Farfbuzz.Diff.ComplexShapers.ArabicDiff

open System
open System.Collections.Immutable
open System.Text
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
let private CasesPerTrace = 2_000

/// <summary>The options of the Arabic differentials: each cluster level, each direction (the vertical ones too, for which the default
/// shaper takes the text), a language, user features that switch the joining features and the ligature features off, the buffer flags
/// of the beginning and end of the text and of the default ignorables, including the flag that removes them, and no script, which
/// HarfBuzz guesses.</summary>
/// <param name="script">The script option, <c>Arab</c> or <c>Syrc</c>.</param>
/// <param name="language">A language of the script.</param>
/// <returns>The option fields.</returns>
let private optionsFor (script: string) (language: string) : string array =
    let prefix = $"--script={script}"
    [| $"{prefix} --direction=rtl --cluster-level=0"; $"{prefix} --direction=rtl --cluster-level=1"
       $"{prefix} --direction=rtl --cluster-level=2"; $"{prefix} --direction=rtl --cluster-level=3"
       $"{prefix} --direction=ltr --cluster-level=1"; $"{prefix} --language={language}"; $"{prefix} --features=-fina"
       $"{prefix} --features=init=0,medi=0"; $"{prefix} --features=rlig=0"; $"{prefix} --features=calt[1:4]=0"; $"{prefix} --direction=rtl --cluster-level=1"
       $"{prefix} --bot --eot"; $"{prefix} --preserve-default-ignorables --cluster-level=2"; $"{prefix} --preserve-default-ignorables --cluster-level=3"
       $"{prefix} --remove-default-ignorables --cluster-level=2"; $"{prefix} --direction=ttb"; $"{prefix} --direction=btt --cluster-level=1"
       $"{prefix} --direction=ttb --features=fina"; $"{prefix} --direction=rtl"; $"{prefix} --direction=rtl"; $"{prefix} --direction=rtl"; String.Empty |]

/// <summary>The file name of a face with its index, a name that is different for each face of the corpus.</summary>
/// <param name="font">The face.</param>
/// <returns>The name.</returns>
let private fileOf (font: TestFont) : string = $"{font.Suite}/{IO.Path.GetFileNameWithoutExtension font.Path}:{font.Index}"

/// <summary>Both joiners: the zero width non-joiner and the zero width joiner.</summary>
let private bothJoiners : uint32 list = [ 0x200Cu; 0x200Du ]

let private arabicRanges : (uint32 * uint32) list =
    [ 0x0600u, 0x06FFu; 0x0750u, 0x077Fu; 0x08A0u, 0x08FFu; 0xFB50u, 0xFDFFu; 0xFE70u, 0xFEFFu ]

let private syriacRanges : (uint32 * uint32) list = [ 0x0700u, 0x074Fu; 0x0860u, 0x086Fu ]

let private latin = spanning [ 0x41u, 0x5Au; 0x61u, 0x7Au; 0x30u, 0x39u; 0x20u, 0x20u ]

/// <summary>The code points of ranges that have a joining type of the Arabic table.</summary>
/// <param name="ranges">The ranges.</param>
/// <param name="types">The joining types.</param>
/// <returns>The code points.</returns>
let private ofTypes (ranges: (uint32 * uint32) list) (types: byte list) : uint32 list =
    spanning ranges |> List.filter (fun u -> types |> List.contains (ArabicTable.joiningType u))

/// <summary>A generator of one code point drawn from the candidates a face maps, and now and then from all of them.</summary>
/// <param name="covered">The code points the face maps.</param>
/// <param name="candidates">The candidates.</param>
/// <returns>The generator.</returns>
let private choose (covered: Set<uint32>) (candidates: uint32 list) : Gen<uint32> =
    match candidates with
    | [] -> Gen.constant 0x20u
    | _ -> HangulKhmerDiff.pick covered candidates

/// <summary>The generator of Arabic or Syriac texts for a face: words of the joining classes with marks, tatweel and joiners,
/// the ligature candidates, digits, Latin, dotted circles and the presentation forms, in valid and odd sequences, and marks after a
/// joiner.</summary>
/// <param name="joiners">The joiners the texts may hold.</param>
/// <param name="ranges">The ranges of the script.</param>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The generator of texts.</returns>
let private joiningText (joiners: uint32 list) (ranges: (uint32 * uint32) list) (covered: Set<uint32>) : Gen<uint32 array> =
    let choose = choose covered
    let dual = choose (ofTypes ranges [ 3uy ])
    let right = choose (ofTypes ranges [ 2uy; 1uy ])
    let alaph = choose (ofTypes ranges [ 4uy; 5uy ])
    let unjoining = choose (ofTypes ranges [ 0uy ])
    let transparent = choose (ofTypes ranges [ 6uy; 7uy ] |> List.filter (fun u -> Unicode.generalCategory u = NonSpacingMark))
    let tatweel = choose [ 0x0640u ]
    let joiner = match joiners with | [] -> Gen.constant 0x20u | _ -> Gen.elements joiners
    let lam = choose [ 0x0644u ]
    let alef = choose [ 0x0622u; 0x0623u; 0x0625u; 0x0627u ]
    let heh = choose [ 0x0647u ]
    let shadda = choose [ 0x0651u ]
    let digits = choose (spanning [ 0x0660u, 0x0669u; 0x06F0u, 0x06F9u ])
    let maybe (probability: int) (item: Gen<uint32 list>) = Gen.frequency [ probability, item; 10 - probability, Gen.constant [] ]
    let marks = maybe 3 (gen { let! first = transparent in let! second = maybe 3 (gen { let! m = transparent in return [ m ] }) in return first :: second })
    let letter = Gen.frequency [ 6, dual; 3, right; 1, alaph; 1, unjoining; 1, tatweel ]
    let glyph = gen { let! l = letter in let! m = marks in let! j = maybe 1 (gen { let! j = joiner in return [ j ] }) in return l :: m @ j }
    let word = gen { let! count = Gen.choose (1, 6) in let! letters = Gen.listOfLength count glyph in return List.concat letters }
    let token =
        Gen.frequency
            [ 24, word
              3, gen { let! l = lam in let! a = alef in return [ l; a ] }
              1, gen { let! a = lam in let! b = lam in let! h = heh in return [ a; b; h ] }
              2, gen { let! l = letter in let! s = shadda in let! m = transparent in return [ l; s; m ] }
              2, gen { let! d = digits in return [ d ] }
              2, gen { let! c = choose (0x25CCu :: latin) in return [ c ] }
              2, gen { let! j = joiner in return [ j ] }
              2, gen { let! j = joiner in let! m = transparent in return [ j; m ] }
              2, gen { let! l = letter in let! m = transparent in return [ l; m ] }
              2, Gen.constant [ 0x20u ]
              1, Gen.constant [ 0x25CCu ] ]
    textOf 5 token

/// <summary>Adds an item context to a case now and then: the text before and after the item, which affects the joining at the
/// edges, <c>hb_buffer_add_utf</c> with a context.</summary>
/// <param name="context">The generator of one context character.</param>
/// <param name="cases">The cases.</param>
/// <returns>The cases, some with a context.</returns>
let private withContext (context: Gen<uint32>) (cases: Gen<ShapeRequest>) : Gen<ShapeRequest> =
    gen {
        let! request = cases
        let! choice = Gen.choose (0, 9)
        let! before = Gen.listOfLength (match choice with | 0 | 1 | 2 | 3 -> 2 | _ -> 0) context
        let! after = Gen.listOfLength (match choice with | 0 | 1 | 4 | 5 -> 2 | _ -> 0) context
        return { request with Before = List.toArray before; After = List.toArray after }
    }

/// <summary>Reads the code points of a face's cmap as the code points and their glyphs.</summary>
/// <param name="mapping">The mapping.</param>
/// <param name="ranges">The ranges.</param>
/// <returns>The glyphs of the code points of the ranges.</returns>
let private glyphsOfRanges (mapping: (uint32 * uint32) list) (ranges: (uint32 * uint32) list) : uint32 list = glyphsIn mapping ranges

/// <summary>Builds a multiple substitution lookup, each glyph replaced by a sequence.</summary>
/// <param name="replacements">The glyphs and their sequences.</param>
/// <returns>The lookup table.</returns>
let private multipleSubstitution (replacements: (uint32 * uint32 list) list) : byte array =
    let sorted = replacements |> List.sortBy fst
    let sequences = sorted |> List.map (fun (_, glyphs) -> Array.concat ([ be16 glyphs.Length ] @ (glyphs |> List.map (fun glyph -> be16 (int glyph)))))
    let header = 6 + 2 * sorted.Length
    let offsets = sequences |> List.scan (fun offset sequence -> offset + sequence.Length) header
    let coverageAt = List.last offsets
    let subtable =
        Array.concat (
            [ be16 1; be16 coverageAt; be16 sorted.Length ]
            @ (List.take sorted.Length offsets |> List.map be16)
            @ sequences
            @ [ coverageTable (sorted |> List.map fst) ])
    Array.concat [ be16 2; be16 0; be16 1; be16 8; subtable ]

/// <summary>The code points the synthetic Arabic fonts start from: the Arabic and Syriac blocks, the presentation forms, the
/// joiners, the dotted circle and Latin.</summary>
let private arabicCodepoints : uint32 list =
    spanning [ 0x0600u, 0x06FFu; 0x0700u, 0x074Fu; 0x0750u, 0x077Fu; 0x08A0u, 0x08FFu; 0xFB50u, 0xFDFFu; 0xFE70u, 0xFEFFu; 0x200Cu, 0x200Du; 0x25CCu, 0x25CCu ] @ latin

/// <summary>The GSUB table of the synthetic Arabic fonts: one lookup for each feature of the shaper, each adding a delta of its own
/// to the glyphs of a group, so that the order of the lookups, the masks and the actions show; <c>stch</c> multiplies a letter.</summary>
/// <param name="script">The script tag of the table.</param>
/// <param name="withJoining">Whether the font has the joining features.</param>
/// <param name="withRclt">Whether the font has <c>rclt</c>.</param>
/// <param name="mapping">The code points and glyphs of the font.</param>
/// <returns>The table.</returns>
let private arabicGsub (script: string) (withJoining: bool) (withRclt: bool) (mapping: (uint32 * uint32) list) : byte array =
    let group (types: byte list) (remainder: uint32) =
        mapping |> List.filter (fun (codepoint, _) -> codepoint < 0xFB00u && types |> List.contains (ArabicTable.joiningType codepoint) && codepoint % 3u = remainder) |> List.map snd
    let letters = [ 3uy; 2uy; 1uy; 4uy; 5uy ]
    let lamAlef = glyphsIn mapping [ 0x0644u, 0x0644u ], glyphsIn mapping [ 0x0622u, 0x0623u; 0x0625u, 0x0625u; 0x0627u, 0x0627u ]
    let stretch =
        match glyphsIn mapping [ 0x0710u, 0x0710u; 0x0711u, 0x0715u ] with
        | ligatured :: rest when rest.Length >= 4 -> [ ligatured, rest |> List.take 4 ]
        | _ -> []
    let joining =
        [ "isol", [ 2 ]; "fina", [ 3 ]; "fin2", [ 4 ]; "fin3", [ 5 ]; "medi", [ 6 ]; "med2", [ 7 ]; "init", [ 8 ] ]
    let others = [ "ccmp", [ 0; 12 ]; "locl", [ 1 ]; "rlig", [ 9 ]; "calt", [ 10 ]; "liga", [ 11 ]; "clig", [ 13 ]; "mset", [ 1 ] ]
    let features = (match withJoining with | true -> joining | false -> []) @ others @ [ "stch", [ 14 ] ] @ (match withRclt with | true -> [ "rclt", [ 12 ] ] | false -> [])
    gsubTable script features
        [ singleSubstitution (glyphsIn mapping [ 0x41u, 0x5Au; 0x30u, 0x39u ]) 30000
          singleSubstitution (glyphsIn mapping [ 0x0660u, 0x0669u ]) 31000
          singleSubstitution (group letters 0u) 1000
          singleSubstitution (group letters 1u) 2000
          singleSubstitution (group letters 2u) 3000
          singleSubstitution (group letters 0u @ group letters 1u) 4000
          singleSubstitution (group letters 1u @ group letters 2u) 5000
          singleSubstitution (group letters 2u @ group letters 0u) 6000
          singleSubstitution (group letters 0u) 7000
          ligatureSubstitution (fst lamAlef) (snd lamAlef) (List.head (glyphsIn mapping [ 0x41u, 0x41u ]))
          singleSubstitution (group [ 3uy ] 1u) 8000
          singleSubstitution (glyphsIn mapping [ 0x0640u, 0x0640u ]) 9000
          singleSubstitution (group [ 3uy ] 2u) 10000
          singleSubstitution (group [ 2uy ] 0u) 11000
          multipleSubstitution (match stretch with | [] -> [ 1u, [ 1u; 2u ] ] | other -> other) ]

/// <summary>A font that looks Windows-1256 encoded: the letters sit at their code page positions and there is no layout table.</summary>
/// <returns>The font.</returns>
let private win1256Font () : TestFont =
    Encoding.RegisterProvider CodePagesEncodingProvider.Instance
    let encoding = Encoding.GetEncoding 1256
    let mapping =
        [ for value in 0x20 .. 0xFF do
            let text = encoding.GetString [| byte value |]
            let codepoint = uint32 (Char.ConvertToUtf32 (text, 0))
            if text.Length = 1 && codepoint <> 0xFFFDu then yield codepoint, uint32 value ]
    syntheticFont "arabic-win1256.otf" (mapping |> List.distinctBy fst |> List.sortBy fst) []

/// <summary>The synthetic Arabic fonts: every feature, no joining features, the presentation forms without layout (and with the
/// glyphs in the reverse order of the code points, which the fallback lookups must sort), no layout and no presentation forms,
/// <c>rclt</c>, a script the shaper does not know, and a Windows-1256 font.</summary>
/// <returns>The fonts, each with its name.</returns>
let private arabicFonts () : (string * TestFont) list =
    let full = distinctMapping arabicCodepoints
    let letters = distinctMapping (arabicCodepoints |> List.filter (fun u -> u < 0xFB00u))
    let reversed = full |> List.map (fun (codepoint, glyph) -> codepoint, uint32 full.Length + 1u - glyph)
    [ "arabic-features", syntheticFont "arabic-gsub.otf" full [ tagOf "GSUB", arabicGsub "arab" true false full ]
      "arabic-no-joining", syntheticFont "arabic-no-joining.otf" full [ tagOf "GSUB", arabicGsub "arab" false false full ]
      "arabic-forms-no-layout", syntheticFont "arabic-forms.otf" full []
      "arabic-forms-reversed", syntheticFont "arabic-forms-reversed.otf" reversed []
      "arabic-letters-only", syntheticFont "arabic-letters.otf" letters [ tagOf "GSUB", arabicGsub "arab" false false letters ]
      "arabic-rclt", syntheticFont "arabic-rclt.otf" full [ tagOf "GSUB", arabicGsub "arab" true true full ]
      "arabic-latn", syntheticFont "arabic-latn.otf" full [ tagOf "GSUB", arabicGsub "latn" true false full ]
      "arabic-dflt", syntheticFont "arabic-dflt.otf" full [ tagOf "GSUB", arabicGsub "DFLT" true false full ]
      "syriac-features", syntheticFont "syriac-gsub.otf" full [ tagOf "GSUB", arabicGsub "syrc" true false full ]
      "syriac-dflt", syntheticFont "syriac-dflt.otf" full [ tagOf "GSUB", arabicGsub "DFLT" true false full ]
      "arabic-win1256", win1256Font () ]

/// <summary>The synthetic Arabic fonts, written once.</summary>
let private arabicSynthetic : Lazy<(string * TestFont) list> = lazy (arabicFonts ())

/// <summary>The Arabic faces of the corpus.</summary>
let private arabicFaces : Lazy<TestFont list> = lazy (corpusFaces arabicRanges)

/// <summary>The Syriac faces of the corpus.</summary>
let private syriacFaces : Lazy<TestFont list> = lazy (corpusFaces syriacRanges)

/// <summary>A generator of one character of a context: a letter, a mark, a joiner or a Latin character.</summary>
let private contextCharacter : Gen<uint32> =
    Gen.elements (ofTypes arabicRanges [ 3uy; 2uy; 0uy; 6uy ] @ [ 0x200Cu; 0x200Du; 0x20u; 0x41u ] |> List.toArray |> Array.truncate 400 |> Array.toList)

/// <summary>A sampled differential for each variant of each face, with item contexts, on texts of a generator built from the
/// face's coverage.</summary>
/// <param name="prefix">The test name prefix.</param>
/// <param name="joiners">The joiners the texts may hold.</param>
/// <param name="faces">The faces.</param>
/// <param name="options">The option fields.</param>
/// <param name="ranges">The ranges of the script.</param>
/// <param name="total">The number of cases to reach over the variants.</param>
/// <returns>The tests.</returns>
let private sampled (prefix: string) (joiners: uint32 list) (faces: TestFont list) (options: string array) (ranges: (uint32 * uint32) list) (total: int) : Test list =
    let all = faces |> List.collect (fun font -> variants font |> List.map (fun (kind, variant) -> font, kind, variant))
    let each = max CasesPerVariantMinimum ((total + all.Length - 1) / max 1 all.Length)
    Console.WriteLine $"{prefix}: {faces.Length} faces ({names faces}), {all.Length} variants, {each} cases each, {each * all.Length} cases"
    [ for font, kind, variant in all ->
        let cases = withContext contextCharacter (ShapeDiff.casesOfTexts variant.Path options (joiningText joiners ranges (coverageOf font)))
        ShapeDiff.againstCases $"{prefix} {fileOf font} {kind}" cases each ]

/// <summary>A sampled differential for each synthetic font, with item contexts.</summary>
/// <param name="prefix">The test name prefix.</param>
/// <param name="joiners">The joiners the texts may hold.</param>
/// <param name="fonts">The fonts.</param>
/// <param name="options">The option fields.</param>
/// <param name="ranges">The ranges of the script.</param>
/// <returns>The tests.</returns>
let private sampledSyntheticFonts (prefix: string) (joiners: uint32 list) (fonts: (string * TestFont) list) (options: string array) (ranges: (uint32 * uint32) list) : Test list =
    let listed = fonts |> List.map fst |> String.concat " "
    Console.WriteLine $"{prefix}: {fonts.Length} synthetic fonts ({listed}), {CasesPerSynthetic} cases each, {fonts.Length * CasesPerSynthetic} cases"
    [ for name, font in fonts ->
        let cases = withContext contextCharacter (ShapeDiff.casesOfTexts font.Path options (joiningText joiners ranges (coverageOf font)))
        ShapeDiff.againstCases $"{prefix} {name}" cases CasesPerSynthetic ]

/// <summary>The Arabic shaping differential: Farfbuzz against HarfBuzz on random Arabic text with item contexts, on every corpus face
/// that maps some Arabic, each with its layout, without layout tables and without GPOS, and on the synthetic fonts.</summary>
/// <returns>The tests.</returns>
let arabicTests () : Test =
    testList "P6.S11 arabic shape" (
        sampled "arabic" bothJoiners arabicFaces.Value (optionsFor "Arab" "ar") arabicRanges CasesPerScript
        @ sampledSyntheticFonts "arabic synthetic" bothJoiners arabicSynthetic.Value (optionsFor "Arab" "fa") arabicRanges)

/// <summary>The Syriac shaping differential, as <see cref="arabicTests"/>.</summary>
/// <returns>The tests.</returns>
let syriacTests () : Test =
    testList "P6.S11 syriac shape" (
        sampled "syriac" bothJoiners syriacFaces.Value (optionsFor "Syrc" "syr") syriacRanges CasesPerScript
        @ sampledSyntheticFonts "syriac synthetic" bothJoiners arabicSynthetic.Value (optionsFor "Syrc" "syr") syriacRanges)

/// <summary>The plan differential: the feature tags and the GSUB and GPOS lookup sets Farfbuzz and HarfBuzz compile for the Arabic and
/// Syriac faces and the synthetic fonts, in each direction.</summary>
/// <returns>The tests.</returns>
let planTests () : Test =
    let faces = (arabicFaces.Value @ syriacFaces.Value |> List.distinct) @ (arabicSynthetic.Value |> List.map snd)
    testList "P6.S11 plan" [
        PlanDiff.featuresAgainst "arabic plan" (planCases faces [ "--script=Arab"; "--script=Arab --language=ar"; "--script=Syrc" ])
    ]

/// <summary>What the trace differential compares of one glyph at a pause point: the glyph, its mask with the glyph flags, its
/// cluster and its shaping action.</summary>
type private TracedGlyph = uint32 * uint32 * uint32 * byte

/// <summary>Reads the glyphs of a HarfBuzz snapshot: the shaping action is the high byte of <c>var2</c>.</summary>
/// <param name="infos">The glyph infos.</param>
/// <returns>The glyphs.</returns>
let private tracedOracle (infos: GlyphInfo array voption) : TracedGlyph array voption =
    infos |> ValueOption.map (Array.map (fun info -> info.Codepoint, info.Mask, info.Cluster, byte (info.Var2 >>> 24)))

/// <summary>Reads the glyphs of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyphs.</returns>
let private tracedRun (run: GlyphRun) : TracedGlyph array =
    Array.init run.Infos.Length (fun index -> run.Infos[index].Codepoint, run.Infos[index].Mask, run.Infos[index].Cluster, run.Props[index].ShaperAuxiliary)

/// <summary>The tag of a lookup as the text of HarfBuzz's trace messages.</summary>
/// <param name="tag">The tag.</param>
/// <returns>The four characters.</returns>
let private tagText (tag: Tag) : string = Tag.toString tag

/// <summary>The message of the start of the lookup at a position of the map.</summary>
/// <param name="plan">The plan.</param>
/// <param name="position">The position.</param>
/// <returns>The message, none past the last lookup.</returns>
let private startMessage (plan: OtShapePlan) (position: int) : string voption =
    match position < plan.Map.GsubLookups.Length with
    | false -> ValueNone
    | true -> ValueSome $"start lookup {plan.Map.GsubLookups[position].LookupIndex} feature '{tagText plan.Map.GsubLookups[position].FeatureTag}'"

/// <summary>The message of the end of the lookup at a position of the map.</summary>
/// <param name="plan">The plan.</param>
/// <param name="position">The position.</param>
/// <returns>The message, none for a position before the first lookup.</returns>
let private endMessage (plan: OtShapePlan) (position: int) : string voption =
    match position >= 0 && position < plan.Map.GsubLookups.Length with
    | false -> ValueNone
    | true -> ValueSome $"end lookup {plan.Map.GsubLookups[position].LookupIndex} feature '{tagText plan.Map.GsubLookups[position].FeatureTag}'"

/// <summary>Whether a pause is one of the three the Arabic shaper adds.</summary>
/// <param name="pause">The pause.</param>
/// <returns>True for the pause that records stch, the fallback and the release.</returns>
let private isArabicPause (pause: PauseKind) =
    match pause with
    | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction -> true
    | NoPause | ClearSyllables | KhmerSetupSyllables | KhmerReorder | IndicSetupSyllables | IndicInitialReordering | IndicFinalReordering
    | ClearSubstitutionFlags | UseSetupSyllables | UseRecordRphf | UseRecordPref | UseReorder | MyanmarSetupSyllables | MyanmarReorder -> false

/// <summary>Walks the stages of the plan, one window at a time, collecting the run at the end of the lookup before and at the start of
/// the lookup after each pause of the Arabic shaper, as the messages of HarfBuzz name them.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="stage">The next stage.</param>
/// <param name="first">The first lookup of the stage.</param>
/// <param name="run">The run.</param>
/// <param name="found">The messages and their snapshots so far.</param>
/// <returns>The messages and their snapshots.</returns>
let rec private walk (plan: OtShapePlan) (font: Font) stage first (run: GlyphRun) (found: (string * TracedGlyph array) list) : (string * TracedGlyph array) list =
    match stage >= plan.Map.GsubStages.Length with
    | true -> found
    | false ->
        let current = plan.Map.GsubStages[stage]
        let windowed = OtLayout.substituteWindow plan font first current.LastLookup run
        let paused = ShaperHooks.applyPause current.Pause plan font windowed
        let before = match isArabicPause current.Pause with | true -> endMessage plan (current.LastLookup - 1) |> ValueOption.filter (fun _ -> current.LastLookup > first) | false -> ValueNone
        let after = match isArabicPause current.Pause with | true -> startMessage plan current.LastLookup | false -> ValueNone
        let withBefore = match before with | ValueSome message -> (message, tracedRun windowed) :: found | ValueNone -> found
        let withAfter = match after with | ValueSome message -> (message, tracedRun paused) :: withBefore | ValueNone -> withBefore
        walk plan font (stage + 1) current.LastLookup paused withAfter

/// <summary>The messages the trace differential asks HarfBuzz for, for a request: the start and end of the GSUB table and the
/// messages around each Arabic pause of the plan Farfbuzz makes.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>The messages, in the order the snapshots come in.</returns>
let private messagesOf (fontFile: string) (request: ShapeRequest) : string list =
    let struct (font, plan, synthesized) = beforeSubstitution fontFile request
    let found = walk plan font 0 0 synthesized []
    [ "start table GSUB" ] @ (found |> List.rev |> List.map fst |> List.distinct) @ [ "end table GSUB" ]

/// <summary>The snapshots Farfbuzz has at the points of <see cref="messagesOf"/>.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <returns>One snapshot per message.</returns>
let private farfbuzzTrace (fontFile: string) (request: ShapeRequest) : TracedGlyph array voption list =
    let struct (font, plan, synthesized) = beforeSubstitution fontFile request
    let found = walk plan font 0 0 synthesized [] |> List.rev
    let messages = found |> List.map fst |> List.distinct
    let first message = found |> List.find (fun (text, _) -> text = message) |> snd
    let table =
        match plan.ApplyMorx with
        | true -> ValueNone, ValueNone
        | false -> ValueSome (tracedRun synthesized), ValueSome (tracedRun (OtLayout.substitute plan font synthesized))
    [ fst table ] @ (messages |> List.map (fun message -> ValueSome (first message))) @ [ snd table ]

/// <summary>A trace differential on random Arabic text: the glyphs, clusters, masks and actions at the start of the GSUB table, around
/// each pause of the shaper and at the end agree with HarfBuzz's, so a difference is found at its stage.</summary>
/// <param name="name">The test name.</param>
/// <param name="joiners">The joiners the texts may hold.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="options">The option fields.</param>
/// <param name="ranges">The ranges of the script.</param>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The test.</returns>
let private arabicTrace (name: string) (joiners: uint32 list) (fontFile: string) (options: string array) (ranges: (uint32 * uint32) list) (covered: Set<uint32>) : Test =
    Differential.sampled name (ShapeDiff.casesOfTexts fontFile options (joiningText joiners ranges covered)) CasesPerTrace
        (fun request -> farfbuzzTrace fontFile request)
        (fun request -> TraceDiff.traceMessages fontFile request (messagesOf fontFile request) |> List.map tracedOracle)

/// <summary>The Arabic trace differentials, on the corpus faces with layout and the synthetic fonts.</summary>
/// <returns>The tests.</returns>
let traceTests () : Test =
    let corpus = arabicFaces.Value |> List.collect (fun font -> variants font |> List.map (fun (kind, variant) -> $"arabic trace {fileOf font} {kind}", variant))
    let synthetic = arabicSynthetic.Value |> List.map (fun (name, font) -> $"arabic trace {name}", font)
    Console.WriteLine $"arabic trace: {arabicFaces.Value.Length} corpus faces ({names arabicFaces.Value}) in {corpus.Length} variants, {synthetic.Length} synthetic fonts, {CasesPerTrace} cases each, {(corpus.Length + synthetic.Length) * CasesPerTrace} cases"
    testList "P6.S11 arabic trace" [
        for name, font in corpus -> arabicTrace name bothJoiners font.Path (optionsFor "Arab" "ar") arabicRanges (coverageOf font)
        for name, font in synthetic -> arabicTrace name bothJoiners font.Path (optionsFor "Arab" "ar") arabicRanges (coverageOf font) ]

/// <summary>The answer of an engine for a request, as the glyph serialisation with glyph flags.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="request">The request.</param>
/// <returns>The text, or why there is none.</returns>
let private answer (shaper: Shaper) (request: ShapeRequest) : Result<string, string> =
    ShapeDiff.serialize (shaper.Shape request)

/// <summary>The fixed cases the Arabic and Myanmar differentials found in the default shaper and in the mark positioning they share:
/// the text, options and face of each, shaped by Farfbuzz and by HarfBuzz and compared as the glyph serialisation with glyph flags.
/// Fallback mark positioning of native right-to-left text after a removed joiner, on a font with no GPOS; and the unsafe-to-concat flag
/// of a lookup that looks ahead over a joiner, vertically, on a font whose GPOS is renamed out.</summary>
/// <returns>The tests.</returns>
let foundCases () : Test =
    let ours = FarfbuzzShaper.shaper
    let oracle = create ()
    let agree (name: string) (font: TestFont) (options: string) (codepoints: uint32 array) =
        testCase name (fun () ->
            let request = ShapeDiff.withFlags (ShapeDiff.request font.Path options codepoints)
            Expect.equal (answer ours request) (answer oracle request) "Farfbuzz shapes the text as HarfBuzz does")
    let synthetic = arabicSynthetic.Value |> List.find (fun (name, _) -> name = "arabic-letters-only") |> snd
    let nastaliq = TestFonts.named "NotoNastaliqUrdu-Regular.ttf" |> withoutGpos |> ValueOption.defaultWith (fun () -> failtest "no GPOS table to rename")
    let gulzar = TestFonts.named "Gulzar-Regular.ttf" |> withoutGpos |> ValueOption.defaultWith (fun () -> failtest "no GPOS table to rename")
    let longText = [| 0x0644u; 0x0627u; 0x0628u; 0x0633u; 0x0020u; 0x0645u; 0x0646u; 0x0640u; 0x0689u; 0x0020u; 0x0639u; 0x0644u; 0x0649u |]
    testList "P6.S11 found cases" [
        agree "marks after a removed joiner, right to left, without GPOS" synthetic "--script=Hebr --direction=rtl --cluster-level=2 --remove-default-ignorables" [| 0x0651u; 0x200Cu; 0x0651u |]
        agree "a lookup that looks ahead over a joiner, vertically, Nastaliq" nastaliq "--script=Arab --direction=ttb --features=fina" [| 0x0640u; 0x0689u; 0x0020u; 0x200Cu |]
        agree "long text vertically, Gulzar" gulzar "--script=Arab --direction=ttb" longText
        agree "long text vertically with a trailing joiner, Gulzar" gulzar "--script=Arab --direction=ttb" (Array.append longText [| 0x200Du |])
    ]
