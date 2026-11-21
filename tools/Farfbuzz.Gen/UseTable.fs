module Farfbuzz.Gen.UseTable

open System
open System.Collections.Generic
open System.IO
open Farfbuzz.Gen

/// <summary>The upstream src directory.</summary>
let private sourceDirectory (root: string) = Path.Combine(root, "upstream", "harfbuzz", "src")

/// <summary>The pinned Unicode data directory.</summary>
let private dataDirectory (root: string) = Path.Combine(root, "tools", "data", "ucd")

/// <summary>Scripts whose code points never reach the table (src/gen-use-table.py:40-46).</summary>
let private disabledScripts = [| "Arabic"; "Lao"; "Samaritan"; "Syriac"; "Thai" |]

/// <summary>The default value of each of the seven merged slots (src/gen-use-table.py:96).</summary>
let private defaults =
    [| "Other"; "Not_Applicable"; "jt_X"; String.Empty; "Cn"; "No_Block"; "Unknown" |]

/// <summary>True when a value is one of the given values.</summary>
/// <param name="values">The candidate values.</param>
/// <param name="value">The value to test.</param>
/// <returns>Whether the value is present.</returns>
let private oneOf (values: string list) (value: string) = List.contains value values

/// <summary>True when a code point is one of the given code points.</summary>
/// <param name="values">The candidate code points.</param>
/// <param name="value">The code point to test.</param>
/// <returns>Whether the code point is present.</returns>
let private oneOfU32 (values: uint32 list) (value: uint32) = List.contains value values

/// <summary>The identity transform.</summary>
/// <param name="value">The value.</param>
/// <returns>The value.</returns>
let private id (value: string) = value

/// <summary>Maps the ISC-Additional Consonant_Final_Modifier to Syllable_Modifier (src/gen-use-table.py:85-87).</summary>
/// <param name="value">The category name.</param>
/// <returns>The mapped name.</returns>
let private syllableModifier (value: string) =
    match value = "Consonant_Final_Modifier" with
    | true -> "Syllable_Modifier"
    | false -> value

/// <summary>Maps the IPC-Additional NA to Not_Applicable (src/gen-use-table.py:88-89).</summary>
/// <param name="value">The position name.</param>
/// <returns>The mapped name.</returns>
let private notApplicable (value: string) =
    match value = "NA" with
    | true -> "Not_Applicable"
    | false -> value

/// <summary>Reads one Unicode data file into a code point map (src/gen-use-table.py:59-94).</summary>
/// <param name="target">The map written into, so additional files merge over the base file.</param>
/// <param name="path">The file path.</param>
/// <param name="fieldIndex">The semicolon field holding the value.</param>
/// <param name="keep">Whether to keep a line's value.</param>
/// <param name="transform">The value transform.</param>
let private readInto (target: Dictionary<uint32, string>) (path: string) (fieldIndex: int) (keep: string -> bool) (transform: string -> string) =
    for line in File.ReadAllLines path do
        let comment = line.IndexOf '#'
        let line = if comment >= 0 then line.Substring(0, comment) else line
        let fields = line.Split(';') |> Array.map (fun field -> field.Trim())
        match fields.Length <= fieldIndex with
        | true -> ()
        | false ->
            let range = fields[0].Split([| ".." |], StringSplitOptions.None)
            let start = Convert.ToInt32(range[0], 16)
            let stop = if range.Length = 1 then start else Convert.ToInt32(range[1], 16)
            let raw = fields[fieldIndex]
            match keep raw with
            | false -> ()
            | true ->
                let value = transform raw
                for u in start .. stop do
                    target[uint32 u] <- value

/// <summary>The USE category exports of the use machine, name to byte value.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The category name to byte value map.</returns>
let private categoryValues (root: string) : Map<string, int> =
    let path = Path.Combine(sourceDirectory root, "hb-ot-shaper-use-machine.hh")
    match RagelFlat.parse path "use" with
    | Ok flat -> flat.Exports |> Map.ofList
    | Error error -> failwith error

/// <summary>is_BASE (src/gen-use-table.py:213-221).</summary>
let private isBase (_: uint32) (uisc: string) (_: bool) (ugc: string) (ajt: string) =
    oneOf [ "Number"; "Consonant"; "Consonant_Head_Letter"; "Tone_Letter"; "Vowel_Independent" ] uisc
    || (oneOf [ "jt_C"; "jt_D"; "jt_L"; "jt_R" ] ajt && uisc <> "Joiner")
    || (ugc = "Lo" && oneOf [ "Avagraha"; "Bindu"; "Consonant_Final"; "Consonant_Medial"; "Consonant_Subjoined"; "Vowel"; "Vowel_Dependent" ] uisc)

/// <summary>is_BASE_NUM (src/gen-use-table.py:222-223).</summary>
let private isBaseNum (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Brahmi_Joining_Number"

/// <summary>is_BASE_OTHER (src/gen-use-table.py:224-226).</summary>
let private isBaseOther (u: uint32) (uisc: string) (_: bool) (_: string) (_: string) =
    uisc = "Consonant_Placeholder" || oneOfU32 [ 0x2015u; 0x2022u; 0x25FBu; 0x25FCu; 0x25FDu; 0x25FEu ] u

/// <summary>is_CGJ (src/gen-use-table.py:227-229).</summary>
let private isCgj (_: uint32) (uisc: string) (udi: bool) (ugc: string) (_: string) =
    uisc = "Joiner" || (udi && oneOf [ "Mc"; "Me"; "Mn" ] ugc)

/// <summary>is_CONS_FINAL (src/gen-use-table.py:230-232).</summary>
let private isConsFinal (_: uint32) (uisc: string) (_: bool) (ugc: string) (_: string) =
    (uisc = "Consonant_Final" && ugc <> "Lo") || uisc = "Consonant_Succeeding_Repha"

/// <summary>is_CONS_FINAL_MOD (src/gen-use-table.py:233-234).</summary>
let private isConsFinalMod (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Syllable_Modifier"

/// <summary>is_CONS_MED (src/gen-use-table.py:235-238).</summary>
let private isConsMed (_: uint32) (uisc: string) (_: bool) (ugc: string) (_: string) =
    (uisc = "Consonant_Medial" && ugc <> "Lo") || uisc = "Consonant_Initial_Postfixed"

/// <summary>is_CONS_MOD (src/gen-use-table.py:239-240).</summary>
let private isConsMod (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) =
    oneOf [ "Nukta"; "Gemination_Mark"; "Consonant_Killer" ] uisc

/// <summary>is_CONS_SUB (src/gen-use-table.py:241-242).</summary>
let private isConsSub (_: uint32) (uisc: string) (_: bool) (ugc: string) (_: string) =
    uisc = "Consonant_Subjoined" && ugc <> "Lo"

/// <summary>is_CONS_WITH_STACKER (src/gen-use-table.py:243-244).</summary>
let private isConsWithStacker (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) =
    uisc = "Consonant_With_Stacker"

/// <summary>is_SAKOT (src/gen-use-table.py:284-286).</summary>
let private isSakot (u: uint32) (_: string) (_: bool) (_: string) (_: string) = u = 0x1A60u

/// <summary>is_HALANT_OR_VOWEL_MODIFIER (src/gen-use-table.py:247-249).</summary>
let private isHalantOrVowelModifier (u: uint32) (_: string) (_: bool) (_: string) (_: string) = u = 0x0DCAu

/// <summary>is_HALANT (src/gen-use-table.py:245-246).</summary>
let private isHalant (u: uint32) (uisc: string) (udi: bool) (ugc: string) (ajt: string) =
    uisc = "Virama" && not (isHalantOrVowelModifier u uisc udi ugc ajt)

/// <summary>is_HALANT_NUM (src/gen-use-table.py:250-251).</summary>
let private isHalantNum (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Number_Joiner"

/// <summary>is_HIEROGLYPH (src/gen-use-table.py:252-253).</summary>
let private isHieroglyph (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Hieroglyph"

/// <summary>is_HIEROGLYPH_JOINER (src/gen-use-table.py:254-255).</summary>
let private isHieroglyphJoiner (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Hieroglyph_Joiner"

/// <summary>is_HIEROGLYPH_MIRROR (src/gen-use-table.py:256-257).</summary>
let private isHieroglyphMirror (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Hieroglyph_Mirror"

/// <summary>is_HIEROGLYPH_MOD (src/gen-use-table.py:258-259).</summary>
let private isHieroglyphMod (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Hieroglyph_Modifier"

/// <summary>is_HIEROGLYPH_SEGMENT_BEGIN (src/gen-use-table.py:260-261).</summary>
let private isHieroglyphSegmentBegin (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) =
    oneOf [ "Hieroglyph_Mark_Begin"; "Hieroglyph_Segment_Begin" ] uisc

/// <summary>is_HIEROGLYPH_SEGMENT_END (src/gen-use-table.py:262-263).</summary>
let private isHieroglyphSegmentEnd (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) =
    oneOf [ "Hieroglyph_Mark_End"; "Hieroglyph_Segment_End" ] uisc

/// <summary>is_INVISIBLE_STACKER (src/gen-use-table.py:264-268).</summary>
let private isInvisibleStacker (u: uint32) (uisc: string) (udi: bool) (ugc: string) (ajt: string) =
    uisc = "Invisible_Stacker" && not (isSakot u uisc udi ugc ajt)

/// <summary>is_ZWNJ (src/gen-use-table.py:269-270).</summary>
let private isZwnj (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Non_Joiner"

/// <summary>is_SYM_MOD (src/gen-use-table.py:287-288).</summary>
let private isSymMod (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Symbol_Modifier"

/// <summary>is_Word_Joiner (src/gen-use-table.py:295-300).</summary>
let private isWordJoiner (u: uint32) (uisc: string) (udi: bool) (ugc: string) (ajt: string) =
    (udi
     && not (oneOfU32 [ 0x115Fu; 0x1160u; 0x3164u; 0xFFA0u; 0x1BCA0u; 0x1BCA1u; 0x1BCA2u; 0x1BCA3u ] u)
     && uisc = "Other"
     && not (isCgj u uisc udi ugc ajt))
    || ugc = "Cn"

/// <summary>is_OTHER (src/gen-use-table.py:271-279).</summary>
let private isOther (u: uint32) (uisc: string) (udi: bool) (ugc: string) (ajt: string) =
    (ugc = "Po" || oneOf [ "Consonant_Dead"; "Joiner"; "Modifying_Letter"; "Other" ] uisc)
    && not (isBase u uisc udi ugc ajt)
    && not (isBaseOther u uisc udi ugc ajt)
    && not (isCgj u uisc udi ugc ajt)
    && not (isSymMod u uisc udi ugc ajt)
    && not (isWordJoiner u uisc udi ugc ajt)

/// <summary>is_REORDERING_KILLER (src/gen-use-table.py:280-281).</summary>
let private isReorderingKiller (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) = uisc = "Reordering_Killer"

/// <summary>is_REPHA (src/gen-use-table.py:282-283).</summary>
let private isRepha (_: uint32) (uisc: string) (_: bool) (_: string) (_: string) =
    oneOf [ "Consonant_Preceding_Repha"; "Consonant_Prefixed" ] uisc

/// <summary>is_VOWEL (src/gen-use-table.py:289-291).</summary>
let private isVowel (_: uint32) (uisc: string) (_: bool) (ugc: string) (_: string) =
    uisc = "Pure_Killer" || (ugc <> "Lo" && oneOf [ "Vowel"; "Vowel_Dependent" ] uisc)

/// <summary>is_VOWEL_MOD (src/gen-use-table.py:292-294).</summary>
let private isVowelMod (_: uint32) (uisc: string) (_: bool) (ugc: string) (_: string) =
    oneOf [ "Tone_Mark"; "Cantillation_Mark"; "Register_Shifter"; "Visarga" ] uisc
    || (ugc <> "Lo" && uisc = "Bindu")

/// <summary>The predicate type of <c>use_mapping</c>: code point, UISC, default-ignorable, UGC, joining type.</summary>
type private Predicate = uint32 -> string -> bool -> string -> string -> bool

/// <summary>The 29 predicates of <c>use_mapping</c>, in their evaluation order (src/gen-use-table.py:302-332).</summary>
let private useMapping : (string * Predicate) list =
    [ "B", isBase
      "N", isBaseNum
      "GB", isBaseOther
      "CGJ", isCgj
      "F", isConsFinal
      "FM", isConsFinalMod
      "M", isConsMed
      "CM", isConsMod
      "SUB", isConsSub
      "CS", isConsWithStacker
      "H", isHalant
      "HVM", isHalantOrVowelModifier
      "HN", isHalantNum
      "IS", isInvisibleStacker
      "G", isHieroglyph
      "HM", isHieroglyphMod
      "HR", isHieroglyphMirror
      "J", isHieroglyphJoiner
      "SB", isHieroglyphSegmentBegin
      "SE", isHieroglyphSegmentEnd
      "ZWNJ", isZwnj
      "O", isOther
      "RK", isReorderingKiller
      "R", isRepha
      "Sk", isSakot
      "SM", isSymMod
      "V", isVowel
      "VM", isVowelMod
      "WJ", isWordJoiner ]

/// <summary>The keys of <c>use_positions</c>, whose categories carry or drop a position suffix (src/gen-use-table.py:334-380).</summary>
let private usePositionKeys =
    [ "B"; "F"; "FM"; "H"; "HM"; "HR"; "HVM"; "IS"; "M"; "CM"; "R"; "RK"; "SM"; "SUB"; "V"; "VM" ]

/// <summary>The position suffix each positional category appends, suffix to accepted IPC values.</summary>
let private usePositions : Map<string, (string * string list) list> =
    [ "F", [ "Abv", [ "Top" ]; "Blw", [ "Bottom" ]; "Pst", [ "Right" ] ]
      "M",
      [ "Abv", [ "Top" ]
        "Blw", [ "Bottom"; "Bottom_And_Left"; "Bottom_And_Right" ]
        "Pst", [ "Right" ]
        "Pre", [ "Left"; "Top_And_Bottom_And_Left" ] ]
      "CM", [ "Abv", [ "Top" ]; "Blw", [ "Bottom"; "Overstruck" ] ]
      "V",
      [ "Abv", [ "Top"; "Top_And_Bottom"; "Top_And_Bottom_And_Right"; "Top_And_Right" ]
        "Blw", [ "Bottom"; "Overstruck"; "Bottom_And_Right" ]
        "Pst", [ "Right" ]
        "Pre", [ "Left"; "Top_And_Left"; "Top_And_Left_And_Right"; "Left_And_Right" ] ]
      "VM", [ "Abv", [ "Top" ]; "Blw", [ "Bottom"; "Overstruck" ]; "Pst", [ "Right" ]; "Pre", [ "Left" ] ]
      "SM", [ "Abv", [ "Top" ]; "Blw", [ "Bottom" ] ]
      "FM", [ "Abv", [ "Top" ]; "Blw", [ "Bottom" ]; "Pst", [ "Not_Applicable" ] ] ]
    |> Map.ofList

/// <summary>Builds the generated UseTable.fs body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The body, without the namespace and opens.</returns>
let generate (root: string) : string =
    let categoryValue = categoryValues root

    let isc = Dictionary<uint32, string>()
    readInto isc (Path.Combine(dataDirectory root, "IndicSyllabicCategory.txt")) 1 (fun _ -> true) id
    readInto isc (Path.Combine(sourceDirectory root, "ms-use", "IndicSyllabicCategory-Additional.txt")) 1 (fun _ -> true) syllableModifier

    let ipc = Dictionary<uint32, string>()
    readInto ipc (Path.Combine(dataDirectory root, "IndicPositionalCategory.txt")) 1 (fun _ -> true) id
    readInto ipc (Path.Combine(sourceDirectory root, "ms-use", "IndicPositionalCategory-Additional.txt")) 1 (fun _ -> true) notApplicable

    let arabic = Dictionary<uint32, string>()
    readInto arabic (Path.Combine(dataDirectory root, "ArabicShaping.txt")) 2 (fun _ -> true) (fun value -> "jt_" + value)

    let derived = Dictionary<uint32, string>()
    readInto derived (Path.Combine(dataDirectory root, "DerivedCoreProperties.txt")) 1 (fun value -> value = "Default_Ignorable_Code_Point") id

    let unicodeData = Dictionary<uint32, string>()
    readInto unicodeData (Path.Combine(dataDirectory root, "UnicodeData.txt")) 2 (fun _ -> true) id

    let blocks = Dictionary<uint32, string>()
    readInto blocks (Path.Combine(dataDirectory root, "Blocks.txt")) 1 (fun _ -> true) id

    let scripts = Dictionary<uint32, string>()
    readInto scripts (Path.Combine(dataDirectory root, "Scripts.txt")) 1 (fun _ -> true) id

    let slots = [| isc; ipc; arabic; derived; unicodeData; blocks; scripts |]
    let combined = Dictionary<uint32, string array>()
    for i in 0 .. 6 do
        for pair in slots[i] do
            match combined.ContainsKey pair.Key, i >= 4 with
            | false, true -> ()
            | false, false ->
                combined[pair.Key] <- Array.copy defaults
                combined[pair.Key][i] <- pair.Value
            | true, _ -> combined[pair.Key][i] <- pair.Value

    for u in combined.Keys |> Seq.toArray do
        let script = combined[u][6]
        match Array.contains script disabledScripts with
        | true -> combined.Remove u |> ignore
        | false -> ()

    let categories = Dictionary<uint32, int>()
    for pair in combined do
        let u = pair.Key
        let row = pair.Value
        let uisc =
            match u with
            | value when value >= 0x1CE2u && value <= 0x1CE8u -> "Cantillation_Mark"
            | value when (value >= 0x0F18u && value <= 0x0F19u) || (value >= 0x0F3Eu && value <= 0x0F3Fu) -> "Vowel_Dependent"
            | value when value = 0x1CEDu -> "Tone_Mark"
            | _ -> row[0]
        let uipc =
            match oneOfU32 [ 0x11302u; 0x11303u; 0x114C1u ] u with
            | true -> "Top"
            | false -> row[1]
        let ajt = row[2]
        let udi = row[3].Length > 0
        let ugc = row[4]
        let matching =
            useMapping
            |> List.filter (fun (_, predicate) -> predicate u uisc udi ugc ajt)
            |> List.map fst
        let categoryName =
            match matching with
            | [ single ] -> single
            | _ -> failwith ($"{u:X}: " + String.Join(", ", matching) + " categories match")
        match (uipc = "Not_Applicable" || uipc = "Visual_Order_Left") || u = 0x0F7Fu || oneOf usePositionKeys categoryName with
        | false -> failwith $"{u:X}: {categoryName} has no position"
        | true -> ()
        let finalName =
            match Map.tryFind categoryName usePositions with
            | None -> categoryName
            | Some mapping ->
                let suffixes =
                    mapping
                    |> List.filter (fun (_, positions) -> List.contains uipc positions)
                    |> List.map fst
                match suffixes with
                | [ suffix ] -> categoryName + suffix
                | _ -> failwith $"{u:X}: {categoryName} has no {uipc} suffix"
        categories[u] <-
            match Map.tryFind finalName categoryValue with
            | Some value -> value
            | None -> failwith $"no USE export for {finalName}"

    let values = Array.create 0x110000 0
    for pair in categories do
        values[int pair.Key] <- pair.Value

    let table = Pack.trie values 0

    let indexItems = table.Index |> Array.map (fun value -> string value + "us") |> String.concat "; "
    let valueItems = table.Values |> Array.map (fun value -> string value + "us") |> String.concat "; "
    let blockSize = 1 <<< table.Shift
    let mask = blockSize - 1

    [ "module internal UseTable ="
      $"    let private categoriesShift = {table.Shift}"
      "    let private categoriesIndex : ImmutableArray<uint16> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {indexItems} |])"
      "    let private categoriesValues : ImmutableArray<uint16> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {valueItems} |])"
      "    /// <summary>Reads the Universal Shaping Engine category of a code point, <c>hb_use_get_category</c> (src/hb-ot-shaper-use-table.hh).</summary>"
      "    /// <param name=\"u\">The code point.</param>"
      "    /// <returns>The category byte, or <c>O</c> (0) outside Unicode.</returns>"
      "    let category (u: uint32) : byte ="
      "        match u < 0x110000u with"
      "        | false -> 0uy"
      "        | true ->"
      "            let block = int categoriesIndex[int (u >>> categoriesShift)]"
      $"            byte categoriesValues[block * {blockSize} + int (u &&& {mask}u)]" ]
    |> String.concat "\n"
