module Farfbuzz.Gen.IndicTable

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Farfbuzz.Gen

/// <summary>The upstream src directory.</summary>
let private sourceDirectory (root: string) = Path.Combine(root, "upstream", "harfbuzz", "src")

/// <summary>The pinned Unicode data directory.</summary>
let private dataDirectory (root: string) = Path.Combine(root, "tools", "data", "ucd")

/// <summary>Code points kept regardless of their block (src/gen-indic-table.py:24).</summary>
let private allowedSingles = [| 0x00A0u; 0x25CCu |]

/// <summary>The blocks whose code points reach the table (src/gen-indic-table.py:25-48).</summary>
let private allowedBlocks =
    [ "Basic Latin"; "Latin-1 Supplement"; "Devanagari"; "Bengali"; "Gurmukhi"; "Gujarati"; "Oriya"
      "Tamil"; "Telugu"; "Kannada"; "Malayalam"; "Myanmar"; "Khmer"; "Vedic Extensions"; "General Punctuation"
      "Superscripts and Subscripts"; "Devanagari Extended"; "Myanmar Extended-B"; "Myanmar Extended-A"
      "Myanmar Extended-C"; "Devanagari Extended-A"; "Bengali Supplement" ]

/// <summary>The category names per shaper, in the order src/gen-indic-table.py:93-145 defines them.</summary>
let private categoryLists =
    [ "indic",
      [ "X"; "C"; "V"; "N"; "H"; "ZWNJ"; "ZWJ"; "M"; "SM"; "A"; "VD"; "PLACEHOLDER"; "DOTTEDCIRCLE"; "RS"
        "MPst"; "Repha"; "Ra"; "CM"; "Symbol"; "CS"; "SMPst" ]
      "khmer", [ "VAbv"; "VBlw"; "VPre"; "VPst"; "Robatic"; "Xgroup"; "Ygroup" ]
      "myanmar",
      [ "VAbv"; "VBlw"; "VPre"; "VPst"; "IV"; "As"; "DB"; "GB"; "MH"; "MR"; "MW"; "MY"; "PT"; "VS"; "ML" ] ]

/// <summary>IndicSyllabicCategory name to shaper category (src/gen-indic-table.py:147-184).</summary>
let private categoryMap =
    [ "Other", "X"; "Avagraha", "Symbol"; "Bindu", "SM"; "Brahmi_Joining_Number", "PLACEHOLDER"
      "Cantillation_Mark", "A"; "Consonant", "C"; "Consonant_Dead", "C"; "Consonant_Final", "CM"
      "Consonant_Head_Letter", "C"; "Consonant_Initial_Postfixed", "C"; "Consonant_Killer", "M"
      "Consonant_Medial", "CM"; "Consonant_Placeholder", "PLACEHOLDER"; "Consonant_Preceding_Repha", "Repha"
      "Consonant_Prefixed", "X"; "Consonant_Subjoined", "CM"; "Consonant_Succeeding_Repha", "CM"
      "Consonant_With_Stacker", "CS"; "Gemination_Mark", "SM"; "Invisible_Stacker", "H"; "Joiner", "ZWJ"
      "Modifying_Letter", "X"; "Non_Joiner", "ZWNJ"; "Nukta", "N"; "Number", "PLACEHOLDER"
      "Number_Joiner", "PLACEHOLDER"; "Pure_Killer", "M"; "Register_Shifter", "RS"; "Syllable_Modifier", "SM"
      "Tone_Letter", "X"; "Tone_Mark", "N"; "Virama", "H"; "Visarga", "SM"; "Vowel", "V"
      "Vowel_Dependent", "M"; "Vowel_Independent", "V" ]
    |> Map.ofList

/// <summary>IndicPositionalCategory name to syllable position (src/gen-indic-table.py:185-205).</summary>
let private positionMap =
    [ "Not_Applicable", "END"; "Left", "PRE_C"; "Top", "ABOVE_C"; "Bottom", "BELOW_C"; "Right", "POST_C"
      "Bottom_And_Right", "POST_C"; "Left_And_Right", "POST_C"; "Top_And_Bottom", "BELOW_C"
      "Top_And_Bottom_And_Left", "BELOW_C"; "Top_And_Bottom_And_Right", "POST_C"; "Top_And_Left", "ABOVE_C"
      "Top_And_Left_And_Right", "POST_C"; "Top_And_Right", "POST_C"; "Overstruck", "AFTER_MAIN"
      "Visual_order_left", "PRE_M" ]
    |> Map.ofList

/// <summary>Code point to category overrides (src/gen-indic-table.py:207-385).</summary>
let private categoryOverrides =
    [ 0xFE00u, "VS"; 0xFE01u, "VS"; 0xFE02u, "VS"; 0xFE03u, "VS"; 0xFE04u, "VS"; 0xFE05u, "VS"
      0xFE06u, "VS"; 0xFE07u, "VS"; 0xFE08u, "VS"; 0xFE09u, "VS"; 0xFE0Au, "VS"; 0xFE0Bu, "VS"
      0xFE0Cu, "VS"; 0xFE0Du, "VS"; 0xFE0Eu, "VS"; 0xFE0Fu, "VS"
      0x2015u, "PLACEHOLDER"; 0x2022u, "PLACEHOLDER"; 0x25FBu, "PLACEHOLDER"; 0x25FCu, "PLACEHOLDER"
      0x25FDu, "PLACEHOLDER"; 0x25FEu, "PLACEHOLDER"
      0x0930u, "Ra"; 0x09B0u, "Ra"; 0x09F0u, "Ra"; 0x0A30u, "Ra"; 0x0AB0u, "Ra"; 0x0B30u, "Ra"
      0x0BB0u, "Ra"; 0x0C30u, "Ra"; 0x0CB0u, "Ra"; 0x0D30u, "Ra"
      0x0953u, "SM"; 0x0954u, "SM"
      0x0A40u, "MPst"
      0x0A72u, "C"; 0x0A73u, "C"
      0x1CE2u, "A"; 0x1CE3u, "A"; 0x1CE4u, "A"; 0x1CE5u, "A"; 0x1CE6u, "A"; 0x1CE7u, "A"; 0x1CE8u, "A"
      0x1CEDu, "A"
      0xA8F2u, "Symbol"; 0xA8F3u, "Symbol"; 0xA8F4u, "Symbol"; 0xA8F5u, "Symbol"; 0xA8F6u, "Symbol"
      0xA8F7u, "Symbol"; 0x1CE9u, "Symbol"; 0x1CEAu, "Symbol"; 0x1CEBu, "Symbol"; 0x1CECu, "Symbol"
      0x1CEEu, "Symbol"; 0x1CEFu, "Symbol"; 0x1CF0u, "Symbol"; 0x1CF1u, "Symbol"
      0x0A51u, "M"
      0x11301u, "SM"; 0x11302u, "SM"; 0x11303u, "SM"; 0x1133Bu, "N"; 0x1133Cu, "N"
      0x0AFBu, "N"; 0x0B55u, "N"
      0x09FCu, "PLACEHOLDER"; 0x0C80u, "PLACEHOLDER"; 0x0D04u, "PLACEHOLDER"
      0x25CCu, "DOTTEDCIRCLE"
      0x179Au, "Ra"
      0x17CCu, "Robatic"; 0x17C9u, "Robatic"; 0x17CAu, "Robatic"
      0x17C6u, "Xgroup"; 0x17CBu, "Xgroup"; 0x17CDu, "Xgroup"; 0x17CEu, "Xgroup"; 0x17CFu, "Xgroup"
      0x17D0u, "Xgroup"; 0x17D1u, "Xgroup"
      0x17C7u, "Ygroup"; 0x17C8u, "Ygroup"; 0x17DDu, "Ygroup"; 0x17D3u, "Ygroup"
      0x17D9u, "PLACEHOLDER"
      0x104Eu, "C"
      0x1004u, "Ra"; 0x101Bu, "Ra"; 0x105Au, "Ra"
      0x1032u, "A"; 0x1036u, "A"
      0x103Au, "As"
      0x103Eu, "MH"; 0x1060u, "ML"; 0x103Cu, "MR"; 0x103Du, "MW"; 0x1082u, "MW"; 0x103Bu, "MY"
      0x105Eu, "MY"; 0x105Fu, "MY"
      0x1063u, "PT"; 0x1064u, "PT"; 0x1069u, "PT"; 0x106Au, "PT"; 0x106Bu, "PT"; 0x106Cu, "PT"
      0x106Du, "PT"; 0xAA7Bu, "PT"
      0x1038u, "SM"; 0x1087u, "SM"; 0x1088u, "SM"; 0x1089u, "SM"; 0x108Au, "SM"; 0x108Bu, "SM"
      0x108Cu, "SM"; 0x108Du, "SM"; 0x108Fu, "SM"; 0x109Au, "SM"; 0x109Bu, "SM"; 0x109Cu, "SM"
      0x104Au, "PLACEHOLDER" ]

/// <summary>Code point to position overrides (src/gen-indic-table.py:386-391).</summary>
let private positionOverrides = [ 0x0A51u, "BELOW_C"; 0x0B01u, "BEFORE_SUB" ]

/// <summary>Asserts the first two header lines of each data file match the generated table.</summary>
/// <param name="root">The repository root.</param>
let private checkHeaders (root: string) =
    let check (name: string) (expected: string list) =
        let path = Path.Combine(dataDirectory root, name)
        let actual = File.ReadAllLines path |> Array.truncate 2 |> Array.toList
        match actual = expected with
        | true -> ()
        | false -> failwith $"unexpected header in {name}: %A{actual}"
    check "IndicSyllabicCategory.txt" [ "# IndicSyllabicCategory-18.0.0.txt"; "# Date: 2026-07-02, 18:58:37 GMT" ]
    check "IndicPositionalCategory.txt" [ "# IndicPositionalCategory-18.0.0.txt"; "# Date: 2026-06-24, 13:39:21 GMT" ]
    check "Blocks.txt" [ "# Blocks-18.0.0.txt"; "# Date: 2026-07-08" ]

/// <summary>Reads a Unicode data file into a code point map (src/gen-indic-table.py:55-76).</summary>
/// <param name="path">The file path.</param>
/// <returns>The code point to value map.</returns>
let private readTable (path: string) : Dictionary<uint32, string> =
    let data = Dictionary<uint32, string>()
    for line in File.ReadAllLines path do
        let comment = line.IndexOf '#'
        let line = if comment >= 0 then line.Substring(0, comment) else line
        let fields = line.Split(';') |> Array.map (fun field -> field.Trim())
        match fields.Length with
        | length when length < 2 -> ()
        | _ ->
            let range = fields[0].Split([| ".." |], StringSplitOptions.None)
            let start = Convert.ToInt32(range[0], 16)
            let stop = if range.Length = 1 then start else Convert.ToInt32(range[1], 16)
            let value = fields[1]
            for u in start .. stop do
                data[uint32 u] <- value
    data

/// <summary>Resolves the category names to their machine export values, first shaper wins (src/gen-indic-table.py:525-538).</summary>
/// <param name="root">The repository root.</param>
/// <returns>The category name to byte value map.</returns>
let private categoryValues (root: string) : Map<string, int> =
    let exports name =
        let path = Path.Combine(sourceDirectory root, $"hb-ot-shaper-{name}-machine.hh")
        match RagelFlat.parse path name with
        | Ok flat -> flat.Exports
        | Error error -> failwith error
    let value = Dictionary<string, int>()
    for shaper, names in categoryLists do
        let byName = exports shaper |> Map.ofList
        for name in names do
            let v = byName[name]
            match value.TryGetValue name with
            | true, existing when existing <> v -> failwith $"category {name} differs between shapers: {existing} vs {v}"
            | true, _ -> ()
            | false, _ -> value[name] <- v
    value |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq

/// <summary>Reads the ot_position_t enum in source order (src/hb-ot-shaper-indic.hh:36-59).</summary>
/// <param name="root">The repository root.</param>
/// <returns>The position name and value pairs.</returns>
let private positions (root: string) : (string * int) list =
    let path = Path.Combine(sourceDirectory root, "hb-ot-shaper-indic.hh")
    let text = File.ReadAllText path
    let start = text.IndexOf "enum ot_position_t"
    let openBrace = text.IndexOf('{', start)
    let closeBrace = text.IndexOf('}', openBrace)
    let body = text.Substring(openBrace + 1, closeBrace - openBrace - 1)
    Regex.Matches(body, @"POS_(\w+)\s*=\s*(\d+)")
    |> Seq.map (fun found -> found.Groups[1].Value, int found.Groups[2].Value)
    |> Seq.toList

/// <summary>Converts an upstream screaming-snake identifier to PascalCase.</summary>
/// <param name="name">The identifier.</param>
/// <returns>The F# name.</returns>
let private pascalCase (name: string) =
    name.Split('_')
    |> Array.map (fun part ->
        match part.Length with
        | 0 -> String.Empty
        | _ -> string (Char.ToUpperInvariant part[0]) + part.Substring(1).ToLowerInvariant())
    |> String.concat ""

/// <summary>The matra position for a pre-base matra (src/gen-indic-table.py:393-394).</summary>
let private matraPosLeft (_: uint32) (_: string) = "PRE_M"

/// <summary>The matra position for a post-base matra (src/gen-indic-table.py:395-405).</summary>
let private matraPosRight (u: uint32) (block: string) =
    if block = "Devanagari" then "AFTER_SUB"
    elif block = "Bengali" then "AFTER_POST"
    elif block = "Gurmukhi" then "AFTER_POST"
    elif block = "Gujarati" then "AFTER_POST"
    elif block = "Oriya" then "AFTER_POST"
    elif block = "Tamil" then "AFTER_POST"
    elif block = "Telugu" then (if u <= 0x0C42u then "BEFORE_SUB" else "AFTER_SUB")
    elif block = "Kannada" then (if u < 0x0CC3u || u > 0x0CD6u then "BEFORE_SUB" else "AFTER_SUB")
    elif block = "Malayalam" then "AFTER_POST"
    else "AFTER_SUB"

/// <summary>The matra position for an above-base matra (src/gen-indic-table.py:406-415).</summary>
let private matraPosTop (_: uint32) (block: string) =
    if block = "Devanagari" then "AFTER_SUB"
    elif block = "Gurmukhi" then "AFTER_POST"
    elif block = "Gujarati" then "AFTER_SUB"
    elif block = "Oriya" then "AFTER_MAIN"
    elif block = "Tamil" then "AFTER_SUB"
    elif block = "Telugu" then "BEFORE_SUB"
    elif block = "Kannada" then "BEFORE_SUB"
    else "AFTER_SUB"

/// <summary>The matra position for a below-base matra (src/gen-indic-table.py:416-426).</summary>
let private matraPosBottom (_: uint32) (block: string) =
    if block = "Devanagari" then "AFTER_SUB"
    elif block = "Bengali" then "AFTER_SUB"
    elif block = "Gurmukhi" then "AFTER_POST"
    elif block = "Gujarati" then "AFTER_POST"
    elif block = "Oriya" then "AFTER_SUB"
    elif block = "Tamil" then "AFTER_POST"
    elif block = "Telugu" then "BEFORE_SUB"
    elif block = "Kannada" then "BEFORE_SUB"
    elif block = "Malayalam" then "AFTER_POST"
    else "AFTER_SUB"

/// <summary>Repositions a matra by its positional category (src/gen-indic-table.py:427-432).</summary>
let private matraPosition (u: uint32) (pos: string) (block: string) =
    if pos = "PRE_C" then matraPosLeft u block
    elif pos = "POST_C" then matraPosRight u block
    elif pos = "ABOVE_C" then matraPosTop u block
    elif pos = "BELOW_C" then matraPosBottom u block
    else failwith $"unexpected matra position {pos}"

/// <summary>Maps a positional category to the Khmer/Myanmar matra category (src/gen-indic-table.py:434-439).</summary>
let private positionToCategory (pos: string) =
    if pos = "PRE_C" then "VPre"
    elif pos = "ABOVE_C" then "VAbv"
    elif pos = "BELOW_C" then "VBlw"
    elif pos = "POST_C" then "VPst"
    else failwith $"unexpected matra position {pos}"

/// <summary>Builds the generated IndicTable.fs body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The body, without the namespace and opens.</returns>
let generate (root: string) : string =
    checkHeaders root
    let isc = readTable (Path.Combine(dataDirectory root, "IndicSyllabicCategory.txt"))
    let ipc = readTable (Path.Combine(dataDirectory root, "IndicPositionalCategory.txt"))
    let blocks = readTable (Path.Combine(dataDirectory root, "Blocks.txt"))

    let combined = Dictionary<uint32, string array>()
    let addFile (i: int) (data: Dictionary<uint32, string>) =
        for pair in data do
            match i = 2 && not (combined.ContainsKey pair.Key) with
            | true -> ()
            | false ->
                match combined.ContainsKey pair.Key with
                | false -> combined[pair.Key] <- [| "Other"; "Not_Applicable"; "No_Block" |]
                | true -> ()
                combined[pair.Key][i] <- pair.Value
    addFile 0 isc
    addFile 1 ipc
    addFile 2 blocks

    let indicData = Dictionary<uint32, string * string * string>()
    for pair in combined do
        match allowedSingles |> Array.contains pair.Key || allowedBlocks |> List.contains pair.Value[2] with
        | false -> ()
        | true ->
            let iscName = pair.Value[0]
            let ipcName = pair.Value[1]
            let mutable cat = categoryMap[iscName]
            match cat = "SM" && ipcName = "Not_Applicable" with
            | true -> cat <- "SMPst"
            | false -> ()
            indicData[pair.Key] <- (cat, positionMap[ipcName], pair.Value[2])

    for (u, newCat) in categoryOverrides do
        let (_, pos, _) =
            match indicData.TryGetValue u with
            | true, existing -> existing
            | false, _ -> ("X", "END", "No_Block")
        indicData[u] <- (newCat, pos, blocks[u])

    let positionedCategories = [| "CM"; "SM"; "RS"; "H"; "M"; "MPst" |]
    for u in indicData.Keys |> Seq.toArray do
        let (cat, _, block) = indicData[u]
        match positionedCategories |> Array.contains cat with
        | true -> ()
        | false -> indicData[u] <- (cat, "END", block)

    let consonantCategories = [| "C"; "CS"; "Ra"; "CM"; "V"; "PLACEHOLDER"; "DOTTEDCIRCLE" |]
    let matraCategories = [| "M"; "MPst" |]
    let smvdCategories = [| "SM"; "SMPst"; "VD"; "A"; "Symbol" |]
    for u in indicData.Keys |> Seq.toArray do
        let (cat, pos, block) = indicData[u]
        if consonantCategories |> Array.contains cat then
            indicData[u] <- (cat, "BASE_C", block)
        elif matraCategories |> Array.contains cat then
            if block.StartsWith "Khmer" || block.StartsWith "Myanmar" then
                indicData[u] <- (positionToCategory pos, pos, block)
            else
                indicData[u] <- (cat, matraPosition u pos block, block)
        elif smvdCategories |> Array.contains cat then
            indicData[u] <- (cat, "SMVD", block)

    for (u, newPos) in positionOverrides do
        let (cat, _, _) =
            match indicData.TryGetValue u with
            | true, existing -> existing
            | false, _ -> ("X", "END", "No_Block")
        indicData[u] <- (cat, newPos, blocks[u])

    let categoryValue = categoryValues root
    let positionList = positions root
    let positionValue = positionList |> Map.ofList
    let defaultValue = categoryValue["X"] ||| (positionValue["END"] <<< 8)

    let values = Array.create 0x110000 defaultValue
    for pair in indicData do
        let (cat, pos, _) = pair.Value
        values[int pair.Key] <- categoryValue[cat] ||| (positionValue[pos] <<< 8)

    let table = Pack.trie values defaultValue

    let positionNames = positionList |> List.map (fun (name, value) -> "Pos" + pascalCase name, value)
    let unionCases = positionNames |> List.map fst |> String.concat "\n    | "
    let ofByteCases =
        positionNames
        |> List.map (fun (name, value) -> $"        | {value}uy -> {name}")
        |> String.concat "\n"
    let literals =
        positionNames
        |> List.map (fun (name, value) -> $"    [<Literal>]\n    let {name} = {value}uy")
        |> String.concat "\n"

    let indexItems = table.Index |> Array.map (fun value -> string value + "us") |> String.concat "; "
    let valueItems = table.Values |> Array.map (fun value -> string value + "us") |> String.concat "; "
    let blockSize = 1 <<< table.Shift
    let mask = blockSize - 1

    [ "/// <summary>The visual position of a glyph in an Indic, Khmer or Myanmar syllable, the values of <c>ot_position_t</c> (src/hb-ot-shaper-indic.hh:36-59).</summary>"
      "[<Struct>]"
      "[<NoEquality; NoComparison>]"
      "type internal SyllablePosition ="
      "    | " + unionCases
      String.Empty
      "module internal SyllablePosition ="
      "    /// <summary>Maps a position byte to its <see cref=\"SyllablePosition\"/>.</summary>"
      "    /// <param name=\"value\">The position byte.</param>"
      "    /// <returns>The position.</returns>"
      "    let ofByte (value: byte) : SyllablePosition ="
      "        match value with"
      ofByteCases
      "        | _ -> failwith \"invalid syllable position\""
      String.Empty
      "module internal IndicPosition ="
      literals
      String.Empty
      "module internal IndicTable ="
      $"    let private categoriesShift = {table.Shift}"
      "    let private categoriesIndex : ImmutableArray<uint16> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {indexItems} |])"
      "    let private categoriesValues : ImmutableArray<uint16> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {valueItems} |])"
      "    /// <summary>Reads the packed Indic category and position of a code point, <c>hb_indic_get_categories</c>.</summary>"
      "    /// <param name=\"u\">The code point.</param>"
      "    /// <returns>The category in the low byte and the position in the high byte, or the default outside Unicode.</returns>"
      "    let categories (u: uint32) : uint16 ="
      "        match u < 0x110000u with"
      $"        | false -> {defaultValue}us"
      "        | true ->"
      "            let block = int categoriesIndex[int (u >>> categoriesShift)]"
      $"            categoriesValues[block * {blockSize} + int (u &&& {mask}u)]"
      "    /// <summary>Reads the Indic syllable category of a code point.</summary>"
      "    /// <param name=\"u\">The code point.</param>"
      "    /// <returns>The category byte.</returns>"
      "    let category (u: uint32) : byte = byte (categories u)"
      "    /// <summary>Reads the Indic syllable position of a code point.</summary>"
      "    /// <param name=\"u\">The code point.</param>"
      "    /// <returns>The position byte.</returns>"
      "    let position (u: uint32) : byte = byte ((categories u) >>> 8)" ]
    |> String.concat "\n"
