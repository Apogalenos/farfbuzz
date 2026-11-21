module Farfbuzz.Gen.ArabicTable

open System
open System.Collections.Generic
open System.IO
open Farfbuzz.Gen

/// <summary>The pinned Unicode data directory.</summary>
let private dataDirectory (root: string) = Path.Combine(root, "tools", "data", "ucd")

/// <summary>JOINING_CODE (src/gen-arabic-table.py:33-43).</summary>
let private joiningCode =
    [ "JOINING_TYPE_U", 0; "JOINING_TYPE_L", 1; "JOINING_TYPE_R", 2; "JOINING_TYPE_D", 3
      "JOINING_TYPE_C", 3; "JOINING_GROUP_ALAPH", 4; "JOINING_GROUP_DALATH_RISH", 5
      "JOINING_TYPE_T", 6; "JOINING_TYPE_X", 7 ]
    |> Map.ofList

/// <summary>LIGATURES, the subset of ligatures kept (src/gen-arabic-table.py:139-148).</summary>
let private ligatureCodepoints =
    [ 0xF2EEu; 0xFC08u; 0xFC0Eu; 0xFC12u; 0xFC32u; 0xFC3Fu; 0xFC40u; 0xFC41u; 0xFC42u
      0xFC43u; 0xFC44u; 0xFC4Eu; 0xFC5Eu; 0xFC60u; 0xFC61u; 0xFC62u; 0xFC6Au; 0xFC6Du
      0xFC6Fu; 0xFC70u; 0xFC73u; 0xFC75u; 0xFC86u; 0xFC8Fu; 0xFC91u; 0xFC94u; 0xFC9Cu
      0xFC9Du; 0xFC9Eu; 0xFC9Fu; 0xFCA1u; 0xFCA2u; 0xFCA3u; 0xFCA4u; 0xFCA8u; 0xFCAAu
      0xFCACu; 0xFCB0u; 0xFCC9u; 0xFCCAu; 0xFCCBu; 0xFCCCu; 0xFCCDu; 0xFCCEu; 0xFCCFu
      0xFCD0u; 0xFCD1u; 0xFCD2u; 0xFCD3u; 0xFCD5u; 0xFCDAu; 0xFCDBu; 0xFCDCu; 0xFCDDu
      0xFD30u; 0xFD88u; 0xFEF5u; 0xFEF6u; 0xFEF7u; 0xFEF8u; 0xFEF9u; 0xFEFAu; 0xFEFBu
      0xFEFCu; 0xF201u; 0xF211u; 0xF2EEu ]
    |> Set.ofList

/// <summary>The three synthetic PUA lines appended to UnicodeData.txt (src/gen-arabic-table.py:156-160).</summary>
let private puaLines =
    [| "F201;PUA ARABIC LIGATURE LELLAH ISOLATED FORM;Lo;0;AL;<isolated> 0644 0644 0647;;;;N;;;;;"
       "F211;PUA ARABIC LIGATURE LAM WITH MEEM WITH JEEM INITIAL FORM;Lo;0;AL;<initial> 0644 0645 062C;;;;N;;;;;"
       "F2EE;PUA ARABIC LIGATURE SHADDA WITH FATHATAN ISOLATED FORM;Lo;0;AL;<isolated> 0020 064B 0651;;;;N;;;;;" |]

/// <summary>Asserts the first two header lines of each data file (src/gen-arabic-table.py:26-27).</summary>
let private checkHeaders (root: string) =
    let check (name: string) (expected: string list) =
        let path = Path.Combine(dataDirectory root, name)
        let actual = File.ReadAllLines path |> Array.truncate 2 |> Array.toList
        match actual = expected with
        | true -> ()
        | false -> failwith $"unexpected header in {name}: %A{actual}"
    check "ArabicShaping.txt" [ "# ArabicShaping-18.0.0.txt"; "# Date: 2026-08-18, 19:02:41 GMT" ]
    check "Blocks.txt" [ "# Blocks-18.0.0.txt"; "# Date: 2026-07-08" ]

/// <summary>Reads ArabicShaping.txt joining types into a code point map (src/gen-arabic-table.py:69-91).</summary>
/// <param name="root">The repository root.</param>
/// <returns>The code point to joining type map.</returns>
let private readJoining (root: string) : Dictionary<uint32, int> =
    let values = Dictionary<uint32, int>()
    for line in File.ReadAllLines (Path.Combine(dataDirectory root, "ArabicShaping.txt")) do
        match line.Length = 0 || line[0] = '#' with
        | true -> ()
        | false ->
            let fields = line.Split(';') |> Array.map (fun field -> field.Trim())
            match fields.Length < 4 with
            | true -> ()
            | false ->
                let u = Convert.ToUInt32(fields[0], 16)
                let name =
                    match fields[3] = "ALAPH" || fields[3] = "DALATH RISH" with
                    | true -> "JOINING_GROUP_" + fields[3].Replace(' ', '_')
                    | false -> "JOINING_TYPE_" + fields[2]
                values[u] <- joiningCode[name]
    values

/// <summary>One ligature read from UnicodeData: its components, shape (none for mark ligatures) and ligature.</summary>
type private Ligature =
    { Items: uint32 array
      Shape: string option
      Ligature: uint32 }

/// <summary>Reads UnicodeData.txt into the shape map and the ordered ligature list (src/gen-arabic-table.py:150-238).</summary>
/// <param name="root">The repository root.</param>
/// <returns>The shape map and the ligatures in file order.</returns>
let private readShaping (root: string) : struct (Dictionary<uint32, Dictionary<string, uint32>> * ResizeArray<Ligature>) =
    let shapes = Dictionary<uint32, Dictionary<string, uint32>>()
    let ligatures = ResizeArray<Ligature>()
    let lines = Array.append (File.ReadAllLines (Path.Combine(dataDirectory root, "UnicodeData.txt"))) puaLines
    for line in lines do
        let fields = line.Split(';') |> Array.map (fun field -> field.Trim())
        match fields.Length <= 5 with
        | true -> ()
        | false ->
            match fields[5].Length = 0 || fields[5][0] <> '<' with
            | true -> ()
            | false ->
                let parts = fields[5].Split(' ')
                let shape = parts[0].Substring(1, parts[0].Length - 2)
                let items = parts[1..] |> Array.map (fun text -> Convert.ToUInt32(text, 16))
                let c = Convert.ToUInt32(fields[0], 16)
                match shape with
                | "initial" | "medial" | "isolated" | "final" ->
                    match items.Length = 1 with
                    | true ->
                        match shapes.TryGetValue items[0] with
                        | true, byShape -> byShape[shape] <- c
                        | false, _ ->
                            let byShape = Dictionary<string, uint32>()
                            byShape[shape] <- c
                            shapes[items[0]] <- byShape
                    | false ->
                        let items, shapeOption =
                            match items[0] = 0x0020u with
                            | true -> Array.rev items[1..], None
                            | false -> items, Some shape
                        match Set.contains c ligatureCodepoints with
                        | true -> ligatures.Add { Items = items; Shape = shapeOption; Ligature = c }
                        | false -> ()
                | _ -> ()
    struct (shapes, ligatures)

/// <summary>Builds the three ligature tables in Python dict insertion order (src/gen-arabic-table.py:199-236).</summary>
/// <param name="shapes">The shape map.</param>
/// <param name="ligatures">The ligatures in file order.</param>
/// <returns>The two-component, three-component and mark ligature tables.</returns>
let private buildLigatures
        (shapes: Dictionary<uint32, Dictionary<string, uint32>>)
        (ligatures: ResizeArray<Ligature>)
        : struct (Dictionary<uint32, ResizeArray<uint16 * uint16>> * Dictionary<uint32, ResizeArray<uint16 * uint16 * uint16>> * Dictionary<uint32, ResizeArray<uint16 * uint16>>) =
    let ligas2 = Dictionary<uint32, ResizeArray<uint16 * uint16>>()
    let ligas3 = Dictionary<uint32, ResizeArray<uint16 * uint16 * uint16>>()
    let ligasMark = Dictionary<uint32, ResizeArray<uint16 * uint16>>()

    let add2 (dict: Dictionary<uint32, ResizeArray<uint16 * uint16>>) (first: uint32) (entry: uint16 * uint16) =
        match dict.TryGetValue first with
        | true, entries -> entries.Add entry
        | false, _ ->
            let entries = ResizeArray<uint16 * uint16>()
            entries.Add entry
            dict[first] <- entries

    let add3 (dict: Dictionary<uint32, ResizeArray<uint16 * uint16 * uint16>>) (first: uint32) (entry: uint16 * uint16 * uint16) =
        match dict.TryGetValue first with
        | true, entries -> entries.Add entry
        | false, _ ->
            let entries = ResizeArray<uint16 * uint16 * uint16>()
            entries.Add entry
            dict[first] <- entries

    let keyOrder = ResizeArray<string>()
    let keyItems = Dictionary<string, uint32 array>()
    let keyEntries = Dictionary<string, ResizeArray<string option * uint32>>()
    let keyOf (items: uint32 array) = items |> Array.map (fun value -> value.ToString("X4")) |> String.concat ","
    for ligature in ligatures do
        let key = keyOf ligature.Items
        match keyEntries.ContainsKey key with
        | false ->
            keyOrder.Add key
            keyItems[key] <- ligature.Items
            keyEntries[key] <- ResizeArray<string option * uint32>()
        | true -> ()
        keyEntries[key].Add (ligature.Shape, ligature.Ligature)

    for key in keyOrder do
        let items = keyItems[key]
        for (shape, c) in keyEntries[key] do
            match items.Length with
            | 3 ->
                let a, b, d =
                    match shape with
                    | Some "isolated" -> shapes[items[0]]["initial"], shapes[items[1]]["medial"], shapes[items[2]]["final"]
                    | Some "final" -> shapes[items[0]]["medial"], shapes[items[1]]["medial"], shapes[items[2]]["final"]
                    | Some "initial" -> shapes[items[0]]["initial"], shapes[items[1]]["medial"], shapes[items[2]]["medial"]
                    | _ -> failwith $"unexpected 3-component shape for %A{items}"
                add3 ligas3 a (uint16 b, uint16 d, uint16 c)
            | 2 ->
                match shape with
                | None -> add2 ligasMark items[0] (uint16 items[1], uint16 c)
                | Some "isolated" ->
                    let firstGlyph = shapes[items[0]]["initial"]
                    let secondGlyph = shapes[items[1]]["final"]
                    add2 ligas2 firstGlyph (uint16 secondGlyph, uint16 c)
                | Some "final" ->
                    let firstGlyph = shapes[items[0]]["medial"]
                    let secondGlyph = shapes[items[1]]["final"]
                    add2 ligas2 firstGlyph (uint16 secondGlyph, uint16 c)
                | Some "initial" ->
                    let firstGlyph = shapes[items[0]]["initial"]
                    let secondGlyph = shapes[items[1]]["medial"]
                    add2 ligas2 firstGlyph (uint16 secondGlyph, uint16 c)
                | Some _ -> failwith $"unexpected 2-component shape for %A{items}"
            | _ -> failwith $"unexpected ligature component count for %A{items}"

    struct (ligas2, ligas3, ligasMark)

/// <summary>Sorts a two-component table by its first glyph.</summary>
/// <param name="dict">The table.</param>
/// <returns>The sets in sorted first-glyph order.</returns>
let private sortSets2 (dict: Dictionary<uint32, ResizeArray<uint16 * uint16>>) : (uint16 * (uint16 list * uint16) list) list =
    dict.Keys
    |> Seq.sort
    |> Seq.map (fun first ->
        let entries = dict[first] |> Seq.map (fun (glyph, ligature) -> ([ glyph ], ligature)) |> List.ofSeq
        (uint16 first, entries))
    |> List.ofSeq

/// <summary>Sorts a three-component table by its first glyph.</summary>
/// <param name="dict">The table.</param>
/// <returns>The sets in sorted first-glyph order.</returns>
let private sortSets3 (dict: Dictionary<uint32, ResizeArray<uint16 * uint16 * uint16>>) : (uint16 * (uint16 list * uint16) list) list =
    dict.Keys
    |> Seq.sort
    |> Seq.map (fun first ->
        let entries = dict[first] |> Seq.map (fun (firstGlyph, secondGlyph, ligature) -> ([ firstGlyph; secondGlyph ], ligature)) |> List.ofSeq
        (uint16 first, entries))
    |> List.ofSeq

/// <summary>Emits one ligature table as an ImmutableArray&lt;ArabicLigatureSet&gt; literal.</summary>
/// <param name="sets">The sets in sorted first-glyph order.</param>
/// <returns>The F# expression.</returns>
let private emitLigatureSets (sets: (uint16 * (uint16 list * uint16) list) list) : string =
    match sets with
    | [] -> "Sealed.ofSpan (ReadOnlySpan [| |])"
    | _ ->
        let setItems =
            sets
            |> List.map (fun (first, entries) ->
                let entryItems =
                    entries
                    |> List.map (fun (components, ligature) ->
                        let componentItems = components |> List.map (fun glyph -> $"{glyph}us") |> String.concat "; "
                        $"struct (Sealed.ofSpan (ReadOnlySpan [| {componentItems} |]), {ligature}us)")
                    |> String.concat "; "
                $"{{ First = {first}us; Entries = Sealed.ofSpan (ReadOnlySpan [| {entryItems} |]) }}")
            |> String.concat "; "
        $"Sealed.ofSpan (ReadOnlySpan [| {setItems} |])"

/// <summary>Builds the generated ArabicTable.fs body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The body, without the namespace and opens.</returns>
let generate (root: string) : string =
    checkHeaders root

    let joining = readJoining root
    let joiningValues = Array.create 0x110000 7
    for pair in joining do
        joiningValues[int pair.Key] <- pair.Value
    let table = Pack.trie joiningValues 7
    let blockSize = 1 <<< table.Shift
    let mask = blockSize - 1
    let joiningIndexItems = table.Index |> Array.map (fun value -> string value + "us") |> String.concat "; "
    let joiningValueItems = table.Values |> Array.map (fun value -> string value + "uy") |> String.concat "; "

    let struct (shapes, ligatures) = readShaping root
    let minU = shapes.Keys |> Seq.min
    let maxU = shapes.Keys |> Seq.max
    let formNames = [| "initial"; "medial"; "final"; "isolated" |]
    let shapingValues = Array.zeroCreate ((int maxU - int minU + 1) * 4)
    for u in minU .. maxU do
        for form in 0 .. 3 do
            let value =
                match shapes.TryGetValue u with
                | true, byShape ->
                    match byShape.TryGetValue formNames[form] with
                    | true, found -> found
                    | false, _ -> 0u
                | false, _ -> 0u
            shapingValues[(int u - int minU) * 4 + form] <- value
    let shapingItems = shapingValues |> Array.map (fun value -> $"{value}us") |> String.concat "; "

    let struct (ligas2, ligas3, ligasMark) = buildLigatures shapes ligatures

    [ "/// <summary>One Arabic ligature set: a sorted first glyph and its ligature entries (src/hb-ot-shaper-arabic-table.hh).</summary>"
      "type internal ArabicLigatureSet ="
      "    { First: uint16"
      "      Entries: ImmutableArray<struct (ImmutableArray<uint16> * uint16)> }"
      String.Empty
      "module internal ArabicTable ="
      $"    let private joiningShift = {table.Shift}"
      "    let private joiningIndex : ImmutableArray<uint16> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {joiningIndexItems} |])"
      "    let private joiningValues : ImmutableArray<byte> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {joiningValueItems} |])"
      "    /// <summary>Reads the Arabic joining type of a code point, <c>joining_type</c> (src/hb-ot-shaper-arabic-table.hh).</summary>"
      "    /// <param name=\"u\">The code point.</param>"
      "    /// <returns>The joining type byte, <c>X</c> (7) outside Unicode.</returns>"
      "    let joiningType (u: uint32) : byte ="
      "        match u < 0x110000u with"
      "        | false -> 7uy"
      "        | true ->"
      "            let block = int joiningIndex[int (u >>> joiningShift)]"
      $"            joiningValues[block * {blockSize} + int (u &&& {mask}u)]"
      String.Empty
      "    [<Literal>]"
      $"    let ShapingFirst = 0x{int minU:X4}u"
      "    [<Literal>]"
      $"    let ShapingLast = 0x{int maxU:X4}u"
      String.Empty
      "    let private shapingValues : ImmutableArray<uint16> ="
      $"        Sealed.ofSpan (ReadOnlySpan [| {shapingItems} |])"
      "    /// <summary>Reads the presentation form of a code point, <c>shaping_table</c> (src/hb-ot-shaper-arabic-table.hh).</summary>"
      "    /// <param name=\"u\">The code point.</param>"
      "    /// <param name=\"form\">The form, 0 initial, 1 medial, 2 final, 3 isolated.</param>"
      "    /// <returns>The presentation form, or 0 outside the shaping range.</returns>"
      "    let shaping (u: uint32) (form: int) : uint16 ="
      "        match u < ShapingFirst || u > ShapingLast with"
      "        | true -> 0us"
      "        | false -> shapingValues[int (u - ShapingFirst) * 4 + form]"
      String.Empty
      "    /// <summary>The three-component Arabic ligature table, <c>ligature_3_table</c>.</summary>"
      "    let ligatures3 : ImmutableArray<ArabicLigatureSet> ="
      $"        {emitLigatureSets (sortSets3 ligas3)}"
      "    /// <summary>The two-component Arabic ligature table, <c>ligature_table</c>.</summary>"
      "    let ligatures : ImmutableArray<ArabicLigatureSet> ="
      $"        {emitLigatureSets (sortSets2 ligas2)}"
      "    /// <summary>The two-component Arabic mark ligature table, <c>ligature_mark_table</c>.</summary>"
      "    let ligaturesMark : ImmutableArray<ArabicLigatureSet> ="
      $"        {emitLigatureSets (sortSets2 ligasMark)}" ]
    |> String.concat "\n"
