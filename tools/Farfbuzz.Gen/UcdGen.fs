module Farfbuzz.Gen.UcdGen

open System
open System.Collections.Generic
open System.IO
open System.Text
open Farfbuzz.Gen

/// <summary>Emits a packed integer accessor.</summary>
/// <param name="name">The accessor name.</param>
/// <param name="table">The packed table.</param>
/// <param name="elementType">The F# element type.</param>
/// <param name="convert">The element conversion.</param>
/// <returns>The binding text.</returns>
let private accessor (name: string) (table: Pack.PackedTable) (elementType: string) (convert: string -> string) =
    let index = table.Index |> Array.map (fun value -> string value + "us") |> String.concat "; "
    let values = table.Values |> Array.map (fun value -> convert (string value)) |> String.concat "; "
    let mask = (1 <<< table.Shift) - 1
    let defaultValue = convert (string table.Default)
    $"""let private {name}Shift = {table.Shift}
let private {name}Index : ImmutableArray<uint16> =
    Sealed.ofSpan (ReadOnlySpan [| {index} |])
let private {name}Values : ImmutableArray<{elementType}> =
    Sealed.ofSpan (ReadOnlySpan [| {values} |])
/// <summary>Reads {name} for a code point.</summary>
/// <param name="u">The code point.</param>
/// <returns>The encoded value, or the default outside Unicode.</returns>
let {name} (u: uint32) : {elementType} =
    match u < 0x110000u with
    | false -> {defaultValue}
    | true ->
        let block = int {name}Index[int (u >>> {name}Shift)]
        {name}Values[block * {1 <<< table.Shift} + int (u &&& {mask}u)]"""

/// <summary>Builds the generated UCD table file body.</summary>
/// <param name="root">The repository root.</param>
/// <param name="description">The UCD description.</param>
/// <param name="records">The records.</param>
/// <returns>The body.</returns>
let generate (root: string) (description: string) (records: UcdXml.CodePointRecord array) =
    let tagValue (text: string) =
        let padded = (text + "    ").Substring(0, 4)
        (uint32 (int padded[0]) <<< 24) ||| (uint32 (int padded[1]) <<< 16) ||| (uint32 (int padded[2]) <<< 8) ||| uint32 (int padded[3])
    let scriptPath = Path.Combine(root, "upstream", "harfbuzz", "src", "hb-script-list.h")
    let scripts = Scripts.scriptTags scriptPath |> List.map (fun (_, tag) -> tag) |> Array.ofList
    let scriptIndex = scripts |> Array.mapi (fun index tag -> tag, index) |> dict
    let gcOrder = [ "Cc"; "Cf"; "Cn"; "Co"; "Cs"; "Ll"; "Lm"; "Lo"; "Lt"; "Lu"; "Mc"; "Me"; "Mn"; "Nd"; "Nl"; "No"; "Pc"; "Pd"; "Pe"; "Pf"; "Pi"; "Po"; "Ps"; "Sc"; "Sk"; "Sm"; "So"; "Zl"; "Zp"; "Zs" ] |> List.mapi (fun index name -> name, index) |> dict
    let gc = records |> Array.map (fun record -> gcOrder[record.Gc])
    let ccc = records |> Array.map (fun record -> record.Ccc)
    let bmg = records |> Array.mapi (fun index record -> match record.Bmg with | ValueSome value -> int value - index | ValueNone -> 0)
    let sc = records |> Array.map (fun record -> scriptIndex[tagValue record.Sc])
    let dmRecords =
        records |> Array.mapi (fun index record ->
            match record.Dt, record.Dm with
            | "can", values when values.Length = 1 || values.Length = 2 -> Some (index, values)
            | _ -> None) |> Array.choose id
    let dm1 = dmRecords |> Array.choose (fun (_, values) -> match values with | [ value ] -> Some value | _ -> None) |> Array.distinct |> Array.sort
    let dm1p0 = dm1 |> Array.filter (fun value -> value >>> 16 = 0u) |> Array.map (fun value -> value &&& 0xFFFFu)
    let dm1p2 = dm1 |> Array.filter (fun value -> value >>> 16 = 2u) |> Array.map (fun value -> value &&& 0xFFFFu)
    let dm1Order = dm1 |> Array.mapi (fun index value -> value, index + 1) |> dict
    let encode11 (a: uint32) (b: uint32) (c: uint32) = ((a &&& 0x7FFu) <<< 21) ||| ((b &&& 0x7Fu) <<< 14) ||| (c &&& 0x3FFFu)
    let encode64 (a: uint32) (b: uint32) (c: uint32) = (uint64 a <<< 42) ||| (uint64 b <<< 21) ||| uint64 c
    let dm2 = dmRecords |> Array.choose (fun (index, values) -> match values with | [ a; b ] -> Some (a, b, if not records[index].CompEx && records[index].Ccc = 0 then uint32 index else 0u) | _ -> None) |> Array.sort
    let filt (a,b,c) = (a &&& 0xFFFFF800u) = 0u && (b &&& 0xFFFFFF80u) = 0x300u && (c &&& 0xFFF0C000u) = 0u
    let dm2u32 = dm2 |> Array.filter (fun (a,b,c) -> filt (a,b,c)) |> Array.map (fun (a,b,c) -> encode11 a b c)
    let dm2u64 = dm2 |> Array.filter (fun (a,b,c) -> not (filt (a,b,c))) |> Array.map (fun (a,b,c) -> encode64 a b c)
    let dm2Order = dm2 |> Array.mapi (fun index (a,b,_) -> (a,b), index + 1 + dm1p0.Length + dm1p2.Length) |> dict
    let dm = records |> Array.map (fun record ->
        match record.Dt, record.Dm with
        | "can", [ value ] -> dm1Order[value]
        | "can", [ a; b ] -> dm2Order[(a,b)]
        | _ -> 0)
    let gcPack = Pack.trie gc 2
    let cccPack = Pack.trie ccc 0
    let bmgPack = Pack.trie bmg 0
    let scPack = Pack.trie sc (scriptIndex[tagValue "Zzzz"])
    let dmPack = Pack.trie dm 0
    let uint16s name values =
        let text = values |> Array.map (fun v -> string v + "us") |> String.concat "; "
        $"let {name} : ImmutableArray<uint16> =\n    Sealed.ofSpan (ReadOnlySpan [| {text} |])"
    let uint32s name (values: uint32 array) =
        let text = values |> Array.map (fun v -> string v + "u") |> String.concat "; "
        $"let {name} : ImmutableArray<uint32> =\n    Sealed.ofSpan (ReadOnlySpan [| {text} |])"
    let uint64s name (values: uint64 array) =
        let text = values |> Array.map (fun v -> string v + "UL") |> String.concat "; "
        $"let {name} : ImmutableArray<uint64> =\n    Sealed.ofSpan (ReadOnlySpan [| {text} |])"
    let body =
        String.Join("\n\n", [
            $"// {description}"
            "module internal UcdTables ="
            "    let scripts : ImmutableArray<Script> = Scripts.all"
            accessor "gc" gcPack "byte" (fun value -> value + "uy") |> fun text -> "    " + text.Replace("\n", "\n    ")
            accessor "ccc" cccPack "byte" (fun value -> value + "uy") |> fun text -> "    " + text.Replace("\n", "\n    ")
            accessor "bmg" bmgPack "int" id |> fun text -> "    " + text.Replace("\n", "\n    ")
            accessor "sc" scPack "byte" (fun value -> value + "uy") |> fun text -> "    " + text.Replace("\n", "\n    ")
            accessor "dm" dmPack "uint16" (fun value -> value + "us") |> fun text -> "    " + text.Replace("\n", "\n    ")
            let indent (text: string) = "    " + text.Replace("\n", "\n    ")
            indent (uint16s "dm1P0" (dm1p0 |> Array.map int))
            indent (uint16s "dm1P2" (dm1p2 |> Array.map int))
            indent (uint32s "dm2U32" dm2u32)
            indent (uint64s "dm2U64" dm2u64) ])
    body + "\n"
