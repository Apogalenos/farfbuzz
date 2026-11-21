module Farfbuzz.Gen.Scripts

open System
open System.Collections.Immutable
open System.IO
open System.Text.RegularExpressions
open Farfbuzz.Gen

/// <summary>A script name and its ISO 15924 tag.</summary>
type private ScriptEntry = { Name: string; Tag: uint32 }

/// <summary>Converts an upstream script name to PascalCase.</summary>
/// <param name="name">The upstream name.</param>
/// <returns>The F# name.</returns>
let private pascalCase (name: string) =
    name.Split('_')
    |> Array.map (fun part ->
        match part.Length with
        | 0 -> String.Empty
        | _ -> string part[0] + part.Substring(1).ToLowerInvariant())
    |> String.concat ""

/// <summary>Reads the script list.</summary>
/// <param name="path">The upstream header.</param>
/// <returns>The entries in file order.</returns>
let private entries (path: string) =
    let pattern = Regex(@"HB_SCRIPT_([A-Z0-9_]+)\s*=\s*HB_TAG\s*\(\s*'(.)'\s*,\s*'(.)'\s*,\s*'(.)'\s*,\s*'(.)'\s*\)")
    File.ReadAllLines path
    |> Array.choose (fun line ->
        let found = pattern.Match line
        match found.Success with
        | false -> None
        | true ->
            let bytes = [| for index in 1 .. 4 -> byte (found.Groups[index + 1].Value[0]) |]
            Some { Name = pascalCase found.Groups[1].Value
                   Tag = (uint32 bytes[0] <<< 24) ||| (uint32 bytes[1] <<< 16) ||| (uint32 bytes[2] <<< 8) ||| uint32 bytes[3] })
    |> Array.toList

/// <summary>Reads a named list of cases from the horizontal-direction switch.</summary>
/// <param name="path">The upstream source.</param>
/// <param name="known">The script names.</param>
/// <returns>The RTL and invalid-direction script names.</returns>
let private directionLists (path: string) (known: Set<string>) =
    let lines = File.ReadAllLines path
    let start = lines |> Array.findIndex (fun line -> line.Contains "hb_script_get_horizontal_direction")
    let casePattern = Regex(@"case HB_SCRIPT_([A-Z0-9_]+):")
    let mutable current = []
    let mutable rtl = []
    let mutable invalid = []
    for line in lines[start .. min (lines.Length - 1) (start + 120)] do
        let found = casePattern.Match line
        match found.Success with
        | true ->
            let name = pascalCase found.Groups[1].Value
            match known.Contains name with
            | true -> current <- name :: current
            | false -> failwith $"Unknown script case {found.Groups[1].Value} in {path}"
        | false -> ()
        match line.Contains "return HB_DIRECTION_RTL", line.Contains "return HB_DIRECTION_INVALID" with
        | true, _ ->
            rtl <- List.rev current
            current <- []
        | false, true ->
            invalid <- List.rev current
            current <- []
        | false, false -> ()
    struct (rtl, invalid)

/// <summary>Returns the script names and tags in file order.</summary>
/// <param name="path">The upstream header.</param>
/// <returns>The names and tags.</returns>
let scriptTags (path: string) = entries path |> List.map (fun entry -> entry.Name, entry.Tag)

/// <summary>Builds the generated script module body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The generated text.</returns>
let generate (root: string) =
    let path = Path.Combine(root, "upstream", "harfbuzz", "src", "hb-script-list.h")
    let common = Path.Combine(root, "upstream", "harfbuzz", "src", "hb-common.cc")
    let scripts = entries path
    match scripts.Length with
    | 179 -> ()
    | count -> failwith $"Unexpected script count in {path}: {count}"
    let names = scripts |> List.map (fun entry -> entry.Name) |> Set.ofList
    let struct (rtl, invalid) = directionLists common names
    let literal (entry: ScriptEntry) =
        $"    [<Literal>]\n    let {entry.Name} = 0x{entry.Tag:X8}u<script>"
    let all = scripts |> List.map (fun entry -> entry.Name)
    let array name values =
        let rows = values |> List.chunkBySize 6 |> List.map (fun row -> "            " + String.Join("; ", row))
        $"    let {name} : ImmutableArray<Script> =\n        Sealed.ofSpan (ReadOnlySpan [| " + String.Join(";\n", rows) + " |])"
    let body =
        String.Join(
            "\n\n",
            [ "    [<Literal>]\n    let Invalid = 0u<script>"
              String.Join("\n", scripts |> List.map literal)
              array "all" all
              array "rtlScripts" rtl
              array "invalidDirectionScripts" invalid ])
    "[<Measure>]\ntype script\n\ntype Script = uint32<script>\n\n"
    + Emit.publicModuleBlock "Scripts" body
