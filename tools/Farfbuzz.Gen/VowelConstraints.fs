module Farfbuzz.Gen.VowelConstraints

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Farfbuzz.Gen

/// <summary>The upstream src directory.</summary>
let private sourceDirectory (root: string) = Path.Combine(root, "upstream", "harfbuzz", "src")

/// <summary>The pinned Unicode data directory.</summary>
let private dataDirectory (root: string) = Path.Combine(root, "tools", "data", "ucd")

/// <summary>Converts an upstream screaming-snake identifier to the F# literal name.</summary>
/// <param name="name">The identifier.</param>
/// <returns>The PascalCase name.</returns>
let private pascalCase (name: string) =
    name.Split('_')
    |> Array.map (fun part ->
        match part.Length with
        | 0 -> String.Empty
        | _ -> string part[0] + part.Substring(1).ToLowerInvariant())
    |> String.concat ""

/// <summary>The F# literal name of every script in hb-script-list.h, by uppercase name.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The uppercase script name to literal name map.</returns>
let private knownScripts (root: string) : Map<string, string> =
    let path = Path.Combine(sourceDirectory root, "hb-script-list.h")
    let pattern = Regex(@"HB_SCRIPT_([A-Z0-9_]+)\s*=")
    File.ReadAllLines path
    |> Array.choose (fun line ->
        let found = pattern.Match line
        match found.Success with
        | false -> None
        | true -> Some (found.Groups[1].Value, pascalCase found.Groups[1].Value))
    |> Map.ofArray

/// <summary>Reads Scripts.txt into a code point to script map and each script's first code point.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The code point map and the first-code-point map.</returns>
let private readScripts (root: string) : struct (Dictionary<uint32, string> * Dictionary<string, int>) =
    let data = Dictionary<uint32, string>()
    let order = Dictionary<string, int>()
    for line in File.ReadAllLines (Path.Combine(dataDirectory root, "Scripts.txt")) do
        let comment = line.IndexOf '#'
        let line = if comment >= 0 then line.Substring(0, comment) else line
        let fields = line.Split(';') |> Array.map (fun field -> field.Trim())
        match fields.Length < 2 with
        | true -> ()
        | false ->
            let range = fields[0].Split([| ".." |], StringSplitOptions.None)
            let start = Convert.ToInt32(range[0], 16)
            let stop = if range.Length = 1 then start else Convert.ToInt32(range[1], 16)
            for u in start .. stop do
                data[uint32 u] <- fields[1]
            match order.ContainsKey fields[1] with
            | true -> ()
            | false -> order[fields[1]] <- start
    struct (data, order)

/// <summary>Reads the constraints header and the prohibited sequences.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The header lines and the constraints.</returns>
let private readConstraints (root: string) : struct (string list * uint32 list list) =
    let path = Path.Combine(sourceDirectory root, "ms-use", "IndicShapingInvalidCluster.txt")
    let header = ResizeArray<string>()
    let constraints = ResizeArray<uint32 list>()
    let mutable inHeader = true
    for line in File.ReadAllLines path do
        let stripped = line.Trim()
        match inHeader with
        | true ->
            match stripped = "#" with
            | true -> inHeader <- false
            | false -> header.Add stripped
        | false ->
            let comment = line.IndexOf '#'
            let line = if comment >= 0 then line.Substring(0, comment) else line
            let semicolon = line.IndexOf ';'
            let line = if semicolon >= 0 then line.Substring(0, semicolon) else line
            let codePoints =
                line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun cp -> Convert.ToUInt32(cp, 16))
                |> Array.toList
            match codePoints.IsEmpty with
            | true -> ()
            | false -> constraints.Add codePoints
    struct (List.ofSeq header, List.ofSeq constraints)

/// <summary>Asserts the pinned header lines of the two inputs.</summary>
/// <param name="root">The repository root.</param>
/// <param name="constraintsHeader">The constraints header lines.</param>
let private checkHeaders (root: string) (constraintsHeader: string list) =
    let scriptsPath = Path.Combine(dataDirectory root, "Scripts.txt")
    let scriptsActual = File.ReadAllLines scriptsPath |> Array.truncate 2 |> Array.toList
    match scriptsActual with
    | [ "# Scripts-18.0.0.txt"; "# Date: 2026-06-29, 15:25:26 GMT" ] -> ()
    | other -> failwith $"unexpected header in Scripts.txt: %A{other}"
    match constraintsHeader with
    | [ "# IndicShapingInvalidCluster.txt"; "# Date: 2015-03-12, 21:17:00 GMT [AG]"; "# Date: 2019-11-08, 23:22:00 GMT [AG]" ] -> ()
    | other -> failwith $"unexpected header in IndicShapingInvalidCluster.txt: %A{other}"

/// <summary>Drops every sequence that has a shorter proper prefix in the set (src/gen-vowel-constraints.py:55-86).</summary>
/// <param name="sequences">The prohibited sequences of one script.</param>
/// <returns>The minimal sequences.</returns>
let private minimal (sequences: uint32 list list) : uint32 list list =
    let deduped = sequences |> Set.ofList |> Set.toList
    deduped
    |> List.filter (fun s ->
        not (deduped |> List.exists (fun t -> t.Length < s.Length && List.forall2 (=) t (List.truncate t.Length s))))

/// <summary>Builds the generated VowelConstraints.fs body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The body, without the namespace and opens.</returns>
let generate (root: string) : string =
    let struct (scripts, order) = readScripts root
    let struct (constraintsHeader, constraints) = readConstraints root
    checkHeaders root constraintsHeader
    let known = knownScripts root
    let byScript = Dictionary<string, uint32 list list>()
    for prohibited in constraints do
        match prohibited.Length with
        | 2 | 3 -> ()
        | length -> failwith $"prohibited sequence length {length} is not 2 or 3: %A{prohibited}"
        match scripts.TryGetValue (List.head prohibited) with
        | false, _ -> failwith $"prohibited code point {List.head prohibited:X4} has no script"
        | true, name ->
            match byScript.ContainsKey name with
            | true -> byScript[name] <- prohibited :: byScript[name]
            | false -> byScript[name] <- [ prohibited ]
    let tripleLines (name: string) =
        let triples =
            minimal byScript[name]
            |> List.map (fun sequence ->
                match sequence with
                | [ a; b ] -> struct (a, b, 0u)
                | [ a; b; c ] -> struct (a, b, c)
                | other -> failwith $"impossible minimal sequence: %A{other}")
            |> List.sortBy (fun struct (a, b, c) -> a, b, c)
            |> List.map (fun struct (a, b, c) ->
                match c = 0u with
                | true -> $"struct (0x{a:X4}u, 0x{b:X4}u, 0u)"
                | false -> $"struct (0x{a:X4}u, 0x{b:X4}u, 0x{c:X4}u)")
        let rows =
            triples
            |> List.chunkBySize 6
            |> List.map (fun row -> "                " + String.concat "; " row)
        match triples.IsEmpty with
        | true -> [ "        | _ -> ImmutableArray.Empty" ]
        | false -> [ "            Sealed.ofSpan (ReadOnlySpan [| " + String.Join(";\n", rows) + " |])" ]
    let scriptNames = byScript.Keys |> Seq.toList |> List.sortBy (fun name -> order[name])
    let cases =
        scriptNames
        |> List.collect (fun name ->
            let literal =
                match Map.tryFind (name.ToUpperInvariant()) known with
                | Some found -> found
                | None -> failwith $"no Scripts literal for {name}"
            $"        | Scripts.{literal} ->" :: tripleLines name)
    [ "module internal VowelConstraintTable ="
      "    /// <summary>The minimal prohibited vowel sequences of a script, <c>_hb_preprocess_text_vowel_constraints</c>'s table (src/hb-ot-shaper-vowel-constraints.cc).</summary>"
      "    /// <param name=\"script\">The script.</param>"
      "    /// <returns>The (first, second, third) sequences; a zero third marks a two-code-point sequence.</returns>"
      "    let forScript (script: Script) : ImmutableArray<struct (uint32 * uint32 * uint32)> ="
      "        match script with"
      yield! cases
      "        | _ -> ImmutableArray.Empty" ]
    |> String.concat "\n"
