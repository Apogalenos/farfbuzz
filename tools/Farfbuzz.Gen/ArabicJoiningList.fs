module Farfbuzz.Gen.ArabicJoiningList

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

/// <summary>Asserts the first two header lines of each data file (src/gen-arabic-joining-list.py:17).</summary>
let private checkHeaders (root: string) =
    let check (name: string) (expected: string list) =
        let path = Path.Combine(dataDirectory root, name)
        let actual = File.ReadAllLines path |> Array.truncate 2 |> Array.toList
        match actual = expected with
        | true -> ()
        | false -> failwith $"unexpected header in {name}: %A{actual}"
    check "ArabicShaping.txt" [ "# ArabicShaping-18.0.0.txt"; "# Date: 2026-08-18, 19:02:41 GMT" ]
    check "Scripts.txt" [ "# Scripts-18.0.0.txt"; "# Date: 2026-06-29, 15:25:26 GMT" ]

/// <summary>Reads the joining code points of ArabicShaping.txt, skipping <c>T</c> and <c>U</c> (src/gen-arabic-joining-list.py:47-62).</summary>
/// <param name="root">The repository root.</param>
/// <returns>The code points that have a joining type other than transparent or non-joining.</returns>
let private readJoiningCodePoints (root: string) : uint32 list =
    let values = ResizeArray<uint32>()
    for line in File.ReadAllLines (Path.Combine(dataDirectory root, "ArabicShaping.txt")) do
        match line.Length = 0 || line[0] = '#' with
        | true -> ()
        | false ->
            let fields = line.Split(';') |> Array.map (fun field -> field.Trim())
            match fields.Length < 3 with
            | true -> ()
            | false ->
                match fields[2] = "T" || fields[2] = "U" with
                | true -> ()
                | false -> values.Add (Convert.ToUInt32(fields[0], 16))
    values |> List.ofSeq

/// <summary>Reads Scripts.txt into a code point to script map (src/gen-arabic-joining-list.py:21-45).</summary>
/// <param name="root">The repository root.</param>
/// <returns>The code point to script name map.</returns>
let private readScripts (root: string) : Dictionary<uint32, string> =
    let data = Dictionary<uint32, string>()
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
    data

/// <summary>The F# literal name of every script in <c>hb-script-list.h</c>, by uppercase name.</summary>
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

/// <summary>Builds the generated ArabicJoiningList.fs body.</summary>
/// <param name="root">The repository root.</param>
/// <returns>The body, without the namespace and opens.</returns>
let generate (root: string) : string =
    checkHeaders root
    let joining = readJoiningCodePoints root
    let scripts = readScripts root
    let known = knownScripts root
    let literals =
        joining
        |> List.map (fun u ->
            match scripts.TryGetValue u with
            | true, name -> name
            | false, _ -> failwith $"joining code point {u:X4} has no script")
        |> List.filter (fun name -> name <> "Common" && name <> "Inherited")
        |> Set.ofList
        |> Set.toList
        |> List.map (fun name ->
            match Map.tryFind (name.ToUpperInvariant()) known with
            | Some literal -> literal
            | None -> failwith $"no Scripts literal for {name}")
    let cases =
        match literals with
        | [] -> []
        | [ single ] -> [ $"        | Scripts.{single} -> true" ]
        | many ->
            let head = many |> List.take (many.Length - 1) |> List.map (fun literal -> $"        | Scripts.{literal}")
            head @ [ $"        | Scripts.{List.last many} -> true" ]
    [ "module internal ArabicJoiningList ="
      "    /// <summary>Whether a script has data in the Arabic table, <c>has_arabic_joining</c> (src/hb-ot-shaper-arabic-joining-list.hh).</summary>"
      "    /// <param name=\"script\">The script.</param>"
      "    /// <returns>True when the script has Arabic joining data.</returns>"
      "    let hasArabicJoining (script: Script) : bool ="
      "        match script with"
      yield! cases
      "        | _ -> false" ]
    |> String.concat "\n"
