module Farfbuzz.Gen.UpstreamSources

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

/// <summary>Walks up from the generator binary to the repository root.</summary>
/// <returns>The directory holding <c>Farfbuzz.slnx</c>.</returns>
[<TailCall>]
let rec private findRoot (directory: DirectoryInfo | null) : string =
    match directory with
    | null -> failwith "No Farfbuzz.slnx above the generator binary."
    | candidate when File.Exists(Path.Combine(candidate.FullName, "Farfbuzz.slnx")) -> candidate.FullName
    | candidate -> findRoot candidate.Parent

/// <summary>The repository root.</summary>
/// <returns>The root path.</returns>
let root () =
    findRoot (DirectoryInfo AppContext.BaseDirectory)

/// <summary>The pinned upstream checkout.</summary>
/// <returns>The path of <c>upstream/harfbuzz</c>.</returns>
let upstream () =
    Path.Combine(root (), "upstream", "harfbuzz")

/// <summary>Extracts <c>HB_STR("...")</c> strings in order.</summary>
/// <param name="path">The source file.</param>
/// <returns>The strings.</returns>
let hbStrList (path: string) : string list =
    File.ReadAllLines path
    |> Array.choose (fun line ->
        let found = Regex.Match(line, "HB_STR\\(\"(.*)\"\\)")
        match found.Success with
        | true -> Some (found.Groups[1].Value)
        | false -> None)
    |> Array.toList

/// <summary>Extracts the numbers of one C array in source order.</summary>
/// <param name="path">The source file.</param>
/// <param name="arrayName">The array name.</param>
/// <returns>The numbers.</returns>
let cArrayNumbers (path: string) (arrayName: string) : uint32 list =
    let lines = File.ReadAllLines path
    let start =
        lines
        |> Array.tryFindIndex (fun line ->
            line.Contains arrayName && line.Contains "[]" && line.Contains "=")
    match start with
    | None -> failwith $"Missing C array {arrayName} in {path}"
    | Some first ->
        lines[first..]
        |> Array.takeWhile (fun line -> not (line.Contains "};"))
        |> Array.collect (fun line -> Regex.Matches(line, @"\b(?:0x[0-9A-Fa-f]+|\d+)\b") |> Seq.map (fun m -> m.Value) |> Seq.toArray)
        |> Array.map (fun text ->
            match text.StartsWith "0x" with
            | true -> Convert.ToUInt32(text.Substring 2, 16)
            | false -> UInt32.Parse(text, Globalization.CultureInfo.InvariantCulture))
        |> Array.toList

/// <summary>Extracts a C array of two-number records as pairs.</summary>
/// <param name="path">The source file.</param>
/// <param name="arrayName">The array name.</param>
/// <returns>The pairs.</returns>
let cArrayPairs (path: string) (arrayName: string) : struct (uint32 * uint32) list =
    let numbers = cArrayNumbers path arrayName
    match numbers.Length % 2 with
    | 0 -> numbers |> List.chunkBySize 2 |> List.map (function | [ first; second ] -> struct (first, second) | _ -> failwith $"Odd C array {arrayName} in {path}")
    | _ -> failwith $"Odd C array {arrayName} in {path}"

/// <summary>Extracts the MacRoman mapping pairs in source order.</summary>
/// <param name="path">The source file.</param>
/// <returns>The Unicode and MacRoman byte pairs.</returns>
let macRomanPairs (path: string) : struct (uint16 * byte) list =
    let lines = File.ReadAllLines path
    let start = lines |> Array.tryFindIndex (fun line -> line.Contains "mapping[] =")
    match start with
    | None -> failwith $"Missing MacRoman mapping in {path}"
    | Some first ->
        lines[first..]
        |> Array.takeWhile (fun line -> not (line.Contains "};"))
        |> Array.collect (fun line ->
            Regex.Matches(line, @"\{\s*(0x[0-9A-Fa-f]+)\s*,\s*(0x[0-9A-Fa-f]+)\s*\}")
            |> Seq.map (fun m -> struct (uint16 (Convert.ToUInt32(m.Groups[1].Value, 16)), byte (Convert.ToUInt32(m.Groups[2].Value, 16))))
            |> Seq.toArray)
        |> Array.sortBy (fun struct (unicode, _) -> unicode)
        |> Array.toList

/// <summary>Extracts an Arabic PUA map, later lines overriding earlier ones.</summary>
/// <param name="path">The tab-separated source file.</param>
/// <returns>The Unicode and PUA pairs sorted by Unicode.</returns>
let arabicPua (path: string) : struct (uint16 * uint16) list =
    let data = Dictionary<uint16, uint16>()
    File.ReadAllLines path
    |> Array.filter (fun line -> line.Length > 0 && not (line.StartsWith "#"))
    |> Array.iter (fun line ->
        let fields = line.Split('\t')
        match fields.Length with
        | length when length >= 2 ->
            data[uint16 (Convert.ToUInt32(fields[1].Trim(), 16))] <- uint16 (Convert.ToUInt32(fields[0].Trim(), 16))
        | _ -> ())
    data
    |> Seq.map (fun pair -> struct (pair.Key, pair.Value))
    |> Seq.sortBy (fun struct (unicode, _) -> unicode)
    |> Seq.toList
