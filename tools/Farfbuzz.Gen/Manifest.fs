module Farfbuzz.Gen.Manifest

open System
open System.IO

/// <summary>One pinned generator input.</summary>
type ManifestEntry =
    { Name: string
      Version: string
      Url: string
      Sha256: string
      LocalPath: string
      Header: string list }

/// <summary>Escapes newline separators in the header column.</summary>
/// <param name="lines">The header lines.</param>
/// <returns>The column text.</returns>
let private escapeHeader (lines: string list) =
    String.Join("\\n", lines)

/// <summary>Reads the header column.</summary>
/// <param name="text">The column text.</param>
/// <returns>The header lines.</returns>
let private readHeader (text: string) =
    match text with
    | "" -> []
    | _ -> text.Split("\\n") |> Array.toList

/// <summary>Reads a tab-separated manifest.</summary>
/// <param name="path">The manifest path.</param>
/// <returns>The entries, or an error.</returns>
let read (path: string) : Result<ManifestEntry list, string> =
    match File.Exists path with
    | false -> Error $"manifest not found: {path}"
    | true ->
        let lines = File.ReadAllLines path |> Array.filter (fun line -> line.Length > 0 && not (line.StartsWith "#"))
        let entries =
            lines
            |> Array.mapi (fun index line ->
                match line.Split('\t') with
                | [| name; version; url; sha; localPath; header |] -> Ok { Name = name; Version = version; Url = url; Sha256 = sha; LocalPath = localPath; Header = readHeader header }
                | _ -> Error $"{path}:{index + 1}: expected six tab-separated columns")
        let errors = entries |> Array.choose (function | Error error -> Some error | Ok _ -> None)
        match errors.Length with
        | 0 -> entries |> Array.choose (function | Ok entry -> Some entry | Error _ -> None) |> Array.toList |> Ok
        | _ -> Error (String.Join("\n", errors))

/// <summary>Writes a tab-separated manifest.</summary>
/// <param name="path">The manifest path.</param>
/// <param name="entries">The entries.</param>
/// <returns>Unit.</returns>
let write (path: string) (entries: ManifestEntry list) : unit =
    let lines =
        entries
        |> List.map (fun entry ->
            String.Join("\t", [ entry.Name; entry.Version; entry.Url; entry.Sha256; entry.LocalPath; escapeHeader entry.Header ]))
    let directory = Path.GetDirectoryName path
    match directory with
    | null -> ()
    | directory -> Directory.CreateDirectory directory |> ignore
    File.WriteAllLines(path, "# Farfbuzz generator inputs: name, version, URL, SHA-256, local path, header lines" :: lines)
