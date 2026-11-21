module Farfbuzz.Gen.Fetch

open System
open System.IO
open System.Net.Http
open System.Security.Cryptography
open System.Text
open Farfbuzz.Gen.Manifest

/// <summary>Computes a file's SHA-256 as lowercase hexadecimal.</summary>
/// <param name="path">The file path.</param>
/// <returns>The digest.</returns>
let sha256 (path: string) : string =
    use stream = File.OpenRead path
    Convert.ToHexString(SHA256.HashData stream).ToLowerInvariant()

/// <summary>Returns the absolute path of an entry's local file.</summary>
/// <param name="dataDir">The tools/data directory.</param>
/// <param name="entry">The manifest entry.</param>
/// <returns>The path.</returns>
let localPath (dataDir: string) (entry: ManifestEntry) : string =
    Path.Combine(dataDir, entry.LocalPath)

/// <summary>Checks a manifest entry's local file against its pinned hash and header.</summary>
/// <param name="dataDir">The tools/data directory.</param>
/// <param name="entry">The manifest entry.</param>
/// <returns>Ok, or an error naming the input.</returns>
let verifyEntry (dataDir: string) (entry: ManifestEntry) : Result<unit, string> =
    let path = localPath dataDir entry
    match File.Exists path with
    | false -> Error $"{entry.Name}: missing {path}"
    | true ->
        let actual = sha256 path
        match String.Equals(actual, entry.Sha256, StringComparison.OrdinalIgnoreCase) with
        | false -> Error $"{entry.Name}: SHA-256 mismatch (expected {entry.Sha256}, got {actual})"
        | true ->
            let text = File.ReadAllText path
            let headerOk =
                match entry.Header with
                | [] -> true
                | expected when path.EndsWith(".html", StringComparison.OrdinalIgnoreCase) ->
                    expected |> List.forall (fun line -> text.Contains(line, StringComparison.Ordinal))
                | expected ->
                    let leading = File.ReadAllLines path |> Array.truncate 32
                    expected |> List.forall (fun line -> leading |> Array.exists ((=) line))
            match headerOk with
            | true -> Ok ()
            | false -> Error $"{entry.Name}: header mismatch in {path}"

/// <summary>Verifies every manifest entry.</summary>
/// <param name="dataDir">The tools/data directory.</param>
/// <param name="entries">The entries.</param>
/// <returns>Ok, or the first error.</returns>
let verify (dataDir: string) (entries: ManifestEntry list) : Result<unit, string> =
    let errors = entries |> List.choose (fun entry -> match verifyEntry dataDir entry with | Ok () -> None | Error error -> Some error)
    match errors with
    | [] -> Ok ()
    | _ -> Error (String.Join("\n", errors))

/// <summary>Downloads every missing entry; existing files are left for verify.</summary>
/// <param name="dataDir">The tools/data directory.</param>
/// <param name="entries">The entries.</param>
/// <returns>Ok, or the first download error.</returns>
let fetchAll (dataDir: string) (entries: ManifestEntry list) : Result<unit, string> =
    let client = new HttpClient()
    let errors = ResizeArray<string>()
    for entry in entries do
        let path = localPath dataDir entry
        match File.Exists path with
        | true -> ()
        | false ->
            try
                match Uri.TryCreate(entry.Url, UriKind.Absolute) with
                | true, null -> errors.Add($"{entry.Name}: invalid URL {entry.Url}")
                | true, uri ->
                    match uri with
                    | null -> errors.Add($"{entry.Name}: invalid URL {entry.Url}")
                    | value ->
                        match value.Scheme = Uri.UriSchemeHttps || value.Scheme = Uri.UriSchemeHttp with
                        | false -> errors.Add($"{entry.Name}: invalid URL {entry.Url}")
                        | true ->
                            let bytes = client.GetByteArrayAsync(value).GetAwaiter().GetResult()
                            let directory = Path.GetDirectoryName path
                            match directory with
                            | null -> ()
                            | directory -> Directory.CreateDirectory directory |> ignore
                            File.WriteAllBytes(path, bytes)
                | _ -> errors.Add($"{entry.Name}: invalid URL {entry.Url}")
            with error -> errors.Add($"{entry.Name}: {error.Message}")
    match errors.Count with
    | 0 -> Ok ()
    | _ -> Error (String.Join("\n", errors))
