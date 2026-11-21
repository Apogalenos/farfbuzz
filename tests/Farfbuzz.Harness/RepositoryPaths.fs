module Farfbuzz.Harness.RepositoryPaths

open System
open System.IO

/// <summary>Walks up from a directory to the first one holding the solution file.</summary>
/// <param name="directory">The directory to start from.</param>
/// <returns>The repository root, or none when the walk reaches the file-system root.</returns>
[<TailCall>]
let rec findRoot (directory: DirectoryInfo | null) : DirectoryInfo voption =
    match directory with
    | null -> ValueNone
    | candidate when File.Exists(Path.Combine(candidate.FullName, "Farfbuzz.slnx")) -> ValueSome candidate
    | candidate -> findRoot candidate.Parent

/// <summary>The repository root of the running test executable.</summary>
/// <returns>The root path; the executable always runs from inside the repository's artifacts folder.</returns>
let root () =
    match findRoot (DirectoryInfo AppContext.BaseDirectory) with
    | ValueSome directory -> directory.FullName
    | ValueNone -> failwith $"No Farfbuzz.slnx above {AppContext.BaseDirectory}: the differential tests run from the repository."

/// <summary>The upstream HarfBuzz checkout.</summary>
/// <returns>The path of <c>upstream/harfbuzz</c>.</returns>
let upstream () = Path.Combine(root (), "upstream", "harfbuzz")
