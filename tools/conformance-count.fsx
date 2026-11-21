// Counts conformance runs per test file from artifacts/conformance/farfbuzz.tsv (written by the Tests project).
// Usage: dotnet fsi tools/conformance-count.fsx [file-name-substring ...]
// Example: dotnet fsi tools/conformance-count.fsx emoji-clusters spaces gsub1
// Phase files state their targets as passing runs per file; this prints exactly that.

open System
open System.IO

/// <summary>The report written by the last run of the Tests project.</summary>
let report = Path.Combine(__SOURCE_DIRECTORY__, "..", "artifacts", "conformance", "farfbuzz.tsv")

/// <summary>The file-name filters given on the command line; none means every file.</summary>
let filters = fsi.CommandLineArgs |> Array.toList |> List.tail

/// <summary>Whether a test file is selected.</summary>
/// <param name="name">The file name.</param>
/// <returns>True when no filter is given or one matches.</returns>
let selected (name: string) = filters.IsEmpty || filters |> List.exists (fun filter -> name.Contains filter)

match File.Exists report with
| false ->
    Console.Error.WriteLine $"No {report}: run `dotnet run --project tests/Farfbuzz.Tests -c Release -- --summary` first."
    exit 2
| true ->
    let rows =
        File.ReadAllLines report
        |> Array.skip 1
        |> Array.map (fun line -> line.Split('\t'))
        |> Array.filter (fun fields -> selected fields[2])
    let perFile =
        rows
        |> Array.groupBy (fun fields -> $"{fields[1]}/{fields[2]}#{fields[3]}")
        |> Array.map (fun (file, runs) -> file, runs |> Array.filter (fun fields -> fields[7] = "pass") |> Array.length, runs.Length)
    perFile |> Array.iter (fun (file, passing, total) -> Console.WriteLine $"{passing,6} / {total,-6} {file}")
    let passing = perFile |> Array.sumBy (fun (_, count, _) -> count)
    let total = perFile |> Array.sumBy (fun (_, _, count) -> count)
    Console.WriteLine $"{passing,6} / {total,-6} total over {perFile.Length} registered files"
