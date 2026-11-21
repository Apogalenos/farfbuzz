module Farfbuzz.Harness.Report

open System
open System.IO
open Farfbuzz.Harness.Runner

/// <summary>Counts of outcomes.</summary>
type Tally = { Pass: int; Fail: int; Skip: int; Broken: int }

/// <summary>An empty tally.</summary>
let empty = { Pass = 0; Fail = 0; Skip = 0; Broken = 0 }

/// <summary>Adds one outcome to a tally.</summary>
/// <param name="tally">The tally.</param>
/// <param name="outcome">The outcome.</param>
/// <returns>The new tally.</returns>
let add (tally: Tally) (outcome: Outcome) =
    match outcome with
    | Pass -> { tally with Pass = tally.Pass + 1 }
    | Fail _ -> { tally with Fail = tally.Fail + 1 }
    | Skip _ -> { tally with Skip = tally.Skip + 1 }
    | Broken _ -> { tally with Broken = tally.Broken + 1 }

/// <summary>Names an outcome for the TSV report.</summary>
/// <param name="outcome">The outcome.</param>
/// <returns>The outcome name and its detail.</returns>
let describe (outcome: Outcome) =
    match outcome with
    | Pass -> "pass", String.Empty
    | Fail (expected, actual) -> "fail", $"expected {expected} actual {actual}"
    | Skip (BackendNotProvided (kind, backend)) -> "skip", $"{kind}={backend} not provided"
    | Skip (SystemFontMissing path) -> "skip", $"{path} not found"
    | Skip (SystemFontVersion (path, expected, actual)) -> "skip", $"{path} sha1 {actual}, expected {expected}"
    | Broken reason -> "broken", reason

/// <summary>Tallies outcomes per suite and per file, in first-seen order.</summary>
/// <param name="results">The runs and their outcomes.</param>
/// <returns>Suite tallies, and file tallies keyed by suite and file.</returns>
let tally (results: (RunSpec * Outcome) list) =
    let bySuite = results |> List.groupBy (fun (spec, _) -> spec.Id.Suite) |> List.map (fun (suite, runs) -> suite, runs |> List.map snd |> List.fold add empty)
    let byFile =
        results
        |> List.groupBy (fun (spec, _) -> spec.Id.Suite, spec.Id.File, spec.Id.Registration)
        |> List.map (fun (file, runs) -> file, runs |> List.map snd |> List.fold add empty)
    bySuite, byFile

/// <summary>Writes the tab-separated report of every run.</summary>
/// <param name="path">The report file.</param>
/// <param name="results">The runs and their outcomes.</param>
let writeTsv (path: string) (results: (RunSpec * Outcome) list) =
    Directory.CreateDirectory(Path.GetDirectoryName path |> nonNull) |> ignore
    let header = "key\tsuite\tfile\tregistration\tline\tshaper\tfont-funcs\toutcome\tdetail"
    let spell (value: string voption) = match value with ValueSome text -> text | ValueNone -> "-"
    let rows =
        results
        |> List.map (fun (spec, outcome) ->
            let name, detail = describe outcome
            let id = spec.Id
            String.Join("\t", [ id.Key; id.Suite; id.File; string id.Registration; string id.Line; spell id.Backend.Shaper; spell id.Backend.FontFuncs; name; detail.Replace('\t', ' ') ]))
    File.WriteAllLines(path, header :: rows)

/// <summary>Formats the per-suite tallies as the Markdown table PROGRESS.md shows.</summary>
/// <param name="suites">The suite tallies.</param>
/// <returns>The table.</returns>
let markdown (suites: (string * Tally) list) =
    let row (suite: string, tally: Tally) =
        let ran = tally.Pass + tally.Fail + tally.Broken
        let percent = if ran = 0 then 0.0 else 100.0 * float tally.Pass / float ran
        $"| {suite} | {ran} | {tally.Pass} | {percent:F1} %% | {tally.Skip} | {tally.Fail} | {tally.Broken} |"
    String.Join("\n", "| Suite | Runs | Passing | % | Skipped | Failing | Broken |" :: "|---|---|---|---|---|---|---|" :: (suites |> List.map row))
