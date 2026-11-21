module Farfbuzz.Harness.Ratchet

open System
open System.IO
open Farfbuzz.Harness.Suites
open Farfbuzz.Harness.Shaper
open Farfbuzz.Harness.Runner

/// <summary>How a run's outcome relates to the baseline of runs known to pass.</summary>
type Verdict =
    | Holds         // baselined and still passing, or skipped in this environment
    | Regressed     // baselined but no longer passing: a failure
    | NewlyPassing  // not baselined yet and passing: recorded for `build.fsx ratchet`
    | StillFailing  // not baselined and not passing: reported as skipped by the conformance test
    | NotRun        // not baselined and skipped

/// <summary>Judges one outcome against the baseline.</summary>
/// <param name="baselined">Whether the run's key is in the baseline.</param>
/// <param name="outcome">The outcome.</param>
/// <returns>The verdict.</returns>
let verdict (baselined: bool) (outcome: Outcome) =
    match baselined, outcome with
    | true, Pass -> Holds
    | true, Skip _ -> Holds
    | true, (Fail _ | Broken _) -> Regressed
    | false, Pass -> NewlyPassing
    | false, (Fail _ | Broken _) -> StillFailing
    | false, Skip _ -> NotRun

/// <summary>Reads a baseline file: one key per line, optionally followed by the run's name; '#' starts a comment.</summary>
/// <param name="path">The baseline file.</param>
/// <returns>The keys; an absent file is an empty baseline.</returns>
let read (path: string) : Set<string> =
    match File.Exists path with
    | false -> Set.empty
    | true ->
        File.ReadAllLines path
        |> Array.choose (fun line ->
            match line.Trim() with
            | text when text.Length = 0 || text.StartsWith '#' -> None
            | text -> Some (text.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0]))
        |> Set.ofArray

/// <summary>Formats the baseline line of a run.</summary>
/// <param name="id">The run identity.</param>
/// <returns>The key, two spaces, and the display name.</returns>
let line (id: TestId) = id.Key + "  " + displayName id

/// <summary>Merges newly passing runs into a baseline file, keeping it sorted; it never removes a key.</summary>
/// <param name="baselinePath">The baseline file.</param>
/// <param name="newlyPassing">The lines to add.</param>
/// <returns>The number of keys added.</returns>
let merge (baselinePath: string) (newlyPassing: string list) =
    let existing = if File.Exists baselinePath then File.ReadAllLines baselinePath |> Array.toList else []
    let known = read baselinePath
    let additions = newlyPassing |> List.filter (fun entry -> not (known.Contains (entry.Split(' ')[0]))) |> List.distinct
    let header = existing |> List.filter (fun entry -> entry.StartsWith '#')
    let body = (existing |> List.filter (fun entry -> entry.Length > 0 && not (entry.StartsWith '#'))) @ additions |> List.sort
    File.WriteAllLines(baselinePath, header @ body)
    additions.Length

/// <summary>The conformance of one test file against the baseline.</summary>
type FileCheck =
    { Results: (RunSpec * Outcome) list
      Regressions: (RunSpec * Outcome) list  // baselined runs that no longer pass
      NewlyPassing: RunSpec list             // passing runs not in the baseline yet
      Passing: int
      Ran: int }                             // runs that executed: passing, failing or broken

/// <summary>Runs one test file with an engine and judges every run against the baseline.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="baseline">The keys known to pass.</param>
/// <param name="entry">The test file.</param>
/// <returns>The file's conformance.</returns>
let checkFile (shaper: Shaper) (baseline: Set<string>) (entry: TestFileEntry) =
    let results = plan shaper entry |> List.map (fun spec -> spec, execute shaper spec)
    let judged = results |> List.map (fun (spec, outcome) -> spec, outcome, verdict (baseline.Contains spec.Id.Key) outcome)
    { Results = results
      Regressions = judged |> List.choose (fun (spec, outcome, judgement) -> match judgement with Regressed -> Some (spec, outcome) | Holds | NewlyPassing | StillFailing | NotRun -> None)
      NewlyPassing = judged |> List.choose (fun (spec, _, judgement) -> match judgement with NewlyPassing -> Some spec | Holds | Regressed | StillFailing | NotRun -> None)
      Passing = results |> List.filter (fun (_, outcome) -> outcome = Pass) |> List.length
      Ran = results |> List.filter (fun (_, outcome) -> match outcome with Pass | Fail _ | Broken _ -> true | Skip _ -> false) |> List.length }
