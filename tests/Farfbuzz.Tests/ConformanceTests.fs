module Farfbuzz.Tests.ConformanceTests

open System
open System.Collections.Concurrent
open System.IO
open Expecto
open Farfbuzz.Harness
open Farfbuzz.Harness.FarfbuzzShaper
open Farfbuzz.Harness.Runner
open Farfbuzz.Harness.Ratchet

/// <summary>Where the conformance baseline lives: the runs Farfbuzz is known to pass.</summary>
/// <returns>The baseline path.</returns>
let baselinePath () = Path.Combine(RepositoryPaths.root (), "tests", "Farfbuzz.Tests", "conformance.baseline")

/// <summary>
/// One Expecto test per registered test file. A file fails only when a baselined run stops passing; a file with no
/// passing run and nothing baselined is reported as skipped, which is how the suite is 0 % passing yet green today.
/// </summary>
/// <param name="sink">Collects every run's outcome and the newly passing runs for the reports written after the run.</param>
/// <returns>The test list.</returns>
let tests (sink: ConcurrentQueue<FileCheck>) =
    let baseline = read (baselinePath ())
    testList "P1.Conformance" [
        for suite in Suites.registered (RepositoryPaths.upstream ()) ->
            testList suite.Name [
                for entry in suite.Files ->
                    testCase $"{entry.Name}#{entry.Registration}" (fun () ->
                        let check = checkFile shaper baseline entry
                        sink.Enqueue check
                        let regressions = check.Regressions |> List.map (fun (spec, outcome) -> $"  {displayName spec.Id}: {fst (Report.describe outcome)} {snd (Report.describe outcome)}")
                        match regressions, check.Passing with
                        | [], 0 -> skiptest $"0 of {check.Ran} runs pass"
                        | [], passing -> Expect.isGreaterThan passing 0 $"{passing} of {check.Ran} runs pass"
                        | lines, _ -> failtest ("baselined runs regressed:\n" + String.Join("\n", lines)))
            ]
    ]

/// <summary>Writes the conformance reports when conformance ran: the TSV of every run, the newly passing runs, and the summary table.</summary>
/// <param name="sink">The collected file checks.</param>
let writeReports (sink: ConcurrentQueue<FileCheck>) =
    match sink.IsEmpty with
    | true -> ()
    | false ->
        let checks = sink.ToArray() |> Array.toList
        let results = checks |> List.collect _.Results
        let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "conformance")
        Report.writeTsv (Path.Combine(directory, "farfbuzz.tsv")) results
        File.WriteAllLines(Path.Combine(directory, "newly-passing.txt"), checks |> List.collect _.NewlyPassing |> List.map (fun spec -> line spec.Id))
        let suites, _ = Report.tally results
        let table = Report.markdown suites
        File.WriteAllText(Path.Combine(directory, "summary.md"), table + "\n")
        Console.WriteLine table
