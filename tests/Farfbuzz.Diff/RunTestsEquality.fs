module Farfbuzz.Diff.RunTestsEquality

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open Expecto
open Farfbuzz.Harness
open Farfbuzz.Harness.Suites
open Farfbuzz.Harness.Runner

/// <summary>What run-tests.py reported for one run, or for one silent skip.</summary>
type Reported =
    | Ok           // "ok N - hb-shape …"
    | NotOk        // "not ok N - hb-shape …"
    | SkipLine     // "ok N - font # skip …": an unsupported backend
    | SilentSkip   // "# … not found, skip." or "# Different version …": no TAP line

/// <summary>The hb-shape that build.fsx oracle builds next to the oracle library.</summary>
/// <returns>The path.</returns>
let hbShape () =
    Path.Combine(RepositoryPaths.root (), "artifacts", "oracle", RuntimeInformation.RuntimeIdentifier, (if OperatingSystem.IsWindows() then "hb-shape.exe" else "hb-shape"))

/// <summary>Classifies one line of run-tests.py's output.</summary>
/// <param name="line">The line.</param>
/// <returns>The report it carries, if any.</returns>
let classify (line: string) =
    match line with
    | _ when line.StartsWith "not ok " -> Some NotOk
    | _ when line.StartsWith "ok " && line.Contains " # skip " -> Some SkipLine
    | _ when line.StartsWith "ok " -> Some Ok
    | _ when line.StartsWith "# " && (line.EndsWith " not found, skip." || line.StartsWith "# Different version of ") -> Some SilentSkip
    | _ -> None

/// <summary>Runs upstream's run-tests.py on every registered file of a suite and splits its output per file.</summary>
/// <param name="suite">The suite.</param>
/// <returns>The reports of each registration, in order.</returns>
let runUpstream (suite: Suite) : Reported list list =
    let script = Path.Combine(RepositoryPaths.upstream (), "test", "shape", "run-tests.py")
    let info = ProcessStartInfo("python3", [ "-I"; script; hbShape () ] @ (suite.Files |> List.map _.Path), RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)
    use proc = Process.Start info |> nonNull
    let errors = proc.StandardError.ReadToEndAsync()
    let output = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    errors.Result |> ignore
    let lines = output.Split('\n') |> Array.toList
    let starts = lines |> List.indexed |> List.filter (fun (_, line) -> line.StartsWith "# Running tests in ") |> List.map fst
    let bounds = List.pairwise (starts @ [ lines.Length ])
    bounds |> List.map (fun (first, after) -> lines[first + 1 .. after - 1] |> List.choose classify)

/// <summary>Translates harness outcomes into what run-tests.py would have printed.</summary>
/// <param name="outcome">The harness outcome.</param>
/// <returns>The equivalent report.</returns>
let expected (outcome: Outcome) =
    match outcome with
    | Pass -> Ok
    | Fail _ | Broken _ -> NotOk
    | Skip (BackendNotProvided _) -> SkipLine
    | Skip (SystemFontMissing _ | SystemFontVersion _) -> SilentSkip

/// <summary>
/// The harness proof: for every registered file, the harness driving HarfBuzz 15.1.0 must report exactly what
/// upstream's run-tests.py reports with hb-shape built from the same tag, run by run and in the same order.
/// </summary>
/// <param name="shaper">The HarfBuzz engine.</param>
/// <param name="sink">Collects the harness results for the oracle report.</param>
/// <returns>The test list.</returns>
let tests (shaper: Shaper.Shaper) (sink: Collections.Concurrent.ConcurrentQueue<RunSpec * Outcome>) =
    let available = File.Exists(hbShape ()) && not (OperatingSystem.IsWindows())
    testList "P1.RunTestsEquality" [
        for suite in Suites.registered (RepositoryPaths.upstream ()) ->
            testCase suite.Name (fun () ->
                match available with
                | false -> skiptest $"no hb-shape at {hbShape ()} (build it with `dotnet fsi build.fsx oracle`; not available on Windows)"
                | true ->
                    let upstream = runUpstream suite
                    let ours =
                        suite.Files
                        |> List.map (fun entry ->
                            let results = plan shaper entry |> List.map (fun spec -> spec, execute shaper spec)
                            results |> List.iter sink.Enqueue
                            results)
                    Expect.equal upstream.Length suite.Files.Length "one run-tests.py section per registration"
                    let differences =
                        List.zip3 suite.Files ours upstream
                        |> List.collect (fun (entry, results, reported) ->
                            let mine = results |> List.map (snd >> expected)
                            match mine = reported with
                            | true -> []
                            | false ->
                                let detail =
                                    results
                                    |> List.map (fun (spec, outcome) -> $"    {displayName spec.Id}: {fst (Report.describe outcome)} {snd (Report.describe outcome)}")
                                [ $"{entry.Name}#{entry.Registration}: harness %A{mine}\n  run-tests.py %A{reported}"; String.Join("\n", detail) ])
                    Expect.isEmpty differences ("the harness disagrees with run-tests.py:\n" + String.Join("\n", differences)))
    ]
