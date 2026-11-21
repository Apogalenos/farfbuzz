module Farfbuzz.Diff.MacosDiff

open System
open Expecto
open Farfbuzz.Harness
open Farfbuzz.Harness.Runner
open Farfbuzz.Harness.Shaper
open Farfbuzz.Harness.ShapeRequest

/// <summary>The registered file of the lines that name macOS system fonts.</summary>
[<Literal>]
let private MacosFile = "macos.tests"

/// <summary>What an engine prints for one request, or why it cannot run it.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="request">The request.</param>
/// <returns>The output line, or the engine's reason.</returns>
let private printed (shaper: Shaper) (request: ShapeRequest) =
    match shaper.Shape request with
    | Ok outcome -> render request outcome
    | Error reason -> $"cannot run: {reason}"

/// <summary>Shapes the request of one planned run with both engines.</summary>
/// <param name="oracle">HarfBuzz.</param>
/// <param name="farfbuzz">Farfbuzz.</param>
/// <param name="spec">The planned run.</param>
/// <returns>The line number and both outputs when the run executes, none when it is skipped or unrunnable.</returns>
let private compared (oracle: Shaper) (farfbuzz: Shaper) (spec: RunSpec) =
    match spec.Kind with
    | Skipped _ | Unrunnable _ -> None
    | Execute (fontPath, unicodes, options, extra, _) ->
        match ShapeRequest.build fontPath unicodes options extra with
        | Error _ -> None
        | Ok request -> Some (spec.Id.Line, printed oracle request, printed farfbuzz request)

/// <summary>Every line of <c>in-house/macos.tests</c> whose system font exists here with the hash the line names:
/// Farfbuzz prints what HarfBuzz prints. Lines with no hash may differ from the file's expectation on another macOS
/// release, which is why the oracle, not the file, is the reference.</summary>
/// <param name="oracle">HarfBuzz.</param>
/// <param name="farfbuzz">Farfbuzz.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    testList "P8.S14" [
        testCase "P8.S14 macos" (fun () ->
            let entry =
                Suites.registered (RepositoryPaths.upstream ())
                |> List.find (fun suite -> suite.Name = "in-house")
                |> _.Files
                |> List.find (fun file -> file.Name = MacosFile)
            let runs = plan farfbuzz entry |> List.choose (compared oracle farfbuzz)
            let mismatching = runs |> List.filter (fun (_, theirs, ours) -> theirs <> ours)
            let lines = runs |> List.map (fun (line, _, _) -> string line) |> String.concat ", "
            printfn "P8.S14 macos: %d lines of macos.tests ran with an installed font of the hash they name (lines %s); %d mismatching" runs.Length lines mismatching.Length
            let report = mismatching |> List.map (fun (line, theirs, ours) -> $"line {line}: HarfBuzz {theirs}, Farfbuzz {ours}")
            Expect.isEmpty report ("macos.tests lines on which Farfbuzz differs from HarfBuzz:\n" + String.concat "\n" report))
    ]
