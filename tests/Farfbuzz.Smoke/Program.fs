module Farfbuzz.Smoke.Program

open System
open Farfbuzz.Smoke.Probes

/// <summary>Formats one check as a report line.</summary>
/// <param name="check">The check.</param>
/// <returns>A line starting with PASS or FAIL.</returns>
let describe (check: Check) =
    match check.Outcome with
    | Ok observed -> $"PASS {check.Name}: {observed}"
    | Error observed -> $"FAIL {check.Name}: {observed}"

/// <summary>Selects the checks requested on the command line.</summary>
/// <param name="arguments">The arguments: none for the gate, <c>--probe-mutual</c>, or <c>--plant-failure</c>.</param>
/// <returns>The evaluated checks.</returns>
let selected (arguments: string list) =
    match arguments with
    | [ "--probe-mutual" ] -> [ mutualRecursionProbe () ]
    | [ "--plant-failure" ] -> gate () @ [ plantedFailure () ]
    | _ -> gate ()

/// <summary>Runs the smoke checks and reports them on standard output.</summary>
/// <param name="argv">The command-line arguments.</param>
/// <returns>Zero when every check passed, one otherwise.</returns>
[<EntryPoint>]
let main argv =
    Console.WriteLine $"Farfbuzz smoke on {runtimeDescription ()}"
    let checks = selected (List.ofArray argv)
    checks |> List.iter (fun check -> Console.WriteLine(describe check))
    match checks |> List.forall (fun check -> Result.isOk check.Outcome) with
    | true -> 0
    | false -> 1
