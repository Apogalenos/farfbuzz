module Farfbuzz.Diff.Differential

open System
open System.Collections.Generic
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz.Harness

/// <summary>The number of generated cases every sampled differential runs by default.</summary>
[<Literal>]
let DefaultCount = 10_000

/// <summary>An FsCheck configuration running a given number of cases.</summary>
/// <param name="count">The number of cases.</param>
/// <returns>The configuration.</returns>
let config (count: int) =
    { FsCheckConfig.defaultConfig with maxTest = count; endSize = 64 }

/// <summary>Writes what a failed differential case holds to <c>artifacts/flaky</c>, so that a failure that happens once in a
/// long run is described by a file and not only by the scrolled output: the test's name, the time, the input and both answers.
/// Shrinking calls the property again, so a failure writes a few files, the last of them the smallest input.</summary>
/// <param name="name">The test name.</param>
/// <param name="report">The input and the two answers.</param>
let private recordFailure (name: string) (report: string) : unit =
    let folder = Path.Combine(RepositoryPaths.root (), "artifacts", "flaky")
    Directory.CreateDirectory folder |> ignore
    let safe = String(name |> Seq.truncate 80 |> Seq.map (fun letter -> match Char.IsLetterOrDigit letter with | true -> letter | false -> '-') |> Seq.toArray)
    let stamp = DateTime.UtcNow.ToString "yyyyMMdd-HHmmss-fffffff"
    File.WriteAllText(Path.Combine(folder, $"diff-{stamp}-{Guid.NewGuid():N}-{safe}.txt"), $"{name}\n{DateTime.UtcNow:O}, process {Environment.ProcessId}\n{report}\n")

/// <summary>
/// Compares Farfbuzz with the oracle on generated inputs. On a mismatch FsCheck shrinks the input and Expecto
/// prints the replay seed, so the failure can be reproduced exactly.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="inputs">The input generator.</param>
/// <param name="count">The number of cases.</param>
/// <param name="ours">Farfbuzz's answer.</param>
/// <param name="oracle">HarfBuzz's answer.</param>
/// <returns>The test.</returns>
let sampled (name: string) (inputs: Gen<'I>) (count: int) (ours: 'I -> 'O) (oracle: 'I -> 'O) =
    testPropertyWithConfig (config count) name (Prop.forAll (Arb.fromGen inputs) (fun input ->
        let expected = oracle input
        let actual = ours input
        match actual = expected with
        | true -> Prop.ofTestable true
        | false ->
            let report = $"input: %A{input}\n  oracle:   %A{expected}\n  farfbuzz: %A{actual}"
            recordFailure name report
            Prop.ofTestable false |> Prop.label report))

/// <summary>Compares Farfbuzz with the oracle on every input of a finite domain, reporting the first mismatches.</summary>
/// <param name="name">The test name.</param>
/// <param name="inputs">The inputs.</param>
/// <param name="ours">Farfbuzz's answer.</param>
/// <param name="oracle">HarfBuzz's answer.</param>
/// <returns>The test.</returns>
let exhaustive (name: string) (inputs: seq<'I>) (ours: 'I -> 'O) (oracle: 'I -> 'O) =
    testCase name (fun () ->
        let mismatches =
            inputs
            |> Seq.choose (fun input ->
                let expected = oracle input
                let actual = ours input
                if actual = expected then None else Some (input, expected, actual))
            |> Seq.truncate 20
            |> Seq.toList
        let report = mismatches |> List.map (fun (input, expected, actual) -> $"  %A{input}: oracle %A{expected}, farfbuzz %A{actual}")
        Expect.isEmpty mismatches ("first mismatches:\n" + String.concat "\n" report))

/// <summary>The outline source of an answer that reports unported outlines, when it is one.</summary>
/// <param name="answer">Farfbuzz's comparable answer.</param>
/// <returns>The source, or none.</returns>
let outlinesNotPorted (answer: Result<'T, string>) : string voption =
    match answer with
    | Error reason ->
        match reason.StartsWith "outlines not ported: " with
        | true -> ValueSome (reason.Substring "outlines not ported: ".Length)
        | false -> ValueNone
    | Ok _ -> ValueNone

/// <summary>A stable seed for a differential name, so its cases replay identically.</summary>
/// <param name="name">The test name.</param>
/// <returns>The seed.</returns>
let private stableSeed (name: string) : uint64 =
    let mutable hash = 5381UL
    for character in name do
        hash <- (hash * 33UL + uint64 (int character)) &&& 0x7FFFFFFFUL
    hash

/// <summary>
/// Compares Farfbuzz with the oracle on generated inputs, counting the cases where Farfbuzz reports unported
/// outlines as a third outcome that is neither agreement nor failure, and failing when more than half the cases
/// report one.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="inputs">The input generator.</param>
/// <param name="count">The number of cases.</param>
/// <param name="ours">Farfbuzz's answer.</param>
/// <param name="oracle">HarfBuzz's answer.</param>
/// <param name="outlinesSource">Extracts the unported outline source from Farfbuzz's answer, when it is one.</param>
/// <returns>The test.</returns>
let sampledWithOutlines (name: string) (inputs: Gen<'I>) (count: int) (ours: 'I -> 'O) (oracle: 'I -> 'O) (outlinesSource: 'O -> string voption) =
    testCase name (fun () ->
        let samples = Gen.sampleWithSeed (Rnd (stableSeed name)) count count inputs
        let notPorted = Dictionary<string, int>()
        let mismatches = ResizeArray<string>()
        let mutable agrees = 0
        for input in samples do
            let actual = ours input
            let expected = oracle input
            match outlinesSource actual with
            | ValueSome source ->
                match notPorted.TryGetValue source with
                | true, n -> notPorted[source] <- n + 1
                | false, _ -> notPorted[source] <- 1
            | ValueNone ->
                match actual = expected with
                | true -> agrees <- agrees + 1
                | false -> mismatches.Add $"  %A{input}: oracle %A{expected}, farfbuzz %A{actual}"
        let total = notPorted |> Seq.sumBy (fun kv -> kv.Value)
        let bySource =
            match notPorted.Count with
            | 0 -> "none"
            | _ -> notPorted |> Seq.map (fun kv -> $"{kv.Key}: {kv.Value}") |> String.concat ", "
        Console.WriteLine $"{name}: {agrees} agree, outlines not ported: {bySource}"
        Expect.isTrue (total <= count / 2) $"{name}: {total} of {count} cases report unported outlines, hiding the differential"
        Expect.isEmpty mismatches ("first mismatches:\n" + String.concat "\n" mismatches))
