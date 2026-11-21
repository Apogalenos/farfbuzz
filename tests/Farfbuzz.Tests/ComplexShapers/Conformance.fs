module Farfbuzz.Tests.ComplexShapers.Conformance

open System
open Expecto
open Farfbuzz.Harness
open Farfbuzz.Harness.FarfbuzzShaper
open Farfbuzz.Harness.Runner
open Farfbuzz.Harness.TestFile

/// <summary>The registered entry of a suite and file name.</summary>
/// <param name="suite">The suite name.</param>
/// <param name="file">The file name.</param>
/// <returns>The entry.</returns>
let private entry (suite: string) (file: string) : Suites.TestFileEntry =
    Suites.registered (RepositoryPaths.upstream ())
    |> List.collect (fun suite -> suite.Files)
    |> List.find (fun entry -> entry.Suite = suite && entry.Name = file)

/// <summary>The line numbers of the case lines of a file, in order.</summary>
/// <param name="entry">The file entry.</param>
/// <returns>The line numbers.</returns>
let private caseLines (entry: Suites.TestFileEntry) : int list =
    TestFile.parse entry.Path
    |> List.choose (fun line -> match line with Case (number, _) -> Some number | CommentedCase _ | Directive _ | Ignored _ | Malformed _ -> None)

/// <summary>The number of case lines across the deduplicated files.</summary>
/// <param name="files">The suite and file pairs.</param>
/// <returns>The case count.</returns>
let caseCount (files: (string * string) list) : int =
    files |> List.distinct |> List.sumBy (fun (suite, file) -> caseLines (entry suite file) |> List.length)

/// <summary>
/// One test per case line of the listed files, each running the line's runs through the Farfbuzz shaper and expecting
/// every run to pass.
/// </summary>
/// <param name="sliceId">The test-list name.</param>
/// <param name="files">The suite and file pairs, deduplicated.</param>
/// <returns>The test list.</returns>
let files (sliceId: string) (files: (string * string) list) : Test =
    testList sliceId [
        for suite, file in List.distinct files do
            let entry = entry suite file
            let specs = Runner.plan shaper entry
            for line in caseLines entry do
                let runs = specs |> List.filter (fun spec -> spec.Id.Line = line)
                yield
                    testCase $"{suite}/{file}:{line}" (fun _ ->
                        let failures = runs |> List.filter (fun spec -> Runner.execute shaper spec <> Pass)
                        match failures with
                        | [] -> ()
                        | _ ->
                            let named = failures |> List.map (fun spec -> displayName spec.Id) |> String.concat ", "
                            failtest $"cases did not pass: {named}")
    ]

/// <summary>
/// One test per case line of the listed files except the excluded lines, each running the line's runs through the Farfbuzz
/// shaper and expecting every run to pass. An excluded line fails for a reason outside the slice that names it; the binding
/// that lists it says which, and is deleted when that reason is gone.
/// </summary>
/// <param name="sliceId">The test-list name.</param>
/// <param name="files">The suite and file pairs, deduplicated.</param>
/// <param name="excluded">The suite, file and line of every excluded case.</param>
/// <returns>The test list.</returns>
let filesExcept (sliceId: string) (files: (string * string) list) (excluded: (string * string * int) list) : Test =
    testList sliceId [
        for suite, file in List.distinct files do
            let entry = entry suite file
            let specs = Runner.plan shaper entry
            for line in caseLines entry do
                match List.contains (suite, file, line) excluded with
                | true -> ()
                | false ->
                    let runs = specs |> List.filter (fun spec -> spec.Id.Line = line)
                    yield
                        testCase $"{suite}/{file}:{line}" (fun _ ->
                            let failures = runs |> List.filter (fun spec -> Runner.execute shaper spec <> Pass)
                            match failures with
                            | [] -> ()
                            | _ ->
                                let named = failures |> List.map (fun spec -> displayName spec.Id) |> String.concat ", "
                                failtest $"cases did not pass: {named}")
    ]

/// <summary>The P6.S02 conformance-helper case: arabic-stch.tests holds exactly three cases.</summary>
/// <returns>The test list.</returns>
[<Tests>]
let tests =
    testList "P6.S02" [
        testCase "conformance helper" (fun _ ->
            Expect.equal (caseCount [ "in-house", "arabic-stch.tests" ]) 3 "arabic-stch.tests yields 3 cases")
    ]
