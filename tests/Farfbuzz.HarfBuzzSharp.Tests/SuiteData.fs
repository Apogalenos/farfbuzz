module Farfbuzz.HarfBuzzSharp.Tests.SuiteData

open System
open Farfbuzz.Harness

/// <summary>The values of one hb-shape option over every case of every registered file of the three shaping suites.</summary>
/// <param name="name">The option, for example <c>--features</c>.</param>
/// <returns>The distinct values, sorted.</returns>
let optionValues (name: string) : string list =
    Suites.registered (RepositoryPaths.upstream ())
    |> List.collect (fun suite -> suite.Files)
    |> List.collect (fun entry -> TestFile.parse entry.Path)
    |> List.collect (fun line ->
        match line with
        | TestFile.Case (_, fields) ->
            match ShapeRequest.pairs fields.Options with
            | Ok pairs -> pairs |> List.choose (fun (option, value) -> match option = name, value with | true, ValueSome text -> Some text | _ -> None)
            | Error _ -> []
        | TestFile.CommentedCase _ | TestFile.Directive _ | TestFile.Ignored _ | TestFile.Malformed _ -> [])
    |> List.distinct
    |> List.sort
