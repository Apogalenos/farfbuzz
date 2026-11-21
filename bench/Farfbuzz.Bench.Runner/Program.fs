module Farfbuzz.Bench.Runner.Program

open System
open System.IO
open Farfbuzz.Bench.Runner.Impl
open Farfbuzz.Bench.Runner.Protocol

/// <summary>The prefix of the line a browser build prints its JSON on, for the host script to write to a file.</summary>
[<Literal>]
let JsonLinePrefix = "BENCH-JSON "

/// <summary>The bytes in a megabyte, for the heap size the runner reports between pairs.</summary>
[<Literal>]
let private BytesPerMegabyte = 1048576L

/// <summary>What the command line asks for.</summary>
type Options =
    { Pairs: int list
      Passes: int
      Impl: string
      Json: string }

/// <summary>The indices of the pairs of one selector.</summary>
/// <param name="text">A pair number or <c>all</c>.</param>
/// <returns>The pair indices, or why the selector is wrong.</returns>
let private pairsOf (text: string) =
    match text, Int32.TryParse text with
    | "all", _ -> Ok [ 0 .. specs.Length - 1 ]
    | _, (true, index) when index >= 0 && index < specs.Length -> Ok [ index ]
    | _ -> Error $"--pair expects 0 to {specs.Length - 1} or all, not {text}"

/// <summary>Reads the options of the <c>shape</c> command.</summary>
/// <param name="options">The options so far.</param>
/// <param name="arguments">The arguments left.</param>
/// <returns>The options, or why the command line is wrong.</returns>
[<TailCall>]
let rec private parse (options: Options) (arguments: string list) =
    match arguments with
    | [] -> Ok options
    | "--pair" :: text :: rest ->
        match pairsOf text with
        | Ok pairs -> parse { options with Pairs = pairs } rest
        | Error problem -> Error problem
    | "--passes" :: text :: rest ->
        match Int32.TryParse text with
        | true, passes when passes > 0 -> parse { options with Passes = passes } rest
        | _ -> Error $"--passes expects a positive number, not {text}"
    | "--impl" :: name :: rest -> parse { options with Impl = name } rest
    | "--json" :: path :: rest -> parse { options with Json = path } rest
    | unknown :: _ -> Error $"unknown or incomplete argument {unknown}"

/// <summary>Reads the command line.</summary>
/// <param name="arguments">The arguments.</param>
/// <returns>The options, or why the command line is wrong.</returns>
let private options (arguments: string list) =
    match arguments with
    | "shape" :: rest -> parse { Pairs = [ 0 .. specs.Length - 1 ]; Passes = 10; Impl = "farfbuzz"; Json = String.Empty } rest
    | _ -> Error "usage: shape --pair <0-9|all> --passes <n> --impl farfbuzz|native|facade --json <file>"

/// <summary>Measures every selected pair, printing each result as it arrives.</summary>
/// <param name="impl">The implementation.</param>
/// <param name="passes">The fewest passes.</param>
/// <param name="pairs">The pair indices left.</param>
/// <param name="finished">The measurements so far, last first.</param>
/// <returns>The measurements in order, or the first failure.</returns>
[<TailCall>]
let rec private measureAll (impl: Impl) (passes: int) (pairs: int list) (finished: Measurement list) =
    match pairs with
    | [] -> Ok (List.rev finished)
    | index :: rest ->
        match load specs[index] with
        | Error problem -> Error problem
        | Ok pair ->
            Console.WriteLine $"pair {index} {pair.Label}: measuring, managed heap {GC.GetTotalMemory true / BytesPerMegabyte} MB"
            let measurement = measure impl pair passes
            Console.WriteLine $"pair {index} {measurement.Label}: median {measurement.MedianMilliseconds:F2} ms, min {measurement.MinimumMilliseconds:F2} ms, {measurement.Passes} passes, {measurement.Glyphs} glyphs/pass, {measurement.BytesPerPass} B/pass, first pass {measurement.WarmUpGlyphs} glyphs hash {measurement.WarmUpHash:x16}, every pass equal to the first: {measurement.Consistent}, hash {measurement.Hash:x16}"
            measureAll impl passes rest (measurement :: finished)

/// <summary>Creates the folder that will hold a file.</summary>
/// <param name="path">The file.</param>
let private ensureFolder (path: string) =
    match Path.GetDirectoryName(Path.GetFullPath path) with
    | null -> ()
    | folder -> Directory.CreateDirectory folder |> ignore

/// <summary>Writes the JSON of a run: to the file, or, in a browser, to the line the host script collects.</summary>
/// <param name="path">The file named on the command line.</param>
/// <param name="json">The document.</param>
let private publish (path: string) (json: string) =
    match path.Length, OperatingSystem.IsBrowser() with
    | 0, _ -> ()
    | _, true -> Console.WriteLine(JsonLinePrefix + json)
    | _, false ->
        ensureFolder path
        File.WriteAllText(path, json)

/// <summary>Runs the command.</summary>
/// <param name="options">The options.</param>
/// <returns>Zero on success, or why it failed.</returns>
let private run (options: Options) =
    match all |> List.tryFind (fun impl -> impl.Name = options.Impl) with
    | None -> Error $"implementation {options.Impl} is not built into this runner (native needs -p:BenchImpl=native, facade needs -p:BenchImpl=facade)"
    | Some impl ->
        Console.WriteLine $"runner {runtimeLabel ()} impl {impl.Name}"
        match measureAll impl options.Passes options.Pairs [] with
        | Error problem -> Error problem
        | Ok results ->
            publish options.Json (toJson impl.Name results)
            match results |> List.filter (fun result -> not result.Consistent) with
            | [] -> Ok results.Length
            | differing ->
                let names = String.Join(", ", differing |> List.map _.Label)
                Error $"a pass differs from the first pass in glyph count or output hash on: {names}"

/// <summary>Shapes the pairs cold, warm and cold again and prints every line whose output differs.</summary>
/// <param name="pairs">The pair indices left.</param>
/// <param name="status">Zero while every pair so far agrees, one once a pair differs.</param>
/// <returns>The exit status, or why a pair could not be loaded.</returns>
[<TailCall>]
let rec private diagnoseAll (pairs: int list) (status: int) =
    match pairs with
    | [] -> Ok status
    | index :: rest ->
        match load specs[index] with
        | Error problem -> Error problem
        | Ok pair ->
            let report = diagnose pair
            Console.WriteLine $"pair {index} {pair.Label}: {report}"
            diagnoseAll rest (if report.Contains "line " then 1 else status)

/// <summary>Runs the <c>diagnose</c> command.</summary>
/// <param name="arguments">The arguments after the command.</param>
/// <returns>The exit status, or why the command line is wrong.</returns>
let private runDiagnose (arguments: string list) =
    match arguments with
    | [ "--pair"; text ] -> pairsOf text |> Result.bind (fun pairs -> diagnoseAll pairs 0)
    | [ "--pair"; text; "--line"; line; "--minimise" ] ->
        pairsOf text
        |> Result.bind (fun pairs ->
            match load specs[pairs[0]] with
            | Error problem -> Error problem
            | Ok pair ->
                Console.WriteLine(minimise pair (int line))
                Ok 0)
    | [ "--pair"; text; "--line"; line; "--mode"; mode ] ->
        pairsOf text
        |> Result.bind (fun pairs ->
            match load specs[pairs[0]] with
            | Error problem -> Error problem
            | Ok pair ->
                Console.WriteLine(diagnoseLine pair (int line) mode)
                Ok 0)
    | _ -> Error "usage: diagnose --pair <0-9|all>"

/// <summary>Runs the <c>shape</c> command.</summary>
/// <param name="arguments">The command-line arguments.</param>
/// <returns>The exit status, or why the command line is wrong.</returns>
let private runShape (arguments: string list) =
    options arguments
    |> Result.bind run
    |> Result.map (fun count ->
        Console.WriteLine $"{count} results"
        0)

/// <summary>The entry point: the <c>shape</c> and <c>diagnose</c> commands.</summary>
/// <param name="argv">The command-line arguments.</param>
/// <returns>Zero on success, two for a bad command line or a failure.</returns>
[<EntryPoint>]
let main argv =
    let outcome =
        match List.ofArray argv with
        | "diagnose" :: rest -> runDiagnose rest
        | arguments -> runShape arguments
    match outcome with
    | Ok status -> status
    | Error problem ->
        Console.Error.WriteLine problem
        2
