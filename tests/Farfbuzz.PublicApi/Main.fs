module Farfbuzz.PublicApi.Main

open System
open System.IO
open Expecto

/// <summary>Runs the tests, or writes the baseline with <c>--write-baseline FILE</c>.</summary>
/// <param name="argv">The arguments.</param>
/// <returns>The exit code.</returns>
[<EntryPoint>]
let main argv =
    match argv with
    | [| "--write-baseline"; path |] ->
        File.WriteAllLines(path, Surface.linesOf Surface.libraryPath)
        printfn $"{(Surface.linesOf Surface.libraryPath).Length} lines written to {path}"
        0
    | _ -> runTestsInAssemblyWithCLIArgs [] argv
