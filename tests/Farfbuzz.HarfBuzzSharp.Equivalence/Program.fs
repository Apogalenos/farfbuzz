module Farfbuzz.HarfBuzzSharp.Equivalence.Program

open System
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper

#if CORE
/// <summary>The core engine through the harness; the requests the public API cannot express are not run, as in the other two.</summary>
/// <param name="request">The request.</param>
/// <returns>The outcome, or why the request cannot run.</returns>
let private shape (request: ShapeRequest) =
    match Dump.unsupported request with
    | ValueSome reason -> Error $"unsupported: {reason}"
    | ValueNone -> FarfbuzzShaper.shaper.Shape request
#else
let private shape = PublicApiShaper.shape
#endif

/// <summary>Writes the dump of this build's implementation, or compares three dumps.</summary>
/// <param name="argv"><c>dump FILE</c> or <c>compare NATIVE FACADE CORE</c>.</param>
/// <returns>The exit code.</returns>
[<EntryPoint>]
let main argv =
    match argv with
    | [| "dump"; path |] ->
#if NATIVE
        match Farfbuzz.Oracle.OracleLibrary.ensureLoaded () with
        | Ok _ -> ()
        | Error reason -> failwith reason
#endif
        Dump.write path shape
        0
    | [| "compare"; native; facade; core |] -> Dump.compareDumps native facade core
    | _ ->
        eprintfn "usage: dump FILE | compare NATIVE FACADE CORE"
        2
