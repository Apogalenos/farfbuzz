module Farfbuzz.HarfBuzzSharp.Equivalence.Dump

open System
open System.IO
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper

/// <summary>Why a request is not run: it asks for something HarfBuzzSharp's public API cannot express, so no implementation runs it.</summary>
/// <param name="request">The request.</param>
/// <returns>The reason, or none when the public API can express the request.</returns>
let unsupported (request: ShapeRequest) =
    match request with
    | _ when request.Ptem <> 0.0 -> ValueSome "ptem"
    | _ when request.Slant <> 0.0 -> ValueSome "slant"
    | _ when request.Embolden <> struct (0.0, 0.0) -> ValueSome "embolden"
    | _ when request.NotFoundVariationSelectorGlyph <> InvalidCodepoint -> ValueSome "not-found variation selector glyph"
    | _ when ValueOption.isSome request.FontFuncs && request.FontFuncs <> ValueSome "ot" -> ValueSome "font funcs"
    | _ -> ValueNone

/// <summary>Every registered case of the three shaping suites the core engine plans, as an id and a request.</summary>
/// <returns>The cases in registration order.</returns>
let cases () =
    Suites.registered (RepositoryPaths.upstream ())
    |> List.collect (fun suite -> suite.Files)
    |> List.collect (fun entry -> Runner.plan FarfbuzzShaper.shaper entry)
    |> List.choose (fun spec ->
        match spec.Kind with
        | Runner.Execute (fontPath, unicodes, options, extra, _) ->
            match ShapeRequest.build fontPath unicodes options extra with
            | Ok request when not request.SinglePar && Array.contains 0x0Au request.Text -> None
            | Ok request -> Some (Runner.displayName spec.Id, request)
            | Error _ -> None
        | Runner.Skipped _ | Runner.Unrunnable _ -> None)

/// <summary>What one case prints, as the Runner compares it: the serialised glyphs, an error line, or the exception.</summary>
/// <param name="shape">The implementation.</param>
/// <param name="request">The request.</param>
/// <returns>The output on one line.</returns>
let outputOf (shape: ShapeRequest -> Result<ShapeOutcome, string>) (request: ShapeRequest) =
    let text =
        try
            match shape request with
            | Error reason -> $"ERROR: {reason}"
            | Ok outcome -> Runner.render request outcome
        with error -> $"EXCEPTION {error.GetType().Name}: {error.Message}"
    text.Replace("\\", "\\\\").Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r")

/// <summary>Writes the dump of an implementation: one line, id, a tab and the output, for every case.</summary>
/// <param name="path">The file.</param>
/// <param name="shape">The implementation.</param>
let write (path: string) (shape: ShapeRequest -> Result<ShapeOutcome, string>) =
    let lines = cases () |> List.map (fun (id, request) -> $"{id}\t{outputOf shape request}")
    File.WriteAllLines(path, lines)
    printfn $"{lines.Length} cases written to {path}"

/// <summary>Reads a dump into a map by id.</summary>
/// <param name="path">The file.</param>
/// <returns>The output of each id.</returns>
let read (path: string) =
    File.ReadAllLines path
    |> Array.map (fun line ->
        let tab = line.IndexOf '\t'
        line.Substring(0, tab), line.Substring(tab + 1))
    |> Map.ofArray

/// <summary>Compares the three dumps: the ids where the facade differs from the native implementation must be exactly the ids where the core does, and
/// the facade must equal the core on them.</summary>
/// <param name="nativePath">The dump of the native implementation.</param>
/// <param name="facadePath">The dump of the facade.</param>
/// <param name="corePath">The dump of the core.</param>
/// <returns>The exit code: 0 when they agree as stated, 1 when they do not.</returns>
let compareDumps (nativePath: string) (facadePath: string) (corePath: string) =
    let native = read nativePath
    let facade = read facadePath
    let core = read corePath
    let ids = native |> Map.keys |> Seq.filter (fun id -> Map.containsKey id facade && Map.containsKey id core) |> List.ofSeq
    let isSkipped (id: string) = native[id].StartsWith "ERROR: unsupported:"
    let compared = ids |> List.filter (isSkipped >> not)
    let facadeDiffers = compared |> List.filter (fun id -> facade[id] <> native[id]) |> Set.ofList
    let coreDiffers = compared |> List.filter (fun id -> core[id] <> native[id]) |> Set.ofList
    let facadeNotCore = compared |> List.filter (fun id -> facade[id] <> core[id])
    let exceptions = compared |> List.filter (fun id -> facade[id].StartsWith "EXCEPTION")
    printfn $"{ids.Length} ids in all three dumps, {ids.Length - compared.Length} not run by the public API, {compared.Length} compared"
    printfn $"facade = native on {compared.Length - facadeDiffers.Count} cases; differences = core differences ({facadeDiffers.Count} ids)"
    let show (title: string) (found: string seq) =
        match Seq.isEmpty found with
        | true -> ()
        | false ->
            printfn $"{title}:"
            found |> Seq.truncate 20 |> Seq.iter (fun id -> printfn $"  {id}\n    native: {native[id]}\n    facade: {facade[id]}\n    core:   {core[id]}")
    show "facade differs from native but core does not" (Set.difference facadeDiffers coreDiffers)
    show "core differs from native but facade does not" (Set.difference coreDiffers facadeDiffers)
    show "facade differs from core" facadeNotCore
    show "facade threw" exceptions
    match facadeDiffers = coreDiffers && List.isEmpty facadeNotCore with
    | true -> 0
    | false -> 1
