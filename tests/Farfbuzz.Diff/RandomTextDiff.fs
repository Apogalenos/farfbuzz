module Farfbuzz.Diff.RandomTextDiff

open System
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts

/// <summary>Strings shaped per face.</summary>
[<Literal>]
let StringsPerFace = 12

/// <summary>What the comparison looks at: glyph ids, clusters and positions, as the owner's plan asks.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable glyph tuples, or the reason there are none.</returns>
let comparable (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: Farfbuzz.GlyphInfo) (position: Farfbuzz.GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>A stable hash of a font file name: the .NET string hash is randomised per process, which would make the
/// random-text cases differ between runs and the ratchet flaky.</summary>
/// <param name="text">The name.</param>
/// <returns>A positive integer stable across processes and .NET versions.</returns>
let private stableHash (text: string) : int =
    let mutable hash = 5381
    for character in text do
        hash <- (hash * 33 + int character) &&& 0x7FFFFFFF
    hash

/// <summary>Random strings over a face's coverage, deterministic per face.</summary>
/// <param name="font">The face.</param>
/// <param name="covered">The code points it maps.</param>
/// <returns>The strings.</returns>
let strings (font: TestFont) (covered: uint32 array) : uint32 array array =
    match covered.Length with
    | 0 -> [||]
    | _ ->
        let text = gen {
            let! length = Gen.choose (1, 24)
            let! picks = Gen.arrayOfLength length (Gen.elements covered)
            return picks }
        let seed = Rnd(uint64 (stableHash (Path.GetFileName font.Path |> nonNull)) + uint64 font.Index)
        Gen.sampleWithSeed seed 24 StringsPerFace text

/// <summary>
/// Farfbuzz and HarfBuzz 15.1.0 shape the same random strings, drawn from each test face's own character map,
/// with default options; agreement on glyph ids, clusters and positions is reported and ratcheted like conformance.
/// </summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    testCase "P1.RandomText" (fun () ->
        let baselinePath = Path.Combine(RepositoryPaths.root (), "tests", "Farfbuzz.Diff", "random-text.baseline")
        let baseline = Ratchet.read baselinePath
        let cases =
            TestFonts.all ()
            |> List.collect (fun font ->
                strings font (coverage font)
                |> Array.toList
                |> List.mapi (fun number text ->
                    let request = { defaults font.Path text with FaceIndex = font.Index; Shaper = ValueSome "ot"; FontFuncs = ValueSome "ot" }
                    let key = $"{Path.GetFileName font.Path}:{font.Index}:{number}"
                    let farfbuzzComparable = comparable (farfbuzz.Shape request)
                    let agree = comparable (oracle.Shape request) = farfbuzzComparable
                    let outlines = outlinesNotPorted farfbuzzComparable |> ValueOption.isSome
                    key, agree, outlines))
        let agreeing = cases |> List.filter (fun (_, agree, _) -> agree) |> List.map (fun (key, _, _) -> key)
        let outlinesCount = cases |> List.filter (fun (_, _, outlines) -> outlines) |> List.length
        let regressions =
            cases
            |> List.filter (fun (key, agree, outlines) -> baseline.Contains key && not agree && not outlines)
            |> List.map (fun (key, _, _) -> key)
        let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "conformance")
        Directory.CreateDirectory directory |> ignore
        File.WriteAllLines(Path.Combine(directory, "random-text-agreeing.txt"), agreeing)
        Console.WriteLine $"Random text: Farfbuzz agrees with HarfBuzz on {agreeing.Length} of {cases.Length} strings over {TestFonts.all().Length} faces; {outlinesCount} report unported outlines."
        match regressions, agreeing with
        | [], [] -> skiptest $"0 of {cases.Length} random strings agree yet"
        | [], _ -> ()
        | lost, _ -> failtest ("random strings that agreed no longer do: " + String.Join(", ", lost |> List.truncate 20)))
