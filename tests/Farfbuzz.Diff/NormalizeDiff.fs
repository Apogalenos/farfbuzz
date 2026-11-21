module Farfbuzz.Diff.NormalizeDiff

open System
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.NoLayoutFonts
open Farfbuzz.Diff.ShapeGens

/// <summary>What the comparison looks at: glyph ids and clusters. S05 forms the clusters.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The glyph ids and clusters, or the reason there are none.</returns>
let private glyphsAndClusters (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) -> Ok (run.Infos |> Array.map (fun info -> struct (info.Codepoint, info.Cluster)))
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>Farfbuzz and HarfBuzz 15.1.0 normalise the same strings on stripped fonts; agreement on glyph ids and clusters.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    testList "P4.S03" [
        sampledWithOutlines "P4.S03 normalize via no-layout shaping"
            (request (all ()) WithMarks)
            5000
            (fun spec -> glyphsAndClusters (farfbuzz.Shape spec))
            (fun spec -> glyphsAndClusters (oracle.Shape spec))
            outlinesNotPorted
    ]
