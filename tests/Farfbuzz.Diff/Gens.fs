module Farfbuzz.Diff.Gens

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>The avar <c>from</c> values of one axis, denormalized back into design space.</summary>
/// <param name="avar">The avar table.</param>
/// <param name="axis">The axis index.</param>
/// <returns>The design-space coordinates of every segment boundary.</returns>
let private avarFromDesigns (avar: Avar) (axis: int) : float32 list =
    match axis < avar.SegmentMaps.Length with
    | false -> []
    | true ->
        avar.SegmentMaps[axis]
        |> Seq.map (fun (struct (from, _)) -> from)
        |> Seq.toList

/// <summary>Maps one normalized coordinate back into an axis' design space.</summary>
/// <param name="axis">The axis.</param>
/// <param name="from">The normalized coordinate.</param>
/// <returns>The design coordinate.</returns>
let private denormalize (axis: AxisInfo) (from: float32) =
    match from < 0.0f with
    | true -> axis.DefaultValue + from * (axis.DefaultValue - axis.MinValue)
    | false -> axis.DefaultValue + from * (axis.MaxValue - axis.DefaultValue)

/// <summary>Generates one design coordinate for an axis, favouring the interesting points.</summary>
/// <param name="axis">The axis.</param>
/// <param name="extra">Extra candidate coordinates, for example the avar boundaries.</param>
/// <returns>The generator.</returns>
let private designValue (axis: AxisInfo) (extra: float32 list) : Gen<float32> =
    let range = axis.MaxValue - axis.MinValue
    let quarter = range / 4.0f
    let candidates =
        [ axis.MinValue
          axis.DefaultValue
          axis.MaxValue
          axis.MinValue - 1.0f
          axis.MaxValue + 1.0f
          (axis.MinValue + axis.DefaultValue) / 2.0f
          (axis.DefaultValue + axis.MaxValue) / 2.0f ]
        @ List.map (denormalize axis) extra
    gen {
        let! pick = Gen.choose (0, candidates.Length)
        match pick = candidates.Length with
        | true ->
            let lo = axis.MinValue - quarter
            let hi = axis.MaxValue + quarter
            let! fraction = Gen.choose (0, 1_000_000) |> Gen.map (fun n -> float32 n / 1_000_000.0f)
            return lo + fraction * (hi - lo)
        | false -> return candidates[pick]
    }

/// <summary>Sequences a list of generators into a generator of a list.</summary>
/// <param name="gens">The generators.</param>
/// <returns>The generator.</returns>
let rec private sequenceGens (gens: Gen<'T> list) : Gen<'T list> =
    match gens with
    | [] -> gen { return [] }
    | first :: rest ->
        gen {
            let! head = first
            let! tail = sequenceGens rest
            return head :: tail
        }

/// <summary>Generates design coordinates for every axis of a face.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The generator.</returns>
let designFor (font: TestFont) (index: uint32) : Gen<ImmutableArray<float32>> =
    let face = faceOf font index
    let fvar = Fvar.load face
    let avar = Avar.load face
    gen {
        let! values =
            fvar.Axes
            |> Seq.map (fun axis -> designValue axis (avarFromDesigns avar axis.AxisIndex))
            |> Seq.toList
            |> sequenceGens
        return ImmutableArray.CreateRange (List.toArray values)
    }

/// <summary>The normalized-coordinate boundaries the coordinate generator favours.</summary>
let private coordBoundaries : int array = [| 0; 1; -1; 16384; -16384; 16383; -16383 |]

/// <summary>One normalized 2.14 coordinate favouring the boundaries.</summary>
let private coordValue : Gen<int> =
    gen {
        let! pick = Gen.choose (0, coordBoundaries.Length)
        match pick = coordBoundaries.Length with
        | true -> return! Gen.choose (-16384, 16384)
        | false -> return coordBoundaries[pick]
    }

/// <summary>Generates normalized 2.14 coordinates for every axis of a face.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The generator.</returns>
let coordsFor (font: TestFont) (index: uint32) : Gen<ImmutableArray<int>> =
    let face = faceOf font index
    let fvar = Fvar.load face
    gen {
        let! values =
            fvar.Axes
            |> Seq.map (fun _ -> coordValue)
            |> Seq.toList
            |> sequenceGens
        return ImmutableArray.CreateRange (List.toArray values)
    }
