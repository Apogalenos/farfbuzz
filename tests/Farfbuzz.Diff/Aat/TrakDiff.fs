module Farfbuzz.Diff.TrakDiff

open System
open FsCheck
open FsCheck.FSharp
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Every face whose trak table has data.</summary>
/// <returns>The faces.</returns>
let private trakFaces : struct (TestFont * uint32) array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) -> Trak.hasData (Trak.load (faceOf font index)))
    |> Seq.toArray

/// <summary>The fixed point sizes of the tracking differential; zero means no point size (the default 12).</summary>
let private fixedPtems : float32 array = [| 0f; 0.5f; 1f; 9f; 12f; 24f; 72f; 144f |]

/// <summary>Generates a point size: one of the fixed values, or a value in (0, 200).</summary>
/// <returns>The generator.</returns>
let private genPtem =
    gen {
        let! kind = Gen.choose (0, 8)
        match kind with
        | 8 ->
            let! whole = Gen.choose (0, 199)
            let! fraction = Gen.choose (0, 99)
            return float32 whole + float32 fraction / 100f
        | index -> return fixedPtems[index]
    }

/// <summary>One tracking case: a trak face, a direction and a point size.</summary>
type private TrakCase =
    { Font: TestFont
      Index: uint32
      Horizontal: bool
      Ptem: float32 }

/// <summary>Generates one tracking case.</summary>
/// <returns>The generator.</returns>
let private genCase =
    gen {
        let! struct (font, index) = Gen.elements trakFaces
        let! horizontal = Gen.elements [| true; false |]
        let! ptem = genPtem
        return { Font = font; Index = index; Horizontal = horizontal; Ptem = ptem }
    }

/// <summary>Farfbuzz's device-space tracking advance at a face, direction and point size.</summary>
/// <param name="case">The case.</param>
/// <returns>The advance.</returns>
let private ourTracking (case: TrakCase) : int =
    let font = Font.create (faceOf case.Font case.Index) |> Font.withPtem case.Ptem
    let ptem = if case.Ptem > 0f then case.Ptem else 12f
    let tracking = Trak.getTracking font.Ot.Trak.Value case.Horizontal ptem 0f
    if case.Horizontal then Font.emScalefX font tracking else Font.emScalefY font tracking

/// <summary>The oracle's device-space tracking advance at a face, direction and point size.</summary>
/// <param name="case">The case.</param>
/// <returns>The advance.</returns>
let private oracleTracking (case: TrakCase) : int =
    let face = OracleFaces.face case.Font case.Index
    Shims.farfbuzz_oracle_trak_tracking(face, (if case.Horizontal then 1u else 0u), case.Ptem)

/// <summary>The P8.S13 trak differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P8.S13" [
        exhaustive "has tracking" (OracleFaces.all ())
            (fun struct (font, index) -> Trak.hasData (Trak.load (faceOf font index)))
            (fun struct (font, index) -> Native.hb_aat_layout_has_tracking (OracleFaces.face font index) <> 0)
        sampled "tracking value" genCase 1000 ourTracking oracleTracking
    ]
