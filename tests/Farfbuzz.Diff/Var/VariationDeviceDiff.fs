module Farfbuzz.Diff.VariationDeviceDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.Gens

/// <summary>The GDEF table tag.</summary>
let private gdefTag = Tag.make 'G' 'D' 'E' 'F'

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>The GDEF var-store offset: the Offset32 after markGlyphSetsDef, for version 1.3 and later.</summary>
/// <param name="table">The GDEF table.</param>
/// <returns>The store offset, or none.</returns>
let private gdefVarStoreOffset (table: ReadOnlyMemory<byte>) : int voption =
    match BinaryCalls.u16 table.Span 0 = 1us && BinaryCalls.u16 table.Span 2 >= 3us with
    | false -> ValueNone
    | true ->
        match int (BinaryCalls.u32 table.Span 14) with
        | 0 -> ValueNone
        | offset -> ValueSome offset

/// <summary>Sanitizes then reads a store at an offset of its parent table, empty on a zero offset or a failure.</summary>
/// <param name="table">The table bytes.</param>
/// <param name="at">The store offset.</param>
/// <returns>The store.</returns>
let private readStore (table: ReadOnlyMemory<byte>) (at: int) : ItemVariationStore =
    match at with
    | 0 -> ItemVariationStore.empty
    | _ ->
        match ItemVariationStore.sanitize table.Span at (Sanitize.start table.Length) with
        | ValueNone -> ItemVariationStore.empty
        | ValueSome _ -> ItemVariationStore.read table at

/// <summary>The GDEF store of a face, or none.</summary>
/// <param name="face">The face.</param>
/// <returns>The store.</returns>
let private gdefStore (face: Face) : ItemVariationStore voption =
    match Face.referenceTable face gdefTag with
    | table when table.Length = 0 -> ValueNone
    | table ->
        match gdefVarStoreOffset table with
        | ValueNone -> ValueNone
        | ValueSome at -> ValueSome (readStore table at)

/// <summary>Reads an offset field of a byte span, two or three bytes wide.</summary>
/// <param name="span">The bytes.</param>
/// <param name="at">The field offset.</param>
/// <param name="width">Two or three bytes.</param>
/// <returns>The field value.</returns>
let private readField (span: ReadOnlySpan<byte>) (at: int) (width: int) : uint32 =
    match width with
    | 3 -> BinaryCalls.u24 span at |> Farfbuzz.Harness.TailCalls.settle
    | _ -> uint32 (BinaryCalls.u16 span at) |> Farfbuzz.Harness.TailCalls.settle

/// <summary>The 16.16 variation indices of every format-3 caret device reachable from a GDEF ligature-caret list.</summary>
/// <param name="gdef">The GDEF.</param>
/// <returns>The distinct delta-set indices.</returns>
let private ligCaretVarIndices (gdef: Gdef) : uint32 array =
    let bytes = gdef.LigCarets
    match bytes.Length with
    | 0 -> [||]
    | _ ->
        let span = bytes.Span
        let result = ResizeArray<uint32>()
        let struct (countAt, offsetsAt, countWidth, offsetWidth) =
            match gdef.LigCaretsWide with
            | false -> struct (2, 4, 2, 2)
            | true -> struct (4, 7, 3, 3)
        let ligGlyphCount = int (readField span countAt countWidth)
        for ligGlyph in 0 .. ligGlyphCount - 1 do
            let ligGlyphOffset = int (readField span (offsetsAt + ligGlyph * offsetWidth) offsetWidth)
            let caretCount = int (BinaryCalls.u16 span ligGlyphOffset)
            for caret in 0 .. caretCount - 1 do
                let caretOffset = int (BinaryCalls.u16 span (ligGlyphOffset + 2 + caret * 2))
                let cv = ligGlyphOffset + caretOffset
                match BinaryCalls.u16 span cv with
                | 3us ->
                    let deviceOffset = int (BinaryCalls.u16 span (cv + 4))
                    let deviceAt = cv + deviceOffset
                    match BinaryCalls.u16 span (deviceAt + 4) with
                    | 0x8000us ->
                        result.Add ((uint32 (BinaryCalls.u16 span deviceAt) <<< 16) ||| uint32 (BinaryCalls.u16 span (deviceAt + 2)))
                    | _ -> ()
                | _ -> ()
        result.ToArray() |> Array.distinct

/// <summary>The P7.S13 variation-device delta differential.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let storeFaces =
        faces
        |> Seq.choose (fun struct (font, index) ->
            match gdefStore (faceOf font index) with
            | ValueSome store when store.Data.Length > 0 -> Some (struct (font, index, store))
            | _ -> None)
        |> Seq.toArray
    let deviceInputs =
        gen {
            let! struct (font, index, store) = Gen.elements storeFaces
            let face = faceOf font index
            let reachable = ligCaretVarIndices (Gdef.load face)
            let upem = face.Upem
            let! coords = coordsFor font index
            let! scale = Gen.elements [| struct (upem, upem); struct (1500, 1500); struct (-1000, -1000); struct (1000, 1500) |]
            let! varIdx =
                gen {
                    let! pick = Gen.choose (0, 2)
                    match pick, reachable.Length with
                    | 0, len when len > 0 ->
                        let! at = Gen.choose (0, len - 1)
                        return reachable[at]
                    | _ ->
                        let! outer = Gen.choose (0, store.Data.Length)
                        let! inner = Gen.choose (0, 0xFFFF)
                        return (uint32 outer <<< 16) ||| uint32 inner
                }
            return struct (font, index, coords, varIdx, scale)
        }
    testList "P7.S13" [
        sampled "P7.S13 device deltas" deviceInputs 5000
            (fun struct (font, index, coords, varIdx, scale) ->
                let struct (xScale, yScale) = scale
                let face = faceOf font index
                let farfbuzz = Font.setVarCoordsNormalized coords (Font.withScale xScale yScale (Font.create face))
                struct (VariationDevice.xDelta farfbuzz varIdx, VariationDevice.yDelta farfbuzz varIdx))
            (fun struct (font, index, coords, varIdx, scale) ->
                let struct (xScale, yScale) = scale
                let coordsArray = coords |> Seq.toArray
                let mutable x = 0
                let mutable y = 0
                Shims.farfbuzz_oracle_device_delta(OracleFaces.face font index, coordsArray, uint32 coordsArray.Length, varIdx, xScale, yScale, &x, &y)
                struct (x, y))
    ]
