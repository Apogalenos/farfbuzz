module Farfbuzz.Diff.FontVariationsDiff

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index, the high 16 bits carrying a named instance.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Creates the default Farfbuzz font for a face.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The font.</returns>
let private ourFont (font: TestFont) (index: uint32) =
    Font.create (faceOf font index)

/// <summary>The canonical bits of a design coordinate, NaN normalised so that NaN equals NaN.</summary>
/// <param name="value">The float32 coordinate.</param>
/// <returns>The bits.</returns>
let private designBits (value: float32) =
    match Single.IsNaN value with
    | true -> 0x7FC00000u
    | false -> BitConverter.SingleToUInt32Bits value

/// <summary>Turns a state into a value structural equality compares: ints, and design floats as bits.</summary>
/// <param name="normalized">The normalized coordinates.</param>
/// <param name="design">The design coordinates.</param>
/// <returns>The comparable pair.</returns>
let private toComparable (normalized: int array) (design: float32 array) =
    struct (normalized, Array.map designBits design)

/// <summary>Reads the normalized and design coordinates of a Farfbuzz font as a comparable pair.</summary>
/// <param name="font">The font.</param>
/// <returns>The comparable pair.</returns>
let private ourComparable (font: Font) =
    toComparable (Font.varCoordsNormalized font |> Seq.toArray) (Font.varCoordsDesign font |> Seq.toArray)

/// <summary>Reads the normalized and design coordinates of an oracle font as a comparable pair.</summary>
/// <param name="handle">The oracle font.</param>
/// <returns>The comparable pair.</returns>
let private readOracleState (handle: nativeint) =
    let mutable length = 0u
    let normalizedPointer = Native.hb_font_get_var_coords_normalized(handle, &length)
    let normalized : int array = Array.zeroCreate (int length)
    match int length with
    | 0 -> ()
    | count -> Marshal.Copy(normalizedPointer, normalized, 0, count)
    let mutable designLength = 0u
    let designPointer = Native.hb_font_get_var_coords_design(handle, &designLength)
    let design : float32 array = Array.zeroCreate (int designLength)
    match int designLength with
    | 0 -> ()
    | count -> Marshal.Copy(designPointer, design, 0, count)
    toComparable normalized design

/// <summary>Creates a fresh oracle font on a face, applies a setter and reads its state.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="set">The setter to apply to the fresh font.</param>
/// <returns>The comparable pair.</returns>
let private oracleComparable (font: TestFont) (index: uint32) (set: nativeint -> unit) =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    set handle
    let result = readOracleState handle
    Native.hb_font_destroy handle
    result

/// <summary>Creates a fresh oracle face at a face index carrying a named instance, then reads its font state.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index with the named instance in the high 16 bits.</param>
/// <returns>The comparable pair.</returns>
let private oracleFaceIndexComparable (font: TestFont) (index: uint32) =
    let face = Native.hb_face_create(OracleFaces.blob font.Path, index)
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    let result = readOracleState handle
    Native.hb_font_destroy handle
    Native.hb_face_destroy face
    result

/// <summary>The axis tags of a face.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The tags.</returns>
let private axisTags (font: TestFont) (index: uint32) : Tag array =
    let fvar = Fvar.load (faceOf font index)
    fvar.Axes |> Seq.map (fun axis -> axis.Tag) |> Seq.toArray

/// <summary>A variation whose tag is a face axis or a known or unknown tag.</summary>
/// <param name="tags">The face's axis tags.</param>
/// <returns>The generator.</returns>
let private variationGen (tags: Tag array) : Gen<Variation> =
    let extra : Tag array = [| 0x77676874u<tag>; 0x58585858u<tag>; Tags.Empty |]
    gen {
        let! tag = Gen.elements (Array.append tags extra)
        let! value = Gen.elements [| -1.0f; 0.0f; 0.25f; 0.5f; 1.0f; 100.0f; 400.0f; 900.0f; -100.0f |]
        return { Tag = tag; Value = value }
    }

/// <summary>The interesting design coordinates of a face's axes.</summary>
/// <param name="fvar">The fvar table.</param>
/// <returns>The candidate values.</returns>
let private designCandidates (fvar: Fvar) : float32 array =
    [| for axis in fvar.Axes do
           yield axis.MinValue
           yield axis.DefaultValue
           yield axis.MaxValue
           yield (axis.MinValue + axis.DefaultValue) / 2.0f
           yield (axis.DefaultValue + axis.MaxValue) / 2.0f
           yield axis.MinValue - 1.0f
           yield axis.MaxValue + 1.0f |]

/// <summary>A variation setting every axis of a face to its midpoint.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The variations.</returns>
let private representativeVariations (font: TestFont) (index: uint32) : Variation array =
    let fvar = Fvar.load (faceOf font index)
    [| for axis in fvar.Axes -> { Tag = axis.Tag; Value = (axis.MinValue + axis.MaxValue) / 2.0f } |]

/// <summary>The P7.S05 font variation state differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let fvarFaces =
        faces
        |> Seq.filter (fun struct (font, index) -> (Fvar.load (faceOf font index)).Axes.Length > 0)
        |> Seq.toArray
    let instanceFaces =
        fvarFaces
        |> Seq.filter (fun struct (font, index) -> (Fvar.load (faceOf font index)).InstanceCount > 0)
        |> Seq.toArray
    let variationInputs =
        gen {
            let! struct (font, index) = Gen.elements fvarFaces
            let tags = axisTags font index
            let! length = Gen.choose (0, 6)
            let! variations = Gen.arrayOfLength length (variationGen tags)
            return struct (font, index, ImmutableArray.CreateRange variations)
        }
    let coordsInputs =
        gen {
            let! struct (font, index) = Gen.elements fvarFaces
            let fvar = Fvar.load (faceOf font index)
            let axisCount = fvar.Axes.Length
            let! length = Gen.choose (0, axisCount + 2)
            let! useNormalized = Gen.elements [ true; false ]
            match useNormalized with
            | true ->
                let! values = Gen.arrayOfLength length (Gen.elements [| 0; 1; -1; 16384; -16384; 16383; -16383; 100000; -100000 |])
                return struct (font, index, true, ImmutableArray.CreateRange values, ImmutableArray<float32>.Empty)
            | false ->
                let! values = Gen.arrayOfLength length (Gen.elements (designCandidates fvar))
                return struct (font, index, false, ImmutableArray<int>.Empty, ImmutableArray.CreateRange values)
        }
    testList "P7.S05" [
        sampled "P7.S05 set variations" variationInputs 10000
            (fun struct (font, index, variations) ->
                ourComparable (Font.setVariations variations (ourFont font index)))
            (fun struct (font, index, variations) ->
                let array = variations |> Seq.toArray
                oracleComparable font index (fun handle -> Native.hb_font_set_variations(handle, array, uint32 array.Length)))
        testCase "P7.S05 named instances" (fun () ->
            let failures = ResizeArray<string>()
            let mutable checkedPairs = 0
            for struct (font, index) in instanceFaces do
                let count = int (Native.hb_ot_var_get_named_instance_count (OracleFaces.face font index))
                for instance in 0 .. count do
                    checkedPairs <- checkedPairs + 1
                    let instance = uint32 instance
                    let ours = ourComparable (Font.setVarNamedInstance instance (ourFont font index))
                    let oracle = oracleComparable font index (fun handle -> Native.hb_font_set_var_named_instance(handle, instance))
                    match ours = oracle with
                    | true -> ()
                    | false -> failures.Add($"{font.Path}#{index} instance {instance}: oracle %A{oracle}, farfbuzz %A{ours}")
                let variations = representativeVariations font index
                checkedPairs <- checkedPairs + 1
                let ours =
                    ourComparable (Font.setVariations (ImmutableArray.CreateRange variations) (Font.setVarNamedInstance 0u (ourFont font index)))
                let oracle =
                    oracleComparable font index (fun handle ->
                        Native.hb_font_set_var_named_instance(handle, 0u)
                        Native.hb_font_set_variations(handle, variations, uint32 variations.Length))
                match ours = oracle with
                | true -> ()
                | false -> failures.Add($"{font.Path}#{index} instance then variation: oracle %A{oracle}, farfbuzz %A{ours}")
            printfn "P7.S05 compared %d named-instance pairs" checkedPairs
            Expect.isEmpty failures ("named-instance differences:\n" + String.concat "\n" failures))
        testCase "P7.S05 face index instance" (fun () ->
            let failures = ResizeArray<string>()
            let mutable checkedPairs = 0
            for struct (font, index) in instanceFaces do
                let count = (Fvar.load (faceOf font index)).InstanceCount
                for i in 0 .. count - 1 do
                    checkedPairs <- checkedPairs + 1
                    let faceIndex = (uint32 (i + 1) <<< 16) ||| index
                    let ours = ourComparable (Font.create (faceOf font faceIndex))
                    let oracle = oracleFaceIndexComparable font faceIndex
                    match ours = oracle with
                    | true -> ()
                    | false -> failures.Add($"{font.Path}#{index} face index {faceIndex}: oracle %A{oracle}, farfbuzz %A{ours}")
            printfn "P7.S05 compared %d face-index instances" checkedPairs
            Expect.isEmpty failures ("face-index instance differences:\n" + String.concat "\n" failures))
        sampled "P7.S05 set coords" coordsInputs 5000
            (fun struct (font, index, useNormalized, ints, design) ->
                match useNormalized with
                | true -> ourComparable (Font.setVarCoordsNormalized ints (ourFont font index))
                | false -> ourComparable (Font.setVarCoordsDesign design (ourFont font index)))
            (fun struct (font, index, useNormalized, ints, design) ->
                let intArray = ints |> Seq.toArray
                let designArray = design |> Seq.toArray
                match useNormalized with
                | true -> oracleComparable font index (fun handle -> Native.hb_font_set_var_coords_normalized(handle, intArray, uint32 intArray.Length))
                | false -> oracleComparable font index (fun handle -> Native.hb_font_set_var_coords_design(handle, designArray, uint32 designArray.Length)))
    ]
