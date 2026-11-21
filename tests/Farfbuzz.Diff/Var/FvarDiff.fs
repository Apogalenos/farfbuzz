module Farfbuzz.Diff.FvarDiff

open System
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Reads every axis info of a face through the oracle.</summary>
/// <param name="face">The oracle face.</param>
/// <returns>The axis infos.</returns>
let private oracleAxisInfos (face: nativeint) : Native.OracleVarAxisInfo array =
    let mutable count = 0u
    let total = Native.hb_ot_var_get_axis_infos(face, 0u, &count, Array.empty)
    let infos = Array.zeroCreate (int total)
    let mutable written = total
    Native.hb_ot_var_get_axis_infos(face, 0u, &written, infos) |> ignore
    infos

/// <summary>An axis info as bits, so the floats compare bit for bit.</summary>
/// <param name="info">The axis info.</param>
/// <returns>The fields and float bits.</returns>
let private axisBits (info: AxisInfo) =
    struct (uint32 info.AxisIndex, uint32 info.Tag, uint32 info.NameId, uint32 info.Flags,
            BitConverter.SingleToUInt32Bits info.MinValue,
            BitConverter.SingleToUInt32Bits info.DefaultValue,
            BitConverter.SingleToUInt32Bits info.MaxValue)

/// <summary>An oracle axis info as bits, so the floats compare bit for bit.</summary>
/// <param name="info">The oracle axis info.</param>
/// <returns>The fields and float bits.</returns>
let private oracleAxisBits (info: Native.OracleVarAxisInfo) =
    struct (info.AxisIndex, info.Tag, info.NameId, info.Flags,
            BitConverter.SingleToUInt32Bits info.MinValue,
            BitConverter.SingleToUInt32Bits info.DefaultValue,
            BitConverter.SingleToUInt32Bits info.MaxValue)

/// <summary>Reads a named instance's design coordinates through the oracle at a given capacity.</summary>
/// <param name="face">The oracle face.</param>
/// <param name="index">The instance index.</param>
/// <param name="capacity">The coordinate capacity.</param>
/// <returns>The coordinates written.</returns>
let private oracleInstanceCoords (face: nativeint) (index: uint32) (capacity: int) : float32 array =
    let coords = Array.zeroCreate capacity
    let mutable written = uint32 capacity
    Native.hb_ot_var_named_instance_get_design_coords(face, index, &written, coords) |> ignore
    match written with
    | 0u -> [||]
    | count -> coords[.. int count - 1]

/// <summary>The fvar state Farfbuzz reads: has-data, axes, instances and coordinates.</summary>
/// <param name="face">The Farfbuzz face.</param>
/// <returns>The state, as a comparable value.</returns>
let private oursFvarState face =
    let fvar = Fvar.load face
    struct (fvar.HasData,
            fvar.Axes.Length,
            fvar.Axes |> Seq.map axisBits |> Seq.toArray,
            fvar.InstanceCount,
            [| for i in 0 .. fvar.InstanceCount ->
                   struct (uint32 (Fvar.instanceSubfamilyNameId i fvar),
                           uint32 (Fvar.instancePostscriptNameId i fvar),
                           Fvar.instanceCoords i fvar |> Seq.toArray) |])

/// <summary>The fvar state the oracle reads: has-data, axes, instances and coordinates.</summary>
/// <param name="face">The oracle face.</param>
/// <returns>The state, as a comparable value.</returns>
let private oracleFvarState face =
    let axisCount = int (Native.hb_ot_var_get_axis_count face)
    let instanceCount = int (Native.hb_ot_var_get_named_instance_count face)
    struct (Native.hb_ot_var_has_data face <> 0,
            axisCount,
            oracleAxisInfos face |> Array.map oracleAxisBits,
            instanceCount,
            [| for i in 0 .. instanceCount ->
                   struct (Native.hb_ot_var_named_instance_get_subfamily_name_id(face, uint32 i),
                           Native.hb_ot_var_named_instance_get_postscript_name_id(face, uint32 i),
                           oracleInstanceCoords face (uint32 i) axisCount) |])

/// <summary>The six fvar header fields, as their byte offset and a label.</summary>
let private headerFields : (int * string) array =
    [| 0, "major"; 4, "firstAxis"; 8, "axisCount"; 10, "axisSize"; 12, "instanceCount"; 14, "instanceSize" |]

/// <summary>The five replacement values for one uint16 header field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements, with wrapped arithmetic.</returns>
let private replacements value =
    [| 0us; 1us; uint16 (int value - 1); uint16 (int value + 1); 0xFFFFus |]

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Writes a big-endian uint16 into bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="value">The value.</param>
let private writeU16 (bytes: byte array) (at: int) (value: uint16) =
    bytes[at] <- byte (uint32 value >>> 8)
    bytes[at + 1] <- byte value

/// <summary>The P7.S01 fvar differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let extraTags : Tag array =
        [| 0x77676874u<tag>; 0x77647468u<tag>; 0x6F70737Au<tag>; 0x736C6E74u<tag>; 0x6974616Cu<tag>; 0x58585858u<tag> |]
    let findAxisInputs =
        faces
        |> Seq.collect (fun struct (font, index) ->
            let fvar = Fvar.load (faceOf font index)
            Seq.append (fvar.Axes |> Seq.map (fun axis -> axis.Tag)) extraTags
            |> Seq.map (fun tag -> struct (font, index, tag)))
    let instanceInputs =
        faces
        |> Seq.collect (fun struct (font, index) ->
            let count = int (Native.hb_ot_var_get_named_instance_count (OracleFaces.face font index))
            [ for i in 0 .. count -> struct (font, index, uint32 i) ])
    testList "Fvar" [
        testList "P7.S01" [
            exhaustive "P7.S01 axis count" faces
                (fun struct (font, index) -> (Fvar.load (faceOf font index)).Axes.Length)
                (fun struct (font, index) -> int (Native.hb_ot_var_get_axis_count (OracleFaces.face font index)))
            exhaustive "P7.S01 axis infos" faces
                (fun struct (font, index) ->
                    let fvar = Fvar.load (faceOf font index)
                    fvar.Axes |> Seq.map axisBits |> Seq.toArray)
                (fun struct (font, index) ->
                    oracleAxisInfos (OracleFaces.face font index) |> Array.map oracleAxisBits)
            exhaustive "P7.S01 find axis info" findAxisInputs
                (fun struct (font, index, tag) ->
                    match Fvar.findAxisInfo tag (Fvar.load (faceOf font index)) with
                    | ValueSome info -> ValueSome (axisBits info)
                    | ValueNone -> ValueNone)
                (fun struct (font, index, tag) ->
                    let mutable info = Unchecked.defaultof<Native.OracleVarAxisInfo>
                    match Native.hb_ot_var_find_axis_info(OracleFaces.face font index, uint32 tag, &info) with
                    | 0 -> ValueNone
                    | _ -> ValueSome (oracleAxisBits info))
            exhaustive "P7.S01 instance names" instanceInputs
                (fun struct (font, index, i) ->
                    let fvar = Fvar.load (faceOf font index)
                    struct (uint32 (Fvar.instanceSubfamilyNameId (int i) fvar),
                            uint32 (Fvar.instancePostscriptNameId (int i) fvar)))
                (fun struct (font, index, i) ->
                    let face = OracleFaces.face font index
                    struct (Native.hb_ot_var_named_instance_get_subfamily_name_id(face, i),
                            Native.hb_ot_var_named_instance_get_postscript_name_id(face, i)))
            exhaustive "P7.S01 instance coords" instanceInputs
                (fun struct (font, index, i) ->
                    let fvar = Fvar.load (faceOf font index)
                    let full = Fvar.instanceCoords (int i) fvar |> Seq.toArray
                    let first = if full.Length = 0 then [||] else [| full[0] |]
                    struct (full, first))
                (fun struct (font, index, i) ->
                    let face = OracleFaces.face font index
                    let axisCount = int (Native.hb_ot_var_get_axis_count face)
                    struct (oracleInstanceCoords face i axisCount, oracleInstanceCoords face i 1))
            exhaustive "P7.S01 has data" faces
                (fun struct (font, index) -> (Fvar.load (faceOf font index)).HasData)
                (fun struct (font, index) -> Native.hb_ot_var_has_data (OracleFaces.face font index) <> 0)
            testCase "P7.S01 mutated fvar" (fun () ->
                let failures = ResizeArray<string>()
                let mutable mutatedFaces = 0
                let mutable mutatedTables = 0
                for struct (font, index) in faces do
                    let bytes = OracleFaces.data font.Path
                    let face = Face.create (ReadOnlyMemory bytes) index
                    match Face.tryFindTable face Tags.Fvar with
                    | ValueNone -> ()
                    | ValueSome record ->
                        let fvarOffset = int (face.BaseOffset + record.Offset)
                        let fvarLength = min (int record.Length) (bytes.Length - fvarOffset)
                        match fvarLength >= 16 with
                        | false -> ()
                        | true ->
                            mutatedFaces <- mutatedFaces + 1
                            for fieldOffset, fieldName in headerFields do
                                let original = readU16 bytes (fvarOffset + fieldOffset)
                                for replacement in replacements original do
                                    let mutated = Array.copy bytes
                                    writeU16 mutated (fvarOffset + fieldOffset) replacement
                                    mutatedTables <- mutatedTables + 1
                                    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                                    let oracleFace = Native.hb_face_create(blob, index)
                                    let ours = oursFvarState (Face.create (ReadOnlyMemory mutated) index)
                                    let oracle = oracleFvarState oracleFace
                                    Native.hb_face_destroy oracleFace
                                    Native.hb_blob_destroy blob
                                    match ours = oracle with
                                    | true -> ()
                                    | false -> failures.Add($"{font.Path}#{index} {fieldName}={replacement}: oracle %A{oracle}, farfbuzz %A{ours}")
                printfn "P7.S01 compared %d mutated fvar faces and %d mutated tables" mutatedFaces mutatedTables
                Expect.isEmpty failures ("mutated-fvar differences:\n" + String.concat "\n" failures))
        ]
    ]
