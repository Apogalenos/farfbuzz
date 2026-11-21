module Farfbuzz.Diff.NormalizeCoordsDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Oracle
open Farfbuzz.Harness
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.Gens

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Normalizes design coordinates through the oracle, <c>hb_ot_var_normalize_coords</c>.</summary>
/// <param name="face">The oracle face.</param>
/// <param name="design">The design coordinates.</param>
/// <returns>The normalized coordinates.</returns>
let private oracleNormalizeCoords (face: nativeint) (design: float32 array) : int array =
    let normalized = Array.zeroCreate design.Length
    Native.hb_ot_var_normalize_coords(face, uint32 design.Length, design, normalized)
    normalized

/// <summary>Normalizes a variation list through the oracle, <c>hb_ot_var_normalize_variations</c>.</summary>
/// <param name="face">The oracle face.</param>
/// <param name="variations">The variations.</param>
/// <returns>The normalized coordinates.</returns>
let private oracleNormalizeVariations (face: nativeint) (variations: ImmutableArray<Variation>) : int array =
    let coordsLength = int (Native.hb_ot_var_get_axis_count face)
    let coords = Array.zeroCreate coordsLength
    let varArray = variations |> Seq.toArray
    Native.hb_ot_var_normalize_variations(face, varArray, uint32 varArray.Length, coords, uint32 coordsLength)
    coords

/// <summary>The fvar design defaults of a face.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The defaults, one per axis.</returns>
let private designDefaults (font: TestFont) (index: uint32) : float32 array =
    let fvar = Fvar.load (faceOf font index)
    fvar.Axes |> Seq.map (fun axis -> axis.DefaultValue) |> Seq.toArray

/// <summary>The axis tags of a face, for the variation-tag generator.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The tags.</returns>
let private axisTags (font: TestFont) (index: uint32) : Tag array =
    let fvar = Fvar.load (faceOf font index)
    fvar.Axes |> Seq.map (fun axis -> axis.Tag) |> Seq.toArray

/// <summary>A variation whose tag is a face axis or a known/unknown tag.</summary>
/// <param name="tags">The face's axis tags.</param>
/// <returns>The generator.</returns>
let private variationGen (tags: Tag array) : Gen<Variation> =
    let extra : Tag array = [| 0x77676874u<tag>; 0x58585858u<tag>; Tags.Empty |]
    gen {
        let! tag = Gen.elements (Array.append tags extra)
        let! value = Gen.elements [| -1.0f; 0.0f; 0.25f; 0.5f; 1.0f; 100.0f; 400.0f; 900.0f; -100.0f |]
        return { Tag = tag; Value = value }
    }

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Reads a big-endian int16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readI16 (bytes: byte array) (at: int) =
    int16 (readU16 bytes at)

/// <summary>Reads a big-endian uint32 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU32 (bytes: byte array) (at: int) =
    (uint32 bytes[at] <<< 24) ||| (uint32 bytes[at + 1] <<< 16) ||| (uint32 bytes[at + 2] <<< 8) ||| uint32 bytes[at + 3]

/// <summary>Encodes a uint16 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let private be16 (value: uint16) : byte array =
    [| byte (uint32 value >>> 8); byte value |]

/// <summary>Encodes a uint32 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The four bytes.</returns>
let private be32 (value: uint32) : byte array =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>The replacement uint16 values for one mutated field, with wrapping arithmetic.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u16Replacements (value: uint16) : uint16 array =
    [| 0us; 1us; uint16 (int value - 1); uint16 (int value + 1); 0xFFFFus |]

/// <summary>The replacement int16 values for one mutated field, with wrapping arithmetic.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements, encoded as uint16.</returns>
let private i16Replacements (value: int16) : uint16 array =
    [| 0us; 1us; uint16 (int value - 1); uint16 (int value + 1); 0x8000us |]

/// <summary>One field mutation: the absolute byte offset, a label and the replacement bytes.</summary>
type private Mutation =
    { At: int
      Name: string
      Replacement: byte array }

/// <summary>The replacement uint32 values for one mutated offset field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements, with wrapped arithmetic.</returns>
let private u32Replacements (value: uint32) : uint32 array =
    [| 0u; 1u; value - 1u; value + 1u; 0xFFFFFFFFu |]

/// <summary>The replacement byte values for one mutated byte field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements, with wrapped arithmetic.</returns>
let private byteReplacements (value: byte) : byte array =
    [| 0uy; 1uy; byte (int value - 1); byte (int value + 1); 0xFFuy |]

/// <summary>The replacement word-size-count values, with and without the long-words bit.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private wordSizeCountReplacements (value: uint16) : uint16 array =
    [| 0us
       1us
       uint16 (int value - 1)
       uint16 (int value + 1)
       0xFFFFus
       value ||| 0x8000us
       value &&& 0x7FFFus
       value ^^^ 0x8000us |]

/// <summary>The design-coordinate probes of a face: defaults, axis ends and values between.</summary>
/// <param name="fvar">The fvar table.</param>
/// <returns>Sixty coordinate arrays.</returns>
let private designProbes (fvar: Fvar) : float32 array array =
    let axes = fvar.Axes
    [| for k in 0 .. 59 ->
           [| for axisIndex in 0 .. axes.Length - 1 ->
                  let axis = axes[axisIndex]
                  let range = axis.MaxValue - axis.MinValue
                  match k with
                  | 0 -> axis.DefaultValue
                  | 1 -> axis.MinValue
                  | 2 -> axis.MaxValue
                  | 3 -> (axis.MinValue + axis.DefaultValue) / 2.0f
                  | 4 -> (axis.DefaultValue + axis.MaxValue) / 2.0f
                  | 5 -> axis.MinValue - 1.0f
                  | 6 -> axis.MaxValue + 1.0f
                  | k ->
                      let fraction = float32 ((k + axisIndex * 17) % 53) / 52.0f
                      axis.MinValue + fraction * range |] |]

/// <summary>Every field mutation of an avar 2 table: version, tail offsets, and the map and store fields they point to.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="tableAt">The absolute offset of the avar table.</param>
/// <param name="tableLength">The avar table length.</param>
/// <param name="axisCount">The axis count.</param>
/// <returns>The mutations.</returns>
let private avar2Mutations (bytes: byte array) (tableAt: int) (tableLength: int) (axisCount: int) : Mutation list =
    let mutations = ResizeArray<Mutation>()
    let mutable mapOffset = tableAt + 8
    let mutable axis = 0
    let mutable walking = axis < axisCount && mapOffset + 2 <= tableAt + tableLength
    while walking do
        let count = int (readU16 bytes mapOffset)
        mapOffset <- mapOffset + 2 + count * 4
        axis <- axis + 1
        walking <- axis < axisCount && mapOffset + 2 <= tableAt + tableLength
    let tailAt = mapOffset
    match tailAt + 8 <= tableAt + tableLength with
    | false -> mutations |> Seq.toList
    | true ->
        for version in [| 0u; 0x00010000u; 0x00020000u; 0x00030000u |] do
            mutations.Add { At = tableAt; Name = "version"; Replacement = be32 version }
        let varIdxMapOffset = int (readU32 bytes tailAt)
        let varStoreOffset = int (readU32 bytes (tailAt + 4))
        for replacement in u32Replacements (uint32 varIdxMapOffset) do
            mutations.Add { At = tailAt; Name = "varIdxMap offset"; Replacement = be32 replacement }
        for replacement in u32Replacements (uint32 varStoreOffset) do
            mutations.Add { At = tailAt + 4; Name = "varStore offset"; Replacement = be32 replacement }
        match varIdxMapOffset with
        | 0 -> ()
        | _ ->
            let mapAt = tableAt + varIdxMapOffset
            match mapAt + 2 <= tableAt + tableLength with
            | false -> ()
            | true ->
                let format = bytes[mapAt]
                let entryFormat = bytes[mapAt + 1]
                for replacement in byteReplacements format do
                    mutations.Add { At = mapAt; Name = "varIdxMap format"; Replacement = [| replacement |] }
                for replacement in byteReplacements entryFormat do
                    mutations.Add { At = mapAt + 1; Name = "varIdxMap entryFormat"; Replacement = [| replacement |] }
                match format with
                | 0uy ->
                    match mapAt + 4 <= tableAt + tableLength with
                    | false -> ()
                    | true ->
                        for replacement in u16Replacements (readU16 bytes (mapAt + 2)) do
                            mutations.Add { At = mapAt + 2; Name = "varIdxMap mapCount"; Replacement = be16 replacement }
                | 1uy ->
                    match mapAt + 6 <= tableAt + tableLength with
                    | false -> ()
                    | true ->
                        for replacement in u32Replacements (readU32 bytes (mapAt + 2)) do
                            mutations.Add { At = mapAt + 2; Name = "varIdxMap mapCount"; Replacement = be32 replacement }
                | _ -> ()
        match varStoreOffset with
        | 0 -> ()
        | _ ->
            let storeAt = tableAt + varStoreOffset
            match storeAt + 8 <= tableAt + tableLength with
            | false -> ()
            | true ->
                let storeFormat = readU16 bytes storeAt
                for replacement in u16Replacements storeFormat do
                    mutations.Add { At = storeAt; Name = "store format"; Replacement = be16 replacement }
                for replacement in u32Replacements (readU32 bytes (storeAt + 2)) do
                    mutations.Add { At = storeAt + 2; Name = "store regionList offset"; Replacement = be32 replacement }
                let dataCount = int (readU16 bytes (storeAt + 6))
                for replacement in u16Replacements (readU16 bytes (storeAt + 6)) do
                    mutations.Add { At = storeAt + 6; Name = "store dataSet count"; Replacement = be16 replacement }
                let mutable dataSet = 0
                let mutable dataSetWalking = dataSet < dataCount && storeAt + 8 + (dataSet + 1) * 4 <= tableAt + tableLength
                while dataSetWalking do
                    let offsetAt = storeAt + 8 + dataSet * 4
                    let offset = readU32 bytes offsetAt
                    for replacement in u32Replacements offset do
                        mutations.Add { At = offsetAt; Name = $"store dataSet {dataSet} offset"; Replacement = be32 replacement }
                    match offset with
                    | 0u -> ()
                    | _ ->
                        let varDataAt = storeAt + int offset
                        match varDataAt + 6 <= tableAt + tableLength with
                        | false -> ()
                        | true ->
                            let itemCount = readU16 bytes varDataAt
                            let wordSizeCount = readU16 bytes (varDataAt + 2)
                            let regionIndexCount = readU16 bytes (varDataAt + 4)
                            for replacement in u16Replacements itemCount do
                                mutations.Add { At = varDataAt; Name = $"dataSet {dataSet} itemCount"; Replacement = be16 replacement }
                            for replacement in wordSizeCountReplacements wordSizeCount do
                                mutations.Add { At = varDataAt + 2; Name = $"dataSet {dataSet} wordSizeCount"; Replacement = be16 replacement }
                            for replacement in u16Replacements regionIndexCount do
                                mutations.Add { At = varDataAt + 4; Name = $"dataSet {dataSet} regionIndexCount"; Replacement = be16 replacement }
                    dataSet <- dataSet + 1
                    dataSetWalking <- dataSet < dataCount && storeAt + 8 + (dataSet + 1) * 4 <= tableAt + tableLength
        mutations |> Seq.toList

/// <summary>The P7.S02 normalization differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let fvarFaces =
        faces
        |> Seq.filter (fun struct (font, index) -> (Fvar.load (faceOf font index)).Axes.Length > 0)
        |> Seq.toArray
    let designInputs =
        gen {
            let! struct (font, index) = Gen.elements fvarFaces
            let! design = designFor font index
            return struct (font, index, design)
        }
    let variationInputs =
        gen {
            let! struct (font, index) = Gen.elements fvarFaces
            let tags = axisTags font index
            let! length = Gen.choose (0, 6)
            let! variations = Gen.arrayOfLength length (variationGen tags)
            return struct (font, index, ImmutableArray.CreateRange variations)
        }
    let avar2Fonts =
        TestFonts.extra
            [ "api/fonts/TestAvar2Instance.ttf"
              "api/fonts/feature-variation-precision-GPOS-legacy-avar2.ttf"
              "api/fonts/feature-variation-precision-GPOS-lookup-avar2.ttf"
              "api/fonts/feature-variation-precision-GSUB-legacy-avar2.ttf"
              "api/fonts/feature-variation-precision-GSUB-lookup-avar2.ttf"
              "api/fonts/varc-unrelated-axis-avar2.ttf"
              "subset/data/fonts/RobotoDelta-NullVarStore-VF.ttf"
              "subset/data/fonts/RobotoDelta-Roman-VF-subset.ttf" ]
        |> Seq.toArray
    let avar2Inputs =
        gen {
            let! font = Gen.elements avar2Fonts
            let! design = designFor font (uint32 font.Index)
            return struct (font, uint32 font.Index, design)
        }
    testList "NormalizeCoords" [
        testList "P7.S02" [
            sampled "P7.S02 normalize coords" designInputs 20000
                (fun struct (font, index, design) ->
                    let face = faceOf font index
                    let coords = NormalizeCoords.normalizeCoords (Fvar.load face) (Avar.load face) (design.AsSpan()) |> TailCalls.settle
                    coords |> Seq.toArray)
                (fun struct (font, index, design) ->
                    oracleNormalizeCoords (OracleFaces.face font index) (design |> Seq.toArray))
            sampled "P7.S02 normalize variations" variationInputs 10000
                (fun struct (font, index, variations) ->
                    let face = faceOf font index
                    let fvar = Fvar.load face
                    let coords = NormalizeCoords.normalizeVariations fvar (Avar.load face) (variations.AsSpan()) fvar.Axes.Length |> TailCalls.settle
                    coords |> Seq.toArray)
                (fun struct (font, index, variations) ->
                    oracleNormalizeVariations (OracleFaces.face font index) variations)
            exhaustive "P7.S02 default coords" faces
                (fun struct (font, index) ->
                    let design = designDefaults font index
                    let face = faceOf font index
                    let coords = NormalizeCoords.normalizeCoords (Fvar.load face) (Avar.load face) (ReadOnlySpan design) |> TailCalls.settle
                    coords |> Seq.toArray)
                (fun struct (font, index) ->
                    oracleNormalizeCoords (OracleFaces.face font index) (designDefaults font index))
            testCase "P7.S02 mutated avar" (fun () ->
                let failures = ResizeArray<string>()
                let mutable mutatedFaces = 0
                let mutable mutatedTables = 0
                for struct (font, index) in faces do
                    let bytes = OracleFaces.data font.Path
                    let face = Face.create (ReadOnlyMemory bytes) index
                    match Face.tryFindTable face Tags.Avar with
                    | ValueNone -> ()
                    | ValueSome record ->
                        let tableAt = int (face.BaseOffset + record.Offset)
                        let tableLength = min (int record.Length) (bytes.Length - tableAt)
                        match tableLength >= 8 with
                        | false -> ()
                        | true ->
                            mutatedFaces <- mutatedFaces + 1
                            let axisCount = int (readU16 bytes (tableAt + 6))
                            let fvar = Fvar.load face
                            let probes =
                                [| fvar.Axes |> Seq.map (fun axis -> axis.DefaultValue) |> Seq.toArray
                                   fvar.Axes |> Seq.map (fun axis -> axis.MinValue) |> Seq.toArray
                                   fvar.Axes |> Seq.map (fun axis -> axis.MaxValue) |> Seq.toArray |]
                            let mutations = ResizeArray<Mutation>()
                            for version in [| 0u; 0x00020000u; 0x00030000u; 0x00010001u |] do
                                mutations.Add { At = tableAt + 0; Name = "version"; Replacement = be32 version }
                            for reserved in [| 0us; 1us; 0xFFFFus |] do
                                mutations.Add { At = tableAt + 4; Name = "reserved"; Replacement = be16 reserved }
                            for count in u16Replacements (uint16 axisCount) do
                                mutations.Add { At = tableAt + 6; Name = "axisCount"; Replacement = be16 count }
                            let mutable mapOffset = tableAt + 8
                            let mutable axis = 0
                            let mutable walking = axis < axisCount && mapOffset + 2 <= tableAt + tableLength
                            while walking do
                                let count = int (readU16 bytes mapOffset)
                                for replacement in u16Replacements (uint16 count) do
                                    mutations.Add { At = mapOffset; Name = $"segment {axis} count"; Replacement = be16 replacement }
                                let mutable recordIndex = 0
                                let mutable reading = recordIndex < count && mapOffset + 2 + (recordIndex + 1) * 4 <= tableAt + tableLength
                                while reading do
                                    let recordAt = mapOffset + 2 + recordIndex * 4
                                    let fromValue = readI16 bytes recordAt
                                    let toValue = readI16 bytes (recordAt + 2)
                                    for replacement in i16Replacements fromValue do
                                        mutations.Add { At = recordAt; Name = $"segment {axis} from {recordIndex}"; Replacement = be16 replacement }
                                    for replacement in i16Replacements toValue do
                                        mutations.Add { At = recordAt + 2; Name = $"segment {axis} to {recordIndex}"; Replacement = be16 replacement }
                                    recordIndex <- recordIndex + 1
                                    reading <- recordIndex < count && mapOffset + 2 + (recordIndex + 1) * 4 <= tableAt + tableLength
                                mapOffset <- mapOffset + 2 + count * 4
                                axis <- axis + 1
                                walking <- axis < axisCount && mapOffset + 2 <= tableAt + tableLength
                            for mutation in mutations do
                                mutatedTables <- mutatedTables + 1
                                let mutated = Array.copy bytes
                                for b in 0 .. mutation.Replacement.Length - 1 do
                                    mutated[mutation.At + b] <- mutation.Replacement[b]
                                let mutatedFace = Face.create (ReadOnlyMemory mutated) index
                                let avar = Avar.load mutatedFace
                                let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                                let oracleFace = Native.hb_face_create(blob, index)
                                for probe in probes do
                                    let ours = NormalizeCoords.normalizeCoords fvar avar (ReadOnlySpan probe) |> TailCalls.settle |> Seq.toArray
                                    let oracle = oracleNormalizeCoords oracleFace probe
                                    match ours = oracle with
                                    | true -> ()
                                    | false -> failures.Add($"{font.Path}#{index} {mutation.Name}: oracle %A{oracle}, farfbuzz %A{ours}")
                                Native.hb_face_destroy oracleFace
                                Native.hb_blob_destroy blob
                printfn "P7.S02 compared %d mutated avar faces and %d mutated tables" mutatedFaces mutatedTables
                Expect.isEmpty failures ("mutated-avar differences:\n" + String.concat "\n" failures))
        ]
        testList "P7.S04" [
            sampled "P7.S04 avar2 normalize" avar2Inputs 5000
                (fun struct (font, index, design) ->
                    let face = faceOf font index
                    let coords = NormalizeCoords.normalizeCoords (Fvar.load face) (Avar.load face) (design.AsSpan()) |> TailCalls.settle
                    coords |> Seq.toArray)
                (fun struct (font, index, design) ->
                    oracleNormalizeCoords (OracleFaces.face font index) (design |> Seq.toArray))
            testCase "P7.S04 mutated avar2" (fun () ->
                let failures = ResizeArray<string>()
                let mutable mutatedFaces = 0
                let mutable mutatedTables = 0
                for font in avar2Fonts do
                    let index = uint32 font.Index
                    let bytes = OracleFaces.data font.Path
                    let face = Face.create (ReadOnlyMemory bytes) index
                    match Face.tryFindTable face Tags.Avar with
                    | ValueNone -> ()
                    | ValueSome record ->
                        let tableAt = int (face.BaseOffset + record.Offset)
                        let tableLength = min (int record.Length) (bytes.Length - tableAt)
                        match tableLength >= 8 with
                        | false -> ()
                        | true ->
                            let axisCount = int (readU16 bytes (tableAt + 6))
                            let fvar = Fvar.load face
                            let probes = designProbes fvar
                            let mutations = avar2Mutations bytes tableAt tableLength axisCount
                            match mutations with
                            | [] -> ()
                            | _ ->
                                mutatedFaces <- mutatedFaces + 1
                                for mutation in mutations do
                                    mutatedTables <- mutatedTables + 1
                                    let mutated = Array.copy bytes
                                    for b in 0 .. mutation.Replacement.Length - 1 do
                                        mutated[mutation.At + b] <- mutation.Replacement[b]
                                    let mutatedFace = Face.create (ReadOnlyMemory mutated) index
                                    let avar = Avar.load mutatedFace
                                    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                                    let oracleFace = Native.hb_face_create(blob, index)
                                    for probe in probes do
                                        let ours = NormalizeCoords.normalizeCoords fvar avar (ReadOnlySpan probe) |> TailCalls.settle |> Seq.toArray
                                        let oracle = oracleNormalizeCoords oracleFace probe
                                        match ours = oracle with
                                        | true -> ()
                                        | false ->
                                            let replacementHex = mutation.Replacement |> Array.map (fun b -> b.ToString "X2") |> String.concat ""
                                            failures.Add($"{font.Path}#{index} {mutation.Name} -> {replacementHex}: oracle %A{oracle}, farfbuzz %A{ours}")
                                    Native.hb_face_destroy oracleFace
                                    Native.hb_blob_destroy blob
                printfn "P7.S04 compared %d mutated avar2 faces and %d mutated tables" mutatedFaces mutatedTables
                Expect.isEmpty failures ("mutated-avar2 differences:\n" + String.concat "\n" failures))
        ]
    ]
