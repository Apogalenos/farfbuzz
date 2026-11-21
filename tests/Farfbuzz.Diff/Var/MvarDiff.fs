module Farfbuzz.Diff.MvarDiff

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

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Every public metric tag, <c>hb_ot_metrics_tag_t</c> in declaration order.</summary>
let private metricTags : Tag array =
    [| Tag.make 'h' 'a' 's' 'c'
       Tag.make 'h' 'd' 's' 'c'
       Tag.make 'h' 'l' 'g' 'p'
       Tag.make 'h' 'c' 'l' 'a'
       Tag.make 'h' 'c' 'l' 'd'
       Tag.make 'v' 'a' 's' 'c'
       Tag.make 'v' 'd' 's' 'c'
       Tag.make 'v' 'l' 'g' 'p'
       Tag.make 'h' 'c' 'r' 's'
       Tag.make 'h' 'c' 'r' 'n'
       Tag.make 'h' 'c' 'o' 'f'
       Tag.make 'v' 'c' 'r' 's'
       Tag.make 'v' 'c' 'r' 'n'
       Tag.make 'v' 'c' 'o' 'f'
       Tag.make 'x' 'h' 'g' 't'
       Tag.make 'c' 'p' 'h' 't'
       Tag.make 's' 'b' 'x' 's'
       Tag.make 's' 'b' 'y' 's'
       Tag.make 's' 'b' 'x' 'o'
       Tag.make 's' 'b' 'y' 'o'
       Tag.make 's' 'p' 'x' 's'
       Tag.make 's' 'p' 'y' 's'
       Tag.make 's' 'p' 'x' 'o'
       Tag.make 's' 'p' 'y' 'o'
       Tag.make 's' 't' 'r' 's'
       Tag.make 's' 't' 'r' 'o'
       Tag.make 'u' 'n' 'd' 's'
       Tag.make 'u' 'n' 'd' 'o' |]

/// <summary>The MVAR delta of a metric tag at normalized coordinates, read from the face's own MVAR table.</summary>
/// <param name="face">The face.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="tag">The metric tag.</param>
/// <returns>The delta bits.</returns>
let private ourVarBits (face: Face) (coords: ImmutableArray<int>) (tag: Tag) : uint32 =
    let mvar = Mvar.load face
    let instance = ItemVariationStore.instantiate (NormalizedCoords.ofSealed coords) mvar.Store
    BitConverter.SingleToUInt32Bits (Mvar.getVar mvar instance tag)

/// <summary>Reads the MVAR delta of a metric tag through the oracle shim, as bits.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="tag">The metric tag.</param>
/// <returns>The delta bits.</returns>
let private oracleVarBits (font: TestFont) (index: uint32) (coords: int array) (tag: Tag) : uint32 =
    BitConverter.SingleToUInt32Bits (Shims.farfbuzz_oracle_mvar_delta(OracleFaces.face font index, coords, uint32 coords.Length, uint32 tag))

/// <summary>Reads one direction's font extents from a Farfbuzz font.</summary>
/// <param name="font">The font.</param>
/// <param name="vertical">True for the vertical extents.</param>
/// <returns>Whether they exist and their values.</returns>
let private ourExtents (font: Font) (vertical: bool) =
    let extents = match vertical with | true -> Font.vExtents font | false -> Font.hExtents font
    match extents with
    | ValueSome extents -> struct (true, extents.Ascender, extents.Descender, extents.LineGap)
    | ValueNone -> struct (false, 0, 0, 0)

/// <summary>Reads one direction's font extents from an oracle font handle.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="vertical">True for the vertical extents.</param>
/// <returns>Whether they exist and their values.</returns>
let private oracleHandleExtents (handle: nativeint) (vertical: bool) =
    let mutable extents = Unchecked.defaultof<Native.OracleFontExtents>
    let present =
        match vertical with
        | true -> Native.hb_font_get_v_extents(handle, &extents)
        | false -> Native.hb_font_get_h_extents(handle, &extents)
    struct (present <> 0, extents.Ascender, extents.Descender, extents.LineGap)

/// <summary>Builds a fresh oracle font at scale = upem with normalized coordinates applied.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The oracle font handle.</returns>
let private oracleFontAt (font: TestFont) (index: uint32) (coords: int array) : nativeint =
    let face = OracleFaces.face font index
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(handle, upem, upem)
    Native.hb_font_set_var_coords_normalized(handle, coords, uint32 coords.Length)
    handle

/// <summary>Reads both font extents through the oracle at normalized coordinates.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The horizontal and vertical extents.</returns>
let private oracleExtentsAt (font: TestFont) (index: uint32) (coords: int array) =
    let handle = oracleFontAt font index coords
    let result = struct (oracleHandleExtents handle false, oracleHandleExtents handle true)
    Native.hb_font_destroy handle
    result

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Encodes a uint16 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint16) : byte array =
    [| byte (uint32 value >>> 8); byte value |]

/// <summary>Encodes a uint32 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) : byte array =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>The replacement uint16 values for one mutated count or size field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u16Replacements (value: uint16) : uint16 array =
    [| 0us; 1us; uint16 (int value - 1); uint16 (int value + 1); 0xFFFFus |]

/// <summary>The replacement values for a value-record size, including sizes below 8 and odd sizes.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private valueRecordSizeReplacements (value: uint16) : uint16 array =
    [| 0us; 1us; 7us; 8us; 9us; 11us; uint16 (int value - 1); uint16 (int value + 1); 0xFFFFus |]

/// <summary>One field mutation: the absolute byte offset, a label and the replacement bytes.</summary>
type private Mutation =
    { At: int
      Name: string
      Replacement: byte array }

/// <summary>Mutates the MVAR header fields and the first record's tag, one field rewritten at a time.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="tableAbsAt">The absolute file offset of the MVAR table.</param>
/// <param name="recordCount">The value-record count.</param>
/// <returns>The mutations.</returns>
let private headerMutations (bytes: byte array) (tableAbsAt: int) (recordCount: int) : Mutation list =
    let mutations = ResizeArray<Mutation>()
    for replacement in u16Replacements (readU16 bytes tableAbsAt) do
        mutations.Add { At = tableAbsAt; Name = "version"; Replacement = be16 replacement }
    for replacement in valueRecordSizeReplacements (readU16 bytes (tableAbsAt + 6)) do
        mutations.Add { At = tableAbsAt + 6; Name = "valueRecordSize"; Replacement = be16 replacement }
    for replacement in u16Replacements (readU16 bytes (tableAbsAt + 8)) do
        mutations.Add { At = tableAbsAt + 8; Name = "valueRecordCount"; Replacement = be16 replacement }
    for replacement in u16Replacements (readU16 bytes (tableAbsAt + 10)) do
        mutations.Add { At = tableAbsAt + 10; Name = "varStore offset"; Replacement = be16 replacement }
    match recordCount > 0 with
    | false -> ()
    | true ->
        for replacement in [| 0xFFFFFFFFu; 0u |] do
            mutations.Add { At = tableAbsAt + 12; Name = "record 0 tag"; Replacement = be32 replacement }
    mutations |> Seq.toList

/// <summary>Mutates the directory length of a table, truncating its tail.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The mutations.</returns>
let private truncationMutations (face: Face) (tag: Tag) : Mutation list =
    match Face.tryFindTable face tag with
    | ValueNone -> []
    | ValueSome record ->
        let index = face.Directory |> Seq.findIndex (fun r -> r.Tag = tag)
        let lengthFieldAt = int face.BaseOffset + 12 + index * 16 + 12
        let original = record.Length
        [| 0u; 20u; 21u; 22u; 23u; original / 2u |]
        |> Array.filter (fun length -> length < original)
        |> Array.distinct
        |> Array.map (fun length -> { At = lengthFieldAt; Name = "truncated tail"; Replacement = be32 length })
        |> Array.toList

/// <summary>The normalized coordinate probes used by the mutated-table differential.</summary>
/// <param name="axisCount">The axis count.</param>
/// <returns>The coordinate arrays.</returns>
let private coordProbes (axisCount: int) : int array array =
    [| Array.create axisCount 0
       Array.create axisCount 0x4000
       Array.create axisCount -0x4000
       Array.create axisCount 0x2000 |]

/// <summary>The scale pairs the metrics-position differential draws from.</summary>
/// <param name="upem">The face's units per em.</param>
/// <returns>The (x, y) scale pairs.</returns>
let private scalePairs (upem: int) : struct (int * int) array =
    [| struct (upem, upem)
       struct (3 * upem + 7, 3 * upem + 7)
       struct (-upem, -upem)
       struct (upem, 3 * upem + 7)
       struct (3 * upem + 7, upem) |]

/// <summary>Reads a metric position from a Farfbuzz font as whether it exists and its value.</summary>
/// <param name="font">The font.</param>
/// <param name="tag">The metric tag.</param>
/// <returns>The presence and the position.</returns>
let private ourPosition (font: Font) (tag: Tag) : struct (bool * int) =
    match Metrics.getPosition font tag with
    | ValueSome position -> struct (true, position)
    | ValueNone -> struct (false, 0)

/// <summary>Reads a metric position through the oracle.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="tag">The metric tag.</param>
/// <returns>The presence and the position.</returns>
let private oraclePosition (handle: nativeint) (tag: Tag) : struct (bool * int) =
    let mutable position = 0
    let present = Native.hb_ot_metrics_get_position(handle, uint32 tag, &position)
    struct (present <> 0, position)

/// <summary>Reads a metric position with fallback through the oracle.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="tag">The metric tag.</param>
/// <returns>The position.</returns>
let private oraclePositionWithFallback (handle: nativeint) (tag: Tag) : int =
    let mutable position = 0
    Native.hb_ot_metrics_get_position_with_fallback(handle, uint32 tag, &position)
    position

/// <summary>Mutates a table's version word or words.</summary>
/// <param name="tableAbsAt">The absolute file offset of the table.</param>
/// <param name="u32Version">True for the post table's 32-bit version.</param>
/// <returns>The mutations.</returns>
let private versionMutations (tableAbsAt: int) (u32Version: bool) : Mutation list =
    let mutations = ResizeArray<Mutation>()
    match u32Version with
    | false ->
        for replacement in [| 0us; 1us; 2us; 3us; 0xFFFFus |] do
            mutations.Add { At = tableAbsAt; Name = "version"; Replacement = be16 replacement }
    | true ->
        for replacement in [| 0u; 0x00010000u; 0x00020000u; 0x00030000u; 0x00020050u; 0xFFFFFFFFu |] do
            mutations.Add { At = tableAbsAt; Name = "version"; Replacement = be32 replacement }
    mutations |> Seq.toList

/// <summary>Builds a fresh oracle font at a scale with normalized coordinates applied.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="scale">The (x, y) scale.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The oracle font handle.</returns>
let private oracleFontAtScale (font: TestFont) (index: uint32) (scale: struct (int * int)) (coords: int array) : nativeint =
    let struct (xScale, yScale) = scale
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_scale(handle, xScale, yScale)
    Native.hb_font_set_var_coords_normalized(handle, coords, uint32 coords.Length)
    handle

/// <summary>The P7.S11 MVAR differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let fvarFaces =
        faces
        |> Seq.filter (fun struct (font, index) -> (Fvar.load (faceOf font index)).Axes.Length > 0)
        |> Seq.toArray
    let variationInputs =
        gen {
            let! struct (font, index) = Gen.elements fvarFaces
            let! tag = Gen.elements metricTags
            let! coords = coordsFor font index
            return struct (font, index, tag, coords)
        }
    let extentsInputs =
        gen {
            let! struct (font, index) = Gen.elements fvarFaces
            let! coords = coordsFor font index
            return struct (font, index, coords)
        }
    let metricsFaces =
        faces
        |> Seq.filter (fun struct (font, index) ->
            let face = faceOf font index
            (Os2.load face).Present || (Hhea.load face Tags.Hhea Tags.UpperHhea).Present)
        |> Seq.toArray
    let positionInputs =
        gen {
            let! struct (font, index) = Gen.elements metricsFaces
            let face = faceOf font index
            let! tag = Gen.elements metricTags
            let! coords = coordsFor font index
            let! scale = Gen.elements (scalePairs face.Upem)
            return struct (font, index, tag, coords, scale)
        }
    testList "P7.S11" [
        sampled "P7.S11 metrics variation" variationInputs 5000
            (fun struct (font, index, tag, coords) -> ourVarBits (faceOf font index) coords tag)
            (fun struct (font, index, tag, coords) -> oracleVarBits font index (coords |> Seq.toArray) tag)
        sampled "P7.S11 font extents" extentsInputs 5000
            (fun struct (font, index, coords) ->
                let ourFont = Font.setVarCoordsNormalized coords (Font.create (faceOf font index))
                struct (ourExtents ourFont false, ourExtents ourFont true))
            (fun struct (font, index, coords) -> oracleExtentsAt font index (coords |> Seq.toArray))
        testCase "P7.S11 mutated MVAR" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutatedFaces = 0
            let mutable mutatedTables = 0
            for struct (font, index) in faces do
                let bytes = OracleFaces.data font.Path
                let face = Face.create (ReadOnlyMemory bytes) index
                let axisCount = (Fvar.load face).Axes.Length
                let probes = coordProbes axisCount
                match Face.tryFindTable face Tags.Mvar with
                | ValueNone -> ()
                | ValueSome record ->
                    let mvar = Mvar.load face
                    let tableAbsAt = int (face.BaseOffset + record.Offset)
                    let mutations = headerMutations bytes tableAbsAt mvar.Count @ truncationMutations face Tags.Mvar
                    mutatedFaces <- mutatedFaces + 1
                    for mutation in mutations do
                        mutatedTables <- mutatedTables + 1
                        let mutated = Array.copy bytes
                        for b in 0 .. mutation.Replacement.Length - 1 do
                            mutated[mutation.At + b] <- mutation.Replacement[b]
                        let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                        let oracleFace = Native.hb_face_create(blob, index)
                        let ourFace = Face.create (ReadOnlyMemory mutated) index
                        for coords in probes do
                            for tag in metricTags do
                                let ours = ourVarBits ourFace (ImmutableArray.CreateRange coords) tag
                                let oracle = BitConverter.SingleToUInt32Bits (Shims.farfbuzz_oracle_mvar_delta(oracleFace, coords, uint32 coords.Length, uint32 tag))
                                match ours = oracle with
                                | true -> ()
                                | false -> failures.Add($"{font.Path}#{index} MVAR {mutation.Name} tag {Tag.toString tag} coords %A{coords}: oracle %A{oracle}, farfbuzz %A{ours}")
                            let ourFont = Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) (Font.create ourFace)
                            let oracleFont = Native.hb_font_create oracleFace
                            Native.hb_ot_font_set_funcs oracleFont
                            let upem = int (Native.hb_face_get_upem oracleFace)
                            Native.hb_font_set_scale(oracleFont, upem, upem)
                            Native.hb_font_set_var_coords_normalized(oracleFont, coords, uint32 coords.Length)
                            let oursExtents = struct (ourExtents ourFont false, ourExtents ourFont true)
                            let oracleExtents = struct (oracleHandleExtents oracleFont false, oracleHandleExtents oracleFont true)
                            Native.hb_font_destroy oracleFont
                            match oursExtents = oracleExtents with
                            | true -> ()
                            | false -> failures.Add($"{font.Path}#{index} MVAR {mutation.Name} extents coords %A{coords}: oracle %A{oracleExtents}, farfbuzz %A{oursExtents}")
                        Native.hb_face_destroy oracleFace
                        Native.hb_blob_destroy blob
            printfn "P7.S11 compared %d mutated MVAR faces and %d mutated tables" mutatedFaces mutatedTables
            Expect.isEmpty failures ("mutated-MVAR differences:\n" + String.concat "\n" failures))
        sampled "P7.S11 metrics position" positionInputs 5000
            (fun struct (font, index, tag, coords, scale) ->
                let struct (xScale, yScale) = scale
                let ourFont = Font.setVarCoordsNormalized coords (Font.withScale xScale yScale (Font.create (faceOf font index)))
                struct (ourPosition ourFont tag, Metrics.getPositionWithFallback ourFont tag))
            (fun struct (font, index, tag, coords, scale) ->
                let handle = oracleFontAtScale font index scale (coords |> Seq.toArray)
                let position = oraclePosition handle tag
                let fallback = oraclePositionWithFallback handle tag
                Native.hb_font_destroy handle
                struct (position, fallback))
        testCase "P7.S11 metrics position fixed cases" (fun () ->
            let failures = ResizeArray<string>()
            let compare (label: string) (font: TestFont) (index: uint32) =
                let ourFont = Font.create (faceOf font index)
                let handle = Native.hb_font_create (OracleFaces.face font index)
                Native.hb_ot_font_set_funcs handle
                for tag in metricTags do
                    let ours = struct (ourPosition ourFont tag, Metrics.getPositionWithFallback ourFont tag)
                    let oracle = struct (oraclePosition handle tag, oraclePositionWithFallback handle tag)
                    match ours = oracle with
                    | true -> ()
                    | false -> failures.Add($"{label} tag {Tag.toString tag}: oracle %A{oracle}, farfbuzz %A{ours}")
                Native.hb_font_destroy handle
            let tryFind (pred: Face -> bool) =
                faces
                |> Seq.tryPick (fun struct (font, index) ->
                    match pred (faceOf font index) with
                    | true -> Some struct (font, index)
                    | false -> None)
            match tryFind (fun face -> (Face.referenceTable face Tags.Os2).Length = 0) with
            | None -> failures.Add("no face without OS/2 in the corpus")
            | Some struct (font, index) -> compare "no OS/2" font index
            match tryFind (fun face -> let os2 = Os2.load face in os2.Present && os2.Version < 2us) with
            | None -> failures.Add("no face with OS/2 version 0 or 1 in the corpus")
            | Some struct (font, index) -> compare "OS/2 version 0 or 1" font index
            match tryFind (fun face -> (Face.referenceTable face Tags.Post).Length = 0) with
            | None -> failures.Add("no face without post in the corpus")
            | Some struct (font, index) -> compare "no post" font index
            match faces |> Seq.tryHead with
            | None -> ()
            | Some struct (font, index) ->
                let ourFont = Font.create (faceOf font index)
                let handle = Native.hb_font_create (OracleFaces.face font index)
                Native.hb_ot_font_set_funcs handle
                for unknownTag in [| Tags.Empty; Tag.make 'X' 'X' 'X' 'X' |] do
                    let ours = struct (ourPosition ourFont unknownTag, Metrics.getPositionWithFallback ourFont unknownTag)
                    let oracle = struct (oraclePosition handle unknownTag, oraclePositionWithFallback handle unknownTag)
                    match ours = oracle with
                    | true -> ()
                    | false -> failures.Add($"unknown tag {Tag.toString unknownTag}: oracle %A{oracle}, farfbuzz %A{ours}")
                Native.hb_font_destroy handle
            printfn "P7.S11 metrics position fixed cases checked"
            Expect.isEmpty failures ("fixed-case differences:\n" + String.concat "\n" failures))
        testCase "P7.S11 mutated metrics tables" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutatedTables = 0
            for struct (font, index) in faces do
                let bytes = OracleFaces.data font.Path
                let face = Face.create (ReadOnlyMemory bytes) index
                for tableTag in [| Tags.Os2; Tags.Hhea; Tags.Vhea; Tags.Post |] do
                    match Face.tryFindTable face tableTag with
                    | ValueNone -> ()
                    | ValueSome record ->
                        let tableAbsAt = int (face.BaseOffset + record.Offset)
                        let mutations = versionMutations tableAbsAt (tableTag = Tags.Post) @ truncationMutations face tableTag
                        for mutation in mutations do
                            mutatedTables <- mutatedTables + 1
                            let mutated = Array.copy bytes
                            for b in 0 .. mutation.Replacement.Length - 1 do
                                mutated[mutation.At + b] <- mutation.Replacement[b]
                            let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                            let oracleFace = Native.hb_face_create(blob, index)
                            let oracleFont = Native.hb_font_create oracleFace
                            Native.hb_ot_font_set_funcs oracleFont
                            let ourFont = Font.create (Face.create (ReadOnlyMemory mutated) index)
                            for tag in metricTags do
                                let ours = ourPosition ourFont tag
                                let oracle = oraclePosition oracleFont tag
                                match ours = oracle with
                                | true -> ()
                                | false -> failures.Add($"{font.Path}#{index} {Tag.toString tableTag} {mutation.Name} tag {Tag.toString tag}: oracle %A{oracle}, farfbuzz %A{ours}")
                            Native.hb_font_destroy oracleFont
                            Native.hb_face_destroy oracleFace
                            Native.hb_blob_destroy blob
            printfn "P7.S11 compared %d mutated metrics tables" mutatedTables
            Expect.isEmpty failures ("mutated-metrics differences:\n" + String.concat "\n" failures))
    ]
