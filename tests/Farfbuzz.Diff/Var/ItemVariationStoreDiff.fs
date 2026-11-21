module Farfbuzz.Diff.ItemVariationStoreDiff

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

/// <summary>The <c>GDEF</c> table tag.</summary>
let private gdefTag = Tag.make 'G' 'D' 'E' 'F'

/// <summary>The <c>HVAR</c> table tag.</summary>
let private hvarTag = Tag.make 'H' 'V' 'A' 'R'

/// <summary>The <c>VVAR</c> table tag.</summary>
let private vvarTag = Tag.make 'V' 'V' 'A' 'R'

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

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

/// <summary>The var-store offset of an HVAR or VVAR header, for version 1.</summary>
/// <param name="table">The HVAR or VVAR table.</param>
/// <returns>The store offset, or none.</returns>
let private metricsVarStoreOffset (table: ReadOnlyMemory<byte>) : int voption =
    match BinaryCalls.u16 table.Span 0 = 1us with
    | false -> ValueNone
    | true ->
        match int (BinaryCalls.u32 table.Span 4) with
        | 0 -> ValueNone
        | offset -> ValueSome offset

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

/// <summary>The store of an HVAR or VVAR table, or none.</summary>
/// <param name="table">The table.</param>
/// <returns>The store.</returns>
let private metricsStore (table: ReadOnlyMemory<byte>) : ItemVariationStore voption =
    match metricsVarStoreOffset table with
    | ValueNone -> ValueNone
    | ValueSome at -> ValueSome (readStore table at)

/// <summary>An HVAR or VVAR table with a non-empty store.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The table.</returns>
let private metricsTableWithStore (face: Face) (tag: Tag) : ReadOnlyMemory<byte> voption =
    match Face.referenceTable face tag with
    | table when table.Length = 0 -> ValueNone
    | table ->
        match metricsStore table with
        | ValueNone -> ValueNone
        | ValueSome store when store.Data.Length = 0 -> ValueNone
        | ValueSome _ -> ValueSome table

/// <summary>The unscaled advance delta of a glyph through HVAR or VVAR, reading the store and the advance map.</summary>
/// <param name="table">The HVAR or VVAR table.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The delta.</returns>
let private metricsDelta (table: ReadOnlyMemory<byte>) (coords: NormalizedCoords) (glyph: uint32) : float32 =
    let varStoreOffset = int (BinaryCalls.u32 table.Span 4)
    let advMapOffset = int (BinaryCalls.u32 table.Span 8)
    let instance = ItemVariationStore.instantiate coords (readStore table varStoreOffset)
    match advMapOffset = 0 && glyph > 0xFFFFu with
    | true -> 0.0f
    | false ->
        let advMap = DeltaSetIndexMap.read table advMapOffset
        ItemVariationStore.delta instance (DeltaSetIndexMap.map advMap glyph)

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Reads a big-endian uint32 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU32 (bytes: byte array) (at: int) =
    (uint32 bytes[at] <<< 24) ||| (uint32 bytes[at + 1] <<< 16) ||| (uint32 bytes[at + 2] <<< 8) ||| uint32 bytes[at + 3]

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

/// <summary>The replacement uint16 values for one mutated count field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u16Replacements (value: uint16) : uint16 array =
    [| 0us; 1us; uint16 (int value - 1); uint16 (int value + 1); 0xFFFFus |]

/// <summary>The replacement uint32 values for one mutated offset field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u32Replacements (value: uint32) : uint32 array =
    [| 0u; 1u; value - 1u; value + 1u; 0xFFFFFFFFu |]

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

/// <summary>One field mutation: the absolute byte offset, a label and the replacement bytes.</summary>
type private Mutation =
    { At: int
      Name: string
      Replacement: byte array }

/// <summary>The region scalar probes used by the mutated-store differential.</summary>
/// <param name="axisCount">The axis count.</param>
/// <returns>The coordinate arrays.</returns>
let private coordProbes (axisCount: int) : int array array =
    [| Array.create axisCount 0
       Array.create axisCount 0x4000
       Array.create axisCount -0x4000
       Array.create axisCount 0x2000 |]

/// <summary>Enumerates the mutations of one store: region count, axis count, data-set offsets and VarData fields.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="storeAbsAt">The absolute file offset of the store.</param>
/// <returns>The mutations.</returns>
let private storeMutations (bytes: byte array) (storeAbsAt: int) : Mutation list =
    let regionsOffset = int (readU32 bytes (storeAbsAt + 2))
    let dataCount = int (readU16 bytes (storeAbsAt + 6))
    let regionListAt = storeAbsAt + regionsOffset
    let mutations = ResizeArray<Mutation>()
    let axisCount = readU16 bytes (regionListAt + 0)
    let regionCount = readU16 bytes (regionListAt + 2)
    for replacement in u16Replacements axisCount do
        mutations.Add { At = regionListAt + 0; Name = "axisCount"; Replacement = be16 replacement }
    for replacement in u16Replacements regionCount do
        mutations.Add { At = regionListAt + 2; Name = "regionCount"; Replacement = be16 replacement }
    let mutable dataSet = 0
    while dataSet < dataCount do
        let offsetAt = storeAbsAt + 8 + dataSet * 4
        let offset = readU32 bytes offsetAt
        for replacement in u32Replacements offset do
            mutations.Add { At = offsetAt; Name = $"dataSet {dataSet} offset"; Replacement = be32 replacement }
        match offset with
        | 0u -> ()
        | _ ->
            let varDataAt = storeAbsAt + int offset
            let itemCount = readU16 bytes (varDataAt + 0)
            let wordSizeCount = readU16 bytes (varDataAt + 2)
            let regionIndexCount = readU16 bytes (varDataAt + 4)
            for replacement in u16Replacements itemCount do
                mutations.Add { At = varDataAt + 0; Name = $"dataSet {dataSet} itemCount"; Replacement = be16 replacement }
            for replacement in wordSizeCountReplacements wordSizeCount do
                mutations.Add { At = varDataAt + 2; Name = $"dataSet {dataSet} wordSizeCount"; Replacement = be16 replacement }
            for replacement in u16Replacements regionIndexCount do
                mutations.Add { At = varDataAt + 4; Name = $"dataSet {dataSet} regionIndexCount"; Replacement = be16 replacement }
        dataSet <- dataSet + 1
    mutations |> Seq.toList

/// <summary>Every 16.16 outer.inner index of a store.</summary>
/// <param name="store">The store.</param>
/// <returns>The delta-set indices.</returns>
let private varIndices (store: ItemVariationStore) : uint32 array =
    [| for outer in 0 .. store.Data.Length - 1 do
           for inner in 0 .. store.Data[outer].ItemCount - 1 -> (uint32 outer <<< 16) ||| uint32 inner |]

/// <summary>The float32 delta of a store, as bits, for a coordinate array and a 16.16 index.</summary>
/// <param name="table">The table holding the store.</param>
/// <param name="at">The store offset.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="varIdx">The delta-set index.</param>
/// <returns>The delta bits.</returns>
let private storeDeltaBits (table: ReadOnlyMemory<byte>) (at: int) (coords: int array) (varIdx: uint32) : uint32 =
    let instance = ItemVariationStore.instantiate (NormalizedCoords.ofSealed (ImmutableArray.CreateRange coords)) (readStore table at)
    BitConverter.SingleToUInt32Bits (ItemVariationStore.delta instance varIdx)

/// <summary>The P7.S03 item variation store and DeltaSetIndexMap differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let gdefStores =
        faces
        |> Seq.choose (fun struct (font, index) ->
            match gdefStore (faceOf font index) with
            | ValueSome store when store.Data.Length > 0 && store.Data |> Seq.forall (fun data -> data.ItemCount > 0) ->
                Some (struct (font, index, store))
            | _ -> None)
        |> Seq.toArray
    let hvarFaces =
        faces
        |> Seq.choose (fun struct (font, index) ->
            match metricsTableWithStore (faceOf font index) hvarTag with
            | ValueNone -> None
            | ValueSome _ -> Some (struct (font, index)))
        |> Seq.toArray
    let vvarFaces =
        faces
        |> Seq.choose (fun struct (font, index) ->
            match metricsTableWithStore (faceOf font index) vvarTag with
            | ValueNone -> None
            | ValueSome _ -> Some (struct (font, index)))
        |> Seq.toArray
    let gdefInputs =
        gen {
            let! struct (font, index, store) = Gen.elements gdefStores
            let! outer = Gen.choose (0, store.Data.Length - 1)
            let! inner = Gen.choose (0, store.Data[outer].ItemCount - 1)
            let! coords = coordsFor font index
            return struct (font, index, coords, (uint32 outer <<< 16) ||| uint32 inner)
        }
    let metricsInputs (fonts: struct (TestFont * uint32) array) =
        gen {
            let! struct (font, index) = Gen.elements fonts
            let glyphCount = (faceOf font index).GlyphCount
            let! glyph = Gen.choose (0, glyphCount - 1) |> Gen.map uint32
            let! coords = coordsFor font index
            return struct (font, index, coords, glyph)
        }
    testList "P7.S03" [
        sampled "P7.S03 gdef deltas" gdefInputs 5000
            (fun struct (font, index, coords, varIdx) ->
                let face = faceOf font index
                let table = Face.referenceTable face gdefTag
                let at = gdefVarStoreOffset table |> ValueOption.defaultValue 0
                storeDeltaBits table at (coords |> Seq.toArray) varIdx)
            (fun struct (font, index, coords, varIdx) ->
                let coordsArray = coords |> Seq.toArray
                BitConverter.SingleToUInt32Bits (Shims.farfbuzz_oracle_gdef_delta(OracleFaces.face font index, coordsArray, uint32 coordsArray.Length, varIdx)))
        sampled "P7.S03 gdef deltas double" gdefInputs 2000
            (fun struct (font, index, coords, varIdx) ->
                let face = faceOf font index
                let table = Face.referenceTable face gdefTag
                let at = gdefVarStoreOffset table |> ValueOption.defaultValue 0
                let instance = ItemVariationStore.instantiate (NormalizedCoords.ofSealed coords) (readStore table at)
                BitConverter.DoubleToUInt64Bits (ItemVariationStore.deltaDouble instance varIdx))
            (fun struct (font, index, coords, varIdx) ->
                let coordsArray = coords |> Seq.toArray
                BitConverter.DoubleToUInt64Bits (Shims.farfbuzz_oracle_gdef_delta_double(OracleFaces.face font index, coordsArray, uint32 coordsArray.Length, varIdx)))
        sampled "P7.S03 hvar deltas" (metricsInputs hvarFaces) 5000
            (fun struct (font, index, coords, glyph) ->
                let face = faceOf font index
                let table = Face.referenceTable face hvarTag
                BitConverter.SingleToUInt32Bits (metricsDelta table (NormalizedCoords.ofSealed coords) glyph))
            (fun struct (font, index, coords, glyph) ->
                let coordsArray = coords |> Seq.toArray
                BitConverter.SingleToUInt32Bits (Shims.farfbuzz_oracle_hvar_delta(OracleFaces.face font index, coordsArray, uint32 coordsArray.Length, glyph)))
        sampled "P7.S03 vvar deltas" (metricsInputs vvarFaces) 1000
            (fun struct (font, index, coords, glyph) ->
                let face = faceOf font index
                let table = Face.referenceTable face vvarTag
                BitConverter.SingleToUInt32Bits (metricsDelta table (NormalizedCoords.ofSealed coords) glyph))
            (fun struct (font, index, coords, glyph) ->
                let coordsArray = coords |> Seq.toArray
                BitConverter.SingleToUInt32Bits (Shims.farfbuzz_oracle_vvar_delta(OracleFaces.face font index, coordsArray, uint32 coordsArray.Length, glyph)))
        testCase "P7.S03 mutated stores" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutatedFaces = 0
            let mutable mutatedTables = 0
            for struct (font, index) in faces do
                let bytes = OracleFaces.data font.Path
                let face = Face.create (ReadOnlyMemory bytes) index
                let probes = coordProbes (Fvar.load face).Axes.Length
                match Face.tryFindTable face gdefTag with
                | ValueNone -> ()
                | ValueSome record ->
                    let tableAt = int (face.BaseOffset + record.Offset)
                    let table = Face.referenceTable face gdefTag
                    match gdefVarStoreOffset table with
                    | ValueNone -> ()
                    | ValueSome storeAt ->
                        let store = readStore table storeAt
                        let indices = varIndices store
                        mutatedFaces <- mutatedFaces + 1
                        for mutation in storeMutations bytes (tableAt + storeAt) do
                            mutatedTables <- mutatedTables + 1
                            let mutated = Array.copy bytes
                            for b in 0 .. mutation.Replacement.Length - 1 do
                                mutated[mutation.At + b] <- mutation.Replacement[b]
                            let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                            let oracleFace = Native.hb_face_create(blob, index)
                            let oursTable = Face.referenceTable (Face.create (ReadOnlyMemory mutated) index) gdefTag
                            for probe in probes do
                                for varIdx in indices do
                                    let ours = storeDeltaBits oursTable storeAt probe varIdx
                                    let oracle = BitConverter.SingleToUInt32Bits (Shims.farfbuzz_oracle_gdef_delta(oracleFace, probe, uint32 probe.Length, varIdx))
                                    match ours = oracle with
                                    | true -> ()
                                    | false -> failures.Add($"{font.Path}#{index} GDEF {mutation.Name} varIdx {varIdx}: oracle %A{oracle}, farfbuzz %A{ours}")
                            Native.hb_face_destroy oracleFace
                            Native.hb_blob_destroy blob
                match Face.tryFindTable face hvarTag with
                | ValueNone -> ()
                | ValueSome record ->
                    let tableAt = int (face.BaseOffset + record.Offset)
                    let table = Face.referenceTable face hvarTag
                    match metricsVarStoreOffset table with
                    | ValueNone -> ()
                    | ValueSome storeAt ->
                        mutatedFaces <- mutatedFaces + 1
                        let glyphCount = face.GlyphCount
                        for mutation in storeMutations bytes (tableAt + storeAt) do
                            mutatedTables <- mutatedTables + 1
                            let mutated = Array.copy bytes
                            for b in 0 .. mutation.Replacement.Length - 1 do
                                mutated[mutation.At + b] <- mutation.Replacement[b]
                            let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                            let oracleFace = Native.hb_face_create(blob, index)
                            let oursTable = Face.referenceTable (Face.create (ReadOnlyMemory mutated) index) hvarTag
                            for probe in probes do
                                for glyph in 0 .. glyphCount - 1 do
                                    let ours = BitConverter.SingleToUInt32Bits (metricsDelta oursTable (NormalizedCoords.ofSealed (ImmutableArray.CreateRange probe)) (uint32 glyph))
                                    let oracle = BitConverter.SingleToUInt32Bits (Shims.farfbuzz_oracle_hvar_delta(oracleFace, probe, uint32 probe.Length, uint32 glyph))
                                    match ours = oracle with
                                    | true -> ()
                                    | false -> failures.Add($"{font.Path}#{index} HVAR {mutation.Name} glyph {glyph}: oracle %A{oracle}, farfbuzz %A{ours}")
                            Native.hb_face_destroy oracleFace
                            Native.hb_blob_destroy blob
            printfn "P7.S03 compared %d mutated store faces and %d mutated tables" mutatedFaces mutatedTables
            Expect.isEmpty failures ("mutated-store differences:\n" + String.concat "\n" failures))
    ]
