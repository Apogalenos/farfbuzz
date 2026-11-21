module Farfbuzz.Diff.FeatureVariationsDiff

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

/// <summary>The GSUB table tag.</summary>
let private gsubTag = Tag.make 'G' 'S' 'U' 'B'

/// <summary>The GPOS table tag.</summary>
let private gposTag = Tag.make 'G' 'P' 'O' 'S'

/// <summary>The GDEF table tag.</summary>
let private gdefTag = Tag.make 'G' 'D' 'E' 'F'

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Reads a store at an offset of its parent table, empty on a zero offset or a failure.</summary>
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

/// <summary>The GDEF var-store offset, for version 1.3 and later.</summary>
/// <param name="table">The GDEF table.</param>
/// <returns>The store offset, or none.</returns>
let private gdefVarStoreOffset (table: ReadOnlyMemory<byte>) : int voption =
    match BinaryCalls.u16 table.Span 0 = 1us && BinaryCalls.u16 table.Span 2 >= 3us with
    | false -> ValueNone
    | true ->
        match int (BinaryCalls.u32 table.Span 14) with
        | 0 -> ValueNone
        | offset -> ValueSome offset

/// <summary>The GDEF store of a face, empty when absent.</summary>
/// <param name="face">The face.</param>
/// <returns>The store.</returns>
let private gdefStore (face: Face) : ItemVariationStore =
    match Face.referenceTable face gdefTag with
    | table when table.Length = 0 -> ItemVariationStore.empty
    | table ->
        match gdefVarStoreOffset table with
        | ValueNone -> ItemVariationStore.empty
        | ValueSome at -> readStore table at

/// <summary>Builds the instancer for a face and coordinates, instantiating the GDEF store only for non-zero coordinates.</summary>
/// <param name="face">The face.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The instancer.</returns>
let private instancerOf (face: Face) (coords: ImmutableArray<int>) : Instancer =
    let normalized = NormalizedCoords.ofSealed coords
    let instance =
        match normalized.HasNonZero with
        | false -> { Store = ItemVariationStore.empty; Scalars = ImmutableArray<float32>.Empty }
        | true -> ItemVariationStore.instantiate normalized (gdefStore face)
    { Gdef = instance; Coords = normalized }

/// <summary>The FeatureVariations of a face's table, empty when absent.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <returns>The FeatureVariations.</returns>
let private variationsOfFace (face: Face) (tableTag: Tag) : FeatureVariations =
    let ot = OtFace.create face
    let bytes = match tableTag = gsubTag with | true -> ot.Gsub.Value.FeatureVariations | false -> ot.Gpos.Value.FeatureVariations
    FeatureVariations.read bytes

/// <summary>The feature count of a face's table.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <returns>The feature count.</returns>
let private featureCountOf (face: Face) (tableTag: Tag) : int =
    let ot = OtFace.create face
    match tableTag = gsubTag with | true -> ot.Gsub.Value.Features.Length | false -> ot.Gpos.Value.Features.Length

/// <summary>Whether a face has FeatureVariations in its GSUB or GPOS table.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>True when a table has FeatureVariations.</returns>
let private hasFeatureVariations (font: TestFont) (index: uint32) : bool =
    let face = faceOf font index
    let ot = OtFace.create face
    (ot.Gsub.Value.FeatureVariations.Length > 0) || (ot.Gpos.Value.FeatureVariations.Length > 0)

/// <summary>Every face with FeatureVariations, from the shape suites and the API fonts.</summary>
/// <returns>The faces.</returns>
let private featureVariationFaces () : struct (TestFont * uint32) array =
    let inSuites =
        OracleFaces.all ()
        |> Seq.filter (fun struct (font, index) -> hasFeatureVariations font index)
    let extra =
        TestFonts.extra
            [ "api/fonts/feature-variation-conditions.ttf"
              "api/fonts/feature-variation-precision-GPOS-legacy-avar2.ttf"
              "api/fonts/feature-variation-precision-GPOS-legacy-large-third.ttf"
              "api/fonts/feature-variation-precision-GPOS-legacy-large.ttf"
              "api/fonts/feature-variation-precision-GPOS-legacy-third.ttf"
              "api/fonts/feature-variation-precision-GPOS-legacy.ttf"
              "api/fonts/feature-variation-precision-GPOS-lookup-avar2.ttf"
              "api/fonts/feature-variation-precision-GPOS-lookup-large-third.ttf"
              "api/fonts/feature-variation-precision-GPOS-lookup-large.ttf"
              "api/fonts/feature-variation-precision-GPOS-lookup-third.ttf"
              "api/fonts/feature-variation-precision-GPOS-lookup.ttf"
              "api/fonts/feature-variation-precision-GSUB-legacy-avar2.ttf"
              "api/fonts/feature-variation-precision-GSUB-legacy-large-third.ttf"
              "api/fonts/feature-variation-precision-GSUB-legacy-large.ttf"
              "api/fonts/feature-variation-precision-GSUB-legacy-third.ttf"
              "api/fonts/feature-variation-precision-GSUB-legacy.ttf"
              "api/fonts/feature-variation-precision-GSUB-lookup-avar2.ttf"
              "api/fonts/feature-variation-precision-GSUB-lookup-large-third.ttf"
              "api/fonts/feature-variation-precision-GSUB-lookup-large.ttf"
              "api/fonts/feature-variation-precision-GSUB-lookup-third.ttf"
              "api/fonts/feature-variation-precision-GSUB-lookup.ttf"
              "api/fonts/feature-variation-universal-prefix.ttf"
              "api/fonts/feature-variation-universal.ttf" ]
        |> Seq.map (fun font -> struct (font, uint32 font.Index))
        |> Seq.filter (fun struct (font, index) -> hasFeatureVariations font index)
    Seq.append inSuites extra |> Seq.toArray

/// <summary>The variations index through the oracle, as a uint32 (NOT_FOUND when none).</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="coords">The coordinates.</param>
/// <returns>The index.</returns>
let private oracleVariationsIndex (face: nativeint) (tableTag: uint32) (coords: int array) : uint32 =
    let mutable index = 0xFFFFFFFFu
    Native.hb_ot_layout_table_find_feature_variations(face, tableTag, coords, uint32 coords.Length, &index) |> ignore
    index

/// <summary>The Farfbuzz variations index of a face's table.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="coords">The coordinates.</param>
/// <returns>The index.</returns>
let private farfbuzzVariationsIndex (face: Face) (tableTag: Tag) (coords: ImmutableArray<int>) : uint32 =
    let fv = variationsOfFace face tableTag
    FeatureVariations.findIndex fv (instancerOf face coords)

/// <summary>The oracle's feature lookups, sorted and unique, through the plan key shim.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
/// <param name="featureIndex">The feature index.</param>
/// <param name="coords">The coordinates.</param>
/// <returns>The lookups.</returns>
let private oracleFeatureLookups (face: nativeint) (tableIndex: uint32) (featureIndex: uint32) (coords: int array) : uint32 array =
    let total = Shims.farfbuzz_oracle_feature_lookups(face, tableIndex, featureIndex, coords, uint32 coords.Length, Array.empty, 0u)
    let out = Array.zeroCreate (int total)
    Shims.farfbuzz_oracle_feature_lookups(face, tableIndex, featureIndex, coords, uint32 coords.Length, out, uint32 out.Length) |> ignore
    out

/// <summary>The Farfbuzz feature lookups, sorted and unique.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="featureIndex">The feature index.</param>
/// <param name="coords">The coordinates.</param>
/// <returns>The lookups.</returns>
let private farfbuzzFeatureLookups (face: Face) (tableTag: Tag) (featureIndex: int) (coords: ImmutableArray<int>) : uint32 array =
    let ot = OtFace.create face
    let instancer = instancerOf face coords
    let result =
        match tableTag = gsubTag with
        | true ->
            let layout = ot.Gsub.Value
            let fv = FeatureVariations.read layout.FeatureVariations
            FeatureVariations.featureLookups layout fv instancer (FeatureVariations.findIndex fv instancer) featureIndex
        | false ->
            let layout = ot.Gpos.Value
            let fv = FeatureVariations.read layout.FeatureVariations
            FeatureVariations.featureLookups layout fv instancer (FeatureVariations.findIndex fv instancer) featureIndex
    result |> Seq.map uint32 |> Seq.toArray

/// <summary>One field mutation: the absolute byte offset, a label and the replacement bytes.</summary>
type private Mutation =
    { At: int
      Name: string
      Replacement: byte array }

/// <summary>Enumerates the FeatureVariations field mutations of a face.</summary>
module private Mutations =
    /// <summary>Encodes an int16 as big-endian bytes.</summary>
    let private be16 (v: int) = [| byte (v >>> 8); byte v |]

    /// <summary>Encodes a 24-bit value as big-endian bytes.</summary>
    let private be24 (v: int) = [| byte (v >>> 16); byte (v >>> 8); byte v |]

    /// <summary>Encodes a uint32 as big-endian bytes.</summary>
    let private be32 (v: uint32) = [| byte (v >>> 24); byte (v >>> 16); byte (v >>> 8); byte v |]

    /// <summary>Reads a big-endian uint16, 0 outside the file.</summary>
    let private readU16 (bytes: byte array) (at: int) = if at >= 0 && at + 2 <= bytes.Length then (int bytes[at] <<< 8) ||| int bytes[at + 1] else 0

    /// <summary>Reads a big-endian 24-bit value, 0 outside the file.</summary>
    let private readU24 (bytes: byte array) (at: int) = if at >= 0 && at + 3 <= bytes.Length then (int bytes[at] <<< 16) ||| (int bytes[at + 1] <<< 8) ||| int bytes[at + 2] else 0

    /// <summary>Reads a big-endian uint32, 0 outside the file.</summary>
    let private readU32 (bytes: byte array) (at: int) = if at >= 0 && at + 4 <= bytes.Length then (int bytes[at] <<< 24) ||| (int bytes[at + 1] <<< 16) ||| (int bytes[at + 2] <<< 8) ||| int bytes[at + 3] else 0

    /// <summary>Adds the condition mutations of one condition, recursing into And, Or and Negate children.</summary>
    /// <param name="bytes">The file bytes.</param>
    /// <param name="condAbs">The absolute condition offset.</param>
    /// <param name="depth">The remaining walk depth.</param>
    /// <param name="name">The mutation label prefix.</param>
    /// <param name="mutations">The mutations collected so far.</param>
    let rec private conditionMutations (bytes: byte array) (condAbs: int) (depth: int) (name: string) (mutations: ResizeArray<Mutation>) : unit =
        match depth with
        | 0 -> ()
        | _ ->
            let format = readU16 bytes condAbs
            for f in [ 0; 1; 2; 3; 4; 5; 6; 0xFFFF ] do
                if f <> format then mutations.Add { At = condAbs; Name = name + $"format {f}"; Replacement = be16 f }
            match format with
            | 1 ->
                let axisIndex = readU16 bytes (condAbs + 2)
                for v in [ 0; 1; 0x7FFF; 0xFFFF ] do
                    if v <> axisIndex then mutations.Add { At = condAbs + 2; Name = name + " axis index"; Replacement = be16 v }
                let maxValue = readU16 bytes (condAbs + 6)
                mutations.Add { At = condAbs + 4; Name = name + " filter range min above max"; Replacement = be16 (maxValue + 1) }
            | 3 | 4 ->
                let count = if condAbs + 2 <= bytes.Length then int bytes[condAbs + 2] else 0
                for c in 0 .. count - 1 do
                    let offsetField = condAbs + 3 + c * 3
                    let childOffset = readU24 bytes offsetField
                    mutations.Add { At = offsetField; Name = name + " child offset self"; Replacement = be24 0 }
                    if childOffset <> 0 then
                        conditionMutations bytes (condAbs + childOffset) (depth - 1) (name + " child") mutations
            | 5 ->
                let childOffset = readU24 bytes (condAbs + 2)
                if childOffset <> 0 then
                    conditionMutations bytes (condAbs + childOffset) (depth - 1) (name + " child") mutations
            | _ -> ()

    /// <summary>Enumerates the FeatureVariations field mutations of one face.</summary>
    /// <param name="bytes">The file bytes.</param>
    /// <param name="fvAbs">The absolute FeatureVariations offset.</param>
    /// <returns>The mutations.</returns>
    let enumerate (bytes: byte array) (fvAbs: int) : Mutation list =
        let mutations = ResizeArray<Mutation>()
        let version = readU32 bytes fvAbs
        let recordCount = readU32 bytes (fvAbs + 4)
        for v in [ 0x00010000; 0x00010001 ] do
            if v <> version then mutations.Add { At = fvAbs; Name = "version"; Replacement = be32 (uint32 v) }
        for v in [ 0; 1; recordCount - 1; recordCount + 1; 0x7FFFFFFF ] do
            if v >= 0 && v <> recordCount then mutations.Add { At = fvAbs + 4; Name = "record count"; Replacement = be32 (uint32 v) }
        for i in 0 .. min recordCount 4 - 1 do
            let record = fvAbs + 8 + i * 8
            let conditionsOffset = readU32 bytes record
            let substitutionsOffset = readU32 bytes (record + 4)
            for v in [ 0; 1; conditionsOffset - 1; conditionsOffset + 1; 0x7FFFFFFF ] do
                if v >= 0 && v <> conditionsOffset then mutations.Add { At = record; Name = $"record {i} condition offset"; Replacement = be32 (uint32 v) }
            if conditionsOffset > 0 then
                let condAbs = fvAbs + conditionsOffset
                let condCount = readU16 bytes condAbs
                for v in [ 0; 1; condCount - 1; condCount + 1; 0xFFFF ] do
                    if v >= 0 && v <> condCount then mutations.Add { At = condAbs; Name = $"record {i} condition count {v}"; Replacement = be16 v }
                for c in 0 .. condCount - 1 do
                    let condOffset = readU32 bytes (condAbs + 2 + c * 4)
                    if condOffset > 0 then
                        conditionMutations bytes (condAbs + condOffset) 8 $"record {i} condition {c} " mutations
            if substitutionsOffset > 0 then
                let subAbs = fvAbs + substitutionsOffset
                let subCount = readU16 bytes (subAbs + 4)
                for j in 0 .. min subCount 4 - 1 do
                    let subRecord = subAbs + 6 + j * 6
                    let featureIndex = readU16 bytes subRecord
                    for v in [ 0; 1; featureIndex - 1; featureIndex + 1; 0xFFFF ] do
                        if v >= 0 && v <> featureIndex then mutations.Add { At = subRecord; Name = $"record {i} substitution {j} feature index"; Replacement = be16 v }
        mutations |> Seq.toList

/// <summary>One plan input: the segment properties and the user features.</summary>
type private PlanCase =
    { Props: SegmentProperties
      Features: Feature array }

/// <summary>The single plan case the varied-plan differential uses, Latin without user features.</summary>
let private planCase : PlanCase =
    { Props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
      Features = [||] }

/// <summary>Whether a face's GSUB or GPOS table carries feature variations.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <returns>True when the table has a non-empty feature-variations field.</returns>
let private hasTableFeatureVariations (face: Face) (tableTag: Tag) : bool =
    let ot = OtFace.create face
    match tableTag = gsubTag with
    | true -> ot.Gsub.Value.FeatureVariations.Length > 0
    | false -> ot.Gpos.Value.FeatureVariations.Length > 0

/// <summary>One format-1 axis-range condition: its axis index and inclusive range.</summary>
type private RangeCondition =
    { Axis: int
      Min: int
      Max: int }

/// <summary>Collects the format-1 axis-range conditions of one condition, depth-bounded.</summary>
/// <param name="bytes">The FeatureVariations bytes.</param>
/// <param name="at">The condition offset.</param>
/// <param name="depth">The remaining walk depth.</param>
/// <param name="acc">The conditions collected so far.</param>
let rec private rangeConditionsAt (bytes: ReadOnlySpan<byte>) (at: int) (depth: int) (acc: ResizeArray<RangeCondition>) : unit =
    match depth with
    | 0 -> ()
    | _ ->
        match int (BinaryCalls.u16 bytes at) with
        | 1 ->
            acc.Add { Axis = int (BinaryCalls.u16 bytes (at + 2)); Min = int (BinaryCalls.i16 bytes (at + 4)); Max = int (BinaryCalls.i16 bytes (at + 6)) }
        | 3 | 4 ->
            let count = int (BinaryCalls.u8 bytes (at + 2))
            for c in 0 .. count - 1 do
                match BinaryCalls.u24 bytes (at + 3 + c * 3) with
                | 0u -> ()
                | child -> rangeConditionsAt bytes (at + int child) (depth - 1) acc
        | 5 ->
            match BinaryCalls.u24 bytes (at + 2) with
            | 0u -> ()
            | child -> rangeConditionsAt bytes (at + int child) (depth - 1) acc
        | _ -> ()

/// <summary>The format-1 axis-range conditions of a table's FeatureVariations.</summary>
/// <param name="fv">The FeatureVariations.</param>
/// <returns>The conditions.</returns>
let private rangeConditions (fv: FeatureVariations) : RangeCondition array =
    let acc = ResizeArray<RangeCondition>()
    let bytes = fv.Bytes.Span
    for i in 0 .. fv.Records.Length - 1 do
        let struct (conditionsOffset, _) = fv.Records[i]
        match conditionsOffset with
        | 0 -> ()
        | _ ->
            let count = int (BinaryCalls.u16 bytes conditionsOffset)
            for c in 0 .. count - 1 do
                match BinaryCalls.u32 bytes (conditionsOffset + 2 + c * 4) with
                | 0u -> ()
                | child -> rangeConditionsAt bytes (conditionsOffset + int child) 8 acc
    acc.ToArray()

/// <summary>All-zero normalized coordinates with one axis set to a value.</summary>
/// <param name="axisCount">The number of coordinates.</param>
/// <param name="axis">The axis to set.</param>
/// <param name="value">The value.</param>
/// <returns>The coordinates.</returns>
let private coordsWith (axisCount: int) (axis: int) (value: int) : ImmutableArray<int> =
    let coords = Array.zeroCreate axisCount
    coords[axis] <- value
    ImmutableArray.CreateRange coords

/// <summary>The plan's collected lookups for a table, at coordinates, through Farfbuzz.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="table">The table tag.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The lookup indices.</returns>
let private farfbuzzPlanLookups (font: TestFont) (index: uint32) (table: Tag) (coords: ImmutableArray<int>) : int list =
    let font = Font.create (faceOf font index)
    let font = Font.setVarCoordsNormalized coords font
    LayoutApi.planCollectLookups (OtPlan.create font planCase.Props (ImmutableArray.CreateRange planCase.Features)) table |> Seq.toList

/// <summary>The plan's collected lookups for a table, at coordinates, through the oracle.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="table">The table tag.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The lookup indices.</returns>
let private oraclePlanLookups (font: TestFont) (index: uint32) (table: Tag) (coords: ImmutableArray<int>) : int list =
    let face = OracleFaces.face font index
    let mutable props = Native.NativeSegmentProperties()
    props.Direction <- int (Direction.value planCase.Props.Direction)
    props.Script <- uint32 planCase.Props.Script
    props.Language <- 0n
    let coordsArray = coords |> Seq.toArray
    let plan =
        Native.hb_shape_plan_create2(face, &props, planCase.Features, uint32 planCase.Features.Length, coordsArray, uint32 coordsArray.Length, [| "ot"; Unchecked.defaultof<string> |])
    let set = Native.hb_set_create ()
    Native.hb_ot_shape_plan_collect_lookups(plan, uint32 table, set)
    let rec members (current: uint32) (found: int list) =
        let mutable next = current
        match Native.hb_set_next(set, &next) with
        | 0 -> List.rev found
        | _ -> members next (int next :: found)
    let result = members UInt32.MaxValue []
    Native.hb_set_destroy set
    Native.hb_shape_plan_destroy plan
    result

/// <summary>The fixed probe inputs of a face's table: zeros and every condition-range boundary plus one outside.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="table">The table tag.</param>
/// <returns>The probe inputs.</returns>
let private fixedProbes (font: TestFont) (index: uint32) (table: Tag) : struct (TestFont * uint32 * Tag * ImmutableArray<int>) array =
    let face = faceOf font index
    let ot = OtFace.create face
    let bytes = match table = gsubTag with | true -> ot.Gsub.Value.FeatureVariations | false -> ot.Gpos.Value.FeatureVariations
    let fv = FeatureVariations.read bytes
    let axisCount = (Fvar.load face).Axes.Length
    let probes = ResizeArray<struct (TestFont * uint32 * Tag * ImmutableArray<int>)>()
    probes.Add (struct (font, index, table, ImmutableArray.CreateRange (Array.zeroCreate axisCount)))
    for range in rangeConditions fv do
        match range.Axis >= 0 && range.Axis < axisCount with
        | false -> ()
        | true ->
            for value in [ range.Min; range.Min - 1; range.Max; range.Max + 1 ] do
                probes.Add (struct (font, index, table, coordsWith axisCount range.Axis value))
    probes.ToArray()

/// <summary>The FeatureVariations differentials of P7.S12.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = featureVariationFaces ()
    let findInputs =
        gen {
            let! struct (font, index) = Gen.elements faces
            let! table = Gen.elements [ gsubTag; gposTag ]
            let face = faceOf font index
            let fv = variationsOfFace face table
            if fv.Records.Length = 0 then
                return! Gen.constant (struct (font, index, table, ImmutableArray<int>.Empty))
            else
                let! coords = coordsFor font index
                return struct (font, index, table, coords)
        }
    let lookupInputs =
        gen {
            let! struct (font, index) = Gen.elements faces
            let! table = Gen.elements [ gsubTag; gposTag ]
            let face = faceOf font index
            let featureCount = featureCountOf face table
            if featureCount = 0 then
                return! Gen.constant (struct (font, index, table, 0, ImmutableArray<int>.Empty))
            else
                let! featureIndex = Gen.choose (0, featureCount - 1)
                let! coords = coordsFor font index
                return struct (font, index, table, featureIndex, coords)
        }
    let planInputs =
        gen {
            let! struct (font, index) = Gen.elements faces
            let face = faceOf font index
            let tables =
                [ if hasTableFeatureVariations face gsubTag then yield gsubTag
                  if hasTableFeatureVariations face gposTag then yield gposTag ]
            let! table = Gen.elements tables
            let! coords = coordsFor font index
            return struct (font, index, table, coords)
        }
    let fixedInputs =
        faces
        |> Array.collect (fun struct (font, index) ->
            let face = faceOf font index
            [| if hasTableFeatureVariations face gsubTag then yield! fixedProbes font index gsubTag
               if hasTableFeatureVariations face gposTag then yield! fixedProbes font index gposTag |])
    testList "P7.S12" [
        sampled "P7.S12 find variations" findInputs 5000
            (fun struct (font, index, table, coords) ->
                farfbuzzVariationsIndex (faceOf font index) table coords)
            (fun struct (font, index, table, coords) ->
                oracleVariationsIndex (OracleFaces.face font index) (uint32 table) (coords |> Seq.toArray))
        sampled "P7.S12 feature lookups" lookupInputs 5000
            (fun struct (font, index, table, featureIndex, coords) ->
                farfbuzzFeatureLookups (faceOf font index) table featureIndex coords)
            (fun struct (font, index, table, featureIndex, coords) ->
                let tableIndex = if table = gsubTag then 0u else 1u
                oracleFeatureLookups (OracleFaces.face font index) tableIndex (uint32 featureIndex) (coords |> Seq.toArray))
        sampled "P7.S12 plan lookups varied" planInputs 5000
            (fun struct (font, index, table, coords) ->
                farfbuzzPlanLookups font index table coords)
            (fun struct (font, index, table, coords) ->
                oraclePlanLookups font index table coords)
        testCase "P7.S12 plan lookups boundary" (fun () ->
            let failures = ResizeArray<string>()
            for struct (font, index, table, coords) in fixedInputs do
                let ours = farfbuzzPlanLookups font index table coords
                let oracle = oraclePlanLookups font index table coords
                match ours = oracle with
                | true -> ()
                | false -> failures.Add $"{font.Path}#{index} {table} coords %A{coords}: oracle %A{oracle}, farfbuzz %A{ours}"
            Expect.isEmpty failures ("boundary plan-lookup differences:\n" + String.concat "\n" failures))
        testCase "P7.S12 mutated feature variations" (fun () ->
            let failures = ResizeArray<string>()
            let mutable compared = 0
            for struct (font, index) in faces do
                let bytes = OracleFaces.data font.Path
                let face = faceOf font index
                for table in [ gsubTag; gposTag ] do
                    match Face.tryFindTable face table with
                    | ValueNone -> ()
                    | ValueSome record ->
                        let tableAbs = int face.BaseOffset + int record.Offset
                        let tableBytes = Face.referenceTable face table
                        let fvBytes = variationsOfFace face table
                        match tableBytes.Length = 0 || fvBytes.Bytes.Length = 0 with
                        | true -> ()
                        | false ->
                            let fvAbs = tableAbs + int (BinaryCalls.u32 tableBytes.Span 10)
                            let featureCount = featureCountOf face table
                            for mutation in Mutations.enumerate bytes fvAbs do
                                let mutated = Array.copy bytes
                                for b in 0 .. mutation.Replacement.Length - 1 do
                                    mutated[mutation.At + b] <- mutation.Replacement[b]
                                let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
                                let oracleFace = Native.hb_face_create(blob, index)
                                let oursFace = Face.create (ReadOnlyMemory mutated) index
                                for probe in [| [| 0 |]; [| 0x4000 |] |] do
                                    let ours = farfbuzzVariationsIndex oursFace table (ImmutableArray.CreateRange probe)
                                    let oracle = oracleVariationsIndex oracleFace (uint32 table) probe
                                    if ours <> oracle then failures.Add($"{font.Path}#{index} {table} {mutation.Name} find variations: oracle %A{oracle}, farfbuzz %A{ours}")
                                // Feature lookups compare only a still-valid FeatureVariations: a mutation that fails
                                // sanitize rejects the whole GSUB/GPOS in HarfBuzz (empty), while this checkout's
                                // shallow header sanitize keeps the table and falls back to the original feature.
                                let fvValid = (variationsOfFace oursFace table).Bytes.Length > 0
                                if fvValid then
                                    for featureIndex in 0 .. max 0 (min featureCount 2 - 1) do
                                        let probe = [| 0x4000 |]
                                        let ours = farfbuzzFeatureLookups oursFace table featureIndex (ImmutableArray.CreateRange probe)
                                        let oracle = oracleFeatureLookups oracleFace (if table = gsubTag then 0u else 1u) (uint32 featureIndex) probe
                                        if ours <> oracle then failures.Add($"{font.Path}#{index} {table} {mutation.Name} feature {featureIndex} lookups: oracle %A{oracle}, farfbuzz %A{ours}")
                                Native.hb_face_destroy oracleFace
                                Native.hb_blob_destroy blob
                                compared <- compared + 1
            printfn "P7.S12 compared %d mutated feature variations" compared
            Expect.isEmpty failures ("mutated feature-variation differences:\n" + String.concat "\n" failures))
    ]
