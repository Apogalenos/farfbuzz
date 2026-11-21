module Farfbuzz.Diff.MapDiff

open System
open System.Collections.Immutable
open System.IO
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Oracle.Native
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>The GSUB table tag.</summary>
let private gsubTag = Tag.make 'G' 'S' 'U' 'B'

/// <summary>The GPOS table tag.</summary>
let private gposTag = Tag.make 'G' 'P' 'O' 'S'

/// <summary>One plan input: segment properties and user features.</summary>
type private PlanCase =
    { Props: SegmentProperties
      Features: Feature array }

/// <summary>The three plan cases the differential compares.</summary>
let private planCases () : PlanCase array =
    [| { Props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }; Features = [||] }
       { Props = { Direction = Direction.Ttb; Script = Scripts.Han; Language = NoLanguage }; Features = [||] }
       { Props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
         Features =
             [| { Tag = Tag.make 'l' 'i' 'g' 'a'; Value = 0u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
                { Tag = Tag.make 's' 'm' 'c' 'p'; Value = 2u; Start = 2u; End = 5u } |] } |]

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Builds a Farfbuzz OpenType face for a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The OpenType face.</returns>
let private otFaceOf (font: TestFont) (index: uint32) =
    OtFace.create (faceOf font index)

/// <summary>Whether a table carries feature variations, which phase 7 evaluates.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="table">The table tag.</param>
/// <returns>True when the table has a non-empty feature-variations field.</returns>
let private hasFeatureVariations (font: TestFont) (index: uint32) (table: Tag) : bool =
    match table = gsubTag with
    | true -> not (otFaceOf font index).Gsub.Value.FeatureVariations.IsEmpty
    | false -> not (otFaceOf font index).Gpos.Value.FeatureVariations.IsEmpty

/// <summary>Builds a Farfbuzz plan for a face and case.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="case">The plan case.</param>
/// <returns>The plan.</returns>
let private farfbuzzPlan (font: TestFont) (index: uint32) (case: PlanCase) : OtShapePlan =
    let font = Font.create (faceOf font index)
    OtPlan.create font case.Props (ImmutableArray.CreateRange case.Features)

/// <summary>Builds the oracle segment properties of a plan case.</summary>
/// <param name="case">The plan case.</param>
/// <returns>The native properties.</returns>
let private nativeProps (case: PlanCase) : NativeSegmentProperties =
    let mutable props = NativeSegmentProperties()
    props.Direction <- int (Direction.value case.Props.Direction)
    props.Script <- uint32 case.Props.Script
    props.Language <-
        match case.Props.Language with
        | Language.NoLanguage -> 0n
        | Language.Bcp47 text ->
            let bytes = Text.Encoding.ASCII.GetBytes text
            Native.hb_language_from_string(bytes, bytes.Length)
    props

/// <summary>Creates an oracle plan for a face and case.</summary>
/// <param name="face">The oracle face handle.</param>
/// <param name="case">The plan case.</param>
/// <returns>The plan handle.</returns>
let private oraclePlan (face: nativeint) (case: PlanCase) : nativeint =
    let mutable props = nativeProps case
    Native.hb_shape_plan_create(face, &props, case.Features, uint32 case.Features.Length, [| "ot"; Unchecked.defaultof<string> |])

/// <summary>Reads a plan's feature tags through the oracle.</summary>
/// <param name="plan">The plan handle.</param>
/// <returns>The tags.</returns>
let private oracleFeatureTags (plan: nativeint) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_shape_plan_get_feature_tags(plan, 0u, &count, Array.empty)
    let tags = Array.zeroCreate (int total)
    let mutable written = uint32 tags.Length
    Native.hb_ot_shape_plan_get_feature_tags(plan, 0u, &written, tags) |> ignore
    tags |> Array.toList

/// <summary>Reads a plan's lookup indices through the oracle.</summary>
/// <param name="plan">The plan handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <returns>The lookup indices, ascending.</returns>
let private oracleCollectLookups (plan: nativeint) (tableTag: uint32) : int list =
    let set = Native.hb_set_create ()
    Native.hb_ot_shape_plan_collect_lookups(plan, tableTag, set)
    let rec members (current: uint32) (found: int list) =
        let mutable next = current
        match Native.hb_set_next(set, &next) with
        | 0 -> List.rev found
        | _ -> members next (int next :: found)
    let result = members UInt32.MaxValue []
    Native.hb_set_destroy set
    result

/// <summary>Creates an oracle plan, reads its feature tags and releases it.</summary>
/// <param name="face">The oracle face handle.</param>
/// <param name="case">The plan case.</param>
/// <returns>The tags.</returns>
let private oraclePlanFeatureTags (face: nativeint) (case: PlanCase) : uint32 list =
    let plan = oraclePlan face case
    let tags = oracleFeatureTags plan
    Native.hb_shape_plan_destroy plan
    tags

/// <summary>Creates an oracle plan, reads its lookup indices and releases it.</summary>
/// <param name="face">The oracle face handle.</param>
/// <param name="case">The plan case.</param>
/// <param name="tableTag">The table tag.</param>
/// <returns>The lookup indices.</returns>
let private oraclePlanCollectLookups (face: nativeint) (case: PlanCase) (tableTag: Tag) : int list =
    let plan = oraclePlan face case
    let lookups = oracleCollectLookups plan (uint32 tableTag)
    Native.hb_shape_plan_destroy plan
    lookups

/// <summary>Whether the oracle accepts a table as present.</summary>
/// <param name="face">The oracle face handle.</param>
/// <param name="table">The table tag.</param>
/// <returns>True when the table has data.</returns>
let private oracleValidTable (face: nativeint) (table: Tag) : bool =
    match table = gsubTag with
    | true -> Native.hb_ot_layout_has_substitution face <> 0
    | false -> Native.hb_ot_layout_has_positioning face <> 0

/// <summary>Every face with a GSUB or GPOS table, with the plan cases and both tables.</summary>
let private planInputs () =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let oracleFace = OracleFaces.face font index
        seq {
            let hasGsub = oracleValidTable oracleFace gsubTag
            let hasGpos = oracleValidTable oracleFace gposTag
            match hasGsub || hasGpos with
            | false -> ()
            | true ->
                match hasFeatureVariations font index gsubTag || hasFeatureVariations font index gposTag with
                | true -> ()
                | false -> for case in planCases () do yield struct (font, index, case)
        })

/// <summary>Every face with GSUB, its plan cases, for the collect-lookups differential.</summary>
let private gsubPlanInputs () =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let oracleFace = OracleFaces.face font index
        seq {
            match oracleValidTable oracleFace gsubTag && not (hasFeatureVariations font index gsubTag) with
            | false -> ()
            | true -> for case in planCases () do yield struct (font, index, case)
        })

/// <summary>Every face with GPOS, its plan cases, for the collect-lookups differential.</summary>
let private gposPlanInputs () =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let oracleFace = OracleFaces.face font index
        seq {
            match oracleValidTable oracleFace gposTag && not (hasFeatureVariations font index gposTag) with
            | false -> ()
            | true -> for case in planCases () do yield struct (font, index, case)
        })

/// <summary>Reads a big-endian uint16 from a byte array.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private u16At (bytes: byte array) (at: int) : uint32 =
    (uint32 bytes[at] <<< 8) ||| uint32 bytes[at + 1]

/// <summary>Writes a big-endian uint16 into a byte array.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="value">The value.</param>
let private writeU16 (bytes: byte array) (at: int) (value: uint32) =
    bytes[at] <- byte (value >>> 8)
    bytes[at + 1] <- byte value

/// <summary>The deep field offsets of a GSUB or GPOS table, from its base offset.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="baseOff">The table's base offset.</param>
/// <returns>The descriptions and offsets of the fields to mutate.</returns>
let private fieldOffsets (bytes: byte array) (baseOff: int) : (string * int) list =
    let scriptList = baseOff + int (u16At bytes (baseOff + 4))
    let featureList = baseOff + int (u16At bytes (baseOff + 6))
    let scriptCount = int (u16At bytes scriptList)
    let fields =
        match scriptCount > 0 with
        | false -> [ "script count", scriptList ]
        | true ->
            let recordOffset = int (u16At bytes (scriptList + 6))
            let scriptTable = scriptList + recordOffset
            let langSys = scriptTable + int (u16At bytes scriptTable)
            [ "script count", scriptList
              "default language-system offset", scriptTable
              "required feature index", langSys + 2 ]
    let featureCount = int (u16At bytes featureList)
    match featureCount > 0 with
    | false -> fields
    | true ->
        let featureTable = featureList + int (u16At bytes (featureList + 6))
        fields @ [ "feature lookup count", featureTable + 2; "lookup index", featureTable + 4 ]

/// <summary>Compares the plan queries of Farfbuzz and the oracle over the same bytes.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="index">The face index.</param>
/// <returns>The mismatch description, or none.</returns>
let private comparePlanQueries (bytes: byte array) (index: uint32) : string option =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let oracleFace = Native.hb_face_create(blob, index)
    let farfbuzz = Face.create (ReadOnlyMemory bytes) index
    let font = Font.create farfbuzz
    let result =
        seq {
            for case in planCases () do
                let plan = OtPlan.create font case.Props (ImmutableArray.CreateRange case.Features)
                let oracle = oraclePlan oracleFace case
                let ourTags = LayoutApi.planFeatureTags plan |> Seq.map uint32 |> Seq.toList
                match ourTags <> oracleFeatureTags oracle with
                | true -> yield "plan feature tags"
                | false ->
                    let ourGsub = LayoutApi.planCollectLookups plan gsubTag |> Seq.toList
                    match ourGsub <> oracleCollectLookups oracle (uint32 gsubTag) with
                    | true -> yield "plan GSUB lookups"
                    | false ->
                        let ourGpos = LayoutApi.planCollectLookups plan gposTag |> Seq.toList
                        match ourGpos <> oracleCollectLookups oracle (uint32 gposTag) with
                        | true -> yield "plan GPOS lookups"
                        | false -> ()
                Native.hb_shape_plan_destroy oracle
        }
        |> Seq.tryHead
    Native.hb_face_destroy oracleFace
    Native.hb_blob_destroy blob
    result

/// <summary>The upstream fonts the mutation differential rewrites.</summary>
let private mutationFonts () : (TestFont * uint32) list =
    let upstream = RepositoryPaths.upstream ()
    [ for name in [ "NotoSans-Bold.ttf"; "Mada-VF.ttf"; "Mplus1p-Regular.ttf" ] do
          yield ({ Path = Path.Combine(upstream, "test", "api", "fonts", name); Suite = "api"; Index = 0 }, 0u) ]

/// <summary>The P5.S04 OpenType map and plan differentials.</summary>
let tests () =
    testList "P5.S04" [
        exhaustive "plan feature tags" (planInputs ())
            (fun struct (font, index, case) -> farfbuzzPlan font index case |> LayoutApi.planFeatureTags |> Seq.map uint32 |> Seq.toList)
            (fun struct (font, index, case) -> oraclePlanFeatureTags (OracleFaces.face font index) case)
        exhaustive "plan GSUB collect lookups" (gsubPlanInputs ())
            (fun struct (font, index, case) -> farfbuzzPlan font index case |> fun plan -> LayoutApi.planCollectLookups plan gsubTag |> Seq.toList)
            (fun struct (font, index, case) -> oraclePlanCollectLookups (OracleFaces.face font index) case gsubTag)
        exhaustive "plan GPOS collect lookups" (gposPlanInputs ())
            (fun struct (font, index, case) -> farfbuzzPlan font index case |> fun plan -> LayoutApi.planCollectLookups plan gposTag |> Seq.toList)
            (fun struct (font, index, case) -> oraclePlanCollectLookups (OracleFaces.face font index) case gposTag)
        testCase "P5.S04 mutated GSUB and GPOS tables" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutations = 0
            for (font, index) in mutationFonts () do
                let bytes = OracleFaces.data font.Path
                let face = Face.create (ReadOnlyMemory bytes) index
                for table in [ gsubTag; gposTag ] do
                    match Face.tryFindTable face table with
                    | ValueNone -> ()
                    | ValueSome record when record.Length = 0u -> ()
                    | ValueSome record ->
                        let baseOff = int (face.BaseOffset + record.Offset)
                        for description, at in fieldOffsets bytes baseOff do
                            let original = u16At bytes at
                            for mutated in [ 0u; 1u; original + 1u; 0xFFFFu ] do
                                let candidate = Array.copy bytes
                                writeU16 candidate at mutated
                                match comparePlanQueries candidate index with
                                | None -> ()
                                | Some mismatch -> failures.Add $"{Path.GetFileName font.Path} face {index} {Tag.toString table} {description} (value {original} -> {mutated}): {mismatch}"
                                mutations <- mutations + 1
            printfn "P5.S04 compared %d mutated GSUB/GPOS plan tables" mutations
            Expect.isEmpty failures ("mutated GSUB/GPOS plan differences:\n" + String.concat "\n" failures))
    ]
