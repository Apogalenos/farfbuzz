module Farfbuzz.Diff.ComplexShapers.PlanDiff

open System
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Oracle
open Farfbuzz.Oracle.Native
open Farfbuzz.Diff
open Farfbuzz.Diff.ComplexShapers
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>A string option's UTF-8 bytes, or null for an absent option.</summary>
/// <param name="value">The option value.</param>
/// <returns>The bytes, or null.</returns>
let private utf8 (value: string voption) : byte array =
    match value with
    | ValueSome text -> Encoding.UTF8.GetBytes text
    | ValueNone -> Unchecked.defaultof<byte array>

/// <summary>The byte length to pass with <see cref="utf8"/>: -1 for null, which HarfBuzz treats as invalid.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The length.</returns>
let private lengthOf (bytes: byte array) =
    match obj.ReferenceEquals(bytes, null) with
    | true -> -1
    | false -> bytes.Length

/// <summary>The native segment properties of a request, read through the oracle's string parsers.</summary>
/// <param name="request">The request.</param>
/// <returns>The properties.</returns>
let private nativeProps (request: ShapeRequest) : NativeSegmentProperties =
    let mutable props = NativeSegmentProperties()
    let direction = utf8 request.Direction
    let script = utf8 request.Script
    let language = Encoding.UTF8.GetBytes request.Language
    props.Direction <- Native.hb_direction_from_string(direction, lengthOf direction)
    props.Script <- Native.hb_script_from_string(script, lengthOf script)
    props.Language <- Native.hb_language_from_string(language, language.Length)
    props

/// <summary>Reads a plan's feature tags through the oracle.</summary>
/// <param name="plan">The plan handle.</param>
/// <returns>The tags in compiled order.</returns>
let private featureTags (plan: nativeint) : uint32 array =
    let mutable count = 0u
    let total = Native.hb_ot_shape_plan_get_feature_tags(plan, 0u, &count, Array.empty)
    let tags = Array.zeroCreate (int total)
    let mutable written = uint32 tags.Length
    Native.hb_ot_shape_plan_get_feature_tags(plan, 0u, &written, tags) |> ignore
    tags

/// <summary>Reads a plan's lookup indices through the oracle.</summary>
/// <param name="plan">The plan handle.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <returns>The lookup indices, ascending.</returns>
let private collectLookups (plan: nativeint) (tableTag: uint32) : int array =
    let set = Native.hb_set_create ()
    Native.hb_ot_shape_plan_collect_lookups(plan, tableTag, set)
    let rec members (current: uint32) (found: int list) =
        let mutable next = current
        match Native.hb_set_next(set, &next) with
        | 0 -> List.rev found
        | _ -> members next (int next :: found)
    let result = members UInt32.MaxValue [] |> List.toArray
    Native.hb_set_destroy set
    result

/// <summary>The feature tags and the GSUB and GPOS lookup sets of a plan built for a font and options.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="options">The option field.</param>
/// <returns>The plan's feature tags and lookup sets.</returns>
let private queryPlan (fontFile: string) (options: string) : struct (uint32 array * int array * int array) =
    let request = ShapeDiff.request fontFile options [||]
    let mutable props = nativeProps request
    let face = OracleFaces.face { Path = fontFile; Suite = String.Empty; Index = 0 } 0u
    let plan = Native.hb_shape_plan_create(face, &props, Unchecked.defaultof<Feature array>, 0u, [| "ot"; Unchecked.defaultof<string> |])
    let tags = featureTags plan
    let gsub = collectLookups plan (uint32 Tags.Gsub)
    let gpos = collectLookups plan (uint32 Tags.Gpos)
    Native.hb_shape_plan_destroy plan
    struct (tags, gsub, gpos)

/// <summary>
/// Compares the feature tags and the GSUB/GPOS lookup sets of <c>hb_shape_plan_create</c> for each font and options
/// pair.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="cases">The font path and options pairs.</param>
/// <returns>The test.</returns>
let features (name: string) (cases: (string * string) list) : Test =
    Differential.exhaustive name cases (fun (fontFile, options) -> queryPlan fontFile options) (fun (fontFile, options) -> queryPlan fontFile options)

/// <summary>The P6.S02 plan self-check: the oracle against itself over a few fonts and directions.</summary>
/// <returns>The test.</returns>
let tests () : Test =
    features "plan self"
        [ (TestFonts.named "54674a3111d209fb6be0ed31745314b7a8d2c244.ttf").Path, String.Empty
          (TestFonts.named "54674a3111d209fb6be0ed31745314b7a8d2c244.ttf").Path, "--direction=rtl"
          (TestFonts.named "Roboto-Regular.ttf").Path, String.Empty
          (TestFonts.named "1a5face3fcbd929d228235c2f72bbd6f8eb37424.ttf").Path, String.Empty ]

/// <summary>The feature tags and the GSUB and GPOS lookup sets of the plan Farfbuzz builds for a font and options, in the
/// shapes <see cref="queryPlan"/> reads from the oracle.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="options">The option field.</param>
/// <returns>The plan's feature tags and lookup sets, or why there is no plan.</returns>
let private queryFarfbuzzPlan (fontFile: string) (options: string) : Result<struct (uint32 array * int array * int array), string> =
    let request = ShapeDiff.request fontFile options [||]
    let face = Face.create (ReadOnlyMemory (OracleFaces.data fontFile)) 0u
    let direction = match request.Direction with | ValueSome text -> Direction.ofString text | ValueNone -> Direction.Invalid
    let script = match request.Script with | ValueSome text -> Script.ofString text | ValueNone -> Scripts.Invalid
    let language = Language.ofString request.Language
    let props = { Direction = direction; Script = script; Language = language }
    match Execute.createPlan (Font.create face) props System.Collections.Immutable.ImmutableArray.Empty ValueNone with
    | Error error -> Error $"{error}"
    | Ok plan ->
        match plan.Ot with
        | ValueNone -> Error "no ot plan"
        | ValueSome ot ->
            let tags = LayoutApi.planFeatureTags ot |> Seq.map (fun tag -> uint32 tag) |> Seq.toArray
            let gsub = LayoutApi.planCollectLookups ot Tags.Gsub |> Seq.toArray
            let gpos = LayoutApi.planCollectLookups ot Tags.Gpos |> Seq.toArray
            Ok (struct (tags, gsub, gpos))

/// <summary>
/// Compares the feature tags and the GSUB/GPOS lookup sets of the plan Farfbuzz builds with those of HarfBuzz's
/// <c>hb_shape_plan_create</c>, for each font and options pair.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="cases">The font path and options pairs.</param>
/// <returns>The test.</returns>
let featuresAgainst (name: string) (cases: (string * string) list) : Test =
    Differential.exhaustive name cases
        (fun (fontFile, options) -> queryFarfbuzzPlan fontFile options)
        (fun (fontFile, options) -> Ok (queryPlan fontFile options))
