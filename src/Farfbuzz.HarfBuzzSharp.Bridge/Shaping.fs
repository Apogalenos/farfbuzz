namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>What shaping into the caller's arrays produced: how many glyphs and positions were written, the length the arrays need when they were
/// too short (then nothing was written), the random state, and the reason when no glyphs came out.</summary>
[<Struct>]
type IntoResult =
    { Count: int
      PositionCount: int
      Needed: int
      RandomState: uint32
      Failure: string }

module internal ShapingImpl =
    /// <summary>The context as an immutable array, nearest first.</summary>
    /// <param name="context">The context.</param>
    /// <returns>The code points.</returns>
    let contextArray (context: Context) =
        match context.Length with
        | 0 -> ImmutableArray<uint32>.Empty
        | 1 -> Sealed.publishArray [| context.C0 |]
        | 2 -> Sealed.publishArray [| context.C0; context.C1 |]
        | 3 -> Sealed.publishArray [| context.C0; context.C1; context.C2 |]
        | 4 -> Sealed.publishArray [| context.C0; context.C1; context.C2; context.C3 |]
        | _ -> Sealed.publishArray [| context.C0; context.C1; context.C2; context.C3; context.C4 |]

    /// <summary>The cluster level of a <c>hb_buffer_cluster_level_t</c> value.</summary>
    /// <param name="level">The value, 0 to 3.</param>
    /// <returns>The core level; an unknown value is the default level.</returns>
    let clusterLevel (level: int) =
        match level with
        | 1 -> ClusterLevel.MonotoneCharacters
        | 2 -> ClusterLevel.Characters
        | 3 -> ClusterLevel.Graphemes
        | _ -> ClusterLevel.MonotoneGraphemes

    /// <summary>The run that holds a buffer's contexts, flags and segment properties, with no glyphs and no properties: the settings of a line shaped
    /// from glyphs the caller holds.</summary>
    /// <param name="props">The segment properties.</param>
    /// <param name="pre">The pre-context.</param>
    /// <param name="post">The post-context.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="invisible">The invisible glyph.</param>
    /// <param name="randomState">The random state.</param>
    /// <returns>The run.</returns>
    let settingsOf (props: SegmentProperties) (pre: Context) (post: Context) (flags: uint32) (level: int) (replacement: uint32) (invisible: uint32) (randomState: uint32) =
        { GlyphRun.empty with
            ContextBefore = contextArray pre
            ContextAfter = contextArray post
            ContentType = UnicodeContent
            SegmentProperties = props
            Flags = LanguagePrimitives.EnumOfValue<uint32, BufferFlags> flags
            ClusterLevel = clusterLevel level
            Replacement = replacement
            Invisible = invisible
            RandomState = randomState }

    /// <summary>A shaper list as the core takes it: none when the caller gave none.</summary>
    /// <param name="shapers">The shaper names.</param>
    /// <returns>The list, or none.</returns>
    let shaperList (shapers: string array | null) =
        match shapers with
        | null -> ValueNone
        | names when names.Length = 0 -> ValueNone
        | names -> ValueSome (ImmutableArray.CreateRange names)

    /// <summary>Why shaping failed, without printing the union by reflection.</summary>
    /// <param name="error">The error.</param>
    /// <returns>The description.</returns>
    let failure (error: ShapeError) =
        match error with
        | OutlinesNotPorted _ -> "outlines are not ported: the glyph outlines this shaping needs cannot be read yet"
        | NoShaperSucceeded _ -> "no requested shaper succeeded"
        | VerifyFailed _ -> "verification of the shaped buffer failed"

/// <summary>Shaping for the HarfBuzzSharp shell: plan keys, plans and execution (hb-shape.cc:127-148, hb-shape-plan.cc).</summary>
[<AbstractClass; Sealed>]
type internal Shaping =
    /// <summary>The shapers this build can use, <c>hb_shape_list_shapers</c>.</summary>
    static member SupportedShapers =
        Shape.listShapers ()

    /// <summary>The key of the plan a shape call needs, <c>hb_shape_plan_key_t::init</c> (hb-shape-plan.cc:158-185); the variation state of the font is part of it,
    /// and only the shaper the list picks first is kept, as <c>hb_shape_plan_create</c> picks it.</summary>
    /// <param name="font">The core font, whose variation coordinates the key reads.</param>
    /// <param name="props">The segment properties of the buffer.</param>
    /// <param name="features">The features of the call.</param>
    /// <param name="shapers">The requested shapers, null or empty for the default list.</param>
    /// <returns>The key, or none when no shaper of the list is supported (<c>hb_shape_full</c> then fails and leaves the buffer).</returns>
    static member PlanKey (font: Font, props: SegmentProps, features: ReadOnlySpan<Feature>, shapers: string array | null) =
        let key =
            match Execute.shaperName (ShapingImpl.shaperList shapers) with
            | ValueNone -> ValueNone
            | ValueSome name -> ShapePlan.key font (Segments.ToCore props) features name
        match key with
        | ValueSome found -> ValueSome found
        | ValueNone -> ValueNone

    /// <summary>Builds the plan of a key, <c>hb_shape_plan_create</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="key">The key.</param>
    /// <returns>The plan, or none and the reason.</returns>
    static member CreatePlan (font: Font, key: ShapePlanKey) =
        match ShapePlan.create font key.Props key.Features (ValueSome (ImmutableArray.Create key.Shaper)) with
        | Ok plan -> struct (ValueSome plan, String.Empty)
        | Error error -> struct (ValueNone, ShapingImpl.failure error)

    /// <summary>Shapes the glyphs of a buffer with a plan into arrays the buffer owns, <see cref="Execute"/> without the run and the copies the
    /// run needs: the result equals the glyphs and positions <c>Execute</c> gives. The arrays may be the array the input is in. When an array is
    /// shorter than the shaped line nothing is written and <c>Needed</c> is the length both must have.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="input">The glyph infos of the buffer: code points and clusters.</param>
    /// <param name="props">The segment properties of the buffer in the core's form.</param>
    /// <param name="pre">The pre-context.</param>
    /// <param name="post">The post-context.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="clusterLevel">The cluster level.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="invisible">The invisible glyph.</param>
    /// <param name="randomState">The random state.</param>
    /// <param name="unicode">The Unicode functions of the buffer when it has replaced ones, else null for the default functions.</param>
    /// <param name="infos">The array the shaped glyphs are written into.</param>
    /// <param name="positions">The array the positions are written into.</param>
    /// <returns>The counts written, or the length needed, or the reason in <c>Failure</c>.</returns>
    static member ExecuteInto (font: Font, plan: ShapePlan, input: ReadOnlySpan<GlyphInfo>, props: SegmentProperties, pre: Context, post: Context, flags: uint32, clusterLevel: int, replacement: uint32, invisible: uint32, randomState: uint32, unicode: UnicodeFuncs | null, infos: Span<GlyphInfo>, positions: Span<GlyphPosition>) =
        let settings = ShapingImpl.settingsOf props pre post flags clusterLevel replacement invisible randomState
        let executed =
            match unicode with
            | null -> plan
            | replaced -> Execute.withUnicode replaced plan
        let outcome = LineShape.shapeInto executed font settings input infos positions |> Settled.value
        match outcome with
        | IntoShaped (count, positionCount, state, _) -> { Count = count; PositionCount = positionCount; Needed = 0; RandomState = state; Failure = String.Empty }
        | IntoTooShort needed -> { Count = 0; PositionCount = 0; Needed = needed; RandomState = randomState; Failure = String.Empty }
        | IntoRefused error -> { Count = 0; PositionCount = 0; Needed = 0; RandomState = randomState; Failure = ShapingImpl.failure error }
