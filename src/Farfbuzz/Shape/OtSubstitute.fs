namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Aat

/// <summary>The substitution stages of the ot pipeline: the default and plan compositions and the post hook.</summary>
[<RequireQualifiedAccess>]
module internal OtSubstitute =
    /// <summary>The fraction slash, <c>0x2044</c>.</summary>
    [<Literal>]
    let private FractionSlash = 0x2044u

    /// <summary>The invalid code point, <c>HB_CODEPOINT_INVALID</c>.</summary>
    [<Literal>]
    let private CodepointInvalid = 0xFFFFFFFFu

    /// <summary>The space code point, the source of the invisible glyph, <c>0x0020</c>.</summary>
    [<Literal>]
    let private Space = 0x0020u

    /// <summary>Sets every glyph's GDEF class and clears its ligature properties, <c>_hb_ot_layout_set_glyph_props</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the glyph classes set.</returns>
    let substituteStart (font: Font) (run: GlyphRun) =
        match Gdef.hasGlyphClasses font.Ot.Gdef.Value with
        | false -> run
        | true ->
            let props =
                Sealed.mapi (fun index (info: GlyphInfo) ->
                    { run.Props[index] with
                        GlyphClass = OtLayout.glyphProps font info.Codepoint
                        LigProps = 0uy }) (run.Infos.AsSpan())
            { run with Props = props }

    /// <summary>Whether a direction is vertical, <c>HB_DIRECTION_IS_VERTICAL</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>True for TTB and BTT.</returns>
    let private isVertical (direction: Direction) : bool =
        match direction with
        | Direction.Ttb | Direction.Btt -> true
        | Direction.Invalid | Direction.Ltr | Direction.Rtl -> false

    /// <summary>Whether a font maps a code point, <c>hb_font_has_glyph</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the font has a nominal glyph for it.</returns>
    let private hasGlyph (font: Font) (codepoint: uint32) : bool =
        match Font.nominalGlyph font codepoint with
        | ValueSome _ -> true
        | ValueNone -> false

    /// <summary>Mirrors and rotates the code points for the target direction, <c>hb_ot_rotate_chars</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="targetDirection">The original direction of the run.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with mirrored or rotated code points.</returns>
    let rotateChars (plan: OtShapePlan) (font: Font) (targetDirection: Direction) (run: GlyphRun) : GlyphRun =
        let backward = Direction.isBackward targetDirection
        let vertical = isVertical targetDirection && not plan.HasVert
        match backward || vertical with
        | false -> run
        | true ->
            let mapping (info: GlyphInfo) =
                let info =
                    match backward with
                    | true ->
                        let mirrored =
                            match plan.Unicode with
                            | ValueNone -> Unicode.mirroring info.Codepoint
                            | ValueSome unicode -> unicode.Mirroring info.Codepoint
                        match mirrored <> info.Codepoint && hasGlyph font mirrored with
                        | true -> { info with Codepoint = mirrored }
                        | false -> { info with Mask = info.Mask ||| plan.RtlmMask }
                    | false -> info
                match vertical with
                | true ->
                    let verticalChar = Unicode.verticalCharFor info.Codepoint
                    match verticalChar <> info.Codepoint && hasGlyph font verticalChar with
                    | true -> { info with Codepoint = verticalChar }
                    | false -> info
                | false -> info
            { run with Infos = Sealed.mapIfChanged mapping run.Infos }

    /// <summary>Extends the fraction start backward over decimal digits, <c>hb_ot_shape_setup_masks_fraction</c>'s first scan.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The fraction slash index.</param>
    /// <returns>The first digit of the fraction.</returns>
    [<TailCall>]
    let rec private fractionStart (props: ReadOnlySpan<GlyphProps>) (index: int) : int =
        match index > 0 && UnicodeProps.generalCategory props[index - 1] = GeneralCategory.DecimalNumber with
        | true -> fractionStart props (index - 1)
        | false -> index

    /// <summary>Extends the fraction finish forward over decimal digits, <c>hb_ot_shape_setup_masks_fraction</c>'s second scan.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="count">The number of glyphs.</param>
    /// <param name="index">The index after the fraction slash.</param>
    /// <returns>The index after the last digit.</returns>
    [<TailCall>]
    let rec private fractionFinish (props: ReadOnlySpan<GlyphProps>) (count: int) (index: int) : int =
        match index < count && UnicodeProps.generalCategory props[index] = GeneralCategory.DecimalNumber with
        | true -> fractionFinish props count (index + 1)
        | false -> index

    /// <summary>Builds the edits of one fraction, <c>hb_ot_shape_setup_masks_fraction</c>'s per-slash branch.</summary>
    /// <param name="preMask">The numerator-side mask.</param>
    /// <param name="fracMask">The slash mask.</param>
    /// <param name="postMask">The denominator-side mask.</param>
    /// <param name="start">The fraction start.</param>
    /// <param name="index">The slash index.</param>
    /// <param name="finish">The fraction finish.</param>
    /// <returns>The edits.</returns>
    let private fractionEdits (preMask: uint32) (fracMask: uint32) (postMask: uint32) (start: int) (index: int) (finish: int) : ImmutableArray<RunEdit> =
        match start = index || finish = index + 1 with
        | true ->
            let first =
                match start = index with
                | true -> Sealed.ofSpan (ReadOnlySpan [| RunEdit.UnsafeToConcat (start, start + 1) |])
                | false -> ImmutableArray.Empty
            let second =
                match finish = index + 1 with
                | true -> Sealed.ofSpan (ReadOnlySpan [| RunEdit.UnsafeToConcat (finish - 1, finish) |])
                | false -> ImmutableArray.Empty
            Sealed.appendSpan (first.AsSpan()) (second.AsSpan())
        | false ->
            Sealed.ofSpan (ReadOnlySpan [|
                RunEdit.UnsafeToBreak (start, finish)
                RunEdit.OrMask (preMask, start, index)
                RunEdit.OrMask (fracMask, index, index + 1)
                RunEdit.OrMask (postMask, index + 1, finish)
            |])

    /// <summary>Builds the set-masks edits of the non-global user features, <c>hb_ot_shape_setup_masks</c>'s feature loop.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="features">The user features.</param>
    /// <returns>The edits.</returns>
    let private userFeatureEdits (map: OtMap) (features: ImmutableArray<Feature>) : ImmutableArray<RunEdit> =
        Sealed.choose (fun (feature: Feature) ->
            match feature.Start = Feature.GlobalStart && feature.End = Feature.GlobalEnd with
            | true -> ValueNone
            | false ->
                let struct (mask, shift) = OtMap.mask map feature.Tag
                ValueSome (RunEdit.SetMasks (feature.Value <<< shift, mask, feature.Start, feature.End))) (features.AsSpan())

    /// <summary>Applies one feature's mask to one glyph, <c>set_masks</c>'s per-glyph update.</summary>
    /// <param name="value">The mask value, already shifted.</param>
    /// <param name="mask">The mask selecting the bits.</param>
    /// <param name="clusterStart">The first cluster to match.</param>
    /// <param name="clusterEnd">The cluster after the last to match.</param>
    /// <param name="cluster">The glyph's cluster.</param>
    /// <param name="current">The glyph's current mask.</param>
    /// <returns>The glyph's updated mask.</returns>
    let private setMasksMask (value: uint32) (mask: uint32) (clusterStart: uint32) (clusterEnd: uint32) (cluster: uint32) (current: uint32) : uint32 =
        match mask <> 0u && clusterStart <= cluster && cluster < clusterEnd with
        | false -> current
        | true -> (current &&& ~~~mask) ||| (value &&& mask)

    /// <summary>Applies every user-feature mask to one glyph's mask, in feature order.</summary>
    /// <param name="edits">The set-masks edits, in order.</param>
    /// <param name="index">The next edit.</param>
    /// <param name="cluster">The glyph's cluster.</param>
    /// <param name="current">The glyph's current mask.</param>
    /// <returns>The glyph's final mask.</returns>
    [<TailCall>]
    let rec private applyUserMasks (edits: ImmutableArray<RunEdit>) (index: int) (cluster: uint32) (current: uint32) : uint32 =
        match index < edits.Length with
        | false -> current
        | true ->
            let edit = edits[index]
            match edit.Kind with
            | EditKind.SetMasks -> applyUserMasks edits (index + 1) cluster (setMasksMask edit.W0 edit.W1 edit.W2 edit.W3 cluster current)
            | _ -> applyUserMasks edits (index + 1) cluster current

    /// <summary>Decrements the operation budget once per non-zero feature mask, <c>set_masks</c>'s budget line.</summary>
    /// <param name="edits">The set-masks edits, in order.</param>
    /// <param name="index">The next edit.</param>
    /// <param name="len">The run length.</param>
    /// <param name="maxOps">The budget so far.</param>
    /// <param name="successful">Whether the run has not failed yet.</param>
    /// <returns>The final budget and success flag.</returns>
    [<TailCall>]
    let rec private applyUserMaxOps (edits: ImmutableArray<RunEdit>) (index: int) (len: int) (maxOps: int) (successful: bool) : struct (int * bool) =
        match index < edits.Length with
        | false -> struct (maxOps, successful)
        | true ->
            let edit = edits[index]
            match edit.Kind with
            | EditKind.SetMasks ->
                match edit.W1 <> 0u with
                | true ->
                    let maxOps = maxOps - len
                    applyUserMaxOps edits (index + 1) len maxOps (match maxOps < 0 with | true -> false | false -> successful)
                | false -> applyUserMaxOps edits (index + 1) len maxOps successful
            | _ -> applyUserMaxOps edits (index + 1) len maxOps successful

    /// <summary>Applies the non-global user-feature masks directly, without the pass, when the run has no fraction slash.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the user-feature masks set, the same instance when there are none.</returns>
    let private setupMasksNoFraction (map: OtMap) (features: ImmutableArray<Feature>) (run: GlyphRun) : GlyphRun =
        let edits = userFeatureEdits map features
        match edits.Length with
        | 0 -> run
        | _ ->
            let struct (maxOps, successful) = applyUserMaxOps edits 0 run.Infos.Length run.MaxOps run.Successful
            let infos = Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = applyUserMasks edits 0 info.Cluster info.Mask }) (run.Infos.AsSpan())
            { run with Infos = infos; MaxOps = maxOps; Successful = successful }

    /// <summary>The state of the setup-masks pass.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private SetupMasksState =
        | SetupScan
        | SetupPending of resume: int * edits: ImmutableArray<RunEdit> * editIndex: int
        | SetupFeatures of edits: ImmutableArray<RunEdit> * editIndex: int
        | SetupDone

    /// <summary>The pass that applies the fraction masks and user-feature masks, <c>hb_ot_shape_setup_masks</c>.</summary>
    [<Struct>]
    type private SetupMasksPass =
        val HasFrac: bool
        val FracMask: uint32
        val PreMask: uint32
        val PostMask: uint32
        val Map: OtMap
        val UserFeatures: ImmutableArray<Feature>

        /// <summary>Creates a setup-masks pass.</summary>
        /// <param name="hasFrac">Whether fractions fire.</param>
        /// <param name="fracMask">The slash mask.</param>
        /// <param name="preMask">The numerator-side mask.</param>
        /// <param name="postMask">The denominator-side mask.</param>
        /// <param name="map">The compiled map.</param>
        /// <param name="userFeatures">The user features.</param>
        new (hasFrac, fracMask, preMask, postMask, map, userFeatures) =
            { HasFrac = hasFrac
              FracMask = fracMask
              PreMask = preMask
              PostMask = postMask
              Map = map
              UserFeatures = userFeatures }

        interface IRunPass<SetupMasksState> with
            member this.Step (cursor, state) =
                match state with
                | SetupDone -> struct (RunEdit.Finish, SetupDone)
                | SetupScan ->
                    match cursor.Index >= cursor.Input.Length with
                    | true ->
                        let edits = userFeatureEdits this.Map this.UserFeatures
                        match edits.Length with
                        | 0 -> struct (RunEdit.Finish, SetupDone)
                        | _ -> struct (edits[0], SetupFeatures (edits, 1))
                    | false ->
                        match this.HasFrac && cursor.Input[cursor.Index].Codepoint = FractionSlash with
                        | false -> struct (RunEdit.NextGlyph, SetupScan)
                        | true ->
                            let index = cursor.Index
                            let start = fractionStart cursor.InputProps index
                            let finish = fractionFinish cursor.InputProps cursor.Input.Length (index + 1)
                            let edits = fractionEdits this.PreMask this.FracMask this.PostMask start index finish
                            struct (edits[0], SetupPending (finish, edits, 1))
                | SetupPending (resume, edits, editIndex) ->
                    match editIndex < edits.Length with
                    | true -> struct (edits[editIndex], SetupPending (resume, edits, editIndex + 1))
                    | false -> struct (RunEdit.NextGlyphs (resume - cursor.Index), SetupScan)
                | SetupFeatures (edits, editIndex) ->
                    match editIndex < edits.Length with
                    | true -> struct (edits[editIndex], SetupFeatures (edits, editIndex + 1))
                    | false -> struct (RunEdit.Finish, SetupDone)

    /// <summary>Applies the fraction and user-feature masks through the out-buffer pass, <c>hb_ot_shape_setup_masks</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the masks set.</returns>
    let internal setupMasksPass (plan: OtShapePlan) (run: GlyphRun) : GlyphRun =
        let hasFrac = plan.HasFrac && run.ScratchFlags &&& ScratchFlags.HasFractionSlash <> ScratchFlags.Default
        let forward = not (Direction.isBackward run.SegmentProperties.Direction)
        let preMask = if forward then plan.NumrMask ||| plan.FracMask else plan.FracMask ||| plan.DnomMask
        let postMask = if forward then plan.FracMask ||| plan.DnomMask else plan.NumrMask ||| plan.FracMask
        GlyphRun.runPass (SetupMasksPass(hasFrac, plan.FracMask, preMask, postMask, plan.Map, plan.UserFeatures)) SetupScan run

    /// <summary>Applies the fraction and user-feature masks of a shaper without a <c>setup_masks</c> hook: a run without a
    /// fraction slash applies only the user features, directly, and skips the pass.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the masks set.</returns>
    let private setupMasksUnhooked (plan: OtShapePlan) (run: GlyphRun) =
        match plan.HasFrac && run.ScratchFlags &&& ScratchFlags.HasFractionSlash <> ScratchFlags.Default with
        | true -> setupMasksPass plan run
        | false -> setupMasksNoFraction plan.Map plan.UserFeatures run

    /// <summary>Applies the masks of <c>hb_ot_shape_setup_masks</c> in its order (src/hb-ot-shape.cc:753-770): the fraction
    /// masks, the shaper's <c>setup_masks</c> hook, then the user features. A shaper with the hook has the fraction part
    /// done alone, by the pass with no user features, and the user part after the hook; a shaper without it keeps the two
    /// as one.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the masks set.</returns>
    let setupMasks (plan: OtShapePlan) (run: GlyphRun) =
        match ShaperHooks.hasSetupMasks plan.Shaper.Kind with
        | false -> setupMasksUnhooked plan run
        | true ->
            let fractioned =
                match plan.HasFrac && run.ScratchFlags &&& ScratchFlags.HasFractionSlash <> ScratchFlags.Default with
                | true -> setupMasksPass { plan with UserFeatures = ImmutableArray.Empty } run
                | false -> run
            setupMasksNoFraction plan.Map plan.UserFeatures (ShaperHooks.setupMasks plan fractioned)

    /// <summary>Applies the masks of <see cref="setupMasks"/> to a run whose arrays the caller made in this call and shared with no one:
    /// the shaper's hook writes into the glyph array where it can.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run, with owned arrays.</param>
    /// <returns>The run with the masks set.</returns>
    let setupMasksOwned (plan: OtShapePlan) (run: GlyphRun) =
        match ShaperHooks.hasSetupMasks plan.Shaper.Kind with
        | false -> setupMasksUnhooked plan run
        | true ->
            let fractioned =
                match plan.HasFrac && run.ScratchFlags &&& ScratchFlags.HasFractionSlash <> ScratchFlags.Default with
                | true -> setupMasksPass { plan with UserFeatures = ImmutableArray.Empty } run
                | false -> run
            setupMasksNoFraction plan.Map plan.UserFeatures (ShaperHooks.setupMasksOwned plan fractioned)

    /// <summary>The user-feature masks of one glyph, <see cref="setupMasksNoFraction"/> for the glyph array of a working set.</summary>
    [<Struct>]
    type private UserMasksMapper =
        val Edits: ImmutableArray<RunEdit>

        /// <summary>Creates the mapper.</summary>
        /// <param name="edits">The set-masks edits, in order.</param>
        new (edits) = { Edits = edits }

        interface IGroupMapper<GlyphInfo> with
            member _.GroupEnd (_, start) = start + 1
            member this.Map (items, _, _, index) =
                let info = items[index]
                { info with Mask = applyUserMasks this.Edits 0 info.Cluster info.Mask }

    /// <summary>Applies the non-global user-feature masks in the glyph array of a working set whose arrays the caller made in this call, as
    /// <see cref="setupMasksNoFraction"/> does.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="features">The user features.</param>
    /// <param name="set">The working set.</param>
    /// <returns>The set with the user-feature masks set.</returns>
    let private setupMasksNoFractionSet (map: OtMap) (features: ImmutableArray<Feature>) (set: WorkingSet) =
        let edits = userFeatureEdits map features
        match edits.Length with
        | 0 -> set
        | _ ->
            let struct (maxOps, successful) = applyUserMaxOps edits 0 set.Infos.Length set.MaxOps set.Successful
            Sealed.mapOwned (UserMasksMapper(edits)) set.Infos
            { set with MaxOps = maxOps; Successful = successful }

    /// <summary>Applies the masks of <see cref="setupMasksOwned"/> to a working set whose arrays the caller made in this call and shared with
    /// no one: the shaper's hook and the user features write into its arrays, and only a line with a fraction slash goes through the pass
    /// of the out-buffer on a run.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="set">The working set, with owned arrays.</param>
    /// <returns>The set with the masks set.</returns>
    let setupMasksOwnedSet (plan: OtShapePlan) (set: WorkingSet) =
        let fraction = plan.HasFrac && set.ScratchFlags &&& ScratchFlags.HasFractionSlash <> ScratchFlags.Default
        match ShaperHooks.hasSetupMasks plan.Shaper.Kind, fraction with
        | false, true -> GlyphRun.setOf (setupMasksPass plan (GlyphRun.runOf set))
        | false, false -> setupMasksNoFractionSet plan.Map plan.UserFeatures set
        | true, true ->
            let fractioned = GlyphRun.setOf (setupMasksPass { plan with UserFeatures = ImmutableArray.Empty } (GlyphRun.runOf set))
            setupMasksNoFractionSet plan.Map plan.UserFeatures (ShaperHooks.setupMasksSet plan fractioned)
        | true, false -> setupMasksNoFractionSet plan.Map plan.UserFeatures (ShaperHooks.setupMasksSet plan set)

    /// <summary>Runs the default and plan substitution stages, <c>hb_ot_substitute_pre</c>.</summary>
    /// <summary>Runs the default and plan substitution stages that follow the normalizer, <c>hb_ot_substitute_pre</c>'s second half.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="normalized">The normalized run.</param>
    /// <returns>The substituted run.</returns>
    let afterNormalize plan font normalized : Result<GlyphRun, ShapeError> =
        let masked = setupMasks plan normalized
        let recategorized =
            match plan.FallbackMarkPositioning with
            | true -> OtLayout.recategorizeMarks masked
            | false -> masked
        let mapped = Normalize.mapGlyphsFast recategorized
        let started = substituteStart font mapped
        let synthesized =
            match plan.FallbackGlyphClasses with
            | true -> OtProps.synthesizeGlyphClasses started
            | false -> started
        let substituted =
            match plan.ApplyMorx with
            | true -> AatLayout.substitute plan font (plan.UserFeatures.AsSpan()) synthesized
            | false -> OtLayout.substitute plan font synthesized
        let removed =
            match plan.ApplyMorx && plan.ApplyGpos with
            | true -> AatLayout.removeDeletedGlyphs substituted
            | false -> substituted
        Ok removed

    /// <summary>Runs the default and plan substitution stages, <c>hb_ot_substitute_pre</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substitutePre plan font run =
        afterNormalize plan font (Normalize.run plan font (rotateChars plan font plan.Props.Direction run))

    /// <summary>Rewrites variation selectors to the not-found glyph, <c>hb_ot_deal_with_variation_selectors</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with variation selectors replaced.</returns>
    let dealWithVariationSelectors (run: GlyphRun) : GlyphRun =
        match run.ScratchFlags &&& ScratchFlags.HasVariationSelectorFallback = ScratchFlags.Default
              || run.NotFoundVariationSelector = CodepointInvalid with
        | true -> run
        | false ->
            let notFound = run.NotFoundVariationSelector
            let infos =
                Sealed.mapi (fun index (info: GlyphInfo) ->
                    match UnicodeProps.isVariationSelector run.Props[index] with
                    | false -> info
                    | true -> { info with Codepoint = notFound }) (run.Infos.AsSpan())
            let props =
                Sealed.mapi (fun _ props ->
                    match UnicodeProps.isVariationSelector props with
                    | false -> props
                    | true -> UnicodeProps.setVariationSelector false props) (run.Props.AsSpan())
            let positions =
                match run.HavePositions with
                | false -> run.Positions
                | true ->
                    Sealed.mapi (fun index (position: GlyphPosition) ->
                        match UnicodeProps.isVariationSelector run.Props[index] with
                        | false -> position
                        | true -> { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = position.Var }) (run.Positions.AsSpan())
            { run with Infos = infos; Positions = positions; Props = props }

    /// <summary>The deletion predicate of <c>hb_ot_hide_default_ignorables</c>, <c>_hb_glyph_info_is_default_ignorable</c>.</summary>
    [<Struct>]
    type private DefaultIgnorablePredicate =
        interface IGlyphPredicate with
            member _.Delete (_, props) = UnicodeProps.isDefaultIgnorable props

    /// <summary>Replaces or removes default-ignorable glyphs, <c>hb_ot_hide_default_ignorables</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with default ignorables hidden.</returns>
    let hideDefaultIgnorables (font: Font) (run: GlyphRun) : GlyphRun =
        match run.ScratchFlags &&& ScratchFlags.HasDefaultIgnorables = ScratchFlags.Default
              || run.Flags &&& BufferFlags.PreserveDefaultIgnorables <> BufferFlags.Default with
        | true -> run
        | false ->
            let replacement =
                match run.Flags &&& BufferFlags.RemoveDefaultIgnorables <> BufferFlags.Default with
                | true -> ValueNone
                | false ->
                    match run.Invisible <> 0u with
                    | true -> ValueSome run.Invisible
                    | false -> Font.nominalGlyph font Space
            match replacement with
            | ValueSome invisible ->
                let infos =
                    Sealed.mapi (fun index (info: GlyphInfo) ->
                        match UnicodeProps.isDefaultIgnorable run.Props[index] with
                        | false -> info
                        | true -> { info with Codepoint = invisible }) (run.Infos.AsSpan())
                { run with Infos = infos }
            | ValueNone -> GlyphRun.deleteGlyphsInplace (DefaultIgnorablePredicate()) run

    /// <summary>Runs the post-substitution hook, <c>hb_ot_substitute_post</c>, then the shaper's <c>postprocess_glyphs</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with variation selectors and default ignorables handled.</returns>
    let substitutePost (plan: OtShapePlan) (font: Font) (run: GlyphRun) : GlyphRun =
        let removed =
            match plan.ApplyMorx && not plan.ApplyGpos with
            | true -> AatLayout.removeDeletedGlyphs run
            | false -> run
        let vs = dealWithVariationSelectors removed
        ShaperHooks.postprocess plan font (hideDefaultIgnorables font vs)
