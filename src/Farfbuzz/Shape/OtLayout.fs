namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.OpenType
open Farfbuzz.Aat

/// <summary>The empty plan capabilities of a face that provides no layout machinery.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal PlanCapabilities =
    /// <summary>The all-false capabilities, the phase-4 body of <c>OtLayout.capabilities</c>.</summary>
    let none : PlanCapabilities =
        { HasGlyphClasses = false
          HasGsub = false
          HasGpos = false
          HasKern = false
          HasMachineKern = false
          HasCrossKern = false
          HasMorx = false
          HasKerx = false
          HasTrak = false
          HasStat = false }

/// <summary>The compiled OpenType feature map and its accessors, <c>hb_ot_map_t</c>.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal OtMap =
    /// <summary>The index meaning "no feature", <c>HB_OT_LAYOUT_NO_FEATURE_INDEX</c>.</summary>
    [<Literal>]
    let NoFeatureIndex = 0xFFFFu

    /// <summary>The single global feature bit, <c>global_bit_mask</c> of <c>hb_ot_map_builder_t::compile</c>.</summary>
    [<Literal>]
    let private GlobalBitMask = 0x80000000u

    /// <summary>The compiled map with no features, one empty stage per table and no chosen scripts.</summary>
    let empty : OtMap =
        { GlobalMask = GlobalBitMask
          Features = ImmutableArray.Empty
          GsubLookups = ImmutableArray.Empty
          GposLookups = ImmutableArray.Empty
          GsubStages = Sealed.init 1 (fun _ -> { LastLookup = 0; Pause = NoPause })
          GposStages = Sealed.init 1 (fun _ -> { LastLookup = 0; Pause = NoPause })
          HasPauses = false
          GsubViews = Memo<obj>()
          ChosenGsubScript = Tags.Empty
          ChosenGposScript = Tags.Empty
          FoundGsubScript = false
          FoundGposScript = false }

    /// <summary>Finds the compiled feature of a tag, <c>features.bsearch</c>.</summary>
    /// <param name="features">The compiled features.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="index">The next feature index.</param>
    /// <returns>The feature, or none.</returns>
    [<TailCall>]
    let rec private findFeature (features: ImmutableArray<FeatureMap>) (tag: Tag) (index: int) : FeatureMap voption =
        match index >= features.Length with
        | true -> ValueNone
        | false ->
            match features[index].Tag = tag with
            | true -> ValueSome features[index]
            | false -> findFeature features tag (index + 1)

    /// <summary>Returns the value-1 mask of a feature, <c>get_1_mask</c>.</summary>
    /// <param name="map">The map.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The mask, or 0 when the feature is absent.</returns>
    let mask1 (map: OtMap) (tag: Tag) : uint32 =
        match findFeature map.Features tag 0 with
        | ValueSome feature -> feature.Mask1
        | ValueNone -> 0u

    /// <summary>Returns the mask and shift of a feature, <c>get_mask</c>.</summary>
    /// <param name="map">The map.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The mask and shift, or (0, 0) when the feature is absent.</returns>
    let mask (map: OtMap) (tag: Tag) : struct (uint32 * int) =
        match findFeature map.Features tag 0 with
        | ValueSome feature -> struct (feature.Mask, feature.Shift)
        | ValueNone -> struct (0u, 0)

    /// <summary>Returns a feature's index in one table, <c>get_feature_index</c>.</summary>
    /// <param name="map">The map.</param>
    /// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The feature index, or <see cref="NoFeatureIndex"/> when absent.</returns>
    let featureIndex (map: OtMap) (tableIndex: int) (tag: Tag) : uint32 =
        match findFeature map.Features tag 0 with
        | ValueSome feature ->
            match tableIndex with
            | 0 -> feature.GsubIndex
            | _ -> feature.GposIndex
        | ValueNone -> NoFeatureIndex

/// <summary>The layout seam: the phase-4 bodies are identity passes over an empty map; phase 5 fills them.</summary>
[<RequireQualifiedAccess>]
module internal OtLayout =
    /// <summary>Returns the face's layout capabilities, <c>hb_ot_face_t</c>'s table presence.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The capabilities.</returns>
    let capabilities (font: Font) : PlanCapabilities =
        let face = font.Ot
        { HasGlyphClasses = Gdef.hasGlyphClasses face.Gdef.Value
          HasGsub = LayoutApi.hasSubstitution face
          HasGpos = LayoutApi.hasPositioning face
          HasKern = KernTable.hasData face.Kern.Value
          HasMachineKern = KernTable.hasStateMachine face.Kern.Value
          HasCrossKern = KernTable.hasCrossStream face.Kern.Value
          HasMorx = AatLayout.hasSubstitution face
          HasKerx = AatLayout.hasPositioning face
          HasTrak = Trak.hasData face.Trak.Value
          HasStat = Stat.hasData face.Stat.Value }

    /// <summary>Categorizes the shaper from the chosen GSUB script, then collects and compiles the map,
    /// <c>hb_ot_shape_planner_t</c>'s construction and compile. The features are collected for the shaper the planner
    /// holds once its constructor has replaced a shaper by the dumber one in a face that applies <c>morx</c>; the shaper
    /// returned is the categorized one, which the compile reads.</summary>
    /// <param name="font">The font.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <returns>The compiled map and the shaper kind.</returns>
    let compileMap (font: Font) (props: SegmentProperties) (features: ImmutableArray<Feature>) : struct (OtMap * ShaperKind) =
        let face = font.Ot
        let kind = Categorize.shaper props.Script props.Direction (MapBuilder.chosenGsubScript face props)
        let applyMorx = AatLayout.hasSubstitution face && (Direction.isHorizontal props.Direction || not (LayoutApi.hasSubstitution face))
        let collectKind =
            match applyMorx && kind <> DefaultShaper with
            | true -> DumberShaper
            | false -> kind
        let builder = ShaperHooks.collectFeaturesFor (MapBuilder.hasFeature face props) props collectKind features
        let map = MapBuilder.compile font.Variations.Coords font.Variations.Gdef face props builder
        struct (map, kind)

    /// <summary>Returns the GDEF glyph properties of a glyph, <c>GDEF::get_glyph_props</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The glyph properties, zero when GDEF is absent.</returns>
    let glyphProps (font: Font) (glyph: uint32) : uint16 =
        Gdef.glyphProps font.Ot.Gdef.Value glyph

    /// <summary>Applies the lookups of a window of the map, those of the stages between two pauses, as the lookup kernel
    /// applies them, <c>hb_ot_map_t::apply</c>'s loop over the lookups of a stage, with the glyph set of every lookup when it is
    /// given.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, or none to walk every lookup over every glyph.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="first">The position of the first lookup.</param>
    /// <param name="last">The position after the last lookup.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the lookups, the same instance when there are none or they changed nothing.</returns>
    let substituteWindowIn (covered: ImmutableArray<ImmutableArray<uint64>>) (plan: OtShapePlan) (font: Font) first last (run: GlyphRun) =
        match last > first with
        | false -> run
        | true ->
            let lookups = Sealed.init (last - first) (fun index -> plan.Map.GsubLookups[first + index])
            let stages = ImmutableArray.Create { LastLookup = last - first; Pause = NoPause }
            match covered.IsDefaultOrEmpty with
            | true -> Engine.applyGsub true font.Ot.Gsub.Value lookups stages font run
            | false -> Engine.applyGsubWindowCovered covered font.Ot.Gsub.Value lookups stages font run

    /// <summary>Applies the lookups of a window of the map, those of the stages between two pauses, as the lookup kernel
    /// applies them, <c>hb_ot_map_t::apply</c>'s loop over the lookups of a stage.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="first">The position of the first lookup.</param>
    /// <param name="last">The position after the last lookup.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the lookups, the same instance when there are none or they changed nothing.</returns>
    let substituteWindow (plan: OtShapePlan) (font: Font) first last (run: GlyphRun) =
        substituteWindowIn ImmutableArray.Empty plan font first last run

    /// <summary>Runs the GSUB stages one after another, the lookups of the stages between two pauses in one pass of the
    /// kernel, then the function of the pause, <c>hb_ot_map_t::apply</c>: a stage whose pause runs no function does not end
    /// the window.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, or none.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="owned">Whether the run's arrays were made in this call and are shared with no one, so a pause may write into them.</param>
    /// <param name="stage">The next stage.</param>
    /// <param name="first">The position of the first lookup of the window.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    [<TailCall>]
    let rec private substituteStages (covered: ImmutableArray<ImmutableArray<uint64>>) (plan: OtShapePlan) (font: Font) (stages: ImmutableArray<StageMap>) owned stage first (run: GlyphRun) =
        match stage < stages.Length with
        | false -> substituteWindowIn covered plan font first stages[stages.Length - 1].LastLookup run
        | true ->
            match PauseKind.runsFunction stages[stage].Pause with
            | false -> substituteStages covered plan font stages owned (stage + 1) first run
            | true ->
                let applied = substituteWindowIn covered plan font first stages[stage].LastLookup run
                let paused =
                    match owned with
                    | true -> GlyphRun.runOf (ShaperHooks.applyPauseSet stages[stage].Pause plan font (GlyphRun.setOf applied))
                    | false -> ShaperHooks.applyPause stages[stage].Pause plan font applied
                substituteStages covered plan font stages owned (stage + 1) stages[stage].LastLookup paused

    /// <summary>Runs the GSUB pass, <c>hb_ot_map_t::substitute</c>: a plan with no pause that runs a function takes the
    /// kernel over every lookup at once, as it always has.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substitute (plan: OtShapePlan) (font: Font) (run: GlyphRun) : GlyphRun =
        match plan.Map.HasPauses with
        | false -> Engine.substitute plan font run
        | true -> substituteStages ImmutableArray.Empty plan font plan.Map.GsubStages false 0 0 run

    /// <summary>Runs the GSUB pass of a plan that has a pause that runs a function, <c>hb_ot_map_t::substitute</c>, walking each
    /// lookup of each window only over the glyphs its subtables cover: the result of <see cref="substitute"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="Engine.gsubLookupCovered"/>.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substituteCovered (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (run: GlyphRun) =
        match plan.Map.HasPauses with
        | false -> Engine.substitute plan font run
        | true -> substituteStages covered plan font plan.Map.GsubStages true 0 0 run

    /// <summary>Applies the lookups of one window cut out of the plan's map, <see cref="substituteWindowIn"/> for the same lookups.</summary>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="font">The font.</param>
    /// <param name="window">The window.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the lookups, the same instance when there are none or they changed nothing.</returns>
    let substituteWindowOf (covered: ImmutableArray<ImmutableArray<uint64>>) (font: Font) (window: GsubWindow) (run: GlyphRun) =
        match window.WindowLookups.IsEmpty with
        | true -> run
        | false -> Engine.applyGsubWindow covered font.Ot.Gsub.Value window font run

    /// <summary>Applies the lookups of one window as <see cref="substituteWindowOf"/> does, to a run whose arrays this call made and has shared
    /// with no one, writing into them in place.</summary>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="font">The font.</param>
    /// <param name="window">The window.</param>
    /// <param name="run">The run, with arrays of its own.</param>
    /// <returns>The run after the lookups, the same instance when there are none or they changed nothing.</returns>
    let substituteWindowOwned (covered: ImmutableArray<ImmutableArray<uint64>>) (font: Font) (window: GsubWindow) (run: GlyphRun) =
        match window.WindowLookups.IsEmpty with
        | true -> run
        | false -> Engine.applyGsubWindowOwned covered font.Ot.Gsub.Value window font run

    /// <summary>Applies the lookups of one window as <see cref="substituteWindowOwned"/> does, to a working set whose arrays this call made and
    /// has shared with no one.</summary>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="font">The font.</param>
    /// <param name="window">The window.</param>
    /// <param name="set">The working set, with arrays of its own.</param>
    /// <returns>The set after the window.</returns>
    let substituteWindowOwnedSet (covered: ImmutableArray<ImmutableArray<uint64>>) (font: Font) (window: GsubWindow) (set: WorkingSet) =
        match window.WindowLookups.IsEmpty with
        | true -> set
        | false -> Engine.applyGsubWindowOwnedSet covered font.Ot.Gsub.Value window font set

    /// <summary>Runs the GSUB stages one after another over the windows the plan cut out of its map, the lookups of each window in one pass
    /// of the kernel, then the function of the pause that ends it, written in place on the working set.</summary>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="windows">The windows of the plan.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="stage">The next stage.</param>
    /// <param name="window">The window the next lookups belong to.</param>
    /// <param name="set">The working set, whose arrays this call made.</param>
    /// <returns>The substituted set.</returns>
    [<TailCall>]
    let rec private substituteWindowsSet (covered: ImmutableArray<ImmutableArray<uint64>>) (plan: OtShapePlan) (font: Font) (windows: ImmutableArray<GsubWindow>) (stages: ImmutableArray<StageMap>) stage window (set: WorkingSet) =
        match stage < stages.Length with
        | false -> substituteWindowOwnedSet covered font windows[window] set
        | true ->
            match PauseKind.runsFunction stages[stage].Pause with
            | false -> substituteWindowsSet covered plan font windows stages (stage + 1) window set
            | true ->
                let applied = substituteWindowOwnedSet covered font windows[window] set
                let paused = ShaperHooks.applyPauseSet stages[stage].Pause plan font applied
                substituteWindowsSet covered plan font windows stages (stage + 1) (window + 1) paused

    /// <summary>Runs the GSUB pass of a plan with a pause that runs a function over the windows the plan cut out of its map, on a working set:
    /// the result of <see cref="substituteCovered"/>, without cutting a window on each line.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="windows">The windows of the plan, <see cref="Engine.gsubWindows"/>.</param>
    /// <param name="set">The working set, whose arrays this call made.</param>
    /// <returns>The substituted set.</returns>
    let substituteWindowedSet (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (windows: ImmutableArray<GsubWindow>) (set: WorkingSet) =
        substituteWindowsSet covered plan font windows plan.Map.GsubStages 0 0 set

    /// <summary>Clears the attach chain and type before the GPOS pass, <c>hb_ot_layout_position_start</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the attachments cleared.</returns>
    let positionStart (run: GlyphRun) : GlyphRun =
        GposApply.positionStart run

    /// <summary>Runs the positioning pass, GPOS else if kerx, then the legacy kern machine,
    /// <c>hb_ot_shape_plan_t::position</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let position (plan: OtShapePlan) (font: Font) (run: GlyphRun) : GlyphRun =
        let positioned =
            match plan.ApplyGpos with
            | true -> Engine.position plan font run
            | false ->
                match plan.ApplyKerx with
                | true -> AatLayout.position plan font run
                | false -> run
        match plan.ApplyKern with
        | true -> Kern.applyKern plan font positioned
        | false -> positioned

    /// <summary>Finishes the advances, <c>hb_ot_layout_position_finish_advances</c>; upstream leaves them as they are.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The unchanged run.</returns>
    let finishAdvances (run: GlyphRun) : GlyphRun =
        run

    /// <summary>Finishes the offsets, <c>hb_ot_layout_position_finish_offsets</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with attachment offsets propagated and the slant correction applied.</returns>
    let finishOffsets (font: Font) (run: GlyphRun) : GlyphRun =
        GposApply.finishOffsets font run

    /// <summary>Recategorizes marks after substitution; <c>_hb_ot_shape_fallback_mark_position_recategorize_marks</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the marks recategorized.</returns>
    let recategorizeMarks (run: GlyphRun) : GlyphRun =
        FallbackMark.recategorizeMarks run

    /// <summary>Finds the end of a base's mark run, skipping hidden and default-ignorable glyphs,
    /// <c>position_cluster_impl</c>'s inner scan (src/hb-ot-shape-fallback.cc:438-447).</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next index after the base.</param>
    /// <returns>The first index that is neither a mark nor hidden nor default-ignorable.</returns>
    [<TailCall>]
    let rec private marksEnd (props: ReadOnlySpan<GlyphProps>) (index: int) : int =
        match index < props.Length
              && (UnicodeProps.isUnicodeMark props[index]
                  || UnicodeProps.isHidden props[index]
                  || UnicodeProps.isDefaultIgnorable props[index]) with
        | true -> marksEnd props (index + 1)
        | false -> index

    /// <summary>The state of the fallback-mark flag pass.</summary>
    [<Struct>]
    type private FallbackFlagState =
        | FallbackNext of index: int
        | FallbackDone

    /// <summary>Computes one step of the fallback-mark flag pass: flag the next base-plus-marks cluster.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next glyph index.</param>
    /// <returns>The edit and the next state.</returns>
    let private fallbackStep (props: ReadOnlySpan<GlyphProps>) (index: int) : struct (RunEdit * FallbackFlagState) =
        match index >= props.Length with
        | true -> struct (RunEdit.Finish, FallbackDone)
        | false ->
            match UnicodeProps.isUnicodeMark props[index] with
            | true -> struct (RunEdit.NextGlyphs 0, FallbackNext (index + 1))
            | false ->
                let finish = marksEnd props (index + 1)
                match finish > index + 1 with
                | true -> struct (RunEdit.UnsafeToBreak (index, finish), FallbackNext finish)
                | false -> struct (RunEdit.NextGlyphs 0, FallbackNext (index + 1))

    /// <summary>The pass that marks each base-plus-marks cluster unsafe to break,
    /// <c>position_around_base</c>'s first line (src/hb-ot-shape-fallback.cc:334).</summary>
    [<Struct>]
    type private FallbackFlagPass =
        interface IRunPass<FallbackFlagState> with
            member _.Step (cursor, state) =
                match state with
                | FallbackDone -> struct (RunEdit.Finish, FallbackDone)
                | FallbackNext index -> fallbackStep cursor.InputProps index

    /// <summary>Whether the fallback-mark pass changes nothing: no glyph that is not a Unicode mark, a default-ignorable or hidden glyph
    /// included, is followed by a mark, hidden or default-ignorable glyph. One walk over the properties, each checked once, as
    /// <c>position_cluster_impl</c> takes the first glyph that is not a Unicode mark for a base whatever else it is
    /// (src/hb-ot-shape-fallback.cc:404-423).</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The next glyph index.</param>
    /// <param name="previousBase">Whether the previous glyph is not a Unicode mark, so that it is a base.</param>
    /// <returns>True when the pass would never emit an edit.</returns>
    [<TailCall>]
    let rec private fallbackNoOp (props: ReadOnlySpan<GlyphProps>) (index: int) (previousBase: bool) : bool =
        match index >= props.Length with
        | true -> true
        | false ->
            let trailing =
                UnicodeProps.isUnicodeMark props[index]
                || UnicodeProps.isHidden props[index]
                || UnicodeProps.isDefaultIgnorable props[index]
            match trailing && previousBase with
            | true -> false
            | false -> fallbackNoOp props (index + 1) (not (UnicodeProps.isUnicodeMark props[index]))

    /// <summary>Positions the fallback marks without the whole-run early exit, <c>_hb_ot_shape_fallback_mark_position</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="adjustOffsets">Whether to adjust the offsets.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the marks positioned.</returns>
    let internal fallbackMarkPositionPass (plan: OtShapePlan) (font: Font) (adjustOffsets: bool) (run: GlyphRun) : Result<GlyphRun, ShapeError> =
        match plan.FallbackMarkPositioning with
        | false -> Ok run
        | true -> FallbackMark.position plan font adjustOffsets run

    /// <summary>Whether the first glyph of a run starts a cluster as a base, as <c>_hb_ot_shape_fallback_mark_position</c>
    /// starts its first cluster at glyph zero whatever the glyph is: only a Unicode mark is not a base
    /// (<c>position_cluster_impl</c>'s base scan), so a default-ignorable or hidden first glyph is one.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the run has a first glyph and it is a base.</returns>
    let private firstIsBase (props: ImmutableArray<GlyphProps>) =
        props.Length > 0 && not (UnicodeProps.isUnicodeMark props[0])

    /// <summary>Positions the fallback marks, <c>_hb_ot_shape_fallback_mark_position</c>. A run with no
    /// base-plus-marks cluster returns the input run unchanged.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="adjustOffsets">Whether to adjust the offsets.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the marks positioned.</returns>
    let fallbackMarkPosition (plan: OtShapePlan) (font: Font) (adjustOffsets: bool) (run: GlyphRun) : Result<GlyphRun, ShapeError> =
        match plan.FallbackMarkPositioning with
        | false -> Ok run
        | true ->
            match fallbackNoOp (run.Props.AsSpan()) 1 (firstIsBase run.Props) with
            | true -> Ok run
            | false -> fallbackMarkPositionPass plan font adjustOffsets run
