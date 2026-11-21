namespace Farfbuzz

open System.Collections.Immutable

/// <summary>Where each complex shaper's hook runs: one total function per hook of <c>hb_ot_shaper_t</c>
/// (src/hb-ot-shaper.hh:67-169), each a <c>match</c> on the shaper kind that names every case. A shaper without a hook shares the
/// arm of the others that have none.</summary>
[<RequireQualifiedAccess>]
module internal ShaperHooks =
    /// <summary>Runs the shaper's <c>collect_features</c> hook, if it has one, which makes the map non-simple, so that its
    /// features keep their order of addition (src/hb-ot-shape.cc:362-366).</summary>
    /// <param name="props">The segment properties of the plan.</param>
    /// <param name="hasFeature">Whether the font has a feature for the chosen script and language, <c>map-&gt;has_feature</c>.</param>
    /// <param name="kind">The categorized shaper kind.</param>
    /// <param name="builder">The builder holding the leading features.</param>
    /// <returns>The builder with the shaper's features added.</returns>
    let private collectHook (props: SegmentProperties) (hasFeature: Tag -> bool) (kind: ShaperKind) (builder: MapBuilder) =
        match kind with
        | HangulShaper -> Hangul.collectFeatures { builder with IsSimple = false }
        | KhmerShaper -> Khmer.collectFeatures { builder with IsSimple = false }
        | IndicShaper -> Indic.collectFeatures { builder with IsSimple = false }
        | UseShaper -> Use.collectFeatures { builder with IsSimple = false }
        | MyanmarShaper -> Myanmar.collectFeatures { builder with IsSimple = false }
        | ArabicShaper -> Arabic.collectFeatures props.Script (hasFeature Tags.Rclt) { builder with IsSimple = false }
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | MyanmarZawgyiShaper -> builder

    /// <summary>Runs the shaper's <c>override_features</c> hook, after the user features (src/hb-ot-shape.cc:401-402).</summary>
    /// <param name="kind">The categorized shaper kind.</param>
    /// <param name="builder">The builder holding every other feature.</param>
    /// <returns>The builder with the shaper's overrides applied.</returns>
    let private overrideHook (kind: ShaperKind) (builder: MapBuilder) =
        match kind with
        | HangulShaper -> Hangul.overrideFeatures builder
        | KhmerShaper -> Khmer.overrideFeatures builder
        | IndicShaper -> Indic.overrideFeatures builder
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | UseShaper -> builder
        | ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper -> builder

    /// <summary>Collects the features of a plan, <c>hb_ot_shape_collect_features</c> (src/hb-ot-shape.cc:320-403): the
    /// leading features, the shaper's <c>collect_features</c> hook, the trailing features, then the shaper's
    /// <c>override_features</c> hook.</summary>
    /// <param name="hasFeature">Whether the font has a feature for the chosen script and language, <c>map-&gt;has_feature</c>.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="kind">The categorized shaper kind.</param>
    /// <param name="userFeatures">The user features.</param>
    /// <returns>The builder with every feature collected.</returns>
    let collectFeaturesFor (hasFeature: Tag -> bool) (props: SegmentProperties) (kind: ShaperKind) (userFeatures: ImmutableArray<Feature>) =
        MapBuilder.collectHead props MapBuilder.empty
        |> collectHook props hasFeature kind
        |> MapBuilder.collectTail props userFeatures
        |> overrideHook kind

    /// <summary>Collects the features of a plan for a font that has no feature for the script, <see cref="collectFeaturesFor"/>.</summary>
    /// <param name="props">The segment properties.</param>
    /// <param name="kind">The categorized shaper kind.</param>
    /// <param name="userFeatures">The user features.</param>
    /// <returns>The builder with every feature collected.</returns>
    let collectFeatures (props: SegmentProperties) (kind: ShaperKind) (userFeatures: ImmutableArray<Feature>) =
        collectFeaturesFor (fun _ -> false) props kind userFeatures

    /// <summary>Prepares the text of a run for its shaper, the <c>preprocess_text</c> hook (src/hb-ot-shape.cc:1185-1190).</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its Unicode properties set, in its native direction.</param>
    /// <returns>The prepared run.</returns>
    let preprocess (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match plan.Shaper.Kind with
        | ThaiShaper -> Thai.preprocess plan font run
        | HangulShaper -> Hangul.preprocess font run
        | IndicShaper -> Indic.preprocess run
        | UseShaper -> Use.preprocess run
        | DefaultShaper | DumberShaper | HebrewShaper | KhmerShaper -> run
        | ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper -> run

    /// <summary>Prepares the text of a run for its shaper as <see cref="preprocess"/> does, the working-set route's form: the shapers
    /// whose hook inserts dotted circles into prohibited vowel sequences scan for one first and leave a run without one alone.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its Unicode properties set, in its native direction.</param>
    /// <returns>The prepared run.</returns>
    let preprocessScanned (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match plan.Shaper.Kind with
        | IndicShaper | UseShaper -> VowelConstraints.preprocessScanned run
        | HangulShaper -> Hangul.preprocessOwned font run
        | ThaiShaper | DefaultShaper | DumberShaper | HebrewShaper | KhmerShaper | ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper ->
            preprocess plan font run

    /// <summary>Prepares the text of a working set for its shaper as <see cref="preprocessScanned"/> does a run: the set whose text the hook
    /// leaves alone is returned as it is.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, with its Unicode properties set, in its native direction.</param>
    /// <returns>The prepared set.</returns>
    let preprocessSet (plan: OtShapePlan) (font: Font) (set: WorkingSet) =
        match plan.Shaper.Kind with
        | IndicShaper | UseShaper -> VowelConstraints.preprocessScannedSet set
        | HangulShaper -> Hangul.preprocessSet font set
        | ThaiShaper -> Thai.preprocessSet plan font set
        | DefaultShaper | DumberShaper | HebrewShaper | KhmerShaper | ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper -> set

    /// <summary>Post-processes the glyphs of a run, the <c>postprocess_glyphs</c> hook (src/hb-ot-shape.cc:961-965).</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font, read by the shapers whose hook needs it.</param>
    /// <param name="run">The run, substituted and with its default ignorables hidden.</param>
    /// <returns>The post-processed run.</returns>
    let postprocess (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match plan.Shaper.Kind with
        | ArabicShaper -> Arabic.postprocess font run
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | HangulShaper | KhmerShaper | IndicShaper | UseShaper
        | MyanmarShaper | MyanmarZawgyiShaper -> run

    /// <summary>Decomposes a code point for the normalizer, the <c>decompose</c> hook, else Unicode's decomposition
    /// (src/hb-ot-shape-normalize.cc:303-305), the decomposition of the plan's Unicode functions when it has them.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="_font">The font, read by the shapers whose hook needs it.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let decompose (plan: OtShapePlan) (_font: Font) u =
        match plan.Unicode, plan.Shaper.Kind with
        | ValueNone, KhmerShaper -> Khmer.decompose u
        | ValueNone, IndicShaper -> Indic.decompose u
        | ValueNone, (DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | HangulShaper | UseShaper) -> Unicode.decompose u
        | ValueNone, (ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper) -> Unicode.decompose u
        | ValueSome unicode, KhmerShaper -> Khmer.decomposeWith unicode u
        | ValueSome unicode, IndicShaper -> Indic.decomposeWith unicode u
        | ValueSome unicode, (DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | HangulShaper | UseShaper) -> unicode.Decompose u |> Settled.value
        | ValueSome unicode, (ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper) -> unicode.Decompose u |> Settled.value

    /// <summary>Composes two code points for the normalizer, the <c>compose</c> hook, else Unicode's composition
    /// (src/hb-ot-shape-normalize.cc:303-306), the composition of the plan's Unicode functions when it has them.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let compose (plan: OtShapePlan) a b =
        match plan.Unicode, plan.Shaper.Kind with
        | ValueNone, HebrewShaper -> Hebrew.compose plan a b
        | ValueNone, KhmerShaper -> Khmer.compose a b
        | ValueNone, IndicShaper -> Indic.compose a b
        | ValueNone, UseShaper -> Use.compose a b
        | ValueNone, (DefaultShaper | DumberShaper | ThaiShaper | HangulShaper) -> Unicode.compose a b
        | ValueNone, (ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper) -> Unicode.compose a b
        | ValueSome unicode, HebrewShaper -> Hebrew.composeWith unicode plan a b
        | ValueSome unicode, KhmerShaper -> Khmer.composeWith unicode a b
        | ValueSome unicode, IndicShaper -> Indic.composeWith unicode a b
        | ValueSome unicode, UseShaper -> Use.composeWith unicode a b
        | ValueSome unicode, (DefaultShaper | DumberShaper | ThaiShaper | HangulShaper) -> unicode.Compose a b |> Settled.value
        | ValueSome unicode, (ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper) -> unicode.Compose a b |> Settled.value

    /// <summary>Reorders the marks of a mark run the normalizer has sorted by combining class, the <c>reorder_marks</c>
    /// hook (src/hb-ot-shape-normalize.cc:389-397).</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run, the mark run sorted.</param>
    /// <param name="start">The first mark of the run.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The run with the marks reordered.</returns>
    let reorderMarks (plan: OtShapePlan) (run: GlyphRun) start finish =
        match plan.Shaper.Kind with
        | HebrewShaper -> Hebrew.reorderMarks run start finish
        | ArabicShaper -> Arabic.reorderMarks run start finish
        | DefaultShaper | DumberShaper | ThaiShaper | HangulShaper | KhmerShaper | IndicShaper | UseShaper
        | MyanmarShaper | MyanmarZawgyiShaper -> run

    /// <summary>Whether the lean normaliser may run a plan's shaper: every hook the normalizer calls (decompose, compose, reorder
    /// marks) is one it reads through <see cref="decompose"/>, <see cref="compose"/> and <see cref="reorderEdits"/>. Only the
    /// shapers that answer true take the lean pass; the others take the out-buffer passes.</summary>
    /// <param name="plan">The plan.</param>
    /// <returns>True when the lean normaliser reads this shaper's hooks.</returns>
    let leanNormalisation (plan: OtShapePlan) =
        match plan.Shaper.Kind with
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | ArabicShaper | IndicShaper | KhmerShaper | MyanmarShaper | UseShaper -> true
        | HangulShaper | MyanmarZawgyiShaper -> false

    /// <summary>The edits the <c>reorder_marks</c> hook makes to a sorted mark run, for the lean normaliser, which applies them to its
    /// own working copy (src/hb-ot-shape-normalize.cc:389-397).</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="props">The glyph properties, the mark run sorted.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first mark of the run.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The edits, none when the hook leaves the run alone.</returns>
    let reorderEdits (plan: OtShapePlan) (props: System.ReadOnlySpan<GlyphProps>) (infos: System.ReadOnlySpan<GlyphInfo>) start finish =
        match plan.Shaper.Kind with
        | HebrewShaper -> Hebrew.reorderEdits props start finish
        | ArabicShaper -> Arabic.reorderEdits props infos start finish
        | DefaultShaper | DumberShaper | ThaiShaper -> ImmutableArray<RunEdit>.Empty
        | HangulShaper | IndicShaper | KhmerShaper | MyanmarShaper | MyanmarZawgyiShaper | UseShaper -> ImmutableArray<RunEdit>.Empty


    /// <summary>Makes the plan data of a shaper from the compiled map, the <c>data_create</c> hook
    /// (src/hb-ot-shape.cc:241-249).</summary>
    /// <param name="kind">The shaper kind of the plan.</param>
    /// <param name="mask1">The value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</param>
    /// <param name="props">The segment properties of the plan.</param>
    /// <param name="map">The compiled map.</param>
    /// <returns>The plan data, none for a shaper without a <c>data_create</c> hook.</returns>
    let createData (kind: ShaperKind) (mask1: Tag -> uint32) (props: SegmentProperties) (map: OtMap) =
        match kind with
        | HangulShaper -> Hangul.createData mask1
        | KhmerShaper -> Khmer.createData mask1
        | IndicShaper -> Indic.createData mask1 props map
        | UseShaper -> Use.createData mask1 props map
        | ArabicShaper -> Arabic.createData mask1 props map
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | MyanmarShaper | MyanmarZawgyiShaper -> NoShaperData

    /// <summary>Whether a shaper has a <c>setup_masks</c> hook, which sets the masks between the fraction masks and the
    /// user-feature masks (src/hb-ot-shape.cc:753-770).</summary>
    /// <param name="kind">The shaper kind of the plan.</param>
    /// <returns>True when the shaper sets masks of its own.</returns>
    let hasSetupMasks (kind: ShaperKind) =
        match kind with
        | HangulShaper | KhmerShaper | IndicShaper | UseShaper | MyanmarShaper | ArabicShaper -> true
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | MyanmarZawgyiShaper -> false

    /// <summary>Runs the shaper's <c>setup_masks</c> hook (src/hb-ot-shape.cc:757-758).</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run, normalised, with the fraction masks set.</param>
    /// <returns>The run with the shaper's masks set.</returns>
    let setupMasks (plan: OtShapePlan) (run: GlyphRun) =
        let category =
            match plan.Unicode with
            | ValueSome unicode -> unicode.GeneralCategory
            | ValueNone -> Unicode.generalCategory
        match plan.Shaper.Kind with
        | HangulShaper ->
            match plan.Data with
            | HangulData data -> Hangul.setupMasks data run
            | NoShaperData | KhmerData _ | IndicData _ | UseData _ | ArabicData _ -> run
        | KhmerShaper -> Khmer.setupMasks run
        | IndicShaper -> Indic.setupMasks run
        | UseShaper ->
            match plan.Data with
            | UseData data -> Use.setupMasksUsing category plan.Props.Script data run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> run
        | MyanmarShaper -> Myanmar.setupMasks run
        | ArabicShaper ->
            match plan.Data with
            | ArabicData data -> Arabic.setupMasksUsing category data plan.Props.Script run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | UseData _ -> run
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | MyanmarZawgyiShaper -> run

    /// <summary>Runs the shaper's <c>setup_masks</c> hook as <see cref="setupMasks"/> does, writing into the glyph array of a run whose arrays
    /// the caller made in this call and shared with no one, for the shapers that can.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run, normalised, with the fraction masks set, with owned arrays.</param>
    /// <returns>The run with the shaper's masks set.</returns>
    let setupMasksOwned (plan: OtShapePlan) (run: GlyphRun) =
        match plan.Shaper.Kind, plan.Data with
        | HangulShaper, HangulData data -> Hangul.setupMasksOwned data run
        | HangulShaper, (NoShaperData | KhmerData _ | IndicData _ | UseData _ | ArabicData _) -> run
        | (DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | KhmerShaper | IndicShaper | UseShaper | ArabicShaper | MyanmarShaper | MyanmarZawgyiShaper), _ ->
            setupMasks plan run

    /// <summary>Runs the shaper's <c>setup_masks</c> hook as <see cref="setupMasksOwned"/> does, on a working set whose arrays the caller made in
    /// this call and shared with no one.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="set">The working set, normalised, with the fraction masks set, with owned arrays.</param>
    /// <returns>The set with the shaper's masks set.</returns>
    let setupMasksSet (plan: OtShapePlan) (set: WorkingSet) =
        match plan.Shaper.Kind with
        | HangulShaper ->
            match plan.Data with
            | HangulData data -> Hangul.setupMasksSet data set
            | NoShaperData | KhmerData _ | IndicData _ | UseData _ | ArabicData _ -> set
        | KhmerShaper -> Khmer.setupMasksSet set
        | IndicShaper -> Indic.setupMasksSet set
        | MyanmarShaper -> Myanmar.setupMasksSet set
        | UseShaper ->
            match plan.Data with
            | UseData data -> Use.setupMasksSet plan.Props.Script data set
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> set
        | ArabicShaper ->
            match plan.Data with
            | ArabicData data -> Arabic.setupMasksSet data plan.Props.Script set
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | UseData _ -> set
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | MyanmarZawgyiShaper -> set

    /// <summary>Runs the function of a pause between two stages of the GSUB table, a <c>pause_func_t</c>
    /// (src/hb-ot-layout.cc:2122-2129). Upstream refreshes the digest of its apply context when the function changed the
    /// run; the lookup kernel builds it again on entry, so the answer is the run alone.</summary>
    /// <param name="pause">The pause of the stage.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, after the lookups of the stage.</param>
    /// <returns>The run after the pause.</returns>
    let applyPause (pause: PauseKind) (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match pause with
        | NoPause -> run
        | ClearSyllables -> run
        | KhmerSetupSyllables -> Khmer.setupSyllables run
        | KhmerReorder ->
            match plan.Data with
            | KhmerData data -> Khmer.reorder data font run
            | NoShaperData | HangulData _ | IndicData _ | UseData _ | ArabicData _ -> run
        | IndicSetupSyllables -> Indic.setupSyllables run
        | IndicInitialReordering ->
            match plan.Data with
            | IndicData data -> IndicInitial.reorder data font run
            | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> run
        | IndicFinalReordering ->
            match plan.Data with
            | IndicData data -> IndicFinal.reorder data font run
            | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> run
        | ClearSubstitutionFlags -> Use.clearSubstitutionFlags run
        | UseSetupSyllables ->
            match plan.Data with
            | UseData data -> Use.setupSyllables data run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> run
        | UseRecordRphf ->
            match plan.Data with
            | UseData data -> Use.recordRphf data run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> run
        | UseRecordPref -> Use.recordPref run
        | UseReorder -> Use.reorder font run
        | MyanmarSetupSyllables -> Myanmar.setupSyllables run
        | MyanmarReorder -> Myanmar.reorder font run
        | ArabicRecordStch ->
            match plan.Data with
            | ArabicData data -> Arabic.recordStch data run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | UseData _ -> run
        | ArabicFallbackShape ->
            match plan.Data with
            | ArabicData data -> ArabicFallback.shape data plan font run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | UseData _ -> run
        | ArabicReleaseAction -> run

    /// <summary>Runs the function of a pause that has an in-place form on a working set whose run has not failed, writing into its arrays; the
    /// other pauses take <see cref="applyPause"/>.</summary>
    /// <param name="pause">The pause of the stage.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The set, after the lookups of the stage, with owned arrays.</param>
    /// <returns>The set after the pause.</returns>
    let private applyPauseWritten (pause: PauseKind) (plan: OtShapePlan) (font: Font) (run: WorkingSet) =
        match pause with
        | KhmerSetupSyllables -> Khmer.setupSyllablesOwned run
        | IndicSetupSyllables -> Indic.setupSyllablesOwned run
        | MyanmarSetupSyllables -> Myanmar.setupSyllablesOwned run
        | ClearSubstitutionFlags -> Use.clearSubstitutionFlagsOwned run
        | UseSetupSyllables ->
            match plan.Data with
            | UseData data -> Use.setupSyllablesOwned data run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> run
        | IndicInitialReordering ->
            match plan.Data with
            | IndicData data -> IndicInitialOwned.reorder data font run
            | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> run
        | IndicFinalReordering ->
            match plan.Data with
            | IndicData data -> IndicFinalOwned.reorder data font run
            | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> run
        | MyanmarReorder -> Myanmar.reorderOwned font run
        | KhmerReorder ->
            match plan.Data with
            | KhmerData data -> Khmer.reorderOwned data font run
            | NoShaperData | HangulData _ | IndicData _ | UseData _ | ArabicData _ -> run
        | UseRecordRphf ->
            match plan.Data with
            | UseData data -> Use.recordRphfOwned data run
            | NoShaperData | HangulData _ | KhmerData _ | IndicData _ | ArabicData _ -> run
        | UseRecordPref -> Use.recordPrefOwned run
        | UseReorder -> Use.reorderOwned font run
        | NoPause | ClearSyllables | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction -> GlyphRun.setOf (applyPause pause plan font (GlyphRun.runOf run))

    /// <summary>Runs the function of a pause on a working set whose arrays were made in this call and are shared with no one, writing the
    /// pauses that have an in-place form into them: the result of <see cref="applyPause"/> in every field. A run that has failed, and the
    /// other pauses, take <see cref="applyPause"/>, so that the arrays a failed buffer keeps are the reference's.</summary>
    /// <param name="pause">The pause of the stage.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The set, after the lookups of the stage, with owned arrays.</param>
    /// <returns>The set after the pause.</returns>
    let applyPauseSet (pause: PauseKind) (plan: OtShapePlan) (font: Font) (run: WorkingSet) =
        match run.Successful with
        | false -> GlyphRun.setOf (applyPause pause plan font (GlyphRun.runOf run))
        | true -> applyPauseWritten pause plan font run
