namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Khmer shaper's hooks, <c>_hb_ot_shaper_khmer</c> (src/hb-ot-shaper-khmer.cc:41-379): the features and the
/// pauses between them, the syllables, the reordering of the pre-base vowels and of coeng and ro, and the decomposition of
/// the split vowels.</summary>
[<RequireQualifiedAccess>]
module internal Khmer =
    /// <summary>The feature <c>pref</c>, the pre-base forms.</summary>
    [<Literal>]
    let private Pref = 0x70726566u<tag>

    /// <summary>The feature <c>blwf</c>, the below-base forms.</summary>
    [<Literal>]
    let private Blwf = 0x626C7766u<tag>

    /// <summary>The feature <c>abvf</c>, the above-base forms.</summary>
    [<Literal>]
    let private Abvf = 0x61627666u<tag>

    /// <summary>The feature <c>pstf</c>, the post-base forms.</summary>
    [<Literal>]
    let private Pstf = 0x70737466u<tag>

    /// <summary>The feature <c>cfar</c>, the conjunct forms after ro.</summary>
    [<Literal>]
    let private Cfar = 0x63666172u<tag>

    /// <summary>The feature <c>pres</c>, the pre-base substitutions.</summary>
    [<Literal>]
    let private Pres = 0x70726573u<tag>

    /// <summary>The feature <c>abvs</c>, the above-base substitutions.</summary>
    [<Literal>]
    let private Abvs = 0x61627673u<tag>

    /// <summary>The feature <c>blws</c>, the below-base substitutions.</summary>
    [<Literal>]
    let private Blws = 0x626C7773u<tag>

    /// <summary>The feature <c>psts</c>, the post-base substitutions.</summary>
    [<Literal>]
    let private Psts = 0x70737473u<tag>

    /// <summary>The syllable type of a broken cluster, <c>khmer_broken_cluster</c>: the low nibble of the syllable byte.</summary>
    [<Literal>]
    let private BrokenCluster = 1uy

    /// <summary>The most coengs that still open another, <c>num_coengs &lt;= 2</c>.</summary>
    [<Literal>]
    let private MaxCoengsBeforeLast = 2

    /// <summary>The flags of the basic features: joiners are not skipped and the lookups stay within a syllable,
    /// <c>F_MANUAL_JOINERS | F_PER_SYLLABLE</c>.</summary>
    let private basicFlags = FeatureFlags.ManualJoiners ||| FeatureFlags.PerSyllable

    /// <summary>Adds the features and pauses of the Khmer shaper, <c>collect_features_khmer</c>: the syllables are set up and
    /// reordered before any lookup, the basic features apply per syllable, the syllables are cleared, and the other features
    /// apply after.</summary>
    /// <param name="builder">The builder holding the leading features.</param>
    /// <returns>The builder with the shaper's features and pauses added.</returns>
    let collectFeatures builder =
        builder
        |> MapBuilder.addGsubPause KhmerSetupSyllables
        |> MapBuilder.addGsubPause KhmerReorder
        |> MapBuilder.enableFeature Tags.Locl FeatureFlags.PerSyllable 1u
        |> MapBuilder.enableFeature Tags.Ccmp FeatureFlags.PerSyllable 1u
        |> MapBuilder.addFeature Pref basicFlags 1u
        |> MapBuilder.addFeature Blwf basicFlags 1u
        |> MapBuilder.addFeature Abvf basicFlags 1u
        |> MapBuilder.addFeature Pstf basicFlags 1u
        |> MapBuilder.addFeature Cfar basicFlags 1u
        |> MapBuilder.addGsubPause ClearSyllables
        |> MapBuilder.addFeature Pres FeatureFlags.GlobalManualJoiners 1u
        |> MapBuilder.addFeature Abvs FeatureFlags.GlobalManualJoiners 1u
        |> MapBuilder.addFeature Blws FeatureFlags.GlobalManualJoiners 1u
        |> MapBuilder.addFeature Psts FeatureFlags.GlobalManualJoiners 1u

    /// <summary>Turns <c>clig</c> on and <c>liga</c> off, <c>override_features_khmer</c>: the Khmer specification has
    /// <c>clig</c> among the required features.</summary>
    /// <param name="builder">The builder holding every other feature.</param>
    /// <returns>The builder with the overrides applied.</returns>
    let overrideFeatures builder =
        builder
        |> MapBuilder.enableFeature Tags.Clig FeatureFlags.None 1u
        |> MapBuilder.disableFeature Tags.Liga

    /// <summary>Reads the masks of the basic features from the compiled map, <c>data_create_khmer</c>: the features applied
    /// globally have no mask of their own.</summary>
    /// <param name="mask1">The value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</param>
    /// <returns>The plan data.</returns>
    let createData mask1 =
        KhmerData
            { Pref = mask1 Pref
              Blwf = mask1 Blwf
              Abvf = mask1 Abvf
              Pstf = mask1 Pstf
              Cfar = mask1 Cfar }

    /// <summary>Saves the category of every glyph, <c>setup_masks_khmer</c>: the masks cannot be set here, because the
    /// syllables are not known yet, so a pause sets them later.</summary>
    /// <param name="run">The run, normalised.</param>
    /// <returns>The run with the categories set.</returns>
    let setupMasks (run: GlyphRun) =
        let props =
            Sealed.mapi (fun index (props: GlyphProps) ->
                { props with ShaperCategory = IndicTable.category run.Infos[index].Codepoint }) (run.Props.AsSpan())
        { run with Props = props }

    /// <summary>The category of one glyph, <see cref="setupMasks"/> for the glyph array of a working set.</summary>
    [<Struct>]
    type private CategoryMapper =
        val Infos: ImmutableArray<GlyphInfo>

        /// <summary>Creates the mapper.</summary>
        /// <param name="infos">The glyphs, whose code points select the categories.</param>
        new (infos) = { Infos = infos }

        interface IGroupMapper<GlyphProps> with
            member _.GroupEnd (_, start) = start + 1
            member this.Map (items, _, _, index) =
                { items[index] with ShaperCategory = IndicTable.category this.Infos[index].Codepoint }

    /// <summary>Saves the category of every glyph as <see cref="setupMasks"/> does, in the properties of a working set whose arrays the caller
    /// made in this call and shared with no one.</summary>
    /// <param name="set">The working set, normalised.</param>
    /// <returns>The set with the categories set.</returns>
    let setupMasksSet (set: WorkingSet) =
        Sealed.mapOwned (CategoryMapper(set.Infos)) set.Props
        set

    /// <summary>The rewriter that marks every syllable unsafe to break, <c>setup_syllables_khmer</c>'s loop.</summary>
    [<Struct>]
    type private UnsafeSyllables =
        val Level: ClusterLevel

        /// <summary>Creates the rewriter.</summary>
        /// <param name="level">The cluster level of the run.</param>
        new (level) = { Level = level }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                let initial : SyllableState =
                    { Rows = Sealed.ofSpan rows
                      Left = ImmutableArray.Empty
                      Right = ImmutableArray.Empty }
                let final = SyllableEdit.unsafeToBreak this.Level 0 rows.Length initial
                let rewrite : SegmentRewrite =
                    { Rows = final.Rows
                      Left = final.Left
                      Right = final.Right }
                rewrite

    /// <summary>Finds the syllables of the run, <c>setup_syllables_khmer</c>: the machine paints each glyph with the syllable
    /// it belongs to, and every syllable is marked unsafe to break. This pause changes no glyph.</summary>
    /// <param name="run">The run, with its categories set.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllables (run: GlyphRun) =
        let categories = Sealed.map (fun (props: GlyphProps) -> props.ShaperCategory) (run.Props.AsSpan())
        let previous = Sealed.map (fun (props: GlyphProps) -> props.Syllable) (run.Props.AsSpan())
        let struct (syllables, broken) = Ragel.findSyllables KhmerMachine.machine categories Unchecked.defaultof<ImmutableArray<int>> previous
        let painted = Sealed.mapi (fun index (props: GlyphProps) -> { props with Syllable = syllables[index] }) (run.Props.AsSpan())
        let struct (infos, props) = Sealed.rewriteSegments (UnsafeSyllables(run.ClusterLevel)) (run.Infos.AsSpan()) (painted.AsSpan())
        let scratch =
            match broken with
            | true -> run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable
            | false -> run.ScratchFlags
        { run with Infos = infos; Props = props; ScratchFlags = scratch }

    /// <summary>Finds the syllables of the run in place, <c>setup_syllables_khmer</c> on the working set: the machine paints the syllable of each
    /// glyph into the properties and every syllable is marked unsafe to break in the glyphs, where <see cref="setupSyllables"/>
    /// publishes copies. The result is the run of <see cref="setupSyllables"/>.</summary>
    /// <param name="run">The run, with its categories set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllablesOwned (run: WorkingSet) =
        let broken = SyllableOwned.paintFlatBreaking Ragel.khmer run.ClusterLevel run.Infos run.Props
        match broken with
        | true -> { run with ScratchFlags = run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable }
        | false -> run

    /// <summary>Gives the coeng and ro at an index the <c>pref</c> mask and every glyph after them the <c>cfar</c> mask, when
    /// the font has that feature.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="index">The index of the coeng.</param>
    /// <param name="rows">The rows of the syllable.</param>
    /// <returns>The rows with the masks set.</returns>
    let private maskCoengRo (data: KhmerPlan) index (rows: ImmutableArray<GlyphRow>) =
        Sealed.mapi (fun position (row: GlyphRow) ->
            match position >= index with
            | false -> row
            | true ->
                match position - index with
                | 0 | 1 -> { row with Info = { row.Info with Mask = row.Info.Mask ||| data.Pref } }
                | _ -> { row with Info = { row.Info with Mask = row.Info.Mask ||| data.Cfar } }) (rows.AsSpan())

    /// <summary>Moves the coeng and ro at an index to the start of the syllable and merges the clusters they pass over,
    /// <c>reorder_consonant_syllable</c>'s coeng and ro branch.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="index">The index of the coeng.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the coeng and ro in front.</returns>
    let private moveCoengRo level data index (state: SyllableState) =
        let merged = SyllableEdit.mergeClusters level 0 (index + 2) { state with Rows = maskCoengRo data index state.Rows }
        { merged with Rows = merged.Rows |> Sealed.move index 0 |> Sealed.move (index + 1) 1 }

    /// <summary>Moves the pre-base vowel at an index to the start of the syllable and merges the clusters it passes over,
    /// <c>reorder_consonant_syllable</c>'s left matra branch.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="index">The index of the vowel.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the vowel in front.</returns>
    let private movePreBase level index (state: SyllableState) =
        let merged = SyllableEdit.mergeClusters level 0 (index + 1) state
        { merged with Rows = Sealed.move index 0 merged.Rows }

    /// <summary>Walks a syllable after its base, <c>reorder_consonant_syllable</c>'s loop: a coeng followed by ro moves with
    /// it in front of everything, and a pre-base vowel moves in front of everything. Three coengs are allowed.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="coengs">The coengs seen so far.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The reordered syllable.</returns>
    [<TailCall>]
    let rec private reorderFrom level (data: KhmerPlan) index coengs (state: SyllableState) =
        match index < state.Rows.Length with
        | false -> state
        | true ->
            let category = state.Rows[index].Props.ShaperCategory
            match category = KhmerMachine.H && coengs <= MaxCoengsBeforeLast && index + 1 < state.Rows.Length with
            | true ->
                match state.Rows[index + 1].Props.ShaperCategory = KhmerMachine.Ra with
                | true -> reorderFrom level data (index + 1) MaxCoengsBeforeLast (moveCoengRo level data index state)
                | false -> reorderFrom level data (index + 1) (coengs + 1) state
            | false ->
                match category = KhmerMachine.VPre with
                | true -> reorderFrom level data (index + 1) coengs (movePreBase level index state)
                | false -> reorderFrom level data (index + 1) coengs state

    /// <summary>Gives every glyph after the first the masks of the post-base features, then reorders the syllable,
    /// <c>reorder_consonant_syllable</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The reordered syllable.</returns>
    let private reorderConsonantSyllable level (data: KhmerPlan) (state: SyllableState) =
        let postBase = data.Blwf ||| data.Abvf ||| data.Pstf
        let rows =
            Sealed.mapi (fun index (row: GlyphRow) ->
                match index with
                | 0 -> row
                | _ -> { row with Info = { row.Info with Mask = row.Info.Mask ||| postBase } }) (state.Rows.AsSpan())
        reorderFrom level data 1 0 { state with Rows = rows }

    /// <summary>The rewriter that reorders every syllable, <c>reorder_syllable_khmer</c>.</summary>
    [<Struct>]
    type private ReorderSyllables =
        val Level: ClusterLevel
        val Data: KhmerPlan

        /// <summary>Creates the rewriter.</summary>
        /// <param name="level">The cluster level of the run.</param>
        /// <param name="data">The plan data.</param>
        new (level, data) = { Level = level; Data = data }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                match KhmerMachine.ofSyllable rows[0].Props.Syllable with
                | KhmerMachine.KhmerNonKhmerCluster -> SegmentRewrite.unchanged
                | KhmerMachine.KhmerBrokenCluster | KhmerMachine.KhmerConsonantSyllable ->
                    let initial : SyllableState =
                        { Rows = Sealed.ofSpan rows
                          Left = ImmutableArray.Empty
                          Right = ImmutableArray.Empty }
                    let final = reorderConsonantSyllable this.Level this.Data initial
                    let rewrite : SegmentRewrite =
                        { Rows = final.Rows
                          Left = final.Left
                          Right = final.Right }
                    rewrite

    /// <summary>Inserts a dotted circle before each broken syllable, <c>hb_syllabic_insert_dotted_circles</c> as
    /// <c>reorder_khmer</c> calls it.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCircles (font: Font) (run: GlyphRun) =
        let struct (inserted, _) = SyllableEdit.insertDottedCircles font BrokenCluster KhmerMachine.DOTTEDCIRCLE ValueNone ValueNone run
        inserted

    /// <summary>Inserts a dotted circle before each broken syllable in arrays of the right size, <see cref="insertDottedCircles"/> for a
    /// run whose arrays were made in this call and are shared with no one.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCirclesOwned (font: Font) (run: WorkingSet) =
        let struct (inserted, _) = SyllableEdit.insertDottedCirclesOwned font BrokenCluster KhmerMachine.DOTTEDCIRCLE ValueNone ValueNone run
        inserted

    /// <summary>Reorders the syllables of the run, <c>reorder_khmer</c>: broken clusters get a dotted circle, then each
    /// syllable has its coeng and ro and its pre-base vowels moved to the front.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The reordered run.</returns>
    let reorder (data: KhmerPlan) (font: Font) (run: GlyphRun) =
        let circled = insertDottedCircles font run
        let struct (infos, props) = Sealed.rewriteSegments (ReorderSyllables(circled.ClusterLevel, data)) (circled.Infos.AsSpan()) (circled.Props.AsSpan())
        { circled with Infos = infos; Props = props }

    /// <summary>Decomposes the split vowels that have no Unicode decomposition, <c>decompose_khmer</c>: each becomes the
    /// vowel sign e and itself.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let decompose u =
        match u with
        | 0x17BEu | 0x17BFu | 0x17C0u | 0x17C4u | 0x17C5u -> ValueSome (struct (0x17C1u, u))
        | _ -> Unicode.decompose u

    /// <summary>Decomposes a code point as <see cref="decompose"/> does, the others through replaced Unicode functions.</summary>
    /// <param name="unicode">The functions.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let decomposeWith (unicode: UnicodeFuncs) u =
        match u with
        | 0x17BEu | 0x17BFu | 0x17C0u | 0x17C4u | 0x17C5u -> ValueSome (struct (0x17C1u, u))
        | _ -> unicode.Decompose u |> Settled.value

    /// <summary>Composes two code points, <c>compose_khmer</c>: a mark is never recomposed, so that the split vowels stay
    /// apart.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let compose a b =
        match UnicodeProps.isMark (Unicode.generalCategory a) with
        | true -> ValueNone
        | false -> Unicode.compose a b

    /// <summary>Composes two code points as <see cref="compose"/> does, through replaced Unicode functions.</summary>
    /// <param name="unicode">The functions.</param>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let composeWith (unicode: UnicodeFuncs) a b =
        match UnicodeProps.isMark (unicode.GeneralCategory a) with
        | true -> ValueNone
        | false -> unicode.Compose a b |> Settled.value

    /// <summary>Gives the glyphs of a syllable from an index the masks of the pre-base features in place, the coeng and ro the mask of
    /// <c>pref</c> and what follows the <c>cfar</c> mask, <see cref="maskCoengRo"/>.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="infos">The glyphs, made in this call and shared with no one.</param>
    /// <param name="start">The first glyph of the syllable.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <param name="index">The index of the coeng in the syllable.</param>
    /// <param name="position">The next glyph in the syllable.</param>
    [<TailCall>]
    let rec private maskCoengRoOwned (data: KhmerPlan) (infos: ImmutableArray<GlyphInfo>) (start: int) (finish: int) (index: int) (position: int) =
        match start + position < finish with
        | false -> ()
        | true ->
            SyllableOwned.orMask infos (start + position) (match position - index with | 0 | 1 -> data.Pref | _ -> data.Cfar)
            maskCoengRoOwned data infos start finish index (position + 1)

    /// <summary>Walks a syllable after its base in place, <c>reorder_consonant_syllable</c>'s loop.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="infos">The glyphs, made in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="start">The first glyph of the syllable.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <param name="index">The next glyph in the syllable.</param>
    /// <param name="coengs">The coengs seen so far.</param>
    [<TailCall>]
    let rec private reorderFromOwned level (data: KhmerPlan) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) (index: int) (coengs: int) =
        match start + index < finish with
        | false -> ()
        | true ->
            let category = props[start + index].ShaperCategory
            match category = KhmerMachine.H && coengs <= MaxCoengsBeforeLast && start + index + 1 < finish with
            | true ->
                match props[start + index + 1].ShaperCategory = KhmerMachine.Ra with
                | true ->
                    maskCoengRoOwned data infos start finish index index
                    SyllableOwned.mergeClusters level infos start (start + index + 2)
                    SyllableOwned.move infos props (start + index) start
                    SyllableOwned.move infos props (start + index + 1) (start + 1)
                    reorderFromOwned level data infos props start finish (index + 1) MaxCoengsBeforeLast
                | false -> reorderFromOwned level data infos props start finish (index + 1) (coengs + 1)
            | false ->
                match category = KhmerMachine.VPre with
                | true ->
                    SyllableOwned.mergeClusters level infos start (start + index + 1)
                    SyllableOwned.move infos props (start + index) start
                    reorderFromOwned level data infos props start finish (index + 1) coengs
                | false -> reorderFromOwned level data infos props start finish (index + 1) coengs

    /// <summary>Reorders the syllable chunks of a run from an index in place.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <param name="start">The first glyph of the next chunk.</param>
    [<TailCall>]
    let rec private reorderChunksOwned (data: KhmerPlan) (run: WorkingSet) (start: int) =
        match start < run.Props.Length with
        | false -> ()
        | true ->
            let finish = Sealed.nextSyllableEnd (run.Props.AsSpan()) start
            match KhmerMachine.ofSyllable run.Props[start].Syllable with
            | KhmerMachine.KhmerNonKhmerCluster -> ()
            | KhmerMachine.KhmerBrokenCluster | KhmerMachine.KhmerConsonantSyllable ->
                let postBase = data.Blwf ||| data.Abvf ||| data.Pstf
                SyllableOwned.maskAfterFirst run.Infos start finish postBase
                reorderFromOwned run.ClusterLevel data run.Infos run.Props start finish 1 0
            reorderChunksOwned data run finish

    /// <summary>Reorders the syllables of the run in place, <c>reorder_khmer</c> on the working set: the result of <see cref="reorder"/>.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The reordered run.</returns>
    let reorderOwned (data: KhmerPlan) (font: Font) (run: WorkingSet) =
        let circled = insertDottedCirclesOwned font run
        reorderChunksOwned data circled 0
        circled
