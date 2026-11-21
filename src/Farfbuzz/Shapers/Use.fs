namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The universal shaping engine's hooks, <c>_hb_ot_shaper_use</c> (src/hb-ot-shaper-use.cc:45-511): the features and the
/// pauses between them, the plan data, the categories and syllables, the masks of the repha and of the joining forms, the
/// recording of the substituted repha and pre-base consonant, the reordering of the syllables, and the composition that avoids
/// recomposing the split matras.</summary>
[<RequireQualifiedAccess>]
module internal Use =
    /// <summary>The feature <c>nukt</c>, the nukta forms.</summary>
    [<Literal>]
    let private Nukt = 0x6E756B74u<tag>

    /// <summary>The feature <c>akhn</c>, the akhand ligatures.</summary>
    [<Literal>]
    let private Akhn = 0x616B686Eu<tag>

    /// <summary>The feature <c>rphf</c>, the reph forms.</summary>
    [<Literal>]
    let private Rphf = 0x72706866u<tag>

    /// <summary>The feature <c>pref</c>, the pre-base forms.</summary>
    [<Literal>]
    let private Pref = 0x70726566u<tag>

    /// <summary>The feature <c>rkrf</c>, the rakaar forms.</summary>
    [<Literal>]
    let private Rkrf = 0x726B7266u<tag>

    /// <summary>The feature <c>abvf</c>, the above-base forms.</summary>
    [<Literal>]
    let private Abvf = 0x61627666u<tag>

    /// <summary>The feature <c>blwf</c>, the below-base forms.</summary>
    [<Literal>]
    let private Blwf = 0x626C7766u<tag>

    /// <summary>The feature <c>half</c>, the half forms.</summary>
    [<Literal>]
    let private Half = 0x68616C66u<tag>

    /// <summary>The feature <c>pstf</c>, the post-base forms.</summary>
    [<Literal>]
    let private Pstf = 0x70737466u<tag>

    /// <summary>The feature <c>vatu</c>, the vattu variants.</summary>
    [<Literal>]
    let private Vatu = 0x76617475u<tag>

    /// <summary>The feature <c>cjct</c>, the conjunct forms.</summary>
    [<Literal>]
    let private Cjct = 0x636A6374u<tag>

    /// <summary>The feature <c>isol</c>, the isolated forms.</summary>
    [<Literal>]
    let private Isol = 0x69736F6Cu<tag>

    /// <summary>The feature <c>init</c>, the initial forms.</summary>
    [<Literal>]
    let private Init = 0x696E6974u<tag>

    /// <summary>The feature <c>medi</c>, the medial forms.</summary>
    [<Literal>]
    let private Medi = 0x6D656469u<tag>

    /// <summary>The feature <c>fina</c>, the final forms.</summary>
    [<Literal>]
    let private Fina = 0x66696E61u<tag>

    /// <summary>The feature <c>abvs</c>, the above-base substitutions.</summary>
    [<Literal>]
    let private Abvs = 0x61627673u<tag>

    /// <summary>The feature <c>blws</c>, the below-base substitutions.</summary>
    [<Literal>]
    let private Blws = 0x626C7773u<tag>

    /// <summary>The feature <c>haln</c>, the halant forms.</summary>
    [<Literal>]
    let private Haln = 0x68616C6Eu<tag>

    /// <summary>The feature <c>pres</c>, the pre-base substitutions.</summary>
    [<Literal>]
    let private Pres = 0x70726573u<tag>

    /// <summary>The feature <c>psts</c>, the post-base substitutions.</summary>
    [<Literal>]
    let private Psts = 0x70737473u<tag>

    /// <summary>The syllable type of a broken cluster, <c>use_broken_cluster</c>: the low nibble of the syllable byte.</summary>
    [<Literal>]
    let private BrokenCluster = 7uy

    /// <summary>The most glyphs of a syllable that get the repha mask when the syllable does not begin with a repha,
    /// <c>hb_min (3u, end - start)</c>.</summary>
    [<Literal>]
    let private RephaGlyphs = 3

    /// <summary>The flags of the features applied once, before the reordering: joiners are not skipped and the lookups stay within
    /// a syllable, <c>F_MANUAL_ZWJ | F_PER_SYLLABLE</c>.</summary>
    let private perSyllableFlags = FeatureFlags.ManualZwj ||| FeatureFlags.PerSyllable

    /// <summary>The basic features, applied together before the reordering, <c>use_basic_features</c>.</summary>
    let private basicFeatures : Tag array = [| Rkrf; Abvf; Blwf; Half; Pstf; Vatu; Cjct |]

    /// <summary>The topographical features, in the order of the joining forms, <c>use_topographical_features</c>.</summary>
    let private topographicalFeatures : Tag array = [| Isol; Init; Medi; Fina |]

    /// <summary>The other features, applied together after the reordering, <c>use_other_features</c>.</summary>
    let private otherFeatures : Tag array = [| Abvs; Blws; Haln; Pres; Psts |]

    /// <summary>The joining form of a syllable that is isolated, <c>JOINING_FORM_ISOL</c>.</summary>
    [<Literal>]
    let private FormIsol = 0

    /// <summary>The joining form of a syllable that begins a word, <c>JOINING_FORM_INIT</c>.</summary>
    [<Literal>]
    let private FormInit = 1

    /// <summary>The joining form of a syllable between two others, <c>JOINING_FORM_MEDI</c>.</summary>
    [<Literal>]
    let private FormMedi = 2

    /// <summary>The joining form of a syllable that ends a word, <c>JOINING_FORM_FINA</c>.</summary>
    [<Literal>]
    let private FormFina = 3

    /// <summary>Enables each feature of a list globally with the same flags.</summary>
    /// <param name="flags">The flags added to <see cref="FeatureFlags.Global"/>.</param>
    /// <param name="tags">The features.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the features enabled.</returns>
    let private enableAll flags (tags: Tag array) builder =
        tags |> Array.fold (fun added tag -> MapBuilder.enableFeature tag flags 1u added) builder

    /// <summary>Adds each feature of a list, not global, with no flags.</summary>
    /// <param name="tags">The features.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the features added.</returns>
    let private addAll (tags: Tag array) builder =
        tags |> Array.fold (fun added tag -> MapBuilder.addFeature tag FeatureFlags.None 1u added) builder

    /// <summary>Adds the features and pauses of the universal shaping engine, <c>collect_features_use</c>: the syllables are set up
    /// before any lookup, the features that shape an orthographic unit apply per syllable around the pauses that record the repha
    /// and the pre-base consonant, the syllables are reordered and cleared, the topographical features and the others apply after.</summary>
    /// <param name="builder">The builder holding the leading features.</param>
    /// <returns>The builder with the shaper's features and pauses added.</returns>
    let collectFeatures builder =
        builder
        |> MapBuilder.addGsubPause UseSetupSyllables
        |> MapBuilder.enableFeature Tags.Locl FeatureFlags.PerSyllable 1u
        |> MapBuilder.enableFeature Tags.Ccmp FeatureFlags.PerSyllable 1u
        |> MapBuilder.enableFeature Nukt FeatureFlags.PerSyllable 1u
        |> MapBuilder.enableFeature Akhn perSyllableFlags 1u
        |> MapBuilder.addGsubPause ClearSubstitutionFlags
        |> MapBuilder.addFeature Rphf perSyllableFlags 1u
        |> MapBuilder.addGsubPause UseRecordRphf
        |> MapBuilder.addGsubPause ClearSubstitutionFlags
        |> MapBuilder.enableFeature Pref perSyllableFlags 1u
        |> MapBuilder.addGsubPause UseRecordPref
        |> enableAll perSyllableFlags basicFeatures
        |> MapBuilder.addGsubPause UseReorder
        |> MapBuilder.addGsubPause ClearSyllables
        |> addAll topographicalFeatures
        |> MapBuilder.addGsubPause NoPause
        |> enableAll FeatureFlags.ManualZwj otherFeatures

    /// <summary>Reads the plan data from the compiled map, <c>data_create_use</c>: the mask of <c>rphf</c>, the masks of the topographical
    /// features (none for one that is global, <c>setup_topographical_masks</c>'s first loop), and for a script that joins the plan
    /// of the Arabic joining.</summary>
    /// <param name="mask1">The value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</param>
    /// <param name="props">The segment properties of the plan.</param>
    /// <param name="map">The compiled map, for its global mask.</param>
    /// <returns>The plan data.</returns>
    let createData mask1 (props: SegmentProperties) (map: OtMap) =
        let topographical =
            Sealed.init topographicalFeatures.Length (fun index ->
                match mask1 topographicalFeatures[index] with
                | mask when mask = map.GlobalMask -> 0u
                | mask -> mask)
        let arabic =
            match ArabicJoiningList.hasArabicJoining props.Script with
            | true -> ValueSome (Arabic.createPlan mask1 props map)
            | false -> ValueNone
        UseData
            { RphfMask = mask1 Rphf
              TopographicalMasks = topographical
              Arabic = arabic }

    /// <summary>Saves the category of every glyph, <c>setup_masks_use</c>: for a script that joins the glyphs first get the masks of
    /// their joining form. The masks of the syllables cannot be set here, because the syllables are not known yet, so a pause
    /// sets them later.</summary>
    /// <param name="category">The general category of a context character.</param>
    /// <param name="script">The script of the plan.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, normalised.</param>
    /// <returns>The run with the categories set.</returns>
    let setupMasksUsing (category: uint32 -> GeneralCategory) script (data: UsePlan) (run: GlyphRun) =
        let joined =
            match data.Arabic with
            | ValueSome plan -> Arabic.setupMasksUsing category plan script run
            | ValueNone -> run
        let props =
            Sealed.mapi (fun index (props: GlyphProps) ->
                { props with ShaperCategory = UseTable.category joined.Infos[index].Codepoint }) (joined.Props.AsSpan())
        { joined with Props = props }

    /// <summary>Saves the category of every glyph as <see cref="setupMasksUsing"/> does, with the general categories of the default Unicode
    /// functions for the context.</summary>
    /// <param name="script">The script of the plan.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, normalised.</param>
    /// <returns>The run with the categories set.</returns>
    let setupMasks script (data: UsePlan) (run: GlyphRun) = setupMasksUsing Unicode.generalCategory script data run

    /// <summary>The category of one glyph, <see cref="setupMasks"/> for the glyph array of a working set that joins no glyph.</summary>
    [<Struct>]
    type private CategoryMapper =
        val Infos: ImmutableArray<GlyphInfo>

        /// <summary>Creates the mapper.</summary>
        /// <param name="infos">The glyphs, whose code points select the categories.</param>
        new (infos) = { Infos = infos }

        interface IGroupMapper<GlyphProps> with
            member _.GroupEnd (_, start) = start + 1
            member this.Map (items, _, _, index) =
                { items[index] with ShaperCategory = UseTable.category this.Infos[index].Codepoint }

    /// <summary>Saves the category of every glyph as <see cref="setupMasks"/> does, in the properties of a working set whose arrays the caller
    /// made in this call and shared with no one; a script that joins goes through <see cref="setupMasks"/> on a run.</summary>
    /// <param name="script">The script of the plan.</param>
    /// <param name="data">The plan data.</param>
    /// <param name="set">The working set, normalised.</param>
    /// <returns>The set with the categories set.</returns>
    let setupMasksSet script (data: UsePlan) (set: WorkingSet) =
        match data.Arabic with
        | ValueSome plan ->
            let joined = Arabic.setupMasksSet plan script set
            Sealed.mapOwned (CategoryMapper(joined.Infos)) joined.Props
            joined
        | ValueNone ->
            Sealed.mapOwned (CategoryMapper(set.Infos)) set.Props
            set

    /// <summary>Gives the first glyphs of a syllable the mask of the repha, <c>setup_rphf_mask</c>'s loop: the one glyph of a syllable
    /// that begins with a repha, else the first three.</summary>
    /// <param name="mask">The mask of the repha feature.</param>
    /// <param name="rows">The rows of the syllable.</param>
    /// <returns>The rows with the mask set.</returns>
    let private maskRepha mask (rows: ImmutableArray<GlyphRow>) =
        let limit =
            match rows[0].Props.ShaperCategory = UseMachine.R with
            | true -> 1
            | false -> min RephaGlyphs rows.Length
        Sealed.mapi (fun index (row: GlyphRow) ->
            match index < limit with
            | true -> { row with Info = { row.Info with Mask = row.Info.Mask ||| mask } }
            | false -> row) (rows.AsSpan())

    /// <summary>The rewriter that marks every syllable unsafe to break and gives its first glyphs the mask of the repha,
    /// <c>setup_syllables_use</c>'s loop and <c>setup_rphf_mask</c>.</summary>
    [<Struct>]
    type private SetupSyllables =
        val Level: ClusterLevel
        val RphfMask: uint32

        /// <summary>Creates the rewriter.</summary>
        /// <param name="level">The cluster level of the run.</param>
        /// <param name="mask">The mask of the repha feature.</param>
        new (level, mask) = { Level = level; RphfMask = mask }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                let initial : SyllableState =
                    { Rows = Sealed.ofSpan rows
                      Left = ImmutableArray.Empty
                      Right = ImmutableArray.Empty }
                let final = SyllableEdit.unsafeToBreak this.Level 0 rows.Length initial
                let masked =
                    match this.RphfMask with
                    | 0u -> final.Rows
                    | mask -> maskRepha mask final.Rows
                let rewrite : SegmentRewrite =
                    { Rows = masked
                      Left = final.Left
                      Right = final.Right }
                rewrite

    /// <summary>Whether a syllable type joins the syllables beside it, <c>setup_topographical_masks</c>'s switch: the hieroglyph and
    /// non clusters do not.</summary>
    /// <param name="syllable">The syllable byte.</param>
    /// <returns>True when the syllable takes a joining form.</returns>
    let private joins syllable =
        match UseMachine.ofSyllable syllable with
        | UseMachine.UseHieroglyphCluster | UseMachine.UseNonCluster -> false
        | UseMachine.UseViramaTerminatedCluster | UseMachine.UseSakotTerminatedCluster | UseMachine.UseStandardCluster
        | UseMachine.UseNumberJoinerTerminatedCluster | UseMachine.UseNumeralCluster | UseMachine.UseSymbolCluster
        | UseMachine.UseBrokenCluster -> true

    /// <summary>The joining form of a syllable that joins, from whether the syllable before joins and whether the one after does:
    /// the loop of <c>setup_topographical_masks</c> gives a syllable the final form when it joins the one before and the form of
    /// the one before is made medial when it joins this one, so each form is a function of the two neighbours.</summary>
    /// <param name="before">Whether the syllable before joins.</param>
    /// <param name="after">Whether the syllable after joins.</param>
    /// <returns>The joining form.</returns>
    let private formOf before after =
        match before, after with
        | true, true -> FormMedi
        | true, false -> FormFina
        | false, true -> FormInit
        | false, false -> FormIsol

    /// <summary>Finds the chunk a glyph is in, by halving the chunks that begin at or before it.</summary>
    /// <param name="chunks">The chunks, as ascending [start, end) pairs.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="low">The first chunk that may hold the glyph.</param>
    /// <param name="high">The last chunk that may hold the glyph.</param>
    /// <returns>The index of the chunk.</returns>
    [<TailCall>]
    let rec private chunkOf (chunks: ImmutableArray<struct (int * int)>) glyph low high =
        match low >= high with
        | true -> low
        | false ->
            let middle = (low + high + 1) / 2
            let struct (start, _) = chunks[middle]
            match start <= glyph with
            | true -> chunkOf chunks glyph middle high
            | false -> chunkOf chunks glyph low (middle - 1)

    /// <summary>The glyphs of a line with the masks of the joining form of their syllables, <c>setup_topographical_masks</c>: none for a script
    /// whose joining the Arabic plan has set, and none when the map has no joining feature of its own.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="props">The glyph properties, with the syllables set.</param>
    /// <param name="infos">The glyphs.</param>
    /// <returns>The glyphs with the masks set, or none when the masks are not set.</returns>
    let private topographicalInfos (data: UsePlan) (props: ImmutableArray<GlyphProps>) (infos: ImmutableArray<GlyphInfo>) =
        let masks = data.TopographicalMasks
        let allMasks = masks[FormIsol] ||| masks[FormInit] ||| masks[FormMedi] ||| masks[FormFina]
        match data.Arabic.IsSome || allMasks = 0u with
        | true -> ValueNone
        | false ->
            let chunks = SyllableEdit.chunks props
            let joining = Sealed.init chunks.Length (fun chunk -> let struct (start, _) = chunks[chunk] in joins props[start].Syllable)
            let otherMasks = ~~~allMasks
            ValueSome (
                Sealed.mapi (fun glyph (info: GlyphInfo) ->
                    let chunk = chunkOf chunks glyph 0 (chunks.Length - 1)
                    match joining[chunk] with
                    | false -> info
                    | true ->
                        let before = chunk > 0 && joining[chunk - 1]
                        let after = chunk + 1 < chunks.Length && joining[chunk + 1]
                        { info with Mask = (info.Mask &&& otherMasks) ||| masks[formOf before after] }) (infos.AsSpan()))

    /// <summary>Gives the syllables the masks of their joining form, <c>setup_topographical_masks</c>, as <see cref="topographicalInfos"/> says.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the masks set.</returns>
    let private setupTopographicalMasks (data: UsePlan) (run: GlyphRun) =
        match topographicalInfos data run.Props run.Infos with
        | ValueNone -> run
        | ValueSome infos -> { run with Infos = infos }

    /// <summary>Gives the syllables of a working set the masks of their joining form, as <see cref="setupTopographicalMasks"/> does a run.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="set">The set, with its syllables set.</param>
    /// <returns>The set with the masks set.</returns>
    let private setupTopographicalMasksSet (data: UsePlan) (set: WorkingSet) =
        match topographicalInfos data set.Props set.Infos with
        | ValueNone -> set
        | ValueSome infos -> { set with Infos = infos }

    /// <summary>Finds the syllables of the run, <c>setup_syllables_use</c>: the machine paints each glyph with the syllable it
    /// belongs to, after the characters it skips are left out, every syllable is marked unsafe to break, the first glyphs of a
    /// syllable get the mask of the repha, and the syllables get the masks of their joining form. This pause changes no glyph.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, with its categories set.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllables (data: UsePlan) (run: GlyphRun) =
        let categories = Sealed.map (fun (props: GlyphProps) -> props.ShaperCategory) (run.Props.AsSpan())
        let marks = Sealed.map UnicodeProps.isUnicodeMark (run.Props.AsSpan())
        let struct (filtered, positions) = Ragel.useFilter categories marks
        let previous = Sealed.map (fun (props: GlyphProps) -> props.Syllable) (run.Props.AsSpan())
        let struct (syllables, broken) = Ragel.findSyllables UseMachine.machine filtered positions previous
        let painted = Sealed.mapi (fun index (props: GlyphProps) -> { props with Syllable = syllables[index] }) (run.Props.AsSpan())
        let struct (infos, props) = Sealed.rewriteSegments (SetupSyllables(run.ClusterLevel, data.RphfMask)) (run.Infos.AsSpan()) (painted.AsSpan())
        let scratch =
            match broken with
            | true -> run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable
            | false -> run.ScratchFlags
        setupTopographicalMasks data { run with Infos = infos; Props = props; ScratchFlags = scratch }

    /// <summary>Whether a glyph of the run from an index has a category the machine's input filter may drop, a combining grapheme joiner or a
    /// zero-width non-joiner: a run with none is read by the machine as it is.</summary>
    /// <param name="props">The properties of the run.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>True when some glyph from the index on is a joiner the filter looks at.</returns>
    [<TailCall>]
    let rec private anyFiltered (props: ImmutableArray<GlyphProps>) index =
        match index >= props.Length with
        | true -> false
        | false ->
            match props[index].ShaperCategory = UseMachine.CGJ || props[index].ShaperCategory = UseMachine.ZWNJ with
            | true -> true
            | false -> anyFiltered props (index + 1)

    /// <summary>Finds the first glyph from an index that is not a combining grapheme joiner, as the filter of the machine's input does.</summary>
    /// <param name="props">The properties of the run.</param>
    /// <param name="from">The first index to test.</param>
    /// <returns>The index, or none.</returns>
    [<TailCall>]
    let rec private nextNonCgjProps (props: ImmutableArray<GlyphProps>) (from: int) =
        match from < props.Length with
        | false -> ValueNone
        | true ->
            match props[from].ShaperCategory = UseMachine.CGJ with
            | true -> nextNonCgjProps props (from + 1)
            | false -> ValueSome from

    /// <summary>Whether the machine's input filter keeps a glyph: it drops a combining grapheme joiner, and a zero-width non-joiner whose next
    /// glyph that is not a joiner is a Unicode mark (src/hb-ot-shaper-use-machine.hh:909-924).</summary>
    /// <param name="props">The properties of the run.</param>
    /// <param name="index">The glyph.</param>
    /// <returns>True when the glyph is kept.</returns>
    let private keepsGlyph (props: ImmutableArray<GlyphProps>) (index: int) =
        match props[index].ShaperCategory = UseMachine.CGJ with
        | true -> false
        | false ->
            match props[index].ShaperCategory = UseMachine.ZWNJ with
            | false -> true
            | true ->
                match nextNonCgjProps props (index + 1) with
                | ValueNone -> true
                | ValueSome next -> not (UnicodeProps.isUnicodeMark props[next])

    /// <summary>Counts the glyphs the filter keeps.</summary>
    /// <param name="props">The properties of the run.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="count">The count so far.</param>
    /// <returns>The number of kept glyphs.</returns>
    [<TailCall>]
    let rec private countKeptProps (props: ImmutableArray<GlyphProps>) (index: int) (count: int) =
        match index < props.Length with
        | false -> count
        | true -> countKeptProps props (index + 1) (match keepsGlyph props index with | true -> count + 1 | false -> count)

    /// <summary>Finds the first kept glyph at or after an index.</summary>
    /// <param name="props">The properties of the run.</param>
    /// <param name="index">The next candidate.</param>
    /// <returns>The index of the kept glyph.</returns>
    [<TailCall>]
    let rec private nextKeptProps (props: ImmutableArray<GlyphProps>) (index: int) =
        match keepsGlyph props index with
        | true -> index
        | false -> nextKeptProps props (index + 1)

    /// <summary>The buffer indices of the kept glyphs, each found from the one before it.</summary>
    [<Struct>]
    type private KeptProps =
        val Props: ImmutableArray<GlyphProps>

        /// <summary>Creates the unfolder.</summary>
        /// <param name="props">The properties of the run.</param>
        new (props) = { Props = props }

        interface ISpanUnfolder<GlyphProps, int, int> with
            member this.Next (_, cursor) =
                let index = nextKeptProps this.Props cursor
                struct (index, index + 1)

    /// <summary>Finds the syllables of a run whose input the filter shortens and marks each unsafe to break: the machine reads the categories
    /// of the kept glyphs from the properties through the buffer index of each, and is mapped back through them.</summary>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>Whether a broken syllable was found.</returns>
    let private paintFiltered (run: WorkingSet) =
        let positions = Sealed.unfoldSpan (run.Props.AsSpan()) (countKeptProps run.Props 0 0) 0 (KeptProps(run.Props))
        let broken = SyllableOwned.paintFlatKept Ragel.universal positions run.Props
        SyllableOwned.unsafeToBreakSyllables run.ClusterLevel run.Infos run.Props
        broken

    /// <summary>Finds the syllables of the run in place, <c>setup_syllables_use</c> on the working set: the machine paints the
    /// syllable of each glyph into the properties, every syllable is marked unsafe to break and its first glyphs get the mask of the
    /// repha in the glyphs, where <see cref="setupSyllables"/> publishes copies. The result is the run of
    /// <see cref="setupSyllables"/>.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, with its categories set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllablesOwned (data: UsePlan) (run: WorkingSet) =
        let broken =
            match anyFiltered run.Props 0 with
            | false -> SyllableOwned.paintFlatBreaking Ragel.universal run.ClusterLevel run.Infos run.Props
            | true -> paintFiltered run
        match data.RphfMask with
        | 0u -> ()
        | mask -> SyllableOwned.maskSyllableHeads run.Infos run.Props mask UseMachine.R RephaGlyphs
        let scratch =
            match broken with
            | true -> run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable
            | false -> run.ScratchFlags
        setupTopographicalMasksSet data { run with ScratchFlags = scratch }

    /// <summary>Clears the substituted bit of every glyph, <c>_hb_clear_substitution_flags</c>: the pauses that record the repha and the
    /// pre-base consonant see only what the lookups before them substituted.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the substituted bits cleared.</returns>
    let clearSubstitutionFlags (run: GlyphRun) =
        let cleared (props: GlyphProps) = { props with GlyphClass = props.GlyphClass &&& ~~~GlyphPropsBits.Substituted }
        { run with Props = Sealed.mapIfChanged cleared run.Props }

    /// <summary>Clears the substituted bit of every glyph in place, <see cref="clearSubstitutionFlags"/> on the working set.</summary>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the substituted bits cleared.</returns>
    let clearSubstitutionFlagsOwned (run: WorkingSet) =
        SyllableOwned.clearClassBits run.Props GlyphPropsBits.Substituted
        run

    /// <summary>Finds, in the leading glyphs of a syllable that carry a mask, the first one a lookup substituted, <c>record_rphf_use</c>'s
    /// scan.</summary>
    /// <param name="mask">The mask of the repha feature.</param>
    /// <param name="rows">The rows of the syllable.</param>
    /// <param name="index">The next row.</param>
    /// <returns>The index of the glyph, or -1 when none.</returns>
    [<TailCall>]
    let rec private firstSubstitutedUnderMask mask (rows: ImmutableArray<GlyphRow>) index =
        match index < rows.Length && rows[index].Info.Mask &&& mask <> 0u with
        | false -> -1
        | true ->
            match GlyphPropsBits.substituted rows[index].Props with
            | true -> index
            | false -> firstSubstitutedUnderMask mask rows (index + 1)

    /// <summary>Finds the first glyph of a syllable that a lookup substituted, <c>record_pref_use</c>'s scan.</summary>
    /// <param name="rows">The rows of the syllable.</param>
    /// <param name="index">The next row.</param>
    /// <returns>The index of the glyph, or -1 when none.</returns>
    [<TailCall>]
    let rec private firstSubstituted (rows: ImmutableArray<GlyphRow>) index =
        match index < rows.Length with
        | false -> -1
        | true ->
            match GlyphPropsBits.substituted rows[index].Props with
            | true -> index
            | false -> firstSubstituted rows (index + 1)

    /// <summary>Gives the category of the glyph at an index to the rows of a syllable.</summary>
    /// <param name="category">The category.</param>
    /// <param name="found">The index, or -1 when no glyph is to change.</param>
    /// <param name="rows">The rows of the syllable.</param>
    /// <returns>The rewrite.</returns>
    let private recategorised category found (rows: ImmutableArray<GlyphRow>) =
        match found with
        | -1 -> SegmentRewrite.unchanged
        | index ->
            let rewrite : SegmentRewrite =
                { Rows = Sealed.mapi (fun position (row: GlyphRow) ->
                            match position = index with
                            | true -> { row with Props = { row.Props with ShaperCategory = category } }
                            | false -> row) (rows.AsSpan())
                  Left = ImmutableArray.Empty
                  Right = ImmutableArray.Empty }
            rewrite

    /// <summary>The rewriter that marks a substituted repha as <c>R</c>, <c>record_rphf_use</c>.</summary>
    [<Struct>]
    type private RecordRphf =
        val Mask: uint32

        /// <summary>Creates the rewriter.</summary>
        /// <param name="mask">The mask of the repha feature.</param>
        new (mask) = { Mask = mask }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                let items = Sealed.ofSpan rows
                let rewrite = recategorised UseMachine.R (firstSubstitutedUnderMask this.Mask items 0) items
                rewrite

    /// <summary>The rewriter that marks a substituted pre-base consonant as <c>VPre</c>, <c>record_pref_use</c>: it behaves the same
    /// way.</summary>
    [<Struct>]
    type private RecordPref =
        interface ISegmentRewriter with
            member _.Rewrite rows =
                let items = Sealed.ofSpan rows
                let rewrite = recategorised UseMachine.VPre (firstSubstituted items 0) items
                rewrite

    /// <summary>Marks the repha a lookup substituted, <c>record_rphf_use</c>: in each syllable, among the leading glyphs that have the
    /// mask of <c>rphf</c>, the first substituted one becomes the repha.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the repha recorded.</returns>
    let recordRphf (data: UsePlan) (run: GlyphRun) =
        match data.RphfMask with
        | 0u -> run
        | mask ->
            let struct (infos, props) = Sealed.rewriteSegments (RecordRphf(mask)) (run.Infos.AsSpan()) (run.Props.AsSpan())
            { run with Infos = infos; Props = props }

    /// <summary>Marks the pre-base consonant a lookup substituted, <c>record_pref_use</c>: in each syllable the first substituted
    /// glyph becomes a pre-base vowel.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the pre-base consonant recorded.</returns>
    let recordPref (run: GlyphRun) =
        let struct (infos, props) = Sealed.rewriteSegments (RecordPref()) (run.Infos.AsSpan()) (run.Props.AsSpan())
        { run with Infos = infos; Props = props }

    /// <summary>The bit of a category in a set of up to 64 categories, <c>FLAG64_UNSAFE</c>: none for a category of 64 or more.</summary>
    /// <param name="category">The category.</param>
    /// <returns>The bit.</returns>
    let private flag64 (category: byte) =
        match category < 64uy with
        | true -> 1UL <<< int category
        | false -> 0UL

    /// <summary>The categories that stand after the base, <c>POST_BASE_FLAGS64</c>.</summary>
    let private postBaseFlags =
        [| UseMachine.FAbv; UseMachine.FBlw; UseMachine.FPst; UseMachine.FMAbv; UseMachine.FMBlw; UseMachine.FMPst
           UseMachine.MAbv; UseMachine.MBlw; UseMachine.MPst; UseMachine.MPre; UseMachine.VAbv; UseMachine.VBlw
           UseMachine.VPst; UseMachine.VPre; UseMachine.VMAbv; UseMachine.VMBlw; UseMachine.VMPst; UseMachine.VMPre |]
        |> Array.fold (fun flags category -> flags ||| flag64 category) 0UL

    /// <summary>Whether a glyph is a halant that has not been ligated, <c>is_halant_use</c>.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>True for a halant, a halant or vowel modifier, or an invisible stacker.</returns>
    let private isHalant (props: GlyphProps) =
        (props.ShaperCategory = UseMachine.H || props.ShaperCategory = UseMachine.HVM || props.ShaperCategory = UseMachine.IS)
        && not (GlyphPropsBits.ligated props)

    /// <summary>Whether a glyph stands after the base, <c>is_post_base_glyph</c>'s test in <c>reorder_syllable_use</c>.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>True for a post-base category or a halant.</returns>
    let private isPostBase (props: GlyphProps) =
        flag64 props.ShaperCategory &&& postBaseFlags <> 0UL || isHalant props

    /// <summary>Finds where a repha goes: before the first post-base glyph, else at the end of the syllable, <c>reorder_syllable_use</c>'s
    /// forward loop.</summary>
    /// <param name="rows">The rows of the syllable.</param>
    /// <param name="index">The next row.</param>
    /// <returns>The index the repha moves to.</returns>
    [<TailCall>]
    let rec private rephaTarget (rows: ImmutableArray<GlyphRow>) index =
        match isPostBase rows[index].Props with
        | true -> index - 1
        | false ->
            match index = rows.Length - 1 with
            | true -> index
            | false -> rephaTarget rows (index + 1)

    /// <summary>Moves a repha towards the end of its syllable, but before the first post-base glyph, merging the clusters it passes.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the repha moved.</returns>
    let private moveRepha level (state: SyllableState) =
        match state.Rows[0].Props.ShaperCategory = UseMachine.R && state.Rows.Length > 1 with
        | false -> state
        | true ->
            let target = rephaTarget state.Rows 1
            let merged = SyllableEdit.mergeClusters level 0 (target + 1) state
            { merged with Rows = Sealed.move 0 target merged.Rows }

    /// <summary>Whether a glyph is a pre-base vowel or vowel modifier that moves back: the first component of a substitution only.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>True for a <c>VPre</c> or <c>VMPre</c> that is not a later component.</returns>
    let private movesBack (props: GlyphProps) =
        (props.ShaperCategory = UseMachine.VPre || props.ShaperCategory = UseMachine.VMPre) && GlyphPropsBits.ligComp props = 0uy

    /// <summary>Moves the pre-base vowels back to just after the last halant, merging the clusters they pass,
    /// <c>reorder_syllable_use</c>'s backward loop.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="index">The next row.</param>
    /// <param name="after">The index after the last halant, or the start of the syllable.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the vowels moved.</returns>
    [<TailCall>]
    let rec private moveBack level index after (state: SyllableState) =
        match index < state.Rows.Length with
        | false -> state
        | true ->
            let props = state.Rows[index].Props
            match isHalant props with
            | true -> moveBack level (index + 1) (index + 1) state
            | false ->
                match movesBack props && after < index with
                | false -> moveBack level (index + 1) after state
                | true ->
                    let merged = SyllableEdit.mergeClusters level after (index + 1) state
                    moveBack level (index + 1) after { merged with Rows = Sealed.move index after merged.Rows }

    /// <summary>The rewriter that reorders every syllable, <c>reorder_syllable_use</c>.</summary>
    [<Struct>]
    type private ReorderSyllables =
        val Level: ClusterLevel

        /// <summary>Creates the rewriter.</summary>
        /// <param name="level">The cluster level of the run.</param>
        new (level) = { Level = level }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                match UseMachine.ofSyllable rows[0].Props.Syllable with
                | UseMachine.UseHieroglyphCluster | UseMachine.UseNonCluster | UseMachine.UseNumberJoinerTerminatedCluster
                | UseMachine.UseNumeralCluster -> SegmentRewrite.unchanged
                | UseMachine.UseViramaTerminatedCluster | UseMachine.UseSakotTerminatedCluster | UseMachine.UseStandardCluster
                | UseMachine.UseSymbolCluster | UseMachine.UseBrokenCluster ->
                    let initial : SyllableState =
                        { Rows = Sealed.ofSpan rows
                          Left = ImmutableArray.Empty
                          Right = ImmutableArray.Empty }
                    let final = initial |> moveRepha this.Level |> moveBack this.Level 0 0
                    let rewrite : SegmentRewrite =
                        { Rows = final.Rows
                          Left = final.Left
                          Right = final.Right }
                    rewrite

    /// <summary>Inserts a dotted circle before each broken syllable, after its repha, <c>hb_syllabic_insert_dotted_circles</c> as
    /// <c>reorder_use</c> calls it.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCircles (font: Font) (run: GlyphRun) =
        let struct (inserted, _) = SyllableEdit.insertDottedCircles font BrokenCluster UseMachine.B (ValueSome UseMachine.R) ValueNone run
        inserted

    /// <summary>Inserts a dotted circle before each broken syllable in arrays of the right size, <see cref="insertDottedCircles"/> for a
    /// run whose arrays were made in this call and are shared with no one.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCirclesOwned (font: Font) (run: WorkingSet) =
        let struct (inserted, _) = SyllableEdit.insertDottedCirclesOwned font BrokenCluster UseMachine.B (ValueSome UseMachine.R) ValueNone run
        inserted

    /// <summary>Reorders the syllables of the run, <c>reorder_use</c>: broken clusters get a dotted circle, then in each syllable a
    /// repha moves towards the end and the pre-base vowels move back after the last halant.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The reordered run.</returns>
    let reorder (font: Font) (run: GlyphRun) =
        let circled = insertDottedCircles font run
        let struct (infos, props) = Sealed.rewriteSegments (ReorderSyllables(circled.ClusterLevel)) (circled.Infos.AsSpan()) (circled.Props.AsSpan())
        { circled with Infos = infos; Props = props }

    /// <summary>Composes two code points, <c>compose_use</c>: a mark is never recomposed, so that the split matras stay apart.</summary>
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

    /// <summary>Inserts a dotted circle into every prohibited vowel sequence of the script, <c>preprocess_text_use</c>.</summary>
    /// <param name="run">The run, with its Unicode properties set.</param>
    /// <returns>The prepared run.</returns>
    let preprocess (run: GlyphRun) = VowelConstraints.preprocess run

    /// <summary>Finds, in the leading glyphs of a syllable that carry a mask, the first one a lookup substituted.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties.</param>
    /// <param name="mask">The mask of the repha feature.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <returns>The index of the glyph, or -1 when none.</returns>
    [<TailCall>]
    let rec private firstSubstitutedUnderMaskOwned (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) mask index finish =
        match index < finish && infos[index].Mask &&& mask <> 0u with
        | false -> -1
        | true ->
            match GlyphPropsBits.substituted props[index] with
            | true -> index
            | false -> firstSubstitutedUnderMaskOwned infos props mask (index + 1) finish

    /// <summary>Finds the first glyph of a syllable that a lookup substituted.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <returns>The index of the glyph, or -1 when none.</returns>
    [<TailCall>]
    let rec private firstSubstitutedOwned (props: ImmutableArray<GlyphProps>) index finish =
        match index < finish with
        | false -> -1
        | true ->
            match GlyphPropsBits.substituted props[index] with
            | true -> index
            | false -> firstSubstitutedOwned props (index + 1) finish

    /// <summary>Gives the glyph at an index a category.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="category">The category.</param>
    /// <param name="found">The index, or -1 when no glyph is to change.</param>
    let private recategorisedOwned (props: ImmutableArray<GlyphProps>) category found =
        match found with
        | -1 -> ()
        | index -> SyllableOwned.setProp props index { props[index] with ShaperCategory = category }

    /// <summary>Marks the glyph each syllable chunk's scan finds, in place: the repha when <paramref name="mask"/> is given, else the
    /// pre-base consonant.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties.</param>
    /// <param name="mask">The mask of the repha feature, zero to record the pre-base consonant.</param>
    /// <param name="start">The first glyph of the next chunk.</param>
    [<TailCall>]
    let rec private recordOwned (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (mask: uint32) (start: int) =
        match start < props.Length with
        | false -> ()
        | true ->
            let finish = Sealed.nextSyllableEnd (props.AsSpan()) start
            match mask with
            | 0u -> recategorisedOwned props UseMachine.VPre (firstSubstitutedOwned props start finish)
            | _ -> recategorisedOwned props UseMachine.R (firstSubstitutedUnderMaskOwned infos props mask start finish)
            recordOwned infos props mask finish

    /// <summary>Marks the repha a lookup substituted in place, <see cref="recordRphf"/> on the working set.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the repha recorded.</returns>
    let recordRphfOwned (data: UsePlan) (run: WorkingSet) =
        match data.RphfMask with
        | 0u -> run
        | mask ->
            recordOwned run.Infos run.Props mask 0
            run

    /// <summary>Marks the pre-base consonant a lookup substituted in place, <see cref="recordPref"/> on the working set.</summary>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the pre-base consonant recorded.</returns>
    let recordPrefOwned (run: WorkingSet) =
        recordOwned run.Infos run.Props 0u 0
        run

    /// <summary>Finds where a repha goes: before the first post-base glyph, else at the end of the syllable.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The index the repha moves to.</returns>
    [<TailCall>]
    let rec private rephaTargetOwned (props: ImmutableArray<GlyphProps>) finish index =
        match isPostBase props[index] with
        | true -> index - 1
        | false ->
            match index = finish - 1 with
            | true -> index
            | false -> rephaTargetOwned props finish (index + 1)

    /// <summary>Moves the pre-base vowels back to just after the last halant in place, merging the clusters they pass.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="after">The index after the last halant, or the start of the syllable.</param>
    [<TailCall>]
    let rec private moveBackOwned level (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) finish index after =
        match index < finish with
        | false -> ()
        | true ->
            let current = props[index]
            match isHalant current with
            | true -> moveBackOwned level infos props finish (index + 1) (index + 1)
            | false ->
                match movesBack current && after < index with
                | false -> moveBackOwned level infos props finish (index + 1) after
                | true ->
                    SyllableOwned.mergeClusters level infos after (index + 1)
                    SyllableOwned.move infos props index after
                    moveBackOwned level infos props finish (index + 1) after

    /// <summary>Reorders the syllable chunks of a run from an index in place.</summary>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <param name="start">The first glyph of the next chunk.</param>
    [<TailCall>]
    let rec private reorderChunksOwned (run: WorkingSet) (start: int) =
        match start < run.Props.Length with
        | false -> ()
        | true ->
            let finish = Sealed.nextSyllableEnd (run.Props.AsSpan()) start
            match UseMachine.ofSyllable run.Props[start].Syllable with
            | UseMachine.UseHieroglyphCluster | UseMachine.UseNonCluster | UseMachine.UseNumberJoinerTerminatedCluster
            | UseMachine.UseNumeralCluster -> ()
            | UseMachine.UseViramaTerminatedCluster | UseMachine.UseSakotTerminatedCluster | UseMachine.UseStandardCluster
            | UseMachine.UseSymbolCluster | UseMachine.UseBrokenCluster ->
                match run.Props[start].ShaperCategory = UseMachine.R && finish - start > 1 with
                | false -> ()
                | true ->
                    let target = rephaTargetOwned run.Props finish (start + 1)
                    SyllableOwned.mergeClusters run.ClusterLevel run.Infos start (target + 1)
                    SyllableOwned.move run.Infos run.Props start target
                moveBackOwned run.ClusterLevel run.Infos run.Props finish start start
            reorderChunksOwned run finish

    /// <summary>Reorders the syllables of the run in place, <c>reorder_use</c> on the working set: the result of <see cref="reorder"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The reordered run.</returns>
    let reorderOwned (font: Font) (run: WorkingSet) =
        let circled = insertDottedCirclesOwned font run
        reorderChunksOwned circled 0
        circled
