namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Indic shaper's plan and hooks, <c>_hb_ot_shaper_indic</c> (src/hb-ot-shaper-indic.cc:37-423,1464-1541): the
/// configuration of each script, the features and the pauses between them, the plan data, the categories and positions of
/// the glyphs, the syllables, the vowel constraints, and the decomposition and composition of the matras. The two
/// reorderings are <see cref="IndicInitial"/> and <see cref="IndicFinal"/>.</summary>
[<RequireQualifiedAccess>]
module internal Indic =
    /// <summary>The feature <c>nukt</c>, the nukta forms.</summary>
    [<Literal>]
    let private Nukt = 0x6E756B74u<tag>

    /// <summary>The feature <c>akhn</c>, the akhand ligatures.</summary>
    [<Literal>]
    let private Akhn = 0x616B686Eu<tag>

    /// <summary>The feature <c>rphf</c>, the reph forms.</summary>
    [<Literal>]
    let private Rphf = 0x72706866u<tag>

    /// <summary>The feature <c>rkrf</c>, the rakaar forms.</summary>
    [<Literal>]
    let private Rkrf = 0x726B7266u<tag>

    /// <summary>The feature <c>pref</c>, the pre-base forms.</summary>
    [<Literal>]
    let private Pref = 0x70726566u<tag>

    /// <summary>The feature <c>blwf</c>, the below-base forms.</summary>
    [<Literal>]
    let private Blwf = 0x626C7766u<tag>

    /// <summary>The feature <c>abvf</c>, the above-base forms.</summary>
    [<Literal>]
    let private Abvf = 0x61627666u<tag>

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

    /// <summary>The feature <c>init</c>, the initial forms.</summary>
    [<Literal>]
    let private Init = 0x696E6974u<tag>

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

    /// <summary>The feature <c>haln</c>, the halant forms.</summary>
    [<Literal>]
    let private Haln = 0x68616C6Eu<tag>

    /// <summary>The syllable type of a broken cluster, <c>indic_broken_cluster</c>: the low nibble of the syllable byte.</summary>
    [<Literal>]
    let private BrokenCluster = 4uy

    /// <summary>The character the second byte of a new-spec script tag ends with, <c>'2'</c> of <c>dev2</c>.</summary>
    [<Literal>]
    let private NewSpecTagSuffix = 0x32u

    /// <summary>The low byte of a tag.</summary>
    [<Literal>]
    let private LowByte = 0xFFu

    /// <summary>The first category that no bit of a 32-bit flag set stands for, <c>FLAG_UNSAFE</c>.</summary>
    [<Literal>]
    let private FlagBits = 32uy

    /// <summary>The flags of a feature applied one at a time after the initial reordering and constrained to the syllable,
    /// that every glyph takes part in, <c>F_GLOBAL_MANUAL_JOINERS | F_PER_SYLLABLE</c>.</summary>
    let private globalFlags = FeatureFlags.GlobalManualJoiners ||| FeatureFlags.PerSyllable

    /// <summary>The flags of a feature only the glyphs the reordering marks take part in, <c>F_MANUAL_JOINERS | F_PER_SYLLABLE</c>.</summary>
    let private maskedFlags = FeatureFlags.ManualJoiners ||| FeatureFlags.PerSyllable

    /// <summary>The features applied one at a time after the initial reordering, <c>indic_features</c> up to
    /// <c>INDIC_BASIC_FEATURES</c> (src/hb-ot-shaper-indic.cc:169-228).</summary>
    let private basicFeatures : struct (Tag * uint32) array =
        [| struct (Nukt, globalFlags); struct (Akhn, globalFlags); struct (Rphf, maskedFlags); struct (Rkrf, globalFlags)
           struct (Pref, maskedFlags); struct (Blwf, maskedFlags); struct (Abvf, maskedFlags); struct (Half, maskedFlags)
           struct (Pstf, maskedFlags); struct (Vatu, globalFlags); struct (Cjct, globalFlags) |]

    /// <summary>The features applied all at once after the final reordering, <c>indic_features</c> from
    /// <c>INDIC_BASIC_FEATURES</c>.</summary>
    let private otherFeatures : struct (Tag * uint32) array =
        [| struct (Init, maskedFlags); struct (Pres, globalFlags); struct (Abvs, globalFlags); struct (Blws, globalFlags)
           struct (Psts, globalFlags); struct (Haln, globalFlags) |]

    /// <summary>The configuration of a script, <c>indic_configs</c> (src/hb-ot-shaper-indic.cc:148-164): the first entry is the
    /// default, for the scripts this shaper takes that are not listed.</summary>
    /// <param name="script">The script of the segment.</param>
    /// <returns>The configuration.</returns>
    let config (script: Script) =
        let entry oldSpec virama reph mode blwf =
            { HasOldSpec = oldSpec
              Virama = virama
              RephPosition = reph
              RephMode = mode
              BlwfMode = blwf }
        match script with
        | Scripts.Devanagari -> entry true 0x094Du RephBeforePost RephImplicit BlwfPreAndPost
        | Scripts.Bengali -> entry true 0x09CDu RephAfterSub RephImplicit BlwfPreAndPost
        | Scripts.Gurmukhi -> entry true 0x0A4Du RephBeforeSub RephImplicit BlwfPreAndPost
        | Scripts.Gujarati -> entry true 0x0ACDu RephBeforePost RephImplicit BlwfPreAndPost
        | Scripts.Oriya -> entry true 0x0B4Du RephAfterMain RephImplicit BlwfPreAndPost
        | Scripts.Tamil -> entry true 0x0BCDu RephAfterPost RephImplicit BlwfPreAndPost
        | Scripts.Telugu -> entry true 0x0C4Du RephAfterPost RephExplicit BlwfPostOnly
        | Scripts.Kannada -> entry true 0x0CCDu RephAfterPost RephImplicit BlwfPostOnly
        | Scripts.Malayalam -> entry true 0x0D4Du RephAfterMain RephLogRepha BlwfPreAndPost
        | _ -> entry false 0u RephBeforePost RephImplicit BlwfPreAndPost

    /// <summary>Adds the features and pauses of the Indic shaper, <c>collect_features_indic</c> (src/hb-ot-shaper-indic.cc:243-273):
    /// the syllables are set up before any lookup, <c>locl</c> and <c>ccmp</c> apply per syllable, the initial reordering
    /// follows, each basic feature is a stage of its own, the final reordering follows them, and the other features apply
    /// after it.</summary>
    /// <param name="builder">The builder holding the leading features.</param>
    /// <returns>The builder with the shaper's features and pauses added.</returns>
    let collectFeatures builder =
        let basic =
            builder
            |> MapBuilder.addGsubPause IndicSetupSyllables
            |> MapBuilder.enableFeature Tags.Locl FeatureFlags.PerSyllable 1u
            |> MapBuilder.enableFeature Tags.Ccmp FeatureFlags.PerSyllable 1u
            |> MapBuilder.addGsubPause IndicInitialReordering
        let basicAdded =
            basicFeatures
            |> Array.fold (fun (state: MapBuilder) (struct (tag, flags)) -> state |> MapBuilder.addFeature tag flags 1u |> MapBuilder.addGsubPause NoPause) basic
        otherFeatures
        |> Array.fold (fun (state: MapBuilder) (struct (tag, flags)) -> MapBuilder.addFeature tag flags 1u state) (MapBuilder.addGsubPause IndicFinalReordering basicAdded)

    /// <summary>Turns <c>liga</c> off and ends the syllables, <c>override_features_indic</c> (src/hb-ot-shaper-indic.cc:275-280).</summary>
    /// <param name="builder">The builder holding every other feature.</param>
    /// <returns>The builder with the overrides applied.</returns>
    let overrideFeatures builder =
        builder
        |> MapBuilder.disableFeature Tags.Liga
        |> MapBuilder.addGsubPause ClearSyllables

    /// <summary>Reads what a feature of the compiled map does at its own stage, <c>hb_indic_would_substitute_feature_t::init</c>.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="zeroContext">Whether the match takes no context.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The lookups of the feature's stage.</returns>
    let private wouldSubstituteOf (map: OtMap) zeroContext tag : IndicWouldSubstitute =
        { Lookups = MapBuilder.stageLookups map tag
          ZeroContext = zeroContext
          Prepared = Memo<obj>() }

    /// <summary>Builds the plan data from the compiled map, <c>data_create_indic</c> (src/hb-ot-shaper-indic.cc:296-342): the
    /// features applied globally have no mask of their own. Zero-context matching is for the new specification of the main
    /// scripts and the scripts with one specification, never for the old specification, and not for Malayalam, where both
    /// allow context.</summary>
    /// <param name="mask1">The value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</param>
    /// <param name="props">The segment properties of the plan.</param>
    /// <param name="map">The compiled map.</param>
    /// <returns>The plan data.</returns>
    let createData mask1 (props: SegmentProperties) (map: OtMap) =
        let configured = config props.Script
        let isOldSpec = configured.HasOldSpec && (uint32 map.ChosenGsubScript &&& LowByte) <> NewSpecTagSuffix
        let zeroContext = not isOldSpec && props.Script <> Scripts.Malayalam
        let wouldSubstitute = wouldSubstituteOf map zeroContext
        IndicData
            { Config = configured
              IsOldSpec = isOldSpec
              RphfFeature = wouldSubstitute Rphf
              PrefFeature = wouldSubstitute Pref
              BlwfFeature = wouldSubstitute Blwf
              PstfFeature = wouldSubstitute Pstf
              VatuFeature = wouldSubstitute Vatu
              Positions = Memo<obj>()
              RphfMask = mask1 Rphf
              PrefMask = mask1 Pref
              BlwfMask = mask1 Blwf
              AbvfMask = mask1 Abvf
              HalfMask = mask1 Half
              PstfMask = mask1 Pstf
              InitMask = mask1 Init }

    /// <summary>The bit a category stands for in a set of categories, <c>FLAG_UNSAFE</c> (src/hb-algs.hh:87): a category past the
    /// width of the set stands for none.</summary>
    /// <param name="category">The category.</param>
    /// <returns>The bit, zero for a category of 32 or more.</returns>
    let flagUnsafe (category: byte) =
        match category < FlagBits with
        | true -> 1u <<< int category
        | false -> 0u

    /// <summary>The categories of the glyphs treated as consonants, <c>CONSONANT_FLAGS_INDIC</c>: vowels and placeholders are
    /// treated as consonants, so that the consonant logic serves the vowel syllable (src/hb-ot-shaper-indic.cc:61-66).</summary>
    let consonantFlags =
        flagUnsafe IndicMachine.C ||| flagUnsafe IndicMachine.CS ||| flagUnsafe IndicMachine.Ra ||| flagUnsafe IndicMachine.CM
        ||| flagUnsafe IndicMachine.V ||| flagUnsafe IndicMachine.PLACEHOLDER ||| flagUnsafe IndicMachine.DOTTEDCIRCLE

    /// <summary>The categories of the joiners, <c>JOINER_FLAGS</c>.</summary>
    let joinerFlags = flagUnsafe IndicMachine.ZWJ ||| flagUnsafe IndicMachine.ZWNJ

    /// <summary>Whether a glyph is one of a set of categories, <c>is_one_of</c> (src/hb-ot-shaper-indic.cc:50-55): a glyph that
    /// ligated is none of them.</summary>
    /// <param name="flags">The set of categories.</param>
    /// <param name="props">The properties of the glyph.</param>
    /// <returns>True when the glyph has not ligated and its category is in the set.</returns>
    let isOneOf flags (props: GlyphProps) =
        not (GlyphPropsBits.ligated props) && flagUnsafe props.ShaperCategory &&& flags <> 0u

    /// <summary>Whether a glyph is a consonant, <c>is_consonant</c>.</summary>
    /// <param name="props">The properties of the glyph.</param>
    /// <returns>True for a consonant, a vowel or a placeholder that has not ligated.</returns>
    let isConsonant (props: GlyphProps) = isOneOf consonantFlags props

    /// <summary>Whether a glyph is a joiner, <c>is_joiner</c>.</summary>
    /// <param name="props">The properties of the glyph.</param>
    /// <returns>True for a ZWJ or a ZWNJ that has not ligated.</returns>
    let isJoiner (props: GlyphProps) = isOneOf joinerFlags props

    /// <summary>Whether a glyph is a halant, <c>is_halant</c>.</summary>
    /// <param name="props">The properties of the glyph.</param>
    /// <returns>True for a halant that has not ligated.</returns>
    let isHalant (props: GlyphProps) = isOneOf (flagUnsafe IndicMachine.H) props

    /// <summary>Skips the joiners from an index, <c>while (limit &lt; end &amp;&amp; is_joiner (info[limit])) limit++</c>.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The first index.</param>
    /// <returns>The index of the first glyph that is not a joiner, the end of the rows when none.</returns>
    [<TailCall>]
    let rec skipJoiners (rows: ImmutableArray<GlyphRow>) index =
        match index < rows.Length && isJoiner rows[index].Props with
        | true -> skipJoiners rows (index + 1)
        | false -> index

    /// <summary>The virama glyph of the script of a plan, <c>load_virama_glyph</c> (src/hb-ot-shaper-indic.cc:283-304): the
    /// glyph of the configuration's virama, none when the script has none or the font does not map it. Upstream reads it once
    /// and keeps it; it reads only the cmap, so reading it again gives the same glyph.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <returns>The glyph, or none.</returns>
    let viramaGlyph (plan: IndicPlan) (font: Font) =
        match plan.Config.Virama with
        | 0u -> ValueNone
        | virama ->
            match Font.nominalGlyph font virama with
            | ValueSome glyph when glyph <> 0u -> ValueSome glyph
            | ValueSome _ | ValueNone -> ValueNone

    /// <summary>Saves the category and position of every glyph, <c>setup_masks_indic</c> (src/hb-ot-shaper-indic.cc:425-440)
    /// with <c>set_indic_properties</c>: the masks cannot be set here, because the syllables are not known yet, so the
    /// reordering pause sets them later.</summary>
    /// <param name="run">The run, normalised.</param>
    /// <returns>The run with the categories and positions set.</returns>
    let setupMasks (run: GlyphRun) =
        let props =
            Sealed.mapi (fun index (props: GlyphProps) ->
                let categories = IndicTable.categories run.Infos[index].Codepoint
                { props with ShaperCategory = byte categories; ShaperAuxiliary = byte (categories >>> 8) }) (run.Props.AsSpan())
        { run with Props = props }

    /// <summary>The category and position of one glyph, <see cref="setupMasks"/> for the glyph array of a working set.</summary>
    [<Struct>]
    type private CategoryMapper =
        val Infos: ImmutableArray<GlyphInfo>

        /// <summary>Creates the mapper.</summary>
        /// <param name="infos">The glyphs, whose code points select the categories.</param>
        new (infos) = { Infos = infos }

        interface IGroupMapper<GlyphProps> with
            member _.GroupEnd (_, start) = start + 1
            member this.Map (items, _, _, index) =
                let categories = IndicTable.categories this.Infos[index].Codepoint
                { items[index] with ShaperCategory = byte categories; ShaperAuxiliary = byte (categories >>> 8) }

    /// <summary>Saves the category and position of every glyph as <see cref="setupMasks"/> does, in the properties of a working set whose arrays
    /// the caller made in this call and shared with no one.</summary>
    /// <param name="set">The working set, normalised.</param>
    /// <returns>The set with the categories and positions set.</returns>
    let setupMasksSet (set: WorkingSet) =
        Sealed.mapOwned (CategoryMapper(set.Infos)) set.Props
        set

    /// <summary>The rewriter that marks every syllable unsafe to break, <c>setup_syllables_indic</c>'s loop.</summary>
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

    /// <summary>Finds the syllables of the run, <c>setup_syllables_indic</c> (src/hb-ot-shaper-indic.cc:442-452): the machine
    /// paints each glyph with the syllable it belongs to, and every syllable is marked unsafe to break. This pause changes
    /// no glyph.</summary>
    /// <param name="run">The run, with its categories set.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllables (run: GlyphRun) =
        let categories = Sealed.map (fun (props: GlyphProps) -> props.ShaperCategory) (run.Props.AsSpan())
        let previous = Sealed.map (fun (props: GlyphProps) -> props.Syllable) (run.Props.AsSpan())
        let struct (syllables, broken) = Ragel.findSyllables IndicMachine.machine categories Unchecked.defaultof<ImmutableArray<int>> previous
        let painted = Sealed.mapi (fun index (props: GlyphProps) -> { props with Syllable = syllables[index] }) (run.Props.AsSpan())
        let struct (infos, props) = Sealed.rewriteSegments (UnsafeSyllables(run.ClusterLevel)) (run.Infos.AsSpan()) (painted.AsSpan())
        let scratch =
            match broken with
            | true -> run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable
            | false -> run.ScratchFlags
        { run with Infos = infos; Props = props; ScratchFlags = scratch }

    /// <summary>Finds the syllables of the run in place, <c>setup_syllables_indic</c> on the working set: the machine paints the syllable of each
    /// glyph into the properties and every syllable is marked unsafe to break in the glyphs, where <see cref="setupSyllables"/>
    /// publishes copies. The result is the run of <see cref="setupSyllables"/>.</summary>
    /// <param name="run">The run, with its categories set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllablesOwned (run: WorkingSet) =
        let broken = SyllableOwned.paintFlatBreaking Ragel.indic run.ClusterLevel run.Infos run.Props
        match broken with
        | true -> { run with ScratchFlags = run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable }
        | false -> run

    /// <summary>Inserts a dotted circle before each broken syllable, after its leading repha glyphs,
    /// <c>hb_syllabic_insert_dotted_circles</c> as <c>initial_reordering_indic</c> calls it (src/hb-ot-shaper-indic.cc:985-988).</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCircles (font: Font) (run: GlyphRun) =
        let struct (inserted, _) =
            SyllableEdit.insertDottedCircles font BrokenCluster IndicMachine.DOTTEDCIRCLE (ValueSome IndicMachine.Repha) (ValueSome IndicPosition.PosEnd) run
        inserted

    /// <summary>Inserts a dotted circle before each broken syllable in arrays of the right size, <see cref="insertDottedCircles"/> for a
    /// run whose arrays were made in this call and are shared with no one.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCirclesOwned (font: Font) (run: WorkingSet) =
        let struct (inserted, _) =
            SyllableEdit.insertDottedCirclesOwned font BrokenCluster IndicMachine.DOTTEDCIRCLE (ValueSome IndicMachine.Repha) (ValueSome IndicPosition.PosEnd) run
        inserted

    /// <summary>Inserts a dotted circle into every prohibited vowel sequence of the script, <c>preprocess_text_indic</c>
    /// (src/hb-ot-shaper-indic.cc:1464-1470).</summary>
    /// <param name="run">The run, with its Unicode properties set.</param>
    /// <returns>The prepared run.</returns>
    let preprocess (run: GlyphRun) = VowelConstraints.preprocess run

    /// <summary>Decomposes a code point for the normalizer, <c>decompose_indic</c> (src/hb-ot-shaper-indic.cc:1472-1500): the
    /// letters whose decomposition would split what the fonts treat as one are kept whole.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let decompose u =
        match u with
        | 0x0931u | 0x09DCu | 0x09DDu | 0x0B94u -> ValueNone
        | _ -> Unicode.decompose u

    /// <summary>Decomposes a code point as <see cref="decompose"/> does, the others through replaced Unicode functions.</summary>
    /// <param name="unicode">The functions.</param>
    /// <param name="u">The code point.</param>
    /// <returns>The two parts, or none.</returns>
    let decomposeWith (unicode: UnicodeFuncs) u =
        match u with
        | 0x0931u | 0x09DCu | 0x09DDu | 0x0B94u -> ValueNone
        | _ -> unicode.Decompose u |> Settled.value

    /// <summary>Composes two code points for the normalizer, <c>compose_indic</c> (src/hb-ot-shaper-indic.cc:1502-1515): a mark
    /// is never recomposed, so that the split matras stay apart, and the Bengali letter yya is composed though Unicode
    /// excludes it.</summary>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let compose a b =
        match UnicodeProps.isMark (Unicode.generalCategory a) with
        | true -> ValueNone
        | false ->
            match a = 0x09AFu && b = 0x09BCu with
            | true -> ValueSome 0x09DFu
            | false -> Unicode.compose a b

    /// <summary>Composes two code points as <see cref="compose"/> does, through replaced Unicode functions.</summary>
    /// <param name="unicode">The functions.</param>
    /// <param name="a">The first code point.</param>
    /// <param name="b">The second code point.</param>
    /// <returns>The composite, or none.</returns>
    let composeWith (unicode: UnicodeFuncs) a b =
        match UnicodeProps.isMark (unicode.GeneralCategory a) with
        | true -> ValueNone
        | false ->
            match a = 0x09AFu && b = 0x09BCu with
            | true -> ValueSome 0x09DFu
            | false -> unicode.Compose a b |> Settled.value
