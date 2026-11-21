namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Myanmar shaper's hooks, <c>_hb_ot_shaper_myanmar</c> (src/hb-ot-shaper-myanmar.cc:41-363): the features and
/// the pauses between them, the syllables, the reordering of the consonants, pre-base vowels, medial ra and the below-base
/// marks, and the left-matra flip. The Zawgyi shaper has no hooks; its profile alone is what <see cref="Categorize.profile"/>
/// returns.</summary>
[<RequireQualifiedAccess>]
module internal Myanmar =
    /// <summary>The feature <c>rphf</c>, the reph forms.</summary>
    [<Literal>]
    let private Rphf = 0x72706866u<tag>

    /// <summary>The feature <c>pref</c>, the pre-base forms.</summary>
    [<Literal>]
    let private Pref = 0x70726566u<tag>

    /// <summary>The feature <c>blwf</c>, the below-base forms.</summary>
    [<Literal>]
    let private Blwf = 0x626C7766u<tag>

    /// <summary>The feature <c>pstf</c>, the post-base forms.</summary>
    [<Literal>]
    let private Pstf = 0x70737466u<tag>

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

    /// <summary>The syllable type of a broken cluster, <c>myanmar_broken_cluster</c>: the low nibble of the syllable byte.</summary>
    [<Literal>]
    let private BrokenCluster = 1uy

    /// <summary>The flags of the basic features: joiners are not skipped and the lookups stay within a syllable,
    /// <c>F_MANUAL_ZWJ | F_PER_SYLLABLE</c>.</summary>
    let private basicFlags = FeatureFlags.ManualZwj ||| FeatureFlags.PerSyllable

    /// <summary>Adds the features and pauses of the Myanmar shaper, <c>collect_features_myanmar</c>: the syllables are set up
    /// and reordered before any lookup, the basic features apply one at a time per syllable, the syllables are cleared, and
    /// the other features apply after.</summary>
    /// <param name="builder">The builder holding the leading features.</param>
    /// <returns>The builder with the shaper's features and pauses added.</returns>
    let collectFeatures builder =
        builder
        |> MapBuilder.addGsubPause MyanmarSetupSyllables
        |> MapBuilder.enableFeature Tags.Locl FeatureFlags.PerSyllable 1u
        |> MapBuilder.enableFeature Tags.Ccmp FeatureFlags.PerSyllable 1u
        |> MapBuilder.addGsubPause MyanmarReorder
        |> MapBuilder.enableFeature Rphf basicFlags 1u
        |> MapBuilder.addGsubPause NoPause
        |> MapBuilder.enableFeature Pref basicFlags 1u
        |> MapBuilder.addGsubPause NoPause
        |> MapBuilder.enableFeature Blwf basicFlags 1u
        |> MapBuilder.addGsubPause NoPause
        |> MapBuilder.enableFeature Pstf basicFlags 1u
        |> MapBuilder.addGsubPause NoPause
        |> MapBuilder.addGsubPause ClearSyllables
        |> MapBuilder.enableFeature Pres FeatureFlags.ManualZwj 1u
        |> MapBuilder.enableFeature Abvs FeatureFlags.ManualZwj 1u
        |> MapBuilder.enableFeature Blws FeatureFlags.ManualZwj 1u
        |> MapBuilder.enableFeature Psts FeatureFlags.ManualZwj 1u

    /// <summary>Saves the category of every glyph, <c>setup_masks_myanmar</c>: no mask is set here, because the syllables
    /// are not known yet, so a pause reorders them later.</summary>
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

    /// <summary>The rewriter that marks every syllable unsafe to break, <c>setup_syllables_myanmar</c>'s loop.</summary>
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

    /// <summary>Finds the syllables of the run, <c>setup_syllables_myanmar</c>: the machine paints each glyph with the
    /// syllable it belongs to, and every syllable is marked unsafe to break. This pause changes no glyph.</summary>
    /// <param name="run">The run, with its categories set.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllables (run: GlyphRun) =
        let categories = Sealed.map (fun (props: GlyphProps) -> props.ShaperCategory) (run.Props.AsSpan())
        let previous = Sealed.map (fun (props: GlyphProps) -> props.Syllable) (run.Props.AsSpan())
        let struct (syllables, broken) = Ragel.findSyllables MyanmarMachine.machine categories Unchecked.defaultof<ImmutableArray<int>> previous
        let painted = Sealed.mapi (fun index (props: GlyphProps) -> { props with Syllable = syllables[index] }) (run.Props.AsSpan())
        let struct (infos, props) = Sealed.rewriteSegments (UnsafeSyllables(run.ClusterLevel)) (run.Infos.AsSpan()) (painted.AsSpan())
        let scratch =
            match broken with
            | true -> run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable
            | false -> run.ScratchFlags
        { run with Infos = infos; Props = props; ScratchFlags = scratch }

    /// <summary>Finds the syllables of the run in place, <c>setup_syllables_myanmar</c> on the working set: the machine paints the syllable of each
    /// glyph into the properties and every syllable is marked unsafe to break in the glyphs, where <see cref="setupSyllables"/>
    /// publishes copies. The result is the run of <see cref="setupSyllables"/>.</summary>
    /// <param name="run">The run, with its categories set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the syllables and the broken-syllable flag set.</returns>
    let setupSyllablesOwned (run: WorkingSet) =
        let broken = SyllableOwned.paintFlatBreaking Ragel.myanmar run.ClusterLevel run.Infos run.Props
        match broken with
        | true -> { run with ScratchFlags = run.ScratchFlags ||| ScratchFlags.HasBrokenSyllable }
        | false -> run

    /// <summary>Inserts a dotted circle before each broken syllable, <c>hb_syllabic_insert_dotted_circles</c> as
    /// <c>reorder_myanmar</c> calls it.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCircles font run =
        let struct (inserted, _) = SyllableEdit.insertDottedCircles font BrokenCluster MyanmarMachine.DOTTEDCIRCLE ValueNone ValueNone run
        inserted

    /// <summary>Inserts a dotted circle before each broken syllable in arrays of the right size, <see cref="insertDottedCircles"/> for a
    /// run whose arrays were made in this call and are shared with no one.</summary>
    /// <param name="font">The font, for the dotted-circle glyph.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The run with the dotted circles inserted.</returns>
    let insertDottedCirclesOwned font run =
        let struct (inserted, _) = SyllableEdit.insertDottedCirclesOwned font BrokenCluster MyanmarMachine.DOTTEDCIRCLE ValueNone ValueNone run
        inserted

    /// <summary>The flag of a category, <c>FLAG_UNSAFE</c>: zero for a value at or above 32, whose shift would wrap.</summary>
    /// <param name="value">The category byte.</param>
    /// <returns>The bit for the category.</returns>
    let private flagUnsafe (value: byte) =
        match value < 32uy with
        | true -> 1u <<< int value
        | false -> 0u

    /// <summary>The category flags of the consonants, <c>CONSONANT_FLAGS_MYANMAR</c>: the consonant, consonant sign, ra,
    /// independent vowel, generic base and dotted circle.</summary>
    let private consonantFlags =
        (1u <<< int MyanmarMachine.C) ||| (1u <<< int MyanmarMachine.CS) ||| (1u <<< int MyanmarMachine.Ra)
        ||| (1u <<< int MyanmarMachine.IV) ||| (1u <<< int MyanmarMachine.GB) ||| (1u <<< int MyanmarMachine.DOTTEDCIRCLE)

    /// <summary>Whether a glyph is a consonant, <c>is_consonant_myanmar</c>: the category is one of the consonant flags and
    /// the glyph was not ligated.</summary>
    /// <param name="row">The glyph.</param>
    /// <returns>True for a consonant.</returns>
    let private isConsonant (row: GlyphRow) =
        not (GlyphPropsBits.ligated row.Props) && (flagUnsafe row.Props.ShaperCategory &&& consonantFlags) <> 0u

    /// <summary>Finds the first consonant at or after an index.</summary>
    /// <param name="rows">The rows of the syllable.</param>
    /// <param name="index">The next candidate.</param>
    /// <returns>The consonant's index, or none.</returns>
    [<TailCall>]
    let rec private firstConsonantFrom (rows: ImmutableArray<GlyphRow>) (index: int) =
        match index < rows.Length with
        | false -> ValueNone
        | true ->
            match isConsonant rows[index] with
            | true -> ValueSome index
            | false -> firstConsonantFrom rows (index + 1)

    /// <summary>The position one glyph of the loop after the base takes, <c>initial_reordering_consonant_syllable</c>'s
    /// trailing loop: a medial ra is pre-base, a pre-base vowel is a left matra, a variation selector takes the previous
    /// glyph's position, and the below-base and above marks follow the main position.</summary>
    /// <param name="category">The glyph's category.</param>
    /// <param name="pos">The running position.</param>
    /// <param name="previous">The position of the previous glyph.</param>
    /// <returns>The glyph's position, the next running position and the next previous.</returns>
    let private trailStep (category: byte) (pos: byte) (previous: byte) =
        match category with
        | _ when category = MyanmarMachine.MR -> struct (IndicPosition.PosPreC, pos, IndicPosition.PosPreC)
        | _ when category = MyanmarMachine.VPre -> struct (IndicPosition.PosPreM, pos, IndicPosition.PosPreM)
        | _ when category = MyanmarMachine.VS -> struct (previous, pos, previous)
        | _ when pos = IndicPosition.PosAfterMain && category = MyanmarMachine.VBlw -> struct (IndicPosition.PosBelowC, IndicPosition.PosBelowC, IndicPosition.PosBelowC)
        | _ when pos = IndicPosition.PosBelowC && category = MyanmarMachine.A -> struct (IndicPosition.PosBeforeSub, pos, IndicPosition.PosBeforeSub)
        | _ when pos = IndicPosition.PosBelowC && category = MyanmarMachine.VBlw -> struct (pos, pos, pos)
        | _ when pos = IndicPosition.PosBelowC -> struct (IndicPosition.PosAfterSub, IndicPosition.PosAfterSub, IndicPosition.PosAfterSub)
        | _ -> struct (pos, pos, pos)

    /// <summary>The running state of the position assignment.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private AssignState =
        { Index: int
          Pos: byte
          Previous: byte }

    /// <summary>Assigns every glyph of a syllable its visual position, <c>initial_reordering_consonant_syllable</c>'s first
    /// half: the reph prefix, the pre-base glyphs, the base and the trailing loop.</summary>
    [<Struct>]
    type private AssignPositions =
        val RephCount: int
        val BaseGlyph: int

        /// <summary>Creates the assigner.</summary>
        /// <param name="rephCount">The number of glyphs of the reph prefix.</param>
        /// <param name="baseGlyph">The index of the base glyph.</param>
        new (rephCount, baseGlyph) = { RephCount = rephCount; BaseGlyph = baseGlyph }

        interface ISpanUnfolder<GlyphRow, AssignState, byte> with
            member this.Next (rows, state) =
                let index = state.Index
                let struct (position, pos, previous) =
                    match index < this.RephCount with
                    | true -> struct (IndicPosition.PosAfterMain, state.Pos, IndicPosition.PosAfterMain)
                    | false ->
                        match index < this.BaseGlyph with
                        | true -> struct (IndicPosition.PosPreC, state.Pos, IndicPosition.PosPreC)
                        | false ->
                            match index = this.BaseGlyph with
                            | true -> struct (IndicPosition.PosBaseC, state.Pos, IndicPosition.PosBaseC)
                            | false -> trailStep rows[index].Props.ShaperCategory state.Pos state.Previous
                struct (position, { Index = index + 1; Pos = pos; Previous = previous })

    /// <summary>Finds the first and last glyph of the left-matra run, whose position is <c>POS_PRE_M</c>.</summary>
    /// <param name="rows">The sorted rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="first">The first left matra so far, or the length when none.</param>
    /// <param name="last">The last left matra so far, or the length when none.</param>
    /// <returns>The first and last left-matra indices.</returns>
    [<TailCall>]
    let rec private leftMatraBounds (rows: ImmutableArray<GlyphRow>) (index: int) (first: int) (last: int) =
        match index < rows.Length with
        | false -> struct (first, last)
        | true ->
            match rows[index].Props.ShaperAuxiliary = IndicPosition.PosPreM with
            | false -> leftMatraBounds rows (index + 1) first last
            | true ->
                let first = match first = rows.Length with | true -> index | false -> first
                leftMatraBounds rows (index + 1) first index

    /// <summary>Reverses each pre-base vowel and its variation selectors back, after the whole left-matra run was reversed,
    /// <c>initial_reordering_consonant_syllable</c>'s left-matra flip.</summary>
    /// <param name="rows">The rows, the left-matra run reversed.</param>
    /// <param name="last">The last index of the left-matra run.</param>
    /// <param name="i">The start of the next vowel group.</param>
    /// <param name="j">The next glyph to look at.</param>
    /// <returns>The rows with the vowel groups flipped back.</returns>
    [<TailCall>]
    let rec private flipVPre (rows: ImmutableArray<GlyphRow>) (last: int) (i: int) (j: int) =
        match j <= last with
        | false -> rows
        | true ->
            match rows[j].Props.ShaperCategory = MyanmarMachine.VPre with
            | true -> flipVPre (Sealed.reverseRange (rows.AsSpan()) i (j + 1)) last (j + 1) (j + 1)
            | false -> flipVPre rows last i (j + 1)

    /// <summary>Assigns the positions of a consonant or broken syllable, sorts by them and flips the left matras,
    /// <c>initial_reordering_consonant_syllable</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The reordered syllable.</returns>
    let private reorderConsonantSyllable level (state: SyllableState) =
        let rows = state.Rows
        let reph =
            rows.Length >= 3
            && rows[0].Props.ShaperCategory = MyanmarMachine.Ra
            && rows[1].Props.ShaperCategory = MyanmarMachine.As
            && rows[2].Props.ShaperCategory = MyanmarMachine.H
        let rephCount = match reph with | true -> 3 | false -> 0
        let baseIndex =
            match firstConsonantFrom rows rephCount with
            | ValueSome index -> index
            | ValueNone -> 0
        let baseGlyph = max rephCount baseIndex
        let seed = { Index = 0; Pos = IndicPosition.PosAfterMain; Previous = IndicPosition.PosAfterMain }
        let positions = Sealed.unfoldSpan (rows.AsSpan()) rows.Length seed (AssignPositions(rephCount, baseGlyph))
        let positioned = Sealed.mapi (fun index (row: GlyphRow) -> { row with Props = { row.Props with ShaperAuxiliary = positions[index] } }) (rows.AsSpan())
        let sorted = SyllableEdit.sortMerging level (fun (row: GlyphRow) -> int row.Props.ShaperAuxiliary) 0 rows.Length { state with Rows = positioned }
        let struct (first, last) = leftMatraBounds sorted.Rows 0 sorted.Rows.Length sorted.Rows.Length
        match first < last with
        | false -> sorted
        | true ->
            let flipped = Sealed.reverseRange (sorted.Rows.AsSpan()) first (last + 1)
            { sorted with Rows = flipVPre flipped last first first }

    /// <summary>The rewriter that reorders every syllable, <c>reorder_syllable_myanmar</c>.</summary>
    [<Struct>]
    type private ReorderSyllables =
        val Level: ClusterLevel

        /// <summary>Creates the rewriter.</summary>
        /// <param name="level">The cluster level of the run.</param>
        new (level) = { Level = level }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                match MyanmarMachine.ofSyllable rows[0].Props.Syllable with
                | MyanmarMachine.MyanmarNonMyanmarCluster -> SegmentRewrite.unchanged
                | MyanmarMachine.MyanmarBrokenCluster | MyanmarMachine.MyanmarConsonantSyllable ->
                    let initial : SyllableState =
                        { Rows = Sealed.ofSpan rows
                          Left = ImmutableArray.Empty
                          Right = ImmutableArray.Empty }
                    let final = reorderConsonantSyllable this.Level initial
                    let rewrite : SegmentRewrite =
                        { Rows = final.Rows
                          Left = final.Left
                          Right = final.Right }
                    rewrite

    /// <summary>Reorders the syllables of the run, <c>reorder_myanmar</c>: broken clusters get a dotted circle, then each
    /// syllable has its consonants, pre-base vowels and below-base marks moved into visual order.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The reordered run.</returns>
    let reorder font (run: GlyphRun) =
        let circled = insertDottedCircles font run
        let struct (infos, props) = Sealed.rewriteSegments (ReorderSyllables(circled.ClusterLevel)) (circled.Infos.AsSpan()) (circled.Props.AsSpan())
        { circled with Infos = infos; Props = props }

    /// <summary>Whether a glyph is a consonant, <c>is_consonant_myanmar</c>, from its properties.</summary>
    /// <param name="props">The properties of the glyph.</param>
    /// <returns>True for a consonant.</returns>
    let private isConsonantProps (props: GlyphProps) =
        not (GlyphPropsBits.ligated props) && (flagUnsafe props.ShaperCategory &&& consonantFlags) <> 0u

    /// <summary>Finds the first consonant of a syllable at or after an index.</summary>
    /// <param name="props">The properties of the run.</param>
    /// <param name="index">The next candidate.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <returns>The consonant's index, or none.</returns>
    [<TailCall>]
    let rec private firstConsonantOwned (props: ImmutableArray<GlyphProps>) (index: int) (finish: int) =
        match index < finish with
        | false -> ValueNone
        | true ->
            match isConsonantProps props[index] with
            | true -> ValueSome index
            | false -> firstConsonantOwned props (index + 1) finish

    /// <summary>The position of one glyph of a syllable, <see cref="AssignPositions"/> for one index.</summary>
    /// <param name="category">The category of the glyph.</param>
    /// <param name="index">The index in the syllable.</param>
    /// <param name="rephCount">The number of glyphs of the reph prefix.</param>
    /// <param name="baseGlyph">The index of the base glyph in the syllable.</param>
    /// <param name="state">The running position and the position of the previous glyph.</param>
    /// <returns>The glyph's position, the next running position and the next previous.</returns>
    let private positionOfGlyph (category: byte) (index: int) (rephCount: int) (baseGlyph: int) (state: AssignState) =
        match index < rephCount, index < baseGlyph, index = baseGlyph with
        | true, _, _ -> struct (IndicPosition.PosAfterMain, state.Pos, IndicPosition.PosAfterMain)
        | false, true, _ -> struct (IndicPosition.PosPreC, state.Pos, IndicPosition.PosPreC)
        | false, false, true -> struct (IndicPosition.PosBaseC, state.Pos, IndicPosition.PosBaseC)
        | false, false, false -> trailStep category state.Pos state.Previous

    /// <summary>Assigns every glyph of a syllable its visual position in place, <c>initial_reordering_consonant_syllable</c>'s first
    /// half.</summary>
    /// <param name="props">The properties of the run, made in this call and shared with no one.</param>
    /// <param name="start">The first glyph of the syllable.</param>
    /// <param name="rephCount">The number of glyphs of the reph prefix.</param>
    /// <param name="baseGlyph">The index of the base glyph in the syllable.</param>
    /// <param name="index">The next glyph in the syllable.</param>
    /// <param name="length">The length of the syllable.</param>
    /// <param name="state">The running position and the position of the previous glyph.</param>
    [<TailCall>]
    let rec private assignPositionsOwned (props: ImmutableArray<GlyphProps>) (start: int) (rephCount: int) (baseGlyph: int) (index: int) (length: int) (state: AssignState) =
        match index < length with
        | false -> ()
        | true ->
            let struct (position, pos, previous) = positionOfGlyph props[start + index].ShaperCategory index rephCount baseGlyph state
            SyllableOwned.setProp props (start + index) { props[start + index] with ShaperAuxiliary = position }
            assignPositionsOwned props start rephCount baseGlyph (index + 1) length { Index = index + 1; Pos = pos; Previous = previous }

    /// <summary>Finds the first and last glyph of the left-matra run of a sorted syllable.</summary>
    /// <param name="props">The properties of the run.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="finish">The end of the syllable.</param>
    /// <param name="first">The first left matra so far, or the end when none.</param>
    /// <param name="last">The last left matra so far, or the end when none.</param>
    /// <returns>The first and last left-matra indices.</returns>
    [<TailCall>]
    let rec private leftMatraBoundsOwned (props: ImmutableArray<GlyphProps>) (index: int) (finish: int) (first: int) (last: int) =
        match index < finish with
        | false -> struct (first, last)
        | true ->
            match props[index].ShaperAuxiliary = IndicPosition.PosPreM with
            | false -> leftMatraBoundsOwned props (index + 1) finish first last
            | true -> leftMatraBoundsOwned props (index + 1) finish (match first = finish with | true -> index | false -> first) index

    /// <summary>Reverses each pre-base vowel and its variation selectors back, in place, after the whole left-matra run was reversed.</summary>
    /// <param name="infos">The glyphs, made in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="last">The last index of the left-matra run.</param>
    /// <param name="i">The start of the next vowel group.</param>
    /// <param name="j">The next glyph to look at.</param>
    [<TailCall>]
    let rec private flipVPreOwned (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (last: int) (i: int) (j: int) =
        match j <= last with
        | false -> ()
        | true ->
            match props[j].ShaperCategory = MyanmarMachine.VPre with
            | true ->
                SyllableOwned.reverse infos props i (j + 1)
                flipVPreOwned infos props last (j + 1) (j + 1)
            | false -> flipVPreOwned infos props last i (j + 1)

    /// <summary>Assigns the positions of a consonant or broken syllable, sorts by them and flips the left matras in place,
    /// <c>initial_reordering_consonant_syllable</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="infos">The glyphs, made in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="start">The first glyph of the syllable.</param>
    /// <param name="finish">The end of the syllable.</param>
    let private reorderConsonantOwned level (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) =
        let length = finish - start
        let reph =
            length >= 3
            && props[start].ShaperCategory = MyanmarMachine.Ra
            && props[start + 1].ShaperCategory = MyanmarMachine.As
            && props[start + 2].ShaperCategory = MyanmarMachine.H
        let rephCount = match reph with | true -> 3 | false -> 0
        let baseIndex =
            match firstConsonantOwned props (start + rephCount) finish with
            | ValueSome index -> index - start
            | ValueNone -> 0
        let seed = { Index = 0; Pos = IndicPosition.PosAfterMain; Previous = IndicPosition.PosAfterMain }
        assignPositionsOwned props start rephCount (max rephCount baseIndex) 0 length seed
        SyllableOwned.sortMerging level infos props start finish
        let struct (first, last) = leftMatraBoundsOwned props start finish finish finish
        match first < last with
        | false -> ()
        | true ->
            SyllableOwned.reverse infos props first (last + 1)
            flipVPreOwned infos props last first first

    /// <summary>Reorders the syllable chunks of a run from an index in place.</summary>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <param name="start">The first glyph of the next chunk.</param>
    [<TailCall>]
    let rec private reorderChunksOwned (run: WorkingSet) (start: int) =
        match start < run.Props.Length with
        | false -> ()
        | true ->
            let finish = Sealed.nextSyllableEnd (run.Props.AsSpan()) start
            match MyanmarMachine.ofSyllable run.Props[start].Syllable with
            | MyanmarMachine.MyanmarNonMyanmarCluster -> ()
            | MyanmarMachine.MyanmarBrokenCluster | MyanmarMachine.MyanmarConsonantSyllable ->
                reorderConsonantOwned run.ClusterLevel run.Infos run.Props start finish
            reorderChunksOwned run finish

    /// <summary>Reorders the syllables of the run in place, <c>reorder_myanmar</c> on the working set: the result of <see cref="reorder"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The reordered run.</returns>
    let reorderOwned font (run: WorkingSet) =
        let circled = insertDottedCirclesOwned font run
        reorderChunksOwned circled 0
        circled
