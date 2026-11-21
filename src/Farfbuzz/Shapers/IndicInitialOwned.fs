namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Indic shaper's initial reordering on the working set, <c>initial_reordering_indic</c>
/// (src/hb-ot-shaper-indic.cc:421-996) as upstream does it: each syllable is an index window of the run's own arrays, its glyphs are
/// moved, sorted and given their masks in place, and the clusters are merged in place. The decisions are those of
/// <see cref="IndicInitial"/>, which stays the reference that publishes a copy for every edit; the result of <see cref="reorder"/> is
/// the run of <see cref="IndicInitial.reorder"/> in every field.</summary>
[<RequireQualifiedAccess>]
module internal IndicInitialOwned =
    /// <summary>The longest syllable whose glyph indices fit the one-byte syllable field, <c>end - start > 127</c>
    /// (src/hb-ot-shaper-indic.cc:826).</summary>
    [<Literal>]
    let private MostIndicesForCycles = 127

    /// <summary>The value that marks a glyph whose cycle of the sort permutation has been merged, <c>255</c>.</summary>
    [<Literal>]
    let private Visited = 255uy

    /// <summary>What the reordering of a syllable reads besides the syllable: the plan data, the script, the cluster level and
    /// the face the <c>would_substitute</c> tests ask.</summary>
    [<Struct>]
    type private Environment =
        { Plan: IndicPlan
          Script: Script
          Level: ClusterLevel
          Face: OtFace }

    /// <summary>One syllable of the run: its glyphs are the arrays from <c>Start</c> for <c>Length</c> glyphs, which the reordering
    /// reads and writes in place.</summary>
    [<Struct>]
    type private Window =
        { Infos: ImmutableArray<GlyphInfo>
          Props: ImmutableArray<GlyphProps>
          Start: int
          Length: int }

    /// <summary>The properties of a glyph of a syllable.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The index in the syllable.</param>
    /// <returns>The properties.</returns>
    let private propsAt (window: Window) index = window.Props[window.Start + index]

    /// <summary>Gives a glyph of a syllable a position.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The index in the syllable.</param>
    /// <param name="position">The position.</param>
    let private setPosition (window: Window) index position =
        let props = window.Props[window.Start + index]
        match props.ShaperAuxiliary = position with
        | true -> ()
        | false -> SyllableOwned.setAuxiliary window.Props (window.Start + index) position

    /// <summary>Gives the glyphs of a range of a syllable the bits of a mask.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="mask">The bits.</param>
    /// <param name="index">The first index in the syllable.</param>
    /// <param name="finish">The end index in the syllable.</param>
    [<TailCall>]
    let rec private addMask (window: Window) mask index finish =
        match index < finish with
        | false -> ()
        | true ->
            SyllableOwned.orMask window.Infos (window.Start + index) mask
            addMask window mask (index + 1) finish

    /// <summary>Updates the positions of the glyphs that could be the base, <c>update_consonant_positions_indic</c>: the two orders of the
    /// virama and the consonant that the font is asked about are written into two arrays made once for the run.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="face">The face.</param>
    /// <param name="virama">The virama glyph.</param>
    /// <param name="newOrder">An array of two for the virama and the consonant.</param>
    /// <param name="oldOrder">An array of two for the consonant and the virama.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties.</param>
    /// <param name="index">The next glyph.</param>
    [<TailCall>]
    let rec private updatePositions (plan: IndicPlan) face virama (newOrder: ImmutableArray<uint32>) (oldOrder: ImmutableArray<uint32>) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) index =
        match index < props.Length with
        | false -> ()
        | true ->
            match props[index].ShaperAuxiliary = IndicPosition.PosBaseC with
            | true ->
                SyllableOwned.setPair newOrder virama infos[index].Codepoint
                SyllableOwned.setPair oldOrder infos[index].Codepoint virama
                SyllableOwned.setProp props index { props[index] with ShaperAuxiliary = IndicInitial.consonantPositionOfPairs plan face newOrder oldOrder }
            | false -> ()
            updatePositions plan face virama newOrder oldOrder infos props (index + 1)

    /// <summary>Updates the positions of the consonants that could be the base, in place, <see cref="IndicInitial.updateConsonantPositions"/>
    /// on the working set.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The run with the positions updated.</returns>
    let updateConsonantPositions (plan: IndicPlan) (font: Font) (run: WorkingSet) =
        match Indic.viramaGlyph plan font with
        | ValueNone -> run
        | ValueSome virama ->
            updatePositions plan font.Ot virama (ImmutableArray.Create(0u, 0u)) (ImmutableArray.Create(0u, 0u)) run.Infos run.Props 0
            run

    /// <summary>Makes a Ra, halant and ZWJ at the start of a Kannada syllable the Ra, ZWJ and halant the shaper expects
    /// (src/hb-ot-shaper-indic.cc:463-474).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    let private kannadaRaHalantJoiner (environment: Environment) (window: Window) =
        match environment.Script = Scripts.Kannada
              && window.Length >= 3
              && Indic.isOneOf (Indic.flagUnsafe IndicMachine.Ra) (propsAt window 0)
              && Indic.isOneOf (Indic.flagUnsafe IndicMachine.H) (propsAt window 1)
              && Indic.isOneOf (Indic.flagUnsafe IndicMachine.ZWJ) (propsAt window 2) with
        | false -> ()
        | true ->
            SyllableOwned.mergeClusters environment.Level window.Infos (window.Start + 1) (window.Start + 3)
            SyllableOwned.move window.Infos window.Props (window.Start + 2) (window.Start + 1)

    /// <summary>Skips the joiners of a syllable from an index.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The first index.</param>
    /// <returns>The index of the first glyph that is not a joiner, the end of the syllable when none.</returns>
    [<TailCall>]
    let rec private skipJoiners (window: Window) index =
        match index < window.Length && Indic.isJoiner (propsAt window index) with
        | true -> skipJoiners window (index + 1)
        | false -> index

    /// <summary>Whether a syllable starts with the glyphs of a reph the font would form (src/hb-ot-shaper-indic.cc:512-530).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="window">The syllable.</param>
    /// <returns>True when the syllable may have a reph of the first kind.</returns>
    let private mayHaveReph (plan: IndicPlan) (window: Window) =
        match plan.RphfMask <> 0u && window.Length >= 3 with
        | false -> false
        | true ->
            match plan.Config.RephMode with
            | RephImplicit -> not (Indic.isJoiner (propsAt window 2))
            | RephExplicit -> (propsAt window 2).ShaperCategory = IndicMachine.ZWJ
            | RephLogRepha -> false

    /// <summary>Whether the font's <c>rphf</c> lookups would form a reph from the first glyphs (src/hb-ot-shaper-indic.cc:524-531).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    /// <returns>True when the lookups would substitute the Ra and the halant, or for an explicit reph the Ra, the halant and the ZWJ.</returns>
    let private rephForms (environment: Environment) (window: Window) =
        let plan = environment.Plan
        let explicit = plan.Config.RephMode = RephExplicit
        let first = window.Infos[window.Start].Codepoint
        let second = window.Infos[window.Start + 1].Codepoint
        match IndicInitial.anyWouldSubstitute environment.Face plan.RphfFeature (ImmutableArray.Create(first, second)) 0 with
        | true -> true
        | false ->
            match explicit with
            | false -> false
            | true -> IndicInitial.anyWouldSubstitute environment.Face plan.RphfFeature (ImmutableArray.Create(first, second, window.Infos[window.Start + 2].Codepoint)) 0

    /// <summary>Finds the reph at the start of a syllable (src/hb-ot-shaper-indic.cc:512-543).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    /// <returns>The limit the base search stops at, the base found so far (the end when none) and whether there is a reph.</returns>
    let private detectReph (environment: Environment) (window: Window) =
        match mayHaveReph environment.Plan window with
        | true ->
            match rephForms environment window with
            | true -> struct (skipJoiners window 2, 0, true)
            | false -> struct (0, window.Length, false)
        | false ->
            match environment.Plan.Config.RephMode = RephLogRepha && (propsAt window 0).ShaperCategory = IndicMachine.Repha with
            | true -> struct (skipJoiners window 1, 0, true)
            | false -> struct (0, window.Length, false)

    /// <summary>Searches for the base from the end of the syllable, the do-while loop of src/hb-ot-shaper-indic.cc:545-585.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="limit">The index the search stops at.</param>
    /// <param name="index">The glyph to examine.</param>
    /// <param name="seenBelow">Whether a below-base consonant has been passed.</param>
    /// <param name="found">The base found so far.</param>
    /// <returns>The base.</returns>
    [<TailCall>]
    let rec private searchBase (window: Window) limit index seenBelow found =
        let props = propsAt window index
        let position = props.ShaperAuxiliary
        let consonant = Indic.isConsonant props
        let isBase = consonant && position <> IndicPosition.PosBelowC && (position <> IndicPosition.PosPostC || seenBelow)
        let stopsAtJoiner =
            not consonant && index > 0 && props.ShaperCategory = IndicMachine.ZWJ && (propsAt window (index - 1)).ShaperCategory = IndicMachine.H
        let candidate = match consonant with | true -> index | false -> found
        match isBase with
        | true -> index
        | false ->
            match stopsAtJoiner || index <= limit with
            | true -> candidate
            | false -> searchBase window limit (index - 1) (seenBelow || (consonant && position = IndicPosition.PosBelowC)) candidate

    /// <summary>Finds the base consonant of a syllable and whether it has a reph, the first step of
    /// <c>initial_reordering_consonant_syllable</c> (src/hb-ot-shaper-indic.cc:477-598).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    /// <returns>The base, the end of the syllable when there is none, and whether the syllable has a reph that stays one.</returns>
    let private findBase (environment: Environment) (window: Window) =
        let struct (limit, initial, hasReph) = detectReph environment window
        let found = searchBase window limit (window.Length - 1) false initial
        match hasReph && found = 0 && limit - found <= 2 with
        | true -> struct (found, false)
        | false -> struct (found, hasReph)

    /// <summary>Gives each glyph before the base at most the position before the consonants, the base its position, and a reph its
    /// own, the "reorder characters" step (src/hb-ot-shaper-indic.cc:631-639).</summary>
    /// <param name="found">The base.</param>
    /// <param name="hasReph">Whether the syllable has a reph.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    [<TailCall>]
    let rec private assignPositions found hasReph (window: Window) index =
        match index < window.Length with
        | false -> ()
        | true ->
            let current = (propsAt window index).ShaperAuxiliary
            let before = match index < found with | true -> min IndicPosition.PosPreC current | false -> current
            let atBase = match index = found with | true -> IndicPosition.PosBaseC | false -> before
            match hasReph && index = 0 with
            | true -> setPosition window index IndicPosition.PosRaToBecomeReph
            | false -> setPosition window index atBase
            assignPositions found hasReph window (index + 1)

    /// <summary>Finds the glyph a halant is moved after, the inner loop of the old-specification halant move
    /// (src/hb-ot-shaper-indic.cc:672-679).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="disallowDoubleHalants">Whether a halant stops the search too.</param>
    /// <param name="halant">The index of the halant.</param>
    /// <param name="index">The next glyph to examine, going down.</param>
    /// <returns>The index the search stopped at.</returns>
    [<TailCall>]
    let rec private halantTarget (window: Window) disallowDoubleHalants halant index =
        match index > halant with
        | false -> index
        | true ->
            let props = propsAt window index
            match Indic.isConsonant props || (disallowDoubleHalants && props.ShaperCategory = IndicMachine.H) with
            | true -> index
            | false -> halantTarget window disallowDoubleHalants halant (index - 1)

    /// <summary>Finds the first halant from an index by its raw category.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The first index.</param>
    /// <returns>The index of the halant, or the end of the syllable.</returns>
    [<TailCall>]
    let rec private firstHalant (window: Window) index =
        match index < window.Length && (propsAt window index).ShaperCategory <> IndicMachine.H with
        | true -> firstHalant window (index + 1)
        | false -> index

    /// <summary>For the old-specification script tags, moves the first post-base halant after the last consonant, except for
    /// Kannada when a halant already follows it (src/hb-ot-shaper-indic.cc:661-689).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    let private moveOldSpecHalant (environment: Environment) found (window: Window) =
        match environment.Plan.IsOldSpec with
        | false -> ()
        | true ->
            let halant = firstHalant window (found + 1)
            match halant < window.Length with
            | false -> ()
            | true ->
                let target = halantTarget window (environment.Script = Scripts.Kannada) halant (window.Length - 1)
                match (propsAt window target).ShaperCategory <> IndicMachine.H && target > halant with
                | true -> SyllableOwned.move window.Infos window.Props (window.Start + halant) (window.Start + target)
                | false -> ()

    /// <summary>The categories whose glyphs take the position of the glyph before them, the joiners, the nukta, the register
    /// shifter, the consonant medial and the halant.</summary>
    let private attachedFlags =
        Indic.joinerFlags ||| Indic.flagUnsafe IndicMachine.N ||| Indic.flagUnsafe IndicMachine.RS ||| Indic.flagUnsafe IndicMachine.CM
        ||| Indic.flagUnsafe IndicMachine.H

    /// <summary>Finds the position a halant after a left matra takes (src/hb-ot-shaper-indic.cc:699-709).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The index of the halant, then the glyphs before it.</param>
    /// <returns>The position, that of a left matra when every glyph before is one.</returns>
    [<TailCall>]
    let rec private positionBeforeMatras (window: Window) index =
        match index > 0 with
        | false -> IndicPosition.PosPreM
        | true ->
            match (propsAt window (index - 1)).ShaperAuxiliary <> IndicPosition.PosPreM with
            | true -> (propsAt window (index - 1)).ShaperAuxiliary
            | false -> positionBeforeMatras window (index - 1)

    /// <summary>The position an attached glyph takes (src/hb-ot-shaper-indic.cc:694-711).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The index of the glyph.</param>
    /// <param name="lastPosition">The last position seen.</param>
    /// <returns>The position.</returns>
    let private attachedPosition (window: Window) index lastPosition =
        match (propsAt window index).ShaperCategory = IndicMachine.H && lastPosition = IndicPosition.PosPreM with
        | true -> positionBeforeMatras window index
        | false -> lastPosition

    /// <summary>Gives a syllable-modifier mark the position of the post-base matra that follows it (src/hb-ot-shaper-indic.cc:712-717).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The index of the matra.</param>
    /// <param name="props">The properties of the matra.</param>
    let private attachModifier (window: Window) index (props: GlyphProps) =
        match props.ShaperCategory = IndicMachine.MPst && index > 0 && (propsAt window (index - 1)).ShaperCategory = IndicMachine.SM with
        | true -> setPosition window (index - 1) props.ShaperAuxiliary
        | false -> ()

    /// <summary>Attaches the miscellaneous marks to the previous glyph so that they move with it, the loop of
    /// src/hb-ot-shaper-indic.cc:691-719.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="lastPosition">The position of the last glyph that is not attached.</param>
    [<TailCall>]
    let rec private attachMarks (window: Window) index lastPosition =
        match index < window.Length with
        | false -> ()
        | true ->
            let props = propsAt window index
            match Indic.flagUnsafe props.ShaperCategory &&& attachedFlags <> 0u with
            | true ->
                setPosition window index (attachedPosition window index lastPosition)
                attachMarks window (index + 1) lastPosition
            | false ->
                match props.ShaperAuxiliary = IndicPosition.PosSmvd with
                | true -> attachMarks window (index + 1) lastPosition
                | false ->
                    attachModifier window index props
                    attachMarks window (index + 1) props.ShaperAuxiliary

    /// <summary>Gives the glyphs of a range that are before the syllable modifier signs a position.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="position">The position.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="finish">The end of the range.</param>
    [<TailCall>]
    let rec private ownRange (window: Window) position index finish =
        match index < finish with
        | false -> ()
        | true ->
            match (propsAt window index).ShaperAuxiliary < IndicPosition.PosSmvd with
            | true -> setPosition window index position
            | false -> ()
            ownRange window position (index + 1) finish

    /// <summary>Lets each post-base consonant own what lies between it and the last consonant or matra before it, the loop of
    /// src/hb-ot-shaper-indic.cc:721-733.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="last">The last consonant or matra.</param>
    [<TailCall>]
    let rec private ownPostBase (window: Window) index last =
        match index < window.Length with
        | false -> ()
        | true ->
            let props = propsAt window index
            match Indic.isConsonant props with
            | true ->
                ownRange window props.ShaperAuxiliary (last + 1) index
                ownPostBase window (index + 1) index
            | false ->
                match Indic.flagUnsafe props.ShaperCategory &&& (Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst) <> 0u with
                | true -> ownPostBase window (index + 1) index
                | false -> ownPostBase window (index + 1) last

    /// <summary>Finds the base again after the sort, and the first and last left matra before it (src/hb-ot-shaper-indic.cc:745-762).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="first">The first left matra, the end of the syllable when none.</param>
    /// <param name="last">The last left matra, the end of the syllable when none.</param>
    /// <returns>The base, the end of the syllable when none, and the first and last left matra.</returns>
    [<TailCall>]
    let rec private scanLeftMatras (window: Window) index first last : struct (int * int * int) =
        match index < window.Length with
        | false -> struct (window.Length, first, last)
        | true ->
            let position = (propsAt window index).ShaperAuxiliary
            match position = IndicPosition.PosBaseC with
            | true -> struct (index, first, last)
            | false ->
                match position = IndicPosition.PosPreM with
                | true -> scanLeftMatras window (index + 1) (match first = window.Length with | true -> index | false -> first) index
                | false -> scanLeftMatras window (index + 1) first last

    /// <summary>Reverses back the runs of a flipped sequence of left matras that end in a matra (src/hb-ot-shaper-indic.cc:769-776).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="runStart">The start of the run being reversed back.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="last">The last glyph of the sequence.</param>
    [<TailCall>]
    let rec private reverseMatraRuns (window: Window) runStart index last =
        match index > last with
        | true -> ()
        | false ->
            match Indic.flagUnsafe (propsAt window index).ShaperCategory &&& (Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst) <> 0u with
            | true ->
                SyllableOwned.reverse window.Infos window.Props (window.Start + runStart) (window.Start + index + 1)
                reverseMatraRuns window (index + 1) (index + 1) last
            | false -> reverseMatraRuns window runStart (index + 1) last

    /// <summary>Flips a sequence of left matras, which the stable sort left in the order of the text (src/hb-ot-shaper-indic.cc:764-777).</summary>
    /// <param name="first">The first left matra.</param>
    /// <param name="last">The last left matra.</param>
    /// <param name="window">The syllable.</param>
    let private flipLeftMatras first last (window: Window) =
        match first < last with
        | false -> ()
        | true ->
            SyllableOwned.reverse window.Infos window.Props (window.Start + first) (window.Start + last + 1)
            reverseMatraRuns window first first last

    /// <summary>Walks the cycles of the permutation the sort made, marking the glyphs it passes, the inner loop of
    /// src/hb-ot-shaper-indic.cc:831-839: the syllable byte holds the index the glyph had before the sort.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The glyph the walk started at.</param>
    /// <param name="current">The glyph reached.</param>
    /// <param name="low">The lowest index seen.</param>
    /// <param name="high">The highest index seen.</param>
    /// <returns>The lowest and highest index of the cycle.</returns>
    [<TailCall>]
    let rec private walkCycle (window: Window) index current low high : struct (int * int) =
        match current = index with
        | true -> struct (low, high)
        | false ->
            let props = propsAt window current
            SyllableOwned.setSyllable window.Props (window.Start + current) Visited
            walkCycle window index (int props.Syllable) (min low current) (max high current)

    /// <summary>Merges the clusters of each cycle of the permutation the sort made, from the base on, the loop of
    /// src/hb-ot-shaper-indic.cc:819-841.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    [<TailCall>]
    let rec private mergeCycles level found (window: Window) index =
        match index < window.Length with
        | false -> ()
        | true ->
            match (propsAt window index).Syllable = Visited with
            | true -> mergeCycles level found window (index + 1)
            | false ->
                let struct (low, high) = walkCycle window index (int (propsAt window index).Syllable) index index
                SyllableOwned.mergeClusters level window.Infos (window.Start + max found low) (window.Start + high + 1)
                mergeCycles level found window (index + 1)

    /// <summary>Merges the clusters the sort moved glyphs across: the whole of the syllable after the base for the old
    /// specification, and else the cycles of the permutation.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    let private mergeSorted (environment: Environment) found (window: Window) =
        match environment.Plan.IsOldSpec || window.Length > MostIndicesForCycles with
        | true -> SyllableOwned.mergeClusters environment.Level window.Infos (window.Start + found) (window.Start + window.Length)
        | false -> mergeCycles environment.Level found window found

    /// <summary>Whether the positions of the glyphs of a syllable never decrease, so that the stable sort would move nothing.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph, after the first.</param>
    /// <returns>True when the sort is the identity.</returns>
    [<TailCall>]
    let rec private positionsSorted (window: Window) index =
        match index < window.Length with
        | false -> true
        | true ->
            match (propsAt window (index - 1)).ShaperAuxiliary <= (propsAt window index).ShaperAuxiliary with
            | false -> false
            | true -> positionsSorted window (index + 1)

    /// <summary>Sorts the glyphs of a syllable by position, flips the left matras and merges the clusters, the sort step of
    /// <c>initial_reordering_consonant_syllable</c> (src/hb-ot-shaper-indic.cc:736-846).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable, positions set.</param>
    /// <returns>The base found again, the end of the syllable when none.</returns>
    let private sortAndMerge (environment: Environment) (window: Window) =
        let syllable = (propsAt window 0).Syllable
        let finish = window.Start + window.Length
        SyllableOwned.sortByAuxiliaryIndexing window.Infos window.Props window.Start finish
        let struct (found, first, last) = scanLeftMatras window 0 window.Length window.Length
        flipLeftMatras first last window
        mergeSorted environment found window
        SyllableOwned.fillSyllable window.Props window.Start finish syllable
        found

    /// <summary>Handles a syllable whose positions are already in order: the sort and the cycles of its permutation are the identity and
    /// merge nothing, so only the merge of the old specification and of a syllable too long for the cycles remains, unless two left matras
    /// are to be flipped.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable, positions set and in order.</param>
    /// <returns>The base, the end of the syllable when none.</returns>
    let private mergeInOrder (environment: Environment) (window: Window) =
        let struct (found, first, last) = scanLeftMatras window 0 window.Length window.Length
        match first < last with
        | true -> sortAndMerge environment window
        | false ->
            match environment.Plan.IsOldSpec || window.Length > MostIndicesForCycles with
            | true -> SyllableOwned.mergeClusters environment.Level window.Infos (window.Start + found) (window.Start + window.Length)
            | false -> ()
            found

    /// <summary>Sorts the glyphs of a syllable by position and merges the clusters, leaving a syllable that is in order as it is.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable, positions set.</param>
    /// <returns>The base found again, the end of the syllable when none.</returns>
    let private sortByPosition (environment: Environment) (window: Window) =
        match positionsSorted window 1 with
        | true -> mergeInOrder environment window
        | false -> sortAndMerge environment window

    /// <summary>Gives each glyph of a syllable the masks of the features applied after the reordering in one pass, "setup masks now"
    /// (src/hb-ot-shaper-indic.cc:848-864): the reph forms on the glyphs that lead with the reph position, the pre-base mask before the base,
    /// and the post-base mask after it.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="before">The mask of the glyphs before the base.</param>
    /// <param name="after">The mask of the glyphs after the base.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="leading">Whether every glyph so far has the reph position.</param>
    [<TailCall>]
    let rec private paintFeatureMasks (plan: IndicPlan) before after found (window: Window) index leading =
        match index < window.Length with
        | false -> ()
        | true ->
            let reph = leading && (propsAt window index).ShaperAuxiliary = IndicPosition.PosRaToBecomeReph
            let mask =
                (match reph with | true -> plan.RphfMask | false -> 0u)
                ||| (match index < found with | true -> before | false -> 0u)
                ||| (match index > found with | true -> after | false -> 0u)
            match mask with
            | 0u -> ()
            | bits -> SyllableOwned.orMask window.Infos (window.Start + index) bits
            paintFeatureMasks plan before after found window (index + 1) reph

    /// <summary>Sets the masks of the features applied after the reordering, "setup masks now"
    /// (src/hb-ot-shaper-indic.cc:848-864).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    let private setupFeatureMasks (plan: IndicPlan) found (window: Window) =
        paintFeatureMasks plan (IndicInitial.preBaseMask plan) (plan.BlwfMask ||| plan.AbvfMask ||| plan.PstfMask) found window 0 true

    /// <summary>Gives the Ra and halant before the base the below-base forms for the old-specification Devanagari, the eyelash Ra
    /// unless a ZWJ follows (src/hb-ot-shaper-indic.cc:866-896).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="found">The base.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="window">The syllable.</param>
    [<TailCall>]
    let rec private eyelashRa (plan: IndicPlan) found index (window: Window) =
        match index + 1 < found with
        | false -> ()
        | true ->
            let isEyelash =
                (propsAt window index).ShaperCategory = IndicMachine.Ra
                && (propsAt window (index + 1)).ShaperCategory = IndicMachine.H
                && (index + 2 = found || (propsAt window (index + 2)).ShaperCategory <> IndicMachine.ZWJ)
            match isEyelash with
            | true -> addMask window plan.BlwfMask index (index + 2)
            | false -> ()
            eyelashRa plan found (index + 1) window

    /// <summary>Finds a halant and a Ra the font's <c>pref</c> would substitute after the base, and gives them the mask of the
    /// feature (src/hb-ot-shaper-indic.cc:898-913).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="window">The syllable.</param>
    [<TailCall>]
    let rec private markPref (environment: Environment) index (window: Window) =
        match index + 1 < window.Length with
        | false -> ()
        | true ->
            let pair = ImmutableArray.Create(window.Infos[window.Start + index].Codepoint, window.Infos[window.Start + index + 1].Codepoint)
            match IndicInitial.anyWouldSubstitute environment.Face environment.Plan.PrefFeature pair 0 with
            | true -> addMask window environment.Plan.PrefMask index (index + 2)
            | false -> markPref environment (index + 1) window

    /// <summary>Clears the half-form mask from the glyphs a ZWNJ follows, back to the previous consonant (src/hb-ot-shaper-indic.cc:916-933).</summary>
    /// <param name="half">The mask of the half forms.</param>
    /// <param name="index">The glyph to clear.</param>
    /// <param name="window">The syllable.</param>
    [<TailCall>]
    let rec private clearHalfBack half index (window: Window) =
        SyllableOwned.clearMask window.Infos (window.Start + index) half
        match index > 0 && not (Indic.isConsonant (propsAt window index)) with
        | true -> clearHalfBack half (index - 1) window
        | false -> ()

    /// <summary>Applies the effects of the ZWJ and ZWNJ: a ZWNJ disables the half forms of what precedes it.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="window">The syllable.</param>
    [<TailCall>]
    let rec private joinerEffects (plan: IndicPlan) index (window: Window) =
        match index < window.Length with
        | false -> ()
        | true ->
            match Indic.isJoiner (propsAt window index) && (propsAt window index).ShaperCategory = IndicMachine.ZWNJ with
            | true -> clearHalfBack plan.HalfMask (index - 1) window
            | false -> ()
            joinerEffects plan (index + 1) window

    /// <summary>Finds the masks of a syllable once its glyphs are in place: the eyelash Ra of the old-specification Devanagari,
    /// the <c>pref</c> candidate, and the effects of the joiners.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    let private setupJoinerAndPrefMasks (environment: Environment) found (window: Window) =
        let plan = environment.Plan
        match plan.IsOldSpec && environment.Script = Scripts.Devanagari with
        | true -> eyelashRa plan found 0 window
        | false -> ()
        match plan.PrefMask <> 0u && found + 2 < window.Length with
        | true -> markPref environment (found + 1) window
        | false -> ()
        joinerEffects plan 1 window

    /// <summary>Reorders a syllable of one glyph, <c>initial_reordering_consonant_syllable</c> as it works out for it: the glyph is the base
    /// when it is a consonant or a repha that stands alone, else it is before the base and takes at most the position before the
    /// consonants; the marks step gives a glyph of an attached category the start position; the glyph is given the masks of the reph
    /// forms, when it still has the reph position, and of the pre-base features, when it is not the base. No sort, merge or joiner
    /// effect is possible for one glyph.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    let private reorderSingle (environment: Environment) (window: Window) =
        let plan = environment.Plan
        let props = propsAt window 0
        let atBase = Indic.isConsonant props || (plan.Config.RephMode = RephLogRepha && props.ShaperCategory = IndicMachine.Repha)
        let assigned = match atBase with | true -> IndicPosition.PosBaseC | false -> min IndicPosition.PosPreC props.ShaperAuxiliary
        let position = match Indic.flagUnsafe props.ShaperCategory &&& attachedFlags <> 0u with | true -> IndicPosition.PosStart | false -> assigned
        setPosition window 0 position
        let mask =
            (match position = IndicPosition.PosRaToBecomeReph with | true -> plan.RphfMask | false -> 0u)
            ||| (match position = IndicPosition.PosBaseC with | true -> 0u | false -> IndicInitial.preBaseMask plan)
        match mask with
        | 0u -> ()
        | bits -> SyllableOwned.orMask window.Infos window.Start bits

    /// <summary>Reorders a consonant syllable of two glyphs or more, <c>initial_reordering_consonant_syllable</c> (src/hb-ot-shaper-indic.cc:458-934).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    let private reorderMany (environment: Environment) (window: Window) =
        kannadaRaHalantJoiner environment window
        let struct (found, hasReph) = findBase environment window
        assignPositions found hasReph window 0
        moveOldSpecHalant environment found window
        attachMarks window 0 IndicPosition.PosStart
        ownPostBase window (found + 1) found
        let sortedBase = sortByPosition environment window
        setupFeatureMasks environment.Plan sortedBase window
        setupJoinerAndPrefMasks environment sortedBase window

    /// <summary>Reorders a consonant syllable, <c>initial_reordering_consonant_syllable</c> (src/hb-ot-shaper-indic.cc:458-934).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    let private reorderConsonantSyllable (environment: Environment) (window: Window) =
        match window.Length with
        | 1 -> reorderSingle environment window
        | _ -> reorderMany environment window

    /// <summary>Reorders the syllable chunks of a run from an index, <c>initial_reordering_syllable_indic</c>
    /// (src/hb-ot-shaper-indic.cc:948-971) for each.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <param name="start">The first glyph of the next chunk.</param>
    [<TailCall>]
    let rec private reorderChunks (environment: Environment) (run: WorkingSet) start =
        match start < run.Props.Length with
        | false -> ()
        | true ->
            let finish = Sealed.nextSyllableEnd (run.Props.AsSpan()) start
            match IndicMachine.ofSyllable run.Props[start].Syllable with
            | IndicMachine.IndicSymbolCluster | IndicMachine.IndicNonIndicCluster -> ()
            | IndicMachine.IndicConsonantSyllable | IndicMachine.IndicVowelSyllable
            | IndicMachine.IndicStandaloneCluster | IndicMachine.IndicBrokenCluster ->
                reorderConsonantSyllable environment { Infos = run.Infos; Props = run.Props; Start = start; Length = finish - start }
            reorderChunks environment run finish

    /// <summary>Reorders the syllables of the run in place, <c>initial_reordering_indic</c> (src/hb-ot-shaper-indic.cc:973-996): the
    /// consonant positions are updated from the font, broken clusters get a dotted circle, then each syllable is reordered.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The reordered run.</returns>
    let reorder (plan: IndicPlan) (font: Font) (run: WorkingSet) =
        let circled = Indic.insertDottedCirclesOwned font (updateConsonantPositions plan font run)
        let environment : Environment =
            { Plan = plan
              Script = circled.SegmentProperties.Script
              Level = circled.ClusterLevel
              Face = font.Ot }
        reorderChunks environment circled 0
        circled
