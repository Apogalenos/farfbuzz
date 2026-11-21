namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Indic shaper's final reordering on the working set, <c>final_reordering_indic</c>
/// (src/hb-ot-shaper-indic.cc:998-1461) as upstream does it: each syllable is an index window of the run's own arrays, the left
/// matras, the reph and the pre-base-reordering consonant are moved in place, and the clusters are merged in place. The decisions are
/// those of <see cref="IndicFinal"/>, which stays the reference that publishes a copy for every edit; the result of
/// <see cref="reorder"/> is the run of <see cref="IndicFinal.reorder"/> in every field.</summary>
[<RequireQualifiedAccess>]
module internal IndicFinalOwned =
    /// <summary>What the reordering of a syllable reads besides the syllable: the plan data, the script, the cluster level and
    /// the glyph of the virama.</summary>
    [<Struct>]
    type private Environment =
        { Plan: IndicPlan
          Script: Script
          Level: ClusterLevel
          Virama: uint32 voption }

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

    /// <summary>Whether the script is Malayalam or Tamil, which have no half forms or explicit virama forms.</summary>
    /// <param name="script">The script.</param>
    /// <returns>True for Malayalam and Tamil.</returns>
    let private hasNoHalfForms script = script = Scripts.Malayalam || script = Scripts.Tamil

    /// <summary>The categories a matra goes after: a matra, a post-base matra and a halant.</summary>
    let private matraOrHalantFlags = Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst ||| Indic.flagUnsafe IndicMachine.H

    /// <summary>Makes the halants the substitutions ligated and multiplied halants again, the recovery at the start of
    /// <c>final_reordering_syllable_indic</c> (src/hb-ot-shaper-indic.cc:1008-1026).</summary>
    /// <param name="virama">The virama glyph.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    [<TailCall>]
    let rec private recoverViramas virama (window: Window) index =
        match index < window.Length with
        | false -> ()
        | true ->
            let props = propsAt window index
            match window.Infos[window.Start + index].Codepoint = virama && GlyphPropsBits.ligated props && GlyphPropsBits.multiplied props with
            | true -> SyllableOwned.setProp window.Props (window.Start + index) (GlyphPropsBits.clearLigatedAndMultiplied { props with ShaperCategory = IndicMachine.H })
            | false -> ()
            recoverViramas virama window (index + 1)

    /// <summary>Finds the first glyph whose position is that of the base or after it.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The index, or the end of the syllable.</returns>
    [<TailCall>]
    let rec private firstFromBase (window: Window) index =
        match index < window.Length && (propsAt window index).ShaperAuxiliary < IndicPosition.PosBaseC with
        | true -> firstFromBase window (index + 1)
        | false -> index

    /// <summary>Finds the first glyph from an index that has some bit of a mask.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="mask">The mask.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The index, or the end of the syllable.</returns>
    [<TailCall>]
    let rec private firstWithMask (window: Window) mask index =
        match index < window.Length && window.Infos[window.Start + index].Mask &&& mask = 0u with
        | true -> firstWithMask window mask (index + 1)
        | false -> index

    /// <summary>Skips the halants from an index.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The first index.</param>
    /// <returns>The index of the first glyph that is not a halant.</returns>
    [<TailCall>]
    let rec private skipHalants (window: Window) index =
        match index < window.Length && Indic.isHalant (propsAt window index) with
        | true -> skipHalants window (index + 1)
        | false -> index

    /// <summary>Skips the joiners from an index.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The first index.</param>
    /// <returns>The index of the first glyph that is not a joiner.</returns>
    [<TailCall>]
    let rec private skipJoiners (window: Window) index =
        match index < window.Length && Indic.isJoiner (propsAt window index) with
        | true -> skipJoiners window (index + 1)
        | false -> index

    /// <summary>Takes as the base the glyph after the halants that follow a <c>pref</c> candidate that did not form its glyph, and
    /// gives it the position of the base (src/hb-ot-shaper-indic.cc:1043-1062).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="candidate">The candidate.</param>
    /// <returns>The base.</returns>
    let private baseAfterUnformedPref (window: Window) candidate =
        let skipped = skipHalants window candidate
        match skipped < window.Length with
        | true -> setPosition window skipped IndicPosition.PosBaseC
        | false -> ()
        skipped

    /// <summary>When a <c>pref</c> candidate did not form its glyph, the base is around it (src/hb-ot-shaper-indic.cc:1043-1062).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="found">The first base candidate.</param>
    /// <param name="tryPref">Whether a <c>pref</c> candidate is still being looked for.</param>
    /// <returns>The base and whether a <c>pref</c> candidate is still being looked for.</returns>
    let private preferredBase (plan: IndicPlan) (window: Window) found tryPref =
        let candidate = match tryPref && found + 1 < window.Length with | true -> firstWithMask window plan.PrefMask (found + 1) | false -> window.Length
        match candidate < window.Length with
        | false -> struct (found, tryPref)
        | true ->
            let props = propsAt window candidate
            match GlyphPropsBits.substituted props && GlyphPropsBits.ligatedAndDidntMultiply props with
            | true -> struct (found, tryPref)
            | false -> struct (baseAfterUnformedPref window candidate, false)

    /// <summary>For Malayalam, skips over the unformed below-forms, but not the post-forms, the loop of
    /// src/hb-ot-shaper-indic.cc:1070-1097.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="found">The base found so far.</param>
    /// <returns>The base.</returns>
    [<TailCall>]
    let rec private malayalamBase (window: Window) index found : int =
        match index < window.Length with
        | false -> found
        | true ->
            let halant = skipJoiners window index
            match halant = window.Length || not (Indic.isHalant (propsAt window halant)) with
            | true -> found
            | false ->
                let consonant = skipJoiners window (halant + 1)
                let isBelow = consonant < window.Length && Indic.isConsonant (propsAt window consonant) && (propsAt window consonant).ShaperAuxiliary = IndicPosition.PosBelowC
                match isBelow with
                | true ->
                    setPosition window consonant IndicPosition.PosBaseC
                    malayalamBase window (consonant + 1) consonant
                | false -> malayalamBase window (consonant + 1) found

    /// <summary>Finds the base from the first glyph whose position is the base's or later (src/hb-ot-shaper-indic.cc:1037-1100).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="first">The first glyph from the base position.</param>
    /// <param name="tryPref">Whether a <c>pref</c> candidate is being looked for.</param>
    /// <returns>The base, and whether a <c>pref</c> candidate is still being looked for.</returns>
    let private baseFromCandidate (environment: Environment) (window: Window) first tryPref : struct (int * bool) =
        let struct (found, stillTrying) = preferredBase environment.Plan window first tryPref
        match found = window.Length with
        | true -> struct (found, stillTrying)
        | false ->
            let formed =
                match environment.Script = Scripts.Malayalam with
                | true -> malayalamBase window (found + 1) found
                | false -> found
            match formed > 0 && (propsAt window formed).ShaperAuxiliary > IndicPosition.PosBaseC with
            | true -> struct (formed - 1, stillTrying)
            | false -> struct (formed, stillTrying)

    /// <summary>Moves the base back over the nuktas and halants that follow the real base (src/hb-ot-shaper-indic.cc:1101-1106).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="found">The base.</param>
    /// <returns>The base.</returns>
    [<TailCall>]
    let rec private backOverMarks (window: Window) found =
        match found > 0 && Indic.isOneOf (Indic.flagUnsafe IndicMachine.N ||| Indic.flagUnsafe IndicMachine.H) (propsAt window found) with
        | true -> backOverMarks window (found - 1)
        | false -> found

    /// <summary>Finds the base again, after the substitutions of the basic features (src/hb-ot-shaper-indic.cc:1028-1106).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    /// <returns>The base, and whether a <c>pref</c> candidate is still being looked for.</returns>
    let private findBase (environment: Environment) (window: Window) =
        let tryPref = environment.Plan.PrefMask <> 0u
        let first = firstFromBase window 0
        let struct (found, stillTrying) =
            match first < window.Length with
            | true -> baseFromCandidate environment window first tryPref
            | false -> struct (window.Length, tryPref)
        let loseTrackOfBase = found = window.Length && found > 0 && Indic.isOneOf (Indic.flagUnsafe IndicMachine.ZWJ) (propsAt window (found - 1))
        let adjusted = match loseTrackOfBase with | true -> found - 1 | false -> found
        match adjusted < window.Length with
        | true -> struct (backOverMarks window adjusted, stillTrying)
        | false -> struct (adjusted, stillTrying)

    /// <summary>Walks back from a glyph to the first matra, post-base matra or halant, or the start of the syllable.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="position">The glyph to examine.</param>
    /// <returns>The index.</returns>
    [<TailCall>]
    let rec private backToMatraOrHalant (window: Window) position =
        match position > 0 && not (Indic.isOneOf matraOrHalantFlags (propsAt window position)) with
        | true -> backToMatraOrHalant window (position - 1)
        | false -> position

    /// <summary>Finds the position a left matra moves to, the search of src/hb-ot-shaper-indic.cc:1139-1189.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="position">The glyph to examine.</param>
    /// <returns>The position, the start of the syllable when the matra does not move.</returns>
    [<TailCall>]
    let rec private searchMatraPosition (window: Window) position =
        let stepped = backToMatraOrHalant window position
        let props = propsAt window stepped
        match Indic.isHalant props && props.ShaperAuxiliary <> IndicPosition.PosPreM with
        | false -> 0
        | true ->
            match stepped + 1 < window.Length && (propsAt window (stepped + 1)).ShaperCategory = IndicMachine.ZWJ && stepped > 0 with
            | true -> searchMatraPosition window (stepped - 1)
            | false -> stepped

    /// <summary>Finds the first left matra before the base and merges its cluster with the base's, when no matra is to move
    /// (src/hb-ot-shaper-indic.cc:1204-1211).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="window">The syllable.</param>
    [<TailCall>]
    let rec private mergeFirstLeftMatra (environment: Environment) found index (window: Window) =
        match index < found with
        | false -> ()
        | true ->
            match (propsAt window index).ShaperAuxiliary = IndicPosition.PosPreM with
            | true -> SyllableOwned.mergeClusters environment.Level window.Infos (window.Start + index) (window.Start + min window.Length (found + 1))
            | false -> mergeFirstLeftMatra environment found (index + 1) window

    /// <summary>Moves the left matras before the base to the position found, one after the other from the nearest, the loop of
    /// src/hb-ot-shaper-indic.cc:1170-1195.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The glyph before the one to examine.</param>
    /// <param name="position">The position the next matra moves to.</param>
    /// <param name="found">The base.</param>
    /// <returns>The base, which moves back when a matra passes it.</returns>
    [<TailCall>]
    let rec private moveLeftMatras (environment: Environment) (window: Window) index position found : int =
        match index > 0 with
        | false -> found
        | true ->
            match (propsAt window (index - 1)).ShaperAuxiliary = IndicPosition.PosPreM with
            | false -> moveLeftMatras environment window (index - 1) position found
            | true ->
                let oldPosition = index - 1
                let moved = match oldPosition < found && found <= position with | true -> found - 1 | false -> found
                SyllableOwned.move window.Infos window.Props (window.Start + oldPosition) (window.Start + position)
                SyllableOwned.mergeClusters environment.Level window.Infos (window.Start + position) (window.Start + min window.Length (moved + 1))
                moveLeftMatras environment window (index - 1) (position - 1) moved

    /// <summary>Reorders the left matras (src/hb-ot-shaper-indic.cc:1108-1212).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    /// <returns>The base.</returns>
    let private reorderMatras (environment: Environment) found (window: Window) =
        match window.Length > 1 && found > 0 with
        | false -> found
        | true ->
            let lostBase = match found = window.Length with | true -> found - 2 | false -> found - 1
            let position = match hasNoHalfForms environment.Script with | true -> lostBase | false -> searchMatraPosition window lostBase
            match position > 0 && (propsAt window position).ShaperAuxiliary <> IndicPosition.PosPreM with
            | true -> moveLeftMatras environment window position position found
            | false ->
                mergeFirstLeftMatra environment found 0 window
                found

    /// <summary>Finds the position after the first halant between the first glyph after the reph and the base, past the joiners
    /// that follow it (src/hb-ot-shaper-indic.cc:1252-1266, repeated at 1313-1325).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="found">The base.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The position, or none when there is no such halant.</returns>
    [<TailCall>]
    let rec private halantBeforeBase (window: Window) found index : int voption =
        match index < found && not (Indic.isHalant (propsAt window index)) with
        | true -> halantBeforeBase window found (index + 1)
        | false ->
            match index < found with
            | false -> ValueNone
            | true ->
                match index + 1 < found && Indic.isJoiner (propsAt window (index + 1)) with
                | true -> ValueSome (index + 1)
                | false -> ValueSome index

    /// <summary>Finds the last glyph after the base whose position is not after the main consonant (src/hb-ot-shaper-indic.cc:1275-1282).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The glyph examined.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private afterMainConsonant (window: Window) index =
        match index + 1 < window.Length && (propsAt window (index + 1)).ShaperAuxiliary <= IndicPosition.PosAfterMain with
        | true -> afterMainConsonant window (index + 1)
        | false -> index

    /// <summary>The positions a reph is put before when it goes before the post-base consonants.</summary>
    let private postBasePositionFlags =
        Indic.flagUnsafe IndicPosition.PosPostC ||| Indic.flagUnsafe IndicPosition.PosAfterPost ||| Indic.flagUnsafe IndicPosition.PosSmvd

    /// <summary>Finds the glyph before the first post-base consonant (src/hb-ot-shaper-indic.cc:1291-1301).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The glyph examined.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private beforePostBase (window: Window) index =
        match index + 1 < window.Length && Indic.flagUnsafe (propsAt window (index + 1)).ShaperAuxiliary &&& postBasePositionFlags = 0u with
        | true -> beforePostBase window (index + 1)
        | false -> index

    /// <summary>Walks back from the end of the syllable over the syllable modifier signs.</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The glyph examined.</param>
    /// <returns>The index of the last glyph that is not one.</returns>
    [<TailCall>]
    let rec private beforeModifierSigns (window: Window) index =
        match index > 0 && (propsAt window index).ShaperAuxiliary = IndicPosition.PosSmvd with
        | true -> beforeModifierSigns window (index - 1)
        | false -> index

    /// <summary>Counts, from after the base, the matras that precede the position the reph goes to (src/hb-ot-shaper-indic.cc:1336-1349).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="target">The position the reph goes to.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private beforeMatraHalant (window: Window) index target =
        match index < target with
        | false -> target
        | true ->
            match Indic.flagUnsafe (propsAt window index).ShaperCategory &&& (Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst) <> 0u with
            | true -> beforeMatraHalant window (index + 1) (target - 1)
            | false -> beforeMatraHalant window (index + 1) target

    /// <summary>Finds the position at the end of the syllable (src/hb-ot-shaper-indic.cc:1327-1351).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="found">The base.</param>
    /// <returns>The position.</returns>
    let private endOfSyllable (window: Window) found =
        let last = beforeModifierSigns window (window.Length - 1)
        match Indic.isHalant (propsAt window last) with
        | true -> beforeMatraHalant window (found + 1) last
        | false -> last

    /// <summary>Finds where the reph goes, <c>new_reph_pos</c> (src/hb-ot-shaper-indic.cc:1229-1361).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    /// <param name="found">The base.</param>
    /// <returns>The position.</returns>
    let private rephPosition (environment: Environment) (window: Window) found =
        match halantBeforeBase window found 1 with
        | ValueSome position -> position
        | ValueNone ->
            let extra =
                match environment.Plan.Config.RephPosition with
                | RephAfterMain -> ValueSome (afterMainConsonant window found)
                | RephAfterSub -> ValueSome (beforePostBase window found)
                | RephBeforeSub | RephBeforePost | RephAfterPost -> ValueNone
            match extra with
            | ValueSome position when position < window.Length -> position
            | ValueSome _ | ValueNone -> endOfSyllable window found

    /// <summary>Reorders the reph (src/hb-ot-shaper-indic.cc:1214-1370).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="window">The syllable.</param>
    /// <returns>The base, which moves back when the reph passes it.</returns>
    let private reorderReph (environment: Environment) found (window: Window) =
        let first = propsAt window 0
        let moves =
            window.Length > 1
            && first.ShaperAuxiliary = IndicPosition.PosRaToBecomeReph
            && ((first.ShaperCategory = IndicMachine.Repha) <> GlyphPropsBits.ligatedAndDidntMultiply first)
        match moves with
        | false -> found
        | true ->
            let target = rephPosition environment window found
            SyllableOwned.mergeClusters environment.Level window.Infos window.Start (window.Start + target + 1)
            let moved = match 0 < found && found <= target with | true -> found - 1 | false -> found
            SyllableOwned.move window.Infos window.Props window.Start (window.Start + target)
            moved

    /// <summary>Walks back from the base to the glyph a pre-base-reordering consonant goes before (src/hb-ot-shaper-indic.cc:1389-1397).</summary>
    /// <param name="window">The syllable.</param>
    /// <param name="position">The position, which the glyph before is examined for.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private backToPrefTarget (window: Window) position =
        match position > 0 && not (Indic.isOneOf matraOrHalantFlags (propsAt window (position - 1))) with
        | true -> backToPrefTarget window (position - 1)
        | false -> position

    /// <summary>Reorders the pre-base-reordering consonant (src/hb-ot-shaper-indic.cc:1372-1427).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="tryPref">Whether a <c>pref</c> candidate is still being looked for.</param>
    /// <param name="window">The syllable.</param>
    let private reorderPref (environment: Environment) found tryPref (window: Window) =
        match tryPref && found + 1 < window.Length with
        | false -> ()
        | true ->
            let candidate = firstWithMask window environment.Plan.PrefMask (found + 1)
            match candidate < window.Length && GlyphPropsBits.ligatedAndDidntMultiply (propsAt window candidate) with
            | false -> ()
            | true ->
                let position = match hasNoHalfForms environment.Script with | true -> found | false -> backToPrefTarget window found
                let afterJoiner = position > 0 && Indic.isHalant (propsAt window (position - 1)) && position < window.Length && Indic.isJoiner (propsAt window position)
                let target = match afterJoiner with | true -> position + 1 | false -> position
                SyllableOwned.mergeClusters environment.Level window.Infos (window.Start + target) (window.Start + candidate + 1)
                SyllableOwned.move window.Infos window.Props (window.Start + candidate) (window.Start + target)

    /// <summary>Gives the <c>init</c> feature to a left matra that starts a word, or marks it and what is before it unsafe to
    /// break (src/hb-ot-shaper-indic.cc:1429-1439).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="window">The syllable.</param>
    let private initialLeftMatra (plan: IndicPlan) (window: Window) =
        match (propsAt window 0).ShaperAuxiliary = IndicPosition.PosPreM with
        | false -> ()
        | true -> SyllableOwned.initOrUnsafeToBreakAcross window.Infos window.Props window.Start plan.InitMask

    /// <summary>Reorders one syllable, <c>final_reordering_syllable_indic</c> (src/hb-ot-shaper-indic.cc:998-1440): a syllable of one glyph has
    /// no base to find and nothing to move, so only the halant recovery and the <c>init</c> mask apply.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="window">The syllable.</param>
    let private reorderSyllable (environment: Environment) (window: Window) =
        match environment.Virama with
        | ValueNone -> ()
        | ValueSome virama -> recoverViramas virama window 0
        match window.Length with
        | 1 -> ()
        | _ ->
            let struct (found, tryPref) = findBase environment window
            let matraBase = reorderMatras environment found window
            let rephBase = reorderReph environment matraBase window
            reorderPref environment rephBase tryPref window
        initialLeftMatra environment.Plan window

    /// <summary>Reorders the syllable chunks of a run from an index.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <param name="start">The first glyph of the next chunk.</param>
    [<TailCall>]
    let rec private reorderChunks (environment: Environment) (run: WorkingSet) start =
        match start < run.Props.Length with
        | false -> ()
        | true ->
            let finish = Sealed.nextSyllableEnd (run.Props.AsSpan()) start
            reorderSyllable environment { Infos = run.Infos; Props = run.Props; Start = start; Length = finish - start }
            reorderChunks environment run finish

    /// <summary>Reorders the syllables of the run in place once the basic features have shaped them, <c>final_reordering_indic</c>
    /// (src/hb-ot-shaper-indic.cc:1443-1461).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, whose arrays were made in this call and are shared with no one.</param>
    /// <returns>The reordered run.</returns>
    let reorder (plan: IndicPlan) (font: Font) (run: WorkingSet) =
        match run.Infos.Length with
        | 0 -> run
        | _ ->
            let environment : Environment =
                { Plan = plan
                  Script = run.SegmentProperties.Script
                  Level = run.ClusterLevel
                  Virama = Indic.viramaGlyph plan font }
            reorderChunks environment run 0
            run
