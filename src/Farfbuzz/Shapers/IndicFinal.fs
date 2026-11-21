namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Indic shaper's final reordering, <c>final_reordering_indic</c> (src/hb-ot-shaper-indic.cc:998-1461): after
/// the basic features have shaped the glyphs, the base is found again, the left matras move towards it, the reph moves to
/// where its script puts it, the pre-base-reordering consonant moves in front of the base, and a left matra that starts a
/// word is given the <c>init</c> feature.</summary>
[<RequireQualifiedAccess>]
module internal IndicFinal =
    /// <summary>What the reordering of a syllable reads besides the syllable: the plan data, the script, the cluster level and
    /// the glyph of the virama.</summary>
    [<Struct>]
    type private Environment =
        { Plan: IndicPlan
          Script: Script
          Level: ClusterLevel
          Virama: uint32 voption }

    /// <summary>Gives a glyph a position.</summary>
    /// <param name="position">The position.</param>
    /// <param name="row">The glyph.</param>
    /// <returns>The glyph at the position.</returns>
    let private withPosition position (row: GlyphRow) = { row with Props = { row.Props with ShaperAuxiliary = position } }

    /// <summary>Whether the script is one of the two that have no half forms or explicit virama forms, Malayalam and Tamil: the
    /// glyphs their <c>half</c> forms make are chillus or ligated explicit viramas, which a matra goes after.</summary>
    /// <param name="script">The script.</param>
    /// <returns>True for Malayalam and Tamil.</returns>
    let private hasNoHalfForms script = script = Scripts.Malayalam || script = Scripts.Tamil

    /// <summary>The categories a matra goes after: a matra, a post-base matra and a halant.</summary>
    let private matraOrHalantFlags = Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst ||| Indic.flagUnsafe IndicMachine.H

    /// <summary>Makes the halants the substitutions ligated and multiplied halants again, so that they pass the halant test,
    /// the recovery at the start of <c>final_reordering_syllable_indic</c> (src/hb-ot-shaper-indic.cc:1008-1026).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the halants recovered.</returns>
    let private recoverViramas (environment: Environment) (rows: ImmutableArray<GlyphRow>) =
        match environment.Virama with
        | ValueNone -> rows
        | ValueSome virama ->
            Sealed.mapi (fun _ (row: GlyphRow) ->
                match row.Info.Codepoint = virama && GlyphPropsBits.ligated row.Props && GlyphPropsBits.multiplied row.Props with
                | true -> { row with Props = GlyphPropsBits.clearLigatedAndMultiplied { row.Props with ShaperCategory = IndicMachine.H } }
                | false -> row) (rows.AsSpan())

    /// <summary>Finds the first glyph whose position is that of the base or after it.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The index, or the end of the syllable.</returns>
    [<TailCall>]
    let rec private firstFromBase (rows: ImmutableArray<GlyphRow>) index =
        match index < rows.Length && rows[index].Props.ShaperAuxiliary < IndicPosition.PosBaseC with
        | true -> firstFromBase rows (index + 1)
        | false -> index

    /// <summary>Finds the first glyph from an index that has some bit of a mask.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="mask">The mask.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The index, or the end of the syllable.</returns>
    [<TailCall>]
    let rec private firstWithMask (rows: ImmutableArray<GlyphRow>) mask index =
        match index < rows.Length && rows[index].Info.Mask &&& mask = 0u with
        | true -> firstWithMask rows mask (index + 1)
        | false -> index

    /// <summary>Skips the halants from an index.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The first index.</param>
    /// <returns>The index of the first glyph that is not a halant.</returns>
    [<TailCall>]
    let rec private skipHalants (rows: ImmutableArray<GlyphRow>) index =
        match index < rows.Length && Indic.isHalant rows[index].Props with
        | true -> skipHalants rows (index + 1)
        | false -> index

    /// <summary>Takes as the base the glyph after the halants that follow a <c>pref</c> candidate that did not form its glyph,
    /// and gives it the position of the base (src/hb-ot-shaper-indic.cc:1043-1062).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="candidate">The candidate.</param>
    /// <returns>The base, the rows with the new base's position set, and that no <c>pref</c> candidate is looked for any more.</returns>
    let private baseAfterUnformedPref (rows: ImmutableArray<GlyphRow>) candidate =
        let skipped = skipHalants rows candidate
        match skipped < rows.Length with
        | true -> struct (skipped, Sealed.setItem skipped (withPosition IndicPosition.PosBaseC rows[skipped]) rows, false)
        | false -> struct (skipped, rows, false)

    /// <summary>When a <c>pref</c> candidate did not form its glyph, the base is around it: after the halants that follow the
    /// first base candidate, and that is the base the shaper takes (src/hb-ot-shaper-indic.cc:1043-1062).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="rows">The rows.</param>
    /// <param name="found">The first base candidate.</param>
    /// <param name="tryPref">Whether a <c>pref</c> candidate is still being looked for.</param>
    /// <returns>The base, the rows with the new base's position set, and whether a <c>pref</c> candidate is still being looked for.</returns>
    let private preferredBase (plan: IndicPlan) (rows: ImmutableArray<GlyphRow>) found tryPref =
        let candidate = match tryPref && found + 1 < rows.Length with | true -> firstWithMask rows plan.PrefMask (found + 1) | false -> rows.Length
        match candidate < rows.Length with
        | false -> struct (found, rows, tryPref)
        | true ->
            let props = rows[candidate].Props
            match GlyphPropsBits.substituted props && GlyphPropsBits.ligatedAndDidntMultiply props with
            | true -> struct (found, rows, tryPref)
            | false -> baseAfterUnformedPref rows candidate

    /// <summary>For Malayalam, skips over the unformed below-forms, but not the post-forms, the loop of
    /// src/hb-ot-shaper-indic.cc:1070-1097: a halant and a consonant that is to go below the base make that consonant the base.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="found">The base found so far.</param>
    /// <returns>The base and the rows with its position set.</returns>
    [<TailCall>]
    let rec private malayalamBase (rows: ImmutableArray<GlyphRow>) index found : struct (int * ImmutableArray<GlyphRow>) =
        match index < rows.Length with
        | false -> struct (found, rows)
        | true ->
            let halant = Indic.skipJoiners rows index
            match halant = rows.Length || not (Indic.isHalant rows[halant].Props) with
            | true -> struct (found, rows)
            | false ->
                let consonant = Indic.skipJoiners rows (halant + 1)
                let isBelow = consonant < rows.Length && Indic.isConsonant rows[consonant].Props && rows[consonant].Props.ShaperAuxiliary = IndicPosition.PosBelowC
                match isBelow with
                | true -> malayalamBase (Sealed.setItem consonant (withPosition IndicPosition.PosBaseC rows[consonant]) rows) (consonant + 1) consonant
                | false -> malayalamBase rows (consonant + 1) found

    /// <summary>Finds the base from the first glyph whose position is the base's or later (src/hb-ot-shaper-indic.cc:1037-1100).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="rows">The rows.</param>
    /// <param name="first">The first glyph from the base position.</param>
    /// <param name="tryPref">Whether a <c>pref</c> candidate is being looked for.</param>
    /// <returns>The base, the rows, and whether a <c>pref</c> candidate is still being looked for.</returns>
    let private baseFromCandidate (environment: Environment) (rows: ImmutableArray<GlyphRow>) first tryPref : struct (int * ImmutableArray<GlyphRow> * bool) =
        let struct (found, preferred, stillTrying) = preferredBase environment.Plan rows first tryPref
        match found = rows.Length with
        | true -> struct (found, preferred, stillTrying)
        | false ->
            let struct (formed, formedRows) =
                match environment.Script = Scripts.Malayalam with
                | true -> malayalamBase preferred (found + 1) found
                | false -> struct (found, preferred)
            match formed > 0 && formedRows[formed].Props.ShaperAuxiliary > IndicPosition.PosBaseC with
            | true -> struct (formed - 1, formedRows, stillTrying)
            | false -> struct (formed, formedRows, stillTrying)

    /// <summary>Moves the base back over the nuktas and halants that follow the real base, the last step of the base search
    /// (src/hb-ot-shaper-indic.cc:1101-1106).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="found">The base.</param>
    /// <returns>The base.</returns>
    [<TailCall>]
    let rec private backOverMarks (rows: ImmutableArray<GlyphRow>) found =
        match found > 0 && Indic.isOneOf (Indic.flagUnsafe IndicMachine.N ||| Indic.flagUnsafe IndicMachine.H) rows[found].Props with
        | true -> backOverMarks rows (found - 1)
        | false -> found

    /// <summary>Finds the base again, after the substitutions of the basic features (src/hb-ot-shaper-indic.cc:1028-1106).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The base, the rows, and whether a <c>pref</c> candidate is still being looked for.</returns>
    let private findBase (environment: Environment) (rows: ImmutableArray<GlyphRow>) =
        let tryPref = environment.Plan.PrefMask <> 0u
        let first = firstFromBase rows 0
        let struct (found, foundRows, stillTrying) =
            match first < rows.Length with
            | true -> baseFromCandidate environment rows first tryPref
            | false -> struct (rows.Length, rows, tryPref)
        let loseTrackOfBase = found = rows.Length && found > 0 && Indic.isOneOf (Indic.flagUnsafe IndicMachine.ZWJ) foundRows[found - 1].Props
        let adjusted = match loseTrackOfBase with | true -> found - 1 | false -> found
        match adjusted < rows.Length with
        | true -> struct (backOverMarks foundRows adjusted, foundRows, stillTrying)
        | false -> struct (adjusted, foundRows, stillTrying)

    /// <summary>Walks back from a glyph to the first matra, post-base matra or halant, or the start of the syllable.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="position">The glyph to examine.</param>
    /// <returns>The index.</returns>
    [<TailCall>]
    let rec private backToMatraOrHalant (rows: ImmutableArray<GlyphRow>) position =
        match position > 0 && not (Indic.isOneOf matraOrHalantFlags rows[position].Props) with
        | true -> backToMatraOrHalant rows (position - 1)
        | false -> position

    /// <summary>Finds the position a left matra moves to, the search of src/hb-ot-shaper-indic.cc:1139-1189: back from the base
    /// to a matra or a halant, where a halant that is not the matra's own is the place unless a ZWJ follows it, in which case
    /// the search goes on, and else the matra does not move.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="position">The glyph to examine.</param>
    /// <returns>The position, the start of the syllable when the matra does not move.</returns>
    [<TailCall>]
    let rec private searchMatraPosition (rows: ImmutableArray<GlyphRow>) position =
        let stepped = backToMatraOrHalant rows position
        let props = rows[stepped].Props
        match Indic.isHalant props && props.ShaperAuxiliary <> IndicPosition.PosPreM with
        | false -> 0
        | true ->
            match stepped + 1 < rows.Length && rows[stepped + 1].Props.ShaperCategory = IndicMachine.ZWJ && stepped > 0 with
            | true -> searchMatraPosition rows (stepped - 1)
            | false -> stepped

    /// <summary>Finds the first left matra before the base and merges its cluster with the base's, when no matra is to move
    /// (src/hb-ot-shaper-indic.cc:1204-1211).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable.</returns>
    [<TailCall>]
    let rec private mergeFirstLeftMatra (environment: Environment) found index (state: SyllableState) =
        match index < found with
        | false -> state
        | true ->
            match state.Rows[index].Props.ShaperAuxiliary = IndicPosition.PosPreM with
            | true -> SyllableEdit.mergeClusters environment.Level index (min state.Rows.Length (found + 1)) state
            | false -> mergeFirstLeftMatra environment found (index + 1) state

    /// <summary>Moves the left matras before the base to the position found, one after the other from the nearest,
    /// the loop of src/hb-ot-shaper-indic.cc:1170-1195: each is moved and the clusters between it and the base are merged
    /// after the move.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="index">The glyph before the one to examine.</param>
    /// <param name="position">The position the next matra moves to.</param>
    /// <param name="found">The base.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable and the base, which moves back when a matra passes it.</returns>
    [<TailCall>]
    let rec private moveLeftMatras (environment: Environment) index position found (state: SyllableState) =
        match index > 0 with
        | false -> struct (state, found)
        | true ->
            match state.Rows[index - 1].Props.ShaperAuxiliary = IndicPosition.PosPreM with
            | false -> moveLeftMatras environment (index - 1) position found state
            | true ->
                let oldPosition = index - 1
                let moved = match oldPosition < found && found <= position with | true -> found - 1 | false -> found
                let rows = Sealed.move oldPosition position state.Rows
                let merged = SyllableEdit.mergeClusters environment.Level position (min rows.Length (moved + 1)) { state with Rows = rows }
                moveLeftMatras environment (index - 1) (position - 1) moved merged

    /// <summary>Reorders the left matras: a matra that had been moved before the half forms is moved closer to the base, to
    /// after the last standalone halant, after the initial matra position and before the main consonant
    /// (src/hb-ot-shaper-indic.cc:1108-1212). A ZWJ after the halant keeps the matra from being repositioned after it.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable and the base.</returns>
    let private reorderMatras (environment: Environment) found (state: SyllableState) =
        let rows = state.Rows
        match rows.Length > 1 && found > 0 with
        | false -> struct (state, found)
        | true ->
            let lostBase = match found = rows.Length with | true -> found - 2 | false -> found - 1
            let position = match hasNoHalfForms environment.Script with | true -> lostBase | false -> searchMatraPosition rows lostBase
            match position > 0 && rows[position].Props.ShaperAuxiliary <> IndicPosition.PosPreM with
            | true -> moveLeftMatras environment position position found state
            | false -> struct (mergeFirstLeftMatra environment found 0 state, found)

    /// <summary>Finds the position after the first halant between the first glyph after the reph and the base, past the joiners
    /// that follow it, the step 2 of the reph's placement (src/hb-ot-shaper-indic.cc:1252-1266, repeated at 1313-1325).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="found">The base.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The position, or none when there is no such halant.</returns>
    [<TailCall>]
    let rec private halantBeforeBase (rows: ImmutableArray<GlyphRow>) found index : int voption =
        match index < found && not (Indic.isHalant rows[index].Props) with
        | true -> halantBeforeBase rows found (index + 1)
        | false ->
            match index < found with
            | false -> ValueNone
            | true ->
                match index + 1 < found && Indic.isJoiner rows[index + 1].Props with
                | true -> ValueSome (index + 1)
                | false -> ValueSome index

    /// <summary>Finds the last glyph after the base whose position is not after the main consonant, the step 3 of the reph's
    /// placement (src/hb-ot-shaper-indic.cc:1275-1282): for the scripts whose reph goes after the main consonant.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The glyph examined.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private afterMainConsonant (rows: ImmutableArray<GlyphRow>) index =
        match index + 1 < rows.Length && rows[index + 1].Props.ShaperAuxiliary <= IndicPosition.PosAfterMain with
        | true -> afterMainConsonant rows (index + 1)
        | false -> index

    /// <summary>The positions a reph is put before when it goes before the post-base consonants: the post-base consonant, what
    /// goes after it and the syllable modifier signs.</summary>
    let private postBasePositionFlags =
        Indic.flagUnsafe IndicPosition.PosPostC ||| Indic.flagUnsafe IndicPosition.PosAfterPost ||| Indic.flagUnsafe IndicPosition.PosSmvd

    /// <summary>Finds the glyph before the first post-base consonant, the step 4 of the reph's placement
    /// (src/hb-ot-shaper-indic.cc:1291-1301): for the scripts whose reph goes after the subjoined consonants.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The glyph examined.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private beforePostBase (rows: ImmutableArray<GlyphRow>) index =
        match index + 1 < rows.Length && Indic.flagUnsafe rows[index + 1].Props.ShaperAuxiliary &&& postBasePositionFlags = 0u with
        | true -> beforePostBase rows (index + 1)
        | false -> index

    /// <summary>Walks back from the end of the syllable over the syllable modifier signs.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The glyph examined.</param>
    /// <returns>The index of the last glyph that is not one.</returns>
    [<TailCall>]
    let rec private beforeModifierSigns (rows: ImmutableArray<GlyphRow>) index =
        match index > 0 && rows[index].Props.ShaperAuxiliary = IndicPosition.PosSmvd with
        | true -> beforeModifierSigns rows (index - 1)
        | false -> index

    /// <summary>Moves a reph that is to end up after a matra and a halant in front of that halant, so that it can interact
    /// with the matra: counts, from after the base, the matras that precede the position (src/hb-ot-shaper-indic.cc:1336-1349).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="target">The position the reph goes to.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private beforeMatraHalant (rows: ImmutableArray<GlyphRow>) index target =
        match index < target with
        | false -> target
        | true ->
            match Indic.flagUnsafe rows[index].Props.ShaperCategory &&& (Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst) <> 0u with
            | true -> beforeMatraHalant rows (index + 1) (target - 1)
            | false -> beforeMatraHalant rows (index + 1) target

    /// <summary>Finds the position at the end of the syllable, the step 6 of the reph's placement
    /// (src/hb-ot-shaper-indic.cc:1327-1351): before the syllable modifier signs, and, when a halant would follow a matra
    /// there, before that halant.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="found">The base.</param>
    /// <returns>The position.</returns>
    let private endOfSyllable (rows: ImmutableArray<GlyphRow>) found =
        let last = beforeModifierSigns rows (rows.Length - 1)
        match Indic.isHalant rows[last].Props with
        | true -> beforeMatraHalant rows (found + 1) last
        | false -> last

    /// <summary>Finds where the reph goes, <c>new_reph_pos</c> (src/hb-ot-shaper-indic.cc:1229-1361): after the first halant
    /// between the reph and the base when there is one; else, for the scripts that put it after the main consonant or after the
    /// subjoined consonants, there; else at the end of the syllable. A script that puts it after the post-base consonants takes
    /// the same halant (step 5) or the end.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="rows">The rows.</param>
    /// <param name="found">The base.</param>
    /// <returns>The position.</returns>
    let private rephPosition (environment: Environment) (rows: ImmutableArray<GlyphRow>) found =
        match halantBeforeBase rows found 1 with
        | ValueSome position -> position
        | ValueNone ->
            let extra =
                match environment.Plan.Config.RephPosition with
                | RephAfterMain -> ValueSome (afterMainConsonant rows found)
                | RephAfterSub -> ValueSome (beforePostBase rows found)
                | RephBeforeSub | RephBeforePost | RephAfterPost -> ValueNone
            match extra with
            | ValueSome position when position < rows.Length -> position
            | ValueSome _ | ValueNone -> endOfSyllable rows found

    /// <summary>Reorders the reph (src/hb-ot-shaper-indic.cc:1214-1370): when the reph is encoded as characters it is moved
    /// only if the characters ligated to the reph form, and when it is encoded as a separate repha in its logical position, only
    /// if it did not ligate, which would be the font making it work without the reordering.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable and the base, which moves back when the reph passes it.</returns>
    let private reorderReph (environment: Environment) found (state: SyllableState) =
        let rows = state.Rows
        let first = rows[0].Props
        let moves =
            rows.Length > 1
            && first.ShaperAuxiliary = IndicPosition.PosRaToBecomeReph
            && ((first.ShaperCategory = IndicMachine.Repha) <> GlyphPropsBits.ligatedAndDidntMultiply first)
        match moves with
        | false -> struct (state, found)
        | true ->
            let target = rephPosition environment rows found
            let merged = SyllableEdit.mergeClusters environment.Level 0 (target + 1) state
            let moved = match 0 < found && found <= target with | true -> found - 1 | false -> found
            struct ({ merged with Rows = Sealed.move 0 target merged.Rows }, moved)

    /// <summary>Walks back from the base to the glyph a pre-base-reordering consonant goes before: the last matra or halant
    /// before it (src/hb-ot-shaper-indic.cc:1389-1397).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="position">The position, which the glyph before is examined for.</param>
    /// <returns>The position.</returns>
    [<TailCall>]
    let rec private backToPrefTarget (rows: ImmutableArray<GlyphRow>) position =
        match position > 0 && not (Indic.isOneOf matraOrHalantFlags rows[position - 1].Props) with
        | true -> backToPrefTarget rows (position - 1)
        | false -> position

    /// <summary>Reorders the pre-base-reordering consonant, the first glyph after the base that took the mask of <c>pref</c>
    /// and ligated, which goes where a left matra would (src/hb-ot-shaper-indic.cc:1372-1427).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="tryPref">Whether a <c>pref</c> candidate is still being looked for.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable.</returns>
    let private reorderPref (environment: Environment) found tryPref (state: SyllableState) =
        let rows = state.Rows
        match tryPref && found + 1 < rows.Length with
        | false -> state
        | true ->
            let candidate = firstWithMask rows environment.Plan.PrefMask (found + 1)
            match candidate < rows.Length && GlyphPropsBits.ligatedAndDidntMultiply rows[candidate].Props with
            | false -> state
            | true ->
                let position = match hasNoHalfForms environment.Script with | true -> found | false -> backToPrefTarget rows found
                let afterJoiner = position > 0 && Indic.isHalant rows[position - 1].Props && position < rows.Length && Indic.isJoiner rows[position].Props
                let target = match afterJoiner with | true -> position + 1 | false -> position
                let merged = SyllableEdit.mergeClusters environment.Level target (candidate + 1) state
                { merged with Rows = Sealed.move candidate target merged.Rows }

    /// <summary>Gives the <c>init</c> feature to a left matra that starts a word, or marks it and what is before it unsafe to
    /// break, once the glyph before the syllable is known (src/hb-ot-shaper-indic.cc:1429-1439): that glyph is in another
    /// chunk, so the decision is a patch applied at the left edge of the syllable.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the patch.</returns>
    let private initialLeftMatra (plan: IndicPlan) (state: SyllableState) =
        match state.Rows[0].Props.ShaperAuxiliary = IndicPosition.PosPreM with
        | false -> state
        | true -> { state with Left = Sealed.appendOne (state.Left.AsSpan()) (BoundaryPatch.InitOrUnsafeToBreakAcross plan.InitMask) }

    /// <summary>Reorders one syllable, <c>final_reordering_syllable_indic</c> (src/hb-ot-shaper-indic.cc:998-1440).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The reordered syllable.</returns>
    let private reorderSyllable (environment: Environment) (state: SyllableState) =
        let struct (found, foundRows, tryPref) = findBase environment (recoverViramas environment state.Rows)
        let struct (matraState, matraBase) = reorderMatras environment found { state with Rows = foundRows }
        let struct (rephState, rephBase) = reorderReph environment matraBase matraState
        initialLeftMatra environment.Plan (reorderPref environment rephBase tryPref rephState)

    /// <summary>The rewriter that reorders every syllable, <c>final_reordering_syllable_indic</c> being called for each
    /// whatever its type.</summary>
    [<Struct>]
    type private ReorderSyllables =
        val Environment: Environment

        /// <summary>Creates the rewriter.</summary>
        /// <param name="environment">The environment.</param>
        new (environment) = { Environment = environment }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                let initial : SyllableState =
                    { Rows = Sealed.ofSpan rows
                      Left = ImmutableArray.Empty
                      Right = ImmutableArray.Empty }
                let final = reorderSyllable this.Environment initial
                let rewrite : SegmentRewrite =
                    { Rows = final.Rows
                      Left = final.Left
                      Right = final.Right }
                rewrite

    /// <summary>Reorders the syllables of the run once the basic features have shaped them, <c>final_reordering_indic</c>
    /// (src/hb-ot-shaper-indic.cc:1443-1461).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The reordered run.</returns>
    let reorder (plan: IndicPlan) (font: Font) (run: GlyphRun) =
        match run.Infos.Length with
        | 0 -> run
        | _ ->
            let environment : Environment =
                { Plan = plan
                  Script = run.SegmentProperties.Script
                  Level = run.ClusterLevel
                  Virama = Indic.viramaGlyph plan font }
            let struct (infos, props) = Sealed.rewriteSegments (ReorderSyllables(environment)) (run.Infos.AsSpan()) (run.Props.AsSpan())
            { run with Infos = infos; Props = props }
