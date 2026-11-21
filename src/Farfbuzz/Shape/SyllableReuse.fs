namespace Farfbuzz

open System.Collections.Immutable

/// <summary>Learning what the units of an Indic line become over the lookups and pauses their plan reuses, from the line's own run of them: the
/// line goes through the region's windows and pauses as it would the ordinary way, while a watch in the kernel's loop notes after each lookup
/// that changed it the ligature ids it allocated, the unit each went to and the glyphs each unit held. The run's result is the line's own, so
/// learning costs the line no second shaping; what the units became is kept when nothing in the run let one unit's lookups reach another.</summary>
[<RequireQualifiedAccess>]
module internal SyllableLearning =
    /// <summary>The general category Cf, the first of the range the final reordering's <c>init</c> test reads (hb-ot-shaper-indic.cc:1434).</summary>
    [<Literal>]
    let private CategoryFormat = 1us

    /// <summary>The general category Mn, the last of that range.</summary>
    [<Literal>]
    let private CategoryNonSpacingMark = 12us

    /// <summary>The state of a line's run between two steps.</summary>
    [<Struct; NoEquality; NoComparison>]
    type private Learning =
        { Set: WorkingSet  // the line's arrays, written in place by the steps
          Watched: bool  // every step so far ran under the watch
          Shifted: bool }  // the kernel shifted the input forward

    /// <summary>The glyph-class bits of the glyphs a lookup may skip: marks and ligatures, the base glyphs being skipped by no lookup of a
    /// plan that has a program.</summary>
    let private skippableClasses = GlyphPropsBits.Mark ||| GlyphPropsBits.Ligature

    /// <summary>Whether a pause is the final reordering of the Indic shaper, after which the next syllable reads this one's last glyph.</summary>
    /// <param name="pause">The pause.</param>
    /// <returns>True for the final reordering.</returns>
    let private isFinalReordering pause =
        match pause with
        | IndicFinalReordering -> true
        | NoPause | ClearSyllables | KhmerSetupSyllables | KhmerReorder | IndicSetupSyllables | IndicInitialReordering | ClearSubstitutionFlags
        | UseSetupSyllables | UseRecordRphf | UseRecordPref | UseReorder | MyanmarSetupSyllables | MyanmarReorder | ArabicRecordStch
        | ArabicFallbackShape | ArabicReleaseAction -> false

    /// <summary>Applies the lookups of one window of the region to the line, under the watch while the line is plain, the ordinary way after.</summary>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="font">The font.</param>
    /// <param name="watch">The watch of the run.</param>
    /// <param name="first">The map position of the window's first lookup.</param>
    /// <param name="window">The window.</param>
    /// <param name="state">The run so far.</param>
    /// <returns>The run after the window.</returns>
    let private windowStep (covered: ImmutableArray<ImmutableArray<uint64>>) (font: Font) (watch: LineWatch) first window (state: Learning) =
        match state.Watched with
        | false -> { state with Set = OtLayout.substituteWindowOwnedSet covered font window state.Set }
        | true ->
            watch.Enter first
            match Engine.applyGsubWindowWatched covered font.Ot.Gsub.Value window font state.Set watch with
            | ValueNone -> { state with Set = OtLayout.substituteWindowOwnedSet covered font window state.Set; Watched = false }
            | ValueSome struct (applied, shifted, _) -> { state with Set = applied; Shifted = state.Shifted || shifted }

    /// <summary>Runs the function of a pause on the line, the watch looking at the line after it.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="watch">The watch of the run.</param>
    /// <param name="pause">The pause.</param>
    /// <param name="state">The run so far.</param>
    /// <returns>The run after the pause.</returns>
    let private pauseStep (plan: OtShapePlan) (font: Font) (watch: LineWatch) pause (state: Learning) =
        let paused = ShaperHooks.applyPauseSet pause plan font state.Set
        match state.Watched with
        | true -> watch.Observe(paused.Props, byte paused.Serial, isFinalReordering pause)
        | false -> ()
        { state with Set = paused }

    /// <summary>Runs the steps of the reused region on the line, one after another.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="steps">The steps.</param>
    /// <param name="watch">The watch of the run.</param>
    /// <param name="index">The next step.</param>
    /// <param name="state">The run so far.</param>
    /// <returns>The run after the steps.</returns>
    [<TailCall>]
    let rec private runSteps (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (steps: ImmutableArray<RegionStep>) (watch: LineWatch) index (state: Learning) =
        match index >= steps.Length with
        | true -> state
        | false ->
            let next =
                match steps[index] with
                | ApplyWindow (first, window) -> windowStep covered font watch first window state
                | RunPause pause -> pauseStep plan font watch pause state
            runSteps plan font covered steps watch (index + 1) next

    /// <summary>Runs the reused region on a line and learns from that run the units the line learns: the run must have stayed under the watch,
    /// shifted nothing, succeeded with operations left and left the scratch flags as they were, so that what each unit contributed to them is
    /// nothing.</summary>
    /// <param name="program">The plan's reuse program.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="harvest">What the line learns.</param>
    /// <param name="ready">The line after the setup of its syllables, whose arrays the run writes in place.</param>
    /// <returns>The line after the region.</returns>
    let learn (program: SyllableProgram) (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (harvest: SyllableHarvest) (ready: WorkingSet) =
        let watch =
            LineWatch(ready.Props, harvest.HarvestUnits, byte ready.Serial, skippableClasses, UnicodeProps.Ignorable, UnicodeProps.GenCatMask, CategoryFormat,
                      CategoryNonSpacingMark, IndicPosition.PosPreM)
        let ran = runSteps plan font covered program.Steps watch 0 { Set = ready; Watched = true; Shifted = false }
        match ran.Watched && not ran.Shifted && ran.Set.Successful && ran.Set.MaxOps > 0 && ran.Set.ScratchFlags = ready.ScratchFlags with
        | true -> SyllableHarvestKernel.learn program.Table harvest watch ran.Set.Infos ran.Set.Props (ready.MaxOps - ran.Set.MaxOps) ready.ScratchFlags
        | false -> ()
        ran.Set

/// <summary>Reuse of what the syllables of an Indic line become, for a plan whose lookups from the setup of the syllables on are confined
/// to one syllable (<c>F_PER_SYLLABLE</c>, every feature of hb-ot-shaper-indic.cc:177-200 and its <c>locl</c> and <c>ccmp</c>): a syllable's
/// glyphs then depend on its own glyphs and on whether the glyph before it is a letter or a mark (hb-ot-shaper-indic.cc:1434) and on nothing
/// else of the line, so the units of a line can be looked up and the line rebuilt from them with the ligature ids, the operations and the
/// flags the lookups would have given it. A line or a unit any guard refuses is shaped the ordinary way.</summary>
[<RequireQualifiedAccess>]
module internal SyllableReuse =
    /// <summary>The lookup flag that skips base glyphs, <c>LookupFlag::IgnoreBaseGlyphs</c>.</summary>
    [<Literal>]
    let private IgnoreBaseGlyphs = 0x0002u

    /// <summary>The syllable type of a broken cluster of the Indic shaper, <c>indic_broken_cluster</c>.</summary>
    [<Literal>]
    let private IndicBrokenCluster = 4uy

    /// <summary>What the region walk does at a stage and position of the map.</summary>
    [<Struct; NoEquality; NoComparison>]
    type private RegionMove =
        | TakeLookup
        | TakePause
        | PassStage
        | Close
        | Refuse

    /// <summary>Whether a pause may stand in the reused region: one that runs no function, or a reordering of the Indic shaper, which works
    /// syllable by syllable.</summary>
    /// <param name="pause">The pause.</param>
    /// <returns>True when the pause may stand in the region.</returns>
    let private reusablePause pause =
        match pause with
        | NoPause | ClearSyllables | IndicInitialReordering | IndicFinalReordering -> true
        | KhmerSetupSyllables | KhmerReorder | IndicSetupSyllables | ClearSubstitutionFlags | UseSetupSyllables | UseRecordRphf | UseRecordPref
        | UseReorder | MyanmarSetupSyllables | MyanmarReorder | ArabicRecordStch | ArabicFallbackShape | ArabicReleaseAction -> false

    /// <summary>Whether no stage from one on has a pause that runs a function, so that its lookups can run as one window after the region.</summary>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="stage">The first stage.</param>
    /// <returns>True when none has.</returns>
    [<TailCall>]
    let rec private noFunctionFrom (stages: ImmutableArray<StageMap>) stage =
        match stage >= stages.Length with
        | true -> true
        | false ->
            match PauseKind.runsFunction stages[stage].Pause with
            | true -> false
            | false -> noFunctionFrom stages (stage + 1)

    /// <summary>Ends the region at a lookup that is not confined to one syllable, when the lookups from it on can run as one window.</summary>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="stage">The stage of the lookup.</param>
    /// <returns>The move.</returns>
    let private closeOrRefuse (stages: ImmutableArray<StageMap>) stage =
        match noFunctionFrom stages stage with
        | true -> Close
        | false -> Refuse

    /// <summary>What the region walk does at a stage and a position of the map.</summary>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="stage">The stage.</param>
    /// <param name="position">The position of the next lookup.</param>
    /// <returns>The move.</returns>
    let private regionMove (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) stage position =
        match stage >= stages.Length with
        | true -> Close
        | false ->
            match position < stages[stage].LastLookup with
            | true ->
                match lookups[position].PerSyllable && not lookups[position].Random with
                | true -> TakeLookup
                | false -> closeOrRefuse stages stage
            | false ->
                match PauseKind.runsFunction stages[stage].Pause, reusablePause stages[stage].Pause with
                | false, _ -> PassStage
                | true, true -> TakePause
                | true, false -> Refuse

    /// <summary>The window of the lookups gathered since the last pause, or nothing when there are none.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="first">The position of the first lookup gathered.</param>
    /// <param name="position">The position after the last.</param>
    /// <param name="steps">The steps so far, newest first.</param>
    /// <returns>The steps with the window put first.</returns>
    let private closeWindow (plan: OtShapePlan) (font: Font) first position (steps: RegionStep list) =
        match position > first with
        | true -> ApplyWindow (first, Engine.gsubWindowOf plan font first position) :: steps
        | false -> steps

    /// <summary>Walks the map from a stage after the setup of the syllables, gathering the steps of the reused region up to the first lookup
    /// that is not confined to one syllable or the end of the map: the lookups between two pauses make one window.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="stage">The next stage.</param>
    /// <param name="first">The position of the first lookup of the window being gathered.</param>
    /// <param name="position">The position of the next lookup.</param>
    /// <param name="steps">The steps so far, newest first.</param>
    /// <returns>The steps in order and the position after the region, or none when the region cannot be cut.</returns>
    [<TailCall>]
    let rec private regionFrom (plan: OtShapePlan) (font: Font) (stages: ImmutableArray<StageMap>) stage first position (steps: RegionStep list) =
        match regionMove plan.Map.GsubLookups stages stage position with
        | TakeLookup -> regionFrom plan font stages stage first (position + 1) steps
        | TakePause -> regionFrom plan font stages (stage + 1) position position (RunPause stages[stage].Pause :: closeWindow plan font first position steps)
        | PassStage -> regionFrom plan font stages (stage + 1) first position steps
        | Close -> ValueSome struct (List.rev (closeWindow plan font first position steps), position)
        | Refuse -> ValueNone

    /// <summary>Finds the stage whose pause sets up the syllables of the Indic shaper.</summary>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="stage">The next stage.</param>
    /// <returns>The stage, or none.</returns>
    [<TailCall>]
    let rec private setupStageFrom (stages: ImmutableArray<StageMap>) stage =
        match stage >= stages.Length with
        | true -> ValueNone
        | false ->
            match stages[stage].Pause with
            | IndicSetupSyllables -> ValueSome stage
            | NoPause | ClearSyllables | KhmerSetupSyllables | KhmerReorder | IndicInitialReordering | IndicFinalReordering | ClearSubstitutionFlags
            | UseSetupSyllables | UseRecordRphf | UseRecordPref | UseReorder | MyanmarSetupSyllables | MyanmarReorder | ArabicRecordStch
            | ArabicFallbackShape | ArabicReleaseAction -> setupStageFrom stages (stage + 1)

    /// <summary>Counts the stages before one whose pause runs a function: the index of the window that stage's lookups belong to.</summary>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="last">The stage.</param>
    /// <param name="stage">The next stage counted.</param>
    /// <param name="count">The count so far.</param>
    /// <returns>The count.</returns>
    [<TailCall>]
    let rec private functionPausesBefore (stages: ImmutableArray<StageMap>) last stage count =
        match stage >= last with
        | true -> count
        | false -> functionPausesBefore stages last (stage + 1) (count + (match PauseKind.runsFunction stages[stage].Pause with | true -> 1 | false -> 0))

    /// <summary>The reuse program of an Indic plan, from the stage that sets up its syllables.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="setup">The stage that sets up the syllables.</param>
    /// <returns>The program, or none when the region cannot be cut or holds nothing.</returns>
    let private programFrom (plan: OtShapePlan) (font: Font) setup =
        let stages = plan.Map.GsubStages
        match regionFrom plan font stages (setup + 1) stages[setup].LastLookup stages[setup].LastLookup [] with
        | ValueNone -> ValueNone
        | ValueSome struct ([], _) -> ValueNone
        | ValueSome struct (steps, finish) ->
            let total = stages[stages.Length - 1].LastLookup
            ValueSome
                { SetupStage = setup
                  SetupWindow = functionPausesBefore stages setup 0 0
                  Steps = ImmutableArray.CreateRange steps
                  Tail = (match finish < total with | true -> ValueSome (Engine.gsubWindowOf plan font finish total) | false -> ValueNone)
                  Table = SyllableTable() }

    /// <summary>The reuse program of an Indic plan whose GSUB has pauses, no AAT substitution and no lookup that skips base glyphs, so that a
    /// glyph that is neither a mark nor a ligature nor a default ignorable stops every iterator.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The program, or none.</returns>
    let private indicProgram (plan: OtShapePlan) (font: Font) =
        let skipsBases = Seq.exists (fun (lookup: System.Lazy<LookupAccel<SubstSubtable>>) -> lookup.Value.Props &&& IgnoreBaseGlyphs <> 0u) font.Ot.Gsub.Value.Lookups
        match plan.ApplyMorx || not plan.Map.HasPauses || skipsBases with
        | true -> ValueNone
        | false ->
            match setupStageFrom plan.Map.GsubStages 0 with
            | ValueNone -> ValueNone
            | ValueSome setup -> programFrom plan font setup

    /// <summary>The reuse program of a plan, made with the plan: only the Indic shaper's plans have one.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The program, or none.</returns>
    let program (plan: OtShapePlan) (font: Font) =
        match plan.Shaper.Kind with
        | IndicShaper -> indicProgram plan font
        | DefaultShaper | DumberShaper | ArabicShaper | ThaiShaper | HangulShaper | HebrewShaper | KhmerShaper | MyanmarShaper | MyanmarZawgyiShaper
        | UseShaper -> ValueNone

    /// <summary>Whether a line may be rebuilt from its units: it succeeded, runs left to right with monotone clusters, has no positions yet,
    /// does not ask for the unsafe-to-concat flag (whose failed matches mark the next syllable), and neither the font nor the plan carries
    /// functions the caller replaced.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The line after the setup of its syllables.</param>
    /// <returns>True when the line may be rebuilt.</returns>
    let private lineTakes (plan: OtShapePlan) (font: Font) (set: WorkingSet) =
        let leftToRight =
            match set.SegmentProperties.Direction with
            | Ltr -> true
            | Invalid | Rtl | Ttb | Btt -> false
        let monotone =
            match set.ClusterLevel with
            | MonotoneGraphemes | MonotoneCharacters -> true
            | Characters | Graphemes -> false
        set.Successful && leftToRight && monotone && not set.HavePositions && set.Infos.Length > 0
        && set.Flags &&& BufferFlags.ProduceUnsafeToConcat = BufferFlags.Default
        && ValueOption.isNone font.Funcs && ValueOption.isNone plan.Unicode

    /// <summary>The lookups after the region, on a line the region was applied to.</summary>
    /// <param name="program">The plan's reuse program.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The line after the region.</param>
    /// <returns>The substituted set.</returns>
    let private afterRegion (program: SyllableProgram) (covered: ImmutableArray<ImmutableArray<uint64>>) (font: Font) (set: WorkingSet) =
        match program.Tail with
        | ValueNone -> set
        | ValueSome tail -> OtLayout.substituteWindowOwnedSet covered font tail set

    /// <summary>Runs the GSUB pass of a plan with a reuse program on a working set: the stages up to the setup of the syllables as always, then
    /// the line rebuilt from its units, or the region run on the line while it learns its units, or the region the ordinary way, and the lookups
    /// after the region. A rebuilt line is said to be rebuilt, since its operations are an upper bound: the caller shapes it again from its
    /// input when it runs out of them, the one case in which the bound would change its glyphs.</summary>
    /// <param name="program">The plan's reuse program.</param>
    /// <param name="shapePlan">The shape plan, which holds the windows and the glyph sets.</param>
    /// <param name="plan">The ot plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, whose arrays this call made.</param>
    /// <returns>The substituted set, and whether the line was rebuilt.</returns>
    let substitute (program: SyllableProgram) (shapePlan: ShapePlan) (plan: OtShapePlan) (font: Font) (set: WorkingSet) =
        let covered = shapePlan.GsubLookupCovered
        let ready = OtLayout.substituteWindowedTo plan font covered shapePlan.GsubWindows program.SetupStage set
        let line =
            match lineTakes plan font ready with
            | false -> LineOrdinary
            | true ->
                SyllableReuseKernel.reuseLine program.Table ready.Infos ready.Props ready.ScratchFlags ready.Flags ready.ClusterLevel (byte ready.Serial)
                    ready.MaxOps ready.MaxLen IndicBrokenCluster
        match line with
        | LineOrdinary -> struct (OtLayout.substituteWindowedFrom plan font covered shapePlan.GsubWindows (program.SetupStage + 1) (program.SetupWindow + 1) ready, false)
        | LineLearning harvest -> struct (afterRegion program covered font (SyllableLearning.learn program plan font covered harvest ready), false)
        | LineRebuilt replay ->
            let rebuilt =
                { ready with
                    Infos = replay.ReplayInfos
                    Props = replay.ReplayProps
                    Positions = ImmutableArray.Empty
                    Serial = uint32 replay.ReplaySerial
                    MaxOps = replay.ReplayMaxOps
                    ScratchFlags = replay.ReplayScratch }
            struct (afterRegion program covered font rebuilt, true)
