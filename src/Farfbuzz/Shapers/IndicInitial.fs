namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The Indic shaper's initial reordering, <c>initial_reordering_indic</c> (src/hb-ot-shaper-indic.cc:421-996): the
/// positions of the consonants the font gives below-base or post-base forms, the dotted circles of the broken syllables,
/// then for each syllable its base, the positions of everything around it, the stable sort by position, the cluster merges
/// and the masks of the features that follow.</summary>
[<RequireQualifiedAccess>]
module internal IndicInitial =
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

    /// <summary>Gives a glyph a position.</summary>
    /// <param name="position">The position.</param>
    /// <param name="row">The glyph.</param>
    /// <returns>The glyph at the position.</returns>
    let private withPosition position (row: GlyphRow) = { row with Props = { row.Props with ShaperAuxiliary = position } }

    /// <summary>Adds the bits of a mask to the mask of a glyph.</summary>
    /// <param name="mask">The bits.</param>
    /// <param name="row">The glyph.</param>
    /// <returns>The glyph with the bits set.</returns>
    let private withMask mask (row: GlyphRow) = { row with Info = { row.Info with Mask = row.Info.Mask ||| mask } }

    /// <summary>Applies a change to the glyphs of a range of rows.</summary>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="change">The change.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the glyphs of the range changed.</returns>
    let private changeRange start finish (change: GlyphRow -> GlyphRow) (rows: ImmutableArray<GlyphRow>) =
        Sealed.mapi (fun index row ->
            match index >= start && index < finish with
            | true -> change row
            | false -> row) (rows.AsSpan())

    /// <summary>Whether any lookup of a feature's stage from an index on would substitute a glyph sequence, <c>would_substitute</c> of
    /// <c>hb_indic_would_substitute_feature_t</c> (src/hb-ot-shaper-indic.cc:98-110), asked of each lookup in turn.</summary>
    /// <param name="face">The face.</param>
    /// <param name="feature">The feature.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next lookup.</param>
    /// <returns>True when a lookup would substitute the sequence.</returns>
    [<TailCall>]
    let rec private anyWouldSubstituteFrom (face: OtFace) (feature: IndicWouldSubstitute) (glyphs: ImmutableArray<uint32>) index =
        match index < feature.Lookups.Length with
        | false -> false
        | true ->
            match LayoutApi.wouldSubstitute face feature.Lookups[index] glyphs feature.ZeroContext with
            | true -> true
            | false -> anyWouldSubstituteFrom face feature glyphs (index + 1)

    /// <summary>Whether any lookup of a feature's stage would substitute a glyph sequence, <c>would_substitute</c> of
    /// <c>hb_indic_would_substitute_feature_t</c> (src/hb-ot-shaper-indic.cc:98-110): from the first lookup the feature's prepared
    /// tester answers, which refuses at one digest test a sequence no lookup can start with.</summary>
    /// <param name="face">The face.</param>
    /// <param name="feature">The feature.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next lookup.</param>
    /// <returns>True when a lookup would substitute the sequence.</returns>
    let internal anyWouldSubstitute (face: OtFace) (feature: IndicWouldSubstitute) (glyphs: ImmutableArray<uint32>) index =
        match index with
        | 0 -> LayoutApi.wouldSubstituteAny (LayoutApi.testerFor face feature.Prepared feature.Lookups feature.ZeroContext) glyphs
        | _ -> anyWouldSubstituteFrom face feature glyphs index

    /// <summary>Whether a feature would substitute a pair of glyphs in either order, the two tests of
    /// <c>consonant_position_from_face</c> (src/hb-ot-shaper-indic.cc:344-374).</summary>
    /// <param name="face">The face.</param>
    /// <param name="feature">The feature.</param>
    /// <param name="newOrder">The virama and the consonant.</param>
    /// <param name="oldOrder">The consonant and the virama.</param>
    /// <returns>True when the feature substitutes the virama and the consonant, or the consonant and the virama.</returns>
    let private wouldSubstituteEither face feature (newOrder: ImmutableArray<uint32>) (oldOrder: ImmutableArray<uint32>) =
        anyWouldSubstitute face feature newOrder 0 || anyWouldSubstitute face feature oldOrder 0

    /// <summary>Finds where a consonant goes according to the font from the two orders of the virama and the consonant, which
    /// <see cref="consonantPosition"/> builds, <c>consonant_position_from_face</c> (src/hb-ot-shaper-indic.cc:344-374).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="face">The face.</param>
    /// <param name="newOrder">The virama and the consonant.</param>
    /// <param name="oldOrder">The consonant and the virama.</param>
    /// <returns>The position.</returns>
    let private positionOfPairs (plan: IndicPlan) face (newOrder: ImmutableArray<uint32>) (oldOrder: ImmutableArray<uint32>) =
        match wouldSubstituteEither face plan.BlwfFeature newOrder oldOrder || wouldSubstituteEither face plan.VatuFeature newOrder oldOrder with
        | true -> IndicPosition.PosBelowC
        | false ->
            match wouldSubstituteEither face plan.PstfFeature newOrder oldOrder || wouldSubstituteEither face plan.PrefFeature newOrder oldOrder with
            | true -> IndicPosition.PosPostC
            | false -> IndicPosition.PosBaseC

    /// <summary>The consonant positions a plan found for one face and one virama: a pure function of the plan's features, the face's GSUB
    /// table and the pair of glyphs, so an entry never changes once written.</summary>
    type private PositionMemo =
        { Source: Lazy<LayoutTable<SubstSubtable>>
          Virama: uint32
          Blwf: Memo<obj>
          Vatu: Memo<obj>
          Pstf: Memo<obj>
          Pref: Memo<obj>
          Pages: IntPages }

    /// <summary>Whether a memo was made for a face, a virama and the features of a plan; the cell of a feature stands for the feature, so
    /// a plan made from another by changing a feature is not served the other's positions.</summary>
    /// <param name="memo">The memo.</param>
    /// <param name="plan">The plan data.</param>
    /// <param name="face">The face.</param>
    /// <param name="virama">The virama glyph.</param>
    /// <returns>True when the memo serves them.</returns>
    let private memoServes (memo: PositionMemo) (plan: IndicPlan) (face: OtFace) (virama: uint32) =
        obj.ReferenceEquals(memo.Source, face.Gsub)
        && memo.Virama = virama
        && obj.ReferenceEquals(memo.Blwf, plan.BlwfFeature.Prepared)
        && obj.ReferenceEquals(memo.Vatu, plan.VatuFeature.Prepared)
        && obj.ReferenceEquals(memo.Pstf, plan.PstfFeature.Prepared)
        && obj.ReferenceEquals(memo.Pref, plan.PrefFeature.Prepared)

    /// <summary>The memo of consonant positions of a plan for a face and a virama, made the first time they are asked for and kept in the
    /// plan's cell. A cell that holds the memo of another face or virama is left alone and the asker has none.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="face">The face.</param>
    /// <param name="virama">The virama glyph.</param>
    /// <returns>The memo, or none.</returns>
    let private memoFor (plan: IndicPlan) (face: OtFace) (virama: uint32) =
        match plan.Positions.TryGet () with
        | ValueSome (:? PositionMemo as kept) when memoServes kept plan face virama -> ValueSome kept
        | ValueSome _ -> ValueNone
        | ValueNone ->
            let created =
                { Source = face.Gsub
                  Virama = virama
                  Blwf = plan.BlwfFeature.Prepared
                  Vatu = plan.VatuFeature.Prepared
                  Pstf = plan.PstfFeature.Prepared
                  Pref = plan.PrefFeature.Prepared
                  Pages = IntPages(face.Face.GlyphCount) }
            match plan.Positions.Set created with
            | :? PositionMemo as held when memoServes held plan face virama -> ValueSome held
            | _ -> ValueNone

    /// <summary>Finds where a consonant goes according to the font from the two orders of the virama and the consonant, as
    /// <c>consonant_position_from_face</c> does; the answer is kept by consonant for the plan, the face and the virama, so a consonant
    /// the plan has met costs one read.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="face">The face.</param>
    /// <param name="newOrder">The virama and the consonant.</param>
    /// <param name="oldOrder">The consonant and the virama.</param>
    /// <returns>The position.</returns>
    let internal consonantPositionOfPairs (plan: IndicPlan) face (newOrder: ImmutableArray<uint32>) (oldOrder: ImmutableArray<uint32>) =
        match memoFor plan face newOrder[0] with
        | ValueNone -> positionOfPairs plan face newOrder oldOrder
        | ValueSome memo ->
            match memo.Pages.Read newOrder[1] with
            | System.Int32.MinValue ->
                let position = positionOfPairs plan face newOrder oldOrder
                memo.Pages.Write(newOrder[1], int position)
                position
            | held -> byte held

    /// <summary>Finds where a consonant goes according to the font, <c>consonant_position_from_face</c>
    /// (src/hb-ot-shaper-indic.cc:344-374): below the base when the font forms it below, after the base when it forms it as a
    /// post-base form or as a pre-base form, else it can be the base. The old-specification order of the glyphs is the
    /// consonant and the virama, the new one the virama and the consonant, and some fonts copied the old lookups to the new
    /// order, so both are tried.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="face">The face.</param>
    /// <param name="virama">The virama glyph.</param>
    /// <param name="consonant">The consonant glyph.</param>
    /// <returns>The position.</returns>
    let internal consonantPosition (plan: IndicPlan) face (virama: uint32) (consonant: uint32) =
        consonantPositionOfPairs plan face (ImmutableArray.Create(virama, consonant)) (ImmutableArray.Create(consonant, virama))

    /// <summary>Updates the positions of the consonants that could be the base, <c>update_consonant_positions_indic</c>
    /// (src/hb-ot-shaper-indic.cc:421-439): only when the script has a virama the font maps.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the positions updated.</returns>
    let updateConsonantPositions (plan: IndicPlan) (font: Font) (run: GlyphRun) =
        match Indic.viramaGlyph plan font with
        | ValueNone -> run
        | ValueSome virama ->
            let props =
                Sealed.mapi (fun index (props: GlyphProps) ->
                    match props.ShaperAuxiliary = IndicPosition.PosBaseC with
                    | true -> { props with ShaperAuxiliary = consonantPosition plan font.Ot virama run.Infos[index].Codepoint }
                    | false -> props) (run.Props.AsSpan())
            { run with Props = props }

    /// <summary>Makes a Ra, halant and ZWJ at the start of a Kannada syllable the Ra, ZWJ and halant the shaper expects,
    /// for compatibility with the legacy usage (src/hb-ot-shaper-indic.cc:463-474).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the halant and the ZWJ swapped.</returns>
    let private kannadaRaHalantJoiner (environment: Environment) (state: SyllableState) =
        let rows = state.Rows
        match environment.Script = Scripts.Kannada
              && rows.Length >= 3
              && Indic.isOneOf (Indic.flagUnsafe IndicMachine.Ra) rows[0].Props
              && Indic.isOneOf (Indic.flagUnsafe IndicMachine.H) rows[1].Props
              && Indic.isOneOf (Indic.flagUnsafe IndicMachine.ZWJ) rows[2].Props with
        | false -> state
        | true ->
            let merged = SyllableEdit.mergeClusters environment.Level 1 3 state
            { merged with Rows = Sealed.move 2 1 merged.Rows }

    /// <summary>Whether a syllable starts with the glyphs of a reph the font would form: a Ra, a halant and, for the scripts
    /// whose reph is explicit, a ZWJ (src/hb-ot-shaper-indic.cc:512-530).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>True when the syllable may have a reph of the first kind.</returns>
    let private mayHaveReph (plan: IndicPlan) (rows: ImmutableArray<GlyphRow>) =
        match plan.RphfMask <> 0u && rows.Length >= 3 with
        | false -> false
        | true ->
            match plan.Config.RephMode with
            | RephImplicit -> not (Indic.isJoiner rows[2].Props)
            | RephExplicit -> rows[2].Props.ShaperCategory = IndicMachine.ZWJ
            | RephLogRepha -> false

    /// <summary>Whether the font's <c>rphf</c> lookups would form a reph from the first glyphs (src/hb-ot-shaper-indic.cc:524-531).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>True when the lookups would substitute the Ra and the halant, or for an explicit reph the Ra, the halant and the ZWJ.</returns>
    let private rephForms (environment: Environment) (rows: ImmutableArray<GlyphRow>) =
        let plan = environment.Plan
        let explicit = plan.Config.RephMode = RephExplicit
        let third = match explicit with | true -> rows[2].Info.Codepoint | false -> 0u
        let pair = ImmutableArray.Create(rows[0].Info.Codepoint, rows[1].Info.Codepoint)
        match anyWouldSubstitute environment.Face plan.RphfFeature pair 0 with
        | true -> true
        | false ->
            match explicit with
            | false -> false
            | true -> anyWouldSubstitute environment.Face plan.RphfFeature (ImmutableArray.Create(rows[0].Info.Codepoint, rows[1].Info.Codepoint, third)) 0

    /// <summary>Finds the reph at the start of a syllable (src/hb-ot-shaper-indic.cc:512-543): the glyphs after which the base
    /// is searched for, and whether the syllable has a reph.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The limit the base search stops at, the base found so far (the end when none) and whether there is a reph.</returns>
    let private detectReph (environment: Environment) (rows: ImmutableArray<GlyphRow>) =
        match mayHaveReph environment.Plan rows with
        | true ->
            match rephForms environment rows with
            | true -> struct (Indic.skipJoiners rows 2, 0, true)
            | false -> struct (0, rows.Length, false)
        | false ->
            match environment.Plan.Config.RephMode = RephLogRepha && rows[0].Props.ShaperCategory = IndicMachine.Repha with
            | true -> struct (Indic.skipJoiners rows 1, 0, true)
            | false -> struct (0, rows.Length, false)

    /// <summary>Searches for the base from the end of the syllable, the do-while loop of src/hb-ot-shaper-indic.cc:545-585:
    /// the last consonant that has no below-base or post-base form (post-base forms have to follow below-base forms), else
    /// the first consonant after the limit. A ZWJ after a halant stops the search and requests an explicit half form; a ZWJ
    /// before a halant requests a subjoined form, so the search goes on.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="limit">The index the search stops at.</param>
    /// <param name="index">The glyph to examine.</param>
    /// <param name="seenBelow">Whether a below-base consonant has been passed.</param>
    /// <param name="found">The base found so far.</param>
    /// <returns>The base.</returns>
    [<TailCall>]
    let rec private searchBase (rows: ImmutableArray<GlyphRow>) limit index seenBelow found =
        let props = rows[index].Props
        let position = props.ShaperAuxiliary
        let consonant = Indic.isConsonant props
        let isBase = consonant && position <> IndicPosition.PosBelowC && (position <> IndicPosition.PosPostC || seenBelow)
        let stopsAtJoiner =
            not consonant && index > 0 && props.ShaperCategory = IndicMachine.ZWJ && rows[index - 1].Props.ShaperCategory = IndicMachine.H
        let candidate = match consonant with | true -> index | false -> found
        match isBase with
        | true -> index
        | false ->
            match stopsAtJoiner || index <= limit with
            | true -> candidate
            | false -> searchBase rows limit (index - 1) (seenBelow || (consonant && position = IndicPosition.PosBelowC)) candidate

    /// <summary>Finds the base consonant of a syllable and whether it has a reph, the first step of
    /// <c>initial_reordering_consonant_syllable</c> (src/hb-ot-shaper-indic.cc:477-598).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The base, the end of the syllable when there is none, and whether the syllable has a reph that stays one.</returns>
    let private findBase (environment: Environment) (rows: ImmutableArray<GlyphRow>) =
        let struct (limit, initial, hasReph) = detectReph environment rows
        let found = searchBase rows limit (rows.Length - 1) false initial
        match hasReph && found = 0 && limit - found <= 2 with
        | true -> struct (found, false)
        | false -> struct (found, hasReph)

    /// <summary>Gives each glyph before the base at most the position before the consonants, the base its position, and a reph
    /// its own, the "reorder characters" step (src/hb-ot-shaper-indic.cc:631-639).</summary>
    /// <param name="found">The base.</param>
    /// <param name="hasReph">Whether the syllable has a reph.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with their positions set.</returns>
    let private assignPositions found hasReph (rows: ImmutableArray<GlyphRow>) =
        Sealed.mapi (fun index (row: GlyphRow) ->
            let before = match index < found with | true -> min IndicPosition.PosPreC row.Props.ShaperAuxiliary | false -> row.Props.ShaperAuxiliary
            let atBase = match index = found with | true -> IndicPosition.PosBaseC | false -> before
            match hasReph && index = 0 with
            | true -> withPosition IndicPosition.PosRaToBecomeReph row
            | false -> withPosition atBase row) (rows.AsSpan())

    /// <summary>Finds the glyph a halant is moved after, the inner loop of the old-specification halant move
    /// (src/hb-ot-shaper-indic.cc:672-679): the last consonant after it, or for Kannada also a halant.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="disallowDoubleHalants">Whether a halant stops the search too.</param>
    /// <param name="halant">The index of the halant.</param>
    /// <param name="index">The next glyph to examine, going down.</param>
    /// <returns>The index the search stopped at.</returns>
    [<TailCall>]
    let rec private halantTarget (rows: ImmutableArray<GlyphRow>) disallowDoubleHalants halant index =
        match index > halant with
        | false -> index
        | true ->
            match Indic.isConsonant rows[index].Props || (disallowDoubleHalants && rows[index].Props.ShaperCategory = IndicMachine.H) with
            | true -> index
            | false -> halantTarget rows disallowDoubleHalants halant (index - 1)

    /// <summary>Finds the first halant from an index by its raw category.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The first index.</param>
    /// <returns>The index of the halant, or the end of the syllable.</returns>
    [<TailCall>]
    let rec private firstHalant (rows: ImmutableArray<GlyphRow>) index =
        match index < rows.Length && rows[index].Props.ShaperCategory <> IndicMachine.H with
        | true -> firstHalant rows (index + 1)
        | false -> index

    /// <summary>For the old-specification script tags, moves the first post-base halant after the last consonant, except for
    /// Kannada when a halant already follows it (src/hb-ot-shaper-indic.cc:661-689).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the halant moved.</returns>
    let private moveOldSpecHalant (environment: Environment) found (rows: ImmutableArray<GlyphRow>) =
        match environment.Plan.IsOldSpec with
        | false -> rows
        | true ->
            let halant = firstHalant rows (found + 1)
            match halant < rows.Length with
            | false -> rows
            | true ->
                let target = halantTarget rows (environment.Script = Scripts.Kannada) halant (rows.Length - 1)
                match rows[target].Props.ShaperCategory <> IndicMachine.H && target > halant with
                | true -> Sealed.move halant target rows
                | false -> rows

    /// <summary>The categories whose glyphs take the position of the glyph before them, the joiners, the nukta, the register
    /// shifter, the consonant medial and the halant.</summary>
    let private attachedFlags =
        Indic.joinerFlags ||| Indic.flagUnsafe IndicMachine.N ||| Indic.flagUnsafe IndicMachine.RS ||| Indic.flagUnsafe IndicMachine.CM
        ||| Indic.flagUnsafe IndicMachine.H

    /// <summary>Finds the position a halant after a left matra takes: the position of the nearest glyph before it that is not
    /// itself a left matra (src/hb-ot-shaper-indic.cc:699-709).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The index of the halant, then the glyphs before it.</param>
    /// <returns>The position, that of a left matra when every glyph before is one.</returns>
    [<TailCall>]
    let rec private positionBeforeMatras (rows: ImmutableArray<GlyphRow>) index =
        match index > 0 with
        | false -> IndicPosition.PosPreM
        | true ->
            match rows[index - 1].Props.ShaperAuxiliary <> IndicPosition.PosPreM with
            | true -> rows[index - 1].Props.ShaperAuxiliary
            | false -> positionBeforeMatras rows (index - 1)

    /// <summary>The position an attached glyph takes: the last position seen, but a halant that would follow a left matra
    /// does not move with it (src/hb-ot-shaper-indic.cc:694-711).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The index of the glyph.</param>
    /// <param name="lastPosition">The last position seen.</param>
    /// <returns>The position.</returns>
    let private attachedPosition (rows: ImmutableArray<GlyphRow>) index lastPosition =
        match rows[index].Props.ShaperCategory = IndicMachine.H && lastPosition = IndicPosition.PosPreM with
        | true -> positionBeforeMatras rows index
        | false -> lastPosition

    /// <summary>Attaches the miscellaneous marks to the previous glyph so that they move with it, the loop of
    /// src/hb-ot-shaper-indic.cc:691-719: a joiner, nukta, register shifter, consonant medial or halant takes the position of
    /// the last glyph that has one, and a post-base syllable-modifier mark takes that of the matra after it.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="lastPosition">The position of the last glyph that is not attached.</param>
    /// <returns>The rows with the positions set.</returns>
    [<TailCall>]
    let rec private attachMarks (rows: ImmutableArray<GlyphRow>) index lastPosition =
        match index < rows.Length with
        | false -> rows
        | true ->
            let props = rows[index].Props
            match Indic.flagUnsafe props.ShaperCategory &&& attachedFlags <> 0u with
            | true -> attachMarks (Sealed.setItem index (withPosition (attachedPosition rows index lastPosition) rows[index]) rows) (index + 1) lastPosition
            | false ->
                match props.ShaperAuxiliary = IndicPosition.PosSmvd with
                | true -> attachMarks rows (index + 1) lastPosition
                | false ->
                    let followsModifier = props.ShaperCategory = IndicMachine.MPst && index > 0 && rows[index - 1].Props.ShaperCategory = IndicMachine.SM
                    let adjusted = match followsModifier with | true -> Sealed.setItem (index - 1) (withPosition props.ShaperAuxiliary rows[index - 1]) rows | false -> rows
                    attachMarks adjusted (index + 1) props.ShaperAuxiliary

    /// <summary>Lets each post-base consonant own what lies between it and the last consonant or matra before it, the loop of
    /// src/hb-ot-shaper-indic.cc:721-733.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="last">The last consonant or matra.</param>
    /// <returns>The rows with the positions set.</returns>
    [<TailCall>]
    let rec private ownPostBase (rows: ImmutableArray<GlyphRow>) index last =
        match index < rows.Length with
        | false -> rows
        | true ->
            let props = rows[index].Props
            match Indic.isConsonant props with
            | true ->
                let owned =
                    changeRange (last + 1) index (fun row ->
                        match row.Props.ShaperAuxiliary < IndicPosition.PosSmvd with
                        | true -> withPosition props.ShaperAuxiliary row
                        | false -> row) rows
                ownPostBase owned (index + 1) index
            | false ->
                match Indic.flagUnsafe props.ShaperCategory &&& (Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst) <> 0u with
                | true -> ownPostBase rows (index + 1) index
                | false -> ownPostBase rows (index + 1) last

    /// <summary>Finds the base again after the sort, and the first and last left matra before it (src/hb-ot-shaper-indic.cc:745-762).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="first">The first left matra, the end of the syllable when none.</param>
    /// <param name="last">The last left matra, the end of the syllable when none.</param>
    /// <returns>The base, the end of the syllable when none, and the first and last left matra.</returns>
    [<TailCall>]
    let rec private scanLeftMatras (rows: ImmutableArray<GlyphRow>) index first last : struct (int * int * int) =
        match index < rows.Length with
        | false -> struct (rows.Length, first, last)
        | true ->
            let position = rows[index].Props.ShaperAuxiliary
            match position = IndicPosition.PosBaseC with
            | true -> struct (index, first, last)
            | false ->
                match position = IndicPosition.PosPreM with
                | true -> scanLeftMatras rows (index + 1) (match first = rows.Length with | true -> index | false -> first) index
                | false -> scanLeftMatras rows (index + 1) first last

    /// <summary>Reverses back the runs of a flipped sequence of left matras that end in a matra, so that the nuktas and the
    /// other marks after a matra stay after it (src/hb-ot-shaper-indic.cc:769-776).</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="runStart">The start of the run being reversed back.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="last">The last glyph of the sequence.</param>
    /// <returns>The rows with the runs reversed back.</returns>
    [<TailCall>]
    let rec private reverseMatraRuns (rows: ImmutableArray<GlyphRow>) runStart index last =
        match index > last with
        | true -> rows
        | false ->
            match Indic.flagUnsafe rows[index].Props.ShaperCategory &&& (Indic.flagUnsafe IndicMachine.M ||| Indic.flagUnsafe IndicMachine.MPst) <> 0u with
            | true -> reverseMatraRuns (Sealed.reverseRange (rows.AsSpan()) runStart (index + 1)) (index + 1) (index + 1) last
            | false -> reverseMatraRuns rows runStart (index + 1) last

    /// <summary>Flips a sequence of left matras, which the stable sort left in the order of the text, so that the first typed is
    /// the nearest to the base (src/hb-ot-shaper-indic.cc:764-777).</summary>
    /// <param name="first">The first left matra.</param>
    /// <param name="last">The last left matra.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the matras flipped.</returns>
    let private flipLeftMatras first last (rows: ImmutableArray<GlyphRow>) =
        match first < last with
        | false -> rows
        | true -> reverseMatraRuns (Sealed.reverseRange (rows.AsSpan()) first (last + 1)) first first last

    /// <summary>Walks the cycles of the permutation the sort made, marking the glyphs it passes, the inner loop of
    /// src/hb-ot-shaper-indic.cc:831-839: the syllable byte holds the index the glyph had before the sort.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The glyph the walk started at.</param>
    /// <param name="current">The glyph reached.</param>
    /// <param name="low">The lowest index seen.</param>
    /// <param name="high">The highest index seen.</param>
    /// <returns>The rows with the visited glyphs marked, and the lowest and highest index of the cycle.</returns>
    [<TailCall>]
    let rec private walkCycle (rows: ImmutableArray<GlyphRow>) index current low high : struct (ImmutableArray<GlyphRow> * int * int) =
        match current = index with
        | true -> struct (rows, low, high)
        | false ->
            let next = int rows[current].Props.Syllable
            let marked = Sealed.setItem current { rows[current] with Props = { rows[current].Props with Syllable = Visited } } rows
            walkCycle marked index next (min low current) (max high current)

    /// <summary>Merges the clusters of each cycle of the permutation the sort made, from the base on, the loop of
    /// src/hb-ot-shaper-indic.cc:819-841: what moved from after the base to before it is merged with the base's side, what
    /// moved before it is left to the final reordering.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="found">The base.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the clusters merged.</returns>
    [<TailCall>]
    let rec private mergeCycles level found index (state: SyllableState) =
        match index < state.Rows.Length with
        | false -> state
        | true ->
            match state.Rows[index].Props.Syllable = Visited with
            | true -> mergeCycles level found (index + 1) state
            | false ->
                let struct (rows, low, high) = walkCycle state.Rows index (int state.Rows[index].Props.Syllable) index index
                mergeCycles level found (index + 1) (SyllableEdit.mergeClusters level (max found low) (high + 1) { state with Rows = rows })

    /// <summary>Merges the clusters the sort moved glyphs across: the whole of the syllable after the base for the old
    /// specification, where halants are moved around, and else the cycles of the permutation.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The syllable with the clusters merged.</returns>
    let private mergeSorted (environment: Environment) found (state: SyllableState) =
        match environment.Plan.IsOldSpec || state.Rows.Length > MostIndicesForCycles with
        | true -> SyllableEdit.mergeClusters environment.Level found state.Rows.Length state
        | false -> mergeCycles environment.Level found found state

    /// <summary>Sorts the glyphs of a syllable by position, flips the left matras and merges the clusters, the sort step of
    /// <c>initial_reordering_consonant_syllable</c> (src/hb-ot-shaper-indic.cc:736-846): the syllable byte holds the index of
    /// each glyph in the text while it lasts.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="state">The syllable, positions set.</param>
    /// <returns>The sorted syllable and the base found again, the end of the syllable when none.</returns>
    let private sortByPosition (environment: Environment) (state: SyllableState) =
        let syllable = state.Rows[0].Props.Syllable
        let indexed = Sealed.mapi (fun index (row: GlyphRow) -> { row with Props = { row.Props with Syllable = byte index } }) (state.Rows.AsSpan())
        let sorted = Sealed.stableSortBy (fun (row: GlyphRow) -> int row.Props.ShaperAuxiliary) indexed
        let struct (found, first, last) = scanLeftMatras sorted 0 sorted.Length sorted.Length
        let merged = mergeSorted environment found { state with Rows = flipLeftMatras first last sorted }
        let restored = Sealed.mapi (fun _ (row: GlyphRow) -> { row with Props = { row.Props with Syllable = syllable } }) (merged.Rows.AsSpan())
        struct ({ merged with Rows = restored }, found)

    /// <summary>The masks the glyphs before the base take: the half forms and, for the new specification of the scripts whose
    /// below-forms apply to the pre-base too, the below-base forms (src/hb-ot-shaper-indic.cc:851-855).</summary>
    /// <param name="plan">The plan data.</param>
    /// <returns>The mask.</returns>
    let internal preBaseMask (plan: IndicPlan) =
        match plan.IsOldSpec with
        | true -> plan.HalfMask
        | false ->
            match plan.Config.BlwfMode with
            | BlwfPreAndPost -> plan.HalfMask ||| plan.BlwfMask
            | BlwfPostOnly -> plan.HalfMask

    /// <summary>Counts the glyphs at the start of a syllable that have the reph position.</summary>
    /// <param name="rows">The rows.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The number of glyphs.</returns>
    [<TailCall>]
    let rec private rephCount (rows: ImmutableArray<GlyphRow>) index =
        match index < rows.Length && rows[index].Props.ShaperAuxiliary = IndicPosition.PosRaToBecomeReph with
        | true -> rephCount rows (index + 1)
        | false -> index

    /// <summary>Sets the masks of the features applied after the reordering: the reph forms on the reph, the half forms and
    /// perhaps the below forms before the base, and the below, above and post forms after it, "setup masks now"
    /// (src/hb-ot-shaper-indic.cc:848-864).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="found">The base.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the masks set.</returns>
    let private setupFeatureMasks (plan: IndicPlan) found (rows: ImmutableArray<GlyphRow>) =
        let reph = rephCount rows 0
        let before = preBaseMask plan
        let after = plan.BlwfMask ||| plan.AbvfMask ||| plan.PstfMask
        Sealed.mapi (fun index (row: GlyphRow) ->
            let withReph = match index < reph with | true -> withMask plan.RphfMask row | false -> row
            let withBefore = match index < found with | true -> withMask before withReph | false -> withReph
            match index > found with
            | true -> withMask after withBefore
            | false -> withBefore) (rows.AsSpan())

    /// <summary>Gives the Ra and halant before the base the below-base forms for the old-specification Devanagari, the eyelash
    /// Ra unless a ZWJ follows (src/hb-ot-shaper-indic.cc:866-896).</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="found">The base.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the masks set.</returns>
    [<TailCall>]
    let rec private eyelashRa (plan: IndicPlan) found index (rows: ImmutableArray<GlyphRow>) =
        match index + 1 < found with
        | false -> rows
        | true ->
            let isEyelash =
                rows[index].Props.ShaperCategory = IndicMachine.Ra
                && rows[index + 1].Props.ShaperCategory = IndicMachine.H
                && (index + 2 = found || rows[index + 2].Props.ShaperCategory <> IndicMachine.ZWJ)
            match isEyelash with
            | true -> eyelashRa plan found (index + 1) (changeRange index (index + 2) (withMask plan.BlwfMask) rows)
            | false -> eyelashRa plan found (index + 1) rows

    /// <summary>Finds a halant and a Ra the font's <c>pref</c> would substitute after the base, and gives them the mask of the
    /// feature, so that the final reordering can find the pre-base-reordering consonant (src/hb-ot-shaper-indic.cc:898-913).</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the masks set.</returns>
    [<TailCall>]
    let rec private markPref (environment: Environment) index (rows: ImmutableArray<GlyphRow>) =
        match index + 1 < rows.Length with
        | false -> rows
        | true ->
            let pair = ImmutableArray.Create(rows[index].Info.Codepoint, rows[index + 1].Info.Codepoint)
            match anyWouldSubstitute environment.Face environment.Plan.PrefFeature pair 0 with
            | true -> changeRange index (index + 2) (withMask environment.Plan.PrefMask) rows
            | false -> markPref environment (index + 1) rows

    /// <summary>Clears the half-form mask from the glyphs a ZWNJ follows, back to the previous consonant (src/hb-ot-shaper-indic.cc:916-933).</summary>
    /// <param name="half">The mask of the half forms.</param>
    /// <param name="index">The glyph to clear.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the masks cleared.</returns>
    [<TailCall>]
    let rec private clearHalfBack half index (rows: ImmutableArray<GlyphRow>) =
        let cleared = Sealed.setItem index { rows[index] with Info = { rows[index].Info with Mask = rows[index].Info.Mask &&& ~~~half } } rows
        match index > 0 && not (Indic.isConsonant cleared[index].Props) with
        | true -> clearHalfBack half (index - 1) cleared
        | false -> cleared

    /// <summary>Applies the effects of the ZWJ and ZWNJ: a ZWNJ disables the half forms of what precedes it, a ZWJ disables
    /// <c>cjct</c> by being there, since the feature does not skip it.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the masks cleared.</returns>
    [<TailCall>]
    let rec private joinerEffects (plan: IndicPlan) index (rows: ImmutableArray<GlyphRow>) =
        match index < rows.Length with
        | false -> rows
        | true ->
            match Indic.isJoiner rows[index].Props && rows[index].Props.ShaperCategory = IndicMachine.ZWNJ with
            | true -> joinerEffects plan (index + 1) (clearHalfBack plan.HalfMask (index - 1) rows)
            | false -> joinerEffects plan (index + 1) rows

    /// <summary>Finds the masks of a syllable once its glyphs are in place: the eyelash Ra of the old-specification Devanagari,
    /// the <c>pref</c> candidate, and the effects of the joiners.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="found">The base.</param>
    /// <param name="rows">The rows.</param>
    /// <returns>The rows with the masks set.</returns>
    let private setupJoinerAndPrefMasks (environment: Environment) found (rows: ImmutableArray<GlyphRow>) =
        let plan = environment.Plan
        let eyelash =
            match plan.IsOldSpec && environment.Script = Scripts.Devanagari with
            | true -> eyelashRa plan found 0 rows
            | false -> rows
        let prefMarked =
            match plan.PrefMask <> 0u && found + 2 < eyelash.Length with
            | true -> markPref environment (found + 1) eyelash
            | false -> eyelash
        joinerEffects plan 1 prefMarked

    /// <summary>Reorders a consonant syllable, <c>initial_reordering_consonant_syllable</c> (src/hb-ot-shaper-indic.cc:458-934):
    /// the base is found, every glyph gets its position, the glyphs are sorted by position, and the masks of the features
    /// that follow are set. A vowel syllable, a standalone cluster and a broken cluster are reordered the same way, since
    /// vowels and placeholders are treated as consonants.</summary>
    /// <param name="environment">The environment.</param>
    /// <param name="state">The syllable.</param>
    /// <returns>The reordered syllable.</returns>
    let private reorderConsonantSyllable (environment: Environment) (state: SyllableState) =
        let swapped = kannadaRaHalantJoiner environment state
        let struct (found, hasReph) = findBase environment swapped.Rows
        let positioned = assignPositions found hasReph swapped.Rows
        let attached = attachMarks (moveOldSpecHalant environment found positioned) 0 IndicPosition.PosStart
        let owned = ownPostBase attached (found + 1) found
        let struct (sorted, sortedBase) = sortByPosition environment { swapped with Rows = owned }
        let masked = setupFeatureMasks environment.Plan sortedBase sorted.Rows
        { sorted with Rows = setupJoinerAndPrefMasks environment sortedBase masked }

    /// <summary>The rewriter that reorders every syllable, <c>initial_reordering_syllable_indic</c>
    /// (src/hb-ot-shaper-indic.cc:948-971).</summary>
    [<Struct>]
    type private ReorderSyllables =
        val Environment: Environment

        /// <summary>Creates the rewriter.</summary>
        /// <param name="environment">The environment.</param>
        new (environment) = { Environment = environment }

        interface ISegmentRewriter with
            member this.Rewrite rows =
                match IndicMachine.ofSyllable rows[0].Props.Syllable with
                | IndicMachine.IndicSymbolCluster | IndicMachine.IndicNonIndicCluster -> SegmentRewrite.unchanged
                | IndicMachine.IndicConsonantSyllable | IndicMachine.IndicVowelSyllable
                | IndicMachine.IndicStandaloneCluster | IndicMachine.IndicBrokenCluster ->
                    let initial : SyllableState =
                        { Rows = Sealed.ofSpan rows
                          Left = ImmutableArray.Empty
                          Right = ImmutableArray.Empty }
                    let final = reorderConsonantSyllable this.Environment initial
                    let rewrite : SegmentRewrite =
                        { Rows = final.Rows
                          Left = final.Left
                          Right = final.Right }
                    rewrite

    /// <summary>Reorders the syllables of the run, <c>initial_reordering_indic</c> (src/hb-ot-shaper-indic.cc:973-996): the
    /// consonant positions are updated from the font, broken clusters get a dotted circle, then each syllable is reordered.</summary>
    /// <param name="plan">The plan data.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its syllables set.</param>
    /// <returns>The reordered run.</returns>
    let reorder (plan: IndicPlan) (font: Font) (run: GlyphRun) =
        let circled = Indic.insertDottedCircles font (updateConsonantPositions plan font run)
        let environment : Environment =
            { Plan = plan
              Script = circled.SegmentProperties.Script
              Level = circled.ClusterLevel
              Face = font.Ot }
        let struct (infos, props) = Sealed.rewriteSegments (ReorderSyllables(environment)) (circled.Infos.AsSpan()) (circled.Props.AsSpan())
        { circled with Infos = infos; Props = props }
