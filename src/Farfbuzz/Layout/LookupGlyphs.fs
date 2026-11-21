namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The glyphs the subtables of a lookup can start a match at, and the glyph set of each lookup of a face's table, built on
/// first use and kept with the face: they depend on the font's tables and the lookup, not on the plan's features.</summary>
[<RequireQualifiedAccess>]
module internal LookupGlyphs =
    /// <summary>The glyphs a coverage can answer for: the glyphs of its array, or every glyph of every range that is
    /// not backwards, up to the last glyph id a face can have. A coverage that is not in order answers for fewer glyphs than
    /// the iterator of <c>Coverage.glyphs</c> lists or for more, so the set is a superset of what the search finds.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The glyphs, with repeats.</returns>
    let rec private coverageMembers coverage =
        match coverage with
        | CoverageNull -> ImmutableArray<uint32>.Empty
        | CoverageDirect (_, _, source) -> coverageMembers source
        | CoverageGlyphs glyphs -> glyphs
        | CoverageRanges ranges ->
            Sealed.flatten (
                Sealed.map
                    (fun (range: RangeRecord) ->
                        let last = min range.Last 0xFFFFu
                        match range.First <= last with
                        | true -> Sealed.init (int (last - range.First) + 1) (fun offset -> range.First + uint32 offset)
                        | false -> ImmutableArray<uint32>.Empty)
                    (ranges.AsSpan()))

    /// <summary>The glyphs that can start a match of a context subtable of either table: the coverage of the first glyph, which
    /// for the third format is the first input coverage. What the nested lookups can touch is not needed, a nested lookup
    /// running only after an outer match.</summary>
    /// <param name="rules">The context rules.</param>
    /// <returns>The glyphs that can start a match.</returns>
    let private contextStartGlyphs (rules: ContextRules) =
        match rules with
        | GlyphRules (coverage, _) -> coverageMembers coverage
        | ClassRules (coverage, _, _, _) -> coverageMembers coverage
        | CoverageRule (coverages, _) ->
            match coverages.IsEmpty with
            | true -> ImmutableArray.Empty
            | false -> coverageMembers coverages[0]

    /// <summary>The glyphs that can start a match of a chained-context subtable of either table.</summary>
    /// <param name="rules">The chained-context rules.</param>
    /// <returns>The glyphs that can start a match.</returns>
    let private chainStartGlyphs (rules: ChainRules) =
        match rules with
        | ChainGlyphRules (coverage, _) -> coverageMembers coverage
        | ChainClassRules (coverage, _, _, _, _, _) -> coverageMembers coverage
        | ChainCoverageRule (_, input, _, _) ->
            match input.IsEmpty with
            | true -> ImmutableArray.Empty
            | false -> coverageMembers input[0]

    /// <summary>The glyphs that can start a match of a GSUB subtable, the glyphs the run must hold for the subtable to
    /// change it: the coverage of a single, multiple or alternate subtable, the coverage of the first component of a
    /// ligature subtable, and none for a pending subtable, which <c>GsubApply.trySubtable</c> never lets match. The match
    /// has no wildcard, so a subtable type that starts to apply must be named here before the build passes.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The glyphs that can start a match.</returns>
    let subst (subtable: SubstSubtable) =
        match subtable with
        | SingleDelta (coverage, _, _) -> coverageMembers coverage
        | SingleList (coverage, _) -> coverageMembers coverage
        | Multiple (coverage, _) -> coverageMembers coverage
        | Alternate (coverage, _) -> coverageMembers coverage
        | LigatureSubst (coverage, _, _) -> coverageMembers coverage
        | SubstContext rules -> contextStartGlyphs rules
        | SubstChainContext rules -> chainStartGlyphs rules
        | ReverseChainSingle (coverage, _, _, _) -> coverageMembers coverage
        | SubstPending _ -> ImmutableArray.Empty

    /// <summary>The glyphs a GPOS subtable can start a match at: the coverage of a single or pair adjustment subtable, the
    /// coverage of the glyph a cursive subtable attaches, the mark coverage of a mark-to-base, mark-to-ligature or mark-to-mark
    /// subtable, the first coverage of a context or chained-context subtable, and none for a pending subtable, which never matches. The match has no wildcard, so a subtable type that
    /// starts to apply must be named here before the build passes.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The covered glyphs.</returns>
    let pos (subtable: PosSubtable) =
        match subtable with
        | SinglePos (coverage, _, _, _, _, _)
        | PairPosGlyphs (coverage, _, _, _, _, _, _, _, _)
        | PairPosClasses (coverage, _, _, _, _, _, _, _, _, _, _, _, _) -> coverageMembers coverage
        | CursivePos cursive -> coverageMembers cursive.CursiveCoverage
        | MarkBasePos attachment
        | MarkMarkPos attachment -> coverageMembers attachment.MarkCoverage
        | MarkLigPos ligature -> coverageMembers ligature.LigMarkCoverage
        | PosContext rules -> contextStartGlyphs rules
        | PosChainContext rules -> chainStartGlyphs rules
        | PosPending _ -> ImmutableArray<uint32>.Empty

    /// <summary>Collects the glyphs the subtables of a lookup cover, with repeats.</summary>
    /// <param name="glyphsOf">Reads the glyphs of one subtable.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="collected">The glyphs so far.</param>
    /// <returns>The glyphs of every subtable.</returns>
    [<TailCall>]
    let rec private collectSubtables (glyphsOf: 'S -> ImmutableArray<uint32>) (subtables: ImmutableArray<SubtableAccel<'S>>) index (collected: ImmutableArray<uint32>) =
        match index < subtables.Length with
        | false -> collected
        | true ->
            collectSubtables glyphsOf subtables (index + 1) (Sealed.appendSpan (collected.AsSpan()) ((glyphsOf subtables[index].Subtable).AsSpan()))

    /// <summary>Builds the glyph set of every lookup of a table, one lazily built set per lookup index: the glyphs the subtables of
    /// the lookup that can apply cover, empty for a lookup whose subtables are all pending.</summary>
    /// <param name="glyphsOf">Reads the glyphs of one subtable.</param>
    /// <param name="table">The table.</param>
    /// <returns>The lazily built sets, indexed by the lookup's index in the table.</returns>
    let setsOf (glyphsOf: 'S -> ImmutableArray<uint32>) (table: LayoutTable<'S>) =
        Sealed.init table.Lookups.Length (fun lookupIndex ->
            lazy (Sealed.glyphSet (collectSubtables glyphsOf table.Lookups[lookupIndex].Value.Subtables 0 ImmutableArray.Empty)))

    /// <summary>The glyphs a GPOS subtable attaches marks to: the base glyphs of a mark-to-base subtable, the ligatures of a
    /// mark-to-ligature one and the marks of a mark-to-mark one.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The glyphs, none for a subtable that is no mark attachment.</returns>
    let private posTargets (subtable: PosSubtable) =
        match subtable with
        | MarkBasePos attachment
        | MarkMarkPos attachment -> coverageMembers attachment.TargetCoverage
        | MarkLigPos ligature -> coverageMembers ligature.LigatureCoverage
        | SinglePos _
        | PairPosGlyphs _
        | PairPosClasses _
        | CursivePos _
        | PosContext _
        | PosChainContext _
        | PosPending _ -> ImmutableArray<uint32>.Empty

    /// <summary>Whether a GPOS subtable is a mark-to-base, mark-to-ligature or mark-to-mark attachment.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>True for a mark attachment.</returns>
    let private isMarkAttachment (subtable: PosSubtable) =
        match subtable with
        | MarkBasePos _
        | MarkMarkPos _
        | MarkLigPos _ -> true
        | SinglePos _
        | PairPosGlyphs _
        | PairPosClasses _
        | CursivePos _
        | PosContext _
        | PosChainContext _
        | PosPending _ -> false

    /// <summary>The glyphs a lookup's subtables attach marks to, when every subtable of the lookup is a mark attachment: a run with none of
    /// them cannot be attached to, so no visit of the lookup succeeds.</summary>
    /// <param name="table">The GPOS table.</param>
    /// <returns>The lazily built set of each lookup, none for a lookup that has a subtable of another kind or has no subtable.</returns>
    let targetSetsOf (table: LayoutTable<PosSubtable>) =
        Sealed.init table.Lookups.Length (fun lookupIndex ->
            lazy
                (let subtables = table.Lookups[lookupIndex].Value.Subtables
                 match subtables.IsEmpty || not (Seq.forall (fun (accel: SubtableAccel<PosSubtable>) -> isMarkAttachment accel.Subtable) subtables) with
                 | true -> ValueNone
                 | false -> ValueSome (Sealed.glyphSet (collectSubtables posTargets subtables 0 ImmutableArray.Empty))))
