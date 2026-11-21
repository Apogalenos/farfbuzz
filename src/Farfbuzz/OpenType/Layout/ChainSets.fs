namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The coverages of a chained-context subtable of the third format as dense sets of glyphs, built with the face: a glyph is a
/// member by one bit test, where <c>Coverage.index</c> searches a sorted array.</summary>
[<NoEquality; NoComparison>]
type internal ChainSets =
    { FirstSet: ImmutableArray<uint64> // the glyphs the first input coverage holds
      BacktrackSets: ImmutableArray<ImmutableArray<uint64>>
      InputSets: ImmutableArray<ImmutableArray<uint64>>
      LookaheadSets: ImmutableArray<ImmutableArray<uint64>> }

/// <summary>Dense glyph sets of the coverages of chained-context subtables.</summary>
[<RequireQualifiedAccess>]
module internal ChainSets =
    /// <summary>Whether a glyph is in a dense set.</summary>
    /// <param name="set">The words of the set.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>True when its bit is set.</returns>
    let inline contains (set: ImmutableArray<uint64>) (glyph: uint32) =
        let word = int (glyph >>> 6)
        word < set.Length && (set[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL

    /// <summary>The dense set of one coverage, none for a coverage whose order a binary search cannot be told from a scan.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The words, or none.</returns>
    let rec private setOf coverage =
        match coverage with
        | CoverageNull -> ValueSome ImmutableArray<uint64>.Empty
        | CoverageDirect (_, _, source) -> setOf source
        | CoverageGlyphs glyphs ->
            let set = Sealed.denseOfGlyphs (glyphs.AsSpan())
            match set with
            | ValueSome words -> ValueSome words
            | ValueNone -> ValueNone
        | CoverageRanges ranges ->
            let spans = Sealed.map (fun (range: RangeRecord) -> { CodepointRange.First = range.First; Last = range.Last }) (ranges.AsSpan())
            let set = Sealed.denseOfRanges (spans.AsSpan())
            match set with
            | ValueSome words -> ValueSome words
            | ValueNone -> ValueNone

    /// <summary>The dense sets of a list of coverages, none when one of them has no dense set.</summary>
    /// <param name="coverages">The coverages.</param>
    /// <returns>The sets, or none.</returns>
    let private setsOf (coverages: ImmutableArray<Coverage>) =
        let sets = Sealed.map setOf (coverages.AsSpan())
        match Seq.forall ValueOption.isSome sets with
        | true -> ValueSome (Sealed.map ValueOption.get (sets.AsSpan()))
        | false -> ValueNone

    /// <summary>Compiles one subtable of a lookup: a chained-context subtable of the third format with at least one input coverage.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The sets, or none for any other subtable and for one whose coverages are not in order.</returns>
    let ofSubtable (subtable: SubtableAccel<SubstSubtable>) =
        match subtable.Subtable with
        | SubstChainContext (ChainCoverageRule (backtrack, input, lookahead, _)) when input.Length > 0 ->
            match setsOf backtrack, setsOf input, setsOf lookahead with
            | ValueSome backtrackSets, ValueSome inputSets, ValueSome lookaheadSets ->
                ValueSome { FirstSet = inputSets[0]; BacktrackSets = backtrackSets; InputSets = inputSets; LookaheadSets = lookaheadSets }
            | _ -> ValueNone
        | SubstChainContext (ChainGlyphRules _) -> ValueNone
        | SubstChainContext (ChainClassRules _) -> ValueNone
        | SubstChainContext (ChainCoverageRule _) -> ValueNone
        | SingleDelta _ -> ValueNone
        | SingleList _ -> ValueNone
        | Multiple _ -> ValueNone
        | Alternate _ -> ValueNone
        | LigatureSubst _ -> ValueNone
        | SubstContext _ -> ValueNone
        | ReverseChainSingle _ -> ValueNone
        | SubstPending _ -> ValueNone

    /// <summary>Compiles the subtables of a lookup, each its own entry in the order of the lookup.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <returns>The sets of each subtable, none for one that has none.</returns>
    let ofLookup (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) =
        Sealed.map ofSubtable (subtables.AsSpan())
