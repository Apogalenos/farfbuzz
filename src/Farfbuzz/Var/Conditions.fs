namespace Farfbuzz.Var

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The condition instancer: the GDEF store scalars and the normalized coordinates, <c>ItemVarStoreInstancer</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Instancer =
    { Gdef: StoreInstance
      Coords: NormalizedCoords }

/// <summary>The condition formats of FeatureVariations, <c>Condition::evaluate</c> and <c>ConditionSet::evaluate</c>.</summary>
[<RequireQualifiedAccess>]
module internal Conditions =
    /// <summary>The nesting depth the sanitizer allows, <c>HB_MAX_NESTING_LEVEL</c>.</summary>
    [<Literal>]
    let MaxNestingLevel = 64

    /// <summary>The no-variation sentinel of <c>VarIdx::NO_VARIATION</c>.</summary>
    [<Literal>]
    let private NoVariation = 0xFFFFFFFFu

    /// <summary>The byte offset of the count field of an And or Or condition.</summary>
    [<Literal>]
    let private AndOrCountOffset = 2

    /// <summary>The byte offset of the first Offset24 child of an And or Or condition.</summary>
    [<Literal>]
    let private AndOrOffsetsOffset = 3

    /// <summary>The width of one Offset24 child offset.</summary>
    [<Literal>]
    let private Offset24Size = 3

    /// <summary>The byte offset of the Offset24 child of a Negate condition.</summary>
    [<Literal>]
    let private NegateOffsetField = 2

    /// <summary>The byte offset of the first Offset32 child of a ConditionSet.</summary>
    [<Literal>]
    let private ConditionSetOffsetsOffset = 2

    /// <summary>The width of one Offset32 child offset.</summary>
    [<Literal>]
    let private Offset32Size = 4

    /// <summary>Evaluates a format-1 axis-range condition, <c>ConditionAxisRange::evaluate</c>.</summary>
    /// <param name="table">The FeatureVariations bytes.</param>
    /// <param name="at">The condition offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <returns>Whether the coordinate lies inside the inclusive filter range.</returns>
    let private axisRange (table: ReadOnlyMemory<byte>) (at: int) (instancer: Instancer) : bool =
        let span = table.Span
        let axisIndex = int (Binary.u16 span (at + 2))
        let coord = NormalizedCoords.at (instancer.Coords.AsSpan()) axisIndex
        let minValue = int (Binary.i16 span (at + 4))
        let maxValue = int (Binary.i16 span (at + 6))
        minValue <= coord && coord <= maxValue

    /// <summary>Evaluates a format-2 value condition, <c>ConditionValue::evaluate</c>.</summary>
    /// <param name="table">The FeatureVariations bytes.</param>
    /// <param name="at">The condition offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <returns>Whether the default value plus its variation delta is positive.</returns>
    let private valueCondition (table: ReadOnlyMemory<byte>) (at: int) (instancer: Instancer) : bool =
        let span = table.Span
        let defaultValue = double (int (Binary.i16 span (at + 2)))
        let varIdx = Binary.u32 span (at + 4)
        let delta =
            match instancer.Coords.HasNonZero && varIdx <> NoVariation with
            | false -> 0.0
            | true -> ItemVariationStore.deltaDouble instancer.Gdef varIdx
        defaultValue + delta > 0.0

    /// <summary>Evaluates one condition at a byte offset, depth-bounded, <c>Condition::evaluate</c>.</summary>
    /// <param name="table">The FeatureVariations bytes.</param>
    /// <param name="at">The condition offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="depth">The remaining nesting depth; zero answers false.</param>
    /// <returns>The condition's truth.</returns>
    let rec evaluate (table: ReadOnlyMemory<byte>) (at: int) (instancer: Instancer) (depth: int) : bool =
        match depth with
        | 0 -> false
        | _ ->
            let span = table.Span
            match int (Binary.u16 span at) with
            | 0 -> true
            | 1 -> axisRange table at instancer
            | 2 -> valueCondition table at instancer
            | 3 -> evaluateAnd table at instancer (depth - 1) 0 (int (Binary.u8 span (at + AndOrCountOffset)))
            | 4 -> evaluateOr table at instancer (depth - 1) 0 (int (Binary.u8 span (at + AndOrCountOffset)))
            | 5 ->
                match Binary.u24 span (at + NegateOffsetField) with
                | 0u -> false
                | childOffset -> not (evaluate table (at + int childOffset) instancer (depth - 1))
            | _ -> false

    /// <summary>Evaluates the children of an And condition, short-circuiting on the first false child.</summary>
    /// <param name="table">The FeatureVariations bytes.</param>
    /// <param name="at">The And condition offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="depth">The remaining nesting depth.</param>
    /// <param name="index">The next child.</param>
    /// <param name="count">The child count.</param>
    /// <returns>Whether every child holds.</returns>
    and [<TailCall>] private evaluateAnd (table: ReadOnlyMemory<byte>) (at: int) (instancer: Instancer) (depth: int) (index: int) (count: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = table.Span
            match Binary.u24 span (at + AndOrOffsetsOffset + index * Offset24Size) with
            | 0u -> evaluateAnd table at instancer depth (index + 1) count
            | childOffset ->
                match evaluate table (at + int childOffset) instancer depth with
                | true -> evaluateAnd table at instancer depth (index + 1) count
                | false -> false

    /// <summary>Evaluates the children of an Or condition, short-circuiting on the first true child.</summary>
    /// <param name="table">The FeatureVariations bytes.</param>
    /// <param name="at">The Or condition offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="depth">The remaining nesting depth.</param>
    /// <param name="index">The next child.</param>
    /// <param name="count">The child count.</param>
    /// <returns>Whether some child holds.</returns>
    and [<TailCall>] private evaluateOr (table: ReadOnlyMemory<byte>) (at: int) (instancer: Instancer) (depth: int) (index: int) (count: int) : bool =
        match index >= count with
        | true -> false
        | false ->
            let span = table.Span
            match Binary.u24 span (at + AndOrOffsetsOffset + index * Offset24Size) with
            | 0u -> true
            | childOffset ->
                match evaluate table (at + int childOffset) instancer depth with
                | false -> evaluateOr table at instancer depth (index + 1) count
                | true -> true

    /// <summary>Iterates a ConditionSet's conditions, short-circuiting on the first false one.</summary>
    /// <param name="table">The FeatureVariations bytes.</param>
    /// <param name="at">The ConditionSet offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="count">The condition count.</param>
    /// <param name="index">The next condition.</param>
    /// <returns>Whether every condition holds.</returns>
    [<TailCall>]
    let rec private evaluateSetChildren (table: ReadOnlyMemory<byte>) (at: int) (instancer: Instancer) (count: int) (index: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = table.Span
            match Binary.u32 span (at + ConditionSetOffsetsOffset + index * Offset32Size) with
            | 0u -> evaluateSetChildren table at instancer count (index + 1)
            | childOffset ->
                match evaluate table (at + int childOffset) instancer MaxNestingLevel with
                | true -> evaluateSetChildren table at instancer count (index + 1)
                | false -> false

    /// <summary>Evaluates a ConditionSet, every condition of it, <c>ConditionSet::evaluate</c>.</summary>
    /// <param name="table">The FeatureVariations bytes.</param>
    /// <param name="at">The ConditionSet offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <returns>Whether every condition holds.</returns>
    let evaluateSet (table: ReadOnlyMemory<byte>) (at: int) (instancer: Instancer) : bool =
        let span = table.Span
        evaluateSetChildren table at instancer (int (Binary.u16 span at)) 0
