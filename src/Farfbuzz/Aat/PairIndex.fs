namespace Farfbuzz.Aat

open System
open Farfbuzz

/// <summary>Reading a <see cref="PairIndex"/>, the compiled pair table of a kern or kerx format 0 subtable.</summary>
[<RequireQualifiedAccess>]
module internal PairLookup =
    /// <summary>Compiles the pairs of a sorted table, or declines when they are not strictly increasing.</summary>
    /// <param name="keys">The keys, the left glyph in the high half and the right glyph in the low half.</param>
    /// <param name="values">The values.</param>
    /// <returns>The index, or none.</returns>
    let build (keys: ReadOnlySpan<uint32>) (values: ReadOnlySpan<int16>) = Sealed.pairIndexOf keys values

    /// <summary>Compiles the pairs of a table that is not strictly increasing into an index that answers as the binary search over the
    /// table does.</summary>
    /// <param name="keys">The keys, the left glyph in the high half and the right glyph in the low half.</param>
    /// <param name="values">The values.</param>
    /// <returns>The index.</returns>
    let buildSearched (keys: ReadOnlySpan<uint32>) (values: ReadOnlySpan<int16>) = Sealed.pairIndexOfSearched keys values

    /// <summary>Compiles the pairs of a kern or kerx format 0 table into an index that answers as the binary search over the table does:
    /// a table whose keys strictly increase compiles as itself, any other table by the search for each of its keys.</summary>
    /// <param name="keys">The keys, the left glyph in the high half and the right glyph in the low half.</param>
    /// <param name="values">The values.</param>
    /// <returns>The index.</returns>
    let compile (keys: ReadOnlySpan<uint32>) (values: ReadOnlySpan<int16>) =
        match build keys values with
        | ValueSome index -> ValueSome index
        | ValueNone -> buildSearched keys values

    /// <summary>Whether some pair starts with a glyph.</summary>
    /// <param name="index">The index.</param>
    /// <param name="left">The glyph.</param>
    /// <returns>True when the glyph is the left glyph of a pair.</returns>
    let hasLeft (index: PairIndex) (left: uint32) =
        let word = int (left >>> 6)
        word < index.Lefts.Length && (index.Lefts[word] >>> int (left &&& 63u)) &&& 1UL <> 0UL

    /// <summary>Probes from a slot for a key.</summary>
    /// <param name="slots">The slots.</param>
    /// <param name="key">The key.</param>
    /// <param name="at">The slot to look at.</param>
    /// <returns>The value, zero when the key is absent.</returns>
    [<TailCall>]
    let rec private probe (slots: ReadOnlySpan<uint64>) (key: uint32) (at: int) =
        let slot = slots[at]
        match slot with
        | 0UL -> 0
        | _ ->
            match uint32 (slot >>> 16) = key with
            | true -> int (int16 (uint16 slot))
            | false -> probe slots key ((at + 1) &&& (slots.Length - 1))

    /// <summary>The kerning of a pair.</summary>
    /// <param name="index">The index.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning, zero when the pair is absent.</returns>
    let find (index: PairIndex) (left: uint32) (right: uint32) =
        match (left ||| right) > 0xFFFFu with
        | true -> 0
        | false ->
            let key = (left <<< 16) ||| right
            probe (index.Slots.AsSpan()) key (int ((key * Sealed.KernPairHash) >>> index.ShiftBits))
