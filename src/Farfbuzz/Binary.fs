namespace Farfbuzz

open System
open System.Buffers.Binary

/// <summary>The result of comparing two OpenType values, as <c>hb_bsearch_impl</c> uses it.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal Order =
    | Before
    | Equal
    | After

/// <summary>A struct comparer over fixed-size records in a byte span, specialised by the JIT.</summary>
type internal IRecordComparer =
    /// <summary>Compares the key with the record at an index.</summary>
    /// <param name="data">The records, as bytes.</param>
    /// <param name="index">The record index.</param>
    /// <returns>A negative number when the key is before the record, positive when after, zero when equal.</returns>
    abstract Compare : data: ReadOnlySpan<byte> * index: int -> int

/// <summary>Patterns shared by the binary reader and the table directories.</summary>
[<AutoOpen>]
module internal BinaryPatterns =
    /// <summary>Classifies a comparison result as <see cref="Order"/>.</summary>
    /// <param name="comparison">The result of a three-way comparison.</param>
    /// <returns>Before for negative, After for positive, Equal for zero.</returns>
    let inline (|Ordered|) (comparison) : Order =
        match comparison with
        | comparison when comparison < 0 -> Before
        | comparison when comparison > 0 -> After
        | _ -> Equal

/// <summary>Total big-endian reads over <see cref="ReadOnlySpan{T}"/>, returning the Null-object value outside it.</summary>
[<RequireQualifiedAccess>]
module internal Binary =
    /// <summary>Reads a big-endian byte; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The byte, or 0.</returns>
    let inline u8 (data: ReadOnlySpan<byte>) (offset) =
        if uint64 (uint32 offset) + 1UL <= uint64 data.Length then data[offset] else 0uy

    /// <summary>Reads a big-endian uint16; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline u16 (data: ReadOnlySpan<byte>) (offset) =
        if uint64 (uint32 offset) + 2UL <= uint64 data.Length then
            BinaryPrimitives.ReadUInt16BigEndian(data.Slice(offset, 2))
        else 0us

    /// <summary>Reads a big-endian int16; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline i16 (data: ReadOnlySpan<byte>) (offset) =
        int16 (u16 data offset)

    /// <summary>Reads a big-endian 24-bit unsigned value; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline u24 (data: ReadOnlySpan<byte>) (offset) =
        if uint64 (uint32 offset) + 3UL <= uint64 data.Length then
            (uint32 (u8 data offset) <<< 16) ||| (uint32 (u8 data (offset + 1)) <<< 8) ||| uint32 (u8 data (offset + 2))
        else 0u

    /// <summary>Reads a big-endian uint32; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline u32 (data: ReadOnlySpan<byte>) (offset) =
        if uint64 (uint32 offset) + 4UL <= uint64 data.Length then
            BinaryPrimitives.ReadUInt32BigEndian(data.Slice(offset, 4))
        else 0u

    /// <summary>Reads an OpenType tag; returns the empty tag outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The tag.</returns>
    let inline tag (data: ReadOnlySpan<byte>) (offset) : Tag =
        LanguagePrimitives.UInt32WithMeasure (u32 data offset)

    /// <summary>Reads a big-endian uint16, distinguishing absence from a zero value.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or none outside the span.</returns>
    let inline tryU16 (data: ReadOnlySpan<byte>) (offset) : uint16 voption =
        if uint64 (uint32 offset) + 2UL <= uint64 data.Length then
            ValueSome (BinaryPrimitives.ReadUInt16BigEndian(data.Slice(offset, 2)))
        else ValueNone

    /// <summary>Reads a big-endian uint32, distinguishing absence from a zero value.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or none outside the span.</returns>
    let inline tryU32 (data: ReadOnlySpan<byte>) (offset) : uint32 voption =
        if uint64 (uint32 offset) + 4UL <= uint64 data.Length then
            ValueSome (BinaryPrimitives.ReadUInt32BigEndian(data.Slice(offset, 4)))
        else ValueNone

    /// <summary>Port of <c>hb_bsearch_impl</c>: the index of a record comparing Equal, else none.</summary>
    /// <param name="comparer">The struct comparer.</param>
    /// <param name="data">The records, as bytes.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec bsearch<'C when 'C : struct and 'C :> IRecordComparer>
            (comparer: 'C) (data: ReadOnlySpan<byte>) (lo) (hi) : int voption =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match comparer.Compare(data, mid) with
            | Ordered Before -> bsearch comparer data lo (mid - 1)
            | Ordered After -> bsearch comparer data (mid + 1) hi
            | Ordered Equal -> ValueSome mid

    /// <summary>Returns the first index comparing Equal while scanning upwards, as <c>lfind</c>.</summary>
    /// <param name="comparer">The struct comparer.</param>
    /// <param name="data">The records, as bytes.</param>
    /// <param name="index">The next candidate index.</param>
    /// <param name="count">The number of records.</param>
    /// <returns>The first matching index, or none.</returns>
    [<TailCall>]
    let rec lfind<'C when 'C : struct and 'C :> IRecordComparer>
            (comparer: 'C) (data: ReadOnlySpan<byte>) (index) (count) : int voption =
        match index < count with
        | false -> ValueNone
        | true ->
            match comparer.Compare(data, index) with
            | Ordered Equal -> ValueSome index
            | Ordered Before | Ordered After -> lfind comparer data (index + 1) count

/// <summary>The readers of <see cref="Binary"/> as calls, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline a function of an internal module there.</summary>
[<RequireQualifiedAccess>]
module internal BinaryCalls =
    /// <summary>Reads a byte; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let u8 (data: ReadOnlySpan<byte>) (offset) = Binary.u8 data offset

    /// <summary>Reads a big-endian uint16; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let u16 (data: ReadOnlySpan<byte>) (offset) = Binary.u16 data offset

    /// <summary>Reads a big-endian int16; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let i16 (data: ReadOnlySpan<byte>) (offset) = Binary.i16 data offset

    /// <summary>Reads a big-endian uint24; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let u24 (data: ReadOnlySpan<byte>) (offset) = Binary.u24 data offset

    /// <summary>Reads a big-endian uint32; returns 0 outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let u32 (data: ReadOnlySpan<byte>) (offset) = Binary.u32 data offset

    /// <summary>Reads an OpenType tag; returns the empty tag outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The tag.</returns>
    let tag (data: ReadOnlySpan<byte>) (offset) = Binary.tag data offset

    /// <summary>Reads a big-endian uint16, none outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or none.</returns>
    let tryU16 (data: ReadOnlySpan<byte>) (offset) = Binary.tryU16 data offset

    /// <summary>Reads a big-endian uint32, none outside the span.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or none.</returns>
    let tryU32 (data: ReadOnlySpan<byte>) (offset) = Binary.tryU32 data offset
