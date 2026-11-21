namespace Farfbuzz

open System

/// <summary>A CFF INDEX: its count field size, count and offset size.</summary>
[<Struct>]
type internal CffIndex =
    { Start: int
      Count: uint32
      OffSize: int
      CountSize: int }

/// <summary>CFF INDEX reading and sanitising, <c>CFFIndex</c>.</summary>
[<RequireQualifiedAccess>]
module internal CffIndex =
    /// <summary>The largest offset size.</summary>
    [<Literal>]
    let private MaxOffSize = 4

    /// <summary>The CFF1 count field size.</summary>
    [<Literal>]
    let private CountSize = 2

    /// <summary>Reads the count field.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The index offset.</param>
    /// <param name="countSize">2 for CFF1, 4 for CFF2.</param>
    /// <returns>The count.</returns>
    let private countAt (table: ReadOnlySpan<byte>) (offset) (countSize) =
        match countSize with
        | 2 -> uint32 (Binary.u16 table offset)
        | _ -> Binary.u32 table offset

    /// <summary>Reads an offset byte by byte.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="baseOffset">The first offset byte.</param>
    /// <param name="offSize">The offset size.</param>
    /// <param name="byteIndex">The next byte.</param>
    /// <param name="value">The value so far.</param>
    /// <returns>The offset.</returns>
    [<TailCall>]
    let rec private readOffsetBytes (table: ReadOnlySpan<byte>) (baseOffset) (offSize) (byteIndex) (value) =
        match byteIndex < offSize with
        | false -> value
        | true -> readOffsetBytes table baseOffset offSize (byteIndex + 1) ((value <<< 8) ||| uint32 (Binary.u8 table (baseOffset + byteIndex)))

    /// <summary>Reads one offset of an arbitrary size.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="start">The index start.</param>
    /// <param name="countSize">The count field size.</param>
    /// <param name="offSize">The offset size.</param>
    /// <param name="index">The offset index.</param>
    /// <returns>The offset value.</returns>
    let private offsetAt (table: ReadOnlySpan<byte>) (start) (countSize) (offSize) (index) =
        readOffsetBytes table (start + countSize + 1 + index * offSize) offSize 0 0u

    /// <summary>Sanitizes the body of a CFF INDEX after its count field.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The index offset.</param>
    /// <param name="countSize">2 for CFF1, 4 for CFF2.</param>
    /// <param name="count">The item count.</param>
    /// <param name="offSize">The offset size.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeBody (table: ReadOnlySpan<byte>) (offset) (countSize) (count) (offSize) (ops) : int voption =
        match offSize >= 1 && offSize <= MaxOffSize with
        | false -> ValueNone
        | true ->
            match Sanitize.array table.Length (int64 offset + int64 countSize + 1L) (count + 1u) (uint32 offSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                let dataBase = offset + countSize + offSize * int (count + 1u)
                let last = offsetAt table offset countSize offSize (int count)
                Sanitize.range table.Length (int64 dataBase) last remaining

    /// <summary>Sanitizes a CFF INDEX, <c>CFFIndex::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The index offset.</param>
    /// <param name="countSize">2 for CFF1, 4 for CFF2.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) (offset) (countSize) (ops) : int voption =
        match Sanitize.fits table.Length (int64 offset) countSize with
        | false -> ValueNone
        | true ->
            let count = countAt table offset countSize
            match count with
            | 0u -> ValueSome ops
            | UInt32.MaxValue -> ValueNone
            | _ ->
                match Sanitize.fits table.Length (int64 offset + int64 countSize) 1 with
                | false -> ValueNone
                | true -> sanitizeBody table offset countSize count (int (Binary.u8 table (offset + countSize))) ops

    /// <summary>Reads a CFF INDEX header.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The index offset.</param>
    /// <param name="countSize">2 for CFF1, 4 for CFF2.</param>
    /// <returns>The index.</returns>
    let read (table: ReadOnlySpan<byte>) (offset) (countSize) : CffIndex =
        let count = countAt table offset countSize
        let offSize = match count with | 0u -> 0 | _ -> int (Binary.u8 table (offset + countSize))
        { Start = offset; Count = count; OffSize = offSize; CountSize = countSize }

    /// <summary>Returns one INDEX item's offset and length, <c>CFFIndex::operator[]</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="index">The index.</param>
    /// <param name="itemIndex">The item index.</param>
    /// <returns>The data offset and length, empty when absent.</returns>
    let item (table: ReadOnlySpan<byte>) (index: CffIndex) (itemIndex) : struct (int * int) =
        match itemIndex >= index.Count with
        | true -> struct (0, 0)
        | false ->
            let first = offsetAt table index.Start index.CountSize index.OffSize (int itemIndex)
            let second = offsetAt table index.Start index.CountSize index.OffSize (int itemIndex + 1)
            let last = offsetAt table index.Start index.CountSize index.OffSize (int index.Count)
            match second < first || second > last with
            | true -> struct (0, 0)
            | false ->
                let dataBase = index.Start + index.CountSize + index.OffSize * int (index.Count + 1u)
                struct (dataBase + int first, int (second - first))

    /// <summary>Returns the INDEX's byte size, saturating as <c>CFFIndex::get_size</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="index">The index.</param>
    /// <returns>The size.</returns>
    let size (table: ReadOnlySpan<byte>) (index: CffIndex) =
        match index.Count with
        | 0u -> uint32 index.CountSize
        | _ ->
            let last = offsetAt table index.Start index.CountSize index.OffSize (int index.Count)
            let total = int64 index.CountSize + 1L + int64 index.OffSize * int64 (index.Count + 1u) + int64 last - 1L
            match total > int64 UInt32.MaxValue with
            | true -> UInt32.MaxValue
            | false -> uint32 total

    /// <summary>The INDEX as the bounds interpreter reads a subroutine INDEX.</summary>
    /// <param name="index">The INDEX.</param>
    /// <returns>The same fields in the interpreter's struct.</returns>
    let subrs (index: CffIndex) =
        { SubrStart = index.Start; SubrCount = index.Count; SubrOffSize = index.OffSize; SubrCountSize = index.CountSize }
