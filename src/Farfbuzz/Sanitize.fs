namespace Farfbuzz

open System

/// <summary>A struct table sanitizer, specialised by the JIT so no span-typed function value is needed.</summary>
type internal ITableSanitizer =
    /// <summary>Sanitizes a table's bytes.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The initial operation budget.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    abstract Sanitize : table: ReadOnlySpan<byte> * ops: int -> int voption

/// <summary>The operation budget and range checks of <c>hb_sanitize_context_t</c>.</summary>
[<RequireQualifiedAccess>]
module internal Sanitize =
    /// <summary>The multiplier turning a table length into an operation budget.</summary>
    [<Literal>]
    let MaxOpsFactor = 64

    /// <summary>The smallest operation budget.</summary>
    [<Literal>]
    let MaxOpsMin = 16384

    /// <summary>The largest operation budget.</summary>
    [<Literal>]
    let MaxOpsMax = 0x3FFFFFFF

    /// <summary>The operation budget for a blob of a given length, <c>start_processing</c>.</summary>
    /// <param name="length">The blob length.</param>
    /// <returns>The budget, clamped and saturated as upstream does.</returns>
    let start (length) =
        let scaled = uint64 (uint32 length) * uint64 MaxOpsFactor
        match scaled > uint64 MaxOpsMax with
        | true -> MaxOpsMax
        | false -> int (max (uint64 MaxOpsMin) scaled)

    /// <summary>Checks that a fixed-size struct fits, <c>check_struct</c>; charges no operations.</summary>
    /// <param name="length">The blob length.</param>
    /// <param name="offset">The struct offset.</param>
    /// <param name="size">The struct size.</param>
    /// <returns>True when the struct lies inside the blob.</returns>
    let inline fits (length: int) (offset: int64) (size: int) =
        offset >= 0L && offset <= int64 length && int64 size <= int64 length - offset

    /// <summary>Checks a byte range and charges its length, <c>check_range</c>.</summary>
    /// <param name="length">The blob length.</param>
    /// <param name="offset">The range offset.</param>
    /// <param name="count">The number of bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the range or the budget fails.</returns>
    let inline range (length: int) (offset: int64) (count: uint32) (ops: int) : int voption =
        match offset >= 0L && offset <= int64 length && int64 count <= int64 length - offset with
        | false -> ValueNone
        | true ->
            let remaining = int64 ops - int64 count
            match remaining > 0L with
            | true -> ValueSome (int remaining)
            | false -> ValueNone

    /// <summary>Checks an array with a 32-bit size product, <c>check_array</c>.</summary>
    /// <param name="length">The blob length.</param>
    /// <param name="offset">The array offset.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="itemSize">The size of one item.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the product overflows or the range fails.</returns>
    let inline array (length: int) (offset: int64) (count: uint32) (itemSize: uint32) (ops: int) : int voption =
        let size = uint64 count * uint64 itemSize
        match size > uint64 UInt32.MaxValue with
        | true -> ValueNone
        | false -> range length offset (uint32 size) ops

    /// <summary>The size of the header of a Device table, three 16-bit fields.</summary>
    [<Literal>]
    let private DeviceHeader = 6

    /// <summary>Sanitizes a Device table, <c>Device::sanitize</c>: the format field must fit, a hinting device
    /// (<c>HintingDevice::sanitize</c>) must hold the delta words its size names, and any other format passes.</summary>
    /// <param name="table">The bytes of the whole table the offset points into.</param>
    /// <param name="offset">The device's offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the device fails its check.</returns>
    let device (table: ReadOnlySpan<byte>) offset ops =
        match fits table.Length offset DeviceHeader with
        | false -> ValueNone
        | true ->
            let at = int offset
            let format = Binary.u16 table (at + 4)
            match format >= 1us && format <= 3us with
            | false -> ValueSome ops
            | true ->
                let startSize = int (Binary.u16 table at)
                let endSize = int (Binary.u16 table (at + 2))
                let size =
                    match startSize > endSize with
                    | true -> DeviceHeader
                    | false -> 2 * (4 + ((endSize - startSize) >>> (4 - int format)))
                range table.Length offset (uint32 size) ops

    /// <summary>Sanitizes a table blob, <c>sanitize_blob</c>: a failure yields the empty blob.</summary>
    /// <param name="check">The table's struct sanitizer.</param>
    /// <param name="blob">The table bytes.</param>
    /// <returns>The blob when it passes, else empty.</returns>
    let table<'C when 'C : struct and 'C :> ITableSanitizer> (check: 'C) (blob: ReadOnlyMemory<byte>) =
        match blob.Length with
        | 0 -> blob
        | length ->
            match check.Sanitize(blob.Span, start length) with
            | ValueSome _ -> blob
            | ValueNone -> ReadOnlyMemory<byte>.Empty
