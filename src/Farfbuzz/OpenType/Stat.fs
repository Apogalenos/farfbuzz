namespace Farfbuzz.OpenType

open System
open Farfbuzz

/// <summary>The sanitized STAT table, read only for its presence.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Stat =
    { Table: ReadOnlyMemory<byte>
      Version: uint32 }

/// <summary>Reading the STAT style-attributes table, <c>OT::STAT</c> (src/hb-ot-stat-table.hh).</summary>
[<RequireQualifiedAccess>]
module internal Stat =
    /// <summary>The size of the STAT header.</summary>
    [<Literal>]
    let private HeaderSize = 20

    /// <summary>The size of one StatAxisRecord.</summary>
    [<Literal>]
    let private AxisRecordSize = 8

    /// <summary>The size of one axis-value offset.</summary>
    [<Literal>]
    let private AxisValueOffsetSize = 2

    /// <summary>The size of one AxisValueRecord of a format-4 value.</summary>
    [<Literal>]
    let private AxisValueRecordSize = 6

    /// <summary>The byte offset of the design-axis count.</summary>
    [<Literal>]
    let private DesignAxisCountOffset = 6

    /// <summary>The byte offset of the design-axes offset.</summary>
    [<Literal>]
    let private DesignAxesOffsetOffset = 8

    /// <summary>The byte offset of the axis-value count.</summary>
    [<Literal>]
    let private AxisValueCountOffset = 12

    /// <summary>The byte offset of the axis-value-offsets offset.</summary>
    [<Literal>]
    let private AxisValueOffsetsOffset = 14

    /// <summary>The empty STAT table, returned when the table is absent or fails to sanitize.</summary>
    let private empty : Stat = { Table = ReadOnlyMemory<byte>.Empty; Version = 0u }

    /// <summary>Sanitizes one AxisValue at an offset, <c>AxisValue::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The value offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeAxisValue (table: ReadOnlySpan<byte>) (at: int) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 at) 2 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table at with
            | 1us ->
                match Sanitize.fits table.Length (int64 at) 12 with
                | true -> ValueSome ops
                | false -> ValueNone
            | 2us ->
                match Sanitize.fits table.Length (int64 at) 20 with
                | true -> ValueSome ops
                | false -> ValueNone
            | 3us ->
                match Sanitize.fits table.Length (int64 at) 16 with
                | true -> ValueSome ops
                | false -> ValueNone
            | 4us ->
                match Sanitize.fits table.Length (int64 at) 8 with
                | false -> ValueNone
                | true ->
                    let axisCount = int (Binary.u16 table (at + 2))
                    Sanitize.array table.Length (int64 (at + 8)) (uint32 axisCount) (uint32 AxisValueRecordSize) ops
            | _ -> ValueSome ops

    /// <summary>Sanitizes the axis-value offsets and their targets, <c>offsetToAxisValueOffsets::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The axis-value-offsets array offset.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="count">The offset count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeAxisValueOffsets (table: ReadOnlySpan<byte>) (at: int) (index: int) (count: int) (ops: int) : int voption =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let valueOffset = int (Binary.u16 table (at + index * AxisValueOffsetSize))
            match valueOffset = 0 with
            | true -> sanitizeAxisValueOffsets table at (index + 1) count ops
            | false ->
                match sanitizeAxisValue table (at + valueOffset) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeAxisValueOffsets table at (index + 1) count remaining

    /// <summary>Sanitizes the design axes and the axis-value offsets, <c>STAT::sanitize</c>'s offset parts.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeOffsets (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        let designAxisCount = int (Binary.u16 table DesignAxisCountOffset)
        let designAxesOffset = int64 (Binary.u32 table DesignAxesOffsetOffset)
        match Sanitize.array table.Length designAxesOffset (uint32 designAxisCount) (uint32 AxisRecordSize) ops with
        | ValueNone -> ValueNone
        | ValueSome remaining ->
            let axisValueCount = int (Binary.u16 table AxisValueCountOffset)
            let axisValueOffsets = int (Binary.u32 table AxisValueOffsetsOffset)
            match Sanitize.array table.Length (int64 axisValueOffsets) (uint32 axisValueCount) (uint32 AxisValueOffsetSize) remaining with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeAxisValueOffsets table axisValueOffsets 0 axisValueCount remaining

    /// <summary>Sanitizes a STAT table, <c>STAT::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        match Sanitize.fits table.Length 0 HeaderSize with
        | false -> ValueNone
        | true ->
            match Binary.u16 table 0 with
            | 1us ->
                match Binary.u16 table 2 > 0us with
                | false -> ValueNone
                | true -> sanitizeOffsets table ops
            | _ -> ValueNone

    /// <summary>The struct form of the STAT sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Loads and sanitizes the STAT table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The table, empty when absent or failing.</returns>
    let load (face: Face) : Stat =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Stat)
        match table.Length with
        | 0 -> empty
        | _ -> { Table = table; Version = Binary.u32 table.Span 0 }

    /// <summary>Whether the table has data, <c>STAT::has_data</c>.</summary>
    /// <param name="stat">The table.</param>
    /// <returns>True when the version is not zero.</returns>
    let hasData (stat: Stat) = stat.Version <> 0u
