namespace Farfbuzz

open System

/// <summary>The parts of an hhea or vhea table the shaper and the metrics need.</summary>
[<Struct>]
type internal Hea =
    { Present: bool
      Ascender: int16
      Descender: int16
      LineGap: int16
      CaretSlopeRise: int16
      CaretSlopeRun: int16
      CaretOffset: int16
      LongMetricCount: uint32 }

/// <summary>The hhea/vhea tables and their 32-bit HHEA/VHEA twins.</summary>
[<RequireQualifiedAccess>]
module internal Hhea =
    /// <summary>The size of a lower hea table.</summary>
    [<Literal>]
    let private LowerSize = 36

    /// <summary>The size of an upper hea table.</summary>
    [<Literal>]
    let private UpperSize = 38

    /// <summary>The offset of the long-metric count.</summary>
    [<Literal>]
    let private LongMetricOffset = 34

    /// <summary>The offset of caretSlopeRise.</summary>
    [<Literal>]
    let private CaretSlopeRiseOffset = 18

    /// <summary>The offset of caretSlopeRun.</summary>
    [<Literal>]
    let private CaretSlopeRunOffset = 20

    /// <summary>The offset of caretOffset.</summary>
    [<Literal>]
    let private CaretOffsetOffset = 22

    /// <summary>Sanitizes a lower or upper hea table, <c>_hea::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="size">36 for hhea/vhea, 38 for HHEA/VHEA.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeWith (table: ReadOnlySpan<byte>) (size) (ops) : int voption =
        match Sanitize.fits table.Length 0L size with
        | true when Binary.u16 table 0 = 1us -> ValueSome ops
        | true -> ValueNone
        | false -> ValueNone

    /// <summary>The struct form of the hea sanitizer.</summary>
    [<Struct>]
    type private Sanitizer =
        { Upper: bool }
        interface ITableSanitizer with
            member this.Sanitize (table, ops) =
                sanitizeWith table (match this.Upper with | true -> UpperSize | false -> LowerSize) ops

    /// <summary>Reads a sanitized hea table.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="upper">Whether it is the 32-bit upper table.</param>
    /// <returns>The metrics.</returns>
    let private read (table: ReadOnlyMemory<byte>) (upper) : Hea =
        let span = table.Span
        { Present = Binary.u16 span 0 <> 0us
          Ascender = Binary.i16 span 4
          Descender = Binary.i16 span 6
          LineGap = Binary.i16 span 8
          CaretSlopeRise = Binary.i16 span CaretSlopeRiseOffset
          CaretSlopeRun = Binary.i16 span CaretSlopeRunOffset
          CaretOffset = Binary.i16 span CaretOffsetOffset
          LongMetricCount =
            match upper with
            | true -> Binary.u32 span LongMetricOffset
            | false -> uint32 (Binary.u16 span LongMetricOffset) }

    /// <summary>Reads one hea table's long-metric count without the dual-table fallback.</summary>
    /// <param name="face">The face.</param>
    /// <param name="tag">The hea table tag.</param>
    /// <param name="upper">Whether this is the 32-bit uppercase table.</param>
    /// <returns>The long-metric count, or 0 when the table does not sanitize.</returns>
    let longMetricCount (face: Face) (tag: Tag) (upper) =
        let table = Sanitize.table { Upper = upper } (Face.referenceTable face tag)
        match upper with
        | true -> Binary.u32 table.Span LongMetricOffset
        | false -> uint32 (Binary.u16 table.Span LongMetricOffset)

    /// <summary>Loads a hea table, preferring the upper table when it has data, <c>hb_dual_table_t</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="lower">The lower table tag.</param>
    /// <param name="upper">The upper table tag.</param>
    /// <returns>The selected metrics.</returns>
    let load (face: Face) (lower: Tag) (upper: Tag) : Hea =
        let lowerTable = Sanitize.table { Upper = false } (Face.referenceTable face lower)
        let upperTable = Sanitize.table { Upper = true } (Face.referenceTable face upper)
        let hasUpper = Binary.u16 upperTable.Span 0 <> 0us
        match hasUpper with
        | true -> read upperTable true
        | false -> read lowerTable false
