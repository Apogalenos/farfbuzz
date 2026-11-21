namespace Farfbuzz

open System

/// <summary>The VORG vertical-origin table: a default origin and the per-glyph overrides.</summary>
[<Struct>]
type internal Vorg =
    { Present: bool
      Table: ReadOnlyMemory<byte>
      DefaultY: int16
      Count: int
      LongGlyph: bool }

/// <summary>Reading the VORG table, <c>hb-ot-vorg-table.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Vorg =
    /// <summary>The size of a version-1 VORG header.</summary>
    [<Literal>]
    let private ShortHeaderSize = 8

    /// <summary>The size of a version-2 VORG header.</summary>
    [<Literal>]
    let private LongHeaderSize = 9

    /// <summary>The size of one version-1 per-glyph origin record.</summary>
    [<Literal>]
    let private ShortRecordSize = 4

    /// <summary>The size of one version-2 per-glyph origin record.</summary>
    [<Literal>]
    let private LongRecordSize = 5

    /// <summary>The offset of the default vertical origin.</summary>
    [<Literal>]
    let private DefaultYOffset = 4

    /// <summary>The offset of the count of per-glyph origins.</summary>
    [<Literal>]
    let private CountOffset = 6

    /// <summary>Reads and sanitizes the VORG table, <c>VORG::has_data</c> and <c>VORGFormat1_2::sanitize</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The table.</returns>
    let load (face: Face) : Vorg =
        let table = Face.referenceTable face Tags.Vorg
        let span = table.Span
        match Binary.u16 span 0 with
        | 1us ->
            let present = table.Length >= ShortHeaderSize && ShortHeaderSize + int (Binary.u16 span CountOffset) * ShortRecordSize <= table.Length
            { Present = present
              Table = table
              DefaultY = if present then Binary.i16 span DefaultYOffset else 0s
              Count = if present then int (Binary.u16 span CountOffset) else 0
              LongGlyph = false }
        | 2us ->
            let present = table.Length >= LongHeaderSize && LongHeaderSize + int (Binary.u24 span CountOffset) * LongRecordSize <= table.Length
            { Present = present
              Table = table
              DefaultY = if present then Binary.i16 span DefaultYOffset else 0s
              Count = if present then int (Binary.u24 span CountOffset) else 0
              LongGlyph = true }
        | _ -> { Present = false; Table = table; DefaultY = 0s; Count = 0; LongGlyph = false }

    /// <summary>Compares a glyph with one sorted VORG record.</summary>
    [<Struct>]
    type private RecordComparer =
        { Glyph: uint32
          HeaderSize: int
          RecordSize: int
          LongGlyph: bool }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let at = this.HeaderSize + index * this.RecordSize
                let value = if this.LongGlyph then Binary.u24 data at else uint32 (Binary.u16 data at)
                match uint32 this.Glyph > value, uint32 this.Glyph < value with
                | true, _ -> 1
                | false, true -> -1
                | false, false -> 0

    /// <summary>Returns the unscaled vertical origin of a glyph, <c>VORGFormat1_2::get_y_origin</c>.</summary>
    /// <param name="vorg">The table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The unscaled origin, defaulting to the table's default when the glyph has no override.</returns>
    let yOrigin (vorg: Vorg) (glyph: uint32) : int =
        match vorg.Present with
        | false -> 0
        | true ->
            let headerSize = if vorg.LongGlyph then LongHeaderSize else ShortHeaderSize
            let recordSize = if vorg.LongGlyph then LongRecordSize else ShortRecordSize
            let comparer = { RecordComparer.Glyph = glyph; HeaderSize = headerSize; RecordSize = recordSize; LongGlyph = vorg.LongGlyph }
            match Binary.bsearch comparer vorg.Table.Span 0 (vorg.Count - 1) with
            | ValueSome index -> int (Binary.i16 vorg.Table.Span (headerSize + index * recordSize + recordSize - 2))
            | ValueNone -> int vorg.DefaultY
