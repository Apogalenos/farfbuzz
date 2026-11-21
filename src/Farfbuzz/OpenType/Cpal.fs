namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The CPAL colour palette table: the counts and the offsets of its arrays, empty when the table is absent or fails the sanitizer.</summary>
[<NoEquality; NoComparison>]
type internal Cpal =
    { Table: ReadOnlyMemory<byte>
      Version: int
      ColorCount: int
      PaletteCount: int
      ColorRecordCount: int
      ColorRecordsOffset: int64
      FlagsOffset: int64
      PaletteLabelsOffset: int64
      ColorLabelsOffset: int64 }

/// <summary>Reading the CPAL table, <c>src/OT/Color/CPAL/CPAL.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Cpal =
    /// <summary>The size of the version 0 header, through the colour records offset.</summary>
    [<Literal>]
    let private HeaderSize = 12

    /// <summary>The size of the version 1 tail: the palette types, palette labels and colour labels offsets.</summary>
    [<Literal>]
    let private TailSize = 12

    /// <summary>The size of one colour record, a BGRA value.</summary>
    [<Literal>]
    let private ColorSize = 4

    /// <summary>The size of one name id or palette index.</summary>
    [<Literal>]
    let private NameIdSize = 2

    /// <summary>The size of one palette flags value.</summary>
    [<Literal>]
    let private FlagsSize = 4

    /// <summary>The name id that means no name, <c>HB_OT_NAME_ID_INVALID</c>.</summary>
    [<Literal>]
    let private NameIdInvalid = 0xFFFFus

    /// <summary>The table of a face that has none.</summary>
    let empty =
        { Table = ReadOnlyMemory.Empty
          Version = 0
          ColorCount = 0
          PaletteCount = 0
          ColorRecordCount = 0
          ColorRecordsOffset = 0L
          FlagsOffset = 0L
          PaletteLabelsOffset = 0L
          ColorLabelsOffset = 0L }

    /// <summary>Whether an array of a count of values lies inside a table, <c>UnsizedArrayOf::sanitize</c> behind a non-null offset.</summary>
    /// <param name="length">The table length.</param>
    /// <param name="offset">The offset of the array.</param>
    /// <param name="count">The number of values.</param>
    /// <param name="size">The size of one value.</param>
    /// <returns>True when the whole array is inside the table.</returns>
    let private fits (length: int) (offset: int64) (count: int) (size: int) =
        offset + int64 count * int64 size <= int64 length

    /// <summary>Reads the table, <c>CPAL::sanitize</c>: a table whose arrays leave it is no table.</summary>
    /// <param name="table">The table bytes.</param>
    /// <returns>The table, or the empty one.</returns>
    let load (table: ReadOnlyMemory<byte>) =
        let span = table.Span
        let length = span.Length
        let version = int (Binary.u16 span 0)
        let colors = int (Binary.u16 span 2)
        let palettes = int (Binary.u16 span 4)
        let records = int (Binary.u16 span 6)
        let recordsOffset = int64 (Binary.u32 span 8)
        let tailAt = HeaderSize + palettes * NameIdSize
        let flags = int64 (Binary.u32 span tailAt)
        let paletteLabels = int64 (Binary.u32 span (tailAt + 4))
        let colorLabels = int64 (Binary.u32 span (tailAt + 8))
        let optional (offset: int64) count size = offset = 0L || fits length offset count size
        let versionOne =
            version = 0
            || (tailAt + TailSize <= length
                && optional flags palettes FlagsSize
                && optional paletteLabels palettes NameIdSize
                && optional colorLabels colors NameIdSize)
        match length >= HeaderSize && fits length recordsOffset records ColorSize && tailAt <= length && versionOne with
        | false -> empty
        | true ->
            { Table = table
              Version = version
              ColorCount = colors
              PaletteCount = palettes
              ColorRecordCount = records
              ColorRecordsOffset = recordsOffset
              FlagsOffset = (match version with | 0 -> 0L | _ -> flags)
              PaletteLabelsOffset = (match version with | 0 -> 0L | _ -> paletteLabels)
              ColorLabelsOffset = (match version with | 0 -> 0L | _ -> colorLabels) }

    /// <summary>The flags of a palette, <c>hb_ot_color_palette_get_flags</c>: none without a flags array, and for an index past the palettes.</summary>
    /// <param name="index">The palette index.</param>
    /// <param name="cpal">The table.</param>
    /// <returns>The flags.</returns>
    let paletteFlags (index: int) (cpal: Cpal) =
        match cpal.FlagsOffset = 0L || index >= cpal.PaletteCount with
        | true -> 0u
        | false -> Binary.u32 cpal.Table.Span (int cpal.FlagsOffset + index * FlagsSize)

    /// <summary>The name id of a palette, <c>hb_ot_color_palette_get_name_id</c>: invalid without a label array, zero past the palettes.</summary>
    /// <param name="index">The palette index.</param>
    /// <param name="cpal">The table.</param>
    /// <returns>The name id.</returns>
    let paletteNameId (index: int) (cpal: Cpal) =
        match cpal.PaletteLabelsOffset = 0L, index >= cpal.PaletteCount with
        | true, _ -> NameIdInvalid
        | false, true -> 0us
        | false, false -> Binary.u16 cpal.Table.Span (int cpal.PaletteLabelsOffset + index * NameIdSize)

    /// <summary>The name id of a colour entry, <c>hb_ot_color_palette_color_get_name_id</c>: invalid without a label array, zero past the colours.</summary>
    /// <param name="index">The colour index.</param>
    /// <param name="cpal">The table.</param>
    /// <returns>The name id.</returns>
    let colorNameId (index: int) (cpal: Cpal) =
        match cpal.ColorLabelsOffset = 0L, index >= cpal.ColorCount with
        | true, _ -> NameIdInvalid
        | false, true -> 0us
        | false, false -> Binary.u16 cpal.Table.Span (int cpal.ColorLabelsOffset + index * NameIdSize)

    /// <summary>The colours of a palette from an offset, at most a count, <c>hb_ot_color_palette_get_colors</c>: the number of colours in each palette
    /// whatever was asked (zero for a palette that does not exist), and the colours the records hold from the offset, each as the four bytes B, G, R, A
    /// read as one big-endian value.</summary>
    /// <param name="index">The palette index.</param>
    /// <param name="start">The first colour wanted.</param>
    /// <param name="count">The most colours wanted.</param>
    /// <param name="cpal">The table.</param>
    /// <returns>The colours in each palette and the colours asked for.</returns>
    let paletteColors (index: int) (start: int) (count: int) (cpal: Cpal) =
        match index >= cpal.PaletteCount with
        | true -> struct (0, ImmutableArray<uint32>.Empty)
        | false ->
            let first = int (Binary.u16 cpal.Table.Span (HeaderSize + index * NameIdSize))
            let inPalette = Math.Min(cpal.ColorCount, Math.Max(0, cpal.ColorRecordCount - first))
            let available = Math.Max(0, inPalette - start)
            let written = Math.Min(available, count)
            let colors =
                Sealed.init written (fun position ->
                    Binary.u32 cpal.Table.Span (int cpal.ColorRecordsOffset + (first + start + position) * ColorSize))
            struct (cpal.ColorCount, colors)

/// <summary>Whether a face has colour glyph data of each kind, read from the headers of the tables.</summary>
[<RequireQualifiedAccess>]
module internal ColorTables =
    /// <summary>Whether the COLR table has version 0 layers, <c>hb_ot_color_has_layers</c>: it has base glyph records.</summary>
    /// <param name="colr">The COLR table bytes.</param>
    /// <returns>True when it has base glyph records.</returns>
    let hasLayers (colr: ReadOnlyMemory<byte>) =
        Binary.u16 colr.Span 2 <> 0us

    /// <summary>Whether the face has PNG glyph images, <c>hb_ot_color_has_png</c>: a CBDT table of a non-zero major version, or an sbix table of a
    /// non-zero version.</summary>
    /// <param name="cbdt">The CBDT table bytes.</param>
    /// <param name="sbix">The sbix table bytes.</param>
    /// <returns>True when either has data.</returns>
    let hasPng (cbdt: ReadOnlyMemory<byte>) (sbix: ReadOnlyMemory<byte>) =
        Binary.u16 cbdt.Span 0 <> 0us || Binary.u16 sbix.Span 0 <> 0us

    /// <summary>Whether the face has SVG glyph documents, <c>hb_ot_color_has_svg</c>: an SVG table whose document list offset is not zero.</summary>
    /// <param name="svg">The SVG table bytes.</param>
    /// <returns>True when it has a document list.</returns>
    let hasSvg (svg: ReadOnlyMemory<byte>) =
        Binary.u32 svg.Span 2 <> 0u
