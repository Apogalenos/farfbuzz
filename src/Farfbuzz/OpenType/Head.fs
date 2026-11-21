namespace Farfbuzz

open System

/// <summary>The <c>head</c> table: units per em and its sanitizer.</summary>
[<RequireQualifiedAccess>]
module internal Head =
    /// <summary>The size of the head table.</summary>
    [<Literal>]
    let private Size = 54

    /// <summary>The offset of the magic number.</summary>
    [<Literal>]
    let private MagicOffset = 12

    /// <summary>The offset of units per em.</summary>
    [<Literal>]
    let private UpemOffset = 18

    /// <summary>The offset of the loca offset format.</summary>
    [<Literal>]
    let private IndexToLocFormatOffset = 50

    /// <summary>The offset of the glyph data format.</summary>
    [<Literal>]
    let private GlyphDataFormatOffset = 52

    /// <summary>The magic number every valid head table carries.</summary>
    [<Literal>]
    let private MagicNumber = 0x5F0F3CF5u

    /// <summary>Sanitizes a head table, <c>head::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) (ops) : int voption =
        match Sanitize.fits table.Length 0L Size with
        | true when Binary.u16 table 0 = 1us && Binary.u32 table MagicOffset = MagicNumber -> ValueSome ops
        | true -> ValueNone
        | false -> ValueNone

    /// <summary>Returns units per em, <c>head::get_upem</c>, with upstream's fallback of 1000.</summary>
    /// <param name="table">The table bytes.</param>
    /// <returns>Units per em, or 1000 when invalid or absent.</returns>
    let upem (table: ReadOnlySpan<byte>) =
        let upem = int (Binary.u16 table UpemOffset)
        match upem >= 16 && upem <= 16384 with
        | true -> upem
        | false -> 1000

    /// <summary>Returns the loca offset format: 0 for short, 1 for long, <c>head::indexToLocFormat</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <returns>0 or 1, defaulting to 0 when the table is absent.</returns>
    let indexToLocFormat (table: ReadOnlySpan<byte>) =
        int (Binary.u16 table IndexToLocFormatOffset)

    /// <summary>Returns the glyph data format, <c>head::glyphDataFormat</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <returns>The format, defaulting to 0 when the table is absent.</returns>
    let glyphDataFormat (table: ReadOnlySpan<byte>) =
        int (Binary.u16 table GlyphDataFormatOffset)

    /// <summary>The struct form of <see cref="sanitize"/>.</summary>
    [<Struct>]
    type Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops
