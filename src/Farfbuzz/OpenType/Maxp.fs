namespace Farfbuzz

open System

/// <summary>The <c>maxp</c> and 24-bit <c>MAXP</c> tables: glyph counts and their sanitizers.</summary>
[<RequireQualifiedAccess>]
module internal Maxp =
    /// <summary>The size of a lower maxp header.</summary>
    [<Literal>]
    let private LowerSize = 6

    /// <summary>The size of an upper MAXP header.</summary>
    [<Literal>]
    let private UpperSize = 7

    /// <summary>The size of the version 1 tail.</summary>
    [<Literal>]
    let private TailSize = 26

    /// <summary>The offset of the glyph count.</summary>
    [<Literal>]
    let private GlyphCountOffset = 4

    /// <summary>The minor version of a 0.5 table.</summary>
    [<Literal>]
    let private VersionHalf = 0x5000us

    /// <summary>Checks a maxp version and its optional version 1 tail.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="headerSize">6 for maxp, 7 for MAXP.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeWith (table: ReadOnlySpan<byte>) (headerSize) (ops) : int voption =
        match Sanitize.fits table.Length 0L headerSize with
        | false -> ValueNone
        | true ->
            let major = Binary.u16 table 0
            let minor = Binary.u16 table 2
            match major with
            | 1us ->
                match Sanitize.fits table.Length (int64 headerSize) TailSize with
                | true -> ValueSome ops
                | false -> ValueNone
            | 0us when minor = VersionHalf -> ValueSome ops
            | _ -> ValueNone

    /// <summary>Sanitizes a lower maxp table, <c>maxp::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitizeMaxp (table: ReadOnlySpan<byte>) (ops) : int voption =
        sanitizeWith table LowerSize ops

    /// <summary>Sanitizes an upper MAXP table, <c>MAXP::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitizeUpperMaxp (table: ReadOnlySpan<byte>) (ops) : int voption =
        sanitizeWith table UpperSize ops

    /// <summary>Reads the glyph count from the upper table when present, else the lower one.</summary>
    /// <param name="lower">The sanitized lower maxp bytes.</param>
    /// <param name="upper">The sanitized upper MAXP bytes.</param>
    /// <returns>The glyph count, or 0 when both are absent.</returns>
    let glyphCount (lower: ReadOnlyMemory<byte>) (upper: ReadOnlyMemory<byte>) =
        match upper.Length with
        | 0 ->
            match lower.Length with
            | 0 -> 0
            | _ -> int (Binary.u16 lower.Span GlyphCountOffset)
        | _ -> int (Binary.u24 upper.Span GlyphCountOffset)

    /// <summary>The struct form of <see cref="sanitizeMaxp"/>.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitizeMaxp table ops

    /// <summary>The struct form of <see cref="sanitizeUpperMaxp"/>.</summary>
    [<Struct>]
    type UpperSanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitizeUpperMaxp table ops
