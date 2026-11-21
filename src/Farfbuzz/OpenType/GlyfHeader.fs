namespace Farfbuzz

open System

/// <summary>The glyf and loca tables of a face, enough to read a glyph's header bounds.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal GlyfHeaders =
    { Present: bool
      Glyf: ReadOnlyMemory<byte>
      Loca: ReadOnlyMemory<byte>
      LongOffsets: bool
      NumGlyphs: int }

/// <summary>Reading the non-variable glyf header bounds, <c>glyf.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal GlyfHeader =
    /// <summary>The size of one glyph header.</summary>
    [<Literal>]
    let private HeaderSize = 10

    /// <summary>The offset of xMin.</summary>
    [<Literal>]
    let private XMinOffset = 2

    /// <summary>The offset of yMin.</summary>
    [<Literal>]
    let private YMinOffset = 4

    /// <summary>The offset of xMax.</summary>
    [<Literal>]
    let private XMaxOffset = 6

    /// <summary>The offset of yMax.</summary>
    [<Literal>]
    let private YMaxOffset = 8

    /// <summary>Builds the accelerator from head, loca and glyf, <c>glyf_accelerator_t</c>'s constructor.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The accelerator.</returns>
    let load (face: Face) : GlyfHeaders =
        let head = Sanitize.table (Head.Sanitizer()) (Face.referenceTable face Tags.Head)
        let indexToLocFormat = Head.indexToLocFormat head.Span
        let glyphDataFormat = Head.glyphDataFormat head.Span
        let validFormat = indexToLocFormat <= 1 && glyphDataFormat <= 1
        let glyf = Face.referenceTable face Tags.Glyf
        let loca = Face.referenceTable face Tags.Loca
        let longOffsets = indexToLocFormat = 1
        let entrySize = match longOffsets with | true -> 4 | false -> 2
        let numGlyphs =
            match validFormat with
            | false -> 0
            | true -> min (max 1 (loca.Length / entrySize) - 1) face.GlyphCount
        { Present = numGlyphs <> 0
          Glyf = glyf
          Loca = loca
          LongOffsets = longOffsets
          NumGlyphs = numGlyphs }

    /// <summary>The data range of one glyph, read from loca, or none when the glyph is out of range.</summary>
    /// <param name="glyf">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The half-open byte range, or none.</returns>
    let private range (glyf: GlyfHeaders) (glyph: uint32) : struct (int * int) voption =
        match uint64 glyph < uint64 glyf.NumGlyphs with
        | false -> ValueNone
        | true ->
            let span = glyf.Loca.Span
            let at = int glyph
            let start, finish =
                match glyf.LongOffsets with
                | true -> int (Binary.u32 span (at * 4)), int (Binary.u32 span (at * 4 + 4))
                | false -> int (Binary.u16 span (at * 2)) * 2, int (Binary.u16 span (at * 2 + 2)) * 2
            ValueSome (struct (start, finish))

    /// <summary>What one glyph is: absent, empty, or a header-bearing glyph.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type GlyfRecord =
        | NoGlyph
        | EmptyGlyph
        | Bounds of xMin: int16 * yMin: int16 * xMax: int16 * yMax: int16

    /// <summary>Classifies one glyph, <c>glyf_accelerator_t::glyph_for_gid</c> and <c>Glyph</c>'s constructor.</summary>
    /// <param name="glyf">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The record.</returns>
    let classify (glyf: GlyfHeaders) (glyph: uint32) =
        match range glyf glyph with
        | ValueNone -> NoGlyph
        | ValueSome (struct (start, finish)) ->
            let span = glyf.Glyf.Span
            match start >= finish || finish > glyf.Glyf.Length || finish - start < HeaderSize with
            | true -> EmptyGlyph
            | false ->
                match Binary.i16 span start = 0s with
                | true -> EmptyGlyph
                | false ->
                    Bounds(Binary.i16 span (start + XMinOffset), Binary.i16 span (start + YMinOffset), Binary.i16 span (start + XMaxOffset), Binary.i16 span (start + YMaxOffset))

    /// <summary>Whether a glyph is empty: zero contours or no data range, <c>Glyph::get_extents_without_var_scaled</c>'s EMPTY branch.</summary>
    /// <param name="glyf">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>True for an empty glyph.</returns>
    let empty (glyf: GlyfHeaders) (glyph: uint32) : bool =
        match classify glyf glyph with
        | EmptyGlyph -> true
        | NoGlyph | Bounds _ -> false

    /// <summary>Returns the raw header bounds of a non-empty glyph.</summary>
    /// <param name="glyf">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bounds in header order, or none for an absent or empty glyph.</returns>
    let bounds (glyf: GlyfHeaders) (glyph: uint32) : struct (int16 * int16 * int16 * int16) voption =
        match classify glyf glyph with
        | Bounds (xMin, yMin, xMax, yMax) -> ValueSome (struct (xMin, yMin, xMax, yMax))
        | NoGlyph | EmptyGlyph -> ValueNone
