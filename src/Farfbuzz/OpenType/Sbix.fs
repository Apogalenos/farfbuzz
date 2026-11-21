namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The sbix table: the strikes indexed by their pixels-per-em and the raw table bytes.</summary>
type internal Sbix =
    { Table: ReadOnlyMemory<byte>
      GlyphCount: int
      Strikes: ImmutableArray<struct (int * int)> }

/// <summary>Reading the sbix bitmap table, <c>src/OT/Color/sbix/sbix.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Sbix =
    /// <summary>The size of the sbix header, through the strike count.</summary>
    [<Literal>]
    let private HeaderSize = 8

    /// <summary>The size of one strike header, through the resolution.</summary>
    [<Literal>]
    let private StrikeHeaderSize = 4

    /// <summary>The byte size of one image offset or strike offset.</summary>
    [<Literal>]
    let private EntrySize = 4

    /// <summary>The offset of the strike count.</summary>
    [<Literal>]
    let private StrikeCountOffset = 4

    /// <summary>The offset of the strikes array.</summary>
    [<Literal>]
    let private StrikesOffset = 8

    /// <summary>The fixed size of an SBIXGlyph header, <c>SBIXGlyph::min_size</c>.</summary>
    [<Literal>]
    let private GlyphHeaderSize = 8

    /// <summary>The offset of the x offset in an SBIXGlyph.</summary>
    [<Literal>]
    let private XOffsetOffset = 0

    /// <summary>The offset of the y offset in an SBIXGlyph.</summary>
    [<Literal>]
    let private YOffsetOffset = 2

    /// <summary>The offset of the graphic type in an SBIXGlyph.</summary>
    [<Literal>]
    let private GraphicTypeOffset = 4

    /// <summary>The offset of the PNG width in a 29-byte PNG header.</summary>
    [<Literal>]
    let private PngWidthOffset = 16

    /// <summary>The offset of the PNG height in a 29-byte PNG header.</summary>
    [<Literal>]
    let private PngHeightOffset = 20

    /// <summary>The largest PNG dimension that still yields extents.</summary>
    [<Literal>]
    let private PngMaxDimension = 65536u

    /// <summary>The number of dupe indirections a glyph may follow, <c>retry_count</c>.</summary>
    [<Literal>]
    let private DupeRetries = 8

    /// <summary>The byte size of the glyph id a dupe names.</summary>
    [<Literal>]
    let private DupeTargetSize = 2

    /// <summary>The PNG graphic type, <c>HB_TAG('p','n','g',' ')</c>.</summary>
    [<Literal>]
    let private PngType = 0x706E6720u<tag>

    /// <summary>The dupe graphic type, <c>HB_TAG('d','u','p','e')</c>.</summary>
    [<Literal>]
    let private DupeType = 0x64757065u<tag>

    /// <summary>The empty sbix of a face without a usable table.</summary>
    let empty =
        { Table = ReadOnlyMemory<byte>.Empty
          GlyphCount = 0
          Strikes = ImmutableArray.Empty }

    /// <summary>Clamps an int64 into the int32 position range, <c>hb_clamp_to&lt;hb_position_t&gt;</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The clamped value.</returns>
    let private clampToInt32 (value: int64) : int =
        int (Math.Clamp(value, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Sanitizes one strike's offsets, threading the operation budget.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="glyphCount">The face's glyph count.</param>
    /// <param name="offset">The strike offset, 0 for a null strike.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeOneStrike (table: ReadOnlySpan<byte>) (glyphCount: int) (offset: int64) (ops: int) : int voption =
        match offset with
        | 0L -> ValueSome ops
        | _ ->
            match Sanitize.fits table.Length offset StrikeHeaderSize with
            | false -> ValueNone
            | true -> Sanitize.array table.Length (offset + int64 StrikeHeaderSize) (uint32 (glyphCount + 1)) (uint32 EntrySize) ops

    /// <summary>Sanitizes every strike, threading the operation budget.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="glyphCount">The face's glyph count.</param>
    /// <param name="strikeCount">The number of strikes.</param>
    /// <param name="index">The next strike.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeStrikes (table: ReadOnlySpan<byte>) (glyphCount: int) (strikeCount: int) (index: int) (ops: int) : int voption =
        match index >= strikeCount with
        | true -> ValueSome ops
        | false ->
            let offset = int64 (Binary.u32 table (StrikesOffset + index * EntrySize))
            match sanitizeOneStrike table glyphCount offset ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeStrikes table glyphCount strikeCount (index + 1) remaining

    /// <summary>Sanitizes an sbix table, <c>sbix::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="glyphCount">The face's glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (glyphCount: int) (ops: int) : int voption =
        match Binary.u16 table 0 >= 1us && Sanitize.fits table.Length 0L HeaderSize with
        | false -> ValueNone
        | true ->
            let strikeCount = int (Binary.u32 table StrikeCountOffset)
            match Sanitize.array table.Length (int64 StrikesOffset) (uint32 strikeCount) (uint32 EntrySize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeStrikes table glyphCount strikeCount 0 remaining

    /// <summary>The struct form of the sbix sanitizer.</summary>
    [<Struct>]
    type private Sanitizer =
        val private glyphCount: int

        /// <summary>Creates an sbix sanitizer.</summary>
        /// <param name="glyphCount">The face's glyph count.</param>
        new (glyphCount) = { glyphCount = glyphCount }

        interface ITableSanitizer with
            member this.Sanitize (table, ops) = sanitize table this.glyphCount ops

    /// <summary>Loads the sbix table, empty on sanitize failure, <c>sbix_accelerator_t</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed sbix.</returns>
    let load (face: Face) : Sbix =
        let table = Sanitize.table (Sanitizer(face.GlyphCount)) (Face.referenceTable face Tags.Sbix)
        match table.Length with
        | 0 -> empty
        | _ ->
            let strikeCount = int (Binary.u32 table.Span StrikeCountOffset)
            let strikes =
                Sealed.init strikeCount (fun index ->
                    let offset = int (Binary.u32 table.Span (StrikesOffset + index * EntrySize))
                    let ppem = match offset with | 0 -> 0 | _ -> int (Binary.u16 table.Span offset)
                    struct (ppem, offset))
            { Table = table
              GlyphCount = face.GlyphCount
              Strikes = strikes }

    /// <summary>Whether the table has data, <c>sbix::has_data</c>.</summary>
    /// <param name="sbix">The table.</param>
    /// <returns>True for a version-1 table.</returns>
    let hasData (sbix: Sbix) : bool =
        Binary.u16 sbix.Table.Span 0 <> 0us

    /// <summary>Finds the chosen strike, the smallest at least the requested ppem else the largest, first of equal ppem.</summary>
    /// <param name="strikes">The strikes, in table order.</param>
    /// <param name="requested">The requested ppem, <c>1 &lt;&lt; 30</c> for the largest.</param>
    /// <param name="index">The next strike.</param>
    /// <param name="best">The smallest ppem at least the requested so far.</param>
    /// <param name="bestOffset">The best strike's offset.</param>
    /// <param name="fallback">The largest ppem so far.</param>
    /// <param name="fallbackOffset">The fallback strike's offset.</param>
    /// <returns>The best and fallback ppems and offsets.</returns>
    [<TailCall>]
    let rec private scanStrikes (strikes: ReadOnlySpan<struct (int * int)>) (requested: int) (index: int) (best: int) (bestOffset: int) (fallback: int) (fallbackOffset: int) : struct (int * int * int * int) =
        match index >= strikes.Length with
        | true -> struct (best, bestOffset, fallback, fallbackOffset)
        | false ->
            let struct (ppem, offset) = strikes[index]
            let best, bestOffset =
                match ppem >= requested && ppem < best with
                | true -> ppem, offset
                | false -> best, bestOffset
            let fallback, fallbackOffset =
                match ppem > fallback with
                | true -> ppem, offset
                | false -> fallback, fallbackOffset
            scanStrikes strikes requested (index + 1) best bestOffset fallback fallbackOffset

    /// <summary>Chooses the strike for a font's ppem, <c>sbix::accelerator_t::choose_strike</c>.</summary>
    /// <param name="strikes">The strikes, in table order.</param>
    /// <param name="xPpem">The horizontal ppem.</param>
    /// <param name="yPpem">The vertical ppem.</param>
    /// <returns>The chosen strike's ppem and offset, or none.</returns>
    let private chooseStrike (strikes: ReadOnlySpan<struct (int * int)>) (xPpem: uint32) (yPpem: uint32) : struct (int * int) voption =
        let requested = match max xPpem yPpem with | 0u -> 1 <<< 30 | ppem -> int ppem
        let struct (best, bestOffset, fallback, fallbackOffset) = scanStrikes strikes requested 0 Int32.MaxValue 0 -1 0
        match best <> Int32.MaxValue with
        | true -> ValueSome (struct (best, bestOffset))
        | false ->
            match fallback >= 0 with
            | true -> ValueSome (struct (fallback, fallbackOffset))
            | false -> ValueNone

    /// <summary>Reads one glyph's data range, <c>get_glyph_blob</c>'s offset checks.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="glyphCount">The face's glyph count.</param>
    /// <param name="strikeOffset">The chosen strike's offset.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The glyph header offset, the data offset and the data length, or none.</returns>
    let private glyphData (table: ReadOnlySpan<byte>) (glyphCount: int) (strikeOffset: int) (glyph: uint32) : struct (int * int * uint32) voption =
        match uint32 glyph >= uint32 glyphCount with
        | true -> ValueNone
        | false ->
            let at = strikeOffset + StrikeHeaderSize + int glyph * EntrySize
            let a = Binary.u32 table at
            let b = Binary.u32 table (at + EntrySize)
            match b <= a || b - a <= uint32 GlyphHeaderSize || b > uint32 (table.Length - strikeOffset) with
            | true -> ValueNone
            | false -> ValueSome (struct (strikeOffset + int a, strikeOffset + int a + GlyphHeaderSize, b - a - uint32 GlyphHeaderSize))

    /// <summary>Resolves one glyph's PNG data, following dupe indirections.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="glyphCount">The face's glyph count.</param>
    /// <param name="strikeOffset">The chosen strike's offset.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="retry">The dupe indirections left.</param>
    /// <returns>The x and y offsets and the PNG header offset, or none.</returns>
    [<TailCall>]
    let rec private resolvePng (table: ReadOnlySpan<byte>) (glyphCount: int) (strikeOffset: int) (glyph: uint32) (retry: int) : struct (int * int * int) voption =
        match glyphData table glyphCount strikeOffset glyph with
        | ValueNone -> ValueNone
        | ValueSome (struct (glyphAt, dataAt, dataLength)) ->
            match Binary.tag table (glyphAt + GraphicTypeOffset) with
            | graphic when graphic = DupeType ->
                match dataLength >= uint32 DupeTargetSize && retry > 0 with
                | true ->
                    let next = uint32 (Binary.u16 table dataAt)
                    resolvePng table glyphCount strikeOffset next (retry - 1)
                | false -> ValueNone
            | graphic when graphic = PngType ->
                ValueSome (struct (int (Binary.i16 table (glyphAt + XOffsetOffset)), int (Binary.i16 table (glyphAt + YOffsetOffset)), dataAt))
            | _ -> ValueNone

    /// <summary>The raw pixel extents of one glyph's PNG, after the dupe resolution, <c>get_png_extents</c> before the conversions.</summary>
    /// <param name="sbix">The table.</param>
    /// <param name="ppem">The chosen strike's ppem, non-zero.</param>
    /// <param name="strikeOffset">The chosen strike's offset.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The raw extents and the strike ppem, or none.</returns>
    let private pngExtents (sbix: Sbix) (ppem: int) (strikeOffset: int) (glyph: uint32) : struct (GlyphExtents * int) voption =
        match resolvePng sbix.Table.Span sbix.GlyphCount strikeOffset glyph DupeRetries with
        | ValueNone -> ValueNone
        | ValueSome (struct (xOffset, yOffset, pngAt)) ->
            let width = Binary.u32 sbix.Table.Span (pngAt + PngWidthOffset)
            let height = Binary.u32 sbix.Table.Span (pngAt + PngHeightOffset)
            match width >= PngMaxDimension || height >= PngMaxDimension with
            | true -> ValueNone
            | false ->
                let raw =
                    { XBearing = xOffset
                      YBearing = clampToInt32 (int64 height + int64 yOffset)
                      Width = int (uint32 width)
                      Height = -int (uint32 height) }
                ValueSome (struct (raw, ppem))

    /// <summary>The raw pixel extents and the strike ppem of a glyph's bitmap, <c>get_png_extents</c> before the conversions.</summary>
    /// <param name="sbix">The table.</param>
    /// <param name="xPpem">The horizontal ppem.</param>
    /// <param name="yPpem">The vertical ppem.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The raw extents and the strike ppem, or none when the glyph has no bitmap.</returns>
    let glyphBitmap (sbix: Sbix) (xPpem: uint32) (yPpem: uint32) (glyph: uint32) : struct (GlyphExtents * int) voption =
        match hasData sbix with
        | false -> ValueNone
        | true ->
            match chooseStrike (sbix.Strikes.AsSpan()) xPpem yPpem with
            | ValueNone -> ValueNone
            | ValueSome (struct (ppem, strikeOffset)) ->
                match ppem = 0 with
                | true -> ValueNone
                | false -> pngExtents sbix ppem strikeOffset glyph
