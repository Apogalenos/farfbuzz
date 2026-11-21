namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The CBDT and CBLC tables: the strikes indexed by their pixels-per-em and the raw table bytes.</summary>
type internal Cbdt =
    { Cblc: ReadOnlyMemory<byte>
      Data: ReadOnlyMemory<byte>
      Strikes: ImmutableArray<struct (int * int)> }

/// <summary>Reading the CBDT and CBLC bitmap tables, <c>src/OT/Color/CBDT/CBDT.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Cbdt =
    /// <summary>The size of the CBLC header, through the size-table count.</summary>
    [<Literal>]
    let private CblcHeaderSize = 8

    /// <summary>The size of the CBDT header.</summary>
    [<Literal>]
    let private CbdtHeaderSize = 4

    /// <summary>The offset of the size-table count.</summary>
    [<Literal>]
    let private SizeTableCountOffset = 4

    /// <summary>The offset of the size tables array.</summary>
    [<Literal>]
    let private SizeTablesOffset = 8

    /// <summary>The size of one BitmapSizeTable.</summary>
    [<Literal>]
    let private SizeTableSize = 48

    /// <summary>The offset of the index-subtable-array offset in a size table.</summary>
    [<Literal>]
    let private IndexSubtableArrayOffsetOffset = 0

    /// <summary>The offset of the number of index subtables in a size table.</summary>
    [<Literal>]
    let private NumberOfIndexSubtablesOffset = 8

    /// <summary>The offset of the horizontal ppem in a size table.</summary>
    [<Literal>]
    let private PpemXOffset = 44

    /// <summary>The offset of the vertical ppem in a size table.</summary>
    [<Literal>]
    let private PpemYOffset = 45

    /// <summary>The size of one IndexSubtableRecord.</summary>
    [<Literal>]
    let private RecordSize = 8

    /// <summary>The offset of a record's last glyph index.</summary>
    [<Literal>]
    let private RecordLastGlyphOffset = 2

    /// <summary>The offset of a record's subtable offset.</summary>
    [<Literal>]
    let private RecordSubtableOffset = 4

    /// <summary>The size of an IndexSubtable header.</summary>
    [<Literal>]
    let private SubtableHeaderSize = 8

    /// <summary>The offset of a subtable's image format.</summary>
    [<Literal>]
    let private SubtableImageFormatOffset = 2

    /// <summary>The offset of a subtable's image-data offset.</summary>
    [<Literal>]
    let private SubtableImageDataOffsetOffset = 4

    /// <summary>The smallest image length of a format-17 glyph bitmap.</summary>
    [<Literal>]
    let private Format17MinSize = 9

    /// <summary>The smallest image length of a format-18 glyph bitmap.</summary>
    [<Literal>]
    let private Format18MinSize = 12

    /// <summary>The empty CBDT of a face without a usable table.</summary>
    let empty =
        { Cblc = ReadOnlyMemory<byte>.Empty
          Data = ReadOnlyMemory<byte>.Empty
          Strikes = ImmutableArray.Empty }

    /// <summary>Sanitizes one index subtable, <c>IndexSubtable::sanitize</c>.</summary>
    /// <param name="cblc">The CBLC bytes.</param>
    /// <param name="subtableOffset">The subtable offset.</param>
    /// <param name="glyphCount">The record's glyph range size.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeSubtable (cblc: ReadOnlySpan<byte>) (subtableOffset: int64) (glyphCount: int) (ops: int) : int voption =
        match Sanitize.fits cblc.Length subtableOffset SubtableHeaderSize with
        | false -> ValueNone
        | true ->
            match Binary.u16 cblc (int subtableOffset) with
            | 1us -> Sanitize.array cblc.Length (subtableOffset + int64 SubtableHeaderSize) (uint32 (glyphCount + 1)) 4u ops
            | 3us -> Sanitize.array cblc.Length (subtableOffset + int64 SubtableHeaderSize) (uint32 (glyphCount + 1)) 2u ops
            | _ -> ValueSome ops

    /// <summary>Sanitizes one index-subtable record, threading the operation budget.</summary>
    /// <param name="cblc">The CBLC bytes.</param>
    /// <param name="arrayOffset">The index-subtable-array offset.</param>
    /// <param name="at">The record offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeOneRecord (cblc: ReadOnlySpan<byte>) (arrayOffset: int64) (at: int) (ops: int) : int voption =
        let first = int (Binary.u16 cblc at)
        let last = int (Binary.u16 cblc (at + RecordLastGlyphOffset))
        let subtableOffset = int64 (Binary.u32 cblc (at + RecordSubtableOffset))
        match first <= last with
        | false -> ValueNone
        | true ->
            match subtableOffset with
            | 0L -> ValueSome ops
            | _ -> sanitizeSubtable cblc (arrayOffset + subtableOffset) (last - first + 1) ops

    /// <summary>Sanitizes every index-subtable record, threading the operation budget.</summary>
    /// <param name="cblc">The CBLC bytes.</param>
    /// <param name="arrayOffset">The index-subtable-array offset.</param>
    /// <param name="numTables">The number of records.</param>
    /// <param name="index">The next record.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeRecords (cblc: ReadOnlySpan<byte>) (arrayOffset: int64) (numTables: int) (index: int) (ops: int) : int voption =
        match index >= numTables with
        | true -> ValueSome ops
        | false ->
            let at = int arrayOffset + index * RecordSize
            match sanitizeOneRecord cblc arrayOffset at ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeRecords cblc arrayOffset numTables (index + 1) remaining

    /// <summary>Sanitizes every size table, threading the operation budget.</summary>
    /// <param name="cblc">The CBLC bytes.</param>
    /// <param name="count">The number of size tables.</param>
    /// <param name="index">The next size table.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeSizeTables (cblc: ReadOnlySpan<byte>) (count: int) (index: int) (ops: int) : int voption =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let at = SizeTablesOffset + index * SizeTableSize
            let arrayOffset = int64 (Binary.u32 cblc (at + IndexSubtableArrayOffsetOffset))
            let numTables = int (Binary.u32 cblc (at + NumberOfIndexSubtablesOffset))
            match Sanitize.array cblc.Length arrayOffset (uint32 numTables) (uint32 RecordSize) ops with
            | ValueNone -> ValueNone
            | ValueSome afterRecords ->
                match sanitizeRecords cblc arrayOffset numTables 0 afterRecords with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeSizeTables cblc count (index + 1) remaining

    /// <summary>Sanitizes the CBLC and CBDT tables, <c>CBLC::sanitize</c> and <c>CBDT::sanitize</c>.</summary>
    /// <param name="cblc">The CBLC bytes.</param>
    /// <param name="cbdt">The CBDT bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (cblc: ReadOnlySpan<byte>) (cbdt: ReadOnlySpan<byte>) (ops: int) : int voption =
        let cblcMajor = Binary.u16 cblc 0
        let cbdtMajor = Binary.u16 cbdt 0
        let headersOk =
            (cbdtMajor = 2us || cbdtMajor = 3us)
            && Sanitize.fits cbdt.Length 0L CbdtHeaderSize
            && (cblcMajor = 2us || cblcMajor = 3us)
            && Sanitize.fits cblc.Length 0L CblcHeaderSize
        match headersOk with
        | false -> ValueNone
        | true ->
            let count = int (Binary.u32 cblc SizeTableCountOffset)
            match Sanitize.array cblc.Length (int64 SizeTablesOffset) (uint32 count) (uint32 SizeTableSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeSizeTables cblc count 0 remaining

    /// <summary>Loads the CBLC and CBDT tables, empty on sanitize failure, <c>CBDT_accelerator_t</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed CBDT.</returns>
    let load (face: Face) : Cbdt =
        let cblc = Face.referenceTable face Tags.Cblc
        let cbdt = Face.referenceTable face Tags.Cbdt
        let ops = Sanitize.start (max cblc.Length cbdt.Length)
        match sanitize cblc.Span cbdt.Span ops with
        | ValueNone -> empty
        | ValueSome _ ->
            let count = int (Binary.u32 cblc.Span SizeTableCountOffset)
            let strikes =
                Sealed.init count (fun index ->
                    let at = SizeTablesOffset + index * SizeTableSize
                    let ppemX = int (Binary.u8 cblc.Span (at + PpemXOffset))
                    let ppemY = int (Binary.u8 cblc.Span (at + PpemYOffset))
                    struct (max ppemX ppemY, at))
            { Cblc = cblc
              Data = cbdt
              Strikes = strikes }

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

    /// <summary>Chooses the strike for a font's ppem, <c>CBDT::accelerator_t::choose_strike</c>.</summary>
    /// <param name="strikes">The strikes, in table order.</param>
    /// <param name="xPpem">The horizontal ppem.</param>
    /// <param name="yPpem">The vertical ppem.</param>
    /// <returns>The chosen strike's ppem and size-table offset, or none.</returns>
    let private chooseStrike (strikes: ReadOnlySpan<struct (int * int)>) (xPpem: uint32) (yPpem: uint32) : struct (int * int) voption =
        let requested = match max xPpem yPpem with | 0u -> 1 <<< 30 | ppem -> int ppem
        let struct (best, bestOffset, fallback, fallbackOffset) = scanStrikes strikes requested 0 Int32.MaxValue 0 -1 0
        match best <> Int32.MaxValue with
        | true -> ValueSome (struct (best, bestOffset))
        | false ->
            match fallback >= 0 with
            | true -> ValueSome (struct (fallback, fallbackOffset))
            | false -> ValueNone

    /// <summary>Finds the first index-subtable record that covers a glyph, <c>IndexSubtableArray::find_table</c>.</summary>
    /// <param name="cblc">The CBLC bytes.</param>
    /// <param name="arrayOffset">The index-subtable-array offset.</param>
    /// <param name="numTables">The number of records.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="index">The next record.</param>
    /// <returns>The record index, or none.</returns>
    [<TailCall>]
    let rec private findRecord (cblc: ReadOnlySpan<byte>) (arrayOffset: int64) (numTables: int) (glyph: uint32) (index: int) : int voption =
        match index >= numTables with
        | true -> ValueNone
        | false ->
            let at = int arrayOffset + index * RecordSize
            let first = uint32 (Binary.u16 cblc at)
            let last = uint32 (Binary.u16 cblc (at + RecordLastGlyphOffset))
            match first <= glyph && glyph <= last with
            | true -> ValueSome index
            | false -> findRecord cblc arrayOffset numTables glyph (index + 1)

    /// <summary>Reads a glyph's image data, <c>IndexSubtable::get_image_data</c>.</summary>
    /// <param name="cblc">The CBLC bytes.</param>
    /// <param name="subtableOffset">The subtable offset.</param>
    /// <param name="idx">The glyph's offset within the record.</param>
    /// <returns>The image format, the image offset and its length, or none.</returns>
    let private readImageData (cblc: ReadOnlySpan<byte>) (subtableOffset: int64) (idx: int) : struct (int * uint32 * uint32) voption =
        let indexFormat = Binary.u16 cblc (int subtableOffset)
        let imageFormat = int (Binary.u16 cblc (int subtableOffset + SubtableImageFormatOffset))
        let imageDataOffset = Binary.u32 cblc (int subtableOffset + SubtableImageDataOffsetOffset)
        match indexFormat with
        | 1us ->
            let a = Binary.u32 cblc (int subtableOffset + SubtableHeaderSize + idx * 4)
            let b = Binary.u32 cblc (int subtableOffset + SubtableHeaderSize + (idx + 1) * 4)
            match b <= a with
            | true -> ValueNone
            | false -> ValueSome (struct (imageFormat, imageDataOffset + a, b - a))
        | 3us ->
            let a = uint32 (Binary.u16 cblc (int subtableOffset + SubtableHeaderSize + idx * 2))
            let b = uint32 (Binary.u16 cblc (int subtableOffset + SubtableHeaderSize + (idx + 1) * 2))
            match b <= a with
            | true -> ValueNone
            | false -> ValueSome (struct (imageFormat, imageDataOffset + a, b - a))
        | _ -> ValueNone

    /// <summary>Reads a glyph's small metrics, <c>SmallGlyphMetrics::get_extents</c>.</summary>
    /// <param name="data">The CBDT bytes.</param>
    /// <param name="at">The metrics offset.</param>
    /// <returns>The raw pixel metrics.</returns>
    let private smallMetrics (data: ReadOnlySpan<byte>) (at: int) : GlyphExtents =
        { XBearing = int (sbyte (Binary.u8 data (at + 2)))
          YBearing = int (sbyte (Binary.u8 data (at + 3)))
          Width = int (Binary.u8 data (at + 1))
          Height = -int (Binary.u8 data at) }

    /// <summary>Reads a glyph's image data into raw metrics, <c>CBDT::accelerator_t::get_extents</c>'s image formats.</summary>
    /// <param name="cbdt">The table.</param>
    /// <param name="imageFormat">The image format, 17 or 18.</param>
    /// <param name="imageOffset">The image offset within the CBDT table.</param>
    /// <param name="imageLength">The image length.</param>
    /// <param name="ppemX">The strike's horizontal ppem.</param>
    /// <param name="ppemY">The strike's vertical ppem.</param>
    /// <returns>The raw metrics and the strike's ppems, or none.</returns>
    let private imageFor (cbdt: Cbdt) (imageFormat: int) (imageOffset: uint32) (imageLength: uint32) (ppemX: int) (ppemY: int) : struct (GlyphExtents * int * int) voption =
        let data = cbdt.Data.Span
        match imageOffset > uint32 data.Length || uint32 data.Length - imageOffset < imageLength with
        | true -> ValueNone
        | false ->
            match imageFormat with
            | 17 ->
                match imageLength >= uint32 Format17MinSize with
                | false -> ValueNone
                | true -> ValueSome (struct (smallMetrics data (int imageOffset), ppemX, ppemY))
            | 18 ->
                match imageLength >= uint32 Format18MinSize with
                | false -> ValueNone
                | true -> ValueSome (struct (smallMetrics data (int imageOffset), ppemX, ppemY))
            | _ -> ValueNone

    /// <summary>Reads one record's subtable image, following the dupe-style null check.</summary>
    /// <param name="cbdt">The table.</param>
    /// <param name="offsetToSubtable">The record's subtable offset.</param>
    /// <param name="arrayOffset">The index-subtable-array offset.</param>
    /// <param name="firstGlyph">The record's first glyph.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="ppemX">The strike's horizontal ppem.</param>
    /// <param name="ppemY">The strike's vertical ppem.</param>
    /// <returns>The raw metrics and the strike's ppems, or none.</returns>
    let private subtableImage (cbdt: Cbdt) (offsetToSubtable: uint32) (arrayOffset: int64) (firstGlyph: uint32) (glyph: uint32) (ppemX: int) (ppemY: int) : struct (GlyphExtents * int * int) voption =
        match offsetToSubtable = 0u with
        | true -> ValueNone
        | false ->
            let subtableOffset = arrayOffset + int64 offsetToSubtable
            match readImageData cbdt.Cblc.Span subtableOffset (int (glyph - firstGlyph)) with
            | ValueNone -> ValueNone
            | ValueSome (struct (imageFormat, imageOffset, imageLength)) ->
                imageFor cbdt imageFormat imageOffset imageLength ppemX ppemY

    /// <summary>Finds the record covering a glyph and reads its image.</summary>
    /// <param name="cbdt">The table.</param>
    /// <param name="arrayOffset">The index-subtable-array offset.</param>
    /// <param name="numTables">The number of records.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="ppemX">The strike's horizontal ppem.</param>
    /// <param name="ppemY">The strike's vertical ppem.</param>
    /// <returns>The raw metrics and the strike's ppems, or none.</returns>
    let private recordImage (cbdt: Cbdt) (arrayOffset: int64) (numTables: int) (glyph: uint32) (ppemX: int) (ppemY: int) : struct (GlyphExtents * int * int) voption =
        match findRecord cbdt.Cblc.Span arrayOffset numTables glyph 0 with
        | ValueNone -> ValueNone
        | ValueSome recordIndex ->
            let cblc = cbdt.Cblc.Span
            let recordAt = int arrayOffset + recordIndex * RecordSize
            let firstGlyph = uint32 (Binary.u16 cblc recordAt)
            let offsetToSubtable = Binary.u32 cblc (recordAt + RecordSubtableOffset)
            subtableImage cbdt offsetToSubtable arrayOffset firstGlyph glyph ppemX ppemY

    /// <summary>Reads a chosen strike's ppems and the glyph's image.</summary>
    /// <param name="cbdt">The table.</param>
    /// <param name="sizeTableAt">The chosen strike's offset.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The raw metrics and the strike's ppems, or none.</returns>
    let private strikeImage (cbdt: Cbdt) (sizeTableAt: int) (glyph: uint32) : struct (GlyphExtents * int * int) voption =
        let cblc = cbdt.Cblc.Span
        let ppemX = int (Binary.u8 cblc (sizeTableAt + PpemXOffset))
        let ppemY = int (Binary.u8 cblc (sizeTableAt + PpemYOffset))
        match ppemX = 0 || ppemY = 0 with
        | true -> ValueNone
        | false ->
            let arrayOffset = int64 (Binary.u32 cblc (sizeTableAt + IndexSubtableArrayOffsetOffset))
            let numTables = int (Binary.u32 cblc (sizeTableAt + NumberOfIndexSubtablesOffset))
            recordImage cbdt arrayOffset numTables glyph ppemX ppemY

    /// <summary>The raw pixel metrics and the strike's ppems of a glyph's bitmap, before the conversions.</summary>
    /// <param name="cbdt">The table.</param>
    /// <param name="xPpem">The horizontal ppem.</param>
    /// <param name="yPpem">The vertical ppem.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The raw metrics and the strike's ppems, or none when the glyph has no bitmap.</returns>
    let glyphMetrics (cbdt: Cbdt) (xPpem: uint32) (yPpem: uint32) (glyph: uint32) : struct (GlyphExtents * int * int) voption =
        match chooseStrike (cbdt.Strikes.AsSpan()) xPpem yPpem with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, sizeTableAt)) -> strikeImage cbdt sizeTableAt glyph
