namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.Aat

/// <summary>The anchor point table, <c>AAT::ankr</c>: the table bytes, the lookup that maps a glyph to its anchor array
/// and the byte offset the lookup's values are relative to. A missing or invalid table has the Null lookup. <c>Offsets</c> is the
/// anchor array of every glyph of the face, read from the lookup at the first anchor the face is asked for.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Ankr =
    { Table: ReadOnlyMemory<byte>
      Lookup: Lookup
      AnchorsAt: int
      Offsets: Lazy<ImmutableArray<int>> }

/// <summary>Reading the ankr table, <c>AAT::ankr</c> (src/hb-aat-layout-ankr-table.hh:35-99).</summary>
[<RequireQualifiedAccess>]
module internal Ankr =
    /// <summary>The size of the table header, <c>ankr::static_size</c>.</summary>
    [<Literal>]
    let private HeaderSize = 12

    /// <summary>The byte width of one lookup value, an <c>NNOffset16To</c>.</summary>
    [<Literal>]
    let private ValueSize = 2

    /// <summary>The byte width of one anchor, <c>Anchor::static_size</c>.</summary>
    [<Literal>]
    let private AnchorSize = 4

    /// <summary>The byte width of the length of a glyph's anchor array, <c>Array32Of</c>.</summary>
    [<Literal>]
    let private LengthSize = 4

    /// <summary>The smallest unit of a format 2 lookup segment, <c>LookupSegmentSingle::static_size</c>.</summary>
    [<Literal>]
    let private SegmentSingleSize = 6

    /// <summary>The smallest unit of a format 4 lookup segment, <c>LookupSegmentArray::static_size</c>.</summary>
    [<Literal>]
    let private SegmentArraySize = 6

    /// <summary>The smallest unit of a format 6 lookup entry, <c>LookupSingle::static_size</c>.</summary>
    [<Literal>]
    let private SingleSize = 4

    /// <summary>The terminator words of a format 2 or format 4 unit, <c>TerminationWordCount</c>.</summary>
    [<Literal>]
    let private SegmentTerminator = 2

    /// <summary>The terminator words of a format 6 unit, <c>TerminationWordCount</c>.</summary>
    [<Literal>]
    let private SingleTerminator = 1

    /// <summary>Sanitizes one lookup value and the anchor array it points at, <c>NNOffset16To&lt;GlyphAnchors&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the value is relative to.</param>
    /// <param name="at">The value's offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the value fails.</returns>
    let private sanitizeValue (table: ReadOnlySpan<byte>) (anchors: int64) (at: int64) (ops: int) =
        match Sanitize.fits table.Length at ValueSize with
        | false -> ValueNone
        | true ->
            let target = anchors + int64 (Binary.u16 table (int at))
            match Sanitize.fits table.Length target LengthSize with
            | false -> ValueNone
            | true -> Sanitize.array table.Length (target + int64 LengthSize) (Binary.u32 table (int target)) (uint32 AnchorSize) ops

    /// <summary>Sanitizes consecutive lookup values.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="at">The offset of the first value.</param>
    /// <param name="stride">The distance between two values.</param>
    /// <param name="count">The number of values.</param>
    /// <param name="index">The next value.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when a value fails.</returns>
    [<TailCall>]
    let rec private sanitizeValues (table: ReadOnlySpan<byte>) (anchors: int64) (at: int64) (stride: int) (count: int) (index: int) (ops: int) =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            match sanitizeValue table anchors (at + int64 index * int64 stride) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeValues table anchors at stride count (index + 1) remaining

    /// <summary>Sanitizes an array of lookup values, <c>UnsizedArrayOf::sanitize</c> with a base.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="at">The offset of the array.</param>
    /// <param name="count">The number of values.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the array fails.</returns>
    let private sanitizeArray (table: ReadOnlySpan<byte>) (anchors: int64) (at: int64) (count: int) (ops: int) =
        match Sanitize.array table.Length at (uint32 count) (uint32 ValueSize) ops with
        | ValueNone -> ValueNone
        | ValueSome remaining -> sanitizeValues table anchors at ValueSize count 0 remaining

    /// <summary>Sanitizes the value of each unit of a binary-search lookup that carries one value per unit, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="unitsAt">The offset of the first unit.</param>
    /// <param name="unitSize">The size of one unit.</param>
    /// <param name="minimum">The size of the record the unit holds.</param>
    /// <param name="valueAt">The offset of the value inside a unit.</param>
    /// <param name="count">The number of units.</param>
    /// <param name="index">The next unit.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when a unit fails.</returns>
    [<TailCall>]
    let rec private sanitizeUnitValues (table: ReadOnlySpan<byte>) (anchors: int64) (unitsAt: int64) (unitSize: int) (minimum: int) (valueAt: int) (count: int) (index: int) (ops: int) =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let unit = unitsAt + int64 index * int64 unitSize
            match Sanitize.fits table.Length unit minimum with
            | false -> ValueNone
            | true ->
                match sanitizeValue table anchors (unit + int64 valueAt) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeUnitValues table anchors unitsAt unitSize minimum valueAt count (index + 1) remaining

    /// <summary>Sanitizes one segment of a format 4 lookup: the segment, its glyph range and its value array.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="lookupAt">The offset of the lookup.</param>
    /// <param name="unit">The offset of the segment.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the segment fails.</returns>
    let private sanitizeSegment (table: ReadOnlySpan<byte>) (anchors: int64) (lookupAt: int64) (unit: int64) (ops: int) =
        match Sanitize.fits table.Length unit SegmentArraySize with
        | false -> ValueNone
        | true ->
            let last = int (Binary.u16 table (int unit))
            let first = int (Binary.u16 table (int unit + 2))
            match first <= last with
            | false -> ValueNone
            | true -> sanitizeArray table anchors (lookupAt + int64 (Binary.u16 table (int unit + 4))) (last - first + 1) ops

    /// <summary>Sanitizes the segments of a format 4 lookup, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="lookupAt">The offset of the lookup.</param>
    /// <param name="unitsAt">The offset of the first segment.</param>
    /// <param name="unitSize">The size of one segment.</param>
    /// <param name="count">The number of segments.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when a segment fails.</returns>
    [<TailCall>]
    let rec private sanitizeSegments (table: ReadOnlySpan<byte>) (anchors: int64) (lookupAt: int64) (unitsAt: int64) (unitSize: int) (count: int) (index: int) (ops: int) =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            match sanitizeSegment table anchors lookupAt (unitsAt + int64 index * int64 unitSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeSegments table anchors lookupAt unitsAt unitSize count (index + 1) remaining

    /// <summary>Sanitizes the header and unit array of a binary-search lookup, then hands the units to the format's loop.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lookupAt">The offset of the lookup.</param>
    /// <param name="minimum">The smallest unit the record type allows.</param>
    /// <param name="terminator">The terminator word count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The offset of the first unit, the unit size, the unit count and the remaining operations, or none.</returns>
    let private sanitizeBinSearch (table: ReadOnlySpan<byte>) (lookupAt: int64) (minimum: int) (terminator: int) (ops: int) =
        match Sanitize.fits table.Length (lookupAt + 2L) 10 with
        | false -> ValueNone
        | true ->
            let unitSize = int (Binary.u16 table (int lookupAt + 2))
            let nUnits = int (Binary.u16 table (int lookupAt + 4))
            let unitsAt = lookupAt + 12L
            match unitSize >= minimum with
            | false -> ValueNone
            | true ->
                match Sanitize.array table.Length unitsAt (uint32 nUnits) (uint32 unitSize) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> ValueSome (struct (AatLookup.getLength table (int unitsAt) unitSize nUnits terminator, unitSize, remaining))

    /// <summary>Sanitizes a binary-search lookup that carries one value per unit (formats 2 and 6): the header and unit
    /// array, then each unit's value.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="lookupAt">The offset of the lookup.</param>
    /// <param name="minimum">The smallest unit the record type allows.</param>
    /// <param name="valueAt">The offset of the value inside a unit.</param>
    /// <param name="terminator">The terminator word count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the lookup fails.</returns>
    let private sanitizeUnits (table: ReadOnlySpan<byte>) (anchors: int64) (lookupAt: int64) (minimum: int) (valueAt: int) (terminator: int) (ops: int) =
        match sanitizeBinSearch table lookupAt minimum terminator ops with
        | ValueNone -> ValueNone
        | ValueSome (struct (count, unitSize, remaining)) -> sanitizeUnitValues table anchors (lookupAt + 12L) unitSize minimum valueAt count 0 remaining

    /// <summary>Sanitizes a format 4 lookup: the header and unit array, then each segment.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="lookupAt">The offset of the lookup.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the lookup fails.</returns>
    let private sanitizeSegmentLookup (table: ReadOnlySpan<byte>) (anchors: int64) (lookupAt: int64) (ops: int) =
        match sanitizeBinSearch table lookupAt SegmentArraySize SegmentTerminator ops with
        | ValueNone -> ValueNone
        | ValueSome (struct (count, unitSize, remaining)) -> sanitizeSegments table anchors lookupAt (lookupAt + 12L) unitSize count 0 remaining

    /// <summary>Sanitizes a format 8 lookup: the six-byte struct, then its array of values.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="lookupAt">The offset of the lookup.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the lookup fails.</returns>
    let private sanitizeTrimmed (table: ReadOnlySpan<byte>) (anchors: int64) (lookupAt: int64) (ops: int) =
        match Sanitize.fits table.Length lookupAt 6 with
        | false -> ValueNone
        | true -> sanitizeArray table anchors (lookupAt + 6L) (int (Binary.u16 table (int lookupAt + 4))) ops

    /// <summary>Sanitizes a lookup whose values are anchor-array offsets,
    /// <c>Lookup&lt;NNOffset16To&lt;GlyphAnchors&gt;&gt;::sanitize</c> with a base; format 10 is refused.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchors">The offset the values are relative to.</param>
    /// <param name="lookupAt">The offset of the lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the lookup fails.</returns>
    let private sanitizeLookup (table: ReadOnlySpan<byte>) (anchors: int64) (lookupAt: int64) (numGlyphs: int) (ops: int) =
        match Sanitize.fits table.Length lookupAt 2 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table (int lookupAt) with
            | 0us -> sanitizeArray table anchors (lookupAt + 2L) numGlyphs ops
            | 2us -> sanitizeUnits table anchors lookupAt SegmentSingleSize 4 SegmentTerminator ops
            | 4us -> sanitizeSegmentLookup table anchors lookupAt ops
            | 6us -> sanitizeUnits table anchors lookupAt SingleSize 2 SingleTerminator ops
            | 8us -> sanitizeTrimmed table anchors lookupAt ops
            | 10us -> ValueNone
            | _ -> ValueSome ops

    /// <summary>Sanitizes the ankr table, <c>ankr::sanitize</c>: the struct, version zero, the anchor data offset as a range
    /// and the lookup with every anchor array it reaches.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (numGlyphs: int) (ops: int) =
        match Sanitize.fits table.Length 0L HeaderSize && Binary.u16 table 0 = 0us with
        | false -> ValueNone
        | true ->
            let lookupOffset = Binary.u32 table 4
            let anchorData = Binary.u32 table 8
            match Sanitize.range table.Length 0L anchorData ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                match lookupOffset with
                | 0u -> ValueSome remaining
                | _ -> sanitizeLookup table (int64 anchorData) (int64 lookupOffset) numGlyphs remaining

    /// <summary>The struct form of the ankr sanitizer.</summary>
    [<Struct>]
    type private Sanitizer =
        { NumGlyphs: int }
        interface ITableSanitizer with
            member this.Sanitize (table, ops) = sanitize table this.NumGlyphs ops

    /// <summary>The Null ankr table, which has no anchors.</summary>
    let empty =
        { Table = ReadOnlyMemory<byte>.Empty
          Lookup = { Lookup.Table = ReadOnlyMemory<byte>.Empty; At = 0; Format = LookupFormat.NullLookup; ValueSize = ValueSize }
          AnchorsAt = 0
          Offsets = lazy ImmutableArray<int>.Empty }

    /// <summary>The offset of a glyph's anchor array, <c>ankr::get_anchor</c>'s lookup: none when the lookup has no value for the glyph.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="anchorsAt">The offset the lookup's values are relative to.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The offset of the array's length, or -1 for a glyph without anchors.</returns>
    let private arrayAt (table: ReadOnlyMemory<byte>) (lookup: Lookup) (anchorsAt: int) (numGlyphs: int) (glyph: uint32) =
        match AatLookup.valueAt lookup glyph numGlyphs with
        | ValueNone -> -1
        | ValueSome valueAt -> int (int64 anchorsAt + int64 (Binary.u16 table.Span valueAt))

    /// <summary>Loads and sanitizes a face's ankr table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The ankr table, the Null table when absent or invalid.</returns>
    let load (face: Face) =
        let sanitizer: Sanitizer = { NumGlyphs = face.GlyphCount }
        let table = Sanitize.table sanitizer (Face.referenceTable face Tags.Ankr)
        match table.Length with
        | 0 -> empty
        | _ ->
            let span = table.Span
            let lookupOffset = Binary.u32 span 4
            let lookup =
                match lookupOffset with
                | 0u -> empty.Lookup
                | _ -> AatLookup.read table (int lookupOffset) ValueSize
            let anchorsAt = int (Binary.u32 span 8)
            { Table = table
              Lookup = lookup
              AnchorsAt = anchorsAt
              Offsets = lazy (Sealed.init face.GlyphCount (fun glyph -> arrayAt table lookup anchorsAt face.GlyphCount (uint32 glyph))) }

    /// <summary>A glyph's anchor, <c>ankr::get_anchor</c>: the Null anchor for a glyph without anchors and for an index past
    /// the glyph's anchor array.</summary>
    /// <param name="ankr">The table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="index">The anchor index.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The anchor's x and y coordinates in font units.</returns>
    let anchor (ankr: Ankr) (glyph: uint32) (index: uint32) (numGlyphs: int) =
        let offsets = ankr.Offsets.Value
        let target =
            match offsets.Length = numGlyphs && glyph < uint32 numGlyphs with
            | true -> offsets[int glyph]
            | false -> arrayAt ankr.Table ankr.Lookup ankr.AnchorsAt numGlyphs glyph
        match target < 0 with
        | true -> struct (0, 0)
        | false ->
            let span = ankr.Table.Span
            match index >= Binary.u32 span target with
            | true -> struct (0, 0)
            | false ->
                let at = target + LengthSize + int index * AnchorSize
                struct (int (Binary.i16 span at), int (Binary.i16 span (at + 2)))
