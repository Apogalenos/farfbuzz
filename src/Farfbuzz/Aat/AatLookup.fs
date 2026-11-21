namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The format of a lookup table, or the Null lookup of format 0xFFFF.</summary>
[<Struct>]
type internal LookupFormat =
    | Format0
    | Format2
    | Format4
    | Format6
    | Format8
    | Format10
    | NullLookup

/// <summary>One AAT lookup table: the table bytes, the lookup's offset inside them, and the value width.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Lookup =
    { Table: ReadOnlyMemory<byte>
      At: int
      Format: LookupFormat
      ValueSize: int }

/// <summary>One obsolete AAT class table, narrow (8-bit) or wide (16-bit).</summary>
[<Struct; NoComparison; NoEquality>]
type internal ClassTable =
    { Table: ReadOnlyMemory<byte>
      At: int
      FirstGlyph: uint16
      Count: int
      Wide: bool }

/// <summary>A value filter for <see cref="AatLookup.collectGlyphsFiltered"/> and the class-table collector.</summary>
type internal IValueFilter =
    /// <summary>Whether a lookup or class value selects its glyph.</summary>
    /// <param name="value">The value.</param>
    /// <returns>True to keep the glyph.</returns>
    abstract Filter : value: uint32 -> bool

/// <summary>Reading AAT lookup tables, formats 0/2/4/6/8/10, and the obsolete class table.</summary>
[<RequireQualifiedAccess>]
module internal AatLookup =
    /// <summary>The glyph id HarfBuzz uses to mark a deleted glyph, <c>DELETED_GLYPH</c>.</summary>
    [<Literal>]
    let DeletedGlyph = 0xFFFFu

    /// <summary>The reserved class for an out-of-range glyph, <c>CLASS_OUT_OF_BOUNDS</c>.</summary>
    [<Literal>]
    let ClassOutOfBounds = 1us

    /// <summary>The SetOp that the range collector ignores, emitted for a skipped unit.</summary>
    let private skipOp = SetOp.Add (UInt32.MaxValue, UInt32.MaxValue)

    /// <summary>Reads a big-endian unsigned value of 1, 2 or 4 bytes; anything else reads 4 bytes.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <param name="valueSize">The width in bytes.</param>
    /// <returns>The value.</returns>
    let internal valueOf (table: ReadOnlySpan<byte>) (offset: int) (valueSize: int) =
        match valueSize with
        | 1 -> uint32 (Binary.u8 table offset)
        | 2 -> uint32 (Binary.u16 table offset)
        | _ -> Binary.u32 table offset

    /// <summary>Reads a big-endian value of one to four bytes, format 10's <c>get_value_or_null</c> loop.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The byte offset of the next byte.</param>
    /// <param name="remaining">The bytes still to read.</param>
    /// <param name="value">The value accumulated so far.</param>
    /// <returns>The value.</returns>
    [<TailCall>]
    let rec private readBytes (table: ReadOnlySpan<byte>) (offset: int) (remaining: int) (value: uint32) =
        match remaining <= 0 with
        | true -> value
        | false -> readBytes table (offset + 1) (remaining - 1) ((value <<< 8) ||| uint32 (Binary.u8 table offset))

    /// <summary>Whether the last unit begins with the required number of 0xFFFF words.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The byte offset of the next terminator word.</param>
    /// <param name="remaining">The words still to check.</param>
    /// <returns>True when every word is 0xFFFF.</returns>
    [<TailCall>]
    let rec private terminatorWords (table: ReadOnlySpan<byte>) (offset: int) (remaining: int) =
        match remaining <= 0 with
        | true -> true
        | false ->
            match Binary.u16 table offset = 0xFFFFus with
            | false -> false
            | true -> terminatorWords table (offset + 2) (remaining - 1)

    /// <summary>The unit count after dropping a trailing terminator, <c>VarSizedBinSearchArrayOf::get_length</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="unitsAt">The byte offset of the first unit.</param>
    /// <param name="unitSize">The size of one unit.</param>
    /// <param name="nUnits">The number of units the header declares.</param>
    /// <param name="terminationWordCount">How many 0xFFFF words mark the terminator.</param>
    /// <returns>The length.</returns>
    let internal getLength (table: ReadOnlySpan<byte>) (unitsAt: int) (unitSize: int) (nUnits: int) (terminationWordCount: int) =
        match nUnits with
        | 0 -> 0
        | _ ->
            match terminatorWords table (unitsAt + (nUnits - 1) * unitSize) terminationWordCount with
            | true -> nUnits - 1
            | false -> nUnits

    /// <summary>Compares a glyph with a segment record, <c>LookupSegmentSingle::cmp</c>.</summary>
    [<Struct>]
    type private SegmentComparer =
        { Key: uint32
          At: int
          UnitSize: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let unit = this.At + index * this.UnitSize
                let last = uint32 (Binary.u16 data unit)
                let first = uint32 (Binary.u16 data (unit + 2))
                match this.Key < first with
                | true -> -1
                | false ->
                    match this.Key <= last with
                    | true -> 0
                    | false -> 1

    /// <summary>Compares a glyph with a single record, <c>LookupSingle::cmp</c>.</summary>
    [<Struct>]
    type private SingleComparer =
        { Key: uint32
          At: int
          UnitSize: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let glyph = uint32 (Binary.u16 data (this.At + index * this.UnitSize))
                match this.Key > glyph, this.Key < glyph with
                | true, _ -> 1
                | false, true -> -1
                | false, false -> 0

    /// <summary>The byte offset of the value of a format 2 segment, or none.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="unitSize">The size of one segment.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The value offset.</returns>
    let private segmentValue (table: ReadOnlySpan<byte>) (at: int) (unitSize: int) (glyph: uint32) =
        let count = getLength table (at + 12) unitSize (int (Binary.u16 table (at + 4))) 2
        let comparer = { SegmentComparer.Key = glyph; At = at + 12; UnitSize = unitSize }
        match Binary.bsearch comparer table 0 (count - 1) with
        | ValueNone -> ValueNone
        | ValueSome index -> ValueSome (at + 12 + index * unitSize + 4)

    /// <summary>The byte offset of the value of a format 6 single, or none.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="unitSize">The size of one single.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The value offset.</returns>
    let private singleValue (table: ReadOnlySpan<byte>) (at: int) (unitSize: int) (glyph: uint32) =
        let count = getLength table (at + 12) unitSize (int (Binary.u16 table (at + 4))) 1
        let comparer = { SingleComparer.Key = glyph; At = at + 12; UnitSize = unitSize }
        match Binary.bsearch comparer table 0 (count - 1) with
        | ValueNone -> ValueNone
        | ValueSome index -> ValueSome (at + 12 + index * unitSize + 2)

    /// <summary>The byte offset of the value of a format 4 segment array, or none.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The value offset.</returns>
    let private segmentArrayValue (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (glyph: uint32) =
        let unitSize = int (Binary.u16 table (at + 2))
        let count = getLength table (at + 12) unitSize (int (Binary.u16 table (at + 4))) 2
        let comparer = { SegmentComparer.Key = glyph; At = at + 12; UnitSize = unitSize }
        match Binary.bsearch comparer table 0 (count - 1) with
        | ValueNone -> ValueNone
        | ValueSome index ->
            let unit = at + 12 + index * unitSize
            let first = uint32 (Binary.u16 table (unit + 2))
            let valuesZ = int (Binary.u16 table (unit + 4))
            ValueSome (at + valuesZ + int (glyph - first) * valueSize)

    /// <summary>The byte offset of the value of a format 0 lookup, or none.</summary>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The value offset.</returns>
    let private format0Value (at: int) (valueSize: int) (glyph: uint32) (numGlyphs: int) =
        match glyph < uint32 numGlyphs with
        | true -> ValueSome (at + 2 + int glyph * valueSize)
        | false -> ValueNone

    /// <summary>The byte offset of the value of a format 8 trimmed array, or none.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The value offset.</returns>
    let private format8Value (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (glyph: uint32) =
        let firstGlyph = uint32 (Binary.u16 table (at + 2))
        let glyphCount = uint32 (Binary.u16 table (at + 4))
        match firstGlyph <= glyph && glyph - firstGlyph < glyphCount with
        | true -> ValueSome (at + 6 + int (glyph - firstGlyph) * valueSize)
        | false -> ValueNone

    /// <summary>Format 10's <c>get_value_or_null</c>: the byte value, or Null when outside the trimmed range.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The value.</returns>
    let private format10ValueOrNull (table: ReadOnlySpan<byte>) (at: int) (glyph: uint32) =
        let valueSize = int (Binary.u16 table (at + 2))
        let firstGlyph = uint32 (Binary.u16 table (at + 4))
        let glyphCount = uint32 (Binary.u16 table (at + 6))
        match firstGlyph <= glyph && glyph - firstGlyph < glyphCount with
        | false -> 0u
        | true -> readBytes table (at + 8 + int (glyph - firstGlyph) * valueSize) valueSize 0u

    /// <summary>Sanitizes a format 2 or format 6 binary-search array.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="minUnitSize">The smallest unit size the record type allows.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget.</returns>
    let private sanitizeBinSearch (table: ReadOnlySpan<byte>) (at: int) (minUnitSize: int) (ops: int) =
        match Sanitize.fits table.Length (int64 (at + 2)) 10 with
        | false -> ValueNone
        | true ->
            let unitSize = int (Binary.u16 table (at + 2))
            let nUnits = int (Binary.u16 table (at + 4))
            match unitSize >= minUnitSize with
            | false -> ValueNone
            | true -> Sanitize.array table.Length (int64 (at + 12)) (uint32 nUnits) (uint32 unitSize) ops

    /// <summary>Sanitizes the value arrays of every format 4 segment.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="unitsAt">The byte offset of the first segment.</param>
    /// <param name="unitSize">The segment size.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="count">The segment count.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget.</returns>
    [<TailCall>]
    let rec private sanitizeSegments
        (table: ReadOnlySpan<byte>) (at: int) (unitsAt: int) (unitSize: int) (valueSize: int)
        (index: int) (count: int) (ops: int) =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let unit = unitsAt + index * unitSize
            let last = int (Binary.u16 table unit)
            let first = int (Binary.u16 table (unit + 2))
            let valuesZ = int (Binary.u16 table (unit + 4))
            match first <= last with
            | false -> ValueNone
            | true ->
                match Sanitize.range table.Length (int64 (at + valuesZ)) (uint32 ((last - first + 1) * valueSize)) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeSegments table at unitsAt unitSize valueSize (index + 1) count remaining

    /// <summary>Sanitizes a format 4 segment-array lookup.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget.</returns>
    let private sanitizeFormat4 (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (ops: int) =
        match Sanitize.fits table.Length (int64 (at + 2)) 10 with
        | false -> ValueNone
        | true ->
            let unitSize = int (Binary.u16 table (at + 2))
            let nUnits = int (Binary.u16 table (at + 4))
            match unitSize >= 6 with
            | false -> ValueNone
            | true ->
                match Sanitize.array table.Length (int64 (at + 12)) (uint32 nUnits) (uint32 unitSize) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining ->
                    let count = getLength table (at + 12) unitSize nUnits 2
                    sanitizeSegments table at (at + 12) unitSize valueSize 0 count remaining

    /// <summary>Sanitizes a format 8 trimmed-array lookup.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget.</returns>
    let private sanitizeFormat8 (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (ops: int) =
        match Sanitize.fits table.Length (int64 at) 6 with
        | false -> ValueNone
        | true ->
            let glyphCount = int (Binary.u16 table (at + 4))
            Sanitize.array table.Length (int64 (at + 6)) (uint32 glyphCount) (uint32 valueSize) ops

    /// <summary>Sanitizes a format 10 byte-value lookup.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget.</returns>
    let private sanitizeFormat10 (table: ReadOnlySpan<byte>) (at: int) (ops: int) =
        match Sanitize.fits table.Length (int64 at) 8 with
        | false -> ValueNone
        | true ->
            let valueSize = int (Binary.u16 table (at + 2))
            let glyphCount = int (Binary.u16 table (at + 6))
            match valueSize >= 1 && valueSize <= 4 with
            | false -> ValueNone
            | true -> Sanitize.range table.Length (int64 (at + 8)) (uint32 (glyphCount * valueSize)) ops

    /// <summary>Sanitizes a lookup, threading the operation budget exactly as upstream does.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width, 1, 2 or 4.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="allowFormat10">Whether format 10 is accepted; the with-base variant refuses it.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget, or none when the lookup fails.</returns>
    let sanitize (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (numGlyphs: int) (allowFormat10: bool) (ops: int) =
        match Sanitize.fits table.Length (int64 at) 2 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table at with
            | 0us -> Sanitize.array table.Length (int64 (at + 2)) (uint32 numGlyphs) (uint32 valueSize) ops
            | 2us -> sanitizeBinSearch table at (4 + valueSize) ops
            | 4us -> sanitizeFormat4 table at valueSize ops
            | 6us -> sanitizeBinSearch table at (2 + valueSize) ops
            | 8us -> sanitizeFormat8 table at valueSize ops
            | 10us ->
                match allowFormat10 with
                | true -> sanitizeFormat10 table at ops
                | false -> ValueNone
            | _ -> ValueSome ops

    /// <summary>Builds a lookup value from the table bytes; the caller has sanitized the table first.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width, 1, 2 or 4.</param>
    /// <returns>The lookup.</returns>
    let read (table: ReadOnlyMemory<byte>) (at: int) (valueSize: int) =
        let format =
            match Binary.u16 table.Span at with
            | 0us -> Format0
            | 2us -> Format2
            | 4us -> Format4
            | 6us -> Format6
            | 8us -> Format8
            | 10us -> Format10
            | _ -> NullLookup
        { Table = table; At = at; Format = format; ValueSize = valueSize }

    /// <summary>The byte offset of a glyph's value, <c>Lookup::get_value</c>; none when the glyph has no value.</summary>
    /// <param name="lookup">The lookup.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The value offset.</returns>
    let valueAt (lookup: Lookup) (glyph: uint32) (numGlyphs: int) =
        let table = lookup.Table.Span
        let at = lookup.At
        match lookup.Format with
        | Format0 -> format0Value at lookup.ValueSize glyph numGlyphs
        | Format2 -> segmentValue table at (4 + lookup.ValueSize) glyph
        | Format4 -> segmentArrayValue table at lookup.ValueSize glyph
        | Format6 -> singleValue table at (2 + lookup.ValueSize) glyph
        | Format8 -> format8Value table at lookup.ValueSize glyph
        | Format10 | NullLookup -> ValueNone

    /// <summary>A glyph's value, or Null, <c>Lookup::get_value_or_null</c>; format 10 is read directly.</summary>
    /// <param name="lookup">The lookup.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The value.</returns>
    let valueOrNull (lookup: Lookup) (glyph: uint32) (numGlyphs: int) =
        let table = lookup.Table.Span
        match lookup.Format with
        | Format10 -> format10ValueOrNull table lookup.At glyph
        | _ ->
            match valueAt lookup glyph numGlyphs with
            | ValueSome offset -> valueOf table offset lookup.ValueSize
            | ValueNone -> 0u

    /// <summary>A glyph's class, or the out-of-range class, <c>Lookup::get_class</c>.</summary>
    /// <param name="lookup">The lookup.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="outOfRange">The class returned when the glyph has no value.</param>
    /// <returns>The class.</returns>
    let classOf (lookup: Lookup) (glyph: uint32) (numGlyphs: int) (outOfRange: uint16) =
        match valueAt lookup glyph numGlyphs with
        | ValueSome offset -> uint16 (valueOf lookup.Table.Span offset lookup.ValueSize)
        | ValueNone -> outOfRange

    /// <summary>The filter that keeps every value, used by the unfiltered collectors.</summary>
    [<Struct>]
    type private TrueFilter =
        interface IValueFilter with
            member _.Filter _ = true

    /// <summary>One true filter.</summary>
    let private trueFilter = TrueFilter()

    /// <summary>Counts the sorted segments of a format 2 lookup, walking with the <c>next</c> boundary.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="unitsAt">The byte offset of the first segment.</param>
    /// <param name="unitSize">The segment size.</param>
    /// <param name="count">The segment count.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="filter">The value filter.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="next">One past the last glyph added so far.</param>
    /// <param name="ops">The number of segments emitted so far.</param>
    /// <returns>The emitted count and the index of the first out-of-order segment.</returns>
    [<TailCall>]
    let rec private countSorted<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (unitsAt: int) (unitSize: int) (count: int) (valueSize: int) (filter: 'F)
        (index: int) (next: uint32) (ops: int) =
        match index >= count with
        | true -> struct (ops, index)
        | false ->
            let unit = unitsAt + index * unitSize
            let last = uint32 (Binary.u16 table unit)
            let first = uint32 (Binary.u16 table (unit + 2))
            match first < next with
            | true -> struct (ops, index)
            | false ->
                let value = valueOf table (unit + 4) valueSize
                match first = DeletedGlyph || first > last || not (filter.Filter value) with
                | true -> countSorted table unitsAt unitSize count valueSize filter (index + 1) next ops
                | false -> countSorted table unitsAt unitSize count valueSize filter (index + 1) (last + 1u) (ops + 1)

    /// <summary>Combines the out-of-order tail of a format 2 lookup into one range.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="unitsAt">The byte offset of the first segment.</param>
    /// <param name="unitSize">The segment size.</param>
    /// <param name="count">The segment count.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="filter">The value filter.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="firstMin">The smallest first glyph seen.</param>
    /// <param name="lastMax">The largest last glyph seen.</param>
    /// <returns>The combined bounds.</returns>
    [<TailCall>]
    let rec private restRange<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (unitsAt: int) (unitSize: int) (count: int) (valueSize: int) (filter: 'F)
        (index: int) (firstMin: uint32) (lastMax: uint32) =
        match index >= count with
        | true -> struct (firstMin, lastMax)
        | false ->
            let unit = unitsAt + index * unitSize
            let last = uint32 (Binary.u16 table unit)
            let first = uint32 (Binary.u16 table (unit + 2))
            let value = valueOf table (unit + 4) valueSize
            match first = DeletedGlyph || first > last || not (filter.Filter value) with
            | true -> restRange table unitsAt unitSize count valueSize filter (index + 1) firstMin lastMax
            | false -> restRange table unitsAt unitSize count valueSize filter (index + 1) (min firstMin first) (max lastMax last)

    /// <summary>The cursor of the sorted-segment collector.</summary>
    [<Struct>]
    type private SortedState =
        { Index: int
          Next: uint32 }

    /// <summary>Emits one sorted segment of a format 2 lookup per step.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="unitsAt">The byte offset of the first segment.</param>
    /// <param name="unitSize">The segment size.</param>
    /// <param name="count">The segment count.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="filter">The value filter.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="next">One past the last glyph added so far.</param>
    /// <returns>The op and the next cursor.</returns>
    [<TailCall>]
    let rec private sortedStep<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (unitsAt: int) (unitSize: int) (count: int) (valueSize: int) (filter: 'F)
        (index: int) (next: uint32) =
        match index >= count with
        | true -> struct (skipOp, { SortedState.Index = index; Next = next })
        | false ->
            let unit = unitsAt + index * unitSize
            let last = uint32 (Binary.u16 table unit)
            let first = uint32 (Binary.u16 table (unit + 2))
            match first < next with
            | true -> struct (skipOp, { SortedState.Index = index; Next = next })
            | false ->
                let value = valueOf table (unit + 4) valueSize
                match first = DeletedGlyph || first > last || not (filter.Filter value) with
                | true -> sortedStep table unitsAt unitSize count valueSize filter (index + 1) next
                | false -> struct (SetOp.Add (first, last), { SortedState.Index = index + 1; Next = last + 1u })

    /// <summary>The unfolder of the sorted-segment collector.</summary>
    [<Struct>]
    type private SortedSegmentUnfolder<'F when 'F : struct and 'F :> IValueFilter> =
        { At: int
          UnitSize: int
          Count: int
          ValueSize: int
          Filter: 'F }
        interface ISpanUnfolder<byte, SortedState, SetOp> with
            member this.Next (source, state) =
                sortedStep source this.At this.UnitSize this.Count this.ValueSize this.Filter state.Index state.Next

    /// <summary>Collects a format 2 lookup: the sorted segments, then the out-of-order tail as one range.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat2<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (filter: 'F) =
        let unitSize = int (Binary.u16 table (at + 2))
        let nUnits = int (Binary.u16 table (at + 4))
        let unitsAt = at + 12
        let count = getLength table unitsAt unitSize nUnits 2
        let struct (sortedCount, breakIndex) = countSorted table unitsAt unitSize count valueSize filter 0 0u 0
        let struct (restFirst, restLast) = restRange table unitsAt unitSize count valueSize filter breakIndex UInt32.MaxValue 0u
        let seed: SortedState = { Index = 0; Next = 0u }
        let unfolder: SortedSegmentUnfolder<'F> = { At = unitsAt; UnitSize = unitSize; Count = count; ValueSize = valueSize; Filter = filter }
        let sorted = Sealed.unfoldSpan table sortedCount seed unfolder
        match restFirst <= restLast with
        | false -> sorted
        | true -> Sealed.concat sorted (Sealed.init 1 (fun _ -> SetOp.Add (restFirst, restLast)))

    /// <summary>The unfolder of the format 4 unfiltered collector, one range per non-deleted segment.</summary>
    [<Struct>]
    type private SegmentRangeUnfolder =
        { At: int
          UnitSize: int }
        interface ISpanUnfolder<byte, int, SetOp> with
            member this.Next (source, index) =
                let unit = this.At + index * this.UnitSize
                let first = uint32 (Binary.u16 source (unit + 2))
                let last = uint32 (Binary.u16 source unit)
                let op =
                    match first = DeletedGlyph with
                    | true -> skipOp
                    | false -> SetOp.Add (first, last)
                struct (op, index + 1)

    /// <summary>Collects a format 4 lookup, one range per non-deleted segment.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat4 (table: ReadOnlySpan<byte>) (at: int) =
        let unitSize = int (Binary.u16 table (at + 2))
        let nUnits = int (Binary.u16 table (at + 4))
        let count = getLength table (at + 12) unitSize nUnits 2
        let unfolder: SegmentRangeUnfolder = { At = at + 12; UnitSize = unitSize }
        Sealed.unfoldSpan table count 0 unfolder

    /// <summary>The unfolder of the format 6 collector, one glyph per selected single.</summary>
    [<Struct>]
    type private SingleFilteredUnfolder<'F when 'F : struct and 'F :> IValueFilter> =
        { At: int
          UnitSize: int
          ValueSize: int
          Filter: 'F }
        interface ISpanUnfolder<byte, int, SetOp> with
            member this.Next (source, index) =
                let unit = this.At + index * this.UnitSize
                let glyph = uint32 (Binary.u16 source unit)
                let value = valueOf source (unit + 2) this.ValueSize
                let op =
                    match glyph = DeletedGlyph || not (this.Filter.Filter value) with
                    | true -> skipOp
                    | false -> SetOp.Add (glyph, glyph)
                struct (op, index + 1)

    /// <summary>Collects a format 6 lookup, one glyph per selected single.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat6<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (filter: 'F) =
        let unitSize = int (Binary.u16 table (at + 2))
        let nUnits = int (Binary.u16 table (at + 4))
        let count = getLength table (at + 12) unitSize nUnits 1
        let unfolder: SingleFilteredUnfolder<'F> = { At = at + 12; UnitSize = unitSize; ValueSize = valueSize; Filter = filter }
        Sealed.unfoldSpan table count 0 unfolder

    /// <summary>Collects a format 0 lookup as the single glyph range.</summary>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat0 (numGlyphs: int) =
        match numGlyphs <= 0 with
        | true -> ImmutableArray.Empty
        | false -> Sealed.init 1 (fun _ -> SetOp.Add (0u, uint32 numGlyphs - 1u))

    /// <summary>Collects a format 8 lookup as its trimmed glyph range.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat8 (table: ReadOnlySpan<byte>) (at: int) =
        let firstGlyph = uint32 (Binary.u16 table (at + 2))
        let glyphCount = uint32 (Binary.u16 table (at + 4))
        match glyphCount = 0u || firstGlyph = DeletedGlyph with
        | true -> ImmutableArray.Empty
        | false -> Sealed.init 1 (fun _ -> SetOp.Add (firstGlyph, firstGlyph + glyphCount - 1u))

    /// <summary>Collects a format 10 lookup as its trimmed glyph range.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat10 (table: ReadOnlySpan<byte>) (at: int) =
        let firstGlyph = uint32 (Binary.u16 table (at + 4))
        let glyphCount = uint32 (Binary.u16 table (at + 6))
        match glyphCount = 0u || firstGlyph = DeletedGlyph with
        | true -> ImmutableArray.Empty
        | false -> Sealed.init 1 (fun _ -> SetOp.Add (firstGlyph, firstGlyph + glyphCount - 1u))

    /// <summary>The unfolder of a contiguous trimmed-array collector, one glyph per selected value.</summary>
    [<Struct>]
    type private RangeFilteredUnfolder<'F when 'F : struct and 'F :> IValueFilter> =
        { ValueBase: int
          ValueSize: int
          StartGlyph: uint32
          Filter: 'F }
        interface ISpanUnfolder<byte, int, SetOp> with
            member this.Next (source, index) =
                let value = valueOf source (this.ValueBase + index * this.ValueSize) this.ValueSize
                let glyph = this.StartGlyph + uint32 index
                let op =
                    match this.Filter.Filter value with
                    | true -> SetOp.Add (glyph, glyph)
                    | false -> skipOp
                struct (op, index + 1)

    /// <summary>The unfolder of a format 10 collector, one glyph per selected byte value.</summary>
    [<Struct>]
    type private Format10FilteredUnfolder<'F when 'F : struct and 'F :> IValueFilter> =
        { At: int
          ValueSize: int
          StartGlyph: uint32
          Filter: 'F }
        interface ISpanUnfolder<byte, int, SetOp> with
            member this.Next (source, index) =
                let value = readBytes source (this.At + 8 + index * this.ValueSize) this.ValueSize 0u
                let glyph = this.StartGlyph + uint32 index
                let op =
                    match this.Filter.Filter value with
                    | true -> SetOp.Add (glyph, glyph)
                    | false -> skipOp
                struct (op, index + 1)

    /// <summary>Collects a format 0 lookup, one glyph per selected value.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat0Filtered<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (numGlyphs: int) (filter: 'F) =
        let unfolder: RangeFilteredUnfolder<'F> = { ValueBase = at + 2; ValueSize = valueSize; StartGlyph = 0u; Filter = filter }
        Sealed.unfoldSpan table numGlyphs 0 unfolder

    /// <summary>Collects a format 8 lookup, one glyph per selected value.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The width of one value.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat8Filtered<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (at: int) valueSize (filter: 'F) =
        let firstGlyph = uint32 (Binary.u16 table (at + 2))
        let glyphCount = int (Binary.u16 table (at + 4))
        let unfolder: RangeFilteredUnfolder<'F> = { ValueBase = at + 6; ValueSize = valueSize; StartGlyph = firstGlyph; Filter = filter }
        Sealed.unfoldSpan table glyphCount 0 unfolder

    /// <summary>Collects a format 10 lookup, one glyph per selected byte value.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat10Filtered<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (at: int) (filter: 'F) =
        let valueSize = int (Binary.u16 table (at + 2))
        let firstGlyph = uint32 (Binary.u16 table (at + 4))
        let glyphCount = int (Binary.u16 table (at + 6))
        let unfolder: Format10FilteredUnfolder<'F> = { At = at; ValueSize = valueSize; StartGlyph = firstGlyph; Filter = filter }
        Sealed.unfoldSpan table glyphCount 0 unfolder

    /// <summary>The cursor of the format 4 filtered collector, a segment and the glyph inside it.</summary>
    [<Struct>]
    type private SegmentGlyphState =
        { Segment: int
          Glyph: uint32 }

    /// <summary>Finds the first non-deleted segment at or after an index.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="unitsAt">The byte offset of the first segment.</param>
    /// <param name="unitSize">The segment size.</param>
    /// <param name="count">The segment count.</param>
    /// <param name="index">The next segment.</param>
    /// <returns>The segment and its first glyph.</returns>
    [<TailCall>]
    let rec private nextNonDeletedSegment (table: ReadOnlySpan<byte>) (unitsAt: int) (unitSize: int) (count: int) (index: int) =
        match index >= count with
        | true -> ValueNone
        | false ->
            let unit = unitsAt + index * unitSize
            let first = uint32 (Binary.u16 table (unit + 2))
            match first = DeletedGlyph with
            | true -> nextNonDeletedSegment table unitsAt unitSize count (index + 1)
            | false -> ValueSome (struct (index, first))

    /// <summary>One step of the format 4 filtered collector, advancing across segments and glyphs.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="unitsAt">The byte offset of the first segment.</param>
    /// <param name="unitSize">The segment size.</param>
    /// <param name="count">The segment count.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="filter">The value filter.</param>
    /// <param name="segment">The current segment.</param>
    /// <param name="glyph">The current glyph.</param>
    /// <returns>The op and the next cursor.</returns>
    [<TailCall>]
    let rec private format4Step<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (at: int) (unitsAt: int) (unitSize: int) (count: int) (valueSize: int) (filter: 'F)
        (segment: int) (glyph: uint32) =
        match segment >= count with
        | true -> struct (skipOp, { SegmentGlyphState.Segment = segment; Glyph = glyph })
        | false ->
            let unit = unitsAt + segment * unitSize
            let last = uint32 (Binary.u16 table unit)
            let first = uint32 (Binary.u16 table (unit + 2))
            let valuesZ = int (Binary.u16 table (unit + 4))
            match glyph <= last with
            | true ->
                let value = valueOf table (at + valuesZ + int (glyph - first) * valueSize) valueSize
                let op =
                    match filter.Filter value with
                    | true -> SetOp.Add (glyph, glyph)
                    | false -> skipOp
                struct (op, { SegmentGlyphState.Segment = segment; Glyph = glyph + 1u })
            | false ->
                match nextNonDeletedSegment table unitsAt unitSize count (segment + 1) with
                | ValueNone -> struct (skipOp, { SegmentGlyphState.Segment = segment; Glyph = glyph })
                | ValueSome (struct (nextSegment, nextFirst)) ->
                    format4Step table at unitsAt unitSize count valueSize filter nextSegment nextFirst

    /// <summary>The unfolder of the format 4 filtered collector.</summary>
    [<Struct>]
    type private Format4FilteredUnfolder<'F when 'F : struct and 'F :> IValueFilter> =
        { At: int
          UnitsAt: int
          UnitSize: int
          Count: int
          ValueSize: int
          Filter: 'F }
        interface ISpanUnfolder<byte, SegmentGlyphState, SetOp> with
            member this.Next (source, state) =
                format4Step source this.At this.UnitsAt this.UnitSize this.Count this.ValueSize this.Filter state.Segment state.Glyph

    /// <summary>The number of glyphs a format 4 lookup covers, non-deleted and non-inverted.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="unitsAt">The byte offset of the first segment.</param>
    /// <param name="unitSize">The segment size.</param>
    /// <param name="count">The segment count.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="total">The glyphs counted so far.</param>
    /// <returns>The total.</returns>
    [<TailCall>]
    let rec private countFormat4 (table: ReadOnlySpan<byte>) (unitsAt: int) (unitSize: int) (count: int) (index: int) (total: int) =
        match index >= count with
        | true -> total
        | false ->
            let unit = unitsAt + index * unitSize
            let first = uint32 (Binary.u16 table (unit + 2))
            let last = uint32 (Binary.u16 table unit)
            let add =
                match first = DeletedGlyph || first > last with
                | true -> 0
                | false -> int (last - first + 1u)
            countFormat4 table unitsAt unitSize count (index + 1) (total + add)

    /// <summary>Collects a format 4 lookup, one glyph per selected value.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The range operations.</returns>
    let private collectFormat4Filtered<'F when 'F : struct and 'F :> IValueFilter>
        (table: ReadOnlySpan<byte>) (at: int) (valueSize: int) (filter: 'F) =
        let unitSize = int (Binary.u16 table (at + 2))
        let nUnits = int (Binary.u16 table (at + 4))
        let unitsAt = at + 12
        let count = getLength table unitsAt unitSize nUnits 2
        let total = countFormat4 table unitsAt unitSize count 0 0
        match total with
        | 0 -> ImmutableArray.Empty
        | _ ->
            let seed =
                match nextNonDeletedSegment table unitsAt unitSize count 0 with
                | ValueSome (struct (segment, first)) -> { SegmentGlyphState.Segment = segment; Glyph = first }
                | ValueNone -> { SegmentGlyphState.Segment = 0; Glyph = 0u }
            let unfolder: Format4FilteredUnfolder<'F> = { At = at; UnitsAt = unitsAt; UnitSize = unitSize; Count = count; ValueSize = valueSize; Filter = filter }
            Sealed.unfoldSpan table total seed unfolder

    /// <summary>Collects the glyphs a lookup covers, <c>Lookup::collect_glyphs</c>.</summary>
    /// <param name="lookup">The lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let collectGlyphs (lookup: Lookup) (numGlyphs: int) =
        let table = lookup.Table.Span
        let at = lookup.At
        let ops =
            match lookup.Format with
            | Format0 -> collectFormat0 numGlyphs
            | Format2 -> collectFormat2 table at lookup.ValueSize trueFilter
            | Format4 -> collectFormat4 table at
            | Format6 -> collectFormat6 table at lookup.ValueSize trueFilter
            | Format8 -> collectFormat8 table at
            | Format10 -> collectFormat10 table at
            | NullLookup -> ImmutableArray.Empty
        Sealed.rangeSet (ops.AsSpan())

    /// <summary>Collects the glyphs whose value the filter selects, <c>Lookup::collect_glyphs_filtered</c>.</summary>
    /// <param name="lookup">The lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let collectGlyphsFiltered<'F when 'F : struct and 'F :> IValueFilter>
        (lookup: Lookup) (numGlyphs: int) (filter: 'F) =
        let table = lookup.Table.Span
        let at = lookup.At
        let ops =
            match lookup.Format with
            | Format0 -> collectFormat0Filtered table at lookup.ValueSize numGlyphs filter
            | Format2 -> collectFormat2 table at lookup.ValueSize filter
            | Format4 -> collectFormat4Filtered table at lookup.ValueSize filter
            | Format6 -> collectFormat6 table at lookup.ValueSize filter
            | Format8 -> collectFormat8Filtered table at lookup.ValueSize filter
            | Format10 -> collectFormat10Filtered table at filter
            | NullLookup -> ImmutableArray.Empty
        Sealed.rangeSet (ops.AsSpan())

/// <summary>Reading the obsolete AAT class table, narrow and wide.</summary>
[<RequireQualifiedAccess>]
module internal ClassTable =
    /// <summary>The width of one class, 1 byte narrow or 2 bytes wide.</summary>
    /// <param name="table">The class table.</param>
    /// <returns>The width.</returns>
    let private classSizeOf (table: ClassTable) =
        match table.Wide with
        | true -> 2
        | false -> 1

    /// <summary>Sanitizes the class table from a span, <c>ClassTable::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The class-table offset.</param>
    /// <param name="count">The class count.</param>
    /// <param name="classSize">The width of one class, 1 or 2.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget, or none when the table fails.</returns>
    let internal sanitizeSpan (table: ReadOnlySpan<byte>) (at: int) (count: int) (classSize: int) (ops: int) =
        match Sanitize.fits table.Length (int64 at) 4 with
        | false -> ValueNone
        | true -> Sanitize.array table.Length (int64 (at + 4)) (uint32 count) (uint32 classSize) ops

    /// <summary>Sanitizes the class table, <c>ClassTable::sanitize</c>.</summary>
    /// <param name="table">The class table.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget, or none when the table fails.</returns>
    let sanitize (table: ClassTable) (ops: int) =
        sanitizeSpan table.Table.Span table.At table.Count (classSizeOf table) ops

    /// <summary>A glyph's class over a precomputed span, the hot read behind <see cref="classOf"/>; the subtraction
    /// wraps as upstream does.</summary>
    /// <param name="span">The class-table bytes.</param>
    /// <param name="at">The byte offset of the class table.</param>
    /// <param name="firstGlyph">The first glyph the array covers.</param>
    /// <param name="count">The number of classes in the array.</param>
    /// <param name="wide">True for 16-bit classes, false for 8-bit.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="outOfRange">The class returned when the glyph is outside the array.</param>
    /// <returns>The class.</returns>
    let inline classOfSpan (span: ReadOnlySpan<byte>) (at: int) (firstGlyph: uint16) (count: int) (wide: bool) (glyph: uint32) (outOfRange: uint16) =
        let index = glyph - uint32 firstGlyph
        match uint32 index >= uint32 count with
        | true -> outOfRange
        | false ->
            match wide with
            | true -> uint16 (Binary.u16 span (at + 4 + int index * 2))
            | false -> uint16 (Binary.u8 span (at + 4 + int index))

    /// <summary>A glyph's class, or the out-of-range class; the subtraction wraps as upstream does.</summary>
    /// <param name="table">The class table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="outOfRange">The class returned when the glyph is outside the array.</param>
    /// <returns>The class.</returns>
    let inline classOf (table: ClassTable) (glyph: uint32) (outOfRange: uint16) =
        classOfSpan table.Table.Span table.At table.FirstGlyph table.Count table.Wide glyph outOfRange

    /// <summary>A glyph's class as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="classOf"/> there.</summary>
    /// <param name="table">The table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="outOfRange">The class of a glyph outside the table.</param>
    /// <returns>The class.</returns>
    let classOfCall (table: ClassTable) (glyph: uint32) (outOfRange: uint16) =
        classOf table glyph outOfRange

    /// <summary>The filter that keeps every class except out-of-bounds.</summary>
    [<Struct>]
    type private NotOutOfBoundsFilter =
        interface IValueFilter with
            member _.Filter value = value <> uint32 AatLookup.ClassOutOfBounds

    /// <summary>One not-out-of-bounds filter.</summary>
    let private notOutOfBounds = NotOutOfBoundsFilter()

    /// <summary>The unfolder of the class-table collector, one glyph per selected class.</summary>
    [<Struct>]
    type private ClassUnfolder<'F when 'F : struct and 'F :> IValueFilter> =
        { At: int
          FirstGlyph: uint32
          ClassSize: int
          Filter: 'F }
        interface ISpanUnfolder<byte, int, SetOp> with
            member this.Next (source, index) =
                let value = AatLookup.valueOf source (this.At + 4 + index * this.ClassSize) this.ClassSize
                let glyph = this.FirstGlyph + uint32 index
                let op =
                    match this.Filter.Filter value with
                    | true -> SetOp.Add (glyph, glyph)
                    | false -> SetOp.Add (UInt32.MaxValue, UInt32.MaxValue)
                struct (op, index + 1)

    /// <summary>Collects the glyphs whose class the filter selects, <c>ClassTable::collect_glyphs_filtered</c>.</summary>
    /// <param name="table">The class table.</param>
    /// <param name="filter">The value filter.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let collectGlyphsFiltered<'F when 'F : struct and 'F :> IValueFilter> (table: ClassTable) (filter: 'F) =
        let span = table.Table.Span
        let unfolder: ClassUnfolder<'F> = { At = table.At; FirstGlyph = uint32 table.FirstGlyph; ClassSize = classSizeOf table; Filter = filter }
        Sealed.rangeSet ((Sealed.unfoldSpan span table.Count 0 unfolder).AsSpan())

    /// <summary>Collects the glyphs whose class is not out-of-bounds, <c>ClassTable::collect_glyphs</c>.</summary>
    /// <param name="table">The class table.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let collectGlyphs (table: ClassTable) =
        collectGlyphsFiltered table notOutOfBounds
