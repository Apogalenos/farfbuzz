namespace Farfbuzz

open System

/// <summary>The result of a variation-selector lookup.</summary>
[<Struct>]
type internal GlyphVariant =
    | NotFound
    | Found of glyph: uint32
    | UseDefault

/// <summary>The cached offsets of a format 4 subtable.</summary>
[<Struct>]
type internal Format4 =
    { Start: int
      SegCount: int
      GlyphIdArrayLength: int }

/// <summary>A cmap subtable, classified by format, or a subtable the shaper ignores.</summary>
[<Struct>]
type internal CmapSubtable =
    | Format0 of format0: int
    | Format4 of format4: Format4
    | Format6 of format6: int
    | Format10 of format10: int
    | Format12 of format12: int
    | Format13 of format13: int
    | Format14 of format14: int
    | Format15 of format15: int
    | Ignored of ignoredFormat: uint16
    | NullSubtable

/// <summary>Cmap subtable sanitising, classification and glyph lookup.</summary>
[<RequireQualifiedAccess>]
module internal CmapSubtable =
    /// <summary>The size of a format 0 subtable.</summary>
    [<Literal>]
    let private Format0Size = 262

    /// <summary>The size of a format 4 header.</summary>
    [<Literal>]
    let private Format4Header = 14

    /// <summary>The size of a format 6 header.</summary>
    [<Literal>]
    let private Format6Header = 10

    /// <summary>The size of a format 10 header.</summary>
    [<Literal>]
    let private Format10Header = 20

    /// <summary>The size of a format 12/13 header.</summary>
    [<Literal>]
    let private LongHeader = 16

    /// <summary>The size of a long group.</summary>
    [<Literal>]
    let private LongGroupSize = 12

    /// <summary>The size of a format 14/15 header before its records.</summary>
    [<Literal>]
    let private VariationHeader = 10

    /// <summary>The size of one variation selector record.</summary>
    [<Literal>]
    let private VariationRecordSize = 11

    /// <summary>The size of a format 14 non-default mapping.</summary>
    [<Literal>]
    let private NonDefault14Size = 5

    /// <summary>The size of a format 15 non-default mapping.</summary>
    [<Literal>]
    let private NonDefault15Size = 6

    /// <summary>The size of a default UVS range.</summary>
    [<Literal>]
    let private DefaultRangeSize = 4

    /// <summary>Compares a codepoint with a format 4 segment.</summary>
    [<Struct>]
    type private SegmentComparer =
        { Key: uint32
          Start: int
          SegCount: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let last = uint32 (Binary.u16 data (this.Start + Format4Header + index * 2))
                let first = uint32 (Binary.u16 data (this.Start + Format4Header + this.SegCount * 2 + 2 + index * 2))
                match this.Key > last, this.Key < first with
                | true, _ -> 1
                | false, true -> -1
                | false, false -> 0

    /// <summary>Compares a codepoint with a format 12/13 group.</summary>
    [<Struct>]
    type private GroupComparer =
        { Key: uint32
          Start: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let group = this.Start + LongHeader + index * LongGroupSize
                let first = Binary.u32 data group
                let last = Binary.u32 data (group + 4)
                match this.Key < first, this.Key > last with
                | true, _ -> -1
                | false, true -> 1
                | false, false -> 0

    /// <summary>Compares a selector with a format 14/15 record.</summary>
    [<Struct>]
    type private SelectorComparer =
        { Key: uint32
          Start: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let selector = Binary.u24 data (this.Start + VariationHeader + index * VariationRecordSize)
                match this.Key < selector, this.Key > selector with
                | true, _ -> -1
                | false, true -> 1
                | false, false -> 0

    /// <summary>Compares a codepoint with a default UVS range.</summary>
    [<Struct>]
    type private DefaultComparer =
        { Key: uint32
          Start: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let range = this.Start + 4 + index * DefaultRangeSize
                let first = Binary.u24 data range
                let last = first + uint32 (Binary.u8 data (range + 3))
                match this.Key < first, this.Key > last with
                | true, _ -> -1
                | false, true -> 1
                | false, false -> 0

    /// <summary>Compares a codepoint with a non-default UVS mapping.</summary>
    [<Struct>]
    type private NonDefaultComparer =
        { Key: uint32
          Start: int
          ItemSize: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let value = Binary.u24 data (this.Start + 4 + index * this.ItemSize)
                match this.Key < value, this.Key > value with
                | true, _ -> -1
                | false, true -> 1
                | false, false -> 0

    /// <summary>Sanitizes a nullable offset to a 32-bit-length array.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="baseOffset">The subtable base.</param>
    /// <param name="offset">The stored offset, 0 for Null.</param>
    /// <param name="itemSize">The array item size.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeNullableArray (cmap: ReadOnlySpan<byte>) (baseOffset) (offset) (itemSize) (ops) : int voption =
        match offset with
        | 0u -> ValueSome ops
        | _ ->
            let target = int64 baseOffset + int64 offset
            match target > int64 UInt32.MaxValue with
            | true -> ValueNone
            | false ->
                match Sanitize.fits cmap.Length target 4 with
                | false -> ValueNone
                | true ->
                    let count = Binary.u32 cmap (int target)
                    Sanitize.array cmap.Length (target + 4L) count itemSize ops

    /// <summary>Sanitizes one variation selector record's two offset arrays.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="baseOffset">The subtable base.</param>
    /// <param name="record">The record offset.</param>
    /// <param name="nonDefaultSize">5 for format 14, 6 for format 15.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeVariationRecord (cmap: ReadOnlySpan<byte>) (baseOffset) (record) (nonDefaultSize) (ops) : int voption =
        match sanitizeNullableArray cmap baseOffset (Binary.u32 cmap (record + 3)) (uint32 DefaultRangeSize) ops with
        | ValueNone -> ValueNone
        | ValueSome remaining ->
            sanitizeNullableArray cmap baseOffset (Binary.u32 cmap (record + 7)) nonDefaultSize remaining

    /// <summary>Sanitizes every variation selector record.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="start">The subtable start.</param>
    /// <param name="count">The record count.</param>
    /// <param name="nonDefaultSize">5 for format 14, 6 for format 15.</param>
    /// <param name="index">The next record.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeVariationRecords
            (cmap: ReadOnlySpan<byte>) (start) (count) (nonDefaultSize) (index) (ops) : int voption =
        match index < int count with
        | false -> ValueSome ops
        | true ->
            match sanitizeVariationRecord cmap start (start + VariationHeader + index * VariationRecordSize) nonDefaultSize ops with
            | ValueSome remaining -> sanitizeVariationRecords cmap start count nonDefaultSize (index + 1) remaining
            | ValueNone -> ValueNone

    /// <summary>Sanitizes a format 14/15 variation subtable.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="format">The format number.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeVariation (cmap: ReadOnlySpan<byte>) (offset) (format) (ops) : int voption =
        let nonDefaultSize = match format with | 15us -> uint32 NonDefault15Size | _ -> uint32 NonDefault14Size
        match Sanitize.fits cmap.Length (int64 offset) VariationHeader with
        | false -> ValueNone
        | true ->
            let count = Binary.u32 cmap (offset + 6)
            match Sanitize.array cmap.Length (int64 offset + int64 VariationHeader) count (uint32 VariationRecordSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeVariationRecords cmap offset count nonDefaultSize 0 remaining

    /// <summary>Sanitizes a cmap subtable, <c>CmapSubtable::sanitize</c>.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (cmap: ReadOnlySpan<byte>) (offset) (ops) : int voption =
        match Sanitize.fits cmap.Length (int64 offset) 2 with
        | false -> ValueNone
        | true ->
            let format = Binary.u16 cmap offset
            match format with
            | 0us ->
                match Sanitize.fits cmap.Length (int64 offset) Format0Size with
                | true -> ValueSome ops
                | false -> ValueNone
            | 4us ->
                match Sanitize.fits cmap.Length (int64 offset) Format4Header with
                | false -> ValueNone
                | true ->
                    let segCountX2 = uint32 (Binary.u16 cmap (offset + 6))
                    Sanitize.range cmap.Length (int64 offset + int64 Format4Header) (2u + 4u * segCountX2) ops
            | 6us ->
                match Sanitize.fits cmap.Length (int64 offset) Format6Header with
                | false -> ValueNone
                | true -> Sanitize.array cmap.Length (int64 offset + int64 Format6Header) (uint32 (Binary.u16 cmap (offset + 8))) 2u ops
            | 10us ->
                match Sanitize.fits cmap.Length (int64 offset) Format10Header with
                | false -> ValueNone
                | true -> Sanitize.array cmap.Length (int64 offset + int64 Format10Header) (Binary.u32 cmap (offset + 16)) 2u ops
            | 12us | 13us ->
                match Sanitize.fits cmap.Length (int64 offset) LongHeader with
                | false -> ValueNone
                | true -> Sanitize.array cmap.Length (int64 offset + int64 LongHeader) (Binary.u32 cmap (offset + 12)) (uint32 LongGroupSize) ops
            | 14us | 15us -> sanitizeVariation cmap offset format ops
            | _ -> ValueSome ops

    /// <summary>Classifies a subtable by its format field.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <returns>The classified subtable.</returns>
    let classify (cmap: ReadOnlySpan<byte>) (offset) : CmapSubtable =
        match Binary.u16 cmap offset with
        | 0us -> Format0 offset
        | 4us ->
            let segCount = int (Binary.u16 cmap (offset + 6)) / 2
            let valuesOffset = offset + 16 + 8 * segCount
            let length = match cmap.Length > valuesOffset with | true -> (cmap.Length - valuesOffset) / 2 | false -> 0
            Format4 { Start = offset; SegCount = segCount; GlyphIdArrayLength = length }
        | 6us -> Format6 offset
        | 10us -> Format10 offset
        | 12us -> Format12 offset
        | 13us -> Format13 offset
        | 14us -> Format14 offset
        | 15us -> Format15 offset
        | format -> Ignored format

    /// <summary>Looks up a range-offset glyph in a format 4 subtable.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The segment index.</param>
    /// <param name="codepoint">The code point.</param>
    /// <param name="rangeOffset">The range offset.</param>
    /// <param name="delta">The segment delta.</param>
    /// <returns>The glyph, or 0.</returns>
    let private glyph4Range (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) (codepoint) (rangeOffset) (delta) =
        let first = uint32 (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 2 + index * 2))
        let at = rangeOffset / 2u + (codepoint - first) + uint32 index - uint32 subtable.SegCount
        match at >= uint32 subtable.GlyphIdArrayLength with
        | true -> 0u
        | false ->
            match uint32 (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 8 + int at * 2)) with
            | 0u -> 0u
            | value -> value + delta

    /// <summary>The glyph a format 4 segment gives a code point inside it.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The segment.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private glyph4At (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) (codepoint) =
        let delta = uint32 (int32 (Binary.i16 cmap (subtable.Start + 16 + subtable.SegCount * 4 + index * 2)))
        let rangeOffset = uint32 (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 6 + index * 2))
        let gid =
            match rangeOffset with
            | 0u -> codepoint + delta
            | _ -> glyph4Range cmap subtable index codepoint rangeOffset delta
        let gid = gid &&& 0xFFFFu
        match gid with
        | 0u -> ValueNone
        | _ -> ValueSome gid

    /// <summary>Looks up a codepoint in a format 4 subtable.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private glyph4 (cmap: ReadOnlySpan<byte>) (subtable: Format4) (codepoint) : uint32 voption =
        let comparer = { SegmentComparer.Key = codepoint; Start = subtable.Start; SegCount = subtable.SegCount }
        match Binary.bsearch comparer cmap 0 (subtable.SegCount - 1) with
        | ValueNone -> ValueNone
        | ValueSome index -> glyph4At cmap subtable index codepoint

    /// <summary>Looks up a codepoint in a trimmed format 6 or 10 subtable.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="header">10 for format 6, 20 for format 10.</param>
    /// <param name="wide">Whether the header uses 32-bit fields.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private glyphTrimmed (cmap: ReadOnlySpan<byte>) (offset) (header) (wide) (codepoint) : uint32 voption =
        let first, count =
            match wide with
            | true -> Binary.u32 cmap (offset + 12), Binary.u32 cmap (offset + 16)
            | false -> uint32 (Binary.u16 cmap (offset + 6)), uint32 (Binary.u16 cmap (offset + 8))
        let index = codepoint - first
        match index >= count with
        | true -> ValueNone
        | false ->
            let gid = uint32 (Binary.u16 cmap (offset + header + int index * 2))
            match gid with
            | 0u -> ValueNone
            | _ -> ValueSome gid

    /// <summary>The glyph a format 12 or 13 group gives a code point inside it.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="format13">Whether this is format 13.</param>
    /// <param name="index">The group.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private glyphLongAt (cmap: ReadOnlySpan<byte>) (offset) (format13) (index) (codepoint) =
        let group = offset + LongHeader + index * LongGroupSize
        let first = Binary.u32 cmap group
        let last = Binary.u32 cmap (group + 4)
        let glyph = Binary.u32 cmap (group + 8)
        let gid =
            match format13, first <= last with
            | true, _ -> glyph
            | false, true -> glyph + (codepoint - first)
            | false, false -> 0u
        match gid with
        | 0u -> ValueNone
        | _ -> ValueSome gid

    /// <summary>Looks up a codepoint in a format 12 or 13 group array.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="format13">Whether this is format 13.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private glyphLong (cmap: ReadOnlySpan<byte>) (offset) (format13) (codepoint) : uint32 voption =
        let count = Binary.u32 cmap (offset + 12)
        let comparer = { GroupComparer.Key = codepoint; Start = offset }
        match Binary.bsearch comparer cmap 0 (int count - 1) with
        | ValueNone -> ValueNone
        | ValueSome index -> glyphLongAt cmap offset format13 index codepoint

    /// <summary>Looks up a default UVS range.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="target">The default UVS table offset.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>True when the range contains the codepoint.</returns>
    let private defaultHas (cmap: ReadOnlySpan<byte>) (target) (codepoint) =
        let count = Binary.u32 cmap target
        let comparer = { DefaultComparer.Key = codepoint; Start = target }
        (Binary.bsearch comparer cmap 0 (int count - 1)) |> ValueOption.isSome

    /// <summary>Looks up a non-default UVS mapping.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="target">The non-default UVS table offset.</param>
    /// <param name="itemSize">5 for format 14, 6 for format 15.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The mapped glyph, or 0.</returns>
    let private nonDefaultGlyph (cmap: ReadOnlySpan<byte>) (target) (itemSize) (codepoint) =
        let count = Binary.u32 cmap target
        let comparer = { NonDefaultComparer.Key = codepoint; Start = target; ItemSize = itemSize }
        match Binary.bsearch comparer cmap 0 (int count - 1) with
        | ValueNone -> 0u
        | ValueSome index ->
            let item = target + 4 + index * itemSize
            match itemSize with
            | 5 -> uint32 (Binary.u16 cmap (item + 3))
            | _ -> Binary.u24 cmap (item + 3)

    /// <summary>Looks up a variation selector in a format 14 or 15 subtable.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="itemSize">5 for format 14, 6 for format 15.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <param name="selector">The variation selector.</param>
    /// <returns>The variant result.</returns>
    let private variantWith (cmap: ReadOnlySpan<byte>) (offset) (itemSize) (codepoint) (selector) : GlyphVariant =
        let count = Binary.u32 cmap (offset + 6)
        let comparer = { SelectorComparer.Key = selector; Start = offset }
        match Binary.bsearch comparer cmap 0 (int count - 1) with
        | ValueNone -> NotFound
        | ValueSome index ->
            let record = offset + VariationHeader + index * VariationRecordSize
            let defaultOffset = Binary.u32 cmap (record + 3)
            let nonDefaultOffset = Binary.u32 cmap (record + 7)
            let useDefault =
                match defaultOffset with
                | 0u -> false
                | value -> defaultHas cmap (offset + int value) codepoint
            match useDefault, nonDefaultOffset with
            | true, _ -> UseDefault
            | false, 0u -> NotFound
            | false, value ->
                let glyph = nonDefaultGlyph cmap (offset + int value) itemSize codepoint
                match glyph with
                | 0u -> NotFound
                | _ -> Found glyph

    /// <summary>Looks up a nominal glyph, <c>CmapSubtable::get_glyph</c>.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The classified subtable.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let glyph (cmap: ReadOnlySpan<byte>) (subtable: CmapSubtable) (codepoint) : uint32 voption =
        match subtable with
        | Format0 offset ->
            match codepoint < 256u with
            | false -> ValueNone
            | true ->
                let gid = uint32 (Binary.u8 cmap (offset + 6 + int codepoint))
                match gid with
                | 0u -> ValueNone
                | _ -> ValueSome gid
        | Format4 format4 -> glyph4 cmap format4 codepoint
        | Format6 offset -> glyphTrimmed cmap offset Format6Header false codepoint
        | Format10 offset -> glyphTrimmed cmap offset Format10Header true codepoint
        | Format12 offset -> glyphLong cmap offset false codepoint
        | Format13 offset -> glyphLong cmap offset true codepoint
        | Format14 _ | Format15 _ | Ignored _ | NullSubtable -> ValueNone

    /// <summary>Looks up a variation glyph, <c>CmapSubtable::get_glyph_variant</c>.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The classified subtable.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <param name="selector">The variation selector.</param>
    /// <returns>The variant result.</returns>
    let variant (cmap: ReadOnlySpan<byte>) (subtable: CmapSubtable) (codepoint) (selector) : GlyphVariant =
        match subtable with
        | Format14 offset -> variantWith cmap offset NonDefault14Size codepoint selector
        | Format15 offset -> variantWith cmap offset NonDefault15Size codepoint selector
        | Format0 _ | Format4 _ | Format6 _ | Format10 _ | Format12 _ | Format13 _ | Ignored _ | NullSubtable -> NotFound

    /// <summary>The glyph a range of a format 4, 12 or 13 subtable gives a code point inside it.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The classified subtable.</param>
    /// <param name="index">The segment or group.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph, or none.</returns>
    let glyphAt (cmap: ReadOnlySpan<byte>) (subtable: CmapSubtable) (index) (codepoint) =
        match subtable with
        | Format4 format4 -> glyph4At cmap format4 index codepoint
        | Format12 offset -> glyphLongAt cmap offset false index codepoint
        | Format13 offset -> glyphLongAt cmap offset true index codepoint
        | Format0 _ | Format6 _ | Format10 _ | Format14 _ | Format15 _ | Ignored _ | NullSubtable -> ValueNone

    /// <summary>The segments of a format 4 subtable as code point ranges.</summary>
    [<Struct>]
    type Format4Ranges =
        { Subtable: Format4 }
        interface ICmapRanges with
            member this.Count _ = this.Subtable.SegCount
            member this.Bounds (data, index) =
                struct (uint32 (Binary.u16 data (this.Subtable.Start + Format4Header + this.Subtable.SegCount * 2 + 2 + index * 2)),
                        uint32 (Binary.u16 data (this.Subtable.Start + Format4Header + index * 2)))
            member this.Line (data, index) =
                let rangeOffset = Binary.u16 data (this.Subtable.Start + 16 + this.Subtable.SegCount * 6 + index * 2)
                let first = uint32 (Binary.u16 data (this.Subtable.Start + Format4Header + this.Subtable.SegCount * 2 + 2 + index * 2))
                let delta = uint32 (int32 (Binary.i16 data (this.Subtable.Start + 16 + this.Subtable.SegCount * 4 + index * 2)))
                struct (rangeOffset = 0us, first + delta, 1u, 0xFFFFu)
            member this.Glyph (data, index, codepoint) =
                match glyph4At data this.Subtable index codepoint with
                | ValueSome glyph -> glyph
                | ValueNone -> 0u

    /// <summary>The groups of a format 12 or 13 subtable as code point ranges.</summary>
    [<Struct>]
    type LongRanges =
        { Offset: int
          IsFormat13: bool }
        interface ICmapRanges with
            member this.Count data = int (Binary.u32 data (this.Offset + 12))
            member this.Bounds (data, index) =
                let group = this.Offset + LongHeader + index * LongGroupSize
                struct (Binary.u32 data group, Binary.u32 data (group + 4))
            member this.Line (data, index) =
                let group = this.Offset + LongHeader + index * LongGroupSize
                let first = Binary.u32 data group
                let last = Binary.u32 data (group + 4)
                let glyph = Binary.u32 data (group + 8)
                match this.IsFormat13, first <= last with
                | true, _ -> struct (true, glyph, 0u, UInt32.MaxValue)
                | false, true -> struct (true, glyph, 1u, UInt32.MaxValue)
                | false, false -> struct (true, 0u, 0u, UInt32.MaxValue)
            member this.Glyph (data, index, codepoint) =
                match glyphLongAt data this.Offset this.IsFormat13 index codepoint with
                | ValueSome glyph -> glyph
                | ValueNone -> 0u
