namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>How a selected cmap subtable's values are remapped before the shaper sees them.</summary>
[<Struct>]
type internal Remap =
    | Plain
    | SymbolPua
    | SymbolArabicSimplified
    | SymbolArabicTraditional
    | MacRoman
    | MacAscii

/// <summary>The ranges of the selected cmap subtable laid out for finding the range of a code point without a search: the
/// first and last code point of each, and for each block of 256 code points the first range that does not end before it.
/// Empty when the subtable is not one the index serves or its ranges are not in order and apart.</summary>
type internal CmapIndex =
    { Firsts: ImmutableArray<uint32>
      Lasts: ImmutableArray<uint32>
      Starts: ImmutableArray<int>
      Lines: ImmutableArray<uint32>  // where each range's glyphs start when all ranges are straight lines, else empty
      Steps: ImmutableArray<uint32> }

/// <summary>The selected cmap subtables and remap strategy of a face.</summary>
type internal CmapAccel =
    { Table: ReadOnlyMemory<byte>
      Best: CmapSubtable
      Uvs: CmapSubtable
      Remap: Remap
      Latin: ImmutableArray<uint32>
      Low: Lazy<ImmutableArray<uint32>>  // built at the first lookup above U+00FF
      Index: Lazy<CmapIndex>  // built at the first lookup above U+20FF
      Pages: Memo<CmapPages>  // made at the first lookup above U+20FF, then filled a code point at a time
      Presence: Memo<PresencePages>  // made at the first presence query, then a page of 256 code points at a time
      Present: bool }

/// <summary>The cmap table: encoding records, subtable selection and nominal lookup.</summary>
[<RequireQualifiedAccess>]
module internal Cmap =
    /// <summary>The number of code points whose glyphs a face keeps in its low table, U+0000 to U+20FF.</summary>
    [<Literal>]
    let private LowLimit = 0x2100

    /// <summary>The number of code points whose glyphs a face keeps in its Latin table, U+0000 to U+00FF.</summary>
    [<Literal>]
    let private LatinLimit = 256

    /// <summary>The size of the cmap header.</summary>
    [<Literal>]
    let private HeaderSize = 4

    /// <summary>The size of one encoding record.</summary>
    [<Literal>]
    let private RecordSize = 8

    /// <summary>The offset of an encoding record's subtable.</summary>
    [<Literal>]
    let private SubtableOffset = 4

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

    /// <summary>The platform and encoding key of one encoding record.</summary>
    [<Struct>]
    type private EncodingComparer =
        { Platform: uint16
          Encoding: uint16 }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let record = HeaderSize + index * RecordSize
                let platform = Binary.u16 data record
                let encoding = Binary.u16 data (record + 2)
                match this.Platform > platform, this.Platform < platform with
                | true, _ -> 1
                | false, true -> -1
                | false, false ->
                    match this.Encoding with
                    | 0xFFFFus -> 0
                    | _ ->
                        match this.Encoding > encoding, this.Encoding < encoding with
                        | true, _ -> 1
                        | false, true -> -1
                        | false, false -> 0

    /// <summary>Sanitizes one encoding record's subtable.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="record">The record offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeRecord (cmap: ReadOnlySpan<byte>) (record) (ops) : int voption =
        match Binary.u32 cmap (record + SubtableOffset) with
        | 0u -> ValueSome ops
        | offset -> CmapSubtable.sanitize cmap (int offset) ops

    /// <summary>Sanitizes every encoding record.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="count">The record count.</param>
    /// <param name="index">The next record.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeRecords (cmap: ReadOnlySpan<byte>) (count) (index) (ops) : int voption =
        match index < count with
        | false -> ValueSome ops
        | true ->
            match sanitizeRecord cmap (HeaderSize + index * RecordSize) ops with
            | ValueSome remaining -> sanitizeRecords cmap count (index + 1) remaining
            | ValueNone -> ValueNone

    /// <summary>Sanitizes a cmap table, <c>cmap::sanitize</c>.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (cmap: ReadOnlySpan<byte>) (ops) : int voption =
        match Sanitize.fits cmap.Length 0L HeaderSize && Binary.u16 cmap 0 = 0us with
        | false -> ValueNone
        | true ->
            let count = uint32 (Binary.u16 cmap 2)
            match Sanitize.array cmap.Length (int64 HeaderSize) count (uint32 RecordSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeRecords cmap (int count) 0 remaining

    /// <summary>The struct form of the cmap sanitizer.</summary>
    [<Struct>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Finds the first subtable with a platform and encoding, <c>cmap::find_subtable</c>.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="platform">The platform id.</param>
    /// <param name="encoding">The encoding id, or 0xFFFF as a wildcard.</param>
    /// <returns>The subtable, or none.</returns>
    let private findSubtable (cmap: ReadOnlySpan<byte>) (platform) (encoding) : CmapSubtable voption =
        let count = int (Binary.u16 cmap 2)
        let comparer = { EncodingComparer.Platform = platform; Encoding = encoding }
        match Binary.bsearch comparer cmap 0 (count - 1) with
        | ValueNone -> ValueNone
        | ValueSome index ->
            match Binary.u32 cmap (HeaderSize + index * RecordSize + SubtableOffset) with
            | 0u -> ValueNone
            | offset -> ValueSome (CmapSubtable.classify cmap (int offset))

    /// <summary>Tries the selection candidates in order.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="candidates">Platform, encoding, symbol, Mac and MacRoman flags.</param>
    /// <returns>The subtable and flags.</returns>
    [<TailCall>]
    let rec private choose (cmap: ReadOnlySpan<byte>) (candidates: (uint16 * uint16 * bool * bool * bool) list) =
        match candidates with
        | [] -> struct (NullSubtable, false, false, false)
        | (platform, encoding, symbol, mac, macroman) :: rest ->
            match findSubtable cmap platform encoding with
            | ValueSome subtable -> struct (subtable, symbol, mac, macroman)
            | ValueNone -> choose cmap rest

    /// <summary>Selects the best subtable and its remap class, <c>cmap::find_best_subtable</c>.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <returns>The subtable, symbol flag, Mac flag and MacRoman flag.</returns>
    let private findBest (cmap: ReadOnlySpan<byte>) =
        choose cmap
            [ 3us, 0us, true, false, false
              3us, 10us, false, false, false
              0us, 6us, false, false, false
              0us, 4us, false, false, false
              3us, 1us, false, false, false
              0us, 3us, false, false, false
              0us, 2us, false, false, false
              0us, 1us, false, false, false
              0us, 0us, false, false, false
              1us, 0us, false, true, true
              1us, 0xFFFFus, false, true, false ]

    /// <summary>Finds the variation selector subtable, <c>cmap::find_variation_selector_subtable</c>.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="count">The number of encoding records.</param>
    /// <param name="index">The next record.</param>
    /// <param name="format14">The last format 14 subtable seen.</param>
    /// <returns>The format 14/15 subtable, or Null.</returns>
    [<TailCall>]
    let rec private findUvs (cmap: ReadOnlySpan<byte>) (count) (index) (format14: CmapSubtable) : CmapSubtable =
        match index < count with
        | false -> format14
        | true ->
            let record = HeaderSize + index * RecordSize
            match Binary.u16 cmap record, Binary.u16 cmap (record + 2), Binary.u32 cmap (record + SubtableOffset) with
            | 0us, 5us, offset when offset <> 0u ->
                let subtable = CmapSubtable.classify cmap (int offset)
                match subtable with
                | Format15 _ -> subtable
                | Format14 _ -> findUvs cmap count (index + 1) subtable
                | Format0 _ | Format4 _ | Format6 _ | Format10 _ | Format12 _ | Format13 _ | Ignored _ | NullSubtable ->
                    findUvs cmap count (index + 1) format14
            | _ -> findUvs cmap count (index + 1) format14

    /// <summary>Looks up a codepoint through the selected subtable and remap, the general path the Latin table is built from.</summary>
    /// <param name="table">The cmap bytes.</param>
    /// <param name="best">The selected subtable.</param>
    /// <param name="remap">The remap strategy.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let internal nominalGlyphGeneral (table: ReadOnlyMemory<byte>) best remap codepoint =
        let lookup codepoint = CmapSubtable.glyph table.Span best codepoint
        let viaRemap remap =
            match remap with
            | 0u -> ValueNone
            | value -> lookup value
        match remap with
        | Plain -> lookup codepoint
        | SymbolPua ->
            match lookup codepoint with
            | ValueSome glyph -> ValueSome glyph
            | ValueNone ->
                viaRemap (match codepoint <= 0xFFu with | true -> 0xF000u + codepoint | false -> 0u)
        | SymbolArabicSimplified ->
            match lookup codepoint with
            | ValueSome glyph -> ValueSome glyph
            | ValueNone -> viaRemap (uint32 (ArabicPua.simplified codepoint))
        | SymbolArabicTraditional ->
            match lookup codepoint with
            | ValueSome glyph -> ValueSome glyph
            | ValueNone -> viaRemap (uint32 (ArabicPua.traditional codepoint))
        | MacRoman ->
            match codepoint < 0x80u with
            | true -> lookup codepoint
            | false -> viaRemap (uint32 (CmapMacRoman.unicodeToMacRoman codepoint))
        | MacAscii ->
            match codepoint < 0x80u with
            | true -> lookup codepoint
            | false -> ValueNone

    /// <summary>The glyphs of the first code points, 0 where there is none, equal to the lookup of each code point alone: read
    /// range by range when the selected subtable has no remap and its ranges are in order, else looked up one by one.</summary>
    /// <param name="table">The cmap bytes.</param>
    /// <param name="best">The selected subtable.</param>
    /// <param name="remap">The remap strategy.</param>
    /// <param name="limit">The number of code points.</param>
    /// <param name="known">The glyphs already known for the first code points, empty when none.</param>
    /// <returns>The table.</returns>
    let private lowTable (table: ReadOnlyMemory<byte>) best remap (limit: int) (known: ImmutableArray<uint32>) =
        let bulk =
            match remap, best with
            | Plain, Format4 format4 -> CmapTable.lowFromRanges { CmapSubtable.Format4Ranges.Subtable = format4 } table.Span limit
            | Plain, Format12 offset -> CmapTable.lowFromRanges { CmapSubtable.LongRanges.Offset = offset; CmapSubtable.LongRanges.IsFormat13 = false } table.Span limit
            | Plain, Format13 offset -> CmapTable.lowFromRanges { CmapSubtable.LongRanges.Offset = offset; CmapSubtable.LongRanges.IsFormat13 = true } table.Span limit
            | _ -> ValueNone
        match bulk with
        | ValueSome glyphs -> glyphs
        | ValueNone ->
            Sealed.init limit (fun index ->
                match index < known.Length with
                | true -> known[index]
                | false ->
                    match nominalGlyphGeneral table best remap (uint32 index) with
                    | ValueSome glyph -> glyph
                    | ValueNone -> 0u)

    /// <summary>The blocks of 256 code points an index covers: U+0000 to U+10FFFF.</summary>
    [<Literal>]
    let private IndexBlocks = 0x1100

    /// <summary>Lays out the ranges of the selected subtable for lookups without a search, when it has no remap and is a
    /// format 4, 12 or 13 subtable with its ranges in order and apart.</summary>
    /// <param name="table">The cmap bytes.</param>
    /// <param name="best">The selected subtable.</param>
    /// <param name="remap">The remap strategy.</param>
    /// <returns>The index, empty when it does not apply.</returns>
    let private indexTable (table: ReadOnlyMemory<byte>) best remap =
        let struct (firsts, lasts, starts, lines, steps) =
            match remap, best with
            | Plain, Format4 format4 -> CmapTable.index { CmapSubtable.Format4Ranges.Subtable = format4 } table.Span IndexBlocks
            | Plain, Format12 offset -> CmapTable.index { CmapSubtable.LongRanges.Offset = offset; CmapSubtable.LongRanges.IsFormat13 = false } table.Span IndexBlocks
            | Plain, Format13 offset -> CmapTable.index { CmapSubtable.LongRanges.Offset = offset; CmapSubtable.LongRanges.IsFormat13 = true } table.Span IndexBlocks
            | _ -> struct (ImmutableArray.Empty, ImmutableArray.Empty, ImmutableArray.Empty, ImmutableArray.Empty, ImmutableArray.Empty)
        { Firsts = firsts; Lasts = lasts; Starts = starts; Lines = lines; Steps = steps }

    /// <summary>Loads the cmap table and selects its subtables.</summary>
    /// <param name="face">The face.</param>
    /// <param name="os2">The OS/2 metrics that choose the symbol remap.</param>
    /// <returns>The accelerator.</returns>
    let load (face: Face) (os2: Os2) : CmapAccel =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Cmap)
        match table.Length with
        | 0 -> { Table = table; Best = NullSubtable; Uvs = NullSubtable; Remap = Plain; Latin = ImmutableArray.Empty; Low = lazy ImmutableArray.Empty; Index = lazy { Firsts = ImmutableArray.Empty; Lasts = ImmutableArray.Empty; Starts = ImmutableArray.Empty; Lines = ImmutableArray.Empty; Steps = ImmutableArray.Empty }; Pages = Memo<CmapPages>(); Presence = Memo<PresencePages>(); Present = false }
        | _ ->
            let span = table.Span
            let struct (best, symbol, mac, macroman) = findBest span
            let uvs = findUvs span (int (Binary.u16 span 2)) 0 NullSubtable
            let remap =
                match symbol with
                | true ->
                    match Os2.fontPage os2 with
                    | 0us -> SymbolPua
                    | 0xB200us -> SymbolArabicSimplified
                    | 0xB300us -> SymbolArabicTraditional
                    | _ -> Plain
                | false ->
                    match macroman, mac with
                    | true, _ -> MacRoman
                    | false, true -> MacAscii
                    | false, false -> Plain
            let latin = lowTable table best remap LatinLimit ImmutableArray.Empty
            let low = lazy (lowTable table best remap LowLimit latin)
            let index = lazy (indexTable table best remap)
            { Table = table; Best = best; Uvs = uvs; Remap = remap; Latin = latin; Low = low; Index = index; Pages = Memo<CmapPages>(); Presence = Memo<PresencePages>(); Present = true }

    /// <summary>The first range between two indices that does not end before a code point, by halving; the ranges end in
    /// order, and the one at the upper index, or none past the last, is taken when no earlier one qualifies.</summary>
    /// <param name="lasts">The last code point of every range.</param>
    /// <param name="low">The lowest range that may qualify.</param>
    /// <param name="high">The range that qualifies when none below it does.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The range.</returns>
    [<TailCall>]
    let rec private rangeBetween (lasts: ImmutableArray<uint32>) low high (codepoint: uint32) =
        match low < high with
        | false -> low
        | true ->
            let middle = low + (high - low) / 2
            match lasts[middle] < codepoint with
            | true -> rangeBetween lasts (middle + 1) high codepoint
            | false -> rangeBetween lasts low middle codepoint

    /// <summary>Looks up a codepoint through the index of the selected subtable.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="index">The index.</param>
    /// <param name="codepoint">The codepoint, below U+110000.</param>
    /// <returns>The glyph, or none.</returns>
    let private nominalGlyphIndexed (cmap: CmapAccel) (index: CmapIndex) (codepoint: uint32) =
        let block = int (codepoint >>> 8)
        let upper = match block + 1 < index.Starts.Length with | true -> index.Starts[block + 1] | false -> index.Lasts.Length
        let range = rangeBetween index.Lasts index.Starts[block] upper codepoint
        match range < index.Firsts.Length && index.Firsts[range] <= codepoint with
        | false -> ValueNone
        | true ->
            match index.Lines.Length with
            | 0 -> CmapSubtable.glyphAt cmap.Table.Span cmap.Best range codepoint
            | _ ->
                match index.Lines[range] + index.Steps[range] * (codepoint - index.Firsts[range]) with
                | 0u -> ValueNone
                | glyph -> ValueSome glyph

    /// <summary>Looks up a codepoint the low table does not hold, through the index of the selected subtable or the subtable itself
    /// when it has no index: the reference of <see cref="nominalGlyphBeyond"/>.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private nominalGlyphUnpaged (cmap: CmapAccel) (codepoint: uint32) =
        let index = cmap.Index.Value
        match index.Starts.Length > 0 && codepoint < uint32 (index.Starts.Length * 256) with
        | true -> nominalGlyphIndexed cmap index codepoint
        | false -> nominalGlyphGeneral cmap.Table cmap.Best cmap.Remap codepoint

    /// <summary>The entry that holds a lookup's answer in a page: 1 for none, the glyph plus 2 for a glyph, 0 for a glyph too large
    /// to keep.</summary>
    /// <param name="answer">The answer.</param>
    /// <returns>The entry.</returns>
    let private pageEntry (answer: uint32 voption) =
        match answer with
        | ValueNone -> 1u
        | ValueSome glyph when glyph >= 0xFFFFFFFEu -> 0u
        | ValueSome glyph -> glyph + 2u

    /// <summary>Looks up a codepoint the low table does not hold, once for each code point of the face: the answer is kept in a page
    /// of its block and read from there the next time.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private nominalGlyphBeyond (cmap: CmapAccel) (codepoint: uint32) =
        match codepoint < 0x110000u with
        | false -> nominalGlyphUnpaged cmap codepoint
        | true ->
            let pages =
                match cmap.Pages.TryGet () with
                | ValueSome held -> held
                | ValueNone -> cmap.Pages.Set (CmapPages())
            match pages.Read codepoint with
            | 0u ->
                let answer = nominalGlyphUnpaged cmap codepoint
                pages.Write (codepoint, pageEntry answer)
                answer
            | 1u -> ValueNone
            | entry -> ValueSome (entry - 2u)

    /// <summary>Looks up a codepoint above U+00FF: U+0100 to U+20FF read the precomputed low table, the rest the index of
    /// the selected subtable, or the subtable itself when it has no index.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let private nominalGlyphWide (cmap: CmapAccel) (codepoint: uint32) =
        match codepoint < uint32 LowLimit with
        | false -> nominalGlyphBeyond cmap codepoint
        | true ->
            let low = cmap.Low.Value
            match codepoint < uint32 low.Length with
            | true ->
                match low[int codepoint] with
                | 0u -> ValueNone
                | glyph -> ValueSome glyph
            | false -> nominalGlyphBeyond cmap codepoint

    /// <summary>Looks up a codepoint: U+0000 to U+00FF read the precomputed Latin table, the rest the low table or the selected subtable.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let nominalGlyph (cmap: CmapAccel) (codepoint) : uint32 voption =
        match codepoint < 256u && cmap.Latin.Length = 256 with
        | true ->
            match cmap.Latin[int codepoint] with
            | 0u -> ValueNone
            | glyph -> ValueSome glyph
        | false -> nominalGlyphWide cmap codepoint

    /// <summary>The page of the answers a face has kept for the code points of a block, for a loop that reads the code points of a line in
    /// order: an entry is 0 for a code point not kept yet, 1 for one without a glyph and the glyph plus 2 otherwise, and a code point
    /// whose entry is 0 is asked of <see cref="nominalGlyph"/>.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="block">The block, the code point shifted right by eight, below 0x1100.</param>
    /// <returns>The page, or null when the face has kept no answer in the block.</returns>
    let pageOf (cmap: CmapAccel) (block: int) =
        match cmap.Pages.TryGet () with
        | ValueSome pages -> pages.Page block
        | ValueNone -> null

    /// <summary>The probe that builds a page of a presence table from the cmap.</summary>
    [<Struct>]
    type private CmapProbe =
        val Cmap: CmapAccel

        /// <summary>Creates the probe.</summary>
        /// <param name="cmap">The accelerator.</param>
        new (cmap) = { Cmap = cmap }

        interface IGlyphProbe with
            member this.Maps codepoint =
                match nominalGlyph this.Cmap codepoint with
                | ValueSome _ -> true
                | ValueNone -> false

    /// <summary>The presence table of a face, made the first time it is asked for and kept for the life of the face: a loop that asks
    /// <see cref="hasGlyph"/> for many code points fetches it once.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <returns>The table, which fills its pages as they are asked for.</returns>
    let presenceOf (cmap: CmapAccel) =
        match cmap.Presence.TryGet () with
        | ValueSome held -> held
        | ValueNone -> cmap.Presence.Set (PresencePages())

    /// <summary>Whether the cmap maps a code point, as <see cref="nominalGlyph"/> answers, read from the page of its block when it is
    /// below U+10000: the page is built from <see cref="nominalGlyph"/> the first time a code point of its block is asked for.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="presence">The presence table of the face, <see cref="presenceOf"/>.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the cmap has a glyph for it.</returns>
    let hasGlyph (cmap: CmapAccel) (presence: PresencePages) (codepoint: uint32) =
        match codepoint < 0x10000u with
        | false ->
            match nominalGlyph cmap codepoint with
            | ValueSome _ -> true
            | ValueNone -> false
        | true ->
            let block = int (codepoint >>> 8)
            let words =
                match presence.Page block with
                | null -> presence.Keep (block, Sealed.presenceWords (CmapProbe(cmap)) block)
                | built -> built
            words[int ((codepoint >>> 6) &&& 3u)] &&& (1UL <<< int (codepoint &&& 63u)) <> 0UL

    /// <summary>Collects format 0 add operations.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="index">The next byte.</param>
    /// <param name="ops">The operations so far, latest first.</param>
    /// <returns>The operations in order.</returns>
    [<TailCall>]
    let rec private collect0 (cmap: ReadOnlySpan<byte>) (offset) (index) (ops: SetOp list) =
        match index < 256 with
        | false -> List.rev ops
        | true ->
            let glyph = Binary.u8 cmap (offset + 6 + index)
            collect0 cmap offset (index + 1) (match glyph with | 0uy -> ops | _ -> Add (uint32 index, uint32 index) :: ops)

    /// <summary>Collects trimmed format 6/10 add operations.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="start">The first code point.</param>
    /// <param name="count">The number of entries.</param>
    /// <param name="header">The header size.</param>
    /// <param name="index">The next entry.</param>
    /// <param name="ops">The operations so far, latest first.</param>
    /// <returns>The operations in order.</returns>
    [<TailCall>]
    let rec private collectTrimmed
            (cmap: ReadOnlySpan<byte>) (offset) (start) (count) (header) (index) (ops: SetOp list) =
        match uint32 index < count with
        | false -> List.rev ops
        | true ->
            let glyph = Binary.u16 cmap (offset + header + index * 2)
            let codepoint = start + uint32 index
            collectTrimmed cmap offset start count header (index + 1)
                (match glyph with | 0us -> ops | _ -> Add (codepoint, codepoint) :: ops)

    /// <summary>Walks one format 4 segment's code points for deletions.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The segment index.</param>
    /// <param name="start">The segment start.</param>
    /// <param name="last">The segment end.</param>
    /// <param name="delta">The id delta.</param>
    /// <param name="rangeOffset">The id range offset.</param>
    /// <param name="codepoint">The next code point.</param>
    /// <param name="acc">The deletion operations so far, latest first.</param>
    /// <returns>The deletions in order.</returns>
    [<TailCall>]
    let rec private segmentDeletes
            (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) (start) (last) (delta)
            (rangeOffset) (codepoint) (acc: SetOp list) =
        match codepoint <= last with
        | false -> List.rev acc
        | true ->
            match rangeOffset with
            | 0 ->
                let deleted = ((uint32 codepoint + uint32 delta) &&& 0xFFFFu) = 0u
                let next = match deleted with | true -> Delete (uint32 codepoint, uint32 codepoint) :: acc | false -> acc
                segmentDeletes cmap subtable index start last delta rangeOffset (codepoint + 1) next
            | _ ->
                let at = uint32 rangeOffset / 2u + (uint32 codepoint - uint32 start) + uint32 index - uint32 subtable.SegCount
                match at >= uint32 subtable.GlyphIdArrayLength with
                | true -> List.rev (Delete (uint32 codepoint, uint32 last) :: acc)
                | false ->
                    let deleted = Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 8 + int at * 2) = 0us
                    let next = match deleted with | true -> Delete (uint32 codepoint, uint32 codepoint) :: acc | false -> acc
                    segmentDeletes cmap subtable index start last delta rangeOffset (codepoint + 1) next

    /// <summary>Builds the operations of one format 4 segment.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The segment index.</param>
    /// <returns>The segment's operations.</returns>
    let private segmentOps (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) : SetOp list =
        let start = int (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 2 + index * 2))
        let last = int (Binary.u16 cmap (subtable.Start + 14 + index * 2))
        let delta = int (int16 (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 4 + index * 2)))
        let rangeOffset = int (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 6 + index * 2))
        Add (uint32 start, uint32 last) :: segmentDeletes cmap subtable index start last delta rangeOffset start []

    /// <summary>Collects format 4 add and delete operations.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="segments">The segment operation lists so far, latest first.</param>
    /// <returns>The operations in order.</returns>
    [<TailCall>]
    let rec private collect4Segments (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) (segments: SetOp list list) =
        let count =
            match subtable.SegCount > 0 && Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 4 - 2) = 0xFFFFus with
            | true -> subtable.SegCount - 1
            | false -> subtable.SegCount
        match index < count with
        | false -> segments |> List.rev |> List.concat
        | true -> collect4Segments cmap subtable (index + 1) (segmentOps cmap subtable index :: segments)

    /// <summary>Reads and clips one format 12/13 group, applying the glyph-zero rules.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="format13">Whether this is format 13.</param>
    /// <param name="glyphCount">The face glyph count.</param>
    /// <param name="index">The group index.</param>
    /// <returns>The start, clipped end and first glyph, or none when skipped.</returns>
    let private longGroup
            (cmap: ReadOnlySpan<byte>) (offset) (format13) (glyphCount) (index) =
        let group = offset + LongHeader + index * LongGroupSize
        let start = Binary.u32 cmap group
        let last = min (Binary.u32 cmap (group + 4)) 0x10FFFFu
        let glyph = Binary.u32 cmap (group + 8)
        match format13, glyph, start <= last with
        | true, 0u, _ -> ValueNone
        | false, 0u, false -> ValueNone
        | _ ->
            let start, glyph =
                match glyph with
                | 0u -> start + 1u, glyph + 1u
                | _ -> start, glyph
            match uint64 glyph >= uint64 glyphCount with
            | true -> ValueNone
            | false ->
                let clipped =
                    match uint64 glyph + uint64 last - uint64 start >= uint64 glyphCount with
                    | true -> start + uint32 glyphCount - glyph
                    | false -> last
                let last = min clipped 0x10FFFFu
                match start <= last with
                | true -> ValueSome (struct (start, last, glyph))
                | false -> ValueNone

    /// <summary>Collects format 12/13 add operations.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="format13">Whether this is format 13.</param>
    /// <param name="glyphCount">The face glyph count.</param>
    /// <param name="index">The next group.</param>
    /// <param name="ops">The operations so far, latest first.</param>
    /// <returns>The operations in order.</returns>
    [<TailCall>]
    let rec private collectLong
            (cmap: ReadOnlySpan<byte>) (offset) (format13) (glyphCount) (index) (ops: SetOp list) =
        let count = int (Binary.u32 cmap (offset + 12))
        match index < count with
        | false -> List.rev ops
        | true ->
            match longGroup cmap offset format13 glyphCount index with
            | ValueNone -> collectLong cmap offset format13 glyphCount (index + 1) ops
            | ValueSome (struct (start, last, _)) -> collectLong cmap offset format13 glyphCount (index + 1) (Add (start, last) :: ops)

    /// <summary>Collects format 0 mapping pairs.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="index">The next byte.</param>
    /// <param name="pairs">The pairs so far, latest first.</param>
    /// <returns>The pairs in order.</returns>
    [<TailCall>]
    let rec private mapping0 (cmap: ReadOnlySpan<byte>) (offset) (index) (pairs: struct (uint32 * uint32) list) =
        match index < 256 with
        | false -> List.rev pairs
        | true ->
            let glyph = Binary.u8 cmap (offset + 6 + index)
            mapping0 cmap offset (index + 1)
                (match glyph with | 0uy -> pairs | _ -> struct (uint32 index, uint32 glyph) :: pairs)

    /// <summary>Collects trimmed format 6/10 mapping pairs.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="start">The first code point.</param>
    /// <param name="count">The number of entries.</param>
    /// <param name="header">The header size.</param>
    /// <param name="index">The next entry.</param>
    /// <param name="pairs">The pairs so far, latest first.</param>
    /// <returns>The pairs in order.</returns>
    [<TailCall>]
    let rec private mappingTrimmed
            (cmap: ReadOnlySpan<byte>) (offset) (start) (count) (header) (index)
            (pairs: struct (uint32 * uint32) list) =
        match uint32 index < count with
        | false -> List.rev pairs
        | true ->
            let glyph = uint32 (Binary.u16 cmap (offset + header + index * 2))
            let codepoint = start + uint32 index
            mappingTrimmed cmap offset start count header (index + 1)
                (match glyph with | 0u -> pairs | _ -> struct (codepoint, glyph) :: pairs)

    /// <summary>Adds a format 4 range-offset glyph to the accumulator.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <param name="delta">The segment delta.</param>
    /// <param name="value">The glyph-array value.</param>
    /// <param name="acc">The pairs so far, latest first.</param>
    /// <returns>The next accumulator.</returns>
    let private pairRangeValue (codepoint) (delta) (value) (acc: struct (uint32 * uint32) list) =
        match value with
        | 0us -> acc
        | _ -> struct (uint32 codepoint, (uint32 value + uint32 delta) &&& 0xFFFFu) :: acc

    /// <summary>Walks one format 4 segment's code points for mapping pairs.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The segment index.</param>
    /// <param name="start">The segment start.</param>
    /// <param name="last">The segment end.</param>
    /// <param name="delta">The id delta.</param>
    /// <param name="rangeOffset">The id range offset.</param>
    /// <param name="codepoint">The next code point.</param>
    /// <param name="acc">The pairs so far, latest first.</param>
    /// <returns>The pairs in order.</returns>
    [<TailCall>]
    let rec private segmentPairLoop
            (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) (start) (last) (delta)
            (rangeOffset) (codepoint) (acc: struct (uint32 * uint32) list) =
        match codepoint <= last with
        | false -> List.rev acc
        | true ->
            match rangeOffset with
            | 0 ->
                let glyph = (uint32 codepoint + uint32 delta) &&& 0xFFFFu
                let next = match glyph with | 0u -> acc | _ -> struct (uint32 codepoint, glyph) :: acc
                segmentPairLoop cmap subtable index start last delta rangeOffset (codepoint + 1) next
            | _ ->
                let at = uint32 rangeOffset / 2u + (uint32 codepoint - uint32 start) + uint32 index - uint32 subtable.SegCount
                match at >= uint32 subtable.GlyphIdArrayLength with
                | true -> List.rev acc
                | false ->
                    let value = Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 8 + int at * 2)
                    segmentPairLoop cmap subtable index start last delta rangeOffset (codepoint + 1) (pairRangeValue codepoint delta value acc)

    /// <summary>Builds the mapping pairs of one format 4 segment.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The segment index.</param>
    /// <returns>The segment's pairs.</returns>
    let private segmentPairs (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) : struct (uint32 * uint32) list =
        let start = int (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 2 + index * 2))
        let last = int (Binary.u16 cmap (subtable.Start + 14 + index * 2))
        let delta = int (int16 (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 4 + index * 2)))
        let rangeOffset = int (Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 6 + index * 2))
        segmentPairLoop cmap subtable index start last delta rangeOffset start []

    /// <summary>Collects format 4 mapping pairs.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="subtable">The format 4 offsets.</param>
    /// <param name="index">The next segment.</param>
    /// <param name="segments">The segment pair lists so far, latest first.</param>
    /// <returns>The pairs in order.</returns>
    [<TailCall>]
    let rec private mapping4Segments
            (cmap: ReadOnlySpan<byte>) (subtable: Format4) (index) (segments: struct (uint32 * uint32) list list) =
        let count =
            match subtable.SegCount > 0 && Binary.u16 cmap (subtable.Start + 16 + subtable.SegCount * 4 - 2) = 0xFFFFus with
            | true -> subtable.SegCount - 1
            | false -> subtable.SegCount
        match index < count with
        | false -> segments |> List.rev |> List.concat
        | true -> mapping4Segments cmap subtable (index + 1) (segmentPairs cmap subtable index :: segments)

    /// <summary>Adds the mapping pairs of one clipped long group.</summary>
    /// <param name="start">The first code point.</param>
    /// <param name="last">The last code point.</param>
    /// <param name="glyph">The first glyph.</param>
    /// <param name="increment">1 for format 12, 0 for format 13.</param>
    /// <param name="pairs">The pairs so far, latest first.</param>
    /// <returns>The pairs in reverse order.</returns>
    [<TailCall>]
    let rec private longPairs (start) (last) (glyph) (increment) (pairs: struct (uint32 * uint32) list) =
        match start <= last with
        | false -> pairs
        | true -> longPairs (start + 1u) last (glyph + increment) increment (struct (start, glyph) :: pairs)

    /// <summary>Collects format 12/13 mapping pairs.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="format13">Whether this is format 13.</param>
    /// <param name="glyphCount">The face glyph count.</param>
    /// <param name="index">The next group.</param>
    /// <param name="lastEnd">The end of the last accepted group.</param>
    /// <param name="pairs">The pairs so far, latest first.</param>
    /// <returns>The pairs in order.</returns>
    [<TailCall>]
    let rec private mappingLong
            (cmap: ReadOnlySpan<byte>) (offset) (format13) (glyphCount) (index) (lastEnd)
            (pairs: struct (uint32 * uint32) list) =
        let count = int (Binary.u32 cmap (offset + 12))
        match index < count with
        | false -> List.rev pairs
        | true ->
            let group = offset + LongHeader + index * LongGroupSize
            let start = Binary.u32 cmap group
            let groupEnd = min (Binary.u32 cmap (group + 4)) 0x10FFFFu
            match start > groupEnd || start < lastEnd with
            | true -> mappingLong cmap offset format13 glyphCount (index + 1) lastEnd pairs
            | false ->
                match longGroup cmap offset format13 glyphCount index with
                | ValueNone -> mappingLong cmap offset format13 glyphCount (index + 1) groupEnd pairs
                | ValueSome (struct (first, last, glyph)) ->
                    let increment = match format13 with | true -> 0u | false -> 1u
                    mappingLong cmap offset format13 glyphCount (index + 1) groupEnd (longPairs first last glyph increment pairs)

    /// <summary>Collects covered code points, <c>cmap::collect_unicodes</c>.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="glyphCount">The face glyph count.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let collectUnicodes (cmap: CmapAccel) (glyphCount) : ImmutableArray<CodepointRange> =
        let span = cmap.Table.Span
        let ops =
            match cmap.Best with
            | Format0 offset -> collect0 span offset 0 []
            | Format4 subtable -> collect4Segments span subtable 0 []
            | Format6 offset -> collectTrimmed span offset (uint32 (Binary.u16 span (offset + 6))) (uint32 (Binary.u16 span (offset + 8))) Format6Header 0 []
            | Format10 offset -> collectTrimmed span offset (Binary.u32 span (offset + 12)) (Binary.u32 span (offset + 16)) Format10Header 0 []
            | Format12 offset -> collectLong span offset false glyphCount 0 []
            | Format13 offset -> collectLong span offset true glyphCount 0 []
            | Format14 _ | Format15 _ | Ignored _ | NullSubtable -> []
        Sealed.rangeSet ((Sealed.ofList ops).AsSpan())

    /// <summary>Collects the Unicode-to-glyph mapping, <c>cmap::collect_mapping</c>.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="glyphCount">The face glyph count.</param>
    /// <returns>The sorted pairs, last value winning.</returns>
    let collectMapping (cmap: CmapAccel) (glyphCount) : ImmutableArray<struct (uint32 * uint32)> =
        let span = cmap.Table.Span
        let pairs =
            match cmap.Best with
            | Format0 offset -> mapping0 span offset 0 []
            | Format4 subtable -> mapping4Segments span subtable 0 []
            | Format6 offset -> mappingTrimmed span offset (uint32 (Binary.u16 span (offset + 6))) (uint32 (Binary.u16 span (offset + 8))) Format6Header 0 []
            | Format10 offset -> mappingTrimmed span offset (Binary.u32 span (offset + 12)) (Binary.u32 span (offset + 16)) Format10Header 0 []
            | Format12 offset -> mappingLong span offset false glyphCount 0 0u []
            | Format13 offset -> mappingLong span offset true glyphCount 0 0u []
            | Format14 _ | Format15 _ | Ignored _ | NullSubtable -> []
        Sealed.sortedMap ((Sealed.ofList pairs).AsSpan())

    /// <summary>Compares a selector with a format 14/15 record.</summary>
    [<Struct>]
    type private SelectorComparer =
        { Key: uint32
          Start: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let selector = Binary.u24 data (this.Start + 10 + index * 11)
                match this.Key < selector, this.Key > selector with
                | true, _ -> -1
                | false, true -> 1
                | false, false -> 0

    /// <summary>Finds a variation selector record.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="selector">The selector.</param>
    /// <returns>The record offset, or none.</returns>
    let private findVariationRecord (cmap: ReadOnlySpan<byte>) (offset) (selector) : int voption =
        let count = int (Binary.u32 cmap (offset + 6))
        let comparer = { SelectorComparer.Key = selector; Start = offset }
        match Binary.bsearch comparer cmap 0 (count - 1) with
        | ValueNone -> ValueNone
        | ValueSome index -> ValueSome (offset + 10 + index * 11)

    /// <summary>Collects selector add operations.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="count">The record count.</param>
    /// <param name="index">The next record.</param>
    /// <param name="ops">The operations so far, latest first.</param>
    /// <returns>The operations in order.</returns>
    [<TailCall>]
    let rec private selectorOps (cmap: ReadOnlySpan<byte>) (offset) (count) (index) (ops: SetOp list) =
        match index < count with
        | false -> List.rev ops
        | true ->
            let selector = Binary.u24 cmap (offset + 10 + index * 11)
            selectorOps cmap offset count (index + 1) (Add (selector, selector) :: ops)

    /// <summary>Collects default UVS range operations.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="target">The default table offset.</param>
    /// <param name="count">The range count.</param>
    /// <param name="index">The next range.</param>
    /// <param name="ops">The operations so far, latest first.</param>
    /// <returns>The operations in order.</returns>
    [<TailCall>]
    let rec private defaultOps (cmap: ReadOnlySpan<byte>) (target) (count) (index) (ops: SetOp list) =
        match index < count with
        | false -> List.rev ops
        | true ->
            let first = Binary.u24 cmap (target + 4 + index * 4)
            let last = min (first + uint32 (Binary.u8 cmap (target + 4 + index * 4 + 3))) 0x10FFFFu
            defaultOps cmap target count (index + 1) (Add (first, last) :: ops)

    /// <summary>Collects non-default UVS mapping operations.</summary>
    /// <param name="cmap">The cmap bytes.</param>
    /// <param name="target">The non-default table offset.</param>
    /// <param name="count">The mapping count.</param>
    /// <param name="itemSize">5 for format 14, 6 for format 15.</param>
    /// <param name="index">The next mapping.</param>
    /// <param name="ops">The operations so far, latest first.</param>
    /// <returns>The operations in order.</returns>
    [<TailCall>]
    let rec private nonDefaultOps (cmap: ReadOnlySpan<byte>) (target) (count) (itemSize) (index) (ops: SetOp list) =
        match index < count with
        | false -> List.rev ops
        | true ->
            let codepoint = Binary.u24 cmap (target + 4 + index * itemSize)
            nonDefaultOps cmap target count itemSize (index + 1) (Add (codepoint, codepoint) :: ops)

    /// <summary>Returns the variation record item size for the format.</summary>
    /// <param name="uvs">The variation-selector subtable.</param>
    /// <returns>The item size.</returns>
    let private variationItemSize (uvs: CmapSubtable) =
        match uvs with
        | Format15 _ -> 6
        | Format14 _ -> 5
        | Format0 _ | Format4 _ | Format6 _ | Format10 _ | Format12 _ | Format13 _ | Ignored _ | NullSubtable -> 5

    /// <summary>Collects variation selectors, <c>cmap::collect_variation_selectors</c>.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <returns>The selectors.</returns>
    let collectVariationSelectors (cmap: CmapAccel) : ImmutableArray<CodepointRange> =
        let span = cmap.Table.Span
        let ops =
            match cmap.Uvs with
            | Format14 offset | Format15 offset ->
                let count = int (Binary.u32 span (offset + 6))
                selectorOps span offset count 0 []
            | Format0 _ | Format4 _ | Format6 _ | Format10 _ | Format12 _ | Format13 _ | Ignored _ | NullSubtable -> []
        Sealed.rangeSet ((Sealed.ofList ops).AsSpan())

    /// <summary>Collects the code points of one variation selector.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="selector">The selector.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let collectVariationUnicodes (cmap: CmapAccel) (selector) : ImmutableArray<CodepointRange> =
        let span = cmap.Table.Span
        let ops =
            match cmap.Uvs with
            | Format14 offset | Format15 offset ->
                match findVariationRecord span offset selector with
                | ValueNone -> []
                | ValueSome record ->
                    let defaultList =
                        match Binary.u32 span (record + 3) with
                        | 0u -> []
                        | value ->
                            let target = offset + int value
                            defaultOps span target (int (Binary.u32 span target)) 0 []
                    let nonDefaultList =
                        match Binary.u32 span (record + 7) with
                        | 0u -> []
                        | value ->
                            let target = offset + int value
                            nonDefaultOps span target (int (Binary.u32 span target)) (variationItemSize cmap.Uvs) 0 []
                    defaultList @ nonDefaultList
            | Format0 _ | Format4 _ | Format6 _ | Format10 _ | Format12 _ | Format13 _ | Ignored _ | NullSubtable -> []
        Sealed.rangeSet ((Sealed.ofList ops).AsSpan())

    /// <summary>Looks up a variation glyph, <c>hb_ot_get_variation_glyph</c>.</summary>
    /// <param name="cmap">The accelerator.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <param name="selector">The variation selector.</param>
    /// <returns>The glyph, or none.</returns>
    let variationGlyph (cmap: CmapAccel) (codepoint) (selector) : uint32 voption =
        match CmapSubtable.variant cmap.Table.Span cmap.Uvs codepoint selector with
        | Found glyph -> ValueSome glyph
        | NotFound -> ValueNone
        | UseDefault -> nominalGlyph cmap codepoint
