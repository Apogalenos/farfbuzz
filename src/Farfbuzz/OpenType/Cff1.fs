namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Text

/// <summary>A CFF1 charset: one of the three predefined ids or a custom table offset.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal Charset =
    | Predefined of predefinedId: int
    | Custom of customOffset: int

/// <summary>The CFF1 FDSelect: a per-glyph font-dict index, or none for a non-CID font.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal Cff1FdSelect =
    | NoFdSelect // non-CID: every glyph belongs to font dict 0
    | FdSelect0 of fdsAt: int // format 0: one byte per glyph
    | FdSelect3 of rangesAt: int * rangeCount: int // format 3: sorted first-glyph ranges

/// <summary>The CFF1 accelerator: validity, charset, strings, names and the subroutines the interpreter reads.</summary>
[<NoEquality; NoComparison>]
type internal Cff1Accel =
    { Valid: bool
      IsCid: bool
      Table: ReadOnlyMemory<byte>
      Charset: Charset
      Strings: CffIndex
      GlyphCount: uint32
      CharStrings: CffIndex
      GlobalSubrs: CffIndex
      LocalSubrs: ImmutableArray<CffIndex>
      FdSelect: Cff1FdSelect
      ByName: Lazy<ImmutableArray<struct (int * uint16)>> }

/// <summary>The CFF1 table: accelerator validity, charsets and glyph names.</summary>
[<RequireQualifiedAccess>]
module internal Cff1 =
    /// <summary>The CFF1 header size.</summary>
    [<Literal>]
    let private HeaderSize = 4

    /// <summary>The number of standard CFF strings.</summary>
    [<Literal>]
    let private StandardStringCount = 391

    /// <summary>The ISOAdobe charset id.</summary>
    [<Literal>]
    let private IsoAdobeCharset = 0

    /// <summary>The Expert charset id.</summary>
    [<Literal>]
    let private ExpertCharset = 1

    /// <summary>The ExpertSubset charset id.</summary>
    [<Literal>]
    let private ExpertSubsetCharset = 2

    /// <summary>The top dict values the accelerator needs.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private TopDict =
        { CharStrings: int
          CharsetOffset: int
          EncodingOffset: int
          FdArrayOffset: int
          FdSelectOffset: int
          IsCid: bool
          PrivateSize: uint32
          PrivateOffset: int }

    /// <summary>The subroutine data the interpreter reads, loaded once with the accelerator.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private SubrData =
        { CharStrings: CffIndex
          GlobalSubrs: CffIndex
          LocalSubrs: ImmutableArray<CffIndex>
          FdSelect: Cff1FdSelect }

    /// <summary>The name lookup source.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private NameSource =
        { Table: ReadOnlyMemory<byte>
          Charset: Charset
          Strings: CffIndex
          GlyphCount: uint32 }

    /// <summary>Sanitizes the CFF1 header, <c>cff1::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops) : int voption =
        match Sanitize.fits table.Length 0L HeaderSize with
        | true when Binary.u8 table 0 = 1uy -> ValueSome ops
        | true -> ValueNone
        | false -> ValueNone

    /// <summary>The struct form of the CFF1 sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Reads an operand counted from the end of an operator's operands.</summary>
    /// <param name="fromEnd">Zero for the last operand.</param>
    /// <param name="item">The operator.</param>
    /// <returns>The operand, or none.</returns>
    let private operandAtEnd (fromEnd) (item: DictOp) : double voption =
        let index = item.Operands.Length - 1 - fromEnd
        match index >= 0 with
        | true -> ValueSome item.Operands[index]
        | false -> ValueNone

    /// <summary>Reads an operand as a CFF integer, rejecting values outside the int range.</summary>
    /// <param name="fromEnd">Zero for the last operand.</param>
    /// <param name="item">The operator.</param>
    /// <returns>The integer operand, or none.</returns>
    let private intOperand (fromEnd) (item: DictOp) : int voption =
        match operandAtEnd fromEnd item with
        | ValueSome value when value >= float Int32.MinValue && value <= float Int32.MaxValue ->
            ValueSome (int value)
        | ValueSome _ -> ValueNone
        | ValueNone -> ValueNone

    /// <summary>Reads an operand as a CFF unsigned integer, rejecting negative or out-of-range values.</summary>
    /// <param name="fromEnd">Zero for the last operand.</param>
    /// <param name="item">The operator.</param>
    /// <returns>The unsigned operand, or none.</returns>
    let private uintOperand (fromEnd) (item: DictOp) : uint32 voption =
        match intOperand fromEnd item with
        | ValueSome value when value >= 0 -> ValueSome (uint32 value)
        | ValueSome _ -> ValueNone
        | ValueNone -> ValueNone

    /// <summary>Validates every top DICT operator and collects the values the accelerator needs.</summary>
    /// <param name="ops">The interpreted top operators.</param>
    /// <param name="index">The next operator.</param>
    /// <param name="top">The values collected so far.</param>
    /// <returns>The top values, or none when an operator is invalid.</returns>
    [<TailCall>]
    let rec private validateTopOps (ops: ImmutableArray<DictOp>) (index) (top: TopDict) : TopDict voption =
        match index >= ops.Length with
        | true -> ValueSome top
        | false ->
            let item = ops[index]
            match item.Op with
            | 0 | 1 | 2 | 3 | 4 | 256 | 277 | 278 | 294 ->
                match uintOperand 0 item with
                | ValueSome _ -> validateTopOps ops (index + 1) top
                | ValueNone -> ValueNone
            | 15 ->
                match intOperand 0 item with
                | ValueSome offset -> validateTopOps ops (index + 1) { top with CharsetOffset = offset }
                | ValueNone -> ValueNone
            | 16 ->
                match intOperand 0 item with
                | ValueSome offset -> validateTopOps ops (index + 1) { top with EncodingOffset = offset }
                | ValueNone -> ValueNone
            | 17 ->
                match intOperand 0 item with
                | ValueSome offset -> validateTopOps ops (index + 1) { top with CharStrings = offset }
                | ValueNone -> ValueNone
            | 18 ->
                match intOperand 0 item, uintOperand 1 item with
                | ValueSome offset, ValueSome size ->
                    validateTopOps ops (index + 1) { top with PrivateOffset = offset; PrivateSize = size }
                | _ -> ValueNone
            | 1230 ->
                match uintOperand 2 item, uintOperand 1 item, uintOperand 0 item with
                | ValueSome _, ValueSome _, ValueSome _ -> validateTopOps ops (index + 1) { top with IsCid = true }
                | _ -> ValueNone
            | 1234 ->
                match uintOperand 0 item with
                | ValueSome _ -> validateTopOps ops (index + 1) top
                | ValueNone -> ValueNone
            | 1236 ->
                match intOperand 0 item with
                | ValueSome offset -> validateTopOps ops (index + 1) { top with FdArrayOffset = offset }
                | ValueNone -> ValueNone
            | 1237 ->
                match intOperand 0 item with
                | ValueSome offset -> validateTopOps ops (index + 1) { top with FdSelectOffset = offset }
                | ValueNone -> ValueNone
            | _ -> validateTopOps ops (index + 1) top

    /// <summary>Reads the top dict values from its interpreted operators.</summary>
    /// <param name="ops">The interpreted operators.</param>
    /// <returns>The top dict values, or none when an operator is invalid.</returns>
    let private topDictOf (ops: ImmutableArray<DictOp>) : TopDict voption =
        validateTopOps ops 0
            { CharStrings = 0
              CharsetOffset = 0
              EncodingOffset = 0
              FdArrayOffset = 0
              FdSelectOffset = 0
              IsCid = false
              PrivateSize = 0u
              PrivateOffset = 0 }

    /// <summary>Walks custom charset ranges while sanitizing.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The charset offset.</param>
    /// <param name="format">The charset format, 1 or 2.</param>
    /// <param name="size">The range record size.</param>
    /// <param name="index">The next range.</param>
    /// <param name="remaining">The glyphs still to cover.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeCharsetLoop
            (table: ReadOnlySpan<byte>) (offset) (format) (size) (index) (remaining) (ops: int) =
        match remaining with
        | 0u -> ValueSome ops
        | _ ->
            let range = int64 offset + 1L + int64 index * int64 size
            match Sanitize.fits table.Length range size with
            | false -> ValueNone
            | true ->
                let nLeft =
                    match format with
                    | 1uy -> uint32 (Binary.u8 table (int range + 2))
                    | _ -> uint32 (Binary.u16 table (int range + 2))
                let consumed = nLeft + 1u
                match consumed > remaining with
                | true -> ValueNone
                | false -> sanitizeCharsetLoop table offset format size (index + 1) (remaining - consumed) ops

    /// <summary>Sanitizes a custom charset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The charset offset.</param>
    /// <param name="glyphCount">The number of glyphs.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeCharset (table: ReadOnlySpan<byte>) (offset) (glyphCount) (ops) : int voption =
        match Sanitize.fits table.Length (int64 offset) 1 with
        | false -> ValueNone
        | true ->
            let format = Binary.u8 table offset
            match format with
            | 0uy -> Sanitize.array table.Length (int64 offset + 1L) (uint32 (max 0 (glyphCount - 1))) 2u ops
            | 1uy | 2uy ->
                let size = match format with | 1uy -> 3 | _ -> 4
                sanitizeCharsetLoop table offset format size 0 (uint32 (max 0 (glyphCount - 1))) ops
            | _ -> ValueNone

    /// <summary>Sanitizes the optional supplement of a custom encoding.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="format">The encoding format byte.</param>
    /// <param name="endOffset">The offset after the codes or ranges.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeEncodingSupplement (table: ReadOnlySpan<byte>) (format) (endOffset) (ops) : int voption =
        match format &&& 0x80uy with
        | 0uy -> ValueSome ops
        | _ ->
            match Sanitize.fits table.Length endOffset 1 with
            | false -> ValueNone
            | true -> Sanitize.array table.Length (endOffset + 1L) (uint32 (Binary.u8 table (int endOffset))) 3u ops

    /// <summary>Sanitizes a format 0 custom encoding body.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The encoding offset.</param>
    /// <param name="format">The encoding format byte.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeEncoding0 (table: ReadOnlySpan<byte>) (offset) (format) (ops) : int voption =
        match Sanitize.fits table.Length (int64 offset + 1L) 1 with
        | false -> ValueNone
        | true ->
            let count = uint32 (Binary.u8 table (offset + 1))
            let endOfCodes = int64 offset + 2L + int64 count
            match Sanitize.array table.Length (int64 offset + 2L) count 1u ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeEncodingSupplement table format endOfCodes remaining

    /// <summary>Sanitizes a format 1 custom encoding body.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The encoding offset.</param>
    /// <param name="format">The encoding format byte.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeEncoding1 (table: ReadOnlySpan<byte>) (offset) (format) (ops) : int voption =
        match Sanitize.fits table.Length (int64 offset + 1L) 1 with
        | false -> ValueNone
        | true ->
            let count = uint32 (Binary.u8 table (offset + 1))
            let endOfRanges = int64 offset + 2L + int64 count * 2L
            match Sanitize.array table.Length (int64 offset + 2L) count 2u ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeEncodingSupplement table format endOfRanges remaining

    /// <summary>Sanitizes a custom encoding.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The encoding offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeEncoding (table: ReadOnlySpan<byte>) (offset) (ops) : int voption =
        match Sanitize.fits table.Length (int64 offset) 1 with
        | false -> ValueNone
        | true ->
            let format = Binary.u8 table offset
            match format &&& 0x7Fuy with
            | 0uy -> sanitizeEncoding0 table offset format ops
            | 1uy -> sanitizeEncoding1 table offset format ops
            | _ -> ValueNone

    /// <summary>Looks up a SID in a predefined pair array.</summary>
    /// <param name="pairs">The sorted SID-to-glyph pairs.</param>
    /// <param name="sid">The SID.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The glyph, or 0.</returns>
    [<TailCall>]
    let rec private pairLookup (pairs: ImmutableArray<struct (uint16 * uint16)>) (sid) (lo) (hi) =
        match lo > hi with
        | true -> 0
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            let struct (key, glyph) = pairs[mid]
            match sid < uint32 key, sid > uint32 key with
            | true, _ -> pairLookup pairs sid lo (mid - 1)
            | false, true -> pairLookup pairs sid (mid + 1) hi
            | false, false -> int glyph

    /// <summary>Walks custom charset ranges to find a glyph's SID.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="offset">The charset offset.</param>
    /// <param name="format">The charset format.</param>
    /// <param name="size">The range record size.</param>
    /// <param name="index">The next range.</param>
    /// <param name="remaining">The glyph index inside the charset.</param>
    /// <returns>The SID, or 0.</returns>
    [<TailCall>]
    let rec private charsetRangeGlyph (span: ReadOnlySpan<byte>) (offset) (format) (size) (index) (remaining) =
        let range = offset + 1 + index * size
        let first = uint32 (Binary.u16 span range)
        let nLeft = match format with | 1uy -> uint32 (Binary.u8 span (range + 2)) | _ -> uint32 (Binary.u16 span (range + 2))
        match remaining <= nLeft with
        | true -> first + remaining
        | false -> charsetRangeGlyph span offset format size (index + 1) (remaining - nLeft - 1u)

    /// <summary>Walks custom charset ranges to find a SID's glyph.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="offset">The charset offset.</param>
    /// <param name="format">The charset format.</param>
    /// <param name="size">The range record size.</param>
    /// <param name="index">The next range.</param>
    /// <param name="sid">The SID.</param>
    /// <param name="glyph">The first glyph of the range.</param>
    /// <param name="glyphCount">The face glyph count.</param>
    /// <returns>The glyph, or 0.</returns>
    [<TailCall>]
    let rec private charsetRangeSid
            (span: ReadOnlySpan<byte>) (offset) (format) (size) (index) (sid)
            (glyph) (glyphCount) =
        match glyph >= glyphCount with
        | true -> 0u
        | false ->
            let range = offset + 1 + index * size
            let first = uint32 (Binary.u16 span range)
            let nLeft = match format with | 1uy -> uint32 (Binary.u8 span (range + 2)) | _ -> uint32 (Binary.u16 span (range + 2))
            match first <= sid && sid <= first + nLeft with
            | true -> glyph + (sid - first)
            | false -> charsetRangeSid span offset format size (index + 1) sid (glyph + nLeft + 1u) glyphCount

    /// <summary>Walks a format 0 charset to find a SID's glyph.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="offset">The charset offset.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="sid">The SID.</param>
    /// <param name="glyph">The next glyph.</param>
    /// <returns>The glyph, or 0.</returns>
    [<TailCall>]
    let rec private charsetSid0 (span: ReadOnlySpan<byte>) (offset) (glyphCount) (sid) (glyph) =
        match glyph < glyphCount with
        | false -> 0u
        | true ->
            match uint32 (Binary.u16 span (offset + 1 + (int glyph - 1) * 2)) with
            | value when value = sid -> glyph
            | _ -> charsetSid0 span offset glyphCount sid (glyph + 1u)

    /// <summary>Returns a glyph's SID from a charset.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The SID, or 0.</returns>
    let private glyphToSid (source: NameSource) (glyph) =
        let span = source.Table.Span
        match source.Charset with
        | Predefined id ->
            match id with
            | 0 -> match glyph <= 228u with | true -> glyph | false -> 0u
            | 1 -> match int glyph < CffPredefined.expertCharsetToSid.Length with | true -> uint32 CffPredefined.expertCharsetToSid[int glyph] | false -> 0u
            | 2 -> match int glyph < CffPredefined.expertSubsetCharsetToSid.Length with | true -> uint32 CffPredefined.expertSubsetCharsetToSid[int glyph] | false -> 0u
            | _ -> 0u
        | Custom offset ->
            match glyph >= source.GlyphCount, glyph with
            | true, _ -> 0u
            | false, 0u -> 0u
            | false, _ ->
                let format = Binary.u8 span offset
                match format with
                | 0uy -> uint32 (Binary.u16 span (offset + 1 + (int glyph - 1) * 2))
                | 1uy | 2uy ->
                    let size = match format with | 1uy -> 3 | _ -> 4
                    charsetRangeGlyph span offset format size 0 (glyph - 1u)
                | _ -> 0u

    /// <summary>Returns the glyph for a SID from a charset.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="sid">The SID.</param>
    /// <returns>The glyph, or 0.</returns>
    let private sidToGlyph (source: NameSource) (sid) =
        let span = source.Table.Span
        match sid with
        | 0u -> 0u
        | _ ->
            match source.Charset with
            | Predefined id ->
                match id with
                | 0 -> match sid <= 228u with | true -> sid | false -> 0u
                | 1 -> uint32 (pairLookup CffPredefined.expertCharsetSidToGid sid 0 (CffPredefined.expertCharsetSidToGid.Length - 1))
                | 2 -> uint32 (pairLookup CffPredefined.expertSubsetCharsetSidToGid sid 0 (CffPredefined.expertSubsetCharsetSidToGid.Length - 1))
                | _ -> 0u
            | Custom offset ->
                let format = Binary.u8 span offset
                match format with
                | 0uy -> charsetSid0 span offset source.GlyphCount sid 1u
                | 1uy | 2uy ->
                    let size = match format with | 1uy -> 3 | _ -> 4
                    charsetRangeSid span offset format size 0 sid 1u source.GlyphCount
                | _ -> 0u
    
    /// <summary>Returns a SID's name bytes.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="sid">The SID.</param>
    /// <returns>The name, or empty.</returns>
    let private nameOfSid (source: NameSource) (sid) : ReadOnlyMemory<byte> =
        match sid < uint32 StandardStringCount with
        | true -> ReadOnlyMemory(CffStandardStrings.strings[int sid])
        | false ->
            let struct (offset, length) = CffIndex.item source.Table.Span source.Strings (sid - uint32 StandardStringCount)
            match length with
            | 0 -> ReadOnlyMemory<byte>.Empty
            | _ -> source.Table.Slice(offset, length)

    /// <summary>Compares bytes as C <c>strncmp</c> over a maximum length, stopping at NUL.</summary>
    /// <param name="a">The first bytes.</param>
    /// <param name="b">The second bytes.</param>
    /// <param name="index">The next byte.</param>
    /// <param name="length">The maximum length.</param>
    /// <returns>The comparison result.</returns>
    [<TailCall>]
    let rec private strncmp (a: ReadOnlySpan<byte>) (b: ReadOnlySpan<byte>) (index) (length) =
        match index >= length with
        | true -> 0
        | false ->
            match a[index] = b[index], a[index] = 0uy || b[index] = 0uy with
            | true, _ -> strncmp a b (index + 1) length
            | false, true -> 0
            | false, false -> int a[index] - int b[index]

    /// <summary>Compares names as the CFF gname_t comparator does.</summary>
    /// <param name="a">The first name.</param>
    /// <param name="b">The second name.</param>
    /// <returns>The comparison result.</returns>
    let private compareNames (a: ReadOnlyMemory<byte>) (b: ReadOnlyMemory<byte>) =
        let common = min a.Length b.Length
        let prefix = strncmp a.Span b.Span 0 common
        match prefix with
        | 0 -> a.Length - b.Length
        | _ -> prefix

    /// <summary>The introsort comparer over CFF glyph ids.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private NameComparer =
        { Source: NameSource }
        interface IIndexComparer with
            member this.Compare (a, b) =
                compareNames
                    (nameOfSid this.Source (uint32 (uint16 (glyphToSid this.Source (uint32 a)))))
                    (nameOfSid this.Source (uint32 (uint16 (glyphToSid this.Source (uint32 b)))))

    /// <summary>Sorts the glyph names for the lazy index.</summary>
    /// <param name="source">The name source.</param>
    /// <returns>The sorted SID and glyph pairs.</returns>
    let private sortNames (source: NameSource) =
        match source.GlyphCount with
        | 0u -> ImmutableArray.Empty
        | count ->
            Sealed.sortInPlaceIntro (int count) { Source = source }
            |> Seq.map (fun glyph ->
                struct (int (uint16 (glyphToSid source (uint32 glyph))), glyph))
            |> Seq.toList
            |> Sealed.ofList

    /// <summary>Compares a key with a SID's name.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="sid">The SID.</param>
    /// <param name="key">The key bytes.</param>
    /// <returns>The comparison result.</returns>
    let private compareKey (source: NameSource) (sid) (key: ReadOnlySpan<byte>) =
        let name = nameOfSid source sid
        let common = min name.Length key.Length
        let prefix = strncmp key name.Span 0 common
        match prefix with
        | 0 -> key.Length - name.Length
        | _ -> prefix

    /// <summary>Binary-searches the sorted name index.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="byName">The sorted SID and glyph pairs.</param>
    /// <param name="key">The key bytes.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The SID, or none.</returns>
    [<TailCall>]
    let rec private findName (source: NameSource) (byName: ImmutableArray<struct (int * uint16)>) (key: ReadOnlySpan<byte>) (lo) (hi) =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            let struct (sid, _) = byName[mid]
            match compareKey source (uint32 sid) key with
            | comparison when comparison < 0 -> findName source byName key lo (mid - 1)
            | comparison when comparison > 0 -> findName source byName key (mid + 1) hi
            | _ -> ValueSome (uint32 sid)

    /// <summary>Selects or sanitizes the charset after the top DICT.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="face">The face.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <returns>The charset, or none when a custom charset is invalid.</returns>
    let private charsetOf (span: ReadOnlySpan<byte>) (top: TopDict) (face: Face) (remaining) : Charset voption =
        match top.CharsetOffset with
        | offset when offset <= ExpertSubsetCharset -> ValueSome (Predefined offset)
        | offset ->
            match sanitizeCharset span offset face.GlyphCount remaining with
            | ValueSome _ -> ValueSome (Custom offset)
            | ValueNone -> ValueNone

    /// <summary>Checks the top DICT encoding is valid.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <returns>True when the encoding is valid.</returns>
    let private validEncoding (span: ReadOnlySpan<byte>) (top: TopDict) (remaining) =
        match top.IsCid, top.EncodingOffset with
        | true, _ -> true
        | false, offset when offset <= 1 -> true
        | false, offset -> ValueOption.isSome (sanitizeEncoding span offset remaining)

    /// <summary>The Null subroutine INDEX of an absent Subrs operator or a zero offset.</summary>
    let private emptyIndex = { Start = 0; Count = 0u; OffSize = 0; CountSize = 2 }

    /// <summary>Reads the Subrs offset of an interpreted private DICT, or none when a Subrs operand is missing.</summary>
    /// <param name="ops">The interpreted private DICT operators.</param>
    /// <param name="index">The next operator.</param>
    /// <returns>The Subrs offset, 0 when there is no Subrs operator.</returns>
    [<TailCall>]
    let rec private privateSubrs (ops: ImmutableArray<DictOp>) (index) : int voption =
        match index >= ops.Length with
        | true -> ValueSome 0
        | false ->
            match ops[index].Op with
            | 19 -> intOperand 0 ops[index]
            | _ -> privateSubrs ops (index + 1)

    /// <summary>Interprets a private DICT and reads its Subrs offset, or none when it is invalid.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="privateOffset">The private DICT offset.</param>
    /// <param name="privateSize">The private DICT size.</param>
    /// <returns>The Subrs offset, or none.</returns>
    let private privateSubrsAt (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (privateOffset: int) (privateSize: uint32) : int voption =
        match privateSize with
        | 0u -> ValueSome 0
        | size ->
            let offset = int64 privateOffset
            let length = int64 size
            match offset < 0L || offset + length > int64 table.Length with
            | true -> ValueNone
            | false ->
                match CffDict.interpret (span.Slice(int offset, int length)) with
                | ValueNone -> ValueNone
                | ValueSome ops -> privateSubrs ops 0

    /// <summary>Reads the local subroutine INDEX at a Subrs offset relative to the private DICT; a broken INDEX is
    /// the Null empty INDEX, as <c>StructAtOffsetOrNull</c> yields.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="privateOffset">The private DICT offset.</param>
    /// <param name="subrsOffset">The Subrs offset, relative to the private DICT.</param>
    /// <param name="ops">The remaining sanitizer operations.</param>
    /// <returns>The local subroutine INDEX and the remaining operations.</returns>
    let private localSubrsIndex (span: ReadOnlySpan<byte>) (privateOffset: int) (subrsOffset: int) (ops: int) : struct (CffIndex * int) =
        match subrsOffset with
        | 0 -> struct (emptyIndex, ops)
        | _ ->
            match CffIndex.sanitize span (privateOffset + subrsOffset) 2 ops with
            | ValueSome remaining -> struct (CffIndex.read span (privateOffset + subrsOffset) 2, remaining)
            | ValueNone -> struct (emptyIndex, ops)

    /// <summary>Reads the Private operator's offset and size of an interpreted font DICT.</summary>
    /// <param name="ops">The interpreted font DICT operators.</param>
    /// <param name="index">The next operator.</param>
    /// <returns>The private offset and size, zeros when there is no Private operator.</returns>
    [<TailCall>]
    let rec private fontDictPrivate (ops: ImmutableArray<DictOp>) (index) : struct (uint32 * uint32) voption =
        match index >= ops.Length with
        | true -> ValueSome (struct (0u, 0u))
        | false ->
            match ops[index].Op with
            | 18 ->
                match uintOperand 0 ops[index], uintOperand 1 ops[index] with
                | ValueSome offset, ValueSome size -> ValueSome (struct (offset, size))
                | _ -> ValueNone
            | _ -> fontDictPrivate ops (index + 1)

    /// <summary>Reads a font DICT's local subroutine INDEX from its private DICT, <c>localSubrsOf</c>'s tail.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="fontOps">The interpreted font DICT operators.</param>
    /// <param name="ops">The remaining sanitizer operations.</param>
    /// <returns>The local subroutine INDEX and the remaining operations, or none.</returns>
    let private localSubrsFromFontDict (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (fontOps: ImmutableArray<DictOp>) (ops: int) : struct (CffIndex * int) voption =
        match fontDictPrivate fontOps 0 with
        | ValueNone -> ValueNone
        | ValueSome (struct (privateOffset, privateSize)) ->
            match privateSubrsAt span table (int privateOffset) privateSize with
            | ValueNone -> ValueNone
            | ValueSome subrsOffset -> ValueSome (localSubrsIndex span (int privateOffset) subrsOffset ops)

    /// <summary>Reads one font DICT's local subroutine INDEX.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="index">The font DICT index.</param>
    /// <param name="ops">The remaining sanitizer operations.</param>
    /// <returns>The local subroutine INDEX and the remaining operations, or none.</returns>
    let private localSubrsOf (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (fdArray: CffIndex) (index: int) (ops: int) : struct (CffIndex * int) voption =
        let struct (fontOffset, fontLength) = CffIndex.item span fdArray (uint32 index)
        match fontLength with
        | 0 -> ValueNone
        | _ ->
            match CffDict.interpret (span.Slice(fontOffset, fontLength)) with
            | ValueNone -> ValueNone
            | ValueSome fontOps -> localSubrsFromFontDict span table fontOps ops

    /// <summary>Validates every font DICT's local subroutines.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="index">The next font DICT.</param>
    /// <param name="fdCount">The font DICT count.</param>
    /// <param name="ops">The remaining sanitizer operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private validateLocalSubrs (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (fdArray: CffIndex) (index: int) (fdCount: int) (ops: int) : int voption =
        match index >= fdCount with
        | true -> ValueSome ops
        | false ->
            match localSubrsOf span table fdArray index ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (_, remaining)) -> validateLocalSubrs span table fdArray (index + 1) fdCount remaining

    /// <summary>Unfolds the local subroutine INDEX of each font DICT.</summary>
    [<Struct>]
    type private LocalSubrsUnfolder =
        val private table: ReadOnlyMemory<byte>
        val private fdArray: CffIndex

        /// <summary>Creates a local-subrs unfolder.</summary>
        /// <param name="table">The CFF table memory.</param>
        /// <param name="fdArray">The FDArray INDEX.</param>
        new (table, fdArray) = { table = table; fdArray = fdArray }

        interface ISpanUnfolder<byte, int, CffIndex> with
            member this.Next (source, index) =
                match localSubrsOf source this.table this.fdArray index (Sanitize.start source.Length) with
                | ValueSome (struct (subrs, _)) -> struct (subrs, index + 1)
                | ValueNone -> struct (emptyIndex, index + 1)

    /// <summary>Builds the local subroutine INDEX of every font DICT, after validation succeeded.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="fdCount">The font DICT count.</param>
    /// <returns>The local subroutine INDEXes.</returns>
    let private buildLocalSubrs (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (fdArray: CffIndex) (fdCount: int) : ImmutableArray<CffIndex> =
        Sealed.unfoldSpan span fdCount 0 (LocalSubrsUnfolder(table, fdArray))

    /// <summary>Walks the format-3 FDSelect ranges, checking order and bounds.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="rangesAt">The first range.</param>
    /// <param name="index">The next range.</param>
    /// <param name="rangeCount">The range count.</param>
    /// <param name="fdCount">The font DICT count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="previous">The previous range's first glyph.</param>
    /// <returns>True when every range is ordered and in bounds.</returns>
    [<TailCall>]
    let rec private sanitizeFd3Ranges (span: ReadOnlySpan<byte>) (rangesAt: int) (index: int) (rangeCount: int) (fdCount: int) (glyphCount: int) (previous: int) : bool =
        match index >= rangeCount with
        | true -> true
        | false ->
            let first = int (Binary.u16 span (rangesAt + index * 3))
            let fd = int (Binary.u8 span (rangesAt + index * 3 + 2))
            match first < glyphCount && fd < fdCount && first > previous with
            | false -> false
            | true -> sanitizeFd3Ranges span rangesAt (index + 1) rangeCount fdCount glyphCount first

    /// <summary>Sanitizes a format-3 FDSelect's ranges and sentinel, <c>FDSelect3_4::sanitize</c>'s tail.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="rangesAt">The first range.</param>
    /// <param name="rangeCount">The range count.</param>
    /// <param name="fdCount">The font DICT count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="ops">The remaining sanitizer operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeFdSelect3Tail (span: ReadOnlySpan<byte>) (rangesAt: int) (rangeCount: int) (fdCount: int) (glyphCount: int) (ops: int) : int voption =
        let sentinelAt = rangesAt + rangeCount * 3
        match Sanitize.fits span.Length (int64 sentinelAt) 2 with
        | false -> ValueNone
        | true ->
            let firstRange = int (Binary.u16 span rangesAt)
            let sentinel = int (Binary.u16 span sentinelAt)
            match rangeCount > 0
                  && firstRange = 0
                  && sentinel = glyphCount
                  && sanitizeFd3Ranges span rangesAt 0 rangeCount fdCount glyphCount -1 with
            | true -> ValueSome ops
            | false -> ValueNone

    /// <summary>Sanitizes a format-3 FDSelect, <c>FDSelect3_4::sanitize</c>.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="offset">The FDSelect offset.</param>
    /// <param name="fdCount">The font DICT count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="ops">The remaining sanitizer operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeFdSelect3 (span: ReadOnlySpan<byte>) (offset: int) (fdCount: int) (glyphCount: int) (ops: int) : int voption =
        match Sanitize.fits span.Length (int64 offset + 1L) 2 with
        | false -> ValueNone
        | true ->
            let rangeCount = int (Binary.u16 span (offset + 1))
            let rangesAt = offset + 3
            match Sanitize.array span.Length (int64 rangesAt) (uint32 rangeCount) 3u ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeFdSelect3Tail span rangesAt rangeCount fdCount glyphCount remaining

    /// <summary>Sanitizes an FDSelect, <c>FDSelect::sanitize</c>.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="offset">The FDSelect offset.</param>
    /// <param name="fdCount">The font DICT count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="ops">The remaining sanitizer operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeFdSelect (span: ReadOnlySpan<byte>) (offset: int) (fdCount: int) (glyphCount: int) (ops: int) : int voption =
        match Sanitize.fits span.Length (int64 offset) 1 with
        | false -> ValueNone
        | true ->
            match Binary.u8 span offset with
            | 0uy -> Sanitize.array span.Length (int64 offset + 1L) (uint32 glyphCount) 1u ops
            | 3uy -> sanitizeFdSelect3 span offset fdCount glyphCount ops
            | _ -> ValueNone

    /// <summary>Reads an FDSelect, <c>FDSelect::get_fd</c>'s format dispatch.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="offset">The FDSelect offset.</param>
    /// <returns>The FDSelect.</returns>
    let private readFdSelect (span: ReadOnlySpan<byte>) (offset: int) : Cff1FdSelect =
        match Binary.u8 span offset with
        | 0uy -> FdSelect0 (offset + 1)
        | _ -> FdSelect3 (offset + 3, int (Binary.u16 span (offset + 1)))

    /// <summary>Binary-searches the format-3 FDSelect ranges, <c>FDSelect3_4::get_fd</c>.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="rangesAt">The first range.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <param name="lastFd">The last range's font dict.</param>
    /// <returns>The glyph's font dict.</returns>
    [<TailCall>]
    let rec private fdRange (span: ReadOnlySpan<byte>) (rangesAt: int) (glyph: uint32) (lo: int) (hi: int) (lastFd: int) : int =
        match lo > hi with
        | true -> lastFd
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            let first = uint32 (Binary.u16 span (rangesAt + mid * 3))
            let next = uint32 (Binary.u16 span (rangesAt + (mid + 1) * 3))
            match glyph < first, glyph < next with
            | true, _ -> fdRange span rangesAt glyph lo (mid - 1) lastFd
            | false, true -> int (Binary.u8 span (rangesAt + mid * 3 + 2))
            | false, false -> fdRange span rangesAt glyph (mid + 1) hi lastFd

    /// <summary>Returns a glyph's font-dict index, <c>FDSelect::get_fd</c>.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The font-dict index.</returns>
    let fdOf (cff: Cff1Accel) (glyph: uint32) : int =
        match cff.FdSelect with
        | NoFdSelect -> 0
        | FdSelect0 fdsAt -> int (Binary.u8 cff.Table.Span (fdsAt + int glyph))
        | FdSelect3 (rangesAt, rangeCount) ->
            let span = cff.Table.Span
            let lastFd = int (Binary.u8 span (rangesAt + (rangeCount - 1) * 3 + 2))
            fdRange span rangesAt glyph 0 (rangeCount - 2) lastFd

    /// <summary>Maps a standard-encoding code to a glyph, <c>std_code_to_glyph</c>.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="code">The standard-encoding code.</param>
    /// <returns>The glyph, 0 when the code has no glyph.</returns>
    let stdCodeToGlyph (cff: Cff1Accel) (code: int) : uint32 =
        match code >= 0 && code < CffPredefined.standardEncodingToSid.Length with
        | false -> 0u
        | true ->
            let sid = uint32 CffPredefined.standardEncodingToSid[code]
            match cff.Charset with
            | Custom _ ->
                let source = { Table = cff.Table; Charset = cff.Charset; Strings = cff.Strings; GlyphCount = cff.GlyphCount }
                sidToGlyph source sid
            | Predefined offset ->
                match offset = IsoAdobeCharset && code <= 228 with
                | true -> sid
                | false -> 0u

    /// <summary>Reads a CID font's per-DICT local subroutines after the FDSelect is sanitized.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="charStrings">The CharStrings INDEX.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="fdCount">The font DICT count.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <returns>The subroutine data, or none.</returns>
    let private loadCidLocalSubrs
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (charStrings: CffIndex)
            (globalSubrs: CffIndex) (fdArray: CffIndex) (fdCount: int) (remaining) : SubrData voption =
        match validateLocalSubrs span table fdArray 0 fdCount remaining with
        | ValueNone -> ValueNone
        | ValueSome _ ->
            ValueSome
                { CharStrings = charStrings
                  GlobalSubrs = globalSubrs
                  LocalSubrs = buildLocalSubrs span table fdArray fdCount
                  FdSelect = readFdSelect span top.FdSelectOffset }

    /// <summary>Reads a CID font's FDArray, FDSelect and per-DICT local subroutines.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="charStrings">The CharStrings INDEX.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <returns>The subroutine data, or none.</returns>
    let private loadCidSubrData
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (charStrings: CffIndex)
            (globalSubrs: CffIndex) (remaining) (face: Face) : SubrData voption =
        match CffIndex.sanitize span top.FdArrayOffset 2 remaining with
        | ValueNone -> ValueNone
        | ValueSome remaining ->
            let fdArray = CffIndex.read span top.FdArrayOffset 2
            let fdCount = int fdArray.Count
            match sanitizeFdSelect span top.FdSelectOffset fdCount face.GlyphCount remaining with
            | ValueNone -> ValueNone
            | ValueSome remaining -> loadCidLocalSubrs span table top charStrings globalSubrs fdArray fdCount remaining

    /// <summary>Reads the subroutine data after the string INDEX.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="charStrings">The CharStrings INDEX.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <returns>The subroutine data, or none.</returns>
    let private loadSubrData
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (charStrings: CffIndex)
            (globalSubrs: CffIndex) (remaining) (face: Face) : SubrData voption =
        match top.IsCid with
        | false ->
            match privateSubrsAt span table top.PrivateOffset top.PrivateSize with
            | ValueNone -> ValueNone
            | ValueSome subrsOffset ->
                let struct (local, _) = localSubrsIndex span top.PrivateOffset subrsOffset remaining
                ValueSome
                    { CharStrings = charStrings
                      GlobalSubrs = globalSubrs
                      LocalSubrs = ImmutableArray.Create local
                      FdSelect = NoFdSelect }
        | true -> loadCidSubrData span table top charStrings globalSubrs remaining face

    /// <summary>Finishes loading after the charset-sensitive stages.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="strings">The string INDEX.</param>
    /// <param name="subrs">The subroutine data.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private finishCharset
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (remaining) (face: Face)
            (strings: CffIndex) (subrs: SubrData) (failure: Cff1Accel) : Cff1Accel =
        match charsetOf span top face remaining with
        | ValueNone -> failure
        | ValueSome charset ->
            match validEncoding span top remaining with
            | false -> failure
            | true ->
                let source = { Table = table; Charset = charset; Strings = strings; GlyphCount = subrs.CharStrings.Count }
                { Valid = true
                  IsCid = top.IsCid
                  Table = table
                  Charset = charset
                  Strings = strings
                  GlyphCount = subrs.CharStrings.Count
                  CharStrings = subrs.CharStrings
                  GlobalSubrs = subrs.GlobalSubrs
                  LocalSubrs = subrs.LocalSubrs
                  FdSelect = subrs.FdSelect
                  ByName = lazy (sortNames source) }

    /// <summary>Checks the CharStrings count before the charset stages.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="strings">The string INDEX.</param>
    /// <param name="subrs">The subroutine data.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private finishLoad
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (remaining) (face: Face)
            (strings: CffIndex) (subrs: SubrData) (failure: Cff1Accel) : Cff1Accel =
        match subrs.CharStrings.Count = uint32 face.GlyphCount with
        | false -> failure
        | true -> finishCharset span table top remaining face strings subrs failure

    /// <summary>Finishes loading after the string and global subroutine INDEXes are read.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="strings">The string INDEX.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private finishAfterStrings
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (remaining) (face: Face)
            (strings: CffIndex) (globalSubrs: CffIndex) (failure: Cff1Accel) : Cff1Accel =
        match top.CharStrings with
        | 0 -> failure
        | _ ->
            match CffIndex.sanitize span top.CharStrings 2 remaining with
            | ValueNone -> failure
            | ValueSome remaining ->
                let charStrings = CffIndex.read span top.CharStrings 2
                match loadSubrData span table top charStrings globalSubrs remaining face with
                | ValueNone -> failure
                | ValueSome subrs -> finishLoad span table top remaining face strings subrs failure

    /// <summary>Finishes loading after the top DICT has been read.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="top">The top DICT values.</param>
    /// <param name="topOffset">The top DICT INDEX offset.</param>
    /// <param name="topIndex">The top DICT INDEX.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private completeLoad
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (topOffset)
            (topIndex: CffIndex) (remaining) (face: Face) (failure: Cff1Accel) : Cff1Accel =
        let stringOffset = topOffset + int (CffIndex.size span topIndex)
        match CffIndex.sanitize span stringOffset 2 remaining with
        | ValueNone -> failure
        | ValueSome remaining ->
            let strings = CffIndex.read span stringOffset 2
            let subrsOffset = stringOffset + int (CffIndex.size span strings)
            let globalSubrs =
                match CffIndex.sanitize span subrsOffset 2 remaining with
                | ValueSome _ -> CffIndex.read span subrsOffset 2
                | ValueNone -> emptyIndex
            finishAfterStrings span table top remaining face strings globalSubrs failure

    /// <summary>Starts loading from the interpreted top DICT.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="topOps">The interpreted top DICT operators.</param>
    /// <param name="topOffset">The top DICT INDEX offset.</param>
    /// <param name="topIndex">The top DICT INDEX.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private loadFromTopOps
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (topOps: ImmutableArray<DictOp>) (topOffset)
            (topIndex: CffIndex) (remaining) (face: Face) (failure: Cff1Accel) : Cff1Accel =
        match topDictOf topOps with
        | ValueNone -> failure
        | ValueSome top -> completeLoad span table top topOffset topIndex remaining face failure

    /// <summary>Loads the first top DICT item.</summary>
    /// <param name="span">The CFF table bytes.</param>
    /// <param name="table">The CFF table memory.</param>
    /// <param name="topOffset">The top DICT INDEX offset.</param>
    /// <param name="topIndex">The top DICT INDEX.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private loadTopIndex
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (topOffset) (topIndex: CffIndex)
            (remaining) (face: Face) (failure: Cff1Accel) : Cff1Accel =
        match topIndex.Count with
        | 0u -> failure
        | _ ->
            let struct (topData, topLength) = CffIndex.item span topIndex 0u
            match topLength with
            | 0 -> failure
            | _ ->
                match CffDict.interpret (span.Slice(topData, topLength)) with
                | ValueNone -> failure
                | ValueSome topOps -> loadFromTopOps span table topOps topOffset topIndex remaining face failure

    /// <summary>Loads the CFF1 accelerator.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The accelerator.</returns>
    let load (face: Face) : Cff1Accel =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Cff)
        let span = table.Span
        let failure =
            { Valid = false
              IsCid = false
              Table = table
              Charset = Predefined 0
              Strings = { Start = 0; Count = 0u; OffSize = 0; CountSize = 2 }
              GlyphCount = 0u
              CharStrings = { Start = 0; Count = 0u; OffSize = 0; CountSize = 2 }
              GlobalSubrs = { Start = 0; Count = 0u; OffSize = 0; CountSize = 2 }
              LocalSubrs = ImmutableArray.Empty
              FdSelect = NoFdSelect
              ByName = lazy ImmutableArray.Empty }
        match table.Length with
        | 0 -> failure
        | _ ->
            let ops = Sanitize.start table.Length
            let headerSize = int (Binary.u8 span 2)
            match CffIndex.sanitize span headerSize 2 ops with
            | ValueNone -> failure
            | ValueSome remaining ->
                let nameIndex = CffIndex.read span headerSize 2
                let topOffset = headerSize + int (CffIndex.size span nameIndex)
                match CffIndex.sanitize span topOffset 2 remaining with
                | ValueNone -> failure
                | ValueSome remaining -> loadTopIndex span table topOffset (CffIndex.read span topOffset 2) remaining face failure

    /// <summary>Returns a glyph's name, <c>cff1::get_glyph_name</c>.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The name, or none.</returns>
    let glyphName (cff: Cff1Accel) (glyph) : ReadOnlyMemory<byte> voption =
        match cff.Valid, cff.IsCid, glyph < cff.GlyphCount with
        | false, _, _ -> ValueNone
        | _, true, _ -> ValueNone
        | _, _, false -> ValueNone
        | true, false, true ->
            let source = { Table = cff.Table; Charset = cff.Charset; Strings = cff.Strings; GlyphCount = cff.GlyphCount }
            let name = nameOfSid source (glyphToSid source glyph)
            match name.Length with
            | 0 -> ValueNone
            | _ -> ValueSome name

    /// <summary>Looks up a glyph by name, <c>cff1::get_glyph_from_name</c>.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="name">The name bytes.</param>
    /// <returns>The glyph, or none.</returns>
    let glyphFromName (cff: Cff1Accel) (name: ReadOnlySpan<byte>) : uint32 voption =
        match cff.Valid, cff.IsCid, name.Length with
        | false, _, _ -> ValueNone
        | _, true, _ -> ValueNone
        | _, _, 0 -> ValueNone
        | true, false, _ ->
            let source = { Table = cff.Table; Charset = cff.Charset; Strings = cff.Strings; GlyphCount = cff.GlyphCount }
            match findName source cff.ByName.Value name 0 (cff.ByName.Value.Length - 1) with
            | ValueNone -> ValueNone
            | ValueSome sid ->
                match sidToGlyph source sid with
                | 0u when sid <> 0u -> ValueNone
                | glyph -> ValueSome glyph
