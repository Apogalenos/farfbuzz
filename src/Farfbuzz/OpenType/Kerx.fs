namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.Aat

/// <summary>One subtable of the kerx table, <c>KerxSubTable</c>: its header fields, the table bytes it reads
/// through, the sanitized range it may read, its two glyph sets and the state machine of formats 1 and 4.</summary>
[<NoComparison; NoEquality>]
type internal KerxSubtable =
    { Format: int
      Coverage: uint32
      TupleCount: uint32
      Body: ReadOnlyMemory<byte>  // the whole kerx table; offsets resolve relative to At
      At: int                    // the subtable's absolute byte offset within the table
      RangeEnd: int              // the exclusive bound of run-time reads (At + length, or the table length for the last)
      KernActionAt: int          // the byte offset of the format 1 kern-action array or the format 4 action data, -1 for other formats
      FirstSet: ImmutableArray<CodepointRange>
      SecondSet: ImmutableArray<CodepointRange>
      Machine: StateMachine voption
      Dense: Lazy<DenseMachine voption>
      Pairs: Lazy<PairIndex voption> } // the format 0 pairs compiled, none for another format or a table that does not compile

/// <summary>The extended kerning table, <c>AAT::kerx</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Kerx =
    { Version: uint16
      Subtables: ImmutableArray<KerxSubtable>
      Candidates: Lazy<ChainCandidates voption> } // the bit table of the subtables' first glyph sets, or none when they reach past the cap

/// <summary>Reading the kerx table, <c>AAT::kerx</c>.</summary>
[<RequireQualifiedAccess>]
module internal Kerx =
    /// <summary>The size of the kerx header.</summary>
    [<Literal>]
    let private HeaderSize = 8

    /// <summary>The size of one subtable header, <c>KerxSubTableHeader::static_size</c>.</summary>
    [<Literal>]
    let private SubtableHeaderSize = 12

    /// <summary>The largest subtable count a table may declare, <c>HB_AAT_MAX_SUBTABLES</c>.</summary>
    [<Literal>]
    let private MaxSubtables = 1024

    /// <summary>The smallest version a kerx table may declare, <c>kerx::minVersion</c>.</summary>
    [<Literal>]
    let private MinVersion = 2u

    /// <summary>The version from which a coverage table follows the subtables, <c>SubtableGlyphCoverage</c>.</summary>
    [<Literal>]
    let private CoverageVersion = 3u

    /// <summary>The byte width of one format 0 pair, <c>KernPair::static_size</c>.</summary>
    [<Literal>]
    let private PairSize = 6

    /// <summary>The byte width of one FWORD, <c>FWORD::static_size</c>.</summary>
    [<Literal>]
    let private FwordSize = 2

    /// <summary>The mask of the byte offset of the format 4 action data, <c>driver_context_t::Offset</c>.</summary>
    [<Literal>]
    let private ActionDataMask = 0x00FFFFFFu

    /// <summary>The byte width of one FWORD32, <c>FWORD32::static_size</c>.</summary>
    [<Literal>]
    let private Fword32Size = 4

    /// <summary>The format 6 values-are-long flag, <c>KerxSubTableFormat6::ValuesAreLong</c>.</summary>
    [<Literal>]
    let private ValuesAreLong = 0x00000001u

    /// <summary>The empty table, returned when the table is absent or fails to sanitize.</summary>
    let private empty =
        { Version = 0us
          Subtables = ImmutableArray.Empty
          Candidates = lazy ValueNone }

    /// <summary>Sanitizes a lookup, or accepts a null offset, with format 10 refused,
    /// <c>Lookup::sanitize (c, base)</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="offset">The lookup offset from the subtable.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeLookupOrNull (table: ReadOnlySpan<byte>) (at: int) (offset: uint32) (valueSize: int) (numGlyphs: int) (limit: int) (ops: int) =
        match offset with
        | 0u -> ValueSome ops
        | _ -> AatLookup.sanitize (table.Slice(0, limit)) (at + int offset) valueSize numGlyphs false ops

    /// <summary>Sanitizes a format 0 subtable, <c>KerxSubTableFormat0::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat0 (table: ReadOnlySpan<byte>) (at: int) (limit: int) (ops: int) =
        match Sanitize.fits limit (int64 (at + 12)) 16 with
        | false -> ValueNone
        | true -> Sanitize.array limit (int64 (at + 28)) (Binary.u32 table (at + 12)) (uint32 PairSize) ops

    /// <summary>Sanitizes a format 1 or format 4 subtable, <c>KerxSubTableFormat1::sanitize</c> and
    /// <c>KerxSubTableFormat4::sanitize</c>: the 32-byte struct, then the state machine. The kern-action array and the
    /// ankr action data are checked at run time.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat1 (table: ReadOnlySpan<byte>) (at: int) (numGlyphs: int) (limit: int) (ops: int) =
        match Sanitize.fits limit (int64 at) 32 with
        | false -> ValueNone
        | true ->
            match StateTable.sanitize (table.Slice(0, limit)) (at + 12) true 1 numGlyphs ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (remaining, _)) -> ValueSome remaining

    /// <summary>Sanitizes the right class lookup and the array offset of a format 2 subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="rightOffset">The right class-lookup offset.</param>
    /// <param name="arrayOffset">The array offset.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat2Right (table: ReadOnlySpan<byte>) (at: int) (rightOffset: uint32) (arrayOffset: uint32) (numGlyphs: int) (limit: int) (ops: int) =
        match sanitizeLookupOrNull table at rightOffset FwordSize numGlyphs limit ops with
        | ValueNone -> ValueNone
        | ValueSome ops1 ->
            match arrayOffset = 0u || int64 at + int64 arrayOffset <= int64 limit with
            | false -> ValueNone
            | true -> ValueSome ops1

    /// <summary>Sanitizes a format 2 subtable, <c>KerxSubTableFormat2::sanitize</c>. The class tables are extended
    /// lookups of 16-bit values.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat2 (table: ReadOnlySpan<byte>) (at: int) (numGlyphs: int) (limit: int) (ops: int) =
        match Sanitize.fits limit (int64 at) 28 with
        | false -> ValueNone
        | true ->
            let leftOffset = Binary.u32 table (at + 16)
            let rightOffset = Binary.u32 table (at + 20)
            let arrayOffset = Binary.u32 table (at + 24)
            match sanitizeLookupOrNull table at leftOffset FwordSize numGlyphs limit ops with
            | ValueNone -> ValueNone
            | ValueSome ops1 -> sanitizeFormat2Right table at rightOffset arrayOffset numGlyphs limit ops1

    /// <summary>Checks the array and vector ranges of a format 6 subtable.</summary>
    /// <param name="at">The subtable offset.</param>
    /// <param name="arrayOffset">The array offset.</param>
    /// <param name="vectorOffset">The vector offset.</param>
    /// <param name="tupleCount">The tuple count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat6Arrays (at: int) (arrayOffset: uint32) (vectorOffset: uint32) (tupleCount: uint32) (limit: int) (ops: int) =
        match arrayOffset = 0u || int64 at + int64 arrayOffset <= int64 limit with
        | false -> ValueNone
        | true ->
            match tupleCount = 0u || int64 at + int64 vectorOffset <= int64 limit with
            | false -> ValueNone
            | true -> ValueSome ops

    /// <summary>Sanitizes the column lookup of a format 6 subtable, then its array and vector ranges.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="columnOffset">The column lookup offset.</param>
    /// <param name="valueSize">The value width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="arrayOffset">The array offset.</param>
    /// <param name="vectorOffset">The vector offset.</param>
    /// <param name="tupleCount">The tuple count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat6Column (table: ReadOnlySpan<byte>) (at: int) (columnOffset: uint32) (valueSize: int) (numGlyphs: int) (limit: int) (arrayOffset: uint32) (vectorOffset: uint32) (tupleCount: uint32) (ops: int) =
        match sanitizeLookupOrNull table at columnOffset valueSize numGlyphs limit ops with
        | ValueNone -> ValueNone
        | ValueSome ops1 -> sanitizeFormat6Arrays at arrayOffset vectorOffset tupleCount limit ops1

    /// <summary>Sanitizes a format 6 subtable, <c>KerxSubTableFormat6::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat6 (table: ReadOnlySpan<byte>) (at: int) (numGlyphs: int) (limit: int) (ops: int) =
        match Sanitize.fits limit (int64 at) 36 with
        | false -> ValueNone
        | true ->
            let valueSize = if Binary.u32 table (at + 12) &&& ValuesAreLong <> 0u then Fword32Size else FwordSize
            let rowOffset = Binary.u32 table (at + 20)
            let columnOffset = Binary.u32 table (at + 24)
            let arrayOffset = Binary.u32 table (at + 28)
            let vectorOffset = Binary.u32 table (at + 32)
            let tupleCount = Binary.u32 table (at + 8)
            match sanitizeLookupOrNull table at rowOffset valueSize numGlyphs limit ops with
            | ValueNone -> ValueNone
            | ValueSome ops1 -> sanitizeFormat6Column table at columnOffset valueSize numGlyphs limit arrayOffset vectorOffset tupleCount ops1

    /// <summary>Sanitizes one subtable's format-specific body, <c>KerxSubTable::dispatch</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeSubtableDispatch (table: ReadOnlySpan<byte>) (at: int) (format: int) (numGlyphs: int) (limit: int) (ops: int) =
        match format with
        | 0 -> sanitizeFormat0 table at limit ops
        | 1 | 4 -> sanitizeFormat1 table at numGlyphs limit ops
        | 2 -> sanitizeFormat2 table at numGlyphs limit ops
        | 6 -> sanitizeFormat6 table at numGlyphs limit ops
        | _ -> ValueSome ops

    /// <summary>Sanitizes one subtable, <c>KerxSubTable::sanitize</c>: the header, the length and then the
    /// format-specific body.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeSubtable (table: ReadOnlySpan<byte>) (at: int) (numGlyphs: int) (limit: int) (ops: int) =
        match Sanitize.fits limit (int64 at) SubtableHeaderSize with
        | false -> ValueNone
        | true ->
            let coverage = Binary.u32 table (at + 4)
            sanitizeSubtableDispatch table at (int (coverage &&& 0xFFu)) numGlyphs limit ops

    /// <summary>Sanitizes the subtables, walking each by its length and bounding each but the last to its own range.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="at">The next subtable offset.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations and the offset after the last subtable, or none when a subtable fails.</returns>
    [<TailCall>]
    let rec private sanitizeSubtables (table: ReadOnlySpan<byte>) (numGlyphs: int) (at: int) (index: int) (count: int) (ops: int) =
        match index >= count with
        | true -> ValueSome (struct (ops, at))
        | false ->
            let length = Binary.u32 table at
            match length >= uint32 SubtableHeaderSize && int64 at + int64 length <= int64 table.Length with
            | false -> ValueNone
            | true ->
                let limit = if index = count - 1 then table.Length else at + int length
                match sanitizeSubtable table at numGlyphs limit ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeSubtables table numGlyphs (at + int length) (index + 1) count remaining

    /// <summary>Checks every non-zero, non-0xFFFFFFFF coverage offset against its bitfield.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The coverage-table offset.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="bytes">The bitfield byte count.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeCoverageOffsets (table: ReadOnlySpan<byte>) (at: int) (count: int) (bytes: int) (index: int) (ops: int) =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let offset = Binary.u32 table (at + index * 4)
            match offset = 0u || offset = 0xFFFFFFFFu with
            | true -> sanitizeCoverageOffsets table at count bytes (index + 1) ops
            | false ->
                match Sanitize.range table.Length (int64 at + int64 offset) (uint32 bytes) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeCoverageOffsets table at count bytes (index + 1) remaining

    /// <summary>Sanitizes the version-3 subtable coverage table, <c>SubtableGlyphCoverage::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The coverage-table offset.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeCoverage (table: ReadOnlySpan<byte>) (at: int) (count: int) (numGlyphs: int) (ops: int) =
        match Sanitize.array table.Length (int64 at) (uint32 count) 4u ops with
        | ValueNone -> ValueNone
        | ValueSome remaining -> sanitizeCoverageOffsets table at count ((numGlyphs + 7) / 8) 0 remaining

    /// <summary>Sanitizes the subtables, then the version-3 coverage table.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="version">The table version.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeAfterSubtables (table: ReadOnlySpan<byte>) (numGlyphs: int) (version: uint32) (count: int) (ops: int) =
        match sanitizeSubtables table numGlyphs HeaderSize 0 count ops with
        | ValueNone -> ValueNone
        | ValueSome (struct (remaining, coverageAt)) ->
            match version >= CoverageVersion with
            | false -> ValueSome remaining
            | true -> sanitizeCoverage table coverageAt count numGlyphs remaining

    /// <summary>Checks the subtable count and then sanitizes the subtables and the version-3 coverage.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="version">The table version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeCount (table: ReadOnlySpan<byte>) (numGlyphs: int) (version: uint32) (ops: int) =
        let count = Binary.u32 table 4
        match count > uint32 MaxSubtables with
        | true -> ValueNone
        | false -> sanitizeAfterSubtables table numGlyphs version (int count) ops

    /// <summary>Checks the version and then the subtable count of the kerx header.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeVersion (table: ReadOnlySpan<byte>) (numGlyphs: int) (ops: int) =
        let version = Binary.u16 table 0
        match uint32 version < MinVersion with
        | true -> ValueNone
        | false -> sanitizeCount table numGlyphs (uint32 version) ops

    /// <summary>Sanitizes the kerx table, <c>kerx::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (numGlyphs: int) (ops: int) =
        match Sanitize.fits table.Length 0L HeaderSize with
        | false -> ValueNone
        | true -> sanitizeVersion table numGlyphs ops

    /// <summary>The struct form of the kerx sanitizer.</summary>
    [<Struct>]
    type private Sanitizer =
        { NumGlyphs: int }
        interface ITableSanitizer with
            member this.Sanitize (table, ops) = sanitize table this.NumGlyphs ops

    /// <summary>A null lookup, the value a null offset resolves to.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="valueSize">The value width.</param>
    /// <returns>The lookup.</returns>
    let private nullLookup (table: ReadOnlyMemory<byte>) (valueSize: int) =
        { Lookup.Table = table; At = 0; Format = LookupFormat.NullLookup; ValueSize = valueSize }

    /// <summary>A lookup, or the null lookup for a null offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="offset">The lookup offset from the subtable.</param>
    /// <param name="valueSize">The value width.</param>
    /// <returns>The lookup.</returns>
    let private lookupAt (table: ReadOnlyMemory<byte>) (at: int) (offset: uint32) (valueSize: int) =
        match offset with
        | 0u -> nullLookup table valueSize
        | _ -> AatLookup.read table (at + int offset) valueSize

    /// <summary>The unfolder of one format 0 pair, producing the left or right glyph.</summary>
    [<Struct>]
    type private PairGlyphUnfolder =
        { PairsAt: int
          Left: bool }
        interface ISpanUnfolder<byte, int, SetOp> with
            member this.Next (source, index) =
                let at = this.PairsAt + index * PairSize
                let glyph = uint32 (if this.Left then Binary.u16 source at else Binary.u16 source (at + 2))
                struct (SetOp.Add (glyph, glyph), index + 1)

    /// <summary>Collects the left or right glyphs of a format 0 subtable, <c>collect_glyphs</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="left">True for the left glyphs, false for the right.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let private collectPairs (table: ReadOnlySpan<byte>) (at: int) (left: bool) =
        let count = int (Binary.u32 table (at + 12))
        let unfolder: PairGlyphUnfolder = { PairsAt = at + 28; Left = left }
        Sealed.rangeSet ((Sealed.unfoldSpan table count 0 unfolder).AsSpan())

    /// <summary>Compiles the pairs of a format 0 subtable into a hashed index that answers as the binary search over them does; none for
    /// another format, a table of tuple kerning or pairs that run past the subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="rangeEnd">The exclusive bound of run-time reads.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="tupleCount">The subtable's tuple count.</param>
    /// <returns>The index, or none.</returns>
    let private compilePairs (table: ReadOnlyMemory<byte>) (at: int) (rangeEnd: int) (format: int) (tupleCount: uint32) =
        let span = table.Span
        let count = match format with | 0 -> int64 (Binary.u32 span (at + 12)) | _ -> -1L
        match count >= 0L && tupleCount = 0u && int64 at + 28L + count * 6L <= int64 (min rangeEnd table.Length) with
        | false -> ValueNone
        | true ->
            let keys = Sealed.init (int count) (fun index -> (uint32 (Binary.u16 table.Span (at + 28 + index * 6)) <<< 16) ||| uint32 (Binary.u16 table.Span (at + 30 + index * 6)))
            let values = Sealed.init (int count) (fun index -> Binary.i16 table.Span (at + 32 + index * 6))
            PairLookup.compile (keys.AsSpan()) (values.AsSpan())

    /// <summary>The most glyphs the bit table of the subtables covers.</summary>
    [<Literal>]
    let private MaxCandidateGlyphs = 262144

    /// <summary>One past the highest glyph any first glyph set of the subtables holds.</summary>
    /// <param name="subtables">The subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="top">The highest so far.</param>
    /// <returns>The top.</returns>
    [<TailCall>]
    let rec private topOfFirstSets (subtables: ImmutableArray<KerxSubtable>) (index: int) (top: int64) =
        match index >= subtables.Length with
        | true -> top
        | false ->
            let ranges = subtables[index].FirstSet
            let next = match ranges.Length with | 0 -> top | count -> max top (int64 ranges[count - 1].Last + 1L)
            topOfFirstSets subtables (index + 1) next

    /// <summary>Compiles the glyph bit table of the subtables: for each glyph, the subtables whose first glyph set holds it, so that
    /// one pass over a line says which subtables can start on it.</summary>
    /// <param name="subtables">The subtables.</param>
    /// <returns>The table, or none when no set holds a glyph or one reaches past the cap.</returns>
    let private compileCandidates (subtables: ImmutableArray<KerxSubtable>) =
        let top = topOfFirstSets subtables 0 0L
        match subtables.Length = 0 || top = 0L || top > int64 MaxCandidateGlyphs with
        | true -> ValueNone
        | false ->
            let words = (subtables.Length + 63) / 64
            let sets = Sealed.init subtables.Length (fun index -> subtables[index].FirstSet)
            ValueSome { Words = words; Top = int top; Bits = Sealed.subtableBits sets words (int top) }

    /// <summary>The entry classifier of a kerx or Apple kern format 1 subtable, for its initial glyph set
    /// (src/hb-aat-layout-kerx-table.hh:237-244 and 169-201).</summary>
    [<Struct>]
    type Format1EntryClassifier =
        { Extended: bool }
        interface IEntryClassifier with
            member this.IsActionable entry =
                match this.Extended with
                | true -> entry.Data0 <> 0xFFFFus
                | false -> entry.Flags &&& 0x3FFFus <> 0us
            member _.IsActionInitiable entry = entry.Flags &&& 0x8000us <> 0us

    /// <summary>Decodes one subtable, <c>KerxSubTable</c>. Formats 0, 1, 2, 4 and 6 are read and their glyph sets
    /// collected.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="rangeEnd">The exclusive bound of run-time reads.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The subtable.</returns>
    let private readSubtable (table: ReadOnlyMemory<byte>) (at: int) (rangeEnd: int) (numGlyphs: int) =
        let span = table.Span
        let coverage = Binary.u32 span (at + 4)
        let tupleCount = Binary.u32 span (at + 8)
        let format = int (coverage &&& 0xFFu)
        let firstSet, secondSet, machine, kernActionAt =
            match format with
            | 0 -> collectPairs span at true, collectPairs span at false, ValueNone, -1
            | 1 ->
                let machine = StateTable.read table (at + 12) true 1
                let classifier = { Extended = true }
                StateTable.initialGlyphs machine numGlyphs classifier, ImmutableArray.Empty, ValueSome machine, int (min (int64 at + 12L + int64 (Binary.u32 span (at + 28))) (int64 Int32.MaxValue))
            | 4 ->
                let machine = StateTable.read table (at + 12) true 1
                let classifier = { Extended = true }
                StateTable.initialGlyphs machine numGlyphs classifier, ImmutableArray.Empty, ValueSome machine, int (min (int64 at + 12L + int64 (Binary.u32 span (at + 28) &&& ActionDataMask)) (int64 Int32.MaxValue))
            | 2 ->
                let left = lookupAt table at (Binary.u32 span (at + 16)) FwordSize
                let right = lookupAt table at (Binary.u32 span (at + 20)) FwordSize
                AatLookup.collectGlyphs left numGlyphs, AatLookup.collectGlyphs right numGlyphs, ValueNone, -1
            | 6 ->
                let valueSize = if Binary.u32 span (at + 12) &&& ValuesAreLong <> 0u then Fword32Size else FwordSize
                let row = lookupAt table at (Binary.u32 span (at + 20)) valueSize
                let column = lookupAt table at (Binary.u32 span (at + 24)) valueSize
                AatLookup.collectGlyphs row numGlyphs, AatLookup.collectGlyphs column numGlyphs, ValueNone, -1
            | _ -> ImmutableArray.Empty, ImmutableArray.Empty, ValueNone, -1
        { Format = format
          Coverage = coverage
          TupleCount = tupleCount
          Body = table
          At = at
          RangeEnd = rangeEnd
          KernActionAt = kernActionAt
          FirstSet = firstSet
          SecondSet = secondSet
          Machine = machine
          Dense =
            lazy (
                match machine with
                | ValueSome compiled -> DenseMachine.build compiled numGlyphs { Format1EntryClassifier.Extended = true }
                | ValueNone -> ValueNone)
          Pairs = lazy (compilePairs table at rangeEnd format tupleCount) }

    /// <summary>Decodes the subtables, walking each by its length.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="at">The next subtable offset.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="builder">The subtables built so far.</param>
    /// <returns>The subtables.</returns>
    [<TailCall>]
    let rec private readSubtables (table: ReadOnlyMemory<byte>) (numGlyphs: int) (at: int) (index: int) (count: int) (builder: PointBuilder<KerxSubtable>) =
        match index >= count with
        | true -> PointBuilder.publish builder
        | false ->
            let length = int (Binary.u32 table.Span at)
            let rangeEnd = if index = count - 1 then table.Length else at + length
            let subtable = readSubtable table at rangeEnd numGlyphs
            readSubtables table numGlyphs (at + length) (index + 1) count (PointBuilder.append builder subtable)

    /// <summary>Loads and sanitizes a face's kerx table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The kerx table, empty when absent or invalid.</returns>
    let load (face: Face) =
        let sanitizer: Sanitizer = { NumGlyphs = face.GlyphCount }
        let table = Sanitize.table sanitizer (Face.referenceTable face Tags.Kerx)
        match table.Length with
        | 0 -> empty
        | _ ->
            let count = int (Binary.u32 table.Span 4)
            let subtables = readSubtables table face.GlyphCount HeaderSize 0 count (PointBuilder.create())
            { Version = Binary.u16 table.Span 0
              Subtables = subtables
              Candidates = lazy (compileCandidates subtables) }

    /// <summary>Whether the table has data, <c>kerx::has_data</c>.</summary>
    /// <param name="kerx">The table.</param>
    /// <returns>True when the version is not zero.</returns>
    let hasData (kerx: Kerx) = kerx.Version <> 0us
