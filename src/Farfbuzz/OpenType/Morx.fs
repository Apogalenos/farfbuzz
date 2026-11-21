namespace Farfbuzz.Aat

open System.Collections.Immutable

/// <summary>One compiled range of a chain: the flags and the cluster span they cover.</summary>
[<Struct; NoComparison; NoEquality>]
type internal RangeFlags =
    { Flags: uint32
      ClusterFirst: uint32
      ClusterLast: uint32 }

/// <summary>The compiled AAT map: one flags range list per morx or mort chain.</summary>
[<NoEquality; NoComparison>]
type internal AatMap =
    { ChainFlags: ImmutableArray<ImmutableArray<RangeFlags>> }

namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.Aat

/// <summary>One feature record of a morx or mort chain.</summary>
[<Struct; NoComparison; NoEquality>]
type internal MorxFeature =
    { FeatureType: uint16
      FeatureSetting: uint16
      EnableFlags: uint32
      DisableFlags: uint32 }

/// <summary>The kind of one morx or mort chain subtable, <c>ChainSubtable::Type</c>.</summary>
[<Struct>]
type internal SubtableKind =
    | Rearrangement
    | Contextual
    | Ligature
    | Noncontextual
    | Insertion
    | UnknownKind of kind: int

/// <summary>One morx or mort chain subtable: its kind, coverage byte, feature flags, body and initial glyph set.</summary>
[<NoComparison; NoEquality>]
type internal ChainSubtable =
    { Kind: SubtableKind
      Coverage: byte
      SubFeatureFlags: uint32
      Body: ReadOnlyMemory<byte>
      Machine: StateMachine voption
      SubstitutionTablesAt: int
      InitialGlyphs: ImmutableArray<CodepointRange>
      Dense: Lazy<DenseMachine voption>
      Replacements: Lazy<ImmutableArray<uint32> voption> } // a noncontextual subtable's replacement of every glyph, a morx contextual subtable's of every glyph under every lookup its entries name, or none

/// <summary>The glyphs of a chain against its subtables: for each glyph below <c>Top</c>, <c>Words</c> words with the bit of
/// every subtable whose initial glyph set holds the glyph, so that one pass over a line says which subtables can start on it.</summary>
[<Struct; NoComparison; NoEquality>]
type internal ChainCandidates =
    { Words: int
      Top: int
      Bits: ImmutableArray<uint64> }

/// <summary>One morx or mort chain: its default flags, feature records and subtables.</summary>
[<Struct; NoComparison; NoEquality>]
type internal MorxChain =
    { DefaultFlags: uint32
      Features: ImmutableArray<MorxFeature>
      Subtables: ImmutableArray<ChainSubtable>
      Candidates: Lazy<ChainCandidates voption> } // none when the chain has no subtable or a set reaches past the cap

/// <summary>The morx or mort table.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Morx =
    { IsMort: bool
      Version: uint32
      Chains: ImmutableArray<MorxChain>
      DefaultMap: Lazy<AatMap voption> } // the map without user features when no chain reads the language, built at its first use

/// <summary>Reading the morx and mort tables, <c>AAT::morx</c>/<c>mort</c>.</summary>
[<RequireQualifiedAccess>]
module internal Morx =
    /// <summary>The size of the mortmorx header.</summary>
    [<Literal>]
    let private HeaderSize = 8

    /// <summary>The size of one Feature record.</summary>
    [<Literal>]
    let private FeatureSize = 12

    /// <summary>The largest total subtable count a table may declare, <c>HB_AAT_MAX_SUBTABLES</c>.</summary>
    [<Literal>]
    let private MaxSubtables = 1024

    /// <summary>The replacement of a glyph the noncontextual lookup has no value for.</summary>
    [<Literal>]
    let NoReplacement = 0xFFFFFFFFu

    /// <summary>The noncontextual subtable type, <c>ChainSubtable::Type::Noncontextual</c>.</summary>
    [<Literal>]
    let private NoncontextualType = 4

    /// <summary>The rearrangement subtable type, <c>ChainSubtable::Type::Rearrangement</c>.</summary>
    [<Literal>]
    let private RearrangementType = 0

    /// <summary>The contextual subtable type, <c>ChainSubtable::Type::Contextual</c>.</summary>
    [<Literal>]
    let private ContextualType = 1

    /// <summary>The ligature subtable type, <c>ChainSubtable::Type::Ligature</c>.</summary>
    [<Literal>]
    let private LigatureType = 2

    /// <summary>The insertion subtable type, <c>ChainSubtable::Type::Insertion</c>.</summary>
    [<Literal>]
    let private InsertionType = 5

    /// <summary>The feature type <c>LETTER_CASE</c>, <c>HB_AAT_LAYOUT_FEATURE_TYPE_LETTER_CASE</c>.</summary>
    [<Literal>]
    let private FeatureTypeLetterCase = 3us

    /// <summary>The feature type <c>LOWER_CASE</c>, <c>HB_AAT_LAYOUT_FEATURE_TYPE_LOWER_CASE</c>.</summary>
    [<Literal>]
    let private FeatureTypeLowerCase = 37us

    /// <summary>The feature type <c>LANGUAGE_TAG_TYPE</c>, <c>HB_AAT_LAYOUT_FEATURE_TYPE_LANGUAGE_TAG_TYPE</c>.</summary>
    [<Literal>]
    let private FeatureTypeLanguageTag = 39us

    /// <summary>The deprecated <c>SMALL_CAPS</c> selector, <c>HB_AAT_LAYOUT_FEATURE_SELECTOR_SMALL_CAPS</c>.</summary>
    [<Literal>]
    let private SelectorSmallCaps = 3us

    /// <summary>The <c>LOWER_CASE_SMALL_CAPS</c> selector.</summary>
    [<Literal>]
    let private SelectorLowerCaseSmallCaps = 1us

    /// <summary>The empty table, returned when the table is absent, fails to sanitize or is blocklisted.</summary>
    /// <param name="isMort">True for the mort table.</param>
    /// <returns>The empty table.</returns>
    let private empty (isMort: bool) =
        { IsMort = isMort; Version = 0u; Chains = ImmutableArray.Empty; DefaultMap = lazy ValueNone }

    /// <summary>Reads a chain's feature count, narrow or wide.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The chain offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <returns>The feature count.</returns>
    let private featureCountAt (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) =
        match hbuintSize with
        | 4 -> Binary.u32 table (at + 8)
        | _ -> uint32 (Binary.u16 table (at + 8))

    /// <summary>Reads a chain's subtable count, narrow or wide.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The chain offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <returns>The subtable count.</returns>
    let private subtableCountAt (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) =
        match hbuintSize with
        | 4 -> Binary.u32 table (at + 12)
        | _ -> uint32 (Binary.u16 table (at + 10))

    /// <summary>Reads a subtable's length field, narrow or wide.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <returns>The subtable length.</returns>
    let private subtableLengthAt (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) =
        match hbuintSize with
        | 4 -> Binary.u32 table at
        | _ -> uint32 (Binary.u16 table at)

    /// <summary>Reads a subtable's coverage field, narrow or wide.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <returns>The coverage field.</returns>
    let private coverageFieldAt (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) =
        match hbuintSize with
        | 4 -> Binary.u32 table (at + 4)
        | _ -> uint32 (Binary.u16 table (at + 2))

    /// <summary>The coverage byte of a subtable, <c>ChainSubtable::get_coverage</c> (the top byte of the coverage field).</summary>
    /// <param name="coverage">The coverage field.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <returns>The coverage byte.</returns>
    let private coverageByteOf (coverage: uint32) (hbuintSize: int) =
        match hbuintSize with
        | 4 -> byte (coverage >>> 24)
        | _ -> byte (coverage >>> 8)

    /// <summary>Counts the lookups a morx contextual subtable references, <c>ContextualSubtable::sanitize</c>'s loop.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="entriesAt">The byte offset of the first entry.</param>
    /// <param name="index">The next entry.</param>
    /// <param name="count">The entry count.</param>
    /// <param name="numLookups">The lookups counted so far.</param>
    /// <returns>The lookup count.</returns>
    [<TailCall>]
    let rec private contextualNumLookups (table: ReadOnlySpan<byte>) entriesAt index count numLookups =
        match index >= count with
        | true -> numLookups
        | false ->
            let at = entriesAt + index * 8
            let mark = Binary.u16 table (at + 4)
            let current = Binary.u16 table (at + 6)
            let afterMark = if mark <> 0xFFFFus then max numLookups (1 + int mark) else numLookups
            let afterCurrent = if current <> 0xFFFFus then max afterMark (1 + int current) else afterMark
            contextualNumLookups table entriesAt (index + 1) count afterCurrent

    /// <summary>Sanitizes the substitution lookups of a morx contextual subtable, <c>UnsizedListOfOffset16To::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="subsAt">The byte offset of the lookup list.</param>
    /// <param name="index">The next lookup.</param>
    /// <param name="count">The lookup count.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeLookupList (table: ReadOnlySpan<byte>) subsAt index count numGlyphs ops =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let lookupAt = subsAt + int (Binary.u32 table (subsAt + index * 4))
            match AatLookup.sanitize table lookupAt 2 numGlyphs true ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeLookupList table subsAt (index + 1) count numGlyphs remaining

    /// <summary>Sanitizes the substitution tables of a contextual subtable once its state table has passed,
    /// <c>ContextualSubtable::sanitize</c> (src/hb-aat-layout-morx-table.hh:355-372).</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="stateTableAt">The state-table offset.</param>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="numEntries">The entry count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeContextualSubstitutions (table: ReadOnlySpan<byte>) stateTableAt extended numGlyphs numEntries ops =
        match extended with
        | false ->
            let offset = int64 (Binary.u16 table (stateTableAt + 8))
            Sanitize.range table.Length (int64 stateTableAt + offset) 0u ops
        | true ->
            let offset = int64 (Binary.u32 table (stateTableAt + 16))
            let entriesAt = stateTableAt + int (Binary.u32 table (stateTableAt + 12))
            let numLookups = contextualNumLookups table entriesAt 0 numEntries 0
            match Sanitize.array table.Length (int64 stateTableAt + offset) (uint32 numLookups) 4u ops with
            | ValueNone -> ValueNone
            | ValueSome afterList -> sanitizeLookupList table (int (int64 stateTableAt + offset)) 0 numLookups numGlyphs afterList

    /// <summary>Sanitizes the substitution tables of a contextual subtable, <c>ContextualSubtable::sanitize</c>
    /// (src/hb-aat-layout-morx-table.hh:347-372).</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="stateTableAt">The state-table offset.</param>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeContextual (table: ReadOnlySpan<byte>) stateTableAt extended numGlyphs ops =
        match StateTable.sanitize table stateTableAt extended 2 numGlyphs ops with
        | ValueNone -> ValueNone
        | ValueSome (struct (remaining, numEntries)) ->
            let hbuintSize = if extended then 4 else 2
            match Sanitize.fits table.Length (int64 (stateTableAt + 4 * hbuintSize)) hbuintSize with
            | false -> ValueNone
            | true -> sanitizeContextualSubstitutions table stateTableAt extended numGlyphs numEntries remaining

    /// <summary>Sanitizes a ligature subtable: the state table, the struct size and the three non-null offsets,
    /// <c>LigatureSubtable::sanitize</c> (src/hb-aat-layout-morx-table.hh:617-624). The arrays themselves are checked at
    /// run time.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="stateTableAt">The state-table offset.</param>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeLigature (table: ReadOnlySpan<byte>) (stateTableAt: int) (extended: bool) (numGlyphs: int) (ops: int) =
        let hbuintSize = if extended then 4 else 2
        let entryDataWords = if extended then 1 else 0
        match Sanitize.fits table.Length (int64 stateTableAt) (7 * hbuintSize) with
        | false -> ValueNone
        | true ->
            match StateTable.sanitize table stateTableAt extended entryDataWords numGlyphs ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (remaining, _)) ->
                let offsetsAt = stateTableAt + 4 * hbuintSize
                let ligAction = if extended then Binary.u32 table offsetsAt else uint32 (Binary.u16 table offsetsAt)
                let componentAt = if extended then Binary.u32 table (offsetsAt + hbuintSize) else uint32 (Binary.u16 table (offsetsAt + hbuintSize))
                let ligature = if extended then Binary.u32 table (offsetsAt + 2 * hbuintSize) else uint32 (Binary.u16 table (offsetsAt + 2 * hbuintSize))
                match ligAction <> 0u && componentAt <> 0u && ligature <> 0u with
                | true -> ValueSome remaining
                | false -> ValueNone

    /// <summary>Sanitizes an insertion subtable: the struct size, the state table and the non-null insertion-action
    /// offset, <c>InsertionSubtable::sanitize</c> (src/hb-aat-layout-morx-table.hh:894-900). The glyph list itself is
    /// checked at run time.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="stateTableAt">The state-table offset.</param>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeInsertion (table: ReadOnlySpan<byte>) (stateTableAt: int) (extended: bool) (numGlyphs: int) (ops: int) =
        let hbuintSize = if extended then 4 else 2
        match Sanitize.fits table.Length (int64 stateTableAt) (5 * hbuintSize) with
        | false -> ValueNone
        | true ->
            match StateTable.sanitize table stateTableAt extended 2 numGlyphs ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (remaining, _)) ->
                let offsetsAt = stateTableAt + 4 * hbuintSize
                let insertionAction = if extended then Binary.u32 table offsetsAt else uint32 (Binary.u16 table offsetsAt)
                match insertionAction <> 0u with
                | true -> ValueSome remaining
                | false -> ValueNone

    /// <summary>Dispatches a sanitized subtable by its type, <c>ChainSubtable::dispatch</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeSubtableDispatch (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) (ops: int) =
        let stateTableAt = at + 2 * hbuintSize + 4
        let extended = hbuintSize = 4
        let coverage = coverageFieldAt table at hbuintSize
        match int (coverage &&& 0xFFu) with
        | RearrangementType ->
            match StateTable.sanitize table stateTableAt extended 0 numGlyphs ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (remaining, _)) -> ValueSome remaining
        | ContextualType -> sanitizeContextual table stateTableAt extended numGlyphs ops
        | LigatureType -> sanitizeLigature table stateTableAt extended numGlyphs ops
        | NoncontextualType -> AatLookup.sanitize table stateTableAt 2 numGlyphs true ops
        | InsertionType -> sanitizeInsertion table stateTableAt extended numGlyphs ops
        | _ -> ValueSome ops

    /// <summary>Sanitizes one subtable, <c>ChainSubtable::sanitize</c>: the length and range check, then the
    /// noncontextual lookup; the other kinds are later slices and only get the shared length check here.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeSubtable (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) (ops: int) =
        let minSize = 2 * hbuintSize + 4
        match Sanitize.fits table.Length (int64 at) minSize with
        | false -> ValueNone
        | true ->
            let length = subtableLengthAt table at hbuintSize
            match length >= uint32 minSize with
            | false -> ValueNone
            | true ->
                match Sanitize.range table.Length (int64 at) length ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeSubtableDispatch table at hbuintSize numGlyphs remaining

    /// <summary>Sanitizes the subtables of one chain, walking each by its length.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The next subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="remaining">The subtables left.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeSubtables (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) (remaining: int) (ops: int) =
        match remaining <= 0 with
        | true -> ValueSome ops
        | false ->
            match sanitizeSubtable table at hbuintSize numGlyphs ops with
            | ValueNone -> ValueNone
            | ValueSome remainingOps ->
                let length = int (subtableLengthAt table at hbuintSize)
                sanitizeSubtables table (at + length) hbuintSize numGlyphs (remaining - 1) remainingOps

    /// <summary>Checks every non-zero, non-0xFFFFFFFF coverage offset against its bitfield.</summary>
    /// <param name="table">The table bytes.</param>
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
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The coverage-table offset.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeCoverage (table: ReadOnlySpan<byte>) (at: int) (count: int) (numGlyphs: int) (ops: int) =
        match Sanitize.array table.Length (int64 at) (uint32 count) 4u ops with
        | ValueNone -> ValueNone
        | ValueSome remaining -> sanitizeCoverageOffsets table at count ((numGlyphs + 7) / 8) 0 remaining

    /// <summary>Sanitizes the version-3 coverage table when the version requires one.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="subAt">The coverage-table offset.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="version">The mortmorx version.</param>
    /// <param name="subtableCount">The subtable count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations and the subtable count, or none.</returns>
    let private sanitizeChainCoverage (table: ReadOnlySpan<byte>) (subAt: int) (numGlyphs: int) (version: uint32) (subtableCount: int) (ops: int) =
        match version >= 3u with
        | false -> ValueSome (struct (ops, subtableCount))
        | true ->
            match sanitizeCoverage table subAt subtableCount numGlyphs ops with
            | ValueNone -> ValueNone
            | ValueSome afterCoverage -> ValueSome (struct (afterCoverage, subtableCount))

    /// <summary>Sanitizes the subtables of one chain, then its version-3 coverage table.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="subAt">The first subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="version">The mortmorx version.</param>
    /// <param name="subtableCount">The subtable count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations and the subtable count, or none.</returns>
    let private sanitizeChainSubtables (table: ReadOnlySpan<byte>) (subAt: int) (hbuintSize: int) (numGlyphs: int) (version: uint32) (subtableCount: int) (ops: int) =
        match sanitizeSubtables table subAt hbuintSize numGlyphs subtableCount ops with
        | ValueNone -> ValueNone
        | ValueSome afterSubtables -> sanitizeChainCoverage table subAt numGlyphs version subtableCount afterSubtables

    /// <summary>Sanitizes the feature records of one chain, then walks its subtables.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The chain offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="version">The mortmorx version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations and the subtable count, or none.</returns>
    let private sanitizeChainFeatures (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) (version: uint32) (ops: int) =
        let minSize = 8 + 2 * hbuintSize
        let featureCount = featureCountAt table at hbuintSize
        let subtableCount = subtableCountAt table at hbuintSize
        match Sanitize.array table.Length (int64 (at + minSize)) featureCount (uint32 FeatureSize) ops with
        | ValueNone -> ValueNone
        | ValueSome afterFeatures ->
            let subAt = at + minSize + int featureCount * FeatureSize
            sanitizeChainSubtables table subAt hbuintSize numGlyphs version (int subtableCount) afterFeatures

    /// <summary>Sanitizes one chain header, its feature records, its subtables and the version-3 coverage.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The chain offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="version">The mortmorx version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations and the subtable count, or none.</returns>
    let private sanitizeChain (table: ReadOnlySpan<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) (version: uint32) (ops: int) =
        let minSize = 8 + 2 * hbuintSize
        match Sanitize.fits table.Length (int64 at + 4L) 4 with
        | false -> ValueNone
        | true ->
            let length = Binary.u32 table (at + 4)
            match length >= uint32 minSize with
            | false -> ValueNone
            | true ->
                match Sanitize.range table.Length (int64 at) length ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeChainFeatures table at hbuintSize numGlyphs version remaining

    /// <summary>Sanitizes every chain, walking each by its length and bounding the total subtable count.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The next chain offset.</param>
    /// <param name="remaining">The chains left.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="version">The mortmorx version.</param>
    /// <param name="subtableTotal">The subtables seen so far.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeChains (table: ReadOnlySpan<byte>) (at: int) (remaining: int) (hbuintSize: int) (numGlyphs: int) (version: uint32) (subtableTotal: int) (ops: int) =
        match remaining <= 0 with
        | true -> ValueSome ops
        | false ->
            match sanitizeChain table at hbuintSize numGlyphs version ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (remainingOps, subtableCount)) ->
                let total = subtableTotal + subtableCount
                match total > MaxSubtables with
                | true -> ValueNone
                | false ->
                    let length = int (Binary.u32 table (at + 4))
                    sanitizeChains table (at + length) (remaining - 1) hbuintSize numGlyphs version total remainingOps

    /// <summary>Sanitizes a mortmorx table, <c>mortmorx::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="isMort">True for the mort table.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (isMort: bool) (numGlyphs: int) (ops: int) =
        let hbuintSize = if isMort then 2 else 4
        match Sanitize.fits table.Length 0 HeaderSize with
        | false -> ValueNone
        | true ->
            let version = uint32 (Binary.u16 table 0)
            match version = 0u with
            | true -> ValueNone
            | false ->
                let chainCount = Binary.u32 table 4
                sanitizeChains table HeaderSize (int chainCount) hbuintSize numGlyphs version 0 ops

    /// <summary>The struct form of the mortmorx sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        { IsMort: bool
          NumGlyphs: int }
        interface ITableSanitizer with
            member this.Sanitize (table, ops) = sanitize table this.IsMort this.NumGlyphs ops

    /// <summary>Whether the morx table of this blob length and face is blocklisted, <c>AAT::morx::is_blocklisted</c>.</summary>
    /// <param name="length">The morx blob length.</param>
    /// <param name="face">The face.</param>
    /// <param name="isMort">True for the mort table, whose blocklist is empty.</param>
    /// <returns>True when the table must be ignored.</returns>
    let private isBlocklisted (length: int) (face: Face) (isMort: bool) =
        match isMort with
        | true -> false
        | false ->
            let gsubLength = (Face.referenceTable face Tags.Gsub).Length
            let gdefLength = (Face.referenceTable face Tags.Gdef).Length
            let triple = (int64 length <<< 42) ||| (int64 gsubLength <<< 21) ||| int64 gdefLength
            let blocklisted = (19892L <<< 42) ||| (2794L <<< 21) ||| 340L
            triple = blocklisted

    /// <summary>The unfolder of one chain's feature records.</summary>
    [<Struct>]
    type private FeatureUnfolder =
        interface ISpanUnfolder<byte, int, MorxFeature> with
            member _.Next (source, state) =
                struct
                    ({ FeatureType = Binary.u16 source state
                       FeatureSetting = Binary.u16 source (state + 2)
                       EnableFlags = Binary.u32 source (state + 4)
                       DisableFlags = Binary.u32 source (state + 8) },
                     state + FeatureSize)

    /// <summary>The entry classifier of a rearrangement or contextual subtable, for its initial glyph set.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private EntryClassifier =
        { Kind: SubtableKind
          Extended: bool }
        interface IEntryClassifier with
            member this.IsActionable entry =
                match this.Kind with
                | Rearrangement -> entry.Flags &&& 0x000Fus <> 0us
                | Contextual ->
                    match this.Extended with
                    | true -> entry.Data0 <> 0xFFFFus || entry.Data1 <> 0xFFFFus
                    | false -> entry.Data0 <> 0us || entry.Data1 <> 0us
                | Ligature ->
                    match this.Extended with
                    | true -> entry.Flags &&& 0x2000us <> 0us
                    | false -> entry.Flags &&& 0x3FFFus <> 0us
                | Insertion ->
                    (entry.Flags &&& 0x03FFus) <> 0us
                    && (entry.Data0 <> 0xFFFFus || entry.Data1 <> 0xFFFFus)
                | Noncontextual | UnknownKind _ -> false
            member this.IsActionInitiable entry =
                match this.Kind with
                | Rearrangement | Contextual | Ligature | Insertion -> entry.Flags &&& 0x8000us <> 0us
                | Noncontextual | UnknownKind _ -> false

    /// <summary>Compiles the replacement of every glyph of a noncontextual subtable into one array, with the no-replacement
    /// marker where the lookup has no value, declining above the glyph cap of the dense tables.</summary>
    /// <param name="body">The subtable body, which starts with the lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The replacements, or none.</returns>
    let compileReplacements (body: ReadOnlyMemory<byte>) (numGlyphs: int) =
        match numGlyphs > 0 && numGlyphs <= DenseMachine.MaxGlyphs with
        | false -> ValueNone
        | true ->
            let lookup = AatLookup.read body 0 2
            ValueSome (Sealed.init numGlyphs (fun glyph ->
                match AatLookup.valueAt lookup (uint32 glyph) numGlyphs with
                | ValueSome offset -> AatLookup.valueOf lookup.Table.Span offset lookup.ValueSize
                | ValueNone -> NoReplacement))

    /// <summary>Resolves a morx contextual substitution through the lookup list, <c>subs[index].get_value</c>
    /// (src/hb-aat-layout-morx-table.hh:259-266).</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="subsAt">The substitution-tables offset within the body.</param>
    /// <param name="index">The lookup index.</param>
    /// <param name="glyph">The glyph code point.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The replacement, or none.</returns>
    let substitutionAt (body: ReadOnlyMemory<byte>) (subsAt: int) (index: uint16) (glyph: uint32) (numGlyphs: int) =
        match index with
        | 0xFFFFus -> ValueNone
        | _ ->
            let lookupAt = subsAt + int (Binary.u32 body.Span (subsAt + int index * 4))
            let lookup = AatLookup.read body lookupAt 2
            match AatLookup.valueAt lookup glyph numGlyphs with
            | ValueNone -> ValueNone
            | ValueSome offset -> ValueSome (AatLookup.valueOf body.Span offset 2)

    /// <summary>The highest lookup index the entries of a contextual machine name, or -1 when they name none.</summary>
    /// <param name="entries">The entries.</param>
    /// <param name="index">The next entry.</param>
    /// <param name="highest">The highest so far.</param>
    /// <returns>The highest index.</returns>
    [<TailCall>]
    let rec private highestSubstitution (entries: ImmutableArray<Entry>) (index: int) (highest: int) =
        match index >= entries.Length with
        | true -> highest
        | false ->
            let mark = match entries[index].Data0 with | 0xFFFFus -> -1 | named -> int named
            let current = match entries[index].Data1 with | 0xFFFFus -> -1 | named -> int named
            highestSubstitution entries (index + 1) (max highest (max mark current))

    /// <summary>The most cells the compiled substitutions of one contextual subtable hold.</summary>
    [<Literal>]
    let private MaxSubstitutionCells = 262144

    /// <summary>Compiles the substitution of every glyph under every lookup a contextual machine names into one array, the
    /// no-replacement marker where there is none: the cell of a lookup index and a glyph is at <c>index * numGlyphs + glyph</c>.</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="subsAt">The substitution-tables offset within the body.</param>
    /// <param name="machine">The state machine, whose entries name the lookups.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The substitutions, or none when the machine names no lookup or they would not fit the cap.</returns>
    let compileSubstitutions (body: ReadOnlyMemory<byte>) (subsAt: int) (machine: StateMachine) (numGlyphs: int) =
        let rows = highestSubstitution machine.Entries 0 -1 + 1
        match numGlyphs > 0 && rows > 0 && int64 rows * int64 numGlyphs <= int64 MaxSubstitutionCells with
        | false -> ValueNone
        | true ->
            ValueSome (Sealed.init (rows * numGlyphs) (fun cell ->
                match substitutionAt body subsAt (uint16 (cell / numGlyphs)) (uint32 (cell % numGlyphs)) numGlyphs with
                | ValueSome replacement -> replacement
                | ValueNone -> NoReplacement))

    /// <summary>Decodes one subtable, <c>ChainSubtable</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The subtable.</returns>
    let private readSubtable (table: ReadOnlyMemory<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) =
        let span = table.Span
        let coverage = coverageFieldAt span at hbuintSize
        let subtableType = int (coverage &&& 0xFFu)
        let body = table.Slice(at + 2 * hbuintSize + 4)
        let extended = hbuintSize = 4
        let kind, machine, substitutionTablesAt, initialGlyphs =
            match subtableType with
            | RearrangementType ->
                let machine = StateTable.read body 0 extended 0
                let classifier: EntryClassifier = { Kind = Rearrangement; Extended = extended }
                Rearrangement, ValueSome machine, 0, StateTable.initialGlyphs machine numGlyphs classifier
            | ContextualType ->
                let machine = StateTable.read body 0 extended 2
                let subsAt = if extended then int (Binary.u32 body.Span 16) else int (Binary.u16 body.Span 8)
                let classifier: EntryClassifier = { Kind = Contextual; Extended = extended }
                Contextual, ValueSome machine, subsAt, StateTable.initialGlyphs machine numGlyphs classifier
            | LigatureType ->
                let machine = StateTable.read body 0 extended (if extended then 1 else 0)
                let classifier: EntryClassifier = { Kind = Ligature; Extended = extended }
                Ligature, ValueSome machine, 0, StateTable.initialGlyphs machine numGlyphs classifier
            | InsertionType ->
                let machine = StateTable.read body 0 extended 2
                let classifier: EntryClassifier = { Kind = Insertion; Extended = extended }
                Insertion, ValueSome machine, 0, StateTable.initialGlyphs machine numGlyphs classifier
            | NoncontextualType ->
                let lookup = AatLookup.read body 0 2
                Noncontextual, ValueNone, 0, AatLookup.collectGlyphs lookup numGlyphs
            | other -> UnknownKind other, ValueNone, 0, ImmutableArray.Empty
        let dense =
            lazy (
                match machine with
                | ValueSome compiled -> DenseMachine.build compiled numGlyphs { Kind = kind; Extended = extended }
                | ValueNone -> ValueNone)
        { Kind = kind
          Coverage = coverageByteOf coverage hbuintSize
          SubFeatureFlags = Binary.u32 span (at + 2 * hbuintSize)
          Body = body
          Machine = machine
          SubstitutionTablesAt = substitutionTablesAt
          InitialGlyphs = initialGlyphs
          Dense = dense
          Replacements =
            lazy (
                match kind with
                | Noncontextual -> compileReplacements body numGlyphs
                | Contextual ->
                    match machine with
                    | ValueSome compiled when extended -> compileSubstitutions body substitutionTablesAt compiled numGlyphs
                    | ValueSome _ -> ValueNone
                    | ValueNone -> ValueNone
                | Rearrangement | Ligature | Insertion | UnknownKind _ -> ValueNone) }

    /// <summary>Decodes the subtables of one chain, walking each by its length.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The next subtable offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="remaining">The subtables left.</param>
    /// <param name="builder">The subtables built so far.</param>
    /// <returns>The subtables.</returns>
    [<TailCall>]
    let rec private readSubtables (table: ReadOnlyMemory<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) (remaining: int) (builder: PointBuilder<ChainSubtable>) =
        match remaining <= 0 with
        | true -> PointBuilder.publish builder
        | false ->
            let subtable = readSubtable table at hbuintSize numGlyphs
            let length = int (subtableLengthAt table.Span at hbuintSize)
            readSubtables table (at + length) hbuintSize numGlyphs (remaining - 1) (PointBuilder.append builder subtable)

    /// <summary>The most glyphs a chain's bit table covers; a chain whose initial glyph sets reach further reads them as ranges.</summary>
    [<Literal>]
    let private MaxCandidateGlyphs = 262144

    /// <summary>One past the highest glyph any initial glyph set of the subtables holds.</summary>
    /// <param name="subtables">The subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="top">The highest so far.</param>
    /// <returns>The top.</returns>
    [<TailCall>]
    let rec private topOfSets (subtables: ImmutableArray<ChainSubtable>) (index: int) (top: int64) =
        match index >= subtables.Length with
        | true -> top
        | false ->
            let ranges = subtables[index].InitialGlyphs
            let next = match ranges.Length with | 0 -> top | count -> max top (int64 ranges[count - 1].Last + 1L)
            topOfSets subtables (index + 1) next

    /// <summary>Compiles the glyph bit table of a chain.</summary>
    /// <param name="subtables">The chain's subtables.</param>
    /// <returns>The table, or none when the chain has no initial glyph or one beyond the cap.</returns>
    let private compileCandidates (subtables: ImmutableArray<ChainSubtable>) =
        let top = topOfSets subtables 0 0L
        match subtables.Length = 0 || top = 0L || top > int64 MaxCandidateGlyphs with
        | true -> ValueNone
        | false ->
            let words = (subtables.Length + 63) / 64
            let sets = Sealed.init subtables.Length (fun index -> subtables[index].InitialGlyphs)
            ValueSome { Words = words; Top = int top; Bits = Sealed.subtableBits sets words (int top) }

    /// <summary>Decodes one chain, <c>Chain</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The chain offset.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The chain.</returns>
    let private readChain (table: ReadOnlyMemory<byte>) (at: int) (hbuintSize: int) (numGlyphs: int) =
        let span = table.Span
        let minSize = 8 + 2 * hbuintSize
        let featureCount = int (featureCountAt span at hbuintSize)
        let subtableCount = int (subtableCountAt span at hbuintSize)
        let defaultFlags = Binary.u32 span at
        let features = Sealed.unfoldSpan span featureCount (at + minSize) (FeatureUnfolder())
        let subAt = at + minSize + featureCount * FeatureSize
        let subtables = readSubtables table subAt hbuintSize numGlyphs subtableCount (PointBuilder.create())
        { DefaultFlags = defaultFlags; Features = features; Subtables = subtables; Candidates = lazy (compileCandidates subtables) }

    /// <summary>Decodes every chain, walking each by its length.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The next chain offset.</param>
    /// <param name="remaining">The chains left.</param>
    /// <param name="hbuintSize">The HBUINT width.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="builder">The chains built so far.</param>
    /// <returns>The chains.</returns>
    [<TailCall>]
    let rec private readChains (table: ReadOnlyMemory<byte>) (at: int) (remaining: int) (hbuintSize: int) (numGlyphs: int) (builder: PointBuilder<MorxChain>) =
        match remaining <= 0 with
        | true -> PointBuilder.publish builder
        | false ->
            let chain = readChain table at hbuintSize numGlyphs
            let length = int (Binary.u32 table.Span (at + 4))
            readChains table (at + length) (remaining - 1) hbuintSize numGlyphs (PointBuilder.append builder chain)

    /// <summary>Whether the table has data, <c>mortmorx::has_data</c>.</summary>
    /// <param name="morx">The table.</param>
    /// <returns>True when the version is not zero.</returns>
    let hasData (morx: Morx) = morx.Version <> 0u

    /// <summary>Binary-searches the current features for an exact type and setting pair.</summary>
    /// <param name="features">The sorted type and setting pairs.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <param name="featureType">The feature type.</param>
    /// <param name="setting">The selector.</param>
    /// <returns>True when the pair is present.</returns>
    [<TailCall>]
    let rec private findFeature
        (features: ImmutableArray<struct (uint16 * uint16)>) (lo: int) (hi: int) (featureType: uint16) (setting: uint16) : bool =
        match lo > hi with
        | true -> false
        | false ->
            let mid = (lo + hi) / 2
            let struct (kind, selector) = features[mid]
            match featureType < kind, featureType > kind with
            | true, _ -> findFeature features lo (mid - 1) featureType setting
            | false, true -> findFeature features (mid + 1) hi featureType setting
            | false, false ->
                match setting < selector, setting > selector with
                | true, _ -> findFeature features lo (mid - 1) featureType setting
                | false, true -> findFeature features (mid + 1) hi featureType setting
                | false, false -> true

    /// <summary>Applies one feature record's enable and disable flags to a mask.</summary>
    /// <param name="flags">The mask.</param>
    /// <param name="feature">The record.</param>
    /// <returns>The updated mask.</returns>
    let private applyFeature (flags: uint32) (feature: MorxFeature) =
        (flags &&& feature.DisableFlags) ||| feature.EnableFlags

    /// <summary>Whether one feature record applies to the current features, <c>Chain::compile_flags</c>'s tests.</summary>
    /// <param name="currentFeatures">The sorted, deduplicated active features.</param>
    /// <param name="ltag">The ltag table.</param>
    /// <param name="language">The segment language.</param>
    /// <param name="feature">The feature record.</param>
    /// <returns>True when the record's flags apply.</returns>
    let private featureApplies
        (currentFeatures: ImmutableArray<struct (uint16 * uint16)>) (ltag: Ltag) (language: Language) (feature: MorxFeature) : bool =
        match findFeature currentFeatures 0 (currentFeatures.Length - 1) feature.FeatureType feature.FeatureSetting with
        | true -> true
        | false ->
            match feature.FeatureType = FeatureTypeLetterCase && feature.FeatureSetting = SelectorSmallCaps with
            | true -> findFeature currentFeatures 0 (currentFeatures.Length - 1) FeatureTypeLowerCase SelectorLowerCaseSmallCaps
            | false ->
                feature.FeatureType = FeatureTypeLanguageTag
                && feature.FeatureSetting <> 0us
                && Ltag.matches ltag (int feature.FeatureSetting - 1) language

    /// <summary>Compiles the flags of one chain, <c>Chain::compile_flags</c>.</summary>
    /// <param name="chain">The chain.</param>
    /// <param name="currentFeatures">The sorted, deduplicated active features.</param>
    /// <param name="ltag">The ltag table.</param>
    /// <param name="language">The segment language.</param>
    /// <param name="flags">The flags compiled so far.</param>
    /// <param name="index">The next feature record.</param>
    /// <returns>The chain flags.</returns>
    [<TailCall>]
    let rec private compileFeatureRecords
        (chain: MorxChain) (currentFeatures: ImmutableArray<struct (uint16 * uint16)>) (ltag: Ltag) (language: Language)
        (flags: uint32) (index: int) : uint32 =
        match index >= chain.Features.Length with
        | true -> flags
        | false ->
            let feature = chain.Features[index]
            let flags =
                match featureApplies currentFeatures ltag language feature with
                | true -> applyFeature flags feature
                | false -> flags
            compileFeatureRecords chain currentFeatures ltag language flags (index + 1)

    /// <summary>Compiles the flags of one chain, <c>Chain::compile_flags</c>.</summary>
    /// <param name="morx">The table.</param>
    /// <param name="chainIndex">The chain index.</param>
    /// <param name="currentFeatures">The sorted, deduplicated active features.</param>
    /// <param name="ltag">The ltag table.</param>
    /// <param name="language">The segment language.</param>
    /// <returns>The chain flags.</returns>
    let compileFlags
        (morx: Morx) (chainIndex: int) (currentFeatures: ImmutableArray<struct (uint16 * uint16)>) (ltag: Ltag) (language: Language) : uint32 =
        let chain = morx.Chains[chainIndex]
        compileFeatureRecords chain currentFeatures ltag language chain.DefaultFlags 0

    /// <summary>Whether a chain has a feature record from an index on that reads the segment's language.</summary>
    /// <param name="features">The chain's feature records.</param>
    /// <param name="index">The next record.</param>
    /// <returns>True when a record is of the language tag type with a setting.</returns>
    [<TailCall>]
    let rec private readsLanguageFrom (features: ImmutableArray<MorxFeature>) (index: int) =
        match index >= features.Length with
        | true -> false
        | false ->
            match features[index].FeatureType = FeatureTypeLanguageTag && features[index].FeatureSetting <> 0us with
            | true -> true
            | false -> readsLanguageFrom features (index + 1)

    /// <summary>Whether a chain from an index on has a feature record that reads the segment's language.</summary>
    /// <param name="chains">The chains.</param>
    /// <param name="index">The next chain.</param>
    /// <returns>True when a chain reads the language.</returns>
    [<TailCall>]
    let rec private chainsReadLanguage (chains: ImmutableArray<MorxChain>) (index: int) =
        match index >= chains.Length with
        | true -> false
        | false ->
            match readsLanguageFrom chains[index].Features 0 with
            | true -> true
            | false -> chainsReadLanguage chains (index + 1)

    /// <summary>The default map of the chains when no feature record reads the language, <c>compile</c> without user features: one
    /// global range per chain, the same for every segment.</summary>
    /// <param name="chains">The chains.</param>
    /// <returns>The map, or none when a chain reads the language and the map depends on the segment.</returns>
    let private defaultMapOf (chains: ImmutableArray<MorxChain>) =
        match chainsReadLanguage chains 0 with
        | true -> ValueNone
        | false ->
            let ltag: Ltag = { Table = ReadOnlyMemory<byte>.Empty; Count = 0 }
            let flagsOf (chain: MorxChain) =
                let flags = compileFeatureRecords chain ImmutableArray<struct (uint16 * uint16)>.Empty ltag NoLanguage chain.DefaultFlags 0
                Sealed.init 1 (fun _ -> { Flags = flags; ClusterFirst = 0u; ClusterLast = UInt32.MaxValue })
            ValueSome { ChainFlags = Sealed.map flagsOf (chains.AsSpan()) }

    /// <summary>Loads and sanitizes a morx or mort table.</summary>
    /// <param name="face">The face.</param>
    /// <param name="isMort">True for the mort table, false for morx.</param>
    /// <returns>The chains, empty when the table is absent, fails or is blocklisted.</returns>
    let load (face: Face) (isMort: bool) =
        let tag = if isMort then Tags.Mort else Tags.Morx
        let sanitizer: Sanitizer = { IsMort = isMort; NumGlyphs = face.GlyphCount }
        let table = Sanitize.table sanitizer (Face.referenceTable face tag)
        match table.Length with
        | 0 -> empty isMort
        | _ ->
            match isBlocklisted table.Length face isMort with
            | true -> empty isMort
            | false ->
                let hbuintSize = if isMort then 2 else 4
                let chainCount = int (Binary.u32 table.Span 4)
                let chains = readChains table HeaderSize chainCount hbuintSize face.GlyphCount (PointBuilder.create())
                { IsMort = isMort
                  Version = uint32 (Binary.u16 table.Span 0)
                  Chains = chains
                  DefaultMap = lazy (defaultMapOf chains) }
