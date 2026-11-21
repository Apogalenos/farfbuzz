namespace Farfbuzz.Var

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The parsed FeatureVariations table: its bytes, its feature-variation records and its lookup-variation records.</summary>
type internal FeatureVariations =
    { Bytes: ReadOnlyMemory<byte>
      Records: ImmutableArray<struct (int * int)>
      LookupRecords: ImmutableArray<struct (uint16 * int)> }

/// <summary>The FeatureVariations 1.0/1.1 table, <c>OT::FeatureVariations</c>.</summary>
[<RequireQualifiedAccess>]
module internal FeatureVariations =
    /// <summary>The version that adds the lookup-variation records.</summary>
    [<Literal>]
    let private V1_1 = 0x00010001u

    /// <summary>The no-match sentinel, <c>NOT_FOUND_INDEX</c>.</summary>
    [<Literal>]
    let NotFound = 0xFFFFFFFFu

    /// <summary>The byte offset of the count field of an And or Or condition.</summary>
    [<Literal>]
    let private AndOrCountOffset = 2

    /// <summary>The byte offset of the first Offset24 child of an And or Or condition.</summary>
    [<Literal>]
    let private AndOrOffsetsOffset = 3

    /// <summary>The width of one Offset24 child offset.</summary>
    [<Literal>]
    let private Offset24Size = 3

    /// <summary>The byte offset of the Offset24 child of a Negate condition.</summary>
    [<Literal>]
    let private NegateOffsetField = 2

    /// <summary>The byte offset of the first Offset32 child of a ConditionSet.</summary>
    [<Literal>]
    let private ConditionSetOffsetsOffset = 2

    /// <summary>The width of one Offset32 child offset.</summary>
    [<Literal>]
    let private Offset32Size = 4

    /// <summary>The size of one FeatureVariationRecord.</summary>
    [<Literal>]
    let private FeatureVariationRecordSize = 8

    /// <summary>The size of one LookupVariationRecord.</summary>
    [<Literal>]
    let private LookupVariationRecordSize = 6

    /// <summary>The size of one LookupConditionRecord.</summary>
    [<Literal>]
    let private LookupConditionRecordSize = 8

    /// <summary>The flag that adds the substituted feature's default lookups, <c>FeatureLookupsTable::adds_default_lookups</c>.</summary>
    [<Literal>]
    let private DefaultLookupsFlag = 0x0001us

    /// <summary>The resolution of one child offset: null, a resolved offset, or out of range.</summary>
    [<Struct>]
    type private Child =
        | NullChild
        | Child of at: int
        | ChildOutOfRange

    /// <summary>Resolves a child offset relative to a base, widening before adding the base.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="baseAt">The base offset the child offset is relative to.</param>
    /// <param name="offset">The unsigned child offset.</param>
    /// <returns>The resolution.</returns>
    let private resolveChild (bytes: ReadOnlyMemory<byte>) (baseAt: int) (offset: uint32) : Child =
        let span = bytes.Span
        match offset with
        | 0u -> NullChild
        | _ ->
            let at = int64 baseAt + int64 offset
            match at >= 0L && at <= int64 span.Length with
            | false -> ChildOutOfRange
            | true -> Child (int at)

    /// <summary>Sanitizes one condition, depth-bounded, <c>Condition::sanitize</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The condition offset.</param>
    /// <param name="depth">The remaining nesting depth; zero fails.</param>
    /// <returns>Whether the condition sanitizes.</returns>
    let rec private sanitizeCondition (bytes: ReadOnlyMemory<byte>) (at: int) (depth: int) : bool =
        match depth with
        | 0 -> false
        | _ ->
            let span = bytes.Span
            match Sanitize.fits span.Length (int64 at) 2 with
            | false -> false
            | true ->
                match int (Binary.u16 span at) with
                | 1 -> Sanitize.fits span.Length (int64 at) 8
                | 2 -> Sanitize.fits span.Length (int64 at) 8
                | 3 -> sanitizeAndOr bytes at (depth - 1)
                | 4 -> sanitizeAndOr bytes at (depth - 1)
                | 5 -> sanitizeNegate bytes at (depth - 1)
                | _ -> true

    /// <summary>Sanitizes a format-3 or format-4 condition's children, <c>ConditionAnd::sanitize</c> and <c>ConditionOr::sanitize</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The condition offset.</param>
    /// <param name="depth">The remaining nesting depth.</param>
    /// <returns>Whether the condition sanitizes.</returns>
    and private sanitizeAndOr (bytes: ReadOnlyMemory<byte>) (at: int) (depth: int) : bool =
        let span = bytes.Span
        let count = int (Binary.u8 span (at + AndOrCountOffset))
        match Sanitize.fits span.Length (int64 at) (3 + count * 3) with
        | false -> false
        | true -> sanitizeAndOrChildren bytes at depth 0 count

    /// <summary>Sanitizes each child of an And or Or condition.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The condition offset.</param>
    /// <param name="depth">The remaining nesting depth.</param>
    /// <param name="index">The next child.</param>
    /// <param name="count">The child count.</param>
    /// <returns>Whether every child sanitizes.</returns>
    and [<TailCall>] private sanitizeAndOrChildren (bytes: ReadOnlyMemory<byte>) (at: int) (depth: int) (index: int) (count: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = bytes.Span
            match resolveChild bytes at (Binary.u24 span (at + AndOrOffsetsOffset + index * Offset24Size)) with
            | NullChild -> sanitizeAndOrChildren bytes at depth (index + 1) count
            | ChildOutOfRange -> false
            | Child childAt ->
                match sanitizeCondition bytes childAt depth with
                | true -> sanitizeAndOrChildren bytes at depth (index + 1) count
                | false -> false

    /// <summary>Sanitizes a format-5 Negate condition, <c>ConditionNegate::sanitize</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The condition offset.</param>
    /// <param name="depth">The remaining nesting depth.</param>
    /// <returns>Whether the condition sanitizes.</returns>
    and private sanitizeNegate (bytes: ReadOnlyMemory<byte>) (at: int) (depth: int) : bool =
        let span = bytes.Span
        match Sanitize.fits span.Length (int64 at) 5 with
        | false -> false
        | true ->
            match resolveChild bytes at (Binary.u24 span (at + NegateOffsetField)) with
            | NullChild -> true
            | ChildOutOfRange -> false
            | Child childAt -> sanitizeCondition bytes childAt depth

    /// <summary>Sanitizes each condition of a ConditionSet.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The ConditionSet offset.</param>
    /// <param name="index">The next condition.</param>
    /// <param name="count">The condition count.</param>
    /// <returns>Whether every condition sanitizes.</returns>
    [<TailCall>]
    let rec private sanitizeConditionSetChildren (bytes: ReadOnlyMemory<byte>) (at: int) (index: int) (count: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = bytes.Span
            match resolveChild bytes at (Binary.u32 span (at + ConditionSetOffsetsOffset + index * Offset32Size)) with
            | NullChild -> sanitizeConditionSetChildren bytes at (index + 1) count
            | ChildOutOfRange -> false
            | Child childAt ->
                match sanitizeCondition bytes childAt Conditions.MaxNestingLevel with
                | true -> sanitizeConditionSetChildren bytes at (index + 1) count
                | false -> false

    /// <summary>Sanitizes the conditions of a ConditionSet, <c>ConditionSet::sanitize</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The ConditionSet offset.</param>
    /// <returns>Whether the ConditionSet sanitizes.</returns>
    let private sanitizeConditionSet (bytes: ReadOnlyMemory<byte>) (at: int) : bool =
        let span = bytes.Span
        let count = int (Binary.u16 span at)
        match Sanitize.fits span.Length (int64 at) (2 + count * 2) with
        | false -> false
        | true -> sanitizeConditionSetChildren bytes at 0 count

    /// <summary>Sanitizes a substituted Feature table, <c>Feature::sanitize</c> without the parameter table.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The Feature offset.</param>
    /// <returns>Whether the Feature sanitizes.</returns>
    let private sanitizeFeature (bytes: ReadOnlyMemory<byte>) (at: int) : bool =
        let span = bytes.Span
        match Sanitize.fits span.Length (int64 at) 4 with
        | false -> false
        | true ->
            let lookupCount = int (Binary.u16 span (at + 2))
            Sanitize.fits span.Length (int64 (at + 4)) (lookupCount * 2)

    /// <summary>Sanitizes each substitution record.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The substitution offset.</param>
    /// <param name="index">The next record.</param>
    /// <param name="count">The record count.</param>
    /// <returns>Whether every record sanitizes.</returns>
    [<TailCall>]
    let rec private sanitizeSubstitutionRecords (bytes: ReadOnlyMemory<byte>) (at: int) (index: int) (count: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = bytes.Span
            let record = at + 6 + index * 6
            match resolveChild bytes at (Binary.u32 span (record + 2)) with
            | NullChild -> sanitizeSubstitutionRecords bytes at (index + 1) count
            | ChildOutOfRange -> false
            | Child featureAt ->
                match sanitizeFeature bytes featureAt with
                | true -> sanitizeSubstitutionRecords bytes at (index + 1) count
                | false -> false

    /// <summary>Sanitizes a FeatureTableSubstitution, <c>FeatureTableSubstitution::sanitize</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The substitution offset.</param>
    /// <returns>Whether the substitution sanitizes.</returns>
    let private sanitizeSubstitution (bytes: ReadOnlyMemory<byte>) (at: int) : bool =
        let span = bytes.Span
        match Sanitize.fits span.Length (int64 at) 6 with
        | false -> false
        | true ->
            match Binary.u32 span at >>> 16 = 1u with
            | false -> false
            | true ->
                let count = int (Binary.u16 span (at + 4))
                match Sanitize.fits span.Length (int64 at) (6 + count * 6) with
                | false -> false
                | true -> sanitizeSubstitutionRecords bytes at 0 count

    /// <summary>Sanitizes a LookupIndexList, <c>LookupIndexList::sanitize</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The lookup-index-list offset.</param>
    /// <returns>Whether the list sanitizes.</returns>
    let private sanitizeLookupIndexList (bytes: ReadOnlyMemory<byte>) (at: int) : bool =
        let span = bytes.Span
        let count = int (Binary.u16 span at)
        Sanitize.fits span.Length (int64 at) (2 + count * 2)

    /// <summary>Sanitizes each LookupConditionRecord of a FeatureLookupsTable.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The feature-lookups offset.</param>
    /// <param name="index">The next record.</param>
    /// <param name="count">The record count.</param>
    /// <returns>Whether every record sanitizes.</returns>
    [<TailCall>]
    let rec private sanitizeLookupConditionRecords (bytes: ReadOnlyMemory<byte>) (at: int) (index: int) (count: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = bytes.Span
            let record = at + 10 + index * LookupConditionRecordSize
            let conditionOk =
                match resolveChild bytes at (Binary.u32 span record) with
                | NullChild -> true
                | ChildOutOfRange -> false
                | Child conditionAt -> sanitizeCondition bytes conditionAt Conditions.MaxNestingLevel
            let lookupsOk =
                match resolveChild bytes at (Binary.u32 span (record + 4)) with
                | NullChild -> true
                | ChildOutOfRange -> false
                | Child listAt -> sanitizeLookupIndexList bytes listAt
            match conditionOk && lookupsOk with
            | true -> sanitizeLookupConditionRecords bytes at (index + 1) count
            | false -> false

    /// <summary>Sanitizes a FeatureLookupsTable, <c>FeatureLookupsTable::sanitize</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="at">The feature-lookups offset.</param>
    /// <returns>Whether the table sanitizes.</returns>
    let private sanitizeFeatureLookups (bytes: ReadOnlyMemory<byte>) (at: int) : bool =
        let span = bytes.Span
        match Sanitize.fits span.Length (int64 at) 10 with
        | false -> false
        | true ->
            match Binary.u32 span at >>> 16 = 1u with
            | false -> false
            | true ->
                let recordCount = Binary.u32 span (at + 6)
                match 10L + int64 recordCount * int64 LookupConditionRecordSize <= int64 span.Length with
                | false -> false
                | true -> sanitizeLookupConditionRecords bytes at 0 (int recordCount)

    /// <summary>Sanitizes the FeatureVariationRecords.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="index">The next record.</param>
    /// <param name="count">The record count.</param>
    /// <returns>Whether every record sanitizes.</returns>
    [<TailCall>]
    let rec private sanitizeVarRecords (bytes: ReadOnlyMemory<byte>) (index: int) (count: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = bytes.Span
            let record = 8 + index * FeatureVariationRecordSize
            let conditionsOk =
                match resolveChild bytes 0 (Binary.u32 span record) with
                | NullChild -> true
                | ChildOutOfRange -> false
                | Child conditionsAt -> sanitizeConditionSet bytes conditionsAt
            let substitutionsOk =
                match resolveChild bytes 0 (Binary.u32 span (record + 4)) with
                | NullChild -> true
                | ChildOutOfRange -> false
                | Child substitutionsAt -> sanitizeSubstitution bytes substitutionsAt
            match conditionsOk && substitutionsOk with
            | true -> sanitizeVarRecords bytes (index + 1) count
            | false -> false

    /// <summary>Sanitizes the LookupVariationRecords.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="lookupStart">The lookup-record array offset.</param>
    /// <param name="index">The next record.</param>
    /// <param name="count">The record count.</param>
    /// <returns>Whether every record sanitizes.</returns>
    [<TailCall>]
    let rec private sanitizeLookupVarRecords (bytes: ReadOnlyMemory<byte>) (lookupStart: int) (index: int) (count: int) : bool =
        match index >= count with
        | true -> true
        | false ->
            let span = bytes.Span
            let record = lookupStart + 4 + index * LookupVariationRecordSize
            match resolveChild bytes 0 (Binary.u32 span (record + 2)) with
            | NullChild -> sanitizeLookupVarRecords bytes lookupStart (index + 1) count
            | ChildOutOfRange -> false
            | Child featureLookupsAt ->
                match sanitizeFeatureLookups bytes featureLookupsAt with
                | true -> sanitizeLookupVarRecords bytes lookupStart (index + 1) count
                | false -> false

    /// <summary>Sanitizes the lookup-variation records and returns their count.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="lookupStart">The lookup-record array offset.</param>
    /// <returns>The record count, or none when invalid.</returns>
    let private sanitizeLookupHeader (bytes: ReadOnlyMemory<byte>) (lookupStart: int) : int voption =
        let span = bytes.Span
        match Sanitize.fits span.Length (int64 lookupStart) 4 with
        | false -> ValueNone
        | true ->
            let lookupCount = Binary.u32 span lookupStart
            match 4L + int64 lookupCount * int64 LookupVariationRecordSize <= int64 span.Length - int64 lookupStart with
            | false -> ValueNone
            | true ->
                match sanitizeLookupVarRecords bytes lookupStart 0 (int lookupCount) with
                | true -> ValueSome (int lookupCount)
                | false -> ValueNone

    /// <summary>Unfolds the FeatureVariationRecords, reading the conditions and substitutions offsets.</summary>
    [<Struct>]
    type private RecordUnfolder =
        { Base: int }
        interface ISpanUnfolder<byte, int, struct (int * int)> with
            member this.Next (source, index) =
                let record = this.Base + index * FeatureVariationRecordSize
                struct (struct (int (Binary.u32 source record), int (Binary.u32 source (record + 4))), index + 1)

    /// <summary>Unfolds the LookupVariationRecords, reading the feature index and the feature-lookups offset.</summary>
    [<Struct>]
    type private LookupRecordUnfolder =
        { Base: int }
        interface ISpanUnfolder<byte, int, struct (uint16 * int)> with
            member this.Next (source, index) =
                let record = this.Base + index * LookupVariationRecordSize
                struct (struct (Binary.u16 source record, int (Binary.u32 source (record + 2))), index + 1)

    /// <summary>Unfolds a big-endian uint16 array.</summary>
    [<Struct>]
    type private Uint16Unfolder =
        { Base: int }
        interface ISpanUnfolder<byte, int, uint16> with
            member this.Next (source, index) =
                struct (Binary.u16 source (this.Base + index * 2), index + 1)

    /// <summary>Builds the FeatureVariationRecords, each a conditions and a substitutions offset.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="recordCount">The record count.</param>
    /// <returns>The records.</returns>
    let private buildRecords (bytes: ReadOnlyMemory<byte>) (recordCount: int) : ImmutableArray<struct (int * int)> =
        Sealed.unfoldSpan bytes.Span recordCount 0 { RecordUnfolder.Base = 8 }

    /// <summary>Builds the LookupVariationRecords, each a feature index and a feature-lookups offset.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="lookupStart">The lookup-record array offset.</param>
    /// <param name="lookupCount">The record count.</param>
    /// <returns>The records.</returns>
    let private buildLookupRecords (bytes: ReadOnlyMemory<byte>) (lookupStart: int) (lookupCount: int) : ImmutableArray<struct (uint16 * int)> =
        Sealed.unfoldSpan bytes.Span lookupCount 0 { LookupRecordUnfolder.Base = lookupStart + 4 }

    /// <summary>Parses the lookup-variation records after the FeatureVariationRecords.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="records">The FeatureVariationRecords.</param>
    /// <param name="lookupStart">The lookup-record array offset.</param>
    /// <returns>The table, or none when invalid.</returns>
    let private parseLookupRecords (bytes: ReadOnlyMemory<byte>) (records: ImmutableArray<struct (int * int)>) (lookupStart: int) : FeatureVariations voption =
        match sanitizeLookupHeader bytes lookupStart with
        | ValueNone -> ValueNone
        | ValueSome lookupCount -> ValueSome { Bytes = bytes; Records = records; LookupRecords = buildLookupRecords bytes lookupStart lookupCount }

    /// <summary>Parses the FeatureVariationRecords after the header, then the lookup records.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="version">The FeatureVariations version.</param>
    /// <param name="recordCount">The record count.</param>
    /// <returns>The table, or none when invalid.</returns>
    let private parseRecords (bytes: ReadOnlyMemory<byte>) (version: uint32) (recordCount: int) : FeatureVariations voption =
        match sanitizeVarRecords bytes 0 recordCount with
        | false -> ValueNone
        | true ->
            let records = buildRecords bytes recordCount
            match version >= V1_1 with
            | false -> ValueSome { Bytes = bytes; Records = records; LookupRecords = ImmutableArray.Empty }
            | true -> parseLookupRecords bytes records (8 + recordCount * FeatureVariationRecordSize)

    /// <summary>Parses and sanitizes a FeatureVariations slice, <c>FeatureVariations::sanitize</c> plus the record read.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <returns>The table, or none when invalid.</returns>
    let private parse (bytes: ReadOnlyMemory<byte>) : FeatureVariations voption =
        match bytes.Length with
        | 0 -> ValueNone
        | _ ->
            let span = bytes.Span
            let version = Binary.u32 span 0
            match version >>> 16 = 1u with
            | false -> ValueNone
            | true ->
                let recordCount = Binary.u32 span 4
                match 8L + int64 recordCount * int64 FeatureVariationRecordSize <= int64 bytes.Length with
                | false -> ValueNone
                | true -> parseRecords bytes version (int recordCount)

    /// <summary>The empty FeatureVariations, read from a missing or failed table.</summary>
    let empty =
        { Bytes = ReadOnlyMemory<byte>.Empty
          Records = ImmutableArray.Empty
          LookupRecords = ImmutableArray.Empty }

    /// <summary>Reads the FeatureVariations bytes, empty when absent or invalid.</summary>
    /// <param name="bytes">The FeatureVariations bytes of a sanitized GSUB/GPOS table.</param>
    /// <returns>The FeatureVariations.</returns>
    let read (bytes: ReadOnlyMemory<byte>) : FeatureVariations =
        match bytes.Length with
        | 0 -> empty
        | _ ->
            match parse bytes with
            | ValueNone -> empty
            | ValueSome fv -> fv

    /// <summary>Finds the first record whose condition set holds, <c>FeatureVariations::find_index</c>.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="index">The next record.</param>
    /// <returns>The record index, or none.</returns>
    [<TailCall>]
    let rec private findIndexFrom (fv: FeatureVariations) (instancer: Instancer) (index: int) : uint32 voption =
        match index >= fv.Records.Length with
        | true -> ValueNone
        | false ->
            let struct (conditionsOffset, _) = fv.Records[index]
            let holds =
                match conditionsOffset with
                | 0 -> true
                | _ -> Conditions.evaluateSet fv.Bytes conditionsOffset instancer
            match holds with
            | true -> ValueSome (uint32 index)
            | false -> findIndexFrom fv instancer (index + 1)

    /// <summary>Finds the variations index of an instance, <c>FeatureVariations::find_index</c>.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="instancer">The instancer.</param>
    /// <returns>The record index, or <see cref="NotFound"/>.</returns>
    let findIndex (fv: FeatureVariations) (instancer: Instancer) : uint32 =
        match findIndexFrom fv instancer 0 with
        | ValueSome index -> index
        | ValueNone -> NotFound

    /// <summary>Reads the lookup list of a Feature table, <c>Feature::add_lookup_indexes_to</c>.</summary>
    /// <param name="span">The FeatureVariations bytes.</param>
    /// <param name="featureAt">The Feature offset.</param>
    /// <returns>The lookup indices in file order.</returns>
    let private featureLookupList (span: ReadOnlySpan<byte>) (featureAt: int) : ImmutableArray<uint16> =
        let count = int (Binary.u16 span (featureAt + 2))
        Sealed.unfoldSpan span count 0 { Uint16Unfolder.Base = featureAt + 4 }

    /// <summary>Finds the substituted Feature of a feature index, <c>FeatureTableSubstitution::find_substitute</c>.</summary>
    /// <param name="bytes">The FeatureVariations bytes.</param>
    /// <param name="substitutionsAt">The substitution offset.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <param name="index">The next substitution record.</param>
    /// <param name="count">The record count.</param>
    /// <returns>The substituted feature's lookups, or none when no record matches.</returns>
    [<TailCall>]
    let rec private findSubstitute (bytes: ReadOnlyMemory<byte>) (substitutionsAt: int) (featureIndex: int) (index: int) (count: int) : ImmutableArray<uint16> voption =
        match index >= count with
        | true -> ValueNone
        | false ->
            let span = bytes.Span
            let record = substitutionsAt + 6 + index * 6
            match int (Binary.u16 span record) = featureIndex with
            | false -> findSubstitute bytes substitutionsAt featureIndex (index + 1) count
            | true ->
                match Binary.u32 span (record + 2) with
                | 0u -> ValueSome ImmutableArray.Empty
                | featureOffset -> ValueSome (featureLookupList span (substitutionsAt + int featureOffset))

    /// <summary>The substituted feature's lookups, or none when there is no substitute.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="variationsIndex">The variations index, or <see cref="NotFound"/>.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <returns>The substituted feature's lookups, or none.</returns>
    let private substitutedLookups (fv: FeatureVariations) (variationsIndex: uint32) (featureIndex: int) : ImmutableArray<uint16> voption =
        match variationsIndex = NotFound with
        | true -> ValueNone
        | false ->
            match int variationsIndex < fv.Records.Length with
            | false -> ValueNone
            | true ->
                let struct (_, substitutionsOffset) = fv.Records[int variationsIndex]
                match substitutionsOffset with
                | 0 -> ValueNone
                | _ ->
                    let count = int (Binary.u16 fv.Bytes.Span (substitutionsOffset + 4))
                    findSubstitute fv.Bytes substitutionsOffset featureIndex 0 count

    /// <summary>The original feature's lookups, or none past the end.</summary>
    /// <param name="layout">The GSUB or GPOS table.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <returns>The lookups in file order.</returns>
    let private originalLookups (layout: LayoutTable<'S>) (featureIndex: int) : ImmutableArray<uint16> =
        match featureIndex >= 0 && featureIndex < layout.Features.Length with
        | true -> layout.Features[featureIndex].LookupIndices
        | false -> ImmutableArray.Empty

    /// <summary>Finds the first lookup-variation record of a feature index.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <param name="index">The next record.</param>
    /// <returns>The record, or none.</returns>
    [<TailCall>]
    let rec private findLookupRecordFrom (fv: FeatureVariations) (featureIndex: int) (index: int) : struct (uint16 * int) voption =
        match index >= fv.LookupRecords.Length with
        | true -> ValueNone
        | false ->
            let struct (recordFeatureIndex, offset) = fv.LookupRecords[index]
            match int recordFeatureIndex = featureIndex with
            | true -> ValueSome (struct (recordFeatureIndex, offset))
            | false -> findLookupRecordFrom fv featureIndex (index + 1)

    /// <summary>Reads the lookup list of one LookupIndexList.</summary>
    /// <param name="span">The FeatureVariations bytes.</param>
    /// <param name="listAt">The lookup-index-list offset.</param>
    /// <returns>The lookups in file order.</returns>
    let private lookupIndexList (span: ReadOnlySpan<byte>) (listAt: int) : ImmutableArray<uint16> =
        let count = int (Binary.u16 span listAt)
        Sealed.unfoldSpan span count 0 { Uint16Unfolder.Base = listAt + 2 }

    /// <summary>Collects the lookups of the LookupConditionRecords whose condition is null or true.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="featureLookupsAt">The feature-lookups offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="recordCount">The record count.</param>
    /// <param name="index">The next record.</param>
    /// <param name="acc">The lookups collected so far.</param>
    /// <returns>The collected lookups.</returns>
    [<TailCall>]
    let rec private collectConditionLookups
            (fv: FeatureVariations) (featureLookupsAt: int) (instancer: Instancer) (recordCount: int) (index: int) (acc: ImmutableArray<uint16>)
            : ImmutableArray<uint16> =
        match index >= recordCount with
        | true -> acc
        | false ->
            let span = fv.Bytes.Span
            let record = featureLookupsAt + 10 + index * LookupConditionRecordSize
            let applies =
                match Binary.u32 span record with
                | 0u -> true
                | conditionOffset -> Conditions.evaluate fv.Bytes (featureLookupsAt + int conditionOffset) instancer Conditions.MaxNestingLevel
            match applies with
            | false -> collectConditionLookups fv featureLookupsAt instancer recordCount (index + 1) acc
            | true ->
                let lookups =
                    match Binary.u32 span (record + 4) with
                    | 0u -> ImmutableArray.Empty
                    | listOffset -> lookupIndexList span (featureLookupsAt + int listOffset)
                collectConditionLookups fv featureLookupsAt instancer recordCount (index + 1) (Sealed.appendSpan (acc.AsSpan()) (lookups.AsSpan()))

    /// <summary>Sorts a lookup list and drops duplicates, the <c>hb_set_t</c> semantics.</summary>
    /// <param name="lookups">The lookups.</param>
    /// <returns>The sorted unique lookups.</returns>
    let private sortedUnique (lookups: ImmutableArray<uint16>) : ImmutableArray<uint16> =
        let pairs = Sealed.map (fun index -> struct (uint32 index, uint32 index)) (lookups.AsSpan())
        let sorted = Sealed.sortedMap (pairs.AsSpan())
        Sealed.map (fun (struct (key, _)) -> uint16 key) (sorted.AsSpan())

    /// <summary>
    /// The lookups a feature contributes at an instance: the lookup-variation set when one exists, else the
    /// substituted feature's lookups, else the original feature's.
    /// </summary>
    /// <param name="layout">The GSUB or GPOS table.</param>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="variationsIndex">The variations index, or <see cref="NotFound"/>.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <returns>The lookups.</returns>
    let featureLookups
            (layout: LayoutTable<'S>) (fv: FeatureVariations) (instancer: Instancer) (variationsIndex: uint32) (featureIndex: int)
            : ImmutableArray<uint16> =
        let baseLookups =
            match substitutedLookups fv variationsIndex featureIndex with
            | ValueSome lookups -> lookups
            | ValueNone -> originalLookups layout featureIndex
        match findLookupRecordFrom fv featureIndex 0 with
        | ValueNone -> baseLookups
        | ValueSome struct (_, featureLookupsAt) ->
            let span = fv.Bytes.Span
            let addsDefault = Binary.u16 span (featureLookupsAt + 4) &&& DefaultLookupsFlag <> 0us
            let defaultPart = match addsDefault with | true -> baseLookups | false -> ImmutableArray.Empty
            let recordCount = int (Binary.u32 span (featureLookupsAt + 6))
            let conditionPart = collectConditionLookups fv featureLookupsAt instancer recordCount 0 ImmutableArray.Empty
            sortedUnique (Sealed.appendSpan (defaultPart.AsSpan()) (conditionPart.AsSpan()))

    /// <summary>The lookups a feature contributes at an instance, reading the table's FeatureVariations and finding its index first.</summary>
    /// <param name="layout">The GSUB or GPOS table.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <returns>The lookups.</returns>
    let featureLookupsFor (layout: LayoutTable<'S>) (instancer: Instancer) (featureIndex: int) : ImmutableArray<uint16> =
        let fv = read layout.FeatureVariations
        featureLookups layout fv instancer (findIndex fv instancer) featureIndex

    /// <summary>Collects the satisfied condition offsets of one FeatureLookupsTable, <c>FeatureLookupsTable::collect_conditions</c>.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="featureLookupsAt">The feature-lookups offset.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="recordCount">The record count.</param>
    /// <param name="index">The next record.</param>
    /// <param name="acc">The offsets collected so far.</param>
    /// <returns>The collected offsets.</returns>
    [<TailCall>]
    let rec private collectTableConditions
            (fv: FeatureVariations) (featureLookupsAt: int) (instancer: Instancer) (recordCount: int) (index: int) (acc: ImmutableArray<uint32>)
            : ImmutableArray<uint32> =
        match index >= recordCount with
        | true -> acc
        | false ->
            let span = fv.Bytes.Span
            let record = featureLookupsAt + 10 + index * LookupConditionRecordSize
            let conditionField = Binary.u32 span record
            match conditionField with
            | 0u -> collectTableConditions fv featureLookupsAt instancer recordCount (index + 1) acc
            | _ ->
                let absolute = uint32 featureLookupsAt + conditionField
                match Conditions.evaluate fv.Bytes (int absolute) instancer Conditions.MaxNestingLevel with
                | false -> collectTableConditions fv featureLookupsAt instancer recordCount (index + 1) acc
                | true -> collectTableConditions fv featureLookupsAt instancer recordCount (index + 1) (Sealed.appendOne (acc.AsSpan()) absolute)

    /// <summary>Collects the satisfied condition offsets of every FeatureLookupsTable, <c>FeatureVariations::collect_lookup_conditions</c>.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="index">The next lookup-variation record.</param>
    /// <param name="acc">The offsets collected so far.</param>
    /// <returns>The collected offsets.</returns>
    [<TailCall>]
    let rec private collectLookupConditionOffsets (fv: FeatureVariations) (instancer: Instancer) (index: int) (acc: ImmutableArray<uint32>) : ImmutableArray<uint32> =
        match index >= fv.LookupRecords.Length with
        | true -> acc
        | false ->
            let struct (_, featureLookupsAt) = fv.LookupRecords[index]
            let recordCount = int (Binary.u32 fv.Bytes.Span (featureLookupsAt + 6))
            let acc = collectTableConditions fv featureLookupsAt instancer recordCount 0 acc
            collectLookupConditionOffsets fv instancer (index + 1) acc

    /// <summary>Collects the sorted distinct offsets of the satisfied conditions, <c>FeatureVariations::collect_lookup_conditions</c>.</summary>
    /// <param name="fv">The FeatureVariations.</param>
    /// <param name="instancer">The instancer.</param>
    /// <returns>The sorted distinct offsets.</returns>
    let lookupConditions (fv: FeatureVariations) (instancer: Instancer) : ImmutableArray<uint32> =
        let collected = collectLookupConditionOffsets fv instancer 0 ImmutableArray.Empty
        match collected.IsEmpty with
        | true -> ImmutableArray.Empty
        | false ->
            let pairs = Sealed.map (fun offset -> struct (offset, offset)) (collected.AsSpan())
            let sorted = Sealed.sortedMap (pairs.AsSpan())
            Sealed.map (fun (struct (key, _)) -> key) (sorted.AsSpan())
