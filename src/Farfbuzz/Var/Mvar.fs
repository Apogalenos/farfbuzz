namespace Farfbuzz.OpenType

open System
open Farfbuzz
open Farfbuzz.Var

/// <summary>The MVAR table: the sorted value records and the item variation store behind them.</summary>
[<NoEquality; NoComparison>]
type internal Mvar =
    { RecordSize: int
      Records: ReadOnlyMemory<byte>
      Count: int
      Store: ItemVariationStore }

/// <summary>The MVAR table of a face, <c>OT::MVAR</c>.</summary>
[<RequireQualifiedAccess>]
module internal Mvar =
    /// <summary>The size of the MVAR header through the var-store offset.</summary>
    [<Literal>]
    let private HeaderSize = 12

    /// <summary>The size of one VariationValueRecord, the value <c>valueRecordSize</c> must reach.</summary>
    [<Literal>]
    let private VariationValueRecordSize = 8

    /// <summary>The offset of the value-record size word.</summary>
    [<Literal>]
    let private RecordSizeOffset = 6

    /// <summary>The offset of the value-record count word.</summary>
    [<Literal>]
    let private RecordCountOffset = 8

    /// <summary>The offset of the var-store offset word.</summary>
    [<Literal>]
    let private VarStoreOffset = 10

    /// <summary>The offset of a record's delta-set index, after its four-byte tag.</summary>
    [<Literal>]
    let private VarIdxOffset = 4

    /// <summary>The Null MVAR of a face without a usable MVAR table.</summary>
    let empty =
        { RecordSize = 0
          Records = ReadOnlyMemory<byte>.Empty
          Count = 0
          Store = ItemVariationStore.empty }

    /// <summary>Sanitizes the var store at the header offset, the Null store on a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The store offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeStore (table: ReadOnlySpan<byte>) (offset: int) (ops: int) : int voption =
        match offset with
        | 0 -> ValueSome ops
        | _ -> ItemVariationStore.sanitize table offset ops

    /// <summary>Sanitizes an MVAR table, <c>MVAR::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) ops =
        match Binary.u16 table 0 = 1us && Sanitize.fits table.Length 0L HeaderSize with
        | false -> ValueNone
        | true ->
            let recordSize = int (Binary.u16 table RecordSizeOffset)
            let recordCount = int (Binary.u16 table RecordCountOffset)
            let storeOffset = int (Binary.u16 table VarStoreOffset)
            match recordSize >= VariationValueRecordSize with
            | false -> ValueNone
            | true ->
                match sanitizeStore table storeOffset ops with
                | ValueNone -> ValueNone
                | ValueSome remaining ->
                    Sanitize.array table.Length (int64 HeaderSize) (uint32 recordCount) (uint32 recordSize) remaining

    /// <summary>The struct form of the MVAR sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Reads the var store at the header offset, the Null store on a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The store offset.</param>
    /// <returns>The store.</returns>
    let private readStore (table: ReadOnlyMemory<byte>) (at: int) : ItemVariationStore =
        match at with
        | 0 -> ItemVariationStore.empty
        | _ -> ItemVariationStore.read table at

    /// <summary>Loads the MVAR table, empty on sanitize failure, <c>MVAR</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed MVAR, or empty.</returns>
    let load face =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Mvar)
        let recordSize = int (Binary.u16 table.Span RecordSizeOffset)
        let recordCount = int (Binary.u16 table.Span RecordCountOffset)
        let storeOffset = int (Binary.u16 table.Span VarStoreOffset)
        let records =
            match recordCount with
            | 0 -> ReadOnlyMemory<byte>.Empty
            | count -> table.Slice(HeaderSize, count * recordSize)
        { RecordSize = recordSize
          Records = records
          Count = recordCount
          Store = readStore table storeOffset }

    /// <summary>Compares a metric tag with one sorted value record.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private TagComparer =
        { Key: Tag
          RecordSize: int }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let recordTag = Binary.tag data (index * this.RecordSize)
                match this.Key > recordTag, this.Key < recordTag with
                | true, _ -> 1
                | false, true -> -1
                | false, false -> 0

    /// <summary>The variation delta of one metric tag, <c>MVAR::get_var</c>.</summary>
    /// <param name="mvar">The MVAR table.</param>
    /// <param name="instance">The store instance holding the region scalars.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The delta, 0 for a missing tag.</returns>
    let getVar (mvar: Mvar) (instance: StoreInstance) (tag: Tag) : float32 =
        let comparer = { TagComparer.Key = tag; RecordSize = mvar.RecordSize }
        match Binary.bsearch comparer (mvar.Records.Span) 0 (mvar.Count - 1) with
        | ValueNone -> 0.0f
        | ValueSome index -> ItemVariationStore.delta instance (Binary.u32 (mvar.Records.Span) (index * mvar.RecordSize + VarIdxOffset))
