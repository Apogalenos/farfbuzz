namespace Farfbuzz.Var

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>One VarData: the delta rows of an item variation store sub-table.</summary>
[<NoEquality; NoComparison>]
type internal VarData =
    { ItemCount: int
      WordCount: int
      LongWords: bool
      RegionIndices: ImmutableArray<uint16>
      Rows: ReadOnlyMemory<byte>
      RowSize: int }

/// <summary>An ItemVariationStore: the region list and the delta-set sub-tables.</summary>
[<NoEquality; NoComparison>]
type internal ItemVariationStore =
    { AxisCount: int
      RegionCount: int
      Regions: ImmutableArray<RegionAxis>
      Data: ImmutableArray<VarData> }

/// <summary>A store with its precomputed region scalars for one font instance.</summary>
[<Struct; NoComparison; NoEquality>]
type internal StoreInstance =
    { Store: ItemVariationStore
      Scalars: ImmutableArray<float32> }

/// <summary>The item variation store, <c>OT::ItemVariationStore</c>.</summary>
[<RequireQualifiedAccess>]
module internal ItemVariationStore =
    /// <summary>The size of the store header: format, regions offset and the data-set count.</summary>
    [<Literal>]
    let private StoreHeaderSize = 8

    /// <summary>The size of a VarRegionList header: axis count and region count.</summary>
    [<Literal>]
    let private RegionListHeaderSize = 4

    /// <summary>The size of one VarRegionAxis.</summary>
    [<Literal>]
    let private VarRegionAxisSize = 6

    /// <summary>The size of a VarData header: item count, word count and the region-index count.</summary>
    [<Literal>]
    let private VarDataHeaderSize = 6

    /// <summary>The size of one region index, a uint16.</summary>
    [<Literal>]
    let private RegionIndexSize = 2

    /// <summary>The size of a long-word column, an int32.</summary>
    [<Literal>]
    let private LongWordSize = 4

    /// <summary>The size of a short-word column, an int16.</summary>
    [<Literal>]
    let private ShortWordSize = 2

    /// <summary>The long-words bit of <c>wordSizeCount</c>, <c>LONG_WORDS</c>.</summary>
    [<Literal>]
    let private LongWordsMask = 0x8000us

    /// <summary>The word-count mask of <c>wordSizeCount</c>, <c>WORD_DELTA_COUNT_MASK</c>.</summary>
    [<Literal>]
    let private WordCountMask = 0x7FFFus

    /// <summary>The Null store of a face without a usable item variation store.</summary>
    let empty =
        { AxisCount = 0
          RegionCount = 0
          Regions = ImmutableArray<RegionAxis>.Empty
          Data = ImmutableArray.Empty }

    /// <summary>The Null VarData of a zero data-set offset.</summary>
    let private emptyVarData =
        { ItemCount = 0
          WordCount = 0
          LongWords = false
          RegionIndices = ImmutableArray.Empty
          Rows = ReadOnlyMemory<byte>.Empty
          RowSize = 0 }

    /// <summary>One step of the delta-sum walk, float32 or double.</summary>
    type private IDeltaSum<'T> =
        /// <summary>Combines one scalar-delta term into the running sum.</summary>
        /// <param name="acc">The running sum.</param>
        /// <param name="scalar">The region scalar.</param>
        /// <param name="delta">The delta value.</param>
        /// <returns>The new sum.</returns>
        abstract Add: acc: 'T -> scalar: float32 -> delta: int -> 'T

    /// <summary>The float32 delta sum, <c>Float = float</c> in <c>_get_delta</c>.</summary>
    [<Struct>]
    type private FloatSum =
        interface IDeltaSum<float32> with
            member _.Add acc scalar delta = acc + scalar * float32 delta

    /// <summary>The double delta sum, <c>Float = double</c> in <c>_get_delta</c>.</summary>
    [<Struct>]
    type private DoubleSum =
        interface IDeltaSum<float> with
            member _.Add acc scalar delta = acc + float scalar * float delta

    /// <summary>What width one delta column of a row is.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Column =
        | Long
        | Short
        | Byte

    /// <summary>Classifies a row column by its width, from <c>is_long</c>, <c>word_count</c> and the index.</summary>
    /// <param name="input">The struct of the long-words flag, word count and column index.</param>
    /// <returns>The column kind.</returns>
    let private (|Column|) (input: struct (bool * int * int)) : Column =
        let struct (isLong, wordCount, index) = input
        match isLong with
        | true ->
            match index < wordCount with
            | true -> Column.Long
            | false -> Column.Short
        | false ->
            match index < wordCount with
            | true -> Column.Short
            | false -> Column.Byte

    /// <summary>The byte offset of a short column, after the long words when the row has them.</summary>
    /// <param name="rowStart">The start of the row.</param>
    /// <param name="isLong">Whether the row has long words.</param>
    /// <param name="wordCount">The number of word columns.</param>
    /// <param name="index">The column index.</param>
    /// <returns>The column's byte offset.</returns>
    let private shortOffset (rowStart: int) (isLong: bool) (wordCount: int) (index: int) : int =
        match isLong with
        | true -> rowStart + wordCount * LongWordSize + (index - wordCount) * ShortWordSize
        | false -> rowStart + index * ShortWordSize

    /// <summary>Reads one column of a row as a signed int, in the order of <c>_get_delta</c>.</summary>
    /// <param name="rows">The delta bytes.</param>
    /// <param name="rowStart">The start of the row.</param>
    /// <param name="isLong">Whether the row has long words.</param>
    /// <param name="wordCount">The number of word columns.</param>
    /// <param name="index">The column index.</param>
    /// <returns>The delta value.</returns>
    let private readColumn (rows: ReadOnlySpan<byte>) (rowStart: int) (isLong: bool) (wordCount: int) (index: int) : int =
        match struct (isLong, wordCount, index) with
        | Column column ->
            match column with
            | Column.Long -> int (Binary.u32 rows (rowStart + index * LongWordSize))
            | Column.Short -> int (Binary.i16 rows (shortOffset rowStart isLong wordCount index))
            | Column.Byte -> int (sbyte (Binary.u8 rows (rowStart + wordCount * ShortWordSize + (index - wordCount))))

    /// <summary>The scalar of one region index, 0 past the region count, as <c>VarRegionList::evaluate</c> guards.</summary>
    /// <param name="scalars">The region scalars.</param>
    /// <param name="regionIndices">The region indices of a VarData.</param>
    /// <param name="index">The column index.</param>
    /// <returns>The scalar.</returns>
    let private scalarAt (scalars: ImmutableArray<float32>) (regionIndices: ReadOnlySpan<uint16>) (index: int) : float32 =
        let regionIndex = int regionIndices[index]
        match regionIndex < scalars.Length with
        | true -> scalars[regionIndex]
        | false -> 0.0f

    /// <summary>Walks one row's columns, accumulating scalar-delta terms in column order.</summary>
    /// <typeparam name="'A">The struct sum.</typeparam>
    /// <typeparam name="'T">The accumulator type.</typeparam>
    /// <param name="folder">The sum step.</param>
    /// <param name="data">The VarData.</param>
    /// <param name="scalars">The region scalars.</param>
    /// <param name="rowStart">The byte offset of the row.</param>
    /// <param name="index">The next column.</param>
    /// <param name="acc">The running sum.</param>
    /// <returns>The final sum.</returns>
    [<TailCall>]
    let rec private deltaRow<'A, 'T when 'A : struct and 'A :> IDeltaSum<'T>>
        (folder: 'A) (data: VarData) (scalars: ImmutableArray<float32>) (rowStart: int) (index: int) (acc: 'T) : 'T =
        match index >= data.RegionIndices.Length with
        | true -> acc
        | false ->
            let scalar = scalarAt scalars (data.RegionIndices.AsSpan()) index
            match scalar = 0.0f with
            | true -> deltaRow folder data scalars rowStart (index + 1) acc
            | false ->
                let delta = readColumn (data.Rows.Span) rowStart data.LongWords data.WordCount index
                deltaRow folder data scalars rowStart (index + 1) (folder.Add acc scalar delta)

    /// <summary>Sanitizes the region list at an absolute offset, <c>VarRegionList::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The region-list offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeRegionList (table: ReadOnlySpan<byte>) (offset: int64) (ops: int) : int voption =
        match Sanitize.fits table.Length offset RegionListHeaderSize with
        | false -> ValueNone
        | true ->
            let at = int offset
            let axisCount = uint32 (Binary.u16 table at)
            let regionCount = uint32 (Binary.u16 table (at + 2))
            Sanitize.array table.Length (offset + int64 RegionListHeaderSize) (axisCount * regionCount) (uint32 VarRegionAxisSize) ops

    /// <summary>Sanitizes a VarData whose header is in range, <c>VarData::sanitize</c> after the struct check.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The VarData offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeVarDataInRange (table: ReadOnlySpan<byte>) (offset: int64) (ops: int) : int voption =
        let at = int offset
        let itemCount = int (Binary.u16 table at)
        let wordSizeCount = Binary.u16 table (at + 2)
        let regionIndexCount = int (Binary.u16 table (at + 4))
        let wordCount = int (wordSizeCount &&& WordCountMask)
        let isLong = (wordSizeCount &&& LongWordsMask) <> 0us
        let rowSize =
            match isLong with
            | true -> (wordCount + regionIndexCount) * 2
            | false -> wordCount + regionIndexCount
        match wordCount <= regionIndexCount with
        | false -> ValueNone
        | true ->
            match Sanitize.array table.Length (offset + int64 VarDataHeaderSize) (uint32 regionIndexCount) (uint32 RegionIndexSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                let rowsAt = offset + int64 VarDataHeaderSize + int64 (regionIndexCount * RegionIndexSize)
                Sanitize.array table.Length rowsAt (uint32 itemCount) (uint32 rowSize) remaining

    /// <summary>Sanitizes one VarData at an absolute offset, <c>VarData::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The VarData offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeVarData (table: ReadOnlySpan<byte>) (offset: int64) (ops: int) : int voption =
        match Sanitize.fits table.Length offset VarDataHeaderSize with
        | false -> ValueNone
        | true -> sanitizeVarDataInRange table offset ops

    /// <summary>Sanitizes the data-set offsets and every VarData, <c>Array16OfOffset32To&lt;VarData&gt;::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="storeAt">The store offset.</param>
    /// <param name="index">The next data set.</param>
    /// <param name="count">The number of data sets.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeVarDatas (table: ReadOnlySpan<byte>) (storeAt: int) (index: int) (count: int) (ops: int) : int voption =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let offsetValue = Binary.u32 table (storeAt + StoreHeaderSize + index * 4)
            match offsetValue with
            | 0u -> sanitizeVarDatas table storeAt (index + 1) count ops
            | _ ->
                match sanitizeVarData table (int64 storeAt + int64 offsetValue) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeVarDatas table storeAt (index + 1) count remaining

    /// <summary>Sanitizes a store at an offset of its parent, <c>ItemVariationStore::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The store offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) at ops =
        match Binary.u16 table at = 1us && Sanitize.fits table.Length (int64 at) StoreHeaderSize with
        | false -> ValueNone
        | true ->
            let regionsOffset = Binary.u32 table (at + 2)
            let dataCount = int (Binary.u16 table (at + 6))
            let afterRegions =
                match regionsOffset with
                | 0u -> ValueSome ops
                | _ -> sanitizeRegionList table (int64 at + int64 regionsOffset) ops
            match afterRegions with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                match Sanitize.array table.Length (int64 (at + StoreHeaderSize)) (uint32 dataCount) 4u remaining with
                | ValueNone -> ValueNone
                | ValueSome afterArray -> sanitizeVarDatas table at 0 dataCount afterArray

    /// <summary>Unfolds the VarRegionAxis records of a region list.</summary>
    [<Struct>]
    type private RegionAxisUnfolder =
        val private recordsAt: int

        /// <summary>Creates a region-axis unfolder.</summary>
        /// <param name="recordsAt">The byte offset of the first record.</param>
        new (recordsAt) = { recordsAt = recordsAt }

        interface ISpanUnfolder<byte, int, RegionAxis> with
            member this.Next (source, index) =
                let at = this.recordsAt + index * VarRegionAxisSize
                struct ({ Start = Binary.i16 source at; Peak = Binary.i16 source (at + 2); End = Binary.i16 source (at + 4) }, index + 1)

    /// <summary>Unfolds the region indices of a VarData.</summary>
    [<Struct>]
    type private RegionIndexUnfolder =
        val private recordsAt: int

        /// <summary>Creates a region-index unfolder.</summary>
        /// <param name="recordsAt">The byte offset of the first index.</param>
        new (recordsAt) = { recordsAt = recordsAt }

        interface ISpanUnfolder<byte, int, uint16> with
            member this.Next (source, index) =
                struct (Binary.u16 source (this.recordsAt + index * RegionIndexSize), index + 1)

    /// <summary>Reads a region list at an absolute offset, <c>this+regions</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The region-list offset.</param>
    /// <returns>The axis count, region count and the flattened region axes.</returns>
    let private readRegionList (table: ReadOnlyMemory<byte>) (offset: int64) : struct (int * int * ImmutableArray<RegionAxis>) =
        let span = table.Span
        let at = int offset
        let axisCount = int (Binary.u16 span at)
        let regionCount = int (Binary.u16 span (at + 2))
        let regions = Sealed.unfoldSpan span (axisCount * regionCount) 0 (RegionAxisUnfolder(at + RegionListHeaderSize))
        struct (axisCount, regionCount, regions)

    /// <summary>Reads one VarData at an absolute offset, <c>this+dataSets[i]</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The VarData offset.</param>
    /// <returns>The VarData.</returns>
    let private readVarData (table: ReadOnlyMemory<byte>) (offset: int64) : VarData =
        let span = table.Span
        let at = int offset
        let itemCount = int (Binary.u16 span at)
        let wordSizeCount = Binary.u16 span (at + 2)
        let regionIndexCount = int (Binary.u16 span (at + 4))
        let wordCount = int (wordSizeCount &&& WordCountMask)
        let isLong = (wordSizeCount &&& LongWordsMask) <> 0us
        let rowSize =
            match isLong with
            | true -> (wordCount + regionIndexCount) * 2
            | false -> wordCount + regionIndexCount
        let regionIndices = Sealed.unfoldSpan span regionIndexCount 0 (RegionIndexUnfolder(at + VarDataHeaderSize))
        let rowsAt = at + VarDataHeaderSize + regionIndexCount * RegionIndexSize
        { ItemCount = itemCount
          WordCount = wordCount
          LongWords = isLong
          RegionIndices = regionIndices
          Rows = table.Slice(rowsAt, itemCount * rowSize)
          RowSize = rowSize }

    /// <summary>Unfolds the VarData records of a store, resolving each store-relative data-set offset.</summary>
    [<Struct>]
    type private VarDataUnfolder =
        val private table: ReadOnlyMemory<byte>
        val private storeAt: int

        /// <summary>Creates a VarData unfolder.</summary>
        /// <param name="table">The table bytes.</param>
        /// <param name="storeAt">The store offset.</param>
        new (table, storeAt) = { table = table; storeAt = storeAt }

        interface ISpanUnfolder<byte, int, VarData> with
            member this.Next (source, index) =
                let offsetValue = Binary.u32 source (this.storeAt + StoreHeaderSize + index * 4)
                match offsetValue with
                | 0u -> struct (emptyVarData, index + 1)
                | _ -> struct (readVarData this.table (int64 this.storeAt + int64 offsetValue), index + 1)

    /// <summary>Reads a store at an offset of its parent, after the parent sanitized.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The store offset.</param>
    /// <returns>The store, empty when its format is not 1.</returns>
    let read (table: ReadOnlyMemory<byte>) at =
        let span = table.Span
        match Binary.u16 span at = 1us with
        | false -> empty
        | true ->
            let regionsOffset = Binary.u32 span (at + 2)
            let dataCount = int (Binary.u16 span (at + 6))
            let struct (axisCount, regionCount, regions) =
                match regionsOffset with
                | 0u -> struct (0, 0, ImmutableArray<RegionAxis>.Empty)
                | _ -> readRegionList table (int64 at + int64 regionsOffset)
            let data = Sealed.unfoldSpan span dataCount 0 (VarDataUnfolder(table, at))
            { AxisCount = axisCount
              RegionCount = regionCount
              Regions = regions
              Data = data }

    /// <summary>Unfolds the region scalar of each region for one instance.</summary>
    [<Struct>]
    type private ScalarUnfolder =
        val private regions: ImmutableArray<RegionAxis>
        val private axisCount: int

        /// <summary>Creates a scalar unfolder.</summary>
        /// <param name="regions">The flattened region axes.</param>
        /// <param name="axisCount">The axis count.</param>
        new (regions, axisCount) = { regions = regions; axisCount = axisCount }

        interface ISpanUnfolder<int, int, float32> with
            member this.Next (coords, region) =
                let axes = this.regions.AsSpan().Slice(region * this.axisCount, this.axisCount)
                struct (Region.scalarFrom axes coords 0 1.0f, region + 1)

    /// <summary>Computes the region scalars of a store for coordinates, <c>create_cache</c> plus the lazy evaluation.</summary>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="store">The store.</param>
    /// <returns>The store with its scalars.</returns>
    let instantiate (coords: NormalizedCoords) (store: ItemVariationStore) : StoreInstance =
        let scalars = Sealed.unfoldSpan (coords.AsSpan()) store.RegionCount 0 (ScalarUnfolder(store.Regions, store.AxisCount))
        { Store = store; Scalars = scalars }

    /// <summary>The region count of one data set, <c>ItemVariationStore::get_region_index_count</c>; an out-of-range
    /// data set number has 0 regions.</summary>
    /// <param name="store">The store.</param>
    /// <param name="major">The data set number.</param>
    /// <returns>The region-index count.</returns>
    let regionIndexCount (store: ItemVariationStore) (major: int) : int =
        match major >= 0 && major < store.Data.Length with
        | false -> 0
        | true -> store.Data[major].RegionIndices.Length

    /// <summary>One region scalar of a data set, <c>VarData::get_region_scalars</c>'s one value; a region index past the
    /// region count reads as 0.</summary>
    /// <param name="instance">The store instance.</param>
    /// <param name="major">The data set number.</param>
    /// <param name="index">The column index within the data set.</param>
    /// <returns>The scalar.</returns>
    let regionScalar (instance: StoreInstance) (major: int) (index: int) : float32 =
        match major >= 0 && major < instance.Store.Data.Length with
        | false -> 0.0f
        | true ->
            let regions = instance.Store.Data[major].RegionIndices
            match index >= 0 && index < regions.Length with
            | false -> 0.0f
            | true ->
                let region = int regions[index]
                match region < instance.Scalars.Length with
                | true -> instance.Scalars[region]
                | false -> 0.0f

    /// <summary>Finds a delta set by a 16.16 outer.inner index, or none when the index is out of range.</summary>
    /// <param name="instance">The store instance.</param>
    /// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
    /// <returns>The VarData and its inner index, or none.</returns>
    let private dataAndInner (instance: StoreInstance) varIdx : struct (VarData * int) voption =
        let outer = int (varIdx >>> 16)
        let inner = int (varIdx &&& 0xFFFFu)
        match outer >= instance.Store.Data.Length with
        | true -> ValueNone
        | false ->
            let data = instance.Store.Data[outer]
            match inner >= data.ItemCount with
            | true -> ValueNone
            | false -> ValueSome (struct (data, inner))

    /// <summary>Computes a float32 delta, <c>ItemVariationStore::get_delta&lt;float&gt;</c>.</summary>
    /// <param name="instance">The store instance.</param>
    /// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
    /// <returns>The delta.</returns>
    let delta (instance: StoreInstance) varIdx =
        match dataAndInner instance varIdx with
        | ValueNone -> 0.0f
        | ValueSome (struct (data, inner)) ->
            match data.RegionIndices.Length with
            | 0 -> 0.0f
            | _ -> deltaRow<FloatSum, float32> (FloatSum()) data instance.Scalars (inner * data.RowSize) 0 0.0f

    /// <summary>Computes a double delta, <c>ItemVariationStore::get_delta&lt;double&gt;</c>.</summary>
    /// <param name="instance">The store instance.</param>
    /// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
    /// <returns>The delta.</returns>
    let deltaDouble (instance: StoreInstance) varIdx =
        match dataAndInner instance varIdx with
        | ValueNone -> 0.0
        | ValueSome (struct (data, inner)) ->
            match data.RegionIndices.Length with
            | 0 -> 0.0
            | _ -> deltaRow<DoubleSum, float> (DoubleSum()) data instance.Scalars (inner * data.RowSize) 0 0.0
