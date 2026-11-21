namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.Var

/// <summary>One avar segment map: the decoded (from, to) pairs of one axis, in F2DOT14 units.</summary>
type internal SegmentMap = ImmutableArray<struct (float32 * float32)>

/// <summary>The avar version 2 tail: a DeltaSetIndexMap and an ItemVariationStore, each relative to the avar table.</summary>
type internal Avar2Tail =
    { VarIdxMap: DeltaSetIndexMap
      Store: ItemVariationStore }

/// <summary>The avar table: one segment map per axis, and the version 2 tail when the major version is 2.</summary>
type internal Avar =
    { AxisCount: int
      SegmentMaps: ImmutableArray<SegmentMap>
      Version2: Avar2Tail voption }

/// <summary>The avar table of a face, <c>OT::avar</c>.</summary>
[<RequireQualifiedAccess>]
module internal Avar =
    /// <summary>The size of the avar header.</summary>
    [<Literal>]
    let private HeaderSize = 8

    /// <summary>The offset of the version word.</summary>
    [<Literal>]
    let private VersionOffset = 0

    /// <summary>The offset of the axis count.</summary>
    [<Literal>]
    let private AxisCountOffset = 6

    /// <summary>The offset of the first segment map.</summary>
    [<Literal>]
    let private FirstMapOffset = 8

    /// <summary>The size of a segment map's count word.</summary>
    [<Literal>]
    let private MapCountSize = 2

    /// <summary>The size of one AxisValueMap record.</summary>
    [<Literal>]
    let private MapRecordSize = 4

    /// <summary>The size of the version 2 tail: two Offset32 values.</summary>
    [<Literal>]
    let private TailSize = 8

    /// <summary>The offset of the DeltaSetIndexMap offset within the version 2 tail.</summary>
    [<Literal>]
    let private VarIdxMapOffset = 0

    /// <summary>The offset of the ItemVariationStore offset within the version 2 tail.</summary>
    [<Literal>]
    let private VarStoreOffset = 4

    /// <summary>The 2.14-to-16.16 delta bound, <c>1 &lt;&lt; 17</c>.</summary>
    [<Literal>]
    let private Avar2DeltaMax = 131072

    /// <summary>The 16.16 coordinate bound, <c>1 &lt;&lt; 16</c>.</summary>
    [<Literal>]
    let private Avar2CoordMax = 65536

    /// <summary>The empty avar of a face without a usable avar table.</summary>
    let empty =
        { AxisCount = 0
          SegmentMaps = ImmutableArray.Empty
          Version2 = ValueNone }

    /// <summary>Sanitizes the segment maps of an avar table, one per axis, <c>avar::sanitize</c>'s walk.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The next segment map's offset.</param>
    /// <param name="remaining">The number of segment maps left.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations and the tail offset, or none.</returns>
    [<TailCall>]
    let rec private sanitizeMaps (table: ReadOnlySpan<byte>) (offset: int) (remaining: int) (ops: int) : struct (int * int) voption =
        match remaining with
        | 0 -> ValueSome (struct (ops, offset))
        | _ ->
            match Sanitize.fits table.Length (int64 offset) MapCountSize with
            | false -> ValueNone
            | true ->
                let count = int (Binary.u16 table offset)
                match Sanitize.array table.Length (int64 (offset + MapCountSize)) (uint32 count) (uint32 MapRecordSize) ops with
                | ValueNone -> ValueNone
                | ValueSome next -> sanitizeMaps table (offset + MapCountSize + count * MapRecordSize) (remaining - 1) next

    /// <summary>Sanitizes the version 2 tail, <c>avarV2Tail::sanitize</c>: both offsets valid, or the Null object.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="tailAt">The byte offset of the tail.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeV2Tail (table: ReadOnlySpan<byte>) (tailAt: int) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 tailAt) TailSize with
        | false -> ValueNone
        | true ->
            match DeltaSetIndexMap.sanitize table (int (Binary.u32 table (tailAt + VarIdxMapOffset))) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                let varStoreOffset = int (Binary.u32 table (tailAt + VarStoreOffset))
                match varStoreOffset with
                | 0 -> ValueSome remaining
                | _ -> ItemVariationStore.sanitize table varStoreOffset remaining

    /// <summary>Sanitizes an avar table, <c>avar::sanitize</c>: version 1 or 2 and every segment map and the tail in range.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) ops =
        let major = Binary.u32 table VersionOffset >>> 16
        match Sanitize.fits table.Length 0L HeaderSize && (major = 1u || major = 2u) with
        | false -> ValueNone
        | true ->
            match sanitizeMaps table FirstMapOffset (int (Binary.u16 table AxisCountOffset)) ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (remaining, tailAt)) ->
                match major = 2u with
                | false -> ValueSome remaining
                | true -> sanitizeV2Tail table tailAt remaining

    /// <summary>The struct form of the avar sanitizer.</summary>
    [<Struct>]
    type Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Unfolds the (from, to) records of one segment map, returning the next record index.</summary>
    [<Struct>]
    type private SegmentMapRecordUnfolder =
        val private records: int

        /// <summary>Creates a segment-map record unfolder.</summary>
        /// <param name="records">The byte offset of the first record.</param>
        new (records) = { records = records }

        interface ISpanUnfolder<byte, int, struct (float32 * float32)> with
            member this.Next (source, index) =
                let fromCoord = Fixed.f2dot14 (Binary.i16 source (this.records + index * MapRecordSize))
                let toCoord = Fixed.f2dot14 (Binary.i16 source (this.records + index * MapRecordSize + 2))
                struct (struct (fromCoord, toCoord), index + 1)

    /// <summary>Unfolds one segment map at a byte offset, returning the next offset.</summary>
    [<Struct>]
    type private SegmentMapUnfolder =
        interface ISpanUnfolder<byte, int, SegmentMap> with
            member _.Next (source, offset) =
                let count = int (Binary.u16 source offset)
                let records = offset + MapCountSize
                let map = Sealed.unfoldSpan source count 0 (SegmentMapRecordUnfolder(records))
                struct (map, records + count * MapRecordSize)

    /// <summary>Walks the segment maps to the version 2 tail, <c>map_coords_16_16</c>'s tail-finding walk.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The next segment map's offset.</param>
    /// <param name="remaining">The number of segment maps left.</param>
    /// <returns>The tail offset.</returns>
    [<TailCall>]
    let rec private tailOffset (table: ReadOnlySpan<byte>) (offset: int) (remaining: int) : int =
        match remaining with
        | 0 -> offset
        | _ ->
            let count = int (Binary.u16 table offset)
            tailOffset table (offset + MapCountSize + count * MapRecordSize) (remaining - 1)

    /// <summary>Reads the version 2 store, the Null store on a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The store offset.</param>
    /// <returns>The store.</returns>
    let private readStore (table: ReadOnlyMemory<byte>) (at: int) : ItemVariationStore =
        match at with
        | 0 -> ItemVariationStore.empty
        | _ -> ItemVariationStore.read table at

    /// <summary>Loads the avar table, empty on sanitize failure, <c>avar</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed avar, or empty.</returns>
    let load face =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Avar)
        let axisCount = int (Binary.u16 table.Span AxisCountOffset)
        let segmentMaps = Sealed.unfoldSpan table.Span axisCount FirstMapOffset (SegmentMapUnfolder())
        let version2 =
            match Binary.u32 table.Span VersionOffset >>> 16 with
            | 2u ->
                let tailAt = tailOffset table.Span FirstMapOffset axisCount
                ValueSome
                    { VarIdxMap = DeltaSetIndexMap.read table (int (Binary.u32 table.Span (tailAt + VarIdxMapOffset)))
                      Store = readStore table (int (Binary.u32 table.Span (tailAt + VarStoreOffset))) }
            | _ -> ValueNone
        { AxisCount = axisCount
          SegmentMaps = segmentMaps
          Version2 = version2 }

    /// <summary>Finds the last index of an exact-match run, <c>map_float</c>'s second scan.</summary>
    /// <param name="map">The segment map.</param>
    /// <param name="value">The coordinate.</param>
    /// <param name="index">The first exact match.</param>
    /// <param name="finish">The exclusive end of the active map.</param>
    /// <returns>The last exact match.</returns>
    let private runEnd (map: SegmentMap) (value: float32) (index: int) (finish: int) =
        match Spans.tryFindIndex (fun (struct (fromCoord, _)) -> value <> fromCoord) (map.AsSpan().Slice(index + 1, finish - index - 1)) with
        | ValueSome relative -> index + relative
        | ValueNone -> finish - 1

    /// <summary>Interpolates or shifts between segments, <c>map_float</c> when there is no exact match.</summary>
    /// <param name="map">The segment map.</param>
    /// <param name="value">The coordinate.</param>
    /// <param name="start">The first active index.</param>
    /// <param name="finish">The exclusive end of the active map.</param>
    /// <returns>The mapped coordinate.</returns>
    let private mapInterpolate (map: SegmentMap) (value: float32) (start: int) (finish: int) =
        match Spans.tryFindIndex (fun (struct (fromCoord, _)) -> value < fromCoord) (map.AsSpan().Slice(start, finish - start)) with
        | ValueSome relative ->
            let index = start + relative
            match index = start with
            | true ->
                let struct (fromCoord, toCoord) = map[start]
                value - fromCoord + toCoord
            | false ->
                let struct (beforeFrom, beforeTo) = map[index - 1]
                let struct (afterFrom, afterTo) = map[index]
                let denominator = afterFrom - beforeFrom
                beforeTo + (afterTo - beforeTo) * (value - beforeFrom) / denominator
        | ValueNone ->
            let struct (fromCoord, toCoord) = map[finish - 1]
            value - fromCoord + toCoord

    /// <summary>Maps a coordinate through a trimmed segment map, <c>map_float</c> for two or more records.</summary>
    /// <param name="map">The segment map.</param>
    /// <param name="value">The coordinate.</param>
    /// <param name="start">The first active index.</param>
    /// <param name="finish">The exclusive end of the active map.</param>
    /// <returns>The mapped coordinate.</returns>
    let private mapTrimmed (map: SegmentMap) (value: float32) (start: int) (finish: int) =
        match Spans.tryFindIndex (fun (struct (fromCoord, _)) -> value = fromCoord) (map.AsSpan().Slice(start, finish - start)) with
        | ValueSome relative ->
            let index = start + relative
            let last = runEnd map value index finish
            match struct (index = last, index + 2 = last, value) with
            | struct (true, _, _) ->
                let struct (_, toCoord) = map[index]
                toCoord
            | struct (false, true, _) ->
                let struct (_, toCoord) = map[index + 1]
                toCoord
            | struct (false, false, v) when v < 0.0f ->
                let struct (_, toCoord) = map[last]
                toCoord
            | struct (false, false, v) when v > 0.0f ->
                let struct (_, toCoord) = map[index]
                toCoord
            | struct (false, false, _) ->
                let struct (_, a) = map[index]
                let struct (_, b) = map[last]
                match MathF.Abs a < MathF.Abs b with
                | true -> a
                | false -> b
        | ValueNone -> mapInterpolate map value start finish

    /// <summary>Maps a coordinate through one segment map, <c>SegmentMaps::map_float</c> with all its special cases.</summary>
    /// <param name="map">The segment map.</param>
    /// <param name="value">The normalized coordinate.</param>
    /// <returns>The mapped coordinate.</returns>
    let mapFloat (map: SegmentMap) (value: float32) =
        match map.Length with
        | 0 -> value
        | 1 ->
            let struct (fromCoord, toCoord) = map[0]
            value - fromCoord + toCoord
        | _ ->
            let struct (firstFrom, firstTo) = map[0]
            let struct (secondFrom, _) = map[1]
            let struct (lastFrom, lastTo) = map[map.Length - 1]
            let struct (penultFrom, _) = map[map.Length - 2]
            let start =
                match firstFrom = -1.0f && firstTo = -1.0f && secondFrom = -1.0f with
                | true -> 1
                | false -> 0
            let finish =
                match lastFrom = 1.0f && lastTo = 1.0f && penultFrom = 1.0f with
                | true -> map.Length - 1
                | false -> map.Length
            mapTrimmed map value start finish

    /// <summary>Applies the version 2 delta rows, <c>map_coords_16_16</c> after the v1 mapping.</summary>
    /// <param name="tail">The version 2 tail.</param>
    /// <param name="coords">The v1-mapped 16.16 coordinates.</param>
    /// <returns>The coordinates with the avar 2 deltas applied.</returns>
    let private mapV2 (tail: Avar2Tail) (coords: ImmutableArray<int>) =
        let coords2_14 = Sealed.init coords.Length (fun index -> int (Fixed.roundf (float32 coords[index] / 4.0f)))
        let instance = ItemVariationStore.instantiate (NormalizedCoords.ofSealed coords2_14) tail.Store
        Sealed.init coords.Length (fun index ->
            let varIdx = DeltaSetIndexMap.map tail.VarIdxMap (uint32 index)
            let delta = ItemVariationStore.delta instance varIdx
            let scaled = Fixed.hbClamp (delta * 4.0f) (float32 -Avar2DeltaMax) (float32 Avar2DeltaMax)
            let value = coords[index] + int (Fixed.roundf scaled)
            min (max value (-Avar2CoordMax)) Avar2CoordMax)

    /// <summary>Maps coordinates through every segment map, <c>avar::map_coords_16_16</c> (versions 1 and 2).</summary>
    /// <param name="avar">The avar table.</param>
    /// <param name="coords">The 16.16 coordinates.</param>
    /// <returns>The avar-mapped 16.16 coordinates.</returns>
    let mapCoords16Dot16 (avar: Avar) (coords: ImmutableArray<int>) =
        let count = min coords.Length avar.AxisCount
        let v1 =
            Sealed.init coords.Length (fun index ->
                match index < count with
                | true ->
                    let mapped = mapFloat avar.SegmentMaps[index] (float32 coords[index] / 65536.0f)
                    int (Fixed.roundf (mapped * 65536.0f))
                | false -> coords[index])
        match avar.Version2 with
        | ValueNone -> v1
        | ValueSome tail -> mapV2 tail v1
