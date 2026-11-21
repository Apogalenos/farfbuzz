namespace Farfbuzz.OpenType

open System
open Farfbuzz
open Farfbuzz.Var

/// <summary>The sanitized trak table, holding its bytes for the tracking reads.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Trak =
    { Table: ReadOnlyMemory<byte>
      Version: uint32 }

/// <summary>Reading the trak tracking table, <c>AAT::trak</c> (src/hb-aat-layout-trak-table.hh).</summary>
[<RequireQualifiedAccess>]
module internal Trak =
    /// <summary>The size of the trak header.</summary>
    [<Literal>]
    let private HeaderSize = 12

    /// <summary>The size of one TrackData header.</summary>
    [<Literal>]
    let private TrackDataSize = 8

    /// <summary>The size of one TrackTableEntry record.</summary>
    [<Literal>]
    let private EntrySize = 8

    /// <summary>The size of one F16DOT16 size value.</summary>
    [<Literal>]
    let private SizeValueSize = 4

    /// <summary>The size of one FWORD track value.</summary>
    [<Literal>]
    let private TrackValueSize = 2

    /// <summary>The byte offset of the horizontal TrackData offset in the header.</summary>
    [<Literal>]
    let private HorizDataOffset = 6

    /// <summary>The byte offset of the vertical TrackData offset in the header.</summary>
    [<Literal>]
    let private VertDataOffset = 8

    /// <summary>The byte offset of the value-array offset in a TrackTableEntry.</summary>
    [<Literal>]
    let private EntryValuesOffset = 6

    /// <summary>The empty trak table, returned when the table is absent or fails to sanitize.</summary>
    let private empty : Trak = { Table = ReadOnlyMemory<byte>.Empty; Version = 0u }

    /// <summary>Sanitizes the value arrays of a TrackData's entries, <c>TrackTableEntry::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The next entry offset.</param>
    /// <param name="count">The entries left.</param>
    /// <param name="nSizes">The number of size values.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when a value array fails.</returns>
    [<TailCall>]
    let rec private sanitizeEntries (table: ReadOnlySpan<byte>) (at: int) (count: int) (nSizes: int) (ops: int) : int voption =
        match count <= 0 with
        | true -> ValueSome ops
        | false ->
            let valuesOffset = int64 (Binary.u16 table (at + EntryValuesOffset))
            match Sanitize.array table.Length valuesOffset (uint32 nSizes) (uint32 TrackValueSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeEntries table (at + EntrySize) (count - 1) nSizes remaining

    /// <summary>Sanitizes the track table and its entries, <c>TrackData::sanitize</c>'s trackTable part.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The track-table offset.</param>
    /// <param name="nTracks">The track count.</param>
    /// <param name="nSizes">The number of size values.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeTrackTable (table: ReadOnlySpan<byte>) (at: int) (nTracks: int) (nSizes: int) (ops: int) : int voption =
        match Sanitize.array table.Length (int64 at) (uint32 nTracks) (uint32 EntrySize) ops with
        | ValueNone -> ValueNone
        | ValueSome remaining -> sanitizeEntries table at nTracks nSizes remaining

    /// <summary>Sanitizes one TrackData, <c>TrackData::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The TrackData offset; zero is the Null object.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeTrackData (table: ReadOnlySpan<byte>) (at: int64) (ops: int) : int voption =
        match at = 0L with
        | true -> ValueSome ops
        | false ->
            match Sanitize.fits table.Length at TrackDataSize with
            | false -> ValueNone
            | true ->
                let nTracks = int (Binary.u16 table (int at))
                let nSizes = int (Binary.u16 table (int at + 2))
                let sizeTableOffset = int64 (Binary.u32 table (int at + 4))
                match Sanitize.array table.Length sizeTableOffset (uint32 nSizes) (uint32 SizeValueSize) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeTrackTable table (int at + TrackDataSize) nTracks nSizes remaining

    /// <summary>Sanitizes a trak table, <c>trak::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        match Sanitize.fits table.Length 0 HeaderSize with
        | false -> ValueNone
        | true ->
            match Binary.u16 table 0 with
            | 1us ->
                match sanitizeTrackData table (int64 (Binary.u16 table HorizDataOffset)) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeTrackData table (int64 (Binary.u16 table VertDataOffset)) remaining
            | _ -> ValueNone

    /// <summary>The struct form of the trak sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Loads and sanitizes the trak table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The table, empty when absent or failing.</returns>
    let load (face: Face) : Trak =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Trak)
        match table.Length with
        | 0 -> empty
        | _ -> { Table = table; Version = Binary.u32 table.Span 0 }

    /// <summary>Whether the table has data, <c>trak::has_data</c>.</summary>
    /// <param name="trak">The table.</param>
    /// <returns>True when the version is not zero.</returns>
    let hasData (trak: Trak) = trak.Version <> 0u

    /// <summary>Decodes the F16DOT16 track value of an entry, <c>TrackTableEntry::get_track_value</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="entryAt">The entry offset.</param>
    /// <returns>The float value.</returns>
    let internal trackValue (table: ReadOnlySpan<byte>) (entryAt: int) : float32 =
        Fixed.f16dot16 (int (Binary.u32 table entryAt))

    /// <summary>Interpolates two adjacent size values, <c>TrackTableEntry::interpolate_at</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="entryAt">The entry offset.</param>
    /// <param name="idx">The lower size index.</param>
    /// <param name="sizeTableAt">The F16DOT16 size-table offset.</param>
    /// <param name="ptem">The point size.</param>
    /// <returns>The interpolated value.</returns>
    let internal interpolateAt (table: ReadOnlyMemory<byte>) (entryAt: int) (idx: int) (sizeTableAt: int) (ptem: float32) : float32 =
        let span = table.Span
        let valuesAt = int (Binary.u16 span (entryAt + EntryValuesOffset))
        let s0 = Fixed.f16dot16 (int (Binary.u32 span (sizeTableAt + idx * SizeValueSize)))
        let s1 = Fixed.f16dot16 (int (Binary.u32 span (sizeTableAt + (idx + 1) * SizeValueSize)))
        let v0 = int (Binary.i16 span (valuesAt + idx * TrackValueSize))
        let v1 = int (Binary.i16 span (valuesAt + (idx + 1) * TrackValueSize))
        let s0, s1, v0, v1 =
            match s1 < s0 with
            | true -> s1, s0, v1, v0
            | false -> s0, s1, v0, v1
        match ptem < s0, ptem > s1, s0 = s1 with
        | true, _, _ -> float32 v0
        | false, true, _ -> float32 v1
        | false, false, true -> float32 (v0 + v1) * 0.5f
        | false, false, false ->
            let t = (ptem - s0) / (s1 - s0)
            float32 v0 + t * float32 (v1 - v0)

    /// <summary>Finds the first size not smaller than ptem, <c>get_value</c>'s scan.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="sizeTableAt">The F16DOT16 size-table offset.</param>
    /// <param name="nSizes">The number of sizes.</param>
    /// <param name="ptem">The point size.</param>
    /// <param name="index">The next size index.</param>
    /// <returns>The first index whose size is at least ptem, or the count.</returns>
    [<TailCall>]
    let rec private findSizeIndex (table: ReadOnlySpan<byte>) (sizeTableAt: int) (nSizes: int) (ptem: float32) (index: int) : int =
        match index >= nSizes || Fixed.f16dot16 (int (Binary.u32 table (sizeTableAt + index * SizeValueSize))) >= ptem with
        | true -> index
        | false -> findSizeIndex table sizeTableAt nSizes ptem (index + 1)

    /// <summary>Chooses and interpolates a size value, <c>TrackTableEntry::get_value</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="entryAt">The entry offset.</param>
    /// <param name="nSizes">The number of sizes.</param>
    /// <param name="sizeTableAt">The F16DOT16 size-table offset.</param>
    /// <param name="ptem">The point size.</param>
    /// <returns>The value.</returns>
    let internal getValue (table: ReadOnlyMemory<byte>) (entryAt: int) (nSizes: int) (sizeTableAt: int) (ptem: float32) : float32 =
        let span = table.Span
        let valuesAt = int (Binary.u16 span (entryAt + EntryValuesOffset))
        match nSizes with
        | 0 -> 0f
        | 1 -> float32 (Binary.i16 span valuesAt)
        | _ ->
            let index = findSizeIndex span sizeTableAt nSizes ptem 0
            match index with
            | 0 -> float32 (Binary.i16 span valuesAt)
            | index when index = nSizes -> float32 (Binary.i16 span (valuesAt + (nSizes - 1) * TrackValueSize))
            | index ->
                match Fixed.f16dot16 (int (Binary.u32 span (sizeTableAt + index * SizeValueSize))) = ptem with
                | true -> float32 (Binary.i16 span (valuesAt + index * TrackValueSize))
                | false -> interpolateAt table entryAt (index - 1) sizeTableAt ptem

    /// <summary>Finds the largest track index whose track value is at most the requested track,
    /// <c>TrackData::get_tracking</c>'s first scan.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="dataAt">The TrackData offset.</param>
    /// <param name="count">The track count.</param>
    /// <param name="track">The requested track.</param>
    /// <param name="index">The current index.</param>
    /// <returns>The bracketing index.</returns>
    [<TailCall>]
    let rec private lowerTrackIndex (table: ReadOnlySpan<byte>) (dataAt: int) (count: int) (track: float32) (index: int) : int =
        match index + 1 < count
              && trackValue table (dataAt + TrackDataSize + (index + 1) * EntrySize) <= track with
        | true -> lowerTrackIndex table dataAt count track (index + 1)
        | false -> index

    /// <summary>Finds the smallest track index whose track value is at least the requested track,
    /// <c>TrackData::get_tracking</c>'s second scan.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="dataAt">The TrackData offset.</param>
    /// <param name="track">The requested track.</param>
    /// <param name="index">The current index.</param>
    /// <returns>The bracketing index.</returns>
    [<TailCall>]
    let rec private upperTrackIndex (table: ReadOnlySpan<byte>) (dataAt: int) (track: float32) (index: int) : int =
        match index > 0 && trackValue table (dataAt + TrackDataSize + (index - 1) * EntrySize) >= track with
        | true -> upperTrackIndex table dataAt track (index - 1)
        | false -> index

    /// <summary>Computes one TrackData's tracking value, <c>TrackData::get_tracking</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="dataAt">The TrackData offset.</param>
    /// <param name="ptem">The point size.</param>
    /// <param name="track">The requested track.</param>
    /// <returns>The design-space tracking value.</returns>
    let internal trackDataValue (table: ReadOnlyMemory<byte>) (dataAt: int) (ptem: float32) (track: float32) : float32 =
        let span = table.Span
        let nTracks = int (Binary.u16 span dataAt)
        let nSizes = int (Binary.u16 span (dataAt + 2))
        let sizeTableAt = int (Binary.u32 span (dataAt + 4))
        let entryAt index = dataAt + TrackDataSize + index * EntrySize
        match nTracks with
        | 0 -> 0f
        | 1 -> getValue table (entryAt 0) nSizes sizeTableAt ptem
        | _ ->
            let low = lowerTrackIndex span dataAt nTracks track 0
            let high = upperTrackIndex span dataAt track (nTracks - 1)
            match low = high with
            | true -> getValue table (entryAt low) nSizes sizeTableAt ptem
            | false ->
                let t0 = trackValue span (entryAt low)
                let t1 = trackValue span (entryAt high)
                let a = getValue table (entryAt low) nSizes sizeTableAt ptem
                let b = getValue table (entryAt high) nSizes sizeTableAt ptem
                match t0 = t1 with
                | true -> (a + b) * 0.5f
                | false ->
                    let t = (track - t0) / (t1 - t0)
                    a + t * (b - a)

    /// <summary>Returns the design-space tracking value for a direction, <c>TrackData::get_tracking</c>
    /// reached through <c>trak::get_h_tracking</c> or <c>get_v_tracking</c>.</summary>
    /// <param name="trak">The table.</param>
    /// <param name="horizontal">True for the horizontal TrackData.</param>
    /// <param name="ptem">The point size, already defaulted to at least 12.</param>
    /// <param name="track">The requested track, zero by default.</param>
    /// <returns>The design-space tracking value.</returns>
    let getTracking (trak: Trak) (horizontal: bool) (ptem: float32) (track: float32) : float32 =
        let span = trak.Table.Span
        let dataAt = int (Binary.u16 span (if horizontal then HorizDataOffset else VertDataOffset))
        match dataAt with
        | 0 -> 0f
        | dataAt -> trackDataValue trak.Table dataAt ptem track
