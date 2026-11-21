namespace Farfbuzz.Var

open System
open Farfbuzz

/// <summary>A DeltaSetIndexMap, format 0 or 1, mapping indices to 16.16 outer.inner delta-set indices.</summary>
[<NoEquality; NoComparison>]
type internal DeltaSetIndexMap =
    { MapCount: uint32
      Width: int
      InnerBitCount: int
      Data: ReadOnlyMemory<byte> }

/// <summary>DeltaSetIndexMap formats 0 and 1, <c>OT::DeltaSetIndexMap</c>.</summary>
[<RequireQualifiedAccess>]
module internal DeltaSetIndexMap =
    /// <summary>The header size of format 0: format, entryFormat and a uint16 map count.</summary>
    [<Literal>]
    let private Format0HeaderSize = 4

    /// <summary>The header size of format 1: format, entryFormat and a uint32 map count.</summary>
    [<Literal>]
    let private Format1HeaderSize = 6

    /// <summary>The shift of the width field in <c>entryFormat</c>.</summary>
    [<Literal>]
    let private EntryFormatWidthShift = 4

    /// <summary>The mask of the inner-bit-count field in <c>entryFormat</c>.</summary>
    [<Literal>]
    let private EntryFormatInnerMask = 0xF

    /// <summary>The identity mapping of a zero map count, <c>DeltaSetIndexMap</c>'s direct mapping.</summary>
    let identity =
        { MapCount = 0u
          Width = 0
          InnerBitCount = 0
          Data = ReadOnlyMemory<byte>.Empty }

    /// <summary>Reads <paramref name="remaining"/> bytes big-endian into an unsigned word, as <c>_map</c> fetches.</summary>
    /// <param name="data">The map data.</param>
    /// <param name="at">The next byte.</param>
    /// <param name="remaining">The number of bytes left.</param>
    /// <param name="acc">The value so far.</param>
    /// <returns>The fetched word.</returns>
    [<TailCall>]
    let rec private fetchWidth (data: ReadOnlySpan<byte>) (at: int) (remaining: int) (acc: uint32) : uint32 =
        match remaining = 0 with
        | true -> acc
        | false -> fetchWidth data (at + 1) (remaining - 1) ((acc <<< 8) + uint32 (Binary.u8 data at))

    /// <summary>The width in bytes from an <c>entryFormat</c> byte, <c>get_width</c>.</summary>
    /// <param name="entryFormat">The packed entry format.</param>
    /// <returns>The width, 1 to 4.</returns>
    let private widthOf (entryFormat: byte) =
        ((int entryFormat >>> EntryFormatWidthShift) &&& 3) + 1

    /// <summary>The inner bit count from an <c>entryFormat</c> byte, <c>get_inner_bit_count</c>.</summary>
    /// <param name="entryFormat">The packed entry format.</param>
    /// <returns>The inner bit count, 1 to 16.</returns>
    let private innerBitCountOf (entryFormat: byte) =
        (int entryFormat &&& EntryFormatInnerMask) + 1

    /// <summary>Sanitizes a DeltaSetIndexMap at an offset of its parent, <c>DeltaSetIndexMap::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The offset of the map, or 0 for the Null object.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) at ops =
        match at with
        | 0 -> ValueSome ops
        | _ ->
            match Binary.u8 table at with
            | 0uy ->
                match Sanitize.fits table.Length (int64 at) Format0HeaderSize with
                | false -> ValueNone
                | true ->
                    let mapCount = Binary.u16 table (at + 2)
                    Sanitize.array table.Length (int64 (at + Format0HeaderSize)) (uint32 mapCount) (uint32 (widthOf (Binary.u8 table (at + 1)))) ops
            | 1uy ->
                match Sanitize.fits table.Length (int64 at) Format1HeaderSize with
                | false -> ValueNone
                | true ->
                    let mapCount = Binary.u32 table (at + 2)
                    Sanitize.array table.Length (int64 (at + Format1HeaderSize)) mapCount (uint32 (widthOf (Binary.u8 table (at + 1)))) ops
            | _ -> ValueSome ops

    /// <summary>Reads a DeltaSetIndexMap at an offset of its parent, after the parent sanitized.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The offset of the map, or 0 for the Null object.</param>
    /// <returns>The map, identity on an unknown format.</returns>
    let read (table: ReadOnlyMemory<byte>) at =
        match at with
        | 0 -> identity
        | _ ->
            let span = table.Span
            match Binary.u8 span at with
            | 0uy ->
                let entryFormat = Binary.u8 span (at + 1)
                let mapCount = Binary.u16 span (at + 2)
                { MapCount = uint32 mapCount
                  Width = widthOf entryFormat
                  InnerBitCount = innerBitCountOf entryFormat
                  Data = table.Slice(at + Format0HeaderSize, widthOf entryFormat * int mapCount) }
            | 1uy ->
                let entryFormat = Binary.u8 span (at + 1)
                let mapCount = Binary.u32 span (at + 2)
                { MapCount = mapCount
                  Width = widthOf entryFormat
                  InnerBitCount = innerBitCountOf entryFormat
                  Data = table.Slice(at + Format1HeaderSize, widthOf entryFormat * int mapCount) }
            | _ -> identity

    /// <summary>Maps an index through the map, <c>DeltaSetIndexMap::map</c>, returning a 16.16 outer.inner value.</summary>
    /// <param name="mapping">The map.</param>
    /// <param name="value">The index.</param>
    /// <returns>The mapped delta-set index, or the input when the map is identity.</returns>
    let map (mapping: DeltaSetIndexMap) (value: uint32) =
        match mapping.MapCount with
        | 0u -> value
        | _ ->
            let clamped =
                match value >= mapping.MapCount with
                | true -> mapping.MapCount - 1u
                | false -> value
            let at = mapping.Width * int clamped
            let fetched = fetchWidth (mapping.Data.Span) at mapping.Width 0u
            let outer = fetched >>> mapping.InnerBitCount
            let inner = fetched &&& ((1u <<< mapping.InnerBitCount) - 1u)
            (outer <<< 16) ||| inner
