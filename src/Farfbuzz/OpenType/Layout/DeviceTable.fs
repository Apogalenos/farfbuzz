namespace Farfbuzz

open System

/// <summary>A Device table: hinting adjustments, a variation index, or no device at all.</summary>
[<Struct>]
type internal Device =
    | NoDevice
    | HintingDevice of startSize: uint16 * endSize: uint16 * deltaFormat: uint16 * deltas: ReadOnlyMemory<byte>
    | VariationIndex of outer: uint16 * inner: uint16

/// <summary>The decoding of hinting and variation Device tables, <c>Device</c> of <c>src/hb-ot-layout-common.hh</c>, which needs no font.</summary>
[<RequireQualifiedAccess>]
module internal DeviceTable =
    /// <summary>The variation-device format identifier, <c>0x8000</c>.</summary>
    [<Literal>]
    let private VariationFormat = 0x8000us

    /// <summary>The size of the Device header, three uint16 values.</summary>
    [<Literal>]
    let private HeaderSize = 6

    /// <summary>The size in bytes of the delta words of a hinting device, <c>HintingDevice::get_size</c> without its header.</summary>
    /// <param name="startSize">The smallest size to correct.</param>
    /// <param name="endSize">The largest size to correct.</param>
    /// <param name="format">The delta format, one to three.</param>
    /// <returns>The byte count; zero for a device whose format or range is invalid.</returns>
    let private deltaBytes (startSize: uint16) (endSize: uint16) (format: uint16) =
        match format >= 1us && format <= 3us && startSize <= endSize with
        | false -> 0
        | true -> 2 * (1 + ((int endSize - int startSize) >>> (4 - int format)))

    /// <summary>Copies the delta array of a hinting device into an immutable byte slice.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The device offset.</param>
    /// <param name="startSize">The smallest size to correct.</param>
    /// <param name="endSize">The largest size to correct.</param>
    /// <param name="format">The delta format, one to three.</param>
    /// <returns>The delta bytes, empty when they do not fit in the table.</returns>
    let private deltasOf (table: ReadOnlySpan<byte>) (offset: int) (startSize: uint16) (endSize: uint16) (format: uint16) =
        let byteCount = deltaBytes startSize endSize format
        match Sanitize.fits table.Length (int64 offset + int64 HeaderSize) byteCount with
        | false -> ReadOnlyMemory<byte>.Empty
        | true -> (Sealed.ofSpan (table.Slice(offset + HeaderSize, byteCount))).AsMemory()

    /// <summary>Decodes a Device table, <c>Device</c> without its sanitize step.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The device offset.</param>
    /// <returns>The device; an unknown format gives the null device.</returns>
    let parse (table: ReadOnlySpan<byte>) (offset: int) =
        let first = Binary.u16 table offset
        let second = Binary.u16 table (offset + 2)
        let format = Binary.u16 table (offset + 4)
        match format with
        | VariationFormat -> VariationIndex (first, second)
        | f when f >= 1us && f <= 3us -> HintingDevice (first, second, f, deltasOf table offset first second f)
        | _ -> NoDevice

    /// <summary>Decodes a Device table as <c>Device::sanitize</c> accepts it: a device whose header or delta words do not
    /// fit in the table is the Null device, and so is a device of an unknown format, whose deltas are zero.</summary>
    /// <param name="table">The bytes of the whole table the offset points into.</param>
    /// <param name="offset">The device offset.</param>
    /// <returns>The device, or the null device when it fails its check.</returns>
    let parseSanitized (table: ReadOnlySpan<byte>) (offset: int) =
        match Sanitize.fits table.Length (int64 offset) HeaderSize with
        | false -> NoDevice
        | true ->
            match parse table offset with
            | HintingDevice (startSize, endSize, format, deltas) when deltas.IsEmpty && deltaBytes startSize endSize format > 0 -> NoDevice
            | device -> device

    /// <summary>Reads one signed delta at a given ppem, <c>HintingDevice::get_delta_pixels</c>.</summary>
    /// <param name="ppem">The pixels per em.</param>
    /// <param name="startSize">The smallest size to correct.</param>
    /// <param name="endSize">The largest size to correct.</param>
    /// <param name="format">The delta format.</param>
    /// <param name="deltas">The delta bytes.</param>
    /// <returns>The delta, in pixels.</returns>
    let private deltaPixels (ppem: uint32) (startSize: uint16) (endSize: uint16) (format: uint16) (deltas: ReadOnlyMemory<byte>) =
        let f = int format
        match f >= 1 && f <= 3 && ppem >= uint32 startSize && ppem <= uint32 endSize with
        | false -> 0
        | true ->
            let s = int ppem - int startSize
            let valueBits = 1 <<< f
            let valuesPerWord = 1 <<< (4 - f)
            let word = int (Binary.u16 deltas.Span ((s >>> (4 - f)) * 2))
            let shift = 16 - ((s &&& (valuesPerWord - 1)) + 1) * valueBits
            let mask = 0xFFFF >>> (16 - valueBits)
            let delta = (word >>> shift) &&& mask
            match delta >= ((mask + 1) >>> 1) with
            | true -> delta - mask - 1
            | false -> delta

    /// <summary>Scales a pixel delta to user space, <c>HintingDevice::get_delta</c>.</summary>
    /// <param name="ppem">The pixels per em.</param>
    /// <param name="scale">The scale.</param>
    /// <param name="startSize">The smallest size to correct.</param>
    /// <param name="endSize">The largest size to correct.</param>
    /// <param name="format">The delta format.</param>
    /// <param name="deltas">The delta bytes.</param>
    /// <returns>The scaled delta, zero when ppem is zero.</returns>
    let hintingDelta (ppem: uint32) (scale: int) (startSize: uint16) (endSize: uint16) (format: uint16) (deltas: ReadOnlyMemory<byte>) =
        match ppem with
        | 0u -> 0
        | _ ->
            let pixels = deltaPixels ppem startSize endSize format deltas
            match pixels with
            | 0 -> 0
            | _ -> int (int64 pixels * int64 scale / int64 ppem)
