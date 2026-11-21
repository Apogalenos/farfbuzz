namespace Farfbuzz.Var

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>A tuple variation header: the serialized data size, the packed tuple index and the header's byte offset in the data.</summary>
[<Struct; NoComparison; NoEquality>]
type internal TupleHeader =
    { DataSize: uint16
      TupleIndex: uint16
      At: int }

/// <summary>Tuple variation headers, packed point numbers and packed deltas, <c>OT::TupleVariationHeader</c> and <c>OT::TupleValues</c>.</summary>
[<RequireQualifiedAccess>]
module internal TupleVariation =
    /// <summary>The embedded-peak flag of a tuple index, <c>TupleIndex::EmbeddedPeakTuple</c>.</summary>
    [<Literal>]
    let private EmbeddedPeakFlag = 0x8000us

    /// <summary>The intermediate-region flag of a tuple index, <c>TupleIndex::IntermediateRegion</c>.</summary>
    [<Literal>]
    let private IntermediateRegionFlag = 0x4000us

    /// <summary>The private-point-numbers flag of a tuple index, <c>TupleIndex::PrivatePointNumbers</c>.</summary>
    [<Literal>]
    let private PrivatePointsFlag = 0x2000us

    /// <summary>The shared-tuple index mask of a tuple index, <c>TupleIndex::TupleIndexMask</c>.</summary>
    [<Literal>]
    let private TupleIndexMask = 0x0FFFus

    /// <summary>The words bit of a packed point run, <c>POINTS_ARE_WORDS</c>.</summary>
    [<Literal>]
    let private PointsAreWords = 0x80uy

    /// <summary>The count mask of a packed point run, <c>POINT_RUN_COUNT_MASK</c>.</summary>
    [<Literal>]
    let private PointRunCountMask = 0x7Fuy

    /// <summary>The zeros kind of a packed value run, <c>VALUES_ARE_ZEROS</c>.</summary>
    [<Literal>]
    let private ValuesAreZeros = 0x80uy

    /// <summary>The words kind of a packed value run, <c>VALUES_ARE_WORDS</c>.</summary>
    [<Literal>]
    let private ValuesAreWords = 0x40uy

    /// <summary>The count mask of a packed value run, <c>VALUE_RUN_COUNT_MASK</c>.</summary>
    [<Literal>]
    let private ValueRunCountMask = 0x3Fuy

    /// <summary>Whether the tuple index carries an embedded peak tuple, <c>TupleIndex::EmbeddedPeakTuple</c>.</summary>
    /// <param name="header">The tuple header.</param>
    /// <returns>True when the 0x8000 bit is set.</returns>
    let inline (|EmbeddedPeak|_|) (header: TupleHeader) =
        header.TupleIndex &&& EmbeddedPeakFlag <> 0us

    /// <summary>Whether the tuple index carries an intermediate region, <c>TupleIndex::IntermediateRegion</c>.</summary>
    /// <param name="header">The tuple header.</param>
    /// <returns>True when the 0x4000 bit is set.</returns>
    let inline (|IntermediateRegion|_|) (header: TupleHeader) =
        header.TupleIndex &&& IntermediateRegionFlag <> 0us

    /// <summary>Whether the tuple index carries private point numbers, <c>TupleIndex::PrivatePointNumbers</c>.</summary>
    /// <param name="header">The tuple header.</param>
    /// <returns>True when the 0x2000 bit is set.</returns>
    let inline (|PrivatePoints|_|) (header: TupleHeader) =
        header.TupleIndex &&& PrivatePointsFlag <> 0us

    /// <summary>What one axis contributes to a tuple scalar, <c>TupleVariationHeader::calculate_scalar</c>.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private TupleFactor =
        | Keep // factor 1: peak 0, the coordinate at the peak, or an invalid intermediate triple
        | Zero // factor 0: the whole tuple scalar is 0
        | Scale of numerator: int * denominator: int // on a slope: float32 numerator / float32 denominator

    /// <summary>Picks the slope of an intermediate axis whose coordinate lies between its start and end.</summary>
    /// <param name="peak">The peak.</param>
    /// <param name="v">The coordinate.</param>
    /// <param name="start">The intermediate start.</param>
    /// <param name="finish">The intermediate end.</param>
    /// <returns>The slope, or <see cref="TupleFactor.Keep"/> when the denominator is zero.</returns>
    let private slopeFactor peak v start finish =
        match v < peak with
        | true ->
            match peak <> start with
            | true -> Scale (v - start, peak - start)
            | false -> Keep
        | false ->
            match peak <> finish with
            | true -> Scale (finish - v, finish - peak)
            | false -> Keep

    /// <summary>Classifies an axis with an intermediate region, after the peak and coordinate checks.</summary>
    /// <param name="peak">The peak, already non-zero.</param>
    /// <param name="v">The coordinate, already non-zero and not equal to the peak.</param>
    /// <param name="start">The intermediate start.</param>
    /// <param name="finish">The intermediate end.</param>
    /// <returns>The factor.</returns>
    let private intermFactor peak v start finish =
        match start > peak || peak > finish || (start < 0 && finish > 0) with
        | true -> Keep
        | false ->
            match v < start || v > finish with
            | true -> Zero
            | false -> slopeFactor peak v start finish

    /// <summary>Classifies an axis without an intermediate region, after the peak and coordinate checks.</summary>
    /// <param name="peak">The peak, already non-zero.</param>
    /// <param name="v">The coordinate, already non-zero and not equal to the peak.</param>
    /// <returns>The factor.</returns>
    let inline private nonIntermFactor peak v =
        match v < min 0 peak || v > max 0 peak with
        | true -> Zero
        | false -> Scale (v, peak)

    /// <summary>The active pattern naming <c>TupleVariationHeader::calculate_scalar</c>'s axis cases.</summary>
    /// <param name="input">The peak, coordinate, intermediate flag, start and end of one axis.</param>
    /// <returns>The factor.</returns>
    let inline private (|TupleAxisFactor|) (input: struct (int * int * bool * int * int)) =
        let struct (peak, v, hasInterm, start, finish) = input
        match peak with
        | 0 -> Keep
        | _ when v = 0 -> Zero
        | _ when v = peak -> Keep
        | _ when hasInterm -> intermFactor peak v start finish
        | _ -> nonIntermFactor peak v

    /// <summary>Accumulates one axis's factor into the tuple scalar, in axis order, <c>calculate_scalar</c>.</summary>
    /// <param name="peakSource">The peak tuples: the header data or the shared tuples.</param>
    /// <param name="data">The header data, for the intermediate tuples.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="axisCount">The number of axes.</param>
    /// <param name="axis">The next axis.</param>
    /// <param name="scalar">The product so far; start with 1.0f.</param>
    /// <param name="hasInterm">Whether the tuple has an intermediate region.</param>
    /// <param name="peakAt">The byte offset of the first peak value.</param>
    /// <param name="startAt">The byte offset of the first intermediate start value.</param>
    /// <param name="endAt">The byte offset of the first intermediate end value.</param>
    /// <returns>The tuple scalar.</returns>
    [<TailCall>]
    let rec private scalarFromAxes
        (peakSource: ReadOnlySpan<byte>) (data: ReadOnlySpan<byte>)
        (coords: ReadOnlySpan<int>) axisCount axis scalar
        hasInterm peakAt startAt endAt =
        match axis >= axisCount with
        | true -> scalar
        | false ->
            let peak = int (Binary.i16 peakSource (peakAt + axis * 2))
            let v = NormalizedCoords.at coords axis
            let start = if hasInterm then int (Binary.i16 data (startAt + axis * 2)) else 0
            let finish = if hasInterm then int (Binary.i16 data (endAt + axis * 2)) else 0
            match struct (peak, v, hasInterm, start, finish) with
            | TupleAxisFactor factor ->
                match factor with
                | Keep -> scalarFromAxes peakSource data coords axisCount (axis + 1) scalar hasInterm peakAt startAt endAt
                | Zero -> 0.0f
                | Scale (numerator, denominator) ->
                    scalarFromAxes peakSource data coords axisCount (axis + 1)
                        (scalar * (float32 numerator / float32 denominator)) hasInterm peakAt startAt endAt

    /// <summary>The tuple scalar of a header, <c>TupleVariationHeader::calculate_scalar</c>.</summary>
    /// <param name="data">The var data bytes holding the header and any embedded tuples.</param>
    /// <param name="header">The parsed header.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="axisCount">The number of axes.</param>
    /// <param name="sharedTuples">The shared tuple records, in F2DOT14 units.</param>
    /// <returns>The scalar, or 0 when a shared tuple index is out of range.</returns>
    let calculateScalar (data: ReadOnlySpan<byte>) header (coords: ReadOnlySpan<int>) axisCount (sharedTuples: ReadOnlySpan<byte>) =
        let index = int (header.TupleIndex &&& TupleIndexMask)
        let hasInterm =
            match header with
            | IntermediateRegion -> true
            | _ -> false
        let hasPeak =
            match header with
            | EmbeddedPeak -> true
            | _ -> false
        let startAt = header.At + 4 + (if hasPeak then axisCount * 2 else 0)
        let endAt = startAt + axisCount * 2
        match hasPeak with
        | true -> scalarFromAxes data data coords axisCount 0 1.0f hasInterm (header.At + 4) startAt endAt
        | false ->
            match (index + 1) * axisCount > sharedTuples.Length / 2 with
            | true -> 0.0f
            | false -> scalarFromAxes sharedTuples data coords axisCount 0 1.0f hasInterm (index * axisCount * 2) startAt endAt

    /// <summary>Reads the point count of a packed point stream, <c>decompile_points</c>' count byte(s).</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="at">The offset of the count.</param>
    /// <returns>The point count and the offset after the count, or none when the count does not hold.</returns>
    let private pointsCount (data: ReadOnlySpan<byte>) at =
        match at < 0 || at >= data.Length with
        | true -> ValueNone
        | false ->
            let first = data[at]
            match first &&& PointsAreWords with
            | 0uy -> ValueSome (struct (int first, at + 1))
            | _ ->
                match at + 1 >= data.Length with
                | true -> ValueNone
                | false -> ValueSome (struct ((int (first &&& PointRunCountMask) <<< 8) ||| int data[at + 1], at + 2))

    /// <summary>Walks a packed point stream to its end, checking every run, <c>decompile_points</c>' bounds.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="at">The offset of the next control byte.</param>
    /// <param name="index">The next point index.</param>
    /// <param name="count">The point count.</param>
    /// <returns>The offset after the runs, or none when a run is out of range.</returns>
    [<TailCall>]
    let rec private pointsEnd (data: ReadOnlySpan<byte>) at index count =
        match index >= count with
        | true -> ValueSome at
        | false ->
            match at >= data.Length with
            | true -> ValueNone
            | false ->
                let control = data[at]
                let runCount = int (control &&& PointRunCountMask) + 1
                let width = if control &&& PointsAreWords <> 0uy then 2 else 1
                let pastCount = int64 index + int64 runCount > int64 count
                let pastData = int64 at + 1L + int64 runCount * int64 width > int64 data.Length
                match pastCount || pastData with
                | true -> ValueNone
                | false -> pointsEnd data (at + 1 + runCount * width) (index + runCount) count

    /// <summary>Reads one point delta of a run.</summary>
    /// <param name="source">The bytes.</param>
    /// <param name="words">Whether the run holds words.</param>
    /// <param name="p">The value offset.</param>
    /// <returns>The delta and the next offset.</returns>
    let private readPointValue (source: ReadOnlySpan<byte>) words p =
        match words with
        | true -> struct (uint32 (Binary.u16 source p), p + 2)
        | false -> struct (uint32 (Binary.u8 source p), p + 1)

    /// <summary>The running state of the point unfolder.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private PointsState =
        { N: uint32
          P: int
          Remaining: int
          Words: bool }

    /// <summary>Unfolds a packed point stream into the running point indices, as an unsigned sum.</summary>
    [<Struct>]
    type private PointsUnfolder =
        interface ISpanUnfolder<byte, PointsState, uint32> with
            member _.Next (source, state) =
                match state.Remaining > 0 with
                | true ->
                    let struct (delta, nextP) = readPointValue source state.Words state.P
                    let n = state.N + delta
                    struct (n, { state with N = n; P = nextP; Remaining = state.Remaining - 1 })
                | false ->
                    let control = Binary.u8 source state.P
                    let words = control &&& PointsAreWords <> 0uy
                    let runCount = int (control &&& PointRunCountMask) + 1
                    let struct (delta, nextP) = readPointValue source words (state.P + 1)
                    let n = state.N + delta
                    struct (n, { N = n; P = nextP; Remaining = runCount - 1; Words = words })

    /// <summary>Decompiles a packed point stream, <c>TupleVariationData::decompile_points</c>.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="at">The offset of the stream.</param>
    /// <returns>The point indices and the offset after the stream, or none when it is truncated.</returns>
    let decompilePoints (data: ReadOnlySpan<byte>) at =
        match at < 0 with
        | true -> ValueNone
        | false ->
            match pointsCount data at with
            | ValueNone -> ValueNone
            | ValueSome struct (count, dataAt) ->
                match pointsEnd data dataAt 0 count with
                | ValueNone -> ValueNone
                | ValueSome finish ->
                    let points = Sealed.unfoldSpan data count { N = 0u; P = dataAt; Remaining = 0; Words = false } (PointsUnfolder())
                    ValueSome (struct (points, finish))

    /// <summary>The kind of one packed value run.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private PackedKind =
        | Zeros
        | Bytes
        | Words
        | Longs

    /// <summary>Classifies a control byte's run kind, <c>VALUES_SIZE_MASK</c>.</summary>
    /// <param name="control">The control byte.</param>
    /// <returns>The run kind.</returns>
    let private packKind control =
        match control &&& ValuesAreZeros <> 0uy, control &&& ValuesAreWords <> 0uy with
        | true, false -> Zeros
        | false, true -> Words
        | true, true -> Longs
        | false, false -> Bytes

    /// <summary>The byte width of a run kind.</summary>
    /// <param name="kind">The run kind.</param>
    /// <returns>The value width in bytes.</returns>
    let private widthOf kind =
        match kind with
        | Zeros -> 0
        | Bytes -> 1
        | Words -> 2
        | Longs -> 4

    /// <summary>Walks a packed delta stream to its end, checking every run, <c>TupleValues::decompile</c>' bounds.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="at">The offset of the next control byte.</param>
    /// <param name="index">The next value index.</param>
    /// <param name="count">The number of values.</param>
    /// <returns>The offset after the runs, or none when a run is out of range.</returns>
    [<TailCall>]
    let rec private deltasEnd (data: ReadOnlySpan<byte>) at index count =
        match index >= count with
        | true -> ValueSome at
        | false ->
            match at >= data.Length with
            | true -> ValueNone
            | false ->
                let control = data[at]
                let runCount = int (control &&& ValueRunCountMask) + 1
                let width = widthOf (packKind control)
                let pastCount = int64 index + int64 runCount > int64 count
                let pastData = int64 at + 1L + int64 runCount * int64 width > int64 data.Length
                match pastCount || pastData with
                | true -> ValueNone
                | false -> deltasEnd data (at + 1 + runCount * width) (index + runCount) count

    /// <summary>Reads one signed delta of a run.</summary>
    /// <param name="source">The bytes.</param>
    /// <param name="kind">The run kind.</param>
    /// <param name="p">The value offset.</param>
    /// <returns>The delta and the next offset.</returns>
    let private readDelta (source: ReadOnlySpan<byte>) kind p =
        match kind with
        | Zeros -> struct (0, p)
        | Bytes -> struct (int (sbyte (Binary.u8 source p)), p + 1)
        | Words -> struct (int (Binary.i16 source p), p + 2)
        | Longs -> struct (int (Binary.u32 source p), p + 4)

    /// <summary>The running state of the delta unfolder.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DeltasState =
        { Index: int
          P: int
          Remaining: int
          Kind: PackedKind
          Start: int }

    /// <summary>Unfolds a packed delta stream, zeroing the leading values up to the skip count.</summary>
    [<Struct>]
    type private DeltasUnfolder =
        interface ISpanUnfolder<byte, DeltasState, int> with
            member _.Next (source, state) =
                match state.Remaining > 0 with
                | true ->
                    let struct (value, nextP) = readDelta source state.Kind state.P
                    let output = if state.Index < state.Start then 0 else value
                    struct (output, { state with Index = state.Index + 1; P = nextP; Remaining = state.Remaining - 1 })
                | false ->
                    let control = Binary.u8 source state.P
                    let kind = packKind control
                    let runCount = int (control &&& ValueRunCountMask) + 1
                    let struct (value, nextP) = readDelta source kind (state.P + 1)
                    let output = if state.Index < state.Start then 0 else value
                    struct (output, { Index = state.Index + 1; P = nextP; Remaining = runCount - 1; Kind = kind; Start = state.Start })

    /// <summary>Decompiles a packed delta stream, <c>TupleValues::decompile</c>.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="at">The offset of the stream.</param>
    /// <param name="count">The number of deltas to produce.</param>
    /// <param name="start">The number of leading deltas left at zero.</param>
    /// <returns>The deltas and the offset after the stream, or none when it is truncated.</returns>
    let decompileDeltas (data: ReadOnlySpan<byte>) at count start =
        match at < 0 || count < 0 with
        | true -> ValueNone
        | false ->
            match deltasEnd data at 0 count with
            | ValueNone -> ValueNone
            | ValueSome finish ->
                let deltas = Sealed.unfoldSpan data count { Index = 0; P = at; Remaining = 0; Kind = Bytes; Start = start } (DeltasUnfolder())
                ValueSome (struct (deltas, finish))
