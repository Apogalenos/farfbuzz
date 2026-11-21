module Farfbuzz.Tests.GvarTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Tests.SfntBuilder

/// <summary>Encodes a single run of byte point numbers as a packed point stream.</summary>
/// <param name="points">The point indices.</param>
/// <returns>The packed bytes.</returns>
let private packBytePoints (points: int list) : byte array =
    match points with
    | [] -> [| 0x00uy |]
    | ps ->
        let count = ps.Length
        let diffs = ps |> List.mapi (fun i p -> if i = 0 then p else p - ps[i - 1])
        Array.concat [| [| byte count |]; [| byte (count - 1) |]; diffs |> List.map byte |> List.toArray |]

/// <summary>Encodes a single run of byte deltas as a packed delta stream.</summary>
/// <param name="deltas">The signed deltas.</param>
/// <returns>The packed bytes.</returns>
let private packByteDeltas (deltas: int list) : byte array =
    Array.concat [| [| byte (deltas.Length - 1) |]; deltas |> List.map (fun d -> byte (d &&& 0xFF)) |> List.toArray |]

/// <summary>Builds a one-tuple gvar for glyph 0 with an embedded peak and byte deltas.</summary>
/// <param name="axisCount">The axis count.</param>
/// <param name="tupleIndex">The tuple index, with its flags.</param>
/// <param name="peak">The embedded peak tuple.</param>
/// <param name="privatePoints">The private point indices, empty for a tuple that applies to all points.</param>
/// <param name="xDeltas">The x deltas.</param>
/// <param name="yDeltas">The y deltas.</param>
/// <returns>The gvar table.</returns>
let private singleTupleGvar (axisCount: int) (tupleIndex: uint16) (peak: int16 list) (privatePoints: int list) (xDeltas: int list) (yDeltas: int list) : Gvar =
    let peakBytes = peak |> List.collect (fun v -> [ byte (uint16 v >>> 8); byte v ]) |> List.toArray
    let headerSize = 4 + peakBytes.Length
    let dataOffset = 4 + headerSize
    let pointsBytes = if List.isEmpty privatePoints then [||] else packBytePoints privatePoints
    let serialized = Array.concat [ pointsBytes; packByteDeltas xDeltas; packByteDeltas yDeltas ]
    let varDataSize = serialized.Length
    let varData =
        Array.concat
            [ be16 1u
              be16 (uint32 dataOffset)
              be16 (uint32 varDataSize)
              be16 (uint32 tupleIndex)
              peakBytes
              serialized ]
    { AxisCount = axisCount
      SharedTuples = ReadOnlyMemory.Empty
      GlyphCount = 1
      LongOffsets = false
      Offsets = ReadOnlyMemory (Array.concat [ be16 0u; be16 (uint32 (varData.Length / 2)) ])
      Data = ReadOnlyMemory varData }

/// <summary>Builds points from coordinate and end-point triples.</summary>
/// <param name="coords">The x, y and end-point triples.</param>
/// <returns>The points.</returns>
let private makePoints (coords: (float32 * float32 * bool) list) : ImmutableArray<ContourPoint> =
    coords
    |> List.map (fun (x, y, isEnd) -> { X = x; Y = y; Flag = 0uy; IsEndPoint = isEnd })
    |> List.toArray
    |> ImmutableArray.CreateRange

/// <summary>Applies a single-tuple gvar to points and returns the final x coordinates.</summary>
/// <param name="gvar">The gvar.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="points">The points.</param>
/// <param name="phantomOnly">The phantom-only mode.</param>
/// <returns>The x coordinates.</returns>
let private applyXs (gvar: Gvar) (coords: int list) (points: ImmutableArray<ContourPoint>) (phantomOnly: bool) : float32 list =
    match Gvar.applyDeltas gvar (ReadOnlySpan (List.toArray coords)) 0u phantomOnly points with
    | ValueNone -> failwith "expected success"
    | ValueSome result -> result |> Seq.map (fun point -> point.X) |> Seq.toList

/// <summary>The one-axis tuple index with an embedded peak and private points.</summary>
[<Literal>]
let private PrivateTuple = 0xA000us

/// <summary>The one-axis tuple index with an embedded peak and no private points.</summary>
[<Literal>]
let private AllPointsTuple = 0x8000us

/// <summary>The 2.14 encoding of the peak 1.0.</summary>
[<Literal>]
let private PeakOne = 0x4000s

/// <summary>The random inputs of the DeltaBuffer property.</summary>
let private dbInput : Gen<struct (int * float32 array * float32 array * float32 * int array * uint32 array * int * int)> =
    gen {
        let! count = Gen.choose (2, 8)
        let! xs = Gen.listOfLength count (Gen.choose (-100, 100) |> Gen.map float32)
        let! ys = Gen.listOfLength count (Gen.choose (-100, 100) |> Gen.map float32)
        let! scalar = Gen.choose (-200, 200) |> Gen.map float32
        let! deltas = Gen.listOfLength count (Gen.choose (-100, 100))
        let! indices = Gen.listOfLength count (Gen.choose (0, count - 1) |> Gen.map uint32)
        let! start = Gen.choose (0, count - 1)
        let! length = Gen.choose (1, count - start)
        return struct (count, Array.ofList xs, Array.ofList ys, scalar, Array.ofList deltas, Array.ofList indices, start, length)
    }

/// <summary>The DeltaBuffer operations applied to a literal array model.</summary>
/// <param name="count">The point count.</param>
/// <param name="xs">The x coordinates.</param>
/// <param name="ys">The y coordinates.</param>
/// <param name="scalar">The scalar.</param>
/// <param name="deltas">The deltas.</param>
/// <param name="indices">The point indices.</param>
/// <param name="start">The range start.</param>
/// <param name="length">The range length.</param>
/// <returns>The model coordinates after the operations.</returns>
let private modelResult (count: int) (xs: float32 array) (ys: float32 array) (scalar: float32) (deltas: int array) (indices: uint32 array) (start: int) (length: int) =
    let dx = Array.zeroCreate count
    let dy = Array.zeroCreate count
    for i in start .. count - 1 do
        xs[i] <- xs[i] + float32 deltas[i] * scalar
        ys[i] <- ys[i] + float32 deltas[i] * scalar
        dx[i] <- dx[i] + float32 deltas[i] * scalar
        dy[i] <- dy[i] + float32 deltas[i] * scalar
    for i in 0 .. count - 1 do
        let point = int indices[i]
        if point < count && point >= start then
            dx[point] <- dx[point] + float32 deltas[i] * scalar
            dy[point] <- dy[point] + float32 deltas[i] * scalar
    dx[0] <- float32 deltas[0] * scalar
    dy[0] <- float32 deltas[0] * scalar
    for i in start .. start + length - 1 do
        dx[i] <- 0.0f
        dy[i] <- 0.0f
    for i in start .. start + length - 1 do
        xs[i] <- xs[i] + dx[i]
        ys[i] <- ys[i] + dy[i]
    struct (xs, ys)

/// <summary>The DeltaBuffer operations applied through the Sealed helper.</summary>
/// <param name="count">The point count.</param>
/// <param name="xs">The x coordinates.</param>
/// <param name="ys">The y coordinates.</param>
/// <param name="scalar">The scalar.</param>
/// <param name="deltas">The deltas.</param>
/// <param name="indices">The point indices.</param>
/// <param name="start">The range start.</param>
/// <param name="length">The range length.</param>
/// <returns>The published coordinates.</returns>
let private bufferResult (count: int) (xs: float32 array) (ys: float32 array) (scalar: float32) (deltas: int array) (indices: uint32 array) (start: int) (length: int) =
    let buffer = DeltaBuffer.create (ReadOnlySpan xs) (ReadOnlySpan ys)
    let buffer = DeltaBuffer.addScaledX buffer scalar (ReadOnlySpan deltas) start
    let buffer = DeltaBuffer.addScaledY buffer scalar (ReadOnlySpan deltas) start
    let buffer = DeltaBuffer.accumulate buffer scalar (ReadOnlySpan deltas) (ReadOnlySpan deltas) start
    let buffer = DeltaBuffer.accumulateIndexed buffer scalar (ReadOnlySpan deltas) (ReadOnlySpan deltas) (ReadOnlySpan indices) start count
    let buffer = DeltaBuffer.setDelta buffer 0 (float32 deltas[0] * scalar) (float32 deltas[0] * scalar)
    let buffer = DeltaBuffer.clear buffer start length
    let buffer = DeltaBuffer.flush buffer start length
    DeltaBuffer.publish buffer

/// <summary>The gvar delta-application tests of P7.S09.</summary>
[<Tests>]
let tests =
    testList "P7.S09" [
        test "iup cases" {
            let interpolation =
                singleTupleGvar 1 PrivateTuple [ PeakOne ] [ 0; 4 ] [ 0; 32 ] [ 0; 0 ]
            let interpolationPoints =
                makePoints [ 0.0f, 0.0f, false; 8.0f, 0.0f, false; 16.0f, 0.0f, false; 24.0f, 0.0f, false; 32.0f, 0.0f, true ]
            Expect.equal (applyXs interpolation [ 16384 ] interpolationPoints false) [ 0.0f; 16.0f; 32.0f; 48.0f; 64.0f ] "interpolated deltas between the two referenced points"

            let equalNeighbours =
                singleTupleGvar 1 PrivateTuple [ PeakOne ] [ 0; 3 ] [ 2; 5 ] [ 0; 0 ]
            let equalPoints =
                makePoints [ 5.0f, 0.0f, false; 7.0f, 0.0f, false; 3.0f, 0.0f, false; 5.0f, 0.0f, true ]
            Expect.equal (applyXs equalNeighbours [ 16384 ] equalPoints false) [ 7.0f; 7.0f; 3.0f; 10.0f ] "equal neighbours infer zero when the deltas differ"

            let outsideRange =
                singleTupleGvar 1 PrivateTuple [ PeakOne ] [ 1; 2 ] [ 10; 20 ] [ 0; 0 ]
            let outsidePoints =
                makePoints [ 10.0f, 0.0f, false; 30.0f, 0.0f, false; 20.0f, 0.0f, false; 40.0f, 0.0f, true ]
            Expect.equal (applyXs outsideRange [ 16384 ] outsidePoints false) [ 30.0f; 40.0f; 40.0f; 50.0f ] "points outside the referenced range take the nearer referenced delta"
        }
        test "phantom only" {
            let gvar =
                singleTupleGvar 1 AllPointsTuple [ PeakOne ] [] [ 1; 2; 3; 4; 5; 6 ] [ 0; 0; 0; 0; 0; 0 ]
            let points =
                makePoints
                    [ 10.0f, 0.0f, false
                      20.0f, 0.0f, true
                      30.0f, 0.0f, false
                      40.0f, 0.0f, false
                      50.0f, 0.0f, false
                      60.0f, 0.0f, false ]
            Expect.equal (applyXs gvar [ 16384 ] points true) [ 10.0f; 20.0f; 33.0f; 44.0f; 55.0f; 66.0f ] "only the last four points receive deltas"
        }
        testProperty "DeltaBuffer matches a literal model" <|
            Prop.forAll (Arb.fromGen dbInput) (fun struct (count, xs, ys, scalar, deltas, indices, start, length) ->
                let struct (mxs, mys) = modelResult count (Array.copy xs) (Array.copy ys) scalar deltas indices start length
                let struct (bxs, bys) = bufferResult count xs ys scalar deltas indices start length
                (mxs = (bxs |> Seq.toArray) && mys = (bys |> Seq.toArray))
                |> Prop.collect $"{count} points")
    ]
