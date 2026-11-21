module Farfbuzz.Tests.TupleVariationTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Var

/// <summary>Encodes an int16 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let private be16 (value: int16) : byte array =
    [| byte (uint16 value >>> 8); byte value |]

/// <summary>Reads a big-endian int16, 0 outside the array, for the literal scalar port.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readI16 (bytes: byte array) (at: int) : int16 =
    if at < 0 || at + 2 > bytes.Length then 0s
    else int16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>
/// A literal transliteration of <c>TupleVariationHeader::calculate_scalar</c> (src/hb-ot-var-common.hh:113-209),
/// kept here to check the port.
/// </summary>
/// <param name="data">The header data, holding the embedded tuples.</param>
/// <param name="header">The header.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="axisCount">The number of axes.</param>
/// <param name="sharedTuples">The shared tuple records.</param>
/// <returns>The scalar.</returns>
let private scalarLiteral (data: byte array) (header: TupleHeader) (coords: int array) (axisCount: int) (sharedTuples: byte array) : float32 =
    let tupleIndex = header.TupleIndex
    let index = int (tupleIndex &&& 0x0FFFus)
    let hasInterm = (tupleIndex &&& 0x4000us) <> 0us
    let hasPeak = (tupleIndex &&& 0x8000us) <> 0us
    if (not hasPeak) && (index + 1) * axisCount > sharedTuples.Length / 2 then
        0.0f
    else
        let peakSource = if hasPeak then data else sharedTuples
        let peakBase = if hasPeak then header.At + 4 else index * axisCount * 2
        let startAt = header.At + 4 + (if hasPeak then axisCount * 2 else 0)
        let mutable scalar = 1.0f
        let mutable i = 0
        let mutable stopped = false
        while (not stopped) && i < axisCount do
            let peak = int (readI16 peakSource (peakBase + i * 2))
            if peak = 0 then
                i <- i + 1
            else
                let v = if i < coords.Length then coords[i] else 0
                if v = 0 then
                    scalar <- 0.0f
                    stopped <- true
                elif v = peak then
                    i <- i + 1
                elif hasInterm then
                    let start = int (readI16 data (startAt + i * 2))
                    let finish = int (readI16 data (startAt + axisCount * 2 + i * 2))
                    if start > peak || peak > finish || (start < 0 && finish > 0) then
                        i <- i + 1
                    elif v < start || v > finish then
                        scalar <- 0.0f
                        stopped <- true
                    elif v < peak then
                        if peak <> start then scalar <- scalar * (float32 (v - start) / float32 (peak - start))
                        i <- i + 1
                    else
                        if peak <> finish then scalar <- scalar * (float32 (finish - v) / float32 (finish - peak))
                        i <- i + 1
                elif v < min 0 peak || v > max 0 peak then
                    scalar <- 0.0f
                    stopped <- true
                else
                    scalar <- scalar * (float32 v / float32 peak)
                    i <- i + 1
        scalar

/// <summary>A random tuple scalar input: header bytes, header, coordinates, axis count and shared tuples.</summary>
let private scalarInput : Gen<struct (byte array * TupleHeader * int array * int * byte array)> =
    gen {
        let! axisCount = Gen.choose (1, 4)
        let! hasPeak = Gen.choose (0, 1)
        let! hasInterm = Gen.choose (0, 1)
        let! indexBits = Gen.frequency [ 3, Gen.choose (0, 8); 1, Gen.choose (0, 0x0FFF) ]
        let tupleIndex =
            uint16 (indexBits ||| (if hasPeak = 1 then 0x8000 else 0) ||| (if hasInterm = 1 then 0x4000 else 0))
        let! coordLen = Gen.choose (0, axisCount + 2)
        let! coords = Gen.listOfLength coordLen (Gen.choose (-20000, 20000))
        let tupleValue = Gen.choose (-32768, 32767) |> Gen.map int16
        let! peak = if hasPeak = 1 then Gen.listOfLength axisCount tupleValue else Gen.constant []
        let! start = if hasInterm = 1 then Gen.listOfLength axisCount tupleValue else Gen.constant []
        let! finish = if hasInterm = 1 then Gen.listOfLength axisCount tupleValue else Gen.constant []
        let! dataSize = Gen.choose (0, 0xFFFF) |> Gen.map uint16
        let tupleBytes = peak @ start @ finish |> List.collect (fun v -> [ byte (uint16 v >>> 8); byte v ]) |> List.toArray
        let data =
            Array.concat [| be16 (int16 dataSize); be16 (int16 tupleIndex); tupleBytes |]
        let! sharedLen = Gen.choose (0, 32)
        let! shared = Gen.listOfLength sharedLen (Gen.choose (0, 255) |> Gen.map byte)
        return struct (data, { DataSize = dataSize; TupleIndex = tupleIndex; At = 0 }, Array.ofList coords, axisCount, Array.ofList shared)
    }

/// <summary>The tuple variation data tests of P7.S08.</summary>
[<Tests>]
let tests =
    testList "P7.S08" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 20000 } "scalar edges match the literal calculate_scalar"
            (Prop.forAll (Arb.fromGen scalarInput) (fun struct (data, header, coords, axisCount, shared) ->
                let ours = TupleVariation.calculateScalar (ReadOnlySpan data) header (ReadOnlySpan coords) axisCount (ReadOnlySpan shared)
                let expected = scalarLiteral data header coords axisCount shared
                ours = expected))
        test "points all: a count of zero decompiles to no points" {
            match TupleVariation.decompilePoints (ReadOnlySpan [| 0x00uy |]) 0 with
            | ValueNone -> Expect.isTrue false "expected success"
            | ValueSome struct (points, consumed) ->
                Expect.equal points.Length 0 "no points"
                Expect.equal consumed 1 "one byte consumed"
        }
        test "points wrap: word runs accumulate the indices as a uint32 sum" {
            let bytes = [| 0x02uy; 0x81uy; 0xFFuy; 0xFFuy; 0x00uy; 0x01uy |]
            match TupleVariation.decompilePoints (ReadOnlySpan bytes) 0 with
            | ValueNone -> Expect.isTrue false "expected success"
            | ValueSome struct (points, consumed) ->
                Expect.equal points.Length 2 "two points"
                Expect.equal points[0] 65535u "first index 65535"
                Expect.equal points[1] 65536u "second index 65536, not wrapped to 0"
                Expect.equal consumed 6 "six bytes consumed"
        }
        test "deltas kinds: zeros, bytes, words and longs, and the start skip" {
            let bytes =
                [| 0x80uy ||| 1uy // zeros run, two values
                   0x00uy ||| 0uy; 0xFFuy // byte run, one value -1
                   0x40uy ||| 0uy; 0x12uy; 0x34uy // word run, one value 0x1234
                   0xC0uy ||| 0uy; 0x00uy; 0x01uy; 0x23uy; 0x45uy |] // long run, one value 0x00012345
            match TupleVariation.decompileDeltas (ReadOnlySpan bytes) 0 5 0 with
            | ValueNone -> Expect.isTrue false "expected success"
            | ValueSome struct (deltas, consumed) ->
                Expect.equal deltas.Length 5 "five deltas"
                Expect.equal deltas[0] 0 "zeros run"
                Expect.equal deltas[1] 0 "zeros run"
                Expect.equal deltas[2] -1 "byte -1"
                Expect.equal deltas[3] 4660 "word 0x1234"
                Expect.equal deltas[4] 74565 "long 0x00012345"
                Expect.equal consumed 11 "eleven bytes consumed"
            let skipBytes = [| 0x03uy; 0x05uy; 0x06uy; 0x07uy; 0x08uy |]
            match TupleVariation.decompileDeltas (ReadOnlySpan skipBytes) 0 4 2 with
            | ValueNone -> Expect.isTrue false "expected success"
            | ValueSome struct (deltas, consumed) ->
                Expect.equal deltas.Length 4 "four deltas"
                Expect.equal deltas[0] 0 "leading delta skipped"
                Expect.equal deltas[1] 0 "leading delta skipped"
                Expect.equal deltas[2] 7 "third value"
                Expect.equal deltas[3] 8 "fourth value"
                Expect.equal consumed 5 "five bytes consumed"
        }
    ]
