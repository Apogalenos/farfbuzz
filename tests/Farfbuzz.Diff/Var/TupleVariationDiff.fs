module Farfbuzz.Diff.TupleVariationDiff

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Var
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>One packed point run.</summary>
type private PointRun =
    | PByte of byte list
    | PWord of uint16 list

/// <summary>One packed delta run.</summary>
type private DeltaRun =
    | DZero of int
    | DByte of sbyte list
    | DWord of int16 list
    | DLong of int list

/// <summary>Encodes a packed point stream for the given runs.</summary>
/// <param name="runs">The runs.</param>
/// <returns>The stream bytes.</returns>
let private encodePoints (runs: PointRun list) : byte array =
    let count =
        runs
        |> List.sumBy (fun run ->
            match run with
            | PByte bs -> bs.Length
            | PWord ws -> ws.Length)
    let bytes = ResizeArray<byte>()
    if count < 128 then
        bytes.Add(byte count)
    else
        bytes.Add(0x80uy ||| byte (count >>> 8))
        bytes.Add(byte count)
    for run in runs do
        match run with
        | PByte bs ->
            bytes.Add(byte (bs.Length - 1))
            for b in bs do
                bytes.Add b
        | PWord ws ->
            bytes.Add(0x80uy ||| byte (ws.Length - 1))
            for w in ws do
                bytes.Add(byte (int w >>> 8))
                bytes.Add(byte w)
    bytes.ToArray()

/// <summary>Generates a list of point runs totalling the given count.</summary>
/// <param name="count">The total point count.</param>
/// <returns>The generator.</returns>
let private genPointRuns (count: int) : Gen<PointRun list> =
    let rec go remaining acc =
        match remaining with
        | 0 -> Gen.constant (List.rev acc)
        | _ ->
            gen {
                let! runLen = Gen.choose (1, min remaining 127)
                let! words = Gen.choose (0, 1)
                match words with
                | 1 ->
                    let! ws = Gen.listOfLength runLen (Gen.choose (0, 0xFFFF) |> Gen.map uint16)
                    return! go (remaining - runLen) (PWord ws :: acc)
                | _ ->
                    let! bs = Gen.listOfLength runLen (Gen.choose (0, 0xFF) |> Gen.map byte)
                    return! go (remaining - runLen) (PByte bs :: acc)
            }
    go count []

/// <summary>Generates a packed point stream, sometimes truncated so a run overruns.</summary>
let private pointsGen : Gen<byte array> =
    gen {
        let! count =
            Gen.frequency [ 3, Gen.elements [ 0; 1; 2; 127; 128; 129; 255; 256; 500 ]
                            2, Gen.choose (0, 600) ]
        let! runs = genPointRuns count
        let stream = encodePoints runs
        let! truncate = Gen.frequency [ 5, Gen.constant false; 1, Gen.constant true ]
        match truncate && stream.Length > 0 with
        | false -> return stream
        | true ->
            let! cut = Gen.choose (0, stream.Length - 1)
            return stream[.. cut]
    }

/// <summary>Encodes a packed delta stream for the given runs.</summary>
/// <param name="runs">The runs.</param>
/// <returns>The stream bytes.</returns>
let private encodeDeltas (runs: DeltaRun list) : byte array =
    let bytes = ResizeArray<byte>()
    for run in runs do
        match run with
        | DZero len -> bytes.Add(0x80uy ||| byte (len - 1))
        | DByte bs ->
            bytes.Add(byte (bs.Length - 1))
            for b in bs do
                bytes.Add(byte b)
        | DWord ws ->
            bytes.Add(0x40uy ||| byte (ws.Length - 1))
            for w in ws do
                bytes.Add(byte (int w >>> 8))
                bytes.Add(byte w)
        | DLong ls ->
            bytes.Add(0xC0uy ||| byte (ls.Length - 1))
            for l in ls do
                bytes.Add(byte (uint32 l >>> 24))
                bytes.Add(byte (uint32 l >>> 16))
                bytes.Add(byte (uint32 l >>> 8))
                bytes.Add(byte l)
    bytes.ToArray()

/// <summary>Generates a list of delta runs totalling the given count.</summary>
/// <param name="count">The total delta count.</param>
/// <returns>The generator.</returns>
let private genDeltaRuns (count: int) : Gen<DeltaRun list> =
    let rec go remaining acc =
        match remaining with
        | 0 -> Gen.constant (List.rev acc)
        | _ ->
            gen {
                let! runLen = Gen.choose (1, min remaining 64)
                let! kind = Gen.choose (0, 3)
                match kind with
                | 0 -> return! go (remaining - runLen) (DZero runLen :: acc)
                | 1 ->
                    let! bs = Gen.listOfLength runLen (Gen.choose (-128, 127) |> Gen.map sbyte)
                    return! go (remaining - runLen) (DByte bs :: acc)
                | 2 ->
                    let! ws = Gen.listOfLength runLen (Gen.choose (-32768, 32767) |> Gen.map int16)
                    return! go (remaining - runLen) (DWord ws :: acc)
                | _ ->
                    let! ls = Gen.listOfLength runLen (Gen.choose (-1000000, 1000000))
                    return! go (remaining - runLen) (DLong ls :: acc)
            }
    go count []

/// <summary>Generates a packed delta stream with a count and start, sometimes truncated.</summary>
let private deltasGen : Gen<struct (byte array * int * int)> =
    gen {
        let! count =
            Gen.frequency [ 3, Gen.elements [ 0; 1; 2; 3; 7; 63; 64; 65; 127; 128; 200 ]
                            2, Gen.choose (0, 300) ]
        let! runs = genDeltaRuns count
        let! start = Gen.frequency [ 5, Gen.choose (0, 3); 1, Gen.choose (0, count + 2) ]
        let stream = encodeDeltas runs
        let! truncate = Gen.frequency [ 5, Gen.constant false; 1, Gen.constant true ]
        match truncate && stream.Length > 0 with
        | false -> return struct (stream, count, start)
        | true ->
            let! cut = Gen.choose (0, stream.Length - 1)
            return struct (stream[.. cut], count, start)
    }

/// <summary>Farfbuzz's decompiled points: the verdict, the indices and the bytes consumed.</summary>
/// <param name="bytes">The stream bytes.</param>
/// <returns>The comparable answer.</returns>
let private oursPoints (bytes: byte array) : struct (bool * uint32[] * int) =
    match TupleVariation.decompilePoints (ReadOnlySpan bytes) 0 with
    | ValueNone -> struct (false, [||], 0)
    | ValueSome struct (points, consumed) -> struct (true, points.AsSpan().ToArray(), consumed)

/// <summary>The oracle's decompiled points: the verdict, the indices and the bytes consumed.</summary>
/// <param name="bytes">The stream bytes.</param>
/// <returns>The comparable answer.</returns>
let private oraclePoints (bytes: byte array) : struct (bool * uint32[] * int) =
    let out = Array.zeroCreate (max 1 bytes.Length)
    let mutable outCount = 0u
    let mutable consumed = 0u
    let ok = Shims.farfbuzz_oracle_decompile_points(bytes, uint32 bytes.Length, out, uint32 out.Length, &outCount, &consumed)
    match ok with
    | 0 -> struct (false, [||], 0)
    | _ -> struct (true, out[.. int outCount - 1], int consumed)

/// <summary>Farfbuzz's decompiled deltas: the verdict, the values and the bytes consumed.</summary>
/// <param name="input">The stream bytes, count and start.</param>
/// <returns>The comparable answer.</returns>
let private oursDeltas (input: struct (byte array * int * int)) : struct (bool * int[] * int) =
    let struct (bytes, count, start) = input
    match TupleVariation.decompileDeltas (ReadOnlySpan bytes) 0 count start with
    | ValueNone -> struct (false, [||], 0)
    | ValueSome struct (deltas, consumed) -> struct (true, deltas.AsSpan().ToArray(), consumed)

/// <summary>The oracle's decompiled deltas: the verdict, the values and the bytes consumed.</summary>
/// <param name="input">The stream bytes, count and start.</param>
/// <returns>The comparable answer.</returns>
let private oracleDeltas (input: struct (byte array * int * int)) : struct (bool * int[] * int) =
    let struct (bytes, count, start) = input
    let out = Array.zeroCreate count
    let mutable consumed = 0u
    let ok = Shims.farfbuzz_oracle_decompile_deltas(bytes, uint32 bytes.Length, uint32 count, uint32 start, out, uint32 out.Length, &consumed)
    match ok with
    | 0 -> struct (false, [||], 0)
    | _ -> struct (true, out, int consumed)

/// <summary>The fixed packed point streams pinning the edge cases.</summary>
let private fixedPointStreams : byte array array =
    [| [| 0x00uy |] // count 0
       [| 0x02uy; 0x01uy; 0x0Auy; 0x0Buy |] // byte run
       [| 0x02uy; 0x81uy; 0xFFuy; 0xFFuy; 0x00uy; 0x01uy |] // word run, sum past 65535
       [| 0x80uy; 0x01uy; 0x81uy; 0xFFuy; 0xFFuy |] // word count above 127, one word
       [| 0x05uy; 0x04uy; 0x01uy |] // run overruns the data
       [| 0x01uy; 0x01uy; 0x0Auy; 0x0Buy |] |] // run count exceeds the point count

/// <summary>The fixed packed delta streams pinning the edge cases.</summary>
let private fixedDeltaStreams : struct (byte array * int * int) array =
    [| struct ([| 0x80uy ||| 1uy; 0x00uy ||| 0uy; 0xFFuy; 0x40uy ||| 0uy; 0x12uy; 0x34uy; 0xC0uy ||| 0uy; 0x00uy; 0x01uy; 0x23uy; 0x45uy |], 5, 0) // every kind
       struct ([| 0x03uy; 0x05uy; 0x06uy; 0x07uy; 0x08uy |], 4, 2) // start skip
       struct ([| 0x02uy; 0x0Auy |], 2, 1) // run overruns the data
       struct ([| 0x01uy; 0x01uy; 0x0Auy; 0x0Buy |], 1, 0) |] // run count exceeds the delta count

/// <summary>The P7.S08 tuple variation data differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P7.S08" [
        sampled "P7.S08 generated points" pointsGen 5000 oursPoints oraclePoints
        sampled "P7.S08 generated deltas" deltasGen 5000 oursDeltas oracleDeltas
        testCase "P7.S08 fixed points" (fun () ->
            for bytes in fixedPointStreams do
                Expect.equal (oursPoints bytes) (oraclePoints bytes) $"points differ for %A{bytes}")
        testCase "P7.S08 fixed deltas" (fun () ->
            for input in fixedDeltaStreams do
                Expect.equal (oursDeltas input) (oracleDeltas input) $"deltas differ for %A{input}")
    ]
