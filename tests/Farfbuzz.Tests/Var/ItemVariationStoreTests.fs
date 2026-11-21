module Farfbuzz.Tests.ItemVariationStoreTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Var

/// <summary>A literal transliteration of <c>VarRegionAxis::evaluate</c> (src/hb-ot-layout-common.hh:2549-2573), kept here to check the pattern.</summary>
/// <param name="start">The tent start, as an int.</param>
/// <param name="peak">The tent peak, as an int.</param>
/// <param name="finish">The tent end, as an int.</param>
/// <param name="coord">The normalized coordinate.</param>
/// <returns>The axis factor.</returns>
let private evaluate (start: int) (peak: int) (finish: int) (coord: int) : float32 =
    if peak = 0 || coord = peak then 1.0f
    elif coord = 0 then 0.0f
    elif start > peak || peak > finish then 1.0f
    elif start < 0 && finish > 0 then 1.0f
    elif coord <= start || finish <= coord then 0.0f
    elif coord < peak then float32 (coord - start) / float32 (peak - start)
    else float32 (finish - coord) / float32 (finish - peak)

/// <summary>The numeric factor of one tent and coordinate, through the <see cref="RegionAxisFactor"/> pattern.</summary>
/// <param name="axis">The tent.</param>
/// <param name="coord">The normalized coordinate.</param>
/// <returns>The axis factor.</returns>
let private factorValue (axis: RegionAxis) (coord: int) : float32 =
    match regionAxisFactorCall (struct (axis, coord)) with
    | Unrestricted -> 1.0f
    | Outside -> 0.0f
    | Slope (numerator, denominator) -> float32 numerator / float32 denominator

/// <summary>The int16 boundaries the region-pattern generator favours.</summary>
let private int16Boundaries : int16 array = [| Int16.MinValue; Int16.MaxValue; -16384s; -16383s; -1s; 0s; 1s; 16383s; 16384s |]

/// <summary>The coordinate boundaries the region-pattern generator favours.</summary>
let private coordBoundaries : int array = [| Int32.MinValue; Int32.MaxValue; -16385; -16384; -16383; -1; 0; 1; 16383; 16384; 16385 |]

/// <summary>A random int16 favouring the boundaries.</summary>
let private int16Gen : Gen<int16> =
    gen {
        let! pick = Gen.choose (0, int16Boundaries.Length)
        match pick = int16Boundaries.Length with
        | true -> return! Gen.choose (Int32.MinValue, Int32.MaxValue) |> Gen.map int16
        | false -> return int16Boundaries[pick]
    }

/// <summary>A random coordinate favouring the boundaries.</summary>
let private coordGen : Gen<int> =
    gen {
        let! pick = Gen.choose (0, coordBoundaries.Length)
        match pick = coordBoundaries.Length with
        | true -> return! Gen.choose (-20000, 20000)
        | false -> return coordBoundaries[pick]
    }

/// <summary>A random tent and coordinate.</summary>
let private tentAndCoord : Gen<struct (RegionAxis * int)> =
    gen {
        let! start = int16Gen
        let! peak = int16Gen
        let! finish = int16Gen
        let! coord = coordGen
        return struct ({ Start = start; Peak = peak; End = finish }, coord)
    }

/// <summary>A store instance with one region at scalar 1, for the delta-decoding tests.</summary>
/// <param name="data">The VarData.</param>
/// <returns>The instance.</returns>
let private deltaInstance (data: VarData) : StoreInstance =
    { Store =
        { AxisCount = 1
          RegionCount = 1
          Regions = ImmutableArray.CreateRange [| { Start = 0s; Peak = 1s; End = 1s } |]
          Data = ImmutableArray.CreateRange [| data |] }
      Scalars = ImmutableArray.CreateRange [| 1.0f |] }

/// <summary>The item variation store and DeltaSetIndexMap tests of P7.S03.</summary>
[<Tests>]
let tests =
    testList "P7.S03" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 20000 } "region pattern matches VarRegionAxis::evaluate"
            (Prop.forAll (Arb.fromGen tentAndCoord) (fun struct (axis, coord) ->
                evaluate (int axis.Start) (int axis.Peak) (int axis.End) coord = factorValue axis coord))
        test "long words read the first columns as int32" {
            let data =
                { ItemCount = 1
                  WordCount = 1
                  LongWords = true
                  RegionIndices = ImmutableArray.CreateRange [| 0us |]
                  Rows = ReadOnlyMemory [| 0x00uy; 0x00uy; 0x12uy; 0x34uy |]
                  RowSize = 4 }
            Expect.equal (ItemVariationStore.delta (deltaInstance data) 0u) 4660.0f "int32 delta 0x1234"
        }
        test "short words read the first columns as int16" {
            let data =
                { ItemCount = 1
                  WordCount = 1
                  LongWords = false
                  RegionIndices = ImmutableArray.CreateRange [| 0us |]
                  Rows = ReadOnlyMemory [| 0xFFuy; 0x9Cuy |]
                  RowSize = 2 }
            Expect.equal (ItemVariationStore.delta (deltaInstance data) 0u) -100.0f "int16 delta -100"
        }
        test "signed bytes read 0x80 as -128" {
            let data =
                { ItemCount = 1
                  WordCount = 0
                  LongWords = false
                  RegionIndices = ImmutableArray.CreateRange [| 0us |]
                  Rows = ReadOnlyMemory [| 0x80uy |]
                  RowSize = 1 }
            Expect.equal (ItemVariationStore.delta (deltaInstance data) 0u) -128.0f "int8 delta -128"
        }
        test "the identity map passes the value unchanged" {
            Expect.equal (DeltaSetIndexMap.map DeltaSetIndexMap.identity 0x12345u) 0x12345u "identity"
        }
        test "a value at or past the map count clamps to the last entry" {
            let bytes = ReadOnlyMemory [| 0x00uy; 0uy; 0uy; 0uy; 2uy; 0x02uy; 0x03uy |]
            let mapping = DeltaSetIndexMap.read bytes 1
            let last = DeltaSetIndexMap.map mapping 1u
            Expect.equal (DeltaSetIndexMap.map mapping 2u) last "equal to the last entry"
            Expect.equal (DeltaSetIndexMap.map mapping 100u) last "far past the end"
        }
        test "every width and inner bit count round-trips the 16.16 repack" {
            for width in 1 .. 4 do
                for innerBits in 1 .. 16 do
                    let packed =
                        match width with
                        | 1 -> 0xABu
                        | 2 -> 0xABCDu
                        | 3 -> 0xABCDEFu
                        | _ -> 0x89ABCDEFu
                    let bytes = Array.zeroCreate width
                    for at in 0 .. width - 1 do
                        bytes[at] <- byte (packed >>> (8 * (width - 1 - at)))
                    let mapping =
                        { MapCount = 1u
                          Width = width
                          InnerBitCount = innerBits
                          Data = ReadOnlyMemory bytes }
                    let expected = ((packed >>> innerBits) <<< 16) ||| (packed &&& ((1u <<< innerBits) - 1u))
                    Expect.equal (DeltaSetIndexMap.map mapping 0u) expected $"width {width} inner bits {innerBits}"
        }
    ]
