module Farfbuzz.Tests.ConditionTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Var

/// <summary>Encodes an int16 as big-endian bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let private be16 (value: int) : byte array = [| byte (value >>> 8); byte value |]

/// <summary>Encodes a 24-bit value as big-endian bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The three bytes.</returns>
let private be24 (value: int) : byte array = [| byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Encodes a uint32 as big-endian bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The four bytes.</returns>
let private be32 (value: uint32) : byte array = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>The empty GDEF store of a test instancer.</summary>
let private noGdef : StoreInstance =
    { Store = ItemVariationStore.empty
      Scalars = ImmutableArray<float32>.Empty }

/// <summary>Builds an instancer over the given coordinates.</summary>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The instancer.</returns>
let private instancerWith (coords: int array) : Instancer =
    { Gdef = noGdef
      Coords = NormalizedCoords.ofSealed (ImmutableArray.CreateRange coords) }

/// <summary>A store whose outer index 1, inner index 0 holds the given delta.</summary>
/// <param name="delta">The delta value.</param>
/// <returns>The store instance with scalar 1.</returns>
let private deltaStore (delta: int) : StoreInstance =
    let region : RegionAxis = { Start = 0s; Peak = 0s; End = 0s }
    let emptyData : VarData =
        { ItemCount = 0
          WordCount = 0
          LongWords = false
          RegionIndices = ImmutableArray.Empty
          Rows = ReadOnlyMemory.Empty
          RowSize = 0 }
    let data : VarData =
        { ItemCount = 1
          WordCount = 1
          LongWords = false
          RegionIndices = ImmutableArray.CreateRange [| 0us |]
          Rows = ReadOnlyMemory [| byte (delta >>> 8); byte delta |]
          RowSize = 2 }
    let store : ItemVariationStore =
        { AxisCount = 0
          RegionCount = 1
          Regions = ImmutableArray.CreateRange [| region |]
          Data = ImmutableArray.CreateRange [| emptyData; data |] }
    { Store = store; Scalars = ImmutableArray.CreateRange [| 1.0f |] }

/// <summary>A format-1 axis-range condition: axis 0, filter [-8192, 8192].</summary>
let private axisRangeTrue : byte array =
    Array.concat [ be16 1; be16 0; be16 -8192; be16 8192 ]

/// <summary>Builds a chain of <paramref name="depth"/> And conditions, each with one child, ending in a terminal.</summary>
/// <param name="depth">The number of And nodes.</param>
/// <param name="terminal">The terminal condition bytes.</param>
/// <returns>The chained bytes.</returns>
let private andChain (depth: int) (terminal: byte array) : byte array =
    let andSize = 6
    let bytes = Array.zeroCreate<byte> (depth * andSize + terminal.Length)
    for index in 0 .. depth - 1 do
        let at = index * andSize
        bytes[at] <- 0uy
        bytes[at + 1] <- 3uy
        bytes[at + 2] <- 1uy
        bytes[at + 3] <- 0uy
        bytes[at + 4] <- 0uy
        bytes[at + 5] <- byte andSize
    Array.Copy(terminal, 0, bytes, depth * andSize, terminal.Length)
    bytes

/// <summary>The condition formats of P7.S12.</summary>
[<Tests>]
let tests =
    testList "P7.S12" [
        test "condition format 1: the axis range compares the raw 2.14 coordinate" {
            let table = ReadOnlyMemory axisRangeTrue
            let instancer = instancerWith [| 0 |]
            Expect.isTrue (Conditions.evaluate table 0 instancer Conditions.MaxNestingLevel) "coord 0 lies inside [-8192, 8192]"
            let outside = instancerWith [| 16384 |]
            Expect.isFalse (Conditions.evaluate table 0 outside Conditions.MaxNestingLevel) "coord 16384 lies outside [-8192, 8192]"
        }
        test "condition format 2: the default value plus the variation delta must be positive" {
            let noVar = ReadOnlyMemory (Array.concat [ be16 2; be16 5; be32 0xFFFFFFFFu ])
            Expect.isTrue (Conditions.evaluate noVar 0 (instancerWith [| 0x4000 |]) Conditions.MaxNestingLevel) "5 + 0 is positive"
            let noVarNeg = ReadOnlyMemory (Array.concat [ be16 2; be16 -5; be32 0xFFFFFFFFu ])
            Expect.isFalse (Conditions.evaluate noVarNeg 0 (instancerWith [| 0x4000 |]) Conditions.MaxNestingLevel) "-5 + 0 is not positive"
            let delta = ReadOnlyMemory (Array.concat [ be16 2; be16 0; be32 0x00010000u ])
            let withStore = { Gdef = deltaStore 100; Coords = NormalizedCoords.ofSealed (ImmutableArray.CreateRange [| 0x4000 |]) }
            Expect.isTrue (Conditions.evaluate delta 0 withStore Conditions.MaxNestingLevel) "0 + 100 is positive"
        }
        test "condition format 3: And holds when every child holds" {
            let andTwoNull = ReadOnlyMemory (Array.concat [ be16 3; [| 2uy |]; be24 0; be24 0 ])
            Expect.isTrue (Conditions.evaluate andTwoNull 0 (instancerWith [| 0 |]) Conditions.MaxNestingLevel) "two null children are true"
            let andWithFalse = ReadOnlyMemory (Array.concat [ be16 3; [| 2uy |]; be24 9; be24 0 ])
            let falseChild = ReadOnlyMemory (Array.concat [ be16 1; be16 0; be16 8192; be16 8192 ])
            let both = Array.concat [ andWithFalse.ToArray(); falseChild.ToArray() ] |> ReadOnlyMemory
            Expect.isFalse (Conditions.evaluate both 0 (instancerWith [| 16384 |]) Conditions.MaxNestingLevel) "a false child makes And false"
        }
        test "condition format 4: Or holds when some child holds" {
            let orNone = ReadOnlyMemory (Array.concat [ be16 4; [| 0uy |] ])
            Expect.isFalse (Conditions.evaluate orNone 0 (instancerWith [| 0 |]) Conditions.MaxNestingLevel) "no children is false"
            let orNull = ReadOnlyMemory (Array.concat [ be16 4; [| 1uy |]; be24 0 ])
            Expect.isTrue (Conditions.evaluate orNull 0 (instancerWith [| 0 |]) Conditions.MaxNestingLevel) "a null child is true"
        }
        test "condition format 5: Negate inverts its child" {
            let negateNull = ReadOnlyMemory (Array.concat [ be16 5; be24 0 ])
            Expect.isFalse (Conditions.evaluate negateNull 0 (instancerWith [| 0 |]) Conditions.MaxNestingLevel) "not (null = true) is false"
            let negateFalse = ReadOnlyMemory (Array.concat [ be16 5; be24 5 ])
            let falseChild = ReadOnlyMemory (Array.concat [ be16 4; [| 0uy |] ])
            let both = Array.concat [ negateFalse.ToArray(); falseChild.ToArray() ] |> ReadOnlyMemory
            Expect.isTrue (Conditions.evaluate both 0 (instancerWith [| 0 |]) Conditions.MaxNestingLevel) "not (Or of nothing = false) is true"
        }
        test "condition depth: one chain at the limit and one beyond it" {
            let atLimit = ReadOnlyMemory (andChain 63 axisRangeTrue)
            Expect.isTrue (Conditions.evaluate atLimit 0 (instancerWith [| 0 |]) Conditions.MaxNestingLevel) "63 And nodes plus a terminal still evaluates"
            let beyond = ReadOnlyMemory (andChain 64 axisRangeTrue)
            Expect.isFalse (Conditions.evaluate beyond 0 (instancerWith [| 0 |]) Conditions.MaxNestingLevel) "64 And nodes exhaust the depth and answer false"
        }
    ]
