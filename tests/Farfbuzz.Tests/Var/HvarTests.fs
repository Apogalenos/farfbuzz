module Farfbuzz.Tests.HvarTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Var
open Farfbuzz.OpenType

/// <summary>A store instance whose outer index 1, inner index 0 holds a delta of 100.</summary>
/// <returns>The instance.</returns>
let private oneDeltaInstance : StoreInstance =
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
          Rows = ReadOnlyMemory [| 0uy; 100uy |]
          RowSize = 2 }
    let store : ItemVariationStore =
        { AxisCount = 0
          RegionCount = 1
          Regions = ImmutableArray.CreateRange [| region |]
          Data = ImmutableArray.CreateRange [| emptyData; data |] }
    { Store = store; Scalars = ImmutableArray.CreateRange [| 1.0f |] }

/// <summary>The HVAR/VVAR clamp and implicit-map tests of P7.S06.</summary>
[<Tests>]
let tests =
    testList "P7.S06" [
        test "clamp to -1" {
            Expect.equal (Hvar.advanceWithVariations 500u 6.0e9f) -1 "a huge delta saturates to unsigned max, reinterpreted as -1"
            Expect.equal (Hvar.advanceWithVariations 500u 0.0f) 500 "a zero delta returns the plain advance"
            Expect.equal (Hvar.advanceWithVariations 0u -1.0f) 0 "a negative delta clamps to zero"
        }
        test "implicit map gid > 0xFFFF" {
            let implicit : MetricsVariations =
                { HasData = true
                  Store = ItemVariationStore.empty
                  AdvMap = ValueNone
                  VorgMap = ValueNone }
            let explicitMap : MetricsVariations =
                { HasData = true
                  Store = ItemVariationStore.empty
                  AdvMap = ValueSome DeltaSetIndexMap.identity
                  VorgMap = ValueNone }
            Expect.equal (Hvar.advanceDelta implicit oneDeltaInstance 0x10000u) 0.0f "implicit mapping ignores a glyph above 0xFFFF"
            Expect.equal (Hvar.advanceDelta explicitMap oneDeltaInstance 0x10000u) 100.0f "an explicit identity map reads the outer=1 delta"
        }
    ]
