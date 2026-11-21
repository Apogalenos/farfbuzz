module Farfbuzz.Tests.VariationDeviceTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Var

/// <summary>An empty store instance, for the slots these tests do not exercise.</summary>
let private emptyInstance : StoreInstance =
    { Store = ItemVariationStore.empty
      Scalars = ImmutableArray<float32>.Empty }

/// <summary>A one-region store instance whose outer 0, inner 0 delta is the signed int16 at the given scalar.</summary>
/// <param name="scalar">The region scalar.</param>
/// <param name="delta">The int16 delta.</param>
/// <returns>The store instance.</returns>
let private deltaInstance (scalar: float32) (delta: int16) : StoreInstance =
    { Store =
        { AxisCount = 1
          RegionCount = 1
          Regions = ImmutableArray.CreateRange [| { Start = 0s; Peak = 1s; End = 1s } |]
          Data =
            ImmutableArray.CreateRange
                [| { ItemCount = 1
                     WordCount = 1
                     LongWords = false
                     RegionIndices = ImmutableArray.CreateRange [| 0us |]
                     Rows = ReadOnlyMemory [| byte (uint16 delta >>> 8); byte delta |]
                     RowSize = 2 } |] }
      Scalars = ImmutableArray.CreateRange [| scalar |] }

/// <summary>Builds a font with the given coordinates, GDEF store instance and scale.</summary>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="gdef">The GDEF store instance.</param>
/// <param name="xScale">The horizontal scale.</param>
/// <param name="yScale">The vertical scale.</param>
/// <returns>The font.</returns>
let private fontWith (coords: int array) (gdef: StoreInstance) (xScale: int) (yScale: int) : Font =
    let variations =
        { Coords = NormalizedCoords.ofSealed (ImmutableArray.CreateRange coords)
          DesignCoords = ImmutableArray.CreateRange (Array.map (fun _ -> Single.NaN) coords)
          NamedInstance = FontVariations.noNamedInstance
          Hvar = emptyInstance
          Vvar = emptyInstance
          Mvar = emptyInstance
          Gdef = gdef
          Cff2 = lazy emptyInstance }
    Font.withScale xScale yScale { Font.create Face.empty with Variations = variations }

/// <summary>The variation-device tests of P7.S13.</summary>
[<Tests>]
let tests =
    testList "P7.S13" [
        test "zero coords" {
            let font = fontWith [| 0 |] (deltaInstance 1.0f 100s) 1000 1000
            Expect.equal (VariationDevice.xDelta font 0u) 0 "the horizontal delta is zero without non-zero coords"
            Expect.equal (VariationDevice.yDelta font 0u) 0 "the vertical delta is zero without non-zero coords"
            Expect.equal (VariationDevice.xDeltaF font 0u) 0.0f "the horizontal float delta is zero without non-zero coords"
            Expect.equal (VariationDevice.yDeltaF font 0u) 0.0f "the vertical float delta is zero without non-zero coords"
        }
        test "float anchors not rounded" {
            let font = fontWith [| 1 |] (deltaInstance 0.5f 1s) 1500 1000
            Expect.equal (VariationDevice.xDelta font 0u) 1 "the horizontal delta rounds 0.75 to 1"
            Expect.equal (VariationDevice.xDeltaF font 0u) 0.75f "the horizontal float delta keeps 0.75"
            Expect.equal (VariationDevice.yDelta font 0u) 1 "the vertical delta rounds 0.5 to 1"
            Expect.equal (VariationDevice.yDeltaF font 0u) 0.5f "the vertical float delta keeps 0.5"
        }
    ]
