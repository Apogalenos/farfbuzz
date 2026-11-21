module Farfbuzz.Tests.TrakTests

open System
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.Harness
open Farfbuzz.OpenType
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.SfntBuilder

/// <summary>Writes a 16.16 fixed value as four big-endian bytes.</summary>
/// <param name="value">The float value.</param>
/// <returns>The bytes.</returns>
let private f16dot16 (value: float32) : byte array =
    be32 (uint32 (int (value * 65536.0f)))

/// <summary>Writes one FWORD track value as two big-endian bytes.</summary>
/// <param name="value">The signed value.</param>
/// <returns>The bytes.</returns>
let private fword (value: int) : byte array =
    be16 (uint32 (value &&& 0xFFFF))

/// <summary>An entry whose value array starts at the given offset, with the size table after it.</summary>
/// <param name="valuesZ">The value-array offset.</param>
/// <returns>The eight entry bytes.</returns>
let private entry (valuesZ: uint32) : byte array =
    Array.concat [ be32 0u; be16 0u; be16 valuesZ ]

/// <summary>Builds the trak bytes of a size table and its values, for the value tests.</summary>
/// <param name="sizes">The F16DOT16 sizes.</param>
/// <param name="values">The FWORD values.</param>
/// <returns>The bytes: entry, values, then the size table.</returns>
let private valueTable (sizes: float32 array) (values: int array) : byte array =
    let valuesStart = 8
    Array.concat
        [ entry (uint32 valuesStart)
          values |> Array.collect fword
          sizes |> Array.collect f16dot16 ]

/// <summary>The P8.S13 trak and STAT unit and allocation tests.</summary>
[<Tests>]
let tests =
    testList "P8.S13" [
        test "exact size match" {
            let table = valueTable [| 9.0f; 12.0f; 24.0f |] [| 50; 100; 200 |]
            let value = Trak.getValue (ReadOnlyMemory table) 0 3 14 12.0f
            Expect.equal value 100.0f "the value of the size exactly matching the point size"
        }
        test "swapped sizes" {
            let table = valueTable [| 24.0f; 12.0f |] [| 200; 100 |]
            let value = Trak.interpolateAt (ReadOnlyMemory table) 0 0 12 18.0f
            Expect.equal value 150.0f "the swapped sizes interpolate the same as the sorted pair"
        }
        test "track with a zero tracking value allocates nothing and returns the same run" {
            let trakTable =
                Array.concat
                    [ be32 0x00010000u
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 0u ]
            let file = sfnt 0x00010000u [ Tags.Trak, trakTable ]
            let font = Font.create (Face.create (ReadOnlyMemory file) 0u)
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x42u |]) 0 -1 GlyphRun.empty
                |> GlyphRun.clearPositions
                |> TailCalls.settle
            let result = AatLayout.track font run
            Expect.isTrue (Object.ReferenceEquals(run, result)) "reference-equal"
            Expect.isTrue (Object.ReferenceEquals(ImmutableCollectionsMarshal.AsArray run.Positions, ImmutableCollectionsMarshal.AsArray result.Positions)) "positions array reused"
            Expect.equal (bytesAllocatedBy (fun () -> AatLayout.track font run |> ignore)) 0L "bytes allocated"
        }
    ]
