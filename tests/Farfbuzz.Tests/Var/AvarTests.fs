module Farfbuzz.Tests.AvarTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a segment map from (from, to) pairs.</summary>
/// <param name="pairs">The pairs.</param>
/// <returns>The segment map.</returns>
let private segment (pairs: struct (float32 * float32) array) : SegmentMap =
    ImmutableArray.CreateRange pairs

/// <summary>The mapFloat tests of P7.S02, one case per special case of <c>SegmentMaps::map_float</c>.</summary>
[<Tests>]
let tests =
    testList "P7.S02" [
        test "map_float empty map returns the value" {
            Expect.equal (Avar.mapFloat (segment [||]) 0.5f) 0.5f "empty map"
        }
        test "map_float one pair shifts" {
            Expect.equal (Avar.mapFloat (segment [| struct (0.5f, 1.0f) |]) 2.0f) 2.5f "value - from + to"
        }
        test "map_float exact single returns its value" {
            let map = segment [| struct (-1.0f, -1.0f); struct (0.0f, 0.5f); struct (1.0f, 1.0f) |]
            Expect.equal (Avar.mapFloat map 0.0f) 0.5f "the single exact match"
        }
        test "map_float exact triple returns the middle" {
            let map =
                segment
                    [| struct (-1.0f, -1.0f)
                       struct (0.25f, 0.1f)
                       struct (0.25f, 0.2f)
                       struct (0.25f, 0.3f)
                       struct (1.0f, 1.0f) |]
            Expect.equal (Avar.mapFloat map 0.25f) 0.2f "the middle of three"
        }
        test "map_float exact run of four negative returns the end nearer zero" {
            let map =
                segment
                    [| struct (-1.0f, -1.0f)
                       struct (-0.5f, -0.4f)
                       struct (-0.5f, -0.3f)
                       struct (-0.5f, -0.2f)
                       struct (-0.5f, -0.1f)
                       struct (1.0f, 1.0f) |]
            Expect.equal (Avar.mapFloat map -0.5f) -0.1f "the last of a negative run"
        }
        test "map_float exact run at zero returns the smaller magnitude" {
            let map = segment [| struct (-1.0f, -1.0f); struct (0.0f, 0.3f); struct (0.0f, -0.2f); struct (1.0f, 1.0f) |]
            Expect.equal (Avar.mapFloat map 0.0f) -0.2f "the smaller magnitude wins"
        }
        test "map_float trims leading and trailing identity pairs" {
            let map =
                segment
                    [| struct (-1.0f, -1.0f)
                       struct (-1.0f, -0.5f)
                       struct (-1.0f, -0.9f)
                       struct (0.0f, 0.0f)
                       struct (1.0f, 0.3f)
                       struct (1.0f, 0.7f)
                       struct (1.0f, 1.0f) |]
            Expect.equal (Avar.mapFloat map -1.0f) -0.9f "leading pair trimmed"
            Expect.equal (Avar.mapFloat map 1.0f) 0.3f "trailing pair trimmed"
        }
        test "map_float before the first segment shifts" {
            Expect.equal (Avar.mapFloat (segment [| struct (0.5f, 0.25f); struct (1.0f, 1.0f) |]) 0.0f) -0.25f "value - from + to"
        }
        test "map_float interpolates between segments" {
            Expect.equal (Avar.mapFloat (segment [| struct (0.0f, 0.0f); struct (1.0f, 0.5f) |]) 0.25f) 0.125f "before + ratio * span"
        }
    ]

/// <summary>The avar version 2 tests of P7.S04.</summary>
[<Tests>]
let avar2Tests =
    testList "P7.S04" [
        test "null store clamps like v1" {
            let avar2 =
                Array.concat
                    [ be32 0x00020000u // version 2
                      be16 0u // reserved
                      be16 1u // axisCount
                      be16 1u // segment map count
                      be16 0u // from 0.0
                      be16 0u // to 0.0
                      be32 0u // varIdxMap offset, the Null object
                      be32 0u ] // varStore offset, the Null object
            let file = sfnt 0x00010000u [ Tags.Avar, avar2 ]
            let loaded = Avar.load (Face.create (ReadOnlyMemory file) 0u)
            let hasVersion2 =
                match loaded.Version2 with
                | ValueSome _ -> true
                | ValueNone -> false
            Expect.isTrue hasVersion2 "version 2 tail parsed"
            let mapped = Avar.mapCoords16Dot16 loaded (ImmutableArray.CreateRange [| 70000; -70000 |])
            Expect.equal mapped (ImmutableArray.CreateRange [| 65536; -65536 |]) "clamped to [-65536, 65536]"
        }
    ]
