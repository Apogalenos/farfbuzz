module Farfbuzz.Tests.GlyphPointsTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder

/// <summary>Encodes a signed 16-bit value as two big-endian bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private beI16 (value: int) =
    be16 (uint32 (value &&& 0xFFFF))

/// <summary>Builds a one-contour, four-point simple glyph whose flags exercise the repeat bit.</summary>
/// <returns>The glyph bytes.</returns>
let private repeatGlyph : byte array =
    Array.concat
        [ be16 1u
          be16 0u
          be16 0u
          be16 100u
          be16 100u
          be16 3u
          be16 0u
          [| 0x09uy; 0x02uy; 0x00uy |]
          beI16 10
          beI16 20
          beI16 -5
          beI16 15
          beI16 1
          beI16 2
          beI16 3
          beI16 4 ]

/// <summary>A comparable view of one contour point.</summary>
/// <param name="point">The point.</param>
/// <returns>The fields.</returns>
let private pointTuple (point: ContourPoint) =
    struct (point.X, point.Y, point.Flag, point.IsEndPoint)

/// <summary>One PointBuilder operation in the property's model.</summary>
type private PbOp =
    | PbAppend of int
    | PbRange of int list
    | PbTruncate of int

/// <summary>Random sequences of append, append-range and truncate operations.</summary>
let private pbOps : Gen<PbOp list> =
    Gen.resize 10 (Gen.listOf (Gen.oneof [
        Gen.map PbAppend (Gen.choose (-1000, 1000))
        Gen.map PbRange (Gen.resize 6 (Gen.listOf (Gen.choose (-1000, 1000))))
        Gen.map PbTruncate (Gen.choose (0, 8))
    ]))

/// <summary>Applies one operation to the list model.</summary>
/// <param name="model">The list so far.</param>
/// <param name="op">The operation.</param>
/// <returns>The next list.</returns>
let private applyModel (model: int list) (op: PbOp) : int list =
    match op with
    | PbAppend x -> model @ [ x ]
    | PbRange xs -> model @ xs
    | PbTruncate k -> List.truncate k model

/// <summary>Applies one operation to the builder.</summary>
/// <param name="builder">The builder so far.</param>
/// <param name="op">The operation.</param>
/// <returns>The next builder.</returns>
let private applyBuilder (builder: PointBuilder<int>) (op: PbOp) : PointBuilder<int> =
    match op with
    | PbAppend x -> PointBuilder.append builder x
    | PbRange xs -> PointBuilder.appendRange builder (ReadOnlySpan (List.toArray xs)) |> TailCalls.settle
    | PbTruncate k -> PointBuilder.truncate builder k

/// <summary>The glyph-point and PointBuilder tests of P7.S07.</summary>
[<Tests>]
let tests =
    testList "P7.S07" [
        test "flag repeat expands a repeated flag and accumulates coordinates" {
            let points = GlyphPoints.simplePoints (ReadOnlySpan repeatGlyph) false false
            let expected =
                [ struct (10.0f, 1.0f, 0x09uy, false)
                  struct (30.0f, 3.0f, 0x09uy, false)
                  struct (25.0f, 6.0f, 0x09uy, false)
                  struct (40.0f, 10.0f, 0x00uy, true) ]
            match points with
            | ValueNone -> failtest "a valid simple glyph must read"
            | ValueSome actual ->
                Expect.equal actual.Length expected.Length "point count"
                actual |> Seq.iteri (fun i point ->
                    Expect.equal (pointTuple point) expected[i] $"point {i}")
        }
        test "transform applies the component matrix" {
            let scale =
                { ComponentFlags = 0us
                  GlyphId = 0u
                  M0 = 2.0f
                  M1 = 0.0f
                  M2 = 0.0f
                  M3 = 3.0f
                  Offset = { X = 0.0f; Y = 0.0f; Flag = 0uy; IsEndPoint = false } }
            let rotated =
                { ComponentFlags = 0us
                  GlyphId = 0u
                  M0 = 0.0f
                  M1 = 1.0f
                  M2 = -1.0f
                  M3 = 0.0f
                  Offset = { X = 0.0f; Y = 0.0f; Flag = 0uy; IsEndPoint = false } }
            let point = { X = 5.0f; Y = 7.0f; Flag = 0uy; IsEndPoint = false }
            let scaled = GlyphPoints.transformCall scale point
            let rotated = GlyphPoints.transformCall rotated point
            Expect.equal scaled.X 10.0f "x scaled by m0"
            Expect.equal scaled.Y 21.0f "y scaled by m3"
            Expect.equal rotated.X -7.0f "x rotated"
            Expect.equal rotated.Y 5.0f "y rotated"
        }
        test "anchored offsets read as zero and keep the identity matrix" {
            let glyph =
                Array.concat
                    [ be16 0xFFFFu
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 5u
                      [| 3uy; 4uy |] ]
            let components = GlyphPoints.components (ReadOnlySpan glyph) false
            Expect.equal components.Length 1 "one component"
            Expect.equal components[0].GlyphId 5u "glyph id"
            Expect.equal components[0].Offset.X 0.0f "anchored tx is zero"
            Expect.equal components[0].Offset.Y 0.0f "anchored ty is zero"
            Expect.isTrue components[0].Offset.IsEndPoint "offset carries the end-point flag"
            Expect.equal components[0].M0 1.0f "identity m0"
            Expect.equal components[0].M3 1.0f "identity m3"
        }
        test "non-anchored offsets read the xy arguments" {
            let glyph =
                Array.concat
                    [ be16 0xFFFFu
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 0u
                      be16 0x0002u
                      be16 7u
                      [| 10uy; 0xECuy |] ]
            let components = GlyphPoints.components (ReadOnlySpan glyph) false
            Expect.equal components.Length 1 "one component"
            Expect.equal components[0].Offset.X 10.0f "tx"
            Expect.equal components[0].Offset.Y -20.0f "ty"
        }
        testProperty "PointBuilder appends, ranges, truncates and publishes like the list model" <|
            Prop.forAll (Arb.fromGen pbOps) (fun ops ->
                let expected = ops |> List.fold applyModel []
                let actual =
                    ops
                    |> List.fold applyBuilder (PointBuilder.create())
                    |> PointBuilder.publish
                    |> Seq.toList
                actual = expected
                |> Prop.collect $"{ops.Length} ops")
    ]
