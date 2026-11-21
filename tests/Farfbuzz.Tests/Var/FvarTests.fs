module Farfbuzz.Tests.FvarTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a one-axis fvar table with one instance at weight 500.</summary>
/// <returns>The fvar table bytes.</returns>
let private oneAxisFvar : byte array =
    let axis =
        Array.concat
            [ be32 0x77676874u // 'wght'
              be32 0x00640000u // min 100.0
              be32 0x01900000u // default 400.0
              be32 0x03840000u // max 900.0
              be16 0u
              be16 256u ]
    let instance =
        Array.concat
            [ be16 257u
              be16 0u
              be32 0x01F40000u ] // coord 500.0
    Array.concat
        [ be32 0x00010000u
          be16 16u
          be16 2u
          be16 1u
          be16 20u
          be16 1u
          be16 8u
          axis
          instance ]

/// <summary>Builds a one-axis fvar table whose minimum equals its default.</summary>
/// <returns>The fvar table bytes.</returns>
let private minEqualsDefaultFvar : byte array =
    let axis =
        Array.concat
            [ be32 0x77676874u // 'wght'
              be32 0x01900000u // min 400.0
              be32 0x01900000u // default 400.0
              be32 0x03840000u // max 900.0
              be16 0u
              be16 256u ]
    Array.concat
        [ be32 0x00010000u
          be16 16u
          be16 2u
          be16 1u
          be16 20u
          be16 0u
          be16 20u
          axis ]

/// <summary>The int32 values whose float32 conversion rounds, the 16.16 boundaries.</summary>
let private boundaryInts =
    [ Int32.MinValue
      Int32.MaxValue
      1 <<< 24
      -(1 <<< 24)
      (1 <<< 24) + 1
      (1 <<< 24) - 1
      -(1 <<< 24) - 1
      -(1 <<< 24) + 1 ]

/// <summary>A uniformly random int32.</summary>
let private anyInt32 =
    gen {
        let! magnitude = Gen.choose (0, Int32.MaxValue)
        let! negate = Gen.elements [ true; false ]
        return if negate then -magnitude else magnitude
    }

/// <summary>A random int32 favouring the float32-rounding boundaries.</summary>
let private int32Gen =
    Gen.frequency [ 1, Gen.elements boundaryInts; 1, anyInt32 ]

/// <summary>The fixed-point, fvar sanitizer and named-instance tests of P7.S01.</summary>
[<Tests>]
let tests =
    testList "P7.S01" [
        test "f2dot14 decodes every raw value" {
            let mutable checkedValues = 0
            for raw in Int16.MinValue .. Int16.MaxValue do
                Expect.equal (Fixed.f2dot14Call raw) (float32 (int raw) / 16384.0f) $"raw {raw}"
                checkedValues <- checkedValues + 1
            Expect.equal checkedValues 65536 "all 65536 values"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 10000 } "f16dot16 decodes like the 16.16 spec"
            (Prop.forAll (Arb.fromGen int32Gen) (fun raw -> Fixed.f16dot16Call raw = float32 raw / 65536.0f))
        test "roundf rounds half towards positive infinity" {
            Expect.equal (Fixed.roundfCall 0.5f) 1.0f "0.5"
            Expect.equal (Fixed.roundfCall -0.5f) 0.0f "-0.5"
            Expect.equal (Fixed.roundfCall 1.5f) 2.0f "1.5"
            Expect.equal (Fixed.roundfCall 2.5f) 3.0f "2.5"
            Expect.equal (Fixed.roundfCall -2.5f) -2.0f "-2.5"
            Expect.equal (Fixed.roundfCall 8388607.5f) 8388608.0f "8388607.5"
            Expect.isTrue (Single.IsNaN (Fixed.roundfCall Single.NaN)) "NaN"
            Expect.isTrue (Single.IsPositiveInfinity (Fixed.roundfCall Single.PositiveInfinity)) "+inf"
            Expect.isTrue (Single.IsNegativeInfinity (Fixed.roundfCall Single.NegativeInfinity)) "-inf"
        }
        test "an axisSize other than 20 is rejected" {
            let fvar =
                Array.concat
                    [ be32 0x00010000u; be16 16u; be16 2u; be16 1u; be16 24u; be16 0u; be16 20u ]
            let file = sfnt 0x00010000u [ Tags.Fvar, fvar ]
            let loaded = Fvar.load (Face.create (ReadOnlyMemory file) 0u)
            Expect.equal loaded.Axes.Length 0 "0 axes"
        }
        test "an out-of-range instance gives empty coords and invalid name ids" {
            let file = sfnt 0x00010000u [ Tags.Fvar, oneAxisFvar ]
            let loaded = Fvar.load (Face.create (ReadOnlyMemory file) 0u)
            Expect.equal loaded.Axes.Length 1 "one axis"
            Expect.equal loaded.InstanceCount 1 "one instance"
            Expect.equal (Fvar.instanceCoords 1 loaded) ImmutableArray.Empty "empty coords at index = count"
            Expect.equal (Fvar.instanceSubfamilyNameId 1 loaded) 0xFFFFus "subfamily 0xFFFF"
            Expect.equal (Fvar.instancePostscriptNameId 1 loaded) 0xFFFFus "postscript 0xFFFF"
            Expect.equal (Fvar.instanceSubfamilyNameId 0 loaded) 257us "subfamily of the instance"
            Expect.equal (Fvar.instanceCoords 0 loaded) (ImmutableArray.CreateRange [| 500.0f |]) "coord 500"
        }
        test "normalizeAxisValue maps the design space" {
            let oneAxis = Fvar.load (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Fvar, oneAxisFvar ])) 0u)
            Expect.equal (Fvar.normalizeAxisValue 0 250.0f oneAxis) -0.5f "below the default"
            Expect.equal (Fvar.normalizeAxisValue 0 700.0f oneAxis) 0.6f "above the default"
            Expect.equal (Fvar.normalizeAxisValue 0 400.0f oneAxis) 0.0f "at the default"
            Expect.equal (Fvar.normalizeAxisValue 0 50.0f oneAxis) -1.0f "below the minimum"
            Expect.equal (Fvar.normalizeAxisValue 0 1000.0f oneAxis) 1.0f "above the maximum"
            let pinned = Fvar.load (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Fvar, minEqualsDefaultFvar ])) 0u)
            Expect.equal (Fvar.normalizeAxisValue 0 300.0f pinned) 0.0f "minimum equal to default"
            Expect.equal (Fvar.normalizeAxisValue 0 500.0f pinned) 0.2f "above a pinned default"
            Expect.equal (Fvar.normalizeAxisValue 1 250.0f oneAxis) 0.0f "out-of-range axis index"
        }
    ]
