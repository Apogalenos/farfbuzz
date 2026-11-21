module Farfbuzz.Tests.MvarTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Tests.SfntBuilder

/// <summary>A store instance with one region at scalar 1 and two one-word rows, for the MVAR tests.</summary>
/// <returns>The instance, whose inner index 0 reads 10 and inner index 1 reads 20.</returns>
let private twoRowInstance : StoreInstance =
    let data =
        { ItemCount = 2
          WordCount = 1
          LongWords = false
          RegionIndices = ImmutableArray.CreateRange [| 0us |]
          Rows = ReadOnlyMemory [| 0uy; 10uy; 0uy; 20uy |]
          RowSize = 2 }
    { Store =
        { AxisCount = 1
          RegionCount = 1
          Regions = ImmutableArray.CreateRange [| { Start = 0s; Peak = 1s; End = 1s } |]
          Data = ImmutableArray.CreateRange [| data |] }
      Scalars = ImmutableArray.CreateRange [| 1.0f |] }

/// <summary>Loads the MVAR table of a face built around one MVAR table.</summary>
/// <param name="mvar">The MVAR table bytes.</param>
/// <returns>The parsed MVAR.</returns>
let private loadMvar (mvar: byte array) : Mvar =
    Mvar.load (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Mvar, mvar ])) 0u)

/// <summary>The MVAR tests of P7.S11.</summary>
[<Tests>]
let tests =
    testList "P7.S11" [
        test "record size 12 walks the records with the declared stride" {
            let mvar =
                Array.concat
                    [ be32 0x00010000u
                      be16 0u
                      be16 12u
                      be16 2u
                      be16 0u
                      be32 0x68617363u // 'hasc'
                      be32 0u
                      be32 0u
                      be32 0x68647363u // 'hdsc'
                      be32 1u
                      be32 0u ]
            let loaded = loadMvar mvar
            Expect.equal (Mvar.getVar loaded twoRowInstance (Tag.make 'h' 'a' 's' 'c')) 10.0f "first record"
            Expect.equal (Mvar.getVar loaded twoRowInstance (Tag.make 'h' 'd' 's' 'c')) 20.0f "second record found at stride 12"
        }
        test "a tag missing from the records reads 0" {
            let mvar =
                Array.concat
                    [ be32 0x00010000u
                      be16 0u
                      be16 8u
                      be16 1u
                      be16 0u
                      be32 0x68617363u // 'hasc'
                      be32 0u ]
            let loaded = loadMvar mvar
            Expect.equal (Mvar.getVar loaded twoRowInstance (Tag.make 'x' 'x' 'x' 'x')) 0.0f "missing tag"
        }
    ]
