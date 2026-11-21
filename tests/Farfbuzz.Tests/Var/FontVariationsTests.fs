module Farfbuzz.Tests.FontVariationsTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Var
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a one-axis fvar table with one named instance at weight 500.</summary>
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
    Array.concat
        [ be32 0x00010000u
          be16 16u
          be16 2u
          be16 1u
          be16 20u
          be16 0u
          be16 12u
          axis ]

/// <summary>Builds a two-axis fvar table: wght 100-900 and wdth 50-200.</summary>
/// <returns>The fvar table bytes.</returns>
let private twoAxisFvar : byte array =
    let wght =
        Array.concat
            [ be32 0x77676874u // 'wght'
              be32 0x00640000u // min 100.0
              be32 0x01900000u // default 400.0
              be32 0x03840000u // max 900.0
              be16 0u
              be16 256u ]
    let wdth =
        Array.concat
            [ be32 0x77647468u // 'wdth'
              be32 0x00320000u // min 50.0
              be32 0x00640000u // default 100.0
              be32 0x00C80000u // max 200.0
              be16 0u
              be16 257u ]
    Array.concat
        [ be32 0x00010000u
          be16 16u
          be16 2u
          be16 2u
          be16 20u
          be16 0u
          be16 12u
          wght
          wdth ]

/// <summary>Loads the variation tables of a face built around one fvar table.</summary>
/// <param name="fvar">The fvar table bytes.</param>
/// <returns>The face's variation tables.</returns>
let private faceVariations (fvar: byte array) : FaceVariations =
    FaceVariations.load (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Fvar, fvar ])) 0u)

/// <summary>The font variation state tests of P7.S05.</summary>
[<Tests>]
let tests =
    testList "P7.S05" [
        test "the non-zero flag is computed once when the state is built" {
            let face = faceVariations twoAxisFvar
            let zero =
                FontVariations.ofNormalized face FontVariations.noNamedInstance (ImmutableArray.CreateRange [| 0; 0 |])
            let zeroCoords = zero.Coords
            Expect.isFalse zeroCoords.HasNonZero "all zero"
            let nonZero =
                FontVariations.ofNormalized face FontVariations.noNamedInstance (ImmutableArray.CreateRange [| 0; 1 |])
            let nonZeroCoords = nonZero.Coords
            Expect.isTrue nonZeroCoords.HasNonZero "one non-zero"
        }
        test "a normalized set keeps the design coordinates NaN and the values unclamped" {
            let face = faceVariations twoAxisFvar
            let state =
                FontVariations.ofNormalized face FontVariations.noNamedInstance (ImmutableArray.CreateRange [| 999999 |])
            Expect.equal state.DesignCoords.Length 2 "padded to two design coords"
            Expect.isTrue (Single.IsNaN state.DesignCoords[0]) "first design coord NaN"
            Expect.isTrue (Single.IsNaN state.DesignCoords[1]) "second design coord NaN"
            let coords = state.Coords
            let span = coords.AsSpan()
            Expect.equal span[0] 999999 "unclamped coordinate"
            Expect.equal span[1] 0 "padded with zero"
        }
        test "design coords shorter than the axes are defaulted and longer ones truncated" {
            let face = faceVariations twoAxisFvar
            let short =
                FontVariations.ofDesign face FontVariations.noNamedInstance (ImmutableArray.CreateRange [| 500.0f |])
            Expect.equal short.DesignCoords.Length 2 "two axes"
            Expect.equal short.DesignCoords[0] 500.0f "first taken"
            Expect.equal short.DesignCoords[1] 100.0f "second defaulted"
            let long =
                FontVariations.ofDesign face FontVariations.noNamedInstance (ImmutableArray.CreateRange [| 500.0f; 150.0f; 999.0f |])
            Expect.equal long.DesignCoords.Length 2 "still two axes"
            Expect.equal long.DesignCoords[0] 500.0f "first taken"
            Expect.equal long.DesignCoords[1] 150.0f "second taken"
        }
        test "the last variation of a duplicated tag wins" {
            let face = faceVariations twoAxisFvar
            let wght = Tag.make 'w' 'g' 'h' 't'
            let duplicated =
                ImmutableArray.CreateRange [| { Tag = wght; Value = 500.0f }; { Tag = wght; Value = 700.0f } |]
            let state = FontVariations.ofVariations face FontVariations.noNamedInstance duplicated
            Expect.equal state.DesignCoords[0] 700.0f "the later wght wins"
            Expect.equal state.DesignCoords[1] 100.0f "wdth stays at its default"
        }
    ]
