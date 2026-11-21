module Farfbuzz.Tests.NeverThrows

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Exercises every public phase-2 query on a byte sequence.</summary>
/// <param name="bytes">The candidate font bytes.</param>
let private exercise (bytes: byte array) =
    let memory = ReadOnlyMemory bytes
    let count = Face.count memory
    let face = Face.create memory 0u
    let font = Font.create face
    let cmap = (OtFace.create face).Cmap.Value
    Face.tableTags face |> ignore
    Face.referenceTable face Tags.Cmap |> ignore
    Face.createOrFail memory 1u |> ignore
    Font.hAdvance font 0u |> ignore
    Font.vAdvance font 1u |> ignore
    Font.hAdvances font (ReadOnlySpan [| 0u; 1u; 2u |]) |> ignore
    Font.vAdvances font (ReadOnlySpan [| 0u; 1u; 2u |]) |> ignore
    Font.hExtents font |> ignore
    Font.vExtents font |> ignore
    Font.nominalGlyph font 0x41u |> ignore
    Font.variationGlyph font 0x41u 0xFE0Fu |> ignore
    Cmap.collectUnicodes cmap count |> ignore
    Cmap.collectMapping cmap count |> ignore
    Cmap.collectVariationSelectors cmap |> ignore
    Cmap.collectVariationUnicodes cmap 0xFE0Fu |> ignore
    Font.glyphName font 0u |> ignore
    Font.glyphFromName font "A" |> ignore
    Font.glyphFromString font "gid1" |> ignore
    true

/// <summary>Flips one byte at a generated position.</summary>
/// <param name="bytes">The font bytes.</param>
/// <param name="index">The index.</param>
/// <param name="mask">The xor mask.</param>
/// <returns>The mutated bytes.</returns>
let private flip (bytes: byte array) (index: int) (mask: byte) =
    match bytes.Length with
    | 0 -> bytes
    | length ->
        let result = Array.copy bytes
        let at = index % length
        result[at] <- result[at] ^^^ mask
        result

/// <summary>Random bytes and mutated sfnt tables, P2.R08.</summary>
[<Tests>]
let tests =
    testList "P2.R08" [
        testProperty "random bytes never throw through phase-2 queries" <|
            Prop.forAll (Arb.fromGen (Gen.arrayOf (Gen.choose (0, 255) |> Gen.map byte))) exercise
        testProperty "mutated sfnt tables never throw through phase-2 queries" <|
            let body = Gen.arrayOf (Gen.choose (0, 255) |> Gen.map byte)
            let input =
                gen {
                    let! maxp = body
                    let! head = body
                    let! index = Gen.choose (0, 1000)
                    let! mask = Gen.choose (0, 255) |> Gen.map byte
                    return flip (sfnt 0x00010000u [ Tags.Maxp, maxp; Tags.Head, head ]) index mask
                }
            Prop.forAll (Arb.fromGen input) exercise
    ]
