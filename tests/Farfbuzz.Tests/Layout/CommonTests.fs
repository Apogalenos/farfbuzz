module Farfbuzz.Tests.Layout.CommonTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format 1 coverage from 16-bit glyph ids.</summary>
/// <param name="glyphs">The glyph ids, in file order.</param>
/// <returns>The coverage bytes.</returns>
let private coverage1 (glyphs: uint32 list) : byte array =
    Array.concat [ be16 1u; be16 (uint32 glyphs.Length); glyphs |> List.map be16 |> Array.concat ]

/// <summary>Builds a format 2 coverage from 16-bit ranges.</summary>
/// <param name="ranges">First, last and value per range.</param>
/// <returns>The coverage bytes.</returns>
let private coverage2 (ranges: (uint32 * uint32 * uint32) list) : byte array =
    Array.concat [ be16 2u; be16 (uint32 ranges.Length); ranges |> List.map (fun (f, l, v) -> Array.concat [ be16 f; be16 l; be16 v ]) |> Array.concat ]

/// <summary>Builds a format 3 coverage from 24-bit glyph ids.</summary>
/// <param name="glyphs">The glyph ids, in file order.</param>
/// <returns>The coverage bytes.</returns>
let private coverage3 (glyphs: uint32 list) : byte array =
    Array.concat [ be16 3u; be24 (uint32 glyphs.Length); glyphs |> List.map be24 |> Array.concat ]

/// <summary>Builds a format 4 coverage from 24-bit ranges.</summary>
/// <param name="ranges">First, last and value per range.</param>
/// <returns>The coverage bytes.</returns>
let private coverage4 (ranges: (uint32 * uint32 * uint32) list) : byte array =
    Array.concat [ be16 4u; be24 (uint32 ranges.Length); ranges |> List.map (fun (f, l, v) -> Array.concat [ be24 f; be24 l; be24 v ]) |> Array.concat ]

/// <summary>Builds a format 1 class definition from 16-bit class values.</summary>
/// <param name="start">The first glyph.</param>
/// <param name="classes">The class values, one per glyph from start.</param>
/// <returns>The class-definition bytes.</returns>
let private classDef1 (start: uint32) (classes: uint32 list) : byte array =
    Array.concat [ be16 1u; be16 start; be16 (uint32 classes.Length); classes |> List.map be16 |> Array.concat ]

/// <summary>Builds a format 2 class definition from 16-bit ranges.</summary>
/// <param name="ranges">First, last and class per range.</param>
/// <returns>The class-definition bytes.</returns>
let private classDef2 (ranges: (uint32 * uint32 * uint32) list) : byte array =
    Array.concat [ be16 2u; be16 (uint32 ranges.Length); ranges |> List.map (fun (f, l, v) -> Array.concat [ be16 f; be16 l; be16 v ]) |> Array.concat ]

/// <summary>Builds a format 3 class definition from 24-bit class values.</summary>
/// <param name="start">The first glyph.</param>
/// <param name="classes">The class values, one per glyph from start.</param>
/// <returns>The class-definition bytes.</returns>
let private classDef3 (start: uint32) (classes: uint32 list) : byte array =
    Array.concat [ be16 3u; be24 start; be24 (uint32 classes.Length); classes |> List.map be24 |> Array.concat ]

/// <summary>Builds a format 4 class definition from 24-bit ranges with 16-bit classes.</summary>
/// <param name="ranges">First, last and class per range.</param>
/// <returns>The class-definition bytes.</returns>
let private classDef4 (ranges: (uint32 * uint32 * uint32) list) : byte array =
    Array.concat [ be16 4u; be24 (uint32 ranges.Length); ranges |> List.map (fun (f, l, v) -> Array.concat [ be24 f; be24 l; be16 v ]) |> Array.concat ]

/// <summary>A closed range at most 200 wide, for the addRange property.</summary>
let private smallRangeGen =
    gen {
        let! a = Gen.choose (0, 0xFFFFFF)
        let! width = Gen.choose (0, 200)
        return struct (uint32 a, uint32 a + uint32 width)
    }

/// <summary>The P5.S01 digest, coverage and class-definition cases.</summary>
[<Tests>]
let tests =
    testList "P5.S01" [
        testProperty "a digest has no false negative" <| fun (glyphs: uint32 array) ->
            let digest = glyphs |> Array.fold (fun d g -> Digest.add g d) Digest.empty
            glyphs |> Array.forall (fun g -> Digest.mayHaveCall g digest)
        testProperty "addRange equals adding every member for ranges up to 200 wide" <|
            Prop.forAll (Arb.fromGen smallRangeGen) (fun struct (a, b) ->
                let viaRange = Digest.addRange a b Digest.empty
                let viaAdd = Ranges.fold (int a) (int b + 1) (fun d i -> Digest.add (uint32 i) d) Digest.empty
                viaRange = viaAdd)
        testProperty "either part may have implies the union may have" <| fun (first: uint32 array) (second: uint32 array) (query: uint32) ->
            let firstDigest = first |> Array.fold (fun d g -> Digest.add g d) Digest.empty
            let secondDigest = second |> Array.fold (fun d g -> Digest.add g d) Digest.empty
            let union = Digest.union firstDigest secondDigest
            not (Digest.mayHaveCall query firstDigest || Digest.mayHaveCall query secondDigest)
            || Digest.mayHaveCall query union
        testProperty "the union of two digests equals the digest of both arrays added in turn" <| fun (first: uint32 array) (second: uint32 array) ->
            let firstDigest = first |> Array.fold (fun d g -> Digest.add g d) Digest.empty
            let secondDigest = second |> Array.fold (fun d g -> Digest.add g d) Digest.empty
            let union = Digest.union firstDigest secondDigest
            let combined = Array.append first second |> Array.fold (fun d g -> Digest.add g d) Digest.empty
            union = combined
        test "the union may answer true where both parts answer false" {
            let first = Digest.add 3u Digest.empty
            let second = Digest.add 64u Digest.empty
            let union = Digest.union first second
            Expect.isTrue (Digest.mayHaveCall 0u union) "the union may have 0"
            Expect.isFalse (Digest.mayHaveCall 0u first) "first does not"
            Expect.isFalse (Digest.mayHaveCall 0u second) "second does not"
        }
        testProperty "full mayHave every glyph" <| fun (glyph: uint32) ->
            Digest.mayHaveCall glyph Digest.full
        test "coverage format 1 on an unsorted array finds what the binary search does" {
            let coverage = Coverage.parse (ReadOnlySpan (coverage1 [ 5u; 3u; 1u; 7u ])) 0
            Expect.equal (Coverage.index coverage 3u) (Covered 1) "3 sits at index 1"
            Expect.equal (Coverage.index coverage 5u) NotCovered "5 is missed by the binary search"
        }
        test "coverage format 2 answers NotCovered through the null range record" {
            let coverage = Coverage.parse (ReadOnlySpan (coverage2 [ 10u, 20u, 0u ])) 0
            Expect.equal (Coverage.index coverage 15u) (Covered 5) "15 is covered at 5"
            Expect.equal (Coverage.index coverage 5u) NotCovered "5 is before every range"
            Expect.equal (Coverage.index coverage 30u) NotCovered "30 is after every range"
        }
        test "coverage formats 3 and 4 decode 24-bit glyph ids" {
            let format3 = Coverage.parse (ReadOnlySpan (coverage3 [ 0x100u; 0x200u; 0x300u ])) 0
            Expect.equal (Coverage.index format3 0x200u) (Covered 1) "format 3 hit"
            Expect.equal (Coverage.index format3 0x250u) NotCovered "format 3 miss"
            let format4 = Coverage.parse (ReadOnlySpan (coverage4 [ 0x100u, 0x200u, 0u ])) 0
            Expect.equal (Coverage.index format4 0x150u) (Covered 0x50) "format 4 hit"
            Expect.equal (Coverage.index format4 0xFFu) NotCovered "format 4 miss"
        }
        test "an unknown coverage format is null" {
            let coverage = Coverage.parse (ReadOnlySpan (be16 5u)) 0
            Expect.equal (Coverage.index coverage 0u) NotCovered "glyph 0"
            Expect.equal (Coverage.index coverage 123u) NotCovered "glyph 123"
        }
        test "class definition format 1 wraps glyphs below startGlyph to zero" {
            let classDef = ClassDef.parse (ReadOnlySpan (classDef1 10u [ 1u; 2u; 3u ])) 0
            Expect.equal (ClassDef.classOf classDef 5u) 0u "below startGlyph wraps"
            Expect.equal (ClassDef.classOf classDef 11u) 2u "glyph 11 is class 2"
            Expect.equal (ClassDef.classOf classDef 13u) 0u "past the array is zero"
        }
        test "class definition format 2 answers zero on a miss" {
            let classDef = ClassDef.parse (ReadOnlySpan (classDef2 [ 10u, 20u, 3u ])) 0
            Expect.equal (ClassDef.classOf classDef 15u) 3u "15 is class 3"
            Expect.equal (ClassDef.classOf classDef 5u) 0u "5 is a miss"
            Expect.equal (ClassDef.classOf classDef 30u) 0u "30 is a miss"
        }
        test "class definition formats 3 and 4 decode 24-bit glyph ids" {
            let format3 = ClassDef.parse (ReadOnlySpan (classDef3 0x100u [ 4u; 5u ])) 0
            Expect.equal (ClassDef.classOf format3 0x100u) 4u "format 3 first"
            Expect.equal (ClassDef.classOf format3 0x101u) 5u "format 3 second"
            Expect.equal (ClassDef.classOf format3 0xFFu) 0u "format 3 below"
            let format4 = ClassDef.parse (ReadOnlySpan (classDef4 [ 0x100u, 0x101u, 2u ])) 0
            Expect.equal (ClassDef.classOf format4 0x100u) 2u "format 4 first"
            Expect.equal (ClassDef.classOf format4 0x101u) 2u "format 4 second"
            Expect.equal (ClassDef.classOf format4 0xFFu) 0u "format 4 below"
        }
    ]
