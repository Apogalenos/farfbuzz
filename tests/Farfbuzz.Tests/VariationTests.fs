module Farfbuzz.Tests.VariationTests

open System
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Tests.Allocation

/// <summary>Builds a variation for expectations.</summary>
/// <param name="tag">The axis tag spelling.</param>
/// <param name="value">The coordinate.</param>
/// <returns>The variation.</returns>
let variation tag value = { Tag = Tag.ofString tag; Value = value }

/// <summary>Variation string tests, P1.Variation.</summary>
[<Tests>]
let tests =
    testList "P1.Variation" [
        testList "parse" [
            for text, expected in
                [ "wght=500", variation "wght" 500.0f; "wght 500", variation "wght" 500.0f; "wdth=75.5", variation "wdth" 75.5f
                  "slnt=-7.5", variation "slnt" -7.5f; "TEST=.3", variation "TEST" 0.3f; "0001=500", variation "0001" 500.0f
                  "  opsz = 72  ", variation "opsz" 72.0f; "'wght' 400", variation "wght" 400.0f ] ->
                test text { Expect.equal (Variation.parse text) (Ok expected) text }
        ]
        testList "rejects" [
            for text in [ String.Empty; "wght"; "wght="; "wght=x"; "wght=5 6"; "toolong=1" ] ->
                test $"'{text}'" { Expect.isError (Variation.parse text) text }
        ]
        testList "format" [
            for value, expected in [ variation "wght" 500.0f, "wght=500"; variation "slnt" -7.5f, "slnt=-7.5"; variation "TEST" 0.3f, "TEST=0.3" ] ->
                test expected { Expect.equal (Variation.format value) expected expected }
        ]
        testProperty "format then parse gives integral coordinates back" <| fun (coordinate: int16) ->
            let value = variation "wght" (float32 coordinate)
            Variation.parse (Variation.format value) = Ok value
    ]

/// <summary>The P2.R09 variation allocation case.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R09" [
        test "Variation.parseUtf8 allocates nothing" {
            let text = Encoding.UTF8.GetBytes "wght=400"
            Expect.equal (bytesAllocatedBy (fun () -> Variation.parseUtf8 text |> ignore)) 0L "bytes allocated"
        }
    ]
