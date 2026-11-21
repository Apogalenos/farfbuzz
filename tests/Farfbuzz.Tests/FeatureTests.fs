module Farfbuzz.Tests.FeatureTests

open System
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.Allocation

/// <summary>Builds a feature for expectations.</summary>
/// <param name="tag">The tag spelling.</param>
/// <param name="value">The value.</param>
/// <param name="start">The first cluster.</param>
/// <param name="stop">The end cluster.</param>
/// <returns>The feature.</returns>
let feature tag value start stop =
    { Tag = Tag.ofString tag; Value = value; Start = start; End = stop }

/// <summary>The examples of <c>hb_feature_from_string</c>'s documentation (hb-common.cc), plus upstream quirks.</summary>
let examples =
    [ "kern", feature "kern" 1u Feature.GlobalStart Feature.GlobalEnd
      "+kern", feature "kern" 1u Feature.GlobalStart Feature.GlobalEnd
      "-kern", feature "kern" 0u Feature.GlobalStart Feature.GlobalEnd
      "kern=0", feature "kern" 0u Feature.GlobalStart Feature.GlobalEnd
      "kern=1", feature "kern" 1u Feature.GlobalStart Feature.GlobalEnd
      "aalt=2", feature "aalt" 2u Feature.GlobalStart Feature.GlobalEnd
      "kern[]", feature "kern" 1u Feature.GlobalStart Feature.GlobalEnd
      "kern[:]", feature "kern" 1u Feature.GlobalStart Feature.GlobalEnd
      "kern[5:]", feature "kern" 1u 5u Feature.GlobalEnd
      "kern[:5]", feature "kern" 1u Feature.GlobalStart 5u
      "kern[3:5]", feature "kern" 1u 3u 5u
      "kern[3]", feature "kern" 1u 3u 4u
      "aalt[3:5]=2", feature "aalt" 2u 3u 5u
      "'liga'", feature "liga" 1u Feature.GlobalStart Feature.GlobalEnd
      "\"liga\" off", feature "liga" 0u Feature.GlobalStart Feature.GlobalEnd
      "liga on", feature "liga" 1u Feature.GlobalStart Feature.GlobalEnd
      "kern xyz", feature "kern" 1u Feature.GlobalStart Feature.GlobalEnd
      "ss1", feature "ss1 " 1u Feature.GlobalStart Feature.GlobalEnd
      "kern=-1", feature "kern" 0xFFFFFFFFu Feature.GlobalStart Feature.GlobalEnd ]

/// <summary>Strings <c>hb_feature_from_string</c> rejects.</summary>
let rejected = [ String.Empty; "="; "kern="; "toolong"; "'lig'"; "'liga"; "kern[1"; "kern 1 2"; "kern=1x" ]

/// <summary>A generator of features whose text form survives a round trip (values and ranges within int).</summary>
let roundTrippable =
    gen {
        let alphanumeric = Gen.elements ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ])
        let! length = Gen.choose (1, 4)
        let! letters = Gen.listOfLength length alphanumeric
        let! value = Gen.choose (0, System.Int32.MaxValue)
        let! start = Gen.choose (0, 1000)
        let! span = Gen.choose (2, 1000)
        let! isGlobal = Gen.elements [ true; false ]
        let tag = Tag.ofString (System.String(List.toArray letters))
        return
            match isGlobal with
            | true -> { Tag = tag; Value = uint32 value; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            | false -> { Tag = tag; Value = uint32 value; Start = uint32 start; End = uint32 (start + span) }
    }

/// <summary>Feature string tests, P1.Feature.</summary>
[<Tests>]
let tests =
    testList "P1.Feature" [
        testList "parse" [
            for text, expected in examples ->
                test text { Expect.equal (Feature.parse text) (Ok expected) text }
        ]
        testList "rejects" [
            for text in rejected ->
                test $"'{text}'" { Expect.isError (Feature.parse text) text }
        ]
        testList "format" [
            test "global on" { Expect.equal (Feature.format (feature "kern" 1u Feature.GlobalStart Feature.GlobalEnd)) "kern" "kern" }
            test "off with range" { Expect.equal (Feature.format (feature "liga" 0u 3u 5u)) "-liga[3:5]" "-liga[3:5]" }
            test "single cluster" { Expect.equal (Feature.format (feature "liga" 1u 3u 4u)) "liga[3]" "liga[3]" }
            test "open end with value" { Expect.equal (Feature.format (feature "aalt" 2u 5u Feature.GlobalEnd)) "aalt[5:]=2" "aalt[5:]=2" }
            test "padded tag is trimmed" { Expect.equal (Feature.format (feature "ss1" 1u Feature.GlobalStart Feature.GlobalEnd)) "ss1" "ss1" }
        ]
        testProperty "format then parse gives the feature back" <| Prop.forAll (Arb.fromGen roundTrippable) (fun value ->
            Feature.parse (Feature.format value) = Ok value)
    ]

/// <summary>The P2.R09 feature allocation case.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R09" [
        test "Feature.parseUtf8 allocates nothing" {
            let text = Encoding.UTF8.GetBytes "kern"
            Expect.equal (bytesAllocatedBy (fun () -> Feature.parseUtf8 text |> ignore)) 0L "bytes allocated"
        }
    ]

/// <summary>Awkward feature bytes: single grammar bytes and whole number spellings.</summary>
let private rawFeatureBytes =
    let single =
        [| ' 'B; '\t'B; 0uy; '\''B; '"'B; '-'B; '+'B; '['B; ']'B; ':'B; ';'B; '='B
           '0'B; '1'B; '5'B; '9'B; 'k'B; 'e'B; 'r'B; 'n'B; 'o'B; 'f'B; 'O'B; 'N'B; 'F'B; 'x'B; 0xE9uy; '\v'B |]
    let numbers =
        [ "2147483647"; "2147483648"; "-2147483648"; "-2147483649"; "4294967295"
          "99999999999999999999999"; "0000000000000000000000001"; "+0"; "-0"; "+"; "-" ]
    let piece =
        Gen.frequency
            [ 8, Gen.elements single |> Gen.map (fun value -> [| value |])
              1, Gen.elements numbers |> Gen.map Encoding.ASCII.GetBytes ]
    gen {
        let! count = Gen.choose (0, 8)
        let! pieces = Gen.listOfLength count piece
        return Array.concat pieces
    }

/// <summary>The cursor stages and staged parser agree, P2.R10.</summary>
[<Tests>]
let cursorReviewTests =
    testList "P2.R10" [
        testProperty "Stages.tryParse and explain agree on raw feature bytes" <|
            Prop.forAll (Arb.fromGen rawFeatureBytes) (fun bytes ->
                match Feature.Stages.tryParse (ReadOnlySpan bytes) with
                | ValueSome feature ->
                    match Feature.explain bytes with
                    | Ok explained -> feature = explained
                    | Error _ -> false
                | ValueNone ->
                    match Feature.explain bytes with
                    | Error _ -> true
                    | Ok _ -> false)
    ]
