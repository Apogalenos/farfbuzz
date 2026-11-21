module Farfbuzz.Tests.TagTests

open System
open Expecto
open FsCheck
open Farfbuzz

/// <summary>Builds a string of printable ASCII characters from arbitrary characters.</summary>
/// <param name="chars">The raw characters.</param>
/// <returns>The characters folded into the printable ASCII range.</returns>
let private printable (chars: char array) =
    String(chars |> Array.map (fun c -> char (0x21 + int c % 0x5E)))

/// <summary>Tag conversion tests, P1.Tag.</summary>
[<Tests>]
let tests =
    testList "P1.Tag" [
        test "a four-letter tag packs its bytes most significant first" {
            Expect.equal (uint32 (Tag.ofString "GSUB")) 0x47535542u "GSUB"
        }
        test "a short tag is padded with spaces" {
            Expect.equal (Tag.ofString "ab") (Tag.ofBytes 0x61uy 0x62uy 0x20uy 0x20uy) "ab"
        }
        test "a long tag is truncated to four bytes" {
            Expect.equal (Tag.ofString "abcdef") (Tag.ofString "abcd") "abcdef"
        }
        test "an empty string gives the empty tag" {
            Expect.equal (Tag.ofString String.Empty) Tags.Empty "empty"
        }
        test "a NUL ends the tag" {
            Expect.equal (Tag.ofString "a\000bc") (Tag.ofString "a") "NUL"
        }
        test "a leading NUL gives the empty tag" {
            Expect.equal (Tag.ofString "\000abc") Tags.Empty "leading NUL"
        }
        testProperty "four printable characters survive a round trip" <| fun (a: char) (b: char) (c: char) (d: char) ->
            let text = printable [| a; b; c; d |]
            Tag.toString (Tag.ofString text) = text
        testProperty "only the first four bytes matter" <| fun (chars: char array) ->
            let text = printable chars
            Tag.ofString text = Tag.ofString (text.Substring(0, min Tag.Width text.Length))
    ]
