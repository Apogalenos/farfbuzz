module Farfbuzz.Tests.UtfTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz

/// <summary>The P3.S07 UTF and text-entry cases.</summary>
[<Tests>]
let tests =
    testList "P3.S07" [
        test "UTF-8 decodes and replaces ill-formed bytes" {
            Expect.equal (Utf.next8 (ReadOnlySpan [| 0x61uy; 0x62uy |]) 0 0xFFFDu) (struct (0x61u, 1)) "ASCII"
            Expect.equal (Utf.next8 (ReadOnlySpan [| 0xFFuy |]) 0 0xFFFDu) (struct (0xFFFDu, 1)) "bad lead"
            Expect.equal (Utf.next8 (ReadOnlySpan [| 0xE0uy; 0x80uy; 0x80uy |]) 0 0xFFFDu) (struct (0xFFFDu, 1)) "overlong"
        }
        test "UTF-16 and UTF-32 surrogate handling" {
            Expect.equal (Utf.next16 (ReadOnlySpan [| 0xD83Dus; 0xDE00us |]) 0 0xFFFDu) (struct (0x1F600u, 2)) "pair"
            Expect.equal (Utf.next16 (ReadOnlySpan [| 0xDE00us |]) 0 0xFFFDu) (struct (0xFFFDu, 1)) "lone"
            Expect.equal (Utf.next32 (ReadOnlySpan [| 0xD800u |]) 0 0xFFFDu true) (struct (0xFFFDu, 1)) "validated"
            Expect.equal (Utf.next32 (ReadOnlySpan [| 0xD800u |]) 0 0xFFFDu false) (struct (0xD800u, 1)) "unvalidated"
        }
        test "text entry appends clusters and contexts" {
            let run = GlyphRun.addUtf8 (ReadOnlySpan [| 0x61uy; 0x62uy; 0x63uy |]) 1 1 GlyphRun.empty
            Expect.equal (run.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 0x62u |] "items"
            Expect.equal (run.Infos[0].Cluster) 1u "cluster"
            Expect.equal (run.ContextBefore |> Seq.toArray) [| 0x61u |] "before"
            Expect.equal (run.ContextAfter |> Seq.toArray) [| 0x63u |] "after"
            Expect.equal run.ContentType ContentType.UnicodeContent "content"
        }
        test "Latin-1 and code-point entry" {
            let latin = GlyphRun.addLatin1 (ReadOnlySpan [| 0xE9uy |]) 0 1 GlyphRun.empty
            Expect.equal latin.Infos[0].Codepoint 0xE9u "Latin-1"
            let codepoints = GlyphRun.addCodepoints (ReadOnlySpan [| 0x110000u |]) 0 1 GlyphRun.empty
            Expect.equal codepoints.Infos[0].Codepoint 0x110000u "unvalidated"
            let added = GlyphRun.add 0x41u 7u GlyphRun.empty
            Expect.equal (added.Infos[0].Codepoint, added.Infos[0].Cluster) (0x41u, 7u) "add"
        }
        test "empty run and out-of-range item slices" {
            Expect.equal GlyphRun.empty.Infos.Length 0 "empty"
            let run = GlyphRun.addUtf8 (ReadOnlySpan [| 0x61uy |]) 9 99 GlyphRun.empty
            Expect.equal run.Infos.Length 0 "clamped"
            Expect.equal run.ContentType ContentType.UnicodeContent "content after add"
        }
    ]

/// <summary>The code points of a text by the checking decoder, one replacement for each ill-formed byte, as the loop of
/// <c>hb_buffer_add_utf8</c> reads them: the code point and the offset of its lead byte.</summary>
/// <param name="text">The text.</param>
/// <param name="at">The index to read from.</param>
/// <param name="stop">The end index.</param>
/// <returns>The code points with their offsets.</returns>
let rec private decodeChecking (text: byte array) (at: int) (stop: int) : (uint32 * int) list =
    match at >= stop with
    | true -> []
    | false ->
        let struct (value, next) = Utf.next8 (ReadOnlySpan(text, 0, stop)) at 0xFFFDu
        (value, at) :: decodeChecking text next stop

/// <summary>Encodes code points of every width, drops a byte or alters one now and then, so that well-formed texts,
/// texts cut inside a sequence and ill-formed texts all appear.</summary>
let private genMixedUtf8 : Gen<byte array * int * int> =
    gen {
        let! count = Gen.choose (0, 24)
        let! codepoints =
            Gen.listOfLength count (Gen.frequency
                [ 4, Gen.choose (0x20, 0x7E)
                  3, Gen.choose (0x80, 0x7FF)
                  3, Gen.choose (0x800, 0xD7FF)
                  1, Gen.choose (0xE000, 0xFFFF)
                  2, Gen.choose (0x10000, 0x10FFFF) ])
        let bytes = codepoints |> List.collect (fun codepoint -> Text.Rune(codepoint).ToString() |> Text.Encoding.UTF8.GetBytes |> Array.toList) |> List.toArray
        let! damage = Gen.frequency [ 6, Gen.constant 0; 2, Gen.constant 1; 1, Gen.constant 2 ]
        let! position = Gen.choose (0, max 0 (bytes.Length - 1))
        let! replacement = Gen.elements [ 0x80uy; 0xBFuy; 0xC0uy; 0xC1uy; 0xE0uy; 0xEDuy; 0xF4uy; 0xF5uy; 0xFFuy; 0x41uy ]
        let damaged =
            match damage, bytes.Length with
            | _, 0 -> bytes
            | 0, _ -> bytes
            | 1, _ -> Array.append bytes[.. position - 1] bytes[position + 1 ..]
            | _, _ -> Array.mapi (fun index value -> match index = position with | true -> replacement | false -> value) bytes
        let! offset = Gen.frequency [ 5, Gen.constant 0; 3, Gen.choose (0, damaged.Length) ]
        let! itemLength = Gen.frequency [ 5, Gen.constant -1; 3, Gen.choose (0, damaged.Length) ]
        return damaged, offset, itemLength
    }

/// <summary>The cases of the UTF-8 entry that decodes a well-formed item without checking.</summary>
[<Tests>]
let wellFormedTests =
    testList "UTF-8 entry of well-formed text" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 20000 } "addUtf8 reads every text as the checking decoder does, in every field of the run" (
            Prop.forAll (Arb.fromGen genMixedUtf8) (fun (bytes, offset, itemLength) ->
                let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) offset itemLength GlyphRun.empty
                let start = match offset < 0 with | true -> bytes.Length | false -> min offset bytes.Length
                let length = max 0 (min (match itemLength < 0 with | true -> bytes.Length - start | false -> itemLength) (bytes.Length - start))
                let expected = decodeChecking bytes start (start + length)
                let actual = [ for info in run.Infos -> info.Codepoint, int info.Cluster ]
                actual = expected
                && run.Infos.Length = run.Props.Length
                && (run.Infos |> Seq.forall (fun info -> info.Mask = 0u && info.Var1 = 0u && info.Var2 = 0u))))
    ]
