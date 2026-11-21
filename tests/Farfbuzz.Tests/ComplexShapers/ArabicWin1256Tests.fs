module Farfbuzz.Tests.ComplexShapers.ArabicWin1256Tests

open System.Collections.Immutable
open Expecto
open Farfbuzz

/// <summary>Reads a big-endian uint16 from the table bytes.</summary>
/// <param name="bytes">The table bytes.</param>
/// <param name="offset">The byte offset.</param>
/// <returns>The value.</returns>
let private be16 (bytes: ImmutableArray<byte>) (offset: int) =
    (int bytes[offset] <<< 8) ||| int bytes[offset + 1]

/// <summary>The five manifest tags in source order (src/hb-ot-shaper-arabic-win1256.hh:199-203).</summary>
let private expectedTags =
    [ Tag.make 'r' 'l' 'i' 'g'
      Tag.make 'i' 'n' 'i' 't'
      Tag.make 'm' 'e' 'd' 'i'
      Tag.make 'f' 'i' 'n' 'a'
      Tag.make 'r' 'l' 'i' 'g' ]

/// <summary>The five manifest lookup types in source order (src/hb-ot-shaper-arabic-win1256.hh:209-230).</summary>
let private expectedTypes = [ 4; 1; 1; 1; 4 ]

/// <summary>The five manifest lookup flags in source order (src/hb-ot-shaper-arabic-win1256.hh:209-230).</summary>
let private expectedFlags = [ 8; 8; 8; 8; 0 ]

/// <summary>The P6.S08 Windows-1256 lookup case.</summary>
[<Tests>]
let tests =
    testList "P6.S08" [
        testCase "win1256 lookups" (fun () ->
            let manifest = ArabicWin1256.manifest
            Expect.equal manifest.Length 5 "manifest length"
            Expect.equal (manifest |> Seq.map (fun struct (tag, _) -> tag) |> Seq.toList) expectedTags "manifest tags"
            let actualTypes = manifest |> Seq.map (fun struct (_, offset) -> be16 ArabicWin1256.bytes offset) |> Seq.toList
            let actualFlags = manifest |> Seq.map (fun struct (_, offset) -> be16 ArabicWin1256.bytes (offset + 2)) |> Seq.toList
            Expect.equal actualTypes expectedTypes "lookup types"
            Expect.equal actualFlags expectedFlags "lookup flags")
    ]
