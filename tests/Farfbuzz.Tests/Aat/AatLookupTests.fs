module Farfbuzz.Tests.AatLookupTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.Tests.SfntBuilder

/// <summary>Reads a lookup from bytes and returns it as a value.</summary>
/// <param name="bytes">The lookup bytes.</param>
/// <param name="valueSize">The value width.</param>
/// <returns>The lookup.</returns>
let private lookupOf (bytes: byte array) (valueSize: int) =
    AatLookup.read (ReadOnlyMemory bytes) 0 valueSize

/// <summary>The expected range list, as an immutable array of ranges.</summary>
/// <param name="ranges">The first and last glyphs.</param>
/// <returns>The ranges.</returns>
let private ranges (ranges: (uint32 * uint32) list) =
    ImmutableArray.CreateRange (List.toArray (List.map (fun (first, last) -> { First = first; Last = last }) ranges))

/// <summary>The filter that keeps only the value 90.</summary>
[<Struct>]
type private Keep90 =
    interface IValueFilter with
        member _.Filter value = value = 90u

/// <summary>The P8.S01 lookup and class-table unit tests, on hand-built bytes.</summary>
[<Tests>]
let tests =
    testList "P8.S01" [
        test "format 0 reads the array by glyph index" {
            let bytes = Array.concat [ be16 0u; be16 10u; be16 20u; be16 30u ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.valueAt lookup 1u 3) (ValueSome 4) "value at glyph 1"
            Expect.equal (AatLookup.valueOrNull lookup 1u 3) 20u "value of glyph 1"
            Expect.equal (AatLookup.valueOrNull lookup 5u 3) 0u "out-of-range glyph"
            Expect.equal (AatLookup.classOf lookup 2u 3 9us) 30us "class of glyph 2"
            Expect.equal (AatLookup.collectGlyphs lookup 3) (ranges [ 0u, 2u ]) "collected range"
        }
        test "format 2 searches segments" {
            let bytes =
                Array.concat
                    [ be16 2u; be16 6u; be16 3u; be16 0u; be16 0u; be16 0u
                      be16 10u; be16 5u; be16 100u
                      be16 20u; be16 20u; be16 200u
                      be16 0xFFFFu; be16 0xFFFFu; be16 0u ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.valueOrNull lookup 7u 100) 100u "inside the first segment"
            Expect.equal (AatLookup.valueOrNull lookup 15u 100) 0u "between segments"
            Expect.equal (AatLookup.valueOrNull lookup 20u 100) 200u "inside the last segment"
            Expect.equal (AatLookup.collectGlyphs lookup 100) (ranges [ 5u, 10u; 20u, 20u ]) "collected ranges"
        }
        test "format 4 searches segments and their value arrays" {
            let bytes =
                Array.concat
                    [ be16 4u; be16 6u; be16 2u; be16 0u; be16 0u; be16 0u
                      be16 4u; be16 2u; be16 24u
                      be16 0xFFFFu; be16 0xFFFFu; be16 0u
                      be16 10u; be16 20u; be16 30u ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.valueAt lookup 3u 100) (ValueSome 26) "value at glyph 3"
            Expect.equal (AatLookup.valueOrNull lookup 3u 100) 20u "value of glyph 3"
            Expect.equal (AatLookup.valueOrNull lookup 1u 100) 0u "below the first glyph"
            Expect.equal (AatLookup.valueOrNull lookup 5u 100) 0u "above the last glyph"
            Expect.equal (AatLookup.collectGlyphs lookup 100) (ranges [ 2u, 4u ]) "collected range"
        }
        test "format 6 searches singles" {
            let bytes =
                Array.concat
                    [ be16 6u; be16 4u; be16 3u; be16 0u; be16 0u; be16 0u
                      be16 5u; be16 50u
                      be16 9u; be16 90u
                      be16 0xFFFFu; be16 0u ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.valueOrNull lookup 5u 100) 50u "value of glyph 5"
            Expect.equal (AatLookup.valueOrNull lookup 6u 100) 0u "missing glyph"
            Expect.equal (AatLookup.collectGlyphs lookup 100) (ranges [ 5u, 5u; 9u, 9u ]) "collected glyphs"
        }
        test "format 8 reads the trimmed array" {
            let bytes = Array.concat [ be16 8u; be16 10u; be16 3u; be16 1u; be16 2u; be16 3u ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.valueOrNull lookup 11u 100) 2u "value of glyph 11"
            Expect.equal (AatLookup.valueOrNull lookup 9u 100) 0u "below the first glyph"
            Expect.equal (AatLookup.collectGlyphs lookup 100) (ranges [ 10u, 12u ]) "collected range"
        }
        test "format 10 reads byte values" {
            let bytes = Array.concat [ be16 10u; be16 2u; be16 100u; be16 2u; [| 0x01uy; 0x02uy; 0x03uy; 0x04uy |] ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.valueAt lookup 100u 200) ValueNone "get_value returns no pointer for format 10"
            Expect.equal (AatLookup.valueOrNull lookup 100u 200) 0x0102u "byte value of glyph 100"
            Expect.equal (AatLookup.valueOrNull lookup 101u 200) 0x0304u "byte value of glyph 101"
            Expect.equal (AatLookup.valueOrNull lookup 99u 200) 0u "below the first glyph"
            Expect.equal (AatLookup.classOf lookup 100u 200 7us) 7us "class falls back to out-of-range"
            Expect.equal (AatLookup.collectGlyphs lookup 200) (ranges [ 100u, 101u ]) "collected range"
        }
        test "a trailing terminator unit is dropped" {
            let bytes =
                Array.concat
                    [ be16 6u; be16 4u; be16 2u; be16 0u; be16 0u; be16 0u
                      be16 10u; be16 50u
                      be16 0xFFFFu; be16 0u ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.valueOrNull lookup 10u 100) 50u "the only single"
            Expect.equal (AatLookup.valueOrNull lookup 0xFFFFu 100) 0u "the terminator is not a single"
            Expect.equal (AatLookup.collectGlyphs lookup 100) (ranges [ 10u, 10u ]) "one collected glyph"
        }
        test "obsolete class table with a glyph below firstGlyph wraps to out-of-range" {
            let bytes = Array.concat [ be16 5u; be16 3u; [| 1uy; 2uy; 3uy |] ]
            let table = { ClassTable.Table = ReadOnlyMemory bytes; At = 0; FirstGlyph = 5us; Count = 3; Wide = false }
            Expect.equal (ClassTable.classOfCall table 5u 9us) 1us "first class"
            Expect.equal (ClassTable.classOfCall table 7u 9us) 3us "last class"
            Expect.equal (ClassTable.classOfCall table 4u 9us) 9us "below firstGlyph wraps"
            Expect.equal (ClassTable.classOfCall table 8u 9us) 9us "above the array"
            Expect.equal (ClassTable.collectGlyphs table) (ranges [ 6u, 7u ]) "classes other than out-of-bounds"
        }
        test "collectGlyphsFiltered keeps only the selected values" {
            let bytes =
                Array.concat
                    [ be16 6u; be16 4u; be16 3u; be16 0u; be16 0u; be16 0u
                      be16 5u; be16 50u
                      be16 9u; be16 90u
                      be16 0xFFFFu; be16 0u ]
            let lookup = lookupOf bytes 2
            Expect.equal (AatLookup.collectGlyphsFiltered lookup 100 (Keep90())) (ranges [ 9u, 9u ]) "only glyph 9"
        }
    ]
