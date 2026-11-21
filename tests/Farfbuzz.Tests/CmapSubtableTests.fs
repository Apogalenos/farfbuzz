module Farfbuzz.Tests.CmapSubtableTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format 0 subtable.</summary>
/// <param name="glyphs">The 256 glyph ids.</param>
/// <returns>The bytes.</returns>
let private format0 (glyphs: byte array) : byte array =
    Array.concat [ be16 0u; be16 262u; be16 0u; glyphs ]

/// <summary>Builds a format 4 subtable.</summary>
/// <param name="segments">Start, end, delta and range offset per segment.</param>
/// <param name="glyphIds">The glyph id array.</param>
/// <returns>The bytes.</returns>
let private format4 (segments: (int * int * int * int) list) (glyphIds: uint16 list) : byte array =
    let count = segments.Length
    let header =
        Array.concat
            [ be16 4u; be16 0u; be16 0u; be16 (uint32 (2 * count)); be16 0u; be16 0u; be16 0u ]
    let ends = segments |> List.map (fun (_, last, _, _) -> be16 (uint32 last)) |> Array.concat
    let starts = segments |> List.map (fun (first, _, _, _) -> be16 (uint32 first)) |> Array.concat
    let deltas = segments |> List.map (fun (_, _, delta, _) -> be16 (uint32 (int16 delta))) |> Array.concat
    let offsets = segments |> List.map (fun (_, _, _, offset) -> be16 (uint32 offset)) |> Array.concat
    let glyphs = glyphIds |> List.map (fun glyph -> be16 (uint32 glyph)) |> Array.concat
    Array.concat [ header; ends; be16 0u; starts; deltas; offsets; glyphs ]

/// <summary>Builds a trimmed format 6 or 10 subtable.</summary>
/// <param name="format">6 or 10.</param>
/// <param name="first">The first code point.</param>
/// <param name="glyphs">The glyph array.</param>
/// <returns>The bytes.</returns>
let private trimmed (format: uint32) (first: uint32) (glyphs: uint16 list) : byte array =
    let array = glyphs |> List.map (fun glyph -> be16 (uint32 glyph)) |> Array.concat
    match format with
    | 6u -> Array.concat [ be16 6u; be16 (uint32 (10 + array.Length)); be16 0u; be16 first; be16 (uint32 glyphs.Length); array ]
    | _ ->
        Array.concat
            [ be16 10u; be16 0u; be32 (uint32 (20 + array.Length)); be32 0u; be32 first
              be32 (uint32 glyphs.Length); array ]

/// <summary>Builds a format 12 or 13 subtable.</summary>
/// <param name="format">12 or 13.</param>
/// <param name="groups">Start, end and glyph per group.</param>
/// <returns>The bytes.</returns>
let private longSegmented (format: uint16) (groups: (uint32 * uint32 * uint32) list) : byte array =
    let body =
        groups
        |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ])
        |> Array.concat
    Array.concat [ be16 (uint32 format); be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]

/// <summary>Builds a format 14 or 15 subtable with one selector record.</summary>
/// <param name="format">14 or 15.</param>
/// <param name="selector">The variation selector.</param>
/// <param name="defaultRange">The default range, start and additional count.</param>
/// <param name="nonDefault">The non-default mapping, code point and glyph.</param>
/// <returns>The bytes.</returns>
let private uvs (format: uint16) (selector: uint32) (defaultRange: uint32 * byte) (nonDefault: uint32 * uint32) : byte array =
    let recordSize = match format with | 15us -> 6 | _ -> 5
    let headerSize = 10 + 11
    let defaultTable =
        let first, additional = defaultRange
        Array.concat [ be32 1u; be24 first; [| additional |] ]
    let defaultOffset = uint32 headerSize
    let nonDefaultOffset = uint32 (headerSize + defaultTable.Length)
    let nonDefaultTable =
        let codepoint, glyph = nonDefault
        match recordSize with
        | 6 -> Array.concat [ be32 1u; be24 codepoint; be24 glyph ]
        | _ -> Array.concat [ be32 1u; be24 codepoint; be16 glyph ]
    let record =
        Array.concat
            [ be24 selector; be32 defaultOffset; be32 nonDefaultOffset ]
    let total = headerSize + defaultTable.Length + nonDefaultTable.Length
    Array.concat [ be16 (uint32 format); be32 (uint32 total); be32 1u; record; defaultTable; nonDefaultTable ]

/// <summary>Sanitizes a standalone subtable with a fresh budget.</summary>
/// <param name="bytes">The subtable bytes.</param>
/// <returns>Whether it passes.</returns>
let private sane (bytes: byte array) =
    ValueOption.isSome (CmapSubtable.sanitize (ReadOnlySpan bytes) 0 (Sanitize.start bytes.Length))

/// <summary>The linear model for the format 4 property.</summary>
/// <param name="segments">The segments.</param>
/// <param name="codepoint">The query.</param>
/// <returns>The glyph, or none.</returns>
let private model4 (segments: (int * int * int) list) (codepoint: uint32) =
    segments
    |> List.tryFind (fun (first, last, _) -> uint32 first <= codepoint && codepoint <= uint32 last)
    |> Option.bind (fun (_, _, delta) ->
        let gid = (codepoint + uint32 delta) &&& 0xFFFFu
        match gid with | 0u -> None | _ -> Some gid)
    |> ValueOption.ofOption

/// <summary>Cmap subtable formats and lookups, P2.S04.</summary>
[<Tests>]
let tests =
    testList "P2.S04" [
        test "format 0 maps bytes below 256" {
            let glyphs = Array.zeroCreate 256
            glyphs[0x41] <- 3uy
            let bytes = format0 glyphs
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x41u) (ValueSome 3u) "mapped"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x100u) ValueNone "out of range"
        }
        test "format 4 uses idDelta" {
            let bytes = format4 [ 0x41, 0x43, 1, 0 ] []
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x41u) (ValueSome 0x42u) "first"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x44u) ValueNone "outside"
        }
        test "format 4 uses idRangeOffset" {
            let bytes = format4 [ 0x41, 0x43, 0, 2 ] [ 5us; 6us; 7us ]
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x41u) (ValueSome 5u) "first"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x43u) (ValueSome 7u) "last"
        }
        test "format 4 ignores a lying length field" {
            let bytes = format4 [ 0x41, 0x43, 0, 2 ] [ 5us; 6us; 7us ]
            let lying = Array.copy bytes
            lying[2] <- 0uy
            lying[3] <- 14uy
            let table = CmapSubtable.classify (ReadOnlySpan lying) 0
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan lying) table 0x42u) (ValueSome 6u) "data size wins"
        }
        test "format 6 wraps the code point subtraction" {
            let bytes = trimmed 6u 0x100u [ 9us; 10us ]
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x101u) (ValueSome 10u) "second"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x50u) ValueNone "wrapped out"
        }
        test "format 10 is the 32-bit trimmed form" {
            let bytes = trimmed 10u 0x10000u [ 3us; 4us ]
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x10001u) (ValueSome 4u) "second"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x10002u) ValueNone "past"
        }
        test "format 12 maps and rejects a reversed group" {
            let bytes = longSegmented 12us [ 0x10u, 0x12u, 5u; 0x20u, 0x1Fu, 1u ]
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x11u) (ValueSome 6u) "mapped"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x20u) ValueNone "reversed"
        }
        test "format 13 repeats the group glyph and rejects zero" {
            let bytes = longSegmented 13us [ 0x10u, 0x12u, 5u; 0x20u, 0x22u, 0u ]
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x12u) (ValueSome 5u) "repeated"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan bytes) table 0x21u) ValueNone "zero glyph"
        }
        test "formats 2 and 8 are ignored" {
            let two = Array.concat [ be16 2u; be16 0u ]
            let eight = Array.concat [ be16 8u; be16 0u ]
            Expect.equal (CmapSubtable.classify (ReadOnlySpan two) 0) (Ignored 2us) "format 2"
            Expect.equal (CmapSubtable.classify (ReadOnlySpan eight) 0) (Ignored 8us) "format 8"
            Expect.isTrue (sane two) "format 2 sanitizes"
            Expect.isTrue (sane eight) "format 8 sanitizes"
            Expect.equal (CmapSubtable.glyph (ReadOnlySpan two) (Ignored 2us) 0x41u) ValueNone "no glyph"
        }
        test "the null subtable misses" {
            Expect.equal (CmapSubtable.glyph ReadOnlySpan<byte>.Empty NullSubtable 0x41u) ValueNone "glyph"
            Expect.equal (CmapSubtable.variant ReadOnlySpan<byte>.Empty NullSubtable 0x41u 0xFE0Fu) NotFound "variant"
        }
        test "format 14 returns default, found and missing variants" {
            let bytes = uvs 14us 0xFE0Fu (0x41u, 1uy) (0x43u, 7u)
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.variant (ReadOnlySpan bytes) table 0x41u 0xFE0Fu) UseDefault "default"
            Expect.equal (CmapSubtable.variant (ReadOnlySpan bytes) table 0x43u 0xFE0Fu) (Found 7u) "found"
            Expect.equal (CmapSubtable.variant (ReadOnlySpan bytes) table 0x44u 0xFE0Fu) NotFound "missing"
            Expect.equal (CmapSubtable.variant (ReadOnlySpan bytes) table 0x41u 0xFE0Eu) NotFound "selector"
        }
        test "format 15 uses a 24-bit non-default glyph" {
            let bytes = uvs 15us 0xE0100u (0x41u, 0uy) (0x42u, 0x10001u)
            let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
            Expect.isTrue (sane bytes) "sanitize"
            Expect.equal (CmapSubtable.variant (ReadOnlySpan bytes) table 0x42u 0xE0100u) (Found 0x10001u) "found"
        }
        testProperty "format 4 lookup equals a linear model" <|
            let startGen = Gen.choose (0, 0xFFFE) |> Gen.map uint16
            let deltaGen = Gen.choose (-1000, 1000)
            let inputGen =
                gen {
                    let! starts = Gen.listOf startGen
                    let! delta = deltaGen
                    let! codepoint = Gen.choose (0, 0xFFFF) |> Gen.map uint32
                    return starts, delta, codepoint
                }
            Prop.forAll (Arb.fromGen inputGen) (fun (starts, delta, codepoint) ->
                let segments =
                    starts
                    |> List.distinct
                    |> List.sort
                    |> List.map (fun start -> int start, int start, delta)
                let bytes = format4 (segments |> List.map (fun (first, last, d) -> first, last, d, 0)) []
                let table = CmapSubtable.classify (ReadOnlySpan bytes) 0
                CmapSubtable.glyph (ReadOnlySpan bytes) table codepoint = model4 segments codepoint)
    ]
