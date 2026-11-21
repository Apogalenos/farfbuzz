module Farfbuzz.Tests.GlyphNameTests

open System
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Creates a font from a test font.</summary>
/// <param name="relative">The path under <c>test/api/fonts</c>.</param>
/// <returns>The font.</returns>
let private fontOf (relative: string) =
    Font.create (Face.create (read $"test/api/fonts/{relative}") 0u)

/// <summary>Font glyph names and strings, P2.S11.</summary>
[<Tests>]
let tests =
    testList "P2.S11" [
        test "post names are returned by Font.glyphName" {
            let font = fontOf "adwaita.ttf"
            Expect.equal (Font.glyphName font 0u) (ValueSome ".notdef") "glyph 0"
            Expect.equal (Font.glyphName font 1u) (ValueSome ".space") "glyph 1"
            Expect.equal (Font.glyphName font 2u) (ValueSome "icon0") "glyph 2"
            Expect.equal (Font.glyphName font 100u) ValueNone "beyond last glyph"
        }
        test "CFF names fall through after post" {
            let font = fontOf "SourceSansPro-Regular.otf"
            Expect.equal (Font.glyphName font 0u) (ValueSome ".notdef") "glyph 0"
            Expect.equal (Font.glyphName font 1u) (ValueSome "space") "glyph 1"
            Expect.equal (Font.glyphName font 2u) (ValueSome "A") "glyph 2"
            Expect.equal (Font.glyphName font 2000u) ValueNone "beyond last glyph"
        }
        test "glyphToString falls back to gidN" {
            let font = fontOf "adwaita.ttf"
            Expect.equal (Font.glyphToString font 0u) ".notdef" "named"
            Expect.equal (Font.glyphToString font 100u) "gid100" "unnamed"
        }
        test "glyphToString truncates names to 127 bytes" {
            let pool =
                Array.concat
                    [ [| 200uy |]
                      Array.create 200 97uy ]
            let post =
                Array.concat
                    [ be32 0x00020000u
                      Array.zeroCreate 28
                      be16 1u
                      be16 258u
                      pool ]
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Post, post ])) 0u
            let font = Font.create face
            Expect.equal (Font.glyphName font 0u) (ValueSome (String('a', 200))) "full name"
            Expect.equal (Font.glyphToString font 0u).Length 127 "truncated"
        }
        test "glyphFromString resolves names, decimals, gidN and uniXXXX" {
            let font = fontOf "adwaita.ttf"
            Expect.equal (Font.glyphFromString font ".space") (ValueSome 1u) "name"
            Expect.equal (Font.glyphFromString font "65") (ValueSome 65u) "decimal"
            Expect.equal (Font.glyphFromString font "gid5") (ValueSome 5u) "gid"
            Expect.equal (Font.glyphFromString font "nosuchname") ValueNone "absent"
            let letters = fontOf "Roboto-Regular.abc.ttf"
            Expect.equal (Font.glyphFromString letters "uni0061") (Font.nominalGlyph letters 0x61u) "uni"
        }
        test "the empty font has no names but still parses gid0" {
            let font = Font.create Face.empty
            Expect.equal (Font.glyphName font 0u) ValueNone "name"
            Expect.equal (Font.glyphToString font 0u) "gid0" "string"
            Expect.equal (Font.glyphFromString font "0") (ValueSome 0u) "decimal zero"
        }
    ]

/// <summary>The P2.R05 byte-lossless name cases.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R05" [
        test "a post name with byte 0xE9 round-trips losslessly" {
            let pool = [| 2uy; 0xE9uy; 0x41uy |]
            let post =
                Array.concat
                    [ be32 0x00020000u; Array.zeroCreate 28; be16 1u; be16 258u; pool ]
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Post, post ])) 0u
            let font = Font.create face
            match Font.glyphName font 0u with
            | ValueSome name ->
                Expect.equal name "\u00E9A" "name"
                Expect.equal (Font.glyphFromName font name) (ValueSome 0u) "lookup"
            | ValueNone -> Expect.isTrue false "name missing"
        }
    ]
