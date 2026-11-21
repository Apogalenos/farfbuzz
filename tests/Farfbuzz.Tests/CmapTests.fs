module Farfbuzz.Tests.CmapTests

open System
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format 0 subtable.</summary>
/// <param name="glyphs">The 256 glyph ids.</param>
/// <returns>The bytes.</returns>
let private format0 (glyphs: byte array) : byte array =
    Array.concat [ be16 0u; be16 262u; be16 0u; glyphs ]

/// <summary>Builds a format 12 subtable.</summary>
/// <param name="groups">Start, end and glyph per group.</param>
/// <returns>The bytes.</returns>
let private format12 (groups: (uint32 * uint32 * uint32) list) : byte array =
    let body =
        groups
        |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ])
        |> Array.concat
    Array.concat [ be16 12u; be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]

/// <summary>Builds a cmap table from sorted encoding records.</summary>
/// <param name="records">Platform, encoding and subtable bytes.</param>
/// <returns>The cmap bytes.</returns>
let private cmapTable (records: (uint16 * uint16 * byte array) list) : byte array =
    let sorted = records |> List.sortBy (fun (platform, encoding, _) -> platform, encoding)
    let headerSize = 4 + 8 * sorted.Length
    let offsets =
        sorted
        |> List.fold (fun (offsets, offset) (_, _, bytes) ->
            (offset :: offsets, offset + bytes.Length)) ([], headerSize)
        |> fst
        |> List.rev
    let recordBytes =
        List.map2 (fun (platform, encoding, _) offset ->
            Array.concat [ be16 (uint32 platform); be16 (uint32 encoding); be32 (uint32 offset) ]) sorted offsets
        |> Array.concat
    let subtables = sorted |> List.map (fun (_, _, bytes) -> bytes) |> Array.concat
    Array.concat [ be16 0u; be16 (uint32 sorted.Length); recordBytes; subtables ]

/// <summary>Builds a face with one cmap table.</summary>
/// <param name="records">The encoding records.</param>
/// <returns>The face.</returns>
let private faceWith (records: (uint16 * uint16 * byte array) list) =
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmapTable records ])) 0u

/// <summary>Builds an OS/2 value with a font page.</summary>
/// <param name="page">The font page.</param>
/// <returns>The OS/2 value.</returns>
let private os2WithPage (page: uint16) : Os2 =
    { Present = true
      Version = 0us
      FsSelection = page
      TypoAscender = 0s
      TypoDescender = 0s
      TypoLineGap = 0s
      WinAscent = 0us
      WinDescent = 0us
      SxHeight = 0s
      CapHeight = 0s
      SubscriptXSize = 0s
      SubscriptYSize = 0s
      SubscriptXOffset = 0s
      SubscriptYOffset = 0s
      SuperscriptXSize = 0s
      SuperscriptYSize = 0s
      SuperscriptXOffset = 0s
      SuperscriptYOffset = 0s
      StrikeoutSize = 0s
      StrikeoutPosition = 0s }

/// <summary>Cmap selection and nominal glyph lookup, P2.S06.</summary>
[<Tests>]
let tests =
    testList "P2.S06" [
        test "symbol (3,0) beats the 32-bit subtables" {
            let symbol = format12 [ 0xF041u, 0xF041u, 1u ]
            let wide = format12 [ 0x41u, 0x41u, 2u ]
            let face = faceWith [ 3us, 0us, symbol; 3us, 10us, wide ]
            let cmap = Cmap.load face (os2WithPage 0us)
            Expect.isTrue cmap.Present "present"
            Expect.equal (Cmap.nominalGlyph cmap 0xF041u) (ValueSome 1u) "symbol"
            Expect.equal (Cmap.nominalGlyph cmap 0x41u) (ValueSome 1u) "symbol is selected"
        }
        test "symbol PUA fallback maps low code points" {
            let glyphs = format12 [ 0xF041u, 0xF041u, 5u ]
            let face = faceWith [ 3us, 0us, glyphs ]
            let cmap = Cmap.load face (os2WithPage 0us)
            Expect.equal (Cmap.nominalGlyph cmap 0xF041u) (ValueSome 5u) "direct"
            Expect.equal (Cmap.nominalGlyph cmap 0x41u) (ValueSome 5u) "remapped"
        }
        test "MacRoman maps U+2026 through the generated table" {
            let glyphs = Array.zeroCreate 256
            glyphs[0xC9] <- 7uy
            let face = faceWith [ 1us, 0us, format0 glyphs ]
            let cmap = Cmap.load face (os2WithPage 0us)
            Expect.equal cmap.Remap MacRoman "remap"
            Expect.equal (Cmap.nominalGlyph cmap 0x2026u) (ValueSome 7u) "ellipsis"
        }
        test "the Mac encoding wildcard selects any Mac subtable as ASCII" {
            let glyphs = Array.zeroCreate 256
            glyphs[0x41] <- 3uy
            let face = faceWith [ 1us, 7us, format0 glyphs ]
            let cmap = Cmap.load face (os2WithPage 0us)
            Expect.equal cmap.Remap MacAscii "remap"
            Expect.equal (Cmap.nominalGlyph cmap 0x41u) (ValueSome 3u) "ASCII"
            Expect.equal (Cmap.nominalGlyph cmap 0x80u) ValueNone "non-ASCII"
        }
        test "the simplified Arabic page remaps through the PUA table" {
            let glyphs = format12 [ 0xF1ADu, 0xF1ADu, 9u ]
            let face = faceWith [ 3us, 0us, glyphs ]
            let cmap = Cmap.load face (os2WithPage 0xB200us)
            Expect.equal cmap.Remap SymbolArabicSimplified "remap"
            Expect.equal (Cmap.nominalGlyph cmap 0x0621u) (ValueSome 9u) "hamza"
        }
        test "a surviving subtable is preferred in the documented order" {
            let low = Array.zeroCreate 256
            low[0x41] <- 4uy
            let high = Array.zeroCreate 256
            high[0x41] <- 8uy
            let face = faceWith [ 0us, 4us, format0 low; 3us, 1us, format0 high ]
            let cmap = Cmap.load face (os2WithPage 0us)
            Expect.equal (Cmap.nominalGlyph cmap 0x41u) (ValueSome 4u) "0,4 before 3,1"
        }
        test "an absent cmap has no glyphs" {
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [])) 0u
            let cmap = Cmap.load face (os2WithPage 0us)
            Expect.isFalse cmap.Present "not present"
            Expect.equal (Cmap.nominalGlyph cmap 0x41u) ValueNone "miss"
        }
    ]
