module Farfbuzz.Tests.AatMorxTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = SfntBuilder.be16 value

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) = SfntBuilder.be32 value

/// <summary>Builds a format-6 single lookup from glyph and value pairs, with the terminator.</summary>
/// <param name="entries">The glyph and value pairs.</param>
/// <returns>The lookup bytes.</returns>
let private singleLookup (entries: (uint32 * uint32) array) : byte array =
    let unitSize = 4u
    let nUnits = uint32 entries.Length + 1u
    let header = Array.concat [ be16 6u; be16 unitSize; be16 nUnits; be16 0u; be16 0u; be16 0u ]
    let units = entries |> Array.collect (fun (glyph, value) -> Array.concat [ be16 glyph; be16 value ])
    let terminator = Array.concat [ be16 0xFFFFu; be16 0u ]
    Array.concat [ header; units; terminator ]

/// <summary>Builds a noncontextual subtable: length, coverage, sub-feature flags, then the substitute lookup.</summary>
/// <param name="lookup">The lookup bytes.</param>
/// <returns>The subtable bytes.</returns>
let private noncontextualSubtable (lookup: byte array) : byte array =
    Array.concat [ be32 (uint32 (12 + lookup.Length)); be32 4u; be32 1u; lookup ]

/// <summary>Builds one chain from its subtables and an optional version-3 coverage table.</summary>
/// <param name="subtables">The subtable bytes.</param>
/// <param name="coverageOffsets">The coverage offsets, for version 3 and later.</param>
/// <returns>The chain bytes.</returns>
let private chainOf (subtables: byte array array) (coverageOffsets: uint32 array) : byte array =
    let subtableBytes = Array.concat subtables
    let coverageBytes = coverageOffsets |> Array.collect be32
    let length = uint32 (16 + subtableBytes.Length + coverageBytes.Length)
    Array.concat [ be32 1u; be32 length; be32 0u; be32 (uint32 subtables.Length); subtableBytes; coverageBytes ]

/// <summary>Builds a morx table from a version and its chains.</summary>
/// <param name="version">The morx version.</param>
/// <param name="chains">The chain bytes.</param>
/// <returns>The morx table bytes.</returns>
let private morxTable (version: uint16) (chains: byte array array) : byte array =
    Array.concat [ be16 (uint32 version); be16 0u; be32 (uint32 chains.Length); Array.concat chains ]

/// <summary>Builds a face from a morx table and extra tables, with the given glyph count.</summary>
/// <param name="morx">The morx table bytes.</param>
/// <param name="glyphCount">The face glyph count.</param>
/// <param name="extra">Extra tables, for example the blocklist's GSUB and GDEF.</param>
/// <returns>The face.</returns>
let private faceOf (morx: byte array) (glyphCount: int) (extra: (Tag * byte array) list) : OtFace =
    let tables = (Tags.Morx, morx) :: extra
    let file = sfnt 0x00010000u tables
    OtFace.create (Face.withGlyphCount glyphCount (Face.create (ReadOnlyMemory file) 0u))

/// <summary>Builds a run of the given code points, one cluster per glyph.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private runOf (codepoints: uint32 array) =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty |> TailCalls.settle

/// <summary>A morx with one chain holding a single noncontextual subtable whose lookup maps glyph 100 to 200.</summary>
/// <param name="glyphCount">The face glyph count.</param>
/// <returns>The face.</returns>
let private singleSubtableFace (glyphCount: int) : OtFace =
    let lookup = singleLookup [| (100u, 200u) |]
    faceOf (morxTable 1us [| chainOf [| noncontextualSubtable lookup |] [||] |]) glyphCount []

/// <summary>The P8.S06 morx/mort table and noncontextual unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S06" [
        test "sanitize rejects more than 1024 subtables in total" {
            let subtable = noncontextualSubtable (Array.concat [ be16 0u ])
            let accepted = faceOf (morxTable 1us [| chainOf [| subtable |] [||] |]) 0 []
            let morx = morxTable 1us [| chainOf (Array.init 1024 (fun _ -> subtable)) [||] |]
            let atLimit = faceOf morx 0 []
            Expect.equal (Morx.hasData (Morx.load accepted.Face false)) true "a single subtable loads"
            Expect.equal (Morx.hasData (Morx.load atLimit.Face false)) true "1024 subtables load"
            let over = morxTable 1us [| chainOf (Array.init 1025 (fun _ -> subtable)) [||] |]
            let overFace = faceOf over 0 []
            Expect.equal (Morx.hasData (Morx.load overFace.Face false)) false "1025 subtables are rejected"
        }
        test "blocklisted triple is rejected" {
            let padding = Array.zeroCreate<byte> (19884 - 16)
            let chain = Array.concat [ be32 0u; be32 19884u; be32 0u; be32 0u; padding ]
            let morx = Array.concat [ be16 1u; be16 0u; be32 1u; chain ]
            Expect.equal morx.Length 19892 "the morx table is 19892 bytes"
            let gsub = Array.zeroCreate<byte> 2794
            let gdef = Array.zeroCreate<byte> 340
            let face = faceOf morx 0 [ Tags.Gsub, gsub; Tags.Gdef, gdef ]
            Expect.equal (Morx.hasData (Morx.load face.Face false)) false "the blocklisted triple is rejected"
            let control = faceOf (morxTable 1us [| chainOf [||] [||] |]) 0 [ Tags.Gsub, gsub; Tags.Gdef, gdef ]
            Expect.equal (Morx.hasData (Morx.load control.Face false)) true "a non-blocklisted morx loads"
        }
        test "version 3 coverage offsets 0 and 0xFFFFFFFF are accepted" {
            let subtable = noncontextualSubtable (singleLookup [| (1u, 2u) |])
            let zeroOffset = morxTable 3us [| chainOf [| subtable |] [| 0u |] |]
            let maxOffset = morxTable 3us [| chainOf [| subtable |] [| 0xFFFFFFFFu |] |]
            Expect.equal (Morx.hasData (Morx.load (faceOf zeroOffset 16 []).Face false)) true "offset 0 is accepted"
            Expect.equal (Morx.hasData (Morx.load (faceOf maxOffset 16 []).Face false)) true "offset 0xFFFFFFFF is accepted"
        }
        test "a skipped subtable allocates nothing" {
            let face = singleSubtableFace 16
            let map = AatMap.defaults face { Direction = Direction.Ltr; Script = 0u<script>; Language = Language.NoLanguage }
            let run = runOf [| 0u |]
            let table = face.Morx.Value
            MorxApply.apply table map face run.SegmentProperties run |> ignore
            let measured () = MorxApply.apply table map face run.SegmentProperties run |> ignore
            Expect.equal (Allocation.bytesAllocatedBy measured) 0L "a non-intersecting subtable costs no allocation"
        }
        test "a noncontextual pass that changes nothing allocates nothing" {
            let face = singleSubtableFace 16
            let lookup = AatLookup.read (face.Morx.Value.Chains[0].Subtables[0].Body) 0 2
            let run = runOf [| 0u |]
            let context: AatContext = { Gdef = face.Gdef.Value }
            let struct (result, _) =
                MorxNoncontextual.apply lookup 16 ImmutableArray.Empty 1u context true run
            Expect.isTrue (obj.ReferenceEquals(result, run)) "an unchanged run is the same instance"
            let measured () = MorxNoncontextual.apply lookup 16 ImmutableArray.Empty 1u context true run |> ignore
            Expect.equal (Allocation.bytesAllocatedBy measured) 0L "an unchanged run allocates nothing"
        }
    ]
