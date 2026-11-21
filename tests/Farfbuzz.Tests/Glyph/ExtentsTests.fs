module Farfbuzz.Tests.Glyph.ExtentsTests

open System
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Builds a valid 54-byte head table.</summary>
/// <param name="upem">Units per em.</param>
/// <param name="indexToLocFormat">0 for short loca, 1 for long.</param>
/// <returns>The bytes.</returns>
let private head (upem: int) (indexToLocFormat: uint32) : byte array =
    Array.concat
        [ be32 0x00010000u
          be32 0x00010000u
          be32 0u
          be32 0x5F0F3CF5u
          be16 0u
          be16 (uint32 upem)
          be32 0u; be32 0u
          be32 0u; be32 0u
          be16 0u; be16 0u
          be16 0u; be16 0u
          be16 0u
          be16 0u
          be16 0u
          be16 indexToLocFormat
          be16 0u ]

/// <summary>Builds a version 0.5 maxp table.</summary>
/// <param name="glyphCount">The number of glyphs.</param>
/// <returns>The bytes.</returns>
let private maxp (glyphCount: int) : byte array =
    Array.concat [ be32 0x00005000u; be16 (uint32 glyphCount) ]

/// <summary>Builds one 10-byte glyph header.</summary>
/// <param name="contours">The number of contours.</param>
/// <param name="xMin">The minimum x.</param>
/// <param name="yMin">The minimum y.</param>
/// <param name="xMax">The maximum x.</param>
/// <param name="yMax">The maximum y.</param>
/// <returns>The bytes.</returns>
let private glyphHeader (contours: int) (xMin: int) (yMin: int) (xMax: int) (yMax: int) : byte array =
    Array.concat
        [ be16 (uint32 (contours &&& 0xFFFF))
          be16 (uint32 (xMin &&& 0xFFFF))
          be16 (uint32 (yMin &&& 0xFFFF))
          be16 (uint32 (xMax &&& 0xFFFF))
          be16 (uint32 (yMax &&& 0xFFFF)) ]

/// <summary>Builds a short-format loca table from raw byte offsets.</summary>
/// <param name="offsets">The byte offsets.</param>
/// <returns>The bytes.</returns>
let private shortLoca (offsets: int array) : byte array =
    offsets |> Array.collect (fun offset -> be16 (uint32 (offset / 2)))

/// <summary>Builds a 36-byte hea table.</summary>
/// <param name="longMetricCount">The long-metric count.</param>
/// <returns>The bytes.</returns>
let private hea (longMetricCount: int) : byte array =
    Array.concat
        [ be32 0x00010000u
          be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u
          be16 0u; be16 0u; be16 0u
          be16 0u; be16 0u; be16 0u; be16 0u
          be16 0u
          be16 (uint32 longMetricCount) ]

/// <summary>Builds an hmtx table from advance and bearing pairs.</summary>
/// <param name="metrics">The pairs.</param>
/// <returns>The bytes.</returns>
let private hmtx (metrics: (int * int) array) : byte array =
    metrics |> Array.collect (fun (advance, bearing) -> Array.concat [ be16 (uint32 advance); be16 (uint32 (bearing &&& 0xFFFF)) ])

/// <summary>A two-glyph glyf font: glyph 0 empty, glyph 1 with bounds (10, 20, 100, 200).</summary>
/// <param name="metrics">The hmtx metrics, or none to omit hmtx.</param>
/// <returns>The face.</returns>
let private glyfFace (metrics: (int * int) array voption) : Face =
    let glyf = Array.concat [ glyphHeader 0 0 0 0 0; glyphHeader 1 10 20 100 200 ]
    let tables =
        [ Tags.Head, head 1000 0u
          Tags.Maxp, maxp 2
          Tags.Loca, shortLoca [| 0; 10; 20 |]
          Tags.Glyf, glyf ]
    let tables =
        match metrics with
        | ValueNone -> tables
        | ValueSome pairs -> tables @ [ Tags.Hhea, hea pairs.Length; Tags.Hmtx, hmtx pairs ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u tables)) 0u

/// <summary>A face with a VORG table: default 880, glyph 2 at 600, glyph 5 at 700.</summary>
/// <returns>The face.</returns>
let private vorgFace () : Face =
    let records =
        [| 2u, 600s; 5u, 700s |]
        |> Array.collect (fun (glyph, y) -> Array.concat [ be16 glyph; be16 (uint32 (int y &&& 0xFFFF)) ])
    let vorg = Array.concat [ be32 0x00010000u; be16 (uint32 (880 &&& 0xFFFF)); be16 2u; records ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Head, head 1000 0u; Tags.Maxp, maxp 10; Tags.Vorg, vorg ])) 0u

/// <summary>A glyf face with vertical metrics, so its v origin needs the phantom point.</summary>
/// <returns>The face.</returns>
let private glyfVmtxFace () : Face =
    let glyf = Array.concat [ glyphHeader 0 0 0 0 0; glyphHeader 1 10 20 100 200 ]
    Face.create
        (ReadOnlyMemory
            (sfnt 0x00010000u
                [ Tags.Head, head 1000 0u
                  Tags.Maxp, maxp 2
                  Tags.Loca, shortLoca [| 0; 10; 20 |]
                  Tags.Glyf, glyf
                  Tags.Vhea, hea 2
                  Tags.Vmtx, hmtx [| 500, 0; 600, 30 |] ])) 0u

/// <summary>The P4.S07 glyph-extents and vertical-origin tests.</summary>
[<Tests>]
let tests =
    testList "P4.S07" [
        test "VORG returns the default and the per-glyph overrides" {
            let vorg = Vorg.load (vorgFace ())
            Expect.isTrue vorg.Present "VORG present"
            Expect.equal (Vorg.yOrigin vorg 0u) 880 "default for an unlisted glyph"
            Expect.equal (Vorg.yOrigin vorg 2u) 600 "per-glyph override"
            Expect.equal (Vorg.yOrigin vorg 5u) 700 "second per-glyph override"
            Expect.equal (Vorg.yOrigin vorg 3u) 880 "default between overrides"
        }
        test "an empty glyf glyph has zero extents" {
            let font = Font.create (glyfFace ValueNone)
            match Extents.glyph font 0u with
            | Ok (ValueSome extents) ->
                Expect.equal (extents.XBearing, extents.YBearing, extents.Width, extents.Height) (0, 0, 0, 0) "zeros"
            | Ok ValueNone -> failtest "expected extents for the empty glyph"
            | Error error -> failtest $"expected extents, got {error}"
        }
        test "the leading bearing is zero when hmtx has no data" {
            let noHmtx = Font.create (glyfFace ValueNone)
            let withHmtx = Font.create (glyfFace (ValueSome [| 500, 0; 600, 30 |]))
            match Extents.glyph noHmtx 1u, Extents.glyph withHmtx 1u with
            | Ok (ValueSome bare), Ok (ValueSome hmtx) ->
                Expect.equal bare.XBearing 0 "no hmtx: leading bearing is zero"
                Expect.equal hmtx.XBearing 30 "hmtx: leading bearing read from the table"
                Expect.equal (bare.YBearing, bare.Width, bare.Height) (200, 90, -180) "bounds from the header"
            | _ -> failtest "expected extents for glyph 1"
        }
        test "extents scale with the font size" {
            let font = Font.withScale 2000 2000 (Font.create (glyfFace ValueNone))
            match Extents.glyph font 1u with
            | Ok (ValueSome extents) ->
                Expect.equal (extents.XBearing, extents.YBearing, extents.Width, extents.Height) (0, 400, 180, -360) "scaled"
            | _ -> failtest "expected scaled extents for glyph 1"
        }
        test "a glyf-vmtx face answers its vertical origin from the phantom top" {
            let glyfVmtx = Font.create (glyfVmtxFace ())
            match Origins.vOrigin glyfVmtx 0u with
            | Ok struct (x, y) ->
                Expect.equal x 250 "half the horizontal advance"
                Expect.equal y 0 "empty glyph phantom top"
            | other -> failtest $"expected a phantom-top origin, got %A{other}"
        }
    ]
