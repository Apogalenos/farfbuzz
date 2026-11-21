module Farfbuzz.Diff.MetricsDiff

open System
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Builds the glyph sequence from 0 through the face's last glyph.</summary>
/// <param name="face">The face.</param>
/// <returns>The glyphs.</returns>
let private glyphsOf (face: Face) : uint32 array =
    Array.init (face.GlyphCount + 1) uint32

/// <summary>Reads horizontal advances from the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The advances.</returns>
let private hAdvances (font: nativeint) (glyphs: uint32 array) : int array =
    let advances = Array.zeroCreate glyphs.Length
    Native.hb_font_get_glyph_h_advances(font, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    advances

/// <summary>Reads vertical advances from the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The advances.</returns>
let private vAdvances (font: nativeint) (glyphs: uint32 array) : int array =
    let advances = Array.zeroCreate glyphs.Length
    Native.hb_font_get_glyph_v_advances(font, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    advances

/// <summary>Reads the oracle's horizontal extents.</summary>
/// <param name="font">The oracle font.</param>
/// <returns>Whether they exist and their values.</returns>
let private hExtents (font: nativeint) =
    let mutable extents = Unchecked.defaultof<Native.OracleFontExtents>
    let present = Native.hb_font_get_h_extents(font, &extents)
    struct (present <> 0, extents.Ascender, extents.Descender, extents.LineGap)

/// <summary>Reads the oracle's vertical extents.</summary>
/// <param name="font">The oracle font.</param>
/// <returns>Whether they exist and their values.</returns>
let private vExtents (font: nativeint) =
    let mutable extents = Unchecked.defaultof<Native.OracleFontExtents>
    let present = Native.hb_font_get_v_extents(font, &extents)
    struct (present <> 0, extents.Ascender, extents.Descender, extents.LineGap)

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Builds a bare sfnt with the given tables.</summary>
/// <param name="version">The sfnt version.</param>
/// <param name="tables">The table bytes.</param>
/// <returns>The file bytes.</returns>
let private sfnt (version: uint32) (tables: (Tag * byte array) list) : byte array =
    let headerSize = 12 + 16 * tables.Length
    let header = Array.concat [ be32 version; be16 (uint32 tables.Length); be16 0u; be16 0u; be16 0u ]
    let records, _ =
        tables
        |> List.fold (fun (records, offset) (tag, bytes) ->
            let record = Array.concat [ be32 (uint32 tag); be32 0u; be32 (uint32 offset); be32 (uint32 bytes.Length) ]
            record :: records, offset + bytes.Length) ([], headerSize)
    Array.concat (header :: List.rev records @ (tables |> List.map snd))

/// <summary>Reads advances from a font built out of bytes through the oracle.</summary>
/// <param name="bytes">The sfnt bytes.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The advances.</returns>
let private oracleBytesAdvances (bytes: byte array) (glyphs: uint32 array) : int array =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let font = Native.hb_font_create face
    Native.hb_ot_font_set_funcs font
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(font, upem, upem)
    let advances = Array.zeroCreate glyphs.Length
    Native.hb_font_get_glyph_h_advances(font, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    Native.hb_font_destroy font
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    advances

/// <summary>The P2.S02 metrics differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let p2s02 =
        testList "P2.S02" [
        exhaustive "P2.S02 h advances" faces
            (fun struct (font, index) ->
                let face = faceOf font index
                let farfbuzzFont = Font.create face
                Font.hAdvances farfbuzzFont (ReadOnlySpan (glyphsOf face)) |> Seq.toArray)
            (fun struct (font, index) ->
                let face = faceOf font index
                hAdvances (OracleFaces.font font index) (glyphsOf face))
        exhaustive "P2.S02 v advances" faces
            (fun struct (font, index) ->
                let face = faceOf font index
                let farfbuzzFont = Font.create face
                let glyphs = glyphsOf face
                Font.vAdvances farfbuzzFont (ReadOnlySpan glyphs) |> Seq.toArray)
            (fun struct (font, index) ->
                let face = faceOf font index
                vAdvances (OracleFaces.font font index) (glyphsOf face))
        exhaustive "P2.S02 h extents" faces
            (fun struct (font, index) ->
                match Font.hExtents (Font.create (faceOf font index)) with
                | ValueSome extents -> struct (true, extents.Ascender, extents.Descender, extents.LineGap)
                | ValueNone -> struct (false, 0, 0, 0))
            (fun struct (font, index) -> hExtents (OracleFaces.font font index))
        exhaustive "P2.S02 v extents" faces
            (fun struct (font, index) ->
                match Font.vExtents (Font.create (faceOf font index)) with
                | ValueSome extents -> struct (true, extents.Ascender, extents.Descender, extents.LineGap)
                | ValueNone -> struct (false, 0, 0, 0))
            (fun struct (font, index) -> vExtents (OracleFaces.font font index))
    ]
    let upperHhea = Array.concat [ be32 0x00010000u; Array.zeroCreate 30; be32 2u ]
    let upperHmtx =
        [| 0uy; 1uy; 0uy; 2uy; 0uy; 3uy; 0uy; 4uy; 0uy; 5uy
           0uy; 6uy; 0uy; 7uy; 0uy; 8uy; 0uy; 9uy |]
    let maxp = [| 0uy; 0uy; 0x50uy; 0uy; 0uy; 5uy |]
    let crafted = sfnt 0x4F54544Fu [ Tags.Maxp, maxp; Tags.UpperHhea, upperHhea; Tags.UpperHmtx, upperHmtx ]
    let glyphs = Array.init 6 uint32
    let p2r03 =
        testList "P2.R03" [
            exhaustive "P2.R03 uppercase metrics" [ () ]
                (fun () ->
                    let font = Font.create (Face.create (ReadOnlyMemory crafted) 0u)
                    Font.hAdvances font (ReadOnlySpan glyphs) |> Seq.toArray)
                (fun () -> oracleBytesAdvances crafted glyphs)
        ]
    testList "Metrics" [ p2s02; p2r03 ]
