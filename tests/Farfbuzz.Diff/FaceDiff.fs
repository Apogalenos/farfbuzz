module Farfbuzz.Diff.FaceDiff

open System
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Turns the oracle's raw tag into a measured tag.</summary>
/// <param name="tag">The raw tag.</param>
/// <returns>The measured tag.</returns>
let private asTag (tag: uint32) : Tag =
    LanguagePrimitives.UInt32WithMeasure tag

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Reads a table's bytes through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The table bytes.</returns>
let private oracleTableBytes (face: nativeint) (tag: uint32) =
    let blob = Native.hb_face_reference_table(face, tag)
    let mutable length = 0u
    let pointer = Native.hb_blob_get_data(blob, &length)
    let bytes: byte array = Array.zeroCreate (int length)
    match int length with
    | 0 -> ()
    | count -> Marshal.Copy(pointer, bytes, 0, count)
    Native.hb_blob_destroy blob
    bytes

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint24.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be24 (value: uint32) = [| byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Builds a tiny sfnt with a MAXP table.</summary>
/// <param name="glyphs">The glyph count.</param>
/// <returns>The sfnt bytes.</returns>
let private sfntWithGlyphs (glyphs: int) : byte array =
    let maxp = Array.concat [ be32 0x00005000u; be16 (uint32 glyphs) ]
    Array.concat
        [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u
          be32 (uint32 Tags.Maxp); be32 0u; be32 28u; be32 6u; maxp ]

/// <summary>Builds a Mac resource fork with sfnt resources.</summary>
/// <param name="types">Each type record's tag and resource payloads.</param>
/// <returns>The dfont bytes.</returns>
let private dfont (types: (Tag * byte array list) list) : byte array =
    let resources = types |> List.collect snd
    let dataParts = resources |> List.map (fun bytes -> Array.concat [ be32 (uint32 bytes.Length); bytes ])
    let dataOffsets = dataParts |> List.scan (fun offset part -> offset + part.Length) 0 |> List.take dataParts.Length
    let data = Array.concat dataParts
    let typeCount = types.Length
    let resourceArrayOffset = 2 + typeCount * 8
    let typeRecords =
        types
        |> List.mapi (fun index (tag, payloads) ->
            let before = types |> List.take index |> List.sumBy (fun (_, previous) -> previous.Length)
            Array.concat [ be32 (uint32 tag); be16 (uint32 (payloads.Length - 1)); be16 (uint32 (resourceArrayOffset + before * 12)) ])
        |> Array.concat
    let resourceRecords =
        List.zip resources dataOffsets
        |> List.map (fun (_, offset) -> Array.concat [ be16 0u; be16 0xFFFFu; [| 0uy |]; be24 (uint32 offset); be32 0u ])
        |> Array.concat
    let map =
        Array.concat
            [ Array.zeroCreate 24; be16 28u; be16 0u; be16 (uint32 (typeCount - 1)); typeRecords; resourceRecords ]
    let dataOffset = 256u
    let mapOffset = uint32 (int dataOffset + data.Length)
    Array.concat
        [ be32 dataOffset; be32 mapOffset; be32 (uint32 data.Length); be32 (uint32 map.Length)
          Array.zeroCreate (int dataOffset - 16); data; map ]

/// <summary>Runs an action on an oracle face created from bytes.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="action">The action.</param>
/// <returns>The action result.</returns>
let private withOracleFace (bytes: byte array) (index: uint32) (action: nativeint -> 'T) : 'T =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, index)
    let result = action face
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    result

/// <summary>The P2.S01 differentials against the HarfBuzz face API.</summary>
/// <returns>The test list.</returns>
let tests () =
    let files = TestFonts.files ()
    let faces = OracleFaces.all ()
    let inputs =
        faces
        |> Seq.collect (fun struct (font, index) ->
            let tags = OracleFaces.tableTags (OracleFaces.face font index)
            [ for tag in tags do
                  yield struct (font, index, tag)
              yield struct (font, index, 0u)
              yield struct (font, index, 0x7A7A7A7Au) ])
    let p2s01 =
        testList "P2.S01" [
        exhaustive "P2.S01 face count" files
            (fun (_, path) -> Face.count (ReadOnlyMemory (OracleFaces.data path)))
            (fun (_, path) -> OracleFaces.count path)
        exhaustive "P2.S01 upem" faces
            (fun struct (font, index) -> (faceOf font index).Upem)
            (fun struct (font, index) -> int (Native.hb_face_get_upem (OracleFaces.face font index)))
        exhaustive "P2.S01 glyph count" faces
            (fun struct (font, index) -> (faceOf font index).GlyphCount)
            (fun struct (font, index) -> int (Native.hb_face_get_glyph_count (OracleFaces.face font index)))
        exhaustive "P2.S01 table tags" faces
            (fun struct (font, index) -> Face.tableTags (faceOf font index) |> Seq.map uint32 |> Seq.toArray)
            (fun struct (font, index) -> OracleFaces.tableTags (OracleFaces.face font index))
        exhaustive "P2.S01 table bytes" inputs
            (fun struct (font, index, tag) -> (Face.referenceTable (faceOf font index) (asTag tag)).ToArray())
            (fun struct (font, index, tag) -> oracleTableBytes (OracleFaces.face font index) tag)
        exhaustive "P2.S01 out-of-range index" files
            (fun (_, path) ->
                let bytes = ReadOnlyMemory (OracleFaces.data path)
                let face = Face.create bytes (uint32 (Face.count bytes))
                struct (face.Upem, face.GlyphCount))
            (fun (suite, path) ->
                let index = uint32 (OracleFaces.count path)
                let handle = OracleFaces.face { Path = path; Suite = suite; Index = 0 } index
                struct (int (Native.hb_face_get_upem handle), int (Native.hb_face_get_glyph_count handle)))
    ]
    let oneType = dfont [ Tags.Sfnt, [ sfntWithGlyphs 5; sfntWithGlyphs 7 ] ]
    let twoTypes = dfont [ Tags.Sfnt, [ sfntWithGlyphs 5 ]; Tags.Sfnt, [ sfntWithGlyphs 7; sfntWithGlyphs 9 ] ]
    let crafted =
        [ struct (oneType, 0u); struct (oneType, 1u)
          struct (twoTypes, 0u); struct (twoTypes, 1u) ]
    let p2r02 =
        testList "P2.R02" [
            exhaustive "P2.R02 crafted dfont faces" crafted
                (fun struct (bytes, index) ->
                    let face = Face.create (ReadOnlyMemory bytes) index
                    struct (face.Upem, face.GlyphCount, Face.tableTags face |> Seq.map uint32 |> Seq.toArray,
                            (Face.referenceTable face Tags.Maxp).ToArray()))
                (fun struct (bytes, index) ->
                    withOracleFace bytes index (fun face ->
                        struct (int (Native.hb_face_get_upem face), int (Native.hb_face_get_glyph_count face),
                                OracleFaces.tableTags face, oracleTableBytes face (uint32 Tags.Maxp))))
        ]
    testList "Face" [ p2s01; p2r02 ]
