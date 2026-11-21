module Farfbuzz.Diff.GlyphNameDiff

open System
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Reads a glyph string from the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The string.</returns>
let private oracleName (font: nativeint) (glyph: uint32) : byte array =
    let buffer = Array.zeroCreate 128
    Native.hb_font_glyph_to_string(font, glyph, buffer, 128u)
    let length = match Array.IndexOf(buffer, 0uy) with | -1 -> buffer.Length | value -> value
    buffer[0 .. length - 1]

/// <summary>Parses a glyph string through the oracle.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="name">The string.</param>
/// <returns>Whether it resolved and the glyph.</returns>
let private oracleFromName (font: nativeint) (name: byte array) =
    let mutable glyph = 0u
    let found = Native.hb_font_glyph_from_string(font, name, name.Length, &glyph)
    struct (found <> 0, glyph)

/// <summary>The fixed strings added to every face's name round-trip.</summary>
let private synthetic = [ "gid0"; "uni0041"; "65"; "nosuchname" ]

/// <summary>The P2.S11 glyph name differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    testList "P2.S11" [
        exhaustive "P2.S11 glyph names" faces
            (fun struct (font, index) ->
                let face = faceOf font index
                let farfbuzzFont = Font.create face
                Array.init face.GlyphCount (fun glyph -> Encoding.Latin1.GetBytes (Font.glyphToString farfbuzzFont (uint32 glyph))))
            (fun struct (font, index) ->
                let face = OracleFaces.face font index
                let count = int (Native.hb_face_get_glyph_count face)
                Array.init count (fun glyph -> oracleName (OracleFaces.font font index) (uint32 glyph)))
        exhaustive "P2.S11 glyph from name" faces
            (fun struct (font, index) ->
                let face = faceOf font index
                let farfbuzzFont = Font.create face
                let names = Array.init face.GlyphCount (fun glyph -> Font.glyphToString farfbuzzFont (uint32 glyph)) |> Array.toList
                (names @ synthetic)
                |> List.map (fun name ->
                    match Font.glyphFromString farfbuzzFont name with
                    | ValueSome glyph -> struct (true, glyph)
                    | ValueNone -> struct (false, 0u))
                |> List.toArray)
            (fun struct (font, index) ->
                let oracleFont = OracleFaces.font font index
                let face = OracleFaces.face font index
                let count = int (Native.hb_face_get_glyph_count face)
                let names = Array.init count (fun glyph -> oracleName oracleFont (uint32 glyph)) |> Array.toList
                (names @ (synthetic |> List.map Encoding.ASCII.GetBytes))
                |> List.map (oracleFromName oracleFont)
                |> List.toArray)
    ]
