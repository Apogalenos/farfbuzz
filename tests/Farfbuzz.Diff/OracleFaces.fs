module Farfbuzz.Diff.OracleFaces

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open Farfbuzz.Oracle
open Farfbuzz.Diff.TestFonts

/// <summary>The file bytes, read once per path.</summary>
let private fileBytes = ConcurrentDictionary<string, byte[]>()
let private fileBlobs = ConcurrentDictionary<string, Lazy<nativeint>>()
let private faceHandles = ConcurrentDictionary<struct (string * uint32), Lazy<nativeint>>()
let private fontHandles = ConcurrentDictionary<struct (string * uint32), Lazy<nativeint>>()

/// <summary>Reads a test font's bytes once.</summary>
/// <param name="path">The font path.</param>
/// <returns>The bytes.</returns>
let data (path: string) : byte[] =
    fileBytes.GetOrAdd(path, File.ReadAllBytes)

/// <summary>Creates a duplicate-mode oracle blob for a file, cached by path.</summary>
/// <param name="path">The font path.</param>
/// <returns>The blob handle.</returns>
let blob (path: string) : nativeint =
    (fileBlobs.GetOrAdd(path, fun path -> lazy (
        let bytes = data path
        Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)))).Value

/// <summary>Creates an oracle face with <c>hb_face_create</c>, cached by path and index.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face handle.</returns>
let face (font: TestFont) (index: uint32) : nativeint =
    (faceHandles.GetOrAdd(struct (font.Path, index), fun _ -> lazy (Native.hb_face_create(blob font.Path, index)))).Value

/// <summary>Creates an OT-func font at scale = upem, cached by path and index.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The font handle.</returns>
let font (font: TestFont) (index: uint32) : nativeint =
    (fontHandles.GetOrAdd(struct (font.Path, index), fun _ -> lazy (
        let face = face font index
        let handle = Native.hb_font_create face
        Native.hb_ot_font_set_funcs handle
        let upem = int (Native.hb_face_get_upem face)
        Native.hb_font_set_scale(handle, upem, upem)
        handle))).Value

/// <summary>Counts the faces of a file through the oracle.</summary>
/// <param name="path">The font path.</param>
/// <returns>The face count.</returns>
let count (path: string) : int =
    int (Native.hb_face_count (blob path))

/// <summary>Lists a face's table tags through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The tags in directory order.</returns>
let tableTags (face: nativeint) : uint32[] =
    let mutable count = 0u
    let total = Native.hb_face_get_table_tags(face, 0u, &count, Array.empty)
    let tags = Array.zeroCreate (int total)
    let mutable written = total
    Native.hb_face_get_table_tags(face, 0u, &written, tags) |> ignore
    tags

/// <summary>Returns a table's length through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The clamped length.</returns>
let tableLength (face: nativeint) (tag: uint32) : int =
    let table = Native.hb_face_reference_table(face, tag)
    let length = Native.hb_blob_get_length table
    Native.hb_blob_destroy table
    int length

/// <summary>Every face index below the oracle's face count.</summary>
/// <returns>The test font and index pairs.</returns>
let all () : ImmutableArray<struct (TestFont * uint32)> =
    TestFonts.files ()
    |> List.collect (fun (suite, path) ->
        match count path with
        | 0 -> []
        | faceCount ->
            [ for index in 0 .. faceCount - 1 ->
                struct ({ Path = path; Suite = suite; Index = index }, uint32 index) ])
    |> List.toArray
    |> ImmutableArray.CreateRange
