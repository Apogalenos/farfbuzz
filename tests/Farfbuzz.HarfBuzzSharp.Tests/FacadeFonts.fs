module Farfbuzz.HarfBuzzSharp.Tests.FacadeFonts

open System
open System.Collections.Concurrent
open System.IO
open System.Runtime.InteropServices

/// <summary>The bytes of every font file read so far, read once.</summary>
let private fileBytes = ConcurrentDictionary<string, byte[]>()

/// <summary>The bytes of a font file.</summary>
/// <param name="path">The file.</param>
/// <returns>The bytes, shared: callers never write to them.</returns>
let bytesOf (path: string) =
    fileBytes.GetOrAdd(path, File.ReadAllBytes)

/// <summary>A facade blob over a font file; the blob copies the bytes, so the pinned handle lives for the construction only.</summary>
/// <param name="path">The file.</param>
/// <returns>The blob, for the caller to dispose.</returns>
let blobOf (path: string) =
    let bytes = bytesOf path
    let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
    let blob = new HarfBuzzSharp.Blob(handle.AddrOfPinnedObject(), bytes.Length, HarfBuzzSharp.MemoryMode.ReadOnly)
    handle.Free()
    blob

/// <summary>The faces made so far, one per file and index: a face is frozen at its first font and safe to share.</summary>
let private faces = ConcurrentDictionary<struct (string * int), Lazy<HarfBuzzSharp.Face>>()

/// <summary>A face over a font file, made once and shared.</summary>
/// <param name="path">The file.</param>
/// <param name="index">The face index.</param>
/// <returns>The face; never dispose it.</returns>
let faceOf (path: string) (index: int) =
    faces.GetOrAdd(struct (path, index), fun _ ->
        lazy
            (use blob = blobOf path
             new HarfBuzzSharp.Face(blob, index))).Value

/// <summary>The faces made from table delegates so far, one per file and index.</summary>
let private callbackFaces = ConcurrentDictionary<struct (string * int), Lazy<HarfBuzzSharp.Face>>()

/// <summary>A face made from a table delegate that answers from the font file's own tables, as Avalonia's typeface does.</summary>
/// <param name="path">The file.</param>
/// <param name="index">The face index.</param>
/// <returns>The face; never dispose it.</returns>
let callbackFaceOf (path: string) (index: int) =
    callbackFaces.GetOrAdd(struct (path, index), fun _ ->
        lazy
            (let origin = faceOf path index
             let answer (_: HarfBuzzSharp.Face) (tag: HarfBuzzSharp.Tag) =
                use table = origin.ReferenceTable tag
                match table.Length with
                | 0 -> Unchecked.defaultof<HarfBuzzSharp.Blob>
                | _ ->
                    let bytes = table.AsSpan().ToArray()
                    let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
                    let blob = new HarfBuzzSharp.Blob(handle.AddrOfPinnedObject(), bytes.Length, HarfBuzzSharp.MemoryMode.ReadOnly)
                    handle.Free()
                    blob
             new HarfBuzzSharp.Face(HarfBuzzSharp.GetTableDelegate answer, null))).Value
