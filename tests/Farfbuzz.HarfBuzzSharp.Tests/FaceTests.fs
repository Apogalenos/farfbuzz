module Farfbuzz.HarfBuzzSharp.Tests.FaceTests

open System
open System.Collections.Concurrent
open System.IO
open System.Runtime.InteropServices
open Expecto
open Farfbuzz.Diff
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>A facade blob over a corpus file.</summary>
let private blobOf (path: string) = FacadeFonts.blobOf path

/// <summary>The bytes of a table through the oracle.</summary>
/// <param name="face">The oracle face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The bytes.</returns>
let private oracleTable (face: nativeint) (tag: uint32) =
    let table = Native.hb_face_reference_table(face, tag)
    let mutable length = 0u
    let data = Native.hb_blob_get_data(table, &length)
    let bytes = Array.zeroCreate<byte> (int length)
    (match bytes.Length with
     | 0 -> ()
     | length -> Marshal.Copy(data, bytes, 0, length))
    Native.hb_blob_destroy table
    bytes

/// <summary>Every face of the corpus: the test font and the oracle's face for it.</summary>
/// <returns>The faces.</returns>
let private corpus () =
    OracleFaces.all () |> Seq.map (fun (struct (font, index)) -> font, index) |> Seq.toList

/// <summary>The P10.S05 cases: faces from blobs and from table callbacks.</summary>
[<Tests>]
let tests =
    testList "P10.S05" [
        testCase "units per em and glyph count equal the oracle for every face of the corpus" (fun () ->
            OracleSession.ensure ()
            let faces = corpus ()
            Expect.isGreaterThan faces.Length 400 "the corpus"
            let mismatches =
                faces
                |> List.filter (fun (font, index) ->
                    use blob = blobOf font.Path
                    use face = new HarfBuzzSharp.Face(blob, int index)
                    let oracle = OracleFaces.face font index
                    face.UnitsPerEm <> int (Native.hb_face_get_upem oracle) || face.GlyphCount <> int (Native.hb_face_get_glyph_count oracle) || face.Index <> int index)
                |> List.map (fun (font, index) -> $"{font.Path}#{index}")
            Expect.isEmpty mismatches "faces that differ")
        testCase "table tags and table bytes equal the oracle for every face of the corpus" (fun () ->
            OracleSession.ensure ()
            let mismatches =
                corpus ()
                |> List.filter (fun (font, index) ->
                    use blob = blobOf font.Path
                    use face = new HarfBuzzSharp.Face(blob, int index)
                    let oracle = OracleFaces.face font index
                    let expectedTags = OracleFaces.tableTags oracle
                    let tags = face.Tables |> Array.map uint32
                    let bytesAgree =
                        Array.append expectedTags [| 0x5A5A5A5Au |]
                        |> Array.forall (fun tag ->
                            use table = face.ReferenceTable(tag)
                            table.AsSpan().ToArray() = oracleTable oracle tag)
                    tags <> expectedTags || not bytesAgree)
                |> List.map (fun (font, index) -> $"{font.Path}#{index}")
            Expect.isEmpty mismatches "faces that differ")
        test "a face made from a table delegate freezes by asking once for each shaping tag and releasing the blobs" {
            let font = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            use source = blobOf font
            use origin = new HarfBuzzSharp.Face(source, 0)
            let asked = System.Collections.Generic.List<uint32>()
            let released = ref 0
            let destroyed = ref 0
            let getTable =
                HarfBuzzSharp.GetTableDelegate(fun _ tag ->
                    asked.Add(uint32 tag)
                    let bytes = (use table = origin.ReferenceTable tag in table.AsSpan().ToArray())
                    match bytes.Length with
                    | 0 -> Unchecked.defaultof<HarfBuzzSharp.Blob>
                    | _ ->
                        let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
                        let blob = new HarfBuzzSharp.Blob(handle.AddrOfPinnedObject(), bytes.Length, HarfBuzzSharp.MemoryMode.ReadOnly, HarfBuzzSharp.ReleaseDelegate(fun () -> released.Value <- released.Value + 1; handle.Free()))
                        blob)
            use face = new HarfBuzzSharp.Face(getTable, HarfBuzzSharp.ReleaseDelegate(fun () -> destroyed.Value <- destroyed.Value + 1))
            Expect.equal asked.Count 0 "nothing asked before the face is frozen"
            face.UnitsPerEm <- 2048
            face.MakeImmutable()
            Expect.isTrue face.IsImmutable "frozen"
            let shaping = Farfbuzz.HarfBuzzSharp.Bridge.Faces.ShapingTags |> Seq.toList
            Expect.equal (asked |> Seq.toList) shaping "each shaping tag once, in order"
            Expect.equal released.Value (asked |> Seq.filter (fun tag -> origin.Tables |> Array.exists (fun present -> uint32 present = tag)) |> Seq.length) "every blob the delegate returned was disposed"
            Expect.equal face.UnitsPerEm 2048 "the override applied at the freeze"
            Expect.equal face.GlyphCount origin.GlyphCount "the glyph count from maxp"
            Expect.isEmpty face.Tables "a table-callback face has no table directory"
            Expect.equal destroyed.Value 0 "destroy waits for Dispose"
        }
        test "the destroy delegate of a table face runs once, at Dispose" {
            let destroyed = ref 0
            let face = new HarfBuzzSharp.Face(HarfBuzzSharp.GetTableDelegate(fun _ _ -> Unchecked.defaultof<HarfBuzzSharp.Blob>), HarfBuzzSharp.ReleaseDelegate(fun () -> destroyed.Value <- destroyed.Value + 1))
            face.Dispose()
            face.Dispose()
            Expect.equal destroyed.Value 1 "once"
        }
        test "setters change a face until it is frozen and are ignored afterwards" {
            let path = TestFonts.files () |> List.map snd |> List.find (fun path -> path.EndsWith "Roboto-Regular.ttf")
            use blob = blobOf path
            use face = new HarfBuzzSharp.Face(blob, 0)
            let own = face.UnitsPerEm
            face.UnitsPerEm <- 500
            face.GlyphCount <- 7
            Expect.equal (face.UnitsPerEm, face.GlyphCount) (500, 7) "overrides read back"
            face.MakeImmutable()
            face.UnitsPerEm <- own
            face.GlyphCount <- 99
            face.Index <- 3
            Expect.equal (face.UnitsPerEm, face.GlyphCount, face.Index) (500, 7, 0) "ignored once frozen"
        }
        test "arguments are checked as upstream does and the empty face is the empty face" {
            Expect.throwsT<ArgumentNullException> (fun () -> new HarfBuzzSharp.Face(null, 0) |> ignore) "null blob"
            use blob = new HarfBuzzSharp.Blob(0n, 0, HarfBuzzSharp.MemoryMode.ReadOnly)
            let error =
                try
                    new HarfBuzzSharp.Face(blob, -1) |> ignore
                    None
                with :? ArgumentOutOfRangeException as error -> Some (error.ParamName, error.Message.StartsWith "Index must be non negative.")
            Expect.equal error (Some ("index", true)) "negative index"
            Expect.throwsT<ArgumentNullException> (fun () -> new HarfBuzzSharp.Face(Unchecked.defaultof<HarfBuzzSharp.GetTableDelegate>) |> ignore) "null delegate"
            Expect.equal (HarfBuzzSharp.Face.Empty.UnitsPerEm, HarfBuzzSharp.Face.Empty.GlyphCount) (1000, 0) "hb_face_get_empty"
            Expect.isEmpty HarfBuzzSharp.Face.Empty.Tables "no tables"
        }
    ]
