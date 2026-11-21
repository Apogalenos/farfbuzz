module Farfbuzz.HarfBuzzSharp.Tests.BlobTests

open System
open System.IO
open System.Runtime.InteropServices
open Expecto
open Farfbuzz.Diff
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>The P10.S04 cases: the blob copies its bytes once and releases them once.</summary>
[<Tests>]
let tests =
    testList "P10.S04" [
        test "the release delegate runs once at Dispose and never from the finaliser" {
            let released = ref 0
            let bytes = [| 1uy; 2uy; 3uy; 4uy |]
            let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
            let make () =
                let blob = new HarfBuzzSharp.Blob(handle.AddrOfPinnedObject(), bytes.Length, HarfBuzzSharp.MemoryMode.ReadOnly, HarfBuzzSharp.ReleaseDelegate(fun () -> released.Value <- released.Value + 1))
                blob.Length
            Expect.equal (make ()) 4 "length"
            GC.Collect()
            GC.WaitForPendingFinalizers()
            Expect.equal released.Value 0 "the finaliser releases nothing"
            let blob = new HarfBuzzSharp.Blob(handle.AddrOfPinnedObject(), bytes.Length, HarfBuzzSharp.MemoryMode.ReadOnly, HarfBuzzSharp.ReleaseDelegate(fun () -> released.Value <- released.Value + 1))
            blob.Dispose()
            blob.Dispose()
            Expect.equal released.Value 1 "released once"
            handle.Free()
        }
        test "the bytes are copied once: later writes to the source do not reach the blob" {
            let bytes = [| 9uy; 8uy; 7uy |]
            let handle = GCHandle.Alloc(bytes, GCHandleType.Pinned)
            use blob = new HarfBuzzSharp.Blob(handle.AddrOfPinnedObject(), bytes.Length, HarfBuzzSharp.MemoryMode.Duplicate)
            bytes[0] <- 0uy
            Expect.equal (blob.AsSpan().ToArray()) [| 9uy; 8uy; 7uy |] "copy"
            handle.Free()
        }
        test "an empty blob releases at once, like hb_blob_create" {
            let released = ref 0
            use blob = new HarfBuzzSharp.Blob(0n, 0, HarfBuzzSharp.MemoryMode.ReadOnly, HarfBuzzSharp.ReleaseDelegate(fun () -> released.Value <- released.Value + 1))
            Expect.equal blob.Length 0 "empty"
            Expect.equal released.Value 1 "released at construction"
            blob.Dispose()
            Expect.equal released.Value 1 "not again"
        }
        test "Empty, immutability, streams and the missing-file exception" {
            Expect.equal HarfBuzzSharp.Blob.Empty.Length 0 "empty"
            HarfBuzzSharp.Blob.Empty.Dispose()
            Expect.equal HarfBuzzSharp.Blob.Empty.Length 0 "still usable after Dispose"
            use blob = HarfBuzzSharp.Blob.FromStream(new MemoryStream([| 5uy; 6uy |]))
            Expect.isFalse blob.IsImmutable "mutable"
            blob.MakeImmutable()
            Expect.isTrue blob.IsImmutable "immutable"
            use stream = blob.AsStream()
            Expect.isFalse stream.CanWrite "read-only stream"
            Expect.equal stream.Length 2L "stream length"
            let missing =
                try
                    HarfBuzzSharp.Blob.FromFile "/no/such/font.ttf" |> ignore
                    None
                with :? FileNotFoundException as error -> Some (error.Message, error.FileName)
            Expect.equal missing (Some ("Unable to find file.", "/no/such/font.ttf")) "the upstream message and file name"
        }
        testCase "FaceCount equals the oracle for every test font" (fun () ->
            OracleSession.ensure ()
            let files = TestFonts.files () |> List.map snd
            Expect.isGreaterThan files.Length 400 "the corpus"
            let mismatches =
                files
                |> List.filter (fun path ->
                    use blob = HarfBuzzSharp.Blob.FromFile path
                    blob.FaceCount <> OracleFaces.count path)
            Expect.isEmpty mismatches "files whose face count differs"
            let collections = files |> List.filter (fun path -> OracleFaces.count path > 1)
            Expect.isNonEmpty collections "at least one collection among them")
    ]
