module Farfbuzz.Tests.ManifestTests

open System
open System.IO
open Expecto
open Farfbuzz.Gen
open Farfbuzz.Gen.Manifest

/// <summary>Creates a unique temporary directory.</summary>
/// <returns>The path.</returns>
let private temporaryDirectory () =
    let path = Path.Combine(Path.GetTempPath(), "farfbuzz-p3s01-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory path |> ignore
    path

/// <summary>The P3.S01 manifest and fetch cases.</summary>
[<Tests>]
let tests =
    testList "P3.S01" [
        test "the manifest round-trips every column" {
            let path = Path.Combine(temporaryDirectory (), "manifest.tsv")
            let entries =
                [ { Name = "Scripts.txt"; Version = "18.0.0"; Url = "https://example.invalid/scripts"; Sha256 = "ABC"; LocalPath = "ucd/Scripts.txt"; Header = [ "# Scripts-18.0.0.txt"; "# Date: 2026-01-01" ] }
                  { Name = "registry"; Version = "2026-09-17"; Url = "https://example.invalid/registry"; Sha256 = "DEF"; LocalPath = "vendored/registry"; Header = [] } ]
            Manifest.write path entries
            match Manifest.read path with
            | Ok actual -> Expect.equal actual entries "entries"
            | Error error -> Expect.isTrue false error
        }
        test "verify detects a flipped byte" {
            let dataDir = temporaryDirectory ()
            let file = Path.Combine(dataDir, "input.txt")
            File.WriteAllText(file, "hello\nworld\n")
            let entry =
                { Name = "input"; Version = "1"; Url = "https://example.invalid/input"; Sha256 = Fetch.sha256 file; LocalPath = "input.txt"; Header = [] }
            Expect.equal (Fetch.verify dataDir [ entry ]) (Ok ()) "before"
            let bytes = File.ReadAllBytes file
            bytes[0] <- bytes[0] ^^^ 0xFFuy
            File.WriteAllBytes(file, bytes)
            match Fetch.verify dataDir [ entry ] with
            | Ok () -> Expect.isTrue false "flipped byte was accepted"
            | Error error -> Expect.stringContains error "SHA-256 mismatch" "hash error"
        }
        test "verify rejects a wrong header line" {
            let dataDir = temporaryDirectory ()
            let file = Path.Combine(dataDir, "input.txt")
            File.WriteAllText(file, "# Header-1.0\n# Date: 2024-01-01\nbody\n")
            let entry =
                { Name = "input"; Version = "1"; Url = "https://example.invalid/input"; Sha256 = Fetch.sha256 file; LocalPath = "input.txt"; Header = [ "# Header-1.0"; "# Date: 2025-01-01" ] }
            match Fetch.verify dataDir [ entry ] with
            | Ok () -> Expect.isTrue false "wrong header was accepted"
            | Error error -> Expect.stringContains error "header mismatch" "header error"
        }
    ]
