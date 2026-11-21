module Farfbuzz.Tests.GeneratedTests

open System
open System.Diagnostics
open System.IO
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Harness

/// <summary>The group of the two tests that read or plant files of the generated directory: Expecto runs the members of
/// one group one after another, so the test that plants an extra file never overlaps the one that checks the directory.</summary>
[<Literal>]
let private GeneratedFilesGroup = "generated files directory"

/// <summary>Generated upstream tables, P2.S05.</summary>
[<Tests>]
let tests =
    testList "P2.S05" [
        test "the post MacRoman names are generated in order" {
            Expect.equal PostMacRomanNames.names.Length 258 "count"
            Expect.equal (Encoding.Latin1.GetString PostMacRomanNames.names[3]) "space" "spot value"
        }
        test "the CFF standard strings are generated in order" {
            Expect.equal CffStandardStrings.strings.Length 391 "count"
            Expect.equal (Encoding.Latin1.GetString CffStandardStrings.strings[0]) ".notdef" "spot value"
        }
        test "the MacRoman map binary-searches the generated pairs" {
            Expect.equal (CmapMacRoman.unicodeToMacRoman 0x00A0u) 0xCAuy "first"
            Expect.equal (CmapMacRoman.unicodeToMacRoman 0x2026u) 0xC9uy "ellipsis"
            Expect.equal (CmapMacRoman.unicodeToMacRoman 0x0041u) 0uy "absent"
        }
        test "the Arabic PUA maps use later lines as overrides" {
            Expect.equal (ArabicPua.simplified 0x0621u) 0xF1ADus "hamza"
            Expect.equal (ArabicPua.simplified 0x0041u) 0us "absent"
        }
        testSequencedGroup GeneratedFilesGroup <| test "regen --check reports the generated files up to date" {
            let info =
                ProcessStartInfo(
                    "dotnet",
                    "run --project tools/Farfbuzz.Gen -c Release -- regen --check",
                    WorkingDirectory = RepositoryPaths.root (),
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false)
            use proc = Process.Start info |> nonNull
            let output = proc.StandardOutput.ReadToEnd()
            let error = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            Expect.equal proc.ExitCode 0 (output + error)
        }
    ]

/// <summary>The P2.R07 generator strictness cases.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R07" [
        test "every generated table has its pinned count" {
            Expect.equal CffPredefined.standardEncodingToCode.Length 150 "standard encoding"
            Expect.equal CffPredefined.expertEncodingToCode.Length 379 "expert encoding"
            Expect.equal CffPredefined.expertCharsetToSid.Length 166 "expert charset"
            Expect.equal CffPredefined.expertSubsetCharsetToSid.Length 87 "expert subset charset"
            Expect.equal CffPredefined.expertCharsetSidToGid.Length 165 "expert sid pairs"
            Expect.equal CffPredefined.expertSubsetCharsetSidToGid.Length 86 "expert subset sid pairs"
            Expect.equal CffPredefined.standardEncodingToSid.Length 256 "standard sid"
        }
        testSequencedGroup GeneratedFilesGroup <| test "regen --check rejects a generated file the generator does not write" {
            let extra = Path.Combine(RepositoryPaths.root (), "src", "Farfbuzz", "Generated", "ReviewExtra.fs")
            File.WriteAllText(extra, "// extra generated file\n")
            let info =
                ProcessStartInfo(
                    "dotnet",
                    "run --project tools/Farfbuzz.Gen -c Release --no-build -- regen --check",
                    WorkingDirectory = RepositoryPaths.root (),
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false)
            try
                use proc = Process.Start info |> nonNull
                let output = proc.StandardOutput.ReadToEnd()
                let error = proc.StandardError.ReadToEnd()
                proc.WaitForExit()
                Expect.equal proc.ExitCode 1 (output + error)
            finally
                File.Delete extra
        }
    ]
