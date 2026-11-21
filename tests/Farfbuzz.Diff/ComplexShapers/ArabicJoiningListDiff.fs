module Farfbuzz.Diff.ComplexShapers.ArabicJoiningListDiff

open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The 181 script values of P6.S01: Scripts.all (179) plus Invalid and the Myanmar Zawgyi tag.</summary>
let private scripts =
    seq {
        yield! Scripts.all
        yield Scripts.Invalid
        yield 0x51616167u<script> // HB_SCRIPT_MYANMAR_ZAWGYI
    }

/// <summary>The P6.S08 Arabic joining-list and Windows-1256 differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P6.S08" [
        exhaustive "has_arabic_joining" scripts
            (fun script -> ArabicJoiningList.hasArabicJoining script)
            (fun script -> Shims.farfbuzz_oracle_has_arabic_joining (uint32 script) <> 0)
        testCase "win1256 bytes" (fun () ->
            let size = Shims.farfbuzz_oracle_arabic_win1256 (Array.zeroCreate 0, 0u)
            let oracle = Array.zeroCreate<byte> (int size)
            let written = Shims.farfbuzz_oracle_arabic_win1256 (oracle, uint32 oracle.Length)
            Expect.equal written (uint32 oracle.Length) "win1256 serialisation length"
            Expect.equal (ArabicWin1256.bytes.AsSpan().ToArray()) oracle "win1256 bytes")
    ]
