module Farfbuzz.Diff.ComplexShapers.ArabicTableDiff

open Expecto
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>Every Unicode code point plus the out-of-range sentinels.</summary>
let private inputs =
    seq {
        for codepoint in 0 .. 0x10FFFF do yield uint32 codepoint
        yield 0x110000u
        yield 0x7FFFFFFFu
        yield 0xFFFFFFFFu
    }

/// <summary>Flattens one ligature table into the uint16 sequence the oracle shim writes.</summary>
/// <param name="sets">The ligature sets.</param>
/// <returns>The flat uint16 sequence.</returns>
let private flat (sets: ImmutableArray<ArabicLigatureSet>) =
    let result = ResizeArray<uint16>()
    for set in sets do
        result.Add set.First
        for struct (components, ligature) in set.Entries do
            for glyph in components do
                result.Add glyph
            result.Add ligature
    result.ToArray()

/// <summary>Compares one ligature table against the oracle serialisation.</summary>
/// <param name="name">The test name.</param>
/// <param name="which">0 three-component, 1 two-component, 2 mark two-component.</param>
/// <param name="sets">The ligature sets.</param>
/// <returns>The test.</returns>
let private ligatureCase (name: string) (which: uint32) (sets: ImmutableArray<ArabicLigatureSet>) =
    testCase name (fun () ->
        let expected = flat sets
        let oracle = Array.zeroCreate<uint16> expected.Length
        let count = Shims.farfbuzz_oracle_arabic_ligatures (which, oracle, uint32 expected.Length)
        Expect.equal count (uint32 expected.Length) "ligature serialisation length"
        Expect.equal oracle expected "ligature serialisation")

/// <summary>The P6.S07 Arabic table differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P6.S07" [
        exhaustive "joining type" inputs
            (fun codepoint -> uint32 (ArabicTable.joiningType codepoint))
            Shims.farfbuzz_oracle_arabic_joining_type_raw
        exhaustive "shaping" inputs
            (fun codepoint ->
                struct (
                    uint32 (ArabicTable.shaping codepoint 0),
                    uint32 (ArabicTable.shaping codepoint 1),
                    uint32 (ArabicTable.shaping codepoint 2),
                    uint32 (ArabicTable.shaping codepoint 3)))
            (fun codepoint ->
                struct (
                    Shims.farfbuzz_oracle_arabic_shaping (codepoint, 0u),
                    Shims.farfbuzz_oracle_arabic_shaping (codepoint, 1u),
                    Shims.farfbuzz_oracle_arabic_shaping (codepoint, 2u),
                    Shims.farfbuzz_oracle_arabic_shaping (codepoint, 3u)))
        ligatureCase "ligature 3 table" 0u ArabicTable.ligatures3
        ligatureCase "ligature table" 1u ArabicTable.ligatures
        ligatureCase "ligature mark table" 2u ArabicTable.ligaturesMark
    ]
