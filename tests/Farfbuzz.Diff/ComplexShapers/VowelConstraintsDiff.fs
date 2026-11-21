module Farfbuzz.Diff.ComplexShapers.VowelConstraintsDiff

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The sixteen scripts that carry vowel constraints, in script order.</summary>
let private scripts : Script array =
    [| Scripts.Devanagari; Scripts.Bengali; Scripts.Gurmukhi; Scripts.Gujarati; Scripts.Oriya; Scripts.Tamil
       Scripts.Telugu; Scripts.Kannada; Scripts.Malayalam; Scripts.Sinhala; Scripts.Brahmi; Scripts.Khojki
       Scripts.Khudawadi; Scripts.Tirhuta; Scripts.Modi; Scripts.Takri |]

/// <summary>The distinct code points of one script's constraints.</summary>
/// <param name="script">The script.</param>
/// <returns>The code points.</returns>
let private distinctCodePoints (script: Script) : uint32 array =
    VowelConstraintTable.forScript script
    |> Seq.collect (fun struct (a, b, c) ->
        match c = 0u with
        | true -> [ a; b ]
        | false -> [ a; b; c ])
    |> Set.ofSeq
    |> Set.toArray

/// <summary>The alphabet of one script: its constraint code points plus the space.</summary>
/// <param name="script">The script.</param>
/// <returns>The alphabet.</returns>
let private alphabet (script: Script) : uint32 array =
    Array.append (distinctCodePoints script) [| 0x20u |]

/// <summary>The union of every script's constraint code points.</summary>
let private unionAlphabet : uint32 array =
    scripts |> Array.collect distinctCodePoints |> Array.distinct |> Array.sort

/// <summary>Every (script, sequence of length 1..3 over that script's alphabet) input.</summary>
let private exhaustiveInputs : seq<struct (Script * uint32 array)> =
    seq {
        for script in scripts do
            let alpha = alphabet script
            for a in alpha do
                yield struct (script, [| a |])
            for a in alpha do
                for b in alpha do
                    yield struct (script, [| a; b |])
            for a in alpha do
                for b in alpha do
                    for c in alpha do
                        yield struct (script, [| a; b; c |])
    }

/// <summary>A random constraint script and a random sequence over the union alphabet.</summary>
let private sampledInput : Gen<struct (Script * uint32 array)> =
    gen {
        let! script = Gen.elements scripts
        let! length = Gen.choose (1, 16)
        let! codepoints = Gen.arrayOfLength length (Gen.elements unionAlphabet)
        return struct (script, codepoints)
    }

/// <summary>Builds a run from code points, script and buffer flags.</summary>
/// <param name="flags">The buffer flags.</param>
/// <param name="script">The script.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private buildRun (flags: BufferFlags) (script: Script) (codepoints: uint32 array) : GlyphRun =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 codepoints.Length GlyphRun.empty
    |> GlyphRun.withScript script
    |> GlyphRun.withFlags flags

/// <summary>Reads a run's code points and clusters.</summary>
/// <param name="run">The run.</param>
/// <returns>The code points and clusters.</returns>
let private extract (run: GlyphRun) : struct (uint32 array * uint32 array) =
    let length = run.Infos.Length
    struct (Array.init length (fun index -> run.Infos[index].Codepoint), Array.init length (fun index -> run.Infos[index].Cluster))

/// <summary>Farfbuzz's vowel-constraint result.</summary>
/// <param name="flags">The buffer flags.</param>
/// <param name="script">The script.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The output code points and clusters.</returns>
let private ours (flags: BufferFlags) (script: Script) (codepoints: uint32 array) : struct (uint32 array * uint32 array) =
    VowelConstraints.preprocess (buildRun flags script codepoints) |> extract

/// <summary>The oracle's vowel-constraint result.</summary>
/// <param name="flags">The buffer flags.</param>
/// <param name="script">The script.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The output code points and clusters.</returns>
let private oracle (flags: BufferFlags) (script: Script) (codepoints: uint32 array) : struct (uint32 array * uint32 array) =
    let len = uint32 codepoints.Length
    let capacity = 2u * len
    let outCodepoints = Array.zeroCreate<uint32> (int capacity)
    let outClusters = Array.zeroCreate<uint32> (int capacity)
    let written = Shims.farfbuzz_oracle_vowel_constraints (uint32 script, uint32 flags, codepoints, len, outCodepoints, outClusters, capacity)
    struct (Array.truncate (int written) outCodepoints, Array.truncate (int written) outClusters)

/// <summary>The P6.S09 vowel-constraint differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P6.S09" [
        exhaustive "vowel constraints" exhaustiveInputs
            (fun struct (script, codepoints) -> ours BufferFlags.Default script codepoints)
            (fun struct (script, codepoints) -> oracle BufferFlags.Default script codepoints)
        sampled "vowel constraints sampled" sampledInput 100_000
            (fun struct (script, codepoints) -> ours BufferFlags.Default script codepoints)
            (fun struct (script, codepoints) -> oracle BufferFlags.Default script codepoints)
        sampled "vowel constraints no dotted circle" sampledInput 10_000
            (fun struct (script, codepoints) -> ours BufferFlags.DoNotInsertDottedCircle script codepoints)
            (fun struct (script, codepoints) -> oracle BufferFlags.DoNotInsertDottedCircle script codepoints)
    ]
