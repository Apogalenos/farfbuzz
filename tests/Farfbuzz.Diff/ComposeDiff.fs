module Farfbuzz.Diff.ComposeDiff

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The Unicode code points and out-of-range sentinels.</summary>
let private inputs =
    seq {
        for codepoint in 0 .. 0x110000 do yield uint32 codepoint
        yield 0x7FFFFFFFu
        yield 0xFFFFFFFFu
    }

/// <summary>Reads the oracle decomposition.</summary>
/// <param name="ab">The code point.</param>
/// <returns>Whether it decomposed and the two parts.</returns>
let private oracleDecompose (ab: uint32) =
    let mutable a = 0u
    let mutable b = 0u
    let found = Native.farfbuzz_oracle_decompose(ab, &a, &b) <> 0
    struct (found, a, b)

/// <summary>Reads the oracle composition.</summary>
/// <param name="a">The first code point.</param>
/// <param name="b">The second code point.</param>
/// <returns>Whether it composed and the composite.</returns>
let private oracleCompose (a: uint32) (b: uint32) =
    let mutable ab = 0u
    let found = Native.farfbuzz_oracle_compose(a, b, &ab) <> 0
    struct (found, ab)

/// <summary>The canonical pairs in the oracle decomposition data.</summary>
let private decompositionPairs =
    inputs
    |> Seq.choose (fun codepoint ->
        match oracleDecompose codepoint with
        | struct (true, a, b) when b <> 0u -> Some (struct (a, b))
        | _ -> None)
    |> Seq.toArray

/// <summary>The Hangul and 32-bit key-space composition pairs.</summary>
let private structuralPairs =
    seq {
        for leading in 0x1100u .. 0x1100u + 18u do
            for vowel in 0x1161u .. 0x1161u + 20u do
                yield struct (leading, vowel)
        for syllable in 0xAC00u .. 0xAC00u + 398u do
            for trailing in 0x11A7u .. 0x11A7u + 27u do
                yield struct (syllable, trailing)
        for a in 0u .. 0x7FFu do
            for b in 0x300u .. 0x37Fu do
                yield struct (a, b)
        for b in [ 0u; 1u; 0x300u; 0x301u; 0x10FFFFu ] do
            yield struct (0u, b)
            yield struct (b, 0u)
    }

/// <summary>The P3.S05 compose and decompose differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S05" [
        exhaustive "decompose" inputs
            (fun codepoint ->
                match Unicode.decompose codepoint with
                | ValueSome (struct (a, b)) -> struct (true, a, b)
                | ValueNone -> struct (false, codepoint, 0u))
            oracleDecompose
        exhaustive "decompose compatibility" inputs
            (fun codepoint -> Unicode.decomposeCompatibility codepoint |> Seq.toArray)
            (fun _ -> [||])
        exhaustive "compose" (Seq.append (decompositionPairs |> Seq.ofArray) structuralPairs)
            (fun (struct (a, b)) ->
                match Unicode.compose a b with
                | ValueSome ab -> struct (true, ab)
                | ValueNone -> struct (false, 0u))
            (fun (struct (a, b)) -> oracleCompose a b)
        sampled "compose generated pairs" (Gen.zip (Gen.choose (0, Int32.MaxValue) |> Gen.map uint32) (Gen.choose (0, Int32.MaxValue) |> Gen.map uint32)) 100_000
            (fun (a, b) ->
                match Unicode.compose a b with
                | ValueSome ab -> struct (true, ab)
                | ValueNone -> struct (false, 0u))
            (fun (a, b) -> oracleCompose a b)
    ]
