module Farfbuzz.Diff.ComplexShapers.SyllableDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The union of the indic, khmer and myanmar machine exports and every value the indic category table yields.</summary>
let private a35 : byte array =
    [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy; 11uy; 12uy; 13uy; 14uy; 15uy; 16uy; 17uy; 18uy
       20uy; 21uy; 22uy; 23uy; 25uy; 26uy; 27uy; 32uy
       35uy; 36uy; 37uy; 38uy; 39uy; 40uy; 41uy; 57uy |]

/// <summary>The 44 USE category exports.</summary>
let private useExports : byte array =
    [| 0uy; 1uy; 4uy; 5uy; 6uy; 11uy; 12uy; 13uy; 14uy; 16uy; 18uy
       22uy; 23uy; 24uy; 25uy; 26uy; 27uy; 28uy; 29uy; 30uy; 31uy; 32uy; 33uy; 34uy; 35uy
       37uy; 38uy; 39uy; 41uy; 42uy; 43uy; 44uy; 45uy; 46uy; 47uy; 48uy; 49uy; 50uy; 51uy; 52uy; 53uy; 54uy; 55uy; 56uy |]

/// <summary>Every sequence over the alphabet of length at most <paramref name="maxLength"/>.</summary>
/// <param name="alphabet">The category alphabet.</param>
/// <param name="maxLength">The longest sequence.</param>
/// <returns>The sequences.</returns>
let private sequences (alphabet: byte array) (maxLength: int) : seq<byte array> =
    let rec emit (length: int) (prefix: byte array) : seq<byte array> =
        seq {
            match length < maxLength with
            | false -> ()
            | true ->
                for c in alphabet do
                    let next = Array.append prefix [| c |]
                    yield next
                    yield! emit (length + 1) next
        }
    seq {
        yield [||]
        yield! emit 0 [||]
    }

/// <summary>Farfbuzz's syllables for a machine over a category run with the identity position mapping.</summary>
/// <param name="machine">The machine.</param>
/// <param name="categories">The categories.</param>
/// <returns>The syllable bytes and the broken flag.</returns>
let private oursIdentity (machine: RagelMachine) (categories: byte array) : struct (byte array * bool) =
    let cat = Sealed.ofSpan (ReadOnlySpan categories)
    let previous = Sealed.init categories.Length (fun _ -> 0uy)
    let struct (syllables, broken) = Ragel.findSyllables machine cat (Unchecked.defaultof<ImmutableArray<int>>) previous
    struct (syllables.AsSpan().ToArray(), broken)

/// <summary>Farfbuzz's syllables for the USE machine after the ZWNJ/CGJ filter.</summary>
/// <param name="marks">The Unicode-mark flags.</param>
/// <param name="categories">The categories.</param>
/// <returns>The syllable bytes and the broken flag.</returns>
let private oursUse (marks: bool array) (categories: byte array) : struct (byte array * bool) =
    let cat = Sealed.ofSpan (ReadOnlySpan categories)
    let markImm = Sealed.ofSpan (ReadOnlySpan marks)
    let struct (filtered, positions) = Ragel.useFilter cat markImm
    let previous = Sealed.init categories.Length (fun _ -> 0uy)
    let struct (syllables, broken) = Ragel.findSyllables UseMachine.machine filtered positions previous
    struct (syllables.AsSpan().ToArray(), broken)

/// <summary>The oracle's syllables for one of the four finders.</summary>
/// <param name="shaper">0 indic, 1 khmer, 2 myanmar, 3 use.</param>
/// <param name="marks">The mark bytes for the USE filter.</param>
/// <param name="categories">The categories.</param>
/// <returns>The syllable bytes and the broken flag.</returns>
let private oracleRun (shaper: uint32) (marks: byte array) (categories: byte array) : struct (byte array * bool) =
    let out = Array.zeroCreate categories.Length
    let broken = Shims.farfbuzz_oracle_find_syllables (shaper, categories, marks, uint32 categories.Length, out)
    struct (out, broken <> 0)

/// <summary>The oracle's syllables for a finder that does not read mark flags, all glyphs marked not-mark.</summary>
/// <param name="shaper">0 indic, 1 khmer, 2 myanmar.</param>
/// <param name="categories">The categories.</param>
/// <returns>The syllable bytes and the broken flag.</returns>
let private oracleNoMarks (shaper: uint32) (categories: byte array) : struct (byte array * bool) =
    oracleRun shaper (Array.zeroCreate<byte> categories.Length) categories

/// <summary>A random category run over an alphabet, length 0..32.</summary>
/// <param name="alphabet">The category alphabet.</param>
/// <returns>The generator.</returns>
let private sampledCategories (alphabet: byte array) : Gen<byte array> =
    gen {
        let! length = Gen.choose (0, 32)
        let! categories = Gen.arrayOfLength length (Gen.elements alphabet)
        return categories
    }

/// <summary>A random USE category run with random mark flags, length 0..32.</summary>
let private sampledUse : Gen<struct (byte array * bool array)> =
    gen {
        let! length = Gen.choose (0, 32)
        let! categories = Gen.arrayOfLength length (Gen.elements useExports)
        let! marks = Gen.arrayOfLength length (Gen.elements [ false; true ])
        return struct (categories, marks)
    }

/// <summary>Asserts that a 40-glyph single-syllable run serials wrap 16 to 1, and matches the oracle.</summary>
/// <param name="name">The test name.</param>
/// <param name="machine">The machine.</param>
/// <param name="shaper">The oracle shaper index.</param>
/// <returns>The test.</returns>
let private serialWrap (name: string) (machine: RagelMachine) (shaper: uint32) =
    testCase name (fun () ->
        let categories = Array.create 40 0uy
        let struct (ours, _) = oursIdentity machine categories
        let struct (oracle, _) = oracleRun shaper (Array.zeroCreate<byte> categories.Length) categories
        Expect.equal ours oracle "syllable bytes equal the oracle"
        let serials = ours |> Array.map (fun syllable -> syllable >>> 4)
        let expected = Array.append (Array.append [| 1uy .. 15uy |] [| 1uy .. 15uy |]) [| 1uy .. 10uy |]
        Expect.equal serials expected "serials wrap 16 to 1")

/// <summary>The P6.S04 syllable differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P6.S04" [
        exhaustive "indic syllables" (sequences a35 4)
            (oursIdentity IndicMachine.machine)
            (oracleNoMarks 0u)
        exhaustive "khmer syllables" (sequences a35 4)
            (oursIdentity KhmerMachine.machine)
            (oracleNoMarks 1u)
        exhaustive "myanmar syllables" (sequences a35 4)
            (oursIdentity MyanmarMachine.machine)
            (oracleNoMarks 2u)
        exhaustive "use syllables, marks all false" (sequences useExports 4)
            (fun categories -> oursUse (Array.zeroCreate<bool> categories.Length) categories)
            (fun categories -> oracleRun 3u (Array.zeroCreate<byte> categories.Length) categories)
        exhaustive "use syllables, marks all true, length to 3" (sequences useExports 3)
            (fun categories -> oursUse (Array.create categories.Length true) categories)
            (fun categories -> oracleRun 3u (Array.create categories.Length 1uy) categories)
        sampled "indic syllables sampled" (sampledCategories a35) 100_000
            (oursIdentity IndicMachine.machine)
            (oracleNoMarks 0u)
        sampled "khmer syllables sampled" (sampledCategories a35) 100_000
            (oursIdentity KhmerMachine.machine)
            (oracleNoMarks 1u)
        sampled "myanmar syllables sampled" (sampledCategories a35) 100_000
            (oursIdentity MyanmarMachine.machine)
            (oracleNoMarks 2u)
        sampled "use syllables sampled" sampledUse 100_000
            (fun struct (categories, marks) -> oursUse marks categories)
            (fun struct (categories, marks) -> oracleRun 3u (marks |> Array.map (fun mark -> if mark then 1uy else 0uy)) categories)
        serialWrap "indic serial wrap" IndicMachine.machine 0u
        serialWrap "khmer serial wrap" KhmerMachine.machine 1u
        serialWrap "myanmar serial wrap" MyanmarMachine.machine 2u
        serialWrap "use serial wrap" UseMachine.machine 3u
        testCase "empty run agrees" (fun () ->
            let struct (ours, _) = oursIdentity IndicMachine.machine [||]
            let struct (oracle, _) = oracleNoMarks 0u [||]
            Expect.equal ours oracle "empty indic run"
            let struct (useOurs, _) = oursUse [||] [||]
            let struct (useOracle, _) = oracleRun 3u [||] [||]
            Expect.equal useOurs useOracle "empty use run")
        testCase "one glyph agrees" (fun () ->
            for machine, shaper in [ IndicMachine.machine, 0u; KhmerMachine.machine, 1u; MyanmarMachine.machine, 2u ] do
                let struct (ours, _) = oursIdentity machine [| 0uy |]
                let struct (oracle, _) = oracleRun shaper [| 0uy |] [| 0uy |]
                Expect.equal ours oracle $"one glyph, shaper {shaper}")
        testCase "a run whose every glyph is filtered out agrees" (fun () ->
            let categories = [| UseMachine.CGJ; UseMachine.CGJ; UseMachine.CGJ |]
            let struct (ours, _) = oursUse [| false; false; false |] categories
            let struct (oracle, _) = oracleRun 3u [| 0uy; 0uy; 0uy |] categories
            Expect.equal ours oracle "all-CGJ run")
        testCase "a trailing ZWNJ agrees" (fun () ->
            let categories = [| UseMachine.O; UseMachine.ZWNJ |]
            let struct (ours, _) = oursUse [| false; false |] categories
            let struct (oracle, _) = oracleRun 3u [| 0uy; 0uy |] categories
            Expect.equal ours oracle "trailing ZWNJ run")
    ]
