module Farfbuzz.Tests.CffDictTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Encodes one integer operand in its shortest DICT form.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private operand (value: int) : byte array =
    match value with
    | v when v >= -107 && v <= 107 -> [| byte (v + 139) |]
    | v when v >= 108 && v <= 1131 ->
        let encoded = v - 108
        [| byte (247 + encoded / 256); byte (encoded % 256) |]
    | v when v >= -1131 && v <= -108 ->
        let encoded = -v - 108
        [| byte (251 + encoded / 256); byte (encoded % 256) |]
    | v when v >= int Int16.MinValue && v <= int Int16.MaxValue ->
        Array.concat [ [| 28uy |]; be16 (uint32 (int16 v)) ]
    | v -> Array.concat [ [| 29uy |]; be32 (uint32 v) ]

/// <summary>Encodes a BCD real number.</summary>
/// <param name="text">The decimal text.</param>
/// <returns>The bytes.</returns>
let private bcd (text: string) : byte array =
    let nibbles =
        text
        |> Seq.collect (fun c ->
            match c with
            | '-' -> [ 14 ]
            | '.' -> [ 10 ]
            | 'e' | 'E' -> [ 11 ]
            | c -> [ int c - int '0' ])
        |> Seq.toList
        |> fun values -> values @ [ 15 ]
    let padded = match nibbles.Length % 2 with | 0 -> nibbles | _ -> nibbles @ [ 15 ]
    padded
    |> List.chunkBySize 2
    |> List.map (fun pair -> byte ((List.head pair <<< 4) ||| (List.last pair)))
    |> List.toArray

/// <summary>Encodes an integer with an operator after it.</summary>
/// <param name="value">The integer.</param>
/// <param name="operator">The operator.</param>
/// <returns>The DICT bytes.</returns>
let private withOperator (value: int) (operator: int) : byte array =
    Array.append (operand value) [| byte operator |]

/// <summary>The operands of the only operator.</summary>
/// <param name="dict">The DICT bytes.</param>
/// <returns>The operands, or none.</returns>
let private operandsOf (dict: byte array) =
    match CffDict.interpret (ReadOnlySpan dict) with
    | ValueSome ops when ops.Length = 1 -> ValueSome (ops[0].Operands |> Seq.toList)
    | _ -> ValueNone

/// <summary>CFF INDEX and DICT decoding, P2.S09.</summary>
[<Tests>]
let tests =
    testList "P2.S09" [
        test "integer operand boundaries round-trip" {
            for value in [ -1131; -108; -107; 0; 107; 108; 1131; 32767; -32768; 100000; -100000 ] do
                Expect.equal (operandsOf (withOperator value 0)) (ValueSome [ double value ]) $"{value}"
        }
        test "short and long forms are recognized" {
            let short = Array.concat [ [| 28uy |]; be16 0x1234u; [| 0uy |] ]
            let long = Array.concat [ [| 29uy |]; be32 0x01020304u; [| 0uy |] ]
            Expect.equal (operandsOf short) (ValueSome [ double 0x1234 ]) "short"
            Expect.equal (operandsOf long) (ValueSome [ double 0x01020304 ]) "long"
        }
        test "the number parser accepts the BCD text" {
            match Number.parseDouble (ReadOnlySpan(Text.Encoding.ASCII.GetBytes "1E-3")) 0 true with
            | ValueSome scanned -> Expect.equal scanned.Value 0.001 "value"
            | ValueNone -> Expect.isTrue false "parse"
        }
        test "raw BCD bytes parse" {
            let raw = [| 30uy; 0x1Cuy; 0x3Fuy; 0uy |]
            Expect.equal (operandsOf raw) (ValueSome [ 1e-3 ]) "raw"
        }
        test "BCD examples parse" {
            let one = Array.concat [ [| 30uy |]; bcd "1e-3"; [| 0uy |] ]
            let two = Array.concat [ [| 30uy |]; bcd "-2.25"; [| 0uy |] ]
            Expect.equal (operandsOf one) (ValueSome [ 1e-3 ]) "1e-3"
            Expect.equal (operandsOf two) (ValueSome [ -2.25 ]) "-2.25"
        }
        test "a reserved BCD nibble is an error" {
            let bytes = [| 30uy; 0xFFuy; 0xD0uy |]
            Expect.equal (CffDict.interpret (ReadOnlySpan bytes)) ValueNone "error"
        }
        test "513 operands fit and 514 fail" {
            let bytes count = Array.append (Array.create count 32uy) [| 0uy |]
            Expect.isTrue (ValueOption.isSome (CffDict.interpret (ReadOnlySpan (bytes 513)))) "513"
            Expect.equal (CffDict.interpret (ReadOnlySpan (bytes 514))) ValueNone "514"
        }
        test "truncated operands fail" {
            Expect.equal (CffDict.interpret (ReadOnlySpan [| 28uy; 0uy |])) ValueNone "short"
            Expect.equal (CffDict.interpret (ReadOnlySpan [| 29uy; 0uy; 0uy |])) ValueNone "long"
            Expect.equal (CffDict.interpret (ReadOnlySpan [| 30uy; 0x12uy |])) ValueNone "BCD"
            Expect.equal (CffDict.interpret (ReadOnlySpan [| 12uy |])) (ValueSome ImmutableArray.Empty) "escape at end"
        }
        test "escaped operators use 1200 plus the second byte" {
            let bytes = Array.concat [ operand 5; [| 12uy; 34uy |] ]
            match CffDict.interpret (ReadOnlySpan bytes) with
            | ValueSome ops -> Expect.equal ops[0].Op 1234 "op"
            | ValueNone -> Expect.isTrue false "interpret"
        }
        test "a one-byte INDEX reads its item and size" {
            let indexBytes = Array.concat [ be16 1u; [| 1uy |]; [| 1uy; 2uy |]; [| 65uy |] ]
            let index = CffIndex.read (ReadOnlySpan indexBytes) 0 2
            Expect.isTrue (ValueOption.isSome (CffIndex.sanitize (ReadOnlySpan indexBytes) 0 2 (Sanitize.start indexBytes.Length))) "sanitize"
            Expect.equal (CffIndex.item (ReadOnlySpan indexBytes) index 0u) (struct (5, 1)) "item"
            Expect.equal (CffIndex.size (ReadOnlySpan indexBytes) index) 6u "size"
            Expect.equal (CffIndex.item (ReadOnlySpan indexBytes) index 1u) (struct (0, 0)) "out of range"
        }
        test "INDEX offSize 0 and 5 fail sanitising" {
            let zero = Array.concat [ be16 1u; [| 0uy |]; [| 1uy; 2uy |] ]
            let five = Array.concat [ be16 1u; [| 5uy |]; Array.zeroCreate 10 ]
            Expect.equal (CffIndex.sanitize (ReadOnlySpan zero) 0 2 (Sanitize.start zero.Length)) ValueNone "zero"
            Expect.equal (CffIndex.sanitize (ReadOnlySpan five) 0 2 (Sanitize.start five.Length)) ValueNone "five"
        }
    ]

/// <summary>The P2.R05 INDEX cases.</summary>
[<Tests>]
let reviewTests =
    let makeIndex countSize offSize =
        let count = if countSize = 2 then be16 1u else be32 1u
        let offsets =
            match offSize with
            | 1 -> [| 1uy; 2uy |]
            | 2 -> [| 0uy; 1uy; 0uy; 2uy |]
            | 3 -> [| 0uy; 0uy; 1uy; 0uy; 0uy; 2uy |]
            | _ -> [| 0uy; 0uy; 0uy; 1uy; 0uy; 0uy; 0uy; 2uy |]
        Array.concat [ count; [| byte offSize |]; offsets; [| 65uy |] ]
    testList "P2.R05" [
        for offSize in [ 1; 2; 3; 4 ] do
            yield
                test $"INDEX offSize {offSize} reads its item" {
                    let bytes = makeIndex 2 offSize
                    let index = CffIndex.read (ReadOnlySpan bytes) 0 2
                    Expect.isTrue (ValueOption.isSome (CffIndex.sanitize (ReadOnlySpan bytes) 0 2 (Sanitize.start bytes.Length))) "sanitize"
                    Expect.equal (CffIndex.item (ReadOnlySpan bytes) index 0u) (struct (2 + 1 + offSize * 2, 1)) "item"
                }
        yield test "CFF2 uses a four-byte count" {
            let bytes = makeIndex 4 1
            let index = CffIndex.read (ReadOnlySpan bytes) 0 4
            Expect.isTrue (ValueOption.isSome (CffIndex.sanitize (ReadOnlySpan bytes) 0 4 (Sanitize.start bytes.Length))) "sanitize"
            Expect.equal (CffIndex.size (ReadOnlySpan bytes) index) 8u "size"
        }
        yield test "a maximum count cannot increment" {
            let bytes = Array.concat [ be32 0xFFFFFFFFu; [| 1uy |] ]
            Expect.equal (CffIndex.sanitize (ReadOnlySpan bytes) 0 4 (Sanitize.start bytes.Length)) ValueNone "overflow"
        }
    ]
