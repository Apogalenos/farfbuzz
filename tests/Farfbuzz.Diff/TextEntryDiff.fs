module Farfbuzz.Diff.TextEntryDiff

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The compared buffer state.</summary>
type private EntryState =
    { Codepoints: uint32 array
      Clusters: uint32 array
      Before: uint32 array
      After: uint32 array
      Content: uint32 }

/// <summary>Reads the oracle state.</summary>
/// <param name="buffer">The buffer.</param>
/// <returns>The state.</returns>
let private oracleState (buffer: nativeint) =
    let count = max 1 (int (Native.hb_buffer_get_length buffer))
    let codepoints = Array.zeroCreate count
    let clusters = Array.zeroCreate count
    let before = Array.zeroCreate 5
    let after = Array.zeroCreate 5
    let mutable length = 0u
    let mutable beforeLength = 0u
    let mutable afterLength = 0u
    let mutable content = 0u
    Native.farfbuzz_oracle_buffer_state(buffer, codepoints, clusters, before, after, &length, &beforeLength, &afterLength, &content)
    { Codepoints = codepoints[0 .. int length - 1]
      Clusters = clusters[0 .. int length - 1]
      Before = before[0 .. int beforeLength - 1]
      After = after[0 .. int afterLength - 1]
      Content = content }

/// <summary>Reads a Farfbuzz run's state.</summary>
/// <param name="run">The run.</param>
/// <returns>The state.</returns>
let private ourState (run: GlyphRun) =
    { Codepoints = run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)
      Clusters = run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Cluster)
      Before = run.ContextBefore.AsSpan().ToArray()
      After = run.ContextAfter.AsSpan().ToArray()
      Content = match run.ContentType with | ContentType.NoContent -> 0u | ContentType.UnicodeContent -> 1u | ContentType.GlyphContent -> 2u }

/// <summary>A random UTF-8 add input.</summary>
let private utf8Input =
    gen {
        let! length = Gen.choose (0, 40)
        let! bytes = Gen.listOfLength length (Gen.elements [ 0uy; 0x41uy; 0x7Fuy; 0x80uy; 0xBFuy; 0xC2uy; 0xE0uy; 0xEDuy; 0xF0uy; 0xF5uy; 0xFFuy ])
        let! offset = Gen.choose (-3, 45)
        let! itemLength = Gen.choose (-3, 45)
        return struct (Array.ofList bytes, offset, itemLength)
    }

/// <summary>A random UTF-8 add input of mostly well-formed text: code points of every width encoded, a byte now and then
/// dropped or altered, and the item cut anywhere, so the unchecked decoder of a well-formed item and the checking decoder
/// of a damaged one both meet HarfBuzz's.</summary>
let private utf8WellFormedInput =
    gen {
        let! count = Gen.choose (0, 24)
        let! codepoints =
            Gen.listOfLength count (Gen.frequency
                [ 4, Gen.choose (0x20, 0x7E); 3, Gen.choose (0x80, 0x7FF); 3, Gen.choose (0x800, 0xD7FF); 1, Gen.choose (0xE000, 0xFFFF); 2, Gen.choose (0x10000, 0x10FFFF) ])
        let bytes = codepoints |> List.collect (fun codepoint -> System.Text.Rune(codepoint).ToString() |> System.Text.Encoding.UTF8.GetBytes |> Array.toList) |> List.toArray
        let! damage = Gen.frequency [ 6, Gen.constant 0; 2, Gen.constant 1; 1, Gen.constant 2 ]
        let! position = Gen.choose (0, max 0 (bytes.Length - 1))
        let! replacement = Gen.elements [ 0x80uy; 0xBFuy; 0xC0uy; 0xC1uy; 0xE0uy; 0xEDuy; 0xF4uy; 0xF5uy; 0xFFuy; 0x41uy ]
        let damaged =
            match damage, bytes.Length with
            | _, 0 -> bytes
            | 0, _ -> bytes
            | 1, _ -> Array.append bytes[.. position - 1] bytes[position + 1 ..]
            | _, _ -> Array.mapi (fun index value -> match index = position with | true -> replacement | false -> value) bytes
        let! offset = Gen.frequency [ 5, Gen.constant 0; 3, Gen.choose (0, damaged.Length) ]
        let! itemLength = Gen.frequency [ 5, Gen.constant -1; 3, Gen.choose (0, damaged.Length) ]
        return struct (damaged, offset, itemLength)
    }

/// <summary>A random UTF-16 add input.</summary>
let private utf16Input =
    gen {
        let! length = Gen.choose (0, 24)
        let! units = Gen.listOfLength length (Gen.elements [ 0x41us; 0xD800us; 0xDBFFus; 0xDC00us; 0xDFFFus; 0xFFFFus ])
        let! offset = Gen.choose (-3, 28)
        let! itemLength = Gen.choose (-3, 28)
        return struct (Array.ofList units, offset, itemLength)
    }

/// <summary>A random UTF-32 add input.</summary>
let private utf32Input =
    gen {
        let! length = Gen.choose (0, 20)
        let! units = Gen.listOfLength length (Gen.elements [ 0u; 0x41u; 0xD800u; 0xDFFFu; 0x10FFFFu; 0x110000u; 0xFFFFFFFFu ])
        let! offset = Gen.choose (-3, 24)
        let! itemLength = Gen.choose (-3, 24)
        return struct (Array.ofList units, offset, itemLength)
    }

/// <summary>The P3.S07 text-entry differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S07" [
        sampled "add utf8" utf8Input 20_000
            (fun struct (text, offset, itemLength) -> GlyphRun.addUtf8 (ReadOnlySpan text) offset itemLength GlyphRun.empty |> ourState)
            (fun struct (text, offset, itemLength) ->
                let buffer = Native.hb_buffer_create ()
                Native.farfbuzz_oracle_buffer_add_utf8(buffer, text, text.Length, offset, (if itemLength < 0 then -1 else itemLength))
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
        sampled "add utf8 well-formed" utf8WellFormedInput 20_000
            (fun struct (text, offset, itemLength) -> GlyphRun.addUtf8 (ReadOnlySpan text) offset itemLength GlyphRun.empty |> ourState)
            (fun struct (text, offset, itemLength) ->
                let buffer = Native.hb_buffer_create ()
                Native.farfbuzz_oracle_buffer_add_utf8(buffer, text, text.Length, offset, (if itemLength < 0 then -1 else itemLength))
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
        sampled "add utf16" utf16Input 20_000
            (fun struct (text, offset, itemLength) -> GlyphRun.addUtf16 (ReadOnlySpan text) offset itemLength GlyphRun.empty |> ourState)
            (fun struct (text, offset, itemLength) ->
                let buffer = Native.hb_buffer_create ()
                Native.farfbuzz_oracle_buffer_add_utf16(buffer, text, text.Length, offset, (if itemLength < 0 then -1 else itemLength))
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
        sampled "add utf32" utf32Input 20_000
            (fun struct (text, offset, itemLength) -> GlyphRun.addUtf32 (ReadOnlySpan text) offset itemLength GlyphRun.empty |> ourState)
            (fun struct (text, offset, itemLength) ->
                let buffer = Native.hb_buffer_create ()
                Native.farfbuzz_oracle_buffer_add_utf32(buffer, text, text.Length, offset, (if itemLength < 0 then -1 else itemLength))
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
        sampled "add latin1" utf8Input 20_000
            (fun struct (text, offset, itemLength) -> GlyphRun.addLatin1 (ReadOnlySpan text) offset itemLength GlyphRun.empty |> ourState)
            (fun struct (text, offset, itemLength) ->
                let buffer = Native.hb_buffer_create ()
                Native.farfbuzz_oracle_buffer_add_latin1(buffer, text, text.Length, offset, (if itemLength < 0 then -1 else itemLength))
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
        sampled "add codepoints" utf32Input 20_000
            (fun struct (text, offset, itemLength) -> GlyphRun.addCodepoints (ReadOnlySpan text) offset itemLength GlyphRun.empty |> ourState)
            (fun struct (text, offset, itemLength) ->
                let buffer = Native.hb_buffer_create ()
                Native.hb_buffer_add_codepoints(buffer, text, text.Length, uint32 offset, (if itemLength < 0 then -1 else itemLength))
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
    ]
