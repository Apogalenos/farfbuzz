module Farfbuzz.HarfBuzzSharp.Tests.BufferAddTests

open System
open System.Collections.Generic
open System.IO
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Bridge
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>The four text encodings the buffer takes, each holding its own text.</summary>
type Input =
    | Utf8 of byte array
    | Utf16 of string
    | Utf32 of uint32 array
    | Codepoints of uint32 array

/// <summary>The state of a buffer as the tests compare it: glyph code points, clusters and both contexts.</summary>
type State = { Codepoints: uint32 list; Clusters: uint32 list; Before: uint32 list; After: uint32 list }

/// <summary>A struct sink that records what the bridge decodes.</summary>
[<Struct>]
type CollectingSink =
    val Codepoints: List<uint32>
    val Clusters: List<uint32>
    new (codepoints, clusters) = { Codepoints = codepoints; Clusters = clusters }
    interface ICodepointSink<int> with
        member this.Put (state, codepoint, cluster) =
            this.Codepoints.Add codepoint
            this.Clusters.Add cluster
            state + 1

/// <summary>Turns a context of the bridge into a list, nearest first.</summary>
/// <param name="context">The context.</param>
/// <returns>The code points.</returns>
let private contextList (context: Context) =
    [ context.C0; context.C1; context.C2; context.C3; context.C4 ] |> List.truncate context.Length

/// <summary>The bridge's result for an input, with the pre-context only when it is installed.</summary>
/// <param name="input">The text.</param>
/// <param name="offset">The item offset.</param>
/// <param name="length">The item length.</param>
/// <returns>The state.</returns>
let private bridgeState (input: Input) (offset: int) (length: int) =
    let sink = CollectingSink(List<uint32>(), List<uint32>())
    let replacement = 0xFFFDu
    let added =
        match input with
        | Utf8 bytes -> Text.AddUtf8(ReadOnlySpan<byte>(bytes), offset, length, replacement, true, sink, 0)
        | Utf16 text -> Text.AddUtf16(text.AsSpan(), offset, length, replacement, true, sink, 0)
        | Utf32 units -> Text.AddUtf32(ReadOnlySpan<uint32>(units), offset, length, replacement, true, sink, 0)
        | Codepoints units -> Text.AddCodepoints(ReadOnlySpan<uint32>(units), offset, length, replacement, true, sink, 0)
    { Codepoints = List.ofSeq sink.Codepoints
      Clusters = List.ofSeq sink.Clusters
      Before = (match added.PreInstalled with | true -> contextList added.Pre | false -> [])
      After = contextList added.Post }

/// <summary>The oracle's result for an input.</summary>
/// <param name="input">The text.</param>
/// <param name="offset">The item offset.</param>
/// <param name="length">The item length.</param>
/// <returns>The state.</returns>
let private oracleState (input: Input) (offset: int) (length: int) =
    NativeText.withBuffer (fun buffer ->
        match input with
        | Utf8 bytes -> Native.farfbuzz_oracle_buffer_add_utf8(buffer, bytes, bytes.Length, offset, length)
        | Utf16 text -> Native.farfbuzz_oracle_buffer_add_utf16(buffer, text.ToCharArray() |> Array.map uint16, text.Length, offset, length)
        | Utf32 units -> Native.farfbuzz_oracle_buffer_add_utf32(buffer, units, units.Length, offset, length)
        | Codepoints units -> Native.hb_buffer_add_codepoints(buffer, units, units.Length, uint32 offset, length)
        let capacity = 16384
        let codepoints = Array.zeroCreate<uint32> capacity
        let clusters = Array.zeroCreate<uint32> capacity
        let before = Array.zeroCreate<uint32> 8
        let after = Array.zeroCreate<uint32> 8
        let mutable count = 0u
        let mutable beforeLength = 0u
        let mutable afterLength = 0u
        let mutable content = 0u
        Native.farfbuzz_oracle_buffer_state(buffer, codepoints, clusters, before, after, &count, &beforeLength, &afterLength, &content)
        { Codepoints = codepoints |> Array.truncate (int count) |> Array.toList
          Clusters = clusters |> Array.truncate (int count) |> Array.toList
          Before = before |> Array.truncate (int beforeLength) |> Array.toList
          After = after |> Array.truncate (int afterLength) |> Array.toList })

/// <summary>The facade's result for an input: infos from <c>GlyphInfos</c>.</summary>
/// <param name="input">The text.</param>
/// <param name="offset">The item offset.</param>
/// <param name="length">The item length.</param>
/// <returns>The code points and clusters.</returns>
let private facadeInfos (input: Input) (offset: int) (length: int) =
    use buffer = new HarfBuzzSharp.Buffer()
    match input with
    | Utf8 bytes -> buffer.AddUtf8(ReadOnlySpan<byte>(bytes), offset, length)
    | Utf16 text -> buffer.AddUtf16(text, offset, length)
    | Utf32 units -> buffer.AddUtf32(ReadOnlySpan<uint32>(units), offset, length)
    | Codepoints units -> buffer.AddCodepoints(ReadOnlySpan<uint32>(units), offset, length)
    let infos = buffer.GlyphInfos |> Array.toList
    infos |> List.map (fun info -> info.Codepoint), infos |> List.map (fun info -> info.Cluster)

/// <summary>Characters that stress decoding: ASCII, two- to four-byte scalars, lone surrogates and a pair.</summary>
let private characters = [ 'a'; 'b'; ' '; 'é'; 'ж'; 'ك'; 'क'; '‌'; '€'; '\uD83D'; '\uDE00'; '\uD800'; '\uDC00'; '￿'; '\u0000' ]

/// <summary>Generates a string with lone surrogates in it.</summary>
let private anyText = Gens.noise characters 40

/// <summary>Generates an item range for a text length: offsets and lengths past the end and the -1 sentinel included.</summary>
/// <param name="length">The length of the text in code units.</param>
/// <returns>The generator of offset and length.</returns>
let private anyRange (length: int) =
    gen {
        let! offset = Gen.choose (0, length + 2)
        let! count = Gen.frequency [ 2, Gen.constant -1; 5, Gen.choose (0, length + 2); 1, Gen.constant -2 ]
        return offset, count
    }

/// <summary>Generates an input of any encoding with its item range.</summary>
let private anyCase =
    gen {
        let! text = anyText
        let! kind = Gen.choose (0, 3)
        let! garbage = Gen.listOfLength 6 (Gen.elements [ 0x80uy; 0xC0uy; 0xE0uy; 0xF0uy; 0xFFuy; 0x41uy; 0xC3uy; 0xA9uy ])
        let! wide = Gen.listOfLength 6 (Gen.elements [ 0x41u; 0xD800u; 0xDFFFu; 0x10FFFFu; 0x110000u; 0xFFFFFFFFu; 0x20u; 0u ])
        let input =
            match kind with
            | 0 -> Utf8 (Array.append (Encoding.UTF8.GetBytes (text.Replace('\uD83D', 'x').Replace('\uDE00', 'y').Replace('\uD800', 'z').Replace('\uDC00', 'w'))) (List.toArray garbage))
            | 1 -> Utf16 text
            | 2 -> Utf32 (Array.append (text |> Seq.map uint32 |> Seq.toArray) (List.toArray wide))
            | _ -> Codepoints (Array.append (text |> Seq.map uint32 |> Seq.toArray) (List.toArray wide))
        let size = match input with | Utf8 bytes -> bytes.Length | Utf16 value -> value.Length | Utf32 units -> units.Length | Codepoints units -> units.Length
        let! offset, count = anyRange size
        return input, offset, count
    }

/// <summary>The lines of the seven texts of the performance benchmark.</summary>
let private perfLines () =
    [ "en-thelittleprince"; "en-words"; "react-dom"; "hi-words"; "fa-thelittleprince"; "fa-words"; "duployan" ]
    |> List.collect (fun name -> File.ReadAllLines (Path.Combine(RepositoryPaths.upstream (), "perf", "texts", name + ".txt")) |> Array.toList)

/// <summary>The P10.S06 cases: text entry, contexts and spans of the buffer.</summary>
[<Tests>]
let tests =
    testList "P10.S06" [
        testCase "every line of the seven performance texts enters the buffer as natively, with random item ranges" (fun () ->
            OracleSession.ensure ()
            let seed = 20261118
            printfn $"P10.S06: line ranges drawn from System.Random seed {seed}"
            let random = Random seed
            let lines = perfLines ()
            Expect.isGreaterThan lines.Length 60000 "the corpus"
            let mismatches =
                lines
                |> List.choose (fun line ->
                    let offset = random.Next(0, line.Length + 1)
                    let count = match random.Next 4 with | 0 -> -1 | _ -> random.Next(0, line.Length + 2)
                    let input = Utf16 line
                    let expected = oracleState input offset count
                    let codepoints, clusters = facadeInfos input offset count
                    let bridge = bridgeState input offset count
                    match codepoints = expected.Codepoints && clusters = expected.Clusters && bridge = expected with
                    | true -> None
                    | false -> Some $"{line.Substring(0, min 20 line.Length)} [{offset},{count}]")
                |> List.truncate 10
            Expect.isEmpty mismatches "lines that differ")
        testCase "2 000 generated texts, lone surrogates and invalid UTF-8 included, enter as natively in every encoding" (fun () ->
            OracleSession.ensure ()
            printfn "P10.S06: generated cases drawn from FsCheck's random generator (not seedable through Gen.sample); a failing case is printed in full"
            let cases = Gens.sample 2000 anyCase
            let mismatches =
                cases
                |> List.choose (fun (input, offset, count) ->
                    let expected = oracleState input offset count
                    let codepoints, clusters = facadeInfos input offset count
                    let bridge = bridgeState input offset count
                    match codepoints = expected.Codepoints && clusters = expected.Clusters && bridge = expected with
                    | true -> None
                    | false -> Some $"{input} [{offset},{count}]: oracle {expected}, bridge {bridge}, facade {codepoints}")
                |> List.truncate 5
            Expect.isEmpty mismatches (String.concat "\n" mismatches)
            let withPreContext = cases |> List.filter (fun (input, offset, count) -> not (oracleState input offset count).Before.IsEmpty) |> List.length
            Expect.isGreaterThan withPreContext 200 "the generated cases reach the pre-context")
        test "the pre-context is taken only by the first add of an empty buffer, the post-context by every add" {
            let sink () = CollectingSink(List<uint32>(), List<uint32>())
            let text = ReadOnlySpan<byte>("abcdefghij"B)
            let first = Text.AddUtf8(text, 4, 2, 0xFFFDu, true, sink (), 0)
            Expect.isTrue first.PreInstalled "empty buffer and a positive offset"
            Expect.equal (contextList first.Pre) [ uint32 'd'; uint32 'c'; uint32 'b'; uint32 'a' ] "nearest first, up to five"
            Expect.equal (contextList first.Post) [ uint32 'g'; uint32 'h'; uint32 'i'; uint32 'j' ] "after the item"
            let second = Text.AddUtf8(text, 4, 2, 0xFFFDu, false, sink (), 0)
            Expect.isFalse second.PreInstalled "a buffer with glyphs keeps its pre-context"
            let none = Text.AddUtf8(text, 0, 2, 0xFFFDu, true, sink (), 0)
            Expect.isFalse none.PreInstalled "offset 0 installs nothing"
            let long = Text.AddUtf8(ReadOnlySpan<byte>("abcdefghijklmnop"B), 8, 1, 0xFFFDu, true, sink (), 0)
            Expect.equal long.Pre.Length 5 "at most five"
            Expect.equal long.Post.Length 5 "at most five"
        }
        test "clusters are offsets from the start of the whole text, not of the item" {
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.AddUtf16("xyz abc def", 4, 3)
            let infos = buffer.GetGlyphInfoSpan().ToArray()
            Expect.equal (infos |> Array.map (fun info -> info.Cluster)) [| 4u; 5u; 6u |] "offsets 4, 5, 6"
            Expect.equal (infos |> Array.map (fun info -> info.Codepoint)) [| uint32 'a'; uint32 'b'; uint32 'c' |] "the item"
            Expect.equal buffer.ContentType HarfBuzzSharp.ContentType.Unicode "Unicode after an add"
        }
        test "argument and state checks carry upstream's exceptions and messages" {
            use buffer = new HarfBuzzSharp.Buffer()
            let offset =
                try
                    buffer.AddUtf16("abc", -1, 1)
                    None
                with :? ArgumentOutOfRangeException as error -> Some (error.ParamName, error.Message.StartsWith "ItemOffset must be non negative.")
            Expect.equal offset (Some ("itemOffset", true)) "negative offset"
            Expect.throwsT<ArgumentNullException> (fun () -> buffer.AddUtf8(Unchecked.defaultof<string>)) "null string"
            buffer.ContentType <- HarfBuzzSharp.ContentType.Glyphs
            let glyphs =
                try
                    buffer.AddUtf16 "abc"
                    None
                with :? InvalidOperationException as error -> Some error.Message
            Expect.equal glyphs (Some "ContentType must not be of type Glyphs") "glyph content"
            buffer.ContentType <- HarfBuzzSharp.ContentType.Unicode
            buffer.AddUtf16 "abc"
            buffer.ContentType <- HarfBuzzSharp.ContentType.Invalid
            let content =
                try
                    buffer.AddUtf16 "abc"
                    None
                with :? InvalidOperationException as error -> Some error.Message
            Expect.equal content (Some "Non empty buffer's ContentType must be of type Unicode.") "non-empty with another content type"
        }
        test "spans alias the live storage, copies do not, and positions are zero and present before shaping" {
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.AddUtf16 "ab"
            let copy = buffer.GlyphInfos
            let span = buffer.GetGlyphInfoSpan()
            System.Runtime.InteropServices.MemoryMarshal.GetReference(span) <- HarfBuzzSharp.GlyphInfo(Codepoint = 0x200Cu, Cluster = 9u)
            Expect.equal buffer.GlyphInfos[0].Codepoint 0x200Cu "a write through the span lands in the buffer"
            Expect.equal copy[0].Codepoint (uint32 'a') "a copy keeps what it had"
            let positions = buffer.GetGlyphPositionSpan()
            Expect.equal positions.Length 2 "one position per glyph"
            Expect.isTrue (positions.ToArray() |> Array.forall (fun position -> position.XAdvance = 0 && position.YAdvance = 0 && position.XOffset = 0 && position.YOffset = 0)) "zeroed"
            Expect.equal buffer.GlyphPositions.Length 2 "the copy"
        }
        test "Add, ClearContents and Reset keep their upstream effects" {
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.Add(65u, 7u)
            Expect.equal (buffer.Length, buffer.GlyphInfos[0].Cluster) (1, 7u) "one glyph"
            Expect.equal buffer.ContentType HarfBuzzSharp.ContentType.Invalid "hb_buffer_add does not set the content type"
            buffer.ClearContents()
            Expect.equal (buffer.Length, buffer.ContentType) (0, HarfBuzzSharp.ContentType.Invalid) "cleared"
            buffer.AddUtf16 "abc"
            buffer.Reset()
            Expect.equal (buffer.Length, buffer.ContentType) (0, HarfBuzzSharp.ContentType.Invalid) "reset"
        }
        test "a second AddUtf16 of the same text after Reset allocates nothing" {
            let text = String('a', 1000)
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.AddUtf16 text
            buffer.Reset()
            let before = GC.GetAllocatedBytesForCurrentThread()
            buffer.AddUtf16 text
            let after = GC.GetAllocatedBytesForCurrentThread()
            Expect.equal (after - before) 0L "bytes allocated by the second add"
            Expect.equal buffer.Length 1000 "the text"
        }
    ]
