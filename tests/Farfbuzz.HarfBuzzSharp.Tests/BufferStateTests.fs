module Farfbuzz.HarfBuzzSharp.Tests.BufferStateTests

open System
open System.Runtime.InteropServices
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>One operation of the random sequences, run on the facade buffer and on the oracle's.</summary>
type Operation =
    | AddText of text: string * offset: int * length: int
    | Reverse
    | ReverseRange of start: int * stop: int
    | ReverseClusters
    | SetPositions
    | SetLength of int
    | AppendSource
    | SetDirection of int
    | SetScript of uint32
    | ClearContents

/// <summary>Everything the sequences compare: glyphs, positions (when requested), segment properties.</summary>
type Snapshot =
    { Infos: (uint32 * uint32 * uint32) list
      Positions: int list
      Direction: int
      Script: uint32
      Language: string option }

/// <summary>Writes one position of a buffer through the live span.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="index">The glyph index.</param>
/// <param name="position">The position.</param>
let private writePosition (buffer: HarfBuzzSharp.Buffer) (index: int) (position: HarfBuzzSharp.GlyphPosition) =
    let span = buffer.GetGlyphPositionSpan()
    MemoryMarshal.GetReference(span.Slice index) <- position

/// <summary>The text the append source holds.</summary>
let private sourceText = "abcक्ष xyz"

/// <summary>The oracle's language name of a buffer.</summary>
/// <param name="buffer">The native buffer.</param>
/// <returns>The name, none for the invalid language.</returns>
let private oracleLanguage (buffer: nativeint) =
    Option.ofObj (Marshal.PtrToStringAnsi (Native.hb_language_to_string (Native.hb_buffer_get_language buffer)))

/// <summary>Reads the oracle buffer as a snapshot.</summary>
/// <param name="buffer">The native buffer.</param>
/// <param name="positions">Whether positions were requested.</param>
/// <returns>The snapshot.</returns>
let private oracleSnapshot (buffer: nativeint) (positions: bool) =
    let mutable length = 0u
    let infos = NativeText.copyFrom<Farfbuzz.GlyphInfo> (Native.hb_buffer_get_glyph_infos(buffer, &length)) (int length)
    let moved =
        match positions with
        | false -> []
        | true ->
            let values = Array.zeroCreate<int> (5 * int length)
            Native.farfbuzz_oracle_buffer_positions(buffer, values)
            List.ofArray values
    { Infos = infos |> Array.map (fun info -> info.Codepoint, info.Mask, info.Cluster) |> List.ofArray
      Positions = moved
      Direction = Native.hb_buffer_get_direction buffer
      Script = Native.hb_buffer_get_script buffer
      Language = oracleLanguage buffer }

/// <summary>Reads the facade buffer as a snapshot.</summary>
/// <param name="buffer">The facade buffer.</param>
/// <param name="positions">Whether positions were requested.</param>
/// <returns>The snapshot.</returns>
let private facadeSnapshot (buffer: HarfBuzzSharp.Buffer) (positions: bool) =
    let moved =
        match positions with
        | false -> []
        | true ->
            buffer.GlyphPositions
            |> Array.collect (fun position -> [| position.XAdvance; position.YAdvance; position.XOffset; position.YOffset; 0 |])
            |> List.ofArray
    use language = buffer.Language
    { Infos = buffer.GlyphInfos |> Array.map (fun info -> info.Codepoint, info.Mask, info.Cluster) |> List.ofArray
      Positions = moved
      Direction = int buffer.Direction
      Script = uint32 buffer.Script
      Language = Option.ofObj language.Name }

/// <summary>Runs one operation on both buffers; positions are only read once <c>SetPositions</c> has asked for them.</summary>
/// <param name="facade">The facade buffer.</param>
/// <param name="oracle">The native buffer.</param>
/// <param name="positions">Whether positions are requested so far.</param>
/// <param name="operation">The operation.</param>
/// <returns>Whether positions are requested afterwards.</returns>
let private apply (facade: HarfBuzzSharp.Buffer) (oracle: nativeint) (positions: bool) (operation: Operation) =
    match operation with
    | AddText (text, offset, length) ->
        facade.AddUtf16(text, offset, length)
        Native.farfbuzz_oracle_buffer_add_utf16(oracle, text.ToCharArray() |> Array.map uint16, text.Length, offset, length)
        positions
    | Reverse ->
        facade.Reverse()
        Native.hb_buffer_reverse oracle
        positions
    | ReverseRange (start, stop) ->
        facade.ReverseRange(start, stop)
        Native.hb_buffer_reverse_range(oracle, uint32 start, uint32 (match stop with | -1 -> facade.Length | _ -> stop))
        positions
    | ReverseClusters ->
        facade.ReverseClusters()
        Native.hb_buffer_reverse_clusters oracle
        positions
    | SetPositions ->
        let mutable length = 0u
        let pointer = Native.hb_buffer_get_glyph_positions(oracle, &length)
        for index in 0 .. facade.Length - 1 do
            writePosition facade index (HarfBuzzSharp.GlyphPosition(XAdvance = index * 3 + 1, YOffset = -index))
            Marshal.WriteInt32(pointer, index * 20, index * 3 + 1)
            Marshal.WriteInt32(pointer, index * 20 + 12, -index)
        true
    | SetLength length ->
        facade.Length <- length
        Native.hb_buffer_set_length(oracle, uint32 length) |> ignore
        positions
    | AppendSource ->
        use source = new HarfBuzzSharp.Buffer()
        source.AddUtf16 sourceText
        let nativeSource = Native.hb_buffer_create ()
        Native.farfbuzz_oracle_buffer_add_utf16(nativeSource, sourceText.ToCharArray() |> Array.map uint16, sourceText.Length, 0, -1)
        facade.Append(source)
        Native.hb_buffer_append(oracle, nativeSource, 0u, UInt32.MaxValue)
        Native.hb_buffer_destroy nativeSource
        positions
    | SetDirection direction ->
        facade.Direction <- enum<HarfBuzzSharp.Direction> direction
        Native.hb_buffer_set_direction(oracle, direction)
        positions
    | SetScript script ->
        facade.Script <- HarfBuzzSharp.Script.Parse (Text.Encoding.ASCII.GetString [| byte (script >>> 24); byte (script >>> 16); byte (script >>> 8); byte script |])
        Native.hb_buffer_set_script(oracle, uint32 facade.Script)
        positions
    | ClearContents ->
        facade.ClearContents()
        Native.hb_buffer_clear_contents oracle
        false

/// <summary>Whether an operation may run on the current state: ranges stay inside the buffer, an append needs a Unicode buffer without positions,
/// and the text enters only a buffer that holds Unicode or nothing and has no positions (HarfBuzz leaves the positions of glyphs added after them undefined).</summary>
/// <param name="content">The facade buffer's content type.</param>
/// <param name="length">The buffer length.</param>
/// <param name="positions">Whether positions are requested.</param>
/// <param name="operation">The operation.</param>
/// <returns>True when the operation is valid.</returns>
let private valid (content: HarfBuzzSharp.ContentType) (length: int) (positions: bool) (operation: Operation) =
    match operation with
    | ReverseRange (start, stop) -> start <= length && stop <= length && (stop = -1 || start <= stop)
    | AppendSource -> not positions && content = HarfBuzzSharp.ContentType.Unicode
    | SetLength newLength -> newLength >= 0 && (newLength <= length || not positions)
    | AddText _ -> (length = 0 || content = HarfBuzzSharp.ContentType.Unicode) && not positions
    | Reverse | ReverseClusters | SetPositions | SetDirection _ | SetScript _ | ClearContents -> true

/// <summary>Generates one operation.</summary>
let private anyOperation =
    let text = Gens.noise [ 'a'; 'b'; ' '; 'é'; 'ж'; 'ك'; 'क'; '‌'; '\uD83D'; '\uDE00'; 'Z' ] 12
    Gen.frequency
        [ 5, gen { let! value = text in let! offset = Gen.choose (0, 14) in let! length = Gen.choose (-1, 14) in return AddText (value, offset, length) }
          2, Gen.constant Reverse
          2, gen { let! start = Gen.choose (0, 6) in let! stop = Gen.choose (-1, 14) in return ReverseRange (start, stop) }
          2, Gen.constant ReverseClusters
          1, Gen.constant SetPositions
          1, gen { let! length = Gen.choose (0, 20) in return SetLength length }
          1, Gen.constant AppendSource
          1, gen { let! direction = Gen.elements [ 0; 4; 5; 6; 7 ] in return SetDirection direction }
          1, gen { let! script = Gen.elements [ 0x4C61746Eu; 0x41726162u; 0x5A797979u; 0u ] in return SetScript script }
          1, Gen.constant ClearContents ]

/// <summary>Describes how two snapshots differ, field by field, shortened.</summary>
/// <param name="expected">The oracle's snapshot.</param>
/// <param name="actual">The facade's snapshot.</param>
/// <returns>The description.</returns>
let private describeDifference (expected: Snapshot) (actual: Snapshot) =
    let infos = match actual.Infos = expected.Infos with | true -> String.Empty | false -> $"  infos oracle {List.truncate 8 expected.Infos} facade {List.truncate 8 actual.Infos}\n"
    let positions = match actual.Positions = expected.Positions with | true -> String.Empty | false -> $"  positions oracle {List.truncate 10 expected.Positions} facade {List.truncate 10 actual.Positions}\n"
    let properties = match (actual.Direction, actual.Script, actual.Language) = (expected.Direction, expected.Script, expected.Language) with | true -> String.Empty | false -> $"  properties oracle {expected.Direction} {expected.Script} {expected.Language} facade {actual.Direction} {actual.Script} {actual.Language}\n"
    infos + positions + properties

/// <summary>Runs a sequence on both buffers and reports the first operation after which they differ.</summary>
/// <param name="operations">The operations.</param>
/// <returns>None when they agree throughout.</returns>
let private firstDifference (operations: Operation list) =
    use facade = new HarfBuzzSharp.Buffer()
    let oracle = Native.hb_buffer_create ()
    let rec run (positions: bool) (done': Operation list) (rest: Operation list) =
        match rest with
        | [] -> None
        | operation :: tail ->
            match valid facade.ContentType facade.Length positions operation with
            | false -> run positions done' tail
            | true ->
                let moved = apply facade oracle positions operation
                let expected = oracleSnapshot oracle moved
                let actual = facadeSnapshot facade moved
                match actual = expected with
                | true -> run moved (operation :: done') tail
                | false ->
                    let steps = List.rev (operation :: done') |> List.map (fun item -> sprintf "%A" item) |> String.concat "; "
                    Some ("after " + steps + ":\n" + describeDifference expected actual)
    let outcome = run false [] operations
    Native.hb_buffer_destroy oracle
    outcome

/// <summary>The P10.S07 cases: buffer properties, guessing, reversing and appending.</summary>
[<Tests>]
let tests =
    testList "P10.S07" [
        test "a new or reset buffer has the upstream defaults and every property reads back" {
            use buffer = new HarfBuzzSharp.Buffer()
            use language = buffer.Language
            Expect.equal (buffer.Direction, uint32 buffer.Script, buffer.Flags, buffer.ClusterLevel) (HarfBuzzSharp.Direction.Invalid, 0u, HarfBuzzSharp.BufferFlags.Default, HarfBuzzSharp.ClusterLevel.MonotoneGraphemes) "defaults"
            Expect.equal (buffer.ReplacementCodepoint, buffer.InvisibleGlyph, language.Name) (0xFFFDu, 0u, null) "replacement, invisible glyph and the invalid language"
            buffer.Direction <- HarfBuzzSharp.Direction.RightToLeft
            buffer.Script <- HarfBuzzSharp.Script.Arabic
            buffer.Flags <- HarfBuzzSharp.BufferFlags.BeginningOfText ||| HarfBuzzSharp.BufferFlags.EndOfText
            buffer.ClusterLevel <- HarfBuzzSharp.ClusterLevel.Characters
            buffer.ReplacementCodepoint <- 0x3Fu
            buffer.InvisibleGlyph <- 3u
            use farsi = new HarfBuzzSharp.Language "fa-IR"
            buffer.Language <- farsi
            use read = buffer.Language
            Expect.equal (buffer.Direction, buffer.Script, buffer.ClusterLevel, buffer.ReplacementCodepoint, buffer.InvisibleGlyph, read.Name) (HarfBuzzSharp.Direction.RightToLeft, HarfBuzzSharp.Script.Arabic, HarfBuzzSharp.ClusterLevel.Characters, 0x3Fu, 3u, "fa-ir") "read back"
            buffer.ClearContents()
            Expect.equal (buffer.Direction, buffer.Flags, buffer.ClusterLevel, buffer.ReplacementCodepoint) (HarfBuzzSharp.Direction.Invalid, HarfBuzzSharp.BufferFlags.BeginningOfText ||| HarfBuzzSharp.BufferFlags.EndOfText, HarfBuzzSharp.ClusterLevel.Characters, 0x3Fu) "ClearContents keeps flags, cluster level and replacement, clears the segment properties"
            buffer.Reset()
            Expect.equal (buffer.Flags, buffer.ClusterLevel, buffer.ReplacementCodepoint, buffer.InvisibleGlyph) (HarfBuzzSharp.BufferFlags.Default, HarfBuzzSharp.ClusterLevel.MonotoneGraphemes, 0xFFFDu, 0u) "Reset restores them"
        }
        testCase "GuessSegmentProperties equals the oracle on 2 000 generated texts with and without preset properties" (fun () ->
            OracleSession.ensure ()
            let text = Gens.noise [ 'a'; ' '; '1'; 'ك'; 'ב'; 'क'; '́'; '؀'; '中'; '\uD83D'; '\uDE00'; '‌'; '?' ] 8
            let cases =
                Gens.sample 2000 (gen {
                    let! value = text
                    let! direction = Gen.elements [ 0; 0; 4; 5; 6 ]
                    let! script = Gen.elements [ 0u; 0u; 0x4C61746Eu ]
                    let! language = Gen.elements [ String.Empty; "en"; "ar-EG" ]
                    return value, direction, script, language })
            let mismatches =
                cases
                |> List.choose (fun (value, direction, script, language) ->
                    use buffer = new HarfBuzzSharp.Buffer()
                    buffer.AddUtf16 value
                    buffer.Direction <- enum<HarfBuzzSharp.Direction> direction
                    buffer.Script <- HarfBuzzSharp.Script.Parse (match script with | 0u -> String.Empty | _ -> "Latn")
                    use preset = new HarfBuzzSharp.Language(language)
                    buffer.Language <- preset
                    buffer.GuessSegmentProperties()
                    let native = Native.hb_buffer_create ()
                    Native.farfbuzz_oracle_buffer_add_utf16(native, value.ToCharArray() |> Array.map uint16, value.Length, 0, -1)
                    Native.hb_buffer_set_direction(native, direction)
                    Native.hb_buffer_set_script(native, script)
                    let languageBytes = Text.Encoding.UTF8.GetBytes language
                    Native.hb_buffer_set_language(native, Native.hb_language_from_string(languageBytes, languageBytes.Length))
                    Native.hb_buffer_guess_segment_properties native
                    let expected = oracleSnapshot native false
                    Native.hb_buffer_destroy native
                    let actual = facadeSnapshot buffer false
                    match actual.Direction = expected.Direction && actual.Script = expected.Script && actual.Language = expected.Language with
                    | true -> None
                    | false -> Some $"{value}: oracle {expected.Direction} {expected.Script} {expected.Language}, facade {actual.Direction} {actual.Script} {actual.Language}")
                |> List.truncate 5
            Expect.isEmpty mismatches (String.concat "\n" mismatches))
        test "GuessSegmentProperties needs Unicode content and a guess names the C locale language" {
            use buffer = new HarfBuzzSharp.Buffer()
            let error =
                try
                    buffer.GuessSegmentProperties()
                    None
                with :? InvalidOperationException as error -> Some error.Message
            Expect.equal error (Some "ContentType must be of type Unicode.") "empty buffer"
            buffer.AddUtf16 "abc"
            buffer.GuessSegmentProperties()
            use language = buffer.Language
            Expect.equal (buffer.Direction, buffer.Script, language.Name) (HarfBuzzSharp.Direction.LeftToRight, HarfBuzzSharp.Script.Latin, "c") "Latin, left to right, language c"
        }
        testCase "2 000 random operation sequences leave the facade and the oracle buffers equal" (fun () ->
            OracleSession.ensure ()
            
            printfn "P10.S07: operation sequences drawn from FsCheck's random generator, which Gen.sample does not let a test seed; a failing sequence is printed in full"
            let sequences = Gens.sample 2000 (Gen.listOf anyOperation)
            let failures = sequences |> List.choose firstDifference |> List.truncate 3
            Expect.isEmpty failures (String.concat "\n" failures))
        test "Reverse, ReverseRange and ReverseClusters move glyphs and positions together; ReverseRange takes -1 for the end" {
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.AddUtf16 "abcd"
            for index in 0 .. 3 do
                writePosition buffer index (HarfBuzzSharp.GlyphPosition(XAdvance = index))
            buffer.ReverseRange(1, -1)
            Expect.equal (buffer.GlyphInfos |> Array.map (fun info -> char info.Codepoint) |> String) "adcb" "range to the end"
            Expect.equal (buffer.GlyphPositions |> Array.map (fun position -> position.XAdvance)) [| 0; 3; 2; 1 |] "positions follow"
            buffer.Reverse()
            Expect.equal (buffer.GlyphInfos |> Array.map (fun info -> char info.Codepoint) |> String) "bcda" "whole"
            buffer.Length <- 2
            Expect.equal (buffer.Length, buffer.GlyphInfos |> Array.map (fun info -> char info.Codepoint) |> String) (2, "bc") "shrunk"
            buffer.Length <- 0
            Expect.equal buffer.ContentType HarfBuzzSharp.ContentType.Invalid "an emptied buffer has no content type"
        }
        test "Append checks its arguments as upstream does" {
            use target = new HarfBuzzSharp.Buffer()
            use empty = new HarfBuzzSharp.Buffer()
            let emptySource =
                try
                    target.Append empty
                    None
                with :? ArgumentException as error -> Some (error.ParamName, error.Message.StartsWith "Buffer must be non empty.")
            Expect.equal emptySource (Some ("buffer", true)) "empty source"
            use source = new HarfBuzzSharp.Buffer()
            source.AddUtf16 "xy"
            target.ContentType <- HarfBuzzSharp.ContentType.Glyphs
            let mixed =
                try
                    target.Append source
                    None
                with :? InvalidOperationException as error -> Some error.Message
            Expect.equal mixed (Some "ContentType must be of same type.") "content types differ"
        }
    ]
