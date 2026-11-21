module Farfbuzz.Diff.GlyphRunDiff

open System
open System.Runtime.InteropServices
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>One compared buffer state.</summary>
type private RunState =
    { Codepoints: uint32 array
      Clusters: uint32 array
      Positions: int32 array
      HavePositions: bool
      Before: uint32 array
      After: uint32 array
      Content: uint32
      Direction: int
      Script: uint32
      Language: string }

/// <summary>Reads the oracle state.</summary>
/// <param name="buffer">The buffer.</param>
/// <returns>The state.</returns>
let private oracleState (buffer: nativeint) =
    let count = max 1 (int (Native.hb_buffer_get_length buffer))
    let codepoints = Array.zeroCreate count
    let clusters = Array.zeroCreate count
    let before = Array.zeroCreate 5
    let after = Array.zeroCreate 5
    let mutable infosLength = 0u
    let mutable beforeLength = 0u
    let mutable afterLength = 0u
    let mutable content = 0u
    Native.farfbuzz_oracle_buffer_state(buffer, codepoints, clusters, before, after, &infosLength, &beforeLength, &afterLength, &content)
    let havePositions = Native.farfbuzz_oracle_buffer_have_positions buffer <> 0
    let positions = Array.zeroCreate (max 1 (int infosLength * 5))
    match havePositions with
    | true -> Native.farfbuzz_oracle_buffer_positions(buffer, positions)
    | false -> ()
    let languagePointer = Native.hb_buffer_get_language buffer
    { Codepoints = codepoints[0 .. int infosLength - 1]
      Clusters = clusters[0 .. int infosLength - 1]
      Positions = match havePositions with | true -> positions[0 .. int infosLength * 5 - 1] | false -> [||]
      HavePositions = havePositions
      Before = before[0 .. int beforeLength - 1]
      After = after[0 .. int afterLength - 1]
      Content = content
      Direction = Native.hb_buffer_get_direction buffer
      Script = Native.hb_buffer_get_script buffer
      Language =
        match languagePointer with
        | 0n -> String.Empty
        | pointer ->
            match Marshal.PtrToStringUTF8 pointer with
            | null -> String.Empty
            | text -> text }

/// <summary>Flattens positions for comparison.</summary>
/// <param name="run">The run.</param>
/// <returns>Five fields per glyph.</returns>
let private flattenPositions (run: GlyphRun) =
    match run.HavePositions with
    | false -> [||]
    | true -> run.Positions.AsSpan().ToArray() |> Array.collect (fun position -> [| position.XAdvance; position.YAdvance; position.XOffset; position.YOffset; 0 |])

/// <summary>Reads a Farfbuzz run's state.</summary>
/// <param name="run">The run.</param>
/// <returns>The state.</returns>
let private ourState (run: GlyphRun) =
    { Codepoints = run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)
      Clusters = run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Cluster)
      Positions = flattenPositions run
      HavePositions = run.HavePositions
      Before = run.ContextBefore.AsSpan().ToArray()
      After = run.ContextAfter.AsSpan().ToArray()
      Content = match run.ContentType with | ContentType.NoContent -> 0u | ContentType.UnicodeContent -> 1u | ContentType.GlyphContent -> 2u
      Direction = int (Direction.value run.SegmentProperties.Direction)
      Script = uint32 run.SegmentProperties.Script
      Language = match run.SegmentProperties.Language with | Language.NoLanguage -> String.Empty | Language.Bcp47 text -> text }

/// <summary>Builds a run from explicit state.</summary>
/// <param name="codepoints">The code points.</param>
/// <param name="clusters">The clusters.</param>
/// <param name="positions">Five position fields per glyph, when present.</param>
/// <param name="havePositions">Whether positions exist.</param>
/// <param name="before">The pre-context.</param>
/// <param name="after">The post-context.</param>
/// <param name="content">The content type.</param>
/// <param name="props">The segment properties.</param>
/// <returns>The run.</returns>
let private makeRun (codepoints: uint32 array) (clusters: uint32 array) (positions: int32 array) (havePositions: bool) (before: uint32 array) (after: uint32 array) (content: ContentType) (props: SegmentProperties) =
    let infos = codepoints |> Array.mapi (fun index codepoint -> { Codepoint = codepoint; Mask = 0u; Cluster = clusters[index]; Var1 = 0u; Var2 = 0u })
    let positionInfos =
        match havePositions with
        | true ->
            Array.init codepoints.Length (fun index ->
                { XAdvance = positions[index * 5]; YAdvance = positions[index * 5 + 1]; XOffset = positions[index * 5 + 2]; YOffset = positions[index * 5 + 3]; Var = uint32 positions[index * 5 + 4] })
        | false -> [||]
    { GlyphRun.empty with
        Infos = Sealed.ofSpan (ReadOnlySpan infos)
        Positions = Sealed.ofSpan (ReadOnlySpan positionInfos)
        HavePositions = havePositions
        ContextBefore = Sealed.ofSpan (ReadOnlySpan before)
        ContextAfter = Sealed.ofSpan (ReadOnlySpan after)
        ContentType = content
        SegmentProperties = props }

/// <summary>Seeds an oracle buffer from explicit state.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="codepoints">The code points.</param>
/// <param name="clusters">The clusters.</param>
/// <param name="positions">Five position fields per glyph.</param>
/// <param name="havePositions">Whether positions exist.</param>
/// <param name="before">The pre-context.</param>
/// <param name="after">The post-context.</param>
/// <param name="content">The content type.</param>
/// <param name="props">The segment properties.</param>
let private seed (buffer: nativeint) (codepoints: uint32 array) (clusters: uint32 array) (positions: int32 array) (havePositions: bool) (before: uint32 array) (after: uint32 array) (content: ContentType) (props: SegmentProperties) =
    let flattened =
        match havePositions with
        | true -> positions
        | false -> [||]
    let contentValue = match content with | ContentType.NoContent -> 0u | ContentType.UnicodeContent -> 1u | ContentType.GlyphContent -> 2u
    Native.farfbuzz_oracle_buffer_seed(buffer, codepoints, clusters, uint32 codepoints.Length, flattened, (if havePositions then 1 else 0), before, uint32 before.Length, after, uint32 after.Length, contentValue)
    Native.hb_buffer_set_direction(buffer, int (Direction.value props.Direction))
    Native.hb_buffer_set_script(buffer, uint32 props.Script)
    match props.Language with
    | Language.NoLanguage -> ()
    | Language.Bcp47 text ->
        let bytes = Text.Encoding.ASCII.GetBytes text
        Native.hb_buffer_set_language(buffer, Native.hb_language_from_string(bytes, bytes.Length))

/// <summary>The process default language, as the oracle reports it.</summary>
let private defaultLanguage =
    lazy
        (match Marshal.PtrToStringUTF8(Native.hb_language_get_default()) with
         | null -> Language.NoLanguage
         | text -> Language.ofString text)

/// <summary>The script samples used for guessing.</summary>
let private scriptSamples = [| 0x41u; 0x627u; 0x5D0u; 0x915u; 0x4E00u; 0x3042u; 0x30A2u; 0x391u; 0x410u; 0xE01u; 0x985u; 0xA05u; 0xA85u; 0xB05u; 0xB85u; 0xC05u; 0xC85u; 0xD05u; 0xD85u; 0x1000u; 0x10A0u; 0xAC00u; 0x1200u; 0x13A0u; 0x1401u; 0x1780u; 0x1820u; 0x710u; 0x780u; 0x20u; 0x300u; 0x10FFFFu |]

/// <summary>A random guess input.</summary>
let private guessInput =
    gen {
        let! length = Gen.choose (0, 8)
        let! text = Gen.listOfLength length (Gen.elements scriptSamples)
        let! direction = Gen.elements [ Direction.Invalid; Direction.Ltr; Direction.Rtl; Direction.Ttb; Direction.Btt ]
        let! script = Gen.elements [ Scripts.Invalid; Scripts.Arabic; Scripts.Latin; Scripts.Common; Scripts.Inherited ]
        let! language = Gen.elements [ Language.NoLanguage; Language.Bcp47 "en"; Language.Bcp47 "ar"; Language.Bcp47 "fa" ]
        return struct (Array.ofList text, direction, script, language)
    }

/// <summary>A random reverse input.</summary>
let private reverseInput =
    gen {
        let! length = Gen.choose (0, 12)
        let! codepoints = Gen.listOfLength length (Gen.elements [ 0u; 0x41u; 0x62u; 0x627u; 0x1F600u ])
        let! clusters = Gen.listOfLength length (Gen.choose (0, 5) |> Gen.map uint32)
        let! havePositions = Gen.elements [ true; false ]
        let! operation = Gen.choose (0, 2)
        let! start = Gen.choose (0, length + 3)
        let! finish = Gen.choose (0, length + 3)
        return struct (Array.ofList codepoints, Array.ofList clusters, havePositions, operation, start, finish)
    }

/// <summary>A random append input.</summary>
let private appendInput =
    gen {
        let! sourceLength = Gen.choose (0, 6)
        let! targetLength = Gen.choose (0, 6)
        let! sourceCodepoints = Gen.listOfLength sourceLength (Gen.elements [ 0u; 0x41u; 0x62u; 0x627u ])
        let! targetCodepoints = Gen.listOfLength targetLength (Gen.elements [ 0u; 0x43u; 0x64u; 0x5D0u ])
        let! sourceClusters = Gen.listOfLength sourceLength (Gen.choose (0, 4) |> Gen.map uint32)
        let! targetClusters = Gen.listOfLength targetLength (Gen.choose (0, 4) |> Gen.map uint32)
        let! havePositions = Gen.elements [ true; false ]
        let! sourceBeforeLength = Gen.choose (0, 5)
        let! targetBeforeLength = Gen.choose (0, 5)
        let! sourceAfterLength = Gen.choose (0, 5)
        let! targetAfterLength = Gen.choose (0, 5)
        let! sourceBefore = Gen.listOfLength sourceBeforeLength (Gen.elements [ 0x61u; 0x62u; 0x63u ])
        let! targetBefore = Gen.listOfLength targetBeforeLength (Gen.elements [ 0x64u; 0x65u; 0x66u ])
        let! sourceAfter = Gen.listOfLength sourceAfterLength (Gen.elements [ 0x78u; 0x79u; 0x7Au ])
        let! targetAfter = Gen.listOfLength targetAfterLength (Gen.elements [ 0x31u; 0x32u; 0x33u ])
        let! start = Gen.choose (0, sourceLength + 2)
        let! finish = Gen.choose (0, sourceLength + 2)
        let! direction = Gen.elements [ Direction.Invalid; Direction.Ltr; Direction.Rtl ]
        let! script = Gen.elements [ Scripts.Invalid; Scripts.Arabic; Scripts.Latin ]
        return struct (Array.ofList sourceCodepoints, Array.ofList sourceClusters, Array.ofList targetCodepoints, Array.ofList targetClusters, havePositions, Array.ofList sourceBefore, Array.ofList targetBefore, Array.ofList sourceAfter, Array.ofList targetAfter, start, finish, direction, script)
    }

/// <summary>The position fields for a seed.</summary>
/// <param name="length">The glyph count.</param>
/// <returns>Five fields per glyph.</returns>
let private randomPositions (length: int) =
    Array.init (length * 5) (fun index -> index % 7 - 3)

/// <summary>The P3.S08 buffer differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S08" [
        sampled "guess segment properties" guessInput 10_000
            (fun struct (text, direction, script, language) ->
                makeRun text (Array.init text.Length uint32) [||] false [||] [||] ContentType.UnicodeContent { Direction = direction; Script = script; Language = language }
                |> GlyphRun.guessSegmentProperties defaultLanguage.Value
                |> ourState)
            (fun struct (text, direction, script, language) ->
                let buffer = Native.hb_buffer_create ()
                seed buffer text (Array.init text.Length uint32) [||] false [||] [||] ContentType.UnicodeContent { Direction = direction; Script = script; Language = language }
                Native.hb_buffer_guess_segment_properties buffer
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
        sampled "reverse family" reverseInput 10_000
            (fun struct (codepoints, clusters, havePositions, operation, start, finish) ->
                let run = makeRun codepoints clusters (randomPositions codepoints.Length) havePositions [||] [||] ContentType.GlyphContent { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.NoLanguage }
                let reversed =
                    match operation with
                    | 0 -> GlyphRun.reverse run
                    | 1 -> GlyphRun.reverseRange start finish run
                    | _ -> GlyphRun.reverseClusters run
                ourState reversed)
            (fun struct (codepoints, clusters, havePositions, operation, start, finish) ->
                let buffer = Native.hb_buffer_create ()
                seed buffer codepoints clusters (randomPositions codepoints.Length) havePositions [||] [||] ContentType.GlyphContent { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.NoLanguage }
                match operation with
                | 0 -> Native.hb_buffer_reverse buffer
                | 1 -> Native.hb_buffer_reverse_range(buffer, uint32 start, uint32 finish)
                | _ -> Native.hb_buffer_reverse_clusters buffer
                let state = oracleState buffer
                Native.hb_buffer_destroy buffer
                state)
        sampled "append" appendInput 10_000
            (fun struct (sourceCodepoints, sourceClusters, targetCodepoints, targetClusters, havePositions, sourceBefore, targetBefore, sourceAfter, targetAfter, start, finish, direction, script) ->
                let source = makeRun sourceCodepoints sourceClusters (randomPositions sourceCodepoints.Length) havePositions sourceBefore sourceAfter ContentType.UnicodeContent { Direction = direction; Script = script; Language = Language.Bcp47 "ar" }
                let target = makeRun targetCodepoints targetClusters (randomPositions targetCodepoints.Length) havePositions targetBefore targetAfter ContentType.UnicodeContent { Direction = Direction.Invalid; Script = Scripts.Latin; Language = Language.NoLanguage }
                GlyphRun.append source start finish target |> ourState)
            (fun struct (sourceCodepoints, sourceClusters, targetCodepoints, targetClusters, havePositions, sourceBefore, targetBefore, sourceAfter, targetAfter, start, finish, direction, script) ->
                let buffer = Native.hb_buffer_create ()
                let source = Native.hb_buffer_create ()
                let sourceProps = { Direction = direction; Script = script; Language = Language.Bcp47 "ar" }
                let targetProps = { Direction = Direction.Invalid; Script = Scripts.Latin; Language = Language.NoLanguage }
                seed buffer targetCodepoints targetClusters (randomPositions targetCodepoints.Length) havePositions targetBefore targetAfter ContentType.UnicodeContent targetProps
                seed source sourceCodepoints sourceClusters (randomPositions sourceCodepoints.Length) havePositions sourceBefore sourceAfter ContentType.UnicodeContent sourceProps
                Native.hb_buffer_append(buffer, source, uint32 start, uint32 finish)
                let state = oracleState buffer
                Native.hb_buffer_destroy source
                Native.hb_buffer_destroy buffer
                state)
    ]
