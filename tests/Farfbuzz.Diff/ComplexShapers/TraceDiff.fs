module Farfbuzz.Diff.ComplexShapers.TraceDiff

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Oracle
open Farfbuzz.Oracle.Native
open Farfbuzz.Oracle.NativeText
open Farfbuzz.Diff
open Farfbuzz.Diff.ComplexShapers
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>A string option's UTF-8 bytes, or null for an absent option.</summary>
/// <param name="value">The option value.</param>
/// <returns>The bytes, or null.</returns>
let private utf8 (value: string voption) : byte array =
    match value with
    | ValueSome text -> Encoding.UTF8.GetBytes text
    | ValueNone -> Unchecked.defaultof<byte array>

/// <summary>The byte length to pass with <see cref="utf8"/>: -1 for null, which HarfBuzz treats as invalid.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The length.</returns>
let private lengthOf (bytes: byte array) =
    match obj.ReferenceEquals(bytes, null) with
    | true -> -1
    | false -> bytes.Length

/// <summary>Shapes a request through the oracle with a message callback, returning the glyph infos captured when the
/// message fires and, when requested, the final infos.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <param name="message">The message whose first snapshot is captured.</param>
/// <param name="alsoFinal">Whether the final infos are captured too.</param>
/// <returns>The message snapshot and the final infos, each none when not captured.</returns>
let private traceRun (fontFile: string) (request: ShapeRequest) (message: string) (alsoFinal: bool) : struct (GlyphInfo array voption * GlyphInfo array voption) =
    let font = OracleFaces.font { Path = fontFile; Suite = String.Empty; Index = 0 } 0u
    let buffer = Native.hb_buffer_create ()
    let mutable snapshot : GlyphInfo array voption = ValueNone
    let callback =
        BufferMessageFunc(fun bufferHandle _ messagePtr _ ->
            let text = Marshal.PtrToStringAnsi messagePtr
            match text with
            | null -> 1
            | text when text = message ->
                let mutable length = 0u
                snapshot <- ValueSome (copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(bufferHandle, &length)) (int length))
                1
            | _ -> 1)
    Native.hb_buffer_set_message_func(buffer, callback, 0n, 0n)
    Native.hb_buffer_add_codepoints(buffer, request.Text, request.Text.Length, 0u, request.Text.Length)
    let direction = utf8 request.Direction
    let script = utf8 request.Script
    let language = Encoding.UTF8.GetBytes request.Language
    Native.hb_buffer_set_direction(buffer, Native.hb_direction_from_string(direction, lengthOf direction))
    Native.hb_buffer_set_script(buffer, Native.hb_script_from_string(script, lengthOf script))
    Native.hb_buffer_set_language(buffer, Native.hb_language_from_string(language, language.Length))
    Native.hb_buffer_guess_segment_properties buffer
    let shapers = [| "ot"; Unchecked.defaultof<string> |]
    Native.hb_shape_full(font, buffer, Unchecked.defaultof<Feature array>, 0u, shapers) |> ignore
    let final =
        match alsoFinal with
        | false -> ValueNone
        | true ->
            let mutable length = 0u
            ValueSome (copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(buffer, &length)) (int length))
    Native.hb_buffer_set_message_func(buffer, Unchecked.defaultof<BufferMessageFunc>, 0n, 0n)
    Native.hb_buffer_destroy buffer
    GC.KeepAlive callback
    struct (snapshot, final)

/// <summary>
/// Compares two captures of the same random text: the glyph infos at the first occurrence of a shaping message, and
/// the final infos when requested, both read through the oracle's message callback.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="options">The option field.</param>
/// <param name="alphabet">The code points to draw from.</param>
/// <param name="maxLength">The longest text.</param>
/// <param name="message">The message whose snapshot is captured.</param>
/// <param name="alsoFinal">Whether the final infos are compared too.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let sampled (name: string) (fontFile: string) (options: string) (alphabet: ImmutableArray<uint32>) (maxLength: int) (message: string) (alsoFinal: bool) (count: int) : Test =
    Differential.sampled name (ShapeDiff.cases fontFile options alphabet maxLength) count
        (fun spec -> traceRun fontFile spec message alsoFinal)
        (fun spec -> traceRun fontFile spec message alsoFinal)

/// <summary>The code points the trace self-check draws from: Tamil consonants, vowels, vowel signs, the virama and the
/// two joiners, so a random string forms an Indic syllable and the initial reordering fires.</summary>
let private tamilAlphabet : ImmutableArray<uint32> =
    ImmutableArray.CreateRange (
        [ 0x0B85u; 0x0B86u; 0x0B87u; 0x0B89u; 0x0B8Eu; 0x0B8Fu; 0x0B90u; 0x0B92u; 0x0B93u; 0x0B94u ]
        @ [ 0x0B95u .. 0x0BB9u ]
        @ [ 0x0BBEu; 0x0BBFu; 0x0BC0u; 0x0BC1u; 0x0BC2u; 0x0BC6u; 0x0BC7u; 0x0BC8u; 0x0BCAu; 0x0BCBu; 0x0BCCu; 0x0BCDu
            0x200Cu; 0x200Du ])

/// <summary>The P6.S02 trace self-check: the oracle against itself on random Tamil text, at the start of the Indic
/// initial reordering.</summary>
/// <returns>The test.</returns>
let tests () : Test =
    sampled "trace self" (TestFonts.named "54674a3111d209fb6be0ed31745314b7a8d2c244.ttf").Path String.Empty tamilAlphabet 12 "start reordering indic initial" false 1000

/// <summary>Shapes a request through the oracle with a message callback, returning the glyph infos captured the first time
/// each of some messages fires; the buffer takes the request's cluster level and flags.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="request">The request.</param>
/// <param name="messages">The messages whose first snapshot is captured; a message also matches one that continues it after a space, as
/// <c>start table GSUB</c> matches <c>start table GSUB script tag 'khmr'</c>.</param>
/// <returns>One snapshot per message, none for a message that did not fire.</returns>
let traceMessages (fontFile: string) (request: ShapeRequest) (messages: string list) : GlyphInfo array voption list =
    let font = OracleFaces.font { Path = fontFile; Suite = String.Empty; Index = 0 } 0u
    let buffer = Native.hb_buffer_create ()
    let snapshots = Array.create messages.Length ValueNone
    let callback =
        BufferMessageFunc(fun bufferHandle _ messagePtr _ ->
            let text = Marshal.PtrToStringAnsi messagePtr |> nonNull
            match messages |> List.tryFindIndex (fun message -> message = text || text.StartsWith(message + " ")) with
            | Some index when ValueOption.isNone snapshots[index] ->
                let mutable length = 0u
                snapshots[index] <- ValueSome (copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(bufferHandle, &length)) (int length))
                1
            | Some _ | None -> 1)
    Native.hb_buffer_set_message_func(buffer, callback, 0n, 0n)
    Native.hb_buffer_add_codepoints(buffer, request.Text, request.Text.Length, 0u, request.Text.Length)
    let direction = utf8 request.Direction
    let script = utf8 request.Script
    let language = Encoding.UTF8.GetBytes request.Language
    Native.hb_buffer_set_direction(buffer, Native.hb_direction_from_string(direction, lengthOf direction))
    Native.hb_buffer_set_script(buffer, Native.hb_script_from_string(script, lengthOf script))
    Native.hb_buffer_set_language(buffer, Native.hb_language_from_string(language, language.Length))
    Native.hb_buffer_set_flags(buffer, uint32 request.Flags)
    Native.hb_buffer_set_cluster_level(buffer, request.ClusterLevel)
    Native.hb_buffer_guess_segment_properties buffer
    let shapers = [| "ot"; Unchecked.defaultof<string> |]
    Native.hb_shape_full(font, buffer, Unchecked.defaultof<Feature array>, 0u, shapers) |> ignore
    Native.hb_buffer_set_message_func(buffer, Unchecked.defaultof<BufferMessageFunc>, 0n, 0n)
    Native.hb_buffer_destroy buffer
    GC.KeepAlive callback
    snapshots |> Array.toList
