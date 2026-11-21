namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open System.Runtime.InteropServices
open Farfbuzz

/// <summary>Up to five context code points (hb_buffer_t::CONTEXT_LENGTH, hb-buffer.hh:109), nearest first.</summary>
[<Struct>]
type Context = { Length: int; C0: uint32; C1: uint32; C2: uint32; C3: uint32; C4: uint32 }

/// <summary>Receives decoded code points; the C# buffer implements it as a struct writing into its arrays.</summary>
type ICodepointSink<'S> =
    abstract Put: state: 'S * codepoint: uint32 * cluster: uint32 -> 'S

/// <summary>What one text entry produced: the sink state, the contexts, and whether the pre-context is to be installed.</summary>
[<Struct>]
type Added<'S> = { State: 'S; Pre: Context; Post: Context; PreInstalled: bool }

/// <summary>One text encoding's forward and backward decoder, <c>hb_utf8_t</c> and its siblings (hb-utf.hh).</summary>
type internal ICodec<'T> =
    abstract Next: text: ReadOnlySpan<'T> * index: int * replacement: uint32 -> struct (uint32 * int)
    abstract Previous: text: ReadOnlySpan<'T> * index: int * replacement: uint32 -> struct (uint32 * int)
    abstract Direct: text: ReadOnlySpan<'T> -> bool
    abstract Unit: text: ReadOnlySpan<'T> * index: int -> uint32

/// <summary>UTF-8, <c>hb_utf8_t</c>.</summary>
[<Struct>]
type internal Utf8Codec =
    interface ICodec<byte> with
        member _.Next (text, index, replacement) = Utf.next8 text index replacement |> Settled.value
        member _.Previous (text, index, replacement) = Utf.prev8 text index replacement |> Settled.value
        member _.Direct text = System.Text.Ascii.IsValid text
        member _.Unit (text, index) = uint32 text[index]

/// <summary>UTF-16, <c>hb_utf16_t</c>.</summary>
[<Struct>]
type internal Utf16Codec =
    interface ICodec<uint16> with
        member _.Next (text, index, replacement) = Utf.next16 text index replacement |> Settled.value
        member _.Previous (text, index, replacement) = Utf.prev16 text index replacement |> Settled.value
        member _.Direct _ = false
        member _.Unit (_, _) = 0u

/// <summary>UTF-32 with validation, <c>hb_utf32_t</c>.</summary>
[<Struct>]
type internal Utf32Codec =
    interface ICodec<uint32> with
        member _.Next (text, index, replacement) = Utf.next32 text index replacement true |> Settled.value
        member _.Previous (text, index, replacement) = Utf.prev32 text index replacement true |> Settled.value
        member _.Direct _ = false
        member _.Unit (_, _) = 0u

/// <summary>Code points taken as they are, <c>hb_utf32_novalidate_t</c>.</summary>
[<Struct>]
type internal CodepointCodec =
    interface ICodec<uint32> with
        member _.Next (text, index, replacement) = Utf.next32 text index replacement false |> Settled.value
        member _.Previous (text, index, replacement) = Utf.prev32 text index replacement false |> Settled.value
        member _.Direct _ = false
        member _.Unit (_, _) = 0u

module internal TextImpl =
    [<Literal>]
    let ContextLength = 5

    let noContext = { Length = 0; C0 = 0u; C1 = 0u; C2 = 0u; C3 = 0u; C4 = 0u }

    /// <summary>Appends a value to a context, <c>context[i][context_len[i]++] = u</c>.</summary>
    /// <param name="context">The context.</param>
    /// <param name="value">The code point.</param>
    /// <returns>The context with the value added.</returns>
    let append (context: Context) (value: uint32) =
        match context.Length with
        | 0 -> { context with Length = 1; C0 = value }
        | 1 -> { context with Length = 2; C1 = value }
        | 2 -> { context with Length = 3; C2 = value }
        | 3 -> { context with Length = 4; C3 = value }
        | _ -> { context with Length = 5; C4 = value }

    /// <summary>Clamps an item as <c>hb_buffer_add_utf</c> does (hb-buffer.cc:1880-1889): an item length of -1 means the rest of the text,
    /// any other negative length means none, the offset stops at the end of the text.</summary>
    /// <param name="textLength">The length of the whole text in code units.</param>
    /// <param name="itemOffset">The first code unit, not negative.</param>
    /// <param name="itemLength">The number of code units, or -1.</param>
    /// <returns>The offset and the length.</returns>
    let clamp (textLength: int) (itemOffset: int) (itemLength: int) =
        let wanted = if itemLength = -1 then textLength - itemOffset else itemLength
        let offset = min itemOffset textLength
        struct (offset, max 0 (min wanted (textLength - offset)))

    /// <summary>Collects the code points before the item, nearest first, up to the context length (hb-buffer.cc:1903-1913).</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="at">The index to read backwards from.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <param name="context">The context so far.</param>
    /// <returns>The context.</returns>
    [<TailCall>]
    let rec preContext<'T, 'C when 'C: struct and 'C :> ICodec<'T>> (text: ReadOnlySpan<'T>) (at: int) (replacement: uint32) (codec: 'C) (context: Context) =
        match at > 0 && context.Length < ContextLength with
        | false -> context
        | true ->
            let struct (value, before) = codec.Previous(text, at, replacement)
            preContext text before replacement codec (append context value)

    /// <summary>Collects the code points after the item, nearest first, up to the context length (hb-buffer.cc:1928-1935).</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="at">The index to read forwards from.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <param name="context">The context so far.</param>
    /// <returns>The context.</returns>
    [<TailCall>]
    let rec postContext<'T, 'C when 'C: struct and 'C :> ICodec<'T>> (text: ReadOnlySpan<'T>) (at: int) (replacement: uint32) (codec: 'C) (context: Context) =
        match at < text.Length && context.Length < ContextLength with
        | false -> context
        | true ->
            let struct (value, next) = codec.Next(text, at, replacement)
            postContext text next replacement codec (append context value)

    /// <summary>Puts the units of an item that decodes unit by unit into the sink, each unit being its own code point.</summary>
    /// <param name="item">The item's code units.</param>
    /// <param name="at">The index in the item.</param>
    /// <param name="offset">The offset of the item in the whole text.</param>
    /// <param name="codec">The decoder, which says what a unit is.</param>
    /// <param name="sink">The sink.</param>
    /// <param name="state">The sink state.</param>
    /// <returns>The final state.</returns>
    [<TailCall>]
    let rec directItems<'T, 'S, 'C, 'K when 'C: struct and 'C :> ICodec<'T> and 'K: struct and 'K :> ICodepointSink<'S>>
            (item: ReadOnlySpan<'T>) (at: int) (offset: int) (codec: 'C) (sink: 'K) (state: 'S) =
        match at >= item.Length with
        | true -> state
        | false -> directItems item (at + 1) offset codec sink (sink.Put(state, codec.Unit(item, at), uint32 (offset + at)))

    /// <summary>Collects the units before an index of a text that decodes unit by unit, nearest first, up to the context length.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="at">The index to read backwards from.</param>
    /// <param name="stop">The index the units are collected down to.</param>
    /// <param name="codec">The decoder, which says what a unit is.</param>
    /// <param name="context">The context so far.</param>
    /// <returns>The context.</returns>
    [<TailCall>]
    let rec directBefore<'T, 'C when 'C: struct and 'C :> ICodec<'T>> (text: ReadOnlySpan<'T>) (at: int) (stop: int) (codec: 'C) (context: Context) =
        match at <= stop with
        | true -> context
        | false -> directBefore text (at - 1) stop codec (append context (codec.Unit(text, at - 1)))

    /// <summary>Collects the units from an index of a text that decodes unit by unit, nearest first, up to the context length.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="at">The index to read forwards from.</param>
    /// <param name="stop">The index the units are collected up to.</param>
    /// <param name="codec">The decoder, which says what a unit is.</param>
    /// <param name="context">The context so far.</param>
    /// <returns>The context.</returns>
    [<TailCall>]
    let rec directAfter<'T, 'C when 'C: struct and 'C :> ICodec<'T>> (text: ReadOnlySpan<'T>) (at: int) (stop: int) (codec: 'C) (context: Context) =
        match at >= stop with
        | true -> context
        | false -> directAfter text (at + 1) stop codec (append context (codec.Unit(text, at)))

    /// <summary>Decodes the item into the sink, the cluster of each code point being its offset from the start of the whole text
    /// (hb-buffer.cc:1918-1925).</summary>
    /// <param name="item">The item's code units.</param>
    /// <param name="at">The index in the item.</param>
    /// <param name="offset">The offset of the item in the whole text.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <param name="sink">The sink.</param>
    /// <param name="state">The sink state.</param>
    /// <returns>The final state.</returns>
    [<TailCall>]
    let rec items<'T, 'S, 'C, 'K when 'C: struct and 'C :> ICodec<'T> and 'K: struct and 'K :> ICodepointSink<'S>>
            (item: ReadOnlySpan<'T>) (at: int) (offset: int) (replacement: uint32) (codec: 'C) (sink: 'K) (state: 'S) =
        match at >= item.Length with
        | true -> state
        | false ->
            let struct (value, next) = codec.Next(item, at, replacement)
            items item next offset replacement codec sink (sink.Put(state, value, uint32 (offset + at)))

    /// <summary>One <c>hb_buffer_add_utf</c> over a decoder, without the buffer: the sink state after the item, the pre-context (only when
    /// the buffer is empty and the offset is positive, hb-buffer.cc:1903) and the post-context.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first code unit.</param>
    /// <param name="itemLength">The number of code units, or -1.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="bufferIsEmpty">Whether the buffer has no glyphs.</param>
    /// <param name="codec">The decoder.</param>
    /// <param name="sink">The sink.</param>
    /// <param name="state">The sink state.</param>
    /// <returns>The final state, the pre-context, the post-context and whether the pre-context is installed.</returns>
    let add<'T, 'S, 'C, 'K when 'C: struct and 'C :> ICodec<'T> and 'K: struct and 'K :> ICodepointSink<'S>>
            (text: ReadOnlySpan<'T>) (itemOffset: int) (itemLength: int) (replacement: uint32) (bufferIsEmpty: bool) (codec: 'C) (sink: 'K) (state: 'S) =
        let struct (offset, length) = clamp text.Length itemOffset itemLength
        let installed = bufferIsEmpty && offset > 0
        let before = min ContextLength offset
        let pre =
            match installed, before > 0 && codec.Direct (text.Slice(offset - before, before)) with
            | false, _ -> noContext
            | true, true -> directBefore text offset (offset - before) codec noContext
            | true, false -> preContext text offset replacement codec noContext
        let item = text.Slice(offset, length)
        let final =
            match codec.Direct item with
            | true -> directItems item 0 offset codec sink state
            | false -> items item 0 offset replacement codec sink state
        let after = min ContextLength (text.Length - offset - length)
        let post =
            match after > 0 && codec.Direct (text.Slice(offset + length, after)) with
            | true -> directAfter text (offset + length) (offset + length + after) codec noContext
            | false -> postContext text (offset + length) replacement codec noContext
        struct (final, pre, post, installed)

/// <summary>Text entry for the HarfBuzzSharp buffer: <c>hb_buffer_add_utf8</c>, <c>_utf16</c>, <c>_utf32</c> and <c>_codepoints</c>
/// (hb-buffer.cc:1868-1938), decoding with the phase-3 <c>Utf</c> functions into a sink the shell implements as a struct.</summary>
[<AbstractClass; Sealed>]
type Text =
    /// <summary>The offset and length an item is clamped to, <c>hb-buffer.cc:1880-1889</c>.</summary>
    /// <param name="textLength">The length of the whole text in code units.</param>
    /// <param name="itemOffset">The first code unit, not negative.</param>
    /// <param name="itemLength">The number of code units, or -1 for the rest.</param>
    /// <returns>The offset and the length.</returns>
    static member Clamp (textLength: int, itemOffset: int, itemLength: int) =
        TextImpl.clamp textLength itemOffset itemLength

    /// <summary><c>hb_buffer_add_utf8</c> (hb-buffer.cc:1868-1938): clamps offsets, decodes with <c>Utf.next8</c> and <c>Utf.prev8</c>, the
    /// cluster of each code point is its offset from the text start, the pre-context is taken only when the buffer is empty and the item
    /// offset is positive. The same contract as <c>GlyphRun.addUtf8</c>, without allocating.</summary>
    static member AddUtf8<'S, 'K when 'K: struct and 'K :> ICodepointSink<'S>>
            (text: ReadOnlySpan<byte>, itemOffset: int, itemLength: int, replacement: uint32, bufferIsEmpty: bool, sink: 'K, state: 'S) =
        let struct (final, pre, post, installed) = TextImpl.add text itemOffset itemLength replacement bufferIsEmpty (Utf8Codec()) sink state
        { State = final; Pre = pre; Post = post; PreInstalled = installed }

    /// <summary><c>hb_buffer_add_utf16</c>, a surrogate pair is one code point and a lone surrogate the replacement.</summary>
    static member AddUtf16<'S, 'K when 'K: struct and 'K :> ICodepointSink<'S>>
            (text: ReadOnlySpan<char>, itemOffset: int, itemLength: int, replacement: uint32, bufferIsEmpty: bool, sink: 'K, state: 'S) =
        let struct (final, pre, post, installed) = TextImpl.add (MemoryMarshal.Cast<char, uint16> text) itemOffset itemLength replacement bufferIsEmpty (Utf16Codec()) sink state
        { State = final; Pre = pre; Post = post; PreInstalled = installed }

    /// <summary><c>hb_buffer_add_utf32</c>, validating (<c>hb_utf32_t</c>): a surrogate or a value past U+10FFFF is the replacement.</summary>
    static member AddUtf32<'S, 'K when 'K: struct and 'K :> ICodepointSink<'S>>
            (text: ReadOnlySpan<uint32>, itemOffset: int, itemLength: int, replacement: uint32, bufferIsEmpty: bool, sink: 'K, state: 'S) =
        let struct (final, pre, post, installed) = TextImpl.add text itemOffset itemLength replacement bufferIsEmpty (Utf32Codec()) sink state
        { State = final; Pre = pre; Post = post; PreInstalled = installed }

    /// <summary><c>hb_buffer_add_codepoints</c>, no validation (<c>hb_utf32_novalidate_t</c>).</summary>
    static member AddCodepoints<'S, 'K when 'K: struct and 'K :> ICodepointSink<'S>>
            (text: ReadOnlySpan<uint32>, itemOffset: int, itemLength: int, replacement: uint32, bufferIsEmpty: bool, sink: 'K, state: 'S) =
        let struct (final, pre, post, installed) = TextImpl.add text itemOffset itemLength replacement bufferIsEmpty (CodepointCodec()) sink state
        { State = final; Pre = pre; Post = post; PreInstalled = installed }
