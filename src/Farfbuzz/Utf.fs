namespace Farfbuzz

open System

/// <summary>The literal UTF decoders of <c>hb-utf.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Utf =
    /// <summary>The default replacement code point.</summary>
    [<Literal>]
    let ReplacementCodepointDefault = 0xFFFDu

    /// <summary>Decodes a two-byte UTF-8 sequence.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The lead index.</param>
    /// <param name="c">The lead byte.</param>
    /// <param name="replacement">The replacement.</param>
    /// <returns>The code point and next index.</returns>
    let private decodeTwo (text: ReadOnlySpan<byte>) (i) (c) (replacement) =
        match i + 1 < text.Length with
        | false -> struct (replacement, i + 1)
        | true ->
            let t1 = int text[i + 1] - 0x80
            match t1 >= 0 && t1 <= 0x3F with
            | true -> struct ((((c &&& 0x1Fu) <<< 6) ||| uint32 t1), i + 2)
            | false -> struct (replacement, i + 1)

    /// <summary>Decodes a three-byte UTF-8 sequence.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The lead index.</param>
    /// <param name="c">The lead byte.</param>
    /// <param name="replacement">The replacement.</param>
    /// <returns>The code point and next index.</returns>
    let private decodeThree (text: ReadOnlySpan<byte>) (i) (c) (replacement) =
        match i + 2 < text.Length with
        | false -> struct (replacement, i + 1)
        | true ->
            let t1 = int text[i + 1] - 0x80
            let t2 = int text[i + 2] - 0x80
            match t1 >= 0 && t1 <= 0x3F && t2 >= 0 && t2 <= 0x3F with
            | false -> struct (replacement, i + 1)
            | true ->
                let value = ((c &&& 0xFu) <<< 12) ||| (uint32 t1 <<< 6) ||| uint32 t2
                match value < 0x800u || (value >= 0xD800u && value <= 0xDFFFu) with
                | true -> struct (replacement, i + 1)
                | false -> struct (value, i + 3)

    /// <summary>Decodes a four-byte UTF-8 sequence.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The lead index.</param>
    /// <param name="c">The lead byte.</param>
    /// <param name="replacement">The replacement.</param>
    /// <returns>The code point and next index.</returns>
    let private decodeFour (text: ReadOnlySpan<byte>) (i) (c) (replacement) =
        match i + 3 < text.Length with
        | false -> struct (replacement, i + 1)
        | true ->
            let t1 = int text[i + 1] - 0x80
            let t2 = int text[i + 2] - 0x80
            let t3 = int text[i + 3] - 0x80
            match t1 >= 0 && t1 <= 0x3F && t2 >= 0 && t2 <= 0x3F && t3 >= 0 && t3 <= 0x3F with
            | false -> struct (replacement, i + 1)
            | true ->
                let value = ((c &&& 0x7u) <<< 18) ||| (uint32 t1 <<< 12) ||| (uint32 t2 <<< 6) ||| uint32 t3
                match value >= 0x10000u && value <= 0x10FFFFu with
                | true -> struct (value, i + 4)
                | false -> struct (replacement, i + 1)

    /// <summary>Reads one UTF-8 code point, one replacement per ill-formed byte.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The current unit index.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <returns>The code point and the next unit index.</returns>
    let next8 (text: ReadOnlySpan<byte>) (i) (replacement) : struct (uint32 * int) =
        match i < text.Length with
        | false -> struct (replacement, i)
        | true ->
            let c = uint32 text[i]
            match c with
            | _ when c <= 0x7Fu -> struct (c, i + 1)
            | _ when c >= 0xC2u && c <= 0xDFu -> decodeTwo text i c replacement
            | _ when c >= 0xE0u && c <= 0xEFu -> decodeThree text i c replacement
            | _ when c >= 0xF0u && c <= 0xF4u -> decodeFour text i c replacement
            | _ -> struct (replacement, i + 1)

    /// <summary>Finds the lead of the UTF-8 sequence ending at an index.</summary>
    /// <param name="text">The text.</param>
    /// <param name="start">The candidate lead.</param>
    /// <param name="endIndex">The end index.</param>
    /// <returns>The lead index.</returns>
    [<TailCall>]
    let rec private back8 (text: ReadOnlySpan<byte>) (start) (endIndex) =
        match start > 0 && (text[start] &&& 0xC0uy) = 0x80uy && endIndex - start < 4 with
        | true -> back8 text (start - 1) endIndex
        | false -> start

    /// <summary>Reads one UTF-8 code point backwards.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The end unit index.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <returns>The code point and the previous unit index.</returns>
    let prev8 (text: ReadOnlySpan<byte>) (i) (replacement) : struct (uint32 * int) =
        let endIndex = i
        let start = back8 text (i - 1) endIndex
        let struct (value, next) = next8 text start replacement
        match next = endIndex with
        | true -> struct (value, start)
        | false -> struct (replacement, endIndex - 1)

    /// <summary>Decodes a UTF-16 surrogate pair.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The high-surrogate index.</param>
    /// <param name="high">The high surrogate.</param>
    /// <param name="replacement">The replacement.</param>
    /// <returns>The code point and next index.</returns>
    let private decodeSurrogate (text: ReadOnlySpan<uint16>) (i) (high) (replacement) =
        match i + 1 < text.Length with
        | false -> struct (replacement, i + 1)
        | true ->
            let low = uint32 text[i + 1]
            match low >= 0xDC00u && low <= 0xDFFFu with
            | true -> struct ((high <<< 10) + low - ((0xD800u <<< 10) - 0x10000u + 0xDC00u), i + 2)
            | false -> struct (replacement, i + 1)

    /// <summary>Reads one UTF-16 code point.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The current unit index.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <returns>The code point and the next unit index.</returns>
    let next16 (text: ReadOnlySpan<uint16>) (i) (replacement) : struct (uint32 * int) =
        match i < text.Length with
        | false -> struct (replacement, i)
        | true ->
            let c = uint32 text[i]
            match c >= 0xD800u && c <= 0xDFFFu with
            | false -> struct (c, i + 1)
            | true ->
                match c <= 0xDBFFu with
                | true -> decodeSurrogate text i c replacement
                | false -> struct (replacement, i + 1)

    /// <summary>Reads one UTF-16 surrogate pair backwards.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The end index.</param>
    /// <param name="low">The low surrogate.</param>
    /// <param name="replacement">The replacement.</param>
    /// <returns>The code point and previous index.</returns>
    let private decodeSurrogateBack (text: ReadOnlySpan<uint16>) (i) (low) (replacement) =
        match i > 1 && low >= 0xDC00u with
        | false -> struct (replacement, i - 1)
        | true ->
            let high = uint32 text[i - 2]
            match high >= 0xD800u && high <= 0xDBFFu with
            | true -> struct ((high <<< 10) + low - ((0xD800u <<< 10) - 0x10000u + 0xDC00u), i - 2)
            | false -> struct (replacement, i - 1)

    /// <summary>Reads one UTF-16 code point backwards.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The end unit index.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <returns>The code point and the previous unit index.</returns>
    let prev16 (text: ReadOnlySpan<uint16>) (i) (replacement) : struct (uint32 * int) =
        match i with
        | 0 -> struct (replacement, 0)
        | _ ->
            let c = uint32 text[i - 1]
            match c >= 0xD800u && c <= 0xDFFFu with
            | false -> struct (c, i - 1)
            | true -> decodeSurrogateBack text i c replacement

    /// <summary>Reads one UTF-32 code point.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The current unit index.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="validate">Whether to replace surrogates and out-of-range values.</param>
    /// <returns>The code point and the next unit index.</returns>
    let next32 (text: ReadOnlySpan<uint32>) (i) (replacement) (validate) : struct (uint32 * int) =
        match i < text.Length with
        | false -> struct (replacement, i)
        | true ->
            let c = text[i]
            match validate && (c >= 0xD800u && (c <= 0xDFFFu || c > 0x10FFFFu)) with
            | true -> struct (replacement, i + 1)
            | false -> struct (c, i + 1)

    /// <summary>Reads one UTF-32 code point backwards.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The end unit index.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="validate">Whether to replace surrogates and out-of-range values.</param>
    /// <returns>The code point and the previous unit index.</returns>
    let prev32 (text: ReadOnlySpan<uint32>) (i) (replacement) (validate) : struct (uint32 * int) =
        match i with
        | 0 -> struct (replacement, 0)
        | _ ->
            let c = text[i - 1]
            match validate && (c >= 0xD800u && (c <= 0xDFFFu || c > 0x10FFFFu)) with
            | true -> struct (replacement, i - 1)
            | false -> struct (c, i - 1)

    /// <summary>Reads one Latin-1 code point.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The current unit index.</param>
    /// <returns>The code point and the next unit index.</returns>
    let nextLatin1 (text: ReadOnlySpan<byte>) (i) : struct (uint32 * int) =
        match i < text.Length with
        | true -> struct (uint32 text[i], i + 1)
        | false -> struct (ReplacementCodepointDefault, i)

    /// <summary>Reads one Latin-1 code point backwards.</summary>
    /// <param name="text">The text.</param>
    /// <param name="i">The end unit index.</param>
    /// <returns>The code point and the previous unit index.</returns>
    let prevLatin1 (text: ReadOnlySpan<byte>) (i) : struct (uint32 * int) =
        match i > 0 with
        | true -> struct (uint32 text[i - 1], i - 1)
        | false -> struct (ReplacementCodepointDefault, 0)
