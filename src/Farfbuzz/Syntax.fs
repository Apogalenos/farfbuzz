namespace Farfbuzz

open System
open System.Text

/// <summary>
/// The character-level parsers of <c>hb-common.cc</c> shared by feature and variation strings
/// (<c>parse_char</c>, <c>parse_tag</c>, <c>parse_bool</c>, <c>parse_uint</c>), ported with their exact
/// side effects: a failed <c>parse_char</c> still skips spaces, a failed <c>parse_bool</c> still skips letters.
/// </summary>
[<RequireQualifiedAccess>]
module internal Syntax =
    /// <summary>Skips spaces, then consumes one expected byte, <c>parse_char</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <param name="expected">The byte to consume.</param>
    /// <returns>Whether it was there, and the offset afterwards (past the spaces even when it was not).</returns>
    let char (text: byte array) (index) (expected) : struct (bool * int) =
        let at = Number.skipSpaces (ReadOnlySpan text) index
        if at < text.Length && text[at] = expected then struct (true, at + 1) else struct (false, at)

    /// <summary>Reads a signed decimal and reinterprets it as unsigned, <c>parse_uint</c> (so -1 is a big number).</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <returns>The value and the offset after it, or none (the offset is then unchanged).</returns>
    let unsignedFromInt (text: byte array) (index) : Scanned<uint32> voption =
        match Number.parseInt (ReadOnlySpan text) index false with
        | ValueSome scanned -> ValueSome { Value = uint32 scanned.Value; Next = scanned.Next }
        | ValueNone -> ValueNone

    /// <summary>Reads <c>on</c> or <c>off</c> in any case, <c>parse_bool</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <returns>The value when the letters were on or off, and the offset past the spaces and letters either way.</returns>
    let boolean (text: byte array) (index) : struct (uint32 voption * int) =
        let start = Number.skipSpaces (ReadOnlySpan text) index
        let stop = Number.skipLetters (ReadOnlySpan text) start
        match stop - start with
        | 2 when Number.toLower text[start] = 'o'B && Number.toLower text[start + 1] = 'n'B -> struct (ValueSome 1u, stop)
        | 3 when Number.toLower text[start] = 'o'B && Number.toLower text[start + 1] = 'f'B && Number.toLower text[start + 2] = 'f'B -> struct (ValueSome 0u, stop)
        | _ -> struct (ValueNone, stop)

    /// <summary>Finds the end of a tag: a space, <c>=</c>, <c>[</c> or the quote byte (NUL when unquoted).</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="quote">The opening quote, or 0.</param>
    /// <returns>The offset of the terminator, or the length.</returns>
    [<TailCall>]
    let rec private tagEnd (text: byte array) (index) (quote) =
        match index < text.Length with
        | true when (let c = text[index] in c <> ' 'B && c <> '='B && c <> '['B && c <> quote) -> tagEnd text (index + 1) quote
        | true | false -> index

    /// <summary>Reads a tag, optionally quoted with ' or " (then exactly four bytes), <c>parse_tag</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <returns>The tag and the offset after it.</returns>
    let tag (text: byte array) (index) : Result<Scanned<Tag>, ParseError> =
        let start = Number.skipSpaces (ReadOnlySpan text) index
        let quote = if start < text.Length && (text[start] = '\''B || text[start] = '"'B) then text[start] else 0uy
        let first = if quote = 0uy then start else start + 1
        let last = tagEnd text first quote
        let length = last - first
        match length, quote with
        | 0, _ -> Error (UnexpectedEnd (first, "a tag of one to four bytes"))
        | n, _ when n > Tag.Width -> Error (InvalidToken (first, Encoding.UTF8.GetString(text, first, n), "a tag has at most four bytes"))
        | _, 0uy -> Ok { Value = Tag.ofUtf8 (ReadOnlySpan(text, first, length)); Next = last }
        | 4, q when last < text.Length && text[last] = q -> Ok { Value = Tag.ofUtf8 (ReadOnlySpan(text, first, length)); Next = last + 1 }
        | 4, _ -> Error (UnexpectedEnd (last, "the closing quote"))
        | n, _ -> Error (InvalidToken (first, Encoding.UTF8.GetString(text, first, n), "a quoted tag has exactly four bytes"))

    /// <summary>Requires that only spaces remain.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <returns>Success, or the offset of the first leftover byte.</returns>
    let finished (text: byte array) (index) : Result<unit, ParseError> =
        match Number.skipSpaces (ReadOnlySpan text) index with
        | stop when stop = text.Length -> Ok ()
        | stop -> Error (TrailingInput stop)

/// <summary>
/// The cursor stages shared by the feature grammar: a parse position that carries its own failure, as upstream
/// chains its parsers with <c>&amp;&amp;</c>.
/// </summary>
[<RequireQualifiedAccess>]
module internal Cursor =
    /// <summary>The cursor of a parse that has failed; every stage hands it on untouched.</summary>
    [<Literal>]
    let Failed = -1

    /// <summary>The answer of <c>boolean</c> when the letters are neither on nor off.</summary>
    [<Literal>]
    let NoWord = -1

    /// <summary>One more than the largest magnitude an <c>int</c> can have: every larger number is refused alike.</summary>
    [<Literal>]
    let Overflowed = 0x80000001UL

    /// <summary>The largest magnitude after a minus sign, 2^31.</summary>
    [<Literal>]
    let NegativeLimit = 0x80000000UL

    /// <summary>The largest magnitude without a minus sign, 2^31 - 1.</summary>
    [<Literal>]
    let PositiveLimit = 0x7FFFFFFFUL

    /// <summary>Four spaces, shifted right past the bytes a tag already holds.</summary>
    [<Literal>]
    let Padding = 0x20202020UL

    /// <summary>Skips white space, <c>parse_space</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <returns>The cursor at the first byte that is not a space.</returns>
    [<TailCall>]
    let rec spacesFrom (text: ReadOnlySpan<byte>) (p) =
        if uint p < uint text.Length && Number.isSpace text[p] then spacesFrom text (p + 1) else p

    /// <summary>Skips white space, entering the loop only for a space.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <returns>The cursor at the first byte that is not a space.</returns>
    let inline spaces (text: ReadOnlySpan<byte>) (p) =
        if uint p < uint text.Length && Number.isSpace text[p] then spacesFrom text (p + 1) else p

    /// <summary>Skips spaces, then consumes one expected byte, <c>parse_char</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <param name="expected">The byte to consume.</param>
    /// <returns>Whether it was there, and the cursor afterwards (past the spaces even when it was not).</returns>
    let inline char (text: ReadOnlySpan<byte>) (p) (expected) : struct (bool * int) =
        let at = spaces text p
        if uint at < uint text.Length && text[at] = expected then struct (true, at + 1) else struct (false, at)

    /// <summary>Accumulates decimal digits, refusing to grow past what an <c>int</c> can hold.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <param name="magnitude">The value so far.</param>
    /// <returns>The magnitude in the upper half and the cursor after the digits in the lower half.</returns>
    [<TailCall>]
    let rec digitsFrom (text: ReadOnlySpan<byte>) (p) (magnitude) =
        if uint p < uint text.Length && Number.isDigit text[p] then
            digitsFrom text (p + 1) (min (magnitude * 10UL + uint64 (text[p] - '0'B)) Overflowed)
        else
            (int64 magnitude <<< 32) ||| int64 p

    /// <summary>Reads a signed decimal as an unsigned value, <c>parse_uint</c> over <c>hb_parse_int</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <returns>Whether a number was there, its value, and the cursor after it (unchanged when there was none).</returns>
    let inline unsigned (text: ReadOnlySpan<byte>) (p) : struct (bool * uint32 * int) =
        let at = spaces text p
        let negative = uint at < uint text.Length && text[at] = '-'B
        let signed = negative || (uint at < uint text.Length && text[at] = '+'B)
        let first = if signed then at + 1 else at
        let packed = if uint first < uint text.Length && Number.isDigit text[first] then digitsFrom text first 0UL else int64 Failed
        let magnitude = uint64 (uint32 (packed >>> 32))
        let stop = int packed
        if stop >= 0 && magnitude <= (if negative then NegativeLimit else PositiveLimit) then
            struct (true, (if negative then 0u - uint32 magnitude else uint32 magnitude), stop)
        else
            struct (false, 0u, p)

    /// <summary>Skips ASCII letters.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <returns>The cursor at the first byte that is not a letter.</returns>
    [<TailCall>]
    let rec lettersFrom (text: ReadOnlySpan<byte>) (p) =
        if uint p < uint text.Length && Number.isAlpha text[p] then lettersFrom text (p + 1) else p

    /// <summary>Reads <c>on</c> or <c>off</c> in any case, <c>parse_bool</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <returns>1 for on, 0 for off or <c>NoWord</c>, and the cursor past the spaces and letters either way.</returns>
    let inline boolean (text: ReadOnlySpan<byte>) (p) : struct (int * int) =
        let start = spaces text p
        let stop = if uint start < uint text.Length && Number.isAlpha text[start] then lettersFrom text (start + 1) else start
        match stop - start with
        | 2 when Number.toLower text[start] = 'o'B && Number.toLower text[start + 1] = 'n'B -> struct (1, stop)
        | 3 when Number.toLower text[start] = 'o'B && Number.toLower text[start + 1] = 'f'B && Number.toLower text[start + 2] = 'f'B -> struct (0, stop)
        | _ -> struct (NoWord, stop)

    /// <summary>Collects the bytes of a tag up to a space, <c>=</c>, <c>[</c> or the quote (NUL when unquoted).</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <param name="quote">The opening quote, or 0.</param>
    /// <param name="bytes">The bytes so far, the latest in the lowest position.</param>
    /// <returns>The bytes in the upper half and the cursor at the terminator in the lower half.</returns>
    [<TailCall>]
    let rec tagFrom (text: ReadOnlySpan<byte>) (p) (quote) (bytes) =
        if uint p < uint text.Length && (let c = text[p] in c <> ' 'B && c <> '='B && c <> '['B && c <> quote) then
            tagFrom text (p + 1) quote ((bytes <<< 8) ||| uint32 text[p])
        else
            (int64 bytes <<< 32) ||| int64 p

    /// <summary>Reads a tag, optionally quoted with ' or " (then exactly four bytes), <c>parse_tag</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="p">The cursor.</param>
    /// <returns>The tag and the cursor after it, <c>Failed</c> when there is no tag.</returns>
    let inline tag (text: ReadOnlySpan<byte>) (p) : struct (uint32 * int) =
        let start = spaces text p
        let quoted = uint start < uint text.Length && (text[start] = '\''B || text[start] = '"'B)
        let quote = if quoted then text[start] else 0uy
        let first = if quoted then start + 1 else start
        let packed = tagFrom text first quote 0u
        let last = int packed
        let length = last - first
        match quoted with
        | false when length >= 1 && length <= Tag.Width ->
            struct ((uint32 (packed >>> 32) <<< (8 * (Tag.Width - length))) ||| uint32 (Padding >>> (8 * length)), last)
        | true when length = Tag.Width && uint last < uint text.Length && text[last] = quote ->
            struct (uint32 (Tag.ofUtf8 (text.Slice(first, Tag.Width))), last + 1)
        | true | false -> struct (0u, Failed)
