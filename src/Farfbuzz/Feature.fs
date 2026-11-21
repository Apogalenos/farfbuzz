namespace Farfbuzz

open System
open System.Globalization
open System.Runtime.InteropServices
open System.Text

/// <summary>
/// An OpenType feature setting over a range of the input, laid out exactly as <c>hb_feature_t</c> (16 bytes).
/// </summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type Feature =
    {
        Tag: Tag       // the feature tag, for example liga
        Value: uint32  // 0 turns the feature off, 1 on; larger values pick alternates
        Start: uint32  // the first cluster the setting applies to
        End: uint32    // the cluster after the last one it applies to
    }

/// <summary>Parsing and formatting of feature strings, as <c>hb_feature_from_string</c> and <c>hb_feature_to_string</c>.</summary>
[<RequireQualifiedAccess>]
module Feature =
    /// <summary>The start of a feature that applies from the beginning, <c>HB_FEATURE_GLOBAL_START</c>.</summary>
    [<Literal>]
    let GlobalStart = 0u

    /// <summary>The end of a feature that applies to the end, <c>HB_FEATURE_GLOBAL_END</c>.</summary>
    [<Literal>]
    let GlobalEnd = 0xFFFFFFFFu

    module internal Stages =
        /// <summary>Reads an optional leading <c>-</c> (value 0) or <c>+</c> (value 1), <c>parse_feature_value_prefix</c>.</summary>
        /// <param name="text">The UTF-8 input.</param>
        /// <returns>The value the prefix implies and the cursor after it.</returns>
        let inline prefix (text: ReadOnlySpan<byte>) : struct (uint32 * int) =
            match Cursor.char text 0 '-'B with
            | struct (true, next) -> struct (0u, next)
            | struct (false, at) ->
                let struct (_, next) = Cursor.char text at '+'B
                struct (1u, next)

        /// <summary>Reads the end of a range: a number after <c>:</c> or <c>;</c>, else one past a written start.</summary>
        /// <param name="text">The UTF-8 input.</param>
        /// <param name="p">The cursor after the start.</param>
        /// <param name="hasStart">Whether a start was written.</param>
        /// <param name="start">The start.</param>
        /// <returns>The end and the cursor after it.</returns>
        let inline rangeEnd (text: ReadOnlySpan<byte>) (p: int) (hasStart: bool) (start: uint32) : struct (uint32 * int) =
            let struct (colon, afterColon) = Cursor.char text p ':'B
            let struct (separated, at) = if colon then struct (true, afterColon) else Cursor.char text afterColon ';'B
            match separated, Cursor.unsigned text at with
            | true, struct (true, stop, next) -> struct (stop, next)
            | true, struct (false, _, _) -> struct (GlobalEnd, at)
            | false, _ -> struct ((if hasStart then start + 1u else GlobalEnd), at)

        /// <summary>Reads an optional <c>[start:end]</c> range, <c>parse_feature_indices</c>.</summary>
        /// <param name="text">The UTF-8 input.</param>
        /// <param name="p">The cursor.</param>
        /// <returns>The start and end, global when absent, and the cursor after them.</returns>
        let inline indices (text: ReadOnlySpan<byte>) (p: int) : struct (uint32 * uint32 * int) =
            match Cursor.char text p '['B with
            | struct (false, at) -> struct (GlobalStart, GlobalEnd, at)
            | struct (true, afterOpen) ->
                let struct (hasStart, start, afterStart) = Cursor.unsigned text afterOpen
                let struct (stop, afterEnd) = rangeEnd text afterStart hasStart start
                match Cursor.char text afterEnd ']'B with
                | struct (true, next) -> struct (start, stop, next)
                | struct (false, _) -> struct (start, stop, Cursor.Failed)

        /// <summary>Reads an optional <c>=value</c>, a bare value, or on/off, <c>parse_feature_value_postfix</c>.</summary>
        /// <param name="text">The UTF-8 input.</param>
        /// <param name="p">The cursor.</param>
        /// <param name="implied">The value set by the prefix.</param>
        /// <returns>The value and the cursor after it, <c>Failed</c> when an equals sign has no value.</returns>
        let inline postfix (text: ReadOnlySpan<byte>) (p: int) (implied: uint32) : struct (uint32 * int) =
            let struct (hadEqual, at) = Cursor.char text p '='B
            match Cursor.unsigned text at with
            | struct (true, value, next) -> struct (value, next)
            | struct (false, _, _) ->
                match Cursor.boolean text at, hadEqual with
                | struct (Cursor.NoWord, _), true -> struct (implied, Cursor.Failed)
                | struct (Cursor.NoWord, next), false -> struct (implied, next)
                | struct (word, next), _ -> struct (uint32 word, next)

        /// <summary>Parses a feature, <c>parse_one_feature</c>: five stages, then nothing but spaces may remain.</summary>
        /// <param name="text">The UTF-8 input.</param>
        /// <returns>The feature, or none when the text is not one.</returns>
        let tryParse (text: ReadOnlySpan<byte>) : Feature voption =
            let struct (implied, p) = prefix text
            let struct (name, p) = Cursor.tag text p
            let struct (start, stop, p) = indices text p
            let struct (value, p) = postfix text p implied
            if Cursor.spaces text p = text.Length then
                ValueSome { Tag = LanguagePrimitives.UInt32WithMeasure name; Value = value; Start = start; End = stop }
            else
                ValueNone

    /// <summary>Reads an optional leading <c>-</c> (value 0) or <c>+</c> (value 1), <c>parse_feature_value_prefix</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <returns>The value implied by the prefix and the offset after it.</returns>
    let private prefix (text: byte array) : Scanned<uint32> =
        match Syntax.char text 0 '-'B with
        | struct (true, next) -> { Value = 0u; Next = next }
        | struct (false, afterSpaces) ->
            let struct (_, next) = Syntax.char text afterSpaces '+'B
            { Value = 1u; Next = next }

    /// <summary>Reads an optional <c>[start:end]</c> range, <c>parse_feature_indices</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <returns>The start and end, global when absent, and the offset after them.</returns>
    let private indices (text: byte array) (index: int) : Result<Scanned<struct (uint32 * uint32)>, ParseError> =
        match Syntax.char text index '['B with
        | struct (false, afterSpaces) -> Ok { Value = struct (GlobalStart, GlobalEnd); Next = afterSpaces }
        | struct (true, afterOpen) ->
            let start = Syntax.unsignedFromInt text afterOpen
            let struct (startValue, afterStart) =
                match start with
                | ValueSome scanned -> struct (scanned.Value, scanned.Next)
                | ValueNone -> struct (GlobalStart, afterOpen)
            let struct (separated, afterSeparator) =
                match Syntax.char text afterStart ':'B with
                | struct (true, next) -> struct (true, next)
                | struct (false, afterSpaces) -> Syntax.char text afterSpaces ';'B
            let struct (endValue, afterEnd) =
                match separated, Syntax.unsignedFromInt text afterSeparator with
                | true, ValueSome scanned -> struct (scanned.Value, scanned.Next)
                | true, ValueNone -> struct (GlobalEnd, afterSeparator)
                | false, _ -> struct ((if start.IsSome then startValue + 1u else GlobalEnd), afterSeparator)
            match Syntax.char text afterEnd ']'B with
            | struct (true, next) -> Ok { Value = struct (startValue, endValue); Next = next }
            | struct (false, at) -> Error (UnexpectedEnd (at, "]"))

    /// <summary>
    /// Reads an optional <c>=value</c>, a bare value, or on/off, <c>parse_feature_value_postfix</c>: after an
    /// equals sign a value is required; without one, letters that are not on/off are skipped silently.
    /// </summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <param name="implied">The value set by the prefix.</param>
    /// <returns>The value and the offset after it.</returns>
    let private postfix (text: byte array) (index: int) (implied: uint32) : Result<Scanned<uint32>, ParseError> =
        let struct (hadEqual, afterEqual) = Syntax.char text index '='B
        match Syntax.unsignedFromInt text afterEqual with
        | ValueSome scanned -> Ok scanned
        | ValueNone ->
            match Syntax.boolean text afterEqual, hadEqual with
            | struct (ValueSome value, next), _ -> Ok { Value = value; Next = next }
            | struct (ValueNone, next), false -> Ok { Value = implied; Next = next }
            | struct (ValueNone, next), true -> Error (UnexpectedEnd (next, "a value after ="))

    /// <summary>Explains a refused feature string; the cursor stages decide acceptance.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <returns>The feature, or why the string is not one.</returns>
    let internal explain (text: byte array) : Result<Feature, ParseError> =
        result {
            let implied = prefix text
            let! tag = Syntax.tag text implied.Next
            let! range = indices text tag.Next
            let! value = postfix text range.Next implied.Value
            do! Syntax.finished text value.Next
            let struct (start, stop) = range.Value
            return { Tag = tag.Value; Value = value.Value; Start = start; End = stop }
        }

    /// <summary>Parses a feature string given as UTF-8 bytes, exactly as <c>hb_feature_from_string</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <returns>The feature, or why the string is not one.</returns>
    let parseUtf8 (text: byte array) : Result<Feature, ParseError> =
        match Stages.tryParse (ReadOnlySpan text) with
        | ValueSome feature -> Ok feature
        | ValueNone -> explain text

    /// <summary>Parses a feature string such as <c>-liga</c>, <c>kern[3:5]</c> or <c>aalt=2</c>.</summary>
    /// <param name="text">The feature string.</param>
    /// <returns>The feature, or why the string is not one.</returns>
    let parse (text: string) : Result<Feature, ParseError> =
        parseUtf8 (Encoding.UTF8.GetBytes text)

    /// <summary>Spells a tag without its trailing padding spaces.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The trimmed spelling.</returns>
    let internal trimmedTag (tag: Tag) =
        (Tag.toString tag).TrimEnd ' '

    /// <summary>Formats a feature in the syntax <see cref="parse"/> reads, exactly as <c>hb_feature_to_string</c>.</summary>
    /// <param name="feature">The feature.</param>
    /// <returns>The text, without white space; the value is omitted when it is 0 or 1.</returns>
    let format (feature: Feature) : string =
        let invariant (value: uint32) = value.ToString(CultureInfo.InvariantCulture)
        let sign = if feature.Value = 0u then "-" else String.Empty
        let range =
            match feature.Start = GlobalStart && feature.End = GlobalEnd with
            | true -> String.Empty
            | false ->
                let first = if feature.Start = 0u then String.Empty else invariant feature.Start
                let last =
                    match feature.End = feature.Start + 1u, feature.End = GlobalEnd with
                    | true, _ -> String.Empty
                    | false, true -> ":"
                    | false, false -> ":" + invariant feature.End
                "[" + first + last + "]"
        let value = if feature.Value > 1u then "=" + invariant feature.Value else String.Empty
        sign + trimmedTag feature.Tag + range + value
