namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Globalization
open System.Text

/// <summary>What the text serialisation of glyphs includes, the values of <c>hb_buffer_serialize_flags_t</c>.</summary>
[<Flags>]
type SerializeFlags =
    | Default = 0u
    | NoClusters = 1u
    | NoPositions = 2u
    | NoGlyphNames = 4u
    | GlyphExtents = 8u
    | GlyphFlags = 16u
    | NoAdvances = 32u

/// <summary>How far one glyph item of a deserialisation got.</summary>
[<Struct>]
type internal ItemStep<'T> =
    | Parsed of value: 'T * next: int  // a value was read; parsing continues at the offset
    | Incomplete                       // the input ended inside the item: upstream drops the item and still reports success
    | Invalid of error: ParseError     // the grammar or a value check rejected the input

/// <summary>Sequences grammar steps: each bind receives a value and the offset after it; Incomplete and Invalid stop the sequence.</summary>
type internal ItemStepBuilder() =
    /// <summary>Continues with a parsed value and its next offset, or stops.</summary>
    /// <param name="step">The step.</param>
    /// <param name="continuation">What to do with the value and the offset.</param>
    /// <returns>The continuation's step, or the stop.</returns>
    member _.Bind(step: ItemStep<'T>, continuation: 'T * int -> ItemStep<'U>) : ItemStep<'U> =
        match step with
        | Parsed (value, next) -> continuation (value, next)
        | Incomplete -> Incomplete
        | Invalid error -> Invalid error

    /// <summary>Ends with a value and the offset after it.</summary>
    /// <param name="result">The value and the offset after it.</param>
    /// <returns>The parsed step.</returns>
    member _.Return(result: 'T * int) : ItemStep<'T> = Parsed result

    /// <summary>Ends with a step as it is.</summary>
    /// <param name="step">The step.</param>
    /// <returns>The same step.</returns>
    member _.ReturnFrom(step: ItemStep<'T>) : ItemStep<'T> = step

/// <summary>The <c>itemStep { }</c> expression.</summary>
[<AutoOpen>]
module internal ItemStepExpression =
    /// <summary>Builds an <see cref="ItemStep{T}"/> sequence.</summary>
    let itemStep = ItemStepBuilder()

/// <summary>
/// The text format of <c>hb_buffer_serialize_glyphs</c> and <c>hb_buffer_deserialize_glyphs</c>, as used by
/// <c>hb-shape</c> output and HarfBuzz's shaping tests: <c>[name=cluster@dx,dy+advance#flags&lt;extents&gt;|…]</c>.
/// </summary>
[<RequireQualifiedAccess>]
module Serialize =
    /// <summary>The longest glyph name HarfBuzz writes: its buffer holds 128 bytes including the NUL.</summary>
    [<Literal>]
    let MaxGlyphName = 127

    /// <summary>Adds two positions, clamping instead of overflowing, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The first position.</param>
    /// <param name="b">The second position.</param>
    /// <returns>The clamped sum.</returns>
    let inline private saturateAdd (a: int) (b: int) =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Formats an integer in the invariant culture.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The decimal text.</returns>
    let inline private invariantText (value: ^T when ^T: (member ToString: IFormatProvider -> string)) =
        (^T: (member ToString: IFormatProvider -> string) (value, CultureInfo.InvariantCulture))

    /// <summary>Spells a glyph: its id, its font name, or <c>gidN</c> when the font has none.</summary>
    /// <param name="flags">The serialisation flags.</param>
    /// <param name="glyphName">The font's glyph names.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The spelling.</returns>
    let private spell (flags: SerializeFlags) (glyphName: uint32 -> string voption) (glyph: uint32) =
        match flags.HasFlag SerializeFlags.NoGlyphNames, glyphName glyph with
        | true, _ -> invariantText glyph
        | false, ValueSome name when name.Length > MaxGlyphName -> name.Substring(0, MaxGlyphName)
        | false, ValueSome name -> name
        | false, ValueNone -> "gid" + invariantText glyph

    /// <summary>Formats one glyph item, without its leading <c>[</c> or <c>|</c>.</summary>
    /// <param name="flags">The serialisation flags.</param>
    /// <param name="glyphName">The font's glyph names.</param>
    /// <param name="extents">The font's glyph extents.</param>
    /// <param name="info">The glyph.</param>
    /// <param name="position">Its position, when positions are serialised.</param>
    /// <param name="x">The sum of the previous horizontal advances when advances are folded into offsets.</param>
    /// <param name="y">The sum of the previous vertical advances when advances are folded into offsets.</param>
    /// <returns>The item text.</returns>
    let private item
        (flags: SerializeFlags)
        (glyphName: uint32 -> string voption)
        (extents: uint32 -> GlyphExtents voption)
        (info: GlyphInfo)
        (position: GlyphPosition voption)
        (x: int)
        (y: int) =
        let cluster = if flags.HasFlag SerializeFlags.NoClusters then String.Empty else "=" + invariantText info.Cluster
        let placement =
            match position with
            | ValueNone -> String.Empty
            | ValueSome pos ->
                let dx = saturateAdd x pos.XOffset
                let dy = saturateAdd y pos.YOffset
                let offsets = if dx <> 0 || dy <> 0 then "@" + invariantText dx + "," + invariantText dy else String.Empty
                let advances =
                    match flags.HasFlag SerializeFlags.NoAdvances, pos.YAdvance with
                    | true, _ -> String.Empty
                    | false, 0 -> "+" + invariantText pos.XAdvance
                    | false, yAdvance -> "+" + invariantText pos.XAdvance + "," + invariantText yAdvance
                offsets + advances
        let glyphFlags = info.Mask &&& uint32 GlyphFlags.Defined
        let flagText =
            if flags.HasFlag SerializeFlags.GlyphFlags && glyphFlags <> 0u then "#" + glyphFlags.ToString("X", CultureInfo.InvariantCulture) else String.Empty
        let extentText =
            match flags.HasFlag SerializeFlags.GlyphExtents, extents info.Codepoint with
            | true, ValueSome box -> "<" + invariantText box.XBearing + "," + invariantText box.YBearing + "," + invariantText box.Width + "," + invariantText box.Height + ">"
            | true, ValueNone -> String.Empty
            | false, _ -> String.Empty
        spell flags glyphName info.Codepoint + cluster + placement + flagText + extentText

    /// <summary>Formats the glyphs from an index on, newest item first in the accumulator.</summary>
    /// <param name="flags">The serialisation flags.</param>
    /// <param name="glyphName">The font's glyph names.</param>
    /// <param name="extents">The font's glyph extents.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="positions">Their positions, or empty when not serialised.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="x">The horizontal pen when advances are folded into offsets.</param>
    /// <param name="y">The vertical pen when advances are folded into offsets.</param>
    /// <param name="items">The items formatted so far, newest first.</param>
    /// <returns>All items, newest first.</returns>
    [<TailCall>]
    let rec private itemsFrom
        (flags: SerializeFlags)
        (glyphName: uint32 -> string voption)
        (extents: uint32 -> GlyphExtents voption)
        (infos: ReadOnlySpan<GlyphInfo>)
        (positions: ReadOnlySpan<GlyphPosition>)
        (index: int)
        (x: int)
        (y: int)
        (items: string list) =
        match index < infos.Length, positions.IsEmpty with
        | false, _ -> items
        | true, true -> itemsFrom flags glyphName extents infos positions (index + 1) x y (item flags glyphName extents infos[index] ValueNone x y :: items)
        | true, false ->
            let position = positions[index]
            let folding = flags.HasFlag SerializeFlags.NoAdvances
            let nextX = if folding then saturateAdd x position.XAdvance else x
            let nextY = if folding then saturateAdd y position.YAdvance else y
            itemsFrom flags glyphName extents infos positions (index + 1) nextX nextY (item flags glyphName extents infos[index] (ValueSome position) x y :: items)

    /// <summary>
    /// Serialises glyphs as text exactly as <c>hb_buffer_serialize_glyphs</c> with
    /// <c>HB_BUFFER_SERIALIZE_FORMAT_TEXT</c>; an empty buffer gives the empty string.
    /// </summary>
    /// <param name="flags">What to include.</param>
    /// <param name="glyphName">The font's name for a glyph, if it has one.</param>
    /// <param name="extents">The font's extents for a glyph, if it has them.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="positions">Their positions, or an empty span when the buffer has none.</param>
    /// <returns>The serialised text.</returns>
    let glyphs
        (flags: SerializeFlags)
        (glyphName: uint32 -> string voption)
        (extents: uint32 -> GlyphExtents voption)
        (infos: ReadOnlySpan<GlyphInfo>)
        (positions: ReadOnlySpan<GlyphPosition>) : string =
        let shown = if flags.HasFlag SerializeFlags.NoPositions then ReadOnlySpan.Empty else positions
        match infos.Length with
        | 0 -> String.Empty
        | _ ->
            let items = itemsFrom flags glyphName extents infos shown 0 0 0 []
            "[" + String.Join("|", List.rev items) + "]"

    /// <summary>Bytes that end a glyph name in the text grammar.</summary>
    /// <param name="c">The byte.</param>
    /// <returns>True for one of <c>\ ] = @ + , # |</c>.</returns>
    let private endsName (c: byte) =
        c = '\\'B || c = ']'B || c = '='B || c = '@'B || c = '+'B || c = ','B || c = '#'B || c = '|'B

    /// <summary>Bytes that a backslash may escape inside a glyph name.</summary>
    /// <param name="c">The byte.</param>
    /// <returns>True for one of <c>\ ] = @ + , |</c>.</returns>
    let private escapable (c: byte) =
        c = '\\'B || c = ']'B || c = '='B || c = '@'B || c = '+'B || c = ','B || c = '|'B

    /// <summary>Finds the end of a digit run.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="at">The current index.</param>
    /// <returns>The end index.</returns>
    [<TailCall>]
    let rec private digitsEnd (text: byte array) at =
        match at < text.Length && Number.isDigit text[at] with
        | true -> digitsEnd text (at + 1)
        | false -> at

    /// <summary>Reads a number token, <c>'-'? ('0' | [1-9] digit*)</c>, returning where it ends.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset of the token.</param>
    /// <param name="signed">Whether a leading minus is allowed.</param>
    /// <returns>The end offset of the token.</returns>
    let private numberToken (text: byte array) (index: int) (signed: bool) : ItemStep<int> =
        let digits = if signed && index < text.Length && text[index] = '-'B then index + 1 else index
        match digits < text.Length with
        | false -> Incomplete
        | true when text[digits] = '0'B -> Parsed (digits + 1, digits + 1)
        | true when text[digits] >= '1'B && text[digits] <= '9'B -> let stop = digitsEnd text (digits + 1) in Parsed (stop, stop)
        | true -> Invalid (UnexpectedByte (digits, text[digits], "a digit"))

    /// <summary>Reads a signed number token and its <c>int</c> value.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset of the token.</param>
    /// <returns>The value and the offset after it.</returns>
    let private signedNumber (text: byte array) (index: int) : ItemStep<int> =
        itemStep {
            let! stop, _ = numberToken text index true
            return!
                match Number.parseInt (ReadOnlySpan(text, index, stop - index)) 0 true with
                | ValueSome scanned -> Parsed (scanned.Value, stop)
                | ValueNone when stop = text.Length -> Incomplete
                | ValueNone -> Invalid (InvalidToken (index, Encoding.UTF8.GetString(text, index, stop - index), "out of the int range"))
        }

    /// <summary>Reads an unsigned number token and its value.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset of the token.</param>
    /// <returns>The value and the offset after it.</returns>
    let private unsignedNumber (text: byte array) (index: int) : ItemStep<uint32> =
        itemStep {
            let! stop, _ = numberToken text index false
            return!
                match Number.parseUInt (ReadOnlySpan(text, index, stop - index)) 0 true 10 with
                | ValueSome scanned -> Parsed (scanned.Value, stop)
                | ValueNone when stop = text.Length -> Incomplete
                | ValueNone -> Invalid (InvalidToken (index, Encoding.UTF8.GetString(text, index, stop - index), "out of the unsigned range"))
        }

    /// <summary>Requires one byte at an offset.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset.</param>
    /// <param name="expected">The byte.</param>
    /// <returns>The offset after it.</returns>
    let private literal (text: byte array) (index: int) (expected: byte) : ItemStep<unit> =
        match index < text.Length with
        | false -> Incomplete
        | true when text[index] = expected -> Parsed ((), index + 1)
        | true -> Invalid (UnexpectedByte (index, text[index], string (char expected)))

    /// <summary>Reads a pair <c>a,b</c> of signed numbers.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset of the first number.</param>
    /// <returns>The pair and the offset after it.</returns>
    let private pair (text: byte array) (index: int) : ItemStep<struct (int * int)> =
        itemStep {
            let! first, afterFirst = signedNumber text index
            let! (), afterComma = literal text afterFirst ','B
            let! second, afterSecond = signedNumber text afterComma
            return struct (first, second), afterSecond
        }

    /// <summary>Reads an extents block <c>&lt;a,b,c,d&gt;</c>; the values are not kept, as upstream ignores them.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset of the <c>&lt;</c>.</param>
    /// <returns>The offset after the <c>&gt;</c>.</returns>
    let private extents (text: byte array) (index: int) : ItemStep<unit> =
        itemStep {
            let! (), afterOpen = literal text index '<'B
            let! _, afterFirst = pair text afterOpen
            let! (), afterComma = literal text afterFirst ','B
            let! _, afterSecond = pair text afterComma
            let! (), afterClose = literal text afterSecond '>'B
            return (), afterClose
        }

    /// <summary>Recognises a complete extents block followed by a separator, which ends a glyph name.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset of the <c>&lt;</c>.</param>
    /// <returns>Whether an extents block starts here.</returns>
    let private extentsBlockAt (text: byte array) (index: int) : bool =
        match extents text index with
        | Parsed ((), after) -> after = text.Length || text[after] = '|'B || text[after] = ']'B
        | Incomplete | Invalid _ -> false

    /// <summary>
    /// Finds the end of a glyph token, honouring backslash escapes. A <c>&lt;</c> may start an extents block, so,
    /// as Ragel's leaving action does there, the token so far must resolve to a glyph; a complete extents block
    /// then ends the token, otherwise the <c>&lt;</c> belongs to the name.
    /// </summary>
    /// <param name="resolve">Resolves a token to a glyph id.</param>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="start">The offset of the token.</param>
    /// <param name="index">The next offset.</param>
    /// <returns>The offset of the byte after the token.</returns>
    [<TailCall>]
    let rec private glyphTokenEnd (resolve: byte array -> uint32 voption) (text: byte array) (start: int) (index: int) : ItemStep<int> =
        match index < text.Length with
        | false -> Incomplete
        | true ->
            match text[index] with
            | '\\'B when index + 1 < text.Length && escapable text[index + 1] -> glyphTokenEnd resolve text start (index + 2)
            | '\\'B when index + 1 = text.Length -> Incomplete
            | '\\'B -> Invalid (UnexpectedByte (index + 1, text[index + 1], "an escapable delimiter after \\"))
            | '<'B when ValueOption.isNone (resolve text[start .. index - 1]) ->
                Invalid (InvalidToken (start, Encoding.UTF8.GetString(text, start, index - start), "not a glyph of the font, before <"))
            | '<'B when extentsBlockAt text index -> Parsed (index, index)
            | c when endsName c -> Parsed (index, index)
            | _ -> glyphTokenEnd resolve text start (index + 1)

    /// <summary>Tests whether a token starts with an ASCII prefix.</summary>
    /// <param name="span">The token.</param>
    /// <param name="prefix">The prefix.</param>
    /// <returns>True when it does.</returns>
    let private hasPrefix (span: ReadOnlySpan<byte>) (prefix: string) =
        span.Length > 3 && Encoding.ASCII.GetString(span.Slice(0, 3).ToArray()) = prefix

    /// <summary>Parses a <c>gidN</c> or <c>uniXXXX</c> token after a failed name lookup.</summary>
    /// <param name="token">The token bytes.</param>
    /// <param name="nominalGlyph">The font's character map.</param>
    /// <returns>The glyph, or none.</returns>
    let private parseGidOrUni (token: byte array) (nominalGlyph: uint32 -> uint32 voption) : uint32 voption =
        let span = ReadOnlySpan token
        match hasPrefix span "gid", hasPrefix span "uni" with
        | true, _ ->
            match Number.parseUInt (span.Slice 3) 0 true 10 with
            | ValueSome scanned -> ValueSome scanned.Value
            | ValueNone -> ValueNone
        | false, true ->
            match Number.parseUInt (span.Slice 3) 0 true 16 with
            | ValueSome scanned -> nominalGlyph scanned.Value
            | ValueNone -> ValueNone
        | false, false -> ValueNone

    /// <summary>
    /// Resolves a glyph token as <c>hb_font_glyph_from_string</c>: a font glyph name, a decimal id, <c>gidN</c>,
    /// or <c>uniXXXX</c> through the font's character map.
    /// </summary>
    /// <param name="glyphFromName">The font's name lookup.</param>
    /// <param name="nominalGlyph">The font's character map.</param>
    /// <param name="token">The token bytes, escapes left as they are (upstream does not unescape).</param>
    /// <returns>The glyph id, or none.</returns>
    let internal resolveGlyph (glyphFromName: string -> uint32 voption) (nominalGlyph: uint32 -> uint32 voption) (token: byte array) : uint32 voption =
        let span = ReadOnlySpan token
        match glyphFromName (Encoding.UTF8.GetString token) with
        | ValueSome glyph -> ValueSome glyph
        | ValueNone ->
            match Number.parseUInt span 0 true 10 with
            | ValueSome scanned -> ValueSome scanned.Value
            | ValueNone -> parseGidOrUni token nominalGlyph

    /// <summary>The fields of one glyph item as they are read.</summary>
    [<Struct>]
    type internal Item = { Info: GlyphInfo; Position: GlyphPosition }

    /// <summary>Reads an optional part introduced by a marker byte, or leaves the item as it is.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="marker">The byte that introduces the part.</param>
    /// <param name="read">Reads the part after the marker and updates the item.</param>
    /// <param name="item">The item so far.</param>
    /// <param name="at">The current offset.</param>
    /// <returns>The item and the offset after the part.</returns>
    let private optionalPart (text: byte array) (marker: byte) (read: Item -> int -> ItemStep<Item>) (item: Item) (at: int) : ItemStep<Item> =
        match at < text.Length && text[at] = marker with
        | true -> read item (at + 1)
        | false -> Parsed (item, at)

    /// <summary>Reads <c>=cluster</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="item">The item so far.</param>
    /// <param name="at">The offset after the marker.</param>
    /// <returns>The item with its cluster.</returns>
    let private cluster (text: byte array) (item: Item) (at: int) =
        itemStep {
            let! value, next = unsignedNumber text at
            return { item with Info = { item.Info with Cluster = value } }, next
        }

    /// <summary>Reads <c>@dx,dy</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="item">The item so far.</param>
    /// <param name="at">The offset after the marker.</param>
    /// <returns>The item with its offsets.</returns>
    let private offsets (text: byte array) (item: Item) (at: int) =
        itemStep {
            let! struct (dx, dy), next = pair text at
            return { item with Position = { item.Position with XOffset = dx; YOffset = dy } }, next
        }

    /// <summary>Reads <c>+xAdvance</c> and an optional <c>,yAdvance</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="item">The item so far.</param>
    /// <param name="at">The offset after the marker.</param>
    /// <returns>The item with its advances.</returns>
    let private advances (text: byte array) (item: Item) (at: int) =
        itemStep {
            let! xAdvance, afterX = signedNumber text at
            let withX = { item with Position = { item.Position with XAdvance = xAdvance } }
            return!
                optionalPart text ','B (fun current next ->
                    itemStep {
                        let! yAdvance, afterY = signedNumber text next
                        return { current with Position = { current.Position with YAdvance = yAdvance } }, afterY
                    }) withX afterX
        }

    /// <summary>Reads <c>#flags</c> into the mask.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="item">The item so far.</param>
    /// <param name="at">The offset after the marker.</param>
    /// <returns>The item with its mask.</returns>
    let private glyphFlags (text: byte array) (item: Item) (at: int) =
        itemStep {
            let! mask, next = unsignedNumber text at
            return { item with Info = { item.Info with Mask = mask } }, next
        }

    /// <summary>Reads the optional parts of an item after its glyph, in grammar order, then its terminator.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The offset after the glyph token.</param>
    /// <param name="item">The item so far.</param>
    /// <returns>The item and the offset after its <c>|</c> or <c>]</c>.</returns>
    let private itemTail (text: byte array) (index: int) (item: Item) : ItemStep<Item> =
        itemStep {
            let! withCluster, afterCluster = optionalPart text '='B (cluster text) item index
            let! withOffsets, afterOffsets = optionalPart text '@'B (offsets text) withCluster afterCluster
            let! withAdvances, afterAdvances = optionalPart text '+'B (advances text) withOffsets afterOffsets
            let! withFlags, afterFlags = optionalPart text '#'B (glyphFlags text) withAdvances afterAdvances
            let! (), afterExtents = if afterFlags < text.Length && text[afterFlags] = '<'B then extents text afterFlags else Parsed ((), afterFlags)
            return!
                match afterExtents < text.Length with
                | false -> Incomplete
                | true when text[afterExtents] = '|'B || text[afterExtents] = ']'B -> Parsed (withFlags, afterExtents + 1)
                | true -> Invalid (UnexpectedByte (afterExtents, text[afterExtents], "| or ]"))
        }

    /// <summary>Reads glyph items until the input ends, newest first.</summary>
    /// <param name="resolve">Resolves a glyph token to a glyph id.</param>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="items">The items read so far, newest first.</param>
    /// <returns>All complete items, newest first, or the first error.</returns>
    [<TailCall>]
    let rec private itemsAfter (resolve: byte array -> uint32 voption) (text: byte array) (index: int) (items: Item list) : Result<Item list, ParseError> =
        match index = text.Length with
        | true -> Ok items
        | false ->
            match glyphTokenEnd resolve text index index with
            | Incomplete -> Ok items
            | Invalid error -> Error error
            | Parsed (tokenEnd, _) ->
                let token = text[index .. tokenEnd - 1]
                let fresh = { Info = { Codepoint = 0u; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u }
                              Position = { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u } }
                match resolve token, itemTail text tokenEnd fresh with
                | ValueNone, _ -> Error (InvalidToken (index, Encoding.UTF8.GetString token, "not a glyph of the font"))
                | ValueSome glyph, Parsed (item, next) ->
                    itemsAfter resolve text next ({ item with Info = { item.Info with Codepoint = glyph } } :: items)
                | ValueSome _, Incomplete -> Ok items
                | ValueSome _, Invalid error -> Error error

    /// <summary>
    /// Reads glyphs in the text format exactly as <c>hb_buffer_deserialize_glyphs</c> with
    /// <c>HB_BUFFER_SERIALIZE_FORMAT_TEXT</c>, including its quirks: empty input is rejected, and input ending
    /// inside an item drops that item and still succeeds.
    /// </summary>
    /// <param name="glyphFromName">The font's glyph-name lookup.</param>
    /// <param name="nominalGlyph">The font's character map, for <c>uniXXXX</c> tokens.</param>
    /// <param name="text">The serialised glyphs.</param>
    /// <returns>The glyphs and their positions, or the first error.</returns>
    let parseGlyphs
        (glyphFromName: string -> uint32 voption)
        (nominalGlyph: uint32 -> uint32 voption)
        (text: string) : Result<struct (ImmutableArray<GlyphInfo> * ImmutableArray<GlyphPosition>), ParseError> =
        let bytes = Encoding.UTF8.GetBytes text
        let start = if bytes.Length > 0 && bytes[0] = '['B then 1 else 0
        let parsed =
            match bytes.Length with
            | 0 -> Error (UnexpectedEnd (0, "serialised glyphs"))
            | _ -> itemsAfter (resolveGlyph glyphFromName nominalGlyph) bytes start []
        match parsed with
        | Ok newestFirst ->
            let items = List.rev newestFirst
            Ok (struct (Sealed.ofList (items |> List.map _.Info), Sealed.ofList (items |> List.map _.Position)))
        | Error error -> Error error
