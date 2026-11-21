namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Text
open System.Text.Unicode

/// <summary>What a buffer holds, the values of <c>hb_buffer_content_type_t</c>.</summary>
[<Struct>]
type internal ContentType =
    | NoContent
    | UnicodeContent
    | GlyphContent

/// <summary>The working set of one line: the arrays of its glyphs, their properties and their positions with the scalar state the stages of
/// the general route read and write, <c>hb_buffer_t</c> as one value, with the fields of <see cref="GlyphRun"/> under the same names. It is
/// made once per line from the run the line came in, is never shared and never escapes: a stage edits its arrays in place through the
/// primitives of <see cref="Sealed"/> and returns the set with the scalars it changed, and the line is published once, at the end of the
/// route, as one run. A struct, so that carrying it from stage to stage allocates nothing; it is declared here, and not in
/// <c>Sealed.fs</c>, only because the segment properties it carries are declared after that file, and every write to its arrays is a
/// primitive of <c>Sealed.fs</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal WorkingSet =
    { Infos: ImmutableArray<GlyphInfo>
      Props: ImmutableArray<GlyphProps>
      Positions: ImmutableArray<GlyphPosition>
      ContextBefore: ImmutableArray<uint32>
      ContextAfter: ImmutableArray<uint32>
      ContentType: ContentType
      SegmentProperties: SegmentProperties
      Flags: BufferFlags
      ClusterLevel: ClusterLevel
      Replacement: uint32
      Invisible: uint32
      NotFound: uint32
      NotFoundVariationSelector: uint32
      RandomState: uint32
      HavePositions: bool
      Successful: bool
      Serial: uint32
      ScratchFlags: ScratchFlags
      MaxLen: int
      MaxOps: int }

/// <summary>An immutable buffer of glyphs and positions, <c>hb_buffer_t</c>.</summary>
type GlyphRun =
    internal
        { Infos: ImmutableArray<GlyphInfo>
          Positions: ImmutableArray<GlyphPosition>
          Props: ImmutableArray<GlyphProps>
          ContextBefore: ImmutableArray<uint32>
          ContextAfter: ImmutableArray<uint32>
          ContentType: ContentType
          SegmentProperties: SegmentProperties
          Flags: BufferFlags
          ClusterLevel: ClusterLevel
          Replacement: uint32
          Invisible: uint32
          NotFound: uint32
          NotFoundVariationSelector: uint32
          RandomState: uint32
          HavePositions: bool
          Successful: bool
          Serial: uint32
          ScratchFlags: ScratchFlags
          MaxLen: int
          MaxOps: int }

/// <summary>The text-entry and buffer-state functions of <c>hb-buffer.cc</c>.</summary>
[<RequireQualifiedAccess>]
module GlyphRun =
    /// <summary>The glyphs of a run: after shaping the shaped glyphs, before it the decoded code points with their clusters.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The glyph infos.</returns>
    let infos (run: GlyphRun) =
        run.Infos

    /// <summary>The direction, script and language of a run, as set or guessed.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The segment properties.</returns>
    let segmentProperties (run: GlyphRun) =
        run.SegmentProperties

    /// <summary>The positions of a shaped run, one for each glyph.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The glyph positions.</returns>
    let positions (run: GlyphRun) =
        run.Positions

    /// <summary>The longest pre- or post-context, <c>hb_buffer_t::CONTEXT_LENGTH</c>.</summary>
    [<Literal>]
    let internal ContextLength = 5

    /// <summary>The default replacement code point.</summary>
    [<Literal>]
    let internal ReplacementCodepointDefault = 0xFFFDu

    /// <summary>The default maximum buffer length.</summary>
    [<Literal>]
    let internal MaxLenDefault = 0x3FFFFFFF

    /// <summary>The default maximum operation count.</summary>
    [<Literal>]
    let internal MaxOpsDefault = 0x1FFFFFFF

    /// <summary>The length limit factor.</summary>
    [<Literal>]
    let internal MaxLenFactor = 256UL

    /// <summary>The minimum length limit.</summary>
    [<Literal>]
    let internal MaxLenMin = 65536UL

    /// <summary>The operation limit factor.</summary>
    [<Literal>]
    let internal MaxOpsFactor = 4096UL

    /// <summary>The minimum operation limit.</summary>
    [<Literal>]
    let internal MaxOpsMin = 65536UL

    /// <summary>The empty run, <c>hb_buffer_reset</c>.</summary>
    let empty =
        { Infos = ImmutableArray.Empty
          Positions = ImmutableArray.Empty
          Props = ImmutableArray.Empty
          ContextBefore = ImmutableArray.Empty
          ContextAfter = ImmutableArray.Empty
          ContentType = NoContent
          SegmentProperties = { Direction = Direction.Invalid; Script = Scripts.Invalid; Language = Language.NoLanguage }
          Flags = BufferFlags.Default
          ClusterLevel = ClusterLevel.MonotoneGraphemes
          Replacement = ReplacementCodepointDefault
          Invisible = 0u
          NotFound = 0u
          NotFoundVariationSelector = 0xFFFFFFFFu
          RandomState = 1u
          HavePositions = false
          Successful = true
          Serial = 0u
          ScratchFlags = ScratchFlags.Default
          MaxLen = MaxLenDefault
          MaxOps = MaxOpsDefault }

    /// <summary>A decoder pair for one text encoding.</summary>
    type private IUtfCodec<'T> =
        /// <summary>Reads forward.</summary>
        /// <param name="text">The text.</param>
        /// <param name="index">The unit index.</param>
        /// <param name="replacement">The replacement.</param>
        /// <returns>The code point and the next index.</returns>
        abstract Next: text: ReadOnlySpan<'T> * index: int * replacement: uint32 -> struct (uint32 * int)
        /// <summary>Reads backward.</summary>
        /// <param name="text">The text.</param>
        /// <param name="index">The end index.</param>
        /// <param name="replacement">The replacement.</param>
        /// <returns>The code point and the previous index.</returns>
        abstract Previous: text: ReadOnlySpan<'T> * index: int * replacement: uint32 -> struct (uint32 * int)
        /// <summary>Whether every unit of a text is one code point equal to its own value, so no decoding is needed.</summary>
        /// <param name="text">The text.</param>
        /// <returns>True when the text decodes unit by unit.</returns>
        abstract Direct: text: ReadOnlySpan<'T> -> bool
        /// <summary>The glyph infos of a direct text, never called for a codec whose <c>Direct</c> is false.</summary>
        /// <param name="text">The text.</param>
        /// <param name="clusterBase">The cluster of the first unit.</param>
        /// <returns>One info per unit.</returns>
        abstract DirectInfos: text: ReadOnlySpan<'T> * clusterBase: int -> ImmutableArray<GlyphInfo>
        /// <summary>The code point of a unit of a direct text.</summary>
        /// <param name="unit">The unit.</param>
        /// <returns>The code point.</returns>
        abstract Unit: unit: 'T -> uint32
        /// <summary>Whether a text is well-formed, so that a decoder that checks nothing reads exactly what the checking decoder does.</summary>
        /// <param name="text">The text.</param>
        /// <returns>True when the text is well-formed.</returns>
        abstract WellFormed: text: ReadOnlySpan<'T> -> bool
        /// <summary>The glyph infos of a well-formed text, never called for a codec whose <c>WellFormed</c> is false.</summary>
        /// <param name="text">The text.</param>
        /// <param name="clusterBase">The cluster of the first unit.</param>
        /// <returns>One info per code point.</returns>
        abstract WellFormedInfos: text: ReadOnlySpan<'T> * clusterBase: int -> ImmutableArray<GlyphInfo>

    /// <summary>The UTF-8 decoder pair.</summary>
    [<Struct>]
    type private Utf8Codec =
        interface IUtfCodec<byte> with
            member _.Next (text, index, replacement) = Utf.next8 text index replacement
            member _.Previous (text, index, replacement) = Utf.prev8 text index replacement
            member _.Direct text = Ascii.IsValid text
            member _.DirectInfos (text, clusterBase) = Sealed.byteInfos text clusterBase
            member _.Unit unit = uint32 unit
            member _.WellFormed text = Utf8.IsValid text
            member _.WellFormedInfos (text, clusterBase) = Sealed.utf8Infos text clusterBase

    /// <summary>The UTF-16 decoder pair.</summary>
    [<Struct>]
    type private Utf16Codec =
        interface IUtfCodec<uint16> with
            member _.Next (text, index, replacement) = Utf.next16 text index replacement
            member _.Previous (text, index, replacement) = Utf.prev16 text index replacement
            member _.Direct _ = false
            member _.DirectInfos (_, _) = ImmutableArray<GlyphInfo>.Empty
            member _.Unit unit = uint32 unit
            member _.WellFormed _ = false
            member _.WellFormedInfos (_, _) = ImmutableArray<GlyphInfo>.Empty

    /// <summary>The UTF-32 validating decoder pair.</summary>
    [<Struct>]
    type private Utf32Codec =
        interface IUtfCodec<uint32> with
            member _.Next (text, index, replacement) = Utf.next32 text index replacement true
            member _.Previous (text, index, replacement) = Utf.prev32 text index replacement true
            member _.Direct _ = false
            member _.DirectInfos (_, _) = ImmutableArray<GlyphInfo>.Empty
            member _.Unit unit = unit
            member _.WellFormed _ = false
            member _.WellFormedInfos (_, _) = ImmutableArray<GlyphInfo>.Empty

    /// <summary>The unvalidated code-point decoder pair.</summary>
    [<Struct>]
    type private CodepointsCodec =
        interface IUtfCodec<uint32> with
            member _.Next (text, index, _) = Utf.next32 text index 0u false
            member _.Previous (text, index, _) = Utf.prev32 text index 0u false
            member _.Direct _ = false
            member _.DirectInfos (_, _) = ImmutableArray<GlyphInfo>.Empty
            member _.Unit unit = unit
            member _.WellFormed _ = false
            member _.WellFormedInfos (_, _) = ImmutableArray<GlyphInfo>.Empty

    /// <summary>The Latin-1 decoder pair.</summary>
    [<Struct>]
    type private Latin1Codec =
        interface IUtfCodec<byte> with
            member _.Next (text, index, _) = Utf.nextLatin1 text index
            member _.Previous (text, index, _) = Utf.prevLatin1 text index
            member _.Direct _ = true
            member _.DirectInfos (text, clusterBase) = Sealed.byteInfos text clusterBase
            member _.Unit unit = uint32 unit
            member _.WellFormed _ = false
            member _.WellFormedInfos (_, _) = ImmutableArray<GlyphInfo>.Empty

    /// <summary>Fills one glyph item from a decoder.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    [<Struct>]
    type private ItemUnfolder<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>> =
        val private codec: 'C
        val private replacement: uint32
        val private clusterBase: int

        /// <summary>Creates a glyph unfolder.</summary>
        /// <param name="codec">The decoder.</param>
        /// <param name="replacement">The replacement code point.</param>
        /// <param name="clusterBase">The cluster offset.</param>
        new (codec, replacement, clusterBase) = { codec = codec; replacement = replacement; clusterBase = clusterBase }

        interface ISpanUnfolder<'T, int, GlyphInfo> with
            member this.Next (source, state) =
                let struct (codepoint, after) = this.codec.Next(source, state, this.replacement)
                struct ({ Codepoint = codepoint; Mask = 0u; Cluster = uint32 (this.clusterBase + state); Var1 = 0u; Var2 = 0u }, after)

    /// <summary>Fills one backward context value from a decoder.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    [<Struct>]
    type private PreviousUnfolder<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>> =
        val private codec: 'C
        val private replacement: uint32

        /// <summary>Creates a backward context unfolder.</summary>
        /// <param name="codec">The decoder.</param>
        /// <param name="replacement">The replacement code point.</param>
        new (codec, replacement) = { codec = codec; replacement = replacement }

        interface ISpanUnfolder<'T, int, uint32> with
            member this.Next (source, state) =
                let struct (value, next) = this.codec.Previous(source, state, this.replacement)
                struct (value, next)

    /// <summary>Fills one forward context value from a decoder.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    [<Struct>]
    type private FollowingUnfolder<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>> =
        val private codec: 'C
        val private replacement: uint32

        /// <summary>Creates a forward context unfolder.</summary>
        /// <param name="codec">The decoder.</param>
        /// <param name="replacement">The replacement code point.</param>
        new (codec, replacement) = { codec = codec; replacement = replacement }

        interface ISpanUnfolder<'T, int, uint32> with
            member this.Next (source, state) =
                let struct (value, next) = this.codec.Next(source, state, this.replacement)
                struct (value, next)

    /// <summary>Fills one forward context value from a text that decodes unit by unit.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    [<Struct>]
    type private DirectFollowingUnfolder<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>> =
        val private codec: 'C

        /// <summary>Creates a forward context unfolder.</summary>
        /// <param name="codec">The decoder.</param>
        new (codec) = { codec = codec }

        interface ISpanUnfolder<'T, int, uint32> with
            member this.Next (source, state) =
                struct (this.codec.Unit source[state], state + 1)

    /// <summary>Counts preceding code points.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The text.</param>
    /// <param name="at">The current index.</param>
    /// <param name="remaining">How many more may be counted.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <param name="count">The count so far.</param>
    /// <returns>The number of code points.</returns>
    [<TailCall>]
    let rec private countPrevious<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (at) (remaining) (replacement) (codec: 'C) (count) =
        match at > 0 && remaining > 0 with
        | false -> count
        | true ->
            let struct (_, next) = codec.Previous(text, at, replacement)
            countPrevious text next (remaining - 1) replacement codec (count + 1)

    /// <summary>Counts following code points.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The text.</param>
    /// <param name="at">The current index.</param>
    /// <param name="remaining">How many more may be counted.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <param name="count">The count so far.</param>
    /// <returns>The number of code points.</returns>
    [<TailCall>]
    let rec private countFollowing<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (at) (remaining) (replacement) (codec: 'C) (count) =
        match at < text.Length && remaining > 0 with
        | false -> count
        | true ->
            let struct (_, next) = codec.Next(text, at, replacement)
            countFollowing text next (remaining - 1) replacement codec (count + 1)

    /// <summary>Counts the item's code points.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The item text.</param>
    /// <param name="at">The current index.</param>
    /// <param name="stop">The item end.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <param name="count">The count so far.</param>
    /// <returns>The number of code points.</returns>
    [<TailCall>]
    let rec private countItems<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (at) (stop) (replacement) (codec: 'C) (count) =
        match at >= stop with
        | true -> count
        | false ->
            let struct (_, next) = codec.Next(text, at, replacement)
            countItems text next stop replacement codec (count + 1)

    /// <summary>Collects the preceding context.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The text.</param>
    /// <param name="at">The item offset.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <returns>The context, nearest first.</returns>
    let private collectPrevious<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (at) (replacement) (codec: 'C) =
        let count = countPrevious text at ContextLength replacement codec 0
        Sealed.unfoldSpan text count at (PreviousUnfolder(codec, replacement))

    /// <summary>Collects the following context.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The text.</param>
    /// <param name="at">The item end.</param>
    /// <param name="replacement">The replacement code point.</param>
    /// <param name="codec">The decoder.</param>
    /// <returns>The context, nearest first.</returns>
    let private collectFollowing<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (at) (replacement) (codec: 'C) =
        let window = text.Slice(at, min ContextLength (text.Length - at))
        match codec.Direct window with
        | true -> Sealed.unfoldSpan window window.Length 0 (DirectFollowingUnfolder(codec))
        | false ->
            let count = countFollowing text at ContextLength replacement codec 0
            Sealed.unfoldSpan text count at (FollowingUnfolder(codec, replacement))

    /// <summary>Decodes an item of text, <c>hb_buffer_add_utf</c> without the packaging: the glyphs of the item and the contexts around it.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first unit.</param>
    /// <param name="itemLength">The number of units, or negative for the rest.</param>
    /// <param name="run">The run the item is appended to, read for its replacement and its contexts.</param>
    /// <param name="codec">The decoder.</param>
    /// <returns>The context before, the glyphs and the context after, or none when the item is too long to hold.</returns>
    let private decodeItem<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (itemOffset) (itemLength) (run: GlyphRun) (codec: 'C) =
        let textLength = text.Length
        let offset = if itemOffset < 0 then textLength else min itemOffset textLength
        let length = if itemLength < 0 then textLength - offset else itemLength
        let length = max 0 (min length (textLength - offset))
        match length > Int32.MaxValue / 8 with
        | true -> ValueNone
        | false ->
            let before =
                match run.Infos.IsEmpty && offset > 0 with
                | true -> collectPrevious text offset run.Replacement codec
                | false -> run.ContextBefore
            let itemText = text.Slice(offset, length)
            let infos =
                match codec.Direct itemText with
                | true -> codec.DirectInfos (itemText, offset)
                | false ->
                    match codec.WellFormed itemText with
                    | true -> codec.WellFormedInfos (itemText, offset)
                    | false ->
                        let count = countItems itemText 0 length run.Replacement codec 0
                        Sealed.unfoldSpan itemText count 0 (ItemUnfolder(codec, run.Replacement, offset))
            let after = collectFollowing text (offset + length) run.Replacement codec
            ValueSome (struct (before, infos, after))

    /// <summary>Appends text through a decoder, <c>hb_buffer_add_utf</c>.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first unit.</param>
    /// <param name="itemLength">The number of units, or negative for the rest.</param>
    /// <param name="run">The run.</param>
    /// <param name="codec">The decoder.</param>
    /// <returns>The run with the text appended.</returns>
    let private addSpan<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (itemOffset) (itemLength) (run: GlyphRun) (codec: 'C) =
        match decodeItem text itemOffset itemLength run codec with
        | ValueNone -> run
        | ValueSome (struct (before, infos, after)) ->
            let count = infos.Length
            let allInfos =
                match run.Infos.IsEmpty with
                | true -> infos
                | false -> Sealed.appendSpan (run.Infos.AsSpan()) (infos.AsSpan())
            let props = Sealed.zeroed<GlyphProps> count
            let allProps =
                match run.Props.IsEmpty with
                | true -> props
                | false -> Sealed.appendSpan (run.Props.AsSpan()) (props.AsSpan())
            { run with
                Infos = allInfos
                Props = allProps
                ContextBefore = before
                ContextAfter = after
                ContentType = UnicodeContent }

    /// <summary>Decodes text into a run that has no properties yet, for a caller that shapes the run it decoded and shares it with no one:
    /// the glyphs are those <see cref="addSpan"/> makes, and the properties array, which the first stage of shaping replaces without
    /// reading, is left out. A run that already holds glyphs takes the text as <see cref="addSpan"/> does.</summary>
    /// <typeparam name="'T">The code-unit type.</typeparam>
    /// <typeparam name="'C">The decoder type.</typeparam>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first unit.</param>
    /// <param name="itemLength">The number of units, or negative for the rest.</param>
    /// <param name="run">The settings run.</param>
    /// <param name="codec">The decoder.</param>
    /// <returns>The run with the text appended and, when it had no glyphs, no properties.</returns>
    let private addSpanFresh<'T, 'C when 'C : struct and 'C :> IUtfCodec<'T>>
            (text: ReadOnlySpan<'T>) (itemOffset) (itemLength) (run: GlyphRun) (codec: 'C) =
        match run.Infos.IsEmpty && run.Props.IsEmpty with
        | false -> addSpan text itemOffset itemLength run codec
        | true ->
            match decodeItem text itemOffset itemLength run codec with
            | ValueNone -> run
            | ValueSome (struct (before, infos, after)) ->
                { run with
                    Infos = infos
                    ContextBefore = before
                    ContextAfter = after
                    ContentType = UnicodeContent }

    /// <summary>Appends UTF-8 text, <c>hb_buffer_add_utf8</c>.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first code unit.</param>
    /// <param name="itemLength">The number of code units, or negative for the rest.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the text appended.</returns>
    let addUtf8 (text: ReadOnlySpan<byte>) (itemOffset) (itemLength) (run: GlyphRun) =
        addSpan text itemOffset itemLength run (Utf8Codec())

    /// <summary>Decodes UTF-8 text into a run for a caller that shapes it at once and shares it with no one, <see cref="addUtf8"/> without
    /// the properties array: the run is for <c>Execute.tryRunFresh</c> and <c>Shape.shapeUtf8</c> only.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first code unit.</param>
    /// <param name="itemLength">The number of code units, or negative for the rest.</param>
    /// <param name="run">The settings run.</param>
    /// <returns>The run with the text appended.</returns>
    let internal addUtf8Fresh (text: ReadOnlySpan<byte>) (itemOffset) (itemLength) (run: GlyphRun) =
        addSpanFresh text itemOffset itemLength run (Utf8Codec())

    /// <summary>Appends UTF-16 text, <c>hb_buffer_add_utf16</c>.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first code unit.</param>
    /// <param name="itemLength">The number of code units, or negative for the rest.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the text appended.</returns>
    let addUtf16 (text: ReadOnlySpan<uint16>) (itemOffset) (itemLength) (run: GlyphRun) =
        addSpan text itemOffset itemLength run (Utf16Codec())

    /// <summary>Appends UTF-32 text, <c>hb_buffer_add_utf32</c>.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first code unit.</param>
    /// <param name="itemLength">The number of code units, or negative for the rest.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the text appended.</returns>
    let addUtf32 (text: ReadOnlySpan<uint32>) (itemOffset) (itemLength) (run: GlyphRun) =
        addSpan text itemOffset itemLength run (Utf32Codec())

    /// <summary>Appends Latin-1 text, <c>hb_buffer_add_latin1</c>.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first code unit.</param>
    /// <param name="itemLength">The number of code units, or negative for the rest.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the text appended.</returns>
    let internal addLatin1 (text: ReadOnlySpan<byte>) (itemOffset) (itemLength) (run: GlyphRun) =
        addSpan text itemOffset itemLength run (Latin1Codec())

    /// <summary>Appends code points without validation, <c>hb_buffer_add_codepoints</c>.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="itemOffset">The first code unit.</param>
    /// <param name="itemLength">The number of code units, or negative for the rest.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the text appended.</returns>
    let addCodepoints (text: ReadOnlySpan<uint32>) (itemOffset) (itemLength) (run: GlyphRun) =
        addSpan text itemOffset itemLength run (CodepointsCodec())

    /// <summary>Appends one code point, <c>hb_buffer_add</c>.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <param name="cluster">The cluster.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the glyph appended.</returns>
    let internal add (codepoint) (cluster) (run: GlyphRun) =
        let info = { Codepoint = codepoint; Mask = 0u; Cluster = cluster; Var1 = 0u; Var2 = 0u }
        { run with
            Infos = Sealed.appendOne (run.Infos.AsSpan()) info
            Props = Sealed.appendOne (run.Props.AsSpan()) (Unchecked.defaultof<GlyphProps>)
            ContextAfter = ImmutableArray.Empty }

    /// <summary>Replaces the segment properties.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the properties.</returns>
    let internal withSegment (props: SegmentProperties) (run: GlyphRun) =
        { run with SegmentProperties = props }

    /// <summary>Replaces the direction.</summary>
    /// <param name="d">The direction.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the direction.</returns>
    let withDirection (d: Direction) (run: GlyphRun) =
        { run with SegmentProperties = { run.SegmentProperties with Direction = d } }

    /// <summary>Replaces the script.</summary>
    /// <param name="s">The script.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the script.</returns>
    let withScript (s: Script) (run: GlyphRun) =
        { run with SegmentProperties = { run.SegmentProperties with Script = s } }

    /// <summary>Replaces the language.</summary>
    /// <param name="l">The language.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the language.</returns>
    let withLanguage (l: Language) (run: GlyphRun) =
        { run with SegmentProperties = { run.SegmentProperties with Language = l } }

    /// <summary>Replaces the buffer flags.</summary>
    /// <param name="f">The flags.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the flags.</returns>
    let withFlags (f: BufferFlags) (run: GlyphRun) =
        { run with Flags = f }

    /// <summary>Replaces the cluster level.</summary>
    /// <param name="c">The cluster level.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the cluster level.</returns>
    let withClusterLevel (c: ClusterLevel) (run: GlyphRun) =
        { run with ClusterLevel = c }

    /// <summary>Replaces the replacement code point.</summary>
    /// <param name="u">The replacement.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the replacement.</returns>
    let internal withReplacement (u) (run: GlyphRun) =
        { run with Replacement = u }

    /// <summary>Replaces the invisible glyph.</summary>
    /// <param name="g">The invisible glyph.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the invisible glyph.</returns>
    let internal withInvisible (g) (run: GlyphRun) =
        { run with Invisible = g }

    /// <summary>Replaces the not-found glyph.</summary>
    /// <param name="g">The not-found glyph.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the not-found glyph.</returns>
    let internal withNotFound (g) (run: GlyphRun) =
        { run with NotFound = g }

    /// <summary>Replaces the not-found variation-selector glyph.</summary>
    /// <param name="g">The variation-selector glyph.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the variation-selector glyph.</returns>
    let internal withNotFoundVariationSelector (g) (run: GlyphRun) =
        { run with NotFoundVariationSelector = g }

    /// <summary>Replaces the random state.</summary>
    /// <param name="s">The random state.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the random state.</returns>
    let internal withRandomState (s) (run: GlyphRun) =
        { run with RandomState = s }

    /// <summary>The script of the first glyph that is neither Common, Inherited nor Unknown, the scan of <c>guess_segment_properties</c>
    /// (<c>hb-buffer.cc:746-775</c>) over a span of infos, for a caller whose glyphs are not an immutable array.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="index">The index to start at.</param>
    /// <returns>The script, or <c>Scripts.Invalid</c> when every glyph is neutral.</returns>
    [<TailCall>]
    let rec internal firstNonCommonScriptOf (infos: ReadOnlySpan<GlyphInfo>) index =
        match index >= infos.Length with
        | true -> Scripts.Invalid
        | false ->
            let candidate = Unicode.script infos[index].Codepoint
            match candidate = Scripts.Common || candidate = Scripts.Inherited || candidate = Scripts.Unknown with
            | true -> firstNonCommonScriptOf infos (index + 1)
            | false -> candidate

    /// <summary>The script of the first glyph that is neither Common, Inherited nor Unknown as <see cref="firstNonCommonScriptOf"/> finds
    /// it, the script of a code point given by the Unicode functions of the buffer.</summary>
    /// <param name="scriptOf">The script of a code point.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="index">The index to start at.</param>
    /// <returns>The script, or <c>Scripts.Invalid</c> when every glyph is neutral.</returns>
    [<TailCall>]
    let rec internal firstNonCommonScriptThrough (scriptOf: uint32 -> Script) (infos: ReadOnlySpan<GlyphInfo>) index =
        match index >= infos.Length with
        | true -> Scripts.Invalid
        | false ->
            let candidate = scriptOf infos[index].Codepoint
            match candidate = Scripts.Common || candidate = Scripts.Inherited || candidate = Scripts.Unknown with
            | true -> firstNonCommonScriptThrough scriptOf infos (index + 1)
            | false -> candidate

    /// <summary>Fills missing segment properties, <c>guess_segment_properties</c>, from the script of the first glyph that is neither
    /// Common, Inherited nor Unknown.</summary>
    /// <param name="defaultLanguage">The language to use when none is set.</param>
    /// <param name="props">The properties the run has.</param>
    /// <param name="firstScript">The script of the first glyph that is no neutral one, or <c>Scripts.Invalid</c>; read only when the
    /// properties have no script.</param>
    /// <returns>The properties with the missing parts guessed.</returns>
    let internal guessWith (defaultLanguage: Language) (props: SegmentProperties) (firstScript: Script) =
        let script =
            match props.Script = Scripts.Invalid with
            | false -> props.Script
            | true -> firstScript
        let direction =
            match props.Direction with
            | Direction.Invalid ->
                match Script.horizontalDirection script with
                | Direction.Invalid -> Direction.Ltr
                | guessed -> guessed
            | set -> set
        let language =
            match props.Language with
            | Language.NoLanguage -> defaultLanguage
            | set -> set
        { Direction = direction; Script = script; Language = language }

    /// <summary>Fills missing segment properties from the buffer, <c>guess_segment_properties</c>.</summary>
    /// <param name="defaultLanguage">The language to use when none is set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the guessed properties.</returns>
    let guessSegmentProperties (defaultLanguage: Language) (run: GlyphRun) =
        let script =
            match run.SegmentProperties.Script = Scripts.Invalid with
            | false -> run.SegmentProperties.Script
            | true -> firstNonCommonScriptOf (run.Infos.AsSpan()) 0
        let direction =
            match run.SegmentProperties.Direction with
            | Direction.Invalid ->
                match Script.horizontalDirection script with
                | Direction.Invalid -> Direction.Ltr
                | guessed -> guessed
            | set -> set
        let language =
            match run.SegmentProperties.Language with
            | Language.NoLanguage -> defaultLanguage
            | set -> set
        { run with SegmentProperties = { Direction = direction; Script = script; Language = language } }

    /// <summary>Resizes the run, zero-filling new entries, <c>hb_buffer_set_length</c>.</summary>
    /// <param name="length">The new length.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run, or an unsuccessful run when the length exceeds <c>MaxLen</c>.</returns>
    let internal setLength (length) (run: GlyphRun) =
        match length < 0 || length > run.MaxLen with
        | true -> { run with Successful = false }
        | false ->
            let infos = Sealed.resize (run.Infos.AsSpan()) length
            let positions =
                match run.HavePositions with
                | true -> Sealed.resize (run.Positions.AsSpan()) length
                | false -> ImmutableArray.Empty
            let props = Sealed.resize (run.Props.AsSpan()) length
            { run with
                Infos = infos
                Positions = positions
                Props = props
                ContentType = if length = 0 then NoContent else run.ContentType
                ContextBefore = if length = 0 then ImmutableArray.Empty else run.ContextBefore
                ContextAfter = ImmutableArray.Empty }

    /// <summary>Overlays missing segment properties, <c>hb_segment_properties_overlay</c>.</summary>
    /// <param name="target">The properties to fill.</param>
    /// <param name="source">The properties to fill from.</param>
    /// <returns>The overlaid properties.</returns>
    let private overlay (target: SegmentProperties) (source: SegmentProperties) =
        let direction = match target.Direction with | Direction.Invalid -> source.Direction | set -> set
        let sameDirection = direction = source.Direction
        let script = match sameDirection && target.Script = Scripts.Invalid with | true -> source.Script | false -> target.Script
        let sameScript = script = source.Script
        let language = match sameDirection && sameScript && target.Language = Language.NoLanguage with | true -> source.Language | false -> target.Language
        { Direction = direction; Script = script; Language = language }

    /// <summary>Builds the target positions for an append.</summary>
    /// <param name="run">The target.</param>
    /// <param name="source">The source.</param>
    /// <param name="start">The first source index.</param>
    /// <param name="count">The number of appended glyphs.</param>
    /// <param name="length">The result length.</param>
    /// <param name="havePositions">Whether the result has positions.</param>
    /// <returns>The positions.</returns>
    let private appendPositions (run: GlyphRun) (source: GlyphRun) (start) (count) (length) (havePositions) =
        match havePositions with
        | false -> ImmutableArray.Empty
        | true ->
            let originalLength = length - count
            Sealed.init length (fun index ->
                match index < originalLength, run.HavePositions, source.HavePositions with
                | true, true, _ -> run.Positions[index]
                | true, false, _ -> { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                | false, _, true -> source.Positions[start + index - originalLength]
                | false, _, false -> { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })

    /// <summary>Builds the target properties for an append, treating absent properties as zeroed.</summary>
    /// <param name="run">The target.</param>
    /// <param name="source">The source.</param>
    /// <param name="start">The first source index.</param>
    /// <param name="count">The number of appended glyphs.</param>
    /// <param name="length">The result length.</param>
    /// <returns>The properties.</returns>
    let private appendProps (run: GlyphRun) (source: GlyphRun) (start) (count) (length) =
        let originalLength = length - count
        Sealed.init length (fun index ->
            match index < originalLength, run.Props.IsEmpty, source.Props.IsEmpty with
            | true, false, _ -> run.Props[index]
            | true, true, _ -> Unchecked.defaultof<GlyphProps>
            | false, _, false -> source.Props[start + index - originalLength]
            | false, _, true -> Unchecked.defaultof<GlyphProps>)

    /// <summary>Builds the pre-context for an append.</summary>
    /// <param name="run">The target run.</param>
    /// <param name="source">The source.</param>
    /// <param name="start">The first source index.</param>
    /// <param name="originalLength">The target length before appending.</param>
    /// <returns>The pre-context.</returns>
    let private appendContextBefore (run: GlyphRun) (source: GlyphRun) (start) (originalLength) =
        match source.ContentType with
        | UnicodeContent ->
            match originalLength = 0 && start + source.ContextBefore.Length > 0 with
            | false -> run.ContextBefore
            | true ->
                let fromInfos = min ContextLength start
                let count = min ContextLength (fromInfos + source.ContextBefore.Length)
                Sealed.init count (fun index ->
                    match index < fromInfos with
                    | true -> source.Infos[start - 1 - index].Codepoint
                    | false -> source.ContextBefore[index - fromInfos])
        | NoContent | GlyphContent -> run.ContextBefore

    /// <summary>Builds the post-context for an append.</summary>
    /// <param name="run">The target run.</param>
    /// <param name="source">The source.</param>
    /// <param name="finish">The source end.</param>
    /// <returns>The post-context.</returns>
    let private appendContextAfter (run: GlyphRun) (source: GlyphRun) (finish) =
        match source.ContentType with
        | UnicodeContent ->
            let fromInfos = min ContextLength (source.Infos.Length - finish)
            let count = min ContextLength (fromInfos + source.ContextAfter.Length)
            Sealed.init count (fun index ->
                match index < fromInfos with
                | true -> source.Infos[finish + index].Codepoint
                | false -> source.ContextAfter[index - fromInfos])
        | NoContent | GlyphContent -> run.ContextAfter

    /// <summary>Appends part of a source run, <c>hb_buffer_append</c>.</summary>
    /// <param name="source">The source run.</param>
    /// <param name="start">The first source index.</param>
    /// <param name="finish">The source end.</param>
    /// <param name="run">The target run.</param>
    /// <returns>The target with the source appended.</returns>
    let internal append (source: GlyphRun) (start) (finish) (run: GlyphRun) =
        let finish = min finish source.Infos.Length
        let start = max 0 (min start finish)
        match start = finish with
        | true -> run
        | false ->
            let count = finish - start
            match run.Infos.Length + count < run.Infos.Length with
            | true -> { run with Successful = false }
            | false ->
                let originalLength = run.Infos.Length
                let newLength = originalLength + count
                match newLength > run.MaxLen with
                | true -> { run with Successful = false }
                | false ->
                    let content = if originalLength = 0 then source.ContentType else run.ContentType
                    let havePositions = run.HavePositions || source.HavePositions
                    let positions = appendPositions run source start count newLength havePositions
                    let props = appendProps run source start count newLength
                    let infos = Sealed.insertAt (run.Infos.AsSpan()) (source.Infos.AsSpan(start, count)) originalLength
                    let before = appendContextBefore run source start originalLength
                    let after = appendContextAfter run source finish
                    { run with
                        Infos = infos
                        Positions = positions
                        Props = props
                        HavePositions = havePositions
                        ContentType = content
                        SegmentProperties = overlay run.SegmentProperties source.SegmentProperties
                        ContextBefore = before
                        ContextAfter = after }

    /// <summary>Clamps a source range as <see cref="append"/> does.</summary>
    /// <param name="length">The source length.</param>
    /// <param name="range">The start and end of the range.</param>
    /// <returns>The clamped range.</returns>
    let private clamped (length) (range) =
        let struct (start, finish) = range
        let finish = min finish length
        struct (max 0 (min start finish), finish)

    /// <summary>Appends several ranges of a source run to an empty run in one copy, the result of folding
    /// <see cref="append"/> over the ranges in order, in time linear in the glyphs copied instead of quadratic in the
    /// ranges. A target that is not empty, or that the folded appends would overflow, is folded one range at a time.</summary>
    /// <param name="source">The source run.</param>
    /// <param name="ranges">The start and end of each source range.</param>
    /// <param name="run">The target run.</param>
    /// <returns>The target with the ranges appended.</returns>
    let internal appendRanges (source: GlyphRun) (ranges: struct (int * int) array) (run: GlyphRun) =
        let clampedRanges = ranges |> Array.map (clamped source.Infos.Length)
        let backward = clampedRanges |> Array.exists (fun (struct (start, finish)) -> finish < start)
        let valid = clampedRanges |> Array.filter (fun (struct (start, finish)) -> start < finish)
        let total = valid |> Array.sumBy (fun (struct (start, finish)) -> finish - start)
        match valid.Length, not backward && run.Infos.Length = 0 && total <= run.MaxLen with
        | _, false -> Array.fold (fun target (struct (start, finish)) -> append source start finish target) run ranges
        | 0, true -> run
        | _, true ->
            let struct (first, _) = valid[0]
            let struct (_, last) = valid[valid.Length - 1]
            let havePositions = run.HavePositions || source.HavePositions
            let positions =
                match havePositions, source.HavePositions with
                | false, _ -> ImmutableArray.Empty
                | true, true -> Sealed.gather (source.Positions.AsSpan()) (ReadOnlySpan valid) total
                | true, false -> Sealed.init total (fun _ -> { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
            let props =
                match source.Props.IsEmpty with
                | false -> Sealed.gather (source.Props.AsSpan()) (ReadOnlySpan valid) total
                | true -> Sealed.init total (fun _ -> Unchecked.defaultof<GlyphProps>)
            { run with
                Infos = Sealed.gather (source.Infos.AsSpan()) (ReadOnlySpan valid) total
                Positions = positions
                Props = props
                HavePositions = havePositions
                ContentType = source.ContentType
                SegmentProperties = overlay run.SegmentProperties source.SegmentProperties
                ContextBefore = appendContextBefore run source first 0
                ContextAfter = appendContextAfter run source last }

    /// <summary>Reverses a range of glyphs and positions, <c>hb_buffer_reverse_range</c>.</summary>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the range reversed.</returns>
    let internal reverseRange (start) (finish) (run: GlyphRun) =
        let start = max 0 (min start run.Infos.Length)
        let finish = max 0 (min finish run.Infos.Length)
        match finish < start + 2 with
        | true -> run
        | false ->
            let infos = Sealed.reverseRange (run.Infos.AsSpan()) start finish
            let positions =
                match run.HavePositions with
                | false -> run.Positions
                | true -> Sealed.reverseRange (run.Positions.AsSpan()) start finish
            let props =
                match run.Props.IsEmpty with
                | true -> Sealed.init run.Infos.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
                | false -> Sealed.reverseRange (run.Props.AsSpan()) start finish
            { run with Infos = infos; Positions = positions; Props = props }

    /// <summary>Reverses every glyph and position, <c>hb_buffer_reverse</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The reversed run.</returns>
    let internal reverse (run: GlyphRun) =
        reverseRange 0 run.Infos.Length run

    /// <summary>Reverses each cluster group, then the whole buffer, <c>hb_buffer_reverse_clusters</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with cluster groups reversed.</returns>
    let internal reverseClusters (run: GlyphRun) =
        match run.Infos.Length with
        | 0 -> run
        | _ ->
            let infos = Sealed.reverseClustersInfo (run.Infos.AsSpan())
            let positions =
                match run.HavePositions with
                | true -> Sealed.reverseClustersPositions (run.Infos.AsSpan()) (run.Positions.AsSpan())
                | false -> run.Positions
            let props =
                match run.Props.IsEmpty with
                | true -> Sealed.init run.Infos.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
                | false -> Sealed.reverseClustersProps (run.Infos.AsSpan()) (run.Props.AsSpan())
            { run with Infos = infos; Positions = positions; Props = props }

    /// <summary>The operation limit <see cref="enter"/> gives a run of a length.</summary>
    /// <param name="length">The glyphs of the run.</param>
    /// <param name="maxOps">The limit the run has.</param>
    /// <returns>The limit.</returns>
    let internal enteredMaxOpsOf (length: int) (maxOps: int) =
        let maxOpsProduct = uint64 length * MaxOpsFactor
        match maxOpsProduct <= uint64 UInt32.MaxValue with
        | true ->
            let limit = int (max maxOpsProduct MaxOpsMin)
            match maxOps < MaxOpsDefault with
            | true -> min maxOps limit
            | false -> limit
        | false -> maxOps

    /// <summary>The operation limit <see cref="enter"/> gives a run.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The limit.</returns>
    let internal enteredMaxOps (run: GlyphRun) =
        enteredMaxOpsOf run.Infos.Length run.MaxOps

    /// <summary>The length limit <see cref="enter"/> gives a run of a length.</summary>
    /// <param name="length">The glyphs of the run.</param>
    /// <param name="maxLen">The limit the run has.</param>
    /// <returns>The limit.</returns>
    let internal enteredMaxLenOf (length: int) (maxLen: int) =
        let maxLenProduct = uint64 length * MaxLenFactor
        match maxLenProduct <= uint64 UInt32.MaxValue with
        | true ->
            let limit = int (max maxLenProduct MaxLenMin)
            match maxLen < MaxLenDefault with
            | true -> min maxLen limit
            | false -> limit
        | false -> maxLen

    /// <summary>The length limit <see cref="enter"/> gives a run.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The limit.</returns>
    let internal enteredMaxLen (run: GlyphRun) =
        enteredMaxLenOf run.Infos.Length run.MaxLen

    /// <summary>Prepares a run for shaping, <c>hb_buffer_enter</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with narrowed limits and a fresh serial.</returns>
    let internal enter (run: GlyphRun) =
        { run with MaxLen = enteredMaxLen run; MaxOps = enteredMaxOps run; Serial = 0u; ScratchFlags = ScratchFlags.Default }

    /// <summary>The working set of a run: its fields, the arrays shared with the run.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The working set.</returns>
    let internal setOf (run: GlyphRun) =
        { WorkingSet.Infos = run.Infos
          Props = run.Props
          Positions = run.Positions
          ContextBefore = run.ContextBefore
          ContextAfter = run.ContextAfter
          ContentType = run.ContentType
          SegmentProperties = run.SegmentProperties
          Flags = run.Flags
          ClusterLevel = run.ClusterLevel
          Replacement = run.Replacement
          Invisible = run.Invisible
          NotFound = run.NotFound
          NotFoundVariationSelector = run.NotFoundVariationSelector
          RandomState = run.RandomState
          HavePositions = run.HavePositions
          Successful = run.Successful
          Serial = run.Serial
          ScratchFlags = run.ScratchFlags
          MaxLen = run.MaxLen
          MaxOps = run.MaxOps }

    /// <summary>The run a working set publishes: its fields, the arrays shared with the set.</summary>
    /// <param name="set">The working set.</param>
    /// <returns>The run.</returns>
    let internal runOf (set: WorkingSet) =
        { Infos = set.Infos
          Positions = set.Positions
          Props = set.Props
          ContextBefore = set.ContextBefore
          ContextAfter = set.ContextAfter
          ContentType = set.ContentType
          SegmentProperties = set.SegmentProperties
          Flags = set.Flags
          ClusterLevel = set.ClusterLevel
          Replacement = set.Replacement
          Invisible = set.Invisible
          NotFound = set.NotFound
          NotFoundVariationSelector = set.NotFoundVariationSelector
          RandomState = set.RandomState
          HavePositions = set.HavePositions
          Successful = set.Successful
          Serial = set.Serial
          ScratchFlags = set.ScratchFlags
          MaxLen = set.MaxLen
          MaxOps = set.MaxOps }

    /// <summary>The working set of a run that is entered, <c>hb_buffer_enter</c>, with the glyphs and properties its first stage made.</summary>
    /// <param name="run">The run as it came in.</param>
    /// <param name="infos">The glyphs, made by the caller in this call.</param>
    /// <param name="props">The properties, made by the caller in this call.</param>
    /// <param name="scratch">The scratch flags the first stage found.</param>
    /// <returns>The working set, with the narrowed limits and a fresh serial.</returns>
    let internal enteredSet (run: GlyphRun) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (scratch: ScratchFlags) =
        { WorkingSet.Infos = infos
          Props = props
          Positions = run.Positions
          ContextBefore = run.ContextBefore
          ContextAfter = run.ContextAfter
          ContentType = run.ContentType
          SegmentProperties = run.SegmentProperties
          Flags = run.Flags
          ClusterLevel = run.ClusterLevel
          Replacement = run.Replacement
          Invisible = run.Invisible
          NotFound = run.NotFound
          NotFoundVariationSelector = run.NotFoundVariationSelector
          RandomState = run.RandomState
          HavePositions = run.HavePositions
          Successful = run.Successful
          Serial = 0u
          ScratchFlags = scratch
          MaxLen = enteredMaxLen run
          MaxOps = enteredMaxOps run }

    /// <summary>The fields of a working set the machines read.</summary>
    /// <param name="set">The working set.</param>
    /// <returns>The input.</returns>
    let internal inputOfSet (set: WorkingSet) =
        { Infos = set.Infos
          Props = set.Props
          Positions = set.Positions
          HavePositions = set.HavePositions
          MaxLen = set.MaxLen
          MaxOps = set.MaxOps
          Successful = set.Successful
          ClusterLevel = set.ClusterLevel
          Flags = set.Flags
          Serial = byte set.Serial
          RandomState = set.RandomState
          ScratchFlags = set.ScratchFlags }

    /// <summary>The working set a machine produced from a set whose arrays the caller owns: the fields the machine can change taken from its
    /// result.</summary>
    /// <param name="set">The set the machine read.</param>
    /// <param name="result">The fields the machine produced.</param>
    /// <returns>The set with those fields.</returns>
    let internal applyResult (set: WorkingSet) (result: RunResult) =
        match result.Unchanged with
        | true -> set
        | false ->
            { set with
                Infos = result.Infos
                Props = result.Props
                Positions = result.Positions
                HavePositions = result.HavePositions
                MaxOps = result.MaxOps
                Successful = result.Successful
                Serial = uint32 result.Serial
                RandomState = result.RandomState
                ScratchFlags = result.ScratchFlags }

    /// <summary>Allocates zeroed positions, <c>hb_buffer_clear_positions</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with zeroed positions.</returns>
    let internal clearPositions (run: GlyphRun) =
        { run with HavePositions = true; Positions = Sealed.init run.Infos.Length (fun _ -> { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }) }

    /// <summary>Runs one shaping pass over the run, the out-buffer machine.</summary>
    /// <typeparam name="'P">The pass type.</typeparam>
    /// <typeparam name="'S">The pass state type.</typeparam>
    /// <param name="pass">The pass.</param>
    /// <param name="seed">The initial pass state.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the pass, the same instance when the pass changed nothing.</returns>
    let internal runPass<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> (pass: 'P) (seed: 'S) (run: GlyphRun) =
        let input =
            { Infos = run.Infos
              Props = run.Props
              Positions = run.Positions
              HavePositions = run.HavePositions
              MaxLen = run.MaxLen
              MaxOps = run.MaxOps
              Successful = run.Successful
              ClusterLevel = run.ClusterLevel
              Flags = run.Flags
              Serial = byte run.Serial
              RandomState = run.RandomState
              ScratchFlags = run.ScratchFlags }
        let result = Sealed.runPass input pass seed
        match result.Unchanged with
        | true -> run
        | false ->
            { run with
                Infos = result.Infos
                Props = result.Props
                Positions = result.Positions
                HavePositions = result.HavePositions
                MaxOps = result.MaxOps
                Successful = result.Successful
                Serial = uint32 result.Serial
                RandomState = result.RandomState
                ScratchFlags = result.ScratchFlags }

    /// <summary>Runs the lookup kernel over the run, on one working copy of the arrays, with what a look at the run's glyphs
    /// has already said about the lookups.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The lookups.</param>
    /// <param name="admission">The look at the glyphs, or an unknown admission.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the kernel, the same instance when the kernel changed nothing.</returns>
    let internal runKernelAdmitted<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: 'P) (admission: RunAdmission) (run: GlyphRun) =
        let input =
            { Infos = run.Infos
              Props = run.Props
              Positions = run.Positions
              HavePositions = run.HavePositions
              MaxLen = run.MaxLen
              MaxOps = run.MaxOps
              Successful = run.Successful
              ClusterLevel = run.ClusterLevel
              Flags = run.Flags
              Serial = byte run.Serial
              RandomState = run.RandomState
              ScratchFlags = run.ScratchFlags }
        let result = Sealed.runGsubAdmitted input program admission
        match result.Unchanged with
        | true -> run
        | false ->
            { run with
                Infos = result.Infos
                Props = result.Props
                Positions = result.Positions
                HavePositions = result.HavePositions
                MaxOps = result.MaxOps
                Successful = result.Successful
                Serial = uint32 result.Serial
                RandomState = result.RandomState
                ScratchFlags = result.ScratchFlags }

    /// <summary>Runs the lookup kernel over the run, on one working copy of the arrays.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The lookups.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the kernel, the same instance when the kernel changed nothing.</returns>
    let internal runKernel<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: 'P) (run: GlyphRun) =
        runKernelAdmitted program { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty } run

    /// <summary>Runs the lookup kernel over a run whose arrays the caller made in this call and has shared with no one, writing into them in
    /// place: the result of <see cref="runKernel"/> in every field, without the copies of the arrays.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The lookups.</param>
    /// <param name="run">The run, with arrays of its own.</param>
    /// <returns>The run after the kernel, the same instance when the kernel changed nothing.</returns>
    let internal runKernelOwned<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: 'P) (run: GlyphRun) =
        let input =
            { Infos = run.Infos
              Props = run.Props
              Positions = run.Positions
              HavePositions = run.HavePositions
              MaxLen = run.MaxLen
              MaxOps = run.MaxOps
              Successful = run.Successful
              ClusterLevel = run.ClusterLevel
              Flags = run.Flags
              Serial = byte run.Serial
              RandomState = run.RandomState
              ScratchFlags = run.ScratchFlags }
        let result = Sealed.runGsubWindowOwned input program { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty }
        match result.Unchanged with
        | true -> run
        | false ->
            { run with
                Infos = result.Infos
                Props = result.Props
                Positions = result.Positions
                HavePositions = result.HavePositions
                MaxOps = result.MaxOps
                Successful = result.Successful
                Serial = uint32 result.Serial
                RandomState = result.RandomState
                ScratchFlags = result.ScratchFlags }

    /// <summary>Runs the lookup kernel over a working set whose arrays the caller made in this call and has shared with no one, as
    /// <see cref="runKernelOwned"/> does a run, without a run record: the lookups write into the arrays in place.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The lookups.</param>
    /// <param name="set">The working set, with arrays of its own.</param>
    /// <returns>The set after the kernel.</returns>
    let internal runKernelOwnedSet<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: 'P) (set: WorkingSet) =
        applyResult set (Sealed.runGsubWindowOwned (inputOfSet set) program { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty })

    /// <summary>The run a machine produced from a run whose arrays the caller owns, the same instance when every field the machine can change
    /// equals the run's: a pass that wrote into the arrays in place and changed no limit or flag needs no new record.</summary>
    /// <param name="run">The run the machine read.</param>
    /// <param name="result">The fields the machine produced.</param>
    /// <returns>The run with those fields.</returns>
    let internal adopt (run: GlyphRun) (result: RunResult) =
        match result.Infos.Equals run.Infos
              && result.Props.Equals run.Props
              && result.Positions.Equals run.Positions
              && result.HavePositions = run.HavePositions
              && result.MaxOps = run.MaxOps
              && result.Successful = run.Successful
              && result.ScratchFlags = run.ScratchFlags with
        | true -> run
        | false ->
            { run with
                Infos = result.Infos
                Props = result.Props
                Positions = result.Positions
                HavePositions = result.HavePositions
                MaxOps = result.MaxOps
                Successful = result.Successful
                ScratchFlags = result.ScratchFlags }

    /// <summary>Runs the positioning kernel over the run, on working copies of the arrays it writes.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The lookups.</param>
    /// <param name="admission">The look at the glyphs, or an unknown admission.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the kernel, the same instance when the kernel changed nothing.</returns>
    let internal runPosKernelAdmitted<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (program: 'P) (admission: RunAdmission) (run: GlyphRun) =
        let input =
            { Infos = run.Infos
              Props = run.Props
              Positions = run.Positions
              HavePositions = run.HavePositions
              MaxLen = run.MaxLen
              MaxOps = run.MaxOps
              Successful = run.Successful
              ClusterLevel = run.ClusterLevel
              Flags = run.Flags
              Serial = byte run.Serial
              RandomState = run.RandomState
              ScratchFlags = run.ScratchFlags }
        let result = Sealed.runGposAdmitted input program admission
        match result.Unchanged with
        | true -> run
        | false ->
            { run with
                Infos = result.Infos
                Positions = result.Positions
                MaxOps = result.MaxOps
                Successful = result.Successful
                ScratchFlags = result.ScratchFlags }

    /// <summary>Runs the positioning kernel over the run, looking at its glyphs itself.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The lookups.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the kernel, the same instance when the kernel changed nothing.</returns>
    let internal runPosKernel<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (program: 'P) (run: GlyphRun) =
        runPosKernelAdmitted program { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty } run

    /// <summary>Sorts a range of infos, merging clusters of moved spans, <c>hb_buffer_t::sort</c>.</summary>
    /// <typeparam name="'C">The comparer type.</typeparam>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="comparer">The comparer.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the range sorted.</returns>
    let internal sortClusters<'C when 'C : struct and 'C :> IGlyphComparer> (start: int) (finish: int) (comparer: 'C) (run: GlyphRun) =
        let struct (infos, props, maxOps, successful) =
            Sealed.sortClusters (run.Infos.AsSpan()) (run.Props.AsSpan()) run.ClusterLevel run.MaxOps start finish comparer
        { run with Infos = infos; Props = props; MaxOps = maxOps; Successful = successful }

    /// <summary>Sorts a range of infos with a properties-aware comparer, merging clusters of moved spans, <c>hb_buffer_t::sort</c>.</summary>
    /// <typeparam name="'C">The comparer type.</typeparam>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="comparer">The comparer.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the range sorted.</returns>
    let internal sortClustersByProps<'C when 'C : struct and 'C :> IGlyphPropsComparer> (start: int) (finish: int) (comparer: 'C) (run: GlyphRun) =
        let struct (infos, props, maxOps, successful) =
            Sealed.sortClustersByProps (run.Infos.AsSpan()) (run.Props.AsSpan()) run.ClusterLevel run.MaxOps start finish comparer
        { run with Infos = infos; Props = props; MaxOps = maxOps; Successful = successful }

    /// <summary>Reverses each group, then the whole buffer, <c>hb_buffer_reverse_groups</c>.</summary>
    /// <typeparam name="'M">The grouping type.</typeparam>
    /// <param name="grouping">The grouping.</param>
    /// <param name="mergeClusters">Whether each group's clusters are merged before it is reversed.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with its groups reversed.</returns>
    let internal reverseGroups<'M when 'M : struct and 'M :> IGlyphGrouping> (grouping: 'M) (mergeClusters: bool) (run: GlyphRun) =
        match run.Infos.Length with
        | 0 -> run
        | _ ->
            let struct (infos, props, positions, maxOps, successful) =
                Sealed.reverseGroups grouping mergeClusters (run.Infos.AsSpan()) (run.Props.AsSpan()) (run.Positions.AsSpan()) run.HavePositions run.ClusterLevel run.Flags run.MaxOps run.Successful
            { run with Infos = infos; Props = props; Positions = positions; MaxOps = maxOps; Successful = successful }

    /// <summary>Deletes filtered glyphs in place, merging their clusters, <c>hb_buffer_t::delete_glyphs_inplace</c>.</summary>
    /// <typeparam name="'P">The predicate type.</typeparam>
    /// <param name="predicate">The deletion predicate.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the filtered glyphs removed.</returns>
    let internal deleteGlyphsInplace<'P when 'P : struct and 'P :> IGlyphPredicate> (predicate: 'P) (run: GlyphRun) =
        match run.Infos.Length with
        | 0 -> run
        | _ ->
            let struct (infos, props, positions, maxOps, successful) =
                Sealed.deleteGlyphsInplace predicate (run.Infos.AsSpan()) (run.Props.AsSpan()) (run.Positions.AsSpan()) run.HavePositions run.ClusterLevel run.MaxOps run.Successful
            { run with Infos = infos; Props = props; Positions = positions; MaxOps = maxOps; Successful = successful }

    /// <summary>Merges the clusters of every group a grouping joins, <c>hb_form_clusters</c>.</summary>
    /// <typeparam name="'M">The grouping type.</typeparam>
    /// <param name="grouping">The grouping.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the clusters merged, the same instance when no group has two glyphs.</returns>
    let internal mergeGroups<'M when 'M : struct and 'M :> IGlyphGrouping> (grouping: 'M) (run: GlyphRun) =
        let input =
            { Infos = run.Infos
              Props = run.Props
              Positions = run.Positions
              HavePositions = run.HavePositions
              MaxLen = run.MaxLen
              MaxOps = run.MaxOps
              Successful = run.Successful
              ClusterLevel = run.ClusterLevel
              Flags = run.Flags
              Serial = byte run.Serial
              RandomState = run.RandomState
              ScratchFlags = run.ScratchFlags }
        let result = Sealed.mergeGroups grouping input
        match result.Unchanged with
        | true -> run
        | false ->
            { run with
                Infos = result.Infos
                Props = result.Props
                Positions = result.Positions
                HavePositions = result.HavePositions
                MaxOps = result.MaxOps
                Successful = result.Successful
                Serial = uint32 result.Serial
                RandomState = result.RandomState
                ScratchFlags = result.ScratchFlags }

    /// <summary>Overlays missing segment properties, <c>hb_segment_properties_overlay</c>.</summary>
    /// <param name="target">The properties to fill.</param>
    /// <param name="source">The properties to fill from.</param>
    /// <returns>The overlaid properties.</returns>
    let internal overlayProperties target source =
        overlay target source
