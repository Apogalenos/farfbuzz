namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

/// <summary>One nested-lookup record of a context or chained rule, <c>LookupRecord</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal LookupRecord =
    { SequenceIndex: int
      LookupListIndex: int }

/// <summary>
/// A read-only view of a run while a shaping pass executes: the remaining input, the output built so far and
/// the buffer properties the pass consults.
/// </summary>
[<IsByRefLike; Struct>]
type internal RunCursor(input: ReadOnlySpan<GlyphInfo>, inputProps: ReadOnlySpan<GlyphProps>, index: int,
               output: ReadOnlySpan<GlyphInfo>, outputProps: ReadOnlySpan<GlyphProps>,
               positions: ReadOnlySpan<GlyphPosition>,
               haveOutput: bool, clusterLevel: ClusterLevel, flags: BufferFlags,
               serial: byte, randomState: uint32, maxOps: int, successful: bool) =
    /// <summary>The whole input span; the pass reads from <see cref="Index"/> on.</summary>
    member _.Input = input
    /// <summary>The per-glyph properties parallel to <see cref="Input"/>.</summary>
    member _.InputProps = inputProps
    /// <summary>The next input index the pass consumes.</summary>
    member _.Index = index
    /// <summary>The output built so far, empty when <see cref="HaveOutput"/> is false.</summary>
    member _.Output = output
    /// <summary>The per-glyph properties parallel to <see cref="Output"/>.</summary>
    member _.OutputProps = outputProps
    /// <summary>The glyph positions parallel to <see cref="Input"/>, present when the pass runs in place.</summary>
    member _.Positions = positions
    /// <summary>Whether the pass has opened an out-buffer.</summary>
    member _.HaveOutput = haveOutput
    /// <summary>The buffer cluster level.</summary>
    member _.ClusterLevel = clusterLevel
    /// <summary>The buffer flags.</summary>
    member _.Flags = flags
    /// <summary>The buffer serial, <c>hb_buffer_t::serial</c>.</summary>
    member _.Serial = serial
    /// <summary>The buffer random state, <c>hb_buffer_t::random_state</c>.</summary>
    member _.RandomState = randomState
    /// <summary>The buffer operation budget, <c>hb_buffer_t::max_ops</c>.</summary>
    member _.MaxOps = maxOps
    /// <summary>Whether the buffer has not failed, <c>hb_buffer_t::successful</c>.</summary>
    member _.Successful = successful

/// <summary>The kind of one <see cref="RunEdit"/>, the tag the out-buffer machine dispatches on.</summary>
type internal EditKind =
    | ClearOutput = 0
    | NextGlyph = 1
    | NextGlyphs = 2
    | ReplaceGlyph = 3
    | ReplaceGlyphs = 4
    | OutputGlyph = 5
    | OutputInfo = 6
    | CopyGlyph = 7
    | SkipGlyph = 8
    | DeleteGlyph = 9
    | MoveTo = 10
    | MergeClusters = 11
    | MergeGraphemeClusters = 12
    | MergeOutClusters = 13
    | UnsafeToBreak = 14
    | UnsafeToConcat = 15
    | UnsafeToBreakFromOutbuffer = 16
    | UnsafeToConcatFromOutbuffer = 17
    | SafeToInsertTatweel = 18
    | SetMasks = 19
    | OrMask = 20
    | SetCurrentProps = 21
    | SetOutputGlyph = 22
    | PopOutput = 23
    | Sync = 24
    | Finish = 25
    | SetInputProps = 26
    | SetPosition = 27
    | SetIndex = 28
    | SetSerial = 29
    | SetRandomState = 30
    | AddScratch = 31
    | ChargeOps = 32
    | Fail = 33
    | SetInputGlyph = 34
    | Rearrange = 35
    | ReplaceGlyphProps = 36
    | MoveOutput = 37
    | MergeOutGraphemeClusters = 38

/// <summary>
/// One edit a shaping pass applies to the out-buffer machine. A 48-byte struct: a kind tag, eight payload words
/// that every kind reuses, and one reference for the kinds whose payload is an array. The kinds that carry a
/// <c>GlyphInfo</c> or a <c>GlyphPosition</c> keep the whole record inline — <c>OutputInfo</c> packs its glyph info
/// and properties into the eight words, and <c>SetPosition</c> packs its index and position — so no edit allocates.
/// </summary>
[<Struct>]
type internal RunEdit =
    val internal Kind: EditKind
    val internal W0: uint32
    val internal W1: uint32
    val internal W2: uint32
    val internal W3: uint32
    val internal W4: uint32
    val internal W5: uint32
    val internal W6: uint32
    val internal W7: uint32
    val internal Ref: obj | null

    /// <summary>Creates an edit from five payload words and a reference; the remaining words are zero.</summary>
    internal new (kind: EditKind, w0: uint32, w1: uint32, w2: uint32, w3: uint32, w4: uint32, ref: obj | null) =
        { Kind = kind
          W0 = w0
          W1 = w1
          W2 = w2
          W3 = w3
          W4 = w4
          W5 = 0u
          W6 = 0u
          W7 = 0u
          Ref = ref }

    /// <summary>Creates an edit from eight payload words and a reference.</summary>
    internal new (kind: EditKind, w0: uint32, w1: uint32, w2: uint32, w3: uint32, w4: uint32, w5: uint32, w6: uint32, w7: uint32, ref: obj | null) =
        { Kind = kind
          W0 = w0
          W1 = w1
          W2 = w2
          W3 = w3
          W4 = w4
          W5 = w5
          W6 = w6
          W7 = w7
          Ref = ref }

    /// <summary>Packs glyph properties into three payload words.</summary>
    static member internal PackProps (props: GlyphProps) : struct (uint32 * uint32 * uint32) =
        struct (
            (uint32 props.UnicodeProps) ||| (uint32 props.GlyphClass <<< 16),
            (uint32 props.LigProps)
            ||| (uint32 props.Syllable <<< 8)
            ||| (uint32 props.ShaperCategory <<< 16)
            ||| (uint32 props.ShaperAuxiliary <<< 24),
            props.GlyphIndex)

    /// <summary>Unpacks three payload words into glyph properties.</summary>
    static member internal UnpackProps (p0: uint32) (p1: uint32) (p2: uint32) : GlyphProps =
        { UnicodeProps = uint16 (p0 &&& 0xFFFFu)
          GlyphClass = uint16 (p0 >>> 16)
          LigProps = byte (p1 &&& 0xFFu)
          Syllable = byte ((p1 >>> 8) &&& 0xFFu)
          ShaperCategory = byte ((p1 >>> 16) &&& 0xFFu)
          ShaperAuxiliary = byte ((p1 >>> 24) &&& 0xFFu)
          GlyphIndex = p2 }

    /// <summary>Packs a glyph info into its five payload words, preserving every field's bits.</summary>
    static member internal PackInfo (info: GlyphInfo) : struct (uint32 * uint32 * uint32 * uint32 * uint32) =
        struct (info.Codepoint, info.Mask, info.Cluster, info.Var1, info.Var2)

    /// <summary>Unpacks five payload words into a glyph info, preserving every field's bits.</summary>
    static member internal UnpackInfo (w0: uint32) (w1: uint32) (w2: uint32) (w3: uint32) (w4: uint32) : GlyphInfo =
        { Codepoint = w0
          Mask = w1
          Cluster = w2
          Var1 = w3
          Var2 = w4 }

    /// <summary>Packs a glyph position into its five payload words, preserving every field's bits.</summary>
    static member internal PackPosition (position: GlyphPosition) : struct (uint32 * uint32 * uint32 * uint32 * uint32) =
        struct (uint32 position.XAdvance, uint32 position.YAdvance, uint32 position.XOffset, uint32 position.YOffset, position.Var)

    /// <summary>Unpacks five payload words into a glyph position, preserving every field's bits.</summary>
    static member internal UnpackPosition (w0: uint32) (w1: uint32) (w2: uint32) (w3: uint32) (w4: uint32) : GlyphPosition =
        { XAdvance = int w0
          YAdvance = int w1
          XOffset = int w2
          YOffset = int w3
          Var = w4 }

    /// <summary>The edit that opens the out-buffer and clears positions.</summary>
    static member ClearOutput = RunEdit(EditKind.ClearOutput, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that advances one glyph.</summary>
    static member NextGlyph = RunEdit(EditKind.NextGlyph, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that advances several glyphs.</summary>
    static member NextGlyphs (count: int) = RunEdit(EditKind.NextGlyphs, uint32 count, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that rewrites one glyph's code point.</summary>
    static member ReplaceGlyph (replacement: uint32) = RunEdit(EditKind.ReplaceGlyph, replacement, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that rewrites the current glyph's code point and properties, consuming one glyph,
    /// <c>replace_glyph</c>.</summary>
    static member ReplaceGlyphProps (codepoint: uint32, props: GlyphProps) =
        let struct (p0, p1, p2) = RunEdit.PackProps props
        RunEdit(EditKind.ReplaceGlyphProps, codepoint, p0, p1, p2, 0u, null)
    /// <summary>The edit that replaces several input glyphs with the produced code points.</summary>
    static member ReplaceGlyphs (consumed: int, produced: ImmutableArray<uint32>) =
        RunEdit(EditKind.ReplaceGlyphs, uint32 consumed, uint32 produced.Length, 0u, 0u, 0u, ImmutableCollectionsMarshal.AsArray produced)
    /// <summary>The edit that outputs one glyph without consuming input.</summary>
    static member OutputGlyph (output: uint32) = RunEdit(EditKind.OutputGlyph, output, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that outputs a whole glyph with its properties.</summary>
    static member OutputInfo (info: GlyphInfo, props: GlyphProps) =
        let struct (i0, i1, i2, i3, i4) = RunEdit.PackInfo info
        let struct (p0, p1, p2) = RunEdit.PackProps props
        RunEdit(EditKind.OutputInfo, i0, i1, i2, i3, i4, p0, p1, p2, null)
    /// <summary>The edit that copies the current glyph to the output without advancing.</summary>
    static member CopyGlyph = RunEdit(EditKind.CopyGlyph, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that advances the cursor without output.</summary>
    static member SkipGlyph = RunEdit(EditKind.SkipGlyph, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that deletes the current glyph.</summary>
    static member DeleteGlyph = RunEdit(EditKind.DeleteGlyph, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that moves the cursor to an output index.</summary>
    static member MoveTo (target: int) = RunEdit(EditKind.MoveTo, uint32 target, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that merges clusters over a range.</summary>
    static member MergeClusters (mergeStart: int, mergeEnd: int) =
        RunEdit(EditKind.MergeClusters, uint32 mergeStart, uint32 mergeEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that merges grapheme clusters over a range.</summary>
    static member MergeGraphemeClusters (graphemeStart: int, graphemeEnd: int) =
        RunEdit(EditKind.MergeGraphemeClusters, uint32 graphemeStart, uint32 graphemeEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that merges output clusters over a range.</summary>
    static member MergeOutClusters (outStart: int, outEnd: int) =
        RunEdit(EditKind.MergeOutClusters, uint32 outStart, uint32 outEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that marks a range unsafe to break and concat.</summary>
    static member UnsafeToBreak (breakStart: int, breakEnd: int) =
        RunEdit(EditKind.UnsafeToBreak, uint32 breakStart, uint32 breakEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that marks a range unsafe to concat.</summary>
    static member UnsafeToConcat (concatStart: int, concatEnd: int) =
        RunEdit(EditKind.UnsafeToConcat, uint32 concatStart, uint32 concatEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that marks an output-buffer range unsafe to break and concat.</summary>
    static member UnsafeToBreakFromOutbuffer (obStart: int, obEnd: int) =
        RunEdit(EditKind.UnsafeToBreakFromOutbuffer, uint32 obStart, uint32 obEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that marks an output-buffer range unsafe to concat.</summary>
    static member UnsafeToConcatFromOutbuffer (ocStart: int, ocEnd: int) =
        RunEdit(EditKind.UnsafeToConcatFromOutbuffer, uint32 ocStart, uint32 ocEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that marks a range safe to insert a tatweel.</summary>
    static member SafeToInsertTatweel (tatweelStart: int, tatweelEnd: int) =
        RunEdit(EditKind.SafeToInsertTatweel, uint32 tatweelStart, uint32 tatweelEnd, 0u, 0u, 0u, null)
    /// <summary>The edit that rewrites glyph masks over a cluster range.</summary>
    static member SetMasks (value: uint32, mask: uint32, clusterStart: uint32, clusterEnd: uint32) =
        RunEdit(EditKind.SetMasks, value, mask, clusterStart, clusterEnd, 0u, null)
    /// <summary>The edit that ors a mask into an index range.</summary>
    static member OrMask (mask: uint32, start: int, finish: int) =
        RunEdit(EditKind.OrMask, mask, uint32 start, uint32 finish, 0u, 0u, null)
    /// <summary>The edit that rewrites the current glyph's properties.</summary>
    static member SetCurrentProps (current: GlyphProps) =
        let struct (p0, p1, p2) = RunEdit.PackProps current
        RunEdit(EditKind.SetCurrentProps, 0u, p0, p1, p2, 0u, null)
    /// <summary>The edit that rewrites one output glyph's code point and properties.</summary>
    static member SetOutputGlyph (outputIndex: int, codepoint: uint32, outputProps: GlyphProps) =
        let struct (p0, p1, p2) = RunEdit.PackProps outputProps
        RunEdit(EditKind.SetOutputGlyph, uint32 outputIndex, p0, p1, p2, codepoint, null)
    /// <summary>The edit that removes the last output glyph.</summary>
    static member PopOutput = RunEdit(EditKind.PopOutput, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that finalises the out-buffer.</summary>
    static member Sync = RunEdit(EditKind.Sync, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that finalises the out-buffer and ends the pass.</summary>
    static member Finish = RunEdit(EditKind.Finish, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that rewrites one input glyph's properties.</summary>
    static member SetInputProps (inputIndex: int, inputProps: GlyphProps) =
        let struct (p0, p1, p2) = RunEdit.PackProps inputProps
        RunEdit(EditKind.SetInputProps, uint32 inputIndex, p0, p1, p2, 0u, null)
    /// <summary>The edit that rewrites one glyph position.</summary>
    static member SetPosition (positionIndex: int, position: GlyphPosition) =
        let struct (w1, w2, w3, w4, w5) = RunEdit.PackPosition position
        RunEdit(EditKind.SetPosition, uint32 positionIndex, w1, w2, w3, w4, w5, 0u, 0u, null)
    /// <summary>The edit that rewinds the cursor to an index.</summary>
    static member SetIndex (index: int) = RunEdit(EditKind.SetIndex, uint32 index, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that rewrites the buffer serial.</summary>
    static member SetSerial (serial: byte) = RunEdit(EditKind.SetSerial, uint32 serial, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that rewrites the buffer random state.</summary>
    static member SetRandomState (randomState: uint32) = RunEdit(EditKind.SetRandomState, randomState, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that ors a scratch flag into the buffer.</summary>
    static member AddScratch (scratch: ScratchFlags) = RunEdit(EditKind.AddScratch, uint32 scratch, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that charges operations against the buffer budget.</summary>
    static member ChargeOps (opsCharged: int) = RunEdit(EditKind.ChargeOps, uint32 opsCharged, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that fails the buffer.</summary>
    static member Fail = RunEdit(EditKind.Fail, 0u, 0u, 0u, 0u, 0u, null)
    /// <summary>The edit that rewrites one input glyph's code point and properties in place.</summary>
    static member SetInputGlyph (inputIndex: int, codepoint: uint32, inputProps: GlyphProps) =
        let struct (p0, p1, p2) = RunEdit.PackProps inputProps
        RunEdit(EditKind.SetInputGlyph, uint32 inputIndex, codepoint, p0, p1, p2, null)
    /// <summary>The edit that rearranges a range of input glyphs, flipping the moved sides when asked.</summary>
    static member Rearrange (rearrangeStart: int, rearrangeEnd: int, moveLeft: int, moveRight: int, flipLeft: bool, flipRight: bool) =
        let flags = (if flipLeft then 1u else 0u) ||| (if flipRight then 2u else 0u)
        RunEdit(EditKind.Rearrange, uint32 rearrangeStart, uint32 rearrangeEnd, uint32 moveLeft, uint32 moveRight, flags, null)
    /// <summary>The edit that moves one output glyph, with its properties, to an earlier output index, shifting the glyphs
    /// between them up by one, the <c>memmove</c> of <c>preprocess_text_thai</c>.</summary>
    static member MoveOutput (fromOutput: int, toOutput: int) = RunEdit(EditKind.MoveOutput, uint32 fromOutput, uint32 toOutput, 0u, 0u, 0u, null)
    /// <summary>The edit that merges output grapheme clusters over a range, <c>merge_out_grapheme_clusters</c>.</summary>
    static member MergeOutGraphemeClusters (outStart: int, outEnd: int) =
        RunEdit(EditKind.MergeOutGraphemeClusters, uint32 outStart, uint32 outEnd, 0u, 0u, 0u, null)

/// <summary>A shaping pass: one <see cref="Step"/> answers one edit for a cursor and state.</summary>
type internal IRunPass<'S> =
    /// <summary>Produces the next edit from the cursor and advances the pass state.</summary>
    /// <param name="cursor">The run state.</param>
    /// <param name="state">The pass state.</param>
    /// <returns>The edit and the next pass state.</returns>
    abstract Step: cursor: inref<RunCursor> * state: 'S -> struct (RunEdit * 'S)

/// <summary>Compares two glyphs for <see cref="Sealed.sortClusters"/>.</summary>
type internal IGlyphComparer =
    /// <summary>Orders two glyph infos.</summary>
    /// <param name="a">The first glyph.</param>
    /// <param name="b">The second glyph.</param>
    /// <returns>Negative when a comes first, positive when b comes first, zero when equal.</returns>
    abstract Compare: a: GlyphInfo * b: GlyphInfo -> int

/// <summary>Compares two glyphs with their properties for <see cref="Sealed.sortClustersByProps"/>.</summary>
type internal IGlyphPropsComparer =
    /// <summary>Orders two glyph info and property pairs.</summary>
    /// <param name="aInfo">The first glyph.</param>
    /// <param name="aProps">The first glyph's properties.</param>
    /// <param name="bInfo">The second glyph.</param>
    /// <param name="bProps">The second glyph's properties.</param>
    /// <returns>Negative when a comes first, positive when b comes first, zero when equal.</returns>
    abstract Compare: aInfo: GlyphInfo * aProps: GlyphProps * bInfo: GlyphInfo * bProps: GlyphProps -> int

/// <summary>The run fields <see cref="Sealed.runPass"/> reads.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal RunInput =
    { Infos: ImmutableArray<GlyphInfo>
      Props: ImmutableArray<GlyphProps>
      Positions: ImmutableArray<GlyphPosition>
      HavePositions: bool
      MaxLen: int
      MaxOps: int
      Successful: bool
      ClusterLevel: ClusterLevel
      Flags: BufferFlags
      Serial: byte
      RandomState: uint32
      ScratchFlags: ScratchFlags }

/// <summary>What one look at the glyphs of a run says about the lookups of a program: the digest of its glyphs and the
/// lookups that cover one of them, one bit each for the first 64 positions. A caller that has just made that look, to
/// decide whether a lookup can apply at all, hands it to the kernel so the kernel does not make it again.</summary>
[<Struct; NoComparison>]
[<NoEquality>]
type internal RunAdmission =
    { Known: bool // false: the kernel looks at the glyphs itself
      Candidates: uint64
      GlyphDigest: Digest }

/// <summary>The run fields <see cref="Sealed.runPass"/> produces.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal RunResult =
    { Infos: ImmutableArray<GlyphInfo>
      Props: ImmutableArray<GlyphProps>
      Positions: ImmutableArray<GlyphPosition>
      HavePositions: bool
      MaxOps: int
      Successful: bool
      Unchanged: bool
      Serial: byte
      RandomState: uint32
      ScratchFlags: ScratchFlags }

/// <summary>A per-glyph map of a glyph and its properties, one pure step of a shaping program.</summary>
type internal IInfoPropsMap =
    /// <summary>Maps one glyph and its properties to the glyph and properties that replace them.</summary>
    /// <param name="info">The glyph info.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The replacement glyph and properties.</returns>
    abstract Map : info: GlyphInfo * props: GlyphProps -> struct (GlyphInfo * GlyphProps)

/// <summary>A per-glyph map of a glyph's position given the glyph and its properties, one pure step of a shaping program.</summary>
type internal IPositionMap =
    /// <summary>Maps one glyph's position.</summary>
    /// <param name="info">The glyph info.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="position">The current position.</param>
    /// <returns>The replacement position.</returns>
    abstract Map : info: GlyphInfo * props: GlyphProps * position: GlyphPosition -> GlyphPosition

/// <summary>A whole-run transform of the run's fields, one step of a shaping program that is not a per-glyph
/// map and not a run pass. A transform that changes nothing returns the input fields with
/// <see cref="RunResult.Unchanged"/> true, so the interpreter keeps its working copy.</summary>
type internal IRunTransform =
    /// <summary>Transforms a whole run.</summary>
    /// <param name="input">The run fields the interpreter has built so far.</param>
    /// <returns>The transformed run fields.</returns>
    abstract Transform : input: RunInput -> RunResult

/// <summary>The immutable data of one ligature substitution, kept off the hot single-substitution path.</summary>
[<NoEquality; NoComparison>]
type internal GsubLigateData =
    { LigGlyph: uint32
      FinalProps: GlyphProps
      Components: ImmutableArray<uint32>
      LigId: byte
      IsLigature: bool
      IsMarkLigature: bool
      MatchEnd: int
      Count: int
      NextSerial: byte
      ConcatAfter: int } // the fast path's unsafe-to-concat bound, marked from the glyph the ligation ends on, or -1

/// <summary>The facts of one matched ligature, what <c>ligate_input</c> needs once <c>Ligature::apply</c> has matched it,
/// as a value: the lookup kernel keeps them on the stack, the interpreter keeps them as a <see cref="GsubLigateData"/>.</summary>
[<Struct>]
type internal LigateFacts =
    val LigGlyph: uint32
    val FinalProps: GlyphProps
    val Components: ImmutableArray<uint32>
    val LigId: byte
    val IsLigature: bool
    val IsMarkLigature: bool
    val MatchEnd: int
    val Count: int
    val NextSerial: byte

    /// <summary>Creates the facts of a matched ligature.</summary>
    /// <param name="ligGlyph">The ligature glyph.</param>
    /// <param name="finalProps">The ligature glyph's properties.</param>
    /// <param name="components">The components after the first glyph.</param>
    /// <param name="ligId">The ligature id.</param>
    /// <param name="isLigature">Whether the ligature is a real ligature.</param>
    /// <param name="isMarkLigature">Whether the ligature is a mark ligature.</param>
    /// <param name="matchEnd">The end of the match.</param>
    /// <param name="count">The component count, including the first glyph.</param>
    /// <param name="nextSerial">The serial after the ligature id is allocated.</param>
    new (ligGlyph, finalProps, components, ligId, isLigature, isMarkLigature, matchEnd, count, nextSerial) =
        { LigGlyph = ligGlyph
          FinalProps = finalProps
          Components = components
          LigId = ligId
          IsLigature = isLigature
          IsMarkLigature = isMarkLigature
          MatchEnd = matchEnd
          Count = count
          NextSerial = nextSerial }

/// <summary>The outcome of applying one ligature of a ligature set, <c>Ligature::apply</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal LigatureTry =
    | LigatureMiss
    | LigatureReplace of info: GlyphInfo * props: GlyphProps
    | LigatureUnsafeToConcat of concatTo: int
    | LigatureLigate of data: LigateFacts * lastLigId: byte * lastNumComponents: int * componentsSoFar: int

/// <summary>What one GSUB lookup does at the glyph it stopped on, the kind of a <see cref="SubstOutcome"/>.</summary>
type internal SubstKind =
    | Miss = 0
    | Delete = 1
    | Replace = 2
    | AlternateRandom = 3
    | Multiple = 4
    | Ligature = 5

/// <summary>The outcome of trying a lookup's subtables on a glyph: what to do, and the few values that go with it. A
/// multiple substitution names its sequence by subtable and coverage index, a ligature substitution its ligature set the
/// same way, so the outcome stays small.</summary>
[<Struct>]
type internal SubstOutcome =
    val Kind: SubstKind
    val Subtable: int
    val Index: int
    val Glyph: uint32
    val Props: GlyphProps
    val RandomState: uint32

    /// <summary>Creates an outcome.</summary>
    /// <param name="kind">What to do.</param>
    /// <param name="subtable">The subtable that matched.</param>
    /// <param name="index">The coverage index of the glyph in it.</param>
    /// <param name="glyph">The replacement glyph.</param>
    /// <param name="props">The replacement properties.</param>
    /// <param name="randomState">The random state to adopt.</param>
    new (kind, subtable, index, glyph, props, randomState) =
        { Kind = kind; Subtable = subtable; Index = index; Glyph = glyph; Props = props; RandomState = randomState }

/// <summary>The unsafe-to-concat ranges the failed rules of a context lookup owe, merged as union: the plain ranges
/// of <c>unsafe_to_concat</c> start at the current glyph, those of <c>unsafe_to_concat_from_outbuffer</c> start in the
/// output. A range longer than <see cref="FlagBounds.MaxRange"/> is dropped when it is added, as
/// <c>_set_glyph_flags</c> drops it, so the union is exactly what the separate calls would mark.</summary>
[<Struct>]
type internal FlagBounds =
    { ConcatTo: int // the end of the plain range from the current glyph, or -1
      OutStart: int // the start of the output-buffer range, or -1
      OutEnd: int } // the end of the output-buffer range

/// <summary>Operations on <see cref="FlagBounds"/>.</summary>
module internal FlagBounds =
    /// <summary>The longest range <c>_set_glyph_flags</c> marks, <c>HB_BUFFER_MAX_GLYPH_FLAGS_RANGE</c>.</summary>
    [<Literal>]
    let MaxRange = 255

    /// <summary>No range owed.</summary>
    let none = { ConcatTo = -1; OutStart = -1; OutEnd = -1 }

    /// <summary>Whether no range is owed.</summary>
    /// <param name="bounds">The bounds.</param>
    /// <returns>True when none is owed.</returns>
    let isNone (bounds: FlagBounds) = bounds.ConcatTo < 0 && bounds.OutStart < 0

    /// <summary>Adds the range <c>unsafe_to_concat (start, finish)</c>.</summary>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="bounds">The bounds so far.</param>
    /// <returns>The bounds with the range merged in, or unchanged when the range is too long.</returns>
    let addPlain start finish (bounds: FlagBounds) =
        match uint32 finish - uint32 start > uint32 MaxRange with
        | true -> bounds
        | false -> { bounds with ConcatTo = max bounds.ConcatTo finish }

    /// <summary>Adds the range <c>unsafe_to_concat_from_outbuffer (start, finish)</c>.</summary>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="bounds">The bounds so far.</param>
    /// <returns>The bounds with the range merged in, or unchanged when the range is too long.</returns>
    let addOutbuffer start finish (bounds: FlagBounds) =
        match uint32 finish - uint32 start > uint32 MaxRange with
        | true -> bounds
        | false ->
            match bounds.OutStart with
            | -1 -> { bounds with OutStart = start; OutEnd = finish }
            | outStart -> { bounds with OutStart = min outStart start; OutEnd = max bounds.OutEnd finish }

    /// <summary>Merges the ranges of two bounds.</summary>
    /// <param name="first">The first bounds.</param>
    /// <param name="second">The second bounds.</param>
    /// <returns>The union.</returns>
    let merge (first: FlagBounds) (second: FlagBounds) =
        let plain = { first with ConcatTo = max first.ConcatTo second.ConcatTo }
        match second.OutStart with
        | -1 -> plain
        | outStart ->
            match plain.OutStart with
            | -1 -> { plain with OutStart = outStart; OutEnd = second.OutEnd }
            | own -> { plain with OutStart = min own outStart; OutEnd = max plain.OutEnd second.OutEnd }

/// <summary>What a context or chained-context lookup does at the glyph it stopped on: the unsafe-to-concat ranges the
/// subtables and rules that failed before the deciding one owe, and, when a rule matched, its match.</summary>
[<Struct; NoComparison; NoEquality>]
type internal ContextOutcome =
    val Matched: bool
    val Chain: bool // a chained rule: the break range is taken from the output buffer
    val InputCount: int // the rule's own count, including the first glyph, which may be 0
    val MatchEnd: int
    val Records: ImmutableArray<LookupRecord>
    val StartIndex: int
    val EndIndex: int
    val Owed: FlagBounds // ranges to mark before the match is applied
    val AfterConcat: int // the fast path's range from the glyph the match ended on, marked after the match, or -1

    /// <summary>Creates an outcome.</summary>
    /// <param name="matched">Whether a rule matched.</param>
    /// <param name="chain">Whether the rule is a chained rule.</param>
    /// <param name="inputCount">The rule's input count.</param>
    /// <param name="matchEnd">The end of the input match.</param>
    /// <param name="records">The rule's lookup records.</param>
    /// <param name="startIndex">The backtrack start, for a chained rule.</param>
    /// <param name="endIndex">The lookahead end, for a chained rule.</param>
    /// <param name="owed">The ranges owed before the match.</param>
    /// <param name="afterConcat">The range owed after the match.</param>
    new (matched, chain, inputCount, matchEnd, records, startIndex, endIndex, owed, afterConcat) =
        { Matched = matched; Chain = chain; InputCount = inputCount; MatchEnd = matchEnd; Records = records
          StartIndex = startIndex; EndIndex = endIndex; Owed = owed; AfterConcat = afterConcat }

/// <summary>The outcome of the reverse-chaining subtables of a lookup on a glyph: a replacement with the backtrack and
/// lookahead bounds, or none, each with the unsafe-to-concat ranges the subtables that failed before it owe.</summary>
[<Struct; NoComparison; NoEquality>]
type internal ReverseOutcome =
    | ReverseMiss of missOwed: FlagBounds
    | ReverseSubstitute of substitute: uint32 * startIndex: int * endIndex: int * substituteOwed: FlagBounds

/// <summary>How the head of a ligature-set walk decides, <c>LigatureSet::apply</c>'s probe of the second glyph.</summary>
type internal LigatureHeadKind =
    | Slow = 0
    | Fast = 1
    | NoSecond = 2

/// <summary>The result of probing the second glyph of a ligature set, with the number of ligatures the set holds.</summary>
[<Struct>]
type internal LigatureHead =
    val Kind: LigatureHeadKind
    val Bound: int
    val Second: uint32
    val Count: int

    /// <summary>Creates a probe result.</summary>
    /// <param name="kind">How the walk goes on.</param>
    /// <param name="bound">The end of the unsafe-to-concat range, or the fast path's unsafe-to bound.</param>
    /// <param name="second">The second glyph of the fast path.</param>
    /// <param name="count">The number of ligatures of the set.</param>
    new (kind, bound, second, count) = { Kind = kind; Bound = bound; Second = second; Count = count }

/// <summary>One GSUB lookup as the lookup kernel of <see cref="Sealed"/> sees it: the decisions, answered from the run's
/// arrays and never writing them.</summary>
type internal IGsubLookup =
    /// <summary>Finds the next glyph the lookup may match, <c>apply_forward</c>'s skip loop.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first candidate index.</param>
    /// <returns>The first matching index, or the run length.</returns>
    abstract Scan: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int -> int
    /// <summary>Tries the subtables on the glyph at an index, the first that matches deciding.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="randomState">The buffer random state.</param>
    /// <returns>The outcome.</returns>
    abstract Try: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int * randomState: uint32 -> SubstOutcome
    /// <summary>Tries the subtables after one on the glyph at an index, for the glyph a ligature subtable covered but no
    /// ligature of it applied to: <c>hb_ot_layout_lookup_accelerator_t::apply</c> goes on to the next subtable.</summary>
    /// <param name="subtable">The subtable that did not apply.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="randomState">The buffer random state.</param>
    /// <returns>The outcome.</returns>
    abstract TryAfter: subtable: int * infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int * randomState: uint32 -> SubstOutcome
    /// <summary>Tries the subtables of a context or chained-context lookup on the glyph at an index, filling the match
    /// positions of the rule that matches.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, for the backtrack and the ligature-base probe.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The buffer a match fills with its input positions, at most 64 entries.</param>
    /// <returns>The outcome.</returns>
    abstract TryContext: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * outInfos: ReadOnlySpan<GlyphInfo> * outProps: ReadOnlySpan<GlyphProps> * index: int * positions: Span<int> -> ContextOutcome
    /// <summary>Tries the subtables as <see cref="TryContext"/> does, passing over a subtable that provably cannot match without matching it:
    /// the ranges the failed subtables owe are not accumulated, so the caller asks only when the run does not produce unsafe-to-concat flags
    /// and the ranges are never marked. Whether a rule matches and which is the same as <see cref="TryContext"/> gives.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, for the backtrack and the ligature-base probe.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The buffer a match fills with its input positions, at most 64 entries.</param>
    /// <returns>The outcome, whose owed ranges are incomplete.</returns>
    abstract TryContextDropping: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * outInfos: ReadOnlySpan<GlyphInfo> * outProps: ReadOnlySpan<GlyphProps> * index: int * positions: Span<int> -> ContextOutcome
    /// <summary>Whether no subtable of a context or chained-context lookup can match at the glyph at an index, decided without building an
    /// outcome and without the unsafe-to-concat ranges the failed subtables owe: the caller asks only when the run does not produce them.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>True when every subtable fails.</returns>
    abstract CannotMatch: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * outInfos: ReadOnlySpan<GlyphInfo> * outProps: ReadOnlySpan<GlyphProps> * index: int -> bool
    /// <summary>Applies one reverse-chaining subtable at an index, <c>apply_backward</c>'s per-glyph step.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The outcome.</returns>
    abstract ReverseTry: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int -> ReverseOutcome
    /// <summary>Whether the glyph at an index passes the reverse lookup's digest, mask and property gate,
    /// <c>apply_backward</c>'s per-glyph check.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>True when the reverse subtable is tried on the glyph.</returns>
    abstract ReverseAdmits: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int -> bool
    /// <summary>The properties of a glyph replaced in place, <c>replace_glyph_inplace</c>'s class update.</summary>
    /// <param name="substitute">The substitute glyph.</param>
    /// <param name="props">The replaced glyph's properties.</param>
    /// <returns>The replacement properties.</returns>
    abstract ReverseReplace: substitute: uint32 * props: GlyphProps -> GlyphProps
    /// <summary>Whether the lookup is a context or chained-context lookup, tried through <see cref="TryContext"/>.</summary>
    /// <returns>True when the lookup holds a context or chained-context subtable.</returns>
    abstract HasContext: unit -> bool
    /// <summary>The settings a nested lookup inherits: the outer mask, random flag and automatic-joiner and per-syllable
    /// switches, the fields <c>dispatch_recurse_func</c> leaves unchanged.</summary>
    /// <returns>The settings.</returns>
    abstract Nest: unit -> struct (uint32 * bool * bool * bool * bool)
    /// <summary>The sequence of a multiple substitution.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="index">The coverage index.</param>
    /// <returns>The glyphs that replace the covered glyph.</returns>
    abstract MultipleComponents: subtable: int * index: int -> ImmutableArray<uint32>
    /// <summary>The properties of one component of a multiple substitution, <c>output_glyph_for_component</c>.</summary>
    /// <param name="baseProps">The replaced glyph's properties.</param>
    /// <param name="glyph">The component glyph.</param>
    /// <param name="index">The component index.</param>
    /// <returns>The component properties.</returns>
    abstract MultipleProps: baseProps: GlyphProps * glyph: uint32 * index: int -> GlyphProps
    /// <summary>Probes the second glyph of a ligature set, <c>LigatureSet::apply</c>'s head, and counts the set's ligatures: a set of at most
    /// one ligature is answered at once, as a slow walk, without the probe.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="set">The coverage index of the first glyph.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first glyph's index.</param>
    /// <returns>How the walk goes on and how many ligatures the set holds.</returns>
    abstract LigatureHead: subtable: int * set: int * infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int -> LigatureHead
    /// <summary>The first ligature of a set at or after an index that the fast path may try: it has no components after the first, or its
    /// second glyph is the probed one.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="set">The ligature set.</param>
    /// <param name="first">The index of the first ligature to consider.</param>
    /// <param name="second">The probed second glyph.</param>
    /// <returns>The ligature's index in the set, or the number of ligatures when none may be tried.</returns>
    abstract LigatureNext: subtable: int * set: int * first: int * second: uint32 -> int
    /// <summary>Applies one ligature of a set, <c>Ligature::apply</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="set">The ligature set.</param>
    /// <param name="ligature">The ligature's index in the set.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The first glyph's index.</param>
    /// <param name="serial">The buffer serial.</param>
    /// <returns>The outcome.</returns>
    abstract LigatureApply: subtable: int * set: int * ligature: int * infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * outInfos: ReadOnlySpan<GlyphInfo> * outProps: ReadOnlySpan<GlyphProps> * index: int * serial: byte -> LigatureTry
    /// <summary>Finds the next component of a ligation, <c>ligate_input</c>'s component walk.</summary>
    /// <param name="data">The ligature data.</param>
    /// <param name="componentIndex">The next component, one-based.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="matchStart">The previous component's match position.</param>
    /// <returns>The component's index, or -1 when there is none.</returns>
    abstract NextComponent: data: LigateFacts * componentIndex: int * infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * matchStart: int -> int

/// <summary>The lookups of a GSUB table, in the order <c>hb_ot_map_t::apply</c> applies them.</summary>
type internal IGsubProgram<'L when 'L : struct and 'L :> IGsubLookup> =
    /// <summary>The number of lookups the stages apply.</summary>
    abstract LookupEnd: int
    /// <summary>For each glyph, a bit per position of the first 64 lookups whose subtables cover it, or empty when the
    /// program has no such table.</summary>
    abstract GlyphLookups: ImmutableArray<uint64>
    /// <summary>Whether a lookup is applied to the run as it stands: its mask is set, it is not reversed, the digest
    /// gate and, when the run would show it, the covered-glyph gate let it through.</summary>
    /// <param name="index">The lookup's position in the map.</param>
    /// <param name="digest">The digest of the run.</param>
    /// <param name="candidates">The lookups of the first 64 positions that cover some glyph of the run, a bit each, all set
    /// when <see cref="GlyphLookups"/> says nothing.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run, which a run
    /// whose arrays differ from what a lookup publishes does.</param>
    /// <returns>True when the lookup is applied.</returns>
    abstract Admits: index: int * digest: Digest * candidates: uint64 * flags: BufferFlags * infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * shown: bool -> bool
    /// <summary>The lookup at a position of the map.</summary>
    /// <param name="index">The lookup's position in the map.</param>
    /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run.</param>
    /// <returns>The lookup.</returns>
    abstract Lookup: index: int * shown: bool -> 'L
    /// <summary>Whether the lookups of the program are kept ready, built once on first use and immutable, so that
    /// <see cref="LookupAt"/> hands out a copy of the one kept instead of <see cref="Lookup"/> building one.</summary>
    abstract Prepared: bool
    /// <summary>The lookup at a position of the map, as <see cref="Lookup"/> gives it, kept by the program and handed out as a copy:
    /// asked only of a program that is <see cref="Prepared"/>. It is never handed out by reference: a reference into the kept object,
    /// held while the lookup is applied and anything allocates, reads the wrong memory on browser-wasm compiled ahead of time
    /// (PITFALLS.md).</summary>
    /// <param name="index">The lookup's position in the map.</param>
    /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run.</param>
    /// <returns>A copy of the lookup kept.</returns>
    abstract LookupAt: index: int * shown: bool -> 'L
    /// <summary>Whether the lookup at a position of the map is a reverse lookup, applied backward in place.</summary>
    /// <param name="index">The lookup's position in the map.</param>
    /// <returns>True for a reverse lookup.</returns>
    abstract IsReverse: index: int -> bool
    /// <summary>The lookup at a table index, for a nested lookup: its own subtables and properties, under the outer mask
    /// and the outer automatic-joiner and per-syllable settings.</summary>
    /// <param name="lookupIndex">The lookup's index in the table.</param>
    /// <param name="mask">The outer lookup mask.</param>
    /// <param name="random">Whether the random feature is on.</param>
    /// <param name="autoZwnj">Whether ZWNJ is ignored automatically.</param>
    /// <param name="autoZwj">Whether ZWJ is ignored automatically.</param>
    /// <param name="perSyllable">Whether matching is per syllable.</param>
    /// <returns>The nested lookup.</returns>
    abstract NestedLookup: lookupIndex: int * mask: uint32 * random: bool * autoZwnj: bool * autoZwj: bool * perSyllable: bool -> 'L

/// <summary>What one GPOS lookup does at the glyph it stopped on: whether it applied, where the lookup goes on, the new
/// positions of the glyph and of the second glyph of a pair when they changed, and the unsafe-to ranges it records,
/// every range counted from the glyph it stopped on and zero for none. The unsafe-to-concat range is that of every
/// subtable that failed or applied as a zero-valued pair, which the kernel honours only when the run produces such
/// flags; the two unsafe-to-break ranges are set in order.</summary>
[<Struct; NoComparison; NoEquality>]
type internal PosOutcome =
    val Applied: bool
    val Next: int
    val First: GlyphPosition
    val FirstWorked: bool
    val SecondIndex: int
    val Second: GlyphPosition
    val SecondWorked: bool
    val ConcatTo: int
    val Break1To: int
    val Break2To: int
    val Advance: int // when non-zero, the glyph's horizontal advance grows by it, saturating, instead of First

    /// <summary>Creates an outcome.</summary>
    /// <param name="applied">Whether the lookup applied.</param>
    /// <param name="next">The index the lookup goes on from when it applied.</param>
    /// <param name="first">The glyph's new position.</param>
    /// <param name="firstWorked">Whether the glyph's position changed.</param>
    /// <param name="secondIndex">The index of the second glyph of a pair.</param>
    /// <param name="second">The second glyph's new position.</param>
    /// <param name="secondWorked">Whether the second glyph's position changed.</param>
    /// <param name="concatTo">The end of the unsafe-to-concat range.</param>
    /// <param name="break1To">The end of the first unsafe-to-break range.</param>
    /// <param name="break2To">The end of the second unsafe-to-break range.</param>
    /// <param name="advance">The horizontal advance to add to the glyph, zero for none.</param>
    new (applied, next, first, firstWorked, secondIndex, second, secondWorked, concatTo, break1To, break2To, advance) =
        { Advance = advance
          Applied = applied
          Next = next
          First = first
          FirstWorked = firstWorked
          SecondIndex = secondIndex
          Second = second
          SecondWorked = secondWorked
          ConcatTo = concatTo
          Break1To = break1To
          Break2To = break2To }

    /// <summary>Creates an outcome with no horizontal advance to add.</summary>
    /// <param name="applied">Whether the lookup applied.</param>
    /// <param name="next">The index the lookup goes on from when it applied.</param>
    /// <param name="first">The glyph's new position.</param>
    /// <param name="firstWorked">Whether the glyph's position changed.</param>
    /// <param name="secondIndex">The index of the second glyph of a pair.</param>
    /// <param name="second">The second glyph's new position.</param>
    /// <param name="secondWorked">Whether the second glyph's position changed.</param>
    /// <param name="concatTo">The end of the unsafe-to-concat range.</param>
    /// <param name="break1To">The end of the first unsafe-to-break range.</param>
    /// <param name="break2To">The end of the second unsafe-to-break range.</param>
    new (applied, next, first, firstWorked, secondIndex, second, secondWorked, concatTo, break1To, break2To) =
        PosOutcome(applied, next, first, firstWorked, secondIndex, second, secondWorked, concatTo, break1To, break2To, 0)

    /// <summary>Creates the outcome of a pair whose only effect is a horizontal advance added to the first glyph.</summary>
    /// <param name="next">The index the lookup goes on from.</param>
    /// <param name="secondIndex">The index of the second glyph.</param>
    /// <param name="advance">The scaled advance, not zero.</param>
    /// <param name="concatTo">The end of the unsafe-to-concat range.</param>
    /// <param name="break1To">The end of the first unsafe-to-break range.</param>
    new (next, secondIndex, advance, concatTo, break1To) =
        { Advance = advance
          Applied = true
          Next = next
          First = Unchecked.defaultof<GlyphPosition>
          FirstWorked = false
          SecondIndex = secondIndex
          Second = Unchecked.defaultof<GlyphPosition>
          SecondWorked = false
          ConcatTo = concatTo
          Break1To = break1To
          Break2To = 0 }

/// <summary>The kinds of effect an attachment has on the run.</summary>
[<RequireQualifiedAccess>]
module internal AttachKind =
    /// <summary>The subtable did not apply: the only effect is an unsafe-to-concat range.</summary>
    [<Literal>]
    let Nothing = 0

    /// <summary>A mark attached to the glyph it found, <c>apply_mark_attachment</c>.</summary>
    [<Literal>]
    let Mark = 1

    /// <summary>A glyph attached to the previous one, <c>apply_cursive_attachment</c>.</summary>
    [<Literal>]
    let Cursive = 2

/// <summary>What one GPOS attachment lookup (cursive, mark-to-base, mark-to-ligature, mark-to-mark) does at the glyph it
/// stopped on, after trying its subtables in order: whether it applied, where the lookup goes on, and what the kernel writes,
/// that being the mark's new position and the unsafe-to ranges of a mark attachment, the glyph and the anchors a cursive
/// attachment is made of, and the unsafe-to-concat range of the subtables that failed; with the lookup's cache of the last
/// base it found, <c>last_base</c> and <c>last_base_until</c>, as the subtables left it.</summary>
[<Struct; NoComparison; NoEquality>]
type internal AttachOutcome =
    val Applied: bool
    val Kind: int
    val Next: int
    val Position: GlyphPosition // a mark attachment: the mark's position, with its attach chain and type
    val Attached: bool // a mark attachment: the chain fits, so the buffer has a GPOS attachment
    val BreakFrom: int // a mark attachment: unsafe_to_break (BreakFrom, BreakTo), none when BreakTo is zero
    val BreakTo: int
    val ConcatFrom: int // unsafe_to_concat_from_outbuffer (ConcatFrom, index + 1), none when negative
    val Partner: int // a cursive attachment: the index of the previous glyph
    val ExitX: float32
    val ExitY: float32
    val EntryX: float32
    val EntryY: float32
    val Direction: int // a cursive attachment: the run's direction, <c>hb_direction_t</c>
    val RightToLeft: bool // a cursive attachment: the lookup's RightToLeft flag
    val LastBase: int
    val LastBaseUntil: int

    /// <summary>Creates an outcome.</summary>
    /// <param name="applied">Whether a subtable applied.</param>
    /// <param name="kind">The kind of effect, <see cref="AttachKind"/>.</param>
    /// <param name="next">The index the lookup goes on from when it applied.</param>
    /// <param name="position">The mark's new position.</param>
    /// <param name="attached">Whether the attachment holds.</param>
    /// <param name="breakFrom">The start of the unsafe-to-break range.</param>
    /// <param name="breakTo">The end of the unsafe-to-break range, zero for none.</param>
    /// <param name="concatFrom">The start of the unsafe-to-concat range, negative for none.</param>
    /// <param name="partner">The previous glyph of a cursive attachment.</param>
    /// <param name="exitX">The previous glyph's exit anchor, x.</param>
    /// <param name="exitY">The previous glyph's exit anchor, y.</param>
    /// <param name="entryX">The glyph's entry anchor, x.</param>
    /// <param name="entryY">The glyph's entry anchor, y.</param>
    /// <param name="direction">The run's direction.</param>
    /// <param name="rightToLeft">Whether the lookup has the RightToLeft flag.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    new (applied, kind, next, position, attached, breakFrom, breakTo, concatFrom, partner, exitX, exitY, entryX, entryY, direction, rightToLeft, lastBase, lastBaseUntil) =
        { Applied = applied
          Kind = kind
          Next = next
          Position = position
          Attached = attached
          BreakFrom = breakFrom
          BreakTo = breakTo
          ConcatFrom = concatFrom
          Partner = partner
          ExitX = exitX
          ExitY = exitY
          EntryX = entryX
          EntryY = entryY
          Direction = direction
          RightToLeft = rightToLeft
          LastBase = lastBase
          LastBaseUntil = lastBaseUntil }

/// <summary>What the pair loops of the positioning kernel do at one glyph of a lookup whose pairs they classify without applying
/// them, packed into one 64-bit value so that building and testing one costs no call: in the low bits whether the pair is unsafe to
/// break, whether the attempt is an unsafe-to-concat hazard, and whether the pair is complex, one whose value records the
/// classifier does not decode, which the kernel applies as the general loop does; above them the horizontal advance the glyph grows
/// by, saturating. A glyph with none of the three flags is nothing.</summary>
[<RequireQualifiedAccess>]
module internal PairStep =
    /// <summary>The flag of a pair that is unsafe to break.</summary>
    [<Literal>]
    let Broken = 1L

    /// <summary>The flag of an attempt that is an unsafe-to-concat hazard, which the kernel honours only on request.</summary>
    [<Literal>]
    let Hazard = 2L

    /// <summary>The flag of a pair the general loop applies.</summary>
    [<Literal>]
    let Complex = 4L

    /// <summary>The number of bits the advance is shifted by.</summary>
    [<Literal>]
    let Shift = 8

    /// <summary>The step of a glyph with nothing to do.</summary>
    [<Literal>]
    let Nothing = 0L

    /// <summary>Packs a step.</summary>
    /// <param name="advance">The advance to add.</param>
    /// <param name="flags">The flags.</param>
    /// <returns>The step.</returns>
    let inline make (advance: int) (flags: int64) =
        (int64 advance <<< Shift) ||| flags

    /// <summary>The advance of a step.</summary>
    /// <param name="step">The step.</param>
    /// <returns>The advance to add.</returns>
    let inline advance (step: int64) =
        int (step >>> Shift)

    /// <summary>Whether a step has something to do: the pair is unsafe to break, or a hazard, or complex.</summary>
    /// <param name="step">The step.</param>
    /// <returns>True when the step is not nothing.</returns>
    let inline acts (step: int64) =
        step &&& (Broken ||| Hazard ||| Complex) <> 0L

    /// <summary>Whether a step is a complex pair.</summary>
    /// <param name="step">The step.</param>
    /// <returns>True when the general loop applies the pair.</returns>
    let inline isComplex (step: int64) =
        step &&& Complex <> 0L

    /// <summary>Whether a step is a pair unsafe to break.</summary>
    /// <param name="step">The step.</param>
    /// <returns>True when the pair is unsafe to break.</returns>
    let inline isBroken (step: int64) =
        step &&& Broken <> 0L

    /// <summary>Whether a step is an unsafe-to-concat hazard.</summary>
    /// <param name="step">The step.</param>
    /// <returns>True when the attempt is a hazard.</returns>
    let inline isHazard (step: int64) =
        step &&& Hazard <> 0L

    /// <summary>The number of bits the gap is shifted by.</summary>
    [<Literal>]
    let GapShift = 3

    /// <summary>The largest gap a step holds, five bits.</summary>
    [<Literal>]
    let MaxGap = 31

    /// <summary>A step with the number of glyphs the second glyph of its pair lies beyond the glyph right after the first, glyphs the
    /// lookup's flag skips.</summary>
    /// <param name="step">The step.</param>
    /// <param name="gap">The number of skipped glyphs between the two, at most <see cref="MaxGap"/>.</param>
    /// <returns>The step with the gap.</returns>
    let inline withGap (step: int64) (gap: int) =
        step ||| (int64 gap <<< GapShift)

    /// <summary>The number of skipped glyphs between the two glyphs of a pair.</summary>
    /// <param name="step">The step.</param>
    /// <returns>The gap.</returns>
    let inline gap (step: int64) =
        int ((step >>> GapShift) &&& int64 MaxGap)

/// <summary>One pair-adjustment subtable compiled into flat arrays for the pair loops of <see cref="Sealed"/>, so that a pair
/// is found by one hash probe (format 1) or three array reads (format 2) and no structure is unpacked. A subtable the arrays
/// cannot represent exactly, or that is no pair adjustment, is of the general kind and holds nothing: the pair loops leave
/// every pair it covers to the general loop.</summary>
[<NoComparison; NoEquality>]
type internal PairTable =
    { Kind: int
      Plain: bool // the records are one horizontal advance for the first glyph and nothing else
      Cover: ImmutableArray<uint16> // format 1: the coverage's direct table; format 2: the class of each covered first glyph, or one of the two markers
      Keys: ImmutableArray<uint32> // format 1: the open-addressed pair keys
      Slots: ImmutableArray<int> // format 1: the record number of each key
      Shift: int // format 1: the shift of the hash
      Filter: ImmutableArray<uint64> // format 1: one bit for each hash of a key, so a pair that is none of them is told without a probe
      FilterShift: int // format 1: the shift that leaves as many bits as the filter has
      Columns: ImmutableArray<uint16> // format 2: the class of each second glyph below the class count, or the marker for out of range
      BeyondColumn: uint16 // format 2: the column of a glyph past the end of the columns
      Width: int // format 2: the number of columns
      Values: ImmutableArray<int16> // the raw fields of the records; empty in a table whose records are resolved
      Totals: ImmutableArray<int64> } // resolved tables: for each record the scaled advance, devices included, shifted left once, the lowest bit set when the record worked

/// <summary>What a set of pair tables compiled for a font's coordinates depends on: the normalised coordinates and the horizontal scale.
/// Two keys are equal when their coordinates are, value by value, and their scales are.</summary>
[<Struct; CustomEquality; NoComparison>]
type internal ResolveKey =
    { Coords: ImmutableArray<int>
      XScale: int }

    /// <summary>Whether another object is a key of the same scale and the same coordinates, value by value.</summary>
    /// <param name="other">The other object.</param>
    /// <returns>True when it is an equal key.</returns>
    override this.Equals other =
        match other with
        | :? ResolveKey as key -> this.XScale = key.XScale && this.Coords.AsSpan().SequenceEqual(key.Coords.AsSpan())
        | _ -> false

    /// <summary>A hash of the scale, which the coordinates of a font rarely change under.</summary>
    /// <returns>The hash.</returns>
    override this.GetHashCode () =
        this.XScale

    interface IEquatable<ResolveKey> with
        member this.Equals key = this.XScale = key.XScale && this.Coords.AsSpan().SequenceEqual(key.Coords.AsSpan())

/// <summary>One GPOS lookup as the positioning kernel of <see cref="Sealed"/> sees it: the decisions, answered from the
/// run's arrays and never writing them.</summary>
type internal IPosLookup =
    /// <summary>Whether the pair loop may apply the lookup: its subtables are all pair adjustments, the run is horizontal and the
    /// glyphs it can start at are known exactly. The lookup's flags may filter glyphs out.</summary>
    abstract Pairs: bool
    /// <summary>Classifies the pair the glyph at an index starts, the answer <c>apply_forward</c> and the pair subtables
    /// give in the common case where the next glyph is the partner. A glyph the lookup does not start at, or one whose
    /// mask the lookup does not cover, is nothing; a partner that is default ignorable or whose mask the lookup does not
    /// cover is complex, so the skipping iterator decides.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    /// <returns>The step, <see cref="PairStep"/>.</returns>
    abstract Pair: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int * hazard: bool -> int64
    /// <summary>Finds the next glyph the lookup may match, <c>apply_forward</c>'s skip loop.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first candidate index.</param>
    /// <returns>The first matching index, or the run length.</returns>
    abstract Scan: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int -> int
    /// <summary>Tries the subtables on the glyph at an index, the first that applies deciding.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions as they stand.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The outcome.</returns>
    abstract Try: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * positions: ReadOnlySpan<GlyphPosition> * index: int -> PosOutcome
    /// <summary>Whether the lookup attaches glyphs, being one of cursive, mark-to-base, mark-to-ligature or mark-to-mark
    /// attachment: the kernel applies it through <see cref="Attach"/>, with the cache of the last base.</summary>
    abstract Attaches: bool
    /// <summary>Tries the subtables of an attachment lookup on the glyph at an index, the first that applies deciding.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions as they stand.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The base the lookup found last, minus one for none.</param>
    /// <param name="lastBaseUntil">The index the lookup has searched for a base down to.</param>
    /// <returns>The outcome.</returns>
    abstract Attach: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * positions: ReadOnlySpan<GlyphPosition> * index: int * lastBase: int * lastBaseUntil: int -> AttachOutcome
    /// <summary>Whether every visit of the lookup is bound to fail on a run: all its subtables are mark-to-base, mark-to-ligature or
    /// mark-to-mark attachments and no glyph of the run is one they attach to. A failed visit changes nothing but the unsafe-to-concat
    /// flags the buffer may ask for, so the kernel skips such a lookup unless it does.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <returns>True when no visit can attach.</returns>
    abstract Futile: infos: ReadOnlySpan<GlyphInfo> -> bool
    /// <summary>Whether the lookup holds context or chained-context subtables, which the kernel applies through
    /// <see cref="TryContext"/> and the nested lookups of the rule that matches.</summary>
    abstract HasContext: bool
    /// <summary>Tries the context and chained-context subtables of a lookup on the glyph at an index in order, filling the match
    /// positions of the rule that matches. The run is positioned in place, so the glyphs before the index are the backtrack.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The buffer a match fills with its input positions, at most 64 entries.</param>
    /// <returns>The outcome.</returns>
    abstract TryContext: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int * positions: Span<int> -> ContextOutcome
    /// <summary>The settings a nested lookup inherits: the outer mask and the automatic-joiner switches, the fields
    /// <c>dispatch_recurse_func</c> leaves unchanged.</summary>
    /// <returns>The mask, whether ZWNJ is ignored automatically and whether ZWJ is.</returns>
    abstract Nest: unit -> struct (uint32 * bool * bool)
    /// <summary>Whether the lookup attaches through dense tables only: every subtable is a compiled mark-to-base or mark-to-mark
    /// attachment, so the kernel's attachment loop decides each visit from the answers below and builds no outcome.</summary>
    abstract Dense: bool
    /// <summary>The number of subtables of a dense lookup.</summary>
    abstract DenseCount: int
    /// <summary>Whether the run is horizontal.</summary>
    abstract DenseHorizontal: bool
    /// <summary>The index of a glyph in the mark coverage of a subtable, minus one for a glyph it does not cover.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The index.</returns>
    abstract MarkOf: subtable: int * glyph: uint32 -> int
    /// <summary>Whether a subtable attaches marks to marks.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>True for a mark-to-mark subtable.</returns>
    abstract OnMarks: subtable: int -> bool
    /// <summary>Whether a glyph is a candidate base for a mark-to-base search: the skipping iterator, which ignores marks, matches it.</summary>
    /// <param name="info">The glyph.</param>
    /// <param name="props">Its properties.</param>
    /// <returns>True when the iterator matches the glyph.</returns>
    abstract BaseMatches: info: GlyphInfo * props: GlyphProps -> bool
    /// <summary>Finds the glyph before an index the lookup's own skipping iterator matches, for a mark-to-mark attachment.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The index of the glyph when it is not negative, else the complement of the unsafe-to-concat bound of the miss.</returns>
    abstract PreviousMark: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int -> int
    /// <summary>The row of a glyph in the target coverage of a subtable, minus one for a glyph it does not cover.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The row.</returns>
    abstract RowOf: subtable: int * glyph: uint32 -> int
    /// <summary>The cell of the anchor matrix a mark of a row reads, minus one when the subtable has no anchor there.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="row">The row of the glyph the mark attaches to.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <returns>The cell.</returns>
    abstract AnchorCell: subtable: int * row: int * markIndex: int -> int
    /// <summary>The x offset of a mark attached through a cell, the rounded difference of the two anchors.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="cell">The cell.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <returns>The offset, saturated.</returns>
    abstract AnchorOffsetX: subtable: int * cell: int * markIndex: int -> int
    /// <summary>The y offset of a mark attached through a cell, the rounded difference of the two anchors.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="cell">The cell.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <returns>The offset, saturated.</returns>
    abstract AnchorOffsetY: subtable: int * cell: int * markIndex: int -> int

/// <summary>The lookups of a GPOS table, in the order <c>hb_ot_map_t::apply</c> applies them.</summary>
type internal IPosProgram<'L when 'L : struct and 'L :> IPosLookup> =
    /// <summary>The number of lookups the stages apply.</summary>
    abstract LookupEnd: int
    /// <summary>For each glyph, a bit per position of the first 64 lookups whose subtables cover it, or empty when the
    /// program has no such table.</summary>
    abstract GlyphLookups: ImmutableArray<uint64>
    /// <summary>Whether a lookup is applied to the run: its mask is set, the glyphs of the run reach it, and the second
    /// glyph gate of <c>hb_ot_map_t::apply</c> lets it through.</summary>
    /// <param name="index">The lookup's position in the map.</param>
    /// <param name="digest">The digest of the run's glyphs.</param>
    /// <param name="candidates">The lookups of the first 64 positions that cover some glyph of the run, a bit each, all set
    /// when <see cref="GlyphLookups"/> says nothing.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <returns>True when the lookup is applied.</returns>
    abstract Admits: index: int * digest: Digest * candidates: uint64 * flags: BufferFlags -> bool
    /// <summary>The lookup at a position of the map.</summary>
    /// <param name="index">The lookup's position in the map.</param>
    /// <returns>The lookup.</returns>
    abstract Lookup: index: int -> 'L
    /// <summary>The lookup at a table index, for a nested lookup: its own subtables and properties, under the outer mask and
    /// the outer automatic-joiner switches, or one with no subtables for an index past the table.</summary>
    /// <param name="lookupIndex">The lookup's index in the table.</param>
    /// <param name="mask">The outer lookup mask.</param>
    /// <param name="autoZwnj">Whether ZWNJ is ignored automatically.</param>
    /// <param name="autoZwj">Whether ZWJ is ignored automatically.</param>
    /// <returns>The nested lookup.</returns>
    abstract NestedLookup: lookupIndex: int * mask: uint32 * autoZwnj: bool * autoZwj: bool -> 'L


/// <summary>
/// The table reads of one AAT state-table pass, <c>StateTableDriver::drive</c>'s class and entry lookups and its gates,
/// implemented by a struct so that the JIT specialises <see cref="Sealed.runAatPass"/> for each machine. Every member is pure.
/// </summary>
type internal IAatTables<'E when 'E : struct> =
    /// <summary><c>driver_context_t::in_place</c>: true when the machine never uses the out-buffer.</summary>
    abstract InPlace: bool
    /// <summary>The number of ranges of the chain; the range walk runs only above one.</summary>
    abstract RangeCount: int
    /// <summary>Finds the range covering a cluster, moving down then up from the current one.</summary>
    /// <param name="index">The current range index.</param>
    /// <param name="cluster">The glyph cluster.</param>
    /// <returns>The range index.</returns>
    abstract AdvanceRange: index: int * cluster: uint32 -> int
    /// <summary>Whether the subtable applies in a range.</summary>
    /// <param name="index">The range index.</param>
    /// <returns>True when the range carries a subtable flag.</returns>
    abstract RangeEnabled: index: int -> bool
    /// <summary>The class of a glyph, the deleted-glyph and out-of-bounds classes included.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The class.</returns>
    abstract ClassOf: glyph: uint32 -> int
    /// <summary>The entry of a state and a class.</summary>
    /// <param name="state">The state.</param>
    /// <param name="klass">The class.</param>
    /// <returns>The entry.</returns>
    abstract EntryAt: state: int * klass: int -> 'E
    /// <summary>The state an entry leads to.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The state number.</returns>
    abstract NewState: entry: 'E -> int
    /// <summary>Whether an entry is an epsilon transition, <c>Flags::DontAdvance</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the cursor does not advance.</returns>
    abstract DontAdvance: entry: 'E -> bool
    /// <summary>Whether breaking before the current glyph is safe, <c>drive</c>'s safe-to-break test.</summary>
    /// <param name="state">The current state.</param>
    /// <param name="klass">The class.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="nextState">The next state.</param>
    /// <returns>True when safe to break.</returns>
    abstract SafeToBreak: state: int * klass: int * entry: 'E * nextState: int -> bool
    /// <summary>Counts the glyphs from an index whose class stays the given class, the fast path's stretch.</summary>
    /// <param name="infos">The input glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="klass">The class the stretch stays on.</param>
    /// <returns>The first index whose class differs, or the length.</returns>
    abstract StretchEnd: infos: ReadOnlySpan<GlyphInfo> * index: int * klass: int -> int
    /// <summary>Counts the glyphs from an index, in the start state, that no class of the table can move: each stays in the
    /// start state, safely, with no action and no mark flag, so that a stretch of them is skipped at once whatever their
    /// classes. A table that cannot tell answers the index itself.</summary>
    /// <param name="infos">The input glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The first index whose glyph the start state may react to, or the length.</returns>
    abstract QuietEnd: infos: ReadOnlySpan<GlyphInfo> * index: int -> int

/// <summary>
/// The table reads and the callbacks of one AAT subtable kind that describes its transition as edits, one micro-step at a
/// time. Every member is pure: the kernel owns the working copy and applies the edits the transition describes.
/// </summary>
type internal IAatMachine<'E, 'C when 'E : struct and 'C : struct> =
    inherit IAatTables<'E>
    /// <summary>Whether the transition of an entry can only update the context, so that the null transition may stand
    /// in for it.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry is quiet.</returns>
    abstract Quiet: entry: 'E -> bool
    /// <summary>Whether the null transition of an entry leaves the context as it is.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the null transition changes nothing.</returns>
    abstract NullIsIdentity: entry: 'E -> bool
    /// <summary>The context update of a null transition, applied to each glyph of a stretch.</summary>
    /// <param name="cursor">The cursor at the glyph.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The context.</param>
    /// <returns>The context.</returns>
    abstract NullTransition: cursor: inref<RunCursor> * entry: 'E * context: 'C -> 'C
    /// <summary>The next edit of the transition, one micro-step at a time.</summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The context.</param>
    /// <returns>Whether the step emits an edit (false: the transition is complete), the edit and the context.</returns>
    abstract Transition: cursor: inref<RunCursor> * entry: 'E * context: 'C -> struct (bool * RunEdit * 'C)

/// <summary>
/// The decisions of a contextual substitution subtable, <c>ContextualSubtable::driver_context_t::transition</c>: which
/// glyphs an entry replaces and what properties they take, pure, so that <see cref="Sealed.runContextualPass"/> can apply
/// the replacements on the working copy directly.
/// </summary>
type internal IContextualDecisions<'E when 'E : struct> =
    /// <summary>Whether the glyphs written are collected for the buffer glyph set.</summary>
    abstract Collect: bool
    /// <summary>Whether the table is an extended one, whose entries that are not actionable change nothing.</summary>
    abstract Extended: bool
    /// <summary><c>is_actionable</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry performs an action.</returns>
    abstract Actionable: entry: 'E -> bool
    /// <summary>Whether the entry sets the mark, <c>Flags::SetMark</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the mark moves to the current glyph.</returns>
    abstract SetsMark: entry: 'E -> bool
    /// <summary>The substitution index of the marked glyph.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The index.</returns>
    abstract MarkIndex: entry: 'E -> uint16
    /// <summary>The substitution index of the current glyph.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The index.</returns>
    abstract CurrentIndex: entry: 'E -> uint16
    /// <summary>The glyph a substitution table gives a glyph.</summary>
    /// <param name="index">The substitution index.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>Whether there is a replacement, and the replacement.</returns>
    abstract Replacement: index: uint16 * glyph: uint32 -> struct (bool * uint32)
    /// <summary>The properties a written glyph takes, <c>replace_glyph_inplace</c>'s property logic.</summary>
    /// <param name="glyph">The written glyph.</param>
    /// <param name="props">The properties it had.</param>
    /// <returns>The properties it takes.</returns>
    abstract Props: glyph: uint32 * props: GlyphProps -> GlyphProps

/// <summary>
/// The decisions of one kern machine, <c>hb_kern_machine_t::kern</c>: which glyph pair kerns and by how much, and what the
/// kerning does to the two positions, implemented by a struct so that the JIT specialises <see cref="Sealed.runKernPass"/>.
/// </summary>
type internal IKernMachine =
    /// <summary>Whether the subtable kerns across the stream, which moves the second glyph only.</summary>
    abstract CrossStream: bool
    /// <summary>Looks at the glyph at an index: skips it, skips the glyphs the matcher ignores, or finds the pair it kerns with.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph.</param>
    /// <returns>Where the scan resumes when nothing kerns; else the index of the second glyph and the kerning, which is not zero.</returns>
    abstract Scan: infos: ReadOnlySpan<GlyphInfo> * props: ReadOnlySpan<GlyphProps> * index: int -> struct (int * int * int)
    /// <summary>The positions of the two glyphs after the kerning.</summary>
    /// <param name="kern">The kerning, in font units.</param>
    /// <param name="first">The first glyph's position.</param>
    /// <param name="second">The second glyph's position.</param>
    /// <returns>The two positions; across the stream the first comes back as it was.</returns>
    abstract Apply: kern: int * first: GlyphPosition * second: GlyphPosition -> struct (GlyphPosition * GlyphPosition)

/// <summary>
/// A sorted pair table of kerning values compiled with the face: a bit per left glyph that starts some pair, and an open
/// addressing table of the pairs, so that a lookup is one filter test and a probe or two instead of a binary search.
/// </summary>
[<Struct; NoComparison; NoEquality>]
type internal PairIndex =
    { Lefts: ImmutableArray<uint64> // bit g set when glyph g is the left glyph of some pair
      Slots: ImmutableArray<uint64> // empty 0, else the occupied bit, the pair key and its value
      ShiftBits: int } // 32 less the log2 of the slot count

/// <summary>
/// The decisions of a ligature subtable, <c>LigatureSubtable::driver_context_t::transition</c>: the entry flags, the action
/// list and the component and ligature reads, pure, so that <see cref="Sealed.runLigaturePass"/> can run the action list
/// straight on the working copy.
/// </summary>
type internal ILigatureDecisions<'E when 'E : struct> =
    /// <summary>Whether the glyphs written are collected for the buffer glyph set.</summary>
    abstract Collect: bool
    /// <summary>Whether an entry performs its action list, <c>LigatureEntryT::performAction</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry runs the action list.</returns>
    abstract Performs: entry: 'E -> bool
    /// <summary>Whether an entry pushes the current glyph onto the component stack, <c>Flags::SetComponent</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the glyph is pushed.</returns>
    abstract SetsComponent: entry: 'E -> bool
    /// <summary>The byte offset of an entry's first action word.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The offset within the subtable.</returns>
    abstract ActionAt: entry: 'E -> int64
    /// <summary>Reads an action word.</summary>
    /// <param name="at">The byte offset.</param>
    /// <returns>Whether the word lies within the subtable, and the word.</returns>
    abstract ReadAction: at: int64 -> struct (bool * uint32)
    /// <summary>Reads the component word of a glyph for an action.</summary>
    /// <param name="glyph">The glyph at the cursor.</param>
    /// <param name="action">The action word, whose sign-extended offset moves the glyph.</param>
    /// <returns>Whether the word lies within the subtable, and the word.</returns>
    abstract ReadComponent: glyph: uint32 * action: uint32 -> struct (bool * uint32)
    /// <summary>Reads the ligature glyph at an index.</summary>
    /// <param name="index">The ligature index.</param>
    /// <returns>Whether the glyph lies within the subtable, and the glyph.</returns>
    abstract ReadLigature: index: uint32 -> struct (bool * uint32)
    /// <summary>The properties a written glyph takes, <c>replace_glyph</c>'s property logic.</summary>
    /// <param name="glyph">The written glyph.</param>
    /// <param name="props">The properties the glyph at the cursor had.</param>
    /// <returns>The properties it takes.</returns>
    abstract Props: glyph: uint32 * props: GlyphProps -> GlyphProps

/// <summary>
/// The decisions of a rearrangement subtable, <c>RearrangementSubtable::driver_context_t::transition</c>: the marks and the verb of an
/// entry, pure, so that <see cref="Sealed.runRearrangementPass"/> can run the transition straight on the working copy.
/// </summary>
type internal IRearrangementDecisions<'E when 'E : struct> =
    /// <summary>The verb of an entry, zero for none.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The verb nibble.</returns>
    abstract Verb: entry: 'E -> int
    /// <summary>The two-nibble move a verb stands for: the glyphs moved from the left side in the high nibble and from the right in
    /// the low one, three meaning two glyphs flipped.</summary>
    /// <param name="verb">The verb nibble.</param>
    /// <returns>The mapped byte.</returns>
    abstract Moves: verb: int -> int
    /// <summary>Whether an entry starts the marked range at the glyph.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True for <c>MarkFirst</c>.</returns>
    abstract MarksFirst: entry: 'E -> bool
    /// <summary>Whether an entry ends the marked range at the glyph.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True for <c>MarkLast</c>.</returns>
    abstract MarksLast: entry: 'E -> bool

/// <summary>
/// The decisions of an insertion subtable, <c>InsertionSubtable::driver_context_t::transition</c>: the entry flags, the counts and
/// indices of the marked and current insertions and the glyph list, pure, so that <see cref="Sealed.runInsertionPass"/> can run the
/// transition straight on the working copy.
/// </summary>
type internal IInsertionDecisions<'E when 'E : struct> =
    /// <summary>Whether the glyphs written are collected for the buffer glyph set.</summary>
    abstract Collect: bool
    /// <summary>Whether an entry sets the mark, <c>Flags::SetMark</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry sets the mark.</returns>
    abstract SetsMark: entry: 'E -> bool
    /// <summary>Whether an entry leaves the cursor where the insertion left it, <c>Flags::DontAdvance</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the cursor does not move past the inserted glyphs.</returns>
    abstract DontAdvance: entry: 'E -> bool
    /// <summary>The index of the glyphs inserted at the mark, 0xFFFF for none.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The index.</returns>
    abstract MarkedIndex: entry: 'E -> uint16
    /// <summary>The count of the glyphs inserted at the mark.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The count.</returns>
    abstract MarkedCount: entry: 'E -> int
    /// <summary>Whether the glyphs at the mark go before it.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True for before.</returns>
    abstract MarkedBefore: entry: 'E -> bool
    /// <summary>The index of the glyphs inserted at the current glyph, 0xFFFF for none.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The index.</returns>
    abstract CurrentIndex: entry: 'E -> uint16
    /// <summary>The count of the glyphs inserted at the current glyph.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>The count.</returns>
    abstract CurrentCount: entry: 'E -> int
    /// <summary>Whether the glyphs at the current glyph go before it.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True for before.</returns>
    abstract CurrentBefore: entry: 'E -> bool
    /// <summary>The count of a list of glyphs that lies within the subtable, zero for one that runs past its end.</summary>
    /// <param name="start">The index of the first glyph.</param>
    /// <param name="count">The count asked for.</param>
    /// <returns>The count to insert.</returns>
    abstract EffectiveCount: start: int * count: int -> int
    /// <summary>Reads a glyph of the insertion list.</summary>
    /// <param name="index">The index in the list.</param>
    /// <returns>The glyph.</returns>
    abstract ReadGlyph: index: int -> uint32
    /// <summary>The properties a written glyph takes, <c>output_glyphs</c>'s property logic.</summary>
    /// <param name="glyph">The written glyph.</param>
    /// <param name="props">The properties the glyph it stands by had.</param>
    /// <returns>The properties it takes.</returns>
    abstract Props: glyph: uint32 * props: GlyphProps -> GlyphProps

/// <summary>
/// The decisions of a kerx format 4 subtable, <c>KerxSubTableFormat4::driver_context_t::transition</c>: whether an entry has an
/// action, what the action does to the current glyph's position and how that position attaches to the marked glyph, pure, so that
/// <see cref="Sealed.runFormat4Pass"/> can run the transition straight on the working copy.
/// </summary>
type internal IFormat4Decisions<'E when 'E : struct> =
    /// <summary>Whether an entry has an action, <c>is_actionable</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry has an action index.</returns>
    abstract Performs: entry: 'E -> bool
    /// <summary>Whether an entry remembers the glyph as the marked glyph.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry sets the mark.</returns>
    abstract SetsMark: entry: 'E -> bool
    /// <summary>What an entry's action does at the current glyph: nothing, charge the budget, or move the glyph and attach it.</summary>
    /// <param name="entry">The entry.</param>
    /// <param name="infos">The glyphs of the run.</param>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="mark">The marked glyph.</param>
    /// <param name="index">The current glyph.</param>
    /// <returns>0 for nothing, 1 to charge <see cref="ChargeAmount"/>, 2 to attach, and the moved position for 2.</returns>
    abstract Resolve: entry: 'E * infos: ReadOnlySpan<GlyphInfo> * positions: ReadOnlySpan<GlyphPosition> * mark: int * index: int -> struct (int * GlyphPosition)
    /// <summary>The operations the charge action costs.</summary>
    abstract ChargeAmount: int
    /// <summary>Attaches a moved glyph to the marked glyph: the cross-stream offset the cursive chain of the mark resolves to, the
    /// attach type and the chain to the mark.</summary>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="index">The current glyph.</param>
    /// <param name="mark">The marked glyph.</param>
    /// <param name="moved">The current glyph's moved position.</param>
    /// <returns>The attached position.</returns>
    abstract Attach: positions: ReadOnlySpan<GlyphPosition> * index: int * mark: int * moved: GlyphPosition -> GlyphPosition

/// <summary>
/// The decisions of a noncontextual substitution subtable, <c>NoncontextualSubtable::apply</c>: which glyphs are replaced by
/// which, in which cluster ranges, and the properties the replacement takes, pure, so that
/// <see cref="Sealed.runNoncontextualPass"/> can write the replacements in one loop.
/// </summary>
type internal INoncontextualDecisions =
    /// <summary>The number of ranges of the chain; the range walk runs only above one.</summary>
    abstract RangeCount: int
    /// <summary>Finds the range covering a cluster, moving down then up from the current one.</summary>
    /// <param name="index">The current range index.</param>
    /// <param name="cluster">The glyph cluster.</param>
    /// <returns>The range index.</returns>
    abstract AdvanceRange: index: int * cluster: uint32 -> int
    /// <summary>Whether the subtable applies in a range.</summary>
    /// <param name="index">The range index.</param>
    /// <returns>True when the range carries a subtable flag.</returns>
    abstract RangeEnabled: index: int -> bool
    /// <summary>The glyph a glyph is replaced by.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <returns>Whether there is a replacement, and the replacement.</returns>
    abstract Replacement: glyph: uint32 -> struct (bool * uint32)
    /// <summary>The properties a replacement takes, the deleted and glyph class logic of the subtable.</summary>
    /// <param name="glyph">The replacement.</param>
    /// <param name="props">The properties the replaced glyph had.</param>
    /// <returns>The properties it takes.</returns>
    abstract Props: glyph: uint32 * props: GlyphProps -> GlyphProps

