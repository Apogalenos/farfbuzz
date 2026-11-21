namespace Farfbuzz

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
open Microsoft.FSharp.Core.OptimizedClosures

/// <summary>One operation applied to a code-point set, in upstream order.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal SetOp =
    | Add of lo: uint32 * hi: uint32
    | Delete of dlo: uint32 * dhi: uint32

/// <summary>A closed range of code points.</summary>
[<Struct>]
type internal CodepointRange =
    { First: uint32
      Last: uint32 }

/// <summary>A struct comparer over indices, used by the deterministic introsort.</summary>
type internal IIndexComparer =
    /// <summary>Compares two indices.</summary>
    /// <param name="a">The first index.</param>
    /// <param name="b">The second index.</param>
    /// <returns>A negative number when a comes first, positive when b comes first, zero when equal.</returns>
    abstract Compare : a: int * b: int -> int

/// <summary>
/// The only place in the library that writes into arrays. Each function creates a fresh array, fills it
/// before anyone else can see it, and publishes it zero-copy as an <see cref="ImmutableArray{T}"/>: from the
/// outside every function here is pure, exactly like a value built in the ST monad.
/// </summary>
/// <summary>A folder for <see cref="Sealed.unfoldSpan"/>.</summary>
type internal ISpanUnfolder<'E, 'S, 'T> =
    /// <summary>Consumes one source position and returns a value and the next state.</summary>
    /// <param name="source">The source span.</param>
    /// <param name="state">The current state.</param>
    /// <returns>The value and the next state.</returns>
    abstract Next: source: ReadOnlySpan<'E> * state: 'S -> struct ('T * 'S)

/// <summary>A mapper for <see cref="Sealed.mapGroups"/>: it groups a span and maps each element with its group's bounds.</summary>
type internal IGroupMapper<'T> =
    /// <summary>Returns the end index of the group that starts at <paramref name="start"/>.</summary>
    /// <param name="items">The source span.</param>
    /// <param name="start">The first index of the group.</param>
    /// <returns>The exclusive end index.</returns>
    abstract GroupEnd: items: ReadOnlySpan<'T> * start: int -> int

    /// <summary>Maps one element of a group, given the whole group's bounds.</summary>
    /// <param name="items">The source span.</param>
    /// <param name="start">The group's first index.</param>
    /// <param name="finish">The group's exclusive end index.</param>
    /// <param name="index">The element's index.</param>
    /// <returns>The mapped element.</returns>
    abstract Map: items: ReadOnlySpan<'T> * start: int * finish: int * index: int -> 'T

/// <summary>A grouping for <see cref="Sealed.reverseGroups"/>: adjacent glyphs stay in one group while it returns true.</summary>
type internal IGlyphGrouping =
    /// <summary>Whether two adjacent glyphs belong to the same group.</summary>
    /// <param name="previous">The previous glyph's properties.</param>
    /// <param name="current">The current glyph's properties.</param>
    /// <returns>True when the current glyph continues the previous glyph's group.</returns>
    abstract Group: previous: GlyphProps * current: GlyphProps -> bool

/// <summary>A glyph predicate for <see cref="Sealed.deleteGlyphsInplace"/>.</summary>
type internal IGlyphPredicate =
    /// <summary>Whether a glyph should be deleted in place.</summary>
    /// <param name="info">The glyph info.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>True to delete the glyph.</returns>
    abstract Delete: info: GlyphInfo * props: GlyphProps -> bool

/// <summary>One step of a run source: a run of indices to paint, or the exhausted scan.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal RunStep<'S> =
    | Run of start: int * stop: int * value: byte * next: 'S
    | Exhausted of final: 'S

/// <summary>A source of painted runs for <see cref="Sealed.paintRuns"/>: one step produces the next run or exhaustion.</summary>
type internal IRunSource<'S> =
    /// <summary>Produces the next run from a state, or reports the scan exhausted.</summary>
    /// <param name="state">The current state.</param>
    /// <returns>The next run to paint, or the exhausted scan.</returns>
    abstract Next: state: 'S -> RunStep<'S>

/// <summary>The codes of the flattened statements of a Ragel action body, the low byte of an <see cref="RagelCompiled"/> op: the argument
/// of a statement that has one is in the bits above.</summary>
module internal RagelCodes =
    [<Literal>]
    let TsFromP = 0

    [<Literal>]
    let TsReset = 1

    [<Literal>]
    let TeFromNextP = 2

    [<Literal>]
    let TeFromP = 3

    [<Literal>]
    let PBack = 4

    [<Literal>]
    let PFromTe = 5

    [<Literal>]
    let ActSet = 6

    [<Literal>]
    let FoundSyllable = 7

    [<Literal>]
    let NoAction = 0uy

    [<Literal>]
    let OpsAction = 1uy

    [<Literal>]
    let SwitchAction = 2uy

    [<Literal>]
    let ArgumentShift = 8

    [<Literal>]
    let BrokenShift = 16

    [<Literal>]
    let CodeMask = 255

    [<Literal>]
    let ArgumentMask = 255

    [<Literal>]
    let NoList = -1

    [<Literal>]
    let EndOfBody = 255

    [<Literal>]
    let EffectSlow = -1

    [<Literal>]
    let EffectSwitch = -2

    [<Literal>]
    let EffectTeFromTe = 1

    [<Literal>]
    let EffectOffsetBias = 4

    [<Literal>]
    let EffectOffsetMask = 7

    [<Literal>]
    let EffectPShift = 4

    [<Literal>]
    let EffectHasAct = 256

    [<Literal>]
    let EffectActShift = 9

    [<Literal>]
    let EffectFound = 131072

    [<Literal>]
    let EffectTypeShift = 18

    [<Literal>]
    let EffectBroken = 67108864

    [<Literal>]
    let EffectFoundShift = 27

/// <summary>A Ragel scanner's tables as flat arrays, made once for a machine by <c>Ragel.compile</c>: the transitions are the machine's
/// own, and every action body is a range of one array of codes, so that the scan runs as one loop with no statement of a body decoded
/// from a union.</summary>
[<NoEquality; NoComparison>]
type internal RagelCompiled =
    { FlatTransKeys: ImmutableArray<byte>
      FlatKeySpans: ImmutableArray<byte>
      FlatIndexOffsets: ImmutableArray<int>
      FlatIndicies: ImmutableArray<byte>
      FlatTransTargs: ImmutableArray<byte>
      FlatTransActions: ImmutableArray<byte>
      FlatSetsTs: ImmutableArray<bool>
      FlatResetsTs: ImmutableArray<bool>
      FlatEofTrans: ImmutableArray<int>
      ActionKinds: ImmutableArray<byte>
      ActionFirstList: ImmutableArray<int>
      ActionListCount: ImmutableArray<int>
      ListStarts: ImmutableArray<int>
      ListEnds: ImmutableArray<int>
      OpCodes: ImmutableArray<int>
      ListEffects: ImmutableArray<int>
      DenseColumns: int
      DenseMaxKey: int
      DenseNext: ImmutableArray<int>
      DenseEffect: ImmutableArray<int>
      DenseTrans: ImmutableArray<int>
      FlatStart: int }

/// <summary>One glyph the simple-run kernel accepts: its properties, which carry the glyph index, and its default
/// horizontal advance.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal SimpleGlyph =
    { SimpleProps: GlyphProps
      SimpleAdvance: int }

/// <summary>The per-glyph decision of the simple-run kernel, implemented by a struct so the loop inlines it.</summary>
type internal ISimpleGlyphs =
    /// <summary>Classifies one code point.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <param name="metrics">The bytes of the horizontal metrics, taken once for the run.</param>
    /// <returns>The glyph, or none when the code point does not qualify as simple.</returns>
    abstract Glyph: codepoint: uint32 * metrics: ReadOnlySpan<byte> -> SimpleGlyph voption

/// <summary>The decision of the simple-run kernel on a glyph the normaliser has already chosen, implemented by a struct so the loop
/// inlines it.</summary>
type internal ISimpleClassifier =
    /// <summary>Classifies one glyph.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <param name="unicode">The packed Unicode properties of the glyph's code point.</param>
    /// <param name="metrics">The bytes of the horizontal metrics, taken once for the run.</param>
    /// <returns>The glyph, or none when it does not qualify as simple.</returns>
    abstract Classify: glyph: uint32 * unicode: uint16 * metrics: ReadOnlySpan<byte> -> SimpleGlyph voption

/// <summary>The per-glyph advance of the simple-run kernel's second loop, implemented by a struct so the loop inlines it.</summary>
type internal ISimpleAdvances =
    /// <summary>Reads the default horizontal advance of a glyph.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <param name="metrics">The bytes of the horizontal metrics, taken once for the run.</param>
    /// <returns>The advance.</returns>
    abstract Advance: glyph: uint32 * metrics: ReadOnlySpan<byte> -> int

/// <summary>The three arrays a simple run is shaped into.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal SimpleRunArrays =
    { KernelInfos: ImmutableArray<GlyphInfo>
      KernelProps: ImmutableArray<GlyphProps>
      KernelPositions: ImmutableArray<GlyphPosition>
      Covered: bool }  // whether some glyph is in the glyph set the kernel was given

/// <summary>The arrays a worker shapes one line at a time in, owned by one chunk of a document and never shared: the glyphs,
/// properties and positions of the line being shaped, in arrays as long as the chunk's longest line.</summary>
[<NoEquality; NoComparison>]
type internal LineScratch =
    { ScratchInfos: GlyphInfo array
      ScratchProps: GlyphProps array
      ScratchPositions: GlyphPosition array }

/// <summary>What the lookup kernel made of a line it read from, and wrote into, arrays a worker owns: the arrays that hold the glyphs
/// afterwards (the worker's own when the line did not outgrow them) and how many glyphs there are, with the rest of a run's state.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal OwnedGsub =
    { OwnedInfos: GlyphInfo array
      OwnedProps: GlyphProps array
      OwnedLength: int
      OwnedUnchanged: bool // no lookup changed the glyphs, the properties or the budget: the worker's arrays are as they were
      OwnedMaxOps: int
      OwnedSuccessful: bool
      OwnedSerial: byte
      OwnedRandom: uint32
      OwnedScratch: ScratchFlags }

/// <summary>The decisions of the lean normaliser <see cref="Sealed.normalizeRun"/>, implemented by a struct so its loops inline them:
/// what the font maps, what a glyph decomposes into and composes with, and what the shaper's hooks say.</summary>
type internal INormalizeDecisions =
    /// <summary>The nominal glyph of a code point.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph, or none when the font has none.</returns>
    abstract Glyph: codepoint: uint32 -> uint32 voption
    /// <summary>The page of the answers the face has kept for the code points of a block of 256 above the low table, for a loop that reads
    /// the code points of a line in order and asks <see cref="Glyph"/> only for those no page holds.</summary>
    /// <param name="block">The block, the code point shifted right by eight.</param>
    /// <returns>The entries of the block's code points, 0 for one not kept yet, 1 for one without a glyph and the glyph plus 2 otherwise,
    /// or null when no code point of the block has been kept.</returns>
    abstract Page: block: int -> (uint32 array | null)
    /// <summary>Whether a code point the font does not map is a plain miss: no decomposition, no space, not the no-break hyphen.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the miss maps straight to the not-found glyph.</returns>
    abstract PlainNotFound: props: GlyphProps * codepoint: uint32 -> bool
    /// <summary>Whether the shaper's decompose hook leaves a code point whole, so that a glyph the font maps for it is its whole
    /// decomposition.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the code point has no decomposition.</returns>
    abstract Undecomposable: codepoint: uint32 -> bool
    /// <summary>The not-found glyph.</summary>
    abstract NotFound: uint32
    /// <summary>The edits that decompose one glyph the font does not map directly, or that map it through its fallbacks.</summary>
    /// <param name="info">The glyph.</param>
    /// <param name="props">Its properties.</param>
    /// <param name="shortest">Whether to short-circuit on the first glyph.</param>
    /// <returns>The edits, the last of which skips or advances the glyph.</returns>
    abstract Edits: info: GlyphInfo * props: GlyphProps * shortest: bool -> ImmutableArray<RunEdit>
    /// <summary>Whether a glyph is a Unicode mark.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>True for a mark.</returns>
    abstract IsMark: props: GlyphProps -> bool
    /// <summary>The modified combining class of a glyph.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The class, zero for a glyph that is no mark.</returns>
    abstract Mcc: props: GlyphProps -> byte
    /// <summary>The composition of a starter and a mark, with the glyph the font has for the composite.</summary>
    /// <param name="starter">The starter's code point.</param>
    /// <param name="mark">The mark's code point.</param>
    /// <returns>The composite and its glyph, or none when they do not compose or the font lacks the composite.</returns>
    abstract Compose: starter: uint32 * mark: uint32 -> struct (uint32 * uint32) voption
    /// <summary>The Unicode properties of a composite.</summary>
    /// <param name="composed">The composite's code point.</param>
    /// <returns>The packed properties.</returns>
    abstract ComposedProps: composed: uint32 -> uint16
    /// <summary>The edits the shaper's mark-reordering hook makes to a sorted mark run.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="infos">The glyphs, whose code points a hook may read.</param>
    /// <param name="start">The first mark of the run.</param>
    /// <param name="finish">The end of the mark run.</param>
    /// <returns>The edits, none when the hook leaves the run alone.</returns>
    abstract Reorder: props: ReadOnlySpan<GlyphProps> * infos: ReadOnlySpan<GlyphInfo> * start: int * finish: int -> ImmutableArray<RunEdit>
    /// <summary>The properties of a combining grapheme joiner after the unhide pass.</summary>
    /// <param name="codepoint">The glyph's code point.</param>
    /// <param name="previous">The previous glyph's properties.</param>
    /// <param name="current">The glyph's properties.</param>
    /// <param name="next">The next glyph's properties.</param>
    /// <returns>The glyph's properties, unhidden when the joiner prevented no reordering.</returns>
    abstract Unhide: codepoint: uint32 * previous: GlyphProps * current: GlyphProps * next: GlyphProps -> GlyphProps
    /// <summary>Whether a mark may compose with some glyph before it: false only when neither the shaper's compose hook nor Unicode
    /// composes any pair that ends with the code point.</summary>
    /// <param name="codepoint">The mark's code point.</param>
    /// <returns>False when the mark composes with nothing.</returns>
    abstract MightCompose: codepoint: uint32 -> bool
    /// <summary>Whether the shaper's mark-reordering hook may change a mark run of a single mark.</summary>
    abstract ReordersSingleMarks: bool
    /// <summary>The scratch flags a glyph with its code point adds to the buffer.</summary>
    /// <param name="info">The glyph, whose code point is read.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The flags, none for a plain glyph.</returns>
    abstract Scratch: info: GlyphInfo * props: GlyphProps -> ScratchFlags
    /// <summary>The scratch flags a glyph adds, from its code point and its packed Unicode properties alone, as <see cref="Scratch"/>
    /// answers them: a loop that reads the two fields of the glyph in place need not copy the glyph and its properties.</summary>
    /// <param name="codepoint">The glyph's code point.</param>
    /// <param name="unicode">The glyph's packed Unicode properties.</param>
    /// <returns>The flags, none for a plain glyph.</returns>
    abstract ScratchOf: codepoint: uint32 * unicode: uint16 -> ScratchFlags
    /// <summary>Whether a glyph with these packed Unicode properties is a Unicode mark, as <see cref="IsMark"/> answers it.</summary>
    /// <param name="unicode">The glyph's packed Unicode properties.</param>
    /// <returns>True for a mark.</returns>
    abstract IsMarkOf: unicode: uint16 -> bool
    /// <summary>The parts one code point decomposes into, as <c>decompose</c> makes them, each with its glyph, written to an array.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <param name="shortest">Whether to stop at the first part the font maps.</param>
    /// <param name="parts">The array to write the code points and glyphs of the parts to, from its start, long enough for any decomposition.</param>
    /// <returns>The number of parts, zero when the code point does not decompose into glyphs the font maps.</returns>
    abstract Decompose: codepoint: uint32 * shortest: bool * parts: struct (uint32 * uint32) array -> int
    /// <summary>The Unicode properties of a part of a decomposition.</summary>
    /// <param name="codepoint">The part's code point.</param>
    /// <returns>The packed properties.</returns>
    abstract PartProps: codepoint: uint32 -> uint16

/// <summary>What the Hangul preprocess asks of the font: which code points it maps and which of them it gives no advance.</summary>
type internal IHangulGlyphs =
    /// <summary>Whether the font maps a code point, <c>font->has_glyph</c>.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the font has a nominal glyph for it.</returns>
    abstract Has: codepoint: uint32 -> bool
    /// <summary>Whether the font gives a code point a glyph with no advance, <c>is_zero_width_char</c>.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the glyph is zero width.</returns>
    abstract ZeroWidth: codepoint: uint32 -> bool

/// <summary>What the page of a presence table asks of the font to be built: whether it maps a code point.</summary>
type internal IGlyphProbe =
    /// <summary>Whether the font maps a code point, <c>font->has_glyph</c>.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the font has a nominal glyph for it.</returns>
    abstract Maps: codepoint: uint32 -> bool

/// <summary>Sees a run after each lookup the GSUB kernel applies that changed it, the part of the syllable reuse that learns inside the kernel's
/// loop: a lookup that left the run as it was is not shown, the watch's last look standing for it.</summary>
type internal ILookupWatch =
    /// <summary>Sees the run after the kernel applied one lookup of its program that changed the run, its glyphs, its operations or its
    /// success: the syllable reuse learns through it what each lookup did.</summary>
    /// <param name="index">The lookup's position in the program.</param>
    /// <param name="serial">The serial after the lookup.</param>
    /// <param name="props">The properties of the run after the lookup.</param>
    abstract After: index: int * serial: byte * props: ReadOnlySpan<GlyphProps> -> unit

    /// <summary>Sees a ligature id made, <c>_hb_allocate_lig_id</c> in <c>ligate_input</c>: the syllable reuse puts it down to the unit whose
    /// clusters hold the ligature's.</summary>
    /// <param name="cluster">The cluster of the ligature glyph.</param>
    abstract Allocated: cluster: uint32 -> unit

/// <summary>What the properties pass asks of the Unicode tables for a code point beyond its small table.</summary>
type internal IPlainProps =
    /// <summary>The packed properties of a code point and whether none of the continuation rules of <c>hb_set_unicode_props</c> reads
    /// its neighbours: a letter or a space.</summary>
    /// <param name="codepoint">The code point.</param>
    /// <returns>Whether it is plain, its packed properties and its scratch flags.</returns>
    abstract Plain: codepoint: uint32 -> struct (bool * uint16 * ScratchFlags)

[<RequireQualifiedAccess>]
module internal Sealed =
    /// <summary>Publishes a freshly filled array without copying it.</summary>
    /// <param name="items">The array, never written again.</param>
    /// <returns>The immutable view.</returns>
    let private publish (items: 'T array) : ImmutableArray<'T> =
        ImmutableCollectionsMarshal.AsImmutableArray items

    /// <summary>Fills a fresh array from an index to its end with a generator.</summary>
    /// <param name="target">The array being built.</param>
    /// <param name="generate">The adapted generator.</param>
    /// <param name="index">The next index.</param>
    [<TailCall>]
    let rec private fillFrom (target: 'T array) (generate: int -> 'T) (index) =
        if index < target.Length then
            target[index] <- generate index
            fillFrom target generate (index + 1)

    /// <summary>Fills a fresh array by mapping a source span, passing indices.</summary>
    /// <param name="target">The array being built.</param>
    /// <param name="source">The source items.</param>
    /// <param name="mapping">The adapted mapping.</param>
    /// <param name="index">The next index.</param>
    [<TailCall>]
    let rec private mapFrom (target: 'T array) (source: ReadOnlySpan<'S>) (mapping: FSharpFunc<int, 'S, 'T>) (index) =
        if index < target.Length then
            target[index] <- mapping.Invoke(index, source[index])
            mapFrom target source mapping (index + 1)

    /// <summary>Writes the chosen results of a source span into a fresh array.</summary>
    /// <param name="target">The array being built, as long as the source.</param>
    /// <param name="source">The source items.</param>
    /// <param name="chooser">The adapted chooser.</param>
    /// <param name="index">The next source index.</param>
    /// <param name="count">The number of results written so far.</param>
    /// <returns>The number of results.</returns>
    [<TailCall>]
    let rec private chooseFrom (target: 'T array) (source: ReadOnlySpan<'S>) (chooser: 'S -> 'T voption) (index) (count) =
        match index < source.Length with
        | false -> count
        | true ->
            match chooser source[index] with
            | ValueSome result ->
                target[count] <- result
                chooseFrom target source chooser (index + 1) (count + 1)
            | ValueNone -> chooseFrom target source chooser (index + 1) count

    /// <summary>Builds an array of a given length from a generator of each index.</summary>
    /// <param name="count">The length.</param>
    /// <param name="generate">The generator.</param>
    /// <returns>The immutable array.</returns>
    let init (count) (generate: int -> 'T) : ImmutableArray<'T> =
        let target = Array.zeroCreate count
        fillFrom target generate 0
        publish target

    /// <summary>Builds an array of a given length whose items are all the default value of their type: one allocation,
    /// no generator call per item.</summary>
    /// <param name="count">The length.</param>
    /// <returns>The immutable array of zeroed items.</returns>
    let zeroed<'T> (count: int) =
        publish (Array.zeroCreate<'T> count)

    /// <summary>Maps every item of a span, passing its index.</summary>
    /// <param name="mapping">The mapping, given the index and the item.</param>
    /// <param name="source">The source items.</param>
    /// <returns>The immutable array of results.</returns>
    let mapi (mapping: int -> 'S -> 'T) (source: ReadOnlySpan<'S>) : ImmutableArray<'T> =
        let target = Array.zeroCreate source.Length
        mapFrom target source (FSharpFunc<int, 'S, 'T>.Adapt mapping) 0
        publish target

    /// <summary>Maps every item of a span.</summary>
    /// <param name="mapping">The mapping.</param>
    /// <param name="source">The source items.</param>
    /// <returns>The immutable array of results.</returns>
    let map (mapping: 'S -> 'T) (source: ReadOnlySpan<'S>) : ImmutableArray<'T> =
        mapi (fun _ item -> mapping item) source

    /// <summary>Whether ranges are in order and apart: each ends no sooner than it starts and before the next one starts, so that a search
    /// over them finds the one range that holds a glyph, whatever the search does with the ranges it passes.</summary>
    /// <param name="firsts">The first glyph of each range.</param>
    /// <param name="lasts">The last glyph of each range.</param>
    /// <returns>True when the ranges are ordered.</returns>
    let rangesOrdered (firsts: ReadOnlySpan<uint32>) (lasts: ReadOnlySpan<uint32>) =
        let mutable ordered = true
        let mutable index = 0
        while ordered && index < firsts.Length do
            ordered <- firsts[index] <= lasts[index] && (index + 1 = firsts.Length || lasts[index] < firsts[index + 1])
            index <- index + 1
        ordered

    /// <summary>The covered index of every glyph below a count for a coverage that lists its glyphs in strictly ascending order, as a
    /// search over them answers, written in one pass over the glyphs and not by a search for each glyph: the index of a covered glyph and
    /// the marker of a glyph that is not covered. A list out of order, or one whose indices do not all fit below the marker, has none.</summary>
    /// <param name="glyphs">The listed glyphs.</param>
    /// <param name="count">The number of glyphs the table holds.</param>
    /// <param name="absent">The marker of a glyph that is not covered, one more than the largest index that fits.</param>
    /// <returns>The table, or none.</returns>
    let directOfGlyphs (glyphs: ReadOnlySpan<uint32>) (count: int) (absent: uint16) =
        let mutable ascending = glyphs.Length < int absent
        let mutable index = 1
        while ascending && index < glyphs.Length do
            ascending <- glyphs[index - 1] < glyphs[index]
            index <- index + 1
        match ascending with
        | false -> ValueNone
        | true ->
            let table = Array.create count absent
            let mutable at = 0
            while at < glyphs.Length && glyphs[at] < uint32 count do
                table[int glyphs[at]] <- uint16 at
                at <- at + 1
            ValueSome (publish table)

    /// <summary>The covered index of every glyph below a count for a coverage of ranges that are in order and apart, written in one pass
    /// over the ranges: a glyph of a range has the range's first index plus its distance from the range's first glyph, and a glyph in no range
    /// has the marker. Ranges out of order, or an index that does not fit below the marker, give none.</summary>
    /// <param name="firsts">The first glyph of each range.</param>
    /// <param name="lasts">The last glyph of each range.</param>
    /// <param name="starts">The index of the first glyph of each range.</param>
    /// <param name="count">The number of glyphs the table holds.</param>
    /// <param name="absent">The marker of a glyph that is not covered, one more than the largest index that fits.</param>
    /// <returns>The table, or none.</returns>
    let directOfRanges (firsts: ReadOnlySpan<uint32>) (lasts: ReadOnlySpan<uint32>) (starts: ReadOnlySpan<uint32>) (count: int) (absent: uint16) =
        match rangesOrdered firsts lasts with
        | false -> ValueNone
        | true ->
            let table = Array.create count absent
            let mutable fits = true
            let mutable at = 0
            while fits && at < firsts.Length && firsts[at] < uint32 count do
                let upper = min lasts[at] (uint32 count - 1u)
                let mutable glyph = firsts[at]
                while fits && glyph <= upper do
                    let answer = starts[at] + (glyph - firsts[at])
                    fits <- answer < uint32 absent
                    table[int glyph] <- uint16 answer
                    glyph <- glyph + 1u
                at <- at + 1
            match fits with
            | true -> ValueSome (publish table)
            | false -> ValueNone

    /// <summary>The class of every glyph below a count for a class definition of ranges that are in order and apart, written in one pass
    /// over the ranges: a glyph of a range has the range's class, kept as the largest a table entry holds when it is larger, and a glyph in
    /// no range has class zero. Ranges out of order give none.</summary>
    /// <param name="firsts">The first glyph of each range.</param>
    /// <param name="lasts">The last glyph of each range.</param>
    /// <param name="classes">The class of each range.</param>
    /// <param name="count">The number of glyphs the table holds.</param>
    /// <returns>The table, or none.</returns>
    let directOfClassRanges (firsts: ReadOnlySpan<uint32>) (lasts: ReadOnlySpan<uint32>) (classes: ReadOnlySpan<uint32>) (count: int) =
        match rangesOrdered firsts lasts with
        | false -> ValueNone
        | true ->
            let table = Array.zeroCreate<uint16> count
            let mutable at = 0
            while at < firsts.Length && firsts[at] < uint32 count do
                let upper = min lasts[at] (uint32 count - 1u)
                let klass = uint16 (min 0xFFFFu classes[at])
                let mutable glyph = firsts[at]
                while glyph <= upper do
                    table[int glyph] <- klass
                    glyph <- glyph + 1u
                at <- at + 1
            ValueSome (publish table)

    /// <summary>The class of every glyph below a count for a class definition that is an array of classes from a first glyph, written in one
    /// pass over the array: a glyph in the array has its class, kept as the largest a table entry holds when it is larger, and any other has
    /// class zero.</summary>
    /// <param name="start">The glyph the array starts at.</param>
    /// <param name="classes">The classes of the glyphs from the first.</param>
    /// <param name="count">The number of glyphs the table holds.</param>
    /// <returns>The table.</returns>
    let directOfClassArray (start: uint32) (classes: ReadOnlySpan<uint32>) (count: int) =
        let table = Array.zeroCreate<uint16> count
        let mutable at = 0
        while at < classes.Length && start + uint32 at < uint32 count do
            table[int (start + uint32 at)] <- uint16 (min 0xFFFFu classes[at])
            at <- at + 1
        publish table

    /// <summary>Keeps the results of a chooser that returns a value.</summary>
    /// <param name="chooser">The chooser.</param>
    /// <param name="source">The source items.</param>
    /// <returns>The immutable array of chosen results, in source order.</returns>
    let choose (chooser: 'S -> 'T voption) (source: ReadOnlySpan<'S>) : ImmutableArray<'T> =
        let target = Array.zeroCreate source.Length
        let count = chooseFrom target source chooser 0 0
        match count = target.Length with
        | true -> publish target
        | false -> publish (target.AsSpan(0, count).ToArray())

    /// <summary>Fills an array by mapping a span, stopping at the first mapping that returns none.</summary>
    /// <param name="target">The array being built, as long as the source.</param>
    /// <param name="source">The source items.</param>
    /// <param name="mapping">The mapping, returning none to fail.</param>
    /// <param name="index">The next source index.</param>
    /// <returns>Some of the filled length when every mapping succeeded, none when one failed.</returns>
    [<TailCall>]
    let rec private tryMapAllFrom (target: 'T array) (source: ReadOnlySpan<'S>) (mapping: 'S -> 'T voption) (index: int) : int voption =
        match index < source.Length with
        | false -> ValueSome source.Length
        | true ->
            match mapping source[index] with
            | ValueSome value ->
                target[index] <- value
                tryMapAllFrom target source mapping (index + 1)
            | ValueNone -> ValueNone

    /// <summary>Maps every item of a span, returning none when any mapping fails.</summary>
    /// <param name="mapping">The mapping, returning none to fail the whole map.</param>
    /// <param name="source">The source items.</param>
    /// <returns>The mapped array, or none when a mapping returned none.</returns>
    let tryMapAll (mapping: 'S -> 'T voption) (source: ReadOnlySpan<'S>) : ImmutableArray<'T> voption =
        let target = Array.zeroCreate source.Length
        match tryMapAllFrom target source mapping 0 with
        | ValueNone -> ValueNone
        | ValueSome _ -> ValueSome (publish target)

    /// <summary>Unfolds a span into an immutable array.</summary>
    /// <param name="source">The source span.</param>
    /// <param name="count">The number of values.</param>
    /// <param name="seed">The initial state.</param>
    /// <param name="folder">The folder.</param>
    /// <returns>The values.</returns>
    let unfoldSpan<'E, 'S, 'T, 'F when 'F : struct and 'F :> ISpanUnfolder<'E, 'S, 'T>>
            (source: ReadOnlySpan<'E>) (count) (seed: 'S) (folder: 'F) : ImmutableArray<'T> =
        let values = Array.zeroCreate count
        let mutable state = seed
        for index in 0 .. count - 1 do
            let struct (value, next) = folder.Next(source, state)
            values[index] <- value
            state <- next
        publish values

    /// <summary>Builds the glyph infos of a text whose every byte is one code point equal to its own value: one loop, one
    /// array, written once.</summary>
    /// <param name="source">The bytes.</param>
    /// <param name="clusterBase">The cluster of the first byte.</param>
    /// <returns>One info per byte, its cluster the byte's offset from the first.</returns>
    let byteInfos (source: ReadOnlySpan<byte>) (clusterBase: int) =
        let infos = Array.zeroCreate<GlyphInfo> source.Length
        for index in 0 .. source.Length - 1 do
            infos[index] <- { Codepoint = uint32 source[index]; Mask = 0u; Cluster = uint32 (clusterBase + index); Var1 = 0u; Var2 = 0u }
        publish infos

    /// <summary>Reads one code point of well-formed UTF-8: nothing is checked, the caller has proved the text well-formed.</summary>
    /// <param name="source">The well-formed bytes.</param>
    /// <param name="at">The index of the lead byte.</param>
    /// <returns>The code point and the number of bytes it takes.</returns>
    let private wellFormedUtf8 (source: ReadOnlySpan<byte>) (at: int) =
        let lead = uint32 source[at]
        match lead with
        | _ when lead < 0x80u -> struct (lead, 1)
        | _ when lead < 0xE0u -> struct (((lead &&& 0x1Fu) <<< 6) ||| (uint32 source[at + 1] &&& 0x3Fu), 2)
        | _ when lead < 0xF0u ->
            struct (((lead &&& 0x0Fu) <<< 12) ||| ((uint32 source[at + 1] &&& 0x3Fu) <<< 6) ||| (uint32 source[at + 2] &&& 0x3Fu), 3)
        | _ ->
            struct (((lead &&& 0x07u) <<< 18) ||| ((uint32 source[at + 1] &&& 0x3Fu) <<< 12) ||| ((uint32 source[at + 2] &&& 0x3Fu) <<< 6) ||| (uint32 source[at + 3] &&& 0x3Fu), 4)

    /// <summary>Counts the code points of well-formed UTF-8: the bytes that are not continuation bytes.</summary>
    /// <param name="source">The well-formed bytes.</param>
    /// <returns>The number of code points.</returns>
    let utf8Count (source: ReadOnlySpan<byte>) =
        let mutable continuations = 0
        let mutable index = 0
        while index + 16 <= source.Length do
            let bytes = Vector128.LoadUnsafe(&MemoryMarshal.GetReference source, unativeint index)
            let isContinuation = Vector128.LessThan(bytes.AsSByte(), Vector128.Create(-64y))
            continuations <- continuations + BitOperations.PopCount (isContinuation.ExtractMostSignificantBits())
            index <- index + 16
        while index < source.Length do
            if (source[index] &&& 0xC0uy) = 0x80uy then continuations <- continuations + 1
            index <- index + 1
        source.Length - continuations

    /// <summary>Builds the glyph infos of well-formed UTF-8 in one decoding loop and one array, written once, the cluster of
    /// each the offset of its lead byte from the first byte. Ill-formed bytes are never given: they take the checking decoder.</summary>
    /// <param name="source">The well-formed bytes.</param>
    /// <param name="clusterBase">The cluster of the first byte.</param>
    /// <returns>One info per code point.</returns>
    let utf8Infos (source: ReadOnlySpan<byte>) (clusterBase: int) =
        let count = utf8Count source
        let infos = GC.AllocateUninitializedArray<GlyphInfo> count
        let mutable at = 0
        for index in 0 .. count - 1 do
            let struct (value, width) = wellFormedUtf8 source at
            infos[index] <- { Codepoint = value; Mask = 0u; Cluster = uint32 (clusterBase + at); Var1 = 0u; Var2 = 0u }
            at <- at + width
        publish infos

    /// <summary>The words of one page of a presence table: the answers of a probe for the 256 code points of a block, one bit each, the
    /// bit of a code point being its low six bits of the word its next two bits select.</summary>
    /// <typeparam name="'P">The probe type.</typeparam>
    /// <param name="probe">The probe.</param>
    /// <param name="block">The block, the code points shifted right by eight.</param>
    /// <returns>Four words, never written again.</returns>
    let presenceWords<'P when 'P : struct and 'P :> IGlyphProbe> (probe: 'P) (block: int) =
        let words = Array.zeroCreate<uint64> 4
        for offset in 0 .. 255 do
            match probe.Maps (uint32 (block <<< 8 ||| offset)) with
            | true -> words[offset >>> 6] <- words[offset >>> 6] ||| (1UL <<< (offset &&& 63))
            | false -> ()
        words

    /// <summary>Concatenates two spans into one fresh array.</summary>
    /// <param name="first">The first span.</param>
    /// <param name="second">The second span.</param>
    /// <returns>The concatenation.</returns>
    let appendSpan (first: ReadOnlySpan<'T>) (second: ReadOnlySpan<'T>) : ImmutableArray<'T> =
        let target = Array.zeroCreate (first.Length + second.Length)
        let mutable index = 0
        while index < first.Length do
            target[index] <- first[index]
            index <- index + 1
        while index < target.Length do
            target[index] <- second[index - first.Length]
            index <- index + 1
        publish target

    /// <summary>Concatenates two immutable arrays.</summary>
    /// <param name="first">The first array.</param>
    /// <param name="second">The second array.</param>
    /// <returns>The concatenation.</returns>
    let concat (first: ImmutableArray<'T>) (second: ImmutableArray<'T>) =
        appendSpan (first.AsSpan()) (second.AsSpan())

    /// <summary>Appends one item into a fresh array.</summary>
    /// <param name="source">The source span.</param>
    /// <param name="item">The item.</param>
    /// <returns>The source followed by the item.</returns>
    let appendOne (source: ReadOnlySpan<'T>) (item: 'T) =
        let target = Array.zeroCreate (source.Length + 1)
        let mutable index = 0
        while index < source.Length do
            target[index] <- source[index]
            index <- index + 1
        target[source.Length] <- item
        publish target

    /// <summary>Resizes a span into a fresh array, zero-filling or truncating.</summary>
    /// <param name="source">The source span.</param>
    /// <param name="length">The result length.</param>
    /// <returns>The resized array.</returns>
    let resize (source: ReadOnlySpan<'T>) length =
        let target = Array.zeroCreate length
        let count = min source.Length length
        let mutable index = 0
        while index < count do
            target[index] <- source[index]
            index <- index + 1
        publish target

    /// <summary>Inserts a span at an index into a fresh array.</summary>
    /// <param name="target">The target span.</param>
    /// <param name="inserted">The inserted span.</param>
    /// <param name="at">The insertion index.</param>
    /// <returns>The target with the insertion.</returns>
    let insertAt (target: ReadOnlySpan<'T>) (inserted: ReadOnlySpan<'T>) at =
        let result = Array.zeroCreate (target.Length + inserted.Length)
        let mutable index = 0
        while index < at do
            result[index] <- target[index]
            index <- index + 1
        let mutable insertedIndex = 0
        while insertedIndex < inserted.Length do
            result[index] <- inserted[insertedIndex]
            index <- index + 1
            insertedIndex <- insertedIndex + 1
        while index < result.Length do
            result[index] <- target[index - inserted.Length]
            index <- index + 1
        publish result

    /// <summary>Reverses a range into a fresh array.</summary>
    /// <param name="source">The source span.</param>
    /// <param name="start">The first reversed index.</param>
    /// <param name="finish">The end reversed index.</param>
    /// <returns>The array with the range reversed.</returns>
    let reverseRange (source: ReadOnlySpan<'T>) start finish =
        let target = Array.zeroCreate source.Length
        let mutable index = 0
        while index < start do
            target[index] <- source[index]
            index <- index + 1
        let mutable reversed = finish - 1
        while index < finish do
            target[index] <- source[reversed]
            index <- index + 1
            reversed <- reversed - 1
        while index < target.Length do
            target[index] <- source[index]
            index <- index + 1
        publish target

    /// <summary>Reorders items to follow reversed equal-cluster groups.</summary>
    /// <typeparam name="'T">The item type.</typeparam>
    /// <param name="infos">The glyph infos, whose clusters define the groups.</param>
    /// <param name="items">The items parallel to the infos.</param>
    /// <returns>The reordered items.</returns>
    let private reverseClustersOf (infos: ReadOnlySpan<GlyphInfo>) (items: ReadOnlySpan<'T>) =
        let target = Array.zeroCreate items.Length
        let mutable output = 0
        let mutable finish = infos.Length
        while finish > 0 do
            let cluster = infos[finish - 1].Cluster
            let mutable start = finish - 1
            while start > 0 && infos[start - 1].Cluster = cluster do
                start <- start - 1
            let mutable index = start
            while index < finish do
                target[output] <- items[index]
                output <- output + 1
                index <- index + 1
            finish <- start
        publish target

    /// <summary>Reverses each equal-cluster group, then the whole run.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <returns>The reordered infos.</returns>
    let reverseClustersInfo (infos: ReadOnlySpan<GlyphInfo>) =
        reverseClustersOf infos infos

    /// <summary>Reorders positions to follow reversed equal-cluster groups.</summary>
    /// <param name="infos">The glyph infos, whose clusters define the groups.</param>
    /// <param name="positions">The positions.</param>
    /// <returns>The reordered positions.</returns>
    let reverseClustersPositions (infos: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) =
        reverseClustersOf infos positions

    /// <summary>Reorders properties to follow reversed equal-cluster groups.</summary>
    /// <param name="infos">The glyph infos, whose clusters define the groups.</param>
    /// <param name="props">The properties.</param>
    /// <returns>The reordered properties.</returns>
    let reverseClustersProps (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) =
        reverseClustersOf infos props

    /// <summary>Gathers ranges of a span into a fresh array, in order.</summary>
    /// <param name="source">The items.</param>
    /// <param name="ranges">The start and end of each range, inside the source.</param>
    /// <param name="total">The sum of the range lengths.</param>
    /// <returns>The gathered items.</returns>
    let gather (source: ReadOnlySpan<'T>) (ranges: ReadOnlySpan<struct (int * int)>) (total: int) =
        let result = Array.zeroCreate total
        let mutable written = 0
        let mutable index = 0
        while index < ranges.Length do
            let struct (start, finish) = ranges[index]
            source.Slice(start, finish - start).CopyTo(Span<'T>(result, written, finish - start))
            written <- written + (finish - start)
            index <- index + 1
        publish result

    /// <summary>Copies a span into an immutable array.</summary>
    /// <param name="source">The items.</param>
    /// <returns>The immutable copy.</returns>
    let ofSpan (source: ReadOnlySpan<'T>) : ImmutableArray<'T> =
        publish (source.ToArray())

    /// <summary>Copies a list into an immutable array.</summary>
    /// <param name="items">The items.</param>
    /// <returns>The immutable copy.</returns>
    let ofList (items: 'T list) : ImmutableArray<'T> =
        publish (List.toArray items)

    /// <summary>Maps an array, returning the input unchanged when no element changes.</summary>
    /// <param name="mapping">The mapping.</param>
    /// <param name="items">The items.</param>
    /// <returns>The input when every mapping equals its source, else the mapped copy.</returns>
    let mapIfChanged (mapping: 'T -> 'T) (items: ImmutableArray<'T>) : ImmutableArray<'T> when 'T :> IEquatable<'T> =
        let comparer = EqualityComparer<'T>.Default
        let source = items.AsSpan()
        let mutable changed = -1
        let mutable mapped = Unchecked.defaultof<'T>
        let mutable index = 0
        while index < source.Length && changed < 0 do
            let candidate = mapping source[index]
            match comparer.Equals(candidate, source[index]) with
            | true -> index <- index + 1
            | false ->
                changed <- index
                mapped <- candidate
        match changed < 0 with
        | true -> items
        | false ->
            let target = Array.zeroCreate source.Length
            let mutable i = 0
            while i < changed do
                target[i] <- source[i]
                i <- i + 1
            target[changed] <- mapped
            let mutable j = changed + 1
            while j < source.Length do
                target[j] <- mapping source[j]
                j <- j + 1
            publish target

    /// <summary>Maps a span in groups, one fresh array for the output.</summary>
    /// <typeparam name="'T">The element type.</typeparam>
    /// <typeparam name="'M">The mapper type.</typeparam>
    /// <param name="mapper">The group mapper.</param>
    /// <param name="items">The source items.</param>
    /// <returns>The mapped elements, as long as the source.</returns>
    let mapGroups<'T, 'M when 'M : struct and 'M :> IGroupMapper<'T>> (mapper: 'M) (items: ReadOnlySpan<'T>) : ImmutableArray<'T> =
        let target = Array.zeroCreate items.Length
        let mutable start = 0
        while start < items.Length do
            let finish = mapper.GroupEnd(items, start)
            let mutable index = start
            while index < finish do
                target[index] <- mapper.Map(items, start, finish, index)
                index <- index + 1
            start <- finish
        publish target

    /// <summary>Adds a closed interval to a descending disjoint range list.</summary>
    /// <param name="lo">The first value.</param>
    /// <param name="hi">The last value.</param>
    /// <param name="ranges">The descending ranges.</param>
    /// <returns>The merged descending ranges.</returns>
    [<TailCall>]
    let rec private addRangeDescending (lo) (hi) (ranges: (uint64 * uint64) list) =
        match ranges with
        | [] -> [ lo, hi ]
        | (first, last) :: rest ->
            match lo > last + 1UL, hi + 1UL < first with
            | true, _ -> (lo, hi) :: ranges
            | false, true -> (first, last) :: addRangeDescending lo hi rest
            | false, false -> addRangeDescending (min lo first) (max hi last) rest

    /// <summary>Subtracts a closed interval from a descending disjoint range list.</summary>
    /// <param name="lo">The first value.</param>
    /// <param name="hi">The last value.</param>
    /// <param name="ranges">The descending ranges.</param>
    /// <param name="acc">The reversed result so far.</param>
    /// <returns>The remaining descending ranges.</returns>
    [<TailCall>]
    let rec private deleteRangeDescending (lo) (hi) (ranges: (uint64 * uint64) list) (acc: (uint64 * uint64) list) =
        match ranges with
        | [] -> List.rev acc
        | (first, last) :: rest ->
            match last < lo, first > hi with
            | true, _ | _, true -> deleteRangeDescending lo hi rest ((first, last) :: acc)
            | _ ->
                let acc = match last > hi with | true -> (hi + 1UL, last) :: acc | false -> acc
                let acc = match first < lo with | true -> (first, lo - 1UL) :: acc | false -> acc
                deleteRangeDescending lo hi rest acc

    /// <summary>Applies set operations in order, ignoring upstream's invalid boundaries.</summary>
    /// <param name="ops">The operations.</param>
    /// <param name="index">The next operation.</param>
    /// <param name="ranges">The descending ranges so far.</param>
    /// <returns>The descending ranges.</returns>
    [<TailCall>]
    let rec private applyOps (ops: ReadOnlySpan<SetOp>) (index) (ranges: (uint64 * uint64) list) =
        match index >= ops.Length with
        | true -> ranges
        | false ->
            let next =
                match ops[index] with
                | Add (lo, hi) when lo <= hi && lo <> UInt32.MaxValue && hi <> UInt32.MaxValue ->
                    addRangeDescending (uint64 lo) (uint64 hi) ranges
                | Add _ -> ranges
                | Delete (lo, hi) when lo <= hi && lo <> UInt32.MaxValue ->
                    let last = match hi with | UInt32.MaxValue -> UInt32.MaxValue - 1u | _ -> hi
                    deleteRangeDescending (uint64 lo) (uint64 last) ranges []
                | Delete _ -> ranges
            applyOps ops (index + 1) next

    /// <summary>Whether no operation of a sequence is a delete.</summary>
    /// <param name="ops">The operations.</param>
    /// <param name="index">The next operation.</param>
    /// <returns>True when every operation adds.</returns>
    [<TailCall>]
    let rec private onlyAdds (ops: ReadOnlySpan<SetOp>) (index) =
        match index >= ops.Length with
        | true -> true
        | false ->
            match ops[index] with
            | Add _ -> onlyAdds ops (index + 1)
            | Delete _ -> false

    /// <summary>The ranges a sequence of adds makes, sorted, disjoint and with adjacent runs merged: the same set the list
    /// walk builds, by sorting the valid intervals once.</summary>
    /// <param name="ops">The operations, all adds.</param>
    /// <returns>The final ranges.</returns>
    let private unionOfAdds (ops: ReadOnlySpan<SetOp>) =
        let valid = ops.Length
        let found = Array.zeroCreate<CodepointRange> valid
        let mutable count = 0
        for index in 0 .. valid - 1 do
            match ops[index] with
            | Add (lo, hi) when lo <= hi && lo <> UInt32.MaxValue && hi <> UInt32.MaxValue ->
                found[count] <- { First = lo; Last = hi }
                count <- count + 1
            | Add _ | Delete _ -> ()
        let keys = Array.zeroCreate<uint32> count
        for index in 0 .. count - 1 do
            keys[index] <- found[index].First
        Array.Sort(keys, found, 0, count)
        let merged = Array.zeroCreate<CodepointRange> count
        let mutable length = 0
        for index in 0 .. count - 1 do
            let next = found[index]
            match length > 0 && uint64 next.First <= uint64 merged[length - 1].Last + 1UL with
            | true -> merged[length - 1] <- { First = merged[length - 1].First; Last = max merged[length - 1].Last next.Last }
            | false ->
                merged[length] <- next
                length <- length + 1
        publish (Array.sub merged 0 length)

    /// <summary>Applies range operations and returns the sorted disjoint ranges.</summary>
    /// <param name="ops">The operations, in upstream order.</param>
    /// <returns>The final ranges.</returns>
    let rangeSet (ops: ReadOnlySpan<SetOp>) : ImmutableArray<CodepointRange> =
        match onlyAdds ops 0 with
        | true -> unionOfAdds ops
        | false ->
            applyOps ops 0 []
            |> List.rev
            |> List.map (fun (first, last) -> { First = uint32 first; Last = uint32 last })
            |> ofList

    /// <summary>Fills a map from pairs, later pairs overriding earlier ones.</summary>
    /// <param name="pairs">The pairs.</param>
    /// <param name="index">The next pair.</param>
    /// <param name="map">The map.</param>
    [<TailCall>]
    let rec private fillMap (pairs: ReadOnlySpan<struct (uint32 * uint32)>) (index) (map: Dictionary<uint32, uint32>) =
        match index < pairs.Length with
        | false -> ()
        | true ->
            let struct (key, value) = pairs[index]
            map[key] <- value
            fillMap pairs (index + 1) map

    /// <summary>Builds a sorted map from pairs, the last value of a key winning.</summary>
    /// <param name="pairs">The pairs.</param>
    /// <returns>The sorted key and value pairs.</returns>
    let sortedMap (pairs: ReadOnlySpan<struct (uint32 * uint32)>) : ImmutableArray<struct (uint32 * uint32)> =
        let map = Dictionary<uint32, uint32>()
        fillMap pairs 0 map
        let keys = map.Keys |> Seq.toArray |> Array.sort
        publish (Array.map (fun key -> struct (key, map[key])) keys)

    /// <summary>Returns the bit length of a positive integer, as <c>hb_bit_storage</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The number of significant bits.</returns>
    let rec private bitStorage (value) =
        match value <= 0 with
        | true -> 0
        | false -> 1 + bitStorage (value >>> 1)

    /// <summary>Sifts one heap node down, <c>hb_heapsort_sift_down</c>.</summary>
    /// <param name="items">The array.</param>
    /// <param name="baseIndex">The first index of the heap.</param>
    /// <param name="root">The heap-relative root index.</param>
    /// <param name="endIndex">The heap-relative end.</param>
    /// <param name="comparer">The comparer.</param>
    let private siftDown<'C when 'C : struct and 'C :> IIndexComparer>
            (items: uint16 array) (baseIndex) (root) (endIndex) (comparer: 'C) =
        let mutable rootIndex = root
        let mutable running = true
        while running do
            let child = 2 * rootIndex + 1
            match child < endIndex with
            | false -> running <- false
            | true ->
                let childIndex =
                    match child + 1 < endIndex && comparer.Compare(int items[baseIndex + child], int items[baseIndex + child + 1]) < 0 with
                    | true -> child + 1
                    | false -> child
                match comparer.Compare(int items[baseIndex + rootIndex], int items[baseIndex + childIndex]) >= 0 with
                | true -> running <- false
                | false ->
                    let temporary = items[baseIndex + rootIndex]
                    items[baseIndex + rootIndex] <- items[baseIndex + childIndex]
                    items[baseIndex + childIndex] <- temporary
                    rootIndex <- childIndex

    /// <summary>Heapsorts a range, <c>hb_heapsort_inline</c>.</summary>
    /// <param name="items">The array.</param>
    /// <param name="baseIndex">The first index.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="comparer">The comparer.</param>
    let private heapSort<'C when 'C : struct and 'C :> IIndexComparer> (items: uint16 array) (baseIndex) (count) (comparer: 'C) =
        match count < 2 with
        | true -> ()
        | false ->
            let mutable root = count / 2
            while root > 0 do
                root <- root - 1
                siftDown items baseIndex root count comparer
            let mutable endIndex = count
            while endIndex > 1 do
                endIndex <- endIndex - 1
                let temporary = items[baseIndex]
                items[baseIndex] <- items[baseIndex + endIndex]
                items[baseIndex + endIndex] <- temporary
                siftDown items baseIndex 0 endIndex comparer

    /// <summary>Swaps two indices in place.</summary>
    /// <param name="items">The array.</param>
    /// <param name="a">The first index.</param>
    /// <param name="b">The second index.</param>
    let private swapAt (items: uint16 array) (a) (b) =
        let temporary = items[a]
        items[a] <- items[b]
        items[b] <- temporary

    /// <summary>Heapsorts a subrange, exposed to the tests for the base-index regression.</summary>
    /// <param name="items">The array.</param>
    /// <param name="baseIndex">The first index.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="comparer">The comparer.</param>
    /// <returns>Unit.</returns>
    let internal heapSortRange<'C when 'C : struct and 'C :> IIndexComparer>
            (items: uint16 array) (baseIndex) (count) (comparer: 'C) =
        heapSort items baseIndex count comparer

    /// <summary>Orders the first two median candidates.</summary>
    /// <param name="items">The array.</param>
    /// <param name="first">The first index.</param>
    /// <param name="second">The second index.</param>
    /// <param name="comparer">The comparer.</param>
    let private orderFirstTwo<'C when 'C : struct and 'C :> IIndexComparer> (items: uint16 array) (first) (second) (comparer: 'C) =
        match comparer.Compare(int items[first], int items[second]) > 0 with
        | true -> swapAt items first second
        | false -> ()

    /// <summary>Orders the last two median candidates.</summary>
    /// <param name="items">The array.</param>
    /// <param name="first">The first index.</param>
    /// <param name="second">The second index.</param>
    /// <param name="third">The third index.</param>
    /// <param name="comparer">The comparer.</param>
    let private orderLastTwo<'C when 'C : struct and 'C :> IIndexComparer> (items: uint16 array) (first) (second) (third) (comparer: 'C) =
        match comparer.Compare(int items[second], int items[third]) > 0 with
        | false -> ()
        | true ->
            swapAt items second third
            match comparer.Compare(int items[first], int items[second]) > 0 with
            | true -> swapAt items first second
            | false -> ()

    /// <summary>Partitions a range recursively, <c>hb_qsort_loop</c>.</summary>
    /// <param name="items">The array.</param>
    /// <param name="baseIndex">The first index.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="depth">The remaining introsort depth.</param>
    /// <param name="comparer">The comparer.</param>
    let rec private quickSortLoop<'C when 'C : struct and 'C :> IIndexComparer> (items: uint16 array) (baseIndex) (count) (depth) (comparer: 'C) =
        match count > 24 with
        | false -> ()
        | true ->
            match depth = 0 with
            | true -> heapSort items baseIndex count comparer
            | false ->
                let last = baseIndex + count - 1
                let mid = baseIndex + count / 2
                orderFirstTwo items baseIndex mid comparer
                orderLastTwo items baseIndex mid last comparer
                swapAt items mid (last - 1)
                let pivot = items[last - 1]
                let mutable i = baseIndex
                let mutable j = last - 1
                let mutable partitioning = true
                while partitioning do
                    i <- i + 1
                    while comparer.Compare(int items[i], int pivot) < 0 do
                        i <- i + 1
                    j <- j - 1
                    while comparer.Compare(int items[j], int pivot) > 0 do
                        j <- j - 1
                    match i >= j with
                    | true -> partitioning <- false
                    | false ->
                        swapAt items i j
                swapAt items i (last - 1)
                let left = i - baseIndex
                let right = count - left - 1
                match left < right with
                | true ->
                    quickSortLoop items baseIndex left (depth - 1) comparer
                    quickSortLoop items (i + 1) right (depth - 1) comparer
                | false ->
                    quickSortLoop items (i + 1) right (depth - 1) comparer
                    quickSortLoop items baseIndex left (depth - 1) comparer

    /// <summary>Sorts indices with the deterministic introsort, <c>hb_qsort_inline</c>.</summary>
    /// <param name="count">The number of indices.</param>
    /// <param name="comparer">The comparer.</param>
    /// <returns>The sorted indices.</returns>
    let sortInPlaceIntro<'C when 'C : struct and 'C :> IIndexComparer> (count) (comparer: 'C) : ImmutableArray<uint16> =
        let items = Array.zeroCreate count
        let mutable index = 0
        while index < count do
            items[index] <- uint16 index
            index <- index + 1
        quickSortLoop items 0 count (2 * bitStorage count) comparer
        let mutable pi = 1
        while pi < count do
            let mutable pj = pi
            while pj > 0 && comparer.Compare(int items[pj - 1], int items[pj]) > 0 do
                let temporary = items[pj - 1]
                items[pj - 1] <- items[pj]
                items[pj] <- temporary
                pj <- pj - 1
            pi <- pi + 1
        publish items

    /// <summary>Publishes the first <paramref name="count"/> items of an array.</summary>
    /// <param name="items">The array.</param>
    /// <param name="count">The number of leading items.</param>
    /// <returns>The immutable view.</returns>
    let private publishRange (items: 'T array) (count: int) : ImmutableArray<'T> =
        match count = items.Length with
        | true -> publish items
        | false -> publish (items.AsSpan(0, count).ToArray())

    /// <summary>The unsafe-to-break glyph flag, <c>HB_GLYPH_FLAG_UNSAFE_TO_BREAK</c>.</summary>
    [<Literal>]
    let private GlyphFlagUnsafeToBreak = 1u

    /// <summary>The unsafe-to-concat glyph flag, <c>HB_GLYPH_FLAG_UNSAFE_TO_CONCAT</c>.</summary>
    [<Literal>]
    let private GlyphFlagUnsafeToConcat = 2u

    /// <summary>The safe-to-insert-tatweel glyph flag, <c>HB_GLYPH_FLAG_SAFE_TO_INSERT_TATWEEL</c>.</summary>
    [<Literal>]
    let private GlyphFlagSafeToInsertTatweel = 4u

    /// <summary>All defined glyph flags, <c>HB_GLYPH_FLAG_DEFINED</c>.</summary>
    [<Literal>]
    let private GlyphFlagDefined = 7u

    /// <summary>The growth increment of the out-buffer, the <c>+ 32</c> in <c>enlarge</c>.</summary>
    [<Literal>]
    let private BufferGrowIncrement = 32

    /// <summary>The glyph-flag range limit above which a flag request is ignored.</summary>
    [<Literal>]
    let private MaxGlyphFlagRange = 255

    /// <summary>The nesting budget a GSUB lookup starts with, <c>HB_MAX_NESTING_LEVEL</c> (src/hb-limits.hh:53).</summary>
    [<Literal>]
    let private MaxNestingLevel = 64

    /// <summary>The idle default maximum buffer length, <c>HB_BUFFER_MAX_LEN_DEFAULT</c>.</summary>
    [<Literal>]
    let private DefaultMaxLen = 0x3FFFFFFF

    /// <summary>The machine state of one shaping pass over the out-buffer. A struct held in a local and passed by
    /// reference inside <see cref="Sealed"/>, so a step copies no machine state and a no-op pass allocates nothing.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type internal Machine =
        { mutable Info: GlyphInfo array
          mutable OutInfo: GlyphInfo array
          mutable Props: GlyphProps array
          mutable OutProps: GlyphProps array
          mutable Positions: GlyphPosition array
          SourceInfos: ImmutableArray<GlyphInfo>
          SourceProps: ImmutableArray<GlyphProps>
          SourcePositions: ImmutableArray<GlyphPosition>
          mutable Materialized: bool
          mutable PositionsMaterialized: bool
          mutable OutIsSeparate: bool
          mutable Idx: int
          mutable Len: int
          mutable OutLen: int
          mutable HaveOutput: bool
          mutable HavePositions: bool
          mutable MaxOps: int
          mutable Successful: bool
          mutable Passive: bool
          MaxLen: int
          ClusterLevel: ClusterLevel
          Flags: BufferFlags
          mutable Serial: byte
          mutable RandomState: uint32
          mutable ScratchFlags: ScratchFlags
          mutable Digest: Digest
          mutable DigestStale: bool
          mutable Candidates: uint64
          mutable LookupPassive: bool
          Normalise: bool
          Exact: bool
          mutable Allocated: int
          mutable Nesting: int
          InitialMaxOps: int
          mutable Shifted: bool  // the input was shifted forward, whose charge depends on the length of what follows
          mutable Grew: bool  // the output outgrew the input, the condition of a shift forward
          mutable Watch: ILookupWatch | null }  // sees the run after each lookup, null on every route but the learning of syllables

    /// <summary>The logical out-buffer array, <c>out_info</c> (aliases <c>info</c> until it outgrows it).</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The out-buffer array.</returns>
    let private outInfoArray (machine: inref<Machine>) : GlyphInfo array =
        match machine.OutIsSeparate with
        | true -> machine.OutInfo
        | false -> machine.Info

    /// <summary>The properties array parallel to <see cref="outInfoArray"/>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The out-buffer properties array.</returns>
    let private outPropsArray (machine: inref<Machine>) : GlyphProps array =
        match machine.OutIsSeparate with
        | true -> machine.OutProps
        | false -> machine.Props

    /// <summary>Whether a cluster level merges into monotone order, <c>HB_BUFFER_CLUSTER_LEVEL_IS_MONOTONE</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>True for the two monotone levels.</returns>
    let private isMonotone (level: ClusterLevel) =
        match level with
        | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters -> true
        | ClusterLevel.Characters | ClusterLevel.Graphemes -> false

    /// <summary>Whether a cluster level groups by graphemes, <c>HB_BUFFER_CLUSTER_LEVEL_IS_GRAPHEMES</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>True for the two grapheme levels.</returns>
    let private isGraphemes (level: ClusterLevel) =
        match level with
        | ClusterLevel.MonotoneGraphemes | ClusterLevel.Graphemes -> true
        | ClusterLevel.MonotoneCharacters | ClusterLevel.Characters -> false

    /// <summary>Whether a cluster level is exactly <c>HB_BUFFER_CLUSTER_LEVEL_CHARACTERS</c>, as the flag functions test.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>True only for the characters level.</returns>
    let private isExactlyCharacters (level: ClusterLevel) =
        match level with
        | ClusterLevel.Characters -> true
        | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters | ClusterLevel.Graphemes -> false

    /// <summary>Grows an allocation until it is at least <paramref name="size"/>, <c>enlarge</c>.</summary>
    /// <param name="size">The required size.</param>
    /// <param name="allocated">The current allocation.</param>
    /// <returns>The new allocation.</returns>
    [<TailCall>]
    let rec private growAllocated (size: int) (allocated: int) : int =
        match size < allocated with
        | true -> allocated
        | false -> growAllocated size (allocated + (allocated >>> 1) + BufferGrowIncrement)

    /// <summary>Copies the input glyphs and properties into fresh working arrays on the first edit that changes the run.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with its own glyph and properties arrays.</returns>
    let private materialize (machine: byref<Machine>) : unit =
        match machine.Materialized with
        | true -> ()
        | false ->
            let allocated = match machine.Len = 0 with | true -> 0 | false -> growAllocated machine.Len 0
            let info = match machine.Exact with | true -> GC.AllocateUninitializedArray<GlyphInfo> machine.Len | false -> Array.zeroCreate allocated
            let props = match machine.Exact with | true -> GC.AllocateUninitializedArray<GlyphProps> machine.Len | false -> Array.zeroCreate allocated
            match machine.SourceInfos.IsEmpty with
            | true -> ()
            | false -> machine.SourceInfos.AsSpan(0, machine.Len).CopyTo(Span<GlyphInfo>(info))
            match machine.SourceProps.IsEmpty with
            | true -> ()
            | false -> machine.SourceProps.AsSpan(0, machine.Len).CopyTo(Span<GlyphProps>(props))
            machine.Info <- info
            machine.Props <- props
            machine.Allocated <- allocated
            machine.Materialized <- true

    /// <summary>Gives the machine its own writable arrays and records that the run is no longer the input.</summary>
    /// <param name="machine">The machine.</param>
    let private touch (machine: byref<Machine>) =
        materialize &machine
        machine.Passive <- false
        machine.LookupPassive <- false

    /// <summary>Copies the input positions into a fresh array on the first position edit.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with its own positions array.</returns>
    let private materializePositions (machine: byref<Machine>) : unit =
        match machine.PositionsMaterialized with
        | true -> ()
        | false ->
            let positions = Array.zeroCreate machine.Info.Length
            match machine.HavePositions && not machine.SourcePositions.IsEmpty with
            | true -> machine.SourcePositions.AsSpan(0, machine.Len).CopyTo(Span<GlyphPosition>(positions))
            | false -> ()
            machine.Positions <- positions
            machine.PositionsMaterialized <- true

    /// <summary>Allocates the separate out-buffer arrays when the output outgrows the input, <c>make_room_for</c>'s copy.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with out-buffer arrays large enough for the current input.</returns>
    let private ensureOutArrays (machine: byref<Machine>) : unit =
        match machine.OutInfo.Length >= machine.Info.Length with
        | true -> ()
        | false ->
            machine.OutInfo <- Array.zeroCreate machine.Info.Length
            machine.OutProps <- Array.zeroCreate machine.Info.Length

    /// <summary>Reallocates the machine arrays to fit at least <paramref name="size"/> items.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="size">The required size.</param>
    /// <returns>The machine with larger arrays, or a failed machine when the size exceeds <c>MaxLen</c>.</returns>
    let private enlarge (machine: byref<Machine>) (size: int) : unit =
        match size > machine.MaxLen with
        | true -> machine.Successful <- false
        | false ->
            match machine.Successful with
            | false -> ()
            | true ->
                let newAllocated = growAllocated size machine.Allocated
                let capacity = max (min newAllocated (max size (machine.Info.Length * 2))) (max machine.Info.Length machine.OutInfo.Length)
                let info = Array.zeroCreate capacity
                let outInfo = match machine.OutIsSeparate with | true -> Array.zeroCreate capacity | false -> machine.OutInfo
                let props = Array.zeroCreate capacity
                let outProps = match machine.OutIsSeparate with | true -> Array.zeroCreate capacity | false -> machine.OutProps
                let positions = match machine.Positions.Length with | 0 -> Array.zeroCreate 0 | _ -> Array.zeroCreate capacity
                Array.Copy(machine.Info, info, machine.Info.Length)
                match machine.OutIsSeparate with
                | true ->
                    Array.Copy(machine.OutInfo, outInfo, machine.OutInfo.Length)
                    Array.Copy(machine.OutProps, outProps, machine.OutProps.Length)
                | false -> ()
                Array.Copy(machine.Props, props, machine.Props.Length)
                Array.Copy(machine.Positions, positions, machine.Positions.Length)
                machine.Info <- info
                machine.OutInfo <- outInfo
                machine.Props <- props
                machine.OutProps <- outProps
                machine.Positions <- positions
                machine.Allocated <- newAllocated

    /// <summary>Reallocates the arrays of an exact machine, when a lookup writes beyond the run's length but within the allocation a buffer
    /// of that length has: to twice their length or the size asked for, whichever is more, and never beyond the logical allocation, which
    /// alone decides when the length limit is checked, so a short run does not take the thirty-two glyphs of room that a buffer has.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="size">The size the caller needs, below the logical allocation.</param>
    let private growPhysical (machine: byref<Machine>) (size: int) =
        let capacity = max (min machine.Allocated (max size (machine.Info.Length * 2))) machine.OutInfo.Length
        let info = Array.zeroCreate capacity
        let outInfo = match machine.OutIsSeparate with | true -> Array.zeroCreate capacity | false -> machine.OutInfo
        let props = Array.zeroCreate capacity
        let outProps = match machine.OutIsSeparate with | true -> Array.zeroCreate capacity | false -> machine.OutProps
        let positions = match machine.Positions.Length with | 0 -> Array.zeroCreate 0 | _ -> Array.zeroCreate capacity
        Array.Copy(machine.Info, info, machine.Info.Length)
        match machine.OutIsSeparate with
        | true ->
            Array.Copy(machine.OutInfo, outInfo, machine.OutInfo.Length)
            Array.Copy(machine.OutProps, outProps, machine.OutProps.Length)
        | false -> ()
        Array.Copy(machine.Props, props, machine.Props.Length)
        Array.Copy(machine.Positions, positions, machine.Positions.Length)
        machine.Info <- info
        machine.OutInfo <- outInfo
        machine.Props <- props
        machine.OutProps <- outProps
        machine.Positions <- positions

    /// <summary>Ensures the arrays hold at least <paramref name="size"/> items, <c>ensure</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="size">The required size.</param>
    /// <returns>The machine, enlarged when needed, and whether the size is now available.</returns>
    let private ensure (machine: byref<Machine>) (size: int) : bool =
        match size = 0 || size < machine.Allocated with
        | true ->
            match size > machine.Info.Length with
            | true -> growPhysical &machine size
            | false -> ()
            true
        | false ->
            enlarge &machine size
            machine.Successful

    /// <summary>Ensures room for an output, switching the out-buffer to a separate array when needed, <c>make_room_for</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="numIn">The number of input glyphs consumed.</param>
    /// <param name="numOut">The number of output glyphs produced.</param>
    /// <returns>The machine and whether the room is available.</returns>
    let private makeRoomFor (machine: byref<Machine>) (numIn: int) (numOut: int) : bool =
        match ensure &machine (machine.OutLen + numOut) with
        | false -> false
        | true ->
            match (not machine.OutIsSeparate) && machine.OutLen + numOut > machine.Idx + numIn with
            | false -> true
            | true ->
                ensureOutArrays &machine
                Array.Copy(machine.Info, 0, machine.OutInfo, 0, machine.OutLen)
                Array.Copy(machine.Props, 0, machine.OutProps, 0, machine.OutLen)
                machine.OutIsSeparate <- true
                machine.Grew <- true
                true

    /// <summary>Assigns a cluster to a glyph, clearing its flags when the cluster changes, <c>set_cluster</c>.</summary>
    /// <param name="info">The glyph.</param>
    /// <param name="cluster">The cluster.</param>
    /// <param name="mask">The mask whose defined bits replace the flags.</param>
    /// <returns>The glyph with the cluster.</returns>
    let private setClusterValue (info: GlyphInfo) (cluster: uint32) (mask: uint32) : GlyphInfo =
        match info.Cluster = cluster with
        | true -> info
        | false -> { info with Mask = (info.Mask &&& ~~~GlyphFlagDefined) ||| (mask &&& GlyphFlagDefined); Cluster = cluster }

    /// <summary>Copies the current glyph, or the previous output glyph at end of input, <c>cur</c>/<c>prev</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The glyph and its properties.</returns>
    let private curOrPrev (machine: inref<Machine>) : struct (GlyphInfo * GlyphProps) =
        match machine.Idx < machine.Len with
        | true -> struct (machine.Info[machine.Idx], machine.Props[machine.Idx])
        | false ->
            let outInfo = outInfoArray &machine
            let outProps = outPropsArray &machine
            let index = match machine.OutLen > 0 with | true -> machine.OutLen - 1 | false -> 0
            struct (outInfo[index], outProps[index])

    /// <summary>Finds the smallest cluster in a range, <c>_infos_find_min_cluster</c>.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="cluster">The starting minimum.</param>
    /// <param name="level">The cluster level.</param>
    /// <returns>The minimum cluster.</returns>
    let private infosFindMinCluster (infos: GlyphInfo array) (start: int) (finish: int) (cluster: uint32) (level: ClusterLevel) : uint32 =
        match start = finish with
        | true -> cluster
        | false ->
            match isExactlyCharacters level with
            | true ->
                let mutable minimum = cluster
                let mutable i = start
                while i < finish do
                    minimum <- min minimum infos[i].Cluster
                    i <- i + 1
                minimum
            | false -> min cluster (min infos[start].Cluster infos[finish - 1].Cluster)

    /// <summary>Ors a mask into the glyphs of a range whose cluster differs, <c>_infos_set_glyph_flags</c>.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="cluster">The cluster to keep unset.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the machine has not failed.</param>
    /// <returns>The updated budget and success flag.</returns>
    let private infosSetGlyphFlags (infos: GlyphInfo array) (start: int) (finish: int) (cluster: uint32) (mask: uint32) (level: ClusterLevel) (maxOps: int) (successful: bool) : struct (int * bool) =
        match start = finish with
        | true -> struct (maxOps, successful)
        | false ->
            let maxOps = maxOps - (finish - start)
            let successful = match maxOps < 0 with | true -> false | false -> successful
            let clusterFirst = infos[start].Cluster
            let clusterLast = infos[finish - 1].Cluster
            match isExactlyCharacters level || (cluster <> clusterFirst && cluster <> clusterLast) with
            | true ->
                let mutable i = start
                while i < finish do
                    match cluster = infos[i].Cluster with
                    | false -> infos[i] <- { infos[i] with Mask = infos[i].Mask ||| mask }
                    | true -> ()
                    i <- i + 1
                struct (maxOps, successful)
            | false ->
                match cluster = clusterFirst with
                | true ->
                    let mutable i = finish
                    while start < i && infos[i - 1].Cluster <> clusterFirst do
                        i <- i - 1
                        infos[i] <- { infos[i] with Mask = infos[i].Mask ||| mask }
                    struct (maxOps, successful)
                | false ->
                    let mutable i = start
                    while i < finish && infos[i].Cluster <> clusterLast do
                        infos[i] <- { infos[i] with Mask = infos[i].Mask ||| mask }
                        i <- i + 1
                    struct (maxOps, successful)

    /// <summary>Applies a glyph-flag request to the input or output buffer, <c>_set_glyph_flags_impl</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="interior">Whether the minimum cluster is skipped.</param>
    /// <param name="fromOutBuffer">Whether the range is in the output buffer.</param>
    /// <returns>The machine with the flags applied.</returns>
    let private setGlyphFlagsImpl (machine: byref<Machine>) (mask: uint32) (start: int) (finish: int) (interior: bool) (fromOutBuffer: bool) : unit =
        match (not fromOutBuffer) || (not machine.HaveOutput) with
        | true ->
            match interior with
            | false ->
                let mutable i = start
                while i < finish do
                    machine.Info[i] <- { machine.Info[i] with Mask = machine.Info[i].Mask ||| mask }
                    i <- i + 1
            | true ->
                let cluster = infosFindMinCluster machine.Info start finish UInt32.MaxValue machine.ClusterLevel
                let struct (maxOps, successful) = infosSetGlyphFlags machine.Info start finish cluster mask machine.ClusterLevel machine.MaxOps machine.Successful
                machine.MaxOps <- maxOps
                machine.Successful <- successful
        | false ->
            let outInfo = outInfoArray &machine
            match interior with
            | false ->
                let mutable i = start
                while i < machine.OutLen do
                    outInfo[i] <- { outInfo[i] with Mask = outInfo[i].Mask ||| mask }
                    i <- i + 1
                let mutable j = machine.Idx
                while j < finish do
                    machine.Info[j] <- { machine.Info[j] with Mask = machine.Info[j].Mask ||| mask }
                    j <- j + 1
            | true ->
                let cluster = infosFindMinCluster machine.Info machine.Idx finish UInt32.MaxValue machine.ClusterLevel
                let cluster = infosFindMinCluster outInfo start machine.OutLen cluster machine.ClusterLevel
                let struct (maxOps, successful) = infosSetGlyphFlags outInfo start machine.OutLen cluster mask machine.ClusterLevel machine.MaxOps machine.Successful
                let struct (maxOps2, successful2) = infosSetGlyphFlags machine.Info machine.Idx finish cluster mask machine.ClusterLevel maxOps successful
                machine.MaxOps <- maxOps2
                machine.Successful <- successful2

    /// <summary>Ors a glyph flag into a range, honouring the range limit, <c>_set_glyph_flags</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index, or -1 for the end of the buffer.</param>
    /// <param name="interior">Whether the minimum cluster is skipped.</param>
    /// <param name="fromOutBuffer">Whether the range is in the output buffer.</param>
    /// <returns>The machine with the flags applied.</returns>
    let private setGlyphFlags (machine: byref<Machine>) (mask: uint32) (start: int) (finish: int) (interior: bool) (fromOutBuffer: bool) : unit =
        match finish <> -1 && (uint32 finish - uint32 start) > uint32 MaxGlyphFlagRange with
        | true -> ()
        | false ->
            let finish = match finish with | -1 -> machine.Len | bounded -> min bounded machine.Len
            match interior && (not fromOutBuffer) && finish - start < 2 with
            | true -> ()
            | false ->
                touch &machine
                setGlyphFlagsImpl &machine mask start finish interior fromOutBuffer

    /// <summary>Marks a range unsafe to break and concat, <c>unsafe_to_break</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the flags applied.</returns>
    let private unsafeToBreak (machine: byref<Machine>) (start: int) (finish: int) : unit =
        setGlyphFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) start finish true false

    /// <summary>Marks a range safe to insert a tatweel, <c>safe_to_insert_tatweel</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the flags applied.</returns>
    let private safeToInsertTatweel (machine: byref<Machine>) (start: int) (finish: int) : unit =
        match (machine.Flags &&& BufferFlags.ProduceSafeToInsertTatweel) = BufferFlags.Default with
        | true -> unsafeToBreak &machine start finish
        | false -> setGlyphFlags &machine GlyphFlagSafeToInsertTatweel start finish true false

    /// <summary>Marks a range unsafe to concat, <c>unsafe_to_concat</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the flags applied.</returns>
    let private unsafeToConcat (machine: byref<Machine>) (start: int) (finish: int) : unit =
        match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
        | true -> ()
        | false -> setGlyphFlags &machine GlyphFlagUnsafeToConcat start finish false false

    /// <summary>Marks an output-buffer range unsafe to break, <c>unsafe_to_break_from_outbuffer</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the flags applied.</returns>
    let private unsafeToBreakFromOutbuffer (machine: byref<Machine>) (start: int) (finish: int) : unit =
        setGlyphFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) start finish true true

    /// <summary>Marks an output-buffer range unsafe to concat, <c>unsafe_to_concat_from_outbuffer</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the flags applied.</returns>
    let private unsafeToConcatFromOutbuffer (machine: byref<Machine>) (start: int) (finish: int) : unit =
        match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
        | true -> ()
        | false -> setGlyphFlags &machine GlyphFlagUnsafeToConcat start finish false true

    /// <summary>Copies glyphs from input to output, advancing the cursor, <c>next_glyphs</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="count">The number of glyphs.</param>
    /// <returns>The machine with the glyphs moved.</returns>
    let private nextGlyphs (machine: byref<Machine>) (count: int) : unit =
        match machine.HaveOutput with
        | false -> machine.Idx <- machine.Idx + count
        | true ->
            match machine.OutIsSeparate || machine.OutLen <> machine.Idx with
            | false ->
                machine.Idx <- machine.Idx + count
                machine.OutLen <- machine.OutLen + count
            | true ->
                match ensure &machine (machine.OutLen + count) with
                | false -> ()
                | true ->
                    let outInfo = outInfoArray &machine
                    let outProps = outPropsArray &machine
                    Array.Copy(machine.Info, machine.Idx, outInfo, machine.OutLen, count)
                    Array.Copy(machine.Props, machine.Idx, outProps, machine.OutLen, count)
                    machine.Idx <- machine.Idx + count
                    machine.OutLen <- machine.OutLen + count

    /// <summary>Merges clusters over a range, <c>merge_clusters_impl</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="startIn">The first index.</param>
    /// <param name="finishIn">The end index.</param>
    /// <returns>The machine with the clusters merged.</returns>
    let private mergeClustersImpl (machine: byref<Machine>) (startIn: int) (finishIn: int) : unit =
        let mutable start = startIn
        let mutable finish = finishIn
        let mutable maxOps = machine.MaxOps
        let mutable successful = machine.Successful
        let mutable cluster = machine.Info[start].Cluster
        let mutable i = start + 1
        while i < finish do
            cluster <- min cluster machine.Info[i].Cluster
            i <- i + 1
        match successful with
        | true ->
            match cluster = machine.Info[finish - 1].Cluster with
            | false ->
                while finish < machine.Len && machine.Info[finish - 1].Cluster = machine.Info[finish].Cluster do
                    finish <- finish + 1
            | true -> ()
            match cluster = machine.Info[start].Cluster with
            | false ->
                while machine.Idx < start && machine.Info[start - 1].Cluster = machine.Info[start].Cluster do
                    start <- start - 1
            | true -> ()
            match machine.Idx = start && machine.Info[start].Cluster <> cluster with
            | true ->
                let outInfo = outInfoArray &machine
                let mutable j = machine.OutLen
                while j > 0 && outInfo[j - 1].Cluster = machine.Info[start].Cluster do
                    j <- j - 1
                    outInfo[j] <- setClusterValue outInfo[j] cluster 0u
                maxOps <- maxOps - (machine.OutLen - j)
            | false -> ()
        | false -> ()
        maxOps <- maxOps - (finish - start)
        match maxOps < 0 with
        | true -> successful <- false
        | false -> ()
        let mutable k = start
        while k < finish do
            machine.Info[k] <- setClusterValue machine.Info[k] cluster 0u
            k <- k + 1
        machine.MaxOps <- maxOps
        machine.Successful <- successful

    /// <summary>Merges clusters in monotone levels, <c>merge_clusters</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the clusters merged.</returns>
    let private mergeClusters (machine: byref<Machine>) (start: int) (finish: int) : unit =
        match finish - start < 2 with
        | true -> ()
        | false ->
            match isMonotone machine.ClusterLevel with
            | false -> unsafeToBreak &machine start finish
            | true -> mergeClustersImpl &machine start finish

    /// <summary>Merges grapheme clusters in grapheme levels, <c>merge_grapheme_clusters</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the clusters merged.</returns>
    let private mergeGraphemeClusters (machine: byref<Machine>) (start: int) (finish: int) : unit =
        match finish - start < 2 with
        | true -> ()
        | false ->
            match isGraphemes machine.ClusterLevel with
            | false -> unsafeToBreak &machine start finish
            | true -> mergeClustersImpl &machine start finish

    /// <summary>Merges clusters over a range of the output buffer, <c>merge_out_clusters_impl</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="startIn">The first index.</param>
    /// <param name="finishIn">The end index.</param>
    /// <returns>The machine with the clusters merged.</returns>
    let private mergeOutClustersImpl (machine: byref<Machine>) (startIn: int) (finishIn: int) : unit =
        let outInfo = outInfoArray &machine
        let mutable start = startIn
        let mutable finish = finishIn
        let mutable maxOps = machine.MaxOps
        let mutable successful = machine.Successful
        let mutable cluster = outInfo[start].Cluster
        let mutable i = start + 1
        while i < finish do
            cluster <- min cluster outInfo[i].Cluster
            i <- i + 1
        match successful with
        | true ->
            match cluster = outInfo[start].Cluster with
            | false ->
                while start > 0 && outInfo[start - 1].Cluster = outInfo[start].Cluster do
                    start <- start - 1
            | true -> ()
            match cluster = outInfo[finish - 1].Cluster with
            | false ->
                while finish < machine.OutLen && outInfo[finish - 1].Cluster = outInfo[finish].Cluster do
                    finish <- finish + 1
            | true -> ()
            match finish = machine.OutLen && outInfo[finish - 1].Cluster <> cluster with
            | true ->
                let mutable j = machine.Idx
                while j < machine.Len && machine.Info[j].Cluster = outInfo[finish - 1].Cluster do
                    machine.Info[j] <- setClusterValue machine.Info[j] cluster 0u
                    j <- j + 1
                maxOps <- maxOps - (j - machine.Idx)
            | false -> ()
        | false -> ()
        maxOps <- maxOps - (finish - start)
        match maxOps < 0 with
        | true -> successful <- false
        | false -> ()
        let mutable k = start
        while k < finish do
            outInfo[k] <- setClusterValue outInfo[k] cluster 0u
            k <- k + 1
        machine.MaxOps <- maxOps
        machine.Successful <- successful

    /// <summary>Merges output clusters in monotone levels, <c>merge_out_clusters</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the clusters merged.</returns>
    let private mergeOutClusters (machine: byref<Machine>) (start: int) (finish: int) : unit =
        match finish - start < 2 || not (isMonotone machine.ClusterLevel) with
        | true -> ()
        | false -> mergeOutClustersImpl &machine start finish

    /// <summary>Merges output clusters in grapheme levels, <c>merge_out_grapheme_clusters</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the clusters merged.</returns>
    let private mergeOutGraphemeClusters (machine: byref<Machine>) (start: int) (finish: int) =
        match finish - start < 2 || not (isGraphemes machine.ClusterLevel) with
        | true -> ()
        | false -> mergeOutClustersImpl &machine start finish

    /// <summary>Moves one output glyph, with its properties, to an earlier output index and shifts the glyphs between up
    /// by one, the <c>memmove</c> and store of <c>preprocess_text_thai</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="fromOutput">The output index of the glyph to move.</param>
    /// <param name="toOutput">The output index it moves to, not after <paramref name="fromOutput"/>.</param>
    /// <returns>The machine with the glyph moved.</returns>
    let private moveOutput (machine: byref<Machine>) (fromOutput: int) (toOutput: int) =
        let outInfo = outInfoArray &machine
        let outProps = outPropsArray &machine
        let movedInfo = outInfo[fromOutput]
        let movedProps = outProps[fromOutput]
        Array.Copy(outInfo, toOutput, outInfo, toOutput + 1, fromOutput - toOutput)
        Array.Copy(outProps, toOutput, outProps, toOutput + 1, fromOutput - toOutput)
        outInfo[toOutput] <- movedInfo
        outProps[toOutput] <- movedProps

    /// <summary>Rewrites one input glyph into the output, <c>replace_glyphs</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="numIn">The number of input glyphs consumed.</param>
    /// <param name="numOut">The number of output glyphs produced.</param>
    /// <param name="glyphs">The produced code points.</param>
    /// <returns>The machine with the glyphs replaced.</returns>
    let private replaceGlyphs (machine: byref<Machine>) (numIn: int) (numOut: int) (glyphs: ReadOnlySpan<uint32>) : unit =
        match makeRoomFor &machine numIn numOut with
        | false -> ()
        | true ->
            mergeClusters &machine machine.Idx (machine.Idx + numIn)
            let struct (origInfo, origProps) = curOrPrev &machine
            let outInfo = outInfoArray &machine
            let outProps = outPropsArray &machine
            let mutable i = 0
            while i < numOut do
                outInfo[machine.OutLen + i] <- { origInfo with Codepoint = glyphs[i] }
                outProps[machine.OutLen + i] <- origProps
                i <- i + 1
            machine.Idx <- machine.Idx + numIn
            machine.OutLen <- machine.OutLen + numOut

    /// <summary>Rewrites the current glyph's code point and properties into the output, consuming one glyph,
    /// <c>replace_glyph</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="codepoint">The replacement code point.</param>
    /// <param name="props">The replacement properties.</param>
    /// <returns>The machine with the glyph replaced.</returns>
    let private replaceGlyph (machine: byref<Machine>) (codepoint: uint32) (props: GlyphProps) =
        match makeRoomFor &machine 1 1 with
        | false -> ()
        | true ->
            let origInfo = machine.Info[machine.Idx]
            let outInfo = outInfoArray &machine
            let outProps = outPropsArray &machine
            outInfo[machine.OutLen] <- { origInfo with Codepoint = codepoint }
            outProps[machine.OutLen] <- props
            machine.Idx <- machine.Idx + 1
            machine.OutLen <- machine.OutLen + 1

    /// <summary>Appends a whole glyph to the output, <c>output_info</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="info">The glyph.</param>
    /// <param name="props">Its properties.</param>
    /// <returns>The machine with the glyph appended.</returns>
    let private outputInfo (machine: byref<Machine>) (info: GlyphInfo) (props: GlyphProps) : unit =
        match makeRoomFor &machine 0 1 with
        | false -> ()
        | true ->
            let outInfo = outInfoArray &machine
            let outProps = outPropsArray &machine
            outInfo[machine.OutLen] <- info
            outProps[machine.OutLen] <- props
            machine.OutLen <- machine.OutLen + 1

    /// <summary>Merges the deleted glyph's cluster backward over the output, <c>delete_glyph</c> helper.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="cluster">The deleted glyph's cluster.</param>
    /// <returns>The machine with the cluster merged backward.</returns>
    let private mergeClusterBackward (machine: byref<Machine>) (cluster: uint32) : unit =
        let outInfo = outInfoArray &machine
        match cluster < outInfo[machine.OutLen - 1].Cluster && machine.Successful with
        | false -> ()
        | true ->
            let mask = machine.Info[machine.Idx].Mask
            let oldCluster = outInfo[machine.OutLen - 1].Cluster
            let mutable i = machine.OutLen
            while i > 0 && outInfo[i - 1].Cluster = oldCluster do
                i <- i - 1
                outInfo[i] <- setClusterValue outInfo[i] cluster mask
            let maxOps = machine.MaxOps - (machine.OutLen - i)
            machine.MaxOps <- maxOps
            machine.Successful <- match maxOps < 0 with | true -> false | false -> machine.Successful

    /// <summary>Deletes the current glyph, merging its cluster, <c>delete_glyph</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with the glyph skipped.</returns>
    let private deleteGlyph (machine: byref<Machine>) : unit =
        let cluster = machine.Info[machine.Idx].Cluster
        let outInfo = outInfoArray &machine
        let survives =
            (machine.Idx + 1 < machine.Len && cluster = machine.Info[machine.Idx + 1].Cluster)
            || (machine.OutLen > 0 && cluster = outInfo[machine.OutLen - 1].Cluster)
        match survives with
        | true -> machine.Idx <- machine.Idx + 1
        | false ->
            match machine.OutLen > 0 with
            | true ->
                mergeClusterBackward &machine cluster
                machine.Idx <- machine.Idx + 1
            | false ->
                match machine.Idx + 1 < machine.Len with
                | true ->
                    mergeClusters &machine machine.Idx (machine.Idx + 2)
                    machine.Idx <- machine.Idx + 1
                | false -> machine.Idx <- machine.Idx + 1

    /// <summary>Shifts the remaining input forward, <c>shift_forward</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="count">The number of slots to shift.</param>
    /// <returns>The machine with the input shifted.</returns>
    let private shiftForward (machine: byref<Machine>) (count: int) : unit =
        machine.Shifted <- true
        match ensure &machine (machine.Len + count) with
        | false -> ()
        | true ->
            let maxOps = machine.MaxOps - (machine.Len - machine.Idx)
            match maxOps < 0 with
            | true ->
                machine.MaxOps <- maxOps
                machine.Successful <- false
            | false ->
                Array.Copy(machine.Info, machine.Idx, machine.Info, machine.Idx + count, machine.Len - machine.Idx)
                Array.Copy(machine.Props, machine.Idx, machine.Props, machine.Idx + count, machine.Len - machine.Idx)
                match machine.Idx + count > machine.Len with
                | true ->
                    let mutable i = machine.Len
                    while i < machine.Idx + count do
                        machine.Info[i] <- Unchecked.defaultof<GlyphInfo>
                        machine.Props[i] <- Unchecked.defaultof<GlyphProps>
                        i <- i + 1
                    machine.MaxOps <- maxOps
                    machine.Len <- machine.Len + count
                    machine.Idx <- machine.Idx + count
                | false ->
                    machine.MaxOps <- maxOps
                    machine.Len <- machine.Len + count
                    machine.Idx <- machine.Idx + count

    /// <summary>Moves the cursor forward over the output, <c>move_to</c> helper.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="count">The number of glyphs to advance.</param>
    /// <returns>The machine with the cursor moved forward.</returns>
    let private moveToForward (machine: byref<Machine>) (count: int) : unit =
        match makeRoomFor &machine count count with
        | false -> ()
        | true ->
            let maxOps = machine.MaxOps - count
            match maxOps < 0 with
            | true ->
                machine.MaxOps <- maxOps
                machine.Successful <- false
            | false ->
                let outInfo = outInfoArray &machine
                let outProps = outPropsArray &machine
                Array.Copy(machine.Info, machine.Idx, outInfo, machine.OutLen, count)
                Array.Copy(machine.Props, machine.Idx, outProps, machine.OutLen, count)
                machine.MaxOps <- maxOps
                machine.Idx <- machine.Idx + count
                machine.OutLen <- machine.OutLen + count

    /// <summary>Rewinds the cursor, moving output glyphs back to the input, <c>move_to</c> helper.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="count">The number of glyphs to rewind.</param>
    /// <returns>The machine with the cursor rewound.</returns>
    let private moveToRewind (machine: byref<Machine>) (count: int) : unit =
        let maxOps = machine.MaxOps - count
        match maxOps < 0 with
        | true ->
            machine.MaxOps <- maxOps
            machine.Successful <- false
        | false ->
            let idx = machine.Idx - count
            let outLen = machine.OutLen - count
            let outInfo = outInfoArray &machine
            let outProps = outPropsArray &machine
            Array.Copy(outInfo, outLen, machine.Info, idx, count)
            Array.Copy(outProps, outLen, machine.Props, idx, count)
            machine.MaxOps <- maxOps
            machine.Idx <- idx
            machine.OutLen <- outLen

    /// <summary>Rewinds the cursor, shifting forward first when needed, <c>move_to</c> helper.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="count">The number of glyphs to rewind.</param>
    /// <returns>The machine with the cursor rewound.</returns>
    let private moveToRewindChecked (machine: byref<Machine>) (count: int) : unit =
        match machine.Idx < count with
        | true ->
            shiftForward &machine (count - machine.Idx)
            match machine.Successful with
            | false -> ()
            | true -> moveToRewind &machine count
        | false -> moveToRewind &machine count

    /// <summary>Moves the cursor to an output index, <c>move_to</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="target">The target output index.</param>
    /// <returns>The machine with the cursor moved.</returns>
    let private moveTo (machine: byref<Machine>) (target: int) : unit =
        match machine.HaveOutput with
        | false -> machine.Idx <- target
        | true ->
            match machine.Successful with
            | false -> ()
            | true ->
                match machine.OutLen < target, machine.OutLen > target with
                | true, _ -> moveToForward &machine (target - machine.OutLen)
                | false, false -> ()
                | false, true -> moveToRewindChecked &machine (machine.OutLen - target)

    /// <summary>Rewrites glyph masks, <c>set_masks</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="value">The mask value to set.</param>
    /// <param name="mask">The mask selecting the bits.</param>
    /// <param name="clusterStart">The first cluster to match.</param>
    /// <param name="clusterEnd">The cluster after the last to match.</param>
    /// <returns>The machine with the masks set.</returns>
    let private setMasks (machine: byref<Machine>) (value: uint32) (mask: uint32) (clusterStart: uint32) (clusterEnd: uint32) : unit =
        match mask = 0u with
        | true -> ()
        | false ->
            let notMask = ~~~mask
            let value = value &&& mask
            let maxOps = machine.MaxOps - machine.Len
            let successful = match maxOps < 0 with | true -> false | false -> machine.Successful
            match clusterStart = 0u && clusterEnd = UInt32.MaxValue with
            | true ->
                let mutable i = 0
                while i < machine.Len do
                    machine.Info[i] <- { machine.Info[i] with Mask = (machine.Info[i].Mask &&& notMask) ||| value }
                    i <- i + 1
                machine.MaxOps <- maxOps
                machine.Successful <- successful
            | false ->
                let mutable i = 0
                while i < machine.Len do
                    match clusterStart <= machine.Info[i].Cluster && machine.Info[i].Cluster < clusterEnd with
                    | true -> machine.Info[i] <- { machine.Info[i] with Mask = (machine.Info[i].Mask &&& notMask) ||| value }
                    | false -> ()
                    i <- i + 1
                machine.MaxOps <- maxOps
                machine.Successful <- successful

    /// <summary>Ors a mask into an index range, the fraction <c>info[j].mask |= mask</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The machine with the masks applied.</returns>
    let private orMask (machine: byref<Machine>) (mask: uint32) (start: int) (finish: int) : unit =
        let mutable i = start
        while i < finish do
            machine.Info[i] <- { machine.Info[i] with Mask = machine.Info[i].Mask ||| mask }
            i <- i + 1

    /// <summary>Rewrites the properties of the current input glyph, the first half of <c>next_char</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="props">The properties.</param>
    /// <returns>The machine with the current glyph's properties rewritten.</returns>
    let private setCurrentProps (machine: byref<Machine>) (props: GlyphProps) : unit =
        machine.Props[machine.Idx] <- props

    /// <summary>Rewrites one output glyph's code point and properties, keeping its cluster and mask from the merge,
    /// <c>hb-ot-shape-normalize.cc</c>'s starter rewrite.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The output index.</param>
    /// <param name="codepoint">The composed code point.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The machine with the output glyph rewritten.</returns>
    let private setOutputGlyph (machine: byref<Machine>) (index: int) (codepoint: uint32) (props: GlyphProps) : unit =
        let outInfo = outInfoArray &machine
        let outProps = outPropsArray &machine
        outInfo[index] <- { outInfo[index] with Codepoint = codepoint }
        outProps[index] <- props

    /// <summary>Removes the last output glyph, the <c>out_len--</c> of the compose round.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with one fewer output glyph.</returns>
    let private popOutput (machine: byref<Machine>) : unit =
        machine.OutLen <- machine.OutLen - 1

    /// <summary>Rewrites one input glyph's properties in place, without moving the cursor.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="inputIndex">The input index.</param>
    /// <param name="inputProps">The properties.</param>
    /// <returns>The machine with the glyph's properties rewritten.</returns>
    let private setInputProps (machine: byref<Machine>) (inputIndex: int) (inputProps: GlyphProps) : unit =
        machine.Props[inputIndex] <- inputProps

    /// <summary>Rewrites one glyph position in place, without moving the cursor.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="positionIndex">The position index.</param>
    /// <param name="position">The position.</param>
    /// <returns>The machine with the position rewritten.</returns>
    let private setPosition (machine: byref<Machine>) (positionIndex: int) (position: GlyphPosition) : unit =
        materializePositions &machine
        machine.Positions[positionIndex] <- position

    /// <summary>Rewinds the cursor to an index, only when no output buffer is open.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The target input index.</param>
    /// <returns>The machine with the cursor rewound.</returns>
    let private setIndex (machine: byref<Machine>) (index: int) : unit =
        match machine.HaveOutput with
        | true -> ()
        | false -> machine.Idx <- index

    /// <summary>Rewrites the buffer serial, <c>hb_buffer_t::serial</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="serial">The new serial.</param>
    /// <returns>The machine with the serial rewritten.</returns>
    let private setSerial (machine: byref<Machine>) (serial: byte) : unit =
        machine.Serial <- serial

    /// <summary>Rewrites the buffer random state, <c>hb_buffer_t::random_state</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="randomState">The new random state.</param>
    /// <returns>The machine with the random state rewritten.</returns>
    let private setRandomState (machine: byref<Machine>) (randomState: uint32) : unit =
        machine.RandomState <- randomState

    /// <summary>Ors a scratch flag into the buffer.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="scratch">The flag to add.</param>
    /// <returns>The machine with the flag accumulated.</returns>
    let private addScratch (machine: byref<Machine>) (scratch: ScratchFlags) : unit =
        machine.ScratchFlags <- machine.ScratchFlags ||| scratch

    /// <summary>The longest range a rearrangement subtable may span, <c>HB_MAX_CONTEXT_LENGTH</c>.</summary>
    [<Literal>]
    let private MaxContextLength = 64

    /// <summary>The length of the positions buffer of a GSUB visit: the input positions of a match, and after them the glyphs a lookup shares among its subtables.</summary>
    [<Literal>]
    let private ContextBufferLength = 128

    /// <summary>Charges operations against the buffer budget, the <c>max_ops -= n</c> of an AAT subtable.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="opsCharged">The number of operations to charge.</param>
    /// <returns>The machine with the budget reduced.</returns>
    let private chargeOps (machine: byref<Machine>) opsCharged =
        machine.MaxOps <- machine.MaxOps - opsCharged

    /// <summary>Fails the buffer, <c>buffer->successful = false</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine marked unsuccessful.</returns>
    let private fail (machine: byref<Machine>) =
        machine.Successful <- false

    /// <summary>Rewrites one input glyph's code point and properties in place, without moving the cursor,
    /// <c>replace_glyph_inplace</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="inputIndex">The input index.</param>
    /// <param name="codepoint">The replacement code point.</param>
    /// <param name="inputProps">The replacement properties.</param>
    /// <returns>The machine with the glyph rewritten.</returns>
    let private setInputGlyph (machine: byref<Machine>) inputIndex codepoint inputProps =
        machine.Info[inputIndex] <- { machine.Info[inputIndex] with Codepoint = codepoint }
        machine.Props[inputIndex] <- inputProps

    /// <summary>Rearranges a range between two moved sides, the copies of <c>RearrangementSubtable::transition</c>
    /// (src/hb-aat-layout-morx-table.hh:136-159). Properties move with their infos; positions do not.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index of the range.</param>
    /// <param name="finish">The end index of the range.</param>
    /// <param name="moveLeft">The number of glyphs moved from the left side.</param>
    /// <param name="moveRight">The number of glyphs moved from the right side.</param>
    /// <param name="flipLeft">Whether the moved left pair is flipped.</param>
    /// <param name="flipRight">Whether the moved right pair is flipped.</param>
    /// <returns>The machine with the range rearranged.</returns>
    let private rearrange (machine: byref<Machine>) start finish moveLeft moveRight flipLeft flipRight =
        match finish - start >= moveLeft + moveRight && finish - start <= MaxContextLength with
        | false -> ()
        | true ->
            let infos = machine.Info
            let props = machine.Props
            let leftInfos = infos.AsSpan(start, moveLeft).ToArray()
            let leftProps = props.AsSpan(start, moveLeft).ToArray()
            let rightInfos = infos.AsSpan(finish - moveRight, moveRight).ToArray()
            let rightProps = props.AsSpan(finish - moveRight, moveRight).ToArray()
            match moveLeft = moveRight with
            | false ->
                Array.Copy(infos, start + moveLeft, infos, start + moveRight, finish - start - moveLeft - moveRight)
                Array.Copy(props, start + moveLeft, props, start + moveRight, finish - start - moveLeft - moveRight)
            | true -> ()
            rightInfos.CopyTo(Span<GlyphInfo>(infos, start, moveRight))
            rightProps.CopyTo(Span<GlyphProps>(props, start, moveRight))
            leftInfos.CopyTo(Span<GlyphInfo>(infos, finish - moveLeft, moveLeft))
            leftProps.CopyTo(Span<GlyphProps>(props, finish - moveLeft, moveLeft))
            match flipLeft with
            | true ->
                let temporaryInfo = infos[finish - 1]
                infos[finish - 1] <- infos[finish - 2]
                infos[finish - 2] <- temporaryInfo
                let temporaryProps = props[finish - 1]
                props[finish - 1] <- props[finish - 2]
                props[finish - 2] <- temporaryProps
            | false -> ()
            match flipRight with
            | true ->
                let temporaryInfo = infos[start]
                infos[start] <- infos[start + 1]
                infos[start + 1] <- temporaryInfo
                let temporaryProps = props[start]
                props[start] <- props[start + 1]
                props[start + 1] <- temporaryProps
            | false -> ()

    /// <summary>Opens the out-buffer and clears positions, <c>clear_output</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with an open out-buffer.</returns>
    let private clearOutput (machine: byref<Machine>) : unit =
        machine.HaveOutput <- true
        machine.HavePositions <- false
        machine.Idx <- 0
        machine.OutLen <- 0
        machine.OutIsSeparate <- false

    /// <summary>Resets the output-buffer indices after a failed or completed sync.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with the output reset.</returns>
    let private syncReset (machine: byref<Machine>) : unit =
        machine.HaveOutput <- false
        machine.OutLen <- 0
        machine.Idx <- 0
        machine.OutIsSeparate <- false

    /// <summary>Swaps the output into the input after a successful sync.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with the output finalised.</returns>
    let private syncFinalize (machine: byref<Machine>) : unit =
        let finalLen = machine.OutLen
        match machine.OutIsSeparate with
        | false ->
            machine.Len <- finalLen
            machine.HaveOutput <- false
            machine.OutLen <- 0
            machine.Idx <- 0
        | true ->
            let info = machine.OutInfo
            let outInfo = machine.Info
            let props = machine.OutProps
            let outProps = machine.Props
            machine.Info <- info
            machine.OutInfo <- outInfo
            machine.Props <- props
            machine.OutProps <- outProps
            machine.Len <- finalLen
            machine.HaveOutput <- false
            machine.OutLen <- 0
            machine.Idx <- 0
            machine.OutIsSeparate <- false

    /// <summary>Finalises the output buffer, <c>sync</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The machine with the output finalised.</returns>
    let private sync (machine: byref<Machine>) : unit =
        match machine.HaveOutput with
        | false -> ()
        | true ->
            match machine.Successful with
            | false -> syncReset &machine
            | true ->
                nextGlyphs &machine (machine.Len - machine.Idx)
                match machine.Successful with
                | false -> syncReset &machine
                | true -> syncFinalize &machine

    /// <summary>Builds the read-only cursor a pass sees for one step.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The cursor.</returns>
    let private makeCursor (machine: inref<Machine>) : RunCursor =
        let infos =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphInfo>(machine.Info, 0, machine.Len)
            | false -> machine.SourceInfos.AsSpan(0, machine.Len)
        let props =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphProps>(machine.Props, 0, machine.Len)
            | false -> machine.SourceProps.AsSpan(0, machine.Len)
        let outInfos =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphInfo>(outInfoArray &machine, 0, machine.OutLen)
            | false -> machine.SourceInfos.AsSpan(0, machine.OutLen)
        let outProps =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphProps>(outPropsArray &machine, 0, machine.OutLen)
            | false -> machine.SourceProps.AsSpan(0, machine.OutLen)
        let positions =
            match machine.HavePositions with
            | false -> ReadOnlySpan.Empty
            | true ->
                match machine.PositionsMaterialized with
                | true -> ReadOnlySpan<GlyphPosition>(machine.Positions, 0, machine.Len)
                | false -> machine.SourcePositions.AsSpan(0, machine.Len)
        RunCursor(
            infos,
            props,
            machine.Idx,
            outInfos,
            outProps,
            positions,
            machine.HaveOutput,
            machine.ClusterLevel,
            machine.Flags,
            machine.Serial,
            machine.RandomState,
            machine.MaxOps,
            machine.Successful)

    /// <summary>Applies one edit to the machine, tracking the passive-edit flag.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="edit">The edit.</param>
    /// <returns>The machine after the edit.</returns>
    let private applyEdit (machine: byref<Machine>) (edit: RunEdit) : unit =
        let passive =
            match edit.Kind with
            | EditKind.NextGlyph | EditKind.NextGlyphs | EditKind.Sync | EditKind.Finish -> true
            | _ -> false
        match passive with
        | false -> materialize &machine
        | true -> ()
        match edit.Kind with
        | EditKind.ClearOutput -> clearOutput &machine
        | EditKind.NextGlyph -> nextGlyphs &machine 1
        | EditKind.NextGlyphs -> nextGlyphs &machine (int edit.W0)
        | EditKind.ReplaceGlyph -> replaceGlyphs &machine 1 1 (ReadOnlySpan [| edit.W0 |])
        | EditKind.ReplaceGlyphProps -> replaceGlyph &machine edit.W0 (RunEdit.UnpackProps edit.W1 edit.W2 edit.W3)
        | EditKind.ReplaceGlyphs ->
            let produced =
                match edit.Ref with
                | :? (uint32[]) as arr -> arr
                | _ -> Array.Empty<uint32>()
            replaceGlyphs &machine (int edit.W0) (int edit.W1) (ReadOnlySpan<uint32> produced)
        | EditKind.OutputGlyph -> replaceGlyphs &machine 0 1 (ReadOnlySpan [| edit.W0 |])
        | EditKind.OutputInfo ->
            let info = RunEdit.UnpackInfo edit.W0 edit.W1 edit.W2 edit.W3 edit.W4
            let props = RunEdit.UnpackProps edit.W5 edit.W6 edit.W7
            outputInfo &machine info props
        | EditKind.CopyGlyph -> outputInfo &machine machine.Info[machine.Idx] machine.Props[machine.Idx]
        | EditKind.SkipGlyph -> machine.Idx <- machine.Idx + 1
        | EditKind.DeleteGlyph -> deleteGlyph &machine
        | EditKind.MoveTo -> moveTo &machine (int edit.W0)
        | EditKind.MergeClusters -> mergeClusters &machine (int edit.W0) (int edit.W1)
        | EditKind.MergeGraphemeClusters -> mergeGraphemeClusters &machine (int edit.W0) (int edit.W1)
        | EditKind.MergeOutClusters -> mergeOutClusters &machine (int edit.W0) (int edit.W1)
        | EditKind.MergeOutGraphemeClusters -> mergeOutGraphemeClusters &machine (int edit.W0) (int edit.W1)
        | EditKind.MoveOutput -> moveOutput &machine (int edit.W0) (int edit.W1)
        | EditKind.UnsafeToBreak -> unsafeToBreak &machine (int edit.W0) (int edit.W1)
        | EditKind.UnsafeToConcat -> unsafeToConcat &machine (int edit.W0) (int edit.W1)
        | EditKind.UnsafeToBreakFromOutbuffer -> unsafeToBreakFromOutbuffer &machine (int edit.W0) (int edit.W1)
        | EditKind.UnsafeToConcatFromOutbuffer -> unsafeToConcatFromOutbuffer &machine (int edit.W0) (int edit.W1)
        | EditKind.SafeToInsertTatweel -> safeToInsertTatweel &machine (int edit.W0) (int edit.W1)
        | EditKind.SetMasks -> setMasks &machine edit.W0 edit.W1 edit.W2 edit.W3
        | EditKind.OrMask -> orMask &machine edit.W0 (int edit.W1) (int edit.W2)
        | EditKind.SetCurrentProps -> setCurrentProps &machine (RunEdit.UnpackProps edit.W1 edit.W2 edit.W3)
        | EditKind.SetOutputGlyph -> setOutputGlyph &machine (int edit.W0) edit.W4 (RunEdit.UnpackProps edit.W1 edit.W2 edit.W3)
        | EditKind.PopOutput -> popOutput &machine
        | EditKind.Sync -> sync &machine
        | EditKind.Finish -> sync &machine
        | EditKind.SetInputProps -> setInputProps &machine (int edit.W0) (RunEdit.UnpackProps edit.W1 edit.W2 edit.W3)
        | EditKind.SetPosition ->
            let position = RunEdit.UnpackPosition edit.W1 edit.W2 edit.W3 edit.W4 edit.W5
            setPosition &machine (int edit.W0) position
        | EditKind.SetIndex -> setIndex &machine (int edit.W0)
        | EditKind.SetSerial -> setSerial &machine (byte edit.W0)
        | EditKind.SetRandomState -> setRandomState &machine edit.W0
        | EditKind.AddScratch -> addScratch &machine (LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> edit.W0)
        | EditKind.ChargeOps -> chargeOps &machine (int edit.W0)
        | EditKind.Fail -> fail &machine
        | EditKind.SetInputGlyph -> setInputGlyph &machine (int edit.W0) edit.W1 (RunEdit.UnpackProps edit.W2 edit.W3 edit.W4)
        | EditKind.Rearrange -> rearrange &machine (int edit.W0) (int edit.W1) (int edit.W2) (int edit.W3) (edit.W4 &&& 1u <> 0u) (edit.W4 &&& 2u <> 0u)
        | _ -> ()
        match passive with
        | false -> machine.Passive <- false
        | true -> ()

    /// <summary>Runs a pass to completion, <c>Sealed.runPass</c>'s interpreter loop.</summary>
    /// <typeparam name="'P">The pass type.</typeparam>
    /// <typeparam name="'S">The pass state type.</typeparam>
    /// <param name="pass">The pass.</param>
    /// <param name="passState">The pass state.</param>
    /// <param name="machine">The machine state.</param>
    /// <returns>The machine after the pass finishes.</returns>
    let private runLoop<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> (pass: 'P) (passState: 'S) (machine: byref<Machine>) : unit =
        let mutable state = passState
        let mutable finished = false
        while not finished do
            let cursor = makeCursor &machine
            let struct (edit, nextState) = pass.Step(&cursor, state)
            state <- nextState
            match edit.Kind with
            | EditKind.Finish ->
                sync &machine
                finished <- true
            | _ -> applyEdit &machine edit

    /// <summary>Publishes the run's positions after a pass, reusing the input array when no position changed.</summary>
    /// <param name="final">The finished machine.</param>
    /// <param name="inputPositions">The input positions.</param>
    /// <returns>The published positions.</returns>
    let private publishPositions (final: inref<Machine>) (inputPositions: ImmutableArray<GlyphPosition>) : ImmutableArray<GlyphPosition> =
        match final.HavePositions with
        | false -> ImmutableArray.Empty
        | true ->
            match final.PositionsMaterialized with
            | true -> publishRange final.Positions final.Len
            | false ->
                match final.Len = inputPositions.Length with
                | true -> inputPositions
                | false -> resize (inputPositions.AsSpan()) final.Len

    /// <summary>Publishes the glyph infos of a finished machine: the working array when an edit materialized it, else the
    /// input array untouched, which is what a pass that failed before its first edit leaves, as
    /// <c>hb_buffer_t::sync</c> leaves <c>info</c> alone on an unsuccessful buffer.</summary>
    /// <param name="final">The finished machine.</param>
    /// <returns>The published infos.</returns>
    let private publishMachineInfos (final: inref<Machine>) =
        match final.Materialized with
        | true -> publishRange final.Info final.Len
        | false -> final.SourceInfos

    /// <summary>Publishes the glyph properties of a finished machine, as <see cref="publishMachineInfos"/> does the infos.</summary>
    /// <param name="final">The finished machine.</param>
    /// <returns>The published properties.</returns>
    let private publishMachineProps (final: inref<Machine>) =
        match final.Materialized with
        | true -> publishRange final.Props final.Len
        | false -> final.SourceProps

    /// <summary>Pads an immutable array to a length with default values when it is shorter than the run.</summary>
    /// <param name="items">The immutable array.</param>
    /// <param name="length">The required length.</param>
    /// <returns>The array, padded when it was shorter.</returns>
    let private padTo (items: ImmutableArray<'T>) (length: int) : ImmutableArray<'T> =
        match items.Length >= length with
        | true -> items
        | false -> resize (items.AsSpan()) length

    /// <summary>Builds the machine of a run, before any edit.</summary>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="digest">The digest of the run's glyphs, empty for a pass that does not gate on it.</param>
    /// <param name="normalise">Whether the first opened output must materialise the arrays, because publishing them would
    /// differ from the input run (positions to drop, properties of another length).</param>
    /// <param name="exact">Whether the working arrays are allocated at the run's length, growing only when a lookup grows the run,
    /// so a lookup that never grows it publishes them without a copy.</param>
    /// <returns>The machine.</returns>
    let private newMachine (input: RunInput) (digest: Digest) (normalise: bool) (exact: bool) =
        let len = input.Infos.Length
        { Info = Array.empty
          OutInfo = Array.empty
          Props = Array.empty
          OutProps = Array.empty
          Positions = Array.empty
          SourceInfos = input.Infos
          SourceProps = padTo input.Props len
          SourcePositions = match input.HavePositions with | true -> padTo input.Positions len | false -> input.Positions
          Materialized = false
          PositionsMaterialized = false
          OutIsSeparate = false
          Idx = 0
          Len = len
          OutLen = 0
          HaveOutput = false
          HavePositions = input.HavePositions
          MaxOps = input.MaxOps
          Successful = input.Successful
          Passive = true
          MaxLen = input.MaxLen
          ClusterLevel = input.ClusterLevel
          Flags = input.Flags
          Serial = input.Serial
          RandomState = input.RandomState
          ScratchFlags = input.ScratchFlags
          Digest = digest
          DigestStale = false
          Candidates = UInt64.MaxValue
          LookupPassive = true
          Normalise = normalise
          Exact = exact
          Allocated = 0
          Nesting = MaxNestingLevel
          InitialMaxOps = input.MaxOps
          Shifted = false
          Grew = false
          Watch = null }

    /// <summary>Builds the result of a finished machine: the unchanged marker when no edit touched the run, else the
    /// published arrays.</summary>
    /// <param name="final">The finished machine.</param>
    /// <param name="unchanged">Whether no edit touched the run.</param>
    /// <param name="inputPositions">The input positions.</param>
    /// <returns>The run fields the machine produces.</returns>
    let private finishRun (final: inref<Machine>) (unchanged: bool) (inputPositions: ImmutableArray<GlyphPosition>) =
        match unchanged with
        | true ->
            { Infos = ImmutableArray.Empty
              Props = ImmutableArray.Empty
              Positions = ImmutableArray.Empty
              HavePositions = final.HavePositions
              MaxOps = final.MaxOps
              Successful = final.Successful
              Unchanged = true
              Serial = final.Serial
              RandomState = final.RandomState
              ScratchFlags = final.ScratchFlags }
        | false ->
            let positions = publishPositions &final inputPositions
            { Infos = publishMachineInfos &final
              Props = publishMachineProps &final
              Positions = positions
              HavePositions = final.HavePositions
              MaxOps = final.MaxOps
              Successful = final.Successful
              Unchanged = false
              Serial = final.Serial
              RandomState = final.RandomState
              ScratchFlags = final.ScratchFlags }

    /// <summary>Runs one shaping pass over a run's arrays, the out-buffer machine.</summary>
    /// <typeparam name="'P">The pass type.</typeparam>
    /// <typeparam name="'S">The pass state type.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="pass">The pass.</param>
    /// <param name="seed">The initial pass state.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runPass<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> (input: RunInput) (pass: 'P) (seed: 'S) =
        let mutable initial = newMachine input Digest.empty false false
        runLoop pass seed &initial
        finishRun &initial (initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps) input.Positions

    /// <summary>The glyphs the pass reads, the input array of the cursor.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The infos from the first to the last input glyph.</returns>
    let private inputInfos (machine: inref<Machine>) =
        match machine.Materialized with
        | true -> ReadOnlySpan<GlyphInfo>(machine.Info, 0, machine.Len)
        | false -> machine.SourceInfos.AsSpan(0, machine.Len)

    /// <summary>The glyph properties parallel to <see cref="inputInfos"/>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The properties of the input glyphs.</returns>
    let private inputProps (machine: inref<Machine>) =
        match machine.Materialized with
        | true -> ReadOnlySpan<GlyphProps>(machine.Props, 0, machine.Len)
        | false -> machine.SourceProps.AsSpan(0, machine.Len)

    /// <summary>The output built so far.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The infos of the output glyphs.</returns>
    let private outputInfos (machine: inref<Machine>) =
        match machine.Materialized with
        | true -> ReadOnlySpan<GlyphInfo>(outInfoArray &machine, 0, machine.OutLen)
        | false -> machine.SourceInfos.AsSpan(0, machine.OutLen)

    /// <summary>The glyph properties parallel to <see cref="outputInfos"/>.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The properties of the output glyphs.</returns>
    let private outputProps (machine: inref<Machine>) =
        match machine.Materialized with
        | true -> ReadOnlySpan<GlyphProps>(outPropsArray &machine, 0, machine.OutLen)
        | false -> machine.SourceProps.AsSpan(0, machine.OutLen)

    /// <summary>Gives the working arrays the allocation a freshly materialised copy of the run has, as the interpreter's
    /// machine of each lookup starts with, so a lookup that grows the run fails at the same length however many lookups
    /// ran before it.</summary>
    /// <param name="machine">The machine.</param>
    let private reseat (machine: byref<Machine>) =
        machine.Allocated <- match machine.Len = 0 with | true -> 0 | false -> growAllocated machine.Len 0

    /// <summary>Opens the out-buffer before the first glyph a lookup touches, <c>clear_output</c>; a run whose published
    /// arrays would differ from its input (positions to drop, properties of another length) gets its own arrays here,
    /// as the interpreter's first edit gives them.</summary>
    /// <param name="machine">The machine.</param>
    let private lookupOpen (machine: byref<Machine>) =
        match machine.HaveOutput with
        | true -> ()
        | false ->
            match machine.Normalise with
            | true -> touch &machine
            | false -> ()
            match machine.Materialized with
            | true -> reseat &machine
            | false -> ()
            clearOutput &machine

    /// <summary>Rewrites the current glyph's code point and properties into the output, consuming one glyph,
    /// <c>replace_glyph</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="codepoint">The replacement code point.</param>
    /// <param name="props">The replacement properties.</param>
    let private lookupReplace (machine: byref<Machine>) (codepoint: uint32) (props: GlyphProps) =
        touch &machine
        match machine.OutIsSeparate || machine.OutLen <> machine.Idx with
        | false ->
            machine.Info[machine.Idx] <- { machine.Info[machine.Idx] with Codepoint = codepoint }
            machine.Props[machine.Idx] <- props
            machine.Idx <- machine.Idx + 1
            machine.OutLen <- machine.OutLen + 1
        | true -> replaceGlyph &machine codepoint props

    /// <summary>Appends a whole glyph to the output, <c>output_info</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="info">The glyph.</param>
    /// <param name="props">Its properties.</param>
    let private lookupOutput (machine: byref<Machine>) (info: GlyphInfo) (props: GlyphProps) =
        touch &machine
        outputInfo &machine info props

    /// <summary>Advances the cursor without output, <c>skip_glyph</c>.</summary>
    /// <param name="machine">The machine.</param>
    let private lookupSkip (machine: byref<Machine>) =
        touch &machine
        machine.Idx <- machine.Idx + 1

    /// <summary>Merges clusters over a range, <c>merge_clusters</c>; a range of fewer than two glyphs changes nothing.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let private lookupMergeClusters (machine: byref<Machine>) (start: int) (finish: int) =
        match finish - start < 2 with
        | true -> ()
        | false ->
            touch &machine
            mergeClusters &machine start finish

    /// <summary>Marks a range unsafe to concat, <c>unsafe_to_concat</c>, which does nothing unless the buffer asks for the flag.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let private lookupUnsafeToConcat (machine: byref<Machine>) (start: int) (finish: int) =
        match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
        | true -> ()
        | false ->
            touch &machine
            unsafeToConcat &machine start finish

    /// <summary>Applies the components of a multiple substitution as output glyphs and drops the replaced glyph,
    /// <c>MultipleSubst::apply</c>'s output loop.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="components">The components.</param>
    let private multipleOutput<'L when 'L : struct and 'L :> IGsubLookup> (lookup: byref<'L>) (machine: byref<Machine>) (components: ImmutableArray<uint32>) =
        let info = (inputInfos &machine)[machine.Idx]
        let baseProps = (inputProps &machine)[machine.Idx]
        let mutable index = 0
        while index < components.Length do
            let glyph = components[index]
            lookupOutput &machine { info with Codepoint = glyph } (lookup.MultipleProps(baseProps, glyph, index))
            index <- index + 1
        lookupSkip &machine

    /// <summary>Walks the components of a ligation after its ligature glyph is written, <c>ligate_input</c>: glyphs between
    /// components are copied, or re-tagged when the ligature is a real one, each component is consumed, and the marks
    /// that follow are re-adjusted.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="data">The ligature data.</param>
    /// <param name="first">The first glyph's index.</param>
    /// <param name="firstLigId">The first glyph's ligature id.</param>
    /// <param name="firstNumComponents">The first glyph's component count.</param>
    let private ligateComponents<'L when 'L : struct and 'L :> IGsubLookup> (lookup: byref<'L>) (machine: byref<Machine>) (data: LigateFacts) (first: int) (firstLigId: byte) (firstNumComponents: int) =
        let mutable componentIndex = 1
        let mutable matchStart = first
        let mutable target = -1
        let mutable lastLigId = firstLigId
        let mutable lastNumComponents = firstNumComponents
        let mutable componentsSoFar = firstNumComponents
        while componentIndex < data.Count do
            match target with
            | -1 ->
                let found = match componentIndex = data.Count - 1 with | true -> data.MatchEnd - 1 | false -> lookup.NextComponent(data, componentIndex, inputInfos &machine, inputProps &machine, matchStart)
                match found < 0 with
                | true -> componentIndex <- data.Count
                | false -> target <- found
            | _ ->
                match machine.Idx < target with
                | true ->
                    match data.IsLigature with
                    | false -> nextGlyphs &machine 1
                    | true ->
                        let markProps = (inputProps &machine)[machine.Idx]
                        let thisComp = GlyphPropsBits.ligComp markProps
                        let resolvedComp = if thisComp = 0uy then lastNumComponents else int thisComp
                        let newComp = componentsSoFar - lastNumComponents + min resolvedComp lastNumComponents
                        touch &machine
                        setInputProps &machine machine.Idx (GlyphPropsBits.setLigPropsForMark data.LigId (byte newComp) markProps)
                        nextGlyphs &machine 1
                | false ->
                    let componentProps = (inputProps &machine)[machine.Idx]
                    let nextNumComponents = int (GlyphPropsBits.ligNumCompsInLigation componentProps)
                    lookupSkip &machine
                    componentIndex <- componentIndex + 1
                    matchStart <- target
                    target <- -1
                    lastLigId <- GlyphPropsBits.ligId componentProps
                    lastNumComponents <- nextNumComponents
                    componentsSoFar <- componentsSoFar + nextNumComponents
        let mutable scan = machine.Idx
        let mutable adjusting = not data.IsMarkLigature && lastLigId <> 0uy
        while adjusting && scan < machine.Len do
            let markProps = (inputProps &machine)[scan]
            match lastLigId = GlyphPropsBits.ligId markProps && GlyphPropsBits.ligComp markProps <> 0uy with
            | false -> adjusting <- false
            | true ->
                let thisComp = int (GlyphPropsBits.ligComp markProps)
                let newComp = componentsSoFar - lastNumComponents + min thisComp lastNumComponents
                touch &machine
                setInputProps &machine scan (GlyphPropsBits.setLigPropsForMark data.LigId (byte newComp) markProps)
                scan <- scan + 1
        machine.Passive <- false
        machine.LookupPassive <- false
        setSerial &machine data.NextSerial

    /// <summary>Writes a matched ligature: the merged clusters, the ligature glyph in place of the first glyph, then the
    /// components, <c>Ligature::apply</c>'s tail and <c>ligate_input</c>, and last the unsafe-to-concat the fast path owes,
    /// from the glyph the ligation ended on (src/OT/Layout/GSUB/LigatureSet.hh:151-155 call it after <c>lig.apply</c>).</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="data">The matched ligature.</param>
    /// <param name="first">The first glyph's index.</param>
    /// <param name="firstLigId">The first glyph's ligature id.</param>
    /// <param name="firstNumComponents">The first glyph's component count.</param>
    /// <param name="concatOwed">Whether the fast path owes an unsafe-to-concat up to <paramref name="unsafeTo"/>.</param>
    /// <param name="unsafeTo">The fast path's unsafe-to bound.</param>
    let private ligateMatched<'L when 'L : struct and 'L :> IGsubLookup> (lookup: byref<'L>) (machine: byref<Machine>) (data: LigateFacts) (first: int) (firstLigId: byte) (firstNumComponents: int) (concatOwed: bool) (unsafeTo: int) =
        lookupMergeClusters &machine first data.MatchEnd
        let ligatureInfo = (inputInfos &machine)[first]
        let serialBefore = machine.Serial
        lookupReplace &machine data.LigGlyph data.FinalProps
        ligateComponents &lookup &machine data first firstLigId firstNumComponents
        match machine.Watch with
        | null -> ()
        | watch ->
            match machine.Serial <> serialBefore with
            | true -> watch.Allocated ligatureInfo.Cluster
            | false -> ()
        match concatOwed with
        | true -> lookupUnsafeToConcat &machine machine.Idx unsafeTo
        | false -> ()

    /// <summary>Walks the ligatures of a set, trying each until one applies, <c>LigatureSet::apply</c>'s slow and fast loops;
    /// when none applies the glyph is left where it is, for the next subtable of the lookup to try. On the fast path the ligatures the
    /// probed second glyph rules out are passed over together, which is what owes the unsafe-to-concat range.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="set">The ligature set.</param>
    /// <param name="count">The number of ligatures in the set.</param>
    /// <param name="fast">Whether the fast path's pre-check applies.</param>
    /// <param name="second">The fast path's second glyph.</param>
    /// <param name="unsafeTo">The fast path's unsafe-to bound.</param>
    /// <returns>True when a ligature applied.</returns>
    let private ligatureWalk<'L when 'L : struct and 'L :> IGsubLookup> (lookup: byref<'L>) (machine: byref<Machine>) (subtable: int) (set: int) (count: int) (fast: bool) (second: uint32) (unsafeTo: int) =
        let mutable index = 0
        let mutable pendingConcat = false
        let mutable walking = true
        let mutable applied = true
        while walking do
            let admitted =
                match fast with
                | true -> lookup.LigatureNext(subtable, set, index, second)
                | false -> index
            pendingConcat <- pendingConcat || admitted > index
            index <- admitted
            match index < count with
            | false ->
                match fast && pendingConcat with
                | true -> lookupUnsafeToConcat &machine machine.Idx unsafeTo
                | false -> ()
                applied <- false
                walking <- false
            | true ->
                let first = machine.Idx
                match lookup.LigatureApply(subtable, set, index, inputInfos &machine, inputProps &machine, outputInfos &machine, outputProps &machine, first, machine.Serial) with
                | LigatureMiss -> index <- index + 1
                | LigatureReplace (info, props) ->
                    lookupReplace &machine info.Codepoint props
                    walking <- false
                | LigatureUnsafeToConcat concatTo ->
                    lookupUnsafeToConcat &machine first concatTo
                    index <- index + 1
                | LigatureLigate (data, lastLigId, lastNumComponents, _) ->
                    ligateMatched &lookup &machine data first lastLigId lastNumComponents (fast && pendingConcat) unsafeTo
                    walking <- false
        applied

    /// <summary>Applies a ligature set at the current glyph: probes the second glyph, then walks the set on the fast path
    /// when the probe allows it, <c>LigatureSet::apply</c>.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="set">The ligature set.</param>
    /// <returns>True when a ligature applied; else the glyph is where it was and the next subtable is to be tried.</returns>
    let private ligatureSet<'L when 'L : struct and 'L :> IGsubLookup> (lookup: byref<'L>) (machine: byref<Machine>) (subtable: int) (set: int) =
        let head = lookup.LigatureHead(subtable, set, inputInfos &machine, inputProps &machine, machine.Idx)
        match head.Kind with
        | LigatureHeadKind.NoSecond ->
            lookupUnsafeToConcat &machine machine.Idx head.Bound
            false
        | LigatureHeadKind.Fast -> ligatureWalk &lookup &machine subtable set head.Count true head.Second head.Bound
        | _ -> ligatureWalk &lookup &machine subtable set head.Count false 0u 0

    /// <summary>The settings a nested lookup inherits: the outer mask, random flag and joiner and per-syllable switches.</summary>
    [<Struct>]
    type private NestSettings =
        val Mask: uint32
        val Random: bool
        val AutoZwnj: bool
        val AutoZwj: bool
        val PerSyllable: bool
        new (mask, random, autoZwnj, autoZwj, perSyllable) =
            { Mask = mask; Random = random; AutoZwnj = autoZwnj; AutoZwj = autoZwj; PerSyllable = perSyllable }

    /// <summary>Applies the outcome of the subtables at the current glyph.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="advance">Whether a miss moves the glyph to the output: the loop of <c>apply_forward</c> does that, a lookup
    /// that a context recurses into does not, as <c>apply_recurse_func</c> only answers that nothing applied.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="outcome">The outcome.</param>
    let rec private applyOutcome<'L when 'L : struct and 'L :> IGsubLookup> (advance: bool) (lookup: byref<'L>) (machine: byref<Machine>) (outcome: SubstOutcome) =
        match outcome.Kind with
        | SubstKind.Replace -> lookupReplace &machine outcome.Glyph outcome.Props
        | SubstKind.Miss ->
            match advance with
            | true -> nextGlyphs &machine 1
            | false -> ()
        | SubstKind.Delete ->
            touch &machine
            deleteGlyph &machine
        | SubstKind.AlternateRandom ->
            touch &machine
            unsafeToBreak &machine 0 machine.Len
            machine.Passive <- false
            machine.LookupPassive <- false
            setRandomState &machine outcome.RandomState
            lookupReplace &machine outcome.Glyph outcome.Props
        | SubstKind.Multiple -> multipleOutput &lookup &machine (lookup.MultipleComponents(outcome.Subtable, outcome.Index))
        | _ ->
            match ligatureSet &lookup &machine outcome.Subtable outcome.Index with
            | true -> ()
            | false -> applyOutcome advance &lookup &machine (lookup.TryAfter(outcome.Subtable, inputInfos &machine, inputProps &machine, machine.Idx, machine.RandomState))

    /// <summary>Marks the unsafe-to-concat ranges the failed rules of a context lookup owe: the plain marks of the ranges
    /// of the calls of <c>unsafe_to_concat</c> and <c>unsafe_to_concat_from_outbuffer</c> they made are the marks of their
    /// union.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="owed">The ranges owed.</param>
    let private applyOwed (machine: byref<Machine>) owed =
        match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default || FlagBounds.isNone owed with
        | true -> ()
        | false ->
            let finish = min (max owed.ConcatTo owed.OutEnd) machine.Len
            match owed.OutStart with
            | -1 ->
                match finish > machine.Idx with
                | false -> ()
                | true ->
                    touch &machine
                    setGlyphFlagsImpl &machine GlyphFlagUnsafeToConcat machine.Idx finish false false
            | outStart ->
                match outStart < machine.OutLen || finish > machine.Idx with
                | false -> ()
                | true ->
                    touch &machine
                    setGlyphFlagsImpl &machine GlyphFlagUnsafeToConcat outStart finish false true

    /// <summary>Whether a context lookup fails at the current glyph with nothing to mark: the run produces no unsafe-to-concat flags, so the
    /// ranges the failed subtables owe are dropped, and the lookup says no subtable can match.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <returns>True when the glyph is left as it is.</returns>
    let private failsAtOnce<'L when 'L : struct and 'L :> IGsubLookup> (lookup: byref<'L>) (machine: byref<Machine>) =
        match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
        | false -> false
        | true ->
            match lookup.CannotMatch(inputInfos &machine, inputProps &machine, outputInfos &machine, outputProps &machine, machine.Idx) with
            | true -> true
            | false -> false

    /// <summary>The input positions of a match, a fixed buffer on the stack of the frame that matched, as many entries as
    /// <c>HB_MAX_CONTEXT_LENGTH</c>: the array <c>match_positions</c> of <c>context_apply_lookup</c>.</summary>
    [<Struct; StructLayout(LayoutKind.Sequential, Size = 512)>]
    type private PositionBuffer =
        val mutable First: int

    /// <summary>What one step of the nested-lookup recursion does: apply the records of a matched rule, apply a matched rule,
    /// recurse into a lookup, or match and apply a nested context lookup.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private GsubWork =
        | WorkRecords of inputCount: int * records: ImmutableArray<LookupRecord> * matchEnd: int
        | WorkMatch of outcome: ContextOutcome
        | WorkNested of lookupIndex: int
        | WorkNestedContext of contextIndex: int

    /// <summary>What <c>apply_lookup</c> does with a lookup record.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private RecordStep =
        | RecordSkip
        | RecordStop
        | RecordApply

    /// <summary>Moves match positions, the <c>memmove</c> of <c>apply_lookup</c>, which copies right or left as the move says.</summary>
    /// <param name="positions">The match positions.</param>
    /// <param name="source">The first position to move.</param>
    /// <param name="destination">Where it goes.</param>
    /// <param name="count">The number of positions.</param>
    let private shiftPositions (positions: Span<int>) (source: int) (destination: int) (count: int) =
        match destination > source with
        | true ->
            let mutable shift = count
            while shift > 0 do
                positions[destination + shift - 1] <- positions[source + shift - 1]
                shift <- shift - 1
        | false ->
            let mutable shift = 0
            while shift < count do
                positions[destination + shift] <- positions[source + shift]
                shift <- shift + 1

    /// <summary>Decides what to do with a lookup record, the first half of <c>apply_lookup</c>'s loop body: records past
    /// the match and positions past the buffer are skipped, then the cursor moves to the position, and a spent budget stops.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="seq">The record's sequence index.</param>
    /// <param name="count">The number of match positions.</param>
    /// <param name="origLen">The buffer's length before the record.</param>
    /// <returns>Whether to skip the record, stop the loop or apply the record.</returns>
    let private startRecord (machine: byref<Machine>) (positions: Span<int>) (seq: int) (count: int) (origLen: int) =
        match seq >= count || positions[seq] >= origLen with
        | true -> RecordSkip
        | false ->
            moveTo &machine positions[seq]
            match machine.MaxOps <= 0 with
            | true -> RecordStop
            | false -> RecordApply

    /// <summary>Adjusts the match positions to the change of length a recursed lookup made, the second half of
    /// <c>apply_lookup</c>'s loop body: growth inserts consecutive positions after the record's, shrinkage drops the next
    /// ones, and the end never rewinds past the record's position.</summary>
    /// <param name="positions">The match positions.</param>
    /// <param name="seq">The record's sequence index.</param>
    /// <param name="delta">The change of length, not zero.</param>
    /// <param name="endIndex">The end of the match, adjusted.</param>
    /// <param name="count">The number of match positions, adjusted.</param>
    /// <returns>True when the positions would outgrow <c>HB_MAX_CONTEXT_LENGTH</c> and the loop stops.</returns>
    let private adjustMatchPositions (positions: Span<int>) (seq: int) (delta: int) (endIndex: byref<int>) (count: byref<int>) =
        endIndex <- endIndex + delta
        let clamp = max 0 (positions[seq] - endIndex)
        endIndex <- endIndex + clamp
        let widened = delta + clamp
        match widened > 0 && widened + count > MaxContextLength with
        | true -> true
        | false ->
            let first = seq + 1
            let shrink = match widened > 0 with | true -> 0 | false -> max widened (first - count) - widened
            let moved = widened + shrink
            let next = first - (match widened > 0 with | true -> 0 | false -> moved)
            shiftPositions positions next (next + moved) (count - next)
            count <- count + moved
            let mutable fill = first
            while fill < next + moved do
                positions[fill] <- positions[fill - 1] + 1
                fill <- fill + 1
            let mutable fixup = next + moved
            while fixup < count do
                positions[fixup] <- positions[fixup] + moved
                fixup <- fixup + 1
            false

    /// <summary>Starts recursing into a lookup, <c>recurse</c> (src/hb-ot-layout-gsubgpos.hh:1142-1162): the nesting and
    /// operation budgets are checked and the nesting budget is spent.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>True when the recursion goes on.</returns>
    let private enterNested (machine: byref<Machine>) =
        match machine.Nesting with
        | 0 ->
            machine.Successful <- false
            false
        | _ ->
            machine.MaxOps <- machine.MaxOps - 1
            match machine.MaxOps < 0 with
            | true ->
                machine.Successful <- false
                false
            | false ->
                machine.Nesting <- machine.Nesting - 1
                true

    /// <summary>Applies one step of the nested-lookup recursion: <c>apply_lookup</c>
    /// (src/hb-ot-layout-gsubgpos.hh:2731-2873), the match handling of <c>context_apply_lookup</c> and
    /// <c>chain_context_apply_lookup</c>, and <c>dispatch_recurse_func</c> (src/hb-ot-layout-gsub-table.hh:88-110). The
    /// recursion is one self-recursive function over a struct union, as deep as <c>HB_MAX_NESTING_LEVEL</c> allows.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="work">The step.</param>
    /// <param name="settings">The nested settings.</param>
    /// <param name="positions">The match positions of the rule being applied, in input-buffer coordinates until the
    /// records are applied.</param>
    let rec private applyNested<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup>
            (program: byref<'P>) (machine: byref<Machine>) (work: GsubWork) (settings: NestSettings) (positions: Span<int>) =
        match work with
        | WorkRecords (inputCount, records, matchEnd) ->
            let baseDelta = machine.OutLen - machine.Idx
            let mutable endIndex = matchEnd + baseDelta
            let mutable count = inputCount
            let mutable adjust = 0
            while baseDelta <> 0 && adjust < count do
                positions[adjust] <- positions[adjust] + baseDelta
                adjust <- adjust + 1
            let mutable index = 0
            let mutable stop = false
            while index < records.Length && machine.Successful && not stop do
                let seq = records[index].SequenceIndex
                let origLen = machine.OutLen + (machine.Len - machine.Idx)
                match startRecord &machine positions seq count origLen with
                | RecordSkip -> ()
                | RecordStop -> stop <- true
                | RecordApply ->
                    applyNested &program &machine (WorkNested records[index].LookupListIndex) settings positions
                    match machine.OutLen + (machine.Len - machine.Idx) - origLen with
                    | 0 -> ()
                    | delta -> stop <- adjustMatchPositions positions seq delta &endIndex &count
                index <- index + 1
            moveTo &machine endIndex
        | WorkMatch outcome ->
            touch &machine
            applyOwed &machine outcome.Owed
            match outcome.Chain with
            | true -> unsafeToBreakFromOutbuffer &machine outcome.StartIndex outcome.EndIndex
            | false -> unsafeToBreak &machine machine.Idx outcome.MatchEnd
            applyNested &program &machine (WorkRecords (outcome.InputCount, outcome.Records, outcome.MatchEnd)) settings positions
            match outcome.AfterConcat > machine.Idx with
            | true -> lookupUnsafeToConcat &machine machine.Idx outcome.AfterConcat
            | false -> ()
        | WorkNested lookupIndex ->
            match enterNested &machine with
            | false -> ()
            | true ->
                let mutable nested = program.NestedLookup(lookupIndex, settings.Mask, settings.Random, settings.AutoZwnj, settings.AutoZwj, settings.PerSyllable)
                match nested.HasContext() with
                | false -> applyOutcome false &nested &machine (nested.Try(inputInfos &machine, inputProps &machine, machine.Idx, machine.RandomState))
                | true -> applyNested &program &machine (WorkNestedContext lookupIndex) settings positions
                machine.Nesting <- machine.Nesting + 1
        | WorkNestedContext contextIndex ->
            let mutable nested = program.NestedLookup(contextIndex, settings.Mask, settings.Random, settings.AutoZwnj, settings.AutoZwj, settings.PerSyllable)
            let mutable buffer = PositionBuffer()
            let nestedPositions = MemoryMarshal.CreateSpan(&buffer.First, ContextBufferLength)
            match failsAtOnce &nested &machine with
            | true -> ()
            | false ->
                let outcome =
                    match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
                    | true -> nested.TryContextDropping(inputInfos &machine, inputProps &machine, outputInfos &machine, outputProps &machine, machine.Idx, nestedPositions)
                    | false -> nested.TryContext(inputInfos &machine, inputProps &machine, outputInfos &machine, outputProps &machine, machine.Idx, nestedPositions)
                match outcome.Matched with
                | true -> applyNested &program &machine (WorkMatch outcome) settings nestedPositions
                | false -> applyOwed &machine outcome.Owed

    /// <summary>Visits the glyph at the cursor with a context lookup: the lookup refuses it at once, or its subtables are tried and the rule
    /// that matches is applied, or the ranges the subtables that failed owe are marked and the cursor moves on.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="settings">The nested settings.</param>
    /// <param name="positions">The match positions.</param>
    let private applyContextVisit<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (lookup: byref<'L>) (machine: byref<Machine>) (settings: NestSettings) (positions: Span<int>) =
        match failsAtOnce &lookup &machine with
        | true -> nextGlyphs &machine 1
        | false ->
            let outcome =
                match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
                | true -> lookup.TryContextDropping(inputInfos &machine, inputProps &machine, outputInfos &machine, outputProps &machine, machine.Idx, positions)
                | false -> lookup.TryContext(inputInfos &machine, inputProps &machine, outputInfos &machine, outputProps &machine, machine.Idx, positions)
            match outcome.Matched with
            | true -> applyNested &program &machine (WorkMatch outcome) settings positions
            | false ->
                applyOwed &machine outcome.Owed
                nextGlyphs &machine 1

    /// <summary>Moves the glyphs from the cursor to a stop to the output as <c>apply_forward</c> moves them (src/hb-ot-layout.cc:1976-2003):
    /// the glyphs the digest, the mask and the glyph properties of the lookup refuse in one move, which ensures the final size at once, and
    /// each glyph that passes them and that no subtable matches by itself with <c>next_glyph</c>, which ensures room for one more glyph. The
    /// two differ only when the length limit is checked, which is when the size reaches the allocation, and the lookups that walk only the
    /// glyphs their subtables cover skip the second kind of glyph in the first kind of move: when the moves would reach the allocation, those
    /// glyphs are found again by the gate of the digest, so that the allocation grows past the limit, or the buffer fails, where HarfBuzz's does.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="index">The position of the lookup in the program.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="stop">The index of the glyph the walk stops at, or the length of the run.</param>
    let private skipGlyphs<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (index: int) (machine: byref<Machine>) (stop: int) =
        match machine.HaveOutput && (machine.OutIsSeparate || machine.OutLen <> machine.Idx) && machine.OutLen + (stop - machine.Idx) >= machine.Allocated with
        | false -> nextGlyphs &machine (stop - machine.Idx)
        | true ->
            let mutable gate = program.Lookup(index, true)
            let mutable running = true
            while running && machine.Successful do
                let candidate = min stop (gate.Scan(inputInfos &machine, inputProps &machine, machine.Idx))
                match candidate > machine.Idx with
                | true -> nextGlyphs &machine (candidate - machine.Idx)
                | false -> ()
                match candidate < stop && machine.Successful with
                | true -> nextGlyphs &machine 1
                | false -> running <- false

    /// <summary>Moves the cursor of a forward lookup to the next glyph the lookup may match, as <c>apply_forward</c>'s skip loop does: opens
    /// the output when there is one to reach, and moves the glyphs before it. When there is none the rest of the glyphs are moved and the
    /// lookup is over.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="index">The position of the lookup in the program.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <returns>True when the cursor stands on a glyph the lookup may match.</returns>
    let private seekCandidate<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (index: int) (lookup: byref<'L>) (machine: byref<Machine>) =
        let found =
            match machine.Idx >= machine.Len || not machine.Successful with
            | true -> machine.Len
            | false -> lookup.Scan(inputInfos &machine, inputProps &machine, machine.Idx)
        match found >= machine.Len with
        | true ->
            match machine.HaveOutput && machine.Successful && machine.Idx < machine.Len with
            | true -> skipGlyphs &program index &machine machine.Len
            | false -> ()
            false
        | false ->
            lookupOpen &machine
            match found > machine.Idx with
            | true -> skipGlyphs &program index &machine found
            | false -> ()
            true

    /// <summary>Applies a lookup with no context subtable from the cursor to the end of the run, <see cref="applyForward"/>: each glyph it
    /// may match is tried and the outcome applied.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="index">The position of the lookup in the program.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private applyForwardPlain<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (index: int) (lookup: byref<'L>) (machine: byref<Machine>) =
        while seekCandidate &program index &lookup &machine do
            let outcome = lookup.Try(inputInfos &machine, inputProps &machine, machine.Idx, machine.RandomState)
            match outcome.Kind with
            | SubstKind.Replace -> lookupReplace &machine outcome.Glyph outcome.Props
            | _ -> applyOutcome true &lookup &machine outcome

    /// <summary>Applies a lookup with a context subtable from the cursor to the end of the run, <see cref="applyForward"/>: each glyph it may
    /// match is visited with the buffer of match positions of this frame.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="index">The position of the lookup in the program.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private applyForwardContext<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (index: int) (lookup: byref<'L>) (machine: byref<Machine>) =
        let struct (mask, random, autoZwnj, autoZwj, perSyllable) = lookup.Nest()
        let settings = NestSettings(mask, random, autoZwnj, autoZwj, perSyllable)
        let mutable buffer = PositionBuffer()
        while seekCandidate &program index &lookup &machine do
            applyContextVisit &program &lookup &machine settings (MemoryMarshal.CreateSpan(&buffer.First, ContextBufferLength))

    /// <summary>Applies one lookup from the cursor to the end of the run, <c>apply_forward</c>: skips to the next glyph the
    /// lookup may match, applies it, and goes on.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="index">The position of the lookup in the program.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private applyForward<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (index: int) (lookup: byref<'L>) (machine: byref<Machine>) =
        match lookup.HasContext() with
        | false -> applyForwardPlain &program index &lookup &machine
        | true -> applyForwardContext &program index &lookup &machine

    /// <summary>Marks the unsafe-to-concat range the subtables of a reverse lookup that failed owe, in place: a reverse lookup
    /// runs on the run itself, so the output-buffer range is a range of the input.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="owed">The ranges owed.</param>
    let private applyReverseOwed (machine: byref<Machine>) owed =
        match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default || owed.OutStart < 0 with
        | true -> ()
        | false ->
            let finish = min owed.OutEnd machine.Len
            match finish > owed.OutStart with
            | false -> ()
            | true ->
                touch &machine
                setGlyphFlagsImpl &machine GlyphFlagUnsafeToConcat owed.OutStart finish false true

    /// <summary>Applies one reverse lookup from the end of the run back to the start, <c>apply_backward</c>
    /// (src/hb-ot-layout.cc:2010-2028): in place, one glyph at a time, the cursor going down.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private applyBackward<'L when 'L : struct and 'L :> IGsubLookup> (lookup: byref<'L>) (machine: byref<Machine>) =
        let mutable index = machine.Len - 1
        while index >= 0 do
            let infos = inputInfos &machine
            let props = inputProps &machine
            match lookup.ReverseAdmits(infos, props, index) with
            | false -> ()
            | true ->
                match lookup.ReverseTry(infos, props, index) with
                | ReverseMiss owed -> applyReverseOwed &machine owed
                | ReverseSubstitute (substitute, start, endIdx, owed) ->
                    applyReverseOwed &machine owed
                    touch &machine
                    unsafeToBreakFromOutbuffer &machine start endIdx
                    let props = inputProps &machine
                    setInputGlyph &machine index substitute (lookup.ReverseReplace(substitute, props[index]))
            index <- index - 1

    /// <summary>Records one input position of a context match in the match-position buffer of the frame that matches,
    /// the write the context matchers make, which only this module performs.</summary>
    /// <param name="positions">The buffer of at most <c>HB_MAX_CONTEXT_LENGTH</c> positions.</param>
    /// <param name="index">The component's index in the match.</param>
    /// <param name="position">The glyph's index in the run.</param>
    let setMatchPosition (positions: Span<int>) index position =
        positions[index] <- position

    /// <summary>The positions whose lookups cover some glyph of a run, one bit each.</summary>
    /// <param name="table">The table of <see cref="glyphLookupTable"/>.</param>
    /// <param name="infos">The glyphs, whose code points are glyph ids.</param>
    /// <returns>The union of the table's words of the run's glyphs.</returns>
    let lookupsCovering (table: ImmutableArray<uint64>) (infos: ReadOnlySpan<GlyphInfo>) =
        let words = table.AsSpan()
        let mutable bits = 0UL
        let mutable index = 0
        while index < infos.Length do
            let glyph = int infos[index].Codepoint
            bits <- bits ||| (match uint glyph < uint words.Length with | true -> words[glyph] | false -> 0UL)
            index <- index + 1
        bits

    /// <summary>One pass over the glyphs of a run that gives both what <see cref="lookupsCovering"/> and <c>Digest.ofSpan</c>
    /// give, the two things the gates of the lookups read.</summary>
    /// <param name="table">The table of <see cref="glyphLookupTable"/>.</param>
    /// <param name="infos">The glyphs, whose code points are glyph ids.</param>
    /// <returns>The union of the table's words of the run's glyphs and the digest of the glyphs.</returns>
    let lookupsAndDigest (table: ImmutableArray<uint64>) (infos: ReadOnlySpan<GlyphInfo>) =
        let words = table.AsSpan()
        let mutable bits = 0UL
        let mutable digest = Digest.empty
        let mutable index = 0
        while index < infos.Length do
            let glyph = infos[index].Codepoint
            bits <- bits ||| (match glyph < uint32 words.Length with | true -> words[int glyph] | false -> 0UL)
            digest <- Digest.add glyph digest
            index <- index + 1
        struct (bits, digest)

    /// <summary>The lookups of a program whose subtables cover some glyph of the run, one bit per position, or every bit when
    /// the program holds no table or the run shows what a lookup that changes nothing does.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="machine">The machine.</param>
    let private candidatesOf<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: 'P) (machine: inref<Machine>) =
        match program.GlyphLookups.IsEmpty || machine.Normalise || not machine.Successful with
        | true -> UInt64.MaxValue
        | false -> lookupsCovering program.GlyphLookups (inputInfos &machine)

    /// <summary>Looks at the run's glyphs once for what the gates of the lookups read: sets the machine's digest and its candidates, in one
    /// pass over the glyphs when the program has a table of candidates and the run shows what a lookup that changes nothing does not.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="machine">The machine, whose digest and candidates are set.</param>
    let private lookAtGlyphs<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (machine: byref<Machine>) =
        match program.GlyphLookups.IsEmpty || machine.Normalise || not machine.Successful with
        | true ->
            machine.Digest <- Digest.ofSpan (inputInfos &machine)
            machine.Candidates <- UInt64.MaxValue
        | false ->
            let struct (bits, digest) = lookupsAndDigest program.GlyphLookups (inputInfos &machine)
            machine.Digest <- digest
            machine.Candidates <- bits

    /// <summary>The positions covered by the candidate bits: one bit each for the first 64 lookups of a program.</summary>
    [<Literal>]
    let private CandidatePositions = 64

    /// <summary>The next position at or after an index that a program may admit: with the table of candidates, the next
    /// of the first 64 positions whose bit is set, then every position from the 65th on; a lookup whose bit is clear is
    /// never admitted when the table is present, so it is not visited at all.</summary>
    /// <param name="candidates">The candidate bits, all set when the program has no table or the run shows what a lookup
    /// that changes nothing does.</param>
    /// <param name="index">The first position to consider.</param>
    /// <returns>The position, at or after <paramref name="index"/>; the first position beyond the bits when none is set.</returns>
    let private nextCandidate (candidates: uint64) (index: int) =
        match index < CandidatePositions with
        | false -> index
        | true ->
            match candidates >>> index with
            | 0UL -> CandidatePositions
            | rest -> index + BitOperations.TrailingZeroCount rest

    /// <summary>Applies one admitted lookup to the run, backward when it is a reverse lookup and forward otherwise.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="index">The position of the lookup in the program.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private applyAdmitted<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (index: int) (lookup: byref<'L>) (machine: byref<Machine>) =
        match program.IsReverse index with
        | true -> applyBackward &lookup &machine
        | false -> applyForward &program index &lookup &machine

    /// <summary>The glyph ids a table of decisions of a lookup that decides on one glyph alone is built for: a lookup that covers a glyph
    /// beyond them keeps asking its subtables.</summary>
    [<Literal>]
    let PerGlyphTableLimit = 16384

    /// <summary>The decision value of a glyph the fused chain does not carry out.</summary>
    [<Literal>]
    let PerGlyphRefused = Int32.MinValue

    /// <summary>The entry of the table of decisions for what a lookup does to a glyph.</summary>
    /// <param name="step">What the lookup does.</param>
    /// <param name="sequences">The sequences collected so far, which a multiple substitution adds to.</param>
    /// <returns>The entry.</returns>
    let private perGlyphDecision (step: PerGlyphStep) (sequences: List<ImmutableArray<uint32>>) =
        match step.Kind with
        | PerGlyphKind.Miss -> 0
        | PerGlyphKind.Replace -> (match step.Glyph >= uint32 Int32.MaxValue with | true -> PerGlyphRefused | false -> int step.Glyph + 1)
        | PerGlyphKind.Multiple ->
            sequences.Add step.Components
            -sequences.Count
        | PerGlyphKind.Refused -> PerGlyphRefused
        | _ -> PerGlyphRefused

    /// <summary>Whether a table of decisions holds an entry the fused chain does not carry out.</summary>
    /// <param name="decisions">The table.</param>
    /// <returns>True when one entry is <see cref="PerGlyphRefused"/>.</returns>
    let anyPerGlyphRefused (decisions: int array) =
        Array.contains PerGlyphRefused decisions

    /// <summary>The length of the longest of the sequences of a table of decisions, one when there are none.</summary>
    /// <param name="sequences">The sequences.</param>
    /// <returns>The length, at least one.</returns>
    let longestSequence (sequences: ImmutableArray<uint32> array) =
        let mutable longest = 1
        for sequence in sequences do
            longest <- max longest sequence.Length
        longest

    /// <summary>Builds the table of decisions of a lookup that decides on one glyph alone, one entry for each glyph up to the last
    /// the lookup covers: 0 for a glyph the lookup leaves alone, the replacement glyph plus one for a replacement, minus the index of the
    /// sequence in the returned array minus one for a multiple substitution, <see cref="PerGlyphRefused"/> for any other outcome.</summary>
    /// <param name="covered">The glyphs the lookup's subtables cover, one bit each.</param>
    /// <param name="decide">What the lookup does to a glyph, the same whatever its mask and properties.</param>
    /// <returns>The table and the sequences, or an empty table when the lookup covers a glyph beyond <see cref="PerGlyphTableLimit"/>.</returns>
    let perGlyphDecisions (covered: ImmutableArray<uint64>) (decide: uint32 -> PerGlyphStep) =
        let words = covered.AsSpan()
        let mutable top = words.Length - 1
        while top >= 0 && words[top] = 0UL do
            top <- top - 1
        match top < 0 with
        | true -> struct (Array.empty<int>, Array.empty<ImmutableArray<uint32>>)
        | false ->
            let last = top * 64 + 63 - BitOperations.LeadingZeroCount words[top]
            match last >= PerGlyphTableLimit with
            | true -> struct (Array.empty<int>, Array.empty<ImmutableArray<uint32>>)
            | false ->
                let decisions = Array.zeroCreate<int> (last + 1)
                let sequences = List<ImmutableArray<uint32>>()
                for glyph in 0 .. last do
                    match (words[glyph >>> 6] >>> (glyph &&& 63)) &&& 1UL with
                    | 0UL -> ()
                    | _ -> decisions[glyph] <- perGlyphDecision (decide (uint32 glyph)) sequences
                struct (decisions, sequences.ToArray())

    /// <summary>A multiple substitution waiting for the rest of its components to go through the lookups that follow it.</summary>
    [<Struct; NoEquality; NoComparison>]
    type private FusedFrame =
        val mutable BaseInfo: GlyphInfo
        val mutable BaseProps: GlyphProps
        val mutable Components: ImmutableArray<uint32>
        val mutable Next: int
        val mutable Level: int

    /// <summary>The state of a fused chain while it carries one glyph through the lookups.</summary>
    [<Struct; NoEquality; NoComparison>]
    type private FusedWork =
        val mutable OutInfo: GlyphInfo array
        val mutable OutProps: GlyphProps array
        val mutable Produced: int
        val mutable Depth: int
        val mutable Level: int
        val mutable Info: GlyphInfo
        val mutable Props: GlyphProps
        val mutable Applied: bool
        val mutable Grew: bool
        val mutable Aborted: bool
        val mutable Active: bool
        val mutable InPlace: bool
        val mutable Frames: FusedFrame array
        val mutable Capacity: int

    /// <summary>The room a fused chain leaves above the length of the run in the arrays it writes.</summary>
    [<Literal>]
    let private FusedSlack = 2

    /// <summary>The bits of the candidate table that stand for the lookups of a chain that a glyph still meets from a position on.</summary>
    /// <param name="chainBits">The bits of the chain's lookups.</param>
    /// <param name="level">The position of the next lookup.</param>
    /// <returns>The bits from the position on.</returns>
    let private fusedFrom (chainBits: uint64) (level: int) =
        match level >= CandidatePositions with
        | true -> 0UL
        | false -> chainBits &&& (UInt64.MaxValue <<< level)

    /// <summary>Makes room for one more glyph in the arrays a fused chain writes, doubling them, or gives the chain up when the run would
    /// pass the length limit.</summary>
    /// <param name="work">The state of the chain.</param>
    /// <param name="maxLen">The length limit of the run.</param>
    let private fusedRoom (work: byref<FusedWork>) (maxLen: int) =
        match work.Produced = work.OutInfo.Length with
        | false -> ()
        | true ->
            match work.Produced >= maxLen with
            | true -> work.Aborted <- true
            | false ->
                Array.Resize(&work.OutInfo, work.OutInfo.Length * 2)
                Array.Resize(&work.OutProps, work.OutProps.Length * 2)

    /// <summary>Writes the glyph that has been through every lookup it meets and takes up the next component waiting for the lookups after
    /// its multiple substitution, or ends the glyph.</summary>
    /// <param name="work">The state of the chain.</param>
    ///     /// <param name="steps">The lookups of the chain.</param>
    /// <param name="first">The position of the first lookup of the chain.</param>
    /// <param name="maxLen">The length limit of the run.</param>
    let private fusedEmit (work: byref<FusedWork>) (steps: IPerGlyph array) (first: int) (maxLen: int) =
        fusedRoom &work maxLen
        match work.Aborted with
        | true -> work.Active <- false
        | false ->
            work.OutInfo[work.Produced] <- work.Info
            work.OutProps[work.Produced] <- work.Props
            work.Produced <- work.Produced + 1
            let mutable popping = true
            while popping do
                match work.Depth with
                | 0 ->
                    popping <- false
                    work.Active <- false
                | depth ->
                    let frame = &work.Frames[depth - 1]
                    match frame.Next < frame.Components.Length with
                    | false -> work.Depth <- depth - 1
                    | true ->
                        let next = frame.Components[frame.Next]
                        work.Props <- steps[frame.Level - first].ComponentProps(frame.BaseProps, next, frame.Next)
                        work.Info <- { frame.BaseInfo with Codepoint = next }
                        work.Level <- frame.Level + 1
                        frame.Next <- frame.Next + 1
                        popping <- false

    /// <summary>Moves the glyphs a chain has written over the run's own arrays into arrays of its own, when a lookup first makes a glyph into
    /// several: the glyphs not yet read are still in the run's arrays, which nothing has been written over beyond the glyphs written.</summary>
    /// <param name="work">The state of the chain.</param>
    let private fusedLeaveInPlace (work: byref<FusedWork>) =
        match work.InPlace with
        | false -> ()
        | true ->
            let info = GC.AllocateUninitializedArray<GlyphInfo> work.Capacity
            let props = GC.AllocateUninitializedArray<GlyphProps> work.Capacity
            Array.Copy(work.OutInfo, info, work.Produced)
            Array.Copy(work.OutProps, props, work.Produced)
            work.OutInfo <- info
            work.OutProps <- props
            work.InPlace <- false

    /// <summary>Makes the room for the multiple substitutions waiting, the first time one waits.</summary>
    /// <param name="work">The state of the chain.</param>
    /// <param name="count">The number of lookups of the chain.</param>
    let private fusedFrames (work: byref<FusedWork>) (count: int) =
        match work.Frames.Length with
        | 0 -> work.Frames <- Array.zeroCreate<FusedFrame> count
        | _ -> ()

    /// <summary>Applies the first lookup of the chain a glyph meets from its level on, and moves the glyph, or the first of its components,
    /// on to the lookups after it.</summary>
    /// <param name="work">The state of the chain.</param>
    ///     /// <param name="steps">The lookups of the chain.</param>
    /// <param name="first">The position of the first lookup of the chain.</param>
    /// <param name="position">The position of the lookup.</param>
    let private fusedStep (work: byref<FusedWork>) (steps: IPerGlyph array) (first: int) (position: int) =
        let step = steps[position - first].Step(work.Info, work.Props)
        match step.Kind with
        | PerGlyphKind.Miss -> work.Level <- position + 1
        | PerGlyphKind.Replace ->
            work.Info <- { work.Info with Codepoint = step.Glyph }
            work.Props <- step.Props
            work.Level <- position + 1
            work.Applied <- true
        | PerGlyphKind.Multiple ->
            fusedLeaveInPlace &work
            fusedFrames &work steps.Length
            let components = step.Components
            let frame = &work.Frames[work.Depth]
            frame.BaseInfo <- work.Info
            frame.BaseProps <- work.Props
            frame.Components <- components
            frame.Next <- 1
            frame.Level <- position
            work.Depth <- work.Depth + 1
            work.Props <- steps[position - first].ComponentProps(work.Props, components[0], 0)
            work.Info <- { work.Info with Codepoint = components[0] }
            work.Level <- position + 1
            work.Applied <- true
            work.Grew <- true
        | PerGlyphKind.Refused ->
            work.Aborted <- true
            work.Active <- false
        | _ ->
            work.Aborted <- true
            work.Active <- false

    /// <summary>The most glyphs one glyph can come to through a chain, the product of the growth of its lookups, or zero when a lookup of the
    /// chain can refuse, and the chain must be applied to arrays of its own. The product stops growing once it passes a limit.</summary>
    /// <param name="steps">The lookups of the chain.</param>
    /// <param name="limit">The product that is too much.</param>
    /// <returns>The product, or zero.</returns>
    let private chainGrowth (steps: IPerGlyph array) (limit: int64) =
        let mutable product = 1L
        let mutable index = 0
        while product > 0L && index < steps.Length do
            let growth = int64 (steps[index].Growth())
            product <- (match growth with | 0L -> 0L | _ -> min limit (product * growth))
            index <- index + 1
        product

    /// <summary>Applies a run of lookups that each decide on one glyph alone to the glyphs of the run, one glyph at a time through all of them:
    /// what applying the lookups one after the other gives, because a lookup of this kind reads nothing but the glyph it stands on, and the
    /// glyphs a multiple substitution makes pass through the lookups after it and not through its own. The arrays the run is written
    /// into are new; when the run would grow past the limit, or a lookup refuses, nothing is changed and the caller applies the lookups one by one.
    /// A lookup that applied nothing leaves the machine as it was; one that applied gives it the new arrays, as the last of the lookups would have
    /// left it after its sync.</summary>
    /// <param name="machine">The machine, between two lookups.</param>
    /// <param name="words">For each glyph, a bit per position of the first 64 lookups that cover it.</param>
    /// <param name="steps">The lookups of the chain.</param>
    /// <param name="first">The position of the first lookup of the chain.</param>
    /// <returns>True when the chain was applied, whether or not it changed the run.</returns>
    let private runFused (machine: byref<Machine>) (words: ImmutableArray<uint64>) (steps: IPerGlyph array) (first: int) =
        let count = steps.Length
        let chainBits = (match first + count >= CandidatePositions with | true -> UInt64.MaxValue | false -> (1UL <<< (first + count)) - 1UL) &&& (UInt64.MaxValue <<< first)
        let inInfo = machine.Info
        let inProps = machine.Props
        let len = machine.Len
        let maxLen = machine.MaxLen
        let capacity = len + (len >>> 1) + FusedSlack
        let growth = chainGrowth steps (int64 maxLen + 1L)
        let inPlace = growth > 0L && growth * int64 len <= int64 maxLen
        let mutable work = Unchecked.defaultof<FusedWork>
        work.InPlace <- inPlace
        work.Capacity <- capacity
        work.OutInfo <- (match inPlace with | true -> inInfo | false -> GC.AllocateUninitializedArray<GlyphInfo> capacity)
        work.OutProps <- (match inPlace with | true -> inProps | false -> GC.AllocateUninitializedArray<GlyphProps> capacity)
        work.Frames <- Array.Empty<FusedFrame>()
        let mutable at = 0
        while at < len && not work.Aborted do
            work.Info <- inInfo[at]
            work.Props <- inProps[at]
            work.Level <- first
            work.Depth <- 0
            work.Active <- true
            while work.Active do
                let glyph = work.Info.Codepoint
                let bits = fusedFrom (match glyph < uint32 words.Length with | true -> words[int glyph] &&& chainBits | false -> 0UL) work.Level
                match bits with
                | 0UL -> fusedEmit &work steps first maxLen
                | _ -> fusedStep &work steps first (BitOperations.TrailingZeroCount bits)
            at <- at + 1
        match work.Aborted || work.Produced > maxLen with
        | true -> false
        | false ->
            match work.Applied with
            | false -> true
            | true ->
                machine.Info <- work.OutInfo
                machine.Props <- work.OutProps
                machine.Len <- work.Produced
                machine.Passive <- false
                machine.LookupPassive <- false
                machine.DigestStale <- true
                machine.Allocated <- growAllocated work.Produced 0
                match work.Grew with
                | true -> machine.Grew <- true
                | false -> ()
                true

    /// <summary>The position after the last of the lookups that decide on one glyph alone which follow one another from a position.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="first">The position of the first lookup.</param>
    /// <param name="limit">The position the chain does not pass.</param>
    /// <returns>The end of the chain, the first position when the lookup there does not decide on one glyph alone.</returns>
    let private fusedEnd<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (first: int) (limit: int) =
        let mutable last = first
        let mutable scanning = true
        while scanning && last < limit do
            match program.PerGlyphAt last with
            | ValueSome _ -> last <- last + 1
            | ValueNone -> scanning <- false
        last

    /// <summary>The lookups of a chain, from the first position to the one after the last.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="first">The position of the first lookup.</param>
    /// <param name="last">The position after the last lookup.</param>
    /// <returns>The lookups.</returns>
    let private fusedSteps<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (first: int) (last: int) =
        let steps = Array.zeroCreate<IPerGlyph> (last - first)
        let mutable position = first
        while position < last do
            match program.PerGlyphAt position with
            | ValueSome step -> steps[position - first] <- step
            | ValueNone -> ()
            position <- position + 1
        steps

    /// <summary>Applies the run of lookups that each decide on one glyph alone which begins at a position, all at once: see
    /// <see cref="runFused"/>. Taken only on a machine that owns plain arrays and shows nothing a lookup that changes nothing would.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="machine">The machine, between two lookups.</param>
    /// <param name="index">The position of the first lookup.</param>
    /// <returns>The position after the last lookup applied, or the position given when the lookups are to be applied one by one.</returns>
    let private fusedChain<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (machine: byref<Machine>) (index: int) =
        match program.Prepared && machine.Successful && not machine.Normalise && not machine.HavePositions && machine.Materialized && not machine.HaveOutput && index < CandidatePositions && not program.GlyphLookups.IsEmpty && isNull machine.Watch with
        | false -> index
        | true ->
            let last = fusedEnd &program index (min program.LookupEnd CandidatePositions)
            match last > index with
            | false -> index
            | true ->
                let steps = fusedSteps &program index last
                match runFused &machine program.GlyphLookups steps index with
                | true -> last
                | false -> index

    /// <summary>Tells the watch of the run what a lookup left, when the run has one.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The position of the lookup.</param>
    /// <param name="serialBefore">The serial before the lookup.</param>
    let private notifyWatch (machine: byref<Machine>) (index: int) (serialBefore: byte) =
        match machine.Watch with
        | null -> machine.Successful
        | watch ->
            match machine.DigestStale || machine.Serial <> serialBefore with
            | true ->
                watch.After(index, machine.Serial, inputProps &machine)
                machine.Successful
            | false -> machine.Successful

    /// <summary>Applies the lookups of a program in order, each to the result of the one before, on one working copy of the
    /// arrays, <c>hb_ot_map_t::apply</c>. Only the lookups whose candidate bit is set are visited, which is every lookup
    /// that <see cref="IGsubProgram{L}.Admits"/> could let through; the digest and the candidates are rebuilt before the
    /// next position is chosen when a lookup changed the run.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="machine">The machine.</param>
    let private applyLookups<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: byref<'P>) (machine: byref<Machine>) =
        let mutable index = 0
        while index < program.LookupEnd do
            match machine.DigestStale with
            | true ->
                lookAtGlyphs &program &machine
                machine.DigestStale <- false
            | false -> ()
            index <- nextCandidate machine.Candidates index
            match index < program.LookupEnd && program.Admits(index, machine.Digest, machine.Candidates, machine.Flags, inputInfos &machine, inputProps &machine, machine.Normalise || not machine.Successful) with
            | false -> ()
            | true ->
                let chainEnd = fusedChain &program &machine index
                match chainEnd > index with
                | true -> index <- chainEnd - 1
                | false ->
                    match machine.Successful with
                    | true -> ()
                    | false -> machine.Passive <- false
                    let opsBefore = machine.MaxOps
                    let serialBefore = machine.Serial
                    machine.LookupPassive <- true
                    let shown = machine.Normalise || not machine.Successful
                    let mutable lookup = (match program.Prepared with | true -> program.LookupAt(index, shown) | false -> program.Lookup(index, shown))
                    applyAdmitted &program index &lookup &machine
                    sync &machine
                    machine.DigestStale <- not (machine.LookupPassive && machine.Successful && machine.MaxOps = opsBefore)
                    notifyWatch &machine index serialBefore |> ignore
            index <- index + 1

    /// <summary>Runs the lookups of a GSUB program over a run, the lookup kernel: each lookup is one loop over the run that
    /// writes through the machine, with no edit and no step, and the working arrays are published once at the end. A known
    /// admission, made over the same glyphs, stands in for the digest and the candidates the kernel would build; it is
    /// used only for a run whose arrays the lookups see as they are (successful, no positions, properties of the run's
    /// length), the runs the candidates describe.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="program">The program.</param>
    /// <param name="admission">What a look at the run's glyphs said, or an unknown admission.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runGsubAdmitted<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (input: RunInput) (program: 'P) (admission: RunAdmission) =
        let mutable program = program
        let normalise = input.HavePositions || not input.Positions.IsEmpty || input.Props.Length <> input.Infos.Length
        match admission.Known && not normalise && input.Successful with
        | true ->
            let mutable machine = newMachine input admission.GlyphDigest normalise true
            machine.Candidates <- admission.Candidates
            applyLookups &program &machine
            finishRun &machine (machine.Passive && machine.MaxOps = machine.InitialMaxOps) input.Positions
        | false ->
            let mutable machine = newMachine input (Digest.ofSpan (input.Infos.AsSpan())) normalise true
            machine.Candidates <- candidatesOf program &machine
            applyLookups &program &machine
            finishRun &machine (machine.Passive && machine.MaxOps = machine.InitialMaxOps) input.Positions

    /// <summary>Runs the lookups of a GSUB program over a run, the lookup kernel, looking at the run's glyphs itself.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="program">The program.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runGsub<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (input: RunInput) (program: 'P) =
        runGsubAdmitted input program { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty }

    /// <summary>Adapts an info-only comparer to the props-aware interface, ignoring the properties.</summary>
    /// <typeparam name="'C">The comparer type.</typeparam>
    [<Struct>]
    type private PropsIgnoring<'C when 'C : struct and 'C :> IGlyphComparer> =
        val Comparer: 'C
        new (comparer) = { Comparer = comparer }
        interface IGlyphPropsComparer with
            member this.Compare (aInfo, _, bInfo, _) = this.Comparer.Compare(aInfo, bInfo)

    /// <summary>Sorts a range of infos with a properties-aware comparer, merging clusters of moved spans, <c>hb_buffer_t::sort</c>.</summary>
    /// <typeparam name="'C">The comparer type.</typeparam>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="clusterLevel">The cluster level.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="comparer">The comparer.</param>
    /// <returns>The sorted infos, properties, remaining budget and success flag.</returns>
    let sortClustersByProps<'C when 'C : struct and 'C :> IGlyphPropsComparer>
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (clusterLevel: ClusterLevel) (maxOps: int) (start: int) (finish: int) (comparer: 'C) =
        let len = infos.Length
        let allocated = max 1 len
        let infoArray = Array.zeroCreate allocated
        let propsArray = Array.zeroCreate allocated
        infos.CopyTo(Span<GlyphInfo>(infoArray))
        match props.Length > 0 with
        | true -> props.CopyTo(Span<GlyphProps>(propsArray))
        | false -> ()
        let mutable machine =
            { Info = infoArray
              OutInfo = Array.zeroCreate allocated
              Props = propsArray
              OutProps = Array.zeroCreate allocated
              Positions = Array.zeroCreate allocated
              SourceInfos = ImmutableArray.Empty
              SourceProps = ImmutableArray.Empty
              SourcePositions = ImmutableArray.Empty
              Materialized = true
              PositionsMaterialized = true
              OutIsSeparate = false
              Idx = 0
              Len = len
              OutLen = 0
              HaveOutput = false
              HavePositions = false
              MaxOps = maxOps
              Successful = true
              Passive = false
              MaxLen = DefaultMaxLen
              ClusterLevel = clusterLevel
              Flags = BufferFlags.Default
              Serial = 0uy
              RandomState = 1u
              ScratchFlags = ScratchFlags.Default
              Allocated = infoArray.Length
              Digest = Digest.empty
              DigestStale = false
              Candidates = UInt64.MaxValue
              LookupPassive = true
              Normalise = false
              Exact = false
              Nesting = MaxNestingLevel
              InitialMaxOps = maxOps
              Shifted = false
              Grew = false
              Watch = null }
        let mutable i = start + 1
        while i < finish do
            let mutable j = i
            while j > start && comparer.Compare(infoArray[j - 1], propsArray[j - 1], infoArray[i], propsArray[i]) > 0 do
                j <- j - 1
            match i = j with
            | true -> ()
            | false ->
                mergeClusters &machine j (i + 1)
                let item = infoArray[i]
                let itemProps = propsArray[i]
                Array.Copy(infoArray, j, infoArray, j + 1, i - j)
                Array.Copy(propsArray, j, propsArray, j + 1, i - j)
                infoArray[j] <- item
                propsArray[j] <- itemProps
            i <- i + 1
        struct (publishRange infoArray len, publishRange propsArray len, machine.MaxOps, machine.Successful)

    /// <summary>Sorts a range of infos, merging clusters of moved spans, <c>hb_buffer_t::sort</c>.</summary>
    /// <typeparam name="'C">The comparer type.</typeparam>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="clusterLevel">The cluster level.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="comparer">The comparer.</param>
    /// <returns>The sorted infos, properties, remaining budget and success flag.</returns>
    let sortClusters<'C when 'C : struct and 'C :> IGlyphComparer>
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (clusterLevel: ClusterLevel) (maxOps: int) (start: int) (finish: int) (comparer: 'C) =
        sortClustersByProps infos props clusterLevel maxOps start finish (PropsIgnoring(comparer))

    /// <summary>Finds the end of a group from the candidate continuation index, <c>group_end</c>.</summary>
    /// <typeparam name="'M">The grouping type.</typeparam>
    /// <param name="grouping">The grouping.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The index of the first candidate continuation.</param>
    /// <returns>The exclusive end index of the group.</returns>
    [<TailCall>]
    let rec private groupEnd<'M when 'M : struct and 'M :> IGlyphGrouping> (grouping: 'M) (props: ReadOnlySpan<GlyphProps>) (index: int) : int =
        match index < props.Length && grouping.Group(props[index - 1], props[index]) with
        | true -> groupEnd grouping props (index + 1)
        | false -> index

    /// <summary>Reverses a range of an array in place.</summary>
    /// <param name="items">The array.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let private reverseArrayRange (items: 'T array) (start: int) (finish: int) =
        let mutable lo = start
        let mutable hi = finish - 1
        while lo < hi do
            let temporary = items[lo]
            items[lo] <- items[hi]
            items[hi] <- temporary
            lo <- lo + 1
            hi <- hi - 1

    /// <summary>Walks the groups left to right, merging and reversing each, <c>reverse_groups</c>'s loop.</summary>
    /// <typeparam name="'M">The grouping type.</typeparam>
    /// <param name="grouping">The grouping.</param>
    /// <param name="merge">Whether each group's clusters are merged before it is reversed.</param>
    /// <param name="machine">The out-buffer machine whose infos and properties are reversed in place.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="havePositions">Whether positions are present.</param>
    /// <param name="props">The original glyph properties the grouping reads.</param>
    /// <returns>The machine with its remaining budget and success flag.</returns>
    let private reverseGroupsLoop<'M when 'M : struct and 'M :> IGlyphGrouping>
            (grouping: 'M)
            (merge: bool)
            (machine: byref<Machine>)
            (positions: GlyphPosition array)
            (havePositions: bool)
            (props: ReadOnlySpan<GlyphProps>)
            : unit =
        let mutable start = 0
        let mutable running = true
        while running do
            match start >= machine.Len with
            | true -> running <- false
            | false ->
                let finish = groupEnd grouping props (start + 1)
                match merge with
                | true -> mergeClusters &machine start finish
                | false -> ()
                reverseArrayRange machine.Info start finish
                reverseArrayRange machine.Props start finish
                if havePositions then reverseArrayRange positions start finish
                start <- finish

    /// <summary>Reverses each group, then the whole span, <c>reverse_groups</c>.</summary>
    /// <typeparam name="'M">The grouping type.</typeparam>
    /// <param name="grouping">The grouping.</param>
    /// <param name="mergeClusters">Whether each group's clusters are merged before it is reversed.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="havePositions">Whether positions are present.</param>
    /// <param name="clusterLevel">The cluster level.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the buffer has not failed.</param>
    /// <returns>The reordered infos, properties, positions, remaining budget and success flag.</returns>
    let reverseGroups<'M when 'M : struct and 'M :> IGlyphGrouping>
            (grouping: 'M)
            (mergeClusters: bool)
            (infos: ReadOnlySpan<GlyphInfo>)
            (props: ReadOnlySpan<GlyphProps>)
            (positions: ReadOnlySpan<GlyphPosition>)
            (havePositions: bool)
            (clusterLevel: ClusterLevel)
            (flags: BufferFlags)
            (maxOps: int)
            (successful: bool)
            : struct (ImmutableArray<GlyphInfo> * ImmutableArray<GlyphProps> * ImmutableArray<GlyphPosition> * int * bool) =
        let len = infos.Length
        let infoArray = Array.zeroCreate len
        let propsArray = Array.zeroCreate len
        infos.CopyTo(Span<GlyphInfo>(infoArray))
        props.CopyTo(Span<GlyphProps>(propsArray))
        let positionArray =
            match havePositions with
            | true ->
                let array = Array.zeroCreate len
                positions.CopyTo(Span<GlyphPosition>(array))
                array
            | false -> Array.zeroCreate 0
        let mutable initial =
            { Info = infoArray
              OutInfo = Array.zeroCreate len
              Props = propsArray
              OutProps = Array.zeroCreate len
              Positions = positionArray
              SourceInfos = ImmutableArray.Empty
              SourceProps = ImmutableArray.Empty
              SourcePositions = ImmutableArray.Empty
              Materialized = true
              PositionsMaterialized = true
              OutIsSeparate = false
              Idx = 0
              Len = len
              OutLen = 0
              HaveOutput = false
              HavePositions = havePositions
              MaxOps = maxOps
              Successful = successful
              Passive = false
              MaxLen = DefaultMaxLen
              ClusterLevel = clusterLevel
              Flags = flags
              Serial = 0uy
              RandomState = 1u
              ScratchFlags = ScratchFlags.Default
              Allocated = infoArray.Length
              Digest = Digest.empty
              DigestStale = false
              Candidates = UInt64.MaxValue
              LookupPassive = true
              Normalise = false
              Exact = false
              Nesting = MaxNestingLevel
              InitialMaxOps = maxOps
              Shifted = false
              Grew = false
              Watch = null }
        reverseGroupsLoop grouping mergeClusters &initial positionArray havePositions props
        reverseArrayRange infoArray 0 len
        reverseArrayRange propsArray 0 len
        if havePositions then reverseArrayRange positionArray 0 len
        let resultPositions = if havePositions then publish positionArray else ImmutableArray.Empty
        struct (publish infoArray, publish propsArray, resultPositions, initial.MaxOps, initial.Successful)

    /// <summary>Merges a deleted glyph's cluster backward over the surviving run, <c>delete_glyphs_inplace</c>'s backward merge.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="i">The deleted glyph's index.</param>
    /// <param name="j">The compaction cursor.</param>
    /// <param name="cluster">The deleted glyph's cluster.</param>
    /// <returns>The machine after the backward merge.</returns>
    let private mergeDeletedBackward (machine: byref<Machine>) (i: int) (j: int) (cluster: uint32) : unit =
        match cluster < machine.Info[j - 1].Cluster && machine.Successful with
        | true ->
            let mask = machine.Info[i].Mask
            let oldCluster = machine.Info[j - 1].Cluster
            let mutable k = j
            while k > 0 && machine.Info[k - 1].Cluster = oldCluster do
                k <- k - 1
                machine.Info[k] <- setClusterValue machine.Info[k] cluster mask
            let maxOps = machine.MaxOps - (j - k)
            machine.MaxOps <- maxOps
            machine.Successful <- match maxOps < 0 with | true -> false | false -> machine.Successful
        | false -> ()

    /// <summary>Deletes one filtered glyph in place, merging its cluster, <c>delete_glyphs_inplace</c>'s filtered branch.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="i">The glyph index.</param>
    /// <param name="j">The compaction cursor.</param>
    /// <returns>The machine after the deletion.</returns>
    let private deleteOneFiltered (machine: byref<Machine>) (i: int) (j: int) : unit =
        let cluster = machine.Info[i].Cluster
        match i + 1 < machine.Len && cluster = machine.Info[i + 1].Cluster with
        | true -> ()
        | false ->
            match j > 0 with
            | true -> mergeDeletedBackward &machine i j cluster
            | false ->
                match i + 1 < machine.Len with
                | true -> mergeClusters &machine i (i + 2)
                | false -> ()

    /// <summary>Deletes filtered glyphs in place, merging their clusters, <c>delete_glyphs_inplace</c>.</summary>
    /// <typeparam name="'P">The predicate type.</typeparam>
    /// <param name="predicate">The deletion predicate.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="havePositions">Whether positions are present.</param>
    /// <param name="clusterLevel">The cluster level.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the buffer has not failed.</param>
    /// <returns>The compacted infos, properties, positions, remaining budget and success flag.</returns>
    let deleteGlyphsInplace<'P when 'P : struct and 'P :> IGlyphPredicate>
            (predicate: 'P)
            (infos: ReadOnlySpan<GlyphInfo>)
            (props: ReadOnlySpan<GlyphProps>)
            (positions: ReadOnlySpan<GlyphPosition>)
            (havePositions: bool)
            (clusterLevel: ClusterLevel)
            (maxOps: int)
            (successful: bool)
            : struct (ImmutableArray<GlyphInfo> * ImmutableArray<GlyphProps> * ImmutableArray<GlyphPosition> * int * bool) =
        let len = infos.Length
        let infoArray = Array.zeroCreate len
        let propsArray = Array.zeroCreate len
        infos.CopyTo(Span<GlyphInfo>(infoArray))
        props.CopyTo(Span<GlyphProps>(propsArray))
        let positionArray =
            match havePositions with
            | true ->
                let array = Array.zeroCreate len
                positions.CopyTo(Span<GlyphPosition>(array))
                array
            | false -> Array.zeroCreate 0
        let mutable machine =
            { Info = infoArray
              OutInfo = Array.zeroCreate len
              Props = propsArray
              OutProps = Array.zeroCreate len
              Positions = positionArray
              SourceInfos = ImmutableArray.Empty
              SourceProps = ImmutableArray.Empty
              SourcePositions = ImmutableArray.Empty
              Materialized = true
              PositionsMaterialized = true
              OutIsSeparate = false
              Idx = 0
              Len = len
              OutLen = 0
              HaveOutput = false
              HavePositions = havePositions
              MaxOps = maxOps
              Successful = successful
              Passive = false
              MaxLen = DefaultMaxLen
              ClusterLevel = clusterLevel
              Flags = BufferFlags.Default
              Serial = 0uy
              RandomState = 1u
              ScratchFlags = ScratchFlags.Default
              Allocated = infoArray.Length
              Digest = Digest.empty
              DigestStale = false
              Candidates = UInt64.MaxValue
              LookupPassive = true
              Normalise = false
              Exact = false
              Nesting = MaxNestingLevel
              InitialMaxOps = maxOps
              Shifted = false
              Grew = false
              Watch = null }
        let mutable j = 0
        let mutable i = 0
        while i < len do
            match predicate.Delete(machine.Info[i], machine.Props[i]) with
            | false ->
                match j = i with
                | false ->
                    machine.Info[j] <- machine.Info[i]
                    machine.Props[j] <- machine.Props[i]
                    if havePositions then positionArray[j] <- positionArray[i]
                | true -> ()
                j <- j + 1
            | true -> deleteOneFiltered &machine i j
            i <- i + 1
        let resultPositions = if havePositions then publishRange positionArray j else ImmutableArray.Empty
        struct (publishRange machine.Info j, publishRange machine.Props j, resultPositions, machine.MaxOps, machine.Successful)

    /// <summary>Paints the runs a source emits over a copy of <paramref name="previous"/>, leaving unpainted positions unchanged.</summary>
    /// <typeparam name="'G">The run source type.</typeparam>
    /// <typeparam name="'S">The source state type.</typeparam>
    /// <param name="previous">The previous values, one per painted position.</param>
    /// <param name="source">The run source.</param>
    /// <param name="seed">The initial source state.</param>
    /// <returns>The painted values and the final source state.</returns>
    let paintRuns<'G, 'S when 'G : struct and 'G :> IRunSource<'S>> (previous: ImmutableArray<byte>) (source: 'G) (seed: 'S) : struct (ImmutableArray<byte> * 'S) =
        let target = Array.zeroCreate previous.Length
        previous.AsSpan().CopyTo(Span<byte>(target))
        let mutable state = seed
        let mutable running = true
        while running do
            match source.Next(state) with
            | RunStep.Run (start, stop, value, next) ->
                let mutable index = start
                while index < stop do
                    target[index] <- value
                    index <- index + 1
                state <- next
            | RunStep.Exhausted final ->
                state <- final
                running <- false
        struct (publish target, state)

    /// <summary>Moves the item at <paramref name="from"/> to <paramref name="into"/>, shifting the items in between, <c>memmove</c>-of-one-element semantics.</summary>
    /// <param name="from">The index of the item to move.</param>
    /// <param name="into">The destination index.</param>
    /// <param name="items">The items.</param>
    /// <returns>The items with the element moved.</returns>
    let move (from: int) (into: int) (items: ImmutableArray<'T>) : ImmutableArray<'T> =
        match from = into with
        | true -> items
        | false ->
            let source = items.AsSpan()
            let target = Array.zeroCreate source.Length
            source.CopyTo(Span<'T>(target))
            let item = target[from]
            match from < into with
            | true -> Array.Copy(target, from + 1, target, from, into - from)
            | false -> Array.Copy(target, into, target, into + 1, from - into)
            target[into] <- item
            publish target

    /// <summary>Replaces the item at <paramref name="index"/> with <paramref name="value"/>.</summary>
    /// <param name="index">The index to replace.</param>
    /// <param name="value">The new value.</param>
    /// <param name="items">The items.</param>
    /// <returns>The items with the element replaced.</returns>
    let setItem (index: int) (value: 'T) (items: ImmutableArray<'T>) : ImmutableArray<'T> =
        let source = items.AsSpan()
        let target = Array.zeroCreate source.Length
        source.CopyTo(Span<'T>(target))
        target[index] <- value
        publish target

    /// <summary>Sorts the items with an insertion sort keyed by <paramref name="key"/>, stable and without merging clusters, <c>hb_stable_sort</c>.</summary>
    /// <param name="key">The sort key.</param>
    /// <param name="items">The items.</param>
    /// <returns>The stably sorted items.</returns>
    let stableSortBy (key: 'T -> int) (items: ImmutableArray<'T>) : ImmutableArray<'T> =
        let source = items.AsSpan()
        let target = Array.zeroCreate source.Length
        source.CopyTo(Span<'T>(target))
        let mutable i = 1
        while i < target.Length do
            let item = target[i]
            let itemKey = key item
            let mutable j = i
            while j > 0 && key target[j - 1] > itemKey do
                j <- j - 1
            match j = i with
            | true -> ()
            | false ->
                Array.Copy(target, j, target, j + 1, i - j)
                target[j] <- item
            i <- i + 1
        publish target

    /// <summary>The longest syllable a per-syllable pass walks in one chunk, <c>HB_MAX_SYLLABLE_LENGTH</c>.</summary>
    [<Literal>]
    let private MaxSyllableLength = 64

    /// <summary>Finds the end of the syllable chunk whose first index is <paramref name="index"/> minus one, the loop of <c>_hb_next_syllable</c>.</summary>
    /// <param name="props">The per-glyph properties, read for their syllable bytes.</param>
    /// <param name="syllable">The chunk's syllable byte.</param>
    /// <param name="count">The chunk's exclusive cap.</param>
    /// <param name="index">The next candidate index.</param>
    /// <returns>The first index whose syllable differs, or the cap.</returns>
    [<TailCall>]
    let rec private nextSyllableFrom (props: ReadOnlySpan<GlyphProps>) (syllable: byte) (count: int) (index: int) : int =
        match index < count && syllable = props[index].Syllable with
        | true -> nextSyllableFrom props syllable count (index + 1)
        | false -> index

    /// <summary>Finds the end of the syllable chunk starting at <paramref name="start"/>, <c>_hb_next_syllable</c>.</summary>
    /// <param name="props">The per-glyph properties, read for their syllable bytes.</param>
    /// <param name="start">The chunk's first index.</param>
    /// <returns>The chunk's exclusive end index.</returns>
    let internal nextSyllableEnd (props: ReadOnlySpan<GlyphProps>) (start: int) : int =
        let count = start + min (props.Length - start) MaxSyllableLength
        nextSyllableFrom props props[start].Syllable count (start + 1)

    /// <summary>Merges the maximal leftward run of <paramref name="runCluster"/> into <paramref name="mergedCluster"/>, resetting its flags.</summary>
    /// <param name="rows">The working rows.</param>
    /// <param name="index">The next candidate index, moving left.</param>
    /// <param name="runCluster">The cluster of the run being merged.</param>
    /// <param name="mergedCluster">The cluster the run takes.</param>
    [<TailCall>]
    let rec private mergeRunLeft (rows: GlyphRow array) (index: int) (runCluster: uint32) (mergedCluster: uint32) : unit =
        match index >= 0 && rows[index].Info.Cluster = runCluster with
        | true ->
            rows[index] <- { rows[index] with Info = setClusterValue rows[index].Info mergedCluster 0u }
            mergeRunLeft rows (index - 1) runCluster mergedCluster
        | false -> ()

    /// <summary>Merges the maximal rightward run of <paramref name="runCluster"/> into <paramref name="mergedCluster"/>, resetting its flags.</summary>
    /// <param name="rows">The working rows.</param>
    /// <param name="index">The next candidate index, moving right.</param>
    /// <param name="runCluster">The cluster of the run being merged.</param>
    /// <param name="mergedCluster">The cluster the run takes.</param>
    [<TailCall>]
    let rec private mergeRunRight (rows: GlyphRow array) (index: int) (runCluster: uint32) (mergedCluster: uint32) : unit =
        match index < rows.Length && rows[index].Info.Cluster = runCluster with
        | true ->
            rows[index] <- { rows[index] with Info = setClusterValue rows[index].Info mergedCluster 0u }
            mergeRunRight rows (index + 1) runCluster mergedCluster
        | false -> ()

    /// <summary>Marks the two glyphs on either side of <paramref name="start"/> unsafe to break, <c>unsafe_to_break (start - 1, start + 1)</c>.</summary>
    /// <param name="rows">The working rows.</param>
    /// <param name="start">The chunk's first index, greater than zero.</param>
    let private unsafeToBreakAcross (rows: GlyphRow array) (start: int) : unit =
        match start > 0 with
        | false -> ()
        | true ->
            let mask = GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat
            match rows[start - 1].Info.Cluster < rows[start].Info.Cluster with
            | true -> rows[start] <- { rows[start] with Info = { rows[start].Info with Mask = rows[start].Info.Mask ||| mask } }
            | false ->
                match rows[start - 1].Info.Cluster > rows[start].Info.Cluster with
                | true -> rows[start - 1] <- { rows[start - 1] with Info = { rows[start - 1].Info with Mask = rows[start - 1].Info.Mask ||| mask } }
                | false -> ()

    /// <summary>The general category of the first of the categories a word's glyphs have, <c>HB_UNICODE_GENERAL_CATEGORY_FORMAT</c>.</summary>
    [<Literal>]
    let private GeneralCategoryFormat = 1us

    /// <summary>The general category of the last of the categories a word's glyphs have, <c>HB_UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK</c>.</summary>
    [<Literal>]
    let private GeneralCategoryNonSpacingMark = 12us

    /// <summary>The bits of the unicode properties that hold the general category.</summary>
    [<Literal>]
    let private GeneralCategoryBits = 0x1Fus

    /// <summary>Gives the first glyph of a chunk a mask when the glyph before it is neither a format character, a letter nor a mark,
    /// else marks the two glyphs on either side of the chunk's left edge unsafe to break, the end of
    /// <c>final_reordering_syllable_indic</c> (src/hb-ot-shaper-indic.cc:1430-1439).</summary>
    /// <param name="rows">The working rows.</param>
    /// <param name="start">The chunk's first index.</param>
    /// <param name="mask">The mask of the feature.</param>
    let private initOrUnsafeToBreakAcross (rows: GlyphRow array) (start: int) (mask: uint32) =
        let category = match start > 0 with | true -> rows[start - 1].Props.UnicodeProps &&& GeneralCategoryBits | false -> 0us
        match start > 0 && category >= GeneralCategoryFormat && category <= GeneralCategoryNonSpacingMark with
        | true -> unsafeToBreakAcross rows start
        | false -> rows[start] <- { rows[start] with Info = { rows[start].Info with Mask = rows[start].Info.Mask ||| mask } }

    /// <summary>Applies a chunk's left boundary patches in list order, each walking backward from the chunk's first index.</summary>
    /// <param name="rows">The working rows.</param>
    /// <param name="start">The chunk's first index.</param>
    /// <param name="patches">The patches.</param>
    /// <param name="index">The next patch.</param>
    [<TailCall>]
    let rec private applyLeftPatches (rows: GlyphRow array) (start: int) (patches: ReadOnlySpan<BoundaryPatch>) (index: int) : unit =
        match index < patches.Length with
        | false -> ()
        | true ->
            match patches[index] with
            | BoundaryPatch.MergeRun (runCluster, mergedCluster) -> mergeRunLeft rows (start - 1) runCluster mergedCluster
            | BoundaryPatch.UnsafeToBreakAcross -> unsafeToBreakAcross rows start
            | BoundaryPatch.InitOrUnsafeToBreakAcross mask -> initOrUnsafeToBreakAcross rows start mask
            applyLeftPatches rows start patches (index + 1)

    /// <summary>Applies a chunk's right boundary patches in list order, each walking forward from the chunk's end index.</summary>
    /// <param name="rows">The working rows.</param>
    /// <param name="finish">The chunk's end index.</param>
    /// <param name="patches">The patches.</param>
    /// <param name="index">The next patch.</param>
    [<TailCall>]
    let rec private applyRightPatches (rows: GlyphRow array) (finish: int) (patches: ReadOnlySpan<BoundaryPatch>) (index: int) : unit =
        match index < patches.Length with
        | false -> ()
        | true ->
            match patches[index] with
            | BoundaryPatch.MergeRun (runCluster, mergedCluster) -> mergeRunRight rows finish runCluster mergedCluster
            | BoundaryPatch.UnsafeToBreakAcross -> ()
            | BoundaryPatch.InitOrUnsafeToBreakAcross _ -> ()
            applyRightPatches rows finish patches (index + 1)

    /// <summary>Rewrites each syllable chunk of a run with a rewriter, then applies its boundary patches, <c>foreach_syllable</c> plus the local cluster merges.</summary>
    /// <typeparam name="'R">The rewriter type.</typeparam>
    /// <param name="rewriter">The chunk rewriter.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties, whose syllables define the chunks.</param>
    /// <returns>The rewritten infos and properties.</returns>
    let rewriteSegments<'R when 'R : struct and 'R :> ISegmentRewriter>
            (rewriter: 'R)
            (infos: ReadOnlySpan<GlyphInfo>)
            (props: ReadOnlySpan<GlyphProps>)
            : struct (ImmutableArray<GlyphInfo> * ImmutableArray<GlyphProps>) =
        let len = infos.Length
        let rows = Array.zeroCreate<GlyphRow> len
        let mutable fill = 0
        while fill < len do
            rows[fill] <- { Info = infos[fill]; Props = props[fill] }
            fill <- fill + 1
        let mutable start = 0
        while start < len do
            let finish = nextSyllableEnd props start
            let rewrite = rewriter.Rewrite(ReadOnlySpan<GlyphRow>(rows, start, finish - start))
            match rewrite.Rows.IsDefaultOrEmpty with
            | false ->
                let result = rewrite.Rows.AsSpan()
                let count = min result.Length (finish - start)
                let mutable k = 0
                while k < count do
                    rows[start + k] <- result[k]
                    k <- k + 1
            | true -> ()
            applyLeftPatches rows start (rewrite.Left.AsSpan()) 0
            applyRightPatches rows finish (rewrite.Right.AsSpan()) 0
            start <- finish
        let outInfos = Array.zeroCreate len
        let outProps = Array.zeroCreate len
        let mutable index = 0
        while index < len do
            outInfos[index] <- rows[index].Info
            outProps[index] <- rows[index].Props
            index <- index + 1
        struct (publish outInfos, publish outProps)

    /// <summary>Unfolds an immutable array into an immutable array, the span built inside so the caller's last call passes no span.</summary>
    /// <typeparam name="'E">The source element type.</typeparam>
    /// <typeparam name="'S">The state type.</typeparam>
    /// <typeparam name="'T">The result type.</typeparam>
    /// <typeparam name="'F">The folder type.</typeparam>
    /// <param name="source">The source array.</param>
    /// <param name="count">The number of values.</param>
    /// <param name="seed">The initial state.</param>
    /// <param name="folder">The folder.</param>
    /// <returns>The values.</returns>
    let unfoldImmutable<'E, 'S, 'T, 'F when 'F : struct and 'F :> ISpanUnfolder<'E, 'S, 'T>>
            (source: ImmutableArray<'E>) (count) (seed: 'S) (folder: 'F) : ImmutableArray<'T> =
        let span = source.AsSpan()
        let values = Array.zeroCreate count
        let mutable state = seed
        for index in 0 .. count - 1 do
            let struct (value, next) = folder.Next(span, state)
            values[index] <- value
            state <- next
        publish values

    /// <summary>A folder for <see cref="unfoldPair"/>: one source position yields two values and the next state.</summary>
    type IPairUnfolder<'E, 'S, 'T, 'U> =
        /// <summary>Consumes one source position and returns two values and the next state.</summary>
        /// <param name="source">The source span.</param>
        /// <param name="state">The current state.</param>
        /// <returns>The two values and the next state.</returns>
        abstract Next: source: ReadOnlySpan<'E> * state: 'S -> struct ('T * 'U * 'S)

    /// <summary>Unfolds a span into two parallel immutable arrays in one pass, returning the final state.</summary>
    /// <typeparam name="'E">The source element type.</typeparam>
    /// <typeparam name="'S">The state type.</typeparam>
    /// <typeparam name="'T">The first result type.</typeparam>
    /// <typeparam name="'U">The second result type.</typeparam>
    /// <typeparam name="'F">The folder type.</typeparam>
    /// <param name="source">The source span.</param>
    /// <param name="count">The number of values.</param>
    /// <param name="seed">The initial state.</param>
    /// <param name="folder">The folder.</param>
    /// <returns>The two arrays and the final state.</returns>
    let unfoldPair<'E, 'S, 'T, 'U, 'F when 'F : struct and 'F :> IPairUnfolder<'E, 'S, 'T, 'U>>
            (source: ReadOnlySpan<'E>) (count) (seed: 'S) (folder: 'F) =
        let first = Array.zeroCreate count
        let second = Array.zeroCreate count
        let mutable state = seed
        for index in 0 .. count - 1 do
            let struct (a, b, next) = folder.Next(source, state)
            first[index] <- a
            second[index] <- b
            state <- next
        struct (publish first, publish second, state)

    /// <summary>Runs a pass to completion and returns its finished state, the AAT driver's interpreter loop. Stops
    /// only when the pass returns <c>Finish</c>.</summary>
    /// <typeparam name="'P">The pass type.</typeparam>
    /// <typeparam name="'S">The pass state type.</typeparam>
    /// <param name="pass">The pass.</param>
    /// <param name="passState">The pass state.</param>
    /// <param name="machine">The machine state.</param>
    /// <returns>The finished pass state.</returns>
    let private runLoopWithState<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> (pass: 'P) (passState: 'S) (machine: byref<Machine>) =
        let mutable state = passState
        let mutable finished = false
        while not finished do
            let cursor = makeCursor &machine
            let struct (edit, nextState) = pass.Step(&cursor, state)
            state <- nextState
            match edit.Kind with
            | EditKind.Finish ->
                sync &machine
                finished <- true
            | _ -> applyEdit &machine edit
        state

    /// <summary>Runs one shaping pass over a run's arrays and returns the finished pass state, the AAT driver's
    /// entry point.</summary>
    /// <typeparam name="'P">The pass type.</typeparam>
    /// <typeparam name="'S">The pass state type.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="pass">The pass.</param>
    /// <param name="seed">The initial pass state.</param>
    /// <returns>The run fields the machine produces and the finished pass state.</returns>
    let runPassWithState<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> (input: RunInput) (pass: 'P) (seed: 'S) =
        let len = input.Infos.Length
        let mutable initial =
            { Info = Array.empty
              OutInfo = Array.empty
              Props = Array.empty
              OutProps = Array.empty
              Positions = Array.empty
              SourceInfos = input.Infos
              SourceProps = padTo input.Props len
              SourcePositions = match input.HavePositions with | true -> padTo input.Positions len | false -> input.Positions
              Materialized = false
              PositionsMaterialized = false
              OutIsSeparate = false
              Idx = 0
              Len = len
              OutLen = 0
              HaveOutput = false
              HavePositions = input.HavePositions
              MaxOps = input.MaxOps
              Successful = input.Successful
              Passive = true
              MaxLen = input.MaxLen
              ClusterLevel = input.ClusterLevel
              Flags = input.Flags
              Serial = input.Serial
              RandomState = input.RandomState
              ScratchFlags = input.ScratchFlags
              Allocated = 0
              Digest = Digest.empty
              DigestStale = false
              Candidates = UInt64.MaxValue
              LookupPassive = true
              Normalise = false
              Exact = false
              Nesting = MaxNestingLevel
              InitialMaxOps = input.MaxOps
              Shifted = false
              Grew = false
              Watch = null }
        let finalState = runLoopWithState pass seed &initial
        let unchanged = initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps
        match unchanged with
        | true ->
            struct (
                { Infos = ImmutableArray.Empty
                  Props = ImmutableArray.Empty
                  Positions = ImmutableArray.Empty
                  HavePositions = initial.HavePositions
                  MaxOps = initial.MaxOps
                  Successful = initial.Successful
                  Unchanged = true
                  Serial = initial.Serial
                  RandomState = initial.RandomState
                  ScratchFlags = initial.ScratchFlags },
                finalState)
        | false ->
            let positions = publishPositions &initial input.Positions
            struct (
                { Infos = publishMachineInfos &initial
                  Props = publishMachineProps &initial
                  Positions = positions
                  HavePositions = initial.HavePositions
                  MaxOps = initial.MaxOps
                  Successful = initial.Successful
                  Unchanged = false
                  Serial = initial.Serial
                  RandomState = initial.RandomState
                  ScratchFlags = initial.ScratchFlags },
                finalState)

    /// <summary>The working copy of a run that a shaping program's stages rewrite. The arrays are written only by
    /// the interpreters below, so a stage outside Sealed stays pure; the machine's run-pass and out-buffer logic is
    /// reached through <see cref="runPass"/>, not carried here.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type internal StageSink =
        { mutable Info: GlyphInfo array
          mutable Props: GlyphProps array
          mutable Positions: GlyphPosition array
          mutable SourceInfos: ImmutableArray<GlyphInfo>
          mutable SourceProps: ImmutableArray<GlyphProps>
          mutable SourcePositions: ImmutableArray<GlyphPosition>
          mutable Materialized: bool
          mutable PositionsMaterialized: bool
          mutable Len: int
          mutable HavePositions: bool
          mutable MaxOps: int
          mutable Successful: bool
          mutable Serial: byte
          mutable RandomState: uint32
          mutable ScratchFlags: ScratchFlags
          mutable ClusterLevel: ClusterLevel
          mutable Flags: BufferFlags
          mutable MaxLen: int
          mutable Changed: bool }

    /// <summary>One stage of a shaping program; <see cref="runStages"/> interprets it against one working copy.
    /// A stage describes pure work to the interpreter and never writes an array itself.</summary>
    type internal IStage =
        /// <summary>Describes the stage's work to the interpreter, which applies it to the working copy.</summary>
        /// <param name="sink">The working copy, held by the interpreter.</param>
        abstract Run : sink: byref<StageSink> -> unit

    /// <summary>Two stages in sequence. A whole program is a tree of <c>Then</c> nodes over the concrete stage
    /// structs, one struct type the JIT specialises, with no closure, list or allocation.</summary>
    [<Struct>]
    type internal Then<'A, 'B when 'A : struct and 'A :> IStage and 'B : struct and 'B :> IStage> =
        val First: 'A
        val Second: 'B

        /// <summary>Composes two stages.</summary>
        /// <param name="first">The first stage.</param>
        /// <param name="second">The second stage.</param>
        new (first, second) = { First = first; Second = second }

        interface IStage with
            member this.Run (sink: byref<StageSink>) =
                this.First.Run(&sink)
                this.Second.Run(&sink)

    /// <summary>The current glyph infos a stage reads: the working array once materialized, else the source.</summary>
    /// <param name="sink">The working copy.</param>
    /// <returns>The infos.</returns>
    let private currentInfos (sink: inref<StageSink>) =
        match sink.Materialized with
        | true -> ReadOnlySpan<GlyphInfo>(sink.Info, 0, sink.Len)
        | false -> sink.SourceInfos.AsSpan(0, sink.Len)

    /// <summary>The current glyph properties a stage reads: the working array once materialized, else the source.</summary>
    /// <param name="sink">The working copy.</param>
    /// <returns>The properties.</returns>
    let private currentProps (sink: inref<StageSink>) =
        match sink.Materialized with
        | true -> ReadOnlySpan<GlyphProps>(sink.Props, 0, sink.Len)
        | false -> sink.SourceProps.AsSpan(0, sink.Len)

    /// <summary>Materializes the working infos and properties by copying the source, exactly the run's length.</summary>
    /// <param name="sink">The working copy.</param>
    /// <returns>The working copy with its own infos and properties arrays.</returns>
    let private materializeStage (sink: byref<StageSink>) =
        match sink.Materialized with
        | true -> ()
        | false ->
            let info = Array.zeroCreate<GlyphInfo> sink.Len
            let props = Array.zeroCreate<GlyphProps> sink.Len
            match sink.SourceInfos.IsEmpty with
            | true -> ()
            | false -> sink.SourceInfos.AsSpan(0, sink.Len).CopyTo(Span<GlyphInfo>(info))
            match sink.SourceProps.IsEmpty with
            | true -> ()
            | false -> sink.SourceProps.AsSpan(0, sink.Len).CopyTo(Span<GlyphProps>(props))
            sink.Info <- info
            sink.Props <- props
            sink.Materialized <- true

    /// <summary>Materializes the working positions by copying the source, exactly the run's length.</summary>
    /// <param name="sink">The working copy.</param>
    /// <returns>The working copy with its own positions array.</returns>
    let private materializeStagePositions (sink: byref<StageSink>) =
        match sink.PositionsMaterialized with
        | true -> ()
        | false ->
            let positions = Array.zeroCreate<GlyphPosition> sink.Len
            match sink.HavePositions && not sink.SourcePositions.IsEmpty with
            | true -> sink.SourcePositions.AsSpan(0, sink.Len).CopyTo(Span<GlyphPosition>(positions))
            | false -> ()
            sink.Positions <- positions
            sink.PositionsMaterialized <- true

    /// <summary>Maps every glyph and property pair through a per-glyph mapper, materializing only on the first change.</summary>
    /// <typeparam name="'M">The mapper type.</typeparam>
    /// <param name="sink">The working copy.</param>
    /// <param name="mapper">The per-glyph mapper.</param>
    /// <returns>Unit.</returns>
    let internal mapInfoProps<'M when 'M : struct and 'M :> IInfoPropsMap> (sink: byref<StageSink>) (mapper: 'M) =
        match sink.Materialized with
        | true ->
            let mutable i = 0
            while i < sink.Len do
                let struct (info, props) = mapper.Map(sink.Info[i], sink.Props[i])
                sink.Info[i] <- info
                sink.Props[i] <- props
                i <- i + 1
            sink.Changed <- true
        | false ->
            let source = sink.SourceInfos.AsSpan(0, sink.Len)
            let sourceProps = sink.SourceProps.AsSpan(0, sink.Len)
            let infoComparer = EqualityComparer<GlyphInfo>.Default
            let propsComparer = EqualityComparer<GlyphProps>.Default
            let mutable first = -1
            let mutable firstInfo = Unchecked.defaultof<GlyphInfo>
            let mutable firstProps = Unchecked.defaultof<GlyphProps>
            let mutable i = 0
            while i < sink.Len && first < 0 do
                let struct (info, props) = mapper.Map(source[i], sourceProps[i])
                match infoComparer.Equals(info, source[i]) && propsComparer.Equals(props, sourceProps[i]) with
                | true -> i <- i + 1
                | false ->
                    first <- i
                    firstInfo <- info
                    firstProps <- props
            match first < 0 with
            | true -> ()
            | false ->
                materializeStage &sink
                sink.Info[first] <- firstInfo
                sink.Props[first] <- firstProps
                let mutable j = first + 1
                while j < sink.Len do
                    let struct (info, props) = mapper.Map(sink.Info[j], sink.Props[j])
                    sink.Info[j] <- info
                    sink.Props[j] <- props
                    j <- j + 1
                sink.Changed <- true

    /// <summary>Maps every position through a per-glyph mapper given the glyph and properties, materializing positions
    /// only on the first change.</summary>
    /// <typeparam name="'M">The mapper type.</typeparam>
    /// <param name="sink">The working copy.</param>
    /// <param name="mapper">The per-glyph position mapper.</param>
    /// <returns>Unit.</returns>
    let internal mapPositions<'M when 'M : struct and 'M :> IPositionMap> (sink: byref<StageSink>) (mapper: 'M) =
        let infos = currentInfos &sink
        let props = currentProps &sink
        match sink.PositionsMaterialized with
        | true ->
            let mutable i = 0
            while i < sink.Len do
                sink.Positions[i] <- mapper.Map(infos[i], props[i], sink.Positions[i])
                i <- i + 1
            sink.Changed <- true
        | false ->
            let source = sink.SourcePositions.AsSpan(0, sink.Len)
            let comparer = EqualityComparer<GlyphPosition>.Default
            let mutable first = -1
            let mutable firstPos = Unchecked.defaultof<GlyphPosition>
            let mutable i = 0
            while i < sink.Len && first < 0 do
                let candidate = mapper.Map(infos[i], props[i], source[i])
                match comparer.Equals(candidate, source[i]) with
                | true -> i <- i + 1
                | false ->
                    first <- i
                    firstPos <- candidate
            match first < 0 with
            | true -> ()
            | false ->
                materializeStagePositions &sink
                sink.Positions[first] <- firstPos
                let mutable j = first + 1
                while j < sink.Len do
                    sink.Positions[j] <- mapper.Map(infos[j], props[j], sink.Positions[j])
                    j <- j + 1
                sink.Changed <- true

    /// <summary>Clears the positions and turns them on, <c>hb_buffer_clear_positions</c>.</summary>
    /// <param name="sink">The working copy.</param>
    /// <returns>Unit.</returns>
    let internal clearPositions (sink: byref<StageSink>) =
        sink.Positions <- Array.zeroCreate<GlyphPosition> sink.Len
        sink.PositionsMaterialized <- true
        sink.HavePositions <- true
        sink.Changed <- true

    /// <summary>Publishes the working copy as run fields, zero-copy over the working arrays.</summary>
    /// <param name="sink">The working copy.</param>
    /// <returns>The run fields the machine has built so far.</returns>
    let private publishInput (sink: inref<StageSink>) =
        { Infos = match sink.Materialized with | true -> publishRange sink.Info sink.Len | false -> sink.SourceInfos
          Props = match sink.Materialized with | true -> publishRange sink.Props sink.Len | false -> sink.SourceProps
          Positions = match sink.PositionsMaterialized with | true -> publishRange sink.Positions sink.Len | false -> sink.SourcePositions
          HavePositions = sink.HavePositions
          MaxLen = sink.MaxLen
          MaxOps = sink.MaxOps
          Successful = sink.Successful
          ClusterLevel = sink.ClusterLevel
          Flags = sink.Flags
          Serial = sink.Serial
          RandomState = sink.RandomState
          ScratchFlags = sink.ScratchFlags }

    /// <summary>Replaces the working copy with a run-pass or transform result, dropping the materialized arrays.</summary>
    /// <param name="sink">The working copy.</param>
    /// <param name="result">The result to adopt.</param>
    /// <returns>Unit.</returns>
    let private adoptResult (sink: byref<StageSink>) (result: RunResult) =
        sink.SourceInfos <- result.Infos
        sink.SourceProps <- result.Props
        sink.SourcePositions <- result.Positions
        sink.Info <- Array.empty
        sink.Props <- Array.empty
        sink.Positions <- Array.empty
        sink.Materialized <- false
        sink.PositionsMaterialized <- false
        sink.Len <- result.Infos.Length
        sink.HavePositions <- result.HavePositions
        sink.MaxOps <- result.MaxOps
        sink.Successful <- result.Successful
        sink.Serial <- result.Serial
        sink.RandomState <- result.RandomState
        sink.ScratchFlags <- result.ScratchFlags

    /// <summary>Runs a run pass to completion against the working copy, adopting the result only when it changes the run.</summary>
    /// <typeparam name="'P">The pass type.</typeparam>
    /// <typeparam name="'S">The pass state type.</typeparam>
    /// <param name="sink">The working copy.</param>
    /// <param name="pass">The pass.</param>
    /// <param name="seed">The initial pass state.</param>
    /// <returns>Unit.</returns>
    let internal runPassStage<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> (sink: byref<StageSink>) (pass: 'P) (seed: 'S) =
        let result = runPass (publishInput &sink) pass seed
        match result.Unchanged with
        | true -> ()
        | false ->
            adoptResult &sink result
            sink.Changed <- true

    /// <summary>Applies a whole-run transform to the working copy, adopting the result only when it changes the run.</summary>
    /// <typeparam name="'T">The transform type.</typeparam>
    /// <param name="sink">The working copy.</param>
    /// <param name="transform">The whole-run transform.</param>
    /// <returns>Unit.</returns>
    let internal runTransform<'T when 'T : struct and 'T :> IRunTransform> (sink: byref<StageSink>) (transform: 'T) =
        let result = transform.Transform (publishInput &sink)
        match result.Unchanged with
        | true -> ()
        | false ->
            adoptResult &sink result
            sink.Changed <- true

    /// <summary>Runs a shaping program against one working copy of a run's arrays, publishing once at the end. The
    /// program is a tree of <see cref="Then"/> nodes over concrete stage structs; each stage rewrites the same arrays,
    /// so a group of stages allocates one published copy instead of one array per stage. A program that changes
    /// nothing returns the input run and allocates nothing.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <param name="input">The run fields the program reads.</param>
    /// <param name="program">The program.</param>
    /// <returns>The run fields the program produces.</returns>
    let internal runStages<'P when 'P : struct and 'P :> IStage> (input: RunInput) (program: 'P) =
        let len = input.Infos.Length
        let mutable sink =
            { Info = Array.empty
              Props = Array.empty
              Positions = Array.empty
              SourceInfos = input.Infos
              SourceProps = padTo input.Props len
              SourcePositions = match input.HavePositions with | true -> padTo input.Positions len | false -> input.Positions
              Materialized = false
              PositionsMaterialized = false
              Len = len
              HavePositions = input.HavePositions
              MaxOps = input.MaxOps
              Successful = input.Successful
              Serial = input.Serial
              RandomState = input.RandomState
              ScratchFlags = input.ScratchFlags
              ClusterLevel = input.ClusterLevel
              Flags = input.Flags
              MaxLen = input.MaxLen
              Changed = false }
        program.Run(&sink)
        match sink.Changed with
        | false ->
            { Infos = ImmutableArray.Empty
              Props = ImmutableArray.Empty
              Positions = ImmutableArray.Empty
              HavePositions = sink.HavePositions
              MaxOps = sink.MaxOps
              Successful = sink.Successful
              Unchanged = true
              Serial = sink.Serial
              RandomState = sink.RandomState
              ScratchFlags = sink.ScratchFlags }
        | true ->
            { Infos = match sink.Materialized with | true -> publishRange sink.Info sink.Len | false -> sink.SourceInfos
              Props = match sink.Materialized with | true -> publishRange sink.Props sink.Len | false -> sink.SourceProps
              Positions = match sink.PositionsMaterialized with | true -> publishRange sink.Positions sink.Len | false -> sink.SourcePositions
              HavePositions = sink.HavePositions
              MaxOps = sink.MaxOps
              Successful = sink.Successful
              Unchanged = false
              Serial = sink.Serial
              RandomState = sink.RandomState
              ScratchFlags = sink.ScratchFlags }

    /// <summary>Fills the infos, properties and positions of a simple run from an index, stopping at the first code
    /// point that does not qualify, and notes whether some glyph is in a glyph set.</summary>
    /// <param name="glyphs">The per-glyph decision.</param>
    /// <param name="mask">The mask written into every glyph.</param>
    /// <param name="source">The input glyphs.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="covered">The words of the glyph set whose members the run is checked for.</param>
    /// <param name="infos">The infos being built.</param>
    /// <param name="props">The properties being built.</param>
    /// <param name="positions">The positions being built.</param>
    /// <param name="hit">Whether a glyph so far is in the set.</param>
    /// <param name="index">The next index.</param>
    /// <returns>None when a code point does not qualify, else whether some glyph is in the set.</returns>
    [<TailCall>]
    let rec private simpleFrom<'G when 'G : struct and 'G :> ISimpleGlyphs>
            (glyphs: 'G)
            (mask: uint32)
            (source: ReadOnlySpan<GlyphInfo>)
            (metrics: ReadOnlySpan<byte>)
            (covered: ReadOnlySpan<uint64>)
            (infos: GlyphInfo array)
            (props: GlyphProps array)
            (positions: GlyphPosition array)
            (hit: bool)
            (index: int) =
        match index < source.Length with
        | false -> ValueSome hit
        | true ->
            let info = source[index]
            match glyphs.Glyph(info.Codepoint, metrics) with
            | ValueNone -> ValueNone
            | ValueSome simple ->
                let glyph = simple.SimpleProps.GlyphIndex
                let word = int (glyph >>> 6)
                let hit = hit || (word < covered.Length && (covered[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL)
                infos[index] <- { Codepoint = glyph; Mask = mask; Cluster = info.Cluster; Var1 = info.Var1; Var2 = info.Var2 }
                props[index] <- simple.SimpleProps
                positions[index] <- { XAdvance = simple.SimpleAdvance; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                simpleFrom glyphs mask source metrics covered infos props positions hit (index + 1)

    /// <summary>Shapes a simple run in one loop: the infos, properties and default positions are written once and
    /// published, or the whole run is refused at the first code point that is not simple. The same loop notes whether
    /// some glyph is in a glyph set.</summary>
    /// <param name="glyphs">The per-glyph decision.</param>
    /// <param name="mask">The mask written into every glyph.</param>
    /// <param name="source">The input glyphs.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="coveredSet">The glyph set whose members the run is checked for.</param>
    /// <returns>The three arrays and the membership flag, or none when a code point does not qualify.</returns>
    let simpleRun<'G when 'G : struct and 'G :> ISimpleGlyphs> (glyphs: 'G) (mask: uint32) (source: ReadOnlySpan<GlyphInfo>) (metrics: ReadOnlySpan<byte>) (coveredSet: ImmutableArray<uint64>) =
        let infos = GC.AllocateUninitializedArray<GlyphInfo>(source.Length)
        let props = GC.AllocateUninitializedArray<GlyphProps>(source.Length)
        let positions = GC.AllocateUninitializedArray<GlyphPosition>(source.Length)
        match simpleFrom glyphs mask source metrics (coveredSet.AsSpan()) infos props positions false 0 with
        | ValueNone -> ValueNone
        | ValueSome hit -> ValueSome { KernelInfos = publish infos; KernelProps = publish props; KernelPositions = publish positions; Covered = hit }

    /// <summary>Fills the kernel's arrays from a normalized run, glyph by glyph, stopping at the first glyph that carries a mask other
    /// than the global one, a Unicode property beyond its category, or a class the kernel refuses.</summary>
    [<TailCall>]
    let rec private normalizedFrom<'G when 'G : struct and 'G :> ISimpleClassifier>
            (classifier: 'G)
            (mask: uint32)
            (refusedUnicode: uint16)
            (source: ReadOnlySpan<GlyphInfo>)
            (sourceProps: ReadOnlySpan<GlyphProps>)
            (metrics: ReadOnlySpan<byte>)
            (covered: ReadOnlySpan<uint64>)
            (infos: GlyphInfo array)
            (props: GlyphProps array)
            (positions: GlyphPosition array)
            (hit: bool)
            (index: int) =
        match index < source.Length with
        | false -> ValueSome hit
        | true ->
            let info = source[index]
            let current = sourceProps[index]
            match info.Mask <> mask || current.UnicodeProps &&& refusedUnicode <> 0us with
            | true -> ValueNone
            | false ->
                match classifier.Classify (current.GlyphIndex, current.UnicodeProps, metrics) with
                | ValueNone -> ValueNone
                | ValueSome simple ->
                    let glyph = simple.SimpleProps.GlyphIndex
                    let word = int (glyph >>> 6)
                    let hit = hit || (word < covered.Length && (covered[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL)
                    infos[index] <- { Codepoint = glyph; Mask = mask; Cluster = info.Cluster; Var1 = info.Var1; Var2 = info.Var2 }
                    props[index] <- simple.SimpleProps
                    positions[index] <- { XAdvance = simple.SimpleAdvance; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                    normalizedFrom classifier mask refusedUnicode source sourceProps metrics covered infos props positions hit (index + 1)

    /// <summary>Makes the kernel's arrays of a run the normalizer has finished, the glyphs it chose already in the properties: the
    /// arrays <see cref="simpleRun"/> makes of the same text, or none when some glyph is not one the kernel would accept.</summary>
    /// <param name="classifier">The per-glyph decision.</param>
    /// <param name="mask">The global mask every glyph must carry, and the arrays are written with.</param>
    /// <param name="refusedUnicode">The Unicode property bits that refuse a glyph.</param>
    /// <param name="source">The glyphs.</param>
    /// <param name="sourceProps">Their properties, which carry the glyph indices.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="coveredSet">The glyph set whose members the run is checked for.</param>
    /// <returns>The three arrays and the membership flag, or none.</returns>
    let simpleRunOfNormalized<'G when 'G : struct and 'G :> ISimpleClassifier> (classifier: 'G) (mask: uint32) (refusedUnicode: uint16) (source: ReadOnlySpan<GlyphInfo>) (sourceProps: ReadOnlySpan<GlyphProps>) (metrics: ReadOnlySpan<byte>) (coveredSet: ImmutableArray<uint64>) =
        let infos = GC.AllocateUninitializedArray<GlyphInfo>(source.Length)
        let props = GC.AllocateUninitializedArray<GlyphProps>(source.Length)
        let positions = GC.AllocateUninitializedArray<GlyphPosition>(source.Length)
        match normalizedFrom classifier mask refusedUnicode source sourceProps metrics (coveredSet.AsSpan()) infos props positions false 0 with
        | ValueNone -> ValueNone
        | ValueSome hit -> ValueSome { KernelInfos = publish infos; KernelProps = publish props; KernelPositions = publish positions; Covered = hit }

    /// <summary>Fills the default positions of a substituted run from an index, stopping at the first glyph of a refused
    /// class or with a refused Unicode property.</summary>
    /// <param name="advances">The per-glyph advance.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="refused">The glyph class bits that send the run to the general positioning.</param>
    /// <param name="refusedUnicode">The Unicode property bits that send the run to the general positioning.</param>
    /// <param name="positions">The positions being built.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when no glyph carried a refused class.</returns>
    [<TailCall>]
    let rec private advancesFrom<'A when 'A : struct and 'A :> ISimpleAdvances>
            (advances: 'A)
            (infos: ReadOnlySpan<GlyphInfo>)
            (props: ReadOnlySpan<GlyphProps>)
            (metrics: ReadOnlySpan<byte>)
            (refused: uint16)
            (refusedUnicode: uint16)
            (positions: GlyphPosition array)
            (index: int) =
        match index < infos.Length with
        | false -> true
        | true ->
            match props[index].GlyphClass &&& refused <> 0us || props[index].UnicodeProps &&& refusedUnicode <> 0us with
            | true -> false
            | false ->
                positions[index] <- { XAdvance = advances.Advance(infos[index].Codepoint, metrics); YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                advancesFrom advances infos props metrics refused refusedUnicode positions (index + 1)

    /// <summary>Writes the default positions of a substituted run in one loop, or refuses the run at the first glyph whose
    /// class is in <paramref name="refused"/> or whose Unicode properties meet <paramref name="refusedUnicode"/>.</summary>
    /// <param name="advances">The per-glyph advance.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="refused">The glyph class bits that send the run to the general positioning.</param>
    /// <param name="refusedUnicode">The Unicode property bits that send the run to the general positioning.</param>
    /// <returns>The positions, or none when a glyph carries a refused class or property.</returns>
    let simpleAdvances<'A when 'A : struct and 'A :> ISimpleAdvances> (advances: 'A) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (metrics: ReadOnlySpan<byte>) (refused: uint16) (refusedUnicode: uint16) =
        let positions = GC.AllocateUninitializedArray<GlyphPosition>(infos.Length)
        match advancesFrom advances infos props metrics refused refusedUnicode positions 0 with
        | false -> ValueNone
        | true -> ValueSome (publish positions)

    /// <summary>Fills the default positions of a substituted run from an index, as <see cref="advancesFrom"/> does, stopping also at the
    /// first glyph that lacks a bit of a mask.</summary>
    /// <param name="advances">The per-glyph advance.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="refused">The glyph class bits that send the run to the general positioning.</param>
    /// <param name="refusedUnicode">The Unicode property bits that send the run to the general positioning.</param>
    /// <param name="required">The mask bits every glyph must carry.</param>
    /// <param name="positions">The positions being built.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when every glyph from the index on was accepted.</returns>
    [<TailCall>]
    let rec private leanAdvancesFrom<'A when 'A : struct and 'A :> ISimpleAdvances>
            (advances: 'A)
            (infos: ReadOnlySpan<GlyphInfo>)
            (props: ReadOnlySpan<GlyphProps>)
            (metrics: ReadOnlySpan<byte>)
            (refused: uint16)
            (refusedUnicode: uint16)
            (required: uint32)
            (positions: GlyphPosition array)
            (index: int) =
        match index < infos.Length with
        | false -> true
        | true ->
            let info = infos[index]
            match props[index].GlyphClass &&& refused <> 0us || props[index].UnicodeProps &&& refusedUnicode <> 0us || info.Mask &&& required <> required with
            | true -> false
            | false ->
                positions[index] <- { XAdvance = advances.Advance(info.Codepoint, metrics); YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                leanAdvancesFrom advances infos props metrics refused refusedUnicode required positions (index + 1)

    /// <summary>Writes the default positions of a substituted run into an array of their own for the lean pair loop to adjust, when the
    /// run is one the loop applies to: no glyph is refused (a class in one mask, a Unicode property in another) and every glyph carries a
    /// given mask. Unlike <see cref="leanAdvancesOf"/>, which finishes the flags in place and so needs a cluster to be one glyph, it accepts
    /// glyphs that share a cluster: the caller's finishing of the flags sends such a run through the shared tail.</summary>
    /// <param name="advances">The per-glyph advance.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="refused">The glyph class bits that send the run to the general positioning.</param>
    /// <param name="refusedUnicode">The Unicode property bits that send the run to the general positioning.</param>
    /// <param name="required">The mask bits every glyph must carry.</param>
    /// <returns>The positions, or none when the run is not one the lean loop applies to.</returns>
    let leanAdvances<'A when 'A : struct and 'A :> ISimpleAdvances> (advances: 'A) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (metrics: ReadOnlySpan<byte>) (refused: uint16) (refusedUnicode: uint16) (required: uint32) =
        let positions = GC.AllocateUninitializedArray<GlyphPosition>(infos.Length)
        match leanAdvancesFrom advances infos props metrics refused refusedUnicode required positions 0 with
        | false -> ValueNone
        | true -> ValueSome (publish positions)

    /// <summary>Finishes a substituted simple run in one pass over its clusters: the default positions of its glyphs, and
    /// every glyph's mask propagated over its cluster, <c>hb_propagate_flags</c> without the tatweel rule. The whole run is
    /// refused at the first glyph whose class is in <paramref name="refused"/> or whose Unicode properties meet
    /// <paramref name="refusedUnicode"/>.</summary>
    /// <param name="advances">The per-glyph advance.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="refused">The glyph class bits that send the run to the general positioning.</param>
    /// <param name="refusedUnicode">The Unicode property bits that send the run to the general positioning.</param>
    /// <param name="andMask">The mask the propagated masks are cut down to.</param>
    /// <returns>The infos with their propagated masks and the positions, or none when a glyph is refused.</returns>
    let simpleFinish<'A when 'A : struct and 'A :> ISimpleAdvances> (advances: 'A) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (metrics: ReadOnlySpan<byte>) (refused: uint16) (refusedUnicode: uint16) (andMask: uint32) =
        let outInfos = GC.AllocateUninitializedArray<GlyphInfo>(infos.Length)
        let positions = GC.AllocateUninitializedArray<GlyphPosition>(infos.Length)
        let mutable start = 0
        let mutable accepted = true
        while accepted && start < infos.Length do
            let cluster = infos[start].Cluster
            let mutable finish = start + 1
            let mutable mask = infos[start].Mask
            while finish < infos.Length && infos[finish].Cluster = cluster do
                mask <- mask ||| infos[finish].Mask
                finish <- finish + 1
            let propagated = mask &&& andMask
            let mutable index = start
            while accepted && index < finish do
                match props[index].GlyphClass &&& refused <> 0us || props[index].UnicodeProps &&& refusedUnicode <> 0us with
                | true -> accepted <- false
                | false ->
                    outInfos[index] <- { infos[index] with Mask = propagated }
                    positions[index] <- { XAdvance = advances.Advance(infos[index].Codepoint, metrics); YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                    index <- index + 1
            start <- finish
        match accepted with
        | false -> ValueNone
        | true -> ValueSome (struct (publish outInfos, publish positions))

    /// <summary>Copies glyph infos giving each the same mask, one pass into an array that is written in full.</summary>
    /// <param name="mask">The mask every copy carries.</param>
    /// <param name="source">The glyph infos.</param>
    /// <returns>The copy.</returns>
    let withMask (mask: uint32) (source: ReadOnlySpan<GlyphInfo>) =
        let target = GC.AllocateUninitializedArray<GlyphInfo> source.Length
        let mutable index = 0
        while index < source.Length do
            target[index] <- { source[index] with Mask = mask }
            index <- index + 1
        publish target

    /// <summary>Builds a bit set of glyph ids: bit <c>g</c> is set when <c>g</c> is among the glyphs.</summary>
    /// <param name="glyphArray">The glyphs, with repeats allowed.</param>
    /// <returns>The words of the set, none when there is no glyph.</returns>
    let glyphSet (glyphArray: ImmutableArray<uint32>) =
        let glyphs = glyphArray.AsSpan()
        let mutable top = 0u
        let mutable index = 0
        while index < glyphs.Length do
            top <- max top glyphs[index]
            index <- index + 1
        let words = Array.zeroCreate<uint64> (match glyphs.Length with | 0 -> 0 | _ -> int (top >>> 6) + 1)
        let mutable next = 0
        while next < glyphs.Length do
            let glyph = glyphs[next]
            words[int (glyph >>> 6)] <- words[int (glyph >>> 6)] ||| (1UL <<< int (glyph &&& 63u))
            next <- next + 1
        publish words

    /// <summary>Whether any glyph of a run is in a bit set of glyph ids.</summary>
    /// <param name="set">The words of the set.</param>
    /// <param name="infos">The glyphs, whose code points are glyph ids.</param>
    /// <returns>True when some glyph is in the set.</returns>
    let anyGlyphIn (set: ImmutableArray<uint64>) (infos: ReadOnlySpan<GlyphInfo>) =
        let words = set.AsSpan()
        let mutable found = false
        let mutable index = 0
        while not found && index < infos.Length && words.Length > 0 do
            let glyph = infos[index].Codepoint
            let word = int (glyph >>> 6)
            found <- word < words.Length && (words[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL
            index <- index + 1
        found

    /// <summary>Builds the table that gives, for each glyph, the positions among the first 64 that list it: bit <c>p</c> of
    /// the word of glyph <c>g</c> is set when <c>g</c> is among the glyphs of position <c>p</c>.</summary>
    /// <param name="positions">The glyphs of each position, repeats allowed; positions from the 65th on are ignored.</param>
    /// <returns>The words indexed by glyph, up to the highest glyph listed.</returns>
    let glyphLookupTable (positions: ImmutableArray<ImmutableArray<uint32>>) =
        let count = min positions.Length 64
        let mutable top = 0u
        let mutable total = 0
        let mutable position = 0
        while position < count do
            let glyphs = positions[position].AsSpan()
            let mutable index = 0
            while index < glyphs.Length do
                top <- max top glyphs[index]
                total <- total + 1
                index <- index + 1
            position <- position + 1
        let words = Array.zeroCreate<uint64> (match total with | 0 -> 0 | _ -> int top + 1)
        let mutable next = 0
        while next < count do
            let glyphs = positions[next].AsSpan()
            let mutable index = 0
            while index < glyphs.Length do
                words[int glyphs[index]] <- words[int glyphs[index]] ||| (1UL <<< next)
                index <- index + 1
            next <- next + 1
        publish words

    /// <summary>The union of the glyph sets of the chosen lookups of a table: the set of a plan's map, built from the sets kept with
    /// the face instead of from the subtables.</summary>
    /// <param name="sets">The lazily built glyph set of every lookup of the table.</param>
    /// <param name="chosen">The indices of the lookups the map applies.</param>
    /// <returns>The words of the union, none when no chosen lookup covers a glyph.</returns>
    let unionLazySets (sets: ImmutableArray<Lazy<ImmutableArray<uint64>>>) (chosen: ReadOnlySpan<int>) =
        let mutable length = 0
        let mutable index = 0
        while index < chosen.Length do
            length <- max length (match chosen[index] < sets.Length with | true -> sets[chosen[index]].Value.Length | false -> 0)
            index <- index + 1
        let words = Array.zeroCreate<uint64> length
        index <- 0
        while index < chosen.Length do
            match chosen[index] < sets.Length with
            | false -> ()
            | true ->
                let set = sets[chosen[index]].Value.AsSpan()
                let mutable word = 0
                while word < set.Length do
                    words[word] <- words[word] ||| set[word]
                    word <- word + 1
            index <- index + 1
        publish words

    /// <summary>The per-lookup value of a table for a map, a glyph set or the compiled pair subtables: the face's value for a lookup
    /// the map applies, the empty array for any other.</summary>
    /// <typeparam name="'T">The element type.</typeparam>
    /// <param name="sets">The lazily built value of every lookup of the table.</param>
    /// <param name="chosen">The indices of the lookups the map applies.</param>
    /// <returns>The sets, indexed by the lookup's index in the table.</returns>
    let chosenLazySets<'T> (sets: ImmutableArray<Lazy<ImmutableArray<'T>>>) (chosen: ReadOnlySpan<int>) =
        let flags = Array.zeroCreate<bool> sets.Length
        let mutable index = 0
        while index < chosen.Length do
            match chosen[index] < sets.Length with
            | true -> flags[chosen[index]] <- true
            | false -> ()
            index <- index + 1
        let result = Array.zeroCreate<ImmutableArray<'T>> sets.Length
        index <- 0
        while index < result.Length do
            result[index] <- (match flags[index] with | true -> sets[index].Value | false -> ImmutableArray<'T>.Empty)
            index <- index + 1
        publish result

    /// <summary>Builds the table that gives, for each glyph, the positions among the first 64 of a map that list it, from the sets kept
    /// with the face: the result of <see cref="glyphLookupTable"/> over the glyphs of those sets.</summary>
    /// <param name="sets">The lazily built glyph set of every lookup of the table.</param>
    /// <param name="chosen">The indices of the lookups of the map, in the map's order; those from the 65th on are ignored.</param>
    /// <returns>The words indexed by glyph, up to the highest glyph listed.</returns>
    let glyphLookupTableOfLazySets (sets: ImmutableArray<Lazy<ImmutableArray<uint64>>>) (chosen: ReadOnlySpan<int>) =
        let count = min chosen.Length 64
        let mutable top = -1L
        let mutable position = 0
        while position < count do
            match chosen[position] < sets.Length with
            | false -> ()
            | true ->
                let set = sets[chosen[position]].Value.AsSpan()
                let mutable word = set.Length - 1
                while word >= 0 && set[word] = 0UL do
                    word <- word - 1
                match word >= 0 with
                | true -> top <- max top (int64 word * 64L + 63L - int64 (Numerics.BitOperations.LeadingZeroCount set[word]))
                | false -> ()
            position <- position + 1
        let words = Array.zeroCreate<uint64> (int (top + 1L))
        position <- 0
        while position < count do
            match chosen[position] < sets.Length with
            | false -> ()
            | true ->
                let set = sets[chosen[position]].Value.AsSpan()
                let mutable word = 0
                while word < set.Length do
                    let mutable bits = set[word]
                    while bits <> 0UL do
                        let glyph = word * 64 + Numerics.BitOperations.TrailingZeroCount bits
                        words[glyph] <- words[glyph] ||| (1UL <<< position)
                        bits <- bits &&& (bits - 1UL)
                    word <- word + 1
            position <- position + 1
        publish words

    /// <summary>Sets the mask and the Unicode properties of a run whose every code point is ASCII in one loop, <c>hb_set_unicode_props</c>
    /// for code points that need none of its continuation rules: the properties are the table's, a glyph is a continuation or a
    /// default ignorable only by the table's bits.</summary>
    /// <param name="table">The properties of U+0000 to U+00FF.</param>
    /// <param name="mask">The mask given to every glyph.</param>
    /// <param name="continuation">The continuation bit of the packed properties.</param>
    /// <param name="ignorable">The default-ignorable bits of the packed properties.</param>
    /// <param name="source">The glyphs.</param>
    /// <returns>The glyphs with the mask, their properties and whether some glyph is a continuation and whether some is a default
    /// ignorable, or none at the first code point beyond ASCII.</returns>
    let asciiProps (table: ImmutableArray<struct (uint16 * ScratchFlags)>) (mask: uint32) (continuation: uint16) (ignorable: uint16) (source: ReadOnlySpan<GlyphInfo>) =
        let mutable scanned = 0
        while scanned < source.Length && source[scanned].Codepoint < 0x80u do
            scanned <- scanned + 1
        match scanned < source.Length with
        | true -> ValueNone
        | false ->
            let infos = GC.AllocateUninitializedArray<GlyphInfo>(source.Length)
            let props = GC.AllocateUninitializedArray<GlyphProps>(source.Length)
            let mutable bits = 0us
            let mutable index = 0
            while index < source.Length do
                let struct (unicode, _) = table[int source[index].Codepoint]
                infos[index] <- { source[index] with Mask = mask }
                props[index] <- { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
                bits <- bits ||| unicode
                index <- index + 1
            ValueSome (struct (publish infos, publish props, bits &&& continuation <> 0us, bits &&& ignorable <> 0us))

    /// <summary>The key of an empty slot of a pair index: no pair of two glyphs below 65535 makes it.</summary>
    [<Literal>]
    let PairEmptyKey = 0xFFFFFFFFu

    /// <summary>The multiplier of the pair index's hash, the 32-bit Fibonacci constant.</summary>
    [<Literal>]
    let private PairHashMultiplier = 2654435761u

    /// <summary>The hash of a pair key.</summary>
    /// <param name="key">The key, the first glyph in the high half and the second in the low.</param>
    /// <returns>The hash, whose high bits are the ones used.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let pairHash (key: uint32) =
        key * PairHashMultiplier

    /// <summary>The slot a pair key starts at in an open-addressed index.</summary>
    /// <param name="key">The key, the first glyph in the high half and the second in the low.</param>
    /// <param name="shift">The shift that leaves as many bits as the index has slots.</param>
    /// <returns>The slot.</returns>
    let pairSlot (key: uint32) (shift: int) =
        int (pairHash key >>> shift)

    /// <summary>The number of filter bits for each pair of an index: one filter bit set per pair filters out a miss in a few percent
    /// of probes at this ratio, in a table that stays in the first-level cache for a font of some thousands of pairs.</summary>
    [<Literal>]
    let private PairFilterBitsPerPair = 8

    /// <summary>Checks that the pair sets of a format 1 subtable can be indexed by their two glyphs: every glyph below
    /// 65535, every set strictly increasing, which is what a binary search over the set assumes.</summary>
    /// <param name="firsts">The first glyphs, in the coverage's order.</param>
    /// <param name="cover">The coverage index of each glyph, 0xFFFF for none.</param>
    /// <param name="starts">The first record of each set, and the end of the last.</param>
    /// <param name="seconds">The second glyph of every record.</param>
    /// <returns>The number of pairs, or none when the sets cannot be indexed.</returns>
    let private pairCount (firsts: ReadOnlySpan<uint32>) (cover: ReadOnlySpan<uint16>) (starts: ReadOnlySpan<int>) (seconds: ReadOnlySpan<uint32>) =
        let mutable count = 0
        let mutable valid = true
        let mutable f = 0
        while valid && f < firsts.Length do
            let first = firsts[f]
            valid <- first < 0xFFFFu && first < uint32 cover.Length && cover[int first] <> 0xFFFFus
            match valid && int cover[int first] + 1 < starts.Length with
            | false -> ()
            | true ->
                let low = starts[int cover[int first]]
                let high = starts[int cover[int first] + 1]
                valid <- low >= 0 && high <= seconds.Length
                let mutable r = low
                while valid && r < high do
                    valid <- seconds[r] < 0xFFFFu && (r = low || seconds[r] > seconds[r - 1])
                    count <- count + 1
                    r <- r + 1
            f <- f + 1
        match valid with
        | true -> ValueSome count
        | false -> ValueNone

    /// <summary>Builds an open-addressed index of the pairs of the sets of a format 1 pair subtable, so a pair is found by one
    /// probe instead of a binary search: the slots hold the pair's key, the first glyph in the high half and the second in
    /// the low, and the number of its record. The load is at most one half.</summary>
    /// <param name="firsts">The first glyphs, in the coverage's order.</param>
    /// <param name="cover">The coverage index of each glyph, 0xFFFF for none.</param>
    /// <param name="starts">The first record of each set, and the end of the last.</param>
    /// <param name="seconds">The second glyph of every record.</param>
    /// <returns>The keys, the record numbers, the shift of <see cref="pairSlot"/>, a filter of one bit per hash of a key and its shift,
    /// or none when the sets cannot be indexed or a pair is listed twice.</returns>
    let pairIndexOfSets (firsts: ReadOnlySpan<uint32>) (cover: ReadOnlySpan<uint16>) (starts: ReadOnlySpan<int>) (seconds: ReadOnlySpan<uint32>) =
        match pairCount firsts cover starts seconds with
        | ValueNone -> ValueNone
        | ValueSome count ->
            let bits = max 3 (32 - BitOperations.LeadingZeroCount (uint32 (2 * (max 1 count) - 1)))
            let shift = 32 - bits
            let filterBits = max 6 (32 - BitOperations.LeadingZeroCount (uint32 (PairFilterBitsPerPair * (max 1 count) - 1)))
            let filter = Array.zeroCreate<uint64> (1 <<< (filterBits - 6))
            let filterShift = 32 - filterBits
            let keys = Array.create (1 <<< bits) PairEmptyKey
            let slots = Array.zeroCreate<int> (1 <<< bits)
            let mutable unique = true
            let mutable f = 0
            while unique && f < firsts.Length do
                let first = firsts[f]
                let set = int cover[int first]
                match set + 1 < starts.Length with
                | false -> ()
                | true ->
                    let mutable r = starts[set]
                    while unique && r < starts[set + 1] do
                        let key = (first <<< 16) ||| seconds[r]
                        let mark = int (pairHash key >>> filterShift)
                        filter[mark >>> 6] <- filter[mark >>> 6] ||| (1UL <<< (mark &&& 63))
                        let mutable slot = pairSlot key shift
                        while keys[slot] <> PairEmptyKey && keys[slot] <> key do
                            slot <- (slot + 1) &&& (keys.Length - 1)
                        unique <- keys[slot] = PairEmptyKey
                        keys[slot] <- key
                        slots[slot] <- r
                        r <- r + 1
                f <- f + 1
            match unique with
            | true -> ValueSome (struct (publish keys, publish slots, shift, publish filter, filterShift))
            | false -> ValueNone

    /// <summary>The kind of a pair table that holds nothing and leaves its pairs to the general loop.</summary>
    [<Literal>]
    let PairKindGeneral = 0

    /// <summary>The kind of a format 1 pair table, answered through the pair index.</summary>
    [<Literal>]
    let PairKindGlyphs = 1

    /// <summary>The kind of a format 2 pair table, answered through the class arrays.</summary>
    [<Literal>]
    let PairKindClasses = 2

    /// <summary>The kind of a format 1 pair table whose records are resolved for a font: the advance of each, devices included, is in <c>Totals</c>.</summary>
    [<Literal>]
    let PairKindGlyphsResolved = 3

    /// <summary>The kind of a format 2 pair table whose records are resolved for a font.</summary>
    [<Literal>]
    let PairKindClassesResolved = 4

    /// <summary>The coverage answer of a glyph a pair table does not cover, in a direct table and in the rows of a format 2 table.</summary>
    [<Literal>]
    let PairNotCovered = 0xFFFFus

    /// <summary>The row of a covered first glyph whose class is not below the class count, which has no record for any pair.</summary>
    [<Literal>]
    let PairNoRow = 0xFFFEus

    /// <summary>The column of a second glyph whose class is not below the class count.</summary>
    [<Literal>]
    let PairNoColumn = 0xFFFFus

    /// <summary>The glyph id from which a pair key cannot hold a glyph.</summary>
    [<Literal>]
    let PairKeyLimit = 0xFFFFu

    /// <summary>The verdict of a pair table that does not cover the first glyph, which records nothing. The verdicts that are no
    /// record are the three smallest ints, below every raw value of a 16-bit field.</summary>
    [<Literal>]
    let private VerdictUntouched = Int32.MinValue

    /// <summary>The verdict of a pair table that covers the first glyph and has no record for the pair.</summary>
    [<Literal>]
    let private VerdictMissed = -2147483647

    /// <summary>The verdict of a pair the general loop must apply.</summary>
    [<Literal>]
    let private VerdictComplex = -2147483646

    /// <summary>The verdict of a resolved table's record is this base plus the record's number, above every raw value of a 16-bit field.</summary>
    [<Literal>]
    let private ResolvedBase = 65536

    /// <summary>The rounding term of <c>em_scale</c>, half of 2^16.</summary>
    [<Literal>]
    let private ScaleRounding = 32768L

    /// <summary>The shift of <c>em_scale</c>, the multiplier being 16.16 fixed point.</summary>
    [<Literal>]
    let private ScaleShift = 16

    /// <summary>Scales a design-space advance by the font's x multiplier, <c>em_scale_x</c>.</summary>
    /// <param name="multiplier">The font's x multiplier.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The scaled value.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private scaleAdvance (multiplier: int64) (value: int) =
        int ((int64 value * multiplier + ScaleRounding) >>> ScaleShift)

    /// <summary>The verdict of a format 1 pair table on a pair of glyphs: a table that has compiled holds no pair of a glyph at or above
    /// <see cref="PairKeyLimit"/>, so such a pair is a miss.</summary>
    /// <param name="table">The table.</param>
    /// <param name="first">The first glyph.</param>
    /// <param name="second">The second glyph.</param>
    /// <param name="hazard">Whether a first glyph the table covers but has no record for must be told from one it does not cover.</param>
    /// <returns>The raw value of the pair's record, or one of the verdicts that are no record.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private glyphsVerdict (table: PairTable) (first: uint32) (second: uint32) (hazard: bool) =
        let key = (first <<< 16) ||| second
        let hash = pairHash key
        let mark = int (hash >>> table.FilterShift)
        let found =
            match first < PairKeyLimit && second < PairKeyLimit && (nonNull (ImmutableCollectionsMarshal.AsArray table.Filter))[mark >>> 6] &&& (1UL <<< (mark &&& 63)) <> 0UL with
            | false -> -1
            | true ->
                let keys = nonNull (ImmutableCollectionsMarshal.AsArray table.Keys)
                let mutable slot = int (hash >>> table.Shift)
                while keys[slot] <> key && keys[slot] <> PairEmptyKey do
                    slot <- (slot + 1) &&& (keys.Length - 1)
                match keys[slot] = key with
                | true -> slot
                | false -> -1
        match found with
        | -1 ->
            match hazard && first < uint32 table.Cover.Length && table.Cover[int first] <> PairNotCovered with
            | true -> VerdictMissed
            | false -> VerdictUntouched
        | slot ->
            match table.Plain with
            | true ->
                let values = nonNull (ImmutableCollectionsMarshal.AsArray table.Values)
                let slots = nonNull (ImmutableCollectionsMarshal.AsArray table.Slots)
                int values[slots[slot]]
            | false -> VerdictComplex

    /// <summary>The verdict of a format 2 pair table on a pair of glyphs.</summary>
    /// <param name="table">The table.</param>
    /// <param name="first">The first glyph.</param>
    /// <param name="second">The second glyph.</param>
    /// <returns>The raw value of the pair's record, or one of the verdicts that are no record.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private classesVerdict (table: PairTable) (first: uint32) (second: uint32) =
        let rows = nonNull (ImmutableCollectionsMarshal.AsArray table.Cover)
        let columns = nonNull (ImmutableCollectionsMarshal.AsArray table.Columns)
        let row = match first < uint32 rows.Length with | true -> rows[int first] | false -> PairNotCovered
        let column = match second < uint32 columns.Length with | true -> columns[int second] | false -> table.BeyondColumn
        match row, column with
        | PairNotCovered, _ -> VerdictUntouched
        | PairNoRow, _ -> VerdictMissed
        | _, PairNoColumn -> VerdictMissed
        | _ ->
            match table.Plain with
            | true ->
                let values = nonNull (ImmutableCollectionsMarshal.AsArray table.Values)
                int values[int row * table.Width + int column]
            | false -> VerdictComplex

    /// <summary>The verdict of a resolved format 1 pair table on a pair of glyphs, as <see cref="glyphsVerdict"/> answers a plain one, the
    /// record being named by its number past <see cref="ResolvedBase"/> where the plain table's raw value would be.</summary>
    /// <param name="table">The table.</param>
    /// <param name="first">The first glyph.</param>
    /// <param name="second">The second glyph.</param>
    /// <param name="hazard">Whether a first glyph the table covers but has no record for must be told from one it does not cover.</param>
    /// <returns>The record's number past the base, or one of the verdicts that are no record.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private glyphsResolved (table: PairTable) (first: uint32) (second: uint32) (hazard: bool) =
        let key = (first <<< 16) ||| second
        let hash = pairHash key
        let mark = int (hash >>> table.FilterShift)
        let found =
            match first < PairKeyLimit && second < PairKeyLimit && (nonNull (ImmutableCollectionsMarshal.AsArray table.Filter))[mark >>> 6] &&& (1UL <<< (mark &&& 63)) <> 0UL with
            | false -> -1
            | true ->
                let keys = nonNull (ImmutableCollectionsMarshal.AsArray table.Keys)
                let mutable slot = int (hash >>> table.Shift)
                while keys[slot] <> key && keys[slot] <> PairEmptyKey do
                    slot <- (slot + 1) &&& (keys.Length - 1)
                match keys[slot] = key with
                | true -> slot
                | false -> -1
        match found with
        | -1 ->
            match hazard && first < uint32 table.Cover.Length && table.Cover[int first] <> PairNotCovered with
            | true -> VerdictMissed
            | false -> VerdictUntouched
        | slot -> ResolvedBase + (nonNull (ImmutableCollectionsMarshal.AsArray table.Slots))[slot]

    /// <summary>The verdict of a resolved format 2 pair table on a pair of glyphs.</summary>
    /// <param name="table">The table.</param>
    /// <param name="first">The first glyph.</param>
    /// <param name="second">The second glyph.</param>
    /// <returns>The record's number past the base, or one of the verdicts that are no record.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private classesResolved (table: PairTable) (first: uint32) (second: uint32) =
        let rows = nonNull (ImmutableCollectionsMarshal.AsArray table.Cover)
        let columns = nonNull (ImmutableCollectionsMarshal.AsArray table.Columns)
        let row = match first < uint32 rows.Length with | true -> rows[int first] | false -> PairNotCovered
        let column = match second < uint32 columns.Length with | true -> columns[int second] | false -> table.BeyondColumn
        match row, column with
        | PairNotCovered, _ -> VerdictUntouched
        | PairNoRow, _ -> VerdictMissed
        | _, PairNoColumn -> VerdictMissed
        | _ -> ResolvedBase + int row * table.Width + int column

    /// <summary>The step of a pair a resolved record decides: nothing when the record did not work, else the record's advance and the
    /// unsafe-to-break flag.</summary>
    /// <param name="total">The record's total, the advance shifted left once and the lowest bit set when the record worked.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    /// <param name="missed">Whether an earlier table covered the first glyph and had no record for the pair.</param>
    /// <returns>The step.</returns>
    let private resolvedStep (total: int64) (hazard: bool) (missed: bool) =
        match total &&& 1L with
        | 0L -> (match hazard with | true -> PairStep.Hazard | false -> PairStep.Nothing)
        | _ -> PairStep.make (int (total >>> 1)) (match hazard && missed with | true -> PairStep.Broken ||| PairStep.Hazard | false -> PairStep.Broken)

    /// <summary>Walks the tables of a lookup for the pair of two adjacent glyphs, the first that applies deciding, and says what the
    /// general loop and its commit would do with the pair, <c>tryPosSubtables</c> followed by the commit of its outcome.</summary>
    /// <param name="tables">The lookup's tables, compiled.</param>
    /// <param name="multiplier">The font's x multiplier.</param>
    /// <param name="first">The first glyph.</param>
    /// <param name="second">The second glyph.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    /// <returns>The step; complex when a table of the general kind or with records that are not plain decides.</returns>
    let pairChain (tables: ImmutableArray<PairTable>) (multiplier: int64) (first: uint32) (second: uint32) (hazard: bool) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray tables)
        let mutable raw = VerdictUntouched
        let mutable missed = false
        let mutable decider = 0
        let mutable next = 0
        while next < array.Length do
            let table = array[next]
            let verdict =
                match table.Kind with
                | PairKindGlyphs -> glyphsVerdict table first second hazard
                | PairKindClasses -> classesVerdict table first second
                | PairKindGlyphsResolved -> glyphsResolved table first second hazard
                | PairKindClassesResolved -> classesResolved table first second
                | _ -> VerdictComplex
            match verdict with
            | VerdictUntouched -> next <- next + 1
            | VerdictMissed ->
                missed <- true
                next <- next + 1
            | _ ->
                raw <- verdict
                decider <- next
                next <- array.Length
        match raw with
        | VerdictUntouched -> (match hazard && missed with | true -> PairStep.Hazard | false -> PairStep.Nothing)
        | VerdictComplex -> PairStep.Complex
        | 0 -> (match hazard with | true -> PairStep.Hazard | false -> PairStep.Nothing)
        | resolved when resolved >= ResolvedBase -> resolvedStep array[decider].Totals[resolved - ResolvedBase] hazard missed
        | _ -> PairStep.make (scaleAdvance multiplier raw) (match hazard && missed with | true -> PairStep.Broken ||| PairStep.Hazard | false -> PairStep.Broken)


    /// <summary>Concatenates arrays into one, in order.</summary>
    /// <param name="parts">The arrays.</param>
    /// <returns>The items of every array, one after the other.</returns>
    let flatten (parts: ImmutableArray<ImmutableArray<'T>>) =
        let mutable total = 0
        let mutable index = 0
        while index < parts.Length do
            total <- total + parts[index].Length
            index <- index + 1
        let target = Array.zeroCreate<'T> total
        let mutable written = 0
        let mutable next = 0
        while next < parts.Length do
            parts[next].AsSpan().CopyTo(target.AsSpan(written))
            written <- written + parts[next].Length
            next <- next + 1
        publish target

    /// <summary>The offsets at which the arrays of a concatenation start, and its total length last.</summary>
    /// <param name="parts">The arrays.</param>
    /// <returns>One more offset than there are arrays: where each starts in the concatenation, then its length.</returns>
    let prefixLengths (parts: ImmutableArray<ImmutableArray<'T>>) =
        let target = Array.zeroCreate<int> (parts.Length + 1)
        let mutable index = 0
        while index < parts.Length do
            target[index + 1] <- target[index] + parts[index].Length
            index <- index + 1
        publish target

    /// <summary>The state of one positioning pass: the run's arrays and the working copies of the two the lookups may write,
    /// each made on the first write and not before.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private GposMachine =
        { SourceInfos: ImmutableArray<GlyphInfo>
          SourceProps: ImmutableArray<GlyphProps>
          SourcePositions: ImmutableArray<GlyphPosition>
          mutable Info: GlyphInfo array
          mutable InfosOwned: bool
          mutable Positions: GlyphPosition array
          mutable PositionsOwned: bool
          Len: int
          mutable Idx: int
          mutable MaxOps: int
          mutable Successful: bool
          mutable Scratch: ScratchFlags
          mutable LastBase: int
          mutable LastBaseUntil: int
          ClusterLevel: ClusterLevel
          Flags: BufferFlags }

    /// <summary>The glyphs a positioning lookup reads: the working copy once flags have been written, else the run's own.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The infos.</returns>
    let private gposInfos (machine: inref<GposMachine>) =
        match machine.InfosOwned with
        | true -> ReadOnlySpan<GlyphInfo>(machine.Info, 0, machine.Len)
        | false -> machine.SourceInfos.AsSpan(0, machine.Len)

    /// <summary>The positions a positioning lookup reads: the working copy once one has been written, else the run's own.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The positions.</returns>
    let private gposPositions (machine: inref<GposMachine>) =
        match machine.PositionsOwned with
        | true -> ReadOnlySpan<GlyphPosition>(machine.Positions, 0, machine.Len)
        | false -> machine.SourcePositions.AsSpan(0, machine.Len)

    /// <summary>Copies the infos into the working array on the first flag written.</summary>
    /// <param name="machine">The machine.</param>
    let private gposOwnInfos (machine: byref<GposMachine>) =
        match machine.InfosOwned with
        | true -> ()
        | false ->
            let info = GC.AllocateUninitializedArray<GlyphInfo> machine.Len
            machine.SourceInfos.AsSpan(0, machine.Len).CopyTo(Span<GlyphInfo>(info))
            machine.Info <- info
            machine.InfosOwned <- true

    /// <summary>Copies the positions into the working array on the first position written.</summary>
    /// <param name="machine">The machine.</param>
    let private gposOwnPositions (machine: byref<GposMachine>) =
        match machine.PositionsOwned with
        | true -> ()
        | false ->
            let positions = GC.AllocateUninitializedArray<GlyphPosition> machine.Len
            machine.SourcePositions.AsSpan(0, machine.Len).CopyTo(Span<GlyphPosition>(positions))
            machine.Positions <- positions
            machine.PositionsOwned <- true

    /// <summary>Ors a glyph flag into a range of a positioning pass whose request was accepted and clamped, <c>_set_glyph_flags_impl</c>
    /// on the in-place buffer.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index, at most the length of the run.</param>
    /// <param name="interior">Whether the minimum cluster is skipped.</param>
    let private gposFlagRange (machine: byref<GposMachine>) mask start finish interior =
        gposOwnInfos &machine
        match interior with
        | false ->
            let mutable i = start
            while i < finish do
                machine.Info[i] <- { machine.Info[i] with Mask = machine.Info[i].Mask ||| mask }
                i <- i + 1
        | true ->
            let cluster = infosFindMinCluster machine.Info start finish UInt32.MaxValue machine.ClusterLevel
            let struct (maxOps, successful) = infosSetGlyphFlags machine.Info start finish cluster mask machine.ClusterLevel machine.MaxOps machine.Successful
            machine.MaxOps <- maxOps
            machine.Successful <- successful

    /// <summary>Ors a glyph flag into a range of a positioning pass, <c>_set_glyph_flags</c> on the in-place buffer.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="interior">Whether the minimum cluster is skipped.</param>
    let private gposSetFlags (machine: byref<GposMachine>) (mask: uint32) (start: int) (finish: int) (interior: bool) =
        match (uint32 finish - uint32 start) > uint32 MaxGlyphFlagRange with
        | true -> ()
        | false ->
            let finish = min finish machine.Len
            match interior && finish - start < 2 with
            | true -> ()
            | false -> gposFlagRange &machine mask start finish interior

    /// <summary>Ors a glyph flag into a range of the output buffer of a positioning pass, which has none, so the range is one
    /// of the run: <c>_set_glyph_flags</c> with <c>from_out_buffer</c>, which does not skip an interior range of fewer than two
    /// glyphs (src/hb-buffer.hh:488-503).</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="interior">Whether the minimum cluster is skipped.</param>
    let private gposSetFlagsOut (machine: byref<GposMachine>) mask start finish interior =
        match (uint32 finish - uint32 start) > uint32 MaxGlyphFlagRange with
        | true -> ()
        | false -> gposFlagRange &machine mask start (min finish machine.Len) interior

    /// <summary>Marks the unsafe-to-concat ranges the failed rules of a context lookup owe: the plain marks of the ranges of the
    /// calls of <c>unsafe_to_concat</c> and <c>unsafe_to_concat_from_outbuffer</c> they made are, on a run positioned in place,
    /// the marks of their union, each range having been checked against the range limit when it was added.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="owed">The ranges owed.</param>
    let private gposMarkOwed (machine: byref<GposMachine>) owed =
        match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default || FlagBounds.isNone owed with
        | true -> ()
        | false ->
            let finish = min (max owed.ConcatTo owed.OutEnd) machine.Len
            let start = match owed.OutStart with | -1 -> machine.Idx | outStart -> outStart
            match finish > start with
            | false -> ()
            | true -> gposFlagRange &machine GlyphFlagUnsafeToConcat start finish false

    /// <summary>Records what a lookup did at the glyph it stopped on: the positions it changed and the unsafe-to ranges, in
    /// the order of <c>PairSet::apply</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The glyph the lookup stopped on.</param>
    /// <param name="outcome">The outcome.</param>
    let private gposCommit (machine: byref<GposMachine>) (index: int) (outcome: PosOutcome) =
        match outcome.Advance with
        | 0 -> ()
        | advance ->
            gposOwnPositions &machine
            let position = machine.Positions[index]
            let sum = int64 position.XAdvance + int64 advance
            let saturated = match sum > int64 Int32.MaxValue, sum < int64 Int32.MinValue with | true, _ -> Int32.MaxValue | _, true -> Int32.MinValue | _ -> int sum
            machine.Positions[index] <- { position with XAdvance = saturated }
        match outcome.FirstWorked with
        | false -> ()
        | true ->
            gposOwnPositions &machine
            machine.Positions[index] <- outcome.First
        match outcome.SecondWorked with
        | false -> ()
        | true ->
            gposOwnPositions &machine
            machine.Positions[outcome.SecondIndex] <- outcome.Second
        match outcome.ConcatTo with
        | 0 -> ()
        | concatTo ->
            match (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
            | true -> ()
            | false -> gposSetFlags &machine GlyphFlagUnsafeToConcat index concatTo false
        match outcome.Break1To with
        | 0 -> ()
        | breakTo -> gposSetFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) index breakTo true
        match outcome.Break2To with
        | 0 -> ()
        | breakTo -> gposSetFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) index breakTo true

    /// <summary>Applies one positioning lookup from the cursor to the end of the run, <c>apply_forward</c>: skips to the next
    /// glyph the lookup may match, applies it, and goes on.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private gposApplyForward<'L when 'L : struct and 'L :> IPosLookup> (lookup: 'L) (machine: byref<GposMachine>) =
        let mutable running = true
        while running do
            match machine.Idx >= machine.Len || not machine.Successful with
            | true -> running <- false
            | false ->
                let found = lookup.Scan(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), machine.Idx)
                match found >= machine.Len with
                | true -> running <- false
                | false ->
                    let outcome = lookup.Try(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), gposPositions &machine, found)
                    gposCommit &machine found outcome
                    machine.Idx <- (match outcome.Applied with | true -> outcome.Next | false -> found + 1)

    /// <summary>Finds the next glyph from an index at which a lookup that the pair loop applies has something to do.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    /// <param name="start">The first index to look at.</param>
    /// <returns>The index and its step, the length of the run and a step of nothing when there is no such glyph.</returns>
    let private gposPairSeek<'L when 'L : struct and 'L :> IPosLookup> (lookup: 'L) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (hazard: bool) (start: int) =
        let mutable index = start
        let mutable step = PairStep.Nothing
        let mutable found = false
        while not found && index < infos.Length do
            step <- lookup.Pair(infos, props, index, hazard)
            found <- PairStep.acts step
            index <- (match found with | true -> index | false -> index + 1)
        struct (index, step)

    /// <summary>The index of the field that follows the first in a glyph info, <c>mask</c> after <c>codepoint</c>, which are both 32 bits
    /// in the sequential layout of <see cref="GlyphInfo"/>.</summary>
    [<Literal>]
    let private MaskField = 1

    /// <summary>Ors a mask into the mask of one glyph, in place, without reading or writing its other fields.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="mask">The mask to or in.</param>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private gposOrMask (infos: GlyphInfo array) (index: int) (mask: uint32) =
        let field = &Unsafe.Add(&Unsafe.As<GlyphInfo, uint32>(&infos[index]), MaskField)
        field <- field ||| mask

    /// <summary>Adds an advance to the horizontal advance of one glyph with int saturation, in place, without reading or writing
    /// its other fields: <c>x_advance</c> is the first field of <see cref="GlyphPosition"/>.</summary>
    /// <param name="positions">The positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="advance">The advance to add.</param>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private gposAddAdvance (positions: GlyphPosition array) (index: int) (advance: int) =
        let field = &Unsafe.As<GlyphPosition, int>(&positions[index])
        let sum = int64 field + int64 advance
        field <- (match sum > int64 Int32.MaxValue, sum < int64 Int32.MinValue with | true, _ -> Int32.MaxValue | _, true -> Int32.MinValue | _ -> int sum)

    /// <summary>Ors the unsafe-to-break and unsafe-to-concat flags into the glyph of a pair whose cluster is not the smaller of the two,
    /// as <see cref="gposSetFlags"/> does for the range of the pair's two glyphs: <c>_infos_find_min_cluster</c> finds the smaller
    /// cluster, and <c>_infos_set_glyph_flags</c> flags the glyph of any other, at every cluster level, and takes the two operations from
    /// the budget.</summary>
    /// <param name="machine">The machine, whose infos are its own.</param>
    /// <param name="index">The first glyph's index.</param>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private gposBreakPair (machine: byref<GposMachine>) (index: int) =
        let infos = machine.Info
        let first = infos[index].Cluster
        let second = infos[index + 1].Cluster
        machine.MaxOps <- machine.MaxOps - 2
        machine.Successful <- machine.Successful && machine.MaxOps >= 0
        match first < second, first > second with
        | true, _ -> gposOrMask infos (index + 1) (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat)
        | _, true -> gposOrMask infos index (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat)
        | _ -> ()

    /// <summary>Applies what the pair loop found at a glyph when it is no complex pair, as <see cref="gposCommit"/> applies the
    /// outcome the general loop finds there: the advance, the unsafe-to-concat range of a hazard, then the unsafe-to-break
    /// range of the pair.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="step">The step.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    let private gposPairEffect (machine: byref<GposMachine>) (index: int) (step: int64) (hazard: bool) =
        match PairStep.advance step with
        | 0 -> ()
        | advance ->
            gposOwnPositions &machine
            gposAddAdvance machine.Positions index advance
        let gap = PairStep.gap step
        match hazard && PairStep.isHazard step with
        | false -> ()
        | true -> gposSetFlags &machine GlyphFlagUnsafeToConcat index (index + 2 + gap) false
        match PairStep.isBroken step, gap with
        | false, _ -> ()
        | true, 0 ->
            gposOwnInfos &machine
            gposBreakPair &machine index
        | true, _ -> gposSetFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) index (index + 2 + gap) true

    /// <summary>Applies what the pair loop found at a glyph, as <see cref="gposCommit"/> applies the outcome the general loop
    /// finds there; a complex pair is applied by the lookup's own subtables.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="step">The step.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    /// <returns>The index the lookup goes on from.</returns>
    let private gposPairCommit<'L when 'L : struct and 'L :> IPosLookup> (lookup: 'L) (machine: byref<GposMachine>) (index: int) (step: int64) (hazard: bool) =
        match PairStep.isComplex step with
        | true ->
            let outcome = lookup.Try(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), gposPositions &machine, index)
            gposCommit &machine index outcome
            match outcome.Applied with
            | true -> outcome.Next
            | false -> index + 1
        | false ->
            gposPairEffect &machine index step hazard
            index + 1

    /// <summary>Applies one positioning lookup that filters no glyph and has only pair subtables, from the start of the run to its
    /// end: it is the general loop of <see cref="gposApplyForward"/> with the skipping iterator and the subtables folded into
    /// one classification of each pair, so a pair that applies with nothing to add is not built and committed.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private gposPairForward<'L when 'L : struct and 'L :> IPosLookup> (lookup: 'L) (machine: byref<GposMachine>) =
        let hazard = machine.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default
        let mutable index = 0
        while index < machine.Len && machine.Successful do
            let struct (found, step) = gposPairSeek lookup (gposInfos &machine) (machine.SourceProps.AsSpan(0, machine.Len)) hazard index
            match found < machine.Len with
            | false -> index <- found
            | true -> index <- gposPairCommit lookup &machine found step hazard

    /// <summary>Applies one lookup of the lean loop, from the start of the run to its end, in place: at each glyph the lookup starts at
    /// the pair with the glyph right after is classified and what it does is applied; no pair is complex.</summary>
    /// <param name="covered">The glyphs the lookup starts at.</param>
    /// <param name="tables">The lookup's tables, compiled.</param>
    /// <param name="multiplier">The font's x multiplier.</param>
    /// <param name="machine">The machine.</param>
    let private gposLeanForward (covered: ImmutableArray<uint64>) (tables: ImmutableArray<PairTable>) (multiplier: int64) (machine: byref<GposMachine>) =
        let hazard = machine.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default
        let infos = machine.Info
        let words = nonNull (ImmutableCollectionsMarshal.AsArray covered)
        let last = machine.Len - 1
        let mutable index = 0
        while index <= last && machine.Successful do
            let glyph = infos[index].Codepoint
            let word = int (glyph >>> 6)
            match word < words.Length && (words[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL with
            | false -> ()
            | true ->
                match index < last with
                | false ->
                    match hazard with
                    | false -> ()
                    | true -> gposSetFlags &machine GlyphFlagUnsafeToConcat index (index + 1) false
                | true ->
                    let step = pairChain tables multiplier glyph infos[index + 1].Codepoint hazard
                    gposPairEffect &machine index step hazard
            index <- index + 1

    /// <summary>Applies pair lookups to the arrays of a run the caller made in this call and has shared with no one, in place: the
    /// lookups at the positions of the map that <paramref name="bits"/> sets, in order, each one loop over the run, writing the
    /// advances and the glyph flags into the arrays themselves. The caller has checked what the lean loop assumes: no pair is complex,
    /// every glyph carries the masks of the lookups, none is default ignorable, and the operation budget cannot run out.</summary>
    /// <param name="covered">The glyphs each position of the map starts at.</param>
    /// <param name="tables">The compiled tables of each position of the map.</param>
    /// <param name="multiplier">The font's x multiplier.</param>
    /// <param name="bits">The positions of the lookups to apply.</param>
    /// <param name="infos">The glyphs of the run, written in place.</param>
    /// <param name="positions">The positions of the run, written in place.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <returns>The operation budget left.</returns>
    let runPairsOwned (covered: ImmutableArray<ImmutableArray<uint64>>) (tables: ImmutableArray<ImmutableArray<PairTable>>) (multiplier: int64) (bits: uint64) (infos: ImmutableArray<GlyphInfo>) (positions: ImmutableArray<GlyphPosition>) (flags: BufferFlags) (level: ClusterLevel) (maxOps: int) =
        let infoArray = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable machine =
            { SourceInfos = ImmutableArray<GlyphInfo>.Empty
              SourceProps = ImmutableArray<GlyphProps>.Empty
              SourcePositions = ImmutableArray<GlyphPosition>.Empty
              Info = infoArray
              InfosOwned = true
              Positions = nonNull (ImmutableCollectionsMarshal.AsArray positions)
              PositionsOwned = true
              Len = infoArray.Length
              Idx = 0
              MaxOps = maxOps
              Successful = true
              Scratch = ScratchFlags.Default
              LastBase = -1
              LastBaseUntil = 0
              ClusterLevel = level
              Flags = flags }
        let mutable remaining = bits
        while remaining <> 0UL do
            let position = BitOperations.TrailingZeroCount remaining
            gposLeanForward covered[position] tables[position] multiplier &machine
            remaining <- remaining &&& (remaining - 1UL)
        machine.MaxOps

    /// <summary>The cursive attachment type, <c>ATTACH_TYPE_CURSIVE</c>.</summary>
    [<Literal>]
    let private AttachTypeCursive = 0x02u

    /// <summary>The attach chains of a run longer than this are not reversed, <c>HB_MAX_NESTING_LEVEL</c> in
    /// <c>reverse_cursive_minor_offset</c>.</summary>
    [<Literal>]
    let private MaxReverseNesting = 64

    /// <summary>The left-to-right direction, <c>HB_DIRECTION_LTR</c>.</summary>
    [<Literal>]
    let private DirectionLtr = 4

    /// <summary>The right-to-left direction, <c>HB_DIRECTION_RTL</c>.</summary>
    [<Literal>]
    let private DirectionRtl = 5

    /// <summary>The top-to-bottom direction, <c>HB_DIRECTION_TTB</c>.</summary>
    [<Literal>]
    let private DirectionTtb = 6

    /// <summary>The bottom-to-top direction, <c>HB_DIRECTION_BTT</c>.</summary>
    [<Literal>]
    let private DirectionBtt = 7

    /// <summary>The attach chain of a position, <c>var.i16[0]</c>.</summary>
    /// <param name="position">The position.</param>
    /// <returns>The chain.</returns>
    let private positionChain (position: GlyphPosition) =
        int (int16 (uint16 (position.Var &&& 0xFFFFu)))

    /// <summary>The attach type of a position, <c>var.u8[2]</c>.</summary>
    /// <param name="position">The position.</param>
    /// <returns>The type.</returns>
    let private positionType (position: GlyphPosition) =
        (position.Var >>> 16) &&& 0xFFu

    /// <summary>A position with its attach chain set, as the 16-bit store of <c>attach_chain() = chain</c>.</summary>
    /// <param name="position">The position.</param>
    /// <param name="chain">The chain.</param>
    /// <returns>The position.</returns>
    let private withChain (position: GlyphPosition) chain =
        { position with Var = (position.Var &&& 0xFFFF0000u) ||| uint32 (uint16 chain) }

    /// <summary>A position with its attach type set, <c>attach_type() = type</c>.</summary>
    /// <param name="position">The position.</param>
    /// <param name="attachType">The type.</param>
    /// <returns>The position.</returns>
    let private withType (position: GlyphPosition) attachType =
        { position with Var = (position.Var &&& 0xFF00FFFFu) ||| (attachType <<< 16) }

    /// <summary>Adds two positions with int saturation, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The augend.</param>
    /// <param name="b">The addend.</param>
    /// <returns>The saturating sum.</returns>
    let private saturatedAdd a b =
        let sum = int64 a + int64 b
        match sum > int64 Int32.MaxValue, sum < int64 Int32.MinValue with
        | true, _ -> Int32.MaxValue
        | _, true -> Int32.MinValue
        | _ -> int sum

    /// <summary>Subtracts two positions with int saturation, <c>hb_saturate_sub</c>.</summary>
    /// <param name="a">The minuend.</param>
    /// <param name="b">The subtrahend.</param>
    /// <returns>The saturating difference.</returns>
    let private saturatedSub a b =
        let difference = int64 a - int64 b
        match difference > int64 Int32.MaxValue, difference < int64 Int32.MinValue with
        | true, _ -> Int32.MaxValue
        | _, true -> Int32.MinValue
        | _ -> int difference

    /// <summary>Converts a rounded float to a position as <c>(hb_position_t) roundf (x)</c> does here: saturating.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The position.</returns>
    let private positionOfFloat (value: float32) =
        let rounded = Math.Floor(float value + 0.5)
        match rounded >= float Int32.MinValue with
        | false -> Int32.MinValue
        | true -> (match rounded <= float Int32.MaxValue with | true -> int rounded | false -> Int32.MaxValue)

    /// <summary>Reverses the links of a glyph's old cursive chain so that the whole tree of its previous connection attaches to its
    /// new parent, <c>reverse_cursive_minor_offset</c>: each glyph along the chain takes the negated cross offset of the one
    /// before it and the opposite link, until the chain ends, leaves the run, or meets the new parent.</summary>
    /// <param name="positions">The positions, written in place.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    /// <param name="i">The glyph whose chain is reversed.</param>
    /// <param name="newParent">The glyph the chain is to attach to.</param>
    /// <param name="nesting">The depth of the walk.</param>
    let rec private reverseCursiveMinorOffset (positions: GlyphPosition array) horizontal i newParent nesting =
        let chain = positionChain positions[i]
        let attachType = positionType positions[i]
        let reversed = -chain
        let j = uint32 (i + chain)
        match nesting > MaxReverseNesting || chain = 0 || attachType &&& AttachTypeCursive = 0u with
        | true -> ()
        | false ->
            positions[i] <- withChain positions[i] 0
            match j >= uint32 positions.Length || int j = newParent || reversed <> int (int16 reversed) with
            | true -> ()
            | false ->
                reverseCursiveMinorOffset positions horizontal (int j) newParent (nesting + 1)
                let source = positions[i]
                let target = positions[int j]
                let negated = saturatedSub 0 (match horizontal with | true -> source.YOffset | false -> source.XOffset)
                let moved = match horizontal with | true -> { target with YOffset = negated } | false -> { target with XOffset = negated }
                positions[int j] <- withType (withChain moved reversed) attachType

    /// <summary>Adds or subtracts the advances between a mark and its base to the mark's offset, the loops of
    /// <c>propagate_attachment_offsets</c>: those of a mark that follows its base, the usual case, and those of a mark whose base
    /// comes after it, in the order of the direction.</summary>
    /// <param name="positions">The positions.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="i">The mark.</param>
    /// <param name="j">The base.</param>
    /// <param name="position">The mark's position with the base's offset added.</param>
    /// <returns>The mark's position.</returns>
    let private markAdvances (positions: GlyphPosition array) forward i j (position: GlyphPosition) =
        let mutable x = position.XOffset
        let mutable y = position.YOffset
        match j < i, forward with
        | true, true ->
            for k in j .. i - 1 do
                x <- saturatedSub x positions[k].XAdvance
                y <- saturatedSub y positions[k].YAdvance
        | true, false ->
            for k in j + 1 .. i do
                x <- saturatedAdd x positions[k].XAdvance
                y <- saturatedAdd y positions[k].YAdvance
        | false, true ->
            for k in i .. j - 1 do
                x <- saturatedAdd x positions[k].XAdvance
                y <- saturatedAdd y positions[k].YAdvance
        | false, false ->
            for k in i + 1 .. j do
                x <- saturatedSub x positions[k].XAdvance
                y <- saturatedSub y positions[k].YAdvance
        { position with XOffset = x; YOffset = y }

    /// <summary>Adds the offset of the glyph a glyph is attached to to its own, in place, after the parent has done the same through
    /// its own chain, <c>propagate_attachment_offsets</c>: the chain is cleared first, so a cycle in the chains ends where it
    /// comes back to a glyph that was already started, and the walk is no deeper than <c>HB_MAX_NESTING_LEVEL</c>.</summary>
    /// <param name="positions">The positions, written in place.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    /// <param name="i">The glyph.</param>
    /// <param name="nesting">The remaining nesting level.</param>
    let rec private propagateOffsetsAt (positions: GlyphPosition array) forward horizontal i nesting =
        let chain = positionChain positions[i]
        let attachType = positionType positions[i]
        positions[i] <- withChain positions[i] 0
        let j = uint32 (i + chain)
        match j >= uint32 positions.Length || nesting = 0 with
        | true -> ()
        | false ->
            let parent = int j
            match positionChain positions[parent] <> 0 with
            | true -> propagateOffsetsAt positions forward horizontal parent (nesting - 1)
            | false -> ()
            let child = positions[i]
            let source = positions[parent]
            positions[i] <-
                match attachType &&& AttachTypeCursive <> 0u, horizontal with
                | true, true -> { child with YOffset = saturatedAdd child.YOffset source.YOffset }
                | true, false -> { child with XOffset = saturatedAdd child.XOffset source.XOffset }
                | false, true -> markAdvances positions forward i parent { child with XOffset = saturatedAdd child.XOffset source.XOffset }
                | false, false -> markAdvances positions forward i parent { child with YOffset = saturatedAdd child.YOffset source.YOffset }

    /// <summary>Propagates the attachment offsets of a run, <c>GPOS::position_finish_offsets</c> for a buffer with a GPOS attachment: each
    /// glyph that has a chain takes the offset of its parent, in the order of the direction, ascending for a forward one and
    /// descending for the others (<c>https://github.com/harfbuzz/harfbuzz/issues/5514</c>), on one working copy.</summary>
    /// <param name="positions">The positions.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    /// <returns>The positions with the offsets propagated and every chain cleared.</returns>
    let propagateAttachments (positions: ImmutableArray<GlyphPosition>) forward horizontal =
        let working = positions.AsSpan().ToArray()
        for step in 0 .. working.Length - 1 do
            let i = match forward with | true -> step | false -> working.Length - 1 - step
            match positionChain working[i] <> 0 with
            | true -> propagateOffsetsAt working forward horizontal i MaxNestingLevel
            | false -> ()
        publish working

    /// <summary>Does the main-direction adjustment of a cursive attachment of the glyph at <c>i</c> to the glyph at <c>j</c>,
    /// <c>apply_cursive_attachment</c>'s switch on the direction: the advance of one of them is made to meet the other's anchor.</summary>
    /// <param name="positions">The positions, written in place.</param>
    /// <param name="outcome">The outcome, with the anchors and the direction.</param>
    /// <param name="i">The previous glyph.</param>
    /// <param name="j">The glyph.</param>
    let private cursiveMainDirection (positions: GlyphPosition array) (outcome: AttachOutcome) i j =
        match outcome.Direction with
        | DirectionLtr ->
            positions[i] <- { positions[i] with XAdvance = saturatedAdd (positionOfFloat outcome.ExitX) positions[i].XOffset }
            let d = saturatedAdd (positionOfFloat outcome.EntryX) positions[j].XOffset
            positions[j] <- { positions[j] with XAdvance = saturatedSub positions[j].XAdvance d; XOffset = saturatedSub positions[j].XOffset d }
        | DirectionRtl ->
            let d = saturatedAdd (positionOfFloat outcome.ExitX) positions[i].XOffset
            positions[i] <- { positions[i] with XAdvance = saturatedSub positions[i].XAdvance d; XOffset = saturatedSub positions[i].XOffset d }
            positions[j] <- { positions[j] with XAdvance = saturatedAdd (positionOfFloat outcome.EntryX) positions[j].XOffset }
        | DirectionTtb ->
            positions[i] <- { positions[i] with YAdvance = saturatedAdd (positionOfFloat outcome.ExitY) positions[i].YOffset }
            let d = saturatedAdd (positionOfFloat outcome.EntryY) positions[j].YOffset
            positions[j] <- { positions[j] with YAdvance = saturatedSub positions[j].YAdvance d; YOffset = saturatedSub positions[j].YOffset d }
        | DirectionBtt ->
            let d = saturatedAdd (positionOfFloat outcome.ExitY) positions[i].YOffset
            positions[i] <- { positions[i] with YAdvance = saturatedSub positions[i].YAdvance d; YOffset = saturatedSub positions[i].YOffset d }
            positions[j] <- { positions[j] with YAdvance = positionOfFloat outcome.EntryY }
        | _ -> ()

    /// <summary>Links a cursive child to its parent once the old chain of the child is reversed, the cross-direction end of
    /// <c>apply_cursive_attachment</c>: the child's chain, type and cross offset, the buffer's attachment flag, and the separation
    /// of a parent that was attached to the child.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    /// <param name="child">The child glyph.</param>
    /// <param name="parent">The parent glyph.</param>
    /// <param name="offset">The child's cross offset.</param>
    let private cursiveLink (machine: byref<GposMachine>) horizontal child parent offset =
        let positions = machine.Positions
        let chain = parent - child
        match chain = int (int16 chain) with
        | false -> positions[child] <- withChain positions[child] 0
        | true ->
            positions[child] <- withType (withChain positions[child] chain) AttachTypeCursive
            machine.Scratch <- machine.Scratch ||| ScratchFlags.HasGposAttachment
            positions[child] <- (match horizontal with | true -> { positions[child] with YOffset = offset } | false -> { positions[child] with XOffset = offset })
            match positionChain positions[parent] = -(positionChain positions[child]) with
            | false -> ()
            | true ->
                positions[parent] <- withChain positions[parent] 0
                positions[parent] <- (match horizontal with | true -> { positions[parent] with YOffset = 0 } | false -> { positions[parent] with XOffset = 0 })

    /// <summary>Attaches the glyph at an index to the previous glyph of a cursive attachment, <c>apply_cursive_attachment</c>
    /// after its subtable found the two glyphs and their anchors.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="outcome">The outcome.</param>
    let private gposCursive (machine: byref<GposMachine>) index (outcome: AttachOutcome) =
        let i = outcome.Partner
        gposSetFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) i (index + 1) true
        gposOwnPositions &machine
        cursiveMainDirection machine.Positions outcome i index
        let horizontal = outcome.Direction = DirectionLtr || outcome.Direction = DirectionRtl
        let xOffset = positionOfFloat (outcome.EntryX - outcome.ExitX)
        let yOffset = positionOfFloat (outcome.EntryY - outcome.ExitY)
        let child, parent, childX, childY =
            match outcome.RightToLeft with
            | true -> i, index, xOffset, yOffset
            | false -> index, i, -xOffset, -yOffset
        reverseCursiveMinorOffset machine.Positions horizontal child parent 0
        cursiveLink &machine horizontal child parent (match horizontal with | true -> childY | false -> childX)

    /// <summary>Records what an attachment lookup did at the glyph it stopped on: the unsafe-to-concat range of the subtables that
    /// failed, then the attachment itself, a mark's position, scratch flag and unsafe-to-break range, or a cursive attachment.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The glyph the lookup stopped on.</param>
    /// <param name="outcome">The outcome.</param>
    let private gposAttachCommit (machine: byref<GposMachine>) index (outcome: AttachOutcome) =
        machine.LastBase <- outcome.LastBase
        machine.LastBaseUntil <- outcome.LastBaseUntil
        match outcome.ConcatFrom >= 0 && (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) <> BufferFlags.Default with
        | false -> ()
        | true -> gposSetFlags &machine GlyphFlagUnsafeToConcat outcome.ConcatFrom (index + 1) false
        match outcome.Kind with
        | AttachKind.Mark ->
            gposSetFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) outcome.BreakFrom outcome.BreakTo true
            gposOwnPositions &machine
            machine.Positions[index] <- outcome.Position
            match outcome.Attached with
            | true -> machine.Scratch <- machine.Scratch ||| ScratchFlags.HasGposAttachment
            | false -> ()
        | AttachKind.Cursive -> gposCursive &machine index outcome
        | _ -> ()

    /// <summary>Applies one attachment lookup from the cursor to the end of the run, <c>apply_forward</c> with the lookup's
    /// cache of the last base: skips to the next glyph the lookup may match, tries its subtables, and goes on.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private gposApplyAttaching<'L when 'L : struct and 'L :> IPosLookup> (lookup: 'L) (machine: byref<GposMachine>) =
        let mutable running = true
        while running do
            match machine.Idx >= machine.Len || not machine.Successful with
            | true -> running <- false
            | false ->
                let found = lookup.Scan(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), machine.Idx)
                match found >= machine.Len with
                | true -> running <- false
                | false ->
                    let outcome = lookup.Attach(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), gposPositions &machine, found, machine.LastBase, machine.LastBaseUntil)
                    gposAttachCommit &machine found outcome
                    machine.Idx <- (match outcome.Applied with | true -> outcome.Next | false -> found + 1)

    /// <summary>The mark attachment type, <c>ATTACH_TYPE_MARK</c>.</summary>
    [<Literal>]
    let private AttachTypeMark = 0x01u

    /// <summary>What the subtables of a dense attachment lookup have decided at one glyph, as <c>tryAttachSubtables</c> threads it: the
    /// start of the unsafe-to-concat range of the subtables that failed, the cache of the last base, and the subtable that applied.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private DenseVisit =
        { mutable ConcatFrom: int
          mutable LastBase: int
          mutable LastBaseUntil: int
          mutable Applied: bool
          mutable Subtable: int
          mutable Target: int
          mutable Cell: int
          mutable MarkIndex: int }

    /// <summary>Merges the unsafe-to-concat range of a subtable that failed into the range so far: a range longer than the limit is
    /// not set at all, and the union of the rest is the longest.</summary>
    /// <param name="so">The start of the range so far, negative for none.</param>
    /// <param name="index">The glyph the lookup stopped on.</param>
    /// <param name="from">The start of the new range, negative for none.</param>
    /// <returns>The start of the merged range.</returns>
    let private gposMergeConcat so index from =
        match from < 0 || index + 1 - from > MaxGlyphFlagRange with
        | true -> so
        | false ->
            match so < 0 with
            | true -> from
            | false -> min so from

    /// <summary>The offset of a glyph's chain of cursive attachments from the baseline, <c>resolve_cross_offset</c>: the glyph's
    /// cross-direction offset plus those of the glyphs it is attached to by cursive links, as far as the run is long.</summary>
    /// <param name="positions">The positions.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    /// <param name="glyphPos">The glyph the walk is at.</param>
    /// <param name="count">The number of links followed, one for the glyph itself.</param>
    /// <param name="offset">The offset so far.</param>
    /// <returns>The offset.</returns>
    [<TailCall>]
    let rec private gposCrossOffset (positions: ReadOnlySpan<GlyphPosition>) horizontal glyphPos count offset =
        let position = positions[glyphPos]
        match count < positions.Length && positionType position &&& AttachTypeCursive <> 0u with
        | false -> offset
        | true ->
            match positionChain position with
            | 0 -> offset
            | chain ->
                let parent = uint32 (glyphPos + chain)
                match parent >= uint32 positions.Length with
                | true -> offset
                | false ->
                    let next = int parent
                    let step = match horizontal with | true -> positions[next].YOffset | false -> positions[next].XOffset
                    gposCrossOffset positions horizontal next (count + 1) (saturatedAdd offset step)

    /// <summary>Whether a glyph may be the base a mark attaches to, <c>MarkBasePosFormat1_2::accept</c>: only the first glyph of a
    /// multiple substitution is a base, unless a mark or another glyph than its predecessor in the sequence stands before it.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="idx">The index of the candidate.</param>
    /// <returns>True when the glyph is accepted.</returns>
    let private gposAcceptBase (props: ReadOnlySpan<GlyphProps>) idx =
        let here = props[idx]
        not (GlyphPropsBits.multiplied here)
        || GlyphPropsBits.ligComp here = 0uy
        || (idx = 0
            || GlyphPropsBits.isMark props[idx - 1]
            || not (GlyphPropsBits.multiplied props[idx - 1])
            || GlyphPropsBits.ligId here <> GlyphPropsBits.ligId props[idx - 1]
            || int (GlyphPropsBits.ligComp here) <> int (GlyphPropsBits.ligComp props[idx - 1]) + 1)

    /// <summary>Whether two marks are on the same base or component, <c>MarkMarkPosFormat1_2::apply</c> on their ligature ids and components.</summary>
    /// <param name="here">The properties of the mark being attached.</param>
    /// <param name="there">The properties of the mark before it.</param>
    /// <returns>True when the marks are on the same base or component.</returns>
    let private gposSameBase (here: GlyphProps) (there: GlyphProps) =
        let id1 = GlyphPropsBits.ligId here
        let id2 = GlyphPropsBits.ligId there
        let comp1 = GlyphPropsBits.ligComp here
        let comp2 = GlyphPropsBits.ligComp there
        match id1 = id2 with
        | true -> id1 = 0uy || comp1 = comp2
        | false -> (id1 > 0uy && comp1 = 0uy) || (id2 > 0uy && comp2 = 0uy)

    /// <summary>Searches backwards for the glyph a mark attaches to, the loop of <c>MarkBasePosFormat1_2::apply</c>: the first glyph the
    /// skipping iterator, which ignores marks, matches, that is accepted or covered by the subtable.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The mark's index; the search starts at the glyph before it.</param>
    /// <param name="until">The lowest index the search reaches.</param>
    /// <returns>The index of the glyph, or minus one.</returns>
    let private gposSearchBase<'L when 'L : struct and 'L :> IPosLookup> (lookup: byref<'L>) subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index until =
        let mutable candidate = index - 1
        let mutable found = -1
        while candidate >= until && found < 0 do
            match lookup.BaseMatches(infos[candidate], props[candidate]) && (gposAcceptBase props candidate || lookup.RowOf(subtable, infos[candidate].Codepoint) >= 0) with
            | true -> found <- candidate
            | false -> candidate <- candidate - 1
        found

    /// <summary>Lets one mark-to-base subtable decide at a covered mark: the base is searched back to the cache or found in it, and its
    /// row and the anchor of the mark's class decide whether the subtable applies.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="visit">What the subtables have decided so far.</param>
    let private gposDenseOnBase<'L when 'L : struct and 'L :> IPosLookup> (lookup: byref<'L>) (machine: inref<GposMachine>) subtable markIndex index (visit: byref<DenseVisit>) =
        let infos = gposInfos &machine
        let props = machine.SourceProps.AsSpan(0, machine.Len)
        let reset = visit.LastBaseUntil > index
        let until = match reset with | true -> 0 | false -> visit.LastBaseUntil
        let searched = gposSearchBase &lookup subtable infos props index until
        let found =
            match searched, reset with
            | -1, true -> -1
            | -1, false -> visit.LastBase
            | candidate, _ -> candidate
        visit.LastBase <- found
        visit.LastBaseUntil <- index
        match found with
        | -1 -> visit.ConcatFrom <- gposMergeConcat visit.ConcatFrom index 0
        | baseIndex ->
            let row = lookup.RowOf(subtable, infos[baseIndex].Codepoint)
            match row < 0 with
            | true -> visit.ConcatFrom <- gposMergeConcat visit.ConcatFrom index baseIndex
            | false ->
                let cell = lookup.AnchorCell(subtable, row, markIndex)
                visit.Applied <- cell >= 0
                visit.Target <- baseIndex
                visit.Cell <- cell
                visit.Subtable <- subtable
                visit.MarkIndex <- markIndex

    /// <summary>Lets one mark-to-mark subtable decide at a covered mark: the mark before it that the lookup's own iterator finds must be
    /// on the same base, covered, and have an anchor for the mark's class.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="visit">What the subtables have decided so far.</param>
    let private gposDenseOnMark<'L when 'L : struct and 'L :> IPosLookup> (lookup: byref<'L>) (machine: inref<GposMachine>) subtable markIndex index (visit: byref<DenseVisit>) =
        let infos = gposInfos &machine
        let props = machine.SourceProps.AsSpan(0, machine.Len)
        let previous = lookup.PreviousMark(infos, props, index)
        match previous < 0 with
        | true -> visit.ConcatFrom <- gposMergeConcat visit.ConcatFrom index (~~~previous)
        | false ->
            let row = match GlyphPropsBits.isMark props[previous] && gposSameBase props[index] props[previous] with | true -> lookup.RowOf(subtable, infos[previous].Codepoint) | false -> -1
            match row < 0 with
            | true -> visit.ConcatFrom <- gposMergeConcat visit.ConcatFrom index previous
            | false ->
                let cell = lookup.AnchorCell(subtable, row, markIndex)
                visit.Applied <- cell >= 0
                visit.Target <- previous
                visit.Cell <- cell
                visit.Subtable <- subtable
                visit.MarkIndex <- markIndex

    /// <summary>Writes the attachment of the mark at an index that a dense subtable decided: the unsafe-to-break range from its base,
    /// and the mark's offset from the two anchors, or its detached chain when the chain does not fit 16 bits.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="visit">What the subtables decided.</param>
    let private gposDenseAttach<'L when 'L : struct and 'L :> IPosLookup> (lookup: byref<'L>) (machine: byref<GposMachine>) index (visit: byref<DenseVisit>) =
        let target = visit.Target
        let horizontal = lookup.DenseHorizontal
        let positions = gposPositions &machine
        let baseOffset = gposCrossOffset positions horizontal target 1 (match horizontal with | true -> positions[target].YOffset | false -> positions[target].XOffset)
        let mark = positions[index]
        let chain = target - index
        let fits = chain = int (int16 chain)
        let xOffset = match fits with | true -> lookup.AnchorOffsetX(visit.Subtable, visit.Cell, visit.MarkIndex) | false -> 0
        let yOffset = match fits with | true -> lookup.AnchorOffsetY(visit.Subtable, visit.Cell, visit.MarkIndex) | false -> 0
        let var = (mark.Var &&& ~~~0x00FFFFFFu) ||| (AttachTypeMark <<< 16) ||| uint32 (uint16 chain)
        let moved =
            match fits, horizontal with
            | false, _ -> { mark with Var = mark.Var &&& 0xFFFF0000u }
            | true, true -> { mark with XOffset = xOffset; YOffset = saturatedAdd yOffset baseOffset; Var = var }
            | true, false -> { mark with XOffset = saturatedAdd xOffset baseOffset; YOffset = yOffset; Var = var }
        gposSetFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) target (index + 1) true
        gposOwnPositions &machine
        machine.Positions[index] <- moved
        match fits with
        | true -> machine.Scratch <- machine.Scratch ||| ScratchFlags.HasGposAttachment
        | false -> ()

    /// <summary>Applies the subtables of a dense attachment lookup at the glyph at an index in order, the first that applies deciding,
    /// and writes what they decided: the cache of the last base, the unsafe-to-concat range of the subtables that failed, and the attachment.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    /// <param name="index">The glyph's index.</param>
    let private gposDenseVisit<'L when 'L : struct and 'L :> IPosLookup> (lookup: byref<'L>) (machine: byref<GposMachine>) hazard index =
        let infos = gposInfos &machine
        let glyph = infos[index].Codepoint
        let count = lookup.DenseCount
        let mutable visit =
            { ConcatFrom = -1
              LastBase = machine.LastBase
              LastBaseUntil = machine.LastBaseUntil
              Applied = false
              Subtable = 0
              Target = -1
              Cell = -1
              MarkIndex = -1 }
        let mutable subtable = 0
        while subtable < count && not visit.Applied do
            let markIndex = lookup.MarkOf(subtable, glyph)
            match markIndex < 0, lookup.OnMarks subtable with
            | true, _ -> ()
            | false, true -> gposDenseOnMark &lookup &machine subtable markIndex index &visit
            | false, false -> gposDenseOnBase &lookup &machine subtable markIndex index &visit
            subtable <- subtable + 1
        machine.LastBase <- visit.LastBase
        machine.LastBaseUntil <- visit.LastBaseUntil
        match visit.ConcatFrom >= 0 && hazard with
        | true -> gposSetFlags &machine GlyphFlagUnsafeToConcat visit.ConcatFrom (index + 1) false
        | false -> ()
        match visit.Applied with
        | true -> gposDenseAttach &lookup &machine index &visit
        | false -> ()

    /// <summary>Applies one dense mark attachment lookup from the cursor to the end of the run: <see cref="gposApplyAttaching"/> with
    /// each visit decided from the lookup's dense tables and written through the machine, no outcome crossing a call.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private gposApplyDense<'L when 'L : struct and 'L :> IPosLookup> (lookup: byref<'L>) (machine: byref<GposMachine>) =
        let hazard = machine.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default
        let mutable running = true
        while running do
            match machine.Idx >= machine.Len || not machine.Successful with
            | true -> running <- false
            | false ->
                let found = lookup.Scan(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), machine.Idx)
                match found >= machine.Len with
                | true -> running <- false
                | false ->
                    gposDenseVisit &lookup &machine hazard found
                    machine.Idx <- found + 1

    /// <summary>What one step of the nested-lookup recursion of a positioning pass does: apply the records of a matched rule,
    /// apply a matched rule, recurse into a lookup, or match and apply a nested context lookup.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private PosWork =
        | PosRecords of inputCount: int * records: ImmutableArray<LookupRecord> * matchEnd: int
        | PosMatch of outcome: ContextOutcome
        | PosNested of lookupIndex: int
        | PosNestedContext of contextIndex: int

    /// <summary>Starts recursing into a lookup, <c>recurse</c> (src/hb-ot-layout-gsubgpos.hh:1142-1162): the nesting and
    /// operation budgets are checked and the operation is spent.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="nestingLeft">The nesting budget left, <c>nesting_level_left</c>.</param>
    /// <returns>True when the recursion goes on.</returns>
    let private gposEnterNested (machine: byref<GposMachine>) nestingLeft =
        match nestingLeft with
        | 0 ->
            machine.Successful <- false
            false
        | _ ->
            machine.MaxOps <- machine.MaxOps - 1
            match machine.MaxOps < 0 with
            | true ->
                machine.Successful <- false
                false
            | false -> true

    /// <summary>Applies a nested lookup that is no context lookup at the cursor: its single, pair or attachment subtables in order,
    /// with no check of the glyph's mask or properties, as <c>PosLookup::dispatch_recurse_func</c> does.</summary>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="nested">The nested lookup.</param>
    /// <param name="machine">The machine.</param>
    let private gposNestedPlain<'L when 'L : struct and 'L :> IPosLookup> (nested: 'L) (machine: byref<GposMachine>) =
        match nested.Attaches with
        | true ->
            let outcome = nested.Attach(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), gposPositions &machine, machine.Idx, machine.LastBase, machine.LastBaseUntil)
            gposAttachCommit &machine machine.Idx outcome
        | false ->
            let outcome = nested.Try(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), gposPositions &machine, machine.Idx)
            gposCommit &machine machine.Idx outcome

    /// <summary>Applies one step of the nested-lookup recursion of a positioning pass: <c>apply_lookup</c>
    /// (src/hb-ot-layout-gsubgpos.hh:2731-2873) on the in-place buffer of GPOS, where no lookup changes the length so the match
    /// positions never move and the cursor ends on the end of the match; the match handling of <c>context_apply_lookup</c> and
    /// <c>chain_context_apply_lookup</c>; and <c>recurse</c> with <c>PosLookup::dispatch_recurse_func</c>
    /// (src/hb-ot-layout-gpos-table.hh:57-77), which applies the named lookup's subtables at the cursor. The recursion is one
    /// self-recursive function over a struct union, as deep as <c>HB_MAX_NESTING_LEVEL</c> allows.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="work">The step.</param>
    /// <param name="settings">The nested settings.</param>
    /// <param name="nestingLeft">The nesting budget left.</param>
    /// <param name="positions">The match positions of the rule being applied.</param>
    let rec private gposApplyNested<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup>
            (program: 'P) (machine: byref<GposMachine>) work (settings: NestSettings) nestingLeft (positions: Span<int>) =
        match work with
        | PosRecords (inputCount, records, matchEnd) ->
            let mutable index = 0
            let mutable stop = false
            while index < records.Length && machine.Successful && not stop do
                let seq = records[index].SequenceIndex
                match seq >= inputCount || positions[seq] >= machine.Len with
                | true -> ()
                | false ->
                    machine.Idx <- positions[seq]
                    match machine.MaxOps <= 0 with
                    | true -> stop <- true
                    | false -> gposApplyNested program &machine (PosNested records[index].LookupListIndex) settings nestingLeft positions
                index <- index + 1
            machine.Idx <- matchEnd
        | PosMatch outcome ->
            gposMarkOwed &machine outcome.Owed
            match outcome.Chain with
            | true -> gposSetFlagsOut &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) outcome.StartIndex outcome.EndIndex true
            | false -> gposSetFlags &machine (GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat) machine.Idx outcome.MatchEnd true
            gposApplyNested program &machine (PosRecords (outcome.InputCount, outcome.Records, outcome.MatchEnd)) settings nestingLeft positions
            match outcome.AfterConcat > machine.Idx && (machine.Flags &&& BufferFlags.ProduceUnsafeToConcat) <> BufferFlags.Default with
            | true -> gposSetFlags &machine GlyphFlagUnsafeToConcat machine.Idx outcome.AfterConcat false
            | false -> ()
        | PosNested lookupIndex ->
            match gposEnterNested &machine nestingLeft with
            | false -> ()
            | true ->
                let nested = program.NestedLookup(lookupIndex, settings.Mask, settings.AutoZwnj, settings.AutoZwj)
                match nested.HasContext with
                | true -> gposApplyNested program &machine (PosNestedContext lookupIndex) settings (nestingLeft - 1) positions
                | false -> gposNestedPlain nested &machine
        | PosNestedContext contextIndex ->
            let nested = program.NestedLookup(contextIndex, settings.Mask, settings.AutoZwnj, settings.AutoZwj)
            let mutable buffer = PositionBuffer()
            let nestedPositions = MemoryMarshal.CreateSpan(&buffer.First, MaxContextLength)
            let outcome = nested.TryContext(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), machine.Idx, nestedPositions)
            match outcome.Matched with
            | true -> gposApplyNested program &machine (PosMatch outcome) settings nestingLeft nestedPositions
            | false -> gposMarkOwed &machine outcome.Owed

    /// <summary>Applies one context or chained-context lookup from the cursor to the end of the run, <c>apply_forward</c>: skips to
    /// the next glyph the lookup may match, matches its rules, applies the nested lookups of the rule that matches, and goes on
    /// from the end of the match.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="lookup">The lookup.</param>
    /// <param name="machine">The machine.</param>
    let private gposApplyContext<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (program: 'P) (lookup: 'L) (machine: byref<GposMachine>) =
        let struct (mask, autoZwnj, autoZwj) = lookup.Nest()
        let settings = NestSettings(mask, false, autoZwnj, autoZwj, false)
        let mutable buffer = PositionBuffer()
        let mutable running = true
        while running do
            match machine.Idx >= machine.Len || not machine.Successful with
            | true -> running <- false
            | false ->
                let found = lookup.Scan(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), machine.Idx)
                match found >= machine.Len with
                | true -> running <- false
                | false ->
                    machine.Idx <- found
                    let positions = MemoryMarshal.CreateSpan(&buffer.First, MaxContextLength)
                    let outcome = lookup.TryContext(gposInfos &machine, machine.SourceProps.AsSpan(0, machine.Len), found, positions)
                    match outcome.Matched with
                    | true -> gposApplyNested program &machine (PosMatch outcome) settings MaxNestingLevel positions
                    | false ->
                        gposMarkOwed &machine outcome.Owed
                        machine.Idx <- found + 1

    /// <summary>Applies the lookups of a positioning program in order on one working copy of the positions, <c>hb_ot_map_t::apply</c>
    /// for GPOS: nothing a lookup does changes a glyph or the run's length, so the digest and the candidates are those of the
    /// run as it came, and only the lookups whose candidate bit is set are visited.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="digest">The digest of the run's glyphs.</param>
    /// <param name="candidates">The lookups that cover some glyph of the run.</param>
    /// <param name="machine">The machine.</param>
    let private gposApplyLookups<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (program: 'P) (digest: Digest) (candidates: uint64) (machine: byref<GposMachine>) =
        let mutable index = 0
        while index < program.LookupEnd do
            index <- nextCandidate candidates index
            match index < program.LookupEnd && program.Admits(index, digest, candidates, machine.Flags) with
            | false -> ()
            | true ->
                machine.Idx <- 0
                machine.LastBase <- -1
                machine.LastBaseUntil <- 0
                let mutable lookup = program.Lookup index
                match lookup.Pairs, lookup.Attaches, lookup.HasContext with
                | true, _, _ -> gposPairForward lookup &machine
                | false, true, _ ->
                    match machine.Flags &&& BufferFlags.ProduceUnsafeToConcat = BufferFlags.Default && lookup.Futile(gposInfos &machine), lookup.Dense with
                    | true, _ -> ()
                    | false, true -> gposApplyDense &lookup &machine
                    | false, false -> gposApplyAttaching lookup &machine
                | false, false, true -> gposApplyContext program lookup &machine
                | false, false, false -> gposApplyForward lookup &machine
            index <- index + 1

    /// <summary>Looks at the glyphs of a run and applies the positioning lookups they can reach: the lookups that cover a glyph
    /// of the run, one bit each, are all that a program with a table of covering bits needs, and the digest of the glyphs is
    /// built only for a program without that table or with more lookups than the table has bits.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="glyphs">The glyphs of the run.</param>
    let private gposLookLooks<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (program: 'P) (machine: byref<GposMachine>) (glyphs: ImmutableArray<GlyphInfo>) =
        let infos = glyphs.AsSpan()
        match program.GlyphLookups.IsEmpty with
        | true -> gposApplyLookups program (Digest.ofSpan infos) UInt64.MaxValue &machine
        | false ->
            let bits = lookupsCovering program.GlyphLookups infos
            let digest = match program.LookupEnd > CandidatePositions with | true -> Digest.ofSpan infos | false -> Digest.empty
            match bits = 0UL && program.LookupEnd <= CandidatePositions with
            | true -> ()
            | false -> gposApplyLookups program digest bits &machine

    /// <summary>Runs the lookups of a GPOS program over a run, the positioning kernel: each lookup is one loop over the run
    /// that writes the positions and the glyph flags in place, on working copies made on the first write, and the arrays
    /// are published once at the end. A run no lookup changed comes back as it went in.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="program">The program.</param>
    /// <param name="admission">What a look at the run's glyphs said, or an unknown admission.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runGposAdmitted<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (input: RunInput) (program: 'P) (admission: RunAdmission) =
        let len = input.Infos.Length
        let usable = input.Successful && input.HavePositions && input.Positions.Length = len && input.Props.Length = len
        let mutable machine =
            { SourceInfos = input.Infos
              SourceProps = input.Props
              SourcePositions = input.Positions
              Info = Array.empty
              InfosOwned = false
              Positions = Array.empty
              PositionsOwned = false
              Len = (match usable with | true -> len | false -> 0)
              Idx = 0
              MaxOps = input.MaxOps
              Successful = input.Successful
              Scratch = input.ScratchFlags
              LastBase = -1
              LastBaseUntil = 0
              ClusterLevel = input.ClusterLevel
              Flags = input.Flags }
        match machine.Len > 0 with
        | false -> ()
        | true ->
            match admission.Known with
            | true -> gposApplyLookups program admission.GlyphDigest admission.Candidates &machine
            | false -> gposLookLooks program &machine input.Infos
        match machine.InfosOwned || machine.PositionsOwned with
        | false ->
            { Infos = input.Infos
              Props = input.Props
              Positions = input.Positions
              HavePositions = input.HavePositions
              MaxOps = machine.MaxOps
              Successful = machine.Successful
              Unchanged = machine.MaxOps = input.MaxOps && machine.Successful = input.Successful
              Serial = input.Serial
              RandomState = input.RandomState
              ScratchFlags = input.ScratchFlags }
        | true ->
            { Infos = (match machine.InfosOwned with | true -> publish machine.Info | false -> input.Infos)
              Props = input.Props
              Positions = (match machine.PositionsOwned with | true -> publish machine.Positions | false -> input.Positions)
              HavePositions = input.HavePositions
              MaxOps = machine.MaxOps
              Successful = machine.Successful
              Unchanged = false
              Serial = input.Serial
              RandomState = input.RandomState
              ScratchFlags = machine.Scratch }

    /// <summary>Finishes the glyph flags of a positioned run that the caller made in the same call and has shared with no one:
    /// every mask is cut down to the defined flags, in place, <c>hb_propagate_flags</c>'s final mask, and the clusters are
    /// checked to be each a single glyph, in which case the propagation over a cluster changes nothing.</summary>
    /// <param name="infos">The glyphs of a run this call made.</param>
    /// <param name="andMask">The mask the masks are cut down to.</param>
    /// <returns>True when no two neighbouring glyphs share a cluster.</returns>
    let finishFlags (infos: ImmutableArray<GlyphInfo>) (andMask: uint32) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable distinct = true
        let mutable index = 0
        while index < array.Length do
            array[index] <- { array[index] with Mask = array[index].Mask &&& andMask }
            distinct <- distinct && (index = 0 || array[index].Cluster <> array[index - 1].Cluster)
            index <- index + 1
        distinct

    /// <summary>Gives every glyph of a run this call made the same mask, in place.</summary>
    /// <param name="infos">The glyphs of a run this call made and has shared with no one.</param>
    /// <param name="mask">The mask.</param>
    let setAllMasks (infos: ImmutableArray<GlyphInfo>) (mask: uint32) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable index = 0
        while index < array.Length do
            array[index] <- { array[index] with Mask = mask }
            index <- index + 1

    /// <summary>Runs the lookups of a GPOS program over a run, looking at the run's glyphs itself.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="program">The program.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runGpos<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (input: RunInput) (program: 'P) =
        runGposAdmitted input program { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty }

    /// <summary>Builds the cursor of a machine at a given input index, the view a null transition of a stretch reads.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The input index.</param>
    /// <returns>The cursor.</returns>
    let private makeCursorAt (machine: inref<Machine>) (index: int) =
        let infos =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphInfo>(machine.Info, 0, machine.Len)
            | false -> machine.SourceInfos.AsSpan(0, machine.Len)
        let props =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphProps>(machine.Props, 0, machine.Len)
            | false -> machine.SourceProps.AsSpan(0, machine.Len)
        let outInfos =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphInfo>(outInfoArray &machine, 0, machine.OutLen)
            | false -> machine.SourceInfos.AsSpan(0, machine.OutLen)
        let outProps =
            match machine.Materialized with
            | true -> ReadOnlySpan<GlyphProps>(outPropsArray &machine, 0, machine.OutLen)
            | false -> machine.SourceProps.AsSpan(0, machine.OutLen)
        let positions =
            match machine.HavePositions with
            | false -> ReadOnlySpan.Empty
            | true ->
                match machine.PositionsMaterialized with
                | true -> ReadOnlySpan<GlyphPosition>(machine.Positions, 0, machine.Len)
                | false -> machine.SourcePositions.AsSpan(0, machine.Len)
        RunCursor(
            infos,
            props,
            index,
            outInfos,
            outProps,
            positions,
            machine.HaveOutput,
            machine.ClusterLevel,
            machine.Flags,
            machine.Serial,
            machine.RandomState,
            machine.MaxOps,
            machine.Successful)

    /// <summary>The code point of the input glyph at the machine's cursor.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The code point.</returns>
    let private cursorGlyph (machine: inref<Machine>) =
        match machine.Materialized with
        | true -> machine.Info[machine.Idx].Codepoint
        | false -> machine.SourceInfos[machine.Idx].Codepoint

    /// <summary>The cluster of the input glyph at the machine's cursor.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The cluster.</returns>
    let private cursorCluster (machine: inref<Machine>) =
        match machine.Materialized with
        | true -> machine.Info[machine.Idx].Cluster
        | false -> machine.SourceInfos[machine.Idx].Cluster

    /// <summary>Whether every glyph of a range already carries all the bits of a mask.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="mask">The mask bits.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="finish">One past the last glyph.</param>
    /// <returns>True when no glyph of the range lacks a bit.</returns>
    [<TailCall>]
    let rec private allFlagged (infos: ReadOnlySpan<GlyphInfo>) (mask: uint32) (index: int) (finish: int) =
        match index >= finish with
        | true -> true
        | false ->
            match infos[index].Mask &&& mask = mask with
            | true -> allFlagged infos mask (index + 1) finish
            | false -> false

    /// <summary>Marks the glyphs of an in-place range unsafe to break from the output buffer, <c>unsafe_to_break_from_outbuffer</c>,
    /// without taking a working copy when every glyph already carries the flags: the range only charges the budget then.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let private aatUnsafeToBreak (machine: byref<Machine>) (start: int) (finish: int) =
        let bound = min finish machine.Len
        let flags = GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat
        match (not machine.HaveOutput) && start < bound && (uint32 finish - uint32 start) <= uint32 MaxGlyphFlagRange && allFlagged (inputInfos &machine) flags start bound with
        | true ->
            let maxOps = machine.MaxOps - (bound - start)
            machine.MaxOps <- maxOps
            match maxOps < 0 with
            | true -> machine.Successful <- false
            | false -> ()
        | false -> unsafeToBreakFromOutbuffer &machine start finish

    /// <summary>Runs the transition of an entry to completion, one micro-step at a time, each edit applied before the next
    /// step reads the machine.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="decisions">The machine's decisions.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The context, updated.</param>
    let private aatTransition<'M, 'E, 'C when 'M : struct and 'M :> IAatMachine<'E, 'C> and 'E : struct and 'C : struct>
            (machine: byref<Machine>) (decisions: byref<'M>) (entry: 'E) (context: byref<'C>) =
        let mutable running = true
        while running do
            let cursor = makeCursor &machine
            let struct (emitted, edit, next) = decisions.Transition(&cursor, entry, context)
            context <- next
            match emitted with
            | true -> applyEdit &machine edit
            | false -> running <- false

    /// <summary>What a subtable kind does with the glyphs when a transition fires: the transition itself and the null
    /// transition of a stretch, applied straight to the working copy.</summary>
    type private IAatActor<'E when 'E : struct> =
        /// <summary>Whether the null transition stands in for the transition of an entry that is not actionable.</summary>
        abstract Quiet: entry: 'E -> bool
        /// <summary>Whether the null transition of an entry changes nothing.</summary>
        abstract NullIsIdentity: entry: 'E -> bool
        /// <summary>The transition of an entry at the cursor.</summary>
        abstract Perform: machine: byref<Machine> * entry: 'E -> unit
        /// <summary>The null transition of an entry at a glyph of a stretch.</summary>
        abstract NullAt: machine: byref<Machine> * entry: 'E * index: int -> unit

    /// <summary>The actor of a subtable kind that describes its transition as edits: it carries the kind's decisions and its
    /// context, and applies each edit the kind emits.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private EditActor<'M, 'E, 'C when 'M : struct and 'M :> IAatMachine<'E, 'C> and 'E : struct and 'C : struct> =
        val mutable Kind: 'M
        val mutable Context: 'C
        new (kind: 'M, context: 'C) = { Kind = kind; Context = context }
        interface IAatActor<'E> with
            member this.Quiet entry = this.Kind.Quiet entry
            member this.NullIsIdentity entry = this.Kind.NullIsIdentity entry
            member this.Perform (machine, entry) = aatTransition &machine &this.Kind entry &this.Context
            member this.NullAt (machine, entry, index) =
                let cursor = makeCursorAt &machine index
                this.Context <- this.Kind.NullTransition(&cursor, entry, this.Context)

    /// <summary>Applies the null transition to every glyph of a stretch, each with the cursor at its own index.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="actor">The actor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="finish">One past the last glyph of the stretch.</param>
    let private aatNullStretch<'A, 'E when 'A : struct and 'A :> IAatActor<'E> and 'E : struct>
            (machine: byref<Machine>) (actor: byref<'A>) (entry: 'E) (finish: int) =
        let mutable index = machine.Idx
        while index < finish do
            actor.NullAt(&machine, entry, index)
            index <- index + 1

    /// <summary>Moves past the glyph a finished transition consumed, or charges an operation for an epsilon transition
    /// and moves on anyway when the budget was already spent.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="entry">The entry.</param>
    let private aatAdvance<'M, 'E when 'M : struct and 'M :> IAatTables<'E> and 'E : struct>
            (machine: byref<Machine>) (tables: byref<'M>) (entry: 'E) =
        match tables.DontAdvance entry with
        | false -> nextGlyphs &machine 1
        | true ->
            let exhausted = machine.MaxOps <= 0
            chargeOps &machine 1
            match exhausted with
            | true -> nextGlyphs &machine 1
            | false -> ()

    /// <summary>Marks the glyphs before the current one unsafe to break when the transition is not safe, runs the
    /// transition of the entry and advances. Returns whether the pass is over.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="actor">The actor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="isSafe">Whether breaking before the glyph is safe.</param>
    /// <param name="state">The machine state, updated.</param>
    /// <returns>True when the pass is over.</returns>
    let private aatAct<'M, 'A, 'E when 'M : struct and 'M :> IAatTables<'E> and 'A : struct and 'A :> IAatActor<'E> and 'E : struct>
            (machine: byref<Machine>) (tables: byref<'M>) (actor: byref<'A>) (entry: 'E) (isSafe: bool) (state: byref<int>) =
        let backtrack = match machine.HaveOutput with | true -> machine.OutLen | false -> machine.Idx
        match (not isSafe) && backtrack > 0 && machine.Idx < machine.Len with
        | true -> aatUnsafeToBreak &machine (backtrack - 1) (machine.Idx + 1)
        | false -> ()
        match actor.Quiet entry, actor.NullIsIdentity entry with
        | true, true -> ()
        | true, false -> actor.NullAt(&machine, entry, machine.Idx)
        | false, _ -> actor.Perform(&machine, entry)
        state <- tables.NewState entry
        match machine.Idx >= machine.Len with
        | true -> true
        | false ->
            aatAdvance &machine &tables entry
            false

    /// <summary>Consumes a stretch of glyphs that stay on a safe non-epsilon transition at once; the null transition's
    /// context update runs for each of them. Returns whether the pass is over.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="actor">The actor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="klass">The class the stretch stays on.</param>
    /// <param name="skipEnd">Whether the end-of-text transition is skipped afterwards, updated.</param>
    /// <param name="skipsEnd">The value the stretch gives <paramref name="skipEnd"/>.</param>
    /// <returns>True when the pass is over.</returns>
    let private aatBatch<'M, 'A, 'E when 'M : struct and 'M :> IAatTables<'E> and 'A : struct and 'A :> IAatActor<'E> and 'E : struct>
            (machine: byref<Machine>) (tables: byref<'M>) (actor: byref<'A>) (entry: 'E) (klass: int) (skipEnd: byref<bool>) (skipsEnd: bool) =
        match machine.Idx >= machine.Len with
        | true -> true
        | false ->
            let infos =
                match machine.Materialized with
                | true -> ReadOnlySpan<GlyphInfo>(machine.Info, 0, machine.Len)
                | false -> machine.SourceInfos.AsSpan(0, machine.Len)
            let sameClass = tables.StretchEnd(infos, machine.Idx, klass)
            let identity = actor.NullIsIdentity entry
            let finish =
                match skipsEnd && identity with
                | true -> max sameClass (tables.QuietEnd(infos, machine.Idx))
                | false -> sameClass
            match identity with
            | true -> ()
            | false -> aatNullStretch &machine &actor entry finish
            nextGlyphs &machine (finish - machine.Idx)
            skipEnd <- skipsEnd
            false

    /// <summary>One step of the range walk: resets the state in a range the subtable does not apply to, else acts.
    /// Returns whether the pass is over.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="actor">The actor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="klass">The class.</param>
    /// <param name="state">The machine state, updated.</param>
    /// <param name="range">The range index, updated.</param>
    /// <returns>True when the pass is over.</returns>
    let private aatRanged<'M, 'A, 'E when 'M : struct and 'M :> IAatTables<'E> and 'A : struct and 'A :> IAatActor<'E> and 'E : struct>
            (machine: byref<Machine>) (tables: byref<'M>) (actor: byref<'A>) (entry: 'E) (klass: int) (state: byref<int>) (range: byref<int>) =
        let found =
            match machine.Idx < machine.Len with
            | true -> tables.AdvanceRange(range, cursorCluster &machine)
            | false -> range
        range <- found
        match tables.RangeEnabled found with
        | false ->
            match machine.Idx >= machine.Len with
            | true -> true
            | false ->
                state <- 0
                nextGlyphs &machine 1
                false
        | true ->
            let isSafe = tables.SafeToBreak(state, klass, entry, tables.NewState entry)
            aatAct &machine &tables &actor entry isSafe &state

    /// <summary>Classifies the glyph at the cursor and takes one step of <c>StateTableDriver::drive</c>. Returns whether the
    /// pass is over.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="actor">The actor.</param>
    /// <param name="state">The machine state, updated.</param>
    /// <param name="range">The range index, updated.</param>
    /// <param name="skipEnd">Whether the end-of-text transition is skipped, updated.</param>
    /// <returns>True when the pass is over.</returns>
    let private aatFetch<'M, 'A, 'E when 'M : struct and 'M :> IAatTables<'E> and 'A : struct and 'A :> IAatActor<'E> and 'E : struct>
            (machine: byref<Machine>) (tables: byref<'M>) (actor: byref<'A>) (state: byref<int>) (range: byref<int>) (skipEnd: byref<bool>) =
        let atEnd = machine.Idx >= machine.Len
        match (not machine.Successful) || (atEnd && skipEnd) with
        | true -> true
        | false ->
            let klass = match atEnd with | true -> 0 | false -> tables.ClassOf(cursorGlyph &machine)
            let entry = tables.EntryAt(state, klass)
            let nextState = tables.NewState entry
            match tables.RangeCount > 1 with
            | true -> aatRanged &machine &tables &actor entry klass &state &range
            | false ->
                let isSafe = tables.SafeToBreak(state, klass, entry, nextState)
                let stable = isSafe && not (tables.DontAdvance entry)
                match stable, state = 0 && nextState = 0, nextState = state && not atEnd with
                | true, true, _ -> aatBatch &machine &tables &actor entry klass &skipEnd true
                | true, false, true -> aatBatch &machine &tables &actor entry klass &skipEnd false
                | _ ->
                    skipEnd <- false
                    aatAct &machine &tables &actor entry isSafe &state

    /// <summary>Drives one AAT state table over the machine: <c>StateTableDriver::drive</c> as one loop.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="actor">The actor.</param>
    let private aatLoop<'M, 'A, 'E when 'M : struct and 'M :> IAatTables<'E> and 'A : struct and 'A :> IAatActor<'E> and 'E : struct>
            (machine: byref<Machine>) (tables: byref<'M>) (actor: byref<'A>) =
        let mutable state = 0
        let mutable range = 0
        let mutable skipEnd = false
        let mutable finished = false
        match tables.InPlace with
        | true -> ()
        | false ->
            let hadPositions = machine.HavePositions || not machine.SourcePositions.IsEmpty
            clearOutput &machine
            machine.Passive <- machine.Passive && not hadPositions
        while not finished do
            finished <- aatFetch &machine &tables &actor &state &range &skipEnd
        sync &machine

    /// <summary>Runs one AAT state-table subtable over a run's arrays and returns the run fields it produces and the
    /// finished context.</summary>
    /// <typeparam name="'M">The machine's decisions, a struct.</typeparam>
    /// <typeparam name="'E">The entry type.</typeparam>
    /// <typeparam name="'C">The context type.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="decisions">The machine's decisions.</param>
    /// <param name="seed">The initial context.</param>
    /// <returns>The run fields the machine produces and the finished context.</returns>
    let runAatPass<'M, 'E, 'C when 'M : struct and 'M :> IAatMachine<'E, 'C> and 'E : struct and 'C : struct>
            (input: RunInput) (decisions: 'M) (seed: 'C) =
        let mutable initial = newMachine input Digest.empty false decisions.InPlace
        let mutable tables = decisions
        let mutable actor = EditActor<'M, 'E, 'C>(decisions, seed)
        aatLoop &initial &tables &actor
        let unchanged = initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps
        struct (finishRun &initial unchanged input.Positions, actor.Context)

    /// <summary>Replaces one input glyph, <c>replace_glyph_inplace</c>, and records it for the glyph set when collecting.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="decisions">The decisions.</param>
    /// <param name="index">The input index.</param>
    /// <param name="glyph">The new glyph.</param>
    /// <param name="written">The glyphs written, when collecting.</param>
    /// <param name="hasDeleted">Whether a deleted glyph has been written, updated.</param>
    let private contextualReplace<'D, 'E when 'D : struct and 'D :> IContextualDecisions<'E> and 'E : struct>
            (machine: byref<Machine>) (decisions: byref<'D>) (index: int) (glyph: uint32) (written: List<uint32> | null) (hasDeleted: byref<bool>) =
        let props = decisions.Props(glyph, (inputProps &machine)[index])
        touch &machine
        setInputGlyph &machine index glyph props
        match written with
        | null -> ()
        | collected -> collected.Add glyph
        hasDeleted <- hasDeleted || glyph = 0xFFFFu

    /// <summary>The actor of a contextual substitution subtable: the transition of <c>ContextualSubtable</c> applied straight to
    /// the working copy, with the mark it keeps and the glyphs it writes.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private ContextualActor<'D, 'E when 'D : struct and 'D :> IContextualDecisions<'E> and 'E : struct> =
        val mutable Decisions: 'D
        val mutable MarkSet: bool
        val mutable Mark: int
        val mutable HasDeleted: bool
        val Written: List<uint32> | null
        new (decisions: 'D) =
            { Decisions = decisions
              MarkSet = false
              Mark = 0
              HasDeleted = false
              Written = (match decisions.Collect with | false -> (null: List<uint32> | null) | true -> List<uint32>()) }
        new (decisions: 'D, written: List<uint32> | null) =
            { Decisions = decisions
              MarkSet = false
              Mark = 0
              HasDeleted = false
              Written = written }
        interface IAatActor<'E> with
            member this.Quiet entry = this.Decisions.Extended && not (this.Decisions.Actionable entry)
            member this.NullIsIdentity entry = not (this.Decisions.SetsMark entry)
            member this.NullAt (machine, entry, index) =
                match this.Decisions.SetsMark entry with
                | true ->
                    this.MarkSet <- true
                    this.Mark <- index
                | false -> ()
            member this.Perform (machine, entry) =
                match machine.Idx >= machine.Len && not this.MarkSet with
                | true -> ()
                | false ->
                    let infos = inputInfos &machine
                    let struct (markFound, markGlyph) = this.Decisions.Replacement(this.Decisions.MarkIndex entry, infos[this.Mark].Codepoint)
                    let target = min machine.Idx (machine.Len - 1)
                    let currentSource = match markFound && target = this.Mark with | true -> markGlyph | false -> infos[target].Codepoint
                    let struct (currentFound, currentGlyph) = this.Decisions.Replacement(this.Decisions.CurrentIndex entry, currentSource)
                    let setsMark = this.Decisions.SetsMark entry
                    let nextMarkSet = setsMark || this.MarkSet
                    let nextMark = match setsMark with | true -> machine.Idx | false -> this.Mark
                    match markFound with
                    | true ->
                        unsafeToBreak &machine this.Mark (min (machine.Idx + 1) machine.Len)
                        contextualReplace &machine &this.Decisions this.Mark markGlyph this.Written &this.HasDeleted
                    | false -> ()
                    match currentFound with
                    | true -> contextualReplace &machine &this.Decisions target currentGlyph this.Written &this.HasDeleted
                    | false -> ()
                    this.MarkSet <- nextMarkSet
                    this.Mark <- nextMark

    /// <summary>Runs one contextual substitution subtable over a run's arrays: the state-table driver with the substitutions
    /// written straight into the working copy.</summary>
    /// <typeparam name="'M">The machine's tables, a struct.</typeparam>
    /// <typeparam name="'E">The entry type.</typeparam>
    /// <typeparam name="'D">The contextual decisions, a struct.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The contextual decisions.</param>
    /// <returns>The run fields the machine produces, the glyphs written when collecting, and whether a deleted glyph was written.</returns>
    let runContextualPass<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> IContextualDecisions<'E>>
            (input: RunInput) (tables: 'M) (decisions: 'D) =
        let mutable initial = newMachine input Digest.empty false true
        let mutable owned = tables
        let mutable actor = ContextualActor<'D, 'E>(decisions)
        aatLoop &initial &owned &actor
        let unchanged = initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps
        let written =
            match actor.Written with
            | null -> ImmutableArray<uint32>.Empty
            | collected -> ImmutableArray.CreateRange collected
        struct (finishRun &initial unchanged input.Positions, written, actor.HasDeleted)

    /// <summary>The position of a glyph as the machine stands: the working copy once written, else the source.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The glyph.</param>
    /// <returns>The position.</returns>
    let private currentPosition (machine: inref<Machine>) (index: int) =
        match machine.PositionsMaterialized with
        | true -> machine.Positions[index]
        | false -> machine.SourcePositions[index]

    /// <summary>Writes the kerning of a pair: the positions, the scratch flag across the stream, and the unsafe-to-break flags.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="decisions">The machine's decisions.</param>
    /// <param name="index">The first glyph.</param>
    /// <param name="pair">The second glyph.</param>
    /// <param name="kern">The kerning.</param>
    let private kernApply<'K when 'K : struct and 'K :> IKernMachine> (machine: byref<Machine>) (decisions: byref<'K>) (index: int) (pair: int) (kern: int) =
        let struct (first, second) = decisions.Apply(kern, currentPosition &machine index, currentPosition &machine pair)
        materialize &machine
        match decisions.CrossStream with
        | false ->
            setPosition &machine index first
            setPosition &machine pair second
        | true ->
            setPosition &machine pair second
            addScratch &machine ScratchFlags.HasGposAttachment
        unsafeToBreak &machine index (pair + 1)
        machine.Passive <- false

    /// <summary>Runs one kern machine over a run's arrays: <c>hb_kern_machine_t::kern</c> as one loop, writing the positions and
    /// the glyph flags of the pairs that kern on working copies made on the first write.</summary>
    /// <typeparam name="'K">The machine's decisions, a struct.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="decisions">The machine's decisions.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runKernPass<'K when 'K : struct and 'K :> IKernMachine> (input: RunInput) (decisions: 'K) =
        let mutable owned = decisions
        let mutable machine = newMachine input Digest.empty false true
        let mutable index = 0
        while index < machine.Len do
            let struct (next, pair, kern) = owned.Scan(inputInfos &machine, inputProps &machine, index)
            match kern = 0 with
            | true -> index <- next
            | false ->
                kernApply &machine &owned index pair kern
                index <- pair
        finishRun &machine (machine.Passive && machine.Successful && machine.MaxOps = machine.InitialMaxOps) input.Positions

    /// <summary>Runs one kern machine over arrays the caller made in this call and shares with no one: the infos, properties and
    /// positions are written in place, so that a line whose pairs kern copies none of them; a run whose properties or positions are not
    /// of its length takes the copying route.</summary>
    /// <typeparam name="'K">The machine's decisions, a struct.</typeparam>
    /// <param name="input">The run fields the machine reads, whose arrays are the caller's own.</param>
    /// <param name="decisions">The machine's decisions.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runKernPassOwned<'K when 'K : struct and 'K :> IKernMachine> (input: RunInput) (decisions: 'K) =
        let len = input.Infos.Length
        let adoptable = len > 0 && input.Props.Length = len && input.HavePositions && input.Positions.Length = len
        match adoptable with
        | false -> runKernPass input decisions
        | true ->
            let mutable owned = decisions
            let mutable machine = newMachine input Digest.empty false true
            machine.Info <- nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)
            machine.Props <- nonNull (ImmutableCollectionsMarshal.AsArray input.Props)
            machine.Positions <- nonNull (ImmutableCollectionsMarshal.AsArray input.Positions)
            machine.Materialized <- true
            machine.PositionsMaterialized <- true
            machine.Allocated <- growAllocated len 0
            let mutable index = 0
            while index < machine.Len do
                let struct (next, pair, kern) = owned.Scan(inputInfos &machine, inputProps &machine, index)
                match kern = 0 with
                | true -> index <- next
                | false ->
                    kernApply &machine &owned index pair kern
                    index <- pair
            finishRun &machine (machine.Passive && machine.Successful && machine.MaxOps = machine.InitialMaxOps) input.Positions

    /// <summary>Sets the unsafe-to-concat flag on every glyph of an array the caller made in this call and shares with no one.</summary>
    /// <param name="infos">The infos, written in place.</param>
    let markConcatOwned (infos: ImmutableArray<GlyphInfo>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        for index in 0 .. array.Length - 1 do
            array[index] <- { array[index] with Mask = array[index].Mask ||| GlyphFlagUnsafeToConcat }

    /// <summary>The component stack of a ligature transition, <c>match_positions</c>: <c>HB_MAX_CONTEXT_LENGTH</c> output
    /// positions, held inline in the actor so that a pass that pushes components allocates nothing for them.</summary>
    [<Struct; InlineArray(64)>]
    type private ComponentStack =
        val mutable First: uint32

    /// <summary>The <c>LigActionLast</c> bit of an action word, the last action of a group.</summary>
    [<Literal>]
    let private LigActionLastBit = 0x80000000u

    /// <summary>The <c>LigActionStore</c> bit of an action word, which stores the ligature.</summary>
    [<Literal>]
    let private LigActionStoreBit = 0x40000000u

    /// <summary>The glyph a ligature leaves in place of a component it takes, the deleted glyph.</summary>
    [<Literal>]
    let private DeletedGlyphId = 0xFFFFu

    [<Literal>]
    let private LigatureRead = 1

    [<Literal>]
    let private LigatureStore = 2

    [<Literal>]
    let private LigatureDeleteCheck = 3

    [<Literal>]
    let private LigatureDeleteProps = 4

    [<Literal>]
    let private LigatureDeleteGlyph = 5

    [<Literal>]
    let private LigatureMerge = 6

    [<Literal>]
    let private LigatureAdvance = 7

    [<Literal>]
    let private LigatureDone = 8

    [<Literal>]
    let private LigatureStop = 9

    /// <summary>Moves the cursor of a ligature transition, the <c>MoveTo</c> edit applied straight to the machine.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="target">The output index to move to.</param>
    let private ligatureMoveTo (machine: byref<Machine>) (target: int) =
        materialize &machine
        moveTo &machine target
        machine.Passive <- false

    /// <summary>Rewrites the properties of the current glyph, the <c>SetCurrentProps</c> edit applied straight to the machine.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="props">The properties.</param>
    let private ligatureSetProps (machine: byref<Machine>) (props: GlyphProps) =
        materialize &machine
        setCurrentProps &machine props
        machine.Passive <- false

    /// <summary>Replaces the current glyph, the <c>ReplaceGlyph</c> edit applied straight to the machine.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="glyph">The glyph.</param>
    let private ligatureReplace (machine: byref<Machine>) (glyph: uint32) =
        materialize &machine
        let mutable written = glyph
        replaceGlyphs &machine 1 1 (MemoryMarshal.CreateReadOnlySpan(&written, 1))
        machine.Passive <- false

    /// <summary>Where a ligature transition is within its action list: the component it pops to, the ligature index it
    /// accumulates, the action word it reads, the ligature it stores and the phase of the micro-program.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private LigatureWalk =
        { mutable Cursor: int
          mutable LigatureIndex: uint32
          mutable ActionAt: int64
          EndAt: int
          mutable Action: uint32
          mutable LigatureGlyph: uint32
          mutable LigatureEnd: int
          mutable Phase: int }

    /// <summary>The actor of a ligature subtable: the transition of <c>LigatureSubtable</c> as straight code over the working
    /// copy, with the component stack, the glyphs it writes and whether it wrote a deleted glyph.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private LigatureActor<'D, 'E when 'D : struct and 'D :> ILigatureDecisions<'E> and 'E : struct> =
        val mutable Decisions: 'D
        val mutable Stack: ComponentStack
        val mutable MatchLength: int
        val mutable HasDeleted: bool
        val mutable Written: List<uint32> | null
        new (decisions: 'D) =
            { Decisions = decisions
              Stack = Unchecked.defaultof<ComponentStack>
              MatchLength = 0
              HasDeleted = false
              Written = (null: List<uint32> | null) }
        new (decisions: 'D, written: List<uint32> | null) =
            { Decisions = decisions
              Stack = Unchecked.defaultof<ComponentStack>
              MatchLength = 0
              HasDeleted = false
              Written = written }

        /// <summary>The position at a stack depth.</summary>
        member this.At (depth: int) = (MemoryMarshal.CreateSpan(&this.Stack.First, 64))[depth % 64]

        /// <summary>Pushes an output position, never twice in a row for the same one, <c>match_positions[match_length++]</c>.</summary>
        member this.Push (outLen: int) =
            let stack = MemoryMarshal.CreateSpan(&this.Stack.First, 64)
            let length = match this.MatchLength > 0 && stack[(this.MatchLength - 1) % 64] = uint32 outLen with | true -> this.MatchLength - 1 | false -> this.MatchLength
            stack[length % 64] <- uint32 outLen
            this.MatchLength <- length + 1

        /// <summary>Records a written glyph for the glyph set and for the deleted-glyph flag.</summary>
        member this.Record (glyph: uint32) =
            match this.Decisions.Collect with
            | false -> ()
            | true ->
                match this.Written with
                | null ->
                    let created = List<uint32>()
                    created.Add glyph
                    this.Written <- created
                | collected -> collected.Add glyph
            this.HasDeleted <- this.HasDeleted || glyph = uint32 DeletedGlyphId

        /// <summary>Moves to the end the transition recorded and leaves the action list.</summary>
        member this.Finish (machine: byref<Machine>, walk: byref<LigatureWalk>) =
            ligatureMoveTo &machine walk.EndAt
            walk.Phase <- LigatureDone

        /// <summary>Pops to the previous component, or clears the stack and leaves when none is left, <c>popOrUnderflow</c>.</summary>
        member this.Pop (machine: byref<Machine>, walk: byref<LigatureWalk>) =
            match walk.Cursor = 0 with
            | true ->
                this.MatchLength <- 0
                this.Finish(&machine, &walk)
            | false ->
                walk.Cursor <- walk.Cursor - 1
                ligatureMoveTo &machine (int (this.At walk.Cursor))
                walk.Phase <- LigatureRead

        /// <summary>Reads the ligature of the current action and stores it, or leaves when it lies outside the subtable.</summary>
        member this.Store (machine: byref<Machine>, walk: byref<LigatureWalk>) =
            let struct (inside, ligature) = this.Decisions.ReadLigature walk.LigatureIndex
            match inside with
            | false -> this.Finish(&machine, &walk)
            | true ->
                walk.LigatureGlyph <- ligature
                walk.LigatureEnd <- int (this.At (this.MatchLength - 1)) + 1
                ligatureSetProps &machine (this.Decisions.Props(ligature, (inputProps &machine)[machine.Idx]))
                walk.Phase <- LigatureStore

        /// <summary>Reads the action word and the component of the glyph at the cursor, and moves on to the next action or to
        /// the ligature.</summary>
        member this.Read (machine: byref<Machine>, walk: byref<LigatureWalk>) =
            let struct (inside, word) = this.Decisions.ReadAction walk.ActionAt
            let glyph = cursorGlyph &machine
            let struct (known, data) = match inside with | true -> this.Decisions.ReadComponent(glyph, word) | false -> struct (false, 0u)
            walk.Action <- word
            match known with
            | false -> this.Finish(&machine, &walk)
            | true ->
                walk.LigatureIndex <- walk.LigatureIndex + data
                match word &&& (LigActionStoreBit ||| LigActionLastBit) <> 0u with
                | true -> this.Store(&machine, &walk)
                | false ->
                    walk.ActionAt <- walk.ActionAt + 4L
                    this.Pop(&machine, &walk)

        /// <summary>Takes the next action, or leaves after the last of a group.</summary>
        member this.Advance (machine: byref<Machine>, walk: byref<LigatureWalk>) =
            walk.ActionAt <- walk.ActionAt + 4L
            match walk.Action &&& LigActionLastBit <> 0u with
            | true -> this.Finish(&machine, &walk)
            | false -> this.Pop(&machine, &walk)

        /// <summary>Deletes a component the ligature took, or moves to the end of the ligature when none is left.</summary>
        member this.DeleteCheck (machine: byref<Machine>, walk: byref<LigatureWalk>) =
            match this.MatchLength - 1 > walk.Cursor with
            | true ->
                this.MatchLength <- this.MatchLength - 1
                ligatureMoveTo &machine (int (this.At this.MatchLength))
                walk.Phase <- LigatureDeleteProps
            | false ->
                ligatureMoveTo &machine walk.LigatureEnd
                walk.Phase <- LigatureMerge

        /// <summary>Runs the action list of an entry: the pop, read, store, delete and merge steps, each checking the machine
        /// first as every micro-step does.</summary>
        member this.Actions (machine: byref<Machine>, entry: 'E) =
            let mutable walk = { Cursor = this.MatchLength - 1; LigatureIndex = 0u; ActionAt = this.Decisions.ActionAt entry; EndAt = machine.OutLen; Action = 0u; LigatureGlyph = 0u; LigatureEnd = 0; Phase = LigatureRead }
            ligatureMoveTo &machine (int (this.At walk.Cursor))
            while walk.Phase <> LigatureStop do
                match machine.Successful, walk.Phase with
                | false, _ -> walk.Phase <- LigatureStop
                | true, LigatureRead -> this.Read(&machine, &walk)
                | true, LigatureStore ->
                    this.Record walk.LigatureGlyph
                    ligatureReplace &machine walk.LigatureGlyph
                    walk.Phase <- LigatureDeleteCheck
                | true, LigatureDeleteCheck -> this.DeleteCheck(&machine, &walk)
                | true, LigatureDeleteProps ->
                    ligatureSetProps &machine (this.Decisions.Props(uint32 DeletedGlyphId, (inputProps &machine)[machine.Idx]))
                    walk.Phase <- LigatureDeleteGlyph
                | true, LigatureDeleteGlyph ->
                    this.Record (uint32 DeletedGlyphId)
                    ligatureReplace &machine (uint32 DeletedGlyphId)
                    walk.Phase <- LigatureDeleteCheck
                | true, LigatureMerge ->
                    materialize &machine
                    mergeOutClusters &machine (int (this.At walk.Cursor)) machine.OutLen
                    machine.Passive <- false
                    walk.Phase <- LigatureAdvance
                | true, LigatureAdvance -> this.Advance(&machine, &walk)
                | true, _ -> walk.Phase <- LigatureStop

        interface IAatActor<'E> with
            member this.Quiet entry = not (this.Decisions.Performs entry)
            member this.NullIsIdentity entry = not (this.Decisions.SetsComponent entry)
            member this.NullAt (_, entry, index) =
                match this.Decisions.SetsComponent entry with
                | true -> this.Push index
                | false -> ()
            member this.Perform (machine, entry) =
                match machine.Successful with
                | false -> ()
                | true ->
                    match this.Decisions.SetsComponent entry with
                    | true -> this.Push machine.Idx
                    | false -> ()
                    match this.Decisions.Performs entry && this.MatchLength > 0 && machine.Idx < machine.Len with
                    | true -> this.Actions(&machine, entry)
                    | false -> ()

    /// <summary>Runs one ligature subtable over a run's arrays: the state-table driver with the action list run straight on the
    /// working copy.</summary>
    /// <typeparam name="'M">The machine's tables, a struct.</typeparam>
    /// <typeparam name="'E">The entry type.</typeparam>
    /// <typeparam name="'D">The ligature decisions, a struct.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The ligature decisions.</param>
    /// <returns>The run fields the machine produces, the glyphs written when collecting, and whether a deleted glyph was written.</returns>
    let runLigaturePass<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> ILigatureDecisions<'E>>
            (input: RunInput) (tables: 'M) (decisions: 'D) =
        let mutable initial = newMachine input Digest.empty false true
        let mutable owned = tables
        let mutable actor = LigatureActor<'D, 'E>(decisions)
        aatLoop &initial &owned &actor
        let unchanged = initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps
        let written =
            match actor.Written with
            | null -> ImmutableArray<uint32>.Empty
            | collected -> ImmutableArray.CreateRange collected
        struct (finishRun &initial unchanged input.Positions, written, actor.HasDeleted)

    /// <summary>The actor of a rearrangement subtable: the transition of <c>RearrangementSubtable</c> as straight code over the working
    /// copy, with the marked range it keeps.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private RearrangementActor<'D, 'E when 'D : struct and 'D :> IRearrangementDecisions<'E> and 'E : struct> =
        val mutable Decisions: 'D
        val mutable Start: int
        val mutable End: int
        new (decisions: 'D) = { Decisions = decisions; Start = 0; End = 0 }

        interface IAatActor<'E> with
            member this.Quiet entry = this.Decisions.Verb entry = 0
            member this.NullIsIdentity entry = not (this.Decisions.MarksFirst entry || this.Decisions.MarksLast entry)
            member this.NullAt (machine, entry, index) =
                match this.Decisions.MarksFirst entry with
                | true -> this.Start <- index
                | false -> ()
                match this.Decisions.MarksLast entry with
                | true -> this.End <- min (index + 1) machine.Len
                | false -> ()
            member this.Perform (machine, entry) =
                let start = match this.Decisions.MarksFirst entry with | true -> machine.Idx | false -> this.Start
                let finish = match this.Decisions.MarksLast entry with | true -> min (machine.Idx + 1) machine.Len | false -> this.End
                this.Start <- start
                this.End <- finish
                let verb = this.Decisions.Verb entry
                match verb <> 0 && start < finish with
                | false -> ()
                | true ->
                    let moves = this.Decisions.Moves verb
                    let moveLeft = min 2 (moves >>> 4)
                    let moveRight = min 2 (moves &&& 0x0F)
                    match finish - start >= moveLeft + moveRight && finish - start <= MaxContextLength with
                    | false -> ()
                    | true ->
                        materialize &machine
                        mergeClusters &machine start (min (machine.Idx + 1) machine.Len)
                        machine.Passive <- false
                        mergeClusters &machine start finish
                        rearrange &machine start finish moveLeft moveRight ((moves >>> 4) = 3) ((moves &&& 0x0F) = 3)

    /// <summary>Runs one rearrangement subtable over a run's arrays: the state-table driver with the transition run straight on the
    /// working copy.</summary>
    /// <typeparam name="'M">The machine's tables, a struct.</typeparam>
    /// <typeparam name="'E">The entry type.</typeparam>
    /// <typeparam name="'D">The rearrangement decisions, a struct.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The rearrangement decisions.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runRearrangementPass<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> IRearrangementDecisions<'E>>
            (input: RunInput) (tables: 'M) (decisions: 'D) =
        let mutable initial = newMachine input Digest.empty false true
        let mutable owned = tables
        let mutable actor = RearrangementActor<'D, 'E>(decisions)
        aatLoop &initial &owned &actor
        let unchanged = initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps
        finishRun &initial unchanged input.Positions

    /// <summary>Copies the glyph at the cursor to the output, the <c>CopyGlyph</c> edit applied straight to the machine.</summary>
    /// <param name="machine">The machine.</param>
    let private insertionCopy (machine: byref<Machine>) =
        materialize &machine
        outputInfo &machine machine.Info[machine.Idx] machine.Props[machine.Idx]
        machine.Passive <- false

    /// <summary>Appends a glyph to the output, the <c>OutputGlyph</c> edit applied straight to the machine.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="glyph">The glyph.</param>
    let private insertionOutput (machine: byref<Machine>) (glyph: uint32) =
        materialize &machine
        let mutable written = glyph
        replaceGlyphs &machine 0 1 (MemoryMarshal.CreateReadOnlySpan(&written, 1))
        machine.Passive <- false

    /// <summary>Gives the arrays of a machine whose arrays are exact the room a buffer of that length has, once, before the first glyph an
    /// insertion writes: an insertion grows the run, and growing in steps of twice the length copies the arrays at each step.</summary>
    /// <param name="machine">The machine, whose arrays are its own.</param>
    let private insertionReserve (machine: byref<Machine>) =
        match machine.Successful && machine.Info.Length < machine.Allocated with
        | true -> growPhysical &machine machine.Allocated
        | false -> ()

    /// <summary>The actor of an insertion subtable: the transition of <c>InsertionSubtable</c> as straight code over the working
    /// copy, with the mark, the mark the null transition keeps, the glyphs it writes and whether it wrote a deleted glyph.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private InsertionActor<'D, 'E when 'D : struct and 'D :> IInsertionDecisions<'E> and 'E : struct> =
        val mutable Decisions: 'D
        val mutable Mark: int
        val mutable NullMarkBase: int
        val mutable HasDeleted: bool
        val Written: List<uint32> | null
        new (decisions: 'D, written: List<uint32> | null) =
            { Decisions = decisions
              Mark = 0
              NullMarkBase = -1
              HasDeleted = false
              Written = written }
        new (decisions: 'D) =
            { Decisions = decisions
              Mark = 0
              NullMarkBase = -1
              HasDeleted = false
              Written = (match decisions.Collect with | false -> (null: List<uint32> | null) | true -> List<uint32>()) }

        /// <summary>Outputs the glyphs of an insertion one by one, <c>output_glyphs</c>, until one fails.</summary>
        member this.Output (machine: byref<Machine>, start: int, count: int, atEnd: bool) =
            let mutable index = 0
            while index < count && machine.Successful do
                let glyph = this.Decisions.ReadGlyph (start + index)
                match this.Written with
                | null -> ()
                | collected -> collected.Add glyph
                this.HasDeleted <- this.HasDeleted || glyph = uint32 DeletedGlyphId
                match atEnd with
                | true ->
                    insertionOutput &machine glyph
                    match machine.Successful with
                    | false -> ()
                    | true ->
                        let last = machine.OutLen - 1
                        setOutputGlyph &machine last glyph (this.Decisions.Props(glyph, (outPropsArray &machine)[last]))
                | false ->
                    setCurrentProps &machine (this.Decisions.Props(glyph, (inputProps &machine)[machine.Idx]))
                    insertionOutput &machine glyph
                index <- index + 1

        /// <summary>One block of an insertion: the glyph at the cursor copied first unless the block goes before it, the glyphs
        /// output, and that glyph skipped.</summary>
        member this.Block (machine: byref<Machine>, start: int, count: int, before: bool, atEnd: bool) =
            let copies = machine.Idx < machine.Len && not before
            match copies with
            | true -> insertionCopy &machine
            | false -> ()
            match machine.Successful with
            | false -> ()
            | true ->
                this.Output(&machine, start, count, atEnd)
                match machine.Successful && copies with
                | true -> machine.Idx <- machine.Idx + 1
                | false -> ()

        /// <summary>The block of the insertion at the mark, the move past it and the unsafe-to-break flags.</summary>
        member this.MarkedBlock (machine: byref<Machine>, entry: 'E, start: int, count: int, blockEnd: int) =
            this.Block(&machine, start, count, this.Decisions.MarkedBefore entry, machine.Idx = machine.Len)
            match machine.Successful with
            | false -> false
            | true ->
                moveTo &machine (blockEnd + count)
                match machine.Successful with
                | false -> false
                | true ->
                    unsafeToBreakFromOutbuffer &machine this.Mark (min (machine.Idx + 1) machine.Len)
                    machine.Successful

        /// <summary>The insertion at the mark. Returns whether the transition goes on to the insertion at the current glyph.</summary>
        member this.Marked (machine: byref<Machine>, entry: 'E, start: int) =
            let asked = this.Decisions.MarkedCount entry
            materialize &machine
            insertionReserve &machine
            chargeOps &machine asked
            machine.Passive <- false
            match machine.MaxOps <= 0 with
            | true -> false
            | false ->
                let count = this.Decisions.EffectiveCount(start, asked)
                let blockEnd = machine.OutLen
                moveTo &machine this.Mark
                match machine.Successful with
                | false -> false
                | true -> this.MarkedBlock(&machine, entry, start, count, blockEnd)

        /// <summary>The insertion at the current glyph.</summary>
        member this.Current (machine: byref<Machine>, entry: 'E, start: int) =
            let asked = this.Decisions.CurrentCount entry
            materialize &machine
            insertionReserve &machine
            chargeOps &machine asked
            machine.Passive <- false
            match machine.MaxOps <= 0 with
            | true -> ()
            | false ->
                let count = this.Decisions.EffectiveCount(start, asked)
                let blockEnd = machine.OutLen
                let moveEnd = match this.Decisions.DontAdvance entry with | true -> blockEnd | false -> blockEnd + count
                this.Block(&machine, start, count, this.Decisions.CurrentBefore entry, machine.Idx = machine.Len)
                match machine.Successful with
                | false -> ()
                | true -> moveTo &machine moveEnd

        interface IAatActor<'E> with
            member _.Quiet _ = false
            member this.NullIsIdentity entry = not (this.Decisions.SetsMark entry)
            member this.NullAt (machine, entry, index) =
                match this.Decisions.SetsMark entry with
                | true ->
                    let offset = match this.NullMarkBase < 0 with | true -> machine.OutLen - index | false -> this.NullMarkBase
                    this.Mark <- index + offset
                    this.NullMarkBase <- offset
                | false -> ()
            member this.Perform (machine, entry) =
                match machine.Successful with
                | false -> ()
                | true ->
                    this.NullMarkBase <- -1
                    let markLoc = machine.OutLen
                    let proceeds =
                        match this.Decisions.MarkedIndex entry with
                        | 0xFFFFus -> true
                        | index -> this.Marked(&machine, entry, int index)
                    match proceeds with
                    | false -> ()
                    | true ->
                        match this.Decisions.SetsMark entry with
                        | true -> this.Mark <- markLoc
                        | false -> ()
                        match this.Decisions.CurrentIndex entry with
                        | 0xFFFFus -> ()
                        | index -> this.Current(&machine, entry, int index)

    /// <summary>Runs one insertion subtable over a run's arrays: the state-table driver with the transition run straight on the
    /// working copy.</summary>
    /// <typeparam name="'M">The machine's tables, a struct.</typeparam>
    /// <typeparam name="'E">The entry type.</typeparam>
    /// <typeparam name="'D">The insertion decisions, a struct.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The insertion decisions.</param>
    /// <returns>The run fields the machine produces, the glyphs written when collecting, and whether a deleted glyph was written.</returns>
    let runInsertionPass<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> IInsertionDecisions<'E>>
            (input: RunInput) (tables: 'M) (decisions: 'D) =
        let mutable initial = newMachine input Digest.empty false true
        let mutable owned = tables
        let mutable actor = InsertionActor<'D, 'E>(decisions)
        aatLoop &initial &owned &actor
        let unchanged = initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps
        let written =
            match actor.Written with
            | null -> ImmutableArray<uint32>.Empty
            | collected -> ImmutableArray.CreateRange collected
        struct (finishRun &initial unchanged input.Positions, written, actor.HasDeleted)

    /// <summary>The positions of the input glyphs as the machine stands: the working copy once written, else the source.</summary>
    /// <param name="machine">The machine.</param>
    /// <returns>The positions from the first to the last input glyph.</returns>
    let private inputPositions (machine: inref<Machine>) =
        match machine.PositionsMaterialized with
        | true -> ReadOnlySpan<GlyphPosition>(machine.Positions, 0, machine.Len)
        | false -> machine.SourcePositions.AsSpan(0, machine.Len)

    /// <summary>The actor of a kerx format 4 subtable: the transition of <c>KerxSubTableFormat4</c> as straight code over the working
    /// copy, with the marked glyph it keeps.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private Format4Actor<'D, 'E when 'D : struct and 'D :> IFormat4Decisions<'E> and 'E : struct> =
        val mutable Decisions: 'D
        val mutable MarkSet: bool
        val mutable Mark: int
        new (decisions: 'D) = { Decisions = decisions; MarkSet = false; Mark = 0 }

        /// <summary>Remembers the glyph at an index when the entry asks for it, the closing <c>if (entry.flags &amp; Mark)</c>.</summary>
        member this.Remember (entry: 'E, index: int) =
            match this.Decisions.SetsMark entry with
            | true ->
                this.MarkSet <- true
                this.Mark <- index
            | false -> ()

        interface IAatActor<'E> with
            member this.Quiet entry = not (this.Decisions.Performs entry)
            member this.NullIsIdentity entry = not (this.Decisions.SetsMark entry)
            member this.NullAt (_, entry, index) = this.Remember(entry, index)
            member this.Perform (machine, entry) =
                match this.MarkSet && this.Decisions.Performs entry && machine.Idx < machine.Len with
                | false -> this.Remember(entry, machine.Idx)
                | true ->
                    let struct (outcome, moved) = this.Decisions.Resolve(entry, inputInfos &machine, inputPositions &machine, this.Mark, machine.Idx)
                    match outcome with
                    | 0 -> ()
                    | 1 ->
                        materialize &machine
                        chargeOps &machine this.Decisions.ChargeAmount
                        machine.Passive <- false
                    | _ ->
                        let attached = this.Decisions.Attach(inputPositions &machine, machine.Idx, this.Mark, moved)
                        materialize &machine
                        setPosition &machine machine.Idx attached
                        machine.Passive <- false
                        materialize &machine
                        addScratch &machine ScratchFlags.HasGposAttachment
                        machine.Passive <- false
                        this.Remember(entry, machine.Idx)

    /// <summary>Runs one kerx format 4 subtable over a run's arrays: the state-table driver with the transition run straight on the
    /// working copy.</summary>
    /// <typeparam name="'M">The machine's tables, a struct.</typeparam>
    /// <typeparam name="'E">The entry type.</typeparam>
    /// <typeparam name="'D">The format 4 decisions, a struct.</typeparam>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The format 4 decisions.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runFormat4Pass<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> IFormat4Decisions<'E>>
            (input: RunInput) (tables: 'M) (decisions: 'D) =
        let mutable initial = newMachine input Digest.empty false true
        let mutable owned = tables
        let mutable actor = Format4Actor<'D, 'E>(decisions)
        aatLoop &initial &owned &actor
        let unchanged = initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps
        finishRun &initial unchanged input.Positions

    /// <summary>Applies a noncontextual subtable to a run in one loop: the glyphs and properties are copied on the first
    /// replacement and written in place after it, so that a run nothing replaces comes back as it went in with nothing
    /// allocated.</summary>
    /// <typeparam name="'D">The subtable's decisions, a struct.</typeparam>
    /// <param name="decisions">The decisions.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="collect">Whether the glyphs written are collected for the buffer glyph set.</param>
    /// <returns>The glyphs and properties after the pass, the glyphs written when collecting, whether a deleted glyph was
    /// written, and whether any glyph was replaced.</returns>
    let runNoncontextualPass<'D when 'D : struct and 'D :> INoncontextualDecisions>
            (decisions: 'D) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (collect: bool) =
        let source = infos.AsSpan()
        let sourceProps = props.AsSpan()
        let mutable copied = false
        let mutable rewritten = Array.Empty<GlyphInfo>()
        let mutable rewrittenProps = Array.Empty<GlyphProps>()
        let mutable written: List<uint32> | null = null
        let mutable hasDeleted = false
        let mutable range = 0
        let mutable index = 0
        while index < source.Length do
            range <- match decisions.RangeCount > 1 with | true -> decisions.AdvanceRange(range, source[index].Cluster) | false -> range
            let enabled = decisions.RangeCount <= 1 || decisions.RangeEnabled range
            let struct (found, glyph) = match enabled with | true -> decisions.Replacement source[index].Codepoint | false -> struct (false, 0u)
            match found with
            | false -> ()
            | true ->
                match copied with
                | true -> ()
                | false ->
                    rewritten <- GC.AllocateUninitializedArray<GlyphInfo> source.Length
                    rewrittenProps <- GC.AllocateUninitializedArray<GlyphProps> source.Length
                    source.CopyTo(Span<GlyphInfo>(rewritten))
                    sourceProps.CopyTo(Span<GlyphProps>(rewrittenProps))
                    written <- (match collect with | false -> (null: List<uint32> | null) | true -> List<uint32>())
                    copied <- true
                rewritten[index] <- { source[index] with Codepoint = glyph }
                rewrittenProps[index] <- decisions.Props(glyph, sourceProps[index])
                hasDeleted <- hasDeleted || glyph = uint32 DeletedGlyphId
                match written with
                | null -> ()
                | collected -> collected.Add glyph
            index <- index + 1
        match copied with
        | false -> struct (infos, props, ImmutableArray<uint32>.Empty, false, false)
        | true ->
            let collectedGlyphs = match written with | null -> ImmutableArray<uint32>.Empty | collected -> ImmutableArray.CreateRange collected
            struct (publish rewritten, publish rewrittenProps, collectedGlyphs, hasDeleted, true)

    /// <summary>One owned working set of a line through the subtables of an AAT table: the machine of a pass kept from one subtable to
    /// the next, so that the glyphs and properties are copied into arrays of the line's own once (not at all when the caller hands over
    /// arrays it made and shares with no one), every pass writes them in place or through the out-buffer arrays it keeps, and the
    /// glyphs the passes wrote and the candidate bits of the chain being applied are held beside them.</summary>
    [<Sealed>]
    type internal AatSession =
        val mutable Machine: Machine
        val mutable Seen: List<uint32> | null
        val mutable Words: uint64 array
        val mutable Absorbed: int
        new (machine: Machine, seen: List<uint32> | null) = { Machine = machine; Seen = seen; Words = Array.Empty<uint64>(); Absorbed = 0 }

    /// <summary>Opens the working set of a line.</summary>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="adopt">Whether the caller made the arrays of the infos and properties in this call and shares them with no one, so
    /// that the passes may write them in place; honoured only for a run with properties of its length and no positions.</param>
    /// <param name="collect">Whether the glyphs the passes write are collected for the glyph set.</param>
    /// <param name="keepOriginals">Whether the glyphs the line starts with are collected too, for a table of several chains.</param>
    /// <returns>The session.</returns>
    let internal aatSessionOpen (input: RunInput) (adopt: bool) (collect: bool) (keepOriginals: bool) =
        let len = input.Infos.Length
        let mutable machine = newMachine input Digest.empty false true
        match adopt && len > 0 && input.Props.Length = len && (not input.HavePositions) && input.Positions.IsEmpty with
        | false -> ()
        | true ->
            machine.Info <- nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)
            machine.Props <- nonNull (ImmutableCollectionsMarshal.AsArray input.Props)
            machine.Materialized <- true
            machine.Allocated <- growAllocated len 0
        let seen =
            match collect with
            | false -> (null: List<uint32> | null)
            | true ->
                let created = List<uint32>(match keepOriginals with | true -> len | false -> 0)
                match keepOriginals with
                | true ->
                    let infos = input.Infos.AsSpan()
                    for index in 0 .. infos.Length - 1 do
                        created.Add infos[index].Codepoint
                | false -> ()
                created
        AatSession(machine, seen)

    /// <summary>Whether the passes so far are all still successful.</summary>
    /// <param name="session">The session.</param>
    /// <returns>True while the run has not failed.</returns>
    let internal aatSessionSuccessful (session: AatSession) = session.Machine.Successful

    /// <summary>The glyphs of the working set as they stand.</summary>
    /// <param name="session">The session.</param>
    /// <returns>The infos.</returns>
    let internal aatSessionInfos (session: AatSession) =
        inputInfos &session.Machine

    /// <summary>Whether any pass wrote a glyph, a property or a flag of the run.</summary>
    /// <param name="session">The session.</param>
    /// <returns>True when the glyphs or properties were edited.</returns>
    let internal aatSessionEdited (session: AatSession) = not session.Machine.Passive

    /// <summary>Starts a chain: the candidate bits of the glyphs the line has, from the glyphs written and the ones it started with
    /// when an earlier chain wrote some, else from the glyphs as they are.</summary>
    /// <param name="session">The session.</param>
    /// <param name="bits">The chain's bit table.</param>
    /// <param name="top">The glyph count of the table.</param>
    /// <param name="words">The words per glyph.</param>
    /// <param name="fromSeen">Whether the glyphs collected are the whole set.</param>
    let internal aatSessionStartChain (session: AatSession) (bits: ImmutableArray<uint64>) (top: int) (words: int) (fromSeen: bool) =
        let merged = Array.zeroCreate<uint64> words
        let table = bits.AsSpan()
        match fromSeen with
        | true ->
            match session.Seen with
            | null -> ()
            | collected ->
                for index in 0 .. collected.Count - 1 do
                    let glyph = collected[index]
                    match glyph < uint32 top with
                    | true ->
                        for word in 0 .. words - 1 do
                            merged[word] <- merged[word] ||| table[int glyph * words + word]
                    | false -> ()
        | false ->
            let infos = inputInfos &session.Machine
            for index in 0 .. infos.Length - 1 do
                let glyph = infos[index].Codepoint
                match glyph < uint32 top with
                | true ->
                    for word in 0 .. words - 1 do
                        merged[word] <- merged[word] ||| table[int glyph * words + word]
                | false -> ()
        session.Words <- merged
        session.Absorbed <- (match session.Seen with | null -> 0 | collected -> collected.Count)

    /// <summary>The candidate bits of a chain over the glyphs a line has, before any working set exists: the OR of the bit rows of the
    /// glyphs below <paramref name="top"/>.</summary>
    /// <param name="bits">The chain's bit table.</param>
    /// <param name="top">The glyph count of the table.</param>
    /// <param name="words">The words per glyph.</param>
    /// <param name="infos">The glyphs of the line.</param>
    /// <returns>The words, which the caller owns.</returns>
    let internal aatWordsOf (bits: ImmutableArray<uint64>) (top: int) (words: int) (infos: ReadOnlySpan<GlyphInfo>) =
        let merged = Array.zeroCreate<uint64> words
        let table = bits.AsSpan()
        for index in 0 .. infos.Length - 1 do
            let glyph = infos[index].Codepoint
            match glyph < uint32 top with
            | true ->
                for word in 0 .. words - 1 do
                    merged[word] <- merged[word] ||| table[int glyph * words + word]
            | false -> ()
        merged

    /// <summary>Gives a session opened after a chain started the candidate bits that chain computed from the line's glyphs.</summary>
    /// <param name="session">The session, just opened.</param>
    /// <param name="words">The chain's candidate bits, which the session now owns.</param>
    let internal aatSessionAdopt (session: AatSession) (words: uint64 array) =
        session.Words <- words
        session.Absorbed <- (match session.Seen with | null -> 0 | collected -> collected.Count)

    /// <summary>Adds the bits of the glyphs the passes wrote since the last call to the chain's candidate bits.</summary>
    /// <param name="session">The session.</param>
    /// <param name="bits">The chain's bit table.</param>
    /// <param name="top">The glyph count of the table.</param>
    /// <param name="words">The words per glyph.</param>
    let internal aatSessionAbsorb (session: AatSession) (bits: ImmutableArray<uint64>) (top: int) (words: int) =
        match session.Seen with
        | null -> ()
        | collected ->
            let table = bits.AsSpan()
            for index in session.Absorbed .. collected.Count - 1 do
                let glyph = collected[index]
                match glyph < uint32 top with
                | true ->
                    for word in 0 .. words - 1 do
                        session.Words[word] <- session.Words[word] ||| table[int glyph * words + word]
                | false -> ()
            session.Absorbed <- collected.Count

    /// <summary>Whether the chain's candidate bits hold a subtable.</summary>
    /// <param name="session">The session.</param>
    /// <param name="index">The subtable's index in its chain.</param>
    /// <returns>True when a glyph of the line can start the subtable.</returns>
    let internal aatSessionCandidate (session: AatSession) (index: int) =
        (session.Words[index / 64] >>> (index % 64)) &&& 1UL <> 0UL

    /// <summary>Charges one buffer pass against the budget, <c>charge_buffer_pass</c>.</summary>
    /// <param name="session">The session.</param>
    /// <returns>True when the budget held.</returns>
    let internal aatSessionCharge (session: AatSession) =
        let maxOps = session.Machine.MaxOps - session.Machine.Len
        session.Machine.MaxOps <- maxOps
        match maxOps < 0 with
        | true ->
            session.Machine.Successful <- false
            false
        | false -> true

    /// <summary>Reverses the glyphs and their properties, <c>hb_buffer_reverse</c>.</summary>
    /// <param name="session">The session.</param>
    let internal aatSessionReverse (session: AatSession) =
        match session.Machine.Len < 2 with
        | true -> ()
        | false ->
            touch &session.Machine
            Array.Reverse(session.Machine.Info, 0, session.Machine.Len)
            Array.Reverse(session.Machine.Props, 0, session.Machine.Len)

    /// <summary>Marks every glyph unsafe to concat when the run asks for the flag.</summary>
    /// <param name="session">The session.</param>
    let internal aatSessionMarkConcat (session: AatSession) =
        match session.Machine.Flags &&& BufferFlags.ProduceUnsafeToConcat = BufferFlags.Default with
        | true -> ()
        | false ->
            touch &session.Machine
            for index in 0 .. session.Machine.Len - 1 do
                session.Machine.Info[index] <- { session.Machine.Info[index] with Mask = session.Machine.Info[index].Mask ||| GlyphFlagUnsafeToConcat }

    /// <summary>Adds scratch flags to the run.</summary>
    /// <param name="session">The session.</param>
    /// <param name="scratch">The flags.</param>
    let internal aatSessionAddScratch (session: AatSession) (scratch: ScratchFlags) =
        addScratch &session.Machine scratch

    /// <summary>Adds glyphs a pass wrote to the session's collection of written glyphs.</summary>
    /// <param name="session">The session.</param>
    /// <param name="glyphs">The glyphs.</param>
    let internal aatSessionAppend (session: AatSession) (glyphs: ImmutableArray<uint32>) =
        match session.Seen with
        | null -> ()
        | collected ->
            for index in 0 .. glyphs.Length - 1 do
                collected.Add glyphs[index]

    /// <summary>Runs one state-table subtable whose transitions are edits over the working set and returns the finished context.</summary>
    /// <param name="session">The session.</param>
    /// <param name="decisions">The machine's decisions.</param>
    /// <param name="seed">The initial context.</param>
    /// <returns>The finished context.</returns>
    let internal aatSessionEdit<'M, 'E, 'C when 'M : struct and 'M :> IAatMachine<'E, 'C> and 'E : struct and 'C : struct>
            (session: AatSession) (decisions: 'M) (seed: 'C) =
        session.Machine.Idx <- 0
        let mutable tables = decisions
        let mutable actor = EditActor<'M, 'E, 'C>(decisions, seed)
        aatLoop &session.Machine &tables &actor
        actor.Context

    /// <summary>Runs one contextual substitution subtable over the working set, the glyphs written going to the session's collection.</summary>
    /// <param name="session">The session.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The contextual decisions.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal aatSessionContextual<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> IContextualDecisions<'E>>
            (session: AatSession) (tables: 'M) (decisions: 'D) =
        session.Machine.Idx <- 0
        let mutable owned = tables
        let mutable actor = ContextualActor<'D, 'E>(decisions, session.Seen)
        aatLoop &session.Machine &owned &actor
        actor.HasDeleted

    /// <summary>Runs one ligature subtable over the working set, the glyphs written going to the session's collection.</summary>
    /// <param name="session">The session.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The ligature decisions.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal aatSessionLigature<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> ILigatureDecisions<'E>>
            (session: AatSession) (tables: 'M) (decisions: 'D) =
        session.Machine.Idx <- 0
        let mutable owned = tables
        let mutable actor = LigatureActor<'D, 'E>(decisions, session.Seen)
        aatLoop &session.Machine &owned &actor
        actor.HasDeleted

    /// <summary>Runs one rearrangement subtable over the working set.</summary>
    /// <param name="session">The session.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The rearrangement decisions.</param>
    let internal aatSessionRearrangement<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> IRearrangementDecisions<'E>>
            (session: AatSession) (tables: 'M) (decisions: 'D) =
        session.Machine.Idx <- 0
        let mutable owned = tables
        let mutable actor = RearrangementActor<'D, 'E>(decisions)
        aatLoop &session.Machine &owned &actor

    /// <summary>Runs one insertion subtable over the working set, the glyphs written going to the session's collection. The out-buffer
    /// arrays the session keeps are used again by the next subtable that writes through them.</summary>
    /// <param name="session">The session.</param>
    /// <param name="tables">The machine's tables.</param>
    /// <param name="decisions">The insertion decisions.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal aatSessionInsertion<'M, 'E, 'D when 'M : struct and 'M :> IAatTables<'E> and 'E : struct and 'D : struct and 'D :> IInsertionDecisions<'E>>
            (session: AatSession) (tables: 'M) (decisions: 'D) =
        session.Machine.Idx <- 0
        let mutable owned = tables
        let mutable actor = InsertionActor<'D, 'E>(decisions, session.Seen)
        aatLoop &session.Machine &owned &actor
        actor.HasDeleted

    /// <summary>Applies a noncontextual subtable to the working set in one loop, the replacements written in place and the glyphs
    /// written going to the session's collection.</summary>
    /// <param name="session">The session.</param>
    /// <param name="decisions">The subtable's decisions.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal aatSessionNoncontextual<'D when 'D : struct and 'D :> INoncontextualDecisions> (session: AatSession) (decisions: 'D) =
        let mutable hasDeleted = false
        let mutable range = 0
        let mutable index = 0
        while index < session.Machine.Len do
            let info = (inputInfos &session.Machine)[index]
            range <- match decisions.RangeCount > 1 with | true -> decisions.AdvanceRange(range, info.Cluster) | false -> range
            let enabled = decisions.RangeCount <= 1 || decisions.RangeEnabled range
            let struct (found, glyph) = match enabled with | true -> decisions.Replacement info.Codepoint | false -> struct (false, 0u)
            match found with
            | false -> ()
            | true ->
                let props = decisions.Props(glyph, (inputProps &session.Machine)[index])
                touch &session.Machine
                setInputGlyph &session.Machine index glyph props
                hasDeleted <- hasDeleted || glyph = uint32 DeletedGlyphId
                match session.Seen with
                | null -> ()
                | collected -> collected.Add glyph
            index <- index + 1
        hasDeleted

    /// <summary>Closes the working set into the run fields the passes produced.</summary>
    /// <param name="session">The session.</param>
    /// <param name="inputPositions">The positions the run had.</param>
    /// <returns>The run fields, marked unchanged when no pass touched the run.</returns>
    let internal aatSessionClose (session: AatSession) (inputPositions: ImmutableArray<GlyphPosition>) =
        let unchanged = session.Machine.Passive && session.Machine.Successful && session.Machine.MaxOps = session.Machine.InitialMaxOps
        finishRun &session.Machine unchanged inputPositions

    /// <summary>Builds the bit table of a chain: for each glyph below <paramref name="top"/>, the bits of the subtables whose
    /// initial glyph set holds it, <paramref name="words"/> words per glyph, the subtable <c>k</c> being bit <c>k % 64</c> of
    /// word <c>k / 64</c>.</summary>
    /// <param name="sets">The initial glyph set of each subtable.</param>
    /// <param name="words">The words per glyph.</param>
    /// <param name="top">One past the highest glyph any set holds.</param>
    /// <returns>The table.</returns>
    let subtableBits (sets: ImmutableArray<ImmutableArray<CodepointRange>>) (words: int) (top: int) =
        let bits = Array.zeroCreate<uint64> (top * words)
        for subtable in 0 .. sets.Length - 1 do
            let ranges = sets[subtable].AsSpan()
            for range in 0 .. ranges.Length - 1 do
                let mutable glyph = int ranges[range].First
                let last = int (min ranges[range].Last (uint32 top - 1u))
                while glyph <= last do
                    let at = glyph * words + subtable / 64
                    bits[at] <- bits[at] ||| (1UL <<< (subtable % 64))
                    glyph <- glyph + 1
        publish bits

    /// <summary>ORs the bits of glyphs into a copy of some words.</summary>
    /// <param name="existing">The words so far.</param>
    /// <param name="bits">The chain's bit table.</param>
    /// <param name="top">The glyph count of the table.</param>
    /// <param name="infos">The glyphs.</param>
    /// <returns>The words with the bits of every glyph in.</returns>
    let candidateBits (existing: ImmutableArray<uint64>) (bits: ImmutableArray<uint64>) (top: int) (infos: ReadOnlySpan<GlyphInfo>) =
        let words = existing.Length
        let merged = existing.AsSpan().ToArray()
        let table = bits.AsSpan()
        for index in 0 .. infos.Length - 1 do
            let glyph = infos[index].Codepoint
            match glyph < uint32 top with
            | true ->
                for word in 0 .. words - 1 do
                    merged[word] <- merged[word] ||| table[int glyph * words + word]
            | false -> ()
        publish merged

    /// <summary>ORs the bits of added glyphs into a copy of some words.</summary>
    /// <param name="existing">The words so far.</param>
    /// <param name="bits">The chain's bit table.</param>
    /// <param name="top">The glyph count of the table.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The words with the bits of every glyph in.</returns>
    let candidateBitsOfGlyphs (existing: ImmutableArray<uint64>) (bits: ImmutableArray<uint64>) (top: int) (glyphs: ReadOnlySpan<uint32>) =
        let words = existing.Length
        let merged = existing.AsSpan().ToArray()
        let table = bits.AsSpan()
        for index in 0 .. glyphs.Length - 1 do
            let glyph = glyphs[index]
            match glyph < uint32 top with
            | true ->
                for word in 0 .. words - 1 do
                    merged[word] <- merged[word] ||| table[int glyph * words + word]
            | false -> ()
        publish merged

    /// <summary>Appends glyphs to the ones already recorded.</summary>
    /// <param name="existing">The glyphs so far.</param>
    /// <param name="added">The glyphs to append.</param>
    /// <returns>The glyphs, the existing ones first.</returns>
    let appendGlyphs (existing: ImmutableArray<uint32>) (added: ImmutableArray<uint32>) =
        let merged = GC.AllocateUninitializedArray<uint32> (existing.Length + added.Length)
        existing.AsSpan().CopyTo(Span<uint32>(merged))
        added.AsSpan().CopyTo(Span<uint32>(merged, existing.Length, added.Length))
        publish merged

    /// <summary>The bit of a pair-index slot that says it is occupied.</summary>
    [<Literal>]
    let private PairOccupiedBit = 0x1_0000_0000_0000UL

    /// <summary>The multiplier of the pair-key hash, Knuth's golden ratio constant.</summary>
    [<Literal>]
    let KernPairHash = 2654435761u

    /// <summary>The fewest slots a pair index has.</summary>
    [<Literal>]
    let private PairMinSlots = 16

    /// <summary>Whether pair keys strictly increase, the order a binary search over the pairs relies on.</summary>
    /// <param name="keys">The keys, left glyph in the high half.</param>
    /// <param name="index">The next key.</param>
    /// <returns>True when every key exceeds the one before.</returns>
    [<TailCall>]
    let rec private pairKeysIncrease (keys: ReadOnlySpan<uint32>) (index: int) =
        match index >= keys.Length with
        | true -> true
        | false ->
            match keys[index] > keys[index - 1] with
            | true -> pairKeysIncrease keys (index + 1)
            | false -> false

    /// <summary>Searches a table of pair keys for a key as <c>hb_bsearch</c> does, the middle of the candidates first.</summary>
    /// <param name="keys">The keys.</param>
    /// <param name="key">The key.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The index the search ends on, or -1 when it finds no equal key.</returns>
    [<TailCall>]
    let rec private searchPairKey (keys: ReadOnlySpan<uint32>) (key: uint32) (lo: int) (hi: int) =
        match lo > hi with
        | true -> -1
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match keys[mid] > key, keys[mid] < key with
            | true, _ -> searchPairKey keys key lo (mid - 1)
            | false, true -> searchPairKey keys key (mid + 1) hi
            | false, false -> mid

    /// <summary>Compiles the pairs of a table that is not strictly increasing into an index that answers as the binary search over
    /// the table does: each key the table holds is stored with the value of the entry the search for it ends on, a key it holds but
    /// the search cannot reach is left out, and any other key is not found by the search either, so a lookup in the index equals the
    /// search for every key.</summary>
    /// <param name="keys">The keys, the left glyph in the high half and the right glyph in the low half.</param>
    /// <param name="values">The values.</param>
    /// <returns>The index.</returns>
    let pairIndexOfSearched (keys: ReadOnlySpan<uint32>) (values: ReadOnlySpan<int16>) =
        let count = keys.Length
        let size = max PairMinSlots (int (BitOperations.RoundUpToPowerOf2 (uint32 count * 2u)))
        let shift = 32 - BitOperations.Log2 (uint32 size)
        let slots = Array.zeroCreate<uint64> size
        let mutable top = 0u
        for index in 0 .. count - 1 do
            top <- max top (keys[index] >>> 16)
        let lefts = Array.zeroCreate<uint64> (int (top >>> 6) + 1)
        for index in 0 .. count - 1 do
            let key = keys[index]
            let found = searchPairKey keys key 0 (count - 1)
            match found < 0 with
            | true -> ()
            | false ->
                let mutable at = int ((key * KernPairHash) >>> shift)
                while slots[at] <> 0UL && uint32 (slots[at] >>> 16) <> key do
                    at <- (at + 1) &&& (size - 1)
                match slots[at] with
                | 0UL ->
                    slots[at] <- PairOccupiedBit ||| (uint64 key <<< 16) ||| uint64 (uint16 values[found])
                    let left = key >>> 16
                    lefts[int (left >>> 6)] <- lefts[int (left >>> 6)] ||| (1UL <<< int (left &&& 63u))
                | _ -> ()
        ValueSome { Lefts = publish lefts; Slots = publish slots; ShiftBits = shift }

    /// <summary>Compiles the pairs of a sorted table into an index, or declines when the keys are not strictly increasing: only
    /// then does the pair a binary search finds equal the pair a hash finds.</summary>
    /// <param name="keys">The keys, the left glyph in the high half and the right glyph in the low half.</param>
    /// <param name="values">The values.</param>
    /// <returns>The index, or none.</returns>
    let pairIndexOf (keys: ReadOnlySpan<uint32>) (values: ReadOnlySpan<int16>) =
        match pairKeysIncrease keys 1 with
        | false -> ValueNone
        | true ->
            let count = keys.Length
            let size = max PairMinSlots (int (BitOperations.RoundUpToPowerOf2 (uint32 count * 2u)))
            let shift = 32 - BitOperations.Log2 (uint32 size)
            let slots = Array.zeroCreate<uint64> size
            let top = match count with | 0 -> 0u | _ -> keys[count - 1] >>> 16
            let lefts = Array.zeroCreate<uint64> (int (top >>> 6) + 1)
            for index in 0 .. count - 1 do
                let key = keys[index]
                let mutable at = int ((key * KernPairHash) >>> shift)
                while slots[at] <> 0UL do
                    at <- (at + 1) &&& (size - 1)
                slots[at] <- PairOccupiedBit ||| (uint64 key <<< 16) ||| uint64 (uint16 values[index])
                let left = key >>> 16
                lefts[int (left >>> 6)] <- lefts[int (left >>> 6)] ||| (1UL <<< int (left &&& 63u))
            ValueSome { Lefts = publish lefts; Slots = publish slots; ShiftBits = shift }

    /// <summary>Merges sorted single glyphs into sorted disjoint ranges, adjacent runs merged, ignoring the glyph that
    /// <c>hb_set_t</c> reserves as its invalid value.</summary>
    /// <param name="sorted">The glyphs, ascending.</param>
    /// <param name="count">How many of them are valid.</param>
    /// <returns>The ranges.</returns>
    let private glyphRunsOf (sorted: uint32 array) (count: int) =
        let merged = Array.zeroCreate<CodepointRange> count
        let mutable length = 0
        for index in 0 .. count - 1 do
            let glyph = sorted[index]
            match length > 0 && uint64 glyph <= uint64 merged[length - 1].Last + 1UL with
            | true -> merged[length - 1] <- { First = merged[length - 1].First; Last = max merged[length - 1].Last glyph }
            | false ->
                merged[length] <- { First = glyph; Last = glyph }
                length <- length + 1
        publish (Array.sub merged 0 length)

    /// <summary>The ranges the glyphs of a run make, the set <c>setup_buffer_glyph_set</c> builds: the glyphs sorted once and
    /// merged, with no per-glyph operation record.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let glyphRanges (infos: ReadOnlySpan<GlyphInfo>) =
        let keys = GC.AllocateUninitializedArray<uint32> infos.Length
        let mutable count = 0
        for index in 0 .. infos.Length - 1 do
            match infos[index].Codepoint <> UInt32.MaxValue with
            | true ->
                keys[count] <- infos[index].Codepoint
                count <- count + 1
            | false -> ()
        Array.Sort(keys, 0, count)
        glyphRunsOf keys count

    /// <summary>The ranges of a set and some glyphs added to it, sorted, disjoint and merged: <c>hb_set_t::add</c> for each
    /// glyph, by sorting once.</summary>
    /// <param name="existing">The ranges of the set.</param>
    /// <param name="added">The glyphs to add.</param>
    /// <returns>The ranges after the additions.</returns>
    let rangesWithGlyphs (existing: ReadOnlySpan<CodepointRange>) (added: ReadOnlySpan<uint32>) =
        let found = Array.zeroCreate<CodepointRange> (existing.Length + added.Length)
        let mutable count = 0
        for index in 0 .. existing.Length - 1 do
            found[count] <- existing[index]
            count <- count + 1
        for index in 0 .. added.Length - 1 do
            match added[index] <> UInt32.MaxValue with
            | true ->
                found[count] <- { First = added[index]; Last = added[index] }
                count <- count + 1
            | false -> ()
        let keys = GC.AllocateUninitializedArray<uint32> count
        for index in 0 .. count - 1 do
            keys[index] <- found[index].First
        Array.Sort(keys, found, 0, count)
        let merged = Array.zeroCreate<CodepointRange> count
        let mutable length = 0
        for index in 0 .. count - 1 do
            let next = found[index]
            match length > 0 && uint64 next.First <= uint64 merged[length - 1].Last + 1UL with
            | true -> merged[length - 1] <- { First = merged[length - 1].First; Last = max merged[length - 1].Last next.Last }
            | false ->
                merged[length] <- next
                length <- length + 1
        publish (Array.sub merged 0 length)

    /// <summary>Finds the end of a simple run, one before the first mark, <c>hb-ot-shape-normalize.cc:333-338</c>.</summary>
    [<TailCall>]
    let rec private leanSimpleEnd<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (props: GlyphProps array) (length: int) (index: int) =
        match index >= length with
        | true -> index
        | false ->
            match decisions.IsMark (props[index]) with
            | true -> index - 1
            | false -> leanSimpleEnd decisions props length (index + 1)

    /// <summary>Finds the end of a mark run, the first non-mark, <c>hb-ot-shape-normalize.cc:359-361</c>.</summary>
    [<TailCall>]
    let rec private leanMarksEnd<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (props: GlyphProps array) (length: int) (index: int) =
        match index >= length with
        | true -> index
        | false ->
            match decisions.IsMark (props[index]) with
            | false -> index
            | true -> leanMarksEnd decisions props length (index + 1)

    /// <summary>Whether a mark sits at or after an index.</summary>
    [<TailCall>]
    let rec private leanHasMark<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (props: ReadOnlySpan<GlyphProps>) (index: int) =
        match index >= props.Length with
        | true -> false
        | false ->
            match decisions.IsMark (props[index]) with
            | true -> true
            | false -> leanHasMark decisions props (index + 1)

    /// <summary>Finds the end of a run of non-zero combining classes, <c>hb-ot-shape-normalize.cc:384-386</c>.</summary>
    [<TailCall>]
    let rec private leanClassEnd<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (props: GlyphProps array) (length: int) (index: int) =
        match index >= length with
        | true -> index
        | false ->
            match decisions.Mcc (props[index]) = 0uy with
            | true -> index
            | false -> leanClassEnd decisions props length (index + 1)

    /// <summary>Copies the current glyph from the input to the output and advances the cursor, <see cref="nextGlyphs"/> for one glyph with
    /// the two stores instead of the two array copies it makes when the output is separate from the input.</summary>
    /// <param name="machine">The machine.</param>
    let private leanNextOne (machine: byref<Machine>) =
        match machine.HaveOutput && (machine.OutIsSeparate || machine.OutLen <> machine.Idx) with
        | false -> nextGlyphs &machine 1
        | true ->
            match ensure &machine (machine.OutLen + 1) with
            | false -> ()
            | true ->
                let outInfo = outInfoArray &machine
                let outProps = outPropsArray &machine
                outInfo[machine.OutLen] <- machine.Info[machine.Idx]
                outProps[machine.OutLen] <- machine.Props[machine.Idx]
                machine.Idx <- machine.Idx + 1
                machine.OutLen <- machine.OutLen + 1

    /// <summary>Applies the edits of one glyph to the machine, one after the other.</summary>
    let private leanApply (machine: byref<Machine>) (edits: ImmutableArray<RunEdit>) =
        let mutable index = 0
        while index < edits.Length do
            applyEdit &machine edits[index]
            index <- index + 1

    /// <summary>Decomposes the current glyph, which the font does not map as it is: a code point that decomposes into glyphs the font maps
    /// has its parts written to the output and the glyph skipped, as the edits of <c>decompose_current_character</c> do, without building
    /// them; any other glyph, one that takes a space, hyphen or not-found fallback, takes those edits.</summary>
    let private leanDecomposeOrEdit<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (parts: struct (uint32 * uint32) array) (info: GlyphInfo) (props: GlyphProps) (shortest: bool) =
        match decisions.Decompose (info.Codepoint, shortest, parts) with
        | 0 -> leanApply &machine (decisions.Edits (info, props, shortest))
        | count ->
            let mutable index = 0
            while index < count do
                let struct (codepoint, glyph) = parts[index]
                outputInfo &machine { info with Codepoint = codepoint } { props with GlyphIndex = glyph; UnicodeProps = decisions.PartProps codepoint }
                index <- index + 1
            machine.Idx <- machine.Idx + 1

    /// <summary>Decomposes one glyph of a simple run or a short-circuiting mark cluster: a glyph the font maps is two stores and a copy,
    /// a plain miss maps to the not-found glyph, and only a glyph that decomposes or has a fallback builds edits.</summary>
    let private leanShortStep<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (parts: struct (uint32 * uint32) array) (info: GlyphInfo) (props: GlyphProps) =
        match decisions.Glyph (info.Codepoint) with
        | ValueSome glyph ->
            setCurrentProps &machine { props with GlyphIndex = glyph }
            leanNextOne &machine
        | ValueNone ->
            match decisions.PlainNotFound (props, info.Codepoint) with
            | true ->
                setCurrentProps &machine { props with GlyphIndex = decisions.NotFound }
                leanNextOne &machine
            | false ->
                leanDecomposeOrEdit decisions &machine parts info props true
                nextGlyphs &machine 0

    /// <summary>Decomposes one glyph of a mark cluster that does not short-circuit: a code point that has no decomposition and a glyph
    /// is its whole decomposition, any other builds edits.</summary>
    let private leanWholeStep<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (parts: struct (uint32 * uint32) array) (info: GlyphInfo) (props: GlyphProps) =
        match decisions.Undecomposable (info.Codepoint) with
        | false ->
            leanDecomposeOrEdit decisions &machine parts info props false
            nextGlyphs &machine 0
        | true ->
            match decisions.Glyph (info.Codepoint) with
            | ValueSome glyph ->
                setCurrentProps &machine { props with GlyphIndex = glyph }
                leanNextOne &machine
            | ValueNone ->
                leanDecomposeOrEdit decisions &machine parts info props false
                nextGlyphs &machine 0

    /// <summary>Decomposes the current glyph, short-circuiting when <paramref name="shortest"/> says so.</summary>
    let private leanStep<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (parts: struct (uint32 * uint32) array) (shortest: bool) =
        let info = machine.Info[machine.Idx]
        let props = machine.Props[machine.Idx]
        match shortest with
        | true -> leanShortStep decisions &machine parts info props
        | false -> leanWholeStep decisions &machine parts info props

    /// <summary>Whether a number of glyphs can be moved from the input to the output in one step with the very effect of moving them
    /// one at a time: in place, or into room that exists, so that no step would enlarge the arrays or fail on the length limit.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="count">The most glyphs that will be moved.</param>
    /// <returns>True when one move of up to <paramref name="count"/> glyphs equals as many moves of one.</returns>
    let private leanBulkSafe (machine: byref<Machine>) (count: int) =
        match machine.HaveOutput with
        | false -> true
        | true ->
            match machine.OutIsSeparate || machine.OutLen <> machine.Idx with
            | false -> true
            | true -> machine.OutLen + count < machine.Allocated && machine.OutLen + count <= machine.Info.Length

    /// <summary>Decomposes the glyphs of a simple run that the font maps, from the current glyph to the end of the run: each gets its
    /// glyph and the whole stretch moves in one step, as <see cref="leanShortStep"/> does glyph by glyph; a glyph the font does not map
    /// is left to <see cref="leanStep"/>, one at a time.</summary>
    let private leanShortRun<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (parts: struct (uint32 * uint32) array) (boundary: int) =
        let start = machine.Idx
        match leanBulkSafe &machine (boundary - start) with
        | false -> leanStep decisions &machine parts true
        | true ->
            let mutable index = start
            let mutable mapped = true
            while mapped && index < boundary do
                match decisions.Glyph (machine.Info[index].Codepoint) with
                | ValueSome glyph ->
                    machine.Props[index] <- { machine.Props[index] with GlyphIndex = glyph }
                    index <- index + 1
                | ValueNone -> mapped <- false
            match index > start with
            | true -> nextGlyphs &machine (index - start)
            | false -> leanStep decisions &machine parts true

    /// <summary>The most parts one code point decomposes into: Unicode's canonical decompositions have at most four code points, and the
    /// hooks of the shapers split a code point in at most three.</summary>
    [<Literal>]
    let private MaxDecompositionParts = 16

    /// <summary>The decompose round of <c>_hb_ot_shape_normalize</c> without variation selectors, written as one loop over the
    /// machine, from a glyph the glyphs before which are already in the output as the loop leaves them: each mapped in place, the
    /// output being the input up to there.</summary>
    let private leanDecomposeFrom<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (might: bool) (always: bool) (start: int) =
        clearOutput &machine
        machine.Idx <- start
        machine.OutLen <- start
        let parts = Array.zeroCreate<struct (uint32 * uint32)> MaxDecompositionParts
        let inCluster = start > 0 && decisions.IsMark machine.Props[start]
        let baseOfCluster = start + 1 < machine.Len && decisions.IsMark machine.Props[start + 1]
        let mutable multi = inCluster || baseOfCluster
        let mutable boundary =
            match inCluster, baseOfCluster with
            | true, _ -> leanMarksEnd decisions machine.Props machine.Len (start + 1)
            | false, true -> leanMarksEnd decisions machine.Props machine.Len (start + 1)
            | false, false -> leanSimpleEnd decisions machine.Props machine.Len (start + 1)
        let mutable running = true
        while running do
            match machine.Idx < boundary with
            | true ->
                match machine.Successful with
                | false -> running <- false
                | true ->
                    match (match multi with | true -> always | false -> might) with
                    | true -> leanShortRun decisions &machine parts boundary
                    | false -> leanStep decisions &machine parts false
            | false ->
                nextGlyphs &machine 0
                match machine.Idx >= machine.Len || not machine.Successful with
                | true -> running <- false
                | false ->
                    match multi with
                    | false ->
                        nextGlyphs &machine 0
                        boundary <- leanMarksEnd decisions machine.Props machine.Len (machine.Idx + 1)
                        multi <- true
                    | true ->
                        boundary <- leanSimpleEnd decisions machine.Props machine.Len (machine.Idx + 1)
                        multi <- false
        sync &machine

    /// <summary>Sorts a mark run by modified combining class with a stable insertion sort, merging the clusters of every moved
    /// glyph, <c>hb_buffer_t::sort</c> as <see cref="sortClustersByProps"/> does it, on the machine's own arrays.</summary>
    let private leanSort<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (start: int) (finish: int) =
        machine.Successful <- true
        let mutable i = start + 1
        while i < finish do
            let mutable j = i
            while j > start && decisions.Mcc (machine.Props[j - 1]) > decisions.Mcc (machine.Props[i]) do
                j <- j - 1
            match i = j with
            | true -> ()
            | false ->
                mergeClusters &machine j (i + 1)
                let item = machine.Info[i]
                let itemProps = machine.Props[i]
                Array.Copy(machine.Info, j, machine.Info, j + 1, i - j)
                Array.Copy(machine.Props, j, machine.Props, j + 1, i - j)
                machine.Info[j] <- item
                machine.Props[j] <- itemProps
            i <- i + 1

    /// <summary>The reorder round from a glyph: every run of non-zero combining classes of at most <paramref name="longest"/> glyphs is
    /// sorted, then handed to the shaper's hook. The glyphs before <paramref name="first"/> are known to start no run.</summary>
    let private leanReorderFrom<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (longest: int) (first: int) =
        let mutable index = first
        while index < machine.Len do
            match decisions.Mcc (machine.Props[index]) = 0uy with
            | true -> index <- index + 1
            | false ->
                let finish = leanClassEnd decisions machine.Props machine.Len (index + 1)
                match finish - index > longest with
                | true -> index <- finish + 1
                | false ->
                    leanSort decisions &machine index finish
                    leanApply &machine (decisions.Reorder (ReadOnlySpan<GlyphProps>(machine.Props, 0, machine.Len), ReadOnlySpan<GlyphInfo>(machine.Info, 0, machine.Len), index, finish))
                    index <- finish + 1

    /// <summary>Unhides the combining grapheme joiners that prevented no reordering, <c>hb-ot-shape-normalize.cc:403-417</c>.</summary>
    let private leanUnhide<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) =
        let mutable index = 1
        while index + 1 < machine.Len do
            machine.Props[index] <- decisions.Unhide (machine.Info[index].Codepoint, machine.Props[index - 1], machine.Props[index], machine.Props[index + 1])
            index <- index + 1

    /// <summary>Finds the end of a stretch of glyphs that are no mark and have the combining class zero, each a starter.</summary>
    [<TailCall>]
    let rec private leanStarterEnd<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (props: GlyphProps array) (length: int) (index: int) =
        match index >= length with
        | true -> index
        | false ->
            match decisions.IsMark (props[index]) || decisions.Mcc (props[index]) <> 0uy with
            | true -> index
            | false -> leanStarterEnd decisions props length (index + 1)

    /// <summary>Moves a stretch of starters of the compose round to the output: each is a new starter and none composes with
    /// anything before it, so the stretch moves in one step and the last of it is the starter, as the glyph-by-glyph loop leaves it.
    /// When one move would not equal the moves of one glyph each, only the current glyph moves.</summary>
    /// <returns>The output index of the starter after the glyphs moved.</returns>
    let private leanStarters<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) =
        let outLen = machine.OutLen
        let count = leanStarterEnd decisions machine.Props machine.Len (machine.Idx + 1) - machine.Idx
        match leanBulkSafe &machine count with
        | true ->
            nextGlyphs &machine count
            outLen + count - 1
        | false ->
            leanNextOne &machine
            outLen

    /// <summary>One glyph of the compose round that is a mark or has a combining class: it composes with the starter, or becomes the
    /// blocker.</summary>
    /// <returns>The output index of the starter after the glyph.</returns>
    let private leanComposeStep<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (starter: int) =
        let current = machine.Info[machine.Idx]
        let currentProps = machine.Props[machine.Idx]
        let outLen = machine.OutLen
        let blocked = match decisions.Mcc currentProps = 0uy with | true -> outLen | false -> starter
        match decisions.IsMark currentProps with
        | false ->
            leanNextOne &machine
            blocked
        | true ->
            let outInfo = outInfoArray &machine
            let outProps = outPropsArray &machine
            let unblocked = starter = outLen - 1 || decisions.Mcc (outProps[outLen - 1]) < decisions.Mcc currentProps
            match unblocked with
            | false ->
                leanNextOne &machine
                blocked
            | true ->
                match decisions.Compose (outInfo[starter].Codepoint, current.Codepoint) with
                | ValueNone ->
                    leanNextOne &machine
                    blocked
                | ValueSome (struct (composed, glyph)) ->
                    let starterProps = outProps[starter]
                    leanNextOne &machine
                    mergeOutClusters &machine starter (outLen + 1)
                    popOutput &machine
                    let composedProps = { starterProps with GlyphIndex = glyph; UnicodeProps = decisions.ComposedProps composed }
                    setOutputGlyph &machine starter composed composedProps
                    addScratch &machine (decisions.Scratch ({ outInfo[starter] with Codepoint = composed }, composedProps))
                    starter

    /// <summary>The compose round, <c>hb-ot-shape-normalize.cc:419-481</c>, as one loop over the machine.</summary>
    let private leanCompose<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) =
        clearOutput &machine
        leanNextOne &machine
        let mutable starter = 0
        while machine.Idx < machine.Len do
            let props = machine.Props[machine.Idx]
            match decisions.IsMark props || decisions.Mcc props <> 0uy with
            | false -> starter <- leanStarters decisions &machine
            | true -> starter <- leanComposeStep decisions &machine starter
        sync &machine

    /// <summary>Runs the three rounds of <c>_hb_ot_shape_normalize</c> on one machine for a run that holds no variation selector:
    /// decompose, reorder, unhide and compose. The result is what the three passes of the out-buffer machine make one after the
    /// other, without an edit per glyph, a copy of the run per mark run or a publish between the rounds.</summary>
    /// <typeparam name="'D">The decisions type.</typeparam>
    /// <param name="decisions">The decisions of the font, the plan and the shaper.</param>
    /// <param name="input">The run fields.</param>
    /// <param name="might">Whether a glyph the font maps may short-circuit the decomposition of a simple run.</param>
    /// <param name="always">Whether a glyph the font maps always short-circuits the decomposition of a mark cluster.</param>
    /// <param name="composes">Whether the compose round runs.</param>
    /// <param name="longest">The longest mark run that is reordered.</param>
    /// <returns>The run fields the rounds produce.</returns>
    let normalizeRun<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (input: RunInput) (might: bool) (always: bool) (composes: bool) (longest: int) =
        let mutable machine = newMachine input Digest.empty false true
        let allSimple = not (leanHasMark decisions (input.Props.AsSpan()) 1)
        touch &machine
        leanDecomposeFrom decisions &machine might always 0
        match allSimple || not machine.Successful with
        | true -> ()
        | false -> leanReorderFrom decisions &machine longest 0
        match input.ScratchFlags &&& ScratchFlags.HasCgj = ScratchFlags.Default with
        | true -> ()
        | false -> leanUnhide decisions &machine
        match allSimple || not machine.Successful || not composes with
        | true -> ()
        | false -> leanCompose decisions &machine
        finishRun &machine false input.Positions

    /// <summary>Merges the clusters of every group a grouping joins, <c>hb_form_clusters</c>: one loop over the groups with the merge of
    /// <c>merge_grapheme_clusters</c> on the machine's own copy of the glyphs, which is made at the first group of two glyphs.</summary>
    /// <typeparam name="'M">The grouping type.</typeparam>
    /// <param name="grouping">The grouping, true for a glyph that continues the group of the glyph before it.</param>
    /// <param name="input">The run fields.</param>
    /// <returns>The run fields after the merges, marked unchanged when no group has two glyphs.</returns>
    let mergeGroups<'M when 'M : struct and 'M :> IGlyphGrouping> (grouping: 'M) (input: RunInput) =
        let mutable machine = newMachine input Digest.empty false false
        let mutable start = 0
        while start < machine.Len do
            let finish = groupEnd grouping (machine.SourceProps.AsSpan(0, machine.Len)) (start + 1)
            match finish - start < 2 with
            | true -> ()
            | false ->
                touch &machine
                mergeGraphemeClusters &machine start finish
            start <- finish
        finishRun &machine machine.Passive input.Positions

    /// <summary>Sets the mask and the Unicode properties of a run whose every code point is in a table's range in one loop,
    /// <c>hb_set_unicode_props</c> for code points that need none of its continuation rules (no emoji modifier, regional indicator,
    /// tag or half-width mark among them): the properties and the scratch flags are the table's. Two code points are refused: the
    /// zero width joiner, whose rule reads the next code point, and the fraction slash, whose scratch flag its code point sets.</summary>
    /// <param name="table">The properties of the code points from zero to the table's length.</param>
    /// <param name="mask">The mask given to every glyph.</param>
    /// <param name="scratch">The scratch flags the run has.</param>
    /// <param name="refusedFirst">The first code point that is refused.</param>
    /// <param name="refusedSecond">The second code point that is refused.</param>
    /// <param name="source">The glyphs.</param>
    /// <returns>The glyphs with the mask, their properties and the scratch flags with those of every glyph, or none at the first code
    /// point beyond the table or the first one that is refused.</returns>
    let lowProps (table: ImmutableArray<struct (uint16 * ScratchFlags)>) (mask: uint32) (scratch: ScratchFlags) (refusedFirst: uint32) (refusedSecond: uint32) (source: ReadOnlySpan<GlyphInfo>) =
        let limit = uint32 table.Length
        let mutable scanned = 0
        while scanned < source.Length && source[scanned].Codepoint < limit && source[scanned].Codepoint <> refusedFirst && source[scanned].Codepoint <> refusedSecond do
            scanned <- scanned + 1
        match scanned < source.Length with
        | true -> ValueNone
        | false ->
            let infos = GC.AllocateUninitializedArray<GlyphInfo>(source.Length)
            let props = GC.AllocateUninitializedArray<GlyphProps>(source.Length)
            let mutable flags = scratch
            let mutable index = 0
            while index < source.Length do
                let struct (unicode, scratchFlags) = table[int source[index].Codepoint]
                infos[index] <- { source[index] with Mask = mask }
                props[index] <- { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
                flags <- flags ||| scratchFlags
                index <- index + 1
            ValueSome (struct (publish infos, publish props, flags))

    /// <summary>Zeroes the advances of the glyphs whose class is mark, <c>zero_mark_widths_by_gdef</c> without the offset adjustment, in
    /// one copy of the positions.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The positions, as many as the glyphs.</param>
    /// <returns>The positions with the advances of the marks zeroed.</returns>
    let zeroMarkAdvances (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) =
        let target = GC.AllocateUninitializedArray<GlyphPosition>(positions.Length)
        let mutable index = 0
        while index < positions.Length do
            let position = positions[index]
            target[index] <-
                match props[index].GlyphClass &&& GlyphPropsBits.Mark <> 0us with
                | true -> { position with XAdvance = 0; YAdvance = 0 }
                | false -> position
            index <- index + 1
        publish target

    /// <summary>Selects, from values kept lazily with a face, those of the chosen indices: each is forced, and every other index
    /// has the value given for none.</summary>
    /// <param name="values">The lazily built value of every lookup of a table.</param>
    /// <param name="chosen">The indices of the lookups wanted.</param>
    /// <param name="none">The value of a lookup that is not wanted.</param>
    /// <returns>The value of every lookup of the table.</returns>
    let chosenLazyValues<'T> (values: ImmutableArray<Lazy<'T>>) (chosen: ReadOnlySpan<int>) (none: 'T) =
        let flags = Array.zeroCreate<bool> values.Length
        let mutable index = 0
        while index < chosen.Length do
            match chosen[index] < values.Length with
            | true -> flags[chosen[index]] <- true
            | false -> ()
            index <- index + 1
        let result = Array.zeroCreate<'T> values.Length
        index <- 0
        while index < result.Length do
            result[index] <- (match flags[index] with | true -> values[index].Value | false -> none)
            index <- index + 1
        publish result

    /// <summary>The multiplier of the key-table hash, the 64-bit Fibonacci constant.</summary>
    [<Literal>]
    let private KeyTableMultiplier = 0x9E3779B97F4A7C15UL

    /// <summary>The slot a key starts at in a key table.</summary>
    /// <param name="key">The key.</param>
    /// <param name="shift">The shift that keeps the high bits of the product the table's size needs.</param>
    /// <returns>The slot.</returns>
    let keyTableSlot (key: uint64) (shift: int) =
        int ((key * KeyTableMultiplier) >>> shift)

    /// <summary>Stores one key and value in a key table under construction, linear probing from the key's slot.</summary>
    /// <param name="keys">The keys, zero for an empty slot.</param>
    /// <param name="values">The values.</param>
    /// <param name="shift">The shift of <see cref="keyTableSlot"/>.</param>
    /// <param name="key">The key, not zero.</param>
    /// <param name="value">The value.</param>
    /// <param name="slot">The slot to look at.</param>
    [<TailCall>]
    let rec private keyTableStore (keys: uint64 array) (values: uint32 array) shift key value slot =
        match keys[slot] with
        | 0UL ->
            keys[slot] <- key
            values[slot] <- value
        | _ -> keyTableStore keys values shift key value ((slot + 1) &&& (keys.Length - 1))

    /// <summary>Builds an open-addressing table of 64-bit keys, none of which is zero, to 32-bit values, with at least twice as many
    /// slots as keys so that a probe ends at an empty slot after a step or two.</summary>
    /// <param name="keys">The keys.</param>
    /// <param name="values">The values, parallel to the keys.</param>
    /// <returns>The slots' keys, the slots' values and the shift of <see cref="keyTableSlot"/>.</returns>
    let keyTable (keys: ImmutableArray<uint64>) (values: ImmutableArray<uint32>) =
        let bits = max 4 (32 - System.Numerics.BitOperations.LeadingZeroCount (uint32 (keys.Length * 2 - 1)))
        let slotKeys = Array.zeroCreate<uint64> (1 <<< bits)
        let slotValues = Array.zeroCreate<uint32> (1 <<< bits)
        let shift = 64 - bits
        for index in 0 .. keys.Length - 1 do
            keyTableStore slotKeys slotValues shift keys[index] values[index] (keyTableSlot keys[index] shift)
        struct (publish slotKeys, publish slotValues, shift)

    /// <summary>The set of the second code points below U+10000 of the pairs in two hash tables of pair keys (the first code point in the
    /// high half of a key, the second in the low half), one bit each.</summary>
    /// <param name="first">The slots' keys of the first table, an empty slot being zero.</param>
    /// <param name="second">The slots' keys of the second table.</param>
    /// <returns>1,024 words: the bit of a code point is its low six bits in the word at its other bits.</returns>
    let lowSecondBits (first: ImmutableArray<uint64>) (second: ImmutableArray<uint64>) =
        let words = Array.zeroCreate<uint64> 1024
        for key in Seq.append first second do
            let low = key &&& 0xFFFFFFFFUL
            match key <> 0UL && low < 0x10000UL with
            | true -> words[int (low >>> 6)] <- words[int (low >>> 6)] ||| (1UL <<< int (low &&& 63UL))
            | false -> ()
        publish words

    /// <summary>Builds the machine of a run whose arrays the caller made in this call and has shared with no one: the machine writes
    /// them in place instead of copying them on its first edit.</summary>
    /// <param name="input">The run fields the machine reads, the infos and properties of one length.</param>
    /// <param name="exact">Whether a pass that never grows the run publishes the arrays without a copy.</param>
    /// <returns>The machine.</returns>
    let private ownedMachine (input: RunInput) (exact: bool) =
        let infos = nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)
        let mutable machine = newMachine input Digest.empty false exact
        machine.Info <- infos
        machine.Props <- nonNull (ImmutableCollectionsMarshal.AsArray input.Props)
        machine.Materialized <- true
        machine.Allocated <- (match infos.Length = 0 with | true -> 0 | false -> growAllocated infos.Length 0)
        machine

    /// <summary>Builds the machine of a run whose arrays the caller made in this call and shares with no one, as <see cref="newMachine"/> and the
    /// assignments of the owned runners do: the arrays are the machine's working arrays, the allocation is that of a run of exactly that
    /// many glyphs, and nothing is padded, the properties and positions being of the length of the run or empty.</summary>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="infos">The caller's glyph array, holding the line's glyphs.</param>
    /// <param name="props">The caller's property array, holding the line's properties.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <returns>The machine.</returns>
    let private newOwnedMachine (input: inref<RunInput>) (infos: GlyphInfo array) (props: GlyphProps array) (count: int) =
        { Info = infos
          OutInfo = Array.empty
          Props = props
          OutProps = Array.empty
          Positions = Array.empty
          SourceInfos = input.Infos
          SourceProps = input.Props
          SourcePositions = input.Positions
          Materialized = true
          PositionsMaterialized = false
          OutIsSeparate = false
          Idx = 0
          Len = count
          OutLen = 0
          HaveOutput = false
          HavePositions = input.HavePositions
          MaxOps = input.MaxOps
          Successful = input.Successful
          Passive = true
          MaxLen = input.MaxLen
          ClusterLevel = input.ClusterLevel
          Flags = input.Flags
          Serial = input.Serial
          RandomState = input.RandomState
          ScratchFlags = input.ScratchFlags
          Digest = Digest.empty
          DigestStale = false
          Candidates = UInt64.MaxValue
          LookupPassive = true
          Normalise = false
          Exact = true
          Allocated = (match count = 0 with | true -> 0 | false -> growAllocated count 0)
          Nesting = MaxNestingLevel
          InitialMaxOps = input.MaxOps
          Shifted = false
          Grew = false
          Watch = null }

    /// <summary>Runs the lookups of a GSUB program over the first glyphs of arrays the caller made in this call and has shared with no one,
    /// writing into them: the machine starts with the arrays as its working arrays, so a line no lookup grows is edited where it is and
    /// nothing is copied in or out, and a line a lookup grows past the arrays is in arrays of the machine's own afterwards. The one
    /// runner of the lookups for a run whose arrays fit it exactly and for a line in the arrays of a worker.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads, without glyphs, properties or positions.</param>
    /// <param name="program">The program.</param>
    /// <param name="admission">What a look at the line's glyphs said, or an unknown admission.</param>
    /// <param name="infos">The caller's glyph array, holding the line's glyphs.</param>
    /// <param name="props">The caller's property array, holding the line's properties.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <param name="machine">The machine, made and run here.</param>
    let private gsubOwnedMachine<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (input: inref<RunInput>) (program: byref<'P>) (admission: inref<RunAdmission>) (infos: GlyphInfo array) (props: GlyphProps array) (count: int) (machine: byref<Machine>) =
        machine <- newOwnedMachine &input infos props count
        match admission.Known && input.Successful with
        | true ->
            machine.Digest <- admission.GlyphDigest
            machine.Candidates <- admission.Candidates
        | false -> lookAtGlyphs &program &machine
        applyLookups &program &machine

    /// <summary>Runs the lookups of a GSUB program over a run as <see cref="runGsubAdmitted"/> does, on arrays the caller made in this call
    /// and has shared with no one, the three arguments passed by reference: see <see cref="runGsubWindowOwned"/>.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads, the infos and properties of one length and owned by the caller.</param>
    /// <param name="program">The program.</param>
    /// <param name="admission">What a look at the run's glyphs said, or an unknown admission.</param>
    /// <returns>The run fields the machine produces, marked unchanged when no lookup touched the run.</returns>
    let runGsubWindowOwnedRef<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (input: inref<RunInput>) (program: inref<'P>) (admission: inref<RunAdmission>) =
        let len = input.Infos.Length
        match len > 0 && input.Successful && not input.HavePositions && input.Positions.IsEmpty && input.Props.Length = len with
        | false -> runGsubAdmitted (input: RunInput) (program: 'P) (admission: RunAdmission)
        | true ->
            let mutable own = program
            let mutable machine = Unchecked.defaultof<Machine>
            gsubOwnedMachine &input &own &admission (nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)) (nonNull (ImmutableCollectionsMarshal.AsArray input.Props)) len &machine
            finishRun &machine (machine.Passive && machine.MaxOps = machine.InitialMaxOps) input.Positions

    /// <summary>Runs the lookups of a GSUB program over a run as <see cref="runGsubAdmitted"/> does, on arrays the caller made in this call
    /// and has shared with no one: the lookups write the glyphs and the properties in place instead of copying them on the first edit, the
    /// arrays are replaced only where a lookup grows the run, and the result is published with one copy at most, when the run got shorter. A
    /// run the caller's arrays do not describe plainly (positions, properties of another length, a failed run) takes the copying route.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads, the infos and properties of one length and owned by the caller.</param>
    /// <param name="program">The program.</param>
    /// <param name="admission">What a look at the run's glyphs said, or an unknown admission.</param>
    /// <returns>The run fields the machine produces, marked unchanged when no lookup touched the run.</returns>
    let runGsubWindowOwned<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (input: RunInput) (program: 'P) (admission: RunAdmission) =
        runGsubWindowOwnedRef &input &program &admission

    /// <summary>Runs the lookups of a GSUB program over a run as <see cref="runGsubWindowOwnedRef"/> does with no admission, and says whether
    /// the input was shifted forward (<c>shift_forward</c>), the one charge of operations that depends on the length of the run after the
    /// cursor: the syllable reuse refuses what it learns from a run that shifted. A run the arrays do not describe plainly gives none.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads, the infos and properties of one length and owned by the caller.</param>
    /// <param name="program">The program.</param>
    /// <param name="watch">Sees the run after each lookup the kernel applies.</param>
    /// <returns>The run fields the machine produces, whether the input was shifted and whether the output outgrew the input, or none.</returns>
    let runGsubWindowWatched<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (input: inref<RunInput>) (program: inref<'P>) (watch: ILookupWatch) =
        let len = input.Infos.Length
        match len > 0 && input.Successful && not input.HavePositions && input.Positions.IsEmpty && input.Props.Length = len with
        | false -> ValueNone
        | true ->
            let mutable own = program
            let mutable machine = newOwnedMachine &input (nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)) (nonNull (ImmutableCollectionsMarshal.AsArray input.Props)) len
            machine.Watch <- watch
            lookAtGlyphs &own &machine
            applyLookups &own &machine
            let result = finishRun &machine (machine.Passive && machine.MaxOps = machine.InitialMaxOps) input.Positions
            ValueSome struct (result, machine.Shifted, machine.Grew)

    /// <summary>The lookup a program hands out by reference for a position, copied.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="program">The program, which keeps its lookups ready.</param>
    /// <param name="index">The lookup's position in the map.</param>
    /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run.</param>
    /// <returns>A copy of the lookup handed out.</returns>
    let heldCopy<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (program: 'P) (index: int) (shown: bool) =
        let mutable own = program
        own.LookupAt(index, shown)

    /// <summary>Merges the clusters of every group a grouping joins as <see cref="mergeGroups"/> does, on arrays the caller made in
    /// this call and has shared with no one, written in place.</summary>
    /// <typeparam name="'M">The grouping type.</typeparam>
    /// <param name="grouping">The grouping, true for a glyph that continues the group of the glyph before it.</param>
    /// <param name="input">The run fields, the infos and properties of one length and owned by the caller.</param>
    /// <returns>The run fields after the merges, marked unchanged when no group has two glyphs.</returns>
    let mergeGroupsOwned<'M when 'M : struct and 'M :> IGlyphGrouping> (grouping: 'M) (input: RunInput) =
        let mutable machine = ownedMachine input true
        let mutable start = 0
        while start < machine.Len do
            let finish = groupEnd grouping (ReadOnlySpan<GlyphProps>(machine.Props, 0, machine.Len)) (start + 1)
            match finish - start < 2 with
            | true -> ()
            | false ->
                touch &machine
                mergeGraphemeClusters &machine start finish
            start <- finish
        finishRun &machine machine.Passive input.Positions

    /// <summary>The rounds after the decompose round, on the machine the decompose round left: reorder, unhide, compose, and the result.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="machine">The machine after the decompose round.</param>
    /// <param name="input">The run fields the machine was made from.</param>
    /// <param name="allSimple">Whether the run has no mark after its first glyph.</param>
    /// <param name="composes">Whether the compose round runs.</param>
    /// <param name="longest">The longest mark run that is reordered.</param>
    /// <returns>The run fields the rounds produce.</returns>
    let private leanLaterRounds<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (machine: byref<Machine>) (input: RunInput) (allSimple: bool) (composes: bool) (longest: int) =
        match allSimple || not machine.Successful with
        | true -> ()
        | false -> leanReorderFrom decisions &machine longest 0
        match input.ScratchFlags &&& ScratchFlags.HasCgj = ScratchFlags.Default with
        | true -> ()
        | false -> leanUnhide decisions &machine
        match allSimple || not machine.Successful || not composes with
        | true -> ()
        | false -> leanCompose decisions &machine
        finishRun &machine false input.Positions

    /// <summary>Stores one part of a decomposition, a code point and its glyph, in the array the parts are collected in.</summary>
    /// <param name="parts">The array.</param>
    /// <param name="at">The index to store to.</param>
    /// <param name="codepoint">The part's code point.</param>
    /// <param name="glyph">The part's glyph.</param>
    let storePart (parts: struct (uint32 * uint32) array) (at: int) (codepoint: uint32) (glyph: uint32) =
        parts[at] <- struct (codepoint, glyph)

    /// <summary>The part of the glyphs a decomposing line has left after its first decomposition that is given room to grow: one in four.</summary>
    [<Literal>]
    let private DecomposeShare = 4

    /// <summary>The glyphs of room a decomposing line is given besides its share.</summary>
    [<Literal>]
    let private DecomposeSlack = 8

    /// <summary>The capacity of the arrays of a line that decomposes from a glyph on: its length, a share of the glyphs from there on and a
    /// few more, so that the decompose round grows nothing for the decompositions a line commonly holds.</summary>
    /// <param name="length">The glyphs of the line.</param>
    /// <param name="stopped">The glyph the first scan stopped at.</param>
    /// <returns>The capacity.</returns>
    let private decomposeCapacity (length: int) (stopped: int) =
        length + (length - stopped) / DecomposeShare + DecomposeSlack

    /// <summary>Moves the glyphs and properties of an owned machine that has not grown to arrays of a capacity, in one allocation each and
    /// never beyond the logical allocation, which alone decides when the length limit is checked: the first growth of a round then finds
    /// the room it needs, where <see cref="growPhysical"/> doubles the four arrays at it.</summary>
    /// <param name="machine">The machine, whose arrays are the caller's and whose output is not separate.</param>
    /// <param name="capacity">The capacity asked for.</param>
    let private reservePhysical (machine: byref<Machine>) (capacity: int) =
        let room = min capacity machine.Allocated
        match room > machine.Info.Length with
        | false -> ()
        | true ->
            let info = Array.zeroCreate room
            let props = Array.zeroCreate room
            Array.Copy(machine.Info, info, machine.Info.Length)
            Array.Copy(machine.Props, props, machine.Props.Length)
            machine.Info <- info
            machine.Props <- props

    /// <summary>The glyph a code point maps to in the flat first round, when mapping it takes no decomposition and no fallback: the
    /// nominal glyph where the round short-circuits, the glyph of a code point the shaper's hook leaves whole where it does not, and
    /// the not-found glyph for a plain miss where it does.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="short">Whether the glyph is in a cluster the round short-circuits.</param>
    /// <param name="codepoint">The code point.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The glyph, or none when the glyph needs edits.</returns>
    let private flatGlyph<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (short: bool) (codepoint: uint32) (props: GlyphProps) =
        match short with
        | true ->
            match decisions.Glyph codepoint with
            | ValueSome glyph -> ValueSome glyph
            | ValueNone ->
                match decisions.PlainNotFound (props, codepoint) with
                | true -> ValueSome decisions.NotFound
                | false -> ValueNone
        | false ->
            match decisions.Undecomposable codepoint with
            | false -> ValueNone
            | true -> decisions.Glyph codepoint

    /// <summary>Maps the glyphs of a stretch of a run in place, for as long as neither a glyph nor the one after it is a mark and the font
    /// maps the glyph: the first round of the normaliser for a starter that short-circuits, as <see cref="flatRound"/> does it glyph by
    /// glyph, with the glyph and its properties read in place.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="infos">The glyphs, owned by the caller.</param>
    /// <param name="props">The properties, owned by the caller, of the glyphs' length.</param>
    /// <param name="first">The first glyph of the stretch.</param>
    /// <param name="length">The number of glyphs.</param>
    /// <param name="scratch">The scratch flags so far, to which those of the glyphs mapped are added.</param>
    /// <returns>The first glyph the stretch did not map.</returns>
    let private flatStretch<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (infos: GlyphInfo array) (props: GlyphProps array) (first: int) (length: int) (scratch: byref<ScratchFlags>) =
        let mutable index = first
        let mutable flags = scratch
        let mutable running = true
        while running && index < length do
            let current = &props[index]
            let unicode = current.UnicodeProps
            match decisions.IsMarkOf unicode || (index + 1 < length && decisions.IsMarkOf props[index + 1].UnicodeProps) with
            | true -> running <- false
            | false ->
                let codepoint = infos[index].Codepoint
                match decisions.Glyph codepoint with
                | ValueNone -> running <- false
                | ValueSome glyph ->
                    flags <- flags ||| decisions.ScratchOf (codepoint, unicode)
                    props[index] <- { current with GlyphIndex = glyph }
                    index <- index + 1
        scratch <- flags
        index

    /// <summary>Resumes the first round's scan after a glyph: the stretch of starters from the glyph is mapped in one loop, and the scan's
    /// carried state is that of the glyph before the next one to map.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="infos">The glyphs, owned by the caller.</param>
    /// <param name="props">The properties, owned by the caller, of the glyphs' length.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="length">The number of glyphs.</param>
    /// <param name="stretches">Whether the stretch may run: the round short-circuits and has not stopped.</param>
    /// <param name="previousMcc">The combining class of the glyph before the next one.</param>
    /// <param name="isMark">Whether the next glyph is a mark.</param>
    /// <param name="scratch">The scratch flags so far.</param>
    /// <returns>The next glyph to map, the combining class of the glyph before it and whether it is a mark.</returns>
    let private flatResume<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (infos: GlyphInfo array) (props: GlyphProps array) (index: int) (length: int) (stretches: bool) (previousMcc: byte) (isMark: bool) (scratch: byref<ScratchFlags>) =
        match stretches with
        | false -> struct (index, previousMcc, isMark)
        | true ->
            let next = flatStretch decisions infos props index length &scratch
            match next > index with
            | false -> struct (index, previousMcc, isMark)
            | true -> struct (next, 0uy, next < length && decisions.IsMark props[next])

    /// <summary>The first round of the normaliser as one scan, in place: the glyph of every code point is written into the properties
    /// as <see cref="leanDecompose"/> writes it, for as long as no code point needs a decomposition or a fallback, and with the glyphs
    /// the scratch flags they add, the first mark run that needs reordering and whether any mark may compose are noted. The scan
    /// writes only what the lean round writes, so stopping at a glyph that needs edits leaves the arrays as that round can take them.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="infos">The glyphs, owned by the caller.</param>
    /// <param name="props">The properties, owned by the caller, of the glyphs' length.</param>
    /// <param name="length">The number of glyphs, not zero.</param>
    /// <param name="might">Whether a glyph the font maps may short-circuit the decomposition of a simple run.</param>
    /// <param name="always">Whether a glyph the font maps always short-circuits the decomposition of a mark cluster.</param>
    /// <returns>The first glyph that was not mapped (the length when all were), the scratch flags of the glyphs mapped, the first glyph of
    /// the first mark run to reorder (the length when there is none) and whether a mark may compose.</returns>
    let private flatRound<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (infos: GlyphInfo array) (props: GlyphProps array) (length: int) (might: bool) (always: bool) =
        let mutable index = 0
        let mutable mapped = true
        let mutable scratch = ScratchFlags.Default
        let mutable reorderFrom = length
        let mutable mayCompose = false
        let mutable previousMcc = 0uy
        let mutable isMark = decisions.IsMark props[0]
        let struct (firstIndex, firstMcc, firstMark) = flatResume decisions infos props 0 length might previousMcc isMark &scratch
        index <- firstIndex
        previousMcc <- firstMcc
        isMark <- firstMark
        while mapped && index < length do
            let current = props[index]
            let info = infos[index]
            let nextMark = index + 1 < length && decisions.IsMark props[index + 1]
            let short = match (index > 0 && isMark) || nextMark with | true -> always | false -> might
            match flatGlyph decisions short info.Codepoint current with
            | ValueNone -> mapped <- false
            | ValueSome glyph ->
                props[index] <- { current with GlyphIndex = glyph }
                scratch <- scratch ||| decisions.Scratch (info, current)
                let mcc = decisions.Mcc current
                match mcc <> 0uy && reorderFrom = length with
                | false -> ()
                | true ->
                    match decisions.ReordersSingleMarks, previousMcc <> 0uy with
                    | true, _ -> reorderFrom <- index
                    | false, true -> reorderFrom <- index - 1
                    | false, false -> ()
                mayCompose <- mayCompose || (isMark && index > 0 && decisions.MightCompose info.Codepoint)
                previousMcc <- mcc
                isMark <- nextMark
                index <- index + 1
            let struct (nextIndex, nextMcc, nextMark) = flatResume decisions infos props index length (might && mapped) previousMcc isMark &scratch
            index <- nextIndex
            previousMcc <- nextMcc
            isMark <- nextMark
        struct (index, scratch, reorderFrom, mayCompose)

    /// <summary>Whether a glyph takes part in the compose round as a mark, a Unicode mark or a glyph with a combining class: only a mark has
    /// a combining class, so the packed properties decide.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>True for a mark.</returns>
    let private composeMark<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (props: byref<GlyphProps>) =
        decisions.IsMarkOf props.UnicodeProps

    /// <summary>Reads a run to see whether its compose round is the composition of isolated pairs: every mark directly follows a glyph that
    /// is no mark, has no mark after it, and has the cluster of that glyph, so that the merge of clusters the round makes for a composed pair
    /// changes no cluster and only charges two operations. Nothing is written.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties, of the glyphs' length.</param>
    /// <param name="length">The number of glyphs.</param>
    /// <returns>The number of marks, or minus one when some mark is first, follows a mark, or has a cluster of its own.</returns>
    let private composeScan<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (infos: GlyphInfo array) (props: GlyphProps array) (length: int) =
        let mutable index = 0
        let mutable marks = 0
        let mutable previousMark = true
        let mutable isolated = true
        while isolated && index < length do
            match composeMark decisions &props[index] with
            | false -> previousMark <- false
            | true ->
                isolated <- not previousMark && infos[index].Cluster = infos[index - 1].Cluster
                previousMark <- true
                marks <- marks + 1
            index <- index + 1
        match isolated with
        | true -> marks
        | false -> -1

    /// <summary>Composes the isolated pairs of a run in place, the compose round of <see cref="leanCompose"/> for a run that
    /// <see cref="composeScan"/> accepted: a starter and the mark after it become the composite where the shaper and the font have one,
    /// the glyphs after them move down, and a pair that does not compose is kept.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="infos">The glyphs, owned by the caller.</param>
    /// <param name="props">The properties, owned by the caller, of the glyphs' length.</param>
    /// <param name="length">The number of glyphs.</param>
    /// <param name="scratch">The scratch flags so far, to which those of the composites are added.</param>
    /// <returns>The number of glyphs left and the number of pairs composed.</returns>
    let private composePairs<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (infos: GlyphInfo array) (props: GlyphProps array) (length: int) (scratch: byref<ScratchFlags>) =
        let mutable flags = scratch
        let mutable read = 0
        let mutable write = 0
        let mutable composed = 0
        while read < length do
            let paired = read + 1 < length && composeMark decisions &props[read + 1]
            match paired with
            | false ->
                infos[write] <- infos[read]
                props[write] <- props[read]
                write <- write + 1
                read <- read + 1
            | true ->
                match decisions.Compose (infos[read].Codepoint, infos[read + 1].Codepoint) with
                | ValueSome (struct (code, glyph)) ->
                    let info = { infos[read] with Codepoint = code }
                    let unicode = { props[read] with GlyphIndex = glyph; UnicodeProps = decisions.ComposedProps code }
                    infos[write] <- info
                    props[write] <- unicode
                    flags <- flags ||| decisions.Scratch (info, unicode)
                    composed <- composed + 1
                    write <- write + 1
                    read <- read + 2
                | ValueNone ->
                    let mark = infos[read + 1]
                    let markProps = props[read + 1]
                    infos[write] <- infos[read]
                    props[write] <- props[read]
                    infos[write + 1] <- mark
                    props[write + 1] <- markProps
                    write <- write + 2
                    read <- read + 2
        scratch <- flags
        struct (write, composed)

    /// <summary>The compose round of a run whose first round left nothing to reorder, in place, when every mark of the run is an isolated pair's:
    /// the run <see cref="leanCompose"/> would make, field for field, without a machine.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="input">The run fields, the infos and properties of one length and owned by the caller.</param>
    /// <param name="flags">The scratch flags of the run and of the glyphs the first round mapped.</param>
    /// <returns>The run fields the round produces with their scratch flags complete, or none when a mark is not an isolated pair's or the operation budget could not pay the clusters' merges.</returns>
    let private composeLine<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (input: RunInput) (flags: ScratchFlags) =
        let infos = nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)
        let props = nonNull (ImmutableCollectionsMarshal.AsArray input.Props)
        let length = infos.Length
        let marks = composeScan decisions infos props length
        let merges = (match isMonotone input.ClusterLevel with | true -> 2 | false -> 0)
        match marks < 0 || input.MaxOps < merges * marks with
        | true -> ValueNone
        | false ->
            let mutable scratch = flags
            let struct (written, composed) = composePairs decisions infos props length &scratch
            ValueSome { Infos = (match written = length with | true -> input.Infos | false -> publishRange infos written)
                        Props = (match written = length with | true -> input.Props | false -> publishRange props written)
                        Positions = ImmutableArray.Empty
                        HavePositions = false
                        MaxOps = input.MaxOps - merges * composed
                        Successful = true
                        Unchanged = false
                        Serial = input.Serial
                        RandomState = input.RandomState
                        ScratchFlags = scratch }

    /// <summary>The reorder and compose rounds that follow a first round which mapped every glyph: a run whose marks are isolated pairs is
    /// composed in place, and any other is read by the machine, which reorders from the first mark run that needs it and composes.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="input">The run fields, the infos and properties of one length and owned by the caller.</param>
    /// <param name="flags">The scratch flags of the run and of the glyphs the first round mapped.</param>
    /// <param name="reorderFrom">The first glyph of the first mark run to reorder, the length when there is none.</param>
    /// <param name="composing">Whether the compose round runs.</param>
    /// <param name="longest">The longest mark run that is reordered.</param>
    /// <returns>The run fields the rounds produce and that their scratch flags are complete.</returns>
    let private flatReorderCompose<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (input: RunInput) (flags: ScratchFlags) (reorderFrom: int) (composing: bool) (longest: int) =
        let isolated =
            match composing && reorderFrom >= input.Infos.Length with
            | true -> composeLine decisions input flags
            | false -> ValueNone
        match isolated with
        | ValueSome composed -> struct (composed, true)
        | ValueNone ->
            let mutable machine = ownedMachine input true
            touch &machine
            machine.HavePositions <- false
            match reorderFrom < input.Infos.Length with
            | true -> leanReorderFrom decisions &machine longest reorderFrom
            | false -> ()
            match composing && machine.Successful with
            | true -> leanCompose decisions &machine
            | false -> ()
            machine.ScratchFlags <- machine.ScratchFlags ||| flags
            struct (finishRun &machine false input.Positions, true)

    /// <summary>Runs the three rounds of the normaliser on arrays the caller made in this call and has shared with no one, as
    /// <see cref="normalizeRunOwned"/> does, but as <c>_hb_ot_shape_normalize</c> is structured: one scan maps the glyphs where they
    /// are, the reorder round starts at the first mark run that needs it and is left out when none does, and the compose round runs only
    /// when a mark may compose. The scan declines a run in which some glyph needs a decomposition or a fallback, one with a combining
    /// grapheme joiner, and one that has already failed; the caller then takes <see cref="normalizeRunOwned"/>. A scan that stops at a
    /// glyph that needs edits hands the rest of the run to the decompose round of the machine, which resumes at that glyph with the
    /// glyphs before it in the output as that round would have left them.</summary>
    /// <typeparam name="'D">The decisions type.</typeparam>
    /// <param name="decisions">The decisions of the font, the plan and the shaper.</param>
    /// <param name="input">The run fields, the infos and properties of one length and owned by the caller.</param>
    /// <param name="might">Whether a glyph the font maps may short-circuit the decomposition of a simple run.</param>
    /// <param name="always">Whether a glyph the font maps always short-circuits the decomposition of a mark cluster.</param>
    /// <param name="composes">Whether the compose round runs.</param>
    /// <param name="longest">The longest mark run that is reordered.</param>
    /// <returns>The run fields the rounds produce and whether their scratch flags are complete (otherwise the caller adds those of the
    /// glyphs of the result), or none when the scan declined. The flags are those of the glyphs of the run before the compose round and
    /// of every composite the round made: a glyph the round composes away is a mark, which adds no flag the properties pass has not set.</returns>
    let normalizeFlatOwned<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (input: RunInput) (might: bool) (always: bool) (composes: bool) (longest: int) =
        let length = input.Infos.Length
        match length = 0 || not input.Successful || input.Props.Length <> length || input.ScratchFlags &&& ScratchFlags.HasCgj <> ScratchFlags.Default with
        | true -> ValueNone
        | false ->
            let struct (stopped, scratch, reorderFrom, mayCompose) = flatRound decisions (nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)) (nonNull (ImmutableCollectionsMarshal.AsArray input.Props)) length might always
            let flags = input.ScratchFlags ||| scratch
            let composing = composes && mayCompose
            match stopped < length, reorderFrom >= length && not composing with
            | true, _ ->
                let mutable machine = ownedMachine input true
                let allSimple = not (leanHasMark decisions (input.Props.AsSpan()) 1)
                touch &machine
                reservePhysical &machine (decomposeCapacity length stopped)
                leanDecomposeFrom decisions &machine might always stopped
                ValueSome (struct (leanLaterRounds decisions &machine input allSimple composes longest, false))
            | false, true ->
                ValueSome (struct ({ Infos = input.Infos
                                     Props = input.Props
                                     Positions = ImmutableArray.Empty
                                     HavePositions = false
                                     MaxOps = input.MaxOps
                                     Successful = true
                                     Unchanged = false
                                     Serial = input.Serial
                                     RandomState = input.RandomState
                                     ScratchFlags = flags }, true))
            | false, false -> ValueSome (flatReorderCompose decisions input flags reorderFrom composing longest)

    /// <summary>The first code point whose answer the face keeps in a page: the code points below it are read from the low table, which
    /// the face's cmap reader holds as <c>Cmap.LowLimit</c>.</summary>
    [<Literal>]
    let private PagedFirst = 0x2100u

    /// <summary>The first code point beyond the pages: the planes of Unicode end at U+10FFFF.</summary>
    [<Literal>]
    let private PagedLimit = 0x110000u

    /// <summary>The block a loop has no page of.</summary>
    [<Literal>]
    let private NoBlock = -1

    /// <summary>Maps the glyphs of a run that has no mark after its first glyph in place, the first round of the normaliser for a run
    /// whose every cluster is simple and short-circuits, with the scratch flags the glyphs add. A plain miss, a code point the font does not
    /// map that neither decomposes nor is a space or the non-breaking hyphen, takes the not-found glyph, as <c>decompose_current_character</c>
    /// gives it; any other miss stops the round.</summary>
    /// <param name="decisions">The decisions.</param>
    /// <param name="infos">The glyphs, owned by the caller.</param>
    /// <param name="props">The properties, owned by the caller, of the glyphs' length.</param>
    /// <param name="length">The number of glyphs.</param>
    /// <returns>The first glyph the round cannot map (the length when it maps all) and the scratch flags of the glyphs before it.</returns>
    let private simpleRound<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (infos: GlyphInfo array) (props: GlyphProps array) (length: int) =
        let mutable index = 0
        let mutable mapped = true
        let mutable scratch = ScratchFlags.Default
        let mutable block = NoBlock
        let mutable page: uint32 array | null = null
        while mapped && index < length do
            let info = infos[index]
            let codepoint = info.Codepoint
            let glyph =
                match codepoint >= PagedFirst && codepoint < PagedLimit with
                | false -> decisions.Glyph codepoint
                | true ->
                    let wanted = int (codepoint >>> 8)
                    match wanted = block with
                    | true -> ()
                    | false ->
                        block <- wanted
                        page <- decisions.Page wanted
                    let entry =
                        match page with
                        | null -> 0u
                        | held -> held[int (codepoint &&& 0xFFu)]
                    match entry with
                    | 0u ->
                        block <- NoBlock
                        decisions.Glyph codepoint
                    | 1u -> ValueNone
                    | kept -> ValueSome (kept - 2u)
            let resolved =
                match glyph with
                | ValueSome _ -> glyph
                | ValueNone ->
                    match decisions.PlainNotFound (props[index], codepoint) with
                    | true -> ValueSome decisions.NotFound
                    | false -> ValueNone
            match resolved with
            | ValueNone -> mapped <- false
            | ValueSome glyph ->
                let current = props[index]
                props[index] <- { current with GlyphIndex = glyph }
                scratch <- scratch ||| decisions.Scratch (info, current)
                index <- index + 1
        struct (index, scratch)

    /// <summary>Runs the normaliser on arrays the caller made in this call and has shared with no one, for a run that has no mark after
    /// its first glyph: the glyphs are written into the properties where they are, a plain miss as the not-found glyph, and nothing else
    /// of the run changes but that its positions are dropped, as the normaliser's first round leaves it. The run is declined when a code
    /// point the font does not map decomposes, is a space or is the non-breaking hyphen, when it has a combining grapheme joiner, or when
    /// it has failed.</summary>
    /// <typeparam name="'D">The decisions type.</typeparam>
    /// <param name="decisions">The decisions of the font and the plan.</param>
    /// <param name="input">The run fields, the infos and properties of one length and owned by the caller.</param>
    /// <returns>The run fields the normaliser produces, or none when the run is declined.</returns>
    let normalizeSimpleOwned<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (input: RunInput) =
        let length = input.Infos.Length
        match length = 0 || not input.Successful || input.Props.Length <> length || input.ScratchFlags &&& ScratchFlags.HasCgj <> ScratchFlags.Default with
        | true -> ValueNone
        | false ->
            let struct (stopped, scratch) = simpleRound decisions (nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)) (nonNull (ImmutableCollectionsMarshal.AsArray input.Props)) length
            match stopped < length with
            | true -> ValueNone
            | false ->
                ValueSome { Infos = input.Infos
                            Props = input.Props
                            Positions = ImmutableArray.Empty
                            HavePositions = false
                            MaxOps = input.MaxOps
                            Successful = true
                            Unchanged = false
                            Serial = input.Serial
                            RandomState = input.RandomState
                            ScratchFlags = input.ScratchFlags ||| scratch }

    /// <summary>Runs the three rounds of the normaliser as <see cref="normalizeRun"/> does, on arrays the caller made in this call
    /// and has shared with no one: the glyphs the normaliser leaves where they are stay in the caller's arrays.</summary>
    /// <typeparam name="'D">The decisions type.</typeparam>
    /// <param name="decisions">The decisions of the font, the plan and the shaper.</param>
    /// <param name="input">The run fields, the infos and properties of one length and owned by the caller.</param>
    /// <param name="might">Whether a glyph the font maps may short-circuit the decomposition of a simple run.</param>
    /// <param name="always">Whether a glyph the font maps always short-circuits the decomposition of a mark cluster.</param>
    /// <param name="composes">Whether the compose round runs.</param>
    /// <param name="longest">The longest mark run that is reordered.</param>
    /// <returns>The run fields the rounds produce.</returns>
    let normalizeRunOwned<'D when 'D : struct and 'D :> INormalizeDecisions> (decisions: 'D) (input: RunInput) (might: bool) (always: bool) (composes: bool) (longest: int) =
        let mutable machine = ownedMachine input true
        let allSimple = not (leanHasMark decisions (input.Props.AsSpan()) 1)
        touch &machine
        leanDecomposeFrom decisions &machine might always 0
        leanLaterRounds decisions &machine input allSimple composes longest

    /// <summary>Replaces every element of an array of one-element groups by its mapping, in place, on an array the caller made in this
    /// call and has shared with no one.</summary>
    /// <typeparam name="'T">The element type.</typeparam>
    /// <typeparam name="'M">The mapper type, whose groups are one element each.</typeparam>
    /// <param name="mapper">The mapper.</param>
    /// <param name="items">The elements.</param>
    let mapOwned<'T, 'M when 'M : struct and 'M :> IGroupMapper<'T>> (mapper: 'M) (items: ImmutableArray<'T>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray items)
        let mutable index = 0
        while index < array.Length do
            array[index] <- mapper.Map(ReadOnlySpan<'T>(array), index, index + 1, index)
            index <- index + 1

    /// <summary>Copies each glyph's index from its properties into its code point, in place, <c>hb_ot_map_glyphs_fast</c>.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties, which carry the glyph indices.</param>
    let mapGlyphsOwned (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let source = props.AsSpan()
        let mutable index = 0
        while index < array.Length do
            array[index] <- { array[index] with Codepoint = source[index].GlyphIndex }
            index <- index + 1

    /// <summary>Whether any glyph carries a Unicode property bit of a mask.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="mask">The bits.</param>
    /// <returns>True when some glyph has a bit of the mask set.</returns>
    let anyUnicodeIn (props: ReadOnlySpan<GlyphProps>) (mask: uint16) =
        let mutable found = false
        let mutable index = 0
        while not found && index < props.Length do
            found <- props[index].UnicodeProps &&& mask <> 0us
            index <- index + 1
        found

    /// <summary>Propagates the glyph flags over each cluster in place, <c>hb_propagate_flags</c>: every glyph takes the flags of its
    /// cluster, cut down to the mask, with the tatweel rule when the buffer asks for it.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="andMask">The mask the propagated masks are cut down to.</param>
    /// <param name="produceTatweel">Whether the buffer produces safe-to-insert-tatweel.</param>
    let propagateFlagsOwned (infos: ImmutableArray<GlyphInfo>) (andMask: uint32) (produceTatweel: bool) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable start = 0
        while start < array.Length do
            let cluster = array[start].Cluster
            let mutable finish = start + 1
            let mutable mask = array[start].Mask
            while finish < array.Length && array[finish].Cluster = cluster do
                mask <- mask ||| array[finish].Mask
                finish <- finish + 1
            let mask =
                match produceTatweel with
                | false -> mask
                | true ->
                    let cleared =
                        match mask &&& GlyphFlagUnsafeToBreak <> 0u with
                        | true -> mask &&& ~~~GlyphFlagSafeToInsertTatweel
                        | false -> mask
                    match cleared &&& GlyphFlagSafeToInsertTatweel <> 0u with
                    | true -> cleared ||| GlyphFlagUnsafeToBreak ||| GlyphFlagUnsafeToConcat
                    | false -> cleared
            let propagated = mask &&& andMask
            let mutable index = start
            while index < finish do
                array[index] <- { array[index] with Mask = propagated }
                index <- index + 1
            start <- finish

    /// <summary>Zeroes the advances of the glyphs whose class is mark, in place, <c>zero_mark_widths_by_gdef</c> without the offset
    /// adjustment.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The positions, made by the caller in this call and shared with no one.</param>
    let zeroMarkAdvancesOwned (props: ImmutableArray<GlyphProps>) (positions: ImmutableArray<GlyphPosition>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray positions)
        let source = props.AsSpan()
        let mutable index = 0
        while index < array.Length do
            match source[index].GlyphClass &&& GlyphPropsBits.Mark <> 0us with
            | true -> array[index] <- { array[index] with XAdvance = 0; YAdvance = 0 }
            | false -> ()
            index <- index + 1

    /// <summary>Zeroes the widths of the glyphs whose class is mark, in place, <c>zero_mark_widths_by_gdef</c>: when the offsets are adjusted
    /// first, each offset loses the advance it is zeroed from, saturating.</summary>
    /// <param name="adjustOffsets">Whether the offsets are adjusted before zeroing.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The positions, made by the caller in this call and shared with no one.</param>
    let zeroMarkWidthsOwned (adjustOffsets: bool) (props: ImmutableArray<GlyphProps>) (positions: ImmutableArray<GlyphPosition>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray positions)
        let source = props.AsSpan()
        let mutable index = 0
        while index < array.Length do
            match source[index].GlyphClass &&& GlyphPropsBits.Mark <> 0us with
            | true ->
                let position = array[index]
                match adjustOffsets with
                | true ->
                    array[index] <-
                        { position with
                            XOffset = int (Math.Clamp(int64 position.XOffset - int64 position.XAdvance, int64 Int32.MinValue, int64 Int32.MaxValue))
                            YOffset = int (Math.Clamp(int64 position.YOffset - int64 position.YAdvance, int64 Int32.MinValue, int64 Int32.MaxValue))
                            XAdvance = 0
                            YAdvance = 0 }
                | false -> array[index] <- { position with XAdvance = 0; YAdvance = 0 }
            | false -> ()
            index <- index + 1

    /// <summary>Zeroes the widths of the default-ignorable glyphs that no lookup substituted and their offset along the main axis, in place,
    /// <c>hb_ot_zero_width_default_ignorables</c> with <c>_hb_glyph_info_is_default_ignorable</c> (src/hb-ot-layout.hh:349-353).</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="ignorable">The Unicode property bit of a default-ignorable code point.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    /// <param name="positions">The positions, made by the caller in this call and shared with no one.</param>
    let zeroIgnorablesOwned (props: ImmutableArray<GlyphProps>) (ignorable: uint16) (horizontal: bool) (positions: ImmutableArray<GlyphPosition>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray positions)
        let source = props.AsSpan()
        let mutable index = 0
        while index < array.Length do
            match source[index].UnicodeProps &&& ignorable <> 0us && source[index].GlyphClass &&& GlyphPropsBits.Substituted = 0us with
            | false -> ()
            | true ->
                let position = array[index]
                array[index] <-
                    { position with
                        XAdvance = 0
                        YAdvance = 0
                        XOffset = (match horizontal with | true -> 0 | false -> position.XOffset)
                        YOffset = (match horizontal with | true -> position.YOffset | false -> 0) }
            index <- index + 1

    /// <summary>Propagates the attachment offsets of a run in place, <see cref="propagateAttachments"/> on positions the caller made in
    /// this call and has shared with no one.</summary>
    /// <param name="positions">The positions.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    let propagateAttachmentsOwned (positions: ImmutableArray<GlyphPosition>) forward horizontal =
        let working = nonNull (ImmutableCollectionsMarshal.AsArray positions)
        for step in 0 .. working.Length - 1 do
            let i = match forward with | true -> step | false -> working.Length - 1 - step
            match positionChain working[i] <> 0 with
            | true -> propagateOffsetsAt working forward horizontal i MaxNestingLevel
            | false -> ()

    /// <summary>Reverses the glyphs, their properties and their positions in place, <c>hb_buffer_reverse</c>.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="positions">The positions.</param>
    let reverseOwned (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (positions: ImmutableArray<GlyphPosition>) =
        Array.Reverse (nonNull (ImmutableCollectionsMarshal.AsArray infos))
        Array.Reverse (nonNull (ImmutableCollectionsMarshal.AsArray props))
        Array.Reverse (nonNull (ImmutableCollectionsMarshal.AsArray positions))

    /// <summary>Runs the lookups of a GPOS program over a run as <see cref="runGposAdmitted"/> does, on infos and positions the caller made
    /// in this call and has shared with no one: the machine writes the positions and the glyph flags into them, without a copy.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields, owned by the caller, with positions of the run's length.</param>
    /// <param name="program">The program.</param>
    /// <returns>The operation budget left, the success flag and the scratch flags.</returns>
    let runGposOwned<'P, 'L when 'P : struct and 'P :> IPosProgram<'L> and 'L : struct and 'L :> IPosLookup> (input: RunInput) (program: 'P) =
        let infos = nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)
        let mutable machine =
            { SourceInfos = input.Infos
              SourceProps = input.Props
              SourcePositions = input.Positions
              Info = infos
              InfosOwned = true
              Positions = nonNull (ImmutableCollectionsMarshal.AsArray input.Positions)
              PositionsOwned = true
              Len = infos.Length
              Idx = 0
              MaxOps = input.MaxOps
              Successful = input.Successful
              Scratch = input.ScratchFlags
              LastBase = -1
              LastBaseUntil = 0
              ClusterLevel = input.ClusterLevel
              Flags = input.Flags }
        match machine.Len > 0 && machine.Successful with
        | false -> ()
        | true -> gposLookLooks program &machine input.Infos
        struct (machine.MaxOps, machine.Successful, machine.Scratch)

    /// <summary>The words of a dense set of glyphs, one bit each, from glyphs in strictly ascending order.</summary>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The words, or none when the glyphs do not strictly ascend, the order a binary search relies on.</returns>
    let denseOfGlyphs (glyphs: ReadOnlySpan<uint32>) =
        let mutable ascending = true
        let mutable index = 1
        while ascending && index < glyphs.Length do
            ascending <- glyphs[index] > glyphs[index - 1]
            index <- index + 1
        match ascending with
        | false -> ValueNone
        | true ->
            let words = Array.zeroCreate<uint64> (match glyphs.Length with | 0 -> 0 | _ -> int (glyphs[glyphs.Length - 1] >>> 6) + 1)
            for glyph in glyphs do
                words[int (glyph >>> 6)] <- words[int (glyph >>> 6)] ||| (1UL <<< int (glyph &&& 63u))
            ValueSome (publish words)

    /// <summary>The words of a dense set of glyphs, one bit each, from ranges in ascending order that do not overlap.</summary>
    /// <param name="ranges">The ranges, each from its first glyph to its last.</param>
    /// <returns>The words, or none when a range is empty or the ranges do not ascend without overlapping, the order a binary search relies on.</returns>
    let denseOfRanges (ranges: ReadOnlySpan<CodepointRange>) =
        let mutable ordered = true
        let mutable index = 0
        while ordered && index < ranges.Length do
            ordered <- ranges[index].First <= ranges[index].Last && (index = 0 || ranges[index].First > ranges[index - 1].Last)
            index <- index + 1
        match ordered with
        | false -> ValueNone
        | true ->
            let words = Array.zeroCreate<uint64> (match ranges.Length with | 0 -> 0 | _ -> int (ranges[ranges.Length - 1].Last >>> 6) + 1)
            for range in ranges do
                let mutable glyph = range.First
                while glyph <= range.Last do
                    words[int (glyph >>> 6)] <- words[int (glyph >>> 6)] ||| (1UL <<< int (glyph &&& 63u))
                    glyph <- glyph + 1u
            ValueSome (publish words)

    /// <summary>Makes an array of a number of elements whose contents are not cleared: the caller writes every element the reader
    /// will see before the array is published, and clears the rest with <see cref="clearWindow"/>.</summary>
    /// <param name="count">The length.</param>
    /// <returns>The array.</returns>
    let uninitialised<'T> (count: int) =
        GC.AllocateUninitializedArray<'T> count

    /// <summary>Publishes an array a document's workers have finished writing, without copying it.</summary>
    /// <param name="items">The array, never written again.</param>
    /// <returns>The immutable view.</returns>
    let publishArray (items: 'T array) =
        ImmutableCollectionsMarshal.AsImmutableArray items

    /// <summary>Decodes the items of a text into the first elements of an array a worker owns, as <see cref="unfoldSpan"/> does into a
    /// fresh one.</summary>
    /// <param name="source">The text.</param>
    /// <param name="target">The array, at least <paramref name="count"/> long.</param>
    /// <param name="count">How many items to decode.</param>
    /// <param name="seed">The initial state.</param>
    /// <param name="folder">The folder.</param>
    let unfoldInto<'E, 'S, 'T, 'F when 'F : struct and 'F :> ISpanUnfolder<'E, 'S, 'T>> (source: ReadOnlySpan<'E>) (target: 'T array) (count: int) (seed: 'S) (folder: 'F) =
        let mutable state = seed
        for index in 0 .. count - 1 do
            let struct (value, next) = folder.Next(source, state)
            target[index] <- value
            state <- next

    /// <summary>Writes the glyph infos of a text whose every byte is one code point equal to its own value into the first elements of an
    /// array a worker owns, as <see cref="byteInfos"/> does into a fresh one.</summary>
    /// <param name="source">The bytes.</param>
    /// <param name="target">The array, at least as long as the text.</param>
    /// <param name="clusterBase">The cluster of the first byte.</param>
    let byteInfosInto (source: ReadOnlySpan<byte>) (target: GlyphInfo array) (clusterBase: int) =
        for index in 0 .. source.Length - 1 do
            target[index] <- { Codepoint = uint32 source[index]; Mask = 0u; Cluster = uint32 (clusterBase + index); Var1 = 0u; Var2 = 0u }

    /// <summary>Shapes a simple run in place: the first <paramref name="count"/> elements of the arrays hold the code points and clusters
    /// of the text, and are overwritten with the glyphs, the mask and the default advances, as <see cref="simpleRun"/> makes them in
    /// fresh arrays, or the run is refused at the first code point that is not simple.</summary>
    /// <param name="glyphs">The per-glyph decision.</param>
    /// <param name="mask">The mask written into every glyph.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="covered">The words of the glyph set whose members the run is checked for.</param>
    /// <param name="infos">The infos, holding the text and then the glyphs.</param>
    /// <param name="props">The properties being written.</param>
    /// <param name="positions">The positions being written.</param>
    /// <param name="count">The glyphs of the run.</param>
    /// <param name="hit">Whether a glyph so far is in the set.</param>
    /// <param name="index">The next index.</param>
    /// <returns>None when a code point does not qualify, else whether some glyph is in the set.</returns>
    [<TailCall>]
    let rec private simpleInPlaceFrom<'G when 'G : struct and 'G :> ISimpleGlyphs>
            (glyphs: 'G)
            (mask: uint32)
            (metrics: ReadOnlySpan<byte>)
            (covered: ReadOnlySpan<uint64>)
            (infos: GlyphInfo array)
            (props: GlyphProps array)
            (positions: GlyphPosition array)
            (count: int)
            (hit: bool)
            (index: int) =
        match index < count with
        | false -> ValueSome hit
        | true ->
            let info = infos[index]
            match glyphs.Glyph(info.Codepoint, metrics) with
            | ValueNone -> ValueNone
            | ValueSome simple ->
                let glyph = simple.SimpleProps.GlyphIndex
                let word = int (glyph >>> 6)
                let hit = hit || (word < covered.Length && (covered[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL)
                infos[index] <- { Codepoint = glyph; Mask = mask; Cluster = info.Cluster; Var1 = info.Var1; Var2 = info.Var2 }
                props[index] <- simple.SimpleProps
                positions[index] <- { XAdvance = simple.SimpleAdvance; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                simpleInPlaceFrom glyphs mask metrics covered infos props positions count hit (index + 1)

    /// <summary>Shapes a simple run in the arrays a worker owns, see <see cref="simpleInPlaceFrom"/>.</summary>
    /// <param name="glyphs">The per-glyph decision.</param>
    /// <param name="mask">The mask written into every glyph.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="coveredSet">The glyph set whose members the run is checked for.</param>
    /// <param name="infos">The infos, holding the text and then the glyphs.</param>
    /// <param name="props">The properties being written.</param>
    /// <param name="positions">The positions being written.</param>
    /// <param name="count">The glyphs of the run.</param>
    /// <returns>None when a code point does not qualify, else whether some glyph is in the set.</returns>
    let simpleInPlace<'G when 'G : struct and 'G :> ISimpleGlyphs> (glyphs: 'G) (mask: uint32) (metrics: ReadOnlySpan<byte>) (coveredSet: ImmutableArray<uint64>) (infos: GlyphInfo array) (props: GlyphProps array) (positions: GlyphPosition array) (count: int) =
        simpleInPlaceFrom glyphs mask metrics (coveredSet.AsSpan()) infos props positions count false 0

    /// <summary>Gives the first glyphs of an array the same mask, in place.</summary>
    /// <param name="infos">The glyphs of a run a worker owns.</param>
    /// <param name="count">The glyphs of the run.</param>
    /// <param name="mask">The mask.</param>
    let setMasksOf (infos: GlyphInfo array) (count: int) (mask: uint32) =
        let mutable index = 0
        while index < count do
            infos[index] <- { infos[index] with Mask = mask }
            index <- index + 1

    /// <summary>Finishes the glyph flags of the first glyphs of an array, see <see cref="finishFlags"/>.</summary>
    /// <param name="infos">The glyphs of a run a worker owns.</param>
    /// <param name="count">The glyphs of the run.</param>
    /// <param name="andMask">The mask the masks are cut down to.</param>
    /// <returns>True when no two neighbouring glyphs share a cluster.</returns>
    let finishFlagsOf (infos: GlyphInfo array) (count: int) (andMask: uint32) =
        let mutable distinct = true
        let mutable index = 0
        while index < count do
            infos[index] <- { infos[index] with Mask = infos[index].Mask &&& andMask }
            distinct <- distinct && (index = 0 || infos[index].Cluster <> infos[index - 1].Cluster)
            index <- index + 1
        distinct

    /// <summary>Applies pair lookups in place to the first glyphs of arrays a worker owns, as <see cref="runPairsOwned"/> does to the
    /// whole of a run's arrays: the arrays may be longer than the run.</summary>
    /// <param name="covered">The glyphs each position of the map starts at.</param>
    /// <param name="tables">The compiled tables of each position of the map.</param>
    /// <param name="multiplier">The font's x multiplier.</param>
    /// <param name="bits">The positions of the lookups to apply.</param>
    /// <param name="infos">The glyphs of the run, written in place.</param>
    /// <param name="positions">The positions of the run, written in place.</param>
    /// <param name="count">The glyphs of the run.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <returns>The operation budget left.</returns>
    let runPairsOf (covered: ImmutableArray<ImmutableArray<uint64>>) (tables: ImmutableArray<ImmutableArray<PairTable>>) (multiplier: int64) (bits: uint64) (infos: GlyphInfo array) (positions: GlyphPosition array) (count: int) (flags: BufferFlags) (level: ClusterLevel) (maxOps: int) =
        let mutable machine =
            { SourceInfos = ImmutableArray<GlyphInfo>.Empty
              SourceProps = ImmutableArray<GlyphProps>.Empty
              SourcePositions = ImmutableArray<GlyphPosition>.Empty
              Info = infos
              InfosOwned = true
              Positions = positions
              PositionsOwned = true
              Len = count
              Idx = 0
              MaxOps = maxOps
              Successful = true
              Scratch = ScratchFlags.Default
              LastBase = -1
              LastBaseUntil = 0
              ClusterLevel = level
              Flags = flags }
        let mutable remaining = bits
        while remaining <> 0UL do
            let position = BitOperations.TrailingZeroCount remaining
            gposLeanForward covered[position] tables[position] multiplier &machine
            remaining <- remaining &&& (remaining - 1UL)
        machine.MaxOps

    /// <summary>Runs the lookups of a GSUB program over a line held in the first elements of arrays a worker owns, as
    /// <see cref="runGsubAdmitted"/> does over a run, but writing into those arrays: a line no lookup grows is edited where it is and
    /// nothing is copied in or out. A line a lookup grows past the arrays is in arrays of the machine's own afterwards, which the result
    /// names.</summary>
    /// <typeparam name="'P">The program type.</typeparam>
    /// <typeparam name="'L">The lookup type.</typeparam>
    /// <param name="input">The run fields the machine reads, without glyphs, properties or positions.</param>
    /// <param name="program">The program.</param>
    /// <param name="admission">What a look at the line's glyphs said, or an unknown admission.</param>
    /// <param name="infos">The worker's glyph array, holding the line's glyphs.</param>
    /// <param name="props">The worker's property array, holding the line's properties.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <returns>Where the glyphs are afterwards and the state of the run.</returns>
    let runGsubOwned<'P, 'L when 'P : struct and 'P :> IGsubProgram<'L> and 'L : struct and 'L :> IGsubLookup> (input: RunInput) (program: 'P) (admission: RunAdmission) (infos: GlyphInfo array) (props: GlyphProps array) (count: int) =
        let mutable program = program
        let mutable machine = Unchecked.defaultof<Machine>
        gsubOwnedMachine &input &program &admission infos props count &machine
        { OwnedInfos = machine.Info
          OwnedProps = machine.Props
          OwnedLength = machine.Len
          OwnedUnchanged = machine.Passive && machine.MaxOps = machine.InitialMaxOps
          OwnedMaxOps = machine.MaxOps
          OwnedSuccessful = machine.Successful
          OwnedSerial = machine.Serial
          OwnedRandom = machine.RandomState
          OwnedScratch = machine.ScratchFlags }

    /// <summary>Copies the first elements of an array into an immutable array of their own.</summary>
    /// <param name="items">The array.</param>
    /// <param name="count">The number of leading elements.</param>
    /// <returns>The copy, which shares nothing with the array.</returns>
    let copiedRange (items: 'T array) (count: int) =
        publish (items.AsSpan(0, count).ToArray())

    /// <summary>Whether any of the first glyphs of a line carries a class in a mask or a Unicode property in another.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <param name="refused">The glyph class bits.</param>
    /// <param name="refusedUnicode">The Unicode property bits.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>True when a glyph is refused.</returns>
    [<TailCall>]
    let rec private anyRefused (props: GlyphProps array) (count: int) (refused: uint16) (refusedUnicode: uint16) (index: int) =
        match index < count with
        | false -> false
        | true ->
            match props[index].GlyphClass &&& refused <> 0us || props[index].UnicodeProps &&& refusedUnicode <> 0us with
            | true -> true
            | false -> anyRefused props count refused refusedUnicode (index + 1)

    /// <summary>Finishes a substituted line in the arrays a worker owns, as <see cref="simpleFinish"/> finishes one in fresh arrays: the
    /// default positions of its glyphs and every glyph's mask propagated over its cluster, cut down to a mask. Nothing is written when
    /// a glyph is refused.</summary>
    /// <param name="advances">The per-glyph advance.</param>
    /// <param name="infos">The glyphs, which carry the masks.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The positions, written.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <param name="refused">The glyph class bits that send the line to the general positioning.</param>
    /// <param name="refusedUnicode">The Unicode property bits that send the line to the general positioning.</param>
    /// <param name="andMask">The mask the propagated masks are cut down to.</param>
    /// <returns>True when the line was finished, false when a glyph is refused.</returns>
    let finishSubstitutedOf<'A when 'A : struct and 'A :> ISimpleAdvances> (advances: 'A) (infos: GlyphInfo array) (props: GlyphProps array) (positions: GlyphPosition array) (metrics: ReadOnlySpan<byte>) (count: int) (refused: uint16) (refusedUnicode: uint16) (andMask: uint32) =
        match anyRefused props count refused refusedUnicode 0 with
        | true -> false
        | false ->
            let mutable start = 0
            while start < count do
                let cluster = infos[start].Cluster
                let mutable finish = start + 1
                let mutable mask = infos[start].Mask
                while finish < count && infos[finish].Cluster = cluster do
                    mask <- mask ||| infos[finish].Mask
                    finish <- finish + 1
                let propagated = mask &&& andMask
                let mutable index = start
                while index < finish do
                    positions[index] <- { XAdvance = advances.Advance(infos[index].Codepoint, metrics); YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                    infos[index] <- { infos[index] with Mask = propagated }
                    index <- index + 1
                start <- finish
            true

    /// <summary>Writes the default positions of a substituted line into the arrays a worker owns, for the lean pair loop to adjust, when
    /// the line is one the loop applies to: no glyph is refused (a class in a mask, a Unicode property in another), every glyph carries
    /// a given mask, and no two neighbouring glyphs share a cluster.</summary>
    /// <param name="advances">The per-glyph advance.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The positions, written.</param>
    /// <param name="metrics">The bytes of the horizontal metrics.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <param name="refused">The glyph class bits that send the line to the general positioning.</param>
    /// <param name="refusedUnicode">The Unicode property bits that send the line to the general positioning.</param>
    /// <param name="required">The mask bits every glyph must carry.</param>
    /// <returns>True when the positions were written, false when the line is not one the lean loop applies to.</returns>
    let leanAdvancesOf<'A when 'A : struct and 'A :> ISimpleAdvances> (advances: 'A) (infos: GlyphInfo array) (props: GlyphProps array) (positions: GlyphPosition array) (metrics: ReadOnlySpan<byte>) (count: int) (refused: uint16) (refusedUnicode: uint16) (required: uint32) =
        let mutable accepted = true
        let mutable index = 0
        while accepted && index < count do
            let info = infos[index]
            match props[index].GlyphClass &&& refused <> 0us || props[index].UnicodeProps &&& refusedUnicode <> 0us || info.Mask &&& required <> required || (index > 0 && info.Cluster = infos[index - 1].Cluster) with
            | true -> accepted <- false
            | false ->
                positions[index] <- { XAdvance = advances.Advance(info.Codepoint, metrics); YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
                index <- index + 1
        accepted

    /// <summary>The first element of each line's window in a document's arrays: the running sum of the lines' glyph counts.</summary>
    /// <param name="counts">The glyphs of each line.</param>
    /// <param name="limit">The most elements an array of the document may have.</param>
    /// <returns>The window starts and the total, or a start of minus one for every line and a total of zero when the lines do not fit
    /// under the limit.</returns>
    let windowStarts (counts: ReadOnlySpan<int>) (limit: int) =
        let starts = Array.zeroCreate<int> counts.Length
        let mutable total = 0L
        let mutable index = 0
        while index < counts.Length do
            starts[index] <- int (min total (int64 Int32.MaxValue))
            total <- total + int64 counts[index]
            index <- index + 1
        match total > int64 limit with
        | true -> struct (publish (Array.create counts.Length -1), 0)
        | false -> struct (publish starts, int total)

    /// <summary>Copies the first elements of a worker's array into a window of a document's array: the one place where a document's
    /// workers write, each into a window no other worker's overlaps.</summary>
    /// <param name="target">The document's array.</param>
    /// <param name="offset">The first element of the window.</param>
    /// <param name="source">The worker's array.</param>
    /// <param name="count">How many elements to copy.</param>
    let copyWindow (target: 'T array) (offset: int) (source: 'T array) (count: int) =
        Array.Copy(source, 0, target, offset, count)

    /// <summary>Copies a shaped line's elements into a window of a document's array.</summary>
    /// <param name="target">The document's array.</param>
    /// <param name="offset">The first element of the window.</param>
    /// <param name="source">The shaped line.</param>
    let copyShaped (target: 'T array) (offset: int) (source: ImmutableArray<'T>) =
        source.AsSpan().CopyTo(Span<'T>(target, offset, source.Length))

    /// <summary>Clears the part of a reserved window a line did not fill, so that the array a reader sees holds nothing but glyphs
    /// and zeros.</summary>
    /// <param name="target">The document's array.</param>
    /// <param name="offset">The first element of the unused part.</param>
    /// <param name="count">How many elements to clear.</param>
    let clearWindow (target: 'T array) (offset: int) (count: int) =
        Array.Clear(target, offset, count)

    [<Literal>]
    let private HangulLBase = 0x1100u

    [<Literal>]
    let private HangulVBase = 0x1161u

    [<Literal>]
    let private HangulTBase = 0x11A7u

    [<Literal>]
    let private HangulSBase = 0xAC00u

    [<Literal>]
    let private HangulLCount = 19u

    [<Literal>]
    let private HangulVCount = 21u

    [<Literal>]
    let private HangulTCount = 28u

    [<Literal>]
    let private HangulNCount = 588u

    [<Literal>]
    let private HangulSCount = 11172u

    [<Literal>]
    let private HangulDottedCircle = 0x25CCu

    [<Literal>]
    let private HangulLjmoFeature = 1uy

    [<Literal>]
    let private HangulVjmoFeature = 2uy

    [<Literal>]
    let private HangulTjmoFeature = 3uy

    /// <summary>Whether a code point is a leading consonant that composes, <c>isCombiningL</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1100 to U+1112.</returns>
    let private hangulCombiningL (u: uint32) =
        u >= HangulLBase && u <= HangulLBase + HangulLCount - 1u

    /// <summary>Whether a code point is a vowel that composes, <c>isCombiningV</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1161 to U+1175.</returns>
    let private hangulCombiningV (u: uint32) =
        u >= HangulVBase && u <= HangulVBase + HangulVCount - 1u

    /// <summary>Whether a code point is a trailing consonant that composes, <c>isCombiningT</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+11A8 to U+11C2.</returns>
    let private hangulCombiningT (u: uint32) =
        u >= HangulTBase + 1u && u <= HangulTBase + HangulTCount - 1u

    /// <summary>Whether a code point is a precomposed syllable, <c>isCombinedS</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+AC00 to U+D7A3.</returns>
    let private hangulCombinedS (u: uint32) =
        u >= HangulSBase && u <= HangulSBase + HangulSCount - 1u

    /// <summary>Whether a code point is a leading consonant, <c>isL</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1100 to U+115F and U+A960 to U+A97C.</returns>
    let private hangulIsL (u: uint32) =
        (u >= 0x1100u && u <= 0x115Fu) || (u >= 0xA960u && u <= 0xA97Cu)

    /// <summary>Whether a code point is a vowel, <c>isV</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+1160 to U+11A7 and U+D7B0 to U+D7C6.</returns>
    let private hangulIsV (u: uint32) =
        (u >= 0x1160u && u <= 0x11A7u) || (u >= 0xD7B0u && u <= 0xD7C6u)

    /// <summary>Whether a code point is a trailing consonant, <c>isT</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+11A8 to U+11FF and U+D7CB to U+D7FB.</returns>
    let private hangulIsT (u: uint32) =
        (u >= 0x11A8u && u <= 0x11FFu) || (u >= 0xD7CBu && u <= 0xD7FBu)

    /// <summary>Whether a code point is a Hangul tone mark, <c>isHangulTone</c>.</summary>
    /// <param name="u">The code point.</param>
    /// <returns>True for U+302E and U+302F.</returns>
    let private hangulIsTone (u: uint32) =
        u >= 0x302Eu && u <= 0x302Fu

    /// <summary>The code point of the glyph at an input index, zero past the end.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The input index.</param>
    /// <returns>The code point.</returns>
    let private hangulInputAt (machine: inref<Machine>) index =
        match index < machine.Len with
        | true -> machine.Info[index].Codepoint
        | false -> 0u

    /// <summary>Gives the output glyph at an index its jamo feature, the <c>SetOutputGlyph</c> edit of the pass.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="index">The glyph, in the output.</param>
    /// <param name="feature">The feature.</param>
    let private hangulSetFeature (machine: byref<Machine>) index (feature: byte) =
        let outInfo = outInfoArray &machine
        let outProps = outPropsArray &machine
        setOutputGlyph &machine index outInfo[index].Codepoint { outProps[index] with ShaperAuxiliary = feature }

    /// <summary>Gives each glyph of a decomposed or uncomposed syllable its jamo feature and merges its clusters, the states
    /// <c>HangulJamo</c> and <c>HangulFeature</c> of the pass, unless the buffer has failed: the loop of <c>preprocess_text_hangul</c> then
    /// breaks before it (src/hb-ot-shaper-hangul.cc:310 and :370), and the buffer is left as the failure left it.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="first">The first glyph of the syllable, in the output.</param>
    /// <param name="finish">The end of the syllable, in the output.</param>
    let private hangulFeatures (machine: byref<Machine>) first finish =
        match machine.Successful with
        | false -> ()
        | true ->
            hangulSetFeature &machine first HangulLjmoFeature
            hangulSetFeature &machine (first + 1) HangulVjmoFeature
            match finish - first = 3 with
            | true -> hangulSetFeature &machine (first + 2) HangulTjmoFeature
            | false -> ()
            mergeOutGraphemeClusters &machine first finish

    /// <summary>Handles a tone mark, <c>toneEdits</c>: moved in front of the syllable it follows unless it is zero width, or given a
    /// dotted circle to stand on, or left alone.</summary>
    /// <param name="font">The font.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="scratch">The array the replacement code points are written to.</param>
    /// <param name="u">The tone mark.</param>
    /// <param name="start">The start of the last syllable, in the output.</param>
    /// <param name="finish">The end of the last syllable, in the output.</param>
    /// <returns>The start and the end of the last syllable after the tone mark.</returns>
    let private hangulTone<'F when 'F: struct and 'F :> IHangulGlyphs> (font: 'F) (machine: byref<Machine>) (scratch: uint32 array) u start finish =
        let outLen = machine.OutLen
        let index = machine.Idx
        let zeroWidth = font.ZeroWidth u
        match start < finish && finish = outLen with
        | true ->
            unsafeToBreakFromOutbuffer &machine start index
            leanNextOne &machine
            match zeroWidth with
            | true -> ()
            | false ->
                mergeOutClusters &machine start (finish + 1)
                moveOutput &machine finish start
            struct (outLen + 1, outLen + 1)
        | false ->
            match machine.Flags &&& BufferFlags.DoNotInsertDottedCircle = BufferFlags.Default && font.Has HangulDottedCircle with
            | false ->
                leanNextOne &machine
                struct (outLen + 1, outLen + 1)
            | true ->
                scratch[0] <- (match zeroWidth with | true -> HangulDottedCircle | false -> u)
                scratch[1] <- (match zeroWidth with | true -> u | false -> HangulDottedCircle)
                replaceGlyphs &machine 1 2 (ReadOnlySpan<uint32>(scratch, 0, 2))
                struct (outLen + 2, outLen + 2)

    /// <summary>Composes the glyphs at the cursor into one when they all share a cluster and the output is the input's own array with room
    /// to spare, the two edits of <c>hangulJamo</c> (<c>unsafeToBreak</c> over the glyphs, then <c>replaceGlyphs</c>) for the case in which
    /// neither moves a cluster nor sets a flag: each costs the operations of its range, at every cluster level (a level that does not merge
    /// clusters marks the range unsafe to break instead), and the one glyph written is the first's with the syllable's code point. Any other
    /// case is left to the edits.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="count">The number of glyphs to compose, two or three.</param>
    /// <param name="composed">The syllable's code point.</param>
    /// <returns>True when the glyphs were composed, false when nothing was done.</returns>
    let private hangulComposeFlat (machine: byref<Machine>) (count: int) (composed: uint32) =
        let index = machine.Idx
        let cluster = machine.Info[index].Cluster
        let mutable flat = true
        let mutable at = index + 1
        while flat && at < index + count do
            flat <- machine.Info[at].Cluster = cluster
            at <- at + 1
        match flat && machine.HaveOutput && not machine.OutIsSeparate && machine.OutLen + 1 < machine.Allocated && machine.OutLen + 1 <= machine.Info.Length with
        | false -> false
        | true ->
            touch &machine
            let maxOps = machine.MaxOps - 2 * count
            let outInfo = outInfoArray &machine
            let outProps = outPropsArray &machine
            outInfo[machine.OutLen] <- { machine.Info[index] with Codepoint = composed }
            outProps[machine.OutLen] <- machine.Props[index]
            machine.Idx <- index + count
            machine.OutLen <- machine.OutLen + 1
            machine.MaxOps <- maxOps
            machine.Successful <- machine.Successful && maxOps >= 0
            true

    /// <summary>Handles a leading consonant followed by a vowel and maybe a trailing consonant, <c>jamoEdits</c>: composed into one
    /// syllable when the font has it, else each glyph is given its jamo feature.</summary>
    /// <param name="font">The font.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="scratch">The array the replacement code points are written to.</param>
    /// <param name="l">The leading consonant.</param>
    /// <param name="v">The vowel.</param>
    /// <returns>The start and the end of the last syllable.</returns>
    let private hangulJamo<'F when 'F: struct and 'F :> IHangulGlyphs> (font: 'F) (machine: byref<Machine>) (scratch: uint32 array) l v =
        let outLen = machine.OutLen
        let index = machine.Idx
        let t =
            match hangulIsT (hangulInputAt &machine (index + 2)) with
            | true -> hangulInputAt &machine (index + 2)
            | false -> 0u
        let tindex = match t with | 0u -> 0u | _ -> t - HangulTBase
        let count = match t with | 0u -> 2 | _ -> 3
        let s = HangulSBase + (l - HangulLBase) * HangulNCount + (v - HangulVBase) * HangulTCount + tindex
        let composes = hangulCombiningL l && hangulCombiningV v && (t = 0u || hangulCombiningT t) && font.Has s
        match composes && hangulComposeFlat &machine count s with
        | true -> struct (outLen, outLen + 1)
        | false ->
            unsafeToBreak &machine index (index + count)
            match composes with
            | true ->
                scratch[0] <- s
                replaceGlyphs &machine count 1 (ReadOnlySpan<uint32>(scratch, 0, 1))
                struct (outLen, outLen + 1)
            | false ->
                nextGlyphs &machine count
                hangulFeatures &machine outLen (outLen + count)
                struct (outLen, outLen + count)

    /// <summary>Handles a precomposed syllable that the font lacks or that a trailing consonant follows, the rest of
    /// <see cref="hangulSyllable"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="scratch">The array the replacement code points are written to.</param>
    /// <param name="s">The syllable.</param>
    /// <param name="finish">The end of the last syllable, in the output.</param>
    /// <param name="hasS">Whether the font has the syllable.</param>
    /// <param name="tindex">The syllable's trailing consonant index.</param>
    /// <param name="following">The code point after the syllable, zero at the end.</param>
    /// <returns>The start and the end of the last syllable.</returns>
    let private hangulSyllableEdited<'F when 'F: struct and 'F :> IHangulGlyphs> (font: 'F) (machine: byref<Machine>) (scratch: uint32 array) s finish hasS tindex following =
        let outLen = machine.OutLen
        let index = machine.Idx
        let lindex = (s - HangulSBase) / HangulNCount
        let vindex = ((s - HangulSBase) % HangulNCount) / HangulTCount
        let combines = tindex = 0u && hangulCombiningT following
        let newS = s + (following - HangulTBase)
        match combines && font.Has newS with
        | true ->
            scratch[0] <- newS
            replaceGlyphs &machine 2 1 (ReadOnlySpan<uint32>(scratch, 0, 1))
            struct (outLen, outLen + 1)
        | false ->
            let trailing = tindex = 0u && hangulIsT following
            let leading = HangulLBase + lindex
            let vowel = HangulVBase + vindex
            let tail = HangulTBase + tindex
            let canDecompose = (not hasS || trailing) && font.Has leading && font.Has vowel && (tindex = 0u || font.Has tail)
            match canDecompose with
            | true ->
                let length = match tindex with | 0u -> 2 | _ -> 3
                let keeps = hasS && tindex = 0u
                scratch[0] <- leading
                scratch[1] <- vowel
                scratch[2] <- tail
                match combines with
                | true -> unsafeToBreak &machine index (index + 2)
                | false -> ()
                replaceGlyphs &machine 1 length (ReadOnlySpan<uint32>(scratch, 0, length))
                match keeps with
                | true -> leanNextOne &machine
                | false -> ()
                let finishOf = outLen + length + (match keeps with | true -> 1 | false -> 0)
                hangulFeatures &machine outLen finishOf
                struct (outLen, finishOf)
            | false ->
                match combines || trailing with
                | true ->
                    unsafeToBreak &machine index (index + 2)
                    leanNextOne &machine
                | false -> leanNextOne &machine
                struct (outLen, (match hasS with | true -> outLen + 1 | false -> finish))

    /// <summary>Handles a precomposed syllable in the text, <c>syllableEdits</c>: kept when the font has it, combined with a trailing
    /// consonant that follows when the font has the result, else decomposed into jamo.</summary>
    /// <param name="font">The font.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="scratch">The array the replacement code points are written to.</param>
    /// <param name="s">The syllable.</param>
    /// <param name="finish">The end of the last syllable, in the output.</param>
    /// <returns>The start and the end of the last syllable.</returns>
    let private hangulSyllable<'F when 'F: struct and 'F :> IHangulGlyphs> (font: 'F) (machine: byref<Machine>) (scratch: uint32 array) s finish =
        let outLen = machine.OutLen
        let index = machine.Idx
        let hasS = font.Has s
        let tindex = (s - HangulSBase) % HangulTCount
        let following = hangulInputAt &machine (index + 1)
        match hasS && not (tindex = 0u && hangulIsT following) with
        | true ->
            leanNextOne &machine
            struct (outLen, outLen + 1)
        | false -> hangulSyllableEdited font &machine scratch s finish hasS tindex following

    /// <summary>Handles the glyph at the cursor that is no tone mark: a syllable, the start of one, or none.</summary>
    /// <param name="font">The font.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="scratch">The array the replacement code points are written to.</param>
    /// <param name="u">The code point.</param>
    /// <param name="finish">The end of the last syllable, in the output.</param>
    /// <returns>The start and the end of the last syllable.</returns>
    let private hangulPlain<'F when 'F: struct and 'F :> IHangulGlyphs> (font: 'F) (machine: byref<Machine>) (scratch: uint32 array) u finish =
        let outLen = machine.OutLen
        let next = hangulInputAt &machine (machine.Idx + 1)
        match hangulIsL u && machine.Idx + 1 < machine.Len && hangulIsV next with
        | true -> hangulJamo font &machine scratch u next
        | false ->
            match hangulIsL u && machine.Idx + 1 < machine.Len, hangulCombinedS u with
            | true, _ ->
                leanNextOne &machine
                struct (outLen, finish)
            | false, true -> hangulSyllable font &machine scratch u finish
            | false, false ->
                leanNextOne &machine
                struct (outLen, finish)

    /// <summary>The most glyphs <see cref="hangulInertRun"/> reads in one scan, so that a scan that cannot be moved in one step, and is made
    /// again one glyph on, reads a bounded number of glyphs.</summary>
    [<Literal>]
    let private HangulInertScan = 64

    /// <summary>Reads the glyphs from the cursor that the pass only advances through, <c>hangulPlain</c> for each: no tone mark, no leading
    /// consonant before a vowel, and no syllable that the font lacks or that a trailing consonant follows, and what the loop's start and
    /// finish are after the last of them.</summary>
    /// <param name="font">The font.</param>
    /// <param name="machine">The machine.</param>
    /// <param name="finish">The end of the last syllable, in the output, before the glyphs.</param>
    /// <returns>The number of glyphs, the start and the end of the last syllable after them.</returns>
    let private hangulInertRun<'F when 'F: struct and 'F :> IHangulGlyphs> (font: 'F) (machine: inref<Machine>) (finish: int) =
        let first = machine.Idx
        let limit = min machine.Len (first + HangulInertScan)
        let mutable at = first
        let mutable start = 0
        let mutable last = finish
        let mutable inert = true
        while inert && at < limit do
            let u = machine.Info[at].Codepoint
            let next = hangulInputAt &machine (at + 1)
            let offset = machine.OutLen + (at - first)
            match hangulIsTone u || (hangulIsL u && hangulIsV next) with
            | true -> inert <- false
            | false ->
                match hangulCombinedS u with
                | false ->
                    start <- offset
                    at <- at + 1
                | true ->
                    match font.Has u && not ((u - HangulSBase) % HangulTCount = 0u && hangulIsT next) with
                    | true ->
                        start <- offset
                        last <- offset + 1
                        at <- at + 1
                    | false -> inert <- false
        struct (at - first, start, last)

    /// <summary>The loop of <c>preprocess_text_hangul</c> on the machine, edit for edit what the pass of <c>Hangul.preprocess</c> issues,
    /// without its states, its cursors and its edit arrays.</summary>
    /// <param name="font">The font.</param>
    /// <param name="machine">The machine, before its first edit.</param>
    let private hangulLoop<'F when 'F: struct and 'F :> IHangulGlyphs> (font: 'F) (machine: byref<Machine>) =
        materialize &machine
        clearOutput &machine
        machine.Passive <- false
        let scratch = Array.zeroCreate<uint32> 3
        let mutable start = 0
        let mutable finish = 0
        while machine.Idx < machine.Len && machine.Successful do
            let struct (inert, inertStart, inertFinish) = hangulInertRun font &machine finish
            match inert > 0 && leanBulkSafe &machine inert with
            | true ->
                nextGlyphs &machine inert
                start <- inertStart
                finish <- inertFinish
            | false ->
                let u = machine.Info[machine.Idx].Codepoint
                let struct (nextStart, nextFinish) =
                    match hangulIsTone u with
                    | true -> hangulTone font &machine scratch u start finish
                    | false -> hangulPlain font &machine scratch u finish
                start <- nextStart
                finish <- nextFinish
        sync &machine

    /// <summary>Runs the Hangul preprocess over a run's arrays on the out-buffer machine, <c>preprocess_text_hangul</c>.</summary>
    /// <param name="input">The run fields the machine reads.</param>
    /// <param name="font">The font's view of what composes.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runHangul<'F when 'F: struct and 'F :> IHangulGlyphs> (input: RunInput) (font: 'F) =
        let mutable initial = newMachine input Digest.empty false false
        hangulLoop font &initial
        finishRun &initial (initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps) input.Positions

    /// <summary>Runs the Hangul preprocess in place over the arrays of a run that this call's caller made and keeps no other reference to:
    /// the machine takes the glyph and property arrays as its working arrays, as <c>materialize</c> would have copied them, so that a run
    /// the pass shortens is published from the same arrays and no copy is made. The result is that of <see cref="runHangul"/>.</summary>
    /// <param name="input">The run fields the machine reads; the properties are as many as the glyphs and there are no positions.</param>
    /// <param name="font">The font's view of what composes.</param>
    /// <returns>The run fields the machine produces.</returns>
    let runHangulOwned<'F when 'F: struct and 'F :> IHangulGlyphs> (input: RunInput) (font: 'F) =
        let mutable initial = newMachine input Digest.empty false true
        initial.Info <- nonNull (ImmutableCollectionsMarshal.AsArray input.Infos)
        initial.Props <- nonNull (ImmutableCollectionsMarshal.AsArray input.Props)
        initial.Allocated <- (match initial.Len = 0 with | true -> 0 | false -> growAllocated initial.Len 0)
        initial.Materialized <- true
        hangulLoop font &initial
        finishRun &initial (initial.Passive && initial.Successful && initial.MaxOps = initial.InitialMaxOps) input.Positions

    /// <summary>Sets the masks of the jamo features, in place, <c>setup_masks_hangul</c>: a glyph gets the mask of the feature its properties
    /// name, and a jamo has <c>calt</c> cleared; a glyph that is no jamo and has no feature, a syllable or any other letter, is not written.</summary>
    /// <param name="ljmo">The mask of <c>ljmo</c>.</param>
    /// <param name="vjmo">The mask of <c>vjmo</c>.</param>
    /// <param name="tjmo">The mask of <c>tjmo</c>.</param>
    /// <param name="calt">The mask of <c>calt</c>.</param>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The glyph properties, as many as the glyphs.</param>
    let hangulMasksOwned (ljmo: uint32) (vjmo: uint32) (tjmo: uint32) (calt: uint32) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let source = props.AsSpan()
        let mutable index = 0
        while index < array.Length do
            let info = array[index]
            let feature = source[index].ShaperAuxiliary
            let jamo = hangulIsL info.Codepoint || hangulIsV info.Codepoint || hangulIsT info.Codepoint
            match feature = 0uy && not jamo with
            | true -> ()
            | false ->
                let mask =
                    info.Mask
                    ||| (match feature with
                         | HangulLjmoFeature -> ljmo
                         | HangulVjmoFeature -> vjmo
                         | HangulTjmoFeature -> tjmo
                         | _ -> 0u)
                array[index] <- { info with Mask = (match jamo with | true -> mask &&& ~~~calt | false -> mask) }
            index <- index + 1

    /// <summary>Sets the mask and the Unicode properties of a run of letters and of the code points a small table holds, in one loop: the
    /// extension of <see cref="lowProps"/> to the letters beyond the table (Hangul syllables, ideographs), whose properties need no
    /// neighbour. The properties and the scratch flags are those of the table or of the code point's own; the same two code points are
    /// refused in the table's range.</summary>
    /// <param name="table">The properties of the code points from zero to the table's length.</param>
    /// <param name="mask">The mask given to every glyph.</param>
    /// <param name="refusedFirst">The first code point the table's range refuses.</param>
    /// <param name="refusedSecond">The second code point the table's range refuses.</param>
    /// <param name="plain">The properties of a code point beyond the table.</param>
    /// <param name="source">The glyphs.</param>
    /// <returns>The glyphs with the mask, their properties and the scratch flags of every glyph, or none at the first code point that is
    /// refused or is not plain.</returns>
    let lettersProps<'F when 'F : struct and 'F :> IPlainProps> (table: ImmutableArray<struct (uint16 * ScratchFlags)>) (mask: uint32) (refusedFirst: uint32) (refusedSecond: uint32) (plain: 'F) (source: ReadOnlySpan<GlyphInfo>) =
        let limit = uint32 table.Length
        let infos = GC.AllocateUninitializedArray<GlyphInfo>(source.Length)
        let props = GC.AllocateUninitializedArray<GlyphProps>(source.Length)
        let mutable flags = ScratchFlags.Default
        let mutable index = 0
        let mutable accepted = true
        while accepted && index < source.Length do
            let codepoint = source[index].Codepoint
            let struct (isPlain, unicode, scratchFlags) =
                match codepoint < limit with
                | true ->
                    let struct (tabled, tableScratch) = table[int codepoint]
                    struct (codepoint <> refusedFirst && codepoint <> refusedSecond, tabled, tableScratch)
                | false -> plain.Plain codepoint
            accepted <- isPlain
            infos[index] <- { source[index] with Mask = mask }
            props[index] <- { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
            flags <- flags ||| scratchFlags
            index <- index + 1
        match accepted with
        | true -> ValueSome (struct (publish infos, publish props, flags))
        | false -> ValueNone

    /// <summary>Sets the mask and the Unicode properties of a run whose every code point is ASCII as <see cref="asciiProps"/> does, writing
    /// the masks into the glyphs the caller made and shares with no one, so that the pass allocates only the properties.</summary>
    /// <param name="table">The properties of U+0000 to U+00FF.</param>
    /// <param name="mask">The mask given to every glyph.</param>
    /// <param name="continuation">The continuation bit of the packed properties.</param>
    /// <param name="ignorable">The default-ignorable bits of the packed properties.</param>
    /// <param name="infos">The glyphs, with owned storage, whose masks are written.</param>
    /// <returns>The properties and whether some glyph is a continuation and whether some is a default ignorable, or none at the first
    /// code point beyond ASCII, before any mask is written.</returns>
    let asciiPropsInPlace (table: ImmutableArray<struct (uint16 * ScratchFlags)>) (mask: uint32) (continuation: uint16) (ignorable: uint16) (infos: ImmutableArray<GlyphInfo>) =
        let target = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable scanned = 0
        while scanned < target.Length && target[scanned].Codepoint < 0x80u do
            scanned <- scanned + 1
        match scanned < target.Length with
        | true -> ValueNone
        | false ->
            let props = GC.AllocateUninitializedArray<GlyphProps>(target.Length)
            let mutable bits = 0us
            let mutable index = 0
            while index < target.Length do
                let info = target[index]
                let struct (unicode, _) = table[int info.Codepoint]
                target[index] <- { info with Mask = mask }
                props[index] <- { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
                bits <- bits ||| unicode
                index <- index + 1
            ValueSome (struct (publish props, bits &&& continuation <> 0us, bits &&& ignorable <> 0us))

    /// <summary>Sets the mask and the Unicode properties of a run whose every code point is in a table's range as <see cref="lowProps"/>
    /// does, writing the masks into the glyphs the caller made and shares with no one.</summary>
    /// <param name="table">The properties of the code points from zero to the table's length.</param>
    /// <param name="mask">The mask given to every glyph.</param>
    /// <param name="scratch">The scratch flags the run has.</param>
    /// <param name="refusedFirst">The first code point that is refused.</param>
    /// <param name="refusedSecond">The second code point that is refused.</param>
    /// <param name="infos">The glyphs, with owned storage, whose masks are written.</param>
    /// <returns>The properties and the scratch flags with those of every glyph, or none at the first code point beyond the table or the
    /// first one that is refused, before any mask is written.</returns>
    let lowPropsInPlace (table: ImmutableArray<struct (uint16 * ScratchFlags)>) (mask: uint32) (scratch: ScratchFlags) (refusedFirst: uint32) (refusedSecond: uint32) (infos: ImmutableArray<GlyphInfo>) =
        let target = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let limit = uint32 table.Length
        let mutable scanned = 0
        while scanned < target.Length && target[scanned].Codepoint < limit && target[scanned].Codepoint <> refusedFirst && target[scanned].Codepoint <> refusedSecond do
            scanned <- scanned + 1
        match scanned < target.Length with
        | true -> ValueNone
        | false ->
            let props = GC.AllocateUninitializedArray<GlyphProps>(target.Length)
            let mutable flags = scratch
            let mutable index = 0
            while index < target.Length do
                let info = target[index]
                let struct (unicode, scratchFlags) = table[int info.Codepoint]
                target[index] <- { info with Mask = mask }
                props[index] <- { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
                flags <- flags ||| scratchFlags
                index <- index + 1
            ValueSome (struct (publish props, flags))

    /// <summary>Sets the mask and the Unicode properties of a run of letters and of the code points a small table holds as
    /// <see cref="lettersProps"/> does, writing the masks into the glyphs the caller made and shares with no one. A run that is refused
    /// part way has the masks of the glyphs before the refusal written, which the pass that takes the run next sets again.</summary>
    /// <param name="table">The properties of the code points from zero to the table's length.</param>
    /// <param name="mask">The mask given to every glyph.</param>
    /// <param name="refusedFirst">The first code point the table's range refuses.</param>
    /// <param name="refusedSecond">The second code point the table's range refuses.</param>
    /// <param name="plain">The properties of a code point beyond the table.</param>
    /// <param name="infos">The glyphs, with owned storage, whose masks are written.</param>
    /// <returns>The properties and the scratch flags of every glyph, or none at the first code point that is refused or is not plain.</returns>
    let lettersPropsInPlace<'F when 'F : struct and 'F :> IPlainProps> (table: ImmutableArray<struct (uint16 * ScratchFlags)>) (mask: uint32) (refusedFirst: uint32) (refusedSecond: uint32) (plain: 'F) (infos: ImmutableArray<GlyphInfo>) =
        let target = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let limit = uint32 table.Length
        let props = GC.AllocateUninitializedArray<GlyphProps>(target.Length)
        let mutable flags = ScratchFlags.Default
        let mutable index = 0
        let mutable accepted = true
        while accepted && index < target.Length do
            let info = target[index]
            let codepoint = info.Codepoint
            let struct (isPlain, unicode, scratchFlags) =
                match codepoint < limit with
                | true ->
                    let struct (tabled, tableScratch) = table[int codepoint]
                    struct (codepoint <> refusedFirst && codepoint <> refusedSecond, tabled, tableScratch)
                | false -> plain.Plain codepoint
            accepted <- isPlain
            target[index] <- { info with Mask = mask }
            props[index] <- { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode }
            flags <- flags ||| scratchFlags
            index <- index + 1
        match accepted with
        | true -> ValueSome (struct (publish props, flags))
        | false -> ValueNone


/// <summary>A growable builder of values: append, append a range, truncate, then publish zero-copy.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal PointBuilder<'T> =
    private
        { Items: 'T array
          Count: int }

/// <summary>Growing and publishing <see cref="PointBuilder{T}"/>.</summary>
[<RequireQualifiedAccess>]
module internal PointBuilder =
    /// <summary>The number of slots a new builder reserves before it starts doubling.</summary>
    [<Literal>]
    let private InitialCapacity = 4

    /// <summary>Creates an empty builder.</summary>
    /// <returns>The empty builder.</returns>
    let create<'T> () : PointBuilder<'T> =
        { Items = Array.zeroCreate<'T> 0
          Count = 0 }

    /// <summary>Returns a backing array with room for one more appended value.</summary>
    /// <param name="builder">The builder.</param>
    /// <returns>The array, grown when full.</returns>
    let private ensureRoom (builder: PointBuilder<'T>) : 'T array =
        match builder.Count < builder.Items.Length with
        | true -> builder.Items
        | false ->
            let capacity = max InitialCapacity (builder.Items.Length * 2)
            let grown = Array.zeroCreate capacity
            Array.Copy(builder.Items, grown, builder.Count)
            grown

    /// <summary>Appends one value.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="item">The value.</param>
    /// <returns>The builder with the value appended.</returns>
    let append (builder: PointBuilder<'T>) (item: 'T) : PointBuilder<'T> =
        let items = ensureRoom builder
        items[builder.Count] <- item
        { Items = items
          Count = builder.Count + 1 }

    /// <summary>Appends a range of values.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="items">The values.</param>
    /// <returns>The builder with the values appended.</returns>
    let appendRange (builder: PointBuilder<'T>) (items: ReadOnlySpan<'T>) : PointBuilder<'T> =
        match items.Length with
        | 0 -> builder
        | length ->
            match builder.Count + length <= builder.Items.Length with
            | true ->
                items.CopyTo(Span<'T>(builder.Items, builder.Count, length))
                { Items = builder.Items
                  Count = builder.Count + length }
            | false ->
                let capacity = max InitialCapacity (max (builder.Items.Length * 2) (builder.Count + length))
                let grown = Array.zeroCreate capacity
                Array.Copy(builder.Items, grown, builder.Count)
                items.CopyTo(Span<'T>(grown, builder.Count, length))
                { Items = grown
                  Count = builder.Count + length }

    /// <summary>Truncates the builder to a length.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="length">The new length.</param>
    /// <returns>The truncated builder.</returns>
    let truncate (builder: PointBuilder<'T>) (length: int) : PointBuilder<'T> =
        { Items = builder.Items
          Count = min length builder.Count }

    /// <summary>Publishes the built values as an immutable array.</summary>
    /// <param name="builder">The builder.</param>
    /// <returns>The values.</returns>
    let publish (builder: PointBuilder<'T>) : ImmutableArray<'T> =
        match builder.Count = builder.Items.Length with
        | true -> ImmutableCollectionsMarshal.AsImmutableArray builder.Items
        | false -> ImmutableCollectionsMarshal.AsImmutableArray (builder.Items.AsSpan(0, builder.Count).ToArray())

/// <summary>The working buffer of one gvar delta application: current point coordinates and per-point accumulated deltas.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal DeltaBuffer =
    private
        { Xs: float32 array
          Ys: float32 array
          Dx: float32 array
          Dy: float32 array
          Referenced: byte array }

/// <summary>Building and publishing <see cref="DeltaBuffer"/>.</summary>
[<RequireQualifiedAccess>]
module internal DeltaBuffer =
    /// <summary>Creates a buffer holding the point coordinates and zeroed deltas.</summary>
    /// <param name="xs">The initial x coordinates.</param>
    /// <param name="ys">The initial y coordinates.</param>
    /// <returns>The buffer.</returns>
    let create (xs: ReadOnlySpan<float32>) (ys: ReadOnlySpan<float32>) : DeltaBuffer =
        let count = xs.Length
        { Xs = xs.ToArray()
          Ys = ys.ToArray()
          Dx = Array.zeroCreate count
          Dy = Array.zeroCreate count
          Referenced = Array.zeroCreate count }

    /// <summary>Adds a scaled delta into a point's x coordinate, the fast path of gvar.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <param name="deltas">The signed deltas.</param>
    /// <param name="start">The first point to update.</param>
    /// <returns>The buffer.</returns>
    let addScaledX (buffer: DeltaBuffer) (scalar: float32) (deltas: ReadOnlySpan<int>) (start: int) : DeltaBuffer =
        let xs = buffer.Xs
        let mutable index = start
        while index < deltas.Length do
            xs[index] <- xs[index] + float32 deltas[index] * scalar
            index <- index + 1
        buffer

    /// <summary>Adds a scaled delta into a point's y coordinate, the fast path of gvar.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <param name="deltas">The signed deltas.</param>
    /// <param name="start">The first point to update.</param>
    /// <returns>The buffer.</returns>
    let addScaledY (buffer: DeltaBuffer) (scalar: float32) (deltas: ReadOnlySpan<int>) (start: int) : DeltaBuffer =
        let ys = buffer.Ys
        let mutable index = start
        while index < deltas.Length do
            ys[index] <- ys[index] + float32 deltas[index] * scalar
            index <- index + 1
        buffer

    /// <summary>Accumulates scaled x and y deltas into the per-point delta array, for a tuple that applies to all points.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <param name="x">The signed x deltas.</param>
    /// <param name="y">The signed y deltas.</param>
    /// <param name="start">The first point to accumulate.</param>
    /// <returns>The buffer.</returns>
    let accumulate (buffer: DeltaBuffer) (scalar: float32) (x: ReadOnlySpan<int>) (y: ReadOnlySpan<int>) (start: int) : DeltaBuffer =
        let dx = buffer.Dx
        let dy = buffer.Dy
        let mutable index = start
        while index < x.Length do
            dx[index] <- dx[index] + float32 x[index] * scalar
            dy[index] <- dy[index] + float32 y[index] * scalar
            index <- index + 1
        buffer

    /// <summary>Accumulates scaled deltas at the given point indices, marking each referenced point, for a tuple with its own points.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <param name="x">The signed x deltas.</param>
    /// <param name="y">The signed y deltas.</param>
    /// <param name="indices">The point indices, one per delta.</param>
    /// <param name="deltaStart">The first point a delta may address in phantom-only mode.</param>
    /// <param name="numDeltas">The number of deltas.</param>
    /// <returns>The buffer.</returns>
    let accumulateIndexed (buffer: DeltaBuffer) (scalar: float32) (x: ReadOnlySpan<int>) (y: ReadOnlySpan<int>) (indices: ReadOnlySpan<uint32>) (deltaStart: int) (numDeltas: int) : DeltaBuffer =
        let dx = buffer.Dx
        let dy = buffer.Dy
        let referenced = buffer.Referenced
        let count = dx.Length
        let mutable index = 0
        while index < numDeltas do
            let point = int indices[index]
            match point >= count || point < deltaStart with
            | true -> ()
            | false ->
                dx[point] <- dx[point] + float32 x[index] * scalar
                dy[point] <- dy[point] + float32 y[index] * scalar
                referenced[point] <- 1uy
            index <- index + 1
        buffer

    /// <summary>Writes an inferred delta into a point's delta slot, leaving the referenced flag alone.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="index">The point index.</param>
    /// <param name="dx">The x delta.</param>
    /// <param name="dy">The y delta.</param>
    /// <returns>The buffer.</returns>
    let setDelta (buffer: DeltaBuffer) (index: int) (dx: float32) (dy: float32) : DeltaBuffer =
        buffer.Dx[index] <- dx
        buffer.Dy[index] <- dy
        buffer

    /// <summary>Adds the accumulated deltas into the point coordinates and clears them, over a range.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="start">The first point of the range.</param>
    /// <param name="length">The number of points.</param>
    /// <returns>The buffer.</returns>
    let flush (buffer: DeltaBuffer) (start: int) (length: int) : DeltaBuffer =
        let xs = buffer.Xs
        let ys = buffer.Ys
        let dx = buffer.Dx
        let dy = buffer.Dy
        let referenced = buffer.Referenced
        let mutable index = start
        while index < start + length do
            xs[index] <- xs[index] + dx[index]
            ys[index] <- ys[index] + dy[index]
            dx[index] <- 0.0f
            dy[index] <- 0.0f
            referenced[index] <- 0uy
            index <- index + 1
        buffer

    /// <summary>Clears the accumulated deltas over a range without touching the point coordinates.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="start">The first point of the range.</param>
    /// <param name="length">The number of points.</param>
    /// <returns>The buffer.</returns>
    let clear (buffer: DeltaBuffer) (start: int) (length: int) : DeltaBuffer =
        let dx = buffer.Dx
        let dy = buffer.Dy
        let referenced = buffer.Referenced
        let mutable index = start
        while index < start + length do
            dx[index] <- 0.0f
            dy[index] <- 0.0f
            referenced[index] <- 0uy
            index <- index + 1
        buffer

    /// <summary>Publishes the buffer's final coordinates as immutable arrays.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <returns>The x and y coordinates.</returns>
    let publish (buffer: DeltaBuffer) : struct (ImmutableArray<float32> * ImmutableArray<float32>) =
        struct (Sealed.ofSpan (ReadOnlySpan<float32> buffer.Xs), Sealed.ofSpan (ReadOnlySpan<float32> buffer.Ys))

/// <summary>The fixed 513-slot argument stack of the CFF charstring interpreter, <c>arg_stack_t</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal CharArgStack =
    private
        { Values: double array
          Count: int
          Error: bool }

/// <summary>Building and mutating <see cref="CharArgStack"/>. Only this module writes the array.</summary>
[<RequireQualifiedAccess>]
module internal CharArgStack =
    /// <summary>The argument-stack limit, <c>cff_stack_t&lt;number_t, 513&gt;</c>.</summary>
    [<Literal>]
    let private MaxArguments = 513

    /// <summary>Creates an empty argument stack with its fixed array.</summary>
    /// <returns>The empty stack.</returns>
    let create () : CharArgStack =
        { Values = Array.zeroCreate MaxArguments
          Count = 0
          Error = false }

    /// <summary>Pushes one number; a full stack is the interpreter's error.</summary>
    /// <param name="stack">The stack.</param>
    /// <param name="value">The number.</param>
    /// <returns>The stack.</returns>
    let push (stack: CharArgStack) (value: double) : CharArgStack =
        match stack.Count < MaxArguments && not stack.Error with
        | true ->
            stack.Values[stack.Count] <- value
            { stack with Count = stack.Count + 1 }
        | false -> { stack with Error = true }

    /// <summary>Pushes an integer operand, <c>arg_stack_t::push_int</c>.</summary>
    /// <param name="stack">The stack.</param>
    /// <param name="value">The integer.</param>
    /// <returns>The stack.</returns>
    let pushInt (stack: CharArgStack) (value: int) : CharArgStack =
        push stack (double value)

    /// <summary>Pushes a 16.16 fixed operand, <c>arg_stack_t::push_fixed</c>.</summary>
    /// <param name="stack">The stack.</param>
    /// <param name="value">The fixed value.</param>
    /// <returns>The stack.</returns>
    let pushFixed (stack: CharArgStack) (value: int) : CharArgStack =
        push stack (double value / 65536.0)

    /// <summary>Pops one number; an empty stack is the interpreter's error.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>The stack and the value.</returns>
    let pop (stack: CharArgStack) : struct (CharArgStack * double) =
        match stack.Count > 0 && not stack.Error with
        | true -> struct ({ stack with Count = stack.Count - 1 }, stack.Values[stack.Count - 1])
        | false -> struct ({ stack with Error = true }, 0.0)

    /// <summary>Pops an integer operand, <c>arg_stack_t::pop_int</c>: an out-of-range number is the interpreter's error.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>The stack and the integer.</returns>
    let popInt (stack: CharArgStack) : struct (CharArgStack * int) =
        let struct (next, value) = pop stack
        match next.Error with
        | true -> struct (next, 0)
        | false ->
            match value >= float Int32.MinValue && value <= float Int32.MaxValue with
            | true -> struct (next, int value)
            | false -> struct ({ next with Error = true }, 0)

    /// <summary>Reads one operand without popping it, <c>cff_stack_t::operator[]</c>.</summary>
    /// <param name="stack">The stack.</param>
    /// <param name="index">The operand index.</param>
    /// <returns>The stack and the value.</returns>
    let evalArg (stack: CharArgStack) (index: int) : struct (CharArgStack * double) =
        match index >= 0 && index < stack.Count && not stack.Error with
        | true -> struct (stack, stack.Values[index])
        | false -> struct ({ stack with Error = true }, 0.0)

    /// <summary>The number of operands, <c>get_count</c>.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>The count.</returns>
    let getCount (stack: CharArgStack) : int = stack.Count

    /// <summary>Whether the stack is in error.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>True in error.</returns>
    let inError (stack: CharArgStack) : bool = stack.Error

    /// <summary>Clears every operand, <c>clear_args</c>.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>The cleared stack.</returns>
    let clear (stack: CharArgStack) : CharArgStack =
        { stack with Count = 0 }

    /// <summary>Writes one operand in place, <c>arg_stack_t::operator[]</c> as an lvalue; the array is shared, so the
    /// write is visible through every copy of the stack.</summary>
    /// <param name="stack">The stack.</param>
    /// <param name="index">The operand index.</param>
    /// <param name="value">The value to write.</param>
    /// <returns>The stack, its array updated.</returns>
    let setAt (stack: CharArgStack) (index: int) (value: double) : CharArgStack =
        stack.Values[index] <- value
        stack

/// <summary>The fixed 10-slot call stack of the CFF charstring interpreter, <c>call_stack_t</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal CharCallStack =
    private
        { Contexts: struct (int * int * int) array
          Count: int
          Error: bool }

/// <summary>Building and mutating <see cref="CharCallStack"/>. Only this module writes the array.</summary>
[<RequireQualifiedAccess>]
module internal CharCallStack =
    /// <summary>The call-stack limit, <c>kMaxCallLimit</c>.</summary>
    [<Literal>]
    let private MaxCalls = 10

    /// <summary>Creates an empty call stack with its fixed array.</summary>
    /// <returns>The empty stack.</returns>
    let create () : CharCallStack =
        { Contexts = Array.zeroCreate MaxCalls
          Count = 0
          Error = false }

    /// <summary>Pushes one saved string position; a full stack is the interpreter's error.</summary>
    /// <param name="stack">The stack.</param>
    /// <param name="start">The string's start offset in the table.</param>
    /// <param name="length">The string's length.</param>
    /// <param name="offset">The string's cursor.</param>
    /// <returns>The stack.</returns>
    let push (stack: CharCallStack) (start: int) (length: int) (offset: int) : CharCallStack =
        match stack.Count < MaxCalls && not stack.Error with
        | true ->
            stack.Contexts[stack.Count] <- struct (start, length, offset)
            { stack with Count = stack.Count + 1 }
        | false -> { stack with Error = true }

    /// <summary>Pops one saved string position; an empty stack is the interpreter's error.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>The stack and the saved position.</returns>
    let pop (stack: CharCallStack) : struct (CharCallStack * struct (int * int * int)) =
        match stack.Count > 0 && not stack.Error with
        | true -> struct ({ stack with Count = stack.Count - 1 }, stack.Contexts[stack.Count - 1])
        | false -> struct ({ stack with Error = true }, struct (0, 0, 0))

    /// <summary>The number of saved positions, <c>get_count</c>.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>The count.</returns>
    let getCount (stack: CharCallStack) : int = stack.Count

    /// <summary>Whether the stack is in error.</summary>
    /// <param name="stack">The stack.</param>
    /// <returns>True in error.</returns>
    let inError (stack: CharCallStack) : bool = stack.Error

/// <summary>The 513 argument slots of the bounds interpreter, held inline in its machine.</summary>
[<Struct; InlineArray(513)>]
type internal CffInlineArgs =
    val mutable First: double

/// <summary>The ten saved string positions of the bounds interpreter, three ints each, held inline in its machine.</summary>
[<Struct; InlineArray(30)>]
type internal CffInlineCalls =
    val mutable First: int

/// <summary>A CFF subroutine INDEX as the bounds interpreter reads it: where it starts, its count and its sizes.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal CffSubrIndex =
    { SubrStart: int
      SubrCount: uint32
      SubrOffSize: int
      SubrCountSize: int }

/// <summary>What CFF2 blending needs from the font, for the bounds interpreter; implemented by a struct so the kernel stays
/// generic and allocation-free.</summary>
type internal ICffBlendSource<'B> =
    /// <summary>Whether blending is on, <c>do_blend</c>.</summary>
    /// <param name="blend">The blend context.</param>
    /// <returns>True when the deltas are added to their defaults.</returns>
    abstract Blending : blend: 'B -> bool

    /// <summary>The region count of a data set; 0 past the data sets.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="ivs">The data set number.</param>
    /// <returns>The region count.</returns>
    abstract RegionCount : blend: 'B * ivs: int -> int

    /// <summary>One region scalar of a data set; 0 past the regions.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="ivs">The data set number.</param>
    /// <param name="index">The column index.</param>
    /// <returns>The scalar.</returns>
    abstract Scalar : blend: 'B * ivs: int * index: int -> float32

/// <summary>The answer of the bounds interpreter: the bounds of the path, how the charstring ended and the budget left.
/// <c>Valid</c> is false exactly where the reference interpreter returns none.</summary>
[<Struct; NoComparison; NoEquality>]
type internal CffBoundsResult =
    { Valid: bool
      PathOpen: bool
      MinX: double
      MinY: double
      MaxX: double
      MaxY: double
      Seac: bool
      Adx: double
      Ady: double
      BaseCode: int
      AccentCode: int
      Budget: int64 }

/// <summary>The state of one charstring run of the bounds interpreter; everything the reference interpreter threads through
/// its loop as a copied record lives here and is written in place.</summary>
[<Struct; NoComparison; NoEquality>]
type internal CffMachine =
    val mutable Args: CffInlineArgs
    val mutable Calls: CffInlineCalls
    val mutable Count: int
    val mutable CallCount: int
    val mutable Failed: bool
    val mutable Done: bool
    val mutable StrStart: int
    val mutable StrLength: int
    val mutable StrOffset: int
    val mutable PtX: double
    val mutable PtY: double
    val mutable Seac: bool
    val mutable Adx: double
    val mutable Ady: double
    val mutable BaseCode: int
    val mutable AccentCode: int
    val mutable SeenHintmask: bool
    val mutable HstemCount: int
    val mutable VstemCount: int
    val mutable HintmaskSize: int
    val mutable MaxOps: int
    val mutable Budget: int64
    val mutable Ivs: int
    val mutable SeenVsindex: bool
    val mutable SeenBlend: bool
    val mutable RegionCount: int
    val mutable PathOpen: bool
    val mutable MinX: double
    val mutable MinY: double
    val mutable MaxX: double
    val mutable MaxY: double

/// <summary>The CFF and CFF2 charstring interpreter for glyph bounds: one loop over one machine with the argument stack
/// inline, the path operators folded straight into the bounds, nothing allocated. It is the reference interpreter of
/// <c>CffCharString</c> with its extents sink, operator by operator.</summary>
[<RequireQualifiedAccess>]
module internal CffKernel =
    [<Literal>]
    let private ArgCapacity = 513

    [<Literal>]
    let private CallCapacity = 10

    [<Literal>]
    let private MaxOps = 200000

    [<Literal>]
    let private OpHstem = 1

    [<Literal>]
    let private OpVstem = 3

    [<Literal>]
    let private OpVmoveto = 4

    [<Literal>]
    let private OpRlineto = 5

    [<Literal>]
    let private OpHlineto = 6

    [<Literal>]
    let private OpVlineto = 7

    [<Literal>]
    let private OpRrcurveto = 8

    [<Literal>]
    let private OpCallsubr = 10

    [<Literal>]
    let private OpReturn = 11

    [<Literal>]
    let private OpEscape = 12

    [<Literal>]
    let private OpEndchar = 14

    [<Literal>]
    let private OpVsindex = 15

    [<Literal>]
    let private OpBlend = 16

    [<Literal>]
    let private OpHstemhm = 18

    [<Literal>]
    let private OpHintmask = 19

    [<Literal>]
    let private OpCntrmask = 20

    [<Literal>]
    let private OpRmoveto = 21

    [<Literal>]
    let private OpHmoveto = 22

    [<Literal>]
    let private OpVstemhm = 23

    [<Literal>]
    let private OpRcurveline = 24

    [<Literal>]
    let private OpRlinecurve = 25

    [<Literal>]
    let private OpVvcurveto = 26

    [<Literal>]
    let private OpHhcurveto = 27

    [<Literal>]
    let private OpShortint = 28

    [<Literal>]
    let private OpCallgsubr = 29

    [<Literal>]
    let private OpVhcurveto = 30

    [<Literal>]
    let private OpHvcurveto = 31

    [<Literal>]
    let private OpFixedcs = 255

    [<Literal>]
    let private OpDotsection = 256

    [<Literal>]
    let private OpHflex = 290

    [<Literal>]
    let private OpFlex = 291

    [<Literal>]
    let private OpHflex1 = 292

    [<Literal>]
    let private OpFlex1 = 293

    [<Literal>]
    let private OpInvalid = 0xFFFF

    /// <summary>The byte at an offset of the table, 0 outside it, <c>Binary.u8</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The offset.</param>
    /// <returns>The byte.</returns>
    let inline private byteAt (table: ReadOnlySpan<byte>) (offset: int) =
        match uint64 (uint32 offset) + 1UL <= uint64 table.Length with
        | true -> table[offset]
        | false -> 0uy

    /// <summary>The big-endian 32-bit word at an offset of the table, 0 outside it, <c>Binary.u32</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The offset.</param>
    /// <returns>The word.</returns>
    let inline private wordAt (table: ReadOnlySpan<byte>) (offset: int) =
        match uint64 (uint32 offset) + 4UL <= uint64 table.Length with
        | true -> System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(table.Slice(offset, 4))
        | false -> 0u

    /// <summary>Reads an argument without popping it, <c>cff_stack_t::operator[]</c>; an index outside the stack fails the run.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="index">The argument index.</param>
    /// <returns>The argument, or 0.</returns>
    let inline private argAt (m: byref<CffMachine>) (index: int) =
        match index >= 0 && index < m.Count && not m.Failed with
        | true -> (MemoryMarshal.CreateSpan(&m.Args.First, ArgCapacity))[index]
        | false ->
            m.Failed <- true
            0.0

    /// <summary>Pushes one number; a full stack fails the run.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="value">The number.</param>
    let private push (m: byref<CffMachine>) (value: double) =
        match m.Count < ArgCapacity && not m.Failed with
        | true ->
            let span = MemoryMarshal.CreateSpan(&m.Args.First, ArgCapacity)
            span[m.Count] <- value
            m.Count <- m.Count + 1
        | false -> m.Failed <- true

    /// <summary>Pops one number; an empty stack fails the run.</summary>
    /// <param name="m">The machine.</param>
    /// <returns>The number, or 0.</returns>
    let private pop (m: byref<CffMachine>) =
        match m.Count > 0 && not m.Failed with
        | true ->
            m.Count <- m.Count - 1
            (MemoryMarshal.CreateSpan(&m.Args.First, ArgCapacity))[m.Count]
        | false ->
            m.Failed <- true
            0.0

    /// <summary>Pops an integer operand, <c>arg_stack_t::pop_int</c>; a number outside the int range fails the run.</summary>
    /// <param name="m">The machine.</param>
    /// <returns>The integer, or 0.</returns>
    let private popInt (m: byref<CffMachine>) =
        let value = pop &m
        match m.Failed with
        | true -> 0
        | false ->
            match value >= double Int32.MinValue && value <= double Int32.MaxValue with
            | true -> int value
            | false ->
                m.Failed <- true
                0

    /// <summary>Pops an unsigned operand, <c>arg_stack_t::pop_uint</c>; a negative one fails the run.</summary>
    /// <param name="m">The machine.</param>
    /// <returns>The number, or 0.</returns>
    let private popUint (m: byref<CffMachine>) =
        let value = popInt &m
        match value < 0 with
        | true ->
            m.Failed <- true
            0
        | false -> value

    /// <summary>Advances the cursor, <c>byte_str_ref_t::inc</c>; running past the string fails the run.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="count">The number of bytes.</param>
    let private inc (m: byref<CffMachine>) (count: int) =
        m.StrOffset <- m.StrOffset + count
        m.Failed <- m.Failed || m.StrOffset > m.StrLength

    /// <summary>Takes a point into the bounds, <c>bounds_t::update</c>.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="x">The point's x.</param>
    /// <param name="y">The point's y.</param>
    let inline private update (m: byref<CffMachine>) (x: double) (y: double) =
        m.MinX <- min m.MinX x
        m.MinY <- min m.MinY y
        m.MaxX <- max m.MaxX x
        m.MaxY <- max m.MaxY y

    /// <summary>The first point of a path that is not open yet, which the bounds take as the path's start.</summary>
    /// <param name="m">The machine.</param>
    let inline private openPath (m: byref<CffMachine>) =
        match m.PathOpen with
        | false -> update &m m.PtX m.PtY
        | true -> ()

    /// <summary>A move-to by an offset: the path closes and the point moves.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="dx">The x offset.</param>
    /// <param name="dy">The y offset.</param>
    let private moveTo (m: byref<CffMachine>) (dx: double) (dy: double) =
        m.PtX <- m.PtX + dx
        m.PtY <- m.PtY + dy
        m.PathOpen <- false

    /// <summary>A line by an offset.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="dx">The x offset.</param>
    /// <param name="dy">The y offset.</param>
    let private lineTo (m: byref<CffMachine>) (dx: double) (dy: double) =
        openPath &m
        let x = m.PtX + dx
        let y = m.PtY + dy
        update &m x y
        m.PathOpen <- true
        m.PtX <- x
        m.PtY <- y

    /// <summary>A cubic curve to absolute points from the current point.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="x1">The first control point's x.</param>
    /// <param name="y1">The first control point's y.</param>
    /// <param name="x2">The second control point's x.</param>
    /// <param name="y2">The second control point's y.</param>
    /// <param name="x3">The end point's x.</param>
    /// <param name="y3">The end point's y.</param>
    let private drawCurve (m: byref<CffMachine>) (x1: double) (y1: double) (x2: double) (y2: double) (x3: double) (y3: double) =
        openPath &m
        update &m x1 y1
        update &m x2 y2
        update &m x3 y3
        m.PathOpen <- true
        m.PtX <- x3
        m.PtY <- y3

    /// <summary>A cubic curve by offsets, each from the point before it.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="dx1">The first control offset's x.</param>
    /// <param name="dy1">The first control offset's y.</param>
    /// <param name="dx2">The second control offset's x.</param>
    /// <param name="dy2">The second control offset's y.</param>
    /// <param name="dx3">The end offset's x.</param>
    /// <param name="dy3">The end offset's y.</param>
    let private curveTo (m: byref<CffMachine>) (dx1: double) (dy1: double) (dx2: double) (dy2: double) (dx3: double) (dy3: double) =
        let x1 = m.PtX + dx1
        let y1 = m.PtY + dy1
        let x2 = x1 + dx2
        let y2 = y1 + dy2
        let x3 = x2 + dx3
        let y3 = y2 + dy3
        drawCurve &m x1 y1 x2 y2 x3 y3

    /// <summary>Two curves, <c>path_procs_t::curve2</c>.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="x1">The first control point's x.</param>
    /// <param name="y1">The first control point's y.</param>
    /// <param name="x2">The second control point's x.</param>
    /// <param name="y2">The second control point's y.</param>
    /// <param name="x3">The first curve's end x.</param>
    /// <param name="y3">The first curve's end y.</param>
    /// <param name="x4">The third control point's x.</param>
    /// <param name="y4">The third control point's y.</param>
    /// <param name="x5">The fourth control point's x.</param>
    /// <param name="y5">The fourth control point's y.</param>
    /// <param name="x6">The second curve's end x.</param>
    /// <param name="y6">The second curve's end y.</param>
    let private curve2 (m: byref<CffMachine>) (x1: double) (y1: double) (x2: double) (y2: double) (x3: double) (y3: double) (x4: double) (y4: double) (x5: double) (y5: double) (x6: double) (y6: double) =
        drawCurve &m x1 y1 x2 y2 x3 y3
        drawCurve &m x4 y4 x5 y5 x6 y6

    /// <summary>The rlineto operator.</summary>
    /// <param name="m">The machine.</param>
    let private rLineTo (m: byref<CffMachine>) =
        let count = m.Count
        let mutable index = 0
        while index + 2 <= count do
            let dx = argAt &m index
            let dy = argAt &m (index + 1)
            lineTo &m dx dy
            index <- index + 2

    /// <summary>The hlineto operator: horizontal and vertical lines in turn, starting horizontal.</summary>
    /// <param name="m">The machine.</param>
    let private hLineTo (m: byref<CffMachine>) =
        let count = m.Count
        let mutable index = 0
        while index + 2 <= count do
            let dx = argAt &m index
            let dy = argAt &m (index + 1)
            lineTo &m dx 0.0
            lineTo &m 0.0 dy
            index <- index + 2
        match index < count with
        | false -> ()
        | true -> lineTo &m (argAt &m index) 0.0

    /// <summary>The vlineto operator: vertical and horizontal lines in turn, starting vertical.</summary>
    /// <param name="m">The machine.</param>
    let private vLineTo (m: byref<CffMachine>) =
        let count = m.Count
        let mutable index = 0
        while index + 2 <= count do
            let dy = argAt &m index
            let dx = argAt &m (index + 1)
            lineTo &m 0.0 dy
            lineTo &m dx 0.0
            index <- index + 2
        match index < count with
        | false -> ()
        | true -> lineTo &m 0.0 (argAt &m index)

    /// <summary>One curve of six consecutive arguments.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="index">The first argument.</param>
    let private sixArgCurve (m: byref<CffMachine>) (index: int) =
        let dx1 = argAt &m index
        let dy1 = argAt &m (index + 1)
        let dx2 = argAt &m (index + 2)
        let dy2 = argAt &m (index + 3)
        let dx3 = argAt &m (index + 4)
        let dy3 = argAt &m (index + 5)
        curveTo &m dx1 dy1 dx2 dy2 dx3 dy3

    /// <summary>The rrcurveto operator.</summary>
    /// <param name="m">The machine.</param>
    let private rrCurveTo (m: byref<CffMachine>) =
        let count = m.Count
        let mutable index = 0
        while index + 6 <= count do
            sixArgCurve &m index
            index <- index + 6

    /// <summary>The rcurveline operator: curves, then one line.</summary>
    /// <param name="m">The machine.</param>
    let private rCurveLine (m: byref<CffMachine>) =
        let count = m.Count
        match count < 8 with
        | true -> ()
        | false ->
            let limit = count - 2
            let mutable index = 0
            while index + 6 <= limit do
                sixArgCurve &m index
                index <- index + 6
            let dx = argAt &m index
            let dy = argAt &m (index + 1)
            lineTo &m dx dy

    /// <summary>The rlinecurve operator: lines, then one curve.</summary>
    /// <param name="m">The machine.</param>
    let private rLineCurve (m: byref<CffMachine>) =
        let count = m.Count
        match count < 8 with
        | true -> ()
        | false ->
            let limit = count - 6
            let mutable index = 0
            while index + 2 <= limit do
                let dx = argAt &m index
                let dy = argAt &m (index + 1)
                lineTo &m dx dy
                index <- index + 2
            sixArgCurve &m index

    /// <summary>The vvcurveto operator; an odd argument count makes the first control point horizontal.</summary>
    /// <param name="m">The machine.</param>
    let private vvCurveTo (m: byref<CffMachine>) =
        let count = m.Count
        let odd = (count &&& 1) <> 0
        let mutable dx1 = (match odd with | true -> argAt &m 0 | false -> 0.0)
        let mutable index = (match odd with | true -> 1 | false -> 0)
        while index + 4 <= count do
            let dy1 = argAt &m index
            let dx2 = argAt &m (index + 1)
            let dy2 = argAt &m (index + 2)
            let dy3 = argAt &m (index + 3)
            curveTo &m dx1 dy1 dx2 dy2 0.0 dy3
            dx1 <- 0.0
            index <- index + 4

    /// <summary>The hhcurveto operator; an odd argument count makes the first control point vertical.</summary>
    /// <param name="m">The machine.</param>
    let private hhCurveTo (m: byref<CffMachine>) =
        let count = m.Count
        let odd = (count &&& 1) <> 0
        let mutable dy1 = (match odd with | true -> argAt &m 0 | false -> 0.0)
        let mutable index = (match odd with | true -> 1 | false -> 0)
        while index + 4 <= count do
            let dx1 = argAt &m index
            let dx2 = argAt &m (index + 1)
            let dy2 = argAt &m (index + 2)
            let dx3 = argAt &m (index + 3)
            curveTo &m dx1 dy1 dx2 dy2 dx3 0.0
            dy1 <- 0.0
            index <- index + 4

    /// <summary>The vhcurveto operator: curves starting vertical, then horizontal, in turn.</summary>
    /// <param name="m">The machine.</param>
    let private vhCurveTo (m: byref<CffMachine>) =
        let count = m.Count
        match count % 8 >= 4 with
        | false ->
            let mutable index = 0
            while index + 8 <= count do
                let e0 = argAt &m index
                let e1 = argAt &m (index + 1)
                let e2 = argAt &m (index + 2)
                let e3 = argAt &m (index + 3)
                let e4 = argAt &m (index + 4)
                let e5 = argAt &m (index + 5)
                let e6 = argAt &m (index + 6)
                let e7 = argAt &m (index + 7)
                let y1 = m.PtY + e0
                let x2 = m.PtX + e1
                let y2 = y1 + e2
                let x3 = x2 + e3
                drawCurve &m m.PtX y1 x2 y2 x3 y2
                let x1 = x3 + e4
                let x2 = x1 + e5
                let y2 = y2 + e6
                let y3 = y2 + e7
                let x3 = (match count - index < 16 && (count &&& 1) <> 0 with | true -> x2 + argAt &m (index + 8) | false -> x2)
                drawCurve &m x1 m.PtY x2 y2 x3 y3
                index <- index + 8
        | true ->
            let e0 = argAt &m 0
            let e1 = argAt &m 1
            let e2 = argAt &m 2
            let e3 = argAt &m 3
            let mutable x1 = m.PtX
            let mutable y1 = m.PtY + e0
            let mutable x2 = x1 + e1
            let mutable y2 = y1 + e2
            let mutable x3 = x2 + e3
            let mutable y3 = y2
            let mutable index = 4
            while index + 8 <= count do
                drawCurve &m x1 y1 x2 y2 x3 y3
                let e0 = argAt &m index
                let e1 = argAt &m (index + 1)
                let e2 = argAt &m (index + 2)
                let e3 = argAt &m (index + 3)
                let e4 = argAt &m (index + 4)
                let e5 = argAt &m (index + 5)
                let e6 = argAt &m (index + 6)
                let e7 = argAt &m (index + 7)
                let ax1 = m.PtX + e0
                let ay1 = m.PtY
                let ax2 = ax1 + e1
                let ay2 = ay1 + e2
                drawCurve &m ax1 ay1 ax2 ay2 ax2 (ay2 + e3)
                x1 <- ax2
                y1 <- ay2 + e3 + e4
                x2 <- x1 + e5
                y2 <- y1 + e6
                x3 <- x2 + e7
                y3 <- y2
                index <- index + 8
            let y3 = (match index < count with | true -> y3 + argAt &m index | false -> y3)
            drawCurve &m x1 y1 x2 y2 x3 y3

    /// <summary>The hvcurveto operator: curves starting horizontal, then vertical, in turn.</summary>
    /// <param name="m">The machine.</param>
    let private hvCurveTo (m: byref<CffMachine>) =
        let count = m.Count
        match count % 8 >= 4 with
        | false ->
            let mutable index = 0
            while index + 8 <= count do
                let e0 = argAt &m index
                let e1 = argAt &m (index + 1)
                let e2 = argAt &m (index + 2)
                let e3 = argAt &m (index + 3)
                let e4 = argAt &m (index + 4)
                let e5 = argAt &m (index + 5)
                let e6 = argAt &m (index + 6)
                let e7 = argAt &m (index + 7)
                let x1 = m.PtX + e0
                let y1 = m.PtY
                let x2 = x1 + e1
                let y2 = y1 + e2
                let y3 = y2 + e3
                drawCurve &m x1 y1 x2 y2 x2 y3
                let y1 = y3 + e4
                let x2 = x2 + e5
                let y2 = y1 + e6
                let x3 = x2 + e7
                let y3 = (match count - index < 16 && (count &&& 1) <> 0 with | true -> y2 + argAt &m (index + 8) | false -> y2)
                drawCurve &m m.PtX y1 x2 y2 x3 y3
                index <- index + 8
        | true ->
            let e0 = argAt &m 0
            let e1 = argAt &m 1
            let e2 = argAt &m 2
            let e3 = argAt &m 3
            let mutable x1 = m.PtX + e0
            let mutable y1 = m.PtY
            let mutable x2 = x1 + e1
            let mutable y2 = y1 + e2
            let mutable x3 = x2
            let mutable y3 = y2 + e3
            let mutable index = 4
            while index + 8 <= count do
                drawCurve &m x1 y1 x2 y2 x3 y3
                let e0 = argAt &m index
                let e1 = argAt &m (index + 1)
                let e2 = argAt &m (index + 2)
                let e3 = argAt &m (index + 3)
                let e4 = argAt &m (index + 4)
                let e5 = argAt &m (index + 5)
                let e6 = argAt &m (index + 6)
                let e7 = argAt &m (index + 7)
                let ax1 = m.PtX
                let ay1 = m.PtY + e0
                let ax2 = ax1 + e1
                let ay2 = ay1 + e2
                let ax3 = ax2 + e3
                drawCurve &m ax1 ay1 ax2 ay2 ax3 ay2
                x1 <- ax3 + e4
                y1 <- ay2
                x2 <- x1 + e5
                y2 <- y1 + e6
                x3 <- x2
                y3 <- y2 + e7
                index <- index + 8
            let x3 = (match index < count with | true -> x3 + argAt &m index | false -> x3)
            drawCurve &m x1 y1 x2 y2 x3 y3

    /// <summary>The hflex operator.</summary>
    /// <param name="m">The machine.</param>
    let private hFlex (m: byref<CffMachine>) =
        match m.Count = 7 with
        | false -> m.Failed <- true
        | true ->
            let d1 = argAt &m 0
            let d2 = argAt &m 1
            let d3 = argAt &m 2
            let d4 = argAt &m 3
            let d5 = argAt &m 4
            let d6 = argAt &m 5
            let d7 = argAt &m 6
            let x1 = m.PtX + d1
            let y1 = m.PtY
            let x2 = x1 + d2
            let y2 = y1 + d3
            let x3 = x2 + d4
            let x4 = x3 + d5
            let x5 = x4 + d6
            let x6 = x5 + d7
            curve2 &m x1 y1 x2 y2 x3 y2 x4 y2 x5 y1 x6 y1

    /// <summary>The flex operator.</summary>
    /// <param name="m">The machine.</param>
    let private flex (m: byref<CffMachine>) =
        match m.Count = 13 with
        | false -> m.Failed <- true
        | true ->
            let a0 = argAt &m 0
            let a1 = argAt &m 1
            let a2 = argAt &m 2
            let a3 = argAt &m 3
            let a4 = argAt &m 4
            let a5 = argAt &m 5
            let a6 = argAt &m 6
            let a7 = argAt &m 7
            let a8 = argAt &m 8
            let a9 = argAt &m 9
            let a10 = argAt &m 10
            let a11 = argAt &m 11
            let x1 = m.PtX + a0
            let y1 = m.PtY + a1
            let x2 = x1 + a2
            let y2 = y1 + a3
            let x3 = x2 + a4
            let y3 = y2 + a5
            let x4 = x3 + a6
            let y4 = y3 + a7
            let x5 = x4 + a8
            let y5 = y4 + a9
            let x6 = x5 + a10
            let y6 = y5 + a11
            curve2 &m x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6

    /// <summary>The hflex1 operator.</summary>
    /// <param name="m">The machine.</param>
    let private hFlex1 (m: byref<CffMachine>) =
        match m.Count = 9 with
        | false -> m.Failed <- true
        | true ->
            let a0 = argAt &m 0
            let a1 = argAt &m 1
            let a2 = argAt &m 2
            let a3 = argAt &m 3
            let a4 = argAt &m 4
            let a5 = argAt &m 5
            let a6 = argAt &m 6
            let a7 = argAt &m 7
            let a8 = argAt &m 8
            let x1 = m.PtX + a0
            let y1 = m.PtY + a1
            let x2 = x1 + a2
            let y2 = y1 + a3
            let x3 = x2 + a4
            let x4 = x3 + a5
            let x5 = x4 + a6
            let y5 = y2 + a7
            let x6 = x5 + a8
            curve2 &m x1 y1 x2 y2 x3 y2 x4 y2 x5 y5 x6 m.PtY

    /// <summary>The flex1 operator.</summary>
    /// <param name="m">The machine.</param>
    let private flex1 (m: byref<CffMachine>) =
        match m.Count = 11 with
        | false -> m.Failed <- true
        | true ->
            let a0 = argAt &m 0
            let a1 = argAt &m 1
            let a2 = argAt &m 2
            let a3 = argAt &m 3
            let a4 = argAt &m 4
            let a5 = argAt &m 5
            let a6 = argAt &m 6
            let a7 = argAt &m 7
            let a8 = argAt &m 8
            let a9 = argAt &m 9
            let a10 = argAt &m 10
            let sumX = a0 + a2 + a4 + a6 + a8
            let sumY = a1 + a3 + a5 + a7 + a9
            let x1 = m.PtX + a0
            let y1 = m.PtY + a1
            let x2 = x1 + a2
            let y2 = y1 + a3
            let x3 = x2 + a4
            let y3 = y2 + a5
            let x4 = x3 + a6
            let y4 = y3 + a7
            let x5 = x4 + a8
            let y5 = y4 + a9
            match abs sumX > abs sumY with
            | true -> curve2 &m x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 (x5 + a10) m.PtY
            | false -> curve2 &m x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 m.PtX (y5 + a10)

    /// <summary>The subroutine bias, <c>biased_subrs_t</c>.</summary>
    /// <param name="count">The subroutine count.</param>
    /// <returns>The bias.</returns>
    let private subrBias (count: uint32) =
        match count < 1240u with
        | true -> 107
        | false ->
            match count < 33900u with
            | true -> 1131
            | false -> 32768

    /// <summary>One INDEX offset, big-endian over its offset size, 0 outside the table, <c>CFFIndex</c>'s offset reader.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="subrs">The INDEX.</param>
    /// <param name="index">The offset's index.</param>
    /// <returns>The offset.</returns>
    let private indexOffset (table: ReadOnlySpan<byte>) (subrs: CffSubrIndex) (index: int) =
        let first = subrs.SubrStart + subrs.SubrCountSize + 1 + index * subrs.SubrOffSize
        let mutable value = 0u
        let mutable byteIndex = 0
        while byteIndex < subrs.SubrOffSize do
            value <- (value <<< 8) ||| uint32 (byteAt table (first + byteIndex))
            byteIndex <- byteIndex + 1
        value

    /// <summary>Calls a subroutine, <c>cs_interp_env_t::call_subr</c>.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="table">The table bytes.</param>
    /// <param name="subrs">The subroutine INDEX.</param>
    let private callSubr (m: byref<CffMachine>) (table: ReadOnlySpan<byte>) (subrs: CffSubrIndex) =
        let number = popInt &m
        let biased = number + subrBias subrs.SubrCount
        match biased < 0 || uint32 biased >= subrs.SubrCount || m.CallCount >= CallCapacity || m.Failed with
        | true -> m.Failed <- true
        | false ->
            let first = indexOffset table subrs biased
            let second = indexOffset table subrs (biased + 1)
            let last = indexOffset table subrs (int subrs.SubrCount)
            let dataBase = subrs.SubrStart + subrs.SubrCountSize + subrs.SubrOffSize * int (subrs.SubrCount + 1u)
            let saved = MemoryMarshal.CreateSpan(&m.Calls.First, CallCapacity * 3)
            saved[m.CallCount * 3] <- m.StrStart
            saved[m.CallCount * 3 + 1] <- m.StrLength
            saved[m.CallCount * 3 + 2] <- m.StrOffset
            m.CallCount <- m.CallCount + 1
            match second < first || second > last with
            | true ->
                m.StrStart <- 0
                m.StrLength <- 0
            | false ->
                m.StrStart <- dataBase + int first
                m.StrLength <- int (second - first)
            m.StrOffset <- 0

    /// <summary>Returns from a subroutine, <c>cs_interp_env_t::return_from_subr</c>; an empty call stack fails the run.</summary>
    /// <param name="m">The machine.</param>
    let private returnFromSubr (m: byref<CffMachine>) =
        match m.CallCount > 0 && not m.Failed with
        | false -> m.Failed <- true
        | true ->
            m.CallCount <- m.CallCount - 1
            let saved = MemoryMarshal.CreateSpan(&m.Calls.First, CallCapacity * 3)
            m.StrStart <- saved[m.CallCount * 3]
            m.StrLength <- saved[m.CallCount * 3 + 1]
            m.StrOffset <- saved[m.CallCount * 3 + 2]

    /// <summary>Saturates a number to an int, <c>number_t::to_int</c>.</summary>
    /// <param name="value">The number.</param>
    /// <returns>The int.</returns>
    let private toIntClamped (value: double) =
        match value >= double Int32.MinValue && value <= double Int32.MaxValue with
        | true -> int value
        | false ->
            match value < 0.0 with
            | true -> Int32.MinValue
            | false ->
                match value > 0.0 with
                | true -> Int32.MaxValue
                | false -> 0

    /// <summary>The endchar operator; a CFF1 charstring with four or more operands ends as a seac.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="cff2">Whether the charstring is CFF2.</param>
    let private endchar (m: byref<CffMachine>) (cff2: bool) =
        let count = m.Count
        match cff2 || count < 4 with
        | true -> ()
        | false ->
            m.Seac <- true
            m.Adx <- argAt &m (count - 4)
            m.Ady <- argAt &m (count - 3)
            m.BaseCode <- toIntClamped (argAt &m (count - 2))
            m.AccentCode <- toIntClamped (argAt &m (count - 1))
        m.Count <- 0
        m.Done <- true

    /// <summary>The hintmask and cntrmask operators, <c>determine_hintmask_size</c> and the mask bytes.</summary>
    /// <param name="m">The machine.</param>
    let private hintmask (m: byref<CffMachine>) =
        match m.SeenHintmask with
        | true -> ()
        | false ->
            m.VstemCount <- m.VstemCount + m.Count / 2
            m.HintmaskSize <- (m.HstemCount + m.VstemCount + 7) >>> 3
            m.SeenHintmask <- true
        match m.StrOffset + m.HintmaskSize <= m.StrLength with
        | false -> ()
        | true ->
            m.Count <- 0
            inc &m m.HintmaskSize

    /// <summary>The vsindex operator, <c>cff2_cs_interp_env_t::process_vsindex</c>.</summary>
    /// <param name="m">The machine.</param>
    let private vsindex (m: byref<CffMachine>) =
        let index = popUint &m
        match m.SeenVsindex || m.SeenBlend with
        | true -> m.Failed <- true
        | false -> m.Ivs <- index
        m.Count <- 0
        m.SeenVsindex <- true

    /// <summary>The blend operator, <c>cff2_cs_opset_t::process_blend</c>, in double.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="m">The machine.</param>
    let private blendOp<'B when 'B : struct and 'B :> ICffBlendSource<'B>> (blend: 'B) (m: byref<CffMachine>) =
        match m.SeenBlend with
        | true -> ()
        | false ->
            m.SeenBlend <- true
            m.RegionCount <- blend.RegionCount(blend, m.Ivs)
        let k = m.RegionCount
        let n = popUint &m
        let count = m.Count
        let total64 = int64 (k + 1) * int64 n
        match m.Failed || total64 > int64 UInt32.MaxValue || total64 > int64 count with
        | true -> m.Failed <- true
        | false ->
            let total = int total64
            let budget = m.Budget - int64 (total - n)
            match budget < 0L with
            | true -> m.Failed <- true
            | false ->
                let start = count - total
                let blending = blend.Blending(blend)
                let span = MemoryMarshal.CreateSpan(&m.Args.First, ArgCapacity)
                let mutable index = 0
                while index < n do
                    let deltasAt = start + n + index * k
                    let mutable acc = 0.0
                    let mutable column = 0
                    while blending && column < k do
                        acc <- acc + double (blend.Scalar(blend, m.Ivs, column)) * span[deltasAt + column]
                        column <- column + 1
                    span[start + index] <- span[start + index] + acc
                    index <- index + 1
                m.Count <- count - k * n
                m.Budget <- budget

    /// <summary>Pushes a number operand that reads bytes after its operator, <c>opset_t::process_op</c>.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="table">The table bytes.</param>
    /// <param name="opcode">The operator: a shortint or one of the 247 to 254 range.</param>
    let private pushNumber (m: byref<CffMachine>) (table: ReadOnlySpan<byte>) (opcode: int) =
        match opcode with
        | OpShortint ->
            match m.StrOffset + 2 <= m.StrLength with
            | false -> m.Failed <- true
            | true ->
                let at = m.StrStart + m.StrOffset
                push &m (double (int (int16 ((uint16 (byteAt table at) <<< 8) ||| uint16 (byteAt table (at + 1))))))
                inc &m 2
        | 247 | 248 | 249 | 250 ->
            match m.StrOffset + 1 <= m.StrLength with
            | false -> m.Failed <- true
            | true ->
                push &m (double ((opcode - 247) * 256 + int (byteAt table (m.StrStart + m.StrOffset)) + 108))
                inc &m 1
        | _ ->
            match m.StrOffset + 1 <= m.StrLength with
            | false -> m.Failed <- true
            | true ->
                push &m (double (-(opcode - 251) * 256 - int (byteAt table (m.StrStart + m.StrOffset)) - 108))
                inc &m 1

    /// <summary>Pushes the fixed operand of the fixedcs operator, <c>arg_stack_t::push_fixed</c>; too few bytes leave the
    /// machine as it is.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="table">The table bytes.</param>
    let private pushFixed (m: byref<CffMachine>) (table: ReadOnlySpan<byte>) =
        match m.StrOffset + 4 <= m.StrLength with
        | false -> ()
        | true ->
            push &m (double (int (int32 (wordAt table (m.StrStart + m.StrOffset)))) / 65536.0)
            inc &m 4

    /// <summary>Fetches the next operator, <c>interp_env_t::fetch_op</c>; CFF2 makes up an endchar or a return when the bytes
    /// run out.</summary>
    /// <param name="m">The machine.</param>
    /// <param name="table">The table bytes.</param>
    /// <param name="cff2">Whether the charstring is CFF2.</param>
    /// <returns>The operator.</returns>
    let private fetchOp (m: byref<CffMachine>) (table: ReadOnlySpan<byte>) (cff2: bool) =
        match m.StrOffset + 1 > m.StrLength with
        | true ->
            match cff2, m.CallCount with
            | false, _ -> OpInvalid
            | true, 0 -> OpEndchar
            | true, _ -> OpReturn
        | false ->
            let opcode = int (byteAt table (m.StrStart + m.StrOffset))
            m.StrOffset <- m.StrOffset + 1
            match opcode = OpEscape with
            | false -> opcode
            | true ->
                match m.StrOffset + 1 > m.StrLength with
                | true -> OpInvalid
                | false ->
                    let escaped = 256 + int (byteAt table (m.StrStart + m.StrOffset))
                    m.StrOffset <- m.StrOffset + 1
                    escaped

    /// <summary>Processes one operator that reads no bytes after itself, <c>cs_opset_t::process_op</c>; an operator the
    /// interpreter ignores clears the arguments.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="cff2">Whether the charstring is CFF2.</param>
    /// <param name="m">The machine.</param>
    /// <param name="opcode">The operator.</param>
    let private processOp<'B when 'B : struct and 'B :> ICffBlendSource<'B>> (blend: 'B) (cff2: bool) (m: byref<CffMachine>) (opcode: int) =
        match opcode with
        | OpDotsection ->
            match cff2 with
            | true -> m.Count <- 0
            | false -> ()
        | OpEndchar -> endchar &m cff2
        | OpReturn -> returnFromSubr &m
        | OpVsindex ->
            match cff2 with
            | true -> vsindex &m
            | false -> m.Count <- 0
        | OpBlend ->
            match cff2 with
            | true -> blendOp blend &m
            | false -> m.Count <- 0
        | OpHstem | OpHstemhm ->
            m.HstemCount <- m.HstemCount + m.Count / 2
            m.Count <- 0
        | OpVstem | OpVstemhm ->
            m.VstemCount <- m.VstemCount + m.Count / 2
            m.Count <- 0
        | OpHintmask | OpCntrmask -> hintmask &m
        | OpRmoveto ->
            let dy = pop &m
            let dx = pop &m
            moveTo &m dx dy
            m.Count <- 0
        | OpHmoveto ->
            moveTo &m (pop &m) 0.0
            m.Count <- 0
        | OpVmoveto ->
            moveTo &m 0.0 (pop &m)
            m.Count <- 0
        | OpRlineto ->
            rLineTo &m
            m.Count <- 0
        | OpHlineto ->
            hLineTo &m
            m.Count <- 0
        | OpVlineto ->
            vLineTo &m
            m.Count <- 0
        | OpRrcurveto ->
            rrCurveTo &m
            m.Count <- 0
        | OpRcurveline ->
            rCurveLine &m
            m.Count <- 0
        | OpRlinecurve ->
            rLineCurve &m
            m.Count <- 0
        | OpVvcurveto ->
            vvCurveTo &m
            m.Count <- 0
        | OpHhcurveto ->
            hhCurveTo &m
            m.Count <- 0
        | OpVhcurveto ->
            vhCurveTo &m
            m.Count <- 0
        | OpHvcurveto ->
            hvCurveTo &m
            m.Count <- 0
        | OpHflex ->
            hFlex &m
            m.Count <- 0
        | OpFlex ->
            flex &m
            m.Count <- 0
        | OpHflex1 ->
            hFlex1 &m
            m.Count <- 0
        | OpFlex1 ->
            flex1 &m
            m.Count <- 0
        | _ -> m.Count <- 0

    /// <summary>Runs the machine to the end of its charstring, <c>cs_interpreter_t::interpret</c>.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="cff2">Whether the charstring is CFF2.</param>
    /// <param name="table">The table bytes.</param>
    /// <param name="globals">The global subroutine INDEX.</param>
    /// <param name="locals">The local subroutine INDEX.</param>
    /// <param name="m">The machine.</param>
    let private run<'B when 'B : struct and 'B :> ICffBlendSource<'B>> (blend: 'B) (cff2: bool) (table: ReadOnlySpan<byte>) (globals: CffSubrIndex) (locals: CffSubrIndex) (m: byref<CffMachine>) =
        while not m.Failed && not m.Done do
            let opcode = fetchOp &m table cff2
            match opcode with
            | OpCallsubr -> callSubr &m table locals
            | OpCallgsubr -> callSubr &m table globals
            | OpFixedcs -> pushFixed &m table
            | OpShortint -> pushNumber &m table opcode
            | opcode when opcode >= 247 && opcode <= 254 -> pushNumber &m table opcode
            | opcode when opcode >= 32 && opcode <= 246 -> push &m (double (opcode - 139))
            | opcode -> processOp blend cff2 &m opcode
            m.MaxOps <- m.MaxOps - 1
            match m.MaxOps = 0 with
            | true -> m.Failed <- true
            | false -> ()

    /// <summary>Interprets one charstring into its bounds.</summary>
    /// <param name="blend">The blend context; the CFF1 context for a CFF1 charstring.</param>
    /// <param name="cff2">Whether the charstring is CFF2.</param>
    /// <param name="ivs">The font dict's initial data set number.</param>
    /// <param name="table">The table bytes.</param>
    /// <param name="charStart">The charstring's offset in the table.</param>
    /// <param name="charLength">The charstring's length.</param>
    /// <param name="globals">The global subroutine INDEX.</param>
    /// <param name="locals">The local subroutine INDEX.</param>
    /// <param name="budget">The remaining glyph budget.</param>
    /// <returns>The bounds, the ending and the budget left, invalid exactly where the reference interpreter returns none.</returns>
    let interpret<'B when 'B : struct and 'B :> ICffBlendSource<'B>> (blend: 'B) (cff2: bool) (ivs: int) (table: ReadOnlySpan<byte>) (charStart: int) (charLength: int) (globals: CffSubrIndex) (locals: CffSubrIndex) (budget: int64) =
        let startOps = match budget < 0L with | true -> 0 | false -> int (min budget (int64 MaxOps))
        let mutable m = Unchecked.defaultof<CffMachine>
        m.StrStart <- charStart
        m.StrLength <- charLength
        m.MaxOps <- startOps
        m.Budget <- budget
        m.Ivs <- ivs
        m.MinX <- double Int32.MaxValue
        m.MinY <- double Int32.MaxValue
        m.MaxX <- double Int32.MinValue
        m.MaxY <- double Int32.MinValue
        m.Failed <- startOps = 0
        run blend cff2 table globals locals &m
        { Valid = not m.Failed
          PathOpen = m.PathOpen
          MinX = m.MinX
          MinY = m.MinY
          MaxX = m.MaxX
          MaxY = m.MaxY
          Seac = m.Seac
          Adx = m.Adx
          Ady = m.Ady
          BaseCode = m.BaseCode
          AccentCode = m.AccentCode
          Budget = m.Budget - int64 (startOps - m.MaxOps) }

/// <summary>The code point ranges of a cmap subtable, in their stored order, for building a lookup table in one pass.</summary>
type internal ICmapRanges =
    /// <summary>The number of ranges.</summary>
    /// <param name="data">The cmap bytes.</param>
    /// <returns>The count.</returns>
    abstract Count : data: ReadOnlySpan<byte> -> int

    /// <summary>The first and last code point of a range.</summary>
    /// <param name="data">The cmap bytes.</param>
    /// <param name="index">The range.</param>
    /// <returns>The first and the last code point.</returns>
    abstract Bounds : data: ReadOnlySpan<byte> * index: int -> struct (uint32 * uint32)

    /// <summary>Whether a range gives glyphs in a straight line, and where the line starts: the glyph of a code point
    /// <c>c</c> is then <c>(start + step * (c - first)) &amp;&amp; mask</c>.</summary>
    /// <param name="data">The cmap bytes.</param>
    /// <param name="index">The range.</param>
    /// <returns>Whether the range is a straight line, the glyph at its first code point, the step and the mask.</returns>
    abstract Line : data: ReadOnlySpan<byte> * index: int -> struct (bool * uint32 * uint32 * uint32)

    /// <summary>The glyph a range that is not a straight line gives a code point inside it; 0 for none.</summary>
    /// <param name="data">The cmap bytes.</param>
    /// <param name="index">The range.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph, or 0.</returns>
    abstract Glyph : data: ReadOnlySpan<byte> * index: int * codepoint: uint32 -> uint32

/// <summary>Building a code point to glyph table from the ranges of a cmap subtable in one pass.</summary>
[<RequireQualifiedAccess>]
module internal CmapTable =
    /// <summary>Whether the ranges are in order and apart: each one starts after the one before ends and ends no earlier than
    /// it starts. Only then does a binary search find the one range that holds a code point.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="data">The cmap bytes.</param>
    /// <returns>True when ordered and disjoint.</returns>
    let private ordered<'R when 'R : struct and 'R :> ICmapRanges> (ranges: 'R) (data: ReadOnlySpan<byte>) =
        let count = ranges.Count data
        let mutable index = 0
        let mutable previousLast = -1L
        let mutable sorted = true
        while sorted && index < count do
            let struct (first, last) = ranges.Bounds(data, index)
            sorted <- int64 first <= int64 last && int64 first > previousLast
            previousLast <- int64 last
            index <- index + 1
        sorted

    /// <summary>The glyphs of the first <paramref name="limit"/> code points, 0 where there is none, read range by range
    /// instead of searching for each code point; none when the ranges are not ordered and disjoint.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="data">The cmap bytes.</param>
    /// <param name="limit">The number of code points.</param>
    /// <returns>The table, or none when the ranges are not in order.</returns>
    let lowFromRanges<'R when 'R : struct and 'R :> ICmapRanges> (ranges: 'R) (data: ReadOnlySpan<byte>) (limit: int) =
        match ordered ranges data with
        | false -> ValueNone
        | true ->
            let low = Array.zeroCreate<uint32> limit
            let count = ranges.Count data
            let mutable index = 0
            while index < count && (let struct (first, _) = ranges.Bounds(data, index) in int64 first < int64 limit) do
                let struct (firstPoint, lastPoint) = ranges.Bounds(data, index)
                let first = int firstPoint
                let last = int (min (int64 lastPoint) (int64 limit - 1L))
                let struct (straight, start, step, mask) = ranges.Line(data, index)
                let mutable codepoint = first
                while codepoint <= last do
                    low[codepoint] <-
                        match straight with
                        | true -> (start + step * uint32 (codepoint - first)) &&& mask
                        | false -> ranges.Glyph(data, index, uint32 codepoint)
                    codepoint <- codepoint + 1
                index <- index + 1
            ValueSome (ImmutableCollectionsMarshal.AsImmutableArray low)

    /// <summary>The first and last code points of every range, for every block of 256 code points the first range that does
    /// not end before it, and for ranges that all give glyphs in a straight line, where each line starts and its step; for
    /// finding the glyph of a code point without a search. All arrays are empty when the ranges are not in order and apart,
    /// where only a search finds what a search finds, and the last two when some range is not a straight line.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="data">The cmap bytes.</param>
    /// <param name="blocks">The number of blocks of 256 code points to index.</param>
    /// <returns>The firsts, the lasts, the block starts, the line starts and the line steps.</returns>
    let index<'R when 'R : struct and 'R :> ICmapRanges> (ranges: 'R) (data: ReadOnlySpan<byte>) (blocks: int) =
        match ordered ranges data with
        | false -> struct (ImmutableArray<uint32>.Empty, ImmutableArray<uint32>.Empty, ImmutableArray<int>.Empty, ImmutableArray<uint32>.Empty, ImmutableArray<uint32>.Empty)
        | true ->
            let count = ranges.Count data
            let firsts = Array.zeroCreate<uint32> count
            let lasts = Array.zeroCreate<uint32> count
            let lines = Array.zeroCreate<uint32> count
            let steps = Array.zeroCreate<uint32> count
            let starts = Array.zeroCreate<int> blocks
            let mutable straight = true
            let mutable position = 0
            while position < count do
                let struct (first, last) = ranges.Bounds(data, position)
                let struct (isLine, start, step, mask) = ranges.Line(data, position)
                firsts[position] <- first
                lasts[position] <- last
                lines[position] <- start
                steps[position] <- step
                straight <- straight && isLine && mask = UInt32.MaxValue
                position <- position + 1
            let mutable block = 0
            let mutable range = 0
            while block < blocks do
                while range < count && int64 lasts[range] < int64 block * 256L do
                    range <- range + 1
                starts[block] <- range
                block <- block + 1
            struct (ImmutableCollectionsMarshal.AsImmutableArray firsts,
                    ImmutableCollectionsMarshal.AsImmutableArray lasts,
                    ImmutableCollectionsMarshal.AsImmutableArray starts,
                    (match straight with | true -> ImmutableCollectionsMarshal.AsImmutableArray lines | false -> ImmutableArray<uint32>.Empty),
                    (match straight with | true -> ImmutableCollectionsMarshal.AsImmutableArray steps | false -> ImmutableArray<uint32>.Empty))

/// <summary>The cell a face holds for its accelerators: empty when the face is made, filled once by whoever needs the
/// accelerators first, then read by every font of the face. Equality ignores it, so faces compare by their tables.</summary>
[<Sealed; AllowNullLiteral>]
type internal AccelSlot() =
    let mutable cell: obj | null = null

    /// <summary>Returns the accelerators, creating them on first use; of two racing creators one wins and both get its value.</summary>
    /// <param name="create">Creates the accelerators.</param>
    /// <returns>The accelerators.</returns>
    member _.GetOrAdd (create: unit -> obj) =
        match cell with
        | null ->
            let created = create ()
            match System.Threading.Interlocked.CompareExchange(&cell, created, null) with
            | null -> created
            | existing -> existing
        | existing -> existing

    override _.Equals other =
        match other with
        | :? AccelSlot -> true
        | _ -> false

    override _.GetHashCode () = 0

/// <summary>A small cache of values built for a key, shared by every user of the cell that holds it. It is empty when made; a value is
/// built the first time its key is asked for and kept as one of the newest <c>capacity</c> keys, the oldest being dropped when a newer
/// key arrives. Of two racing builders of one key one value wins and both get it. Equality ignores it, so the records that hold one compare
/// by their other fields.</summary>
/// <typeparam name="'K">The key type.</typeparam>
/// <typeparam name="'V">The value type.</typeparam>
[<Sealed; AllowNullLiteral>]
type internal KeyedSlot<'K, 'V when 'K : struct and 'K :> IEquatable<'K>>(capacity: int) =
    let mutable entries: struct ('K * 'V) array = Array.empty

    /// <summary>Finds a key among entries.</summary>
    /// <param name="items">The entries.</param>
    /// <param name="key">The key.</param>
    /// <param name="index">The next entry.</param>
    /// <returns>The entry's value, or none.</returns>
    let rec find (items: struct ('K * 'V) array) (key: 'K) (index: int) =
        match index < items.Length with
        | false -> ValueNone
        | true ->
            let struct (held, value) = items[index]
            match held.Equals key with
            | true -> ValueSome value
            | false -> find items key (index + 1)

    /// <summary>Publishes a built value as the newest entry, unless another builder published the key first.</summary>
    /// <param name="seen">The entries the caller saw before building.</param>
    /// <param name="key">The key.</param>
    /// <param name="created">The built value.</param>
    /// <returns>The value the cell holds for the key.</returns>
    member private this.Publish (seen: struct ('K * 'V) array, key: 'K, created: 'V) =
        let kept = Array.sub seen 0 (min seen.Length (capacity - 1))
        let next = Array.append [| struct (key, created) |] kept
        let now = System.Threading.Interlocked.CompareExchange(&entries, next, seen)
        match obj.ReferenceEquals(now, seen) with
        | true -> created
        | false ->
            match find now key 0 with
            | ValueSome value -> value
            | ValueNone -> this.Publish (now, key, created)

    /// <summary>The value held for a key, without building one.</summary>
    /// <param name="key">The key.</param>
    /// <returns>The value, or none.</returns>
    member this.TryFind (key: 'K) =
        find (System.Threading.Volatile.Read(&entries)) key 0

    /// <summary>Keeps a value built for a key, unless another builder kept one first.</summary>
    /// <param name="key">The key.</param>
    /// <param name="created">The built value.</param>
    /// <returns>The value the cell holds for the key.</returns>
    member this.Add (key: 'K, created: 'V) =
        this.Publish (System.Threading.Volatile.Read(&entries), key, created)

    /// <summary>Returns the value for a key, building it on first use.</summary>
    /// <param name="key">The key.</param>
    /// <param name="create">Builds the value.</param>
    /// <returns>The value.</returns>
    member this.GetOrAdd (key: 'K, create: unit -> 'V) =
        match this.TryFind key with
        | ValueSome value -> value
        | ValueNone -> this.Add (key, create ())

    /// <summary>Whether another object is a cell; equality ignores the values, so the records that hold one compare by their other fields.</summary>
    /// <param name="other">The other object.</param>
    /// <returns>True for any cell.</returns>
    override _.Equals other =
        match other with
        | :? KeyedSlot<'K, 'V> -> true
        | _ -> false

    /// <summary>The hash of a cell, the same for every cell.</summary>
    /// <returns>Zero.</returns>
    override _.GetHashCode () = 0

/// <summary>The cell a font holds for one value computed from its own state (scale, coordinates, synthetic strength): empty when the
/// font is made, filled the first time the value is asked for, and never changed afterwards. The value is a pure function of the font's
/// fields, and every change of those fields makes a new font with a new, empty cell, so the cell has no invalidation. Of two racing
/// builders one value is kept and both are equal. Equality ignores the value, so the fonts that hold one compare by their other fields.</summary>
/// <typeparam name="'T">The value type, an immutable reference type.</typeparam>
[<Sealed; AllowNullLiteral>]
type internal Memo<'T when 'T : not struct>(unset: 'T) =
    let mutable value = unset

    /// <summary>Creates an empty cell that tells its emptiness by holding nothing.</summary>
    new () = Memo<'T>(Unchecked.defaultof<'T>)

    /// <summary>The value held, or the unset value the cell was made with when none has been built: a caller that can read a plan
    /// from the unset value without a test reads one field and no more.</summary>
    /// <returns>The value or the unset value.</returns>
    member _.Value = System.Threading.Volatile.Read(&value)

    /// <summary>The value, when it has been built.</summary>
    /// <returns>The value, or none.</returns>
    member _.TryGet () =
        let held = System.Threading.Volatile.Read(&value)
        match obj.ReferenceEquals(held, unset) with
        | true -> ValueNone
        | false -> ValueSome held

    /// <summary>Publishes a built value; of two racing builders the first is kept.</summary>
    /// <param name="created">The built value.</param>
    /// <returns>The value the cell holds.</returns>
    member _.Set (created: 'T) =
        let now = System.Threading.Interlocked.CompareExchange(&value, created, unset)
        match obj.ReferenceEquals(now, unset) with
        | true -> created
        | false -> now

    /// <summary>Whether another object is a cell; equality ignores the values.</summary>
    /// <param name="other">The other object.</param>
    /// <returns>True for any cell.</returns>
    override _.Equals other =
        match other with
        | :? Memo<'T> -> true
        | _ -> false

    /// <summary>The hash of a cell, the same for every cell.</summary>
    /// <returns>Zero.</returns>
    override _.GetHashCode () = 0

/// <summary>A struct held in an object of its own, set when the object is made and never changed, so that it can be handed out by
/// reference and used where a copy of a large struct would be made at every call.</summary>
/// <typeparam name="'T">The struct type.</typeparam>
[<Sealed>]
type internal Cell<'T when 'T : struct> =
    val mutable Item: 'T

    /// <summary>Holds a struct.</summary>
    /// <param name="item">The struct.</param>
    new (item: 'T) = { Item = item }

/// <summary>The values a plan builds on demand, one for each position of an array, empty when made: a value is built the first time its
/// position is asked for and never changed afterwards. Of two racing builders of one position one value is kept and both get it, which is
/// sound because a value is a pure function of its position and of the plan the array belongs to.</summary>
/// <typeparam name="'T">The value type, an immutable reference type.</typeparam>
[<Sealed>]
type internal SlotArray<'T when 'T : not struct and 'T : not null>(count: int) =
    let slots: 'T array = Array.zeroCreate count

    /// <summary>The value built for a position.</summary>
    /// <param name="index">The position.</param>
    /// <returns>The value, or none when none has been built.</returns>
    member _.TryGet (index: int) =
        let held = System.Threading.Volatile.Read(&slots[index])
        match obj.ReferenceEquals(held, null) with
        | true -> ValueNone
        | false -> ValueSome held

    /// <summary>Keeps a value built for a position, unless another builder kept one first.</summary>
    /// <param name="index">The position.</param>
    /// <param name="created">The built value.</param>
    /// <returns>The value the position holds.</returns>
    member _.Set (index: int, created: 'T) =
        let now = System.Threading.Interlocked.CompareExchange(&slots[index], created, Unchecked.defaultof<'T>)
        match obj.ReferenceEquals(now, null) with
        | true -> created
        | false -> now

/// <summary>The unscaled advances of a font's glyphs found so far, for a font whose advances take a variation delta: kept in pages of 256
/// glyphs, a page made the first time one of its glyphs is stored and an entry written the first time its glyph is looked up. An entry
/// holds <c>Int32.MinValue</c> until then, and a value equal to it is never kept, so the glyph is worked out again. An entry is a pure
/// function of the font's coordinates, which are fixed for the font that holds the pages, so two threads that store one entry store the
/// same value.</summary>
[<Sealed>]
type internal IntPages(count: int) =
    let pages: (int array | null) array = Array.zeroCreate ((count + 255) / 256)

    /// <summary>The entry of a glyph.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The entry, <c>Int32.MinValue</c> when none has been stored or the glyph is past the count.</returns>
    member _.Read (glyph: uint32) =
        match glyph < uint32 count with
        | false -> System.Int32.MinValue
        | true ->
            match System.Threading.Volatile.Read(&pages[int (glyph >>> 8)]) with
            | null -> System.Int32.MinValue
            | page -> System.Threading.Volatile.Read(&page[int (glyph &&& 0xFFu)])

    /// <summary>Stores the entry of a glyph, making its page when it has none; a glyph past the count is not stored.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <param name="entry">The entry.</param>
    member _.Write (glyph: uint32, entry: int) =
        match glyph < uint32 count with
        | false -> ()
        | true ->
            let block = int (glyph >>> 8)
            let page =
                match System.Threading.Volatile.Read(&pages[block]) with
                | null ->
                    let created = Array.create 256 System.Int32.MinValue
                    match System.Threading.Interlocked.CompareExchange(&pages[block], created, null) with
                    | null -> created
                    | existing -> existing
                | existing -> existing
            System.Threading.Volatile.Write(&page[int (glyph &&& 0xFFu)], entry)

/// <summary>The cell of one page of a presence table, a struct so that reading it from an array of cells needs no check of the array's
/// element type.</summary>
[<Struct; NoEquality; NoComparison>]
type internal PresenceSlot =
    val mutable Words: uint64 array | null

/// <summary>Which code points below U+10000 a face's cmap maps, found so far, one bit each in pages of 256 code points: a page is built, from
/// the cmap, the first time a code point of its block is asked for, and never changed afterwards. A page is a pure function of the face's
/// cmap, which is fixed, so two threads that build one page build the same words and the first is kept; a reader sees no page or a whole one.</summary>
[<Sealed; AllowNullLiteral>]
type internal PresencePages() =
    let pages: PresenceSlot array = Array.zeroCreate 256

    /// <summary>The page of a block of 256 code points.</summary>
    /// <param name="block">The block, below 256.</param>
    /// <returns>The four words of the page, or null when none has been built.</returns>
    member _.Page (block: int) = System.Threading.Volatile.Read(&pages[block].Words)

    /// <summary>Keeps a page built for a block, unless another builder kept one first.</summary>
    /// <param name="block">The block, below 256.</param>
    /// <param name="created">The four words of the page.</param>
    /// <returns>The words the block holds.</returns>
    member _.Keep (block: int, created: uint64 array) =
        match System.Threading.Interlocked.CompareExchange(&pages[block].Words, created, null) with
        | null -> created
        | existing -> existing

/// <summary>The cell of one page of the nominal glyphs of a face, a struct so that reading it from an array of cells needs no check of the
/// array's element type.</summary>
[<Struct; NoEquality; NoComparison>]
type internal CmapSlot =
    val mutable Words: uint32 array | null

/// <summary>The nominal glyphs of a face found so far for the code points of the Unicode planes, kept in pages of 256 code points: a
/// page is made the first time a code point of its block is stored, and an entry is written the first time its code point is looked up.
/// An entry holds 0 until then, 1 for a code point without a glyph and the glyph plus 2 otherwise. An entry is a pure function of the
/// face's cmap, so two threads that store one entry store the same value, and a reader sees either 0 or the value.</summary>
[<Sealed; AllowNullLiteral>]
type internal CmapPages() =
    let pages: CmapSlot array = Array.zeroCreate 0x1100

    /// <summary>The entry of a code point.</summary>
    /// <param name="codepoint">The code point, below U+110000.</param>
    /// <returns>The entry, 0 when none has been stored.</returns>
    member _.Read (codepoint: uint32) =
        match System.Threading.Volatile.Read(&pages[int (codepoint >>> 8)].Words) with
        | null -> 0u
        | page -> System.Threading.Volatile.Read(&page[int (codepoint &&& 0xFFu)])

    /// <summary>The page of a block of 256 code points.</summary>
    /// <param name="block">The block, below 0x1100.</param>
    /// <returns>The page, or null when no entry of the block has been stored.</returns>
    member _.Page (block: int) =
        System.Threading.Volatile.Read(&pages[block].Words)

    /// <summary>Stores the entry of a code point, making its page when it has none.</summary>
    /// <param name="codepoint">The code point, below U+110000.</param>
    /// <param name="entry">The entry.</param>
    member _.Write (codepoint: uint32, entry: uint32) =
        let block = int (codepoint >>> 8)
        let page =
            match System.Threading.Volatile.Read(&pages[block].Words) with
            | null ->
                let created = Array.zeroCreate<uint32> 256
                match System.Threading.Interlocked.CompareExchange(&pages[block].Words, created, null) with
                | null -> created
                | existing -> existing
            | existing -> existing
        System.Threading.Volatile.Write(&page[int (codepoint &&& 0xFFu)], entry)

/// <summary>Advances read from a metrics table in one pass.</summary>
[<RequireQualifiedAccess>]
module internal AdvanceTable =
    /// <summary>The scaled advances of glyphs read straight from an hmtx or vmtx table, <c>get_advance_without_var_unscaled</c>
    /// and <c>em_scale</c> for each glyph: the long metric of a glyph below the bearing count, else the default advance
    /// when the table has no advances at all and 0 when it has.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="bearings">The number of glyphs the table gives bearings.</param>
    /// <param name="longMetrics">The number of long metrics.</param>
    /// <param name="advances">The number of advances the table holds, 0 when it holds none.</param>
    /// <param name="defaultAdvance">The advance of a glyph beyond a table without advances.</param>
    /// <param name="multiplier">The font's multiplier, <c>em_scale</c>'s.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The scaled advances, in glyph order.</returns>
    let scaled (table: ReadOnlySpan<byte>) (bearings: int) (longMetrics: int) (advances: int) (defaultAdvance: int) (multiplier: int64) (glyphs: ReadOnlySpan<uint32>) =
        let result = Array.zeroCreate<int> glyphs.Length
        let last = uint64 longMetrics - 1UL
        let mutable index = 0
        while index < glyphs.Length do
            let glyph = uint64 glyphs[index]
            let unscaled =
                match glyph < uint64 bearings with
                | true ->
                    let at = int (min glyph last) * 4
                    match uint64 (uint32 at) + 2UL <= uint64 table.Length with
                    | true -> int (System.Buffers.Binary.BinaryPrimitives.ReadUInt16BigEndian(table.Slice(at, 2)))
                    | false -> 0
                | false ->
                    match advances with
                    | 0 -> defaultAdvance
                    | _ -> 0
            result[index] <- int ((int64 unscaled * multiplier + 32768L) >>> 16)
            index <- index + 1
        ImmutableCollectionsMarshal.AsImmutableArray result

    /// <summary>The array and the offset a block of memory is a window of, none when it is not backed by an array.</summary>
    /// <param name="memory">The memory.</param>
    /// <returns>The array and the offset of the first byte in it, or none.</returns>
    let tryArray (memory: ReadOnlyMemory<byte>) =
        let mutable segment = ArraySegment<byte>()
        match System.Runtime.InteropServices.MemoryMarshal.TryGetArray(memory, &segment) with
        | true -> ValueSome (struct (nonNull segment.Array, segment.Offset))
        | false -> ValueNone

    /// <summary>An array of one value repeated.</summary>
    /// <param name="count">The length.</param>
    /// <param name="value">The value.</param>
    /// <returns>The array.</returns>
    let replicate (count: int) (value: int) =
        ImmutableCollectionsMarshal.AsImmutableArray (Array.create count value)

/// <summary>A piece of work that makes the item at one index, run by <see cref="ParallelFill"/> from any thread: it may read
/// shared immutable values and nothing else.</summary>
type internal IIndexedWorker<'T> =
    /// <summary>Makes the item at an index.</summary>
    /// <param name="index">The index.</param>
    /// <returns>The item.</returns>
    abstract At : index: int -> 'T

/// <summary>Filling one array by index, in chunks, from several threads: the one place where workers write.</summary>
[<RequireQualifiedAccess>]
module internal ParallelFill =
    /// <summary>The smallest number of cost units in a chunk: below it, scheduling a chunk costs more than the work in it.</summary>
    [<Literal>]
    let MinChunkCost = 512

    /// <summary>How many chunks each worker thread gets on average, so that a thread that finishes early takes another chunk.</summary>
    [<Literal>]
    let ChunksPerThread = 8

    /// <summary>The first index of each chunk: a chunk ends once its items cost at least the chunk cost, so a chunk of tiny
    /// items is long and a chunk of one huge item is short.</summary>
    /// <param name="costs">The cost of each item.</param>
    /// <param name="chunkCost">The cost a chunk reaches before it ends.</param>
    /// <returns>The chunk starts, the last one followed by the item count.</returns>
    let chunkStarts (costs: ReadOnlySpan<int>) (chunkCost: int) =
        let starts = List<int>()
        let mutable accumulated = 0
        let mutable index = 0
        starts.Add 0
        while index < costs.Length do
            accumulated <- accumulated + max 1 costs[index]
            index <- index + 1
            if accumulated >= chunkCost && index < costs.Length then
                starts.Add index
                accumulated <- 0
        starts.Add costs.Length
        starts.ToArray()

    /// <summary>The total cost of the items.</summary>
    /// <param name="costs">The cost of each item.</param>
    /// <returns>The sum, at least one per item.</returns>
    let totalCost (costs: ReadOnlySpan<int>) =
        let mutable total = 0L
        let mutable index = 0
        while index < costs.Length do
            total <- total + int64 (max 1 costs[index])
            index <- index + 1
        total

    /// <summary>Makes every item in index order on the calling thread.</summary>
    /// <param name="count">The number of items.</param>
    /// <param name="worker">The worker.</param>
    /// <returns>The items.</returns>
    let sequential<'T, 'W when 'W :> IIndexedWorker<'T> and 'W : struct> (count: int) (worker: 'W) =
        let items = Array.zeroCreate<'T> count
        let mutable index = 0
        while index < count do
            items[index] <- worker.At index
            index <- index + 1
        ImmutableCollectionsMarshal.AsImmutableArray items

    /// <summary>Makes every item of one chunk, writing each into its own slot.</summary>
    /// <param name="items">The array being filled.</param>
    /// <param name="starts">The chunk starts.</param>
    /// <param name="chunk">The chunk.</param>
    /// <param name="worker">The worker.</param>
    let private fillChunk<'T, 'W when 'W :> IIndexedWorker<'T> and 'W : struct> (items: 'T array) (starts: int array) (chunk: int) (worker: 'W) =
        let mutable index = starts[chunk]
        let last = starts[chunk + 1]
        while index < last do
            items[index] <- worker.At index
            index <- index + 1

    /// <summary>Makes every item, the chunks spread over the thread pool with at most the given degree of parallelism; each
    /// chunk writes only its own slots, so the result equals <see cref="sequential"/>'s.</summary>
    /// <param name="degree">The most threads at once.</param>
    /// <param name="count">The number of items.</param>
    /// <param name="starts">The chunk starts, the last followed by the item count.</param>
    /// <param name="worker">The worker.</param>
    /// <returns>The items.</returns>
    let chunked<'T, 'W when 'W :> IIndexedWorker<'T> and 'W : struct> (degree: int) (count: int) (starts: int array) (worker: 'W) =
        let items = Array.zeroCreate<'T> count
        let options = System.Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = degree)
        System.Threading.Tasks.Parallel.For(0, starts.Length - 1, options, fun chunk -> fillChunk items starts chunk worker) |> ignore
        ImmutableCollectionsMarshal.AsImmutableArray items

/// <summary>A piece of work that makes the item at one index with the scratch arrays of its chunk, run by
/// <see cref="ParallelFill"/> from any thread: it may read shared immutable values, write its own window of a document's arrays and
/// the scratch, and nothing else.</summary>
type internal IScratchWorker<'T> =
    /// <summary>Makes the item at an index.</summary>
    /// <param name="index">The index.</param>
    /// <param name="scratch">The scratch arrays of the chunk, at least as long as its longest line.</param>
    /// <returns>The item.</returns>
    abstract At : index: int * scratch: LineScratch -> 'T

/// <summary>Filling one array by index with chunk-owned scratch arrays.</summary>
[<RequireQualifiedAccess>]
module internal ScratchFill =
    /// <summary>The longest line of a range of items.</summary>
    /// <param name="sizes">The size of each item.</param>
    /// <param name="first">The first item.</param>
    /// <param name="last">The end of the range.</param>
    /// <returns>The largest size.</returns>
    let private longest (sizes: ImmutableArray<int>) (first: int) (last: int) =
        let mutable most = 0
        let mutable index = first
        while index < last do
            most <- max most sizes[index]
            index <- index + 1
        most

    /// <summary>Makes the scratch arrays for lines of up to a size.</summary>
    /// <param name="size">The longest line.</param>
    /// <returns>The arrays, not cleared: a worker reads only what it wrote.</returns>
    let private scratchOf (size: int) =
        { ScratchInfos = GC.AllocateUninitializedArray<GlyphInfo> size
          ScratchProps = GC.AllocateUninitializedArray<GlyphProps> size
          ScratchPositions = GC.AllocateUninitializedArray<GlyphPosition> size }

    /// <summary>Makes every item of a range in order, with one scratch for the whole range.</summary>
    /// <param name="items">The array being filled.</param>
    /// <param name="sizes">The size of each item.</param>
    /// <param name="first">The first item.</param>
    /// <param name="last">The end of the range.</param>
    /// <param name="worker">The worker.</param>
    let private fillRange<'T, 'W when 'W :> IScratchWorker<'T>> (items: 'T array) (sizes: ImmutableArray<int>) (first: int) (last: int) (worker: 'W) =
        let scratch = scratchOf (longest sizes first last)
        let mutable index = first
        while index < last do
            items[index] <- worker.At(index, scratch)
            index <- index + 1

    /// <summary>Makes every item in index order on the calling thread.</summary>
    /// <param name="sizes">The size of each item.</param>
    /// <param name="worker">The worker.</param>
    /// <returns>The items.</returns>
    let sequential<'T, 'W when 'W :> IScratchWorker<'T>> (sizes: ImmutableArray<int>) (worker: 'W) =
        let items = Array.zeroCreate<'T> sizes.Length
        fillRange items sizes 0 sizes.Length worker
        ImmutableCollectionsMarshal.AsImmutableArray items

    /// <summary>Makes every item, the chunks spread over the thread pool with at most the given degree of parallelism; each chunk
    /// has scratch arrays of its own and writes only its own slots, so the result equals <see cref="sequential"/>'s.</summary>
    /// <param name="degree">The most threads at once.</param>
    /// <param name="sizes">The size of each item.</param>
    /// <param name="starts">The chunk starts, the last followed by the item count.</param>
    /// <param name="worker">The worker.</param>
    /// <returns>The items.</returns>
    let chunked<'T, 'W when 'W :> IScratchWorker<'T>> (degree: int) (sizes: ImmutableArray<int>) (starts: int array) (worker: 'W) =
        let items = Array.zeroCreate<'T> sizes.Length
        let options = System.Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = degree)
        System.Threading.Tasks.Parallel.For(0, starts.Length - 1, options, fun chunk -> fillRange items sizes starts[chunk] starts[chunk + 1] worker) |> ignore
        ImmutableCollectionsMarshal.AsImmutableArray items

/// <summary>The in-place passes of the syllabic shapers on the working set: the arrays belong to the run being shaped, made in this
/// call and shared with no one, so a pause writes its effect into them instead of publishing a new run, as upstream's pause
/// functions write into the one buffer.</summary>
[<RequireQualifiedAccess>]
module internal SyllableOwned =
    /// <summary>The flags range above which a flag request is ignored, <c>HB_MAX_GLYPH_FLAG_RANGE</c>.</summary>
    [<Literal>]
    let private MaxGlyphFlagRange = 255

    /// <summary>The unsafe-to-break and unsafe-to-concat flags or-ed together.</summary>
    [<Literal>]
    let private UnsafeToBreakMask = 3u

    /// <summary>Paints the syllable byte of the runs a source emits into the properties, in place, leaving the glyphs no run covers as
    /// they are: <see cref="Sealed.paintRuns"/> without the copy of the previous values and the copy of the properties.</summary>
    /// <typeparam name="'G">The run source type.</typeparam>
    /// <typeparam name="'S">The source state type.</typeparam>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <param name="source">The run source.</param>
    /// <param name="seed">The initial source state.</param>
    /// <returns>The final source state.</returns>
    let paint<'G, 'S when 'G : struct and 'G :> IRunSource<'S>> (props: ImmutableArray<GlyphProps>) (source: 'G) (seed: 'S) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let mutable state = seed
        let mutable running = true
        while running do
            match source.Next(state) with
            | RunStep.Run (start, stop, value, next) ->
                let mutable index = start
                while index < stop do
                    array[index] <- { array[index] with Syllable = value }
                    index <- index + 1
                state <- next
            | RunStep.Exhausted final ->
                state <- final
                running <- false
        state

    /// <summary>The label a Ragel scan is at: the key of the current glyph is to be looked up.</summary>
    [<Literal>]
    let private LabelResume = 0

    /// <summary>The label a Ragel scan is at: a transition is to be taken.</summary>
    [<Literal>]
    let private LabelDispatch = 1

    /// <summary>The label a Ragel scan is at: the statements of an action body are to be run.</summary>
    [<Literal>]
    let private LabelExecute = 2

    /// <summary>The label a Ragel scan is at: the cursor is to advance.</summary>
    [<Literal>]
    let private LabelAgain = 3

    /// <summary>The label a Ragel scan is at: the end of the input is to be tested for an end-of-file transition.</summary>
    [<Literal>]
    let private LabelTestEof = 4

    /// <summary>The syllable serial after another: 16 wraps to 1, <c>(serial + 1) % 16</c> with zero skipped.</summary>
    [<Literal>]
    let private SerialLimit = 16

    /// <summary>The bits the syllable type takes in a syllable byte, below the serial.</summary>
    [<Literal>]
    let private SyllableSerialShift = 4

    [<Literal>]
    let private NoEffect = 137

    /// <summary>The buffer index of a machine index.</summary>
    /// <param name="mapped">Whether a mapping is given.</param>
    /// <param name="positions">The buffer index of each machine index, when mapped.</param>
    /// <param name="index">The machine index.</param>
    /// <returns>The buffer index.</returns>
    let private mappedIndex mapped (positions: int array) index =
        match mapped with
        | true -> positions[index]
        | false -> index

    /// <summary>Gives the glyphs of a range the syllable byte of a syllable, in place.</summary>
    /// <param name="props">The properties.</param>
    /// <param name="start">The first index.</param>
    /// <param name="stop">The end index.</param>
    /// <param name="value">The syllable byte.</param>
    let private paintSyllable (props: GlyphProps array) start stop value =
        let mutable index = start
        while index < stop do
            props[index] <- { props[index] with Syllable = value }
            index <- index + 1

    /// <summary>Runs a Ragel scanner over a run of categories one glyph at a time: the same transitions, the same action statements in the same
    /// order and the same serial and broken flag as the interpreter, as one loop with the scan state in locals and no label to dispatch
    /// on. Every syllable found is handed to the caller, which knows how its indices map to the buffer.</summary>
    /// <param name="flat">The flat tables of the machine.</param>
    /// <param name="length">The number of machine indices.</param>
    /// <param name="category">The category byte of a machine index.</param>
    /// <param name="found">Called with the start and end machine indices and the syllable byte of every syllable found.</param>
    /// <returns>Whether a broken syllable was found.</returns>
    let inline private scanFlat (flat: RagelCompiled) (length: int) ([<InlineIfLambda>] category: int -> byte) ([<InlineIfLambda>] found: int -> int -> byte -> unit) =
        let transTargs = nonNull (ImmutableCollectionsMarshal.AsArray flat.FlatTransTargs)
        let transActions = nonNull (ImmutableCollectionsMarshal.AsArray flat.FlatTransActions)
        let setsTs = nonNull (ImmutableCollectionsMarshal.AsArray flat.FlatSetsTs)
        let resetsTs = nonNull (ImmutableCollectionsMarshal.AsArray flat.FlatResetsTs)
        let eofTrans = nonNull (ImmutableCollectionsMarshal.AsArray flat.FlatEofTrans)
        let actionKinds = nonNull (ImmutableCollectionsMarshal.AsArray flat.ActionKinds)
        let actionFirst = nonNull (ImmutableCollectionsMarshal.AsArray flat.ActionFirstList)
        let actionCount = nonNull (ImmutableCollectionsMarshal.AsArray flat.ActionListCount)
        let listStarts = nonNull (ImmutableCollectionsMarshal.AsArray flat.ListStarts)
        let listEnds = nonNull (ImmutableCollectionsMarshal.AsArray flat.ListEnds)
        let opCodes = nonNull (ImmutableCollectionsMarshal.AsArray flat.OpCodes)
        let listEffects = nonNull (ImmutableCollectionsMarshal.AsArray flat.ListEffects)
        let denseNext = nonNull (ImmutableCollectionsMarshal.AsArray flat.DenseNext)
        let denseEffect = nonNull (ImmutableCollectionsMarshal.AsArray flat.DenseEffect)
        let denseTrans = nonNull (ImmutableCollectionsMarshal.AsArray flat.DenseTrans)
        let columns = flat.DenseColumns
        let maxKey = flat.DenseMaxKey
        let mutable cs = flat.FlatStart
        let mutable setNow = setsTs[cs]
        let mutable p = 0
        let mutable ts = 0
        let mutable te = 0
        let mutable act = 0
        let mutable trans = 0
        let mutable serial = 1
        let mutable broken = false
        let mutable atEof = (length = 0)
        let mutable running = true
        match atEof with
        | true ->
            let eof = eofTrans[cs]
            trans <- eof - 1
            running <- eof > 0
        | false -> ()
        while running do
            let mutable target = 0
            let mutable nextSets = false
            let mutable nextResets = false
            let mutable effect = RagelCodes.EffectSwitch
            match atEof with
            | true -> ()
            | false ->
                ts <- (match setNow with | true -> p | false -> ts)
                let key = int (category p)
                let slot = cs * columns + (match key > maxKey with | true -> columns - 1 | false -> key)
                effect <- denseEffect[slot]
                match effect with
                | RagelCodes.EffectSwitch -> trans <- denseTrans[slot]
                | _ ->
                    let next = denseNext[slot]
                    target <- next &&& 255
                    nextSets <- (next &&& 256) <> 0
                    nextResets <- (next &&& 512) <> 0
            let mutable list = RagelCodes.NoList
            match effect with
            | RagelCodes.EffectSwitch ->
                let action = int transActions[trans]
                target <- int transTargs[trans]
                nextSets <- setsTs[target]
                nextResets <- resetsTs[target]
                list <-
                    match actionKinds[action] with
                    | RagelCodes.OpsAction -> actionFirst[action]
                    | RagelCodes.SwitchAction when act < actionCount[action] -> actionFirst[action] + act
                    | _ -> RagelCodes.NoList
                effect <- (match list with | RagelCodes.NoList -> NoEffect | listed -> listEffects[listed])
            | _ -> ()
            match effect with
            | RagelCodes.EffectSlow ->
                let mutable opIndex = listStarts[list]
                let opEnd = listEnds[list]
                while opIndex < opEnd do
                    let code = opCodes[opIndex]
                    opIndex <- opIndex + 1
                    let argument = (code >>> RagelCodes.ArgumentShift) &&& RagelCodes.ArgumentMask
                    match code &&& RagelCodes.CodeMask with
                    | RagelCodes.TsFromP -> ts <- p
                    | RagelCodes.TsReset -> ts <- 0
                    | RagelCodes.TeFromNextP -> te <- p + 1
                    | RagelCodes.TeFromP -> te <- p
                    | RagelCodes.PBack -> p <- p - 1
                    | RagelCodes.PFromTe -> p <- te - 1
                    | RagelCodes.ActSet -> act <- argument
                    | RagelCodes.FoundSyllable ->
                        found ts te (byte ((serial <<< SyllableSerialShift) ||| argument))
                        serial <- (match serial + 1 with | SerialLimit -> 1 | next -> next)
                        broken <- broken || (code >>> RagelCodes.BrokenShift) <> 0
                    | _ -> ()
            | _ ->
                let oldP = p
                let oldTe = te
                match effect &&& RagelCodes.EffectFound with
                | 0 -> ()
                | _ ->
                    let foundForm = (effect >>> RagelCodes.EffectFoundShift) &&& 15
                    let foundTe = (match foundForm &&& RagelCodes.EffectTeFromTe with | 0 -> oldP | _ -> oldTe) + ((foundForm >>> 1) - RagelCodes.EffectOffsetBias)
                    found ts foundTe (byte ((serial <<< SyllableSerialShift) ||| ((effect >>> RagelCodes.EffectTypeShift) &&& RagelCodes.ArgumentMask)))
                    serial <- (match serial + 1 with | SerialLimit -> 1 | next -> next)
                    broken <- broken || (effect &&& RagelCodes.EffectBroken) <> 0
                te <- (match effect &&& RagelCodes.EffectTeFromTe with | 0 -> oldP | _ -> oldTe) + (((effect >>> 1) &&& RagelCodes.EffectOffsetMask) - RagelCodes.EffectOffsetBias)
                p <- (match (effect >>> RagelCodes.EffectPShift) &&& RagelCodes.EffectTeFromTe with | 0 -> oldP | _ -> oldTe) + (((effect >>> (RagelCodes.EffectPShift + 1)) &&& RagelCodes.EffectOffsetMask) - RagelCodes.EffectOffsetBias)
                act <- (match effect &&& RagelCodes.EffectHasAct with | 0 -> act | _ -> (effect >>> RagelCodes.EffectActShift) &&& RagelCodes.ArgumentMask)
            cs <- target
            setNow <- nextSets
            ts <- (match nextResets with | true -> 0 | false -> ts)
            p <- p + 1
            match p = length with
            | false -> atEof <- false
            | true ->
                let eof = eofTrans[cs]
                trans <- eof - 1
                atEof <- true
                running <- eof > 0
        broken

    /// <summary>Runs a Ragel scanner over a category array and paints the syllable byte of every syllable it finds into the properties, in
    /// place, the result of <see cref="paint"/> over <c>Ragel.step</c>: the same transitions, the same action statements
    /// in the same order, and the same serial and broken flag, with the scan state in locals.</summary>
    /// <param name="flat">The flat tables of the machine.</param>
    /// <param name="categories">The per-glyph category bytes of the machine's input.</param>
    /// <param name="positions">The buffer index of each machine index, or the default array for the identity mapping.</param>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <returns>Whether a broken syllable was found.</returns>
    let paintFlat (flat: RagelCompiled) (categories: ImmutableArray<byte>) (positions: ImmutableArray<int>) (props: ImmutableArray<GlyphProps>) =
        let cats = nonNull (ImmutableCollectionsMarshal.AsArray categories)
        let propsArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let mapped = not positions.IsDefault
        let positionArray = match mapped with | true -> nonNull (ImmutableCollectionsMarshal.AsArray positions) | false -> Array.empty<int>
        let length = cats.Length
        scanFlat flat length (fun index -> cats[index]) (fun ts te value ->
            paintSyllable propsArray (mappedIndex mapped positionArray ts) (match te < length with | true -> mappedIndex mapped positionArray te | false -> propsArray.Length) value)

    /// <summary>Runs a Ragel scanner over the glyphs a filter kept and paints the syllable byte of every syllable it finds into the properties,
    /// in place, <see cref="paintFlat"/> for categories read from the properties of the kept glyphs themselves: the machine index
    /// <c>k</c> reads the category of the glyph at <c>positions[k]</c>, so no category array is copied.</summary>
    /// <param name="flat">The flat tables of the machine.</param>
    /// <param name="positions">The buffer index of each kept glyph, in order.</param>
    /// <param name="props">The properties, which carry the categories and are given the syllable bytes.</param>
    /// <returns>Whether a broken syllable was found.</returns>
    let paintFlatKept (flat: RagelCompiled) (positions: ImmutableArray<int>) (props: ImmutableArray<GlyphProps>) =
        let positionArray = nonNull (ImmutableCollectionsMarshal.AsArray positions)
        let propsArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let length = positionArray.Length
        scanFlat flat length (fun index -> propsArray[positionArray[index]].ShaperCategory) (fun ts te value ->
            paintSyllable propsArray positionArray[ts] (match te < length with | true -> positionArray[te] | false -> propsArray.Length) value)

    /// <summary>Flags the glyphs of a range unsafe to break and to concat.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let private flagRange (infos: GlyphInfo array) (start: int) (finish: int) =
        let mutable index = start
        while index < finish do
            infos[index] <- { infos[index] with Mask = infos[index].Mask ||| UnsafeToBreakMask }
            index <- index + 1

    /// <summary>The smallest cluster over a range.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The smallest cluster.</returns>
    let private minOverRange (infos: GlyphInfo array) (start: int) (finish: int) =
        let mutable best = UInt32.MaxValue
        let mutable index = start
        while index < finish do
            best <- min best infos[index].Cluster
            index <- index + 1
        best

    /// <summary>Flags the glyphs of a range whose cluster is not the given one.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="cluster">The interior cluster.</param>
    let private flagOutside (infos: GlyphInfo array) (start: int) (finish: int) (cluster: uint32) =
        let mutable index = start
        while index < finish do
            match infos[index].Cluster <> cluster with
            | true -> infos[index] <- { infos[index] with Mask = infos[index].Mask ||| UnsafeToBreakMask }
            | false -> ()
            index <- index + 1

    /// <summary>Marks a range unsafe to break, skipping its minimum cluster, <c>unsafe_to_break</c> (src/hb-buffer.cc), in place: the
    /// flags and the interior of <see cref="SyllableEdit.unsafeToBreak"/>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="infos">The glyphs.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let unsafeToBreak (level: ClusterLevel) (infos: GlyphInfo array) (start: int) (finish: int) =
        match finish - start > MaxGlyphFlagRange || finish - start < 2 with
        | true -> ()
        | false ->
            let exact =
                match level with
                | ClusterLevel.Characters -> true
                | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters | ClusterLevel.Graphemes -> false
            let clusterFirst = infos[start].Cluster
            let clusterLast = infos[finish - 1].Cluster
            let cluster =
                match exact with
                | true -> minOverRange infos start finish
                | false -> min clusterFirst clusterLast
            match exact || (cluster <> clusterFirst && cluster <> clusterLast) with
            | true -> flagOutside infos start finish cluster
            | false ->
                match cluster = clusterFirst with
                | true ->
                    let mutable index = finish
                    while index > start && infos[index - 1].Cluster <> clusterFirst do
                        index <- index - 1
                    flagRange infos index finish
                | false ->
                    let mutable index = start
                    while index < finish && infos[index].Cluster <> clusterLast do
                        index <- index + 1
                    flagRange infos start index

    /// <summary>Finds the syllables of a run whose machine reads the shaper category of the glyph at the same index, paints their bytes into
    /// the properties and marks each unsafe to break, in place and in the one scan, the result of <see cref="paintFlat"/> over a copy of the
    /// categories followed by <see cref="unsafeToBreakSyllables"/>: the syllables tile the glyphs and no two neighbours share a byte, so the
    /// chunks of the second are the syllables the first found.</summary>
    /// <param name="flat">The flat tables of the machine.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties, which carry the categories and are given the syllable bytes.</param>
    /// <returns>Whether a broken syllable was found.</returns>
    let paintFlatBreaking (flat: RagelCompiled) (level: ClusterLevel) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) =
        let infosArray = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let propsArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
        scanFlat flat propsArray.Length (fun index -> propsArray[index].ShaperCategory) (fun ts te value ->
            paintSyllable propsArray ts te value
            unsafeToBreak level infosArray ts te)

    /// <summary>Marks every syllable chunk of a run unsafe to break, in place, the loop of <c>setup_syllables_*</c>: the chunks
    /// <see cref="SyllableEdit.chunks"/> finds.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties, whose syllable bytes define the chunks.</param>
    let unsafeToBreakSyllables (level: ClusterLevel) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable start = 0
        while start < array.Length do
            let finish = Sealed.nextSyllableEnd (props.AsSpan()) start
            unsafeToBreak level array start finish
            start <- finish

    /// <summary>Gives the first glyphs of every syllable chunk a mask, in place, <c>setup_rphf_mask</c>'s loop of
    /// <c>setup_syllables_use</c>: the one glyph of a chunk whose first glyph has the single category, else the first
    /// <paramref name="count"/> glyphs.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties, whose syllable bytes define the chunks.</param>
    /// <param name="mask">The mask.</param>
    /// <param name="single">The shaper category whose first glyph takes the mask alone.</param>
    /// <param name="count">The glyphs a chunk of another start takes.</param>
    let maskSyllableHeads (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (mask: uint32) (single: byte) (count: int) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable start = 0
        while start < array.Length do
            let finish = Sealed.nextSyllableEnd (props.AsSpan()) start
            let limit =
                match props[start].ShaperCategory = single with
                | true -> 1
                | false -> min count (finish - start)
            let mutable index = start
            while index < start + limit do
                array[index] <- { array[index] with Mask = array[index].Mask ||| mask }
                index <- index + 1
            start <- finish

    /// <summary>Clears bits of the glyph class of every glyph, in place.</summary>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <param name="bits">The bits to clear.</param>
    let clearClassBits (props: ImmutableArray<GlyphProps>) (bits: uint16) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let mutable index = 0
        while index < array.Length do
            array[index] <- { array[index] with GlyphClass = array[index].GlyphClass &&& ~~~bits }
            index <- index + 1

    /// <summary>The cluster-flag bits a cluster change resets, <c>HB_GLYPH_FLAG_DEFINED</c>.</summary>
    [<Literal>]
    let private GlyphFlagDefined = 7u

    /// <summary>The index of the mask among the 32-bit fields of a glyph info, <c>mask</c> after <c>codepoint</c> in the sequential layout of
    /// <see cref="GlyphInfo"/>.</summary>
    [<Literal>]
    let private InfoMaskField = 1

    /// <summary>The byte offset of the syllable byte in the sequential layout of <see cref="GlyphProps"/>: two 16-bit fields and one byte
    /// come first.</summary>
    [<Literal>]
    let private SyllableByte = 5

    /// <summary>The byte offset of the shaper auxiliary byte, the position, in the sequential layout of <see cref="GlyphProps"/>.</summary>
    [<Literal>]
    let private AuxiliaryByte = 7

    /// <summary>Gives one glyph a syllable byte, in place, without reading or writing its other fields.</summary>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <param name="index">The glyph.</param>
    /// <param name="syllable">The syllable byte.</param>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let setSyllable (props: ImmutableArray<GlyphProps>) (index: int) (syllable: byte) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let field = &Unsafe.Add(&Unsafe.As<GlyphProps, byte>(&array[index]), SyllableByte)
        field <- syllable

    /// <summary>Gives one glyph a position, in place, without reading or writing its other fields.</summary>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <param name="index">The glyph.</param>
    /// <param name="position">The position.</param>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let setAuxiliary (props: ImmutableArray<GlyphProps>) (index: int) (position: byte) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let field = &Unsafe.Add(&Unsafe.As<GlyphProps, byte>(&array[index]), AuxiliaryByte)
        field <- position

    /// <summary>Writes the properties of one glyph.</summary>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <param name="index">The glyph.</param>
    /// <param name="value">The properties.</param>
    let setProp (props: ImmutableArray<GlyphProps>) (index: int) (value: GlyphProps) =
        (nonNull (ImmutableCollectionsMarshal.AsArray props))[index] <- value

    /// <summary>Writes one glyph.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="index">The glyph.</param>
    /// <param name="value">The glyph.</param>
    let setInfo (infos: ImmutableArray<GlyphInfo>) (index: int) (value: GlyphInfo) =
        (nonNull (ImmutableCollectionsMarshal.AsArray infos))[index] <- value

    /// <summary>Sets bits of the mask of one glyph.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="index">The glyph.</param>
    /// <param name="mask">The bits to set.</param>
    let orMask (infos: ImmutableArray<GlyphInfo>) (index: int) (mask: uint32) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let field = &Unsafe.Add(&Unsafe.As<GlyphInfo, uint32>(&array[index]), InfoMaskField)
        field <- field ||| mask

    /// <summary>Clears bits of the mask of one glyph.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="index">The glyph.</param>
    /// <param name="mask">The bits to clear.</param>
    let clearMask (infos: ImmutableArray<GlyphInfo>) (index: int) (mask: uint32) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let field = &Unsafe.Add(&Unsafe.As<GlyphInfo, uint32>(&array[index]), InfoMaskField)
        field <- field &&& ~~~mask

    /// <summary>Moves the glyph at <paramref name="from"/> to <paramref name="into"/>, shifting the glyphs in between, in place:
    /// <see cref="Sealed.move"/> on the glyphs and their properties together.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="from">The index of the glyph to move.</param>
    /// <param name="into">The destination index.</param>
    let move (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (from: int) (into: int) =
        match from = into with
        | true -> ()
        | false ->
            let infoArray = nonNull (ImmutableCollectionsMarshal.AsArray infos)
            let propArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
            let info = infoArray[from]
            let prop = propArray[from]
            let mutable k = from
            match from < into with
            | true ->
                while k < into do
                    infoArray[k] <- infoArray[k + 1]
                    propArray[k] <- propArray[k + 1]
                    k <- k + 1
            | false ->
                while k > into do
                    infoArray[k] <- infoArray[k - 1]
                    propArray[k] <- propArray[k - 1]
                    k <- k - 1
            infoArray[into] <- info
            propArray[into] <- prop

    /// <summary>Reverses the glyphs of a range and their properties, in place, <c>hb_buffer_t::reverse_range</c>.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let reverse (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) =
        match finish - start < 2 with
        | true -> ()
        | false ->
            let infoArray = nonNull (ImmutableCollectionsMarshal.AsArray infos)
            let propArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
            let mutable low = start
            let mutable high = finish - 1
            while low < high do
                let info = infoArray[low]
                infoArray[low] <- infoArray[high]
                infoArray[high] <- info
                let prop = propArray[low]
                propArray[low] <- propArray[high]
                propArray[high] <- prop
                low <- low + 1
                high <- high - 1

    /// <summary>Gives the glyphs of a range one syllable byte.</summary>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="syllable">The syllable byte.</param>
    let fillSyllable (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) (syllable: byte) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let mutable index = start
        while index < finish do
            let field = &Unsafe.Add(&Unsafe.As<GlyphProps, byte>(&array[index]), SyllableByte)
            field <- syllable
            index <- index + 1

    /// <summary>Sorts a range of glyphs stably by the position in their properties, in place, <c>hb_stable_sort</c>: the glyphs and
    /// their properties move together.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let sortByAuxiliary (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) =
        let infoArray = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let propArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let mutable i = start + 1
        while i < finish do
            let key = propArray[i].ShaperAuxiliary
            let mutable j = i
            while j > start && propArray[j - 1].ShaperAuxiliary > key do
                j <- j - 1
            match j = i with
            | true -> ()
            | false ->
                let info = infoArray[i]
                let prop = propArray[i]
                let mutable k = i
                while k > j do
                    infoArray[k] <- infoArray[k - 1]
                    propArray[k] <- propArray[k - 1]
                    k <- k - 1
                infoArray[j] <- info
                propArray[j] <- prop
            i <- i + 1

    /// <summary>Gives the glyphs of a range their offsets from the start of the range as syllable bytes and sorts them stably by the position
    /// in their properties, in place, in one pass: the offsets first, then <see cref="sortByAuxiliary"/>.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let sortByAuxiliaryIndexing (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) =
        let infoArray = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let propArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
        match start < finish with
        | false -> ()
        | true ->
            let firstField = &Unsafe.Add(&Unsafe.As<GlyphProps, byte>(&propArray[start]), SyllableByte)
            firstField <- 0uy
            let mutable i = start + 1
            while i < finish do
                let syllableField = &Unsafe.Add(&Unsafe.As<GlyphProps, byte>(&propArray[i]), SyllableByte)
                syllableField <- byte (i - start)
                let prop = propArray[i]
                let key = prop.ShaperAuxiliary
                let mutable j = i
                while j > start && propArray[j - 1].ShaperAuxiliary > key do
                    j <- j - 1
                match j = i with
                | true -> ()
                | false ->
                    let info = infoArray[i]
                    let mutable k = i
                    while k > j do
                        infoArray[k] <- infoArray[k - 1]
                        propArray[k] <- propArray[k - 1]
                        k <- k - 1
                    infoArray[j] <- info
                    propArray[j] <- prop
                i <- i + 1

    /// <summary>The index of the cluster among the 32-bit fields of a glyph info, after the code point and the mask in the sequential layout of
    /// <see cref="GlyphInfo"/>.</summary>
    [<Literal>]
    let private InfoClusterField = 2

    /// <summary>Gives a glyph a cluster, resetting its glyph flags when the cluster changes, <c>set_cluster</c>, in place: a glyph that has
    /// the cluster is not written, and a glyph that changes has only its mask and its cluster written.</summary>
    /// <param name="array">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="index">The glyph.</param>
    /// <param name="cluster">The cluster.</param>
    let private setCluster (array: GlyphInfo array) (index: int) (cluster: uint32) =
        let origin = &Unsafe.As<GlyphInfo, uint32>(&array[index])
        let clusterField = &Unsafe.Add(&origin, InfoClusterField)
        match clusterField = cluster with
        | true -> ()
        | false ->
            let maskField = &Unsafe.Add(&origin, InfoMaskField)
            maskField <- maskField &&& ~~~GlyphFlagDefined
            clusterField <- cluster

    /// <summary>Merges the clusters of a range, <c>merge_clusters</c> and <c>merge_clusters_impl</c> (src/hb-buffer.cc), in place: the
    /// range grows over the glyphs of the clusters it touches at either end, outside the syllable as well, as upstream's does.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let mergeClusters (level: ClusterLevel) (infos: ImmutableArray<GlyphInfo>) (start: int) (finish: int) =
        match finish - start < 2 with
        | true -> ()
        | false ->
            let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
            match level with
            | ClusterLevel.Characters | ClusterLevel.Graphemes -> unsafeToBreak level array start finish
            | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters ->
                let cluster = minOverRange array start finish
                let mutable last = finish
                let mutable first = start
                match cluster <> array[last - 1].Cluster with
                | true ->
                    while last < array.Length && array[last - 1].Cluster = array[last].Cluster do
                        last <- last + 1
                | false -> ()
                match cluster <> array[first].Cluster with
                | true ->
                    while first > 0 && array[first - 1].Cluster = array[first].Cluster do
                        first <- first - 1
                | false -> ()
                let mutable index = first
                while index < last do
                    setCluster array index cluster
                    index <- index + 1

    /// <summary>Marks the two glyphs on either side of <paramref name="start"/> unsafe to break, <c>unsafe_to_break (start - 1, start + 1)</c>:
    /// the one with the greater cluster, none when they share one.</summary>
    /// <param name="array">The glyphs.</param>
    /// <param name="start">The first index of the chunk, greater than zero.</param>
    let private unsafeToBreakAcross (array: GlyphInfo array) (start: int) =
        match array[start - 1].Cluster < array[start].Cluster with
        | true -> array[start] <- { array[start] with Mask = array[start].Mask ||| UnsafeToBreakMask }
        | false ->
            match array[start - 1].Cluster > array[start].Cluster with
            | true -> array[start - 1] <- { array[start - 1] with Mask = array[start - 1].Mask ||| UnsafeToBreakMask }
            | false -> ()

    /// <summary>The general category of the first of the categories a word's glyphs have, <c>HB_UNICODE_GENERAL_CATEGORY_FORMAT</c>.</summary>
    [<Literal>]
    let private GeneralCategoryFormat = 1us

    /// <summary>The general category of the last of the categories a word's glyphs have, <c>HB_UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK</c>.</summary>
    [<Literal>]
    let private GeneralCategoryNonSpacingMark = 12us

    /// <summary>The bits of the Unicode properties that hold the general category.</summary>
    [<Literal>]
    let private GeneralCategoryBits = 0x1Fus

    /// <summary>Gives the first glyph of a chunk a mask when the glyph before it is neither a format character, a letter nor a mark,
    /// else marks the two glyphs on either side of the chunk's left edge unsafe to break, the end of
    /// <c>final_reordering_syllable_indic</c> (src/hb-ot-shaper-indic.cc:1430-1439), in place.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties, read for the general category of the glyph before the chunk.</param>
    /// <param name="start">The chunk's first index.</param>
    /// <param name="mask">The mask of the feature.</param>
    let initOrUnsafeToBreakAcross (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (mask: uint32) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let category = match start > 0 with | true -> props[start - 1].UnicodeProps &&& GeneralCategoryBits | false -> 0us
        match start > 0 && category >= GeneralCategoryFormat && category <= GeneralCategoryNonSpacingMark with
        | true -> unsafeToBreakAcross array start
        | false -> array[start] <- { array[start] with Mask = array[start].Mask ||| mask }

    /// <summary>Sorts a range of glyphs stably by the position in their properties, merging the clusters of each moved span, in place,
    /// <c>hb_buffer_t::sort</c>: the glyphs and their properties move together.</summary>
    /// <param name="level">The cluster level.</param>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="props">The properties.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let sortMerging (level: ClusterLevel) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) =
        let propArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let mutable i = start + 1
        while i < finish do
            let key = propArray[i].ShaperAuxiliary
            let mutable j = i
            while j > start && propArray[j - 1].ShaperAuxiliary > key do
                j <- j - 1
            match j = i with
            | true -> ()
            | false ->
                mergeClusters level infos j (i + 1)
                move infos props i j
            i <- i + 1

    /// <summary>Gives every glyph of a range but the first the bits of a mask, in place.</summary>
    /// <param name="infos">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="start">The first index, which is left alone.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="mask">The bits.</param>
    let maskAfterFirst (infos: ImmutableArray<GlyphInfo>) (start: int) (finish: int) (mask: uint32) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let mutable index = start + 1
        while index < finish do
            array[index] <- { array[index] with Mask = array[index].Mask ||| mask }
            index <- index + 1

    /// <summary>Walks the glyphs of a run as <c>hb_syllabic_insert_dotted_circles</c> does, counting the dotted circles it inserts and,
    /// when asked, writing the glyphs and the circles into output arrays.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties.</param>
    /// <param name="outInfos">The output glyphs, written only when <paramref name="write"/>.</param>
    /// <param name="outProps">The output properties, written only when <paramref name="write"/>.</param>
    /// <param name="write">Whether to write the output.</param>
    /// <param name="glyph">The glyph of the dotted circle.</param>
    /// <param name="brokenType">The broken syllable type.</param>
    /// <param name="category">The category of the dotted circle.</param>
    /// <param name="position">The position of the dotted circle, zero when none.</param>
    /// <param name="hasRepha">Whether the circle goes after the leading repha glyphs of its syllable.</param>
    /// <param name="repha">The repha category.</param>
    /// <returns>The number of dotted circles.</returns>
    let private walkDottedCircles
            (infos: GlyphInfo array) (props: GlyphProps array) (outInfos: GlyphInfo array) (outProps: GlyphProps array) (write: bool)
            (glyph: uint32) (brokenType: byte) (category: byte) (position: byte) (hasRepha: bool) (repha: byte) =
        let mutable last = 0uy
        let mutable inserted = 0
        let mutable index = 0
        let mutable out = 0
        while index < infos.Length do
            let syllable = props[index].Syllable
            let broken = last <> syllable && (syllable &&& 0x0Fuy) = brokenType
            let circle =
                { Codepoint = glyph; Mask = infos[index].Mask; Cluster = infos[index].Cluster; Var1 = 0u; Var2 = 0u }
            let circleProps =
                { UnicodeProps = 0us
                  GlyphClass = 0us
                  LigProps = 0uy
                  Syllable = syllable
                  ShaperCategory = category
                  ShaperAuxiliary = position
                  GlyphIndex = 0u }
            match broken with
            | false ->
                match write with
                | true ->
                    outInfos[out] <- infos[index]
                    outProps[out] <- props[index]
                | false -> ()
                index <- index + 1
                out <- out + 1
            | true ->
                last <- syllable
                let skip = hasRepha && props[index].ShaperCategory = repha
                match skip with
                | false -> ()
                | true ->
                    let mutable running = true
                    while running do
                        match write with
                        | true ->
                            outInfos[out] <- infos[index]
                            outProps[out] <- props[index]
                        | false -> ()
                        index <- index + 1
                        out <- out + 1
                        running <- index < infos.Length && props[index].Syllable = last && props[index].ShaperCategory = repha
                match write with
                | true ->
                    outInfos[out] <- circle
                    outProps[out] <- circleProps
                | false -> ()
                out <- out + 1
                inserted <- inserted + 1
        inserted

    /// <summary>Inserts a dotted circle before every broken syllable, after its leading repha glyphs when the shaper has them,
    /// <c>hb_syllabic_insert_dotted_circles</c>: one scan counts the circles, and the output arrays are made once at their size.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties.</param>
    /// <param name="glyph">The glyph of the dotted circle.</param>
    /// <param name="brokenType">The broken syllable type.</param>
    /// <param name="category">The category of the dotted circle.</param>
    /// <param name="position">The position of the dotted circle, zero when none.</param>
    /// <param name="hasRepha">Whether the circle goes after the leading repha glyphs of its syllable.</param>
    /// <param name="repha">The repha category.</param>
    /// <returns>The glyphs and the properties, the arrays given when no circle is inserted.</returns>
    let insertDottedCircles
            (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>)
            (glyph: uint32) (brokenType: byte) (category: byte) (position: byte) (hasRepha: bool) (repha: byte) =
        let infoArray = nonNull (ImmutableCollectionsMarshal.AsArray infos)
        let propArray = nonNull (ImmutableCollectionsMarshal.AsArray props)
        let none = Array.empty
        let count = walkDottedCircles infoArray propArray none none false glyph brokenType category position hasRepha repha
        match count with
        | 0 -> struct (infos, props)
        | _ ->
            let outInfos = Array.zeroCreate (infoArray.Length + count)
            let outProps = Array.zeroCreate (infoArray.Length + count)
            walkDottedCircles infoArray propArray outInfos outProps true glyph brokenType category position hasRepha repha |> ignore
            struct (ImmutableCollectionsMarshal.AsImmutableArray outInfos, ImmutableCollectionsMarshal.AsImmutableArray outProps)

    /// <summary>Writes the two values of a pair array in place, so that one array serves every test of a run that asks about a pair.</summary>
    /// <param name="pair">The array of two, made by the caller in this call and shared with no one that keeps it.</param>
    /// <param name="first">The first value.</param>
    /// <param name="second">The second value.</param>
    let setPair (pair: ImmutableArray<uint32>) (first: uint32) (second: uint32) =
        let array = nonNull (ImmutableCollectionsMarshal.AsArray pair)
        array[0] <- first
        array[1] <- second

/// <summary>What the glyf kernel reads from a face: a glyph's bytes, the four side bearings and advances that give its phantom points,
/// and its gvar data. A struct that holds the tables, so the kernel is specialised to it and calls nothing through an object.</summary>
type internal IGlyfSource =
    /// <summary>The bytes of a glyph, <c>glyph_for_gid</c>.</summary>
    /// <param name="gid">The glyph id.</param>
    /// <returns>The bytes, and false when the glyph is out of range or its loca entry is bad.</returns>
    abstract Glyph: gid: uint32 -> struct (ReadOnlyMemory<byte> * bool)

    /// <summary>The leading bearings and advances of a glyph in the horizontal and vertical metrics.</summary>
    /// <param name="gid">The glyph id.</param>
    /// <returns>The left side bearing, the top side bearing, the horizontal advance and the vertical advance.</returns>
    abstract Metrics: gid: uint32 -> struct (int * int * int * int)

    /// <summary>The gvar variation data of a glyph.</summary>
    /// <param name="gid">The glyph id.</param>
    /// <returns>The data, empty when the glyph has none.</returns>
    abstract VarData: gid: uint32 -> ReadOnlyMemory<byte>

/// <summary>The working memory of the glyf kernel on one thread: the arena the points of the glyph being loaded live in, the integer
/// scratch for point numbers, and the points assembled so far. A thread makes it at its first glyph and every glyph it loads after that
/// starts it again from the bottom, so a run allocates nothing; no other thread reads it.</summary>
[<Sealed; AllowNullLiteral>]
type internal GlyfWork =
    val mutable F: float32 array
    val mutable B: byte array
    val mutable I: int array
    val mutable OutX: float32 array
    val mutable OutY: float32 array
    val mutable FTop: int
    val mutable BTop: int
    val mutable ITop: int
    val mutable OutLen: int
    val mutable Budget: int64
    val mutable Edge: int
    val mutable Source: IGlyfSource
    val mutable Extended: bool
    val mutable AxisCount: int
    val mutable SharedTuples: ReadOnlyMemory<byte>
    val mutable SharedScalars: float32 array
    val mutable Coords: ImmutableArray<int>
    val mutable Flush: bool
    val mutable AnyPrivate: bool
    val mutable PrivateChecked: bool
    val mutable Bounded: bool
    val mutable MinX: float32
    val mutable MinY: float32
    val mutable MaxX: float32
    val mutable MaxY: float32

    /// <summary>Makes the first arrays of a thread.</summary>
    new () =
        { F = Array.zeroCreate 8192
          B = Array.zeroCreate 2048
          I = Array.zeroCreate 1024
          OutX = Array.zeroCreate 1024
          OutY = Array.zeroCreate 1024
          FTop = 0
          BTop = 0
          ITop = 0
          OutLen = 0
          Budget = 0L
          Edge = 0
          Source = Unchecked.defaultof<IGlyfSource>
          Extended = false
          AxisCount = 0
          SharedTuples = ReadOnlyMemory<byte>.Empty
          SharedScalars = Array.empty
          Coords = ImmutableArray<int>.Empty
          Flush = false
          AnyPrivate = false
          PrivateChecked = false
          Bounded = false
          MinX = 0.0f
          MinY = 0.0f
          MaxX = 0.0f
          MaxY = 0.0f }

/// <summary>The glyf and gvar kernel for the bounds of a glyph at variation coordinates: the contour points of a simple glyph read
/// straight into an arena, every gvar tuple applied to them, composites assembled in place, the four phantom points included, and the
/// bounds taken from the result; nothing is allocated beyond the rented arrays. It is <c>GlyphOutline.extentsWithVariations</c> operation by
/// operation, with the same float32 arithmetic in the same order. A glyph that is out of the ordinary (damaged data, a budget or a
/// nesting limit, a composite that nests deeper than any font does) is not answered: the caller runs the reference for it.</summary>
[<RequireQualifiedAccess>]
module internal GlyfKernel =
    [<Literal>]
    let private HeaderSize = 10

    [<Literal>]
    let private PhantomCount = 4

    [<Literal>]
    let private MaxDepth = 8

    [<Literal>]
    let private MaxEdges = 16384

    [<Literal>]
    let private MaxPoints = 200000

    [<Literal>]
    let private BudgetGlyph = 16777216L

    [<Literal>]
    let private Budget16 = 16L

    [<Literal>]
    let private ArgWords = 0x0001us

    [<Literal>]
    let private ArgsXy = 0x0002us

    [<Literal>]
    let private HaveScale = 0x0008us

    [<Literal>]
    let private MoreComponents = 0x0020us

    [<Literal>]
    let private HaveXYScale = 0x0040us

    [<Literal>]
    let private Have2x2 = 0x0080us

    [<Literal>]
    let private UseMyMetrics = 0x0200us

    [<Literal>]
    let private ScaledComponentOffset = 0x0800us

    [<Literal>]
    let private UnscaledComponentOffset = 0x1000us

    [<Literal>]
    let private Gid24 = 0x2000us

    [<Literal>]
    let private EmbeddedPeakFlag = 0x8000us

    [<Literal>]
    let private IntermediateRegionFlag = 0x4000us

    [<Literal>]
    let private PrivatePointsFlag = 0x2000us

    [<Literal>]
    let private SharedPointNumbersFlag = 0x8000us

    [<Literal>]
    let private TupleVarCountMask = 0x0FFFus

    [<Literal>]
    let private TupleIndexMask = 0x0FFFus

    [<Literal>]
    let private TupleHeaderMinSize = 4

    [<Literal>]
    let private GlyphVarDataMinSize = 4

    /// <summary>The working memory of each thread, made at the thread's first glyph.</summary>
    let private threadWork = new System.Threading.ThreadLocal<GlyfWork>(fun () -> GlyfWork())

    /// <summary>Reads a byte, 0 outside the span, as <c>Binary.u8</c> does.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The byte, or 0.</returns>
    let inline private u8At (data: ReadOnlySpan<byte>) (offset: int) =
        if uint64 (uint32 offset) + 1UL <= uint64 data.Length then data[offset] else 0uy

    /// <summary>Reads a big-endian uint16, 0 outside the span, as <c>Binary.u16</c> does.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline private u16At (data: ReadOnlySpan<byte>) (offset: int) =
        if uint64 (uint32 offset) + 2UL <= uint64 data.Length then System.Buffers.Binary.BinaryPrimitives.ReadUInt16BigEndian(data.Slice(offset, 2)) else 0us

    /// <summary>Reads a big-endian int16, 0 outside the span, as <c>Binary.i16</c> does.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline private i16At (data: ReadOnlySpan<byte>) (offset: int) =
        int16 (u16At data offset)

    /// <summary>Reads a big-endian uint32, 0 outside the span, as <c>Binary.u32</c> does.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline private u32At (data: ReadOnlySpan<byte>) (offset: int) =
        if uint64 (uint32 offset) + 4UL <= uint64 data.Length then System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(data.Slice(offset, 4)) else 0u

    /// <summary>Reads a big-endian 24-bit value, 0 outside the span, as <c>Binary.u24</c> does.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <returns>The value, or 0.</returns>
    let inline private u24At (data: ReadOnlySpan<byte>) (offset: int) =
        if uint64 (uint32 offset) + 3UL <= uint64 data.Length then (uint32 data[offset] <<< 16) ||| (uint32 data[offset + 1] <<< 8) ||| uint32 data[offset + 2] else 0u

    /// <summary>The smaller of two values, <c>hb_min</c>: a NaN operand never wins.</summary>
    /// <param name="a">The first value.</param>
    /// <param name="b">The second value.</param>
    /// <returns>The minimum.</returns>
    let inline private hbMin (a: float32) (b: float32) =
        if a <= b then a else b

    /// <summary>The larger of two values, <c>hb_max</c>: a NaN operand never wins.</summary>
    /// <param name="a">The first value.</param>
    /// <param name="b">The second value.</param>
    /// <returns>The maximum.</returns>
    let inline private hbMax (a: float32) (b: float32) =
        if a >= b then a else b

    /// <summary>Makes room in the float arena for a frame.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="needed">The floats the frame takes above the top.</param>
    let private reserveFloats (work: GlyfWork) (needed: int) =
        match work.FTop + needed > work.F.Length with
        | false -> ()
        | true ->
            let grown = Array.zeroCreate<float32> (max (work.F.Length * 2) (work.FTop + needed))
            Array.Copy(work.F, grown, work.FTop)
            work.F <- grown

    /// <summary>Makes room in the byte arena for a frame.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="needed">The bytes the frame takes above the top.</param>
    let private reserveBytes (work: GlyfWork) (needed: int) =
        match work.BTop + needed > work.B.Length with
        | false -> ()
        | true ->
            let grown = Array.zeroCreate<byte> (max (work.B.Length * 2) (work.BTop + needed))
            Array.Copy(work.B, grown, work.BTop)
            work.B <- grown

    /// <summary>Makes room in the integer scratch for point numbers or decoded deltas.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="needed">The integers taken above the top.</param>
    let private reserveInts (work: GlyfWork) (needed: int) =
        match work.ITop + needed > work.I.Length with
        | false -> ()
        | true ->
            let grown = Array.zeroCreate<int> (max (work.I.Length * 2) (work.ITop + needed))
            Array.Copy(work.I, grown, work.ITop)
            work.I <- grown

    /// <summary>Makes room in the output points for more of them.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="needed">The points to be appended.</param>
    let private reserveOut (work: GlyfWork) (needed: int) =
        match work.OutLen + needed > work.OutX.Length with
        | false -> ()
        | true ->
            let size = max (work.OutX.Length * 2) (work.OutLen + needed)
            let growX = Array.zeroCreate<float32> size
            let growY = Array.zeroCreate<float32> size
            Array.Copy(work.OutX, growX, work.OutLen)
            Array.Copy(work.OutY, growY, work.OutLen)
            work.OutX <- growX
            work.OutY <- growY

    /// <summary>The byte size of a tuple header, <c>TupleVariationHeader::get_size</c>.</summary>
    /// <param name="tupleIndex">The tuple index.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <returns>The size.</returns>
    let private headerSize (tupleIndex: uint16) (axisCountTimes2: int) =
        let hasPeak = tupleIndex &&& EmbeddedPeakFlag <> 0us
        let hasInterm = tupleIndex &&& IntermediateRegionFlag <> 0us
        match hasPeak || hasInterm with
        | false -> TupleHeaderMinSize
        | true -> TupleHeaderMinSize + ((if hasPeak then 1 else 0) + (if hasInterm then 2 else 0)) * axisCountTimes2

    /// <summary>Whether a tuple header at an offset is valid, <c>tuple_iterator_t::is_valid</c>.</summary>
    /// <param name="data">The glyph variation data.</param>
    /// <param name="headerAt">The header offset.</param>
    /// <param name="tuplesLeft">The tuples still to visit.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <returns>True when the header fits.</returns>
    let private headerValid (data: ReadOnlySpan<byte>) (headerAt: int) (tuplesLeft: int) (axisCountTimes2: int) =
        match tuplesLeft > 0 && headerAt + TupleHeaderMinSize <= data.Length with
        | false -> false
        | true -> headerAt + headerSize (u16At data (headerAt + 2)) axisCountTimes2 <= data.Length

    /// <summary>Whether any tuple from a header on carries private point numbers, <c>apply_deltas_to_points</c>' scan.</summary>
    /// <param name="data">The glyph variation data.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <param name="headerAt">The first header to scan.</param>
    /// <param name="tuplesLeft">The tuples to scan.</param>
    /// <returns>True when a tuple has private points.</returns>
    let private anyPrivate (data: ReadOnlySpan<byte>) (axisCountTimes2: int) (headerAt: int) (tuplesLeft: int) =
        let mutable at = headerAt
        let mutable left = tuplesLeft
        let mutable found = false
        let mutable scanning = true
        while scanning do
            match left = 0 with
            | true -> scanning <- false
            | false ->
                let tupleIndex = u16At data (at + 2)
                match tupleIndex &&& PrivatePointsFlag <> 0us with
                | true ->
                    found <- true
                    scanning <- false
                | false ->
                    let nextAt = at + headerSize tupleIndex axisCountTimes2
                    let nextLeft = left - 1
                    match headerValid data nextAt nextLeft axisCountTimes2 with
                    | false -> scanning <- false
                    | true ->
                        at <- nextAt
                        left <- nextLeft
        found
    /// <summary>The scalar of one axis of a tuple that has an intermediate region, once the peak and the coordinate are known to be
    /// non-zero and apart, <c>TupleVariationHeader::calculate_scalar</c>.</summary>
    /// <param name="peak">The peak.</param>
    /// <param name="v">The coordinate.</param>
    /// <param name="start">The intermediate start.</param>
    /// <param name="finish">The intermediate end.</param>
    /// <param name="scalar">The product so far.</param>
    /// <returns>The product with this axis, 0 when the axis rules the tuple out.</returns>
    let private intermediateScalar (peak: int) (v: int) (start: int) (finish: int) (scalar: float32) =
        match start > peak || peak > finish || (start < 0 && finish > 0) with
        | true -> scalar
        | false ->
            match v < start || v > finish with
            | true -> 0.0f
            | false ->
                match v < peak, peak <> start, peak <> finish with
                | true, true, _ -> scalar * (float32 (v - start) / float32 (peak - start))
                | true, false, _ -> scalar
                | false, _, true -> scalar * (float32 (finish - v) / float32 (finish - peak))
                | false, _, false -> scalar

    /// <summary>The scalar of one axis of a tuple, <c>TupleVariationHeader::calculate_scalar</c>.</summary>
    /// <param name="peak">The peak.</param>
    /// <param name="v">The coordinate.</param>
    /// <param name="hasInterm">Whether the tuple has an intermediate region.</param>
    /// <param name="start">The intermediate start.</param>
    /// <param name="finish">The intermediate end.</param>
    /// <param name="scalar">The product so far.</param>
    /// <returns>The product with this axis, 0 when the axis rules the tuple out.</returns>
    let private axisScalar (peak: int) (v: int) (hasInterm: bool) (start: int) (finish: int) (scalar: float32) =
        match peak = 0 || v = peak with
        | true -> scalar
        | false ->
            match v = 0, hasInterm with
            | true, _ -> 0.0f
            | false, true -> intermediateScalar peak v start finish scalar
            | false, false ->
                match v < min 0 peak || v > max 0 peak with
                | true -> 0.0f
                | false -> scalar * (float32 v / float32 peak)

    /// <summary>The scalar of a tuple header with its own peak or an intermediate region, <c>TupleVariationHeader::calculate_scalar</c>.</summary>
    /// <param name="dataMemory">The glyph variation data.</param>
    /// <param name="sharedMemory">The shared tuple records.</param>
    /// <param name="coordArray">The normalized coordinates.</param>
    /// <param name="axisCount">The axis count.</param>
    /// <param name="headerAt">The header's offset.</param>
    /// <param name="tupleIndex">The header's tuple index.</param>
    /// <returns>The scalar.</returns>
    let private tupleScalar (dataMemory: ReadOnlyMemory<byte>) (sharedMemory: ReadOnlyMemory<byte>) (coordArray: ImmutableArray<int>) (axisCount: int) (headerAt: int) (tupleIndex: uint16) =
        let data = dataMemory.Span
        let sharedTuples = sharedMemory.Span
        let coords = coordArray.AsSpan()
        let index = int (tupleIndex &&& TupleIndexMask)
        let hasInterm = tupleIndex &&& IntermediateRegionFlag <> 0us
        let hasPeak = tupleIndex &&& EmbeddedPeakFlag <> 0us
        let startAt = headerAt + 4 + (if hasPeak then axisCount * 2 else 0)
        let endAt = startAt + axisCount * 2
        match (not hasPeak) && (index + 1) * axisCount > sharedTuples.Length / 2 with
        | true -> 0.0f
        | false ->
            let mutable scalar = 1.0f
            let mutable axis = 0
            while axis < axisCount do
                let peak = int (i16At (match hasPeak with | true -> data | false -> sharedTuples) (match hasPeak with | true -> headerAt + 4 + axis * 2 | false -> index * axisCount * 2 + axis * 2))
                let v = match axis < coords.Length with | true -> coords[axis] | false -> 0
                let start = match hasInterm with | true -> int (i16At data (startAt + axis * 2)) | false -> 0
                let finish = match hasInterm with | true -> int (i16At data (endAt + axis * 2)) | false -> 0
                scalar <- axisScalar peak v hasInterm start finish scalar
                axis <- match scalar = 0.0f with | true -> axisCount | false -> axis + 1
            scalar

    /// <summary>The offset after a packed point stream, checking every run, <c>decompile_points</c>' bounds.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="limit">The end of the region the stream must lie in.</param>
    /// <param name="dataAt">The offset of the first run.</param>
    /// <param name="count">The number of points.</param>
    /// <returns>The offset after the runs, or -1 when a run is out of range.</returns>
    let private pointsEnd (data: ReadOnlySpan<byte>) (limit: int) (dataAt: int) (count: int) =
        let mutable p = dataAt
        let mutable index = 0
        let mutable ok = true
        while ok && index < count do
            ok <- p < limit
            let control = u8At data p
            let runCount = int (control &&& 0x7Fuy) + 1
            let width = match control &&& 0x80uy <> 0uy with | true -> 2 | false -> 1
            ok <- ok && int64 index + int64 runCount <= int64 count && int64 p + 1L + int64 runCount * int64 width <= int64 limit
            p <- p + 1 + runCount * width
            index <- index + runCount
        match ok with
        | true -> p
        | false -> -1

    /// <summary>Decodes a packed point stream into the integer scratch, <c>TupleVariationData::decompile_points</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="data">The bytes.</param>
    /// <param name="limit">The end of the region the stream must lie in.</param>
    /// <param name="at">The offset of the stream.</param>
    /// <param name="dest">The scratch index the point numbers go to.</param>
    /// <returns>The point count and the offset after the stream, or a negative count when the stream is truncated.</returns>
    let private decodePoints (work: GlyfWork) (data: ReadOnlySpan<byte>) (limit: int) (at: int) (dest: int) =
        let first = u8At data at
        let wide = first &&& 0x80uy <> 0uy
        let count = match wide with | true -> ((int (first &&& 0x7Fuy)) <<< 8) ||| int (u8At data (at + 1)) | false -> int first
        let dataAt = match wide with | true -> at + 2 | false -> at + 1
        let finish = match at < 0 || at >= limit || (wide && at + 1 >= limit) with | true -> -1 | false -> pointsEnd data limit dataAt count
        match finish < 0 with
        | true -> struct (-1, 0)
        | false ->
            reserveInts work (dest + count - work.ITop)
            let target = work.I
            let mutable q = dataAt
            let mutable written = 0
            let mutable n = 0u
            while written < count do
                let control = data[q]
                let words = control &&& 0x80uy <> 0uy
                let runCount = int (control &&& 0x7Fuy) + 1
                q <- q + 1
                let mutable k = 0
                while k < runCount do
                    n <- n + (match words with | true -> uint32 (u16At data q) | false -> uint32 data[q])
                    q <- q + (match words with | true -> 2 | false -> 1)
                    target[dest + written] <- int n
                    written <- written + 1
                    k <- k + 1
            struct (count, finish)

    /// <summary>The offset after a packed delta stream, checking every run, <c>TupleValues::decompile</c>' bounds.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="limit">The end of the region the stream must lie in.</param>
    /// <param name="at">The offset of the stream.</param>
    /// <param name="count">The number of deltas.</param>
    /// <returns>The offset after the stream, or -1 when it is truncated.</returns>
    let private deltasEnd (data: ReadOnlySpan<byte>) (limit: int) (at: int) (count: int) =
        let mutable p = at
        let mutable index = 0
        let mutable ok = at >= 0
        while ok && index < count do
            match p >= limit with
            | true -> ok <- false
            | false ->
                let control = data[p]
                let runCount = int (control &&& 0x3Fuy) + 1
                let zeros = control &&& 0x80uy <> 0uy
                let words = control &&& 0x40uy <> 0uy
                let width = match zeros, words with | true, false -> 0 | false, true -> 2 | true, true -> 4 | false, false -> 1
                match int64 index + int64 runCount > int64 count || int64 p + 1L + int64 runCount * int64 width > int64 limit with
                | true -> ok <- false
                | false ->
                    p <- p + 1 + runCount * width
                    index <- index + runCount
        match ok with
        | true -> p
        | false -> -1

    /// <summary>Walks a packed delta stream already checked, handing every delta with its index to a body: the runs of zeros, bytes, words
    /// and longs are told apart once each, not once a delta.</summary>
    /// <param name="data">The bytes.</param>
    /// <param name="at">The offset of the stream.</param>
    /// <param name="count">The number of deltas.</param>
    /// <param name="body">What to do with a delta: its index and its signed value.</param>
    let inline private forEachDelta (data: ReadOnlySpan<byte>) (at: int) (count: int) ([<InlineIfLambda>] body: int -> int -> unit) =
        let mutable q = at
        let mutable written = 0
        while written < count do
            let control = data[q]
            let runCount = int (control &&& 0x3Fuy) + 1
            q <- q + 1
            match control &&& 0xC0uy with
            | 0x80uy ->
                let mutable k = 0
                while k < runCount do
                    body (written + k) 0
                    k <- k + 1
            | 0x40uy ->
                let mutable k = 0
                while k < runCount do
                    body (written + k) (int (i16At data q))
                    q <- q + 2
                    k <- k + 1
            | 0xC0uy ->
                let mutable k = 0
                while k < runCount do
                    body (written + k) (int (u32At data q))
                    q <- q + 4
                    k <- k + 1
            | _ ->
                let mutable k = 0
                while k < runCount do
                    body (written + k) (int (sbyte data[q]))
                    q <- q + 1
                    k <- k + 1
            written <- written + runCount

    /// <summary>The delta of a point between two referenced points, <c>infer_delta</c>.</summary>
    /// <param name="pointCoord">The point's coordinate.</param>
    /// <param name="prevCoord">The previous referenced coordinate.</param>
    /// <param name="nextCoord">The next referenced coordinate.</param>
    /// <param name="prevDelta">The previous referenced delta.</param>
    /// <param name="nextDelta">The next referenced delta.</param>
    /// <returns>The inferred delta.</returns>
    let inline private inferDelta (pointCoord: float32) (prevCoord: float32) (nextCoord: float32) (prevDelta: float32) (nextDelta: float32) =
        match prevCoord = nextCoord with
        | true -> (if prevDelta = nextDelta then prevDelta else 0.0f)
        | false ->
            match pointCoord <= hbMin prevCoord nextCoord with
            | true -> (if prevCoord < nextCoord then prevDelta else nextDelta)
            | false ->
                match pointCoord >= hbMax prevCoord nextCoord with
                | true -> (if prevCoord > nextCoord then prevDelta else nextDelta)
                | false ->
                    let r = (pointCoord - prevCoord) / (nextCoord - prevCoord)
                    prevDelta + r * (nextDelta - prevDelta)

    /// <summary>Infers the deltas of the unreferenced points of one contour from the referenced ones, <c>apply_deltas_to_points</c>' IUP pass.</summary>
    /// <param name="f">The float arena.</param>
    /// <param name="b">The byte arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <param name="start">The contour's first point.</param>
    /// <param name="finish">The contour's last point.</param>
    /// <param name="unreferenced">The number of unreferenced points in the contour.</param>
    let private iupContour (f: float32 array) (b: byte array) (fb: int) (bb: int) (n: int) (start: int) (finish: int) (unreferenced: int) =
        let ox = fb + 2 * n
        let oy = fb + 3 * n
        let dx = fb + 4 * n
        let dy = fb + 5 * n
        let referenced = bb
        let mutable remaining = unreferenced
        let mutable j = start
        while remaining <> 0 do
            let mutable prev = j
            let mutable searching = true
            while searching do
                let jNext = match prev >= finish with | true -> start | false -> prev + 1
                searching <- not (b[referenced + prev] <> 0uy && b[referenced + jNext] = 0uy)
                prev <- match searching with | true -> jNext | false -> prev
            let mutable next = prev
            searching <- true
            while searching do
                let jNext = match next >= finish with | true -> start | false -> next + 1
                searching <- not (b[referenced + next] = 0uy && b[referenced + jNext] <> 0uy)
                next <- jNext
            let mutable i = prev
            let mutable filling = true
            while filling do
                let iNext = match i >= finish with | true -> start | false -> i + 1
                filling <- iNext <> next
                match filling with
                | false -> ()
                | true ->
                    f[dx + iNext] <- inferDelta f[ox + iNext] f[ox + prev] f[ox + next] f[dx + prev] f[dx + next]
                    f[dy + iNext] <- inferDelta f[oy + iNext] f[oy + prev] f[oy + next] f[dy + prev] f[dy + next]
                    remaining <- remaining - 1
                    filling <- remaining <> 0
                    i <- iNext
            j <- next

    /// <summary>Infers the deltas of the unreferenced points of every contour from the referenced ones, <c>apply_deltas_to_points</c>' IUP pass.</summary>
    /// <param name="f">The float arena.</param>
    /// <param name="b">The byte arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    let private iup (f: float32 array) (b: byte array) (fb: int) (bb: int) (n: int) =
        let referenced = bb
        let isEnd = bb + n
        let mutable start = 0
        let mutable running = true
        while running do
            let mutable finish = start
            while finish < n && b[isEnd + finish] = 0uy do
                finish <- finish + 1
            running <- finish < n
            let mutable refCount = 0
            let mutable k = start
            while running && k <= finish do
                refCount <- refCount + (match b[referenced + k] <> 0uy with | true -> 1 | false -> 0)
                k <- k + 1
            let remaining = (finish - start + 1) - refCount
            match running && remaining <> 0 && remaining <= finish - start with
            | false -> ()
            | true -> iupContour f b fb bb n start finish remaining
            start <- finish + 1

    /// <summary>Adds the accumulated deltas into the point coordinates and clears them, <c>DeltaBuffer.flush</c> over every point.</summary>
    /// <param name="f">The float arena.</param>
    /// <param name="b">The byte arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    let private flushDeltas (f: float32 array) (b: byte array) (fb: int) (bb: int) (n: int) =
        let dx = fb + 4 * n
        let dy = fb + 5 * n
        let mutable index = 0
        while index < n do
            f[fb + index] <- f[fb + index] + f[dx + index]
            f[fb + n + index] <- f[fb + n + index] + f[dy + index]
            f[dx + index] <- 0.0f
            f[dy + index] <- 0.0f
            b[bb + index] <- 0uy
            index <- index + 1

    /// <summary>Clears the accumulated deltas without touching the coordinates, <c>DeltaBuffer.clear</c> over every point.</summary>
    /// <param name="f">The float arena.</param>
    /// <param name="b">The byte arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    let private clearDeltas (f: float32 array) (b: byte array) (fb: int) (bb: int) (n: int) =
        let dx = fb + 4 * n
        let dy = fb + 5 * n
        let mutable index = 0
        while index < n do
            f[dx + index] <- 0.0f
            f[dy + index] <- 0.0f
            b[bb + index] <- 0uy
            index <- index + 1
    /// <summary>Adds a tuple's scaled deltas straight into the points of a frame, the tuple applying to every point.</summary>
    /// <param name="f">The float arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="n">The point count.</param>
    /// <param name="varMemory">The glyph variation data.</param>
    /// <param name="xAt">The offset of the x deltas.</param>
    /// <param name="yAt">The offset of the y deltas.</param>
    /// <param name="scalar">The tuple scalar.</param>
    let private addToPoints (f: float32 array) (fb: int) (n: int) (varMemory: ReadOnlyMemory<byte>) (xAt: int) (yAt: int) (scalar: float32) =
        let varData = varMemory.Span
        forEachDelta varData xAt n (fun index value -> f[fb + index] <- f[fb + index] + float32 value * scalar)
        forEachDelta varData yAt n (fun index value -> f[fb + n + index] <- f[fb + n + index] + float32 value * scalar)

    /// <summary>Accumulates a tuple's scaled deltas into the delta arrays of a frame, the tuple applying to every point.</summary>
    /// <param name="f">The float arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="n">The point count.</param>
    /// <param name="varMemory">The glyph variation data.</param>
    /// <param name="xAt">The offset of the x deltas.</param>
    /// <param name="yAt">The offset of the y deltas.</param>
    /// <param name="scalar">The tuple scalar.</param>
    let private accumulateAll (f: float32 array) (fb: int) (n: int) (varMemory: ReadOnlyMemory<byte>) (xAt: int) (yAt: int) (scalar: float32) =
        let varData = varMemory.Span
        let dx = fb + 4 * n
        let dy = fb + 5 * n
        forEachDelta varData xAt n (fun index value -> f[dx + index] <- f[dx + index] + float32 value * scalar)
        forEachDelta varData yAt n (fun index value -> f[dy + index] <- f[dy + index] + float32 value * scalar)

    /// <summary>Accumulates a tuple's scaled deltas into the delta arrays of a frame at the points the tuple names, marking each,
    /// <c>accumulate_indexed</c>, then infers the deltas of the points it leaves out.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <param name="varMemory">The glyph variation data.</param>
    /// <param name="xAt">The offset of the x deltas.</param>
    /// <param name="yAt">The offset of the y deltas.</param>
    /// <param name="count">The number of deltas.</param>
    /// <param name="indexBase">Where the point numbers are in the integer scratch.</param>
    /// <param name="scalar">The tuple scalar.</param>
    let private accumulateIndexed (work: GlyfWork) (fb: int) (bb: int) (n: int) (varMemory: ReadOnlyMemory<byte>) (xAt: int) (yAt: int) (count: int) (indexBase: int) (scalar: float32) =
        let varData = varMemory.Span
        let f = work.F
        let b = work.B
        let ints = work.I
        let dx = fb + 4 * n
        let dy = fb + 5 * n
        forEachDelta varData xAt count (fun index value ->
            let point = ints[indexBase + index]
            match point >= n || point < 0 with
            | true -> ()
            | false ->
                f[dx + point] <- f[dx + point] + float32 value * scalar
                b[bb + point] <- 1uy)
        forEachDelta varData yAt count (fun index value ->
            let point = ints[indexBase + index]
            match point >= n || point < 0 with
            | true -> ()
            | false -> f[dy + point] <- f[dy + point] + float32 value * scalar)
        iup f b fb bb n

    /// <summary>Applies one tuple's decoded stream: straight into the points when it applies to every point and no tuple has private
    /// points, else through the delta arrays, as <c>apply_deltas_to_points</c> does.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="xAt">The offset of the x deltas.</param>
    /// <param name="yAt">The offset of the y deltas.</param>
    /// <param name="count">The number of deltas.</param>
    /// <param name="applyToAll">Whether the tuple applies to every point.</param>
    /// <param name="indexBase">Where the point numbers are in the integer scratch.</param>
    /// <param name="scalar">The tuple scalar.</param>
    let private applyStreams (work: GlyfWork) (fb: int) (bb: int) (n: int) (varData: ReadOnlyMemory<byte>) (xAt: int) (yAt: int) (count: int) (applyToAll: bool) (indexBase: int) (scalar: float32) =
        match applyToAll, work.AnyPrivate with
        | true, false -> addToPoints work.F fb n varData xAt yAt scalar
        | true, true ->
            accumulateAll work.F fb n varData xAt yAt scalar
            work.Flush <- true
        | false, _ ->
            match work.Flush with
            | true -> flushDeltas work.F work.B fb bb n
            | false -> clearDeltas work.F work.B fb bb n
            accumulateIndexed work fb bb n varData xAt yAt count indexBase scalar
            work.Flush <- true

    /// <summary>Applies one tuple whose scalar is not zero: reads its point numbers, checks both delta streams, and applies them.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="tupleIndex">The tuple index of the header.</param>
    /// <param name="varDataSize">The size of the tuple's data.</param>
    /// <param name="serializedAt">The offset of the tuple's data.</param>
    /// <param name="sharedCount">The number of shared point numbers.</param>
    /// <param name="iBase">Where the shared point numbers are in the integer scratch.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <returns>False when the data is not the ordinary kind.</returns>
    let private applyTuple (work: GlyfWork) (varData: ReadOnlyMemory<byte>) (tupleIndex: uint16) (varDataSize: int) (serializedAt: int) (sharedCount: int) (iBase: int) (scalar: float32) (fb: int) (bb: int) (n: int) =
        let span = varData.Span
        let limit = serializedAt + varDataSize
        let hasPrivate = tupleIndex &&& PrivatePointsFlag <> 0us
        let top = work.ITop
        let struct (count, deltasAt) =
            match hasPrivate with
            | false -> struct (sharedCount, serializedAt)
            | true -> decodePoints work span limit serializedAt top
        let applyToAll = count = 0
        let numDeltas = match applyToAll with | true -> n | false -> count
        let afterX = match count < 0 with | true -> -1 | false -> deltasEnd span limit deltasAt numDeltas
        let afterY = match afterX < 0 with | true -> -1 | false -> deltasEnd span limit afterX numDeltas
        match afterY < 0 with
        | true -> false
        | false ->
            work.ITop <- (match hasPrivate with | true -> top + count | false -> top)
            applyStreams work fb bb n varData deltasAt afterX numDeltas applyToAll (match hasPrivate with | true -> top | false -> iBase) scalar
            true

    /// <summary>The scalar of a tuple header: the shared one worked out at the coordinates when the header names a shared tuple, else the
    /// header's own.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="headerAt">The header's offset.</param>
    /// <param name="tupleIndex">The header's tuple index.</param>
    /// <returns>The scalar.</returns>
    let private headerScalar (work: GlyfWork) (varData: ReadOnlyMemory<byte>) (headerAt: int) (tupleIndex: uint16) =
        match tupleIndex &&& (EmbeddedPeakFlag ||| IntermediateRegionFlag) = 0us with
        | false -> tupleScalar varData work.SharedTuples work.Coords work.AxisCount headerAt tupleIndex
        | true ->
            let index = int (tupleIndex &&& TupleIndexMask)
            match index < work.SharedScalars.Length with
            | true -> work.SharedScalars[index]
            | false -> 0.0f

    /// <summary>Visits one tuple header: skips it when its scalar is zero, else applies it.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="data">The offset of the serialized data.</param>
    /// <param name="iBase">Where the shared point numbers are in the integer scratch.</param>
    /// <param name="sharedCount">The number of shared point numbers.</param>
    /// <param name="headerAt">The header's offset.</param>
    /// <param name="left">The tuples still to visit, this one included.</param>
    /// <param name="tupleIndex">The header's tuple index.</param>
    /// <param name="varDataSize">The size of the tuple's data.</param>
    /// <param name="dataOffset">The offset of the tuple's data from the serialized data.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <returns>False when the data is not the ordinary kind.</returns>
    let private visitTuple (work: GlyfWork) (varData: ReadOnlyMemory<byte>) (data: int) (iBase: int) (sharedCount: int) (headerAt: int) (left: int) (tupleIndex: uint16) (varDataSize: int) (dataOffset: int) (fb: int) (bb: int) (n: int) =
        let scalar = headerScalar work varData headerAt tupleIndex
        match scalar = 0.0f with
        | true -> true
        | false ->
            match work.PrivateChecked with
            | true -> ()
            | false ->
                work.AnyPrivate <- anyPrivate varData.Span (work.AxisCount * 2) headerAt left
                work.PrivateChecked <- true
            let serializedAt = data + dataOffset
            match data = 0 || varDataSize > varData.Length - serializedAt with
            | true -> false
            | false -> applyTuple work varData tupleIndex varDataSize serializedAt sharedCount iBase scalar fb bb n

    /// <summary>Walks the tuples of a glyph's variation data, applying each, <c>apply_deltas_to_points</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="varMemory">The glyph variation data.</param>
    /// <param name="data">The offset of the serialized data.</param>
    /// <param name="iBase">Where the shared point numbers are in the integer scratch.</param>
    /// <param name="sharedCount">The number of shared point numbers.</param>
    /// <param name="sharedConsumed">The bytes the shared point numbers take.</param>
    /// <param name="tupleCount">The number of tuples.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <returns>False when the data is not the ordinary kind.</returns>
    let private walkTuples (work: GlyfWork) (varMemory: ReadOnlyMemory<byte>) (data: int) (iBase: int) (sharedCount: int) (sharedConsumed: int) (tupleCount: int) (fb: int) (bb: int) (n: int) =
        let varData = varMemory.Span
        let axisTimes2 = work.AxisCount * 2
        let mutable headerAt = TupleHeaderMinSize
        let mutable dataOffset = sharedConsumed
        let mutable left = tupleCount
        let mutable ok = true
        let mutable walking = true
        while walking do
            let tupleIndex = u16At varData (headerAt + 2)
            let varDataSize = int (u16At varData headerAt)
            ok <- visitTuple work varMemory data iBase sharedCount headerAt left tupleIndex varDataSize dataOffset fb bb n
            let nextAt = headerAt + headerSize tupleIndex axisTimes2
            left <- left - 1
            walking <- ok && headerValid varData nextAt left axisTimes2
            headerAt <- nextAt
            dataOffset <- dataOffset + varDataSize
        ok

    /// <summary>Applies the gvar deltas of a glyph that has tuples: the shared point numbers, the walk, and the last flush.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="varMemory">The glyph variation data.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <returns>False when the data is not the ordinary kind.</returns>
    let private applyTuples (work: GlyfWork) (varMemory: ReadOnlyMemory<byte>) (fb: int) (bb: int) (n: int) =
        let varData = varMemory.Span
        let tupleVarCount = u16At varData 0
        let tupleCount = int (tupleVarCount &&& TupleVarCountMask)
        let data = int (u16At varData 2)
        let iBase = work.ITop
        let hasShared = tupleVarCount &&& SharedPointNumbersFlag <> 0us && data <> 0
        let struct (sharedCount, sharedEnd) =
            match hasShared with
            | false -> struct (0, 0)
            | true -> decodePoints work varData varData.Length data iBase
        match sharedCount < 0 || not (headerValid varData TupleHeaderMinSize tupleCount (work.AxisCount * 2)) with
        | true -> false
        | false ->
            work.ITop <- iBase + sharedCount
            work.Flush <- false
            work.AnyPrivate <- false
            work.PrivateChecked <- false
            let ok = walkTuples work varMemory data iBase sharedCount (match hasShared with | true -> sharedEnd - data | false -> 0) tupleCount fb bb n
            match ok && work.Flush with
            | true -> flushDeltas work.F work.B fb bb n
            | false -> ()
            work.ITop <- iBase
            ok

    /// <summary>Applies a glyph's gvar deltas to the points of a frame, <c>apply_deltas_to_points</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="varData">The glyph's variation data.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="n">The point count.</param>
    /// <returns>False when the data is not the ordinary kind and the reference has to answer.</returns>
    let private applyGvar (work: GlyfWork) (varData: ReadOnlyMemory<byte>) (fb: int) (bb: int) (n: int) =
        match varData.Length < GlyphVarDataMinSize || (u16At varData.Span 0 &&& TupleVarCountMask) = 0us with
        | true -> true
        | false -> applyTuples work varData fb bb n

    /// <summary>Appends a frame's points to the output.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="n">The point count.</param>
    let private appendFrame (work: GlyfWork) (fb: int) (n: int) =
        reserveOut work n
        Array.Copy(work.F, fb, work.OutX, work.OutLen, n)
        Array.Copy(work.F, fb + n, work.OutY, work.OutLen, n)
        work.OutLen <- work.OutLen + n

    /// <summary>Reads the run-length flag stream of a simple glyph into the flag bytes of its frame, and counts the coordinate bytes
    /// each axis will read, <c>read_flags</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="flagsAt">The offset of the first flag byte.</param>
    /// <param name="numPoints">The number of points.</param>
    /// <param name="b">The byte arena.</param>
    /// <param name="flagsBase">Where the flags of the points go.</param>
    /// <returns>Whether the flags hold, the offset after them, and the coordinate bytes of x and of y.</returns>
    let private scanFlags (glyph: ReadOnlySpan<byte>) (flagsAt: int) (numPoints: int) (b: byte array) (flagsBase: int) =
        let mutable at = flagsAt
        let mutable index = 0
        let mutable ok = true
        let mutable xBytes = 0
        let mutable yBytes = 0
        while ok && index < numPoints do
            match at >= glyph.Length with
            | true -> ok <- false
            | false ->
                let flag = int glyph[at]
                let hasRepeat = (flag >>> 3) &&& 1
                match hasRepeat = 1 && at + 1 >= glyph.Length with
                | true -> ok <- false
                | false ->
                    let repeat = hasRepeat * int (u8At glyph (at + 1))
                    at <- at + 1 + hasRepeat
                    let xShort = (flag >>> 1) &&& 1
                    let xSame = (flag >>> 4) &&& 1
                    let yShort = (flag >>> 2) &&& 1
                    let ySame = (flag >>> 5) &&& 1
                    let runLength = min (1 + repeat) (numPoints - index)
                    let mutable k = 0
                    while k < runLength do
                        b[flagsBase + index + k] <- byte flag
                        k <- k + 1
                    xBytes <- xBytes + (xShort + 2 * (1 - xShort) * (1 - xSame)) * runLength
                    yBytes <- yBytes + (yShort + 2 * (1 - yShort) * (1 - ySame)) * runLength
                    index <- index + 1 + repeat
        struct (ok && index >= numPoints && at + xBytes + yBytes <= glyph.Length, at, xBytes, yBytes)

    /// <summary>Reads the coordinates of a simple glyph's points into its frame, each axis accumulating the deltas its flags describe.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="b">The byte arena.</param>
    /// <param name="flagsBase">Where the flags of the points are.</param>
    /// <param name="f">The float arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="n">The point count of the frame.</param>
    /// <param name="numPoints">The number of contour points.</param>
    /// <param name="xAt">The offset of the x coordinates.</param>
    /// <param name="yAt">The offset of the y coordinates.</param>
    let private readCoordinates (glyph: ReadOnlySpan<byte>) (b: byte array) (flagsBase: int) (f: float32 array) (fb: int) (n: int) (numPoints: int) (xAt: int) (yAt: int) =
        let mutable vx = 0
        let mutable vy = 0
        let mutable px = xAt
        let mutable py = yAt
        let mutable index = 0
        while index < numPoints do
            let flag = int b[flagsBase + index]
            let xShort = (flag >>> 1) &&& 1
            let xSame = (flag >>> 4) &&& 1
            let xWord = (1 - xShort) * (1 - xSame)
            vx <- vx + xShort * (int (u8At glyph px) * (2 * xSame - 1)) + xWord * int (i16At glyph px)
            px <- px + xShort + 2 * xWord
            let yShort = (flag >>> 2) &&& 1
            let ySame = (flag >>> 5) &&& 1
            let yWord = (1 - yShort) * (1 - ySame)
            vy <- vy + yShort * (int (u8At glyph py) * (2 * ySame - 1)) + yWord * int (i16At glyph py)
            py <- py + yShort + 2 * yWord
            f[fb + index] <- float32 vx
            f[fb + n + index] <- float32 vy
            index <- index + 1

    /// <summary>Writes the four phantom points of a glyph into its frame and copies the points as the originals the gap inference reads,
    /// <c>Glyph::get_points</c>' phantom initialisation.</summary>
    /// <param name="f">The float arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="n">The point count of the frame.</param>
    /// <param name="first">The index of the first phantom.</param>
    /// <param name="hDelta">The glyph's x minimum less its left side bearing.</param>
    /// <param name="hAdv">The horizontal advance.</param>
    /// <param name="vOrig">The glyph's y maximum plus its top side bearing.</param>
    /// <param name="vAdv">The vertical advance.</param>
    let private setPhantoms (f: float32 array) (fb: int) (n: int) (first: int) (hDelta: int) (hAdv: int) (vOrig: int) (vAdv: int) =
        f[fb + first] <- float32 hDelta
        f[fb + n + first] <- 0.0f
        f[fb + first + 1] <- float32 (hAdv + hDelta)
        f[fb + n + first + 1] <- 0.0f
        f[fb + first + 2] <- 0.0f
        f[fb + n + first + 2] <- float32 vOrig
        f[fb + first + 3] <- 0.0f
        f[fb + n + first + 3] <- float32 (vOrig - vAdv)
        Array.Copy(f, fb, f, fb + 2 * n, 2 * n)
        Array.Clear(f, fb + 4 * n, 2 * n)

    /// <summary>The bounds of the contour points of a frame once the left phantom's shift is taken off x, which is how a glyph at the top
    /// of the nesting is read: the phantoms themselves are left out.</summary>
    /// <param name="work">The working memory, which keeps the bounds.</param>
    /// <param name="f">The float arena.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="n">The point count of the frame.</param>
    /// <param name="numPoints">The number of contour points.</param>
    let private keepBounds (work: GlyfWork) (f: float32 array) (fb: int) (n: int) (numPoints: int) =
        let shift = f[fb + n - PhantomCount]
        let mutable minX = Single.MaxValue
        let mutable minY = Single.MaxValue
        let mutable maxX = -Single.MaxValue
        let mutable maxY = -Single.MaxValue
        let mutable i = 0
        while i < numPoints do
            let x = f[fb + i] - shift
            let y = f[fb + n + i]
            minX <- hbMin minX x
            minY <- hbMin minY y
            maxX <- hbMax maxX x
            maxY <- hbMax maxY y
            i <- i + 1
        work.MinX <- minX
        work.MinY <- minY
        work.MaxX <- maxX
        work.MaxY <- maxY
        work.Bounded <- true
        work.OutLen <- n
    /// <summary>Takes the arena for a frame of points and clears its flags.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="n">The point count.</param>
    /// <param name="bytesPerPoint">The bytes of the byte arena each point takes.</param>
    /// <returns>The float base and the byte base of the frame.</returns>
    let private takeFrame (work: GlyfWork) (n: int) (bytesPerPoint: int) =
        let fb = work.FTop
        let bb = work.BTop
        reserveFloats work (6 * n)
        reserveBytes work (bytesPerPoint * n)
        work.FTop <- fb + 6 * n
        work.BTop <- bb + bytesPerPoint * n
        Array.Clear(work.B, bb, 2 * n)
        struct (fb, bb)

    /// <summary>Gives a frame back to the arena.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    let private releaseFrame (work: GlyfWork) (fb: int) (bb: int) =
        work.FTop <- fb
        work.BTop <- bb

    /// <summary>Marks the end point of every contour of a simple glyph in its frame.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="b">The byte arena.</param>
    /// <param name="isEnd">Where the end-point flags of the frame are.</param>
    /// <param name="numContours">The number of contours.</param>
    /// <param name="numPoints">The number of contour points.</param>
    let private markEndPoints (glyph: ReadOnlySpan<byte>) (b: byte array) (isEnd: int) (numContours: int) (numPoints: int) =
        let mutable c = 0
        while c < numContours do
            let endPoint = int (u16At glyph (HeaderSize + 2 * c))
            match endPoint < numPoints with
            | true -> b[isEnd + endPoint] <- 1uy
            | false -> ()
            c <- c + 1

    /// <summary>Finishes a loaded frame: keeps the bounds of a glyph at the top, appends the points of any other.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="n">The point count.</param>
    /// <param name="numPoints">The number of contour points.</param>
    /// <param name="depth">The nesting depth.</param>
    let private finishFrame (work: GlyfWork) (fb: int) (n: int) (numPoints: int) (depth: int) =
        match depth = 0 with
        | false -> appendFrame work fb n
        | true -> keepBounds work work.F fb n numPoints

    /// <summary>Reads a simple glyph into its frame, phantoms and deltas included, once its flags are known to hold.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="numContours">The number of contours.</param>
    /// <param name="numPoints">The number of contour points.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="at">The offset of the x coordinates.</param>
    /// <param name="xBytes">The bytes of the x coordinates.</param>
    /// <returns>False when the gvar data is not the ordinary kind.</returns>
    let private fillSimple (work: GlyfWork) (gid: uint32) (memory: ReadOnlyMemory<byte>) (numContours: int) (numPoints: int) (fb: int) (bb: int) (at: int) (xBytes: int) =
        let glyph = memory.Span
        let n = numPoints + PhantomCount
        markEndPoints glyph work.B (bb + n) numContours numPoints
        readCoordinates glyph work.B (bb + 2 * n) work.F fb n numPoints at (at + xBytes)
        let struct (lsb, tsb, hAdv, vAdv) = work.Source.Metrics gid
        setPhantoms work.F fb n numPoints (int (i16At glyph 2) - lsb) hAdv (int (i16At glyph 8) + tsb) vAdv
        applyGvar work (work.Source.VarData gid) fb bb n

    /// <summary>Loads a simple glyph whose points are paid for: its frame, its flags, its coordinates and its deltas.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="numContours">The number of contours.</param>
    /// <param name="numPoints">The number of contour points.</param>
    /// <param name="flagsAt">The offset of the flags.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    let private loadSimpleFrame (work: GlyfWork) (gid: uint32) (memory: ReadOnlyMemory<byte>) (numContours: int) (numPoints: int) (flagsAt: int) (depth: int) =
        let n = numPoints + PhantomCount
        let struct (fb, bb) = takeFrame work n 3
        let struct (flagsHold, at, xBytes, _) = scanFlags memory.Span flagsAt numPoints work.B (bb + 2 * n)
        let loaded = flagsHold && fillSimple work gid memory numContours numPoints fb bb at xBytes
        match loaded with
        | true -> finishFrame work fb n numPoints depth
        | false -> ()
        releaseFrame work fb bb
        loaded

    /// <summary>Loads a simple glyph's points, phantoms and deltas and keeps its bounds or appends them, <c>assembleSimple</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    let private loadSimple (work: GlyfWork) (gid: uint32) (memory: ReadOnlyMemory<byte>) (depth: int) =
        let glyph = memory.Span
        let numContours = int (i16At glyph 0)
        let numPoints = int (u16At glyph (HeaderSize + 2 * (numContours - 1))) + 1
        let flagsAt = HeaderSize + 2 * (numContours + 1) + int (u16At glyph (HeaderSize + 2 * numContours))
        let fits = numContours > 0 && HeaderSize + 2 * (numContours + 1) <= glyph.Length && numPoints >= numContours && numPoints + PhantomCount <= MaxPoints && flagsAt <= glyph.Length
        work.Budget <- work.Budget - int64 (match fits with | true -> numPoints | false -> 0)
        match fits && work.Budget >= 0L with
        | false -> false
        | true -> loadSimpleFrame work gid memory numContours numPoints flagsAt depth

    /// <summary>Loads an empty glyph: its four phantoms and their deltas.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="xMin">The header's x minimum.</param>
    /// <param name="yMax">The header's y maximum.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    let private loadEmpty (work: GlyfWork) (gid: uint32) (xMin: int) (yMax: int) =
        let struct (fb, bb) = takeFrame work PhantomCount 2
        let struct (lsb, tsb, hAdv, vAdv) = work.Source.Metrics gid
        setPhantoms work.F fb PhantomCount 0 (xMin - lsb) hAdv (yMax + tsb) vAdv
        let loaded = applyGvar work (work.Source.VarData gid) fb bb PhantomCount
        match loaded with
        | true -> appendFrame work fb PhantomCount
        | false -> ()
        releaseFrame work fb bb
        loaded

    /// <summary>The byte size of one composite record, <c>CompositeGlyphRecord::get_size</c>.</summary>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="flags">The record flags.</param>
    /// <returns>The record size.</returns>
    let private componentSize (extended: bool) (flags: uint16) =
        let glyphIdExtra = match extended && flags &&& Gid24 <> 0us with | true -> 1 | false -> 0
        let argBytes = match flags &&& ArgWords <> 0us with | true -> 4 | false -> 2
        let matrixBytes =
            match flags &&& HaveScale <> 0us, flags &&& HaveXYScale <> 0us, flags &&& Have2x2 <> 0us with
            | true, _, _ -> 2
            | false, true, _ -> 4
            | false, false, true -> 8
            | false, false, false -> 0
        4 + glyphIdExtra + argBytes + matrixBytes

    /// <summary>The number of composite records a composite glyph holds, stopping at the first record out of range, <c>CompositeGlyph::iter</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <returns>The number of components.</returns>
    let private countComponents (glyph: ReadOnlySpan<byte>) (extended: bool) =
        let mutable m = 0
        let mutable at = HeaderSize
        let mutable counting = true
        while counting do
            let flags = u16At glyph at
            let size = componentSize extended flags
            counting <- at + 4 <= glyph.Length && at + size <= glyph.Length
            m <- m + (match counting with | true -> 1 | false -> 0)
            at <- at + size
            counting <- counting && flags &&& MoreComponents <> 0us
        m

    /// <summary>Writes the offsets of a composite's components and its phantoms into its frame.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="fb">The frame's float base.</param>
    /// <param name="bb">The frame's byte base.</param>
    /// <param name="m">The number of components.</param>
    let private fillCompositeFrame (work: GlyfWork) (gid: uint32) (glyph: ReadOnlySpan<byte>) (fb: int) (bb: int) (m: int) =
        let n = m + PhantomCount
        let f = work.F
        let mutable position = HeaderSize
        let mutable index = 0
        while index < m do
            let flags = u16At glyph position
            let argAt = position + 2 + (match work.Extended && flags &&& Gid24 <> 0us with | true -> 3 | false -> 2)
            let words = flags &&& ArgWords <> 0us
            let withXy = flags &&& ArgsXy <> 0us
            f[fb + index] <- float32 (match withXy, words with | true, true -> int (i16At glyph argAt) | true, false -> int (sbyte (u8At glyph argAt)) | false, _ -> 0)
            f[fb + n + index] <- float32 (match withXy, words with | true, true -> int (i16At glyph (argAt + 2)) | true, false -> int (sbyte (u8At glyph (argAt + 1))) | false, _ -> 0)
            work.B[bb + n + index] <- 1uy
            position <- position + componentSize work.Extended flags
            index <- index + 1
        let struct (lsb, tsb, hAdv, vAdv) = work.Source.Metrics gid
        setPhantoms f fb n m (int (i16At glyph 2) - lsb) hAdv (int (i16At glyph 8) + tsb) vAdv

    /// <summary>Reads one entry of a component's matrix, a 2.14 fixed value, <c>HBFixed&lt;HBINT16,14&gt;::to_float</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="at">The offset of the entry.</param>
    /// <returns>The entry.</returns>
    let inline private matrixEntry (glyph: ReadOnlySpan<byte>) (at: int) =
        float32 (i16At glyph at) * (1.0f / float32 (1 <<< 14))

    /// <summary>Reads a component's two-by-two matrix, <c>CompositeGlyphRecord::get_transformation</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="matrixAt">The offset of the matrix.</param>
    /// <param name="flags">The record flags.</param>
    /// <returns>The four entries.</returns>
    let private readMatrix (glyph: ReadOnlySpan<byte>) (matrixAt: int) (flags: uint16) =
        match flags &&& HaveScale <> 0us, flags &&& HaveXYScale <> 0us, flags &&& Have2x2 <> 0us with
        | true, _, _ -> struct (matrixEntry glyph matrixAt, 0.0f, 0.0f, matrixEntry glyph matrixAt)
        | false, true, _ -> struct (matrixEntry glyph matrixAt, 0.0f, 0.0f, matrixEntry glyph (matrixAt + 2))
        | false, false, true -> struct (matrixEntry glyph matrixAt, matrixEntry glyph (matrixAt + 2), matrixEntry glyph (matrixAt + 4), matrixEntry glyph (matrixAt + 6))
        | false, false, false -> struct (1.0f, 0.0f, 0.0f, 1.0f)

    /// <summary>Adds an offset to a run of output points, skipping a zero offset on each axis, <c>CompositeGlyphRecord::translate</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="start">The first point.</param>
    /// <param name="count">The number of points.</param>
    /// <param name="tx">The x offset.</param>
    /// <param name="ty">The y offset.</param>
    let private translateRun (work: GlyfWork) (start: int) (count: int) (tx: float32) (ty: float32) =
        let outX = work.OutX
        let outY = work.OutY
        let mutable k = start
        while tx <> 0.0f && k < start + count do
            outX[k] <- outX[k] + tx
            k <- k + 1
        k <- start
        while ty <> 0.0f && k < start + count do
            outY[k] <- outY[k] + ty
            k <- k + 1

    /// <summary>Transforms a run of output points by a component matrix, <c>CompositeGlyphRecord::transform</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="start">The first point.</param>
    /// <param name="count">The number of points.</param>
    /// <param name="m0">The matrix entry.</param>
    /// <param name="m1">The matrix entry.</param>
    /// <param name="m2">The matrix entry.</param>
    /// <param name="m3">The matrix entry.</param>
    let private matrixRun (work: GlyfWork) (start: int) (count: int) (m0: float32) (m1: float32) (m2: float32) (m3: float32) =
        let outX = work.OutX
        let outY = work.OutY
        let mutable k = start
        while not (m0 = 1.0f && m1 = 0.0f && m2 = 0.0f && m3 = 1.0f) && k < start + count do
            let oldX = outX[k]
            outX[k] <- oldX * m0 + outY[k] * m2
            outY[k] <- oldX * m1 + outY[k] * m3
            k <- k + 1

    /// <summary>Places a loaded child among the points of its composite: copies its phantoms when the component uses its metrics,
    /// transforms and offsets it, anchors it, and drops its phantoms, <c>finishComponent</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="fb">The composite frame's float base.</param>
    /// <param name="n">The composite frame's point count.</param>
    /// <param name="m">The number of components.</param>
    /// <param name="index">The component's index.</param>
    /// <param name="flags">The component's flags.</param>
    /// <param name="matrix">The component's matrix.</param>
    /// <param name="anchors">The component's anchor points.</param>
    /// <param name="outStart">Where the points of the composite begin in the output.</param>
    /// <param name="childStart">Where the points of the child begin in the output.</param>
    /// <returns>False when the composite has more points than a glyph may.</returns>
    let private placeChild (work: GlyfWork) (fb: int) (n: int) (m: int) (index: int) (flags: uint16) (matrix: struct (float32 * float32 * float32 * float32)) (anchors: struct (uint32 * uint32)) (outStart: int) (childStart: int) =
        let struct (m0, m1, m2, m3) = matrix
        let struct (anchor1, anchor2) = anchors
        let childLen = work.OutLen - childStart
        match flags &&& UseMyMetrics <> 0us with
        | false -> ()
        | true ->
            let mutable k = 0
            while k < PhantomCount do
                work.F[fb + m + k] <- work.OutX[childStart + childLen - PhantomCount + k]
                work.F[fb + n + m + k] <- work.OutY[childStart + childLen - PhantomCount + k]
                k <- k + 1
        let transX = work.F[fb + index]
        let transY = work.F[fb + n + index]
        match (flags &&& (ScaledComponentOffset ||| UnscaledComponentOffset)) = ScaledComponentOffset with
        | true ->
            translateRun work childStart childLen transX transY
            matrixRun work childStart childLen m0 m1 m2 m3
        | false ->
            matrixRun work childStart childLen m0 m1 m2 m3
            translateRun work childStart childLen transX transY
        let anchored = flags &&& ArgsXy = 0us && int anchor1 < childStart - outStart && int anchor2 < childLen
        let dx = work.OutX[outStart + (match anchored with | true -> int anchor1 | false -> 0)] - work.OutX[childStart + (match anchored with | true -> int anchor2 | false -> 0)]
        let dy = work.OutY[outStart + (match anchored with | true -> int anchor1 | false -> 0)] - work.OutY[childStart + (match anchored with | true -> int anchor2 | false -> 0)]
        match anchored with
        | true -> translateRun work childStart childLen dx dy
        | false -> ()
        work.OutLen <- childStart + childLen - PhantomCount
        work.OutLen - outStart <= MaxPoints

    /// <summary>The anchor points of a component, the raw unsigned arguments, <c>get_anchor_points</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="argAt">The offset of the arguments.</param>
    /// <param name="words">Whether the arguments are words.</param>
    /// <returns>The two anchor point numbers.</returns>
    let private readAnchors (glyph: ReadOnlySpan<byte>) (argAt: int) (words: bool) =
        match words with
        | true -> struct (uint32 (u16At glyph argAt), uint32 (u16At glyph (argAt + 2)))
        | false -> struct (uint32 (u8At glyph argAt), uint32 (u8At glyph (argAt + 1)))

    /// <summary>Loads one glyph of any kind at a nesting depth and appends its points and phantoms, <c>Glyph::get_points</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    let rec private loadGlyph (work: GlyfWork) (gid: uint32) (depth: int) =
        match depth > MaxDepth || work.Edge > MaxEdges with
        | true -> false
        | false ->
            work.Edge <- work.Edge + 1
            work.Budget <- work.Budget - Budget16
            match work.Budget >= 0L with
            | false -> false
            | true -> loadFound work gid (work.Source.Glyph gid) depth

    /// <summary>Loads a glyph whose bytes have been looked up.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="found">The glyph bytes, and whether the glyph is in range.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    and private loadFound (work: GlyfWork) (gid: uint32) (found: struct (ReadOnlyMemory<byte> * bool)) (depth: int) =
        let struct (memory, valid) = found
        match valid with
        | false -> false
        | true -> loadByKind work gid memory depth

    /// <summary>Loads a glyph by what its header says it is.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    and private loadByKind (work: GlyfWork) (gid: uint32) (memory: ReadOnlyMemory<byte>) (depth: int) =
        let contours = match memory.Length < HeaderSize with | true -> 0 | false -> int (i16At memory.Span 0)
        let xMin = match memory.Length < HeaderSize with | true -> 0 | false -> int (i16At memory.Span 2)
        let yMax = match memory.Length < HeaderSize with | true -> 0 | false -> int (i16At memory.Span 8)
        match contours with
        | 0 -> loadEmpty work gid xMin yMax
        | simple when simple > 0 -> loadSimple work gid memory depth
        | _ -> loadComposite work gid memory depth

    /// <summary>Assembles a composite glyph, <c>assembleComposite</c> and <c>descendComposite</c>.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    and private loadComposite (work: GlyfWork) (gid: uint32) (memory: ReadOnlyMemory<byte>) (depth: int) =
        let m = countComponents memory.Span work.Extended
        work.Budget <- work.Budget - Budget16 * int64 m
        match work.Budget >= 0L && m + PhantomCount + work.OutLen <= MaxPoints with
        | false -> false
        | true -> loadCompositeFrame work gid memory depth m

    /// <summary>Loads a composite glyph whose records are paid for: its frame and deltas, then its components, then its phantoms.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <param name="m">The number of components.</param>
    /// <returns>False when the glyph is not the ordinary kind.</returns>
    and private loadCompositeFrame (work: GlyfWork) (gid: uint32) (memory: ReadOnlyMemory<byte>) (depth: int) (m: int) =
        let n = m + PhantomCount
        let struct (fb, bb) = takeFrame work n 2
        fillCompositeFrame work gid memory.Span fb bb m
        let outStart = work.OutLen
        let loaded = applyGvar work (work.Source.VarData gid) fb bb n && loadComponents work memory depth fb n m outStart
        match loaded with
        | true -> finishComposite work fb n m outStart depth
        | false -> ()
        releaseFrame work fb bb
        loaded

    /// <summary>Loads and places every component of a composite.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <param name="fb">The composite frame's float base.</param>
    /// <param name="n">The composite frame's point count.</param>
    /// <param name="m">The number of components.</param>
    /// <param name="outStart">Where the points of the composite begin in the output.</param>
    /// <returns>False when a component is not the ordinary kind.</returns>
    and private loadComponents (work: GlyfWork) (memory: ReadOnlyMemory<byte>) (depth: int) (fb: int) (n: int) (m: int) (outStart: int) =
        let mutable position = HeaderSize
        let mutable index = 0
        let mutable ok = true
        while ok && index < m do
            let flags = u16At memory.Span position
            ok <- loadComponent work memory position flags index depth fb n m outStart
            position <- position + componentSize work.Extended flags
            index <- index + 1
        ok

    /// <summary>Loads and places one component of a composite.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="memory">The glyph bytes.</param>
    /// <param name="position">The offset of the component's record.</param>
    /// <param name="flags">The component's flags.</param>
    /// <param name="index">The component's index.</param>
    /// <param name="depth">The nesting depth.</param>
    /// <param name="fb">The composite frame's float base.</param>
    /// <param name="n">The composite frame's point count.</param>
    /// <param name="m">The number of components.</param>
    /// <param name="outStart">Where the points of the composite begin in the output.</param>
    /// <returns>False when the component is not the ordinary kind.</returns>
    and private loadComponent (work: GlyfWork) (memory: ReadOnlyMemory<byte>) (position: int) (flags: uint16) (index: int) (depth: int) (fb: int) (n: int) (m: int) (outStart: int) =
        let glyph = memory.Span
        let wide = work.Extended && flags &&& Gid24 <> 0us
        let childGid = match wide with | true -> u24At glyph (position + 2) | false -> uint32 (u16At glyph (position + 2))
        let argAt = position + 2 + (match wide with | true -> 3 | false -> 2)
        let words = flags &&& ArgWords <> 0us
        let anchors = readAnchors glyph argAt words
        let matrix = readMatrix glyph (argAt + (match words with | true -> 4 | false -> 2)) flags
        let childStart = work.OutLen
        let loaded = loadGlyph work childGid (depth + 1)
        work.Budget <- work.Budget - int64 (work.OutLen - childStart)
        loaded && work.Budget >= 0L && placeChild work fb n m index flags matrix anchors outStart childStart

    /// <summary>Appends a composite's own phantoms to its points, and keeps the bounds when the composite is the glyph asked for.</summary>
    /// <param name="work">The working memory.</param>
    /// <param name="fb">The composite frame's float base.</param>
    /// <param name="n">The composite frame's point count.</param>
    /// <param name="m">The number of components.</param>
    /// <param name="outStart">Where the points of the composite begin in the output.</param>
    /// <param name="depth">The nesting depth.</param>
    and private finishComposite (work: GlyfWork) (fb: int) (n: int) (m: int) (outStart: int) (depth: int) =
        reserveOut work PhantomCount
        let mutable k = 0
        while k < PhantomCount do
            work.OutX[work.OutLen + k] <- work.F[fb + m + k]
            work.OutY[work.OutLen + k] <- work.F[fb + n + m + k]
            k <- k + 1
        work.OutLen <- work.OutLen + PhantomCount
        let shift = work.F[fb + m]
        let mutable i = outStart
        while depth = 0 && i < work.OutLen do
            work.OutX[i] <- work.OutX[i] - shift
            i <- i + 1

    /// <summary>The bounds of a glyph's contour points at the coordinates: the minimum and maximum of x and y over the points of the
    /// outline, the four phantoms left out, <c>contour_bounds_t</c> over <c>Glyph::get_points</c>.</summary>
    /// <param name="source">The tables the glyph is read from.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="axisCount">The axis count of gvar.</param>
    /// <param name="sharedTuples">The shared tuple records.</param>
    /// <param name="sharedScalars">The scalar of every shared tuple at the coordinates.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="glyph">The glyph, below the glyph count.</param>
    /// <returns>Whether the kernel answered, and the minimum x, minimum y, maximum x and maximum y.</returns>
    let bounds (source: IGlyfSource) (extended: bool) (axisCount: int) (sharedTuples: ReadOnlyMemory<byte>) (sharedScalars: float32 array) (coords: ImmutableArray<int>) (glyph: uint32) =
        let work = threadWork.Value
        work.Source <- source
        work.Extended <- extended
        work.AxisCount <- axisCount
        work.SharedTuples <- sharedTuples
        work.SharedScalars <- sharedScalars
        work.Coords <- coords
        work.FTop <- 0
        work.BTop <- 0
        work.ITop <- 0
        work.OutLen <- 0
        work.Edge <- 0
        work.Bounded <- false
        work.Budget <- BudgetGlyph
        let ok = loadGlyph work glyph 0
        let afterBudget = work.Budget - int64 work.OutLen
        let count = work.OutLen - PhantomCount
        let mutable minX = Single.MaxValue
        let mutable minY = Single.MaxValue
        let mutable maxX = -Single.MaxValue
        let mutable maxY = -Single.MaxValue
        let mutable index = 0
        while ok && not work.Bounded && index < count do
            minX <- hbMin minX work.OutX[index]
            minY <- hbMin minY work.OutY[index]
            maxX <- hbMax maxX work.OutX[index]
            maxY <- hbMax maxY work.OutY[index]
            index <- index + 1
        let answered = ok && afterBudget >= 0L && work.OutLen >= PhantomCount
        let result =
            match work.Bounded with
            | true -> struct (answered, work.MinX, work.MinY, work.MaxX, work.MaxY)
            | false -> struct (answered, minX, minY, maxX, maxY)
        work.Source <- Unchecked.defaultof<IGlyfSource>
        work.SharedTuples <- ReadOnlyMemory<byte>.Empty
        work.Coords <- ImmutableArray<int>.Empty
        result

/// <summary>What the joining pass of the Arabic mask setup reads: the joining types of the glyphs and of the context after them, the
/// state the context before them leaves, and the state table, supplied by the shaper that owns the tables.</summary>
type internal IArabicJoining =
    /// <summary>The joining type of a transparent character, <c>JOINING_TYPE_T</c>.</summary>
    abstract Transparent: byte
    /// <summary>The first joining type that joins to the letter before it, <c>JOINING_TYPE_R</c>.</summary>
    abstract Right: byte
    /// <summary>The action that selects no feature, <c>NONE</c>.</summary>
    abstract NoAction: byte
    /// <summary>The first state of the table that has a possible action for the previous letter.</summary>
    abstract FirstPossiblePrevState: int
    /// <summary>The last state of the table that has a possible action for the previous letter.</summary>
    abstract LastPossiblePrevState: int
    /// <summary>The number of characters of the context after the text.</summary>
    abstract ContextCount: int
    /// <summary>The state of the table after the context before the text.</summary>
    abstract PreContextState: int
    /// <summary>The joining type of a glyph of the text.</summary>
    /// <param name="index">The glyph.</param>
    /// <returns>The joining type.</returns>
    abstract OwnType: index: int -> byte
    /// <summary>The joining type of a character of the context after the text.</summary>
    /// <param name="index">The character.</param>
    /// <returns>The joining type.</returns>
    abstract ContextType: index: int -> byte
    /// <summary>The entry of the state table for a joining type that is not transparent.</summary>
    /// <param name="state">The state.</param>
    /// <param name="joiningType">The joining type.</param>
    /// <returns>The action for the previous letter, the action for this one and the next state.</returns>
    abstract Entry: state: int * joiningType: byte -> struct (byte * byte * byte)

/// <summary>The joining pass of <c>setup_masks_arabic_plan</c> and its mask loop, writing into arrays the caller made in this call and
/// shared with no one.</summary>
[<RequireQualifiedAccess>]
module internal ArabicOwned =
    /// <summary>The unsafe-to-break and unsafe-to-concat glyph flags or-ed together.</summary>
    [<Literal>]
    let private UnsafeToBreakMask = 3u

    /// <summary>The unsafe-to-concat glyph flag.</summary>
    [<Literal>]
    let private UnsafeToConcatMask = 2u

    /// <summary>The safe-to-insert-tatweel glyph flag.</summary>
    [<Literal>]
    let private SafeToInsertTatweelMask = 4u

    /// <summary>The longest range a glyph flag request covers.</summary>
    [<Literal>]
    let private FlagRangeLimit = 255

    /// <summary>The first code point of the run of Mongolian free variation selectors that follow a letter.</summary>
    [<Literal>]
    let private FreeVariationSelectorFirst = 0x180Bu

    /// <summary>The last code point of the run of Mongolian free variation selectors.</summary>
    [<Literal>]
    let private FreeVariationSelectorLast = 0x180Du

    /// <summary>The fourth Mongolian free variation selector, which lies apart from the run.</summary>
    [<Literal>]
    let private FreeVariationSelectorFourth = 0x180Fu

    /// <summary>Whether a cluster level is the characters level.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>True only for the characters level.</returns>
    let private exactly (level: ClusterLevel) =
        match level with
        | ClusterLevel.Characters -> true
        | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters | ClusterLevel.Graphemes -> false

    /// <summary>Ors a mask into the glyphs of a range.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    let private orRange (infos: GlyphInfo array) (mask: uint32) (start: int) (finish: int) =
        let mutable index = start
        while index < finish do
            infos[index] <- { infos[index] with Mask = infos[index].Mask ||| mask }
            index <- index + 1

    /// <summary>Ors a mask into the glyphs of a range whose cluster is not the given one.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="cluster">The cluster to leave alone.</param>
    let private orOutside (infos: GlyphInfo array) (mask: uint32) (start: int) (finish: int) (cluster: uint32) =
        let mutable index = start
        while index < finish do
            match cluster = infos[index].Cluster with
            | false -> infos[index] <- { infos[index] with Mask = infos[index].Mask ||| mask }
            | true -> ()
            index <- index + 1

    /// <summary>Ors a mask into the glyphs of a range from its end back to the last glyph of the given cluster.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="cluster">The cluster of the first glyph.</param>
    let private orBackward (infos: GlyphInfo array) (mask: uint32) (start: int) (finish: int) (cluster: uint32) =
        let mutable index = finish
        while start < index && infos[index - 1].Cluster <> cluster do
            index <- index - 1
            infos[index] <- { infos[index] with Mask = infos[index].Mask ||| mask }

    /// <summary>Ors a mask into the glyphs of a range from its start up to the first glyph of the given cluster.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <param name="cluster">The cluster of the last glyph.</param>
    let private orForward (infos: GlyphInfo array) (mask: uint32) (start: int) (finish: int) (cluster: uint32) =
        let mutable index = start
        while index < finish && infos[index].Cluster <> cluster do
            infos[index] <- { infos[index] with Mask = infos[index].Mask ||| mask }
            index <- index + 1

    /// <summary>The smallest cluster a range's flags are measured against: of the first and the last glyph, and of every glyph at the
    /// characters level, <c>_infos_find_min_cluster</c>.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index.</param>
    /// <returns>The cluster.</returns>
    let private minCluster (infos: GlyphInfo array) (level: ClusterLevel) (start: int) (finish: int) =
        let mutable cluster = min infos[start].Cluster infos[finish - 1].Cluster
        match exactly level with
        | true ->
            let mutable index = start
            while index < finish do
                cluster <- min cluster infos[index].Cluster
                index <- index + 1
        | false -> ()
        cluster

    /// <summary>Ors a mask into the glyphs of an interior range, those of the minimum cluster excepted, and charges the range against the
    /// operation budget, <c>_infos_set_glyph_flags</c>.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="finish">The end index, at least two after the start.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the buffer has not failed, cleared when the budget runs out.</param>
    let private orInterior (infos: GlyphInfo array) (level: ClusterLevel) (mask: uint32) (start: int) (finish: int) (maxOps: byref<int>) (successful: byref<bool>) =
        let cluster = minCluster infos level start finish
        maxOps <- maxOps - (finish - start)
        successful <- successful && maxOps >= 0
        let clusterFirst = infos[start].Cluster
        let clusterLast = infos[finish - 1].Cluster
        match exactly level || (cluster <> clusterFirst && cluster <> clusterLast), cluster = clusterFirst with
        | true, _ -> orOutside infos mask start finish cluster
        | false, true -> orBackward infos mask start finish clusterFirst
        | false, false -> orForward infos mask start finish clusterLast

    /// <summary>Applies a glyph-flag request to the input array, <c>_set_glyph_flags</c>.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="mask">The mask to or in.</param>
    /// <param name="start">The first index.</param>
    /// <param name="stop">The end index.</param>
    /// <param name="interior">Whether the minimum cluster is skipped.</param>
    /// <param name="maxOps">The operation budget, charged for an interior range.</param>
    /// <param name="successful">Whether the buffer has not failed, cleared when the budget runs out.</param>
    let private setFlags (infos: GlyphInfo array) (level: ClusterLevel) (mask: uint32) (start: int) (stop: int) (interior: bool) (maxOps: byref<int>) (successful: byref<bool>) =
        let finish = min stop infos.Length
        match uint32 stop - uint32 start > uint32 FlagRangeLimit, interior, finish - start < 2 with
        | true, _, _ -> ()
        | false, true, true -> ()
        | false, false, _ -> orRange infos mask start finish
        | false, true, false -> orInterior infos level mask start finish &maxOps &successful

    /// <summary>Flags a range safe to insert a tatweel, <c>safe_to_insert_tatweel</c>: unsafe to break when the buffer does not ask for the
    /// tatweel flag.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="start">The first index.</param>
    /// <param name="stop">The end index.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the buffer has not failed.</param>
    let private safeToInsertTatweel (infos: GlyphInfo array) (level: ClusterLevel) (flags: BufferFlags) (start: int) (stop: int) (maxOps: byref<int>) (successful: byref<bool>) =
        match (flags &&& BufferFlags.ProduceSafeToInsertTatweel) = BufferFlags.Default with
        | true -> setFlags infos level UnsafeToBreakMask start stop true &maxOps &successful
        | false -> setFlags infos level SafeToInsertTatweelMask start stop true &maxOps &successful

    /// <summary>Flags a range unsafe to concat, <c>unsafe_to_concat</c>, when the buffer asks for the flag.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="start">The first index.</param>
    /// <param name="stop">The end index.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the buffer has not failed.</param>
    let private unsafeToConcat (infos: GlyphInfo array) (level: ClusterLevel) (flags: BufferFlags) (start: int) (stop: int) (maxOps: byref<int>) (successful: byref<bool>) =
        match (flags &&& BufferFlags.ProduceUnsafeToConcat) = BufferFlags.Default with
        | true -> ()
        | false -> setFlags infos level UnsafeToConcatMask start stop false &maxOps &successful

    /// <summary>Settles the last letter of the text against the first character of the context after it that is not transparent, the last
    /// loop of <c>arabic_joining</c>: the letter takes the action the character gives it and is flagged safe to insert a tatweel, or the
    /// text is flagged unsafe to concat from the letter on.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="props">The properties.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="count">The number of glyphs.</param>
    /// <param name="prev">The last letter that took part, or -1.</param>
    /// <param name="overrides">Whether the character gives the letter an action.</param>
    /// <param name="prevAction">The action it gives.</param>
    /// <param name="possiblePrev">Whether the state is one that has a possible action for the previous letter.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the buffer has not failed.</param>
    let private closeContext (infos: GlyphInfo array) (props: GlyphProps array) (level: ClusterLevel) (flags: BufferFlags) (count: int) (prev: int) (overrides: bool) (prevAction: byte) (possiblePrev: bool) (maxOps: byref<int>) (successful: byref<bool>) =
        match overrides, possiblePrev && prev >= 0 with
        | true, _ ->
            props[prev] <- { props[prev] with ShaperAuxiliary = prevAction }
            safeToInsertTatweel infos level flags prev count &maxOps &successful
        | false, true -> unsafeToConcat infos level flags prev count &maxOps &successful
        | false, false -> ()

    /// <summary>Runs the joining machine over the glyphs and the context after them in one pass, <c>arabic_joining</c>: the state is in locals,
    /// the action of each glyph is written into the auxiliary byte of its properties, and the glyph flags are set as the machine decides
    /// them.</summary>
    /// <typeparam name="'J">The joining source type.</typeparam>
    /// <param name="joining">The joining types, the state before the text and the state table.</param>
    /// <param name="infosOwned">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="propsOwned">The properties, made by the caller in this call and shared with no one.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="maxOps">The operation budget.</param>
    /// <param name="successful">Whether the buffer has not failed.</param>
    /// <returns>The operation budget and whether the buffer is still successful.</returns>
    let join<'J when 'J : struct and 'J :> IArabicJoining> (joining: 'J) (infosOwned: ImmutableArray<GlyphInfo>) (propsOwned: ImmutableArray<GlyphProps>) (level: ClusterLevel) (flags: BufferFlags) (maxOps: int) (successful: bool) =
        let infos = nonNull (ImmutableCollectionsMarshal.AsArray infosOwned)
        let props = nonNull (ImmutableCollectionsMarshal.AsArray propsOwned)
        let count = infos.Length
        let none = joining.NoAction
        let transparent = joining.Transparent
        let right = joining.Right
        let firstPrev = joining.FirstPossiblePrevState
        let lastPrev = joining.LastPossiblePrevState
        let mutable budget = maxOps
        let mutable alive = successful
        let mutable state = joining.PreContextState
        let mutable prev = -1
        let mutable index = 0
        while index < count do
            let thisType = joining.OwnType index
            match thisType = transparent with
            | true -> props[index] <- { props[index] with ShaperAuxiliary = none }
            | false ->
                let struct (prevAction, currAction, next) = joining.Entry (state, thisType)
                let overrides = prevAction <> none && prev >= 0
                match overrides with
                | true -> props[prev] <- { props[prev] with ShaperAuxiliary = prevAction }
                | false -> ()
                match overrides, prev < 0, thisType >= right, firstPrev <= state && state <= lastPrev with
                | true, _, _, _ -> safeToInsertTatweel infos level flags prev (index + 1) &budget &alive
                | false, true, true, _ -> unsafeToConcat infos level flags 0 (index + 1) &budget &alive
                | false, false, true, _ | false, false, false, true -> unsafeToConcat infos level flags prev (index + 1) &budget &alive
                | false, true, false, _ | false, false, false, false -> ()
                props[index] <- { props[index] with ShaperAuxiliary = currAction }
                prev <- index
                state <- int next
            index <- index + 1
        let mutable position = 0
        let mutable looking = true
        while looking && position < joining.ContextCount do
            let thisType = joining.ContextType position
            position <- position + 1
            match thisType = transparent with
            | true -> ()
            | false ->
                let struct (prevAction, _, _) = joining.Entry (state, thisType)
                closeContext infos props level flags count prev (prevAction <> none && prev >= 0) prevAction (firstPrev <= state && state <= lastPrev) &budget &alive
                looking <- false
        struct (budget, alive)

    /// <summary>Gives every glyph the mask of the feature its action selects and, for Mongolian, gives a free variation selector the action
    /// of the glyph before it first, <c>mongolian_variation_selectors</c> and the mask loop of <c>setup_masks_arabic_plan</c>.</summary>
    /// <param name="infosOwned">The glyphs, made by the caller in this call and shared with no one.</param>
    /// <param name="propsOwned">The properties, with the joining actions in their auxiliary byte, made by the caller in this call.</param>
    /// <param name="masks">The mask of each action.</param>
    /// <param name="mongolian">Whether the variation selectors take the action of the letter before them.</param>
    let maskActions (infosOwned: ImmutableArray<GlyphInfo>) (propsOwned: ImmutableArray<GlyphProps>) (masks: ImmutableArray<uint32>) (mongolian: bool) =
        let infos = nonNull (ImmutableCollectionsMarshal.AsArray infosOwned)
        let props = nonNull (ImmutableCollectionsMarshal.AsArray propsOwned)
        let mutable index = 0
        while index < infos.Length do
            let u = infos[index].Codepoint
            let selector = mongolian && index > 0 && ((u >= FreeVariationSelectorFirst && u <= FreeVariationSelectorLast) || u = FreeVariationSelectorFourth)
            let action = (match selector with | true -> props[index - 1].ShaperAuxiliary | false -> props[index].ShaperAuxiliary)
            props[index] <- { props[index] with ShaperAuxiliary = action }
            infos[index] <- { infos[index] with Mask = infos[index].Mask ||| masks[int action] }
            index <- index + 1

/// <summary>Reads the advance of one glyph, for the batch entries that write advances into an array the caller owns: implemented by a
/// struct, so that the loop that calls it is specialised for each reader and the read inlined.</summary>
type internal IAdvanceReader =
    /// <summary>The advance of a glyph.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    abstract Read: glyph: uint32 -> int

/// <summary>Advances written into an array the caller owns, so that a batch of advances allocates nothing: the caller sizes the array.</summary>
[<RequireQualifiedAccess>]
module internal AdvanceInto =
    /// <summary>Writes the advance of every glyph into the first elements of an array, one glyph at a time through a reader.</summary>
    /// <param name="reader">The reader.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <param name="target">The array, at least as long as the glyphs.</param>
    let readInto<'R when 'R : struct and 'R :> IAdvanceReader> (reader: 'R) (glyphs: ReadOnlySpan<uint32>) (target: Span<int>) =
        let mutable index = 0
        while index < glyphs.Length do
            target[index] <- reader.Read glyphs[index]
            index <- index + 1

    /// <summary>Writes one value into the first elements of an array.</summary>
    /// <param name="value">The value.</param>
    /// <param name="count">The elements.</param>
    /// <param name="target">The array, at least as long as the count.</param>
    let fillInto (value: int) (count: int) (target: Span<int>) =
        target.Slice(0, count).Fill value

    /// <summary>Writes the scaled advances of glyphs read straight from an hmtx or vmtx table into the first elements of an array, the
    /// advances <see cref="AdvanceTable.scaled"/> returns: when the table holds every long metric the loop reads them without a bounds
    /// test, and otherwise a metric the table lacks reads as 0.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="bearings">The number of glyphs the table gives bearings.</param>
    /// <param name="longMetrics">The number of long metrics.</param>
    /// <param name="advances">The number of advances the table holds, 0 when it holds none.</param>
    /// <param name="defaultAdvance">The advance of a glyph beyond a table without advances.</param>
    /// <param name="multiplier">The font's multiplier, <c>em_scale</c>'s.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <param name="target">The array, at least as long as the glyphs.</param>
    let scaledInto (table: ReadOnlySpan<byte>) (bearings: int) (longMetrics: int) (advances: int) (defaultAdvance: int) (multiplier: int64) (glyphs: ReadOnlySpan<uint32>) (target: Span<int>) =
        let last = uint64 longMetrics - 1UL
        let beyond = int ((int64 (match advances with | 0 -> defaultAdvance | _ -> 0) * multiplier + 32768L) >>> 16)
        let whole = longMetrics > 0 && int64 longMetrics * 4L <= int64 table.Length
        let first = &MemoryMarshal.GetReference table
        let mutable index = 0
        while index < glyphs.Length do
            let glyph = uint64 glyphs[index]
            let scaled =
                match glyph < uint64 bearings with
                | false -> beyond
                | true ->
                    let at = int (min glyph last) * 4
                    let unscaled =
                        match whole || uint64 (uint32 at) + 2UL <= uint64 table.Length with
                        | true ->
                            let stored = Unsafe.ReadUnaligned<uint16>(&Unsafe.Add(&first, at))
                            int (match BitConverter.IsLittleEndian with | true -> System.Buffers.Binary.BinaryPrimitives.ReverseEndianness stored | false -> stored)
                        | false -> 0
                    int ((int64 unscaled * multiplier + 32768L) >>> 16)
            target[index] <- scaled
            index <- index + 1

/// <summary>Copies into arrays the caller owns, the one place where a shaped line is written into them.</summary>
[<RequireQualifiedAccess>]
module internal CallerArrays =
    /// <summary>Copies a source into the first elements of a target that is at least as long.</summary>
    /// <param name="source">The elements.</param>
    /// <param name="target">The target.</param>
    let copyInto (source: ReadOnlySpan<'T>) (target: Span<'T>) =
        source.CopyTo target

/// <summary>One glyph of what a unit becomes: its info with its cluster as an offset from the unit's lowest cluster, its properties with the
/// syllable's place in the unit instead of its serial and with the ligature id a label replaces cleared, and the label: 0 keeps the ligature
/// id the glyph holds, n gives it the id of the unit's n-th allocation.</summary>
[<Struct; NoEquality; NoComparison>]
type internal LearnedGlyph =
    { LearnedInfo: GlyphInfo
      LearnedProps: GlyphProps
      LearnedLabel: byte }

/// <summary>What one unit of an Indic line becomes over the lookups and pauses its plan reuses: a unit is a syllable, or the syllables that
/// share clusters with one another. Learned from a line's own run of those lookups and pauses when no lookup could reach the unit from the rest
/// of the line, it is a function of the unit's key, so any line that holds the same unit in the same surroundings gets the same glyphs from it.</summary>
[<NoEquality; NoComparison>]
type internal SyllableOutcome =
    { OutcomeHash: uint64  // the hash of the key
      OutcomeKey: uint32 array  // the key the unit was learned for
      OutcomeGlyphs: LearnedGlyph array
      OutcomeAllocations: int array  // the map position of the lookup that made each ligature id, in the order they were made
      OutcomeTwins: byte array  // for each allocation, the first one it was learned with the same id as, empty when all differed: a line rebuilt from the unit gives twins the same id
      OutcomeInputIds: byte  // a bit for each ligature id the unit's own glyphs held before the lookups
      OutcomeTailInRange: bool  // whether the category of the last glyph after the final reordering is between Cf and Mn
      OutcomeSensitive: bool  // the unit starts with a pre-base matra after the final reordering, so the glyph before it matters
      OutcomeComponents: bool  // a glyph of the unit was a ligature component, the only glyphs whose ligature ids are compared
      OutcomeFirstHeld: byte  // the ids the unit held that its first glyph had while a ligature component, the only ids it compares with the unit before
      OutcomeFirstAllocations: byte  // a bit for each allocation of the unit whose id its first glyph had while a ligature component
      OutcomeSyllables: int  // the syllables of the unit
      OutcomeLoose: uint16  // a bit for each syllable of the unit that lacked, at the start or after some lookup, a glyph no lookup skips
      OutcomeOps: int  // at least the operations the unit charged: those of the whole line it was learned from
      OutcomeScratch: ScratchFlags  // the scratch flags after the unit
      OutcomeLongest: int }  // the most glyphs the unit held between two lookups

/// <summary>One unit of a line: the position after its last glyph and its lowest cluster.</summary>
[<Struct; NoEquality; NoComparison>]
type internal LineUnit =
    { UnitEnd: int
      UnitBase: uint32 }

/// <summary>The size of a plan's table of learned units.</summary>
[<RequireQualifiedAccess>]
module internal SyllableTableSize =
    /// <summary>The bits of a hash that name an entry's first slot in the largest table: it grows to 2^15 slots.</summary>
    [<Literal>]
    let Bits = 15

    /// <summary>The bits of a hash that name an entry's first slot in a new table: it starts with 2^10 slots and doubles once half of them
    /// are taken.</summary>
    [<Literal>]
    let InitialBits = 10

    /// <summary>The most slots a search or an insertion visits from the one a hash names.</summary>
    [<Literal>]
    let Probes = 32

    /// <summary>The entries after which the table learns no more, half of its slots, so that an insertion rarely finds its probes taken.</summary>
    [<Literal>]
    let Fill = 16384

    /// <summary>The bits of a hash that name its first slot among the hashes of the units met, in the largest table: 2^15 slots.</summary>
    [<Literal>]
    let SeenBits = 15

    /// <summary>The bits of a hash that name its first slot among the hashes of the units met, in a new table: 2^11 slots, doubling once
    /// half of them are taken.</summary>
    [<Literal>]
    let InitialSeenBits = 11

    /// <summary>The hashes noted after which every unit counts as met often enough, half of the largest table's slots.</summary>
    [<Literal>]
    let SeenFill = 16384

    /// <summary>The low bits of a noted hash that count its sightings.</summary>
    [<Literal>]
    let SightingBits = 3UL

    /// <summary>The sightings of a unit before the one at which it is learned.</summary>
    [<Literal>]
    let Sightings = 1UL

    /// <summary>The units looked up before the share found decides when a unit is learned.</summary>
    [<Literal>]
    let HitSample = 256

    /// <summary>The units looked up after which the counts of those looked up and found are halved, so that the share follows the text.</summary>
    [<Literal>]
    let HitWindow = 65536

    /// <summary>The first slot a hash names in a table of a length.</summary>
    /// <param name="hash">The hash.</param>
    /// <param name="length">The slots, a power of two.</param>
    /// <returns>The slot.</returns>
    let firstSlot (hash: uint64) (length: int) =
        int (hash >>> (64 - BitOperations.Log2(uint32 length)))

/// <summary>The units a plan has learned, in a table made the first time one is kept, doubled once half of its slots are taken up to a
/// largest size, and never cleared: an entry is a pure function of its key and of the plan and face the table belongs to, so of two threads
/// that keep one key both entries are equal, a reader sees an entry whole or not at all, and an entry that finds no free slot, or that is kept
/// in a table another thread has just replaced with a larger copy, is not kept, which changes no result. Once half of the largest table is
/// taken the table learns no more. The hashes of the units met are counted beside it, so that a unit can wait to be learned until it has been
/// met before, and so are the units looked up and those found, which say whether the units of the text recur. The counts choose only when a
/// unit is learned, never what a line becomes. Equality ignores the entries and the counts, so the plans that hold one compare by their other
/// fields.</summary>
[<Sealed; AllowNullLiteral>]
type internal SyllableTable() =
    let mutable slots: (SyllableOutcome | null) array | null = null
    let mutable count = 0
    let mutable seen: uint64 array | null = null
    let mutable seenCount = 0
    let mutable looked = 0
    let mutable found = 0

    /// <summary>Puts an entry in the first free slot from the one its hash names.</summary>
    /// <param name="table">The slots.</param>
    /// <param name="entry">The entry.</param>
    /// <returns>True when a free slot was found among the probed ones.</returns>
    let place (table: (SyllableOutcome | null) array) (entry: SyllableOutcome) =
        let first = SyllableTableSize.firstSlot entry.OutcomeHash table.Length
        let mask = table.Length - 1
        let held: SyllableOutcome | null = entry
        let mutable probe = 0
        let mutable placed = false
        while not placed && probe < SyllableTableSize.Probes do
            match System.Threading.Interlocked.CompareExchange(&table[(first + probe) &&& mask], held, null) with
            | null -> placed <- true
            | _ -> probe <- probe + 1
        placed

    /// <summary>Notes a hash with its count in the first free slot from the one it names, or adds one sighting where it is already noted.</summary>
    /// <param name="marks">The slots of the hashes.</param>
    /// <param name="value">The hash with its sightings in the low bits.</param>
    /// <param name="adding">Whether a sighting is added to a hash already noted.</param>
    /// <returns>True when the hash took a free slot.</returns>
    let mark (marks: uint64 array) (value: uint64) (adding: bool) =
        let key = value &&& ~~~SyllableTableSize.SightingBits
        let first = SyllableTableSize.firstSlot key marks.Length
        let mask = marks.Length - 1
        let mutable probe = 0
        let mutable placed = false
        let mutable taken = false
        while not placed && probe < SyllableTableSize.Probes do
            let slot = (first + probe) &&& mask
            let held = System.Threading.Volatile.Read(&marks[slot])
            match held with
            | 0UL ->
                taken <- System.Threading.Interlocked.CompareExchange(&marks[slot], value, 0UL) = 0UL
                placed <- taken
            | _ when held &&& ~~~SyllableTableSize.SightingBits = key ->
                match adding && held &&& SyllableTableSize.SightingBits < SyllableTableSize.Sightings with
                | true -> System.Threading.Interlocked.CompareExchange(&marks[slot], held + 1UL, held) |> ignore
                | false -> ()
                placed <- true
            | _ -> probe <- probe + 1
        taken

    /// <summary>The slots, null until the first entry is kept.</summary>
    /// <returns>The slots, or null.</returns>
    member _.Slots = System.Threading.Volatile.Read(&slots)

    /// <summary>Whether the table holds as many entries as it learns.</summary>
    /// <returns>True once it is full.</returns>
    member _.Saturated = System.Threading.Volatile.Read(&count) >= SyllableTableSize.Fill

    /// <summary>Keeps an entry in the first free slot from the one its hash names, making the slots when there are none and doubling them once
    /// half are taken; when the probed slots are all taken the entry is not kept.</summary>
    /// <param name="entry">The entry.</param>
    member _.Keep (entry: SyllableOutcome) =
        let table =
            match System.Threading.Volatile.Read(&slots) with
            | null ->
                let created: (SyllableOutcome | null) array = Array.zeroCreate (1 <<< SyllableTableSize.InitialBits)
                match System.Threading.Interlocked.CompareExchange(&slots, created, null) with
                | null -> created
                | existing -> existing
            | existing -> existing
        match place table entry with
        | false -> ()
        | true ->
            let kept = System.Threading.Interlocked.Increment(&count)
            match kept * 2 > table.Length && table.Length < (1 <<< SyllableTableSize.Bits) with
            | false -> ()
            | true ->
                let larger: (SyllableOutcome | null) array = Array.zeroCreate (table.Length * 2)
                let mutable index = 0
                while index < table.Length do
                    match System.Threading.Volatile.Read(&table[index]) with
                    | null -> ()
                    | held -> place larger held |> ignore
                    index <- index + 1
                System.Threading.Interlocked.CompareExchange(&slots, larger, table) |> ignore

    /// <summary>Counts the units a line looked up and those it found. Two threads may halve the counts together, which changes no result.</summary>
    /// <param name="lookedUp">The units looked up.</param>
    /// <param name="foundThere">The units found.</param>
    member _.Count (lookedUp: int, foundThere: int) =
        let total = System.Threading.Interlocked.Add(&looked, lookedUp)
        System.Threading.Interlocked.Add(&found, foundThere) |> ignore
        match total > SyllableTableSize.HitWindow with
        | false -> ()
        | true ->
            System.Threading.Volatile.Write(&looked, total / 2)
            System.Threading.Volatile.Write(&found, System.Threading.Volatile.Read(&found) / 2)

    /// <summary>Whether more than half of the units looked up lately were found, once enough were: the units of the text recur, so a unit is
    /// learned the first time it is met. Otherwise a unit is learned only once it has been met before, so that a text whose units seldom recur
    /// spends little learning them.</summary>
    /// <returns>True when units are learned the first time they are met.</returns>
    member _.Recurring =
        let lookedUp = System.Threading.Volatile.Read(&looked)
        lookedUp >= SyllableTableSize.HitSample && System.Threading.Volatile.Read(&found) * 2 > lookedUp

    /// <summary>Whether a unit was met often enough to be learned: a unit is learned once it has been met that often, so that one met only
    /// once costs no learning. Once the hashes fill half of the largest table every unit counts as met often enough. A hash that matches
    /// another unit's only makes that unit learned sooner, which changes no result.</summary>
    /// <param name="hash">The hash of the unit's sightings.</param>
    /// <returns>True when the unit is to be learned.</returns>
    member _.SeenBefore (hash: uint64) =
        match System.Threading.Volatile.Read(&seen) with
        | null -> false
        | marks ->
            let key = (match hash with | 0UL -> 1UL | value -> value) &&& ~~~SyllableTableSize.SightingBits
            let first = SyllableTableSize.firstSlot key marks.Length
            let mask = marks.Length - 1
            let mutable often = System.Threading.Volatile.Read(&seenCount) >= SyllableTableSize.SeenFill
            let mutable ended = false
            let mutable probe = 0
            while not often && not ended && probe < SyllableTableSize.Probes do
                let held = System.Threading.Volatile.Read(&marks[(first + probe) &&& mask])
                often <- held &&& ~~~SyllableTableSize.SightingBits = key && held &&& SyllableTableSize.SightingBits >= SyllableTableSize.Sightings
                ended <- held = 0UL || held &&& ~~~SyllableTableSize.SightingBits = key
                probe <- probe + 1
            often

    /// <summary>Notes one more sighting of a unit, making the slots when there are none and doubling them once half are taken; when the probed
    /// slots are all taken by other hashes it is not noted.</summary>
    /// <param name="hash">The hash of the unit's sightings.</param>
    member _.Note (hash: uint64) =
        let marks =
            match System.Threading.Volatile.Read(&seen) with
            | null ->
                let created = Array.zeroCreate<uint64> (1 <<< SyllableTableSize.InitialSeenBits)
                match System.Threading.Interlocked.CompareExchange(&seen, created, null) with
                | null -> created
                | existing -> existing
            | existing -> existing
        let key = (match hash with | 0UL -> 1UL | value -> value) &&& ~~~SyllableTableSize.SightingBits
        match mark marks (key ||| 1UL) true with
        | false -> ()
        | true ->
            let noted = System.Threading.Interlocked.Increment(&seenCount)
            match noted * 2 > marks.Length && marks.Length < (1 <<< SyllableTableSize.SeenBits) with
            | false -> ()
            | true ->
                let larger = Array.zeroCreate<uint64> (marks.Length * 2)
                let mutable index = 0
                while index < marks.Length do
                    match System.Threading.Volatile.Read(&marks[index]) with
                    | 0UL -> ()
                    | held -> mark larger held false |> ignore
                    index <- index + 1
                System.Threading.Interlocked.CompareExchange(&seen, larger, marks) |> ignore

    /// <summary>Whether another object is a table; equality ignores the entries.</summary>
    /// <param name="other">The other object.</param>
    /// <returns>True for any table.</returns>
    override _.Equals other =
        match other with
        | :? SyllableTable -> true
        | _ -> false

    /// <summary>The hash of a table, the same for every table.</summary>
    /// <returns>Zero.</returns>
    override _.GetHashCode () = 0

/// <summary>A line rebuilt from the units a plan learned, with the counters the lookups it stands for would have left.</summary>
[<Struct; NoEquality; NoComparison>]
type internal SyllableReplay =
    { ReplayInfos: ImmutableArray<GlyphInfo>
      ReplayProps: ImmutableArray<GlyphProps>
      ReplaySerial: byte
      ReplayMaxOps: int  // the line's operations left, at most what the lookups would have left: the line is shaped the ordinary way if it runs out
      ReplayScratch: ScratchFlags }

/// <summary>What a line whose units the table does not all hold learns from its own run of the reused region: its units, and for each unit to
/// learn the key it is kept under, built before the run, whose first word the run completes with the glyph before the unit.</summary>
[<Struct; NoEquality; NoComparison>]
type internal SyllableHarvest =
    { HarvestUnits: LineUnit array  // the units of the line
      HarvestKeys: uint32 array array  // the key of each unit to learn, empty for the others
      HarvestLevel: ClusterLevel
      HarvestScratch: uint32  // the second header word of the keys
      HarvestFlags: uint32 }  // the third header word

/// <summary>What the syllable reuse does with a line.</summary>
[<Struct; NoEquality; NoComparison>]
type internal SyllableLine =
    | LineRebuilt of replay: SyllableReplay  // the table holds every unit: the line is rebuilt from them
    | LineLearning of harvest: SyllableHarvest  // the line is shaped through the region and learns units from that run
    | LineOrdinary  // the line is shaped the ordinary way

/// <summary>The keys, the table search and the replay of the syllable reuse, written over arrays.</summary>
[<RequireQualifiedAccess>]
module internal SyllableReuseKernel =
    /// <summary>The words of a key before its glyphs: the three header words and the two halves of the hash of the glyphs.</summary>
    [<Literal>]
    let HeaderWords = 5

    /// <summary>The word of a key that holds the low half of the hash of the glyphs; the next word holds the high half.</summary>
    [<Literal>]
    let GlyphHashWord = 3

    /// <summary>The shift of a syllable's serial in its syllable byte.</summary>
    [<Literal>]
    let private SerialShift = 4

    /// <summary>The bits a syllable's serial takes in the serials of a unit, one place after another.</summary>
    [<Literal>]
    let private SerialWidth = 4

    /// <summary>The bits of one serial among the serials of a unit.</summary>
    [<Literal>]
    let private SerialMask = 0xFUL

    /// <summary>The bits a ligature id takes among the ids of a rebuilt line, one allocation after another.</summary>
    [<Literal>]
    let private IdWidth = 3

    /// <summary>The bits of one ligature id among the ids of a rebuilt line.</summary>
    [<Literal>]
    let private IdMask = 7UL

    /// <summary>The most allocations a rebuilt line has for its ids to be packed in one word, three bits each, and found by counting the
    /// allocations before each: a count costs as many steps as the line has allocations and units, so a line that makes more ids, or has
    /// more units than <see cref="PackedUnits"/>, sorts them in arrays of its own.</summary>
    [<Literal>]
    let private PackedIds = 8

    /// <summary>The most units a rebuilt line has for its ids to be packed.</summary>
    [<Literal>]
    let private PackedUnits = 8

    /// <summary>The shift of the unit in the sort key of an allocation of a rebuilt line, above the allocation.</summary>
    [<Literal>]
    let private EventUnitShift = 8

    /// <summary>The bits of the unit in the sort key of an allocation, after the shift.</summary>
    [<Literal>]
    let private EventUnitMask = 0xFFFFFFL

    /// <summary>The bits of the allocation in its sort key.</summary>
    [<Literal>]
    let private EventAllocationMask = 0xFFL

    /// <summary>The most allocations a learned unit may make, so that its allocations fit their sort key.</summary>
    [<Literal>]
    let MaxOwnAllocations = 255

    /// <summary>The words of a key for each glyph.</summary>
    [<Literal>]
    let GlyphWords = 8

    /// <summary>The seed of a key's hash.</summary>
    [<Literal>]
    let private HashSeed = 0xCBF29CE484222325UL

    /// <summary>The multiplier of a key's hash.</summary>
    [<Literal>]
    let private HashPrime = 0x9E3779B97F4A7C15UL

    /// <summary>The bit of the first header word that says the glyph before the unit is a letter or a mark.</summary>
    [<Literal>]
    let private LetterBeforeBit = 0x10000u

    /// <summary>The bit of the first header word that says the unit starts the line.</summary>
    [<Literal>]
    let private FirstBit = 0x20000u

    /// <summary>The shift of the cluster level in the first header word.</summary>
    [<Literal>]
    let private LevelShift = 18

    /// <summary>The low nibble of a syllable byte, its type.</summary>
    [<Literal>]
    let SyllableTypeBits = 0x0Fuy

    /// <summary>The high nibble of a syllable byte, its serial.</summary>
    [<Literal>]
    let private SyllableSerialBits = 0xF0uy

    /// <summary>The most syllables a unit holds: fewer than the 15 serials a line's syllables cycle through, so that no two syllables of a
    /// unit share a syllable byte.</summary>
    [<Literal>]
    let MaxUnitSyllables = 15

    /// <summary>The longest run of loose syllables a line may hold: a lookup confined to one syllable could reach, past fourteen syllables
    /// whose glyphs it all skips, the next syllable with the same serial (hb-ot-layout-gsubgpos.hh, <c>may_match</c> compares the syllable
    /// bytes only).</summary>
    [<Literal>]
    let MaxLooseRun = 13

    /// <summary>The shift of the ligature id in the ligature properties.</summary>
    [<Literal>]
    let LigIdShift = 5

    /// <summary>The bits of the ligature properties below the ligature id.</summary>
    [<Literal>]
    let BelowLigId = 0x1Fuy

    /// <summary>The bit of the ligature properties that marks a ligature's own glyph, <c>IS_LIG_BASE</c>.</summary>
    [<Literal>]
    let private LigBaseBit = 0x10uy

    /// <summary>The bits of the ligature properties that hold a component index.</summary>
    [<Literal>]
    let private ComponentBits = 0x0Fuy

    /// <summary>The ligature ids an allocation can give, 1 to 7.</summary>
    [<Literal>]
    let MaxAllocations = 7

    /// <summary>The values a serial takes, every byte but zero: the allocations of one lookup are counted up to that many.</summary>
    [<Literal>]
    let SerialValues = 255

    /// <summary>The ligature id bits of a value.</summary>
    [<Literal>]
    let private LigIdBits = 0x07uy

    /// <summary>The code of a cluster level.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>The code.</returns>
    let levelCode (level: ClusterLevel) =
        match level with
        | MonotoneGraphemes -> 0u
        | MonotoneCharacters -> 1u
        | Characters -> 2u
        | Graphemes -> 3u

    /// <summary>The first header word of a key: the glyph count, the letter-before and first bits and the cluster level.</summary>
    /// <param name="count">The glyphs of the unit.</param>
    /// <param name="letterBefore">Whether the glyph before is a letter or a mark.</param>
    /// <param name="first">Whether the unit starts the line.</param>
    /// <param name="level">The cluster level.</param>
    /// <returns>The word.</returns>
    let headerWord (count: int) (letterBefore: bool) (first: bool) (level: ClusterLevel) =
        uint32 count
        ||| (match letterBefore with | true -> LetterBeforeBit | false -> 0u)
        ||| (match first with | true -> FirstBit | false -> 0u)
        ||| (levelCode level <<< LevelShift)

    /// <summary>The first header word with the other value of the letter-before bit.</summary>
    /// <param name="header0">The first header word.</param>
    /// <returns>The word.</returns>
    let otherLetterBefore (header0: uint32) =
        header0 ^^^ LetterBeforeBit

    /// <summary>The word of a glyph's part of a key that holds its Unicode properties and glyph class.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The word.</returns>
    let private classWord (props: GlyphProps) =
        uint32 props.UnicodeProps ||| (uint32 props.GlyphClass <<< 16)

    /// <summary>The word of a glyph's part of a key that holds its ligature properties, its syllable type with the syllable's place in the
    /// unit instead of its serial, and its shaper category and position.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <param name="place">The place of the glyph's syllable in the unit.</param>
    /// <returns>The word.</returns>
    let private shaperWord (props: GlyphProps) (place: int) =
        uint32 props.LigProps ||| (uint32 (props.Syllable &&& SyllableTypeBits) <<< 8) ||| (uint32 place <<< 12)
        ||| (uint32 props.ShaperCategory <<< 16) ||| (uint32 props.ShaperAuxiliary <<< 24)

    /// <summary>Mixes one word into a hash.</summary>
    /// <param name="hash">The hash so far.</param>
    /// <param name="word">The word.</param>
    /// <returns>The mixed hash.</returns>
    let private mix (hash: uint64) (word: uint32) =
        let mixed = (hash ^^^ uint64 word) * HashPrime
        mixed ^^^ (mixed >>> 29)

    /// <summary>The hash of a unit's key from the hash of its glyphs and the header words.</summary>
    /// <param name="glyphs">The hash of the glyphs, <see cref="glyphHash"/>.</param>
    /// <param name="header0">The first header word.</param>
    /// <param name="header1">The second header word, the scratch flags.</param>
    /// <param name="header2">The third header word, the buffer flags.</param>
    /// <returns>The hash.</returns>
    let withHeader (glyphs: uint64) (header0: uint32) (header1: uint32) (header2: uint32) =
        mix (mix (mix glyphs header0) header1) header2

    /// <summary>The hash of a unit's glyphs, read from the line without building the key.</summary>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph.</param>
    /// <param name="count">The glyphs of the unit.</param>
    /// <param name="baseCluster">The unit's lowest cluster.</param>
    /// <returns>The hash.</returns>
    let glyphHash (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (count: int) (baseCluster: uint32) =
        let mutable hash = HashSeed
        let mutable place = 0
        let mutable index = start
        while index < start + count do
            let info = infos[index]
            let glyphProps = props[index]
            place <- place + (match index > start && glyphProps.Syllable <> props[index - 1].Syllable with | true -> 1 | false -> 0)
            hash <- mix (mix (mix (mix (mix (mix (mix (mix hash info.Codepoint) info.Mask) (info.Cluster - baseCluster)) info.Var1) info.Var2) (classWord glyphProps)) (shaperWord glyphProps place)) glyphProps.GlyphIndex
            index <- index + 1
        hash

    /// <summary>Whether a stored key is the key of a unit of the line.</summary>
    /// <param name="key">The stored key.</param>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph.</param>
    /// <param name="count">The glyphs of the unit.</param>
    /// <param name="baseCluster">The unit's lowest cluster.</param>
    /// <param name="glyphs">The hash of the unit's glyphs.</param>
    /// <param name="header0">The first header word.</param>
    /// <param name="header1">The second header word.</param>
    /// <param name="header2">The third header word.</param>
    /// <returns>True when every word is equal.</returns>
    let matches (key: uint32 array) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (count: int) (baseCluster: uint32) (glyphs: uint64) (header0: uint32) (header1: uint32) (header2: uint32) =
        let mutable same =
            key.Length = HeaderWords + count * GlyphWords && key[0] = header0 && key[1] = header1 && key[2] = header2
            && key[GlyphHashWord] = uint32 glyphs && key[GlyphHashWord + 1] = uint32 (glyphs >>> 32)
        let mutable place = 0
        let mutable index = 0
        while same && index < count do
            let info = infos[start + index]
            let glyphProps = props[start + index]
            place <- place + (match index > 0 && glyphProps.Syllable <> props[start + index - 1].Syllable with | true -> 1 | false -> 0)
            let at = HeaderWords + index * GlyphWords
            same <-
                key[at] = info.Codepoint && key[at + 1] = info.Mask && key[at + 2] = info.Cluster - baseCluster && key[at + 3] = info.Var1
                && key[at + 4] = info.Var2 && key[at + 5] = classWord glyphProps && key[at + 6] = shaperWord glyphProps place && key[at + 7] = glyphProps.GlyphIndex
            index <- index + 1
        same

    /// <summary>Builds the key of a unit of the line.</summary>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph.</param>
    /// <param name="count">The glyphs of the unit.</param>
    /// <param name="baseCluster">The unit's lowest cluster.</param>
    /// <param name="header0">The first header word.</param>
    /// <param name="header1">The second header word.</param>
    /// <param name="header2">The third header word.</param>
    /// <returns>The key, the hash of the glyphs (<see cref="glyphHash"/>, the same words mixed in the same order) in its hash words.</returns>
    let keyOf (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (count: int) (baseCluster: uint32) (header0: uint32) (header1: uint32) (header2: uint32) =
        let key = Array.zeroCreate<uint32> (HeaderWords + count * GlyphWords)
        key[0] <- header0
        key[1] <- header1
        key[2] <- header2
        let mutable place = 0
        let mutable index = 0
        while index < count do
            let info = infos[start + index]
            let glyphProps = props[start + index]
            place <- place + (match index > 0 && glyphProps.Syllable <> props[start + index - 1].Syllable with | true -> 1 | false -> 0)
            let at = HeaderWords + index * GlyphWords
            key[at] <- info.Codepoint
            key[at + 1] <- info.Mask
            key[at + 2] <- info.Cluster - baseCluster
            key[at + 3] <- info.Var1
            key[at + 4] <- info.Var2
            key[at + 5] <- classWord glyphProps
            key[at + 6] <- shaperWord glyphProps place
            key[at + 7] <- glyphProps.GlyphIndex
            index <- index + 1
        let mutable glyphs = HashSeed
        let mutable word = HeaderWords
        while word < key.Length do
            glyphs <- mix glyphs key[word]
            word <- word + 1
        key[GlyphHashWord] <- uint32 glyphs
        key[GlyphHashWord + 1] <- uint32 (glyphs >>> 32)
        key

    /// <summary>Finds the entry of a unit of the line in a table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="hash">The unit's hash.</param>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph.</param>
    /// <param name="count">The glyphs of the unit.</param>
    /// <param name="baseCluster">The unit's lowest cluster.</param>
    /// <param name="glyphs">The hash of the unit's glyphs.</param>
    /// <param name="header0">The first header word.</param>
    /// <param name="header1">The second header word.</param>
    /// <param name="header2">The third header word.</param>
    /// <returns>The entry, or null when the table holds none.</returns>
    let find (table: SyllableTable) (hash: uint64) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (count: int) (baseCluster: uint32) (glyphs: uint64) (header0: uint32) (header1: uint32) (header2: uint32) =
        let mutable found: SyllableOutcome | null = null
        match table.Slots with
        | null -> ()
        | slots ->
            let first = SyllableTableSize.firstSlot hash slots.Length
            let mask = slots.Length - 1
            let mutable probe = 0
            let mutable ended = false
            while isNull found && not ended && probe < SyllableTableSize.Probes do
                match System.Threading.Volatile.Read(&slots[(first + probe) &&& mask]) with
                | null -> ended <- true
                | entry ->
                    match entry.OutcomeHash = hash && matches entry.OutcomeKey infos props start count baseCluster glyphs header0 header1 header2 with
                    | true -> found <- entry
                    | false -> probe <- probe + 1
        found

    /// <summary>The next ligature id and serial, <c>_hb_allocate_lig_id</c> over <c>hb_buffer_t::next_serial</c>: the serial skips zero, and an id
    /// of zero is skipped too. The same rule as <c>Matching.allocateLigId</c>, which is compiled after this file.</summary>
    /// <param name="serial">The serial.</param>
    /// <returns>The id and the advanced serial.</returns>
    let allocateLigId (serial: byte) =
        let mutable next = match serial + 1uy with | 0uy -> serial + 2uy | value -> value
        let mutable ligId = next &&& LigIdBits
        while ligId = 0uy do
            next <- (match next + 1uy with | 0uy -> next + 2uy | value -> value)
            ligId <- next &&& LigIdBits
        struct (ligId, next)

    /// <summary>Whether a glyph's properties make it a ligature component: a ligature id and a component index, a mark a ligature took in.</summary>
    /// <param name="ligProps">The ligature properties.</param>
    /// <returns>True for a component.</returns>
    let isComponent (ligProps: byte) =
        ligProps >>> LigIdShift <> 0uy && ligProps &&& LigBaseBit = 0uy && ligProps &&& ComponentBits <> 0uy

    /// <summary>The syllable of a line that starts at a glyph, a run of glyphs with the same syllable byte: the position after it and its lowest
    /// and highest clusters; past the last glyph, the end of the line.</summary>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The syllable's first glyph.</param>
    /// <returns>The position after the syllable, its lowest cluster and its highest.</returns>
    let syllableAt (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) =
        match start < infos.Length with
        | false -> struct (start, 0u, 0u)
        | true ->
            let mutable low = infos[start].Cluster
            let mutable high = low
            let mutable next = start + 1
            while next < infos.Length && props[next].Syllable = props[next - 1].Syllable do
                low <- min low infos[next].Cluster
                high <- max high infos[next].Cluster
                next <- next + 1
            struct (next, low, high)

    /// <summary>The unit of a line that starts with a syllable: a unit is a syllable or a run of syllables whose clusters meet, each syllable
    /// after the first having a cluster at or below the highest of the unit before it. The syllable after the unit is read to end it, and
    /// handed back for the next unit to start with.</summary>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="first">The unit's first syllable, <see cref="syllableAt"/>.</param>
    /// <returns>The position after the unit, its lowest cluster, whether it holds no more syllables than its key can tell apart, and the
    /// syllable after it.</returns>
    let nextUnit (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (first: struct (int * uint32 * uint32)) =
        let struct (firstEnd, firstLow, firstHigh) = first
        let mutable unitLow = firstLow
        let mutable unitHigh = firstHigh
        let mutable syllables = 1
        let mutable finish = firstEnd
        let mutable following = syllableAt infos props firstEnd
        let mutable joining = true
        while joining && finish < infos.Length do
            let struct (next, low, high) = following
            joining <- low <= unitHigh
            match joining with
            | false -> ()
            | true ->
                unitLow <- min unitLow low
                unitHigh <- max unitHigh high
                syllables <- syllables + 1
                finish <- next
                following <- syllableAt infos props next
        struct (finish, unitLow, syllables <= MaxUnitSyllables, following)

    /// <summary>The units of a line, <see cref="nextUnit"/> after <see cref="nextUnit"/>.</summary>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="count">The units of the line.</param>
    /// <returns>The units, in order.</returns>
    let unitsOf (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (count: int) =
        let lineUnits = Array.zeroCreate<LineUnit> count
        let mutable syllable = syllableAt infos props 0
        let mutable unit = 0
        while unit < count do
            let struct (finish, low, _, following) = nextUnit infos props syllable
            lineUnits[unit] <- { UnitEnd = finish; UnitBase = low }
            syllable <- following
            unit <- unit + 1
        lineUnits

    /// <summary>The serials of the syllables of a unit of the line, four bits for each place, in place order, and the unit's lowest cluster.</summary>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph.</param>
    /// <param name="finish">The position after the unit.</param>
    /// <returns>The serials and the lowest cluster.</returns>
    let private unitSerials (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) =
        let mutable serials = uint64 (props[start].Syllable >>> SerialShift)
        let mutable lowest = infos[start].Cluster
        let mutable place = 0
        let mutable index = start + 1
        while index < finish do
            lowest <- min lowest infos[index].Cluster
            match props[index].Syllable <> props[index - 1].Syllable with
            | true ->
                place <- place + 1
                serials <- serials ||| (uint64 (props[index].Syllable >>> SerialShift) <<< (SerialWidth * place))
            | false -> ()
            index <- index + 1
        struct (serials, lowest)

    /// <summary>The place of one allocation of a unit among the allocations of a rebuilt line, in the order <c>hb_ot_map_t::apply</c> makes them:
    /// lookup by lookup, unit by unit within a lookup, and a unit's own in their order, which follows its lookups. The count grows by a
    /// statement: the optimised tier of the WebAssembly interpreter never ends these loops when they add a 0 or 1 made from
    /// <c>a || (b &amp;&amp; c)</c>.</summary>
    /// <param name="outcomes">What each unit of the line becomes.</param>
    /// <param name="unit">The unit.</param>
    /// <param name="allocation">The allocation among the unit's.</param>
    /// <returns>The place.</returns>
    let private rankOf (outcomes: SyllableOutcome array) (unit: int) (allocation: int) =
        let position = outcomes[unit].OutcomeAllocations[allocation]
        let mutable rank = allocation
        let mutable other = 0
        while other < outcomes.Length do
            let allocations = outcomes[other].OutcomeAllocations
            let mutable index = 0
            while other <> unit && index < allocations.Length do
                match allocations[index] < position || (allocations[index] = position && other < unit) with
                | true -> rank <- rank + 1
                | false -> ()
                index <- index + 1
            other <- other + 1
        rank

    /// <summary>The ligature id a rebuilt line gives one allocation of a unit: from the ids in the order they are made, three bits each, when the
    /// line makes few enough to pack them, or else from the ids put down to each unit's allocations.</summary>
    /// <param name="outcomes">What each unit of the line becomes.</param>
    /// <param name="offsets">The first allocation of each unit among the ids, empty when the ids are packed.</param>
    /// <param name="ids">The id of each unit's allocations, empty when the ids are packed.</param>
    /// <param name="packed">The ids in the order they are made, three bits each.</param>
    /// <param name="unit">The unit.</param>
    /// <param name="allocation">The allocation among the unit's.</param>
    /// <returns>The id.</returns>
    let private idOf (outcomes: SyllableOutcome array) (offsets: int array) (ids: byte array) (packed: uint64) (unit: int) (allocation: int) =
        match ids.Length with
        | 0 -> byte ((packed >>> (IdWidth * rankOf outcomes unit allocation)) &&& IdMask)
        | _ -> ids[offsets[unit] + allocation]

    /// <summary>Rebuilds a line from the learned units, in the order <c>hb_ot_map_t::apply</c> would have made its ligature ids: lookup by
    /// lookup, unit by unit within a lookup, each id taken from the serial the line had. Where a unit holds a ligature component, whose id the
    /// lookups compare, the line is refused when two ids of that unit meet, and for a unit learned with two allocations of one id, when they get
    /// two ids, since its glyphs do not tell which of them they hold; it is refused when the first glyph of a unit had, as a ligature
    /// component, an id of the unit before it, the one way a lookup compares ids across two units (the ligature base search of
    /// <c>match_input</c> and the mark adjustment of <c>ligate_input</c> pass from a unit into the one before only through components); it is
    /// refused too when fourteen loose syllables follow one another, or when it would run out of operations or length. The operations of a
    /// unit are an upper bound of its own, so the operations left are at most those the lookups would have left. A rebuilt line as long as the
    /// line, whose units never write past the glyphs of the line still to be read, is written over the line's own arrays, its serials read
    /// unit by unit before the unit is written. Each unit covers as many glyphs of the line as its key holds.</summary>
    /// <param name="infos">The glyphs of the line, on arrays the caller owns: a rebuilt line of the same length is written over them.</param>
    /// <param name="props">The properties of the line, whose syllable bytes give the serials, on an array the caller owns.</param>
    /// <param name="outcomes">What each unit of the line becomes.</param>
    /// <param name="serial">The serial of the line.</param>
    /// <param name="maxOps">The operations the line has left.</param>
    /// <param name="maxLen">The length the line may reach.</param>
    /// <param name="scratch">The scratch flags of the line.</param>
    /// <returns>The rebuilt line, or none when a guard refuses it.</returns>
    let replay (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (outcomes: SyllableOutcome array) (serial: byte) (maxOps: int) (maxLen: int) (scratch: ScratchFlags) =
        let count = outcomes.Length
        let mutable total = 0
        let mutable unit = 0
        while unit < count do
            total <- total + outcomes[unit].OutcomeAllocations.Length
            unit <- unit + 1
        let fits = total <= PackedIds && count <= PackedUnits
        let offsets = match fits with | true -> Array.Empty<int>() | false -> Array.zeroCreate<int> (count + 1)
        let ids = match fits with | true -> Array.Empty<byte>() | false -> Array.zeroCreate<byte> total
        let mutable packed = 0UL
        let mutable current = serial
        let mutable made = 0
        while fits && made < total do
            let struct (ligId, next) = allocateLigId current
            packed <- packed ||| (uint64 ligId <<< (IdWidth * made))
            current <- next
            made <- made + 1
        match fits with
        | true -> ()
        | false ->
            unit <- 0
            while unit < count do
                offsets[unit + 1] <- offsets[unit] + outcomes[unit].OutcomeAllocations.Length
                unit <- unit + 1
            let events = Array.zeroCreate<int64> total
            unit <- 0
            while unit < count do
                let allocations = outcomes[unit].OutcomeAllocations
                let mutable allocation = 0
                while allocation < allocations.Length do
                    events[offsets[unit] + allocation] <- (int64 allocations[allocation] <<< 32) ||| (int64 unit <<< EventUnitShift) ||| int64 allocation
                    allocation <- allocation + 1
                unit <- unit + 1
            Array.Sort events
            let mutable event = 0
            while event < total do
                let struct (ligId, next) = allocateLigId current
                let owner = int ((events[event] >>> EventUnitShift) &&& EventUnitMask)
                ids[offsets[owner] + int (events[event] &&& EventAllocationMask)] <- ligId
                current <- next
                event <- event + 1
        let mutable valid = true
        let mutable previous = 0uy
        let mutable looseRun = 0
        let mutable ops = 0L
        let mutable longest = 0L
        let mutable length = 0
        let mutable behind = true
        let mutable flags = scratch
        let mutable read = 0
        unit <- 0
        while valid && unit < count do
            let outcome = outcomes[unit]
            read <- read + (outcome.OutcomeKey.Length - HeaderWords) / GlyphWords
            let mutable bits = outcome.OutcomeInputIds
            let mutable first = outcome.OutcomeFirstHeld
            let mutable allocation = 0
            while valid && allocation < outcome.OutcomeAllocations.Length do
                let ligId = idOf outcomes offsets ids packed unit allocation
                let bit = 1uy <<< int ligId
                let twinned = outcome.OutcomeTwins.Length = 0 || ligId = idOf outcomes offsets ids packed unit (int outcome.OutcomeTwins[allocation])
                valid <- twinned && (not outcome.OutcomeComponents || bits &&& bit = 0uy)
                bits <- bits ||| bit
                first <- first ||| (match (outcome.OutcomeFirstAllocations >>> allocation) &&& 1uy with | 0uy -> 0uy | _ -> bit)
                allocation <- allocation + 1
            valid <- valid && first &&& previous = 0uy
            previous <- bits
            let mutable place = 0
            while valid && place < outcome.OutcomeSyllables do
                looseRun <- (match (outcome.OutcomeLoose >>> place) &&& 1us with | 0us -> 0 | _ -> looseRun + 1)
                valid <- looseRun <= MaxLooseRun
                place <- place + 1
            ops <- ops + int64 outcome.OutcomeOps
            longest <- longest + int64 outcome.OutcomeLongest
            length <- length + outcome.OutcomeGlyphs.Length
            behind <- behind && length <= read
            flags <- flags ||| outcome.OutcomeScratch
            unit <- unit + 1
        match valid && ops < int64 maxOps && longest * 2L <= int64 maxLen with
        | false -> ValueNone
        | true ->
            let inPlace = behind && length = props.Length
            let outInfos = match inPlace with | true -> nonNull (ImmutableCollectionsMarshal.AsArray infos) | false -> Array.zeroCreate<GlyphInfo> length
            let outProps = match inPlace with | true -> nonNull (ImmutableCollectionsMarshal.AsArray props) | false -> Array.zeroCreate<GlyphProps> length
            let mutable write = 0
            let mutable start = 0
            unit <- 0
            while unit < count do
                let outcome = outcomes[unit]
                let finish = start + (outcome.OutcomeKey.Length - HeaderWords) / GlyphWords
                let struct (serials, baseCluster) = unitSerials infos props start finish
                let mutable index = 0
                while index < outcome.OutcomeGlyphs.Length do
                    let learned = outcome.OutcomeGlyphs[index]
                    let glyphProps = learned.LearnedProps
                    outInfos[write] <- { learned.LearnedInfo with Cluster = learned.LearnedInfo.Cluster + baseCluster }
                    let ligProps =
                        match learned.LearnedLabel with
                        | 0uy -> glyphProps.LigProps
                        | label -> glyphProps.LigProps ||| (idOf outcomes offsets ids packed unit (int label - 1) <<< LigIdShift)
                    let place = int (glyphProps.Syllable >>> SerialShift)
                    let syllable = (byte ((serials >>> (SerialWidth * place)) &&& SerialMask) <<< SerialShift) ||| (glyphProps.Syllable &&& SyllableTypeBits)
                    outProps[write] <- { glyphProps with Syllable = syllable; LigProps = ligProps }
                    write <- write + 1
                    index <- index + 1
                start <- finish
                unit <- unit + 1
            ValueSome
                { ReplayInfos = ImmutableCollectionsMarshal.AsImmutableArray outInfos
                  ReplayProps = ImmutableCollectionsMarshal.AsImmutableArray outProps
                  ReplaySerial = current
                  ReplayMaxOps = maxOps - int ops
                  ReplayScratch = flags }

    /// <summary>The first and the last syllable byte of a unit whose type is a broken cluster, 0 when it has none.</summary>
    /// <param name="props">The properties of the line.</param>
    /// <param name="start">The unit's first glyph.</param>
    /// <param name="finish">The position after the unit.</param>
    /// <param name="broken">The syllable type of a broken cluster.</param>
    /// <returns>The first and the last broken syllable byte.</returns>
    let private brokenBytes (props: ImmutableArray<GlyphProps>) (start: int) (finish: int) (broken: byte) =
        let mutable first = 0uy
        let mutable last = 0uy
        let mutable index = start
        while index < finish do
            let syllable = props[index].Syllable
            match syllable &&& SyllableTypeBits = broken with
            | true ->
                first <- (match first with | 0uy -> syllable | kept -> kept)
                last <- syllable
            | false -> ()
            index <- index + 1
        struct (first, last)

    /// <summary>The mark a line puts on a unit to learn before it builds the unit's key.</summary>
    let private PendingKey = [| 0u |]

    /// <summary>The hash of the glyphs a key was built for.</summary>
    /// <param name="key">The key.</param>
    /// <returns>The hash.</returns>
    let keyGlyphs (key: uint32 array) =
        uint64 key[GlyphHashWord] ||| (uint64 key[GlyphHashWord + 1] <<< 32)

    /// <summary>Looks a unit up under one letter-before bit.</summary>
    /// <param name="table">The plan's table.</param>
    /// <param name="infos">The glyphs of the line.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph.</param>
    /// <param name="count">The glyphs of the unit.</param>
    /// <param name="baseCluster">The unit's lowest cluster.</param>
    /// <param name="glyphs">The hash of the unit's glyphs.</param>
    /// <param name="header0">The first header word.</param>
    /// <param name="header1">The second header word.</param>
    /// <param name="header2">The third header word.</param>
    /// <returns>The entry, or null.</returns>
    let private lookUp (table: SyllableTable) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (count: int) (baseCluster: uint32) (glyphs: uint64) (header0: uint32) (header1: uint32) (header2: uint32) =
        find table (withHeader glyphs header0 header1 header2) infos props start count baseCluster glyphs header0 header1 header2

    /// <summary>Decides what the syllable reuse does with a line. The units are looked up along the line: a unit whose glyph before does not
    /// matter to what it becomes is kept under the letter-before bit cleared, the others under the bit the unit before gives them, so a unit is
    /// looked up under the cleared bit and then, when it is not found or the glyph before matters to it and the bit is set, under the set bit;
    /// after a unit the table does not hold, under either bit. A line whose every unit the table holds is rebuilt from them. A line whose every
    /// missing unit may be learned, while the table still learns, is shaped through the region and learns them from that run, with those whose
    /// glyph before matters and that the table holds only under the other bit, so that the line and the lines like it can be rebuilt next time. A
    /// missing unit may be learned when the units of the text recur (<see cref="SyllableTable.Recurring"/>) or when it was met before; one that
    /// may not is counted as met, the units after it are only counted, and the line is shaped the ordinary way. The dotted circles of broken
    /// syllables are inserted in a pass over the whole line that remembers the last syllable it gave one and gives none to a syllable with the
    /// same byte (<c>hb_syllabic_insert_dotted_circles</c>), so a line where the first broken syllable of a unit has the byte of the last broken
    /// syllable before it is not rebuilt, and that unit is not learned. A line with a unit of more syllables than its key tells apart is shaped
    /// the ordinary way.</summary>
    /// <param name="table">The plan's table.</param>
    /// <param name="infos">The glyphs of the line after the setup of its syllables, not empty.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="scratch">The scratch flags of the line.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="serial">The serial of the line.</param>
    /// <param name="maxOps">The operations the line has left.</param>
    /// <param name="maxLen">The length the line may reach.</param>
    /// <param name="broken">The syllable type of a broken cluster.</param>
    /// <returns>What to do with the line.</returns>
    let reuseLine (table: SyllableTable) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (scratch: ScratchFlags) (flags: BufferFlags) (level: ClusterLevel) (serial: byte) (maxOps: int) (maxLen: int) (broken: byte) =
        let mutable count = 0
        let mutable fits = true
        let mutable syllable = syllableAt infos props 0
        let mutable start = 0
        while fits && start < infos.Length do
            let struct (finish, _, unitFits, following) = nextUnit infos props syllable
            fits <- unitFits
            count <- count + 1
            start <- finish
            syllable <- following
        match fits with
        | false -> LineOrdinary
        | true ->
            let outcomes = Array.zeroCreate<SyllableOutcome> count
            let header1 = uint32 scratch
            let header2 = uint32 flags
            let mutable keys: uint32 array array | null = null
            let mutable rebuilt = true
            let mutable learning = false
            let mutable allMet = true
            let recurring = table.Recurring
            let mutable lookedUp = 0
            let mutable foundThere = 0
            let mutable known = true
            let mutable letterBefore = false
            let mutable lastBroken = 0uy
            start <- 0
            syllable <- syllableAt infos props 0
            let mutable unit = 0
            while unit < count do
                let struct (finish, baseCluster, _, following) = nextUnit infos props syllable
                let length = finish - start
                let struct (firstBroken, unitBroken) = brokenBytes props start finish broken
                let quirked = firstBroken <> 0uy && firstBroken = lastBroken
                lastBroken <- (match unitBroken with | 0uy -> lastBroken | value -> value)
                let hash = glyphHash infos props start length baseCluster
                let header0 = headerWord length false (unit = 0) level
                let exact = known && letterBefore
                lookedUp <- lookedUp + (match allMet with | true -> 1 | false -> 0)
                let entry =
                    match allMet with
                    | false -> null
                    | true ->
                        match lookUp table infos props start length baseCluster hash header0 header1 header2 with
                        | null when known && not exact -> null
                        | null -> lookUp table infos props start length baseCluster hash (otherLetterBefore header0) header1 header2
                        | found when not found.OutcomeSensitive || not exact || not known -> found
                        | _ -> lookUp table infos props start length baseCluster hash (otherLetterBefore header0) header1 header2
                rebuilt <- rebuilt && not quirked && not (isNull entry)
                foundThere <- foundThere + (match isNull entry with | true -> 0 | false -> 1)
                let learnable =
                    match entry with
                    | null ->
                        known <- false
                        let sighting = withHeader hash header0 header1 header2
                        match table.Saturated || quirked, allMet && (recurring || table.SeenBefore sighting) with
                        | true, _ -> false
                        | false, true -> true
                        | false, false ->
                            table.Note sighting
                            allMet <- false
                            false
                    | found when known ->
                        outcomes[unit] <- found
                        letterBefore <- found.OutcomeTailInRange
                        false
                    | found -> found.OutcomeSensitive && not table.Saturated && not quirked
                learning <- learning || (learnable && isNull entry)
                match learnable with
                | false -> ()
                | true ->
                    match keys with
                    | null -> keys <- Array.create count (Array.Empty<uint32>())
                    | _ -> ()
                    (nonNull keys)[unit] <- PendingKey
                start <- finish
                syllable <- following
                unit <- unit + 1
            table.Count(lookedUp, foundThere)
            match rebuilt, learning && allMet with
            | true, _ ->
                match replay infos props outcomes serial maxOps maxLen scratch with
                | ValueSome replayed -> LineRebuilt replayed
                | ValueNone -> LineOrdinary
            | false, true ->
                let marked = nonNull keys
                let units = unitsOf infos props count
                start <- 0
                unit <- 0
                while unit < count do
                    let finish = units[unit].UnitEnd
                    match obj.ReferenceEquals(marked[unit], PendingKey) with
                    | false -> ()
                    | true ->
                        let length = finish - start
                        marked[unit] <- keyOf infos props start length units[unit].UnitBase (headerWord length false (unit = 0) level) header1 header2
                    start <- finish
                    unit <- unit + 1
                LineLearning
                    { HarvestUnits = units
                      HarvestKeys = marked
                      HarvestLevel = level
                      HarvestScratch = header1
                      HarvestFlags = header2 }
            | false, false -> LineOrdinary

    /// <summary>The map position of the lookup each ligature id of a unit came from, and the id, in the order they were made, from the
    /// allocations of a line's run.</summary>
    /// <param name="made">The allocations: the unit, the map position and the id of each.</param>
    /// <param name="madeCount">The allocations in use.</param>
    /// <param name="unit">The unit.</param>
    /// <returns>The positions and the ids.</returns>
    let allocationsOf (made: int64 array) (madeCount: int) (unit: int) =
        let mutable owned = 0
        let mutable index = 0
        while index < madeCount do
            owned <- owned + (match int (made[index] >>> 40) = unit with | true -> 1 | false -> 0)
            index <- index + 1
        let positions = match owned with | 0 -> Array.Empty<int>() | _ -> Array.zeroCreate<int> owned
        let unitIds = match owned with | 0 -> Array.Empty<byte>() | _ -> Array.zeroCreate<byte> owned
        let mutable written = 0
        index <- 0
        while index < madeCount do
            match int (made[index] >>> 40) = unit with
            | true ->
                positions[written] <- int ((made[index] >>> 8) &&& 0xFFFFFFFFL)
                unitIds[written] <- byte (made[index] &&& 0xFFL)
                written <- written + 1
            | false -> ()
            index <- index + 1
        struct (positions, unitIds)

/// <summary>The bits of what a watch notes of each unit.</summary>
[<RequireQualifiedAccess>]
module internal WatchMarks =
    /// <summary>A glyph of the unit was a ligature component.</summary>
    [<Literal>]
    let Component = 1uy

    /// <summary>The category of the unit's last glyph after the final reordering is between Cf and Mn.</summary>
    [<Literal>]
    let Tail = 4uy

    /// <summary>The unit starts with a pre-base matra after the final reordering, or the final reordering was not seen.</summary>
    [<Literal>]
    let Sensitive = 8uy

    /// <summary>The allocations a watch makes room for when a line makes its first ligature id.</summary>
    [<Literal>]
    let FirstAllocations = 4

    /// <summary>The bytes a watch keeps for each unit.</summary>
    [<Literal>]
    let UnitBytes = 5

    /// <summary>The block of a unit's ids at the last look.</summary>
    [<Literal>]
    let Now = 0

    /// <summary>The block of every id a unit held.</summary>
    [<Literal>]
    let Ever = 1

    /// <summary>The block of a unit's ids at the start.</summary>
    [<Literal>]
    let Held = 2

    /// <summary>The block of the ids a unit's first glyph had while a ligature component.</summary>
    [<Literal>]
    let First = 3

    /// <summary>The block of a unit's marks.</summary>
    [<Literal>]
    let Marks = 4

    /// <summary>The shift of a syllable's unit in its word.</summary>
    [<Literal>]
    let private OwnerShift = 12

    /// <summary>The shift of a syllable's place in its word.</summary>
    [<Literal>]
    let private PlaceShift = 8

    /// <summary>The bits of a syllable's place in its word, after the shift.</summary>
    [<Literal>]
    let private PlaceBits = 0xF

    /// <summary>The bits of a syllable's byte in its word.</summary>
    [<Literal>]
    let private ByteBits = 0xFF

    /// <summary>The word of a syllable: its unit, its place in the unit and its byte.</summary>
    /// <param name="unit">The unit.</param>
    /// <param name="place">The place.</param>
    /// <param name="syllable">The syllable byte.</param>
    /// <returns>The word.</returns>
    let wordOf (unit: int) (place: int) (syllable: byte) =
        (unit <<< OwnerShift) ||| (place <<< PlaceShift) ||| int syllable

    /// <summary>The unit of a syllable's word.</summary>
    /// <param name="word">The word.</param>
    /// <returns>The unit.</returns>
    let ownerOf (word: int) = word >>> OwnerShift

    /// <summary>The byte of a syllable's word.</summary>
    /// <param name="word">The word.</param>
    /// <returns>The syllable byte.</returns>
    let byteOf (word: int) = byte (word &&& ByteBits)

    /// <summary>The bit of a syllable's place, from its word.</summary>
    /// <param name="word">The word.</param>
    /// <returns>The bit.</returns>
    let bitOf (word: int) = 1us <<< ((word >>> PlaceShift) &&& PlaceBits)



/// <summary>What a line's own run of the reused region shows of its units, at the start, after each lookup the kernel applies that changed the
/// run and after each pause. Every lookup of the region is confined to one syllable and the reorderings work syllable by syllable, so the glyphs
/// of each syllable stay together and in line order: a glyph belongs to its syllable or to the next, and every syllable keeps at least one
/// glyph, or the run is refused. For each unit the watch notes the lookup each ligature id it made came from, the kernel telling it the
/// cluster of each ligature that made one (a run whose serial moved by other than the ids it was told of is refused), the syllables that lacked a glyph no lookup skips,
/// whether a glyph was a ligature component, the ids it ever held and those its first glyph had as a component, the most glyphs it held, and
/// after the final reordering whether its last glyph is a letter or a mark and whether it starts with a pre-base matra.</summary>
/// <param name="props">The properties of the line at the start of the region.</param>
/// <param name="lineUnits">The units of the line, their lowest clusters rising.</param>
/// <param name="serial">The serial of the line at the start of the region.</param>
/// <param name="skippable">The glyph-class bits of the glyphs a lookup may skip.</param>
/// <param name="ignorable">The Unicode-property bit of a default ignorable.</param>
/// <param name="categoryBits">The bits of the general category in the Unicode properties.</param>
/// <param name="firstCategory">The lowest category the final reordering of a syllable reads in the glyph before it, Cf.</param>
/// <param name="lastCategory">The highest, Mn.</param>
/// <param name="preBaseMatra">The position of a pre-base matra.</param>
[<Sealed>]
type internal LineWatch(props: ImmutableArray<GlyphProps>, lineUnits: LineUnit array, serial: byte, skippable: uint16, ignorable: uint16, categoryBits: uint16, firstCategory: uint16, lastCategory: uint16, preBaseMatra: byte) =
    let units = lineUnits.Length
    let syllables =
        let mutable count = 1
        let mutable index = 1
        while index < props.Length do
            count <- count + (match props[index].Syllable <> props[index - 1].Syllable with | true -> 1 | false -> 0)
            index <- index + 1
        count
    let line = Array.zeroCreate<int> syllables  // for each syllable its unit, its place in the unit and its byte, WatchMarks.wordOf
    let counts = Array.zeroCreate<int> (2 * units)  // for each unit the glyphs at the last look, then the most at a look
    let bits = Array.zeroCreate<uint16> (2 * units)  // for each unit a bit for each of its syllables, then for each that lacked an anchored glyph
    let ids = Array.zeroCreate<byte> (WatchMarks.UnitBytes * units)  // for each unit the ids at the last look, ever, at the start, of its first glyph as a component, and its marks
    let mutable allocations = Array.Empty<int64>()  // each id made: its unit, the map position of its lookup and the id; the last few await their lookup
    let mutable allocated = 0  // the allocations in use
    let mutable waiting = 0  // the ids made since the last look
    let mutable first = 0
    let mutable current = serial
    let mutable refused = false

    do
        let mutable syllable = 0
        let mutable unit = 0
        let mutable place = 0
        let mutable index = 0
        Array.Fill(ids, WatchMarks.Sensitive, WatchMarks.Marks * units, units)
        while index < props.Length do
            match index > 0 && props[index].Syllable <> props[index - 1].Syllable with
            | false -> ()
            | true ->
                syllable <- syllable + 1
                let next = match index >= lineUnits[unit].UnitEnd with | true -> unit + 1 | false -> unit
                place <- (match next = unit with | true -> place + 1 | false -> 0)
                unit <- next
            line[syllable] <- WatchMarks.wordOf unit place props[index].Syllable
            bits[unit] <- bits[unit] ||| (1us <<< place)
            index <- index + 1

    /// <summary>The unit whose clusters hold a cluster: the last whose lowest cluster is at or below it.</summary>
    /// <param name="cluster">The cluster.</param>
    /// <returns>The unit.</returns>
    let unitOfCluster (cluster: uint32) =
        let mutable low = 0
        let mutable high = units - 1
        while low < high do
            let middle = (low + high + 1) / 2
            match lineUnits[middle].UnitBase <= cluster with
            | true -> low <- middle
            | false -> high <- middle - 1
        low

    /// <summary>Looks at the run's glyphs: the glyphs, anchored syllables and ligature ids of each unit and those of its first glyph while a
    /// component, refusing the run when its glyphs leave their syllables' order. The glyphs of a unit come together, so what a unit holds is
    /// gathered as the look passes it and written once.</summary>
    /// <param name="glyphs">The properties of the run.</param>
    let see (glyphs: ReadOnlySpan<GlyphProps>) =
        let mutable syllable = 0
        let mutable syllableStart = 0
        let mutable expected = WatchMarks.byteOf line[0]
        let mutable owner = 0
        let mutable bit = WatchMarks.bitOf line[0]
        let mutable start = 0
        let mutable anchored = 0us
        let mutable heldIds = 0uy
        let mutable isPart = false
        let mutable valid = glyphs.Length > 0
        let mutable index = 0
        while valid && index <= glyphs.Length do
            let ended = index = glyphs.Length
            let next = match ended with | true -> 0uy | false -> glyphs[index].Syllable
            match not ended && next = expected with
            | true -> ()
            | false ->
                let filled = index > syllableStart
                syllable <- syllable + 1
                syllableStart <- index
                match ended || syllable >= syllables || WatchMarks.ownerOf line[syllable] <> owner with
                | false -> ()
                | true ->
                    counts[owner] <- index - start
                    counts[units + owner] <- max counts[units + owner] (index - start)
                    bits[units + owner] <- bits[units + owner] ||| (bits[owner] &&& ~~~anchored)
                    ids[WatchMarks.Now * units + owner] <- heldIds
                    ids[WatchMarks.Ever * units + owner] <- ids[WatchMarks.Ever * units + owner] ||| heldIds
                    ids[WatchMarks.Marks * units + owner] <- ids[WatchMarks.Marks * units + owner] ||| (match isPart with | true -> WatchMarks.Component | false -> 0uy)
                    start <- index
                    anchored <- 0us
                    heldIds <- 0uy
                    isPart <- false
                valid <- filled && ((ended && syllable = syllables) || (not ended && syllable < syllables && next = WatchMarks.byteOf line[syllable]))
                owner <- (match valid && not ended with | true -> WatchMarks.ownerOf line[syllable] | false -> owner)
                bit <- (match valid && not ended with | true -> WatchMarks.bitOf line[syllable] | false -> 0us)
                expected <- next
            match ended with
            | true -> ()
            | false ->
                let glyph = glyphs[index]
                let ligId = glyph.LigProps >>> SyllableReuseKernel.LigIdShift
                let bitOfId = match ligId with | 0uy -> 0uy | value -> 1uy <<< int value
                let part = SyllableReuseKernel.isComponent glyph.LigProps
                heldIds <- heldIds ||| bitOfId
                isPart <- isPart || part
                ids[WatchMarks.First * units + owner] <- ids[WatchMarks.First * units + owner] ||| (match index = start && part with | true -> bitOfId | false -> 0uy)
                anchored <- anchored ||| (match glyph.GlyphClass &&& skippable = 0us && glyph.UnicodeProps &&& ignorable = 0us with | true -> bit | false -> 0us)
            index <- index + 1
        refused <- refused || not valid

    do
        see (props.AsSpan())
        Array.Copy(ids, WatchMarks.Now * units, ids, WatchMarks.Held * units, units)

    /// <summary>Sets the map position of the first lookup of the window the kernel applies next.</summary>
    /// <param name="position">The position.</param>
    member _.Enter (position: int) = first <- position

    /// <summary>Whether the run is refused.</summary>
    /// <returns>True when it is.</returns>
    member _.Refused = refused

    /// <summary>The glyphs of a unit at the last look.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>The count.</returns>
    member _.LengthOf (unit: int) = counts[unit]

    /// <summary>The most glyphs a unit held at a look.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>The count.</returns>
    member _.LongestOf (unit: int) = counts[units + unit]

    /// <summary>The ligature ids a unit's glyphs held at the start.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>A bit for each id.</returns>
    member _.HeldOf (unit: int) = ids[WatchMarks.Held * units + unit]

    /// <summary>The syllables of a unit.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>The count.</returns>
    member _.SyllablesOf (unit: int) = BitOperations.PopCount(uint32 bits[unit])

    /// <summary>A bit for each syllable of a unit that lacked, at some look, a glyph no lookup skips.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>The bits.</returns>
    member _.LooseOf (unit: int) = bits[units + unit]

    /// <summary>Whether a glyph of a unit was a ligature component.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>True when one was.</returns>
    member _.ComponentsOf (unit: int) = ids[WatchMarks.Marks * units + unit] &&& WatchMarks.Component <> 0uy

    /// <summary>The ids a unit's first glyph had while a ligature component.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>A bit for each id.</returns>
    member _.FirstIdsOf (unit: int) = ids[WatchMarks.First * units + unit]

    /// <summary>Whether the category of a unit's last glyph after the final reordering is between Cf and Mn.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>True when it is.</returns>
    member _.TailOf (unit: int) = ids[WatchMarks.Marks * units + unit] &&& WatchMarks.Tail <> 0uy

    /// <summary>Whether a unit starts with a pre-base matra after the final reordering, or the final reordering was not seen.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>True when the glyph before it matters to it.</returns>
    member _.SensitiveOf (unit: int) = ids[WatchMarks.Marks * units + unit] &&& WatchMarks.Sensitive <> 0uy

    /// <summary>The longest run of syllables that lacked, at some look, a glyph no lookup skips.</summary>
    /// <returns>The length of the run.</returns>
    member _.LooseRun =
        let mutable run = 0
        let mutable longestRun = 0
        let mutable syllable = 0
        while syllable < syllables do
            run <- (match bits[units + WatchMarks.ownerOf line[syllable]] &&& WatchMarks.bitOf line[syllable] with | 0us -> 0 | _ -> run + 1)
            longestRun <- max longestRun run
            syllable <- syllable + 1
        longestRun

    /// <summary>The map position of the lookup each ligature id of a unit came from, and the id, in the order they were made.</summary>
    /// <param name="unit">The unit.</param>
    /// <returns>The positions and the ids.</returns>
    member _.AllocationsOf (unit: int) =
        SyllableReuseKernel.allocationsOf allocations allocated unit


    /// <summary>Whether two neighbouring units kept apart the ligature ids the lookups compare across units. A lookup compares the id of a
    /// ligature component with the glyphs before it back to the base of its ligature (<c>match_input</c>), and a ligature hands its id on to
    /// the components after it (<c>ligate_input</c>); both stop at the first glyph that is not a component with that id, so they reach from one
    /// unit into the other only through the first glyph of the unit after, while it is a component with an id of the unit before.</summary>
    /// <param name="before">The unit before.</param>
    /// <param name="after">The unit after.</param>
    /// <returns>True when the first glyph of the unit after never had, as a component, an id the unit before held.</returns>
    member private _.Apart (before: int) (after: int) =
        before < 0 || after >= units || ids[WatchMarks.First * units + after] &&& ids[WatchMarks.Ever * units + before] = 0uy

    /// <summary>Whether a unit may be learned from the run: the run is not refused, no id the unit made is one it held, the ids it made differ
    /// from one another unless no glyph of it was a ligature component (the lookups then compare none of them, and a line rebuilt from it must
    /// give the twins one id), and no lookup could compare an id of the unit before with one of its own: what the unit before does reaches it only so,
    /// and what it does reaches the unit after only in that unit's glyphs, which that unit's own guard answers for.</summary>
    /// <param name="unit">The unit.</param>
    /// <param name="unitIds">The ids the unit made, <see cref="AllocationsOf"/>.</param>
    /// <returns>True when it may be learned.</returns>
    member this.Accepts (unit: int, unitIds: byte array) =
        let mutable made = 0uy
        let mutable distinct = true
        let mutable index = 0
        while index < unitIds.Length do
            let bit = 1uy <<< int unitIds[index]
            distinct <- distinct && made &&& bit = 0uy
            made <- made ||| bit
            index <- index + 1
        let free =
            (distinct || ids[WatchMarks.Marks * units + unit] &&& WatchMarks.Component = 0uy) && made &&& ids[WatchMarks.Held * units + unit] = 0uy
            && unitIds.Length <= SyllableReuseKernel.MaxOwnAllocations
        not refused && free && this.Apart (unit - 1) unit

    /// <summary>Looks at the run after a pause, where no id is allocated; after the final reordering, notes for each unit whether its last
    /// glyph is a letter or a mark and whether it starts with a pre-base matra.</summary>
    /// <param name="glyphs">The properties of the run.</param>
    /// <param name="after">The serial of the run.</param>
    /// <param name="final">Whether the pause is the final reordering.</param>
    member _.Observe (glyphs: ImmutableArray<GlyphProps>, after: byte, final: bool) =
        see (glyphs.AsSpan())
        refused <- refused || after <> current
        match final && not refused with
        | false -> ()
        | true ->
            let mutable start = 0
            let mutable unit = 0
            while unit < units do
                let category = glyphs[start + counts[unit] - 1].UnicodeProps &&& categoryBits
                let tail = match category >= firstCategory && category <= lastCategory with | true -> WatchMarks.Tail | false -> 0uy
                let sensitive = match glyphs[start].ShaperAuxiliary = preBaseMatra with | true -> WatchMarks.Sensitive | false -> 0uy
                let at = WatchMarks.Marks * units + unit
                ids[at] <- (ids[at] &&& ~~~(WatchMarks.Tail ||| WatchMarks.Sensitive)) ||| tail ||| sensitive
                start <- start + counts[unit]
                unit <- unit + 1

    interface ILookupWatch with
        /// <summary>Looks at the run after a lookup that changed it: each id allocated since the last look is put down to the lookup and to the
        /// one unit whose glyphs gained it; when no unit or more than one did, every unit that holds it is refused, and the run when none does.</summary>
        /// <param name="index">The lookup's position in the window.</param>
        /// <param name="after">The serial after the lookup.</param>
        /// <param name="glyphs">The properties of the run.</param>
        member _.After(index, after, glyphs) =
            see glyphs
            let mutable count = 0
            while current <> after && count < waiting do
                let struct (ligId, next) = SyllableReuseKernel.allocateLigId current
                let at = allocated - waiting + count
                allocations[at] <- allocations[at] ||| (int64 (first + index) <<< 8) ||| int64 ligId
                current <- next
                count <- count + 1
            refused <- refused || current <> after || count <> waiting
            waiting <- 0

        /// <summary>Puts down a ligature id made to the unit whose clusters hold the ligature's: the units' clusters rise and do not meet, and
        /// a lookup merges clusters only within one unit.</summary>
        /// <param name="cluster">The cluster of the ligature glyph.</param>
        member _.Allocated(cluster) =
            match allocated = allocations.Length with
            | false -> ()
            | true ->
                let larger = Array.zeroCreate<int64> (max WatchMarks.FirstAllocations (2 * allocations.Length))
                Array.Copy(allocations, larger, allocated)
                allocations <- larger
            allocations[allocated] <- int64 (unitOfCluster cluster) <<< 40
            allocated <- allocated + 1
            waiting <- waiting + 1

/// <summary>Learning the units of a line from its own run of the reused region.</summary>
[<RequireQualifiedAccess>]
module internal SyllableHarvestKernel =
    /// <summary>Whether the table holds an entry under a key whose first word is given.</summary>
    /// <param name="table">The plan's table.</param>
    /// <param name="hash">The hash of the key with that first word.</param>
    /// <param name="key">The key.</param>
    /// <param name="header0">The first word.</param>
    /// <returns>True when the table holds such an entry.</returns>
    let private holds (table: SyllableTable) (hash: uint64) (key: uint32 array) (header0: uint32) =
        let mutable found = false
        match table.Slots with
        | null -> ()
        | slots ->
            let first = SyllableTableSize.firstSlot hash slots.Length
            let mask = slots.Length - 1
            let mutable probe = 0
            let mutable ended = false
            while not found && not ended && probe < SyllableTableSize.Probes do
                match System.Threading.Volatile.Read(&slots[(first + probe) &&& mask]) with
                | null -> ended <- true
                | entry ->
                    found <-
                        entry.OutcomeHash = hash && entry.OutcomeKey.Length = key.Length && entry.OutcomeKey[0] = header0
                        && MemoryExtensions.SequenceEqual(ReadOnlySpan<uint32>(entry.OutcomeKey, 1, key.Length - 1), ReadOnlySpan<uint32>(key, 1, key.Length - 1))
                    probe <- probe + 1
        found

    /// <summary>The glyphs, properties and labels of a unit after the run: each ligature id it holds is one it made, labelled by its place among
    /// them (the first of those made with that id), or one it held, and every cluster is at or above the unit's lowest.</summary>
    /// <param name="infos">The glyphs after the run.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph after the run.</param>
    /// <param name="count">The glyphs of the unit after the run.</param>
    /// <param name="baseCluster">The unit's lowest cluster.</param>
    /// <param name="ids">The ids the unit made, in order.</param>
    /// <param name="held">The ids it held before the run.</param>
    /// <returns>The glyphs with their labels, or none when a guard refuses them.</returns>
    let private partsOf (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (count: int) (baseCluster: uint32) (ids: byte array) (held: byte) =
        let learned = Array.zeroCreate<LearnedGlyph> count
        let mutable place = 0
        let mutable valid = true
        let mutable glyph = 0
        while valid && glyph < count do
            let info = infos[start + glyph]
            let glyphProps = props[start + glyph]
            place <- place + (match glyph > 0 && glyphProps.Syllable <> props[start + glyph - 1].Syllable with | true -> 1 | false -> 0)
            let ligId = glyphProps.LigProps >>> SyllableReuseKernel.LigIdShift
            let position = Array.IndexOf(ids, ligId)
            valid <- info.Cluster >= baseCluster && (ligId = 0uy || position >= 0 || held &&& (1uy <<< int ligId) <> 0uy)
            let syllable = (byte place <<< 4) ||| (glyphProps.Syllable &&& SyllableReuseKernel.SyllableTypeBits)
            let labelled = ligId <> 0uy && position >= 0
            learned[glyph] <-
                { LearnedInfo = { info with Cluster = info.Cluster - baseCluster }
                  LearnedProps = { glyphProps with Syllable = syllable; LigProps = (match labelled with | true -> glyphProps.LigProps &&& SyllableReuseKernel.BelowLigId | false -> glyphProps.LigProps) }
                  LearnedLabel = (match labelled with | true -> byte (position + 1) | false -> 0uy) }
            glyph <- glyph + 1
        match valid with
        | true -> ValueSome learned
        | false -> ValueNone

    /// <summary>For each allocation of a unit, the first one made with the same id; empty when the ids all differ.</summary>
    /// <param name="ids">The ids of the unit's allocations, in order.</param>
    /// <returns>The twins.</returns>
    let private twinsOf (ids: byte array) =
        let mutable made = 0uy
        let mutable distinct = true
        let mutable index = 0
        while index < ids.Length do
            let bit = 1uy <<< int ids[index]
            distinct <- distinct && made &&& bit = 0uy
            made <- made ||| bit
            index <- index + 1
        match distinct with
        | true -> Array.Empty<byte>()
        | false -> Array.init ids.Length (fun index -> byte (Array.IndexOf(ids, ids[index])))

    /// <summary>A bit for each allocation of a unit whose id is among some ids.</summary>
    /// <param name="chosen">The ids.</param>
    /// <param name="ids">The ids of the unit's allocations, in order.</param>
    /// <returns>The bits.</returns>
    let private firstAllocations (chosen: byte) (ids: byte array) =
        let mutable bits = 0uy
        let mutable index = 0
        while index < ids.Length do
            bits <- bits ||| (match chosen &&& (1uy <<< int ids[index]) with | 0uy -> 0uy | _ -> 1uy <<< index)
            index <- index + 1
        bits

    /// <summary>Keeps what one unit of a line became in the line's run, unless the table holds it: when the glyph before it matters to it, under
    /// the key with the letter-before bit the unit before it gave, and otherwise under the key with the bit cleared.</summary>
    /// <param name="table">The plan's table.</param>
    /// <param name="harvest">What the line learns.</param>
    /// <param name="watch">The watch of the run.</param>
    /// <param name="infos">The glyphs after the run.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="start">The unit's first glyph after the run.</param>
    /// <param name="unit">The unit.</param>
    /// <param name="ops">The operations the line's run charged.</param>
    /// <param name="scratch">The scratch flags of the line, which the run did not change.</param>
    /// <param name="positions">The map position of the lookup each id the unit made came from.</param>
    /// <param name="ids">The ids the unit made, in order.</param>
    let private learnUnit (table: SyllableTable) (harvest: SyllableHarvest) (watch: LineWatch) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (start: int) (unit: int) (ops: int) (scratch: ScratchFlags) (positions: int array) (ids: byte array) =
        match partsOf infos props start (watch.LengthOf unit) harvest.HarvestUnits[unit].UnitBase ids (watch.HeldOf unit) with
        | ValueNone -> ()
        | ValueSome learned ->
            let key = harvest.HarvestKeys[unit]
            let count = (key.Length - SyllableReuseKernel.HeaderWords) / SyllableReuseKernel.GlyphWords
            let sensitive = watch.SensitiveOf unit
            let letterBefore = unit > 0 && watch.TailOf (unit - 1) && sensitive
            let header0 = SyllableReuseKernel.headerWord count letterBefore (unit = 0) harvest.HarvestLevel
            let hash = SyllableReuseKernel.withHeader (SyllableReuseKernel.keyGlyphs key) header0 harvest.HarvestScratch harvest.HarvestFlags
            let held = holds table hash key header0
            key[0] <- header0
            let outcome =
                { OutcomeHash = hash
                  OutcomeKey = key
                  OutcomeGlyphs = learned
                  OutcomeAllocations = positions
                  OutcomeTwins = twinsOf ids
                  OutcomeInputIds = watch.HeldOf unit
                  OutcomeTailInRange = watch.TailOf unit
                  OutcomeSensitive = sensitive
                  OutcomeComponents = watch.ComponentsOf unit
                  OutcomeFirstHeld = watch.FirstIdsOf unit &&& watch.HeldOf unit
                  OutcomeFirstAllocations = firstAllocations (watch.FirstIdsOf unit) ids
                  OutcomeSyllables = watch.SyllablesOf unit
                  OutcomeLoose = watch.LooseOf unit
                  OutcomeOps = ops
                  OutcomeScratch = scratch
                  OutcomeLongest = watch.LongestOf unit }
            match held with
            | true -> ()
            | false -> table.Keep outcome

    /// <summary>Keeps what the units a line learns became in the line's run of the region: none when the watch refused the run or when fourteen
    /// loose syllables follow one another (a lookup could then have reached past them), and of the others each unit to learn that the watch
    /// accepts; a unit that got no dotted circle has no key. Each gets the operations of the whole run, more than its own.</summary>
    /// <param name="table">The plan's table.</param>
    /// <param name="harvest">What the line learns.</param>
    /// <param name="watch">The watch of the run.</param>
    /// <param name="infos">The glyphs after the run.</param>
    /// <param name="props">Their properties.</param>
    /// <param name="ops">The operations the run charged.</param>
    /// <param name="scratch">The scratch flags of the line, which the run did not change.</param>
    let learn (table: SyllableTable) (harvest: SyllableHarvest) (watch: LineWatch) (infos: ImmutableArray<GlyphInfo>) (props: ImmutableArray<GlyphProps>) (ops: int) (scratch: ScratchFlags) =
        match watch.Refused || watch.LooseRun > SyllableReuseKernel.MaxLooseRun with
        | true -> ()
        | false ->
            let mutable start = 0
            let mutable unit = 0
            while unit < harvest.HarvestUnits.Length do
                match harvest.HarvestKeys[unit].Length > 0 with
                | false -> ()
                | true ->
                    let struct (positions, ids) = watch.AllocationsOf unit
                    match watch.Accepts(unit, ids) with
                    | true -> learnUnit table harvest watch infos props start unit ops scratch positions ids
                    | false -> ()
                start <- start + watch.LengthOf unit
                unit <- unit + 1
