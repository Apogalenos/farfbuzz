namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// <summary>The ligature-base skip classification computed by <c>match_input</c>'s out-buffer probe
/// (src/hb-ot-layout-gsubgpos.hh:1999-2051).</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type private LigBase =
    | LigBaseNotChecked
    | LigBaseMayNotSkip
    | LigBaseMaySkip

/// <summary>The shared input-matching fold, <c>match_input</c> (src/hb-ot-layout-gsubgpos.hh:1942-2079).</summary>
[<RequireQualifiedAccess>]
module internal ContextApply =
    /// <summary>The longest context a rule may match, <c>HB_MAX_CONTEXT_LENGTH</c> (src/hb-limits.hh:58).</summary>
    [<Literal>]
    let private MaxContextLength = 64

    /// <summary>The rule count above which a rule set takes the fast path, <c>num_rules &lt;= 4</c> takes the slow one
    /// (src/hb-ot-layout-gsubgpos.hh:3356).</summary>
    [<Literal>]
    let private FastPathMinRules = 4

    /// <summary>Scans the out-buffer backwards for the ligature base the first glyph belongs to, then reports
    /// whether that base may be skipped, the <c>ligbase</c> probe of <c>match_input</c>.</summary>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="firstLigId">The first glyph's ligature id.</param>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <returns>True when the base may be skipped.</returns>
    [<TailCall>]
    let rec private ligBaseMaySkip
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) (outLen: int)
            (firstLigId: byte) (matcher: Matcher) (gdef: Gdef) =
        match outLen > 0 && GlyphPropsBits.ligId outProps[outLen - 1] = firstLigId with
        | false -> false
        | true ->
            match GlyphPropsBits.ligComp outProps[outLen - 1] = 0uy with
            | true -> Matching.maySkip matcher gdef outInfos[outLen - 1] outProps[outLen - 1] = SkipVerdict.SkipYes
            | false -> ligBaseMaySkip outInfos outProps (outLen - 1) firstLigId matcher gdef

    /// <summary>Resolves the unchecked ligature base from the out-buffer probe.</summary>
    /// <param name="ligBase">The current ligature-base classification.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="firstLigId">The first glyph's ligature id.</param>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <returns>The resolved classification.</returns>
    let private resolveLigBase ligBase outInfos outProps outLen firstLigId matcher gdef =
        match ligBase with
        | LigBaseNotChecked ->
            match ligBaseMaySkip outInfos outProps outLen firstLigId matcher gdef with
            | true -> LigBaseMaySkip
            | false -> LigBaseMayNotSkip
        | LigBaseMayNotSkip -> LigBaseMayNotSkip
        | LigBaseMaySkip -> LigBaseMaySkip

    /// <summary>The ligature-component rule of <c>match_input</c>: a component is accepted, or the match fails.</summary>
    /// <param name="firstLigId">The first glyph's ligature id.</param>
    /// <param name="firstLigComp">The first glyph's ligature component.</param>
    /// <param name="thisLigId">The component's ligature id.</param>
    /// <param name="thisLigComp">The component's ligature component.</param>
    /// <param name="ligBase">The current ligature-base classification.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <returns>Whether the component is accepted, and the resolved ligature-base classification.</returns>
    let private componentAccepted firstLigId firstLigComp thisLigId thisLigComp ligBase outInfos outProps outLen matcher gdef =
        match firstLigId <> 0uy && firstLigComp <> 0uy with
        | false ->
            match thisLigId <> 0uy && thisLigComp <> 0uy && thisLigId <> firstLigId with
            | true -> struct (false, ligBase)
            | false -> struct (true, ligBase)
        | true ->
            match firstLigId <> thisLigId || firstLigComp <> thisLigComp with
            | false -> struct (true, ligBase)
            | true ->
                let resolved = resolveLigBase ligBase outInfos outProps outLen firstLigId matcher gdef
                match resolved with
                | LigBaseMayNotSkip -> struct (false, resolved)
                | LigBaseMaySkip -> struct (true, resolved)
                | LigBaseNotChecked -> struct (true, resolved)

    /// <summary>Walks the components after the first glyph, <c>match_input</c>'s loop.</summary>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="count">The component count, including the first glyph.</param>
    /// <param name="components">The components after the first glyph.</param>
    /// <param name="componentIndex">The next component, one-based.</param>
    /// <param name="matchStart">The previous component's match position.</param>
    /// <param name="firstLigId">The first glyph's ligature id.</param>
    /// <param name="firstLigComp">The first glyph's ligature component.</param>
    /// <param name="ligBase">The ligature-base classification.</param>
    /// <param name="total">The component count accumulated so far.</param>
    /// <returns>The matched flag, the end position and the total component count.</returns>
    [<TailCall>]
    let rec private matchInputLoop
            (matcher: Matcher) (gdef: Gdef)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) (outLen: int)
            (idx: int) (count: int) (components: ImmutableArray<uint32>)
            (componentIndex: int) (matchStart: int)
            (firstLigId: byte) (firstLigComp: byte)
            (ligBase: LigBase) (total: int) =
        match componentIndex < count with
        | false -> struct (true, matchStart + 1, total + int (GlyphPropsBits.ligNumComps props[idx]))
        | true ->
            match Matching.next matcher gdef (GlyphIds components) (componentIndex - 1) infos props matchStart infos.Length with
            | Probe.Miss bound -> struct (false, bound, 0)
            | Probe.Hit hitIndex ->
                let struct (accepted, nextLigBase) =
                    componentAccepted firstLigId firstLigComp
                        (GlyphPropsBits.ligId props[hitIndex]) (GlyphPropsBits.ligComp props[hitIndex])
                        ligBase outInfos outProps outLen matcher gdef
                match accepted with
                | false -> struct (false, 0, 0)
                | true ->
                    matchInputLoop matcher gdef infos props outInfos outProps outLen idx count components
                        (componentIndex + 1) hitIndex firstLigId firstLigComp nextLigBase
                        (total + int (GlyphPropsBits.ligNumCompsInLigation props[hitIndex]))

    /// <summary>Matches the components of a ligature after its first glyph, checking the ligature-component rules,
    /// <c>match_input</c> (src/hb-ot-layout-gsubgpos.hh:1942-2079).</summary>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="count">The component count, including the first glyph.</param>
    /// <param name="components">The components after the first glyph.</param>
    /// <returns>The matched flag, the end position and the total component count.</returns>
    let matchInput (matcher: Matcher) (gdef: Gdef)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) (outLen: int)
            (idx: int) (count: int) (components: ImmutableArray<uint32>) =
        match count with
        | 1 -> struct (true, idx + 1, int (GlyphPropsBits.ligNumComps props[idx]))
        | count when count > MaxContextLength -> struct (false, idx, 0)
        | _ ->
            matchInputLoop matcher gdef infos props outInfos outProps outLen idx count components 1 idx
                (GlyphPropsBits.ligId props[idx]) (GlyphPropsBits.ligComp props[idx]) LigBaseNotChecked 0

    /// <summary>Walks the input components, filling the match positions, the generic <c>match_input</c> loop
    /// (src/hb-ot-layout-gsubgpos.hh:2005-2079).</summary>
    /// <typeparam name="'D">The glyph-data matcher.</typeparam>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data the components match against.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="count">The component count, including the first glyph.</param>
    /// <param name="positions">The match positions, filled in order.</param>
    /// <param name="componentIndex">The next component, one-based.</param>
    /// <param name="matchStart">The previous component's match position.</param>
    /// <param name="firstLigId">The first glyph's ligature id.</param>
    /// <param name="firstLigComp">The first glyph's ligature component.</param>
    /// <param name="ligBase">The ligature-base classification.</param>
    /// <returns>The matched flag and the end position.</returns>
    [<TailCall>]
    let rec private matchInputPositionsLoop<'D when 'D : struct and 'D :> IGlyphData>
            matcher gdef (data: 'D)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx count (positions: Span<int>)
            componentIndex matchStart
            firstLigId firstLigComp (ligBase: LigBase) =
        match componentIndex < count with
        | false -> struct (true, matchStart + 1)
        | true ->
            match Matching.next matcher gdef data (componentIndex - 1) infos props matchStart infos.Length with
            | Probe.Miss bound -> struct (false, bound)
            | Probe.Hit hitIndex ->
                let struct (accepted, nextLigBase) =
                    componentAccepted firstLigId firstLigComp
                        (GlyphPropsBits.ligId props[hitIndex]) (GlyphPropsBits.ligComp props[hitIndex])
                        ligBase outInfos outProps outLen matcher gdef
                match accepted with
                | false -> struct (false, 0)
                | true ->
                    Sealed.setMatchPosition positions componentIndex hitIndex
                    matchInputPositionsLoop matcher gdef data infos props outInfos outProps outLen idx count positions
                        (componentIndex + 1) hitIndex firstLigId firstLigComp nextLigBase

    /// <summary>Matches the input sequence after the first glyph, filling the match positions,
    /// <c>match_input</c> (src/hb-ot-layout-gsubgpos.hh:1942-2079).</summary>
    /// <typeparam name="'D">The glyph-data matcher.</typeparam>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data the input values match against.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="count">The input count, including the first glyph.</param>
    /// <param name="positions">The buffer the match positions are written into.</param>
    /// <returns>The matched flag and the end position.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let matchInputPositions<'D when 'D : struct and 'D :> IGlyphData>
            matcher gdef (data: 'D)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx count (positions: Span<int>) =
        match count with
        | 1 ->
            Sealed.setMatchPosition positions 0 idx
            struct (true, idx + 1)
        | count when count > MaxContextLength -> struct (false, idx)
        | _ ->
            Sealed.setMatchPosition positions 0 idx
            matchInputPositionsLoop matcher gdef data infos props outInfos outProps outLen idx count positions 1 idx
                (GlyphPropsBits.ligId props[idx]) (GlyphPropsBits.ligComp props[idx]) LigBaseNotChecked

    /// <summary>Walks the backtrack sequence backward over the out-buffer, <c>match_backtrack</c>
    /// (src/hb-ot-layout-gsubgpos.hh:2201-2233).</summary>
    /// <typeparam name="'D">The glyph-data matcher.</typeparam>
    /// <param name="matcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data the backtrack values match against.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="count">The backtrack count.</param>
    /// <param name="position">The number of backtrack values matched so far.</param>
    /// <param name="index">The cursor index.</param>
    /// <returns>The matched flag and the match start, an out-buffer coordinate.</returns>
    [<TailCall>]
    let rec private matchBacktrackLoop<'D when 'D : struct and 'D :> IGlyphData>
            matcher gdef (data: 'D)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>)
            count position index =
        match position < count with
        | false -> struct (true, index)
        | true ->
            match Matching.prev matcher gdef data position outInfos outProps index with
            | Probe.Miss bound -> struct (false, bound)
            | Probe.Hit hitIndex -> matchBacktrackLoop matcher gdef data outInfos outProps count (position + 1) hitIndex

    /// <summary>Matches the backtrack sequence, <c>match_backtrack</c>.</summary>
    /// <typeparam name="'D">The glyph-data matcher.</typeparam>
    /// <param name="matcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data the backtrack values match against.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length the backtrack walks back from.</param>
    /// <param name="count">The backtrack count.</param>
    /// <returns>The matched flag and the match start.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let matchBacktrack<'D when 'D : struct and 'D :> IGlyphData>
            matcher gdef (data: 'D)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>)
            outLen count =
        match count with
        | 0 -> struct (true, outLen)
        | _ -> matchBacktrackLoop matcher gdef data outInfos outProps count 0 outLen

    /// <summary>Walks the lookahead sequence forward, <c>match_lookahead</c>
    /// (src/hb-ot-layout-gsubgpos.hh:2240-2274).</summary>
    /// <typeparam name="'D">The glyph-data matcher.</typeparam>
    /// <param name="matcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data the lookahead values match against.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="count">The lookahead count.</param>
    /// <param name="position">The number of lookahead values matched so far.</param>
    /// <param name="index">The cursor index.</param>
    /// <returns>The matched flag and the end index.</returns>
    [<TailCall>]
    let rec private matchLookaheadLoop<'D when 'D : struct and 'D :> IGlyphData>
            matcher gdef (data: 'D)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            count position index =
        match position < count with
        | false -> struct (true, index + 1)
        | true ->
            match Matching.next matcher gdef data position infos props index infos.Length with
            | Probe.Miss bound -> struct (false, bound)
            | Probe.Hit hitIndex -> matchLookaheadLoop matcher gdef data infos props count (position + 1) hitIndex

    /// <summary>Matches the lookahead sequence, <c>match_lookahead</c>.</summary>
    /// <typeparam name="'D">The glyph-data matcher.</typeparam>
    /// <param name="matcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The glyph data the lookahead values match against.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="startIndex">The input index the lookahead starts after.</param>
    /// <param name="count">The lookahead count.</param>
    /// <returns>The matched flag and the end index.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let matchLookahead<'D when 'D : struct and 'D :> IGlyphData>
            matcher gdef (data: 'D)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            startIndex count =
        match count with
        | 0 -> struct (true, startIndex)
        | _ -> matchLookaheadLoop matcher gdef data infos props count 0 (startIndex - 1)

    /// <summary>Matches a coverage sequence starting at the second entry, <c>match_coverage</c> over
    /// <c>ContextFormat3_6</c>'s coverages after the first.</summary>
    [<Struct>]
    type private CoverageRest =
        val Coverages: ImmutableArray<Coverage>
        new (coverages) = { Coverages = coverages }
        interface IGlyphData with
            member _.HasMatchFunc = true
            member this.Matches (info, _, position) =
                match Coverage.index this.Coverages[position + 1] info.Codepoint with
                | CoverageHit.Covered _ -> true
                | CoverageHit.NotCovered -> false

    /// <summary>What a context rule set matches its values against: how the values of a rule become glyph data, and the
    /// value a glyph has, <c>match_glyph</c> and <c>match_class</c> with <c>get_class_value</c>.</summary>
    /// <typeparam name="'D">The glyph data.</typeparam>
    type private IRuleSource<'D when 'D : struct and 'D :> IGlyphData> =
        abstract InputData: input: ImmutableArray<uint32> -> 'D
        abstract ValueOf: info: GlyphInfo -> uint32

    /// <summary>Rules over glyphs, <c>match_glyph</c>.</summary>
    [<Struct>]
    type private GlyphSource =
        interface IRuleSource<GlyphIds> with
            member _.InputData input = GlyphIds input
            member _.ValueOf info = info.Codepoint

    /// <summary>Rules over classes, <c>match_class</c>.</summary>
    [<Struct>]
    type private ClassSource =
        val ClassDef: ClassDef
        new (classDef) = { ClassDef = classDef }
        interface IRuleSource<ClassValues> with
            member this.InputData input = ClassValues (this.ClassDef, input)
            member this.ValueOf info = ClassDef.classOf this.ClassDef info.Codepoint

    /// <summary>What a chained rule set matches its values against, for its three sequences.</summary>
    /// <typeparam name="'D">The glyph data.</typeparam>
    type private IChainSource<'D when 'D : struct and 'D :> IGlyphData> =
        abstract BacktrackData: values: ImmutableArray<uint32> -> 'D
        abstract InputData: values: ImmutableArray<uint32> -> 'D
        abstract LookaheadData: values: ImmutableArray<uint32> -> 'D
        abstract InputValueOf: info: GlyphInfo -> uint32
        abstract LookaheadValueOf: info: GlyphInfo -> uint32

    /// <summary>Chained rules over glyphs.</summary>
    [<Struct>]
    type private ChainGlyphSource =
        interface IChainSource<GlyphIds> with
            member _.BacktrackData values = GlyphIds values
            member _.InputData values = GlyphIds values
            member _.LookaheadData values = GlyphIds values
            member _.InputValueOf info = info.Codepoint
            member _.LookaheadValueOf info = info.Codepoint

    /// <summary>Chained rules over classes, one class definition for each sequence.</summary>
    [<Struct>]
    type private ChainClassSource =
        val BacktrackClasses: ClassDef
        val InputClasses: ClassDef
        val LookaheadClasses: ClassDef
        new (backtrack, input, lookahead) = { BacktrackClasses = backtrack; InputClasses = input; LookaheadClasses = lookahead }
        interface IChainSource<ClassValues> with
            member this.BacktrackData values = ClassValues (this.BacktrackClasses, values)
            member this.InputData values = ClassValues (this.InputClasses, values)
            member this.LookaheadData values = ClassValues (this.LookaheadClasses, values)
            member this.InputValueOf info = ClassDef.classOf this.InputClasses info.Codepoint
            member this.LookaheadValueOf info = ClassDef.classOf this.LookaheadClasses info.Codepoint

    /// <summary>The first and second glyph a rule set's fast path read with <c>iter_context</c>: where they are, and
    /// whether the second exists.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Heads =
        { First: GlyphInfo
          Second: GlyphInfo
          HasSecond: bool
          UnsafeTo1: int
          UnsafeTo2: int
          FirstValue: uint32 // the value of each head a rule set compares with its rules, read once for the whole rule set
          SecondValue: uint32
          FirstLookahead: uint32
          SecondLookahead: uint32 }

    /// <summary>What reading the first head of a rule set decided.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FirstHead =
        | FirstSlow
        | FirstMissing of unsafeTo: int
        | FirstRead of first: int

    /// <summary>What reading the second head of a rule set decided.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private SecondHead =
        | SecondSlow
        | SecondRead of heads: Heads

    /// <summary>Reads the second glyph after the current one, after the first was read and the digest let it through.</summary>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="first">The first glyph's index.</param>
    /// <returns>That the slow path must run, or the heads.</returns>
    let private readSecondHead contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) first =
        match Matching.next contextMatcher gdef (MatchAlways()) 0 infos props first infos.Length with
        | Probe.Miss _ -> SecondRead { First = infos[first]; Second = infos[first]; HasSecond = false; UnsafeTo1 = first + 1; UnsafeTo2 = 0; FirstValue = 0u; SecondValue = 0u; FirstLookahead = 0u; SecondLookahead = 0u }
        | Probe.Hit second ->
            match Matching.maySkip contextMatcher gdef infos[second] props[second] with
            | SkipVerdict.SkipYes | SkipVerdict.SkipMaybe -> SecondSlow
            | SkipVerdict.SkipNo -> SecondRead { First = infos[first]; Second = infos[second]; HasSecond = true; UnsafeTo1 = first + 1; UnsafeTo2 = second + 1; FirstValue = 0u; SecondValue = 0u; FirstLookahead = 0u; SecondLookahead = 0u }

    /// <summary>Reads, once for a whole context rule set, the value of each head the rules compare with their first inputs.</summary>
    /// <typeparam name="'S">The rule source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The rule source.</param>
    /// <param name="heads">The heads.</param>
    /// <returns>The heads with their values.</returns>
    let private contextHeadValues<'S, 'D when 'S : struct and 'S :> IRuleSource<'D> and 'D : struct and 'D :> IGlyphData> (source: 'S) (heads: Heads) =
        { heads with
            FirstValue = source.ValueOf heads.First
            SecondValue = (match heads.HasSecond with | true -> source.ValueOf heads.Second | false -> 0u) }

    /// <summary>Reads, once for a whole chained rule set, the value of each head the rules compare with their first inputs and
    /// lookahead glyphs.</summary>
    /// <typeparam name="'S">The chain source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The chain source.</param>
    /// <param name="heads">The heads.</param>
    /// <returns>The heads with their values.</returns>
    let private chainHeadValues<'S, 'D when 'S : struct and 'S :> IChainSource<'D> and 'D : struct and 'D :> IGlyphData> (source: 'S) (heads: Heads) =
        { heads with
            FirstValue = source.InputValueOf heads.First
            FirstLookahead = source.LookaheadValueOf heads.First
            SecondValue = (match heads.HasSecond with | true -> source.InputValueOf heads.Second | false -> 0u)
            SecondLookahead = (match heads.HasSecond with | true -> source.LookaheadValueOf heads.Second | false -> 0u) }

    /// <summary>Reads the first glyph after the current one, skipping like <c>iter_context</c> with <c>match_always</c>: the start of the
    /// fast path of <c>RuleSet::apply</c> and <c>ChainRuleSet::apply</c>, which tests the rule set's digest on this glyph before it reads
    /// the second.</summary>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="idx">The current glyph's index.</param>
    /// <returns>That the slow path must run, that there is no first glyph, or the first glyph.</returns>
    let private readFirstHead contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) idx =
        match Matching.next contextMatcher gdef (MatchAlways()) 0 infos props idx infos.Length with
        | Probe.Miss bound -> FirstMissing bound
        | Probe.Hit first ->
            match Matching.maySkip contextMatcher gdef infos[first] props[first] with
            | SkipVerdict.SkipYes | SkipVerdict.SkipMaybe -> FirstSlow
            | SkipVerdict.SkipNo -> FirstRead first

    /// <summary>Applies one context rule's match, <c>context_apply_lookup</c> up to the point where the recursion starts.</summary>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="data">The input values.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="inputCount">The rule's input count.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>Whether the rule matched, where the match ends, and the ranges owed.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private contextApply<'D when 'D : struct and 'D :> IGlyphData>
            matcher gdef (data: 'D)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx inputCount (positions: Span<int>) owed =
        match inputCount > MaxContextLength with
        | true -> struct (false, 0, owed)
        | false ->
            match matchInputPositions matcher gdef data infos props outInfos outProps outLen idx inputCount positions with
            | struct (true, matchEnd) -> struct (true, matchEnd, owed)
            | struct (false, matchEnd) -> struct (false, matchEnd, FlagBounds.addPlain idx matchEnd owed)

    /// <summary>Tries the rules of a context rule set one after the other, <c>RuleSet::apply</c>'s slow path.</summary>
    /// <typeparam name="'S">The rule source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The rule source.</param>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private contextRulesSlow<'S, 'D when 'S : struct and 'S :> IRuleSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) matcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<Rule>) (positions: Span<int>) index owed =
        match index < rules.Length with
        | false -> ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let rule = rules[index]
            match contextApply matcher gdef (source.InputData rule.Input) infos props outInfos outProps outLen idx rule.InputCount positions owed with
            | struct (true, matchEnd, owed) -> ContextOutcome(true, false, rule.InputCount, matchEnd, rule.Records, 0, 0, owed, -1)
            | struct (false, _, owed) -> contextRulesSlow source matcher gdef infos props outInfos outProps outLen idx rules positions (index + 1) owed

    /// <summary>Whether a rule survives the fast path's skip-ahead, which steps over the rules that share the first value
    /// of a rule that failed on it.</summary>
    /// <param name="rules">The rules.</param>
    /// <param name="index">The failed rule.</param>
    /// <returns>The index of the last rule that shares the failed rule's first input value.</returns>
    [<TailCall>]
    let rec private skipSharedFirst (rules: ImmutableArray<Rule>) index =
        match index + 1 < rules.Length with
        | false -> index
        | true ->
            let next = rules[index + 1]
            match next.InputCount <= 1 || next.Input[0] <> rules[index].Input[0] with
            | true -> index
            | false -> skipSharedFirst rules (index + 1)

    /// <summary>The rule loop of <c>RuleSet::apply</c>'s fast path, which matches the first and second values here.</summary>
    /// <typeparam name="'S">The rule source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The rule source.</param>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="heads">The first and second glyph after the current one.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="unsafeTo">The end of the range the failed rules owe, or -1.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private contextRulesFast<'S, 'D when 'S : struct and 'S :> IRuleSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) matcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<Rule>) heads (positions: Span<int>) index unsafeTo owed =
        match index < rules.Length with
        | false ->
            let owed = match unsafeTo with | -1 -> owed | bound -> FlagBounds.addPlain idx bound owed
            ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let rule = &rules.ItemRef index
            let firstMatches = rule.InputCount <= 1 || heads.FirstValue = rule.Input[0]
            let secondMatches = not heads.HasSecond || rule.InputCount <= 2 || heads.SecondValue = rule.Input[1]
            match struct (firstMatches, secondMatches) with
            | struct (false, _) ->
                let unsafeTo = match unsafeTo with | -1 -> heads.UnsafeTo1 | bound -> bound
                let last = match rule.InputCount > 1 with | true -> skipSharedFirst rules index | false -> index
                contextRulesFast source matcher gdef infos props outInfos outProps outLen idx rules heads positions (last + 1) unsafeTo owed
            | struct (true, false) -> contextRulesFast source matcher gdef infos props outInfos outProps outLen idx rules heads positions (index + 1) heads.UnsafeTo2 owed
            | struct (true, true) ->
                match contextApply matcher gdef (source.InputData rule.Input) infos props outInfos outProps outLen idx rule.InputCount positions owed with
                | struct (true, matchEnd, owed) -> ContextOutcome(true, false, rule.InputCount, matchEnd, rule.Records, 0, 0, owed, unsafeTo)
                | struct (false, _, owed) -> contextRulesFast source matcher gdef infos props outInfos outProps outLen idx rules heads positions (index + 1) unsafeTo owed

    /// <summary>Applies the first rule of a list that can have a single glyph, which is all the rules the fast path tries
    /// when there is no glyph after the current one.</summary>
    /// <typeparam name="'S">The rule source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The rule source.</param>
    /// <param name="matcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="skipped">Whether a rule with more than one glyph was passed over.</param>
    /// <param name="bound">The end of the range owed for a passed-over rule.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private contextRulesAtEnd<'S, 'D when 'S : struct and 'S :> IRuleSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) matcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<Rule>) (positions: Span<int>) index skipped bound owed =
        match index < rules.Length with
        | false ->
            let owed = match skipped with | true -> FlagBounds.addPlain idx bound owed | false -> owed
            ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let rule = rules[index]
            match rule.InputCount <= 1 with
            | false -> contextRulesAtEnd source matcher gdef infos props outInfos outProps outLen idx rules positions (index + 1) true bound owed
            | true ->
                match contextApply matcher gdef (source.InputData rule.Input) infos props outInfos outProps outLen idx rule.InputCount positions owed with
                | struct (true, matchEnd, owed) -> ContextOutcome(true, false, rule.InputCount, matchEnd, rule.Records, 0, 0, owed, (match skipped with | true -> bound | false -> -1))
                | struct (false, _, owed) -> contextRulesAtEnd source matcher gdef infos props outInfos outProps outLen idx rules positions (index + 1) skipped bound owed

    /// <summary>Applies a context rule set after its first head was read, <c>RuleSet::apply</c>: the digest of the first input values
    /// refuses the rule set before the second head is read, as upstream does (src/hb-ot-layout-gsubgpos.hh:3398-3403), so that a second glyph
    /// that is skippable does not send a refused rule set down the slow path, which flags nothing.</summary>
    /// <typeparam name="'S">The rule source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The rule source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="digest">The first-class digest of the rule set, or none.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="first">The index of the first glyph after the current one.</param>
    /// <returns>The outcome.</returns>
    let private contextAfterFirst<'S, 'D when 'S : struct and 'S :> IRuleSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<Rule>) (digest: uint64 voption) (positions: Span<int>) first =
        match digest with
        | ValueSome mask when (mask &&& (1UL <<< int (source.ValueOf infos[first] &&& 63u))) = 0UL ->
            ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, FlagBounds.addPlain idx (first + 1) FlagBounds.none, -1)
        | ValueSome _ | ValueNone ->
            match readSecondHead contextMatcher gdef infos props first with
            | SecondSlow -> contextRulesSlow source inputMatcher gdef infos props outInfos outProps outLen idx rules positions 0 FlagBounds.none
            | SecondRead heads -> contextRulesFast source inputMatcher gdef infos props outInfos outProps outLen idx rules (contextHeadValues source heads) positions 0 -1 FlagBounds.none

    /// <summary>Applies a context rule set, <c>RuleSet::apply</c>: the slow path for four rules or fewer, else the fast path.</summary>
    /// <typeparam name="'S">The rule source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The rule source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="digest">The first-class digest of the rule set, or none.</param>
    /// <param name="positions">The match positions.</param>
    /// <returns>The outcome.</returns>
    let private contextRuleSet<'S, 'D when 'S : struct and 'S :> IRuleSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<Rule>) (digest: uint64 voption) (positions: Span<int>) =
        match rules.Length <= FastPathMinRules with
        | true -> contextRulesSlow source inputMatcher gdef infos props outInfos outProps outLen idx rules positions 0 FlagBounds.none
        | false ->
            match readFirstHead contextMatcher gdef infos props idx with
            | FirstSlow -> contextRulesSlow source inputMatcher gdef infos props outInfos outProps outLen idx rules positions 0 FlagBounds.none
            | FirstMissing bound -> contextRulesAtEnd source inputMatcher gdef infos props outInfos outProps outLen idx rules positions 0 false bound FlagBounds.none
            | FirstRead first -> contextAfterFirst source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules digest positions first

    /// <summary>The coverage-index guard of a rule-set subtable: the rule set the covered glyph selects, or none.</summary>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="index">The coverage index or class.</param>
    /// <returns>The rule set, empty when the index is past the array, a Null rule set.</returns>
    let private ruleSetAt (ruleSets: ImmutableArray<ImmutableArray<Rule>>) index =
        match index < ruleSets.Length with
        | true -> ruleSets[index]
        | false -> ImmutableArray<Rule>.Empty

    /// <summary>The outcome of a context subtable that does not apply.</summary>
    let private contextMiss = ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, FlagBounds.none, -1)

    /// <summary>Matches a context subtable of the third or sixth format whose coverages are not empty.</summary>
    /// <param name="inputCoverages">The input coverages, the first of which the current glyph must be in.</param>
    /// <param name="records">The lookup records.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The match positions.</param>
    /// <returns>The outcome.</returns>
    let private matchContextCoverage (inputCoverages: ImmutableArray<Coverage>) records inputMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen idx (positions: Span<int>) =
        match Coverage.index inputCoverages[0] infos[idx].Codepoint with
        | CoverageHit.NotCovered -> contextMiss
        | CoverageHit.Covered _ ->
            match contextApply inputMatcher gdef (CoverageRest inputCoverages) infos props outInfos outProps outLen idx inputCoverages.Length positions FlagBounds.none with
            | struct (true, matchEnd, owed) -> ContextOutcome(true, false, inputCoverages.Length, matchEnd, records, 0, 0, owed, -1)
            | struct (false, _, owed) -> ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)

    /// <summary>Matches a context subtable, formats 1-6, <c>Context::apply</c>.</summary>
    /// <param name="rules">The context rules.</param>
    /// <param name="useDigest">Whether the subtable is among the first eight of its lookup, which get an external cache
    /// holding the rule-set digests.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher, for the fast path's look at the next glyphs.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The buffer the match positions are written into.</param>
    /// <returns>The outcome.</returns>
    let matchContext (rules: ContextRules) useDigest gdef inputMatcher contextMatcher
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (positions: Span<int>) =
        match rules with
        | GlyphRules (coverage, ruleSets) ->
            match Coverage.index coverage infos[idx].Codepoint with
            | CoverageHit.NotCovered -> contextMiss
            | CoverageHit.Covered coverageIndex ->
                contextRuleSet (GlyphSource()) inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx (ruleSetAt ruleSets coverageIndex) ValueNone positions
        | ClassRules (coverage, classDef, ruleSets, digests) ->
            match Coverage.index coverage infos[idx].Codepoint with
            | CoverageHit.NotCovered -> contextMiss
            | CoverageHit.Covered _ ->
                let classIndex = int (ClassDef.classOf classDef infos[idx].Codepoint)
                let digest = match useDigest && classIndex < ruleSets.Length with | true -> ValueSome digests[classIndex] | false -> ValueNone
                contextRuleSet (ClassSource classDef) inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx (ruleSetAt ruleSets classIndex) digest positions
        | CoverageRule (inputCoverages, records) ->
            match inputCoverages.IsEmpty with
            | true -> contextMiss
            | false -> matchContextCoverage inputCoverages records inputMatcher gdef infos props outInfos outProps outLen idx positions

    /// <summary>Matches the lookahead and then the backtrack of a chained rule whose input matched, each failure owing its
    /// own range.</summary>
    /// <typeparam name="'B">The backtrack data.</typeparam>
    /// <typeparam name="'L">The lookahead data.</typeparam>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="backtrack">The backtrack values.</param>
    /// <param name="backtrackCount">The backtrack count.</param>
    /// <param name="lookahead">The lookahead values.</param>
    /// <param name="lookaheadCount">The lookahead count.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="matchEnd">The end of the input match.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>See <see cref="chainApply"/>.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private chainLookahead<'B, 'L when 'B : struct and 'B :> IGlyphData and 'L : struct and 'L :> IGlyphData>
            (contextMatcher: Matcher) (gdef: Gdef) (backtrack: 'B) (backtrackCount: int) (lookahead: 'L) (lookaheadCount: int)
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) (outLen: int)
            (idx: int) (matchEnd: int) (owed: FlagBounds) =
        match matchLookahead contextMatcher gdef lookahead infos props matchEnd lookaheadCount with
        | struct (false, endIndex) -> struct (false, 0, 0, 0, FlagBounds.addPlain idx endIndex owed)
        | struct (true, endIndex) ->
            match matchBacktrack contextMatcher gdef backtrack outInfos outProps outLen backtrackCount with
            | struct (false, startIndex) -> struct (false, 0, 0, 0, FlagBounds.addOutbuffer startIndex endIndex owed)
            | struct (true, startIndex) -> struct (true, matchEnd, startIndex, endIndex, owed)

    /// <summary>Applies one chained rule's match, <c>chain_context_apply_lookup</c> up to the point where the recursion
    /// starts: the input, then the lookahead, then the backtrack, each failure owing its own range.</summary>
    /// <typeparam name="'B">The backtrack data.</typeparam>
    /// <typeparam name="'I">The input data.</typeparam>
    /// <typeparam name="'L">The lookahead data.</typeparam>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="backtrack">The backtrack values.</param>
    /// <param name="backtrackCount">The backtrack count.</param>
    /// <param name="input">The input values.</param>
    /// <param name="inputCount">The input count, including the first glyph.</param>
    /// <param name="lookahead">The lookahead values.</param>
    /// <param name="lookaheadCount">The lookahead count.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>Whether the rule matched, the end of the input match, the backtrack start, the lookahead end, and the
    /// ranges owed.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private chainApply<'B, 'I, 'L when 'B : struct and 'B :> IGlyphData and 'I : struct and 'I :> IGlyphData and 'L : struct and 'L :> IGlyphData>
            inputMatcher contextMatcher gdef
            (backtrack: 'B) backtrackCount (input: 'I) inputCount (lookahead: 'L) lookaheadCount
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (positions: Span<int>) owed =
        match inputCount > MaxContextLength with
        | true -> struct (false, 0, 0, 0, owed)
        | false ->
            match matchInputPositions inputMatcher gdef input infos props outInfos outProps outLen idx inputCount positions with
            | struct (false, _) -> struct (false, 0, 0, 0, owed)
            | struct (true, matchEnd) -> chainLookahead contextMatcher gdef backtrack backtrackCount lookahead lookaheadCount infos props outInfos outProps outLen idx matchEnd owed

    /// <summary>Applies one chained glyph or class rule.</summary>
    /// <typeparam name="'S">The chain source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The chain source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="rule">The rule.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>See <see cref="chainApply"/>.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private chainRuleApply<'S, 'D when 'S : struct and 'S :> IChainSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef rule
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (positions: Span<int>) owed =
        chainApply inputMatcher contextMatcher gdef
            (source.BacktrackData rule.Backtrack) rule.Backtrack.Length
            (source.InputData rule.ChainInput) rule.ChainInputCount
            (source.LookaheadData rule.Lookahead) rule.Lookahead.Length
            infos props outInfos outProps outLen idx positions owed

    /// <summary>Tries the rules of a chained rule set one after the other, <c>ChainRuleSet::apply</c>'s slow path.</summary>
    /// <typeparam name="'S">The chain source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The chain source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private chainRulesSlow<'S, 'D when 'S : struct and 'S :> IChainSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<ChainRule>) (positions: Span<int>) index owed =
        match index < rules.Length with
        | false -> ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let rule = rules[index]
            match chainRuleApply source inputMatcher contextMatcher gdef rule infos props outInfos outProps outLen idx positions owed with
            | struct (true, matchEnd, startIndex, endIndex, owed) -> ContextOutcome(true, true, rule.ChainInputCount, matchEnd, rule.ChainRecords, startIndex, endIndex, owed, -1)
            | struct (false, _, _, _, owed) -> chainRulesSlow source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules positions (index + 1) owed

    /// <summary>Whether a chained rule survives the skip-ahead of the fast path.</summary>
    /// <param name="rules">The rules.</param>
    /// <param name="index">The failed rule.</param>
    /// <returns>The index of the last rule that shares the failed rule's first input value.</returns>
    [<TailCall>]
    let rec private skipSharedFirstChain (rules: ImmutableArray<ChainRule>) index =
        match index + 1 < rules.Length with
        | false -> index
        | true ->
            let next = rules[index + 1]
            match next.ChainInputCount <= 1 || next.ChainInput[0] <> rules[index].ChainInput[0] with
            | true -> index
            | false -> skipSharedFirstChain rules (index + 1)

    /// <summary>Whether a chained rule's first glyph after the current one matches, in the fast path.</summary>
    /// <param name="heads">The heads, with their values.</param>
    /// <param name="rule">The rule.</param>
    /// <returns>True when it matches.</returns>
    let private chainFirstMatches (heads: Heads) (rule: inref<ChainRule>) =
        match rule.ChainInputCount > 1 with
        | true -> heads.FirstValue = rule.ChainInput[0]
        | false -> rule.Lookahead.Length = 0 || heads.FirstLookahead = rule.Lookahead[0]

    /// <summary>Whether a chained rule's second glyph after the current one matches, in the fast path.</summary>
    /// <param name="heads">The heads, with their values.</param>
    /// <param name="rule">The rule.</param>
    /// <returns>True when it matches.</returns>
    let private chainSecondMatches (heads: Heads) (rule: inref<ChainRule>) =
        let lenP1 = max rule.ChainInputCount 1
        match lenP1 > 2 with
        | true -> heads.SecondValue = rule.ChainInput[1]
        | false -> rule.Lookahead.Length <= 2 - lenP1 || heads.SecondLookahead = rule.Lookahead[2 - lenP1]

    /// <summary>The rule loop of <c>ChainRuleSet::apply</c>'s fast path.</summary>
    /// <typeparam name="'S">The chain source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The chain source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="heads">The first and second glyph after the current one.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="unsafeTo">The end of the range the failed rules owe, or -1.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private chainRulesFast<'S, 'D when 'S : struct and 'S :> IChainSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<ChainRule>) heads (positions: Span<int>) index unsafeTo owed =
        match index < rules.Length with
        | false ->
            let owed = match unsafeTo with | -1 -> owed | bound -> FlagBounds.addPlain idx bound owed
            ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let rule = &rules.ItemRef index
            let firstMatches = chainFirstMatches heads &rule
            let secondMatches = not heads.HasSecond || chainSecondMatches heads &rule
            match struct (firstMatches, secondMatches) with
            | struct (false, _) ->
                let unsafeTo = match unsafeTo with | -1 -> heads.UnsafeTo1 | bound -> bound
                let last = match rule.ChainInputCount > 1 with | true -> skipSharedFirstChain rules index | false -> index
                chainRulesFast source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules heads positions (last + 1) unsafeTo owed
            | struct (true, false) -> chainRulesFast source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules heads positions (index + 1) heads.UnsafeTo2 owed
            | struct (true, true) ->
                match chainRuleApply source inputMatcher contextMatcher gdef rule infos props outInfos outProps outLen idx positions owed with
                | struct (true, matchEnd, startIndex, endIndex, owed) -> ContextOutcome(true, true, rule.ChainInputCount, matchEnd, rule.ChainRecords, startIndex, endIndex, owed, unsafeTo)
                | struct (false, _, _, _, owed) -> chainRulesFast source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules heads positions (index + 1) unsafeTo owed

    /// <summary>Applies the rules of a chained rule set that can match without a glyph after the current one: those with
    /// one input glyph and no lookahead. The rules with more are passed over and owe a range.</summary>
    /// <typeparam name="'S">The chain source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The chain source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="skipped">Whether a rule was passed over.</param>
    /// <param name="bound">The end of the range a passed-over rule owes.</param>
    /// <param name="owed">The ranges owed so far.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private chainRulesAtEnd<'S, 'D when 'S : struct and 'S :> IChainSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<ChainRule>) (positions: Span<int>) index skipped bound owed =
        match index < rules.Length with
        | false ->
            let owed = match skipped with | true -> FlagBounds.addPlain idx bound owed | false -> owed
            ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let rule = rules[index]
            match rule.ChainInputCount <= 1 && rule.Lookahead.Length = 0 with
            | false -> chainRulesAtEnd source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules positions (index + 1) true bound owed
            | true ->
                match chainRuleApply source inputMatcher contextMatcher gdef rule infos props outInfos outProps outLen idx positions owed with
                | struct (true, matchEnd, startIndex, endIndex, owed) -> ContextOutcome(true, true, rule.ChainInputCount, matchEnd, rule.ChainRecords, startIndex, endIndex, owed, (match skipped with | true -> bound | false -> -1))
                | struct (false, _, _, _, owed) -> chainRulesAtEnd source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules positions (index + 1) skipped bound owed

    /// <summary>Applies a chained rule set after its first head was read, <c>ChainRuleSet::apply</c>: the digest of the first input values
    /// refuses the rule set before the second head is read, as upstream does (src/hb-ot-layout-gsubgpos.hh:5002-5007).</summary>
    /// <typeparam name="'S">The chain source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The chain source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="digest">The first-class digest of the rule set, or none.</param>
    /// <param name="positions">The match positions.</param>
    /// <param name="first">The index of the first glyph after the current one.</param>
    /// <returns>The outcome.</returns>
    let private chainAfterFirst<'S, 'D when 'S : struct and 'S :> IChainSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<ChainRule>) (digest: uint64 voption) (positions: Span<int>) first =
        match digest with
        | ValueSome mask when (mask &&& (1UL <<< int (source.InputValueOf infos[first] &&& 63u))) = 0UL ->
            ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, FlagBounds.addPlain idx (first + 1) FlagBounds.none, -1)
        | ValueSome _ | ValueNone ->
            match readSecondHead contextMatcher gdef infos props first with
            | SecondSlow -> chainRulesSlow source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules positions 0 FlagBounds.none
            | SecondRead heads -> chainRulesFast source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules (chainHeadValues source heads) positions 0 -1 FlagBounds.none

    /// <summary>Applies a chained rule set, <c>ChainRuleSet::apply</c>.</summary>
    /// <typeparam name="'S">The chain source.</typeparam>
    /// <typeparam name="'D">The glyph data.</typeparam>
    /// <param name="source">The chain source.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="digest">The first-class digest of the rule set, or none.</param>
    /// <param name="positions">The match positions.</param>
    /// <returns>The outcome.</returns>
    let private chainRuleSet<'S, 'D when 'S : struct and 'S :> IChainSource<'D> and 'D : struct and 'D :> IGlyphData>
            (source: 'S) inputMatcher contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (rules: ImmutableArray<ChainRule>) (digest: uint64 voption) (positions: Span<int>) =
        match rules.Length <= FastPathMinRules with
        | true -> chainRulesSlow source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules positions 0 FlagBounds.none
        | false ->
            match readFirstHead contextMatcher gdef infos props idx with
            | FirstSlow -> chainRulesSlow source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules positions 0 FlagBounds.none
            | FirstMissing bound -> chainRulesAtEnd source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules positions 0 false bound FlagBounds.none
            | FirstRead first -> chainAfterFirst source inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx rules digest positions first

    /// <summary>The rule set a covered glyph selects in a chained subtable, or an empty one for an index past the array.</summary>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="index">The coverage index or class.</param>
    /// <returns>The rule set.</returns>
    let private chainRuleSetAt (ruleSets: ImmutableArray<ImmutableArray<ChainRule>>) index =
        match index < ruleSets.Length with
        | true -> ruleSets[index]
        | false -> ImmutableArray<ChainRule>.Empty

    /// <summary>The outcome of a chained subtable that does not apply.</summary>
    let private chainMiss = ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, FlagBounds.none, -1)

    /// <summary>Matches a chained-context subtable of the third format whose input is not empty.</summary>
    /// <param name="backtrack">The backtrack coverages.</param>
    /// <param name="inputCoverages">The input coverages, the first of which the current glyph must be in.</param>
    /// <param name="lookahead">The lookahead coverages.</param>
    /// <param name="records">The lookup records.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The match positions.</param>
    /// <returns>The outcome.</returns>
    let private matchChainCoverage (backtrack: ImmutableArray<Coverage>) (inputCoverages: ImmutableArray<Coverage>) (lookahead: ImmutableArray<Coverage>) records inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen idx (positions: Span<int>) =
        match Coverage.index inputCoverages[0] infos[idx].Codepoint with
        | CoverageHit.NotCovered -> chainMiss
        | CoverageHit.Covered _ ->
            match chainApply inputMatcher contextMatcher gdef (CoverageList backtrack) backtrack.Length (CoverageRest inputCoverages) inputCoverages.Length (CoverageList lookahead) lookahead.Length infos props outInfos outProps outLen idx positions FlagBounds.none with
            | struct (true, matchEnd, startIndex, endIndex, owed) -> ContextOutcome(true, true, inputCoverages.Length, matchEnd, records, startIndex, endIndex, owed, -1)
            | struct (false, _, _, _, owed) -> ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)

    /// <summary>Matches a chained-context subtable, formats 1-5, <c>ChainContext::apply</c>.</summary>
    /// <param name="rules">The chain rules.</param>
    /// <param name="useDigest">Whether the subtable is among the first eight of its lookup.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher, for backtrack and lookahead.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The buffer the match positions are written into.</param>
    /// <returns>The outcome.</returns>
    let matchChain (rules: ChainRules) useDigest gdef inputMatcher contextMatcher
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>)
            (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen
            idx (positions: Span<int>) =
        match rules with
        | ChainGlyphRules (coverage, ruleSets) ->
            match Coverage.index coverage infos[idx].Codepoint with
            | CoverageHit.NotCovered -> chainMiss
            | CoverageHit.Covered coverageIndex ->
                chainRuleSet (ChainGlyphSource()) inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx (chainRuleSetAt ruleSets coverageIndex) ValueNone positions
        | ChainClassRules (coverage, backtrackClassDef, inputClassDef, lookaheadClassDef, ruleSets, digests) ->
            match Coverage.index coverage infos[idx].Codepoint with
            | CoverageHit.NotCovered -> chainMiss
            | CoverageHit.Covered _ ->
                let classIndex = int (ClassDef.classOf inputClassDef infos[idx].Codepoint)
                let digest = match useDigest && classIndex < ruleSets.Length with | true -> ValueSome digests[classIndex] | false -> ValueNone
                chainRuleSet (ChainClassSource (backtrackClassDef, inputClassDef, lookaheadClassDef)) inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx (chainRuleSetAt ruleSets classIndex) digest positions
        | ChainCoverageRule (backtrack, inputCoverages, lookahead, records) ->
            match inputCoverages.IsEmpty with
            | true -> chainMiss
            | false -> matchChainCoverage backtrack inputCoverages lookahead records inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx positions

    /// <summary>Matches the lookahead of a reverse-chaining subtable whose backtrack matched.</summary>
    /// <param name="lookahead">The lookahead coverages.</param>
    /// <param name="substitute">The substitute glyph.</param>
    /// <param name="startIndex">The backtrack start.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The reverse outcome.</returns>
    let private reverseLookahead (lookahead: ImmutableArray<Coverage>) substitute startIndex contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index =
        match matchLookahead contextMatcher gdef (CoverageList lookahead) infos props (index + 1) lookahead.Length with
        | struct (true, endIndex) -> ReverseSubstitute (substitute, startIndex, endIndex, FlagBounds.none)
        | struct (false, endIndex) -> ReverseMiss (FlagBounds.addOutbuffer startIndex endIndex FlagBounds.none)

    /// <summary>Matches a reverse-chaining single subtable, <c>ReverseChainSingleSubstFormat1_2::apply</c>
    /// (src/OT/Layout/GSUB/ReverseChainSingleSubstFormat1.hh:184-236). The lookahead is matched only after the backtrack
    /// matched (the <c>&amp;&amp;</c> at lines 199-206), so a failed backtrack owes the range (start, 0) the call at line 233 makes
    /// with the end index initialised at line 198, which <c>_set_glyph_flags</c> drops.</summary>
    /// <param name="coverage">The input coverage.</param>
    /// <param name="backtrack">The backtrack coverages.</param>
    /// <param name="lookahead">The lookahead coverages.</param>
    /// <param name="substitutes">The substitutes, by coverage index.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The reverse outcome.</returns>
    let matchReverse
            (coverage: Coverage) (backtrack: ImmutableArray<Coverage>) (lookahead: ImmutableArray<Coverage>)
            (substitutes: ImmutableArray<uint32>) contextMatcher gdef
            (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index =
        match Coverage.index coverage infos[index].Codepoint with
        | CoverageHit.NotCovered -> ReverseMiss FlagBounds.none
        | CoverageHit.Covered coverageIndex ->
            match coverageIndex < substitutes.Length with
            | false -> ReverseMiss FlagBounds.none
            | true ->
                match matchBacktrack contextMatcher gdef (CoverageList backtrack) infos props index backtrack.Length with
                | struct (false, startIndex) -> ReverseMiss (FlagBounds.addOutbuffer startIndex 0 FlagBounds.none)
                | struct (true, startIndex) -> reverseLookahead lookahead substitutes[coverageIndex] startIndex contextMatcher gdef infos props index

    /// <summary>Matches dense glyph sets against the glyphs of a rule, <c>match_coverage</c> over a list of coverages.</summary>
    [<Struct>]
    type private DenseList =
        val Sets: ImmutableArray<ImmutableArray<uint64>>
        new (sets) = { Sets = sets }
        interface IGlyphData with
            member _.HasMatchFunc = true
            member this.Matches (info, _, position) = ChainSets.contains this.Sets[position] info.Codepoint

    /// <summary>Matches dense glyph sets against the glyphs of a rule after its first, <c>match_coverage</c> over the coverages of the
    /// input after the first.</summary>
    [<Struct>]
    type private DenseRest =
        val Sets: ImmutableArray<ImmutableArray<uint64>>
        new (sets) = { Sets = sets }
        interface IGlyphData with
            member _.HasMatchFunc = true
            member this.Matches (info, _, position) = ChainSets.contains this.Sets[position + 1] info.Codepoint

    /// <summary>Matches a chained-context subtable of the third format through its dense sets, <see cref="matchChain"/> with a bit test
    /// where it searches a coverage: the outcome is the same in every field.</summary>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="records">The lookup records of the subtable.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The match positions.</param>
    /// <returns>The outcome.</returns>
    let matchChainDense (sets: ChainSets) records inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen idx (positions: Span<int>) =
        match ChainSets.contains sets.FirstSet infos[idx].Codepoint with
        | false -> chainMiss
        | true ->
            match chainApply inputMatcher contextMatcher gdef (DenseList sets.BacktrackSets) sets.BacktrackSets.Length (DenseRest sets.InputSets) sets.InputSets.Length (DenseList sets.LookaheadSets) sets.LookaheadSets.Length infos props outInfos outProps outLen idx positions FlagBounds.none with
            | struct (true, matchEnd, startIndex, endIndex, owed) -> ContextOutcome(true, true, sets.InputSets.Length, matchEnd, records, startIndex, endIndex, owed, -1)
            | struct (false, _, _, _, owed) -> ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)

    /// <summary>Whether the first step of a walk found no glyph.</summary>
    /// <param name="probe">The step.</param>
    /// <returns>True for a miss.</returns>
    let private walkMissed (probe: Probe) =
        match probe with
        | Probe.Miss _ -> true
        | Probe.Hit _ -> false

    /// <summary>The first steps of the three walks a chained-context match starts with at one glyph, taken once for every subtable of a
    /// lookup: the input after the first glyph, the backtrack and the lookahead.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type VisitHeads =
        { InputHead: WalkHead
          BacktrackHead: WalkHead
          LookaheadHead: WalkHead }

    /// <summary>The heads of a visit that make every walk fall back to its own steps: no walk is shared.</summary>
    let unsharedHeads =
        { InputHead = HeadMaybe
          BacktrackHead = HeadMaybe
          LookaheadHead = HeadMaybe }

    /// <summary>Takes the first steps of the walks of a visit at a glyph.</summary>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <returns>The heads.</returns>
    let visitHeads inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx =
        { InputHead = Matching.headNext inputMatcher gdef infos props idx infos.Length
          BacktrackHead = Matching.headPrev contextMatcher gdef outInfos outProps outInfos.Length
          LookaheadHead = Matching.headNext contextMatcher gdef infos props idx infos.Length }

    /// <summary>Whether the first step of the input after the first glyph finds no glyph in the second input set, from the head.</summary>
    /// <param name="head">The head of the walk.</param>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <returns>True for a miss.</returns>
    let private inputMissed (head: WalkHead) (sets: ChainSets) inputMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) idx =
        match head with
        | HeadEnd -> true
        | HeadGlyph found -> not (Matching.maskAdmits inputMatcher infos[found] props[found] && ChainSets.contains sets.InputSets[1] infos[found].Codepoint)
        | HeadMaybe -> walkMissed (Matching.next inputMatcher gdef (DenseRest sets.InputSets) 0 infos props idx infos.Length)

    /// <summary>Whether the first step of the backtrack finds no glyph in the first backtrack set, from the head.</summary>
    /// <param name="head">The head of the walk.</param>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <returns>True for a miss.</returns>
    let private backtrackMissed (head: WalkHead) (sets: ChainSets) contextMatcher gdef (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) =
        match head with
        | HeadEnd -> true
        | HeadGlyph found -> not (Matching.maskAdmits contextMatcher outInfos[found] outProps[found] && ChainSets.contains sets.BacktrackSets[0] outInfos[found].Codepoint)
        | HeadMaybe -> walkMissed (Matching.prev contextMatcher gdef (DenseList sets.BacktrackSets) 0 outInfos outProps outInfos.Length)

    /// <summary>Whether the first step of the lookahead finds no glyph in the first lookahead set, from the head.</summary>
    /// <param name="head">The head of the walk.</param>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <returns>True for a miss.</returns>
    let private lookaheadMissed (head: WalkHead) (sets: ChainSets) contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) idx =
        match head with
        | HeadEnd -> true
        | HeadGlyph found -> not (Matching.maskAdmits contextMatcher infos[found] props[found] && ChainSets.contains sets.LookaheadSets[0] infos[found].Codepoint)
        | HeadMaybe -> walkMissed (Matching.next contextMatcher gdef (DenseList sets.LookaheadSets) 0 infos props idx infos.Length)

    /// <summary><see cref="chainDenseCannotMatch"/> with the first steps of the three walks taken once for the lookup's visit: the decision
    /// is the same for every glyph and subtable, and a bit test replaces each walk where the walk stopped at a glyph the lookup
    /// does not skip whatever the match function says.</summary>
    /// <param name="heads">The first steps of the walks at this glyph.</param>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <returns>True when no rule of the subtable can match.</returns>
    let chainDenseCannotMatchWith (heads: VisitHeads) (sets: ChainSets) inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx =
        not (ChainSets.contains sets.FirstSet infos[idx].Codepoint)
        || (sets.InputSets.Length > 1 && inputMissed heads.InputHead sets inputMatcher gdef infos props idx)
        || (sets.BacktrackSets.Length > 0 && backtrackMissed heads.BacktrackHead sets contextMatcher gdef outInfos outProps)
        || (sets.InputSets.Length = 1 && sets.LookaheadSets.Length > 0 && lookaheadMissed heads.LookaheadHead sets contextMatcher gdef infos props idx)

    /// <summary>How a walk that collects the glyphs a lookup does not skip ended.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type ShareStop =
        | ShareEnough
        | ShareEnd

    /// <summary>The glyphs a lookup does not skip, taken once for every subtable of the lookup at a glyph: those before the output end
    /// (for the backtracks) and those after the glyph (for the inputs), each in the positions buffer from its offset on, as the glyph's
    /// index twice with the low bit set for a glyph whose skipping depends on the match function; how many of each, and why each walk
    /// stopped.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type VisitShare =
        { BackFound: int
          BackStop: ShareStop
          ForwardFound: int
          ForwardStop: ShareStop }

    /// <summary>The first slot of the shared backward walk in a positions buffer, after the slots of the input match.</summary>
    [<Literal>]
    let ShareOffset = 64

    /// <summary>The first slot of the shared forward walk in a positions buffer.</summary>
    [<Literal>]
    let ShareForwardOffset = 96

    /// <summary>The most glyphs a shared walk collects, a longer one being matched by the subtable's own walk.</summary>
    [<Literal>]
    let ShareDepthMax = 32

    /// <summary>The length a positions buffer needs for the shared walks.</summary>
    [<Literal>]
    let ShareBufferLength = 128

    /// <summary>The glyphs a walk collects beyond the longest context of the lookup, for the glyphs that depend on the match function.</summary>
    [<Literal>]
    let ShareMargin = 4

    /// <summary>How many glyphs of each direction the subtables of a lookup share: none for a direction that fewer than two subtables
    /// walk.</summary>
    [<Struct>]
    type ShareDepths =
        { BackDepth: int
          ForwardDepth: int }

    /// <summary>The visit share of a visit that has none: every walk is the subtable's own.</summary>
    let noShare =
        { BackFound = 0
          BackStop = ShareEnough
          ForwardFound = 0
          ForwardStop = ShareEnough }

    /// <summary>Collects the glyphs the lookup does not skip in one direction from a cursor, <c>skipping_iterator_t::next</c> or
    /// <c>prev</c> without a match function: the glyphs the matcher skips whatever the match function says are passed, and each other
    /// glyph is noted, with a mark when its skipping depends on the match function.</summary>
    /// <param name="matcher">The matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="step">One going forward, minus one going backward.</param>
    /// <param name="finish">The exclusive end going forward; unused going backward.</param>
    /// <param name="slots">The number of glyphs wanted.</param>
    /// <param name="positions">The positions buffer.</param>
    /// <param name="offset">The first slot of the walk.</param>
    /// <param name="found">The number of glyphs found so far.</param>
    /// <param name="index">The cursor index.</param>
    /// <returns>The number of glyphs found and why the walk stopped.</returns>
    [<TailCall>]
    let rec private shareLoop matcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) step finish slots (positions: Span<int>) offset found index =
        match found < slots with
        | false -> struct (found, ShareEnough)
        | true ->
            let candidate = index + step
            match candidate >= 0 && candidate < finish with
            | false -> struct (found, ShareEnd)
            | true ->
                match Matching.maySkip matcher gdef infos[candidate] props[candidate] with
                | SkipYes -> shareLoop matcher gdef infos props step finish slots positions offset found candidate
                | SkipNo ->
                    Sealed.setMatchPosition positions (offset + found) (candidate <<< 1)
                    shareLoop matcher gdef infos props step finish slots positions offset (found + 1) candidate
                | SkipMaybe ->
                    Sealed.setMatchPosition positions (offset + found) ((candidate <<< 1) ||| 1)
                    shareLoop matcher gdef infos props step finish slots positions offset (found + 1) candidate

    /// <summary>Takes the share of a visit: the glyphs the backtracks and the inputs of every subtable of the lookup are matched against.</summary>
    /// <param name="depths">The depths the lookup shares.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The positions buffer.</param>
    /// <returns>The share, none when the buffer has no room for it.</returns>
    let shareVisit (depths: ShareDepths) inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx (positions: Span<int>) =
        match (depths.BackDepth > 0 || depths.ForwardDepth > 0) && positions.Length >= ShareBufferLength with
        | false -> noShare
        | true ->
            let struct (backFound, backStop) =
                match depths.BackDepth > 0 with
                | true -> shareLoop contextMatcher gdef outInfos outProps -1 outInfos.Length depths.BackDepth positions ShareOffset 0 outInfos.Length
                | false -> struct (0, ShareEnough)
            let struct (forwardFound, forwardStop) =
                match depths.ForwardDepth > 0 with
                | true -> shareLoop inputMatcher gdef infos props 1 infos.Length depths.ForwardDepth positions ShareForwardOffset 0 idx
                | false -> struct (0, ShareEnough)
            { BackFound = backFound
              BackStop = backStop
              ForwardFound = forwardFound
              ForwardStop = forwardStop }

    /// <summary>The depths a lookup shares among its dense chained-context subtables: the longest backtrack, and the longest input after its
    /// first glyph, each with a margin and at most the cap, when at least two subtables have one.</summary>
    /// <param name="chains">The dense sets of the subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="backCount">The subtables with a backtrack so far.</param>
    /// <param name="backLongest">The longest backtrack so far.</param>
    /// <param name="forwardCount">The subtables with an input after the first glyph so far.</param>
    /// <param name="forwardLongest">The longest such input so far.</param>
    /// <returns>The depths.</returns>
    [<TailCall>]
    let rec shareDepthsOf (chains: ImmutableArray<ChainSets voption>) (index: int) (backCount: int) (backLongest: int) (forwardCount: int) (forwardLongest: int) =
        match index < chains.Length with
        | false ->
            { BackDepth = (match backCount >= 2 with | true -> min (backLongest + ShareMargin) ShareDepthMax | false -> 0)
              ForwardDepth = (match forwardCount >= 2 with | true -> min (forwardLongest + ShareMargin) ShareDepthMax | false -> 0) }
        | true ->
            match chains[index] with
            | ValueNone -> shareDepthsOf chains (index + 1) backCount backLongest forwardCount forwardLongest
            | ValueSome sets ->
                let back = sets.BacktrackSets.Length
                let forward = sets.InputSets.Length - 1
                shareDepthsOf chains (index + 1) (backCount + (match back > 0 with | true -> 1 | false -> 0)) (max backLongest back) (forwardCount + (match forward > 0 with | true -> 1 | false -> 0)) (max forwardLongest forward)

    /// <summary>Matches the backtrack of a dense subtable against the glyphs of a share, step for step as <c>match_backtrack</c> does: a
    /// glyph the lookup does not skip is admitted by the matcher and a member of its set, or the walk misses; a glyph whose skipping
    /// depends on the match function is matched when it is admitted and a member, and passed otherwise.</summary>
    /// <param name="sets">The dense backtrack sets.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="positions">The positions buffer holding the share.</param>
    /// <param name="share">The share.</param>
    /// <param name="position">The number of backtrack values matched so far.</param>
    /// <param name="slot">The next glyph of the share.</param>
    /// <param name="hit">The last glyph matched, the output length before the first.</param>
    /// <returns>Zero and the unsafe bound for a miss, one and the match start for a match, two when the share cannot tell.</returns>
    [<TailCall>]
    let rec private backtrackSharedLoop (sets: ImmutableArray<ImmutableArray<uint64>>) contextMatcher (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) (positions: Span<int>) (share: VisitShare) position slot hit =
        match position < sets.Length with
        | false -> struct (1, hit)
        | true ->
            match slot < share.BackFound with
            | false ->
                match share.BackStop with
                | ShareEnd -> struct (0, 0)
                | ShareEnough -> struct (2, 0)
            | true ->
                let entry = positions[ShareOffset + slot]
                let found = entry >>> 1
                let admitted = Matching.maskAdmits contextMatcher outInfos[found] outProps[found] && ChainSets.contains sets[position] outInfos[found].Codepoint
                match admitted, entry &&& 1 with
                | true, _ -> backtrackSharedLoop sets contextMatcher outInfos outProps positions share (position + 1) (slot + 1) found
                | false, 0 -> struct (0, max 1 found - 1)
                | false, _ -> backtrackSharedLoop sets contextMatcher outInfos outProps positions share position (slot + 1) hit

    /// <summary>Whether the input of a dense subtable after its first glyph misses against the glyphs of a share: a miss of the walk
    /// of its sets, which the match of the input would make too, whatever else it asks of the glyphs. Whether a walk that the share
    /// cannot follow to the end misses is not told.</summary>
    /// <param name="sets">The dense input sets, the first being that of the first glyph.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The positions buffer holding the share.</param>
    /// <param name="share">The share.</param>
    /// <param name="position">The input glyph to match, from one.</param>
    /// <param name="slot">The next glyph of the share.</param>
    /// <returns>True when the sets are known not to be matched.</returns>
    [<TailCall>]
    let rec private inputSharedMissed (sets: ImmutableArray<ImmutableArray<uint64>>) inputMatcher (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: Span<int>) (share: VisitShare) position slot =
        match position < sets.Length with
        | false -> false
        | true ->
            match slot < share.ForwardFound with
            | false ->
                match share.ForwardStop with
                | ShareEnd -> true
                | ShareEnough -> false
            | true ->
                let entry = positions[ShareForwardOffset + slot]
                let found = entry >>> 1
                let admitted = Matching.maskAdmits inputMatcher infos[found] props[found] && ChainSets.contains sets[position] infos[found].Codepoint
                match admitted, entry &&& 1 with
                | true, _ -> inputSharedMissed sets inputMatcher infos props positions share (position + 1) (slot + 1)
                | false, 0 -> true
                | false, _ -> inputSharedMissed sets inputMatcher infos props positions share position (slot + 1)

    /// <summary>Matches the backtrack of a dense subtable, <c>match_backtrack</c>, through the share of the visit where it can tell and by
    /// the subtable's own walk where it cannot: the answer is the same.</summary>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="positions">The positions buffer holding the share.</param>
    /// <param name="share">The share.</param>
    /// <returns>The matched flag and the match start.</returns>
    let private backtrackDense (sets: ChainSets) contextMatcher gdef (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen (positions: Span<int>) (share: VisitShare) =
        match sets.BacktrackSets.Length with
        | 0 -> struct (true, outLen)
        | _ ->
            match backtrackSharedLoop sets.BacktrackSets contextMatcher outInfos outProps positions share 0 0 outLen with
            | struct (1, start) -> struct (true, start)
            | struct (0, bound) -> struct (false, bound)
            | struct (_, _) -> matchBacktrack contextMatcher gdef (DenseList sets.BacktrackSets) outInfos outProps outLen sets.BacktrackSets.Length

    /// <summary>Matches the lookahead and then the backtrack of a dense chained rule whose input matched, as <see cref="chainLookahead"/>
    /// does, with the backtrack matched through the share of the visit.</summary>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="matchEnd">The end of the input match.</param>
    /// <param name="positions">The positions buffer holding the share.</param>
    /// <param name="share">The share.</param>
    /// <returns>See <see cref="chainApply"/>.</returns>
    let private chainLookaheadShared (sets: ChainSets) contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen idx matchEnd (positions: Span<int>) (share: VisitShare) =
        match matchLookahead contextMatcher gdef (DenseList sets.LookaheadSets) infos props matchEnd sets.LookaheadSets.Length with
        | struct (false, endIndex) -> struct (false, 0, 0, 0, FlagBounds.addPlain idx endIndex FlagBounds.none)
        | struct (true, endIndex) ->
            match backtrackDense sets contextMatcher gdef outInfos outProps outLen positions share with
            | struct (false, startIndex) -> struct (false, 0, 0, 0, FlagBounds.addOutbuffer startIndex endIndex FlagBounds.none)
            | struct (true, startIndex) -> struct (true, matchEnd, startIndex, endIndex, FlagBounds.none)

    /// <summary>Applies one dense chained rule's match as <see cref="chainApply"/> does, with the backtrack matched through the share of the visit.</summary>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The match positions, and the share after them.</param>
    /// <param name="share">The share.</param>
    /// <returns>See <see cref="chainApply"/>.</returns>
    let private chainApplyShared (sets: ChainSets) inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen idx (positions: Span<int>) (share: VisitShare) =
        match sets.InputSets.Length > MaxContextLength with
        | true -> struct (false, 0, 0, 0, FlagBounds.none)
        | false ->
            match matchInputPositions inputMatcher gdef (DenseRest sets.InputSets) infos props outInfos outProps outLen idx sets.InputSets.Length positions with
            | struct (false, _) -> struct (false, 0, 0, 0, FlagBounds.none)
            | struct (true, matchEnd) -> chainLookaheadShared sets contextMatcher gdef infos props outInfos outProps outLen idx matchEnd positions share

    /// <summary>Matches a chained-context subtable of the third format through its dense sets, <see cref="matchChainDense"/> with the
    /// backtrack matched through the share of the visit: the outcome is the same in every field.</summary>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="records">The lookup records of the subtable.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="outLen">The output length.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The match positions, and the share after them.</param>
    /// <param name="share">The share.</param>
    /// <returns>The outcome.</returns>
    let matchChainDenseShared (sets: ChainSets) records inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) outLen idx (positions: Span<int>) (share: VisitShare) =
        match ChainSets.contains sets.FirstSet infos[idx].Codepoint with
        | false -> chainMiss
        | true ->
            match chainApplyShared sets inputMatcher contextMatcher gdef infos props outInfos outProps outLen idx positions share with
            | struct (true, matchEnd, startIndex, endIndex, owed) -> ContextOutcome(true, true, sets.InputSets.Length, matchEnd, records, startIndex, endIndex, owed, -1)
            | struct (false, _, _, _, owed) -> ContextOutcome(false, true, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)

    /// <summary><see cref="chainDenseCannotMatchWith"/> with the whole input after the first glyph followed through the share of the visit: the
    /// subtable cannot match when its input sets miss against the glyphs the lookup does not skip, as the match of its input would.</summary>
    /// <param name="heads">The first steps of the walks at this glyph.</param>
    /// <param name="share">The share of the visit.</param>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <param name="positions">The positions buffer holding the share.</param>
    /// <returns>True when no rule of the subtable can match.</returns>
    let chainDenseCannotMatchShared (heads: VisitHeads) (share: VisitShare) (sets: ChainSets) inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx (positions: Span<int>) =
        chainDenseCannotMatchWith heads sets inputMatcher contextMatcher gdef infos props outInfos outProps idx
        || (sets.InputSets.Length > 1 && inputSharedMissed sets.InputSets inputMatcher infos props positions share 1 0)

    /// <summary>Whether a chained-context subtable of the third format, given as dense sets, cannot match at a glyph. Every part of the
    /// match is a conjunction of steps of the skipping iterator, so the match fails when the first glyph is not in the first input set or when
    /// the first step of the input after it, of the backtrack, or of the lookahead of a one-glyph input finds no glyph in its set: these are the
    /// very steps <see cref="matchChainDense"/> takes first, and each is independent of the others, so the order of the tests does not change the
    /// outcome. A caller that needs the unsafe-to-concat ranges a failed match owes does not ask: only the decision is given.</summary>
    /// <param name="sets">The dense sets of the subtable.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The first glyph's index.</param>
    /// <returns>True when no rule of the subtable can match.</returns>
    let chainDenseCannotMatch (sets: ChainSets) inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx =
        not (ChainSets.contains sets.FirstSet infos[idx].Codepoint)
        || (sets.InputSets.Length > 1 && walkMissed (Matching.next inputMatcher gdef (DenseRest sets.InputSets) 0 infos props idx infos.Length))
        || (sets.BacktrackSets.Length > 0 && walkMissed (Matching.prev contextMatcher gdef (DenseList sets.BacktrackSets) 0 outInfos outProps outInfos.Length))
        || (sets.InputSets.Length = 1 && sets.LookaheadSets.Length > 0 && walkMissed (Matching.next contextMatcher gdef (DenseList sets.LookaheadSets) 0 infos props idx infos.Length))
