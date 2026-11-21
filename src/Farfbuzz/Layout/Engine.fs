namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

/// <summary>The boxed state of one in-progress multiple substitution, kept off the hot single-substitution path.</summary>
[<NoEquality; NoComparison>]
type private GsubMultiplePayload =
    { MultipleInfo: GlyphInfo
      MultipleComponents: ImmutableArray<uint32>
      MultipleKlass: uint16
      MultipleBaseProps: GlyphProps
      MultipleLigId: byte
      MultipleIndex: int }

/// <summary>The boxed data of one ligature-set application: the set being walked, its second-glyph digest and the subtable it
/// belongs to, which the next subtable follows when no ligature of the set applies.</summary>
[<NoEquality; NoComparison>]
type private GsubLigatureData =
    { Ligatures: ImmutableArray<Ligature>
      SecondDigest: Digest
      Subtable: int }

/// <summary>The state of one GSUB lookup application, the forward skip-and-apply machine.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type private GsubState =
    | GsubScan
    | GsubOpen of skip: int
    | GsubScanTry
    | GsubTry of subtable: int
    | GsubSkip
    | GsubMultiple of multiple: GsubMultiplePayload
    | GsubAltRandom of info: GlyphInfo * props: GlyphProps * randomState: uint32
    | GsubAltOutput of info: GlyphInfo * props: GlyphProps
    | GsubLigature of ligatureSet: GsubLigatureData * mode: int * index: int * unsafeTo: int * unsafeToConcatFlag: bool * second: uint32
    | GsubLigate of ligate: GsubLigateData * phase: int * componentIndex: int * matchStart: int * target: int * lastLigId: byte * lastNumComponents: int * componentsSoFar: int * scan: int
    | GsubLigatureDone of subtable: int
    | GsubConcatAfter of concatTo: int
    | GsubDone

/// <summary>The lookup application loop, <c>hb_ot_map_t::apply</c> and <c>apply_string</c>.</summary>
[<RequireQualifiedAccess>]
module internal Engine =
    /// <summary>Finds the next glyph a lookup may match, <c>apply_forward</c>'s skip loop.</summary>
    /// <param name="digest">The lookup digest.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="lookupProps">The lookup props.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first candidate index.</param>
    /// <returns>The first matching index, or the run length.</returns>
    [<TailCall; MethodImpl(MethodImplOptions.NoInlining)>]
    let rec private scanForward
            digest
            lookupMask
            lookupProps
            gdef
            (infos: ReadOnlySpan<GlyphInfo>)
            (props: ReadOnlySpan<GlyphProps>)
            index =
        match index < infos.Length with
        | false -> index
        | true ->
            match Digest.mayHave infos[index].Codepoint digest
                  && infos[index].Mask &&& lookupMask <> 0u
                  && Matching.checkGlyphProperty gdef infos[index].Codepoint props[index].GlyphClass lookupProps with
            | true -> index
            | false -> scanForward digest lookupMask lookupProps gdef infos props (index + 1)

    /// <summary>Finds the next glyph a lookup may match, <c>apply_forward</c>'s skip loop, with the exact set of glyphs the
    /// lookup's subtables cover instead of the digest: a glyph outside the set can start no match, so the walk stops at none
    /// of the glyphs a digest lets through by chance.</summary>
    /// <param name="covered">The bit set of the covered glyphs.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="lookupProps">The lookup props.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first candidate index.</param>
    /// <returns>The first matching index, or the run length.</returns>
    [<TailCall; MethodImpl(MethodImplOptions.NoInlining)>]
    let rec private scanCovered
            (covered: ImmutableArray<uint64>)
            lookupMask
            lookupProps
            gdef
            (infos: ReadOnlySpan<GlyphInfo>)
            (props: ReadOnlySpan<GlyphProps>)
            index =
        match index < infos.Length with
        | false -> index
        | true ->
            let glyph = infos[index].Codepoint
            let word = int (glyph >>> 6)
            match word < covered.Length
                  && (covered[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL
                  && infos[index].Mask &&& lookupMask <> 0u
                  && Matching.checkGlyphProperty gdef glyph props[index].GlyphClass lookupProps with
            | true -> index
            | false -> scanCovered covered lookupMask lookupProps gdef infos props (index + 1)

    /// <summary>Builds the properties of one multiple-substitution component,
    /// <c>output_glyph_for_component</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="klass">The guessed class when GDEF classes are absent.</param>
    /// <param name="baseProps">The original glyph properties.</param>
    /// <param name="ligId">The original ligature id.</param>
    /// <param name="glyph">The component glyph.</param>
    /// <param name="index">The component index.</param>
    /// <returns>The component properties.</returns>
    let private multipleComponentProps gdef hasGlyphClasses klass baseProps ligId glyph index =
        let props =
            match ligId with
            | 0uy -> GlyphPropsBits.setLigPropsForComponent (byte index) baseProps
            | _ -> baseProps
        Matching.setGlyphClass gdef hasGlyphClasses glyph klass false true props

    /// <summary>Builds the edit that starts a matched ligature, the shared tail of the ligature-set walk.</summary>
    /// <param name="idx">The current glyph index.</param>
    /// <param name="ligateData">The ligature data.</param>
    /// <param name="lastLigId">The first component's ligature id.</param>
    /// <param name="lastNumComponents">The first component's component count.</param>
    /// <param name="componentsSoFar">The component count accumulated so far.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligatureSetLigate idx (ligateData: GsubLigateData) lastLigId lastNumComponents componentsSoFar =
        struct (RunEdit.MergeClusters (idx, ligateData.MatchEnd), GsubLigate (ligateData, 1, 1, idx, -1, lastLigId, lastNumComponents, componentsSoFar, 0))

    /// <summary>The state a ligation ends in: the unsafe-to-concat range the fast path owes, marked from the glyph the
    /// ligation ended on as <c>LigatureSet::apply</c> does after <c>lig.apply</c> (src/OT/Layout/GSUB/LigatureSet.hh:151-155),
    /// or the next scan.</summary>
    /// <param name="data">The ligature data.</param>
    /// <returns>The next state.</returns>
    let private ligateDone (data: GsubLigateData) =
        match data.ConcatAfter with
        | -1 -> GsubScanTry
        | concatTo -> GsubConcatAfter concatTo

    /// <summary>The next action of the ligature-set walk: continue to the next ligature, or emit an edit.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LigatureStep =
        | LigatureContinue of index: int * unsafeToConcatFlag: bool
        | LigatureEmit of edit: RunEdit * state: GsubState

    /// <summary>Handles one ligature of the set walk, <c>LigatureSet::apply</c>'s per-ligature step.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The current glyph index.</param>
    /// <param name="serial">The buffer serial.</param>
    /// <param name="data">The ligature set.</param>
    /// <param name="mode">The walk phase.</param>
    /// <param name="fast">Whether the fast path pre-check applies.</param>
    /// <param name="second">The fast path's second glyph.</param>
    /// <param name="index">The next ligature.</param>
    /// <param name="unsafeTo">The fast path's unsafe-to bound.</param>
    /// <param name="unsafeToConcatFlag">The fast path's pending unsafe-to-concat.</param>
    /// <returns>The next action.</returns>
    let private ligatureSetOne gdef hasGlyphClasses inputMatcher (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx serial (data: GsubLigatureData) mode fast second index unsafeTo unsafeToConcatFlag =
        let lig = data.Ligatures[index]
        match (not fast) || GsubTables.ligCount lig <= 1 || lig.Components[0] = second with
        | false -> LigatureContinue (index + 1, true)
        | true ->
            match GsubApply.ligatureApply gdef hasGlyphClasses inputMatcher lig infos props outInfos outProps idx serial with
            | LigatureMiss -> LigatureContinue (index + 1, unsafeToConcatFlag)
            | LigatureReplace (info, props) -> LigatureEmit (RunEdit.ReplaceGlyphProps (info.Codepoint, props), GsubScanTry)
            | LigatureUnsafeToConcat concatTo -> LigatureEmit (RunEdit.UnsafeToConcat (idx, concatTo), GsubLigature (data, mode, index + 1, unsafeTo, unsafeToConcatFlag, second))
            | LigatureLigate (facts, lastLigId, lastNumComponents, componentsSoFar) ->
                let ligateData =
                    { GsubLigateData.LigGlyph = facts.LigGlyph
                      FinalProps = facts.FinalProps
                      Components = facts.Components
                      LigId = facts.LigId
                      IsLigature = facts.IsLigature
                      IsMarkLigature = facts.IsMarkLigature
                      MatchEnd = facts.MatchEnd
                      Count = facts.Count
                      NextSerial = facts.NextSerial
                      ConcatAfter = (match fast && unsafeToConcatFlag with | true -> unsafeTo | false -> -1) }
                let struct (edit, state) = ligatureSetLigate idx ligateData lastLigId lastNumComponents componentsSoFar
                LigatureEmit (edit, state)

    /// <summary>Iterates a ligature set in one tight loop, trying each candidate ligature,
    /// <c>LigatureSet::apply</c>'s slow and fast loops.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="idx">The current glyph index.</param>
    /// <param name="serial">The buffer serial.</param>
    /// <param name="data">The ligature set.</param>
    /// <param name="mode">The walk phase.</param>
    /// <param name="fast">Whether the fast path pre-check applies.</param>
    /// <param name="second">The fast path's second glyph.</param>
    /// <param name="index">The next ligature.</param>
    /// <param name="unsafeTo">The fast path's unsafe-to bound.</param>
    /// <param name="unsafeToConcatFlag">The fast path's pending unsafe-to-concat.</param>
    /// <returns>The edit and the next state.</returns>
    [<TailCall>]
    let rec private ligatureSetIterate gdef hasGlyphClasses inputMatcher (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) idx serial (data: GsubLigatureData) mode fast second index unsafeTo unsafeToConcatFlag =
        match index < data.Ligatures.Length with
        | false ->
            match fast && unsafeToConcatFlag with
            | true -> struct (RunEdit.UnsafeToConcat (idx, unsafeTo), GsubLigatureDone data.Subtable)
            | false -> struct (RunEdit.NextGlyphs 0, GsubTry (data.Subtable + 1))
        | true ->
            match ligatureSetOne gdef hasGlyphClasses inputMatcher infos props outInfos outProps idx serial data mode fast second index unsafeTo unsafeToConcatFlag with
            | LigatureContinue (nextIndex, nextFlag) -> ligatureSetIterate gdef hasGlyphClasses inputMatcher infos props outInfos outProps idx serial data mode fast second nextIndex unsafeTo nextFlag
            | LigatureEmit (edit, state) -> struct (edit, state)

    /// <summary>Checks the probed second glyph against the subtable's second digest.</summary>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature set.</param>
    /// <param name="hitIndex">The probed glyph's index.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligatureSetProbeSecond (cursor: inref<RunCursor>) (data: GsubLigatureData) hitIndex =
        let secondGlyph = cursor.Input[hitIndex].Codepoint
        match Digest.mayHave secondGlyph data.SecondDigest with
        | false -> struct (RunEdit.UnsafeToConcat (cursor.Index, hitIndex + 1), GsubLigatureDone data.Subtable)
        | true -> struct (RunEdit.NextGlyphs 0, GsubLigature (data, 2, 0, hitIndex + 1, false, secondGlyph))

    /// <summary>Checks whether the probed second glyph is skippable, falling back to the slow path when it is.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature set.</param>
    /// <param name="unsafeTo">The fast path's unsafe-to bound.</param>
    /// <param name="hitIndex">The probed glyph's index.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligatureSetProbeHit gdef contextMatcher (cursor: inref<RunCursor>) (data: GsubLigatureData) unsafeTo hitIndex =
        match Matching.maySkip contextMatcher gdef cursor.Input[hitIndex] cursor.InputProps[hitIndex] = SkipVerdict.SkipNo with
        | false -> struct (RunEdit.NextGlyphs 0, GsubLigature (data, 1, 0, unsafeTo, false, 0u))
        | true -> ligatureSetProbeSecond &cursor data hitIndex

    /// <summary>Probes the second glyph of the fast path, <c>LigatureSet::apply</c>'s head.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature set.</param>
    /// <param name="unsafeTo">The fast path's unsafe-to bound.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligatureSetProbe gdef contextMatcher (cursor: inref<RunCursor>) (data: GsubLigatureData) unsafeTo =
        match Matching.next contextMatcher gdef (MatchAlways()) 0 cursor.Input cursor.InputProps cursor.Index cursor.Input.Length with
        | Probe.Miss _ -> struct (RunEdit.NextGlyphs 0, GsubLigature (data, 1, 0, unsafeTo, false, 0u))
        | Probe.Hit hitIndex -> ligatureSetProbeHit gdef contextMatcher &cursor data unsafeTo hitIndex

    /// <summary>Walks one ligature set, the fast and slow paths of <c>LigatureSet::apply</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature set.</param>
    /// <param name="mode">The walk phase.</param>
    /// <param name="index">The next ligature.</param>
    /// <param name="unsafeTo">The fast path's unsafe-to bound.</param>
    /// <param name="unsafeToConcatFlag">The fast path's pending unsafe-to-concat.</param>
    /// <param name="second">The fast path's second glyph.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligatureSetStep gdef hasGlyphClasses inputMatcher contextMatcher (cursor: inref<RunCursor>) (data: GsubLigatureData) mode index unsafeTo unsafeToConcatFlag second =
        match mode with
        | 0 -> ligatureSetProbe gdef contextMatcher &cursor data unsafeTo
        | 1 -> ligatureSetIterate gdef hasGlyphClasses inputMatcher cursor.Input cursor.InputProps cursor.Output cursor.OutputProps cursor.Index cursor.Serial data 1 false 0u index unsafeTo false
        | 2 -> ligatureSetIterate gdef hasGlyphClasses inputMatcher cursor.Input cursor.InputProps cursor.Output cursor.OutputProps cursor.Index cursor.Serial data 2 true second index unsafeTo unsafeToConcatFlag
        | _ -> struct (RunEdit.NextGlyph, GsubScanTry)

    /// <summary>Finds the next component of a ligation, <c>ligate_input</c>'s component walk.</summary>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature data.</param>
    /// <param name="componentIndex">The next component, one-based.</param>
    /// <param name="matchStart">The previous component's match position.</param>
    /// <param name="lastLigId">The previous component's ligature id.</param>
    /// <param name="lastNumComponents">The previous component's component count.</param>
    /// <param name="componentsSoFar">The component count accumulated so far.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligateFindStep inputMatcher gdef (cursor: inref<RunCursor>) (data: GsubLigateData) componentIndex matchStart lastLigId lastNumComponents componentsSoFar =
        match Matching.next inputMatcher gdef (GlyphIds data.Components) (componentIndex - 1) cursor.Input cursor.InputProps matchStart cursor.Input.Length with
        | Probe.Miss _ -> struct (RunEdit.NextGlyphs 0, GsubLigate (data, 4, componentIndex, matchStart, matchStart, lastLigId, lastNumComponents, componentsSoFar, cursor.Index))
        | Probe.Hit hitIndex -> struct (RunEdit.NextGlyphs 0, GsubLigate (data, 2, componentIndex, matchStart, hitIndex, lastLigId, lastNumComponents, componentsSoFar, 0))

    /// <summary>Copies or skips the glyph at a component position, <c>ligate_input</c>'s component walk.</summary>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature data.</param>
    /// <param name="componentIndex">The next component, one-based.</param>
    /// <param name="matchStart">The previous component's match position.</param>
    /// <param name="target">The current component's match position.</param>
    /// <param name="lastLigId">The previous component's ligature id.</param>
    /// <param name="lastNumComponents">The previous component's component count.</param>
    /// <param name="componentsSoFar">The component count accumulated so far.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligateAtTargetStep (cursor: inref<RunCursor>) data componentIndex matchStart target lastLigId lastNumComponents componentsSoFar =
        match cursor.Index < target with
        | true ->
            match data.IsLigature with
            | false -> struct (RunEdit.NextGlyph, GsubLigate (data, 2, componentIndex, matchStart, target, lastLigId, lastNumComponents, componentsSoFar, 0))
            | true ->
                let markProps = cursor.InputProps[cursor.Index]
                let thisComp = GlyphPropsBits.ligComp markProps
                let resolvedComp = if thisComp = 0uy then lastNumComponents else int thisComp
                let newComp = componentsSoFar - lastNumComponents + min resolvedComp lastNumComponents
                let updated = GlyphPropsBits.setLigPropsForMark data.LigId (byte newComp) markProps
                struct (RunEdit.SetInputProps (cursor.Index, updated), GsubLigate (data, 3, componentIndex, matchStart, target, lastLigId, lastNumComponents, componentsSoFar, 0))
        | false ->
            let componentProps = cursor.InputProps[cursor.Index]
            let nextLigId = GlyphPropsBits.ligId componentProps
            let nextNumComponents = int (GlyphPropsBits.ligNumCompsInLigation componentProps)
            struct (RunEdit.SkipGlyph, GsubLigate (data, 2, componentIndex + 1, target, -1, nextLigId, nextNumComponents, componentsSoFar + nextNumComponents, 0))

    /// <summary>Re-adjusts the marks following a ligature, <c>ligate_input</c>'s tail.</summary>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature data.</param>
    /// <param name="lastLigId">The final component's ligature id.</param>
    /// <param name="lastNumComponents">The final component's component count.</param>
    /// <param name="componentsSoFar">The component count accumulated so far.</param>
    /// <param name="scan">The next glyph to scan.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligateReadjustStep (cursor: inref<RunCursor>) data lastLigId lastNumComponents componentsSoFar scan =
        match not data.IsMarkLigature && lastLigId <> 0uy && scan < cursor.Input.Length with
        | false -> struct (RunEdit.SetSerial data.NextSerial, ligateDone data)
        | true ->
            match lastLigId = GlyphPropsBits.ligId cursor.InputProps[scan] with
            | false -> struct (RunEdit.SetSerial data.NextSerial, ligateDone data)
            | true ->
                match GlyphPropsBits.ligComp cursor.InputProps[scan] = 0uy with
                | true -> struct (RunEdit.SetSerial data.NextSerial, ligateDone data)
                | false ->
                    let thisComp = int (GlyphPropsBits.ligComp cursor.InputProps[scan])
                    let newComp = componentsSoFar - lastNumComponents + min thisComp lastNumComponents
                    let updated = GlyphPropsBits.setLigPropsForMark data.LigId (byte newComp) cursor.InputProps[scan]
                    struct (RunEdit.SetInputProps (scan, updated), GsubLigate (data, 4, 0, 0, 0, lastLigId, lastNumComponents, componentsSoFar, scan + 1))

    /// <summary>Replays the ligate-input edits, <c>ligate_input</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="cursor">The run cursor.</param>
    /// <param name="data">The ligature data.</param>
    /// <param name="phase">The replay phase.</param>
    /// <param name="componentIndex">The next component, one-based.</param>
    /// <param name="matchStart">The previous component's match position.</param>
    /// <param name="target">The current component's match position.</param>
    /// <param name="lastLigId">The previous component's ligature id.</param>
    /// <param name="lastNumComponents">The previous component's component count.</param>
    /// <param name="componentsSoFar">The component count accumulated so far.</param>
    /// <param name="scan">The readjust scan index.</param>
    /// <returns>The edit and the next state.</returns>
    let private ligateStep gdef inputMatcher (cursor: inref<RunCursor>) (data: GsubLigateData) phase componentIndex matchStart target lastLigId lastNumComponents componentsSoFar scan =
        match phase with
        | 0 -> struct (RunEdit.MergeClusters (cursor.Index, data.MatchEnd), GsubLigate (data, 1, 1, cursor.Index, -1, lastLigId, lastNumComponents, componentsSoFar, 0))
        | 1 -> struct (RunEdit.ReplaceGlyphProps (data.LigGlyph, data.FinalProps), GsubLigate (data, 2, 1, matchStart, -1, lastLigId, lastNumComponents, componentsSoFar, 0))
        | 2 ->
            match componentIndex >= data.Count with
            | true -> struct (RunEdit.NextGlyphs 0, GsubLigate (data, 4, componentIndex, matchStart, target, lastLigId, lastNumComponents, componentsSoFar, cursor.Index))
            | false ->
                match target with
                | -1 -> ligateFindStep inputMatcher gdef &cursor data componentIndex matchStart lastLigId lastNumComponents componentsSoFar
                | target -> ligateAtTargetStep &cursor data componentIndex matchStart target lastLigId lastNumComponents componentsSoFar
        | 3 -> struct (RunEdit.NextGlyph, GsubLigate (data, 2, componentIndex, matchStart, target, lastLigId, lastNumComponents, componentsSoFar, 0))
        | 4 -> ligateReadjustStep &cursor data lastLigId lastNumComponents componentsSoFar scan
        | _ -> struct (RunEdit.Finish, GsubDone)

    /// <summary>The number of subtables of a lookup that get an external cache with their rule-set digests,
    /// <c>if (i &lt; 8)</c> in <c>hb_accelerate_subtables_context_t::dispatch</c> (src/hb-ot-layout-gsubgpos.hh:1569).</summary>
    [<Literal>]
    let private CachedSubtables = 8

    /// <summary>The coverage index of a glyph in a multiple or ligature subtable, the index of its sequence or ligature set.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The index, zero for a subtable without sets or a glyph it does not cover.</returns>
    let private setIndexOf subtable glyph =
        match subtable with
        | Multiple (coverage, _) | LigatureSubst (coverage, _, _) ->
            match Coverage.index coverage glyph with
            | CoverageHit.Covered index -> index
            | CoverageHit.NotCovered -> 0
        | SingleDelta _ | SingleList _ | Alternate _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> 0

    /// <summary>The outcome of one subtable on the glyph at an index, a miss when it does not match.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="random">Whether the random feature is on.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="position">The subtable's position in the lookup.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="randomState">The buffer random state.</param>
    /// <returns>The outcome.</returns>
    let private subtableOutcome gdef hasGlyphClasses lookupMask random subtable position (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index randomState =
        let miss = SubstOutcome(SubstKind.Miss, 0, 0, 0u, Unchecked.defaultof<GlyphProps>, 0u)
        match subtable with
        | LigatureSubst (coverage, sets, _) ->
            match Coverage.index coverage infos[index].Codepoint with
            | CoverageHit.Covered setIndex when setIndex < sets.Length -> SubstOutcome(SubstKind.Ligature, position, setIndex, 0u, Unchecked.defaultof<GlyphProps>, 0u)
            | CoverageHit.Covered _ | CoverageHit.NotCovered -> miss
        | SingleDelta (coverage, delta, glyphMask) ->
            match GsubApply.singleDeltaGlyph coverage delta glyphMask infos[index] with
            | ValueNone -> miss
            | ValueSome glyph -> SubstOutcome(SubstKind.Replace, position, 0, glyph, Matching.setGlyphClass gdef hasGlyphClasses glyph 0us false false props[index], 0u)
        | SingleList (coverage, substitutes) ->
            match GsubApply.singleListGlyph coverage substitutes infos[index] with
            | ValueNone -> miss
            | ValueSome glyph -> SubstOutcome(SubstKind.Replace, position, 0, glyph, Matching.setGlyphClass gdef hasGlyphClasses glyph 0us false false props[index], 0u)
        | Multiple _ | Alternate _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ ->
            match GsubApply.trySubtableAt gdef hasGlyphClasses lookupMask random subtable infos props index randomState with
            | GsubMatch.GsubMiss -> miss
            | GsubMatch.GsubDelete -> SubstOutcome(SubstKind.Delete, position, 0, 0u, Unchecked.defaultof<GlyphProps>, 0u)
            | GsubMatch.GsubReplace (info, replaced) -> SubstOutcome(SubstKind.Replace, position, 0, info.Codepoint, replaced, 0u)
            | GsubMatch.GsubAlternateRandom (info, replaced, next) -> SubstOutcome(SubstKind.AlternateRandom, position, 0, info.Codepoint, replaced, next)
            | GsubMatch.GsubMultiple _ -> SubstOutcome(SubstKind.Multiple, position, setIndexOf subtable infos[index].Codepoint, 0u, Unchecked.defaultof<GlyphProps>, 0u)
            | GsubMatch.GsubLigature _ -> miss

    /// <summary>Tries the subtables of a lookup on the glyph at an index in turn, <c>Lookup::apply</c>'s subtable loop, and
    /// answers the first outcome that is not a miss.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="random">Whether the random feature is on.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="subtable">The next subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="randomState">The buffer random state.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private trySubtables gdef hasGlyphClasses lookupMask random (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index randomState =
        match subtable < subtables.Length with
        | false -> SubstOutcome(SubstKind.Miss, 0, 0, 0u, Unchecked.defaultof<GlyphProps>, 0u)
        | true ->
            let accel = subtables[subtable]
            match Digest.mayHave infos[index].Codepoint accel.SubtableDigest with
            | false -> trySubtables gdef hasGlyphClasses lookupMask random subtables (subtable + 1) infos props index randomState
            | true ->
                let outcome = subtableOutcome gdef hasGlyphClasses lookupMask random accel.Subtable subtable infos props index randomState
                match outcome.Kind with
                | SubstKind.Miss -> trySubtables gdef hasGlyphClasses lookupMask random subtables (subtable + 1) infos props index randomState
                | _ -> outcome

    /// <summary>The outcome of one context or chained-context subtable on the glyph at an index.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="chains">The dense sets of the lookup's chained-context subtables, empty for none.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="position">The subtable's position in the lookup.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The buffer a match fills with its input positions, and the share after them.</param>
    /// <param name="share">The share of the visit, <see cref="ContextApply.shareBacktrack"/>.</param>
    /// <returns>The outcome.</returns>
    let private contextSubtableOutcome gdef inputMatcher contextMatcher (chains: ImmutableArray<ChainSets voption>) subtable position (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) index (positions: Span<int>) (share: ContextApply.VisitShare) =
        let useDigest = position < CachedSubtables
        match subtable with
        | SubstContext rules -> ContextApply.matchContext rules useDigest gdef inputMatcher contextMatcher infos props outInfos outProps outInfos.Length index positions
        | SubstChainContext rules ->
            match rules, (match position < chains.Length with | true -> chains[position] | false -> ValueNone) with
            | ChainCoverageRule (_, _, _, records), ValueSome sets -> ContextApply.matchChainDenseShared sets records inputMatcher contextMatcher gdef infos props outInfos outProps outInfos.Length index positions share
            | _, _ -> ContextApply.matchChain rules useDigest gdef inputMatcher contextMatcher infos props outInfos outProps outInfos.Length index positions
        | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | ReverseChainSingle _ | SubstPending _ ->
            ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, FlagBounds.none, -1)

    /// <summary>Whether one subtable of a context lookup cannot match at a glyph: a chained subtable of the third format with dense sets
    /// decides by the first steps of its match; any other subtable cannot be told without matching.</summary>
    /// <param name="heads">The first steps of the walks at this glyph, <see cref="ContextApply.visitHeads"/>.</param>
    /// <param name="share">The share of the visit, empty when the lookup has none.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="chains">The dense sets of the lookup's chained-context subtables, empty for none.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="position">The subtable's position in the lookup.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The positions buffer holding the share.</param>
    /// <returns>True when the subtable cannot match.</returns>
    let private subtableCannotMatch (heads: ContextApply.VisitHeads) (share: ContextApply.VisitShare) gdef inputMatcher contextMatcher (chains: ImmutableArray<ChainSets voption>) subtable position (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) index (positions: Span<int>) =
        match subtable, (match position < chains.Length with | true -> chains[position] | false -> ValueNone) with
        | SubstChainContext (ChainCoverageRule _), ValueSome sets -> ContextApply.chainDenseCannotMatchShared heads share sets inputMatcher contextMatcher gdef infos props outInfos outProps index positions
        | _, _ -> false

    /// <summary>Whether no subtable of a context lookup from one on can match at a glyph.</summary>
    /// <param name="heads">The first steps of the walks at this glyph.</param>
    /// <param name="share">The share of the visit, empty when the lookup has none.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="chains">The dense sets of the lookup's chained-context subtables, empty for none.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="subtable">The next subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The positions buffer holding the share.</param>
    /// <returns>True when every subtable from the next cannot match.</returns>
    [<TailCall>]
    let rec private noSubtableCanMatch (heads: ContextApply.VisitHeads) (share: ContextApply.VisitShare) gdef inputMatcher contextMatcher (chains: ImmutableArray<ChainSets voption>) (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) index (positions: Span<int>) =
        match subtable < subtables.Length with
        | false -> true
        | true ->
            let accel = subtables[subtable]
            match Digest.mayHave infos[index].Codepoint accel.SubtableDigest && not (subtableCannotMatch heads share gdef inputMatcher contextMatcher chains accel.Subtable subtable infos props outInfos outProps index positions) with
            | true -> false
            | false -> noSubtableCanMatch heads share gdef inputMatcher contextMatcher chains subtables (subtable + 1) infos props outInfos outProps index positions

    /// <summary>Tries the subtables of a context lookup on the glyph at an index in turn, <c>Lookup::apply</c>'s subtable
    /// loop, accumulating the ranges the subtables that failed owe.</summary>
    /// <param name="dropping">Whether a subtable that cannot match is passed over and the ranges the failed subtables owe are dropped.</param>
    /// <param name="heads">The first steps of the walks at this glyph, when the subtables that cannot match are passed over.</param>
    /// <param name="share">The share of the visit.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="chains">The dense sets of the lookup's chained-context subtables, empty for none.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="subtable">The next subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The buffer a match fills with its input positions.</param>
    /// <param name="owed">The ranges the subtables that failed owe.</param>
    /// <returns>The outcome of the first subtable that matches, or a miss holding the ranges owed.</returns>
    [<TailCall>]
    let rec private tryContextSubtables dropping (heads: ContextApply.VisitHeads) (share: ContextApply.VisitShare) gdef inputMatcher contextMatcher (chains: ImmutableArray<ChainSets voption>) (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) index (positions: Span<int>) owed =
        match subtable < subtables.Length with
        | false -> ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let accel = subtables[subtable]
            match Digest.mayHave infos[index].Codepoint accel.SubtableDigest && not (dropping && subtableCannotMatch heads share gdef inputMatcher contextMatcher chains accel.Subtable subtable infos props outInfos outProps index positions) with
            | false -> tryContextSubtables dropping heads share gdef inputMatcher contextMatcher chains subtables (subtable + 1) infos props outInfos outProps index positions owed
            | true ->
                let outcome = contextSubtableOutcome gdef inputMatcher contextMatcher chains accel.Subtable subtable infos props outInfos outProps index positions share
                match outcome.Matched with
                | true -> ContextOutcome(true, outcome.Chain, outcome.InputCount, outcome.MatchEnd, outcome.Records, outcome.StartIndex, outcome.EndIndex, FlagBounds.merge owed outcome.Owed, outcome.AfterConcat)
                | false -> tryContextSubtables dropping heads share gdef inputMatcher contextMatcher chains subtables (subtable + 1) infos props outInfos outProps index positions (FlagBounds.merge owed outcome.Owed)

    /// <summary>The outcome of one subtable on the glyph at an index of a reverse lookup: the subtable's own outcome when it is
    /// a reverse-chaining subtable, a miss otherwise.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The outcome.</returns>
    let private reverseSubtableOutcome gdef contextMatcher subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index =
        match subtable with
        | ReverseChainSingle (coverage, backtrack, lookahead, substitutes) -> ContextApply.matchReverse coverage backtrack lookahead substitutes contextMatcher gdef infos props index
        | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | SubstPending _ -> ReverseMiss FlagBounds.none

    /// <summary>Tries the reverse-chaining subtables of a lookup on the glyph at an index in turn, <c>apply_backward</c>'s
    /// per-glyph step: the first subtable that substitutes decides, and the subtables that failed before it owe the
    /// unsafe-to-concat ranges they set.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="subtable">The next subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="owed">The ranges the subtables that failed owe.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private reverseTry gdef contextMatcher (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index owed =
        match subtable < subtables.Length with
        | false -> ReverseMiss owed
        | true ->
            let accel = subtables[subtable]
            match Digest.mayHave infos[index].Codepoint accel.SubtableDigest with
            | false -> reverseTry gdef contextMatcher subtables (subtable + 1) infos props index owed
            | true ->
                match reverseSubtableOutcome gdef contextMatcher accel.Subtable infos props index with
                | ReverseMiss failed -> reverseTry gdef contextMatcher subtables (subtable + 1) infos props index (FlagBounds.merge owed failed)
                | ReverseSubstitute (substitute, start, endIndex, _) -> ReverseSubstitute (substitute, start, endIndex, owed)

    /// <summary>Probes the second glyph of a ligature set, <c>LigatureSet::apply</c>'s head.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="secondDigest">The subtable's second-glyph digest.</param>
    /// <param name="count">The number of ligatures of the set.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first glyph's index.</param>
    /// <returns>How the walk of the set goes on.</returns>
    let private ligatureProbe gdef contextMatcher secondDigest count (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index =
        match Matching.next contextMatcher gdef (MatchAlways()) 0 infos props index infos.Length with
        | Probe.Miss _ -> LigatureHead(LigatureHeadKind.Slow, 0, 0u, count)
        | Probe.Hit hitIndex ->
            match Matching.maySkip contextMatcher gdef infos[hitIndex] props[hitIndex] = SkipVerdict.SkipNo with
            | false -> LigatureHead(LigatureHeadKind.Slow, 0, 0u, count)
            | true ->
                let secondGlyph = infos[hitIndex].Codepoint
                match Digest.mayHave secondGlyph secondDigest with
                | false -> LigatureHead(LigatureHeadKind.NoSecond, hitIndex + 1, 0u, count)
                | true -> LigatureHead(LigatureHeadKind.Fast, hitIndex + 1, secondGlyph, count)

    /// <summary>The head of a ligature-set walk and the number of ligatures of the set: a set of at most one ligature is walked slowly
    /// and its second glyph is not probed.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="secondDigest">The subtable's second-glyph digest.</param>
    /// <param name="count">The number of ligatures of the set.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first glyph's index.</param>
    /// <returns>How the walk of the set goes on.</returns>
    let private ligatureHead gdef contextMatcher secondDigest count (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index =
        match count <= 1 with
        | true -> LigatureHead(LigatureHeadKind.Slow, 0, 0u, count)
        | false -> ligatureProbe gdef contextMatcher secondDigest count infos props index

    /// <summary>The first ligature of a set at or after an index that the fast path of <c>LigatureSet::apply</c> tries: one with no
    /// components after the first, or whose second glyph is the probed one.</summary>
    /// <param name="ligatures">The ligature set.</param>
    /// <param name="index">The first ligature to consider.</param>
    /// <param name="second">The probed second glyph.</param>
    /// <returns>The ligature's index, or the number of ligatures when none is tried.</returns>
    [<TailCall>]
    let rec internal ligatureNext (ligatures: ImmutableArray<Ligature>) index second =
        match index < ligatures.Length with
        | false -> index
        | true ->
            let lig = ligatures[index]
            match GsubTables.ligCount lig <= 1 || lig.Components[0] = second with
            | true -> index
            | false -> ligatureNext ligatures (index + 1) second

    /// <summary>Tries one subtable on the current glyph and turns the outcome into the next edits,
    /// <c>Lookup::apply</c>'s subtable loop.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="lookupMask">The lookup mask.</param>
    /// <param name="random">Whether the random feature is on.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="subtable">The subtable index.</param>
    /// <param name="cursor">The run cursor.</param>
    /// <returns>The edit and the next state.</returns>
    let private trySubtable gdef hasGlyphClasses lookupMask random (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) subtable (cursor: inref<RunCursor>) =
        match subtable < subtables.Length with
        | false -> struct (RunEdit.NextGlyph, GsubScanTry)
        | true ->
            let accel = subtables[subtable]
            match Digest.mayHave cursor.Input[cursor.Index].Codepoint accel.SubtableDigest with
            | false -> struct (RunEdit.NextGlyphs 0, GsubTry (subtable + 1))
            | true ->
                match GsubApply.trySubtable gdef hasGlyphClasses lookupMask random accel.Subtable &cursor with
                | GsubMatch.GsubMiss -> struct (RunEdit.NextGlyphs 0, GsubTry (subtable + 1))
                | GsubMatch.GsubDelete -> struct (RunEdit.DeleteGlyph, GsubScanTry)
                | GsubMatch.GsubReplace (info, props) -> struct (RunEdit.ReplaceGlyphProps (info.Codepoint, props), GsubScanTry)
                | GsubMatch.GsubAlternateRandom (info, props, randomState) ->
                    struct (RunEdit.UnsafeToBreak (0, cursor.Input.Length), GsubAltRandom (info, props, randomState))
                | GsubMatch.GsubMultiple (info, components, klass, baseProps, ligId) ->
                    let glyph = components[0]
                    let props = multipleComponentProps gdef hasGlyphClasses klass baseProps ligId glyph 0
                    struct (RunEdit.OutputInfo ({ info with Codepoint = glyph }, props), GsubMultiple { MultipleInfo = info; MultipleComponents = components; MultipleKlass = klass; MultipleBaseProps = baseProps; MultipleLigId = ligId; MultipleIndex = 1 })
                | GsubMatch.GsubLigature (ligatures, secondDigest) ->
                    let mode = if ligatures.Length <= 1 then 1 else 0
                    struct (RunEdit.NextGlyphs 0, GsubLigature ({ Ligatures = ligatures; SecondDigest = secondDigest; Subtable = subtable }, mode, 0, 0, false, 0u))

    /// <summary>The first steps of the walks of a visit when the lookup shares them among its subtables, the unshared heads when it does not.</summary>
    /// <param name="shareDepths">The depths the lookup shares.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="outInfos">The output glyphs, as long as the output.</param>
    /// <param name="outProps">The output properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The heads.</returns>
    let private visitHeadsOf (shareDepths: ContextApply.ShareDepths) inputMatcher contextMatcher gdef (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (outInfos: ReadOnlySpan<GlyphInfo>) (outProps: ReadOnlySpan<GlyphProps>) index =
        match shareDepths.BackDepth > 0 || shareDepths.ForwardDepth > 0 with
        | true -> ContextApply.visitHeads inputMatcher contextMatcher gdef infos props outInfos outProps index
        | false -> ContextApply.unsharedHeads

    /// <summary>Whether a subtable of the lookup is a context or chained-context subtable.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when one is.</returns>
    [<TailCall>]
    let rec private anyContextSubtable (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) index =
        match index < subtables.Length with
        | false -> false
        | true ->
            match subtables[index].Subtable with
            | SubstContext _ | SubstChainContext _ -> true
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | ReverseChainSingle _ | SubstPending _ -> anyContextSubtable subtables (index + 1)

    /// <summary>The pass that applies one GSUB lookup, <c>apply_string</c> for the forward single, multiple and
    /// alternate subtables.</summary>
    [<Struct>]
    type private GsubPass =
        val Gdef: Gdef
        val HasGlyphClasses: bool
        val LookupMask: uint32
        val LookupProps: uint32
        val LookupDigest: Digest
        val Random: bool
        val AutoZwnj: bool
        val AutoZwj: bool
        val PerSyllable: bool
        val Subtables: ImmutableArray<SubtableAccel<SubstSubtable>>
        val ContextLookup: bool
        val InputMatcher: Matcher
        val ContextMatcher: Matcher
        val Covered: ImmutableArray<uint64>
        val ExactScan: bool
        val Chains: ImmutableArray<ChainSets voption>
        val ShareDepths: ContextApply.ShareDepths

        /// <summary>Creates a GSUB pass.</summary>
        /// <param name="gdef">The GDEF.</param>
        /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
        /// <param name="lookupMask">The lookup mask.</param>
        /// <param name="lookupProps">The sanitized lookup props.</param>
        /// <param name="lookupDigest">The lookup digest.</param>
        /// <param name="random">Whether the random feature drives alternates.</param>
        /// <param name="subtables">The lookup's subtables.</param>
        /// <param name="autoZwnj">Whether ZWNJ is ignored automatically.</param>
        /// <param name="autoZwj">Whether ZWJ is ignored automatically.</param>
        /// <param name="perSyllable">Whether matching is per syllable.</param>
        /// <param name="covered">The bit set of the glyphs the subtables cover, walked instead of the digest when
        /// <paramref name="exactScan"/> is set.</param>
        /// <param name="exactScan">Whether the walk for the next glyph tests the exact set.</param>
        new (gdef, hasGlyphClasses, lookupMask, lookupProps, lookupDigest, random, subtables, autoZwnj, autoZwj, perSyllable, covered, exactScan) =
            { Gdef = gdef
              HasGlyphClasses = hasGlyphClasses
              LookupMask = lookupMask
              LookupProps = lookupProps
              LookupDigest = lookupDigest
              Random = random
              AutoZwnj = autoZwnj
              AutoZwj = autoZwj
              PerSyllable = perSyllable
              Subtables = subtables
              ContextLookup = anyContextSubtable subtables 0
              InputMatcher = Matching.matcherFor 0 false lookupProps lookupMask autoZwnj autoZwj perSyllable 0uy
              ContextMatcher = Matching.matcherFor 0 true lookupProps lookupMask autoZwnj autoZwj perSyllable 0uy
              Covered = covered
              ExactScan = exactScan
              Chains = ImmutableArray<ChainSets voption>.Empty
              ShareDepths = { BackDepth = 0; ForwardDepth = 0 } }

        /// <summary>Creates a GSUB pass whose chained-context subtables of the third format are matched through dense sets.</summary>
        /// <param name="gdef">The GDEF.</param>
        /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
        /// <param name="lookupMask">The lookup mask.</param>
        /// <param name="lookupProps">The sanitized lookup props.</param>
        /// <param name="lookupDigest">The lookup digest.</param>
        /// <param name="random">Whether the random feature drives alternates.</param>
        /// <param name="subtables">The lookup's subtables.</param>
        /// <param name="autoZwnj">Whether ZWNJ is ignored automatically.</param>
        /// <param name="autoZwj">Whether ZWJ is ignored automatically.</param>
        /// <param name="perSyllable">Whether matching is per syllable.</param>
        /// <param name="covered">The bit set of the glyphs the subtables cover.</param>
        /// <param name="exactScan">Whether the walk for the next glyph tests the exact set.</param>
        /// <param name="chains">The dense sets of each subtable that has them.</param>
        new (gdef, hasGlyphClasses, lookupMask, lookupProps, lookupDigest, random, subtables, autoZwnj, autoZwj, perSyllable, covered, exactScan, chains) =
            { Gdef = gdef
              HasGlyphClasses = hasGlyphClasses
              LookupMask = lookupMask
              LookupProps = lookupProps
              LookupDigest = lookupDigest
              Random = random
              AutoZwnj = autoZwnj
              AutoZwj = autoZwj
              PerSyllable = perSyllable
              Subtables = subtables
              ContextLookup = anyContextSubtable subtables 0
              InputMatcher = Matching.matcherFor 0 false lookupProps lookupMask autoZwnj autoZwj perSyllable 0uy
              ContextMatcher = Matching.matcherFor 0 true lookupProps lookupMask autoZwnj autoZwj perSyllable 0uy
              Covered = covered
              ExactScan = exactScan
              Chains = chains
              ShareDepths = ContextApply.shareDepthsOf chains 0 0 0 0 0 }

        interface IGsubLookup with
            member this.Scan (infos, props, index) =
                match this.ExactScan with
                | true -> scanCovered this.Covered this.LookupMask this.LookupProps this.Gdef infos props index
                | false -> scanForward this.LookupDigest this.LookupMask this.LookupProps this.Gdef infos props index

            member this.Try (infos, props, index, randomState) =
                trySubtables this.Gdef this.HasGlyphClasses this.LookupMask this.Random this.Subtables 0 infos props index randomState

            member this.TryAfter (subtable, infos, props, index, randomState) =
                trySubtables this.Gdef this.HasGlyphClasses this.LookupMask this.Random this.Subtables (subtable + 1) infos props index randomState

            member this.TryContext (infos, props, outInfos, outProps, index, positions) =
                let inputMatcher = Matching.atSyllable this.InputMatcher props index
                let contextMatcher = Matching.atSyllable this.ContextMatcher props index
                let share = ContextApply.shareVisit this.ShareDepths inputMatcher contextMatcher this.Gdef infos props outInfos outProps index positions
                tryContextSubtables false ContextApply.unsharedHeads share this.Gdef inputMatcher contextMatcher this.Chains this.Subtables 0 infos props outInfos outProps index positions FlagBounds.none

            member this.TryContextDropping (infos, props, outInfos, outProps, index, positions) =
                let inputMatcher = Matching.atSyllable this.InputMatcher props index
                let contextMatcher = Matching.atSyllable this.ContextMatcher props index
                let heads = visitHeadsOf this.ShareDepths inputMatcher contextMatcher this.Gdef infos props outInfos outProps index
                let share = ContextApply.shareVisit this.ShareDepths inputMatcher contextMatcher this.Gdef infos props outInfos outProps index positions
                tryContextSubtables true heads share this.Gdef inputMatcher contextMatcher this.Chains this.Subtables 0 infos props outInfos outProps index positions FlagBounds.none

            member this.CannotMatch (infos, props, outInfos, outProps, index) =
                let inputMatcher = Matching.atSyllable this.InputMatcher props index
                let contextMatcher = Matching.atSyllable this.ContextMatcher props index
                let heads = visitHeadsOf this.ShareDepths inputMatcher contextMatcher this.Gdef infos props outInfos outProps index
                noSubtableCanMatch heads ContextApply.noShare this.Gdef inputMatcher contextMatcher this.Chains this.Subtables 0 infos props outInfos outProps index Span<int>.Empty

            member this.ReverseTry (infos, props, index) =
                reverseTry this.Gdef (Matching.atSyllable this.ContextMatcher props index) this.Subtables 0 infos props index FlagBounds.none

            member this.ReverseAdmits (infos, props, index) =
                Digest.mayHave infos[index].Codepoint this.LookupDigest
                && infos[index].Mask &&& this.LookupMask <> 0u
                && Matching.checkGlyphProperty this.Gdef infos[index].Codepoint props[index].GlyphClass this.LookupProps

            member this.ReverseReplace (substitute, props) =
                Matching.setGlyphClass this.Gdef this.HasGlyphClasses substitute 0us false false props

            member this.HasContext () = this.ContextLookup

            member this.Nest () = struct (this.LookupMask, this.Random, this.AutoZwnj, this.AutoZwj, this.PerSyllable)

            member this.MultipleComponents (subtable, index) =
                match this.Subtables[subtable].Subtable with
                | Multiple (_, sequences) -> sequences[index]
                | SingleDelta _ | SingleList _ | Alternate _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> ImmutableArray.Empty

            member this.MultipleProps (baseProps, glyph, index) =
                let klass = match GlyphPropsBits.isLigature baseProps with | true -> GlyphPropsBits.Base | false -> 0us
                multipleComponentProps this.Gdef this.HasGlyphClasses klass baseProps (GlyphPropsBits.ligId baseProps) glyph index

            member this.LigatureHead (subtable, set, infos, props, index) =
                match this.Subtables[subtable].Subtable with
                | LigatureSubst (_, sets, secondDigest) -> ligatureHead this.Gdef (Matching.atSyllable this.ContextMatcher props index) secondDigest sets[set].Length infos props index
                | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> LigatureHead(LigatureHeadKind.Slow, 0, 0u, 0)

            member this.LigatureNext (subtable, set, first, second) =
                match this.Subtables[subtable].Subtable with
                | LigatureSubst (_, sets, _) -> ligatureNext sets[set] first second
                | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> first

            member this.LigatureApply (subtable, set, ligature, infos, props, outInfos, outProps, index, serial) =
                match this.Subtables[subtable].Subtable with
                | LigatureSubst (_, sets, _) ->
                    let lig = sets[set][ligature]
                    GsubApply.ligatureApply this.Gdef this.HasGlyphClasses (Matching.atSyllable this.InputMatcher props index) lig infos props outInfos outProps index serial
                | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> LigatureMiss

            member this.NextComponent (data, componentIndex, infos, props, matchStart) =
                match Matching.next (Matching.withSyllable this.InputMatcher data.FinalProps.Syllable) this.Gdef (GlyphIds data.Components) (componentIndex - 1) infos props matchStart infos.Length with
                | Probe.Hit hitIndex -> hitIndex
                | Probe.Miss _ -> -1

        interface IRunPass<GsubState> with
            member this.Step (cursor, state) =
                match state with
                | GsubDone -> struct (RunEdit.Finish, GsubDone)
                | GsubScan ->
                    match cursor.Index >= cursor.Input.Length || not cursor.Successful with
                    | true -> struct (RunEdit.Finish, GsubDone)
                    | false ->
                        let j = scanForward this.LookupDigest this.LookupMask this.LookupProps this.Gdef cursor.Input cursor.InputProps cursor.Index
                        match j >= cursor.Input.Length with
                        | true -> struct (RunEdit.Finish, GsubDone)
                        | false -> struct (RunEdit.ClearOutput, GsubOpen j)
                | GsubOpen skip -> struct (RunEdit.NextGlyphs (skip - cursor.Index), GsubTry 0)
                | GsubScanTry ->
                    match cursor.Index >= cursor.Input.Length || not cursor.Successful with
                    | true -> struct (RunEdit.Finish, GsubDone)
                    | false ->
                        let j = scanForward this.LookupDigest this.LookupMask this.LookupProps this.Gdef cursor.Input cursor.InputProps cursor.Index
                        match struct (j >= cursor.Input.Length, j = cursor.Index) with
                        | struct (true, _) -> struct (RunEdit.Finish, GsubDone)
                        | struct (false, true) ->
                            trySubtable this.Gdef this.HasGlyphClasses this.LookupMask this.Random this.Subtables 0 &cursor
                        | struct (false, false) -> struct (RunEdit.NextGlyphs (j - cursor.Index), GsubTry 0)
                | GsubTry subtable ->
                    trySubtable this.Gdef this.HasGlyphClasses this.LookupMask this.Random this.Subtables subtable &cursor
                | GsubSkip -> struct (RunEdit.SkipGlyph, GsubScanTry)
                | GsubMultiple multiple ->
                    match multiple.MultipleIndex < multiple.MultipleComponents.Length with
                    | false -> struct (RunEdit.SkipGlyph, GsubScanTry)
                    | true ->
                        let glyph = multiple.MultipleComponents[multiple.MultipleIndex]
                        let props = multipleComponentProps this.Gdef this.HasGlyphClasses multiple.MultipleKlass multiple.MultipleBaseProps multiple.MultipleLigId glyph multiple.MultipleIndex
                        struct (RunEdit.OutputInfo ({ multiple.MultipleInfo with Codepoint = glyph }, props), GsubMultiple { multiple with MultipleIndex = multiple.MultipleIndex + 1 })
                | GsubAltRandom (info, props, randomState) ->
                    struct (RunEdit.SetRandomState randomState, GsubAltOutput (info, props))
                | GsubAltOutput (info, props) -> struct (RunEdit.ReplaceGlyphProps (info.Codepoint, props), GsubScanTry)
                | GsubLigature (data, mode, index, unsafeTo, unsafeToConcatFlag, second) ->
                    ligatureSetStep this.Gdef this.HasGlyphClasses (Matching.atSyllable this.InputMatcher cursor.InputProps cursor.Index) (Matching.atSyllable this.ContextMatcher cursor.InputProps cursor.Index) &cursor data mode index unsafeTo unsafeToConcatFlag second
                | GsubLigate (data, phase, componentIndex, matchStart, target, lastLigId, lastNumComponents, componentsSoFar, scan) ->
                    ligateStep this.Gdef (Matching.withSyllable this.InputMatcher data.FinalProps.Syllable) &cursor data phase componentIndex matchStart target lastLigId lastNumComponents componentsSoFar scan
                | GsubLigatureDone subtable -> struct (RunEdit.NextGlyphs 0, GsubTry (subtable + 1))
                | GsubConcatAfter concatTo -> struct (RunEdit.UnsafeToConcat (cursor.Index, concatTo), GsubScanTry)

    /// <summary>Applies one lookup of a table to a run, <c>apply_string</c>.</summary>
    type ITableApplier<'S> =
        /// <summary>Applies the lookup at an index.</summary>
        /// <param name="table">The table.</param>
        /// <param name="lookupIndex">The lookup index.</param>
        /// <param name="lookupMap">The compiled lookup.</param>
        /// <param name="font">The font.</param>
        /// <param name="run">The run.</param>
        /// <returns>The run.</returns>
        abstract Apply : table: LayoutTable<'S> * lookupIndex: int * lookupMap: LookupMap * font: Font * run: GlyphRun -> GlyphRun

    /// <summary>The GSUB table applier of the interpreter the lookup kernel replaced: single, multiple, alternate and ligature
    /// subtables apply; context, chained-context and reverse-chaining subtables do not, and the kernel is the only engine that
    /// applies them.</summary>
    [<Struct>]
    type GsubApplier =
        interface ITableApplier<SubstSubtable> with
            member _.Apply (table, lookupIndex, lookupMap, font, run) =
                match run.Infos.IsEmpty || lookupMap.Mask = 0u with
                | true -> run
                | false ->
                    let accel = table.Lookups[lookupIndex].Value
                    let gdef = font.Ot.Gdef.Value
                    let pass =
                        GsubPass(gdef, Gdef.hasGlyphClasses gdef, lookupMap.Mask, Gdef.sanitizeLookupProps gdef accel.Props, accel.LookupDigest, lookupMap.Random, accel.Subtables, lookupMap.AutoZwnj, lookupMap.AutoZwj, lookupMap.PerSyllable, ImmutableArray.Empty, false)
                    match accel.IsReverse with
                    | false -> GlyphRun.runPass pass GsubScan run
                    | true -> run

    /// <summary>Whether a lookup should be applied, the digest gating of <c>hb_ot_map_t::apply</c>.</summary>
    /// <param name="gate">Whether digest gating is on.</param>
    /// <param name="accel">The lookup accelerator.</param>
    /// <param name="digest">The run digest.</param>
    /// <param name="run">The run.</param>
    /// <returns>True when the lookup is attempted.</returns>
    let private shouldApply (gate: bool) (accel: LookupAccel<'S>) (digest: Digest) (run: GlyphRun) : bool =
        let produceUnsafeToConcat = run.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default
        match gate with
        | false -> true
        | true -> Digest.mayIntersect accel.LookupDigest digest && (produceUnsafeToConcat || Digest.mayIntersect accel.SecondDigest digest)

    /// <summary>Applies the lookups of one table stage by stage, <c>hb_ot_map_t::apply</c>.</summary>
    /// <param name="gate">Whether digest gating is on.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="table">The table.</param>
    /// <param name="applier">The table applier.</param>
    /// <param name="font">The font.</param>
    /// <param name="stageIndex">The next stage.</param>
    /// <param name="lookupIndex">The next lookup.</param>
    /// <param name="digest">The run digest.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every lookup.</returns>
    [<TailCall>]
    let rec private applyStages<'A, 'S when 'A : struct and 'A :> ITableApplier<'S>>
            (gate: bool)
            (lookups: ReadOnlySpan<LookupMap>)
            (stages: ReadOnlySpan<StageMap>)
            (table: LayoutTable<'S>)
            (applier: 'A)
            (font: Font)
            (stageIndex: int)
            (lookupIndex: int)
            (digest: Digest)
            (run: GlyphRun)
            : GlyphRun =
        match stageIndex < stages.Length with
        | false -> run
        | true ->
            let stage = stages[stageIndex]
            match lookupIndex < stage.LastLookup with
            | false -> applyStages gate lookups stages table applier font (stageIndex + 1) lookupIndex digest run
            | true ->
                let lookupMap = lookups[lookupIndex]
                let accel = table.Lookups[lookupMap.LookupIndex].Value
                let run, digest =
                    match shouldApply gate accel digest run with
                    | false -> run, digest
                    | true ->
                        let next = applier.Apply (table, lookupMap.LookupIndex, lookupMap, font, run)
                        let digest = match obj.ReferenceEquals(next, run) with | true -> digest | false -> Digest.ofSpan (next.Infos.AsSpan())
                        next, digest
                applyStages gate lookups stages table applier font stageIndex (lookupIndex + 1) digest run

    /// <summary>Applies one table's compiled lookups, <c>hb_ot_map_t::apply</c>.</summary>
    /// <param name="gate">Whether digest gating is on.</param>
    /// <param name="applier">The table applier.</param>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the table.</returns>
    let internal applyTable<'A, 'S when 'A : struct and 'A :> ITableApplier<'S>> (gate: bool) (applier: 'A) (table: LayoutTable<'S>) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (font: Font) (run: GlyphRun) : GlyphRun =
        let digest = Digest.ofSpan (run.Infos.AsSpan())
        applyStages gate (lookups.AsSpan()) (stages.AsSpan()) table applier font 0 0 digest run

    /// <summary>The glyphs that can start a match of a GSUB subtable, <see cref="LookupGlyphs.subst"/>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The glyphs that can start a match.</returns>
    let internal substSubtableGlyphs (subtable: SubstSubtable) = LookupGlyphs.subst subtable

    /// <summary>The glyphs a GPOS subtable can start a match at, <see cref="LookupGlyphs.pos"/>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The covered glyphs.</returns>
    let private posSubtableGlyphs (subtable: PosSubtable) = LookupGlyphs.pos subtable

    /// <summary>Collects the glyphs the applicable subtables of a lookup cover, after those already collected.</summary>
    /// <param name="glyphsOf">Reads the glyphs of one subtable.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="collected">The glyphs so far.</param>
    /// <returns>The glyphs so far and those of the remaining subtables.</returns>
    [<TailCall>]
    let rec private collectSubtableGlyphs (glyphsOf: 'S -> ImmutableArray<uint32>) (subtables: ImmutableArray<SubtableAccel<'S>>) index (collected: ImmutableArray<uint32>) =
        match index < subtables.Length with
        | false -> collected
        | true ->
            collectSubtableGlyphs glyphsOf subtables (index + 1) (Sealed.appendSpan (collected.AsSpan()) ((glyphsOf subtables[index].Subtable).AsSpan()))

    /// <summary>Collects the glyphs the applicable subtables of a map's lookups cover.</summary>
    /// <param name="glyphsOf">Reads the glyphs of one subtable.</param>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="index">The next lookup.</param>
    /// <param name="collected">The glyphs so far.</param>
    /// <returns>Every glyph some applicable subtable covers, with repeats.</returns>
    [<TailCall>]
    let rec private collectLookupGlyphs (glyphsOf: 'S -> ImmutableArray<uint32>) (table: LayoutTable<'S>) (lookups: ImmutableArray<LookupMap>) index (collected: ImmutableArray<uint32>) =
        match index < lookups.Length with
        | false -> collected
        | true ->
            let accel = table.Lookups[lookups[index].LookupIndex].Value
            collectLookupGlyphs glyphsOf table lookups (index + 1) (collectSubtableGlyphs glyphsOf accel.Subtables 0 collected)

    /// <summary>The indices in the table of the lookups a compiled map applies, in the map's order.</summary>
    /// <param name="lookups">The compiled lookups.</param>
    /// <returns>The lookup indices.</returns>
    let private mapIndices (lookups: ImmutableArray<LookupMap>) =
        Sealed.map (fun (lookup: LookupMap) -> lookup.LookupIndex) (lookups.AsSpan())

    /// <summary>Builds the set of glyphs some GSUB subtable that can apply covers, over every lookup of a plan: a run
    /// without such a glyph cannot be touched by any GSUB lookup, the gate of <c>hb_ot_map_t::apply</c> taken exactly
    /// and for all lookups at once. A subtable whose type is not ported applies nothing and adds nothing.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The glyph set, empty when no GSUB lookup of the plan can apply.</returns>
    let gsubCovered (plan: OtShapePlan) (font: Font) =
        Sealed.unionLazySets font.Ot.GsubLookupSets.Value ((mapIndices plan.Map.GsubLookups).AsSpan())

    /// <summary>Whether a lookup index is in a compiled map.</summary>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="lookupIndex">The lookup index in the table.</param>
    /// <param name="index">The next lookup of the map.</param>
    /// <returns>True when the map applies the lookup.</returns>
    [<TailCall>]
    let rec private inMap (lookups: ImmutableArray<LookupMap>) lookupIndex index =
        match index < lookups.Length with
        | false -> false
        | true ->
            match lookups[index].LookupIndex = lookupIndex with
            | true -> true
            | false -> inMap lookups lookupIndex (index + 1)

    /// <summary>The glyph set of one GSUB lookup: the glyphs its subtables that can apply cover, empty for a lookup
    /// outside the map or a lookup whose subtables are all pending.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="lookupIndex">The lookup index in the table.</param>
    /// <returns>The glyph set.</returns>
    let private substLookupSet (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) lookupIndex =
        match inMap lookups lookupIndex 0 with
        | false -> ImmutableArray<uint64>.Empty
        | true ->
            let accel = table.Lookups[lookupIndex].Value
            Sealed.glyphSet (collectSubtableGlyphs substSubtableGlyphs accel.Subtables 0 ImmutableArray.Empty)

    /// <summary>Builds the glyph set of every GSUB lookup of a map, indexed by the lookup's index in the table: a lookup none
    /// of whose applicable subtables covers a glyph of a run cannot change it.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <returns>The glyph sets, one per lookup of the table.</returns>
    let lookupSetsOf (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) =
        Sealed.init table.Lookups.Length (fun lookupIndex -> substLookupSet table lookups lookupIndex)

    /// <summary>Builds the glyph set of every GSUB lookup of a plan, indexed by the lookup's index in the table: a lookup none
    /// of whose applicable subtables covers a glyph of a run cannot change it.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The glyph sets, one per lookup of the table.</returns>
    let gsubLookupCovered (plan: OtShapePlan) (font: Font) =
        Sealed.chosenLazySets font.Ot.GsubLookupSets.Value ((mapIndices plan.Map.GsubLookups).AsSpan())

    /// <summary>The glyphs the subtables of the lookup at a position of the map cover.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="position">The position in the map.</param>
    /// <returns>The covered glyphs, with repeats.</returns>
    let private positionGlyphs (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) position =
        let accel = table.Lookups[lookups[position].LookupIndex].Value
        collectSubtableGlyphs substSubtableGlyphs accel.Subtables 0 ImmutableArray.Empty

    /// <summary>Builds, for each glyph, a bit per position among the first 64 of a map's lookups whose subtables cover it, so
    /// one pass over a run says which lookups can apply to it.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <returns>The words indexed by glyph.</returns>
    let glyphLookupsOf (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) =
        Sealed.glyphLookupTable (Sealed.init (min lookups.Length 64) (fun position -> positionGlyphs table lookups position))

    /// <summary>The table of the lookups that cover each glyph for the lookups of a map, built from the glyph sets of the face the first
    /// time a map with those lookups asks and kept with the face as one of the newest few: the table is a pure function of the face's
    /// tables and of the lookups, so a plan made again for the same script and features finds it instead of filling a word for each glyph
    /// of the largest glyph its lookups cover again (350 KB for a CJK face).</summary>
    /// <param name="font">The font.</param>
    /// <param name="positioning">Whether the lookups are those of GPOS.</param>
    /// <param name="sets">The lazily built glyph set of every lookup of the table.</param>
    /// <param name="indices">The indices of the lookups of the map, in the map's order.</param>
    /// <returns>The words indexed by glyph.</returns>
    let private glyphLookupsKept (font: Font) positioning sets (indices: ImmutableArray<int>) =
        let key = { Positioning = positioning; Lookups = indices }
        match font.Ot.GlyphLookupTables.TryFind key with
        | ValueSome kept -> kept
        | ValueNone -> font.Ot.GlyphLookupTables.Add (key, Sealed.glyphLookupTableOfLazySets sets (indices.AsSpan()))

    /// <summary>Builds, for each glyph, a bit per position among the first 64 of the plan's lookups whose subtables cover it,
    /// so one pass over a run says which lookups can apply to it.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The words indexed by glyph.</returns>
    let gsubGlyphLookups (plan: OtShapePlan) (font: Font) =
        glyphLookupsKept font false font.Ot.GsubLookupSets.Value (mapIndices plan.Map.GsubLookups)

    /// <summary>Builds the set of glyphs some GPOS subtable that can apply covers, over every lookup of a plan.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The glyph set, empty when no GPOS lookup of the plan can apply.</returns>
    let gposCovered (plan: OtShapePlan) (font: Font) =
        Sealed.unionLazySets font.Ot.GposLookupSets.Value ((mapIndices plan.Map.GposLookups).AsSpan())

    /// <summary>Applies one GSUB lookup through an applier, without boxing the struct.</summary>
    /// <param name="applier">The applier.</param>
    /// <param name="table">The table.</param>
    /// <param name="lookupIndex">The lookup index.</param>
    /// <param name="lookupMap">The compiled lookup.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run.</returns>
    let private applyThrough<'A when 'A : struct and 'A :> ITableApplier<SubstSubtable>> (applier: 'A) (table: LayoutTable<SubstSubtable>) lookupIndex lookupMap font run =
        applier.Apply (table, lookupIndex, lookupMap, font, run)

    /// <summary>Runs the GSUB pass through the edit interpreter, one run pass per lookup, <c>hb_ot_map_t::substitute</c>:
    /// the reference the lookup kernel is proved against.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substituteReference (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        applyTable true (GsubApplier()) font.Ot.Gsub.Value plan.Map.GsubLookups plan.Map.GsubStages font run

    /// <summary>The GSUB table applier that skips a lookup none of whose applicable subtables covers a glyph of the run:
    /// such a lookup returns its input run unchanged through <see cref="GsubApplier"/> too, after stepping through every
    /// glyph its digest lets by.</summary>
    [<Struct>]
    type GsubApplicableApplier =
        val Covered: ImmutableArray<ImmutableArray<uint64>>

        /// <summary>Creates the applier.</summary>
        /// <param name="covered">The glyph set of every lookup of the table.</param>
        new (covered) = { Covered = covered }

        interface ITableApplier<SubstSubtable> with
            member this.Apply (table, lookupIndex, lookupMap, font, run) =
                match Sealed.anyGlyphIn this.Covered[lookupIndex] (run.Infos.AsSpan()) with
                | false -> run
                | true -> applyThrough (GsubApplier()) table lookupIndex lookupMap font run

    /// <summary>Runs the GSUB pass over the lookups that can apply through the edit interpreter, the reference of
    /// <see cref="substituteApplicable"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substituteApplicableReference (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (run: GlyphRun) =
        applyTable true (GsubApplicableApplier(covered)) font.Ot.Gsub.Value plan.Map.GsubLookups plan.Map.GsubStages font run

    /// <summary>The position after the last lookup a map's stages apply.</summary>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="index">The next stage.</param>
    /// <param name="last">The largest end so far.</param>
    /// <returns>The number of lookups the stages apply.</returns>
    [<TailCall>]
    let rec private lookupEndOf (stages: ImmutableArray<StageMap>) index last =
        match index < stages.Length with
        | false -> last
        | true -> lookupEndOf stages (index + 1) (max last stages[index].LastLookup)

    /// <summary>Whether two passes hold the same lookup: the same objects, flags, digest and matchers.</summary>
    /// <param name="first">The first pass.</param>
    /// <param name="second">The second pass.</param>
    /// <returns>True when every field is the same.</returns>
    let private sameLookup (first: GsubPass) (second: GsubPass) =
        obj.ReferenceEquals(first.Gdef, second.Gdef)
        && first.HasGlyphClasses = second.HasGlyphClasses
        && first.LookupMask = second.LookupMask
        && first.LookupProps = second.LookupProps
        && first.LookupDigest = second.LookupDigest
        && first.Random = second.Random
        && first.AutoZwnj = second.AutoZwnj
        && first.AutoZwj = second.AutoZwj
        && first.PerSyllable = second.PerSyllable
        && first.Subtables.Equals second.Subtables
        && first.ContextLookup = second.ContextLookup
        && first.InputMatcher = second.InputMatcher
        && first.ContextMatcher = second.ContextMatcher
        && first.Covered.Equals second.Covered
        && first.ExactScan = second.ExactScan
        && first.Chains.Equals second.Chains
        && first.ShareDepths = second.ShareDepths

    /// <summary>Whether every sequence of a multiple substitution has a glyph.</summary>
    /// <param name="sequences">The sequences.</param>
    /// <param name="index">The next sequence.</param>
    /// <returns>True when none is empty.</returns>
    [<TailCall>]
    let rec private noEmptySequence (sequences: ImmutableArray<ImmutableArray<uint32>>) index =
        match index < sequences.Length with
        | false -> true
        | true ->
            match sequences[index].IsEmpty with
            | true -> false
            | false -> noEmptySequence sequences (index + 1)

    /// <summary>Whether every subtable of a lookup is a single substitution or a multiple substitution with no empty sequence, the
    /// lookups whose effect on a glyph depends on that glyph alone.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when the lookup decides on one glyph alone.</returns>
    [<TailCall>]
    let rec private onlyPerGlyph (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) index =
        match index < subtables.Length with
        | false -> true
        | true ->
            match subtables[index].Subtable with
            | SingleDelta _ | SingleList _ -> onlyPerGlyph subtables (index + 1)
            | Multiple (_, sequences) ->
                match noEmptySequence sequences 0 with
                | true -> onlyPerGlyph subtables (index + 1)
                | false -> false
            | Alternate _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> false

    /// <summary>What the subtables of a lookup that decides on one glyph alone do to a glyph, <see cref="trySubtables"/> for such a lookup.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="subtable">The next subtable.</param>
    /// <param name="info">The glyph.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The decision.</returns>
    [<TailCall>]
    let rec private stepSubtables gdef hasGlyphClasses (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) subtable (info: GlyphInfo) (props: GlyphProps) =
        match subtable < subtables.Length with
        | false -> PerGlyphStep(PerGlyphKind.Miss, 0u, props, ImmutableArray.Empty)
        | true ->
            let accel = subtables[subtable]
            match Digest.mayHave info.Codepoint accel.SubtableDigest with
            | false -> stepSubtables gdef hasGlyphClasses subtables (subtable + 1) info props
            | true ->
                match GsubApply.trySimple gdef hasGlyphClasses accel.Subtable info props with
                | GsubMatch.GsubMiss -> stepSubtables gdef hasGlyphClasses subtables (subtable + 1) info props
                | GsubMatch.GsubReplace (replaced, replacedProps) -> PerGlyphStep(PerGlyphKind.Replace, replaced.Codepoint, replacedProps, ImmutableArray.Empty)
                | GsubMatch.GsubMultiple (_, components, _, _, _) -> PerGlyphStep(PerGlyphKind.Multiple, 0u, props, components)
                | GsubMatch.GsubDelete | GsubMatch.GsubAlternateRandom _ | GsubMatch.GsubLigature _ -> PerGlyphStep(PerGlyphKind.Refused, 0u, props, ImmutableArray.Empty)

    /// <summary>What a lookup that decides on one glyph alone does to a glyph that has passed the lookup's gate: read from the table of
    /// decisions when the lookup has one, asked of the subtables when not.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="decisions">The table of decisions, empty for none.</param>
    /// <param name="sequences">The sequences the table's multiple substitutions point to.</param>
    /// <param name="info">The glyph.</param>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>The decision.</returns>
    let private stepDecided gdef hasGlyphClasses subtables (decisions: int array) (sequences: ImmutableArray<uint32> array) (info: GlyphInfo) (props: GlyphProps) =
        let miss = PerGlyphStep(PerGlyphKind.Miss, 0u, props, ImmutableArray.Empty)
        let decision =
            match decisions.Length with
            | 0 -> ValueNone
            | length ->
                match info.Codepoint < uint32 length with
                | true -> ValueSome decisions[int info.Codepoint]
                | false -> ValueSome 0
        match decision with
        | ValueNone -> stepSubtables gdef hasGlyphClasses subtables 0 info props
        | ValueSome 0 -> miss
        | ValueSome Sealed.PerGlyphRefused -> PerGlyphStep(PerGlyphKind.Refused, 0u, props, ImmutableArray.Empty)
        | ValueSome value when value > 0 ->
            let glyph = uint32 (value - 1)
            PerGlyphStep(PerGlyphKind.Replace, glyph, Matching.setGlyphClass gdef hasGlyphClasses glyph 0us false false props, ImmutableArray.Empty)
        | ValueSome value -> PerGlyphStep(PerGlyphKind.Multiple, 0u, props, sequences[-value - 1])

    /// <summary>A GSUB lookup that decides on one glyph alone, as the fused chain of the kernel applies it.</summary>
    [<Sealed>]
    type private PerGlyphLookup =
        val Gdef: Gdef
        val HasGlyphClasses: bool
        val LookupMask: uint32
        val LookupProps: uint32
        val Subtables: ImmutableArray<SubtableAccel<SubstSubtable>>
        val Decisions: int array
        val Sequences: ImmutableArray<uint32> array
        val GrowthOf: int

        /// <summary>Keeps what the lookup reads of a glyph, and the decisions its subtables come to for every glyph it covers.</summary>
        /// <param name="gdef">The GDEF.</param>
        /// <param name="hasGlyphClasses">Whether GDEF glyph classes are present.</param>
        /// <param name="lookupMask">The lookup mask.</param>
        /// <param name="lookupProps">The sanitised lookup props.</param>
        /// <param name="subtables">The lookup's subtables.</param>
        /// <param name="decisions">The table of decisions, empty when the subtables are asked.</param>
        /// <param name="sequences">The sequences the table's multiple substitutions point to.</param>
        new (gdef, hasGlyphClasses, lookupMask, lookupProps, subtables, decisions, sequences) =
            { Gdef = gdef
              HasGlyphClasses = hasGlyphClasses
              LookupMask = lookupMask
              LookupProps = lookupProps
              Subtables = subtables
              Decisions = decisions
              Sequences = sequences
              GrowthOf = (match decisions.Length > 0 && not (Sealed.anyPerGlyphRefused decisions) with | true -> Sealed.longestSequence sequences | false -> 0) }

        interface IPerGlyph with
            member this.Step (info, props) =
                match info.Mask &&& this.LookupMask <> 0u && Matching.checkGlyphProperty this.Gdef info.Codepoint props.GlyphClass this.LookupProps with
                | false -> PerGlyphStep(PerGlyphKind.Miss, 0u, props, ImmutableArray.Empty)
                | true -> stepDecided this.Gdef this.HasGlyphClasses this.Subtables this.Decisions this.Sequences info props

            member this.ComponentProps (baseProps, glyph, index) =
                let klass = match GlyphPropsBits.isLigature baseProps with | true -> GlyphPropsBits.Base | false -> 0us
                multipleComponentProps this.Gdef this.HasGlyphClasses klass baseProps (GlyphPropsBits.ligId baseProps) glyph index

            member this.Growth () = this.GrowthOf

    /// <summary>The slot of a lookup in the plan's record of lookups that decide on one glyph alone: the lookup, or none.</summary>
    [<Sealed>]
    type private PerGlyphSlot =
        val Lookup: IPerGlyph voption

        /// <summary>Keeps the answer for one position.</summary>
        /// <param name="lookup">The lookup, or none.</param>
        new (lookup) = { Lookup = lookup }

    /// <summary>One lookup of a plan kept ready for the kernel: the two passes the program builds for it (the one that walks the covered
    /// glyphs and the one for a run that shows a lookup that changes nothing) held where they are handed out by reference, and the facts
    /// <c>Admits</c> reads, so that entering an applied lookup is an array index and admitting a visited one reads one object.</summary>
    [<Sealed>]
    type private GsubView =
        val Exact: Cell<GsubPass>
        val Shown: Cell<GsubPass>
        val LookupDigest: Digest
        val SecondDigest: Digest
        val Mask: uint32
        val IsReverse: bool
        val LookupIndex: int
        val Props: uint32

        /// <summary>Keeps the two passes and the admission facts of a lookup.</summary>
        /// <param name="exact">The pass for a run that does not show a lookup that changes nothing.</param>
        /// <param name="shown">The pass for a run that does.</param>
        /// <param name="lookupDigest">The digest of the lookup's first glyphs.</param>
        /// <param name="secondDigest">The digest of the lookup's second glyphs.</param>
        /// <param name="mask">The lookup mask.</param>
        /// <param name="isReverse">Whether the lookup is applied backward.</param>
        /// <param name="lookupIndex">The lookup's index in the table.</param>
        /// <param name="props">The lookup's props as the table gives them.</param>
        new (exact, shown, lookupDigest, secondDigest, mask, isReverse, lookupIndex, props) =
            { Exact = exact
              Shown = shown
              LookupDigest = lookupDigest
              SecondDigest = secondDigest
              Mask = mask
              IsReverse = isReverse
              LookupIndex = lookupIndex
              Props = props }

    /// <summary>The lookups of one plan's map kept ready, with the objects they were built from, so that a program built from other objects
    /// does not take them. Made when a program over the plan's lookups first asks for it and kept for as long as the plan's lookup array is
    /// alive; each lookup is built the first time it is visited, so no lookup the plan never visits is built at all.</summary>
    [<Sealed>]
    type private GsubViews =
        val Lookups: ImmutableArray<LookupMap>
        val Table: LayoutTable<SubstSubtable>
        val Covered: ImmutableArray<ImmutableArray<uint64>>
        val Chains: ImmutableArray<Lazy<ImmutableArray<ChainSets voption>>>
        val Gdef: Gdef
        val HasCovered: bool
        val Slots: SlotArray<GsubView>
        val PerGlyph: SlotArray<PerGlyphSlot>

        /// <summary>Keeps the objects the lookups are built from and the empty slots.</summary>
        /// <param name="lookups">The compiled lookups of the map or window.</param>
        /// <param name="table">The table.</param>
        /// <param name="covered">The glyph set of every lookup of the table.</param>
        /// <param name="chains">The dense sets of every lookup of the table.</param>
        /// <param name="gdef">The GDEF.</param>
        /// <param name="hasCovered">Whether a lookup is walked over the glyphs its subtables cover.</param>
        /// <param name="slots">One empty slot for each lookup of the map.</param>
        /// <param name="perGlyph">One empty slot for each lookup of the map, for its answer as a lookup that decides on one glyph alone.</param>
        new (lookups, table, covered, chains, gdef, hasCovered, slots, perGlyph) =
            { Lookups = lookups
              Table = table
              Covered = covered
              Chains = chains
              Gdef = gdef
              HasCovered = hasCovered
              Slots = slots
              PerGlyph = perGlyph }

    /// <summary>The lookups of a GSUB table as the lookup kernel applies them, <c>hb_ot_map_t::apply</c>.</summary>
    [<Struct>]
    type private GsubProgram =
        val Gate: bool
        val HasCovered: bool
        val Covered: ImmutableArray<ImmutableArray<uint64>>
        val Table: LayoutTable<SubstSubtable>
        val Lookups: ImmutableArray<LookupMap>
        val GlyphLookupTable: ImmutableArray<uint64>
        val End: int
        val Gdef: Gdef
        val HasGlyphClasses: bool
        val Chains: ImmutableArray<Lazy<ImmutableArray<ChainSets voption>>>
        val Views: GsubViews voption

        /// <summary>Creates the program.</summary>
        /// <param name="gate">Whether digest gating is on.</param>
        /// <param name="hasCovered">Whether a lookup whose subtables cover no glyph of the run is skipped.</param>
        /// <param name="covered">The glyph set of every lookup of the table.</param>
        /// <param name="glyphLookups">The positions of the first 64 lookups that cover each glyph.</param>
        /// <param name="table">The table.</param>
        /// <param name="lookups">The compiled lookups.</param>
        /// <param name="stages">The compiled stages.</param>
        /// <param name="gdef">The GDEF.</param>
        new (gate, hasCovered, covered, glyphLookups, table, lookups, stages, gdef) =
            { Gate = gate
              HasCovered = hasCovered
              Covered = covered
              Table = table
              Lookups = lookups
              GlyphLookupTable = glyphLookups
              End = lookupEndOf stages 0 0
              Gdef = gdef
              HasGlyphClasses = Gdef.hasGlyphClasses gdef
              Chains = ImmutableArray<Lazy<ImmutableArray<ChainSets voption>>>.Empty
              Views = ValueNone }

        /// <summary>Creates the program with the dense sets of the chained-context subtables of the table's lookups.</summary>
        /// <param name="gate">Whether digest gating is on.</param>
        /// <param name="hasCovered">Whether a lookup whose subtables cover no glyph of the run is skipped.</param>
        /// <param name="covered">The glyph set of every lookup of the table.</param>
        /// <param name="glyphLookups">The positions of the first 64 lookups that cover each glyph.</param>
        /// <param name="table">The table.</param>
        /// <param name="lookups">The compiled lookups.</param>
        /// <param name="stages">The compiled stages.</param>
        /// <param name="gdef">The GDEF.</param>
        /// <param name="chains">The dense sets of every lookup of the table, built on first use.</param>
        new (gate, hasCovered, covered, glyphLookups, table, lookups, stages, gdef, chains) =
            { Gate = gate
              HasCovered = hasCovered
              Covered = covered
              Table = table
              Lookups = lookups
              GlyphLookupTable = glyphLookups
              End = lookupEndOf stages 0 0
              Gdef = gdef
              HasGlyphClasses = Gdef.hasGlyphClasses gdef
              Chains = chains
              Views = ValueNone }

        /// <summary>Creates the program of another, with the lookups of its plan kept ready.</summary>
        /// <param name="program">The program.</param>
        /// <param name="views">The lookups of the plan, kept ready, built from the objects the program holds.</param>
        new (program: GsubProgram, views) =
            { Gate = program.Gate
              HasCovered = program.HasCovered
              Covered = program.Covered
              Table = program.Table
              Lookups = program.Lookups
              GlyphLookupTable = program.GlyphLookupTable
              End = program.End
              Gdef = program.Gdef
              HasGlyphClasses = program.HasGlyphClasses
              Chains = program.Chains
              Views = ValueSome views }

        /// <summary>Builds the lookup at a position of the map, the pass the kernel applies.</summary>
        /// <param name="index">The lookup's position in the map.</param>
        /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run.</param>
        /// <returns>The pass.</returns>
        member this.Build (index, shown) =
            let lookupMap = this.Lookups[index]
            let accel = this.Table.Lookups[lookupMap.LookupIndex].Value
            let exact = this.HasCovered && not shown
            GsubPass(this.Gdef, this.HasGlyphClasses, lookupMap.Mask, Gdef.sanitizeLookupProps this.Gdef accel.Props, accel.LookupDigest, lookupMap.Random, accel.Subtables, lookupMap.AutoZwnj, lookupMap.AutoZwj, lookupMap.PerSyllable, (if exact then this.Covered[lookupMap.LookupIndex] else ImmutableArray.Empty), exact, (match this.Chains.IsEmpty with | true -> ImmutableArray<ChainSets voption>.Empty | false -> this.Chains[lookupMap.LookupIndex].Value))

        /// <summary>The lookup at a position of the map kept ready, built the first time it is asked for.</summary>
        /// <param name="views">The lookups of the plan.</param>
        /// <param name="index">The lookup's position in the map.</param>
        /// <returns>The lookup kept ready.</returns>
        member this.ViewOf (views: GsubViews, index) =
            match views.Slots.TryGet index with
            | ValueSome view -> view
            | ValueNone ->
                let lookupMap = this.Lookups[index]
                let accel = this.Table.Lookups[lookupMap.LookupIndex].Value
                views.Slots.Set(index, GsubView(Cell(this.Build(index, false)), Cell(this.Build(index, true)), accel.LookupDigest, accel.SecondDigest, lookupMap.Mask, accel.IsReverse, lookupMap.LookupIndex, accel.Props))

        /// <summary>The lookup at a position of the map as a lookup that decides on one glyph alone.</summary>
        /// <param name="index">The lookup's position in the map.</param>
        /// <returns>The lookup, or none when it is reversed or holds another kind of subtable.</returns>
        member this.PerGlyphOf (index) =
            let lookupMap = this.Lookups[index]
            let accel = this.Table.Lookups[lookupMap.LookupIndex].Value
            match accel.IsReverse || not (onlyPerGlyph accel.Subtables 0) with
            | true -> ValueNone
            | false ->
                let gdef = this.Gdef
                let hasGlyphClasses = this.HasGlyphClasses
                let lookupProps = Gdef.sanitizeLookupProps gdef accel.Props
                let struct (decisions, sequences) =
                    match this.HasCovered && lookupMap.LookupIndex < this.Covered.Length with
                    | true -> Sealed.perGlyphDecisions this.Covered[lookupMap.LookupIndex] (fun glyph -> stepSubtables gdef hasGlyphClasses accel.Subtables 0 { Codepoint = glyph; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u } Unchecked.defaultof<GlyphProps>)
                    | false -> struct (Array.empty<int>, Array.empty<ImmutableArray<uint32>>)
                ValueSome (PerGlyphLookup(gdef, hasGlyphClasses, lookupMap.Mask, lookupProps, accel.Subtables, decisions, sequences) :> IPerGlyph)

        /// <summary>The covered-glyph gate of <see cref="IGsubProgram{L}.Admits"/>: whether the lookup's subtables cover some glyph of the run, when
        /// the run would show it.</summary>
        /// <param name="isReverse">Whether the lookup is applied backward.</param>
        /// <param name="lookupIndex">The lookup's index in the table.</param>
        /// <param name="mask">The lookup mask.</param>
        /// <param name="lookupProps">The lookup's props as the table gives them.</param>
        /// <param name="index">The lookup's position in the map.</param>
        /// <param name="candidates">The lookups of the first 64 positions that cover some glyph of the run.</param>
        /// <param name="infos">The glyph infos.</param>
        /// <param name="props">The glyph properties.</param>
        /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run.</param>
        /// <returns>True when the gate lets the lookup through.</returns>
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.CoveredGate (isReverse, lookupIndex, mask: uint32, lookupProps, index, candidates, infos: ReadOnlySpan<GlyphInfo>, props: ReadOnlySpan<GlyphProps>, shown) =
            match isReverse with
            | true -> true
            | false ->
                (shown || not this.HasCovered || (match index < 64 && not this.GlyphLookupTable.IsEmpty with | true -> (candidates >>> index) &&& 1UL <> 0UL | false -> scanCovered this.Covered[lookupIndex] mask (Gdef.sanitizeLookupProps this.Gdef lookupProps) this.Gdef infos props 0 < infos.Length))
                && ((not this.HasCovered) || (not shown) || Sealed.anyGlyphIn this.Covered[lookupIndex] infos)

        interface IGsubProgram<GsubPass> with
            member this.LookupEnd = this.End

            member this.GlyphLookups = this.GlyphLookupTable

            member this.Admits (index, digest, candidates, flags, infos, props, shown) =
                let produceUnsafeToConcat = flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default
                match this.Views with
                | ValueSome views ->
                    let view = this.ViewOf(views, index)
                    (not this.Gate || (Digest.mayIntersect view.LookupDigest digest && (produceUnsafeToConcat || Digest.mayIntersect view.SecondDigest digest)))
                    && infos.Length > 0
                    && view.Mask <> 0u
                    && this.CoveredGate(view.IsReverse, view.LookupIndex, view.Mask, view.Props, index, candidates, infos, props, shown)
                | ValueNone ->
                    let lookupMap = this.Lookups[index]
                    let accel = this.Table.Lookups[lookupMap.LookupIndex].Value
                    (not this.Gate || (Digest.mayIntersect accel.LookupDigest digest && (produceUnsafeToConcat || Digest.mayIntersect accel.SecondDigest digest)))
                    && infos.Length > 0
                    && lookupMap.Mask <> 0u
                    && this.CoveredGate(accel.IsReverse, lookupMap.LookupIndex, lookupMap.Mask, accel.Props, index, candidates, infos, props, shown)

            member this.Lookup (index, shown) = this.Build(index, shown)

            member this.Prepared = this.Views.IsSome

            member this.LookupAt (index, shown) =
                match this.Views with
                | ValueSome views ->
                    let view = this.ViewOf(views, index)
                    match shown with
                    | false -> view.Exact.Item
                    | true -> view.Shown.Item
                | ValueNone -> this.Build(index, shown)

            member this.PerGlyphAt (index) =
                match this.Views with
                | ValueNone -> ValueNone
                | ValueSome views ->
                    match views.PerGlyph.TryGet index with
                    | ValueSome slot -> slot.Lookup
                    | ValueNone -> (views.PerGlyph.Set(index, PerGlyphSlot(this.PerGlyphOf index))).Lookup

            member this.IsReverse (index) =
                match this.Views with
                | ValueSome views -> (this.ViewOf(views, index)).IsReverse
                | ValueNone ->
                    let lookupMap = this.Lookups[index]
                    this.Table.Lookups[lookupMap.LookupIndex].Value.IsReverse

            member this.NestedLookup (lookupIndex, mask, random, autoZwnj, autoZwj, perSyllable) =
                match lookupIndex < this.Table.Lookups.Length with
                | false -> GsubPass(this.Gdef, this.HasGlyphClasses, mask, 0u, Digest.empty, random, ImmutableArray.Empty, autoZwnj, autoZwj, perSyllable, ImmutableArray.Empty, false)
                | true ->
                    let accel = this.Table.Lookups[lookupIndex].Value
                    GsubPass(this.Gdef, this.HasGlyphClasses, mask, Gdef.sanitizeLookupProps this.Gdef accel.Props, accel.LookupDigest, random, accel.Subtables, autoZwnj, autoZwj, perSyllable, ImmutableArray.Empty, false, (match this.Chains.IsEmpty with | true -> ImmutableArray<ChainSets voption>.Empty | false -> this.Chains[lookupIndex].Value))

    /// <summary>Whether lookups kept ready were built from the objects a program holds.</summary>
    /// <param name="views">The lookups kept ready.</param>
    /// <param name="program">The program.</param>
    /// <returns>True when the program would build the same lookups.</returns>
    let private viewsServe (views: GsubViews) (program: GsubProgram) =
        views.Lookups.Equals program.Lookups
        && obj.ReferenceEquals(views.Table, program.Table)
        && obj.ReferenceEquals(views.Gdef, program.Gdef)
        && views.HasCovered = program.HasCovered
        && views.Covered.Equals program.Covered
        && views.Chains.Equals program.Chains

    /// <summary>The program with the lookups of its plan kept ready: its lookups are built once, when first visited, and handed out by
    /// reference afterwards. The lookups are kept in the cell of the plan's map or of the window, made the first time a line applies them and
    /// alive as long as the plan; nothing invalidates them, because they are pure functions of the objects they record. A program whose
    /// objects differ from those the kept lookups were built from, or that has no lookups, is returned as it is.</summary>
    /// <param name="cell">The cell of the map or window the program's lookups belong to.</param>
    /// <param name="program">The program.</param>
    /// <returns>The program that keeps its lookups ready.</returns>
    let private prepared (cell: Memo<obj>) (program: GsubProgram) =
        match program.Lookups.IsEmpty with
        | true -> program
        | false ->
            match cell.TryGet() with
            | ValueSome (:? GsubViews as views) when viewsServe views program -> GsubProgram(program, views)
            | ValueSome _ -> program
            | ValueNone ->
                let created = GsubViews(program.Lookups, program.Table, program.Covered, program.Chains, program.Gdef, program.HasCovered, SlotArray<GsubView>(program.Lookups.Length), SlotArray<PerGlyphSlot>(program.Lookups.Length))
                match cell.Set created with
                | :? GsubViews as kept when viewsServe kept program -> GsubProgram(program, kept)
                | _ -> program

    /// <summary>Applies the lookups of a GSUB table to a run on one working copy of its arrays, <c>hb_ot_map_t::apply</c>:
    /// each lookup is a loop over the run that writes through the machine, with no edit and no step.</summary>
    /// <param name="gate">Whether digest gating is on.</param>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every lookup, the same instance when nothing changed.</returns>
    let applyGsub (gate: bool) (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (font: Font) (run: GlyphRun) =
        GlyphRun.runKernel (GsubProgram(gate, false, ImmutableArray.Empty, ImmutableArray.Empty, table, lookups, stages, font.Ot.Gdef.Value)) run

    /// <summary>Applies the lookups of a window of a plan's map, the lookups between two pauses, as <see cref="applyGsub"/> does, but with
    /// the glyph set of every lookup and the dense sets of the chained contexts, as <see cref="substituteApplicable"/> does for a plan with
    /// no pause: a lookup is walked only over the glyphs its subtables cover. The glyph sets are indexed by the lookup of the table, so a
    /// window whose lookups were cut out of the map reads them as the whole map would.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The lookups of the window.</param>
    /// <param name="stages">The stage of the window.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every lookup of the window, the same instance when nothing changed.</returns>
    let applyGsubWindowCovered (covered: ImmutableArray<ImmutableArray<uint64>>) (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (font: Font) (run: GlyphRun) =
        GlyphRun.runKernel (GsubProgram(true, true, covered, ImmutableArray.Empty, table, lookups, stages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value)) run

    /// <summary>Applies the lookups of a GSUB table to a run through the kernel, the way a plan does: with the glyph sets of
    /// its lookups, so a lookup is walked only over glyphs its subtables cover.</summary>
    /// <param name="gate">Whether digest gating is on.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="lookupSetsOf"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="glyphLookupsOf"/>.</param>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every lookup, the same instance when nothing changed.</returns>
    let applyGsubCovered (gate: bool) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (font: Font) (run: GlyphRun) =
        GlyphRun.runKernel (GsubProgram(gate, true, covered, glyphLookups, table, lookups, stages, font.Ot.Gdef.Value)) run

    /// <summary>Applies the lookups of a GSUB table to a run through the kernel with the glyph sets of its lookups, given a
    /// look at the run's glyphs made over the same glyphs, <see cref="lookAt"/>.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="lookupSetsOf"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="glyphLookupsOf"/>.</param>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="font">The font.</param>
    /// <param name="admission">The look at the glyphs of the run.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every lookup, the same instance when nothing changed.</returns>
    let applyGsubCoveredAdmitted (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (font: Font) (admission: RunAdmission) (run: GlyphRun) =
        GlyphRun.runKernelAdmitted (GsubProgram(true, true, covered, glyphLookups, table, lookups, stages, font.Ot.Gdef.Value)) admission run

    /// <summary>Runs the GSUB pass, <c>hb_ot_map_t::substitute</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substitute (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        applyGsub true font.Ot.Gsub.Value plan.Map.GsubLookups plan.Map.GsubStages font run

    /// <summary>Runs the GSUB pass over the lookups that can apply, <c>hb_ot_map_t::substitute</c> without the lookups
    /// whose subtables are all pending: it returns the input run itself exactly when <see cref="substitute"/> does.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gsubGlyphLookups"/>.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substituteApplicable (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (run: GlyphRun) =
        GlyphRun.runKernel (GsubProgram(true, true, covered, glyphLookups, font.Ot.Gsub.Value, plan.Map.GsubLookups, plan.Map.GsubStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value)) run

    /// <summary>Whether a lookup among those at the positions of a candidate set passes the gates of
    /// <c>hb_ot_map_t::apply</c> on a run whose digest and mask are given.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="digest">The digest of the run.</param>
    /// <param name="produceUnsafeToConcat">Whether the buffer produces unsafe-to-concat flags.</param>
    /// <param name="runMask">The mask every glyph of the run carries.</param>
    /// <param name="candidates">The positions to try, one bit each.</param>
    /// <returns>True when some lookup passes.</returns>
    [<TailCall>]
    let rec private anyPositionAdmitted (table: LayoutTable<SubstSubtable>) (lookups: ImmutableArray<LookupMap>) (digest: Digest) (produceUnsafeToConcat: bool) (runMask: uint32) (candidates: uint64) =
        match candidates = 0UL with
        | true -> false
        | false ->
            let lookupMap = lookups[BitOperations.TrailingZeroCount candidates]
            let accel = table.Lookups[lookupMap.LookupIndex].Value
            let admitted =
                lookupMap.Mask &&& runMask <> 0u
                && Digest.mayIntersect accel.LookupDigest digest
                && (produceUnsafeToConcat || Digest.mayIntersect accel.SecondDigest digest)
            match admitted with
            | true -> true
            | false -> anyPositionAdmitted table lookups digest produceUnsafeToConcat runMask (candidates &&& (candidates - 1UL))

    /// <summary>The look at the glyphs of a run that the kernel makes before it applies a lookup: the lookups that cover a
    /// glyph of the run and the digest of its glyphs.</summary>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph.</param>
    /// <param name="infos">The glyphs.</param>
    /// <returns>The look, unknown when there is no table to look at.</returns>
    let lookAt (glyphLookups: ImmutableArray<uint64>) (infos: ReadOnlySpan<GlyphInfo>) =
        match glyphLookups.IsEmpty with
        | true -> { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty }
        | false -> { Known = true; Candidates = Sealed.lookupsCovering glyphLookups infos; GlyphDigest = Digest.ofSpan infos }

    /// <summary>What a look at a run all of whose glyphs carry one mask says about the lookups of the plan: whether one of
    /// them can apply, the lookups that cover a glyph of the run, and the digest of its glyphs. A run for which no lookup
    /// can apply comes out of <see cref="substituteApplicable"/> as it went in. The look is made once: the kernel is given
    /// its result by <see cref="substituteAdmitted"/>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gsubGlyphLookups"/>.</param>
    /// <param name="runMask">The mask every glyph of the run carries.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="infos">The glyphs.</param>
    /// <returns>Whether a lookup may apply and the look at the glyphs; true with an unknown look, without looking, when the
    /// plan holds more than 64 lookups or no table.</returns>
    let admit (plan: OtShapePlan) (font: Font) (glyphLookups: ImmutableArray<uint64>) (runMask: uint32) (flags: BufferFlags) (infos: ReadOnlySpan<GlyphInfo>) =
        match glyphLookups.IsEmpty || plan.Map.GsubLookups.Length > 64 with
        | true -> struct (true, { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty })
        | false ->
            let struct (candidates, digest) = Sealed.lookupsAndDigest glyphLookups infos
            match candidates with
            | 0UL -> struct (false, { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty })
            | _ ->
                let applies = anyPositionAdmitted font.Ot.Gsub.Value plan.Map.GsubLookups digest (flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default) runMask candidates
                struct (applies, { Known = true; Candidates = candidates; GlyphDigest = digest })

    /// <summary>Whether some lookup of the plan can apply to a run all of whose glyphs carry one mask: a glyph some lookup
    /// covers stands in the run, the lookup's mask meets the run's, and the digest gates let it through. A run for which it
    /// is false comes out of <see cref="substituteApplicable"/> as it went in.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gsubGlyphLookups"/>.</param>
    /// <param name="runMask">The mask every glyph of the run carries.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="infos">The glyphs.</param>
    /// <returns>True when a lookup may apply; true without looking when the plan holds more than 64 lookups or no table.</returns>
    let mayApply (plan: OtShapePlan) (font: Font) (glyphLookups: ImmutableArray<uint64>) (runMask: uint32) (flags: BufferFlags) (infos: ReadOnlySpan<GlyphInfo>) =
        let struct (applies, _) = admit plan font glyphLookups runMask flags infos
        applies

    /// <summary>Runs the GSUB pass over the lookups that can apply, as <see cref="substituteApplicable"/> does, given the
    /// look at the run's glyphs that <see cref="admit"/> made over the same glyphs.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gsubGlyphLookups"/>.</param>
    /// <param name="admission">The look at the glyphs of the run.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run, the input run itself when nothing changed.</returns>
    let substituteAdmitted (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (admission: RunAdmission) (run: GlyphRun) =
        applyGsubCoveredAdmitted covered glyphLookups font.Ot.Gsub.Value plan.Map.GsubLookups plan.Map.GsubStages font admission run

    /// <summary>Runs the GSUB pass as <see cref="substituteAdmitted"/> does, on a line held in the first elements of arrays a worker
    /// owns and shares with no one: the lookups edit those arrays where they are, so a line they shrink or leave alone costs no copy.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gsubGlyphLookups"/>.</param>
    /// <param name="admission">The look at the glyphs of the line.</param>
    /// <param name="input">The run fields the pass reads, without glyphs, properties or positions.</param>
    /// <param name="infos">The worker's glyph array, holding the line's glyphs.</param>
    /// <param name="props">The worker's property array, holding the line's properties.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <returns>Where the glyphs are afterwards and the state of the run.</returns>
    let substituteOwned (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (admission: RunAdmission) (input: RunInput) (infos: GlyphInfo array) (props: GlyphProps array) (count: int) =
        Sealed.runGsubOwned input (GsubProgram(true, true, covered, glyphLookups, font.Ot.Gsub.Value, plan.Map.GsubLookups, plan.Map.GsubStages, font.Ot.Gdef.Value)) admission infos props count

    /// <summary>Tries the subtables of a GPOS lookup on the glyph at an index in order, the first that applies deciding, and
    /// collects the unsafe-to-concat range of every subtable that failed before it, <c>hb_ot_layout_lookup_accelerator_t::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="next">The next subtable.</param>
    /// <param name="concatTo">The end of the unsafe-to-concat range of the subtables that failed so far.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private tryPosSubtables (env: PosEnv) (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (next: int) (concatTo: int) =
        match next < subtables.Length with
        | false -> PosOutcome(false, 0, Unchecked.defaultof<GlyphPosition>, false, 0, Unchecked.defaultof<GlyphPosition>, false, concatTo, 0, 0)
        | true ->
            let accel = subtables[next]
            match Digest.mayHave infos[index].Codepoint accel.SubtableDigest with
            | false -> tryPosSubtables env subtables infos props positions index (next + 1) concatTo
            | true ->
                let outcome = GposLookups.applySubtable env accel.Subtable infos props positions index
                match outcome.Applied, concatTo with
                | false, _ -> tryPosSubtables env subtables infos props positions index (next + 1) (max concatTo outcome.ConcatTo)
                | true, 0 -> outcome
                | true, _ -> PosOutcome(true, outcome.Next, outcome.First, outcome.FirstWorked, outcome.SecondIndex, outcome.Second, outcome.SecondWorked, max concatTo outcome.ConcatTo, outcome.Break1To, outcome.Break2To, outcome.Advance)

    /// <summary>Whether a lookup has a subtable of one of the attachment types, from a subtable on; the subtables of a lookup are
    /// all of one type, so a lookup attaches when its first does, and the rest are asked all the same.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when a subtable from the index on is an attachment.</returns>
    [<TailCall>]
    let rec private anyAttachment (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) index =
        match index < subtables.Length with
        | false -> false
        | true ->
            match GposAttach.isAttachment subtables[index].Subtable with
            | true -> true
            | false -> anyAttachment subtables (index + 1)

    /// <summary>Tries the subtables of an attachment lookup on the glyph at an index in order, the first that applies deciding,
    /// the cache of the last base threaded through them as the apply context holds it, and the unsafe-to-concat range of every
    /// subtable that failed before it collected, <c>hb_ot_layout_lookup_accelerator_t::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="compiled">The compiled table of each subtable that has one, or empty.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="next">The next subtable.</param>
    /// <param name="concatFrom">The start of the unsafe-to-concat range of the subtables that failed so far.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    [<TailCall>]
    let rec private tryAttachSubtables (env: PosEnv) (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) (compiled: ImmutableArray<MarkTable voption>) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index next concatFrom lastBase lastBaseUntil =
        match next < subtables.Length with
        | false -> GposAttach.nothing concatFrom lastBase lastBaseUntil
        | true ->
            let accel = subtables[next]
            match Digest.mayHave infos[index].Codepoint accel.SubtableDigest with
            | false -> tryAttachSubtables env subtables compiled infos props positions index (next + 1) concatFrom lastBase lastBaseUntil
            | true ->
                let outcome = GposAttach.applyCompiled env accel.Subtable (match next < compiled.Length with | true -> compiled[next] | false -> ValueNone) infos props positions index lastBase lastBaseUntil
                match outcome.Applied with
                | true -> GposAttach.withConcat outcome (GposAttach.mergeConcat concatFrom index outcome.ConcatFrom)
                | false -> tryAttachSubtables env subtables compiled infos props positions index (next + 1) (GposAttach.mergeConcat concatFrom index outcome.ConcatFrom) outcome.LastBase outcome.LastBaseUntil

    /// <summary>Whether every subtable of a lookup is a pair adjustment, from a subtable on, <see cref="PairTables.onlyPairs"/>.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when no subtable from the index on is anything else.</returns>
    let private onlyPairs (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) (index: int) =
        PairTables.onlyPairs subtables index

    /// <summary>Finds the glyph a pair is made with when the lookup's flags skip glyphs: the first glyph from an index on that the
    /// flags do not filter out, which is the run's length when every one is.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="lookupProps">The lookup properties.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first candidate index.</param>
    /// <returns>The index of the glyph, or the run's length.</returns>
    [<TailCall>]
    let rec private pairPartner (gdef: Gdef) lookupProps (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index =
        match index < infos.Length && not (Matching.checkGlyphProperty gdef infos[index].Codepoint props[index].GlyphClass lookupProps) with
        | true -> pairPartner gdef lookupProps infos props (index + 1)
        | false -> index

    /// <summary>Adds the gap of skipped glyphs between the two glyphs of a pair to its step: a step that does nothing and a complex
    /// step carry none, and a gap the step cannot hold makes the pair complex.</summary>
    /// <param name="step">The step.</param>
    /// <param name="gap">The number of skipped glyphs between the two glyphs.</param>
    /// <returns>The step.</returns>
    let private pairWithGap step gap =
        match PairStep.acts step && not (PairStep.isComplex step), gap > PairStep.MaxGap with
        | false, _ -> step
        | true, true -> PairStep.Complex
        | true, false -> PairStep.withGap step gap

    /// <summary>Classifies the pair a glyph starts: the glyph the lookup's scan stops at, and the partner <c>skippy_iter.next</c>
    /// finds when it is the glyph right after, or when the glyphs between are all ones the lookup's flags filter out, which the step
    /// counts as its gap.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="covered">The glyphs the lookup starts at.</param>
    /// <param name="mask">The lookup mask.</param>
    /// <param name="tables">The lookup's subtables, compiled.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="hazard">Whether the unsafe-to-concat hazard is wanted.</param>
    /// <returns>The step.</returns>
    let private pairStep (env: PosEnv) (covered: ImmutableArray<uint64>) (mask: uint32) (tables: ImmutableArray<PairTable>) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) (hazard: bool) =
        let glyph = infos[index].Codepoint
        let word = int (glyph >>> 6)
        let lookupProps = env.Matcher.LookupProps
        match word < covered.Length && (covered[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL && infos[index].Mask &&& mask <> 0u
              && (lookupProps = 0u || Matching.checkGlyphProperty env.Gdef glyph props[index].GlyphClass lookupProps) with
        | false -> PairStep.Nothing
        | true ->
            let next = match lookupProps with | 0u -> index + 1 | _ -> pairPartner env.Gdef lookupProps infos props (index + 1)
            let gap = next - index - 1
            match next < infos.Length with
            | false ->
                let reach = max 0 (infos.Length - index - 2)
                match hazard, reach > PairStep.MaxGap with
                | false, _ -> PairStep.Nothing
                | true, true -> PairStep.Complex
                | true, false -> PairStep.withGap PairStep.Hazard reach
            | true ->
                match infos[next].Mask &&& mask = 0u || UnicodeProps.isDefaultIgnorable props[next] with
                | true -> PairStep.Complex
                | false -> pairWithGap (Sealed.pairChain tables env.Font.XMult glyph infos[next].Codepoint hazard) gap

    /// <summary>The outcome of one context or chained-context subtable of a GPOS lookup on the glyph at an index: the matching
    /// of GSUB's, on a run positioned in place, whose output buffer is the run itself up to the glyph.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="position">The subtable's position in the lookup.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The buffer a match fills with its input positions.</param>
    /// <returns>The outcome.</returns>
    let private posContextSubtableOutcome gdef inputMatcher contextMatcher subtable position (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index (positions: Span<int>) =
        let useDigest = position < CachedSubtables
        match subtable with
        | PosContext rules -> ContextApply.matchContext rules useDigest gdef inputMatcher contextMatcher infos props infos props index index positions
        | PosChainContext rules -> ContextApply.matchChain rules useDigest gdef inputMatcher contextMatcher infos props infos props index index positions
        | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosPending _ ->
            ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, FlagBounds.none, -1)

    /// <summary>Tries the context subtables of a GPOS lookup on the glyph at an index in turn, accumulating the ranges the
    /// subtables that failed owe, as <see cref="tryContextSubtables"/> does for GSUB.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="inputMatcher">The input matcher.</param>
    /// <param name="contextMatcher">The context matcher.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="subtable">The next subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="positions">The buffer a match fills with its input positions.</param>
    /// <param name="owed">The ranges the subtables that failed owe.</param>
    /// <returns>The outcome of the first subtable that matches, or a miss holding the ranges owed.</returns>
    [<TailCall>]
    let rec private tryPosContextSubtables gdef inputMatcher contextMatcher (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) subtable (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index (positions: Span<int>) owed =
        match subtable < subtables.Length with
        | false -> ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, owed, -1)
        | true ->
            let accel = subtables[subtable]
            match Digest.mayHave infos[index].Codepoint accel.SubtableDigest with
            | false -> tryPosContextSubtables gdef inputMatcher contextMatcher subtables (subtable + 1) infos props index positions owed
            | true ->
                let outcome = posContextSubtableOutcome gdef inputMatcher contextMatcher accel.Subtable subtable infos props index positions
                match outcome.Matched with
                | true -> ContextOutcome(true, outcome.Chain, outcome.InputCount, outcome.MatchEnd, outcome.Records, outcome.StartIndex, outcome.EndIndex, FlagBounds.merge owed outcome.Owed, outcome.AfterConcat)
                | false -> tryPosContextSubtables gdef inputMatcher contextMatcher subtables (subtable + 1) infos props index positions (FlagBounds.merge owed outcome.Owed)

    /// <summary>Whether a lookup has a context or chained-context subtable, from a subtable on; the subtables of a lookup are all
    /// of one type, so a lookup holds contexts when its first does, and the rest are asked all the same.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when a subtable from the index on is a context.</returns>
    [<TailCall>]
    let rec private anyPosContext (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) index =
        match index < subtables.Length with
        | false -> false
        | true ->
            match subtables[index].Subtable with
            | PosContext _ -> true
            | PosChainContext _ -> true
            | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosPending _ -> anyPosContext subtables (index + 1)

    /// <summary>Whether every subtable of a lookup from a subtable on is a mark-to-base or mark-to-mark attachment with a compiled
    /// table, which the dense attachment loop of the kernel applies.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="compiled">The compiled table of each subtable.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when no subtable from the index on is anything else or lacks a table.</returns>
    [<TailCall>]
    let rec private allDenseMarks (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) (compiled: ImmutableArray<MarkTable voption>) index =
        match index < subtables.Length with
        | false -> true
        | true ->
            let has = index < compiled.Length && (match compiled[index] with | ValueSome _ -> true | ValueNone -> false)
            match subtables[index].Subtable, has with
            | MarkBasePos _, true -> allDenseMarks subtables compiled (index + 1)
            | MarkMarkPos _, true -> allDenseMarks subtables compiled (index + 1)
            | _, _ -> false

    /// <summary>One GPOS lookup as the positioning kernel applies it.</summary>
    [<Struct>]
    type private GposLookup =
        val Env: PosEnv
        val LookupMask: uint32
        val LookupProps: uint32
        val LookupDigest: Digest
        val Covered: ImmutableArray<uint64>
        val Subtables: ImmutableArray<SubtableAccel<PosSubtable>>
        val Tables: ImmutableArray<PairTable>
        val Compiled: ImmutableArray<MarkTable voption>
        val Targets: ImmutableArray<uint64> voption
        val PairLoop: bool
        val Attaching: bool
        val ContextLookup: bool
        val ContextMatcher: Matcher
        val AutoZwnj: bool
        val AutoZwj: bool
        val DenseMarks: bool
        val BaseMatcher: Matcher
        val MarkMatcher: Matcher

        /// <summary>Creates the lookup.</summary>
        /// <param name="env">The environment.</param>
        /// <param name="lookupMask">The lookup mask.</param>
        /// <param name="lookupProps">The lookup props.</param>
        /// <param name="lookupDigest">The lookup digest.</param>
        /// <param name="covered">The exact set of the glyphs the lookup covers, empty to scan by the digest.</param>
        /// <param name="subtables">The subtables.</param>
        /// <param name="tables">The subtables compiled for the pair loop, empty when the program has none.</param>
        /// <param name="compiled">The mark subtables compiled for the mark attachment, empty for none.</param>
        /// <param name="targets">The glyphs the lookup's mark attachments attach to, none for a lookup that is not one.</param>
        /// <param name="autoZwnj">Whether ZWNJ is ignored automatically.</param>
        /// <param name="autoZwj">Whether ZWJ is ignored automatically.</param>
        new (env, lookupMask, lookupProps, lookupDigest, covered: ImmutableArray<uint64>, subtables, tables: ImmutableArray<PairTable>, compiled: ImmutableArray<MarkTable voption>, targets: ImmutableArray<uint64> voption, autoZwnj, autoZwj) =
            { Env = env
              LookupMask = lookupMask
              LookupProps = lookupProps
              LookupDigest = lookupDigest
              Covered = covered
              Subtables = subtables
              Tables = tables
              Compiled = compiled
              Targets = targets
              PairLoop = env.Horizontal && not covered.IsEmpty && not tables.IsEmpty && onlyPairs subtables 0
              Attaching = anyAttachment subtables 0
              ContextLookup = anyPosContext subtables 0
              ContextMatcher = Matching.matcherFor 1 true lookupProps lookupMask autoZwnj autoZwj false 0uy
              AutoZwnj = autoZwnj
              AutoZwj = autoZwj
              DenseMarks = subtables.Length > 0 && allDenseMarks subtables compiled 0
              BaseMatcher = { env.Matcher with LookupProps = LookupFlag.IgnoreMarks }
              MarkMatcher = { env.Matcher with LookupProps = env.Matcher.LookupProps &&& ~~~LookupFlag.IgnoreFlags } }

        interface IPosLookup with
            member this.Pairs = this.PairLoop

            member this.Pair (infos, props, index, hazard) =
                pairStep this.Env this.Covered this.LookupMask this.Tables infos props index hazard

            member this.Scan (infos, props, index) =
                match this.Covered.IsEmpty with
                | true -> scanForward this.LookupDigest this.LookupMask this.LookupProps this.Env.Gdef infos props index
                | false -> scanCovered this.Covered this.LookupMask this.LookupProps this.Env.Gdef infos props index

            member this.Try (infos, props, positions, index) =
                tryPosSubtables this.Env this.Subtables infos props positions index 0 0

            member this.Attaches = this.Attaching

            member this.Futile infos =
                match this.Targets with
                | ValueNone -> false
                | ValueSome targets -> not (Sealed.anyGlyphIn targets infos)

            member this.Attach (infos, props, positions, index, lastBase, lastBaseUntil) =
                tryAttachSubtables this.Env this.Subtables this.Compiled infos props positions index 0 -1 lastBase lastBaseUntil

            member this.HasContext = this.ContextLookup

            member this.TryContext (infos, props, index, positions) =
                tryPosContextSubtables this.Env.Gdef this.Env.Matcher this.ContextMatcher this.Subtables 0 infos props index positions FlagBounds.none

            member this.Nest () = struct (this.LookupMask, this.AutoZwnj, this.AutoZwj)

            member this.Dense = this.DenseMarks

            member this.DenseCount = this.Subtables.Length

            member this.DenseHorizontal = this.Env.Horizontal

            member this.MarkOf (subtable, glyph) =
                match this.Compiled[subtable] with
                | ValueSome table -> GposLookups.coverageIndex table.Source.MarkCoverage table.Source.MarkDirect glyph
                | ValueNone -> -1

            member this.OnMarks subtable =
                match this.Subtables[subtable].Subtable with
                | MarkMarkPos _ -> true
                | MarkBasePos _ | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ | CursivePos _ | MarkLigPos _ | PosContext _ | PosChainContext _ | PosPending _ -> false

            member this.BaseMatches (info, props) =
                match Matching.verdict this.BaseMatcher this.Env.Gdef (NoMatchFunc()) 0 info props with
                | Matched -> true
                | NotMatched -> false
                | Skipped -> false

            member this.PreviousMark (infos, props, index) =
                match Matching.prev this.MarkMatcher this.Env.Gdef (NoMatchFunc()) 0 infos props index with
                | Hit previous -> previous
                | Miss unsafeFrom -> ~~~unsafeFrom

            member this.RowOf (subtable, glyph) =
                match this.Compiled[subtable] with
                | ValueSome table -> GposLookups.coverageIndex table.Source.TargetCoverage table.Source.TargetDirect glyph
                | ValueNone -> -1

            member this.AnchorCell (subtable, row, markIndex) =
                match this.Compiled[subtable] with
                | ValueNone -> -1
                | ValueSome table ->
                    let attachment = table.Source
                    let markClass = match uint32 markIndex < uint32 attachment.MarkClasses.Length with | true -> int attachment.MarkClasses[markIndex] | false -> 0
                    let at = int64 row * int64 attachment.ClassCount + int64 markClass
                    match row >= attachment.Rows || markClass >= attachment.ClassCount || uint64 at >= uint64 table.AnchorKnown.Length || not table.AnchorKnown[int at] with
                    | true -> -1
                    | false -> int at

            member this.AnchorOffsetX (subtable, cell, markIndex) =
                match this.Compiled[subtable] with
                | ValueNone -> 0
                | ValueSome table ->
                    let markX = match uint32 markIndex < uint32 table.MarkX.Length with | true -> table.MarkX[markIndex] | false -> 0
                    GposAttach.anchorDelta this.Env table.AnchorX[cell] markX true

            member this.AnchorOffsetY (subtable, cell, markIndex) =
                match this.Compiled[subtable] with
                | ValueNone -> 0
                | ValueSome table ->
                    let markY = match uint32 markIndex < uint32 table.MarkY.Length with | true -> table.MarkY[markIndex] | false -> 0
                    GposAttach.anchorDelta this.Env table.AnchorY[cell] markY false

    /// <summary>The lookup a context rule's record names, <c>PosLookup::dispatch_recurse_func</c>: its own subtables and properties
    /// under the outer mask and the outer automatic-joiner switches; a table index past the table has no subtables, so nothing applies.</summary>
    /// <param name="font">The font.</param>
    /// <param name="direction">The run's direction.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="table">The GPOS table.</param>
    /// <param name="lookupIndex">The lookup's index in the table.</param>
    /// <param name="mask">The outer lookup mask.</param>
    /// <param name="autoZwnj">Whether ZWNJ is ignored automatically.</param>
    /// <param name="autoZwj">Whether ZWJ is ignored automatically.</param>
    /// <returns>The nested lookup.</returns>
    let private nestedPosLookup (font: Font) direction gdef (table: LayoutTable<PosSubtable>) lookupIndex mask autoZwnj autoZwj =
        let known = lookupIndex < table.Lookups.Length
        let accel = match known with | true -> ValueSome (table.Lookups[lookupIndex].Value) | false -> ValueNone
        let props = match accel with | ValueSome found -> Gdef.sanitizeLookupProps gdef found.Props | ValueNone -> 0u
        let matcher = Matching.matcherFor 1 false props mask autoZwnj autoZwj false 0uy
        let env = { Font = font; Direction = direction; Horizontal = Direction.isHorizontal direction; Matcher = matcher; Gdef = gdef }
        match accel with
        | ValueNone -> GposLookup(env, mask, props, Digest.empty, ImmutableArray<uint64>.Empty, ImmutableArray.Empty, ImmutableArray<PairTable>.Empty, ImmutableArray<MarkTable voption>.Empty, ValueNone, autoZwnj, autoZwj)
        | ValueSome found -> GposLookup(env, mask, props, found.LookupDigest, ImmutableArray<uint64>.Empty, found.Subtables, ImmutableArray<PairTable>.Empty, ImmutableArray<MarkTable voption>.Empty, ValueNone, autoZwnj, autoZwj)

    /// <summary>The lookups of a GPOS table as the positioning kernel applies them, <c>hb_ot_map_t::apply</c>.</summary>
    [<Struct>]
    type private GposProgram =
        val Font: Font
        val Direction: Direction
        val Gdef: Gdef
        val Covered: ImmutableArray<ImmutableArray<uint64>>
        val Table: LayoutTable<PosSubtable>
        val Lookups: ImmutableArray<LookupMap>
        val GlyphLookupTable: ImmutableArray<uint64>
        val End: int
        val PairTables: ImmutableArray<ImmutableArray<PairTable>>
        val Marks: ImmutableArray<ImmutableArray<MarkTable voption>>
        val Targets: ImmutableArray<ImmutableArray<uint64> voption>

        /// <summary>Creates the program.</summary>
        /// <param name="font">The font.</param>
        /// <param name="table">The GPOS table.</param>
        /// <param name="gdef">The GDEF.</param>
        /// <param name="direction">The run's direction.</param>
        /// <param name="covered">The glyph set of every lookup of the table, empty to scan by the digests.</param>
        /// <param name="glyphLookups">The positions of the first 64 lookups that cover each glyph.</param>
        /// <param name="lookups">The compiled lookups.</param>
        /// <param name="stages">The compiled stages.</param>
        /// <param name="pairTables">The compiled pair subtables of every lookup of the table, empty to apply every lookup by the general loop.</param>
        /// <param name="marks">The compiled mark subtables of every lookup of the table, empty to attach by the general reader.</param>
        /// <param name="targets">The glyphs every lookup of mark attachments attaches to, empty to visit every lookup.</param>
        new (font: Font, table, gdef, direction, covered, glyphLookups, lookups, stages, pairTables, marks, targets) =
            { Font = font
              Direction = direction
              Gdef = gdef
              Covered = covered
              Table = table
              Lookups = lookups
              GlyphLookupTable = glyphLookups
              End = lookupEndOf stages 0 0
              PairTables = pairTables
              Marks = marks
              Targets = targets }

        interface IPosProgram<GposLookup> with
            member this.LookupEnd = this.End

            member this.GlyphLookups = this.GlyphLookupTable

            member this.Admits (index, digest, candidates, flags) =
                let lookupMap = this.Lookups[index]
                let accel = this.Table.Lookups[lookupMap.LookupIndex].Value
                let produceUnsafeToConcat = flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default
                lookupMap.Mask <> 0u
                && (match index < 64 && not this.GlyphLookupTable.IsEmpty with
                    | true -> (candidates >>> index) &&& 1UL <> 0UL
                    | false -> Digest.mayIntersect accel.LookupDigest digest && (produceUnsafeToConcat || Digest.mayIntersect accel.SecondDigest digest))

            member this.Lookup index =
                let lookupMap = this.Lookups[index]
                let accel = this.Table.Lookups[lookupMap.LookupIndex].Value
                let props = Gdef.sanitizeLookupProps this.Gdef accel.Props
                let matcher = Matching.matcherFor 1 false props lookupMap.Mask lookupMap.AutoZwnj lookupMap.AutoZwj false 0uy
                let env = { Font = this.Font; Direction = this.Direction; Horizontal = Direction.isHorizontal this.Direction; Matcher = matcher; Gdef = this.Gdef }
                let covered = match this.Covered.IsEmpty with | true -> ImmutableArray<uint64>.Empty | false -> this.Covered[lookupMap.LookupIndex]
                let tables = match this.PairTables.IsEmpty with | true -> ImmutableArray<PairTable>.Empty | false -> this.PairTables[lookupMap.LookupIndex]
                let compiled = match this.Marks.IsEmpty with | true -> ImmutableArray<MarkTable voption>.Empty | false -> this.Marks[lookupMap.LookupIndex]
                let targets = match this.Targets.IsEmpty with | true -> ValueNone | false -> this.Targets[lookupMap.LookupIndex]
                GposLookup(env, lookupMap.Mask, props, accel.LookupDigest, covered, accel.Subtables, tables, compiled, targets, lookupMap.AutoZwnj, lookupMap.AutoZwj)

            member this.NestedLookup (lookupIndex, mask, autoZwnj, autoZwj) =
                nestedPosLookup this.Font this.Direction this.Gdef this.Table lookupIndex mask autoZwnj autoZwj

    /// <summary>Runs the GPOS pass through the positioning kernel, <c>hb_ot_map_t::position</c>: every lookup is a loop over
    /// the run that writes the positions and the glyph flags on one working copy.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run, the same instance when nothing changed.</returns>
    let position (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        GlyphRun.runPosKernel (GposProgram(font, font.Ot.Gpos.Value, font.Ot.Gdef.Value, run.SegmentProperties.Direction, ImmutableArray.Empty, ImmutableArray.Empty, plan.Map.GposLookups, plan.Map.GposStages, ImmutableArray.Empty, ImmutableArray.Empty, ImmutableArray.Empty)) run

    /// <summary>The glyph set of one GPOS lookup: the glyphs its subtables that can apply cover, empty for a lookup outside the
    /// map or whose subtables are all pending.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="lookupIndex">The lookup index in the table.</param>
    /// <returns>The glyph set.</returns>
    let private posLookupSet (table: LayoutTable<PosSubtable>) (lookups: ImmutableArray<LookupMap>) lookupIndex =
        match inMap lookups lookupIndex 0 with
        | false -> ImmutableArray<uint64>.Empty
        | true -> Sealed.glyphSet (collectSubtableGlyphs posSubtableGlyphs table.Lookups[lookupIndex].Value.Subtables 0 ImmutableArray.Empty)

    /// <summary>Builds the glyph set of every lookup of a GPOS table, indexed by the lookup's index in the table, so the
    /// positioning kernel scans a run for exactly the glyphs a lookup can start a match at.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <returns>The glyph sets, one per lookup of the table.</returns>
    let internal gposLookupCoveredOf (table: LayoutTable<PosSubtable>) (lookups: ImmutableArray<LookupMap>) =
        Sealed.init table.Lookups.Length (fun lookupIndex -> posLookupSet table lookups lookupIndex)

    /// <summary>Builds the glyph set of every GPOS lookup of a plan, indexed by the lookup's index in the table, so the
    /// positioning kernel scans a run for exactly the glyphs a lookup can start a match at.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The glyph sets, one per lookup of the table.</returns>
    let gposLookupCovered (plan: OtShapePlan) (font: Font) =
        Sealed.chosenLazySets font.Ot.GposLookupSets.Value ((mapIndices plan.Map.GposLookups).AsSpan())

    /// <summary>The glyphs the subtables of the lookup at a position of the map cover.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="position">The position in the map.</param>
    /// <returns>The covered glyphs, with repeats.</returns>
    let private posPositionGlyphs (table: LayoutTable<PosSubtable>) (lookups: ImmutableArray<LookupMap>) position =
        collectSubtableGlyphs posSubtableGlyphs table.Lookups[lookups[position].LookupIndex].Value.Subtables 0 ImmutableArray.Empty

    /// <summary>Builds, for each glyph, a bit per position among the first 64 of the GPOS lookups of a map whose subtables cover
    /// it, so one pass over a run says which lookups can apply to it.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <returns>The words indexed by glyph.</returns>
    let internal gposGlyphLookupsOf (table: LayoutTable<PosSubtable>) (lookups: ImmutableArray<LookupMap>) =
        Sealed.glyphLookupTable (Sealed.init (min lookups.Length 64) (fun position -> posPositionGlyphs table lookups position))

    /// <summary>Builds, for each glyph, a bit per position among the first 64 of the plan's GPOS lookups whose subtables cover
    /// it, so one pass over a run says which lookups can apply to it.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The words indexed by glyph.</returns>
    let gposGlyphLookups (plan: OtShapePlan) (font: Font) =
        glyphLookupsKept font true font.Ot.GposLookupSets.Value (mapIndices plan.Map.GposLookups)

    /// <summary>Whether two arrays of compiled pair tables are the same arrays, lookup by lookup.</summary>
    /// <param name="left">The first arrays.</param>
    /// <param name="right">The second arrays.</param>
    /// <param name="index">The next lookup.</param>
    /// <returns>True when every lookup's array is one and the same.</returns>
    [<TailCall>]
    let rec private sameTables (left: ImmutableArray<ImmutableArray<PairTable>>) (right: ImmutableArray<ImmutableArray<PairTable>>) (index: int) =
        match index < left.Length with
        | false -> true
        | true ->
            match left[index].Equals right[index] with
            | true -> sameTables left right (index + 1)
            | false -> false

    /// <summary>Whether the devices of a value record do nothing for a font: it has neither a ppem nor non-zero coordinates,
    /// <c>ValueFormat::apply_value</c> reading a device only then.</summary>
    /// <param name="font">The font.</param>
    /// <returns>True when no device is read.</returns>
    let private devicesInert (font: Font) =
        font.XPpem = 0u && font.YPpem = 0u && not font.Variations.Coords.HasNonZero

    /// <summary>The compiled pair subtables of one GPOS lookup: empty for a lookup outside the map or one that has a subtable
    /// that is no pair adjustment.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="lookupIndex">The lookup index in the table.</param>
    /// <returns>The compiled subtables.</returns>
    let private pairTablesOf (table: LayoutTable<PosSubtable>) (lookups: ImmutableArray<LookupMap>) lookupIndex =
        match inMap lookups lookupIndex 0 with
        | false -> ImmutableArray<PairTable>.Empty
        | true ->
            let subtables = table.Lookups[lookupIndex].Value.Subtables
            match onlyPairs subtables 0 with
            | false -> ImmutableArray<PairTable>.Empty
            | true -> PairTables.compileAll subtables

    /// <summary>The compiled pair subtables of one GPOS lookup for a font whose devices do nothing: empty for a lookup outside the map or
    /// one that has a subtable that is no pair adjustment.</summary>
    /// <param name="table">The table.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="lookupIndex">The lookup index in the table.</param>
    /// <returns>The compiled subtables.</returns>
    let private pairTablesInertOf (table: LayoutTable<PosSubtable>) (lookups: ImmutableArray<LookupMap>) lookupIndex =
        match inMap lookups lookupIndex 0 with
        | false -> ImmutableArray<PairTable>.Empty
        | true -> PairTables.ofLookupInert table.Lookups[lookupIndex].Value.Subtables

    /// <summary>The largest number of positions a candidate word of the lean loop names.</summary>
    [<Literal>]
    let private LeanPositions = 64

    /// <summary>The operations of the budget a lookup of the lean loop may use for each glyph of the run: an unsafe-to-break range
    /// of a pair is two glyphs, so two is the bound, and four leaves room.</summary>
    [<Literal>]
    let private LeanOpsPerGlyph = 4L

    /// <summary>Whether the lookup at a position of the map is one the lean loop applies, but for the glyphs it skips: its mask is in
    /// the global mask, its subtables are pairs with plain records and it starts at some glyph.</summary>
    /// <param name="globalMask">The global mask of the map.</param>
    /// <param name="covered">The glyphs the lookup starts at.</param>
    /// <param name="tables">The compiled subtables of the lookup.</param>
    /// <param name="lookupMap">The lookup in the map.</param>
    /// <returns>True when the lookup is a plain pair lookup of the map.</returns>
    let private isPlainPairs (globalMask: uint32) (covered: ImmutableArray<uint64>) (tables: ImmutableArray<PairTable>) (lookupMap: LookupMap) =
        lookupMap.Mask &&& globalMask <> 0u
        && not covered.IsEmpty
        && not tables.IsEmpty
        && PairTables.allPlain tables 0

    /// <summary>Whether the lookup at a position of the map is one the lean loop applies: a plain pair lookup that filters no glyph.</summary>
    /// <param name="globalMask">The global mask of the map.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="table">The table.</param>
    /// <param name="covered">The glyphs the lookup starts at.</param>
    /// <param name="tables">The compiled subtables of the lookup.</param>
    /// <param name="lookupMap">The lookup in the map.</param>
    /// <returns>True when the lean loop applies the lookup.</returns>
    let private isLean (globalMask: uint32) (gdef: Gdef) (table: LayoutTable<PosSubtable>) (covered: ImmutableArray<uint64>) (tables: ImmutableArray<PairTable>) (lookupMap: LookupMap) =
        isPlainPairs globalMask covered tables lookupMap
        && Gdef.sanitizeLookupProps gdef table.Lookups[lookupMap.LookupIndex].Value.Props = 0u

    /// <summary>Whether the lookup at a position of the map is a plain pair lookup that skips marks and nothing else: its flags ignore no
    /// base glyph and no ligature (<c>match_properties</c> reads the mark filters of a glyph only when the glyph is a mark), so on a run
    /// of which no glyph is a mark it skips nothing, and the lean loop applies it as it applies a lookup that filters no glyph.</summary>
    /// <param name="globalMask">The global mask of the map.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="table">The table.</param>
    /// <param name="covered">The glyphs the lookup starts at.</param>
    /// <param name="tables">The compiled subtables of the lookup.</param>
    /// <param name="lookupMap">The lookup in the map.</param>
    /// <returns>True when the lean loop applies the lookup to a run without marks.</returns>
    let private isUnmarkedLean (globalMask: uint32) (gdef: Gdef) (table: LayoutTable<PosSubtable>) (covered: ImmutableArray<uint64>) (tables: ImmutableArray<PairTable>) (lookupMap: LookupMap) =
        isPlainPairs globalMask covered tables lookupMap
        && Gdef.sanitizeLookupProps gdef table.Lookups[lookupMap.LookupIndex].Value.Props &&& (LookupFlag.IgnoreBaseGlyphs ||| LookupFlag.IgnoreLigatures) = 0u

    /// <summary>The bits of the positions of the map, among the first 64, for which a predicate holds.</summary>
    /// <param name="count">The number of positions.</param>
    /// <param name="holds">The predicate, given a position.</param>
    /// <param name="position">The next position.</param>
    /// <param name="bits">The bits so far.</param>
    /// <returns>The bits.</returns>
    [<TailCall>]
    let rec private positionBits (count: int) (holds: int -> bool) (position: int) (bits: uint64) =
        match position < count with
        | false -> bits
        | true ->
            match holds position with
            | true -> positionBits count holds (position + 1) (bits ||| (1UL <<< position))
            | false -> positionBits count holds (position + 1) bits

    /// <summary>Builds the pair lookups of a GPOS table and map, compiled once: the subtables of every lookup that is made of pair
    /// adjustments, and the positions of the map whose lookups the lean loop applies. A map of more than 64 lookups has no lean
    /// position.</summary>
    /// <param name="table">The table.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="globalMask">The global mask of the map.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph.</param>
    /// <param name="tables">The compiled pair subtables of every lookup of the table, empty for a lookup the map does not apply.</param>
    /// <returns>The pair lookups.</returns>
    let internal gposPairsFrom (table: LayoutTable<PosSubtable>) (gdef: Gdef) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (globalMask: uint32) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (tables: ImmutableArray<ImmutableArray<PairTable>>) =
        let enabled = lookups.Length <= LeanPositions && not glyphLookups.IsEmpty
        let count = match enabled with | true -> lookups.Length | false -> 0
        let positionTables = Sealed.init count (fun position -> tables[lookups[position].LookupIndex])
        let positionCovered = Sealed.init count (fun position -> covered[lookups[position].LookupIndex])
        let lookupEnd = lookupEndOf stages 0 0
        let lean = positionBits (min count lookupEnd) (fun position -> isLean globalMask gdef table positionCovered[position] positionTables[position] lookups[position]) 0 0UL
        let unmarked = positionBits (min count lookupEnd) (fun position -> isUnmarkedLean globalMask gdef table positionCovered[position] positionTables[position] lookups[position]) 0 0UL &&& ~~~lean
        let masked = positionBits count (fun position -> lookups[position].Mask = 0u) 0 0UL
        let ended = positionBits LeanPositions (fun position -> position >= lookupEnd) 0 0UL
        let reaching = positionBits count (fun position -> not positionCovered[position].IsEmpty) 0 0UL
        { Tables = tables
          PositionCovered = positionCovered
          PositionTables = positionTables
          Glyphs = glyphLookups
          Enabled = enabled
          Lean = lean
          LeanOnly = count > 0 && reaching &&& ~~~(lean ||| masked ||| ended) = 0UL
          Unmarked = unmarked
          UnmarkedOnly = count > 0 && reaching &&& ~~~(lean ||| unmarked ||| masked ||| ended) = 0UL
          Idle = masked ||| ended
          Marks = ImmutableArray<ImmutableArray<MarkTable voption>>.Empty
          Targets = ImmutableArray<ImmutableArray<uint64> voption>.Empty
          Inert = ValueNone
          Resolved = ValueNone }

    /// <summary>Builds the pair lookups of a GPOS table and map, compiling the pair subtables of the lookups the map applies.</summary>
    /// <param name="table">The table.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="globalMask">The global mask of the map.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph.</param>
    /// <returns>The pair lookups.</returns>
    let internal gposPairsOf (table: LayoutTable<PosSubtable>) (gdef: Gdef) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (globalMask: uint32) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) =
        let strict = gposPairsFrom table gdef lookups stages globalMask covered glyphLookups (Sealed.init table.Lookups.Length (fun lookupIndex -> pairTablesOf table lookups lookupIndex))
        let inert = gposPairsFrom table gdef lookups stages globalMask covered glyphLookups (Sealed.init table.Lookups.Length (fun lookupIndex -> pairTablesInertOf table lookups lookupIndex))
        match sameTables strict.Tables inert.Tables 0 with
        | true -> strict
        | false -> { strict with Inert = ValueSome inert }

    /// <summary>Whether a font reads the devices of the horizontal value records: it has a horizontal ppem or non-zero coordinates,
    /// <c>ValueFormat::apply_value</c> reading a device only then.</summary>
    /// <param name="font">The font.</param>
    /// <returns>True when the devices are read.</returns>
    let private devicesRead (font: Font) =
        font.XPpem <> 0u || font.Variations.Coords.HasNonZero

    /// <summary>The key the pair tables resolved for a font are kept under: its coordinates and horizontal scale.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The key.</returns>
    let private resolveKeyOf (font: Font) =
        { Coords = font.Variations.Coords.Sealed
          XScale = font.XScale }

    /// <summary>The scaled delta of a device for a font, as <c>apply_value</c> adds it to an advance; none for a hinting device, whose
    /// delta depends on the ppem and the scale of the line.</summary>
    /// <param name="font">The font.</param>
    /// <param name="device">The device.</param>
    /// <returns>The delta.</returns>
    let private resolvedDelta (font: Font) (device: Device) =
        match device with
        | HintingDevice _ -> ValueNone
        | NoDevice -> ValueSome 0
        | VariationIndex _ -> ValueSome (Device.xDelta font device)

    /// <summary>The pair tables of every lookup of a face resolved for a font that reads devices: the records that carry a variation
    /// device hold the font's advance with the delta of the device added. They are built the first time a font of the coordinates and
    /// scale asks, lookup by lookup as the lookups are used, and a face keeps the newest few keys; a font of other coordinates or scale
    /// has its own, so nothing is stale.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The tables of every lookup, by index in the table.</returns>
    let internal resolvedFaceTables (font: Font) =
        font.Ot.GposPairTablesResolved.GetOrAdd (resolveKeyOf font, fun () ->
            lazy (Sealed.init font.Ot.Gpos.Value.Lookups.Length (fun lookupIndex ->
                lazy (
                    let subtables = font.Ot.Gpos.Value.Lookups[lookupIndex].Value.Subtables
                    match PairTables.anyDevices subtables 0 with
                    | true -> PairTables.ofLookupResolved (Font.emScaleX font) (resolvedDelta font) subtables
                    | false -> font.Ot.GposPairTables.Value[lookupIndex].Value))))

    /// <summary>The pair lookups of a plan to use for a font: the program compiled for a font whose devices do nothing when the font
    /// is one, the program resolved for the font's coordinates and scale when it reads devices, else the plan's own.</summary>
    /// <param name="font">The font.</param>
    /// <param name="pairs">The pair lookups of the plan.</param>
    /// <returns>The pair lookups.</returns>
    let pairsForFont (font: Font) (pairs: PairProgram) =
        match pairs.Inert with
        | ValueNone -> pairs
        | ValueSome inert ->
            match devicesInert font, devicesRead font, pairs.Resolved with
            | true, _, _ -> inert
            | false, true, ValueSome resolved ->
                let key = resolveKeyOf font
                match resolved.TryProgram key with
                | ValueSome program -> program
                | ValueNone -> resolved.Add (key, resolvedFaceTables font)
            | false, true, ValueNone -> pairs
            | false, false, _ -> pairs

    /// <summary>Builds the pair lookups of a plan, compiled once with the plan.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gposLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gposGlyphLookups"/>.</param>
    /// <returns>The pair lookups.</returns>
    let gposPairs (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) =
        let indices = mapIndices plan.Map.GposLookups
        let marks = Sealed.chosenLazyValues font.Ot.GposMarkTables.Value (indices.AsSpan()) ImmutableArray<MarkTable voption>.Empty
        let targets = Sealed.chosenLazyValues font.Ot.GposTargetSets.Value (indices.AsSpan()) ValueNone
        let programOf (faceTables: Lazy<ImmutableArray<Lazy<ImmutableArray<PairTable>>>>) =
            let tables = Sealed.chosenLazySets faceTables.Value (indices.AsSpan())
            { gposPairsFrom font.Ot.Gpos.Value font.Ot.Gdef.Value plan.Map.GposLookups plan.Map.GposStages plan.Map.GlobalMask covered glyphLookups tables with
                Marks = marks
                Targets = targets }
        let strict = programOf font.Ot.GposPairTables
        let inert = programOf font.Ot.GposPairTablesInert
        match sameTables strict.Tables inert.Tables 0 with
        | true -> strict
        | false -> { strict with Inert = ValueSome inert; Resolved = ValueSome (ResolvedPairs programOf) }

    /// <summary>Runs the GPOS pass of a table through the positioning kernel with the sets a map holds: a lookup is visited only
    /// when a glyph of the run is among those its subtables cover, and the run is scanned for exactly those glyphs; a lookup of
    /// pair adjustments that filters no glyph is applied by the pair loop when the pair tables are given.</summary>
    /// <param name="font">The font.</param>
    /// <param name="table">The table.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="lookups">The compiled lookups.</param>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph.</param>
    /// <param name="pairTables">The compiled pair subtables of every lookup of the table, empty for the general loop alone.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run, the same instance when nothing changed.</returns>
    let internal positionTable (font: Font) (table: LayoutTable<PosSubtable>) (gdef: Gdef) (lookups: ImmutableArray<LookupMap>) (stages: ImmutableArray<StageMap>) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (pairTables: ImmutableArray<ImmutableArray<PairTable>>) (run: GlyphRun) =
        GlyphRun.runPosKernel (GposProgram(font, table, gdef, run.SegmentProperties.Direction, covered, glyphLookups, lookups, stages, pairTables, ImmutableArray.Empty, ImmutableArray.Empty)) run

    /// <summary>Runs the GPOS pass through the positioning kernel with the sets a plan holds: a lookup is visited only when a
    /// glyph of the run is among those its subtables cover, and the run is scanned for exactly those glyphs; a lookup of pair
    /// adjustments that filters no glyph is applied by the pair loop.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gposLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gposGlyphLookups"/>.</param>
    /// <param name="pairs">The pair lookups of the plan, <see cref="gposPairs"/>.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run, the same instance when nothing changed.</returns>
    let positionPlanned (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (pairs: PairProgram) (run: GlyphRun) =
        GlyphRun.runPosKernel (GposProgram(font, font.Ot.Gpos.Value, font.Ot.Gdef.Value, run.SegmentProperties.Direction, covered, glyphLookups, plan.Map.GposLookups, plan.Map.GposStages, (pairsForFont font pairs).Tables, pairs.Marks, pairs.Targets)) run

    /// <summary>Runs the GPOS pass as <see cref="positionPlanned"/> does but with the general loop for every lookup, the
    /// reference the pair loop and the lean loop are held to: every field of the run it returns is the one they return.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gposLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gposGlyphLookups"/>.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run, the same instance when nothing changed.</returns>
    let positionPlannedReference (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (run: GlyphRun) =
        positionTable font font.Ot.Gpos.Value font.Ot.Gdef.Value plan.Map.GposLookups plan.Map.GposStages covered glyphLookups ImmutableArray.Empty run

    /// <summary>Applies the pair lookups to the arrays of a horizontal run that the caller made in this call, shared with no one, in
    /// place, when the run is one the lean loop applies them to: every glyph carries the global mask, none is default ignorable,
    /// the lookups that cover its glyphs are all lookups the lean loop applies, and the operation budget cannot run out. The
    /// caller vouches for the mask and the ignorables; the rest is checked here, and nothing is written when it does not hold. A
    /// plan whose every lookup that covers a glyph is one the lean loop applies needs no look at the run's glyphs first: each
    /// lookup's loop passes over the glyphs it does not start at.</summary>
    /// <param name="planPairs">The pair lookups of the plan, <see cref="gposPairs"/>.</param>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs of the run, which carry the global mask.</param>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="maxOps">The operation budget of the run.</param>
    /// <returns>The operation budget left, or none when the lean loop does not apply.</returns>
    let positionOwned (planPairs: PairProgram) (font: Font) (infos: ImmutableArray<GlyphInfo>) (positions: ImmutableArray<GlyphPosition>) (flags: BufferFlags) (level: ClusterLevel) (maxOps: int) =
        let pairs = pairsForFont font planPairs
        let bits = match pairs.LeanOnly with | true -> pairs.Lean | false -> Sealed.lookupsCovering pairs.Glyphs (infos.AsSpan())
        let applied = bits &&& pairs.Lean
        match pairs.Enabled && bits &&& ~~~(pairs.Lean ||| pairs.Idle) = 0UL && int64 maxOps >= LeanOpsPerGlyph * int64 infos.Length * int64 (BitOperations.PopCount applied) with
        | false -> ValueNone
        | true -> ValueSome (Sealed.runPairsOwned pairs.PositionCovered pairs.PositionTables font.XMult applied infos positions flags level maxOps)

    /// <summary>Applies the pair lookups to the arrays of a horizontal run that the caller made in this call, shared with no one, as
    /// <see cref="positionOwned"/> does, for a run of which no glyph is a mark: the lookups that skip marks and nothing else
    /// (<see cref="PairProgram.Unmarked"/>) are applied by the lean loop too, since on such a run they skip no glyph. The caller vouches
    /// for that, as it does for the mask and the ignorables; the kernel's arrays and the arrays of a substituted line the lean tails
    /// have checked are such runs.</summary>
    /// <param name="planPairs">The pair lookups of the plan, <see cref="gposPairs"/>.</param>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs of the run, which carry the global mask and none of which is a mark.</param>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="maxOps">The operation budget of the run.</param>
    /// <returns>The operation budget left, or none when the lean loop does not apply.</returns>
    let positionOwnedUnmarked (planPairs: PairProgram) (font: Font) (infos: ImmutableArray<GlyphInfo>) (positions: ImmutableArray<GlyphPosition>) (flags: BufferFlags) (level: ClusterLevel) (maxOps: int) =
        let pairs = pairsForFont font planPairs
        let lean = pairs.Lean ||| pairs.Unmarked
        let bits = match pairs.UnmarkedOnly with | true -> lean | false -> Sealed.lookupsCovering pairs.Glyphs (infos.AsSpan())
        let applied = bits &&& lean
        match pairs.Enabled && bits &&& ~~~(lean ||| pairs.Idle) = 0UL && int64 maxOps >= LeanOpsPerGlyph * int64 infos.Length * int64 (BitOperations.PopCount applied) with
        | false -> ValueNone
        | true -> ValueSome (Sealed.runPairsOwned pairs.PositionCovered pairs.PositionTables font.XMult applied infos positions flags level maxOps)

    /// <summary>Applies the pair lookups in place to the first glyphs of arrays a worker owns, as <see cref="positionOwned"/> does to
    /// the arrays of a run: the arrays may be longer than the run, and the lookups see only its first <paramref name="count"/> glyphs.</summary>
    /// <param name="pairs">The pair lookups of the plan, <see cref="gposPairs"/>.</param>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs of the run, which carry the global mask.</param>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="count">The glyphs of the run.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="maxOps">The operation budget of the run.</param>
    /// <returns>The operation budget left, or none when the lean loop does not apply.</returns>
    let positionOwnedFirst (pairs: PairProgram) (font: Font) (infos: GlyphInfo array) (positions: GlyphPosition array) (count: int) (flags: BufferFlags) (level: ClusterLevel) (maxOps: int) =
        let bits = match pairs.LeanOnly with | true -> pairs.Lean | false -> Sealed.lookupsCovering pairs.Glyphs (ReadOnlySpan<GlyphInfo>(infos, 0, count))
        let applied = bits &&& pairs.Lean
        match pairs.Enabled && bits &&& ~~~(pairs.Lean ||| pairs.Idle) = 0UL && int64 maxOps >= LeanOpsPerGlyph * int64 count * int64 (BitOperations.PopCount applied) with
        | false -> ValueNone
        | true -> ValueSome (Sealed.runPairsOf pairs.PositionCovered pairs.PositionTables font.XMult applied infos positions count flags level maxOps)

    /// <summary>Applies the pair lookups in place to the first glyphs of arrays a worker owns, as <see cref="positionOwnedUnmarked"/> does to
    /// the arrays of a run: the arrays may be longer than the run, and the lookups see only its first <paramref name="count"/> glyphs.</summary>
    /// <param name="pairs">The pair lookups of the plan, <see cref="gposPairs"/>.</param>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs of the run, which carry the global mask and none of which is a mark.</param>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="count">The glyphs of the run.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="level">The cluster level.</param>
    /// <param name="maxOps">The operation budget of the run.</param>
    /// <returns>The operation budget left, or none when the lean loop does not apply.</returns>
    let positionOwnedFirstUnmarked (pairs: PairProgram) (font: Font) (infos: GlyphInfo array) (positions: GlyphPosition array) (count: int) (flags: BufferFlags) (level: ClusterLevel) (maxOps: int) =
        let lean = pairs.Lean ||| pairs.Unmarked
        let bits = match pairs.UnmarkedOnly with | true -> lean | false -> Sealed.lookupsCovering pairs.Glyphs (ReadOnlySpan<GlyphInfo>(infos, 0, count))
        let applied = bits &&& lean
        match pairs.Enabled && bits &&& ~~~(lean ||| pairs.Idle) = 0UL && int64 maxOps >= LeanOpsPerGlyph * int64 count * int64 (BitOperations.PopCount applied) with
        | false -> ValueNone
        | true -> ValueSome (Sealed.runPairsOf pairs.PositionCovered pairs.PositionTables font.XMult applied infos positions count flags level maxOps)

    /// <summary>Runs the GPOS pass of a plan on the infos and positions of a run that the caller made in this call and has shared with no
    /// one, writing the positions and the glyph flags into them in place: the result of <see cref="positionPlanned"/> in every field.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyphs each lookup covers.</param>
    /// <param name="glyphLookups">The lookups that cover each glyph.</param>
    /// <param name="pairs">The pair lookups of the plan.</param>
    /// <param name="direction">The direction of the run.</param>
    /// <param name="input">The run fields, owned by the caller, with positions of the run's length.</param>
    /// <returns>The operation budget left, the success flag and the scratch flags.</returns>
    let internal positionPlannedOwned (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (pairs: PairProgram) direction input =
        Sealed.runGposOwned input (GposProgram(font, font.Ot.Gpos.Value, font.Ot.Gdef.Value, direction, covered, glyphLookups, plan.Map.GposLookups, plan.Map.GposStages, (pairsForFont font pairs).Tables, pairs.Marks, pairs.Targets))

    /// <summary>The position after the last lookup of each window of a plan's GSUB stages: the windows end at the stages whose pause
    /// runs a function, and the last window ends at the last stage, as <c>OtLayout.substituteStages</c> cuts them.</summary>
    /// <param name="stages">The compiled stages, not empty.</param>
    /// <param name="stage">The next stage.</param>
    /// <param name="ends">The ends found so far, newest first.</param>
    /// <returns>The ends, in order.</returns>
    [<TailCall>]
    let rec private windowEnds (stages: ImmutableArray<StageMap>) stage (ends: int list) =
        match stage < stages.Length with
        | false -> List.rev (stages[stages.Length - 1].LastLookup :: ends)
        | true ->
            match PauseKind.runsFunction stages[stage].Pause with
            | false -> windowEnds stages (stage + 1) ends
            | true -> windowEnds stages (stage + 1) (stages[stage].LastLookup :: ends)

    /// <summary>Cuts the windows out of a plan's map one after another.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="indices">The index in the table of every lookup of the map.</param>
    /// <param name="first">The position of the first lookup of the next window.</param>
    /// <param name="ends">The positions after the last lookup of the windows still to cut.</param>
    /// <param name="built">The windows cut so far, newest first.</param>
    /// <returns>The windows, in order.</returns>
    [<TailCall>]
    let rec private windowsFrom (plan: OtShapePlan) (font: Font) (indices: ImmutableArray<int>) first (ends: int list) (built: GsubWindow list) =
        match ends with
        | [] -> List.rev built
        | last :: rest ->
            let count = last - first
            let window =
                { WindowLookups = Sealed.init count (fun index -> plan.Map.GsubLookups[first + index])
                  WindowStages = ImmutableArray.Create { LastLookup = count; Pause = NoPause }
                  WindowGlyphLookups = Sealed.glyphLookupTableOfLazySets font.Ot.GsubLookupSets.Value (indices.AsSpan(first, count))
                  WindowViews = Memo<obj>() }
            windowsFrom plan font indices last rest (window :: built)

    /// <summary>Cuts the lookups of a plan with pauses into the windows between the pauses that run a function, each with its own
    /// table of the lookups that cover each glyph, made when the plan is made and not on every line.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>The windows, none for a plan that has no pause that runs a function.</returns>
    let gsubWindows (plan: OtShapePlan) (font: Font) =
        match plan.Map.HasPauses with
        | false -> ImmutableArray<GsubWindow>.Empty
        | true ->
            let windows = windowsFrom plan font (mapIndices plan.Map.GsubLookups) 0 (windowEnds plan.Map.GsubStages 0 []) []
            Sealed.init windows.Length (fun index -> windows[index])

    /// <summary>Cuts one window out of a plan's map, the lookups from one position to another with their own table of the lookups that
    /// cover each glyph, as <see cref="gsubWindows"/> cuts each of its windows.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="first">The position of the first lookup.</param>
    /// <param name="last">The position after the last lookup.</param>
    /// <returns>The window.</returns>
    let gsubWindowOf (plan: OtShapePlan) (font: Font) first last =
        List.head (windowsFrom plan font (mapIndices plan.Map.GsubLookups) first [ last ] [])

    /// <summary>Applies the lookups of one window of a plan through the kernel, the result of <see cref="applyGsubWindowCovered"/> for the
    /// same lookups: a lookup is walked only over the glyphs its subtables cover, and the lookups that cover no glyph of the run are not
    /// visited at all.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="table">The table.</param>
    /// <param name="window">The window.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every lookup of the window, the same instance when nothing changed.</returns>
    let applyGsubWindow (covered: ImmutableArray<ImmutableArray<uint64>>) (table: LayoutTable<SubstSubtable>) (window: GsubWindow) (font: Font) (run: GlyphRun) =
        GlyphRun.runKernel (GsubProgram(true, true, covered, window.WindowGlyphLookups, table, window.WindowLookups, window.WindowStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value)) run

    /// <summary>Applies the lookups of one window of a plan as <see cref="applyGsubWindow"/> does, to a run whose arrays the caller made in
    /// this call and has shared with no one: the lookups write into them in place.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="table">The table.</param>
    /// <param name="window">The window.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with arrays of its own.</param>
    /// <returns>The run after every lookup of the window, the same instance when nothing changed.</returns>
    let applyGsubWindowOwned (covered: ImmutableArray<ImmutableArray<uint64>>) (table: LayoutTable<SubstSubtable>) (window: GsubWindow) (font: Font) (run: GlyphRun) =
        GlyphRun.runKernelOwned (GsubProgram(true, true, covered, window.WindowGlyphLookups, table, window.WindowLookups, window.WindowStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value)) run

    /// <summary>Runs the lookup kernel over a working set whose arrays the caller made in this call and has shared with no one: the fields
    /// the machine reads are taken from the set where it stands and the fields it produces are put in a copy of it, <c>GlyphRun.runKernelOwnedSet</c>
    /// without passing the set or the program by value from one call to the next.</summary>
    /// <param name="program">The program.</param>
    /// <param name="set">The working set, with arrays of its own.</param>
    /// <returns>The set after the kernel, the set itself when the kernel changed nothing.</returns>
    let private kernelOwnedSet (program: inref<GsubProgram>) (set: inref<WorkingSet>) =
        let input =
            { RunInput.Infos = set.Infos
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
        let admission = { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty }
        let result = Sealed.runGsubWindowOwnedRef &input &program &admission
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

    /// <summary>Applies the lookups of one window of a plan as <see cref="applyGsubWindowOwned"/> does, to a working set whose arrays the caller
    /// made in this call and has shared with no one.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="table">The table.</param>
    /// <param name="window">The window.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, with arrays of its own.</param>
    /// <returns>The set after every lookup of the window.</returns>
    let applyGsubWindowOwnedSet (covered: ImmutableArray<ImmutableArray<uint64>>) (table: LayoutTable<SubstSubtable>) (window: GsubWindow) (font: Font) (set: WorkingSet) =
        let program = prepared window.WindowViews (GsubProgram(true, true, covered, window.WindowGlyphLookups, table, window.WindowLookups, window.WindowStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value))
        kernelOwnedSet &program &set

    /// <summary>Applies the lookups of one window as <see cref="applyGsubWindowOwnedSet"/> does, and says whether the kernel shifted the input
    /// forward, the charge of operations that depends on the length of what follows: the syllable reuse learns through it.</summary>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="table">The table.</param>
    /// <param name="window">The window.</param>
    /// <param name="font">The font.</param>
    /// <param name="set">The working set, with arrays of its own.</param>
    /// <param name="watch">Sees the run after each lookup the kernel applies.</param>
    /// <returns>The set after every lookup of the window, whether the input was shifted and whether the output outgrew the input, or none when
    /// the set is not plain.</returns>
    let applyGsubWindowWatched (covered: ImmutableArray<ImmutableArray<uint64>>) (table: LayoutTable<SubstSubtable>) (window: GsubWindow) (font: Font) (set: WorkingSet) (watch: ILookupWatch) =
        let program = prepared window.WindowViews (GsubProgram(true, true, covered, window.WindowGlyphLookups, table, window.WindowLookups, window.WindowStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value))
        let input =
            { RunInput.Infos = set.Infos
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
        match Sealed.runGsubWindowWatched &input &program watch with
        | ValueNone -> ValueNone
        | ValueSome struct (result, shifted, grew) ->
            let next =
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
            ValueSome struct (next, shifted, grew)

    /// <summary>Runs the GSUB pass over the lookups that can apply as <see cref="substituteApplicable"/> does, to a run whose arrays the
    /// caller made in this call and has shared with no one.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gsubGlyphLookups"/>.</param>
    /// <param name="run">The run, with arrays of its own.</param>
    /// <returns>The substituted run.</returns>
    let substituteApplicableOwned (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (run: GlyphRun) =
        GlyphRun.runKernelOwned (GsubProgram(true, true, covered, glyphLookups, font.Ot.Gsub.Value, plan.Map.GsubLookups, plan.Map.GsubStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value)) run

    /// <summary>Runs the GSUB pass over the lookups that can apply as <see cref="substituteApplicableOwned"/> does, to a working set whose
    /// arrays the caller made in this call and has shared with no one.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table, <see cref="gsubLookupCovered"/>.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph, <see cref="gsubGlyphLookups"/>.</param>
    /// <param name="set">The working set, with arrays of its own.</param>
    /// <returns>The substituted set.</returns>
    let substituteApplicableOwnedSet (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (set: WorkingSet) =
        let program = prepared plan.Map.GsubViews (GsubProgram(true, true, covered, glyphLookups, font.Ot.Gsub.Value, plan.Map.GsubLookups, plan.Map.GsubStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value))
        kernelOwnedSet &program &set

    /// <summary>Whether a program admits a lookup to a run, through the interface the kernel calls.</summary>
    /// <param name="program">The program.</param>
    /// <param name="index">The lookup's position in the map.</param>
    /// <param name="digest">The digest of the run.</param>
    /// <param name="candidates">The lookups of the first 64 positions that cover some glyph of the run.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run.</param>
    /// <returns>True when the lookup is applied.</returns>
    let private admitsVia<'P when 'P : struct and 'P :> IGsubProgram<GsubPass>> (program: 'P) index digest candidates flags (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) shown =
        program.Admits(index, digest, candidates, flags, infos, props, shown)

    /// <summary>Whether a program says a lookup is a reverse lookup, through the interface the kernel calls.</summary>
    /// <param name="program">The program.</param>
    /// <param name="index">The lookup's position in the map.</param>
    /// <returns>True for a reverse lookup.</returns>
    let private isReverseVia<'P when 'P : struct and 'P :> IGsubProgram<GsubPass>> (program: 'P) index =
        program.IsReverse index

    /// <summary>Whether the lookup a program hands out by reference for a position is the one expected.</summary>
    /// <param name="program">The program.</param>
    /// <param name="index">The lookup's position in the map.</param>
    /// <param name="shown">Whether applying a lookup that changes nothing still shows in the published run.</param>
    /// <param name="expected">The lookup built afresh.</param>
    /// <returns>True when the lookup handed out is the expected one.</returns>
    let private handsOut (program: GsubProgram) index shown (expected: GsubPass) =
        sameLookup (Sealed.heldCopy<GsubProgram, GsubPass> program index shown) expected

    /// <summary>Compares each lookup from a position on that a program keeps ready with the lookup built afresh, and what each admits to a run.</summary>
    /// <param name="kept">The program that keeps its lookups ready.</param>
    /// <param name="views">The lookups it keeps.</param>
    /// <param name="plain">The program that builds each lookup when asked.</param>
    /// <param name="index">The next lookup's position in the map.</param>
    /// <param name="digest">The digest of the run.</param>
    /// <param name="candidates">The lookups of the first 64 positions that cover some glyph of the run.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="compared">The lookups compared so far.</param>
    /// <param name="differing">The lookups that differ so far.</param>
    /// <returns>The lookups compared and the lookups that differ.</returns>
    [<TailCall>]
    let rec private compareFrom (kept: GsubProgram) (views: GsubViews) (plain: GsubProgram) index digest candidates (flags: BufferFlags) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) compared differing =
        match index < plain.Lookups.Length with
        | false -> struct (compared, differing)
        | true ->
            let view = kept.ViewOf(views, index)
            let produce = flags ||| BufferFlags.ProduceUnsafeToConcat
            let same =
                sameLookup view.Exact.Item (plain.Build(index, false))
                && sameLookup view.Shown.Item (plain.Build(index, true))
                && handsOut kept index false (plain.Build(index, false))
                && handsOut kept index true (plain.Build(index, true))
                && view.IsReverse = isReverseVia plain index
                && isReverseVia kept index = isReverseVia plain index
                && admitsVia kept index digest candidates flags infos props false = admitsVia plain index digest candidates flags infos props false
                && admitsVia kept index digest candidates flags infos props true = admitsVia plain index digest candidates flags infos props true
                && admitsVia kept index digest candidates produce infos props false = admitsVia plain index digest candidates produce infos props false
                && admitsVia kept index digest UInt64.MaxValue produce infos props true = admitsVia plain index digest UInt64.MaxValue produce infos props true
            compareFrom kept views plain (index + 1) digest candidates flags infos props (compared + 1) (match same with | true -> differing | false -> differing + 1)

    /// <summary>Compares the lookups a cell keeps ready for a program with the lookups built afresh, for every lookup of the program.</summary>
    /// <param name="cell">The cell of the map or window.</param>
    /// <param name="plain">The program.</param>
    /// <param name="run">The run the admissions are compared on.</param>
    /// <returns>The lookups compared and the lookups that differ; none compared when the program keeps none ready.</returns>
    let private compareReady (cell: Memo<obj>) (plain: GsubProgram) (run: GlyphRun) =
        let kept = prepared cell plain
        match kept.Views with
        | ValueNone -> struct (0, 0)
        | ValueSome views ->
            let infos = run.Infos.AsSpan()
            let candidates = Sealed.lookupsCovering plain.GlyphLookupTable infos
            compareFrom kept views plain 0 (Digest.ofSpan infos) candidates run.Flags infos (run.Props.AsSpan()) 0 0

    /// <summary>Compares the lookups the plan's map keeps ready with the lookups built afresh, as <see cref="substituteApplicableOwnedSet"/> uses them.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="glyphLookups">The lookups of the map that cover each glyph.</param>
    /// <param name="run">The run the admissions are compared on.</param>
    /// <returns>The lookups compared and the lookups that differ.</returns>
    let internal readyDifferences (plan: OtShapePlan) (font: Font) (covered: ImmutableArray<ImmutableArray<uint64>>) (glyphLookups: ImmutableArray<uint64>) (run: GlyphRun) =
        compareReady plan.Map.GsubViews (GsubProgram(true, true, covered, glyphLookups, font.Ot.Gsub.Value, plan.Map.GsubLookups, plan.Map.GsubStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value)) run

    /// <summary>Compares the lookups a window keeps ready with the lookups built afresh, as <see cref="applyGsubWindowOwnedSet"/> uses them.</summary>
    /// <param name="covered">The glyph set of every lookup of the table.</param>
    /// <param name="window">The window.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run the admissions are compared on.</param>
    /// <returns>The lookups compared and the lookups that differ.</returns>
    let internal windowReadyDifferences (covered: ImmutableArray<ImmutableArray<uint64>>) (window: GsubWindow) (font: Font) (run: GlyphRun) =
        compareReady window.WindowViews (GsubProgram(true, true, covered, window.WindowGlyphLookups, font.Ot.Gsub.Value, window.WindowLookups, window.WindowStages, font.Ot.Gdef.Value, font.Ot.GsubChainSets.Value)) run
