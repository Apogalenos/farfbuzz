namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>Applying the kerx pair subtables (formats 0, 2 and 6) through the kern machine,
/// <c>KerxSubTableFormat0/2/6::apply</c> and <c>KerxTable::apply</c>.</summary>
[<RequireQualifiedAccess>]
module internal KerxPairs =
    /// <summary>The coverage bit <c>KerxSubTableHeader::Vertical</c>.</summary>
    [<Literal>]
    let private CoverageVertical = 0x80000000u

    /// <summary>The coverage bit <c>KerxSubTableHeader::CrossStream</c>.</summary>
    [<Literal>]
    let private CoverageCrossStream = 0x40000000u

    /// <summary>The coverage bit <c>KerxSubTableHeader::Backwards</c>.</summary>
    [<Literal>]
    let private CoverageBackwards = 0x10000000u

    /// <summary>The format 6 values-are-long flag, <c>KerxSubTableFormat6::ValuesAreLong</c>.</summary>
    [<Literal>]
    let private ValuesAreLong = 0x00000001u

    /// <summary>The cursive attachment type, <c>ATTACH_TYPE_CURSIVE</c>.</summary>
    [<Literal>]
    let private AttachTypeCursive = 0x02u

    /// <summary>The byte width of one format 0 pair, <c>KernPair::static_size</c>.</summary>
    [<Literal>]
    let private PairSize = 6

    /// <summary>The byte width of one FWORD, <c>FWORD::static_size</c>.</summary>
    [<Literal>]
    let private FwordSize = 2

    /// <summary>The byte width of one FWORD32, <c>FWORD32::static_size</c>.</summary>
    [<Literal>]
    let private Fword32Size = 4

    /// <summary>Whether two sorted disjoint range sets overlap, <c>hb_set_t::intersects</c>.</summary>
    /// <param name="a">The first set.</param>
    /// <param name="b">The second set.</param>
    /// <param name="i">The next range of the first.</param>
    /// <param name="j">The next range of the second.</param>
    /// <returns>True when they overlap.</returns>
    [<TailCall>]
    let rec private rangesIntersectFrom (a: ReadOnlySpan<CodepointRange>) (b: ReadOnlySpan<CodepointRange>) (i: int) (j: int) =
        match i >= a.Length || j >= b.Length with
        | true -> false
        | false ->
            match a[i].Last < b[j].First with
            | true -> rangesIntersectFrom a b (i + 1) j
            | false ->
                match b[j].Last < a[i].First with
                | true -> rangesIntersectFrom a b i (j + 1)
                | false -> true

    /// <summary>Marks the whole run unsafe to concat, <c>hb_buffer_t::unsafe_to_concat</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the flag set when requested.</returns>
    let private markUnsafeToConcat (run: GlyphRun) =
        match run.Flags &&& BufferFlags.ProduceUnsafeToConcat = BufferFlags.Default with
        | true -> run
        | false ->
            let infos =
                Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = info.Mask ||| uint32 GlyphFlags.UnsafeToConcat }) (run.Infos.AsSpan())
            { run with Infos = infos }

    /// <summary>Attaches every glyph into a cursive chain, <c>KerxTable::apply</c>'s cross-stream init.</summary>
    /// <param name="run">The run.</param>
    /// <param name="direction">The direction.</param>
    /// <returns>The run with the chain set.</returns>
    let private initChain (run: GlyphRun) (direction: Direction) =
        let chain = if Direction.isBackward direction then 1s else -1s
        let var = (AttachTypeCursive <<< 16) ||| uint32 (uint16 chain)
        let positions = Sealed.mapi (fun _ (position: GlyphPosition) -> { position with Var = position.Var ||| var }) (run.Positions.AsSpan())
        { run with Positions = positions }

    /// <summary>Collects every code point of a run into a sorted disjoint set, <c>setup_buffer_glyph_set</c>.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <returns>The set.</returns>
    let private collectCodepoints (infos: ReadOnlySpan<GlyphInfo>) = Sealed.glyphRanges infos

    /// <summary>Applies the tuple-count indirection of a pair value, <c>kerxTupleKern</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="value">The pair value, an offset when tuple count is non-zero.</param>
    /// <param name="tupleBase">The byte offset the tuple offset is relative to.</param>
    /// <returns>The resolved value, or zero when the tuple array is out of range.</returns>
    let private tupleKern (subtable: KerxSubtable) (value: int) (tupleBase: int) =
        match subtable.TupleCount with
        | 0u -> value
        | _ ->
            let offset = uint32 value
            let pv = int64 tupleBase + int64 offset
            let bytes = uint64 subtable.TupleCount * 2UL
            match bytes > uint64 Int32.MaxValue || not (Sanitize.fits subtable.RangeEnd pv (int bytes)) with
            | true -> 0
            | false -> int (Binary.i16 subtable.Body.Span (int pv))

    /// <summary>Binary-searches a format 0 pair array, <c>KernPair::cmp</c> over <c>hb_bsearch_impl</c>.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="pairsAt">The byte offset of the first pair.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The pair value, zero when the pair is absent.</returns>
    [<TailCall>]
    let rec private bsearchPairs (span: ReadOnlySpan<byte>) (pairsAt: int) (left: uint32) (right: uint32) (lo: int) (hi: int) =
        match lo > hi with
        | true -> 0
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            let at = pairsAt + mid * PairSize
            let pairLeft = uint32 (Binary.u16 span at)
            let pairRight = uint32 (Binary.u16 span (at + 2))
            match pairLeft > left, pairLeft < left with
            | true, _ -> bsearchPairs span pairsAt left right lo (mid - 1)
            | false, true -> bsearchPairs span pairsAt left right (mid + 1) hi
            | false, false ->
                match pairRight > right, pairRight < right with
                | true, _ -> bsearchPairs span pairsAt left right lo (mid - 1)
                | false, true -> bsearchPairs span pairsAt left right (mid + 1) hi
                | false, false -> int (Binary.i16 span (at + 4))

    /// <summary>The kerning of a format 0 pair, <c>KerxSubTableFormat0::get_kerning</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning.</returns>
    let private getKerning0 (subtable: KerxSubtable) (left: uint32) (right: uint32) =
        let span = subtable.Body.Span
        let count = int (Binary.u32 span (subtable.At + 12))
        let value = bsearchPairs span (subtable.At + 28) left right 0 (count - 1)
        tupleKern subtable value subtable.At

    /// <summary>A lookup, or the null lookup for a null offset.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="offset">The lookup offset from the subtable.</param>
    /// <param name="valueSize">The value width.</param>
    /// <returns>The lookup.</returns>
    let private lookupAt (subtable: KerxSubtable) (offset: uint32) (valueSize: int) =
        match offset with
        | 0u -> { Lookup.Table = subtable.Body; At = 0; Format = LookupFormat.NullLookup; ValueSize = valueSize }
        | _ -> AatLookup.read subtable.Body (subtable.At + int offset) valueSize

    /// <summary>The kerning of a format 2 pair, <c>KerxSubTableFormat2::get_kerning</c>. The class tables are
    /// extended lookups; the summed byte offsets index the array through <c>byteOffsetToIndex</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning.</returns>
    let private getKerning2 (subtable: KerxSubtable) (numGlyphs: int) (left: uint32) (right: uint32) =
        let span = subtable.Body.Span
        let at = subtable.At
        let leftLookup = lookupAt subtable (Binary.u32 span (at + 16)) FwordSize
        let rightLookup = lookupAt subtable (Binary.u32 span (at + 20)) FwordSize
        let l = uint32 (AatLookup.classOf leftLookup left numGlyphs 0us)
        let r = uint32 (AatLookup.classOf rightLookup right numGlyphs 0us)
        let arrayOffset = Binary.u32 span (at + 24)
        let index = (l + r) / 2u
        let vAt = int64 at + int64 arrayOffset + int64 index * 2L
        match Sanitize.fits subtable.RangeEnd vAt FwordSize with
        | false -> 0
        | true -> tupleKern subtable (int (Binary.i16 span (int vAt))) at

    /// <summary>The kerning of a format 6 pair, <c>KerxSubTableFormat6::get_kerning</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning.</returns>
    let private getKerning6 (subtable: KerxSubtable) (numGlyphs: int) (left: uint32) (right: uint32) =
        let span = subtable.Body.Span
        let at = subtable.At
        let long = Binary.u32 span (at + 12) &&& ValuesAreLong <> 0u
        let valueSize = if long then Fword32Size else FwordSize
        let rowOffset = Binary.u32 span (at + 20)
        let columnOffset = Binary.u32 span (at + 24)
        let arrayOffset = Binary.u32 span (at + 28)
        let vectorOffset = Binary.u32 span (at + 32)
        let l = AatLookup.valueOrNull (lookupAt subtable rowOffset valueSize) left numGlyphs
        let r = AatLookup.valueOrNull (lookupAt subtable columnOffset valueSize) right numGlyphs
        let offset = l + r
        let tupleBase = at + int vectorOffset
        match long with
        | true ->
            match offset < l || offset > 0x3FFFFFFFu with
            | true -> 0
            | false ->
                let vAt = int64 at + int64 arrayOffset + int64 (offset * 4u)
                match Sanitize.fits subtable.RangeEnd vAt Fword32Size with
                | false -> 0
                | true -> tupleKern subtable (int (Binary.u32 span (int vAt))) tupleBase
        | false ->
            let vAt = int64 at + int64 arrayOffset + int64 (offset * 2u)
            match Sanitize.fits subtable.RangeEnd vAt FwordSize with
            | false -> 0
            | true -> tupleKern subtable (int (Binary.i16 span (int vAt))) tupleBase

    /// <summary>The kerning of a pair through one subtable, <c>KerxSubTable::get_kerning</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning.</returns>
    let private getKerning (subtable: KerxSubtable) (numGlyphs: int) (left: uint32) (right: uint32) =
        match subtable.Format with
        | 0 -> getKerning0 subtable left right
        | 2 -> getKerning2 subtable numGlyphs left right
        | 6 -> getKerning6 subtable numGlyphs left right
        | _ -> 0

    /// <summary>The pair provider of one kerx format 0/2/6 subtable, its <c>accelerator_t</c>: kerning is zero unless
    /// both glyphs are in the subtable's sets, which is semantic for format 2 where an unlisted glyph has class 0.</summary>
    [<Struct>]
    type KerxPairProvider =
        val Subtable: KerxSubtable
        val NumGlyphs: int
        new (subtable, numGlyphs) = { Subtable = subtable; NumGlyphs = numGlyphs }
        interface Kern.IKernProvider with
            member this.GetKerning (left, right) =
                match MachineGate.rangesHas (this.Subtable.FirstSet.AsSpan()) left 0 (this.Subtable.FirstSet.Length - 1)
                      && MachineGate.rangesHas (this.Subtable.SecondSet.AsSpan()) right 0 (this.Subtable.SecondSet.Length - 1) with
                | false -> 0
                | true -> getKerning this.Subtable this.NumGlyphs left right
            member this.MayKern left = MachineGate.rangesHas (this.Subtable.FirstSet.AsSpan()) left 0 (this.Subtable.FirstSet.Length - 1)

    /// <summary>Reverses a run back when it is currently reversed, <c>KerxTable::apply</c>'s closing reverse.</summary>
    /// <param name="reversed">Whether the run is reversed.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run in logical order.</returns>
    let private unReverse (reversed: bool) (run: GlyphRun) =
        match reversed with
        | true -> GlyphRun.reverse run
        | false -> run

    /// <summary>Applies one subtable, <c>KerxSubTable::dispatch</c>.</summary>
    /// <param name="owned">Whether the run's arrays are the caller's own, so that a pair subtable writes them in place.</param>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the run is reversed.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let private applySubtable (owned: bool) (runner: MachineRunner) (plan: OtShapePlan) (font: Font) (horizontal: bool) (reversed: bool) (numGlyphs: int) (subtable: KerxSubtable) (run: GlyphRun) =
        match subtable.Format with
        | 0 | 2 | 6 ->
            match subtable.Coverage &&& CoverageBackwards <> 0u with
            | true -> run
            | false ->
                let crossStream = subtable.Coverage &&& CoverageCrossStream <> 0u
                match subtable.Pairs.Value, owned with
                | ValueSome index, true -> Kern.applyProviderOwned plan font horizontal crossStream (Kern.IndexedPairProvider(index)) run
                | ValueSome index, false -> Kern.applyProviderWith runner plan font horizontal crossStream (Kern.IndexedPairProvider(index)) run
                | ValueNone, true -> Kern.applyProviderOwned plan font horizontal crossStream (KerxPairProvider(subtable, numGlyphs)) run
                | ValueNone, false -> Kern.applyProviderWith runner plan font horizontal crossStream (KerxPairProvider(subtable, numGlyphs)) run
        | 1 ->
            match subtable.Machine with
            | ValueNone -> run
            | ValueSome machine ->
                let spec =
                    { Extended = true
                      TableBytes = subtable.Body
                      RangeEnd = subtable.RangeEnd
                      MachineAt = subtable.At + 12
                      KernActionAt = subtable.KernActionAt
                      TupleCount = subtable.TupleCount }
                KerxStateMachineApply.applyWith runner subtable.Dense.Value spec machine plan font horizontal (subtable.Coverage &&& CoverageCrossStream <> 0u) run
        | 4 ->
            match subtable.Machine with
            | ValueNone -> run
            | ValueSome machine ->
                let spec =
                    { TableBytes = subtable.Body
                      RangeEnd = subtable.RangeEnd
                      ActionsAt = int64 subtable.KernActionAt
                      ActionType = int (Binary.u32 subtable.Body.Span (subtable.At + 28) >>> 30) }
                KerxFormat4Apply.applyWith runner subtable.Dense.Value spec machine font horizontal reversed run
        | _ -> run

    /// <summary>What applying one charged subtable produced: the run continues, or it is done.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private KerxOutcome =
        | KerxContinue of reversed: bool * seenCrossStream: bool * run: GlyphRun
        | KerxDone of run: GlyphRun

    /// <summary>Charges and applies one subtable, <c>KerxTable::apply</c>'s per-subtable body.</summary>
    /// <param name="owned">Whether the run's arrays are the caller's own.</param>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="reversed">Whether the run is reversed.</param>
    /// <param name="seenCrossStream">Whether a cross-stream subtable already ran.</param>
    /// <param name="run">The run.</param>
    /// <returns>The outcome.</returns>
    let private applyOne (owned: bool) (runner: MachineRunner) (plan: OtShapePlan) (font: Font) (direction: Direction) (numGlyphs: int) (subtable: KerxSubtable) (reversed: bool) (seenCrossStream: bool) (run: GlyphRun) =
        let charged = AatApply.chargeBufferPass run
        match charged.Successful with
        | false -> KerxDone (unReverse reversed charged)
        | true ->
            let horizontal = Direction.isHorizontal direction
            let run, seenCrossStream =
                match subtable.Coverage &&& CoverageCrossStream <> 0u && not seenCrossStream with
                | true -> initChain charged direction, true
                | false -> charged, seenCrossStream
            let reverse = (subtable.Coverage &&& CoverageBackwards <> 0u) <> Direction.isBackward direction
            let prepared, reversed =
                match reverse <> reversed with
                | true -> GlyphRun.reverse run, not reversed
                | false -> run, reversed
            let applied = applySubtable owned runner plan font horizontal reversed numGlyphs subtable prepared
            match applied.Successful with
            | false -> KerxDone (unReverse reversed applied)
            | true -> KerxContinue (reversed, seenCrossStream, applied)

    /// <summary>Applies the subtables in order, <c>KerxTable::apply</c>'s loop.</summary>
    /// <param name="owned">Whether the run's arrays are the caller's own.</param>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="subtables">The subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="reversed">Whether the run is reversed.</param>
    /// <param name="seenCrossStream">Whether a cross-stream subtable already ran.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="set">The glyph set of the run, empty when the bit table is used.</param>
    /// <param name="words">The bits of the subtables whose first glyph set holds a glyph of the run, empty when the table has no bit table.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every subtable.</returns>
    [<TailCall>]
    let rec private applySubtables (owned: bool) (runner: MachineRunner) (plan: OtShapePlan) (font: Font) (direction: Direction) (numGlyphs: int)
        (subtables: ImmutableArray<KerxSubtable>) (index: int) (reversed: bool) (seenCrossStream: bool) (usingSet: bool) (set: ImmutableArray<CodepointRange>) (words: ImmutableArray<uint64>) (run: GlyphRun) =
        match index >= subtables.Length || not run.Successful with
        | true -> unReverse reversed run
        | false ->
            let subtable = subtables[index]
            let horizontal = Direction.isHorizontal direction
            let isHorizontal = subtable.Coverage &&& CoverageVertical = 0u
            let intersects =
                match words.Length > 0, usingSet with
                | true, _ -> (words[index / 64] >>> (index % 64)) &&& 1UL <> 0UL
                | false, true -> rangesIntersectFrom (set.AsSpan()) (subtable.FirstSet.AsSpan()) 0 0
                | false, false -> MachineGate.scanIntersects (run.Infos.AsSpan()) (subtable.FirstSet.AsSpan()) 0
            match horizontal <> isHorizontal || not intersects with
            | true -> applySubtables owned runner plan font direction numGlyphs subtables (index + 1) reversed seenCrossStream usingSet set words run
            | false ->
                match applyOne owned runner plan font direction numGlyphs subtable reversed seenCrossStream run with
                | KerxDone run -> run
                | KerxContinue (reversed, seenCrossStream, run) -> applySubtables owned runner plan font direction numGlyphs subtables (index + 1) reversed seenCrossStream usingSet set words run

    /// <summary>Applies the kerx table, <c>hb_aat_layout_position</c>, with the arrays of the run the caller's own or not.</summary>
    /// <param name="owned">Whether the run's infos, properties and positions were made by the caller in this call and are shared with
    /// no one, so that the pair subtables and the unsafe-to-concat marks write them in place.</param>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let private applyFrom (owned: bool) (runner: MachineRunner) (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match run.Successful with
        | false -> run
        | true ->
            let kerx = font.Ot.Kerx.Value
            match Kerx.hasData kerx with
            | false -> run
            | true ->
                let direction = run.SegmentProperties.Direction
                let numGlyphs = font.Ot.Face.GlyphCount
                let marked =
                    match owned && run.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default with
                    | true ->
                        Sealed.markConcatOwned run.Infos
                        run
                    | false -> markUnsafeToConcat run
                let usingSet = run.Infos.Length >= 4
                let candidates = match usingSet with | true -> kerx.Candidates.Value | false -> ValueNone
                let words =
                    match candidates with
                    | ValueSome table -> Sealed.candidateBits (Sealed.init table.Words (fun _ -> 0UL)) table.Bits table.Top (marked.Infos.AsSpan())
                    | ValueNone -> ImmutableArray<uint64>.Empty
                let set =
                    match usingSet && words.Length = 0 with
                    | true -> collectCodepoints (marked.Infos.AsSpan())
                    | false -> ImmutableArray.Empty
                applySubtables owned runner plan font direction numGlyphs kerx.Subtables 0 false false usingSet set words marked

    /// <summary>Applies the kerx table, <c>hb_aat_layout_position</c>.</summary>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let applyWith (runner: MachineRunner) (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        applyFrom false runner plan font run

    /// <summary>Applies the kerx table, <c>KerxTable::apply</c>, on the kernel, on a run whose infos, properties and positions the caller
    /// made in this call and shares with no one: the pair subtables write them in place instead of copying them.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The kerned run.</returns>
    let applyOwned (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        applyFrom true Kernel plan font run

    /// <summary>Applies the kerx table, <c>KerxTable::apply</c>, on the kernel.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The kerned run.</returns>
    let apply (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        applyWith Kernel plan font run
