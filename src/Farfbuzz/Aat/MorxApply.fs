namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>Applying the morx and mort tables, <c>mortmorx::apply</c> and <c>Chain::apply</c>.</summary>
[<RequireQualifiedAccess>]
module internal MorxApply =
    /// <summary>The subtable coverage bit <c>ChainSubtable::Coverage::Vertical</c>.</summary>
    [<Literal>]
    let private CoverageVertical = 0x80uy

    /// <summary>The subtable coverage bit <c>ChainSubtable::Coverage::Backwards</c>.</summary>
    [<Literal>]
    let private CoverageBackwards = 0x40uy

    /// <summary>The subtable coverage bit <c>ChainSubtable::Coverage::AllDirections</c>.</summary>
    [<Literal>]
    let private CoverageAllDirections = 0x20uy

    /// <summary>The subtable coverage bit <c>ChainSubtable::Coverage::Logical</c>.</summary>
    [<Literal>]
    let private CoverageLogical = 0x10uy

    /// <summary>Whether a glyph lies in a sorted disjoint range set, <c>hb_set_t::has</c>.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>True when the glyph is covered.</returns>
    [<TailCall>]
    let rec private rangesHas (ranges: ReadOnlySpan<CodepointRange>) (glyph: uint32) (lo: int) (hi: int) =
        match lo > hi with
        | true -> false
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match glyph < ranges[mid].First, glyph <= ranges[mid].Last with
            | true, _ -> rangesHas ranges glyph lo (mid - 1)
            | false, true -> true
            | false, false -> rangesHas ranges glyph (mid + 1) hi

    /// <summary>Scans the buffer for a glyph in the initial set, <c>buffer_intersects_machine</c>'s short path.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="initial">The initial glyph set.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>True when the buffer intersects.</returns>
    [<TailCall>]
    let rec private scanIntersects (infos: ReadOnlySpan<GlyphInfo>) (initial: ReadOnlySpan<CodepointRange>) (index: int) =
        match index >= infos.Length with
        | true -> false
        | false ->
            match rangesHas initial infos[index].Codepoint 0 (initial.Length - 1) with
            | true -> true
            | false -> scanIntersects infos initial (index + 1)

    /// <summary>Scans written glyphs for one in the initial set.</summary>
    /// <param name="glyphs">The glyphs.</param>
    /// <param name="initial">The initial glyph set.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>True when a glyph is in the set.</returns>
    [<TailCall>]
    let rec private glyphsIntersect (glyphs: ReadOnlySpan<uint32>) (initial: ReadOnlySpan<CodepointRange>) (index: int) =
        match index >= glyphs.Length with
        | true -> false
        | false ->
            match rangesHas initial glyphs[index] 0 (initial.Length - 1) with
            | true -> true
            | false -> glyphsIntersect glyphs initial (index + 1)

    /// <summary>The glyphs the buffer has had, <c>buffer_glyph_set</c>: the glyphs it started with and those the subtables
    /// wrote, with, for a chain that has a bit table, the bits of the subtables that can start on any of them.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private GlyphSet =
        { Originals: ImmutableArray<GlyphInfo>
          Added: ImmutableArray<uint32>
          Words: ImmutableArray<uint64> }

    /// <summary>The bits of the subtables of a chain that can start on a glyph set.</summary>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="set">The glyph set.</param>
    /// <returns>The words, empty when the chain has no table.</returns>
    let private chainWords (candidates: ChainCandidates voption) (set: GlyphSet) =
        match candidates with
        | ValueNone -> ImmutableArray<uint64>.Empty
        | ValueSome table ->
            let started = Sealed.candidateBits (Sealed.init table.Words (fun _ -> 0UL)) table.Bits table.Top (set.Originals.AsSpan())
            Sealed.candidateBitsOfGlyphs started table.Bits table.Top (set.Added.AsSpan())

    /// <summary>Whether the buffer intersects a subtable's initial glyph set, <c>buffer_intersects_machine</c>.</summary>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="set">The growing glyph set.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="index">The subtable's index in its chain.</param>
    /// <param name="run">The run.</param>
    /// <param name="initial">The initial glyph set.</param>
    /// <returns>True when the buffer intersects.</returns>
    let private intersects (usingSet: bool) (set: GlyphSet) (candidates: ChainCandidates voption) (index: int) (run: GlyphRun) (initial: ImmutableArray<CodepointRange>) =
        match usingSet, candidates with
        | false, _ -> scanIntersects (run.Infos.AsSpan()) (initial.AsSpan()) 0
        | true, ValueSome _ -> (set.Words[index / 64] >>> (index % 64)) &&& 1UL <> 0UL
        | true, ValueNone -> scanIntersects (set.Originals.AsSpan()) (initial.AsSpan()) 0 || glyphsIntersect (set.Added.AsSpan()) (initial.AsSpan()) 0

    /// <summary>Whether any range carries the subtable's flags, <c>Chain::apply</c>'s range test.</summary>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="subFlags">The subtable feature flags.</param>
    /// <param name="index">The next range.</param>
    /// <returns>True when a range enables the subtable.</returns>
    [<TailCall>]
    let rec private anyRangeEnabled (ranges: ReadOnlySpan<RangeFlags>) (subFlags: uint32) (index: int) =
        match index >= ranges.Length with
        | true -> false
        | false ->
            match ranges[index].Flags &&& subFlags <> 0u with
            | true -> true
            | false -> anyRangeEnabled ranges subFlags (index + 1)

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

    /// <summary>Adds written glyphs to the growing glyph set and to the bits of the chain's subtables.</summary>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="existing">The current set.</param>
    /// <param name="added">The glyphs to add.</param>
    /// <returns>The set with the glyphs.</returns>
    let private setWith (candidates: ChainCandidates voption) (existing: GlyphSet) (added: ImmutableArray<uint32>) =
        match added.Length with
        | 0 -> existing
        | _ ->
            let words =
                match candidates with
                | ValueSome table -> Sealed.candidateBitsOfGlyphs existing.Words table.Bits table.Top (added.AsSpan())
                | ValueNone -> existing.Words
            { Originals = existing.Originals; Added = Sealed.appendGlyphs existing.Added added; Words = words }

    /// <summary>Builds the driver config for a state-machine subtable.</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <param name="machine">The decoded state machine.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="subtableFlags">The subtable feature flags.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="kind">The kind, for the start-state end-of-text classifier.</param>
    /// <returns>The config.</returns>
    let private configOf<'Kind when 'Kind :> IEntryClassifier> (machine: StateMachine) (ranges: ImmutableArray<RangeFlags>) subtableFlags numGlyphs (kind: 'Kind) =
        { Machine = machine
          Ranges = ranges
          SubtableFlags = subtableFlags
          NumGlyphs = numGlyphs
          StartSafeToBreakAtEndOfText = not (kind.IsActionable (StateTable.entry machine StateTable.StartOfText StateTable.ClassEndOfText)) }

    /// <summary>The entry flags that make the null transition of a contextual, ligature or insertion subtable change the
    /// context: the mark or component flag.</summary>
    [<Literal>]
    let private ActionNullMask = 0x8000us

    /// <summary>The entry flags that make the null transition of a rearrangement subtable change the context: mark first
    /// and mark last.</summary>
    [<Literal>]
    let private RearrangementNullMask = 0xA000us

    /// <summary>Runs a state-machine subtable pass and returns the run and the finished context.</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none.</param>
    /// <param name="nullMask">The entry flags that make the null transition change the context.</param>
    /// <param name="quiet">Whether the null transition stands in for the transition of an entry that is not actionable.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="seed">The initial context.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run and the finished context.</returns>
    let private runMachine<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (runner: MachineRunner) (dense: DenseMachine voption) (nullMask: uint16) (quiet: bool) (config: DriverConfig) (kind: 'Kind) (seed: 'Context) (run: GlyphRun) =
        AatKernel.run runner dense config kind nullMask quiet seed run

    /// <summary>Applies one subtable, <c>ChainSubtable::dispatch</c>.</summary>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the written glyphs feed the growing glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run and the glyphs written.</returns>
    let private applySubtable (runner: MachineRunner) (subtable: ChainSubtable) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool) (run: GlyphRun) =
        match subtable.Kind with
        | Noncontextual ->
            let lookup = AatLookup.read subtable.Body 0 2
            MorxNoncontextualDirect.run runner subtable.Replacements.Value lookup numGlyphs ranges subtable.SubFeatureFlags context usingSet run
        | Rearrangement ->
            match subtable.Machine with
            | ValueNone -> struct (run, ImmutableArray.Empty)
            | ValueSome machine ->
                let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs (RearrangementKind())
                let struct (result, _) = runMachine runner subtable.Dense.Value RearrangementNullMask true config (RearrangementKind()) MorxRearrangement.seed run
                struct (result, ImmutableArray.Empty)
        | Contextual ->
            match subtable.Machine with
            | ValueNone -> struct (run, ImmutableArray.Empty)
            | ValueSome machine ->
                let kind =
                    { ContextualKind.Extended = machine.IsExtended
                      Body = subtable.Body
                      SubstitutionTablesAt = subtable.SubstitutionTablesAt
                      NumGlyphs = numGlyphs
                      Context = context }
                let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs kind
                let struct (result, written, hasDeleted) = MorxContextualDirect.run runner subtable.Dense.Value config kind usingSet run
                let scratch = if hasDeleted then result.ScratchFlags ||| AatApply.aatHasDeleted else result.ScratchFlags
                struct ({ result with ScratchFlags = scratch }, written)
        | Ligature ->
            match subtable.Machine with
            | ValueNone -> struct (run, ImmutableArray.Empty)
            | ValueSome machine ->
                let extended = machine.IsExtended
                let ligActionAt = if extended then int64 (Binary.u32 subtable.Body.Span 16) else int64 (Binary.u16 subtable.Body.Span 8)
                let componentAt = if extended then int64 (Binary.u32 subtable.Body.Span 20) else int64 (Binary.u16 subtable.Body.Span 10)
                let ligatureAt = if extended then int64 (Binary.u32 subtable.Body.Span 24) else int64 (Binary.u16 subtable.Body.Span 12)
                let kind =
                    { LigatureKind.Extended = extended
                      Body = subtable.Body
                      LigActionAt = ligActionAt
                      ComponentAt = componentAt
                      LigatureAt = ligatureAt
                      Context = context }
                let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs kind
                let struct (result, written, hasDeleted) = MorxLigatureDirect.run runner subtable.Dense.Value config kind usingSet run
                let scratch = if hasDeleted then result.ScratchFlags ||| AatApply.aatHasDeleted else result.ScratchFlags
                struct ({ result with ScratchFlags = scratch }, written)
        | Insertion ->
            match subtable.Machine with
            | ValueNone -> struct (run, ImmutableArray.Empty)
            | ValueSome machine ->
                let extended = machine.IsExtended
                let insertionActionAt =
                    if extended then int64 (Binary.u32 subtable.Body.Span 16) else int64 (Binary.u16 subtable.Body.Span 8)
                let kind =
                    { InsertionKind.Body = subtable.Body
                      InsertionActionAt = insertionActionAt
                      Context = context }
                let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs kind
                let struct (result, finalContext) = runMachine runner subtable.Dense.Value ActionNullMask false config kind (MorxInsertion.seed usingSet) run
                let written = PointBuilder.publish finalContext.Written
                let scratch = if finalContext.HasDeleted then result.ScratchFlags ||| AatApply.aatHasDeleted else result.ScratchFlags
                struct ({ result with ScratchFlags = scratch }, written)
        | UnknownKind _ -> struct (run, ImmutableArray.Empty)

    /// <summary>Reverses a run back when it is currently reversed, <c>Chain::apply</c>'s closing reverse.</summary>
    /// <param name="reversed">Whether the run is reversed.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run in logical order.</returns>
    let private unReverse (reversed: bool) (run: GlyphRun) =
        match reversed with
        | true -> GlyphRun.reverse run
        | false -> run

    /// <summary>The reverse flag of a subtable, <c>Chain::apply</c>'s reverse decision.</summary>
    /// <param name="coverage">The coverage byte.</param>
    /// <param name="direction">The direction.</param>
    /// <returns>Whether to apply in reverse.</returns>
    let private reverseOf (coverage: byte) (direction: Direction) =
        match coverage &&& CoverageLogical <> 0uy with
        | true -> coverage &&& CoverageBackwards <> 0uy
        | false -> (coverage &&& CoverageBackwards <> 0uy) <> Direction.isBackward direction

    /// <summary>Reverses the run for a subtable when its reverse flag differs from the current state.</summary>
    /// <param name="reverse">The subtable's reverse flag.</param>
    /// <param name="reversed">The current reversed state.</param>
    /// <param name="charged">The charged run.</param>
    /// <returns>The prepared run and the new reversed state.</returns>
    let private prepareRun (reverse: bool) (reversed: bool) (charged: GlyphRun) =
        match reverse <> reversed with
        | true -> GlyphRun.reverse charged, not reversed
        | false -> charged, reversed

    /// <summary>Adds the written glyphs to the growing set when it is in use.</summary>
    /// <param name="usingSet">Whether the set is in use.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="written">The glyphs written.</param>
    /// <param name="set">The current set.</param>
    /// <returns>The updated set.</returns>
    let private updateSet (usingSet: bool) (candidates: ChainCandidates voption) (written: ImmutableArray<uint32>) (set: GlyphSet) =
        match usingSet && written.Length > 0 with
        | true -> setWith candidates set written
        | false -> set

    /// <summary>What applying one subtable produced: either the chain continues with the next subtable, or it is done.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private ChainOutcome =
        | ChainContinue of reversed: bool * set: GlyphSet * run: GlyphRun
        | ChainDone of run: GlyphRun * set: GlyphSet

    /// <summary>Applies one charged subtable and reports whether the chain continues, <c>Chain::apply</c>'s apply step.</summary>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="reverse">The reverse flag.</param>
    /// <param name="reversed">The current reversed state.</param>
    /// <param name="set">The growing glyph set.</param>
    /// <param name="charged">The charged run.</param>
    /// <returns>The outcome.</returns>
    let private applyAfterCharge (runner: MachineRunner) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool)
        (candidates: ChainCandidates voption) (subtable: ChainSubtable) (reverse: bool) (reversed: bool) (set: GlyphSet) (charged: GlyphRun) =
        let prepared, reversed = prepareRun reverse reversed charged
        let struct (applied, written) = applySubtable runner subtable numGlyphs ranges context usingSet prepared
        let set = updateSet usingSet candidates written set
        match applied.Successful with
        | false -> ChainDone (unReverse reversed applied, set)
        | true -> ChainContinue (reversed, set, applied)

    /// <summary>Runs the range, direction and intersection checks of one subtable, then charges and applies it,
    /// <c>Chain::apply</c>'s per-subtable body.</summary>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="subtables">The chain's subtables.</param>
    /// <param name="index">The current subtable.</param>
    /// <param name="reversed">Whether the run is currently reversed.</param>
    /// <param name="set">The growing glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The outcome.</returns>
    let private applyChainStep (runner: MachineRunner) (segment: SegmentProperties) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool)
        (candidates: ChainCandidates voption) (subtables: ImmutableArray<ChainSubtable>) (index: int) (reversed: bool) (set: GlyphSet) (run: GlyphRun) =
        let subtable = subtables[index]
        let coverage = subtable.Coverage
        let rangeEnabled = anyRangeEnabled (ranges.AsSpan()) subtable.SubFeatureFlags 0
        let directionOk =
            coverage &&& CoverageAllDirections <> 0uy
            || (not (Direction.isHorizontal segment.Direction)) = (coverage &&& CoverageVertical <> 0uy)
        let machineIntersects = intersects usingSet set candidates index run subtable.InitialGlyphs
        match rangeEnabled && directionOk && machineIntersects with
        | false -> ChainContinue (reversed, set, run)
        | true ->
            let charged = AatApply.chargeBufferPass run
            match charged.Successful with
            | false -> ChainDone (unReverse reversed charged, set)
            | true -> applyAfterCharge runner numGlyphs ranges context usingSet candidates subtable (reverseOf coverage segment.Direction) reversed set charged

    /// <summary>Applies the subtables of one chain, <c>Chain::apply</c>.</summary>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="subtables">The chain's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="reversed">Whether the run is currently reversed.</param>
    /// <param name="set">The growing glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run and the glyph set.</returns>
    [<TailCall>]
    let rec private applyChainSubtables (runner: MachineRunner) (segment: SegmentProperties) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool)
        (candidates: ChainCandidates voption) (subtables: ImmutableArray<ChainSubtable>) (index: int) (reversed: bool) (set: GlyphSet) (run: GlyphRun) =
        match index >= subtables.Length with
        | true -> struct (unReverse reversed run, set)
        | false ->
            match applyChainStep runner segment numGlyphs ranges context usingSet candidates subtables index reversed set run with
            | ChainContinue (reversed, set, run) -> applyChainSubtables runner segment numGlyphs ranges context usingSet candidates subtables (index + 1) reversed set run
            | ChainDone (run, set) -> struct (run, set)

    /// <summary>Applies every chain of a table, <c>mortmorx::apply</c>.</summary>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="map">The compiled AAT map.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="set">The growing glyph set.</param>
    /// <param name="chainIndex">The next chain.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    [<TailCall>]
    let rec private applyChains (runner: MachineRunner) (table: Morx) (map: AatMap) (segment: SegmentProperties) (numGlyphs: int) (context: AatContext) (usingSet: bool)
        (set: GlyphSet) (chainIndex: int) (run: GlyphRun) =
        match chainIndex >= table.Chains.Length with
        | true -> run
        | false ->
            match run.Successful with
            | false -> run
            | true ->
                let chain = table.Chains[chainIndex]
                let candidates = chain.Candidates.Value
                let started = match usingSet with | true -> { set with Words = chainWords candidates set } | false -> set
                let struct (applied, set) = applyChainSubtables runner segment numGlyphs map.ChainFlags[chainIndex] context usingSet candidates chain.Subtables 0 false started run
                applyChains runner table map segment numGlyphs context usingSet set (chainIndex + 1) applied

    /// <summary>Applies the morx or mort table, <c>mortmorx::apply</c>, with the implementation of the state-table subtables chosen.</summary>
    /// <param name="runner">Which implementation runs the state-table subtables.</param>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="map">The compiled AAT map.</param>
    /// <param name="face">The face.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let applyWith (runner: MachineRunner) (table: Morx) (map: AatMap) (face: OtFace) (segment: SegmentProperties) (run: GlyphRun) =
        match run.Successful with
        | false -> run
        | true ->
            let numGlyphs = face.Face.GlyphCount
            let context: AatContext = { Gdef = face.Gdef.Value }
            let usingSet = not table.IsMort && run.Infos.Length >= 4
            let marked = markUnsafeToConcat run
            let set: GlyphSet = { Originals = marked.Infos; Added = ImmutableArray<uint32>.Empty; Words = ImmutableArray<uint64>.Empty }
            applyChains runner table map segment numGlyphs context usingSet set 0 marked

    /// <summary>Applies a rearrangement subtable to the working set of a line.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private applyRearrangementSession (session: Sealed.AatSession) (subtable: ChainSubtable) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) =
        match subtable.Machine with
        | ValueNone -> ()
        | ValueSome machine ->
            let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs (RearrangementKind())
            MorxRearrangementDirect.runSession session subtable.Dense.Value config

    /// <summary>Applies a contextual subtable to the working set of a line.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private applyContextualSession (session: Sealed.AatSession) (subtable: ChainSubtable) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) =
        match subtable.Machine with
        | ValueNone -> false
        | ValueSome machine ->
            let kind =
                { ContextualKind.Extended = machine.IsExtended
                  Body = subtable.Body
                  SubstitutionTablesAt = subtable.SubstitutionTablesAt
                  NumGlyphs = numGlyphs
                  Context = context }
            let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs kind
            let substitutions = match subtable.Replacements.Value with | ValueSome compiled -> compiled | ValueNone -> ImmutableArray.Empty
            MorxContextualDirect.runSession session subtable.Dense.Value config kind substitutions

    /// <summary>Applies a ligature subtable to the working set of a line.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the written glyphs feed the growing glyph set.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private applyLigatureSession (session: Sealed.AatSession) (subtable: ChainSubtable) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool) =
        match subtable.Machine with
        | ValueNone -> false
        | ValueSome machine ->
            let extended = machine.IsExtended
            let ligActionAt = if extended then int64 (Binary.u32 subtable.Body.Span 16) else int64 (Binary.u16 subtable.Body.Span 8)
            let componentAt = if extended then int64 (Binary.u32 subtable.Body.Span 20) else int64 (Binary.u16 subtable.Body.Span 10)
            let ligatureAt = if extended then int64 (Binary.u32 subtable.Body.Span 24) else int64 (Binary.u16 subtable.Body.Span 12)
            let kind =
                { LigatureKind.Extended = extended
                  Body = subtable.Body
                  LigActionAt = ligActionAt
                  ComponentAt = componentAt
                  LigatureAt = ligatureAt
                  Context = context }
            let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs kind
            MorxLigatureDirect.runSession session subtable.Dense.Value config kind usingSet

    /// <summary>Applies an insertion subtable to the working set of a line.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the written glyphs feed the growing glyph set.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private applyInsertionSession (session: Sealed.AatSession) (subtable: ChainSubtable) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool) =
        match subtable.Machine with
        | ValueNone -> false
        | ValueSome machine ->
            let extended = machine.IsExtended
            let insertionActionAt =
                if extended then int64 (Binary.u32 subtable.Body.Span 16) else int64 (Binary.u16 subtable.Body.Span 8)
            let kind =
                { InsertionKind.Body = subtable.Body
                  InsertionActionAt = insertionActionAt
                  Context = context }
            let config = configOf machine ranges subtable.SubFeatureFlags numGlyphs kind
            MorxInsertionDirect.runSession session subtable.Dense.Value config kind usingSet

    /// <summary>Applies a noncontextual subtable to the working set of a line.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private applyNoncontextualSession (session: Sealed.AatSession) (subtable: ChainSubtable) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) =
        let lookup = AatLookup.read subtable.Body 0 2
        MorxNoncontextualDirect.runSession session subtable.Replacements.Value lookup numGlyphs ranges subtable.SubFeatureFlags context

    /// <summary>Applies one subtable to the working set of a line, <c>ChainSubtable::dispatch</c>.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the written glyphs feed the growing glyph set.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let private applySubtableSession (session: Sealed.AatSession) (subtable: ChainSubtable) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool) =
        match subtable.Kind with
        | Noncontextual -> applyNoncontextualSession session subtable numGlyphs ranges context
        | Rearrangement ->
            applyRearrangementSession session subtable numGlyphs ranges
            false
        | Contextual -> applyContextualSession session subtable numGlyphs ranges context
        | Ligature -> applyLigatureSession session subtable numGlyphs ranges context usingSet
        | Insertion -> applyInsertionSession session subtable numGlyphs ranges context usingSet
        | UnknownKind _ -> false

    /// <summary>Whether the working set holds a glyph in a subtable's initial set: the bit of the chain's candidates when the growing
    /// glyph set is in use (the working set's, or the ones computed from the line's glyphs while none is open), else a scan of the
    /// glyphs the line has now.</summary>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="session">The working set, none while no subtable has applied.</param>
    /// <param name="pending">The chain's candidate bits while no working set is open.</param>
    /// <param name="run">The run the line started as.</param>
    /// <param name="index">The subtable's index in its chain.</param>
    /// <param name="initial">The initial glyph set.</param>
    /// <returns>True when the buffer intersects.</returns>
    let private intersectsSession (usingSet: bool) (session: Sealed.AatSession voption) (pending: uint64 array) (run: GlyphRun) (index: int) (initial: ImmutableArray<CodepointRange>) =
        match usingSet, session with
        | true, ValueSome opened -> Sealed.aatSessionCandidate opened index
        | true, ValueNone -> (pending[index / 64] >>> (index % 64)) &&& 1UL <> 0UL
        | false, ValueSome opened -> scanIntersects (Sealed.aatSessionInfos opened) (initial.AsSpan()) 0
        | false, ValueNone -> scanIntersects (run.Infos.AsSpan()) (initial.AsSpan()) 0

    /// <summary>Adds the glyphs a pass wrote to the candidate bits of the chain when the growing glyph set is in use.</summary>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="session">The working set.</param>
    let private absorbWritten (usingSet: bool) (candidates: ChainCandidates voption) (session: Sealed.AatSession) =
        match usingSet, candidates with
        | true, ValueSome table -> Sealed.aatSessionAbsorb session table.Bits table.Top table.Words
        | true, ValueNone -> ()
        | false, ValueSome _ -> ()
        | false, ValueNone -> ()

    /// <summary>Applies one subtable that passed its checks to the working set: charges the pass, turns the glyphs round when the
    /// subtable's direction differs, applies it and takes the glyphs it wrote into the candidates of the chain.</summary>
    /// <param name="segment">The segment properties.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="reversed">Whether the glyphs are currently turned round.</param>
    /// <param name="session">The working set.</param>
    /// <returns>Whether the glyphs are now turned round, and whether the chain goes on.</returns>
    let private applyCheckedSession (segment: SegmentProperties) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool)
        (candidates: ChainCandidates voption) (subtable: ChainSubtable) (reversed: bool) (session: Sealed.AatSession) =
        match Sealed.aatSessionCharge session with
        | false -> struct (reversed, false)
        | true ->
            let reverse = reverseOf subtable.Coverage segment.Direction
            match reverse <> reversed with
            | true -> Sealed.aatSessionReverse session
            | false -> ()
            let deleted = applySubtableSession session subtable numGlyphs ranges context usingSet
            match deleted with
            | true -> Sealed.aatSessionAddScratch session AatApply.aatHasDeleted
            | false -> ()
            absorbWritten usingSet candidates session
            struct (reverse, Sealed.aatSessionSuccessful session)

    /// <summary>Opens the working set of a line.</summary>
    /// <param name="adopt">Whether the line's arrays are the caller's own.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="chains">The chains of the table.</param>
    /// <param name="run">The run.</param>
    /// <returns>The working set, with the unsafe-to-concat flag set when the run asks for it.</returns>
    let private openSession (adopt: bool) (usingSet: bool) (chains: int) (run: GlyphRun) =
        let session = Sealed.aatSessionOpen (AatKernel.inputOf run) adopt usingSet (chains > 1)
        Sealed.aatSessionMarkConcat session
        session

    /// <summary>Opens the working set of a line at the first subtable that applies, giving it the candidate bits the chain computed
    /// from the line's glyphs.</summary>
    /// <param name="adopt">Whether the line's arrays are the caller's own.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="chains">The chains of the table.</param>
    /// <param name="pending">The chain's candidate bits.</param>
    /// <param name="run">The run.</param>
    /// <returns>The working set.</returns>
    let private openSessionWith (adopt: bool) (usingSet: bool) (chains: int) (pending: uint64 array) (run: GlyphRun) =
        let session = openSession adopt usingSet chains run
        match usingSet with
        | true -> Sealed.aatSessionAdopt session pending
        | false -> ()
        session

    /// <summary>Runs the range, direction and intersection checks of one subtable and applies it to the working set, which is opened
    /// by the first subtable that applies when no growing glyph set needs it earlier, <c>Chain::apply</c>'s per-subtable body.</summary>
    /// <param name="adopt">Whether the line's arrays are the caller's own.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="chains">The chains of the table.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="subtables">The chain's subtables.</param>
    /// <param name="index">The current subtable.</param>
    /// <param name="reversed">Whether the glyphs are currently turned round.</param>
    /// <param name="session">The working set, none while no subtable has applied.</param>
    /// <param name="pending">The chain's candidate bits while no working set is open.</param>
    /// <param name="run">The run the line started as.</param>
    /// <returns>Whether the glyphs are turned round, the working set, and whether the chain goes on.</returns>
    let private applyChainStepSession (adopt: bool) (segment: SegmentProperties) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool) (chains: int)
        (candidates: ChainCandidates voption) (subtables: ImmutableArray<ChainSubtable>) (index: int) (reversed: bool) (session: Sealed.AatSession voption) (pending: uint64 array) (run: GlyphRun) =
        let subtable = subtables[index]
        let coverage = subtable.Coverage
        let rangeEnabled = anyRangeEnabled (ranges.AsSpan()) subtable.SubFeatureFlags 0
        let directionOk =
            coverage &&& CoverageAllDirections <> 0uy
            || (not (Direction.isHorizontal segment.Direction)) = (coverage &&& CoverageVertical <> 0uy)
        match rangeEnabled && directionOk && intersectsSession usingSet session pending run index subtable.InitialGlyphs with
        | false -> struct (reversed, session, true)
        | true ->
            let opened = match session with | ValueSome existing -> existing | ValueNone -> openSessionWith adopt usingSet chains pending run
            let struct (turned, goesOn) = applyCheckedSession segment numGlyphs ranges context usingSet candidates subtable reversed opened
            struct (turned, ValueSome opened, goesOn)

    /// <summary>Applies the subtables of one chain to the working set, <c>Chain::apply</c>.</summary>
    /// <param name="adopt">Whether the line's arrays are the caller's own.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ranges">The compiled ranges.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="chains">The chains of the table.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="subtables">The chain's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="reversed">Whether the glyphs are currently turned round.</param>
    /// <param name="session">The working set, none while no subtable has applied.</param>
    /// <param name="pending">The chain's candidate bits while no working set is open.</param>
    /// <param name="run">The run the line started as.</param>
    /// <returns>Whether the glyphs are turned round, and the working set.</returns>
    [<TailCall>]
    let rec private applyChainSubtablesSession (adopt: bool) (segment: SegmentProperties) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (context: AatContext) (usingSet: bool) (chains: int)
        (candidates: ChainCandidates voption) (subtables: ImmutableArray<ChainSubtable>) (index: int) (reversed: bool) (session: Sealed.AatSession voption) (pending: uint64 array) (run: GlyphRun) =
        match index >= subtables.Length with
        | true -> struct (reversed, session)
        | false ->
            let struct (turned, next, goesOn) = applyChainStepSession adopt segment numGlyphs ranges context usingSet chains candidates subtables index reversed session pending run
            match goesOn with
            | true -> applyChainSubtablesSession adopt segment numGlyphs ranges context usingSet chains candidates subtables (index + 1) turned next pending run
            | false -> struct (turned, next)

    /// <summary>Starts a chain: its candidate bits from the glyphs the line has, in the working set when one is open, else computed
    /// from the line's own glyphs, which no pass has changed.</summary>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="candidates">The chain's bit table, or none.</param>
    /// <param name="chainIndex">The chain.</param>
    /// <param name="session">The working set, none while no subtable has applied.</param>
    /// <param name="run">The run the line started as.</param>
    /// <returns>The candidate bits to use while no working set is open, empty when there is nothing to compute.</returns>
    let private startChainSession (usingSet: bool) (candidates: ChainCandidates voption) (chainIndex: int) (session: Sealed.AatSession voption) (run: GlyphRun) =
        match usingSet, candidates, session with
        | true, ValueSome table, ValueSome opened ->
            Sealed.aatSessionStartChain opened table.Bits table.Top table.Words (chainIndex > 0)
            Array.Empty<uint64>()
        | true, ValueSome table, ValueNone -> Sealed.aatWordsOf table.Bits table.Top table.Words (run.Infos.AsSpan())
        | true, ValueNone, _ -> Array.Empty<uint64>()
        | false, _, _ -> Array.Empty<uint64>()

    /// <summary>Turns the glyphs back into logical order when a chain left them turned round.</summary>
    /// <param name="reversed">Whether the glyphs are turned round.</param>
    /// <param name="session">The working set, none while no subtable has applied.</param>
    let private unReverseSession (reversed: bool) (session: Sealed.AatSession voption) =
        match reversed, session with
        | true, ValueSome opened -> Sealed.aatSessionReverse opened
        | true, ValueNone -> ()
        | false, _ -> ()

    /// <summary>Applies every chain of a table to the working set, <c>mortmorx::apply</c>.</summary>
    /// <param name="adopt">Whether the line's arrays are the caller's own.</param>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="map">The compiled AAT map.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="context">The apply context.</param>
    /// <param name="usingSet">Whether the growing glyph set is in use.</param>
    /// <param name="chainIndex">The next chain.</param>
    /// <param name="session">The working set, none while no subtable has applied.</param>
    /// <param name="run">The run the line started as.</param>
    /// <returns>The working set.</returns>
    [<TailCall>]
    let rec private applyChainsSession (adopt: bool) (table: Morx) (map: AatMap) (segment: SegmentProperties) (numGlyphs: int) (context: AatContext) (usingSet: bool)
        (chainIndex: int) (session: Sealed.AatSession voption) (run: GlyphRun) =
        let succeeded = match session with | ValueSome opened -> Sealed.aatSessionSuccessful opened | ValueNone -> true
        match chainIndex >= table.Chains.Length || not succeeded with
        | true -> session
        | false ->
            let chain = table.Chains[chainIndex]
            let candidates = chain.Candidates.Value
            let pending = startChainSession usingSet candidates chainIndex session run
            let struct (reversed, next) =
                applyChainSubtablesSession adopt segment numGlyphs map.ChainFlags[chainIndex] context usingSet table.Chains.Length candidates chain.Subtables 0 false session pending run
            unReverseSession reversed next
            applyChainsSession adopt table map segment numGlyphs context usingSet (chainIndex + 1) next run

    /// <summary>Whether every chain of a table has the bit table the growing glyph set reads.</summary>
    /// <param name="table">The table.</param>
    /// <param name="index">The next chain.</param>
    /// <returns>True when every chain has one.</returns>
    [<TailCall>]
    let rec private chainsHaveCandidates (table: Morx) (index: int) =
        match index >= table.Chains.Length with
        | true -> true
        | false ->
            match table.Chains[index].Candidates.Value with
            | ValueSome _ -> chainsHaveCandidates table (index + 1)
            | ValueNone -> false

    /// <summary>Applies the morx or mort table, <c>mortmorx::apply</c>, with one working set of the line's own through every chain and
    /// subtable: a run with positions, or a table the bit tables of the chains do not cover, takes the subtable-by-subtable route.</summary>
    /// <param name="adopt">Whether the run's arrays were made by the caller in this call and are shared with no one, so that the
    /// subtables may write them in place.</param>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="map">The compiled AAT map.</param>
    /// <param name="face">The face.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run, and whether any subtable wrote a glyph, a property or a flag of it (true on the
    /// subtable-by-subtable route unless the run came back as it was).</returns>
    let applySessionOf (adopt: bool) (table: Morx) (map: AatMap) (face: OtFace) (segment: SegmentProperties) (run: GlyphRun) =
        match run.Successful with
        | false -> struct (run, false)
        | true ->
            let usingSet = not table.IsMort && run.Infos.Length >= 4
            match run.HavePositions || not run.Positions.IsEmpty || (usingSet && not (chainsHaveCandidates table 0)) with
            | true ->
                let applied = applyWith Kernel table map face segment run
                struct (applied, not (applied.Infos.Equals run.Infos && applied.Props.Equals run.Props))
            | false ->
                let numGlyphs = face.Face.GlyphCount
                let context: AatContext = { Gdef = face.Gdef.Value }
                let flagged = run.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default
                let started =
                    match flagged with
                    | true -> ValueSome (openSession adopt usingSet table.Chains.Length run)
                    | false -> ValueNone
                match applyChainsSession adopt table map segment numGlyphs context usingSet 0 started run with
                | ValueNone -> struct (run, false)
                | ValueSome finished ->
                    let edited = Sealed.aatSessionEdited finished
                    struct (AatKernel.resultOf run (Sealed.aatSessionClose finished run.Positions), edited)

    /// <summary>Applies the morx or mort table, <c>mortmorx::apply</c>, on arrays the caller made in this call and shares with no one.</summary>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="map">The compiled AAT map.</param>
    /// <param name="face">The face.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run, and whether any subtable wrote a glyph, a property or a flag of it.</returns>
    let applyOwned (table: Morx) (map: AatMap) (face: OtFace) (segment: SegmentProperties) (run: GlyphRun) =
        applySessionOf true table map face segment run

    /// <summary>Applies the morx or mort table, <c>mortmorx::apply</c>, on the kernel.</summary>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="map">The compiled AAT map.</param>
    /// <param name="face">The face.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let apply (table: Morx) (map: AatMap) (face: OtFace) (segment: SegmentProperties) (run: GlyphRun) =
        let struct (applied, _) = applySessionOf false table map face segment run
        applied
