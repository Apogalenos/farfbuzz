namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>Whether a transition step produced an edit or has nothing left to do.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Emission =
    | Emit of edit: RunEdit
    | Complete

/// <summary>
/// A subtable kind (rearrangement, contextual, ligature, insertion, kerx 1, kerx 4): its callbacks, implemented by a
/// struct so that the JIT specialises the driver for each kind.
/// </summary>
type internal IStateMachineKind<'Context when 'Context : struct> =
    /// <summary><c>is_actionable</c> and <c>is_action_initiable</c>, shared with the initial-glyph builder.</summary>
    inherit IEntryClassifier

    /// <summary><c>driver_context_t::in_place</c>: true when the kind never uses the out-buffer.</summary>
    abstract InPlace : bool

    /// <summary>
    /// The next edit of the transition for <paramref name="entry"/>, or Complete. The transducer applies one edit per
    /// step, so a transition is a micro-program whose program counter lives in the context.
    /// </summary>
    abstract Transition : cursor: inref<RunCursor> * entry: Entry * context: 'Context -> struct (Emission * 'Context)

    /// <summary>
    /// The context update of a null transition (non-actionable, non-epsilon), applied to the last glyph of a batched
    /// stretch. Upstream's null-transition fast path still calls the transition, so SetMark, MarkFirst and MarkLast
    /// still take effect there.
    /// </summary>
    abstract NullTransition : cursor: inref<RunCursor> * entry: Entry * context: 'Context -> 'Context

/// <summary>Where the driver is within StateTableDriver::drive (src/hb-aat-layout-common.hh:1282-1443).</summary>
[<Struct; NoComparison; NoEquality>]
type internal DriverPhase =
    | Start  // first step: a kind that is not in place clears the output (src/hb-aat-layout-common.hh:1286-1287)
    | Fetch  // top of the loop: classify the glyph at the cursor (or end of text) and choose the entry
    | Transitioning of entry: Entry  // running the kind's transition for this entry; its next state is entry.NewState
    | ForcedAdvance  // the epsilon transition found max_ops exhausted: advance anyway
    | Closing  // the loop has ended: Finish, which synchronises the out-buffer when there is one (src/hb-aat-layout-common.hh:1440-1441)

/// <summary>
/// The pass state: phase, machine state, current range flags index, whether reaching the end of the text now ends the
/// pass without the end-of-text transition (the fast path's behaviour), and the kind context.
/// </summary>
[<Struct; NoComparison; NoEquality>]
type internal DriverState<'Context when 'Context : struct> =
    { Phase: DriverPhase
      State: int
      Range: int
      SkipEndOfText: bool
      Context: 'Context }

/// <summary>What the driver needs besides the kind.</summary>
[<Struct; NoComparison; NoEquality>]
type internal DriverConfig =
    { Machine: StateMachine
      Ranges: ImmutableArray<RangeFlags>
      SubtableFlags: uint32
      NumGlyphs: int
      StartSafeToBreakAtEndOfText: bool }

/// <summary>StateTableDriver::drive, one transducer step at a time.</summary>
[<RequireQualifiedAccess>]
module internal Driver =
    /// <summary>The <c>Flags::DontAdvance</c> entry bit, an epsilon transition (src/hb-aat-layout-morx-table.hh:59).</summary>
    [<Literal>]
    let private DontAdvanceFlag = 0x4000us

    /// <summary>Finds the range covering a cluster, moving down then up, <c>drive</c>'s range walk
    /// (src/hb-aat-layout-common.hh:1312-1320).</summary>
    /// <param name="ranges">The compiled ranges; the first starts at cluster 0, the last ends at 0xFFFFFFFF.</param>
    /// <param name="index">The current range index.</param>
    /// <param name="cluster">The glyph cluster.</param>
    /// <returns>The range index.</returns>
    [<TailCall>]
    let rec internal rangeFor (ranges: ReadOnlySpan<RangeFlags>) index cluster =
        match index > 0 && cluster < ranges[index].ClusterFirst with
        | true -> rangeFor ranges (index - 1) cluster
        | false ->
            match index < ranges.Length - 1 && cluster > ranges[index].ClusterLast with
            | true -> rangeFor ranges (index + 1) cluster
            | false -> index

    /// <summary>Counts consecutive glyphs whose narrow or wide class-table class stays <paramref name="klass"/>, the
    /// fast path's stretch (src/hb-aat-layout-common.hh:1347-1359). The span, offset, first glyph, count and stride
    /// are resolved once, before the loop, so each glyph is one class read and one comparison.</summary>
    /// <param name="span">The class-table bytes.</param>
    /// <param name="at">The byte offset of the class table.</param>
    /// <param name="firstGlyph">The first glyph the array covers.</param>
    /// <param name="count">The number of classes in the array.</param>
    /// <param name="wide">True for 16-bit classes, false for 8-bit.</param>
    /// <param name="input">The input glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="klass">The class the stretch stays on.</param>
    /// <returns>The first index whose class differs, or the length.</returns>
    [<TailCall>]
    let rec private nullStretchClass (span: ReadOnlySpan<byte>) (at: int) (firstGlyph: uint16) (count: int) (wide: bool) (input: ReadOnlySpan<GlyphInfo>) index (klass: uint16) =
        match index < input.Length with
        | false -> index
        | true ->
            let cls =
                match input[index].Codepoint with
                | StateTable.DeletedGlyph -> StateTable.ClassDeletedGlyph
                | glyph -> ClassTable.classOfSpan span at firstGlyph count wide glyph StateTable.ClassOutOfBounds
            match cls = klass with
            | true -> nullStretchClass span at firstGlyph count wide input (index + 1) klass
            | false -> index

    /// <summary>Counts consecutive glyphs whose extended-lookup class stays <paramref name="klass"/>.</summary>
    /// <param name="lookup">The extended class lookup.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="input">The input glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="klass">The class the stretch stays on.</param>
    /// <returns>The first index whose class differs, or the length.</returns>
    [<TailCall>]
    let rec private nullStretchExtended (lookup: Lookup) (numGlyphs: int) (input: ReadOnlySpan<GlyphInfo>) index (klass: uint16) =
        match index < input.Length with
        | false -> index
        | true ->
            let cls =
                match input[index].Codepoint with
                | StateTable.DeletedGlyph -> StateTable.ClassDeletedGlyph
                | glyph -> AatLookup.classOf lookup glyph numGlyphs StateTable.ClassOutOfBounds
            match cls = klass with
            | true -> nullStretchExtended lookup numGlyphs input (index + 1) klass
            | false -> index

    /// <summary>Counts the consecutive glyphs from the cursor whose class stays <paramref name="klass"/>, the fast
    /// path's stretch (src/hb-aat-layout-common.hh:1347-1359). The class reader is resolved once, before the loop.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="input">The input glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="klass">The class the stretch stays on.</param>
    /// <returns>The first index whose class differs, or the length.</returns>
    let internal nullStretch machine numGlyphs (input: ReadOnlySpan<GlyphInfo>) index (klass: uint16) =
        match machine.Classes with
        | Narrow table -> nullStretchClass table.Table.Span table.At table.FirstGlyph table.Count false input index klass
        | Wide table -> nullStretchClass table.Table.Span table.At table.FirstGlyph table.Count true input index klass
        | Extended lookup -> nullStretchExtended lookup numGlyphs input index klass

    /// <summary>Whether breaking before the current glyph is safe, the condition of src/hb-aat-layout-common.hh:1397-1423.</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="state">The current state number.</param>
    /// <param name="klass">The class.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="nextState">The next state.</param>
    /// <returns>True when safe to break.</returns>
    let internal safeToBreakAt<'Kind when 'Kind :> IEntryClassifier> (config: inref<DriverConfig>) (kind: 'Kind) state (klass: uint16) (entry: Entry) nextState =
        StateSafety.safeToBreak config.Machine kind state klass entry nextState

    /// <summary>Advances after a finished transition, or charges an operation for an epsilon transition
    /// (src/hb-aat-layout-common.hh:1433-1437).</summary>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <param name="entry">The finished entry.</param>
    /// <returns>The edit and the next state.</returns>
    let private advancingStep<'Context when 'Context : struct> (cursor: inref<RunCursor>) (state: DriverState<'Context>) (entry: Entry) =
        match entry.Flags &&& DontAdvanceFlag <> 0us with
        | false -> struct (RunEdit.NextGlyph, { state with Phase = Fetch })
        | true ->
            match cursor.MaxOps <= 0 with
            | true -> struct (RunEdit.ChargeOps 1, { state with Phase = ForcedAdvance })
            | false -> struct (RunEdit.ChargeOps 1, { state with Phase = Fetch })

    /// <summary>Runs the kind's transition until it completes, then advances (src/hb-aat-layout-common.hh:1428-1437).</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="nextState">The next state.</param>
    /// <returns>The edit and the next state.</returns>
    let private transitionStep<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) (entry: Entry) nextState =
        let struct (emission, nextContext) = kind.Transition (&cursor, entry, state.Context)
        match emission with
        | Emission.Emit edit -> struct (edit, { state with Context = nextContext })
        | Emission.Complete ->
            let next = { state with Context = nextContext; State = nextState }
            match cursor.Index >= cursor.Input.Length with
            | true -> struct (RunEdit.Finish, { next with Phase = Closing })
            | false -> advancingStep &cursor next entry

    /// <summary>Emits the unsafe-to-break edit when needed, then starts the transition
    /// (src/hb-aat-layout-common.hh:1425-1428).</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <param name="klass">The class.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="nextState">The next state.</param>
    /// <returns>The edit and the next state.</returns>
    let private safeToBreak<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (config: inref<DriverConfig>) (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) (klass: uint16) (entry: Entry) nextState =
        let isSafe = safeToBreakAt &config kind state.State klass entry nextState
        let backtrack = match cursor.HaveOutput with | true -> cursor.Output.Length | false -> cursor.Index
        let next = { state with Phase = Transitioning entry }
        match (not isSafe) && backtrack > 0 && cursor.Index < cursor.Input.Length with
        | true -> struct (RunEdit.UnsafeToBreakFromOutbuffer (backtrack - 1, cursor.Index + 1), next)
        | false -> transitionStep kind &cursor next entry nextState

    /// <summary>Applies the null transition for every glyph of a stretch, threading the context so per-glyph side
    /// effects (SetComponent) take effect for the whole stretch (src/hb-aat-layout-common.hh:1347-1359). The cursor is
    /// rebuilt per glyph so each sees its own index.</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="kind">The kind.</param>
    /// <param name="input">The input glyphs.</param>
    /// <param name="inputProps">The input properties.</param>
    /// <param name="output">The output glyphs.</param>
    /// <param name="outputProps">The output properties.</param>
    /// <param name="positions">The positions.</param>
    /// <param name="haveOutput">Whether the out-buffer is open.</param>
    /// <param name="clusterLevel">The cluster level.</param>
    /// <param name="flags">The buffer flags.</param>
    /// <param name="serial">The serial.</param>
    /// <param name="randomState">The random state.</param>
    /// <param name="maxOps">The budget.</param>
    /// <param name="successful">Whether the buffer has not failed.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="finish">One past the last glyph.</param>
    /// <param name="entry">The null-transition entry.</param>
    /// <param name="context">The kind context.</param>
    /// <returns>The context after the stretch.</returns>
    [<TailCall>]
    let rec private nullTransitionStretch<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (kind: 'Kind) (input: ReadOnlySpan<GlyphInfo>) (inputProps: ReadOnlySpan<GlyphProps>)
        (output: ReadOnlySpan<GlyphInfo>) (outputProps: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>)
        (haveOutput: bool) (clusterLevel: ClusterLevel) (flags: BufferFlags) (serial: byte) (randomState: uint32)
        (maxOps: int) (successful: bool) (index: int) (finish: int) (entry: Entry) (context: 'Context) =
        match index >= finish with
        | true -> context
        | false ->
            let atCursor =
                RunCursor(input, inputProps, index, output, outputProps, positions, haveOutput, clusterLevel, flags, serial, randomState, maxOps, successful)
            let next = kind.NullTransition (&atCursor, entry, context)
            nullTransitionStretch kind input inputProps output outputProps positions haveOutput clusterLevel flags serial randomState maxOps successful (index + 1) finish entry next

    /// <summary>Consumes a whole stretch of glyphs that stay on a non-actionable, non-epsilon transition, one
    /// <c>NextGlyphs n</c> edit (src/hb-aat-layout-common.hh:1334-1366). The null-transition form skips the end-of-text
    /// transition the way upstream's loop does; the stable-state form leaves it to the next fetch. The transition's
    /// context side effects are applied per glyph.</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <param name="klass">The class the stretch stays on.</param>
    /// <param name="entry">The non-actionable entry.</param>
    /// <param name="skipEndOfText">True to skip the end-of-text transition after the stretch.</param>
    /// <returns>The edit and the next state.</returns>
    let private batch<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (config: inref<DriverConfig>) (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) (klass: uint16) entry (skipEndOfText: bool) =
        match cursor.Index >= cursor.Input.Length with
        | true -> struct (RunEdit.Finish, { state with Phase = Closing })
        | false ->
            let finish = nullStretch config.Machine config.NumGlyphs cursor.Input cursor.Index klass
            let context =
                nullTransitionStretch kind cursor.Input cursor.InputProps cursor.Output cursor.OutputProps cursor.Positions
                    cursor.HaveOutput cursor.ClusterLevel cursor.Flags cursor.Serial cursor.RandomState cursor.MaxOps cursor.Successful
                    cursor.Index finish entry state.Context
            struct (RunEdit.NextGlyphs (finish - cursor.Index), { state with Phase = Fetch; SkipEndOfText = skipEndOfText; Context = context })

    /// <summary>Handles the range flags, resetting the state when the range does not carry the subtable flags
    /// (src/hb-aat-layout-common.hh:1307-1331).</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <param name="klass">The class.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="nextState">The next state.</param>
    /// <returns>The edit and the next state.</returns>
    let private rangeStep<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (config: inref<DriverConfig>) (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) (klass: uint16) (entry: Entry) nextState =
        let ranges = config.Ranges.AsSpan()
        let range =
            match cursor.Index < cursor.Input.Length with
            | true -> rangeFor ranges state.Range cursor.Input[cursor.Index].Cluster
            | false -> state.Range
        let next = { state with Range = range }
        match ranges[range].Flags &&& config.SubtableFlags = 0u with
        | true ->
            match cursor.Index >= cursor.Input.Length with
            | true -> struct (RunEdit.Finish, { next with Phase = Closing })
            | false -> struct (RunEdit.NextGlyph, { next with Phase = Fetch; State = StateTable.StartOfText })
        | false -> safeToBreak &config kind &cursor next klass entry nextState

    /// <summary>Computes the entry and next state for a class, then dispatches between the range walk, the null-transition
    /// fast path, the stable non-actionable stretch and the safe-to-break path (src/hb-aat-layout-common.hh:1294-1366).</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <param name="klass">The class.</param>
    /// <returns>The edit and the next state.</returns>
    let private fetchOne<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (config: inref<DriverConfig>) (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) (klass: uint16) =
        let entry = StateTable.entry config.Machine state.State klass
        let nextState = entry.NewState
        match config.Ranges.Length > 1 with
        | true -> rangeStep &config kind &cursor state klass entry nextState
        | false ->
            let isSafe = safeToBreakAt &config kind state.State klass entry nextState
            let nonEpsilon = entry.Flags &&& DontAdvanceFlag = 0us
            match state.State = StateTable.StartOfText && nextState = StateTable.StartOfText && isSafe && nonEpsilon with
            | true -> batch &config kind &cursor state klass entry true
            | false ->
                match isSafe && nonEpsilon && nextState = state.State && cursor.Index < cursor.Input.Length with
                | true -> batch &config kind &cursor state klass entry false
                | false -> safeToBreak &config kind &cursor { state with SkipEndOfText = false } klass entry nextState

    /// <summary>Classifies the glyph at the cursor and starts one transition (src/hb-aat-layout-common.hh:1294-1302).</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <returns>The edit and the next state.</returns>
    let private fetchStep<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (config: inref<DriverConfig>) (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) =
        match cursor.Successful with
        | false -> struct (RunEdit.Finish, { state with Phase = Closing })
        | true ->
            match cursor.Index >= cursor.Input.Length with
            | true ->
                match state.SkipEndOfText with
                | true -> struct (RunEdit.Finish, { state with Phase = Closing })
                | false -> fetchOne &config kind &cursor state StateTable.ClassEndOfText
            | false ->
                let klass = StateTable.classOf config.Machine cursor.Input[cursor.Index].Codepoint config.NumGlyphs
                fetchOne &config kind &cursor state klass

    /// <summary>Opens the output buffer when the kind is not in place, then fetches (src/hb-aat-layout-common.hh:1286-1289).</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <typeparam name="'Context">The kind's transition context.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <returns>The edit and the next state.</returns>
    let private startStep<'Kind, 'Context when 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (config: inref<DriverConfig>) (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) =
        match kind.InPlace with
        | true -> fetchStep &config kind &cursor { state with Phase = Fetch }
        | false -> struct (RunEdit.ClearOutput, { state with Phase = Fetch })

    /// <summary>
    /// Returns the next edit and state. Non-recursive: Fetch may fall through to the first emission of the
    /// transition, which may fall through to the advance, by plain calls.
    /// </summary>
    /// <typeparam name="'Kind">The subtable kind, a struct.</typeparam>
    /// <typeparam name="'Context">The kind's transition context, a struct.</typeparam>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="state">The driver state.</param>
    /// <returns>The edit and the next state.</returns>
    let step<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (config: inref<DriverConfig>) (kind: 'Kind) (cursor: inref<RunCursor>) (state: DriverState<'Context>) =
        match state.Phase with
        | Start -> startStep &config kind &cursor state
        | Fetch -> fetchStep &config kind &cursor state
        | Transitioning entry -> transitionStep kind &cursor state entry entry.NewState
        | ForcedAdvance -> struct (RunEdit.NextGlyph, { state with Phase = Fetch })
        | Closing -> struct (RunEdit.Finish, state)

/// <summary>The driver packaged for Sealed.runPassWithState.</summary>
[<Struct; NoComparison; NoEquality>]
type internal StateMachinePass<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct> =
    { Config: DriverConfig
      Kind: 'Kind }

    interface IRunPass<DriverState<'Context>> with
        member this.Step (cursor, state) = Driver.step &this.Config this.Kind &cursor state
