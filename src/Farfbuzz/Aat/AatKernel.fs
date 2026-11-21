namespace Farfbuzz.Aat

open System
open System.Runtime.CompilerServices
open Farfbuzz

/// <summary>Which implementation runs a state-table subtable: the direct kernel, or the edit interpreter that stays as
/// the reference.</summary>
[<Struct; NoComparison; NoEquality>]
type internal MachineRunner =
    | Kernel
    | Reference

/// <summary>
/// One state-table subtable as the decisions of <see cref="Sealed.runAatPass"/>: the table reads and the predicates of
/// the subtable kind, behind <see cref="IAatMachine{E,C}"/> so that the JIT specialises the kernel for each kind.
/// </summary>
[<Struct; NoComparison; NoEquality; IsReadOnly>]
type internal KernelMachine<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct> =
    { Config: DriverConfig
      Kind: 'Kind
      QuietNull: bool // the null transition stands in for the transition of an entry that is not actionable
      NullMask: uint16 } // the entry flags that make the null transition change the context

    interface IAatTables<Entry> with
        member this.InPlace = this.Kind.InPlace
        member this.RangeCount = this.Config.Ranges.Length
        member this.AdvanceRange (index, cluster) = Driver.rangeFor (this.Config.Ranges.AsSpan()) index cluster
        member this.RangeEnabled index = this.Config.Ranges[index].Flags &&& this.Config.SubtableFlags <> 0u
        member this.ClassOf glyph = int (StateTable.classOf this.Config.Machine glyph this.Config.NumGlyphs)
        member this.EntryAt (state, klass) = StateTable.entry this.Config.Machine state (uint16 klass)
        member _.NewState entry = entry.NewState
        member _.DontAdvance entry = entry.Flags &&& 0x4000us <> 0us
        member this.SafeToBreak (state, klass, entry, nextState) =
            Driver.safeToBreakAt &this.Config this.Kind state (uint16 klass) entry nextState
        member this.StretchEnd (infos, index, klass) =
            Driver.nullStretch this.Config.Machine this.Config.NumGlyphs infos index (uint16 klass)
        member _.QuietEnd (_, index) = index

    interface IAatMachine<Entry, 'Context> with
        member this.Quiet entry = this.QuietNull && not (this.Kind.IsActionable entry)
        member this.NullIsIdentity entry = this.NullMask <> 0us && entry.Flags &&& this.NullMask = 0us
        member this.NullTransition (cursor, entry, context) = this.Kind.NullTransition (&cursor, entry, context)
        member this.Transition (cursor, entry, context) =
            let struct (emission, next) = this.Kind.Transition (&cursor, entry, context)
            match emission with
            | Emission.Emit edit -> struct (true, edit, next)
            | Emission.Complete -> struct (false, Unchecked.defaultof<RunEdit>, next)

/// <summary>Reading a dense class array.</summary>
[<RequireQualifiedAccess>]
module internal DenseReads =
    /// <summary>The class of a glyph beyond the dense array, the table's own reader; kept out of line so that the dense read inlines.</summary>
    /// <param name="machine">The state machine.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The class.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let beyond (machine: StateMachine) (glyph: uint32) (numGlyphs: int) = int (StateTable.classOf machine glyph numGlyphs)

    /// <summary>Counts the glyphs from an index whose class stays the given class, reading a dense class array.</summary>
    /// <param name="classes">The dense class array.</param>
    /// <param name="machine">The state machine, for glyphs beyond the array.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="input">The input glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="klass">The class the stretch stays on.</param>
    /// <returns>The first index whose class differs, or the length.</returns>
    [<TailCall>]
    let rec stretchEnd (classes: ReadOnlySpan<byte>) (machine: StateMachine) (numGlyphs: int) (input: ReadOnlySpan<GlyphInfo>) (index: int) (klass: int) =
        match index < input.Length with
        | false -> index
        | true ->
            let glyph = input[index].Codepoint
            let cls =
                match glyph < uint32 classes.Length with
                | true -> int classes[int glyph]
                | false -> beyond machine glyph numGlyphs
            match cls = klass with
            | true -> stretchEnd classes machine numGlyphs input (index + 1) klass
            | false -> index

    /// <summary>Counts the glyphs from an index whose class leaves the start state alone, whatever the class: one read of the
    /// per-glyph answer for a glyph the table covers, the class reads for the others.</summary>
    /// <param name="quiet">Per glyph, one when its class leaves the start state alone.</param>
    /// <param name="inert">Per class, one when the start state stays on it with nothing to do.</param>
    /// <param name="machine">The state machine, for glyphs beyond the array.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="input">The input glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>The first index whose class does something, or the length.</returns>
    [<TailCall>]
    let rec quietEnd (quiet: ReadOnlySpan<byte>) (inert: ReadOnlySpan<byte>) (machine: StateMachine) (numGlyphs: int) (input: ReadOnlySpan<GlyphInfo>) (index: int) =
        match index < input.Length with
        | false -> index
        | true ->
            let glyph = input[index].Codepoint
            let answer =
                match glyph < uint32 quiet.Length with
                | true -> quiet[int glyph]
                | false ->
                    let cls = beyond machine glyph numGlyphs
                    inert[match cls < inert.Length with | true -> cls | false -> int StateTable.ClassOutOfBounds]
            match answer with
            | 0uy -> index
            | _ -> quietEnd quiet inert machine numGlyphs input (index + 1)

/// <summary>
/// <see cref="KernelMachine{K,C}"/> over a <see cref="DenseMachine"/>: the class of a glyph, the entry of a state and a
/// class and the safe-to-break answer are each one array read.
/// </summary>
[<Struct; NoComparison; NoEquality; IsReadOnly>]
type internal DenseKernelMachine<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct> =
    { Config: DriverConfig
      Kind: 'Kind
      Dense: DenseMachine
      QuietNull: bool
      NullMask: uint16 }

    interface IAatTables<Entry> with
        member this.InPlace = this.Kind.InPlace
        member this.RangeCount = this.Config.Ranges.Length
        member this.AdvanceRange (index, cluster) = Driver.rangeFor (this.Config.Ranges.AsSpan()) index cluster
        member this.RangeEnabled index = this.Config.Ranges[index].Flags &&& this.Config.SubtableFlags <> 0u
        member this.ClassOf glyph =
            match glyph < uint32 this.Dense.GlyphClasses.Length with
            | true -> int this.Dense.GlyphClasses[int glyph]
            | false -> DenseReads.beyond this.Config.Machine glyph this.Config.NumGlyphs
        member this.EntryAt (state, klass) =
            let klassIndex = match klass >= this.Dense.Stride with | true -> int StateTable.ClassOutOfBounds | false -> klass
            this.Dense.Cells[(state + this.Dense.Bias) * this.Dense.Stride + klassIndex]
        member _.NewState entry = entry.NewState
        member _.DontAdvance entry = entry.Flags &&& 0x4000us <> 0us
        member this.SafeToBreak (state, klass, _, _) =
            let klassIndex = match klass >= this.Dense.Stride with | true -> int StateTable.ClassOutOfBounds | false -> klass
            this.Dense.SafeCells[(state + this.Dense.Bias) * this.Dense.Stride + klassIndex]
        member this.StretchEnd (infos, index, klass) =
            DenseReads.stretchEnd (this.Dense.GlyphClasses.AsSpan()) this.Config.Machine this.Config.NumGlyphs infos index klass
        member this.QuietEnd (infos, index) =
            DenseReads.quietEnd (this.Dense.QuietGlyphs.AsSpan()) (this.Dense.StartInert.AsSpan()) this.Config.Machine this.Config.NumGlyphs infos index

    interface IAatMachine<Entry, 'Context> with
        member this.Quiet entry = this.QuietNull && not (this.Kind.IsActionable entry)
        member this.NullIsIdentity entry = this.NullMask <> 0us && entry.Flags &&& this.NullMask = 0us
        member this.NullTransition (cursor, entry, context) = this.Kind.NullTransition (&cursor, entry, context)
        member this.Transition (cursor, entry, context) =
            let struct (emission, next) = this.Kind.Transition (&cursor, entry, context)
            match emission with
            | Emission.Emit edit -> struct (true, edit, next)
            | Emission.Complete -> struct (false, Unchecked.defaultof<RunEdit>, next)

/// <summary>Running one state-table subtable over a run, by the kernel or by the reference interpreter.</summary>
[<RequireQualifiedAccess>]
module internal AatKernel =
    /// <summary>The run fields a subtable pass reads.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The input of the pass.</returns>
    let inputOf (run: GlyphRun) =
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

    /// <summary>The run after a pass: the same instance when the pass changed nothing.</summary>
    /// <param name="run">The run before the pass.</param>
    /// <param name="result">The fields the pass produced.</param>
    /// <returns>The run after the pass.</returns>
    let resultOf (run: GlyphRun) (result: RunResult) =
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

    /// <summary>Runs one subtable's state machine over a run and returns the run and the finished context.</summary>
    /// <typeparam name="'Kind">The subtable kind, a struct.</typeparam>
    /// <typeparam name="'Context">The kind's transition context, a struct.</typeparam>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="nullMask">The entry flags that make the null transition change the context, zero when unknown.</param>
    /// <param name="quiet">Whether the null transition stands in for the transition of an entry that is not actionable.</param>
    /// <param name="seed">The initial context.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable and the finished context.</returns>
    let run<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (runner: MachineRunner) (dense: DenseMachine voption) (config: DriverConfig) (kind: 'Kind) (nullMask: uint16) (quiet: bool) (seed: 'Context) (run: GlyphRun) =
        match runner, dense with
        | Kernel, ValueSome table ->
            let machine = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = quiet; NullMask = nullMask }
            let struct (result, context) = Sealed.runAatPass (inputOf run) machine seed
            struct (resultOf run result, context)
        | Kernel, ValueNone ->
            let machine = { KernelMachine.Config = config; Kind = kind; QuietNull = quiet; NullMask = nullMask }
            let struct (result, context) = Sealed.runAatPass (inputOf run) machine seed
            struct (resultOf run result, context)
        | Reference, _ ->
            let state: DriverState<'Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = seed }
            let struct (result, final) = Sealed.runPassWithState (inputOf run) { StateMachinePass.Config = config; Kind = kind } state
            struct (resultOf run result, final.Context)

    /// <summary>Runs one subtable's state machine over the working set of a line and returns the finished context.</summary>
    /// <typeparam name="'Kind">The subtable kind, a struct.</typeparam>
    /// <typeparam name="'Context">The kind's transition context, a struct.</typeparam>
    /// <param name="session">The working set.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="nullMask">The entry flags that make the null transition change the context, zero when unknown.</param>
    /// <param name="quiet">Whether the null transition stands in for the transition of an entry that is not actionable.</param>
    /// <param name="seed">The initial context.</param>
    /// <returns>The finished context.</returns>
    let internal runSession<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
        (session: Sealed.AatSession) (dense: DenseMachine voption) (config: DriverConfig) (kind: 'Kind) (nullMask: uint16) (quiet: bool) (seed: 'Context) =
        match dense with
        | ValueSome table ->
            let machine = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = quiet; NullMask = nullMask }
            Sealed.aatSessionEdit session machine seed
        | ValueNone ->
            let machine = { KernelMachine.Config = config; Kind = kind; QuietNull = quiet; NullMask = nullMask }
            Sealed.aatSessionEdit session machine seed
