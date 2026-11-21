namespace Farfbuzz.Aat

open Farfbuzz

/// <summary>Where the rearrangement transition is within its three-edit micro-program.</summary>
[<Struct; NoComparison; NoEquality>]
type internal RearrangementPhase =
    | Idle
    | MergeRange
    | Rearrange
    | Done

/// <summary>The rearrangement transition context: the marked range, the decoded verb and the micro-program cursor.</summary>
[<Struct; NoComparison; NoEquality>]
type internal RearrangementContext =
    { Start: int
      End: int
      MoveLeft: int
      MoveRight: int
      FlipLeft: bool
      FlipRight: bool
      Phase: RearrangementPhase }

/// <summary>Reading and applying the rearrangement morx/mort subtable (src/hb-aat-layout-morx-table.hh:48-195).</summary>
[<RequireQualifiedAccess>]
module internal MorxRearrangement =
    /// <summary>The <c>MarkFirst</c> entry bit, which starts the marked range.</summary>
    [<Literal>]
    let private MarkFirstFlag = 0x8000us

    /// <summary>The <c>MarkLast</c> entry bit, which ends the marked range.</summary>
    [<Literal>]
    let private MarkLastFlag = 0x2000us

    /// <summary>The <c>Verb</c> entry nibble, the rearrangement operation.</summary>
    [<Literal>]
    let private VerbMask = 0x000Fus

    /// <summary>The longest range a rearrangement may span, <c>HB_MAX_CONTEXT_LENGTH</c>.</summary>
    [<Literal>]
    let private MaxContextLength = 64

    /// <summary>The two-nibble rearrangement verb map (src/hb-aat-layout-morx-table.hh:106-123).</summary>
    /// <param name="verb">The verb nibble, 0 to 15.</param>
    /// <returns>The mapped byte.</returns>
    let verbMap verb =
        match verb with
        | 0 -> 0x00
        | 1 -> 0x10
        | 2 -> 0x01
        | 3 -> 0x11
        | 4 -> 0x20
        | 5 -> 0x30
        | 6 -> 0x02
        | 7 -> 0x03
        | 8 -> 0x12
        | 9 -> 0x13
        | 10 -> 0x21
        | 11 -> 0x31
        | 12 -> 0x22
        | 13 -> 0x32
        | 14 -> 0x23
        | _ -> 0x33

    /// <summary>The initial rearrangement context.</summary>
    let seed =
        { Start = 0; End = 0; MoveLeft = 0; MoveRight = 0; FlipLeft = false; FlipRight = false; Phase = Idle }

    /// <summary>One step of the rearrangement transition micro-program (src/hb-aat-layout-morx-table.hh:88-162).</summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The next edit and context.</returns>
    let transition (cursor: inref<RunCursor>) (entry: Entry) (context: RearrangementContext) =
        match context.Phase with
        | MergeRange ->
            struct (Emission.Emit (RunEdit.MergeClusters (context.Start, context.End)), { context with Phase = Rearrange })
        | Rearrange ->
            struct (
                Emission.Emit (RunEdit.Rearrange (context.Start, context.End, context.MoveLeft, context.MoveRight, context.FlipLeft, context.FlipRight)),
                { context with Phase = Done })
        | Done ->
            struct (Emission.Complete, { context with Phase = Idle })
        | Idle ->
            let start = if entry.Flags &&& MarkFirstFlag <> 0us then cursor.Index else context.Start
            let finish = if entry.Flags &&& MarkLastFlag <> 0us then min (cursor.Index + 1) cursor.Input.Length else context.End
            match entry.Flags &&& VerbMask <> 0us && start < finish with
            | false -> struct (Emission.Complete, { context with Start = start; End = finish })
            | true ->
                let m = verbMap (int (entry.Flags &&& VerbMask))
                let moveLeft = min 2 (m >>> 4)
                let moveRight = min 2 (m &&& 0x0F)
                match finish - start >= moveLeft + moveRight && finish - start <= MaxContextLength with
                | false -> struct (Emission.Complete, { context with Start = start; End = finish })
                | true ->
                    struct (
                        Emission.Emit (RunEdit.MergeClusters (start, min (cursor.Index + 1) cursor.Input.Length)),
                        { context with Start = start; End = finish; MoveLeft = moveLeft; MoveRight = moveRight; FlipLeft = (m >>> 4) = 3; FlipRight = (m &&& 0x0F) = 3; Phase = MergeRange })

/// <summary>The rearrangement subtable kind, an in-place state machine.</summary>
[<Struct; NoComparison; NoEquality>]
type internal RearrangementKind =
    interface IStateMachineKind<RearrangementContext> with
        member _.InPlace = true
        member _.IsActionable entry = entry.Flags &&& 0x000Fus <> 0us
        member _.IsActionInitiable entry = entry.Flags &&& 0x8000us <> 0us
        member _.Transition (cursor, entry, context) = MorxRearrangement.transition &cursor entry context
        member _.NullTransition (cursor, entry, context) =
            let start = if entry.Flags &&& 0x8000us <> 0us then cursor.Index else context.Start
            let finish = if entry.Flags &&& 0x2000us <> 0us then min (cursor.Index + 1) cursor.Input.Length else context.End
            { context with Start = start; End = finish }

/// <summary>The decisions of a rearrangement subtable as <see cref="IRearrangementDecisions{E}"/>: the marks and the verb of its
/// entries, for the kernel that runs the transition directly.</summary>
[<Struct; NoComparison; NoEquality>]
type internal RearrangementDecisions =
    interface IRearrangementDecisions<Entry> with
        member _.Verb entry = int (entry.Flags &&& 0x000Fus)
        member _.Moves verb = MorxRearrangement.verbMap verb
        member _.MarksFirst entry = entry.Flags &&& 0x8000us <> 0us
        member _.MarksLast entry = entry.Flags &&& 0x2000us <> 0us

/// <summary>Running one rearrangement subtable over a run, on the kernel that runs the transition straight on the working copy, or
/// on the reference interpreter.</summary>
[<RequireQualifiedAccess>]
module internal MorxRearrangementDirect =
    /// <summary>Runs a rearrangement subtable's state machine.</summary>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let run (runner: MachineRunner) (dense: DenseMachine voption) (config: DriverConfig) (run: GlyphRun) =
        let kind = RearrangementKind()
        match runner, dense with
        | Kernel, ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = true; NullMask = 0xA000us }
            AatKernel.resultOf run (Sealed.runRearrangementPass (AatKernel.inputOf run) tables (RearrangementDecisions()))
        | Kernel, ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = true; NullMask = 0xA000us }
            AatKernel.resultOf run (Sealed.runRearrangementPass (AatKernel.inputOf run) tables (RearrangementDecisions()))
        | Reference, _ ->
            let struct (applied, _) = AatKernel.run Reference ValueNone config kind 0xA000us true MorxRearrangement.seed run
            applied

    /// <summary>Runs a rearrangement subtable's state machine over the working set of a line.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    let internal runSession (session: Sealed.AatSession) (dense: DenseMachine voption) (config: DriverConfig) =
        let kind = RearrangementKind()
        match dense with
        | ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = true; NullMask = 0xA000us }
            Sealed.aatSessionRearrangement session tables (RearrangementDecisions())
        | ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = true; NullMask = 0xA000us }
            Sealed.aatSessionRearrangement session tables (RearrangementDecisions())
