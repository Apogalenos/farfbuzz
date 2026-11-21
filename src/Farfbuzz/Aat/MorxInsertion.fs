namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>Where the insertion transition is within its micro-program.</summary>
[<Struct; NoComparison; NoEquality>]
type internal InsertionPhase =
    | Idle
    | MarkedChargeCheck
    | MarkedCopy
    | MarkedOutput
    | MarkedOutputSecond
    | MarkedSkip
    | MarkedMoveEnd
    | MarkedUnsafe
    | MarkedDone
    | CurrentChargeCheck
    | CurrentOutput
    | CurrentOutputSecond
    | CurrentSkip
    | CurrentMoveEnd
    | Done

/// <summary>The insertion transition context: the mark, the pending insertion micro-program and the written glyphs.</summary>
[<Struct; NoComparison; NoEquality>]
type internal InsertionContext =
    { Mark: int
      MarkLoc: int
      NullMarkBase: int
      Count: int
      Start: int
      Before: bool
      AtEnd: bool
      End: int
      MoveEnd: int
      Glyph: uint32
      OutputIndex: int
      Phase: InsertionPhase
      CollectWritten: bool
      Written: PointBuilder<uint32>
      HasDeleted: bool }

/// <summary>Reading and applying the insertion morx/mort subtable (src/hb-aat-layout-morx-table.hh:702-912).</summary>
[<RequireQualifiedAccess>]
module internal MorxInsertion =
    /// <summary>The <c>SetMark</c> entry bit, which marks the current glyph.</summary>
    [<Literal>]
    let SetMarkFlag = 0x8000us

    /// <summary>The <c>DontAdvance</c> entry bit.</summary>
    [<Literal>]
    let DontAdvanceFlag = 0x4000us

    /// <summary>The <c>CurrentInsertCount</c> mask, a five-bit count shifted left five.</summary>
    [<Literal>]
    let CurrentInsertCountMask = 0x03E0us

    /// <summary>The <c>MarkedInsertCount</c> mask, a five-bit count.</summary>
    [<Literal>]
    let MarkedInsertCountMask = 0x001Fus

    /// <summary>The <c>CurrentInsertBefore</c> bit.</summary>
    [<Literal>]
    let CurrentInsertBeforeFlag = 0x0800us

    /// <summary>The <c>MarkedInsertBefore</c> bit.</summary>
    [<Literal>]
    let MarkedInsertBeforeFlag = 0x0400us

    /// <summary>The two count fields together, <c>is_actionable</c>'s mask.</summary>
    [<Literal>]
    let ActionableMask = 0x03FFus

    /// <summary>The effective count after the runtime bounds check, <c>check_array</c>: 0 when the glyph list runs past
    /// the table (src/hb-aat-layout-morx-table.hh:808, 838).</summary>
    /// <param name="body">The subtable body, which runs to the end of the morx table.</param>
    /// <param name="insertionActionAt">The byte offset of the insertion glyph list.</param>
    /// <param name="start">The start index.</param>
    /// <param name="count">The requested count.</param>
    /// <returns>The count, or 0 when out of bounds.</returns>
    let effectiveCount (body: ReadOnlyMemory<byte>) (insertionActionAt: int64) (start: int) (count: int) =
        let glyphEnd = insertionActionAt + int64 start * 2L + int64 count * 2L
        match glyphEnd <= int64 body.Length with
        | true -> count
        | false -> 0

    /// <summary>The initial insertion context.</summary>
    /// <param name="collectWritten">Whether the written glyphs feed the growing glyph set.</param>
    /// <returns>The context.</returns>
    let seed (collectWritten: bool) =
        { Mark = 0
          MarkLoc = 0
          NullMarkBase = -1
          Count = 0
          Start = 0
          Before = false
          AtEnd = false
          End = 0
          MoveEnd = 0
          Glyph = 0u
          OutputIndex = 0
          Phase = Idle
          CollectWritten = collectWritten
          Written = PointBuilder.create ()
          HasDeleted = false }

    /// <summary>Starts the current insertion after the SetMark update, <c>transition</c>'s tail
    /// (src/hb-aat-layout-morx-table.hh:829-869).</summary>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private startCurrent (entry: Entry) (context: InsertionContext) =
        let mark = if entry.Flags &&& SetMarkFlag <> 0us then context.MarkLoc else context.Mark
        match entry.Data0 with
        | 0xFFFFus -> struct (Emission.Complete, { context with Mark = mark; Phase = Idle })
        | currentIndex ->
            let count = int ((entry.Flags &&& CurrentInsertCountMask) >>> 5)
            struct (
                Emission.Emit (RunEdit.ChargeOps count),
                { context with
                    Mark = mark
                    Count = count
                    Start = int currentIndex
                    Before = entry.Flags &&& CurrentInsertBeforeFlag <> 0us
                    OutputIndex = 0
                    Phase = CurrentChargeCheck })

    /// <summary>Emits the first edit of one output glyph, <c>output_glyphs</c>'s per-glyph step
    /// (src/hb-aat-layout-common.hh:202-231).</summary>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="insertionActionAt">The byte offset of the insertion glyph list.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context, with the glyph remembered for the second edit.</returns>
    let private outputFirst (applyContext: AatContext) (body: ReadOnlyMemory<byte>) (insertionActionAt: int64)
        (cursor: inref<RunCursor>) (context: InsertionContext) =
        let glyph = uint32 (Binary.u16 body.Span (int (insertionActionAt + int64 (context.Start + context.OutputIndex) * 2L)))
        let written = if context.CollectWritten then PointBuilder.append context.Written glyph else context.Written
        let deleted = context.HasDeleted || glyph = AatLookup.DeletedGlyph
        match context.AtEnd with
        | true -> struct (Emission.Emit (RunEdit.OutputGlyph glyph), { context with Glyph = glyph; Written = written; HasDeleted = deleted })
        | false ->
            let props = AatApply.propsFor applyContext glyph (cursor.InputProps[cursor.Index])
            struct (Emission.Emit (RunEdit.SetCurrentProps props), { context with Glyph = glyph; Written = written; HasDeleted = deleted })

    /// <summary>Emits the second edit of one output glyph, finishing the per-glyph step.</summary>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context, with the output index advanced.</returns>
    let private outputSecond (applyContext: AatContext) (cursor: inref<RunCursor>) (context: InsertionContext) =
        match context.AtEnd with
        | true ->
            let props = AatApply.propsFor applyContext context.Glyph (cursor.OutputProps[cursor.Output.Length - 1])
            struct (Emission.Emit (RunEdit.SetOutputGlyph (cursor.Output.Length - 1, context.Glyph, props)), { context with OutputIndex = context.OutputIndex + 1 })
        | false ->
            struct (Emission.Emit (RunEdit.OutputGlyph context.Glyph), { context with OutputIndex = context.OutputIndex + 1 })

    /// <summary>The copy decision and the output-loop entry of one insertion block: a copy, the move to the end, or the
    /// first output edit (src/hb-aat-layout-morx-table.hh:811-824, 845-851).</summary>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="insertionActionAt">The byte offset of the insertion glyph list.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <param name="outputPhase">The phase of the output loop.</param>
    /// <param name="outputSecondPhase">The phase of a glyph's second edit.</param>
    /// <param name="moveEndPhase">The phase after the block's final move.</param>
    /// <param name="skipPhase">The phase that skips the copied glyph, when there is nothing to output.</param>
    /// <returns>The edit and the context.</returns>
    let private copyOrOutput (applyContext: AatContext) (body: ReadOnlyMemory<byte>) (insertionActionAt: int64)
        (cursor: inref<RunCursor>) (context: InsertionContext) (outputPhase: InsertionPhase) (outputSecondPhase: InsertionPhase) (moveEndPhase: InsertionPhase) (skipPhase: InsertionPhase) =
        let atEnd = cursor.Index = cursor.Input.Length
        let doCopy = cursor.Index < cursor.Input.Length && not context.Before
        match doCopy, context.Count with
        | true, 0 -> struct (Emission.Emit RunEdit.CopyGlyph, { context with AtEnd = atEnd; Phase = skipPhase })
        | true, _ -> struct (Emission.Emit RunEdit.CopyGlyph, { context with AtEnd = atEnd; Phase = outputPhase })
        | false, 0 -> struct (Emission.Emit (RunEdit.MoveTo context.MoveEnd), { context with AtEnd = atEnd; Phase = moveEndPhase })
        | false, _ ->
            let struct (emission, next) = outputFirst applyContext body insertionActionAt &cursor { context with AtEnd = atEnd }
            struct (emission, { next with Phase = outputSecondPhase })

    /// <summary>One step of the insertion transition micro-program (src/hb-aat-layout-morx-table.hh:793-870).</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="insertionActionAt">The byte offset of the insertion glyph list.</param>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The next edit and context.</returns>
    let transition (body: ReadOnlyMemory<byte>) (insertionActionAt: int64) (applyContext: AatContext)
        (cursor: inref<RunCursor>) (entry: Entry) (context: InsertionContext) =
        match cursor.Successful with
        | false -> struct (Emission.Complete, context)
        | true ->
            match context.Phase with
            | Idle ->
                let ctx = { context with MarkLoc = cursor.Output.Length; NullMarkBase = -1 }
                match entry.Data1 with
                | 0xFFFFus -> startCurrent entry ctx
                | markedIndex ->
                    let count = int (entry.Flags &&& MarkedInsertCountMask)
                    struct (
                        Emission.Emit (RunEdit.ChargeOps count),
                        { ctx with
                            Count = count
                            Start = int markedIndex
                            Before = entry.Flags &&& MarkedInsertBeforeFlag <> 0us
                            OutputIndex = 0
                            Phase = MarkedChargeCheck })
            | MarkedChargeCheck ->
                match cursor.MaxOps <= 0 with
                | true -> struct (Emission.Complete, { context with Phase = Idle })
                | false ->
                    let count = effectiveCount body insertionActionAt context.Start context.Count
                    let blockEnd = cursor.Output.Length
                    struct (
                        Emission.Emit (RunEdit.MoveTo context.Mark),
                        { context with Count = count; End = blockEnd; MoveEnd = blockEnd + count; Phase = MarkedCopy })
            | MarkedCopy -> copyOrOutput applyContext body insertionActionAt &cursor context MarkedOutput MarkedOutputSecond MarkedUnsafe MarkedSkip
            | MarkedOutput ->
                let struct (emission, next) = outputFirst applyContext body insertionActionAt &cursor context
                struct (emission, { next with Phase = MarkedOutputSecond })
            | MarkedOutputSecond ->
                let struct (emission, next) = outputSecond applyContext &cursor context
                let phase = if next.OutputIndex >= next.Count then MarkedSkip else MarkedOutput
                struct (emission, { next with Phase = phase })
            | MarkedSkip ->
                match cursor.Index < cursor.Input.Length && not context.Before with
                | true -> struct (Emission.Emit RunEdit.SkipGlyph, { context with Phase = MarkedMoveEnd })
                | false -> struct (Emission.Emit (RunEdit.MoveTo context.MoveEnd), { context with Phase = MarkedUnsafe })
            | MarkedMoveEnd ->
                struct (Emission.Emit (RunEdit.MoveTo context.MoveEnd), { context with Phase = MarkedUnsafe })
            | MarkedUnsafe ->
                struct (
                    Emission.Emit (RunEdit.UnsafeToBreakFromOutbuffer (context.Mark, min (cursor.Index + 1) cursor.Input.Length)),
                    { context with Phase = MarkedDone })
            | MarkedDone -> startCurrent entry context
            | CurrentChargeCheck ->
                match cursor.MaxOps <= 0 with
                | true -> struct (Emission.Complete, { context with Phase = Idle })
                | false ->
                    let count = effectiveCount body insertionActionAt context.Start context.Count
                    let blockEnd = cursor.Output.Length
                    let moveEnd = if entry.Flags &&& DontAdvanceFlag <> 0us then blockEnd else blockEnd + count
                    copyOrOutput applyContext body insertionActionAt &cursor { context with Count = count; End = blockEnd; MoveEnd = moveEnd } CurrentOutput CurrentOutputSecond Done CurrentSkip
            | CurrentOutput ->
                let struct (emission, next) = outputFirst applyContext body insertionActionAt &cursor context
                struct (emission, { next with Phase = CurrentOutputSecond })
            | CurrentOutputSecond ->
                let struct (emission, next) = outputSecond applyContext &cursor context
                let phase = if next.OutputIndex >= next.Count then CurrentSkip else CurrentOutput
                struct (emission, { next with Phase = phase })
            | CurrentSkip ->
                match cursor.Index < cursor.Input.Length && not context.Before with
                | true -> struct (Emission.Emit RunEdit.SkipGlyph, { context with Phase = CurrentMoveEnd })
                | false -> struct (Emission.Emit (RunEdit.MoveTo context.MoveEnd), { context with Phase = Done })
            | CurrentMoveEnd ->
                struct (Emission.Emit (RunEdit.MoveTo context.MoveEnd), { context with Phase = Done })
            | Done -> struct (Emission.Complete, { context with Phase = Idle })

/// <summary>The insertion morx/mort subtable kind (src/hb-aat-layout-morx-table.hh:702-912).</summary>
[<Struct; NoComparison; NoEquality>]
type internal InsertionKind =
    { Body: ReadOnlyMemory<byte>
      InsertionActionAt: int64
      Context: AatContext }

    interface IStateMachineKind<InsertionContext> with
        member _.InPlace = false
        member this.IsActionable entry =
            (entry.Flags &&& MorxInsertion.ActionableMask) <> 0us
            && (entry.Data0 <> 0xFFFFus || entry.Data1 <> 0xFFFFus)
        member _.IsActionInitiable entry = entry.Flags &&& MorxInsertion.SetMarkFlag <> 0us
        member this.Transition (cursor, entry, context) =
            MorxInsertion.transition this.Body this.InsertionActionAt this.Context &cursor entry context
        member _.NullTransition (cursor, entry, context) =
            match entry.Flags &&& MorxInsertion.SetMarkFlag <> 0us with
            | true ->
                let offset = if context.NullMarkBase < 0 then cursor.Output.Length - cursor.Index else context.NullMarkBase
                { context with Mark = cursor.Index + offset; NullMarkBase = offset }
            | false -> context

/// <summary>The decisions of an insertion subtable as <see cref="IInsertionDecisions{E}"/>: the flags, counts and indices of its
/// entries and the glyph list, for the kernel that runs the transition directly.</summary>
[<Struct; NoComparison; NoEquality; System.Runtime.CompilerServices.IsReadOnly>]
type internal InsertionDecisions =
    { Kind: InsertionKind
      CollectWritten: bool }

    interface IInsertionDecisions<Entry> with
        member this.Collect = this.CollectWritten
        member _.SetsMark entry = entry.Flags &&& MorxInsertion.SetMarkFlag <> 0us
        member _.DontAdvance entry = entry.Flags &&& MorxInsertion.DontAdvanceFlag <> 0us
        member _.MarkedIndex entry = entry.Data1
        member _.MarkedCount entry = int (entry.Flags &&& MorxInsertion.MarkedInsertCountMask)
        member _.MarkedBefore entry = entry.Flags &&& MorxInsertion.MarkedInsertBeforeFlag <> 0us
        member _.CurrentIndex entry = entry.Data0
        member _.CurrentCount entry = int ((entry.Flags &&& MorxInsertion.CurrentInsertCountMask) >>> 5)
        member _.CurrentBefore entry = entry.Flags &&& MorxInsertion.CurrentInsertBeforeFlag <> 0us
        member this.EffectiveCount (start, count) = MorxInsertion.effectiveCount this.Kind.Body this.Kind.InsertionActionAt start count
        member this.ReadGlyph index = uint32 (Binary.u16 this.Kind.Body.Span (int (this.Kind.InsertionActionAt + int64 index * 2L)))
        member this.Props (glyph, props) = AatApply.propsFor this.Kind.Context glyph props

/// <summary>Running one insertion subtable over a run, on the kernel that runs the transition straight on the working copy, or on
/// the reference interpreter.</summary>
[<RequireQualifiedAccess>]
module internal MorxInsertionDirect =
    /// <summary>Runs an insertion subtable's state machine.</summary>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="collect">Whether the glyphs written are collected for the buffer glyph set.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable, the glyphs written when collecting, and whether a deleted glyph was written.</returns>
    let run (runner: MachineRunner) (dense: DenseMachine voption) (config: DriverConfig) (kind: InsertionKind) (collect: bool) (run: GlyphRun) =
        let decisions = { InsertionDecisions.Kind = kind; CollectWritten = collect }
        match runner, dense with
        | Kernel, ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = false; NullMask = 0x8000us }
            let struct (result, written, deleted) = Sealed.runInsertionPass (AatKernel.inputOf run) tables decisions
            struct (AatKernel.resultOf run result, written, deleted)
        | Kernel, ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = false; NullMask = 0x8000us }
            let struct (result, written, deleted) = Sealed.runInsertionPass (AatKernel.inputOf run) tables decisions
            struct (AatKernel.resultOf run result, written, deleted)
        | Reference, _ ->
            let struct (applied, context) = AatKernel.run Reference ValueNone config kind 0x8000us false (MorxInsertion.seed collect) run
            struct (applied, PointBuilder.publish context.Written, context.HasDeleted)

    /// <summary>Runs an insertion subtable's state machine over the working set of a line, the glyphs written going to the
    /// session's collection.</summary>
    /// <param name="session">The working set.</param>
    /// <param name="dense">The compiled state table, or none to read the table as it is.</param>
    /// <param name="config">The driver config.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="collect">Whether the glyphs written are collected for the buffer glyph set.</param>
    /// <returns>Whether a deleted glyph was written.</returns>
    let internal runSession (session: Sealed.AatSession) (dense: DenseMachine voption) (config: DriverConfig) (kind: InsertionKind) (collect: bool) =
        let decisions = { InsertionDecisions.Kind = kind; CollectWritten = collect }
        match dense with
        | ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = false; NullMask = 0x8000us }
            Sealed.aatSessionInsertion session tables decisions
        | ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = false; NullMask = 0x8000us }
            Sealed.aatSessionInsertion session tables decisions
