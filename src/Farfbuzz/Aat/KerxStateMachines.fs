namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>The eight-slot circular kerning history of a kerx or kern format 1 state machine.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Stack8 =
    { S0: int
      S1: int
      S2: int
      S3: int
      S4: int
      S5: int
      S6: int
      S7: int }

/// <summary>Reading and replacing the slots of a <see cref="Stack8"/> without an array.</summary>
[<RequireQualifiedAccess>]
module internal Stack8 =
    /// <summary>The empty history.</summary>
    let empty =
        { S0 = 0; S1 = 0; S2 = 0; S3 = 0
          S4 = 0; S5 = 0; S6 = 0; S7 = 0 }

    /// <summary>Reads one slot of the history.</summary>
    /// <param name="stack">The history.</param>
    /// <param name="slot">The slot, 0 to 7.</param>
    /// <returns>The slot's value.</returns>
    let get (stack: Stack8) (slot: int) =
        match slot with
        | 0 -> stack.S0
        | 1 -> stack.S1
        | 2 -> stack.S2
        | 3 -> stack.S3
        | 4 -> stack.S4
        | 5 -> stack.S5
        | 6 -> stack.S6
        | _ -> stack.S7

    /// <summary>Replaces one slot of the history.</summary>
    /// <param name="stack">The history.</param>
    /// <param name="slot">The slot, 0 to 7.</param>
    /// <param name="value">The new value.</param>
    /// <returns>The history with the slot replaced.</returns>
    let set (stack: Stack8) (slot: int) (value: int) =
        match slot with
        | 0 -> { stack with S0 = value }
        | 1 -> { stack with S1 = value }
        | 2 -> { stack with S2 = value }
        | 3 -> { stack with S3 = value }
        | 4 -> { stack with S4 = value }
        | 5 -> { stack with S5 = value }
        | 6 -> { stack with S6 = value }
        | _ -> { stack with S7 = value }

/// <summary>Where the kerx/kern format 1 transition is within its action micro-program.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Format1Phase =
    | Idle
    | Done
    | Failing
    | Charge
    | ObsoleteRead
    | Unsafe
    | Apply
    | Scratch

/// <summary>The fixed data of one kerx or kern format 1 subtable: the state machine's address, the kern-action
/// array's address, the runtime read bound and the tuple count.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Format1Spec =
    { Extended: bool
      TableBytes: ReadOnlyMemory<byte>
      RangeEnd: int
      MachineAt: int
      KernActionAt: int
      TupleCount: uint32 }

/// <summary>The transition context of a kerx or kern format 1 subtable: the eight-slot history and the action
/// micro-program state.</summary>
[<Struct; NoComparison; NoEquality>]
type internal KerxFormat1Context =
    { Stack: Stack8
      Depth: int
      StackPos: int
      WalkCursor: int
      Last: bool
      ActionsAt: int64
      Idx: int
      Value: int
      Position: GlyphPosition
      ScratchPending: bool
      Phase: Format1Phase }

/// <summary>Reading and applying the kerx and kern format 1 subtables
/// (src/hb-aat-layout-kerx-table.hh:164-457).</summary>
[<RequireQualifiedAccess>]
module internal KerxStateMachines =
    /// <summary>The <c>Push</c> entry bit, which pushes the current glyph on the history.</summary>
    [<Literal>]
    let PushFlag = 0x8000us

    /// <summary>The <c>Offset</c> entry mask of the obsolete kern table, the action byte offset.</summary>
    [<Literal>]
    let private OffsetMask = 0x3FFFus

    /// <summary>The history length, <c>ARRAY_LENGTH (stack)</c>.</summary>
    [<Literal>]
    let private StackLength = 8

    /// <summary>The byte width of one kern action word, <c>FWORD::static_size</c>.</summary>
    [<Literal>]
    let private FwordSize = 2

    /// <summary>The cross-stream reset value, <c>-0x8000</c>.</summary>
    [<Literal>]
    let private ResetValue = -0x8000

    /// <summary>The mask that clears the attach type and chain, <c>var</c>'s low three bytes.</summary>
    [<Literal>]
    let private AttachClearMask = 0xFF000000u

    /// <summary>Adds two positions with int saturation, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The augend.</param>
    /// <param name="b">The addend.</param>
    /// <returns>The saturating sum.</returns>
    let private saturateAdd (a: int) (b: int) =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Whether an entry performs an action, <c>Format1EntryT::performAction</c>.</summary>
    /// <param name="extended">True for kerx, false for kern.</param>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry has an action index.</returns>
    let performAction (extended: bool) (entry: Entry) =
        match extended with
        | true -> entry.Data0 <> 0xFFFFus
        | false -> entry.Flags &&& OffsetMask <> 0us

    /// <summary>The byte offset of an entry's first kern-action word, relative to the table; the obsolete offset is
    /// turned into a whole word index first, <c>ObsoleteTypes::offsetToIndex</c>.</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="entry">The entry.</param>
    /// <returns>The word byte offset.</returns>
    let private actionAtOf (spec: Format1Spec) (entry: Entry) =
        match spec.Extended with
        | true -> int64 spec.KernActionAt + int64 entry.Data0 * 2L
        | false ->
            let arrayOffset = spec.KernActionAt - spec.MachineAt
            let index = (int (entry.Flags &&& OffsetMask) - arrayOffset) / FwordSize
            int64 spec.KernActionAt + int64 index * int64 FwordSize

    /// <summary>Pushes a glyph index onto the history, <c>driver_context_t::transition</c>'s push.</summary>
    /// <param name="extended">True for kerx, false for kern.</param>
    /// <param name="index">The glyph index.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The context with the glyph pushed.</returns>
    let push (extended: bool) (index: int) (context: KerxFormat1Context) =
        match extended with
        | true ->
            { context with
                Stack = Stack8.set context.Stack context.StackPos index
                StackPos = (context.StackPos + 1) % StackLength
                Depth = min (context.Depth + 1) StackLength }
        | false ->
            match context.Depth < StackLength with
            | true -> { context with Stack = Stack8.set context.Stack context.Depth index; Depth = context.Depth + 1 }
            | false -> { context with Depth = 0 }

    /// <summary>The initial transition context.</summary>
    /// <returns>The context.</returns>
    let seed () =
        { Stack = Stack8.empty
          Depth = 0
          StackPos = 0
          WalkCursor = 0
          Last = false
          ActionsAt = 0L
          Idx = 0
          Value = 0
          Position = { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
          ScratchPending = false
          Phase = Idle }

    /// <summary>What applying one kern value produced: no edit, or a position and a scratch flag.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private ApplyOutcome =
        | NoEdit
        | PositionEdit of position: GlyphPosition * scratch: bool

    /// <summary>Applies one kern value to a position, <c>driver_context_t::transition</c>'s value cases
    /// (src/hb-aat-layout-kerx-table.hh:352-402).</summary>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="kernMask">The kern feature mask.</param>
    /// <param name="position">The glyph's position.</param>
    /// <param name="mask">The glyph's mask.</param>
    /// <param name="value">The kern value.</param>
    /// <returns>The outcome.</returns>
    let private applyValue (font: Font) (horizontal: bool) (crossStream: bool) (kernMask: uint32) (position: GlyphPosition) (mask: uint32) (value: int) =
        match horizontal, crossStream with
        | true, true ->
            match value = ResetValue with
            | true -> PositionEdit ({ position with Var = position.Var &&& AttachClearMask; YOffset = 0 }, false)
            | false ->
                match byte ((position.Var >>> 16) &&& 0xFFu) <> 0uy with
                | false -> NoEdit
                | true -> PositionEdit ({ position with YOffset = saturateAdd position.YOffset (Font.emScaleY font value) }, true)
        | false, true ->
            match value = ResetValue with
            | true -> PositionEdit ({ position with Var = position.Var &&& AttachClearMask; XOffset = 0 }, false)
            | false ->
                match byte ((position.Var >>> 16) &&& 0xFFu) <> 0uy with
                | false -> NoEdit
                | true -> PositionEdit ({ position with XOffset = saturateAdd position.XOffset (Font.emScaleX font value) }, true)
        | true, false ->
            match mask &&& kernMask <> 0u with
            | false -> NoEdit
            | true ->
                let scaled = Font.emScaleX font value
                PositionEdit ({ position with XAdvance = saturateAdd position.XAdvance scaled; XOffset = saturateAdd position.XOffset scaled }, false)
        | false, false ->
            match mask &&& kernMask <> 0u with
            | false -> NoEdit
            | true ->
                let scaled = Font.emScaleY font value
                PositionEdit ({ position with YAdvance = saturateAdd position.YAdvance scaled; YOffset = saturateAdd position.YOffset scaled }, false)

    /// <summary>The next iteration phase of an action loop.</summary>
    /// <param name="extended">True for kerx, false for kern.</param>
    /// <returns>The phase.</returns>
    let private nextIteration (extended: bool) =
        match extended with
        | true -> Charge
        | false -> ObsoleteRead

    /// <summary>The extended walk step after a value was read: the tuple bounds check, then the next glyph of the
    /// history (src/hb-aat-layout-kerx-table.hh:323-345).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <param name="value">The value read.</param>
    /// <returns>The edit and the context.</returns>
    let private walkStep (spec: Format1Spec) (cursor: inref<RunCursor>) (context: KerxFormat1Context) value =
        match spec.TupleCount > 1u && context.ActionsAt + int64 spec.TupleCount * 2L > int64 spec.RangeEnd with
        | true -> struct (Emission.Emit (RunEdit.ChargeOps 1), { context with Depth = 0; Phase = Done })
        | false ->
            let walkCursor = (context.WalkCursor + StackLength - 1) % StackLength
            let idx = Stack8.get context.Stack walkCursor
            let next =
                { context with
                    WalkCursor = walkCursor
                    ActionsAt = context.ActionsAt + int64 (max 1u spec.TupleCount) * 2L }
            match idx >= cursor.Input.Length with
            | true -> struct (Emission.Emit (RunEdit.ChargeOps 1), { next with Phase = Charge })
            | false -> struct (Emission.Emit (RunEdit.ChargeOps 1), { next with Idx = idx; Value = value; Phase = Unsafe })

    /// <summary>The extended read step after the budget check: the word's bounds, the terminator, then the walk
    /// (src/hb-aat-layout-kerx-table.hh:314-323).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private readStep (spec: Format1Spec) (cursor: inref<RunCursor>) (context: KerxFormat1Context) =
        match Sanitize.fits spec.RangeEnd context.ActionsAt 2 with
        | false -> struct (Emission.Emit (RunEdit.ChargeOps 1), { context with Depth = 0; Phase = Done })
        | true ->
            let value = int (Binary.i16 spec.TableBytes.Span (int context.ActionsAt))
            match value = -1 with
            | true -> struct (Emission.Emit (RunEdit.ChargeOps 1), { context with Phase = Done })
            | false -> walkStep spec &cursor context value

    /// <summary>The extended charge-and-read step: charges one operation and reads the next value
    /// (src/hb-aat-layout-kerx-table.hh:305-343).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private chargeStep (spec: Format1Spec) (cursor: inref<RunCursor>) (context: KerxFormat1Context) =
        match cursor.MaxOps <= 0 with
        | true -> struct (Emission.Emit (RunEdit.ChargeOps 1), { context with Phase = Failing })
        | false -> readStep spec &cursor context

    /// <summary>The obsolete pop-and-read step: pops one glyph, reads its value and decides
    /// (src/hb-aat-layout-kerx-table.hh:331-350).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="kernMask">The kern feature mask.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private obsoleteReadStep (spec: Format1Spec) (font: Font) (horizontal: bool) (crossStream: bool) (kernMask: uint32)
        (cursor: inref<RunCursor>) (context: KerxFormat1Context) =
        match context.Depth <= 0 || context.Last with
        | true -> struct (Emission.Complete, { context with Phase = Idle })
        | false ->
            let depth = context.Depth - 1
            let idx = Stack8.get context.Stack depth
            let value = int (Binary.i16 spec.TableBytes.Span (int context.ActionsAt))
            let next = { context with Depth = depth; ActionsAt = context.ActionsAt + 2L }
            match idx >= cursor.Input.Length with
            | true -> struct (Emission.Emit (RunEdit.NextGlyphs 0), { next with Phase = ObsoleteRead })
            | false ->
                let last = value &&& 1 <> 0
                let cleared = value &&& ~~~1
                match applyValue font horizontal crossStream kernMask (cursor.Positions[idx]) (cursor.Input[idx].Mask) cleared with
                | NoEdit -> struct (Emission.Emit (RunEdit.NextGlyphs 0), { next with Last = last; Phase = ObsoleteRead })
                | PositionEdit (position, scratch) ->
                    struct (
                        Emission.Emit (RunEdit.NextGlyphs 0),
                        { next with Last = last; Idx = idx; Value = cleared; Position = position; ScratchPending = scratch; Phase = Apply })

    /// <summary>The start of a transition: the push, then the action list when the entry performs one
    /// (src/hb-aat-layout-kerx-table.hh:263-304).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="kernMask">The kern feature mask.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private startStep (spec: Format1Spec) (font: Font) (horizontal: bool) (crossStream: bool) (kernMask: uint32)
        (cursor: inref<RunCursor>) (entry: Entry) (context: KerxFormat1Context) =
        let pushed = if entry.Flags &&& PushFlag <> 0us then push spec.Extended cursor.Index context else context
        match performAction spec.Extended entry && pushed.Depth > 0 with
        | false -> struct (Emission.Complete, pushed)
        | true ->
            let actionsAt = actionAtOf spec entry
            match spec.Extended with
            | true -> chargeStep spec &cursor { pushed with WalkCursor = pushed.StackPos; Last = false; ActionsAt = actionsAt; Phase = Charge }
            | false ->
                let flagsOffset = int (entry.Flags &&& OffsetMask)
                let kernActionOffset = spec.KernActionAt - spec.MachineAt
                match flagsOffset < kernActionOffset || actionsAt + int64 pushed.Depth * 2L > int64 spec.RangeEnd with
                | true -> struct (Emission.Complete, { pushed with Depth = 0; Last = false; Phase = Idle })
                | false -> obsoleteReadStep spec font horizontal crossStream kernMask &cursor { pushed with WalkCursor = 0; Last = false; ActionsAt = actionsAt; Phase = ObsoleteRead }

    /// <summary>One step of the format 1 transition micro-program
    /// (src/hb-aat-layout-kerx-table.hh:263-405).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="kernMask">The kern feature mask.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let transition (spec: Format1Spec) (font: Font) (horizontal: bool) (crossStream: bool) (kernMask: uint32)
        (cursor: inref<RunCursor>) (entry: Entry) (context: KerxFormat1Context) =
        match context.Phase with
        | Idle -> startStep spec font horizontal crossStream kernMask &cursor entry context
        | Done -> struct (Emission.Complete, { context with Phase = Idle })
        | Failing -> struct (Emission.Emit RunEdit.Fail, { context with Phase = Done })
        | Charge -> chargeStep spec &cursor context
        | ObsoleteRead -> obsoleteReadStep spec font horizontal crossStream kernMask &cursor context
        | Unsafe ->
            match applyValue font horizontal crossStream kernMask (cursor.Positions[context.Idx]) (cursor.Input[context.Idx].Mask) context.Value with
            | NoEdit -> struct (Emission.Emit (RunEdit.UnsafeToBreak (context.Idx, cursor.Index + 1)), { context with Phase = Charge })
            | PositionEdit (position, scratch) ->
                struct (
                    Emission.Emit (RunEdit.UnsafeToBreak (context.Idx, cursor.Index + 1)),
                    { context with Position = position; ScratchPending = scratch; Phase = Apply })
        | Apply ->
            struct (
                Emission.Emit (RunEdit.SetPosition (context.Idx, context.Position)),
                { context with Phase = (match context.ScratchPending with | true -> Scratch | false -> nextIteration spec.Extended) })
        | Scratch ->
            struct (Emission.Emit (RunEdit.AddScratch ScratchFlags.HasGposAttachment), { context with Phase = nextIteration spec.Extended })

/// <summary>The kerx and kern format 1 subtable kind (src/hb-aat-layout-kerx-table.hh:223-456).</summary>
[<Struct; NoComparison; NoEquality>]
type internal KerxFormat1Kind =
    { Spec: Format1Spec
      Font: Font
      Horizontal: bool
      CrossStream: bool
      KernMask: uint32 }

    interface IStateMachineKind<KerxFormat1Context> with
        member _.InPlace = true
        member this.IsActionable entry = KerxStateMachines.performAction this.Spec.Extended entry
        member _.IsActionInitiable entry = entry.Flags &&& KerxStateMachines.PushFlag <> 0us
        member this.Transition (cursor, entry, context) =
            KerxStateMachines.transition this.Spec this.Font this.Horizontal this.CrossStream this.KernMask &cursor entry context
        member this.NullTransition (cursor, entry, context) =
            match entry.Flags &&& KerxStateMachines.PushFlag <> 0us with
            | true -> KerxStateMachines.push this.Spec.Extended cursor.Index context
            | false -> context

/// <summary>Applying the kerx and kern format 1 subtables, <c>KerxSubTableFormat1::apply</c>.</summary>
[<RequireQualifiedAccess>]
module internal KerxStateMachineApply =
    /// <summary>The entry flag that makes the null transition of a state-machine kind change the context: push or mark.</summary>
    [<Literal>]
    let private PushMask = 0x8000us

    /// <summary>Runs one format 1 subtable's state machine, <c>KerxSubTableFormat1::apply</c>, with the implementation chosen.</summary>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none.</param>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="machine">The decoded state machine.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let applyWith (runner: MachineRunner) (dense: DenseMachine voption) (spec: Format1Spec) (machine: StateMachine) (plan: OtShapePlan) (font: Font) (horizontal: bool) (crossStream: bool) (run: GlyphRun) =
        match plan.RequestedKerning || crossStream with
        | false -> run
        | true ->
            let kind =
                { Spec = spec
                  Font = font
                  Horizontal = horizontal
                  CrossStream = crossStream
                  KernMask = plan.KernMask }
            let config =
                let endEntry = StateTable.entry machine StateTable.StartOfText StateTable.ClassEndOfText
                { Machine = machine
                  Ranges = ImmutableArray.Empty
                  SubtableFlags = 0u
                  NumGlyphs = font.Ot.Face.GlyphCount
                  StartSafeToBreakAtEndOfText = not (KerxStateMachines.performAction spec.Extended endEntry) }
            let struct (applied, _) = AatKernel.run runner dense config kind PushMask true (KerxStateMachines.seed ()) run
            applied

    /// <summary>Runs one format 1 subtable's state machine, <c>KerxSubTableFormat1::apply</c>.</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="machine">The decoded state machine.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let apply (spec: Format1Spec) (machine: StateMachine) (plan: OtShapePlan) (font: Font) (horizontal: bool) (crossStream: bool) (run: GlyphRun) =
        applyWith Kernel ValueNone spec machine plan font horizontal crossStream run

/// <summary>The initial-glyph gate of <c>KerxTable::apply</c>, shared by the kerx and the Apple kern loops.</summary>
[<RequireQualifiedAccess>]
module internal MachineGate =
    /// <summary>Whether a glyph lies in a sorted disjoint range set, <c>hb_set_t::has</c>.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>True when the glyph is covered.</returns>
    [<TailCall>]
    let rec rangesHas (ranges: ReadOnlySpan<CodepointRange>) (glyph: uint32) (lo: int) (hi: int) =
        match lo > hi with
        | true -> false
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match glyph < ranges[mid].First, glyph <= ranges[mid].Last with
            | true, _ -> rangesHas ranges glyph lo (mid - 1)
            | false, true -> true
            | false, false -> rangesHas ranges glyph (mid + 1) hi

    /// <summary>Scans the buffer for a glyph in the first set, <c>buffer_intersects_machine</c>'s short path.</summary>
    /// <param name="infos">The glyphs.</param>
    /// <param name="firstSet">The first glyph set.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>True when the buffer intersects.</returns>
    [<TailCall>]
    let rec scanIntersects (infos: ReadOnlySpan<GlyphInfo>) (firstSet: ReadOnlySpan<CodepointRange>) (index: int) =
        match index >= infos.Length with
        | true -> false
        | false ->
            match rangesHas firstSet infos[index].Codepoint 0 (firstSet.Length - 1) with
            | true -> true
            | false -> scanIntersects infos firstSet (index + 1)

/// <summary>Where the kerx format 4 transition is within its edit sequence: the position is emitted as the transition
/// starts, then the scratch flag, then the completion that updates the mark; a control point action ends after its
/// budget charge.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Format4Phase =
    | Fresh
    | Flagging
    | Closing
    | Charged

/// <summary>The fixed data of one kerx format 4 subtable: the table bytes, the runtime read bound, the byte offset of the
/// ankr action data and the two-bit action type.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Format4Spec =
    { TableBytes: ReadOnlyMemory<byte>
      RangeEnd: int
      ActionsAt: int64
      ActionType: int }

/// <summary>The transition context of a kerx format 4 subtable: the marked glyph and the edit sequence state.</summary>
[<Struct; NoComparison; NoEquality>]
type internal KerxFormat4Context =
    { MarkSet: bool
      Mark: int
      Phase: Format4Phase }

/// <summary>Applying the kerx format 4 subtable (src/hb-aat-layout-kerx-table.hh:547-748).</summary>
[<RequireQualifiedAccess>]
module internal KerxFormat4Machine =
    /// <summary>The <c>Mark</c> entry bit, which remembers the glyph as the marked glyph.</summary>
    [<Literal>]
    let MarkFlag = 0x8000us

    /// <summary>The entry data meaning no action, <c>ankrActionIndex</c> 0xFFFF.</summary>
    [<Literal>]
    let NoAction = 0xFFFFus

    /// <summary>The operations two glyph outline lookups charge, <c>2 * HB_BUDGET_256</c>.</summary>
    [<Literal>]
    let private ControlPointCharge = 512

    /// <summary>The control point action, <c>action_type</c> 0.</summary>
    [<Literal>]
    let private ControlPointAction = 0

    /// <summary>The anchor point action, <c>action_type</c> 1.</summary>
    [<Literal>]
    let private AnchorPointAction = 1

    /// <summary>The control point coordinate action, <c>action_type</c> 2.</summary>
    [<Literal>]
    let private CoordinateAction = 2

    /// <summary>The byte width of the two 16-bit fields of an ankr action, <c>2 * HBUINT16::static_size</c>.</summary>
    [<Literal>]
    let private PairActionSize = 4

    /// <summary>The byte width of the four FWORDs of a coordinate action, <c>4 * FWORD::static_size</c>.</summary>
    [<Literal>]
    let private CoordinateActionSize = 8

    /// <summary>The mask that keeps what the attach chain and type do not hold, <c>var</c>'s top byte.</summary>
    [<Literal>]
    let private AttachKeepMask = 0xFF000000u

    /// <summary>The mark attachment type, <c>ATTACH_TYPE_MARK</c>, in the third byte of <c>var</c>.</summary>
    [<Literal>]
    let private AttachTypeMark = 0x00010000u

    /// <summary>The cursive attachment type, <c>ATTACH_TYPE_CURSIVE</c>.</summary>
    [<Literal>]
    let private AttachTypeCursive = 0x02uy

    /// <summary>Adds two positions with int saturation, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The augend.</param>
    /// <param name="b">The addend.</param>
    /// <returns>The saturating sum.</returns>
    let private saturateAdd (a: int) (b: int) =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Subtracts two positions with int saturation, <c>hb_saturate_sub</c>.</summary>
    /// <param name="a">The minuend.</param>
    /// <param name="b">The subtrahend.</param>
    /// <returns>The saturating difference.</returns>
    let private saturateSub (a: int) (b: int) =
        int (Math.Clamp(int64 a - int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>The initial transition context, <c>mark_set (false), mark (0)</c>.</summary>
    /// <returns>The context.</returns>
    let seed () =
        { MarkSet = false
          Mark = 0
          Phase = Fresh }

    /// <summary>Whether an entry performs an action, <c>is_actionable</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry has an action index.</returns>
    let performAction (entry: Entry) =
        entry.Data0 <> NoAction

    /// <summary>Remembers the glyph when the entry asks for it, the closing <c>if (entry.flags &amp; Mark)</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <param name="index">The current glyph index.</param>
    /// <param name="context">The context.</param>
    /// <returns>The context with the mark updated.</returns>
    let remember (entry: Entry) (index: int) (context: KerxFormat4Context) =
        match entry.Flags &&& MarkFlag <> 0us with
        | true -> { context with MarkSet = true; Mark = index; Phase = Fresh }
        | false -> { context with Phase = Fresh }

    /// <summary>The attach chain of a position, <c>attach_chain()</c>, the signed low 16 bits of <c>var</c>.</summary>
    /// <param name="position">The position.</param>
    /// <returns>The chain.</returns>
    let private attachChain (position: GlyphPosition) =
        int (int16 (uint16 (position.Var &&& 0xFFFFu)))

    /// <summary>The attach type of a position, <c>attach_type()</c>, the third byte of <c>var</c>.</summary>
    /// <param name="position">The position.</param>
    /// <returns>The type.</returns>
    let private attachType (position: GlyphPosition) =
        byte ((position.Var >>> 16) &&& 0xFFu)

    /// <summary>The cross-stream offset of a position, <c>y_offset</c> in a horizontal run, else <c>x_offset</c>.</summary>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="position">The position.</param>
    /// <returns>The offset.</returns>
    let private crossOffset (horizontal: bool) (position: GlyphPosition) =
        match horizontal with
        | true -> position.YOffset
        | false -> position.XOffset

    /// <summary>The position of a glyph, with the glyph being rewritten read from its new value rather than the buffer.</summary>
    /// <param name="positions">The buffer positions.</param>
    /// <param name="rewritten">The index of the glyph being rewritten.</param>
    /// <param name="replacement">Its new position.</param>
    /// <param name="index">The glyph.</param>
    /// <returns>The position.</returns>
    let private positionAt (positions: ReadOnlySpan<GlyphPosition>) (rewritten: int) (replacement: GlyphPosition) (index: int) =
        match index = rewritten with
        | true -> replacement
        | false -> positions[index]

    /// <summary>Walks the cursive chain of a glyph, adding each parent's cross-stream offset, <c>resolve_cross_offset</c>
    /// (src/OT/Layout/GPOS/MarkArray.hh:12-40). The walk starts with a count of one, the marked glyph and its own
    /// cross-stream offset, and is bounded by the glyph count.</summary>
    /// <param name="positions">The buffer positions.</param>
    /// <param name="rewritten">The index of the glyph being rewritten.</param>
    /// <param name="replacement">Its new position.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the buffer is reversed.</param>
    /// <param name="count">The links walked, counting the start.</param>
    /// <param name="glyphPos">The glyph the walk is at.</param>
    /// <param name="offset">The offset so far.</param>
    /// <returns>The offset.</returns>
    [<TailCall>]
    let rec resolveCrossOffset (positions: ReadOnlySpan<GlyphPosition>) (rewritten: int) (replacement: GlyphPosition) (horizontal: bool) (reversed: bool) (count: int) (glyphPos: int) (offset: int) =
        let here = positionAt positions rewritten replacement glyphPos
        match count < positions.Length && attachType here &&& AttachTypeCursive <> 0uy with
        | false -> offset
        | true ->
            let chain = attachChain here
            let parent = glyphPos + (match reversed with | true -> -chain | false -> chain)
            match chain = 0 || parent < 0 || parent >= positions.Length with
            | true -> offset
            | false ->
                let next = saturateAdd offset (crossOffset horizontal (positionAt positions rewritten replacement parent))
                resolveCrossOffset positions rewritten replacement horizontal reversed (count + 1) parent next

    /// <summary>The offset of an action's data, <c>ankrData[index * n]</c>.</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="size">The byte width of one action.</param>
    /// <returns>The byte offset.</returns>
    let private actionAt (spec: Format4Spec) (entry: Entry) (size: int) =
        spec.ActionsAt + int64 entry.Data0 * int64 size

    /// <summary>The position a moved glyph takes when it attaches to the marked glyph: the cross-stream offset resolved from the
    /// mark, the mark attach type and the chain to the mark.</summary>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the buffer is reversed.</param>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="index">The current glyph.</param>
    /// <param name="mark">The marked glyph.</param>
    /// <param name="moved">The glyph's position with the action's offsets.</param>
    /// <returns>The attached position.</returns>
    let attached (horizontal: bool) (reversed: bool) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (mark: int) (moved: GlyphPosition) =
        let markOffset = crossOffset horizontal (positionAt positions index moved mark)
        let baseOffset = resolveCrossOffset positions index moved horizontal reversed 1 mark markOffset
        let offsets =
            match horizontal with
            | true -> { moved with YOffset = saturateAdd moved.YOffset baseOffset }
            | false -> { moved with XOffset = saturateAdd moved.XOffset baseOffset }
        let chain = mark - index
        let signed = match reversed with | true -> -chain | false -> chain
        let var = (offsets.Var &&& AttachKeepMask) ||| AttachTypeMark ||| uint32 (uint16 signed)
        { offsets with Var = var }

    /// <summary>The attachment the action produced, then the position and scratch flag edits.</summary>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the buffer is reversed.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The context.</param>
    /// <param name="moved">The glyph's position with the action's offsets.</param>
    /// <returns>The first edit and the context.</returns>
    let private attach (horizontal: bool) (reversed: bool) (cursor: inref<RunCursor>) (context: KerxFormat4Context) (moved: GlyphPosition) =
        let position = attached horizontal reversed cursor.Positions cursor.Index context.Mark moved
        struct (Emission.Emit (RunEdit.SetPosition (cursor.Index, position)), { context with Phase = Flagging })

    /// <summary>The outcome of an action that does nothing and leaves the mark alone.</summary>
    [<Literal>]
    let ResolveNothing = 0

    /// <summary>The outcome of an action that only charges the budget.</summary>
    [<Literal>]
    let ResolveCharge = 1

    /// <summary>The outcome of an action that moves the glyph and attaches it.</summary>
    [<Literal>]
    let ResolveAttach = 2

    /// <summary>The operations the control point action charges, two outline lookups.</summary>
    let ChargeAmount = ControlPointCharge

    /// <summary>What an entry's action does at the current glyph, src/hb-aat-layout-kerx-table.hh:596-692: nothing when the action
    /// lies outside the subtable, a charge for the control point action (the font has no contour point function), else the
    /// glyph's position with the action's offsets: the anchor points subtracted, the coordinates subtracted, or none.</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="ankr">The ankr table.</param>
    /// <param name="font">The font.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="input">The glyphs of the run.</param>
    /// <param name="positions">The positions of the run.</param>
    /// <param name="mark">The marked glyph.</param>
    /// <param name="index">The current glyph.</param>
    /// <returns>The outcome and the moved position.</returns>
    let resolve (spec: Format4Spec) (ankr: Ankr) (font: Font) (entry: Entry) (input: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) (mark: int) (index: int) =
        match spec.ActionType with
        | ControlPointAction ->
            match Sanitize.fits spec.RangeEnd (actionAt spec entry PairActionSize) PairActionSize with
            | false -> struct (ResolveNothing, Unchecked.defaultof<GlyphPosition>)
            | true -> struct (ResolveCharge, Unchecked.defaultof<GlyphPosition>)
        | AnchorPointAction ->
            let at = actionAt spec entry PairActionSize
            match Sanitize.fits spec.RangeEnd at PairActionSize with
            | false -> struct (ResolveNothing, Unchecked.defaultof<GlyphPosition>)
            | true ->
                let span = spec.TableBytes.Span
                let numGlyphs = font.Ot.Face.GlyphCount
                let struct (markX, markY) = Ankr.anchor ankr input[mark].Codepoint (uint32 (Binary.u16 span (int at))) numGlyphs
                let struct (currX, currY) = Ankr.anchor ankr input[index].Codepoint (uint32 (Binary.u16 span (int at + 2))) numGlyphs
                let current = positions[index]
                struct (ResolveAttach,
                        { current with
                            XOffset = saturateSub (Font.emScaleX font markX) (Font.emScaleX font currX)
                            YOffset = saturateSub (Font.emScaleY font markY) (Font.emScaleY font currY) })
        | CoordinateAction ->
            let at = actionAt spec entry CoordinateActionSize
            match Sanitize.fits spec.RangeEnd at CoordinateActionSize with
            | false -> struct (ResolveNothing, Unchecked.defaultof<GlyphPosition>)
            | true ->
                let span = spec.TableBytes.Span
                let current = positions[index]
                struct (ResolveAttach,
                        { current with
                            XOffset = saturateSub (Font.emScaleX font (int (Binary.i16 span (int at)))) (Font.emScaleX font (int (Binary.i16 span (int at + 4))))
                            YOffset = saturateSub (Font.emScaleY font (int (Binary.i16 span (int at + 2)))) (Font.emScaleY font (int (Binary.i16 span (int at + 6)))) })
        | _ -> struct (ResolveAttach, positions[index])

    /// <summary>The start of a transition: the action when a mark is set and the entry has an action, else the mark update
    /// alone (src/hb-aat-layout-kerx-table.hh:596-692).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="ankr">The ankr table.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the buffer is reversed.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The context.</param>
    /// <returns>The edit and the context.</returns>
    let private freshStep (spec: Format4Spec) (ankr: Ankr) (font: Font) (horizontal: bool) (reversed: bool) (cursor: inref<RunCursor>) (entry: Entry) (context: KerxFormat4Context) =
        match context.MarkSet && performAction entry && cursor.Index < cursor.Input.Length with
        | false -> struct (Emission.Complete, remember entry cursor.Index context)
        | true ->
            let struct (outcome, moved) = resolve spec ankr font entry cursor.Input cursor.Positions context.Mark cursor.Index
            match outcome with
            | ResolveNothing -> struct (Emission.Complete, { context with Phase = Fresh })
            | ResolveCharge -> struct (Emission.Emit (RunEdit.ChargeOps ControlPointCharge), { context with Phase = Charged })
            | _ -> attach horizontal reversed &cursor context moved

    /// <summary>One step of the format 4 transition micro-program (src/hb-aat-layout-kerx-table.hh:596-701).</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="ankr">The ankr table.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the buffer is reversed.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The context.</param>
    /// <returns>The edit and the context.</returns>
    let transition (spec: Format4Spec) (ankr: Ankr) (font: Font) (horizontal: bool) (reversed: bool) (cursor: inref<RunCursor>) (entry: Entry) (context: KerxFormat4Context) =
        match context.Phase with
        | Fresh -> freshStep spec ankr font horizontal reversed &cursor entry context
        | Flagging -> struct (Emission.Emit (RunEdit.AddScratch ScratchFlags.HasGposAttachment), { context with Phase = Closing })
        | Closing -> struct (Emission.Complete, remember entry cursor.Index context)
        | Charged -> struct (Emission.Complete, { context with Phase = Fresh })

/// <summary>The kerx format 4 subtable kind (src/hb-aat-layout-kerx-table.hh:547-748).</summary>
[<Struct; NoComparison; NoEquality>]
type internal KerxFormat4Kind =
    { Spec: Format4Spec
      Ankr: Ankr
      Font: Font
      Horizontal: bool
      Reversed: bool }

    interface IStateMachineKind<KerxFormat4Context> with
        member _.InPlace = true
        member _.IsActionable entry = KerxFormat4Machine.performAction entry
        member _.IsActionInitiable entry = entry.Flags &&& KerxFormat4Machine.MarkFlag <> 0us
        member this.Transition (cursor, entry, context) =
            KerxFormat4Machine.transition this.Spec this.Ankr this.Font this.Horizontal this.Reversed &cursor entry context
        member _.NullTransition (cursor, entry, context) =
            KerxFormat4Machine.remember entry cursor.Index context

/// <summary>The decisions of a kerx format 4 subtable as <see cref="IFormat4Decisions{E}"/>: the action of an entry and the
/// attachment of the moved glyph, for the kernel that runs the transition directly.</summary>
[<Struct; NoComparison; NoEquality; System.Runtime.CompilerServices.IsReadOnly>]
type internal KerxFormat4Decisions =
    { Kind: KerxFormat4Kind }

    interface IFormat4Decisions<Entry> with
        member _.Performs entry = KerxFormat4Machine.performAction entry
        member _.SetsMark entry = entry.Flags &&& KerxFormat4Machine.MarkFlag <> 0us
        member this.Resolve (entry, input, positions, mark, index) =
            KerxFormat4Machine.resolve this.Kind.Spec this.Kind.Ankr this.Kind.Font entry input positions mark index
        member _.ChargeAmount = KerxFormat4Machine.ChargeAmount
        member this.Attach (positions, index, mark, moved) =
            KerxFormat4Machine.attached this.Kind.Horizontal this.Kind.Reversed positions index mark moved

/// <summary>Applying the kerx format 4 subtable, <c>KerxSubTableFormat4::apply</c>.</summary>
[<RequireQualifiedAccess>]
module internal KerxFormat4Apply =
    /// <summary>Runs one format 4 subtable's state machine, <c>KerxSubTableFormat4::apply</c>, with the implementation chosen.</summary>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="dense">The compiled state table, or none.</param>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="machine">The decoded state machine.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the buffer is reversed.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let applyWith (runner: MachineRunner) (dense: DenseMachine voption) (spec: Format4Spec) (machine: StateMachine) (font: Font) (horizontal: bool) (reversed: bool) (run: GlyphRun) =
        let kind =
            { Spec = spec
              Ankr = font.Ot.Ankr.Value
              Font = font
              Horizontal = horizontal
              Reversed = reversed }
        let config =
            let endEntry = StateTable.entry machine StateTable.StartOfText StateTable.ClassEndOfText
            { Machine = machine
              Ranges = ImmutableArray.Empty
              SubtableFlags = 0u
              NumGlyphs = font.Ot.Face.GlyphCount
              StartSafeToBreakAtEndOfText = not (KerxFormat4Machine.performAction endEntry) }
        match runner, dense with
        | Kernel, ValueSome table ->
            let tables = { DenseKernelMachine.Config = config; Kind = kind; Dense = table; QuietNull = true; NullMask = KerxFormat4Machine.MarkFlag }
            AatKernel.resultOf run (Sealed.runFormat4Pass (AatKernel.inputOf run) tables { KerxFormat4Decisions.Kind = kind })
        | Kernel, ValueNone ->
            let tables = { KernelMachine.Config = config; Kind = kind; QuietNull = true; NullMask = KerxFormat4Machine.MarkFlag }
            AatKernel.resultOf run (Sealed.runFormat4Pass (AatKernel.inputOf run) tables { KerxFormat4Decisions.Kind = kind })
        | Reference, _ ->
            let struct (applied, _) = AatKernel.run Reference ValueNone config kind KerxFormat4Machine.MarkFlag true (KerxFormat4Machine.seed ()) run
            applied

    /// <summary>Runs one format 4 subtable's state machine, <c>KerxSubTableFormat4::apply</c>.</summary>
    /// <param name="spec">The subtable spec.</param>
    /// <param name="machine">The decoded state machine.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="reversed">Whether the buffer is reversed.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let apply (spec: Format4Spec) (machine: StateMachine) (font: Font) (horizontal: bool) (reversed: bool) (run: GlyphRun) =
        applyWith Kernel ValueNone spec machine font horizontal reversed run
