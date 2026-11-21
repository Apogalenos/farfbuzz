namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>Where the ligature transition is within its action micro-program (src/hb-aat-layout-morx-table.hh:448-638).</summary>
[<Struct; NoComparison; NoEquality>]
type internal LigaturePhase =
    | Idle
    | Read
    | StoreGlyph
    | DeleteCheck
    | DeleteProps
    | DeleteGlyph
    | Merge
    | Advance
    | Done

/// <summary>The ligature transition context: the component ring, the action micro-program and the written glyphs.</summary>
[<Struct; NoComparison; NoEquality>]
type internal LigatureContext =
    { MatchLength: int
      Ring: ImmutableArray<uint32>
      Cursor: int
      LigatureIndex: uint32
      ActionAt: int64
      End: int
      Action: uint32
      LigatureGlyph: uint32
      LigEnd: int
      CollectWritten: bool
      Written: PointBuilder<uint32>
      HasDeleted: bool
      Phase: LigaturePhase }

/// <summary>Reading and applying the ligature morx/mort subtable (src/hb-aat-layout-morx-table.hh:385-638).</summary>
[<RequireQualifiedAccess>]
module internal MorxLigature =
    /// <summary>The <c>SetComponent</c> entry bit, which pushes the current glyph.</summary>
    [<Literal>]
    let SetComponentFlag = 0x8000us

    /// <summary>The <c>PerformAction</c> entry bit (morx), which runs the ligature action list.</summary>
    [<Literal>]
    let PerformActionFlag = 0x2000us

    /// <summary>The mort action byte-offset mask, <c>LigatureEntry&lt;false&gt;::Offset</c>.</summary>
    [<Literal>]
    let OffsetMask = 0x3FFFus

    /// <summary>The <c>LigActionLast</c> bit, the last action of a group.</summary>
    [<Literal>]
    let private LigActionLast = 0x80000000u

    /// <summary>The <c>LigActionStore</c> bit, which stores the ligature.</summary>
    [<Literal>]
    let private LigActionStore = 0x40000000u

    /// <summary>The 30-bit <c>LigActionOffset</c> mask.</summary>
    [<Literal>]
    let private LigActionOffsetMask = 0x3FFFFFFFu

    /// <summary>The sign bit of the 30-bit offset, before sign extension.</summary>
    [<Literal>]
    let private SignExtendBit = 0x20000000u

    /// <summary>The sign-extension bits ORed in for a negative offset.</summary>
    [<Literal>]
    let private SignExtendMask = 0xC0000000u

    /// <summary>The ring length, <c>HB_MAX_CONTEXT_LENGTH</c>.</summary>
    [<Literal>]
    let RingSize = 64

    /// <summary>The word index of an obsolete offset, <c>ObsoleteTypes::offsetToIndex</c>
    /// (src/hb-aat-layout-common.hh:1204-1223). An offset below the array, or an index past the bad index, returns the
    /// bad index so the later bounds check fails.</summary>
    /// <param name="offset">The byte offset.</param>
    /// <param name="arrayAt">The byte offset of the array within the subtable.</param>
    /// <param name="unitSize">The size of one array element.</param>
    /// <returns>The word index.</returns>
    let private obsoleteIndex (offset: int64) (arrayAt: int64) (unitSize: int64) =
        let badIndex = (int64 Int32.MaxValue) / unitSize
        match offset < arrayAt with
        | true -> badIndex
        | false ->
            let index = (offset - arrayAt) / unitSize
            match index > badIndex with
            | true -> badIndex
            | false -> index

    /// <summary>The byte offset of an entry's ligature action word.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="ligActionAt">The byte offset of the ligAction array.</param>
    /// <param name="entry">The entry.</param>
    /// <returns>The action word byte offset.</returns>
    let internal actionAtOf (extended: bool) (ligActionAt: int64) (entry: Entry) =
        match extended with
        | true -> ligActionAt + int64 entry.Data0 * 4L
        | false -> ligActionAt + obsoleteIndex (int64 (entry.Flags &&& OffsetMask)) ligActionAt 4L * 4L

    /// <summary>The byte offset of a component word, <c>wordOffsetToIndex</c>.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="componentAt">The byte offset of the component array.</param>
    /// <param name="componentIdx">The component index.</param>
    /// <returns>The component word byte offset.</returns>
    let internal componentAtOf (extended: bool) (componentAt: int64) (componentIdx: uint32) =
        match extended with
        | true -> componentAt + int64 componentIdx * 2L
        | false -> componentAt + obsoleteIndex (2L * int64 componentIdx) componentAt 2L * 2L

    /// <summary>The byte offset of a ligature word, <c>offsetToIndex</c>.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="ligatureAt">The byte offset of the ligature array.</param>
    /// <param name="ligatureIdx">The ligature index.</param>
    /// <returns>The ligature word byte offset.</returns>
    let internal ligatureAtOf (extended: bool) (ligatureAt: int64) (ligatureIdx: uint32) =
        match extended with
        | true -> ligatureAt + int64 ligatureIdx * 2L
        | false -> ligatureAt + obsoleteIndex (int64 ligatureIdx) ligatureAt 2L * 2L

    /// <summary>Reads a big-endian uint32 at a byte offset, none when it lies outside the body.</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="at">The byte offset.</param>
    /// <returns>The value.</returns>
    let internal readU32 (body: ReadOnlyMemory<byte>) (at: int64) =
        match at >= 0L && at + 4L <= int64 body.Length with
        | false -> ValueNone
        | true -> ValueSome (Binary.u32 body.Span (int at))

    /// <summary>Reads a big-endian uint16 at a byte offset, none when it lies outside the body.</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="at">The byte offset.</param>
    /// <returns>The value.</returns>
    let internal readU16 (body: ReadOnlyMemory<byte>) (at: int64) =
        match at >= 0L && at + 2L <= int64 body.Length with
        | false -> ValueNone
        | true -> ValueSome (uint32 (Binary.u16 body.Span (int at)))

    /// <summary>Whether an entry performs an action, <c>LigatureEntryT::performAction</c>.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry runs the action list.</returns>
    let performAction (extended: bool) (entry: Entry) =
        match extended with
        | true -> entry.Flags &&& PerformActionFlag <> 0us
        | false -> entry.Flags &&& OffsetMask <> 0us

    /// <summary>Sign-extends the 30-bit action offset to 32 bits (src/hb-aat-layout-morx-table.hh:547-550).</summary>
    /// <param name="action">The action word.</param>
    /// <returns>The sign-extended offset.</returns>
    let internal signExtendOffset (action: uint32) =
        let offset = action &&& LigActionOffsetMask
        match offset &&& SignExtendBit <> 0u with
        | true -> offset ||| SignExtendMask
        | false -> offset

    /// <summary>The initial ligature context.</summary>
    /// <param name="collectWritten">Whether the written glyphs feed the growing glyph set.</param>
    /// <returns>The context.</returns>
    let seed (collectWritten: bool) =
        { MatchLength = 0
          Ring = Sealed.init RingSize (fun _ -> 0u)
          Cursor = 0
          LigatureIndex = 0u
          ActionAt = 0L
          End = 0
          Action = 0u
          LigatureGlyph = 0u
          LigEnd = 0
          CollectWritten = collectWritten
          Written = PointBuilder.create ()
          HasDeleted = false
          Phase = Idle }

    /// <summary>Applies the <c>SetComponent</c> push: the duplicate guard, then pushing the output length
    /// (src/hb-aat-layout-morx-table.hh:501-508). The output length equals the cursor index at every fetch, including
    /// the last glyph of a batched null-transition stretch.</summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The context with the component pushed.</returns>
    let pushComponent (cursor: inref<RunCursor>) (context: LigatureContext) =
        let outLen = cursor.Index
        let matchLength =
            match context.MatchLength > 0 && context.Ring[(context.MatchLength - 1) % RingSize] = uint32 outLen with
            | true -> context.MatchLength - 1
            | false -> context.MatchLength
        let ring = Sealed.setItem (matchLength % RingSize) (uint32 outLen) context.Ring
        { context with MatchLength = matchLength + 1; Ring = ring }

    /// <summary>Emits the pop move to the previous component (src/hb-aat-layout-morx-table.hh:540).</summary>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private popEmit (context: LigatureContext) =
        let cursor = context.Cursor - 1
        struct (Emission.Emit (RunEdit.MoveTo (int context.Ring[cursor % RingSize])), { context with Cursor = cursor; Phase = Read })

    /// <summary>Emits the closing move to the recorded end, then completes (src/hb-aat-layout-morx-table.hh:588).</summary>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private moveEndEmit (context: LigatureContext) =
        struct (Emission.Emit (RunEdit.MoveTo context.End), { context with Phase = Done })

    /// <summary>Emits the next pop, or clears the match and closes when the stack underflows
    /// (src/hb-aat-layout-morx-table.hh:529-538).</summary>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private popOrUnderflow (context: LigatureContext) =
        match context.Cursor = 0 with
        | true -> moveEndEmit { context with MatchLength = 0 }
        | false -> popEmit context

    /// <summary>Advances to the next action and emits the next pop or the closing move
    /// (src/hb-aat-layout-morx-table.hh:585-588).</summary>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private advanceEmit (context: LigatureContext) =
        let advanced = { context with ActionAt = context.ActionAt + 4L }
        match context.Action &&& LigActionLast <> 0u with
        | true -> moveEndEmit advanced
        | false -> popOrUnderflow advanced

    /// <summary>Reads the ligature glyph and emits the store's property edit (src/hb-aat-layout-morx-table.hh:560-570).</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="ligatureAt">The byte offset of the ligature array.</param>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="action">The action word.</param>
    /// <param name="ligatureIdx">The ligature index.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private ligatureStep (extended: bool) (body: ReadOnlyMemory<byte>) (ligatureAt: int64) (applyContext: AatContext)
        (cursor: inref<RunCursor>) (action: uint32) (ligatureIdx: uint32) (context: LigatureContext) =
        match readU16 body (ligatureAtOf extended ligatureAt ligatureIdx) with
        | ValueNone -> moveEndEmit { context with Action = action; LigatureIndex = ligatureIdx }
        | ValueSome lig ->
            let lig = uint32 lig
            let props = AatApply.propsFor applyContext lig (cursor.InputProps[cursor.Index])
            let ligEnd = int context.Ring[(context.MatchLength - 1) % RingSize] + 1
            struct (Emission.Emit (RunEdit.SetCurrentProps props), { context with Action = action; LigatureIndex = ligatureIdx; LigatureGlyph = lig; LigEnd = ligEnd; Phase = StoreGlyph })

    /// <summary>Reads the component and dispatches to the ligature store or the next action
    /// (src/hb-aat-layout-morx-table.hh:547-558).</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="componentAt">The byte offset of the component array.</param>
    /// <param name="ligatureAt">The byte offset of the ligature array.</param>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="action">The action word.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private componentStep (extended: bool) (body: ReadOnlyMemory<byte>) (componentAt: int64) (ligatureAt: int64)
        (applyContext: AatContext) (cursor: inref<RunCursor>) (action: uint32) (context: LigatureContext) =
        let componentIdx = cursor.Input[cursor.Index].Codepoint + signExtendOffset action
        match readU16 body (componentAtOf extended componentAt componentIdx) with
        | ValueNone -> moveEndEmit { context with Action = action }
        | ValueSome componentData ->
            let ligatureIdx = context.LigatureIndex + uint32 componentData
            match action &&& (LigActionStore ||| LigActionLast) <> 0u with
            | false -> advanceEmit { context with Action = action; LigatureIndex = ligatureIdx }
            | true -> ligatureStep extended body ligatureAt applyContext &cursor action ligatureIdx context

    /// <summary>Reads the action word and starts one action iteration (src/hb-aat-layout-morx-table.hh:542-546).</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="componentAt">The byte offset of the component array.</param>
    /// <param name="ligatureAt">The byte offset of the ligature array.</param>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let private actionStep (extended: bool) (body: ReadOnlyMemory<byte>) (componentAt: int64) (ligatureAt: int64)
        (applyContext: AatContext) (cursor: inref<RunCursor>) (context: LigatureContext) =
        match readU32 body context.ActionAt with
        | ValueNone -> moveEndEmit context
        | ValueSome action -> componentStep extended body componentAt ligatureAt applyContext &cursor action context

    /// <summary>One step of the ligature transition micro-program (src/hb-aat-layout-morx-table.hh:495-589).</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="ligActionAt">The byte offset of the ligAction array.</param>
    /// <param name="componentAt">The byte offset of the component array.</param>
    /// <param name="ligatureAt">The byte offset of the ligature array.</param>
    /// <param name="applyContext">The apply context.</param>
    /// <param name="cursor">The cursor.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="context">The transition context.</param>
    /// <returns>The edit and the context.</returns>
    let transition (extended: bool) (body: ReadOnlyMemory<byte>) (ligActionAt: int64) (componentAt: int64) (ligatureAt: int64)
        (applyContext: AatContext) (cursor: inref<RunCursor>) (entry: Entry) (context: LigatureContext) =
        match cursor.Successful with
        | false -> struct (Emission.Complete, context)
        | true ->
            match context.Phase with
            | Idle ->
                let next =
                    match entry.Flags &&& SetComponentFlag <> 0us with
                    | true -> pushComponent &cursor context
                    | false -> context
                match performAction extended entry && next.MatchLength > 0 && cursor.Index < cursor.Input.Length with
                | false -> struct (Emission.Complete, next)
                | true ->
                    popEmit
                        { next with
                            Cursor = next.MatchLength
                            LigatureIndex = 0u
                            ActionAt = actionAtOf extended ligActionAt entry
                            End = cursor.Output.Length }
            | Read -> actionStep extended body componentAt ligatureAt applyContext &cursor context
            | StoreGlyph ->
                let written = if context.CollectWritten then PointBuilder.append context.Written context.LigatureGlyph else context.Written
                let deleted = context.HasDeleted || context.LigatureGlyph = AatLookup.DeletedGlyph
                struct (Emission.Emit (RunEdit.ReplaceGlyph context.LigatureGlyph), { context with Written = written; HasDeleted = deleted; Phase = DeleteCheck })
            | DeleteCheck ->
                match context.MatchLength - 1 > context.Cursor with
                | true ->
                    let matchLength = context.MatchLength - 1
                    struct (Emission.Emit (RunEdit.MoveTo (int context.Ring[matchLength % RingSize])), { context with MatchLength = matchLength; Phase = DeleteProps })
                | false ->
                    struct (Emission.Emit (RunEdit.MoveTo context.LigEnd), { context with Phase = Merge })
            | DeleteProps ->
                let props = AatApply.propsFor applyContext AatLookup.DeletedGlyph (cursor.InputProps[cursor.Index])
                struct (Emission.Emit (RunEdit.SetCurrentProps props), { context with Phase = DeleteGlyph })
            | DeleteGlyph ->
                let written = if context.CollectWritten then PointBuilder.append context.Written AatLookup.DeletedGlyph else context.Written
                struct (Emission.Emit (RunEdit.ReplaceGlyph AatLookup.DeletedGlyph), { context with Written = written; HasDeleted = true; Phase = DeleteCheck })
            | Merge ->
                struct (Emission.Emit (RunEdit.MergeOutClusters (int context.Ring[context.Cursor % RingSize], cursor.Output.Length)), { context with Phase = Advance })
            | Advance -> advanceEmit context
            | Done -> struct (Emission.Complete, { context with Phase = Idle })

/// <summary>The reads of a ligature subtable that the direct action loop shares with its micro-program.</summary>
[<RequireQualifiedAccess>]
module internal MorxLigatureReads =
    /// <summary>The byte offset of an entry's first action word.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="ligActionAt">The byte offset of the ligAction array.</param>
    /// <param name="entry">The entry.</param>
    /// <returns>The offset.</returns>
    let actionStart (extended: bool) (ligActionAt: int64) (entry: Entry) = MorxLigature.actionAtOf extended ligActionAt entry

    /// <summary>Reads an action word.</summary>
    /// <param name="body">The subtable body.</param>
    /// <param name="at">The byte offset.</param>
    /// <returns>Whether the word lies within the body, and the word.</returns>
    let action (body: ReadOnlyMemory<byte>) (at: int64) =
        match MorxLigature.readU32 body at with
        | ValueSome word -> struct (true, word)
        | ValueNone -> struct (false, 0u)

    /// <summary>Reads the component word of a glyph for an action.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="componentAt">The byte offset of the component array.</param>
    /// <param name="glyph">The glyph at the cursor.</param>
    /// <param name="action">The action word.</param>
    /// <returns>Whether the word lies within the body, and the word.</returns>
    let componentWord (extended: bool) (body: ReadOnlyMemory<byte>) (componentAt: int64) (glyph: uint32) (action: uint32) =
        match MorxLigature.readU16 body (MorxLigature.componentAtOf extended componentAt (glyph + MorxLigature.signExtendOffset action)) with
        | ValueSome word -> struct (true, word)
        | ValueNone -> struct (false, 0u)

    /// <summary>Reads the ligature glyph at an index.</summary>
    /// <param name="extended">True for morx, false for mort.</param>
    /// <param name="body">The subtable body.</param>
    /// <param name="ligatureAt">The byte offset of the ligature array.</param>
    /// <param name="index">The ligature index.</param>
    /// <returns>Whether the glyph lies within the body, and the glyph.</returns>
    let ligature (extended: bool) (body: ReadOnlyMemory<byte>) (ligatureAt: int64) (index: uint32) =
        match MorxLigature.readU16 body (MorxLigature.ligatureAtOf extended ligatureAt index) with
        | ValueSome word -> struct (true, word)
        | ValueNone -> struct (false, 0u)

/// <summary>The ligature morx/mort subtable kind (src/hb-aat-layout-morx-table.hh:448-638).</summary>
[<Struct; NoComparison; NoEquality>]
type internal LigatureKind =
    { Extended: bool
      Body: ReadOnlyMemory<byte>
      LigActionAt: int64
      ComponentAt: int64
      LigatureAt: int64
      Context: AatContext }

    interface IStateMachineKind<LigatureContext> with
        member _.InPlace = false
        member this.IsActionable entry = MorxLigature.performAction this.Extended entry
        member _.IsActionInitiable entry = entry.Flags &&& MorxLigature.SetComponentFlag <> 0us
        member this.Transition (cursor, entry, context) =
            MorxLigature.transition this.Extended this.Body this.LigActionAt this.ComponentAt this.LigatureAt this.Context &cursor entry context
        member _.NullTransition (cursor, entry, context) =
            match entry.Flags &&& MorxLigature.SetComponentFlag <> 0us with
            | true -> MorxLigature.pushComponent &cursor context
            | false -> context
