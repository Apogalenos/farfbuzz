namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>One decoded state-table entry; <c>NewState</c> is a state number (mort/kern offsets already converted).</summary>
[<Struct; NoComparison; NoEquality>]
type internal Entry =
    { NewState: int
      Flags: uint16
      Data0: uint16
      Data1: uint16 }

/// <summary>The class reader of a state table: an obsolete narrow/wide class table or the extended lookup.</summary>
[<Struct>]
type internal ClassReader =
    | Narrow of narrow: ClassTable
    | Wide of wide: ClassTable
    | Extended of lookup: Lookup

/// <summary>The two action predicates a subtable kind supplies for initial-glyph collection.</summary>
type internal IEntryClassifier =
    /// <summary><c>is_actionable</c>.</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry performs an action.</returns>
    abstract IsActionable : entry: Entry -> bool

    /// <summary><c>is_action_initiable</c> (initial glyph sets only).</summary>
    /// <param name="entry">The entry.</param>
    /// <returns>True when the entry begins an action.</returns>
    abstract IsActionInitiable : entry: Entry -> bool

/// <summary>A decoded AAT state table, from the minimum to the maximum reachable state.</summary>
[<NoComparison; NoEquality>]
type internal StateMachine =
    { ClassCount: int
      Classes: ClassReader
      States: ImmutableArray<uint16>
      StateBias: int
      RowCount: int
      Entries: ImmutableArray<Entry>
      IsExtended: bool }

/// <summary>Reading AAT state tables, obsolete (16-bit) and extended (32-bit).</summary>
[<RequireQualifiedAccess>]
module internal StateTable =
    /// <summary>The reserved class of the end of text, <c>CLASS_END_OF_TEXT</c>.</summary>
    [<Literal>]
    let ClassEndOfText = 0us

    /// <summary>The reserved class of an out-of-bounds glyph, <c>CLASS_OUT_OF_BOUNDS</c>.</summary>
    [<Literal>]
    let ClassOutOfBounds = 1us

    /// <summary>The reserved class of a deleted glyph, <c>CLASS_DELETED_GLYPH</c>.</summary>
    [<Literal>]
    let ClassDeletedGlyph = 2us

    /// <summary>The reserved class of the end of line, <c>CLASS_END_OF_LINE</c>.</summary>
    [<Literal>]
    let ClassEndOfLine = 3us

    /// <summary>The glyph id marking a deleted glyph, <c>DELETED_GLYPH</c>.</summary>
    [<Literal>]
    let DeletedGlyph = 0xFFFFu

    /// <summary>The start-of-text state, <c>STATE_START_OF_TEXT</c>.</summary>
    [<Literal>]
    let StartOfText = 0

    /// <summary>The bits of one bit page, <c>hb_bit_page_t::BITS</c>.</summary>
    [<Literal>]
    let BitsPerPage = 512

    /// <summary>Reads one state-array value, an entry index, narrow or wide.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <param name="extended">True for the wide (16-bit) state array.</param>
    /// <returns>The entry index.</returns>
    let private stateValueOf (table: ReadOnlySpan<byte>) (offset: int) (extended: bool) =
        match extended with
        | true -> Binary.u16 table offset
        | false -> uint16 (Binary.u8 table offset)

    /// <summary>Converts an entry's raw <c>newState</c> to a state number, <c>StateTable::new_state</c>.</summary>
    /// <param name="extended">True when the value is already a state index.</param>
    /// <param name="stateArrayOffset">The state-array offset, for the obsolete byte-offset form.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="rawNewState">The raw entry value.</param>
    /// <returns>The state number.</returns>
    let inline private newStateOf (extended: bool) (stateArrayOffset: int) (nClasses: int) (rawNewState: int) =
        match extended with
        | true -> rawNewState
        | false -> (rawNewState - stateArrayOffset) / nClasses

    /// <summary>Where the sanitize sweep is within one loop iteration.</summary>
    [<Struct>]
    type private SweepPhase =
        | Negative
        | Positive
        | Entries

    /// <summary>The sweep state: the reachable state bounds, the swept prefixes and the budget.</summary>
    [<Struct>]
    type private SweepState =
        { MinState: int
          MaxState: int
          StatePos: int
          StateNeg: int
          Entry: int
          NumEntries: int
          Ops: int
          Phase: SweepPhase }

    /// <summary>The sweep outcome: the reachable bounds, the entry count and the budget left.</summary>
    [<Struct>]
    type private SweepResult =
        { MinState: int
          MaxState: int
          NumEntries: int
          Ops: int }

    /// <summary>Sweeps the negative state rows, taking the largest entry index plus one.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="statesAt">The byte offset of the state-0 row.</param>
    /// <param name="stateUnitSize">The width of one state-array element.</param>
    /// <param name="extended">True for the wide state array.</param>
    /// <param name="remaining">The elements still to sweep.</param>
    /// <param name="numEntries">The entry count so far.</param>
    /// <returns>The entry count.</returns>
    [<TailCall>]
    let rec private sweepNegStates
        (table: ReadOnlySpan<byte>) (statesAt: int) (stateUnitSize: int) (extended: bool) (remaining: int) (numEntries: int) =
        match remaining <= 0 with
        | true -> numEntries
        | false ->
            let value = int (stateValueOf table (statesAt - remaining * stateUnitSize) extended)
            sweepNegStates table statesAt stateUnitSize extended (remaining - 1) (max numEntries (value + 1))

    /// <summary>Sweeps the positive state rows, taking the largest entry index plus one.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="statesAt">The byte offset of the state-0 row.</param>
    /// <param name="stateUnitSize">The width of one state-array element.</param>
    /// <param name="extended">True for the wide state array.</param>
    /// <param name="index">The next element.</param>
    /// <param name="stop">One past the last element.</param>
    /// <param name="numEntries">The entry count so far.</param>
    /// <returns>The entry count.</returns>
    [<TailCall>]
    let rec private sweepPosStates
        (table: ReadOnlySpan<byte>) (statesAt: int) (stateUnitSize: int) (extended: bool) (index: int) (stop: int) (numEntries: int) =
        match index >= stop with
        | true -> numEntries
        | false ->
            let value = int (stateValueOf table (statesAt + index * stateUnitSize) extended)
            sweepPosStates table statesAt stateUnitSize extended (index + 1) stop (max numEntries (value + 1))

    /// <summary>Sweeps the new entries, widening the reachable state bounds.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="entriesAt">The byte offset of the first entry.</param>
    /// <param name="entrySize">The width of one entry.</param>
    /// <param name="stateArrayOffset">The state-array offset, for the obsolete byte-offset form.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="index">The next entry.</param>
    /// <param name="numEntries">The entry count.</param>
    /// <param name="minState">The smallest state seen.</param>
    /// <param name="maxState">The largest state seen.</param>
    /// <returns>The state bounds.</returns>
    [<TailCall>]
    let rec private sweepEntries
        (table: ReadOnlySpan<byte>) (entriesAt: int) (entrySize: int) (stateArrayOffset: int) (nClasses: int) (extended: bool)
        (index: int) (numEntries: int) (minState: int) (maxState: int) =
        match index >= numEntries with
        | true -> struct (minState, maxState)
        | false ->
            let newState = newStateOf extended stateArrayOffset nClasses (int (Binary.u16 table (entriesAt + index * entrySize)))
            sweepEntries table entriesAt entrySize stateArrayOffset nClasses extended (index + 1) numEntries (min minState newState) (max maxState newState)

    /// <summary>Processes the negative states of one sweep iteration.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="statesAt">The byte offset of the state-0 row.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="stateUnitSize">The width of one state-array element.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="rowStride">The byte width of one state row.</param>
    /// <param name="state">The sweep state.</param>
    /// <returns>The updated sweep state, or none when the table fails.</returns>
    let private negativeBlock
        (table: ReadOnlySpan<byte>) (statesAt: int64) (nClasses: int) (stateUnitSize: int) (extended: bool) (rowStride: int64) (state: SweepState) =
        let numNegStates = -state.MinState
        let negElements = int64 numNegStates * int64 nClasses
        let negBytes = int64 numNegStates * rowStride
        match negElements > int64 UInt32.MaxValue || negBytes > int64 UInt32.MaxValue with
        | true -> ValueNone
        | false ->
            match Sanitize.range table.Length (statesAt - negBytes) (uint32 negBytes) state.Ops with
            | ValueNone -> ValueNone
            | ValueSome ops ->
                let remaining = ops - (state.StateNeg - state.MinState)
                match remaining > 0 with
                | false -> ValueNone
                | true ->
                    let numEntries = sweepNegStates table (int statesAt) stateUnitSize extended (int negElements) state.NumEntries
                    ValueSome { state with NumEntries = numEntries; StateNeg = state.MinState; Ops = remaining }

    /// <summary>Processes the positive states of one sweep iteration.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="statesAt">The byte offset of the state-0 row.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="stateUnitSize">The width of one state-array element.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="rowStride">The byte width of one state row.</param>
    /// <param name="state">The sweep state.</param>
    /// <returns>The updated sweep state, or none when the table fails.</returns>
    let private positiveBlock
        (table: ReadOnlySpan<byte>) (statesAt: int64) (nClasses: int) (stateUnitSize: int) (extended: bool) (rowStride: int64) (state: SweepState) =
        let count = state.MaxState + 1
        match Sanitize.array table.Length statesAt (uint32 count) (uint32 rowStride) state.Ops with
        | ValueNone -> ValueNone
        | ValueSome ops ->
            let remaining = ops - (state.MaxState - state.StatePos + 1)
            match remaining > 0 with
            | false -> ValueNone
            | true ->
                let elements = int64 count * int64 nClasses
                match elements > int64 UInt32.MaxValue with
                | true -> ValueNone
                | false ->
                    let numEntries = sweepPosStates table (int statesAt) stateUnitSize extended (state.StatePos * nClasses) (int elements) state.NumEntries
                    ValueSome { state with NumEntries = numEntries; StatePos = state.MaxState + 1; Ops = remaining }

    /// <summary>Processes the entries of one sweep iteration.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="entriesAt">The byte offset of the first entry.</param>
    /// <param name="stateArrayOffset">The state-array offset, for the obsolete byte-offset form.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="entrySize">The width of one entry.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="state">The sweep state.</param>
    /// <returns>The updated sweep state, or none when the table fails.</returns>
    let private entriesBlock
        (table: ReadOnlySpan<byte>) (entriesAt: int64) (stateArrayOffset: int) (nClasses: int) (entrySize: int) (extended: bool) (state: SweepState) =
        match Sanitize.array table.Length entriesAt (uint32 state.NumEntries) (uint32 entrySize) state.Ops with
        | ValueNone -> ValueNone
        | ValueSome ops ->
            let remaining = ops - (state.NumEntries - state.Entry)
            match remaining > 0 with
            | false -> ValueNone
            | true ->
                let struct (minState, maxState) = sweepEntries table (int entriesAt) entrySize stateArrayOffset nClasses extended state.Entry state.NumEntries state.MinState state.MaxState
                ValueSome { state with MinState = minState; MaxState = maxState; Entry = state.NumEntries; Ops = remaining }

    /// <summary>Runs the sanitize sweep to its fixpoint, <c>StateTable::sanitize</c>'s loop.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="statesAt">The byte offset of the state-0 row.</param>
    /// <param name="entriesAt">The byte offset of the first entry.</param>
    /// <param name="stateArrayOffset">The state-array offset, for the obsolete byte-offset form.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="stateUnitSize">The width of one state-array element.</param>
    /// <param name="entrySize">The width of one entry.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="rowStride">The byte width of one state row.</param>
    /// <param name="state">The sweep state.</param>
    /// <returns>The sweep outcome, or none when the table fails.</returns>
    [<TailCall>]
    let rec private sweepLoop
        (table: ReadOnlySpan<byte>) (statesAt: int64) (entriesAt: int64) (stateArrayOffset: int) (nClasses: int)
        (stateUnitSize: int) (entrySize: int) (extended: bool) (rowStride: int64) (state: SweepState) : SweepResult voption =
        match state.Phase with
        | Negative ->
            match state.MinState < state.StateNeg with
            | false -> sweepLoop table statesAt entriesAt stateArrayOffset nClasses stateUnitSize entrySize extended rowStride { state with Phase = Positive }
            | true ->
                match negativeBlock table statesAt nClasses stateUnitSize extended rowStride state with
                | ValueNone -> ValueNone
                | ValueSome next -> sweepLoop table statesAt entriesAt stateArrayOffset nClasses stateUnitSize entrySize extended rowStride { next with Phase = Positive }
        | Positive ->
            match state.StatePos <= state.MaxState with
            | false -> sweepLoop table statesAt entriesAt stateArrayOffset nClasses stateUnitSize entrySize extended rowStride { state with Phase = Entries }
            | true ->
                match positiveBlock table statesAt nClasses stateUnitSize extended rowStride state with
                | ValueNone -> ValueNone
                | ValueSome next -> sweepLoop table statesAt entriesAt stateArrayOffset nClasses stateUnitSize entrySize extended rowStride { next with Phase = Entries }
        | Entries ->
            match entriesBlock table entriesAt stateArrayOffset nClasses entrySize extended state with
            | ValueNone -> ValueNone
            | ValueSome next ->
                match next.MinState >= next.StateNeg && next.StatePos > next.MaxState with
                | true -> ValueSome { MinState = next.MinState; MaxState = next.MaxState; NumEntries = next.NumEntries; Ops = next.Ops }
                | false -> sweepLoop table statesAt entriesAt stateArrayOffset nClasses stateUnitSize entrySize extended rowStride { next with Phase = Negative }

    /// <summary>Checks the state-row stride and runs the sweep.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="statesAt">The byte offset of the state-0 row.</param>
    /// <param name="entriesAt">The byte offset of the first entry.</param>
    /// <param name="stateArrayOffset">The state-array offset, for the obsolete byte-offset form.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="stateUnitSize">The width of one state-array element.</param>
    /// <param name="entrySize">The width of one entry.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The sweep outcome, or none when the table fails.</returns>
    let private runSweep
        (table: ReadOnlySpan<byte>) (statesAt: int64) (entriesAt: int64) (stateArrayOffset: int) (nClasses: int)
        (stateUnitSize: int) (entrySize: int) (extended: bool) (ops: int) =
        let rowStride = int64 nClasses * int64 stateUnitSize
        match rowStride > int64 UInt32.MaxValue with
        | true -> ValueNone
        | false ->
            let initial: SweepState =
                { MinState = 0; MaxState = 0; StatePos = 0; StateNeg = 0; Entry = 0; NumEntries = 0; Ops = ops; Phase = Negative }
            sweepLoop table statesAt entriesAt stateArrayOffset nClasses stateUnitSize entrySize extended rowStride initial

    /// <summary>Runs the sweep and reports the remaining budget and entry count.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="at">The state-table offset.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="entryDataWords">The entry data words.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="stateArrayOffset">The state-array offset value.</param>
    /// <param name="entryTableOffset">The entry-table offset value.</param>
    /// <param name="stateArrayOffsetForNewState">The offset used by the obsolete <c>new_state</c>.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget and entry count, or none when the sweep fails.</returns>
    let private sweepAndReport
        (table: ReadOnlySpan<byte>) (at: int) (extended: bool) (entryDataWords: int) (nClasses: int)
        (stateArrayOffset: int64) (entryTableOffset: int64) (stateArrayOffsetForNewState: int) (ops: int) =
        let stateUnitSize = if extended then 2 else 1
        let entrySize = 4 + 2 * entryDataWords
        match runSweep table (int64 at + stateArrayOffset) (int64 at + entryTableOffset) stateArrayOffsetForNewState nClasses stateUnitSize entrySize extended ops with
        | ValueNone -> ValueNone
        | ValueSome result -> ValueSome (struct (result.Ops, result.NumEntries))

    /// <summary>Sanitizes the class table and sweeps the reachable states.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="at">The state-table offset.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="entryDataWords">The entry data words.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="stateArrayOffset">The state-array offset value.</param>
    /// <param name="entryTableOffset">The entry-table offset value.</param>
    /// <param name="classTableAt">The class-table offset.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget and entry count, or none when the table fails.</returns>
    let private finishSanitize
        (table: ReadOnlySpan<byte>) (at: int) (extended: bool) (entryDataWords: int) (numGlyphs: int) (nClasses: int)
        (stateArrayOffset: int64) (entryTableOffset: int64) (classTableAt: int) (ops: int) =
        match extended with
        | true ->
            match AatLookup.sanitize table classTableAt 2 numGlyphs true ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sweepAndReport table at extended entryDataWords nClasses stateArrayOffset entryTableOffset 0 remaining
        | false ->
            match ClassTable.sanitizeSpan table classTableAt (int (Binary.u16 table (classTableAt + 2))) 1 ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sweepAndReport table at extended entryDataWords nClasses stateArrayOffset entryTableOffset (int stateArrayOffset) remaining

    /// <summary>Sanitizes a state table, threading the operation budget exactly as upstream does.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="at">The state-table offset.</param>
    /// <param name="extended">True for the 32-bit form, false for the obsolete 16-bit form.</param>
    /// <param name="entryDataWords">The number of 16-bit data words per entry.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining budget.</param>
    /// <returns>The remaining budget and the entry count, or none when the table fails.</returns>
    let sanitize (table: ReadOnlySpan<byte>) (at: int) (extended: bool) (entryDataWords: int) (numGlyphs: int) (ops: int) =
        let hbuintSize = if extended then 4 else 2
        let headerSize = 4 * hbuintSize
        match Sanitize.fits table.Length (int64 at) headerSize with
        | false -> ValueNone
        | true ->
            let nClasses = if extended then int (Binary.u32 table at) else int (Binary.u16 table at)
            match nClasses >= 4 with
            | false -> ValueNone
            | true ->
                let classTableOffset = if extended then int64 (Binary.u32 table (at + hbuintSize)) else int64 (Binary.u16 table (at + hbuintSize))
                let stateArrayOffset = if extended then int64 (Binary.u32 table (at + 2 * hbuintSize)) else int64 (Binary.u16 table (at + 2 * hbuintSize))
                let entryTableOffset = if extended then int64 (Binary.u32 table (at + 3 * hbuintSize)) else int64 (Binary.u16 table (at + 3 * hbuintSize))
                let classTableAt = int64 at + classTableOffset
                match classTableAt <= int64 Int32.MaxValue with
                | false -> ValueNone
                | true -> finishSanitize table at extended entryDataWords numGlyphs nClasses stateArrayOffset entryTableOffset (int classTableAt) ops

    /// <summary>A safe class reader for a state table whose header failed.</summary>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="table">The bytes.</param>
    /// <returns>The reader.</returns>
    let private defaultReader (extended: bool) (table: ReadOnlyMemory<byte>) =
        match extended with
        | true -> Extended (AatLookup.read table 0 2)
        | false -> Narrow { ClassTable.Table = table; At = 0; FirstGlyph = 0us; Count = 0; Wide = false }

    /// <summary>An empty state machine, returned when a state table fails to decode.</summary>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="classes">The class reader.</param>
    /// <returns>The machine.</returns>
    let private emptyMachine (extended: bool) (classes: ClassReader) =
        { ClassCount = 4
          Classes = classes
          States = ImmutableArray.Empty
          StateBias = 0
          RowCount = 0
          Entries = ImmutableArray.Empty
          IsExtended = extended }

    /// <summary>The unfolder of the flattened state rows, one entry index per element.</summary>
    [<Struct>]
    type private StateUnfolder =
        { Base: int
          StateUnitSize: int
          Extended: bool }
        interface ISpanUnfolder<byte, int, uint16> with
            member this.Next (source, index) =
                struct (stateValueOf source (this.Base + index * this.StateUnitSize) this.Extended, index + 1)

    /// <summary>The unfolder of the decoded entries.</summary>
    [<Struct>]
    type private EntryUnfolder =
        { At: int
          EntrySize: int
          StateArrayOffset: int
          NClasses: int
          Extended: bool
          DataWords: int }
        interface ISpanUnfolder<byte, int, Entry> with
            member this.Next (source, index) =
                let entryAt = this.At + index * this.EntrySize
                struct ({ NewState = newStateOf this.Extended this.StateArrayOffset this.NClasses (int (Binary.u16 source entryAt))
                          Flags = Binary.u16 source (entryAt + 2)
                          Data0 = (match this.DataWords >= 1 with | true -> Binary.u16 source (entryAt + 4) | false -> 0us)
                          Data1 = (match this.DataWords >= 2 with | true -> Binary.u16 source (entryAt + 6) | false -> 0us) }, index + 1)

    /// <summary>Decodes the class reader of a state table.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="classTableAt">The class-table offset.</param>
    /// <returns>The reader.</returns>
    let private readClasses (table: ReadOnlyMemory<byte>) (extended: bool) (classTableAt: int) =
        match extended with
        | true -> Extended (AatLookup.read table classTableAt 2)
        | false ->
            let span = table.Span
            Narrow { ClassTable.Table = table; At = classTableAt; FirstGlyph = Binary.u16 span classTableAt; Count = int (Binary.u16 span (classTableAt + 2)); Wide = false }

    /// <summary>Decodes the state machine from a sanitized table's header fields.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="at">The state-table offset.</param>
    /// <param name="extended">True for the extended form.</param>
    /// <param name="entryDataWords">The entry data words.</param>
    /// <param name="nClasses">The class count.</param>
    /// <param name="stateArrayOffset">The state-array offset value.</param>
    /// <param name="entryTableOffset">The entry-table offset value.</param>
    /// <param name="classTableAt">The class-table offset.</param>
    /// <returns>The machine.</returns>
    let private decodeStateTable
        (table: ReadOnlyMemory<byte>) (at: int) (extended: bool) (entryDataWords: int) (nClasses: int)
        (stateArrayOffset: int64) (entryTableOffset: int64) (classTableAt: int) =
        let span = table.Span
        let classes = readClasses table extended classTableAt
        let stateUnitSize = if extended then 2 else 1
        let entrySize = 4 + 2 * entryDataWords
        let stateArrayOffsetForNewState = if extended then 0 else int stateArrayOffset
        match runSweep span (int64 at + stateArrayOffset) (int64 at + entryTableOffset) stateArrayOffsetForNewState nClasses stateUnitSize entrySize extended (Sanitize.start span.Length) with
        | ValueNone -> emptyMachine extended classes
        | ValueSome result ->
            let rowCount = result.MaxState - result.MinState + 1
            let stateBias = -result.MinState
            let statesAt = int (int64 at + stateArrayOffset)
            let entriesAt = int (int64 at + entryTableOffset)
            let stateBase = statesAt + result.MinState * nClasses * stateUnitSize
            let states = Sealed.unfoldSpan span (rowCount * nClasses) 0 { StateUnfolder.Base = stateBase; StateUnitSize = stateUnitSize; Extended = extended }
            let entries = Sealed.unfoldSpan span result.NumEntries 0 { EntryUnfolder.At = entriesAt; EntrySize = entrySize; StateArrayOffset = stateArrayOffsetForNewState; NClasses = nClasses; Extended = extended; DataWords = entryDataWords }
            { ClassCount = nClasses
              Classes = classes
              States = states
              StateBias = stateBias
              RowCount = rowCount
              Entries = entries
              IsExtended = extended }

    /// <summary>Decodes a state table into a <see cref="StateMachine"/>; the caller has sanitized it first.</summary>
    /// <param name="table">The enclosing bytes.</param>
    /// <param name="at">The state-table offset.</param>
    /// <param name="extended">True for the 32-bit form, false for the obsolete 16-bit form.</param>
    /// <param name="entryDataWords">The number of 16-bit data words per entry.</param>
    /// <returns>The machine.</returns>
    let read (table: ReadOnlyMemory<byte>) (at: int) (extended: bool) (entryDataWords: int) =
        let span = table.Span
        let hbuintSize = if extended then 4 else 2
        let headerSize = 4 * hbuintSize
        match Sanitize.fits span.Length (int64 at) headerSize with
        | false -> emptyMachine extended (defaultReader extended table)
        | true ->
            let nClasses = if extended then int (Binary.u32 span at) else int (Binary.u16 span at)
            match nClasses >= 4 with
            | false -> emptyMachine extended (defaultReader extended table)
            | true ->
                let classTableOffset = if extended then int64 (Binary.u32 span (at + hbuintSize)) else int64 (Binary.u16 span (at + hbuintSize))
                let stateArrayOffset = if extended then int64 (Binary.u32 span (at + 2 * hbuintSize)) else int64 (Binary.u16 span (at + 2 * hbuintSize))
                let entryTableOffset = if extended then int64 (Binary.u32 span (at + 3 * hbuintSize)) else int64 (Binary.u16 span (at + 3 * hbuintSize))
                let classTableAt = int64 at + classTableOffset
                match classTableAt <= int64 Int32.MaxValue with
                | false -> emptyMachine extended (defaultReader extended table)
                | true -> decodeStateTable table at extended entryDataWords nClasses stateArrayOffset entryTableOffset (int classTableAt)

    /// <summary>A glyph's class, or the out-of-range class, <c>StateTable::get_class</c>.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <returns>The class.</returns>
    let inline classOf (machine: StateMachine) (glyph: uint32) (numGlyphs: int) =
        match glyph with
        | DeletedGlyph -> ClassDeletedGlyph
        | other ->
            match machine.Classes with
            | Narrow table -> ClassTable.classOf table other ClassOutOfBounds
            | Wide table -> ClassTable.classOf table other ClassOutOfBounds
            | Extended lookup -> AatLookup.classOf lookup other numGlyphs ClassOutOfBounds

    /// <summary>A glyph's class as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="classOf"/> there.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="numGlyphs">The glyph count of the font.</param>
    /// <returns>The class.</returns>
    let classOfCall (machine: StateMachine) (glyph: uint32) (numGlyphs: int) =
        classOf machine glyph numGlyphs


    /// <summary>The entry of a state and class, <c>StateTable::get_entry</c>; a class at or above the count is out-of-bounds.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="state">The state.</param>
    /// <param name="klass">The class.</param>
    /// <returns>The entry.</returns>
    let inline entry (machine: StateMachine) (state: int) (klass: uint16) =
        let klassIndex =
            match int klass >= machine.ClassCount with
            | true -> int ClassOutOfBounds
            | false -> int klass
        let entryIndex = int machine.States[(state + machine.StateBias) * machine.ClassCount + klassIndex]
        machine.Entries[entryIndex]

    /// <summary>A state machine entry as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="entry"/> there.</summary>
    /// <param name="machine">The machine.</param>
    /// <param name="state">The state.</param>
    /// <param name="klass">The class.</param>
    /// <returns>The entry.</returns>
    let entryCall (machine: StateMachine) (state: int) (klass: uint16) =
        entry machine state klass


    /// <summary>Turns sorted disjoint ranges back into add operations.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <returns>The operations.</returns>
    let private rangesToOps (ranges: ImmutableArray<CodepointRange>) =
        Sealed.mapi (fun _ (range: CodepointRange) -> SetOp.Add (range.First, range.Last)) (ranges.AsSpan())

    /// <summary>The filter of classes reachable from the start state, <c>collect_initial_glyphs</c>'s bit page.</summary>
    [<Struct>]
    type private InitialFilter<'K when 'K : struct and 'K :> IEntryClassifier> =
        { Machine: StateMachine
          Kind: 'K }
        interface IValueFilter with
            member this.Filter value =
                let klass = (int value) &&& (BitsPerPage - 1)
                match klass < this.Machine.ClassCount with
                | false -> false
                | true ->
                    let e = entry this.Machine StartOfText (uint16 klass)
                    e.NewState <> StartOfText || this.Kind.IsActionInitiable e || this.Kind.IsActionable e

    /// <summary>Collects the glyphs the state machine can act on from its start state, <c>collect_initial_glyphs</c>.</summary>
    /// <typeparam name="'K">The subtable-kind classifier.</typeparam>
    /// <param name="machine">The machine.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="kind">The classifier.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let initialGlyphs<'K when 'K : struct and 'K :> IEntryClassifier> (machine: StateMachine) (numGlyphs: int) (kind: 'K) =
        match machine.ClassCount > BitsPerPage with
        | true ->
            match machine.Classes with
            | Narrow table -> ClassTable.collectGlyphs table
            | Wide table -> ClassTable.collectGlyphs table
            | Extended lookup -> AatLookup.collectGlyphs lookup numGlyphs
        | false ->
            let filter: InitialFilter<'K> = { Machine = machine; Kind = kind }
            let collected =
                match machine.Classes with
                | Narrow table -> ClassTable.collectGlyphsFiltered table filter
                | Wide table -> ClassTable.collectGlyphsFiltered table filter
                | Extended lookup -> AatLookup.collectGlyphsFiltered lookup numGlyphs filter
            let includeDeleted =
                let e = entry machine StartOfText ClassDeletedGlyph
                e.NewState <> StartOfText || kind.IsActionInitiable e || kind.IsActionable e
            let ops =
                match includeDeleted with
                | false -> rangesToOps collected
                | true -> Sealed.concat (Sealed.init 1 (fun _ -> SetOp.Add (DeletedGlyph, DeletedGlyph))) (rangesToOps collected)
            Sealed.rangeSet (ops.AsSpan())
