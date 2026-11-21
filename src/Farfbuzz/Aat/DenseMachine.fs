namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The safe-to-break test of <c>StateTableDriver::drive</c>, shared by the driver and by the dense compile.</summary>
[<RequireQualifiedAccess>]
module internal StateSafety =
    /// <summary>The <c>Flags::DontAdvance</c> entry bit, an epsilon transition (src/hb-aat-layout-morx-table.hh:59).</summary>
    [<Literal>]
    let DontAdvanceFlag = 0x4000us

    /// <summary>Whether breaking before the current glyph is safe, the condition of src/hb-aat-layout-common.hh:1397-1423.</summary>
    /// <typeparam name="'Kind">The subtable kind.</typeparam>
    /// <param name="machine">The state machine.</param>
    /// <param name="kind">The kind.</param>
    /// <param name="state">The current state number.</param>
    /// <param name="klass">The class.</param>
    /// <param name="entry">The entry.</param>
    /// <param name="nextState">The next state.</param>
    /// <returns>True when safe to break.</returns>
    let safeToBreak<'Kind when 'Kind :> IEntryClassifier> (machine: StateMachine) (kind: 'Kind) state (klass: uint16) (entry: Entry) nextState =
        match kind.IsActionable entry with
        | true -> false
        | false ->
            match state = StateTable.StartOfText
                  || (entry.Flags &&& DontAdvanceFlag <> 0us && nextState = StateTable.StartOfText) with
            | true -> not (kind.IsActionable (StateTable.entry machine state StateTable.ClassEndOfText))
            | false ->
                let wouldBeEntry = StateTable.entry machine StateTable.StartOfText klass
                let sameAsStart =
                    not (kind.IsActionable wouldBeEntry)
                    && nextState = wouldBeEntry.NewState
                    && (entry.Flags &&& DontAdvanceFlag) = (wouldBeEntry.Flags &&& DontAdvanceFlag)
                sameAsStart && not (kind.IsActionable (StateTable.entry machine state StateTable.ClassEndOfText))

/// <summary>
/// A state table compiled into dense arrays, built once with the face: the class of every glyph, the entry of every
/// state and class with the indirection through the state array removed, and the safe-to-break answer of every cell.
/// </summary>
[<NoComparison; NoEquality>]
type internal DenseMachine =
    { GlyphClasses: ImmutableArray<byte>
      Stride: int // the classes per state
      Bias: int // the state bias
      Cells: ImmutableArray<Entry>
      SafeCells: ImmutableArray<bool>
      StartInert: ImmutableArray<byte> // per class: the start state stays, safely, with no action and no mark flag
      QuietGlyphs: ImmutableArray<byte> } // per glyph: the glyph's class is one of those

/// <summary>Compiling a state table into a <see cref="DenseMachine"/>.</summary>
[<RequireQualifiedAccess>]
module internal DenseMachine =
    /// <summary>The most state-and-class cells a dense table holds; a larger table is read through the state array.</summary>
    [<Literal>]
    let MaxCells = 65536

    /// <summary>The most glyphs a dense class array covers.</summary>
    [<Literal>]
    let MaxGlyphs = 200000

    /// <summary>The entry flags a transition that leaves the start state untouched must not carry: set mark (or mark first),
    /// do not advance and mark last, the bits every kind's null transition reads.</summary>
    [<Literal>]
    let private InertFlags = 0xE000us

    /// <summary>The fewest classes a table has: the four reserved ones.</summary>
    [<Literal>]
    let MinClasses = 4

    /// <summary>Whether every class of an array fits a byte.</summary>
    /// <param name="classes">The classes.</param>
    /// <param name="index">The next class.</param>
    /// <returns>True when all fit.</returns>
    [<TailCall>]
    let rec private fitBytes (classes: ReadOnlySpan<uint16>) (index: int) =
        match index >= classes.Length with
        | true -> true
        | false ->
            match classes[index] > 255us with
            | true -> false
            | false -> fitBytes classes (index + 1)

    /// <summary>Whether every cell of the state array names an entry.</summary>
    /// <param name="states">The state array.</param>
    /// <param name="entryCount">The number of entries.</param>
    /// <param name="index">The next cell.</param>
    /// <returns>True when all cells are in range.</returns>
    [<TailCall>]
    let rec private cellsInRange (states: ReadOnlySpan<uint16>) (entryCount: int) (index: int) =
        match index >= states.Length with
        | true -> true
        | false ->
            match int states[index] >= entryCount with
            | true -> false
            | false -> cellsInRange states entryCount (index + 1)

    /// <summary>Compiles a state table, or declines when it does not fit the caps or is not well formed.</summary>
    /// <typeparam name="'Kind">The subtable kind's classifier.</typeparam>
    /// <param name="machine">The state machine.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="kind">The kind's classifier.</param>
    /// <returns>The dense machine, or none.</returns>
    let build<'Kind when 'Kind :> IEntryClassifier> (machine: StateMachine) (numGlyphs: int) (kind: 'Kind) =
        let classCount = machine.ClassCount
        let rows = match classCount > 0 with | true -> machine.States.Length / classCount | false -> 0
        let wellFormed =
            classCount >= MinClasses
            && rows > 0
            && rows * classCount <= MaxCells
            && machine.StateBias >= 0
            && machine.StateBias < rows
            && numGlyphs > 0
            && numGlyphs <= MaxGlyphs
            && cellsInRange (machine.States.AsSpan()) machine.Entries.Length 0
        match wellFormed with
        | false -> ValueNone
        | true ->
            let classes = Sealed.init numGlyphs (fun glyph -> StateTable.classOf machine (uint32 glyph) numGlyphs)
            match fitBytes (classes.AsSpan()) 0 with
            | false -> ValueNone
            | true ->
                let entries = Sealed.init (rows * classCount) (fun cell -> machine.Entries[int machine.States[cell]])
                let safe =
                    Sealed.init (rows * classCount) (fun cell ->
                        let entry = entries[cell]
                        let state = cell / classCount - machine.StateBias
                        StateSafety.safeToBreak machine kind state (uint16 (cell % classCount)) entry entry.NewState)
                let inert =
                    Sealed.init classCount (fun klass ->
                        let cell = machine.StateBias * classCount + klass
                        let entry = entries[cell]
                        match (not (kind.IsActionable entry)) && entry.NewState = StateTable.StartOfText && entry.Flags &&& InertFlags = 0us && safe[cell] with
                        | true -> 1uy
                        | false -> 0uy)
                let quiet =
                    Sealed.init numGlyphs (fun glyph ->
                        let klass = int classes[glyph]
                        inert[match klass < classCount with | true -> klass | false -> int StateTable.ClassOutOfBounds])
                ValueSome
                    { GlyphClasses = Sealed.map byte (classes.AsSpan())
                      Stride = classCount
                      Bias = machine.StateBias
                      Cells = entries
                      SafeCells = safe
                      StartInert = inert
                      QuietGlyphs = quiet }
