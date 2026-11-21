namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>A label of the Ragel scanner's goto machine (src/hb-ot-shaper-indic-machine.hh:1145-1294).</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal RagelLabel =
    | Resume
    | Dispatch
    | Execute
    | Again
    | TestEof

/// <summary>The state of one Ragel scan: the label, cursor, token bounds, action pointer, serial and broken flag.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal RagelState =
    { Label: RagelLabel
      Cs: int
      P: int
      Ts: int
      Te: int
      Act: int
      Trans: int
      Ops: ImmutableArray<RagelOp>
      OpIndex: int
      Serial: int
      Broken: bool }

/// <summary>What the scanner returns: a found syllable with its resume state, or the finished scan.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal RagelEvent =
    | SyllableFound of start: int * stop: int * value: byte * resume: RagelState
    | ScanFinished of final: RagelState

/// <summary>The single Ragel interpreter that runs every machine's flat tables.</summary>
[<RequireQualifiedAccess>]
module internal Ragel =
    /// <summary>The key index of the -F1 lookup (src/hb-ot-shaper-indic-machine.hh:1165-1168).</summary>
    /// <param name="low">The first key of the state's range.</param>
    /// <param name="high">The last key of the state's range.</param>
    /// <param name="span">The state's key span.</param>
    /// <param name="c">The glyph category.</param>
    /// <returns>The index into the state's indicies, or the span when out of range.</returns>
    let inline keySlot (low: byte) (high: byte) (span: int) (c: byte) : int =
        match span > 0 && low <= c && c <= high with
        | true -> int (c - low)
        | false -> span

    /// <summary>The syllable serial wraps 16 to 1 (src/hb-ot-shaper-indic-machine.rl:128-129).</summary>
    /// <param name="serial">The current serial.</param>
    /// <returns>The next serial.</returns>
    let inline nextSerial (serial: int) : int =
        match serial + 1 with
        | 16 -> 1
        | next -> next

    /// <summary>Runs the scanner from <paramref name="state"/> until a syllable is found or the input ends; mirrors the exec block of src/hb-ot-shaper-indic-machine.hh:1145-1294.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <param name="categories">The per-glyph category bytes.</param>
    /// <param name="state">The scanner state.</param>
    /// <returns>The next syllable or the finished scan.</returns>
    [<TailCall>]
    let rec step (machine: RagelMachine) (categories: ReadOnlySpan<byte>) (state: RagelState) : RagelEvent =
        match state.Label with
        | RagelLabel.Resume ->
            let ts = (match machine.SetsTsOnEntry[state.Cs] with | true -> state.P | false -> state.Ts)
            let slot = keySlot machine.TransKeys[2 * state.Cs] machine.TransKeys[2 * state.Cs + 1] (int machine.KeySpans[state.Cs]) categories[state.P]
            step machine categories { state with Ts = ts; Trans = int machine.Indicies[int machine.IndexOffsets[state.Cs] + slot]; Label = RagelLabel.Dispatch }
        | RagelLabel.Dispatch ->
            let target = int machine.TransTargs[state.Trans]
            match machine.Actions[int machine.TransActions[state.Trans]] with
            | RagelAction.NoAction -> step machine categories { state with Cs = target; Label = RagelLabel.Again }
            | RagelAction.Ops ops -> step machine categories { state with Cs = target; Ops = ops; OpIndex = 0; Label = RagelLabel.Execute }
            | RagelAction.ActSwitch byAct ->
                let ops = (match state.Act < byAct.Length with | true -> byAct[state.Act] | false -> ImmutableArray<RagelOp>.Empty)
                step machine categories { state with Cs = target; Ops = ops; OpIndex = 0; Label = RagelLabel.Execute }
        | RagelLabel.Execute when state.OpIndex = state.Ops.Length -> step machine categories { state with Label = RagelLabel.Again }
        | RagelLabel.Execute ->
            let next = state.OpIndex + 1
            match state.Ops[state.OpIndex] with
            | RagelOp.TsFromP -> step machine categories { state with Ts = state.P; OpIndex = next }
            | RagelOp.TsReset -> step machine categories { state with Ts = 0; OpIndex = next }
            | RagelOp.TeFromNextP -> step machine categories { state with Te = state.P + 1; OpIndex = next }
            | RagelOp.TeFromP -> step machine categories { state with Te = state.P; OpIndex = next }
            | RagelOp.PBack -> step machine categories { state with P = state.P - 1; OpIndex = next }
            | RagelOp.PFromTe -> step machine categories { state with P = state.Te - 1; OpIndex = next }
            | RagelOp.ActSet act -> step machine categories { state with Act = int act; OpIndex = next }
            | RagelOp.FoundSyllable (syllableType, broken) ->
                RagelEvent.SyllableFound (state.Ts, state.Te, byte ((state.Serial <<< 4) ||| int syllableType),
                                          { state with OpIndex = next; Serial = nextSerial state.Serial; Broken = state.Broken || broken })
        | RagelLabel.Again ->
            let ts = (match machine.ResetsTsOnExit[state.Cs] with | true -> 0 | false -> state.Ts)
            let p = state.P + 1
            match p = categories.Length with
            | false -> step machine categories { state with Ts = ts; P = p; Label = RagelLabel.Resume }
            | true -> step machine categories { state with Ts = ts; P = p; Label = RagelLabel.TestEof }
        | RagelLabel.TestEof ->
            let eof = int machine.EofTrans[state.Cs]
            match state.P = categories.Length && eof > 0 with
            | true -> step machine categories { state with Trans = eof - 1; Label = RagelLabel.Dispatch }
            | false -> RagelEvent.ScanFinished state

    /// <summary>Builds the initial scanner state: <c>TestEof</c> when the input is empty, otherwise <c>Resume</c> at the machine's start state.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <param name="length">The input length.</param>
    /// <returns>The initial state.</returns>
    let start (machine: RagelMachine) (length: int) : RagelState =
        { Label = (match length = 0 with | true -> RagelLabel.TestEof | false -> RagelLabel.Resume)
          Cs = machine.Start
          P = 0
          Ts = 0
          Te = 0
          Act = 0
          Trans = 0
          Ops = ImmutableArray<RagelOp>.Empty
          OpIndex = 0
          Serial = 1
          Broken = false }

    /// <summary>Maps one machine index to its buffer index, through the USE positions when present.</summary>
    /// <param name="positions">The USE kept-original-index array, or the default for the identity mapping.</param>
    /// <param name="index">The machine index.</param>
    /// <returns>The buffer index.</returns>
    let private mapIndex (positions: ImmutableArray<int>) (index: int) : int =
        match positions.IsDefault with
        | true -> index
        | false -> positions[index]

    /// <summary>Maps a machine syllable [ts, te) to the buffer indices it paints, through the USE positions when present.</summary>
    /// <param name="positions">The USE kept-original-index array, or the default for the identity mapping.</param>
    /// <param name="categoriesLength">The filtered input length.</param>
    /// <param name="length">The buffer length.</param>
    /// <param name="ts">The machine start index.</param>
    /// <param name="te">The machine end index, possibly one past the filtered input.</param>
    /// <returns>The painted start and stop buffer indices.</returns>
    let private paintedRange (positions: ImmutableArray<int>) (categoriesLength: int) (length: int) (ts: int) (te: int) : struct (int * int) =
        let stop = (match te < categoriesLength with | true -> mapIndex positions te | false -> length)
        struct (mapIndex positions ts, stop)

    /// <summary>Wraps a machine scan as a run source, mapping machine indices back to buffer indices.</summary>
    [<Struct>]
    type private SyllableSource =
        val Machine: RagelMachine
        val Categories: ImmutableArray<byte>
        val Positions: ImmutableArray<int>
        val Length: int
        new (machine, categories, positions, length) =
            { Machine = machine
              Categories = categories
              Positions = positions
              Length = length }
        interface IRunSource<RagelState> with
            member this.Next(state) =
                match step this.Machine (this.Categories.AsSpan()) state with
                | RagelEvent.SyllableFound (ts, te, value, resume) ->
                    let struct (start, stop) = paintedRange this.Positions this.Categories.Length this.Length ts te
                    RunStep.Run (start, stop, value, resume)
                | RagelEvent.ScanFinished final ->
                    RunStep.Exhausted final

    /// <summary>Finds the syllables of a category run, painting the syllable bytes and reporting the broken-syllable flag.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <param name="categories">The per-glyph category bytes.</param>
    /// <param name="positions">The default array for the identity mapping, or USE's kept-original-index array.</param>
    /// <param name="previous">The previous syllable bytes, one per buffer glyph.</param>
    /// <returns>The painted syllable bytes and the broken-syllable flag.</returns>
    let findSyllables (machine: RagelMachine) (categories: ImmutableArray<byte>) (positions: ImmutableArray<int>) (previous: ImmutableArray<byte>) : struct (ImmutableArray<byte> * bool) =
        let source = SyllableSource(machine, categories, positions, previous.Length)
        let struct (painted, final) = Sealed.paintRuns previous source (start machine categories.Length)
        struct (painted, final.Broken)

    /// <summary>The flat code of one statement of an action body.</summary>
    /// <param name="op">The statement.</param>
    /// <returns>The code, the statement in the low byte and its argument above.</returns>
    let private opCode (op: RagelOp) =
        match op with
        | RagelOp.TsFromP -> RagelCodes.TsFromP
        | RagelOp.TsReset -> RagelCodes.TsReset
        | RagelOp.TeFromNextP -> RagelCodes.TeFromNextP
        | RagelOp.TeFromP -> RagelCodes.TeFromP
        | RagelOp.PBack -> RagelCodes.PBack
        | RagelOp.PFromTe -> RagelCodes.PFromTe
        | RagelOp.ActSet act -> RagelCodes.ActSet ||| (int act <<< RagelCodes.ArgumentShift)
        | RagelOp.FoundSyllable (syllableType, broken) ->
            RagelCodes.FoundSyllable ||| (int syllableType <<< RagelCodes.ArgumentShift) ||| ((match broken with | true -> 1 | false -> 0) <<< RagelCodes.BrokenShift)

    /// <summary>The action bodies an action holds, each a list of statements: none, the one, or one for each value of <c>act</c>.</summary>
    /// <param name="action">The action.</param>
    /// <returns>The lists.</returns>
    let private listsOf (action: RagelAction) =
        match action with
        | RagelAction.NoAction -> List.empty
        | RagelAction.Ops ops -> [ ops ]
        | RagelAction.ActSwitch byAct -> List.ofSeq byAct

    /// <summary>The kind of an action as the flat scan reads it.</summary>
    /// <param name="action">The action.</param>
    /// <returns>The kind.</returns>
    let private kindOf (action: RagelAction) =
        match action with
        | RagelAction.NoAction -> RagelCodes.NoAction
        | RagelAction.Ops _ -> RagelCodes.OpsAction
        | RagelAction.ActSwitch _ -> RagelCodes.SwitchAction

    /// <summary>A position as the scan holds it before an action body runs: the cursor or the token end, and an offset from it.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Form =
        { FromTe: bool
          Offset: int }

    /// <summary>What the statements of an action body have done so far, as forms of the cursor and the token end at its start.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Effect =
        { Te: Form
          P: Form
          Act: int
          Found: int
          FoundTe: Form
          Supported: bool }

    /// <summary>The form of a cursor or token end that no statement has moved.</summary>
    let private unmoved isTe = { FromTe = isTe; Offset = 0 }

    /// <summary>The effect of no statement.</summary>
    let private idle =
        { Te = unmoved true
          P = unmoved false
          Act = -1
          Found = -1
          FoundTe = unmoved true
          Supported = true }

    /// <summary>The effect after one more statement of an action body.</summary>
    /// <param name="effect">The effect so far.</param>
    /// <param name="op">The statement.</param>
    /// <returns>The effect with the statement run.</returns>
    let private stepEffect (effect: Effect) (op: RagelOp) =
        match op with
        | RagelOp.TsFromP | RagelOp.TsReset -> { effect with Supported = false }
        | RagelOp.TeFromNextP -> { effect with Te = { effect.P with Offset = effect.P.Offset + 1 } }
        | RagelOp.TeFromP -> { effect with Te = effect.P }
        | RagelOp.PBack -> { effect with P = { effect.P with Offset = effect.P.Offset - 1 } }
        | RagelOp.PFromTe -> { effect with P = { effect.Te with Offset = effect.Te.Offset - 1 } }
        | RagelOp.ActSet act -> { effect with Act = int act }
        | RagelOp.FoundSyllable (syllableType, broken) ->
            match effect.Found < 0 with
            | false -> { effect with Supported = false }
            | true ->
                { effect with
                    Found = int syllableType ||| (match broken with | true -> 256 | false -> 0)
                    FoundTe = effect.Te }

    /// <summary>The packed form of a form.</summary>
    /// <param name="form">The form.</param>
    /// <returns>The base bit and the biased offset, in the low four bits.</returns>
    let private packForm (form: Form) =
        (match form.FromTe with | true -> RagelCodes.EffectTeFromTe | false -> 0) ||| ((form.Offset + RagelCodes.EffectOffsetBias) <<< 1)

    /// <summary>Whether a form fits the packed offset.</summary>
    /// <param name="form">The form.</param>
    /// <returns>True when the offset is within the range of the packing.</returns>
    let private fitsForm (form: Form) =
        form.Offset + RagelCodes.EffectOffsetBias >= 0 && form.Offset + RagelCodes.EffectOffsetBias <= RagelCodes.EffectOffsetMask

    /// <summary>The effect of an action body as one number the scan applies without running its statements, or the slow marker for a body
    /// the packing does not hold (a statement that moves the token start, two syllables found, an offset out of range).</summary>
    /// <param name="ops">The statements of the body.</param>
    /// <returns>The packed effect.</returns>
    let private effectCode (ops: RagelOp seq) =
        let effect = Seq.fold stepEffect idle ops
        match effect.Supported && fitsForm effect.Te && fitsForm effect.P && fitsForm effect.FoundTe && effect.Act < 256 with
        | false -> RagelCodes.EffectSlow
        | true ->
            packForm effect.Te
            ||| (packForm effect.P <<< RagelCodes.EffectPShift)
            ||| (match effect.Act < 0 with | true -> 0 | false -> RagelCodes.EffectHasAct ||| (effect.Act <<< RagelCodes.EffectActShift))
            ||| (match effect.Found < 0 with
                 | true -> 0
                 | false ->
                     RagelCodes.EffectFound
                     ||| ((effect.Found &&& 255) <<< RagelCodes.EffectTypeShift)
                     ||| (match effect.Found &&& 256 with | 0 -> 0 | _ -> RagelCodes.EffectBroken)
                     ||| (packForm effect.FoundTe <<< RagelCodes.EffectFoundShift))

    /// <summary>The highest key any state of a machine has a range for.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <returns>The key, zero when no state has a range.</returns>
    let private highestKey (machine: RagelMachine) =
        Seq.init (machine.TransKeys.Length / 2) (fun state -> match machine.KeySpans[state] > 0uy with | true -> int machine.TransKeys[2 * state + 1] | false -> 0)
        |> Seq.fold max 0

    /// <summary>The transition a state takes on a category, <c>-F1</c>'s lookup: the range of the state's keys, else its default.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <param name="state">The state.</param>
    /// <param name="category">The category, which may lie above every key.</param>
    /// <returns>The index of the transition.</returns>
    let private transitionOf (machine: RagelMachine) state category =
        let low = int machine.TransKeys[2 * state]
        let high = int machine.TransKeys[2 * state + 1]
        let span = int machine.KeySpans[state]
        let slot = (match span > 0 && low <= category && category <= high with | true -> category - low | false -> span)
        int machine.Indicies[int machine.IndexOffsets[state] + slot]

    /// <summary>The packed next state of a transition: the target in the low byte, and whether the target sets and whether it resets the
    /// token start.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <param name="transition">The transition.</param>
    /// <returns>The packed target.</returns>
    let private nextOf (machine: RagelMachine) transition =
        let target = int machine.TransTargs[transition]
        target
        ||| (match machine.SetsTsOnEntry[target] with | true -> 256 | false -> 0)
        ||| (match machine.ResetsTsOnExit[target] with | true -> 512 | false -> 0)

    /// <summary>The effect a transition applies without a look at the action table, or the switch marker for a transition whose body
    /// depends on <c>act</c> or is not held by the packed effects.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <param name="firsts">The first list of every action.</param>
    /// <param name="effects">The packed effect of every list.</param>
    /// <param name="transition">The transition.</param>
    /// <returns>The packed effect, or the switch marker.</returns>
    let private denseEffectOf (machine: RagelMachine) (firsts: int array) (effects: int array) transition =
        let action = int machine.TransActions[transition]
        match machine.Actions[action] with
        | RagelAction.NoAction -> effectCode Seq.empty
        | RagelAction.Ops _ -> (match effects[firsts[action]] with | RagelCodes.EffectSlow -> RagelCodes.EffectSwitch | packed -> packed)
        | RagelAction.ActSwitch _ -> RagelCodes.EffectSwitch

    /// <summary>Flattens a machine's tables for <see cref="Sealed.SyllableOwned.paintFlat"/>, once for the machine: the transition tables
    /// as they are, and every action body as a range of one array of codes.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <returns>The flat tables.</returns>
    let compile (machine: RagelMachine) =
        let actions = List.ofSeq machine.Actions
        let lists = actions |> List.collect listsOf
        let firsts = actions |> List.scan (fun total action -> total + (listsOf action).Length) 0
        let ends = lists |> List.scan (fun total ops -> total + ops.Length) 0
        let effects = lists |> List.map (fun ops -> effectCode ops)
        let columns = highestKey machine + 2
        let transitions = Array.init ((machine.TransKeys.Length / 2) * columns) (fun slot -> transitionOf machine (slot / columns) (slot % columns))
        { FlatTransKeys = machine.TransKeys
          FlatKeySpans = machine.KeySpans
          FlatIndexOffsets = ImmutableArray.CreateRange (Seq.map int machine.IndexOffsets)
          FlatIndicies = machine.Indicies
          FlatTransTargs = machine.TransTargs
          FlatTransActions = machine.TransActions
          FlatSetsTs = machine.SetsTsOnEntry
          FlatResetsTs = machine.ResetsTsOnExit
          FlatEofTrans = ImmutableArray.CreateRange (Seq.map int machine.EofTrans)
          ActionKinds = ImmutableArray.CreateRange (Seq.map kindOf actions)
          ActionFirstList = ImmutableArray.CreateRange (List.take actions.Length firsts)
          ActionListCount = ImmutableArray.CreateRange (Seq.map (fun action -> (listsOf action).Length) actions)
          ListStarts = ImmutableArray.CreateRange (List.take lists.Length ends)
          ListEnds = ImmutableArray.CreateRange (List.tail ends)
          OpCodes = ImmutableArray.CreateRange (lists |> List.collect (fun ops -> List.ofSeq (Seq.map opCode ops)))
          ListEffects = ImmutableArray.CreateRange effects
          DenseColumns = columns
          DenseMaxKey = columns - 2
          DenseNext = ImmutableArray.CreateRange (transitions |> Array.map (nextOf machine))
          DenseEffect = ImmutableArray.CreateRange (transitions |> Array.map (denseEffectOf machine (Array.ofList (List.take actions.Length firsts)) (Array.ofList effects)))
          DenseTrans = ImmutableArray.CreateRange transitions
          FlatStart = machine.Start }

    /// <summary>The flat tables of the Indic machine.</summary>
    let indic = compile IndicMachine.machine

    /// <summary>The flat tables of the Khmer machine.</summary>
    let khmer = compile KhmerMachine.machine

    /// <summary>The flat tables of the Myanmar machine.</summary>
    let myanmar = compile MyanmarMachine.machine

    /// <summary>The flat tables of the universal shaping engine's machine.</summary>
    let universal = compile UseMachine.machine

    /// <summary>Finds the syllables of a category run and paints their bytes into the properties in place, the result of
    /// <see cref="findSyllables"/> without the copies, with the scan run as one loop on the machine's flat tables.</summary>
    /// <param name="flat">The flat tables of the machine, <see cref="compile"/>.</param>
    /// <param name="categories">The per-glyph category bytes.</param>
    /// <param name="positions">The default array for the identity mapping, or USE's kept-original-index array.</param>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <returns>Whether a broken syllable was found.</returns>
    let findSyllablesOwned (flat: RagelCompiled) (categories: ImmutableArray<byte>) (positions: ImmutableArray<int>) (props: ImmutableArray<GlyphProps>) =
        SyllableOwned.paintFlat flat categories positions props

    /// <summary>Finds the syllables of a category run and paints their bytes into the properties in place through the interpreter that
    /// <see cref="findSyllables"/> runs, the reference of <see cref="findSyllablesOwned"/>.</summary>
    /// <param name="machine">The machine tables.</param>
    /// <param name="categories">The per-glyph category bytes.</param>
    /// <param name="positions">The default array for the identity mapping, or USE's kept-original-index array.</param>
    /// <param name="props">The properties, made by the caller in this call and shared with no one.</param>
    /// <returns>Whether a broken syllable was found.</returns>
    let findSyllablesOwnedStepped (machine: RagelMachine) (categories: ImmutableArray<byte>) (positions: ImmutableArray<int>) (props: ImmutableArray<GlyphProps>) =
        let source = SyllableSource(machine, categories, positions, props.Length)
        let final = SyllableOwned.paint props source (start machine categories.Length)
        final.Broken

    /// <summary>Finds the first index at or after <paramref name="from"/> whose category is not CGJ, or none.</summary>
    /// <param name="categories">The categories.</param>
    /// <param name="from">The first index to test.</param>
    /// <returns>The index, or none.</returns>
    [<TailCall>]
    let rec private nextNonCgj (categories: ImmutableArray<byte>) (from: int) : int voption =
        match from < categories.Length with
        | false -> ValueNone
        | true ->
            match categories[from] = UseMachine.CGJ with
            | true -> nextNonCgj categories (from + 1)
            | false -> ValueSome from

    /// <summary>Whether a ZWNJ is kept: kept when no later non-CGJ glyph exists, or that glyph is not a Unicode mark.</summary>
    /// <param name="categories">The categories.</param>
    /// <param name="isUnicodeMark">The mark flags.</param>
    /// <param name="index">The ZWNJ index.</param>
    /// <returns>True when the ZWNJ is kept.</returns>
    let private keepsZwnj (categories: ImmutableArray<byte>) (isUnicodeMark: ImmutableArray<bool>) (index: int) : bool =
        match nextNonCgj categories (index + 1) with
        | ValueNone -> true
        | ValueSome j -> not isUnicodeMark[j]

    /// <summary>Whether a USE glyph survives the machine_index filter (src/hb-ot-shaper-use-machine.hh:909-924).</summary>
    /// <param name="categories">The categories.</param>
    /// <param name="isUnicodeMark">The mark flags.</param>
    /// <param name="index">The glyph index.</param>
    /// <returns>True when the glyph is kept.</returns>
    let private shouldKeep (categories: ImmutableArray<byte>) (isUnicodeMark: ImmutableArray<bool>) (index: int) : bool =
        match categories[index] = UseMachine.CGJ with
        | true -> false
        | false ->
            match categories[index] = UseMachine.ZWNJ with
            | false -> true
            | true -> keepsZwnj categories isUnicodeMark index

    /// <summary>Counts the glyphs kept by the USE filter.</summary>
    /// <param name="categories">The categories.</param>
    /// <param name="isUnicodeMark">The mark flags.</param>
    /// <param name="index">The next index.</param>
    /// <param name="count">The kept count so far.</param>
    /// <returns>The total kept count.</returns>
    [<TailCall>]
    let rec private countKept (categories: ImmutableArray<byte>) (isUnicodeMark: ImmutableArray<bool>) (index: int) (count: int) : int =
        match index < categories.Length with
        | false -> count
        | true -> countKept categories isUnicodeMark (index + 1) (match shouldKeep categories isUnicodeMark index with | true -> count + 1 | false -> count)

    /// <summary>Finds the first kept glyph at or after an index.</summary>
    /// <param name="categories">The categories.</param>
    /// <param name="isUnicodeMark">The mark flags.</param>
    /// <param name="index">The next candidate index.</param>
    /// <returns>The original index of the kept glyph.</returns>
    [<TailCall>]
    let rec private nextKept (categories: ImmutableArray<byte>) (isUnicodeMark: ImmutableArray<bool>) (index: int) =
        match shouldKeep categories isUnicodeMark index with
        | true -> index
        | false -> nextKept categories isUnicodeMark (index + 1)

    /// <summary>The original indices of the kept glyphs, each found by the cursor of the one before it, so that the whole run is
    /// read once.</summary>
    [<Struct>]
    type private KeptIndices =
        val Categories: ImmutableArray<byte>
        val IsUnicodeMark: ImmutableArray<bool>

        /// <summary>Creates the unfolder.</summary>
        /// <param name="categories">The categories.</param>
        /// <param name="isUnicodeMark">The mark flags.</param>
        new (categories, isUnicodeMark) = { Categories = categories; IsUnicodeMark = isUnicodeMark }

        interface ISpanUnfolder<byte, int, int> with
            member this.Next (_, cursor) =
                let index = nextKept this.Categories this.IsUnicodeMark cursor
                struct (index, index + 1)

    /// <summary>Filters a USE category run the way <c>find_syllables_use</c> does: drop CGJ, and drop a ZWNJ before a Unicode mark.</summary>
    /// <param name="categories">The per-glyph categories.</param>
    /// <param name="isUnicodeMark">The per-glyph Unicode-mark flags.</param>
    /// <returns>The kept categories and their original indices.</returns>
    let useFilter (categories: ImmutableArray<byte>) (isUnicodeMark: ImmutableArray<bool>) : struct (ImmutableArray<byte> * ImmutableArray<int>) =
        let count = countKept categories isUnicodeMark 0 0
        let indices = Sealed.unfoldSpan (categories.AsSpan()) count 0 (KeptIndices(categories, isUnicodeMark))
        let filtered = Sealed.map (fun index -> categories[index]) (indices.AsSpan())
        struct (filtered, indices)
