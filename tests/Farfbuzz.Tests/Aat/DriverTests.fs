module Farfbuzz.Tests.AatDriverTests

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Aat

/// <summary>The <c>Flags::Actionable</c> entry bit the synthetic kinds treat as an action.</summary>
[<Literal>]
let private ActionableFlag = 0x8000us

/// <summary>The <c>Flags::DontAdvance</c> entry bit, an epsilon transition.</summary>
[<Literal>]
let private DontAdvanceFlag = 0x4000us

/// <summary>Whether an entry performs an action for the synthetic kinds.</summary>
/// <param name="entry">The entry.</param>
/// <returns>True for the actionable flag.</returns>
let private isActionableEntry (entry: Entry) = entry.Flags &&& ActionableFlag <> 0us

/// <summary>Builds one decoded entry.</summary>
/// <param name="newState">The state the entry leads to.</param>
/// <param name="flags">The entry flags.</param>
/// <returns>The entry.</returns>
let private entry (newState: int) (flags: uint16) = { NewState = newState; Flags = flags; Data0 = 0us; Data1 = 0us }

/// <summary>Builds a state machine whose class table maps glyph <c>g</c> to class <c>g</c>, with one entry per
/// state-and-class pair in row-major order.</summary>
/// <param name="nClasses">The class count, at least 4.</param>
/// <param name="entries">The entries, <c>nStates * nClasses</c> of them.</param>
/// <returns>The machine.</returns>
let private machineFromEntries (nClasses: int) (entries: Entry array) =
    let nStates = entries.Length / nClasses
    let classBytes = Array.init (4 + nClasses) (fun i -> if i < 4 then 0uy else byte (i - 4))
    { ClassCount = nClasses
      Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = nClasses; Wide = false }
      States = Sealed.init entries.Length (fun i -> uint16 i)
      StateBias = 0
      RowCount = nStates
      Entries = Sealed.init entries.Length (fun i -> entries[i])
      IsExtended = false }

/// <summary>Builds the driver config, computing the start state's end-of-text safety.</summary>
/// <param name="machine">The machine.</param>
/// <param name="ranges">The compiled ranges.</param>
/// <param name="subtableFlags">The subtable flags.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>The config.</returns>
let private configOf (machine: StateMachine) (ranges: ImmutableArray<RangeFlags>) (subtableFlags: uint32) (numGlyphs: int) =
    { Machine = machine
      Ranges = ranges
      SubtableFlags = subtableFlags
      NumGlyphs = numGlyphs
      StartSafeToBreakAtEndOfText = not (isActionableEntry (StateTable.entryCall machine StateTable.StartOfText StateTable.ClassEndOfText)) }

/// <summary>The initial driver state for the synthetic kinds.</summary>
let private seedInt : DriverState<int> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = 0 }

/// <summary>A kind that stays in place and never emits a transition edit.</summary>
[<Struct>]
type private NoopKind =
    interface IStateMachineKind<int> with
        member _.InPlace = true
        member _.IsActionable entry = isActionableEntry entry
        member _.IsActionInitiable _ = false
        member _.Transition (_, _, context) = struct (Emission.Complete, context)
        member _.NullTransition (_, _, context) = context

/// <summary>A kind whose transition emits one marker edit for the first actionable entry it sees, then completes.</summary>
[<Struct>]
type private MarkerKind =
    val Mark: RunEdit
    new (mark) = { Mark = mark }
    interface IStateMachineKind<int> with
        member _.InPlace = true
        member _.IsActionable entry = isActionableEntry entry
        member _.IsActionInitiable _ = false
        member this.Transition (_, entry, context) =
            match context = 0 && isActionableEntry entry with
            | true -> struct (Emission.Emit this.Mark, 1)
            | false -> struct (Emission.Complete, context)
        member _.NullTransition (_, _, context) = context

/// <summary>A pass that records every edit the driver emits before forwarding it.</summary>
[<Struct>]
type private RecordingPass<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct> =
    val Config: DriverConfig
    val Kind: 'Kind
    val Edits: ResizeArray<RunEdit>
    new (config, kind, edits) = { Config = config; Kind = kind; Edits = edits }
    interface IRunPass<DriverState<'Context>> with
        member this.Step (cursor, state) =
            let struct (edit, next) = Driver.step &this.Config this.Kind &cursor state
            this.Edits.Add edit
            struct (edit, next)

/// <summary>Normalises one edit to a comparable text form.</summary>
/// <param name="edit">The edit.</param>
/// <returns>The text.</returns>
let private editText (edit: RunEdit) =
    match edit.Kind with
    | EditKind.ClearOutput -> "clear"
    | EditKind.NextGlyph -> "next"
    | EditKind.NextGlyphs -> $"nexts{edit.W0}"
    | EditKind.ChargeOps -> $"charge{edit.W0}"
    | EditKind.UnsafeToBreakFromOutbuffer -> $"unsafe{edit.W0},{edit.W1}"
    | EditKind.UnsafeToBreak -> $"unsafeb{edit.W0},{edit.W1}"
    | EditKind.Finish -> "finish"
    | other -> $"kind{int other}"

/// <summary>Builds the run fields the out-buffer machine reads.</summary>
/// <param name="codepoints">The code points.</param>
/// <param name="clusters">The clusters.</param>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The run fields.</returns>
let private runInputOf (codepoints: uint32 array) (clusters: uint32 array) (maxOps: int) =
    let infos = Sealed.init codepoints.Length (fun i -> { Codepoint = codepoints[i]; Mask = 0u; Cluster = clusters[i]; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init codepoints.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
    { Infos = infos
      Props = props
      Positions = ImmutableArray.Empty
      HavePositions = false
      MaxLen = GlyphRun.MaxLenDefault
      MaxOps = maxOps
      Successful = true
      ClusterLevel = ClusterLevel.MonotoneGraphemes
      Flags = BufferFlags.Default
      Serial = 0uy
      RandomState = 1u
      ScratchFlags = ScratchFlags.Default }

/// <summary>Runs the driver over a text and returns its raw edit sequence.</summary>
/// <typeparam name="'Kind">The subtable kind.</typeparam>
/// <typeparam name="'Context">The kind's transition context.</typeparam>
/// <param name="config">The driver config.</param>
/// <param name="kind">The kind.</param>
/// <param name="seed">The initial state.</param>
/// <param name="codepoints">The code points.</param>
/// <param name="clusters">The clusters.</param>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The edits.</returns>
let private runEditList<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
    (config: DriverConfig) (kind: 'Kind) (seed: DriverState<'Context>) (codepoints: uint32 array) (clusters: uint32 array) (maxOps: int) =
    let edits = ResizeArray<RunEdit>()
    let input = runInputOf codepoints clusters maxOps
    let pass = RecordingPass(config, kind, edits)
    Sealed.runPassWithState input pass seed |> ignore
    edits.ToArray()

/// <summary>Runs the driver over a text and returns its edit sequence as text.</summary>
/// <typeparam name="'Kind">The subtable kind.</typeparam>
/// <typeparam name="'Context">The kind's transition context.</typeparam>
/// <param name="config">The driver config.</param>
/// <param name="kind">The kind.</param>
/// <param name="seed">The initial state.</param>
/// <param name="codepoints">The code points.</param>
/// <param name="clusters">The clusters.</param>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The edits.</returns>
let private runEdits<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct>
    (config: DriverConfig) (kind: 'Kind) (seed: DriverState<'Context>) (codepoints: uint32 array) (clusters: uint32 array) (maxOps: int) =
    runEditList config kind seed codepoints clusters maxOps |> Array.map editText

/// <summary>Expands every <c>NextGlyphs n</c> into n <c>NextGlyph</c> edits, so a batched driver can be compared with
/// a literal one-glyph-per-step transliteration.</summary>
/// <param name="edits">The edits.</param>
/// <returns>The expanded edits.</returns>
let private expandEdits (edits: RunEdit array) : RunEdit array =
    edits
    |> Array.collect (fun edit ->
        match edit.Kind with
        | EditKind.NextGlyphs -> Array.init (int edit.W0) (fun _ -> RunEdit.NextGlyph)
        | _ -> [| edit |])

/// <summary>
/// A literal transliteration of StateTableDriver::drive (src/hb-aat-layout-common.hh:1282-1442) for an in-place kind
/// whose transition is a no-op: one glyph per iteration, the null-transition fast path written as upstream writes it,
/// and no stretch of any kind. The budget and success flag are carried because the epsilon transition and the
/// unsafe-to-break edit both charge them.
/// </summary>
/// <param name="machine">The machine.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="ranges">The compiled ranges.</param>
/// <param name="subtableFlags">The subtable flags.</param>
/// <param name="isActionable">The action predicate.</param>
/// <param name="codepoints">The code points.</param>
/// <param name="clusters">The clusters.</param>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The edits.</returns>
let private referenceDrive
    (machine: StateMachine) (numGlyphs: int) (ranges: ImmutableArray<RangeFlags>) (subtableFlags: uint32)
    (isActionable: Entry -> bool) (codepoints: uint32 array) (clusters: uint32 array) (maxOps: int)
    =
    let edits = ResizeArray<RunEdit>()
    let len = codepoints.Length
    let classOf g = StateTable.classOfCall machine g numGlyphs
    let getEntry s k = StateTable.entryCall machine s k
    let hasRanges = ranges.Length > 1
    let startStateSafeToBreakEot = not (isActionable (getEntry StateTable.StartOfText StateTable.ClassEndOfText))
    let mutable state = 0
    let mutable rangeIdx = 0
    let mutable idx = 0
    let mutable ops = maxOps
    let mutable successful = true
    let mutable finished = false
    while not finished && successful do
        let mutable klass = if idx < len then classOf codepoints[idx] else StateTable.ClassEndOfText
        let mutable resume = true
        while resume && not finished && successful do
            resume <- false
            let entry = getEntry state klass
            let nextState = entry.NewState
            let isNotEpsilon = entry.Flags &&& DontAdvanceFlag = 0us
            let isNotActionable = not (isActionable entry)
            let mutable proceed = false
            if hasRanges then
                let mutable range = rangeIdx
                if idx < len then
                    let cluster = clusters[idx]
                    while cluster < ranges[range].ClusterFirst do range <- range - 1
                    while cluster > ranges[range].ClusterLast do range <- range + 1
                    rangeIdx <- range
                if ranges[range].Flags &&& subtableFlags = 0u then
                    if idx = len then finished <- true
                    else
                        state <- 0
                        edits.Add RunEdit.NextGlyph
                        idx <- idx + 1
                        klass <- if idx < len then classOf codepoints[idx] else StateTable.ClassEndOfText
                        resume <- true
                else proceed <- true
            else
                let isNullTransition =
                    state = StateTable.StartOfText
                    && nextState = StateTable.StartOfText
                    && startStateSafeToBreakEot
                    && isNotActionable
                    && isNotEpsilon
                if isNullTransition then
                    let oldKlass = klass
                    let mutable stopping = false
                    while not stopping do
                        if idx = len || not successful then stopping <- true
                        else
                            edits.Add RunEdit.NextGlyph
                            idx <- idx + 1
                            klass <- if idx < len then classOf codepoints[idx] else StateTable.ClassEndOfText
                            if klass <> oldKlass then stopping <- true
                    if idx = len || not successful then finished <- true
                    else resume <- true
                else proceed <- true
            if proceed then
                let wouldBe = getEntry StateTable.StartOfText klass
                let sameAsStart =
                    not (isActionable wouldBe)
                    && nextState = wouldBe.NewState
                    && (entry.Flags &&& DontAdvanceFlag) = (wouldBe.Flags &&& DontAdvanceFlag)
                let isSafe =
                    isNotActionable
                    && (state = StateTable.StartOfText
                        || (entry.Flags &&& DontAdvanceFlag <> 0us && nextState = StateTable.StartOfText)
                        || sameAsStart)
                    && not (isActionable (getEntry state StateTable.ClassEndOfText))
                if not isSafe && idx > 0 && idx < len then
                    edits.Add (RunEdit.UnsafeToBreakFromOutbuffer (idx - 1, idx + 1))
                    ops <- ops - 2
                    if ops < 0 then successful <- false
                state <- nextState
                if idx = len then finished <- true
                elif isNotEpsilon then
                    edits.Add RunEdit.NextGlyph
                    idx <- idx + 1
                else
                    let budgetBefore = ops
                    ops <- ops - 1
                    edits.Add (RunEdit.ChargeOps 1)
                    if budgetBefore <= 0 then
                        edits.Add RunEdit.NextGlyph
                        idx <- idx + 1
    edits.Add RunEdit.Finish
    edits |> Seq.map editText |> Seq.toArray

/// <summary>A random machine (4-6 classes, 1-5 states) with one entry per state-and-class pair.</summary>
let private machineGen =
    gen {
        let! nClasses = Gen.elements [ 4; 5; 6 ]
        let! nStates = Gen.choose (1, 5)
        let entryGen =
            gen {
                let! newState = Gen.choose (0, nStates - 1)
                let! dontAdvance = Gen.elements [ false; true ]
                let! actionable = Gen.elements [ false; true ]
                let flags = (if dontAdvance then DontAdvanceFlag else 0us) ||| (if actionable then ActionableFlag else 0us)
                return { NewState = newState; Flags = flags; Data0 = 0us; Data1 = 0us }
            }
        let! entries = Gen.arrayOfLength (nStates * nClasses) entryGen
        let! count = Gen.choose (0, 20)
        let! codepoints = Gen.arrayOfLength count (Gen.choose (0, nClasses - 1) |> Gen.map uint32)
        return (machineFromEntries nClasses entries, codepoints)
    }

/// <summary>The P8.S04 driver unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S04" [
        test "end-of-text action from the start state" {
            let machine =
                machineFromEntries 4 [|
                    entry 0 ActionableFlag
                    entry 0 0us
                    entry 0 0us
                    entry 0 0us
                |]
            let config = configOf machine ImmutableArray.Empty 0u 100
            let edits = runEdits config (MarkerKind(RunEdit.ChargeOps 7)) seedInt [||] [||] GlyphRun.MaxOpsDefault
            Expect.equal edits [| "charge7"; "finish" |] "the end-of-text transition runs before finishing"
        }
        test "a DontAdvance loop ends when the budget is exhausted" {
            let machine =
                machineFromEntries 4 [|
                    entry 0 0us
                    entry 0 DontAdvanceFlag
                    entry 0 0us
                    entry 0 0us
                |]
            let config = configOf machine ImmutableArray.Empty 0u 100
            let edits = runEdits config (NoopKind()) seedInt [| 1u |] [| 0u |] 3
            Expect.equal edits [| "charge1"; "charge1"; "charge1"; "charge1"; "next"; "finish" |] "four charges then a forced advance"
        }
        test "range-flag resets skip the transition and reset the state" {
            let machine =
                machineFromEntries 4 [|
                    entry 0 0us
                    entry 0 ActionableFlag
                    entry 0 0us
                    entry 0 0us
                |]
            let ranges =
                Sealed.init 2 (fun i ->
                    match i with
                    | 0 -> { Flags = 0x0001u; ClusterFirst = 0u; ClusterLast = 4u }
                    | _ -> { Flags = 0x0000u; ClusterFirst = 5u; ClusterLast = UInt32.MaxValue })
            let config = configOf machine ranges 0x0001u 100
            let edits = runEdits config (MarkerKind(RunEdit.ChargeOps 7)) seedInt [| 1u; 1u |] [| 0u; 5u |] GlyphRun.MaxOpsDefault
            Expect.equal edits [| "charge7"; "next"; "next"; "finish" |] "the non-matching range glyph is skipped without a transition"
        }
        test "unsafe-to-break flags are written on a crafted machine" {
            let machine =
                machineFromEntries 4 [|
                    entry 0 0us
                    entry 0 ActionableFlag
                    entry 0 0us
                    entry 0 0us
                |]
            let config = configOf machine ImmutableArray.Empty 0u 100
            let codepoints = [| 1u; 1u |]
            let infos = Sealed.init 2 (fun i -> { Codepoint = codepoints[i]; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 2 (fun _ -> Unchecked.defaultof<GlyphProps>)
            let run = { GlyphRun.empty with Infos = infos; Props = props }
            let result = GlyphRun.runPass { Config = config; Kind = NoopKind() } seedInt run
            Expect.equal result.Infos[0].Mask 0u "the first glyph is the min-cluster glyph and stays untouched"
            Expect.equal result.Infos[1].Mask 3u "the second glyph is marked unsafe to break and concat"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 } "P8.S04 drive equals a literal transliteration of drive" <|
            Prop.forAll (Arb.fromGen machineGen) (fun (machine, codepoints) ->
                let config = configOf machine ImmutableArray.Empty 0u 100
                let clusters = Array.init codepoints.Length uint32
                let ours =
                    expandEdits (runEditList config (NoopKind()) seedInt codepoints clusters 50) |> Array.map editText
                let reference = referenceDrive machine 100 ImmutableArray.Empty 0u isActionableEntry codepoints clusters 50
                Expect.equal ours reference "drive edits"
                true)
        test "P8.S04 the driver allocates nothing over the null-transition machine" {
            let machine =
                machineFromEntries 5 [|
                    entry 0 0us
                    entry 0 0us
                    entry 0 0us
                    entry 0 0us
                    entry 0 0us
                |]
            let config = configOf machine ImmutableArray.Empty 0u 100
            let codepoints = Array.init 1000 (fun _ -> 4u)
            let clusters = Array.init 1000 uint32
            let input = runInputOf codepoints clusters GlyphRun.MaxOpsDefault
            let pass = { Config = config; Kind = NoopKind() }
            let measure count =
                let before = GC.GetAllocatedBytesForCurrentThread()
                let mutable i = 0
                while i < count do
                    Sealed.runPassWithState input pass seedInt |> ignore
                    i <- i + 1
                GC.GetAllocatedBytesForCurrentThread() - before
            let mutable warm = 0
            while warm < 1000 do
                Sealed.runPassWithState input pass seedInt |> ignore
                warm <- warm + 1
            Expect.equal (measure 1000) 0L "a null-transition pass allocates nothing after warm-up"
        }
        test "P8.S04 a stable non-actionable stretch is one batch" {
            let machine =
                machineFromEntries 5 (Array.init 10 (fun _ -> entry 1 0us))
            let config = configOf machine ImmutableArray.Empty 0u 100
            let codepoints = Array.init 10 (fun _ -> 4u)
            let edits = runEdits config (NoopKind()) seedInt codepoints (Array.init 10 uint32) GlyphRun.MaxOpsDefault
            Expect.equal edits [| "next"; "nexts9"; "finish" |] "the stable non-actionable stretch is one NextGlyphs batch"
        }
        test "P8.S04 the driver state is 32 bytes" {
            Expect.equal (Unsafe.SizeOf<DriverState<int>>()) 32 "DriverState<int> must fit in 32 bytes"
        }
    ]
