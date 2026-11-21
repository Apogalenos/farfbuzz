module Farfbuzz.Diff.RunPassDiff

open System
open System.Threading
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>The machine model the op-script generator maintains.</summary>
type private OpModel =
    { Len: int
      Idx: int
      OutLen: int
      HaveOutput: bool }

/// <summary>The generated input: a seeded run and an op script.</summary>
type private OpState =
    { Codepoints: uint32 array
      Clusters: uint32 array
      Level: int
      Flags: uint32
      InitialMaxOps: int
      Ops: uint32 array }

/// <summary>The compared final state.</summary>
type private OutState =
    { Codepoints: uint32 array
      Masks: uint32 array
      Clusters: uint32 array
      Length: int
      Successful: int }

/// <summary>The counters for one op-script differential test run.</summary>
type private Counters =
    { mutable ScriptsWhole: int
      mutable ScriptsCut: int
      mutable OpsGenerated: int
      mutable OpsKept: int }

/// <summary>Converts an op-script end argument to the -1 sentinel.</summary>
/// <param name="value">The raw value.</param>
/// <returns>-1 for 0xFFFFFFFF, the value otherwise.</returns>
let private endOf (value: uint32) =
    match value = UInt32.MaxValue with
    | true -> -1
    | false -> int value

/// <summary>Decodes one op of the P3.S09 op script into an edit.</summary>
/// <param name="ops">The op stream.</param>
/// <param name="index">The next stream index.</param>
/// <returns>The edit and the next index.</returns>
let rec private decodeOp (ops: uint32 array) (index: int) : struct (RunEdit * int) =
    match ops[index] with
    | 1u -> struct (RunEdit.ClearOutput, index + 1)
    | 2u -> struct (RunEdit.NextGlyph, index + 1)
    | 3u -> struct (RunEdit.ReplaceGlyph ops[index + 1], index + 2)
    | 4u -> struct (RunEdit.OutputGlyph ops[index + 1], index + 2)
    | 5u -> struct (RunEdit.SkipGlyph, index + 1)
    | 6u -> struct (RunEdit.DeleteGlyph, index + 1)
    | 7u -> struct (RunEdit.CopyGlyph, index + 1)
    | 8u -> struct (RunEdit.MoveTo (int ops[index + 1]), index + 2)
    | 9u -> struct (RunEdit.MergeClusters (int ops[index + 1], int ops[index + 2]), index + 3)
    | 10u -> struct (RunEdit.MergeOutClusters (int ops[index + 1], int ops[index + 2]), index + 3)
    | 11u -> struct (RunEdit.UnsafeToBreak (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 12u -> struct (RunEdit.UnsafeToConcat (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 13u -> struct (RunEdit.UnsafeToBreakFromOutbuffer (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 14u -> struct (RunEdit.UnsafeToConcatFromOutbuffer (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 15u -> struct (RunEdit.Sync, index + 1)
    | 16u ->
        let produced = Sealed.init (int ops[index + 2]) (fun i -> ops[index + 3 + i])
        struct (RunEdit.ReplaceGlyphs (int ops[index + 1], produced), index + 3 + produced.Length)
    | 17u -> struct (RunEdit.SetMasks (ops[index + 1], ops[index + 2], ops[index + 3], ops[index + 4]), index + 5)
    | 18u -> struct (RunEdit.NextGlyphs (int ops[index + 1]), index + 2)
    | 19u -> struct (RunEdit.SafeToInsertTatweel (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | _ -> struct (RunEdit.Finish, index + 1)

/// <summary>A pass that replays a P3.S09 op script.</summary>
[<Struct>]
type ScriptPass =
    val private Ops: uint32 array
    /// <summary>Creates a script pass.</summary>
    /// <param name="ops">The op stream.</param>
    new (ops: uint32 array) = { Ops = ops }
    interface IRunPass<int> with
        member this.Step (_, index) =
            match index >= this.Ops.Length with
            | true -> struct (RunEdit.Finish, index)
            | false -> decodeOp this.Ops index

/// <summary>Generates a flag range over the buffer, with an open-ended option.</summary>
/// <param name="upper">The inclusive upper bound.</param>
/// <returns>The start and end values.</returns>
let private flagRange (upper: int) =
    gen {
        let! start = Gen.choose (0, max 0 upper)
        let! finish = Gen.choose (start, max start upper)
        let! openEnded = Gen.elements [ true; false ]
        return struct (start, match openEnded with | true -> UInt32.MaxValue | false -> uint32 finish)
    }

/// <summary>Generates an output-buffer flag range, respecting the <c>start &lt;= out_len</c> and <c>idx &lt;= end</c> asserts.</summary>
/// <param name="outLen">The output length.</param>
/// <param name="idx">The input cursor.</param>
/// <param name="len">The input length.</param>
/// <returns>The start and end values.</returns>
let private outBufferRange (outLen: int) (idx: int) (len: int) =
    gen {
        let! start = Gen.choose (0, max 0 outLen)
        let! finish = Gen.choose (idx, max idx len)
        let! openEnded = Gen.elements [ true; false ]
        return struct (start, match openEnded with | true -> UInt32.MaxValue | false -> uint32 finish)
    }

/// <summary>Generates one valid op from the model, returning its encoding and the next model.</summary>
/// <param name="model">The model.</param>
/// <returns>The op encoding and the next model.</returns>
let private genValidOp (model: OpModel) : Gen<struct (uint32 array * OpModel)> =
    let candidates = ResizeArray<Gen<struct (uint32 array * OpModel)>>()
    candidates.Add(gen { return struct ([| 1u |], { model with HaveOutput = true; Idx = 0; OutLen = 0 }) })
    match model.Idx < model.Len with
    | true ->
        candidates.Add(gen { return struct ([| 2u |], { model with Idx = model.Idx + 1; OutLen = if model.HaveOutput then model.OutLen + 1 else model.OutLen }) })
        candidates.Add(gen {
            let! count = Gen.choose (1, model.Len - model.Idx)
            return struct ([| 18u; uint32 count |], { model with Idx = model.Idx + count; OutLen = if model.HaveOutput then model.OutLen + count else model.OutLen })
        })
        candidates.Add(gen { return struct ([| 5u |], { model with Idx = model.Idx + 1 }) })
        candidates.Add(gen { return struct ([| 6u |], { model with Idx = model.Idx + 1 }) })
    | false -> ()
    match model.HaveOutput, model.Idx < model.Len with
    | true, true ->
        candidates.Add(gen { let! g = Gen.choose (0, 4095) |> Gen.map uint32 in return struct ([| 3u; g |], { model with Idx = model.Idx + 1; OutLen = model.OutLen + 1 }) })
        candidates.Add(gen {
            let! nIn = Gen.choose (1, model.Len - model.Idx)
            let! nOut = Gen.choose (0, 3)
            let! produced = Gen.listOfLength nOut (Gen.choose (0, 4095) |> Gen.map uint32)
            let encoding = Array.concat [| [| 16u; uint32 nIn; uint32 nOut |]; Array.ofList produced |]
            return struct (encoding, { model with Idx = model.Idx + nIn; OutLen = model.OutLen + nOut })
        })
        candidates.Add(gen { return struct ([| 7u |], { model with OutLen = model.OutLen + 1 }) })
    | _ -> ()
    match model.HaveOutput with
    | true ->
        candidates.Add(gen { let! g = Gen.choose (0, 4095) |> Gen.map uint32 in return struct ([| 4u; g |], { model with OutLen = model.OutLen + 1 }) })
        match model.OutLen >= 2 with
        | true ->
            candidates.Add(gen {
                let! s = Gen.choose (0, model.OutLen - 2)
                let! e = Gen.choose (s + 1, model.OutLen - 1)
                return struct ([| 10u; uint32 s; uint32 e |], model)
            })
        | false -> ()
        candidates.Add(gen {
            let! struct (s, e) = outBufferRange model.OutLen model.Idx model.Len
            return struct ([| 13u; uint32 s; e |], model)
        })
        candidates.Add(gen {
            let! struct (s, e) = outBufferRange model.OutLen model.Idx model.Len
            return struct ([| 14u; uint32 s; e |], model)
        })
        let moveLo = max 0 (model.OutLen - model.Idx)
        let moveHi = model.OutLen + (model.Len - model.Idx)
        match moveLo <= moveHi with
        | true ->
            candidates.Add(gen {
                let! target = Gen.choose (moveLo, moveHi)
                return struct ([| 8u; uint32 target |], { model with Idx = model.Idx + (target - model.OutLen); OutLen = target })
            })
        | false -> ()
        candidates.Add(gen { return struct ([| 15u |], { Len = model.OutLen + (model.Len - model.Idx); Idx = 0; OutLen = 0; HaveOutput = false }) })
    | false ->
        match model.Len >= 0 with
        | true ->
            candidates.Add(gen {
                let! target = Gen.choose (0, model.Len)
                return struct ([| 8u; uint32 target |], { model with Idx = target })
            })
        | false -> ()
    match model.Len >= 2 with
    | true ->
        candidates.Add(gen {
            let! s = Gen.choose (0, model.Len - 2)
            let! e = Gen.choose (s + 1, model.Len - 1)
            return struct ([| 9u; uint32 s; uint32 e |], model)
        })
    | false -> ()
    candidates.Add(gen {
        let! struct (s, e) = flagRange (model.Len - 1)
        return struct ([| 11u; uint32 s; e |], model)
    })
    candidates.Add(gen {
        let! struct (s, e) = flagRange (model.Len - 1)
        return struct ([| 12u; uint32 s; e |], model)
    })
    candidates.Add(gen {
        let! struct (s, e) = flagRange (model.Len - 1)
        return struct ([| 19u; uint32 s; e |], model)
    })
    candidates.Add(gen {
        let! value = Gen.choose (0, 7) |> Gen.map uint32
        let! mask = Gen.choose (0, 7) |> Gen.map uint32
        let! cs = Gen.choose (0, max 0 model.Len) |> Gen.map uint32
        let! ce = Gen.elements [ uint32 (model.Len); UInt32.MaxValue ]
        return struct ([| 17u; value; mask; cs; ce |], model)
    })
    Gen.oneof (List.ofSeq candidates)

/// <summary>Generates an op script from a model.</summary>
/// <param name="model">The model.</param>
/// <param name="remaining">The number of ops left.</param>
/// <returns>The op stream.</returns>
let rec private genScript (model: OpModel) (remaining: int) : Gen<uint32 array> =
    gen {
        match remaining <= 0 with
        | true -> return [||]
        | false ->
            let! struct (encoding, next) = genValidOp model
            let! rest = genScript next (remaining - 1)
            return Array.append encoding rest
    }

/// <summary>Generates a run and an op script, with a given initial operation budget.</summary>
/// <param name="initialMaxOps">The initial budget generator.</param>
/// <returns>The input state.</returns>
let private genRun (initialMaxOps: Gen<int>) : Gen<OpState> =
    gen {
        let! length = Gen.choose (0, 30)
        let! codepoints = Gen.listOfLength length (Gen.elements [ 0u; 1u; 2u; 3u; 0x41u; 100u; 999u ])
        let! sorted = Gen.elements [ true; false ]
        let! rawClusters = Gen.listOfLength length (Gen.choose (0, 8) |> Gen.map uint32)
        let clusters = match sorted with | true -> rawClusters |> List.sort |> Array.ofList | false -> Array.ofList rawClusters
        let! level = Gen.choose (0, 3)
        let! flagBits = Gen.choose (0, 3)
        let flags = match flagBits with | 0 -> 0u | 1 -> 0x40u | 2 -> 0x80u | _ -> 0xC0u
        let! maxOps = initialMaxOps
        let! opCount = Gen.choose (1, 60)
        let! ops = genScript { Len = length; Idx = 0; OutLen = 0; HaveOutput = false } opCount
        return { Codepoints = Array.ofList codepoints; Clusters = clusters; Level = level; Flags = flags; InitialMaxOps = maxOps; Ops = ops }
    }

/// <summary>The cluster level of a generated value.</summary>
/// <param name="level">The value 0 to 3.</param>
/// <returns>The cluster level.</returns>
let private clusterLevelOf (level: int) =
    match level with
    | 0 -> ClusterLevel.MonotoneGraphemes
    | 1 -> ClusterLevel.MonotoneCharacters
    | 2 -> ClusterLevel.Characters
    | _ -> ClusterLevel.Graphemes

/// <summary>The buffer flags of a generated value.</summary>
/// <param name="flags">The raw flags.</param>
/// <returns>The buffer flags.</returns>
let private flagsOf (flags: uint32) : BufferFlags =
    LanguagePrimitives.EnumOfValue flags

/// <summary>Builds a run from the generated state.</summary>
/// <param name="state">The input state.</param>
/// <returns>The run.</returns>
let private makeRun (state: OpState) =
    let infos =
        state.Codepoints
        |> Array.mapi (fun index codepoint -> { Codepoint = codepoint; Mask = 0u; Cluster = state.Clusters[index]; Var1 = 0u; Var2 = 0u })
    { GlyphRun.empty with
        Infos = Sealed.ofSpan (ReadOnlySpan infos)
        ClusterLevel = clusterLevelOf state.Level
        Flags = flagsOf state.Flags
        MaxOps = state.InitialMaxOps }

/// <summary>Reads the final state of a Farfbuzz run.</summary>
/// <param name="run">The run.</param>
/// <returns>The state.</returns>
let private ourState (run: GlyphRun) =
    { Codepoints = run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)
      Masks = run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Mask)
      Clusters = run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Cluster)
      Length = run.Infos.Length
      Successful = match run.Successful with | true -> 1 | false -> 0 }

/// <summary>Seeds an oracle buffer from the generated state.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="state">The input state.</param>
let private seed (buffer: nativeint) (state: OpState) =
    Native.farfbuzz_oracle_buffer_seed(buffer, state.Codepoints, state.Clusters, uint32 state.Codepoints.Length, [||], 0, [||], 0u, [||], 0u, 1u)
    Native.hb_buffer_set_cluster_level(buffer, state.Level)
    Native.hb_buffer_set_flags(buffer, state.Flags)

/// <summary>Runs an op script through the oracle and reads the final state.</summary>
/// <param name="state">The input state.</param>
/// <returns>The final state.</returns>
let private oracleState (state: OpState) =
    let buffer = Native.hb_buffer_create ()
    seed buffer state
    let successful = Native.farfbuzz_oracle_buffer_run_script(buffer, state.InitialMaxOps, state.Ops, uint32 state.Ops.Length)
    let count = max 1 (int (Native.hb_buffer_get_length buffer))
    let codepoints = Array.zeroCreate count
    let clusters = Array.zeroCreate count
    let masks = Array.zeroCreate count
    let before = Array.zeroCreate 5
    let after = Array.zeroCreate 5
    let mutable length = 0u
    let mutable beforeLength = 0u
    let mutable afterLength = 0u
    let mutable content = 0u
    Native.farfbuzz_oracle_buffer_state(buffer, codepoints, clusters, before, after, &length, &beforeLength, &afterLength, &content)
    Native.farfbuzz_oracle_buffer_masks(buffer, masks)
    Native.hb_buffer_destroy buffer
    { Codepoints = codepoints[0 .. int length - 1]
      Masks = masks[0 .. int length - 1]
      Clusters = clusters[0 .. int length - 1]
      Length = int length
      Successful = successful }

/// <summary>The stream index after <paramref name="remaining"/> ops.</summary>
/// <param name="ops">The op stream.</param>
/// <param name="index">The current stream index.</param>
/// <param name="remaining">How many more ops to skip.</param>
/// <returns>The index after the ops.</returns>
let rec private cutIndex (ops: uint32 array) (index: int) (remaining: int) : int =
    match remaining <= 0 with
    | true -> index
    | false ->
        let struct (_, next) = decodeOp ops index
        cutIndex ops next (remaining - 1)

/// <summary>Truncates an op stream to its first <paramref name="k"/> ops.</summary>
/// <param name="ops">The op stream.</param>
/// <param name="k">The number of leading ops to keep.</param>
/// <returns>The truncated stream.</returns>
let private cutOps (ops: uint32 array) (k: int) : uint32 array =
    ops[0 .. cutIndex ops 0 k - 1]

/// <summary>The number of ops in a stream.</summary>
/// <param name="ops">The op stream.</param>
/// <param name="index">The current stream index.</param>
/// <param name="count">The ops counted so far.</param>
/// <returns>The op count.</returns>
let rec private opCount (ops: uint32 array) (index: int) (count: int) : int =
    match index >= ops.Length with
    | true -> count
    | false ->
        let struct (_, next) = decodeOp ops index
        opCount ops next (count + 1)

/// <summary>The ordinary op-script differential input.</summary>
let private opScriptInput = genRun (Gen.constant GlyphRun.MaxOpsDefault)

/// <summary>The small-budget op-script differential input.</summary>
let private smallBudgetInput = genRun (Gen.elements [ 1; 2; 3; 4; 8 ])

/// <summary>Runs an op script through Farfbuzz.</summary>
/// <param name="state">The input state.</param>
/// <returns>The final state.</returns>
let private ours (state: OpState) =
    let run = GlyphRun.enter (makeRun state)
    GlyphRun.runPass (ScriptPass(state.Ops)) 0 run |> ourState

/// <summary>Compares Farfbuzz and the oracle on one script, cutting it when the shim rejects an op.</summary>
/// <param name="counters">The counters for the test this script belongs to.</param>
/// <param name="input">The input state.</param>
/// <returns>The comparison property.</returns>
let private compareScript (counters: Counters) (input: OpState) : Property =
    Interlocked.Add(&counters.OpsGenerated, opCount input.Ops 0 0) |> ignore
    let buffer = Native.hb_buffer_create ()
    seed buffer input
    let full = Native.farfbuzz_oracle_buffer_run_script(buffer, input.InitialMaxOps, input.Ops, uint32 input.Ops.Length)
    Native.hb_buffer_destroy buffer
    let cut, wasCut =
        match full < 0 with
        | true -> cutOps input.Ops (-full - 1), true
        | false -> input.Ops, false
    Interlocked.Add(&counters.OpsKept, opCount cut 0 0) |> ignore
    match wasCut with
    | true -> Interlocked.Increment(&counters.ScriptsCut) |> ignore
    | false -> Interlocked.Increment(&counters.ScriptsWhole) |> ignore
    let expected = oracleState { input with Ops = cut }
    let actual = ours { input with Ops = cut }
    match actual = expected with
    | true -> Prop.ofTestable true
    | false -> Prop.ofTestable false |> Prop.label $"input: %A{input}\n  oracle:   %A{expected}\n  farfbuzz: %A{actual}"

/// <summary>Runs an op-script differential and reports how much of each script was compared.</summary>
/// <param name="name">The test name.</param>
/// <param name="inputs">The input generator.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let private scriptTest (name: string) (inputs: Gen<OpState>) (count: int) =
    testCase name (fun () ->
        let counters =
            { ScriptsWhole = 0
              ScriptsCut = 0
              OpsGenerated = 0
              OpsKept = 0 }
        let cfg = config count
        let fsConfig =
            FsCheck.Config.QuickThrowOnFailure
                .WithMaxTest(cfg.maxTest)
                .WithStartSize(cfg.startSize)
                .WithEndSize(cfg.endSize)
                .WithQuietOnSuccess(true)
        FsCheck.Check.One(fsConfig, Prop.forAll (Arb.fromGen inputs) (compareScript counters))
        printfn $"P3.S09 {name}: {counters.ScriptsWhole + counters.ScriptsCut} scripts, {counters.ScriptsWhole} whole, {counters.ScriptsCut} cut, {counters.OpsKept} of {counters.OpsGenerated} ops compared")

/// <summary>The P3.S09 differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S09" [
        scriptTest "op scripts" opScriptInput 20_000
        scriptTest "small budgets" smallBudgetInput 20_000
    ]