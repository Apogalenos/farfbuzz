module Farfbuzz.Bench.AatCost

open System
open System.Collections.Immutable
open System.Diagnostics
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.OpenType
open Farfbuzz.Bench.RunPassCost

/// <summary>The glyphs of the synthetic run.</summary>
[<Literal>]
let private RunGlyphs = 10_000

/// <summary>The number of repetitions whose minimum is reported.</summary>
[<Literal>]
let private Repeats = 15

/// <summary>Times an action: the minimum of several repetitions after a warm-up, and the bytes one repetition allocates.</summary>
/// <param name="action">The action.</param>
/// <returns>Nanoseconds and bytes of one call.</returns>
let private measure (action: unit -> int) =
    let warm = Stopwatch.StartNew()
    while warm.ElapsedMilliseconds < 500L do action () |> ignore
    let mutable best = Int64.MaxValue
    for _ in 1..Repeats do
        let before = Stopwatch.GetTimestamp()
        action () |> ignore
        let elapsed = Stopwatch.GetTimestamp() - before
        if elapsed < best then best <- elapsed
    let bytesBefore = GC.GetAllocatedBytesForCurrentThread()
    action () |> ignore
    let bytes = GC.GetAllocatedBytesForCurrentThread() - bytesBefore
    struct (float best * 1.0e9 / float Stopwatch.Frequency, bytes)

/// <summary>Prints one row.</summary>
/// <param name="name">The case.</param>
/// <param name="action">The call, over 10,000 glyphs.</param>
let private row (name: string) (action: unit -> int) =
    let struct (ns, bytes) = measure action
    Console.WriteLine(String.Format("| {0} | {1,9:N1} ns/glyph | {2,8:N1} B/glyph |", name, ns / float RunGlyphs, float bytes / float RunGlyphs))

/// <summary>Builds the kernel call of the pass a benchmark builds, with its table compiled once.</summary>
/// <param name="mask">The null-transition mask of the kind.</param>
/// <param name="quiet">Whether the null transition stands in for the transition of an entry that is not actionable.</param>
/// <param name="built">The pass, its seed and its run.</param>
/// <returns>The call, returning the glyph count.</returns>
let private kernelOf (mask: uint16) (quiet: bool) (built: struct (StateMachinePass<'K, 'C> * DriverState<'C> * GlyphRun)) =
    let struct (pass, seed, run) = built
    let dense = DenseMachine.build pass.Config.Machine pass.Config.NumGlyphs pass.Kind
    fun () ->
        let struct (result, _) = AatKernel.run Kernel dense pass.Config pass.Kind mask quiet seed.Context run
        result.Infos.Length

/// <summary>A kind whose transition does nothing: the cost of the driver and of one transition call, without the work.</summary>
[<Struct>]
type private DroppedKind<'K, 'C when 'K : struct and 'K :> IStateMachineKind<'C> and 'C : struct> =
    { Inner: 'K }
    interface IStateMachineKind<'C> with
        member this.InPlace = this.Inner.InPlace
        member this.IsActionable entry = this.Inner.IsActionable entry
        member this.IsActionInitiable entry = this.Inner.IsActionInitiable entry
        member _.Transition (_, _, context) = struct (Emission.Complete, context)
        member this.NullTransition (cursor, entry, context) = this.Inner.NullTransition (&cursor, entry, context)

/// <summary>Builds the kernel call of the pass a benchmark builds with its transition dropped.</summary>
/// <param name="mask">The null-transition mask of the kind.</param>
/// <param name="built">The pass, its seed and its run.</param>
/// <returns>The call, returning the glyph count.</returns>
let private droppedOf (mask: uint16) (built: struct (StateMachinePass<'K, 'C> * DriverState<'C> * GlyphRun)) =
    let struct (pass, seed, run) = built
    let kind: DroppedKind<'K, 'C> = { Inner = pass.Kind }
    let dense = DenseMachine.build pass.Config.Machine pass.Config.NumGlyphs kind
    fun () ->
        let struct (result, _) = AatKernel.run Kernel dense pass.Config kind mask true seed.Context run
        result.Infos.Length

/// <summary>Builds the direct contextual kernel call of the contextual pass a benchmark builds.</summary>
/// <param name="built">The pass, its seed and its run.</param>
/// <returns>The call, returning the glyph count.</returns>
let private contextualDirectOf (built: struct (StateMachinePass<ContextualKind, ContextualContext> * DriverState<ContextualContext> * GlyphRun)) =
    let struct (pass, _, run) = built
    let dense = DenseMachine.build pass.Config.Machine pass.Config.NumGlyphs pass.Kind
    fun () ->
        let struct (result, _, _) = MorxContextualDirect.run Kernel dense pass.Config pass.Kind false run
        result.Infos.Length

/// <summary>Builds the direct ligature kernel call of the ligature pass a benchmark builds.</summary>
/// <param name="built">The pass, its seed and its run.</param>
/// <returns>The call, returning the glyph count.</returns>
let private ligatureDirectOf (built: struct (StateMachinePass<LigatureKind, LigatureContext> * DriverState<LigatureContext> * GlyphRun)) =
    let struct (pass, _, run) = built
    let dense = DenseMachine.build pass.Config.Machine pass.Config.NumGlyphs pass.Kind
    fun () ->
        let struct (result, _, _) = MorxLigatureDirect.run Kernel dense pass.Config pass.Kind false run
        result.Infos.Length

/// <summary>Runs the interpreter on the pass a benchmark builds.</summary>
/// <param name="built">The pass, its seed and its run.</param>
/// <returns>The glyph count.</returns>
let private referenceOf (built: struct (StateMachinePass<'K, 'C> * DriverState<'C> * GlyphRun)) =
    let struct (pass, seed, run) = built
    (GlyphRun.runPass pass seed run).Infos.Length

/// <summary>Prints one comparison row: the interpreter and the kernel.</summary>
/// <param name="name">The case.</param>
/// <param name="reference">The interpreter call.</param>
/// <param name="kernel">The kernel call.</param>
let private pairRow (name: string) (reference: unit -> int) (kernel: unit -> int) =
    let struct (referenceNs, referenceBytes) = measure reference
    let struct (kernelNs, kernelBytes) = measure kernel
    Console.WriteLine(String.Format("| {0} | {1,9:N1} ns, {2,7:N1} B | {3,9:N1} ns, {4,7:N1} B |", name, referenceNs / float RunGlyphs, float referenceBytes / float RunGlyphs, kernelNs / float RunGlyphs, float kernelBytes / float RunGlyphs))

/// <summary>Prints the rows of the noncontextual subtable: the reference that builds the run again and the kernel that writes the
/// replacements in one loop, on a run whose every glyph is replaced and on one that nothing replaces.</summary>
let private noncontextualRows () =
    let infos = Sealed.init RunGlyphs (fun index -> { Codepoint = uint32 (index % 16); Mask = 3u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init RunGlyphs (fun _ -> Unchecked.defaultof<GlyphProps>)
    let run = { GlyphRun.empty with Infos = infos; Props = props; MaxOps = Int32.MaxValue }
    let be16 (value: uint32) = [| byte (value >>> 8); byte value |]
    let every = Array.concat [ be16 0u; Array.concat (Array.init 16 (fun glyph -> be16 (uint32 (glyph + 1)))) ]
    let nothing = Array.concat [ be16 8u; be16 0u; be16 0u ]
    let context: AatContext = { Gdef = Gdef.load Face.empty }
    let prepared (bytes: byte array) (runner: MachineRunner) =
        let lookup = AatLookup.read (ReadOnlyMemory bytes) 0 2
        let replacements = Morx.compileReplacements (ReadOnlyMemory bytes) 16
        fun () ->
            let struct (result, _) = MorxNoncontextualDirect.run runner replacements lookup 16 ImmutableArray.Empty 1u context false run
            result.Infos.Length
    pairRow "NoncontextualReplaceAll" (prepared every Reference) (prepared every Kernel)
    pairRow "Noncontextual replaces nothing" (prepared nothing Reference) (prepared nothing Kernel)

/// <summary>Calls the transition of a kind 10,000 times on one cursor, applying no edit: the cost of the transition alone.</summary>
/// <param name="built">The pass, its seed and its run.</param>
/// <param name="entry">The entry.</param>
/// <returns>A count, so the loop is kept.</returns>
let private transitionCalls (built: struct (StateMachinePass<'K, 'C> * DriverState<'C> * GlyphRun)) (entry: Entry) =
    let struct (pass, seed, run) = built
    let infos = run.Infos
    let props = run.Props
    let cursor = RunCursor(infos.AsSpan(), props.AsSpan(), 5, ReadOnlySpan.Empty, ReadOnlySpan.Empty, run.Positions.AsSpan(), false, ClusterLevel.MonotoneGraphemes, BufferFlags.Default, 0uy, 0u, 100000, true)
    let kind = pass.Kind
    let mutable total = 0
    let mutable index = 0
    while index < RunGlyphs do
        let struct (emission, _) = (kind :> IStateMachineKind<'C>).Transition(&cursor, entry, seed.Context)
        total <- total + (match emission with | Emission.Emit edit -> int edit.Kind | Emission.Complete -> 0)
        index <- index + 1
    total

/// <summary>Prints the RunPassCost rows of the AAT machines on the interpreter and on the kernel, and the step and edit
/// counts of each.</summary>
/// <param name="counts">Whether to print the tallies.</param>
let run (counts: bool) =
    let bench = RunPassCost()
    bench.Setup()
    Console.WriteLine("### AAT state machines, 10,000-glyph run, minimum of 15 repetitions, per glyph")
    Console.WriteLine("| case | edit interpreter | kernel |")
    Console.WriteLine("|---|---:|---:|")
    pairRow "matches nothing, contextual" (fun () -> bench.ContextualMatchNothing() |> int) (fun () -> bench.ContextualMatchNothing() |> int)
    pairRow "matches nothing, kerx format 1" (fun () -> bench.KerxFormat1MatchNothing() |> int) (fun () -> bench.KerxFormat1MatchNothing() |> int)
    pairRow "non-actionable, alternating classes" (fun () -> bench.NonActionableAlternatingRun() |> int) (kernelOf 0x8000us true (bench.PassAlternating()))
    let struct (alternatingPass, alternatingSeed, alternatingRun) = bench.PassAlternating()
    let flagged = { alternatingRun with Infos = Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = 3u }) (alternatingRun.Infos.AsSpan()) }
    pairRow "non-actionable, alternating classes, flags already set" (fun () -> (GlyphRun.runPass alternatingPass alternatingSeed flagged).Infos.Length) (kernelOf 0x8000us true (struct (alternatingPass, alternatingSeed, flagged)))
    pairRow "ContextualChangeAll" (fun () -> bench.ContextualChangeAll() |> int) (kernelOf 0x8000us false (bench.PassContextual()))
    pairRow "ContextualChangeAll, direct kernel" (fun () -> bench.ContextualChangeAll() |> int) (contextualDirectOf (bench.PassContextual()))
    pairRow "RearrangementChangeAll" (fun () -> bench.RearrangementChangeAll() |> int) (kernelOf 0xA000us true (bench.PassRearrangement()))
    pairRow "LigatureChangeAll" (fun () -> bench.LigatureChangeAll() |> int) (kernelOf 0x8000us true (bench.PassLigature()))
    pairRow "LigatureChangeAll, direct kernel" (fun () -> bench.LigatureChangeAll() |> int) (ligatureDirectOf (bench.PassLigature()))
    noncontextualRows ()
    pairRow "InsertionChangeAll" (fun () -> bench.InsertionChangeAll() |> int) (kernelOf 0x8000us false (bench.PassInsertion()))
    pairRow "KerxFormat0ChangeAll" (fun () -> bench.KerxFormat0ChangeAllReference() |> int) (fun () -> bench.KerxFormat0ChangeAll() |> int)
    pairRow "KerxFormat0ChangeAll, hashed pair index" (fun () -> bench.KerxFormat0ChangeAllReference() |> int) (fun () -> bench.KerxFormat0IndexedChangeAll() |> int)
    pairRow "KerxFormat0 matches nothing, hashed pair index" (fun () -> bench.KerxFormat0MatchNothingGeneral() |> int) (fun () -> bench.KerxFormat0IndexedMatchNothing() |> int)
    pairRow "KerxFormat1ChangeAll" (fun () -> bench.KerxFormat1ChangeAll() |> int) (kernelOf 0x8000us true (bench.PassKerxFormat1()))
    pairRow "KerxFormat4AttachAll" (fun () -> bench.KerxFormat4AttachAll() |> int) (kernelOf 0x8000us true (bench.PassKerxFormat4()))
    pairRow "  contextual, transition dropped (driver only)" (fun () -> 0) (droppedOf 0x8000us (bench.PassContextual()))
    pairRow "  ligature, transition dropped" (fun () -> 0) (droppedOf 0x8000us (bench.PassLigature()))
    pairRow "  kerx format 1, transition dropped" (fun () -> 0) (droppedOf 0x8000us (bench.PassKerxFormat1()))
    pairRow "  contextual, 10,000 Idle transition calls, no edits" (fun () -> 0) (fun () -> transitionCalls (bench.PassContextual()) { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 1us })
    pairRow "  ligature, 10,000 Idle transition calls, no edits" (fun () -> 0) (fun () -> transitionCalls (bench.PassLigature()) { NewState = 0; Flags = 0xA000us; Data0 = 0us; Data1 = 0us })
    Console.WriteLine()
    match counts with
    | false -> ()
    | true ->
        Console.WriteLine($"RunEdit = {Runtime.CompilerServices.Unsafe.SizeOf<RunEdit>()} B, RunCursor = {Runtime.CompilerServices.Unsafe.SizeOf<RunCursor>()} B")
        Console.WriteLine($"DriverState: contextual {Runtime.CompilerServices.Unsafe.SizeOf<DriverState<ContextualContext>>()} B, ligature {Runtime.CompilerServices.Unsafe.SizeOf<DriverState<LigatureContext>>()} B, insertion {Runtime.CompilerServices.Unsafe.SizeOf<DriverState<InsertionContext>>()} B, rearrangement {Runtime.CompilerServices.Unsafe.SizeOf<DriverState<RearrangementContext>>()} B, kerx1 {Runtime.CompilerServices.Unsafe.SizeOf<DriverState<KerxFormat1Context>>()} B, kerx4 {Runtime.CompilerServices.Unsafe.SizeOf<DriverState<KerxFormat4Context>>()} B")
        Console.WriteLine($"Gdef {Runtime.CompilerServices.Unsafe.SizeOf<Gdef>()} B, AatContext {Runtime.CompilerServices.Unsafe.SizeOf<AatContext>()} B, ContextualKind {Runtime.CompilerServices.Unsafe.SizeOf<ContextualKind>()} B, LigatureKind {Runtime.CompilerServices.Unsafe.SizeOf<LigatureKind>()} B, InsertionKind {Runtime.CompilerServices.Unsafe.SizeOf<InsertionKind>()} B, ContextualContext {Runtime.CompilerServices.Unsafe.SizeOf<ContextualContext>()} B, DenseMachine {Runtime.CompilerServices.Unsafe.SizeOf<DenseMachine>()} B, DriverConfig {Runtime.CompilerServices.Unsafe.SizeOf<DriverConfig>()} B, StateMachine {Runtime.CompilerServices.Unsafe.SizeOf<StateMachine>()} B")
        let tally name (counted: unit -> int) = Console.WriteLine(name + ": " + bench.Tally(fun () -> counted ()))
        tally "alternating" (fun () -> let struct (p, s, r) = bench.PassAlternating() in (GlyphRun.runPass { CountingPass.Inner = p } s r).Infos.Length)
        tally "contextual" (fun () -> let struct (p, s, r) = bench.PassContextual() in (GlyphRun.runPass { CountingPass.Inner = p } s r).Infos.Length)
        tally "rearrangement" (fun () -> let struct (p, s, r) = bench.PassRearrangement() in (GlyphRun.runPass { CountingPass.Inner = p } s r).Infos.Length)
        tally "ligature" (fun () -> let struct (p, s, r) = bench.PassLigature() in (GlyphRun.runPass { CountingPass.Inner = p } s r).Infos.Length)
        tally "insertion" (fun () -> let struct (p, s, r) = bench.PassInsertion() in (GlyphRun.runPass { CountingPass.Inner = p } s r).Infos.Length)
        tally "kerx format 1" (fun () -> let struct (p, s, r) = bench.PassKerxFormat1() in (GlyphRun.runPass { CountingPass.Inner = p } s r).Infos.Length)
        tally "kerx format 4" (fun () -> let struct (p, s, r) = bench.PassKerxFormat4() in (GlyphRun.runPass { CountingPass.Inner = p } s r).Infos.Length)
