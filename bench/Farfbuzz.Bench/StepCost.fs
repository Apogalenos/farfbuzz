module Farfbuzz.Bench.StepCost

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.Corpus
open Farfbuzz.Bench.RunPassCost

/// <summary>The number of repetitions whose minimum is reported.</summary>
[<Literal>]
let private Repeats = 15

/// <summary>The glyphs of the synthetic run.</summary>
[<Literal>]
let private RunGlyphs = 10_000

/// <summary>Times an action: the minimum of several repetitions after a warm-up, and the bytes one repetition allocates.</summary>
/// <param name="action">The action.</param>
/// <returns>Nanoseconds and bytes of one call.</returns>
let private measure (action: unit -> int) =
    let warm = Stopwatch.StartNew()
    while warm.ElapsedMilliseconds < 600L do action () |> ignore
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

/// <summary>Prints one row of the step table.</summary>
/// <param name="name">What the row measures.</param>
/// <param name="unitName">What one unit is.</param>
/// <param name="units">The units one call covers.</param>
/// <param name="action">The call.</param>
let private row (name: string) (unitName: string) (units: int) (action: unit -> int) =
    let struct (nanoseconds, bytes) = measure action
    Console.WriteLine(String.Format("| {0} | {1,9:N1} ns/{2} | {3,8:N1} B/{2} |", name, nanoseconds / float units, unitName, float bytes / float units))

/// <summary>Builds a cursor over a run's arrays 10,000 times and reads one field of it, the cost of the 112-byte cursor
/// every step of the interpreter builds.</summary>
/// <param name="run">The run.</param>
/// <returns>A sum, so the loop is kept.</returns>
let private cursorBuilds (run: GlyphRun) =
    let infos = run.Infos
    let props = run.Props
    let mutable total = 0
    let mutable index = 0
    while index < RunGlyphs do
        let cursor = RunCursor(infos.AsSpan(), props.AsSpan(), index, ReadOnlySpan.Empty, ReadOnlySpan.Empty, ReadOnlySpan.Empty, true, ClusterLevel.MonotoneGraphemes, BufferFlags.Default, 0uy, 0u, 1000, true)
        total <- total + cursor.Index
        index <- index + 1
    total

/// <summary>Calls a trivial pass's step 10,000 times on one cursor: the interface dispatch and the 48-byte edit returned
/// by value, with no machine behind it.</summary>
/// <param name="run">The run.</param>
/// <returns>A sum, so the loop is kept.</returns>
let private stepCalls (run: GlyphRun) =
    let infos = run.Infos
    let props = run.Props
    let pass = NextGlyphPass()
    let cursor = RunCursor(infos.AsSpan(), props.AsSpan(), 0, ReadOnlySpan.Empty, ReadOnlySpan.Empty, ReadOnlySpan.Empty, true, ClusterLevel.MonotoneGraphemes, BufferFlags.Default, 0uy, 0u, 1000, true)
    let mutable total = 0
    let mutable index = 0
    while index < RunGlyphs do
        let struct (edit, _) = (pass :> IRunPass<unit>).Step(&cursor, ())
        total <- total + int edit.Kind
        index <- index + 1
    total

/// <summary>Prints the cost table of the lookup mechanism on the synthetic cases of <c>RunPassCost</c>, through the edit
/// interpreter and through the lookup kernel.</summary>
let private syntheticTable () =
    let bench = RunPassCost()
    bench.Setup()
    let run =
        let infos = Sealed.init RunGlyphs (fun index -> { Codepoint = uint32 (index % 128); Mask = 0u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
        let props = Sealed.init RunGlyphs (fun _ -> Unchecked.defaultof<GlyphProps>)
        { GlyphRun.empty with Infos = infos; Props = props }
    Console.WriteLine("### Interpreter, one step at a time (10,000-glyph run, minimum of 15)")
    Console.WriteLine("| what | time | allocation |")
    Console.WriteLine("|---|---:|---:|")
    row "building the 112-byte cursor" "glyph" RunGlyphs (fun () -> cursorBuilds run)
    row "calling Step through the interface, returning a 48-byte edit" "glyph" RunGlyphs (fun () -> stepCalls run)
    row "NextGlyph step (cursor, Step, dispatch)" "glyph" RunGlyphs (fun () -> (bench.NextGlyphPassRun()).Infos.Length)
    row "SkipGlyph step (floor plus materialise and Idx)" "glyph" RunGlyphs (fun () -> (bench.SkipGlyphPassRun()).Infos.Length)
    row "OutputInfo step (floor plus makeRoomFor, separate buffer)" "glyph" RunGlyphs (fun () -> (bench.OutputInfoPassRun()).Infos.Length)
    row "ReplaceGlyphProps step (floor plus makeRoomFor and two writes)" "glyph" RunGlyphs (fun () -> (bench.ReplaceGlyphPropsPassRun()).Infos.Length)
    Console.WriteLine()
    Console.WriteLine("### Pure pieces of a lookup (no machine)")
    Console.WriteLine("| what | time | allocation |")
    Console.WriteLine("|---|---:|---:|")
    row "Coverage.index, range coverage" "glyph" RunGlyphs (fun () -> bench.CoverageIndexOnly())
    row "Matching.setGlyphClass, no GDEF classes" "glyph" RunGlyphs (fun () -> bench.SetGlyphClassOnly())
    row "Gdef.glyphProps" "glyph" RunGlyphs (fun () -> bench.GdefGlyphPropsOnly())
    row "GsubApply.trySubtable, single delta" "glyph" RunGlyphs (fun () -> bench.TrySubtableOnly())
    row "Digest.ofSpan over the run" "glyph" RunGlyphs (fun () -> bench.DigestOfSpanOnly() |> int)
    row "copying the run's infos and properties once" "glyph" RunGlyphs (fun () -> (bench.MapiIdentity()).Length)
    Console.WriteLine()
    Console.WriteLine("### Whole cases, interpreter against kernel")
    Console.WriteLine("| case | interpreter | kernel |")
    Console.WriteLine("|---|---:|---:|")
    let pair (name: string) (unitName: string) (units: int) (interpreter: unit -> int) (kernel: unit -> int) =
        let struct (interpreterNs, interpreterBytes) = measure interpreter
        let struct (kernelNs, kernelBytes) = measure kernel
        Console.WriteLine(String.Format("| {0} | {1,9:N1} ns/{2}, {3,8:N1} B/{2} | {4,9:N1} ns/{2}, {5,8:N1} B/{2} |", name, interpreterNs / float units, unitName, float interpreterBytes / float units, kernelNs / float units, float kernelBytes / float units))
    pair "single substitution, no match, not gated" "glyph" RunGlyphs (fun () -> (bench.SingleSubstNonGatedNonMatching()).Infos.Length) (fun () -> (bench.KernelSingleSubstNonGatedNonMatching()).Infos.Length)
    pair "single substitution, no match, gated" "glyph" RunGlyphs (fun () -> (bench.SingleSubstGated()).Infos.Length) (fun () -> (bench.KernelSingleSubstGated()).Infos.Length)
    pair "single substitution replaces every glyph" "glyph" RunGlyphs (fun () -> (bench.SingleSubstReplaceAll()).Infos.Length) (fun () -> (bench.KernelSingleSubstReplaceAll()).Infos.Length)
    pair "ligature at every second glyph" "glyph" RunGlyphs (fun () -> (bench.LigatureFormsEverySecond()).Infos.Length) (fun () -> (bench.KernelLigatureFormsEverySecond()).Infos.Length)
    pair "ligature, no match, not gated" "glyph" RunGlyphs (fun () -> (bench.LigatureNonGatedNonMatching()).Infos.Length) (fun () -> (bench.KernelLigatureNonGatedNonMatching()).Infos.Length)
    pair "ten lookups each replacing one glyph of 50" "lookup" 10 (fun () -> (bench.Stage10ReplaceOne()).Infos.Length) (fun () -> (bench.KernelStage10ReplaceOne()).Infos.Length)
    pair "ten lookups matching nothing in 50 glyphs" "lookup" 10 (fun () -> (bench.Stage10NonMatching()).Infos.Length) (fun () -> (bench.KernelStage10NonMatching()).Infos.Length)
    Console.WriteLine()

/// <summary>Prints the cost of the AAT driver on the alternating case, for the record.</summary>
let private aatRow () =
    let bench = RunPassCost()
    bench.Setup()
    Console.WriteLine("### AAT state-machine driver, classes and states alternating (10,000 glyphs)")
    Console.WriteLine("| what | time | allocation |")
    Console.WriteLine("|---|---:|---:|")
    row "driver step per glyph" "glyph" RunGlyphs (fun () -> bench.NonActionableAlternatingRun())
    Console.WriteLine()

/// <summary>Runs the step-cost measurements on the synthetic cases.</summary>
/// <returns>Zero.</returns>
let run () =
    Console.WriteLine("Step cost of the lookup mechanism (minimum of 15 repetitions after a warm-up; the machine is shared).")
    Console.WriteLine()
    syntheticTable ()
    aatRow ()
    0

/// <summary>Runs one synthetic case for a number of seconds, so a sampling profiler can attach to the process.</summary>
/// <param name="name">The case: <c>replace</c> for the single substitution that replaces every glyph, <c>ligature</c> for the
/// ligature at every second glyph, <c>stage</c> for ten lookups over fifty glyphs.</param>
/// <returns>Zero.</returns>
let loop (name: string) =
    let bench = RunPassCost()
    bench.Setup()
    let clock = Stopwatch.StartNew()
    let mutable calls = 0
    Console.WriteLine($"pid {Environment.ProcessId}")
    while clock.Elapsed.TotalSeconds < 20.0 do
        match name with
        | "replace" -> bench.KernelSingleSubstReplaceAll() |> ignore
        | "ligature" -> bench.KernelLigatureFormsEverySecond() |> ignore
        | _ -> bench.KernelStage10ReplaceOne() |> ignore
        calls <- calls + 1
    Console.WriteLine($"{calls} calls, {clock.Elapsed.TotalMilliseconds * 1000.0 / float calls:N1} µs per call")
    0

/// <summary>Runs the step-cost measurements on real lines of Roboto, in a process of their own so that the code the
/// synthetic cases compiled does not shape what the real lines run.</summary>
/// <returns>Zero.</returns>
let runReal () =
    Console.WriteLine("Step cost of the lookup mechanism on real lines (minimum of 20 passes after a warm-up; the machine is shared).")
    Console.WriteLine()
    ShapeProfile.realLineSteps ()
    0
