module Farfbuzz.Bench.Program

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running

/// <summary>
/// Every benchmark runs under two jobs: native HarfBuzz 15.1.0 built from the submodule, and the HarfBuzz 14.4.0
/// HarfBuzzSharp ships (FARFBUZZ_ORACLE=bundled), so each table compares Farfbuzz with both.
/// </summary>
let config =
    DefaultConfig.Instance
        .AddJob(Job.ShortRun.WithId("HarfBuzz 15.1.0"))
        .AddJob(Job.ShortRun.WithEnvironmentVariables(EnvironmentVariable("FARFBUZZ_ORACLE", "bundled")).WithId("HarfBuzzSharp 14.4.0"))

/// <summary>Runs the benchmarks selected on the command line, for example <c>--filter *Shaping*</c>.</summary>
/// <param name="argv">BenchmarkDotNet arguments.</param>
/// <returns>Zero.</returns>
[<EntryPoint>]
let main argv =
    if Array.exists (fun arg -> arg = "--bdn-font") argv then
        let rest = argv |> Array.filter (fun arg -> arg <> "--bdn-font")
        BenchmarkSwitcher.FromTypes([| typeof<CffExtents.CffExtents>; typeof<UpstreamFont.UpstreamFont> |]).Run(rest, config) |> ignore
        0
    elif Array.exists (fun arg -> arg = "--font-ops") argv then
        FontOps.run (match Array.tryItem (1 + Array.findIndex (fun arg -> arg = "--font-ops") argv) argv with | Some filter -> filter | None -> System.String.Empty)
        0
    elif Array.exists (fun arg -> arg = "--face-parts") argv then
        FontOps.faceParts (Array.item (1 + Array.findIndex (fun arg -> arg = "--face-parts") argv) argv)
        0
    elif Array.exists (fun arg -> arg = "--font-loop") argv then
        let at = Array.findIndex (fun arg -> arg = "--font-loop") argv
        FontOps.loop (Array.item (at + 1) argv) (Array.item (at + 2) argv) (Array.item (at + 3) argv = "native") (int (Array.item (at + 4) argv))
        0
    elif Array.exists (fun arg -> arg = "--profile") argv then
        ShapeProfile.run ()
    elif Array.exists (fun arg -> arg = "--verify-cost") argv then
        VerifyCost.run ()
        0
    elif Array.exists (fun arg -> arg = "--profile-inner") argv then
        ShapeProfile.profileInner ()
    elif Array.exists (fun arg -> arg = "--per-line") argv then
        ShapeProfile.perLine ()
    elif Array.exists (fun arg -> arg = "--steps") argv then
        StepCost.run ()
    elif Array.exists (fun arg -> arg = "--loop-replace") argv then
        StepCost.loop "replace"
    elif Array.exists (fun arg -> arg = "--loop-ligature") argv then
        StepCost.loop "ligature"
    elif Array.exists (fun arg -> arg = "--loop-stage") argv then
        StepCost.loop "stage"
    elif Array.exists (fun arg -> arg = "--fast-lines") argv then
        ShapeProfile.fastLineSplit ()
    elif Array.exists (fun arg -> arg = "--steps-real") argv then
        StepCost.runReal ()
    elif Array.exists (fun arg -> arg = "--kernel-loop") argv then
        ShapeProfile.substitutionLoop 25 true false
    elif Array.exists (fun arg -> arg = "--kernel-loop-changed") argv then
        ShapeProfile.substitutionLoop 25 true true
    elif Array.exists (fun arg -> arg = "--entry-pair") argv then
        ShapeProfile.entryPair ()
        0
    elif Array.exists (fun arg -> arg = "--layout-cost") argv then
        ShapeProfile.layoutCost ()
        0
    elif Array.exists (fun arg -> arg = "--kernel-loop-gpos") argv then
        LayoutShape.kernelLoop 25
        0
    elif Array.exists (fun arg -> arg = "--pair-anatomy") argv then
        LayoutShape.pairAnatomy "Roboto/en-thelittleprince"
        LayoutShape.pairAnatomy "Roboto/en-words"
        0
    elif Array.exists (fun arg -> arg = "--line-cost") argv then
        LayoutShape.lineCost ()
        0
    elif Array.exists (fun arg -> arg = "--line-loop") argv then
        LayoutShape.lineLoop 12
        0
    elif Array.exists (fun arg -> arg = "--gpos-cost") argv then
        LayoutShape.gposCost ()
        LayoutShape.attachCost ()
        LayoutShape.contextCost ()
        0
    elif Array.exists (fun arg -> arg = "--general-loop") argv then
        GeneralShape.loop (Array.item (1 + Array.findIndex (fun arg -> arg = "--general-loop") argv) argv) 25
        0
    elif Array.exists (fun arg -> arg = "--plan-warm") argv then
        GeneralShape.planWarm ()
        0
    elif Array.exists (fun arg -> arg = "--plan-pieces") argv then
        GeneralShape.planPieces ()
        0
    elif Array.exists (fun arg -> arg = "--plan-cost") argv then
        GeneralShape.planCost ()
        0
    elif Array.exists (fun arg -> arg = "--attach-primitives") argv then
        GeneralShape.attachPrimitives ()
        0
    elif Array.exists (fun arg -> arg = "--attach-leaf") argv then
        GeneralShape.attachLeaf ()
        0
    elif Array.exists (fun arg -> arg = "--marks-anatomy") argv then
        GeneralShape.anatomy ()
        0
    elif Array.exists (fun arg -> arg = "--norm-anatomy") argv then
        GeneralShape.normAnatomy ()
        0
    elif Array.exists (fun arg -> arg = "--norm-primitives") argv then
        GeneralShape.normPrimitives ()
        0
    elif Array.exists (fun arg -> arg = "--general-copies") argv then
        GeneralShape.copies ()
        0
    elif Array.exists (fun arg -> arg = "--gsub-anatomy") argv then
        GeneralShape.gsubAnatomy ()
        0
    elif Array.exists (fun arg -> arg = "--context-visits") argv then
        GeneralShape.contextVisits ()
        0
    elif Array.exists (fun arg -> arg = "--visit-anatomy") argv then
        GeneralShape.visitAnatomy ()
        0
    elif Array.exists (fun arg -> arg = "--general-stages") argv then
        GeneralShape.stages ()
        0
    elif Array.exists (fun arg -> arg = "--utf8-input") argv then
        GeneralShape.utf8Input ()
        0
    elif Array.exists (fun arg -> arg = "--general-shape") argv then
        GeneralShape.run ()
        0
    elif Array.exists (fun arg -> arg = "--batch-shape") argv then
        BatchShape.run ()
        0
    elif Array.exists (fun arg -> arg = "--kernel-paths") argv then
        AllocPerGlyph.paths ()
        0
    elif Array.exists (fun arg -> arg = "--kernel-admission") argv then
        AllocPerGlyph.admissions ()
        0
    elif Array.exists (fun arg -> arg = "--kernel-path-alloc") argv then
        AllocPerGlyph.pathAllocation ()
        0
    elif Array.exists (fun arg -> arg = "--kernel-tail-reasons") argv then
        AllocPerGlyph.tailReasons ()
        0
    elif Array.exists (fun arg -> arg = "--kernel-ligature-stages") argv then
        AllocPerGlyph.ligatureStages ()
        0
    elif Array.exists (fun arg -> arg = "--alloc-per-glyph") argv then
        AllocPerGlyph.run ()
        0
    elif Array.exists (fun arg -> arg = "--line-routes") argv then
        BatchShape.lineRoutes ()
        0
    elif Array.exists (fun arg -> arg = "--batch-alloc") argv then
        BatchShape.allocation ()
        0
    elif Array.exists (fun arg -> arg = "--batch-break-even") argv then
        BatchShape.breakEven ()
        0
    elif Array.exists (fun arg -> arg = "--layout-shape") argv then
        LayoutShape.run ()
        0
    elif Array.exists (fun arg -> arg = "--aat-attribution") argv then
        AatShape.attribution ()
        0
    elif Array.exists (fun arg -> arg = "--aat-loop") argv then
        AatShape.loop (int (Array.item (1 + Array.findIndex (fun arg -> arg = "--aat-loop") argv) argv)) 25
        0
    elif Array.exists (fun arg -> arg = "--aat-alloc") argv then
        AatShape.allocation ()
        0
    elif Array.exists (fun arg -> arg = "--aat-stages") argv then
        AatShape.stages ()
        0
    elif Array.exists (fun arg -> arg = "--aat-types") argv then
        AatShape.types (int (Array.item (1 + Array.findIndex (fun arg -> arg = "--aat-types") argv) argv))
        0
    elif Array.exists (fun arg -> arg = "--aat-paths") argv then
        AatShape.paths ()
        0
    elif Array.exists (fun arg -> arg = "--aat-shape") argv then
        AatShape.run ()
        0
    elif Array.exists (fun arg -> arg = "--aat-cost") argv then
        AatCost.run true
        0
    elif Array.exists (fun arg -> arg = "--interpreter-loop") argv then
        ShapeProfile.substitutionLoop 25 false false
    elif Array.exists (fun arg -> arg = "--complex-shape") argv then
        ComplexShape.run ()
        0
    elif Array.exists (fun arg -> arg = "--complex-reuse") argv then
        ComplexShape.reuse ()
        0
    elif Array.exists (fun arg -> arg = "--complex-norm") argv then
        ComplexShape.normComplex ()
        0
    elif Array.exists (fun arg -> arg = "--complex-compare") argv then
        ComplexShape.compare ()
        0
    elif Array.exists (fun arg -> arg = "--complex-gsub") argv then
        ComplexShape.gsubOnly ()
        0
    elif Array.exists (fun arg -> arg = "--complex-replay") argv then
        ComplexShape.gsubReplay ()
        0
    elif Array.exists (fun arg -> arg = "--complex-lookups") argv then
        ComplexShape.gsubByLookup ()
        0
    elif Array.exists (fun arg -> arg = "--complex-stages") argv then
        ComplexShape.stages ()
        0
    elif Array.exists (fun arg -> arg = "--complex-loop") argv then
        let at = Array.findIndex (fun arg -> arg = "--complex-loop") argv
        ComplexShape.loop (Array.item (at + 1) argv) (int (Array.item (at + 2) argv))
        0
    elif Array.exists (fun arg -> arg = "--tally") argv then
        UpstreamShape.tally ()
        0
    else
        BenchmarkSwitcher.FromAssembly(typeof<Toolkit.Folds>.Assembly).Run(argv, config) |> ignore
        0
