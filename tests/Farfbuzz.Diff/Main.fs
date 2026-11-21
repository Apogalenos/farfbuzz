module Farfbuzz.Diff.Main

open System
open Farfbuzz.Oracle
open System.Collections.Concurrent
open System.IO
open Expecto
open Farfbuzz.Harness
open Farfbuzz.Harness.Runner
open Farfbuzz.Oracle.OracleLibrary

/// <summary>Every differential test, given the oracle chosen at start-up.</summary>
/// <param name="oracle">The oracle in use.</param>
/// <param name="sink">Collects the harness results of the oracle for its report.</param>
/// <returns>The root test list.</returns>
let all (oracle: Oracle) (sink: ConcurrentQueue<RunSpec * Outcome>) =
    testList "Farfbuzz.Diff" [
        OracleTests.tests oracle
        TextFormatDiff.tests ()
        FaceDiff.tests ()
        MetricsDiff.tests ()
        FontOpsDiff.tests ()
        FontDiff.tests ()
        FvarDiff.tests ()
        CommonDiff.tests ()
        OtTagDiff.tests ()
        UnicodeDiff.tests ()
        ComposeDiff.tests ()
        UnicodePredicateDiff.tests ()
        TextEntryDiff.tests ()
        GlyphRunDiff.tests ()
        RunPassDiff.tests ()
        CmapDiff.tests ()
        CollectDiff.tests ()
        GlyphNameDiff.tests ()
        MutationDiff.tests ()
        RunTestsEquality.tests (HarfBuzzSharpShaper.create ()) sink
        AdapterDiff.tests ()
        RandomTextDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        ShapeDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        NormalizeDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        ExtentsDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        NormalizeCoordsDiff.tests ()
        ItemVariationStoreDiff.tests ()
        FontVariationsDiff.tests ()
        HvarDiff.tests ()
        TupleVariationDiff.tests ()
        AatLookupDiff.tests ()
        LayoutCommonDiff.tests ()
        GdefDiff.tests ()
        LayoutTableDiff.tests ()
        MapDiff.tests ()
        testList "P9.S08" [ AccelDiff.tests () ]
        GsubDiff.tests ()
        AatStateTableDiff.tests ()
        AatMapDiff.tests ()
        TrakDiff.tests ()
        MorxDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        KerxDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        MacosDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        FeatApiDiff.tests ()
        ComplexShapers.IndicTableDiff.tests ()
        ComplexShapers.UseTableDiff.tests ()
        ComplexShapers.ArabicTableDiff.tests ()
        ComplexShapers.ArabicJoiningListDiff.tests ()
        GlyphPointsDiff.tests ()
        ComplexShapers.VowelConstraintsDiff.tests ()
        ComplexShapers.SyllableDiff.tests ()
        ComplexShapers.SyllabicDiff.tests ()
        ComplexShapers.CategorizeDiff.tests ()
        testList "P6.S02" [
            ComplexShapers.ShapeDiff.tests ()
            ComplexShapers.TraceDiff.tests ()
            ComplexShapers.PlanDiff.tests ()
        ]
        ComplexShapers.HebrewThaiDiff.hebrewTests ()
        ComplexShapers.HebrewThaiDiff.thaiTests ()
        ComplexShapers.HebrewThaiDiff.planTests ()
        ComplexShapers.FallbackDiff.tests ()

        ComplexShapers.HangulKhmerDiff.hangulTests ()
        ComplexShapers.HangulKhmerDiff.khmerTests ()
        ComplexShapers.HangulKhmerDiff.khmerTraceTests ()
        ComplexShapers.HangulKhmerDiff.planTests ()
        ComplexShapers.IndicDiff.shapeTestsOfAllScripts ()
        ComplexShapers.IndicDiff.traceTests ()
        ComplexShapers.IndicDiff.planTests ()
        ComplexShapers.UseDiff.shapeTestsOfAllScripts ()
        ComplexShapers.UseDiff.traceTests ()
        ComplexShapers.UseDiff.planTests ()
        ComplexShapers.DefaultShaperDiff.foundCases ()
        ComplexShapers.DefaultShaperDiff.tests ()
        ComplexShapers.MyanmarDiff.foundCases ()
        ComplexShapers.MyanmarDiff.myanmarTests ()
        ComplexShapers.MyanmarDiff.zawgyiTests ()
        ComplexShapers.MyanmarDiff.myanmarTraceTests ()
        ComplexShapers.MyanmarDiff.planTests ()
        ComplexShapers.ArabicDiff.foundCases ()
        ComplexShapers.ArabicDiff.arabicTests ()
        ComplexShapers.ArabicDiff.syriacTests ()
        ComplexShapers.ArabicDiff.traceTests ()
        ComplexShapers.ArabicDiff.planTests ()
        GvarDiff.tests ()
        GlyphOutlineDiff.tests ()
        BitmapExtentsDiff.tests ()
        ColrExtentsDiff.tests ()

        Cff1ExtentsDiff.tests ()
        Cff2ExtentsDiff.tests ()
        MvarDiff.tests ()
        FeatureVariationsDiff.tests ()
        VariationDeviceDiff.tests ()
        ShapePlanDiff.tests ()
        FallbackMarkDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper

        KernDiff.tests (HarfBuzzSharpShaper.create ()) FarfbuzzShaper.shaper
        GposDiff.tests ()
        ContextDiff.tests ()
        GposAttachDiff.tests ()
        GposContextDiff.tests ()
        BudgetDiff.tests oracle
        GposAttachSynthDiff.tests ()
    ]

/// <summary>Writes the oracle's conformance report when the equality check ran.</summary>
/// <param name="sink">The harness results collected from the oracle.</param>
let writeOracleReport (sink: ConcurrentQueue<RunSpec * Outcome>) =
    match sink.IsEmpty with
    | true -> ()
    | false ->
        let results = sink.ToArray() |> Array.toList
        let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "conformance")
        Report.writeTsv (Path.Combine(directory, "oracle.tsv")) results
        let suites, _ = Report.tally results
        Console.WriteLine "Oracle through the harness:"
        Console.WriteLine(Report.markdown suites)

/// <summary>Loads the oracle, then runs the differential tests with the Expecto arguments given.</summary>
/// <param name="argv">Expecto arguments.</param>
/// <returns>The process exit code: zero when every test passed and the adapter's tripwire stayed silent, 2 when no oracle can be loaded.</returns>
[<EntryPoint>]
let main argv =
    match ensureLoaded () with
    | Ok oracle ->
        let description = match oracle with Built path -> $"HarfBuzz {Native.version ()} built at {path}" | Bundled -> $"HarfBuzz {Native.version ()} bundled with HarfBuzzSharp"
        Console.WriteLine $"Oracle: {description}"
        let sink = ConcurrentQueue<RunSpec * Outcome>()
        let code = runTestsWithCLIArgs [] argv (all oracle sink)
        writeOracleReport sink
        let struct (examined, differing) = HarfBuzzSharpShaper.recheck ()
        Console.WriteLine $"HarfBuzz adapter: {examined} answers looked at again after the tests, {differing} changed or could not be repeated"
        match HarfBuzzSharpShaper.Tripwire.reports.IsEmpty with
        | true -> code
        | false ->
            Console.Error.WriteLine $"The HarfBuzz adapter gave answers that do not hold ({HarfBuzzSharpShaper.Tripwire.reports.Count} reports):"
            HarfBuzzSharpShaper.Tripwire.reports |> Seq.iter (fun path -> Console.Error.WriteLine $"  {path}")
            max code 1
    | Error reason ->
        Console.Error.WriteLine reason
        2
