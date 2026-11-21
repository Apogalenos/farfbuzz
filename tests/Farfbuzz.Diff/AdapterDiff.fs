module Farfbuzz.Diff.AdapterDiff

open System
open System.Collections.Concurrent
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Harness
open Farfbuzz.Harness.Runner
open Farfbuzz.Diff.HarfBuzzSharpShaper

/// <summary>What runs beside the shaping workers of a schedule.</summary>
type Churn =
    | Languages    // new languages entering HarfBuzz's global list
    | Faces        // faces created and destroyed on other threads
    | Collections  // forced garbage collections, finalisers included
    | Allocations  // managed allocation, to make the collector run by itself

/// <summary>One way of loading the adapter.</summary>
type Schedule =
    { Suite: string       // the upstream suite whose runs are shaped
      First: int          // where the window of runs starts in the suite
      Workers: int        // threads shaping every run of the window
      FreshFaces: bool    // a face cache per worker, not one shared by all
      Churn: Churn list }

/// <summary>The number of runs each schedule shapes on every worker.</summary>
[<Literal>]
let private Window = 240

/// <summary>Schedules: mostly the two small suites, the large one now and then.</summary>
let private schedules =
    gen {
        let! suite = Gen.frequency [ 5, Gen.constant "aots"; 4, Gen.constant "text-rendering-tests"; 1, Gen.constant "in-house" ]
        let! first = Gen.choose (0, 5000)
        let! workers = Gen.choose (2, 8)
        let! fresh = Gen.elements [ false; false; true ]
        let! churn = Gen.subListOf [ Languages; Faces; Collections; Allocations ]
        return { Suite = suite; First = first; Workers = workers; FreshFaces = fresh; Churn = churn }
    }

/// <summary>The runs of each suite, planned once.</summary>
let private planned = ConcurrentDictionary<string, RunSpec array>()

/// <summary>The runs of a suite as run-tests.py would run them.</summary>
/// <param name="suite">The suite name.</param>
/// <returns>The runs.</returns>
let private specsOf (suite: string) =
    planned.GetOrAdd(suite, fun name ->
        let shaper = HarfBuzzSharpShaper.create ()
        let entries = (Suites.registered (RepositoryPaths.upstream ()) |> List.find (fun candidate -> candidate.Name = name)).Files
        entries |> List.collect (plan shaper) |> List.toArray)

/// <summary>The font files a suite's runs open.</summary>
/// <param name="specs">The runs.</param>
/// <returns>The distinct paths that exist.</returns>
let private fontsOf (specs: RunSpec array) =
    specs
    |> Array.choose (fun spec ->
        match spec.Kind with
        | Execute (fontPath, _, _, _, _) when File.Exists fontPath -> Some fontPath
        | Execute _ | Skipped _ | Unrunnable _ -> None)
    |> Array.distinct

/// <summary>Runs one kind of churn until it is told to stop.</summary>
/// <param name="fonts">Font files to open.</param>
/// <param name="token">Signals the end of the schedule.</param>
/// <param name="kind">The churn.</param>
let private churn (fonts: string array) (token: CancellationToken) (kind: Churn) =
    match kind with
    | Languages ->
        let mutable made = 0
        while not token.IsCancellationRequested && made < 200 do
            let bytes = Encoding.ASCII.GetBytes $"x-{Guid.NewGuid():N}"
            Native.hb_language_from_string(bytes, bytes.Length) |> ignore
            made <- made + 1
    | Faces ->
        let mutable index = 0
        while not token.IsCancellationRequested && fonts.Length > 0 do
            use blob = HbBlob.FromFile fonts[index % fonts.Length]
            use face = new HbFace(blob, 0)
            Native.hb_face_get_glyph_count face.Handle |> ignore
            index <- index + 1
    | Collections ->
        while not token.IsCancellationRequested do
            GC.Collect()
            GC.WaitForPendingFinalizers()
            Thread.Sleep 2
    | Allocations ->
        while not token.IsCancellationRequested do
            GC.KeepAlive(Array.zeroCreate<byte> 8192)

/// <summary>Shapes every run of a schedule's suite on its workers, beside its churn.</summary>
/// <param name="schedule">The schedule.</param>
/// <returns>One line per run that did not pass, with what a second pass and a fresh engine say about it.</returns>
let private failuresOf (schedule: Schedule) : string list =
    let whole = specsOf schedule.Suite
    let start = schedule.First % whole.Length
    let specs = Array.init (min Window whole.Length) (fun index -> whole[(start + index) % whole.Length])
    let fonts = fontsOf specs
    let reportsBefore = Tripwire.reports.Count
    use stop = new CancellationTokenSource()
    let background = schedule.Churn |> List.map (fun kind -> Task.Factory.StartNew((fun () -> churn fonts stop.Token kind), TaskCreationOptions.LongRunning))
    let shared = HarfBuzzSharpShaper.create ()
    let failures = ConcurrentQueue<string>()
    let work () =
        let shaper = if schedule.FreshFaces then HarfBuzzSharpShaper.create () else shared
        for spec in specs do
            match execute shaper spec with
            | Pass | Skip _ -> ()
            | Fail _ | Broken _ as outcome ->
                let again = execute shaper spec
                let fresh = execute (HarfBuzzSharpShaper.create ()) spec
                failures.Enqueue $"{spec.Id.Suite}/{spec.Id.File}:{spec.Id.Line} on thread {Environment.CurrentManagedThreadId}: %A{outcome}\n      same engine again: %A{again}\n      fresh engine: %A{fresh}"
    let workers = [ for _ in 1 .. schedule.Workers -> Task.Factory.StartNew(work, TaskCreationOptions.LongRunning) ]
    Task.WaitAll(List.toArray workers)
    stop.Cancel()
    Task.WaitAll(List.toArray background)
    let reports = Tripwire.reports |> Seq.skip reportsBefore |> Seq.map (fun path -> $"tripwire report: {path}") |> Seq.toList
    (failures |> Seq.truncate 8 |> Seq.toList) @ reports

/// <summary>A glyph for the planted faults.</summary>
/// <param name="id">The glyph number.</param>
/// <param name="cluster">The cluster.</param>
/// <returns>The glyph.</returns>
let private glyph (id: uint32) (cluster: uint32) : GlyphInfo =
    { Codepoint = id; Mask = 0u; Cluster = cluster; Var1 = 0u; Var2 = 0u }

/// <summary>A real request to hang planted faults on: the first run of the aots suite.</summary>
/// <returns>The request.</returns>
let private anyRequest () =
    specsOf "aots"
    |> Array.pick (fun spec ->
        match spec.Kind with
        | Execute (fontPath, unicodes, options, extra, _) ->
            match ShapeRequest.build fontPath unicodes options extra with
            | Ok request -> Some request
            | Error _ -> None
        | Skipped _ | Unrunnable _ -> None)

/// <summary>Runs the tripwire on planted passes, in a directory and a report list of its own.</summary>
/// <param name="first">The first pass.</param>
/// <param name="again">The second pass on the same face.</param>
/// <param name="fresh">The pass on a fresh face.</param>
/// <returns>The verdict, the reports written and how many extra passes were made.</returns>
let private planted (first: GlyphInfo array) (again: GlyphInfo array) (fresh: GlyphInfo array) =
    let folder = Path.Combine(Path.GetTempPath(), $"farfbuzz-tripwire-{Guid.NewGuid():N}")
    let sink = ConcurrentQueue<string>()
    let mutable passes = 0
    let count (result: GlyphInfo array) () =
        passes <- passes + 1
        result
    let request = anyRequest ()
    let outcome = Tripwire.examine sink folder request 100u first (count again) (count fresh)
    let texts = sink |> Seq.map File.ReadAllText |> Seq.toList
    if Directory.Exists folder then Directory.Delete(folder, true)
    outcome, texts, passes

/// <summary>Runs the look after the tests on one planted answer, in a directory and a report list of its own.</summary>
/// <param name="handedOut">What the adapter handed out.</param>
/// <param name="heldNow">What the caller's array holds when the tests are over.</param>
/// <param name="replay">What shaping the request again gives.</param>
/// <returns>How many answers were looked at, how many differ, and the reports written.</returns>
let private lookedAtLater (handedOut: GlyphInfo array) (heldNow: GlyphInfo array) (replay: GlyphInfo array) =
    let folder = Path.Combine(Path.GetTempPath(), $"farfbuzz-tripwire-{Guid.NewGuid():N}")
    let sink = ConcurrentQueue<string>()
    let queue = ConcurrentQueue<Handed>()
    let live = Array.copy handedOut
    Tripwire.remember queue (anyRequest ()) live
    Array.blit heldNow 0 live 0 live.Length
    let struct (examined, differing) = Tripwire.recheck queue sink folder (fun _ -> replay)
    let texts = sink |> Seq.map File.ReadAllText |> Seq.toList
    if Directory.Exists folder then Directory.Delete(folder, true)
    examined, differing, texts

/// <summary>
/// The HarfBuzz adapter under load, and its tripwire. Added after `P1.RunTestsEquality.aots` failed once on
/// 2026-11-10 with glyph numbers no font holds, and could not be made to fail again.
/// </summary>
/// <returns>The test list.</returns>
let tests () =
    let sound = [| glyph 17u 0u; glyph 23u 1u; glyph 24u 2u |]
    let garbage = [| glyph 0u 0u; glyph 579273584u 1u; glyph 2498263752u 2u |]
    testList "Adapter" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 12 } "every run gives its one answer under any schedule" <|
            Prop.forAll (Arb.fromGen schedules) (fun schedule ->
                let failures = failuresOf schedule
                failures.IsEmpty |> Prop.label ($"%A{schedule}\n  " + String.concat "\n  " failures))
        test "a sound result is not looked at twice" {
            let outcome, texts, passes = planted sound garbage garbage
            Expect.equal (outcome, texts.Length, passes) (ValueNone, 0, 0) "no verdict, no report, no extra pass"
        }
        test "a disturbed pass is told from a damaged face" {
            let transient, transientTexts, _ = planted garbage sound sound
            Expect.equal transient (ValueSome Transient) "the second pass differs"
            Expect.equal transientTexts.Length 1 "one report"
            Expect.stringContains transientTexts[0] "verdict: Transient" "the report names the verdict"
            Expect.stringContains transientTexts[0] "579273584=1" "the report shows the glyphs"
            let damaged, damagedTexts, _ = planted garbage garbage sound
            Expect.equal damaged (ValueSome DamagedFace) "the cached face repeats them, a fresh face does not"
            Expect.equal damagedTexts.Length 1 "one report"
            Expect.stringContains damagedTexts[0] "verdict: DamagedFace" "the report names the verdict"
        }
        test "glyphs a font really produces are not reported" {
            let outcome, texts, passes = planted garbage garbage garbage
            Expect.equal (outcome, texts.Length, passes) (ValueSome Deterministic, 0, 2) "looked at twice, nothing written"
        }
        test "an answer that still holds after the tests is not reported" {
            let examined, differing, texts = lookedAtLater sound sound sound
            Expect.equal (examined, differing, texts.Length) (1, 0, 0) "looked at, nothing written"
        }
        test "an answer overwritten after it was handed out is reported" {
            let examined, differing, texts = lookedAtLater sound garbage sound
            Expect.equal (examined, differing, texts.Length) (1, 1, 1) "one report"
            Expect.stringContains texts[0] "finding: Overwritten" "the report names the finding"
            Expect.stringContains texts[0] "handed out: 3 glyphs [17=0|23=1|24=2]" "the report shows what was handed out"
            Expect.stringContains texts[0] "579273584=1" "the report shows what the array holds now"
        }
        test "an answer that cannot be repeated is reported, however plausible it looks" {
            let plausible = [| glyph 17u 0u; glyph 0u 1u; glyph 1u 2u |]
            let examined, differing, texts = lookedAtLater plausible plausible sound
            Expect.equal (examined, differing, texts.Length) (1, 1, 1) "one report"
            Expect.stringContains texts[0] "finding: Unrepeatable" "the report names the finding"
            Expect.stringContains texts[0] "shaped now: 3 glyphs [17=0|23=1|24=2]" "the report shows the repeat"
        }
        test "a cluster outside the text is suspicious too" {
            Expect.isTrue (Tripwire.suspicious 100u 3 [| glyph 5u 7u |]) "cluster 7 in a text of 3"
            Expect.isFalse (Tripwire.suspicious 100u 3 sound) "a sound result"
        }
    ]
