module Farfbuzz.Tests.Allocation

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Harness

/// <summary>Measures the bytes the current thread allocates while running an action, after a warm-up run.</summary>
/// <param name="action">The action, run twice: once to warm up (JIT, static initialisers), once to measure.</param>
/// <returns>The bytes allocated by the second run.</returns>
let bytesAllocatedBy (action: unit -> unit) =
    action ()
    let before = GC.GetAllocatedBytesForCurrentThread()
    action ()
    GC.GetAllocatedBytesForCurrentThread() - before

/// <summary>The number of measurements <see cref="bytesAllocatedAtBest"/> takes before it gives up on a repeat.</summary>
[<Literal>]
let private BestOfAttempts = 40

/// <summary>The pause between two measurements of <see cref="bytesAllocatedAtBest"/>, in milliseconds, long enough for
/// the tiered compiler and the other tests' threads to settle.</summary>
[<Literal>]
let private BestOfPauseMilliseconds = 15

/// <summary>Measures the fewest bytes the current thread allocates while running an action, over repeated runs, until
/// two runs in a row allocate the same: an action allocates the same on every run once it is warm, so the least of the
/// runs is its allocation, while a single run can be spoiled by the first use of a lazy value, by a method still waiting
/// for its optimised code, or by the runtime when other tests run beside it.</summary>
/// <param name="action">The action, run once to warm up and then until two runs in a row allocate the same.</param>
/// <returns>The fewest bytes any measured run allocated.</returns>
let bytesAllocatedAtBest (action: unit -> unit) =
    action ()
    let rec measure attempt previous fewest =
        match attempt >= BestOfAttempts with
        | true -> fewest
        | false ->
            Threading.Thread.Sleep BestOfPauseMilliseconds
            let before = GC.GetAllocatedBytesForCurrentThread()
            action ()
            let allocated = GC.GetAllocatedBytesForCurrentThread() - before
            match allocated = previous with
            | true -> min fewest allocated
            | false -> measure (attempt + 1) allocated (min fewest allocated)
    measure 0 -1L Int64.MaxValue

/// <summary>A group mapper whose every group is a single element, mapping to itself.</summary>
[<Struct>]
type private SingleMapper =
    interface IGroupMapper<int> with
        member _.GroupEnd (_, start) = start + 1
        member _.Map (items, _, _, index) = items[index]

/// <summary>Maps an int array through <see cref="SingleMapper"/> without capturing a span.</summary>
/// <param name="items">The items.</param>
/// <returns>The mapped items.</returns>
let private mapGroupsInt (items: int array) =
    Sealed.mapGroups (SingleMapper()) (ReadOnlySpan items) |> TailCalls.settle

/// <summary>The P4.S02 allocation tests for the new Sealed builders.</summary>
[<Tests>]
let sealedBuilderAllocations =
    testList "P4.S02" [
        test "mapIfChanged identity allocates nothing" {
            let items = Sealed.init 1000 id
            Sealed.mapIfChanged id items |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> Sealed.mapIfChanged id items |> ignore)) 0L "bytes allocated"
        }
        test "mapGroups allocates exactly one array" {
            let items = Array.init 1000 id
            let arrayBytes = int64 (sizeof<int> * 1000)
            let allocated = bytesAllocatedBy (fun () -> mapGroupsInt items |> ignore)
            Expect.isLessThanOrEqual allocated (arrayBytes + 256L) "bytes allocated beyond the array"
        }
    ]

/// <summary>The P5.S01 allocation case.</summary>
[<Tests>]
let p5s01Allocations =
    testList "P5.S01" [
        test "Coverage.index and ClassDef.classOf allocate nothing" {
            let coverage =
                [| 0x00uy; 0x01uy; 0x00uy; 0x05uy; 0x00uy; 0x01uy; 0x00uy; 0x02uy; 0x00uy; 0x03uy; 0x00uy; 0x04uy; 0x00uy; 0x05uy |]
            let classDef =
                [| 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x03uy; 0x00uy; 0x01uy; 0x00uy; 0x02uy; 0x00uy; 0x03uy |]
            let coverage = Coverage.parse (ReadOnlySpan coverage) 0
            let classDef = ClassDef.parse (ReadOnlySpan classDef) 0
            Expect.equal (bytesAllocatedBy (fun () -> Coverage.index coverage 3u |> ignore)) 0L "coverage index"
            Expect.equal (bytesAllocatedBy (fun () -> ClassDef.classOf classDef 2u |> ignore)) 0L "class lookup"
        }
    ]

/// <summary>Appends a thousand ints through the growable builder and publishes them.</summary>
/// <returns>Unit.</returns>
let private appendAndPublish () =
    let mutable builder : PointBuilder<int> = PointBuilder.create()
    for value in 0 .. 1000 do
        builder <- PointBuilder.append builder value
    PointBuilder.publish builder |> ignore

/// <summary>The P7.S07 allocation test for the growable PointBuilder.</summary>
[<Tests>]
let pointBuilderAllocations =
    testList "P7.S07" [
        test "PointBuilder appends a thousand values with a bounded number of allocations" {
            let allocated = bytesAllocatedBy appendAndPublish
            Expect.isLessThan allocated (int64 (sizeof<int> * 8192)) "appends reallocated far more than the doubling policy"
        }
    ]

/// <summary>The budget a thousand-character ASCII shape stays under, measured at 265 696 bytes after PERF2
/// stopped <c>Sealed.runPass</c> allocating for no-op passes and <c>Normalize</c> allocating per glyph.</summary>
[<Literal>]
let ShapeAsciiBudget = 270_000L

/// <summary>The P4.S10 allocation cases for the basic shaping pipeline.</summary>
[<Tests>]
let p4s10Allocations =
    testList "P4.S10" [
        test "an identity stage returns the same GlyphRun instance" {
            let run = GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x42u |]) 0 -1 GlyphRun.empty |> TailCalls.settle
            let result = OtLayout.positionStart run
            Expect.isTrue (Object.ReferenceEquals(run, result)) "reference-equal"
        }
        test "shaping 1000 ASCII characters allocates at most the recorded budget" {
            let font = Font.create Face.empty
            let text = Array.create 1000 0x61u
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            let allocated = bytesAllocatedBy (fun () -> Shape.shape font ImmutableArray.Empty run |> ignore)
            Expect.isLessThanOrEqual allocated ShapeAsciiBudget "bytes allocated"
        }
        test "mapIfChanged allocates nothing" {
            let items = Sealed.init 1000 id
            Expect.equal (bytesAllocatedBy (fun () -> Sealed.mapIfChanged id items |> ignore)) 0L "bytes allocated"
        }
    ]

/// <summary>Emits a fixed array of runs in order, then reports exhaustion.</summary>
[<Struct>]
type RunQueueSource =
    val Runs: struct (int * int * byte) array
    new (runs: struct (int * int * byte) array) = { Runs = runs }
    interface IRunSource<int> with
        member this.Next(state) =
            match state < this.Runs.Length with
            | false -> RunStep.Exhausted state
            | true ->
                let struct (start, stop, value) = this.Runs[state]
                RunStep.Run (start, stop, value, state + 1)

/// <summary>Paints two fixed runs through Sealed.paintRuns over a pre-built previous array.</summary>
/// <param name="previous">The previous bytes, already immutable.</param>
/// <param name="source">The run source.</param>
/// <returns>The painted bytes.</returns>
let private paintRunsMeasure previous source =
    let struct (painted, _) = Sealed.paintRuns previous source 0
    painted

/// <summary>The P6.S04 allocation test for Sealed.paintRuns.</summary>
[<Tests>]
let paintRunsAllocations =
    testList "P6.S04" [
        test "paintRuns allocates exactly one array" {
            let previous = Sealed.init 1000 (fun _ -> 0uy)
            let source = RunQueueSource [| struct (0, 3, 9uy); struct (5, 8, 7uy) |]
            let arrayBytes = int64 previous.Length
            let allocated = bytesAllocatedAtBest (fun () -> paintRunsMeasure previous source |> ignore)
            Expect.isLessThanOrEqual allocated (arrayBytes + 256L) "bytes allocated beyond the array"
        }
    ]

/// <summary>A rewriter that keeps every chunk's rows unchanged and applies no boundary patches.</summary>
[<Struct>]
type private UnchangedRewriter =
    interface ISegmentRewriter with
        member _.Rewrite (_) = SegmentRewrite.unchanged

/// <summary>Rewrites a run of tagged infos and properties with an unchanged rewriter.</summary>
/// <param name="infos">The infos.</param>
/// <param name="props">The properties.</param>
/// <returns>The rewritten pair.</returns>
let private rewriteSegmentsMeasure (infos: GlyphInfo array) (props: GlyphProps array) =
    Sealed.rewriteSegments (UnchangedRewriter()) (ReadOnlySpan infos) (ReadOnlySpan props) |> TailCalls.settle

/// <summary>The P6.S10 allocation test for Sealed.rewriteSegments.</summary>
[<Tests>]
let rewriteSegmentsAllocations =
    testList "P6.S10" [
        test "rewriteSegments allocates the working rows and the two output arrays" {
            let infos =
                Array.init 1000 (fun i ->
                    { Codepoint = uint32 i; Mask = 0u; Cluster = uint32 (i % 4); Var1 = 0u; Var2 = 0u })
            let props =
                Array.init 1000 (fun i ->
                    { Unchecked.defaultof<GlyphProps> with Syllable = byte (i % 4) })
            let arrayBytes = int64 (1000 * (sizeof<GlyphRow> + sizeof<GlyphInfo> + sizeof<GlyphProps>))
            let allocated = bytesAllocatedBy (fun () -> rewriteSegmentsMeasure infos props |> ignore)
            Expect.isLessThanOrEqual allocated (arrayBytes + 256L) "bytes allocated beyond the three arrays"
        }
    ]

/// <summary>Creates, fills and publishes a DeltaBuffer over a thousand points.</summary>
/// <returns>Unit.</returns>
let private fillAndPublishDeltaBuffer () =
    let xs = Array.init 1000 (fun _ -> 1.0f)
    let ys = Array.init 1000 (fun _ -> 2.0f)
    let deltas = Array.init 1000 (fun _ -> 3)
    let buffer = DeltaBuffer.create (ReadOnlySpan xs) (ReadOnlySpan ys)
    let buffer = DeltaBuffer.addScaledX buffer 2.0f (ReadOnlySpan deltas) 0
    let buffer = DeltaBuffer.accumulate buffer 2.0f (ReadOnlySpan deltas) (ReadOnlySpan deltas) 0
    let buffer = DeltaBuffer.flush buffer 0 1000
    DeltaBuffer.publish buffer |> ignore

/// <summary>The P7.S09 allocation test for the DeltaBuffer.</summary>
[<Tests>]
let deltaBufferAllocations =
    testList "P7.S09" [
        test "DeltaBuffer allocates a bounded number of arrays" {
            let allocated = bytesAllocatedBy fillAndPublishDeltaBuffer
            Expect.isLessThan allocated (int64 (sizeof<float32> * 1000 * 12)) "create and publish reallocated far more than their arrays"
        }
    ]

/// <summary>A pass that advances every glyph without editing anything.</summary>
[<Struct>]
type private NoEditPass =
    interface IRunPass<unit> with
        member _.Step (cursor, _) =
            match cursor.Index < cursor.Input.Length with
            | true -> struct (RunEdit.NextGlyph, ())
            | false -> struct (RunEdit.Finish, ())

/// <summary>A pass that sets one glyph's mask without touching positions.</summary>
[<Struct>]
type private MaskPass =
    interface IRunPass<bool> with
        member _.Step (_, done_) =
            match done_ with
            | true -> struct (RunEdit.Finish, true)
            | false -> struct (RunEdit.OrMask (1u, 0, 1), true)

/// <summary>Returns the backing array of an immutable array for reference comparison.</summary>
/// <param name="items">The immutable array.</param>
/// <returns>The backing array.</returns>
let private backing (items: ImmutableArray<'T>) = ImmutableCollectionsMarshal.AsArray items

/// <summary>Builds a two-glyph run with zeroed positions.</summary>
/// <returns>The run.</returns>
let private runWithPositions () =
    GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x42u |]) 0 -1 GlyphRun.empty
    |> GlyphRun.clearPositions
    |> TailCalls.settle

/// <summary>Compiles the default plan for the normalize allocation test.</summary>
/// <returns>The plan.</returns>
let private normalizePlan () : OtShapePlan =
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    OtPlan.compile PlanCapabilities.none props ImmutableArray.Empty OtMap.empty DefaultShaper

/// <summary>Measures the bytes <see cref="Normalize.run"/> allocates for a plain ASCII run of the given length.</summary>
/// <param name="length">The number of code points.</param>
/// <returns>The bytes allocated.</returns>
let private normalizeBytes (length: int) : int64 =
    let font = Font.create Face.empty
    let plan = normalizePlan ()
    let text = Array.create length 0x61u
    let run = GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
    bytesAllocatedBy (fun () -> Normalize.run plan font run |> ignore)

/// <summary>The PERF2 allocation cases for the lazy run-pass machine and the normalize fast path.</summary>
[<Tests>]
let perf2Allocations =
    testList "PERF2" [
        test "a pass without edits returns the same run" {
            let run = runWithPositions ()
            let result = GlyphRun.runPass (NoEditPass()) () run
            Expect.isTrue (Object.ReferenceEquals(run, result)) "reference-equal"
            Expect.isTrue (Object.ReferenceEquals(backing run.Infos, backing result.Infos)) "infos array reused"
            Expect.isTrue (Object.ReferenceEquals(backing run.Props, backing result.Props)) "props array reused"
            Expect.isTrue (Object.ReferenceEquals(backing run.Positions, backing result.Positions)) "positions array reused"
        }
        test "a pass without position edits reuses the positions array" {
            let run = runWithPositions ()
            let result = GlyphRun.runPass (MaskPass()) false run
            Expect.isFalse (Object.ReferenceEquals(run, result)) "the run changed"
            Expect.isTrue (Object.ReferenceEquals(backing run.Positions, backing result.Positions)) "positions array reused"
        }
        test "normalize of simple text allocates a constant number of arrays" {
            let thousand = normalizeBytes 1000
            let twoThousand = normalizeBytes 2000
            let bound = int64 (2000 * (sizeof<GlyphInfo> + sizeof<GlyphProps>) * 2)
            Expect.isLessThanOrEqual (twoThousand - thousand) bound $"normalize grew per glyph (1 000 = {thousand}, 2 000 = {twoThousand})"
        }
    ]

/// <summary>Writes a big-endian int16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private beI16 (value: int16) : byte array =
    [| byte (uint16 value >>> 8); byte value |]

/// <summary>Writes a big-endian uint16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) : byte array =
    [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32 as four bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) : byte array =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Builds a font whose only table is an OT format 0 kern table with one pair.</summary>
/// <returns>The font.</returns>
let private kernOnlyFont () : Font =
    let binSearch = Array.concat [ be16 1u; be16 0u; be16 0u; be16 0u ]
    let pair = Array.concat [ be16 1u; be16 1u; beI16 500s ]
    let subtable = Array.concat [ be16 0u; be16 (uint32 (14 + pair.Length)); [| 0uy; 0x01uy |]; binSearch; pair ]
    let kern = Array.concat [ be16 0u; be16 1u; subtable ]
    let header = Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u ]
    let record = Array.concat [ be32 (uint32 Tags.Kern); be32 0u; be32 28u; be32 (uint32 kern.Length) ]
    let file = Array.concat [ header; record; kern ]
    Face.create (ReadOnlyMemory file) 0u |> Font.create

/// <summary>Builds a plan that applies the kern table.</summary>
/// <returns>The plan.</returns>
let private kernOnlyPlan () : OtShapePlan =
    { Props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
      Shaper = Categorize.profile DefaultShaper
      Map = OtMap.empty
      Data = NoShaperData
      UserFeatures = ImmutableArray.Empty
      FracMask = 0u
      NumrMask = 0u
      DnomMask = 0u
      RtlmMask = 0u
      KernMask = 0x80000000u
      RequestedKerning = true
      HasFrac = false
      HasVert = false
      HasGposMark = false
      ZeroMarks = false
      FallbackGlyphClasses = true
      FallbackMarkPositioning = false
      AdjustMarkPositioningWhenZeroing = false
      ApplyGpos = false
      ApplyKern = true
      ApplyFallbackKern = false
      ApplyKerx = false
      ApplyMorx = false
      ApplyTrak = false; Unicode = ValueNone }

/// <summary>Builds a 1,000-glyph horizontal run of glyph 2, which has no kern pair.</summary>
/// <returns>The run.</returns>
let private kernNoPairRun () : GlyphRun =
    let infos = Sealed.init 1000 (fun i -> { Codepoint = 2u; Mask = 0x80000000u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init 1000 (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = 0us })
    let positions = Sealed.init 1000 (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>Builds a horizontal run of <paramref name="count"/> glyphs, the first <paramref name="pairs"/> + 1 of
/// which are glyph 1 (kerned) and the rest glyph 2 (not kerned), so the run has <paramref name="pairs"/> kerned pairs.</summary>
/// <param name="pairs">The number of kerned pairs.</param>
/// <param name="count">The total number of glyphs.</param>
/// <returns>The run.</returns>
let private kernPairRun (pairs: int) (count: int) : GlyphRun =
    let infos = Sealed.init count (fun i -> { Codepoint = (if i <= pairs then 1u else 2u); Mask = 0x80000000u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = 0us })
    let positions = Sealed.init count (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>The P5.S13 allocation case: kern of a run with no pair allocates nothing.</summary>
[<Tests>]
let p5s13Allocations =
    testList "P5.S13" [
        test "kern of a run with no pair allocates nothing" {
            let font = kernOnlyFont ()
            let plan = kernOnlyPlan ()
            let run = kernNoPairRun ()
            Expect.equal (bytesAllocatedBy (fun () -> Kern.applyKern plan font run |> ignore)) 0L "bytes allocated"
        }
        test "kerning allocates nothing per kerned pair" {
            let font = kernOnlyFont ()
            let plan = kernOnlyPlan ()
            let runWith pairs =
                let run = kernPairRun pairs 1001
                bytesAllocatedBy (fun () -> Kern.applyKern plan font run |> ignore)
            let ten = runWith 10
            let thousand = runWith 1000
            Expect.equal (thousand - ten) 0L "the same positions arrays, and no allocation per kerned pair"
        }
    ]

/// <summary>Shapes one plain ASCII run and returns the plan the cache holds after a warm-up.</summary>
/// <param name="length">The number of code points.</param>
/// <returns>The font, the run, the cached plan and the warmed cache.</returns>
let private cachedPlanFixture (length: int) : Font * GlyphRun * ShapePlan * PlanCache =
    let font = Font.create Face.empty
    let text = Array.create length 0x61u
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
        |> GlyphRun.guessSegmentProperties Language.cLocale
        |> TailCalls.settle
    let features = ImmutableArray.Empty
    let struct (_, cache) = Shape.shapeCached PlanCache.empty font features ValueNone run
    match ShapePlan.create font run.SegmentProperties features ValueNone with
    | Ok plan -> font, run, plan, cache
    | Error _ -> failwith "plan creation failed"

/// <summary>The PERF6 allocation case: a plan-cache hit allocates only what shaping allocates.</summary>
[<Tests>]
let perf6Allocations =
    testList "PERF6" [
        test "shapeCached with a cached plan allocates what shaping alone allocates" {
            let font, run, plan, cache = cachedPlanFixture 1000
            let features = ImmutableArray.Empty
            let shapeBytes = bytesAllocatedBy (fun () -> Shape.shapeWithPlan font plan features run |> ignore)
            let cachedBytes = bytesAllocatedBy (fun () -> Shape.shapeCached cache font features ValueNone run |> ignore)
            Expect.equal cachedBytes shapeBytes "the plan-cache lookup adds no allocation"
        }
    ]
