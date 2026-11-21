module Farfbuzz.Bench.ShapeProfile

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Text
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.Corpus

/// <summary>The layout-table tags a no-layout profile renames out of a font.</summary>
let private layoutTags =
    [ "GSUB"; "GPOS"; "GDEF"; "kern"; "morx"; "mort"; "kerx"; "trak"; "ankr"; "feat" ]
    |> List.map (fun text -> uint32 (Tag.ofString text))
    |> Set.ofList

/// <summary>Renames the layout-table tags of one sfnt offset-table directory so shaping reads none of them.</summary>
/// <param name="bytes">The font bytes, mutated in place.</param>
let private stripDirectory (bytes: byte array) =
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
    let mutable index = 0
    while index < count do
        let record = 12 + index * 16
        if record + 4 <= bytes.Length && layoutTags.Contains (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4))) then
            bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'
        index <- index + 1

/// <summary>Writes a layout-stripped copy of a font beside the corpus and returns its path.</summary>
/// <param name="fontPath">The original font path.</param>
/// <returns>The stripped font's path.</returns>
let private stripFont (fontPath: string) : string =
    let bytes = File.ReadAllBytes fontPath
    stripDirectory bytes
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "no-layout")
    Directory.CreateDirectory directory |> ignore
    let target = Path.Combine(directory, $"profile-{Path.GetFileName fontPath}#no-layout")
    File.WriteAllBytes (target, bytes)
    target

/// <summary>The 1,000-character ASCII line: prose repeated and cut to exactly 1,000 bytes.</summary>
let private asciiBytes : byte array =
    let phrase = "the quick brown fox jumps over the lazy dog, and the five boxing wizards jump quickly. "
    let builder = StringBuilder()
    while builder.Length < 1000 do
        builder.Append phrase |> ignore
    Encoding.UTF8.GetBytes (builder.ToString(0, 1000))

/// <summary>One stage's accumulated ticks and bytes.</summary>
[<Struct>]
type private Measurement =
    { mutable Ticks: int64
      mutable Bytes: int64 }

/// <summary>The timing clock carried from one stage to the next.</summary>
[<Struct>]
type private Clock =
    { mutable Bytes: int64
      mutable Timestamp: int64 }

/// <summary>Records the time and bytes of the stage that just finished, and advances the clock.</summary>
/// <param name="acc">The accumulator array.</param>
/// <param name="index">The stage index.</param>
/// <param name="clock">The clock carried between stages.</param>
let private finishStage (acc: Measurement array) (index: int) (clock: Clock byref) =
    let timestamp = Stopwatch.GetTimestamp()
    let bytes = GC.GetAllocatedBytesForCurrentThread()
    acc[index].Ticks <- acc[index].Ticks + (timestamp - clock.Timestamp)
    acc[index].Bytes <- acc[index].Bytes + (bytes - clock.Bytes)
    clock.Timestamp <- timestamp
    clock.Bytes <- bytes

/// <summary>The number of real pipeline stages, the calls <c>Shape.shapeCached</c> actually makes.</summary>
[<Literal>]
let private RealStageCount = 20

/// <summary>The real pipeline stage names, in the order the real shaper runs them.</summary>
let private realStageNames : string array =
    [| "addUtf8"
       "guessSegmentProperties"
       "plan (key + cache)"
       "enter"
       "setUnicodeProps (fused)"
       "insertDottedCircle"
       "formClusters"
       "ensureNativeDirection"
       "rotateChars"
       "normalize"
       "setupMasks"
       "recategorizeMarks"
       "mapGlyphsFast"
       "substituteStart"
       "synthesizeGlyphClasses"
       "substitute (GSUB)"
       "position (program)"
       "substitutePost"
       "propagate"
       "result extraction" |]

/// <summary>The glyph-record arrays (GlyphInfo, GlyphProps or GlyphPosition) each real stage creates in one
/// plain-Latin, LTR, mark-free run. The count is static; the positioning program materialises one Positions array
/// where the reference decomposition materialised one per sub-stage.</summary>
let private realGlyphRecordArrays : int array =
    [| 2; 0; 0; 0; 2; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0; 1; 0; 1; 0 |]

/// <summary>The number of reference decomposition stages, the old per-stage view.</summary>
[<Literal>]
let private RefStageCount = 28

/// <summary>The reference decomposition stage names, in pipeline order.</summary>
let private refStageNames : string array =
    [| "addUtf8"
       "guessSegmentProperties"
       "plan (createPlan)"
       "enter"
       "setUnicodeProps"
       "insertDottedCircle"
       "formClusters"
       "ensureNativeDirection"
       "rotateChars"
       "normalize"
       "setupMasks"
       "recategorizeMarks"
       "mapGlyphsFast"
       "substituteStart"
       "synthesizeGlyphClasses"
       "substitute (GSUB)"
       "clearPositions"
       "positionDefault"
       "positionStart"
       "position (GPOS)"
       "zeroMarkWidthsByGdef"
       "finishAdvances"
       "zeroWidthDefaultIgnorables"
       "finishOffsets"
       "fallbackMarkPosition"
       "substitutePost"
       "propagate"
       "result extraction" |]

/// <summary>The glyph-record arrays each reference stage creates in one plain-Latin run.</summary>
let private refGlyphRecordArrays : int array =
    [| 2; 0; 0; 0; 2; 0; 0; 0; 0; 1; 0; 0; 1; 1; 1; 0; 1; 1; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0 |]

/// <summary>Reconstructs the horizontal branch of the private <c>OtPosition.positionDefault</c> from public
/// functions, in the same order: map code points, batch the advances, build the positions, then the space fallback.</summary>
/// <param name="font">The font.</param>
/// <param name="run">The cleared run.</param>
/// <returns>The run with default advances.</returns>
let private positionDefaultReconstructed (font: Font) (run: GlyphRun) : GlyphRun =
    let positions = Font.hAdvancePositions font (run.Infos.AsSpan())
    let defaulted = { run with Positions = positions }
    match run.ScratchFlags &&& ScratchFlags.HasSpaceFallback = ScratchFlags.Default with
    | true -> defaulted
    | false -> OtPosition.fallbackSpaces font defaulted

/// <summary>Runs one line through the real pipeline, the same public functions <c>Shape.shapeCached</c> calls in
/// the same order, timing every stage into <paramref name="acc"/>. The positioning group runs as one program, so it
/// is one row. The plan comes from <paramref name="cache"/>, threaded as <c>hb_shape_plan_create_cached2</c> does.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text, the rest of which is post-context.</param>
/// <param name="line">The line's start offset and byte length.</param>
/// <param name="acc">The accumulator array, one entry per stage.</param>
/// <param name="cache">The plan cache, returned with any new plan appended.</param>
/// <returns>The shaped run and the updated cache.</returns>
let private runRealStages (font: Font) (text: byte array) (line: struct (int * int)) (acc: Measurement array) (cache: PlanCache) =
    let struct (start, length) = line
    let remaining = ReadOnlySpan(text, start, text.Length - start)
    let mutable clock = { Bytes = GC.GetAllocatedBytesForCurrentThread(); Timestamp = Stopwatch.GetTimestamp() }
    let mutable run = GlyphRun.empty

    run <- GlyphRun.addUtf8 remaining 0 length GlyphRun.empty
    finishStage acc 0 &clock
    run <- GlyphRun.guessSegmentProperties Language.cLocale run
    finishStage acc 1 &clock
    let struct (planResult, cache) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
    let plan =
        match planResult with
        | Ok plan -> plan
        | Error _ -> failwith "plan creation failed"
    finishStage acc 2 &clock
    let otPlan =
        match plan.Ot with
        | ValueSome p -> p
        | ValueNone -> failwith "no ot plan"
    run <- GlyphRun.enter run
    finishStage acc 3 &clock
    let targetDirection = run.SegmentProperties.Direction

    run <- OtProps.setUnicodePropsFused otPlan.Map.GlobalMask run
    finishStage acc 4 &clock
    run <- OtProps.insertDottedCircle font run
    finishStage acc 5 &clock
    run <- OtProps.formClusters run
    finishStage acc 6 &clock
    run <- OtProps.ensureNativeDirection run
    finishStage acc 7 &clock
    run <- OtSubstitute.rotateChars otPlan font otPlan.Props.Direction run
    finishStage acc 8 &clock
    run <- Normalize.run otPlan font run
    finishStage acc 9 &clock
    run <- OtSubstitute.setupMasks otPlan run
    finishStage acc 10 &clock
    run <-
        match otPlan.FallbackMarkPositioning with
        | true -> OtLayout.recategorizeMarks run
        | false -> run
    finishStage acc 11 &clock
    run <- Normalize.mapGlyphsFast run
    finishStage acc 12 &clock
    run <- OtSubstitute.substituteStart font run
    finishStage acc 13 &clock
    run <-
        match otPlan.FallbackGlyphClasses with
        | true -> OtProps.synthesizeGlyphClasses run
        | false -> run
    finishStage acc 14 &clock
    run <- OtLayout.substitute otPlan font run
    finishStage acc 15 &clock
    run <-
        match OtPosition.position otPlan font run with
        | Ok positioned -> positioned
        | Error _ -> failwith "position failed"
    finishStage acc 16 &clock
    run <- OtSubstitute.substitutePost otPlan font run
    finishStage acc 17 &clock
    run <- GlyphRun.withDirection targetDirection (Flags.propagate run)
    finishStage acc 18 &clock
    let result = { run with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u }
    finishStage acc 19 &clock
    struct (result, cache)

/// <summary>Runs one line through the reference decomposition, the same public functions <c>Shape.shapeFull</c> calls
/// in the same order, timing and counting every stage into <paramref name="acc"/>.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text, the rest of which is post-context.</param>
/// <param name="line">The line's start offset and byte length.</param>
/// <param name="acc">The accumulator array, one entry per stage.</param>
/// <param name="cache">The plan cache, returned with any new plan appended.</param>
/// <returns>The shaped run and the updated cache.</returns>
let private runReferenceStages (font: Font) (text: byte array) (line: struct (int * int)) (acc: Measurement array) (cache: PlanCache) =
    let struct (start, length) = line
    let remaining = ReadOnlySpan(text, start, text.Length - start)
    let mutable clock = { Bytes = GC.GetAllocatedBytesForCurrentThread(); Timestamp = Stopwatch.GetTimestamp() }
    let mutable run = GlyphRun.empty
    let mutable otPlan = Unchecked.defaultof<OtShapePlan>

    run <- GlyphRun.addUtf8 remaining 0 length GlyphRun.empty
    finishStage acc 0 &clock
    run <- GlyphRun.guessSegmentProperties Language.cLocale run
    finishStage acc 1 &clock
    let struct (planResult, cache) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
    let plan =
        match planResult with
        | Ok plan -> plan
        | Error _ -> failwith "plan creation failed"
    finishStage acc 2 &clock
    otPlan <- match plan.Ot with | ValueSome p -> p | ValueNone -> failwith "no ot plan"
    let entered = GlyphRun.enter run
    let targetDirection = entered.SegmentProperties.Direction
    finishStage acc 3 &clock

    let propped = OtProps.setUnicodeProps otPlan.Map.GlobalMask entered
    finishStage acc 4 &clock
    let circled = OtProps.insertDottedCircle font propped
    finishStage acc 5 &clock
    let clustered = OtProps.formClusters circled
    finishStage acc 6 &clock
    let native = OtProps.ensureNativeDirection clustered
    finishStage acc 7 &clock

    let rotated = OtSubstitute.rotateChars otPlan font otPlan.Props.Direction native
    finishStage acc 8 &clock
    let normalized = Normalize.run otPlan font rotated
    finishStage acc 9 &clock
    let masked = OtSubstitute.setupMasks otPlan normalized
    finishStage acc 10 &clock
    let recategorized =
        match otPlan.FallbackMarkPositioning with
        | true -> OtLayout.recategorizeMarks masked
        | false -> masked
    finishStage acc 11 &clock
    let mapped = Normalize.mapGlyphsFast recategorized
    finishStage acc 12 &clock
    let started = OtSubstitute.substituteStart font mapped
    finishStage acc 13 &clock
    let synthesized =
        match otPlan.FallbackGlyphClasses with
        | true -> OtProps.synthesizeGlyphClasses started
        | false -> started
    finishStage acc 14 &clock
    let substituted = OtLayout.substitute otPlan font synthesized
    finishStage acc 15 &clock

    let cleared = GlyphRun.clearPositions substituted
    finishStage acc 16 &clock
    let defaulted = positionDefaultReconstructed font cleared
    finishStage acc 17 &clock
    let adjustOffsets =
        otPlan.AdjustMarkPositioningWhenZeroing && not (Direction.isBackward defaulted.SegmentProperties.Direction)
    let startedPos = OtLayout.positionStart defaulted
    finishStage acc 18 &clock
    let early =
        match otPlan.ZeroMarks, otPlan.Shaper.ZeroWidthMarks with
        | true, ZeroWidthMarksByGdefEarly -> OtPosition.zeroMarkWidthsByGdef adjustOffsets startedPos
        | _ -> startedPos
    let positioned = OtLayout.position otPlan font early
    finishStage acc 19 &clock
    let late =
        match otPlan.ZeroMarks, otPlan.Shaper.ZeroWidthMarks with
        | true, ZeroWidthMarksByGdefLate -> OtPosition.zeroMarkWidthsByGdef adjustOffsets positioned
        | _ -> positioned
    finishStage acc 20 &clock
    let advanced = OtLayout.finishAdvances late
    finishStage acc 21 &clock
    let zeroIgnorables = OtPosition.zeroWidthDefaultIgnorables advanced
    finishStage acc 22 &clock
    let offset = OtLayout.finishOffsets font zeroIgnorables
    finishStage acc 23 &clock
    let positionedFinal =
        match OtLayout.fallbackMarkPosition otPlan font adjustOffsets offset with
        | Error _ -> failwith "fallback mark position failed"
        | Ok positionedRun ->
            match Direction.isBackward positionedRun.SegmentProperties.Direction with
            | true -> GlyphRun.reverse positionedRun
            | false -> positionedRun
    finishStage acc 24 &clock

    let post = OtSubstitute.substitutePost otPlan font positionedFinal
    finishStage acc 25 &clock
    let withDirection = GlyphRun.withDirection targetDirection (Flags.propagate post)
    finishStage acc 26 &clock
    let result = { withDirection with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u }
    finishStage acc 27 &clock
    struct (result, cache)

/// <summary>Checks that both the real and reference pipelines equal <c>Shape.shapeFull</c> for one line, failing loudly otherwise.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="line">The line to check.</param>
let private checkAgreement (font: Font) (text: byte array) (line: struct (int * int)) : unit =
    let struct (start, length) = line
    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    let run = GlyphRun.withFlags BufferFlags.Default run
    let reference =
        match Shape.shapeFull font ImmutableArray<Feature>.Empty ValueNone run with
        | Ok shaped -> shaped
        | Error _ -> failwith "shapeFull failed"
    let struct (real, _) = runRealStages font text line (Array.init RealStageCount (fun _ -> { Ticks = 0L; Bytes = 0L })) PlanCache.empty
    match Verify.diff real reference ValueNone 0u with
    | DiffFlags.Equal -> ()
    | flags -> failwith $"real pipeline disagrees with Shape.shapeFull (diff {int flags})"
    let struct (staged, _) = runReferenceStages font text line (Array.init RefStageCount (fun _ -> { Ticks = 0L; Bytes = 0L })) PlanCache.empty
    match Verify.diff staged reference ValueNone 0u with
    | DiffFlags.Equal -> ()
    | flags -> failwith $"reference pipeline disagrees with Shape.shapeFull (diff {int flags})"

/// <summary>Shapes every line once to count the glyphs one pass produces.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The total glyph count.</returns>
let private countGlyphs (font: Font) (text: byte array) (lines: struct (int * int) array) : int =
    let mutable glyphs = 0
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let run = GlyphRun.withFlags BufferFlags.Default run
        match Shape.shapeFull font ImmutableArray<Feature>.Empty ValueNone run with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    glyphs

/// <summary>Runs <paramref name="passes"/> timed passes of a stage runner and returns the per-stage totals and the
/// per-pass minima.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines.</param>
/// <param name="stageCount">The number of stages.</param>
/// <param name="passes">The number of timed passes.</param>
/// <param name="runOne">The per-line stage runner.</param>
/// <returns>The per-stage tick and byte totals, and the per-pass minima arrays.</returns>
let private measureStages
        (font: Font)
        (text: byte array)
        (lines: struct (int * int) array)
        (stageCount: int)
        (passes: int)
        (runOne: Font -> byte array -> struct (int * int) -> Measurement array -> PlanCache -> struct (GlyphRun * PlanCache))
        =
    let perPass = Array.init passes (fun _ -> Array.init stageCount (fun _ -> { Ticks = 0L; Bytes = 0L }))
    let mutable cache = PlanCache.empty
    for _ in 1..5 do
        for line in lines do
            let struct (_, warmed) = runOne font text line (Array.init stageCount (fun _ -> { Ticks = 0L; Bytes = 0L })) cache
            cache <- warmed
    for pass in 0 .. passes - 1 do
        for line in lines do
            let struct (shaped, passCache) = runOne font text line perPass[pass] cache
            cache <- passCache
            ignore shaped.Infos.Length
    let totalTicks = Array.init stageCount (fun index -> Array.sumBy (fun (p: Measurement array) -> p[index].Ticks) perPass)
    let totalBytes = Array.init stageCount (fun index -> Array.sumBy (fun (p: Measurement array) -> p[index].Bytes) perPass)
    let minima = Array.init stageCount (fun index -> perPass |> Array.map (fun (p: Measurement array) -> p[index]) |> Array.minBy (fun m -> m.Ticks))
    struct (totalTicks, totalBytes, minima)

/// <summary>Prints one stage table from the totals.</summary>
/// <param name="title">The table title.</param>
/// <param name="names">The stage names.</param>
/// <param name="glyphArrays">The glyph-record array counts.</param>
/// <param name="totalTicks">The per-stage tick totals.</param>
/// <param name="totalBytes">The per-stage byte totals.</param>
/// <param name="minima">The per-stage minima.</param>
/// <param name="passes">The number of passes the totals cover.</param>
/// <param name="glyphs">The glyph count per pass.</param>
/// <param name="totalRow">The optional real-total row, printed instead of the sum.</param>
let private printTable (title: string) (names: string array) (glyphArrays: int array) (totalTicks: int64 array) (totalBytes: int64 array) (minima: Measurement array) (passes: int) (glyphs: int) (totalRow: struct (float * float) voption) =
    let frequency = float Stopwatch.Frequency
    let sumTicks = Array.sum totalTicks
    let sumBytes = Array.sum totalBytes
    Console.WriteLine()
    Console.WriteLine($"## {title} — {glyphs} glyphs per pass")
    Console.WriteLine("| stage | mean µs/pass | min µs/pass | time % | bytes/pass | bytes % | glyph arrays/run |")
    Console.WriteLine("|---|---:|---:|---:|---:|---:|---:|")
    for index in 0 .. names.Length - 1 do
        let ticks = totalTicks[index]
        let bytes = totalBytes[index]
        let meanUs = float ticks / float passes / frequency * 1e6
        let minUs = float minima[index].Ticks / frequency * 1e6
        let timeShare = meanUs / (float sumTicks / float passes / frequency * 1e6) * 100.0
        let byteShare = float bytes / float sumBytes * 100.0
        Console.WriteLine(
            String.Format(
                "| {0,-26} | {1,13:N1} | {2,9:N1} | {3,6:N2} | {4,12:N0} | {5,6:N2} | {6,7} |",
                names[index], meanUs, minUs, timeShare, float bytes / float passes, byteShare, glyphArrays[index]))
    match totalRow with
    | ValueSome (totalUs, totalBytesPerPass) ->
        Console.WriteLine(
            String.Format(
                "| **total (Shape.shapeCached)** | {0,13:N1} | | 100.00 | {1,12:N0} | 100.00 | {2,7} |",
                totalUs, totalBytesPerPass, Array.sum glyphArrays))
    | ValueNone ->
        Console.WriteLine(
            String.Format(
                "| **total** | {0,13:N1} | | 100.00 | {1,12:N0} | 100.00 | {2,7} |",
                float sumTicks / float passes / frequency * 1e6, float sumBytes / float passes, Array.sum glyphArrays))

/// <summary>Measures one whole pass through <c>Shape.shapeCached</c>, the real total.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines.</param>
/// <param name="passes">The number of timed passes.</param>
/// <returns>The minimum pass time in microseconds and bytes per pass.</returns>
let private measureShapeCached (font: Font) (text: byte array) (lines: struct (int * int) array) (passes: int) =
    let frequency = float Stopwatch.Frequency
    let mutable cache = PlanCache.empty
    let shapePass () =
        let mutable glyphs = 0
        for line in lines do
            let struct (start, length) = line
            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (shaped, next) = Shape.shapeCached cache font ImmutableArray<Feature>.Empty ValueNone run
            cache <- next
            match shaped with
            | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
            | Error _ -> ()
        glyphs
    for _ in 1..5 do shapePass () |> ignore
    let mutable minUs = Double.MaxValue
    let mutable bytesPerPass = 0.0
    for _ in 1..passes do
        let beforeBytes = GC.GetAllocatedBytesForCurrentThread()
        let before = Stopwatch.GetTimestamp()
        shapePass () |> ignore
        let elapsed = Stopwatch.GetTimestamp() - before
        let allocated = GC.GetAllocatedBytesForCurrentThread() - beforeBytes
        let us = float elapsed / frequency * 1e6
        minUs <- min minUs us
        bytesPerPass <- float allocated
    struct (minUs, bytesPerPass)

/// <summary>Times one execution entry alone: the runs and plans are built before the clock starts, so the figure is the
/// cost of the entry, either <c>Execute.run</c> (the simple-run kernel first) or <c>Execute.runGeneral</c> (the general
/// pipeline only). The plans are changed by <paramref name="adjust"/> before the clock starts.</summary>
/// <param name="adjust">The change of each plan, applied once.</param>
/// <param name="execute">The entry to time.</param>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines to shape.</param>
/// <param name="passes">The number of timed passes.</param>
/// <returns>The minimum pass time in microseconds and the bytes per pass.</returns>
let private measureEntryAdjusted (adjust: ShapePlan -> ShapePlan) (execute: ShapePlan -> Font -> ImmutableArray<Feature> -> GlyphRun -> Result<GlyphRun, ShapeError>) (font: Font) (text: byte array) (lines: struct (int * int) array) (passes: int) =
    let frequency = float Stopwatch.Frequency
    let mutable cache = PlanCache.empty
    let prepared =
        lines
        |> Array.map (fun line ->
            let struct (start, length) = line
            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (planResult, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
            cache <- next
            match planResult with
            | Ok plan -> struct (adjust plan, run)
            | Error _ -> failwith "plan creation failed")
    let entryPass () =
        let mutable glyphs = 0
        for struct (plan, run) in prepared do
            match execute plan font ImmutableArray<Feature>.Empty run with
            | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
            | Error _ -> ()
        glyphs
    let warmUntil = Stopwatch.GetTimestamp() + Stopwatch.Frequency
    while Stopwatch.GetTimestamp() < warmUntil do entryPass () |> ignore
    let mutable minUs = Double.MaxValue
    let mutable bytesPerPass = 0.0
    for _ in 1..passes do
        let beforeBytes = GC.GetAllocatedBytesForCurrentThread()
        let before = Stopwatch.GetTimestamp()
        entryPass () |> ignore
        let elapsed = Stopwatch.GetTimestamp() - before
        let allocated = GC.GetAllocatedBytesForCurrentThread() - beforeBytes
        minUs <- min minUs (float elapsed / frequency * 1e6)
        bytesPerPass <- float allocated
    struct (minUs, bytesPerPass)

/// <summary>Times the plan execution entry over the lines of a text, <see cref="measureEntryAdjusted"/> with the plans as built.</summary>
/// <param name="execute">The entry.</param>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines.</param>
/// <param name="passes">The number of timed passes.</param>
/// <returns>Microseconds and bytes of the fastest pass.</returns>
let private measureEntry (execute: ShapePlan -> Font -> ImmutableArray<Feature> -> GlyphRun -> Result<GlyphRun, ShapeError>) (font: Font) (text: byte array) (lines: struct (int * int) array) (passes: int) =
    measureEntryAdjusted id execute font text lines passes

/// <summary>Profiles one input: five warm-up passes, then twenty timed passes, and printed tables.</summary>
/// <param name="title">The input name.</param>
/// <param name="font">The stripped font.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines to shape.</param>
let private profileInput (title: string) (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let passes = 20
    let glyphs = countGlyphs font text lines

    let struct (realTicks, realBytes, realMinima) =
        measureStages font text lines RealStageCount passes runRealStages
    let struct (totalUs, totalBytes) = measureShapeCached font text lines passes
    printTable $"{title} — real pipeline" realStageNames realGlyphRecordArrays realTicks realBytes realMinima passes glyphs (ValueSome (totalUs, totalBytes))
    let struct (entryUs, entryBytes) = measureEntry Execute.run font text lines passes
    Console.WriteLine(String.Format("Execute.run alone (runs and plans prepared): {0:N1} µs per pass, {1:N0} B per pass, {2:N1} ns per glyph, {3:N0} ns per line", entryUs, entryBytes, entryUs * 1000.0 / float glyphs, entryUs * 1000.0 / float lines.Length))
    let struct (generalUs, generalBytes) = measureEntry Execute.runGeneral font text lines passes
    Console.WriteLine(String.Format("Execute.runGeneral alone (the general pipeline): {0:N1} µs per pass, {1:N0} B per pass, {2:N1} ns per glyph, {3:N0} ns per line", generalUs, generalBytes, generalUs * 1000.0 / float glyphs, generalUs * 1000.0 / float lines.Length))
    Console.WriteLine()

    let struct (refTicks, refBytes, refMinima) =
        measureStages font text lines RefStageCount passes runReferenceStages
    printTable $"{title} — reference decomposition (for comparison)" refStageNames refGlyphRecordArrays refTicks refBytes refMinima passes glyphs ValueNone

/// <summary>Names a path of the kernel entry.</summary>
/// <param name="path">The path.</param>
/// <returns>Its name.</returns>
let private pathName (path: SimplePath) =
    match path with
    | LookupFree -> "lookup-free"
    | NoLookupApplies -> "no lookup applies"
    | SubstitutionUnchanged -> "substitution unchanged"
    | SubstitutionFast -> "substitution fast"
    | SubstitutionGeneral -> "substitution general"
    | PositioningGeneral -> "positioning general"
    | PositioningFast -> "positioning fast"
    | PositioningTail -> "positioning tail"
    | AatSimple -> "aat simple"

/// <summary>Groups the lines of a text by the path of the kernel entry on a font, and times each group alone.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines.</param>
/// <returns>One report line per path: lines, glyphs and the minimum time of the group.</returns>
let private pathReport (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let mutable cache = PlanCache.empty
    let paths =
        lines
        |> Array.map (fun line ->
            let struct (start, length) = line
            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (planResult, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
            cache <- next
            match planResult with
            | Ok plan ->
                match SimpleRun.tryShapePath plan font run with
                | ValueSome (struct (path, _)) -> pathName path
                | ValueNone -> "general pipeline"
            | Error _ -> "no plan")
    paths
    |> Array.zip lines
    |> Array.groupBy snd
    |> Array.map (fun (name, group) ->
        let groupLines = group |> Array.map fst
        let struct (us, _) = measureEntry Execute.run font text groupLines 20
        $"{name}: {groupLines.Length} lines, {countGlyphs font text groupLines} glyphs, {us:N1} µs per pass")
    |> String.concat "; "

/// <summary>Times the execution entries on one input for a font whose layout tables are present, against the same text
/// on the layout-stripped font, and reports the paths the lines take.</summary>
/// <param name="title">The input name.</param>
/// <param name="stripped">The layout-stripped font.</param>
/// <param name="font">The font with layout tables.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines to shape.</param>
let private profileEntries (title: string) (stripped: Font) (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let glyphs = countGlyphs font text lines
    let struct (strippedUs, strippedBytes) = measureEntry Execute.run stripped text lines 20
    let struct (entryUs, entryBytes) = measureEntry Execute.run font text lines 20
    let noLookups (plan: ShapePlan) = { plan with GsubCovered = ImmutableArray.Empty; GposCovered = ImmutableArray.Empty; GsubLookupCovered = ImmutableArray.Empty; GsubGlyphLookups = ImmutableArray.Empty }
    let struct (floorUs, floorBytes) = measureEntryAdjusted noLookups Execute.run font text lines 20
    let struct (generalUs, generalBytes) = measureEntry Execute.runGeneral font text lines 20
    Console.WriteLine($"## {title} — layout tables present — {glyphs} glyphs per pass")
    Console.WriteLine("| entry | min µs/pass | bytes/pass | ns/glyph | ns/line | vs stripped |")
    Console.WriteLine("|---|---:|---:|---:|---:|---:|")
    let row (name: string) (us: float) (bytes: float) =
        Console.WriteLine(String.Format("| {0} | {1,11:N1} | {2,12:N0} | {3,8:N1} | {4,7:N0} | {5,6:N2}x |", name, us, bytes, us * 1000.0 / float glyphs, us * 1000.0 / float lines.Length, us / strippedUs))
    row "Execute.run, layout stripped" strippedUs strippedBytes
    row "Execute.run, layout present, lookups skipped by hand" floorUs floorBytes
    row "Execute.run, layout present" entryUs entryBytes
    row "Execute.runGeneral, layout present" generalUs generalBytes
    Console.WriteLine($"paths on the layout font: {pathReport font text lines}")
    Console.WriteLine()

/// <summary>Loads the stripped Roboto font for the two pairs.</summary>
/// <returns>The font.</returns>
let private loadStrippedRoboto () : Font =
    let originalPath, _ = Corpus.load "Roboto/en-thelittleprince"
    let strippedPath = stripFont originalPath
    Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes strippedPath)) 0u)

/// <summary>The number of lines in the per-line benchmark.</summary>
[<Literal>]
let private PerLineCount = 10000

/// <summary>Builds a text of <paramref name="count"/> lines, each the given ASCII word.</summary>
/// <param name="word">The word each line holds, without the newline.</param>
/// <param name="count">The number of lines.</param>
/// <returns>The UTF-8 text, newline-terminated.</returns>
let private perLineText (word: string) (count: int) =
    let builder = StringBuilder()
    for _ in 1..count do
        builder.Append word |> ignore
        builder.Append '\n' |> ignore
    Encoding.UTF8.GetBytes (builder.ToString())

/// <summary>Shapes one line with a reused native buffer, as the per-line benchmark measures HarfBuzz.</summary>
/// <param name="font">The native font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="remaining">The text from the line start to the end.</param>
/// <param name="length">The line length in bytes.</param>
let private shapeHarfBuzzLine (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (remaining: ReadOnlySpan<byte>) (length: int) =
    buffer.ClearContents()
    buffer.AddUtf8(remaining, 0, length)
    buffer.GuessSegmentProperties()
    font.Shape(buffer)

/// <summary>Measures Farfbuzz and HarfBuzz on one line text, reporting nanoseconds and bytes per line.</summary>
/// <param name="font">The stripped Farfbuzz font.</param>
/// <param name="hbFont">The native font.</param>
/// <param name="label">The label describing the line shape.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines.</param>
let private measurePerLine (font: Font) (hbFont: HarfBuzzSharp.Font) (label: string) (text: byte array) (lines: struct (int * int) array) =
    let frequency = float Stopwatch.Frequency
    let mutable cache = PlanCache.empty
    let shapeFarfbuzzPass () =
        let mutable glyphs = 0
        for line in lines do
            let struct (start, length) = line
            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (shaped, next) = Shape.shapeCached cache font ImmutableArray<Feature>.Empty ValueNone run
            cache <- next
            match shaped with
            | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
            | Error _ -> ()
        glyphs
    use hbBuffer = new HarfBuzzSharp.Buffer()
    let shapeHarfBuzzPass () =
        let mutable glyphs = 0
        for line in lines do
            let struct (start, length) = line
            shapeHarfBuzzLine hbFont hbBuffer (ReadOnlySpan(text, start, text.Length - start)) length
            glyphs <- glyphs + hbBuffer.Length
        glyphs
    for _ in 1..5 do
        shapeFarfbuzzPass () |> ignore
        shapeHarfBuzzPass () |> ignore
    let mutable farfbuzzMinNs = Double.MaxValue
    let mutable farfbuzzBytes = 0L
    let mutable harfBuzzMinNs = Double.MaxValue
    for _ in 1..20 do
        let beforeBytes = GC.GetAllocatedBytesForCurrentThread()
        let before = Stopwatch.GetTimestamp()
        shapeFarfbuzzPass () |> ignore
        let elapsed = Stopwatch.GetTimestamp() - before
        let allocated = GC.GetAllocatedBytesForCurrentThread() - beforeBytes
        farfbuzzMinNs <- min farfbuzzMinNs (float elapsed / frequency * 1e9 / float lines.Length)
        farfbuzzBytes <- allocated
    for _ in 1..20 do
        let before = Stopwatch.GetTimestamp()
        shapeHarfBuzzPass () |> ignore
        let elapsed = Stopwatch.GetTimestamp() - before
        harfBuzzMinNs <- min harfBuzzMinNs (float elapsed / frequency * 1e9 / float lines.Length)
    Console.WriteLine($"## {label} — {lines.Length} lines")
    Console.WriteLine($"| engine | ns/line (min) | bytes/line |")
    Console.WriteLine($"|---|---:|---:|")
    Console.WriteLine(String.Format("| Farfbuzz | {0,11:N0} | {1,11:N0} |", farfbuzzMinNs, float farfbuzzBytes / float lines.Length))
    Console.WriteLine(String.Format("| HarfBuzz (native) | {0,11:N0} | {1,11} |", harfBuzzMinNs, "native, not measured"))

/// <summary>Times each stage of the front of the entry on the lines of a text: <c>addUtf8</c>, <c>guessSegmentProperties</c>,
/// the plan key and cache lookup, and the entry alone, one cumulative loop each, so the stages can be subtracted.</summary>
/// <param name="font">The font.</param>
/// <param name="label">The label describing the lines.</param>
/// <param name="text">The whole text.</param>
/// <param name="lines">The lines.</param>
let private measureFront (font: Font) (label: string) (text: byte array) (lines: struct (int * int) array) =
    let frequency = float Stopwatch.Frequency
    let cache = ref PlanCache.empty
    let decode (start: int) (length: int) =
        GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty |> TailCalls.settle
    let guessed (start: int) (length: int) =
        GlyphRun.guessSegmentProperties Language.cLocale (decode start length)
    let passes =
        [ "addUtf8", (fun () ->
            let mutable glyphs = 0
            for struct (start, length) in lines do
                glyphs <- glyphs + (decode start length).Infos.Length
            glyphs)
          "addUtf8 + guessSegmentProperties", (fun () ->
            let mutable glyphs = 0
            for struct (start, length) in lines do
                glyphs <- glyphs + (guessed start length).Infos.Length
            glyphs)
          "addUtf8 + guess + plan key and cache", (fun () ->
            let mutable glyphs = 0
            for struct (start, length) in lines do
                let run = guessed start length
                let struct (plan, next) = Shape.planFromCache cache.Value font ImmutableArray<Feature>.Empty ValueNone run
                cache.Value <- next
                match plan with
                | Ok _ -> glyphs <- glyphs + run.Infos.Length
                | Error _ -> ()
            glyphs)
          "whole Shape.shapeCached", (fun () ->
            let mutable glyphs = 0
            for struct (start, length) in lines do
                let run = guessed start length
                let struct (shaped, next) = Shape.shapeCached cache.Value font ImmutableArray<Feature>.Empty ValueNone run
                cache.Value <- next
                match shaped with
                | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
                | Error _ -> ()
            glyphs) ]
    Console.WriteLine($"## {label} — front of the entry, cumulative")
    Console.WriteLine("| stages | ns/line (min) | bytes/line |")
    Console.WriteLine("|---|---:|---:|")
    for (name, pass) in passes do
        let warmUntil = Stopwatch.GetTimestamp() + Stopwatch.Frequency
        while Stopwatch.GetTimestamp() < warmUntil do pass () |> ignore
        let mutable minNs = Double.MaxValue
        let mutable bytes = 0L
        for _ in 1..20 do
            let beforeBytes = GC.GetAllocatedBytesForCurrentThread()
            let before = Stopwatch.GetTimestamp()
            pass () |> ignore
            let elapsed = Stopwatch.GetTimestamp() - before
            bytes <- GC.GetAllocatedBytesForCurrentThread() - beforeBytes
            minNs <- min minNs (float elapsed / frequency * 1e9 / float lines.Length)
        Console.WriteLine(String.Format("| {0} | {1,11:N0} | {2,11:N0} |", name, minNs, float bytes / float lines.Length))
    Console.WriteLine()

/// <summary>Benchmarks the per-line cost: 10,000 lines of one glyph and 10,000 lines of seven glyphs, Farfbuzz
/// through <c>Shape.shapeCached</c> against native HarfBuzz on the same lines.</summary>
/// <returns>Zero.</returns>
let perLine () =
    Console.WriteLine("Per-line cost of the basic shaping pipeline (other sessions were building on this machine).")
    NativeShaping.oracle.Force() |> ignore
    let originalPath, _ = Corpus.load "Roboto/en-thelittleprince"
    let strippedPath = stripFont originalPath
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes strippedPath)) 0u)
    let hbFont = new HarfBuzzSharp.Font(new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile strippedPath, 0))
    hbFont.SetFunctionsOpenType()
    let oneGlyph = perLineText "a" PerLineCount
    let sevenGlyphs = perLineText "abcdefg" PerLineCount
    measurePerLine font hbFont "one glyph per line" oneGlyph (Corpus.lines oneGlyph)
    measurePerLine font hbFont "seven glyphs per line" sevenGlyphs (Corpus.lines sevenGlyphs)
    measureFront font "one glyph per line" oneGlyph (Corpus.lines oneGlyph)
    measureFront font "seven glyphs per line" sevenGlyphs (Corpus.lines sevenGlyphs)
    0

/// <summary>Runs the profile for the two Roboto pairs and the 1,000-character ASCII line.</summary>
/// <returns>Zero.</returns>
let run () : int =
    Console.WriteLine("Shape profile of the basic shaping pipeline (other sessions were building on this machine).")
    let font = loadStrippedRoboto ()

    let _, littlePrince = Corpus.load "Roboto/en-thelittleprince"
    let littlePrinceLines = Corpus.lines littlePrince
    checkAgreement font littlePrince littlePrinceLines[0]
    profileInput "Roboto/en-thelittleprince" font littlePrince littlePrinceLines

    let _, words = Corpus.load "Roboto/en-words"
    let wordLines = Corpus.lines words
    checkAgreement font words wordLines[0]
    profileInput "Roboto/en-words" font words wordLines

    let asciiLines = [| struct (0, asciiBytes.Length) |]
    checkAgreement font asciiBytes asciiLines[0]
    profileInput "1,000-character ASCII line" font asciiBytes asciiLines

    let layoutPath, _ = Corpus.load "Roboto/en-thelittleprince"
    let layoutFont = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes layoutPath)) 0u)
    profileEntries "Roboto/en-thelittleprince" font layoutFont littlePrince littlePrinceLines
    profileEntries "Roboto/en-words" font layoutFont words wordLines

    0

/// <summary>Times the sub-phases of <c>Normalize.run</c> and the position-default passes, per glyph, over the
/// Little Prince. Reconstructs each line through <c>rotateChars</c>, then times each sub-phase in a full pass.</summary>
/// <returns>Zero.</returns>
let profileInner () : int =
    Console.WriteLine("Inner profile of normalize and positionDefault (other sessions were building on this machine).")
    let font = loadStrippedRoboto ()
    let _, littlePrince = Corpus.load "Roboto/en-thelittleprince"
    let littlePrinceLines = Corpus.lines littlePrince
    let otPlan =
        match Execute.createPlan font { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale } ImmutableArray<Feature>.Empty ValueNone with
        | Ok plan -> match plan.Ot with | ValueSome p -> p | ValueNone -> failwith "no ot plan"
        | Error _ -> failwith "plan creation failed"

    let buildRun (line: struct (int * int)) : GlyphRun =
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(littlePrince, start, littlePrince.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let propped = OtProps.setUnicodeProps otPlan.Map.GlobalMask run
        let circled = OtProps.insertDottedCircle font propped
        let clustered = OtProps.formClusters circled
        let native = OtProps.ensureNativeDirection clustered
        OtSubstitute.rotateChars otPlan font otPlan.Props.Direction native

    let runs = Array.map buildRun littlePrinceLines
    let glyphs = Array.sumBy (fun (run: GlyphRun) -> run.Infos.Length) runs
    let frequency = float Stopwatch.Frequency

    for _ in 1..2 do
        for run in runs do Normalize.run otPlan font run |> ignore
        for run in runs do Font.nominalGlyphsAll font (run.Infos.AsSpan()) |> ignore
        for run in runs do Font.hAdvancePositions font (run.Infos.AsSpan()) |> ignore

    let measure (action: unit -> unit) : int64 =
        let before = Stopwatch.GetTimestamp()
        action ()
        Stopwatch.GetTimestamp() - before

    let normalizeTotal =
        measure (fun () -> for run in runs do Normalize.run otPlan font run |> ignore)
    let cmapTotal =
        measure (fun () -> for run in runs do Font.nominalGlyphsAll font (run.Infos.AsSpan()) |> ignore)
    let mapiTotal =
        measure (fun () -> for run in runs do Sealed.map (fun (p: GlyphProps) -> p) (run.Props.AsSpan()) |> ignore)
    let codepointsTotal =
        measure (fun () -> for run in runs do Sealed.map (fun (info: GlyphInfo) -> info.Codepoint) (run.Infos.AsSpan()) |> ignore)
    let advancesTotal =
        measure (fun () ->
            for run in runs do
                let codepoints = Sealed.map (fun (info: GlyphInfo) -> info.Codepoint) (run.Infos.AsSpan())
                Font.hAdvances font (codepoints.AsSpan()) |> ignore)
    let positionsTotal =
        measure (fun () ->
            for run in runs do
                let codepoints = Sealed.map (fun (info: GlyphInfo) -> info.Codepoint) (run.Infos.AsSpan())
                let advances = Font.hAdvances font (codepoints.AsSpan())
                Sealed.init run.Infos.Length (fun index -> { XAdvance = advances[index]; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }) |> ignore)

    let nsPerGlyph (ticks: int64) = float ticks / frequency * 1e9 / float glyphs
    Console.WriteLine($"normalize total: {nsPerGlyph normalizeTotal:N2} ns/glyph")
    Console.WriteLine($"  nominalGlyphsAll (cmap batch): {nsPerGlyph cmapTotal:N2} ns/glyph")
    Console.WriteLine($"  Sealed.map identity: {nsPerGlyph mapiTotal:N2} ns/glyph")
    Console.WriteLine($"positionDefault:")
    Console.WriteLine($"  Sealed.map codepoints: {nsPerGlyph codepointsTotal:N2} ns/glyph")
    Console.WriteLine($"  codepoints + Font.hAdvances: {nsPerGlyph advancesTotal:N2} ns/glyph")
    Console.WriteLine($"  codepoints + hAdvances + Sealed.init positions: {nsPerGlyph positionsTotal:N2} ns/glyph")
    0

/// <summary>Builds the run of one line as the substitution stage receives it, the stages of the real pipeline before
/// <c>OtLayout.substitute</c>.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text.</param>
/// <param name="cache">The plan cache.</param>
/// <param name="line">The line.</param>
/// <returns>The plan, the run and the updated cache.</returns>
let private prepareSubstitution (font: Font) (text: byte array) (cache: PlanCache) (line: struct (int * int)) =
    let struct (start, length) = line
    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    let struct (planResult, cache) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
    let plan =
        match planResult with
        | Ok plan -> plan
        | Error _ -> failwith "plan creation failed"
    let otPlan =
        match plan.Ot with
        | ValueSome p -> p
        | ValueNone -> failwith "no ot plan"
    let run = GlyphRun.enter run
    let run = OtProps.setUnicodePropsFused otPlan.Map.GlobalMask run
    let run = OtProps.insertDottedCircle font run
    let run = OtProps.formClusters run
    let run = OtProps.ensureNativeDirection run
    let run = OtSubstitute.rotateChars otPlan font otPlan.Props.Direction run
    let run = Normalize.run otPlan font run
    let run = OtSubstitute.setupMasks otPlan run
    let run =
        match otPlan.FallbackMarkPositioning with
        | true -> OtLayout.recategorizeMarks run
        | false -> run
    let run = Normalize.mapGlyphsFast run
    let run = OtSubstitute.substituteStart font run
    let run =
        match otPlan.FallbackGlyphClasses with
        | true -> OtProps.synthesizeGlyphClasses run
        | false -> run
    struct (plan, otPlan, run, cache)

/// <summary>The minimum time and the bytes of one pass of a substitution over the prepared lines.</summary>
/// <param name="font">The font.</param>
/// <param name="prepared">The plans and runs.</param>
/// <param name="substitute">The substitution.</param>
/// <returns>Microseconds and bytes of one pass.</returns>
let private timeSubstitution (font: Font) (prepared: struct (ShapePlan * OtShapePlan * GlyphRun) array) (substitute: ShapePlan -> OtShapePlan -> Font -> GlyphRun -> GlyphRun) =
    let pass () =
        let mutable glyphs = 0
        for struct (plan, otPlan, run) in prepared do
            glyphs <- glyphs + (substitute plan otPlan font run).Infos.Length
        glyphs
    let warm = Stopwatch.StartNew()
    while warm.ElapsedMilliseconds < 1000L do pass () |> ignore
    let mutable best = Int64.MaxValue
    for _ in 1..20 do
        let before = Stopwatch.GetTimestamp()
        pass () |> ignore
        let elapsed = Stopwatch.GetTimestamp() - before
        if elapsed < best then best <- elapsed
    let bytesBefore = GC.GetAllocatedBytesForCurrentThread()
    pass () |> ignore
    struct (float best * 1.0e6 / float Stopwatch.Frequency, GC.GetAllocatedBytesForCurrentThread() - bytesBefore)

/// <summary>Counts what each lookup of the real plan does to each line of a text on unstripped Roboto and times the
/// substitution of the whole text through the interpreter and through the kernel.</summary>
/// <param name="name">The corpus pair.</param>
/// <param name="font">The font.</param>
let private realLinesOf (name: string) (font: Font) =
    let _, text = Corpus.load name
    let lines = Corpus.lines text
    let mutable cache = PlanCache.empty
    let prepared =
        lines
        |> Array.map (fun line ->
            let struct (plan, otPlan, run, next) = prepareSubstitution font text cache line
            cache <- next
            struct (plan, otPlan, run))
        |> Array.filter (fun (struct (plan, _, run)) -> Sealed.anyGlyphIn plan.GsubCovered (run.Infos.AsSpan()) |> TailCalls.settle)
    let table = font.Ot.Gsub.Value
    let oneStage = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
    let mutable glyphs = 0
    let mutable linesTouched = 0
    let mutable lookupsTouched = 0
    let mutable lookupsChanged = 0
    let mutable lookupsGated = 0
    let mutable ligatures = 0
    for struct (_, otPlan, run) in prepared do
        glyphs <- glyphs + run.Infos.Length
        let lookups = otPlan.Map.GsubLookups
        let mutable current = run
        let mutable touched = false
        for index in 0 .. lookups.Length - 1 do
            let single = ImmutableArray.Create lookups[index]
            let next = Engine.applyTable true (Engine.GsubApplier()) table single oneStage font current
            match obj.ReferenceEquals(next, current) with
            | true -> lookupsGated <- lookupsGated + 1
            | false ->
                lookupsTouched <- lookupsTouched + 1
                touched <- true
                if next.Infos.AsSpan().ToArray() <> current.Infos.AsSpan().ToArray() || next.Props.AsSpan().ToArray() <> current.Props.AsSpan().ToArray() then
                    lookupsChanged <- lookupsChanged + 1
                if next.Infos.Length < current.Infos.Length then
                    ligatures <- ligatures + 1
            current <- next
        if touched then linesTouched <- linesTouched + 1
    let struct (referenceUs, referenceBytes) = timeSubstitution font prepared (fun _ otPlan font run -> Engine.substituteReference otPlan font run)
    let struct (kernelUs, kernelBytes) = timeSubstitution font prepared (fun _ otPlan font run -> Engine.substitute otPlan font run)
    let struct (applicableUs, applicableBytes) = timeSubstitution font prepared (fun plan otPlan font run -> Engine.substituteApplicable otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups run)
    let struct (applicableReferenceUs, applicableReferenceBytes) = timeSubstitution font prepared (fun plan otPlan font run -> Engine.substituteApplicableReference otPlan font plan.GsubLookupCovered run)
    Console.WriteLine($"#### {name}: {lines.Length} lines, of which {prepared.Length} hold a glyph some lookup covers and reach the substitution ({glyphs} glyphs)")
    Console.WriteLine($"- lines a lookup touched (a glyph the lookup may match stood in the line): {linesTouched}; lookups that touched a line: {lookupsTouched}; of them that changed glyphs or properties: {lookupsChanged}; that shortened the run (formed a ligature): {ligatures}; lookups the digest gate stopped: {lookupsGated}")
    Console.WriteLine("| substitution of the whole text | µs/pass | ns/glyph | ns/line | bytes/pass |")
    Console.WriteLine("|---|---:|---:|---:|---:|")
    let print (label: string) (us: float) (bytes: int64) =
        Console.WriteLine(String.Format("| {0} | {1,10:N1} | {2,8:N1} | {3,8:N0} | {4,12:N0} |", label, us, us * 1000.0 / float glyphs, us * 1000.0 / float lines.Length, bytes))
    print "interpreter, all lookups gated by the digest (Engine.substituteReference)" referenceUs referenceBytes
    print "kernel, all lookups gated by the digest (Engine.substitute)" kernelUs kernelBytes
    print "interpreter, applicable lookups (Engine.substituteApplicableReference)" applicableReferenceUs applicableReferenceBytes
    print "kernel, applicable lookups (Engine.substituteApplicable)" applicableUs applicableBytes
    if lookupsTouched > 0 then
        Console.WriteLine(String.Format("- interpreter: {0:N0} ns per touching lookup; kernel: {1:N0} ns per touching lookup", (referenceUs - 0.0) * 1000.0 / float lookupsTouched, kernelUs * 1000.0 / float lookupsTouched))
    Console.WriteLine()

/// <summary>Counts and times the substitution of the real lines of unstripped Roboto.</summary>
let realLineSteps () =
    Console.WriteLine("### Real lines, unstripped Roboto (substitution only, runs prepared by the real pipeline)")
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    realLinesOf "Roboto/en-thelittleprince" font
    realLinesOf "Roboto/en-words" font

/// <summary>Substitutes the Little Prince through the kernel or the interpreter for a number of seconds, so a sampling
/// profiler can attach to the process.</summary>
/// <param name="seconds">How long to run.</param>
/// <param name="kernel">Whether the kernel runs, the interpreter otherwise.</param>
/// <param name="changedOnly">Whether only the lines the substitution changes are run.</param>
let substitutionLoop (seconds: int) (kernel: bool) (changedOnly: bool) =
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let _, text = Corpus.load "Roboto/en-thelittleprince"
    let lines = Corpus.lines text
    let mutable cache = PlanCache.empty
    let prepared =
        lines
        |> Array.map (fun line ->
            let struct (plan, otPlan, run, next) = prepareSubstitution font text cache line
            cache <- next
            struct (plan, otPlan, run))
        |> Array.filter (fun (struct (plan, _, run)) -> Sealed.anyGlyphIn plan.GsubCovered (run.Infos.AsSpan()) |> TailCalls.settle)
    let prepared =
        match changedOnly with
        | false -> prepared
        | true -> prepared |> Array.filter (fun (struct (plan, otPlan, run)) -> not (obj.ReferenceEquals(Engine.substituteApplicable otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups run, run)))
    Console.WriteLine($"{prepared.Length} lines")
    let clock = Stopwatch.StartNew()
    let mutable glyphs = 0
    let mutable passes = 0
    Console.WriteLine($"substituting for {seconds} s, pid {Environment.ProcessId}")
    while clock.Elapsed.TotalSeconds < float seconds do
        for struct (plan, otPlan, run) in prepared do
            let result =
                match kernel with
                | true -> Engine.substituteApplicable otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups run
                | false -> Engine.substituteApplicableReference otPlan font plan.GsubLookupCovered run
            glyphs <- glyphs + result.Infos.Length
        passes <- passes + 1
    Console.WriteLine($"{passes} passes, {clock.Elapsed.TotalMilliseconds * 1000.0 / float passes:N1} µs per pass")
    glyphs

/// <summary>Splits the time of the lines the kernel entry shapes by the substitution fast path into the lookup kernel and
/// the rest of the entry, on the Little Prince with unstripped Roboto.</summary>
let fastLineSplit () =
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let _, text = Corpus.load "Roboto/en-thelittleprince"
    let lines = Corpus.lines text
    let mutable cache = PlanCache.empty
    let all =
        lines
        |> Array.map (fun line ->
            let struct (plan, otPlan, run, next) = prepareSubstitution font text cache line
            cache <- next
            let struct (start, length) = line
            let input = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale |> TailCalls.settle
            let path =
                match SimpleRun.tryShapePath plan font input with
                | ValueSome (struct (found, _)) -> pathName found
                | ValueNone -> "general"
            struct (path, plan, otPlan, run, line))
    for name in [ "substitution fast"; "substitution unchanged"; "no lookup applies" ] do
        let group = all |> Array.filter (fun (struct (path, _, _, _, _)) -> path = name)
        let prepared = group |> Array.map (fun (struct (_, plan, otPlan, run, _)) -> struct (plan, otPlan, run))
        let groupLines = group |> Array.map (fun (struct (_, _, _, _, line)) -> line)
        let glyphs = prepared |> Array.sumBy (fun (struct (_, _, run)) -> run.Infos.Length)
        let struct (kernelUs, kernelBytes) = timeSubstitution font prepared (fun plan otPlan font run -> Engine.substituteApplicable otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups run)
        let struct (entryUs, entryBytes) = measureEntry Execute.run font text groupLines 20
        Console.WriteLine($"{name}: {group.Length} lines, {glyphs} glyphs: lookup kernel alone {kernelUs:N1} µs ({kernelBytes:N0} B), whole entry {entryUs:N1} µs ({entryBytes:N0} B)")
    0

/// <summary>The minimum time and the bytes of one pass of an action over a group, after a warm-up of half a second.</summary>
/// <param name="action">One pass; returns a number so nothing is optimised away.</param>
/// <returns>Microseconds and bytes of the fastest of eighty passes.</returns>
let private timePass (action: unit -> int) =
    let warm = Stopwatch.StartNew()
    while warm.ElapsedMilliseconds < 500L do action () |> ignore
    let mutable best = Int64.MaxValue
    let mutable bytes = 0L
    for _ in 1..80 do
        let bytesBefore = GC.GetAllocatedBytesForCurrentThread()
        let before = Stopwatch.GetTimestamp()
        action () |> ignore
        let elapsed = Stopwatch.GetTimestamp() - before
        if elapsed < best then
            best <- elapsed
            bytes <- GC.GetAllocatedBytesForCurrentThread() - bytesBefore
    struct (float best * 1.0e6 / float Stopwatch.Frequency, bytes)

/// <summary>Splits the time of <c>Execute.run</c> on a font with layout tables, per group of lines that take the same
/// path, into the parts the entry runs around the lookups and the lookups themselves.</summary>
/// <param name="name">The corpus pair.</param>
/// <param name="font">The font with layout tables.</param>
let private layoutCostOf (name: string) (font: Font) =
    let _, text = Corpus.load name
    let lines = Corpus.lines text
    let mutable cache = PlanCache.empty
    let items =
        lines
        |> Array.map (fun line ->
            let struct (start, length) = line
            let input = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale |> TailCalls.settle
            let struct (plan, otPlan, pre, next) = prepareSubstitution font text cache line
            cache <- next
            let path =
                match SimpleRun.tryShapePath plan font input with
                | ValueSome (struct (found, _)) -> pathName found
                | ValueNone -> "general"
            struct (path, plan, otPlan, input, pre))
    let noLookups (plan: ShapePlan) = { plan with GsubCovered = ImmutableArray.Empty; GposCovered = ImmutableArray.Empty; GsubLookupCovered = ImmutableArray.Empty; GsubGlyphLookups = ImmutableArray.Empty }
    Console.WriteLine($"#### {name}: {lines.Length} lines")
    for group in [ "no lookup applies"; "substitution unchanged"; "substitution fast" ] do
        let members = items |> Array.filter (fun (struct (path, _, _, _, _)) -> path = group)
        let glyphs = members |> Array.sumBy (fun (struct (_, _, _, _, pre)) -> pre.Infos.Length)
        let built =
            members
            |> Array.map (fun (struct (_, plan, otPlan, input, pre)) ->
                let mask = otPlan.Map.GlobalMask
                let entered = GlyphRun.enter input
                let masked = { entered with Infos = Sealed.withMask mask (pre.Infos.AsSpan()); Props = pre.Props; Positions = ImmutableArray.Empty; HavePositions = false; ContentType = GlyphContent }
                let substituted = Engine.substituteApplicable otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups masked
                struct (plan, otPlan, input, pre, masked, substituted))
        let report (label: string) (action: struct (ShapePlan * OtShapePlan * GlyphRun * GlyphRun * GlyphRun * GlyphRun) -> int) =
            let struct (us, bytes) =
                timePass (fun () ->
                    let mutable total = 0
                    for item in built do
                        total <- total + action item
                    total)
            Console.WriteLine(String.Format("| {0,-62} | {1,9:N1} | {2,7:N0} | {3,11:N0} |", label, us, us * 1000.0 / float built.Length, bytes))
            us
        Console.WriteLine($"##### {group}: {members.Length} lines, {glyphs} glyphs")
        Console.WriteLine("| part | µs/pass | ns/line | B/pass |")
        Console.WriteLine("|---|---:|---:|---:|")
        let whole = report "Execute.run (whole entry)" (fun (struct (plan, _, input, _, _, _)) -> match Execute.run plan font ImmutableArray<Feature>.Empty input with | Ok shaped -> shaped.Infos.Length | Error _ -> 0)
        let floorPlans = built |> Array.map (fun (struct (plan, _, _, _, _, _)) -> noLookups plan)
        let floor =
            let struct (us, bytes) =
                timePass (fun () ->
                    let mutable total = 0
                    for index in 0 .. built.Length - 1 do
                        let struct (_, _, input, _, _, _) = built[index]
                        match Execute.run floorPlans[index] font ImmutableArray<Feature>.Empty input with
                        | Ok shaped -> total <- total + shaped.Infos.Length
                        | Error _ -> ()
                    total)
            Console.WriteLine(String.Format("| {0,-62} | {1,9:N1} | {2,7:N0} | {3,11:N0} |", "Execute.run, lookups skipped by hand (plans prepared)", us, us * 1000.0 / float built.Length, bytes))
            us
        let covering = report "lookupsCovering (one table read per glyph)" (fun (struct (plan, _, _, pre, _, _)) -> int (Sealed.lookupsCovering plan.GsubGlyphLookups (pre.Infos.AsSpan()) |> TailCalls.settle))
        let may = report "Engine.mayApply (covering plus the gates of the candidates)" (fun (struct (plan, otPlan, input, pre, _, _)) -> match Engine.mayApply otPlan font plan.GsubGlyphLookups otPlan.Map.GlobalMask input.Flags (pre.Infos.AsSpan()) |> TailCalls.settle with | true -> 1 | false -> 0)
        let gpos = report "Sealed.anyGlyphIn plan.GposCovered" (fun (struct (plan, _, _, pre, _, _)) -> match Sealed.anyGlyphIn plan.GposCovered (pre.Infos.AsSpan()) |> TailCalls.settle with | true -> 1 | false -> 0)
        let masked = report "GlyphRun.enter, Sealed.withMask and the record (the run the lookups read)" (fun (struct (_, otPlan, input, pre, _, _)) ->
            let entered = GlyphRun.enter input
            let run = { entered with Infos = Sealed.withMask otPlan.Map.GlobalMask (pre.Infos.AsSpan()); Props = pre.Props; Positions = ImmutableArray.Empty; HavePositions = false; ContentType = GlyphContent }
            run.Infos.Length)
        let kernel = report "Engine.substituteApplicable (the lookup kernel)" (fun (struct (plan, otPlan, _, _, masked, _)) -> (Engine.substituteApplicable otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups masked).Infos.Length)
        let admitted = report "Engine.admit plus Engine.substituteAdmitted (one look at the glyphs, then the kernel)" (fun (struct (plan, otPlan, input, pre, masked, _)) ->
            let struct (_, admission) = Engine.admit otPlan font plan.GsubGlyphLookups otPlan.Map.GlobalMask input.Flags (pre.Infos.AsSpan()) |> TailCalls.settle
            (Engine.substituteAdmitted otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups admission masked).Infos.Length)
        ignore admitted
        let after =
            match group with
            | "substitution fast" -> report "SimpleRun.afterSubstitution (advances, flags, publish)" (fun (struct (_, otPlan, input, _, _, substituted)) -> let struct (_, shaped) = SimpleRun.afterSubstitution otPlan font input substituted in match shaped with | Ok run -> run.Infos.Length | Error _ -> 0)
            | _ -> 0.0
        (match group with
         | "substitution fast" ->
            let positioned = built |> Array.map (fun (struct (_, _, _, _, _, substituted)) -> { substituted with Positions = ImmutableArray.CreateRange(Seq.replicate substituted.Infos.Length { XAdvance = 1; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }); HavePositions = true })
            let timeParts (label: string) (action: OtShapePlan -> GlyphRun -> GlyphRun -> int) =
                let struct (us, bytes) =
                    timePass (fun () ->
                        let mutable total = 0
                        for index in 0 .. built.Length - 1 do
                            let struct (_, otPlan, input, _, _, _) = built[index]
                            total <- total + action otPlan input positioned[index]
                        total)
                Console.WriteLine(String.Format("|   {0,-60} | {1,9:N1} | {2,7:N0} | {3,11:N0} |", label, us, us * 1000.0 / float built.Length, bytes))
            timeParts "of which OtShaper.afterPosition (substitutePost, propagate, direction)" (fun otPlan input positioned -> (OtShaper.afterPosition otPlan font input.SegmentProperties.Direction positioned).Infos.Length)
            timeParts "of which OtSubstitute.substitutePost" (fun otPlan _ positioned -> (OtSubstitute.substitutePost otPlan font positioned).Infos.Length)
            timeParts "of which Flags.propagate" (fun _ _ positioned -> (Flags.propagate positioned).Infos.Length)
         | _ -> ())
        let finished = report "SimpleRun.finished-style result record (no work, harness floor)" (fun (struct (_, _, input, pre, _, _)) -> { input with Infos = pre.Infos }.Infos.Length)
        Console.WriteLine(String.Format("| residual: whole - floor - mayApply - gpos - masked - kernel - after | {0,9:N1} | {1,7:N0} | |", whole - floor - may - gpos - masked - kernel - after, (whole - floor - may - gpos - masked - kernel - after) * 1000.0 / float built.Length))
        ignore covering
        ignore finished
        Console.WriteLine()

/// <summary>Splits the time of the entry on both Roboto texts with layout tables present.</summary>
let layoutCost () =
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    layoutCostOf "Roboto/en-thelittleprince" font
    layoutCostOf "Roboto/en-words" font

/// <summary>Times <c>Execute.run</c> on both Roboto texts with the layout tables stripped and present, sixty passes each,
/// so that two builds can be compared one run after the other on a shared machine.</summary>
let entryPair () =
    let stripped = loadStrippedRoboto ()
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    for name in [ "Roboto/en-thelittleprince"; "Roboto/en-words" ] do
        let _, text = Corpus.load name
        let lines = Corpus.lines text
        let struct (strippedUs, strippedBytes) = measureEntry Execute.run stripped text lines 60
        let struct (presentUs, presentBytes) = measureEntry Execute.run font text lines 60
        Console.WriteLine(String.Format("{0}: stripped {1:N1} us ({2:N0} B), layout present {3:N1} us ({4:N0} B), ratio {5:N3}", name, strippedUs, strippedBytes, presentUs, presentBytes, presentUs / strippedUs))
