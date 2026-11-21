module Farfbuzz.Bench.VerifyCost

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open Farfbuzz
open Farfbuzz.Bench.Corpus

/// <summary>Time and bytes spent in one phase of the verification walk.</summary>
type private Phase() =
    member val Ticks = 0L with get, set
    member val Bytes = 0L with get, set
    member val Calls = 0L with get, set

/// <summary>Runs one phase, adding its ticks, bytes and call to the account.</summary>
/// <param name="phase">The account.</param>
/// <param name="work">The work.</param>
/// <returns>What the work returned.</returns>
let private measured (phase: Phase) (work: unit -> 'T) =
    let bytes = GC.GetAllocatedBytesForCurrentThread()
    let started = Stopwatch.GetTimestamp()
    let result = work ()
    phase.Ticks <- phase.Ticks + (Stopwatch.GetTimestamp() - started)
    phase.Bytes <- phase.Bytes + (GC.GetAllocatedBytesForCurrentThread() - bytes)
    phase.Calls <- phase.Calls + 1L
    result

/// <summary>The end of a forward fragment's text range.</summary>
let rec private textEndForward (text: ImmutableArray<GlyphInfo>) (numChars: int) (cluster: uint32) (finish: int) =
    if finish < numChars && text[finish].Cluster < cluster then textEndForward text numChars cluster (finish + 1) else finish

/// <summary>Walks one left-to-right line as Verify.breakLoop does, timing each phase.</summary>
let private walk (phases: Phase array) (font: Font) (plan: ShapePlan) (text: GlyphRun) (shaped: GlyphRun) =
    let template =
        { GlyphRun.empty with
            Flags = shaped.Flags &&& ~~~BufferFlags.Verify
            ClusterLevel = shaped.ClusterLevel
            Replacement = shaped.Replacement
            Invisible = shaped.Invisible
            NotFound = shaped.NotFound
            NotFoundVariationSelector = shaped.NotFoundVariationSelector }
    let numChars = text.Infos.Length
    let numGlyphs = shaped.Infos.Length
    let mutable reconstruction = template
    let mutable flags = template.Flags
    let mutable textStart = 0
    let mutable textEnd = 0
    let mutable finish = 1
    let mutable fragments = 0
    while finish <= numGlyphs do
        let boundary = finish < numGlyphs && (shaped.Infos[finish].Cluster = shaped.Infos[finish - 1].Cluster || shaped.Infos[finish].Mask &&& uint32 GlyphFlags.UnsafeToBreak <> 0u)
        if not boundary then
            let nextStart = textStart
            let nextEnd = if finish = numGlyphs then numChars else textEndForward text.Infos numChars shaped.Infos[finish].Cluster textEnd
            if nextStart < nextEnd then
                if nextStart > 0 then flags <- flags &&& ~~~BufferFlags.Bot
                if nextEnd < numChars then flags <- flags &&& ~~~BufferFlags.Eot
                let fragment = measured phases[0] (fun () -> GlyphRun.append text nextStart nextEnd { template with Flags = flags })
                let result = measured phases[1] (fun () -> Execute.run plan font ImmutableArray<Feature>.Empty fragment)
                match result with
                | Ok result ->
                    reconstruction <- measured phases[2] (fun () -> GlyphRun.append result 0 result.Infos.Length reconstruction)
                | Error _ -> ()
                fragments <- fragments + 1
                textStart <- nextEnd
                textEnd <- nextEnd
        finish <- finish + 1
    measured phases[3] (fun () -> Verify.diff reconstruction shaped ValueNone 0u) |> ignore
    fragments

/// <summary>Prints where a verified line's time and bytes go on one pair, left-to-right lines only.</summary>
/// <param name="pair">The corpus pair.</param>
let private report (pair: string) =
    let originalPath, bytes = Corpus.load pair
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes (BasicShapeBench.stripFont originalPath))) 0u)
    let lines = Corpus.lines bytes
    let phases = Array.init 4 (fun _ -> Phase())
    let mutable glyphs = 0L
    let mutable fragments = 0L
    let mutable plan = Unchecked.defaultof<ShapePlan>
    let one (count: bool) =
        for struct (start, length) in lines do
            let run = GlyphRun.addUtf8 (ReadOnlySpan(bytes, start, bytes.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run |> GlyphRun.withFlags BufferFlags.Verify
            let text = GlyphRun.append run 0 run.Infos.Length GlyphRun.empty
            if Object.ReferenceEquals(plan, null) then
                match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Ok created -> plan <- created
                | Error _ -> ()
            match Execute.run plan font ImmutableArray<Feature>.Empty run with
            | Ok shaped ->
                let found = walk phases font plan text shaped
                if count then
                    glyphs <- glyphs + int64 shaped.Infos.Length
                    fragments <- fragments + int64 found
            | Error _ -> ()
    one false
    for phase in phases do
        phase.Ticks <- 0L
        phase.Bytes <- 0L
        phase.Calls <- 0L
    one true
    let probe = Phase()
    let overheadStart = Stopwatch.GetTimestamp()
    for _ in 1 .. 1000000 do measured probe (fun () -> 0) |> ignore
    let overheadNs = float (Stopwatch.GetTimestamp() - overheadStart) * 1e9 / float Stopwatch.Frequency / 1e6
    let names = [| "fragment build (GlyphRun.append text range)"; "fragment shape (Execute.run)"; "reconstruction append"; "diff (once per line)" |]
    let ms (ticks: int64) = float ticks * 1000.0 / float Stopwatch.Frequency
    printfn "%s: %d lines, %d glyphs, %d fragments (%.1f glyphs per fragment, %.1f fragments per line); stopwatch overhead %.0f ns per call" pair lines.Length glyphs fragments (float glyphs / float fragments) (float fragments / float lines.Length) overheadNs
    Array.iteri (fun index (phase: Phase) ->
        printfn "  %-46s %8.1f ms (minus overhead %8.1f ms)  %12d bytes  %9d calls  %7.1f bytes/call" names[index] (ms phase.Ticks) (ms phase.Ticks - float phase.Calls * overheadNs / 1e6) phase.Bytes phase.Calls (float phase.Bytes / float phase.Calls)) phases

/// <summary>Runs the report on both Roboto texts.</summary>
let run () =
    report "Roboto/en-thelittleprince"
    report "Roboto/en-words"
