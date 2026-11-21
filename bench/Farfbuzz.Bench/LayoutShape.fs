module Farfbuzz.Bench.LayoutShape

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.NativeShaping

/// <summary>The layout-table tags a stripped copy of a font renames away.</summary>
let private layoutTags =
    [ "GSUB"; "GPOS"; "GDEF"; "kern"; "morx"; "mort"; "kerx"; "trak"; "ankr"; "feat" ]
    |> List.map (fun text -> uint32 (Tag.ofString text))
    |> Set.ofList

/// <summary>Writes a layout-stripped copy of a font and returns its path.</summary>
/// <param name="fontPath">The original font path.</param>
/// <returns>The stripped font's path.</returns>
let internal stripFont (fontPath: string) =
    let bytes = File.ReadAllBytes fontPath
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
    for index in 0 .. count - 1 do
        let record = 12 + index * 16
        if record + 4 <= bytes.Length && layoutTags.Contains (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4))) then
            bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "no-layout")
    Directory.CreateDirectory directory |> ignore
    let target = Path.Combine(directory, $"layout-shape-{Path.GetFileName fontPath}#no-layout")
    File.WriteAllBytes (target, bytes)
    target

/// <summary>Shapes every line of a text with Farfbuzz through a plan cache kept from one pass to the next, as HarfBuzz's
/// per-face cache lets the native job.</summary>
/// <param name="cache">The plan cache from the previous pass.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count and the cache to keep.</returns>
let private shapeFarfbuzz (cache: PlanCache) (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    let mutable kept = cache
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (shaped, next) = Shape.shapeCached kept font ImmutableArray<Feature>.Empty ValueNone run
        kept <- next
        match shaped with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    struct (glyphs, kept)

/// <summary>Shapes every line of a text with native HarfBuzz, reusing one buffer as benchmark-shape.cc does.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count.</returns>
let private shapeHarfBuzz (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    for line in lines do
        let struct (start, length) = line
        buffer.ClearContents()
        buffer.AddUtf8(ReadOnlySpan(text, start, text.Length - start), 0, length)
        buffer.GuessSegmentProperties()
        font.Shape(buffer)
        glyphs <- glyphs + buffer.Length
    glyphs

/// <summary>Shapes every line of a text with one plan through the entry, which is how a caller that holds a plan shapes.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines to shape.</param>
/// <returns>The total glyph count.</returns>
let private shapePlanned (plan: ShapePlan) (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    for line in lines do
        let struct (start, length) = line
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        match Execute.run plan font ImmutableArray<Feature>.Empty run with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    glyphs

/// <summary>The minimum time and the allocation of a pass over several passes.</summary>
/// <param name="passes">The number of passes.</param>
/// <param name="pass">One pass.</param>
/// <returns>The minimum microseconds and the bytes of the last pass.</returns>
let private minimum (passes: int) (pass: unit -> int) =
    pass () |> ignore
    let mutable best = Double.MaxValue
    let mutable bytes = 0L
    for _ in 1 .. passes do
        let before = GC.GetAllocatedBytesForCurrentThread()
        let started = Stopwatch.GetTimestamp()
        pass () |> ignore
        let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0
        bytes <- GC.GetAllocatedBytesForCurrentThread() - before
        best <- min best elapsed
    struct (best, bytes)

/// <summary>The minimum time and the allocation of a pass over several passes of work on arrays that the work writes: each pass
/// gets its own copies of the arrays of every run, made before the clock starts.</summary>
/// <param name="passes">The number of passes.</param>
/// <param name="runs">The runs whose infos and positions are copied.</param>
/// <param name="work">The work on a copy of a run's infos and positions.</param>
/// <returns>The minimum microseconds and the bytes of the last pass.</returns>
let private minimumOwned (passes: int) (runs: GlyphRun array) (work: ImmutableArray<GlyphInfo> -> ImmutableArray<GlyphPosition> -> int) =
    let mutable best = Double.MaxValue
    let mutable bytes = 0L
    for pass in 0 .. passes do
        let copies = runs |> Array.map (fun run -> struct (ImmutableArray.CreateRange run.Infos, ImmutableArray.CreateRange run.Positions))
        let before = GC.GetAllocatedBytesForCurrentThread()
        let started = Stopwatch.GetTimestamp()
        let mutable total = 0
        for struct (infos, positions) in copies do
            total <- total + work infos positions
        let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0
        if pass > 0 then
            best <- min best elapsed
            bytes <- GC.GetAllocatedBytesForCurrentThread() - before
    struct (best, bytes)

/// <summary>Times several pieces of work in rounds, each round one pass of each, so that load on the machine falls on all of
/// them alike, and keeps the minimum of each.</summary>
/// <param name="rounds">The number of rounds.</param>
/// <param name="works">The pieces of work, each returning the microseconds it took and the bytes it allocated.</param>
/// <returns>The minimum microseconds and the bytes of the last round, for each piece.</returns>
let private interleaved (rounds: int) (works: (unit -> struct (float * int64)) array) =
    let warm = Stopwatch.StartNew()
    while warm.ElapsedMilliseconds < 600L do
        for work in works do
            work () |> ignore
        if warm.ElapsedMilliseconds > 300L && warm.ElapsedMilliseconds < 320L then Threading.Thread.Sleep 250
    let best = Array.create works.Length Double.MaxValue
    let bytes = Array.zeroCreate<int64> works.Length
    for round in 0 .. rounds do
        for index in 0 .. works.Length - 1 do
            let struct (elapsed, allocated) = works[index] ()
            if round > 0 then
                best[index] <- min best[index] elapsed
                bytes[index] <- allocated
    Array.map2 (fun us b -> struct (us, b)) best bytes

/// <summary>One pass of work timed, with the bytes it allocated.</summary>
/// <param name="work">The work.</param>
/// <returns>The microseconds and the bytes.</returns>
let private timed (work: unit -> int) =
    let before = GC.GetAllocatedBytesForCurrentThread()
    let started = Stopwatch.GetTimestamp()
    work () |> ignore
    let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0
    struct (elapsed, GC.GetAllocatedBytesForCurrentThread() - before)

/// <summary>One pass of work on copies of the arrays of runs, timed after the copies are made.</summary>
/// <param name="runs">The runs.</param>
/// <param name="work">The work on a copy of a run's infos and positions.</param>
/// <returns>The microseconds and the bytes.</returns>
let private timedOwned (runs: GlyphRun array) (work: ImmutableArray<GlyphInfo> -> ImmutableArray<GlyphPosition> -> int) =
    let copies = runs |> Array.map (fun run -> struct (ImmutableArray.CreateRange run.Infos, ImmutableArray.CreateRange run.Positions))
    timed (fun () ->
        let mutable total = 0
        for struct (infos, positions) in copies do
            total <- total + work infos positions
        total)

/// <summary>Times Farfbuzz and native HarfBuzz on both Roboto texts with the layout tables stripped and present, in one
/// process, the minimum of many passes each.</summary>
let run () =
    NativeShaping.oracle.Force() |> ignore
    Console.WriteLine "| text | font | engine | us/pass | ns/glyph | B/pass | vs HarfBuzz |"
    Console.WriteLine "|---|---|---|---:|---:|---:|---:|"
    for name in [ "Roboto/en-thelittleprince"; "Roboto/en-words" ] do
        let path, text = Corpus.load name
        let lines = Corpus.lines text
        for stripped in (match Environment.GetEnvironmentVariable "LAYOUT_SHAPE_STRIPPED_ONLY" with | "1" -> [ true ] | _ -> [ true; false ]) do
            let fontPath = match stripped with | true -> stripFont path | false -> path
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes fontPath)) 0u)
            use face = new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile fontPath, 0)
            use hbFont = new HarfBuzzSharp.Font(face)
            hbFont.SetFunctionsOpenType()
            use buffer = new HarfBuzzSharp.Buffer()
            let mutable cache = PlanCache.empty
            let glyphs = shapeHarfBuzz hbFont buffer text lines
            let struct (hbUs, _) = minimum 60 (fun () -> shapeHarfBuzz hbFont buffer text lines)
            let struct (fUs, fBytes) =
                minimum 60 (fun () ->
                    let struct (total, kept) = shapeFarfbuzz cache font text lines
                    cache <- kept
                    total)
            let label = match stripped with | true -> "stripped" | false -> "with layout"
            let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
            match stripped, Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
            | false, Ok plan ->
                let noGpos = { plan with GposCovered = ImmutableArray.Empty }
                let struct (planned, plannedBytes) = minimum 60 (fun () -> shapePlanned plan font text lines)
                let struct (skipped, skippedBytes) = minimum 60 (fun () -> shapePlanned noGpos font text lines)
                Console.WriteLine($"| {name} | {label} | Farfbuzz, one plan | {planned:N1} | {planned * 1000.0 / float glyphs:N2} | {plannedBytes:N0} | {planned / hbUs:N3} |")
                match plan.Ot with
                | ValueNone -> ()
                | ValueSome otPlan ->
                    for lookupMap in otPlan.Map.GposLookups do
                        let accel = font.Ot.Gpos.Value.Lookups[lookupMap.LookupIndex].Value
                        let kinds =
                            accel.Subtables
                            |> Seq.map (fun s -> match s.Subtable with | SinglePos _ -> "S" | PairPosGlyphs (c, _, _, _, _, _, _, _, _) -> $"P1[{(Coverage.glyphs c).Length}]" | PairPosClasses (c, _, _, _, _, _, _, _, _, _, _, _, _) -> $"P2[{(Coverage.glyphs c).Length}]" | CursivePos _ -> "C" | MarkBasePos _ -> "MB" | MarkLigPos _ -> "ML" | MarkMarkPos _ -> "MM" | PosContext _ -> "CX" | PosChainContext _ -> "CC" | PosPending (t, _, _, _) -> $"x{t}")
                            |> String.concat " "
                        Console.WriteLine($"lookup {lookupMap.LookupIndex} mask {lookupMap.Mask} props {accel.Props}: {kinds}")
                    let prepared =
                        lines
                        |> Array.choose (fun line ->
                            let struct (start, length) = line
                            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
                            match Execute.run noGpos font ImmutableArray<Feature>.Empty run with
                            | Ok shaped -> Some { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                            | Error _ -> None)
                    let struct (kernel, kernelBytes) =
                        minimum 60 (fun () ->
                            let mutable total = 0
                            for run in prepared do
                                total <- total + (Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run).Infos.Length
                            total)
                    Console.WriteLine($"| {name} | {label} | the positioning kernel alone, pair loop | {kernel:N1} | {kernel * 1000.0 / float glyphs:N2} | {kernelBytes:N0} | {kernel / hbUs:N3} |")
                    let struct (reference, referenceBytes) =
                        minimum 60 (fun () ->
                            let mutable total = 0
                            for run in prepared do
                                total <- total + (Engine.positionPlannedReference otPlan font plan.GposLookupCovered plan.GposGlyphLookups run).Infos.Length
                            total)
                    Console.WriteLine($"| {name} | {label} | the positioning kernel alone, general loop | {reference:N1} | {reference * 1000.0 / float glyphs:N2} | {referenceBytes:N0} | {reference / hbUs:N3} |")
                    let struct (lean, leanBytes) =
                        minimumOwned 60 (Array.ofSeq prepared) (fun infos positions ->
                            match Engine.positionOwned plan.GposPairs font infos positions BufferFlags.Default ClusterLevel.MonotoneGraphemes GlyphRun.MaxOpsDefault with
                            | ValueSome _ -> infos.Length
                            | ValueNone -> 0)
                    Console.WriteLine($"| {name} | {label} | the positioning kernel alone, lean loop | {lean:N1} | {lean * 1000.0 / float glyphs:N2} | {leanBytes:N0} | {lean / hbUs:N3} |")
                Console.WriteLine($"| {name} | {label} | Farfbuzz, one plan, GPOS gate closed by hand | {skipped:N1} | {skipped * 1000.0 / float glyphs:N2} | {skippedBytes:N0} | {skipped / hbUs:N3} |")
            | _ -> ()
            Console.WriteLine($"| {name} | {label} | HarfBuzz | {hbUs:N1} | {hbUs * 1000.0 / float glyphs:N2} | n/a | 1.000 |")
            Console.WriteLine($"| {name} | {label} | Farfbuzz | {fUs:N1} | {fUs * 1000.0 / float glyphs:N2} | {fBytes:N0} | {fUs / hbUs:N3} |")

/// <summary>Loops the positioning kernel over the prepared lines of unstripped Roboto for a number of seconds, so that a
/// sampling profiler can look at it.</summary>
/// <param name="seconds">How long to loop.</param>
let kernelLoop (seconds: int) =
    let path, text = Corpus.load "Roboto/en-thelittleprince"
    let lines = Corpus.lines text
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    match Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> ()
    | Ok plan ->
        let noGpos = { plan with GposCovered = ImmutableArray.Empty }
        match plan.Ot with
        | ValueNone -> ()
        | ValueSome otPlan ->
            let synthetic = match Environment.GetEnvironmentVariable "GPOS_LOOP_TEXT" with | null -> String.Empty | value -> value
            let prepared =
                match String.IsNullOrEmpty synthetic with
                | false ->
                    let bytes = Text.Encoding.UTF8.GetBytes (String.replicate 5000 synthetic)
                    let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
                    match Execute.run noGpos font ImmutableArray<Feature>.Empty run with
                    | Ok shaped -> [| { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) } |]
                    | Error _ -> [||]
                | true ->
                lines
                |> Array.choose (fun line ->
                    let struct (start, length) = line
                    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
                    match Execute.run noGpos font ImmutableArray<Feature>.Empty run with
                    | Ok shaped -> Some { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                    | Error _ -> None)
            Console.WriteLine($"pid {Environment.ProcessId}")
            let watch = Stopwatch.StartNew()
            let mutable total = 0
            let lean = Environment.GetEnvironmentVariable "GPOS_LOOP_LEAN" = "1"
            while watch.Elapsed.TotalSeconds < float seconds do
                for run in prepared do
                    match lean with
                    | true ->
                        match Engine.positionOwned plan.GposPairs font run.Infos run.Positions BufferFlags.Default ClusterLevel.MonotoneGraphemes GlyphRun.MaxOpsDefault with
                        | ValueSome _ -> total <- total + run.Infos.Length
                        | ValueNone -> ()
                    | false -> total <- total + (Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run).Infos.Length
            Console.WriteLine total

/// <summary>Whether a glyph is in a bit set of glyph ids.</summary>
/// <param name="set">The words of the set.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>True when the glyph is in the set.</returns>
let private inSet (set: ImmutableArray<uint64>) (glyph: uint32) =
    let word = int (glyph >>> 6)
    word < set.Length && (set[word] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL

/// <summary>Times the positioning kernel on runs of 10,000 glyphs of unstripped Roboto: a run no lookup covers and a run whose
/// every neighbouring pair is kerned, each the minimum of many passes.</summary>
let gposCost () =
    NativeShaping.oracle.Force() |> ignore
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    match Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> ()
    | Ok plan ->
        let noGpos = { plan with GposCovered = ImmutableArray.Empty }
        match plan.Ot with
        | ValueNone -> ()
        | ValueSome otPlan ->
            let prepare (text: string) =
                let bytes = Text.Encoding.UTF8.GetBytes text
                let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
                match Execute.run noGpos font ImmutableArray<Feature>.Empty run with
                | Ok shaped -> { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                | Error _ -> GlyphRun.empty
            let uncovered =
                [| 0x20u .. 0x24Fu |]
                |> Array.tryFind (fun codepoint ->
                    match Font.nominalGlyph font codepoint with
                    | ValueSome glyph -> not (inSet plan.GposCovered glyph)
                    | ValueNone -> false)
            let report (label: string) (run: GlyphRun) =
                let glyphs = run.Infos.Length
                let lean infos positions =
                    match Engine.positionOwned plan.GposPairs font infos positions BufferFlags.Default ClusterLevel.MonotoneGraphemes GlyphRun.MaxOpsDefault with
                    | ValueSome _ -> infos.Length
                    | ValueNone -> 0
                let results =
                    interleaved 300 [|
                        (fun () -> timed (fun () -> (Engine.positionPlannedReference otPlan font plan.GposLookupCovered plan.GposGlyphLookups run).Infos.Length))
                        (fun () -> timed (fun () -> (Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run).Infos.Length))
                        (fun () -> timedOwned [| run |] lean) |]
                for (kind, struct (us, bytes)) in Array.zip [| "general loop"; "pair loop"; "lean loop" |] results do
                    Console.WriteLine($"| {label}, {kind} | {glyphs} | {us:N1} | {us * 1000.0 / float glyphs:N2} | {bytes:N0} |")
                let only (position: int) =
                    { plan.GposPairs with PositionTables = plan.GposPairs.PositionTables |> Seq.map (fun tables -> match tables.IsEmpty with | true -> tables | false -> ImmutableArray.Create tables[position]) |> ImmutableArray.CreateRange }
                let leanWith (program: PairProgram) infos positions =
                    match Engine.positionOwned program font infos positions BufferFlags.Default ClusterLevel.MonotoneGraphemes GlyphRun.MaxOpsDefault with
                    | ValueSome _ -> infos.Length
                    | ValueNone -> 0
                let partial = interleaved 300 [| (fun () -> timedOwned [| run |] (leanWith (only 0))); (fun () -> timedOwned [| run |] (leanWith (only 1))) |]
                for (kind, struct (us, bytes)) in Array.zip [| "lean loop, only the first table"; "lean loop, only the second table" |] partial do
                    Console.WriteLine($"| {label}, {kind} | {glyphs} | {us:N1} | {us * 1000.0 / float glyphs:N2} | {bytes:N0} |")
            Console.WriteLine "| run | glyphs | us/pass | ns/glyph | B/pass |"
            Console.WriteLine "|---|---:|---:|---:|---:|"
            match uncovered with
            | Some codepoint -> report $"no glyph covered by a lookup (U+{codepoint:X4})" (prepare (String.replicate 10000 (string (char codepoint))))
            | None -> Console.WriteLine "| no uncovered glyph in U+0020..U+024F | | | | |"
            report "every neighbouring pair kerned (AV)" (prepare (String.replicate 5000 "AV"))

/// <summary>Times the positioning kernel on runs of 10,000 glyphs and a table of one attachment lookup: a run with no glyph the lookup
/// starts at, a run of marks with no base, a base and a mark alternating so that every mark attaches, and cursive attachment of every
/// glyph to the one before, each the minimum of many passes.</summary>
let attachCost () =
    let font = Font.create Face.empty |> Font.withScale 1000 1000
    let coverageOf (glyphs: uint32 array) = CoverageGlyphs (ImmutableArray.CreateRange glyphs)
    let direct = { Table = ImmutableArray<uint16>.Empty; Complete = false }
    let lookupOf (lookupType: int) (subtable: PosSubtable) =
        LayoutAccel.build GposTables.subtableDigest GposTables.secondDigest { LookupType = lookupType; Props = 0u; IsReverse = false; Subtables = ImmutableArray.Create subtable }
    let tableOf (lookup: LookupAccel<PosSubtable>) =
        { Version = 0x00010000u; Scripts = ImmutableArray.Empty; Features = ImmutableArray.Empty; Lookups = ImmutableArray.Create (lazy lookup); FeatureVariations = ReadOnlyMemory.Empty }
    let markBase =
        MarkBasePos
            { MarkCoverage = coverageOf [| 5u |]
              MarkDirect = direct
              TargetCoverage = coverageOf [| 1u |]
              TargetDirect = direct
              ClassCount = 1
              MarkClasses = ImmutableArray.Create 0us
              MarkAnchors = ImmutableArray.Create (PlainAnchor (10, 10))
              Rows = 1
              Anchors = ImmutableArray.Create (PlainAnchor (300, 400)) }
    let cursive =
        CursivePos
            { CursiveCoverage = coverageOf [| 1u |]
              CursiveDirect = direct
              Entries = ImmutableArray.Create (PlainAnchor (0, 10))
              Exits = ImmutableArray.Create (PlainAnchor (500, 30)) }
    let maps = ImmutableArray.Create { LookupIndex = 0; AutoZwnj = false; AutoZwj = false; Random = false; PerSyllable = false; Mask = 1u; FeatureTag = Tag.make 'm' 'a' 'r' 'k' }
    let stages = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
    let runOf (glyphOf: int -> uint32) (classOf: int -> uint16) =
        let count = 10000
        { GlyphRun.empty with
            Infos = Sealed.init count (fun index -> { Codepoint = glyphOf index; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
            Props = Sealed.init count (fun index -> { Unchecked.defaultof<GlyphProps> with GlyphClass = classOf index })
            Positions = Sealed.init count (fun _ -> { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
            HavePositions = true
            ContentType = GlyphContent
            SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale } }
    let report (label: string) (table: LayoutTable<PosSubtable>) (run: GlyphRun) =
        let gdef = font.Ot.Gdef.Value
        let covered = Engine.gposLookupCoveredOf table maps
        let glyphLookups = Engine.gposGlyphLookupsOf table maps
        let glyphs = run.Infos.Length
        let results = interleaved 300 [| (fun () -> timed (fun () -> (Engine.positionTable font table gdef maps stages covered glyphLookups ImmutableArray.Empty run).Infos.Length)) |]
        let struct (us, bytes) = results[0]
        Console.WriteLine($"| {label} | {glyphs} | {us:N1} | {us * 1000.0 / float glyphs:N2} | {bytes:N0} |")
    Console.WriteLine "| attachment run | glyphs | us/pass | ns/glyph | B/pass |"
    Console.WriteLine "|---|---:|---:|---:|---:|"
    let markTable = tableOf (lookupOf 4 markBase)
    report "mark lookup, no glyph it covers (gated)" markTable (runOf (fun _ -> 1u) (fun _ -> GlyphPropsBits.Base))
    report "mark lookup, marks and no base (covered, nothing attaches)" markTable (runOf (fun _ -> 5u) (fun _ -> GlyphPropsBits.Mark))
    report "mark lookup, attach a mark to every base" markTable (runOf (fun index -> match index % 2 with | 0 -> 1u | _ -> 5u) (fun index -> match index % 2 with | 0 -> GlyphPropsBits.Base | _ -> GlyphPropsBits.Mark))
    report "cursive lookup, attach every glyph to the one before" (tableOf (lookupOf 3 cursive)) (runOf (fun _ -> 1u) (fun _ -> GlyphPropsBits.Base))

/// <summary>Times the positioning kernel on runs of 10,000 glyphs and a table of a single adjustment lookup and one context
/// lookup: a run no glyph of which the lookup covers, a run it covers throughout and whose rules match nowhere (a context subtable
/// and a chained one), and a run of three glyphs repeating in which the lookup matches at every third glyph and applies the single
/// adjustment there, each the minimum of many passes.</summary>
let contextCost () =
    let font = Font.create Face.empty |> Font.withScale 1000 1000
    let coverageOf (glyph: uint32) = CoverageGlyphs (ImmutableArray.Create glyph)
    let direct = { Table = ImmutableArray<uint16>.Empty; Complete = false }
    let lookupOf (lookupType: int) (subtable: PosSubtable) =
        LayoutAccel.build GposTables.subtableDigest GposTables.secondDigest { LookupType = lookupType; Props = 0u; IsReverse = false; Subtables = ImmutableArray.Create subtable }
    let single = lookupOf 1 (SinglePos (coverageOf 1u, 4us, 1, ImmutableArray.Create 4s, ImmutableArray<Device>.Empty, direct))
    let tableOf (subtable: PosSubtable) (lookupType: int) =
        { Version = 0x00010000u; Scripts = ImmutableArray.Empty; Features = ImmutableArray.Empty; Lookups = ImmutableArray.Create (lazy single, lazy (lookupOf lookupType subtable)); FeatureVariations = ReadOnlyMemory.Empty }
    let record = { SequenceIndex = 1; LookupListIndex = 0 }
    let neverMatching = ImmutableArray.Create { InputCount = 2; Input = ImmutableArray.Create 99u; Records = ImmutableArray.Create record }
    let gated = tableOf (PosContext (GlyphRules (coverageOf 0xFFFFu, ImmutableArray.Create neverMatching))) 7
    let covered = tableOf (PosContext (GlyphRules (coverageOf 1u, ImmutableArray.Create neverMatching))) 7
    let chained = tableOf (PosChainContext (ChainCoverageRule (ImmutableArray.Create (coverageOf 99u), ImmutableArray.Create (coverageOf 1u, coverageOf 2u), ImmutableArray.Empty, ImmutableArray.Create record))) 8
    let everyThird = tableOf (PosContext (CoverageRule (ImmutableArray.Create (coverageOf 0u, coverageOf 1u), ImmutableArray.Create record))) 7
    let maps = ImmutableArray.Create { LookupIndex = 1; AutoZwnj = false; AutoZwj = false; Random = false; PerSyllable = false; Mask = 1u; FeatureTag = Tag.make 'm' 'a' 'r' 'k' }
    let stages = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
    let runOf (glyphOf: int -> uint32) =
        let count = 10000
        { GlyphRun.empty with
            Infos = Sealed.init count (fun index -> { Codepoint = glyphOf index; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
            Props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
            Positions = Sealed.init count (fun _ -> { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
            HavePositions = true
            ContentType = GlyphContent
            SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale } }
    let report (label: string) (table: LayoutTable<PosSubtable>) (run: GlyphRun) =
        let gdef = font.Ot.Gdef.Value
        let coveredSets = Engine.gposLookupCoveredOf table maps
        let glyphLookups = Engine.gposGlyphLookupsOf table maps
        let glyphs = run.Infos.Length
        let results = interleaved 300 [| (fun () -> timed (fun () -> (Engine.positionTable font table gdef maps stages coveredSets glyphLookups ImmutableArray.Empty run).Infos.Length)) |]
        let struct (us, bytes) = results[0]
        Console.WriteLine($"| {label} | {glyphs} | {us:N1} | {us * 1000.0 / float glyphs:N2} | {bytes:N0} |")
    Console.WriteLine "| context run | glyphs | us/pass | ns/glyph | B/pass |"
    Console.WriteLine "|---|---:|---:|---:|---:|"
    report "context lookup, no glyph it covers (gated)" gated (runOf (fun _ -> 1u))
    report "context lookup, covered throughout, no rule matches" covered (runOf (fun _ -> 1u))
    report "chained-context lookup, covered throughout, the backtrack matches nowhere" chained (runOf (fun _ -> 1u))
    report "context lookup, a match at every third glyph, one nested single adjustment" everyThird (runOf (fun index -> uint32 (index % 3)))
    let substitution (subtable: SubstSubtable) =
        let built lookupType (subtables: SubstSubtable list) =
            LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest { LookupType = lookupType; Props = 0u; IsReverse = false; Subtables = ImmutableArray.CreateRange subtables }
        { Version = 0x00010000u
          Scripts = ImmutableArray.Empty
          Features = ImmutableArray.Empty
          Lookups = ImmutableArray.Create (lazy (built 1 [ SingleDelta (coverageOf 1u, 4u, 0xFFFFu) ]), lazy (built 5 [ subtable ]))
          FeatureVariations = ReadOnlyMemory.Empty }
    let substRun (glyphOf: int -> uint32) =
        { GlyphRun.empty with
            Infos = Sealed.init 10000 (fun index -> { Codepoint = glyphOf index; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
            Props = Sealed.init 10000 (fun _ -> Unchecked.defaultof<GlyphProps>) }
    let substReport (label: string) (table: LayoutTable<SubstSubtable>) (run: GlyphRun) =
        let sets = Engine.lookupSetsOf table maps
        let glyphLookups = Engine.glyphLookupsOf table maps
        let results = interleaved 300 [| (fun () -> timed (fun () -> (Engine.applyGsubCovered false sets glyphLookups table maps stages font run).Infos.Length)) |]
        let struct (us, bytes) = results[0]
        Console.WriteLine($"| {label} | {run.Infos.Length} | {us:N1} | {us * 1000.0 / float run.Infos.Length:N2} | {bytes:N0} |")
    let substRecord = { SequenceIndex = 1; LookupListIndex = 0 }
    let substNever = ImmutableArray.Create { InputCount = 2; Input = ImmutableArray.Create 99u; Records = ImmutableArray.Create substRecord }
    substReport "the GSUB context lookup for comparison, covered throughout, no rule matches" (substitution (SubstContext (GlyphRules (coverageOf 1u, ImmutableArray.Create substNever)))) (substRun (fun _ -> 1u))
    substReport "the GSUB context lookup for comparison, a match at every third glyph, one nested single substitution" (substitution (SubstContext (CoverageRule (ImmutableArray.Create (coverageOf 0u, coverageOf 1u), ImmutableArray.Create substRecord)))) (substRun (fun index -> uint32 (index % 3)))

/// <summary>Counts what the positioning kernel does on the prepared lines of a Roboto text and times the parts of a pair
/// lookup apart, so the cost of pair kerning can be told per thing it does.</summary>
/// <param name="name">The corpus pair.</param>
let pairAnatomy (name: string) =
    let path, text = Corpus.load name
    let lines = Corpus.lines text
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    match Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> ()
    | Ok plan ->
        let noGpos = { plan with GposCovered = ImmutableArray.Empty }
        match plan.Ot with
        | ValueNone -> ()
        | ValueSome otPlan ->
            let prepared =
                lines
                |> Array.choose (fun line ->
                    let struct (start, length) = line
                    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
                    match Execute.run noGpos font ImmutableArray<Feature>.Empty run with
                    | Ok shaped -> Some { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                    | Error _ -> None)
            let glyphs = prepared |> Array.sumBy (fun run -> run.Infos.Length)
            let pairs = prepared |> Array.sumBy (fun run -> max 0 (run.Infos.Length - 1))
            let candidateCounts = Array.zeroCreate<int> 64
            let mutable withMarks = 0
            let mutable withIgnorables = 0
            let mutable partialMasks = 0
            for run in prepared do
                let bits = Sealed.lookupsCovering plan.GposGlyphLookups (run.Infos.AsSpan())
                for p in 0 .. 63 do
                    if (bits >>> p) &&& 1UL <> 0UL then candidateCounts[p] <- candidateCounts[p] + 1
                if run.Props.AsSpan().ToArray() |> Array.exists (fun p -> GlyphPropsBits.isMark p) then withMarks <- withMarks + 1
                if run.Props.AsSpan().ToArray() |> Array.exists (fun p -> UnicodeProps.isDefaultIgnorable p) then withIgnorables <- withIgnorables + 1
                if run.Infos.AsSpan().ToArray() |> Array.exists (fun i -> i.Mask <> otPlan.Map.GlobalMask) then partialMasks <- partialMasks + 1
            Console.WriteLine $"pair program: lean {plan.GposPairs.Lean:X}, idle {plan.GposPairs.Idle:X}, positions {plan.GposPairs.PositionTables.Length}, global mask {otPlan.Map.GlobalMask:X}"
            for t in plan.GposPairs.PositionTables do
                Console.WriteLine $"  tables {[ for x in t -> (x.Kind, x.Plain) ]}"
            for t in font.Ot.Gpos.Value.Lookups[otPlan.Map.GposLookups[0].LookupIndex].Value.Subtables do
                match t.Subtable with
                | PairPosGlyphs (c, _, _, _, seconds, values, _, _, _) ->
                    Console.WriteLine $"F1: {(Coverage.glyphs c).Length} first glyphs, {seconds.Length} pairs, {(seconds |> Seq.distinct |> Seq.length)} distinct seconds, {values |> Seq.filter (fun v -> v <> 0s) |> Seq.length} nonzero values"
                | PairPosClasses (c, _, _, _, _, n1, n2, values, _, _, _, _, _) ->
                    Console.WriteLine $"F2: {(Coverage.glyphs c).Length} first glyphs, {n1} x {n2} classes, {values |> Seq.filter (fun v -> v <> 0s) |> Seq.length} nonzero of {values.Length}"
                | _ -> ()
            Console.WriteLine $"{name}: {prepared.Length} lines, {glyphs} glyphs, {pairs} adjacent pairs; lines with a mark {withMarks}, with a default ignorable {withIgnorables}, with a mask other than the global one {partialMasks}"
            for p in 0 .. 63 do
                if candidateCounts[p] > 0 then
                    let lookupMap = otPlan.Map.GposLookups[p]
                    Console.WriteLine $"map position {p} (lookup {lookupMap.LookupIndex}, mask {lookupMap.Mask}) reaches {candidateCounts[p]} of {prepared.Length} lines"
            let lookupMap = otPlan.Map.GposLookups[0]
            let accel = font.Ot.Gpos.Value.Lookups[lookupMap.LookupIndex].Value
            let lookupProps = Gdef.sanitizeLookupProps font.Ot.Gdef.Value accel.Props
            let matcher = Matching.matcherFor 1 false lookupProps lookupMap.Mask lookupMap.AutoZwnj lookupMap.AutoZwj false 0uy
            let env = { Font = font; Direction = Direction.Ltr; Horizontal = true; Matcher = matcher; Gdef = font.Ot.Gdef.Value }
            let subtables = accel.Subtables
            Console.WriteLine $"lookup {lookupMap.LookupIndex}: {subtables.Length} subtables, lookup props {lookupProps}"
            let tried = Array.zeroCreate<int> 8
            let applied = Array.zeroCreate<int> 8
            let wonWithAdvance = Array.zeroCreate<int> 8
            let wonWithZero = Array.zeroCreate<int> 8
            let mutable pairsNone = 0
            let mutable covered = 0
            let mutable breakFlags = 0
            for run in prepared do
                let infos = run.Infos.AsSpan()
                let propsSpan = run.Props.AsSpan()
                let mutable index = 0
                let positionsSpan = (if run.HavePositions then run.Positions.AsSpan() else ReadOnlySpan<GlyphPosition>(Array.zeroCreate<GlyphPosition> infos.Length))
                while index < infos.Length do
                    let mutable won = false
                    let mutable s = 0
                    let mutable next = index + 1
                    while not won && s < subtables.Length do
                        let accelS = subtables[s]
                        match Digest.mayHaveCall infos[index].Codepoint accelS.SubtableDigest with
                        | false -> ()
                        | true ->
                            let outcome = GposLookups.applySubtable env accelS.Subtable infos propsSpan positionsSpan index
                            tried[s] <- tried[s] + 1
                            match outcome.Applied with
                            | false -> ()
                            | true ->
                                applied[s] <- applied[s] + 1
                                won <- true
                                next <- outcome.Next
                                match outcome.Advance <> 0 || outcome.FirstWorked with
                                | true -> wonWithAdvance[s] <- wonWithAdvance[s] + 1
                                | false -> wonWithZero[s] <- wonWithZero[s] + 1
                                if outcome.Break1To <> 0 then breakFlags <- breakFlags + 1
                        s <- s + 1
                    match won with
                    | true -> covered <- covered + 1
                    | false -> pairsNone <- pairsNone + 1
                    index <- (match won with | true -> max next (index + 1) | false -> index + 1)
            Console.WriteLine $"visits of the kern lookup {covered + pairsNone} ({covered} applied, {pairsNone} not); break flags written {breakFlags}"
            for s in 0 .. subtables.Length - 1 do
                Console.WriteLine $"subtable {s}: tried {tried[s]}, applied {applied[s]} (nonzero {wonWithAdvance[s]}, zero {wonWithZero[s]})"
            let runs = Array.ofSeq prepared
            let tablesOnly () =
                let mutable total = 0
                for run in runs do
                    let infos = run.Infos.AsSpan()
                    let propsSpan = run.Props.AsSpan()
                    let positionsSpan = run.Positions.AsSpan()
                    for index in 0 .. infos.Length - 1 do
                        let mutable s = 0
                        let mutable won = false
                        while not won && s < subtables.Length do
                            match Digest.mayHaveCall infos[index].Codepoint subtables[s].SubtableDigest with
                            | false -> ()
                            | true -> won <- (GposLookups.applySubtable env subtables[s].Subtable infos propsSpan positionsSpan index).Applied
                            s <- s + 1
                        if won then total <- total + 1
                total
            let leanWith infos positions =
                match Engine.positionOwned plan.GposPairs font infos positions BufferFlags.Default ClusterLevel.MonotoneGraphemes GlyphRun.MaxOpsDefault with
                | ValueSome _ -> 1
                | ValueNone -> 0
            let results =
                interleaved 40 [|
                    (fun () -> timed (fun () -> runs |> Array.sumBy (fun run -> (Engine.positionPlannedReference otPlan font plan.GposLookupCovered plan.GposGlyphLookups run).Infos.Length)))
                    (fun () -> timed tablesOnly)
                    (fun () -> timed (fun () -> runs |> Array.sumBy (fun run -> (Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run).Infos.Length)))
                    (fun () -> timedOwned runs leanWith) |]
            for (kind, struct (us, bytes)) in Array.zip [| "general loop (the old kernel), whole"; "the subtables alone, every glyph, nothing committed"; "pair loop (generic kernel, compiled tables)"; "lean loop, in place" |] results do
                Console.WriteLine($"{name}: {kind}: {us:N0} us per pass, {us * 1000.0 / float glyphs:N1} ns per glyph, {bytes:N0} B per pass")

/// <summary>A lookup that never applies.</summary>
[<Struct>]
type IdleLookup =
    interface IPosLookup with
        member _.Pairs = false
        member _.Pair (_, _, _, _) = PairStep.Nothing
        member _.Scan (infos, _, _) = infos.Length
        member _.Try (_, _, _, _) = PosOutcome(false, 0, Unchecked.defaultof<GlyphPosition>, false, 0, Unchecked.defaultof<GlyphPosition>, false, 0, 0, 0)
        member _.Attaches = false
        member _.Futile _ = false
        member _.Attach (_, _, _, _, lastBase, lastBaseUntil) = GposAttach.nothing -1 lastBase lastBaseUntil
        member _.HasContext = false
        member _.TryContext (_, _, _, _) = ContextOutcome(false, false, 0, 0, ImmutableArray.Empty, 0, 0, FlagBounds.none, -1)
        member _.Nest () = struct (0u, false, false)
        member _.Dense = false
        member _.DenseCount = 0
        member _.DenseHorizontal = true
        member _.MarkOf (_, _) = -1
        member _.OnMarks _ = false
        member _.BaseMatches (_, _) = false
        member _.PreviousMark (_, _, _) = -1
        member _.RowOf (_, _) = -1
        member _.AnchorCell (_, _, _) = -1
        member _.AnchorOffsetX (_, _, _) = 0
        member _.AnchorOffsetY (_, _, _) = 0

/// <summary>A program with no lookup at all, which shows what the kernel costs before any lookup is looked at.</summary>
[<Struct>]
type IdleProgram =
    val Glyphs: ImmutableArray<uint64>
    new (glyphs) = { Glyphs = glyphs }
    interface IPosProgram<IdleLookup> with
        member _.LookupEnd = 0
        member this.GlyphLookups = this.Glyphs
        member _.Admits (_, _, _, _) = false
        member _.Lookup _ = IdleLookup()
        member _.NestedLookup (_, _, _, _) = IdleLookup()

/// <summary>Times the positioning kernel on short lines, which tells what it costs per call apart from per glyph.</summary>
let lineCost () =
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    match Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> ()
    | Ok plan ->
        let noGpos = { plan with GposCovered = ImmutableArray.Empty }
        match plan.Ot with
        | ValueNone -> ()
        | ValueSome otPlan ->
            let prepare (text: string) =
                let bytes = Text.Encoding.UTF8.GetBytes text
                let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
                match Execute.run noGpos font ImmutableArray<Feature>.Empty run with
                | Ok shaped -> { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                | Error _ -> GlyphRun.empty
            let report (label: string) (text: string) =
                let run = prepare text
                let repeat = 1000
                let leanWork infos positions =
                    match Engine.positionOwned plan.GposPairs font infos positions run.Flags run.ClusterLevel run.MaxOps with
                    | ValueSome _ -> 1
                    | ValueNone -> 0
                let results =
                    interleaved 200 [|
                        (fun () -> timed (fun () ->
                            let mutable total = 0
                            for _ in 1 .. repeat do
                                total <- total + (Engine.positionPlannedReference otPlan font plan.GposLookupCovered plan.GposGlyphLookups run).Infos.Length
                            total))
                        (fun () -> timed (fun () ->
                            let mutable total = 0
                            for _ in 1 .. repeat do
                                total <- total + (Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs run).Infos.Length
                            total))
                        (fun () -> timedOwned (Array.create repeat run) leanWork) |]
                let cell (struct (us: float, bytes: int64)) = $"{us * 1000.0 / float repeat:N1} ns, {bytes / int64 repeat:N0} B"
                Console.WriteLine($"| {label} | {run.Infos.Length} | {cell results[0]} | {cell results[1]} | {cell results[2]} |")
            Console.WriteLine "| line | glyphs | general loop | pair loop | lean loop |"
            Console.WriteLine "|---|---:|---:|---:|---:|"
            report "warm-up" "!"
            report "one uncovered glyph" "!"
            report "uncovered, 7 glyphs" "!!!!!!!"
            report "AV, kerned" "AV"
            report "word, 7 glyphs" "hello w"
            report "AVAVAVA, 7 glyphs" "AVAVAVA"

/// <summary>Loops the positioning kernel on one short line for a number of seconds, so that a sampling profiler can look at the
/// cost of a call.</summary>
/// <param name="seconds">How long to loop.</param>
let lineLoop (seconds: int) =
    let path, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    match Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> ()
    | Ok plan ->
        let noGpos = { plan with GposCovered = ImmutableArray.Empty }
        match plan.Ot with
        | ValueNone -> ()
        | ValueSome otPlan ->
            let text = match Environment.GetEnvironmentVariable "GPOS_LINE_TEXT" with | null -> "!" | value -> value
            let bytes = Text.Encoding.UTF8.GetBytes text
            let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale
            let prepared =
                match Execute.run noGpos font ImmutableArray<Feature>.Empty run with
                | Ok shaped -> { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                | Error _ -> GlyphRun.empty
            Console.WriteLine($"pid {Environment.ProcessId}")
            let watch = Stopwatch.StartNew()
            let mutable total = 0
            while watch.Elapsed.TotalSeconds < float seconds do
                for _ in 1 .. 1000 do
                    total <- total + (Engine.positionPlanned otPlan font plan.GposLookupCovered plan.GposGlyphLookups plan.GposPairs prepared).Infos.Length
            Console.WriteLine total
