module Farfbuzz.Bench.GeneralShape

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Text
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.NativeShaping

/// <summary>How a row's lines are shaped by Farfbuzz.</summary>
type private Route =
    | Entry // Shape.shapeCached, which takes the simple-run kernel when the line qualifies
    | General // Execute.runLean, the general pipeline with its lean front half, the kernel not tried
    | Reference // Execute.runGeneral, the general pipeline stage by stage, the reference

/// <summary>One benchmark row: a font, the lines of a text, a forced direction and the route.</summary>
type private Row =
    { Name: string
      FontPath: string
      Text: byte array
      Direction: Direction voption
      Route: Route }

/// <summary>A deterministic pseudo-random generator, the next state and a value below a bound.</summary>
/// <param name="state">The state.</param>
/// <returns>The next state.</returns>
let private next (state: uint64) = state * 6364136223846793005UL + 1442695040888963407UL

/// <summary>A value below a bound from a state.</summary>
/// <param name="state">The state.</param>
/// <param name="bound">The exclusive bound.</param>
/// <returns>The value.</returns>
let private below (state: uint64) (bound: int) = int ((state >>> 33) % uint64 bound)

/// <summary>Builds a text of lines from a generator of one word.</summary>
/// <param name="lineCount">The number of lines.</param>
/// <param name="wordsPerLine">The words in a line.</param>
/// <param name="word">The word writer: the state, and the builder to append to; returns the next state.</param>
/// <returns>The UTF-8 text, every line ending in a newline.</returns>
let private generated (lineCount: int) (wordsPerLine: int) (word: uint64 -> StringBuilder -> uint64) =
    let builder = StringBuilder()
    let mutable state = 12345UL
    for _ in 1 .. lineCount do
        for index in 1 .. wordsPerLine do
            if index > 1 then builder.Append ' ' |> ignore
            state <- word state builder
        builder.Append '\n' |> ignore
    Encoding.UTF8.GetBytes (builder.ToString())

/// <summary>A Hebrew word with points: two to six letters, most with a point.</summary>
let private hebrewWord (state: uint64) (builder: StringBuilder) =
    let mutable state = next state
    let letters = 2 + below state 5
    for _ in 1 .. letters do
        state <- next state
        builder.Append (char (0x05D0 + below state 27)) |> ignore
        state <- next state
        if below state 4 <> 0 then
            builder.Append (char (0x05B0 + below state 13)) |> ignore
    state

/// <summary>A Thai word of one to three syllables with leading vowels, upper and lower vowels and tone marks.</summary>
let private thaiWord (state: uint64) (builder: StringBuilder) =
    let mutable state = next state
    let syllables = 1 + below state 3
    for _ in 1 .. syllables do
        state <- next state
        if below state 4 = 0 then builder.Append (char (0x0E40 + below state 5)) |> ignore
        state <- next state
        builder.Append (char (0x0E01 + below state 46)) |> ignore
        state <- next state
        match below state 4 with
        | 0 -> builder.Append (char 0x0E34) |> ignore
        | 1 -> builder.Append (char 0x0E38) |> ignore
        | 2 -> builder.Append (char 0x0E31) |> ignore
        | _ -> ()
        state <- next state
        if below state 3 = 0 then builder.Append (char (0x0E48 + below state 4)) |> ignore
        state <- next state
        if below state 3 = 0 then builder.Append (char 0x0E32) |> ignore
    state

/// <summary>Replaces letters of a text by accented forms, one in a fixed number of the letter's occurrences.</summary>
/// <param name="text">The text.</param>
/// <param name="table">The letter, its period and its replacement.</param>
/// <returns>The transformed text.</returns>
let private accented (text: string) (table: (char * int * string) list) =
    let counts = Collections.Generic.Dictionary<char, int>()
    let builder = StringBuilder()
    for letter in text do
        match table |> List.tryFind (fun (c, _, _) -> c = letter) with
        | Some (_, period, replacement) ->
            let seen = match counts.TryGetValue letter with | true, n -> n | false, _ -> 0
            counts[letter] <- seen + 1
            if seen % period = 0 then builder.Append replacement |> ignore else builder.Append letter |> ignore
        | None -> builder.Append letter |> ignore
    builder.ToString()

/// <summary>The rows of the benchmark.</summary>
let private rows () =
    let upstream = RepositoryPaths.upstream ()
    let roboto = Path.Combine(upstream, "perf", "fonts", "Roboto-Regular.ttf")
    let prince = Encoding.UTF8.GetString (File.ReadAllBytes (Path.Combine(upstream, "perf", "texts", "en-thelittleprince.txt")))
    let hebrewFont = Path.Combine(upstream, "test", "subset", "data", "fonts", "NotoIKEAHebrewLatin-Regular.ttf")
    let thaiFont = Path.Combine(upstream, "test", "fuzzing", "fonts", "kanit.ttf")
    let utf8 (text: string) = Encoding.UTF8.GetBytes text
    let frenchComposed = accented prince [ ('e', 3, "é"); ('a', 5, "à"); ('o', 7, "ô"); ('c', 9, "ç"); ('u', 6, "ù") ]
    let frenchDecomposed = accented prince [ ('e', 3, "é"); ('a', 5, "à"); ('o', 7, "ô"); ('c', 9, "ç"); ('u', 6, "ù") ]
    let vietnameseComposed = accented prince [ ('e', 3, "ế"); ('a', 4, "ạ"); ('o', 5, "ộ"); ('u', 6, "ữ"); ('i', 7, "ị") ]
    let vietnameseDecomposed = accented prince [ ('e', 3, "ế"); ('a', 4, "ạ"); ('o', 5, "ộ"); ('u', 6, "ữ"); ('i', 7, "ị") ]
    let stripped = LayoutShape.stripFont roboto
    [ { Name = "plain Latin stripped, kernel"; FontPath = stripped; Text = utf8 prince; Direction = ValueNone; Route = Entry }
      { Name = "plain Latin stripped, forced general (lean)"; FontPath = stripped; Text = utf8 prince; Direction = ValueNone; Route = General }
      { Name = "plain Latin stripped, forced general (reference)"; FontPath = stripped; Text = utf8 prince; Direction = ValueNone; Route = Reference }
      { Name = "plain Latin, kernel"; FontPath = roboto; Text = utf8 prince; Direction = ValueNone; Route = Entry }
      { Name = "plain Latin, forced general (lean)"; FontPath = roboto; Text = utf8 prince; Direction = ValueNone; Route = General }
      { Name = "plain Latin, forced general (reference)"; FontPath = roboto; Text = utf8 prince; Direction = ValueNone; Route = Reference }
      { Name = "French precomposed"; FontPath = roboto; Text = utf8 frenchComposed; Direction = ValueNone; Route = Entry }
      { Name = "French combining marks"; FontPath = roboto; Text = utf8 frenchDecomposed; Direction = ValueNone; Route = Entry }
      { Name = "Vietnamese precomposed"; FontPath = roboto; Text = utf8 vietnameseComposed; Direction = ValueNone; Route = Entry }
      { Name = "Vietnamese combining marks"; FontPath = roboto; Text = utf8 vietnameseDecomposed; Direction = ValueNone; Route = Entry }
      { Name = "Latin text, right-to-left"; FontPath = roboto; Text = utf8 prince; Direction = ValueSome Direction.Rtl; Route = Entry }
      { Name = "Hebrew with points"; FontPath = hebrewFont; Text = generated 1500 9 hebrewWord; Direction = ValueNone; Route = Entry }
      { Name = "Thai"; FontPath = thaiFont; Text = generated 1500 8 thaiWord; Direction = ValueNone; Route = Entry } ]

/// <summary>Builds a Farfbuzz run for a line.</summary>
/// <param name="direction">The forced direction, if any.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="start">The line start.</param>
/// <param name="length">The line length in bytes.</param>
/// <returns>The run with guessed segment properties.</returns>
let private makeRun (direction: Direction voption) (text: byte array) (start: int) (length: int) =
    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    match direction with
    | ValueSome forced -> GlyphRun.withDirection forced run
    | ValueNone -> run

/// <summary>Shapes every line with Farfbuzz.</summary>
/// <param name="row">The row.</param>
/// <param name="font">The font.</param>
/// <param name="plan">The plan for the forced route, built once.</param>
/// <param name="cache">The plan cache of the entry route.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count, the notdef count and the cache.</returns>
let private shapeFarfbuzz (row: Row) (font: Font) (plan: ShapePlan voption) (cache: PlanCache) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    let mutable notdef = 0
    let mutable kept = cache
    for struct (start, length) in lines do
        let run = makeRun row.Direction row.Text start length
        let shaped =
            match row.Route, plan with
            | General, ValueSome plan -> Execute.runLean plan font ImmutableArray<Feature>.Empty run
            | Reference, ValueSome plan -> Execute.runGeneral plan font ImmutableArray<Feature>.Empty run
            | _ ->
                let struct (shaped, next) = Shape.shapeCached kept font ImmutableArray<Feature>.Empty ValueNone run
                kept <- next
                shaped
        match shaped with
        | Ok shaped ->
            glyphs <- glyphs + shaped.Infos.Length
            for info in shaped.Infos do
                if info.Codepoint = 0u then notdef <- notdef + 1
        | Error _ -> ()
    struct (glyphs, notdef, kept)

/// <summary>Shapes every line with native HarfBuzz.</summary>
/// <param name="row">The row.</param>
/// <param name="font">The font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count.</returns>
let private shapeHarfBuzz (row: Row) (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    for struct (start, length) in lines do
        buffer.ClearContents()
        buffer.AddUtf8(ReadOnlySpan(row.Text, start, row.Text.Length - start), 0, length)
        buffer.GuessSegmentProperties()
        match row.Direction with
        | ValueSome Direction.Rtl -> buffer.Direction <- HarfBuzzSharp.Direction.RightToLeft
        | ValueSome _ -> ()
        | ValueNone -> ()
        font.Shape buffer
        glyphs <- glyphs + buffer.Length
    glyphs

/// <summary>One pass timed, with the bytes it allocated.</summary>
/// <param name="work">The work.</param>
/// <returns>The microseconds and the bytes.</returns>
let private timed (work: unit -> int) =
    let before = GC.GetAllocatedBytesForCurrentThread()
    let started = Stopwatch.GetTimestamp()
    work () |> ignore
    let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0
    struct (elapsed, GC.GetAllocatedBytesForCurrentThread() - before)

/// <summary>Times the rows of the general-pipeline benchmark, Farfbuzz against HarfBuzz, alternating the two so that load
/// falls on both, keeping the minimum of each.</summary>
let run () =
    NativeShaping.oracle.Force() |> ignore
    let rounds = match Environment.GetEnvironmentVariable "GENERAL_SHAPE_ROUNDS" with | null | "" -> 30 | text -> int text
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    Console.WriteLine "| text | lines | glyphs | notdef | HarfBuzz ms/pass | Farfbuzz ms/pass | HarfBuzz ns/glyph | Farfbuzz ns/glyph | Farfbuzz B/line | Farfbuzz / HarfBuzz |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
    for row in rows () do
        if String.IsNullOrEmpty only || row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            use face = new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile row.FontPath, 0)
            use hbFont = new HarfBuzzSharp.Font(face)
            hbFont.SetFunctionsOpenType()
            use buffer = new HarfBuzzSharp.Buffer()
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            let plan =
                match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
                | Ok plan -> ValueSome plan
                | Error _ -> ValueNone
            let mutable cache = PlanCache.empty
            let struct (glyphs, notdef, kept) = shapeFarfbuzz row font plan cache lines
            cache <- kept
            let mutable hbBest = Double.MaxValue
            let mutable ffBest = Double.MaxValue
            let mutable ffBytes = 0L
            for round in 0 .. rounds do
                let struct (hbUs, _) = timed (fun () -> shapeHarfBuzz row hbFont buffer lines)
                let struct (ffUs, bytes) =
                    timed (fun () ->
                        let struct (total, _, kept) = shapeFarfbuzz row font plan cache lines
                        cache <- kept
                        total)
                if round > 0 then
                    hbBest <- min hbBest hbUs
                    ffBest <- min ffBest ffUs
                    ffBytes <- bytes
            Console.WriteLine($"| {row.Name} | {lines.Length} | {glyphs} | {notdef} | {hbBest / 1000.0:N2} | {ffBest / 1000.0:N2} | {hbBest * 1000.0 / float glyphs:N1} | {ffBest * 1000.0 / float glyphs:N1} | {ffBytes / int64 lines.Length:N0} | {ffBest / hbBest:N2} |")

/// <summary>The stages of the general pipeline, in order.</summary>
let private stageNames =
    [| "enter"; "unicode props"; "dotted circle"; "clusters"; "native direction"; "preprocess hook"; "rotate chars"; "normalise"
       "setup masks"; "recategorise marks"; "map glyphs"; "glyph props"; "synthesise classes"; "GSUB"; "position"; "post hook"
       "propagate flags"; "direction + reset" |]

/// <summary>Where the time and the bytes of the general pipeline go: every stage timed over every line of a row, one pass
/// after warm-up, with the allocation of each.</summary>
let stages () =
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    Console.WriteLine "| text | stage | ns/glyph | % of time | B/line |"
    Console.WriteLine "|---|---|---:|---:|---:|"
    for row in rows () do
        if String.IsNullOrEmpty only || row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let ticks = Array.zeroCreate<int64> stageNames.Length
                    let bytes = Array.zeroCreate<int64> stageNames.Length
                    let mutable glyphs = 0
                    let mutable engineTicks = 0L
                    let mutable engineBytes = 0L
                    let measure (index: int) (work: unit -> 'a) =
                        let before = GC.GetAllocatedBytesForCurrentThread()
                        let started = Stopwatch.GetTimestamp()
                        let result = work ()
                        ticks[index] <- ticks[index] + (Stopwatch.GetTimestamp() - started)
                        bytes[index] <- bytes[index] + (GC.GetAllocatedBytesForCurrentThread() - before)
                        result
                    for pass in 0 .. 60 do
                        if pass = 60 then
                            Array.fill ticks 0 ticks.Length 0L
                            Array.fill bytes 0 bytes.Length 0L
                            glyphs <- 0
                        for struct (start, length) in lines do
                            let run = makeRun row.Direction row.Text start length
                            let target = run.SegmentProperties.Direction
                            let entered = measure 0 (fun () -> GlyphRun.enter run)
                            let propped = measure 1 (fun () -> OtProps.setUnicodePropsFused plan.Map.GlobalMask entered)
                            let circled = measure 2 (fun () -> OtProps.insertDottedCircle font propped)
                            let clustered = measure 3 (fun () -> OtProps.formClusters circled)
                            let native = measure 4 (fun () -> OtProps.ensureNativeDirection clustered)
                            let prepared = measure 5 (fun () -> ShaperHooks.preprocess plan font native)
                            let rotated = measure 6 (fun () -> OtSubstitute.rotateChars plan font plan.Props.Direction prepared)
                            let normalized = measure 7 (fun () -> Normalize.run plan font rotated)
                            let masked = measure 8 (fun () -> OtSubstitute.setupMasks plan normalized)
                            let recategorized = measure 9 (fun () -> match plan.FallbackMarkPositioning with | true -> OtLayout.recategorizeMarks masked | false -> masked)
                            let mapped = measure 10 (fun () -> Normalize.mapGlyphsFast recategorized)
                            let started = measure 11 (fun () -> OtSubstitute.substituteStart font mapped)
                            let synthesized = measure 12 (fun () -> match plan.FallbackGlyphClasses with | true -> OtProps.synthesizeGlyphClasses started | false -> started)
                            let substituted = measure 13 (fun () -> OtLayout.substitute plan font synthesized)
                            let positioned = measure 14 (fun () -> OtPosition.position plan font substituted)
                            if pass = 60 then
                                let cleared = GlyphRun.clearPositions substituted
                                let defaulted = { cleared with Positions = Font.hAdvancePositions font (cleared.Infos.AsSpan()) }
                                let started = OtLayout.positionStart defaulted
                                let before = GC.GetAllocatedBytesForCurrentThread()
                                let t0 = Stopwatch.GetTimestamp()
                                OtLayout.position plan font started |> ignore
                                engineTicks <- engineTicks + (Stopwatch.GetTimestamp() - t0)
                                engineBytes <- engineBytes + (GC.GetAllocatedBytesForCurrentThread() - before)
                            match positioned with
                            | Error _ -> ()
                            | Ok positioned ->
                                let post = measure 15 (fun () -> OtSubstitute.substitutePost plan font positioned)
                                let propagated = measure 16 (fun () -> Flags.propagate post)
                                let final = measure 17 (fun () -> let shaped = GlyphRun.withDirection target propagated in { shaped with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u })
                                glyphs <- glyphs + final.Infos.Length
                    let mutable frontTicks = 0L
                    let mutable wholeTicks = 0L
                    let mutable frontBytes = 0L
                    let mutable wholeBytes = 0L
                    let mutable measuredGlyphs = 0
                    for pass in 0 .. 40 do
                        for struct (start, length) in lines do
                            let run = makeRun row.Direction row.Text start length
                            let b0 = GC.GetAllocatedBytesForCurrentThread()
                            let t0 = Stopwatch.GetTimestamp()
                            let normalized = OtShaper.normalised plan font (GlyphRun.enter run)
                            let t1 = Stopwatch.GetTimestamp()
                            let b1 = GC.GetAllocatedBytesForCurrentThread()
                            let struct (_, shaped) = SimpleRun.shapeGeneralPath shapePlan plan font run
                            let t2 = Stopwatch.GetTimestamp()
                            let b2 = GC.GetAllocatedBytesForCurrentThread()
                            if pass = 40 then
                                frontTicks <- frontTicks + (t1 - t0)
                                wholeTicks <- wholeTicks + (t2 - t1)
                                frontBytes <- frontBytes + (b1 - b0)
                                wholeBytes <- wholeBytes + (b2 - b1)
                                measuredGlyphs <- measuredGlyphs + normalized.Infos.Length
                                ignore shaped
                    Console.WriteLine($"| {row.Name} | (front half alone: OtShaper.normalised) | {float frontTicks * 1e9 / float Stopwatch.Frequency / float glyphs:N1} | n/a | {frontBytes / int64 lines.Length:N0} |")
                    Console.WriteLine($"| {row.Name} | (lean general, whole line) | {float wholeTicks * 1e9 / float Stopwatch.Frequency / float glyphs:N1} | n/a | {wholeBytes / int64 lines.Length:N0} |")
                    let total = Array.sum ticks
                    for index in 0 .. stageNames.Length - 1 do
                        let ns = float ticks[index] * 1e9 / float Stopwatch.Frequency
                        Console.WriteLine($"| {row.Name} | {stageNames[index]} | {ns / float glyphs:N1} | {100.0 * float ticks[index] / float total:N1} | {bytes[index] / int64 lines.Length:N0} |")
                    Console.WriteLine($"| {row.Name} | (of position: the GPOS engine alone) | {float engineTicks * 1e9 / float Stopwatch.Frequency / float glyphs:N1} | n/a | {engineBytes / int64 lines.Length:N0} |")
                    let ns = float total * 1e9 / float Stopwatch.Frequency
                    Console.WriteLine($"| {row.Name} | total | {ns / float glyphs:N1} | 100.0 | {Array.sum bytes / int64 lines.Length:N0} |")

/// <summary>The fonts whose plan building is measured: the three of the task and the largest of the corpus.</summary>
let private planFonts () =
    let upstream = RepositoryPaths.upstream ()
    let subset name = Path.Combine(upstream, "test", "subset", "data", "fonts", name)
    [ "Roboto", Path.Combine(upstream, "perf", "fonts", "Roboto-Regular.ttf")
      "Open Sans", Path.Combine(upstream, "test", "api", "fonts", "OpenSans-Regular.ttf")
      "Noto Sans Bold", Path.Combine(upstream, "test", "api", "fonts", "NotoSans-Bold.ttf")
      "Amiri", subset "Amiri-Regular.ttf"
      "STIX Two Math", subset "STIXTwoMath-Regular.ttf"
      "Noto Nastaliq Urdu", subset "NotoNastaliqUrdu-Regular.ttf"
      "Harmattan", subset "Harmattan-Regular.ttf"
      "Roboto Flex", subset "RobotoFlex-Variable.ttf"
      "M PLUS 1", subset "MPLUS1-Variable.ttf" ]

/// <summary>A feature that the plan will carry, to make the second plan on a face a different one.</summary>
/// <param name="tag">The four-letter tag.</param>
/// <returns>The feature, global and on.</returns>
let private globalFeature (tag: string) =
    { Tag = Tag.ofString tag; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }

/// <summary>Times <c>Execute.createPlan</c> on several faces: the first plan on a fresh font, the second plan with another
/// feature set on the same font, and the same again, minimum of repeated builds, with the allocation of each.</summary>
let planCost () =
    Console.WriteLine "| font | file KB | first plan us | first plan KB | second plan (other features) us | second plan KB | repeat (same features) us | repeat KB |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|"
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    let none = ImmutableArray<Feature>.Empty
    let kernOff = ImmutableArray.Create { globalFeature "kern" with Value = 0u }
    let ligaOff = ImmutableArray.Create { globalFeature "liga" with Value = 0u }
    let smcp = ImmutableArray.Create (globalFeature "smcp")
    let sets = [| kernOff; ligaOff; smcp; none |]
    for name, path in planFonts () do
        let bytes = File.ReadAllBytes path
        let once (font: Font) (features: ImmutableArray<Feature>) =
            let before = GC.GetAllocatedBytesForCurrentThread()
            let started = Stopwatch.GetTimestamp()
            Execute.createPlan font props features ValueNone |> ignore
            let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0
            struct (elapsed, float (GC.GetAllocatedBytesForCurrentThread() - before) / 1024.0)
        let mutable firstUs = Double.MaxValue
        let mutable firstKb = 0.0
        let mutable secondUs = Double.MaxValue
        let mutable secondKb = 0.0
        let mutable repeatUs = Double.MaxValue
        let mutable repeatKb = 0.0
        for trial in 0 .. 4 do
            let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
            let struct (us, kb) = once font none
            if trial > 0 then
                firstUs <- min firstUs us
                firstKb <- kb
            let struct (us2, kb2) = once font sets[trial % 3]
            if trial > 0 then
                secondUs <- min secondUs us2
                secondKb <- kb2
            let struct (us3, kb3) = once font sets[trial % 3]
            if trial > 0 then
                repeatUs <- min repeatUs us3
                repeatKb <- kb3
        Console.WriteLine($"| {name} | {bytes.Length / 1024} | {firstUs:N0} | {firstKb:N0} | {secondUs:N0} | {secondKb:N0} | {repeatUs:N0} | {repeatKb:N0} |")

/// <summary>Times <c>Execute.createPlan</c> on warm faces, many builds of the same plan and of alternating feature sets, and reports
/// the minimum time and the bytes of the last build.</summary>
let planWarm () =
    Console.WriteLine "| font | warm plan us (min of 200) | warm plan KB | alternating feature sets us | alternating KB |"
    Console.WriteLine "|---|---:|---:|---:|---:|"
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    let sets = [| ImmutableArray<Feature>.Empty; ImmutableArray.Create { globalFeature "kern" with Value = 0u }; ImmutableArray.Create { globalFeature "liga" with Value = 0u }; ImmutableArray.Create (globalFeature "smcp") |]
    for name, path in planFonts () do
        let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
        for set in sets do
            Execute.createPlan font props set ValueNone |> ignore
        let measure (features: int -> ImmutableArray<Feature>) =
            let mutable best = Double.MaxValue
            let mutable kb = 0.0
            for trial in 0 .. 200 do
                let before = GC.GetAllocatedBytesForCurrentThread()
                let started = Stopwatch.GetTimestamp()
                Execute.createPlan font props (features trial) ValueNone |> ignore
                let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0
                if trial > 0 then best <- min best elapsed
                kb <- float (GC.GetAllocatedBytesForCurrentThread() - before) / 1024.0
            struct (best, kb)
        let struct (warm, warmKb) = measure (fun _ -> ImmutableArray<Feature>.Empty)
        let struct (alt, altKb) = measure (fun trial -> sets[trial % sets.Length])
        Console.WriteLine($"| {name} | {warm:N0} | {warmKb:N0} | {alt:N0} | {altKb:N0} |")

/// <summary>Where the time and the bytes of a second plan go, piece by piece, on each font: the map, and each set or table
/// <c>Execute.createPlan</c> builds from it.</summary>
let planPieces () =
    Console.WriteLine "| font | piece | us | KB |"
    Console.WriteLine "|---|---|---:|---:|"
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
    let features = ImmutableArray.Create { globalFeature "kern" with Value = 0u }
    for name, path in planFonts () do
        let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
        Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone |> ignore
        let best = Collections.Generic.Dictionary<string, struct (float * float)>()
        let order = Collections.Generic.List<string>()
        let measure (label: string) (work: unit -> 'a) =
            let before = GC.GetAllocatedBytesForCurrentThread()
            let started = Stopwatch.GetTimestamp()
            let result = work ()
            let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds * 1000.0
            let kb = float (GC.GetAllocatedBytesForCurrentThread() - before) / 1024.0
            if not (best.ContainsKey label) then order.Add label
            match best.TryGetValue label with
            | true, struct (us, _) when us <= elapsed -> best[label] <- struct (us, kb)
            | _ -> best[label] <- struct (elapsed, kb)
            result
        for _ in 1 .. 6 do
            measure "Execute.createPlan (all)" (fun () -> Execute.createPlan font props features ValueNone) |> ignore
            let plan = measure "OtPlan.create (map)" (fun () -> OtPlan.create font props features)
            measure "gsubCovered" (fun () -> Engine.gsubCovered plan font) |> ignore
            measure "gposCovered" (fun () -> Engine.gposCovered plan font) |> ignore
            measure "gsubLookupCovered" (fun () -> Engine.gsubLookupCovered plan font) |> ignore
            let gsubGl = measure "gsubGlyphLookups" (fun () -> Engine.gsubGlyphLookups plan font)
            let covered = measure "gposLookupCovered" (fun () -> Engine.gposLookupCovered plan font)
            let glyphLookups = measure "gposGlyphLookups" (fun () -> Engine.gposGlyphLookups plan font)
            measure "gposPairs" (fun () -> Engine.gposPairs plan font covered glyphLookups) |> ignore
            ignore gsubGl
        for label in order do
            let struct (us, kb) = best[label]
            Console.WriteLine($"| {name} | {label} | {us:N0} | {kb:N0} |")

/// <summary>Loops the entry over the lines of one row for a number of seconds, so that a sampling profiler can look at it.</summary>
/// <param name="name">A fragment of the row's name.</param>
/// <param name="seconds">How long to loop.</param>
let loop (name: string) (seconds: int) =
    match rows () |> List.tryFind (fun row -> row.Name.Contains name) with
    | None -> Console.WriteLine "no such row"
    | Some row ->
        let lines = Corpus.lines row.Text
        let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
        let mutable cache = PlanCache.empty
        let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
        let plan =
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Ok plan -> ValueSome plan
            | Error _ -> ValueNone
        Console.WriteLine $"pid {Environment.ProcessId}"
        let clock = Stopwatch.StartNew()
        while clock.Elapsed.TotalSeconds < float seconds do
            let struct (_, _, kept) = shapeFarfbuzz row font plan cache lines
            cache <- kept

/// <summary>The name of a GPOS subtable kind.</summary>
/// <param name="subtable">The subtable.</param>
/// <returns>The name.</returns>
let private subtableKind (subtable: PosSubtable) =
    match subtable with
    | SinglePos _ -> "single"
    | PairPosGlyphs _ -> "pair glyphs"
    | PairPosClasses _ -> "pair classes"
    | CursivePos _ -> "cursive"
    | MarkBasePos _ -> "mark-to-base"
    | MarkLigPos _ -> "mark-to-ligature"
    | MarkMarkPos _ -> "mark-to-mark"
    | PosContext _ -> "context"
    | PosChainContext _ -> "chain context"
    | PosPending _ -> "pending"

/// <summary>What reaches the positioning of the rows that keep marks: the lookups of the plan with their kinds and flags, how many
/// glyphs of the substituted lines each covers, the share of marks, and every position stage timed alone.</summary>
let anatomy () =
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    for row in rows () do
        if not (String.IsNullOrEmpty only) && row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let substitutedRuns =
                        lines
                        |> Array.map (fun (struct (start, length)) ->
                            let run = makeRun row.Direction row.Text start length
                            let normalized = OtShaper.normalised plan font (GlyphRun.enter run)
                            match OtSubstitute.afterNormalize plan font normalized with
                            | Ok substituted -> substituted
                            | Error _ -> normalized)
                    Console.WriteLine($"## {row.Name}: ApplyGpos {plan.ApplyGpos}, ApplyKern {plan.ApplyKern}, ZeroMarks {plan.ZeroMarks}, FallbackMarkPositioning {plan.FallbackMarkPositioning}, HasGposMark {plan.HasGposMark}, GPOS lookups in the map {plan.Map.GposLookups.Length}, GSUB lookups in the map {plan.Map.GsubLookups.Length}, Shaper {plan.Shaper.ZeroWidthMarks}")
                    let glyphs = substitutedRuns |> Array.sumBy (fun run -> run.Infos.Length)
                    let marks = substitutedRuns |> Array.sumBy (fun run -> run.Props |> Seq.filter (fun props -> props.GlyphClass &&& GlyphPropsBits.Mark <> 0us) |> Seq.length)
                    let unicodeMarks = substitutedRuns |> Array.sumBy (fun run -> run.Props |> Seq.filter UnicodeProps.isUnicodeMark |> Seq.length)
                    Console.WriteLine($"   glyphs after GSUB {glyphs}, GDEF-or-synthesised mark class {marks}, Unicode mark {unicodeMarks}")
                    if plan.ApplyGpos then
                        let table = font.Ot.Gpos.Value
                        for map in plan.Map.GposLookups do
                            let accel = table.Lookups[map.LookupIndex].Value
                            let kinds = accel.Subtables |> Seq.map (fun s -> subtableKind s.Subtable) |> Seq.distinct |> String.concat "+"
                            let covered =
                                substitutedRuns
                                |> Array.sumBy (fun run ->
                                    run.Infos |> Seq.filter (fun info -> accel.Subtables |> Seq.exists (fun s -> Digest.mayHaveCall info.Codepoint s.SubtableDigest)) |> Seq.length)
                            Console.WriteLine($"   GPOS lookup {map.LookupIndex}: {kinds}, subtables {accel.Subtables.Length}, flag {accel.Props:X}, mask {map.Mask:X}, feature {map.FeatureTag}, glyphs the digests admit {covered}")
                    let time (name: string) (work: GlyphRun -> GlyphRun) =
                        let mutable best = Int64.MaxValue
                        let mutable bytes = 0L
                        for pass in 0 .. 12 do
                            let b0 = GC.GetAllocatedBytesForCurrentThread()
                            let t0 = Stopwatch.GetTimestamp()
                            for run in substitutedRuns do
                                work run |> ignore
                            let t1 = Stopwatch.GetTimestamp()
                            if pass > 0 then
                                best <- min best (t1 - t0)
                                bytes <- GC.GetAllocatedBytesForCurrentThread() - b0
                        Console.WriteLine($"   {name}: {float best * 1e9 / float Stopwatch.Frequency / float glyphs:N1} ns/glyph, {bytes / int64 lines.Length:N0} B/line")
                    let defaulted = substitutedRuns |> Array.map (fun run -> let cleared = GlyphRun.clearPositions run in { cleared with Positions = Font.hAdvancePositions font (cleared.Infos.AsSpan()) })
                    let timeOn (name: string) (inputs: GlyphRun array) (work: GlyphRun -> GlyphRun) =
                        let mutable best = Int64.MaxValue
                        let mutable bytes = 0L
                        for pass in 0 .. 12 do
                            let b0 = GC.GetAllocatedBytesForCurrentThread()
                            let t0 = Stopwatch.GetTimestamp()
                            for run in inputs do
                                work run |> ignore
                            let t1 = Stopwatch.GetTimestamp()
                            if pass > 0 then
                                best <- min best (t1 - t0)
                                bytes <- GC.GetAllocatedBytesForCurrentThread() - b0
                        Console.WriteLine($"   {name}: {float best * 1e9 / float Stopwatch.Frequency / float glyphs:N1} ns/glyph, {bytes / int64 lines.Length:N0} B/line")
                    time "clear positions + default advances" (fun run -> let cleared = GlyphRun.clearPositions run in { cleared with Positions = Font.hAdvancePositions font (cleared.Infos.AsSpan()) })
                    let started = defaulted |> Array.map OtLayout.positionStart
                    timeOn "position start" defaulted OtLayout.positionStart
                    let zeroed = started |> Array.map (OtPosition.zeroMarkWidthsByGdef true)
                    timeOn "zero mark widths (by GDEF)" started (OtPosition.zeroMarkWidthsByGdef true)
                    let gpos = started |> Array.map (fun run -> OtLayout.position plan font run)
                    timeOn "GPOS engine (OtLayout.position)" started (fun run -> OtLayout.position plan font run)
                    timeOn "GPOS engine, planned (Engine.positionPlanned)" started (fun run -> Engine.positionPlanned plan font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups shapePlan.GposPairs run)
                    timeOn "finish offsets" gpos (OtLayout.finishOffsets font)
                    let finished = gpos |> Array.map (OtLayout.finishOffsets font)
                    timeOn "fallback mark positioning" finished (fun run -> match OtLayout.fallbackMarkPosition plan font true run with | Ok r -> r | Error _ -> run)
                    timeOn "zero width default ignorables" finished OtPosition.zeroWidthDefaultIgnorables
                    timeOn "whole OtPosition.position" substitutedRuns (fun run -> match OtPosition.position plan font run with | Ok r -> r | Error _ -> run)
                    ignore zeroed

/// <summary>What one visit of an attachment subtable costs on its own, without the lookup loop around it: every glyph a subtable's
/// digest admits is offered to <c>GposAttach.applySubtable</c> on the line's default positions.</summary>
let attachLeaf () =
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    for row in rows () do
        if not (String.IsNullOrEmpty only) && row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let runs =
                        lines
                        |> Array.map (fun (struct (start, length)) ->
                            let run = makeRun row.Direction row.Text start length
                            let normalized = OtShaper.normalised plan font (GlyphRun.enter run)
                            match OtSubstitute.afterNormalize plan font normalized with
                            | Ok substituted -> { substituted with Positions = Font.hAdvancePositions font (substituted.Infos.AsSpan()); HavePositions = true }
                            | Error _ -> normalized)
                    let table = font.Ot.Gpos.Value
                    let gdef = font.Ot.Gdef.Value
                    for map in plan.Map.GposLookups do
                        let accel = table.Lookups[map.LookupIndex].Value
                        if accel.Subtables |> Seq.exists (fun s -> GposAttach.isAttachment s.Subtable) then
                            let props = Gdef.sanitizeLookupProps gdef accel.Props
                            let matcher = Matching.matcherFor 1 false props map.Mask map.AutoZwnj map.AutoZwj false 0uy
                            let env = { Font = font; Direction = first.SegmentProperties.Direction; Horizontal = true; Matcher = matcher; Gdef = gdef }
                            let mutable best = Int64.MaxValue
                            let mutable visits = 0
                            let mutable applied = 0
                            let passes = match Environment.GetEnvironmentVariable "ATTACH_PASSES" with | null | "" -> 10 | text -> int text
                            Console.WriteLine($"pid {Environment.ProcessId}")
                            for pass in 0 .. passes do
                                visits <- 0
                                applied <- 0
                                let t0 = Stopwatch.GetTimestamp()
                                for run in runs do
                                    let infos = run.Infos.AsSpan()
                                    let props = run.Props.AsSpan()
                                    let positions = run.Positions.AsSpan()
                                    for index in 0 .. infos.Length - 1 do
                                        let sub = accel.Subtables[0]
                                        if Digest.mayHaveCall infos[index].Codepoint sub.SubtableDigest then
                                            visits <- visits + 1
                                            let outcome = GposAttach.applySubtable env sub.Subtable infos props positions index -1 0
                                            if outcome.Applied then applied <- applied + 1
                                let t1 = Stopwatch.GetTimestamp()
                                if pass > 0 then best <- min best (t1 - t0)
                            let compiled = MarkTables.ofLookup accel.Subtables
                            let mutable bestCompiled = Int64.MaxValue
                            let mutable appliedCompiled = 0
                            for pass in 0 .. passes do
                                appliedCompiled <- 0
                                let t0 = Stopwatch.GetTimestamp()
                                for run in runs do
                                    let infos = run.Infos.AsSpan()
                                    let props = run.Props.AsSpan()
                                    let positions = run.Positions.AsSpan()
                                    for index in 0 .. infos.Length - 1 do
                                        let sub = accel.Subtables[0]
                                        if Digest.mayHaveCall infos[index].Codepoint sub.SubtableDigest then
                                            let outcome = GposAttach.applyCompiled env sub.Subtable compiled[0] infos props positions index -1 0
                                            if outcome.Applied then appliedCompiled <- appliedCompiled + 1
                                let t1 = Stopwatch.GetTimestamp()
                                if pass > 0 then bestCompiled <- min bestCompiled (t1 - t0)
                            Console.WriteLine($"   {row.Name} lookup {map.LookupIndex} ({subtableKind accel.Subtables[0].Subtable}): {visits} visits, {applied} applied, {float best * 1e9 / float Stopwatch.Frequency / float (max visits 1):N1} ns per visit; compiled ({appliedCompiled} applied): {float bestCompiled * 1e9 / float Stopwatch.Frequency / float (max visits 1):N1} ns per visit")

/// <summary>What the primitives of an attachment visit cost alone, for the marks of the Hebrew row: the coverage lookup, the
/// skipping verdict of a glyph, and a backwards search for a base.</summary>
let attachPrimitives () =
    let row = rows () |> List.find (fun row -> row.Name = "Hebrew with points")
    let lines = Corpus.lines row.Text
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
    let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
    match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> ()
    | Ok shapePlan ->
        let plan = match shapePlan.Ot with | ValueSome plan -> plan | ValueNone -> failwith "no ot plan"
        let runs =
            lines
            |> Array.map (fun (struct (start, length)) ->
                let run = makeRun row.Direction row.Text start length
                let normalized = OtShaper.normalised plan font (GlyphRun.enter run)
                match OtSubstitute.afterNormalize plan font normalized with
                | Ok substituted -> substituted
                | Error _ -> normalized)
        let table = font.Ot.Gpos.Value
        let gdef = font.Ot.Gdef.Value
        let accel = table.Lookups[plan.Map.GposLookups[1].LookupIndex].Value
        let attachment = match accel.Subtables[0].Subtable with | MarkBasePos attachment -> attachment | _ -> failwith "no"
        let matcher = Matching.matcherFor 1 false LookupFlag.IgnoreMarks 0x80000000u false false false 0uy
        let time (name: string) (work: unit -> int) =
            let mutable best = Int64.MaxValue
            let mutable count = 0
            for pass in 0 .. 60 do
                let t0 = Stopwatch.GetTimestamp()
                count <- work ()
                let t1 = Stopwatch.GetTimestamp()
                if pass > 0 then best <- min best (t1 - t0)
            Console.WriteLine($"   {name}: {float best * 1e9 / float Stopwatch.Frequency / float count:N2} ns per call ({count} calls)")
        time "coverageIndex of every glyph" (fun () ->
            let mutable sum = 0
            let mutable count = 0
            for run in runs do
                for info in run.Infos do
                    sum <- sum + GposLookups.coverageIndex attachment.MarkCoverage attachment.MarkDirect info.Codepoint
                    count <- count + 1
            if sum = -1 then Console.Write ""
            count)
        time "Matching.verdict of every glyph (IgnoreMarks matcher)" (fun () ->
            let mutable sum = 0
            let mutable count = 0
            for run in runs do
                let infos = run.Infos.AsSpan()
                let props = run.Props.AsSpan()
                for index in 0 .. infos.Length - 1 do
                    match Matching.verdict matcher gdef (NoMatchFunc()) 0 infos[index] props[index] with
                    | Matched -> sum <- sum + 1
                    | NotMatched -> ()
                    | Skipped -> ()
                    count <- count + 1
            if sum = -1 then Console.Write ""
            count)
        time "Matching.prev of every glyph with index > 0" (fun () ->
            let mutable sum = 0
            let mutable count = 0
            for run in runs do
                let infos = run.Infos.AsSpan()
                let props = run.Props.AsSpan()
                for index in 1 .. infos.Length - 1 do
                    match Matching.prev matcher gdef (NoMatchFunc()) 0 infos props index with
                    | Hit i -> sum <- sum + i
                    | Miss i -> sum <- sum + i
                    count <- count + 1
            if sum = -1 then Console.Write ""
            count)

/// <summary>The stages of the general line on one working set, in order.</summary>
let private copyStageNames =
    [| "enter + unicode props"; "dotted circle"; "clusters (in place)"; "native direction"; "preprocess hook"; "rotate chars (in place)"
       "normalise (owned)"; "hand-off check + glyphs + classes (in place)"; "GSUB"; "default advances"; "GPOS + zero marks (owned)"
       "offsets, reversal, post hook, flags (in place)" |]

/// <summary>Where the time and the bytes of the general line on one working set go: every stage timed over every line of a row,
/// with the array bytes it allocates per line and what that is per glyph (one array of infos, properties or positions is 20, 12
/// or 20 bytes a glyph), beside the whole line through <c>Execute.runLean</c> and the stage by stage reference.</summary>
let copies () =
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    Console.WriteLine "| text | stage | ns/glyph | B/line | B/glyph |"
    Console.WriteLine "|---|---|---:|---:|---:|"
    for row in rows () do
        if String.IsNullOrEmpty only || row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let ticks = Array.zeroCreate<int64> copyStageNames.Length
                    let bytes = Array.zeroCreate<int64> copyStageNames.Length
                    let mutable glyphs = 0
                    let mutable wholeTicks = 0L
                    let mutable wholeBytes = 0L
                    let mutable referenceBytes = 0L
                    let measure (index: int) (work: unit -> 'a) =
                        let before = GC.GetAllocatedBytesForCurrentThread()
                        let started = Stopwatch.GetTimestamp()
                        let result = work ()
                        ticks[index] <- ticks[index] + (Stopwatch.GetTimestamp() - started)
                        bytes[index] <- bytes[index] + (GC.GetAllocatedBytesForCurrentThread() - before)
                        result
                    let best = Array.create copyStageNames.Length Int64.MaxValue
                    let mutable bestWhole = Int64.MaxValue
                    for pass in 0 .. 90 do
                        Array.fill ticks 0 ticks.Length 0L
                        Array.fill bytes 0 bytes.Length 0L
                        glyphs <- 0
                        wholeTicks <- 0L
                        wholeBytes <- 0L
                        referenceBytes <- 0L
                        for struct (start, length) in lines do
                            let run = makeRun row.Direction row.Text start length
                            let target = run.SegmentProperties.Direction
                            let propped = measure 0 (fun () -> OtProps.setUnicodePropsFused plan.Map.GlobalMask (GlyphRun.enter run))
                            let circled = measure 1 (fun () -> OtProps.insertDottedCircle font propped)
                            let clustered = measure 2 (fun () -> GeneralLine.formClusters circled)
                            let native = measure 3 (fun () -> OtProps.ensureNativeDirection clustered)
                            let prepared = measure 4 (fun () -> ShaperHooks.preprocess plan font native)
                            let rotated = measure 5 (fun () -> GeneralLine.rotateChars plan font prepared)
                            let normalized = measure 6 (fun () -> Normalize.runOwned plan font rotated)
                            let synthesized = measure 7 (fun () -> GeneralLine.glyphsOf plan font normalized)
                            let substituted = measure 8 (fun () -> GeneralLine.lookups shapePlan plan font synthesized)
                            let metrics = font.Ot.Hmtx.Value.Table
                            let advances =
                                measure 9 (fun () ->
                                    match Sealed.simpleAdvances (SimpleRun.SimpleGlyphs(font, plan.FallbackGlyphClasses)) (substituted.Infos.AsSpan()) (substituted.Props.AsSpan()) metrics.Span 0us 0us with
                                    | ValueSome advances -> advances
                                    | ValueNone -> ImmutableArray<GlyphPosition>.Empty)
                            let positioned = measure 10 (fun () -> GeneralLine.gposPass shapePlan plan font substituted advances)
                            match positioned with
                            | ValueNone -> ()
                            | ValueSome positioned ->
                                let final = measure 11 (fun () -> GeneralLine.finished plan font target positioned)
                                glyphs <- glyphs + final.Infos.Length
                            if pass >= 50 then
                                let run = makeRun row.Direction row.Text start length
                                let b0 = GC.GetAllocatedBytesForCurrentThread()
                                let t0 = Stopwatch.GetTimestamp()
                                Execute.runLean shapePlan font ImmutableArray<Feature>.Empty run |> ignore
                                wholeTicks <- wholeTicks + (Stopwatch.GetTimestamp() - t0)
                                wholeBytes <- wholeBytes + (GC.GetAllocatedBytesForCurrentThread() - b0)
                                let run = makeRun row.Direction row.Text start length
                                let r0 = GC.GetAllocatedBytesForCurrentThread()
                                Execute.runGeneral shapePlan font ImmutableArray<Feature>.Empty run |> ignore
                                referenceBytes <- referenceBytes + (GC.GetAllocatedBytesForCurrentThread() - r0)
                        if pass >= 50 then
                            for index in 0 .. copyStageNames.Length - 1 do
                                best[index] <- min best[index] ticks[index]
                            bestWhole <- min bestWhole wholeTicks
                    Array.blit best 0 ticks 0 ticks.Length
                    wholeTicks <- bestWhole
                    let perLine (value: int64) = value / int64 lines.Length
                    let perGlyph (value: int64) = float value / float glyphs
                    for index in 0 .. copyStageNames.Length - 1 do
                        let ns = float ticks[index] * 1e9 / float Stopwatch.Frequency
                        Console.WriteLine($"| {row.Name} | {copyStageNames[index]} | {ns / float glyphs:N1} | {perLine bytes[index]:N0} | {perGlyph bytes[index]:N1} |")
                    Console.WriteLine($"| {row.Name} | total of the stages | {float (Array.sum ticks) * 1e9 / float Stopwatch.Frequency / float glyphs:N1} | {perLine (Array.sum bytes):N0} | {perGlyph (Array.sum bytes):N1} |")
                    Console.WriteLine($"| {row.Name} | whole line, Execute.runLean | {float wholeTicks * 1e9 / float Stopwatch.Frequency / float glyphs:N1} | {perLine wholeBytes:N0} | {perGlyph wholeBytes:N1} |")
                    Console.WriteLine($"| {row.Name} | whole line, Execute.runGeneral (reference) | n/a | {perLine referenceBytes:N0} | {perGlyph referenceBytes:N1} |")

/// <summary>The kind of a GSUB subtable, for the anatomy of a plan.</summary>
let private substKind (subtable: SubstSubtable) =
    match subtable with
    | SingleDelta _ -> "single"
    | SingleList _ -> "single"
    | Multiple _ -> "multiple"
    | Alternate _ -> "alternate"
    | LigatureSubst _ -> "ligature"
    | SubstContext _ -> "context"
    | SubstChainContext rules ->
        match rules with
        | ChainGlyphRules (_, sets) -> $"chain format 1 ({sets.Length} sets, {sets |> Seq.sumBy (fun set -> set.Length)} rules)"
        | ChainClassRules (_, _, _, _, sets, _) -> $"chain format 2 ({sets.Length} sets, {sets |> Seq.sumBy (fun set -> set.Length)} rules)"
        | ChainCoverageRule (backtrack, input, lookahead, records) -> $"chain format 3 (backtrack {backtrack.Length}, input {input.Length}, lookahead {lookahead.Length}, {records.Length} records)"
    | ReverseChainSingle _ -> "reverse"
    | SubstPending _ -> "pending"

/// <summary>The lookups of the GSUB map of a row's plan: its feature, the kinds of its subtables, how many glyphs of the row's lines it
/// covers before the substitution, how many lookups of a line apply (the glyphs a lookup changed) and the time of the lookup alone on
/// the lines as they are before the substitution.</summary>
let gsubAnatomy () =
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    Console.WriteLine "| text | lookup | feature | flags | subtables | glyphs covered | glyphs per line | ns per covered glyph (alone) | changed lines of 1000 |"
    Console.WriteLine "|---|---:|---|---:|---|---:|---:|---:|---:|"
    for row in rows () do
        if String.IsNullOrEmpty only || row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let runs =
                        lines
                        |> Array.map (fun (struct (start, length)) ->
                            let run = makeRun row.Direction row.Text start length
                            GeneralLine.glyphsOf plan font (Normalize.runOwned plan font (GeneralLine.beforeNormaliser plan font run)))
                    let glyphs = runs |> Array.sumBy (fun run -> run.Infos.Length)
                    let table = font.Ot.Gsub.Value
                    for position in 0 .. plan.Map.GsubLookups.Length - 1 do
                        let lookupMap = plan.Map.GsubLookups[position]
                        let accel = table.Lookups[lookupMap.LookupIndex].Value
                        let covered = shapePlan.GsubLookupCovered[lookupMap.LookupIndex]
                        let words = covered.AsSpan().ToArray()
                        let isCovered (glyph: uint32) = int (glyph >>> 6) < words.Length && (words[int (glyph >>> 6)] >>> int (glyph &&& 63u)) &&& 1UL <> 0UL
                        let count = runs |> Array.sumBy (fun run -> run.Infos.AsSpan().ToArray() |> Array.filter (fun info -> isCovered info.Codepoint) |> Array.length)
                        let kinds = accel.Subtables.AsSpan().ToArray() |> Array.map (fun subtable -> substKind subtable.Subtable) |> String.concat "; "
                        let initialHits =
                            accel.Subtables.AsSpan().ToArray()
                            |> Array.map (fun subtable ->
                                let initial =
                                    match subtable.Subtable with
                                    | SubstChainContext (ChainCoverageRule (_, input, _, _)) -> ValueSome input[0]
                                    | SubstContext (CoverageRule (input, _)) -> ValueSome input[0]
                                    | _ -> ValueNone
                                match initial with
                                | ValueNone -> -1
                                | ValueSome coverage ->
                                    runs |> Array.sumBy (fun run -> run.Infos.AsSpan().ToArray() |> Array.filter (fun info -> match Coverage.index coverage info.Codepoint with | CoverageHit.Covered _ -> true | CoverageHit.NotCovered -> false) |> Array.length))
                        let hitText = String.Join (" ", initialHits)
                        Console.WriteLine($"   lookup {position} initial-glyph hits per subtable: {hitText} (covered glyphs {count})")
                        let lookups = ImmutableArray.Create lookupMap
                        let stages = ImmutableArray.Create { LastLookup = 1; Pause = NoPause }
                        let mutable changed = 0
                        let mutable best = Int64.MaxValue
                        for pass in 0 .. 12 do
                            let t0 = Stopwatch.GetTimestamp()
                            let mutable changes = 0
                            for run in runs do
                                let applied = Engine.applyGsub true table lookups stages font run
                                if not (obj.ReferenceEquals (applied, run)) then changes <- changes + 1
                            let t1 = Stopwatch.GetTimestamp()
                            changed <- changes
                            if pass > 0 then best <- min best (t1 - t0)
                        let nsPerCovered = match count with | 0 -> 0.0 | _ -> float best * 1e9 / float Stopwatch.Frequency / float count
                        Console.WriteLine($"| {row.Name} | {position} (#{lookupMap.LookupIndex}) | {Tag.toString lookupMap.FeatureTag} | {accel.Props:X} | {kinds} | {count} | {float count / float lines.Length:N1} | {nsPerCovered:N1} | {changed * 1000 / lines.Length} |")
                    Console.WriteLine($"| {row.Name} | (glyphs of the row: {glyphs}) | | | | | | | |")

/// <summary>What one visit of a context subtable costs on the glyphs of a row that its first coverage holds: each subtable of the first
/// context lookup with a chained or coverage rule, applied by <c>ContextApply</c> at every glyph its first coverage holds, how many
/// matched, and the time per visit.</summary>
let contextVisits () =
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    Console.WriteLine "| text | lookup | subtable | visits | matched | ns per visit | ns per visit that fails |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|"
    for row in rows () do
        if String.IsNullOrEmpty only || row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let runs =
                        lines
                        |> Array.map (fun (struct (start, length)) ->
                            let run = makeRun row.Direction row.Text start length
                            GeneralLine.glyphsOf plan font (Normalize.runOwned plan font (GeneralLine.beforeNormaliser plan font run)))
                    let table = font.Ot.Gsub.Value
                    let gdef = font.Ot.Gdef.Value
                    for position in 0 .. plan.Map.GsubLookups.Length - 1 do
                        let lookupMap = plan.Map.GsubLookups[position]
                        let accel = table.Lookups[lookupMap.LookupIndex].Value
                        let matcher = Matching.matcherFor 0 false accel.Props lookupMap.Mask lookupMap.AutoZwnj lookupMap.AutoZwj false 0uy
                        let contextMatcher = Matching.matcherFor 0 true accel.Props lookupMap.Mask lookupMap.AutoZwnj lookupMap.AutoZwj false 0uy
                        let onlySubtable = Environment.GetEnvironmentVariable "CONTEXT_ONLY" |> Option.ofObj |> Option.map int
                        let passes = Environment.GetEnvironmentVariable "CONTEXT_PASSES" |> Option.ofObj |> Option.map int |> Option.defaultValue 20
                        for subtable in 0 .. accel.Subtables.Length - 1 do
                            match accel.Subtables[subtable].Subtable with
                            | SubstChainContext rules when onlySubtable.IsNone || onlySubtable = Some subtable ->
                                let mutable visits = 0
                                let mutable matched = 0
                                let mutable best = Int64.MaxValue
                                for pass in 0 .. passes do
                                    let mutable count = 0
                                    let mutable hits = 0
                                    let positions = Array.zeroCreate<int> 64
                                    let t0 = Stopwatch.GetTimestamp()
                                    for run in runs do
                                        let infos = run.Infos.AsSpan()
                                        let props = run.Props.AsSpan()
                                        for index in 0 .. infos.Length - 1 do
                                            if Digest.mayHaveCall infos[index].Codepoint accel.Subtables[subtable].SubtableDigest then
                                                let outcome = ContextApply.matchChain rules true gdef matcher contextMatcher infos props infos props infos.Length index (Span<int> positions)
                                                count <- count + 1
                                                if outcome.Matched then hits <- hits + 1
                                    let t1 = Stopwatch.GetTimestamp()
                                    visits <- count
                                    matched <- hits
                                    if pass > 0 then best <- min best (t1 - t0)
                                let ns = match visits with | 0 -> 0.0 | _ -> float best * 1e9 / float Stopwatch.Frequency / float visits
                                Console.WriteLine($"| {row.Name} | {position} | {subtable} | {visits} | {matched} | {ns:N1} | |")
                                match ChainSets.ofSubtable accel.Subtables[subtable] with
                                | ValueNone -> ()
                                | ValueSome sets ->
                                    let mutable refusedBest = Int64.MaxValue
                                    let mutable refusedCount = 0
                                    let mutable refused = 0
                                    for pass in 0 .. passes do
                                        let mutable count = 0
                                        let mutable no = 0
                                        let t0 = Stopwatch.GetTimestamp()
                                        for run in runs do
                                            let infos = run.Infos.AsSpan()
                                            let props = run.Props.AsSpan()
                                            for index in 0 .. infos.Length - 1 do
                                                if Digest.mayHaveCall infos[index].Codepoint accel.Subtables[subtable].SubtableDigest then
                                                    count <- count + 1
                                                    if ContextApply.chainDenseCannotMatch sets matcher contextMatcher gdef infos props infos props index then no <- no + 1
                                        let t1 = Stopwatch.GetTimestamp()
                                        refusedCount <- count
                                        refused <- no
                                        if pass > 0 then refusedBest <- min refusedBest (t1 - t0)
                                    let mutable loopBest = Int64.MaxValue
                                    for pass in 0 .. passes do
                                        let mutable count = 0
                                        let t0 = Stopwatch.GetTimestamp()
                                        for run in runs do
                                            let infos = run.Infos.AsSpan()
                                            for index in 0 .. infos.Length - 1 do
                                                if Digest.mayHaveCall infos[index].Codepoint accel.Subtables[subtable].SubtableDigest then
                                                    count <- count + 1
                                        let t1 = Stopwatch.GetTimestamp()
                                        if count = -1 then Console.Write ""
                                        if pass > 0 then loopBest <- min loopBest (t1 - t0)
                                    let nsRefused = match refusedCount with | 0 -> 0.0 | _ -> float (refusedBest - loopBest) * 1e9 / float Stopwatch.Frequency / float refusedCount
                                    Console.WriteLine($"|   | | {subtable} | {refusedCount} | refused at once: {refused} | {nsRefused:N1} (decision alone, per visit, the loop over the glyphs taken off) | |")
                                match rules with
                                | ChainCoverageRule (backtrack, input, _, _) ->
                                    let mutable coverageBest = Int64.MaxValue
                                    let mutable calls = 0
                                    let mutable sink = 0
                                    for pass in 0 .. passes do
                                        let mutable count = 0
                                        let t0 = Stopwatch.GetTimestamp()
                                        for run in runs do
                                            let infos = run.Infos.AsSpan()
                                            for index in 0 .. infos.Length - 1 do
                                                if Digest.mayHaveCall infos[index].Codepoint accel.Subtables[subtable].SubtableDigest then
                                                    count <- count + 1
                                                    (match Coverage.index input[0] infos[index].Codepoint with
                                                     | CoverageHit.Covered _ -> sink <- sink + 1
                                                     | CoverageHit.NotCovered -> ())
                                                    (match backtrack.Length with
                                                     | 0 -> ()
                                                     | _ ->
                                                        match Coverage.index backtrack[0] infos[index].Codepoint with
                                                        | CoverageHit.Covered _ -> sink <- sink + 1
                                                        | CoverageHit.NotCovered -> ())
                                        let t1 = Stopwatch.GetTimestamp()
                                        calls <- count
                                        if pass > 0 then coverageBest <- min coverageBest (t1 - t0)
                                    let perCall = float coverageBest * 1e9 / float Stopwatch.Frequency / float calls
                                    Console.WriteLine($"|   | | {subtable} | {calls} | two coverage lookups (input first, backtrack first) | {perCall:N1} | {sink % 2} |")
                                | _ -> ()
                            | _ -> ()

/// <summary>The cost of the whole GPOS pass on the lines of the marked rows after the GSUB engine, in a warm loop: nanoseconds
/// per glyph and per mark glyph, and the bytes allocated per pass. Run on two builds, the difference over the marks is what a visit costs.</summary>
let visitAnatomy () =
    let wanted =
        match Environment.GetEnvironmentVariable "VISIT_ROW" with
        | null | "" -> [ "Hebrew with points"; "Thai"; "French combining marks"; "Vietnamese combining marks" ]
        | name -> [ name ]
    let perLookup = String.IsNullOrEmpty (Environment.GetEnvironmentVariable "VISIT_ROW")
    let passes = match Environment.GetEnvironmentVariable "VISIT_PASSES" with | null | "" -> 300 | text -> int text
    Console.WriteLine "| text | glyphs | mark glyphs | GPOS ns per glyph | GPOS ns per mark glyph | GPOS B per pass |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|"
    for row in rows () do
        if List.contains row.Name wanted then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let runs =
                        lines
                        |> Array.map (fun (struct (start, length)) ->
                            let run = makeRun row.Direction row.Text start length
                            let normalized = OtShaper.normalised plan font (GlyphRun.enter run)
                            let substituted =
                                match OtSubstitute.afterNormalize plan font normalized with
                                | Ok substituted -> substituted
                                | Error _ -> normalized
                            { substituted with Positions = Font.hAdvancePositions font (substituted.Infos.AsSpan()); HavePositions = true })
                    let table = font.Ot.Gpos.Value
                    for map in plan.Map.GposLookups do
                        let accel = table.Lookups[map.LookupIndex].Value
                        let covered = runs |> Array.sumBy (fun run -> run.Infos.AsSpan().ToArray() |> Array.filter (fun info -> Digest.mayHaveCall info.Codepoint accel.LookupDigest) |> Array.length)
                        let kinds = accel.Subtables |> Seq.map (fun s -> subtableKind s.Subtable) |> Seq.distinct |> String.concat "+"
                        Console.WriteLine($"   lookup {map.LookupIndex}: props {accel.Props:X}, {accel.Subtables.Length} subtables ({kinds}), {covered} glyphs through its digest")
                    let glyphs = runs |> Array.sumBy (fun run -> run.Infos.Length)
                    let marks = runs |> Array.sumBy (fun run -> run.Props.AsSpan().ToArray() |> Array.filter (fun props -> GlyphPropsBits.isMark props) |> Array.length)
                    let pass () =
                        let mutable total = 0
                        for run in runs do
                            total <- total + (Engine.positionPlanned plan font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups shapePlan.GposPairs run).Infos.Length
                        total
                    pass () |> ignore
                    let mutable best = Int64.MaxValue
                    let mutable bytes = 0L
                    for _ in 1 .. passes do
                        let b0 = GC.GetAllocatedBytesForCurrentThread()
                        let t0 = Stopwatch.GetTimestamp()
                        pass () |> ignore
                        let t1 = Stopwatch.GetTimestamp()
                        bytes <- GC.GetAllocatedBytesForCurrentThread() - b0
                        best <- min best (t1 - t0)
                    let only (k: int) =
                        let lookups = plan.Map.GposLookups |> Seq.mapi (fun i (m: LookupMap) -> match i = k with | true -> m | false -> { m with Mask = 0u }) |> Seq.toArray |> ImmutableArray.CreateRange
                        { plan with Map = { plan.Map with GposLookups = lookups } }
                    let alone (k: int) =
                        let restricted = only k
                        let run () =
                            let mutable total = 0
                            for r in runs do
                                total <- total + (Engine.positionPlanned restricted font shapePlan.GposLookupCovered shapePlan.GposGlyphLookups shapePlan.GposPairs r).Infos.Length
                            total
                        run () |> ignore
                        let mutable best = Int64.MaxValue
                        for _ in 1 .. passes do
                            let t0 = Stopwatch.GetTimestamp()
                            run () |> ignore
                            best <- min best (Stopwatch.GetTimestamp() - t0)
                        float best * 1e9 / float Stopwatch.Frequency
                    let idle = match perLookup with | true -> alone -1 | false -> 0.0
                    for k in 0 .. (match perLookup with | true -> plan.Map.GposLookups.Length - 1 | false -> -1) do
                        let ms = alone k - idle
                        if ms > 1e5 then Console.WriteLine($"   lookup at position {k} alone: {ms / float glyphs:N1} ns per glyph")
                    let ns = float best * 1e9 / float Stopwatch.Frequency
                    Console.WriteLine($"| {row.Name} | {glyphs} | {marks} | {ns / float glyphs:N1} | {ns / float marks:N1} | {bytes:N0} |")

/// <summary>Where the text enters: the UTF-8 decoding of every line of each row into a run, against HarfBuzz's
/// <c>hb_buffer_add_utf8</c> into a reused buffer, alternated, the minimum of each over many passes, with the bytes Farfbuzz
/// allocates.</summary>
let utf8Input () =
    NativeShaping.oracle.Force() |> ignore
    let rounds = match Environment.GetEnvironmentVariable "GENERAL_SHAPE_ROUNDS" with | null | "" -> 200 | text -> int text
    Console.WriteLine "| text | lines | code points | HarfBuzz ns/code point | Farfbuzz ns/code point | Farfbuzz B/line | Farfbuzz / HarfBuzz |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|"
    for row in rows () do
        match row.Route with
        | Entry ->
            let lines = Corpus.lines row.Text
            use buffer = new HarfBuzzSharp.Buffer()
            let decode () =
                let mutable total = 0
                for struct (start, length) in lines do
                    total <- total + (GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty).Infos.Length
                total
            let native () =
                let mutable total = 0
                for struct (start, length) in lines do
                    buffer.ClearContents()
                    buffer.AddUtf8(ReadOnlySpan(row.Text, start, row.Text.Length - start), 0, length)
                    total <- total + buffer.Length
                total
            let codepoints = decode ()
            let mutable hbBest = Int64.MaxValue
            let mutable ffBest = Int64.MaxValue
            let mutable ffBytes = 0L
            for round in 0 .. rounds do
                let t0 = Stopwatch.GetTimestamp()
                native () |> ignore
                let t1 = Stopwatch.GetTimestamp()
                let b0 = GC.GetAllocatedBytesForCurrentThread()
                decode () |> ignore
                let t2 = Stopwatch.GetTimestamp()
                if round > 0 then
                    hbBest <- min hbBest (t1 - t0)
                    ffBest <- min ffBest (t2 - t1)
                    ffBytes <- GC.GetAllocatedBytesForCurrentThread() - b0
            let ns (ticks: int64) = float ticks * 1e9 / float Stopwatch.Frequency / float codepoints
            Console.WriteLine($"| {row.Name} | {lines.Length} | {codepoints} | {ns hbBest:N2} | {ns ffBest:N2} | {ffBytes / int64 lines.Length:N0} | {float ffBest / float hbBest:N2} |")
        | General | Reference -> ()

/// <summary>A copy of a run with arrays of its own, so that an in-place stage can be timed again on the same input.</summary>
/// <param name="run">The run.</param>
/// <returns>The run with copies of its infos, properties and positions.</returns>
let private ownedCopy (run: GlyphRun) =
    { run with
        Infos = ImmutableArray.Create<GlyphInfo>(run.Infos.AsSpan())
        Props = ImmutableArray.Create<GlyphProps>(run.Props.AsSpan())
        Positions = ImmutableArray.Create<GlyphPosition>(run.Positions.AsSpan()) }

/// <summary>The normaliser alone on the lines of the marked rows, in a warm loop: ns per glyph of its input and per mark, the minimum of
/// many passes, the arrays copied outside the timer. With <c>NORM_LOOP</c> set to a number of seconds it loops for a sampling profile.</summary>
let normAnatomy () =
    let only = Environment.GetEnvironmentVariable "GENERAL_SHAPE_ONLY" |> Option.ofObj |> Option.defaultValue String.Empty
    let passes = match Environment.GetEnvironmentVariable "NORM_PASSES" with | null | "" -> 300 | text -> int text
    let loopSeconds = match Environment.GetEnvironmentVariable "NORM_LOOP" with | null | "" -> 0 | text -> int text
    Console.WriteLine "| text | glyphs in | glyphs out | marks in | ns per glyph in | ns per mark in | B per line |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|"
    for row in rows () do
        if (String.IsNullOrEmpty only || row.Name.Contains only) && row.Route = Entry then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = makeRun row.Direction row.Text 0 (let struct (_, length) = lines[0] in length)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let prepared =
                        lines |> Array.map (fun (struct (start, length)) -> GeneralLine.beforeNormaliser plan font (makeRun row.Direction row.Text start length))
                    let glyphsIn = prepared |> Array.sumBy (fun run -> run.Infos.Length)
                    let marksIn = prepared |> Array.sumBy (fun run -> run.Props.AsSpan().ToArray() |> Array.filter UnicodeProps.isUnicodeMark |> Array.length)
                    let mutable glyphsOut = 0
                    let mutable best = Int64.MaxValue
                    let mutable bytes = 0L
                    let started = Stopwatch.GetTimestamp()
                    let mutable pass = 0
                    while (loopSeconds = 0 && pass <= passes) || (loopSeconds > 0 && Stopwatch.GetElapsedTime(started).TotalSeconds < float loopSeconds) do
                        let copies = prepared |> Array.map ownedCopy
                        let b0 = GC.GetAllocatedBytesForCurrentThread()
                        let t0 = Stopwatch.GetTimestamp()
                        let mutable out = 0
                        for run in copies do
                            out <- out + (Normalize.runOwned plan font run).Infos.Length
                        let t1 = Stopwatch.GetTimestamp()
                        glyphsOut <- out
                        if pass > 0 then
                            best <- min best (t1 - t0)
                            bytes <- GC.GetAllocatedBytesForCurrentThread() - b0
                        pass <- pass + 1
                    let ns = float best * 1e9 / float Stopwatch.Frequency
                    Console.WriteLine($"| {row.Name} | {glyphsIn} | {glyphsOut} | {marksIn} | {ns / float glyphsIn:N1} | {ns / float (max 1 marksIn):N1} | {bytes / int64 lines.Length:N0} |")

/// <summary>The primitives the normaliser calls for a mark, each alone in a warm loop over the base and mark code points of the
/// French and Vietnamese text: ns per call.</summary>
let normPrimitives () =
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes (Path.Combine(RepositoryPaths.upstream (), "perf", "fonts", "Roboto-Regular.ttf")))) 0u)
    let starters = [| 0x65u; 0x61u; 0x6Fu; 0x63u; 0x75u; 0x69u; 0x45u; 0x41u |]
    let marks = [| 0x301u; 0x300u; 0x302u; 0x327u; 0x323u; 0x303u; 0x309u |]
    let pairs = [| for a in starters do for b in marks -> struct (a, b) |]
    let rounds = 2_000_000
    let time (name: string) (work: unit -> int) =
        let mutable best = Int64.MaxValue
        let mutable calls = 0
        for pass in 0 .. 20 do
            let t0 = Stopwatch.GetTimestamp()
            calls <- work ()
            let t1 = Stopwatch.GetTimestamp()
            if pass > 0 then best <- min best (t1 - t0)
        Console.WriteLine($"| {name} | {float best * 1e9 / float Stopwatch.Frequency / float calls:N2} |")
    Console.WriteLine "| primitive | ns per call |"
    Console.WriteLine "|---|---:|"
    time "Unicode.compose (starter, mark)" (fun () ->
        let mutable sum = 0u
        for _ in 0 .. rounds / pairs.Length do
            for struct (a, b) in pairs do
                match Unicode.compose a b with
                | ValueSome c -> sum <- sum + c
                | ValueNone -> ()
        if sum = 1u then Console.Write ""
        (rounds / pairs.Length + 1) * pairs.Length)
    time "Unicode.decompose (mark)" (fun () ->
        let mutable sum = 0u
        for _ in 0 .. rounds / marks.Length do
            for m in marks do
                match Unicode.decompose m with
                | ValueSome (struct (a, _)) -> sum <- sum + a
                | ValueNone -> ()
        if sum = 1u then Console.Write ""
        (rounds / marks.Length + 1) * marks.Length)
    time "Unicode.decompose (starter)" (fun () ->
        let mutable sum = 0u
        for _ in 0 .. rounds / starters.Length do
            for m in starters do
                match Unicode.decompose m with
                | ValueSome (struct (a, _)) -> sum <- sum + a
                | ValueNone -> ()
        if sum = 1u then Console.Write ""
        (rounds / starters.Length + 1) * starters.Length)
    time "Font.nominalGlyph (starter)" (fun () ->
        let mutable sum = 0u
        for _ in 0 .. rounds / starters.Length do
            for m in starters do
                match Font.nominalGlyph font m with
                | ValueSome g -> sum <- sum + g
                | ValueNone -> ()
        if sum = 1u then Console.Write ""
        (rounds / starters.Length + 1) * starters.Length)
    time "Font.nominalGlyph (precomposed e acute, a grave)" (fun () ->
        let mutable sum = 0u
        let cps = [| 0xE9u; 0xE0u; 0xF4u; 0xE7u; 0xF9u |]
        for _ in 0 .. rounds / cps.Length do
            for m in cps do
                match Font.nominalGlyph font m with
                | ValueSome g -> sum <- sum + g
                | ValueNone -> ()
        if sum = 1u then Console.Write ""
        (rounds / cps.Length + 1) * cps.Length)
    time "UnicodeProps.compute (precomposed)" (fun () ->
        let mutable sum = 0
        let cps = [| 0xE9u; 0xE0u; 0xF4u; 0xE7u; 0xF9u |]
        for _ in 0 .. rounds / cps.Length do
            for m in cps do
                let struct (p, _) = UnicodeProps.compute m
                sum <- sum + int p
        if sum = 1 then Console.Write ""
        (rounds / cps.Length + 1) * cps.Length)
