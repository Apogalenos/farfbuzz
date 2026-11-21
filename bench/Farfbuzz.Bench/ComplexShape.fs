module Farfbuzz.Bench.ComplexShape

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Text
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.NativeShaping

/// <summary>One benchmark row: a name, the font file, the UTF-8 text and a sentence saying what the text is.</summary>
type private Row =
    { Name: string
      FontPath: string
      Text: byte array
      Description: string }

/// <summary>A deterministic pseudo-random generator step.</summary>
/// <param name="state">The state.</param>
/// <returns>The next state.</returns>
let private next (state: uint64) = state * 6364136223846793005UL + 1442695040888963407UL

/// <summary>A value below a bound from a state.</summary>
/// <param name="state">The state.</param>
/// <param name="bound">The exclusive bound.</param>
/// <returns>The value.</returns>
let private below (state: uint64) (bound: int) = int ((state >>> 33) % uint64 bound)

/// <summary>The code points of inclusive ranges that the font maps, in order.</summary>
/// <param name="font">The font.</param>
/// <param name="ranges">The inclusive ranges.</param>
/// <returns>The mapped code points.</returns>
let private mapped (font: Font) (ranges: (int * int) list) =
    ranges
    |> List.collect (fun (first, last) -> [ first .. last ])
    |> List.filter (fun codepoint -> ValueOption.isSome (Font.nominalGlyph font (uint32 codepoint)))
    |> List.toArray

/// <summary>The description of a generated script: the syllable bases, the marks that follow and the code points
/// that join the next base to the previous one (virama, coeng).</summary>
type private Script =
    { Bases: (int * int) list
      Marks: (int * int) list
      Joiner: int list
      Leading: (int * int) list }

/// <summary>Appends one random code point of a non-empty array.</summary>
/// <param name="builder">The text.</param>
/// <param name="state">The state.</param>
/// <param name="values">The code points.</param>
/// <returns>The next state.</returns>
let private append (builder: StringBuilder) (state: uint64) (values: int array) =
    let state = next state
    builder.Append (Char.ConvertFromUtf32 values[below state values.Length]) |> ignore
    state

/// <summary>Builds lines of words of random syllables: an optional leading vowel, a base, an optional joiner and a
/// second base, and up to two marks.</summary>
/// <param name="font">The font, whose character map filters the code points.</param>
/// <param name="script">The script description.</param>
/// <param name="lineCount">The number of lines.</param>
/// <param name="wordsPerLine">The words in a line.</param>
/// <returns>The UTF-8 text, every line ending in a newline.</returns>
let private generate (font: Font) (script: Script) (lineCount: int) (wordsPerLine: int) =
    let bases = mapped font script.Bases
    let marks = mapped font script.Marks
    let leading = mapped font script.Leading
    let joiners = script.Joiner |> List.filter (fun codepoint -> ValueOption.isSome (Font.nominalGlyph font (uint32 codepoint))) |> List.toArray
    let builder = StringBuilder()
    let mutable state = 777UL
    for _ in 1 .. lineCount do
        for word in 1 .. wordsPerLine do
            if word > 1 then builder.Append ' ' |> ignore
            for _ in 1 .. 1 + below (next state) 3 do
                state <- next state
                if leading.Length > 0 && below state 5 = 0 then state <- append builder state leading
                state <- append builder state bases
                state <- next state
                if joiners.Length > 0 && below state 4 = 0 then
                    state <- append builder state joiners
                    state <- append builder state bases
                state <- next state
                if marks.Length > 0 then
                    for _ in 1 .. below state 3 do
                        state <- append builder state marks
        builder.Append '\n' |> ignore
    Encoding.UTF8.GetBytes (builder.ToString())

/// <summary>Builds lines of precomposed Hangul syllables and runs of conjoining jamo.</summary>
/// <param name="font">The font.</param>
/// <param name="lineCount">The number of lines.</param>
/// <param name="wordsPerLine">The words in a line.</param>
/// <returns>The UTF-8 text.</returns>
let private hangul (font: Font) (lineCount: int) (wordsPerLine: int) =
    let syllables = mapped font [ (0xAC00, 0xD7A3) ]
    let builder = StringBuilder()
    let mutable state = 4242UL
    for _ in 1 .. lineCount do
        for word in 1 .. wordsPerLine do
            if word > 1 then builder.Append ' ' |> ignore
            state <- next state
            match below state 2 with
            | 0 ->
                for _ in 1 .. 1 + below (next state) 3 do
                    state <- append builder state syllables
            | _ ->
                for _ in 1 .. 1 + below (next state) 2 do
                    state <- next state
                    builder.Append (char (0x1100 + below state 19)) |> ignore
                    state <- next state
                    builder.Append (char (0x1161 + below state 21)) |> ignore
                    state <- next state
                    if below state 2 = 0 then builder.Append (char (0x11A8 + below state 27)) |> ignore
        builder.Append '\n' |> ignore
    Encoding.UTF8.GetBytes (builder.ToString())

/// <summary>The rows: upstream's ten pairs, then the other ported scripts on the repository's real fonts.</summary>
let private rows () =
    let upstream = RepositoryPaths.upstream ()
    let subset file = Path.Combine(upstream, "test", "subset", "data", "fonts", file)
    let shape file = Path.Combine(upstream, "test", "shape", "data", "text-rendering-tests", "fonts", file)
    let font (path: string) = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
    let upstreamRows =
        Corpus.pairs
        |> List.map (fun pair ->
            { Name = pair.Name
              FontPath = Path.Combine(upstream, pair.Font)
              Text = File.ReadAllBytes(Path.Combine(upstream, pair.Text))
              Description = "upstream perf/texts file" })
    let synthetic (name: string) (path: string) (script: Script) (description: string) =
        { Name = name; FontPath = path; Text = generate (font path) script 1500 8; Description = description }
    let khmerOpenType =
        let bytes = File.ReadAllBytes (subset "Khmer.ttf")
        let tables = int (Buffers.Binary.BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
        for index in 0 .. tables - 1 do
            let record = 12 + index * 16
            if Encoding.ASCII.GetString(bytes, record, 4) = "morx" then bytes[record + 1] <- byte 't'
        let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "bench")
        Directory.CreateDirectory directory |> ignore
        let path = Path.Combine(directory, "Khmer-OpenType.ttf")
        File.WriteAllBytes(path, bytes)
        path
    let thaiPath = Path.Combine(upstream, "test", "fuzzing", "fonts", "kanit.ttf")
    let blankPath = subset "AdobeBlank-Regular.ttf"
    let robotoPath = Path.Combine(upstream, "perf", "fonts", "Roboto-Regular.ttf")
    let prince = Encoding.UTF8.GetString (File.ReadAllBytes (Path.Combine(upstream, "perf", "texts", "en-thelittleprince.txt")))
    let combining =
        let counts = Collections.Generic.Dictionary<char, int>()
        let builder = StringBuilder()
        for letter in prince do
            let replacement = match letter with | 'e' -> ValueSome (3, "e\u0301") | 'a' -> ValueSome (5, "a\u0300") | 'o' -> ValueSome (7, "o\u0302") | 'c' -> ValueSome (9, "c\u0327") | 'u' -> ValueSome (6, "u\u0300") | _ -> ValueNone
            match replacement with
            | ValueSome (period, text) ->
                let seen = match counts.TryGetValue letter with | true, n -> n | false, _ -> 0
                counts[letter] <- seen + 1
                if seen % period = 0 then builder.Append text |> ignore else builder.Append letter |> ignore
            | ValueNone -> builder.Append letter |> ignore
        Encoding.UTF8.GetBytes (builder.ToString())
    upstreamRows
    @ [ synthetic "Thai/kanit" thaiPath { Bases = [ (0x0E01, 0x0E2E) ]; Marks = [ (0x0E31, 0x0E31); (0x0E34, 0x0E3A); (0x0E47, 0x0E4E) ]; Joiner = []; Leading = [ (0x0E40, 0x0E44) ] } "generated: 1,500 lines of 8 words of 1 to 3 syllables (leading vowel, consonant, up to 2 above or below marks)"
        synthetic "Hebrew/NotoIKEAHebrew" (subset "NotoIKEAHebrewLatin-Regular.ttf") { Bases = [ (0x05D0, 0x05EA) ]; Marks = [ (0x05B0, 0x05BC) ]; Joiner = []; Leading = [] } "generated: 1,500 lines of 8 words of 1 to 3 letters with up to 2 points"
        synthetic "Khmer/Khmer.ttf" (subset "Khmer.ttf") { Bases = [ (0x1780, 0x17A2) ]; Marks = [ (0x17B6, 0x17C5); (0x17C6, 0x17D1) ]; Joiner = [ 0x17D2 ]; Leading = [] } "generated: 1,500 lines of 8 words of consonant, optional coeng and consonant, vowels and signs"
        synthetic "Khmer/Khmer.ttf without morx (OpenType)" khmerOpenType { Bases = [ (0x1780, 0x17A2) ]; Marks = [ (0x17B6, 0x17C5); (0x17C6, 0x17D1) ]; Joiner = [ 0x17D2 ]; Leading = [] } "generated as the row above; the font's morx table is renamed away in a copy, so the plan is the Khmer shaper on the OpenType path"
        synthetic "Myanmar/NotoSerifMyanmar" (subset "NotoSerifMyanmar-Regular.otf") { Bases = [ (0x1000, 0x1021) ]; Marks = [ (0x102B, 0x1032); (0x103A, 0x103E) ]; Joiner = [ 0x1039 ]; Leading = [] } "generated: 1,500 lines of 8 words of consonant, optional virama and consonant, vowels, medials, asat"
        synthetic "Telugu/SreeKrushnadevaraya" (subset "SreeKrushnadevaraya-Regular.ttf") { Bases = [ (0x0C15, 0x0C39) ]; Marks = [ (0x0C3E, 0x0C4C); (0x0C02, 0x0C03) ]; Joiner = [ 0x0C4D ]; Leading = [] } "generated: Indic syllables, consonant, optional virama and consonant, vowel signs"
        synthetic "Kannada/NotoSerifKannada" (shape "NotoSerifKannada-Regular.ttf") { Bases = [ (0x0C95, 0x0CB9) ]; Marks = [ (0x0CBE, 0x0CCC); (0x0C82, 0x0C83) ]; Joiner = [ 0xCCD ]; Leading = [] } "generated: Indic syllables, consonant, optional virama and consonant, vowel signs"
        synthetic "Newa/NotoSansNewa (USE)" (subset "NotoSansNewa-Regular.ttf") { Bases = [ (0x11400, 0x11434) ]; Marks = [ (0x11435, 0x11446) ]; Joiner = [ 0x11442 ]; Leading = [] } "generated: USE syllables, consonant, optional virama and consonant, vowel signs"
        synthetic "Balinese/NotoSansBalinese (USE)" (shape "NotoSansBalinese-Regular.ttf") { Bases = [ (0x1B13, 0x1B33) ]; Marks = [ (0x1B35, 0x1B43); (0x1B00, 0x1B04) ]; Joiner = [ 0x1B44 ]; Leading = [] } "generated: USE syllables, consonant, optional adeg-adeg and consonant, vowel signs"
        { Name = "Hangul/AdobeBlank"; FontPath = blankPath; Text = hangul (font blankPath) 1500 8; Description = "generated: precomposed syllables and jamo triples; the font has no layout tables, so this is the normaliser and composition cost only" }
        { Name = "Latin/Roboto French combining marks"; FontPath = robotoPath; Text = combining; Description = "the Little Prince with French accents written as a letter and a combining mark" }
        synthetic "Tibetan/AdobeBlank (USE)" blankPath { Bases = [ (0x0F40, 0x0F6C) ]; Marks = [ (0x0F71, 0x0F84) ]; Joiner = []; Leading = [] } "generated: Tibetan consonants with vowel signs; the font has no layout tables, so this is the USE segmentation cost only" ]

/// <summary>The one-minute load average of the machine, as <c>sysctl</c> prints it.</summary>
/// <returns>The text.</returns>
let loadAverage () =
    let info = ProcessStartInfo("sysctl", "-n vm.loadavg", RedirectStandardOutput = true)
    match Process.Start info with
    | null -> String.Empty
    | sysctl ->
        use sysctl = sysctl
        let text = sysctl.StandardOutput.ReadToEnd().Trim()
        sysctl.WaitForExit()
        text

/// <summary>Whether the whole pass takes the entry that decodes into the shaping arrays, <c>COMPLEX_FRESH</c> set to 1.</summary>
let private freshEntry = Environment.GetEnvironmentVariable "COMPLEX_FRESH" = "1"

/// <summary>Shapes every line with Farfbuzz, threading the plan cache.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <param name="cache">The plan cache from the previous pass.</param>
/// <returns>The glyph count and the cache.</returns>
let private shapeFarfbuzz (font: Font) (text: byte array) (lines: struct (int * int) array) (cache: PlanCache) =
    let mutable glyphs = 0
    let mutable kept = cache
    for struct (start, length) in lines do
        let struct (shaped, next) =
            match freshEntry with
            | true -> Shape.shapeUtf8 kept font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory(text, start, text.Length - start)) 0 length
            | false ->
                let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
                let run = GlyphRun.guessSegmentProperties Language.cLocale run
                Shape.shapeCached kept font ImmutableArray<Feature>.Empty ValueNone run
        kept <- next
        match shaped with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    struct (glyphs, kept)

/// <summary>Shapes every line with HarfBuzz.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count.</returns>
let private shapeHarfBuzz (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    for struct (start, length) in lines do
        buffer.ClearContents()
        buffer.AddUtf8(ReadOnlySpan(text, start, text.Length - start), 0, length)
        buffer.GuessSegmentProperties()
        font.Shape buffer
        glyphs <- glyphs + buffer.Length
    glyphs

/// <summary>Counts the lines whose shaped glyph ids, clusters and positions differ between Farfbuzz and HarfBuzz.</summary>
/// <param name="farfbuzzFont">The Farfbuzz font.</param>
/// <param name="font">The HarfBuzz font.</param>
/// <param name="buffer">The reused buffer.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The number of differing lines.</returns>
let private differing (farfbuzzFont: Font) (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (text: byte array) (lines: struct (int * int) array) =
    let mutable cache = PlanCache.empty
    let mutable different = 0
    for struct (start, length) in lines do
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (shaped, next) = Shape.shapeCached cache farfbuzzFont ImmutableArray<Feature>.Empty ValueNone run
        cache <- next
        buffer.ClearContents()
        buffer.AddUtf8(ReadOnlySpan(text, start, text.Length - start), 0, length)
        buffer.GuessSegmentProperties()
        font.Shape buffer
        match shaped with
        | Error _ -> different <- different + 1
        | Ok shaped ->
            let theirInfos = buffer.GlyphInfos
            let theirPositions = buffer.GlyphPositions
            let same =
                theirInfos.Length = shaped.Infos.Length
                && Seq.forall2 (fun (a: GlyphInfo) (b: HarfBuzzSharp.GlyphInfo) -> a.Codepoint = b.Codepoint && a.Cluster = b.Cluster) shaped.Infos theirInfos
                && Seq.forall2 (fun (a: GlyphPosition) (b: HarfBuzzSharp.GlyphPosition) -> a.XAdvance = b.XAdvance && a.YAdvance = b.YAdvance && a.XOffset = b.XOffset && a.YOffset = b.YOffset) shaped.Positions theirPositions
            if not same then different <- different + 1
    different

/// <summary>One pass timed, with the bytes it allocated.</summary>
/// <param name="work">The work.</param>
/// <returns>The milliseconds and the bytes.</returns>
let private timed (work: unit -> int) =
    let before = GC.GetAllocatedBytesForCurrentThread()
    let started = Stopwatch.GetTimestamp()
    work () |> ignore
    let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds
    struct (elapsed, GC.GetAllocatedBytesForCurrentThread() - before)

/// <summary>Opens the HarfBuzz font of a path.</summary>
/// <param name="path">The font file.</param>
/// <returns>The face and the font, to dispose.</returns>
let private openHarfBuzz (path: string) =
    let face = new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile path, 0)
    let font = new HarfBuzzSharp.Font(face)
    font.SetFunctionsOpenType()
    struct (face, font)

/// <summary>Sets the weight axis of both fonts when <c>COMPLEX_WGHT</c> names a value, so that a variable font is shaped at non-default
/// coordinates by both engines.</summary>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="hbFont">The HarfBuzz font.</param>
/// <returns>The Farfbuzz font.</returns>
let private withWeight (font: Font) (hbFont: HarfBuzzSharp.Font) =
    match Environment.GetEnvironmentVariable "COMPLEX_WGHT" with
    | null | "" -> font
    | text ->
        let value = Single.Parse (text, Globalization.CultureInfo.InvariantCulture)
        let variation = HarfBuzzSharp.Variation(Tag = 0x77676874u, Value = value)
        hbFont.SetVariations (ReadOnlySpan [| variation |]) |> TailCalls.settle
        Font.setVariation (Tag.ofString "wght") value font

/// <summary>Times every row, HarfBuzz and Farfbuzz alternating, warm, keeping the minimum and the median of the rounds.
/// <c>COMPLEX_ROUNDS</c> sets the rounds (default 9), <c>COMPLEX_ONLY</c> keeps the rows whose name contains the text,
/// <c>COMPLEX_WGHT</c> sets the weight axis.</summary>
let run () =
    oracle.Force() |> ignore
    let rounds = match Environment.GetEnvironmentVariable "COMPLEX_ROUNDS" with | null | "" -> 9 | text -> int text
    let only = match Environment.GetEnvironmentVariable "COMPLEX_ONLY" with | null -> String.Empty | text -> text
    Console.WriteLine $"load average at the start: {loadAverage ()}; rounds per row: {rounds}"
    Console.WriteLine "| row | lines | glyphs | differing lines | HarfBuzz ms (min / median) | Farfbuzz ms (min / median) | HarfBuzz ns/glyph | Farfbuzz ns/glyph | ratio (min) | ratio (median) | Farfbuzz B/glyph | load after |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
    for row in rows () do
        if String.IsNullOrEmpty only || row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let struct (face, hbFont) = openHarfBuzz row.FontPath
            use _face = face
            use _font = hbFont
            let font = withWeight font hbFont
            use buffer = new HarfBuzzSharp.Buffer()
            let struct (glyphs, kept) = shapeFarfbuzz font row.Text lines PlanCache.empty
            let mutable cache = kept
            let different = differing font hbFont buffer row.Text lines
            let warmUntil = Stopwatch.GetTimestamp() + Stopwatch.Frequency * 2L
            while Stopwatch.GetTimestamp() < warmUntil do
                let struct (_, next) = shapeFarfbuzz font row.Text lines cache
                cache <- next
                shapeHarfBuzz hbFont buffer row.Text lines |> ignore
            let hbTimes = Collections.Generic.List<float>()
            let ffTimes = Collections.Generic.List<float>()
            let mutable bytes = 0L
            for round in 0 .. rounds do
                let struct (hb, _) = timed (fun () -> shapeHarfBuzz hbFont buffer row.Text lines)
                let struct (ff, allocated) =
                    timed (fun () ->
                        let struct (total, next) = shapeFarfbuzz font row.Text lines cache
                        cache <- next
                        total)
                if round > 0 then
                    hbTimes.Add hb
                    ffTimes.Add ff
                    bytes <- allocated
            hbTimes.Sort()
            ffTimes.Sort()
            let hbMin, hbMedian = hbTimes[0], hbTimes[hbTimes.Count / 2]
            let ffMin, ffMedian = ffTimes[0], ffTimes[ffTimes.Count / 2]
            let perGlyph (milliseconds: float) = milliseconds * 1.0e6 / float glyphs
            Console.WriteLine($"| {row.Name} | {lines.Length} | {glyphs} | {different} | {hbMin:N2} / {hbMedian:N2} | {ffMin:N2} / {ffMedian:N2} | {perGlyph hbMin:N1} | {perGlyph ffMin:N1} | {ffMin / hbMin:N2} | {ffMedian / hbMedian:N2} | {float bytes / float glyphs:N0} | {loadAverage ()} |")
            Console.Out.Flush()

/// <summary>Shapes one row with Farfbuzz only, for a fixed number of seconds, printing the process id first so that
/// <c>sample</c> can be attached; the row is the first whose name contains the argument.</summary>
/// <param name="name">Part of the row name.</param>
/// <param name="seconds">The duration.</param>
let loop (name: string) (seconds: int) =
    let row = rows () |> List.find (fun row -> row.Name.Contains name)
    let lines = Corpus.lines row.Text
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
    let font =
        match Environment.GetEnvironmentVariable "COMPLEX_WGHT" with
        | null | "" -> font
        | text -> Font.setVariation (Tag.ofString "wght") (Single.Parse (text, Globalization.CultureInfo.InvariantCulture)) font
    let struct (_, kept) = shapeFarfbuzz font row.Text lines PlanCache.empty
    let mutable cache = kept
    Console.WriteLine $"pid {Environment.ProcessId}, row {row.Name}"
    Console.Out.Flush()
    let started = Stopwatch.GetTimestamp()
    let mutable passes = 0
    while Stopwatch.GetElapsedTime(started).TotalSeconds < float seconds do
        let struct (_, next) = shapeFarfbuzz font row.Text lines cache
        cache <- next
        passes <- passes + 1
    Console.WriteLine $"{passes} passes"

/// <summary>Where the time and the bytes of one row go, stage by stage through the general pipeline, the GSUB stages one
/// window and one pause at a time. <c>COMPLEX_ONLY</c> selects the row; each line is shaped <c>COMPLEX_PASSES</c> times
/// (default 12), the last pass measured. <c>COMPLEX_OWNED</c> 1 takes the working-set route, with the pauses that write in place; 2 takes the
/// working-set route with the pauses of the reference.</summary>
let stages () =
    let only = match Environment.GetEnvironmentVariable "COMPLEX_ONLY" with | null -> String.Empty | text -> text
    let passes = match Environment.GetEnvironmentVariable "COMPLEX_PASSES" with | null | "" -> 12 | text -> int text
    let route = Environment.GetEnvironmentVariable "COMPLEX_OWNED"
    let owned = route = "1" || route = "2"
    let ownedPauses = route = "1"
    for row in rows () do
        if row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let first = GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan(row.Text, 0, row.Text.Length)) 0 (let struct (_, length) = lines[0] in length) GlyphRun.empty)
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> Console.WriteLine "no plan"
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> Console.WriteLine "no ot plan"
                | ValueSome plan ->
                    let names = Collections.Generic.List<string>()
                    let ticks = Collections.Generic.Dictionary<string, int64>()
                    let bytes = Collections.Generic.Dictionary<string, int64>()
                    let mutable glyphs = 0
                    let mutable recording = false
                    let measure (name: string) (work: unit -> 'a) =
                        let before = GC.GetAllocatedBytesForCurrentThread()
                        let started = Stopwatch.GetTimestamp()
                        let result = work ()
                        let elapsed = Stopwatch.GetTimestamp() - started
                        let allocated = GC.GetAllocatedBytesForCurrentThread() - before
                        if recording then
                            if not (ticks.ContainsKey name) then
                                names.Add name
                                ticks[name] <- 0L
                                bytes[name] <- 0L
                            ticks[name] <- ticks[name] + elapsed
                            bytes[name] <- bytes[name] + allocated
                        result
                    let stageList = plan.Map.GsubStages
                    for pass in 1 .. passes do
                        recording <- pass = passes
                        for struct (start, length) in lines do
                            let decoded = measure "decode (addUtf8)" (fun () -> GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty |> TailCalls.settle)
                            let run = measure "segment properties" (fun () -> GlyphRun.guessSegmentProperties Language.cLocale decoded)
                            let target = run.SegmentProperties.Direction
                            measure "whole entry (Execute.run, for the total below)" (fun () -> Execute.run shapePlan font ImmutableArray<Feature>.Empty run) |> ignore
                            if owned then
                                let propped = measure "unicode props (entered)" (fun () -> OtProps.setUnicodePropsEnteredSet plan.Map.GlobalMask run)
                                let circled = measure "dotted circle" (fun () -> GeneralLine.insertDottedCircleSet font propped)
                                let clustered = measure "form clusters" (fun () -> GeneralLine.formClustersSet circled)
                                let native = measure "native direction" (fun () -> GeneralLine.ensureNativeDirectionSet clustered)
                                let prepared = measure "preprocess hook" (fun () -> ShaperHooks.preprocessSet plan font native)
                                let rotated = measure "rotate chars" (fun () -> GeneralLine.rotateCharsSet plan font prepared)
                                let normalised = measure "normalise" (fun () -> Normalize.runOwnedSet plan font rotated)
                                match plan.Shaper.Kind, plan.Data with
                                | ArabicShaper, ArabicData arabicData ->
                                    let copyOfNormalised = GlyphRun.setOf { GlyphRun.runOf normalised with Infos = Sealed.ofSpan (normalised.Infos.AsSpan()); Props = Sealed.ofSpan (normalised.Props.AsSpan()) }
                                    measure "  of which the joining and its masks (on a copy)" (fun () -> Arabic.setupMasksSet arabicData plan.Props.Script copyOfNormalised) |> ignore
                                    let copyForReference = GlyphRun.runOf normalised
                                    measure "  of which the joining and its masks, the reference (on a copy)" (fun () -> Arabic.setupMasksPlan arabicData plan.Props.Script copyForReference) |> ignore
                                | _ -> ()
                                let synthesised = measure "masks, glyphs and classes" (fun () -> GeneralLine.glyphsOfSet plan font normalised)
                                let mutable current = synthesised
                                let mutable windowFirst = 0
                                let mutable windowIndex = 0
                                for stage in 0 .. stageList.Length - 1 do
                                    if PauseKind.runsFunction stageList[stage].Pause then
                                        let last = stageList[stage].LastLookup
                                        let window = current
                                        let applied = measure $"GSUB lookups {windowFirst} to {last}" (fun () -> OtLayout.substituteWindowOwnedSet shapePlan.GsubLookupCovered font shapePlan.GsubWindows[windowIndex] window)
                                        match ownedPauses, stageList[stage].Pause, plan.Data with
                                        | true, IndicInitialReordering, IndicData indicData ->
                                            let copyOfApplied = GlyphRun.setOf { GlyphRun.runOf applied with Infos = Sealed.ofSpan (applied.Infos.AsSpan()); Props = Sealed.ofSpan (applied.Props.AsSpan()) }
                                            measure "  of which the consonant positions" (fun () -> IndicInitialOwned.updateConsonantPositions indicData font copyOfApplied) |> ignore
                                        | _ -> ()
                                        current <- measure (sprintf "pause %A" stageList[stage].Pause) (fun () -> if ownedPauses then ShaperHooks.applyPauseSet stageList[stage].Pause plan font applied else GlyphRun.setOf (ShaperHooks.applyPause stageList[stage].Pause plan font (GlyphRun.runOf applied)))
                                        windowFirst <- last
                                        windowIndex <- windowIndex + 1
                                let tail = current
                                let substituted = measure $"GSUB lookups {windowFirst} to end" (fun () -> if not shapePlan.GsubWindows.IsEmpty then OtLayout.substituteWindowOwnedSet shapePlan.GsubLookupCovered font shapePlan.GsubWindows[windowIndex] tail else Engine.substituteApplicableOwnedSet plan font shapePlan.GsubLookupCovered shapePlan.GsubGlyphLookups tail)
                                match measure "default advances" (fun () -> GeneralLine.advancesOf plan font substituted.Infos substituted.Props) with
                                | ValueNone -> ()
                                | ValueSome advances ->
                                    match measure "GPOS" (fun () -> GeneralLine.gposPartsSet shapePlan plan font substituted advances) with
                                    | ValueNone -> ()
                                    | ValueSome (struct (maxOps, scratch)) ->
                                        let shaped = measure "finishing" (fun () -> GeneralLine.finishedSet plan font target { substituted with Positions = advances; HavePositions = true; MaxOps = maxOps; ScratchFlags = scratch })
                                        if recording then glyphs <- glyphs + shaped.Infos.Length
                            else
                                let entered = measure "enter" (fun () -> GlyphRun.enter run)
                                let propped = measure "unicode props" (fun () -> OtProps.setUnicodePropsFused plan.Map.GlobalMask entered)
                                let circled = measure "dotted circle" (fun () -> OtProps.insertDottedCircle font propped)
                                let clustered = measure "form clusters" (fun () -> OtProps.formClusters circled)
                                let native = measure "native direction" (fun () -> OtProps.ensureNativeDirection clustered)
                                let prepared = measure "preprocess hook" (fun () -> ShaperHooks.preprocess plan font native)
                                let rotated = measure "rotate chars" (fun () -> OtSubstitute.rotateChars plan font plan.Props.Direction prepared)
                                let normalised = measure "normalise" (fun () -> Normalize.run plan font rotated)
                                let masked = measure "setup masks" (fun () -> OtSubstitute.setupMasks plan normalised)
                                let recategorised = measure "recategorise marks" (fun () -> match plan.FallbackMarkPositioning with | true -> OtLayout.recategorizeMarks masked | false -> masked)
                                let mapped = measure "map glyphs" (fun () -> Normalize.mapGlyphsFast recategorised)
                                let started = measure "glyph props" (fun () -> OtSubstitute.substituteStart font mapped)
                                let synthesised = measure "synthesise classes" (fun () -> match plan.FallbackGlyphClasses with | true -> OtProps.synthesizeGlyphClasses started | false -> started)
                                let mutable current = synthesised
                                let mutable windowFirst = 0
                                for stage in 0 .. stageList.Length - 1 do
                                    if PauseKind.runsFunction stageList[stage].Pause then
                                        let last = stageList[stage].LastLookup
                                        let window = current
                                        let applied = measure $"GSUB lookups {windowFirst} to {last}" (fun () -> OtLayout.substituteWindow plan font windowFirst last window)
                                        current <- measure (sprintf "pause %A" stageList[stage].Pause) (fun () -> ShaperHooks.applyPause stageList[stage].Pause plan font applied)
                                        windowFirst <- last
                                let tail = current
                                let substituted = measure $"GSUB lookups {windowFirst} to end" (fun () -> OtLayout.substituteWindow plan font windowFirst stageList[stageList.Length - 1].LastLookup tail)
                                match measure "GPOS" (fun () -> OtPosition.position plan font substituted) with
                                | Error _ -> ()
                                | Ok positioned ->
                                    let post = measure "post hook" (fun () -> OtSubstitute.substitutePost plan font positioned)
                                    let propagated = measure "propagate flags" (fun () -> Flags.propagate post)
                                    if recording then glyphs <- glyphs + propagated.Infos.Length
                                    ignore target
                    let entryName = "whole entry (Execute.run, for the total below)"
                    let total = ticks |> Seq.filter (fun pair -> pair.Key <> entryName) |> Seq.sumBy (fun pair -> pair.Value)
                    Console.WriteLine $"### {row.Name}: {glyphs} glyphs over {lines.Length} lines (a stage that this pass did not reach is omitted; times with per-stage timer overhead of about 50 ns)"
                    Console.WriteLine "| stage | ns/glyph | % of time | B/glyph |"
                    Console.WriteLine "|---|---:|---:|---:|"
                    for name in names do
                        let ns = float ticks[name] * 1e9 / float Stopwatch.Frequency
                        Console.WriteLine $"| {name} | {ns / float glyphs:N1} | {100.0 * float ticks[name] / float total:N1} | {float bytes[name] / float glyphs:N1} |"
                    let totalNs = float total * 1e9 / float Stopwatch.Frequency
                    Console.WriteLine $"| total | {totalNs / float glyphs:N1} | 100.0 | {float (bytes |> Seq.filter (fun pair -> pair.Key <> entryName) |> Seq.sumBy (fun pair -> pair.Value)) / float glyphs:N1} |"

/// <summary>The stages of the comparison, in the order of the table, and the rows of HarfBuzz's direct stages each one is the sum of.</summary>
let private comparedStages =
    [| "unicode props", [ 0 ]
       "dotted circle, clusters, native direction", [ 1; 2; 3 ]
       "preprocess hook", [ 4 ]
       "rotate chars", [ 5 ]
       "normalise", [ 6 ]
       "masks, recategorise, map glyphs, glyph props, classes", [ 7; 8; 9; 10; 11 ]
       "GSUB (lookups and pauses)", [ 12 ]
       "default advances", [ 13 ]
       "GPOS (position start, zeroing, the table)", [ 14; 15; 16; 17 ]
       "finishing (offsets, fallback marks, reverse, post hook, flags)", [ 18; 19; 20; 21; 22 ] |]

/// <summary>Runs the timing entry of the oracle once.</summary>
/// <param name="font">The font file.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <param name="rounds">The measured rounds.</param>
/// <param name="mode">The mode of the entry.</param>
/// <returns>The doubles the entry wrote, or none when it refused the font.</returns>
let private hbTimes (font: byte array) (text: byte array) (lines: struct (int * int) array) (rounds: int) (mode: int) =
    let starts = lines |> Array.map (fun (struct (start, _)) -> start)
    let lengths = lines |> Array.map (fun (struct (_, length)) -> length)
    let output = Array.zeroCreate<double> 64
    let written = Farfbuzz.Oracle.Shims.farfbuzz_oracle_time_stages (font, uint32 font.Length, text, uint32 text.Length, starts, lengths, uint32 lines.Length, uint32 rounds, uint32 mode, output, uint32 output.Length)
    match written with
    | -1 -> ValueNone
    | count -> ValueSome output[.. count - 1]

/// <summary>The cost of one pair of timestamps with the .NET stopwatch, in nanoseconds: the least of 200 batches.</summary>
let private stopwatchCost () =
    let mutable best = Double.MaxValue
    for _ in 1 .. 200 do
        let mutable sum = 0L
        for _ in 1 .. 500 do
            let first = Stopwatch.GetTimestamp()
            let second = Stopwatch.GetTimestamp()
            sum <- sum + (second - first)
        best <- min best (float sum * 1e9 / float Stopwatch.Frequency / 500.0)
    best

/// <summary>The first slot of the ticks of each pause of the stage list.</summary>
[<Literal>]
let private PauseBase = 12

/// <summary>The pauses whose ticks are kept apart.</summary>
[<Literal>]
let private PauseSlots = 6

/// <summary>Farfbuzz's working-set route through the stages of the comparison on one line: the ticks of each stage, GSUB windows and
/// pauses kept apart (stage 6 and 10).</summary>
/// <param name="shapePlan">The shape plan.</param>
/// <param name="plan">The ot plan.</param>
/// <param name="font">The font.</param>
/// <param name="run">The run, made outside the timing.</param>
/// <param name="timed">Whether a timestamp is taken between stages; false takes the two around the line.</param>
/// <param name="ticks">The ticks of each stage, overwritten.</param>
/// <returns>The ticks of the line.</returns>
let private ownedLine (shapePlan: ShapePlan) (plan: OtShapePlan) (font: Font) (run: GlyphRun) (timed: bool) (ticks: int64 array) =
    Array.fill ticks 0 ticks.Length 0L
    let stageList = plan.Map.GsubStages
    let target = run.SegmentProperties.Direction
    let started = Stopwatch.GetTimestamp()
    let mutable mark = started
    let inline stage (index: int) =
        if timed then
            let now = Stopwatch.GetTimestamp()
            ticks[index] <- now - mark
            mark <- now
    let propped = OtProps.setUnicodePropsEnteredSet plan.Map.GlobalMask run
    stage 0
    let circled = GeneralLine.insertDottedCircleSet font propped
    let clustered = GeneralLine.formClustersSet circled
    let native = GeneralLine.ensureNativeDirectionSet clustered
    stage 1
    let prepared = ShaperHooks.preprocessSet plan font native
    stage 2
    let rotated = GeneralLine.rotateCharsSet plan font prepared
    stage 3
    let normalised = Normalize.runOwnedSet plan font rotated
    stage 4
    let synthesised = GeneralLine.glyphsOfSet plan font normalised
    stage 5
    let mutable current = synthesised
    let mutable windowIndex = 0
    let mutable gsub = 0L
    let mutable pauses = 0L
    let mutable stamps = 9L
    for index in 0 .. stageList.Length - 1 do
        if PauseKind.runsFunction stageList[index].Pause then
            let applied = OtLayout.substituteWindowOwnedSet shapePlan.GsubLookupCovered font shapePlan.GsubWindows[windowIndex] current
            if timed then
                let now = Stopwatch.GetTimestamp()
                gsub <- gsub + (now - mark)
                mark <- now
                stamps <- stamps + 2L
            current <- ShaperHooks.applyPauseSet stageList[index].Pause plan font applied
            if timed then
                let now = Stopwatch.GetTimestamp()
                pauses <- pauses + (now - mark)
                ticks[PauseBase + min windowIndex (PauseSlots - 1)] <- ticks[PauseBase + min windowIndex (PauseSlots - 1)] + (now - mark)
                mark <- now
            windowIndex <- windowIndex + 1
    let substituted = if not shapePlan.GsubWindows.IsEmpty then OtLayout.substituteWindowOwnedSet shapePlan.GsubLookupCovered font shapePlan.GsubWindows[windowIndex] current else Engine.substituteApplicableOwnedSet plan font shapePlan.GsubLookupCovered shapePlan.GsubGlyphLookups current
    if timed then
        let now = Stopwatch.GetTimestamp()
        gsub <- gsub + (now - mark)
        mark <- now
        stamps <- stamps + 1L
    ticks[6] <- gsub
    ticks[10] <- pauses
    ticks[11] <- stamps
    match GeneralLine.advancesOf plan font substituted.Infos substituted.Props with
    | ValueNone -> ()
    | ValueSome advances ->
        stage 7
        match GeneralLine.gposPartsSet shapePlan plan font substituted advances with
        | ValueNone -> ()
        | ValueSome (struct (maxOps, scratch)) ->
            stage 8
            GeneralLine.finishedSet plan font target { substituted with Positions = advances; HavePositions = true; MaxOps = maxOps; ScratchFlags = scratch } |> ignore
            stage 9
    match timed with
    | true -> Array.sum (Array.sub ticks 0 11) - stamps
    | false -> Stopwatch.GetTimestamp() - started

/// <summary>The normaliser alone on the lines of the complex rows, in a warm loop: ns per glyph of its input, the minimum of
/// <c>NORM_PASSES</c> passes (default 60), the arrays copied outside the timer. <c>COMPLEX_ONLY</c> selects the rows; with
/// <c>NORM_LOOP</c> set to a number of seconds it loops for a sampling profile.</summary>
let normComplex () =
    let only = match Environment.GetEnvironmentVariable "COMPLEX_ONLY" with | null -> String.Empty | text -> text
    let passes = match Environment.GetEnvironmentVariable "NORM_PASSES" with | null | "" -> 60 | text -> int text
    let loopSeconds = match Environment.GetEnvironmentVariable "NORM_LOOP" with | null | "" -> 0 | text -> int text
    Console.WriteLine "| row | glyphs in | glyphs out | ns per glyph in | bytes per glyph in |"
    Console.WriteLine "|---|---:|---:|---:|---:|"
    for row in rows () do
        if row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let runOf (struct (start, length)) = GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty)
            let first = runOf lines[0]
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> ()
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> ()
                | ValueSome plan ->
                    let prepared =
                        lines
                        |> Array.map (fun line ->
                            let propped = OtProps.setUnicodePropsEnteredSet plan.Map.GlobalMask (runOf line)
                            let circled = GeneralLine.insertDottedCircleSet font propped
                            let clustered = GeneralLine.formClustersSet circled
                            let native = GeneralLine.ensureNativeDirectionSet clustered
                            let prepared = ShaperHooks.preprocessSet plan font native
                            GeneralLine.rotateCharsSet plan font prepared)
                    let glyphsIn = prepared |> Array.sumBy (fun set -> set.Infos.Length)
                    let copy (set: WorkingSet) =
                        { set with
                            Infos = ImmutableArray.Create<GlyphInfo>(set.Infos.AsSpan())
                            Props = ImmutableArray.Create<GlyphProps>(set.Props.AsSpan()) }
                    let mutable glyphsOut = 0
                    let mutable best = Int64.MaxValue
                    let mutable bytes = 0L
                    let started = Stopwatch.GetTimestamp()
                    let mutable pass = 0
                    while (loopSeconds = 0 && pass <= passes) || (loopSeconds > 0 && Stopwatch.GetElapsedTime(started).TotalSeconds < float loopSeconds) do
                        let copies = prepared |> Array.map copy
                        let b0 = GC.GetAllocatedBytesForCurrentThread()
                        let t0 = Stopwatch.GetTimestamp()
                        let mutable out = 0
                        for set in copies do
                            out <- out + (Normalize.runOwnedSet plan font set).Infos.Length
                        let t1 = Stopwatch.GetTimestamp()
                        glyphsOut <- out
                        if pass > 0 then
                            best <- min best (t1 - t0)
                            bytes <- GC.GetAllocatedBytesForCurrentThread() - b0
                        pass <- pass + 1
                    let ns = float best * 1e9 / float Stopwatch.Frequency
                    Console.WriteLine $"| {row.Name} | {glyphsIn} | {glyphsOut} | {ns / float glyphsIn:N1} | {float bytes / float glyphsIn:N1} |"

/// <summary>Compares HarfBuzz's stages with Farfbuzz's, one row of the corpus at a time: HarfBuzz's direct stages (the pieces of
/// <c>hb_ot_shape_internal</c> timed in the oracle), its message stages, the calibration of the trace, and Farfbuzz's working-set route
/// stage by stage, ns per glyph, the minimum of <c>COMPLEX_ROUNDS</c> rounds (default 25); <c>COMPLEX_ONLY</c> selects the rows.</summary>
let compare () =
    oracle.Force() |> ignore
    let rounds = match Environment.GetEnvironmentVariable "COMPLEX_ROUNDS" with | null | "" -> 25 | text -> int text
    let only = match Environment.GetEnvironmentVariable "COMPLEX_ONLY" with | null -> String.Empty | text -> text
    let swCost = stopwatchCost ()
    Console.WriteLine $"load average at the start: {loadAverage ()}; rounds: {rounds}; .NET timestamp pair: {swCost:N1} ns; each line keeps its fastest round"
    for row in rows () do
        if row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let fontBytes = File.ReadAllBytes row.FontPath
            let font = Font.create (Face.create (ReadOnlyMemory fontBytes) 0u)
            let runOf (struct (start, length)) = GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty)
            let first = runOf lines[0]
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> Console.WriteLine $"### {row.Name}: no plan"
            | Ok shapePlan ->
                match shapePlan.Ot, hbTimes fontBytes row.Text lines rounds 0 with
                | ValueNone, _ | _, ValueNone -> Console.WriteLine $"### {row.Name}: no OpenType plan or AAT"
                | ValueSome plan, ValueSome direct ->
                    let hbMessages = ValueOption.get (hbTimes fontBytes row.Text lines rounds 1)
                    let untraced = ValueOption.get (hbTimes fontBytes row.Text lines rounds 2)
                    let noop = ValueOption.get (hbTimes fontBytes row.Text lines rounds 3)
                    let stamp = ValueOption.get (hbTimes fontBytes row.Text lines rounds 4)
                    let glyphs = direct[0]
                    let hbTimer = direct[1]
                    let lineCount = float lines.Length
                    let hbRawSum = Seq.sum (seq { for index in 0 .. 22 -> direct[5 + 2 * index] })
                    let hbPerMark = max 0.0 ((hbRawSum - untraced[4]) / (lineCount * 23.0))
                    let hbStage (index: int) = max 0.0 (direct[5 + 2 * index] - lineCount * hbPerMark)
                    let hbRow (members: int list) = members |> List.sumBy hbStage
                    let runs = lines |> Array.map (fun line -> runOf line |> TailCalls.settle)
                    let stageCount = PauseBase + PauseSlots
                    let scratch = Array.zeroCreate<int64> stageCount
                    let bestLine = Array.create runs.Length Int64.MaxValue
                    let bestTicks = Array.zeroCreate<int64> (runs.Length * stageCount)
                    let bestPlain = Array.create runs.Length Int64.MaxValue
                    let bestEntry = Array.create runs.Length Int64.MaxValue
                    for round in 0 .. rounds do
                        for index in 0 .. runs.Length - 1 do
                            let total = ownedLine shapePlan plan font runs[index] true scratch
                            if round > 0 && total < bestLine[index] then
                                bestLine[index] <- total
                                Array.blit scratch 0 bestTicks (index * stageCount) stageCount
                        for index in 0 .. runs.Length - 1 do
                            let plain = ownedLine shapePlan plan font runs[index] false scratch
                            if round > 0 && plain < bestPlain[index] then bestPlain[index] <- plain
                        for index in 0 .. runs.Length - 1 do
                            let started = Stopwatch.GetTimestamp()
                            Execute.run shapePlan font ImmutableArray<Feature>.Empty runs[index] |> ignore
                            let entryTicks = Stopwatch.GetTimestamp() - started
                            if round > 0 && entryTicks < bestEntry[index] then bestEntry[index] <- entryTicks
                    let toNs (value: int64) = float value * 1e9 / float Stopwatch.Frequency
                    let ffRaw (index: int) = toNs (Seq.sum (seq { for line in 0 .. runs.Length - 1 -> bestTicks[line * stageCount + index] }))
                    let stampTotal = Seq.sum (seq { for line in 0 .. runs.Length - 1 -> float bestTicks[line * stageCount + 11] })
                    let plainTotal = toNs (Array.sum bestPlain)
                    let ffSum = Seq.sum (seq { for index in [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] -> ffRaw index })
                    let ffPerMark = max 0.0 ((ffSum - plainTotal) / stampTotal)
                    let ffStage (index: int) = max 0.0 (ffRaw index - lineCount * ffPerMark)
                    let pauseCount = (stampTotal - 10.0 * lineCount) / 2.0
                    let ffWindows = max 0.0 (ffRaw 6 - (pauseCount + lineCount) * ffPerMark)
                    let ffPauses = max 0.0 (ffRaw 10 - pauseCount * ffPerMark)
                    let ffGsub = ffWindows + ffPauses
                    let ffValues = [| ffStage 0; ffStage 1; ffStage 2; ffStage 3; ffStage 4; ffStage 5; ffGsub; ffStage 7; ffStage 8; ffStage 9 |]
                    let hbValues = comparedStages |> Array.map (fun (_, members) -> hbRow members)
                    let entry = toNs (Array.sum bestEntry)
                    let bestText = Array.create runs.Length Int64.MaxValue
                    let mutable cache = PlanCache.empty
                    for round in 0 .. rounds do
                        for index in 0 .. runs.Length - 1 do
                            let started = Stopwatch.GetTimestamp()
                            let run = runOf lines[index]
                            let struct (shaped, next) = Shape.shapeCached cache font ImmutableArray<Feature>.Empty ValueNone run
                            cache <- next
                            let spent = Stopwatch.GetTimestamp() - started
                            if round > 0 && spent < bestText[index] then bestText[index] <- spent
                            shaped |> ignore
                    let fromText = toNs (Array.sum bestText)
                    let bestDecode = Array.create runs.Length Int64.MaxValue
                    let bestGuess = Array.create runs.Length Int64.MaxValue
                    for round in 0 .. rounds do
                        for index in 0 .. runs.Length - 1 do
                            let struct (start, length) = lines[index]
                            let started = Stopwatch.GetTimestamp()
                            let decoded = GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty |> TailCalls.settle
                            let decodeTicks = Stopwatch.GetTimestamp() - started
                            let guessed = GlyphRun.guessSegmentProperties Language.cLocale decoded
                            let guessTicks = Stopwatch.GetTimestamp() - started - decodeTicks
                            if round > 0 && decodeTicks < bestDecode[index] then bestDecode[index] <- decodeTicks
                            if round > 0 && guessTicks < bestGuess[index] then bestGuess[index] <- guessTicks
                            guessed |> ignore
                    let bestFresh = Array.create runs.Length Int64.MaxValue
                    for round in 0 .. rounds do
                        for index in 0 .. runs.Length - 1 do
                            let fresh = runOf lines[index]
                            let started = Stopwatch.GetTimestamp()
                            Execute.run shapePlan font ImmutableArray<Feature>.Empty fresh |> ignore
                            let spent = Stopwatch.GetTimestamp() - started
                            if round > 0 && spent < bestFresh[index] then bestFresh[index] <- spent
                    let freshNs = toNs (Array.sum bestFresh)
                    let bestPlanLookup = Array.create runs.Length Int64.MaxValue
                    for round in 0 .. rounds do
                        for index in 0 .. runs.Length - 1 do
                            let started = Stopwatch.GetTimestamp()
                            let struct (found, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone runs[index]
                            let spent = Stopwatch.GetTimestamp() - started
                            cache <- next
                            if round > 0 && spent < bestPlanLookup[index] then bestPlanLookup[index] <- spent
                            found |> ignore
                    let planNs = max 0.0 (toNs (Array.sum bestPlanLookup) - lineCount * swCost)
                    let decodeNs = max 0.0 (toNs (Array.sum bestDecode) - lineCount * swCost)
                    let guessNs = max 0.0 (toNs (Array.sum bestGuess) - lineCount * swCost)
                    let hbFromText = ValueOption.get (hbTimes fontBytes row.Text lines rounds 5)
                    let struct (hbFace, hbFile) = openHarfBuzz row.FontPath
                    use _hbFace = hbFace
                    use _hbFont = hbFile
                    use hbBuffer = new HarfBuzzSharp.Buffer()
                    let struct (_, kept) = shapeFarfbuzz font row.Text lines PlanCache.empty
                    let mutable passCache = kept
                    let mutable hbPass = Double.MaxValue
                    let mutable ffPass = Double.MaxValue
                    let mutable pauseTotal = TimeSpan.Zero
                    let mutable passTotal = 0.0
                    for round in 0 .. rounds do
                        let struct (hb, _) = timed (fun () -> shapeHarfBuzz hbFile hbBuffer row.Text lines)
                        let pauseBefore = GC.GetTotalPauseDuration()
                        let struct (ff, _) =
                            timed (fun () ->
                                let struct (total, next) = shapeFarfbuzz font row.Text lines passCache
                                passCache <- next
                                total)
                        if round > 0 then
                            hbPass <- min hbPass hb
                            ffPass <- min ffPass ff
                            pauseTotal <- pauseTotal + (GC.GetTotalPauseDuration() - pauseBefore)
                            passTotal <- passTotal + ff
                    let hbTotal = Array.sum hbValues
                    let ffTotal = Array.sum ffValues
                    let gap = ffTotal - hbTotal
                    Console.WriteLine $"### {row.Name}: {glyphs:N0} glyphs over {lines.Length} lines; HarfBuzz direct replica differs from hb_shape_plan_execute on {direct[3]} lines"
                    Console.WriteLine "| stage | HarfBuzz ns/glyph (direct) | Farfbuzz ns/glyph (direct) | difference | share of the gap | ratio |"
                    Console.WriteLine "|---|---:|---:|---:|---:|---:|"
                    for index in 0 .. comparedStages.Length - 1 do
                        let name, _ = comparedStages[index]
                        let share = if abs gap < 1.0 then 0.0 else 100.0 * (ffValues[index] - hbValues[index]) / gap
                        let ratio = if hbValues[index] < 0.5 then nan else ffValues[index] / hbValues[index]
                        Console.WriteLine $"| {name} | {hbValues[index] / glyphs:N1} | {ffValues[index] / glyphs:N1} | {(ffValues[index] - hbValues[index]) / glyphs:N1} | {share:N0} %% | {ratio:N2} |"
                    Console.WriteLine $"| sum of the stages | {hbTotal / glyphs:N1} | {ffTotal / glyphs:N1} | {gap / glyphs:N1} | 100 %% | {ffTotal / hbTotal:N2} |"
                    Console.WriteLine $"| whole shape, untimed inside (HarfBuzz hb_shape_plan_execute; Farfbuzz Execute.run) | {untraced[4] / glyphs:N1} | {entry / glyphs:N1} | {(entry - untraced[4]) / glyphs:N1} | | {entry / untraced[4]:N2} |"
                    let perMessage = (hbMessages[4] - untraced[4]) / hbMessages[2]
                    Console.WriteLine $"| from the text, per line (clear, fill, guess, plan, shape; Farfbuzz addUtf8, guess, Shape.shapeCached) | {hbFromText[4] / glyphs:N1} | {fromText / glyphs:N1} | {(fromText - hbFromText[4]) / glyphs:N1} | | {fromText / hbFromText[4]:N2} |"
                    Console.WriteLine $"| whole pass over the text, the minimum of {rounds} (the row of --complex-shape) | {hbPass * 1e6 / glyphs:N1} | {ffPass * 1e6 / glyphs:N1} | {(ffPass - hbPass) * 1e6 / glyphs:N1} | | {ffPass / hbPass:N2} |"
                    let hbLookups = max 0.0 (direct[5 + 2 * 23] - direct[6 + 2 * 23] * hbPerMark)
                    let hbPauses = max 0.0 (direct[5 + 2 * 24] - direct[6 + 2 * 24] * hbPerMark)
                    Console.WriteLine $"| GSUB inside the row above: the lookups (windows) | {hbLookups / glyphs:N1} | {ffWindows / glyphs:N1} | {(ffWindows - hbLookups) / glyphs:N1} | | {ffWindows / hbLookups:N2} |"
                    Console.WriteLine $"| GSUB inside the row above: the pause functions (setup of syllables, reordering, digest refresh) | {hbPauses / glyphs:N1} | {ffPauses / glyphs:N1} | {(ffPauses - hbPauses) / glyphs:N1} | | {ffPauses / hbPauses:N2} |"
                    Console.WriteLine $"| Execute.run on a run decoded just before (decode not timed) | | {freshNs / glyphs:N1} | | | |"
                    Console.WriteLine $"| outside the stages: decode the text into a run (addUtf8) | | {decodeNs / glyphs:N1} | | | |"
                    Console.WriteLine $"| outside the stages: guess segment properties | | {guessNs / glyphs:N1} | | | |"
                    Console.WriteLine $"| outside the stages: the plan from the cache (key and lookup) | | {planNs / glyphs:N1} | | | |"
                    Console.WriteLine $"| outside the stages: the rest of shapeCached (text copy, verify, publication; from the text less the three above less the whole entry) | | {(fromText - decodeNs - guessNs - planNs - entry) / glyphs:N1} | | | |"
                    Console.WriteLine $"Farfbuzz pass: GC pause {pauseTotal.TotalMilliseconds / passTotal * 100.0:N1} %% of the time of the passes"
                    Console.WriteLine $"HarfBuzz trace: untraced {untraced[4] / glyphs:N1} ns/glyph, with a callback that returns {noop[4] / glyphs:N1}, with a timestamp {stamp[4] / glyphs:N1}, with the recording {hbMessages[4] / glyphs:N1}; {hbMessages[2] / lineCount:N1} messages a line; {perMessage:N1} ns a message; timer {hbTimer:N1} ns"
                    Console.WriteLine "| HarfBuzz message stage (derived) | ns/glyph raw | intervals a line | ns/glyph less the trace's cost |"
                    Console.WriteLine "|---|---:|---:|---:|"
                    for index in 0 .. 18 do
                        let name = Runtime.InteropServices.Marshal.PtrToStringAnsi (Farfbuzz.Oracle.Shims.farfbuzz_oracle_stage_name (1u, uint32 index))
                        let raw = hbMessages[5 + 2 * index]
                        let intervals = hbMessages[6 + 2 * index]
                        Console.WriteLine $"| {name} | {raw / glyphs:N1} | {intervals / lineCount:N1} | {max 0.0 (raw - intervals * perMessage) / glyphs:N1} |"
                    let pauseNames = plan.Map.GsubStages |> Seq.filter (fun stage -> PauseKind.runsFunction stage.Pause) |> Seq.map (fun stage -> $"{stage.Pause}") |> Seq.toArray
                    for slot in 0 .. min (pauseNames.Length - 1) (PauseSlots - 1) do
                        Console.WriteLine $"| Farfbuzz pause {slot}: {pauseNames[slot]} (before the per-stamp correction) | | {ffRaw (PauseBase + slot) / glyphs:N1} | | | |"
                    Console.WriteLine $"Farfbuzz GSUB: lookup windows {ffRaw 6 / glyphs:N1} ns/glyph, pauses {ffRaw 10 / glyphs:N1} ns/glyph (before the per-stamp correction of {ffPerMark:N1} ns; HarfBuzz per-stamp correction {hbPerMark:N1} ns)"
                    Console.Out.Flush()

/// <summary>Times only Farfbuzz's GSUB lookup windows of a row, the figure of the row "GSUB inside the row above: the lookups (windows)" of
/// <see cref="compare"/> without HarfBuzz's side: each line keeps its fastest of <c>COMPLEX_ROUNDS</c> rounds (default 25), less the cost of the
/// timestamp pair around each window; <c>COMPLEX_ONLY</c> selects the rows.</summary>
let gsubOnly () =
    let rounds = match Environment.GetEnvironmentVariable "COMPLEX_ROUNDS" with | null | "" -> 25 | text -> int text
    let only = match Environment.GetEnvironmentVariable "COMPLEX_ONLY" with | null -> String.Empty | text -> text
    let swCost = stopwatchCost ()
    Console.WriteLine $"load average at the start: {loadAverage ()}; rounds: {rounds}; .NET timestamp pair: {swCost:N1} ns"
    for row in rows () do
        if row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let runOf (struct (start, length)) = GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty)
            let first = runOf lines[0]
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> Console.WriteLine $"### {row.Name}: no plan"
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> Console.WriteLine $"### {row.Name}: no OpenType plan"
                | ValueSome plan ->
                    let runs = lines |> Array.map (fun line -> runOf line |> TailCalls.settle)
                    let stageCount = PauseBase + PauseSlots
                    let scratch = Array.zeroCreate<int64> stageCount
                    let bestWindows = Array.create runs.Length Int64.MaxValue
                    let bestPauses = Array.create runs.Length Int64.MaxValue
                    let bestPositions = Array.create runs.Length Int64.MaxValue
                    for round in 0 .. rounds do
                        for index in 0 .. runs.Length - 1 do
                            ownedLine shapePlan plan font runs[index] true scratch |> ignore
                            if round > 0 then
                                bestWindows[index] <- min bestWindows[index] scratch[6]
                                bestPauses[index] <- min bestPauses[index] scratch[10]
                                bestPositions[index] <- min bestPositions[index] scratch[8]
                    let windowCalls = float (max 1 shapePlan.GsubWindows.Length) * float runs.Length
                    let glyphs = float (runs |> Array.sumBy (fun run -> run.Infos.Length))
                    let toNs (value: int64) = float value * 1e9 / float Stopwatch.Frequency
                    let windows = max 0.0 (toNs (Array.sum bestWindows) - windowCalls * swCost)
                    let pauses = toNs (Array.sum bestPauses)
                    let positions = max 0.0 (toNs (Array.sum bestPositions) - float runs.Length * swCost)
                    Console.WriteLine $"### {row.Name}: {glyphs:N0} glyphs; GSUB lookup windows {windows / glyphs:N1} ns/glyph; pause functions (stamps included) {pauses / glyphs:N1} ns/glyph; GPOS stage (position start, zeroing, the table) {positions / glyphs:N1} ns/glyph"
                    Console.Out.Flush()

/// <summary>Applies one GSUB window of a plan, or all its lookups when the plan has no windows.</summary>
/// <param name="shapePlan">The shape plan.</param>
/// <param name="plan">The ot plan.</param>
/// <param name="font">The font.</param>
/// <param name="index">The window index.</param>
/// <param name="set">The working set.</param>
/// <returns>The set after the window.</returns>
let private applyWindow (shapePlan: ShapePlan) (plan: OtShapePlan) (font: Font) (index: int) (set: WorkingSet) =
    match shapePlan.GsubWindows.IsEmpty with
    | true -> Engine.substituteApplicableOwnedSet plan font shapePlan.GsubLookupCovered shapePlan.GsubGlyphLookups set
    | false -> OtLayout.substituteWindowOwnedSet shapePlan.GsubLookupCovered font shapePlan.GsubWindows[index] set

/// <summary>Records the working set each GSUB window of a line is given, taken on the route <see cref="ownedLine"/> follows.</summary>
/// <param name="shapePlan">The shape plan.</param>
/// <param name="plan">The ot plan.</param>
/// <param name="font">The font.</param>
/// <param name="run">The run.</param>
/// <returns>The window index and a copy of the working set of every window of the line.</returns>
let private windowInputs (shapePlan: ShapePlan) (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
    let stageList = plan.Map.GsubStages
    let propped = OtProps.setUnicodePropsEnteredSet plan.Map.GlobalMask run
    let circled = GeneralLine.insertDottedCircleSet font propped
    let clustered = GeneralLine.formClustersSet circled
    let native = GeneralLine.ensureNativeDirectionSet clustered
    let prepared = ShaperHooks.preprocessSet plan font native
    let rotated = GeneralLine.rotateCharsSet plan font prepared
    let normalised = Normalize.runOwnedSet plan font rotated
    let mutable current = GeneralLine.glyphsOfSet plan font normalised
    let recorded = Collections.Generic.List<struct (int * WorkingSet)>()
    let copy (set: WorkingSet) =
        { set with
            Infos = ImmutableArray.Create<GlyphInfo>(set.Infos.AsSpan())
            Props = ImmutableArray.Create<GlyphProps>(set.Props.AsSpan()) }
    let mutable windowIndex = 0
    for index in 0 .. stageList.Length - 1 do
        if PauseKind.runsFunction stageList[index].Pause then
            recorded.Add(struct (windowIndex, copy current))
            let applied = applyWindow shapePlan plan font windowIndex current
            current <- ShaperHooks.applyPauseSet stageList[index].Pause plan font applied
            windowIndex <- windowIndex + 1
    recorded.Add(struct (windowIndex, copy current))
    recorded.ToArray()

/// <summary>Replays the GSUB windows of a row on the sets they were given, in a warm loop with no timestamp inside: ns per glyph of the
/// lines and ns per window, the minimum of <c>COMPLEX_ROUNDS</c> passes (default 40), the arrays copied outside the timer. With
/// <c>COMPLEX_LOOP</c> set to a number of seconds it loops for a sampling profile. <c>COMPLEX_ONLY</c> selects the rows.</summary>
let gsubReplay () =
    let rounds = match Environment.GetEnvironmentVariable "COMPLEX_ROUNDS" with | null | "" -> 40 | text -> int text
    let only = match Environment.GetEnvironmentVariable "COMPLEX_ONLY" with | null -> String.Empty | text -> text
    let loopSeconds = match Environment.GetEnvironmentVariable "COMPLEX_LOOP" with | null | "" -> 0 | text -> int text
    Console.WriteLine $"load average at the start: {loadAverage ()}; passes: {rounds}"
    for row in rows () do
        if row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let runOf (struct (start, length)) = GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty)
            let first = runOf lines[0]
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> Console.WriteLine $"### {row.Name}: no plan"
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> Console.WriteLine $"### {row.Name}: no OpenType plan"
                | ValueSome plan ->
                    let inputs = lines |> Array.collect (fun line -> windowInputs shapePlan plan font (runOf line |> TailCalls.settle))
                    let glyphs = float (inputs |> Array.sumBy (fun (struct (index, set)) -> match index with | 0 -> set.Infos.Length | _ -> 0))
                    let copy (set: WorkingSet) =
                        { set with
                            Infos = ImmutableArray.Create<GlyphInfo>(set.Infos.AsSpan())
                            Props = ImmutableArray.Create<GlyphProps>(set.Props.AsSpan()) }
                    let copies = inputs |> Array.map (fun (struct (index, set)) -> struct (index, copy set))
                    let mutable best = Int64.MaxValue
                    let started = Stopwatch.GetTimestamp()
                    let mutable pass = 0
                    while (loopSeconds = 0 && pass <= rounds) || (loopSeconds > 0 && Stopwatch.GetElapsedTime(started).TotalSeconds < float loopSeconds) do
                        for position in 0 .. inputs.Length - 1 do
                            let struct (_, source) = inputs[position]
                            let struct (_, target) = copies[position]
                            source.Infos.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(target.Infos).AsSpan())
                            source.Props.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(target.Props).AsSpan())
                        let t0 = Stopwatch.GetTimestamp()
                        let mutable out = 0
                        for struct (index, set) in copies do
                            out <- out + (applyWindow shapePlan plan font index set).Infos.Length
                        let t1 = Stopwatch.GetTimestamp()
                        if pass > 0 then best <- min best (t1 - t0)
                        pass <- pass + 1
                    let thrash = Array.zeroCreate<int64> (1 <<< 20)
                    let mutable sink = 0L
                    let coldWindow = Array.create inputs.Length Int64.MaxValue
                    if Environment.GetEnvironmentVariable "COMPLEX_COLD" = "1" then
                        for _ in 0 .. 5 do
                            for position in 0 .. inputs.Length - 1 do
                                let struct (_, source) = inputs[position]
                                let struct (index, target) = copies[position]
                                source.Infos.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(target.Infos).AsSpan())
                                source.Props.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(target.Props).AsSpan())
                                for line in 0 .. 512 .. thrash.Length - 1 do sink <- sink + thrash[line]
                                let t0 = Stopwatch.GetTimestamp()
                                applyWindow shapePlan plan font index target |> ignore
                                coldWindow[position] <- min coldWindow[position] (Stopwatch.GetTimestamp() - t0)
                        Console.WriteLine $"    cold (8 MB read between windows, stamp pair subtracted): {(float (Array.sum coldWindow) * 1e9 / float Stopwatch.Frequency / float inputs.Length) - stopwatchCost ():N1} ns a window{(match sink with | 0L -> String.Empty | _ -> String.Empty)}"
                    let perWindow = Array.create inputs.Length Int64.MaxValue
                    for _ in 0 .. 20 do
                        for position in 0 .. inputs.Length - 1 do
                            let struct (_, source) = inputs[position]
                            let struct (index, target) = copies[position]
                            source.Infos.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(target.Infos).AsSpan())
                            source.Props.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(target.Props).AsSpan())
                            let t0 = Stopwatch.GetTimestamp()
                            applyWindow shapePlan plan font index target |> ignore
                            perWindow[position] <- min perWindow[position] (Stopwatch.GetTimestamp() - t0)
                    let stamp = stopwatchCost ()
                    let windowCount = max 1 shapePlan.GsubWindows.Length
                    for slot in 0 .. windowCount - 1 do
                        let mutable total = 0L
                        let mutable count = 0
                        let mutable length = 0
                        for position in 0 .. inputs.Length - 1 do
                            let struct (index, set) = inputs[position]
                            if index = slot then
                                total <- total + perWindow[position]
                                count <- count + 1
                                length <- length + set.Infos.Length
                        Console.WriteLine $"    window {slot}: {float total * 1e9 / float Stopwatch.Frequency / float count - stamp:N1} ns a window, {float length / float count:N2} glyphs in"
                    let ns = float best * 1e9 / float Stopwatch.Frequency
                    Console.WriteLine $"### {row.Name}: {inputs.Length} windows over {lines.Length} lines, {glyphs / float lines.Length:N2} glyphs a line in; replay {ns / float inputs.Length:N1} ns a window, {ns / glyphs:N1} ns per glyph in"
                    Console.Out.Flush()

/// <summary>Times each lookup of a row's GSUB windows alone, on the lines of the sets the windows were given: every lookup of a window is
/// applied in order, each as a window of its own, so the output of one is the input of the next as in the window. ns a line over all the
/// lines of the row's text, less the cost of a window that visits nothing, and the lines on which the lookup changed the glyphs.
/// <c>COMPLEX_ONLY</c> selects the rows, <c>COMPLEX_ROUNDS</c> the passes (default 15).</summary>
let gsubByLookup () =
    let rounds = match Environment.GetEnvironmentVariable "COMPLEX_ROUNDS" with | null | "" -> 15 | text -> int text
    let only = match Environment.GetEnvironmentVariable "COMPLEX_ONLY" with | null -> String.Empty | text -> text
    for row in rows () do
        if row.Name.Contains only then
            let lines = Corpus.lines row.Text
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes row.FontPath)) 0u)
            let runOf (struct (start, length)) = GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan(row.Text, start, row.Text.Length - start)) 0 length GlyphRun.empty)
            let first = runOf lines[0]
            match Execute.createPlan font first.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> Console.WriteLine $"### {row.Name}: no plan"
            | Ok shapePlan ->
                match shapePlan.Ot with
                | ValueNone -> Console.WriteLine $"### {row.Name}: no OpenType plan"
                | ValueSome plan ->
                    let inputs = lines |> Array.collect (fun line -> windowInputs shapePlan plan font (runOf line |> TailCalls.settle))
                    let stages = plan.Map.GsubStages
                    let lookups = plan.Map.GsubLookups
                    let table = font.Ot.Gsub.Value
                    let bounds =
                        match shapePlan.GsubWindows.IsEmpty with
                        | true -> [| struct (0, lookups.Length) |]
                        | false ->
                            let ends = [| for stage in stages do if PauseKind.runsFunction stage.Pause then yield stage.LastLookup |]
                            let all = Array.append ends [| stages[stages.Length - 1].LastLookup |]
                            all |> Array.mapi (fun index last -> struct ((match index with | 0 -> 0 | _ -> all[index - 1]), last))
                    let single (position: int) =
                        { WindowLookups = ImmutableArray.Create lookups[position]
                          WindowStages = ImmutableArray.Create { LastLookup = 1; Pause = NoPause }
                          WindowGlyphLookups = Sealed.glyphLookupTableOfLazySets font.Ot.GsubLookupSets.Value (ReadOnlySpan [| lookups[position].LookupIndex |])
                          WindowViews = Memo<obj>() }
                    let singles = Array.init lookups.Length single
                    let copy (set: WorkingSet) =
                        { set with
                            Infos = ImmutableArray.Create<GlyphInfo>(set.Infos.AsSpan())
                            Props = ImmutableArray.Create<GlyphProps>(set.Props.AsSpan()) }
                    let stamp = stopwatchCost ()
                    let glyphs = float (inputs |> Array.sumBy (fun (struct (index, set)) -> match index with | 0 -> set.Infos.Length | _ -> 0))
                    Console.WriteLine $"### {row.Name}: lookups in ns a glyph of the line's first window (stamp pair {stamp:N1} ns subtracted); window, map position, table index, feature, lines changed, ns/glyph"
                    let loopMap = match Environment.GetEnvironmentVariable "COMPLEX_MAP" with | null | "" -> -1 | text -> int text
                    let loopSeconds = match Environment.GetEnvironmentVariable "COMPLEX_LOOP" with | null | "" -> 0 | text -> int text
                    if loopMap >= 0 then
                        let windowIndex = bounds |> Array.findIndex (fun (struct (a, b)) -> loopMap >= a && loopMap < b)
                        let struct (a, _) = bounds[windowIndex]
                        let members = inputs |> Array.filter (fun (struct (index, _)) -> index = windowIndex) |> Array.map (fun (struct (_, set)) -> set)
                        let before =
                            members
                            |> Array.map (fun member' ->
                                let mutable current = copy member'
                                for k in a .. loopMap - 1 do
                                    current <- Engine.applyGsubWindowOwnedSet shapePlan.GsubLookupCovered table singles[k] font current
                                current)
                        let work = before |> Array.map copy
                        let started = Stopwatch.GetTimestamp()
                        let mutable best = Int64.MaxValue
                        let mutable pass = 0
                        while (loopSeconds = 0 && pass <= rounds) || (loopSeconds > 0 && Stopwatch.GetElapsedTime(started).TotalSeconds < float loopSeconds) do
                            for position in 0 .. before.Length - 1 do
                                before[position].Infos.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(work[position].Infos).AsSpan())
                                before[position].Props.AsSpan().CopyTo(Runtime.InteropServices.ImmutableCollectionsMarshal.AsArray(work[position].Props).AsSpan())
                            let t0 = Stopwatch.GetTimestamp()
                            let mutable out = 0
                            for position in 0 .. work.Length - 1 do
                                out <- out + (Engine.applyGsubWindowOwnedSet shapePlan.GsubLookupCovered table singles[loopMap] font work[position]).Infos.Length
                            let t1 = Stopwatch.GetTimestamp()
                            if pass > 0 then best <- min best (t1 - t0)
                            pass <- pass + 1
                        Console.WriteLine $"### {row.Name} map {loopMap}: {float best * 1e9 / float Stopwatch.Frequency / float work.Length:N1} ns a line over {work.Length} lines"
                    else
                      for windowIndex in 0 .. bounds.Length - 1 do
                        let struct (a, b) = bounds[windowIndex]
                        let members = inputs |> Array.filter (fun (struct (index, _)) -> index = windowIndex) |> Array.map (fun (struct (_, set)) -> set)
                        let best = Array2D.create members.Length (b - a) Int64.MaxValue
                        let changed = Array.zeroCreate<int> (b - a)
                        for round in 0 .. rounds do
                            for line in 0 .. members.Length - 1 do
                                let mutable current = copy members[line]
                                for k in 0 .. b - a - 1 do
                                    let before = current.Infos
                                    let snapshot = before.AsSpan().ToArray()
                                    let t0 = Stopwatch.GetTimestamp()
                                    let next = Engine.applyGsubWindowOwnedSet shapePlan.GsubLookupCovered table singles[a + k] font current
                                    let t1 = Stopwatch.GetTimestamp()
                                    if round > 0 then best[line, k] <- min best[line, k] (t1 - t0)
                                    if round = 1 && not (next.Infos.AsSpan().SequenceEqual(ReadOnlySpan snapshot)) then changed[k] <- changed[k] + 1
                                    current <- next
                        let whole = Array.create members.Length Int64.MaxValue
                        for round in 0 .. rounds do
                            for line in 0 .. members.Length - 1 do
                                let target = copy members[line]
                                let t0 = Stopwatch.GetTimestamp()
                                applyWindow shapePlan plan font windowIndex target |> ignore
                                if round > 0 then whole[line] <- min whole[line] (Stopwatch.GetTimestamp() - t0)
                        Console.WriteLine $"  window {windowIndex} as the route applies it: {(float (Array.sum whole) * 1e9 / float Stopwatch.Frequency - float members.Length * stamp) / glyphs:N1} ns/glyph"
                        for k in 0 .. b - a - 1 do
                            let mutable total = 0L
                            for line in 0 .. members.Length - 1 do total <- total + best[line, k]
                            let ns = float total * 1e9 / float Stopwatch.Frequency - float members.Length * stamp
                            Console.WriteLine $"  window {windowIndex} map {a + k} table {lookups[a + k].LookupIndex} {Feature.trimmedTag lookups[a + k].FeatureTag}: {changed[k]} of {members.Length} lines changed, {ns / glyphs:N1} ns/glyph"
                    Console.Out.Flush()
