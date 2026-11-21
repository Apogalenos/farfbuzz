module Farfbuzz.Bench.AatShape

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Text
open System.Buffers.Binary
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Harness
open Farfbuzz.Bench.NativeShaping

/// <summary>One font and text of the real-text AAT benchmark.</summary>
type private Subject =
    { Label: string
      Path: string
      Index: int
      Text: string // a path under the upstream checkout, or empty to draw lines from the font's cmap
      InCorpus: bool }

/// <summary>The subjects: the corpus fonts with morx and kerx, and the system fonts of this machine, which are not in
/// the repository.</summary>
let private subjects =
    let inHouse name = Path.Combine("test", "shape", "data", "in-house", "fonts", name)
    [ { Label = "corpus a8b4a746 (ankr, kerx, morx), cmap lines"; Path = inHouse "a8b4a746c2d1a1fd5d5d6108e366e79c0ae74ef5.ttf"; Index = 0; Text = String.Empty; InCorpus = true }
      { Label = "corpus TestAATMorx (morx), cmap lines"; Path = Path.Combine("test", "shape", "data", "text-rendering-tests", "fonts", "TestAATMorx.ttf"); Index = 0; Text = String.Empty; InCorpus = true }
      { Label = "system Devanagari Sangam MN (GSUB, GPOS, ankr, kerx, morx), hi-words"; Path = "/System/Library/Fonts/Supplemental/Devanagari Sangam MN.ttc"; Index = 0; Text = Path.Combine("perf", "texts", "hi-words.txt"); InCorpus = false }
      { Label = "system Helvetica (kern, morx), en-thelittleprince"; Path = "/System/Library/Fonts/Helvetica.ttc"; Index = 0; Text = Path.Combine("perf", "texts", "en-thelittleprince.txt"); InCorpus = false }
      { Label = "system Hoefler Text (kern, morx), en-thelittleprince"; Path = "/System/Library/Fonts/Supplemental/Hoefler Text.ttc"; Index = 0; Text = Path.Combine("perf", "texts", "en-thelittleprince.txt"); InCorpus = false }
      { Label = "system Zapfino (kern, morx), en-thelittleprince"; Path = "/System/Library/Fonts/Supplemental/Zapfino.ttf"; Index = 0; Text = Path.Combine("perf", "texts", "en-thelittleprince.txt"); InCorpus = false } ]

/// <summary>Lines drawn from the characters a font maps, 60 characters each, with a fixed seed.</summary>
/// <param name="font">The font.</param>
/// <returns>The UTF-8 text.</returns>
let private cmapText (font: Font) =
    let rnd = Random 7
    let mapped =
        [| 0x20u .. 0xFFFFu |]
        |> Array.filter (fun codepoint -> match Font.nominalGlyph font codepoint with | ValueSome _ -> true | ValueNone -> false)
    match mapped.Length with
    | 0 -> Encoding.UTF8.GetBytes "\n"
    | _ ->
        let line () = String(Array.init 60 (fun _ -> char mapped[rnd.Next mapped.Length]))
        Encoding.UTF8.GetBytes(String.Join("\n", Array.init 200 (fun _ -> line ())) + "\n")

/// <summary>Shapes every line of a text with Farfbuzz through a plan cache kept from one pass to the next.</summary>
/// <param name="cache">The plan cache from the previous pass.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The total glyph count and the cache to keep.</returns>
let private shapeFarfbuzz (cache: PlanCache) (font: Font) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    let mutable kept = cache
    for struct (start, length) in lines do
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (shaped, next) = Shape.shapeCached kept font ImmutableArray<Feature>.Empty ValueNone run
        kept <- next
        match shaped with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error _ -> ()
    struct (glyphs, kept)

/// <summary>Shapes every line of a text with native HarfBuzz, reusing one buffer.</summary>
/// <param name="font">The font.</param>
/// <param name="buffer">The buffer.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The total glyph count.</returns>
let private shapeHarfBuzz (font: HarfBuzzSharp.Font) (buffer: HarfBuzzSharp.Buffer) (text: byte array) (lines: struct (int * int) array) =
    let mutable glyphs = 0
    for struct (start, length) in lines do
        buffer.ClearContents()
        buffer.AddUtf8(ReadOnlySpan(text, start, text.Length - start), 0, length)
        buffer.GuessSegmentProperties()
        font.Shape(buffer)
        glyphs <- glyphs + buffer.Length
    glyphs

/// <summary>Times pieces of work in rounds, each round one pass of each, and keeps the minimum of each.</summary>
/// <param name="rounds">The number of rounds.</param>
/// <param name="works">The work, each returning the glyph count.</param>
/// <returns>The minimum milliseconds of each.</returns>
let private interleaved (rounds: int) (works: (unit -> int) array) =
    let best = Array.create works.Length Double.MaxValue
    for round in 0 .. rounds do
        for index in 0 .. works.Length - 1 do
            let started = Stopwatch.GetTimestamp()
            works[index] () |> ignore
            let elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds
            if round > 0 then best[index] <- min best[index] elapsed
    best

/// <summary>Prints the real-text AAT table: Farfbuzz against native HarfBuzz on lines of real fonts.</summary>
let run () =
    NativeShaping.oracle.Force() |> ignore
    let upstream = RepositoryPaths.upstream ()
    Console.WriteLine "| font and text | glyphs/pass | Farfbuzz ms/pass | HarfBuzz ms/pass | Farfbuzz ns/glyph | HarfBuzz ns/glyph | Farfbuzz / HarfBuzz | Farfbuzz B/glyph |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|"
    let only = match Environment.GetEnvironmentVariable "AATSHAPE_ONLY" with | null -> String.Empty | text -> text
    for subject in subjects |> List.filter (fun subject -> only.Length = 0 || subject.Label.Contains only) do
        let path = match subject.InCorpus with | true -> Path.Combine(upstream, subject.Path) | false -> subject.Path
        match File.Exists path with
        | false -> Console.WriteLine($"| {subject.Label} | not on this machine | | | | | | |")
        | true ->
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) (uint32 subject.Index))
            let text =
                match subject.Text.Length with
                | 0 -> cmapText font
                | _ -> File.ReadAllBytes(Path.Combine(upstream, subject.Text))
            let lines = Corpus.lines text
            use face = new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile path, subject.Index)
            use hbFont = new HarfBuzzSharp.Font(face)
            hbFont.SetFunctionsOpenType()
            use buffer = new HarfBuzzSharp.Buffer()
            let mutable cache = PlanCache.empty
            let glyphs = shapeHarfBuzz hbFont buffer text lines
            let struct (farfbuzzGlyphs, kept) = shapeFarfbuzz cache font text lines
            cache <- kept
            let times =
                interleaved 40 [| (fun () -> shapeHarfBuzz hbFont buffer text lines)
                                  (fun () ->
                                      let struct (total, next) = shapeFarfbuzz cache font text lines
                                      cache <- next
                                      total) |]
            let agree = match farfbuzzGlyphs = glyphs with | true -> String.Empty | false -> $" (Farfbuzz {farfbuzzGlyphs} glyphs)"
            let before = GC.GetAllocatedBytesForCurrentThread()
            shapeFarfbuzz cache font text lines |> ignore
            let bytes = GC.GetAllocatedBytesForCurrentThread() - before
            Console.WriteLine($"| {subject.Label} | {glyphs}{agree} | {times[1]:N3} | {times[0]:N3} | {times[1] * 1.0e6 / float glyphs:N1} | {times[0] * 1.0e6 / float glyphs:N1} | {times[1] / times[0]:N3} | {float bytes / float glyphs:N1} |")

/// <summary>Writes a copy of a font file whose given tables are renamed away, so that neither shaper finds them.</summary>
/// <param name="fontPath">The original path.</param>
/// <param name="tags">The tags to rename.</param>
/// <param name="label">A label for the copy's name.</param>
/// <returns>The copy's path.</returns>
let private stripTables (fontPath: string) (tags: string list) (label: string) =
    let bytes = File.ReadAllBytes fontPath
    let wanted = tags |> List.map (fun text -> uint32 (Tag.ofString text)) |> Set.ofList
    let origin = match bytes[0] = byte 't' && bytes[1] = byte 't' && bytes[2] = byte 'c' with | true -> int (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, 12, 4))) | false -> 0
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, origin + 4, 2)))
    for index in 0 .. count - 1 do
        let record = origin + 12 + index * 16
        if record + 4 <= bytes.Length && wanted.Contains (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4))) then
            bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "no-layout")
    Directory.CreateDirectory directory |> ignore
    let target = Path.Combine(directory, $"aat-shape-{label}-{Path.GetFileName fontPath}#stripped")
    File.WriteAllBytes (target, bytes)
    target

/// <summary>Forces the tables a face compiles at the first use of each morx subtable, the dense state tables and the replacement
/// arrays, and prints what each took and how large they are, the creation moment the caches are named for.</summary>
/// <param name="font">The font.</param>
let private compileCost (font: Font) =
    let table = font.Ot.Morx.Value
    let mutable subtables = 0
    let mutable cells = 0L
    let dense = Stopwatch.StartNew()
    for chain in 0 .. table.Chains.Length - 1 do
        for index in 0 .. table.Chains[chain].Subtables.Length - 1 do
            subtables <- subtables + 1
            table.Chains[chain].Subtables[index].Dense.Value |> ignore
    let denseMs = dense.Elapsed.TotalMilliseconds
    let replacements = Stopwatch.StartNew()
    for chain in 0 .. table.Chains.Length - 1 do
        for index in 0 .. table.Chains[chain].Subtables.Length - 1 do
            match table.Chains[chain].Subtables[index].Replacements.Value with
            | ValueSome compiled -> cells <- cells + int64 compiled.Length
            | ValueNone ->
                match table.Chains[chain].Subtables[index].Kind with
                | Contextual -> Console.WriteLine($"  contextual subtable {index} of chain {chain} has no compiled substitutions (glyphs {font.Ot.Face.GlyphCount}, entries {(match table.Chains[chain].Subtables[index].Machine with | ValueSome machine -> machine.Entries.Length | ValueNone -> -1)})")
                | Noncontextual | Rearrangement | Ligature | Insertion | UnknownKind _ -> ()
    Console.WriteLine($"compiled at first use: {subtables} subtables, dense tables {denseMs:N1} ms, replacement arrays {replacements.Elapsed.TotalMilliseconds:N1} ms ({cells} cells, {cells * 4L} bytes)")

/// <summary>Shapes the lines of one subject with Farfbuzz for a number of seconds, so that a sampling profiler can attach.</summary>
/// <param name="index">The subject's position in the table.</param>
/// <param name="seconds">How long to run.</param>
let loop (index: int) (seconds: int) =
    let subject = subjects[index]
    let upstream = RepositoryPaths.upstream ()
    let path = match subject.InCorpus with | true -> Path.Combine(upstream, subject.Path) | false -> subject.Path
    let path =
        match Environment.GetEnvironmentVariable "AATLOOP_STRIP" with
        | null -> path
        | tags -> stripTables path (List.ofArray (tags.Split ',')) "loop"
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) (uint32 subject.Index))
    let text =
        match subject.Text.Length with
        | 0 -> cmapText font
        | _ -> File.ReadAllBytes(Path.Combine(upstream, subject.Text))
    let lines = Corpus.lines text
    Console.WriteLine($"pid {Environment.ProcessId}")
    compileCost font
    let clock = Stopwatch.StartNew()
    let mutable cache = PlanCache.empty
    let mutable passes = 0
    while clock.Elapsed.TotalSeconds < float seconds do
        let started = Stopwatch.GetTimestamp()
        let struct (_, next) = shapeFarfbuzz cache font text lines
        cache <- next
        passes <- passes + 1
        match passes <= 3 || passes % 100 = 0 with
        | true -> Console.WriteLine($"pass {passes}: {Stopwatch.GetElapsedTime(started).TotalMilliseconds:N1} ms")
        | false -> ()
    Console.WriteLine($"{passes} passes")

/// <summary>Times one font and text with both shapers, in rounds.</summary>
/// <param name="label">The row label.</param>
/// <param name="path">The font path.</param>
/// <param name="index">The face index.</param>
/// <param name="text">The UTF-8 text.</param>
let private row (label: string) (path: string) (index: int) (text: byte array) =
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) (uint32 index))
    let lines = Corpus.lines text
    use face = new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile path, index)
    use hbFont = new HarfBuzzSharp.Font(face)
    hbFont.SetFunctionsOpenType()
    use buffer = new HarfBuzzSharp.Buffer()
    let mutable cache = PlanCache.empty
    let glyphs = shapeHarfBuzz hbFont buffer text lines
    let times =
        interleaved 30 [| (fun () -> shapeHarfBuzz hbFont buffer text lines)
                          (fun () ->
                              let struct (total, next) = shapeFarfbuzz cache font text lines
                              cache <- next
                              total) |]
    Console.WriteLine($"| {label} | {glyphs} | {times[1]:N3} | {times[0]:N3} | {times[1] * 1.0e6 / float glyphs:N1} | {times[0] * 1.0e6 / float glyphs:N1} | {times[1] / times[0]:N3} |")

/// <summary>Attributes the cost of the three system Latin fonts with kern and morx: each font whole, without morx, without kern, and
/// with neither, so that the share of each table is read in the same run against HarfBuzz.</summary>
let attribution () =
    NativeShaping.oracle.Force() |> ignore
    let upstream = RepositoryPaths.upstream ()
    let text = File.ReadAllBytes(Path.Combine(upstream, "perf", "texts", "en-thelittleprince.txt"))
    Console.WriteLine "| variant | glyphs/pass | Farfbuzz ms/pass | HarfBuzz ms/pass | Farfbuzz ns/glyph | HarfBuzz ns/glyph | Farfbuzz / HarfBuzz |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|"
    let fonts =
        [ ("Helvetica", "/System/Library/Fonts/Helvetica.ttc")
          ("Hoefler Text", "/System/Library/Fonts/Supplemental/Hoefler Text.ttc")
          ("Zapfino", "/System/Library/Fonts/Supplemental/Zapfino.ttf") ]
    for (name, path) in fonts do
        row $"{name} whole (kern, morx)" path 0 text
        row $"{name} without morx" (stripTables path [ "morx" ] $"{name}-nomorx") 0 text
        row $"{name} without kern" (stripTables path [ "kern" ] $"{name}-nokern") 0 text
        row $"{name} without kern and morx" (stripTables path [ "kern"; "morx" ] $"{name}-none") 0 text

/// <summary>Names a path of the simple-run kernel.</summary>
/// <param name="path">The path.</param>
/// <returns>Its name.</returns>
let private pathLabel (path: SimplePath) =
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

/// <summary>Times the lines of a text through one execution entry, in microseconds per pass, minimum of a few passes.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="lines">The lines.</param>
/// <param name="execute">The entry.</param>
/// <returns>The minimum microseconds per pass.</returns>
let private timeEntry (font: Font) (text: byte array) (lines: struct (int * int) array) (execute: ShapePlan -> Font -> ImmutableArray<Feature> -> GlyphRun -> Result<GlyphRun, ShapeError>) =
    let mutable cache = PlanCache.empty
    let prepared =
        lines
        |> Array.map (fun (struct (start, length)) ->
            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (planResult, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
            cache <- next
            match planResult with
            | Ok plan -> struct (plan, run)
            | Error _ -> failwith "no plan")
    let pass () =
        let mutable total = 0
        for struct (plan, run) in prepared do
            match execute plan font ImmutableArray<Feature>.Empty run with
            | Ok shaped -> total <- total + shaped.Infos.Length
            | Error _ -> ()
        total
    pass () |> ignore
    pass () |> ignore
    let mutable best = Double.MaxValue
    for _ in 1 .. 12 do
        let started = Stopwatch.GetTimestamp()
        pass () |> ignore
        best <- min best (Stopwatch.GetElapsedTime(started).TotalMicroseconds)
    best

/// <summary>Reports, for each subject and for the Zapfino variants, the path the simple-run kernel takes on each line and the
/// time of the entry and of the general pipeline.</summary>
let paths () =
    let upstream = RepositoryPaths.upstream ()
    let zapfino = "/System/Library/Fonts/Supplemental/Zapfino.ttf"
    let little = Path.Combine(upstream, "perf", "texts", "en-thelittleprince.txt")
    let cases =
        [ ("Zapfino whole", zapfino, little)
          ("Zapfino no kern, no morx", stripTables zapfino [ "kern"; "morx" ] "none", little)
          ("Zapfino no morx", stripTables zapfino [ "morx" ] "nomorx", little)
          ("Helvetica", "/System/Library/Fonts/Helvetica.ttc", little)
          ("Hoefler Text", "/System/Library/Fonts/Supplemental/Hoefler Text.ttc", little) ]
    for (label, path, textPath) in cases do
        let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u)
        let text = File.ReadAllBytes textPath
        let lines = Corpus.lines text
        let mutable cache = PlanCache.empty
        let names =
            lines
            |> Array.map (fun (struct (start, length)) ->
                let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
                let run = GlyphRun.guessSegmentProperties Language.cLocale run
                let struct (planResult, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
                cache <- next
                match planResult with
                | Ok plan ->
                    match SimpleRun.tryShapePath plan font run with
                    | ValueSome (struct (path, _)) -> pathLabel path
                    | ValueNone -> "general pipeline"
                | Error _ -> "no plan")
            |> Array.countBy id
        let differing = lines |> Array.filter (fun (struct (start, length)) ->
            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (planResult, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
            cache <- next
            match planResult with
            | Ok plan ->
                match Execute.run plan font ImmutableArray<Feature>.Empty run, Execute.runGeneral plan font ImmutableArray<Feature>.Empty run with
                | Ok a, Ok b ->
                    not (a.Infos.AsSpan().SequenceEqual(b.Infos.AsSpan()) && a.Props.AsSpan().SequenceEqual(b.Props.AsSpan()) && a.Positions.AsSpan().SequenceEqual(b.Positions.AsSpan())
                         && a.MaxOps = b.MaxOps && a.Successful = b.Successful && a.ScratchFlags = b.ScratchFlags && a.Serial = b.Serial && a.HavePositions = b.HavePositions)
                | _ -> true
            | Error _ -> true)
        Console.WriteLine($"{label}: lines whose entry result differs from the general pipeline: {differing.Length}")
        match Array.tryHead differing with
        | None -> ()
        | Some (struct (start, length)) ->
            let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            let run = GlyphRun.guessSegmentProperties Language.cLocale run
            let struct (planResult, _) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
            match planResult with
            | Ok plan ->
                match Execute.run plan font ImmutableArray<Feature>.Empty run, Execute.runGeneral plan font ImmutableArray<Feature>.Empty run with
                | Ok a, Ok b ->
                    Console.WriteLine($"  first differing line, fields equal: infos {a.Infos.AsSpan().SequenceEqual(b.Infos.AsSpan())} props {a.Props.AsSpan().SequenceEqual(b.Props.AsSpan())} positions {a.Positions.AsSpan().SequenceEqual(b.Positions.AsSpan())} maxOps {a.MaxOps} {b.MaxOps} successful {a.Successful} {b.Successful} scratch {a.ScratchFlags} {b.ScratchFlags} serial {a.Serial} {b.Serial} havePositions {a.HavePositions} {b.HavePositions} lengths {a.Infos.Length} {b.Infos.Length}")
                | _ -> Console.WriteLine "  first differing line: an error"
            | Error _ -> ()
        let entryUs = timeEntry font text lines Execute.run
        let generalUs = timeEntry font text lines Execute.runGeneral
        let described = String.Join(", ", names |> Array.map (fun (name, count) -> name + " " + string count))
        Console.WriteLine($"{label}: {lines.Length} lines; paths {described}; Execute.run {entryUs:N0} us per pass, Execute.runGeneral {generalUs:N0} us per pass")

/// <summary>Reports, for each subject whole and with tables removed, the bytes and the nanoseconds of one line through the entry, and
/// the paths the simple-run kernel takes on its lines, so that what a line allocates is read per table.</summary>
let allocation () =
    let upstream = RepositoryPaths.upstream ()
    Console.WriteLine "| subject and variant | lines | glyphs | paths | bytes/line | bytes/glyph | ns/line |"
    Console.WriteLine "|---|---:|---:|---|---:|---:|---:|"
    let only = match Environment.GetEnvironmentVariable "AATSHAPE_ONLY" with | null -> String.Empty | text -> text
    for subject in subjects |> List.filter (fun subject -> only.Length = 0 || subject.Label.Contains only) do
        let path = match subject.InCorpus with | true -> Path.Combine(upstream, subject.Path) | false -> subject.Path
        match File.Exists path with
        | false -> ()
        | true ->
            let variants =
                [ ("whole", path)
                  ("no morx", stripTables path [ "morx" ] "alloc-nomorx")
                  ("no kerx, kern", stripTables path [ "kerx"; "kern" ] "alloc-nokerx")
                  ("no ankr", stripTables path [ "ankr" ] "alloc-noankr")
                  ("no morx, kerx, kern", stripTables path [ "morx"; "kerx"; "kern" ] "alloc-none") ]
            for (name, variantPath) in variants do
                let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes variantPath)) (uint32 subject.Index))
                let text =
                    match subject.Text.Length with
                    | 0 -> cmapText (Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) (uint32 subject.Index)))
                    | _ -> File.ReadAllBytes(Path.Combine(upstream, subject.Text))
                let lines = Corpus.lines text
                let mutable cache = PlanCache.empty
                let names =
                    lines
                    |> Array.map (fun (struct (start, length)) ->
                        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
                        let run = GlyphRun.guessSegmentProperties Language.cLocale run
                        let struct (planResult, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
                        cache <- next
                        match planResult with
                        | Ok plan ->
                            match SimpleRun.tryShapePath plan font run with
                            | ValueSome (struct (taken, _)) -> pathLabel taken
                            | ValueNone -> "general"
                        | Error _ -> "no plan")
                    |> Array.countBy id
                let struct (glyphs, kept) = shapeFarfbuzz cache font text lines
                cache <- kept
                shapeFarfbuzz cache font text lines |> ignore
                let before = GC.GetAllocatedBytesForCurrentThread()
                shapeFarfbuzz cache font text lines |> ignore
                let bytes = GC.GetAllocatedBytesForCurrentThread() - before
                let mutable best = Double.MaxValue
                for _ in 1 .. 15 do
                    let started = Stopwatch.GetTimestamp()
                    shapeFarfbuzz cache font text lines |> ignore
                    best <- min best (Stopwatch.GetElapsedTime(started).TotalNanoseconds)
                let described = String.Join(", ", names |> Array.map (fun (label, count) -> label + " " + string count))
                let shortLabel = subject.Label.Substring(0, min 28 subject.Label.Length)
                Console.WriteLine($"| {shortLabel} {name} | {lines.Length} | {glyphs} | {described} | {float bytes / float lines.Length:N0} | {float bytes / float glyphs:N1} | {best / float lines.Length:N0} |")

/// <summary>Reports, per subject, the bytes of one line through each AAT stage on its own: the default map, the morx substitution on
/// arrays the stage may write, and the kerx positioning, the lines prepared from the shaped lines of the font without its AAT
/// tables.</summary>
let stages () =
    let upstream = RepositoryPaths.upstream ()
    Console.WriteLine "| subject | lines | glyphs | copy B/line | map B/line | morx B/line | morx ns/line | kerx B/line | kerx ns/line | ankr B/line |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
    let only = match Environment.GetEnvironmentVariable "AATSHAPE_ONLY" with | null -> String.Empty | text -> text
    for subject in subjects |> List.filter (fun subject -> only.Length = 0 || subject.Label.Contains only) do
        let path = match subject.InCorpus with | true -> Path.Combine(upstream, subject.Path) | false -> subject.Path
        match File.Exists path with
        | false -> ()
        | true ->
            let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) (uint32 subject.Index))
            let bare = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes (stripTables path [ "morx"; "kerx"; "kern" ] "stages-none"))) (uint32 subject.Index))
            let text =
                match subject.Text.Length with
                | 0 -> cmapText font
                | _ -> File.ReadAllBytes(Path.Combine(upstream, subject.Text))
            let lines = Corpus.lines text
            let mutable cache = PlanCache.empty
            let mutable bareCache = PlanCache.empty
            let prepared =
                lines
                |> Array.choose (fun (struct (start, length)) ->
                    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
                    let run = GlyphRun.guessSegmentProperties Language.cLocale run
                    let struct (planResult, next) = Shape.planFromCache cache font ImmutableArray<Feature>.Empty ValueNone run
                    cache <- next
                    let struct (shaped, bareNext) = Shape.shapeCached bareCache bare ImmutableArray<Feature>.Empty ValueNone run
                    bareCache <- bareNext
                    match planResult, shaped with
                    | Ok plan, Ok shapedRun ->
                        match plan.Ot with
                        | ValueSome otPlan -> Some (otPlan, shapedRun)
                        | ValueNone -> None
                    | _ -> None)
            let glyphs = prepared |> Array.sumBy (fun (_, run) -> run.Infos.Length)
            let bytesOf (action: unit -> unit) =
                action ()
                action ()
                let before = GC.GetAllocatedBytesForCurrentThread()
                action ()
                GC.GetAllocatedBytesForCurrentThread() - before
            let nanosOf (action: unit -> unit) =
                let mutable best = Double.MaxValue
                for _ in 1 .. 15 do
                    let started = Stopwatch.GetTimestamp()
                    action ()
                    best <- min best (Stopwatch.GetElapsedTime(started).TotalNanoseconds)
                best
            let plain (run: GlyphRun) =
                { run with Positions = ImmutableArray.Empty; HavePositions = false; ScratchFlags = ScratchFlags.Default }
            let copy () =
                for (_, run) in prepared do
                    let plained = plain run
                    ImmutableArray.CreateRange(plained.Infos) |> ignore
                    ImmutableArray.CreateRange(plained.Props) |> ignore
            let copyBytes = bytesOf copy
            let map () =
                for (otPlan, _) in prepared do
                    AatMap.defaults font.Ot otPlan.Props |> ignore
            let morx () =
                for (otPlan, run) in prepared do
                    let plained = { plain run with Infos = ImmutableArray.CreateRange run.Infos; Props = ImmutableArray.CreateRange run.Props }
                    AatLayout.substituteOwned otPlan font (ReadOnlySpan<Feature>()) plained |> ignore
            let kerx () =
                for (otPlan, run) in prepared do
                    AatLayout.position otPlan font run |> ignore
            let ankr () =
                for (_, run) in prepared do
                    for index in 0 .. run.Infos.Length - 1 do
                        Ankr.anchor font.Ot.Ankr.Value run.Infos[index].Codepoint 0u font.Ot.Face.GlyphCount |> ignore
            let n = float prepared.Length
            let morxBytes = bytesOf morx - copyBytes
            let ankrBytes = try bytesOf ankr with _ -> 0L
            Console.WriteLine($"| {subject.Label.Substring(0, min 40 subject.Label.Length)} | {prepared.Length} | {glyphs} | {float copyBytes / n:N0} | {float (bytesOf map) / n:N0} | {float morxBytes / n:N0} | {nanosOf morx / n:N0} | {float (bytesOf kerx) / n:N0} | {nanosOf kerx / n:N0} | {float ankrBytes / n:N0} |")

/// <summary>Collects the type names of the allocation ticks the runtime raises, with the bytes each tick stands for.</summary>
type private TickListener() =
    inherit System.Diagnostics.Tracing.EventListener()
    let totals = System.Collections.Generic.Dictionary<string, int64>()
    member _.Totals = totals
    override _.OnEventSourceCreated(source) =
        match source.Name = "Microsoft-Windows-DotNETRuntime" with
        | true -> base.EnableEvents(source, System.Diagnostics.Tracing.EventLevel.Verbose, LanguagePrimitives.EnumOfValue<int64, System.Diagnostics.Tracing.EventKeywords> 1L)
        | false -> ()
    override _.OnEventWritten(data) =
        match data.EventName with
        | null -> ()
        | name when name.StartsWith "GCAllocationTick" ->
            let names = nonNull data.PayloadNames
            let payload = nonNull data.Payload
            let typeName = string payload[names.IndexOf "TypeName"]
            let amount = System.Convert.ToInt64 payload[names.IndexOf "AllocationAmount64"]
            lock totals (fun () ->
                let existing = match totals.ContainsKey typeName with | true -> totals[typeName] | false -> 0L
                totals[typeName] <- existing + amount)
        | _ -> ()

/// <summary>Prints, for one subject, the types the shaping of its lines allocates, by the bytes the runtime's allocation ticks
/// attribute to each, over many passes.</summary>
/// <param name="index">The subject's position in the table.</param>
let types (index: int) =
    let subject = subjects[index]
    let upstream = RepositoryPaths.upstream ()
    let path = match subject.InCorpus with | true -> Path.Combine(upstream, subject.Path) | false -> subject.Path
    let font = Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) (uint32 subject.Index))
    let text =
        match subject.Text.Length with
        | 0 -> cmapText font
        | _ -> File.ReadAllBytes(Path.Combine(upstream, subject.Text))
    let lines = Corpus.lines text
    let mutable cache = PlanCache.empty
    let struct (_, first) = shapeFarfbuzz cache font text lines
    cache <- first
    use listener = new TickListener()
    let passes = 60
    for _ in 1 .. passes do
        let struct (_, next) = shapeFarfbuzz cache font text lines
        cache <- next
    let total = listener.Totals.Values |> Seq.sum
    Console.WriteLine($"{subject.Label}: {total / int64 passes / int64 lines.Length} bytes per line over {passes} passes, by type")
    for pair in listener.Totals |> Seq.sortByDescending (fun pair -> pair.Value) |> Seq.truncate 14 do
        Console.WriteLine($"  {float pair.Value * 100.0 / float total,5:N1} %%  {pair.Value / int64 passes / int64 lines.Length,7} B/line  {pair.Key}")
