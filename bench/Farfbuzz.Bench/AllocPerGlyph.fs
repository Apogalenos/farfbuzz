module Farfbuzz.Bench.AllocPerGlyph

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Text
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Bench.NativeShaping

/// <summary>The managed bytes the calling thread allocates running a piece of work, once warm.</summary>
/// <param name="work">The work.</param>
/// <returns>The bytes.</returns>
let private bytes (work: unit -> 'a) : int64 =
    work () |> ignore
    let before = GC.GetAllocatedBytesForCurrentThread()
    let result = work ()
    let after = GC.GetAllocatedBytesForCurrentThread()
    GC.KeepAlive result
    after - before

/// <summary>The bytes of an array of a number of elements: the 24-byte header and the payload rounded to eight.</summary>
/// <param name="count">The elements.</param>
/// <param name="size">The element size.</param>
/// <returns>The object size.</returns>
let private arrayBytes (count: int) (size: int) = 24L + ((int64 count * int64 size + 7L) / 8L) * 8L

/// <summary>The name of a kernel path.</summary>
/// <param name="path">The path.</param>
/// <returns>Its name.</returns>
let private nameOfPath (path: SimplePath) =
    match path with
    | LookupFree -> "LookupFree"
    | NoLookupApplies -> "NoLookupApplies"
    | SubstitutionUnchanged -> "SubstitutionUnchanged"
    | SubstitutionFast -> "SubstitutionFast"
    | SubstitutionGeneral -> "SubstitutionGeneral"
    | PositioningGeneral -> "PositioningGeneral"
    | PositioningFast -> "PositioningFast"
    | PositioningTail -> "PositioningTail"
    | AatSimple -> "AatSimple"

/// <summary>A text of the first characters of a pangram, repeated to the length.</summary>
/// <param name="length">The length.</param>
/// <returns>The ASCII text.</returns>
let private textOf (length: int) =
    let source = "the quick brown fox jumps over a lazy dog while seven kernels grow again and again"
    Encoding.ASCII.GetBytes (source.Substring(0, length))

/// <summary>Shapes one line, prints every stage of its allocation.</summary>
/// <param name="label">The row label.</param>
/// <param name="font">The font.</param>
/// <param name="text">The ASCII or UTF-8 text of the line.</param>
let private stages (label: string) (font: Font) (text: byte array) =
    let none = ImmutableArray<Feature>.Empty
    let run = GlyphRun.addUtf8 (ReadOnlySpan text) 0 text.Length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    let glyphs = run.Infos.Length
    let plan =
        match ShapePlan.create font run.SegmentProperties none ValueNone with
        | Ok plan -> plan
        | Error _ -> failwith "no plan"
    let path =
        match SimpleRun.tryShapePath plan font run with
        | ValueSome (struct (path, _)) -> nameOfPath path
        | ValueNone -> "general"
    let build = bytes (fun () -> GlyphRun.addUtf8 (ReadOnlySpan text) 0 text.Length GlyphRun.empty |> TailCalls.settle)
    let infosOnly = bytes (fun () -> Sealed.byteInfos (ReadOnlySpan text) 0 |> TailCalls.settle)
    let propsOnly = bytes (fun () -> Sealed.zeroed<GlyphProps> glyphs)
    let recordCopy = bytes (fun () -> { run with Serial = 1u })
    let guess = bytes (fun () -> GlyphRun.guessSegmentProperties Language.cLocale run)
    let key = bytes (fun () -> ShapePlan.key font run.SegmentProperties (none.AsSpan()) "ot" |> TailCalls.settle)
    let shapeWithPlan = bytes (fun () -> Shape.shapeWithPlan font plan none run)
    let okWrapper = bytes (fun () -> Ok run : Result<GlyphRun, ShapeError>)
    let cache = PlanCache.empty
    let struct (_, warmed) = Shape.shapeCached cache font none ValueNone run
    let cached = bytes (fun () -> Shape.shapeCached warmed font none ValueNone run)
    let arrays = arrayBytes glyphs 20 + arrayBytes glyphs 12 + arrayBytes glyphs 20
    Console.WriteLine $"| {label} | {glyphs} | {path} | {build} | {infosOnly} | {propsOnly} | {recordCopy} | {build - infosOnly - propsOnly - recordCopy} | {guess} | {key} | {shapeWithPlan} | {arrays} | {recordCopy} | {okWrapper} | {shapeWithPlan - arrays - recordCopy - okWrapper} | {cached} | {build + guess + cached} |"

/// <summary>Shapes one marked line through the general pipeline and prints the allocation of the stages that can be told apart.</summary>
/// <param name="label">The row label.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text of the line.</param>
let private generalStages (label: string) (font: Font) (text: byte array) =
    let none = ImmutableArray<Feature>.Empty
    let run = GlyphRun.addUtf8 (ReadOnlySpan text) 0 text.Length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    let plan =
        match ShapePlan.create font run.SegmentProperties none ValueNone with
        | Ok plan -> plan
        | Error _ -> Unchecked.defaultof<ShapePlan>
    match plan.Ot with
    | ValueNone -> Console.WriteLine $"| {label} | no ot plan |"
    | ValueSome otPlan ->
        let before = GeneralLine.beforeNormaliser otPlan font run
        let normalized = Normalize.runOwned otPlan font before
        let glyphs = GeneralLine.glyphsOf otPlan font normalized
        let build = bytes (fun () -> GlyphRun.addUtf8 (ReadOnlySpan text) 0 text.Length GlyphRun.empty |> TailCalls.settle)
        let beforeBytes = bytes (fun () -> GeneralLine.beforeNormaliser otPlan font run)
        let normaliseBytes = bytes (fun () -> Normalize.runOwned otPlan font before)
        let glyphsBytes = bytes (fun () -> GeneralLine.glyphsOf otPlan font normalized)
        let lookupBytes = bytes (fun () -> GeneralLine.lookups plan otPlan font glyphs)
        let total = bytes (fun () -> Shape.shapeWithPlan font plan none run)
        let outGlyphs, held =
            match Shape.shapeWithPlan font plan none run with
            | Ok result -> result.Infos.Length, arrayBytes result.Infos.Length 20 + arrayBytes result.Infos.Length 12 + arrayBytes result.Infos.Length 20 + 144L
            | Error _ -> 0, 0L
        Console.WriteLine $"| {label} | {run.Infos.Length} -> {outGlyphs} | {build} | {beforeBytes} | {normaliseBytes} | {glyphsBytes} | {lookupBytes} | not separable | {total} | {held} |"

/// <summary>The allocation of the document entry on a document of identical lines, per line and per glyph, with what the caller keeps.</summary>
/// <param name="label">The row label.</param>
/// <param name="font">The font.</param>
/// <param name="line">The text of one line.</param>
let private documentRow (label: string) (font: Font) (line: byte array) =
    let count = 200
    let text = Array.concat (Array.init count (fun _ -> Array.append line [| 10uy |]))
    let lines = ImmutableArray.CreateRange (Array.init count (fun index -> struct (index * (line.Length + 1), line.Length)))
    let memory = ReadOnlyMemory text
    let none = ImmutableArray<Feature>.Empty
    let struct (warm, cache) = LineShape.utf8 1 PlanCache.empty font none ValueNone Language.cLocale GlyphRun.empty memory lines
    let glyphs = float warm.ArenaInfos.Length
    let total = bytes (fun () -> let struct (document, _) = LineShape.utf8 1 cache font none ValueNone Language.cLocale GlyphRun.empty memory lines in document.Lines.Length)
    let runs = lines |> Seq.map (fun (struct (start, length)) -> GlyphRun.guessSegmentProperties Language.cLocale (GlyphRun.addUtf8 (ReadOnlySpan(text, start, length)) 0 length GlyphRun.empty)) |> Array.ofSeq
    let old = bytes (fun () ->
        let mutable kept = cache
        let mutable total = 0
        for run in runs do
            let struct (shaped, next) = Shape.shapeCached kept font none ValueNone run
            kept <- next
            total <- total + (match shaped with | Ok shaped -> shaped.Infos.Length | Error _ -> 0)
        total)
    Console.WriteLine $"| {label} | {count} | {line.Length} | {float total / float count:N0} | {float total / glyphs:N1} | {float old / float count:N0} | {float old / glyphs:N1} |"

/// <summary>The document entry on documents of identical lines of 1, 7 and 47 glyphs, and on a marked line.</summary>
/// <param name="font">The layout font.</param>
/// <param name="stripped">The same font without layout.</param>
let private documentRows (font: Font) (stripped: Font) =
    Console.WriteLine ""
    Console.WriteLine "| document of identical lines | lines | glyphs per line | LineShape.utf8, bytes per line | LineShape.utf8, bytes per glyph | shapeCached loop, bytes per line (the runs built beforehand) | shapeCached loop, bytes per glyph |"
    Console.WriteLine "|---|---:|---:|---:|---:|---:|---:|"
    for glyphs in [ 1; 7; 47 ] do
        documentRow $"{glyphs} glyphs, Roboto stripped" stripped (textOf glyphs)
        documentRow $"{glyphs} glyphs, Roboto with layout" font (textOf glyphs)
    documentRow "ligature line of 107 glyphs, Roboto with layout" font (System.Text.Encoding.ASCII.GetBytes "the first official office staff were fine but the flowers fluffed after a shift and the fox found the field")
    documentRow "marked line of 54 glyphs, Roboto with layout" font (Encoding.UTF8.GetBytes "the quick brown fox jumps over a lazy dog and x\u0323 again")

/// <summary>The stage-by-stage allocation of a line of the kernel path and of a marked line of the general path.</summary>
let run () =
    NativeShaping.oracle.Force() |> ignore
    let roboto, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes roboto)) 0u)
    let stripped = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes (LayoutShape.stripFont roboto))) 0u)
    Console.WriteLine $"Sizes: GlyphInfo {Unsafe.SizeOf<GlyphInfo>()} bytes, GlyphProps {Unsafe.SizeOf<GlyphProps>()} bytes, GlyphPosition {Unsafe.SizeOf<GlyphPosition>()} bytes"
    Console.WriteLine ""
    Console.WriteLine "| line | glyphs | path | addUtf8 total, bytes | of it infos array | of it props array | of it run record | of it contexts before and after | guessSegmentProperties (one record), bytes | plan key, bytes | shapeWithPlan total, bytes | of it three arrays | of it finished record | of it Ok wrapper | of it other | shapeCached warm total, bytes | caller total per line, bytes |"
    Console.WriteLine "|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
    for glyphs in [ 1; 7; 47 ] do
        stages $"{glyphs} glyphs, Roboto stripped" stripped (textOf glyphs)
        stages $"{glyphs} glyphs, Roboto with layout" font (textOf glyphs)
    documentRows font stripped
    Console.WriteLine ""
    Console.WriteLine "| line | glyphs in -> out | addUtf8, bytes | beforeNormaliser (enter, cluster forming, rotation), bytes | Normalize.runOwned called alone on shared arrays (an upper bound: in the pipeline it owns them), bytes | glyph properties (glyphsOf), bytes | GSUB lookups, bytes | the rest (GPOS, marks, offsets, flags, result), bytes | shapeWithPlan total, bytes | what the caller keeps: three arrays and the record, bytes |"
    Console.WriteLine "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|"
    for text in [ "x\u0323"; "the quick brown fox jumps over a lazy dog and x\u0323 again" ] do
        generalStages $"marked {text.Length}" font (Encoding.UTF8.GetBytes text)

/// <summary>Which path of the kernel each line of a text takes, counted, and how many glyphs each path shapes.</summary>
/// <param name="label">The row label.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
let private pathsOf (label: string) (font: Font) (text: byte array) =
    let lines = Corpus.lines text
    let counts = Collections.Generic.Dictionary<string, struct (int * int)>()
    let none = ImmutableArray<Feature>.Empty
    let mutable cache = PlanCache.empty
    for struct (start, length) in lines do
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (planned, next) = Shape.planFromCache cache font none ValueNone run
        cache <- next
        match planned with
        | Error _ -> ()
        | Ok plan ->
            let name = match SimpleRun.tryShapePath plan font run with | ValueSome (struct (path, _)) -> nameOfPath path | ValueNone -> "general pipeline"
            let struct (lineCount, glyphs) = match counts.TryGetValue name with | true, found -> found | false, _ -> struct (0, 0)
            counts[name] <- struct (lineCount + 1, glyphs + run.Infos.Length)
    Console.WriteLine $"{label}"
    for pair in counts do
        let struct (lineCount, glyphs) = pair.Value
        Console.WriteLine $"| {pair.Key} | {lineCount} | {glyphs} |"

/// <summary>The paths of the corpus lines.</summary>
let paths () =
    NativeShaping.oracle.Force() |> ignore
    let roboto, prince = Corpus.load "Roboto/en-thelittleprince"
    let _, words = Corpus.load "Roboto/en-words"
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes roboto)) 0u)
    pathsOf "Little Prince, Roboto with layout" font prince
    pathsOf "word list, Roboto with layout" font words
    match Corpus.scriptPairsFetched () with
    | false -> ()
    | true ->
        for pair in Corpus.scriptPairs do
            let path, text = Corpus.load pair.Name
            pathsOf pair.Name (Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes path)) 0u)) text

/// <summary>How many lines and glyphs of a text the substitution admission lets through, which the document entry hands to the
/// general route.</summary>
/// <param name="label">The row label.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
let private admission (label: string) (font: Font) (text: byte array) =
    let lines = Corpus.lines text
    let none = ImmutableArray<Feature>.Empty
    let mutable cache = PlanCache.empty
    let mutable admitted = 0
    let mutable admittedGlyphs = 0
    let mutable total = 0
    for struct (start, length) in lines do
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (planned, next) = Shape.planFromCache cache font none ValueNone run
        cache <- next
        match planned with
        | Error _ -> ()
        | Ok plan ->
            match plan.Ot, SimpleRun.tryShapePath plan font run with
            | ValueSome otPlan, ValueSome _ ->
                total <- total + run.Infos.Length
                let infos = Seq.toArray run.Infos |> Array.map (fun info -> { info with Codepoint = (Cmap.nominalGlyph font.Ot.Cmap.Value info.Codepoint |> ValueOption.defaultValue 0u) })
                let struct (applies, _) = Engine.admit otPlan font plan.GsubGlyphLookups otPlan.Map.GlobalMask run.Flags (ReadOnlySpan infos)
                if applies && not plan.GsubCovered.IsEmpty then
                    admitted <- admitted + 1
                    admittedGlyphs <- admittedGlyphs + run.Infos.Length
            | ValueNone, _ -> ()
            | _, ValueNone -> ()
    Console.WriteLine $"| {label} | {lines.Length} | {total} | {admitted} | {admittedGlyphs} |"

/// <summary>The lines the substitution admission lets through.</summary>
let admissions () =
    NativeShaping.oracle.Force() |> ignore
    let roboto, prince = Corpus.load "Roboto/en-thelittleprince"
    let _, words = Corpus.load "Roboto/en-words"
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes roboto)) 0u)
    Console.WriteLine "| text | lines | glyphs | lines admitted | glyphs of the admitted lines |"
    admission "Little Prince, Roboto with layout" font prince
    admission "word list, Roboto with layout" font words

/// <summary>Where the allocation of the document entry goes by the path the old kernel takes for each line: the lines of one path
/// shaped as a document of their own.</summary>
/// <param name="label">The row label.</param>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
let private perPath (label: string) (font: Font) (text: byte array) =
    let lines = Corpus.lines text
    let none = ImmutableArray<Feature>.Empty
    let mutable cache = PlanCache.empty
    let groups = Collections.Generic.Dictionary<string, ResizeArray<struct (int * int)>>()
    let glyphCounts = Collections.Generic.Dictionary<string, int>()
    for struct (start, length) in lines do
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (planned, next) = Shape.planFromCache cache font none ValueNone run
        cache <- next
        match planned with
        | Error _ -> ()
        | Ok plan ->
            let name = match SimpleRun.tryShapePath plan font run with | ValueSome (struct (path, _)) -> nameOfPath path | ValueNone -> "general pipeline"
            (match groups.TryGetValue name with
             | true, found -> found
             | false, _ ->
                let made = ResizeArray()
                groups[name] <- made
                made).Add (struct (start, length))
            glyphCounts[name] <- (match glyphCounts.TryGetValue name with | true, found -> found | false, _ -> 0) + run.Infos.Length
    Console.WriteLine label
    for pair in groups do
        let single = ImmutableArray.CreateRange pair.Value
        let memory = ReadOnlyMemory text
        let total = bytes (fun () -> let struct (document, _) = LineShape.utf8 1 cache font none ValueNone Language.cLocale GlyphRun.empty memory single in document.Lines.Length)
        let glyphs = glyphCounts[pair.Key]
        Console.WriteLine $"| {pair.Key} | {pair.Value.Count} | {glyphs} | {total} | {float total / float (max glyphs 1):N1} |"

/// <summary>The allocation of the document entry by path.</summary>
let pathAllocation () =
    NativeShaping.oracle.Force() |> ignore
    let roboto, prince = Corpus.load "Roboto/en-thelittleprince"
    let _, words = Corpus.load "Roboto/en-words"
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes roboto)) 0u)
    Console.WriteLine "| path of the old kernel | lines | glyphs | bytes of LineShape.utf8 on those lines alone | per glyph |"
    perPath "Little Prince, Roboto with layout" font prince
    perPath "word list, Roboto with layout" font words

/// <summary>Why a line takes the tail path: whether the lean loop refuses it or the flags cannot be finished in place.</summary>
let tailReasons () =
    NativeShaping.oracle.Force() |> ignore
    let roboto, prince = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes roboto)) 0u)
    let none = ImmutableArray<Feature>.Empty
    let mutable cache = PlanCache.empty
    let mutable leanRefused = 0
    let mutable flagsRefused = 0
    let mutable fine = 0
    let mutable shown = 0
    for struct (start, length) in Corpus.lines prince do
        let run = GlyphRun.addUtf8 (ReadOnlySpan(prince, start, prince.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (planned, next) = Shape.planFromCache cache font none ValueNone run
        cache <- next
        match planned with
        | Error _ -> ()
        | Ok plan ->
            match plan.Ot, SimpleRun.tryShapePath plan font run with
            | ValueSome otPlan, ValueSome (struct (PositioningTail, _)) ->
                let glyphs = SimpleRun.SimpleGlyphs(font, otPlan.FallbackGlyphClasses)
                let metrics = font.Ot.Hmtx.Value.Table.Span
                match Sealed.simpleRun glyphs otPlan.Map.GlobalMask (run.Infos.AsSpan()) metrics plan.GsubCovered with
                | ValueNone -> ()
                | ValueSome arrays ->
                    match Engine.positionOwned plan.GposPairs font arrays.KernelInfos arrays.KernelPositions run.Flags run.ClusterLevel (GlyphRun.enteredMaxOps run) with
                    | ValueNone ->
                        leanRefused <- leanRefused + 1
                        if shown < 3 then
                            shown <- shown + 1
                            Console.WriteLine $"refused: {System.Text.Encoding.UTF8.GetString(prince, start, length)}"
                    | ValueSome _ ->
                        match Sealed.finishFlags arrays.KernelInfos (Flags.andMaskOf run.Flags) with
                        | false -> flagsRefused <- flagsRefused + 1
                        | true -> fine <- fine + 1
            | _ -> ()
    Console.WriteLine $"tail lines: lean loop refused {leanRefused}, flags refused {flagsRefused}, fine {fine}"

/// <summary>The allocation of the stages of a line the substitution engine changes, after the kernel: the engine, then the tails.</summary>
let ligatureStages () =
    NativeShaping.oracle.Force() |> ignore
    let roboto, _ = Corpus.load "Roboto/en-thelittleprince"
    let font = Font.create (Face.create (ReadOnlyMemory(File.ReadAllBytes roboto)) 0u)
    let none = ImmutableArray<Feature>.Empty
    let text = System.Text.Encoding.ASCII.GetBytes "the first official office staff were fine but the flowers fluffed after a shift and the fox found the field"
    let run = GlyphRun.addUtf8 (ReadOnlySpan text) 0 text.Length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    let plan = match ShapePlan.create font run.SegmentProperties none ValueNone with | Ok plan -> plan | Error _ -> Unchecked.defaultof<ShapePlan>
    let otPlan = match plan.Ot with | ValueSome otPlan -> otPlan | ValueNone -> Unchecked.defaultof<OtShapePlan>
    let glyphs = SimpleRun.SimpleGlyphs(font, otPlan.FallbackGlyphClasses)
    let metrics = font.Ot.Hmtx.Value.Table.Span
    match Sealed.simpleRun glyphs otPlan.Map.GlobalMask (run.Infos.AsSpan()) metrics plan.GsubCovered with
    | ValueNone -> Console.WriteLine "refused"
    | ValueSome arrays ->
        let pre = { run with MaxLen = GlyphRun.enteredMaxLen run; MaxOps = GlyphRun.enteredMaxOps run; Serial = 0u; ScratchFlags = ScratchFlags.Default; Infos = arrays.KernelInfos; Props = arrays.KernelProps; Positions = ImmutableArray.Empty; HavePositions = false; ContentType = GlyphContent }
        let struct (_, admission) = Engine.admit otPlan font plan.GsubGlyphLookups otPlan.Map.GlobalMask run.Flags (arrays.KernelInfos.AsSpan())
        let engineBytes = bytes (fun () -> Engine.substituteAdmitted otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups admission pre)
        let substituted = Engine.substituteAdmitted otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups admission pre
        let tails = bytes (fun () -> SimpleRun.substitutedKernelled plan otPlan font run substituted)
        let n = run.Infos.Length
        Console.WriteLine $"glyphs in {n}, out {substituted.Infos.Length}: engine {engineBytes} bytes ({float engineBytes / float n:N1} per glyph), tails {tails} bytes ({float tails / float n:N1} per glyph)"
        let infos = Array.zeroCreate<GlyphInfo> n
        let props = Array.zeroCreate<GlyphProps> n
        let input = { RunInput.Infos = ImmutableArray<GlyphInfo>.Empty; Props = ImmutableArray<GlyphProps>.Empty; Positions = ImmutableArray<GlyphPosition>.Empty; HavePositions = false; MaxLen = pre.MaxLen; MaxOps = pre.MaxOps; Successful = true; ClusterLevel = run.ClusterLevel; Flags = run.Flags; Serial = 0uy; RandomState = run.RandomState; ScratchFlags = ScratchFlags.Default }
        let inPlace () =
            arrays.KernelInfos.AsSpan().CopyTo(Span<GlyphInfo>(infos))
            arrays.KernelProps.AsSpan().CopyTo(Span<GlyphProps>(props))
            Engine.substituteOwned otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups admission input infos props n
        let inPlaceBytes = bytes inPlace
        let owned = inPlace ()
        Console.WriteLine $"engine in the worker's arrays: {inPlaceBytes} bytes ({float inPlaceBytes / float n:N1} per glyph), glyphs out {owned.OwnedLength}, in place {obj.ReferenceEquals(owned.OwnedInfos, infos)}"
