namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The public shaping entry points, <c>hb_shape</c> and <c>hb_shape_full</c>.</summary>
[<RequireQualifiedAccess>]
module Shape =
    /// <summary>Lists the implemented shapers, <c>hb_shape_list_shapers</c>.</summary>
    /// <returns>The shaper names, <c>fallback</c> last.</returns>
    let listShapers () =
        ImmutableArray.Create("ot", "fallback")

    /// <summary>Copies the text when verification is requested, <c>hb_shape_full</c>'s text-buffer branch.</summary>
    /// <param name="run">The input run.</param>
    /// <returns>The text copy, or none when verification is not requested.</returns>
    let internal textCopy (run: GlyphRun) =
        match run.Flags &&& BufferFlags.Verify = BufferFlags.Default with
        | true -> ValueNone
        | false -> ValueSome (GlyphRun.append run 0 run.Infos.Length GlyphRun.empty)

    /// <summary>Verifies the shaped run against its text when both succeeded, <c>hb_shape_full</c>'s verify branch.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="text">The text copy, or none.</param>
    /// <param name="plan">The plan that shaped the run.</param>
    /// <param name="props">The segment properties the plan was built for.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let private verifyOrPass (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (text: GlyphRun voption) (plan: ShapePlan) (props: SegmentProperties) (shaped: GlyphRun) : Result<GlyphRun, ShapeError> =
        match text with
        | ValueNone -> Ok shaped
        | ValueSome textRun ->
            match shaped.Successful && textRun.Successful with
            | false -> Ok shaped
            | true -> Verify.verifyStreaming font features shapers (ValueSome (struct (props, plan))) textRun shaped

    /// <summary>Executes a plan and then verifies, <c>hb_shape_full</c>'s tail.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="text">The text copy, or none.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let internal executeAndVerify (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (text: GlyphRun voption) (plan: ShapePlan) (run: GlyphRun) =
        match Execute.run plan font features run with
        | Error error -> Error error
        | Ok shaped -> verifyOrPass font features shapers text plan run.SegmentProperties shaped

    /// <summary>Shapes a run with an explicit shaper list, <c>hb_shape_full</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let shapeFull font features shapers run =
        match run.Infos.Length with
        | 0 -> Ok run
        | _ ->
            match Execute.createPlan font run.SegmentProperties features shapers with
            | Error error -> Error error
            | Ok plan -> executeAndVerify font features shapers (textCopy run) plan run

    /// <summary>Shapes a run with the default shaper list, <c>hb_shape</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let shape font features run =
        shapeFull font features ValueNone run

    /// <summary>Returns the plan cached under a key, creating and caching it on a miss.</summary>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="run">The run, whose segment properties build the plan.</param>
    /// <param name="key">The plan key.</param>
    /// <returns>The plan and the updated cache.</returns>
    let private cachedPlan (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (run: GlyphRun) (key: ShapePlanKey) : struct (Result<ShapePlan, ShapeError> * PlanCache) =
        match PlanCache.tryFind cache key with
        | ValueSome plan -> struct (Ok plan, cache)
        | ValueNone ->
            match ShapePlan.create font run.SegmentProperties features shapers with
            | Error error -> struct (Error error, cache)
            | Ok plan -> struct (Ok plan, PlanCache.add cache key plan)

    /// <summary>Resolves the shaper and returns a plan from the cache, creating and caching it on a miss,
    /// <c>hb_shape_plan_create_cached2</c>.</summary>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="run">The run, whose segment properties key the plan.</param>
    /// <returns>The plan and the updated cache.</returns>
    let internal planFromCache (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (run: GlyphRun) : struct (Result<ShapePlan, ShapeError> * PlanCache) =
        match Execute.shaperName shapers with
        | ValueNone ->
            let requested = match shapers with | ValueSome names -> names | ValueNone -> ImmutableArray.Empty
            struct (Error (NoShaperSucceeded requested), cache)
        | ValueSome shaper ->
            match ShapePlan.key font run.SegmentProperties (features.AsSpan()) shaper with
            | ValueNone -> struct (Error (NoShaperSucceeded ImmutableArray.Empty), cache)
            | ValueSome key -> cachedPlan cache font features shapers run key

    /// <summary>Shapes a run through a plan cache, <c>hb_shape_full</c> using <c>hb_shape_plan_create_cached2</c>.
    /// The caller threads the cache through its lines; a hit returns the same cache value.</summary>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run and the cache, unchanged on a hit.</returns>
    let internal shapeCached (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (run: GlyphRun) =
        match run.Infos.Length with
        | 0 -> struct (Ok run, cache)
        | _ ->
            let struct (plan, cache) = planFromCache cache font features shapers run
            match plan with
            | Error error -> struct (Error error, cache)
            | Ok plan -> struct (executeAndVerify font features shapers (textCopy run) plan run, cache)

    /// <summary>Shapes a line decoded for the stages to write into: the plan from the cache, the line through <c>Execute.tryRunFresh</c>, and a
    /// line the route declined decoded again and shaped by <c>Execute.runDeclined</c>.</summary>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language the segment properties of a line decoded again are guessed with.</param>
    /// <param name="settings">The run that holds the segment properties, flags and limits of the line.</param>
    /// <param name="text">The whole text.</param>
    /// <param name="offset">The first code unit of the line.</param>
    /// <param name="length">The code units of the line.</param>
    /// <param name="decoded">The line, decoded without properties, with its segment properties guessed, not empty.</param>
    /// <returns>The shaped run and the cache.</returns>
    let private shapeFresh (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<byte>) (offset: int) (length: int) (decoded: GlyphRun) =
        let struct (plan, cache) = planFromCache cache font features shapers decoded
        match plan with
        | Error error -> struct (Error error, cache)
        | Ok plan ->
            match Execute.tryRunFresh plan font decoded with
            | ValueSome shaped -> struct (shaped, cache)
            | ValueNone -> struct (Execute.runDeclined plan font features (GlyphRun.guessSegmentProperties defaultLanguage (GlyphRun.addUtf8 text.Span offset length settings)), cache)

    /// <summary>Shapes UTF-8 text as one line through a plan cache: the result of decoding the text with <c>GlyphRun.addUtf8</c> into the
    /// settings, guessing the segment properties with <c>GlyphRun.guessSegmentProperties</c> and shaping the run with
    /// <see cref="shapeCached"/>, in every field. The glyphs are decoded once and written into by the stages, with no properties array
    /// made for the decoded run, so that a line shapes with a third less allocation than the three calls it replaces.</summary>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language <c>guessSegmentProperties</c> gives a run that has none.</param>
    /// <param name="settings">The run that holds the segment properties, flags and limits of the line.</param>
    /// <param name="text">The whole text.</param>
    /// <param name="offset">The first code unit of the line.</param>
    /// <param name="length">The code units of the line, or negative for the rest.</param>
    /// <returns>The shaped run and the cache, unchanged on a hit.</returns>
    let internal shapeUtf8 (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<byte>) (offset: int) (length: int) =
        let verifies = settings.Flags &&& BufferFlags.Verify <> BufferFlags.Default
        let decoded =
            match verifies with
            | true -> GlyphRun.guessSegmentProperties defaultLanguage (GlyphRun.addUtf8 text.Span offset length settings)
            | false -> GlyphRun.guessSegmentProperties defaultLanguage (GlyphRun.addUtf8Fresh text.Span offset length settings)
        match verifies || decoded.Infos.Length = 0 with
        | true -> shapeCached cache font features shapers decoded
        | false -> shapeFresh cache font features shapers defaultLanguage settings text offset length decoded

    /// <summary>Executes a plan on a run, <c>hb_shape_plan_execute</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let shapeWithPlan (font: Font) (plan: ShapePlan) (features: ImmutableArray<Feature>) (run: GlyphRun) : Result<GlyphRun, ShapeError> =
        Execute.run plan font features run

    /// <summary>The degree of parallelism that means every core of the machine.</summary>
    [<Literal>]
    let internal AllCores = 0

    /// <summary>The fewest glyphs of a document that are worth spreading over threads, measured with <c>--batch-shape</c>:
    /// below it the scheduling of the chunks costs more than the shaping they hold.</summary>
    [<Literal>]
    let internal SequentialBelowGlyphs = 1024

    /// <summary>A shape plan, or the reason there is none, kept with the segment properties it was resolved for.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ResolvedPlan =
        { Props: SegmentProperties
          Plan: Result<ShapePlan, ShapeError> }

    /// <summary>Whether a segment-properties value already has a resolved plan.</summary>
    /// <param name="plans">The resolved plans.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="index">The next plan.</param>
    /// <returns>True when a plan was resolved for the properties.</returns>
    [<TailCall>]
    let rec private isResolved (plans: ImmutableArray<ResolvedPlan>) (props: SegmentProperties) (index: int) =
        match index >= plans.Length with
        | true -> false
        | false ->
            match plans[index].Props = props with
            | true -> true
            | false -> isResolved plans props (index + 1)

    /// <summary>Finds the plan resolved for segment properties. Every non-empty run has one by construction of
    /// <c>resolvePlans</c>, so the end of the list is not reached.</summary>
    /// <param name="plans">The resolved plans.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="index">The next plan.</param>
    /// <returns>The plan, or why there is none.</returns>
    [<TailCall>]
    let rec private findPlan (plans: ImmutableArray<ResolvedPlan>) (props: SegmentProperties) (index: int) =
        match index >= plans.Length with
        | true -> Error (NoShaperSucceeded ImmutableArray.Empty)
        | false ->
            match plans[index].Props = props with
            | true -> plans[index].Plan
            | false -> findPlan plans props (index + 1)

    /// <summary>Resolves, in the order the runs first need them, one plan for every distinct segment-properties value of the
    /// non-empty runs, through the cache, as <c>shapeCached</c> does line by line.</summary>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="runs">The runs.</param>
    /// <param name="index">The next run.</param>
    /// <param name="plans">The plans resolved so far.</param>
    /// <returns>The resolved plans and the cache with the new plans added.</returns>
    [<TailCall>]
    let rec private resolvePlans (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (runs: ImmutableArray<GlyphRun>) (index: int) (plans: ImmutableArray<ResolvedPlan>) =
        match index >= runs.Length with
        | true -> struct (plans, cache)
        | false ->
            let run = runs[index]
            match run.Infos.Length = 0 || isResolved plans run.SegmentProperties 0 with
            | true -> resolvePlans cache font features shapers runs (index + 1) plans
            | false ->
                let struct (plan, cache) = planFromCache cache font features shapers run
                let current = plans.AsSpan()
                let added = Sealed.appendOne current { Props = run.SegmentProperties; Plan = plan }
                resolvePlans cache font features shapers runs (index + 1) added

    /// <summary>Shapes one run with the plan resolved for its segment properties: <c>shapeCached</c> with the cache lookup done.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="plans">The resolved plans.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let private shapeResolved (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (plans: ImmutableArray<ResolvedPlan>) (run: GlyphRun) =
        match run.Infos.Length with
        | 0 -> Ok run
        | _ ->
            match findPlan plans run.SegmentProperties 0 with
            | Error error -> Error error
            | Ok plan -> executeAndVerify font features shapers (textCopy run) plan run

    /// <summary>Everything one worker reads to shape the run at an index: shared immutable values only.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LineWorker =
        { Font: Font
          Features: ImmutableArray<Feature>
          Shapers: ImmutableArray<string> voption
          Plans: ImmutableArray<ResolvedPlan>
          Runs: ImmutableArray<GlyphRun> }

        interface IIndexedWorker<Result<GlyphRun, ShapeError>> with
            member this.At index = shapeResolved this.Font this.Features this.Shapers this.Plans (this.Runs[index])

    /// <summary>Shapes the runs through chunks spread over threads, or on the calling thread when the degree is one, the
    /// document is small or the platform is the browser.</summary>
    /// <param name="sequentialBelow">The glyph count below which the document is shaped on the calling thread.</param>
    /// <param name="degree">The most threads at once.</param>
    /// <param name="runs">The runs.</param>
    /// <param name="worker">The worker that shapes the run at an index.</param>
    /// <returns>The shaped runs, in order.</returns>
    let private fill (sequentialBelow: int) (degree: int) (runs: ImmutableArray<GlyphRun>) (worker: LineWorker) =
        let threads = match degree <= 0 with | true -> Environment.ProcessorCount | false -> degree
        let costs = Sealed.init runs.Length (fun index -> runs[index].Infos.Length)
        let total = ParallelFill.totalCost (costs.AsSpan())
        match threads = 1 || OperatingSystem.IsBrowser() || total < int64 sequentialBelow with
        | true -> ParallelFill.sequential runs.Length worker
        | false ->
            let chunkCost = int (max (int64 ParallelFill.MinChunkCost) (total / int64 (threads * ParallelFill.ChunksPerThread)))
            let starts = ParallelFill.chunkStarts (costs.AsSpan()) chunkCost
            ParallelFill.chunked threads runs.Length starts worker

    /// <summary>Shapes the runs of a document, equal element by element to mapping <c>shapeCached</c> over them in order with
    /// the cache threaded through. The plans are resolved first, one per distinct segment-properties value, in an immutable
    /// list the workers read; the runs are then shaped in chunks of similar glyph counts on the thread pool, each worker
    /// writing only its own slots. Nothing can throw: a run that cannot be shaped is an <c>Error</c> in its own slot and the
    /// others are unaffected. A degree of one is the sequential path.</summary>
    /// <param name="sequentialBelow">The glyph count below which the document is shaped on the calling thread.</param>
    /// <param name="degree">The most threads at once; <c>AllCores</c> or any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="runs">The runs.</param>
    /// <returns>The shaped runs in order and the cache with the plans the document needed.</returns>
    let internal shapeManyFrom (sequentialBelow: int) (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (runs: ImmutableArray<GlyphRun>) =
        let struct (plans, cache) = resolvePlans cache font features shapers runs 0 ImmutableArray.Empty
        let worker = { Font = font; Features = features; Shapers = shapers; Plans = plans; Runs = runs }
        struct (fill sequentialBelow degree runs worker, cache)

    /// <summary>Shapes the runs of a document, <c>shapeManyFrom</c> with the measured threshold <c>SequentialBelowGlyphs</c>.</summary>
    /// <param name="degree">The most threads at once; <c>AllCores</c> or any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="runs">The runs.</param>
    /// <returns>The shaped runs in order and the cache with the plans the document needed.</returns>
    let internal shapeMany (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (runs: ImmutableArray<GlyphRun>) =
        shapeManyFrom SequentialBelowGlyphs degree cache font features shapers runs
