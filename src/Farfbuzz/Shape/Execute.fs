namespace Farfbuzz

open System.Collections.Immutable

/// <summary>Plan creation and execution, <c>hb_shape_plan_create</c> and <c>hb_shape_plan_execute</c>.</summary>
[<RequireQualifiedAccess>]
module internal Execute =
    /// <summary>The name of the ot shaper.</summary>
    [<Literal>]
    let private OtShaperName = "ot"

    /// <summary>The name of the fallback shaper, the last of the default list.</summary>
    [<Literal>]
    let private FallbackShaperName = "fallback"

    /// <summary>Which shaper a requested list picks first.</summary>
    [<Struct>]
    type private ShaperChoice =
        | ChooseOt
        | ChooseFallback

    /// <summary>Finds the first name in a requested list that names a known shaper, <c>hb_shape_plan_create</c>'s choice loop.</summary>
    /// <param name="names">The requested shapers.</param>
    /// <param name="index">The next name.</param>
    /// <returns>The choice, or none when no name is known.</returns>
    [<TailCall>]
    let rec private chooseShaper (names: ImmutableArray<string>) index : ShaperChoice voption =
        match index >= names.Length with
        | true -> ValueNone
        | false ->
            match names[index] with
            | name when name = OtShaperName -> ValueSome ChooseOt
            | name when name = FallbackShaperName -> ValueSome ChooseFallback
            | _ -> chooseShaper names (index + 1)

    /// <summary>Builds the ot plan for the ot shaper.</summary>
    /// <param name="font">The font.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <returns>The plan.</returns>
    let private otPlan (font: Font) (props: SegmentProperties) (features: ImmutableArray<Feature>) =
        let compiled = OtPlan.create font props features
        let lookupCovered = Engine.gposLookupCovered compiled font
        let glyphLookups = Engine.gposGlyphLookups compiled font
        { ShaperName = OtShaperName
          Ot = ValueSome compiled
          GsubCovered = Engine.gsubCovered compiled font
          GposCovered = Engine.gposCovered compiled font
          GsubLookupCovered = Engine.gsubLookupCovered compiled font
          GsubGlyphLookups = Engine.gsubGlyphLookups compiled font
          GsubWindows = Engine.gsubWindows compiled font
          GposLookupCovered = lookupCovered
          GposGlyphLookups = glyphLookups
          GposPairs = Engine.gposPairs compiled font lookupCovered glyphLookups }

    /// <summary>Builds a plan for one shaping run, choosing the shaper as <c>hb_shape_plan_create</c> does.</summary>
    /// <param name="font">The font.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <returns>The plan, or why no shaper can run.</returns>
    let createPlan font props features shapers =
        match shapers with
        | ValueNone -> Ok (otPlan font props features)
        | ValueSome names ->
            match chooseShaper names 0 with
            | ValueNone -> Error (NoShaperSucceeded names)
            | ValueSome ChooseOt -> Ok (otPlan font props features)
            | ValueSome ChooseFallback -> Ok { ShaperName = FallbackShaperName; Ot = ValueNone; GsubCovered = ImmutableArray.Empty; GposCovered = ImmutableArray.Empty; GsubLookupCovered = ImmutableArray.Empty; GsubGlyphLookups = ImmutableArray.Empty; GsubWindows = ImmutableArray.Empty; GposLookupCovered = ImmutableArray.Empty; GposGlyphLookups = ImmutableArray.Empty; GposPairs = PairTables.none }

    /// <summary>Whether the font or the plan carries functions the caller replaced, which only the general pipeline asks.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <returns>True when the font has functions or the plan has Unicode functions.</returns>
    let private replaced plan (font: Font) =
        ValueOption.isSome font.Funcs || ValueOption.exists (fun (ot: OtShapePlan) -> ValueOption.isSome ot.Unicode) plan.Ot

    /// <summary>Gives a plan the Unicode functions of a buffer, which the shapers read where <c>hb_shape_plan_execute</c> reads
    /// <c>buffer-&gt;unicode</c>.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="plan">The plan.</param>
    /// <returns>The plan answering through the functions.</returns>
    let withUnicode funcs (plan: ShapePlan) =
        { plan with Ot = ValueOption.map (fun (ot: OtShapePlan) -> { ot with Unicode = ValueSome funcs }) plan.Ot }

    /// <summary>Executes a plan on a run through the general pipeline only, <c>hb_shape_plan_execute</c>: the reference
    /// the simple-run kernel of <see cref="run"/> must equal in every field.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let runGeneral plan font (features: ImmutableArray<Feature>) run =
        ignore features
        match run.Infos.Length with
        | 0 -> Ok run
        | _ ->
            let entered = GlyphRun.enter run
            let shaped =
                match plan.Ot with
                | ValueSome otPlan -> OtShaper.shape otPlan font entered
                | ValueNone -> FallbackShaper.shape font entered
            match shaped with
            | Error error -> Error error
            | Ok result -> Ok { result with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u }

    /// <summary>Executes a plan on a run through the general pipeline with its lean front half and leaves the shaped line in a working
    /// set, <see cref="runLean"/> without the run record: for a caller that copies the glyphs and positions out of the set itself.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped set.</returns>
    let runLeanSet plan font (features: ImmutableArray<Feature>) run =
        match run.Infos.Length, plan.Ot with
        | 0, _ -> Ok (GlyphRun.setOf run)
        | _, _ when replaced plan font -> Result.map GlyphRun.setOf (runGeneral plan font features run)
        | _, ValueSome otPlan ->
            match GeneralLine.tryShapeSet plan otPlan font run with
            | ValueSome shaped -> shaped
            | ValueNone -> Result.map GlyphRun.setOf (SimpleRun.shapeGeneral plan otPlan font run)
        | _, ValueNone -> Result.map GlyphRun.setOf (runGeneral plan font features run)

    /// <summary>Executes a plan on a run through the general pipeline with its lean front half, <c>hb_shape_plan_execute</c>: the
    /// result of <see cref="runGeneral"/> in every field, the kernel's tails taking a line whose normalizer left no mark.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let runLean plan font (features: ImmutableArray<Feature>) run =
        Result.map GlyphRun.runOf (runLeanSet plan font features run)

    /// <summary>Executes a plan on a run and leaves the shaped line in a working set, <see cref="run"/> without the run record when the
    /// general route shapes the line.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped set.</returns>
    let runSet plan font (features: ImmutableArray<Feature>) run =
        match run.Infos.Length with
        | 0 -> Ok (GlyphRun.setOf run)
        | _ when replaced plan font -> Result.map GlyphRun.setOf (runGeneral plan font features run)
        | _ ->
            match SimpleRun.tryShape plan font run with
            | ValueSome shaped -> Result.map GlyphRun.setOf shaped
            | ValueNone -> runLeanSet plan font features run

    /// <summary>Executes a plan on a run that <see cref="tryRunFresh"/> declined, decoded again: the simple-run kernel has declined it and
    /// the working-set route declines it at the same point, so the run takes the general route as <see cref="runLean"/> takes a run
    /// that route declines.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run, not empty, decoded again with its properties.</param>
    /// <returns>The shaped run.</returns>
    let runDeclined plan font (features: ImmutableArray<Feature>) run =
        match plan.Ot with
        | _ when replaced plan font -> runGeneral plan font features run
        | ValueSome otPlan -> SimpleRun.shapeGeneral plan otPlan font run
        | ValueNone -> runGeneral plan font features run

    /// <summary>Executes a plan on a run the caller decoded in this call and shares with no one, <see cref="run"/> with the general route
    /// writing into the glyphs of the run: the answer is the shaped run, or none when the run must be decoded again and shaped by
    /// <see cref="run"/>, because the route that takes it may have written into its glyphs before it declined. The run is not to be read
    /// again after any answer.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with glyphs of its own, which may have no properties array.</param>
    /// <returns>The shaped run, or none when the caller must shape a run decoded again.</returns>
    let tryRunFresh plan font run =
        match run.Infos.Length with
        | 0 -> ValueSome (Ok run)
        | _ when replaced plan font -> ValueNone
        | _ ->
            match SimpleRun.tryShape plan font run with
            | ValueSome shaped -> ValueSome shaped
            | ValueNone ->
                match plan.Ot with
                | ValueNone -> ValueNone
                | ValueSome otPlan -> GeneralLine.tryShapeFresh plan otPlan font run

    /// <summary>Executes a plan on a run, <c>hb_shape_plan_execute</c>: the simple-run kernel first, the general
    /// pipeline when the run does not qualify.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run.</returns>
    let run plan font (features: ImmutableArray<Feature>) run =
        match run.Infos.Length with
        | 0 -> Ok run
        | _ when replaced plan font -> runGeneral plan font features run
        | _ ->
            match SimpleRun.tryShape plan font run with
            | ValueSome shaped -> shaped
            | ValueNone -> runLean plan font features run

    /// <summary>Resolves a shaper list to the first known shaper name, <c>hb_shape_plan_create</c>'s choice loop.</summary>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <returns>The shaper name, or none when no name is known.</returns>
    let shaperName (shapers: ImmutableArray<string> voption) : string voption =
        match shapers with
        | ValueNone -> ValueSome OtShaperName
        | ValueSome names ->
            match chooseShaper names 0 with
            | ValueNone -> ValueNone
            | ValueSome ChooseOt -> ValueSome OtShaperName
            | ValueSome ChooseFallback -> ValueSome FallbackShaperName
