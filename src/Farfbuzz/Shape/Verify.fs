namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The differences between two buffers, the values of <c>hb_buffer_diff_flags_t</c>.</summary>
[<Flags>]
type internal DiffFlags =
    | Equal = 0x0000
    | ContentTypeMismatch = 0x0001
    | LengthMismatch = 0x0002
    | NotdefPresent = 0x0004
    | DottedCirclePresent = 0x0008
    | CodepointMismatch = 0x0010
    | ClusterMismatch = 0x0020
    | GlyphFlagsMismatch = 0x0040
    | PositionMismatch = 0x0080

/// <summary>Buffer verification and comparison, <c>hb-buffer-verify.cc</c> and <c>hb_buffer_diff</c>.</summary>
[<RequireQualifiedAccess>]
module internal Verify =
    /// <summary>Whether a direction is forward, the predicate behind <c>HB_DIRECTION_IS_FORWARD</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>True for LTR and TTB.</returns>
    let private isForward (direction: Direction) : bool =
        match direction with
        | Ltr | Ttb -> true
        | Invalid | Rtl | Btt -> false

    /// <summary>Whether a cluster level is monotone, <c>HB_BUFFER_CLUSTER_LEVEL_IS_MONOTONE</c>.</summary>
    /// <param name="level">The cluster level.</param>
    /// <returns>True for the two monotone levels.</returns>
    let private isMonotoneLevel (level: ClusterLevel) : bool =
        match level with
        | ClusterLevel.MonotoneGraphemes | ClusterLevel.MonotoneCharacters -> true
        | ClusterLevel.Characters | ClusterLevel.Graphemes -> false

    /// <summary>Scans for a non-monotone cluster, <c>buffer_verify_monotone</c>'s loop.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="index">The next index.</param>
    /// <returns>True when the clusters are monotone.</returns>
    [<TailCall>]
    let rec private monotoneScan (infos: ImmutableArray<GlyphInfo>) (forward: bool) (index: int) : bool =
        match index >= infos.Length with
        | true -> true
        | false ->
            let increasing = infos[index - 1].Cluster < infos[index].Cluster
            match infos[index - 1].Cluster <> infos[index].Cluster && increasing <> forward with
            | true -> false
            | false -> monotoneScan infos forward (index + 1)

    /// <summary>The absolute difference of two positions as an unsigned value, <c>position_diff</c>.</summary>
    /// <param name="a">The first position.</param>
    /// <param name="b">The second position.</param>
    /// <returns>The absolute difference.</returns>
    let private positionDiff (a: int) (b: int) : uint64 =
        let diff = int64 a - int64 b
        if diff < 0L then uint64 -diff else uint64 diff

    /// <summary>Scans the reference for dotted-circle and not-def glyphs when lengths differ.</summary>
    /// <param name="infos">The reference glyphs.</param>
    /// <param name="dottedCircle">The dotted-circle glyph, or none.</param>
    /// <param name="index">The next index.</param>
    /// <param name="result">The flags so far.</param>
    /// <returns>The accumulated flags.</returns>
    [<TailCall>]
    let rec private scanReference (infos: ImmutableArray<GlyphInfo>) (dottedCircle: uint32 voption) (index: int) (result: DiffFlags) : DiffFlags =
        match index >= infos.Length with
        | true -> result
        | false ->
            let info = infos[index]
            let dotted =
                match dottedCircle with
                | ValueSome dc when info.Codepoint = dc -> DiffFlags.DottedCirclePresent
                | _ -> DiffFlags.Equal
            let notdef =
                match dottedCircle with
                | ValueSome _ when info.Codepoint = 0u -> DiffFlags.NotdefPresent
                | _ -> DiffFlags.Equal
            scanReference infos dottedCircle (index + 1) (result ||| dotted ||| notdef)

    /// <summary>Compares two glyph streams element by element, <c>hb_buffer_diff</c>'s glyph loop.</summary>
    /// <param name="buffer">The first glyphs.</param>
    /// <param name="reference">The reference glyphs.</param>
    /// <param name="dottedCircle">The dotted-circle glyph, or none.</param>
    /// <param name="count">The number of glyphs.</param>
    /// <param name="index">The next index.</param>
    /// <param name="result">The flags so far.</param>
    /// <returns>The accumulated flags.</returns>
    [<TailCall>]
    let rec private diffGlyphs (buffer: ImmutableArray<GlyphInfo>) (reference: ImmutableArray<GlyphInfo>) (dottedCircle: uint32 voption) (count: int) (index: int) (result: DiffFlags) : DiffFlags =
        match index >= count with
        | true -> result
        | false ->
            let b = buffer[index]
            let r = reference[index]
            let result = if b.Codepoint <> r.Codepoint then result ||| DiffFlags.CodepointMismatch else result
            let result = if b.Cluster <> r.Cluster then result ||| DiffFlags.ClusterMismatch else result
            let result = if (b.Mask ^^^ r.Mask) &&& uint32 GlyphFlags.Defined <> 0u then result ||| DiffFlags.GlyphFlagsMismatch else result
            let dotted =
                match dottedCircle with
                | ValueSome dc when r.Codepoint = dc -> DiffFlags.DottedCirclePresent
                | _ -> DiffFlags.Equal
            let notdef =
                match dottedCircle with
                | ValueSome _ when r.Codepoint = 0u -> DiffFlags.NotdefPresent
                | _ -> DiffFlags.Equal
            diffGlyphs buffer reference dottedCircle count (index + 1) (result ||| dotted ||| notdef)

    /// <summary>Compares two position streams, <c>hb_buffer_diff</c>'s position loop.</summary>
    /// <param name="buffer">The first positions.</param>
    /// <param name="reference">The reference positions.</param>
    /// <param name="positionFuzz">The allowed position difference.</param>
    /// <param name="count">The number of positions.</param>
    /// <param name="index">The next index.</param>
    /// <param name="result">The flags so far.</param>
    /// <returns>The accumulated flags.</returns>
    [<TailCall>]
    let rec private diffPositions (buffer: ImmutableArray<GlyphPosition>) (reference: ImmutableArray<GlyphPosition>) (positionFuzz: uint32) (count: int) (index: int) (result: DiffFlags) : DiffFlags =
        match index >= count with
        | true -> result
        | false ->
            let b = buffer[index]
            let r = reference[index]
            let mismatch =
                positionDiff b.XAdvance r.XAdvance > uint64 positionFuzz
                || positionDiff b.YAdvance r.YAdvance > uint64 positionFuzz
                || positionDiff b.XOffset r.XOffset > uint64 positionFuzz
                || positionDiff b.YOffset r.YOffset > uint64 positionFuzz
            let result = if mismatch then result ||| DiffFlags.PositionMismatch else result
            diffPositions buffer reference positionFuzz count (index + 1) result

    /// <summary>Compares two equal-length buffers glyph by glyph, <c>hb_buffer_diff</c>'s tail.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="reference">The reference buffer.</param>
    /// <param name="dottedCircle">The dotted-circle glyph, or none.</param>
    /// <param name="positionFuzz">The allowed position difference.</param>
    /// <param name="count">The number of glyphs.</param>
    /// <returns>The differences.</returns>
    let private diffSameLength (buffer: GlyphRun) (reference: GlyphRun) (dottedCircle: uint32 voption) (positionFuzz: uint32) (count: int) : DiffFlags =
        match count with
        | 0 -> DiffFlags.Equal
        | _ ->
            let compared = diffGlyphs buffer.Infos reference.Infos dottedCircle count 0 DiffFlags.Equal
            match buffer.ContentType with
            | GlyphContent -> diffPositions buffer.Positions reference.Positions positionFuzz count 0 compared
            | NoContent | UnicodeContent -> compared

    /// <summary>Compares two buffers, <c>hb_buffer_diff</c>.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="reference">The reference buffer.</param>
    /// <param name="dottedCircle">The dotted-circle glyph, or none to skip those bits.</param>
    /// <param name="positionFuzz">The allowed absolute position difference.</param>
    /// <returns>The differences.</returns>
    let diff (buffer: GlyphRun) (reference: GlyphRun) (dottedCircle: uint32 voption) (positionFuzz: uint32) : DiffFlags =
        match buffer.ContentType <> reference.ContentType && buffer.Infos.Length > 0 && reference.Infos.Length > 0 with
        | true -> DiffFlags.ContentTypeMismatch
        | false ->
            let count = reference.Infos.Length
            match buffer.Infos.Length = count with
            | false -> scanReference reference.Infos dottedCircle 0 DiffFlags.LengthMismatch
            | true -> diffSameLength buffer reference dottedCircle positionFuzz count

    /// <summary>Builds an empty run with the buffer settings of another, minus the verify flag, <c>hb_buffer_create_similar</c>.</summary>
    /// <param name="run">The run whose settings are copied.</param>
    /// <returns>The empty similar run.</returns>
    let private similar (run: GlyphRun) : GlyphRun =
        { GlyphRun.empty with
            Flags = run.Flags &&& ~~~BufferFlags.Verify
            ClusterLevel = run.ClusterLevel
            Replacement = run.Replacement
            Invisible = run.Invisible
            NotFound = run.NotFound
            NotFoundVariationSelector = run.NotFoundVariationSelector }

    /// <summary>Finds the end of the text range of a forward fragment, <c>hb-buffer-verify.cc:139-141</c>.</summary>
    /// <param name="text">The text glyphs.</param>
    /// <param name="numChars">The number of text glyphs.</param>
    /// <param name="cluster">The cluster boundary.</param>
    /// <param name="finish">The current end.</param>
    /// <returns>The first index whose cluster reaches the boundary.</returns>
    [<TailCall>]
    let rec private textEndForward (text: ImmutableArray<GlyphInfo>) (numChars: int) (cluster: uint32) (finish: int) : int =
        match finish < numChars && text[finish].Cluster < cluster with
        | true -> textEndForward text numChars cluster (finish + 1)
        | false -> finish

    /// <summary>Finds the start of the text range of a backward fragment, <c>hb-buffer-verify.cc:145-147</c>.</summary>
    /// <param name="text">The text glyphs.</param>
    /// <param name="cluster">The cluster boundary.</param>
    /// <param name="start">The current start.</param>
    /// <returns>The first index whose preceding cluster is before the boundary.</returns>
    [<TailCall>]
    let rec private textStartBackward (text: ImmutableArray<GlyphInfo>) (cluster: uint32) (start: int) : int =
        match start > 0 && text[start - 1].Cluster >= cluster with
        | true -> textStartBackward text cluster (start - 1)
        | false -> start

    /// <summary>Returns the plan of a verify fragment, <c>hb_shape_plan_create_cached2</c>: the shaped run's own plan
    /// when the fragment has the segment properties that plan was built for, a new plan otherwise.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="props">The fragment's segment properties.</param>
    /// <returns>The plan, or why no shaper can run.</returns>
    let private fragmentPlan font features shapers known props =
        match known with
        | ValueSome (struct (knownProps, plan)) when knownProps = props -> Ok plan
        | ValueSome _
        | ValueNone -> Execute.createPlan font props features shapers

    /// <summary>Shapes one verify fragment, <c>hb_shape_full</c> with the verify flag cleared.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="fragment">The fragment run.</param>
    /// <returns>The shaped fragment, or none when it fails to shape.</returns>
    let private shapeFragment (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (fragment: GlyphRun) : GlyphRun voption =
        match fragment.Infos.Length with
        | 0 -> ValueSome fragment
        | _ ->
            match fragmentPlan font features shapers known fragment.SegmentProperties with
            | Error _ -> ValueNone
            | Ok plan ->
                match Execute.run plan font features fragment with
                | Error _ -> ValueNone
                | Ok result -> ValueSome result

    /// <summary>Shapes one verify fragment and keeps it only when it shaped successfully, <c>hb-buffer-verify.cc:169-171</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="fragment">The fragment run.</param>
    /// <returns>The successfully shaped fragment, or none.</returns>
    let private shapeFragmentSuccessful (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (fragment: GlyphRun) : GlyphRun voption =
        match shapeFragment font features shapers known fragment with
        | ValueNone -> ValueNone
        | ValueSome result ->
            match result.Successful with
            | false -> ValueNone
            | true -> ValueSome result

    /// <summary>Returns the reconstruction failure when it differs from the shaped run.</summary>
    /// <param name="shaped">The shaped run.</param>
    /// <param name="reconstruction">The reconstruction.</param>
    /// <param name="check">The check that failed.</param>
    /// <returns>The shaped run, or the failure with the reconstruction.</returns>
    let private failedDiff (shaped: GlyphRun) (reconstruction: GlyphRun) (check: VerifyCheck) : Result<GlyphRun, ShapeError> =
        match reconstruction.Successful with
        | false -> Ok shaped
        | true ->
            let flags = diff reconstruction shaped ValueNone 0u
            match flags &&& ~~~DiffFlags.GlyphFlagsMismatch = DiffFlags.Equal with
            | true -> Ok shaped
            | false -> Error (VerifyFailed (check, reconstruction))

    /// <summary>Computes the text range of one break-check fragment, <c>hb-buffer-verify.cc:128-149</c>.</summary>
    /// <param name="infos">The shaped glyphs.</param>
    /// <param name="text">The text glyphs.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="numGlyphs">The number of shaped glyphs.</param>
    /// <param name="numChars">The number of text glyphs.</param>
    /// <param name="finish">The fragment end.</param>
    /// <param name="textStart">The current text start.</param>
    /// <param name="textEnd">The current text end.</param>
    /// <returns>The fragment's text range.</returns>
    let private breakTextRange (infos: ImmutableArray<GlyphInfo>) (text: ImmutableArray<GlyphInfo>) (forward: bool) (numGlyphs: int) (numChars: int) (finish: int) (textStart: int) (textEnd: int) : struct (int * int) =
        match finish = numGlyphs with
        | true ->
            match forward with
            | true -> struct (textStart, numChars)
            | false -> struct (0, textEnd)
        | false ->
            match forward with
            | true -> struct (textStart, textEndForward text numChars infos[finish].Cluster textEnd)
            | false -> struct (textStartBackward text infos[finish - 1].Cluster textStart, textEnd)

    /// <summary>The state of the break-check walk: either scanning for the next boundary or shaping a fragment.</summary>
    [<Struct>]
    type private BreakState =
        | BreakAdvance of textStart: int * textEnd: int * reconstruction: GlyphRun * finish: int * flags: BufferFlags
        | BreakShape of textStart: int * textEnd: int * reconstruction: GlyphRun * finish: int * flags: BufferFlags

    /// <summary>Decides the next break-check action at a glyph boundary, <c>hb-buffer-verify.cc:120-154</c>.</summary>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="numChars">The number of text glyphs.</param>
    /// <param name="textStart">The current text start.</param>
    /// <param name="textEnd">The current text end.</param>
    /// <param name="reconstruction">The reconstruction so far.</param>
    /// <param name="finish">The current glyph boundary.</param>
    /// <param name="flags">The fragment flags left by the previous fragment.</param>
    /// <returns>The next state, or the invalid-range failure.</returns>
    let private breakNextState (text: GlyphRun) (shaped: GlyphRun) (forward: bool) (numChars: int) (textStart: int) (textEnd: int) (reconstruction: GlyphRun) (finish: int) (flags: BufferFlags) : Result<BreakState, ShapeError> =
        let numGlyphs = shaped.Infos.Length
        let boundary =
            finish < numGlyphs
            && (shaped.Infos[finish].Cluster = shaped.Infos[finish - 1].Cluster
                || shaped.Infos[finish - (if forward then 0 else 1)].Mask &&& uint32 GlyphFlags.UnsafeToBreak <> 0u)
        match boundary with
        | true -> Ok (BreakAdvance (textStart, textEnd, reconstruction, finish + 1, flags))
        | false ->
            let struct (nextStart, nextEnd) = breakTextRange shaped.Infos text.Infos forward numGlyphs numChars finish textStart textEnd
            match nextStart >= nextEnd with
            | true -> Error (VerifyFailed (UnsafeToBreakRangeInvalid, shaped))
            | false -> Ok (BreakShape (nextStart, nextEnd, reconstruction, finish, flags))

    /// <summary>Walks the safe-to-break boundaries and shapes each fragment, <c>buffer_verify_unsafe_to_break</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="template">The empty similar run.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <param name="forward">Whether the direction is forward.</param>
    /// <param name="numChars">The number of text glyphs.</param>
    /// <param name="state">The walk state.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    [<TailCall>]
    let rec private breakLoop (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (template: GlyphRun) (text: GlyphRun) (shaped: GlyphRun) (forward: bool) (numChars: int) (state: BreakState) : Result<GlyphRun, ShapeError> =
        match state with
        | BreakAdvance (textStart, textEnd, reconstruction, finish, flags) ->
            match finish >= shaped.Infos.Length + 1 with
            | true -> failedDiff shaped reconstruction UnsafeToBreakFailed
            | false ->
                match breakNextState text shaped forward numChars textStart textEnd reconstruction finish flags with
                | Error error -> Error error
                | Ok next -> breakLoop font features shapers known template text shaped forward numChars next
        | BreakShape (textStart, textEnd, reconstruction, finish, flags) ->
            let flags = if textStart > 0 then flags &&& ~~~BufferFlags.Bot else flags
            let flags = if textEnd < numChars then flags &&& ~~~BufferFlags.Eot else flags
            let fragment = GlyphRun.append text textStart textEnd { template with Flags = flags }
            match shapeFragmentSuccessful font features shapers known fragment with
            | ValueNone -> Ok shaped
            | ValueSome result ->
                let next = if forward then textEnd else textStart
                breakLoop font features shapers known template text shaped forward numChars (BreakAdvance (next, next, GlyphRun.append result 0 result.Infos.Length reconstruction, finish + 1, flags))

    /// <summary>Runs the unsafe-to-break check, <c>buffer_verify_unsafe_to_break</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let private unsafeToBreak (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) : Result<GlyphRun, ShapeError> =
        match isMonotoneLevel shaped.ClusterLevel with
        | false -> Ok shaped
        | true ->
            let forward = isForward shaped.SegmentProperties.Direction
            let template = similar shaped
            let numChars = text.Infos.Length
            let textStart = if forward then 0 else numChars
            breakLoop font features shapers known template text shaped forward numChars (BreakAdvance (textStart, textStart, template, 1, template.Flags))

    /// <summary>Finds the end of a concat segment, <c>hb-buffer-verify.cc:341-344</c>.</summary>
    /// <param name="infos">The fragment glyphs.</param>
    /// <param name="numGlyphs">The number of glyphs.</param>
    /// <param name="finish">The candidate end.</param>
    /// <returns>The segment end.</returns>
    [<TailCall>]
    let rec private segmentEnd (infos: ImmutableArray<GlyphInfo>) (numGlyphs: int) (finish: int) : int =
        match finish < numGlyphs && (infos[finish].Cluster = infos[finish - 1].Cluster || infos[finish].Mask &&& uint32 GlyphFlags.UnsafeToConcat <> 0u) with
        | true -> segmentEnd infos numGlyphs (finish + 1)
        | false -> finish

    /// <summary>Splits the text into the two concat fragment streams, <c>hb-buffer-verify.cc:266-307</c>.</summary>
    /// <param name="text">The text.</param>
    /// <param name="infos">The shaped glyphs.</param>
    /// <param name="numChars">The number of text glyphs.</param>
    /// <param name="fragments">The two fragment runs.</param>
    /// <param name="fragmentIdx">Which fragment receives the next segment.</param>
    /// <param name="textPos">The current text boundary.</param>
    /// <param name="finish">The current glyph boundary.</param>
    /// <returns>The two fragment runs.</returns>
    [<TailCall>]
    let rec private concatSplit (text: GlyphRun) (infos: ImmutableArray<GlyphInfo>) (numChars: int) (fragments: struct (GlyphRun * GlyphRun)) (fragmentIdx: int) (textPos: int) (finish: int) : struct (GlyphRun * GlyphRun) =
        let numGlyphs = infos.Length
        match finish >= numGlyphs + 1 with
        | true -> fragments
        | false ->
            let boundary =
                finish < numGlyphs
                && (infos[finish].Cluster = infos[finish - 1].Cluster
                    || infos[finish].Mask &&& uint32 GlyphFlags.UnsafeToConcat <> 0u)
            match boundary with
            | true -> concatSplit text infos numChars fragments fragmentIdx textPos (finish + 1)
            | false ->
                let nextTextEnd =
                    match finish = numGlyphs with
                    | true -> numChars
                    | false -> textEndForward text.Infos numChars infos[finish].Cluster textPos
                let struct (first, second) = fragments
                let fragments =
                    match fragmentIdx with
                    | 0 -> struct (GlyphRun.append text textPos nextTextEnd first, second)
                    | _ -> struct (first, GlyphRun.append text textPos nextTextEnd second)
                concatSplit text infos numChars fragments (1 - fragmentIdx) nextTextEnd (finish + 1)

    /// <summary>Interleaves the two shaped fragment streams, <c>hb-buffer-verify.cc:337-350</c>.</summary>
    /// <param name="fragments">The two shaped fragment runs.</param>
    /// <param name="start0">The first stream's position.</param>
    /// <param name="start1">The second stream's position.</param>
    /// <param name="fragmentIdx">Which stream provides the next segment.</param>
    /// <param name="reconstruction">The reconstruction so far.</param>
    /// <returns>The reconstruction.</returns>
    [<TailCall>]
    let rec private concatReconstruct (fragments: struct (GlyphRun * GlyphRun)) (start0: int) (start1: int) (fragmentIdx: int) (reconstruction: GlyphRun) : GlyphRun =
        let struct (first, second) = fragments
        let numGlyphs0 = first.Infos.Length
        let numGlyphs1 = second.Infos.Length
        match start0 < numGlyphs0 || start1 < numGlyphs1 with
        | false -> reconstruction
        | true ->
            let fragment, start, numGlyphs =
                match fragmentIdx with
                | 0 -> first, start0, numGlyphs0
                | _ -> second, start1, numGlyphs1
            let fragmentEnd = segmentEnd fragment.Infos numGlyphs (start + 1)
            let reconstruction = GlyphRun.append fragment start fragmentEnd reconstruction
            let start0, start1 =
                match fragmentIdx with
                | 0 -> fragmentEnd, start1
                | _ -> start0, fragmentEnd
            concatReconstruct fragments start0 start1 (1 - fragmentIdx) reconstruction

    /// <summary>Shapes the two concat fragment streams, <c>hb-buffer-verify.cc:313-319</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="fragments">The two fragment runs.</param>
    /// <returns>The shaped streams, or none when one fails.</returns>
    let private concatShapeBoth (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (fragments: struct (GlyphRun * GlyphRun)) : struct (GlyphRun * GlyphRun) voption =
        let struct (first, second) = fragments
        match shapeFragmentSuccessful font features shapers known first with
        | ValueNone -> ValueNone
        | ValueSome shaped0 ->
            match shapeFragmentSuccessful font features shapers known second with
            | ValueNone -> ValueNone
            | ValueSome shaped1 -> ValueSome (struct (shaped0, shaped1))

    /// <summary>Runs the unsafe-to-concat check, <c>buffer_verify_unsafe_to_concat</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let private unsafeToConcat (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) : Result<GlyphRun, ShapeError> =
        match isMonotoneLevel shaped.ClusterLevel with
        | false -> Ok shaped
        | true ->
            let forward = isForward shaped.SegmentProperties.Direction
            let template = similar shaped
            let reversed = if forward then shaped else GlyphRun.reverse shaped
            let struct (first, second) = concatSplit text reversed.Infos text.Infos.Length struct (template, template) 0 0 1
            match concatShapeBoth font features shapers known struct (first, second) with
            | ValueNone -> Ok shaped
            | ValueSome (struct (shaped0, shaped1)) ->
                let struct (g0, g1) = if forward then struct (shaped0, shaped1) else struct (GlyphRun.reverse shaped0, GlyphRun.reverse shaped1)
                let reconstruction = concatReconstruct struct (g0, g1) 0 0 0 template
                let reconstruction = if forward then reconstruction else GlyphRun.reverse reconstruction
                failedDiff shaped reconstruction UnsafeToConcatFailed

    /// <summary>Runs the unsafe-to-concat check when the buffer asks for it, <c>hb_buffer_t::verify</c>'s third check.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let private verifyConcatIfAsked (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) : Result<GlyphRun, ShapeError> =
        match shaped.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default with
        | false -> Ok shaped
        | true ->
            match unsafeToConcat font features shapers known text shaped with
            | Error error -> Error error
            | Ok _ -> Ok shaped

    /// <summary>Runs the unsafe-to-break check after the monotone check passes, <c>hb_buffer_t::verify</c>'s second check.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let private verifyBreak (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) : Result<GlyphRun, ShapeError> =
        match unsafeToBreak font features shapers known text shaped with
        | Error error -> Error error
        | Ok _ -> verifyConcatIfAsked font features shapers known text shaped

    /// <summary>Runs the monotone check, <c>hb_buffer_t::verify</c>'s first check.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let private verifyMonotone (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) : Result<GlyphRun, ShapeError> =
        match monotoneScan shaped.Infos (isForward shaped.SegmentProperties.Direction) 1 with
        | false -> Error (VerifyFailed (NotMonotone, shaped))
        | true -> verifyBreak font features shapers known text shaped

    /// <summary>What the streamed break check keeps constant over one line.</summary>
    type private BreakLine =
        { LineFont: Font
          LineFeatures: ImmutableArray<Feature>
          LinePlan: ShapePlan
          LineWindow: SimpleRun.SimpleWindow voption
          LineTemplate: GlyphRun
          LineText: GlyphRun
          LineShaped: GlyphRun
          LineForward: bool
          LineChars: int }

    /// <summary>Whether a successfully shaped run has the content the streamed compare reads: glyphs with one position each.</summary>
    /// <param name="run">The run.</param>
    /// <returns>True for a glyph run with positions for every glyph.</returns>
    let private hasGlyphsAndPositions (run: GlyphRun) =
        run.ContentType = GlyphContent && run.HavePositions && run.Positions.Length = run.Infos.Length

    /// <summary>Compares a slice of a shaped fragment with the shaped glyphs from an offset, the diff of the reconstruction
    /// restricted to the slice.</summary>
    /// <param name="fragment">The shaped fragment.</param>
    /// <param name="from">The first fragment glyph of the slice.</param>
    /// <param name="count">The number of glyphs of the slice.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <param name="offset">The first shaped glyph of the slice.</param>
    /// <param name="index">The next slice glyph.</param>
    /// <returns>True when every glyph, cluster and position equals the shaped run's.</returns>
    [<TailCall>]
    let rec private sliceAgrees (fragment: GlyphRun) (from: int) (count: int) (shaped: GlyphRun) (offset: int) (index: int) =
        match index >= count with
        | true -> true
        | false ->
            let b = fragment.Infos[from + index]
            let r = shaped.Infos[offset + index]
            let bp = fragment.Positions[from + index]
            let rp = shaped.Positions[offset + index]
            let same =
                b.Codepoint = r.Codepoint
                && b.Cluster = r.Cluster
                && bp.XAdvance = rp.XAdvance
                && bp.YAdvance = rp.YAdvance
                && bp.XOffset = rp.XOffset
                && bp.YOffset = rp.YOffset
            match same with
            | true -> sliceAgrees fragment from count shaped offset (index + 1)
            | false -> false

    /// <summary>Shapes a fragment with a plan and keeps it only when it shaped successfully, <c>hb-buffer-verify.cc:169-171</c>.</summary>
    /// <param name="line">The line.</param>
    /// <param name="fragment">The fragment run.</param>
    /// <returns>The successfully shaped fragment, or none.</returns>
    let private shapeKept (line: BreakLine) (fragment: GlyphRun) =
        match fragment.Infos.Length with
        | 0 -> ValueSome fragment
        | _ ->
            match Execute.run line.LinePlan line.LineFont line.LineFeatures fragment with
            | Error _ -> ValueNone
            | Ok result when result.Successful -> ValueSome result
            | Ok _ -> ValueNone

    /// <summary>Shapes one fragment through the general route and compares it with the shaped glyphs from an offset.</summary>
    /// <param name="line">The line.</param>
    /// <param name="start">The first text index.</param>
    /// <param name="finish">The end text index.</param>
    /// <param name="flags">The fragment flags.</param>
    /// <param name="offset">The first shaped glyph of the fragment.</param>
    /// <returns>The fragment's glyph count, or minus one when it fails to shape or differs.</returns>
    let private fragmentByShaping (line: BreakLine) (start: int) (finish: int) (flags: BufferFlags) (offset: int) =
        match shapeKept line (GlyphRun.append line.LineText start finish { line.LineTemplate with Flags = flags }) with
        | ValueNone -> -1
        | ValueSome result ->
            match result.Infos.Length with
            | 0 -> 0
            | count ->
                match hasGlyphsAndPositions result && offset + count <= line.LineShaped.Infos.Length && sliceAgrees result 0 count line.LineShaped offset 0 with
                | true -> count
                | false -> -1

    /// <summary>Checks one fragment against the shaped glyphs from an offset: by the window kernel when it applies, else by
    /// shaping the fragment.</summary>
    /// <param name="line">The line.</param>
    /// <param name="start">The first text index.</param>
    /// <param name="finish">The end text index.</param>
    /// <param name="flags">The fragment flags.</param>
    /// <param name="offset">The first shaped glyph of the fragment.</param>
    /// <returns>The fragment's glyph count, or minus one when it fails to shape or differs.</returns>
    let private fragmentCount (line: BreakLine) (start: int) (finish: int) (flags: BufferFlags) (offset: int) =
        match line.LineWindow with
        | ValueNone -> fragmentByShaping line start finish flags offset
        | ValueSome window ->
            match SimpleRun.compareWindow window line.LineFont line.LineText start finish line.LineShaped offset with
            | SimpleRun.WindowAgrees -> finish - start
            | SimpleRun.WindowDiffers -> -1
            | SimpleRun.WindowRefused -> fragmentByShaping line start finish flags offset

    /// <summary>The outcome of checking the fragment at a boundary.</summary>
    [<Struct>]
    type private BreakStep =
        | StepFailed
        | StepDone of count: int * next: int * flags: BufferFlags

    /// <summary>Checks the fragment that ends at a safe boundary against the shaped glyphs, <c>hb-buffer-verify.cc:128-171</c>.</summary>
    /// <param name="line">The line.</param>
    /// <param name="textStart">The current text start.</param>
    /// <param name="textEnd">The current text end.</param>
    /// <param name="offset">The shaped glyphs the fragments so far account for.</param>
    /// <param name="finish">The current glyph boundary.</param>
    /// <param name="flags">The fragment flags left by the previous fragment.</param>
    /// <returns>The fragment's glyph count, the next text boundary and the flags, or the failure.</returns>
    let private breakStep (line: BreakLine) (textStart: int) (textEnd: int) (offset: int) (finish: int) (flags: BufferFlags) =
        let struct (nextStart, nextEnd) = breakTextRange line.LineShaped.Infos line.LineText.Infos line.LineForward line.LineShaped.Infos.Length line.LineChars finish textStart textEnd
        match nextStart >= nextEnd with
        | true -> StepFailed
        | false ->
            let flags = if nextStart > 0 then flags &&& ~~~BufferFlags.Bot else flags
            let flags = if nextEnd < line.LineChars then flags &&& ~~~BufferFlags.Eot else flags
            match fragmentCount line nextStart nextEnd flags offset with
            | -1 -> StepFailed
            | count -> StepDone (count, (if line.LineForward then nextEnd else nextStart), flags)

    /// <summary>Walks the safe-to-break boundaries as <see cref="breakLoop"/> does, comparing each shaped fragment with the
    /// shaped glyphs at the running offset instead of building the reconstruction.</summary>
    /// <param name="line">The line.</param>
    /// <param name="textStart">The current text start.</param>
    /// <param name="textEnd">The current text end.</param>
    /// <param name="offset">The shaped glyphs the fragments so far account for.</param>
    /// <param name="finish">The current glyph boundary.</param>
    /// <param name="flags">The fragment flags left by the previous fragment.</param>
    /// <returns>True when every fragment agrees and together they account for every shaped glyph; false when the reference
    /// must decide.</returns>
    [<TailCall>]
    let rec private breakStreamed (line: BreakLine) (textStart: int) (textEnd: int) (offset: int) (finish: int) (flags: BufferFlags) =
        let shaped = line.LineShaped
        let numGlyphs = shaped.Infos.Length
        match finish >= numGlyphs + 1 with
        | true -> offset = numGlyphs
        | false ->
            let boundary =
                finish < numGlyphs
                && (shaped.Infos[finish].Cluster = shaped.Infos[finish - 1].Cluster
                    || shaped.Infos[finish - (if line.LineForward then 0 else 1)].Mask &&& uint32 GlyphFlags.UnsafeToBreak <> 0u)
            match boundary with
            | true -> breakStreamed line textStart textEnd offset (finish + 1) flags
            | false ->
                match breakStep line textStart textEnd offset finish flags with
                | StepFailed -> false
                | StepDone (count, next, flags) -> breakStreamed line next next (offset + count) (finish + 1) flags

    /// <summary>Starts the streamed break check of a line.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>True when the check passes, false when the reference must decide.</returns>
    let private breakStreamedLine (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) =
        match shaped.Infos.Length > 0 && hasGlyphsAndPositions shaped with
        | false -> false
        | true ->
            match fragmentPlan font features shapers known text.SegmentProperties with
            | Error _ -> false
            | Ok plan ->
                let template = similar shaped
                let forward = isForward shaped.SegmentProperties.Direction
                let line =
                    { LineFont = font
                      LineFeatures = features
                      LinePlan = plan
                      LineWindow = SimpleRun.window plan font text
                      LineTemplate = template
                      LineText = text
                      LineShaped = shaped
                      LineForward = forward
                      LineChars = text.Infos.Length }
                let textStart = if forward then 0 else text.Infos.Length
                breakStreamed line textStart textStart 0 1 template.Flags

    /// <summary>Splits the text into the two concat fragment streams as <see cref="concatSplit"/> does, keeping the text
    /// ranges of each stream, last first.</summary>
    /// <param name="text">The text.</param>
    /// <param name="infos">The shaped glyphs.</param>
    /// <param name="numChars">The number of text glyphs.</param>
    /// <param name="first">The ranges of the first stream so far.</param>
    /// <param name="second">The ranges of the second stream so far.</param>
    /// <param name="fragmentIdx">Which stream receives the next segment.</param>
    /// <param name="textPos">The current text boundary.</param>
    /// <param name="finish">The current glyph boundary.</param>
    /// <returns>The ranges of the two streams.</returns>
    [<TailCall>]
    let rec private concatRanges (text: GlyphRun) (infos: ImmutableArray<GlyphInfo>) (numChars: int) (first: struct (int * int) list) (second: struct (int * int) list) (fragmentIdx: int) (textPos: int) (finish: int) =
        let numGlyphs = infos.Length
        match finish >= numGlyphs + 1 with
        | true -> struct (first, second)
        | false ->
            let boundary =
                finish < numGlyphs
                && (infos[finish].Cluster = infos[finish - 1].Cluster
                    || infos[finish].Mask &&& uint32 GlyphFlags.UnsafeToConcat <> 0u)
            match boundary with
            | true -> concatRanges text infos numChars first second fragmentIdx textPos (finish + 1)
            | false ->
                let nextTextEnd =
                    match finish = numGlyphs with
                    | true -> numChars
                    | false -> textEndForward text.Infos numChars infos[finish].Cluster textPos
                let range = struct (textPos, nextTextEnd)
                match fragmentIdx with
                | 0 -> concatRanges text infos numChars (range :: first) second 1 nextTextEnd (finish + 1)
                | _ -> concatRanges text infos numChars first (range :: second) 0 nextTextEnd (finish + 1)

    /// <summary>Interleaves the two shaped fragment streams as <see cref="concatReconstruct"/> does, comparing each segment
    /// with the shaped glyphs at the running offset instead of appending it to a reconstruction.</summary>
    /// <param name="first">The first shaped stream.</param>
    /// <param name="second">The second shaped stream.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <param name="start0">The first stream's position.</param>
    /// <param name="start1">The second stream's position.</param>
    /// <param name="fragmentIdx">Which stream provides the next segment.</param>
    /// <param name="offset">The shaped glyphs the segments so far account for.</param>
    /// <returns>True when every segment agrees and together they account for every shaped glyph.</returns>
    [<TailCall>]
    let rec private concatCompare (first: GlyphRun) (second: GlyphRun) (shaped: GlyphRun) (start0: int) (start1: int) (fragmentIdx: int) (offset: int) =
        let numGlyphs0 = first.Infos.Length
        let numGlyphs1 = second.Infos.Length
        match start0 < numGlyphs0 || start1 < numGlyphs1 with
        | false -> offset = shaped.Infos.Length
        | true ->
            let struct (fragment, start, numGlyphs) =
                match fragmentIdx with
                | 0 -> struct (first, start0, numGlyphs0)
                | _ -> struct (second, start1, numGlyphs1)
            let fragmentEnd = segmentEnd fragment.Infos numGlyphs (start + 1)
            let count = max 0 (min fragmentEnd numGlyphs - start)
            match count = 0 || (offset + count <= shaped.Infos.Length && sliceAgrees fragment start count shaped offset 0) with
            | false -> false
            | true ->
                let struct (next0, next1) =
                    match fragmentIdx with
                    | 0 -> struct (fragmentEnd, start1)
                    | _ -> struct (start0, fragmentEnd)
                concatCompare first second shaped next0 next1 (1 - fragmentIdx) (offset + count)

    /// <summary>Whether a shaped concat stream is empty or has glyphs with one position each.</summary>
    /// <param name="run">The stream.</param>
    /// <returns>True when the streamed compare may read it.</returns>
    let private streamReadable (run: GlyphRun) =
        run.Infos.Length = 0 || hasGlyphsAndPositions run

    /// <summary>Runs the unsafe-to-concat check on a left-to-right run without building the fragments' runs one append at a
    /// time or the reconstruction.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>True when the check passes, false when the reference must decide.</returns>
    let private concatStreamed (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) =
        match isForward shaped.SegmentProperties.Direction && shaped.Infos.Length > 0 && hasGlyphsAndPositions shaped with
        | false -> false
        | true ->
            match fragmentPlan font features shapers known text.SegmentProperties with
            | Error _ -> false
            | Ok plan ->
                let template = similar shaped
                let line =
                    { LineFont = font
                      LineFeatures = features
                      LinePlan = plan
                      LineWindow = ValueNone
                      LineTemplate = template
                      LineText = text
                      LineShaped = shaped
                      LineForward = true
                      LineChars = text.Infos.Length }
                let struct (first, second) = concatRanges text shaped.Infos text.Infos.Length [] [] 0 0 1
                let fragment0 = GlyphRun.appendRanges text (first |> List.rev |> Array.ofList) template
                let fragment1 = GlyphRun.appendRanges text (second |> List.rev |> Array.ofList) template
                match shapeKept line fragment0, shapeKept line fragment1 with
                | ValueSome shaped0, ValueSome shaped1 -> streamReadable shaped0 && streamReadable shaped1 && concatCompare shaped0 shaped1 shaped 0 0 0 0
                | ValueNone, _
                | _, ValueNone -> false

    /// <summary>Runs the unsafe-to-concat check when the buffer asks for it, streamed where the run allows and by the
    /// reference where it does not.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let private concatChecked (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) =
        match shaped.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default && concatStreamed font features shapers known text shaped with
        | true -> Ok shaped
        | false -> verifyConcatIfAsked font features shapers known text shaped

    /// <summary>Verifies a shaped run against its text, <c>hb_buffer_t::verify</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The original text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let verifyKnowing (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) : Result<GlyphRun, ShapeError> =
        match isMonotoneLevel shaped.ClusterLevel with
        | false -> Ok shaped
        | true -> verifyMonotone font features shapers known text shaped

    /// <summary>Verifies a shaped run against its text, building a plan for every fragment: the reference
    /// <see cref="verifyKnowing"/> must equal.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="text">The original text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let verify font features shapers text shaped =
        verifyKnowing font features shapers ValueNone text shaped

    /// <summary>Verifies a shaped run against its text as <see cref="verifyKnowing"/> does and gives the same result, but
    /// compares each shaped fragment with the shaped run as it is produced instead of building the reconstruction. Whenever the
    /// streamed check cannot prove the run correct, the reference decides, so the failing verdicts and their runs are the
    /// reference's.</summary>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="known">The plan of the shaped run with the segment properties it was built for, or none.</param>
    /// <param name="text">The original text.</param>
    /// <param name="shaped">The shaped run.</param>
    /// <returns>The shaped run, or the verification failure.</returns>
    let verifyStreaming (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (known: struct (SegmentProperties * ShapePlan) voption) (text: GlyphRun) (shaped: GlyphRun) =
        match isMonotoneLevel shaped.ClusterLevel with
        | false -> Ok shaped
        | true ->
            match monotoneScan shaped.Infos (isForward shaped.SegmentProperties.Direction) 1 with
            | false -> Error (VerifyFailed (NotMonotone, shaped))
            | true ->
                match breakStreamedLine font features shapers known text shaped with
                | false -> verifyKnowing font features shapers known text shaped
                | true -> concatChecked font features shapers known text shaped
