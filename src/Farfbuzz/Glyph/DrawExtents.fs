namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Var

/// <summary>The draw session of one glyph under the extents sink: the path state of <c>hb_draw_funcs_t</c> and the box and
/// budget of <c>hb_paint_extents_sink_t</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal DrawState =
    { PathOpen: bool
      StartX: float32
      StartY: float32
      CurX: float32
      CurY: float32
      Extents: Box
      Budget: int64 }

/// <summary>A point that may be missing, <c>optional_point_t</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal OptionalPoint =
    { Has: bool
      PX: float32
      PY: float32 }

/// <summary>The contour state of the glyf path builder, <c>glyf_impl::path_builder_t</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal PathBuilder =
    { FirstOnCurve: OptionalPoint
      FirstOffCurve: OptionalPoint
      FirstOffCurve2: OptionalPoint
      LastOffCurve: OptionalPoint
      LastOffCurve2: OptionalPoint }

/// <summary>The extents of what <c>hb_font_draw_glyph</c> draws under the paint-extents sink, <c>hb-paint-extents.cc</c>,
/// <c>hb-draw.hh</c>, <c>OT/glyf/path-builder.hh</c> and the CFF path procs.</summary>
[<RequireQualifiedAccess>]
module internal DrawExtents =
    /// <summary>The flag of an on-curve glyf point, <c>FLAG_ON_CURVE</c>.</summary>
    [<Literal>]
    let private FlagOnCurve = 0x01uy

    /// <summary>The flag of a cubic control point, <c>FLAG_CUBIC</c>.</summary>
    [<Literal>]
    let private FlagCubic = 0x80uy

    /// <summary>The number of phantom points at the end of a glyf point list, <c>PHANTOM_COUNT</c>.</summary>
    [<Literal>]
    let private PhantomCount = 4

    /// <summary>The cost of one drawn segment, <c>HB_BUDGET_1</c>.</summary>
    [<Literal>]
    let private SegmentCost = 1L

    /// <summary>The Null subroutine INDEX of a CFF1 font-dict index out of range.</summary>
    let private emptyCff1Index = { Start = 0; Count = 0u; OffSize = 0; CountSize = 2 }

    /// <summary>The Null subroutine INDEX of a CFF2 font-dict index out of range.</summary>
    let private emptyCff2Index = { Start = 0; Count = 0u; OffSize = 0; CountSize = 4 }

    /// <summary>A fresh draw session.</summary>
    /// <param name="budget">The live budget.</param>
    /// <returns>The state.</returns>
    let start budget =
        { PathOpen = false
          StartX = 0.0f
          StartY = 0.0f
          CurX = 0.0f
          CurY = 0.0f
          Extents = PaintGeometry.voidBox
          Budget = budget }

    /// <summary>Takes a segment from the budget, <c>hb_paint_extents_sink_t::consume_segment</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>Whether the budget still holds, and the state with the segment spent.</returns>
    let private consume (state: DrawState) =
        let budget = state.Budget - SegmentCost
        struct (budget >= 0L, { state with Budget = budget })

    /// <summary>Adds the point of one emitted segment to the box when the budget allows.</summary>
    /// <param name="state">The state.</param>
    /// <param name="x">The point's x.</param>
    /// <param name="y">The point's y.</param>
    /// <returns>The state.</returns>
    let private emitOne (state: DrawState) x y =
        let struct (allowed, spent) = consume state
        match allowed with
        | false -> spent
        | true -> { spent with Extents = PaintGeometry.addPoint spent.Extents x y }

    /// <summary>Adds the two points of one emitted segment to the box when the budget allows.</summary>
    /// <param name="state">The state.</param>
    /// <param name="x1">The first point's x.</param>
    /// <param name="y1">The first point's y.</param>
    /// <param name="x2">The second point's x.</param>
    /// <param name="y2">The second point's y.</param>
    /// <returns>The state.</returns>
    let private emitTwo (state: DrawState) x1 y1 x2 y2 =
        let struct (allowed, spent) = consume state
        match allowed with
        | false -> spent
        | true -> { spent with Extents = PaintGeometry.addPoint (PaintGeometry.addPoint spent.Extents x1 y1) x2 y2 }

    /// <summary>Adds the three points of one emitted segment to the box when the budget allows.</summary>
    /// <param name="state">The state.</param>
    /// <param name="x1">The first point's x.</param>
    /// <param name="y1">The first point's y.</param>
    /// <param name="x2">The second point's x.</param>
    /// <param name="y2">The second point's y.</param>
    /// <param name="x3">The third point's x.</param>
    /// <param name="y3">The third point's y.</param>
    /// <returns>The state.</returns>
    let private emitThree (state: DrawState) x1 y1 x2 y2 x3 y3 =
        let struct (allowed, spent) = consume state
        match allowed with
        | false -> spent
        | true ->
            let first = PaintGeometry.addPoint spent.Extents x1 y1
            let second = PaintGeometry.addPoint first x2 y2
            { spent with Extents = PaintGeometry.addPoint second x3 y3 }

    /// <summary>Starts the path at the current point, <c>hb_draw_funcs_t::start_path</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state with the path open.</returns>
    let private startPath (state: DrawState) =
        let emitted = emitOne state state.CurX state.CurY
        { emitted with PathOpen = true; StartX = state.CurX; StartY = state.CurY }

    /// <summary>Opens the path if it is not open yet.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private ensureOpen (state: DrawState) =
        match state.PathOpen with
        | true -> state
        | false -> startPath state

    /// <summary>Closes the path, <c>hb_draw_funcs_t::close_path</c>: a line back to the start unless the path ended there.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state with no path open and the points reset.</returns>
    let closePath (state: DrawState) =
        match state.PathOpen with
        | false -> { state with StartX = 0.0f; StartY = 0.0f; CurX = 0.0f; CurY = 0.0f }
        | true ->
            let closed =
                match state.StartX <> state.CurX || state.StartY <> state.CurY with
                | true -> emitOne state state.StartX state.StartY
                | false -> state
            { closed with PathOpen = false; StartX = 0.0f; StartY = 0.0f; CurX = 0.0f; CurY = 0.0f }

    /// <summary>Moves to a point, <c>hb_draw_funcs_t::move_to</c>: an open path closes first.</summary>
    /// <param name="state">The state.</param>
    /// <param name="x">The x.</param>
    /// <param name="y">The y.</param>
    /// <returns>The state.</returns>
    let moveTo (state: DrawState) x y =
        let closed = closePath state
        { closed with CurX = x; CurY = y }

    /// <summary>Draws a line, <c>hb_draw_funcs_t::line_to</c>.</summary>
    /// <param name="state">The state.</param>
    /// <param name="x">The x.</param>
    /// <param name="y">The y.</param>
    /// <returns>The state.</returns>
    let lineTo (state: DrawState) x y =
        let opened = ensureOpen state
        let emitted = emitOne opened x y
        { emitted with CurX = x; CurY = y }

    /// <summary>Draws a quadratic curve, <c>hb_draw_funcs_t::quadratic_to</c>.</summary>
    /// <param name="state">The state.</param>
    /// <param name="cx">The control x.</param>
    /// <param name="cy">The control y.</param>
    /// <param name="x">The x.</param>
    /// <param name="y">The y.</param>
    /// <returns>The state.</returns>
    let quadraticTo (state: DrawState) cx cy x y =
        let opened = ensureOpen state
        let emitted = emitTwo opened cx cy x y
        { emitted with CurX = x; CurY = y }

    /// <summary>Draws a cubic curve, <c>hb_draw_funcs_t::cubic_to</c>.</summary>
    /// <param name="state">The state.</param>
    /// <param name="c1x">The first control x.</param>
    /// <param name="c1y">The first control y.</param>
    /// <param name="c2x">The second control x.</param>
    /// <param name="c2y">The second control y.</param>
    /// <param name="x">The x.</param>
    /// <param name="y">The y.</param>
    /// <returns>The state.</returns>
    let cubicTo (state: DrawState) c1x c1y c2x c2y x y =
        let opened = ensureOpen state
        let emitted = emitThree opened c1x c1y c2x c2y x y
        { emitted with CurX = x; CurY = y }

    /// <summary>No point.</summary>
    let private noPoint = { Has = false; PX = 0.0f; PY = 0.0f }

    /// <summary>The path builder before any point.</summary>
    let private freshBuilder =
        { FirstOnCurve = noPoint
          FirstOffCurve = noPoint
          FirstOffCurve2 = noPoint
          LastOffCurve = noPoint
          LastOffCurve2 = noPoint }

    /// <summary>The midpoint of two points, <c>optional_point_t::mid</c>.</summary>
    /// <param name="a">The first point.</param>
    /// <param name="b">The second point.</param>
    /// <returns>The midpoint.</returns>
    let private mid (a: OptionalPoint) (b: OptionalPoint) =
        { Has = true; PX = (a.PX + b.PX) * 0.5f; PY = (a.PY + b.PY) * 0.5f }

    /// <summary>Draws a cubic through the optional points of a path builder.</summary>
    /// <param name="state">The state.</param>
    /// <param name="c1">The first control.</param>
    /// <param name="c2">The second control.</param>
    /// <param name="p">The end point.</param>
    /// <returns>The state.</returns>
    let private cubicVia (state: DrawState) (c1: OptionalPoint) (c2: OptionalPoint) (p: OptionalPoint) =
        cubicTo state c1.PX c1.PY c2.PX c2.PY p.PX p.PY

    /// <summary>Draws a quadratic through the optional points of a path builder.</summary>
    /// <param name="state">The state.</param>
    /// <param name="c">The control.</param>
    /// <param name="p">The end point.</param>
    /// <returns>The state.</returns>
    let private quadraticVia (state: DrawState) (c: OptionalPoint) (p: OptionalPoint) =
        quadraticTo state c.PX c.PY p.PX p.PY

    /// <summary>The first point of a contour, <c>path_builder_t::consume_point</c> before the first on-curve point.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <param name="p">The point.</param>
    /// <param name="isOnCurve">Whether the point is on the curve.</param>
    /// <param name="isCubic">Whether the point is a cubic control.</param>
    /// <returns>The builder and the state.</returns>
    let private consumeFirst (builder: PathBuilder) (state: DrawState) (p: OptionalPoint) isOnCurve isCubic =
        match isOnCurve with
        | true -> struct ({ builder with FirstOnCurve = p }, moveTo state p.PX p.PY)
        | false ->
            match isCubic && not builder.FirstOffCurve2.Has with
            | true -> struct ({ builder with FirstOffCurve2 = builder.FirstOffCurve; FirstOffCurve = p }, state)
            | false ->
                match builder.FirstOffCurve.Has with
                | true ->
                    let middle = mid builder.FirstOffCurve p
                    struct ({ builder with FirstOnCurve = middle; LastOffCurve = p }, moveTo state middle.PX middle.PY)
                | false -> struct ({ builder with FirstOffCurve = p }, state)

    /// <summary>A point after a pending off-curve point, <c>path_builder_t::consume_point</c> with a last off-curve point.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <param name="p">The point.</param>
    /// <param name="isOnCurve">Whether the point is on the curve.</param>
    /// <param name="isCubic">Whether the point is a cubic control.</param>
    /// <returns>The builder and the state.</returns>
    let private consumeAfterOffCurve (builder: PathBuilder) (state: DrawState) (p: OptionalPoint) isOnCurve isCubic =
        match isOnCurve with
        | true ->
            let drawn =
                match builder.LastOffCurve2.Has with
                | true -> cubicVia state builder.LastOffCurve2 builder.LastOffCurve p
                | false -> quadraticVia state builder.LastOffCurve p
            struct ({ builder with LastOffCurve2 = noPoint; LastOffCurve = noPoint }, drawn)
        | false ->
            match isCubic && not builder.LastOffCurve2.Has with
            | true -> struct ({ builder with LastOffCurve2 = builder.LastOffCurve; LastOffCurve = p }, state)
            | false ->
                let middle = mid builder.LastOffCurve p
                match isCubic with
                | true ->
                    let drawn = cubicVia state builder.LastOffCurve2 builder.LastOffCurve middle
                    struct ({ builder with LastOffCurve2 = noPoint; LastOffCurve = p }, drawn)
                | false ->
                    let drawn = quadraticVia state builder.LastOffCurve middle
                    struct ({ builder with LastOffCurve = p }, drawn)

    /// <summary>Takes one glyf contour point, <c>path_builder_t::consume_point</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="point">The point.</param>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <returns>The builder and the state.</returns>
    let private consumePoint (font: Font) (point: ContourPoint) (builder: PathBuilder) (state: DrawState) =
        let isOnCurve = (point.Flag &&& FlagOnCurve) <> 0uy
        let isCubic = not isOnCurve && (point.Flag &&& FlagCubic) <> 0uy
        let p = { Has = true; PX = Font.emFscalefX font point.X; PY = Font.emFscalefY font point.Y }
        match builder.FirstOnCurve.Has, builder.LastOffCurve.Has with
        | false, _ -> consumeFirst builder state p isOnCurve isCubic
        | true, true -> consumeAfterOffCurve builder state p isOnCurve isCubic
        | true, false ->
            match isOnCurve with
            | true -> struct (builder, lineTo state p.PX p.PY)
            | false -> struct ({ builder with LastOffCurve = p }, state)

    /// <summary>Closes the pending curves of a contour, <c>path_builder_t::contour_end</c> before the first point is joined.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <returns>The builder and the state.</returns>
    let private closeLast (builder: PathBuilder) (state: DrawState) =
        match builder.FirstOffCurve.Has && builder.LastOffCurve.Has with
        | false -> struct (builder, state)
        | true ->
            let target = match builder.FirstOffCurve2.Has with | true -> builder.FirstOffCurve2 | false -> builder.FirstOffCurve
            let middle = mid builder.LastOffCurve target
            let drawn =
                match builder.LastOffCurve2.Has with
                | true -> cubicVia state builder.LastOffCurve2 builder.LastOffCurve middle
                | false -> quadraticVia state builder.LastOffCurve middle
            struct ({ builder with LastOffCurve = noPoint }, drawn)

    /// <summary>Joins the end of a contour to its first point, <c>path_builder_t::contour_end</c> after the last curve.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private joinFirst (builder: PathBuilder) (state: DrawState) =
        match builder.FirstOffCurve.Has, builder.LastOffCurve.Has, builder.FirstOnCurve.Has with
        | true, _, true ->
            match builder.FirstOffCurve2.Has with
            | true -> cubicVia state builder.FirstOffCurve2 builder.FirstOffCurve builder.FirstOnCurve
            | false -> quadraticVia state builder.FirstOffCurve builder.FirstOnCurve
        | false, true, true ->
            match builder.LastOffCurve2.Has with
            | true -> cubicVia state builder.LastOffCurve2 builder.LastOffCurve builder.FirstOnCurve
            | false -> quadraticVia state builder.LastOffCurve builder.FirstOnCurve
        | false, false, true -> lineTo state builder.FirstOnCurve.PX builder.FirstOnCurve.PY
        | true, _, false ->
            let bare = builder.FirstOffCurve
            let moved = moveTo state bare.PX bare.PY
            quadraticTo moved bare.PX bare.PY bare.PX bare.PY
        | false, _, false ->
            match builder.FirstOffCurve2.Has with
            | true ->
                let middle = mid builder.FirstOffCurve2 builder.FirstOffCurve
                let moved = moveTo state middle.PX middle.PY
                cubicVia moved builder.FirstOffCurve2 builder.FirstOffCurve middle
            | false -> state

    /// <summary>Ends a contour, <c>path_builder_t::contour_end</c>.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private contourEnd (builder: PathBuilder) (state: DrawState) =
        let struct (closedBuilder, closedState) = closeLast builder state
        closePath (joinFirst closedBuilder closedState)

    /// <summary>Whether the glyf point list ends a contour at an index.</summary>
    /// <param name="points">The points.</param>
    /// <param name="index">The index.</param>
    /// <returns>True when the point ends a contour.</returns>
    let private endsContour (points: ImmutableArray<ContourPoint>) index =
        points[index].IsEndPoint

    /// <summary>Takes the points of a contour that starts on the curve, one after the other up to its end point.</summary>
    /// <param name="font">The font.</param>
    /// <param name="points">The points.</param>
    /// <param name="count">The number of contour points.</param>
    /// <param name="index">The next point.</param>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <returns>The index after the contour, the builder and the state.</returns>
    [<TailCall>]
    let rec private onCurveContour (font: Font) (points: ImmutableArray<ContourPoint>) count index (builder: PathBuilder) (state: DrawState) =
        match index >= count with
        | true -> struct (index, state)
        | false ->
            let struct (next, drawn) = consumePoint font points[index] builder state
            match endsContour points index with
            | true -> struct (index, contourEnd next drawn)
            | false -> onCurveContour font points count (index + 1) next drawn

    /// <summary>Takes a run of points in order.</summary>
    /// <param name="font">The font.</param>
    /// <param name="points">The points.</param>
    /// <param name="index">The next point.</param>
    /// <param name="limit">The end of the run.</param>
    /// <param name="builder">The builder.</param>
    /// <param name="state">The state.</param>
    /// <returns>The builder and the state.</returns>
    [<TailCall>]
    let rec private takeRun (font: Font) (points: ImmutableArray<ContourPoint>) index limit (builder: PathBuilder) (state: DrawState) =
        match index >= limit with
        | true -> struct (builder, state)
        | false ->
            let struct (next, drawn) = consumePoint font points[index] builder state
            takeRun font points (index + 1) limit next drawn

    /// <summary>Finds the end of a contour, the first end point from an index.</summary>
    /// <param name="points">The points.</param>
    /// <param name="count">The number of contour points.</param>
    /// <param name="index">The next point.</param>
    /// <returns>The index of the end point, or the count when there is none.</returns>
    [<TailCall>]
    let rec private findEnd (points: ImmutableArray<ContourPoint>) count index =
        match index >= count || endsContour points index with
        | true -> index
        | false -> findEnd points count (index + 1)

    /// <summary>Takes a contour that starts off the curve, rotating its end point to the front unless cubic controls pair up.</summary>
    /// <param name="font">The font.</param>
    /// <param name="points">The points.</param>
    /// <param name="count">The number of contour points.</param>
    /// <param name="start">The first point of the contour.</param>
    /// <param name="state">The state.</param>
    /// <returns>The index of the end point and the state.</returns>
    let private offCurveContour (font: Font) (points: ImmutableArray<ContourPoint>) count start (state: DrawState) =
        let finish = findEnd points count start
        let keepsOrder =
            finish < count
            && (points[start].Flag &&& FlagCubic) <> 0uy
            && (points[finish].Flag &&& (FlagOnCurve ||| FlagCubic)) = FlagCubic
        let limit = match keepsOrder with | true -> finish + 1 | false -> finish
        let struct (rotated, rotatedState) =
            match keepsOrder || finish >= count with
            | true -> struct (freshBuilder, state)
            | false -> consumePoint font points[finish] freshBuilder state
        let struct (run, runState) = takeRun font points start limit rotated rotatedState
        struct (finish, contourEnd run runState)

    /// <summary>Walks the contours of a glyf point list, <c>glyf_accelerator_t::get_points</c> feeding the path builder.</summary>
    /// <param name="font">The font.</param>
    /// <param name="points">The points, phantoms last.</param>
    /// <param name="count">The number of contour points.</param>
    /// <param name="index">The next point.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    [<TailCall>]
    let rec private contours (font: Font) (points: ImmutableArray<ContourPoint>) count index (state: DrawState) =
        match index >= count with
        | true -> state
        | false ->
            match (points[index].Flag &&& FlagOnCurve) <> 0uy with
            | true ->
                let struct (last, drawn) = onCurveContour font points count index freshBuilder state
                contours font points count (last + 1) drawn
            | false ->
                let struct (last, drawn) = offCurveContour font points count index state
                contours font points count (last + 1) drawn

    /// <summary>Draws a glyf glyph, <c>glyf_accelerator_t::get_path</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="state">The state.</param>
    /// <returns>Whether the glyph drew, and the state.</returns>
    let private glyfPath (font: Font) (glyph: uint32) (state: DrawState) =
        let glyf = font.Ot.Glyf.Value
        match glyf.Present && uint64 glyph < uint64 glyf.NumGlyphs with
        | false -> struct (false, state)
        | true ->
            let coords = font.Variations.Coords
            match GlyphOutline.pointsWithin font.Ot.GlyfPoints.Value font.Ot.Gvar.Value coords glyph state.Budget with
            | ValueNone -> struct (false, state)
            | ValueSome (struct (points, remaining)) ->
                let count = points.Length - PhantomCount
                struct (true, contours font points count 0 { state with Budget = remaining })

    /// <summary>Scales a charstring coordinate and its seac offset into the font's scale, <c>em_fscalef_x</c> of a real.</summary>
    /// <param name="mult">The multiplier.</param>
    /// <param name="value">The coordinate.</param>
    /// <param name="delta">The seac offset.</param>
    /// <returns>The scaled value.</returns>
    let private scaled mult value delta =
        float32 (value + delta) * mult

    /// <summary>The CFF charstring sink that draws, <c>cff1_path_procs_path_t</c> and <c>cff2_path_procs_path_t</c>.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private CffDrawSink =
        { XMult: float32
          YMult: float32
          DeltaX: float
          DeltaY: float }
        interface ICharStringSink<DrawState> with
            member this.MoveTo (state, x, y) =
                moveTo state (scaled this.XMult x this.DeltaX) (scaled this.YMult y this.DeltaY)
            member this.LineTo (state, _, _, x, y) =
                lineTo state (scaled this.XMult x this.DeltaX) (scaled this.YMult y this.DeltaY)
            member this.CurveTo (state, _, _, x1, y1, x2, y2, x3, y3) =
                cubicTo
                    state
                    (scaled this.XMult x1 this.DeltaX)
                    (scaled this.YMult y1 this.DeltaY)
                    (scaled this.XMult x2 this.DeltaX)
                    (scaled this.YMult y2 this.DeltaY)
                    (scaled this.XMult x3 this.DeltaX)
                    (scaled this.YMult y3 this.DeltaY)

    /// <summary>The sink of a font's charstrings.</summary>
    /// <param name="font">The font.</param>
    /// <param name="deltaX">The seac offset's x.</param>
    /// <param name="deltaY">The seac offset's y.</param>
    /// <returns>The sink.</returns>
    let private cffSink (font: Font) deltaX deltaY =
        { XMult = font.XMultF; YMult = font.YMultF; DeltaX = deltaX; DeltaY = deltaY }

    /// <summary>Carries the interpreter's remaining budget into the state: the segments the sink spent come off it too.</summary>
    /// <param name="before">The state before the charstring.</param>
    /// <param name="after">The state after the charstring.</param>
    /// <param name="remaining">The budget the interpreter has left.</param>
    /// <returns>The state with both costs taken.</returns>
    let private settleBudget (before: DrawState) (after: DrawState) remaining =
        { after with Budget = remaining - (before.Budget - after.Budget) }

    /// <summary>Draws one CFF1 glyph, <c>_get_path</c> of hb-ot-cff1-table.cc, with the seac of its endchar.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cff">The CFF1 accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="inSeac">Whether the glyph is a part of a seac.</param>
    /// <param name="deltaX">The accent's offset x.</param>
    /// <param name="deltaY">The accent's offset y.</param>
    /// <param name="state">The state.</param>
    /// <returns>Whether the glyph drew, and the state.</returns>
    let rec private cff1Path (font: Font) (cff: Cff1Accel) (glyph: uint32) inSeac deltaX deltaY (state: DrawState) =
        match cff.Valid && glyph < cff.GlyphCount with
        | false -> struct (false, state)
        | true ->
            let table = cff.Table.Span
            let struct (charOffset, charLength) = CffIndex.item table cff.CharStrings glyph
            let fd = Cff1.fdOf cff glyph
            let localSubrs = match fd < cff.LocalSubrs.Length with | true -> cff.LocalSubrs[fd] | false -> emptyCff1Index
            let struct (ran, drawn, ending, remaining) = CffCharString.interpretKeeping (cffSink font deltaX deltaY) state CffCharString.cff1Flavour charOffset charLength cff.GlobalSubrs localSubrs table state.Budget
            let settled = settleBudget state drawn remaining
            match ran, ending with
            | false, _ -> struct (false, settled)
            | true, Ended -> struct (true, closePath settled)
            | true, EndedWithSeac (adx, ady, baseCode, accentCode) -> cff1Seac font cff inSeac adx ady baseCode accentCode (closePath settled)

    /// <summary>Draws the base and the accent of a seac endchar, <c>cff1_cs_opset_path_t::process_seac</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cff">The CFF1 accelerator.</param>
    /// <param name="inSeac">Whether the glyph is already a part of a seac.</param>
    /// <param name="adx">The accent's offset x.</param>
    /// <param name="ady">The accent's offset y.</param>
    /// <param name="baseCode">The base's standard code.</param>
    /// <param name="accentCode">The accent's standard code.</param>
    /// <param name="state">The state with the path closed.</param>
    /// <returns>Whether both drew, and the state.</returns>
    and private cff1Seac (font: Font) (cff: Cff1Accel) inSeac adx ady baseCode accentCode (state: DrawState) =
        let baseGlyph = Cff1.stdCodeToGlyph cff baseCode
        let accentGlyph = Cff1.stdCodeToGlyph cff accentCode
        match inSeac || baseGlyph = 0u || accentGlyph = 0u with
        | true -> struct (false, state)
        | false ->
            let struct (baseDrawn, afterBase) = cff1Path font cff baseGlyph true 0.0 0.0 state
            match baseDrawn with
            | false -> struct (false, afterBase)
            | true ->
                let struct (accentDrawn, afterAccent) = cff1Path font cff accentGlyph true adx ady afterBase
                match accentDrawn with
                | false -> struct (false, afterAccent)
                | true -> struct (true, closePath afterAccent)

    /// <summary>Draws one CFF2 glyph at the font's coordinates, <c>cff2::accelerator_t::get_path_at</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cff">The CFF2 accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="state">The state.</param>
    /// <returns>Whether the glyph drew, and the state.</returns>
    let private cff2Path (font: Font) (cff: Cff2Accel) (glyph: uint32) (state: DrawState) =
        match cff.Valid && glyph < uint32 cff.NumGlyphs with
        | false -> struct (false, state)
        | true ->
            let table = cff.Table.Span
            let struct (charOffset, charLength) = CffIndex.item table cff.CharStrings glyph
            let fd = Cff2.fdOf cff glyph
            let ivs = match fd < cff.PrivateDicts.Length with | true -> cff.PrivateDicts[fd].Ivs | false -> 0
            let localSubrs = match fd < cff.PrivateDicts.Length with | true -> cff.PrivateDicts[fd].LocalSubrs | false -> emptyCff2Index
            let coords = match font.Variations.Coords.HasNonZero with | true -> font.Variations.Coords | false -> NormalizedCoords.empty
            let blend = { Blending = coords.Length > 0 && cff.HasStore; Store = font.Variations.Cff2.Value; Ivs = ivs }
            let struct (ran, drawn, _, remaining) = CffCharString.interpretKeeping (cffSink font 0.0 0.0) state (CffFlavour.Cff2 blend) charOffset charLength cff.GlobalSubrs localSubrs table state.Budget
            struct (ran, settleBudget state drawn remaining)

    /// <summary>Draws a glyph with the outlines of a face, <c>hb_ot_draw_glyph_or_fail</c> after VARC: glyf, then CFF2, then CFF1.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private outlines (font: Font) (glyph: uint32) (state: DrawState) =
        let struct (glyfDrawn, afterGlyf) = glyfPath font glyph state
        match glyfDrawn with
        | true -> afterGlyf
        | false ->
            let struct (cff2Drawn, afterCff2) =
                match Face.tryFindTable font.Face Tags.Cff2 with
                | ValueSome _ -> cff2Path font font.Ot.Cff2.Value glyph afterGlyf
                | ValueNone -> struct (false, afterGlyf)
            match cff2Drawn with
            | true -> afterCff2
            | false ->
                match Face.tryFindTable font.Face Tags.Cff with
                | ValueSome _ ->
                    let struct (_, afterCff1) = cff1Path font font.Ot.Cff1.Value glyph false 0.0 0.0 afterCff2
                    afterCff1
                | ValueNone -> afterCff2

    /// <summary>The box <c>hb_font_draw_glyph</c> draws into the paint-extents sink, and the budget left: the session closes
    /// its last path, <c>~hb_draw_session_t</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The live budget of the paint session.</param>
    /// <returns>The void box when nothing drew, and the remaining budget.</returns>
    let glyphBox (font: Font) (glyph: uint32) budget =
        let finished = closePath (outlines font glyph (start budget))
        struct (finished.Extents, finished.Budget)
