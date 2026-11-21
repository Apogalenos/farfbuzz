namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.OpenType
open Farfbuzz.Var

/// <summary>What a paint walk has recorded so far: the extents context of <c>hb_paint_extents_context_t</c> or the
/// boundedness context of <c>hb_paint_bounded_context_t</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal PaintState =
    | ExtentsState of transforms: Transform list * clips: Bounds list * groups: Bounds list * budget: int64 * drawn: Map<uint32, struct (Box * int64)>
    | BoundedState of isBounded: bool * clipDepth: int * boundedGroups: bool list

/// <summary>The path of a depth-first walk for <c>hb_decycler_t</c>: the values of the nodes entered and the tortoise.</summary>
[<Struct; NoComparison; NoEquality>]
type internal PaintDecycler =
    { Values: ImmutableArray<uint32>
      Tortoise: int
      Awake: bool }

/// <summary>The extents of COLR glyphs, <c>COLR::get_extents</c> with <c>hb_paint_extents_context_t</c> and
/// <c>hb_paint_bounded_context_t</c>: the paint graph is walked, nothing is rendered.</summary>
[<RequireQualifiedAccess>]
module internal PaintExtents =
    /// <summary>The work budget of one glyph, <c>HB_BUDGET_GLYPH</c>.</summary>
    [<Literal>]
    let private BudgetGlyph = 16777216L

    /// <summary>The nesting limit of a paint walk, <c>HB_MAX_NESTING_LEVEL</c>.</summary>
    [<Literal>]
    let private MaxNestingLevel = 64

    /// <summary>The edge limit of a paint walk, <c>HB_MAX_GRAPH_EDGE_COUNT</c>.</summary>
    [<Literal>]
    let private MaxGraphEdgeCount = 16384

    /// <summary>The offset a paint keeps its source paint at.</summary>
    [<Literal>]
    let private SourceOffset = 1

    /// <summary>The last composite mode upstream knows, <c>HB_PAINT_COMPOSITE_MODE_HSL_LUMINOSITY</c>.</summary>
    [<Literal>]
    let private LastCompositeMode = 27

    /// <summary>The composite mode clear, <c>HB_PAINT_COMPOSITE_MODE_CLEAR</c>.</summary>
    [<Literal>]
    let private ModeClear = 0

    /// <summary>The composite mode source, <c>HB_PAINT_COMPOSITE_MODE_SRC</c>.</summary>
    [<Literal>]
    let private ModeSrc = 1

    /// <summary>The composite mode destination, <c>HB_PAINT_COMPOSITE_MODE_DEST</c>.</summary>
    [<Literal>]
    let private ModeDest = 2

    /// <summary>The composite mode source over, <c>HB_PAINT_COMPOSITE_MODE_SRC_OVER</c>.</summary>
    [<Literal>]
    let private ModeSrcOver = 3

    /// <summary>The composite mode source in, <c>HB_PAINT_COMPOSITE_MODE_SRC_IN</c>.</summary>
    [<Literal>]
    let private ModeSrcIn = 5

    /// <summary>The composite mode destination in, <c>HB_PAINT_COMPOSITE_MODE_DEST_IN</c>.</summary>
    [<Literal>]
    let private ModeDestIn = 6

    /// <summary>The composite mode source out, <c>HB_PAINT_COMPOSITE_MODE_SRC_OUT</c>.</summary>
    [<Literal>]
    let private ModeSrcOut = 7

    /// <summary>The composite mode destination out, <c>HB_PAINT_COMPOSITE_MODE_DEST_OUT</c>.</summary>
    [<Literal>]
    let private ModeDestOut = 8

    /// <summary>The value of pi the paint functions use, <c>HB_PI</c>.</summary>
    [<Literal>]
    let private Pi = 3.14159265358979323846f

    /// <summary>The scale of a 2.14 value, <c>HBFixed&lt;HBINT16, 14&gt;::mult</c>.</summary>
    [<Literal>]
    let private F2Dot14Mult = 6.103515625e-05f

    /// <summary>The scale of a 16.16 value, <c>HBFixed&lt;HBINT32, 16&gt;::mult</c>.</summary>
    [<Literal>]
    let private F16Dot16Mult = 1.52587890625e-05f

    /// <summary>The ids of the paint formats the walk acts on.</summary>
    [<Literal>]
    let private FormatColrLayers = 1

    /// <summary>The first solid colour format, <c>PaintSolid</c>.</summary>
    [<Literal>]
    let private FormatSolid = 2

    /// <summary>The variable solid colour format, <c>PaintSolid</c> with a variation index.</summary>
    [<Literal>]
    let private FormatSolidVar = 3

    /// <summary>The offset of a PaintGlyph's glyph id.</summary>
    [<Literal>]
    let private GlyphIdOffset = 4

    /// <summary>The offset of a PaintColrGlyph's glyph id.</summary>
    [<Literal>]
    let private ColrGlyphIdOffset = 1

    /// <summary>The variation index meaning no variation.</summary>
    [<Literal>]
    let private NoVariation = 0xFFFFFFFFu

    /// <summary>The state of one walk: what it recorded and the edges it has left.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private Walk =
        { Paint: PaintState
          Edges: int }

    /// <summary>What a walk reads: the table, the font and the variation deltas.</summary>
    [<NoComparison; NoEquality>]
    type private PaintEnv =
        { Colr: Colr
          Font: Font
          Instancer: ColrInstancer }

    /// <summary>The first element of a list that is never empty by construction.</summary>
    /// <param name="fallback">The value of an empty list.</param>
    /// <param name="items">The list.</param>
    /// <returns>The first element.</returns>
    let private headOr (fallback: 'T) (items: 'T list) =
        match items with
        | top :: _ -> top
        | [] -> fallback

    /// <summary>The list without its first element.</summary>
    /// <param name="items">The list.</param>
    /// <returns>The rest.</returns>
    let private rest (items: 'T list) =
        match items with
        | _ :: remaining -> remaining
        | [] -> []

    /// <summary>The empty group, <c>hb_bounds_t&lt;&gt;{EMPTY}</c>.</summary>
    let private emptyGroup = { Status = Empty; Box = PaintGeometry.voidBox }

    /// <summary>The unbounded clip a walk starts under, <c>hb_bounds_t&lt;&gt;{UNBOUNDED}</c>.</summary>
    let private unboundedClip = { Status = Unbounded; Box = PaintGeometry.voidBox }

    /// <summary>A fresh extents context, <c>hb_paint_extents_context_t::clear</c>.</summary>
    let private freshExtents = ExtentsState ([ PaintGeometry.identity ], [ unboundedClip ], [ emptyGroup ], BudgetGlyph, Map.empty)

    /// <summary>A fresh boundedness context, <c>hb_paint_bounded_context_t::clear</c>.</summary>
    let private freshBounded = BoundedState (true, 0, [])

    /// <summary>Pushes a transform, <c>hb_paint_funcs_t::push_transform</c> and <c>hb_paint_extents_context_t::push_transform</c>:
    /// a zero translation loses its sign, and the boundedness context ignores transforms.</summary>
    /// <param name="state">The state.</param>
    /// <param name="t">The transform.</param>
    /// <returns>The state.</returns>
    let private pushTransform (state: PaintState) (t: Transform) =
        match state with
        | BoundedState _ -> state
        | ExtentsState (transforms, clips, groups, budget, drawn) ->
            let cleaned =
                { t with
                    X0 = match t.X0 = 0.0f with | true -> 0.0f | false -> t.X0
                    Y0 = match t.Y0 = 0.0f with | true -> 0.0f | false -> t.Y0 }
            ExtentsState (PaintGeometry.multiply (headOr PaintGeometry.identity transforms) cleaned :: transforms, clips, groups, budget, drawn)

    /// <summary>Pops a transform, <c>hb_paint_extents_context_t::pop_transform</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private popTransform (state: PaintState) =
        match state with
        | BoundedState _ -> state
        | ExtentsState (transforms, clips, groups, budget, drawn) -> ExtentsState (rest transforms, clips, groups, budget, drawn)

    /// <summary>Pushes a clip box, <c>hb_paint_extents_context_t::push_clip</c>: the box transformed and intersected with the clip below.</summary>
    /// <param name="state">The state.</param>
    /// <param name="box">The box.</param>
    /// <returns>The state.</returns>
    let private pushClipBox (state: PaintState) (box: Box) =
        match state with
        | BoundedState (bounded, clips, groups) -> BoundedState (bounded, clips + 1, groups)
        | ExtentsState (transforms, clips, groups, budget, drawn) ->
            let transformed = PaintGeometry.transformBox (headOr PaintGeometry.identity transforms) box
            let bounds = PaintGeometry.intersectBounds (PaintGeometry.boundsOf transformed) (headOr unboundedClip clips)
            ExtentsState (transforms, bounds :: clips, groups, budget, drawn)

    /// <summary>Draws a glyph's outline into the extents, <c>hb_font_draw_glyph</c> under the paint-extents sink. A glyph drawn
    /// completely before costs the same again, whatever the budget, so its box and cost are remembered by the walk and drawn
    /// again only when the budget cannot cover the cost.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The live budget.</param>
    /// <param name="drawn">The glyphs drawn completely so far, with their boxes and costs.</param>
    /// <returns>The box, the budget left and the glyphs drawn completely.</returns>
    let private drawGlyph (font: Font) (glyph: uint32) budget (drawn: Map<uint32, struct (Box * int64)>) =
        match budget >= 0L, Map.tryFind glyph drawn with
        | false, _ -> struct (PaintGeometry.voidBox, budget, drawn)
        | true, Some (struct (box, cost)) when budget - cost >= 0L -> struct (box, budget - cost, drawn)
        | true, _ ->
            let struct (box, remaining) = DrawExtents.glyphBox font glyph budget
            match remaining >= 0L with
            | true -> struct (box, remaining, Map.add glyph (struct (box, budget - remaining)) drawn)
            | false -> struct (box, remaining, drawn)

    /// <summary>Pushes the clip of a glyph's outline, <c>hb_paint_extents_push_clip_glyph</c>: the outline is drawn into the box
    /// unless the budget is spent.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private pushClipGlyph (font: Font) (glyph: uint32) (state: PaintState) =
        match state with
        | BoundedState (bounded, clips, groups) -> BoundedState (bounded, clips + 1, groups)
        | ExtentsState (transforms, clips, groups, budget, drawn) ->
            let struct (box, remaining, known) = drawGlyph font glyph budget drawn
            pushClipBox (ExtentsState (transforms, clips, groups, remaining, known)) box

    /// <summary>Pops a clip, <c>pop_clip</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private popClip (state: PaintState) =
        match state with
        | BoundedState (bounded, clips, groups) ->
            match clips with
            | 0 -> state
            | _ -> BoundedState (bounded, clips - 1, groups)
        | ExtentsState (transforms, clips, groups, budget, drawn) -> ExtentsState (transforms, rest clips, groups, budget, drawn)

    /// <summary>Pushes a group, <c>push_group</c> and <c>push_group_for</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private pushGroup (state: PaintState) =
        match state with
        | BoundedState (bounded, clips, groups) -> BoundedState (true, clips, bounded :: groups)
        | ExtentsState (transforms, clips, groups, budget, drawn) -> ExtentsState (transforms, clips, emptyGroup :: groups, budget, drawn)

    /// <summary>The backdrop of a composite once its source is known, <c>hb_paint_extents_context_t::pop_group</c>.</summary>
    /// <param name="mode">The composite mode.</param>
    /// <param name="source">The source's bounds.</param>
    /// <param name="backdrop">The backdrop's bounds.</param>
    /// <returns>The composed bounds.</returns>
    let private composeBounds mode (source: Bounds) (backdrop: Bounds) =
        match mode with
        | ModeClear -> { backdrop with Status = Empty }
        | ModeSrc
        | ModeSrcOut -> source
        | ModeDest
        | ModeDestOut -> backdrop
        | ModeSrcIn
        | ModeDestIn -> PaintGeometry.intersectBounds backdrop source
        | _ -> PaintGeometry.unionBounds backdrop source

    /// <summary>The boundedness of a composite once its source is known, <c>hb_paint_bounded_context_t::pop_group</c>.</summary>
    /// <param name="mode">The composite mode.</param>
    /// <param name="source">Whether the source is bounded.</param>
    /// <param name="backdrop">Whether the backdrop is bounded.</param>
    /// <returns>Whether the result is bounded.</returns>
    let private composeBounded mode source backdrop =
        match mode with
        | ModeClear -> true
        | ModeSrc
        | ModeSrcOut -> source
        | ModeDest
        | ModeDestOut -> backdrop
        | ModeSrcIn
        | ModeDestIn -> backdrop && source
        | _ -> backdrop || source

    /// <summary>Pops a group, <c>pop_group</c>.</summary>
    /// <param name="mode">The composite mode.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private popGroup mode (state: PaintState) =
        match state with
        | BoundedState (bounded, clips, groups) ->
            let backdrop = headOr true groups
            BoundedState (composeBounded mode bounded backdrop, clips, rest groups)
        | ExtentsState (transforms, clips, groups, budget, drawn) ->
            let source = headOr emptyGroup groups
            let below = rest groups
            let composed = composeBounds mode source (headOr emptyGroup below)
            ExtentsState (transforms, clips, composed :: rest below, budget, drawn)

    /// <summary>Paints the clip into the group, <c>paint</c>: the colour, the gradients and the images all do this and no more.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private paintClip (state: PaintState) =
        match state with
        | BoundedState (_, clips, groups) ->
            match clips with
            | 0 -> BoundedState (false, clips, groups)
            | _ -> state
        | ExtentsState (transforms, clips, groups, budget, drawn) ->
            let united = PaintGeometry.unionBounds (headOr emptyGroup groups) (headOr unboundedClip clips)
            ExtentsState (transforms, clips, united :: rest groups, budget, drawn)

    /// <summary>Whether the live budget is spent, <c>*c-&gt;budget &lt; 0</c> in <c>recurse</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>True when spent.</returns>
    let private budgetSpent (state: PaintState) =
        match state with
        | BoundedState _ -> false
        | ExtentsState (_, _, _, budget, _) -> budget < 0L

    /// <summary>Fills a glyph, <c>hb_paint_fill_glyph_nil</c>: a clip of the glyph, the paint, the pop.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private fillGlyph (font: Font) (glyph: uint32) (state: PaintState) =
        popClip (paintClip (pushClipGlyph font glyph state))

    /// <summary>The transform of the font's scale, <c>push_font_transform</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The transform.</returns>
    let private fontTransform (font: Font) =
        let upem = float32 font.Face.Upem
        { XX = float32 font.XScale / upem; YX = 0.0f; XY = 0.0f; YY = float32 font.YScale / upem; X0 = 0.0f; Y0 = 0.0f }

    /// <summary>The inverse of the font's scale, <c>push_inverse_font_transform</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The transform.</returns>
    let private inverseFontTransform (font: Font) =
        let upem = float32 font.Face.Upem
        let xScale = match font.XScale <> 0 with | true -> font.XScale | false -> font.Face.Upem
        let yScale = match font.YScale <> 0 with | true -> font.YScale | false -> font.Face.Upem
        { XX = upem / float32 xScale; YX = 0.0f; XY = 0.0f; YY = upem / float32 yScale; X0 = 0.0f; Y0 = 0.0f }

    /// <summary>The scaling around a centre, <c>hb_transform_t::scaling_around_center</c>.</summary>
    /// <param name="sx">The x scale.</param>
    /// <param name="sy">The y scale.</param>
    /// <param name="cx">The centre's x.</param>
    /// <param name="cy">The centre's y.</param>
    /// <returns>The transform.</returns>
    let private scalingAroundCenter sx sy cx cy =
        { XX = sx
          YX = 0.0f
          XY = 0.0f
          YY = sy
          X0 = match cx <> 0.0f with | true -> (1.0f - sx) * cx | false -> 0.0f
          Y0 = match cy <> 0.0f with | true -> (1.0f - sy) * cy | false -> 0.0f }

    /// <summary>The rotation, <c>hb_transform_t::rotation</c>.</summary>
    /// <param name="radians">The angle.</param>
    /// <returns>The transform.</returns>
    let private rotation radians =
        let c = MathF.Cos radians
        let s = MathF.Sin radians
        { XX = c; YX = s; XY = -s; YY = c; X0 = 0.0f; Y0 = 0.0f }

    /// <summary>The rotation around a centre, <c>hb_transform_t::rotation_around_center</c>.</summary>
    /// <param name="radians">The angle.</param>
    /// <param name="cx">The centre's x.</param>
    /// <param name="cy">The centre's y.</param>
    /// <returns>The transform.</returns>
    let private rotationAroundCenter radians cx cy =
        let c = MathF.Cos radians
        let s = MathF.Sin radians
        { XX = c; YX = s; XY = -s; YY = c; X0 = (1.0f - c) * cx + s * cy; Y0 = -s * cx + (1.0f - c) * cy }

    /// <summary>The tangent of a skew angle, zero for no skew.</summary>
    /// <param name="angle">The angle.</param>
    /// <returns>The tangent.</returns>
    let private skewTangent angle =
        match angle <> 0.0f with
        | true -> MathF.Tan angle
        | false -> 0.0f

    /// <summary>The skew, <c>hb_transform_t::skewing</c>.</summary>
    /// <param name="skewX">The x angle.</param>
    /// <param name="skewY">The y angle.</param>
    /// <returns>The transform.</returns>
    let private skewing skewX skewY =
        { XX = 1.0f; YX = skewTangent skewY; XY = skewTangent skewX; YY = 1.0f; X0 = 0.0f; Y0 = 0.0f }

    /// <summary>The skew around a centre, <c>hb_transform_t::skewing_around_center</c>.</summary>
    /// <param name="skewX">The x angle.</param>
    /// <param name="skewY">The y angle.</param>
    /// <param name="cx">The centre's x.</param>
    /// <param name="cy">The centre's y.</param>
    /// <returns>The transform.</returns>
    let private skewingAroundCenter skewX skewY cx cy =
        let tx = skewTangent skewX
        let ty = skewTangent skewY
        { XX = 1.0f
          YX = ty
          XY = tx
          YY = 1.0f
          X0 = match cy <> 0.0f with | true -> -tx * cy | false -> 0.0f
          Y0 = match cx <> 0.0f with | true -> -ty * cx | false -> 0.0f }

    /// <summary>A 2.14 value with its delta, <c>HBFixed::to_float (offset)</c>.</summary>
    /// <param name="raw">The raw value.</param>
    /// <param name="delta">The delta.</param>
    /// <returns>The value.</returns>
    let private fixed2Dot14 raw delta =
        (float32 raw + delta) * F2Dot14Mult

    /// <summary>A 16.16 value with its delta, <c>HBFixed::to_float (offset)</c>.</summary>
    /// <param name="raw">The raw value.</param>
    /// <param name="delta">The delta.</param>
    /// <returns>The value.</returns>
    let private fixed16Dot16 raw delta =
        (float32 raw + delta) * F16Dot16Mult

    /// <summary>The variation index of a variable paint, or no variation for the plain format.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="format">The paint format; the variable ones are odd.</param>
    /// <param name="valueEnd">The offset the variation index follows the fields at.</param>
    /// <param name="at">The offset of the paint.</param>
    /// <returns>The variation index.</returns>
    let private varIdxOf (env: PaintEnv) format valueEnd at =
        match format % 2 with
        | 1 -> Colr.u32 env.Colr.Table (at + valueEnd)
        | _ -> NoVariation

    /// <summary>A variation delta of a paint.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="varIdx">The variation index.</param>
    /// <param name="offset">The offset into the variation index.</param>
    /// <returns>The delta.</returns>
    let private deltaOf (env: PaintEnv) (varIdx: uint32) offset =
        Colr.delta env.Instancer varIdx offset

    /// <summary>Enters a node of the path, <c>hb_decycler_node_t</c>'s constructor.</summary>
    /// <param name="decycler">The path so far.</param>
    /// <returns>The path with the node pushed.</returns>
    let private enter (decycler: PaintDecycler) =
        let awake = not decycler.Awake
        let tortoise =
            match decycler.Values.Length with
            | 0 -> 0
            | _ when awake -> decycler.Tortoise + 1
            | _ -> decycler.Tortoise
        { Values = Sealed.appendOne (decycler.Values.AsSpan()) 0u
          Tortoise = tortoise
          Awake = awake }

    /// <summary>Visits a value at the top node, <c>hb_decycler_node_t::visit</c>.</summary>
    /// <param name="decycler">The path.</param>
    /// <param name="value">The value.</param>
    /// <returns>Whether the value is not on a cycle, and the path with the value set.</returns>
    let private visit (decycler: PaintDecycler) (value: uint32) =
        let top = decycler.Values.Length - 1
        let updated = { decycler with Values = decycler.Values.SetItem(top, value) }
        match decycler.Tortoise = top with
        | true -> struct (true, updated)
        | false -> struct (decycler.Values[decycler.Tortoise] <> value, updated)

    /// <summary>The path before any node.</summary>
    let private noPath = { Values = ImmutableArray<uint32>.Empty; Tortoise = -1; Awake = false }

    /// <summary>A clip rectangle as a box, <c>push_clip_rectangle</c> as <c>COLR::paint_glyph</c> calls it.</summary>
    /// <param name="clip">The clip box in font units.</param>
    /// <returns>The box.</returns>
    let private clipRectangle (clip: GlyphExtents) =
        { XMin = float32 clip.XBearing
          YMin = float32 clip.YBearing + float32 clip.Height
          XMax = float32 clip.XBearing + float32 clip.Width
          YMax = float32 clip.YBearing }

    /// <summary>Whether a paint is a solid colour, <c>Paint::get_solid_color</c>.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="paint">The paint.</param>
    /// <returns>True for the two solid formats.</returns>
    let private isSolid (env: PaintEnv) (paint: ColrNode) =
        match paint with
        | NullNode -> false
        | NodeAt at ->
            match Colr.u8 env.Colr.Table at with
            | FormatSolid
            | FormatSolidVar -> true
            | _ -> false

    /// <summary>Walks one paint with the nesting and edge limits, <c>hb_paint_context_t::recurse</c>.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="paint">The paint.</param>
    /// <returns>The walk.</returns>
    let rec private recurse (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) (paint: ColrNode) =
        match depthLeft <= 0 || walk.Edges <= 0 || budgetSpent walk.Paint with
        | true -> walk
        | false -> dispatch env glyphs layers (depthLeft - 1) { walk with Edges = walk.Edges - 1 } paint

    /// <summary>Walks the layers of a PaintColrLayers one after the other, <c>PaintColrLayers::paint_glyph</c>.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers with the node entered.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="index">The next layer.</param>
    /// <param name="last">The end of the layers, unsigned and wrapping as upstream's sum.</param>
    /// <returns>The walk.</returns>
    and private layersLoop (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) (index: uint32) (last: uint32) =
        match index < last with
        | false -> walk
        | true ->
            let struct (admitted, visited) = visit layers index
            match admitted with
            | false -> walk
            | true ->
                let next = recurse env glyphs visited depthLeft walk (Colr.layerPaint env.Colr index)
                layersLoop env glyphs visited depthLeft next (index + 1u) last

    /// <summary>Walks a paint's source under a transform: push, the source, pop.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="at">The offset of the paint.</param>
    /// <param name="t">The transform.</param>
    /// <returns>The walk.</returns>
    and private underTransform (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) at (t: Transform) =
        let pushed = { walk with Paint = pushTransform walk.Paint t }
        let inner = recurse env glyphs layers depthLeft pushed (Colr.node24 env.Colr.Table (at + SourceOffset) at)
        { inner with Paint = popTransform inner.Paint }

    /// <summary>Walks a PaintGlyph, <c>PaintGlyph::paint_glyph</c>.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="at">The offset of the paint.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The walk.</returns>
    and private paintGlyph (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) at (glyph: uint32) =
        let child = Colr.node24 env.Colr.Table (at + SourceOffset) at
        let font = env.Font
        let inverse = { walk with Paint = pushTransform walk.Paint (inverseFontTransform font) }
        match depthLeft > 0 && walk.Edges > 0 && isSolid env child with
        | true ->
            let filled = { inverse with Edges = inverse.Edges - 1; Paint = fillGlyph font glyph inverse.Paint }
            { filled with Paint = popTransform filled.Paint }
        | false ->
            let clipped = { inverse with Paint = pushClipGlyph font glyph inverse.Paint }
            let scaled = { clipped with Paint = pushTransform clipped.Paint (fontTransform font) }
            let inner = recurse env glyphs layers depthLeft scaled child
            let unscaled = { inner with Paint = popTransform inner.Paint }
            let unclipped = { unscaled with Paint = popClip unscaled.Paint }
            { unclipped with Paint = popTransform unclipped.Paint }

    /// <summary>Walks a PaintColrGlyph, <c>PaintColrGlyph::paint_glyph</c>.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="glyph">The colour glyph to paint.</param>
    /// <returns>The walk.</returns>
    and private paintColrGlyph (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) (glyph: uint32) =
        let struct (admitted, visited) = visit (enter glyphs) glyph
        match admitted with
        | false -> walk
        | true ->
            let clip = Colr.clipBox env.Colr env.Instancer glyph
            let clipped =
                match clip with
                | ValueSome box -> { walk with Paint = pushClipBox walk.Paint (clipRectangle box) }
                | ValueNone -> walk
            let inner =
                match Colr.baseGlyphPaint env.Colr glyph with
                | ValueSome paint -> recurse env visited layers depthLeft clipped paint
                | ValueNone -> clipped
            match clip with
            | ValueSome _ -> { inner with Paint = popClip inner.Paint }
            | ValueNone -> inner

    /// <summary>Walks a PaintComposite, <c>PaintComposite::paint_glyph</c>: the backdrop, then the source, in groups.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="at">The offset of the paint.</param>
    /// <returns>The walk.</returns>
    and private paintComposite (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) at =
        let table = env.Colr.Table
        let modeByte = Colr.u8 table (at + 4)
        let mode = match modeByte <= LastCompositeMode with | true -> modeByte | false -> ModeClear
        let outer = { walk with Paint = pushGroup walk.Paint }
        let backdrop = recurse env glyphs layers depthLeft outer (Colr.node24 table (at + 5) at)
        let inner = { backdrop with Paint = pushGroup backdrop.Paint }
        let source = recurse env glyphs layers depthLeft inner (Colr.node24 table (at + SourceOffset) at)
        let composed = { source with Paint = popGroup mode source.Paint }
        { composed with Paint = popGroup ModeSrcOver composed.Paint }

    /// <summary>Walks a PaintTransform, <c>PaintTransform::paint_glyph</c> and <c>Affine2x3::paint_glyph</c>.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="format">The paint format, 12 or 13.</param>
    /// <param name="at">The offset of the paint.</param>
    /// <returns>The walk.</returns>
    and private paintTransform (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) format at =
        let table = env.Colr.Table
        let target = Colr.node24 table (at + 4) at
        let varIdx =
            match target, format with
            | NullNode, 13 -> 0u
            | NodeAt affine, 13 -> Colr.u32 table (affine + 24)
            | NullNode, _ -> NoVariation
            | NodeAt _, _ -> NoVariation
        let raw (index: int) =
            match target with
            | NullNode -> 0
            | NodeAt affine -> Colr.i32 table (affine + 4 * index)
        let field (index: int) = fixed16Dot16 (raw index) (deltaOf env varIdx index)
        let t = { XX = field 0; YX = field 1; XY = field 2; YY = field 3; X0 = field 4; Y0 = field 5 }
        underTransform env glyphs layers depthLeft walk at t

    /// <summary>Walks one paint, <c>Paint::dispatch</c> with the paint functions of the extents and boundedness contexts.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyphs">The path of colour glyphs.</param>
    /// <param name="layers">The path of layers.</param>
    /// <param name="depthLeft">The nesting left.</param>
    /// <param name="walk">The walk so far.</param>
    /// <param name="paint">The paint.</param>
    /// <returns>The walk.</returns>
    and private dispatch (env: PaintEnv) (glyphs: PaintDecycler) (layers: PaintDecycler) depthLeft (walk: Walk) (paint: ColrNode) =
        match paint with
        | NullNode -> walk
        | NodeAt at ->
            let table = env.Colr.Table
            let format = Colr.u8 table at
            let paintOnly = { walk with Paint = paintClip walk.Paint }
            match format with
            | FormatColrLayers ->
                let first = Colr.u32 table (at + 2)
                let last = first + uint32 (Colr.u8 table (at + 1))
                layersLoop env glyphs (enter layers) depthLeft walk first last
            | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 -> paintOnly
            | 10 -> paintGlyph env glyphs layers depthLeft walk at (uint32 (Colr.u16 table (at + GlyphIdOffset)))
            | 33 -> paintGlyph env glyphs layers depthLeft walk at (uint32 (Colr.u24 table (at + GlyphIdOffset)))
            | 11 -> paintColrGlyph env glyphs layers depthLeft walk (uint32 (Colr.u16 table (at + ColrGlyphIdOffset)))
            | 12 | 13 -> paintTransform env glyphs layers depthLeft walk format at
            | 14 | 15 ->
                let varIdx = varIdxOf env format 8 at
                let dx = float32 (Colr.i16 table (at + 4)) + deltaOf env varIdx 0
                let dy = float32 (Colr.i16 table (at + 6)) + deltaOf env varIdx 1
                underTransform env glyphs layers depthLeft walk at { PaintGeometry.identity with X0 = dx; Y0 = dy }
            | 16 | 17 ->
                let varIdx = varIdxOf env format 8 at
                let sx = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                let sy = fixed2Dot14 (Colr.i16 table (at + 6)) (deltaOf env varIdx 1)
                underTransform env glyphs layers depthLeft walk at { PaintGeometry.identity with XX = sx; YY = sy }
            | 18 | 19 ->
                let varIdx = varIdxOf env format 12 at
                let sx = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                let sy = fixed2Dot14 (Colr.i16 table (at + 6)) (deltaOf env varIdx 1)
                let cx = float32 (Colr.i16 table (at + 8)) + deltaOf env varIdx 2
                let cy = float32 (Colr.i16 table (at + 10)) + deltaOf env varIdx 3
                underTransform env glyphs layers depthLeft walk at (scalingAroundCenter sx sy cx cy)
            | 20 | 21 ->
                let varIdx = varIdxOf env format 6 at
                let s = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                underTransform env glyphs layers depthLeft walk at { PaintGeometry.identity with XX = s; YY = s }
            | 22 | 23 ->
                let varIdx = varIdxOf env format 10 at
                let s = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                let cx = float32 (Colr.i16 table (at + 6)) + deltaOf env varIdx 1
                let cy = float32 (Colr.i16 table (at + 8)) + deltaOf env varIdx 2
                underTransform env glyphs layers depthLeft walk at (scalingAroundCenter s s cx cy)
            | 24 | 25 ->
                let varIdx = varIdxOf env format 6 at
                let angle = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                underTransform env glyphs layers depthLeft walk at (rotation (angle * Pi))
            | 26 | 27 ->
                let varIdx = varIdxOf env format 10 at
                let angle = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                let cx = float32 (Colr.i16 table (at + 6)) + deltaOf env varIdx 1
                let cy = float32 (Colr.i16 table (at + 8)) + deltaOf env varIdx 2
                underTransform env glyphs layers depthLeft walk at (rotationAroundCenter (angle * Pi) cx cy)
            | 28 | 29 ->
                let varIdx = varIdxOf env format 8 at
                let sx = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                let sy = fixed2Dot14 (Colr.i16 table (at + 6)) (deltaOf env varIdx 1)
                underTransform env glyphs layers depthLeft walk at (skewing (-sx * Pi) (sy * Pi))
            | 30 | 31 ->
                let varIdx = varIdxOf env format 12 at
                let sx = fixed2Dot14 (Colr.i16 table (at + 4)) (deltaOf env varIdx 0)
                let sy = fixed2Dot14 (Colr.i16 table (at + 6)) (deltaOf env varIdx 1)
                let cx = float32 (Colr.i16 table (at + 8)) + deltaOf env varIdx 2
                let cy = float32 (Colr.i16 table (at + 10)) + deltaOf env varIdx 3
                underTransform env glyphs layers depthLeft walk at (skewingAroundCenter (-sx * Pi) (sy * Pi) cx cy)
            | 32 -> paintComposite env glyphs layers depthLeft walk at
            | _ -> walk

    /// <summary>Walks a colour glyph's v1 paint from the top, <c>COLR::paint_glyph</c> without clip: the font transform, the paint, the pop.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="paint">The glyph's paint.</param>
    /// <param name="start">The state to begin with.</param>
    /// <param name="paintIt">Whether the paint is walked, false for an unbounded glyph.</param>
    /// <returns>The state.</returns>
    let private paintVersion1 (env: PaintEnv) (glyph: uint32) (paint: ColrNode) (start: PaintState) paintIt =
        let struct (_, glyphs) = visit (enter noPath) glyph
        let pushed = { Paint = pushTransform start (fontTransform env.Font); Edges = MaxGraphEdgeCount }
        let walked =
            match paintIt with
            | true -> recurse env glyphs noPath MaxNestingLevel pushed paint
            | false -> pushed
        popTransform walked.Paint

    /// <summary>Paints the layers of a version 0 glyph one after the other, <c>COLR::paint_glyph</c>'s version 0 branch.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="index">The next layer.</param>
    /// <param name="last">The end of the layers.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    [<TailCall>]
    let rec private paintLayers (env: PaintEnv) index last (state: PaintState) =
        match index >= last with
        | true -> state
        | false ->
            let layerGlyph = uint32 (Colr.u16 env.Colr.Table (env.Colr.LayersAt + index * Colr.LayerRecordSize))
            paintLayers env (index + 1) last (fillGlyph env.Font layerGlyph state)

    /// <summary>The extents a finished walk leaves, <c>hb_paint_extents_context_t::get_extents</c> and the void check of <c>COLR::get_extents</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The glyph extents.</returns>
    let private resultOf (state: PaintState) =
        match state with
        | BoundedState _ -> { XBearing = 0; YBearing = 0; Width = 0; Height = 0 }
        | ExtentsState (_, _, groups, _, _) ->
            let box = (headOr emptyGroup groups).Box
            match PaintGeometry.isVoid box with
            | true -> { XBearing = 0; YBearing = 0; Width = 0; Height = 0 }
            | false -> PaintGeometry.toGlyphExtents box

    /// <summary>Scales extents into the font's scale, <c>hb_font_t::scale_glyph_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="extents">The extents in font units.</param>
    /// <returns>The scaled extents.</returns>
    let private scaleGlyphExtents (font: Font) (extents: GlyphExtents) =
        let x1 = Font.emFscaleX font extents.XBearing
        let y1 = Font.emFscaleY font extents.YBearing
        let x2 = x1 + Font.emFscaleX font extents.Width
        let y2 = y1 + Font.emFscaleY font extents.Height
        let xBearing = Fixed.clampToInt32 (double (MathF.Floor x1))
        let yBearing = Fixed.clampToInt32 (double (MathF.Floor y1))
        { XBearing = xBearing
          YBearing = yBearing
          Width = Fixed.clampToInt32 (double (MathF.Ceiling x2) - double xBearing)
          Height = Fixed.clampToInt32 (double (MathF.Ceiling y2) - double yBearing) }

    /// <summary>Whether a version 1 glyph's paint graph is bounded, the boundedness pass of <c>COLR::paint_glyph</c>.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="paint">The glyph's paint.</param>
    /// <returns>True when bounded.</returns>
    let private isBounded (env: PaintEnv) (glyph: uint32) (paint: ColrNode) =
        match paintVersion1 env glyph paint freshBounded true with
        | BoundedState (bounded, _, _) -> bounded
        | ExtentsState _ -> false

    /// <summary>The extents of a version 0 glyph, the layers of its base glyph record painted one after the other.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the glyph has no record.</returns>
    let private glyphVersion0 (env: PaintEnv) (glyph: uint32) =
        match Colr.baseGlyphRecord env.Colr glyph with
        | ValueNone -> ValueNone
        | ValueSome (struct (firstLayer, layerCount)) ->
            let first = min firstLayer env.Colr.NumLayers
            let last = min (firstLayer + layerCount) env.Colr.NumLayers
            ValueSome (resultOf (paintLayers env first last freshExtents))

    /// <summary>The extents of a glyph without a clip box, <c>COLR::paint_glyph</c> run with the extents context.</summary>
    /// <param name="env">The walk's inputs.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the table paints nothing for the glyph.</returns>
    let private glyphUnclipped (env: PaintEnv) (glyph: uint32) =
        match Colr.baseGlyphPaint env.Colr glyph with
        | ValueSome paint ->
            let bounded = isBounded env glyph paint
            ValueSome (resultOf (paintVersion1 env glyph paint freshExtents bounded))
        | ValueNone -> glyphVersion0 env glyph

    /// <summary>The extents of a COLR glyph, <c>COLR::get_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="colr">The table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the table does not answer for the glyph.</returns>
    let glyph (font: Font) (colr: Colr) (glyph: uint32) =
        let instancer = Colr.instancer colr font.Variations.Coords
        match Colr.hasData colr, Colr.clipBox colr instancer glyph with
        | false, _ -> ValueNone
        | true, ValueSome clip -> ValueSome (scaleGlyphExtents font clip)
        | true, ValueNone -> glyphUnclipped { Colr = colr; Font = font; Instancer = instancer } glyph
