namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.OpenType
open Farfbuzz.Var

/// <summary>Assembling a glyph's contour points and phantoms, with gvar deltas, <c>OT/glyf/Glyph.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal GlyphOutline =
    /// <summary>The maximum composite nesting depth, <c>HB_MAX_NESTING_LEVEL</c>.</summary>
    [<Literal>]
    let private MaxNestingLevel = 64

    /// <summary>The maximum component graph edge count, <c>HB_MAX_GRAPH_EDGE_COUNT</c>.</summary>
    [<Literal>]
    let private MaxGraphEdgeCount = 16384

    /// <summary>The maximum contour points of one assembled glyph, <c>HB_GLYF_MAX_POINTS</c>.</summary>
    [<Literal>]
    let private MaxGlyfPoints = 200000

    /// <summary>The per-glyph work budget, <c>HB_BUDGET_GLYPH</c>.</summary>
    [<Literal>]
    let private BudgetGlyph = 16777216L

    /// <summary>The fixed cost of one glyph or component traversal, <c>HB_BUDGET_16</c>.</summary>
    [<Literal>]
    let private Budget16 = 16

    /// <summary>The number of phantom points, <c>PHANTOM_COUNT</c>.</summary>
    [<Literal>]
    let private PhantomCount = 4

    /// <summary>The size of one glyph header, <c>GlyphHeader::static_size</c>.</summary>
    [<Literal>]
    let private HeaderSize = 10

    /// <summary>The advance clamp ceiling, <c>(float) UINT_MAX / 2</c>.</summary>
    [<Literal>]
    let private MaxAdvanceFloat = 2147483648.0f

    /// <summary>The arguments-are-words composite flag, <c>ARG_1_AND_2_ARE_WORDS</c>.</summary>
    [<Literal>]
    let private ArgWords = 0x0001us

    /// <summary>The arguments-are-xy-values composite flag, <c>ARGS_ARE_XY_VALUES</c>.</summary>
    [<Literal>]
    let private ArgsXy = 0x0002us

    /// <summary>The have-a-scale composite flag, <c>WE_HAVE_A_SCALE</c>.</summary>
    [<Literal>]
    let private HaveScale = 0x0008us

    /// <summary>The more-components composite flag, <c>MORE_COMPONENTS</c>.</summary>
    [<Literal>]
    let private MoreComponents = 0x0020us

    /// <summary>The have-an-x-and-y-scale composite flag, <c>WE_HAVE_AN_X_AND_Y_SCALE</c>.</summary>
    [<Literal>]
    let private HaveXYScale = 0x0040us

    /// <summary>The have-a-two-by-two composite flag, <c>WE_HAVE_A_TWO_BY_TWO</c>.</summary>
    [<Literal>]
    let private Have2x2 = 0x0080us

    /// <summary>The use-my-metrics composite flag, <c>USE_MY_METRICS</c>.</summary>
    [<Literal>]
    let private UseMyMetrics = 0x0200us

    /// <summary>The scaled-component-offset composite flag, <c>SCALED_COMPONENT_OFFSET</c>.</summary>
    [<Literal>]
    let private ScaledComponentOffset = 0x0800us

    /// <summary>The unscaled-component-offset composite flag, <c>UNSCALED_COMPONENT_OFFSET</c>.</summary>
    [<Literal>]
    let private UnscaledComponentOffset = 0x1000us

    /// <summary>The 24-bit glyph id composite flag, <c>GID_IS_24BIT</c>.</summary>
    [<Literal>]
    let private Gid24 = 0x2000us

    /// <summary>The minimum size of one composite record, flags plus a 16-bit glyph id.</summary>
    [<Literal>]
    let private CompositeMinSize = 4

    /// <summary>The fixed inputs of one glyph assembly, constant across the recursion.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private Context =
        { Glyf: GlyfFace
          Gvar: Gvar
          Coords: NormalizedCoords
          PhantomOnly: bool
          Extended: bool }

    /// <summary>The cycle detector path, <c>hb_decycler_t</c>: the tortoise-and-hare over the current composite path.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private Decycler =
        { Values: ImmutableArray<uint32>
          Tortoise: int
          Awake: bool }

    /// <summary>The mutable state of one composite component walk.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private CompositeState =
        { AllPoints: ImmutableArray<ContourPoint>
          CompPoints: ImmutableArray<ContourPoint>
          Decycler: Decycler
          EdgeCount: int
          Budget: int64
          Failed: bool }

    /// <summary>The no-path cycle detector.</summary>
    let private decyclerCreate : Decycler =
        { Values = ImmutableArray<uint32>.Empty
          Tortoise = -1
          Awake = false }

    /// <summary>Pushes one composite node onto the path, <c>hb_decycler_node_t</c>'s constructor.</summary>
    /// <param name="decycler">The path so far.</param>
    /// <returns>The path with the node pushed.</returns>
    let private decyclerEnter (decycler: Decycler) : Decycler =
        let awake = not decycler.Awake
        let tortoise =
            match decycler.Values.Length with
            | 0 -> 0
            | _ when awake -> decycler.Tortoise + 1
            | _ -> decycler.Tortoise
        { Values = Sealed.appendOne (decycler.Values.AsSpan()) 0u
          Tortoise = tortoise
          Awake = awake }

    /// <summary>Visits one component glyph id, returning whether it is not on the current path.</summary>
    /// <param name="decycler">The path.</param>
    /// <param name="value">The component glyph id.</param>
    /// <returns>Whether the id is admitted, and the path with the node's value set.</returns>
    let private decyclerVisit (decycler: Decycler) (value: uint32) : struct (bool * Decycler) =
        let top = decycler.Values.Length - 1
        let updated = { decycler with Values = decycler.Values.SetItem(top, value) }
        match decycler.Tortoise = top with
        | true -> struct (true, updated)
        | false ->
            match decycler.Values[decycler.Tortoise] = value with
            | true -> struct (false, updated)
            | false -> struct (true, updated)

    /// <summary>The zero glyph header of the empty glyph, <c>Glyph ()</c>'s Null header.</summary>
    let private zeroHeader : GlyphHeader =
        { NumberOfContours = 0s
          XMin = 0s
          YMin = 0s
          XMax = 0s
          YMax = 0s }

    /// <summary>Resolves one glyph id to its bytes, kind, header and effective id, <c>glyph_for_gid</c>.</summary>
    /// <param name="glyf">The accelerator.</param>
    /// <param name="gid">The glyph id.</param>
    /// <returns>The glyph bytes, kind, header and the id the phantoms use (0xFFFFFFFF for the empty glyph).</returns>
    let private glyphForGid (glyf: GlyfFace) (gid: uint32) : struct (ReadOnlyMemory<byte> * GlyphKind * GlyphHeader * uint32) =
        match uint64 gid < uint64 glyf.NumGlyphs with
        | false -> struct (ReadOnlyMemory<byte>.Empty, EmptyGlyph, zeroHeader, 0xFFFFFFFFu)
        | true ->
            let span = glyf.Loca.Span
            let startU, finishU =
                match glyf.ShortOffsets with
                | true -> uint32 (int (Binary.u16 span (int gid * 2)) * 2), uint32 (int (Binary.u16 span (int gid * 2 + 2)) * 2)
                | false -> Binary.u32 span (int gid * 4), Binary.u32 span (int gid * 4 + 4)
            match startU > finishU || uint64 finishU > uint64 glyf.Glyf.Length with
            | true -> struct (ReadOnlyMemory<byte>.Empty, EmptyGlyph, zeroHeader, 0xFFFFFFFFu)
            | false ->
                let bytes = glyf.Glyf.Slice(int startU, int (finishU - startU))
                match bytes.Length < HeaderSize with
                | true -> struct (bytes, EmptyGlyph, zeroHeader, gid)
                | false ->
                    let span = bytes.Span
                    let header =
                        { NumberOfContours = Binary.i16 span 0
                          XMin = Binary.i16 span 2
                          YMin = Binary.i16 span 4
                          XMax = Binary.i16 span 6
                          YMax = Binary.i16 span 8 }
                    struct (bytes, GlyphPoints.kind span, header, gid)

    /// <summary>The byte size of one composite record, <c>CompositeGlyphRecord::get_size</c>.</summary>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="flags">The record flags.</param>
    /// <returns>The record size.</returns>
    let private componentSizeRec (extended: bool) (flags: uint16) : int =
        let glyphIdExtra = match extended && flags &&& Gid24 <> 0us with | true -> 1 | false -> 0
        let argBytes = match flags &&& ArgWords <> 0us with | true -> 4 | false -> 2
        let matrixBytes =
            match flags &&& HaveScale <> 0us, flags &&& HaveXYScale <> 0us, flags &&& Have2x2 <> 0us with
            | true, _, _ -> 2
            | false, true, _ -> 4
            | false, false, true -> 8
            | false, false, false -> 0
        CompositeMinSize + glyphIdExtra + argBytes + matrixBytes

    /// <summary>The byte width of one component's glyph id.</summary>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="flags">The record flags.</param>
    /// <returns>The glyph id byte count.</returns>
    let private glyphIdBytes (extended: bool) (flags: uint16) : int =
        match extended && flags &&& Gid24 <> 0us with
        | true -> 3
        | false -> 2

    /// <summary>Reads one component's anchor points, the raw unsigned argument bytes, <c>get_anchor_points</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="argAt">The argument offset.</param>
    /// <param name="flags">The record flags.</param>
    /// <returns>The two anchor point indices.</returns>
    let private readAnchor (glyph: ReadOnlySpan<byte>) (argAt: int) (flags: uint16) : struct (uint32 * uint32) =
        match flags &&& ArgWords <> 0us with
        | true -> struct (uint32 (Binary.u16 glyph argAt), uint32 (Binary.u16 glyph (argAt + 2)))
        | false -> struct (uint32 (Binary.u8 glyph argAt), uint32 (Binary.u8 glyph (argAt + 1)))

    /// <summary>Walks the composite records, collecting each anchor pair, stopping at the first record out of range.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="at">The next record's byte offset.</param>
    /// <param name="builder">The anchors built so far.</param>
    /// <returns>The completed builder.</returns>
    [<TailCall>]
    let rec private anchorPointsFrom (glyph: ReadOnlySpan<byte>) (extended: bool) (at: int) (builder: PointBuilder<struct (uint32 * uint32)>) : PointBuilder<struct (uint32 * uint32)> =
        match at + CompositeMinSize > glyph.Length with
        | true -> builder
        | false ->
            let flags = Binary.u16 glyph at
            let size = componentSizeRec extended flags
            match at + size > glyph.Length with
            | true -> builder
            | false ->
                let argAt = at + 2 + glyphIdBytes extended flags
                let builder = PointBuilder.append builder (readAnchor glyph argAt flags)
                match flags &&& MoreComponents <> 0us with
                | true -> anchorPointsFrom glyph extended (at + size) builder
                | false -> builder

    /// <summary>Reads the anchor points of a composite glyph, <c>CompositeGlyphRecord::get_anchor_points</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <returns>The anchor pairs, one per component.</returns>
    let private anchorPoints (glyph: ReadOnlySpan<byte>) (extended: bool) : ImmutableArray<struct (uint32 * uint32)> =
        PointBuilder.publish (anchorPointsFrom glyph extended HeaderSize (PointBuilder.create()))

    /// <summary>Applies the gvar deltas to a glyph's points when the coordinates carry a non-zero value.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="phantomOnly">Whether only the last four points receive deltas.</param>
    /// <param name="points">The points, phantoms last.</param>
    /// <returns>The points with deltas, unchanged without coordinates, or none where the data fails.</returns>
    let private applyDeltas (ctx: Context) (gid: uint32) (phantomOnly: bool) (points: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> voption =
        match ctx.Coords.HasNonZero with
        | false -> ValueSome points
        | true -> Gvar.applyDeltas ctx.Gvar (ctx.Coords.AsSpan()) gid phantomOnly points

    /// <summary>Subtracts the left phantom x at the top level, the rasterizer's horizontal shift.</summary>
    /// <param name="depth">The recursion depth; only zero shifts.</param>
    /// <param name="phantomOnly">Whether only the last four points shift.</param>
    /// <param name="shift">The left phantom x.</param>
    /// <param name="points">The points.</param>
    /// <returns>The shifted points.</returns>
    let private applyTopShift (depth: int) (phantomOnly: bool) (shift: float32) (points: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> =
        match depth = 0 && shift <> 0.0f with
        | false -> points
        | true ->
            let start = if phantomOnly && points.Length >= PhantomCount then points.Length - PhantomCount else 0
            Sealed.mapi (fun index point -> if index >= start then { point with X = point.X - shift } else point) (points.AsSpan())

    /// <summary>Transforms a run of points by a component matrix, <c>CompositeGlyphRecord::transform</c>.</summary>
    /// <param name="m0">The matrix entry.</param>
    /// <param name="m1">The matrix entry.</param>
    /// <param name="m2">The matrix entry.</param>
    /// <param name="m3">The matrix entry.</param>
    /// <param name="source">The points.</param>
    /// <returns>The transformed points.</returns>
    let private transformMatrix (m0: float32) (m1: float32) (m2: float32) (m3: float32) (source: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> =
        match m0 = 1.0f && m1 = 0.0f && m2 = 0.0f && m3 = 1.0f with
        | true -> source
        | false -> Sealed.map (fun point -> let oldX = point.X in { point with X = oldX * m0 + point.Y * m2; Y = oldX * m1 + point.Y * m3 }) (source.AsSpan())

    /// <summary>Translates a run of points, skipping a zero offset on each axis, <c>CompositeGlyphRecord::translate</c>.</summary>
    /// <param name="trans">The translation.</param>
    /// <param name="source">The points.</param>
    /// <returns>The translated points.</returns>
    let private translatePoints (trans: ContourPoint) (source: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> =
        match trans.X <> 0.0f, trans.Y <> 0.0f with
        | false, false -> source
        | true, false -> Sealed.map (fun point -> { point with X = point.X + trans.X }) (source.AsSpan())
        | false, true -> Sealed.map (fun point -> { point with Y = point.Y + trans.Y }) (source.AsSpan())
        | true, true -> Sealed.map (fun point -> { point with X = point.X + trans.X; Y = point.Y + trans.Y }) (source.AsSpan())

    /// <summary>Applies a component transform and translation, <c>CompositeGlyphRecord::transform_points</c>.</summary>
    /// <param name="comp">The component.</param>
    /// <param name="trans">The offset point carrying its deltas.</param>
    /// <param name="points">The child points.</param>
    /// <returns>The transformed points.</returns>
    let private transformPoints (comp: Component) (trans: ContourPoint) (points: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> =
        let scaledOffsets = (comp.ComponentFlags &&& (ScaledComponentOffset ||| UnscaledComponentOffset)) = ScaledComponentOffset
        match scaledOffsets with
        | true -> transformMatrix comp.M0 comp.M1 comp.M2 comp.M3 (translatePoints trans points)
        | false -> translatePoints trans (transformMatrix comp.M0 comp.M1 comp.M2 comp.M3 points)

    /// <summary>Translates an anchored component so its second anchor lands on the parent's first, <c>translate</c>.</summary>
    /// <param name="allPoints">The parent's accumulated points.</param>
    /// <param name="p1">The parent anchor point.</param>
    /// <param name="p2">The child anchor point.</param>
    /// <param name="transformed">The transformed child points.</param>
    /// <returns>The translated child points.</returns>
    let private anchorTranslate (allPoints: ImmutableArray<ContourPoint>) (p1: uint32) (p2: uint32) (transformed: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> =
        match int p1 < allPoints.Length && int p2 < transformed.Length with
        | false -> transformed
        | true ->
            let delta =
                { X = allPoints[int p1].X - transformed[int p2].X
                  Y = allPoints[int p1].Y - transformed[int p2].Y
                  Flag = 0uy
                  IsEndPoint = false }
            translatePoints delta transformed

    /// <summary>Copies a child's phantoms over the last four of a composite's own points, <c>USE_MY_METRICS</c>.</summary>
    /// <param name="compPoints">The composite's offsets and phantoms.</param>
    /// <param name="childAll">The child's points.</param>
    /// <returns>The composite points with the child phantoms.</returns>
    let private copyPhantoms (compPoints: ImmutableArray<ContourPoint>) (childAll: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> =
        let offsetCount = compPoints.Length - PhantomCount
        let prefix = Sealed.init offsetCount (fun index -> compPoints[index])
        let phantoms = Sealed.init PhantomCount (fun index -> childAll[childAll.Length - PhantomCount + index])
        Sealed.concat prefix phantoms

    /// <summary>Finishes one admitted component: transforms, anchors and appends its contour points.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="anchors">The component anchor pairs.</param>
    /// <param name="comp">The component.</param>
    /// <param name="state">The walk state.</param>
    /// <param name="decycler">The path after the visit.</param>
    /// <param name="compIndex">The component index.</param>
    /// <param name="childAll">The child's contour points and phantoms.</param>
    /// <param name="edgeCount">The edge count after the child.</param>
    /// <param name="budget">The budget after the child.</param>
    /// <param name="compPoints">The composite's points, phantoms possibly overwritten.</param>
    /// <returns>The updated state.</returns>
    let private finishComponent (ctx: Context) (anchors: ImmutableArray<struct (uint32 * uint32)>) (comp: Component) (state: CompositeState) (decycler: Decycler) (compIndex: int) (childAll: ImmutableArray<ContourPoint>) (edgeCount: int) (budget: int64) (compPoints: ImmutableArray<ContourPoint>) : CompositeState =
        let childContour =
            match ctx.PhantomOnly with
            | true -> ImmutableArray<ContourPoint>.Empty
            | false ->
                let trans = state.CompPoints[compIndex]
                let transformed = transformPoints comp trans childAll
                let translated =
                    match comp.ComponentFlags &&& ArgsXy = 0us with
                    | false -> transformed
                    | true ->
                        let struct (anchor1, anchor2) = anchors[compIndex]
                        anchorTranslate state.AllPoints anchor1 anchor2 transformed
                Sealed.init (translated.Length - PhantomCount) (fun index -> translated[index])
        let allPoints = Sealed.concat state.AllPoints childContour
        match allPoints.Length > MaxGlyfPoints with
        | true -> { state with Failed = true }
        | false ->
            { state with
                AllPoints = allPoints
                CompPoints = compPoints
                Decycler = decycler
                EdgeCount = edgeCount
                Budget = budget }

    /// <summary>Assembles a simple glyph: contour points, phantoms and deltas.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="depth">The recursion depth.</param>
    /// <param name="edgeCount">The edge count so far.</param>
    /// <param name="budget">The budget so far.</param>
    /// <param name="contour">The contour points.</param>
    /// <param name="phantomArr">The four phantom points.</param>
    /// <returns>The assembled points, edge count and budget.</returns>
    let private assembleSimple (ctx: Context) (gid: uint32) (depth: int) (edgeCount: int) (budget: int64) (contour: ImmutableArray<ContourPoint>) (phantomArr: ImmutableArray<ContourPoint>) : struct (ImmutableArray<ContourPoint> * int * int64) voption =
        let budget = budget - int64 contour.Length
        match budget >= 0L with
        | false -> ValueNone
        | true ->
            let combined = Sealed.concat contour phantomArr
            match applyDeltas ctx gid ctx.PhantomOnly combined with
            | ValueNone -> ValueNone
            | ValueSome deltas -> ValueSome (struct (applyTopShift depth ctx.PhantomOnly deltas[deltas.Length - PhantomCount].X deltas, edgeCount, budget))

    /// <summary>Finds the last admitted use-my-metrics component, the phantom-only pre-scan.</summary>
    /// <param name="components">The component records.</param>
    /// <param name="state">The metrics item and path so far.</param>
    /// <param name="index">The component index.</param>
    /// <returns>The updated state.</returns>
    let private preScanStep (components: ImmutableArray<Component>) (state: struct (int * Decycler)) (index: int) : struct (int * Decycler) =
        let struct (metricsItem, decycler) = state
        let comp = components[index]
        match comp.ComponentFlags &&& UseMyMetrics <> 0us with
        | false -> state
        | true ->
            let struct (admitted, decycler) = decyclerVisit decycler comp.GlyphId
            match admitted with
            | true -> struct (index, decycler)
            | false -> struct (metricsItem, decycler)

    /// <summary>Descends into one admitted component, applying its budget charge, then finishes it.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="anchors">The component anchor pairs.</param>
    /// <param name="comp">The component.</param>
    /// <param name="copyMetrics">Whether to copy the child phantoms.</param>
    /// <param name="depth">The recursion depth.</param>
    /// <param name="state">The walk state.</param>
    /// <param name="decycler">The path after the visit.</param>
    /// <param name="compIndex">The component index.</param>
    /// <returns>The updated state.</returns>
    let rec private descendComponent (ctx: Context) (anchors: ImmutableArray<struct (uint32 * uint32)>) (comp: Component) (copyMetrics: bool) (depth: int) (state: CompositeState) (decycler: Decycler) (compIndex: int) : CompositeState =
        let struct (childBytes, childKind, childHeader, childGid) = glyphForGid ctx.Glyf comp.GlyphId
        match getPoints ctx childBytes childKind childHeader childGid (depth + 1) state.EdgeCount state.Budget decycler with
        | ValueNone -> { state with Failed = true }
        | ValueSome struct (childAll, edgeCount, budget) ->
            let compPoints =
                match copyMetrics with
                | false -> state.CompPoints
                | true -> copyPhantoms state.CompPoints childAll
            let budget = budget - int64 childAll.Length
            match budget >= 0L with
            | false -> { state with Failed = true }
            | true -> finishComponent ctx anchors comp state decycler compIndex childAll edgeCount budget compPoints

    /// <summary>Processes one composite component, <c>Glyph::get_points</c>' component loop.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="components">The component records.</param>
    /// <param name="anchors">The component anchor pairs.</param>
    /// <param name="metricsItem">The last admitted use-my-metrics component, in phantom-only mode.</param>
    /// <param name="depth">The recursion depth.</param>
    /// <param name="state">The walk state.</param>
    /// <param name="compIndex">The component index.</param>
    /// <returns>The updated state.</returns>
    and private compositeStep (ctx: Context) (components: ImmutableArray<Component>) (anchors: ImmutableArray<struct (uint32 * uint32)>) (metricsItem: int) (depth: int) (state: CompositeState) (compIndex: int) : CompositeState =
        match state.Failed with
        | true -> state
        | false ->
            let comp = components[compIndex]
            let struct (admitted, decycler) = decyclerVisit state.Decycler comp.GlyphId
            let copyMetrics = comp.ComponentFlags &&& UseMyMetrics <> 0us && (not ctx.PhantomOnly || compIndex = metricsItem)
            match admitted, ctx.PhantomOnly && not copyMetrics with
            | false, _ -> { state with Decycler = decycler }
            | true, true -> { state with Decycler = decycler }
            | true, false -> descendComponent ctx anchors comp copyMetrics depth state decycler compIndex

    /// <summary>Descends through a composite's components and appends its phantoms.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="components">The component records.</param>
    /// <param name="anchors">The component anchor pairs.</param>
    /// <param name="depth">The recursion depth.</param>
    /// <param name="edgeCount">The edge count so far.</param>
    /// <param name="budget">The budget so far.</param>
    /// <param name="decycler">The path so far.</param>
    /// <param name="compPoints">The composite's offsets and phantoms, deltas applied.</param>
    /// <returns>The assembled points, edge count and budget.</returns>
    and private descendComposite (ctx: Context) (components: ImmutableArray<Component>) (anchors: ImmutableArray<struct (uint32 * uint32)>) (depth: int) (edgeCount: int) (budget: int64) (decycler: Decycler) (compPoints: ImmutableArray<ContourPoint>) : struct (ImmutableArray<ContourPoint> * int * int64) voption =
        let decycler = decyclerEnter decycler
        let struct (metricsItem, decycler) =
            match ctx.PhantomOnly with
            | false -> struct (-1, decycler)
            | true -> Ranges.fold 0 components.Length (preScanStep components) (struct (-1, decycler))
        let initialState =
            { AllPoints = ImmutableArray<ContourPoint>.Empty
              CompPoints = compPoints
              Decycler = decycler
              EdgeCount = edgeCount
              Budget = budget
              Failed = false }
        let final = Ranges.fold 0 components.Length (compositeStep ctx components anchors metricsItem depth) initialState
        match final.Failed with
        | true -> ValueNone
        | false ->
            let offsetCount = components.Length
            let phantoms = Sealed.init PhantomCount (fun index -> final.CompPoints[offsetCount + index])
            let allPoints = Sealed.concat final.AllPoints phantoms
            let shifted = applyTopShift depth ctx.PhantomOnly phantoms[0].X allPoints
            ValueSome (struct (shifted, final.EdgeCount, final.Budget))

    /// <summary>Assembles a composite glyph: offsets, phantoms, deltas, then the component descent.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="bytes">The glyph bytes.</param>
    /// <param name="components">The component records.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="depth">The recursion depth.</param>
    /// <param name="edgeCount">The edge count so far.</param>
    /// <param name="budget">The budget so far.</param>
    /// <param name="decycler">The path so far.</param>
    /// <param name="phantomArr">The four phantom points.</param>
    /// <returns>The assembled points, edge count and budget.</returns>
    and private assembleComposite (ctx: Context) (bytes: ReadOnlyMemory<byte>) (components: ImmutableArray<Component>) (gid: uint32) (depth: int) (edgeCount: int) (budget: int64) (decycler: Decycler) (phantomArr: ImmutableArray<ContourPoint>) : struct (ImmutableArray<ContourPoint> * int * int64) voption =
        let anchors = anchorPoints bytes.Span ctx.Extended
        let offsets = Sealed.init components.Length (fun index -> { X = components[index].Offset.X; Y = components[index].Offset.Y; Flag = 0uy; IsEndPoint = true })
        let combined = Sealed.concat offsets phantomArr
        match applyDeltas ctx gid false combined with
        | ValueNone -> ValueNone
        | ValueSome compPoints -> descendComposite ctx components anchors depth edgeCount budget decycler compPoints

    /// <summary>Loads one glyph's points, phantoms and deltas, then assembles it by kind.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="bytes">The glyph bytes.</param>
    /// <param name="kind">The glyph kind.</param>
    /// <param name="header">The glyph header.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="depth">The recursion depth.</param>
    /// <param name="edgeCount">The edge count so far.</param>
    /// <param name="budget">The budget so far.</param>
    /// <param name="decycler">The path so far.</param>
    /// <returns>The assembled points, edge count and budget.</returns>
    and private loadPoints (ctx: Context) (bytes: ReadOnlyMemory<byte>) (kind: GlyphKind) (header: GlyphHeader) (gid: uint32) (depth: int) (edgeCount: int) (budget: int64) (decycler: Decycler) : struct (ImmutableArray<ContourPoint> * int * int64) voption =
        let span = bytes.Span
        let struct (left, right, top, bottom) = GlyphPoints.phantoms ctx.Glyf gid header
        let phantomArr = Sealed.init PhantomCount (fun index -> match index with | 0 -> left | 1 -> right | 2 -> top | _ -> bottom)
        match kind with
        | EmptyGlyph ->
            match applyDeltas ctx gid false phantomArr with
            | ValueNone -> ValueNone
            | ValueSome phantoms -> ValueSome (struct (applyTopShift depth ctx.PhantomOnly phantoms[0].X phantoms, edgeCount, budget))
        | SimpleGlyph _ ->
            match GlyphPoints.simplePoints span ctx.Extended ctx.PhantomOnly with
            | ValueNone -> ValueNone
            | ValueSome contour -> assembleSimple ctx gid depth edgeCount budget contour phantomArr
        | CompositeGlyph ->
            let components = GlyphPoints.components span ctx.Extended
            let budget = budget - int64 Budget16 * int64 components.Length
            match budget >= 0L with
            | false -> ValueNone
            | true -> assembleComposite ctx bytes components gid depth edgeCount budget decycler phantomArr

    /// <summary>Recursively assembles one glyph, <c>Glyph::get_points</c>.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="bytes">The glyph bytes.</param>
    /// <param name="kind">The glyph kind.</param>
    /// <param name="header">The glyph header.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="depth">The recursion depth.</param>
    /// <param name="edgeCount">The edge count so far.</param>
    /// <param name="budget">The budget so far.</param>
    /// <param name="decycler">The path so far.</param>
    /// <returns>The assembled points, edge count and budget.</returns>
    and private getPoints (ctx: Context) (bytes: ReadOnlyMemory<byte>) (kind: GlyphKind) (header: GlyphHeader) (gid: uint32) (depth: int) (edgeCount: int) (budget: int64) (decycler: Decycler) : struct (ImmutableArray<ContourPoint> * int * int64) voption =
        match depth > MaxNestingLevel || edgeCount > MaxGraphEdgeCount with
        | true -> ValueNone
        | false ->
            let edgeCount = edgeCount + 1
            let budget = budget - int64 Budget16
            match budget >= 0L with
            | false -> ValueNone
            | true -> loadPoints ctx bytes kind header gid depth edgeCount budget decycler

    /// <summary>Assembles one glyph with a fresh budget and cycle detector, <c>glyph_for_gid</c>'s caller.</summary>
    /// <param name="ctx">The assembly inputs.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The assembled points, edge count and budget.</returns>
    let private runPoints (ctx: Context) (glyph: uint32) : struct (ImmutableArray<ContourPoint> * int * int64) voption =
        let struct (bytes, kind, header, gid) = glyphForGid ctx.Glyf glyph
        getPoints ctx bytes kind header gid 0 0 BudgetGlyph decyclerCreate

    /// <summary>Assembles a glyph's points and phantoms, gvar deltas applied, as <c>Glyph::get_points</c>.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="phantomOnly">Whether only the phantom points are wanted.</param>
    /// <returns>The points, phantoms last, or none where the glyph fails to load.</returns>
    let points (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (glyph: uint32) (phantomOnly: bool) : ImmutableArray<ContourPoint> voption =
        let ctx = { Glyf = glyf; Gvar = gvar; Coords = coords; PhantomOnly = phantomOnly; Extended = glyf.Extended }
        match runPoints ctx glyph with
        | ValueNone -> ValueNone
        | ValueSome struct (allPoints, _, _) -> ValueSome allPoints

    /// <summary>Assembles a glyph's contour points and phantoms inside a live budget, <c>glyf_accelerator_t::get_points</c> as the
    /// path builder calls it: the traversal and the points are paid out of the budget it is given.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="budget">The live budget.</param>
    /// <returns>The points, phantoms last, and the budget left; none where the glyph or the budget fails.</returns>
    let pointsWithin (glyf: GlyfFace) gvar coords glyph budget =
        let ctx = { Glyf = glyf; Gvar = gvar; Coords = coords; PhantomOnly = false; Extended = glyf.Extended }
        let struct (bytes, kind, header, gid) = glyphForGid ctx.Glyf glyph
        match getPoints ctx bytes kind header gid 0 0 budget decyclerCreate with
        | ValueNone -> ValueNone
        | ValueSome struct (allPoints, _, remaining) ->
            let left = remaining - int64 allPoints.Length
            match left >= 0L with
            | false -> ValueNone
            | true -> ValueSome (struct (allPoints, left))

    /// <summary>Loads a glyph's four phantoms and pays the top-level point budget.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The four phantoms, or none where the glyph or budget fails.</returns>
    let private phantomPoints (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (glyph: uint32) : ImmutableArray<ContourPoint> voption =
        let ctx = { Glyf = glyf; Gvar = gvar; Coords = coords; PhantomOnly = true; Extended = glyf.Extended }
        match runPoints ctx glyph with
        | ValueNone -> ValueNone
        | ValueSome struct (allPoints, _, budget) ->
            let budget = budget - int64 allPoints.Length
            match budget >= 0L with
            | false -> ValueNone
            | true -> ValueSome (Sealed.init PhantomCount (fun index -> allPoints[allPoints.Length - PhantomCount + index]))

    /// <summary>Returns a glyph's unscaled advance from its gvar phantoms, <c>get_advance_with_var_unscaled</c>.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="upem">The face's units per em, the fallback advance.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="vertical">Whether to read the vertical advance.</param>
    /// <returns>The unscaled advance.</returns>
    let advanceWithVariations (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (upem: int) (glyph: uint32) (vertical: bool) : int =
        match glyph >= uint32 glyf.NumGlyphs with
        | true -> 0
        | false ->
            match phantomPoints glyf gvar coords glyph with
            | ValueNone -> if vertical then upem else upem / 2
            | ValueSome phantoms ->
                let result =
                    match vertical with
                    | true -> phantoms[2].Y - phantoms[3].Y
                    | false -> phantoms[1].X - phantoms[0].X
                let rounded = Math.Floor(double result + 0.5)
                let maxed = if rounded >= 0.0 then rounded else 0.0
                let clamped = if maxed <= double MaxAdvanceFloat then maxed else double MaxAdvanceFloat
                int (uint32 clamped)

    /// <summary>Returns a glyph's unscaled vertical origin from its top phantom, <c>get_v_origin_with_var_unscaled</c>.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="upem">The face's units per em, the fallback origin.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The unscaled vertical origin.</returns>
    let vOriginWithVariations (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (upem: int) (glyph: uint32) : float32 =
        match glyph >= uint32 glyf.NumGlyphs with
        | true -> 0.0f
        | false ->
            match phantomPoints glyf gvar coords glyph with
            | ValueNone -> float32 upem
            | ValueSome phantoms -> phantoms[2].Y

    /// <summary>Folds one glyph's contour points into a bounding box, <c>contour_bounds_t</c>.</summary>
    /// <param name="points">The points, phantoms included.</param>
    /// <param name="index">The next point index.</param>
    /// <param name="count">The number of contour points.</param>
    /// <param name="minX">The minimum x so far.</param>
    /// <param name="minY">The minimum y so far.</param>
    /// <param name="maxX">The maximum x so far.</param>
    /// <param name="maxY">The maximum y so far.</param>
    /// <returns>The four bounds.</returns>
    [<TailCall>]
    let rec private boundsFrom (points: ReadOnlySpan<ContourPoint>) (index: int) (count: int) (minX: float32) (minY: float32) (maxX: float32) (maxY: float32) : struct (float32 * float32 * float32 * float32) =
        match index >= count with
        | true -> struct (minX, minY, maxX, maxY)
        | false ->
            let point = points[index]
            boundsFrom points (index + 1) count (Fixed.hbMin minX point.X) (Fixed.hbMin minY point.Y) (Fixed.hbMax maxX point.X) (Fixed.hbMax maxY point.Y)

    /// <summary>The unscaled ink extents of contour bounds, <c>contour_bounds_t::get_extents</c>.</summary>
    /// <param name="minX">The minimum x.</param>
    /// <param name="minY">The minimum y.</param>
    /// <param name="maxX">The maximum x.</param>
    /// <param name="maxY">The maximum y.</param>
    /// <returns>The unscaled extents.</returns>
    let private extentsOfBounds (minX: float32) (minY: float32) (maxX: float32) (maxY: float32) =
        match minX >= maxX || minY >= maxY with
        | true -> ValueSome { XBearing = 0; YBearing = 0; Width = 0; Height = 0 }
        | false ->
            let xBearing = Math.Floor(double minX + 0.5)
            let yBearing = Math.Floor(double maxY + 0.5)
            let xBearingInt = Fixed.clampToInt32 xBearing
            let yBearingInt = Fixed.clampToInt32 yBearing
            let width = Fixed.clampToInt32 (Math.Floor(double maxX + 0.5) - xBearing)
            let height = Fixed.clampToInt32 (Math.Floor(double minY + 0.5) - yBearing)
            ValueSome { XBearing = xBearingInt; YBearing = yBearingInt; Width = width; Height = height }

    /// <summary>Computes a glyph's unscaled ink extents from its contour points, <c>contour_bounds_t::get_extents</c>.</summary>
    /// <param name="allPoints">The assembled points, phantoms last.</param>
    /// <returns>The unscaled extents.</returns>
    let private extentsFromContour (allPoints: ImmutableArray<ContourPoint>) =
        let count = allPoints.Length - PhantomCount
        let struct (minX, minY, maxX, maxY) = boundsFrom (allPoints.AsSpan()) 0 count Single.MaxValue Single.MaxValue -Single.MaxValue -Single.MaxValue
        extentsOfBounds minX minY maxX maxY

    /// <summary>Returns a glyph's unscaled ink extents from its varied contour points, <c>get_extents_at</c>.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The unscaled extents, or none where the glyph fails.</returns>
    let extentsWithVariations (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (glyph: uint32) : GlyphExtents voption =
        match glyph >= uint32 glyf.NumGlyphs with
        | true -> ValueNone
        | false ->
            let ctx = { Glyf = glyf; Gvar = gvar; Coords = coords; PhantomOnly = false; Extended = glyf.Extended }
            match runPoints ctx glyph with
            | ValueNone -> ValueNone
            | ValueSome struct (allPoints, _, budget) ->
                let budget = budget - int64 allPoints.Length
                match budget >= 0L with
                | false -> ValueNone
                | true -> extentsFromContour allPoints

    /// <summary>The glyf and gvar tables the kernel reads a glyph from.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private KernelSource =
        val private Glyf: GlyfFace
        val private Gvar: Gvar

        /// <summary>Holds the tables.</summary>
        /// <param name="glyf">The glyf accelerator.</param>
        /// <param name="gvar">The gvar table.</param>
        new (glyf, gvar) = { Glyf = glyf; Gvar = gvar }

        interface IGlyfSource with
            member this.Glyph gid =
                let glyf = this.Glyf
                match uint64 gid < uint64 glyf.NumGlyphs with
                | false -> struct (ReadOnlyMemory<byte>.Empty, false)
                | true ->
                    let span = glyf.Loca.Span
                    let startU, finishU =
                        match glyf.ShortOffsets with
                        | true -> uint32 (int (Binary.u16 span (int gid * 2)) * 2), uint32 (int (Binary.u16 span (int gid * 2 + 2)) * 2)
                        | false -> Binary.u32 span (int gid * 4), Binary.u32 span (int gid * 4 + 4)
                    match startU > finishU || uint64 finishU > uint64 glyf.Glyf.Length with
                    | true -> struct (ReadOnlyMemory<byte>.Empty, false)
                    | false -> struct (glyf.Glyf.Slice(int startU, int (finishU - startU)), true)

            member this.Metrics gid =
                let glyf = this.Glyf
                struct (Hmtx.leadingBearing glyf.Hmtx gid, Hmtx.leadingBearing glyf.Vmtx gid, Hmtx.advance glyf.Hmtx gid, Hmtx.advance glyf.Vmtx gid)

            member this.VarData gid =
                Gvar.glyphVarData this.Gvar gid

    /// <summary>The scalar of every shared tuple of gvar at the coordinates: the same for every glyph of a font, so a font works it out
    /// once, when its first varied glyph is asked for.</summary>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <returns>The scalars by shared tuple index, empty for a table without axes.</returns>
    let sharedScalars (gvar: Gvar) (coords: NormalizedCoords) =
        match gvar.AxisCount with
        | 0 -> Array.empty
        | axisCount ->
            let count = min 4096 ((gvar.SharedTuples.Length / 2) / axisCount)
            let scalars = Sealed.init count (fun index -> TupleVariation.calculateScalar ReadOnlySpan<byte>.Empty { DataSize = 0us; TupleIndex = uint16 index; At = 0 } (coords.AsSpan()) axisCount gvar.SharedTuples.Span)
            Array.init scalars.Length (fun index -> scalars[index])

    /// <summary>Asks the glyf kernel for a glyph's unscaled ink extents at the coordinates.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="scalars">The scalars of the shared tuples at the coordinates, from <see cref="sharedScalars"/>.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>Whether the kernel answered, and the extents when it did.</returns>
    let kernelExtents (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (scalars: float32 array) (glyph: uint32) =
        match glyph >= uint32 glyf.NumGlyphs || gvar.AxisCount = 0 || not coords.HasNonZero with
        | true -> struct (false, ValueNone)
        | false ->
            let struct (answered, minX, minY, maxX, maxY) = GlyfKernel.bounds (KernelSource(glyf, gvar) :> IGlyfSource) glyf.Extended gvar.AxisCount gvar.SharedTuples scalars coords.Sealed glyph
            match answered with
            | false -> struct (false, ValueNone)
            | true -> struct (true, extentsOfBounds minX minY maxX maxY)

    /// <summary>Returns a glyph's unscaled ink extents from its varied contour points: the glyf kernel answers the ordinary glyph,
    /// and <see cref="extentsWithVariations"/> any other.</summary>
    /// <param name="glyf">The glyf accelerator.</param>
    /// <param name="gvar">The gvar table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="scalars">The scalars of the shared tuples at the coordinates, from <see cref="sharedScalars"/>.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The unscaled extents, or none where the glyph fails.</returns>
    let extentsWithVariationsFast (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (scalars: float32 array) (glyph: uint32) =
        let struct (answered, extents) = kernelExtents glyf gvar coords scalars glyph
        match answered with
        | true -> extents
        | false -> extentsWithVariations glyf gvar coords glyph
