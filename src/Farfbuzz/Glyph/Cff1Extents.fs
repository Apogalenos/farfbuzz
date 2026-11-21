namespace Farfbuzz

open System
open Farfbuzz.Var

/// <summary>The bounds of one charstring, <c>bounds_t</c>; the numbers are double.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal CffBounds =
    { MinX: float
      MinY: float
      MaxX: float
      MaxY: float }

/// <summary>The CFF1 glyph extents from the charstring interpreter, bounds only.</summary>
[<RequireQualifiedAccess>]
module internal Cff1Extents =
    /// <summary>The per-glyph live-work budget, <c>HB_BUDGET_GLYPH</c>.</summary>
    [<Literal>]
    let GlyphBudget = 16777216L

    /// <summary>The Null subroutine INDEX of an out-of-range font-dict index.</summary>
    let private emptyIndex = { Start = 0; Count = 0u; OffSize = 0; CountSize = 2 }

    /// <summary>The initial empty bounds, <c>bounds_t::init</c>.</summary>
    let empty =
        { MinX = float Int32.MaxValue
          MinY = float Int32.MaxValue
          MaxX = float Int32.MinValue
          MaxY = float Int32.MinValue }

    /// <summary>Updates the bounds with one point, <c>bounds_t::update</c>.</summary>
    /// <param name="bounds">The bounds.</param>
    /// <param name="x">The point's x.</param>
    /// <param name="y">The point's y.</param>
    /// <returns>The bounds.</returns>
    let private update (bounds: CffBounds) (x: float) (y: float) : CffBounds =
        { MinX = min bounds.MinX x
          MinY = min bounds.MinY y
          MaxX = max bounds.MaxX x
          MaxY = max bounds.MaxY y }

    /// <summary>Whether the bounds are empty, <c>bounds_t::empty</c>.</summary>
    /// <param name="bounds">The bounds.</param>
    /// <returns>True when empty.</returns>
    let private isEmpty (bounds: CffBounds) : bool =
        bounds.MinX >= bounds.MaxX || bounds.MinY >= bounds.MaxY

    /// <summary>Merges one bounds into another, <c>bounds_t::merge</c>.</summary>
    /// <param name="bounds">The bounds.</param>
    /// <param name="other">The other bounds.</param>
    /// <returns>The merged bounds.</returns>
    let private merge (bounds: CffBounds) (other: CffBounds) : CffBounds =
        match isEmpty bounds with
        | true -> other
        | false ->
            match isEmpty other with
            | true -> bounds
            | false ->
                { MinX = min bounds.MinX other.MinX
                  MinY = min bounds.MinY other.MinY
                  MaxX = max bounds.MaxX other.MaxX
                  MaxY = max bounds.MaxY other.MaxY }

    /// <summary>Offsets the bounds by a delta, <c>bounds_t::offset</c>.</summary>
    /// <param name="bounds">The bounds.</param>
    /// <param name="dx">The x delta.</param>
    /// <param name="dy">The y delta.</param>
    /// <returns>The offset bounds.</returns>
    let private offset (bounds: CffBounds) (dx: float) (dy: float) : CffBounds =
        match isEmpty bounds with
        | true -> bounds
        | false ->
            { MinX = bounds.MinX + dx
              MinY = bounds.MinY + dy
              MaxX = bounds.MaxX + dx
              MaxY = bounds.MaxY + dy }

    /// <summary>The extents sink state: whether a path is open and the accumulated bounds.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type SinkState =
        { PathOpen: bool
          Bounds: CffBounds }

    /// <summary>The bounds sink, <c>cff1_path_procs_extents_t</c>: a path opens at the first line or curve.</summary>
    [<Struct>]
    type ExtentsSink =
        interface ICharStringSink<SinkState> with
            member _.MoveTo (state, _, _) = { state with PathOpen = false }
            member _.LineTo (state, fromX, fromY, x, y) =
                let bounds = match state.PathOpen with | false -> update state.Bounds fromX fromY | true -> state.Bounds
                { PathOpen = true; Bounds = update bounds x y }
            member _.CurveTo (state, fromX, fromY, x1, y1, x2, y2, x3, y3) =
                let bounds = match state.PathOpen with | false -> update state.Bounds fromX fromY | true -> state.Bounds
                let bounds = update bounds x1 y1
                let bounds = update bounds x2 y2
                let bounds = update bounds x3 y3
                { PathOpen = true; Bounds = bounds }

    /// <summary>Rounds half towards positive infinity in double, <c>_hb_roundf</c> on a double.</summary>
    /// <param name="x">The value.</param>
    /// <returns>The rounded value.</returns>
    let roundfHalfUp (x: float) : float =
        Math.Floor(x + 0.5)

    /// <summary>Interprets one glyph's charstring without resolving a seac endchar, through the reference interpreter or the
    /// bounds interpreter.</summary>
    /// <param name="fast">Whether to interpret with the bounds interpreter.</param>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The sink state, the ending and the remaining budget, or none.</returns>
    let private interpretOne (fast: bool) (cff: Cff1Accel) (glyph: uint32) (budget: int64) =
        match cff.Valid && glyph < cff.GlyphCount with
        | false -> ValueNone
        | true ->
            let table = cff.Table.Span
            let struct (charOffset, charLength) = CffIndex.item table cff.CharStrings glyph
            let fd = Cff1.fdOf cff glyph
            let localSubrs = match fd < cff.LocalSubrs.Length with | true -> cff.LocalSubrs[fd] | false -> emptyIndex
            match fast with
            | false -> CffCharString.interpret (ExtentsSink()) { PathOpen = false; Bounds = empty } CffCharString.cff1Flavour charOffset charLength cff.GlobalSubrs localSubrs table budget
            | true ->
                let result = CffKernel.interpret Cff1Blend.Cff1Blend false 0 table charOffset charLength (CffIndex.subrs cff.GlobalSubrs) (CffIndex.subrs localSubrs) budget
                match result.Valid with
                | false -> ValueNone
                | true ->
                    let bounds = { MinX = result.MinX; MinY = result.MinY; MaxX = result.MaxX; MaxY = result.MaxY }
                    let ending = match result.Seac with | true -> EndedWithSeac (result.Adx, result.Ady, result.BaseCode, result.AccentCode) | false -> Ended
                    ValueSome (struct ({ PathOpen = result.PathOpen; Bounds = bounds }, ending, result.Budget))

    /// <summary>Merges an accent's bounds into a seac's base and glyph bounds, offset by the accent position.</summary>
    /// <param name="fast">Whether to interpret with the bounds interpreter.</param>
    /// <param name="cff">The accelerator.</param>
    /// <param name="sink">The main glyph's bounds.</param>
    /// <param name="baseBounds">The base glyph's bounds.</param>
    /// <param name="adx">The accent's x offset.</param>
    /// <param name="ady">The accent's y offset.</param>
    /// <param name="accentGlyph">The accent glyph.</param>
    /// <param name="remaining">The remaining budget.</param>
    /// <returns>The merged bounds and the remaining budget, or none.</returns>
    let private mergeAccent (fast: bool) (cff: Cff1Accel) (sink: SinkState) (baseBounds: CffBounds) (adx: float) (ady: float) (accentGlyph: uint32) (remaining: int64) =
        match interpretOne fast cff accentGlyph remaining with
        | ValueNone -> ValueNone
        | ValueSome (struct (accentSink, accentEnding, afterAccent)) ->
            match accentEnding with
            | Ended ->
                let merged = merge sink.Bounds baseBounds
                ValueSome (struct (merge merged (offset accentSink.Bounds adx ady), afterAccent))
            | EndedWithSeac _ -> ValueNone

    /// <summary>Resolves a seac endchar into its base and accent bounds, a bounded loop of two inner glyphs.</summary>
    /// <param name="fast">Whether to interpret with the bounds interpreter.</param>
    /// <param name="cff">The accelerator.</param>
    /// <param name="sink">The main glyph's bounds.</param>
    /// <param name="adx">The accent's x offset.</param>
    /// <param name="ady">The accent's y offset.</param>
    /// <param name="baseGlyph">The base glyph.</param>
    /// <param name="accentGlyph">The accent glyph.</param>
    /// <param name="remaining">The remaining budget.</param>
    /// <returns>The merged bounds and the remaining budget, or none.</returns>
    let private resolveSeac (fast: bool) (cff: Cff1Accel) (sink: SinkState) (adx: float) (ady: float) (baseGlyph: uint32) (accentGlyph: uint32) (remaining: int64) =
        match interpretOne fast cff baseGlyph remaining with
        | ValueNone -> ValueNone
        | ValueSome (struct (baseSink, baseEnding, afterBase)) ->
            match baseEnding with
            | Ended -> mergeAccent fast cff sink baseSink.Bounds adx ady accentGlyph afterBase
            | EndedWithSeac _ -> ValueNone

    /// <summary>Handles a charstring's ending: its bounds, or a seac endchar resolved into base and accent.</summary>
    /// <param name="fast">Whether to interpret with the bounds interpreter.</param>
    /// <param name="cff">The accelerator.</param>
    /// <param name="sink">The sink state.</param>
    /// <param name="ending">The ending.</param>
    /// <param name="inSeac">Whether the caller is already inside a seac.</param>
    /// <param name="remaining">The remaining budget.</param>
    /// <returns>The bounds and the remaining budget, or none.</returns>
    let private endingBounds (fast: bool) (cff: Cff1Accel) (sink: SinkState) (ending: CharStringEnd) (inSeac: bool) (remaining: int64) =
        match ending with
        | Ended -> ValueSome (struct (sink.Bounds, remaining))
        | EndedWithSeac (adx, ady, baseCode, accentCode) ->
            match inSeac with
            | true -> ValueNone
            | false ->
                let baseGlyph = Cff1.stdCodeToGlyph cff baseCode
                let accentGlyph = Cff1.stdCodeToGlyph cff accentCode
                match baseGlyph <> 0u && accentGlyph <> 0u with
                | false -> ValueNone
                | true -> resolveSeac fast cff sink adx ady baseGlyph accentGlyph remaining

    /// <summary>Computes one glyph's bounds, resolving a seac endchar into its base and accent.</summary>
    /// <param name="fast">Whether to interpret with the bounds interpreter.</param>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <param name="inSeac">Whether the caller is already inside a seac.</param>
    /// <returns>The bounds and the remaining budget, or none.</returns>
    let private boundsAt (fast: bool) (cff: Cff1Accel) (glyph: uint32) (budget: int64) (inSeac: bool) =
        match interpretOne fast cff glyph budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (sink, ending, remaining)) -> endingBounds fast cff sink ending inSeac remaining

    /// <summary>Computes one glyph's bounds, <c>_get_bounds</c>.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The bounds and the remaining budget, or none.</returns>
    let bounds (cff: Cff1Accel) (glyph: uint32) (budget: int64) =
        boundsAt false cff glyph budget false

    /// <summary>Computes one glyph's bounds with the bounds interpreter, <c>_get_bounds</c>.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The bounds and the remaining budget, or none.</returns>
    let boundsFast (cff: Cff1Accel) (glyph: uint32) (budget: int64) =
        boundsAt true cff glyph budget false

    /// <summary>Computes one glyph's unscaled ink extents, <c>get_extents</c> before scaling.</summary>
    /// <param name="bounds">The bounds and the remaining budget, or none.</param>
    /// <returns>The extents, or none when the charstring has no bounds.</returns>
    let private extentsOf (bounds: struct (CffBounds * int64) voption) =
        match bounds with
        | ValueNone -> ValueNone
        | ValueSome (struct (bounds, _)) ->
            let xBearing, width =
                match bounds.MinX >= bounds.MaxX with
                | true -> 0, 0
                | false ->
                    let xBearing = Fixed.clampToInt32 (roundfHalfUp bounds.MinX)
                    let width = Fixed.clampToInt32 (roundfHalfUp bounds.MaxX - float xBearing)
                    xBearing, width
            let yBearing, height =
                match bounds.MinY >= bounds.MaxY with
                | true -> 0, 0
                | false ->
                    let yBearing = Fixed.clampToInt32 (roundfHalfUp bounds.MaxY)
                    let height = Fixed.clampToInt32 (roundfHalfUp bounds.MinY - float yBearing)
                    yBearing, height
            ValueSome { XBearing = xBearing; YBearing = yBearing; Width = width; Height = height }

    /// <summary>Computes one glyph's unscaled ink extents, <c>get_extents</c> before scaling.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the charstring has no bounds.</returns>
    let glyph (cff: Cff1Accel) (glyph: uint32) =
        extentsOf (bounds cff glyph GlyphBudget)

    /// <summary>Computes one glyph's unscaled ink extents with the bounds interpreter, <c>get_extents</c> before scaling.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the charstring has no bounds.</returns>
    let glyphFast (cff: Cff1Accel) (glyph: uint32) =
        extentsOf (boundsFast cff glyph GlyphBudget)
