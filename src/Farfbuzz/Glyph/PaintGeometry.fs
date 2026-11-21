namespace Farfbuzz

open System
open Farfbuzz.Var

/// <summary>A box of floats, <c>hb_extents_t&lt;float&gt;</c>; a minimum above the maximum is void.</summary>
[<Struct>]
type internal Box =
    { XMin: float32
      YMin: float32
      XMax: float32
      YMax: float32 }

/// <summary>An affine transform, <c>hb_transform_t&lt;float&gt;</c>.</summary>
[<Struct>]
type internal Transform =
    { XX: float32
      YX: float32
      XY: float32
      YY: float32
      X0: float32
      Y0: float32 }

/// <summary>Whether a region is unbounded, bounded or empty, <c>hb_bounds_t::status_t</c>.</summary>
[<Struct>]
type internal BoundsStatus =
    | Unbounded
    | Bounded
    | Empty

/// <summary>A region and what is known of it, <c>hb_bounds_t&lt;float&gt;</c>; the box is stale once the status leaves bounded.</summary>
[<Struct>]
type internal Bounds =
    { Status: BoundsStatus
      Box: Box }

/// <summary>The geometry of <c>hb-geometry.hh</c>: boxes, transforms and bounds.</summary>
[<RequireQualifiedAccess>]
module internal PaintGeometry =
    /// <summary>The void box a fresh <c>hb_extents_t</c> is.</summary>
    let voidBox = { XMin = 0.0f; YMin = 0.0f; XMax = -1.0f; YMax = -1.0f }

    /// <summary>The identity transform.</summary>
    let identity = { XX = 1.0f; YX = 0.0f; XY = 0.0f; YY = 1.0f; X0 = 0.0f; Y0 = 0.0f }

    /// <summary>Whether a box has no area, <c>hb_extents_t::is_empty</c>.</summary>
    /// <param name="box">The box.</param>
    /// <returns>True when empty.</returns>
    let isEmpty (box: Box) =
        box.XMin >= box.XMax || box.YMin >= box.YMax

    /// <summary>Whether a box is void, <c>hb_extents_t::is_void</c>.</summary>
    /// <param name="box">The box.</param>
    /// <returns>True when void.</returns>
    let isVoid (box: Box) =
        box.XMin > box.XMax

    /// <summary>The union of two boxes, <c>hb_extents_t::union_</c>.</summary>
    /// <param name="box">The box.</param>
    /// <param name="other">The other box.</param>
    /// <returns>The union.</returns>
    let union (box: Box) (other: Box) =
        match isEmpty other with
        | true -> box
        | false ->
            match isEmpty box with
            | true -> other
            | false ->
                { XMin = Fixed.hbMin box.XMin other.XMin
                  YMin = Fixed.hbMin box.YMin other.YMin
                  XMax = Fixed.hbMax box.XMax other.XMax
                  YMax = Fixed.hbMax box.YMax other.YMax }

    /// <summary>The intersection of two boxes, <c>hb_extents_t::intersect</c>.</summary>
    /// <param name="box">The box.</param>
    /// <param name="other">The other box.</param>
    /// <returns>The intersection, void when either is empty.</returns>
    let intersect (box: Box) (other: Box) =
        match isEmpty other || isEmpty box with
        | true -> voidBox
        | false ->
            { XMin = Fixed.hbMax box.XMin other.XMin
              YMin = Fixed.hbMax box.YMin other.YMin
              XMax = Fixed.hbMin box.XMax other.XMax
              YMax = Fixed.hbMin box.YMax other.YMax }

    /// <summary>Grows a box to hold a point, <c>hb_extents_t::add_point</c>.</summary>
    /// <param name="box">The box.</param>
    /// <param name="x">The point's x.</param>
    /// <param name="y">The point's y.</param>
    /// <returns>The grown box.</returns>
    let addPoint (box: Box) x y =
        match isVoid box with
        | true -> { XMin = x; YMin = y; XMax = x; YMax = y }
        | false ->
            { XMin = Fixed.hbMin box.XMin x
              YMin = Fixed.hbMin box.YMin y
              XMax = Fixed.hbMax box.XMax x
              YMax = Fixed.hbMax box.YMax y }

    /// <summary>Rounds a box into glyph extents, <c>hb_extents_t::to_glyph_extents</c> with both flags off: <c>round</c> of
    /// every corner, zeros when any is not finite.</summary>
    /// <param name="box">The box.</param>
    /// <returns>The glyph extents.</returns>
    let toGlyphExtents (box: Box) =
        let x0 = Math.Round(float box.XMin, MidpointRounding.AwayFromZero)
        let y0 = Math.Round(float box.YMin, MidpointRounding.AwayFromZero)
        let x1 = Math.Round(float box.XMax, MidpointRounding.AwayFromZero)
        let y1 = Math.Round(float box.YMax, MidpointRounding.AwayFromZero)
        match Double.IsFinite x0 && Double.IsFinite y0 && Double.IsFinite x1 && Double.IsFinite y1 with
        | false -> { XBearing = 0; YBearing = 0; Width = 0; Height = 0 }
        | true ->
            { XBearing = Fixed.clampToInt32 x0
              YBearing = Fixed.clampToInt32 y1
              Width = Fixed.clampToInt32 (x1 - x0)
              Height = Fixed.clampToInt32 (y0 - y1) }

    /// <summary>Multiplies a transform by another, <c>hb_transform_t::multiply</c> with <c>before</c> off: the other is applied first.</summary>
    /// <param name="a">This transform.</param>
    /// <param name="b">The other transform.</param>
    /// <returns>The product.</returns>
    let multiply (a: Transform) (b: Transform) =
        { XX = a.XX * b.XX + a.XY * b.YX
          YX = a.YX * b.XX + a.YY * b.YX
          XY = a.XX * b.XY + a.XY * b.YY
          YY = a.YX * b.XY + a.YY * b.YY
          X0 = a.XX * b.X0 + a.XY * b.Y0 + a.X0
          Y0 = a.YX * b.X0 + a.YY * b.Y0 + a.Y0 }

    /// <summary>Transforms a box, <c>hb_transform_t::transform_extents</c>: the box of its four transformed corners; a void box stays void.</summary>
    /// <param name="t">The transform.</param>
    /// <param name="box">The box.</param>
    /// <returns>The transformed box.</returns>
    let transformBox (t: Transform) (box: Box) =
        match isVoid box with
        | true -> box
        | false ->
            let corner (x: float32) (y: float32) (acc: Box) =
                addPoint acc (t.X0 + t.XX * x + t.XY * y) (t.Y0 + t.YX * x + t.YY * y)
            let first = corner box.XMin box.YMin voidBox
            let second = corner box.XMin box.YMax first
            let third = corner box.XMax box.YMin second
            corner box.XMax box.YMax third

    /// <summary>The bounds of a box, <c>hb_bounds_t (const hb_extents_t &amp;)</c>.</summary>
    /// <param name="box">The box.</param>
    /// <returns>Empty bounds for an empty box, else bounded.</returns>
    let boundsOf (box: Box) =
        match isEmpty box with
        | true -> { Status = Empty; Box = box }
        | false -> { Status = Bounded; Box = box }

    /// <summary>Unites bounds, <c>hb_bounds_t::union_</c>.</summary>
    /// <param name="bounds">The bounds.</param>
    /// <param name="other">The other bounds.</param>
    /// <returns>The united bounds.</returns>
    let unionBounds (bounds: Bounds) (other: Bounds) =
        match other.Status with
        | Unbounded -> { bounds with Status = Unbounded }
        | Empty -> bounds
        | Bounded ->
            match bounds.Status with
            | Empty -> other
            | Bounded -> { bounds with Box = union bounds.Box other.Box }
            | Unbounded -> bounds

    /// <summary>Intersects bounds, <c>hb_bounds_t::intersect</c>.</summary>
    /// <param name="bounds">The bounds.</param>
    /// <param name="other">The other bounds.</param>
    /// <returns>The intersected bounds.</returns>
    let intersectBounds (bounds: Bounds) (other: Bounds) =
        match other.Status with
        | Empty -> { bounds with Status = Empty }
        | Unbounded -> bounds
        | Bounded ->
            match bounds.Status with
            | Unbounded -> other
            | Empty -> bounds
            | Bounded ->
                let box = intersect bounds.Box other.Box
                match isEmpty box with
                | true -> { Status = Empty; Box = box }
                | false -> { Status = Bounded; Box = box }
