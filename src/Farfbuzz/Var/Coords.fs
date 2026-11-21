namespace Farfbuzz.Var

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>
/// The normalized coordinates of a font instance, HarfBuzz's <c>font->coords</c>: one 2.14 value per fvar axis held in
/// an <c>int</c> (hb_font_set_var_coords_normalized stores its input unclamped), and <c>has_nonzero_coords</c> computed
/// once when the value is built (src/hb-font.cc:2177).
/// </summary>
[<Struct; NoComparison; NoEquality>]
type internal NormalizedCoords =
    private
        { Values: ImmutableArray<int>
          NonZero: bool }

    /// <summary>The coordinates in fvar axis order.</summary>
    /// <returns>A span over the sealed coordinates.</returns>
    member this.AsSpan() : ReadOnlySpan<int> = this.Values.AsSpan()

    /// <summary>The coordinates as the sealed array they are held in, for a key that compares them value by value.</summary>
    member this.Sealed = this.Values

    /// <summary>Whether any coordinate is non-zero, <c>font->has_nonzero_coords</c>.</summary>
    member this.HasNonZero = this.NonZero

    /// <summary>The number of coordinates, <c>font->num_coords</c>.</summary>
    member this.Length = this.Values.Length

/// <summary>Builders and readers of <see cref="NormalizedCoords"/>.</summary>
[<RequireQualifiedAccess>]
module internal NormalizedCoords =
    /// <summary>No coordinates: a font without fvar.</summary>
    let empty = { Values = ImmutableArray<int>.Empty; NonZero = false }

    /// <summary>Wraps sealed 2.14 coordinates and computes <c>hb_any</c> over them.</summary>
    /// <param name="values">The coordinates, already sealed.</param>
    /// <returns>The coordinates with their non-zero flag.</returns>
    let ofSealed (values: ImmutableArray<int>) =
        { Values = values; NonZero = Spans.exists (fun coord -> coord <> 0) (values.AsSpan()) }

    /// <summary>The coordinate of an axis, 0 past the end, as <c>i &lt; coord_len ? coords[i] : 0</c>.</summary>
    /// <param name="coords">The coordinates.</param>
    /// <param name="axis">The axis index.</param>
    /// <returns>The coordinate or 0.</returns>
    let inline at (coords: ReadOnlySpan<int>) (axis: int) =
        match axis < coords.Length with
        | true -> coords[axis]
        | false -> 0

/// <summary>One VarRegionAxis: a tent in raw F2DOT14 units.</summary>
[<Struct; NoComparison; NoEquality>]
type internal RegionAxis = { Start: int16; Peak: int16; End: int16 }

/// <summary>What one region axis contributes to a region scalar.</summary>
[<Struct; NoComparison; NoEquality>]
type internal AxisFactor =
    | Unrestricted // factor 1: no peak, the coordinate at the peak, or an invalid tent HarfBuzz ignores
    | Outside // factor 0: the coordinate is zero or outside the tent; the whole region scalar is 0
    | Slope of numerator: int * denominator: int // on a slope: float (numerator) / denominator, divided in float32

/// <summary>The active pattern naming VarRegionAxis::evaluate's cases.</summary>
[<AutoOpen>]
module internal AxisFactorPatterns =
    /// <summary>Classifies a tent against a coordinate in the order of src/hb-ot-layout-common.hh:2549-2573.</summary>
    /// <param name="tent">The struct of the region axis and the coordinate.</param>
    /// <returns>The factor.</returns>
    let inline (|RegionAxisFactor|) (tent: struct (RegionAxis * int)) : AxisFactor =
        let struct (axis, coord) = tent
        let start = int axis.Start
        let peak = int axis.Peak
        let finish = int axis.End
        match peak with
        | 0 -> Unrestricted
        | _ when coord = peak -> Unrestricted
        | _ when coord = 0 -> Outside
        | _ when start > peak || peak > finish -> Unrestricted
        | _ when start < 0 && finish > 0 -> Unrestricted
        | _ when coord <= start || finish <= coord -> Outside
        | _ when coord < peak -> Slope (coord - start, peak - start)
        | _ -> Slope (finish - coord, finish - peak)

    /// <summary>Classifies a tent against a coordinate as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline the active pattern there.</summary>
    /// <param name="tent">The struct of the region axis and the coordinate.</param>
    /// <returns>The factor.</returns>
    let regionAxisFactorCall (tent: struct (RegionAxis * int)) =
        (|RegionAxisFactor|) tent

/// <summary>Region scalars, VarRegionList::evaluate.</summary>
[<RequireQualifiedAccess>]
module internal Region =
    /// <summary>
    /// The product of one region's axis factors in axis order, stopping at the first 0, in float32 and without FMA
    /// (src/hb-ot-layout-common.hh:2731-2752).
    /// </summary>
    /// <param name="axes">The region's axes, <c>axisCount</c> of them.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="axis">The next axis to multiply in.</param>
    /// <param name="product">The product so far; start with 1.0f.</param>
    /// <returns>The region scalar.</returns>
    [<TailCall>]
    let rec scalarFrom (axes: ReadOnlySpan<RegionAxis>) (coords: ReadOnlySpan<int>) (axis: int) (product: float32) =
        match axis = axes.Length with
        | true -> product
        | false ->
            match struct (axes[axis], NormalizedCoords.at coords axis) with
            | RegionAxisFactor factor ->
                match factor with
                | Unrestricted -> scalarFrom axes coords (axis + 1) product
                | Outside -> 0.0f
                | Slope (numerator, denominator) ->
                    scalarFrom axes coords (axis + 1) (product * (float32 numerator / float32 denominator))
