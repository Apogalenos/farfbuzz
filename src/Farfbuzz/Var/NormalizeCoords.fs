namespace Farfbuzz.Var

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>Normalizes design and variation coordinates to 2.14, <c>hb_ot_var_normalize_coords</c> and <c>hb_ot_var_normalize_variations</c>.</summary>
[<RequireQualifiedAccess>]
module internal NormalizeCoords =
    /// <summary>The last value a variation assigns to an axis, <c>hb_ot_var_normalize_variations</c>' scan in order.</summary>
    /// <param name="fvar">The fvar table.</param>
    /// <param name="variations">The variations.</param>
    /// <param name="axis">The axis whose value is sought.</param>
    /// <param name="index">The next variation.</param>
    /// <param name="found">The last matching value so far.</param>
    /// <returns>The last matching value, or none.</returns>
    [<TailCall>]
    let rec private lastVariationValue (fvar: Fvar) (variations: ReadOnlySpan<Variation>) (axis: int) (index: int) (found: float32 voption) : float32 voption =
        match index >= variations.Length with
        | true -> found
        | false ->
            let next =
                match Fvar.findAxisInfo variations[index].Tag fvar with
                | ValueSome info when info.AxisIndex = axis -> ValueSome variations[index].Value
                | ValueSome _ | ValueNone -> found
            lastVariationValue fvar variations axis (index + 1) next

    /// <summary>Unfolds the 16.16 coordinate of each axis from the variation list.</summary>
    [<Struct>]
    type private VariationCoordFolder =
        val private fvar: Fvar

        /// <summary>Creates a variation-coordinate folder.</summary>
        /// <param name="fvar">The fvar table.</param>
        new (fvar) = { fvar = fvar }

        interface ISpanUnfolder<Variation, int, int> with
            member this.Next (source, axis) =
                let coord =
                    match lastVariationValue this.fvar source axis 0 ValueNone with
                    | ValueSome value -> int (Fixed.roundf (Fvar.normalizeAxisValue axis value this.fvar * 65536.0f))
                    | ValueNone -> 0
                struct (coord, axis + 1)

    /// <summary>Normalizes design coordinates: fvar, then avar, then 2.14, <c>hb_ot_var_normalize_coords</c>.</summary>
    /// <param name="fvar">The fvar table.</param>
    /// <param name="avar">The avar table.</param>
    /// <param name="design">The design coordinates.</param>
    /// <returns>The normalized 2.14 coordinates.</returns>
    let normalizeCoords (fvar: Fvar) (avar: Avar) (design: ReadOnlySpan<float32>) =
        let coords = Sealed.mapi (fun index value -> int (Fixed.roundf (Fvar.normalizeAxisValue index value fvar * 65536.0f))) design
        let mapped = Avar.mapCoords16Dot16 avar coords
        Sealed.init mapped.Length (fun index -> (mapped[index] + 2) >>> 2)

    /// <summary>Normalizes variations: the last value per tag wins, then avar, then 2.14, <c>hb_ot_var_normalize_variations</c>.</summary>
    /// <param name="fvar">The fvar table.</param>
    /// <param name="avar">The avar table.</param>
    /// <param name="variations">The variations, in order.</param>
    /// <param name="coordsLength">The number of coordinates to produce.</param>
    /// <returns>The normalized 2.14 coordinates.</returns>
    let normalizeVariations (fvar: Fvar) (avar: Avar) (variations: ReadOnlySpan<Variation>) (coordsLength: int) =
        let coords = Sealed.unfoldSpan variations coordsLength 0 (VariationCoordFolder(fvar))
        let mapped = Avar.mapCoords16Dot16 avar coords
        Sealed.init mapped.Length (fun index -> (mapped[index] + 2) >>> 2)
