namespace Farfbuzz.Var

open Farfbuzz

/// <summary>The variation-device deltas of a Device table, <c>OT::VariationDevice</c>.</summary>
[<RequireQualifiedAccess>]
module internal VariationDevice =
    /// <summary>The horizontal variation delta in user space, <c>get_x_delta</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
    /// <returns>The delta, zero without non-zero coordinates.</returns>
    let xDelta (font: Font) (varIdx: uint32) : int =
        match font.Variations.Coords.HasNonZero with
        | false -> 0
        | true -> Font.emScalefX font (ItemVariationStore.delta font.Variations.Gdef varIdx)

    /// <summary>The vertical variation delta in user space, <c>get_y_delta</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
    /// <returns>The delta, zero without non-zero coordinates.</returns>
    let yDelta (font: Font) (varIdx: uint32) : int =
        match font.Variations.Coords.HasNonZero with
        | false -> 0
        | true -> Font.emScalefY font (ItemVariationStore.delta font.Variations.Gdef varIdx)

    /// <summary>The horizontal variation delta as an unrounded float, <c>get_x_delta_f</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
    /// <returns>The delta, zero without non-zero coordinates.</returns>
    let xDeltaF (font: Font) (varIdx: uint32) : float32 =
        match font.Variations.Coords.HasNonZero with
        | false -> 0.0f
        | true -> Font.emFscalefX font (ItemVariationStore.delta font.Variations.Gdef varIdx)

    /// <summary>The vertical variation delta as an unrounded float, <c>get_y_delta_f</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
    /// <returns>The delta, zero without non-zero coordinates.</returns>
    let yDeltaF (font: Font) (varIdx: uint32) : float32 =
        match font.Variations.Coords.HasNonZero with
        | false -> 0.0f
        | true -> Font.emFscalefY font (ItemVariationStore.delta font.Variations.Gdef varIdx)
