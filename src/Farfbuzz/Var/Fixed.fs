namespace Farfbuzz.Var

open System

/// <summary>The fixed-point decoding, rounding and saturating conversions of <c>hb-open-type.hh</c> and <c>hb-algs.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Fixed =
    /// <summary>The number of fraction bits in a 2.14 value, <c>fraction_bits</c> of F2DOT14.</summary>
    [<Literal>]
    let private F2Dot14FractionBits = 14

    /// <summary>The number of fraction bits in a 16.16 value, <c>fraction_bits</c> of F16DOT16.</summary>
    [<Literal>]
    let private F16Dot16FractionBits = 16

    /// <summary>Decodes a 2.14 fixed value, <c>HBFixed&lt;HBINT16,14&gt;::to_float</c>: the raw int to float32, then the power-of-two scale.</summary>
    /// <param name="raw">The raw 2.14 value.</param>
    /// <returns>The float32 value.</returns>
    let inline f2dot14 (raw: int16) =
        float32 raw * (1.0f / float32 (1 <<< F2Dot14FractionBits))

    /// <summary>Converts a 2.14 fixed-point value as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="f2dot14"/> there.</summary>
    /// <param name="raw">The raw value.</param>
    /// <returns>The value.</returns>
    let f2dot14Call (raw: int16) =
        f2dot14 raw


    /// <summary>Decodes a 16.16 fixed value, <c>HBFixed&lt;HBINT32,16&gt;::to_float</c>: the raw int to float32, then the power-of-two scale.</summary>
    /// <param name="raw">The raw 16.16 value.</param>
    /// <returns>The float32 value.</returns>
    let inline f16dot16 (raw: int) =
        float32 raw * (1.0f / float32 (1 <<< F16Dot16FractionBits))

    /// <summary>A 16.16 fixed-point value converted as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="f16dot16"/> there.</summary>
    /// <param name="raw">The raw value.</param>
    /// <returns>The value.</returns>
    let f16dot16Call (raw: int) =
        f16dot16 raw


    /// <summary>Rounds half towards positive infinity, HarfBuzz's <c>_hb_roundf</c> (src/hb-algs.hh:343-350).</summary>
    /// <param name="x">The value.</param>
    /// <returns>The rounded value.</returns>
    let inline roundf (x: float32) =
        float32 (Math.Floor(float x + 0.5))

    /// <summary>A value rounded half towards positive infinity as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="roundf"/> there.</summary>
    /// <param name="x">The value.</param>
    /// <returns>The rounded value.</returns>
    let roundfCall (x: float32) =
        roundf x


    /// <summary>The smaller of two values, <c>hb_min</c>: a NaN operand never wins.</summary>
    /// <param name="a">The first value.</param>
    /// <param name="b">The second value.</param>
    /// <returns>The minimum.</returns>
    let inline hbMin (a: float32) (b: float32) =
        if a <= b then a else b

    /// <summary>The larger of two values, <c>hb_max</c>: a NaN operand never wins.</summary>
    /// <param name="a">The first value.</param>
    /// <param name="b">The second value.</param>
    /// <returns>The maximum.</returns>
    let inline hbMax (a: float32) (b: float32) =
        if a >= b then a else b

    /// <summary>Clamps a value into a range, <c>hb_clamp</c>.</summary>
    /// <param name="x">The value.</param>
    /// <param name="lo">The lower bound.</param>
    /// <param name="hi">The upper bound.</param>
    /// <returns>The clamped value.</returns>
    let inline hbClamp (x: float32) (lo: float32) (hi: float32) =
        hbMin (hbMax x lo) hi

    /// <summary>Converts a double to <see cref="uint32"/> saturating to its range, <c>hb_clamp_to&lt;unsigned&gt;</c>.</summary>
    /// <param name="x">The value.</param>
    /// <returns>The saturated value; NaN gives 0.</returns>
    let clampToUInt32 x =
        let maxed = if x >= 0.0 then x else 0.0
        let clamped = if maxed <= float UInt32.MaxValue then maxed else float UInt32.MaxValue
        uint32 clamped

    /// <summary>Converts a double to <see cref="int"/> saturating to its range, <c>hb_clamp_to&lt;int&gt;</c>.</summary>
    /// <param name="x">The value.</param>
    /// <returns>The saturated value; NaN gives <see cref="Int32.MinValue"/>.</returns>
    let clampToInt32 x =
        let maxed = if x >= float Int32.MinValue then x else float Int32.MinValue
        let clamped = if maxed <= float Int32.MaxValue then maxed else float Int32.MaxValue
        int clamped
