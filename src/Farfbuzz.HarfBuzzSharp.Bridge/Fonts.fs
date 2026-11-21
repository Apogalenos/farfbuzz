namespace Farfbuzz.HarfBuzzSharp.Bridge

open Farfbuzz

/// <summary>Fonts for the HarfBuzzSharp shell (hb-font.cc).</summary>
[<AbstractClass; Sealed>]
type Fonts =
    /// <summary>The default font of a face, <c>hb_font_create</c> (hb-font.cc:2102-2125): scale equal to the units per em, the OpenType functions, the named instance of the face index.</summary>
    /// <param name="face">The core face.</param>
    /// <returns>The core font.</returns>
    static member Create (face: Face) =
        Font.create face

    /// <summary>The font a snapshot stands for: the default font of the face at the scale.</summary>
    /// <param name="face">The core face.</param>
    /// <param name="xScale">The horizontal scale.</param>
    /// <param name="yScale">The vertical scale.</param>
    /// <returns>The core font.</returns>
    static member Build (face: Face, xScale: int, yScale: int) =
        Font.withScale xScale yScale (Font.create face)

    /// <summary>A font with another scale, <c>hb_font_set_scale</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="xScale">The horizontal scale.</param>
    /// <param name="yScale">The vertical scale.</param>
    /// <returns>The font with the scale replaced.</returns>
    static member WithScale (font: Font, xScale: int, yScale: int) =
        Font.withScale xScale yScale font

    /// <summary>The horizontal scale, <c>hb_font_get_scale</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <returns>The scale.</returns>
    static member XScale (font: Font) =
        font.XScale

    /// <summary>The vertical scale, <c>hb_font_get_scale</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <returns>The scale.</returns>
    static member YScale (font: Font) =
        font.YScale

    /// <summary><c>hb_font_get_h_extents</c>: the horizontal font extents.</summary>
    /// <param name="font">The core font.</param>
    /// <returns>Whether the font has them, and the extents (zero when it has none).</returns>
    static member HExtents (font: Font) =
        match Font.hExtents font with
        | ValueSome extents -> struct (true, extents)
        | ValueNone -> struct (false, { Ascender = 0; Descender = 0; LineGap = 0 })

    /// <summary><c>hb_font_get_v_extents</c>: the vertical font extents.</summary>
    /// <param name="font">The core font.</param>
    /// <returns>Whether the font has them, and the extents (zero when it has none).</returns>
    static member VExtents (font: Font) =
        match Font.vExtents font with
        | ValueSome extents -> struct (true, extents)
        | ValueNone -> struct (false, { Ascender = 0; Descender = 0; LineGap = 0 })

    /// <summary><c>hb_font_get_extents_for_direction</c>: the extents of the direction's axis, made up from the other when the font has none
    /// (a direction that is not horizontal takes the vertical ones, as <c>HB_DIRECTION_IS_HORIZONTAL</c> decides).</summary>
    /// <param name="font">The core font.</param>
    /// <param name="direction">The <c>hb_direction_t</c> value.</param>
    /// <returns>The extents.</returns>
    static member ExtentsForDirection (font: Font, direction: int) =
        match direction with
        | 4 | 5 -> Font.hExtentsWithFallback font
        | _ -> Font.vExtentsWithFallback font

    /// <summary>The empty font, <c>hb_font_get_empty</c>, whose names are <c>gidN</c> and which has no extents.</summary>
    static member EmptyFont =
        Font.create Face.empty
