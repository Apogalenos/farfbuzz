namespace Farfbuzz

open System

/// <summary>The font-function records HarfBuzz builds itself: the OpenType defaults written out as a record, and the delegation of a sub-font to its parent
/// (<c>hb_font_create_sub_font</c>, hb-font.cc:2195).</summary>
[<RequireQualifiedAccess>]
module internal FontFuncsOf =
    /// <summary>The default (OpenType) functions of a font as a record, each answering from the font without functions.</summary>
    /// <param name="font">The font; its own functions, if any, are ignored.</param>
    /// <returns>The record.</returns>
    let defaults (font: Font) =
        let plain = Font.withFuncs ValueNone font
        { Nominal = Font.nominalGlyph plain
          Variation = Font.variationGlyph plain
          HAdvance = Font.hAdvance plain
          VAdvance = Font.vAdvance plain
          HOrigin = fun _ -> ValueSome (struct (0, 0))
          VOrigin = fun glyph -> match Origins.vOrigin plain glyph with | Ok origin -> ValueSome origin | Error _ -> ValueNone
          HKerning = fun _ _ -> 0
          Extents = fun glyph -> match Extents.glyph plain glyph with | Ok extents -> extents | Error _ -> ValueNone
          ContourPoint = fun _ _ -> ValueNone
          GlyphName = Font.glyphName plain
          GlyphFromName = Font.glyphFromName plain
          HFontExtents = fun () -> Font.hExtents plain
          VFontExtents = fun () -> Font.vExtents plain }

    /// <summary>Scales a distance of the parent to the sub-font, <c>parent_scale_x_distance</c>.</summary>
    /// <param name="own">The sub-font's scale.</param>
    /// <param name="parent">The parent's scale.</param>
    /// <param name="value">The distance.</param>
    /// <returns>The distance at the sub-font's scale.</returns>
    let private scaled (own: int) (parent: int) (value: int) =
        match own = parent with
        | true -> value
        | false -> int (int64 value * int64 own / int64 parent)

    /// <summary>A sub-font: a copy of the parent at a scale, whose functions are the parent's, distances converted from the parent's scale to its own.</summary>
    /// <param name="parent">The parent font, with its functions if it has some.</param>
    /// <param name="xScale">The sub-font's horizontal scale.</param>
    /// <param name="yScale">The sub-font's vertical scale.</param>
    /// <returns>The sub-font.</returns>
    let subFont (parent: Font) (xScale: int) (yScale: int) =
        let funcs = match parent.Funcs with | ValueSome own -> own | ValueNone -> defaults parent
        let sx = scaled xScale parent.XScale
        let sy = scaled yScale parent.YScale
        let point (found: struct (int * int) voption) = match found with | ValueSome (struct (x, y)) -> ValueSome (struct (sx x, sy y)) | ValueNone -> ValueNone
        let extents (found: FontExtents voption) = match found with | ValueSome e -> ValueSome { Ascender = sy e.Ascender; Descender = sy e.Descender; LineGap = sy e.LineGap } | ValueNone -> ValueNone
        let sub =
            { Nominal = funcs.Nominal
              Variation = funcs.Variation
              HAdvance = fun glyph -> sx (funcs.HAdvance glyph)
              VAdvance = fun glyph -> sy (funcs.VAdvance glyph)
              HOrigin = fun glyph -> point (funcs.HOrigin glyph)
              VOrigin = fun glyph -> point (funcs.VOrigin glyph)
              HKerning = fun left right -> sx (funcs.HKerning left right)
              Extents = fun glyph -> match funcs.Extents glyph with | ValueSome e -> ValueSome { XBearing = sx e.XBearing; YBearing = sy e.YBearing; Width = sx e.Width; Height = sy e.Height } | ValueNone -> ValueNone
              ContourPoint = fun glyph index -> point (funcs.ContourPoint glyph index)
              GlyphName = funcs.GlyphName
              GlyphFromName = funcs.GlyphFromName
              HFontExtents = fun () -> extents (funcs.HFontExtents ())
              VFontExtents = fun () -> extents (funcs.VFontExtents ()) }
        Font.withFuncs (ValueSome sub) (Font.withScale xScale yScale parent)
