namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open Farfbuzz

/// <summary>Builds the core's font functions from the delegates a HarfBuzzSharp <c>FontFunctions</c> holds (hb-font.cc <c>hb_font_funcs_t</c>).
/// A function the caller did not set answers as HarfBuzz's nil function does: nothing found, the font's scale as the horizontal advance, minus its vertical scale as the vertical one.</summary>
[<AbstractClass; Sealed>]
type internal FontFuncsBuilder =
    /// <summary>The core functions of a font.</summary>
    /// <param name="xScale">The font's horizontal scale.</param>
    /// <param name="yScale">The font's vertical scale.</param>
    /// <param name="nominal">The nominal glyph delegate wrapped by the shell, or null.</param>
    /// <param name="variation">The variation glyph delegate, or null.</param>
    /// <param name="hAdvance">The horizontal advance delegate, or null.</param>
    /// <param name="vAdvance">The vertical advance delegate, or null.</param>
    /// <param name="hOrigin">The horizontal origin delegate, or null.</param>
    /// <param name="vOrigin">The vertical origin delegate, or null.</param>
    /// <param name="hKerning">The horizontal kerning delegate, or null.</param>
    /// <param name="extents">The glyph extents delegate as x bearing, y bearing, width, height, or null.</param>
    /// <param name="contourPoint">The contour point delegate, or null.</param>
    /// <param name="glyphName">The glyph name delegate, or null.</param>
    /// <param name="glyphFromName">The glyph from name delegate, or null.</param>
    /// <param name="hFontExtents">The horizontal font extents delegate as ascender, descender, line gap, or null.</param>
    /// <param name="vFontExtents">The vertical font extents delegate, or null.</param>
    /// <returns>The functions.</returns>
    static member Build
        (xScale: int, yScale: int,
         nominal: Func<uint32, struct (bool * uint32)> | null,
         variation: Func<uint32, uint32, struct (bool * uint32)> | null,
         hAdvance: Func<uint32, int> | null,
         vAdvance: Func<uint32, int> | null,
         hOrigin: Func<uint32, struct (bool * int * int)> | null,
         vOrigin: Func<uint32, struct (bool * int * int)> | null,
         hKerning: Func<uint32, uint32, int> | null,
         extents: Func<uint32, struct (bool * int * int * int * int)> | null,
         contourPoint: Func<uint32, uint32, struct (bool * int * int)> | null,
         glyphName: Func<uint32, struct (bool * string)> | null,
         glyphFromName: Func<string, struct (bool * uint32)> | null,
         hFontExtents: Func<struct (bool * int * int * int)> | null,
         vFontExtents: Func<struct (bool * int * int * int)> | null) =
        let point (found: struct (bool * int * int)) = match found with | struct (true, x, y) -> ValueSome (struct (x, y)) | struct (false, _, _) -> ValueNone
        let fontExtents (found: struct (bool * int * int * int)) = match found with | struct (true, a, d, g) -> ValueSome { Ascender = a; Descender = d; LineGap = g } | struct (false, _, _, _) -> ValueNone
        { Nominal = (match nominal with | null -> (fun _ -> ValueNone) | f -> (fun cp -> match f.Invoke cp with | struct (true, g) -> ValueSome g | struct (false, _) -> ValueNone))
          Variation = (match variation with | null -> (fun _ _ -> ValueNone) | f -> (fun cp sel -> match f.Invoke(cp, sel) with | struct (true, g) -> ValueSome g | struct (false, _) -> ValueNone))
          HAdvance = (match hAdvance with | null -> (fun _ -> xScale) | f -> (fun g -> f.Invoke g))
          VAdvance = (match vAdvance with | null -> (fun _ -> -yScale) | f -> (fun g -> f.Invoke g))
          HOrigin = (match hOrigin with | null -> (fun _ -> ValueSome (struct (0, 0))) | f -> (fun g -> point (f.Invoke g)))
          VOrigin = (match vOrigin with | null -> (fun _ -> ValueNone) | f -> (fun g -> point (f.Invoke g)))
          HKerning = (match hKerning with | null -> (fun _ _ -> 0) | f -> (fun l r -> f.Invoke(l, r)))
          Extents = (match extents with | null -> (fun _ -> ValueNone) | f -> (fun g -> match f.Invoke g with | struct (true, x, y, w, h) -> ValueSome { XBearing = x; YBearing = y; Width = w; Height = h } | struct (false, _, _, _, _) -> ValueNone))
          ContourPoint = (match contourPoint with | null -> (fun _ _ -> ValueNone) | f -> (fun g i -> point (f.Invoke(g, i))))
          GlyphName = (match glyphName with | null -> (fun _ -> ValueNone) | f -> (fun g -> match f.Invoke g with | struct (true, n) -> ValueSome n | struct (false, _) -> ValueNone))
          GlyphFromName = (match glyphFromName with | null -> (fun _ -> ValueNone) | f -> (fun n -> match f.Invoke n with | struct (true, g) -> ValueSome g | struct (false, _) -> ValueNone))
          HFontExtents = (match hFontExtents with | null -> (fun () -> ValueNone) | f -> (fun () -> fontExtents (f.Invoke ())))
          VFontExtents = (match vFontExtents with | null -> (fun () -> ValueNone) | f -> (fun () -> fontExtents (f.Invoke ()))) }

    /// <summary>A font with functions, <c>hb_font_set_funcs</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="funcs">The functions.</param>
    /// <returns>The font answering through them.</returns>
    static member Apply (font: Font, funcs: FontFuncs) =
        Font.withFuncs (ValueSome funcs) font
