namespace Farfbuzz.HarfBuzzSharp.Bridge

open System.Collections.Immutable
open Farfbuzz

/// <summary>The advances of a run of glyphs.</summary>
[<Struct>]
type GlyphAdvances = { Advances: ImmutableArray<int> }

/// <summary>Glyph queries for the HarfBuzzSharp shell: the OpenType font functions of hb-ot-font.cc over a core font.</summary>
[<AbstractClass; Sealed>]
type Glyphs =
    /// <summary><c>hb_font_get_nominal_glyph</c> (hb-ot-font.cc:338-441): the glyph of a code point, 0 when there is none.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="unicode">The code point.</param>
    /// <returns>Whether a glyph was found, and the glyph.</returns>
    static member Nominal (font: Font, unicode: uint32) =
        match Font.nominalGlyph font unicode with
        | ValueSome glyph -> struct (true, glyph)
        | ValueNone -> struct (false, 0u)

    /// <summary><c>hb_font_get_variation_glyph</c>: the glyph of a code point followed by a variation selector, 0 when there is none.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="unicode">The code point.</param>
    /// <param name="selector">The variation selector.</param>
    /// <returns>Whether a glyph was found, and the glyph.</returns>
    static member Variation (font: Font, unicode: uint32, selector: uint32) =
        match Font.variationGlyph font unicode selector with
        | ValueSome glyph -> struct (true, glyph)
        | ValueNone -> struct (false, 0u)

    /// <summary><c>hb_font_get_glyph</c> (hb-font.hh <c>get_glyph</c>): the variation glyph when a selector is given, else the nominal glyph.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="unicode">The code point.</param>
    /// <param name="selector">The variation selector, 0 for none.</param>
    /// <returns>Whether a glyph was found, and the glyph.</returns>
    static member Glyph (font: Font, unicode: uint32, selector: uint32) =
        match selector with
        | 0u -> Glyphs.Nominal (font, unicode)
        | _ -> Glyphs.Variation (font, unicode, selector)

    /// <summary><c>hb_font_get_glyph_h_advance</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance at the font's scale.</returns>
    static member HAdvance (font: Font, glyph: uint32) =
        Font.hAdvance font glyph

    /// <summary><c>hb_font_get_glyph_v_advance</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance at the font's scale.</returns>
    static member VAdvance (font: Font, glyph: uint32) =
        Font.vAdvance font glyph

    /// <summary><c>hb_font_get_glyph_h_advances</c>, the span built here from the array the shell passes.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>One advance per glyph.</returns>
    static member HAdvances (font: Font, glyphs: uint32 array) =
        let advances = Font.hAdvances font (System.ReadOnlySpan glyphs)
        { Advances = advances }

    /// <summary><c>hb_font_get_glyph_v_advances</c>, the span built here from the array the shell passes.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>One advance per glyph.</returns>
    static member VAdvances (font: Font, glyphs: uint32 array) =
        let advances = Font.vAdvances font (System.ReadOnlySpan glyphs)
        { Advances = advances }

    /// <summary><c>hb_font_get_glyph_extents</c> (hb-ot-font.cc:811-900): the ink box of a glyph at the font's scale.</summary>
    /// <param name="font">The core font, over a face that has the outline tables.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>Whether the glyph has extents, the extents (zero when it has none), and the reason when the core cannot read this kind of outline yet (else empty).</returns>
    static member Extents (font: Font, glyph: uint32) =
        match Extents.glyph font glyph with
        | Ok (ValueSome extents) -> struct (true, extents, System.String.Empty)
        | Ok ValueNone -> struct (false, Unchecked.defaultof<GlyphExtents>, System.String.Empty)
        | Error _ -> struct (false, Unchecked.defaultof<GlyphExtents>, "the glyph outlines of this font cannot be read yet")

    /// <summary><c>hb_font_get_glyph_h_origin</c>: the functions' answer, (0, 0) for the OpenType functions.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>Whether there is an origin, and it.</returns>
    static member HOrigin (font: Font, glyph: uint32) =
        match font.Funcs with
        | ValueSome funcs ->
            match funcs.HOrigin glyph with
            | ValueSome (struct (x, y)) -> struct (true, x, y, System.String.Empty)
            | ValueNone -> struct (false, 0, 0, System.String.Empty)
        | ValueNone -> struct (true, 0, 0, System.String.Empty)

    /// <summary><c>hb_font_get_glyph_v_origin</c>: the functions' answer, the OpenType one (hb-ot-font.cc) without.</summary>
    /// <param name="font">The core font, over a face that has the outline tables.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>Whether there is an origin, the origin, and the reason when the core cannot read the outline yet (else empty).</returns>
    static member VOrigin (font: Font, glyph: uint32) =
        match font.Funcs with
        | ValueSome funcs ->
            match funcs.VOrigin glyph with
            | ValueSome (struct (x, y)) -> struct (true, x, y, System.String.Empty)
            | ValueNone -> struct (false, 0, 0, System.String.Empty)
        | ValueNone ->
            match Origins.vOrigin font glyph with
            | Ok (struct (x, y)) -> struct (true, x, y, System.String.Empty)
            | Error _ -> struct (false, 0, 0, "the glyph outlines of this font cannot be read yet")

    /// <summary><c>hb_font_get_glyph_h_kerning</c>: the functions' answer, 0 for the OpenType functions.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning.</returns>
    static member Kerning (font: Font, left: uint32, right: uint32) =
        match font.Funcs with
        | ValueSome funcs -> funcs.HKerning left right
        | ValueNone -> 0

    /// <summary><c>hb_font_get_glyph_contour_point</c>: the functions' answer; the OpenType functions have none.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="index">The point index.</param>
    /// <returns>Whether the point exists, and it.</returns>
    static member ContourPoint (font: Font, glyph: uint32, index: uint32) =
        match font.Funcs with
        | ValueSome funcs ->
            match funcs.ContourPoint glyph index with
            | ValueSome (struct (x, y)) -> struct (true, x, y)
            | ValueNone -> struct (false, 0, 0)
        | ValueNone -> struct (false, 0, 0)

    /// <summary><c>hb_font_get_glyph_name</c> with HarfBuzzSharp's 128-byte buffer: a name is cut to 127 characters.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>Whether the glyph has a name, and it.</returns>
    static member Name (font: Font, glyph: uint32) =
        match Font.glyphName font glyph with
        | ValueSome name -> struct (true, (if name.Length > 127 then name.Substring(0, 127) else name))
        | ValueNone -> struct (false, System.String.Empty)

    /// <summary><c>hb_font_get_glyph_from_name</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="name">The name.</param>
    /// <returns>Whether the name is known, and the glyph.</returns>
    static member FromName (font: Font, name: string) =
        match Font.glyphFromName font name with
        | ValueSome glyph -> struct (true, glyph)
        | ValueNone -> struct (false, 0u)

    /// <summary><c>hb_font_glyph_to_string</c>: the name, or <c>gidN</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The text.</returns>
    static member ToText (font: Font, glyph: uint32) =
        Font.glyphToString font glyph

    /// <summary><c>hb_font_glyph_from_string</c>: a name, <c>gidN</c> or <c>uniXXXX</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="text">The text.</param>
    /// <returns>Whether it resolves, and the glyph.</returns>
    static member FromText (font: Font, text: string) =
        match Font.glyphFromString font text with
        | ValueSome glyph -> struct (true, glyph)
        | ValueNone -> struct (false, 0u)
