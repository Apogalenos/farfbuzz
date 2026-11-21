namespace Farfbuzz

open System
open System.Runtime.CompilerServices
open Farfbuzz.OpenType
open Farfbuzz.Var

/// <summary>Glyph ink extents from the glyf header bounds, <c>hb-ot-font.cc:810-836</c> and <c>hb-font.hh:201-248</c>.</summary>
[<RequireQualifiedAccess>]
module internal Extents =
    /// <summary>Clamps an int64 into the int32 position range, <c>hb_clamp_to&lt;hb_position_t&gt;</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The clamped value.</returns>
    let private clampToInt32 (value: int64) : int =
        int (Math.Clamp(value, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Clamps a double into the int32 position range, <c>hb_clamp_to&lt;hb_position_t&gt;</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The clamped value.</returns>
    let private clampToInt32FromDouble (value: double) : int =
        match struct (value <= double Int32.MinValue, value >= double Int32.MaxValue) with
        | struct (true, _) -> Int32.MinValue
        | struct (false, true) -> Int32.MaxValue
        | struct (false, false) -> int value

    /// <summary>Scales header extents, <c>hb_font_t::scale_glyph_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="extents">The unscaled extents.</param>
    /// <returns>The scaled extents.</returns>
    let private scaleGlyphExtents (font: Font) (extents: GlyphExtents) : GlyphExtents =
        let x1 = Font.emFscaleX font extents.XBearing
        let y1 = Font.emFscaleY font extents.YBearing
        let x2 = x1 + Font.emFscaleX font extents.Width
        let y2 = y1 + Font.emFscaleY font extents.Height
        let xBearing = clampToInt32FromDouble (double (MathF.Floor x1))
        let yBearing = clampToInt32FromDouble (double (MathF.Floor y1))
        let width = clampToInt32FromDouble (double (MathF.Ceiling x2) - double xBearing)
        let height = clampToInt32FromDouble (double (MathF.Ceiling y2) - double yBearing)
        { XBearing = xBearing; YBearing = yBearing; Width = width; Height = height }

    /// <summary>Applies synthetic slant and emboldening, <c>hb_font_t::synthetic_glyph_extents</c>; phase 7 applies it
    /// in the fallback branch after paint and draw both fail, which this phase never reaches.</summary>
    /// <param name="font">The font.</param>
    /// <param name="extents">The extents.</param>
    /// <returns>The synthesized extents.</returns>
    let private syntheticGlyphExtents (font: Font) (extents: GlyphExtents) : GlyphExtents =
        let slanted =
            match font.SlantXY <> 0f with
            | false -> extents
            | true ->
                let y1 = int64 extents.YBearing
                let y2 = int64 extents.YBearing + int64 extents.Height
                let x1 = int64 extents.XBearing + int64 (MathF.Floor (MathF.Min (float32 y1 * font.SlantXY, float32 y2 * font.SlantXY)))
                let x2 = int64 extents.XBearing + int64 extents.Width + int64 (MathF.Ceiling (MathF.Max (float32 y1 * font.SlantXY, float32 y2 * font.SlantXY)))
                { extents with XBearing = clampToInt32 x1; Width = clampToInt32 (x2 - x1) }
        match font.XStrength <> 0 || font.YStrength <> 0 with
        | false -> slanted
        | true ->
            let yShift = match font.YScale < 0 with | true -> -font.YStrength | false -> font.YStrength
            let xShift = match font.XScale < 0 with | true -> -font.XStrength | false -> font.XStrength
            let xBearing =
                match font.EmboldenInPlace with
                | true -> clampToInt32 (int64 slanted.XBearing - int64 (xShift / 2))
                | false -> slanted.XBearing
            { slanted with
                YBearing = clampToInt32 (int64 slanted.YBearing + int64 yShift)
                Height = clampToInt32 (int64 slanted.Height - int64 yShift)
                XBearing = xBearing
                Width = clampToInt32 (int64 slanted.Width + int64 xShift) }

    /// <summary>The colour source of a face not ported, in dispatch order: only VARC, <c>VARC::get_extents</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The source, or none.</returns>
    let private bitmapOrColorSource (face: Face) : OutlineSource voption =
        match Face.tryFindTable face Tags.Varc with
        | ValueSome _ -> ValueSome VarcOutlines
        | ValueNone -> ValueNone

    /// <summary>Whether a face has strikes or a COLR table with data, the sources a synthetic font reads from the painted image.</summary>
    /// <param name="font">The font.</param>
    /// <returns>True when it has any.</returns>
    let private hasBitmapsOrColr (font: Font) =
        font.Ot.Sbix.Value.Strikes.Length > 0 || font.Ot.Cbdt.Value.Strikes.Length > 0 || Colr.hasData font.Ot.Colr.Value

    /// <summary>Computes the glyf header extents of one glyph, before synthetic state, <c>get_extents_at</c>'s non-variable branch.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled extents, or none when the glyph has no glyf answer.</returns>
    let private supportedGlyph (font: Font) (glyph: uint32) : GlyphExtents voption =
        let glyf = font.Ot.Glyf.Value
        match uint64 glyph < uint64 glyf.NumGlyphs with
        | false -> ValueNone
        | true ->
            match GlyfHeader.empty glyf glyph with
            | true -> ValueSome { XBearing = 0; YBearing = 0; Width = 0; Height = 0 }
            | false ->
                match GlyfHeader.bounds glyf glyph with
                | ValueNone -> ValueNone
                | ValueSome (struct (xMin, yMin, xMax, yMax)) ->
                    let xMinInt = int xMin
                    let yMinInt = int yMin
                    let xMaxInt = int xMax
                    let yMaxInt = int yMax
                    ValueSome
                        (scaleGlyphExtents font
                            { XBearing = Hmtx.leadingBearing font.Ot.Hmtx.Value glyph
                              YBearing = max yMinInt yMaxInt
                              Width = max xMinInt xMaxInt - min xMinInt xMaxInt
                              Height = min yMinInt yMaxInt - max yMinInt yMaxInt })

    /// <summary>The not-yet-ported outline source of a face found by looking for each table: the reference of
    /// <see cref="source"/>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The source, or none when the bitmap, glyf header or CFF1 extents (or no outlines) answer.</returns>
    let sourceReference (font: Font) =
        match bitmapOrColorSource font.Face, font.IsSynthetic && hasBitmapsOrColr font with
        | ValueSome notPorted, _ -> ValueSome notPorted
        | ValueNone, true -> ValueSome SyntheticOutlines
        | ValueNone, false -> ValueNone

    /// <summary>The not-yet-ported outline source of a face, <c>hb_ot_get_glyph_extents</c>'s dispatch order. A
    /// synthetic font over a face with bitmaps is not ported either: upstream then reads the extents from the
    /// painted image (<c>hb-font.hh:608-621</c>).</summary>
    /// <param name="font">The font.</param>
    /// <returns>The source, or none when the bitmap, glyf header or CFF1 extents (or no outlines) answer.</returns>
    let source (font: Font) =
        match font.Ot.Outlines.Value.Plain with
        | true -> ValueNone
        | false ->
            match bitmapOrColorSource font.Face, font.IsSynthetic && hasBitmapsOrColr font with
            | ValueSome notPorted, _ -> ValueSome notPorted
            | ValueNone, true -> ValueSome SyntheticOutlines
            | ValueNone, false -> ValueNone

    /// <summary>Converts bitmap extents to font units, <c>roundf (value * scale)</c> clamped, per axis.</summary>
    /// <param name="xScale">The horizontal upem-over-ppem ratio.</param>
    /// <param name="yScale">The vertical upem-over-ppem ratio.</param>
    /// <param name="extents">The pixel extents.</param>
    /// <returns>The font-unit extents.</returns>
    let private scaleToFontUnits (xScale: float32) (yScale: float32) (extents: GlyphExtents) : GlyphExtents =
        { XBearing = Fixed.clampToInt32 (double (Fixed.roundf (float32 extents.XBearing * xScale)))
          YBearing = Fixed.clampToInt32 (double (Fixed.roundf (float32 extents.YBearing * yScale)))
          Width = Fixed.clampToInt32 (double (Fixed.roundf (float32 extents.Width * xScale)))
          Height = Fixed.clampToInt32 (double (Fixed.roundf (float32 extents.Height * yScale))) }

    /// <summary>The sbix extents of a glyph before synthetic state, in upstream's order: convert to font units, then
    /// scale (<c>sbix::accelerator_t::get_png_extents</c>).</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled extents when the sbix table answers, else none.</returns>
    let private sbixRaw (font: Font) (glyph: uint32) : GlyphExtents voption =
        match Sbix.glyphBitmap font.Ot.Sbix.Value font.XPpem font.YPpem glyph with
        | ValueNone -> ValueNone
        | ValueSome (struct (raw, ppem)) ->
            let scale = float32 font.Face.Upem / float32 ppem
            ValueSome (scaleGlyphExtents font (scaleToFontUnits scale scale raw))

    /// <summary>The CBDT extents of a glyph before synthetic state, in upstream's order: scale, then convert to font
    /// units (<c>CBDT::accelerator_t::get_extents</c>).</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled extents when the CBDT table answers, else none.</returns>
    let private cbdtRaw (font: Font) (glyph: uint32) : GlyphExtents voption =
        match Cbdt.glyphMetrics font.Ot.Cbdt.Value font.XPpem font.YPpem glyph with
        | ValueNone -> ValueNone
        | ValueSome (struct (raw, ppemX, ppemY)) ->
            let xScale = float32 font.Face.Upem / float32 ppemX
            let yScale = float32 font.Face.Upem / float32 ppemY
            ValueSome (scaleToFontUnits xScale yScale (scaleGlyphExtents font raw))

    /// <summary>The bitmap extents of a glyph before synthetic state, sbix first and then CBDT,
    /// <c>hb_ot_get_glyph_extents</c>'s first two sources (<c>hb-ot-font.cc:820-821</c>).</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled extents when a bitmap table answers, else none.</returns>
    let private bitmapRaw (font: Font) (glyph: uint32) : GlyphExtents voption =
        match sbixRaw font glyph with
        | ValueSome extents -> ValueSome extents
        | ValueNone -> cbdtRaw font glyph

    /// <summary>The COLR extents of a glyph before synthetic state, <c>COLR::accelerator_t::get_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents when the COLR table answers for the glyph, else none.</returns>
    let private colrRaw (font: Font) (glyph: uint32) =
        PaintExtents.glyph font font.Ot.Colr.Value glyph

    /// <summary>The COLR stage of the extents dispatch for a font that may be synthetic or may paint through VARC.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents or none, or the not-yet-ported source the answer would need.</returns>
    let private colrStage (font: Font) (glyph: uint32) =
        match Colr.hasData font.Ot.Colr.Value, font.IsSynthetic, bitmapOrColorSource font.Face with
        | false, _, _ -> Ok ValueNone
        | true, true, _ -> Error (OutlinesNotPorted SyntheticOutlines)
        | true, false, ValueSome notPorted -> Error (OutlinesNotPorted notPorted)
        | true, false, ValueNone -> Ok (colrRaw font glyph)

    /// <summary>The scaled CFF1 bounds of one glyph before synthetic state, <c>cff1-&gt;get_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cff">The CFF1 accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bounds, or none when the glyph has none.</returns>
    let private cff1Raw (font: Font) (cff: Cff1Accel) (glyph: uint32) : GlyphExtents voption =
        match glyph < cff.GlyphCount with
        | false -> ValueNone
        | true ->
            match Cff1Extents.glyphFast cff glyph with
            | ValueNone -> ValueNone
            | ValueSome extents -> ValueSome (scaleGlyphExtents font extents)

    /// <summary>The raw CFF1 bounds of one glyph, or none when the face has no CFF1 table.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bounds, or none.</returns>
    let private cff1RawOrNone (font: Font) (glyph: uint32) : GlyphExtents voption =
        match Face.tryFindTable font.Face Tags.Cff |> ValueOption.isSome with
        | false -> ValueNone
        | true -> cff1Raw font font.Ot.Cff1.Value glyph

    /// <summary>The scaled CFF2 bounds of one glyph before synthetic state, <c>cff2-&gt;get_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cff">The CFF2 accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bounds, or none when the glyph has none.</returns>
    let private cff2Raw (font: Font) (cff: Cff2Accel) (glyph: uint32) : GlyphExtents voption =
        match glyph < uint32 cff.NumGlyphs with
        | false -> ValueNone
        | true ->
            match Cff2Extents.glyphFast cff font.Variations.Coords font.Variations.Cff2.Value glyph with
            | ValueNone -> ValueNone
            | ValueSome extents -> ValueSome (scaleGlyphExtents font extents)

    /// <summary>The raw CFF2 bounds of one glyph, or none when the face has no CFF2 table.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bounds, or none.</returns>
    let private cff2RawOrNone (font: Font) (glyph: uint32) : GlyphExtents voption =
        match Face.tryFindTable font.Face Tags.Cff2 |> ValueOption.isSome with
        | false -> ValueNone
        | true -> cff2Raw font font.Ot.Cff2.Value glyph

    /// <summary>The raw CFF bounds of one glyph, CFF2 first and then CFF1, or none.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bounds, or none.</returns>
    let private cffRawOrNone (font: Font) (glyph: uint32) : GlyphExtents voption =
        match cff2RawOrNone font glyph with
        | ValueSome extents -> ValueSome extents
        | ValueNone -> cff1RawOrNone font glyph

    /// <summary>The glyf extents of a face, from the varied contour points when the coordinates carry a non-zero value.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled extents, or none when the glyph has no answer.</returns>
    let private variedOrHeaderExtents (font: Font) (glyph: uint32) : GlyphExtents voption =
        match font.Variations.Coords.HasNonZero with
        | false -> supportedGlyph font glyph
        | true ->
            match GlyphOutline.extentsWithVariations font.Ot.GlyfPoints.Value font.Ot.Gvar.Value font.Variations.Coords glyph with
            | ValueNone -> ValueNone
            | ValueSome extents -> ValueSome (scaleGlyphExtents font extents)

    /// <summary>The scaled glyf header extents of one glyph read once, <c>get_extents_at</c>'s non-variable branch.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled extents, or none when the glyph has no glyf answer.</returns>
    let private supportedGlyphOnce (font: Font) (glyph: uint32) =
        let glyf = font.Ot.Glyf.Value
        match uint64 glyph < uint64 glyf.NumGlyphs with
        | false -> ValueNone
        | true ->
            match GlyfHeader.classify glyf glyph with
            | GlyfHeader.NoGlyph -> ValueNone
            | GlyfHeader.EmptyGlyph -> ValueSome { XBearing = 0; YBearing = 0; Width = 0; Height = 0 }
            | GlyfHeader.Bounds (xMin, yMin, xMax, yMax) ->
                let xMinInt = int xMin
                let yMinInt = int yMin
                let xMaxInt = int xMax
                let yMaxInt = int yMax
                ValueSome
                    (scaleGlyphExtents font
                        { XBearing = Hmtx.leadingBearing font.Ot.Hmtx.Value glyph
                          YBearing = max yMinInt yMaxInt
                          Width = max xMinInt xMaxInt - min xMinInt xMaxInt
                          Height = min yMinInt yMaxInt - max yMinInt yMaxInt })

    /// <summary>The glyf extents of a face, from the varied contour points when the coordinates carry a non-zero value, the
    /// header read once otherwise.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled extents, or none when the glyph has no answer.</returns>
    let private variedOrHeaderOnce (font: Font) (glyph: uint32) =
        match font.Variations.Coords.HasNonZero with
        | false -> supportedGlyphOnce font glyph
        | true ->
            let gvar = font.Ot.Gvar.Value
            let scalars =
                match font.Memo.GvarScalars.TryGet () with
                | ValueSome held -> held.Scalars
                | ValueNone -> (font.Memo.GvarScalars.Set { Scalars = GlyphOutline.sharedScalars gvar font.Variations.Coords }).Scalars
            match GlyphOutline.extentsWithVariationsFast font.Ot.GlyfPoints.Value gvar font.Variations.Coords scalars glyph with
            | ValueNone -> ValueNone
            | ValueSome extents -> ValueSome (scaleGlyphExtents font extents)

    /// <summary>The raw CFF bounds of one glyph, CFF2 first and then CFF1, by the tables the plan says the face has.</summary>
    /// <param name="plan">The face's outline plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bounds, or none.</returns>
    let private cffRawPlanned (plan: OutlinePlan) (font: Font) (glyph: uint32) =
        let fromCff2 = match plan.HasCff2 with | true -> cff2Raw font font.Ot.Cff2.Value glyph | false -> ValueNone
        match fromCff2 with
        | ValueSome extents -> ValueSome extents
        | ValueNone -> match plan.HasCff1 with | true -> cff1Raw font font.Ot.Cff1.Value glyph | false -> ValueNone

    /// <summary>The extents the colour, glyf and CFF tables give a glyph the bitmap tables did not answer, in upstream's order.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none.</returns>
    let private colrOrOutlineRaw (font: Font) (glyph: uint32) =
        match colrRaw font glyph with
        | ValueSome extents -> ValueSome extents
        | ValueNone ->
            match font.Ot.Glyf.Value.Present with
            | true -> variedOrHeaderExtents font glyph
            | false -> cffRawOrNone font glyph

    /// <summary>The glyph extents the vertical-origin fallback uses, found by looking for each table: the reference of
    /// <see cref="value"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the glyph has none.</returns>
    let valueReference (font: Font) (glyph: uint32) =
        let raw =
            match bitmapRaw font glyph with
            | ValueSome extents -> ValueSome extents
            | ValueNone -> colrOrOutlineRaw font glyph
        match raw with
        | ValueNone -> ValueNone
        | ValueSome extents -> ValueSome (syntheticGlyphExtents font extents)

    /// <summary>The glyph extents the vertical-origin fallback uses, <c>hb_font_t::get_glyph_extents</c>: the
    /// bitmap extents when sbix or CBDT answers, else the glyf header or CFF bounds, with synthetic slant and
    /// emboldening applied.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the glyph has none.</returns>
    let value (font: Font) (glyph: uint32) =
        let plan = font.Ot.Outlines.Value
        let raw =
            match plan.Plain with
            | true ->
                match font.Ot.Glyf.Value.Present with
                | true -> variedOrHeaderOnce font glyph
                | false -> cffRawPlanned plan font glyph
            | false ->
                match bitmapRaw font glyph with
                | ValueSome extents -> ValueSome extents
                | ValueNone -> colrOrOutlineRaw font glyph
        match raw with
        | ValueNone -> ValueNone
        | ValueSome extents -> ValueSome (syntheticGlyphExtents font extents)

    /// <summary>The CFF1 extents of a glyph, or none; a synthetic glyph stays not ported.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cff">The CFF1 accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The result.</returns>
    let private cff1Extents (font: Font) (cff: Cff1Accel) (glyph: uint32) : Result<GlyphExtents voption, ShapeError> =
        match glyph < cff.GlyphCount with
        | false -> Ok ValueNone
        | true ->
            match font.IsSynthetic with
            | true -> Error (OutlinesNotPorted SyntheticOutlines)
            | false -> Ok (cff1Raw font cff glyph)

    /// <summary>The CFF2 extents of a glyph, or none; a synthetic glyph stays not ported.</summary>
    /// <param name="font">The font.</param>
    /// <param name="cff">The CFF2 accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The result.</returns>
    let private cff2Extents (font: Font) (cff: Cff2Accel) (glyph: uint32) : Result<GlyphExtents voption, ShapeError> =
        match glyph < uint32 cff.NumGlyphs with
        | false -> Ok ValueNone
        | true ->
            match font.IsSynthetic with
            | true -> Error (OutlinesNotPorted SyntheticOutlines)
            | false -> Ok (cff2Raw font cff glyph)

    /// <summary>The CFF extents of a glyph, or none; a synthetic glyph stays not ported.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The result.</returns>
    let private cffOrNone (font: Font) (glyph: uint32) : Result<GlyphExtents voption, ShapeError> =
        match Face.tryFindTable font.Face Tags.Cff2 |> ValueOption.isSome, Face.tryFindTable font.Face Tags.Cff |> ValueOption.isSome with
        | true, _ -> cff2Extents font font.Ot.Cff2.Value glyph
        | false, true -> cff1Extents font font.Ot.Cff1.Value glyph
        | false, false -> Ok ValueNone

    /// <summary>The glyf or CFF answer for a face without bitmap or colour tables.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The result.</returns>
    let private glyfOrCff (font: Font) (glyph: uint32) : Result<GlyphExtents voption, ShapeError> =
        let glyf = font.Ot.Glyf.Value
        match glyf.Present with
        | false -> cffOrNone font glyph
        | true ->
            match uint64 glyph < uint64 glyf.NumGlyphs with
            | false -> cffOrNone font glyph
            | true ->
                match font.IsSynthetic with
                | true -> Error (OutlinesNotPorted SyntheticOutlines)
                | false -> Ok (variedOrHeaderExtents font glyph)

    /// <summary>Computes one glyph's ink extents the way the tables are looked up one by one, <c>hb_font_get_glyph_extents</c>: the reference of <see cref="glyph"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, none when the glyph has none, or a not-yet-ported outline source.</returns>
    let glyphReference (font: Font) (glyph: uint32) =
        match bitmapRaw font glyph, font.IsSynthetic with
        | ValueSome _, true -> Error (OutlinesNotPorted SyntheticOutlines)
        | ValueSome extents, false -> Ok (ValueSome extents)
        | ValueNone, _ ->
            match colrStage font glyph with
            | Error notPorted -> Error notPorted
            | Ok (ValueSome extents) -> Ok (ValueSome extents)
            | Ok ValueNone ->
                match bitmapOrColorSource font.Face with
                | ValueSome notPorted -> Error (OutlinesNotPorted notPorted)
                | ValueNone -> glyfOrCff font glyph

    /// <summary>The CFF extents of a glyph by the plan, or none; a synthetic glyph stays not ported.</summary>
    /// <param name="plan">The face's outline plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The result.</returns>
    let private cffPlanned (plan: OutlinePlan) (font: Font) (glyph: uint32) =
        match plan.HasCff2, plan.HasCff1 with
        | true, _ -> cff2Extents font font.Ot.Cff2.Value glyph
        | false, true -> cff1Extents font font.Ot.Cff1.Value glyph
        | false, false -> Ok ValueNone

    /// <summary>The glyf or CFF answer for a face the plan says has no bitmap or colour tables.</summary>
    /// <param name="plan">The face's outline plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The result.</returns>
    let private glyfOrCffPlanned (plan: OutlinePlan) (font: Font) (glyph: uint32) =
        let glyf = font.Ot.Glyf.Value
        match glyf.Present && uint64 glyph < uint64 glyf.NumGlyphs with
        | false -> cffPlanned plan font glyph
        | true ->
            match font.IsSynthetic with
            | true -> Error (OutlinesNotPorted SyntheticOutlines)
            | false -> Ok (variedOrHeaderOnce font glyph)

    /// <summary>Asks the extents function of a font's functions, out of line so that the lookup of a font without functions stays as small as it was.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private glyphViaFuncs (funcs: FontFuncs) (glyph: uint32) =
        Ok (funcs.Extents glyph)

    /// <summary>Computes one glyph's ink extents, <c>hb_font_get_glyph_extents</c>; a face with no bitmap or colour tables is
    /// answered without looking for them, and a glyf header is read once.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, none when the glyph has none, or a not-yet-ported outline source.</returns>
    let glyph (font: Font) (glyph: uint32) =
        match font.Funcs with
        | ValueSome funcs -> glyphViaFuncs funcs glyph
        | ValueNone ->
            let plan = font.Ot.Outlines.Value
            match plan.Plain with
            | false -> glyphReference font glyph
            | true -> glyfOrCffPlanned plan font glyph
