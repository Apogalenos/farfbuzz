namespace Farfbuzz

open System
open Farfbuzz.OpenType
open Farfbuzz.Var

/// <summary>The metric tags of <c>hb_ot_metrics_tag_t</c> plus the private per-table tags of the metrics reader.</summary>
type internal MetricTag =
    | HorizontalAscender
    | HorizontalDescender
    | HorizontalLineGap
    | HorizontalClippingAscent
    | HorizontalClippingDescent
    | VerticalAscender
    | VerticalDescender
    | VerticalLineGap
    | HorizontalCaretRise
    | HorizontalCaretRun
    | HorizontalCaretOffset
    | VerticalCaretRise
    | VerticalCaretRun
    | VerticalCaretOffset
    | XHeight
    | CapHeight
    | SubscriptEmXSize
    | SubscriptEmYSize
    | SubscriptEmXOffset
    | SubscriptEmYOffset
    | SuperscriptEmXSize
    | SuperscriptEmYSize
    | SuperscriptEmXOffset
    | SuperscriptEmYOffset
    | StrikeoutSize
    | StrikeoutOffset
    | UnderlineSize
    | UnderlineOffset
    | HorizontalAscenderOs2
    | HorizontalAscenderHhea
    | HorizontalDescenderOs2
    | HorizontalDescenderHhea
    | HorizontalLineGapOs2
    | HorizontalLineGapHhea

/// <summary>The OpenType metrics positions, <c>hb-ot-metrics.cc</c>.</summary>
[<RequireQualifiedAccess>]
module internal Metrics =
    /// <summary>The horizontal-ascender tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_ASCENDER</c>.</summary>
    [<Literal>]
    let private HorizontalAscenderTag = 0x68617363u<tag>

    /// <summary>The horizontal-descender tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_DESCENDER</c>.</summary>
    [<Literal>]
    let private HorizontalDescenderTag = 0x68647363u<tag>

    /// <summary>The horizontal-line-gap tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_LINE_GAP</c>.</summary>
    [<Literal>]
    let private HorizontalLineGapTag = 0x686C6770u<tag>

    /// <summary>The horizontal-clipping-ascent tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_CLIPPING_ASCENT</c>.</summary>
    [<Literal>]
    let private HorizontalClippingAscentTag = 0x68636C61u<tag>

    /// <summary>The horizontal-clipping-descent tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_CLIPPING_DESCENT</c>.</summary>
    [<Literal>]
    let private HorizontalClippingDescentTag = 0x68636C64u<tag>

    /// <summary>The vertical-ascender tag, <c>HB_OT_METRICS_TAG_VERTICAL_ASCENDER</c>.</summary>
    [<Literal>]
    let private VerticalAscenderTag = 0x76617363u<tag>

    /// <summary>The vertical-descender tag, <c>HB_OT_METRICS_TAG_VERTICAL_DESCENDER</c>.</summary>
    [<Literal>]
    let private VerticalDescenderTag = 0x76647363u<tag>

    /// <summary>The vertical-line-gap tag, <c>HB_OT_METRICS_TAG_VERTICAL_LINE_GAP</c>.</summary>
    [<Literal>]
    let private VerticalLineGapTag = 0x766C6770u<tag>

    /// <summary>The horizontal-caret-rise tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_CARET_RISE</c>.</summary>
    [<Literal>]
    let private HorizontalCaretRiseTag = 0x68637273u<tag>

    /// <summary>The horizontal-caret-run tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_CARET_RUN</c>.</summary>
    [<Literal>]
    let private HorizontalCaretRunTag = 0x6863726Eu<tag>

    /// <summary>The horizontal-caret-offset tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_CARET_OFFSET</c>.</summary>
    [<Literal>]
    let private HorizontalCaretOffsetTag = 0x68636F66u<tag>

    /// <summary>The vertical-caret-rise tag, <c>HB_OT_METRICS_TAG_VERTICAL_CARET_RISE</c>.</summary>
    [<Literal>]
    let private VerticalCaretRiseTag = 0x76637273u<tag>

    /// <summary>The vertical-caret-run tag, <c>HB_OT_METRICS_TAG_VERTICAL_CARET_RUN</c>.</summary>
    [<Literal>]
    let private VerticalCaretRunTag = 0x7663726Eu<tag>

    /// <summary>The vertical-caret-offset tag, <c>HB_OT_METRICS_TAG_VERTICAL_CARET_OFFSET</c>.</summary>
    [<Literal>]
    let private VerticalCaretOffsetTag = 0x76636F66u<tag>

    /// <summary>The x-height tag, <c>HB_OT_METRICS_TAG_X_HEIGHT</c>.</summary>
    [<Literal>]
    let private XHeightTag = 0x78686774u<tag>

    /// <summary>The cap-height tag, <c>HB_OT_METRICS_TAG_CAP_HEIGHT</c>.</summary>
    [<Literal>]
    let private CapHeightTag = 0x63706874u<tag>

    /// <summary>The subscript-em-x-size tag, <c>HB_OT_METRICS_TAG_SUBSCRIPT_EM_X_SIZE</c>.</summary>
    [<Literal>]
    let private SubscriptEmXSizeTag = 0x73627873u<tag>

    /// <summary>The subscript-em-y-size tag, <c>HB_OT_METRICS_TAG_SUBSCRIPT_EM_Y_SIZE</c>.</summary>
    [<Literal>]
    let private SubscriptEmYSizeTag = 0x73627973u<tag>

    /// <summary>The subscript-em-x-offset tag, <c>HB_OT_METRICS_TAG_SUBSCRIPT_EM_X_OFFSET</c>.</summary>
    [<Literal>]
    let private SubscriptEmXOffsetTag = 0x7362786Fu<tag>

    /// <summary>The subscript-em-y-offset tag, <c>HB_OT_METRICS_TAG_SUBSCRIPT_EM_Y_OFFSET</c>.</summary>
    [<Literal>]
    let private SubscriptEmYOffsetTag = 0x7362796Fu<tag>

    /// <summary>The superscript-em-x-size tag, <c>HB_OT_METRICS_TAG_SUPERSCRIPT_EM_X_SIZE</c>.</summary>
    [<Literal>]
    let private SuperscriptEmXSizeTag = 0x73707873u<tag>

    /// <summary>The superscript-em-y-size tag, <c>HB_OT_METRICS_TAG_SUPERSCRIPT_EM_Y_SIZE</c>.</summary>
    [<Literal>]
    let private SuperscriptEmYSizeTag = 0x73707973u<tag>

    /// <summary>The superscript-em-x-offset tag, <c>HB_OT_METRICS_TAG_SUPERSCRIPT_EM_X_OFFSET</c>.</summary>
    [<Literal>]
    let private SuperscriptEmXOffsetTag = 0x7370786Fu<tag>

    /// <summary>The superscript-em-y-offset tag, <c>HB_OT_METRICS_TAG_SUPERSCRIPT_EM_Y_OFFSET</c>.</summary>
    [<Literal>]
    let private SuperscriptEmYOffsetTag = 0x7370796Fu<tag>

    /// <summary>The strikeout-size tag, <c>HB_OT_METRICS_TAG_STRIKEOUT_SIZE</c>.</summary>
    [<Literal>]
    let private StrikeoutSizeTag = 0x73747273u<tag>

    /// <summary>The strikeout-offset tag, <c>HB_OT_METRICS_TAG_STRIKEOUT_OFFSET</c>.</summary>
    [<Literal>]
    let private StrikeoutOffsetTag = 0x7374726Fu<tag>

    /// <summary>The underline-size tag, <c>HB_OT_METRICS_TAG_UNDERLINE_SIZE</c>.</summary>
    [<Literal>]
    let private UnderlineSizeTag = 0x756E6473u<tag>

    /// <summary>The underline-offset tag, <c>HB_OT_METRICS_TAG_UNDERLINE_OFFSET</c>.</summary>
    [<Literal>]
    let private UnderlineOffsetTag = 0x756E646Fu<tag>

    /// <summary>The private horizontal-ascender OS/2 tag.</summary>
    [<Literal>]
    let private HorizontalAscenderOs2Tag = 0x4F617363u<tag>

    /// <summary>The private horizontal-ascender hhea tag.</summary>
    [<Literal>]
    let private HorizontalAscenderHheaTag = 0x48617363u<tag>

    /// <summary>The private horizontal-descender OS/2 tag.</summary>
    [<Literal>]
    let private HorizontalDescenderOs2Tag = 0x4F647363u<tag>

    /// <summary>The private horizontal-descender hhea tag.</summary>
    [<Literal>]
    let private HorizontalDescenderHheaTag = 0x48647363u<tag>

    /// <summary>The private horizontal-line-gap OS/2 tag.</summary>
    [<Literal>]
    let private HorizontalLineGapOs2Tag = 0x4F6C6770u<tag>

    /// <summary>The private horizontal-line-gap hhea tag.</summary>
    [<Literal>]
    let private HorizontalLineGapHheaTag = 0x486C6770u<tag>

    /// <summary>Maps a tag to its metric tag, none for a value outside the enum and the private tags.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The metric tag, or none.</returns>
    let ofTag (tag: Tag) : MetricTag voption =
        match tag with
        | HorizontalAscenderTag -> ValueSome HorizontalAscender
        | HorizontalDescenderTag -> ValueSome HorizontalDescender
        | HorizontalLineGapTag -> ValueSome HorizontalLineGap
        | HorizontalClippingAscentTag -> ValueSome HorizontalClippingAscent
        | HorizontalClippingDescentTag -> ValueSome HorizontalClippingDescent
        | VerticalAscenderTag -> ValueSome VerticalAscender
        | VerticalDescenderTag -> ValueSome VerticalDescender
        | VerticalLineGapTag -> ValueSome VerticalLineGap
        | HorizontalCaretRiseTag -> ValueSome HorizontalCaretRise
        | HorizontalCaretRunTag -> ValueSome HorizontalCaretRun
        | HorizontalCaretOffsetTag -> ValueSome HorizontalCaretOffset
        | VerticalCaretRiseTag -> ValueSome VerticalCaretRise
        | VerticalCaretRunTag -> ValueSome VerticalCaretRun
        | VerticalCaretOffsetTag -> ValueSome VerticalCaretOffset
        | XHeightTag -> ValueSome XHeight
        | CapHeightTag -> ValueSome CapHeight
        | SubscriptEmXSizeTag -> ValueSome SubscriptEmXSize
        | SubscriptEmYSizeTag -> ValueSome SubscriptEmYSize
        | SubscriptEmXOffsetTag -> ValueSome SubscriptEmXOffset
        | SubscriptEmYOffsetTag -> ValueSome SubscriptEmYOffset
        | SuperscriptEmXSizeTag -> ValueSome SuperscriptEmXSize
        | SuperscriptEmYSizeTag -> ValueSome SuperscriptEmYSize
        | SuperscriptEmXOffsetTag -> ValueSome SuperscriptEmXOffset
        | SuperscriptEmYOffsetTag -> ValueSome SuperscriptEmYOffset
        | StrikeoutSizeTag -> ValueSome StrikeoutSize
        | StrikeoutOffsetTag -> ValueSome StrikeoutOffset
        | UnderlineSizeTag -> ValueSome UnderlineSize
        | UnderlineOffsetTag -> ValueSome UnderlineOffset
        | HorizontalAscenderOs2Tag -> ValueSome HorizontalAscenderOs2
        | HorizontalAscenderHheaTag -> ValueSome HorizontalAscenderHhea
        | HorizontalDescenderOs2Tag -> ValueSome HorizontalDescenderOs2
        | HorizontalDescenderHheaTag -> ValueSome HorizontalDescenderHhea
        | HorizontalLineGapOs2Tag -> ValueSome HorizontalLineGapOs2
        | HorizontalLineGapHheaTag -> ValueSome HorizontalLineGapHhea
        | _ -> ValueNone

    /// <summary>The MVAR delta of a metric tag at the font's coordinates, <c>MVAR::get_var</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The unscaled delta.</returns>
    let private getVar (font: Font) (tag: Tag) : float32 =
        Mvar.getVar (font.Ot.Variations.Value.Mvar) font.Variations.Mvar tag

    /// <summary>Forces an ascender or descender sign, <c>_fix_ascender_descender</c> in double.</summary>
    /// <param name="value">The metric value with its MVAR delta.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The value, ascenders positive and descenders negative.</returns>
    let private fixAscenderDescender (value: float32) (tag: Tag) : float32 =
        match tag with
        | t when t = HorizontalAscenderTag || t = VerticalAscenderTag -> float32 (Math.Abs (float value))
        | t when t = HorizontalDescenderTag || t = VerticalDescenderTag -> float32 (-(Math.Abs (float value)))
        | _ -> value

    /// <summary>Scales and sign-fixes a metric on the x axis, gated on the table having data.</summary>
    /// <param name="font">The font.</param>
    /// <param name="hasData">Whether the source table has data.</param>
    /// <param name="value">The table value.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The scaled metric, or none.</returns>
    let private heaX (font: Font) (hasData: bool) (value: float32) (tag: Tag) : int voption =
        match hasData with
        | true -> ValueSome (Font.emScalefX font (fixAscenderDescender (value + getVar font tag) tag))
        | false -> ValueNone

    /// <summary>Scales and sign-fixes a metric on the y axis, gated on the table having data.</summary>
    /// <param name="font">The font.</param>
    /// <param name="hasData">Whether the source table has data.</param>
    /// <param name="value">The table value.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The scaled metric, or none.</returns>
    let private heaY (font: Font) (hasData: bool) (value: float32) (tag: Tag) : int voption =
        match hasData with
        | true -> ValueSome (Font.emScalefY font (fixAscenderDescender (value + getVar font tag) tag))
        | false -> ValueNone

    /// <summary>Scales a metric on the x axis without sign-fixing, gated on the table having data.</summary>
    /// <param name="font">The font.</param>
    /// <param name="hasData">Whether the source table has data.</param>
    /// <param name="value">The table value.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The scaled metric, or none.</returns>
    let private metricX (font: Font) (hasData: bool) (value: float32) (tag: Tag) : int voption =
        match hasData with
        | true -> ValueSome (Font.emScalefX font (value + getVar font tag))
        | false -> ValueNone

    /// <summary>Scales a metric on the y axis without sign-fixing, gated on the table having data.</summary>
    /// <param name="font">The font.</param>
    /// <param name="hasData">Whether the source table has data.</param>
    /// <param name="value">The table value.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The scaled metric, or none.</returns>
    let private metricY (font: Font) (hasData: bool) (value: float32) (tag: Tag) : int voption =
        match hasData with
        | true -> ValueSome (Font.emScalefY font (value + getVar font tag))
        | false -> ValueNone

    /// <summary>The six ascender, descender and line-gap positions shared by the font extents and <c>getPosition</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">One of the six common metric tags.</param>
    /// <returns>The scaled position, or none when no source table has data.</returns>
    let private positionCommon (font: Font) (tag: Tag) : int voption =
        let os2 = font.Ot.Os2.Value
        let hhea = font.Ot.Hhea.Value
        let vhea = font.Ot.Vhea.Value
        match tag with
        | t when t = HorizontalAscenderTag ->
            match Os2.useTypoMetrics os2 && os2.Present with
            | true -> heaY font true (float32 os2.TypoAscender) t
            | false -> heaY font hhea.Present (float32 hhea.Ascender) t
        | t when t = HorizontalDescenderTag ->
            match Os2.useTypoMetrics os2 && os2.Present with
            | true -> heaY font true (float32 os2.TypoDescender) t
            | false -> heaY font hhea.Present (float32 hhea.Descender) t
        | t when t = HorizontalLineGapTag ->
            match Os2.useTypoMetrics os2 && os2.Present with
            | true -> heaY font true (float32 os2.TypoLineGap) t
            | false -> heaY font hhea.Present (float32 hhea.LineGap) t
        | t when t = VerticalAscenderTag -> heaX font vhea.Present (float32 vhea.Ascender) t
        | t when t = VerticalDescenderTag -> heaX font vhea.Present (float32 vhea.Descender) t
        | t when t = VerticalLineGapTag -> heaX font vhea.Present (float32 vhea.LineGap) t
        | _ -> ValueNone

    /// <summary>Clamps an int64 into the int32 position range, <c>hb_clamp_to&lt;hb_position_t&gt;</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The clamped value.</returns>
    let private clampToInt32 (value: int64) : int =
        int (Math.Clamp(value, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Adds two positions, saturating instead of overflowing, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The first position.</param>
    /// <param name="b">The second position.</param>
    /// <returns>The saturated sum.</returns>
    let private saturateAdd (a: int) (b: int) : int =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Negates a position, saturating <c>Int32.MinValue</c> to <c>Int32.MaxValue</c>, <c>hb_saturate_neg</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The saturated negation.</returns>
    let private saturateNeg (value: int) : int =
        int (Math.Clamp(-(int64 value), int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>The caret-slope multiplier of a slanted font, <c>mult</c> of the caret cases.</summary>
    /// <param name="font">The font.</param>
    /// <param name="hhea">The horizontal metrics.</param>
    /// <returns>The multiplier, 1 when not slanted or the rise is out of range.</returns>
    let private caretMult (font: Font) (hhea: Hea) : int =
        match font.Slant <> 0f with
        | false -> 1
        | true ->
            let rise = uint32 (int hhea.CaretSlopeRise)
            let upem = uint32 font.Face.Upem
            match rise <> 0u && rise < upem with
            | true -> min (int (upem / rise)) 256
            | false -> 1

    /// <summary>The horizontal caret rise with its slant multiplier, <c>HB_OT_METRICS_TAG_HORIZONTAL_CARET_RISE</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The tag.</param>
    /// <param name="hhea">The horizontal metrics.</param>
    /// <returns>The scaled position, or none.</returns>
    let private horizontalCaretRise (font: Font) (tag: Tag) (hhea: Hea) : int voption =
        match metricY font hhea.Present (float32 hhea.CaretSlopeRise) tag with
        | ValueNone -> ValueNone
        | ValueSome position -> ValueSome (clampToInt32 (int64 position * int64 (caretMult font hhea)))

    /// <summary>The horizontal caret run with its slant multiplier, <c>HB_OT_METRICS_TAG_HORIZONTAL_CARET_RUN</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The tag.</param>
    /// <param name="hhea">The horizontal metrics.</param>
    /// <returns>The scaled position, or none.</returns>
    let private horizontalCaretRun (font: Font) (tag: Tag) (hhea: Hea) : int voption =
        let mult = caretMult font hhea
        let rise =
            match font.Slant <> 0f with
            | false -> 0
            | true ->
                match metricY font hhea.Present (float32 hhea.CaretSlopeRise) tag with
                | ValueSome value -> value
                | ValueNone -> 0
        match metricX font hhea.Present (float32 hhea.CaretSlopeRun) tag with
        | ValueNone -> ValueNone
        | ValueSome position ->
            let scaled = clampToInt32 (int64 position * int64 mult)
            match font.Slant <> 0f with
            | true -> ValueSome (saturateAdd scaled (Fixed.clampToInt32 (float (Fixed.roundf ((float32 mult * font.SlantXY) * float32 rise)))))
            | false -> ValueSome scaled

    /// <summary>Computes one metric position from its tag, <c>hb_ot_metrics_get_position</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The raw metric tag, for the MVAR lookup.</param>
    /// <param name="metricTag">The metric tag.</param>
    /// <returns>The scaled position, or none when the metric is absent.</returns>
    let private positionOf (font: Font) (tag: Tag) (metricTag: MetricTag) : int voption =
        let os2 = font.Ot.Os2.Value
        let hhea = font.Ot.Hhea.Value
        let vhea = font.Ot.Vhea.Value
        let post = font.Ot.Post.Value
        let v2HasData = os2.SxHeight <> 0s || os2.CapHeight <> 0s
        match metricTag with
        | HorizontalAscender | HorizontalDescender | HorizontalLineGap
        | VerticalAscender | VerticalDescender | VerticalLineGap -> positionCommon font tag
        | HorizontalClippingAscent -> metricY font os2.Present (float32 os2.WinAscent) tag
        | HorizontalClippingDescent -> metricY font os2.Present (float32 os2.WinDescent) tag
        | HorizontalCaretRise -> horizontalCaretRise font tag hhea
        | HorizontalCaretRun -> horizontalCaretRun font tag hhea
        | HorizontalCaretOffset -> metricX font hhea.Present (float32 hhea.CaretOffset) tag
        | VerticalCaretRise -> metricX font vhea.Present (float32 vhea.CaretSlopeRise) tag
        | VerticalCaretRun -> metricY font vhea.Present (float32 vhea.CaretSlopeRun) tag
        | VerticalCaretOffset -> metricY font vhea.Present (float32 vhea.CaretOffset) tag
        | XHeight -> metricY font v2HasData (float32 os2.SxHeight) tag
        | CapHeight -> metricY font v2HasData (float32 os2.CapHeight) tag
        | SubscriptEmXSize -> metricX font os2.Present (float32 os2.SubscriptXSize) tag
        | SubscriptEmYSize -> metricY font os2.Present (float32 os2.SubscriptYSize) tag
        | SubscriptEmXOffset -> metricX font os2.Present (float32 os2.SubscriptXOffset) tag
        | SubscriptEmYOffset -> metricY font os2.Present (float32 os2.SubscriptYOffset) tag
        | SuperscriptEmXSize -> metricX font os2.Present (float32 os2.SuperscriptXSize) tag
        | SuperscriptEmYSize -> metricY font os2.Present (float32 os2.SuperscriptYSize) tag
        | SuperscriptEmXOffset -> metricX font os2.Present (float32 os2.SuperscriptXOffset) tag
        | SuperscriptEmYOffset -> metricY font os2.Present (float32 os2.SuperscriptYOffset) tag
        | StrikeoutSize -> metricY font os2.Present (float32 os2.StrikeoutSize) tag
        | StrikeoutOffset -> metricY font os2.Present (float32 os2.StrikeoutPosition) tag
        | UnderlineSize -> metricY font (post.Version <> 0u) (float32 post.UnderlineThickness) tag
        | UnderlineOffset -> metricY font (post.Version <> 0u) (float32 post.UnderlinePosition) tag
        | HorizontalAscenderOs2 -> metricY font os2.Present (float32 os2.TypoAscender) tag
        | HorizontalAscenderHhea -> metricY font hhea.Present (float32 hhea.Ascender) tag
        | HorizontalDescenderOs2 -> metricY font os2.Present (float32 os2.TypoDescender) tag
        | HorizontalDescenderHhea -> metricY font hhea.Present (float32 hhea.Descender) tag
        | HorizontalLineGapOs2 -> metricY font os2.Present (float32 os2.TypoLineGap) tag
        | HorizontalLineGapHhea -> metricY font hhea.Present (float32 hhea.LineGap) tag

    /// <summary>Fetches a metric value from the font, <c>hb_ot_metrics_get_position</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The scaled position, or none when the font does not provide the metric.</returns>
    let getPosition (font: Font) (tag: Tag) : int voption =
        match ofTag tag with
        | ValueNone -> ValueNone
        | ValueSome metricTag -> positionOf font tag metricTag

    /// <summary>The x-height synthesized from the glyph 'x', <c>hb_ot_metrics_get_position_with_fallback</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The x-height.</returns>
    let private xHeightFallback (font: Font) : int =
        match Font.nominalGlyph font (uint32 'x') with
        | ValueNone -> font.YScale / 2
        | ValueSome glyph ->
            match Extents.glyph font glyph with
            | Ok (ValueSome extents) -> extents.YBearing
            | Ok ValueNone -> font.YScale / 2
            | Error _ -> font.YScale / 2

    /// <summary>The cap-height synthesized from the glyph 'O', <c>hb_ot_metrics_get_position_with_fallback</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The cap-height.</returns>
    let private capHeightFallback (font: Font) : int =
        match Font.nominalGlyph font (uint32 'O') with
        | ValueNone -> clampToInt32 (int64 font.YScale * 2L / 3L)
        | ValueSome glyph ->
            match Extents.glyph font glyph with
            | Ok (ValueSome extents) -> clampToInt32 (int64 extents.Height + 2L * int64 extents.YBearing)
            | Ok ValueNone -> clampToInt32 (int64 font.YScale * 2L / 3L)
            | Error _ -> clampToInt32 (int64 font.YScale * 2L / 3L)

    /// <summary>Synthesizes one metric position, <c>hb_ot_metrics_get_position_with_fallback</c>'s switch.</summary>
    /// <param name="font">The font.</param>
    /// <param name="metricTag">The metric tag.</param>
    /// <returns>The synthesized position.</returns>
    let private fallbackOf (font: Font) (metricTag: MetricTag) : int =
        match metricTag with
        | HorizontalAscender | HorizontalClippingAscent -> (Font.hExtentsWithFallback font).Ascender
        | VerticalAscender -> (Font.vExtentsWithFallback font).Ascender
        | HorizontalDescender | HorizontalClippingDescent -> (Font.hExtentsWithFallback font).Descender
        | VerticalDescender -> (Font.vExtentsWithFallback font).Descender
        | HorizontalLineGap -> (Font.hExtentsWithFallback font).LineGap
        | VerticalLineGap -> (Font.vExtentsWithFallback font).LineGap
        | HorizontalCaretRise | VerticalCaretRise -> 1
        | HorizontalCaretRun | VerticalCaretRun -> 0
        | HorizontalCaretOffset | VerticalCaretOffset -> 0
        | XHeight -> xHeightFallback font
        | CapHeight -> capHeightFallback font
        | StrikeoutSize | UnderlineSize -> font.YScale / 18
        | StrikeoutOffset ->
            match getPosition font HorizontalAscenderTag with
            | ValueSome position -> position / 2
            | ValueNone -> (Font.hExtentsWithFallback font).Ascender / 2
        | UnderlineOffset -> saturateNeg font.YScale / 18
        | SubscriptEmXSize | SuperscriptEmXSize -> clampToInt32 (int64 font.XScale * 10L / 12L)
        | SubscriptEmYSize | SuperscriptEmYSize -> clampToInt32 (int64 font.YScale * 10L / 12L)
        | SubscriptEmXOffset | SuperscriptEmXOffset -> 0
        | SubscriptEmYOffset | SuperscriptEmYOffset -> font.YScale / 5
        | HorizontalAscenderOs2 | HorizontalAscenderHhea
        | HorizontalDescenderOs2 | HorizontalDescenderHhea
        | HorizontalLineGapOs2 | HorizontalLineGapHhea -> 0

    /// <summary>Fetches a metric value, synthesizing one when the font does not provide it, <c>hb_ot_metrics_get_position_with_fallback</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The position, always a value.</returns>
    let getPositionWithFallback (font: Font) (tag: Tag) : int =
        match getPosition font tag with
        | ValueSome position when (tag <> StrikeoutSizeTag && tag <> UnderlineSizeTag) || position <> 0 -> position
        | _ ->
            match ofTag tag with
            | ValueNone -> 0
            | ValueSome metricTag -> fallbackOf font metricTag
