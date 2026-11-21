namespace Farfbuzz

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open Farfbuzz.Var
open Farfbuzz.OpenType

/// <summary>Font-wide vertical metrics.</summary>
[<Struct>]
type FontExtents = { Ascender: int; Descender: int; LineGap: int }

/// <summary>The font extents a font holds once built.</summary>
[<NoEquality; NoComparison>]
type internal HExtentsHeld = { Extents: FontExtents }

/// <summary>The advance of every glyph of a font, fixed by the font's scale, coordinates and emboldening: the metrics table bytes with the
/// counts that bound them, the multiplier, the synthetic strength and the finished advance of the glyphs the table does not hold. A plan
/// that <c>Reads</c> its table answers a glyph below <c>Bearings</c> from the table and any other glyph with <c>Absent</c>. A plan that is
/// also <c>Direct</c> (it reads, adds no strength, and its table holds every long metric it names) answers by comparisons alone: a glyph
/// below <c>Longs</c> reads its own long metric, a glyph from <c>Longs</c> to <c>Cover</c> has the last long metric, <c>Tail</c>, and a glyph
/// from <c>Cover</c> on has <c>Absent</c>. A plan that does not read has <c>Longs</c> of zero and a <c>Cover</c> no glyph reaches, so that
/// every glyph is sent to the code that builds the plan, pages the advances of a font with a variation to apply to them (working a glyph
/// out the first time it is asked for) or asks the functions of a font that has some.</summary>
[<NoEquality; NoComparison>]
type internal AdvancePlan =
    { Direct: bool
      Reads: bool
      Table: byte array
      Offset: int
      Bearings: int
      LastLong: int
      Longs: int
      Cover: uint64
      Tail: int
      Beyond: int
      Absent: int
      Multiplier: int64
      Strength: int
      Paged: bool
      Pages: IntPages }

/// <summary>Where a font's vertical origins come from, <c>hb-ot-font.cc:632-806</c>, or the outline source that would answer them
/// and is not ported.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal VOriginKind =
    | VorgOrigin
    | GlyfPhantomOrigin
    | ExtentsOrigin
    | ColrNotPorted
    | VarcNotPorted
    | SyntheticNotPorted

/// <summary>The vertical-origin source of a font with the font extents its fallback reads, held once built.</summary>
[<NoEquality; NoComparison>]
type internal VOriginPlan = { Kind: VOriginKind; Extents: FontExtents; Pages: IntPages }

/// <summary>The scalars of the shared gvar tuples at a font's coordinates, held once worked out.</summary>
[<NoEquality; NoComparison>]
type internal GvarScalarsHeld = { Scalars: float32 array }

/// <summary>The values a font computes from its own state at the first request and then holds: its font extents, vertical-origin plan
/// and gvar scalars (its two advance plans are cells of the font itself, one load nearer). A font made by any change of scale, coordinates or synthetic state has an empty memo, so none of them is ever stale.</summary>
[<NoEquality; NoComparison>]
type internal FontMemo =
    { HExtents: Memo<HExtentsHeld>
      VOrigin: Memo<VOriginPlan>
      GvarScalars: Memo<GvarScalarsHeld> }

/// <summary>Making the memo of a font.</summary>
[<RequireQualifiedAccess>]
module internal FontMemo =
    /// <summary>The plan a memo holds before one is built: it has no direct reading, so the reader that finds it builds the real one.</summary>
    let private unbuiltPlan =
        { Direct = false
          Reads = false
          Table = Array.empty
          Offset = 0
          Bearings = 0
          LastLong = 0
          Longs = 0
          Cover = UInt64.MaxValue
          Tail = 0
          Beyond = 0
          Absent = 0
          Multiplier = 0L
          Strength = 0
          Paged = false
          Pages = IntPages(0) }

    /// <summary>An empty cell for an advance plan: it holds the unbuilt plan, which has no direct reading.</summary>
    /// <returns>The cell.</returns>
    let advanceCell () = Memo<AdvancePlan>(unbuiltPlan)

    /// <summary>An empty memo, for a font whose state has just been set.</summary>
    /// <returns>The memo.</returns>
    let create () =
        { HExtents = Memo<HExtentsHeld>()
          VOrigin = Memo<VOriginPlan>()
          GvarScalars = Memo<GvarScalarsHeld>() }

/// <summary>The replaceable glyph functions of a font, <c>hb_font_funcs_t</c>: every glyph question the shapers ask, answered at the font's own scale as
/// HarfBuzz's callbacks answer (hb-font.hh). A font without them (<c>Funcs = ValueNone</c>) answers from its face, the OpenType functions of
/// <c>hb-ot-font.cc</c>.</summary>
[<NoEquality; NoComparison>]
type internal FontFuncs =
    { Nominal: uint32 -> uint32 voption
      Variation: uint32 -> uint32 -> uint32 voption
      HAdvance: uint32 -> int
      VAdvance: uint32 -> int
      HOrigin: uint32 -> struct (int * int) voption
      VOrigin: uint32 -> struct (int * int) voption
      HKerning: uint32 -> uint32 -> int
      Extents: uint32 -> GlyphExtents voption
      ContourPoint: uint32 -> uint32 -> struct (int * int) voption
      GlyphName: uint32 -> string voption
      GlyphFromName: string -> uint32 voption
      HFontExtents: unit -> FontExtents voption
      VFontExtents: unit -> FontExtents voption }

/// <summary>A font: a face, its accelerators, and the scale and synthetic state.</summary>
[<NoEquality; NoComparison>]
type Font =
    internal
        { Face: Face
          Ot: OtFace
          XScale: int
          YScale: int
          XPpem: uint32
          YPpem: uint32
          Ptem: float32
          XEmbolden: float32
          YEmbolden: float32
          EmboldenInPlace: bool
          Slant: float32
          XMultF: float32
          YMultF: float32
          XMult: int64
          YMult: int64
          XStrength: int
          YStrength: int
          SlantXY: float32
          IsSynthetic: bool
          Variations: FontVariations
          Memo: FontMemo
          Funcs: FontFuncs voption
          HPlan: Memo<AdvancePlan>
          VPlan: Memo<AdvancePlan> }

/// <summary>The default font and the OpenType font funcs of <c>hb-ot-font.cc</c>.</summary>
[<RequireQualifiedAccess>]
module Font =
    /// <summary>Rounds half towards positive infinity, as HarfBuzz's <c>_hb_roundf</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The rounded value.</returns>
    let private roundHalfUp (value: float32) : float32 =
        float32 (Math.Floor(float value + 0.5))

    /// <summary>Returns the machine-int absolute value, including C's wrapping <c>abs(INT_MIN)</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The wrapping absolute value.</returns>
    let private absWrapping (value: int) : int =
        match value with
        | Int32.MinValue -> Int32.MinValue
        | v when v < 0 -> -v
        | v -> v

    /// <summary>A font whose state has just been set: the same font with empty cells, so that no plan it holds is ever stale.</summary>
    /// <param name="font">The font with its new state.</param>
    /// <returns>The font with empty memo and advance cells.</returns>
    let private renewed (font: Font) =
        { font with Memo = FontMemo.create (); HPlan = FontMemo.advanceCell (); VPlan = FontMemo.advanceCell () }

    /// <summary>Recomputes the multipliers and synthetic state of a font whose cells are new, <c>hb_font_t::changed</c>.</summary>
    /// <param name="font">The font with new state.</param>
    /// <returns>The font with derived fields refreshed.</returns>
    let private derived (font: Font) =
        let upem = float32 font.Face.Upem
        { font with
            XMultF = float32 font.XScale / upem
            YMultF = float32 font.YScale / upem
            XMult = int64 (float32 (int64 font.XScale * 65536L) / upem)
            YMult = int64 (float32 (int64 font.YScale * 65536L) / upem)
            IsSynthetic = font.XEmbolden <> 0f || font.YEmbolden <> 0f || font.Slant <> 0f
            XStrength = int (roundHalfUp (float32 (absWrapping font.XScale) * font.XEmbolden))
            YStrength = int (roundHalfUp (float32 (absWrapping font.YScale) * font.YEmbolden))
            SlantXY =
                match font.YScale with
                | 0 -> 0f
                | _ -> font.Slant * float32 font.XScale / float32 font.YScale }

    /// <summary>Recomputes the multipliers and synthetic state of a font whose state has been set, and empties its cells,
    /// <c>hb_font_t::changed</c>.</summary>
    /// <param name="font">The font with new state.</param>
    /// <returns>The font with derived fields refreshed and empty cells.</returns>
    let private changed (font: Font) = renewed (derived font)

    /// <summary>Returns the face's accelerators, building them once per face: the face holds the cell they go in, which
    /// every font made from it shares.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The accelerators.</returns>
    let private otFaceOf (face: Face) =
        face.Accelerators.GetOrAdd(fun () -> OtFace.create face :> obj) :?> OtFace

    /// <summary>Creates the default font for a face: scale = upem and synthetic state cleared.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The font.</returns>
    let create (face: Face) : Font =
        let upem = face.Upem
        let ot = otFaceOf face
        let namedInstance =
            match face.Index >>> 16 with
            | 0u -> FontVariations.noNamedInstance
            | high -> high - 1u
        let variations = FontVariations.ofVariations (ot.Variations.Value) namedInstance ImmutableArray<Variation>.Empty
        derived
            { Face = face
              Ot = ot
              XScale = upem
              YScale = upem
              XPpem = 0u
              YPpem = 0u
              Ptem = 0f
              XEmbolden = 0f
              YEmbolden = 0f
              EmboldenInPlace = true
              Slant = 0f
              XMultF = 0f
              YMultF = 0f
              XMult = 0L
              YMult = 0L
              XStrength = 0
              YStrength = 0
              SlantXY = 0f
              IsSynthetic = false
              Variations = variations
              Funcs = ValueNone
              Memo = FontMemo.create ()
              HPlan = FontMemo.advanceCell ()
              VPlan = FontMemo.advanceCell () }

    /// <summary>A font with other functions, or with none, and an empty memo: a plan the font built while it answered from its face is
    /// never read by a font that answers through functions, nor the reverse.</summary>
    /// <param name="funcs">The functions, none for the OpenType ones.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the functions replaced.</returns>
    let internal withFuncs (funcs: FontFuncs voption) (font: Font) =
        renewed { font with Funcs = funcs }

    /// <summary>Sets the scale, returning the input font when both values are unchanged.</summary>
    /// <param name="xScale">The horizontal scale.</param>
    /// <param name="yScale">The vertical scale.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new scale.</returns>
    let withScale (xScale: int) (yScale: int) (font: Font) : Font =
        match font.XScale = xScale && font.YScale = yScale with
        | true -> font
        | false -> changed { font with XScale = xScale; YScale = yScale }

    /// <summary>Sets the pixels per em, returning the input font when both values are unchanged.</summary>
    /// <param name="xPpem">The horizontal ppem.</param>
    /// <param name="yPpem">The vertical ppem.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new ppem.</returns>
    let withPpem (xPpem: uint32) (yPpem: uint32) (font: Font) : Font =
        match font.XPpem = xPpem && font.YPpem = yPpem with
        | true -> font
        | false -> changed { font with XPpem = xPpem; YPpem = yPpem }

    /// <summary>Sets the point size, returning the input font when unchanged.</summary>
    /// <param name="ptem">The point size.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new point size.</returns>
    let withPtem (ptem: float32) (font: Font) : Font =
        match font.Ptem = ptem with
        | true -> font
        | false -> changed { font with Ptem = ptem }

    /// <summary>Sets synthetic boldness, returning the input font when all three values are unchanged.</summary>
    /// <param name="xEmbolden">The horizontal strength.</param>
    /// <param name="yEmbolden">The vertical strength.</param>
    /// <param name="inPlace">Whether advances stay unchanged.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new synthetic boldness.</returns>
    let withSyntheticBold (xEmbolden: float32) (yEmbolden: float32) (inPlace: bool) (font: Font) : Font =
        match font.XEmbolden = xEmbolden && font.YEmbolden = yEmbolden && font.EmboldenInPlace = inPlace with
        | true -> font
        | false -> changed { font with XEmbolden = xEmbolden; YEmbolden = yEmbolden; EmboldenInPlace = inPlace }

    /// <summary>Sets synthetic slant, returning the input font when unchanged.</summary>
    /// <param name="slant">The slant ratio.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new slant.</returns>
    let withSyntheticSlant (slant: float32) (font: Font) : Font =
        match font.Slant = slant with
        | true -> font
        | false -> changed { font with Slant = slant }

    /// <summary>Scales a design-space integer by the x multiplier, <c>em_scale_x</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The scaled value.</returns>
    let internal emScaleX (font: Font) (value: int) =
        int ((int64 value * font.XMult + 32768L) >>> 16)

    /// <summary>Scales a design-space integer by an x multiplier read once from the font, <c>em_scale_x</c>.</summary>
    /// <param name="multiplier">The font's x multiplier.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The scaled value.</returns>
    let internal emScaleBy (multiplier: int64) (value: int) =
        int ((int64 value * multiplier + 32768L) >>> 16)

    /// <summary>Scales a design-space integer by the y multiplier, <c>em_scale_y</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The scaled value.</returns>
    let internal emScaleY (font: Font) (value: int) =
        int ((int64 value * font.YMult + 32768L) >>> 16)

    /// <summary>Scales a design-space float by the x multiplier, <c>em_scalef_x</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The scaled value.</returns>
    let internal emScalefX (font: Font) (value: float32) =
        int (roundHalfUp (value * font.XMultF))

    /// <summary>Scales a design-space float by the y multiplier, <c>em_scalef_y</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The scaled value.</returns>
    let internal emScalefY (font: Font) (value: float32) =
        int (roundHalfUp (value * font.YMultF))

    /// <summary>Converts a design-space integer to user-space without rounding, <c>em_fscale_x</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The float value.</returns>
    let internal emFscaleX (font: Font) (value: int) =
        float32 value * font.XMultF

    /// <summary>Converts a design-space integer to user-space without rounding, <c>em_fscale_y</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The float value.</returns>
    let internal emFscaleY (font: Font) (value: int) =
        float32 value * font.YMultF

    /// <summary>Converts a design-space float to user-space without rounding, <c>em_fscalef_x</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The float value.</returns>
    let internal emFscalefX (font: Font) (value: float32) =
        value * font.XMultF

    /// <summary>Converts a design-space float to user-space without rounding, <c>em_fscalef_y</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="value">The design value.</param>
    /// <returns>The float value.</returns>
    let internal emFscalefY (font: Font) (value: float32) =
        value * font.YMultF

    /// <summary>Applies synthetic emboldening to an advance when it is not in place.</summary>
    /// <param name="strength">The strength.</param>
    /// <param name="scale">The scale whose sign selects the direction.</param>
    /// <param name="advance">The advance.</param>
    /// <returns>The adjusted advance.</returns>
    let private embolden (strength: int) (scale: int) (advance: int) : int =
        let signed = match scale >= 0 with | true -> strength | false -> -strength
        match advance <> 0 with
        | true -> advance + signed
        | false -> advance

    /// <summary>The horizontal-ascender metric tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_ASCENDER</c>.</summary>
    [<Literal>]
    let private HorizontalAscender = 0x68617363u<tag>

    /// <summary>The horizontal-descender metric tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_DESCENDER</c>.</summary>
    [<Literal>]
    let private HorizontalDescender = 0x68647363u<tag>

    /// <summary>The horizontal-line-gap metric tag, <c>HB_OT_METRICS_TAG_HORIZONTAL_LINE_GAP</c>.</summary>
    [<Literal>]
    let private HorizontalLineGap = 0x686C6770u<tag>

    /// <summary>The vertical-ascender metric tag, <c>HB_OT_METRICS_TAG_VERTICAL_ASCENDER</c>.</summary>
    [<Literal>]
    let private VerticalAscender = 0x76617363u<tag>

    /// <summary>The vertical-descender metric tag, <c>HB_OT_METRICS_TAG_VERTICAL_DESCENDER</c>.</summary>
    [<Literal>]
    let private VerticalDescender = 0x76647363u<tag>

    /// <summary>The vertical-line-gap metric tag, <c>HB_OT_METRICS_TAG_VERTICAL_LINE_GAP</c>.</summary>
    [<Literal>]
    let private VerticalLineGap = 0x766C6770u<tag>

    /// <summary>Forces an ascender or descender sign, <c>_fix_ascender_descender</c> in double.</summary>
    /// <param name="value">The metric value with its MVAR delta.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The value, ascenders positive and descenders negative.</returns>
    let private fixAscenderDescender (value: float32) (tag: Tag) : float32 =
        match tag with
        | t when t = HorizontalAscender || t = VerticalAscender -> float32 (Math.Abs (float value))
        | t when t = HorizontalDescender || t = VerticalDescender -> float32 (-(Math.Abs (float value)))
        | _ -> value

    /// <summary>The MVAR delta of a metric tag at the font's coordinates.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The unscaled delta.</returns>
    let private mvarVar (font: Font) (tag: Tag) : float32 =
        Mvar.getVar (font.Ot.Variations.Value.Mvar) font.Variations.Mvar tag

    /// <summary>Returns the horizontal font extents, <c>hb_ot_get_font_h_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The extents, or none when no source table has data.</returns>
    let hExtents (font: Font) : FontExtents voption =
        match font.Funcs with
        | ValueSome funcs -> funcs.HFontExtents () |> Settled.value
        | ValueNone ->
            let os2 = font.Ot.Os2.Value
            let hhea = font.Ot.Hhea.Value
            let source =
                match Os2.useTypoMetrics os2 && os2.Present, hhea.Present with
                | true, _ -> ValueSome (struct (os2.TypoAscender, os2.TypoDescender, os2.TypoLineGap))
                | false, true -> ValueSome (struct (hhea.Ascender, hhea.Descender, hhea.LineGap))
                | false, false -> ValueNone
            match source with
            | ValueNone -> ValueNone
            | ValueSome (struct (ascender, descender, lineGap)) ->
                let shift = match font.YScale < 0 with | true -> -font.YStrength | false -> font.YStrength
                ValueSome
                    { Ascender = emScalefY font (fixAscenderDescender (float32 ascender + mvarVar font HorizontalAscender) HorizontalAscender) + shift
                      Descender = emScalefY font (fixAscenderDescender (float32 descender + mvarVar font HorizontalDescender) HorizontalDescender)
                      LineGap = emScalefY font (float32 lineGap + mvarVar font HorizontalLineGap) }

    /// <summary>Returns the vertical font extents, <c>hb_ot_get_font_v_extents</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The extents, or none when vhea has no data.</returns>
    let vExtents (font: Font) : FontExtents voption =
        match font.Funcs with
        | ValueSome funcs -> funcs.VFontExtents () |> Settled.value
        | ValueNone ->
            let vhea = font.Ot.Vhea.Value
            match vhea.Present with
            | false -> ValueNone
            | true ->
                let shift = match font.XScale < 0 with | true -> -font.XStrength | false -> font.XStrength
                let ascender, descender =
                    match font.EmboldenInPlace with
                    | true -> shift / 2, -(shift - shift / 2)
                    | false -> shift, 0
                ValueSome
                    { Ascender = emScalefX font (fixAscenderDescender (float32 vhea.Ascender + mvarVar font VerticalAscender) VerticalAscender) + ascender
                      Descender = emScalefX font (fixAscenderDescender (float32 vhea.Descender + mvarVar font VerticalDescender) VerticalDescender) + descender
                      LineGap = emScalefX font (float32 vhea.LineGap + mvarVar font VerticalLineGap) }

    /// <summary>Returns the MVAR delta of a metric tag, <c>hb_ot_metrics_get_variation</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The unscaled delta.</returns>
    let metricsVariation (font: Font) (tag: Tag) : float32 =
        mvarVar font tag

    /// <summary>Returns the horizontally scaled MVAR delta of a metric tag, <c>hb_ot_metrics_get_x_variation</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The scaled delta.</returns>
    let metricsXVariation (font: Font) (tag: Tag) : int =
        emScalefX font (mvarVar font tag)

    /// <summary>Returns the vertically scaled MVAR delta of a metric tag, <c>hb_ot_metrics_get_y_variation</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="tag">The metric tag.</param>
    /// <returns>The scaled delta.</returns>
    let metricsYVariation (font: Font) (tag: Tag) : int =
        emScalefY font (mvarVar font tag)

    /// <summary>Returns horizontal extents, synthesizing them as <c>get_h_extents_with_fallback</c> does, reading the tables: the
    /// reference of <see cref="hExtentsWithFallback"/>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The extents.</returns>
    let internal hExtentsWithFallbackReference (font: Font) =
        match hExtents font with
        | ValueSome extents -> extents
        | ValueNone ->
            let ascender = int (float font.YScale * 0.8)
            { Ascender = ascender; Descender = ascender - font.YScale; LineGap = 0 }

    /// <summary>Returns horizontal extents, synthesizing them as <c>get_h_extents_with_fallback</c> does, read from the tables once
    /// for the font's state and held by the font.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The extents.</returns>
    let hExtentsWithFallback (font: Font) =
        match font.Memo.HExtents.TryGet () with
        | ValueSome held -> held.Extents
        | ValueNone -> (font.Memo.HExtents.Set { Extents = hExtentsWithFallbackReference font }).Extents

    /// <summary>Returns vertical extents, synthesizing them as <c>get_v_extents_with_fallback</c> does.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The extents.</returns>
    let vExtentsWithFallback (font: Font) : FontExtents =
        match vExtents font with
        | ValueSome extents -> extents
        | ValueNone ->
            let ascender = font.XScale / 2
            { Ascender = ascender; Descender = ascender - font.XScale; LineGap = 0 }

    /// <summary>Asks the nominal-glyph function of a font's functions, out of line so that the lookup of a font without functions stays as small as
    /// it was.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private nominalViaFuncs (funcs: FontFuncs) (codepoint: uint32) =
        funcs.Nominal codepoint |> Settled.value

    /// <summary>Looks up the nominal glyph of a codepoint, <c>hb_ot_get_nominal_glyph</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <returns>The glyph, or none.</returns>
    let nominalGlyph (font: Font) (codepoint: uint32) : uint32 voption =
        match font.Funcs with
        | ValueSome funcs -> nominalViaFuncs funcs codepoint
        | ValueNone -> Cmap.nominalGlyph font.Ot.Cmap.Value codepoint

    /// <summary>Looks up the nominal glyphs of every glyph's code point, none when any is missing, <c>hb_font_get_nominal_glyphs</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs, whose code points are mapped.</param>
    /// <returns>The glyphs in glyph order, or none when a code point has no glyph.</returns>
    let internal nominalGlyphsAll (font: Font) (infos: ReadOnlySpan<GlyphInfo>) =
        match font.Funcs with
        | ValueSome funcs -> Sealed.tryMapAll (fun (info: GlyphInfo) -> funcs.Nominal info.Codepoint) infos
        | ValueNone ->
            let cmap = font.Ot.Cmap.Value
            Sealed.tryMapAll (fun (info: GlyphInfo) -> Cmap.nominalGlyph cmap info.Codepoint) infos

    /// <summary>Looks up a variation glyph, <c>hb_ot_get_variation_glyph</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="codepoint">The codepoint.</param>
    /// <param name="selector">The variation selector.</param>
    /// <returns>The glyph, or none.</returns>
    let variationGlyph (font: Font) (codepoint: uint32) (selector: uint32) : uint32 voption =
        match font.Funcs with
        | ValueSome funcs -> funcs.Variation codepoint selector |> Settled.value
        | ValueNone ->
            Cmap.variationGlyph font.Ot.Cmap.Value codepoint selector

    /// <summary>Looks up a glyph's name bytes, trying post then CFF1, <c>hb_ot_get_glyph_name</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The name bytes, or none.</returns>
    let private glyphNameBytes (font: Font) (glyph: uint32) : ReadOnlyMemory<byte> voption =
        match Post.glyphName font.Ot.Post.Value glyph with
        | ValueSome name -> ValueSome name
        | ValueNone -> Cff1.glyphName font.Ot.Cff1.Value glyph

    /// <summary>Returns a glyph name as Latin-1 text, cut at the first NUL.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The name, or none.</returns>
    let private glyphNameOt (font: Font) (glyph: uint32) =
        match glyphNameBytes font glyph with
        | ValueNone -> ValueNone
        | ValueSome name ->
            match name.Length with
            | 0 -> ValueNone
            | _ ->
                let span = name.Span
                match span.IndexOf 0uy with
                | -1 -> ValueSome (Encoding.Latin1.GetString span)
                | nul -> ValueSome (Encoding.Latin1.GetString (span.Slice(0, nul)))

    /// <summary>Returns a glyph's name from the font's functions or, without them, from its face, <c>hb_font_get_glyph_name</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The name, or none.</returns>
    let glyphName (font: Font) (glyph: uint32) =
        match font.Funcs with
        | ValueSome funcs -> funcs.GlyphName glyph |> Settled.value
        | ValueNone -> glyphNameOt font glyph

    /// <summary>Returns a glyph's string with the 128-byte serializer buffer, <c>hb_font_glyph_to_string</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The name, truncated to 127 bytes, or <c>gidN</c>.</returns>
    let glyphToString (font: Font) (glyph: uint32) : string =
        match glyphName font glyph with
        | ValueSome name -> match name.Length > 127 with | true -> name.Substring(0, 127) | false -> name
        | ValueNone -> $"gid{glyph}"

    /// <summary>Looks up a glyph by name, trying post then CFF1, <c>hb_ot_get_glyph_from_name</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="name">The name text.</param>
    /// <returns>The glyph, or none.</returns>
    let glyphFromName (font: Font) (name: string) : uint32 voption =
        match font.Funcs with
        | ValueSome funcs -> funcs.GlyphFromName name |> Settled.value
        | ValueNone ->
            let bytes = ReadOnlySpan(Encoding.Latin1.GetBytes name)
            match Post.glyphFromName font.Ot.Post.Value bytes with
            | ValueSome glyph -> ValueSome glyph
            | ValueNone -> Cff1.glyphFromName font.Ot.Cff1.Value bytes

    /// <summary>Parses a glyph string, <c>hb_font_glyph_from_string</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="s">The text.</param>
    /// <returns>The glyph, or none.</returns>
    let glyphFromString (font: Font) (s: string) : uint32 voption =
        Serialize.resolveGlyph (glyphFromName font) (nominalGlyph font) (Encoding.UTF8.GetBytes s)

    /// <summary>The unscaled horizontal advance with HVAR then gvar, falling back to plain hmtx.</summary>
    /// <param name="font">The font.</param>
    /// <param name="mtx">The horizontal metrics.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The unscaled advance.</returns>
    let private hAdvanceUnscaled (font: Font) (mtx: Mtx) (glyph: uint32) : int =
        let variations = font.Variations
        match variations.Coords.HasNonZero with
        | false -> Hmtx.advance mtx glyph
        | true ->
            let hvar = font.Ot.Variations.Value.Hvar
            match hvar.HasData with
            | true -> Hvar.advanceWithVariations (uint32 (Hmtx.advance mtx glyph)) (Hvar.advanceDelta hvar variations.Hvar glyph)
            | false ->
                let gvar = font.Ot.Gvar.Value
                match gvar.GlyphCount <> 0 with
                | false -> Hmtx.advance mtx glyph
                | true -> GlyphOutline.advanceWithVariations font.Ot.GlyfPoints.Value gvar variations.Coords font.Face.Upem glyph false

    /// <summary>The unscaled horizontal advance of every glyph of a font that has non-zero coordinates and an HVAR table, the delta of each
    /// read once: built the first time a font of the coordinates asks, and a face keeps the newest few, so a font of other coordinates
    /// has its own. A scale change leaves it valid, the advances being unscaled.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The advances by glyph, empty for a font whose advances are not read from HVAR.</returns>
    let internal hvarAdvances (font: Font) =
        let mtx = font.Ot.Hmtx.Value
        match Hmtx.hasData mtx && font.Variations.Coords.HasNonZero && font.Ot.Variations.Value.Hvar.HasData with
        | false -> ImmutableArray<int>.Empty
        | true ->
            let key = { Coords = font.Variations.Coords.Sealed; XScale = 0 }
            match font.Ot.HvarAdvances.TryFind key with
            | ValueSome advances -> advances
            | ValueNone -> font.Ot.HvarAdvances.Add (key, Sealed.init font.Face.GlyphCount (fun glyph -> hAdvanceUnscaled font mtx (uint32 glyph)))

    /// <summary>Returns a glyph's horizontal advance reading every table: the reference of <see cref="hAdvance"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    let internal hAdvanceReference (font: Font) (glyph: uint32) =
        match font.Funcs with
        | ValueSome funcs -> funcs.HAdvance glyph |> Settled.value
        | ValueNone ->
            let mtx = font.Ot.Hmtx.Value
            let advance =
                match Hmtx.hasData mtx with
                | false -> emScaleX font (font.Face.Upem / 2)
                | true -> emScaleX font (hAdvanceUnscaled font mtx glyph)
            match font.XStrength <> 0 && not font.EmboldenInPlace with
            | true -> embolden font.XStrength font.XScale advance
            | false -> advance

    /// <summary>The signed synthetic strength an advance is increased by, 0 when the font does not embolden advances.</summary>
    /// <param name="strength">The font's strength on the axis.</param>
    /// <param name="scale">The scale on the axis.</param>
    /// <param name="font">The font.</param>
    /// <returns>The strength to add to a non-zero advance.</returns>
    let private advanceStrength (strength: int) (scale: int) (font: Font) =
        match strength <> 0 && not font.EmboldenInPlace with
        | false -> 0
        | true -> match scale >= 0 with | true -> strength | false -> -strength

    /// <summary>The pages of a plan that has none to fill.</summary>
    let private unpaged = IntPages(0)

    /// <summary>Scales an unscaled advance by a plan's multiplier and adds its synthetic strength to a non-zero result.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="unscaled">The unscaled advance.</param>
    /// <returns>The scaled advance.</returns>
    let inline private scaleAdvance (plan: AdvancePlan) (unscaled: int) =
        let advance = int ((int64 unscaled * plan.Multiplier + 32768L) >>> 16)
        match advance <> 0 with
        | true -> advance + plan.Strength
        | false -> advance

    /// <summary>Reads the unscaled advance of a long metric from the table bytes without a bounds test: the quick reading, which a plan
    /// has only when its table holds every long metric it answers (see <c>tablePlan</c>).</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The offset of the long metric in the array.</param>
    /// <returns>The unscaled advance.</returns>
    let inline private longAdvance (table: byte array) (at: int) =
        let stored = Unsafe.ReadUnaligned<uint16>(&Unsafe.Add(&MemoryMarshal.GetArrayDataReference table, at))
        int (match BitConverter.IsLittleEndian with | true -> BinaryPrimitives.ReverseEndianness stored | false -> stored)

    /// <summary>Builds the advance plan of a metrics table: one that reads its table when the table bytes can be read as an array and
    /// no variation applies to the advances, and a direct one when it adds no strength and its table is as long as the metrics it names.</summary>
    /// <param name="mtx">The metrics.</param>
    /// <param name="hasData">Whether the metrics have data; a font without gives every glyph <c>beyond</c>.</param>
    /// <param name="varied">Whether the font's coordinates change the advances read from the table.</param>
    /// <param name="pageCount">The glyph count the pages of a varied plan cover.</param>
    /// <param name="beyond">The unscaled value of a glyph past the table.</param>
    /// <param name="multiplier">The multiplier, negated for a vertical advance.</param>
    /// <param name="strength">The signed synthetic strength.</param>
    /// <returns>The plan.</returns>
    let private tablePlan (mtx: Mtx) (hasData: bool) (varied: bool) (pageCount: int) (beyond: int) (multiplier: int64) (strength: int) =
        let table = AdvanceTable.tryArray mtx.Table
        let reads = not hasData || (ValueOption.isSome table && not varied)
        let struct (array, offset) =
            match table with
            | ValueSome found -> found
            | ValueNone -> struct (Array.empty, 0)
        let bearings = match hasData && reads with | true -> mtx.Bearings | false -> 0
        let lastLong = max 0 (mtx.LongMetrics - 1)
        let own = min (lastLong + 1) bearings
        let holds (count: int) = count = 0 || offset + (count - 1) * 4 + 2 <= array.Length
        let direct = reads && strength = 0 && holds own
        let plan =
            { Direct = direct
              Reads = reads
              Table = array
              Offset = offset
              Bearings = bearings
              LastLong = lastLong
              Longs = match direct with | true -> own | false -> 0
              Cover = match reads with | true -> uint64 bearings | false -> UInt64.MaxValue
              Tail = 0
              Beyond = beyond
              Absent = 0
              Multiplier = multiplier
              Strength = strength
              Paged = hasData && varied
              Pages = match hasData && varied with | true -> IntPages(pageCount) | false -> unpaged }
        let unscaledTail = match direct && bearings > own && holds (lastLong + 1) with | true -> longAdvance array (offset + lastLong * 4) | false -> 0
        { plan with Absent = scaleAdvance plan beyond; Tail = scaleAdvance plan unscaledTail }

    /// <summary>The plan that answers every glyph through the code that builds, pages or asks: no glyph is below its longs or from its cover.</summary>
    /// <param name="plan">The plan.</param>
    /// <returns>The plan with no reading of its own.</returns>
    let private indirect (plan: AdvancePlan) =
        { plan with Direct = false; Reads = false; Paged = false; Bearings = 0; Longs = 0; Cover = UInt64.MaxValue }

    /// <summary>The unscaled value of a glyph past the table, <c>get_advance_without_var_unscaled</c>.</summary>
    /// <param name="mtx">The metrics.</param>
    /// <returns>The default advance when the table holds no advances, else 0.</returns>
    let private beyondTable (mtx: Mtx) =
        match mtx.Advances with
        | 0 -> mtx.DefaultAdvance
        | _ -> 0

    /// <summary>Builds the horizontal advance plan of a font.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The plan.</returns>
    let private hAdvancePlan (font: Font) =
        let mtx = font.Ot.Hmtx.Value
        let strength = advanceStrength font.XStrength font.XScale font
        let plan =
            match Hmtx.hasData mtx with
            | false -> tablePlan mtx false false 0 (font.Face.Upem / 2) font.XMult strength
            | true -> tablePlan mtx true font.Variations.Coords.HasNonZero font.Face.GlyphCount (beyondTable mtx) font.XMult strength
        match font.Funcs with
        | ValueSome _ -> indirect plan
        | ValueNone -> plan

    /// <summary>Reads one advance from a plan that reads its table, with every bounds test: for a glyph below the bearings one load of the
    /// long metric and one multiply, and for any other glyph the finished advance of the glyphs beyond the table.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The scaled advance.</returns>
    let private planAdvance (plan: AdvancePlan) (glyph: uint32) =
        match uint64 glyph < uint64 plan.Bearings with
        | true -> scaleAdvance plan (int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan<byte>(plan.Table, plan.Offset + int (min glyph (uint32 plan.LastLong)) * 4, 2))))
        | false -> plan.Absent

    /// <summary>Answers an advance from the pages of a plan that has them when the glyph has been worked out, and from the code that works it
    /// out, builds the plan or asks the functions when it has not.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="slow">The answer of that code.</param>
    /// <returns>The scaled advance.</returns>
    let inline private pagedOr (plan: AdvancePlan) (glyph: uint32) ([<InlineIfLambda>] slow: unit -> int) =
        match plan.Pages.Read glyph with
        | Int32.MinValue -> slow ()
        | unscaled -> scaleAdvance plan unscaled

    /// <summary>Answers an advance from a direct plan by comparisons and one table load, and sends every other case to the code that
    /// answers it: a glyph below <c>Longs</c> reads its own long metric, one from <c>Cover</c> on has <c>Absent</c>, one between has
    /// <c>Tail</c> when the plan is direct, and in the pages of a plan that has them a glyph already worked out has its unscaled advance.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="slow">The answer of the code that builds the plan, pages, asks the functions or reads with strength.</param>
    /// <returns>The scaled advance.</returns>
    let inline private planned (plan: AdvancePlan) (glyph: uint32) ([<InlineIfLambda>] slow: unit -> int) =
        match uint64 glyph < uint64 plan.Longs with
        | true -> int ((int64 (longAdvance plan.Table (plan.Offset + int glyph * 4)) * plan.Multiplier + 32768L) >>> 16)
        | false ->
            match uint64 glyph >= plan.Cover with
            | true -> plan.Absent
            | false ->
                match plan.Direct with
                | true -> plan.Tail
                | false -> pagedOr plan glyph slow

    /// <summary>Answers the horizontal advance of a font whose plan is not direct: builds the plan first when the font has none.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private hAdvanceIndirect (font: Font) (glyph: uint32) =
        let plan =
            match font.HPlan.TryGet () with
            | ValueSome held -> held
            | ValueNone -> font.HPlan.Set (hAdvancePlan font)
        match plan.Reads, plan.Paged with
        | true, _ -> planAdvance plan glyph
        | false, true ->
            match plan.Pages.Read glyph with
            | Int32.MinValue ->
                let unscaled = hAdvanceUnscaled font font.Ot.Hmtx.Value glyph
                plan.Pages.Write (glyph, unscaled)
                scaleAdvance plan unscaled
            | unscaled -> scaleAdvance plan unscaled
        | false, false -> hAdvanceReference font glyph

    /// <summary>Returns a glyph's horizontal advance, <c>hb_ot_get_glyph_h_advances</c>: a font whose advances are not varied reads
    /// it from its plan, built at the first request and held by the font.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let hAdvance (font: Font) (glyph: uint32) =
        planned font.HPlan.Value glyph (fun () -> hAdvanceIndirect font glyph |> Settled.value)

    /// <summary>Returns a glyph's horizontal advance with the font-invariant state supplied by the caller, the
    /// per-glyph body of <see cref="hAdvancePositions"/>, so a loop reads the hmtx accelerator and the scaled half
    /// em once.</summary>
    /// <param name="font">The font.</param>
    /// <param name="mtx">The horizontal metrics.</param>
    /// <param name="hasData">Whether the metrics have data.</param>
    /// <param name="upemHalf">The scaled half em, the advance of a font without metrics.</param>
    /// <param name="emboldenHere">Whether the advance is emboldened.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    let internal hAdvanceFrom (font: Font) (mtx: Mtx) hasData upemHalf emboldenHere (glyph: uint32) =
        let advance =
            match hasData with
            | false -> upemHalf
            | true -> emScaleX font (hAdvanceUnscaled font mtx glyph)
        match emboldenHere with
        | true -> embolden font.XStrength font.XScale advance
        | false -> advance

    /// <summary>The unscaled vertical advance with VVAR then gvar, falling back to plain vmtx, negated.</summary>
    /// <param name="font">The font.</param>
    /// <param name="mtx">The vertical metrics.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The negated unscaled advance.</returns>
    let private vAdvanceUnscaled (font: Font) (mtx: Mtx) (glyph: uint32) : int =
        let variations = font.Variations
        match variations.Coords.HasNonZero with
        | false -> - Hmtx.advance mtx glyph
        | true ->
            let vvar = font.Ot.Variations.Value.Vvar
            match vvar.HasData with
            | true -> - (Hvar.advanceWithVariations (uint32 (Hmtx.advance mtx glyph)) (Hvar.advanceDelta vvar variations.Vvar glyph))
            | false ->
                let gvar = font.Ot.Gvar.Value
                match gvar.GlyphCount <> 0 with
                | false -> - Hmtx.advance mtx glyph
                | true -> - (GlyphOutline.advanceWithVariations font.Ot.GlyfPoints.Value gvar variations.Coords font.Face.Upem glyph true)

    /// <summary>Returns one vertical advance with a precomputed fallback.</summary>
    /// <param name="font">The font.</param>
    /// <param name="mtx">The vertical metrics.</param>
    /// <param name="fallback">The advance used when vmtx has no data.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    let private vAdvanceFrom (font: Font) (mtx: Mtx) (fallback: int) (glyph: uint32) : int =
        let advance =
            match Hmtx.hasData mtx with
            | false -> fallback
            | true -> emScaleY font (vAdvanceUnscaled font mtx glyph)
        match font.YStrength <> 0 && not font.EmboldenInPlace with
        | true -> embolden font.YStrength font.YScale advance
        | false -> advance

    /// <summary>Returns a glyph's vertical advance reading every table: the reference of <see cref="vAdvance"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    let internal vAdvanceReference (font: Font) (glyph: uint32) =
        match font.Funcs with
        | ValueSome funcs -> funcs.VAdvance glyph |> Settled.value
        | ValueNone ->
            let mtx = font.Ot.Vmtx.Value
            match Hmtx.hasData mtx with
            | true -> vAdvanceFrom font mtx 0 glyph
            | false ->
                let extents = hExtentsWithFallbackReference font
                vAdvanceFrom font mtx (extents.Descender - extents.Ascender) glyph

    /// <summary>The unit multiplier of a plan whose constant is already scaled.</summary>
    [<Literal>]
    let private UnitMultiplier = 65536L

    /// <summary>Builds the vertical advance plan of a font: a font without vmtx has one advance, descender less ascender, the same
    /// for every glyph, worked out once.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The plan.</returns>
    let private vAdvancePlan (font: Font) =
        let mtx = font.Ot.Vmtx.Value
        let strength = advanceStrength font.YStrength font.YScale font
        let plan =
            match Hmtx.hasData mtx with
            | true ->
                let varied = font.Variations.Coords.HasNonZero
                tablePlan mtx true varied font.Face.GlyphCount (beyondTable mtx) (match varied with | true -> font.YMult | false -> -font.YMult) strength
            | false ->
                let extents = hExtentsWithFallback font
                tablePlan mtx false false 0 (extents.Descender - extents.Ascender) UnitMultiplier strength
        match font.Funcs with
        | ValueSome _ -> indirect plan
        | ValueNone -> plan

    /// <summary>Answers the vertical advance of a font whose plan is not direct: builds the plan first when the font has none.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private vAdvanceIndirect (font: Font) (glyph: uint32) =
        let plan =
            match font.VPlan.TryGet () with
            | ValueSome held -> held
            | ValueNone -> font.VPlan.Set (vAdvancePlan font)
        match plan.Reads, plan.Paged with
        | true, _ -> planAdvance plan glyph
        | false, true ->
            match plan.Pages.Read glyph with
            | Int32.MinValue ->
                let unscaled = vAdvanceUnscaled font font.Ot.Vmtx.Value glyph
                plan.Pages.Write (glyph, unscaled)
                scaleAdvance plan unscaled
            | unscaled -> scaleAdvance plan unscaled
        | false, false -> vAdvanceReference font glyph

    /// <summary>Returns a glyph's vertical advance, <c>hb_ot_get_glyph_v_advances</c>: a font whose advances are not varied reads
    /// it from its plan, built at the first request and held by the font, a font without vmtx answering a constant.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let vAdvance (font: Font) (glyph: uint32) =
        planned font.VPlan.Value glyph (fun () -> vAdvanceIndirect font glyph |> Settled.value)

    /// <summary>Returns vertical advances for a span glyph by glyph, computing the fallback once: the reference of
    /// <see cref="vAdvances"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The advances in glyph order.</returns>
    let internal vAdvancesReference (font: Font) (glyphs: ReadOnlySpan<uint32>) =
        let mtx = font.Ot.Vmtx.Value
        match Hmtx.hasData mtx with
        | true -> Sealed.map (vAdvanceFrom font mtx 0) glyphs
        | false ->
            let extents = hExtentsWithFallback font
            let fallback = extents.Descender - extents.Ascender
            Sealed.map (vAdvanceFrom font mtx fallback) glyphs

    /// <summary>Returns vertical advances for a span, computing the fallback once; a font without vertical metrics gives every
    /// glyph the same advance, which is written without looking at the glyphs.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The advances in glyph order.</returns>
    let vAdvances (font: Font) (glyphs: ReadOnlySpan<uint32>) =
        match font.Funcs with
        | ValueSome funcs -> Sealed.map (fun (glyph: uint32) -> funcs.VAdvance glyph) glyphs
        | ValueNone ->
            let mtx = font.Ot.Vmtx.Value
            match Hmtx.hasData mtx with
            | true -> Sealed.map (vAdvanceFrom font mtx 0) glyphs
            | false ->
                let extents = hExtentsWithFallback font
                let fallback = extents.Descender - extents.Ascender
                AdvanceTable.replicate glyphs.Length (vAdvanceFrom font mtx fallback 0u)

    /// <summary>Returns horizontal advances for a span of glyphs one glyph at a time: the reference of <see cref="hAdvances"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The advances in glyph order.</returns>
    let internal hAdvancesReference (font: Font) (glyphs: ReadOnlySpan<uint32>) =
        Sealed.map (hAdvance font) glyphs

    /// <summary>Returns horizontal advances for a span of glyphs; a font with metrics, no variations and no synthetic
    /// emboldening in place has them read from the table in one pass.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The advances in glyph order.</returns>
    let hAdvances (font: Font) (glyphs: ReadOnlySpan<uint32>) =
        match font.Funcs with
        | ValueSome funcs -> Sealed.map (fun (glyph: uint32) -> funcs.HAdvance glyph) glyphs
        | ValueNone ->
            let mtx = font.Ot.Hmtx.Value
            match Hmtx.hasData mtx && not font.Variations.Coords.HasNonZero && (font.XStrength = 0 || font.EmboldenInPlace) with
            | true -> AdvanceTable.scaled mtx.Table.Span mtx.Bearings mtx.LongMetrics mtx.Advances mtx.DefaultAdvance font.XMult glyphs
            | false -> Sealed.map (hAdvance font) glyphs

    /// <summary>Reads a font's horizontal advance of a glyph through its plan, for the batch loops.</summary>
    [<Struct>]
    type private HAdvanceReader =
        val Font: Font
        new (font) = { Font = font }
        interface IAdvanceReader with
            member this.Read glyph = hAdvance this.Font glyph

    /// <summary>Reads a font's vertical advance of a glyph with the metrics and fallback worked out once for the batch.</summary>
    [<Struct>]
    type private VAdvanceFromReader =
        val Font: Font
        val Mtx: Mtx
        val Fallback: int
        new (font, mtx, fallback) = { Font = font; Mtx = mtx; Fallback = fallback }
        interface IAdvanceReader with
            member this.Read glyph = vAdvanceFrom this.Font this.Mtx this.Fallback glyph

    /// <summary>Reads the horizontal advance of a glyph from the functions of a font that has some, for the batch loops.</summary>
    [<Struct>]
    type private HFuncsReader =
        val Funcs: FontFuncs
        new (funcs) = { Funcs = funcs }
        interface IAdvanceReader with
            member this.Read glyph = this.Funcs.HAdvance glyph |> Settled.value

    /// <summary>Reads the vertical advance of a glyph from the functions of a font that has some, for the batch loops.</summary>
    [<Struct>]
    type private VFuncsReader =
        val Funcs: FontFuncs
        new (funcs) = { Funcs = funcs }
        interface IAdvanceReader with
            member this.Read glyph = this.Funcs.VAdvance glyph |> Settled.value

    /// <summary>Writes the horizontal advances of a span of glyphs into an array the caller owns, <c>hb_ot_get_glyph_h_advances</c> with no
    /// allocation: the advances <see cref="hAdvances"/> returns. An array shorter than the glyphs is left as it is, and the length it
    /// would need is returned, so that the caller grows it and asks again.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <param name="target">The array the advances are written into, from its first element.</param>
    /// <returns>The number of glyphs, which is the number of advances written when the array holds them.</returns>
    let hAdvancesInto (font: Font) (glyphs: ReadOnlySpan<uint32>) (target: Span<int>) =
        match target.Length < glyphs.Length with
        | true -> glyphs.Length
        | false ->
            match font.Funcs with
            | ValueSome funcs -> AdvanceInto.readInto (HFuncsReader funcs) glyphs target
            | ValueNone ->
                let mtx = font.Ot.Hmtx.Value
                match Hmtx.hasData mtx && not font.Variations.Coords.HasNonZero && (font.XStrength = 0 || font.EmboldenInPlace) with
                | true -> AdvanceInto.scaledInto mtx.Table.Span mtx.Bearings mtx.LongMetrics mtx.Advances mtx.DefaultAdvance font.XMult glyphs target
                | false -> AdvanceInto.readInto (HAdvanceReader font) glyphs target
            glyphs.Length

    /// <summary>Writes the vertical advances of a span of glyphs into an array the caller owns, <c>hb_ot_get_glyph_v_advances</c> with no
    /// allocation: the advances <see cref="vAdvances"/> returns. An array shorter than the glyphs is left as it is, and the length it
    /// would need is returned, so that the caller grows it and asks again.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <param name="target">The array the advances are written into, from its first element.</param>
    /// <returns>The number of glyphs, which is the number of advances written when the array holds them.</returns>
    let vAdvancesInto (font: Font) (glyphs: ReadOnlySpan<uint32>) (target: Span<int>) =
        match target.Length < glyphs.Length with
        | true -> glyphs.Length
        | false ->
            match font.Funcs with
            | ValueSome funcs -> AdvanceInto.readInto (VFuncsReader funcs) glyphs target
            | ValueNone ->
                let mtx = font.Ot.Vmtx.Value
                match Hmtx.hasData mtx with
                | true -> AdvanceInto.readInto (VAdvanceFromReader (font, mtx, 0)) glyphs target
                | false ->
                    let extents = hExtentsWithFallback font
                    AdvanceInto.fillInto (vAdvanceFrom font mtx (extents.Descender - extents.Ascender) 0u) glyphs.Length target
            glyphs.Length

    /// <summary>Returns the default horizontal positions of a run's code points, one hmtx read per glyph and the
    /// invariant font state read once, <c>hb_ot_get_glyph_h_advances</c> (src/hb-ot-font.cc:382-460).</summary>
    /// <param name="font">The font.</param>
    /// <param name="infos">The glyphs, whose code points are mapped to advances.</param>
    /// <returns>The positions with the default horizontal advances.</returns>
    let internal hAdvancePositions (font: Font) (infos: ReadOnlySpan<GlyphInfo>) =
        match font.Funcs with
        | ValueSome funcs -> Sealed.mapi (fun _ (info: GlyphInfo) -> { XAdvance = funcs.HAdvance info.Codepoint; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }) infos
        | ValueNone ->
            let mtx = font.Ot.Hmtx.Value
            let hasData = Hmtx.hasData mtx
            let upemHalf = emScaleX font (font.Face.Upem / 2)
            let emboldenHere = font.XStrength <> 0 && not font.EmboldenInPlace
            Sealed.mapi (fun _ (info: GlyphInfo) ->
                let advance =
                    match hasData with
                    | false -> upemHalf
                    | true -> emScaleX font (hAdvanceUnscaled font mtx info.Codepoint)
                let advance = match emboldenHere with | true -> embolden font.XStrength font.XScale advance | false -> advance
                { XAdvance = advance; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }) infos

    /// <summary>Sets variation values, <c>hb_font_set_variations</c>.</summary>
    /// <param name="variations">The variations, in order.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new variation state.</returns>
    let setVariations (variations: ImmutableArray<Variation>) (font: Font) : Font =
        renewed { font with Variations = FontVariations.ofVariations (font.Ot.Variations.Value) font.Variations.NamedInstance variations }

    /// <summary>Sets one variation axis, <c>hb_font_set_variation</c>.</summary>
    /// <param name="tag">The axis tag.</param>
    /// <param name="value">The value.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new variation state.</returns>
    let setVariation (tag: Tag) (value: float32) (font: Font) : Font =
        renewed { font with Variations = FontVariations.ofVariation (font.Ot.Variations.Value) font.Variations.NamedInstance font.Variations.DesignCoords tag value }

    /// <summary>Sets design-space coordinates, <c>hb_font_set_var_coords_design</c>.</summary>
    /// <param name="coords">The design coordinates.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new variation state.</returns>
    let setVarCoordsDesign (coords: ImmutableArray<float32>) (font: Font) : Font =
        renewed { font with Variations = FontVariations.ofDesign (font.Ot.Variations.Value) font.Variations.NamedInstance coords }

    /// <summary>Sets normalized coordinates, <c>hb_font_set_var_coords_normalized</c>.</summary>
    /// <param name="coords">The normalized 2.14 coordinates, stored unclamped.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font with the new variation state.</returns>
    let setVarCoordsNormalized (coords: ImmutableArray<int>) (font: Font) : Font =
        renewed { font with Variations = FontVariations.ofNormalized (font.Ot.Variations.Value) font.Variations.NamedInstance coords }

    /// <summary>Sets the named instance, <c>hb_font_set_var_named_instance</c>.</summary>
    /// <param name="instance">The named-instance index.</param>
    /// <param name="font">The font.</param>
    /// <returns>The font, or the input when the instance is unchanged.</returns>
    let setVarNamedInstance (instance: uint32) (font: Font) : Font =
        match font.Variations.NamedInstance = instance with
        | true -> font
        | false ->
            renewed { font with Variations = FontVariations.ofVariations (font.Ot.Variations.Value) instance ImmutableArray<Variation>.Empty }

    /// <summary>Returns the normalized coordinates, <c>hb_font_get_var_coords_normalized</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The coordinates.</returns>
    let varCoordsNormalized (font: Font) : ImmutableArray<int> =
        let coords = font.Variations.Coords
        let result = Sealed.ofSpan (coords.AsSpan())
        result

    /// <summary>Returns the design coordinates, <c>hb_font_get_var_coords_design</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The coordinates, NaN after a normalized set.</returns>
    let varCoordsDesign (font: Font) : ImmutableArray<float32> =
        font.Variations.DesignCoords

    /// <summary>Returns the named instance, <c>hb_font_get_var_named_instance</c>.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The named-instance index, or the no-instance sentinel.</returns>
    let varNamedInstance (font: Font) : uint32 =
        font.Variations.NamedInstance


