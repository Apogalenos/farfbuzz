namespace Farfbuzz

open System
open System.Runtime.CompilerServices
open Farfbuzz.OpenType

/// <summary>Vertical glyph origins: VORG, the glyf phantom point and the extents fallback, <c>hb-ot-font.cc:632-806</c>.</summary>
[<RequireQualifiedAccess>]
module internal Origins =
    /// <summary>Clamps an int64 into the int32 position range, <c>hb_clamp_to&lt;hb_position_t&gt;</c>.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The clamped value.</returns>
    let private clampToInt32 (value: int64) : int =
        int (Math.Clamp(value, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Applies the synthetic embolden shift to a vertical origin, <c>hb_font_t::get_glyph_v_origins</c>'s tail.</summary>
    /// <param name="font">The font.</param>
    /// <param name="x">The horizontal origin.</param>
    /// <param name="y">The vertical origin.</param>
    /// <returns>The shifted origin.</returns>
    let private syntheticShift (font: Font) (x: int) (y: int) : struct (int * int) =
        match font.IsSynthetic && not font.EmboldenInPlace with
        | false -> struct (x, y)
        | true ->
            let xShift = match font.XScale < 0 with | true -> -font.XStrength | false -> font.XStrength
            let yShift = match font.YScale < 0 with | true -> -font.YStrength | false -> font.YStrength
            struct (x + xShift, y + yShift)

    /// <summary>The vertical-origin source of a font, or a not-yet-ported source, looked up in the tables.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The source.</returns>
    let private sourceKind (font: Font) =
        match font.Ot.Vorg.Value.Present with
        | true -> VorgOrigin
        | false ->
            let vmtx = font.Ot.Vmtx.Value
            let glyf = font.Ot.Glyf.Value
            match Hmtx.hasData vmtx && glyf.Present with
            | true -> GlyfPhantomOrigin
            | false ->
                match Extents.source font with
                | ValueSome ColrOutlines -> ColrNotPorted
                | ValueSome VarcOutlines -> VarcNotPorted
                | ValueSome SyntheticOutlines -> SyntheticNotPorted
                | ValueNone -> ExtentsOrigin

    /// <summary>The font extents of a plan that does not read them.</summary>
    let private noExtents = { Ascender = 0; Descender = 0; LineGap = 0 }

    /// <summary>Builds the vertical-origin plan of a font: the source, and for the extents fallback the font extents it reads.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The plan.</returns>
    let private buildPlan (font: Font) =
        match sourceKind font with
        | ExtentsOrigin -> { Kind = ExtentsOrigin; Extents = Font.hExtentsWithFallback font; Pages = IntPages(font.Face.GlyphCount) }
        | VorgOrigin -> { Kind = VorgOrigin; Extents = noExtents; Pages = IntPages(font.Face.GlyphCount) }
        | GlyfPhantomOrigin -> { Kind = GlyfPhantomOrigin; Extents = noExtents; Pages = IntPages(font.Face.GlyphCount) }
        | ColrNotPorted -> { Kind = ColrNotPorted; Extents = noExtents; Pages = IntPages(0) }
        | VarcNotPorted -> { Kind = VarcNotPorted; Extents = noExtents; Pages = IntPages(0) }
        | SyntheticNotPorted -> { Kind = SyntheticNotPorted; Extents = noExtents; Pages = IntPages(0) }

    /// <summary>The vertical-origin plan of a font, built at the first request and held by the font.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The plan.</returns>
    let private planOf (font: Font) =
        match font.Memo.VOrigin.TryGet () with
        | ValueSome plan -> plan
        | ValueNone -> font.Memo.VOrigin.Set (buildPlan font)

    /// <summary>The source of a font's vertical origins, or the not-yet-ported source that would answer.</summary>
    /// <param name="font">The font.</param>
    /// <returns>The source and the font extents the fallback reads, or why there is none.</returns>
    let private source (font: Font) =
        let plan = planOf font
        match plan.Kind with
        | ColrNotPorted -> Error (OutlinesNotPorted ColrOutlines)
        | VarcNotPorted -> Error (OutlinesNotPorted VarcOutlines)
        | SyntheticNotPorted -> Error (OutlinesNotPorted SyntheticOutlines)
        | VorgOrigin
        | GlyfPhantomOrigin
        | ExtentsOrigin -> Ok plan

    /// <summary>Computes one glyph's vertical origin before the synthetic shift, from the source the plan names.</summary>
    /// <param name="font">The font.</param>
    /// <param name="plan">The source and font extents.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The vertical origin.</returns>
    let private originY (font: Font) (plan: VOriginPlan) (glyph: uint32) =
        match plan.Kind with
        | ColrNotPorted
        | VarcNotPorted
        | SyntheticNotPorted -> 0
        | VorgOrigin ->
            let vorgY = float32 (Vorg.yOrigin font.Ot.Vorg.Value glyph)
            let delta =
                match font.Variations.Coords.HasNonZero with
                | false -> 0.0f
                | true -> Hvar.vorgDelta (font.Ot.Variations.Value.Vvar) font.Variations.Vvar glyph
            Font.emScalefY font (vorgY + delta)
        | GlyfPhantomOrigin ->
            let top = GlyphOutline.vOriginWithVariations font.Ot.GlyfPoints.Value font.Ot.Gvar.Value font.Variations.Coords font.Face.Upem glyph
            Font.emScalefY font top
        | ExtentsOrigin ->
            match Extents.value font glyph with
            | ValueNone -> plan.Extents.Ascender
            | ValueSome glyphExtents ->
                let fontAdvance = plan.Extents.Ascender - plan.Extents.Descender
                clampToInt32 (int64 glyphExtents.YBearing + ((int64 fontAdvance + int64 glyphExtents.Height) >>> 1))

    /// <summary>Computes one glyph's vertical origin reading the tables every time: the reference of <see cref="originOf"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="plan">The source and font extents.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The origin.</returns>
    let private originOfReference (font: Font) (plan: VOriginPlan) (glyph: uint32) =
        let x = Font.hAdvance font glyph / 2
        syntheticShift font x (originY font plan glyph)

    /// <summary>Computes one glyph's vertical origin when the source is supported: the vertical origin before the shift is worked
    /// out once for each glyph and kept by the plan, as <c>hb_ot_font_origin_cache_t</c> keeps it, so the font that holds the plan
    /// answers each glyph from the pages.</summary>
    /// <param name="font">The font.</param>
    /// <param name="plan">The source and font extents.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The origin.</returns>
    let private originOf (font: Font) (plan: VOriginPlan) (glyph: uint32) =
        let x = Font.hAdvance font glyph / 2
        match plan.Pages.Read glyph with
        | Int32.MinValue ->
            let y = originY font plan glyph
            plan.Pages.Write (glyph, y)
            syntheticShift font x y
        | y -> syntheticShift font x y

    /// <summary>Returns one glyph's vertical origin, <c>hb_ot_get_glyph_v_origins</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The origin, or a not-yet-ported source.</returns>
    let vOrigin (font: Font) (glyph: uint32) : Result<struct (int * int), ShapeError> =
        match source font with
        | Error error -> Error error
        | Ok plan -> Ok (originOf font plan glyph)

    /// <summary>Returns one glyph's vertical origin reading the tables every time: the reference of <see cref="vOrigin"/>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The origin, or a not-yet-ported source.</returns>
    let vOriginReference (font: Font) (glyph: uint32) =
        match source font with
        | Error error -> Error error
        | Ok plan -> Ok (originOfReference font plan glyph)

    /// <summary>The vertical origin of every glyph from the origin function of a font's functions, an origin the function does not give being none.</summary>
    /// <param name="funcs">The functions.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The origins.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private vOriginsViaFuncs (funcs: FontFuncs) (glyphs: ReadOnlySpan<uint32>) =
        Sealed.map (fun glyph -> match funcs.VOrigin glyph with | ValueSome origin -> origin | ValueNone -> struct (0, 0)) glyphs

    /// <summary>Returns the vertical origin of every glyph, <c>hb_ot_get_glyph_v_origins</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The origins, or a not-yet-ported source.</returns>
    let vOrigins (font: Font) (glyphs: ReadOnlySpan<uint32>) =
        match font.Funcs with
        | ValueSome funcs -> Ok (vOriginsViaFuncs funcs glyphs)
        | ValueNone ->
            match source font with
            | Error error -> Error error
            | Ok plan -> Ok (Sealed.map (fun glyph -> originOf font plan glyph) glyphs)

    /// <summary>Subtracts every glyph's vertical origin from its offsets, <c>apply_glyph_v_origins_with_fallback (buf, -1)</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the origins subtracted, or a not-yet-ported source.</returns>
    let subtractVOrigins (font: Font) (run: GlyphRun) : Result<GlyphRun, ShapeError> =
        match run.Infos.Length with
        | 0 -> Ok run
        | _ ->
            match font.Funcs with
            | ValueSome funcs ->
                let positions =
                    Sealed.mapi (fun index (position: GlyphPosition) ->
                        let struct (x, y) = match funcs.VOrigin run.Infos[index].Codepoint with | ValueSome origin -> origin | ValueNone -> struct (0, 0)
                        { position with XOffset = position.XOffset - x; YOffset = position.YOffset - y }) (run.Positions.AsSpan())
                Ok { run with Positions = positions }
            | ValueNone ->
                match source font with
                | Error error -> Error error
                | Ok plan ->
                    let positions =
                        Sealed.mapi (fun index (position: GlyphPosition) ->
                            let struct (x, y) = originOf font plan run.Infos[index].Codepoint
                            { position with XOffset = position.XOffset - x; YOffset = position.YOffset - y }) (run.Positions.AsSpan())
                    Ok { run with Positions = positions }
