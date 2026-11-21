namespace Farfbuzz.Aat

open System
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>The AAT layout entry points the plan and stage composition call.</summary>
[<RequireQualifiedAccess>]
module internal AatLayout =
    /// <summary>The point size used when the font has none, <c>HB_CORETEXT_DEFAULT_FONT_SIZE</c>.</summary>
    [<Literal>]
    let private CoreTextDefaultFontSize = 12.0f

    /// <summary>Adds two positions, saturating instead of overflowing, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The first position.</param>
    /// <param name="b">The second position.</param>
    /// <returns>The saturated sum.</returns>
    let private saturateAdd (a: int) (b: int) : int =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Applies tracking to the first glyph of every grapheme, <c>AAT::trak::apply</c>. A run whose
    /// tracking value is zero, or that has no positions, returns the input run unchanged.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The tracked run.</returns>
    let track (font: Font) (run: GlyphRun) : GlyphRun =
        let horizontal = Direction.isHorizontal run.SegmentProperties.Direction
        let ptem = if font.Ptem > 0f then font.Ptem else CoreTextDefaultFontSize
        let tracking = Trak.getTracking font.Ot.Trak.Value horizontal ptem 0f
        let advanceToAdd = if horizontal then Font.emScalefX font tracking else Font.emScalefY font tracking
        match advanceToAdd = 0 || run.Positions.Length = 0 with
        | true -> run
        | false ->
            let positions =
                Sealed.mapi (fun index (position: GlyphPosition) ->
                    match index = 0 || not (UnicodeProps.isContinuation run.Props[index]) with
                    | false -> position
                    | true ->
                        match horizontal with
                        | true -> { position with XAdvance = saturateAdd position.XAdvance advanceToAdd }
                        | false -> { position with YAdvance = saturateAdd position.YAdvance advanceToAdd }) (run.Positions.AsSpan())
            { run with Positions = positions }

    /// <summary>Whether a face includes AAT substitutions, <c>hb_aat_layout_has_substitution</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>True when morx or mort has data.</returns>
    let hasSubstitution (face: OtFace) =
        Morx.hasData face.Morx.Value || Morx.hasData face.Mort.Value

    /// <summary>Whether a face includes AAT positioning, <c>hb_aat_layout_has_positioning</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>True when kerx has data.</returns>
    let hasPositioning (face: OtFace) =
        Kerx.hasData face.Kerx.Value

    /// <summary>Runs the AAT positioning pass, <c>hb_aat_layout_position</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let position (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        KerxPairs.apply plan font run

    /// <summary>Runs the AAT positioning pass, <c>hb_aat_layout_position</c>, on a run whose infos, properties and positions the caller
    /// made in this call and shares with no one, which the pair subtables write in place.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The positioned run.</returns>
    let positionOwned (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        KerxPairs.applyOwned plan font run

    /// <summary>Runs the AAT substitution pass, <c>hb_aat_layout_substitute</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run.</param>
    /// <returns>The substituted run.</returns>
    let substitute (plan: OtShapePlan) (font: Font) (features: ReadOnlySpan<Feature>) (run: GlyphRun) =
        let face = font.Ot
        let table = if Morx.hasData face.Morx.Value then face.Morx.Value else face.Mort.Value
        match Morx.hasData table with
        | false -> run
        | true ->
            let map =
                match features.Length with
                | 0 -> AatMap.defaults face plan.Props
                | _ -> AatMap.build face plan.Props features
            MorxApply.apply table map face run.SegmentProperties run

    /// <summary>Runs the AAT substitution pass, <c>hb_aat_layout_substitute</c>, on arrays the caller made in this call and shares
    /// with no one, which the subtables write in place.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="run">The run, whose infos and properties are the caller's own.</param>
    /// <returns>The substituted run, and whether any subtable wrote a glyph, a property or a flag of it.</returns>
    let substituteOwned (plan: OtShapePlan) (font: Font) (features: ReadOnlySpan<Feature>) (run: GlyphRun) =
        let face = font.Ot
        let table = if Morx.hasData face.Morx.Value then face.Morx.Value else face.Mort.Value
        match Morx.hasData table with
        | false -> struct (run, false)
        | true ->
            let map =
                match features.Length with
                | 0 -> AatMap.defaults face plan.Props
                | _ -> AatMap.build face plan.Props features
            MorxApply.applyOwned table map face run.SegmentProperties run

    /// <summary>The deletion predicate of <c>hb_aat_layout_remove_deleted_glyphs</c>, <c>_hb_glyph_info_is_aat_deleted</c>.</summary>
    [<Struct>]
    type private AatDeletedPredicate =
        interface IGlyphPredicate with
            member _.Delete (_, props) =
                UnicodeProps.generalCategory props = GeneralCategory.Format
                && props.UnicodeProps &&& UnicodeProps.CfAatDeleted <> 0us

    /// <summary>Removes AAT-deleted glyphs in place, <c>hb_aat_layout_remove_deleted_glyphs</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with deleted glyphs removed.</returns>
    let removeDeletedGlyphs (run: GlyphRun) =
        match run.ScratchFlags &&& AatApply.aatHasDeleted = ScratchFlags.Default with
        | true -> run
        | false -> GlyphRun.deleteGlyphsInplace (AatDeletedPredicate()) run
