namespace Farfbuzz.Aat

open Farfbuzz

/// <summary>The apply context of one AAT subtable pass: the GDEF the written glyphs take their properties from.</summary>
[<Struct; NoComparison; NoEquality>]
type internal AatContext =
    { Gdef: Gdef }

/// <summary>The AAT apply helpers: deleted-glyph properties, written-glyph properties and the per-pass budget charge.</summary>
[<RequireQualifiedAccess>]
module internal AatApply =
    /// <summary>Marks a glyph's properties as an AAT-deleted glyph, <c>_hb_glyph_info_set_aat_deleted</c>.</summary>
    /// <param name="props">The properties.</param>
    /// <returns>The deleted properties.</returns>
    let deletedProps props =
        let formatted = UnicodeProps.setGeneralCategory GeneralCategory.Format props
        { formatted with UnicodeProps = formatted.UnicodeProps ||| UnicodeProps.CfAatDeleted ||| UnicodeProps.Hidden }

    /// <summary>Returns the properties a written glyph takes: deleted wins, else the GDEF glyph class when the face
    /// has one, else the caller's properties, <c>replace_glyph</c>'s property logic (src/hb-aat-layout-common.hh:213-247).</summary>
    /// <param name="context">The apply context.</param>
    /// <param name="glyph">The written glyph.</param>
    /// <param name="props">The caller's properties.</param>
    /// <returns>The written glyph's properties.</returns>
    let propsFor context glyph props =
        match glyph = AatLookup.DeletedGlyph with
        | true -> deletedProps props
        | false ->
            match context.Gdef.HasGlyphClasses with
            | true -> { props with GlyphClass = Gdef.glyphProps context.Gdef glyph }
            | false -> props

    /// <summary>Charges one buffer pass against the budget, failing when it runs out, <c>charge_buffer_pass</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the pass charged.</returns>
    let chargeBufferPass run =
        let maxOps = run.MaxOps - run.Infos.Length
        match maxOps < 0 with
        | true -> { run with MaxOps = maxOps; Successful = false }
        | false -> { run with MaxOps = maxOps }

    /// <summary>The scratch flag marking an AAT-deleted glyph, <c>HB_BUFFER_SCRATCH_FLAG_SHAPER0</c>.</summary>
    let aatHasDeleted = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> 0x01000000u
