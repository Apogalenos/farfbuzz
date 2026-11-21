namespace Farfbuzz

open System
open System.Runtime.InteropServices

/// <summary>Buffer flags, the values of <c>hb_buffer_flags_t</c>.</summary>
[<Flags>]
type BufferFlags =
    | Default = 0x00u
    | Bot = 0x01u
    | Eot = 0x02u
    | PreserveDefaultIgnorables = 0x04u
    | RemoveDefaultIgnorables = 0x08u
    | DoNotInsertDottedCircle = 0x10u
    | Verify = 0x20u
    | ProduceUnsafeToConcat = 0x40u
    | ProduceSafeToInsertTatweel = 0x80u
    | Defined = 0xFFu

/// <summary>The cluster level, the values of <c>hb_buffer_cluster_level_t</c>.</summary>
[<Struct>]
type ClusterLevel =
    | MonotoneGraphemes
    | MonotoneCharacters
    | Characters
    | Graphemes

/// <summary>Scratch bits accumulated while computing Unicode properties.</summary>
[<Flags>]
type internal ScratchFlags =
    | Default = 0x00u
    | HasFractionSlash = 0x01u
    | HasDefaultIgnorables = 0x02u
    | HasSpaceFallback = 0x04u
    | HasGposAttachment = 0x08u
    | HasCgj = 0x10u
    | HasBrokenSyllable = 0x20u
    | HasVariationSelectorFallback = 0x40u
    | HasContinuations = 0x80u
    | Shaper0 = 0x01000000u

/// <summary>
/// The per-glyph shaper properties, unpacked from <c>hb_glyph_info_t.var1</c>/<c>var2</c> and kept in their
/// own array parallel to the glyph infos.
/// </summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type internal GlyphProps =
    { UnicodeProps: uint16
      GlyphClass: uint16
      LigProps: byte
      Syllable: byte
      ShaperCategory: byte
      ShaperAuxiliary: byte
      GlyphIndex: uint32 }

/// <summary>The glyph-properties bits of <c>hb_ot_layout_glyph_props_flags_t</c> (src/hb-ot-layout.hh:84-99).</summary>
[<RequireQualifiedAccess>]
module internal GlyphPropsBits =
    /// <summary>The base-glyph bit, <c>HB_OT_LAYOUT_GLYPH_PROPS_BASE_GLYPH</c>.</summary>
    [<Literal>]
    let Base = 0x02us

    /// <summary>The ligature bit, <c>HB_OT_LAYOUT_GLYPH_PROPS_LIGATURE</c>.</summary>
    [<Literal>]
    let Ligature = 0x04us

    /// <summary>The mark bit, <c>HB_OT_LAYOUT_GLYPH_PROPS_MARK</c>.</summary>
    [<Literal>]
    let Mark = 0x08us

    /// <summary>The substituted bit, <c>HB_OT_LAYOUT_GLYPH_PROPS_SUBSTITUTED</c>.</summary>
    [<Literal>]
    let Substituted = 0x10us

    /// <summary>The ligated bit, <c>HB_OT_LAYOUT_GLYPH_PROPS_LIGATED</c>.</summary>
    [<Literal>]
    let Ligated = 0x20us

    /// <summary>The multiplied bit, <c>HB_OT_LAYOUT_GLYPH_PROPS_MULTIPLIED</c>.</summary>
    [<Literal>]
    let Multiplied = 0x40us

    /// <summary>The preserved bits, <c>HB_OT_LAYOUT_GLYPH_PROPS_PRESERVE</c>.</summary>
    [<Literal>]
    let Preserve = 0x70us

    /// <summary>The ligature-base bit of the lig-props byte, <c>IS_LIG_BASE</c>.</summary>
    [<Literal>]
    let private LigBase = 0x10uy

    /// <summary>Returns the ligature id, <c>_hb_glyph_info_get_lig_id</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The ligature id in the high three bits.</returns>
    let ligId (props: GlyphProps) : byte =
        props.LigProps >>> 5

    /// <summary>Returns the ligature component index, <c>_hb_glyph_info_get_lig_comp</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The component, zero for a ligature base.</returns>
    let ligComp (props: GlyphProps) : byte =
        match props.LigProps &&& LigBase <> 0uy with
        | true -> 0uy
        | false -> props.LigProps &&& 0x0Fuy

    /// <summary>Returns the ligature component count, <c>_hb_glyph_info_get_lig_num_comps</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The component count, one for a non-ligature.</returns>
    let ligNumComps (props: GlyphProps) : byte =
        match props.GlyphClass &&& Ligature <> 0us && props.LigProps &&& LigBase <> 0uy with
        | true -> props.LigProps &&& 0x0Fuy
        | false -> 1uy

    /// <summary>Returns the component count contributed by a glyph inside a ligation,
    /// <c>_hb_glyph_info_get_lig_num_comps_in_ligation</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The count, zero for a multiplied non-first component.</returns>
    let ligNumCompsInLigation (props: GlyphProps) : byte =
        match props.GlyphClass &&& Multiplied <> 0us && ligComp props <> 0uy with
        | true -> 0uy
        | false -> ligNumComps props

    /// <summary>Sets the lig-props of a ligature base, <c>_hb_glyph_info_set_lig_props_for_ligature</c>.</summary>
    /// <param name="ligatureId">The ligature id.</param>
    /// <param name="componentCount">The component count.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The properties with the lig-props set.</returns>
    let setLigPropsForLigature (ligatureId: byte) (componentCount: byte) (props: GlyphProps) : GlyphProps =
        { props with LigProps = (ligatureId <<< 5) ||| LigBase ||| (componentCount &&& 0x0Fuy) }

    /// <summary>Sets the lig-props of a mark attached to a ligature, <c>_hb_glyph_info_set_lig_props_for_mark</c>.</summary>
    /// <param name="ligatureId">The ligature id.</param>
    /// <param name="componentIndex">The component index.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The properties with the lig-props set.</returns>
    let setLigPropsForMark (ligatureId: byte) (componentIndex: byte) (props: GlyphProps) : GlyphProps =
        { props with LigProps = (ligatureId <<< 5) ||| (componentIndex &&& 0x0Fuy) }

    /// <summary>Sets the lig-props of a multiple-substitution component, <c>_hb_glyph_info_set_lig_props_for_component</c>.</summary>
    /// <param name="componentIndex">The component index.</param>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The properties with the lig-props set.</returns>
    let setLigPropsForComponent (componentIndex: byte) (props: GlyphProps) : GlyphProps =
        setLigPropsForMark 0uy componentIndex props

    /// <summary>Whether a glyph is a base glyph, <c>_hb_glyph_info_is_base_glyph</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the base bit is set.</returns>
    let isBaseGlyph (props: GlyphProps) : bool =
        props.GlyphClass &&& Base <> 0us

    /// <summary>Whether a glyph is a ligature, <c>_hb_glyph_info_is_ligature</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the ligature bit is set.</returns>
    let isLigature (props: GlyphProps) : bool =
        props.GlyphClass &&& Ligature <> 0us

    /// <summary>Whether a glyph is a mark, <c>_hb_glyph_info_is_mark</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the mark bit is set.</returns>
    let isMark (props: GlyphProps) : bool =
        props.GlyphClass &&& Mark <> 0us

    /// <summary>Whether a glyph was substituted, <c>_hb_glyph_info_substituted</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the substituted bit is set.</returns>
    let substituted (props: GlyphProps) : bool =
        props.GlyphClass &&& Substituted <> 0us

    /// <summary>Whether a glyph was ligated, <c>_hb_glyph_info_ligated</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the ligated bit is set.</returns>
    let ligated (props: GlyphProps) : bool =
        props.GlyphClass &&& Ligated <> 0us

    /// <summary>Whether a glyph was multiplied, <c>_hb_glyph_info_multiplied</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when the multiplied bit is set.</returns>
    let multiplied (props: GlyphProps) : bool =
        props.GlyphClass &&& Multiplied <> 0us

    /// <summary>Whether a glyph was ligated without being multiplied,
    /// <c>_hb_glyph_info_ligated_and_didnt_multiply</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>True when ligated and not multiplied.</returns>
    let ligatedAndDidntMultiply (props: GlyphProps) : bool =
        ligated props && not (multiplied props)

    /// <summary>Clears the ligated and multiplied bits, <c>_hb_glyph_info_clear_ligated_and_multiplied</c>.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <returns>The properties with the bits cleared.</returns>
    let clearLigatedAndMultiplied (props: GlyphProps) : GlyphProps =
        { props with GlyphClass = props.GlyphClass &&& ~~~(Ligated ||| Multiplied) }
