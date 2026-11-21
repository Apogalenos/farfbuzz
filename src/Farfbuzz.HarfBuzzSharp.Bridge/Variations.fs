namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>One axis of a variable face as the shell shows it, <c>hb_ot_var_axis_info_t</c>.</summary>
[<Struct>]
type AxisRow =
    { AxisIndex: int
      Tag: uint32
      NameId: uint32
      Flags: uint32
      MinValue: float32
      DefaultValue: float32
      MaxValue: float32 }

/// <summary>Variations for the HarfBuzzSharp shell: the axes and named instances of a face and the variation state of a font (hb-ot-var.cc, hb-font.cc:3085-3330).</summary>
[<AbstractClass; Sealed>]
type Variations =
    /// <summary>The axes of a face, <c>hb_ot_var_get_axis_infos</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <returns>The axes in table order.</returns>
    static member Axes (face: Face) =
        let fvar = Fvar.load face
        Sealed.init fvar.Axes.Length (fun index ->
            let axis = fvar.Axes[index]
            { AxisIndex = axis.AxisIndex
              Tag = uint32 axis.Tag
              NameId = uint32 axis.NameId
              Flags = uint32 axis.Flags
              MinValue = axis.MinValue
              DefaultValue = axis.DefaultValue
              MaxValue = axis.MaxValue })

    /// <summary>Whether the face has variation data, <c>hb_ot_var_has_data</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <returns>True when the fvar table has data.</returns>
    static member HasData (face: Face) =
        (Fvar.load face).HasData

    /// <summary>The first axis with a tag, <c>hb_ot_var_find_axis_info</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <param name="tag">The axis tag.</param>
    /// <returns>Whether the axis exists, and the axis (zero when it does not).</returns>
    static member FindAxis (face: Face, tag: uint32) =
        let axes = Variations.Axes face
        let found = Seq.tryFind (fun (axis: AxisRow) -> axis.Tag = tag) axes
        match found with
        | Some axis -> struct (true, axis)
        | None -> struct (false, Unchecked.defaultof<AxisRow>)

    /// <summary>The number of named instances, <c>hb_ot_var_get_named_instance_count</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <returns>The count.</returns>
    static member NamedInstanceCount (face: Face) =
        (Fvar.load face).InstanceCount

    /// <summary>The subfamily name id of a named instance, <c>hb_ot_var_named_instance_get_subfamily_name_id</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <param name="instance">The instance index.</param>
    /// <returns>The name id, <c>HB_OT_NAME_ID_INVALID</c> past the instances.</returns>
    static member SubfamilyNameId (face: Face, instance: int) =
        uint32 (Fvar.instanceSubfamilyNameId instance (Fvar.load face))

    /// <summary>The PostScript name id of a named instance, <c>hb_ot_var_named_instance_get_postscript_name_id</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <param name="instance">The instance index.</param>
    /// <returns>The name id, <c>HB_OT_NAME_ID_INVALID</c> past the instances or when the records have none.</returns>
    static member PostScriptNameId (face: Face, instance: int) =
        uint32 (Fvar.instancePostscriptNameId instance (Fvar.load face))

    /// <summary>The design coordinates of a named instance, <c>hb_ot_var_named_instance_get_design_coords</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <param name="instance">The instance index.</param>
    /// <returns>One coordinate per axis, none past the instances.</returns>
    static member NamedInstanceCoords (face: Face, instance: int) =
        Fvar.instanceCoords instance (Fvar.load face)

    /// <summary>The variations as the core takes them.</summary>
    /// <param name="tags">The axis tags.</param>
    /// <param name="values">The values, one per tag.</param>
    /// <returns>The variations.</returns>
    static member private Pairs (tags: uint32 array, values: float32 array) =
        Sealed.init tags.Length (fun index ->
            { Variation.Tag = LanguagePrimitives.UInt32WithMeasure tags[index]; Value = values[index] })

    /// <summary>A font with variation values applied, <c>hb_font_set_variations</c>: every axis not named is at its default.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="tags">The axis tags.</param>
    /// <param name="values">The values, one per tag.</param>
    /// <returns>The font with the new variation state.</returns>
    static member Set (font: Font, tags: uint32 array, values: float32 array) =
        Font.setVariations (Variations.Pairs (tags, values)) font

    /// <summary>A font with design coordinates applied, <c>hb_font_set_var_coords_design</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="coords">The coordinates, one per axis.</param>
    /// <returns>The font with the new variation state.</returns>
    static member SetDesign (font: Font, coords: float32 array) =
        Font.setVarCoordsDesign (Sealed.ofSpan (ReadOnlySpan coords)) font

    /// <summary>A font with normalized coordinates applied, <c>hb_font_set_var_coords_normalized</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="coords">The 2.14 coordinates, one per axis.</param>
    /// <returns>The font with the new variation state.</returns>
    static member SetNormalized (font: Font, coords: int array) =
        Font.setVarCoordsNormalized (Sealed.ofSpan (ReadOnlySpan coords)) font

    /// <summary>A font with a named instance applied, <c>hb_font_set_var_named_instance</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="instance">The instance index.</param>
    /// <returns>The font with the new variation state.</returns>
    static member SetNamedInstance (font: Font, instance: int) =
        Font.setVarNamedInstance (uint32 instance) font

    /// <summary>The normalized coordinates of a font, <c>hb_font_get_var_coords_normalized</c>.</summary>
    /// <param name="font">The core font.</param>
    /// <returns>The coordinates, none until some were set.</returns>
    static member Normalized (font: Font) =
        Font.varCoordsNormalized font

/// <summary>Colour palettes and colour glyph kinds for the HarfBuzzSharp shell (hb-ot-color.cc).</summary>
[<AbstractClass; Sealed>]
type internal Colors =
    /// <summary>The palette table of a face, <c>face->table.CPAL</c>.</summary>
    /// <param name="table">The CPAL table bytes, empty when the face has none.</param>
    /// <returns>The table, empty when it fails the sanitizer.</returns>
    static member Palettes (table: ReadOnlyMemory<byte>) =
        Cpal.load table

    /// <summary>The number of palettes, <c>hb_ot_color_palette_get_count</c>.</summary>
    /// <param name="cpal">The table.</param>
    /// <returns>The count.</returns>
    static member PaletteCount (cpal: Cpal) =
        cpal.PaletteCount

    /// <summary>The flags of a palette, <c>hb_ot_color_palette_get_flags</c>.</summary>
    /// <param name="cpal">The table.</param>
    /// <param name="index">The palette index.</param>
    /// <returns>The flags.</returns>
    static member PaletteFlags (cpal: Cpal, index: int) =
        Cpal.paletteFlags index cpal

    /// <summary>The name id of a palette, <c>hb_ot_color_palette_get_name_id</c>.</summary>
    /// <param name="cpal">The table.</param>
    /// <param name="index">The palette index.</param>
    /// <returns>The name id.</returns>
    static member PaletteNameId (cpal: Cpal, index: int) =
        uint32 (Cpal.paletteNameId index cpal)

    /// <summary>The name id of a colour entry, <c>hb_ot_color_palette_color_get_name_id</c>.</summary>
    /// <param name="cpal">The table.</param>
    /// <param name="index">The colour index.</param>
    /// <returns>The name id.</returns>
    static member ColorNameId (cpal: Cpal, index: int) =
        uint32 (Cpal.colorNameId index cpal)

    /// <summary>The colours of a palette, <c>hb_ot_color_palette_get_colors</c>.</summary>
    /// <param name="cpal">The table.</param>
    /// <param name="index">The palette index.</param>
    /// <param name="start">The first colour wanted.</param>
    /// <param name="count">The most colours wanted.</param>
    /// <returns>The number of colours in a palette and the colours asked for.</returns>
    static member PaletteColors (cpal: Cpal, index: int, start: int, count: int) =
        Cpal.paletteColors index start count cpal

    /// <summary>Whether the COLR table has layers, <c>hb_ot_color_has_layers</c>.</summary>
    /// <param name="colr">The COLR table bytes.</param>
    /// <returns>True when it has.</returns>
    static member HasLayers (colr: ReadOnlyMemory<byte>) =
        ColorTables.hasLayers colr

    /// <summary>Whether the face has PNG images, <c>hb_ot_color_has_png</c>.</summary>
    /// <param name="cbdt">The CBDT table bytes.</param>
    /// <param name="sbix">The sbix table bytes.</param>
    /// <returns>True when it has.</returns>
    static member HasPng (cbdt: ReadOnlyMemory<byte>, sbix: ReadOnlyMemory<byte>) =
        ColorTables.hasPng cbdt sbix

    /// <summary>Whether the face has SVG documents, <c>hb_ot_color_has_svg</c>.</summary>
    /// <param name="svg">The SVG table bytes.</param>
    /// <returns>True when it has.</returns>
    static member HasSvg (svg: ReadOnlyMemory<byte>) =
        ColorTables.hasSvg svg
