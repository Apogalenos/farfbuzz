module Farfbuzz.Oracle.Native

open System
open System.Runtime.InteropServices

#nowarn "9"

/// <summary>The HarfBuzz version string, <c>hb_version_string</c>.</summary>
/// <returns>A pointer to a static NUL-terminated string.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_version_string()

/// <summary>Converts a tag spelling into a tag, <c>hb_tag_from_string</c>.</summary>
/// <param name="text">The UTF-8 bytes.</param>
/// <param name="length">Their length, or -1 for NUL-terminated.</param>
/// <returns>The tag.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_tag_from_string(byte[] text, int length)

/// <summary>Reads the version of the loaded HarfBuzz.</summary>
/// <returns>The version, for example <c>15.1.0</c>.</returns>
let version () =
    match Marshal.PtrToStringAnsi(hb_version_string ()) with
    | null -> String.Empty
    | text -> text

/// <summary>Parses a feature string, <c>hb_feature_from_string</c>.</summary>
/// <param name="text">The UTF-8 bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="feature">The parsed feature, zeroed on failure.</param>
/// <returns>Non-zero on success.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_feature_from_string(byte[] text, int length, Farfbuzz.Feature& feature)

/// <summary>Formats a feature, <c>hb_feature_to_string</c>.</summary>
/// <param name="feature">The feature.</param>
/// <param name="buffer">The output buffer.</param>
/// <param name="size">Its size in bytes.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_feature_to_string(Farfbuzz.Feature& feature, byte[] buffer, uint32 size)

/// <summary>Parses a variation string, <c>hb_variation_from_string</c>.</summary>
/// <param name="text">The UTF-8 bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="variation">The parsed variation, zeroed on failure.</param>
/// <returns>Non-zero on success.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_variation_from_string(byte[] text, int length, Farfbuzz.Variation& variation)

/// <summary>Formats a variation, <c>hb_variation_to_string</c>.</summary>
/// <param name="variation">The variation.</param>
/// <param name="buffer">The output buffer.</param>
/// <param name="size">Its size in bytes.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_variation_to_string(Farfbuzz.Variation& variation, byte[] buffer, uint32 size)

/// <summary>Creates an empty buffer, <c>hb_buffer_create</c>.</summary>
/// <returns>The buffer handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_buffer_create()

/// <summary>Releases a buffer, <c>hb_buffer_destroy</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_destroy(nativeint buffer)

/// <summary>Sets what a buffer holds, <c>hb_buffer_set_content_type</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="contentType">1 for Unicode, 2 for glyphs.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_content_type(nativeint buffer, int contentType)

/// <summary>Resizes a buffer, zeroing new items, <c>hb_buffer_set_length</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="length">The new length.</param>
/// <returns>Non-zero on success.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_buffer_set_length(nativeint buffer, uint32 length)

/// <summary>The number of items in a buffer, <c>hb_buffer_get_length</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <returns>The length.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_buffer_get_length(nativeint buffer)

/// <summary>The glyph infos of a buffer, <c>hb_buffer_get_glyph_infos</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="length">Receives the number of items.</param>
/// <returns>A pointer to the infos.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_buffer_get_glyph_infos(nativeint buffer, uint32& length)

/// <summary>The glyph positions of a buffer, created zeroed when absent, <c>hb_buffer_get_glyph_positions</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="length">Receives the number of items.</param>
/// <returns>A pointer to the positions.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_buffer_get_glyph_positions(nativeint buffer, uint32& length)

/// <summary>Serialises glyphs, <c>hb_buffer_serialize_glyphs</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="start">The first item.</param>
/// <param name="stop">The item after the last.</param>
/// <param name="output">The output buffer.</param>
/// <param name="size">Its size in bytes.</param>
/// <param name="consumed">Receives the bytes written.</param>
/// <param name="font">The font for names and extents, or zero.</param>
/// <param name="format">The format tag; TEXT is used here.</param>
/// <param name="flags">The serialisation flags.</param>
/// <returns>The number of items serialised.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_buffer_serialize_glyphs(nativeint buffer, uint32 start, uint32 stop, byte[] output, uint32 size, uint32& consumed, nativeint font, uint32 format, uint32 flags)

/// <summary>Deserialises glyphs, <c>hb_buffer_deserialize_glyphs</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="text">The UTF-8 bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="endPointer">Receives where parsing stopped.</param>
/// <param name="font">The font for glyph names, or zero.</param>
/// <param name="format">The format tag; TEXT is used here.</param>
/// <returns>Non-zero on success.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_buffer_deserialize_glyphs(nativeint buffer, byte[] text, int length, nativeint& endPointer, nativeint font, uint32 format)

/// <summary>Sets the point size, <c>hb_font_set_ptem</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="ptem">Points per em.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_ptem(nativeint font, float32 ptem)

/// <summary>Sets synthetic emboldening, <c>hb_font_set_synthetic_bold</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="x">Horizontal strength.</param>
/// <param name="y">Vertical strength.</param>
/// <param name="inPlace">Non-zero to keep advances unchanged.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_synthetic_bold(nativeint font, float32 x, float32 y, int inPlace)

/// <summary>Sets synthetic slant, <c>hb_font_set_synthetic_slant</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="slant">The slant ratio.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_synthetic_slant(nativeint font, float32 slant)

/// <summary>Sets the scale, <c>hb_font_set_scale</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="x">Horizontal scale.</param>
/// <param name="y">Vertical scale.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_scale(nativeint font, int x, int y)

/// <summary>Sets design-space coordinates, <c>hb_font_set_variations</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="variations">The coordinates.</param>
/// <param name="count">Their number.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_variations(nativeint font, Farfbuzz.Variation[] variations, uint32 count)

/// <summary>Selects font functions by name, <c>hb_font_set_funcs_using</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="name">The implementation name, for example ot.</param>
/// <returns>Non-zero on success.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_font_set_funcs_using(nativeint font, [<MarshalAs(UnmanagedType.LPStr)>] string name)

/// <summary>The units per em of a face, <c>hb_face_get_upem</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The units per em.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_face_get_upem(nativeint face)

/// <summary>Appends code points with context, <c>hb_buffer_add_codepoints</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="text">The code points.</param>
/// <param name="length">Their number.</param>
/// <param name="itemOffset">The first one to add; those before are pre-context.</param>
/// <param name="itemLength">How many to add; those after are post-context.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_add_codepoints(nativeint buffer, uint32[] text, int length, uint32 itemOffset, int itemLength)

/// <summary>Parses a direction, <c>hb_direction_from_string</c>.</summary>
/// <param name="text">The UTF-8 bytes, or null.</param>
/// <param name="length">Their length.</param>
/// <returns>The direction value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_direction_from_string(byte[] text, int length)

/// <summary>Parses a script, <c>hb_script_from_string</c>.</summary>
/// <param name="text">The UTF-8 bytes, or null.</param>
/// <param name="length">Their length.</param>
/// <returns>The script tag.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_script_from_string(byte[] text, int length)

/// <summary>Parses a language, <c>hb_language_from_string</c>.</summary>
/// <param name="text">The UTF-8 bytes, or null.</param>
/// <param name="length">Their length.</param>
/// <returns>The interned language.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_language_from_string(byte[] text, int length)

/// <summary>Sets the direction, <c>hb_buffer_set_direction</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="direction">The direction value.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_direction(nativeint buffer, int direction)

/// <summary>Sets the script, <c>hb_buffer_set_script</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="script">The script tag.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_script(nativeint buffer, uint32 script)

/// <summary>Sets the language, <c>hb_buffer_set_language</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="language">The interned language.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_language(nativeint buffer, nativeint language)

/// <summary>Sets the buffer flags, <c>hb_buffer_set_flags</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="flags">The flags.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_flags(nativeint buffer, uint32 flags)

/// <summary>Sets the cluster level, <c>hb_buffer_set_cluster_level</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="level">The level, 0 to 3.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_cluster_level(nativeint buffer, int level)

/// <summary>Sets the glyph for unsupported variation selectors, <c>hb_buffer_set_not_found_variation_selector_glyph</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="glyph">The glyph, or HB_CODEPOINT_INVALID.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_not_found_variation_selector_glyph(nativeint buffer, uint32 glyph)

/// <summary>Guesses unset segment properties, <c>hb_buffer_guess_segment_properties</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_guess_segment_properties(nativeint buffer)

/// <summary>Shapes a buffer, <c>hb_shape_full</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="buffer">The buffer handle.</param>
/// <param name="features">The user features.</param>
/// <param name="count">Their number.</param>
/// <param name="shapers">A shaper list ending with a null element, or a null array for all shapers.</param>
/// <returns>Non-zero on success.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_shape_full(nativeint font, nativeint buffer, Farfbuzz.Feature[] features, uint32 count, [<MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)>] string[] shapers)

/// <summary>The shapers compiled in, <c>hb_shape_list_shapers</c>.</summary>
/// <returns>A NULL-terminated array of names.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_shape_list_shapers()

/// <summary>The font-function implementations compiled in, <c>hb_font_list_funcs</c>.</summary>
/// <returns>A NULL-terminated array of names.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_font_list_funcs()

/// <summary>Reads a NULL-terminated array of C strings.</summary>
/// <param name="pointer">The array.</param>
/// <returns>The strings.</returns>
let stringList (pointer: nativeint) =
    let rec read (index: int) (names: string list) =
        match Marshal.ReadIntPtr(pointer, index * IntPtr.Size) with
        | 0n -> List.rev names
        | entry -> read (index + 1) ((Marshal.PtrToStringAnsi entry |> nonNull) :: names)
    read 0 []

/// <summary>Creates an empty set, <c>hb_set_create</c>.</summary>
/// <returns>The set handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_set_create()

/// <summary>Releases a set, <c>hb_set_destroy</c>.</summary>
/// <param name="set">The set handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_set_destroy(nativeint set)

/// <summary>Advances to the next member of a set, <c>hb_set_next</c>.</summary>
/// <param name="set">The set handle.</param>
/// <param name="codepoint">The current member, HB_SET_VALUE_INVALID to start; receives the next.</param>
/// <returns>Non-zero while a member was found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_set_next(nativeint set, uint32& codepoint)

/// <summary>Collects the code points a face maps, <c>hb_face_collect_unicodes</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="set">The set receiving them.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_face_collect_unicodes(nativeint face, nativeint set)

/// <summary>Creates a blob by copying bytes, <c>hb_blob_create</c>.</summary>
/// <param name="data">The bytes.</param>
/// <param name="length">Their number.</param>
/// <param name="mode">The memory mode; 0 duplicates the data.</param>
/// <param name="userData">The destroy closure's data.</param>
/// <param name="destroy">The destroy closure, or zero.</param>
/// <returns>The blob handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_blob_create(byte[] data, uint32 length, int mode, nativeint userData, nativeint destroy)

/// <summary>Releases a blob, <c>hb_blob_destroy</c>.</summary>
/// <param name="blob">The blob handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_blob_destroy(nativeint blob)

/// <summary>The length of a blob, <c>hb_blob_get_length</c>.</summary>
/// <param name="blob">The blob handle.</param>
/// <returns>The length in bytes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_blob_get_length(nativeint blob)

/// <summary>Creates a face from a blob, <c>hb_face_create</c>.</summary>
/// <param name="blob">The file blob.</param>
/// <param name="index">The face index; only the low 16 bits select.</param>
/// <returns>The face handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_face_create(nativeint blob, uint32 index)

/// <summary>Releases a face, <c>hb_face_destroy</c>.</summary>
/// <param name="face">The face handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_face_destroy(nativeint face)

/// <summary>The number of faces in a blob, <c>hb_face_count</c>.</summary>
/// <param name="blob">The file blob.</param>
/// <returns>The face count.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_face_count(nativeint blob)

/// <summary>The number of glyphs in a face, <c>hb_face_get_glyph_count</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The glyph count.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_face_get_glyph_count(nativeint face)

/// <summary>Lists a face's table tags, <c>hb_face_get_table_tags</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="startOffset">The first tag.</param>
/// <param name="tableCount">The capacity on input, the number written on output.</param>
/// <param name="tableTags">The output array.</param>
/// <returns>The total number of tags.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_face_get_table_tags(nativeint face, uint32 startOffset, uint32& tableCount, uint32[] tableTags)

/// <summary>References one table of a face, <c>hb_face_reference_table</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tag">The table tag.</param>
/// <returns>A new blob, empty when absent.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_face_reference_table(nativeint face, uint32 tag)

/// <summary>The native <c>hb_font_extents_t</c> layout: three values and nine reserved slots.</summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type OracleFontExtents =
    { Ascender: int
      Descender: int
      LineGap: int
      Reserved9: int
      Reserved8: int
      Reserved7: int
      Reserved6: int
      Reserved5: int
      Reserved4: int
      Reserved3: int
      Reserved2: int
      Reserved1: int }

/// <summary>Creates a font from a face, <c>hb_font_create</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The font handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_font_create(nativeint face)

/// <summary>Releases a font, <c>hb_font_destroy</c>.</summary>
/// <param name="font">The font handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_destroy(nativeint font)

/// <summary>Selects the OpenType font functions, <c>hb_ot_font_set_funcs</c>.</summary>
/// <param name="font">The font handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_font_set_funcs(nativeint font)

/// <summary>Sets the pixels per em, <c>hb_font_set_ppem</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="x">The horizontal ppem.</param>
/// <param name="y">The vertical ppem.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_ppem(nativeint font, uint32 x, uint32 y)

/// <summary>Reads horizontal advances in one call, <c>hb_font_get_glyph_h_advances</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="count">The number of glyphs.</param>
/// <param name="firstGlyph">The glyph array.</param>
/// <param name="glyphStride">The byte stride between glyphs.</param>
/// <param name="firstAdvance">The advance array.</param>
/// <param name="advanceStride">The byte stride between advances.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_get_glyph_h_advances(nativeint font, uint32 count, uint32[] firstGlyph, uint32 glyphStride, int[] firstAdvance, uint32 advanceStride)

/// <summary>Reads vertical advances in one call, <c>hb_font_get_glyph_v_advances</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="count">The number of glyphs.</param>
/// <param name="firstGlyph">The glyph array.</param>
/// <param name="glyphStride">The byte stride between glyphs.</param>
/// <param name="firstAdvance">The advance array.</param>
/// <param name="advanceStride">The byte stride between advances.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_get_glyph_v_advances(nativeint font, uint32 count, uint32[] firstGlyph, uint32 glyphStride, int[] firstAdvance, uint32 advanceStride)

/// <summary>Reads horizontal font extents, <c>hb_font_get_h_extents</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="extents">The output extents.</param>
/// <returns>Non-zero when the extents exist.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_font_get_h_extents(nativeint font, OracleFontExtents& extents)

/// <summary>Reads vertical font extents, <c>hb_font_get_v_extents</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="extents">The output extents.</param>
/// <returns>Non-zero when the extents exist.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_font_get_v_extents(nativeint font, OracleFontExtents& extents)

/// <summary>The native <c>hb_glyph_extents_t</c> layout: four position values.</summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type OracleGlyphExtents =
    { XBearing: int
      YBearing: int
      Width: int
      Height: int }

/// <summary>Reads a glyph's ink extents, <c>hb_font_get_glyph_extents</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="extents">The output extents, zeroed when absent.</param>
/// <returns>Non-zero when the glyph has extents.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_font_get_glyph_extents(nativeint font, uint32 glyph, OracleGlyphExtents& extents)

/// <summary>Reads a glyph's vertical origin, <c>hb_font_get_glyph_v_origin</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="x">Receives the horizontal origin.</param>
/// <param name="y">Receives the vertical origin.</param>
/// <returns>Non-zero when the origin exists.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_font_get_glyph_v_origin(nativeint font, uint32 glyph, int& x, int& y)

/// <summary>Fetches a metric position, <c>hb_ot_metrics_get_position</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="metricsTag">The metric tag.</param>
/// <param name="position">Receives the position.</param>
/// <returns>Non-zero when the font provides the metric.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_metrics_get_position(nativeint font, uint32 metricsTag, int& position)

/// <summary>Fetches a metric position with a synthesized fallback, <c>hb_ot_metrics_get_position_with_fallback</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="metricsTag">The metric tag.</param>
/// <param name="position">Receives the position.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_metrics_get_position_with_fallback(nativeint font, uint32 metricsTag, int& position)

/// <summary>Advances to the next range of a set, <c>hb_set_next_range</c>.</summary>
/// <param name="set">The set handle.</param>
/// <param name="first">Receives the first code point.</param>
/// <param name="last">The current last code point on input, the range end on output.</param>
/// <returns>Non-zero while a range was found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_set_next_range(nativeint set, uint32& first, uint32& last)

/// <summary>Creates an empty map, <c>hb_map_create</c>.</summary>
/// <returns>The map handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_map_create()

/// <summary>Releases a map, <c>hb_map_destroy</c>.</summary>
/// <param name="map">The map handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_map_destroy(nativeint map)

/// <summary>Advances through a map, <c>hb_map_next</c>.</summary>
/// <param name="map">The map handle.</param>
/// <param name="index">The current index, -1 to start.</param>
/// <param name="key">Receives the key.</param>
/// <param name="value">Receives the value.</param>
/// <returns>Non-zero while an entry was found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_map_next(nativeint map, int& index, uint32& key, uint32& value)

/// <summary>Collects the nominal glyph mapping, <c>hb_face_collect_nominal_glyph_mapping</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="mapping">The output map.</param>
/// <param name="unicodes">The output code-point set.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_face_collect_nominal_glyph_mapping(nativeint face, nativeint mapping, nativeint unicodes)

/// <summary>Collects variation selectors, <c>hb_face_collect_variation_selectors</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="out">The output set.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_face_collect_variation_selectors(nativeint face, nativeint out)

/// <summary>Collects one selector's code points, <c>hb_face_collect_variation_unicodes</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="selector">The variation selector.</param>
/// <param name="out">The output set.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_face_collect_variation_unicodes(nativeint face, uint32 selector, nativeint out)

/// <summary>Looks up a variation glyph, <c>hb_font_get_variation_glyph</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="unicode">The code point.</param>
/// <param name="selector">The variation selector.</param>
/// <param name="glyph">Receives the glyph.</param>
/// <returns>Non-zero when found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_font_get_variation_glyph(nativeint font, uint32 unicode, uint32 selector, uint32& glyph)

/// <summary>Formats a glyph string, <c>hb_font_glyph_to_string</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="buffer">The output buffer.</param>
/// <param name="size">Its size.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_glyph_to_string(nativeint font, uint32 glyph, byte[] buffer, uint32 size)

/// <summary>Parses a glyph string, <c>hb_font_glyph_from_string</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="text">The UTF-8 bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="glyph">Receives the glyph.</param>
/// <returns>Non-zero when found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_font_glyph_from_string(nativeint font, byte[] text, int length, uint32& glyph)

/// <summary>Returns a blob's bytes, <c>hb_blob_get_data</c>.</summary>
/// <param name="blob">The blob handle.</param>
/// <param name="length">Receives the byte length.</param>
/// <returns>The data pointer, valid while the blob lives.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_blob_get_data(nativeint blob, uint32& length)

/// <summary>Spells a direction, <c>hb_direction_to_string</c>.</summary>
/// <param name="direction">The direction value.</param>
/// <returns>The static string pointer.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_direction_to_string(int direction)

/// <summary>Spells a language, <c>hb_language_to_string</c>.</summary>
/// <param name="language">The language handle.</param>
/// <returns>The static string pointer, or null.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_language_to_string(nativeint language)

/// <summary>Tests whether a language matches, <c>hb_language_matches</c>.</summary>
/// <param name="language">The broader language.</param>
/// <param name="specific">The specific language.</param>
/// <returns>Non-zero when they match.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_language_matches(nativeint language, nativeint specific)

/// <summary>Returns a script's horizontal direction, <c>hb_script_get_horizontal_direction</c>.</summary>
/// <param name="script">The script tag.</param>
/// <returns>The direction value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_script_get_horizontal_direction(uint32 script)

/// <summary>Converts an ISO 15924 tag, <c>hb_script_from_iso15924_tag</c>.</summary>
/// <param name="tag">The tag.</param>
/// <returns>The script tag.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_script_from_iso15924_tag(uint32 tag)

/// <summary>The default general category, <c>hb_unicode_general_category</c>.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The category value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_general_category(uint32 codepoint)

/// <summary>The default combining class, <c>hb_unicode_combining_class</c>.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The class.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_combining_class(uint32 codepoint)

/// <summary>The default mirroring, <c>hb_unicode_mirroring</c>.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The mirrored code point.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_mirroring(uint32 codepoint)

/// <summary>The default script, <c>hb_unicode_script</c>.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The script tag.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_script(uint32 codepoint)

/// <summary>The default east Asian width, <c>hb_unicode_east_asian_width</c>.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The width value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_east_asian_width(uint32 codepoint)

/// <summary>Composes a pair, <c>hb_unicode_compose</c>.</summary>
/// <param name="a">The first code point.</param>
/// <param name="b">The second code point.</param>
/// <param name="ab">Receives the composite.</param>
/// <returns>Non-zero when composed.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_compose(uint32 a, uint32 b, uint32& ab)

/// <summary>Decomposes a code point, <c>hb_unicode_decompose</c>.</summary>
/// <param name="ab">The code point.</param>
/// <param name="a">Receives the first part.</param>
/// <param name="b">Receives the second part.</param>
/// <returns>Non-zero when decomposed.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_decompose(uint32 ab, uint32& a, uint32& b)

/// <summary>The modified combining class, <c>hb_unicode_modified_combining_class</c>.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The class.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_modified_ccc(uint32 codepoint)

/// <summary>The default-ignorable predicate.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>Non-zero when ignorable.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_is_default_ignorable(uint32 codepoint)

/// <summary>The variation-selector predicate.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>Non-zero when a selector.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_is_variation_selector(uint32 codepoint)

/// <summary>The fallback space type.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The type value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_space_fallback(uint32 codepoint)

/// <summary>The vertical form.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The vertical code point.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_vertical_char(uint32 codepoint)

/// <summary>The packed unicode properties and scratch flags.</summary>
/// <param name="codepoint">The code point.</param>
/// <param name="scratch">Receives the scratch flags.</param>
/// <returns>The packed properties.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_unicode_props(uint32 codepoint, uint32& scratch)

/// <summary>Adds UTF-8 text to a buffer.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="text">The text.</param>
/// <param name="length">The text length.</param>
/// <param name="itemOffset">The item offset.</param>
/// <param name="itemLength">The item length.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_add_utf8(nativeint buffer, byte[] text, int length, int itemOffset, int itemLength)

/// <summary>Adds UTF-16 text to a buffer.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="text">The text.</param>
/// <param name="length">The text length.</param>
/// <param name="itemOffset">The item offset.</param>
/// <param name="itemLength">The item length.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_add_utf16(nativeint buffer, uint16[] text, int length, int itemOffset, int itemLength)

/// <summary>Adds UTF-32 text to a buffer.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="text">The text.</param>
/// <param name="length">The text length.</param>
/// <param name="itemOffset">The item offset.</param>
/// <param name="itemLength">The item length.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_add_utf32(nativeint buffer, uint32[] text, int length, int itemOffset, int itemLength)

/// <summary>Adds Latin-1 text to a buffer.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="text">The text.</param>
/// <param name="length">The text length.</param>
/// <param name="itemOffset">The item offset.</param>
/// <param name="itemLength">The item length.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_add_latin1(nativeint buffer, byte[] text, int length, int itemOffset, int itemLength)

/// <summary>Reads a buffer's codepoints, clusters, contexts and content type.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="codepoints">Receives the codepoints.</param>
/// <param name="clusters">Receives the clusters.</param>
/// <param name="before">Receives the pre-context.</param>
/// <param name="after">Receives the post-context.</param>
/// <param name="length">Receives the length.</param>
/// <param name="beforeLength">Receives the pre-context length.</param>
/// <param name="afterLength">Receives the post-context length.</param>
/// <param name="content">Receives the content type.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_state(nativeint buffer, uint32[] codepoints, uint32[] clusters, uint32[] before, uint32[] after, uint32& length, uint32& beforeLength, uint32& afterLength, uint32& content)

/// <summary>Seeds a buffer with infos, optional positions and contexts.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="codepoints">The glyph code points.</param>
/// <param name="clusters">The clusters.</param>
/// <param name="length">The length.</param>
/// <param name="positions">Four position fields per glyph.</param>
/// <param name="havePositions">Whether positions exist.</param>
/// <param name="before">The pre-context.</param>
/// <param name="beforeLength">Its length.</param>
/// <param name="after">The post-context.</param>
/// <param name="afterLength">Its length.</param>
/// <param name="content">The content type.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_seed(nativeint buffer, uint32[] codepoints, uint32[] clusters, uint32 length, int32[] positions, int havePositions, uint32[] before, uint32 beforeLength, uint32[] after, uint32 afterLength, uint32 content)

/// <summary>Whether a buffer has positions.</summary>
/// <param name="buffer">The buffer.</param>
/// <returns>Non-zero when positions exist.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_buffer_have_positions(nativeint buffer)

/// <summary>Reads five position fields per glyph.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="positions">Receives the fields.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_positions(nativeint buffer, int32[] positions)

/// <summary>The direction, <c>hb_buffer_get_direction</c>.</summary>
/// <param name="buffer">The buffer.</param>
/// <returns>The direction value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_buffer_get_direction(nativeint buffer)

/// <summary>The script, <c>hb_buffer_get_script</c>.</summary>
/// <param name="buffer">The buffer.</param>
/// <returns>The script tag.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_buffer_get_script(nativeint buffer)

/// <summary>The language, <c>hb_buffer_get_language</c>.</summary>
/// <param name="buffer">The buffer.</param>
/// <returns>The language handle, or zero.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_buffer_get_language(nativeint buffer)

/// <summary>Reverses the whole buffer, <c>hb_buffer_reverse</c>.</summary>
/// <param name="buffer">The buffer.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_reverse(nativeint buffer)

/// <summary>Reverses a range, <c>hb_buffer_reverse_range</c>.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_reverse_range(nativeint buffer, uint32 start, uint32 finish)

/// <summary>Reverses cluster groups, <c>hb_buffer_reverse_clusters</c>.</summary>
/// <param name="buffer">The buffer.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_reverse_clusters(nativeint buffer)

/// <summary>Appends part of a source buffer, <c>hb_buffer_append</c>.</summary>
/// <param name="buffer">The target.</param>
/// <param name="source">The source.</param>
/// <param name="start">The first source index.</param>
/// <param name="finish">The source end.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_append(nativeint buffer, nativeint source, uint32 start, uint32 finish)

/// <summary>The process default language, <c>hb_language_get_default</c>.</summary>
/// <returns>The language handle, or zero.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_language_get_default()

/// <summary>Executes the P3.S09 op script against a seeded buffer.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="initialMaxOps">The operation budget to pre-set, or 0 to leave the default.</param>
/// <param name="ops">The opcode and argument stream.</param>
/// <param name="count">The number of stream elements.</param>
/// <returns>1 when the buffer succeeded, 0 when it failed, or -(k+1) when the (k+1)-th op trips a precondition.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_buffer_run_script(nativeint buffer, int initialMaxOps, uint32[] ops, uint32 count)

/// <summary>Reads the glyph masks of a buffer.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="masks">Receives one mask per glyph.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_masks(nativeint buffer, uint32[] masks)

/// <summary>Converts a script and a language to OpenType tags, <c>hb_ot_tags_from_script_and_language</c>.</summary>
/// <param name="script">The script.</param>
/// <param name="language">The language handle, or zero.</param>
/// <param name="scriptCount">The script capacity on input, the number written on output.</param>
/// <param name="scriptTags">The script tag output array.</param>
/// <param name="languageCount">The language capacity on input, the number written on output.</param>
/// <param name="languageTags">The language tag output array.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_tags_from_script_and_language(uint32 script, nativeint language, uint32& scriptCount, uint32[] scriptTags, uint32& languageCount, uint32[] languageTags)

/// <summary>Converts a script tag to a script, <c>hb_ot_tag_to_script</c>.</summary>
/// <param name="tag">The tag.</param>
/// <returns>The script.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_tag_to_script(uint32 tag)

/// <summary>Converts a language tag to a language, <c>hb_ot_tag_to_language</c>.</summary>
/// <param name="tag">The tag.</param>
/// <returns>The language handle, or zero.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_ot_tag_to_language(uint32 tag)

/// <summary>Converts a script tag and a language tag to a script and a language, <c>hb_ot_tags_to_script_and_language</c>.</summary>
/// <param name="scriptTag">The script tag.</param>
/// <param name="languageTag">The language tag.</param>
/// <param name="script">Receives the script.</param>
/// <param name="language">Receives the language handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_tags_to_script_and_language(uint32 scriptTag, uint32 languageTag, uint32& script, nativeint& language)

/// <summary>The native <c>hb_ot_var_axis_info_t</c> layout.</summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type OracleVarAxisInfo =
    { AxisIndex: uint32
      Tag: uint32
      NameId: uint32
      Flags: uint32
      MinValue: float32
      DefaultValue: float32
      MaxValue: float32
      Reserved: uint32 }

/// <summary>Tests whether a face has variation data, <c>hb_ot_var_has_data</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when the face has an fvar table with data.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_var_has_data(nativeint face)

/// <summary>The number of variation axes, <c>hb_ot_var_get_axis_count</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The axis count.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_var_get_axis_count(nativeint face)

/// <summary>Fetches variation-axis infos, <c>hb_ot_var_get_axis_infos</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="startOffset">The first axis to fetch.</param>
/// <param name="axesCount">The capacity on input, the number written on output.</param>
/// <param name="axesArray">The output array.</param>
/// <returns>The total number of axes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_var_get_axis_infos(nativeint face, uint32 startOffset, uint32& axesCount, [<Out>] OracleVarAxisInfo[] axesArray)

/// <summary>Finds one axis by tag, <c>hb_ot_var_find_axis_info</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="axisTag">The axis tag.</param>
/// <param name="axisInfo">Receives the axis info.</param>
/// <returns>Non-zero when found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_var_find_axis_info(nativeint face, uint32 axisTag, OracleVarAxisInfo& axisInfo)

/// <summary>The number of named instances, <c>hb_ot_var_get_named_instance_count</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The instance count.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_var_get_named_instance_count(nativeint face)

/// <summary>A named instance's subfamily name id, <c>hb_ot_var_named_instance_get_subfamily_name_id</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="instanceIndex">The instance index.</param>
/// <returns>The name id, or <c>HB_OT_NAME_ID_INVALID</c>.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_var_named_instance_get_subfamily_name_id(nativeint face, uint32 instanceIndex)

/// <summary>A named instance's PostScript name id, <c>hb_ot_var_named_instance_get_postscript_name_id</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="instanceIndex">The instance index.</param>
/// <returns>The name id, or <c>HB_OT_NAME_ID_INVALID</c>.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_var_named_instance_get_postscript_name_id(nativeint face, uint32 instanceIndex)

/// <summary>A named instance's design coordinates, <c>hb_ot_var_named_instance_get_design_coords</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="instanceIndex">The instance index.</param>
/// <param name="coordsLength">The capacity on input, the number written on output.</param>
/// <param name="coords">The output array.</param>
/// <returns>The number of axes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_var_named_instance_get_design_coords(nativeint face, uint32 instanceIndex, uint32& coordsLength, float32[] coords)

/// <summary>Normalizes a variation list, <c>hb_ot_var_normalize_variations</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="variations">The variations.</param>
/// <param name="variationsLength">Their number.</param>
/// <param name="coords">The output normalized coordinates.</param>
/// <param name="coordsLength">The coordinate array length.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_var_normalize_variations(nativeint face, Farfbuzz.Variation[] variations, uint32 variationsLength, int[] coords, uint32 coordsLength)

/// <summary>Normalizes design coordinates, <c>hb_ot_var_normalize_coords</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coordsLength">The coordinate array length.</param>
/// <param name="designCoords">The design coordinates.</param>
/// <param name="normalizedCoords">The output normalized coordinates.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_var_normalize_coords(nativeint face, uint32 coordsLength, float32[] designCoords, int[] normalizedCoords)
/// <summary>The GDEF glyph class of a glyph, <c>hb_ot_layout_get_glyph_class</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The glyph class, zero when unclassified.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_get_glyph_class(nativeint face, uint32 glyph)

/// <summary>Whether a face has glyph classes, <c>hb_ot_layout_has_glyph_classes</c>.</summary>
/// <param name="face">The face.</param>
/// <returns>Non-zero when glyph classes are defined.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_has_glyph_classes(nativeint face)

/// <summary>Collects a face's glyphs of a class, <c>hb_ot_layout_get_glyphs_in_class</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="klass">The class.</param>
/// <param name="glyphs">The set receiving the glyphs.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_layout_get_glyphs_in_class(nativeint face, uint32 klass, nativeint glyphs)

/// <summary>Fetches a glyph's attach points, <c>hb_ot_layout_get_attach_points</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="startOffset">The first attach point to return.</param>
/// <param name="pointCount">The capacity on input, the number written on output.</param>
/// <param name="pointArray">The output array.</param>
/// <returns>The total number of attach points.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_get_attach_points(nativeint face, uint32 glyph, uint32 startOffset, uint32& pointCount, uint32[] pointArray)

/// <summary>Fetches a glyph's ligature carets, <c>hb_ot_layout_get_ligature_carets</c>.</summary>
/// <param name="font">The font.</param>
/// <param name="direction">The direction.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="startOffset">The first caret to return.</param>
/// <param name="caretCount">The capacity on input, the number written on output.</param>
/// <param name="caretArray">The output array.</param>
/// <returns>The total number of carets.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_get_ligature_carets(nativeint font, int direction, uint32 glyph, uint32 startOffset, uint32& caretCount, int[] caretArray)

/// <summary>The number of members of a set, <c>hb_set_get_population</c>.</summary>
/// <param name="set">The set.</param>
/// <returns>The population.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_set_get_population(nativeint set)

/// <summary>Lists a table's script tags, <c>hb_ot_layout_table_get_script_tags</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="startOffset">The first tag.</param>
/// <param name="scriptCount">The capacity on input, the number written on output.</param>
/// <param name="scriptTags">The output array.</param>
/// <returns>The total number of script tags.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_table_get_script_tags(nativeint face, uint32 tableTag, uint32 startOffset, uint32& scriptCount, uint32[] scriptTags)

/// <summary>Selects a script, <c>hb_ot_layout_table_select_script</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptCount">The number of candidate tags.</param>
/// <param name="scriptTags">The candidate tags.</param>
/// <param name="scriptIndex">Receives the selected index.</param>
/// <param name="chosenScript">Receives the chosen tag.</param>
/// <returns>Non-zero when a candidate matched.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_table_select_script(nativeint face, uint32 tableTag, uint32 scriptCount, uint32[] scriptTags, uint32& scriptIndex, uint32& chosenScript)

/// <summary>Lists a script's language-system tags, <c>hb_ot_layout_script_get_language_tags</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="startOffset">The first tag.</param>
/// <param name="languageCount">The capacity on input, the number written on output.</param>
/// <param name="languageTags">The output array.</param>
/// <returns>The total number of language-system tags.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_script_get_language_tags(nativeint face, uint32 tableTag, uint32 scriptIndex, uint32 startOffset, uint32& languageCount, uint32[] languageTags)

/// <summary>Selects a language system, <c>hb_ot_layout_script_select_language2</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageCount">The number of candidate tags.</param>
/// <param name="languageTags">The candidate tags.</param>
/// <param name="languageIndex">Receives the selected index.</param>
/// <param name="chosenLanguage">Receives the chosen tag.</param>
/// <returns>Non-zero when a candidate matched.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_script_select_language2(nativeint face, uint32 tableTag, uint32 scriptIndex, uint32 languageCount, uint32[] languageTags, uint32& languageIndex, uint32& chosenLanguage)

/// <summary>Fetches a language system's required feature, <c>hb_ot_layout_language_get_required_feature</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageIndex">The language index.</param>
/// <param name="featureIndex">Receives the feature index.</param>
/// <param name="featureTag">Receives the feature tag.</param>
/// <returns>Non-zero when a required feature exists.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_language_get_required_feature(nativeint face, uint32 tableTag, uint32 scriptIndex, uint32 languageIndex, uint32& featureIndex, uint32& featureTag)

/// <summary>Lists a language system's feature indices, <c>hb_ot_layout_language_get_feature_indexes</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageIndex">The language index.</param>
/// <param name="startOffset">The first index.</param>
/// <param name="featureCount">The capacity on input, the number written on output.</param>
/// <param name="featureIndexes">The output array.</param>
/// <returns>The total number of feature indices.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_language_get_feature_indexes(nativeint face, uint32 tableTag, uint32 scriptIndex, uint32 languageIndex, uint32 startOffset, uint32& featureCount, uint32[] featureIndexes)

/// <summary>Lists a language system's feature tags, <c>hb_ot_layout_language_get_feature_tags</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageIndex">The language index.</param>
/// <param name="startOffset">The first tag.</param>
/// <param name="featureCount">The capacity on input, the number written on output.</param>
/// <param name="featureTags">The output array.</param>
/// <returns>The total number of feature tags.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_language_get_feature_tags(nativeint face, uint32 tableTag, uint32 scriptIndex, uint32 languageIndex, uint32 startOffset, uint32& featureCount, uint32[] featureTags)

/// <summary>Lists a table's feature tags, <c>hb_ot_layout_table_get_feature_tags</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="startOffset">The first tag.</param>
/// <param name="featureCount">The capacity on input, the number written on output.</param>
/// <param name="featureTags">The output array.</param>
/// <returns>The total number of feature tags.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_table_get_feature_tags(nativeint face, uint32 tableTag, uint32 startOffset, uint32& featureCount, uint32[] featureTags)

/// <summary>Lists a feature's lookup indices, <c>hb_ot_layout_feature_get_lookups</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="featureIndex">The feature index.</param>
/// <param name="startOffset">The first lookup.</param>
/// <param name="lookupCount">The capacity on input, the number written on output.</param>
/// <param name="lookupIndexes">The output array.</param>
/// <returns>The total number of lookups.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_feature_get_lookups(nativeint face, uint32 tableTag, uint32 featureIndex, uint32 startOffset, uint32& lookupCount, uint32[] lookupIndexes)

/// <summary>The number of lookups, <c>hb_ot_layout_table_get_lookup_count</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="tableTag">The table tag.</param>
/// <returns>The lookup count.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_table_get_lookup_count(nativeint face, uint32 tableTag)

/// <summary>Whether the GSUB table has data, <c>hb_ot_layout_has_substitution</c>.</summary>
/// <param name="face">The face.</param>
/// <returns>Non-zero when GSUB is present.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_has_substitution(nativeint face)

/// <summary>Whether the GPOS table has data, <c>hb_ot_layout_has_positioning</c>.</summary>
/// <param name="face">The face.</param>
/// <returns>Non-zero when GPOS is present.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_has_positioning(nativeint face)

/// <summary>Sets one variation axis, <c>hb_font_set_variation</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="tag">The axis tag.</param>
/// <param name="value">The value.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_variation(nativeint font, uint32 tag, float32 value)

/// <summary>Sets design-space coordinates, <c>hb_font_set_var_coords_design</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="coords">The design coordinates.</param>
/// <param name="coordsLength">Their number.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_var_coords_design(nativeint font, float32[] coords, uint32 coordsLength)

/// <summary>Reads design-space coordinates, <c>hb_font_get_var_coords_design</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="length">Receives the number of coordinates.</param>
/// <returns>A pointer to the coordinate array, valid while the font is unchanged.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_font_get_var_coords_design(nativeint font, uint32& length)

/// <summary>Sets normalized coordinates, <c>hb_font_set_var_coords_normalized</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="coords">The normalized 2.14 coordinates.</param>
/// <param name="coordsLength">Their number.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_var_coords_normalized(nativeint font, int[] coords, uint32 coordsLength)

/// <summary>Reads normalized coordinates, <c>hb_font_get_var_coords_normalized</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="length">Receives the number of coordinates.</param>
/// <returns>A pointer to the coordinate array, valid while the font is unchanged.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_font_get_var_coords_normalized(nativeint font, uint32& length)

/// <summary>Sets the named instance, <c>hb_font_set_var_named_instance</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="instanceIndex">The named-instance index.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_set_var_named_instance(nativeint font, uint32 instanceIndex)

/// <summary>Reads the named instance, <c>hb_font_get_var_named_instance</c>.</summary>
/// <param name="font">The font handle.</param>
/// <returns>The named-instance index, or <c>HB_FONT_NO_VAR_NAMED_INSTANCE</c>.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_font_get_var_named_instance(nativeint font)

/// <summary>The native <c>hb_segment_properties_t</c> layout.</summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type NativeSegmentProperties =
    val mutable Direction: int
    val mutable Script: uint32
    val mutable Language: nativeint
    val mutable Reserved1: nativeint
    val mutable Reserved2: nativeint

/// <summary>Creates a shaping plan, <c>hb_shape_plan_create</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="props">The segment properties.</param>
/// <param name="features">The user features, or null.</param>
/// <param name="count">Their number.</param>
/// <param name="shapers">A null-terminated shaper list, or null for all shapers.</param>
/// <returns>The plan handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_shape_plan_create(nativeint face, NativeSegmentProperties& props, Farfbuzz.Feature[] features, uint32 count, [<MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)>] string[] shapers)

/// <summary>Releases a shaping plan, <c>hb_shape_plan_destroy</c>.</summary>
/// <param name="plan">The plan handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_shape_plan_destroy(nativeint plan)

/// <summary>Executes a shaping plan, <c>hb_shape_plan_execute</c>.</summary>
/// <param name="plan">The plan handle.</param>
/// <param name="font">The font handle.</param>
/// <param name="buffer">The buffer handle.</param>
/// <param name="features">The user features, or null.</param>
/// <param name="count">Their number.</param>
/// <returns>Non-zero on success.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_shape_plan_execute(nativeint plan, nativeint font, nativeint buffer, Farfbuzz.Feature[] features, uint32 count)

/// <summary>Collects a plan's lookups into a set, <c>hb_ot_shape_plan_collect_lookups</c>.</summary>
/// <param name="plan">The plan handle.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <param name="lookups">The set receiving the lookup indices.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_shape_plan_collect_lookups(nativeint plan, uint32 tableTag, nativeint lookups)

/// <summary>Reads a plan's feature tags, <c>hb_ot_shape_plan_get_feature_tags</c>.</summary>
/// <param name="plan">The plan handle.</param>
/// <param name="startOffset">The first feature.</param>
/// <param name="tagCount">The capacity on input, the number written on output.</param>
/// <param name="tags">The output array.</param>
/// <returns>The total number of feature tags.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_shape_plan_get_feature_tags(nativeint plan, uint32 startOffset, uint32& tagCount, uint32[] tags)

/// <summary>Reads nominal glyphs in one call, <c>hb_font_get_nominal_glyphs</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="count">The number of code points.</param>
/// <param name="firstUnicode">The code point array.</param>
/// <param name="unicodeStride">The byte stride between code points.</param>
/// <param name="firstGlyph">The glyph array.</param>
/// <param name="glyphStride">The byte stride between glyphs.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_get_nominal_glyphs(nativeint font, uint32 count, uint32[] firstUnicode, uint32 unicodeStride, uint32[] firstGlyph, uint32 glyphStride)

/// <summary>Reads vertical origins in one call, <c>hb_font_get_glyph_v_origins</c>.</summary>
/// <param name="font">The font handle.</param>
/// <param name="count">The number of glyphs.</param>
/// <param name="firstGlyph">The glyph array.</param>
/// <param name="glyphStride">The byte stride between glyphs.</param>
/// <param name="firstX">The horizontal origin array.</param>
/// <param name="xStride">The byte stride between x origins.</param>
/// <param name="firstY">The vertical origin array.</param>
/// <param name="yStride">The byte stride between y origins.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_font_get_glyph_v_origins(nativeint font, uint32 count, uint32[] firstGlyph, uint32 glyphStride, int[] firstX, uint32 xStride, int[] firstY, uint32 yStride)

/// <summary>Finds the feature-variations index at coordinates, <c>hb_ot_layout_table_find_feature_variations</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="variationsIndex">Receives the record index, or <c>HB_OT_LAYOUT_NO_VARIATIONS_INDEX</c>.</param>
/// <returns>Non-zero when a record matched.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_table_find_feature_variations(nativeint face, uint32 tableTag, int[] coords, uint32 numCoords, uint32& variationsIndex)

/// <summary>Fetches a feature's lookups at a variations index, <c>hb_ot_layout_feature_with_variations_get_lookups</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <param name="featureIndex">The feature index.</param>
/// <param name="variationsIndex">The variations index, or <c>HB_OT_LAYOUT_NO_VARIATIONS_INDEX</c>.</param>
/// <param name="startOffset">The first lookup to return.</param>
/// <param name="lookupCount">The capacity on input, the number written on output.</param>
/// <param name="lookupIndexes">The output array.</param>
/// <returns>The total number of lookups.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_feature_with_variations_get_lookups(nativeint face, uint32 tableTag, uint32 featureIndex, uint32 variationsIndex, uint32 startOffset, uint32& lookupCount, uint32[] lookupIndexes)

/// <summary>Creates a shaping plan at variation coordinates, <c>hb_shape_plan_create2</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="props">The segment properties.</param>
/// <param name="features">The user features, or null.</param>
/// <param name="count">Their number.</param>
/// <param name="coords">The normalized variation coordinates, or null.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="shapers">A null-terminated shaper list, or null for all shapers.</param>
/// <returns>The plan handle.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint hb_shape_plan_create2(nativeint face, NativeSegmentProperties& props, Farfbuzz.Feature[] features, uint32 count, int[] coords, uint32 numCoords, [<MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)>] string[] shapers)

/// <summary>Whether the trak table has data, <c>hb_aat_layout_has_tracking</c>.</summary>
/// <param name="face">The face.</param>
/// <returns>Non-zero when trak is present.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_aat_layout_has_tracking(nativeint face)

/// <summary>Whether a face includes AAT substitutions, <c>hb_aat_layout_has_substitution</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when morx or mort has data.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_aat_layout_has_substitution(nativeint face)

/// <summary>Whether a face includes AAT positioning, <c>hb_aat_layout_has_positioning</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when kerx has data.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_aat_layout_has_positioning(nativeint face)

/// <summary>Whether a lookup would substitute a glyph sequence, <c>hb_ot_layout_lookup_would_substitute</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="lookupIndex">The lookup index.</param>
/// <param name="glyphs">The glyph sequence.</param>
/// <param name="glyphsLength">Its length.</param>
/// <param name="zeroContext">Non-zero when pre- and post-context are disallowed.</param>
/// <returns>Non-zero when the lookup would substitute the sequence.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_layout_lookup_would_substitute(nativeint face, uint32 lookupIndex, uint32[] glyphs, uint32 glyphsLength, int zeroContext)

/// <summary>Fetches a glyph's alternates from a lookup, <c>hb_ot_layout_lookup_get_glyph_alternates</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="lookupIndex">The lookup index.</param>
/// <param name="glyph">The glyph id.</param>
/// <param name="startOffset">The first alternate to return.</param>
/// <param name="alternateCount">The capacity on input, the number written on output.</param>
/// <param name="alternateGlyphs">The output array.</param>
/// <returns>The total number of alternates.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_layout_lookup_get_glyph_alternates(nativeint face, uint32 lookupIndex, uint32 glyph, uint32 startOffset, uint32& alternateCount, uint32[] alternateGlyphs)

/// <summary>Counts and fetches the feature types of the feat table, <c>hb_aat_layout_get_feature_types</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="startOffset">The first feature type to return.</param>
/// <param name="featureCount">The capacity on input, the number written on output.</param>
/// <param name="features">The output array, or null.</param>
/// <returns>The total number of feature types.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_aat_layout_get_feature_types(nativeint face, uint32 startOffset, uint32& featureCount, uint32[] features)

/// <summary>The name id of a feature type, <c>hb_aat_layout_feature_type_get_name_id</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="featureType">The feature type.</param>
/// <returns>The name id.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_aat_layout_feature_type_get_name_id(nativeint face, uint32 featureType)

/// <summary>Counts and fetches the selectors of a feature type, <c>hb_aat_layout_feature_type_get_selector_infos</c>;
/// each selector is four 32-bit words: name id, enable, disable, reserved.</summary>
/// <param name="face">The face handle.</param>
/// <param name="featureType">The feature type.</param>
/// <param name="startOffset">The first selector to return.</param>
/// <param name="selectorCount">The capacity on input, the number written on output.</param>
/// <param name="selectors">The output words, four per selector, or null.</param>
/// <param name="defaultIndex">The default selector index.</param>
/// <returns>The total number of selectors.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_aat_layout_feature_type_get_selector_infos(nativeint face, uint32 featureType, uint32 startOffset, uint32& selectorCount, uint32[] selectors, uint32& defaultIndex)
/// <summary>The native callback of <c>hb_buffer_message_func_t</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="font">The font handle.</param>
/// <param name="message">The message text pointer.</param>
/// <param name="userData">The data passed to the callback.</param>
/// <returns>Non-zero to continue shaping.</returns>
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type BufferMessageFunc = delegate of buffer: nativeint * font: nativeint * message: nativeint * userData: nativeint -> int

/// <summary>Sets a buffer's message callback, <c>hb_buffer_set_message_func</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="func">The callback, or null to remove it.</param>
/// <param name="userData">The data passed to the callback.</param>
/// <param name="destroy">The data destroy callback, or zero.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_set_message_func(nativeint buffer, BufferMessageFunc func, nativeint userData, nativeint destroy)

/// <summary>Collects the glyphs of a lookup into four sets, <c>hb_ot_layout_lookup_collect_glyphs</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <param name="lookupIndex">The lookup index.</param>
/// <param name="glyphsBefore">The before set, or zero.</param>
/// <param name="glyphsInput">The input set, or zero.</param>
/// <param name="glyphsAfter">The after set, or zero.</param>
/// <param name="glyphsOutput">The output set, or zero.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_layout_lookup_collect_glyphs(nativeint face, uint32 tableTag, uint32 lookupIndex, nativeint glyphsBefore, nativeint glyphsInput, nativeint glyphsAfter, nativeint glyphsOutput)

/// <summary>Collects the lookup indices under scripts, languages and features,
/// <c>hb_ot_layout_collect_lookups</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag, GSUB or GPOS.</param>
/// <param name="scripts">The zero-terminated script tag array, or null for all.</param>
/// <param name="languages">The zero-terminated language tag array, or null for all.</param>
/// <param name="features">The zero-terminated feature tag array, or null for all.</param>
/// <param name="lookupIndexes">The set receiving the lookup indices.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_ot_layout_collect_lookups(nativeint face, uint32 tableTag, uint32[] scripts, uint32[] languages, uint32[] features, nativeint lookupIndexes)
/// <summary>Clears a buffer's contents, keeping its flags, <c>hb_buffer_clear_contents</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_clear_contents(nativeint buffer)

/// <summary>Appends UTF-8 text, <c>hb_buffer_add_utf8</c>.</summary>
/// <param name="buffer">The buffer handle.</param>
/// <param name="text">The text pointer; the first <paramref name="textLength"/> bytes.</param>
/// <param name="textLength">The length of <paramref name="text"/> in bytes.</param>
/// <param name="itemOffset">The first byte of the item; bytes before it are pre-context.</param>
/// <param name="itemLength">The item's length in bytes; bytes after it are post-context.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void hb_buffer_add_utf8(nativeint buffer, nativeint text, int textLength, uint32 itemOffset, int itemLength)

/// <summary>Whether a face has palettes, <c>hb_ot_color_has_palettes</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when it has.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_color_has_palettes(nativeint face)

/// <summary>The number of palettes, <c>hb_ot_color_palette_get_count</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The count.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_color_palette_get_count(nativeint face)

/// <summary>The name id of a palette, <c>hb_ot_color_palette_get_name_id</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="palette">The palette index.</param>
/// <returns>The name id.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_color_palette_get_name_id(nativeint face, uint32 palette)

/// <summary>The name id of a colour entry, <c>hb_ot_color_palette_color_get_name_id</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="color">The colour index.</param>
/// <returns>The name id.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_color_palette_color_get_name_id(nativeint face, uint32 color)

/// <summary>The flags of a palette, <c>hb_ot_color_palette_get_flags</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="palette">The palette index.</param>
/// <returns>The flags.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_color_palette_get_flags(nativeint face, uint32 palette)

/// <summary>The colours of a palette, <c>hb_ot_color_palette_get_colors</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="palette">The palette index.</param>
/// <param name="start">The first colour.</param>
/// <param name="count">The room in, the colours written out.</param>
/// <param name="colors">The room.</param>
/// <returns>The number of colours in a palette.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 hb_ot_color_palette_get_colors(nativeint face, uint32 palette, uint32 start, uint32& count, uint32[] colors)

/// <summary>Whether a face has colour layers, <c>hb_ot_color_has_layers</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when it has.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_color_has_layers(nativeint face)

/// <summary>Whether a face has PNG images, <c>hb_ot_color_has_png</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when it has.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_color_has_png(nativeint face)

/// <summary>Whether a face has SVG documents, <c>hb_ot_color_has_svg</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when it has.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_ot_color_has_svg(nativeint face)
