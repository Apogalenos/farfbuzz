module Farfbuzz.Oracle.Shims

open System.Runtime.InteropServices

/// <summary>The version of the shim set compiled into the oracle.</summary>
/// <returns>The shim-set version.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_shims_version()

/// <summary>Indic category (low byte) and position (high byte) of a code point.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The packed categories.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_indic_categories(uint32 codepoint)

/// <summary>Universal Shaping Engine category of a code point.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The category.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_use_category(uint32 codepoint)

/// <summary>Arabic joining type of a code point, after resolving the general-category default.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The joining type.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_arabic_joining_type(uint32 codepoint)

/// <summary>Whether a code point is Extended_Pictographic.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>Non-zero when it is.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_is_extended_pictographic(uint32 codepoint)

/// <summary>Whether HarfBuzz treats a code point as default-ignorable.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>Non-zero when it is.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_is_default_ignorable(uint32 codepoint)

/// <summary>Looks up a contiguous code-point range and writes 0xFFFFFFFF for misses.</summary>
/// <param name="font">The font handle.</param>
/// <param name="first">The first code point.</param>
/// <param name="count">The number of code points.</param>
/// <param name="glyphs">Receives one glyph per code point.</param>
/// <returns>The number of code points found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_nominal_glyph_range(nativeint font, uint32 first, uint32 count, uint32[] glyphs)

/// <summary>The unscaled horizontal advance delta of a glyph through HVAR.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The delta.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern float32 farfbuzz_oracle_hvar_delta(nativeint face, int[] coords, uint32 numCoords, uint32 glyph)

/// <summary>The unscaled vertical advance delta of a glyph through VVAR.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The delta.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern float32 farfbuzz_oracle_vvar_delta(nativeint face, int[] coords, uint32 numCoords, uint32 glyph)

/// <summary>The unscaled vertical-origin delta of a glyph through VVAR.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The delta.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern float32 farfbuzz_oracle_vorg_delta(nativeint face, int[] coords, uint32 numCoords, uint32 glyph)

/// <summary>The unscaled metric delta of a tag through MVAR.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="tag">The value-record tag.</param>
/// <returns>The delta.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern float32 farfbuzz_oracle_mvar_delta(nativeint face, int[] coords, uint32 numCoords, uint32 tag)

/// <summary>The unscaled item-variation delta of a 16.16 outer.inner index through the GDEF var store.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
/// <returns>The delta.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern float32 farfbuzz_oracle_gdef_delta(nativeint face, int[] coords, uint32 numCoords, uint32 varIdx)

/// <summary>The double item-variation delta of a 16.16 outer.inner index through the GDEF var store.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
/// <returns>The delta.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern float farfbuzz_oracle_gdef_delta_double(nativeint face, int[] coords, uint32 numCoords, uint32 varIdx)

/// <summary>Sanitizes an AAT lookup and reports its verdict.</summary>
/// <param name="data">The lookup bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="valueSize">The value width, 1, 2 or 4.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>Non-zero when the lookup sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_aat_lookup_sanitize(byte[] data, uint32 length, uint32 valueSize, uint32 numGlyphs)

/// <summary>Reads a glyph's lookup value, reporting whether it was found.</summary>
/// <param name="data">The lookup bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="found">Receives non-zero when the value exists.</param>
/// <returns>The value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_lookup_get_value(byte[] data, uint32 length, uint32 valueSize, uint32 numGlyphs, uint32 glyph, uint32& found)

/// <summary>Reads a glyph's lookup value, or Null.</summary>
/// <param name="data">The lookup bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The value.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_lookup_get_value_or_null(byte[] data, uint32 length, uint32 valueSize, uint32 numGlyphs, uint32 glyph)

/// <summary>Reads a glyph's class, or the out-of-range class.</summary>
/// <param name="data">The lookup bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="outOfRange">The out-of-range class.</param>
/// <returns>The class.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_lookup_get_class(byte[] data, uint32 length, uint32 valueSize, uint32 numGlyphs, uint32 glyph, uint32 outOfRange)

/// <summary>Collects the glyphs a lookup covers, as interleaved ranges.</summary>
/// <param name="data">The lookup bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="out">Receives first and last glyph pairs.</param>
/// <param name="capacity">The number of ranges that fit.</param>
/// <returns>The number of ranges.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_lookup_collect_glyphs(byte[] data, uint32 length, uint32 valueSize, uint32 numGlyphs, uint32[] out, uint32 capacity)

/// <summary>Reads a glyph's class from an obsolete class table.</summary>
/// <param name="data">The class-table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="wide">Non-zero for a wide (16-bit) class table.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="outOfRange">The out-of-range class.</param>
/// <returns>The class.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_class_table_get_class(byte[] data, uint32 length, uint32 wide, uint32 glyph, uint32 outOfRange)

/// <summary>Collects the glyphs an obsolete class table covers, as interleaved ranges.</summary>
/// <param name="data">The class-table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="wide">Non-zero for a wide (16-bit) class table.</param>
/// <param name="out">Receives first and last glyph pairs.</param>
/// <param name="capacity">The number of ranges that fit.</param>
/// <returns>The number of ranges.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_class_table_collect_glyphs(byte[] data, uint32 length, uint32 wide, uint32[] out, uint32 capacity)

/// <summary>Sanitizes an obsolete class table and reports its verdict.</summary>
/// <param name="data">The class-table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="wide">Non-zero for a wide (16-bit) class table.</param>
/// <returns>Non-zero when the class table sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_aat_class_table_sanitize(byte[] data, uint32 length, uint32 wide)
/// <summary>Builds an hb_set_digest_t from an add/add_range op stream and answers may_have.</summary>
/// <param name="ops">The op stream: per op, kind (0 add, 1 add_range), a, b.</param>
/// <param name="opCount">The number of ops.</param>
/// <param name="query">The glyph to test.</param>
/// <returns>Non-zero when the digest may contain the glyph.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_set_digest_may_have(uint32[] ops, uint32 opCount, uint32 query)

/// <summary>The GDEF glyph props of a glyph, through the GDEF accelerator.</summary>
/// <param name="face">The face handle.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The packed glyph props.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_gdef_glyph_props(nativeint face, uint32 glyph)

/// <summary>Whether a mark set covers a glyph; an out-of-range set index is not covered.</summary>
/// <param name="face">The face handle.</param>
/// <param name="set">The mark set index.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Non-zero when covered.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_gdef_mark_set_covers(nativeint face, uint32 set, uint32 glyph)

/// <summary>Finds a feature's index at table level, <c>hb_ot_layout_table_find_feature</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="featureTag">The feature tag.</param>
/// <param name="featureIndex">Receives the feature index.</param>
/// <returns>Non-zero when found.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_table_find_feature(nativeint face, uint32 tableTag, uint32 featureTag, uint32& featureIndex)

/// <summary>Sanitizes a Coverage's bytes, then reports get_coverage, the iteration and the digest.</summary>
/// <param name="bytes">The coverage bytes.</param>
/// <param name="length">The byte length.</param>
/// <param name="glyphs">The sample glyphs.</param>
/// <param name="glyphCount">The sample count.</param>
/// <param name="sanitized">Receives 1 when the table sanitizes.</param>
/// <param name="coverages">Receives get_coverage per sample glyph, or -1 when not sanitized.</param>
/// <param name="iterGlyphs">Receives the iter_t glyphs, up to its capacity.</param>
/// <param name="iterCapacity">The iteration capacity.</param>
/// <param name="iterCount">Receives the iteration length.</param>
/// <param name="mayHave">Receives may_have of the collect_coverage digest per sample glyph.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_coverage(byte[] bytes, uint32 length, uint32[] glyphs, uint32 glyphCount, uint32& sanitized, int32[] coverages, uint32[] iterGlyphs, uint32 iterCapacity, uint32& iterCount, byte[] mayHave)

/// <summary>Sanitizes a ClassDef's bytes, then reports get_class and collect_class as set ranges.</summary>
/// <param name="bytes">The class-definition bytes.</param>
/// <param name="length">The byte length.</param>
/// <param name="glyphs">The sample glyphs.</param>
/// <param name="glyphCount">The sample count.</param>
/// <param name="collectClasses">The classes to collect.</param>
/// <param name="collectCount">The number of classes.</param>
/// <param name="sanitized">Receives 1 when the table sanitizes.</param>
/// <param name="classes">Receives get_class per sample glyph.</param>
/// <param name="rangeFirsts">Receives the first glyph of each collected range.</param>
/// <param name="rangeLasts">Receives the last glyph of each collected range.</param>
/// <param name="rangeCapacity">The range capacity.</param>
/// <param name="rangeCounts">Receives the number of ranges per collected class.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_classdef(byte[] bytes, uint32 length, uint32[] glyphs, uint32 glyphCount, uint32[] collectClasses, uint32 collectCount, uint32& sanitized, uint32[] classes, uint32[] rangeFirsts, uint32[] rangeLasts, uint32 rangeCapacity, uint32[] rangeCounts)

/// <summary>Sanitizes an AAT state table and reports its verdict, entry count and state bounds.</summary>
/// <param name="data">The state-table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="extended">Non-zero for the extended (32-bit) form.</param>
/// <param name="entryDataWords">The number of 16-bit data words per entry.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="numEntries">Receives the derived entry count.</param>
/// <param name="minState">Receives the smallest reachable state.</param>
/// <param name="maxState">Receives the largest reachable state.</param>
/// <returns>Non-zero when the table sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_aat_state_table_sanitize(byte[] data, uint32 length, uint32 extended, uint32 entryDataWords, uint32 numGlyphs, uint32& numEntries, int& minState, int& maxState)

/// <summary>Reads a glyph's class from an AAT state table.</summary>
/// <param name="data">The state-table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="extended">Non-zero for the extended (32-bit) form.</param>
/// <param name="entryDataWords">The number of 16-bit data words per entry.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The class.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_state_table_get_class(byte[] data, uint32 length, uint32 extended, uint32 entryDataWords, uint32 numGlyphs, uint32 glyph)

/// <summary>Reads the entry of a state and class, reporting the decoded fields.</summary>
/// <param name="data">The state-table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="extended">Non-zero for the extended (32-bit) form.</param>
/// <param name="entryDataWords">The number of 16-bit data words per entry.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="state">The state.</param>
/// <param name="klass">The class.</param>
/// <param name="newState">Receives the decoded new state.</param>
/// <param name="flags">Receives the entry flags.</param>
/// <param name="data0">Receives the first data word.</param>
/// <param name="data1">Receives the second data word.</param>
/// <returns>Non-zero when the entry was read.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_aat_state_table_get_entry(byte[] data, uint32 length, uint32 extended, uint32 entryDataWords, uint32 numGlyphs, int state, uint32 klass, int& newState, uint32& flags, uint32& data0, uint32& data1)
/// <summary>Raw Arabic joining type from the table, before the general-category resolution.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The joining type.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_arabic_joining_type_raw(uint32 codepoint)

/// <summary>Arabic presentation form of a code point and form.</summary>
/// <param name="codepoint">The code point.</param>
/// <param name="form">The form, 0 initial, 1 medial, 2 final, 3 isolated.</param>
/// <returns>The presentation form, or 0 outside the shaping range.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_arabic_shaping(uint32 codepoint, uint32 form)

/// <summary>Serialises one Arabic ligature table into a flat uint16 array.</summary>
/// <param name="which">0 = three-component, 1 = two-component, 2 = mark two-component.</param>
/// <param name="out">Receives first, then per entry the components and the ligature.</param>
/// <param name="capacity">The output capacity.</param>
/// <returns>The number of uint16 written.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_arabic_ligatures(uint32 which, uint16[] out, uint32 capacity)

/// <summary>Whether a script has data in the Arabic joining table.</summary>
/// <param name="script">The script tag.</param>
/// <returns>Non-zero when the script has Arabic joining data.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_has_arabic_joining(uint32 script)

/// <summary>Copies the Windows-1256 GSUB lookup table bytes.</summary>
/// <param name="out">Receives the table bytes when its capacity allows.</param>
/// <param name="capacity">The output capacity.</param>
/// <returns>The table byte size.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_arabic_win1256(byte[] out, uint32 capacity)

/// <summary>All points of a glyph after <c>Glyph::get_points</c>, phantoms last.</summary>
/// <param name="font">The font handle.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="phantomOnly">Non-zero to load only the phantom points.</param>
/// <param name="xy">Receives interleaved x and y, two floats per point.</param>
/// <param name="flags">Receives one packed flag byte per point: the raw flag, with 0x80 when the point ends a contour.</param>
/// <param name="capacity">The maximum number of points to write.</param>
/// <returns>The point count, or 0xFFFFFFFF when the glyph fails to load.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_glyf_points(nativeint font, uint32 gid, uint32 phantomOnly, float32[] xy, byte[] flags, uint32 capacity)
/// <summary>Runs the vowel-constraint preprocess on a fresh LTR buffer and copies its output.</summary>
/// <param name="script">The script tag.</param>
/// <param name="bufferFlags">The buffer flags.</param>
/// <param name="text">The input code points.</param>
/// <param name="len">The input length.</param>
/// <param name="outCodepoints">Receives the output code points.</param>
/// <param name="outClusters">Receives the output clusters.</param>
/// <param name="capacity">The output capacity.</param>
/// <returns>The output length.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_vowel_constraints(uint32 script, uint32 bufferFlags, uint32[] text, uint32 len, uint32[] outCodepoints, uint32[] outClusters, uint32 capacity)

/// <summary>Decompiles a packed point stream, reporting the verdict, the points and the bytes consumed.</summary>
/// <param name="data">The stream bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="out">Receives the decoded point indices.</param>
/// <param name="capacity">The number of points that fit.</param>
/// <param name="outCount">Receives the decoded point count.</param>
/// <param name="consumed">Receives the number of bytes consumed.</param>
/// <returns>Non-zero when the stream decompiles.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_decompile_points(byte[] data, uint32 length, uint32[] out, uint32 capacity, uint32& outCount, uint32& consumed)

/// <summary>Decompiles a packed delta stream, reporting the verdict, the deltas and the bytes consumed.</summary>
/// <param name="data">The stream bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="count">The number of deltas.</param>
/// <param name="start">The number of leading deltas left at zero.</param>
/// <param name="out">Receives the decoded deltas.</param>
/// <param name="capacity">The number of deltas that fit.</param>
/// <param name="consumed">Receives the number of bytes consumed.</param>
/// <returns>Non-zero when the stream decompiles.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_decompile_deltas(byte[] data, uint32 length, uint32 count, uint32 start, int[] out, uint32 capacity, uint32& consumed)
/// <summary>Runs one of the four Ragel syllable finders over a category run and copies the syllable bytes.</summary>
/// <param name="shaper">0 indic, 1 khmer, 2 myanmar, 3 use.</param>
/// <param name="categories">The per-glyph category bytes.</param>
/// <param name="isUnicodeMark">The per-glyph mark flags, read by the USE ZWNJ filter only; may be null.</param>
/// <param name="len">The input length.</param>
/// <param name="outSyllables">Receives one syllable byte per glyph.</param>
/// <returns>1 when a broken syllable was raised, 0 when not, -1 on bad input.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_find_syllables(uint32 shaper, byte[] categories, byte[] isUnicodeMark, uint32 len, byte[] outSyllables)

/// <summary>Runs the syllabic dotted-circle insertion on a buffer whose glyphs carry the given clusters, syllables and categories.</summary>
/// <param name="font">The font handle.</param>
/// <param name="bufferFlags">The buffer flags.</param>
/// <param name="glyphs">The input glyphs.</param>
/// <param name="clusters">The input clusters.</param>
/// <param name="syllables">The per-glyph syllable bytes.</param>
/// <param name="categories">The per-glyph category bytes.</param>
/// <param name="len">The input length.</param>
/// <param name="brokenSyllableType">The broken syllable type.</param>
/// <param name="dottedcircleCategory">The dotted-circle category.</param>
/// <param name="rephaCategory">The repha category to skip, or -1.</param>
/// <param name="dottedcirclePosition">The dotted-circle position, or -1.</param>
/// <param name="outGlyphs">Receives the output glyphs.</param>
/// <param name="outClusters">Receives the output clusters.</param>
/// <param name="outSyllables">Receives the output syllable bytes.</param>
/// <param name="outCategories">Receives the output category bytes.</param>
/// <param name="outAuxiliary">Receives the output auxiliary bytes.</param>
/// <param name="capacity">The output capacity.</param>
/// <returns>The output length, or 0xFFFFFFFF on bad input.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_insert_dotted_circles(nativeint font, uint32 bufferFlags, uint32[] glyphs, uint32[] clusters, byte[] syllables, byte[] categories, uint32 len, uint32 brokenSyllableType, uint32 dottedcircleCategory, int rephaCategory, int dottedcirclePosition, uint32[] outGlyphs, uint32[] outClusters, byte[] outSyllables, byte[] outCategories, byte[] outAuxiliary, uint32 capacity)

/// <summary>Reads a lookup accelerator's digest and digest-second masks.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The GSUB or GPOS table tag.</param>
/// <param name="lookup">The lookup index.</param>
/// <param name="out">Receives the three digest masks followed by the three digest-second masks.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_lookup_digest(nativeint face, uint32 tableTag, uint32 lookup, uint64[] out)
/// <summary>Lookup indices a feature contributes at coordinates, as <c>hb_ot_map_builder_t::add_lookups</c> collects them.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
/// <param name="featureIndex">The feature index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="out">Receives the lookups in ascending order.</param>
/// <param name="capacity">The number of lookups that fit.</param>
/// <returns>The total number of lookups.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_feature_lookups(nativeint face, uint32 tableIndex, uint32 featureIndex, int[] coords, uint32 numCoords, uint32[] out, uint32 capacity)

/// <summary>The x and y variation deltas of a 16.16 outer.inner index through the face's GDEF var store, <c>Device::get_x_delta</c>/<c>get_y_delta</c>.</summary>
/// <param name="face">The face handle.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="varIdx">The 16.16 outer.inner delta-set index.</param>
/// <param name="xScale">The horizontal scale.</param>
/// <param name="yScale">The vertical scale.</param>
/// <param name="xDelta">Receives the horizontal delta.</param>
/// <param name="yDelta">Receives the vertical delta.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_device_delta(nativeint face, int[] coords, uint32 numCoords, uint32 varIdx, int xScale, int yScale, int& xDelta, int& yDelta)
/// <summary>Resolves an OpenType feature tag through the AAT mapping table.</summary>
/// <param name="tag">The feature tag.</param>
/// <param name="featureType">Receives the AAT feature type.</param>
/// <param name="selectorToEnable">Receives the enabling selector.</param>
/// <param name="selectorToDisable">Receives the disabling selector.</param>
/// <returns>Non-zero when the tag maps.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_aat_find_feature_mapping(uint32 tag, uint32& featureType, uint32& selectorToEnable, uint32& selectorToDisable)

/// <summary>Compiles the AAT map for a face, segment properties and user features, and serializes its chain flags.</summary>
/// <param name="face">The face handle.</param>
/// <param name="direction">The direction value.</param>
/// <param name="script">The script tag.</param>
/// <param name="language">The language handle, or zero.</param>
/// <param name="features">The user features.</param>
/// <param name="numFeatures">Their number.</param>
/// <param name="out">Receives the serialized chain flags, or an empty array to query the size.</param>
/// <param name="capacity">The output capacity.</param>
/// <returns>The total number of uint32 entries.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_aat_map_compile(nativeint face, uint32 direction, uint32 script, nativeint language, Farfbuzz.Feature[] features, uint32 numFeatures, uint32[] out, uint32 capacity)

/// <summary>Creates (or returns the cached) shape plan for a face and inputs; the returned pointer identifies the plan.</summary>
/// <param name="face">The face handle.</param>
/// <param name="direction">The direction value.</param>
/// <param name="script">The script tag.</param>
/// <param name="language">The language handle, or zero.</param>
/// <param name="features">The user features.</param>
/// <param name="numFeatures">Their number.</param>
/// <param name="coords">The variation coordinates.</param>
/// <param name="numCoords">Their number.</param>
/// <param name="shaperList">The shaper list, ending with a null element.</param>
/// <returns>The plan pointer, valid while the face lives.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint farfbuzz_oracle_shape_plan_create_cached2(nativeint face, uint32 direction, uint32 script, nativeint language, Farfbuzz.Feature[] features, uint32 numFeatures, int[] coords, uint32 numCoords, [<MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)>] string[] shaperList)

/// <summary>The device-space tracking advance <c>hb_aat_layout_track</c> adds to the first glyph of every
/// grapheme: the trak table's horizontal or vertical tracking at scale = upem and the given point size.</summary>
/// <param name="face">The face handle.</param>
/// <param name="horizontal">Non-zero for the horizontal TrackData, zero for the vertical one.</param>
/// <param name="ptem">The point size; the table defaults a non-positive value to 12.</param>
/// <returns>The tracking advance in device units.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_trak_tracking(nativeint face, uint32 horizontal, float32 ptem)

/// <summary>Whether a face includes kerning data, <c>hb_ot_layout_has_kerning</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when the kern table has data.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_has_kerning(nativeint face)

/// <summary>Whether a face includes state-machine kerning, <c>hb_ot_layout_has_machine_kerning</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when the kern table has a format 1 subtable.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_has_machine_kerning(nativeint face)

/// <summary>Whether a face includes cross-stream kerning, <c>hb_ot_layout_has_cross_kerning</c>.</summary>
/// <param name="face">The face handle.</param>
/// <returns>Non-zero when the kern table has a cross-stream subtable.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_has_cross_kerning(nativeint face)

/// <summary>Sanitizes a morx or mort table from its raw bytes, <c>AAT::morx::sanitize</c>/<c>mort</c>.</summary>
/// <param name="data">The table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="isMort">Non-zero for the mort table.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>Non-zero when the table sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_morx_sanitize(byte[] data, uint32 length, uint32 isMort, uint32 numGlyphs)

/// <summary>Sanitizes a kerx table from its raw bytes, <c>AAT::kerx::sanitize</c>.</summary>
/// <param name="data">The table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>Non-zero when the table sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_kerx_sanitize(byte[] data, uint32 length, uint32 numGlyphs)

/// <summary>Sanitizes a kern table from its raw bytes, <c>OT::kern::sanitize</c>.</summary>
/// <param name="data">The table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>Non-zero when the table sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_kern_sanitize(byte[] data, uint32 length, uint32 numGlyphs)

/// <summary>Sanitizes an ankr table from its raw bytes, <c>AAT::ankr::sanitize</c>.</summary>
/// <param name="data">The table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>Non-zero when the table sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_ankr_sanitize(byte[] data, uint32 length, uint32 numGlyphs)

/// <summary>The anchor an ankr table gives a glyph, <c>ankr::get_anchor</c>, the Null table when the bytes do not sanitize;
/// x in the high 16 bits and y in the low 16.</summary>
/// <param name="data">The table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="index">The anchor index.</param>
/// <returns>The packed coordinates.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_ankr_anchor(byte[] data, uint32 length, uint32 numGlyphs, uint32 glyph, uint32 index)

/// <summary>Sanitizes a kerx table the way a face loads it, from a blob, so that a table of several subtables is checked
/// as <c>hb_face_t</c> checks it; <c>farfbuzz_oracle_kerx_sanitize</c> keeps the range of the first subtable for the
/// rest.</summary>
/// <param name="data">The table bytes.</param>
/// <param name="length">Their length.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>Non-zero when the table sanitizes.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_kerx_load_sane(byte[] data, uint32 length, uint32 numGlyphs)

/// <summary>The index of a script/direction/GSUB-script's shaper in the
/// <c>HB_OT_SHAPERS_IMPLEMENT_SHAPERS</c> order (arabic, default, dumber, hangul, hebrew, indic, khmer, myanmar,
/// myanmar_zawgyi, thai, use), or 255 for a shaper outside the list.</summary>
/// <param name="script">The script tag.</param>
/// <param name="direction">The direction value.</param>
/// <param name="gsubScript">The chosen GSUB script tag.</param>
/// <returns>The shaper index.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern uint32 farfbuzz_oracle_shaper_categorize(uint32 script, uint32 direction, uint32 gsubScript)

/// <summary>Lowers the length limit and the operation budget the next shaping call of a buffer starts from, as
/// <c>hb_buffer_t::enter</c> keeps a limit below the idle default; a negative argument leaves that limit as it is.</summary>
/// <param name="buffer">The buffer.</param>
/// <param name="maxLen">The length limit, or a negative number.</param>
/// <param name="maxOps">The operation budget, or a negative number.</param>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern void farfbuzz_oracle_buffer_set_limits(nativeint buffer, int maxLen, int maxOps)

/// <summary>Whether a buffer's allocations succeeded, <c>hb_buffer_allocation_successful</c>.</summary>
/// <param name="buffer">The buffer.</param>
/// <returns>Non-zero when no allocation or limit failed.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int hb_buffer_allocation_successful(nativeint buffer)
/// <summary>Times the stages of shaping every line of a text, in the mode given (0 direct stages, 1 message stages, 2 untraced
/// shape, 3 no-op message callback, 4 timestamping message callback).</summary>
/// <param name="font">The font file.</param>
/// <param name="fontLength">The length of the font file.</param>
/// <param name="text">The UTF-8 text.</param>
/// <param name="textLength">The length of the text.</param>
/// <param name="starts">The first byte of each line.</param>
/// <param name="lengths">The length in bytes of each line.</param>
/// <param name="lines">The number of lines.</param>
/// <param name="rounds">The measured rounds, after one warm round.</param>
/// <param name="mode">The mode.</param>
/// <param name="out">Receives glyphs, timer cost, messages, differing lines, total nanoseconds, then nanoseconds and intervals per stage.</param>
/// <param name="capacity">The length of <paramref name="out"/>.</param>
/// <returns>The number of doubles written, or -1.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern int farfbuzz_oracle_time_stages(byte[] font, uint32 fontLength, byte[] text, uint32 textLength, int[] starts, int[] lengths, uint32 lines, uint32 rounds, uint32 mode, double[] out, uint32 capacity)

/// <summary>The name of a stage of a timing mode.</summary>
/// <param name="mode">The mode.</param>
/// <param name="index">The stage index.</param>
/// <returns>A pointer to a static string, or null past the last stage.</returns>
[<DllImport(OracleLibrary.LibraryName, CallingConvention = CallingConvention.Cdecl)>]
extern nativeint farfbuzz_oracle_stage_name(uint32 mode, uint32 index)
