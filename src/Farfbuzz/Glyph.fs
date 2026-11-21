namespace Farfbuzz

open System
open System.Runtime.InteropServices

/// <summary>
/// One glyph of shaped output, laid out exactly as <c>hb_glyph_info_t</c> (20 bytes) so that the
/// HarfBuzzSharp-compatible facade can reinterpret arrays of it without copying.
/// </summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type GlyphInfo =
    {
        Codepoint: uint32  // the Unicode code point before shaping, the glyph index after
        Mask: uint32       // the feature mask while shaping; the GlyphFlags bits after
        Cluster: uint32    // the index of the input character this glyph belongs to
        Var1: uint32       // shaper-private storage, var1
        Var2: uint32       // shaper-private storage, var2
    }

/// <summary>The position of one glyph, laid out exactly as <c>hb_glyph_position_t</c> (20 bytes).</summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type GlyphPosition =
    {
        XAdvance: int  // how far the line advances horizontally after this glyph
        YAdvance: int  // how far the line advances vertically after this glyph
        XOffset: int   // horizontal displacement of the glyph from its pen position
        YOffset: int   // vertical displacement of the glyph from its pen position
        Var: uint32    // shaper-private storage, var
    }

/// <summary>Flags attached to output glyphs, the values of <c>hb_glyph_flags_t</c>.</summary>
[<Flags>]
type GlyphFlags =
    | None = 0u
    | UnsafeToBreak = 1u
    | UnsafeToConcat = 2u
    | SafeToInsertTatweel = 4u
    | Defined = 7u

/// <summary>The ink box of a glyph, <c>hb_glyph_extents_t</c>.</summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type GlyphExtents =
    {
        XBearing: int  // distance from the origin to the left edge of the ink
        YBearing: int  // distance from the origin to the top edge of the ink
        Width: int     // width of the ink; negative when right-to-left
        Height: int    // height of the ink; negative when the y axis points up
    }
