using System;
using System.Globalization;
using HarfBuzzSharp;
using Buffer = HarfBuzzSharp.Buffer;

namespace Farfbuzz.Facade.Consumer;

/// <summary>
/// Repeats, call for call, how Avalonia 11.3.13 (TextShaperImpl, GlyphTypefaceImpl) uses HarfBuzzSharp,
/// so that a facade can be checked against a binary compiled for the real package.
/// </summary>
public static class AvaloniaMirror
{
    /// <summary>
    /// Fills a buffer, writes a cluster and a code point through a pointer taken from the read-only span, as
    /// Avalonia's MergeBreakPair does, reverses, and reports what the buffer then holds.
    /// </summary>
    public static unsafe string WriteThroughGlyphSpan(string text)
    {
        using var buffer = new Buffer();
        buffer.AddUtf16(text.AsSpan(), 0, text.Length);
        buffer.Direction = Direction.LeftToRight;
        buffer.Language = new Language(CultureInfo.InvariantCulture);
        var glyphInfos = buffer.GetGlyphInfoSpan();
        fixed (GlyphInfo* p = &glyphInfos[1])
        {
            p->Cluster = 42;
            p->Codepoint = 0x1F600;
        }
        var afterWrite = buffer.GetGlyphInfoSpan();
        var written = $"{afterWrite[1].Cluster}:{afterWrite[1].Codepoint:X}";
        buffer.Reverse();
        var reversed = buffer.GetGlyphInfoSpan();
        return $"{buffer.Length};{written};{reversed[buffer.Length - 2].Cluster};{buffer.Direction}";
    }

    /// <summary>Builds features as Avalonia does, from a tag spelling.</summary>
    public static string Features()
    {
        var feature = new Feature(Tag.Parse("liga"), 0, 3, 5);
        uint tag = feature.Tag;
        return $"{tag:X8};{feature.Value};{feature.Start};{feature.End}";
    }

    /// <summary>Avalonia's whole shaping path. Every member it touches must bind; running it needs phases 2 to 4.</summary>
    public static unsafe int ShapeLikeAvalonia(IntPtr fontData, int fontLength, string text)
    {
        using var blob = new Blob(fontData, fontLength, MemoryMode.ReadOnly, () => { });
        using var face = new Face((f, tag) => blob) { UnitsPerEm = 2048 };
        using var font = new Font(face);
        font.SetFunctionsOpenType();
        font.GetScale(out var xScale, out _);
        font.OpenTypeMetrics.TryGetPosition(OpenTypeMetricsTag.UnderlineOffset, out var underline);
        font.TryGetGlyph(0x41u, out var glyph);
        var advance = font.GetHorizontalGlyphAdvance(glyph);
        var advances = font.GetHorizontalGlyphAdvances(stackalloc uint[] { glyph });
        font.TryGetGlyphExtents(glyph, out var extents);
        using var buffer = new Buffer();
        buffer.Reset();
        buffer.AddUtf16(text.AsSpan(), 0, text.Length);
        buffer.GuessSegmentProperties();
        buffer.Direction = Direction.RightToLeft;
        font.Shape(buffer, new Feature(Tag.Parse("kern")));
        var positions = buffer.GetGlyphPositionSpan();
        return xScale + underline + advance + advances[0] + extents.Width + positions[0].XAdvance + positions[0].XOffset + positions[0].YOffset;
    }
}
