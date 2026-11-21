using System;
using System.IO;
using HarfBuzzSharp;
using Buffer = HarfBuzzSharp.Buffer;

namespace CSharpProbe
{
	internal static class Program
	{
		private static int passed;
		private static int failed;

		private static void Check (bool condition, string name)
		{
			if (condition) {
				passed++;
			} else {
				failed++;
				Console.WriteLine ($"FAIL {name}");
			}
		}

		private static int Main ()
		{
			Console.WriteLine ($"impl: {typeof (Tag).Assembly.FullName}");
			ValueTypes ();
			BufferShapes ();
			ShapedMergeBreakPair ();
			GlyphQueries ();
			Console.WriteLine (failed == 0 ? $"PASS {passed}" : $"FAIL {failed} of {passed + failed}");
			return failed == 0 ? 0 : 1;
		}

		private static string FindFont ()
		{
			var directory = new DirectoryInfo (AppContext.BaseDirectory);
			while (directory != null && !File.Exists (Path.Combine (directory.FullName, "Farfbuzz.slnx"))) {
				directory = directory.Parent;
			}

			return Path.Combine (directory.FullName, "upstream", "harfbuzz", "perf", "fonts", "Roboto-Regular.ttf");
		}

		private static (uint[] glyphs, int[] advances) Shaped (Font font, Buffer buffer)
		{
			buffer.Direction = Direction.LeftToRight;
			buffer.Language = new Language (System.Globalization.CultureInfo.InvariantCulture);
			font.Shape (buffer);
			var infos = buffer.GlyphInfos;
			var positions = buffer.GlyphPositions;
			var glyphs = new uint[infos.Length];
			var advances = new int[infos.Length];
			for (var i = 0; i < infos.Length; i++) {
				glyphs[i] = infos[i].Codepoint;
				advances[i] = positions[i].XAdvance;
			}

			return (glyphs, advances);
		}

		private static void GlyphQueries ()
		{
			using var blob = Blob.FromFile (FindFont ());
			using var face = new Face (blob, 0);
			using var font = new Font (face);
			font.SetScale (512, 512);
			font.GetScale (out var xScale, out var yScale);
			Check (xScale == 512 && yScale == 512, "GetScale out parameters");
			Check (font.TryGetGlyph (0x41, out var glyph) && glyph != 0, "TryGetGlyph (int, out)");
			Check (font.TryGetNominalGlyph (0x41u, out var nominal) && nominal == glyph, "TryGetNominalGlyph (uint, out)");
			Check (!font.TryGetVariationGlyph (0x41u, 0xFE0Fu, out var variation) && variation == 0, "TryGetVariationGlyph reports no variation");
			var advance = font.GetHorizontalGlyphAdvance (glyph);
			var advances = font.GetHorizontalGlyphAdvances (stackalloc uint[] { glyph });
			Check (advance > 0 && advances.Length == 1 && advances[0] == advance, "advance and advances");
			Check (font.TryGetGlyphExtents (glyph, out var extents) && extents.Width > 0 && extents.Height != 0, "TryGetGlyphExtents");
			Check (font.TryGetHorizontalFontExtents (out var fontExtents) && fontExtents.Ascender > 0, "TryGetHorizontalFontExtents");
			Check (font.GetFontExtentsForDirection (Direction.LeftToRight).Ascender == fontExtents.Ascender, "GetFontExtentsForDirection");
			Check (font.OpenTypeMetrics.TryGetPosition (OpenTypeMetricsTag.UnderlineOffset, out var underline) && underline != 0, "OpenTypeMetrics.TryGetPosition");
			Check (font.SupportedShapers.Length >= 1 && font.SupportedShapers[0] == "ot", "SupportedShapers");
		}

		private static unsafe void ShapedMergeBreakPair ()
		{
			using var blob = Blob.FromFile (FindFont ());
			using var face = new Face (blob, 0);
			using var font = new Font (face);
			using var written = new Buffer ();
			written.AddUtf16 ("a\r\n", 0, 3);
			var glyphInfos = written.GetGlyphInfoSpan ();
			fixed (GlyphInfo* p = &glyphInfos[2]) {
				p->Codepoint = 0x200C;
			}

			using var direct = new Buffer ();
			direct.AddUtf16 ("a\r\u200C", 0, 3);
			var (writtenGlyphs, writtenAdvances) = Shaped (font, written);
			var (directGlyphs, directAdvances) = Shaped (font, direct);
			Check (writtenGlyphs.Length == 3 && writtenGlyphs.AsSpan ().SequenceEqual (directGlyphs), "shaping a buffer written through the glyph span equals shaping the written text");
			Check (writtenAdvances.AsSpan ().SequenceEqual (directAdvances), "positions of the written buffer equal those of the written text");
			Check (written.ContentType == ContentType.Glyphs && written.GlyphInfos[0].Cluster == 0, "shaped content and clusters");
		}

		private static unsafe void BufferShapes ()
		{
			var buffer = new Buffer ();
			buffer.Reset ();
			buffer.AddUtf16 ("ab\r\ncd", 0, 6);
			Check (buffer.Length == 6 && buffer.ContentType == ContentType.Unicode, "AddUtf16 optional-free overload");
			Check (buffer.GlyphInfos[2].Cluster == 2, "clusters are offsets from the start of the text");

			var glyphInfos = buffer.GetGlyphInfoSpan ();
			var length = glyphInfos.Length;
			fixed (GlyphInfo* p = &glyphInfos[length - 2]) {
				p[0].Codepoint = 0x200C;
				p[1].Codepoint = 0x200D;
			}

			Check (buffer.GlyphInfos[4].Codepoint == 0x200C && buffer.GlyphInfos[5].Codepoint == 0x200D, "MergeBreakPair pointer write lands in the buffer");
			Check (buffer.GetGlyphPositionSpan ().Length == 6, "GetGlyphPositionSpan");

			buffer.Reset ();
			buffer.AddUtf8 ("héllo");
			buffer.AddUtf32 (new uint[] { 0x1F600 }.AsSpan (), 0, -1);
			buffer.AddCodepoints (new uint[] { 0x41 }.AsSpan ());
			Check (buffer.Length == 7, "AddUtf8, AddUtf32 and AddCodepoints overloads");
			buffer.ClearContents ();
			Check (buffer.Length == 0, "ClearContents");
		}

		private static void ValueTypes ()
		{
			uint raw = new Tag ('l', 'i', 'g', 'a');
			Check (raw == 0x6C696761u, "Tag to uint implicit conversion");
			Tag back = raw;
			Check (back == new Tag ('l', 'i', 'g', 'a'), "uint to Tag implicit conversion");
			Check (Tag.None.ToString () == "None" && Tag.Max.ToString () == "Max" && Tag.MaxSigned.ToString () == "MaxSigned", "Tag static readonly fields");
			Check (Tag.Parse ("li") == new Tag ('l', 'i', ' ', ' '), "Tag.Parse pads");

			Check (Feature.TryParse ("-liga[2:5]", out var feature), "Feature.TryParse out");
			Check (feature.Value == 0 && feature.Start == 2 && feature.End == 5, "Feature fields");
			Check (feature.ToString () == "-liga[2:5]", "Feature.ToString");
			Check (!Feature.TryParse (string.Empty, out var rejected) && rejected.Value == 0, "Feature.TryParse rejects");
			var whole = new Feature (Tag.Parse ("kern"));
			Check (whole.Value == 1 && whole.Start == 0 && whole.End == uint.MaxValue, "Feature constructor defaults");

			Script latin = Script.Latin;
			uint latinRaw = latin;
			Check (latinRaw == 0x4C61746Eu, "Script static readonly field and conversion");
			Check (Script.Parse ("Arab").HorizontalDirection == Direction.RightToLeft, "Script.Parse and HorizontalDirection");
			Check (Script.TryParse ("Zinh", out var inherited) && inherited == Script.Inherited, "Script.TryParse out");
			Check (Script.Common.ToString () == "Zyyy", "Script.ToString");

			var info = new GlyphInfo { Codepoint = 65, Cluster = 3, Mask = 1 };
			Check (info.Codepoint == 65 && info.Cluster == 3 && info.GlyphFlags == GlyphFlags.UnsafeToBreak, "GlyphInfo setters and GlyphFlags");
			var position = new GlyphPosition { XAdvance = 10, YOffset = -2 };
			Check (position.XAdvance == 10 && position.YOffset == -2, "GlyphPosition setters");
			Check (Buffer.DefaultReplacementCodepoint == 0xFFFD, "Buffer.DefaultReplacementCodepoint const");

			ReleaseDelegate release = () => { };
			Check (release != null, "ReleaseDelegate construction");
			Check ((int)Direction.TopToBottom == 6 && (int)ClusterLevel.MonotoneCharacters == 1 && (int)ContentType.Glyphs == 2, "enum values");
		}
	}
}
