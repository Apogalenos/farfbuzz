using System;
using System.Collections.Immutable;
using System.Globalization;
using System.Runtime.InteropServices;
using Avalonia;
using Avalonia.Media;
using Avalonia.Media.TextFormatting;
using Avalonia.Platform;
using Farfbuzz.HarfBuzzSharp.Bridge;
using CoreFeature = Farfbuzz.Feature;
using CoreInfo = Farfbuzz.GlyphInfo;
using CoreRun = Farfbuzz.GlyphRun;

namespace Farfbuzz.AvaloniaShaping
{
	/// <summary>Avalonia 12's text shaper on Farfbuzz: the steps of <c>HarfBuzzTextShaper</c> on the core's run and shape functions.</summary>
	public sealed class FarfbuzzTextShaper : ITextShaperImpl
	{
		private const char ZeroWidthNonJoiner = '‌';
		private const char Space = ' ';
		private const char Tab = '\t';
		private const int TabWidthInSpaces = 4;

		/// <summary>Creates the Farfbuzz side of a glyph typeface.</summary>
		/// <param name="glyphTypeface">The glyph typeface.</param>
		/// <returns>The typeface the shaper reads the font of.</returns>
		public ITextShaperTypeface CreateTypeface (GlyphTypeface glyphTypeface) =>
			new FarfbuzzTypeface (glyphTypeface);

		/// <summary>Shapes a text.</summary>
		/// <param name="text">The text, which may be a part of a longer memory.</param>
		/// <param name="options">The options of the run.</param>
		/// <returns>The glyphs, clusters relative to the start of <paramref name="text" />.</returns>
		public ShapedBuffer ShapeText (ReadOnlyMemory<char> text, TextShaperOptions options)
		{
			var glyphTypeface = options.GlyphTypeface;
			var emSize = options.FontRenderingEmSize;
			var bidiLevel = options.BidiLevel;
			if (text.IsEmpty) {
				return new ShapedBuffer (text, 0, glyphTypeface, emSize, bidiLevel);
			}

			var typeface = (FarfbuzzTypeface)glyphTypeface.TextShaperTypeface;
			var whole = ContainingMemory (text, out var start);
			var run = AdapterRuns.Decode (whole.Span, start, text.Length);
			run = MergeBreakPair (run);
			run = GlyphRunModule.guessSegmentProperties (LanguageModule.cLocale, run);
			run = GlyphRunModule.withDirection ((bidiLevel & 1) == 0 ? Direction.Ltr : Direction.Rtl, run);
			run = GlyphRunModule.withLanguage (LanguageModule.ofString ((options.Culture ?? CultureInfo.CurrentUICulture).TwoLetterISOLanguageName), run);

			var features = Features (options);
			var (shaped, cache) = Shape.shapeCached (typeface.Plans, typeface.Font, features, Microsoft.FSharp.Core.FSharpValueOption<ImmutableArray<string>>.ValueNone, run);
			typeface.Keep (cache);
			if (!shaped.IsOk) {
				throw new InvalidOperationException ("Farfbuzz could not shape the text: " + shaped.ErrorValue);
			}

			var result = shaped.ResultValue;
			var infos = result.Infos;
			var positions = result.Positions;
			var textScale = emSize / typeface.Font.XScale;
			var buffer = new ShapedBuffer (text, infos.Length, glyphTypeface, emSize, bidiLevel);
			for (var index = 0; index < infos.Length; index++) {
				var glyphIndex = (ushort)infos[index].Codepoint;
				var cluster = (int)infos[index].Cluster - start;
				var advance = positions[index].XAdvance * textScale + options.LetterSpacing;
				var offset = new Vector (positions[index].XOffset * textScale, -positions[index].YOffset * textScale);
				if (text.Span[cluster] == Tab) {
					glyphIndex = SpaceGlyph (typeface);
					advance = options.IncrementalTabWidth > 0
						? options.IncrementalTabWidth
						: TabWidthInSpaces * FontModule.hAdvance (typeface.Font, glyphIndex) * textScale;
				}

				buffer[index] = new Avalonia.Media.TextFormatting.GlyphInfo (glyphIndex, cluster, advance, offset);
			}

			return buffer;
		}

		private static ushort SpaceGlyph (FarfbuzzTypeface typeface)
		{
			var glyph = FontModule.nominalGlyph (typeface.Font, Space);
			return glyph.IsSome ? (ushort)glyph.Value : (ushort)0;
		}

		private static ReadOnlyMemory<char> ContainingMemory (ReadOnlyMemory<char> text, out int start)
		{
			if (MemoryMarshal.TryGetString (text, out var content, out var offset, out _)) {
				start = offset;
				return content.AsMemory ();
			}

			if (MemoryMarshal.TryGetArray (text, out var segment)) {
				start = segment.Offset;
				return segment.Array;
			}

			start = 0;
			return text;
		}

		private static ImmutableArray<CoreFeature> Features (TextShaperOptions options)
		{
			var requested = options.FontFeatures;
			if (requested == null || requested.Count == 0) {
				return ImmutableArray<CoreFeature>.Empty;
			}

			var builder = ImmutableArray.CreateBuilder<CoreFeature> (requested.Count);
			foreach (var feature in requested) {
				builder.Add (new CoreFeature (TagModule.ofString (feature.Tag), (uint)feature.Value, (uint)feature.Start, (uint)feature.End));
			}

			return builder.MoveToImmutable ();
		}

		private static CoreRun MergeBreakPair (CoreRun run) =>
			AdapterRuns.MergeBreakPair (run);
	}
}
