using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using Avalonia;
using Avalonia.Media;
using Avalonia.Media.TextFormatting;

namespace Farfbuzz.Avalonia.Tests
{
	/// <summary>The application: no theme, no window.</summary>
	public class App : Application
	{
	}

	/// <summary>One line of the corpus with the family that shapes it.</summary>
	internal sealed record Line (string Name, string Family, string Text, sbyte BidiLevel, string Culture, int Start = 0, int Length = -1, double LetterSpacing = 0.0, string Features = null);

	internal static class Program
	{
		private static readonly Line[] Corpus = {
			new Line ("latin", "Roboto", "Office waffles: fluffy 12.5% off, AVATAR.", 0, "en-US"),
			new Line ("latin-features", "Roboto", "fi fl ffi 1/2 Th", 0, "en-US"),
			new Line ("arabic", "Amiri", "العربية مرحبا بالعالم", 1, "ar-EG"),
			new Line ("devanagari", "Noto Sans Devanagari", "हिन्दी क्षत्रिय संस्कृत", 0, "hi-IN"),
			new Line ("crlf", "Roboto", "first line\r\n", 0, "en-US"),
			new Line ("lf", "Roboto", "second line\n", 0, "en-US"),
			new Line ("tab", "Roboto", "a\tb\tc", 0, "en-US"),
			new Line ("arabic-slice", "Amiri", "العربية مرحبا بالعالم", 1, "ar-EG", 7, 5),
			new Line ("latin-slice", "Roboto", "Office waffles: fluffy 12.5% off, AVATAR.", 0, "en-US", 3, 12),
			new Line ("devanagari-slice", "Noto Sans Devanagari", "हिन्दी क्षत्रिय संस्कृत", 0, "hi-IN", 7, 8),
			new Line ("latin-spaced", "Roboto", "Office waffles", 0, "en-US", 0, -1, 1.5),
			new Line ("latin-no-liga", "Roboto", "fi fl ffi office", 0, "en-US", 0, -1, 0.0, "liga=0"),
			new Line ("crlf-slice", "Roboto", "one\r\ntwo\r\nthree", 0, "en-US", 0, 5),
		};

		private static int Main (string[] args)
		{
			if (args.Length != 3 || args[0] != "--impl" || (args[1] != "harfbuzz" && args[1] != "farfbuzz")) {
				Console.Error.WriteLine ("usage: --impl harfbuzz|farfbuzz FILE");
				return 2;
			}

			var builder = AppBuilder.Configure<App> ()
				.UseSkia ()
				.UseStandardRuntimePlatformSubsystem ()
				.UseWindowingSubsystem (() => { }, "none");
			builder = args[1] == "harfbuzz" ? builder.UseHarfBuzz () : builder.UseFarfbuzz ();
			builder.SetupWithoutStarting ();

			var lines = new List<string> ();
			foreach (var line in Corpus) {
				var family = new FontFamily ($"avares://Farfbuzz.Avalonia.Tests/Assets#{line.Family}");
				var glyphTypeface = new Typeface (family).GlyphTypeface;
				var features = line.Features == null ? null : new List<FontFeature> { FontFeature.Parse (line.Features) };
				var options = new TextShaperOptions (glyphTypeface, 16.0, line.BidiLevel, new CultureInfo (line.Culture), 32.0, line.LetterSpacing, features);
				var text = line.Length < 0 ? line.Text.AsMemory () : line.Text.AsMemory (line.Start, line.Length);
				var buffer = TextShaper.Current.ShapeText (text, options);
				for (var index = 0; index < buffer.Length; index++) {
					var glyph = buffer[index];
					lines.Add (string.Join ('\t', line.Name, index, glyph.GlyphIndex, glyph.GlyphCluster, glyph.GlyphAdvance.ToString ("R", CultureInfo.InvariantCulture), glyph.GlyphOffset.X.ToString ("R", CultureInfo.InvariantCulture), glyph.GlyphOffset.Y.ToString ("R", CultureInfo.InvariantCulture)));
				}
			}

			Directory.CreateDirectory (Path.GetDirectoryName (Path.GetFullPath (args[2])));
			File.WriteAllText (args[2], string.Join ('\n', lines) + "\n");
			Console.WriteLine ($"{lines.Count} glyphs written to {args[2]} through {args[1]}");
			return 0;
		}
	}
}
