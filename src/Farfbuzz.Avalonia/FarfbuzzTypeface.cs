using System;
using System.Collections.Immutable;
using System.IO;
using Avalonia.Media;
using Avalonia.Media.Fonts;
using Farfbuzz.HarfBuzzSharp.Bridge;
using CoreFace = Farfbuzz.Face;
using CoreFont = Farfbuzz.Font;
using CorePlanCache = Farfbuzz.PlanCache;

namespace Farfbuzz.AvaloniaShaping
{
	/// <summary>The face of a glyph typeface made for Farfbuzz: the core font at the face's units per em, built from the tables the platform typeface holds, and the plans the lines shaped with it
	/// share.</summary>
	public sealed class FarfbuzzTypeface : ITextShaperTypeface
	{
		private readonly object gate = new object ();
		private CorePlanCache plans = PlanCacheModule.empty;

		internal FarfbuzzTypeface (GlyphTypeface glyphTypeface)
		{
			Font = FontModule.create (FaceOf (glyphTypeface.PlatformTypeface));
		}

		internal CoreFont Font { get; }

		/// <summary>Releases nothing: the font is an immutable value over the bytes of the platform typeface.</summary>
		public void Dispose ()
		{
		}

		/// <summary>Takes the plan cache for one line; the cache a finished line leaves replaces it, a line that raced another keeping the cache it was given.</summary>
		internal CorePlanCache Plans {
			get {
				lock (gate) {
					return plans;
				}
			}
		}

		internal void Keep (CorePlanCache cache)
		{
			lock (gate) {
				plans = cache;
			}
		}

		private static CoreFace FaceOf (IPlatformTypeface platform)
		{
			if (platform is IFontMemory memory) {
				var tables = ImmutableDictionary.CreateBuilder<uint, ReadOnlyMemory<byte>> ();
				foreach (var tag in Faces.ShapingTags) {
					Add (memory, tables, tag);
				}

				foreach (var tag in Faces.OutlineTags) {
					Add (memory, tables, tag);
				}

				return Faces.OfTables (tables.ToImmutable (), 0, 0, -1);
			}

			if (platform.TryGetStream (out var stream)) {
				using (stream) {
					using var copy = new MemoryStream ();
					stream.CopyTo (copy);
					return Faces.OfBlob (copy.ToArray (), 0, 0, -1);
				}
			}

			return Faces.Empty;
		}

		private static void Add (IFontMemory memory, ImmutableDictionary<uint, ReadOnlyMemory<byte>>.Builder tables, uint tag)
		{
			if (memory.TryGetTable (new OpenTypeTag (tag), out var table)) {
				tables[tag] = table;
			}
		}
	}
}
