#nullable disable

using System;
using System.Collections.Generic;

namespace HarfBuzzSharp
{
	/// <summary>Represents a set of font functions that can be used to replace the way a <see cref="T:HarfBuzzSharp.Font" /> answers glyph questions.</summary>
	/// <remarks />
	public class FontFunctions : NativeObject
	{
		private static readonly Lazy<FontFunctions> emptyFontFunctions = new Lazy<FontFunctions> (() => new StaticFontFunctions ());

		internal FontExtentsDelegate hExtentsDelegate;
		internal FontExtentsDelegate vExtentsDelegate;
		internal NominalGlyphDelegate nominalDelegate;
		internal NominalGlyphsDelegate nominalsDelegate;
		internal VariationGlyphDelegate variationDelegate;
		internal GlyphAdvanceDelegate hAdvanceDelegate;
		internal GlyphAdvanceDelegate vAdvanceDelegate;
		internal GlyphAdvancesDelegate hAdvancesDelegate;
		internal GlyphAdvancesDelegate vAdvancesDelegate;
		internal GlyphOriginDelegate hOriginDelegate;
		internal GlyphOriginDelegate vOriginDelegate;
		internal GlyphKerningDelegate hKerningDelegate;
		internal GlyphExtentsDelegate extentsDelegate;
		internal GlyphContourPointDelegate contourDelegate;
		internal GlyphNameDelegate nameDelegate;
		internal GlyphFromNameDelegate fromNameDelegate;
		private readonly List<ReleaseDelegate> destroys = new List<ReleaseDelegate> ();
		private bool immutable;

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.FontFunctions" /> class with no callbacks set.</summary>
		/// <remarks />
		public FontFunctions ()
		{
		}

		/// <summary>Gets a reference to the empty <see cref="T:HarfBuzzSharp.FontFunctions" /> instance with no callbacks set.</summary>
		/// <value>The empty font functions instance.</value>
		/// <remarks />
		public static FontFunctions Empty => emptyFontFunctions.Value;

		/// <summary>Gets a value indicating whether this font functions instance is immutable.</summary>
		/// <value><see langword="true" /> if the instance is immutable; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool IsImmutable => immutable;

		/// <summary>Makes this font functions instance immutable, preventing further modifications.</summary>
		/// <remarks />
		public void MakeImmutable ()
		{
			immutable = true;
		}

		/// <summary>Sets the callback for retrieving font extents for horizontal text layout.</summary>
		/// <param name="del">The delegate to set for retrieving horizontal font extents.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetHorizontalFontExtentsDelegate (FontExtentsDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			hExtentsDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving font extents for vertical text layout.</summary>
		/// <param name="del">The delegate to set for retrieving vertical font extents.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetVerticalFontExtentsDelegate (FontExtentsDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			vExtentsDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for mapping a Unicode code point to its nominal glyph identifier.</summary>
		/// <param name="del">The delegate to set for mapping code points to glyphs.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetNominalGlyphDelegate (NominalGlyphDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			nominalDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for mapping multiple Unicode code points to their nominal glyph identifiers.</summary>
		/// <param name="del">The delegate to set for batch mapping code points to glyphs.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetNominalGlyphsDelegate (NominalGlyphsDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			nominalsDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving the glyph identifier for a Unicode code point with a variation selector.</summary>
		/// <param name="del">The delegate to set for retrieving variation glyphs.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetVariationGlyphDelegate (VariationGlyphDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			variationDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving horizontal advance width for a single glyph.</summary>
		/// <param name="del">The delegate to set for retrieving horizontal glyph advances.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetHorizontalGlyphAdvanceDelegate (GlyphAdvanceDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			hAdvanceDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving vertical advance height for a single glyph.</summary>
		/// <param name="del">The delegate to set for retrieving vertical glyph advances.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetVerticalGlyphAdvanceDelegate (GlyphAdvanceDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			vAdvanceDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving horizontal advance widths for multiple glyphs.</summary>
		/// <param name="del">The delegate to set for retrieving horizontal glyph advances in batch.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetHorizontalGlyphAdvancesDelegate (GlyphAdvancesDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			hAdvancesDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving vertical advance heights for multiple glyphs.</summary>
		/// <param name="del">The delegate to set for retrieving vertical glyph advances in batch.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetVerticalGlyphAdvancesDelegate (GlyphAdvancesDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			vAdvancesDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving the origin point for horizontal glyph layout.</summary>
		/// <param name="del">The delegate to set for retrieving horizontal glyph origins.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetHorizontalGlyphOriginDelegate (GlyphOriginDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			hOriginDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving the origin point for vertical glyph layout.</summary>
		/// <param name="del">The delegate to set for retrieving vertical glyph origins.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetVerticalGlyphOriginDelegate (GlyphOriginDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			vOriginDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving horizontal kerning adjustment between two glyphs.</summary>
		/// <param name="del">The delegate to set for retrieving horizontal glyph kerning.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetHorizontalGlyphKerningDelegate (GlyphKerningDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			hKerningDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving glyph extents (bounding box).</summary>
		/// <param name="del">The delegate to set for retrieving glyph extents.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetGlyphExtentsDelegate (GlyphExtentsDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			extentsDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving glyph contour point coordinates.</summary>
		/// <param name="del">The delegate to set for retrieving glyph contour point coordinates.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetGlyphContourPointDelegate (GlyphContourPointDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			contourDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving the name of a glyph.</summary>
		/// <param name="del">The delegate to set for retrieving glyph names.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetGlyphNameDelegate (GlyphNameDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			nameDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Sets the callback for retrieving a glyph identifier from a glyph name.</summary>
		/// <param name="del">The delegate to set for retrieving glyph identifiers from names.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetGlyphFromNameDelegate (GlyphFromNameDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			fromNameDelegate = del;
			if (destroy != null) {
				destroys.Add (destroy);
			}
		}

		/// <summary>Releases the unmanaged resources used by the <see cref="T:HarfBuzzSharp.FontFunctions" /> and optionally releases the managed resources.</summary>
		/// <param name="disposing"><see langword="true" /> to release both managed and unmanaged resources; <see langword="false" /> to release only unmanaged resources.</param>
		/// <remarks />
		protected override void Dispose (bool disposing) =>
			base.Dispose (disposing);

		/// <summary>Releases the unmanaged resources used.</summary>
		/// <remarks />
		protected override void DisposeHandler ()
		{
			foreach (var destroy in destroys) {
				destroy ();
			}

			destroys.Clear ();
		}

		private void VerifyParameters (Delegate del)
		{
			_ = del ?? throw new ArgumentNullException (nameof (del));

			if (IsImmutable)
				throw new InvalidOperationException ($"{nameof (FontFunctions)} is immutable and can't be changed.");
		}

		private class StaticFontFunctions : FontFunctions
		{
			public StaticFontFunctions ()
			{
				MakeImmutable ();
			}

			protected override void Dispose (bool disposing)
			{
			}
		}
	}
}
