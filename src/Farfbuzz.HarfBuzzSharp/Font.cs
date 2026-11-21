#nullable disable

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>Represents a font at a particular size and with particular variations.</summary>
	/// <remarks />
	public class Font : NativeObject
	{
		private readonly Face face;
		private volatile FontSnapshot snapshot;
		private ReleaseDelegate fontFunctionsDestroy;

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Font" /> class using a specific font face.</summary>
		/// <param name="face">The face to use.</param>
		/// <remarks />
		public Font (Face face)
		{
			if (face == null)
				throw new ArgumentNullException (nameof (face));

			this.face = face;
			var frozen = face.Freeze ();
			face.AddFont ();
			snapshot = new FontSnapshot (frozen, frozen.Core.Upem, frozen.Core.Upem);
			OpenTypeMetrics = new OpenTypeMetrics (this);
		}

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Font" /> class as a sub-font of the specified parent font.</summary>
		/// <param name="parent">The parent font to use as a basis for this font.</param>
		/// <remarks>The new font inherits settings from the parent that can be overridden.</remarks>
		public Font (Font parent)
		{
			if (parent == null)
				throw new ArgumentNullException (nameof (parent));
			if (parent.Handle == IntPtr.Zero)
				throw new ArgumentException (nameof (parent.Handle));

			Parent = parent;
			face = parent.face;
			face.AddFont ();
			snapshot = parent.snapshot;
			OpenTypeMetrics = new OpenTypeMetrics (this);
		}

		/// <summary>Gets the parent font, if this font was created as a sub-font.</summary>
		/// <value>The parent font, or <see langword="null" /> if this font has no parent.</value>
		/// <remarks />
		public Font Parent { get; }

		/// <summary>Gets access to OpenType metrics for this font.</summary>
		/// <value>The OpenType metrics accessor.</value>
		/// <remarks>Provides access to metrics defined in the OpenType MVAR table.</remarks>
		public OpenTypeMetrics OpenTypeMetrics { get; }

		/// <summary>Gets the list of supported shaper names.</summary>
		/// <value>An array of shaper names that can be used with the Shape method.</value>
		/// <remarks />
		public string[] SupportedShapers =>
			Shaping.SupportedShapers.ToArray ();

		/// <summary>Sets custom font functions for this font.</summary>
		/// <param name="fontFunctions">The font functions to use.</param>
		/// <remarks>Font functions provide callbacks for glyph metrics and other font information.</remarks>
		public void SetFontFunctions (FontFunctions fontFunctions) =>
			SetFontFunctions (fontFunctions, null, null);

		/// <summary>Sets custom font functions for this font with associated user data.</summary>
		/// <param name="fontFunctions">The font functions to use.</param>
		/// <param name="fontData">User data to pass to the font functions.</param>
		/// <remarks>Font functions provide callbacks for glyph metrics and other font information.</remarks>
		public void SetFontFunctions (FontFunctions fontFunctions, object fontData) =>
			SetFontFunctions (fontFunctions, fontData, null);

		/// <summary>Sets custom font functions for this font with associated user data and a destroy callback.</summary>
		/// <param name="fontFunctions">The font functions to use.</param>
		/// <param name="fontData">User data to pass to the font functions.</param>
		/// <param name="destroy">A delegate to call when the font data is no longer needed.</param>
		/// <remarks />
		public void SetFontFunctions (FontFunctions fontFunctions, object fontData, ReleaseDelegate destroy)
		{
			_ = fontFunctions ?? throw new ArgumentNullException (nameof (fontFunctions));

			fontFunctions.MakeImmutable ();
			snapshot = snapshot.WithFunctions (new FontFuncsHolder (this, fontFunctions, fontData));
			Interlocked.Exchange (ref fontFunctionsDestroy, destroy)?.Invoke ();
		}

		/// <summary>Retrieves the font scale.</summary>
		/// <param name="xScale">The scale along the x-axis.</param>
		/// <param name="yScale">The scale along the y-axis.</param>
		/// <remarks />
		public void GetScale (out int xScale, out int yScale)
		{
			var current = snapshot;
			xScale = current.XScale;
			yScale = current.YScale;
		}

		/// <summary>Sets the font scale.</summary>
		/// <param name="xScale">The scale along the x-axis.</param>
		/// <param name="yScale">The scale along the y-axis.</param>
		/// <remarks />
		public void SetScale (int xScale, int yScale)
		{
			snapshot = snapshot.WithScale (xScale, yScale);
		}

		/// <summary>Tries to get the horizontal extents of the font.</summary>
		/// <param name="extents">Returns the horizontal font extents.</param>
		/// <returns><see langword="true" /> if the extents were found; otherwise, <see langword="false" />.</returns>
		/// <remarks>Extents include ascender, descender, and line gap values.</remarks>
		public bool TryGetHorizontalFontExtents (out FontExtents extents)
		{
			var (found, value) = Fonts.HExtents (snapshot.Light);
			extents = ToExtents (value);
			return found;
		}

		/// <summary>Tries to get the vertical extents of the font.</summary>
		/// <param name="extents">Returns the vertical font extents.</param>
		/// <returns><see langword="true" /> if the extents were found; otherwise, <see langword="false" />.</returns>
		/// <remarks>Extents include ascender, descender, and line gap values for vertical text.</remarks>
		public bool TryGetVerticalFontExtents (out FontExtents extents)
		{
			var (found, value) = Fonts.VExtents (snapshot.Light);
			extents = ToExtents (value);
			return found;
		}

		/// <summary>Tries to get the nominal glyph ID for a Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="glyph">Returns the nominal glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks>The nominal glyph is the default glyph for a character, without variation selectors.</remarks>
		public bool TryGetNominalGlyph (int unicode, out uint glyph) =>
			TryGetNominalGlyph ((uint)unicode, out glyph);

		/// <summary>Tries to get the nominal glyph ID for a Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="glyph">Returns the nominal glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks>The nominal glyph is the default glyph for a character, without variation selectors.</remarks>
		public bool TryGetNominalGlyph (uint unicode, out uint glyph)
		{
			var (found, value) = Glyphs.Nominal (snapshot.Light, unicode);
			glyph = value;
			return found;
		}

		/// <summary>Tries to get a variation glyph for a Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="glyph">Returns the variation glyph ID if found.</param>
		/// <returns><see langword="true" /> if a variation glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetVariationGlyph (int unicode, out uint glyph) =>
			TryGetVariationGlyph (unicode, 0, out glyph);

		/// <summary>Tries to get a variation glyph for a Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="glyph">Returns the variation glyph ID if found.</param>
		/// <returns><see langword="true" /> if a variation glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetVariationGlyph (uint unicode, out uint glyph)
		{
			var (found, value) = Glyphs.Variation (snapshot.Light, unicode, 0);
			glyph = value;
			return found;
		}

		/// <summary>Tries to get a variation glyph for a Unicode code point with a specific variation selector.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="variationSelector">The variation selector code point.</param>
		/// <param name="glyph">Returns the variation glyph ID if found.</param>
		/// <returns><see langword="true" /> if a variation glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetVariationGlyph (int unicode, uint variationSelector, out uint glyph) =>
			TryGetVariationGlyph ((uint)unicode, variationSelector, out glyph);

		/// <summary>Tries to get a variation glyph for a Unicode code point with a specific variation selector.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="variationSelector">The variation selector code point.</param>
		/// <param name="glyph">Returns the variation glyph ID if found.</param>
		/// <returns><see langword="true" /> if a variation glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetVariationGlyph (uint unicode, uint variationSelector, out uint glyph)
		{
			var (found, value) = Glyphs.Variation (snapshot.Light, unicode, variationSelector);
			glyph = value;
			return found;
		}

		/// <summary>Gets the horizontal advance width of a glyph.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <returns>The horizontal advance width in font units.</returns>
		/// <remarks />
		public int GetHorizontalGlyphAdvance (uint glyph) =>
			Glyphs.HAdvance (snapshot.Light, glyph);

		/// <summary>Gets the vertical advance height of a glyph.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <returns>The vertical advance height in font units.</returns>
		/// <remarks />
		public int GetVerticalGlyphAdvance (uint glyph) =>
			Glyphs.VAdvance (snapshot.Light, glyph);

		/// <summary>Gets the horizontal advance widths of multiple glyphs.</summary>
		/// <param name="glyphs">The span of glyph IDs.</param>
		/// <returns>An array of horizontal advances in font units.</returns>
		/// <remarks />
		public int[] GetHorizontalGlyphAdvances (ReadOnlySpan<uint> glyphs) =>
			Glyphs.HAdvances (snapshot.Light, glyphs.ToArray ()).Advances.ToArray ();

		/// <summary>Gets the horizontal advance widths of multiple glyphs.</summary>
		/// <param name="firstGlyph">A pointer to the first glyph ID.</param>
		/// <param name="count">The number of glyphs.</param>
		/// <returns>An array of horizontal advances in font units.</returns>
		/// <remarks />
		public unsafe int[] GetHorizontalGlyphAdvances (IntPtr firstGlyph, int count) =>
			GetHorizontalGlyphAdvances (new ReadOnlySpan<uint> ((void*)firstGlyph, count));

		/// <summary>Gets the vertical advance heights of multiple glyphs.</summary>
		/// <param name="glyphs">The span of glyph IDs.</param>
		/// <returns>An array of vertical advances in font units.</returns>
		/// <remarks />
		public int[] GetVerticalGlyphAdvances (ReadOnlySpan<uint> glyphs) =>
			Glyphs.VAdvances (snapshot.Light, glyphs.ToArray ()).Advances.ToArray ();

		/// <summary>Gets the vertical advance heights of multiple glyphs.</summary>
		/// <param name="firstGlyph">A pointer to the first glyph ID.</param>
		/// <param name="count">The number of glyphs.</param>
		/// <returns>An array of vertical advances in font units.</returns>
		/// <remarks />
		public unsafe int[] GetVerticalGlyphAdvances (IntPtr firstGlyph, int count) =>
			GetVerticalGlyphAdvances (new ReadOnlySpan<uint> ((void*)firstGlyph, count));

		/// <summary>Tries to get the horizontal origin of a glyph.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <param name="xOrigin">Returns the X coordinate of the origin.</param>
		/// <param name="yOrigin">Returns the Y coordinate of the origin.</param>
		/// <returns><see langword="true" /> if the origin was found; otherwise, <see langword="false" />.</returns>
		/// <remarks>The origin is the point from which the glyph is drawn for horizontal text.</remarks>
		public bool TryGetHorizontalGlyphOrigin (uint glyph, out int xOrigin, out int yOrigin)
		{
			var (found, x, y, _) = Glyphs.HOrigin (snapshot.Light, glyph);
			xOrigin = x;
			yOrigin = y;
			return found;
		}

		/// <summary>Tries to get the vertical origin of a glyph.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <param name="xOrigin">Returns the X coordinate of the origin.</param>
		/// <param name="yOrigin">Returns the Y coordinate of the origin.</param>
		/// <returns><see langword="true" /> if the origin was found; otherwise, <see langword="false" />.</returns>
		/// <remarks>The origin is the point from which the glyph is drawn for vertical text.</remarks>
		public bool TryGetVerticalGlyphOrigin (uint glyph, out int xOrigin, out int yOrigin)
		{
			var (found, x, y, failure) = Glyphs.VOrigin (snapshot.Full, glyph);
			if (failure.Length != 0)
				throw new NotSupportedException (failure);

			xOrigin = x;
			yOrigin = y;
			return found;
		}

		/// <summary>Gets the horizontal kerning adjustment between two glyphs.</summary>
		/// <param name="leftGlyph">The glyph ID of the left glyph in the pair.</param>
		/// <param name="rightGlyph">The glyph ID of the right glyph in the pair.</param>
		/// <returns>The kerning adjustment in font units.</returns>
		/// <remarks>Kerning adjusts the spacing between specific pairs of glyphs for better visual appearance.</remarks>
		public int GetHorizontalGlyphKerning (uint leftGlyph, uint rightGlyph) =>
			Glyphs.Kerning (snapshot.Light, leftGlyph, rightGlyph);

		/// <summary>Tries to get the extents (bounding box) of a glyph.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <param name="extents">Returns the extents of the glyph.</param>
		/// <returns><see langword="true" /> if the extents were found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyphExtents (uint glyph, out GlyphExtents extents)
		{
			var (found, value, failure) = Glyphs.Extents (snapshot.Full, glyph);
			if (failure.Length != 0)
				throw new NotSupportedException (failure);

			extents = new GlyphExtents { XBearing = value.XBearing, YBearing = value.YBearing, Width = value.Width, Height = value.Height };
			return found;
		}

		/// <summary>Tries to get the coordinates of a contour point in a glyph.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <param name="pointIndex">The index of the contour point.</param>
		/// <param name="x">Returns the X coordinate of the point.</param>
		/// <param name="y">Returns the Y coordinate of the point.</param>
		/// <returns><see langword="true" /> if the point was found; otherwise, <see langword="false" />.</returns>
		/// <remarks>Contour points are used for TrueType hinting.</remarks>
		public bool TryGetGlyphContourPoint (uint glyph, uint pointIndex, out int x, out int y)
		{
			var (found, px, py) = Glyphs.ContourPoint (snapshot.Light, glyph, pointIndex);
			x = px;
			y = py;
			return found;
		}

		/// <summary>Tries to get the name of a glyph.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <param name="name">Returns the glyph name if available.</param>
		/// <returns><see langword="true" /> if the name was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyphName (uint glyph, out string name)
		{
			var (found, value) = Glyphs.Name (snapshot.Light, glyph);
			name = value;
			return found;
		}

		/// <summary>Tries to get the glyph ID for a glyph name.</summary>
		/// <param name="name">The glyph name.</param>
		/// <param name="glyph">Returns the glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyphFromName (string name, out uint glyph)
		{
			var (found, value) = Glyphs.FromName (snapshot.Light, name);
			glyph = value;
			return found;
		}

		/// <summary>Tries to get the glyph ID for a Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="glyph">Returns the glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyph (int unicode, out uint glyph) =>
			TryGetGlyph ((uint)unicode, 0, out glyph);

		/// <summary>Tries to get the glyph ID for a Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="glyph">Returns the glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyph (uint unicode, out uint glyph) =>
			TryGetGlyph (unicode, 0, out glyph);

		/// <summary>Tries to get the glyph ID for a Unicode code point with a variation selector.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="variationSelector">The variation selector code point.</param>
		/// <param name="glyph">Returns the glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyph (int unicode, uint variationSelector, out uint glyph) =>
			TryGetGlyph ((uint)unicode, variationSelector, out glyph);

		/// <summary>Tries to get the glyph ID for a Unicode code point with a variation selector.</summary>
		/// <param name="unicode">The Unicode code point.</param>
		/// <param name="variationSelector">The variation selector code point.</param>
		/// <param name="glyph">Returns the glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyph (uint unicode, uint variationSelector, out uint glyph)
		{
			var (found, value) = Glyphs.Glyph (snapshot.Light, unicode, variationSelector);
			glyph = value;
			return found;
		}

		/// <summary>Gets the font extents for the specified text direction.</summary>
		/// <param name="direction">The text direction.</param>
		/// <returns>The font extents for the given direction.</returns>
		/// <remarks>For horizontal text, returns horizontal extents; for vertical text, returns vertical extents.</remarks>
		public FontExtents GetFontExtentsForDirection (Direction direction) =>
			ToExtents (Fonts.ExtentsForDirection (snapshot.Light, (int)direction));

		/// <summary>Gets the advance of a glyph for the specified direction.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <param name="direction">The text direction.</param>
		/// <param name="x">Returns the horizontal advance.</param>
		/// <param name="y">Returns the vertical advance.</param>
		/// <remarks>For horizontal text, x contains the advance; for vertical text, y contains the advance.</remarks>
		public void GetGlyphAdvanceForDirection (uint glyph, Direction direction, out int x, out int y)
		{
			var horizontal = direction == Direction.LeftToRight || direction == Direction.RightToLeft;
			x = horizontal ? GetHorizontalGlyphAdvance (glyph) : 0;
			y = horizontal ? 0 : GetVerticalGlyphAdvance (glyph);
		}

		/// <summary>Gets the advances of multiple glyphs for the specified direction.</summary>
		/// <param name="glyphs">The span of glyph IDs.</param>
		/// <param name="direction">The text direction.</param>
		/// <returns>An array of advances for the glyphs.</returns>
		/// <remarks />
		public int[] GetGlyphAdvancesForDirection (ReadOnlySpan<uint> glyphs, Direction direction) =>
			direction == Direction.LeftToRight || direction == Direction.RightToLeft ? GetHorizontalGlyphAdvances (glyphs) : GetVerticalGlyphAdvances (glyphs);

		/// <summary>Gets the advances of multiple glyphs for the specified direction.</summary>
		/// <param name="firstGlyph">A pointer to the first glyph ID.</param>
		/// <param name="count">The number of glyphs.</param>
		/// <param name="direction">The text direction.</param>
		/// <returns>An array of advances for the glyphs.</returns>
		/// <remarks />
		public unsafe int[] GetGlyphAdvancesForDirection (IntPtr firstGlyph, int count, Direction direction) =>
			GetGlyphAdvancesForDirection (new ReadOnlySpan<uint> ((void*)firstGlyph, count), direction);

		/// <summary>Tries to get the coordinates of a contour point in a glyph, adjusted for text direction.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <param name="pointIndex">The index of the contour point.</param>
		/// <param name="direction">The text direction for origin adjustment.</param>
		/// <param name="x">Returns the X coordinate of the point.</param>
		/// <param name="y">Returns the Y coordinate of the point.</param>
		/// <returns><see langword="true" /> if the point was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetGlyphContourPointForOrigin (uint glyph, uint pointIndex, Direction direction, out int x, out int y)
		{
			if (!TryGetGlyphContourPoint (glyph, pointIndex, out x, out y)) {
				return false;
			}

			var horizontal = direction == Direction.LeftToRight || direction == Direction.RightToLeft;
			var found = horizontal ? TryGetHorizontalGlyphOrigin (glyph, out var originX, out var originY) : TryGetVerticalGlyphOrigin (glyph, out originX, out originY);
			if (found) {
				x -= originX;
				y -= originY;
			}

			return true;
		}

		/// <summary>Converts a glyph ID to its string representation.</summary>
		/// <param name="glyph">The glyph ID.</param>
		/// <returns>The string representation of the glyph, typically its name if available, otherwise its ID.</returns>
		/// <remarks />
		public string GlyphToString (uint glyph) =>
			Glyphs.ToText (snapshot.Light, glyph);

		/// <summary>Tries to parse a string to get a glyph ID.</summary>
		/// <param name="s">The string representation of the glyph (name or ID).</param>
		/// <param name="glyph">Returns the glyph ID if found.</param>
		/// <returns><see langword="true" /> if the glyph was found; otherwise, <see langword="false" />.</returns>
		/// <remarks>The string can be a glyph name or a numeric glyph ID.</remarks>
		public bool TryGetGlyphFromString (string s, out uint glyph)
		{
			var (found, value) = Glyphs.FromText (snapshot.Light, s);
			glyph = value;
			return found;
		}

		/// <summary>Sets the variation coordinates for this font using an array of tag-value pairs.</summary>
		/// <param name="variations">A read-only span of <see cref="T:HarfBuzzSharp.Variation" /> values specifying the axis tag and design-space value for each axis to set.</param>
		/// <remarks />
		public void SetVariations (ReadOnlySpan<Variation> variations)
		{
			var tags = new uint[variations.Length];
			var values = new float[variations.Length];
			for (var position = 0; position < tags.Length; position++) {
				tags[position] = variations[position].Tag;
				values[position] = variations[position].Value;
			}

			snapshot = snapshot.WithVariation (font => Variations.Set (font, tags, values));
		}

		/// <summary>Sets the variation coordinates for this font using design-space values.</summary>
		/// <param name="coords">A read-only span of design-space coordinate values, one per variation axis.</param>
		/// <remarks />
		public void SetVariationCoordsDesign (ReadOnlySpan<float> coords)
		{
			var copy = coords.ToArray ();
			snapshot = snapshot.WithVariation (font => Variations.SetDesign (font, copy));
		}

		/// <summary>Sets the variation coordinates for this font using normalized values.</summary>
		/// <param name="coords">A read-only span of normalized coordinate values in the range [-16384, 16384], one per variation axis.</param>
		/// <remarks />
		public void SetVariationCoordsNormalized (ReadOnlySpan<int> coords)
		{
			var copy = coords.ToArray ();
			snapshot = snapshot.WithVariation (font => Variations.SetNormalized (font, copy));
		}

		/// <summary>Gets the current normalized variation coordinates for this font.</summary>
		/// <value>An array of normalized variation coordinate values in the range [-16384, 16384], one per variation axis.</value>
		/// <remarks />
		public int[] VariationCoordsNormalized
		{
			get => Variations.Normalized (snapshot.Light).ToArray ();
		}

		/// <summary>Fills a span with the current normalized variation coordinates for this font.</summary>
		/// <param name="coords">A span to receive the normalized coordinate values in the range [-16384, 16384].</param>
		/// <returns>The number of normalized coordinates written to <paramref name="coords" />.</returns>
		/// <remarks />
		public int GetVariationCoordsNormalized (Span<int> coords)
		{
			var current = Variations.Normalized (snapshot.Light);
			var count = Math.Min (current.Length, coords.Length);
			for (var position = 0; position < count; position++) {
				coords[position] = current[position];
			}

			return count;
		}

		/// <summary>Sets the variation coordinates for this font to those of the specified named instance.</summary>
		/// <param name="instanceIndex">The zero-based index of the named instance to apply.</param>
		/// <remarks />
		public void SetVariationNamedInstance (int instanceIndex)
		{
			if (instanceIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (instanceIndex));
			snapshot = snapshot.WithVariation (font => Variations.SetNamedInstance (font, instanceIndex));
		}

		/// <summary>Sets the font functions to that of OpenType.</summary>
		/// <remarks />
		public void SetFunctionsOpenType ()
		{
			snapshot = snapshot.WithFunctions (null);
			Interlocked.Exchange (ref fontFunctionsDestroy, null)?.Invoke ();
		}

		/// <summary>Shapes the specified buffer using the current font.</summary>
		/// <param name="buffer">The buffer to shape.</param>
		/// <param name="features">The features to control the shaping process.</param>
		/// <remarks />
		public void Shape (Buffer buffer, params Feature[] features) =>
			Shape (buffer, features, null);

		/// <summary>Shapes the specified buffer using the current font with explicit shapers.</summary>
		/// <param name="buffer">The buffer to shape.</param>
		/// <param name="features">The features to control the shaping process, or <see langword="null" />.</param>
		/// <param name="shapers">A list of shaper names to try, or <see langword="null" /> for the default.</param>
		/// <remarks>Shapers determine which text shaping engine is used (e.g., "ot" for OpenType, "fallback" for the fallback shaper).</remarks>
		public void Shape (Buffer buffer, IReadOnlyList<Feature> features, IReadOnlyList<string> shapers)
		{
			if (buffer == null)
				throw new ArgumentNullException (nameof (buffer));

			if (buffer.Direction == Direction.Invalid)
				throw new InvalidOperationException ("Buffer's Direction must be valid.");

			if (buffer.ContentType != ContentType.Unicode) {
				throw new InvalidOperationException ("Buffer's ContentType must of type Unicode.");
			}

			if (buffer.Length == 0) {
				return;
			}

			var featuresArray = features as Feature[] ?? features?.ToArray () ?? Array.Empty<Feature> ();
			var shapersArray = shapers?.Count > 0 ? shapers.ToArray () : null;
			buffer.ShapeWith (snapshot, featuresArray, shapersArray);
		}

		/// <summary>Releases the unmanaged resources used by the <see cref="T:HarfBuzzSharp.Font" /> and optionally releases the managed resources.</summary>
		/// <param name="disposing"><see langword="true" /> to release both managed and unmanaged resources; <see langword="false" /> to release only unmanaged resources.</param>
		/// <remarks>Always dispose the object before you release your last reference to the <see cref="T:HarfBuzzSharp.Font" />. Otherwise, the resources it is using will not be freed until the garbage collector calls the finalizer.</remarks>
		protected override void Dispose (bool disposing)
		{
			if (disposing) {
				Interlocked.Exchange (ref fontFunctionsDestroy, null)?.Invoke ();
				face.ReleaseFont ();
			}

			base.Dispose (disposing);
		}

		/// <summary>Releases the unmanaged resources used.</summary>
		/// <remarks />
		protected override void DisposeHandler ()
		{
		}

		internal FontSnapshot Snapshot => snapshot;

		private static FontExtents ToExtents (Farfbuzz.FontExtents value) =>
			new FontExtents { Ascender = value.Ascender, Descender = value.Descender, LineGap = value.LineGap };
	}
}
