#nullable disable

using System;
using System.Collections.Immutable;
using System.Threading;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>Represents a typeface.</summary>
	/// <remarks />
	public class Face : NativeObject
	{
		private static readonly Lazy<Face> emptyFace = new Lazy<Face> (() => new StaticFace ());

		private readonly ReadOnlyMemory<byte> file;
		private readonly GetTableDelegate getTable;
		private ReleaseDelegate destroy;
		private readonly Lazy<FrozenFace> frozen;
		private Lazy<Farfbuzz.Face> raw;
		private int index;
		private int upem;
		private int glyphCount = -1;
		private int fontCount;
		private bool immutable;
		private bool disposed;

		/// <summary>Gets a reference to the empty <see cref="T:HarfBuzzSharp.Face" /> instance.</summary>
		/// <value>The empty <see cref="T:HarfBuzzSharp.Face" /> instance.</value>
		/// <remarks />
		public static Face Empty => emptyFace.Value;

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Face" /> class, using the specified typeface blob.</summary>
		/// <param name="blob">The typeface data.</param>
		/// <param name="index">The zero-based face index in a collection.</param>
		/// <remarks />
		public Face (Blob blob, uint index)
			: this (blob, (int)index)
		{
		}

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Face" /> class, using the specified typeface blob.</summary>
		/// <param name="blob">The typeface data.</param>
		/// <param name="index">The zero-based face index in a collection.</param>
		/// <remarks />
		public Face (Blob blob, int index)
		{
			if (blob == null) {
				throw new ArgumentNullException (nameof (blob));
			}

			if (index < 0) {
				throw new ArgumentOutOfRangeException (nameof (index), "Index must be non negative.");
			}

			file = blob.Bytes;
			this.index = index;
			raw = RawFace ();
			frozen = new Lazy<FrozenFace> (FreezeBlob, LazyThreadSafetyMode.ExecutionAndPublication);
		}

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Face" /> class, using the delegate to assemble the data.</summary>
		/// <param name="getTable">The delegate to retrieve the table data.</param>
		/// <remarks />
		public Face (GetTableDelegate getTable)
			: this (getTable, null)
		{
		}

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Face" /> class, using the delegate to assemble the data.</summary>
		/// <param name="getTable">The delegate to retrieve the table data.</param>
		/// <param name="destroy">The delegate to call when the face is destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public Face (GetTableDelegate getTable, ReleaseDelegate destroy)
		{
			if (getTable == null)
				throw new ArgumentNullException (nameof (getTable));

			this.getTable = getTable;
			this.destroy = destroy;
			frozen = new Lazy<FrozenFace> (FreezeTables, LazyThreadSafetyMode.ExecutionAndPublication);
		}

		private Face ()
		{
			frozen = new Lazy<FrozenFace> (() => new FrozenFace (Faces.Empty), LazyThreadSafetyMode.ExecutionAndPublication);
			raw = new Lazy<Farfbuzz.Face> (() => Faces.Empty);
			immutable = true;
		}

		/// <summary>Gets or sets the zero-based face index in a collection.</summary>
		/// <value>The zero-based face index in a collection.</value>
		/// <remarks />
		public int Index {
			get => index;
			set {
				if (immutable) {
					return;
				}

				index = value;
				if (getTable == null) {
					raw = RawFace ();
				}
			}
		}

		/// <summary>Gets or sets the units per EM.</summary>
		/// <value>The units per EM.</value>
		/// <remarks />
		public int UnitsPerEm {
			get => upem != 0 ? upem : Unfrozen.Upem;
			set {
				if (!immutable) {
					upem = value;
				}
			}
		}

		/// <summary>Gets or sets the number of glyphs in the face.</summary>
		/// <value>The number of glyphs in the face.</value>
		/// <remarks />
		public int GlyphCount {
			get => glyphCount >= 0 ? glyphCount : Unfrozen.GlyphCount;
			set {
				if (!immutable) {
					glyphCount = value;
				}
			}
		}

		/// <summary>Gets the list of OpenType table tags present in this face.</summary>
		/// <value>An array of <see cref="T:HarfBuzzSharp.Tag" /> values representing the tables in the face.</value>
		/// <remarks />
		public Tag[] Tables {
			get {
				if (getTable != null) {
					return Array.Empty<Tag> ();
				}

				ImmutableArray<uint> tags = Faces.TableTags (raw.Value);
				var tables = new Tag[tags.Length];
				for (var position = 0; position < tables.Length; position++) {
					tables[position] = tags[position];
				}

				return tables;
			}
		}

		/// <summary>Retrieves a reference to the specified font table as a blob.</summary>
		/// <param name="table">The tag identifying the table to retrieve.</param>
		/// <returns>A <see cref="T:HarfBuzzSharp.Blob" /> containing the table data, or an empty blob if the table is not found.</returns>
		/// <remarks />
		public Blob ReferenceTable (Tag table)
		{
			if (getTable != null) {
				return getTable (this, table) ?? new Blob (Array.Empty<byte> (), null);
			}

			return new Blob (Faces.Table (raw.Value, table).ToArray (), null);
		}

		/// <summary>Gets a value indicating whether this face is immutable.</summary>
		/// <value><see langword="true" /> if the face is immutable; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool IsImmutable => immutable;

		/// <summary>Makes this face immutable, preventing further modifications.</summary>
		/// <remarks />
		public void MakeImmutable ()
		{
			Freeze ();
		}

		internal FrozenFace Freeze ()
		{
			immutable = true;
			return frozen.Value;
		}

		internal void AddFont ()
		{
			Interlocked.Increment (ref fontCount);
		}

		internal void ReleaseFont ()
		{
			Interlocked.Decrement (ref fontCount);
			Destroy ();
		}

		/// <summary>Releases the unmanaged resources used by the <see cref="T:HarfBuzzSharp.Face" /> and optionally releases the managed resources.</summary>
		/// <param name="disposing"><see langword="true" /> to release both managed and unmanaged resources; <see langword="false" /> to release only unmanaged resources.</param>
		/// <remarks>Always dispose the object before you release your last reference to the <see cref="T:HarfBuzzSharp.Face" />. Otherwise, the resources it is using will not be freed until the garbage collector calls the finalizer.</remarks>
		protected override void Dispose (bool disposing)
		{
			if (disposing) {
				disposed = true;
			}

			base.Dispose (disposing);
		}

		/// <summary>Releases the unmanaged resources used.</summary>
		/// <remarks />
		protected override void DisposeHandler ()
		{
			Destroy ();
		}

		private Farfbuzz.Face Unfrozen => getTable == null ? raw.Value : Freeze ().Core;

		private Lazy<Farfbuzz.Face> RawFace ()
		{
			var snapshot = index;
			var bytes = file;
			return new Lazy<Farfbuzz.Face> (() => Faces.OfBlob (bytes, snapshot, 0, -1), LazyThreadSafetyMode.ExecutionAndPublication);
		}

		private FrozenFace FreezeBlob () =>
			new FrozenFace (Faces.WithOverrides (raw.Value, upem, glyphCount));

		private FrozenFace FreezeTables ()
		{
			var tables = ImmutableDictionary.CreateBuilder<uint, ReadOnlyMemory<byte>> ();
			foreach (var tag in Faces.ShapingTags) {
				using var blob = getTable (this, tag);
				if (blob != null) {
					tables[tag] = blob.Bytes.ToArray ();
				}
			}

			return new FrozenFace (Faces.OfTables (tables.ToImmutable (), index, upem, glyphCount), FetchOutlineTables);
		}

		private ImmutableDictionary<uint, ReadOnlyMemory<byte>> FetchOutlineTables ()
		{
			var tables = ImmutableDictionary.CreateBuilder<uint, ReadOnlyMemory<byte>> ();
			foreach (var tag in Faces.OutlineTags) {
				using var blob = getTable (this, tag);
				if (blob != null) {
					tables[tag] = blob.Bytes.ToArray ();
				}
			}

			return tables.ToImmutable ();
		}

		private void Destroy ()
		{
			if (!disposed || Volatile.Read (ref fontCount) != 0) {
				return;
			}

			Interlocked.Exchange (ref destroy, null)?.Invoke ();
		}

		private class StaticFace : Face
		{
			protected override void Dispose (bool disposing)
			{
			}
		}

		/// <summary>Gets a value indicating whether the font face contains OpenType variation data.</summary>
		/// <value><see langword="true" /> if the face contains OpenType variation data (fvar table); otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool HasVariationData => Variations.HasData (Unfrozen);

		/// <summary>Gets the number of variation axes in the font face.</summary>
		/// <value>The number of variation axes defined in the font's fvar table.</value>
		/// <remarks />
		public int VariationAxisCount => Variations.Axes (Unfrozen).Length;

		/// <summary>Gets information about all variation axes in the font face.</summary>
		/// <value>An array of <see cref="T:HarfBuzzSharp.OpenTypeVarAxisInfo" /> describing each variation axis.</value>
		/// <remarks />
		public OpenTypeVarAxisInfo[] VariationAxisInfos
		{
			get {
				var axes = Variations.Axes (Unfrozen);
				var result = new OpenTypeVarAxisInfo[axes.Length];
				for (var position = 0; position < result.Length; position++) {
					result[position] = ToInfo (axes[position]);
				}

				return result;
			}
		}

		/// <summary>Fills a span with information about the font's variation axes.</summary>
		/// <param name="axes">A span to receive the <see cref="T:HarfBuzzSharp.OpenTypeVarAxisInfo" /> values.</param>
		/// <returns>The number of variation axis infos written to <paramref name="axes" />.</returns>
		/// <remarks />
		public int GetVariationAxisInfos (Span<OpenTypeVarAxisInfo> axes)
		{
			var all = Variations.Axes (Unfrozen);
			var count = Math.Min (all.Length, axes.Length);
			for (var position = 0; position < count; position++) {
				axes[position] = ToInfo (all[position]);
			}

			return count;
		}

		/// <summary>Attempts to find information about the variation axis with the specified tag.</summary>
		/// <param name="tag">The four-byte OpenType tag of the variation axis to find.</param>
		/// <param name="axisInfo">When this method returns, contains the axis information if found. This parameter is treated as uninitialized.</param>
		/// <returns><see langword="true" /> if the axis was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryFindVariationAxis (Tag tag, out OpenTypeVarAxisInfo axisInfo)
		{
			var (found, axis) = Variations.FindAxis (Unfrozen, tag);
			axisInfo = found ? ToInfo (axis) : default;
			return found;
		}

		/// <summary>Gets the number of named instances in the font face.</summary>
		/// <value>The number of named instances defined in the font's fvar table.</value>
		/// <remarks />
		public int NamedInstanceCount => Variations.NamedInstanceCount (Unfrozen);

		/// <summary>Returns the OpenType name ID of the subfamily name for the specified named instance.</summary>
		/// <param name="instanceIndex">The zero-based index of the named instance.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.OpenTypeNameId" /> of the subfamily name string for the named instance.</returns>
		/// <remarks />
		public OpenTypeNameId GetNamedInstanceSubfamilyNameId (int instanceIndex)
		{
			if (instanceIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (instanceIndex));
			return (OpenTypeNameId)Variations.SubfamilyNameId (Unfrozen, instanceIndex);
		}

		/// <summary>Returns the OpenType name ID of the PostScript name for the specified named instance.</summary>
		/// <param name="instanceIndex">The zero-based index of the named instance.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.OpenTypeNameId" /> of the PostScript name string for the named instance.</returns>
		/// <remarks />
		public OpenTypeNameId GetNamedInstancePostScriptNameId (int instanceIndex)
		{
			if (instanceIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (instanceIndex));
			return (OpenTypeNameId)Variations.PostScriptNameId (Unfrozen, instanceIndex);
		}

		/// <summary>Returns the number of design-space coordinates for the specified named instance.</summary>
		/// <param name="instanceIndex">The zero-based index of the named instance.</param>
		/// <returns>The number of design-space coordinates for the named instance.</returns>
		/// <remarks />
		public int GetNamedInstanceDesignCoordsCount (int instanceIndex)
		{
			if (instanceIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (instanceIndex));
			return Variations.NamedInstanceCoords (Unfrozen, instanceIndex).Length;
		}

		/// <summary>Returns the design-space coordinates for the specified named instance.</summary>
		/// <param name="instanceIndex">The zero-based index of the named instance.</param>
		/// <returns>An array of design-space coordinate values for the named instance, one per variation axis.</returns>
		/// <remarks />
		public float[] GetNamedInstanceDesignCoords (int instanceIndex)
		{
			if (instanceIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (instanceIndex));
			return System.Linq.Enumerable.ToArray (Variations.NamedInstanceCoords (Unfrozen, instanceIndex));
		}

		/// <summary>Fills a span with the design-space coordinates for the specified named instance.</summary>
		/// <param name="instanceIndex">The zero-based index of the named instance.</param>
		/// <param name="coords">A span to receive the design-space coordinate values.</param>
		/// <returns>The number of design-space coordinates written to <paramref name="coords" />.</returns>
		/// <remarks />
		public int GetNamedInstanceDesignCoords (int instanceIndex, Span<float> coords)
		{
			if (instanceIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (instanceIndex));
			var all = Variations.NamedInstanceCoords (Unfrozen, instanceIndex);
			var count = Math.Min (all.Length, coords.Length);
			for (var position = 0; position < count; position++) {
				coords[position] = all[position];
			}

			return all.Length == 0 ? 0 : count;
		}

		/// <summary>Gets a value indicating whether the font face contains a CPAL color-palette table.</summary>
		/// <value><see langword="true" /> if the face contains a CPAL color-palette table; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool HasPalettes => Palettes.PaletteCount != 0;

		/// <summary>Gets the number of color palettes in the font face.</summary>
		/// <value>The number of color palettes defined in the font's CPAL table.</value>
		/// <remarks />
		public int PaletteCount => Palettes.PaletteCount;

		/// <summary>Returns all color entries for the specified font palette.</summary>
		/// <param name="paletteIndex">The zero-based index of the palette.</param>
		/// <returns>An array of <see cref="T:HarfBuzzSharp.HBColor" /> values for the palette, in BGRA byte order.</returns>
		/// <remarks />
		public HBColor[] GetPaletteColors (int paletteIndex)
		{
			if (paletteIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (paletteIndex));

			var (total, _) = Colors.PaletteColors (Palettes, paletteIndex, 0, 0);
			if (total == 0)
				return Array.Empty<HBColor> ();

			var (_, found) = Colors.PaletteColors (Palettes, paletteIndex, 0, total);
			var colors = new HBColor[total];
			for (var position = 0; position < found.Length; position++) {
				colors[position] = new HBColor (found[position]);
			}

			return colors;
		}

		/// <summary>Fills a span with the color entries for the specified font palette.</summary>
		/// <param name="paletteIndex">The zero-based index of the palette.</param>
		/// <param name="colors">A span to receive the <see cref="T:HarfBuzzSharp.HBColor" /> values.</param>
		/// <returns>The number of color entries written to <paramref name="colors" />.</returns>
		/// <remarks />
		public int GetPaletteColors (int paletteIndex, Span<HBColor> colors)
		{
			if (paletteIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (paletteIndex));

			var (_, found) = Colors.PaletteColors (Palettes, paletteIndex, 0, colors.Length);
			for (var position = 0; position < found.Length; position++) {
				colors[position] = new HBColor (found[position]);
			}

			return found.Length;
		}

		/// <summary>Returns the flags for the specified font palette.</summary>
		/// <param name="paletteIndex">The zero-based index of the palette.</param>
		/// <returns>A bitwise combination of <see cref="T:HarfBuzzSharp.OpenTypeColorPaletteFlags" /> values describing the palette.</returns>
		/// <remarks />
		public OpenTypeColorPaletteFlags GetPaletteFlags (int paletteIndex)
		{
			if (paletteIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (paletteIndex));
			return (OpenTypeColorPaletteFlags)Colors.PaletteFlags (Palettes, paletteIndex);
		}

		/// <summary>Returns the OpenType name ID of the name string for the specified palette.</summary>
		/// <param name="paletteIndex">The zero-based index of the palette.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.OpenTypeNameId" /> of the palette name string.</returns>
		/// <remarks />
		public OpenTypeNameId GetPaletteNameId (int paletteIndex)
		{
			if (paletteIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (paletteIndex));
			return (OpenTypeNameId)Colors.PaletteNameId (Palettes, paletteIndex);
		}

		/// <summary>Returns the OpenType name ID of the name string for the specified palette color entry.</summary>
		/// <param name="colorIndex">The zero-based index of the color entry within the palette.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.OpenTypeNameId" /> of the color-entry name string.</returns>
		/// <remarks />
		public OpenTypeNameId GetPaletteColorNameId (int colorIndex)
		{
			if (colorIndex < 0)
				throw new ArgumentOutOfRangeException (nameof (colorIndex));
			return (OpenTypeNameId)Colors.ColorNameId (Palettes, colorIndex);
		}

		/// <summary>Gets a value indicating whether the font face contains a COLR color-layers table.</summary>
		/// <value><see langword="true" /> if the face contains a COLR color-layers table; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool HasColorLayers => Colors.HasLayers (TableBytes (ColrTag));

		/// <summary>Gets a value indicating whether the font face contains a PNG color-bitmap table.</summary>
		/// <value><see langword="true" /> if the face contains a CBDT/CBLC PNG color-bitmap table; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool HasColorPng => Colors.HasPng (TableBytes (CbdtTag), TableBytes (SbixTag));

		/// <summary>Gets a value indicating whether the font face contains an SVG color table.</summary>
		/// <value><see langword="true" /> if the face contains an SVG color table; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool HasColorSvg => Colors.HasSvg (TableBytes (SvgTag));

		private readonly System.Collections.Concurrent.ConcurrentDictionary<uint, ReadOnlyMemory<byte>> colorTables = new System.Collections.Concurrent.ConcurrentDictionary<uint, ReadOnlyMemory<byte>> ();
		private static readonly uint ColrTag = new Tag ('C', 'O', 'L', 'R');
		private static readonly uint CpalTag = new Tag ('C', 'P', 'A', 'L');
		private static readonly uint CbdtTag = new Tag ('C', 'B', 'D', 'T');
		private static readonly uint SbixTag = new Tag ('s', 'b', 'i', 'x');
		private static readonly uint SvgTag = new Tag ('S', 'V', 'G', ' ');

		private Farfbuzz.OpenType.Cpal Palettes => Colors.Palettes (TableBytes (CpalTag));

		private ReadOnlyMemory<byte> TableBytes (uint tag) =>
			colorTables.GetOrAdd (tag, FetchTable);

		private ReadOnlyMemory<byte> FetchTable (uint tag)
		{
			if (getTable == null) {
				return Faces.Table (raw.Value, tag);
			}

			using var blob = getTable (this, tag);
			return blob == null ? ReadOnlyMemory<byte>.Empty : blob.Bytes.ToArray ();
		}

		private static OpenTypeVarAxisInfo ToInfo (AxisRow axis)
		{
			var info = new OpenTypeVarAxisInfo ();
			info.AxisIndex = (uint)axis.AxisIndex;
			info.Tag = axis.Tag;
			info.NameId = (OpenTypeNameId)axis.NameId;
			info.Flags = (OpenTypeVarAxisFlags)axis.Flags;
			info.MinValue = axis.MinValue;
			info.DefaultValue = axis.DefaultValue;
			info.MaxValue = axis.MaxValue;
			return info;
		}
	}
}
