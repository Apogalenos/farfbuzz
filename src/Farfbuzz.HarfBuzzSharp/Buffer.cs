#nullable disable

using System;
using System.Buffers;
using System.Runtime.InteropServices;
using System.Text;
using System.Collections.Immutable;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>Represents a buffer of text or glyphs to be shaped.</summary>
	/// <remarks />
	public class Buffer : NativeObject
	{
		private const int ContextLength = 5;

		private GlyphInfo[] info = Array.Empty<GlyphInfo> ();
		private GlyphPosition[] pos = Array.Empty<GlyphPosition> ();
		private int length;
		private ContentType contentType;
		private bool havePositions;
		private uint replacement = DefaultReplacementCodepoint;
		private readonly uint[] preContext = new uint[ContextLength];
		private int preLength;
		private readonly uint[] postContext = new uint[ContextLength];
		private int postLength;
		private Direction direction;
		private Script script;
		private string language;
		private BufferFlags flags;
		private ClusterLevel clusterLevel;
		private uint invisible;
		private UnicodeFunctions unicodeFunctions = UnicodeFunctions.Default;
		private uint randomState = 1;
		private bool haveCoreProperties;
		private Direction coreDirection;
		private Script coreScript;
		private string coreLanguage;
		private Farfbuzz.SegmentProperties coreProperties;

		/// <summary>The default replacement code point (U+FFFD) used for invalid characters.</summary>
		/// <remarks />
		public const int DefaultReplacementCodepoint = '\uFFFD';

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Buffer" /> class with default values.</summary>
		/// <remarks />
		public Buffer ()
		{
		}

		/// <summary>Gets or sets the type of content in the buffer.</summary>
		/// <value>The content type.</value>
		/// <remarks>Buffers can contain either Unicode characters (before shaping) or glyphs (after shaping).</remarks>
		public ContentType ContentType {
			get => contentType;
			set => contentType = value;
		}

		/// <summary>Gets or sets the text flow direction of the buffer.</summary>
		/// <value>One of the enumeration values that specifies the text flow direction.</value>
		/// <remarks>No shaping can happen without setting the direction, or invoking <see cref="M:HarfBuzzSharp.Buffer.GuessSegmentProperties" />.</remarks>
		public Direction Direction {
			get => direction;
			set => direction = value;
		}

		/// <summary>Gets or sets the language of the text in the buffer.</summary>
		/// <value>The language, or <see langword="null" /> if not set.</value>
		/// <remarks>Language is used by some OpenType features to customize shaping behavior.</remarks>
		public Language Language {
			get => new Language (language);
			set => language = value.Name;
		}

		/// <summary>Gets or sets the buffer flags that control shaping behavior.</summary>
		/// <value>The buffer flags.</value>
		/// <remarks />
		public BufferFlags Flags {
			get => flags;
			set => flags = value;
		}

		/// <summary>Gets or sets the cluster level of the buffer.</summary>
		/// <value>The cluster level.</value>
		/// <remarks>The cluster level dictates how HarfBuzz will treat non-base characters during shaping.</remarks>
		public ClusterLevel ClusterLevel {
			get => clusterLevel;
			set => clusterLevel = value;
		}

		/// <summary>Gets or sets the code point used to replace invalid characters during text processing.</summary>
		/// <value>The replacement code point. The default is <see cref="F:HarfBuzzSharp.Buffer.DefaultReplacementCodepoint" /> (U+FFFD).</value>
		/// <remarks />
		public uint ReplacementCodepoint {
			get => replacement;
			set => replacement = value;
		}

		/// <summary>Gets or sets the glyph ID used for invisible characters.</summary>
		/// <value>The glyph ID for invisible characters, or 0 to use the default behavior.</value>
		/// <remarks>This glyph is used for characters marked as invisible in the Unicode database.</remarks>
		public uint InvisibleGlyph {
			get => invisible;
			set => invisible = value;
		}

		/// <summary>Gets or sets the script of the text in the buffer.</summary>
		/// <value>The script.</value>
		/// <remarks>Script determines which OpenType features are applied during shaping.</remarks>
		public Script Script {
			get => script;
			set => script = value;
		}

		/// <summary>Gets or sets the size of the buffer.</summary>
		/// <value>The size of the buffer.</value>
		/// <remarks>If the new length is greater that the current length, more memory will be allocated. If the new length is less than the current length, the extra items will be cleared.</remarks>
		public int Length {
			get => length;
			set => SetLength (value);
		}

		/// <summary>Gets or sets the Unicode functions used by the buffer.</summary>
		/// <value>The Unicode functions.</value>
		/// <remarks>Unicode functions provide character property lookups needed during shaping.</remarks>
		public UnicodeFunctions UnicodeFunctions {
			get => unicodeFunctions;
			set => unicodeFunctions = value.Handle == IntPtr.Zero ? UnicodeFunctions.Default : value;
		}

		/// <summary>Gets the buffer glyph information array.</summary>
		/// <value>An array of <see cref="T:HarfBuzzSharp.GlyphInfo" /> values.</value>
		/// <remarks>The information is valid as long as buffer contents are not modified.</remarks>
		public GlyphInfo[] GlyphInfos {
			get => GetGlyphInfoSpan ().ToArray ();
		}

		/// <summary>Gets the buffer glyph position array.</summary>
		/// <value>An array of <see cref="T:HarfBuzzSharp.GlyphPosition" /> values.</value>
		/// <remarks>The positions are valid as long as buffer contents are not modified.</remarks>
		public GlyphPosition[] GlyphPositions {
			get => GetGlyphPositionSpan ().ToArray ();
		}

		/// <summary>Appends a character with the Unicode value and gives it the initial cluster value.</summary>
		/// <param name="codepoint">The Unicode code point.</param>
		/// <param name="cluster">The cluster value of the code point.</param>
		/// <remarks>This function does not check the validity of the codepoint.</remarks>
		public void Add (int codepoint, int cluster) => Add ((uint)codepoint, (uint)cluster);

		/// <summary>Appends a character with the Unicode value and gives it the initial cluster value.</summary>
		/// <param name="codepoint">The Unicode code point.</param>
		/// <param name="cluster">The cluster value of the code point.</param>
		/// <remarks>This function does not check the validity of the codepoint.</remarks>
		public void Add (uint codepoint, uint cluster)
		{
			if (Length != 0 && ContentType != ContentType.Unicode)
				throw new InvalidOperationException ("Non empty buffer's ContentType must be of type Unicode.");
			if (ContentType == ContentType.Glyphs)
				throw new InvalidOperationException ("ContentType must not be of type Glyphs");

			Ensure (length + 1);
			info[length] = new GlyphInfo { Codepoint = codepoint, Cluster = cluster };
			length++;
			postLength = 0;
		}

		/// <summary>Appends the specified text to the buffer.</summary>
		/// <param name="utf8text">The array of UTF-8 characters to append.</param>
		/// <remarks />
		public void AddUtf8 (string utf8text)
		{
			if (utf8text == null)
				throw new ArgumentNullException (nameof (utf8text));

			var maxByteCount = Encoding.UTF8.GetMaxByteCount (utf8text.Length);
			var utf8bytes = ArrayPool<byte>.Shared.Rent (maxByteCount);
			try {
				var byteCount = Encoding.UTF8.GetBytes (utf8text, 0, utf8text.Length, utf8bytes, 0);
				AddUtf8 (new ReadOnlySpan<byte> (utf8bytes, 0, byteCount), 0, -1);
			} finally {
				ArrayPool<byte>.Shared.Return (utf8bytes);
			}
		}

		/// <summary>Appends the specified text bytes to the buffer.</summary>
		/// <param name="bytes">The array of UTF-8 character bytes to append.</param>
		/// <remarks />
		public void AddUtf8 (byte[] bytes) => AddUtf8 (new ReadOnlySpan<byte> (bytes));

		/// <summary>Appends UTF-8 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-8 encoded bytes to append.</param>
		/// <remarks />
		public void AddUtf8 (ReadOnlySpan<byte> text) => AddUtf8 (text, 0, -1);

		/// <summary>Appends a range of UTF-8 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-8 encoded bytes to append.</param>
		/// <param name="itemOffset">The offset of the first byte to add to the buffer.</param>
		/// <param name="itemLength">The number of bytes to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public void AddUtf8 (ReadOnlySpan<byte> text, int itemOffset, int itemLength)
		{
			CheckAdd (itemOffset);
			var (_, count) = Text.Clamp (text.Length, itemOffset, itemLength);
			if (count > int.MaxValue / 8)
				return;

			Ensure (length + count);
			Commit (Text.AddUtf8 (text, itemOffset, itemLength, replacement, length == 0, new BufferSink (info), length));
		}

		/// <summary>Appends UTF-8 encoded text to the buffer.</summary>
		/// <param name="text">A pointer to UTF-8 encoded text.</param>
		/// <param name="textLength">The length of the text in bytes.</param>
		/// <remarks />
		public void AddUtf8 (IntPtr text, int textLength) => AddUtf8 (text, textLength, 0, -1);

		/// <summary>Appends a range of UTF-8 encoded text to the buffer.</summary>
		/// <param name="text">A pointer to UTF-8 encoded text.</param>
		/// <param name="textLength">The length of the text in bytes.</param>
		/// <param name="itemOffset">The offset of the first byte to add to the buffer.</param>
		/// <param name="itemLength">The number of bytes to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public unsafe void AddUtf8 (IntPtr text, int textLength, int itemOffset, int itemLength) =>
			AddUtf8 (textLength == -1 ? MemoryMarshal.CreateReadOnlySpanFromNullTerminated ((byte*)text) : new ReadOnlySpan<byte> ((void*)text, textLength), itemOffset, itemLength);

		/// <summary>Appends UTF-16 encoded text to the buffer.</summary>
		/// <param name="text">The UTF-16 string to append.</param>
		/// <remarks />
		public void AddUtf16 (string text) => AddUtf16 (text, 0, -1);

		/// <summary>Appends a range of UTF-16 encoded text to the buffer.</summary>
		/// <param name="text">The UTF-16 string to append.</param>
		/// <param name="itemOffset">The offset of the first character to add to the buffer.</param>
		/// <param name="itemLength">The number of characters to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public void AddUtf16 (string text, int itemOffset, int itemLength) =>
			AddUtf16 (text.AsSpan (0, text.Length), itemOffset, itemLength);

		/// <summary>Appends UTF-16 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-16 encoded bytes to append.</param>
		/// <remarks />
		public void AddUtf16 (ReadOnlySpan<byte> text) =>
			AddUtf16 (MemoryMarshal.Cast<byte, char> (text), 0, -1);

		/// <summary>Appends UTF-16 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-16 characters to append.</param>
		/// <remarks />
		public void AddUtf16 (ReadOnlySpan<char> text) => AddUtf16 (text, 0, -1);

		/// <summary>Appends a range of UTF-16 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-16 characters to append.</param>
		/// <param name="itemOffset">The offset of the first character to add to the buffer.</param>
		/// <param name="itemLength">The number of characters to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public void AddUtf16 (ReadOnlySpan<char> text, int itemOffset, int itemLength)
		{
			CheckAdd (itemOffset);
			var (_, count) = Text.Clamp (text.Length, itemOffset, itemLength);
			if (count > int.MaxValue / 8)
				return;

			Ensure (length + count);
			Commit (Text.AddUtf16 (text, itemOffset, itemLength, replacement, length == 0, new BufferSink (info), length));
		}

		/// <summary>Appends UTF-16 encoded text to the buffer.</summary>
		/// <param name="text">A pointer to UTF-16 encoded text.</param>
		/// <param name="textLength">The length of the text in UTF-16 code units.</param>
		/// <remarks />
		public void AddUtf16 (IntPtr text, int textLength) =>
			AddUtf16 (text, textLength, 0, -1);

		/// <summary>Appends a range of UTF-16 encoded text to the buffer.</summary>
		/// <param name="text">A pointer to UTF-16 encoded text.</param>
		/// <param name="textLength">The length of the text in UTF-16 code units.</param>
		/// <param name="itemOffset">The offset of the first character to add to the buffer.</param>
		/// <param name="itemLength">The number of characters to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public unsafe void AddUtf16 (IntPtr text, int textLength, int itemOffset, int itemLength) =>
			AddUtf16 (textLength == -1 ? MemoryMarshal.CreateReadOnlySpanFromNullTerminated ((char*)text) : new ReadOnlySpan<char> ((void*)text, textLength), itemOffset, itemLength);

		/// <summary>Appends the string as UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">The string to append as UTF-32 encoded text.</param>
		/// <remarks />
		public void AddUtf32 (string text) => AddUtf32 (Encoding.UTF32.GetBytes (text));

		/// <summary>Appends UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-32 encoded bytes to append.</param>
		/// <remarks />
		public void AddUtf32 (ReadOnlySpan<byte> text) =>
			AddUtf32 (MemoryMarshal.Cast<byte, uint> (text), 0, -1);

		/// <summary>Appends UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-32 code points to append.</param>
		/// <remarks />
		public void AddUtf32 (ReadOnlySpan<uint> text) => AddUtf32 (text, 0, -1);

		/// <summary>Appends a range of UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-32 code points to append.</param>
		/// <param name="itemOffset">The offset of the first code point to add to the buffer.</param>
		/// <param name="itemLength">The number of code points to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public void AddUtf32 (ReadOnlySpan<uint> text, int itemOffset, int itemLength)
		{
			CheckAdd (itemOffset);
			var (_, count) = Text.Clamp (text.Length, itemOffset, itemLength);
			if (count > int.MaxValue / 8)
				return;

			Ensure (length + count);
			Commit (Text.AddUtf32 (text, itemOffset, itemLength, replacement, length == 0, new BufferSink (info), length));
		}

		/// <summary>Appends UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-32 code points to append.</param>
		/// <remarks />
		public void AddUtf32 (ReadOnlySpan<int> text) => AddUtf32 (text, 0, -1);

		/// <summary>Appends a range of UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">The span of UTF-32 code points to append.</param>
		/// <param name="itemOffset">The offset of the first code point to add to the buffer.</param>
		/// <param name="itemLength">The number of code points to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public void AddUtf32 (ReadOnlySpan<int> text, int itemOffset, int itemLength) =>
			AddUtf32 (MemoryMarshal.Cast<int, uint> (text), itemOffset, itemLength);

		/// <summary>Appends UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">A pointer to UTF-32 encoded text.</param>
		/// <param name="textLength">The length of the text in UTF-32 code units.</param>
		/// <remarks />
		public void AddUtf32 (IntPtr text, int textLength) =>
			AddUtf32 (text, textLength, 0, -1);

		/// <summary>Appends a range of UTF-32 encoded text to the buffer.</summary>
		/// <param name="text">A pointer to UTF-32 encoded text.</param>
		/// <param name="textLength">The length of the text in UTF-32 code units.</param>
		/// <param name="itemOffset">The offset of the first code point to add to the buffer.</param>
		/// <param name="itemLength">The number of code points to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks />
		public unsafe void AddUtf32 (IntPtr text, int textLength, int itemOffset, int itemLength) =>
			AddUtf32 (textLength == -1 ? NullTerminated ((uint*)text) : new ReadOnlySpan<uint> ((void*)text, textLength), itemOffset, itemLength);

		/// <summary>Appends characters from the span to the buffer.</summary>
		/// <param name="text">The span of Unicode code points to append.</param>
		/// <remarks>This function does not check the validity of the characters.</remarks>
		public void AddCodepoints (ReadOnlySpan<uint> text) => AddCodepoints (text, 0, -1);

		/// <summary>Appends characters from the span to the buffer.</summary>
		/// <param name="text">The span of Unicode code points to append.</param>
		/// <param name="itemOffset">The offset of the first code point to add to the buffer.</param>
		/// <param name="itemLength">The number of code points to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks>This function does not check the validity of the characters.</remarks>
		public void AddCodepoints (ReadOnlySpan<uint> text, int itemOffset, int itemLength)
		{
			CheckAdd (itemOffset);
			var (_, count) = Text.Clamp (text.Length, itemOffset, itemLength);
			if (count > int.MaxValue / 8)
				return;

			Ensure (length + count);
			Commit (Text.AddCodepoints (text, itemOffset, itemLength, replacement, length == 0, new BufferSink (info), length));
		}

		/// <summary>Appends characters from the span to the buffer.</summary>
		/// <param name="text">The span of Unicode code points to append.</param>
		/// <remarks>This function does not check the validity of the characters.</remarks>
		public void AddCodepoints (ReadOnlySpan<int> text) => AddCodepoints (text, 0, -1);

		/// <summary>Appends characters from the span to the buffer.</summary>
		/// <param name="text">The span of Unicode code points to append.</param>
		/// <param name="itemOffset">The offset of the first code point to add to the buffer.</param>
		/// <param name="itemLength">The number of code points to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks>This function does not check the validity of the characters.</remarks>
		public void AddCodepoints (ReadOnlySpan<int> text, int itemOffset, int itemLength) =>
			AddCodepoints (MemoryMarshal.Cast<int, uint> (text), itemOffset, itemLength);

		/// <summary>Appends characters from the pointer to the buffer.</summary>
		/// <param name="text">A pointer to an array of Unicode code points.</param>
		/// <param name="textLength">The number of code points in the array.</param>
		/// <remarks>This function does not check the validity of the characters.</remarks>
		public void AddCodepoints (IntPtr text, int textLength) => AddCodepoints (text, textLength, 0, -1);

		/// <summary>Appends characters from the pointer to the buffer.</summary>
		/// <param name="text">A pointer to an array of Unicode code points.</param>
		/// <param name="textLength">The number of code points in the array.</param>
		/// <param name="itemOffset">The offset of the first code point to add to the buffer.</param>
		/// <param name="itemLength">The number of code points to add to the buffer, or -1 for the end of the text.</param>
		/// <remarks>This function does not check the validity of the characters.</remarks>
		public unsafe void AddCodepoints (IntPtr text, int textLength, int itemOffset, int itemLength) =>
			AddCodepoints (textLength == -1 ? NullTerminated ((uint*)text) : new ReadOnlySpan<uint> ((void*)text, textLength), itemOffset, itemLength);

		/// <summary>Gets a span over the glyph information array.</summary>
		/// <returns>A read-only span of glyph information.</returns>
		/// <remarks>The span is valid as long as buffer contents are not modified.</remarks>
		public ReadOnlySpan<GlyphInfo> GetGlyphInfoSpan () =>
			new ReadOnlySpan<GlyphInfo> (info, 0, length);

		/// <summary>Gets a span over the glyph position array.</summary>
		/// <returns>A read-only span of glyph positions.</returns>
		/// <remarks>The span is valid as long as buffer contents are not modified.</remarks>
		public ReadOnlySpan<GlyphPosition> GetGlyphPositionSpan ()
		{
			if (!havePositions) {
				ClearPositions ();
			}

			return new ReadOnlySpan<GlyphPosition> (pos, 0, length);
		}

		/// <summary>Sets the unset buffer segment properties based on the buffer's Unicode contents.</summary>
		/// <remarks />
		public void GuessSegmentProperties ()
		{
			if (ContentType != ContentType.Unicode)
				throw new InvalidOperationException ("ContentType must be of type Unicode.");

			var guessed = Segments.Guess (MemoryMarshal.Cast<GlyphInfo, Farfbuzz.GlyphInfo> (GetGlyphInfoSpan ()), Properties, unicodeFunctions.ToCore ());
			direction = (Direction)guessed.Direction;
			script = guessed.Script;
			language = guessed.Language;
		}

		/// <summary>Clears the buffer's contents.</summary>
		/// <remarks>This operation preserves the Unicode functions and replacement code point.</remarks>
		public void ClearContents ()
		{
			Clear ();
		}

		/// <summary>Resets the buffer to its initial state as if it were newly created.</summary>
		/// <remarks>This clears contents and resets all properties to their default values.</remarks>
		public void Reset ()
		{
			flags = BufferFlags.Default;
			clusterLevel = ClusterLevel.Default;
			unicodeFunctions = UnicodeFunctions.Default;
			replacement = DefaultReplacementCodepoint;
			invisible = 0;
			Clear ();
		}

		/// <summary>Appends the contents of another buffer to this buffer.</summary>
		/// <param name="buffer">The buffer to append.</param>
		/// <remarks />
		public void Append (Buffer buffer) => Append (buffer, 0, -1);

		/// <summary>Appends a range from another buffer to this buffer.</summary>
		/// <param name="buffer">The buffer to append from.</param>
		/// <param name="start">The start index of the range to append.</param>
		/// <param name="end">The end index of the range to append.</param>
		/// <remarks />
		public void Append (Buffer buffer, int start, int end)
		{
			if (buffer.Length == 0)
				throw new ArgumentException ("Buffer must be non empty.", nameof (buffer));
			if (buffer.ContentType != ContentType)
				throw new InvalidOperationException ("ContentType must be of same type.");

			AppendRange (buffer, (uint)start, (uint)(end == -1 ? buffer.Length : end));
		}

		/// <summary>Reorders glyphs so that they can be run through subsequent phases consistently.</summary>
		/// <remarks>This ensures that clusters of glyphs are arranged in a canonical order after shaping.</remarks>
		public void NormalizeGlyphs ()
		{
			if (ContentType != ContentType.Glyphs)
				throw new InvalidOperationException ("ContentType must be of type Glyphs.");
			if (GlyphPositions.Length == 0)
				throw new InvalidOperationException ("GlyphPositions can't be empty.");

			var (infos, positions) = Serialization.Normalize (ImmutableArray.Create (MemoryMarshal.Cast<GlyphInfo, Farfbuzz.GlyphInfo> (new ReadOnlySpan<GlyphInfo> (info, 0, length)).ToArray ()),
				ImmutableArray.Create (MemoryMarshal.Cast<GlyphPosition, Farfbuzz.GlyphPosition> (new ReadOnlySpan<GlyphPosition> (pos, 0, length)).ToArray ()), direction == Direction.RightToLeft || direction == Direction.BottomToTop);
			MemoryMarshal.Cast<Farfbuzz.GlyphInfo, GlyphInfo> (infos.AsSpan ()).CopyTo (info);
			MemoryMarshal.Cast<Farfbuzz.GlyphPosition, GlyphPosition> (positions.AsSpan ()).CopyTo (pos);
		}

		/// <summary>Reverses the order of buffer contents.</summary>
		/// <remarks />
		public void Reverse ()
		{
			ReverseInPlace (0, length);
		}

		/// <summary>Reverses the order of buffer contents within a specified range.</summary>
		/// <param name="start">The start index of the range to reverse.</param>
		/// <param name="end">The end index of the range to reverse.</param>
		/// <remarks />
		public void ReverseRange (int start, int end)
		{
			ReverseInPlace ((int)Math.Min ((uint)start, (uint)length), (int)Math.Min ((uint)(end == -1 ? Length : end), (uint)length));
		}

		/// <summary>Reverses the order of clusters in the buffer.</summary>
		/// <remarks>Within each cluster, glyphs maintain their original order.</remarks>
		public void ReverseClusters ()
		{
			if (length == 0) {
				return;
			}

			var start = 0;
			var i = 1;
			for (; i < length; i++) {
				if (info[i - 1].Cluster != info[i].Cluster) {
					ReverseInPlace (start, i);
					start = i;
				}
			}

			ReverseInPlace (start, i);
			ReverseInPlace (0, length);
		}

		/// <summary>Serializes the buffer glyphs to a string.</summary>
		/// <returns>A string containing the serialized glyph data.</returns>
		/// <remarks />
		public string SerializeGlyphs () =>
			SerializeGlyphs (0, -1, null, SerializeFormat.Text, SerializeFlag.Default);

		/// <summary>Serializes a range of buffer glyphs to a string.</summary>
		/// <param name="start">The start index of the range to serialize.</param>
		/// <param name="end">The end index of the range to serialize.</param>
		/// <returns>A string containing the serialized glyph data.</returns>
		/// <remarks />
		public string SerializeGlyphs (int start, int end) =>
			SerializeGlyphs (start, end, null, SerializeFormat.Text, SerializeFlag.Default);

		/// <summary>Serializes the buffer glyphs to a string using the specified font.</summary>
		/// <param name="font">The font to use for glyph names, or <see langword="null" />.</param>
		/// <returns>A string containing the serialized glyph data.</returns>
		/// <remarks />
		public string SerializeGlyphs (Font font) =>
			SerializeGlyphs (0, -1, font, SerializeFormat.Text, SerializeFlag.Default);

		/// <summary>Serializes the buffer glyphs to a string using the specified font, format, and flags.</summary>
		/// <param name="font">The font to use for glyph names, or <see langword="null" />.</param>
		/// <param name="format">The serialization format.</param>
		/// <param name="flags">Flags that control serialization behavior.</param>
		/// <returns>A string containing the serialized glyph data.</returns>
		/// <remarks />
		public string SerializeGlyphs (Font font, SerializeFormat format, SerializeFlag flags) =>
			SerializeGlyphs (0, -1, font, format, flags);

		/// <summary>Serializes a range of buffer glyphs to a string using the specified font, format, and flags.</summary>
		/// <param name="start">The start index of the range to serialize.</param>
		/// <param name="end">The end index of the range to serialize.</param>
		/// <param name="font">The font to use for glyph names, or <see langword="null" />.</param>
		/// <param name="format">The serialization format.</param>
		/// <param name="flags">Flags that control serialization behavior.</param>
		/// <returns>A string containing the serialized glyph data.</returns>
		/// <remarks />
		public string SerializeGlyphs (int start, int end, Font font, SerializeFormat format, SerializeFlag flags)
		{
			if (Length == 0)
				throw new InvalidOperationException ("Buffer should not be empty.");
			if (ContentType != ContentType.Glyphs)
				throw new InvalidOperationException ("ContentType should be of type Glyphs.");

			if (end == -1)
				end = Length;

			if (start >= end) {
				return string.Empty;
			}

			var infos = ImmutableArray.Create (MemoryMarshal.Cast<GlyphInfo, Farfbuzz.GlyphInfo> (new ReadOnlySpan<GlyphInfo> (info, 0, length)).ToArray ());
			var positions = ImmutableArray.Create (MemoryMarshal.Cast<GlyphPosition, Farfbuzz.GlyphPosition> (GetGlyphPositionSpan ()).ToArray ());
			return Serialization.Serialize (infos, positions, start, end, font == null ? Fonts.EmptyFont : font.Snapshot.Full, (uint)format, (uint)flags);
		}

		/// <summary>Deserializes glyphs from a string and populates the buffer.</summary>
		/// <param name="data">The serialized glyph data.</param>
		/// <remarks />
		public void DeserializeGlyphs (string data) =>
			DeserializeGlyphs (data, null, SerializeFormat.Text);

		/// <summary>Deserializes glyphs from a string and populates the buffer using the specified font.</summary>
		/// <param name="data">The serialized glyph data.</param>
		/// <param name="font">The font to use for resolving glyph names, or <see langword="null" />.</param>
		/// <remarks />
		public void DeserializeGlyphs (string data, Font font) =>
			DeserializeGlyphs (data, font, SerializeFormat.Text);

		/// <summary>Deserializes glyphs from a string and populates the buffer using the specified font and format.</summary>
		/// <param name="data">The serialized glyph data.</param>
		/// <param name="font">The font to use for resolving glyph names, or <see langword="null" />.</param>
		/// <param name="format">The serialization format of the data.</param>
		/// <remarks />
		public void DeserializeGlyphs (string data, Font font, SerializeFormat format)
		{
			if (Length != 0)
				throw new InvalidOperationException ("Buffer must be empty.");
			if (ContentType == ContentType.Glyphs)
				throw new InvalidOperationException ("ContentType must not be Glyphs.");

			if (data.Length == 0) {
				return;
			}

			contentType = ContentType.Glyphs;
			var (parsed, infos, positions) = Serialization.Deserialize (data, font == null ? Fonts.EmptyFont : font.Snapshot.Full, (uint)format);
			if (!parsed && infos.Length == 0) {
				return;
			}

			Ensure (infos.Length);
			if (pos.Length < info.Length) {
				pos = new GlyphPosition[info.Length];
			}

			MemoryMarshal.Cast<Farfbuzz.GlyphInfo, GlyphInfo> (infos.AsSpan ()).CopyTo (info);
			MemoryMarshal.Cast<Farfbuzz.GlyphPosition, GlyphPosition> (positions.AsSpan ()).CopyTo (pos);
			length = infos.Length;
			havePositions = true;
		}

		/// <summary>Releases the unmanaged resources used by the <see cref="T:HarfBuzzSharp.Buffer" /> and optionally releases the managed resources.</summary>
		/// <param name="disposing"><see langword="true" /> to release both managed and unmanaged resources; <see langword="false" /> to release only unmanaged resources.</param>
		/// <remarks>Always dispose the object before you release your last reference to the <see cref="T:HarfBuzzSharp.Buffer" />. Otherwise, the resources it is using will not be freed until the garbage collector calls the finalizer.</remarks>
		protected override void Dispose (bool disposing) =>
			base.Dispose (disposing);

		/// <summary>Releases the unmanaged resources used.</summary>
		/// <remarks />
		protected override void DisposeHandler ()
		{
		}


		private void CheckAdd (int itemOffset)
		{
			if (itemOffset < 0)
				throw new ArgumentOutOfRangeException (nameof (itemOffset), "ItemOffset must be non negative.");
			if (Length != 0 && ContentType != ContentType.Unicode)
				throw new InvalidOperationException ("Non empty buffer's ContentType must be of type Unicode.");
			if (ContentType == ContentType.Glyphs)
				throw new InvalidOperationException ("ContentType must not be of type Glyphs");
		}

		private void Ensure (int needed)
		{
			if (needed <= info.Length) {
				return;
			}

			var capacity = Math.Max (needed, Math.Max (8, info.Length * 2));
			Array.Resize (ref info, capacity);
			if (havePositions || pos.Length != 0) {
				Array.Resize (ref pos, capacity);
			}
		}

		private void Commit (Added<int> added)
		{
			length = added.State;
			if (added.PreInstalled) {
				preLength = Store (added.Pre, preContext);
			}

			postLength = Store (added.Post, postContext);
			contentType = ContentType.Unicode;
		}

		private static int Store (Context context, uint[] target)
		{
			target[0] = context.C0;
			target[1] = context.C1;
			target[2] = context.C2;
			target[3] = context.C3;
			target[4] = context.C4;
			return context.Length;
		}

		private void ClearPositions ()
		{
			if (pos.Length < info.Length) {
				pos = new GlyphPosition[info.Length];
			}

			havePositions = true;
			Array.Clear (pos, 0, length);
		}

		private void Clear ()
		{
			contentType = ContentType.Invalid;
			direction = Direction.Invalid;
			script = Script.Invalid;
			language = null;
			havePositions = false;
			length = 0;
			preLength = 0;
			postLength = 0;
			randomState = 1;
		}

		private Farfbuzz.ShapePlan PlanFor (FontSnapshot snapshot, Farfbuzz.Font font, ReadOnlySpan<Farfbuzz.Feature> called, string[] shapers)
		{
			var plain = called.IsEmpty && shapers == null;
			if (plain) {
				var memo = snapshot.LastPlan;
				if (memo != null && memo.Direction == direction && memo.Script.Equals (script) && string.Equals (memo.Language, language)) {
					return memo.Plan;
				}
			}

			var key = Shaping.PlanKey (font, Properties, called, shapers);
			if (key.IsNone) {
				return null;
			}

			var plan = snapshot.Face.Plan (font, key.Value);
			if (plain) {
				snapshot.LastPlan = new PlanMemo (direction, script, language, plan);
			}

			return plan;
		}

		internal void ShapeWith (FontSnapshot snapshot, Feature[] features, string[] shapers)
		{
			var font = snapshot.Full;
			var called = MemoryMarshal.Cast<Feature, Farfbuzz.Feature> (features);
			var plan = PlanFor (snapshot, font, called, shapers);
			if (plan == null) {
				return;
			}

			var result = ExecuteInto (font, plan, called);
			if (result.Needed != 0) {
				Ensure (result.Needed);
				result = ExecuteInto (font, plan, called);
			}

			if (result.Failure.Length != 0) {
				throw new NotSupportedException (result.Failure);
			}

			length = result.Count;
			havePositions = true;
			contentType = ContentType.Glyphs;
			randomState = result.RandomState;
		}

		private IntoResult ExecuteInto (Farfbuzz.Font font, Farfbuzz.ShapePlan plan, ReadOnlySpan<Farfbuzz.Feature> called)
		{
			if (pos.Length < info.Length) {
				pos = new GlyphPosition[info.Length];
			}

			var input = MemoryMarshal.Cast<GlyphInfo, Farfbuzz.GlyphInfo> (new ReadOnlySpan<GlyphInfo> (info, 0, length));
			var infoTarget = MemoryMarshal.Cast<GlyphInfo, Farfbuzz.GlyphInfo> (new Span<GlyphInfo> (info));
			var positionTarget = MemoryMarshal.Cast<GlyphPosition, Farfbuzz.GlyphPosition> (new Span<GlyphPosition> (pos));
			return Shaping.ExecuteInto (font, plan, input, CoreProperties (), PreContext (), PostContext (), (uint)flags, (int)clusterLevel, replacement, invisible, randomState, unicodeFunctions.ToCore (), infoTarget, positionTarget);
		}

		private Context PreContext () =>
			new Context (preLength, preContext[0], preContext[1], preContext[2], preContext[3], preContext[4]);

		private Context PostContext () =>
			new Context (postLength, postContext[0], postContext[1], postContext[2], postContext[3], postContext[4]);

		private SegmentProps Properties => new SegmentProps ((int)direction, (uint)script, language);

		private Farfbuzz.SegmentProperties CoreProperties ()
		{
			if (!haveCoreProperties || coreDirection != direction || !coreScript.Equals (script) || !string.Equals (coreLanguage, language)) {
				coreProperties = Segments.ToCore (Properties);
				coreDirection = direction;
				coreScript = script;
				coreLanguage = language;
				haveCoreProperties = true;
			}

			return coreProperties;
		}

		private void SetLength (int newLength)
		{
			Ensure (newLength);
			if (newLength > length) {
				Array.Clear (info, length, newLength - length);
				if (havePositions) {
					Array.Clear (pos, length, newLength - length);
				}
			}

			length = newLength;
			if (newLength == 0) {
				contentType = ContentType.Invalid;
				preLength = 0;
			}

			postLength = 0;
		}

		private void ReverseInPlace (int start, int end)
		{
			if (start < end) {
				Array.Reverse (info, start, end - start);
			}

			if (havePositions && start < end) {
				Array.Reverse (pos, start, end - start);
			}
		}

		private void AppendRange (Buffer source, uint start, uint end)
		{
			if (end > (uint)source.length) {
				end = (uint)source.length;
			}

			if (start > end) {
				start = end;
			}

			if (start == end) {
				return;
			}

			var count = (int)(end - start);
			if ((long)length + count > int.MaxValue) {
				return;
			}

			var originalLength = length;
			SetLength (length + count);
			if (originalLength == 0) {
				contentType = source.contentType;
			}

			if (!havePositions && source.havePositions) {
				ClearPositions ();
			}

			var overlaid = Segments.Overlay (Properties, source.Properties);
			direction = (Direction)overlaid.Direction;
			script = overlaid.Script;
			language = overlaid.Language;

			Array.Copy (source.info, (int)start, info, originalLength, count);
			if (havePositions) {
				if (source.havePositions) {
					Array.Copy (source.pos, (int)start, pos, originalLength, count);
				} else {
					Array.Clear (pos, originalLength, count);
				}
			}

			if (source.contentType != ContentType.Unicode) {
				return;
			}

			if (originalLength == 0 && start + source.preLength > 0) {
				preLength = 0;
				while (start > 0 && preLength < ContextLength) {
					preContext[preLength++] = source.info[(int)--start].Codepoint;
				}

				for (var i = 0; i < source.preLength && preLength < ContextLength; i++) {
					preContext[preLength++] = source.preContext[i];
				}
			}

			postLength = 0;
			while (end < source.length && postLength < ContextLength) {
				postContext[postLength++] = source.info[(int)end++].Codepoint;
			}

			for (var i = 0; i < source.postLength && postLength < ContextLength; i++) {
				postContext[postLength++] = source.postContext[i];
			}
		}

		private static unsafe ReadOnlySpan<uint> NullTerminated (uint* text)
		{
			var count = 0;
			while (text[count] != 0) {
				count++;
			}

			return new ReadOnlySpan<uint> (text, count);
		}
		private struct BufferSink : ICodepointSink<int>
		{
			private readonly GlyphInfo[] target;

			public BufferSink (GlyphInfo[] target)
			{
				this.target = target;
			}

			public int Put (int state, uint codepoint, uint cluster)
			{
				target[state] = new GlyphInfo { Codepoint = codepoint, Cluster = cluster };
				return state + 1;
			}
		}
	}
}
