using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Flags indicating differences between two buffers when compared.</summary>
	/// <remarks />
	public enum BufferDiffFlags {
		/// <summary>The buffers are equal.</summary>
		Equal = 0,
		/// <summary>Content types differ between the buffers.</summary>
		ContentTypeMismatch = 1,
		/// <summary>Buffer lengths differ.</summary>
		LengthMismatch = 2,
		/// <summary>A .notdef glyph is present in one of the buffers.</summary>
		NotdefPresent = 4,
		/// <summary>A dotted circle glyph is present in one of the buffers.</summary>
		DottedCirclePresent = 8,
		/// <summary>Codepoints or glyph indices differ between the buffers.</summary>
		CodepointMismatch = 16,
		/// <summary>Cluster values differ between the buffers.</summary>
		ClusterMismatch = 32,
		/// <summary>Glyph flags differ between the buffers.</summary>
		GlyphFlagsMismatch = 64,
		/// <summary>Glyph positions differ between the buffers.</summary>
		PositionMismatch = 128,
	}
}
