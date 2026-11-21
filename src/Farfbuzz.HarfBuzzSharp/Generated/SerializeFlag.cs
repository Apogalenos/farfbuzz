using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>The various flags that control what glyph information are serialized by <see cref="M:HarfBuzzSharp.Buffer.SerializeGlyphs(System.Int32,System.Int32,HarfBuzzSharp.Font,HarfBuzzSharp.SerializeFormat,HarfBuzzSharp.SerializeFlag)" />.</summary>
	/// <remarks />
	[Flags]
	public enum SerializeFlag {
		/// <summary>Serialize glyph names, clusters and position information.</summary>
		Default = 0,
		/// <summary>Do not serialize glyph clusters.</summary>
		NoClusters = 1,
		/// <summary>Do not serialize glyph position information.</summary>
		NoPositions = 2,
		/// <summary>Do not serialize glyph names.</summary>
		NoGlyphNames = 4,
		/// <summary>Serialize glyph extents.</summary>
		GlyphExtents = 8,
		/// <summary>Serialize glyph flags.</summary>
		GlyphFlags = 16,
		/// <summary>Do not serialize glyph advances (glyph offsets will reflect absolute glyph positions).</summary>
		NoAdvances = 32,
	}
}
