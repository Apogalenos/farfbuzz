using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Specifies the glyph class for OpenType layout operations.</summary>
	/// <remarks />
	public enum OpenTypeLayoutGlyphClass {
		/// <summary>The glyph has no class definition.</summary>
		Unclassified = 0,
		/// <summary>A base glyph that acts as a single character.</summary>
		BaseGlyph = 1,
		/// <summary>A ligature glyph composed of multiple characters.</summary>
		Ligature = 2,
		/// <summary>A combining mark glyph that attaches to a base glyph.</summary>
		Mark = 3,
		/// <summary>A glyph that is a component of a ligature.</summary>
		Component = 4,
	}
}
