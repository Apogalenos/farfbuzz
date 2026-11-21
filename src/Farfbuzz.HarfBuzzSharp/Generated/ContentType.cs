using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>The various types of buffer contents.</summary>
	/// <remarks />
	public enum ContentType {
		/// <summary>Initial value for new buffer.</summary>
		Invalid = 0,
		/// <summary>The buffer contains input characters (before shaping).</summary>
		Unicode = 1,
		/// <summary>The buffer contains output glyphs (after shaping).</summary>
		Glyphs = 2,
	}
}
