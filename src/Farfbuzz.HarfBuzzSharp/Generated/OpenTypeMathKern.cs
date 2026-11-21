using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Specifies math kerning positions in the OpenType MATH table.</summary>
	/// <remarks />
	public enum OpenTypeMathKern {
		/// <summary>Top-right kerning position.</summary>
		TopRight = 0,
		/// <summary>Top-left kerning position.</summary>
		TopLeft = 1,
		/// <summary>Bottom-right kerning position.</summary>
		BottomRight = 2,
		/// <summary>Bottom-left kerning position.</summary>
		BottomLeft = 3,
	}
}
