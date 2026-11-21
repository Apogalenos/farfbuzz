using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Specifies numeric values stored in OpenType font tables.</summary>
	/// <remarks />
	public enum OpenTypeNumberTag {
		/// <summary>The minimum horizontal coordinate from the font bounding box in the <c>head</c> table.</summary>
		FontXMin = 2020436334,
		/// <summary>The minimum vertical coordinate from the font bounding box in the <c>head</c> table.</summary>
		FontYMin = 2037213550,
		/// <summary>The maximum horizontal coordinate from the font bounding box in the <c>head</c> table.</summary>
		FontXMax = 2020434296,
		/// <summary>The maximum vertical coordinate from the font bounding box in the <c>head</c> table.</summary>
		FontYMax = 2037211512,
	}
}
