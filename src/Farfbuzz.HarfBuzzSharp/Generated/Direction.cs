using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Various text directions that can be set via <see cref="P:HarfBuzzSharp.Buffer.Direction" />.</summary>
	/// <remarks />
	public enum Direction {
		/// <summary>Initial, unset direction.</summary>
		Invalid = 0,
		/// <summary>Text is set horizontally from left to right.</summary>
		LeftToRight = 4,
		/// <summary>Text is set horizontally from right to left.</summary>
		RightToLeft = 5,
		/// <summary>Text is set vertically from top to bottom.</summary>
		TopToBottom = 6,
		/// <summary>Text is set vertically from bottom to top.</summary>
		BottomToTop = 7,
	}
}
