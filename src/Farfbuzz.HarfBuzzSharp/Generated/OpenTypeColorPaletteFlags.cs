using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Flags for OpenType color palettes in CPAL table.</summary>
	/// <remarks />
	public enum OpenTypeColorPaletteFlags {
		/// <summary>No special flags.</summary>
		Default = 0,
		/// <summary>Palette is suitable for light backgrounds.</summary>
		UsableWithLightBackground = 1,
		/// <summary>Palette is suitable for dark backgrounds.</summary>
		UsableWithDarkBackground = 2,
	}
}
