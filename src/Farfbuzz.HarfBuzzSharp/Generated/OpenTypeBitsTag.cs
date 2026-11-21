using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Specifies raw bit-field values stored in OpenType font tables.</summary>
	/// <remarks />
	public enum OpenTypeBitsTag {
		/// <summary>The embedding licensing rights from the <c>fsType</c> field of the <c>OS/2</c> table.</summary>
		FsType = 1718842480,
		/// <summary>The font selection flags from the <c>fsSelection</c> field of the <c>OS/2</c> table.</summary>
		FsSelection = 1718842220,
		/// <summary>The font style flags from the <c>macStyle</c> field of the <c>head</c> table.</summary>
		MacStyle = 1835234164,
		/// <summary>The fixed-pitch value from the <c>isFixedPitch</c> field of the <c>post</c> table.</summary>
		IsFixedPitch = 1719169140,
		/// <summary>The first Unicode-range bit field from the <c>OS/2</c> table.</summary>
		UnicodeRange1 = 1970433585,
		/// <summary>The second Unicode-range bit field from the <c>OS/2</c> table.</summary>
		UnicodeRange2 = 1970433586,
		/// <summary>The third Unicode-range bit field from the <c>OS/2</c> table.</summary>
		UnicodeRange3 = 1970433587,
		/// <summary>The fourth Unicode-range bit field from the <c>OS/2</c> table.</summary>
		UnicodeRange4 = 1970433588,
		/// <summary>The first code-page-range bit field from the <c>OS/2</c> table.</summary>
		CodePageRange1 = 1668313649,
		/// <summary>The second code-page-range bit field from the <c>OS/2</c> table.</summary>
		CodePageRange2 = 1668313650,
	}
}
