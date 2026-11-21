using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Specifies the Unicode canonical combining class of a character.</summary>
	/// <remarks>Combining classes determine how combining marks are ordered and rendered relative to base characters. Values correspond to the Unicode Canonical Combining Class property.</remarks>
	public enum UnicodeCombiningClass {
		/// <summary>Combining class value.</summary>
		NotReordered = 0,
		/// <summary>Combining class value.</summary>
		Overlay = 1,
		/// <summary>Combining class value.</summary>
		Nukta = 7,
		/// <summary>Combining class value.</summary>
		KanaVoicing = 8,
		/// <summary>Combining class value.</summary>
		Virama = 9,
		/// <summary>Combining class value.</summary>
		CCC10 = 10,
		/// <summary>Combining class value.</summary>
		CCC11 = 11,
		/// <summary>Combining class value.</summary>
		CCC12 = 12,
		/// <summary>Combining class value.</summary>
		CCC13 = 13,
		/// <summary>Combining class value.</summary>
		CCC14 = 14,
		/// <summary>Combining class value.</summary>
		CCC15 = 15,
		/// <summary>Combining class value.</summary>
		CCC16 = 16,
		/// <summary>Combining class value.</summary>
		CCC17 = 17,
		/// <summary>Combining class value.</summary>
		CCC18 = 18,
		/// <summary>Combining class value.</summary>
		CCC19 = 19,
		/// <summary>Combining class value.</summary>
		CCC20 = 20,
		/// <summary>Combining class value.</summary>
		CCC21 = 21,
		/// <summary>Combining class value.</summary>
		CCC22 = 22,
		/// <summary>Combining class value.</summary>
		CCC23 = 23,
		/// <summary>Combining class value.</summary>
		CCC24 = 24,
		/// <summary>Combining class value.</summary>
		CCC25 = 25,
		/// <summary>Combining class value.</summary>
		CCC26 = 26,
		/// <summary>Combining class value.</summary>
		CCC27 = 27,
		/// <summary>Combining class value.</summary>
		CCC28 = 28,
		/// <summary>Combining class value.</summary>
		CCC29 = 29,
		/// <summary>Combining class value.</summary>
		CCC30 = 30,
		/// <summary>Combining class value.</summary>
		CCC31 = 31,
		/// <summary>Combining class value.</summary>
		CCC32 = 32,
		/// <summary>Combining class value.</summary>
		CCC33 = 33,
		/// <summary>Combining class value.</summary>
		CCC34 = 34,
		/// <summary>Combining class value.</summary>
		CCC35 = 35,
		/// <summary>Combining class value.</summary>
		CCC36 = 36,
		/// <summary>Combining class value.</summary>
		CCC84 = 84,
		/// <summary>Combining class value.</summary>
		CCC91 = 91,
		/// <summary>Combining class value.</summary>
		CCC103 = 103,
		/// <summary>Combining class value.</summary>
		CCC107 = 107,
		/// <summary>Combining class value.</summary>
		CCC118 = 118,
		/// <summary>Combining class value.</summary>
		CCC122 = 122,
		/// <summary>Combining class value.</summary>
		CCC129 = 129,
		/// <summary>Combining class value.</summary>
		CCC130 = 130,
		/// <summary>Canonical combining class 132.</summary>
		CCC132 = 132,
		/// <summary>Combining class value.</summary>
		AttachedBelowLeft = 200,
		/// <summary>Combining class value.</summary>
		AttachedBelow = 202,
		/// <summary>Combining class value.</summary>
		AttachedAbove = 214,
		/// <summary>Combining class value.</summary>
		AttachedAboveRight = 216,
		/// <summary>Combining class value.</summary>
		BelowLeft = 218,
		/// <summary>Combining class value.</summary>
		Below = 220,
		/// <summary>Combining class value.</summary>
		BelowRight = 222,
		/// <summary>Combining class value.</summary>
		Left = 224,
		/// <summary>Combining class value.</summary>
		Right = 226,
		/// <summary>Combining class value.</summary>
		AboveLeft = 228,
		/// <summary>Combining class value.</summary>
		Above = 230,
		/// <summary>Combining class value.</summary>
		AboveRight = 232,
		/// <summary>Combining class value.</summary>
		DoubleBelow = 233,
		/// <summary>Combining class value.</summary>
		DoubleAbove = 234,
		/// <summary>Combining class value.</summary>
		IotaSubscript = 240,
		/// <summary>Combining class value.</summary>
		Invalid = 255,
	}
}
