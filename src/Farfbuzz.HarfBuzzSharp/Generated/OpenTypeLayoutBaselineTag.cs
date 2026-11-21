using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Baseline tags from the OpenType BASE table.</summary>
	/// <remarks />
	public enum OpenTypeLayoutBaselineTag {
		/// <summary>Roman baseline.</summary>
		Roman = 1919905134,
		/// <summary>Hanging baseline for Indic scripts.</summary>
		Hanging = 1751215719,
		/// <summary>Ideographic face bottom or left baseline.</summary>
		IdeoFaceBottomOrLeft = 1768121954,
		/// <summary>Ideographic face top or right baseline.</summary>
		IdeoFaceTopOrRight = 1768121972,
		/// <summary>Ideographic face central baseline.</summary>
		IdeoFaceCentral = 1231251043,
		/// <summary>Ideographic em-box bottom or left baseline.</summary>
		IdeoEmboxBottomOrLeft = 1768187247,
		/// <summary>Ideographic em-box top or right baseline.</summary>
		IdeoEmboxTopOrRight = 1768191088,
		/// <summary>Ideographic em-box central baseline.</summary>
		IdeoEmboxCentral = 1231315813,
		/// <summary>Math baseline.</summary>
		Math = 1835103336,
	}
}
