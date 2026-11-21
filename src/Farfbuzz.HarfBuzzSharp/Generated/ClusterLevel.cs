using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>The various levels of buffer clustering.</summary>
	/// <remarks />
	public enum ClusterLevel {
		/// <summary>Cluster values grouped by graphemes into monotone order.</summary>
		MonotoneGraphemes = 0,
		/// <summary>Cluster values grouped into monotone order.</summary>
		MonotoneCharacters = 1,
		/// <summary>Don't group cluster values.</summary>
		Characters = 2,
		/// <summary>Cluster values grouped by graphemes without requiring monotone order.</summary>
		Graphemes = 3,
		/// <summary>Default cluster level (<see cref="F:HarfBuzzSharp.ClusterLevel.MonotoneGraphemes" />).</summary>
		Default = 0,
	}
}
