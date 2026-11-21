using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>The various serialization and de-serialization formats.</summary>
	/// <remarks />
	public enum SerializeFormat {
		/// <summary>A human-readable, plain text format.</summary>
		Text = 1413830740,
		/// <summary>A machine-readable JSON format.</summary>
		Json = 1246973774,
		/// <summary>The format is invalid.</summary>
		Invalid = 0,
	}
}
