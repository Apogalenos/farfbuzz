using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Various memory modes for  <see cref="T:HarfBuzzSharp.Blob" /></summary>
	/// <remarks>In no case shall the HarfBuzz client modify memory that is passed to HarfBuzz in a blob. If there is any such possibility, <see cref="F:HarfBuzzSharp.MemoryMode.Duplicate" /> should be used such that HarfBuzz makes a copy immediately.</remarks>
	public enum MemoryMode {
		/// <summary>HarfBuzz makes a copy immediately.</summary>
		Duplicate = 0,
		/// <summary>Default mode indicating that the memory won't be changed.</summary>
		ReadOnly = 1,
		/// <summary>Indicates that the data was copied solely for the purpose of passing to HarfBuzz.</summary>
		Writeable = 2,
		/// <summary>The font file was mmap()ed, but <see cref="F:HarfBuzzSharp.MemoryMode.ReadOnly" /> should still be used.</summary>
		ReadOnlyMayMakeWriteable = 3,
	}
}
