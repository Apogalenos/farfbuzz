using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Flags for configuring buffer behavior during text shaping.</summary>
	/// <remarks />
	[Flags]
	public enum BufferFlags {
		/// <summary>The default buffer flags with no special behavior.</summary>
		Default = 0,
		/// <summary>Indicates that the buffer represents the beginning of the text.</summary>
		BeginningOfText = 1,
		/// <summary>Indicates that the buffer represents the end of the text.</summary>
		EndOfText = 2,
		/// <summary>Preserve default ignorable characters in the output.</summary>
		PreserveDefaultIgnorables = 4,
		/// <summary>Remove default ignorable characters from the output.</summary>
		RemoveDefaultIgnorables = 8,
		/// <summary>Do not insert dotted circle glyph for invalid character sequences.</summary>
		DoNotInsertDottedCircle = 16,
		/// <summary>Verifies buffer contents before text shaping.</summary>
		Verify = 32,
		/// <summary>Produces flags that identify glyphs unsafe to concatenate.</summary>
		ProduceUnsafeToConcat = 64,
		/// <summary>Produces flags that identify positions safe for tatweel insertion.</summary>
		ProduceSafeToInsertTatweel = 128,
	}
}
