using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#region Namespaces

#endregion

namespace HarfBuzzSharp
{

	/// <summary>Specifies the Unicode general category of a character.</summary>
	/// <remarks />
	public enum UnicodeGeneralCategory {
		/// <summary>Control character (Cc).</summary>
		Control = 0,
		/// <summary>Format character (Cf).</summary>
		Format = 1,
		/// <summary>Unassigned code point (Cn).</summary>
		Unassigned = 2,
		/// <summary>Private-use character (Co).</summary>
		PrivateUse = 3,
		/// <summary>Surrogate code point (Cs).</summary>
		Surrogate = 4,
		/// <summary>Lowercase letter (Ll).</summary>
		LowercaseLetter = 5,
		/// <summary>Modifier letter (Lm).</summary>
		ModifierLetter = 6,
		/// <summary>Other letter (Lo).</summary>
		OtherLetter = 7,
		/// <summary>Titlecase letter (Lt).</summary>
		TitlecaseLetter = 8,
		/// <summary>Uppercase letter (Lu).</summary>
		UppercaseLetter = 9,
		/// <summary>Spacing combining mark (Mc).</summary>
		SpacingMark = 10,
		/// <summary>Enclosing mark (Me).</summary>
		EnclosingMark = 11,
		/// <summary>Non-spacing mark (Mn).</summary>
		NonSpacingMark = 12,
		/// <summary>Decimal digit number (Nd).</summary>
		DecimalNumber = 13,
		/// <summary>Letter number (Nl).</summary>
		LetterNumber = 14,
		/// <summary>Other number (No).</summary>
		OtherNumber = 15,
		/// <summary>Connector punctuation (Pc).</summary>
		ConnectPunctuation = 16,
		/// <summary>Dash punctuation (Pd).</summary>
		DashPunctuation = 17,
		/// <summary>Close punctuation (Pe).</summary>
		ClosePunctuation = 18,
		/// <summary>Final punctuation (Pf).</summary>
		FinalPunctuation = 19,
		/// <summary>Initial punctuation (Pi).</summary>
		InitialPunctuation = 20,
		/// <summary>Other punctuation (Po).</summary>
		OtherPunctuation = 21,
		/// <summary>Open punctuation (Ps).</summary>
		OpenPunctuation = 22,
		/// <summary>Currency symbol (Sc).</summary>
		CurrencySymbol = 23,
		/// <summary>Modifier symbol (Sk).</summary>
		ModifierSymbol = 24,
		/// <summary>Math symbol (Sm).</summary>
		MathSymbol = 25,
		/// <summary>Other symbol (So).</summary>
		OtherSymbol = 26,
		/// <summary>Line separator (Zl).</summary>
		LineSeparator = 27,
		/// <summary>Paragraph separator (Zp).</summary>
		ParagraphSeparator = 28,
		/// <summary>Space separator (Zs).</summary>
		SpaceSeparator = 29,
	}
}
