#nullable disable

using System;
using System.Collections.Generic;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>Represents a collection of callback functions used by HarfBuzz for Unicode character property lookups such as script, general category, and combining class.</summary>
	/// <remarks />
	public class UnicodeFunctions : NativeObject
	{
		private static readonly Lazy<UnicodeFunctions> defaultFunctions =
			new Lazy<UnicodeFunctions> (() => new StaticUnicodeFunctions (true));

		private static readonly Lazy<UnicodeFunctions> emptyFunctions =
			new Lazy<UnicodeFunctions> (() => new StaticUnicodeFunctions (false));

		private sealed class Slot<T> where T : Delegate
		{
			internal T Del;
			internal UnicodeFunctions Owner;
			internal ReleaseDelegate Destroy;

			internal Slot<T> CopyForChild () =>
				new Slot<T> { Del = Del, Owner = Owner };
		}

		private readonly bool builtinBase;
		private bool immutable;
		private Slot<CombiningClassDelegate> combining = new Slot<CombiningClassDelegate> ();
		private Slot<GeneralCategoryDelegate> category = new Slot<GeneralCategoryDelegate> ();
		private Slot<MirroringDelegate> mirroring = new Slot<MirroringDelegate> ();
		private Slot<ScriptDelegate> script = new Slot<ScriptDelegate> ();
		private Slot<ComposeDelegate> compose = new Slot<ComposeDelegate> ();
		private Slot<DecomposeDelegate> decompose = new Slot<DecomposeDelegate> ();

		private UnicodeFunctions (bool builtinBase, bool immutable)
		{
			this.builtinBase = builtinBase;
			this.immutable = immutable;
		}

		/// <summary>Gets the default <see cref="T:HarfBuzzSharp.UnicodeFunctions" /> instance with built-in Unicode support.</summary>
		/// <value>The default Unicode functions instance.</value>
		/// <remarks />
		public static UnicodeFunctions Default => defaultFunctions.Value;

		/// <summary>Gets a reference to the empty <see cref="T:HarfBuzzSharp.UnicodeFunctions" /> instance with no callbacks set.</summary>
		/// <value>The empty Unicode functions instance.</value>
		/// <remarks />
		public static UnicodeFunctions Empty => emptyFunctions.Value;

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.UnicodeFunctions" /> class that inherits from the specified parent.</summary>
		/// <param name="parent">The parent Unicode functions to inherit from for any unset callbacks, or <see langword="null" />.</param>
		/// <remarks />
		public UnicodeFunctions (UnicodeFunctions parent)
		{
			if (parent == null)
				throw new ArgumentNullException (nameof (parent));
			if (parent.Handle == IntPtr.Zero)
				throw new ArgumentException (nameof (parent.Handle));

			parent.immutable = true;
			Parent = parent;
			builtinBase = parent.builtinBase;
			combining = parent.combining.CopyForChild ();
			category = parent.category.CopyForChild ();
			mirroring = parent.mirroring.CopyForChild ();
			script = parent.script.CopyForChild ();
			compose = parent.compose.CopyForChild ();
			decompose = parent.decompose.CopyForChild ();
		}

		/// <summary>Gets the parent Unicode functions that this instance inherits from.</summary>
		/// <value>The parent <see cref="T:HarfBuzzSharp.UnicodeFunctions" />, or <see langword="null" /> if there is no parent.</value>
		/// <remarks />
		public UnicodeFunctions Parent { get; }

		/// <summary>Gets a value indicating whether this Unicode functions instance is immutable.</summary>
		/// <value><see langword="true" /> if the instance is immutable; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool IsImmutable => immutable;

		/// <summary>Makes this Unicode functions instance immutable, preventing further modifications.</summary>
		/// <remarks />
		public void MakeImmutable ()
		{
			immutable = true;
		}

		/// <summary>Gets the canonical combining class of the specified Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.UnicodeCombiningClass" /> for the code point.</returns>
		/// <remarks />
		public UnicodeCombiningClass GetCombiningClass (int unicode) => GetCombiningClass ((uint)unicode);

		/// <summary>Gets the canonical combining class of the specified Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.UnicodeCombiningClass" /> for the code point.</returns>
		/// <remarks />
		public UnicodeCombiningClass GetCombiningClass (uint unicode)
		{
			if (combining.Del != null)
				return combining.Del (combining.Owner, unicode);
			return builtinBase ? (UnicodeCombiningClass)UnicodeQueries.CombiningClass (unicode) : UnicodeCombiningClass.NotReordered;
		}

		/// <summary>Gets the general category of the specified Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.UnicodeGeneralCategory" /> for the code point.</returns>
		/// <remarks />
		public UnicodeGeneralCategory GetGeneralCategory (int unicode) => GetGeneralCategory ((uint)unicode);

		/// <summary>Gets the general category of the specified Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.UnicodeGeneralCategory" /> for the code point.</returns>
		/// <remarks />
		public UnicodeGeneralCategory GetGeneralCategory (uint unicode)
		{
			if (category.Del != null)
				return category.Del (category.Owner, unicode);
			return builtinBase ? (UnicodeGeneralCategory)UnicodeQueries.GeneralCategory (unicode) : UnicodeGeneralCategory.OtherLetter;
		}

		/// <summary>Gets the mirrored code point for bidirectional text rendering.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The mirrored Unicode code point, or the original code point if no mirroring is defined.</returns>
		/// <remarks />
		public int GetMirroring (int unicode) => (int)GetMirroring ((uint)unicode);

		/// <summary>Gets the mirrored code point for bidirectional text rendering.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The mirrored Unicode code point, or the original code point if no mirroring is defined.</returns>
		/// <remarks />
		public uint GetMirroring (uint unicode)
		{
			if (mirroring.Del != null)
				return mirroring.Del (mirroring.Owner, unicode);
			return builtinBase ? UnicodeQueries.Mirroring (unicode) : unicode;
		}

		/// <summary>Gets the script of the specified Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.Script" /> for the code point.</returns>
		/// <remarks />
		public Script GetScript (int unicode) => GetScript ((uint)unicode);

		/// <summary>Gets the script of the specified Unicode code point.</summary>
		/// <param name="unicode">The Unicode code point to query.</param>
		/// <returns>The <see cref="T:HarfBuzzSharp.Script" /> for the code point.</returns>
		/// <remarks />
		public Script GetScript (uint unicode)
		{
			if (script.Del != null)
				return script.Del (script.Owner, unicode);
			return builtinBase ? (Script)UnicodeQueries.Script (unicode) : Script.Unknown;
		}

		/// <summary>Attempts to compose two Unicode code points into a single code point.</summary>
		/// <param name="a">The first Unicode code point to compose.</param>
		/// <param name="b">The second Unicode code point to compose.</param>
		/// <param name="ab">When this method returns, contains the composed Unicode code point if successful.</param>
		/// <returns><see langword="true" /> if the code points were successfully composed; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryCompose (int a, int b, out int ab)
		{
			var result = TryCompose ((uint)a, (uint)b, out var composed);

			ab = (int)composed;

			return result;
		}

		/// <summary>Attempts to compose two Unicode code points into a single code point.</summary>
		/// <param name="a">The first Unicode code point to compose.</param>
		/// <param name="b">The second Unicode code point to compose.</param>
		/// <param name="ab">When this method returns, contains the composed Unicode code point if successful.</param>
		/// <returns><see langword="true" /> if the code points were successfully composed; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryCompose (uint a, uint b, out uint ab)
		{
			ab = 0;
			if (compose.Del != null)
				return compose.Del (compose.Owner, a, b, out ab);
			if (!builtinBase)
				return false;
			var (found, composed) = UnicodeQueries.Compose (a, b);
			if (found)
				ab = composed;
			return found;
		}

		/// <summary>Attempts to decompose a Unicode code point into two code points.</summary>
		/// <param name="ab">The Unicode code point to decompose.</param>
		/// <param name="a">When this method returns, contains the first component of the decomposition if successful.</param>
		/// <param name="b">When this method returns, contains the second component of the decomposition if successful.</param>
		/// <returns><see langword="true" /> if the code point was successfully decomposed; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryDecompose (int ab, out int a, out int b)
		{
			var result = TryDecompose ((uint)ab, out var decomposedA, out var decomposedB);

			a = (int)decomposedA;

			b = (int)decomposedB;

			return result;
		}

		/// <summary>Attempts to decompose a Unicode code point into two code points.</summary>
		/// <param name="ab">The Unicode code point to decompose.</param>
		/// <param name="a">When this method returns, contains the first component of the decomposition if successful.</param>
		/// <param name="b">When this method returns, contains the second component of the decomposition if successful.</param>
		/// <returns><see langword="true" /> if the code point was successfully decomposed; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryDecompose (uint ab, out uint a, out uint b)
		{
			a = ab;
			b = 0;
			if (decompose.Del != null)
				return decompose.Del (decompose.Owner, ab, out a, out b);
			if (!builtinBase)
				return false;
			var (found, first, second) = UnicodeQueries.Decompose (ab);
			if (found) {
				a = first;
				b = second;
			}
			return found;
		}

		/// <summary>Sets the callback for retrieving the canonical combining class of a Unicode code point.</summary>
		/// <param name="del">The delegate to set for retrieving combining classes.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetCombiningClassDelegate (CombiningClassDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			Replace (combining, del, destroy);
		}

		/// <summary>Sets the callback for retrieving the general category of a Unicode code point.</summary>
		/// <param name="del">The delegate to set for retrieving general categories.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetGeneralCategoryDelegate (GeneralCategoryDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			Replace (category, del, destroy);
		}

		/// <summary>Sets the callback for retrieving the mirrored glyph of a Unicode code point.</summary>
		/// <param name="del">The delegate to set for retrieving mirrored characters.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetMirroringDelegate (MirroringDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			Replace (mirroring, del, destroy);
		}

		/// <summary>Sets the callback for retrieving the script of a Unicode code point.</summary>
		/// <param name="del">The delegate to set for retrieving scripts.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetScriptDelegate (ScriptDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			Replace (script, del, destroy);
		}

		/// <summary>Sets the callback for composing two Unicode code points into a single code point.</summary>
		/// <param name="del">The delegate to set for Unicode composition.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetComposeDelegate (ComposeDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			Replace (compose, del, destroy);
		}

		/// <summary>Sets the callback for decomposing a Unicode code point into two code points.</summary>
		/// <param name="del">The delegate to set for Unicode decomposition.</param>
		/// <param name="destroy">The delegate to call when the callback is replaced or destroyed, or <see langword="null" />.</param>
		/// <remarks />
		public void SetDecomposeDelegate (DecomposeDelegate del, ReleaseDelegate destroy = null)
		{
			VerifyParameters (del);
			Replace (decompose, del, destroy);
		}

		private void Replace<T> (Slot<T> slot, T del, ReleaseDelegate destroy) where T : Delegate
		{
			var previous = slot.Destroy;
			slot.Del = del;
			slot.Owner = this;
			slot.Destroy = destroy;
			previous?.Invoke ();
		}

		private void VerifyParameters (Delegate del)
		{
			_ = del ?? throw new ArgumentNullException (nameof (del));

			if (IsImmutable)
				throw new InvalidOperationException ($"{nameof (UnicodeFunctions)} is immutable and can't be changed.");
		}

		internal Farfbuzz.UnicodeFuncs ToCore ()
		{
			var replaced = combining.Del != null || category.Del != null || mirroring.Del != null || script.Del != null || compose.Del != null || decompose.Del != null;
			if (!replaced && builtinBase)
				return null;

			Func<uint, int> combiningAnswer = null;
			if (combining.Del != null || !builtinBase) {
				var slot = combining;
				combiningAnswer = u => slot.Del != null ? (int)slot.Del (slot.Owner, u) : 0;
			}
			Func<uint, int> categoryAnswer = null;
			if (category.Del != null || !builtinBase) {
				var slot = category;
				categoryAnswer = u => slot.Del != null ? (int)slot.Del (slot.Owner, u) : (int)UnicodeGeneralCategory.OtherLetter;
			}
			Func<uint, uint> mirroringAnswer = null;
			if (mirroring.Del != null || !builtinBase) {
				var slot = mirroring;
				mirroringAnswer = u => slot.Del != null ? slot.Del (slot.Owner, u) : u;
			}
			Func<uint, uint> scriptAnswer = null;
			if (script.Del != null || !builtinBase) {
				var slot = script;
				scriptAnswer = u => slot.Del != null ? (uint)slot.Del (slot.Owner, u) : (uint)Script.Unknown;
			}
			Func<uint, uint, ValueTuple<bool, uint>> composeAnswer = null;
			if (compose.Del != null || !builtinBase) {
				var slot = compose;
				composeAnswer = (a, b) => {
					if (slot.Del == null)
						return (false, 0u);
					var ok = slot.Del (slot.Owner, a, b, out var ab);
					return (ok, ab);
				};
			}
			Func<uint, ValueTuple<bool, uint, uint>> decomposeAnswer = null;
			if (decompose.Del != null || !builtinBase) {
				var slot = decompose;
				decomposeAnswer = ab => {
					if (slot.Del == null)
						return (false, 0u, 0u);
					var ok = slot.Del (slot.Owner, ab, out var a, out var b);
					return (ok, a, b);
				};
			}

			return UnicodeQueries.Build (combiningAnswer, categoryAnswer, mirroringAnswer, scriptAnswer, composeAnswer, decomposeAnswer);
		}

		/// <summary>Releases the unmanaged resources used by the <see cref="T:HarfBuzzSharp.UnicodeFunctions" /> and optionally releases the managed resources.</summary>
		/// <param name="disposing"><see langword="true" /> to release both managed and unmanaged resources; <see langword="false" /> to release only unmanaged resources.</param>
		/// <remarks />
		protected override void Dispose (bool disposing) =>
			base.Dispose (disposing);

		/// <summary>Releases the unmanaged resources used.</summary>
		/// <remarks />
		protected override void DisposeHandler ()
		{
			combining.Destroy?.Invoke ();
			category.Destroy?.Invoke ();
			mirroring.Destroy?.Invoke ();
			script.Destroy?.Invoke ();
			compose.Destroy?.Invoke ();
			decompose.Destroy?.Invoke ();
			combining.Destroy = category.Destroy = null;
			mirroring.Destroy = script.Destroy = compose.Destroy = decompose.Destroy = null;
		}

		private class StaticUnicodeFunctions : UnicodeFunctions
		{
			public StaticUnicodeFunctions (bool builtin)
				: base (builtin, true)
			{
			}

			protected override void Dispose (bool disposing)
			{
				// do not dispose
			}
		}
	}
}
