using System;
using System.Collections.Immutable;
using System.Threading;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>The plan the last shape call of a font used, kept with the segment properties it was made for, so that the next line of the same properties and no features or shapers finds
	/// it without making a key.</summary>
	internal sealed class PlanMemo
	{
		internal PlanMemo (Direction direction, Script script, string language, Farfbuzz.ShapePlan plan)
		{
			Direction = direction;
			Script = script;
			Language = language;
			Plan = plan;
		}

		internal Direction Direction { get; }

		internal Script Script { get; }

		internal string Language { get; }

		internal Farfbuzz.ShapePlan Plan { get; }
	}

	/// <summary>The immutable state of a <see cref="T:HarfBuzzSharp.Font" /> a call reads once: the face it was made from, its scale and its font functions. The core fonts are
	/// made from them on first use, so that making a font over a table-callback face asks the callback for nothing: <c>Light</c> reads the shaping tables only
	/// and answers the queries that need no outline, <c>Full</c> also has the outline tables.</summary>
	internal sealed class FontSnapshot
	{
		private readonly Lazy<Farfbuzz.Font> light;
		private readonly Lazy<Farfbuzz.Font> full;

		private readonly ImmutableList<Func<Farfbuzz.Font, Farfbuzz.Font>> variations;

		internal FontSnapshot (FrozenFace face, int xScale, int yScale, FontFuncsHolder holder = null, ImmutableList<Func<Farfbuzz.Font, Farfbuzz.Font>> variations = null)
		{
			Face = face;
			XScale = xScale;
			YScale = yScale;
			Holder = holder;
			this.variations = variations ?? ImmutableList<Func<Farfbuzz.Font, Farfbuzz.Font>>.Empty;
			light = new Lazy<Farfbuzz.Font> (() => Make (face.Core), LazyThreadSafetyMode.ExecutionAndPublication);
			full = new Lazy<Farfbuzz.Font> (() => Make (face.Full), LazyThreadSafetyMode.ExecutionAndPublication);
		}

		internal FrozenFace Face { get; }

		internal PlanMemo LastPlan { get; set; }

		internal int XScale { get; }

		internal int YScale { get; }

		internal FontFuncsHolder Holder { get; }

		internal Farfbuzz.Font Light => light.Value;

		internal Farfbuzz.Font Full => full.Value;

		internal FontSnapshot WithScale (int xScale, int yScale) =>
			new FontSnapshot (Face, xScale, yScale, Holder, variations);

		internal FontSnapshot WithFunctions (FontFuncsHolder holder) =>
			new FontSnapshot (Face, XScale, YScale, holder, variations);

		internal FontSnapshot WithVariation (Func<Farfbuzz.Font, Farfbuzz.Font> operation) =>
			new FontSnapshot (Face, XScale, YScale, Holder, variations.Add (operation));

		private Farfbuzz.Font Make (Farfbuzz.Face core)
		{
			var font = Fonts.Build (core, XScale, YScale);
			foreach (var operation in variations) {
				font = operation (font);
			}

			return Holder == null ? font : FontFuncsBuilder.Apply (font, Holder.Build (XScale, YScale));
		}
	}
}
