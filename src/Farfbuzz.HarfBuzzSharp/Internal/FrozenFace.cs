using System;
using System.Collections.Immutable;
using System.Threading;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>The core face a <see cref="T:HarfBuzzSharp.Face" /> published when it became immutable: its overrides applied, never changed again, with the shape plans made for it.</summary>
	internal sealed class FrozenFace
	{
		private ImmutableDictionary<Farfbuzz.ShapePlanKey, Farfbuzz.ShapePlan> plans = ImmutableDictionary<Farfbuzz.ShapePlanKey, Farfbuzz.ShapePlan>.Empty;

		private readonly Lazy<Farfbuzz.Face> full;

		internal FrozenFace (Farfbuzz.Face core)
		{
			Core = core;
			full = new Lazy<Farfbuzz.Face> (() => core);
		}

		internal FrozenFace (Farfbuzz.Face core, Func<ImmutableDictionary<uint, ReadOnlyMemory<byte>>> outlineTables)
		{
			Core = core;
			full = new Lazy<Farfbuzz.Face> (() => Faces.WithTables (core, outlineTables ()), LazyThreadSafetyMode.ExecutionAndPublication);
		}

		internal Farfbuzz.Face Core { get; }

		internal Farfbuzz.Face Full => full.Value;

		internal Farfbuzz.ShapePlan Plan (Farfbuzz.Font font, Farfbuzz.ShapePlanKey key) =>
			ImmutableInterlocked.GetOrAdd (ref plans, key, wanted => {
				var (plan, failure) = Shaping.CreatePlan (font, wanted);
				return plan.IsSome ? plan.Value : throw new NotSupportedException (failure);
			});
	}
}
