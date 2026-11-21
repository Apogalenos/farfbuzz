#nullable disable

using System;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>Provides access to OpenType font metrics.</summary>
	/// <remarks />
	public class OpenTypeMetrics
	{
		private readonly Font font;

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.OpenTypeMetrics" /> struct for the specified font.</summary>
		/// <param name="font">The font to retrieve metrics from.</param>
		/// <remarks />
		public OpenTypeMetrics (Font font)
		{
			this.font = font ?? throw new ArgumentNullException (nameof (font));
		}

		/// <summary>Tries to get the position for the specified metrics tag.</summary>
		/// <param name="metricsTag">The metrics tag to query.</param>
		/// <param name="position">When this method returns, contains the position value if found.</param>
		/// <returns><see langword="true" /> if the position was found; otherwise, <see langword="false" />.</returns>
		/// <remarks />
		public bool TryGetPosition (OpenTypeMetricsTag metricsTag, out int position)
		{
			var (found, value) = Metrics.Position (font.Snapshot.Light, (uint)metricsTag);
			position = value;
			return found;
		}

		/// <summary>Gets the variation value for the specified metrics tag.</summary>
		/// <param name="metricsTag">The metrics tag to query.</param>
		/// <returns>The variation value.</returns>
		/// <remarks />
		public float GetVariation (OpenTypeMetricsTag metricsTag) =>
			Metrics.Variation (font.Snapshot.Light, (uint)metricsTag);

		/// <summary>Gets the horizontal variation value for the specified metrics tag.</summary>
		/// <param name="metricsTag">The metrics tag to query.</param>
		/// <returns>The horizontal variation value in font units.</returns>
		/// <remarks />
		public int GetXVariation (OpenTypeMetricsTag metricsTag) =>
			Metrics.XVariation (font.Snapshot.Light, (uint)metricsTag);

		/// <summary>Gets the vertical variation value for the specified metrics tag.</summary>
		/// <param name="metricsTag">The metrics tag to query.</param>
		/// <returns>The vertical variation value in font units.</returns>
		/// <remarks />
		public int GetYVariation (OpenTypeMetricsTag metricsTag) =>
			Metrics.YVariation (font.Snapshot.Light, (uint)metricsTag);
	}
}
