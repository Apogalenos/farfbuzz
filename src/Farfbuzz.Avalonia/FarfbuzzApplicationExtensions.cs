using Avalonia.Platform;
using Farfbuzz.AvaloniaShaping;

namespace Avalonia
{
	/// <summary>Application builder extensions for Farfbuzz.</summary>
	public static class FarfbuzzApplicationExtensions
	{
		/// <summary>Makes Farfbuzz the text shaper of the application. Call it after <c>UsePlatformDetect</c>, which installs HarfBuzz: the last initializer of the text shaping subsystem wins.</summary>
		/// <param name="builder">The application builder.</param>
		/// <returns>The application builder.</returns>
		public static AppBuilder UseFarfbuzz (this AppBuilder builder) =>
			builder.UseTextShapingSubsystem (() => AvaloniaLocator.CurrentMutable.Bind<ITextShaperImpl> ().ToConstant (new FarfbuzzTextShaper ()), "Farfbuzz");
	}
}
