using System;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>The functions a font was given with the font and data they are called with; turns the delegates into the core's font functions at a scale.</summary>
	internal sealed class FontFuncsHolder
	{
		private readonly Font font;
		private readonly FontFunctions functions;
		private readonly object data;

		internal FontFuncsHolder (Font font, FontFunctions functions, object data)
		{
			this.font = font;
			this.functions = functions;
			this.data = data;
		}

		internal Farfbuzz.FontFuncs Build (int xScale, int yScale)
		{
			var f = functions;
			var font = this.font;
			var data = this.data;

			Func<uint, ValueTuple<bool, uint>> nominal = null;
			if (f.nominalDelegate != null) {
				nominal = u => { var ok = f.nominalDelegate (font, data, u, out var g); return (ok, g); };
			} else if (f.nominalsDelegate != null) {
				nominal = u => {
					var glyphs = new uint[1];
					var found = f.nominalsDelegate (font, data, 1, new[] { u }, glyphs) == 1;
					return (found, glyphs[0]);
				};
			}

			Func<uint, uint, ValueTuple<bool, uint>> variation = f.variationDelegate == null ? null :
				(u, s) => { var ok = f.variationDelegate (font, data, u, s, out var g); return (ok, g); };

			Func<uint, int> hAdvance = null;
			if (f.hAdvanceDelegate != null) {
				hAdvance = g => f.hAdvanceDelegate (font, data, g);
			} else if (f.hAdvancesDelegate != null) {
				hAdvance = g => { var a = new int[1]; f.hAdvancesDelegate (font, data, 1, new[] { g }, a); return a[0]; };
			}

			Func<uint, int> vAdvance = null;
			if (f.vAdvanceDelegate != null) {
				vAdvance = g => f.vAdvanceDelegate (font, data, g);
			} else if (f.vAdvancesDelegate != null) {
				vAdvance = g => { var a = new int[1]; f.vAdvancesDelegate (font, data, 1, new[] { g }, a); return a[0]; };
			}

			Func<uint, ValueTuple<bool, int, int>> hOrigin = f.hOriginDelegate == null ? null :
				g => { var ok = f.hOriginDelegate (font, data, g, out var x, out var y); return (ok, x, y); };
			Func<uint, ValueTuple<bool, int, int>> vOrigin = f.vOriginDelegate == null ? null :
				g => { var ok = f.vOriginDelegate (font, data, g, out var x, out var y); return (ok, x, y); };
			Func<uint, uint, int> kerning = f.hKerningDelegate == null ? null : (l, r) => f.hKerningDelegate (font, data, l, r);
			Func<uint, ValueTuple<bool, int, int, int, int>> extents = f.extentsDelegate == null ? null :
				g => { var ok = f.extentsDelegate (font, data, g, out var e); return (ok, e.XBearing, e.YBearing, e.Width, e.Height); };
			Func<uint, uint, ValueTuple<bool, int, int>> contour = f.contourDelegate == null ? null :
				(g, i) => { var ok = f.contourDelegate (font, data, g, i, out var x, out var y); return (ok, x, y); };
			Func<uint, ValueTuple<bool, string>> name = f.nameDelegate == null ? null :
				g => { var ok = f.nameDelegate (font, data, g, out var n); return (ok, n); };
			Func<string, ValueTuple<bool, uint>> fromName = f.fromNameDelegate == null ? null :
				n => { var ok = f.fromNameDelegate (font, data, n, out var g); return (ok, g); };
			Func<ValueTuple<bool, int, int, int>> hExtents = f.hExtentsDelegate == null ? null :
				() => { var ok = f.hExtentsDelegate (font, data, out var e); return (ok, e.Ascender, e.Descender, e.LineGap); };
			Func<ValueTuple<bool, int, int, int>> vExtents = f.vExtentsDelegate == null ? null :
				() => { var ok = f.vExtentsDelegate (font, data, out var e); return (ok, e.Ascender, e.Descender, e.LineGap); };

			return FontFuncsBuilder.Build (xScale, yScale, nominal, variation, hAdvance, vAdvance, hOrigin, vOrigin, kerning, extents, contour, name, fromName, hExtents, vExtents);
		}
	}
}
