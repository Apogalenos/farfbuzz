// Shapes one line through the installed facade and fails unless the HarfBuzzSharp assembly that loaded is Farfbuzz's and the
// line shaped as expected: in Roboto, "office" forms the "ffi" ligature, so the thirteen characters give eleven glyphs.
using System;
using System.Diagnostics;
using System.Linq;
using HarfBuzzSharp;

const string text = "Hello, office";
const int expectedGlyphs = 11;

var assembly = typeof(Blob).Assembly;
var version = FileVersionInfo.GetVersionInfo(assembly.Location).ProductVersion ?? string.Empty;
using var blob = Blob.FromFile(args[0]);
using var face = new Face(blob, 0);
using var font = new Font(face);
using var buffer = new HarfBuzzSharp.Buffer();
buffer.AddUtf8(text);
buffer.GuessSegmentProperties();
font.Shape(buffer);
var advance = buffer.GlyphPositions.Sum(position => position.XAdvance);
Console.WriteLine($"{assembly.GetName().Name} {version}: \"{text}\" shaped into {buffer.Length} glyphs from {text.Length} characters, {advance} font units wide");

var failures =
    new[]
    {
        (version.Contains("+farfbuzz"), "the HarfBuzzSharp assembly that loaded is not the Farfbuzz facade"),
        (buffer.Length == expectedGlyphs, $"expected {expectedGlyphs} glyphs"),
        (advance > 0, "the line has no advance"),
    }
    .Where(check => !check.Item1)
    .Select(check => check.Item2)
    .ToArray();
foreach (var failure in failures)
{
    Console.Error.WriteLine(failure);
}
return failures.Length == 0 ? 0 : 1;
