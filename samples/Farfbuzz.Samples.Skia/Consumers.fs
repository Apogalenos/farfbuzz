module Farfbuzz.Samples.Skia.Consumers

open System
open System.Globalization
open System.IO
open SkiaSharp
open SkiaSharp.HarfBuzz
open Topten.RichTextKit

/// <summary>A line of the corpus with the font file that shapes it.</summary>
type Line = { Name: string; Font: string; Text: string }

/// <summary>The lines: Latin with features, Arabic and Devanagari.</summary>
let lines (assets: string) =
    [ { Name = "latin"; Font = Path.Combine(assets, "Roboto-Regular.ttf"); Text = "Office waffles: fluffy 12.5% off, AVATAR." }
      { Name = "latin-ligatures"; Font = Path.Combine(assets, "Roboto-Regular.ttf"); Text = "fi fl ffi 1/2 Th" }
      { Name = "arabic"; Font = Path.Combine(assets, "Amiri-Regular.ttf"); Text = "العربية مرحبا بالعالم" }
      { Name = "devanagari"; Font = Path.Combine(assets, "NotoSansDevanagari-Regular.ttf"); Text = "हिन्दी क्षत्रिय संस्कृत" } ]

/// <summary>Formats a float the same way on every run.</summary>
/// <param name="value">The value.</param>
/// <returns>The text.</returns>
let number (value: float32) = value.ToString("R", CultureInfo.InvariantCulture)

/// <summary>Shapes a line with SkiaSharp.HarfBuzz's <c>SKShaper</c>, as <c>CanvasExtensions.DrawShapedText</c> does.</summary>
/// <param name="line">The line.</param>
/// <returns>The output lines.</returns>
let shapeWithSkShaper (line: Line) =
    use typeface = SKTypeface.FromFile line.Font
    use shaper = new SKShaper(typeface)
    use font = new SKFont(typeface, 16.0f)
    let result = shaper.Shape(line.Text, font)
    [ for index in 0 .. result.Codepoints.Length - 1 ->
        $"skshaper\t{line.Name}\t{index}\t{result.Codepoints[index]}\t{result.Clusters[index]}\t{number result.Points[index].X}\t{number result.Points[index].Y}" ]

/// <summary>A font mapper that answers every style with the typeface of one file, so that the run does not depend on installed fonts.</summary>
type FileFontMapper(typeface: SKTypeface) =
    inherit FontMapper()
    override _.TypefaceFromStyle(_, _) = typeface

/// <summary>Lays a line out with RichTextKit's <c>TextBlock</c> and writes the glyphs of its font runs.</summary>
/// <param name="line">The line.</param>
/// <returns>The output lines.</returns>
let shapeWithRichTextKit (line: Line) =
    use typeface = SKTypeface.FromFile line.Font
    let block = TextBlock()
    block.FontMapper <- FileFontMapper typeface
    block.AddText(line.Text, Style(FontFamily = "File", FontSize = 16.0f))
    [ for run in block.FontRuns ->
        String.Join(";", [ for index in 0 .. run.Glyphs.Length - 1 -> $"{run.Glyphs[index]}@{run.Clusters[index]}:{number run.GlyphPositions[index].X},{number run.GlyphPositions[index].Y}" ])
      |> fun glyphs -> $"richtextkit\t{line.Name}\t{glyphs}" ]

/// <summary>Both consumers' output for the corpus, one line per glyph (SKShaper) and per font run (RichTextKit).</summary>
/// <param name="assets">The directory of the three fonts.</param>
/// <returns>The output lines.</returns>
let output (assets: string) =
    lines assets |> List.collect (fun line -> shapeWithSkShaper line @ shapeWithRichTextKit line)
