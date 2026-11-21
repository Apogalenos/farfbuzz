namespace Farfbuzz.Samples.Avalonia11

open System
open System.Globalization
open Avalonia.Media
open Avalonia.Media.TextFormatting

/// <summary>The lines the samples shape and the way they are shaped.</summary>
module Corpus =
    /// <summary>One line with the family that shapes it.</summary>
    type Line = { Name: string; Family: string; Text: string; BidiLevel: sbyte; Culture: string }

    /// <summary>The corpus: Latin, Arabic, Devanagari, a line ending in CRLF and one in LF (Avalonia merges the break pair), a tab, letter spacing 0.</summary>
    let lines =
        [ { Name = "latin"; Family = "Roboto"; Text = "Office waffles: fluffy 12.5% off, AVATAR."; BidiLevel = 0y; Culture = "en-US" }
          { Name = "latin-features"; Family = "Roboto"; Text = "fi fl ffi 1/2 Th"; BidiLevel = 0y; Culture = "en-US" }
          { Name = "arabic"; Family = "Amiri"; Text = "العربية مرحبا بالعالم"; BidiLevel = 1y; Culture = "ar-EG" }
          { Name = "devanagari"; Family = "Noto Sans Devanagari"; Text = "हिन्दी क्षत्रिय संस्कृत"; BidiLevel = 0y; Culture = "hi-IN" }
          { Name = "crlf"; Family = "Roboto"; Text = "first line\r\n"; BidiLevel = 0y; Culture = "en-US" }
          { Name = "lf"; Family = "Roboto"; Text = "second line\n"; BidiLevel = 0y; Culture = "en-US" }
          { Name = "tab"; Family = "Roboto"; Text = "a\tb\tc"; BidiLevel = 0y; Culture = "en-US" } ]

    /// <summary>Shapes one line through Avalonia's current text shaper and writes <c>index, cluster, advance, offset</c> for each glyph.</summary>
    /// <param name="line">The line.</param>
    /// <returns>The output lines.</returns>
    let shape (line: Line) =
        let family = FontFamily($"avares://Farfbuzz.Samples.Avalonia11/Assets#{line.Family}")
        let typeface = Typeface family
        let glyphTypeface = typeface.GlyphTypeface
        let options = TextShaperOptions(glyphTypeface, 16.0, line.BidiLevel, CultureInfo line.Culture, 32.0, 0.0)
        let buffer = TextShaper.Current.ShapeText(line.Text.AsMemory(), options)
        [ for index in 0 .. buffer.Length - 1 ->
            let glyph = buffer[index]
            String.Join('\t', [ line.Name; string index; string glyph.GlyphIndex; string glyph.GlyphCluster; glyph.GlyphAdvance.ToString("R", CultureInfo.InvariantCulture); glyph.GlyphOffset.X.ToString("R", CultureInfo.InvariantCulture); glyph.GlyphOffset.Y.ToString("R", CultureInfo.InvariantCulture) ]) ]
