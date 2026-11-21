module Farfbuzz.Harness.Shaper

open Farfbuzz
open Farfbuzz.Harness.ShapeRequest

/// <summary>The output of one shaping run and the font queries the serialiser and the gid normalisation need.</summary>
type ShapedRun =
    { Infos: GlyphInfo array
      Positions: GlyphPosition array
      GlyphName: uint32 -> string voption       // the font's name for a glyph
      Extents: uint32 -> GlyphExtents voption   // the font's ink box for a glyph, at the font's scale
      GlyphFromName: string -> uint32 voption   // the font's glyph for a name
      NominalGlyph: uint32 -> uint32 voption }  // the font's glyph for a code point

/// <summary>What hb-shape prints for one run.</summary>
type ShapeOutcome =
    | Shaped of run: ShapedRun
    | ShapingFailed  // hb_shape_full returned false, for example because --verify found a problem

/// <summary>A shaping engine the harness can drive: Farfbuzz, or HarfBuzz through HarfBuzzSharp.</summary>
type Shaper =
    { Name: string
      Shapers: string list      // what hb-shape --list-shapers would print
      FaceLoaders: string list  // what hb-shape --list-face-loaders would print
      FontFuncs: string list    // what hb-shape --list-font-funcs would print
      Shape: ShapeRequest -> Result<ShapeOutcome, string> }  // Error: the engine cannot run this request at all

/// <summary>The line hb-shape prints when shaping fails.</summary>
[<Literal>]
let ShapingFailedLine = "error: Shaping failed."
