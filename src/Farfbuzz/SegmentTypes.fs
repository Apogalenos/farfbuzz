namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>One glyph with its properties, the unit every syllable edit moves (<c>hb_glyph_info_t</c> incl. <c>var1</c>/<c>var2</c>).</summary>
[<Struct>]
type internal GlyphRow =
    { Info: GlyphInfo
      Props: GlyphProps }

/// <summary>A cluster merge or flag change that crosses a syllable-chunk boundary, applied by <see cref="Sealed.rewriteSegments"/>.</summary>
[<Struct>]
type internal BoundaryPatch =
    /// <summary>The maximal adjacent run whose cluster equals <paramref name="runCluster"/> takes <paramref name="mergedCluster"/>, with the glyph flags reset as <c>set_cluster</c> does.</summary>
    | MergeRun of runCluster: uint32 * mergedCluster: uint32
    /// <summary>Marks the two glyphs on either side of the chunk's left edge unsafe to break (<c>unsafe_to_break (start - 1, start + 1)</c>).</summary>
    | UnsafeToBreakAcross
    /// <summary>Sets <paramref name="mask"/> on the first glyph of the chunk when the glyph before it is not a letter, a mark or a format character, as a word start, else marks the two glyphs on either side of the chunk's left edge unsafe to break (<c>src/hb-ot-shaper-indic.cc:1430-1439</c>).</summary>
    | InitOrUnsafeToBreakAcross of mask: uint32

/// <summary>The result of rewriting one syllable chunk: its rows (empty means unchanged) and the patches to apply across its boundaries.</summary>
[<Struct>]
type internal SegmentRewrite =
    { Rows: ImmutableArray<GlyphRow>
      Left: ImmutableArray<BoundaryPatch>
      Right: ImmutableArray<BoundaryPatch> }

/// <summary>Builds the values of <see cref="SegmentRewrite"/>.</summary>
[<RequireQualifiedAccess>]
module internal SegmentRewrite =
    /// <summary>The rewrite that keeps a chunk's rows and applies nothing across its boundaries.</summary>
    let unchanged : SegmentRewrite =
        { Rows = ImmutableArray.Empty
          Left = ImmutableArray.Empty
          Right = ImmutableArray.Empty }

/// <summary>Rewrites one syllable chunk: <see cref="Sealed.rewriteSegments"/> hands it the chunk's rows and writes back its result.</summary>
type internal ISegmentRewriter =
    /// <summary>Rewrites one chunk's rows, returning the same-length rows and any boundary patches.</summary>
    /// <param name="rows">The chunk's rows.</param>
    /// <returns>The rewrite, or <see cref="SegmentRewrite.unchanged"/> to keep the rows.</returns>
    abstract Rewrite: rows: ReadOnlySpan<GlyphRow> -> SegmentRewrite
