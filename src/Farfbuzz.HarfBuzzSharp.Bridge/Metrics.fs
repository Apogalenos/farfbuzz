namespace Farfbuzz.HarfBuzzSharp.Bridge

open Farfbuzz

/// <summary>OpenType metrics for the HarfBuzzSharp shell: <c>hb-ot-metrics.cc</c> over a core font.</summary>
[<AbstractClass; Sealed>]
type Metrics =
    /// <summary><c>hb_ot_metrics_get_position</c> (hb-ot-metrics.cc:142-240).</summary>
    /// <param name="font">The core font.</param>
    /// <param name="tag">The <c>hb_ot_metrics_tag_t</c> value, a tag.</param>
    /// <returns>Whether the metric exists, and its position at the font's scale (0 when it does not).</returns>
    static member Position (font: Font, tag: uint32) =
        match Farfbuzz.Metrics.getPosition font (LanguagePrimitives.UInt32WithMeasure tag) with
        | ValueSome position -> struct (true, position)
        | ValueNone -> struct (false, 0)

    /// <summary><c>hb_ot_metrics_get_variation</c>: the variation of a metric in font units.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="tag">The metrics tag.</param>
    /// <returns>The variation.</returns>
    static member Variation (font: Font, tag: uint32) =
        Font.metricsVariation font (LanguagePrimitives.UInt32WithMeasure tag)

    /// <summary><c>hb_ot_metrics_get_x_variation</c>: the horizontal variation scaled to the font.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="tag">The metrics tag.</param>
    /// <returns>The variation.</returns>
    static member XVariation (font: Font, tag: uint32) =
        Font.metricsXVariation font (LanguagePrimitives.UInt32WithMeasure tag)

    /// <summary><c>hb_ot_metrics_get_y_variation</c>: the vertical variation scaled to the font.</summary>
    /// <param name="font">The core font.</param>
    /// <param name="tag">The metrics tag.</param>
    /// <returns>The variation.</returns>
    static member YVariation (font: Font, tag: uint32) =
        Font.metricsYVariation font (LanguagePrimitives.UInt32WithMeasure tag)
