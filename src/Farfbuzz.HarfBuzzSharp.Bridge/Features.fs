namespace Farfbuzz.HarfBuzzSharp.Bridge

open Farfbuzz

/// <summary>Feature strings for the HarfBuzzSharp shell: <c>hb_feature_from_string</c> and <c>hb_feature_to_string</c> (hb-common.cc).</summary>
[<AbstractClass; Sealed>]
type Features =
    /// <summary>Parses a feature string as <c>hb_feature_from_string</c> with length -1.</summary>
    /// <param name="text">The feature string.</param>
    /// <returns>Whether the string parsed, and the feature (all zero when it did not).</returns>
    static member Parse (text: string) =
        match Feature.parse text with
        | Ok feature -> struct (true, feature)
        | Error _ -> struct (false, Unchecked.defaultof<Feature>)

    /// <summary>Formats a feature as <c>hb_feature_to_string</c>.</summary>
    /// <param name="feature">The feature.</param>
    /// <returns>The text.</returns>
    static member Format (feature: Feature) =
        Feature.format feature
