namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open Farfbuzz

/// <summary>The direction, script and language of a buffer as the shell holds them: <c>hb_direction_t</c> values, the script tag and the canonical language name, null for the invalid language (hb_segment_properties_t, hb-common.h).</summary>
[<Struct>]
type SegmentProps = { Direction: int; Script: uint32; Language: string | null }

/// <summary>Scripts, languages and segment properties for the HarfBuzzSharp shell (hb-common.cc, hb-buffer.cc:746-775).</summary>
[<AbstractClass; Sealed>]
type internal Segments =
    /// <summary>Parses a script string as <c>hb_script_from_string</c> with length -1 (hb-common.cc); a null string gives the invalid script, as a NULL pointer does.</summary>
    /// <param name="text">The ISO 15924 string.</param>
    /// <returns>The script tag value.</returns>
    static member ScriptFromString (text: string | null) =
        match text with
        | null -> 0u
        | spelling -> uint32 (Script.ofString spelling)

    /// <summary>The horizontal direction of a script, <c>hb_script_get_horizontal_direction</c> (hb-common.cc).</summary>
    /// <param name="script">The script tag value.</param>
    /// <returns>The <c>hb_direction_t</c> value: 4 left to right, 5 right to left, 0 for a script of no fixed direction.</returns>
    static member ScriptHorizontalDirection (script: uint32) =
        int (Direction.value (Script.horizontalDirection (LanguagePrimitives.UInt32WithMeasure script)))

    /// <summary>The canonical form of a language string, <c>hb_language_from_string</c> then <c>hb_language_to_string</c> (hb-common.cc:325-396).</summary>
    /// <param name="text">The BCP 47 string.</param>
    /// <returns>The canonical name, or none for the invalid language (a null or empty string); the shell shows none as a null name.</returns>
    static member LanguageFromString (text: string | null) =
        match text with
        | null -> ValueNone
        | spelling -> Language.toString (Language.ofString spelling)

    /// <summary>The default language, always the C locale <c>"c"</c> (hb.hh:505-528, hb-common.cc:384-396); no locale is read.</summary>
    static member DefaultLanguage =
        match Language.toString Language.cLocale with
        | ValueSome canonical -> canonical
        | ValueNone -> String.Empty

    /// <summary>The core form of segment properties: the direction value, the script tag and the language, parsed again from its canonical name.</summary>
    static member internal ToCore (props: SegmentProps) =
        let direction =
            match props.Direction with
            | 4 -> Direction.Ltr
            | 5 -> Direction.Rtl
            | 6 -> Direction.Ttb
            | 7 -> Direction.Btt
            | _ -> Direction.Invalid
        let language =
            match props.Language with
            | null -> NoLanguage
            | name -> Language.ofString name
        { SegmentProperties.Direction = direction; Script = LanguagePrimitives.UInt32WithMeasure props.Script; Language = language }

    /// <summary>The shell form of core segment properties.</summary>
    static member internal OfCore (props: SegmentProperties) =
        match Language.toString props.Language with
        | ValueSome name -> { SegmentProps.Direction = int (Direction.value props.Direction); Script = uint32 props.Script; Language = name }
        | ValueNone -> { SegmentProps.Direction = int (Direction.value props.Direction); Script = uint32 props.Script; Language = null }

    /// <summary>Fills the segment properties that are unset from the glyphs, <c>hb_buffer_guess_segment_properties</c> (hb-buffer.cc:746-775):
    /// the script of the first glyph that is no Common, Inherited or Unknown one, the horizontal direction of that script (left to right when it has none),
    /// and the default language <c>"c"</c> (hb-buffer.cc:773 with hb.hh:527). The scan is the span form of the one <c>GlyphRun.guessSegmentProperties</c> makes.</summary>
    /// <param name="input">The glyph infos of the buffer.</param>
    /// <param name="current">The properties the buffer has.</param>
    /// <param name="unicode">The Unicode functions the buffer has when it has replaced ones, else null for the default functions.</param>
    /// <returns>The properties with the unset parts guessed.</returns>
    static member Guess (input: ReadOnlySpan<GlyphInfo>, current: SegmentProps, unicode: UnicodeFuncs | null) =
        let first =
            match unicode with
            | null -> GlyphRun.firstNonCommonScriptOf input 0
            | replaced -> GlyphRun.firstNonCommonScriptThrough replaced.Script input 0
        let guessed = GlyphRun.guessWith Language.cLocale (Segments.ToCore current) first
        Segments.OfCore guessed

    /// <summary>Overlays unset properties of a buffer with those of another, <c>hb_segment_properties_overlay</c> (hb-common.cc), as <c>hb_buffer_append</c> does.</summary>
    /// <param name="target">The properties of the buffer appended to.</param>
    /// <param name="source">The properties of the buffer appended from.</param>
    /// <returns>The overlaid properties.</returns>
    static member Overlay (target: SegmentProps, source: SegmentProps) =
        Segments.OfCore (GlyphRun.overlayProperties (Segments.ToCore target) (Segments.ToCore source))
