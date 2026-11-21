namespace Farfbuzz

open System
open System.Collections.Frozen
open System.Collections.Immutable
open System.Text

[<Struct>]
type Direction =
    | Invalid
    | Ltr
    | Rtl
    | Ttb
    | Btt

/// <summary>Direction values and predicates, <c>hb-common.cc</c>.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Direction =
    /// <summary>Returns the numeric value, <c>hb_direction_t</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>0, 4, 5, 6 or 7.</returns>
    let value (direction: Direction) : uint32 =
        match direction with
        | Invalid -> 0u
        | Ltr -> 4u
        | Rtl -> 5u
        | Ttb -> 6u
        | Btt -> 7u

    /// <summary>Reads the first letter, <c>hb_direction_from_string</c>.</summary>
    /// <param name="text">The input string.</param>
    /// <returns>The direction, or <c>Invalid</c>.</returns>
    let ofString (text: string) : Direction =
        let bytes = Encoding.UTF8.GetBytes text
        match bytes.Length with
        | 0 -> Invalid
        | _ when bytes[0] = 0uy -> Invalid
        | _ ->
            match Number.toLower bytes[0] with
            | 'l'B -> Ltr
            | 'r'B -> Rtl
            | 't'B -> Ttb
            | 'b'B -> Btt
            | _ -> Invalid

    /// <summary>Spells a direction, <c>hb_direction_to_string</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>Four lower-case letters, or <c>invalid</c>.</returns>
    let toString (direction: Direction) =
        match direction with
        | Invalid -> "invalid"
        | Ltr -> "ltr"
        | Rtl -> "rtl"
        | Ttb -> "ttb"
        | Btt -> "btt"

    /// <summary>Whether a direction is horizontal, <c>HB_DIRECTION_IS_HORIZONTAL</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>True for LTR and RTL.</returns>
    let isHorizontal (direction: Direction) =
        match direction with
        | Ltr | Rtl -> true
        | Invalid | Ttb | Btt -> false

    /// <summary>Whether a direction is backward, <c>HB_DIRECTION_IS_BACKWARD</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>True for RTL and BTT.</returns>
    let isBackward (direction: Direction) =
        match direction with
        | Rtl | Btt -> true
        | Invalid | Ltr | Ttb -> false

    /// <summary>Reverses a direction, <c>HB_DIRECTION_REVERSE</c>.</summary>
    /// <param name="direction">The direction.</param>
    /// <returns>The opposite direction, or <c>Invalid</c>.</returns>
    let reverse (direction: Direction) =
        match direction with
        | Invalid -> Invalid
        | Ltr -> Rtl
        | Rtl -> Ltr
        | Ttb -> Btt
        | Btt -> Ttb


/// <summary>The estimates of a space's width, <c>space_t</c>.</summary>
[<Struct>]
type internal SpaceFallback =
    | NotSpace
    | SpaceEm
    | SpaceEm2
    | SpaceEm3
    | SpaceEm4
    | SpaceEm5
    | SpaceEm6
    | SpaceEm16
    | Space4Em18
    | Space
    | SpaceFigure
    | SpacePunctuation
    | SpaceNarrow

/// <summary>A Unicode general category, <c>hb_unicode_general_category_t</c>.</summary>
[<Struct>]
type internal GeneralCategory =
    | Control
    | Format
    | Unassigned
    | PrivateUse
    | Surrogate
    | LowercaseLetter
    | ModifierLetter
    | OtherLetter
    | TitlecaseLetter
    | UppercaseLetter
    | SpacingMark
    | EnclosingMark
    | NonSpacingMark
    | DecimalNumber
    | LetterNumber
    | OtherNumber
    | ConnectPunctuation
    | DashPunctuation
    | ClosePunctuation
    | FinalPunctuation
    | InitialPunctuation
    | OtherPunctuation
    | OpenPunctuation
    | CurrencySymbol
    | ModifierSymbol
    | MathSymbol
    | OtherSymbol
    | LineSeparator
    | ParagraphSeparator
    | SpaceSeparator

/// <summary>A BCP 47 language, <c>hb_language_t</c>.</summary>
[<Struct>]
type Language =
    | NoLanguage
    | Bcp47 of canonical: string

/// <summary>The direction, script and language of a segment.</summary>
[<Struct>]
type SegmentProperties =
    { Direction: Direction
      Script: Script
      Language: Language }

/// <summary>Language canonicalisation and matching, <c>hb-common.cc</c>.</summary>
[<RequireQualifiedAccess>]
module Language =
    /// <summary>Maps one byte through upstream's <c>canon_map</c>.</summary>
    /// <param name="value">The byte.</param>
    /// <returns>The canonical byte, or 0 at the end of the C string.</returns>
    let internal canonicalByte (value: byte) =
        match value with
        | 45uy -> 45uy
        | value when value >= 48uy && value <= 57uy -> value
        | value when value >= 65uy && value <= 90uy -> value + 32uy
        | value when value >= 97uy && value <= 122uy -> value
        | 95uy -> 45uy
        | _ -> 0uy

    /// <summary>Finds the first canonical NUL in a bounded byte sequence.</summary>
    /// <param name="bytes">The bytes.</param>
    /// <param name="length">The bounded length.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The first index whose canonical value is 0, or the length.</returns>
    [<TailCall>]
    let rec private canonicalEnd (bytes: byte array) (length: int) (index: int) =
        match index < length with
        | false -> length
        | true ->
            match canonicalByte bytes[index] with
            | 0uy -> index
            | _ -> canonicalEnd bytes length (index + 1)

    /// <summary>Appends canonical bytes to a builder.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="bytes">The bytes.</param>
    /// <param name="last">The exclusive end.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The builder.</returns>
    [<TailCall>]
    let rec private appendCanonical (builder: StringBuilder) (bytes: byte array) (last: int) (index: int) =
        match index < last with
        | false -> builder
        | true ->
            builder.Append(canonicalByte bytes[index] |> char) |> ignore
            appendCanonical builder bytes last (index + 1)

    /// <summary>Canonicalises a BCP 47 string, <c>hb_language_from_string</c>.</summary>
    /// <param name="text">The input string.</param>
    /// <returns>The language, or <c>NoLanguage</c> for an empty input.</returns>
    let ofString (text: string) : Language =
        let bytes = Encoding.UTF8.GetBytes text
        let length = min bytes.Length 63
        match length with
        | 0 -> NoLanguage
        | _ when bytes[0] = 0uy -> NoLanguage
        | _ ->
            let last = canonicalEnd bytes length 0
            let builder = StringBuilder(last)
            Bcp47 ((appendCanonical builder bytes last 0).ToString())

    /// <summary>Returns the canonical string, <c>hb_language_to_string</c>.</summary>
    /// <param name="language">The language.</param>
    /// <returns>The string, or none for <c>NoLanguage</c>.</returns>
    let toString (language: Language) : string voption =
        match language with
        | NoLanguage -> ValueNone
        | Bcp47 canonical -> ValueSome canonical

    /// <summary>Tests whether a language is the same as or more specific than another, <c>hb_language_matches</c>.</summary>
    /// <param name="language">The broader language.</param>
    /// <param name="specific">The candidate specific language.</param>
    /// <returns>True when they match.</returns>
    let matches (language: Language) (specific: Language) : bool =
        match language, specific with
        | NoLanguage, NoLanguage -> true
        | NoLanguage, _ | _, NoLanguage -> false
        | Bcp47 broad, Bcp47 narrow ->
            match broad = narrow with
            | true -> true
            | false ->
                match broad.Length > narrow.Length with
                | true -> false
                | false ->
                    String.CompareOrdinal(broad, 0, narrow, 0, broad.Length) = 0
                    && (narrow.Length = broad.Length || narrow[broad.Length] = '-')

    /// <summary>The C-locale language used by the shaping suite, <c>Language.ofString "C"</c>.</summary>
    let cLocale = ofString "C"

/// <summary>Script parsing and horizontal direction, <c>hb-common.cc</c>.</summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Script =
    /// <summary>The <c>Aran</c> Arabic alias.</summary>
    [<Literal>]
    let private Aran = 0x4172616Eu

    /// <summary>The <c>Cyrs</c> Cyrillic alias.</summary>
    [<Literal>]
    let private Cyrs = 0x43797273u

    /// <summary>The <c>Geok</c> Georgian alias.</summary>
    [<Literal>]
    let private Geok = 0x47656F6Bu

    /// <summary>The <c>Hans</c> Han alias.</summary>
    [<Literal>]
    let private Hans = 0x48616E73u

    /// <summary>The <c>Hant</c> Han alias.</summary>
    [<Literal>]
    let private Hant = 0x48616E74u

    /// <summary>The <c>Jamo</c> Hangul alias.</summary>
    [<Literal>]
    let private Jamo = 0x4A616D6Fu

    /// <summary>The <c>Latf</c> Latin alias.</summary>
    [<Literal>]
    let private Latf = 0x4C617466u

    /// <summary>The <c>Latg</c> Latin alias.</summary>
    [<Literal>]
    let private Latg = 0x4C617467u

    /// <summary>The <c>Syre</c> Syriac alias.</summary>
    [<Literal>]
    let private Syre = 0x53797265u

    /// <summary>The <c>Syrj</c> Syriac alias.</summary>
    [<Literal>]
    let private Syrj = 0x5379726Au

    /// <summary>The <c>Syrn</c> Syriac alias.</summary>
    [<Literal>]
    let private Syrn = 0x5379726Eu

    /// <summary>Script parsing and horizontal direction, <c>hb-common.cc</c>.</summary>
    /// <summary>Converts an ISO 15924 tag, <c>hb_script_from_iso15924_tag</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The script, or <c>Scripts.Unknown</c>.</returns>
    let ofIso15924 (tag: Tag) : Script =
        let value = uint32 tag
        match value with
        | 0u -> Scripts.Invalid
        | _ ->
            let forced = (value &&& 0xDFDFDFDFu) ||| 0x00202020u
            let script =
                match forced with
                | Aran -> Scripts.Arabic
                | Cyrs -> Scripts.Cyrillic
                | Geok -> Scripts.Georgian
                | Hans | Hant -> Scripts.Han
                | Jamo -> Scripts.Hangul
                | Latf | Latg -> Scripts.Latin
                | Syre | Syrj | Syrn -> Scripts.Syriac
                | _ when (forced &&& 0xE0E0E0E0u) = 0x40606060u -> LanguagePrimitives.UInt32WithMeasure forced
                | _ -> Scripts.Unknown
            script

    /// <summary>Converts a string, <c>hb_script_from_string</c>.</summary>
    /// <param name="text">The string.</param>
    /// <returns>The script.</returns>
    let ofString (text: string) : Script =
        ofIso15924 (Tag.ofString text)

    /// <summary>The right-to-left scripts as a frozen set, built once when the module is loaded: the direction of a script is
    /// asked once per line, and a scan of the generated array costs more than the rest of a one-glyph line's guess.</summary>
    let private rtlSet = FrozenSet.ToFrozenSet (Scripts.rtlScripts.AsSpan().ToArray())

    /// <summary>The scripts of no fixed direction as a frozen set, built once when the module is loaded.</summary>
    let private invalidDirectionSet = FrozenSet.ToFrozenSet (Scripts.invalidDirectionScripts.AsSpan().ToArray())

    /// <summary>Returns the horizontal direction of a script, <c>hb_script_get_horizontal_direction</c>.</summary>
    /// <param name="script">The script.</param>
    /// <returns>RTL for right-to-left scripts, Invalid for dual-direction scripts, else LTR.</returns>
    let horizontalDirection (script: Script) : Direction =
        match rtlSet.Contains script with
        | true -> Rtl
        | false ->
            match invalidDirectionSet.Contains script with
            | true -> Invalid
            | false -> Ltr
