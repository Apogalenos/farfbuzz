namespace Farfbuzz

open System
open System.Runtime.InteropServices
open System.Text

/// <summary>A design-space coordinate on one variation axis, laid out exactly as <c>hb_variation_t</c> (8 bytes).</summary>
[<Struct; StructLayout(LayoutKind.Sequential)>]
type Variation =
    {
        Tag: Tag        // the axis tag, for example wght
        Value: float32  // the coordinate in the axis' user units
    }

/// <summary>Parsing and formatting of variation strings, as <c>hb_variation_from_string</c> and <c>hb_variation_to_string</c>.</summary>
[<RequireQualifiedAccess>]
module Variation =
    /// <summary>Reads an optional <c>=</c> and a number, <c>parse_variation_value</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <param name="index">The current offset.</param>
    /// <returns>The value and the offset after it.</returns>
    let private value (text: byte array) (index: int) : Result<Scanned<float32>, ParseError> =
        let struct (_, afterEqual) = Syntax.char text index '='B
        match Number.parseDouble (ReadOnlySpan text) afterEqual false with
        | ValueSome scanned -> Ok { Value = float32 scanned.Value; Next = scanned.Next }
        | ValueNone -> Error (UnexpectedEnd (afterEqual, "a number"))

    /// <summary>Parses a variation string given as UTF-8 bytes, exactly as <c>hb_variation_from_string</c>.</summary>
    /// <param name="text">The UTF-8 input.</param>
    /// <returns>The variation, or why the string is not one.</returns>
    let parseUtf8 (text: byte array) : Result<Variation, ParseError> =
        result {
            let! tag = Syntax.tag text 0
            let! coordinate = value text tag.Next
            do! Syntax.finished text coordinate.Next
            return { Tag = tag.Value; Value = coordinate.Value }
        }

    /// <summary>Parses a variation string such as <c>wght=500</c>, <c>wdth 75</c> or <c>slnt=-7.5</c>.</summary>
    /// <param name="text">The variation string.</param>
    /// <returns>The variation, or why the string is not one.</returns>
    let parse (text: string) : Result<Variation, ParseError> =
        parseUtf8 (Encoding.UTF8.GetBytes text)

    /// <summary>Formats a variation as <c>tag=value</c>, the value as C's <c>%g</c>, exactly as <c>hb_variation_to_string</c>.</summary>
    /// <param name="variation">The variation.</param>
    /// <returns>The text, without white space.</returns>
    let format (variation: Variation) : string =
        Feature.trimmedTag variation.Tag + "=" + Number.formatG (double variation.Value)
