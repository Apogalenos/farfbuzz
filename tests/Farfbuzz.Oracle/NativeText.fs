module Farfbuzz.Oracle.NativeText

#nowarn "9"

open System
open System.Text
open Microsoft.FSharp.NativeInterop
open Farfbuzz

/// <summary>The serialisation format tag TEXT, <c>HB_BUFFER_SERIALIZE_FORMAT_TEXT</c>.</summary>
[<Literal>]
let TextFormat = 0x54455854u

/// <summary>The content type for glyph buffers, <c>HB_BUFFER_CONTENT_TYPE_GLYPHS</c>.</summary>
[<Literal>]
let GlyphsContent = 2

/// <summary>Decodes a NUL-terminated byte buffer, each byte as a Latin-1 character.</summary>
/// <param name="bytes">The buffer.</param>
/// <returns>The text up to the first NUL.</returns>
let latin1 (bytes: byte array) =
    let length = match Array.IndexOf(bytes, 0uy) with -1 -> bytes.Length | nul -> nul
    Encoding.Latin1.GetString(bytes, 0, length)

/// <summary>The oracle's <c>hb_tag_from_string</c> on a string's UTF-8 bytes.</summary>
/// <param name="text">The spelling.</param>
/// <returns>The tag value.</returns>
let tagFromString (text: string) =
    let bytes = Encoding.UTF8.GetBytes text
    Native.hb_tag_from_string(bytes, bytes.Length)

/// <summary>The oracle's <c>hb_feature_from_string</c>.</summary>
/// <param name="text">The feature string.</param>
/// <returns>The feature, or none when rejected.</returns>
let featureFromString (text: string) : Feature voption =
    let bytes = Encoding.UTF8.GetBytes text
    let mutable feature = Unchecked.defaultof<Feature>
    match Native.hb_feature_from_string(bytes, bytes.Length, &feature) with
    | 0 -> ValueNone
    | _ -> ValueSome feature

/// <summary>The oracle's <c>hb_feature_to_string</c>.</summary>
/// <param name="feature">The feature.</param>
/// <returns>The text.</returns>
let featureToString (feature: Feature) =
    let output = Array.zeroCreate 128
    let mutable copy = feature
    Native.hb_feature_to_string(&copy, output, 128u)
    latin1 output

/// <summary>The oracle's <c>hb_variation_from_string</c>.</summary>
/// <param name="text">The variation string.</param>
/// <returns>The variation, or none when rejected.</returns>
let variationFromString (text: string) : Variation voption =
    let bytes = Encoding.UTF8.GetBytes text
    let mutable variation = Unchecked.defaultof<Variation>
    match Native.hb_variation_from_string(bytes, bytes.Length, &variation) with
    | 0 -> ValueNone
    | _ -> ValueSome variation

/// <summary>The oracle's <c>hb_variation_to_string</c>.</summary>
/// <param name="variation">The variation.</param>
/// <returns>The text.</returns>
let variationToString (variation: Variation) =
    let output = Array.zeroCreate 128
    let mutable copy = variation
    Native.hb_variation_to_string(&copy, output, 128u)
    latin1 output

/// <summary>Copies a native array into a managed one.</summary>
/// <param name="pointer">The first item.</param>
/// <param name="length">The number of items.</param>
/// <returns>The copy.</returns>
let copyFrom<'T when 'T: unmanaged> (pointer: nativeint) (length: int) : 'T array =
    ReadOnlySpan<'T>(NativePtr.toVoidPtr (NativePtr.ofNativeInt<'T> pointer), length).ToArray()

/// <summary>Copies a managed array into native memory.</summary>
/// <param name="items">The items.</param>
/// <param name="pointer">The destination.</param>
let copyInto<'T when 'T: unmanaged> (items: 'T array) (pointer: nativeint) =
    ReadOnlySpan<'T>(items).CopyTo(Span<'T>(NativePtr.toVoidPtr (NativePtr.ofNativeInt<'T> pointer), items.Length))

/// <summary>Runs an action on a fresh native buffer, destroying it afterwards.</summary>
/// <param name="action">The action, given the buffer handle.</param>
/// <returns>The action's result.</returns>
let withBuffer (action: nativeint -> 'T) : 'T =
    let buffer = Native.hb_buffer_create ()
    let result = action buffer
    Native.hb_buffer_destroy buffer
    result

/// <summary>The oracle's text serialisation of glyphs, with no font (names are then always <c>gidN</c>, no extents).</summary>
/// <param name="flags">The flags.</param>
/// <param name="infos">The glyphs.</param>
/// <param name="positions">The positions, as long as the glyphs.</param>
/// <returns>The text.</returns>
let serializeGlyphs (flags: SerializeFlags) (infos: GlyphInfo array) (positions: GlyphPosition array) =
    withBuffer (fun buffer ->
        Native.hb_buffer_set_content_type(buffer, GlyphsContent)
        Native.hb_buffer_set_length(buffer, uint32 infos.Length) |> ignore
        let mutable length = 0u
        copyInto infos (Native.hb_buffer_get_glyph_infos(buffer, &length))
        copyInto positions (Native.hb_buffer_get_glyph_positions(buffer, &length))
        let output = Array.zeroCreate (infos.Length * 256 + 16)
        let mutable consumed = 0u
        Native.hb_buffer_serialize_glyphs(buffer, 0u, uint32 infos.Length, output, uint32 output.Length, &consumed, 0n, TextFormat, uint32 flags) |> ignore
        latin1 output)

/// <summary>The oracle's text deserialisation of glyphs, with no font.</summary>
/// <param name="text">The serialised glyphs.</param>
/// <returns>The glyphs and positions, or none when rejected.</returns>
let deserializeGlyphs (text: string) : (GlyphInfo array * GlyphPosition array) voption =
    withBuffer (fun buffer ->
        let bytes = Encoding.UTF8.GetBytes text
        let mutable stop = 0n
        match Native.hb_buffer_deserialize_glyphs(buffer, bytes, bytes.Length, &stop, 0n, TextFormat) with
        | 0 -> ValueNone
        | _ ->
            let mutable length = 0u
            let infos = Native.hb_buffer_get_glyph_infos(buffer, &length)
            let positions = Native.hb_buffer_get_glyph_positions(buffer, &length)
            ValueSome (copyFrom<GlyphInfo> infos (int length), copyFrom<GlyphPosition> positions (int length)))
