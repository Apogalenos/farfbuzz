module Farfbuzz.HarfBuzzSharp.Tests.SerializeTests

open System
open System.Runtime.InteropServices
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Diff
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

[<Literal>]
let private TextTag = 0x54455854u

[<Literal>]
let private JsonTag = 0x4A534F4Eu

/// <summary>Copies glyphs and positions into a facade buffer through the live spans.</summary>
/// <param name="buffer">The facade buffer, of content type glyphs.</param>
/// <param name="infos">The glyphs.</param>
/// <param name="positions">The positions.</param>
let private fill (buffer: HarfBuzzSharp.Buffer) (infos: GlyphInfo array) (positions: GlyphPosition array) =
    buffer.ContentType <- HarfBuzzSharp.ContentType.Glyphs
    buffer.Length <- infos.Length
    let target = buffer.GetGlyphInfoSpan()
    MemoryMarshal.Cast<GlyphInfo, HarfBuzzSharp.GlyphInfo>(ReadOnlySpan<GlyphInfo> infos).CopyTo(MemoryMarshal.CreateSpan(&MemoryMarshal.GetReference target, target.Length))
    let moved = buffer.GetGlyphPositionSpan()
    MemoryMarshal.Cast<GlyphPosition, HarfBuzzSharp.GlyphPosition>(ReadOnlySpan<GlyphPosition> positions).CopyTo(MemoryMarshal.CreateSpan(&MemoryMarshal.GetReference moved, moved.Length))

/// <summary>The oracle's serialisation of the same glyphs.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="infos">The glyphs.</param>
/// <param name="positions">The positions.</param>
/// <param name="format">The format tag.</param>
/// <param name="flags">The flags.</param>
/// <returns>The text.</returns>
let private oracleText (font: nativeint) (infos: GlyphInfo array) (positions: GlyphPosition array) (format: uint32) (flags: uint32) =
    NativeText.withBuffer (fun buffer ->
        Native.hb_buffer_set_content_type(buffer, 2)
        Native.hb_buffer_set_length(buffer, uint32 infos.Length) |> ignore
        let mutable length = 0u
        NativeText.copyInto infos (Native.hb_buffer_get_glyph_infos(buffer, &length))
        NativeText.copyInto positions (Native.hb_buffer_get_glyph_positions(buffer, &length))
        let output = Array.zeroCreate (infos.Length * 400 + 64)
        let mutable consumed = 0u
        Native.hb_buffer_serialize_glyphs(buffer, 0u, uint32 infos.Length, output, uint32 output.Length, &consumed, font, format, flags) |> ignore
        NativeText.latin1 output)

/// <summary>Generates glyph runs over glyph ids that exist, clusters in order and positions over the whole int range.</summary>
/// <param name="glyphCount">The glyph count of the font.</param>
/// <returns>The generator.</returns>
let private runs (glyphCount: int) =
    gen {
        let! length = Gen.choose (1, 8)
        let! glyphs = Gen.listOfLength length (Gen.choose (0, glyphCount - 1))
        let! masks = Gen.listOfLength length (Gen.elements [ 0u; 1u; 2u; 4u; 7u; 0xF0u ])
        let! clusters = Gen.listOfLength length (Gen.choose (0, 5))
        let! numbers = Gen.listOfLength (4 * length) (Gen.frequency [ 6, Gen.choose (-3000, 3000); 1, Gen.elements [ Int32.MaxValue; Int32.MinValue; Int32.MaxValue - 1 ] ])
        let infos = [| for index in 0 .. length - 1 -> { Codepoint = uint32 glyphs[index]; Mask = masks[index]; Cluster = uint32 (List.take (index + 1) clusters |> List.sum); Var1 = 0u; Var2 = 0u } |]
        let positions = [| for index in 0 .. length - 1 -> { XAdvance = numbers[4 * index]; YAdvance = numbers[4 * index + 1]; XOffset = numbers[4 * index + 2]; YOffset = numbers[4 * index + 3]; Var = 0u } |]
        return infos, positions
    }

/// <summary>The font file of a face that has glyph names, so that names are exercised.</summary>
/// <returns>The path.</returns>
let private namedFont () =
    let named (path: string) =
        let font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
        match font.TryGetNominalGlyph 0x41u with
        | true, glyph -> fst (font.TryGetGlyphName glyph)
        | false, _ -> false
    TestFonts.files () |> List.map snd |> List.find named

/// <summary>The P10.S14 cases: serialisation and its inverse.</summary>
[<Tests>]
let tests =
    testList "P10.S14" [
        testCase "SerializeGlyphs equals the oracle in text and JSON for every flag combination" (fun () ->
            OracleSession.ensure ()
            let path = namedFont ()
            let faceIndex = 0
            let oracleFace = OracleFaces.face { Path = path; Suite = String.Empty; Index = faceIndex } (uint32 faceIndex)
            let oracle = Native.hb_font_create oracleFace
            Native.hb_ot_font_set_funcs oracle
            let upem = int (Native.hb_face_get_upem oracleFace)
            Native.hb_font_set_scale(oracle, upem, upem)
            use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path faceIndex)
            let cases = Gens.sample 40 (runs (int (Native.hb_face_get_glyph_count oracleFace)))
            let mutable compared = 0
            let differences =
                [ for (infos, positions) in cases do
                    for flags in 0u .. 63u do
                        for format in [ TextTag; JsonTag ] do
                            use buffer = new HarfBuzzSharp.Buffer()
                            fill buffer infos positions
                            let expected = oracleText oracle infos positions format flags
                            let actual = buffer.SerializeGlyphs(0, -1, font, enum<HarfBuzzSharp.SerializeFormat> (int format), enum<HarfBuzzSharp.SerializeFlag> (int flags))
                            compared <- compared + 1
                            match actual = expected with
                            | true -> ()
                            | false -> yield $"flags {flags} format {format:X}: oracle {expected} facade {actual}" ]
            Native.hb_font_destroy oracle
            printfn $"P10.S14: {compared} serialisations compared"
            Expect.isEmpty (List.truncate 5 differences) (String.concat "\n" (List.truncate 5 differences)))
        testCase "DeserializeGlyphs reads what the oracle serialised, in text and JSON, as the oracle reads it" (fun () ->
            OracleSession.ensure ()
            let path = namedFont ()
            let oracleFace = OracleFaces.face { Path = path; Suite = String.Empty; Index = 0 } 0u
            let oracle = Native.hb_font_create oracleFace
            Native.hb_ot_font_set_funcs oracle
            use font = new HarfBuzzSharp.Font(FacadeFonts.faceOf path 0)
            let cases = Gens.sample 60 (runs (int (Native.hb_face_get_glyph_count oracleFace)))
            let differences =
                [ for (infos, positions) in cases do
                    for format in [ TextTag; JsonTag ] do
                        let text = oracleText oracle infos positions format 0u
                        let bytes = Text.Encoding.UTF8.GetBytes text
                        let native = Native.hb_buffer_create ()
                        let mutable ending = 0n
                        Native.hb_buffer_deserialize_glyphs(native, bytes, bytes.Length, &ending, oracle, format) |> ignore
                        let mutable length = 0u
                        let expectedInfos = NativeText.copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(native, &length)) (int length)
                        let expectedPositions = NativeText.copyFrom<GlyphPosition> (Native.hb_buffer_get_glyph_positions(native, &length)) (int length)
                        Native.hb_buffer_destroy native
                        use buffer = new HarfBuzzSharp.Buffer()
                        buffer.DeserializeGlyphs(text, font, enum<HarfBuzzSharp.SerializeFormat> (int format))
                        let actualInfos = MemoryMarshal.Cast<HarfBuzzSharp.GlyphInfo, GlyphInfo>(ReadOnlySpan<HarfBuzzSharp.GlyphInfo> buffer.GlyphInfos).ToArray()
                        let actualPositions = MemoryMarshal.Cast<HarfBuzzSharp.GlyphPosition, GlyphPosition>(ReadOnlySpan<HarfBuzzSharp.GlyphPosition> buffer.GlyphPositions).ToArray()
                        match actualInfos = expectedInfos && actualPositions = expectedPositions && buffer.ContentType = HarfBuzzSharp.ContentType.Glyphs with
                        | true -> ()
                        | false -> yield $"{format:X}: {text}" ]
            Native.hb_font_destroy oracle
            Expect.isEmpty (List.truncate 5 differences) (String.concat "\n" (List.truncate 5 differences)))
        test "argument checks and messages of SerializeGlyphs and DeserializeGlyphs" {
            use buffer = new HarfBuzzSharp.Buffer()
            let empty = try buffer.SerializeGlyphs() |> ignore; None with :? InvalidOperationException as error -> Some error.Message
            Expect.equal empty (Some "Buffer should not be empty.") "empty buffer"
            buffer.AddUtf16 "ab"
            let content = try buffer.SerializeGlyphs() |> ignore; None with :? InvalidOperationException as error -> Some error.Message
            Expect.equal content (Some "ContentType should be of type Glyphs.") "unicode content"
            Expect.throwsT<InvalidOperationException> (fun () -> buffer.DeserializeGlyphs "[gid1=0]") "buffer must be empty"
            use fresh = new HarfBuzzSharp.Buffer()
            fresh.DeserializeGlyphs "[gid3=0+500|gid4=1+600]"
            Expect.equal (fresh.GlyphInfos |> Array.map (fun info -> info.Codepoint, info.Cluster)) [| 3u, 0u; 4u, 1u |] "text round trip"
            Expect.throwsT<InvalidOperationException> (fun () -> fresh.DeserializeGlyphs "[gid1=0]") "glyph content refused"
        }
        test "a range that starts after the first glyph begins with the separator" {
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.DeserializeGlyphs "[gid3=0+500|gid4=1+600|gid5=2+700]"
            Expect.equal (buffer.SerializeGlyphs(1, 3)) "|gid4=1+600|gid5=2+700]" "text range"
            Expect.equal (buffer.SerializeGlyphs(1, 2, null, HarfBuzzSharp.SerializeFormat.Json, HarfBuzzSharp.SerializeFlag.Default)).[0] ',' "JSON range"
        }
        test "NormalizeGlyphs moves each cluster's advance to one glyph and sorts the rest by glyph id" {
            use buffer = new HarfBuzzSharp.Buffer()
            buffer.DeserializeGlyphs "[gid9=0@0,0+10|gid5=0@0,0+20|gid7=1@0,0+30]"
            buffer.Direction <- HarfBuzzSharp.Direction.LeftToRight
            buffer.NormalizeGlyphs()
            Expect.equal (buffer.GlyphInfos |> Array.map (fun info -> info.Codepoint)) [| 9u; 5u; 7u |] "one glyph per cluster stays first, the rest sorted"
            Expect.equal (buffer.GlyphPositions |> Array.map (fun position -> position.XAdvance, position.XOffset)) [| 30, 0; 0, -30 + 10; 30, 0 |] "advance on the first glyph, offsets absorb the rest"
            let single = try (use fresh = new HarfBuzzSharp.Buffer() in fresh.NormalizeGlyphs(); None) with :? InvalidOperationException as error -> Some error.Message
            Expect.equal single (Some "ContentType must be of type Glyphs.") "content type"
        }
    ]
