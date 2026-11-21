module Farfbuzz.Diff.MutationDiff

open System
open System.IO
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>The exact fields compared for one mutated face.</summary>
type private FaceCheck =
    { Upem: int
      Glyphs: int
      Tags: uint32 array
      TableHash: uint64
      Nominal: uint32 array
      Variations: uint32 array
      HAdvances: int array
      VAdvances: int array
      HExtents: struct (bool * int * int * int)
      VExtents: struct (bool * int * int * int)
      CollectHash: uint64
      MappingHash: uint64
      Names: byte array array
      FromNames: struct (bool * uint32) array }

/// <summary>The mutation-wide result: the face count and up to four face checks.</summary>
type private FileCheck =
    { FaceCount: int
      Faces: FaceCheck array }

/// <summary>The FNV-1a 64 offset basis.</summary>
[<Literal>]
let private FnvOffset = 14695981039346656037UL

/// <summary>The FNV-1a 64 prime.</summary>
[<Literal>]
let private FnvPrime = 1099511628211UL

/// <summary>Mixes bytes into a hash.</summary>
/// <param name="hash">The hash so far.</param>
/// <param name="bytes">The bytes.</param>
/// <returns>The new hash.</returns>
let private hashBytes (hash: uint64) (bytes: ReadOnlySpan<byte>) =
    let mutable current = hash
    for byte in bytes do
        current <- (current ^^^ uint64 byte) * FnvPrime
    current

/// <summary>Mixes a range list into a hash.</summary>
/// <param name="ranges">The ranges.</param>
/// <returns>The hash.</returns>
let private hashRanges (ranges: struct (uint32 * uint32) array) =
    ranges
    |> Array.fold (fun hash (struct (first, last)) -> (hash ^^^ uint64 first) * FnvPrime |> fun value -> (value ^^^ uint64 last) * FnvPrime) FnvOffset

/// <summary>Mixes a sorted pair list into a hash.</summary>
/// <param name="pairs">The pairs.</param>
/// <returns>The hash.</returns>
let private hashPairs (pairs: struct (uint32 * uint32) array) =
    pairs
    |> Array.fold (fun hash (struct (key, value)) -> (hash ^^^ uint64 key) * FnvPrime |> fun result -> (result ^^^ uint64 value) * FnvPrime) FnvOffset

/// <summary>Reads a table hash through a face's directory.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tags">The tags.</param>
/// <returns>The hash.</returns>
let private oracleTableHash (face: nativeint) (tags: uint32 array) =
    tags
    |> Array.fold (fun hash tag ->
        let blob = Native.hb_face_reference_table(face, tag)
        let mutable length = 0u
        let pointer = Native.hb_blob_get_data(blob, &length)
        let bytes: byte array = Array.zeroCreate (int length)
        match int length with
        | 0 -> ()
        | count -> Runtime.InteropServices.Marshal.Copy(pointer, bytes, 0, count)
        let result = hashBytes hash (ReadOnlySpan bytes)
        Native.hb_blob_destroy blob
        result) FnvOffset

/// <summary>Reads an oracle face's table tags and table hash.</summary>
/// <param name="face">The face.</param>
/// <returns>The tags and hash.</returns>
let private oracleTables (face: nativeint) =
    let tags = OracleFaces.tableTags face
    struct (tags, oracleTableHash face tags)

/// <summary>Reads a face's nominal glyphs for the fixed sample.</summary>
/// <param name="font">The font handle.</param>
/// <param name="sample">The code points.</param>
/// <returns>The glyphs, with 0xFFFFFFFF for misses.</returns>
let private oracleNominal (font: nativeint) (sample: uint32 array) =
    let glyphs = Array.zeroCreate sample.Length
    Shims.farfbuzz_oracle_nominal_glyph_range(font, 0u, 0u, glyphs) |> ignore
    sample |> Array.map (fun codepoint ->
        let one = Array.zeroCreate 1
        Shims.farfbuzz_oracle_nominal_glyph_range(font, codepoint, 1u, one) |> ignore
        one[0])

/// <summary>Reads a face's variation glyphs for the fixed sample.</summary>
/// <param name="font">The font handle.</param>
/// <param name="sample">The code point and selector pairs.</param>
/// <returns>The glyphs, with 0xFFFFFFFF for misses.</returns>
let private oracleVariations (font: nativeint) (sample: struct (uint32 * uint32) array) =
    sample |> Array.map (fun (struct (codepoint, selector)) ->
        let mutable glyph = 0u
        match Native.hb_font_get_variation_glyph(font, codepoint, selector, &glyph) with
        | 0 -> 0xFFFFFFFFu
        | _ -> glyph)

/// <summary>Reads a face's advances in one batch.</summary>
/// <param name="font">The font handle.</param>
/// <param name="glyphs">The glyphs.</param>
/// <param name="horizontal">Whether to read horizontal advances.</param>
/// <returns>The advances.</returns>
let private oracleAdvances (font: nativeint) (glyphs: uint32 array) (horizontal: bool) =
    let advances = Array.zeroCreate glyphs.Length
    match horizontal with
    | true -> Native.hb_font_get_glyph_h_advances(font, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    | false -> Native.hb_font_get_glyph_v_advances(font, uint32 glyphs.Length, glyphs, 4u, advances, 4u)
    advances

/// <summary>Reads a name from the oracle as raw bytes.</summary>
/// <param name="font">The font handle.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The name bytes.</returns>
let private oracleName (font: nativeint) (glyph: uint32) =
    let buffer = Array.zeroCreate 128
    Native.hb_font_glyph_to_string(font, glyph, buffer, 128u)
    let length = match Array.IndexOf(buffer, 0uy) with | -1 -> buffer.Length | value -> value
    buffer[0 .. length - 1]

/// <summary>Reads the extents from the oracle.</summary>
/// <param name="font">The font handle.</param>
/// <param name="horizontal">Whether to read horizontal extents.</param>
/// <returns>Whether they exist and their values.</returns>
let private oracleExtents (font: nativeint) (horizontal: bool) =
    let mutable extents = Unchecked.defaultof<Native.OracleFontExtents>
    let present =
        match horizontal with
        | true -> Native.hb_font_get_h_extents(font, &extents)
        | false -> Native.hb_font_get_v_extents(font, &extents)
    struct (present <> 0, extents.Ascender, extents.Descender, extents.LineGap)

/// <summary>Reads the oracle's collect-unicode ranges.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The ranges.</returns>
let private oracleUnicodes (face: nativeint) =
    let set = Native.hb_set_create ()
    Native.hb_face_collect_unicodes(face, set)
    let result = ResizeArray<struct (uint32 * uint32)>()
    let mutable first = 0u
    let mutable last = UInt32.MaxValue
    while Native.hb_set_next_range(set, &first, &last) <> 0 do
        result.Add(struct (first, last))
    Native.hb_set_destroy set
    result.ToArray()

/// <summary>Reads the oracle's nominal mapping pairs.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The sorted pairs.</returns>
let private oracleMapping (face: nativeint) =
    let map = Native.hb_map_create ()
    let unicodes = Native.hb_set_create ()
    Native.hb_face_collect_nominal_glyph_mapping(face, map, unicodes)
    let result = ResizeArray<struct (uint32 * uint32)>()
    let mutable index = -1
    let mutable key = 0u
    let mutable value = 0u
    while Native.hb_map_next(map, &index, &key, &value) <> 0 do
        result.Add(struct (key, value))
    Native.hb_map_destroy map
    Native.hb_set_destroy unicodes
    result.ToArray() |> Array.sortBy (fun struct (key, _) -> key)

/// <summary>Compares one face through Farfbuzz and the oracle.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="oracleFace">The oracle face.</param>
/// <param name="index">The face index.</param>
/// <returns>The combined checks.</returns>
let private checkFace (bytes: byte array) (oracleFace: nativeint) (index: uint32) =
    let face = Face.create (ReadOnlyMemory bytes) index
    let oracleFont = Native.hb_font_create oracleFace
    Native.hb_ot_font_set_funcs oracleFont
    let upem = int (Native.hb_face_get_upem oracleFace)
    Native.hb_font_set_scale(oracleFont, upem, upem)
    let font = Font.create face
    let glyphCount = int (Native.hb_face_get_glyph_count oracleFace)
    let glyphs = Array.init (min 300 (glyphCount + 1)) uint32
    let sample = [| 0u; 0x41u; 0x61u; 0x80u; 0x100u; 0x3FFu; 0x4E00u; 0x10FFFFu; 0x110000u |]
    let variations = [| struct (0x41u, 0xFE0Fu); struct (0x2026u, 0xFE0Fu); struct (0x1F600u, 0xE0100u) |]
    let names = Array.init (min 300 glyphCount) (fun glyph -> oracleName oracleFont (uint32 glyph))
    let fromNames =
        names
        |> Array.map (fun name ->
            let mutable glyph = 0u
            let found = Native.hb_font_glyph_from_string(oracleFont, name, name.Length, &glyph)
            struct (found <> 0, glyph))
    let tags = OracleFaces.tableTags oracleFace
    let tableHash = oracleTableHash oracleFace tags
    let oracleUnicodeHash = oracleUnicodes oracleFace |> hashRanges
    let oracleMappingHash = oracleMapping oracleFace |> hashPairs
    let oracleH = oracleAdvances oracleFont glyphs true
    let oracleV = oracleAdvances oracleFont glyphs false
    let oracleHExtents = oracleExtents oracleFont true
    let oracleVExtents = oracleExtents oracleFont false
    let result =
        { Upem = upem
          Glyphs = glyphCount
          Tags = tags
          TableHash = tableHash
          Nominal = oracleNominal oracleFont sample
          Variations = oracleVariations oracleFont variations
          HAdvances = oracleH
          VAdvances = oracleV
          HExtents = oracleHExtents
          VExtents = oracleVExtents
          CollectHash = oracleUnicodeHash
          MappingHash = oracleMappingHash
          Names = names
          FromNames = fromNames }
    let actual =
        let cmap = (OtFace.create face).Cmap.Value
        let tags = Face.tableTags face |> Seq.map uint32 |> Seq.toArray
        let tableHash = tags |> Array.fold (fun hash tag -> hashBytes hash (Face.referenceTable face (LanguagePrimitives.UInt32WithMeasure tag)).Span) FnvOffset
        let unicodeHash = Cmap.collectUnicodes cmap glyphCount |> Seq.map (fun range -> struct (range.First, range.Last)) |> Seq.toArray |> hashRanges
        let mappingHash = Cmap.collectMapping cmap glyphCount |> Seq.toArray |> hashPairs
        let h = Font.hAdvances font (ReadOnlySpan glyphs) |> Seq.toArray
        let v = Font.vAdvances font (ReadOnlySpan glyphs) |> Seq.toArray
        let hExtents =
            match Font.hExtents font with
            | ValueSome extents -> struct (true, extents.Ascender, extents.Descender, extents.LineGap)
            | ValueNone -> struct (false, 0, 0, 0)
        let vExtents =
            match Font.vExtents font with
            | ValueSome extents -> struct (true, extents.Ascender, extents.Descender, extents.LineGap)
            | ValueNone -> struct (false, 0, 0, 0)
        let names =
            names
            |> Array.map (fun name ->
                match Font.glyphFromString font (Text.Encoding.Latin1.GetString name) with
                | ValueSome glyph -> struct (true, glyph)
                | ValueNone -> struct (false, 0u))
        let nameBytes = Array.init (min 300 face.GlyphCount) (fun glyph -> Text.Encoding.Latin1.GetBytes (Font.glyphToString font (uint32 glyph)))
        { Upem = face.Upem
          Glyphs = face.GlyphCount
          Tags = tags
          TableHash = tableHash
          Nominal = sample |> Array.map (fun codepoint -> match Font.nominalGlyph font codepoint with | ValueSome glyph -> glyph | ValueNone -> 0xFFFFFFFFu)
          Variations = variations |> Array.map (fun (struct (codepoint, selector)) -> match Font.variationGlyph font codepoint selector with | ValueSome glyph -> glyph | ValueNone -> 0xFFFFFFFFu)
          HAdvances = h
          VAdvances = v
          HExtents = hExtents
          VExtents = vExtents
          CollectHash = unicodeHash
          MappingHash = mappingHash
          Names = nameBytes
          FromNames = names }
    Native.hb_font_destroy oracleFont
    struct (result, actual)

/// <summary>Names the first differing field of two face checks.</summary>
/// <param name="expected">The oracle check.</param>
/// <param name="actual">The Farfbuzz check.</param>
/// <returns>A short description.</returns>
let private difference (expected: FaceCheck) (actual: FaceCheck) =
    match expected.Upem <> actual.Upem with
    | true -> "upem"
    | false ->
        match expected.Glyphs <> actual.Glyphs with
        | true -> "glyph count"
        | false ->
            match expected.Tags <> actual.Tags with
            | true -> "table tags"
            | false ->
                match expected.TableHash <> actual.TableHash with
                | true -> "table bytes"
                | false ->
                    match expected.Nominal <> actual.Nominal with
                    | true -> "nominal glyphs"
                    | false ->
                        match expected.Variations <> actual.Variations with
                        | true -> "variation glyphs"
                        | false ->
                            match expected.HAdvances <> actual.HAdvances with
                            | true -> "h advances"
                            | false ->
                                match expected.VAdvances <> actual.VAdvances with
                                | true -> "v advances"
                                | false ->
                                    match expected.HExtents <> actual.HExtents with
                                    | true -> "h extents"
                                    | false ->
                                        match expected.VExtents <> actual.VExtents with
                                        | true -> "v extents"
                                        | false ->
                                            match expected.CollectHash <> actual.CollectHash with
                                            | true -> "collect unicodes"
                                            | false ->
                                                match expected.MappingHash <> actual.MappingHash with
                                                | true -> "collect mapping"
                                                | false ->
                                                    match expected.Names <> actual.Names with
                                                    | true -> "glyph names"
                                                    | false ->
                                                        match expected.FromNames <> actual.FromNames with
                                                        | true -> "glyph from name"
                                                        | false -> "unknown"

/// <summary>Reads a file's faces from both implementations.</summary>
/// <param name="bytes">The mutated bytes.</param>
/// <returns>The face count and the checks for up to four faces.</returns>
let private checkFile (bytes: byte array) =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let oracleCount = int (Native.hb_face_count blob)
    let oursCount = Face.count (ReadOnlyMemory bytes)
    let count = min 4 (min oracleCount oursCount)
    let faces =
        Array.init count (fun index ->
            let oracleFace = Native.hb_face_create(blob, uint32 index)
            let struct (expected, actual) = checkFace bytes oracleFace (uint32 index)
            Native.hb_face_destroy oracleFace
            struct (expected, actual))
    Native.hb_blob_destroy blob
    struct (oracleCount, oursCount, faces)

/// <summary>The P2.R08 mutation differential over every corpus file.</summary>
/// <returns>The test list.</returns>
let tests () =
    let files = TestFonts.files ()
    testList "P2.R08" [
        testCase "P2.R08 mutated fonts" (fun () ->
            let failures = ResizeArray<string>()
            let mutable blobs = 0
            let mutable faces = 0
            let outputDirectory = Path.Combine(RepositoryPaths.root (), "artifacts", "mutation")
            Directory.CreateDirectory outputDirectory |> ignore
            for suite, path in files do
                for k in 0 .. 7 do
                    let mutation = Mutate.mutate path k
                    let struct (oracleCount, oursCount, checks) = checkFile mutation.Bytes
                    blobs <- blobs + 1
                    faces <- faces + checks.Length
                    let mismatch = oracleCount <> oursCount || checks |> Array.exists (fun (struct (expected, actual)) -> expected <> actual)
                    match mismatch with
                    | false -> ()
                    | true ->
                        let saved = Path.Combine(outputDirectory, $"{Path.GetFileName path}-{k}.bin")
                        File.WriteAllBytes(saved, mutation.Bytes)
                        let field =
                            checks
                            |> Array.tryPick (fun (struct (expected, actual)) ->
                                match expected = actual with
                                | true -> None
                                | false -> Some (difference expected actual))
                            |> Option.defaultValue "face count"
                        failures.Add($"{suite}/{Path.GetFileName path} k={k}: {mutation.Description}; {field}; oracle faces={oracleCount}, farfbuzz faces={oursCount}; saved {saved}")
            printfn "P2.R08 compared %d mutated blobs and %d faces" blobs faces
            Expect.isEmpty failures ("mutated-font differences:\n" + String.concat "\n" failures))
    ]
