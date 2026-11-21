module Farfbuzz.Diff.CollectDiff

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Reads every range of a native set.</summary>
/// <param name="set">The set handle.</param>
/// <returns>The ranges.</returns>
let private rangesOfSet (set: nativeint) : struct (uint32 * uint32) array =
    let ranges = ResizeArray<struct (uint32 * uint32)>()
    let mutable first = 0u
    let mutable last = UInt32.MaxValue
    while Native.hb_set_next_range(set, &first, &last) <> 0 do
        ranges.Add(struct (first, last))
    ranges.ToArray()

/// <summary>Reads a native set's members as one range array.</summary>
/// <param name="collect">The collector that fills the set.</param>
/// <returns>The ranges.</returns>
let private oracleRanges (collect: nativeint -> unit) =
    let set = Native.hb_set_create ()
    collect set
    let ranges = rangesOfSet set
    Native.hb_set_destroy set
    ranges

/// <summary>Reads a face's nominal mapping through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The sorted key and value pairs.</returns>
let private oracleMapping (face: nativeint) : struct (uint32 * uint32) array =
    let map = Native.hb_map_create ()
    let unicodes = Native.hb_set_create ()
    Native.hb_face_collect_nominal_glyph_mapping(face, map, unicodes)
    let pairs = ResizeArray<struct (uint32 * uint32)>()
    let mutable index = -1
    let mutable key = 0u
    let mutable value = 0u
    while Native.hb_map_next(map, &index, &key, &value) <> 0 do
        pairs.Add(struct (key, value))
    Native.hb_map_destroy map
    Native.hb_set_destroy unicodes
    pairs.ToArray() |> Array.sortBy (fun struct (key, _) -> key)

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Returns a face's selectors through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <returns>The selector code points.</returns>
let private oracleSelectors (face: nativeint) =
    oracleRanges (fun set -> Native.hb_face_collect_variation_selectors(face, set))
    |> Array.map (fun struct (first, _) -> first)

/// <summary>Returns the code points of one selector through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="selector">The selector.</param>
/// <returns>The code points.</returns>
let private oracleVariationUnicodes (face: nativeint) (selector: uint32) =
    oracleRanges (fun set -> Native.hb_face_collect_variation_unicodes(face, selector, set))
    |> Array.collect (fun struct (first, last) -> [| first .. last |])

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Builds a bare sfnt.</summary>
/// <param name="tables">The table bytes.</param>
/// <returns>The file bytes.</returns>
let private sfnt (tables: (Tag * byte array) list) : byte array =
    let headerSize = 12 + 16 * tables.Length
    let header = Array.concat [ be32 0x00010000u; be16 (uint32 tables.Length); be16 0u; be16 0u; be16 0u ]
    let records, _ =
        tables
        |> List.fold (fun (records, offset) (tag, bytes) ->
            Array.concat [ be32 (uint32 tag); be32 0u; be32 (uint32 offset); be32 (uint32 bytes.Length) ] :: records,
            offset + bytes.Length) ([], headerSize)
    Array.concat (header :: List.rev records @ (tables |> List.map snd))

/// <summary>Builds a cmap with one format 12 or 13 subtable.</summary>
/// <param name="format">12 or 13.</param>
/// <param name="groups">Start, end and glyph per group.</param>
/// <returns>The cmap bytes.</returns>
let private longCmap (format: uint16) (groups: (uint32 * uint32 * uint32) list) : byte array =
    let body =
        groups
        |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ])
        |> Array.concat
    let subtable =
        Array.concat [ be16 (uint32 format); be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]
    let cmap =
        Array.concat [ be16 0u; be16 1u; be16 0u; be16 4u; be32 12u; subtable ]
    let maxp = Array.concat [ be32 0x00005000u; be16 32u ]
    sfnt [ Tags.Maxp, maxp; Tags.Cmap, cmap ]

/// <summary>Runs an action with an oracle face made from bytes.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="action">The action.</param>
/// <returns>The action result.</returns>
let private withOracleFace (bytes: byte array) (action: nativeint -> 'T) : 'T =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let result = action face
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    result

/// <summary>The P2.S07 collect and variation differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let selectorInputs =
        faces
        |> Seq.collect (fun struct (font, index) ->
            let face = OracleFaces.face font index
            oracleSelectors face
            |> Seq.map (fun selector -> struct (font, index, selector)))
    let p2s07 =
        testList "P2.S07" [
        exhaustive "P2.S07 collect unicodes" faces
            (fun struct (font, index) ->
                let face = faceOf font index
                Cmap.collectUnicodes (OtFace.create face).Cmap.Value face.GlyphCount
                |> Seq.map (fun range -> struct (range.First, range.Last)) |> Seq.toArray)
            (fun struct (font, index) ->
                oracleRanges (fun set -> Native.hb_face_collect_unicodes(OracleFaces.face font index, set)))
        exhaustive "P2.S07 collect mapping" faces
            (fun struct (font, index) ->
                let face = faceOf font index
                Cmap.collectMapping (OtFace.create face).Cmap.Value face.GlyphCount |> Seq.toArray)
            (fun struct (font, index) -> oracleMapping (OracleFaces.face font index))
        exhaustive "P2.S07 variation selectors" faces
            (fun struct (font, index) ->
                Cmap.collectVariationSelectors (OtFace.create (faceOf font index)).Cmap.Value
                |> Seq.map (fun range -> struct (range.First, range.Last)) |> Seq.toArray)
            (fun struct (font, index) ->
                oracleRanges (fun set -> Native.hb_face_collect_variation_selectors(OracleFaces.face font index, set)))
        exhaustive "P2.S07 variation unicodes" selectorInputs
            (fun struct (font, index, selector) ->
                Cmap.collectVariationUnicodes (OtFace.create (faceOf font index)).Cmap.Value selector
                |> Seq.map (fun range -> struct (range.First, range.Last)) |> Seq.toArray)
            (fun struct (font, index, selector) ->
                oracleRanges (fun set -> Native.hb_face_collect_variation_unicodes(OracleFaces.face font index, selector, set)))
        exhaustive "P2.S07 variation glyphs"
            (faces
             |> Seq.collect (fun struct (font, index) ->
                 let oracleFace = OracleFaces.face font index
                 let selectors = oracleSelectors oracleFace
                 let hasFe0F = selectors |> Array.contains 0xFE0Fu
                 selectors
                 |> Seq.collect (fun selector ->
                     let codepoints = oracleVariationUnicodes oracleFace selector
                     seq {
                         for codepoint in codepoints do
                             yield struct (font, index, codepoint, selector)
                         match hasFe0F with
                         | true -> ()
                         | false when selector <> 0xFE0Fu ->
                             for codepoint in codepoints do
                                 yield struct (font, index, codepoint, 0xFE0Fu)
                         | false -> ()
                     })))
            (fun struct (font, index, codepoint, selector) ->
                Font.variationGlyph (Font.create (faceOf font index)) codepoint selector
                |> function | ValueSome glyph -> struct (true, glyph) | ValueNone -> struct (false, 0u))
            (fun struct (font, index, codepoint, selector) ->
                let mutable glyph = 0u
                let found = Native.hb_font_get_variation_glyph(OracleFaces.font font index, codepoint, selector, &glyph)
                struct (found <> 0, glyph))
    ]
    let crafted =
        [ "ordering", longCmap 12us [ 0x100u, 0x1FFu, 5u; 0x150u, 0x150u, 3u ]
          "clipping", longCmap 12us [ 0x10FFF0u, 0x110010u, 1u ]
          "format13-zero", longCmap 13us [ 0x41u, 0x5Au, 0u ] ]
    let p2r04 =
        testList "P2.R04" [
            exhaustive "P2.R04 crafted collection" crafted
                (fun (_, bytes) ->
                    let face = Face.create (ReadOnlyMemory bytes) 0u
                    let cmap = Cmap.load face (Os2.load face)
                    struct (Cmap.collectUnicodes cmap face.GlyphCount |> Seq.map (fun range -> struct (range.First, range.Last)) |> Seq.toArray,
                            Cmap.collectMapping cmap face.GlyphCount |> Seq.toArray))
                (fun (_, bytes) ->
                    withOracleFace bytes (fun face ->
                        struct (oracleRanges (fun set -> Native.hb_face_collect_unicodes(face, set)),
                                oracleMapping face)))
        ]
    let lookupInputs =
        faces
        |> Seq.collect (fun struct (font, index) ->
            let oracleFace = OracleFaces.face font index
            seq {
                for table in [ Tags.Gsub; Tags.Gpos ] do
                    let count = int (Native.hb_ot_layout_table_get_lookup_count(oracleFace, uint32 table))
                    for lookup in 0 .. count - 1 do
                        yield struct (font, index, table, lookup)
            })
    let glyphRanges (ranges: ImmutableArray<CodepointRange>) : struct (uint32 * uint32) array =
        ranges |> Seq.map (fun (range: CodepointRange) -> struct (range.First, range.Last)) |> Seq.toArray
    let oursLookupCollect (struct (font, index, table, lookup)) =
        let struct (before, input, after, output) = LayoutApi.lookupCollectGlyphs (OtFace.create (faceOf font index)) table lookup
        struct (glyphRanges before, glyphRanges input, glyphRanges after, glyphRanges output)
    let oracleLookupCollect (struct (font, index, table, lookup)) =
        let face = OracleFaces.face font index
        let before = Native.hb_set_create ()
        let input = Native.hb_set_create ()
        let after = Native.hb_set_create ()
        let output = Native.hb_set_create ()
        Native.hb_ot_layout_lookup_collect_glyphs(face, uint32 table, uint32 lookup, before, input, after, output)
        let result = struct (rangesOfSet before, rangesOfSet input, rangesOfSet after, rangesOfSet output)
        Native.hb_set_destroy before
        Native.hb_set_destroy input
        Native.hb_set_destroy after
        Native.hb_set_destroy output
        result
    let p5s15 =
        testList "P5.S15" [
            exhaustive "P5.S15 lookup collect glyphs" lookupInputs oursLookupCollect oracleLookupCollect
        ]
    testList "Collect" [ p2s07; p2r04; p5s15 ]
