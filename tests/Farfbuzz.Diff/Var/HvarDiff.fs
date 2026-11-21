module Farfbuzz.Diff.HvarDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.Gens

/// <summary>The <c>gvar</c> table tag, whose presence would change the no-HVAR fallback.</summary>
let private gvarTag = Tag.make 'g' 'v' 'a' 'r'

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Whether a face carries a gvar table, which HarfBuzz falls back to when HVAR/VVAR has no data.</summary>
/// <param name="face">The face.</param>
/// <returns>True when a non-empty gvar table is present.</returns>
let private hasGvar (face: Face) : bool =
    (Face.referenceTable face gvarTag).Length > 0

/// <summary>Sets scale and normalized coordinates on an oracle font, then reads one horizontal advance.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="scale">The horizontal and vertical scale.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The horizontal advance.</returns>
let private oracleHAdvance (font: TestFont) (index: uint32) (coords: int array) (scale: int) (glyph: uint32) : int =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_scale(handle, scale, scale)
    Native.hb_font_set_var_coords_normalized(handle, coords, uint32 coords.Length)
    let glyphs = [| glyph |]
    let advances = [| 0 |]
    Native.hb_font_get_glyph_h_advances(handle, 1u, glyphs, 4u, advances, 4u)
    Native.hb_font_destroy handle
    advances[0]

/// <summary>Sets normalized coordinates on an oracle font, then reads one vertical advance.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The vertical advance.</returns>
let private oracleVAdvance (font: TestFont) (index: uint32) (coords: int array) (glyph: uint32) : int =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_var_coords_normalized(handle, coords, uint32 coords.Length)
    let glyphs = [| glyph |]
    let advances = [| 0 |]
    Native.hb_font_get_glyph_v_advances(handle, 1u, glyphs, 4u, advances, 4u)
    Native.hb_font_destroy handle
    advances[0]

/// <summary>Sets normalized coordinates on an oracle font, then reads one vertical origin.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The origin, or none.</returns>
let private oracleVOrigin (font: TestFont) (index: uint32) (coords: int array) (glyph: uint32) : struct (int * int) voption =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_var_coords_normalized(handle, coords, uint32 coords.Length)
    let mutable x = 0
    let mutable y = 0
    let result =
        match Native.hb_font_get_glyph_v_origin(handle, glyph, &x, &y) with
        | 0 -> ValueNone
        | _ -> ValueSome struct (x, y)
    Native.hb_font_destroy handle
    result

/// <summary>Builds the Farfbuzz font for a face at a scale with normalized coordinates.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="scale">The horizontal and vertical scale.</param>
/// <returns>The font.</returns>
let private ourFontAt (font: TestFont) (index: uint32) (coords: ImmutableArray<int>) (scale: int) : Font =
    Font.setVarCoordsNormalized coords (Font.withScale scale scale (Font.create (faceOf font index)))

/// <summary>The horizontal advance of a Farfbuzz font.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The advance.</returns>
let private ourHAdvance (font: Font) (glyph: uint32) : int =
    Font.hAdvance font glyph

/// <summary>The vertical advance of a Farfbuzz font.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The advance.</returns>
let private ourVAdvance (font: Font) (glyph: uint32) : int =
    Font.vAdvance font glyph

/// <summary>The vertical origin of a Farfbuzz font.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The origin, or none.</returns>
let private ourVOrigin (font: Font) (glyph: uint32) : struct (int * int) voption =
    match Origins.vOrigin font glyph with
    | Ok struct (x, y) -> ValueSome struct (x, y)
    | Error _ -> ValueNone

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Reads a big-endian uint32 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU32 (bytes: byte array) (at: int) =
    (uint32 bytes[at] <<< 24) ||| (uint32 bytes[at + 1] <<< 16) ||| (uint32 bytes[at + 2] <<< 8) ||| uint32 bytes[at + 3]

/// <summary>Encodes a uint16 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint16) : byte array =
    [| byte (uint32 value >>> 8); byte value |]

/// <summary>Encodes a uint32 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) : byte array =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>The replacement uint16 values for one mutated count field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u16Replacements (value: uint16) : uint16 array =
    [| 0us; 1us; uint16 (int value - 1); uint16 (int value + 1); 0xFFFFus |]

/// <summary>The replacement uint32 values for one mutated offset field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u32Replacements (value: uint32) : uint32 array =
    [| 0u; 1u; value - 1u; value + 1u; 0xFFFFFFFFu |]

/// <summary>One field mutation: the absolute byte offset, a label and the replacement bytes.</summary>
type private Mutation =
    { At: int
      Name: string
      Replacement: byte array }

/// <summary>The map header offsets of an HVAR/VVAR table, with their labels.</summary>
let private mapFields : (int * string) array =
    [| 8, "advMap offset"; 12, "lsbMap offset"; 16, "rsbMap offset" |]

/// <summary>The vertical-origin map header offset of a VVAR table.</summary>
[<Literal>]
let private VorgMapHeaderOffset = 20

/// <summary>Mutates the format and entry count of one map, when its offset is non-zero.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="tableAbsAt">The absolute file offset of the HVAR/VVAR table.</param>
/// <param name="fieldOffset">The header offset of the map's offset field.</param>
/// <param name="name">The map's name.</param>
/// <returns>The mutations.</returns>
let private mapMutations (bytes: byte array) (tableAbsAt: int) (fieldOffset: int) (name: string) : Mutation list =
    let mapOffset = int (readU32 bytes (tableAbsAt + fieldOffset))
    match mapOffset with
    | 0 -> []
    | _ ->
        let mapAbsAt = tableAbsAt + mapOffset
        let format = bytes[mapAbsAt]
        let mutations = ResizeArray<Mutation>()
        for replacement in [| 0uy; 1uy; 2uy; 0xFFuy |] do
            mutations.Add { At = mapAbsAt; Name = $"{name} format"; Replacement = [| replacement |] }
        match format with
        | 0uy ->
            for replacement in u16Replacements (readU16 bytes (mapAbsAt + 2)) do
                mutations.Add { At = mapAbsAt + 2; Name = $"{name} entry count"; Replacement = be16 replacement }
        | 1uy ->
            for replacement in u32Replacements (readU32 bytes (mapAbsAt + 2)) do
                mutations.Add { At = mapAbsAt + 2; Name = $"{name} entry count"; Replacement = be32 replacement }
        | _ -> ()
        mutations |> Seq.toList

/// <summary>Mutates one header field or map of an HVAR or VVAR table, one field rewritten at a time.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="tableAbsAt">The absolute file offset of the HVAR/VVAR table.</param>
/// <param name="withVorg">True for VVAR.</param>
/// <returns>The mutations.</returns>
let private headerMutations (bytes: byte array) (tableAbsAt: int) (withVorg: bool) : Mutation list =
    let mutations = ResizeArray<Mutation>()
    for replacement in u16Replacements (readU16 bytes tableAbsAt) do
        mutations.Add { At = tableAbsAt; Name = "version"; Replacement = be16 replacement }
    for replacement in u32Replacements (readU32 bytes (tableAbsAt + 4)) do
        mutations.Add { At = tableAbsAt + 4; Name = "varStore offset"; Replacement = be32 replacement }
    for fieldOffset, name in mapFields do
        for replacement in u32Replacements (readU32 bytes (tableAbsAt + fieldOffset)) do
            mutations.Add { At = tableAbsAt + fieldOffset; Name = name; Replacement = be32 replacement }
        mutations.AddRange(mapMutations bytes tableAbsAt fieldOffset name)
    match withVorg with
    | false -> ()
    | true ->
        for replacement in u32Replacements (readU32 bytes (tableAbsAt + VorgMapHeaderOffset)) do
            mutations.Add { At = tableAbsAt + VorgMapHeaderOffset; Name = "vorgMap offset"; Replacement = be32 replacement }
        mutations.AddRange(mapMutations bytes tableAbsAt VorgMapHeaderOffset "vorgMap offset")
    mutations |> Seq.toList

/// <summary>Mutates the directory length of a table, truncating its tail.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The mutations.</returns>
let private truncationMutations (face: Face) (tag: Tag) : Mutation list =
    match Face.tryFindTable face tag with
    | ValueNone -> []
    | ValueSome record ->
        let index = face.Directory |> Seq.findIndex (fun r -> r.Tag = tag)
        let lengthFieldAt = int face.BaseOffset + 12 + index * 16 + 12
        let original = record.Length
        [| 0u; 20u; 21u; 22u; 23u; original / 2u |]
        |> Array.filter (fun length -> length < original)
        |> Array.distinct
        |> Array.map (fun length -> { At = lengthFieldAt; Name = "truncated tail"; Replacement = be32 length })
        |> Array.toList

/// <summary>The normalized coordinate probes used by the mutated-table differentials.</summary>
/// <param name="axisCount">The axis count.</param>
/// <returns>The coordinate arrays.</returns>
let private coordProbes (axisCount: int) : int array array =
    [| Array.create axisCount 0
       Array.create axisCount 0x4000
       Array.create axisCount -0x4000
       Array.create axisCount 0x2000 |]

/// <summary>A small deterministic sample of glyph ids across the glyph range.</summary>
/// <param name="glyphCount">The face glyph count.</param>
/// <returns>The sampled glyph ids.</returns>
let private sampleGlyphs (glyphCount: int) : uint32 array =
    [| 0u; 1u; 2u; 3u; uint32 (glyphCount - 1) |]
    |> Array.filter (fun glyph -> int glyph < glyphCount)
    |> Array.distinct

/// <summary>Creates the oracle font for mutated bytes, with the normalized coordinates applied.</summary>
/// <param name="mutated">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The oracle font handle.</returns>
let private mutatedOracleFont (mutated: byte array) (index: uint32) (coords: int array) : nativeint =
    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, index)
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(handle, upem, upem)
    Native.hb_font_set_var_coords_normalized(handle, coords, uint32 coords.Length)
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    handle

/// <summary>Reads one horizontal advance through the oracle for mutated bytes.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The advance.</returns>
let private mutatedOracleHAdvance (handle: nativeint) (glyph: uint32) : int =
    let glyphs = [| glyph |]
    let advances = [| 0 |]
    Native.hb_font_get_glyph_h_advances(handle, 1u, glyphs, 4u, advances, 4u)
    advances[0]

/// <summary>Reads one vertical advance through the oracle for mutated bytes.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The advance.</returns>
let private mutatedOracleVAdvance (handle: nativeint) (glyph: uint32) : int =
    let glyphs = [| glyph |]
    let advances = [| 0 |]
    Native.hb_font_get_glyph_v_advances(handle, 1u, glyphs, 4u, advances, 4u)
    advances[0]

/// <summary>Reads one vertical origin through the oracle for mutated bytes.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The origin, or none.</returns>
let private mutatedOracleVOrigin (handle: nativeint) (glyph: uint32) : struct (int * int) voption =
    let mutable x = 0
    let mutable y = 0
    match Native.hb_font_get_glyph_v_origin(handle, glyph, &x, &y) with
    | 0 -> ValueNone
    | _ -> ValueSome struct (x, y)

/// <summary>The P7.S06 HVAR/VVAR advance and origin differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let hvarFaces =
        faces
        |> Seq.filter (fun struct (font, index) -> (Hvar.loadHvar (faceOf font index)).HasData)
        |> Seq.toArray
    let vvarFaces =
        faces
        |> Seq.filter (fun struct (font, index) -> (Hvar.loadVvar (faceOf font index)).HasData)
        |> Seq.toArray
    let vorgVvarFaces =
        vvarFaces
        |> Array.filter (fun struct (font, index) -> (Vorg.load (faceOf font index)).Present)
    let hvarNoGvarFaces =
        hvarFaces
        |> Array.filter (fun struct (font, index) -> not (hasGvar (faceOf font index)))
    let vvarNoGvarFaces =
        vvarFaces
        |> Array.filter (fun struct (font, index) -> not (hasGvar (faceOf font index)))
    let hAdvanceInputs =
        gen {
            let! struct (font, index) = Gen.elements hvarFaces
            let face = faceOf font index
            let upem = face.Upem
            let glyphCount = face.GlyphCount
            let! glyph = Gen.choose (0, glyphCount - 1) |> Gen.map uint32
            let! coords = coordsFor font index
            let! scale = Gen.elements [| upem; 3 * upem + 7; -upem |]
            return struct (font, index, glyph, coords, scale)
        }
    let vAdvanceInputs =
        gen {
            let! struct (font, index) = Gen.elements vvarFaces
            let glyphCount = (faceOf font index).GlyphCount
            let! glyph = Gen.choose (0, glyphCount - 1) |> Gen.map uint32
            let! coords = coordsFor font index
            return struct (font, index, glyph, coords)
        }
    let vOriginInputs =
        gen {
            let! struct (font, index) = Gen.elements vorgVvarFaces
            let glyphCount = (faceOf font index).GlyphCount
            let! glyph = Gen.choose (0, glyphCount - 1) |> Gen.map uint32
            let! coords = coordsFor font index
            return struct (font, index, glyph, coords)
        }
    testList "P7.S06" [
        sampled "P7.S06 h advances" hAdvanceInputs 20000
            (fun struct (font, index, glyph, coords, scale) ->
                ourHAdvance (ourFontAt font index coords scale) glyph)
            (fun struct (font, index, glyph, coords, scale) ->
                oracleHAdvance font index (coords |> Seq.toArray) scale glyph)
        sampled "P7.S06 v advances" vAdvanceInputs 3000
            (fun struct (font, index, glyph, coords) ->
                ourVAdvance (ourFontAt font index coords ((faceOf font index).Upem)) glyph)
            (fun struct (font, index, glyph, coords) ->
                oracleVAdvance font index (coords |> Seq.toArray) glyph)
        sampled "P7.S06 v origins" vOriginInputs 3000
            (fun struct (font, index, glyph, coords) ->
                ourVOrigin (ourFontAt font index coords ((faceOf font index).Upem)) glyph)
            (fun struct (font, index, glyph, coords) ->
                oracleVOrigin font index (coords |> Seq.toArray) glyph)
        testCase "P7.S06 mutated HVAR" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutatedFaces = 0
            let mutable mutatedTables = 0
            for struct (font, index) in hvarNoGvarFaces do
                let bytes = OracleFaces.data font.Path
                let face = Face.create (ReadOnlyMemory bytes) index
                let axisCount = (Fvar.load face).Axes.Length
                let glyphs = sampleGlyphs face.GlyphCount
                let probes = coordProbes axisCount
                match Face.tryFindTable face Tags.Hvar with
                | ValueNone -> ()
                | ValueSome record ->
                    let tableAbsAt = int (face.BaseOffset + record.Offset)
                    let mutations = headerMutations bytes tableAbsAt false @ truncationMutations face Tags.Hvar
                    mutatedFaces <- mutatedFaces + 1
                    for mutation in mutations do
                        mutatedTables <- mutatedTables + 1
                        let mutated = Array.copy bytes
                        for b in 0 .. mutation.Replacement.Length - 1 do
                            mutated[mutation.At + b] <- mutation.Replacement[b]
                        let oursFace = Face.create (ReadOnlyMemory mutated) index
                        for coords in probes do
                            let oracleFont = mutatedOracleFont mutated index coords
                            let oursFont = Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) (Font.create oursFace)
                            for glyph in glyphs do
                                let ours = Font.hAdvance oursFont glyph
                                let oracle = mutatedOracleHAdvance oracleFont glyph
                                match ours = oracle with
                                | true -> ()
                                | false -> failures.Add($"{font.Path}#{index} HVAR {mutation.Name} glyph {glyph} coords %A{coords}: oracle %A{oracle}, farfbuzz %A{ours}")
                            Native.hb_font_destroy oracleFont
            printfn "P7.S06 compared %d mutated HVAR faces and %d mutated tables" mutatedFaces mutatedTables
            Expect.isEmpty failures ("mutated-HVAR differences:\n" + String.concat "\n" failures))
        testCase "P7.S06 mutated VVAR" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutatedFaces = 0
            let mutable mutatedTables = 0
            for struct (font, index) in vvarNoGvarFaces do
                let bytes = OracleFaces.data font.Path
                let face = Face.create (ReadOnlyMemory bytes) index
                let axisCount = (Fvar.load face).Axes.Length
                let glyphs = sampleGlyphs face.GlyphCount
                let probes = coordProbes axisCount
                match Face.tryFindTable face Tags.Vvar with
                | ValueNone -> ()
                | ValueSome record ->
                    let tableAbsAt = int (face.BaseOffset + record.Offset)
                    let mutations = headerMutations bytes tableAbsAt true @ truncationMutations face Tags.Vvar
                    mutatedFaces <- mutatedFaces + 1
                    for mutation in mutations do
                        mutatedTables <- mutatedTables + 1
                        let mutated = Array.copy bytes
                        for b in 0 .. mutation.Replacement.Length - 1 do
                            mutated[mutation.At + b] <- mutation.Replacement[b]
                        let oursFace = Face.create (ReadOnlyMemory mutated) index
                        for coords in probes do
                            let oracleFont = mutatedOracleFont mutated index coords
                            let oursFont = Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) (Font.create oursFace)
                            for glyph in glyphs do
                                let oursV = Font.vAdvance oursFont glyph
                                let oracleV = mutatedOracleVAdvance oracleFont glyph
                                let oursOrigin = ourVOrigin oursFont glyph
                                let oracleOrigin = mutatedOracleVOrigin oracleFont glyph
                                match oursV = oracleV && oursOrigin = oracleOrigin with
                                | true -> ()
                                | false -> failures.Add($"{font.Path}#{index} VVAR {mutation.Name} glyph {glyph} coords %A{coords}: oracle v=%A{oracleV} origin=%A{oracleOrigin}, farfbuzz v=%A{oursV} origin=%A{oursOrigin}")
                            Native.hb_font_destroy oracleFont
            printfn "P7.S06 compared %d mutated VVAR faces and %d mutated tables" mutatedFaces mutatedTables
            Expect.isEmpty failures ("mutated-VVAR differences:\n" + String.concat "\n" failures))
    ]
