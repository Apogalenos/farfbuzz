module Farfbuzz.Diff.GdefDiff

open System
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>The GDEF table tag.</summary>
let private gdefTag = Tag.make 'G' 'D' 'E' 'F'

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Builds a Farfbuzz GDEF for a face.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The GDEF.</returns>
let private gdefOf (font: TestFont) (index: uint32) : Gdef =
    Gdef.load (faceOf font index)

/// <summary>Every face that has a GDEF table.</summary>
/// <returns>The faces.</returns>
let private gdefFaces () =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) -> not (Face.referenceTable (faceOf font index) gdefTag).IsEmpty)

/// <summary>Every glyph id of every GDEF face, including the one past the end.</summary>
/// <returns>The face, index and glyph triples.</returns>
let private glyphInputs () =
    gdefFaces ()
    |> Seq.collect (fun struct (font, index) ->
        let glyphCount = (faceOf font index).GlyphCount
        seq { for glyph in 0 .. glyphCount -> struct (font, index, uint32 glyph) })

/// <summary>Reads a glyph's attach points through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The points.</returns>
let private oracleAttachPoints (face: nativeint) (glyph: uint32) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_layout_get_attach_points (face, glyph, 0u, &count, Array.empty)
    let points = Array.zeroCreate (int total)
    let mutable written = uint32 points.Length
    Native.hb_ot_layout_get_attach_points (face, glyph, 0u, &written, points) |> ignore
    points |> Array.take (int written) |> Array.toList

/// <summary>Reads a glyph's ligature carets through the oracle.</summary>
/// <param name="font">The font handle.</param>
/// <param name="direction">The direction.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The carets.</returns>
let private oracleLigatureCarets (font: nativeint) (direction: Direction) (glyph: uint32) : int list =
    let directionValue = int (Direction.value direction)
    let mutable count = 0u
    let total = Native.hb_ot_layout_get_ligature_carets (font, directionValue, glyph, 0u, &count, Array.empty)
    let carets = Array.zeroCreate (int total)
    let mutable written = uint32 carets.Length
    Native.hb_ot_layout_get_ligature_carets (font, directionValue, glyph, 0u, &written, carets) |> ignore
    carets |> Array.take (int written) |> Array.toList

/// <summary>Reads the glyphs of a class through the oracle as set ranges.</summary>
/// <param name="face">The face handle.</param>
/// <param name="klass">The class.</param>
/// <returns>The ranges, ascending.</returns>
let private oracleGlyphsInClass (face: nativeint) (klass: uint32) : struct (uint32 * uint32) array =
    let set = Native.hb_set_create ()
    Native.hb_ot_layout_get_glyphs_in_class (face, klass, set)
    let ranges = ResizeArray<struct (uint32 * uint32)>()
    let mutable first = 0u
    let mutable last = UInt32.MaxValue
    while Native.hb_set_next_range (set, &first, &last) <> 0 do
        ranges.Add (struct (first, last))
    Native.hb_set_destroy set
    ranges.ToArray()

/// <summary>Every set and glyph of every GDEF face's mark sets, plus fifty deterministic glyphs.</summary>
/// <returns>The face, index, set and glyph tuples.</returns>
let private markSetInputs () =
    gdefFaces ()
    |> Seq.collect (fun struct (font, index) ->
        let gdef = gdefOf font index
        seq {
            for set in 0 .. gdef.MarkGlyphSets.Length - 1 do
                for glyph in Coverage.glyphs gdef.MarkGlyphSets[set] do
                    yield struct (font, index, set, glyph)
                for k in 0 .. 49 do
                    yield struct (font, index, set, uint32 ((set * 7919 + k * 3571) &&& 0xFFFF))
        })

/// <summary>Reads a big-endian field of the GDEF header.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="baseOffset">The GDEF table offset.</param>
/// <param name="fieldOffset">The field offset.</param>
/// <param name="width">Two or four bytes.</param>
/// <returns>The field value.</returns>
let private readField (bytes: byte array) (baseOffset: int) (fieldOffset: int) (width: int) : uint32 =
    let at = baseOffset + fieldOffset
    match width with
    | 2 -> (uint32 bytes[at] <<< 8) ||| uint32 bytes[at + 1]
    | _ -> (uint32 bytes[at] <<< 24) ||| (uint32 bytes[at + 1] <<< 16) ||| (uint32 bytes[at + 2] <<< 8) ||| uint32 bytes[at + 3]

/// <summary>Writes a big-endian field of the GDEF header.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="baseOffset">The GDEF table offset.</param>
/// <param name="fieldOffset">The field offset.</param>
/// <param name="width">Two or four bytes.</param>
/// <param name="value">The value.</param>
let private writeField (bytes: byte array) (baseOffset: int) (fieldOffset: int) (width: int) (value: uint32) =
    let at = baseOffset + fieldOffset
    match width with
    | 2 ->
        bytes[at] <- byte (value >>> 8)
        bytes[at + 1] <- byte value
    | _ ->
        bytes[at] <- byte (value >>> 24)
        bytes[at + 1] <- byte (value >>> 16)
        bytes[at + 2] <- byte (value >>> 8)
        bytes[at + 3] <- byte value

/// <summary>Compares every GDEF query for mutated file bytes, returning the first mismatch.</summary>
/// <param name="bytes">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <returns>The mismatch description, or none.</returns>
let private compareMutated (bytes: byte array) (index: uint32) : string voption =
    let blob = Native.hb_blob_create (bytes, uint32 bytes.Length, 0, 0n, 0n)
    let oracleFace = Native.hb_face_create (blob, index)
    let oracleFont = Native.hb_font_create oracleFace
    Native.hb_ot_font_set_funcs oracleFont
    let upem = int (Native.hb_face_get_upem oracleFace)
    Native.hb_font_set_scale (oracleFont, upem, upem)
    let face = Face.create (ReadOnlyMemory bytes) index
    let gdef = Gdef.load face
    let otFace = OtFace.create face
    let farfbuzzFont = Font.create face
    let glyphCount = face.GlyphCount
    let classGlyphs = [| for glyph in 0 .. min glyphCount 40 -> uint32 glyph |]
    let carets = [| for glyph in 0 .. min glyphCount 8 -> uint32 glyph |]
    let classes = [| 0u; 1u; 2u; 3u; 4u |]
    let result =
        match Native.hb_ot_layout_has_glyph_classes oracleFace <> 0 = Gdef.hasGlyphClasses gdef with
        | false -> ValueSome "has glyph classes"
        | true ->
            match classGlyphs |> Array.tryFind (fun glyph -> Native.hb_ot_layout_get_glyph_class (oracleFace, glyph) <> int (Gdef.glyphClass gdef glyph)) with
            | Some glyph -> ValueSome $"glyph class of {glyph}"
            | None ->
                match classes |> Array.tryFind (fun klass -> oracleGlyphsInClass oracleFace klass <> (LayoutApi.getGlyphsInClass otFace klass |> Seq.map (fun r -> struct (r.First, r.Last)) |> Seq.toArray)) with
                | Some klass -> ValueSome $"glyphs in class {klass}"
                | None ->
                    match classGlyphs |> Array.tryFind (fun glyph -> int (Shims.farfbuzz_oracle_gdef_glyph_props (oracleFace, glyph)) <> int (Gdef.glyphProps gdef glyph)) with
                    | Some glyph -> ValueSome $"glyph props of {glyph}"
                    | None ->
                        match carets |> Array.tryFind (fun glyph -> oracleAttachPoints oracleFace glyph <> (LayoutApi.getAttachPoints otFace glyph |> List.ofSeq)) with
                        | Some glyph -> ValueSome $"attach points of {glyph}"
                        | None ->
                            match carets |> Array.tryFind (fun glyph -> oracleLigatureCarets oracleFont Direction.Ltr glyph <> (LayoutApi.getLigatureCarets farfbuzzFont Direction.Ltr glyph |> List.ofSeq)) with
                            | Some glyph -> ValueSome $"ligature carets of {glyph}"
                            | None ->
                                match gdef.MarkGlyphSets.Length with
                                | 0 -> ValueNone
                                | setCount ->
                                    let sets = [| 0 .. min setCount 4 - 1 |]
                                    match sets |> Array.tryFind (fun set ->
                                        let covered glyph = Gdef.markSetCovers gdef set glyph
                                        let oracleCovers glyph = Shims.farfbuzz_oracle_gdef_mark_set_covers (oracleFace, uint32 set, glyph) <> 0
                                        [| 0u; 1u; uint32 glyphCount |] |> Array.exists (fun glyph -> covered glyph <> oracleCovers glyph)) with
                                    | Some set -> ValueSome $"mark set covers of set {set}"
                                    | None -> ValueNone
    Native.hb_font_destroy oracleFont
    Native.hb_face_destroy oracleFace
    Native.hb_blob_destroy blob
    result

/// <summary>The P5.S02 GDEF differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P5.S02" [
        exhaustive "has glyph classes" (gdefFaces ())
            (fun struct (font, index) -> Gdef.hasGlyphClasses (gdefOf font index))
            (fun struct (font, index) -> Native.hb_ot_layout_has_glyph_classes (OracleFaces.face font index) <> 0)
        exhaustive "glyph class" (glyphInputs ())
            (fun struct (font, index, glyph) -> int (Gdef.glyphClass (gdefOf font index) glyph))
            (fun struct (font, index, glyph) -> Native.hb_ot_layout_get_glyph_class (OracleFaces.face font index, glyph))
        exhaustive "glyphs in class" (gdefFaces () |> Seq.collect (fun struct (font, index) -> seq { for klass in 0u .. 4u -> struct (font, index, klass) }))
            (fun struct (font, index, klass) -> LayoutApi.getGlyphsInClass (OtFace.create (faceOf font index)) klass |> Seq.map (fun r -> struct (r.First, r.Last)) |> Seq.toArray)
            (fun struct (font, index, klass) -> oracleGlyphsInClass (OracleFaces.face font index) klass)
        exhaustive "attach points" (glyphInputs ())
            (fun struct (font, index, glyph) -> LayoutApi.getAttachPoints (OtFace.create (faceOf font index)) glyph |> List.ofSeq)
            (fun struct (font, index, glyph) -> oracleAttachPoints (OracleFaces.face font index) glyph)
        exhaustive "ligature carets" (gdefFaces () |> Seq.collect (fun struct (font, index) ->
            let glyphCount = (faceOf font index).GlyphCount
            seq { for glyph in 0 .. glyphCount -> struct (font, index, uint32 glyph, Direction.Ltr)
                  for glyph in 0 .. glyphCount -> struct (font, index, uint32 glyph, Direction.Ttb) }))
            (fun struct (font, index, glyph, direction) -> LayoutApi.getLigatureCarets (Font.create (faceOf font index)) direction glyph |> List.ofSeq)
            (fun struct (font, index, glyph, direction) -> oracleLigatureCarets (OracleFaces.font font index) direction glyph)
        exhaustive "glyph props" (glyphInputs ())
            (fun struct (font, index, glyph) -> int (Gdef.glyphProps (gdefOf font index) glyph))
            (fun struct (font, index, glyph) -> int (Shims.farfbuzz_oracle_gdef_glyph_props (OracleFaces.face font index, glyph)))
        exhaustive "mark set covers" (markSetInputs ())
            (fun struct (font, index, set, glyph) -> Gdef.markSetCovers (gdefOf font index) set glyph)
            (fun struct (font, index, set, glyph) -> Shims.farfbuzz_oracle_gdef_mark_set_covers (OracleFaces.face font index, uint32 set, glyph) <> 0)
        testCase "P5.S02 mutated GDEF tables" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutations = 0
            for struct (font, index) in gdefFaces () do
                let face = faceOf font index
                match Face.tryFindTable face gdefTag with
                | ValueNone -> ()
                | ValueSome record ->
                    let bytes = OracleFaces.data font.Path
                    let baseOffset = int (face.BaseOffset + record.Offset)
                    let version = readField bytes baseOffset 0 4
                    let v12 = version >= 0x00010002u
                    let v14 = version >= 0x00010004u
                    let fields = [ 0, 4, true; 4, 2, true; 6, 2, true; 8, 2, true; 10, 2, true; 12, 2, v12; 18, 4, v14; 22, 4, v14; 26, 4, v14; 30, 4, v14; 34, 4, v14 ]
                    for fieldOffset, width, present in fields do
                        match present with
                        | false -> ()
                        | true ->
                            let value = readField bytes baseOffset fieldOffset width
                            let maxValue = if width = 2 then 0xFFFFu else 0xFFFFFFFFu
                            for mutated in [ 0u; 1u; value - 1u; value + 1u; maxValue ] do
                                let candidate = Array.copy bytes
                                writeField candidate baseOffset fieldOffset width mutated
                                match compareMutated candidate index with
                                | ValueNone -> ()
                                | ValueSome description -> failures.Add $"{font.Suite}/{System.IO.Path.GetFileName font.Path} face {index}: GDEF field {fieldOffset} (value {value} -> {mutated}): {description}"
                                mutations <- mutations + 1
                    match version >= 0x00010003u with
                    | false -> ()
                    | true ->
                        let varStoreOffset = readField bytes baseOffset 14 4
                        match varStoreOffset with
                        | 0u -> ()
                        | _ ->
                            let storeAt = baseOffset + int varStoreOffset
                            let storeFields = [ 0, 2, "store format"; 2, 4, "region-list offset"; 6, 2, "data-set count"; 8, 4, "data-set 0 offset" ]
                            for fieldOffset, width, name in storeFields do
                                let value = readField bytes storeAt fieldOffset width
                                let maxValue = if width = 2 then 0xFFFFu else 0xFFFFFFFFu
                                for mutated in [ 0u; 1u; value - 1u; value + 1u; maxValue ] do
                                    let candidate = Array.copy bytes
                                    writeField candidate storeAt fieldOffset width mutated
                                    match compareMutated candidate index with
                                    | ValueNone -> ()
                                    | ValueSome description -> failures.Add $"{font.Suite}/{System.IO.Path.GetFileName font.Path} face {index}: GDEF {name} (value {value} -> {mutated}): {description}"
                                    mutations <- mutations + 1
            printfn "P5.S02 compared %d mutated GDEF tables" mutations
            Expect.isEmpty failures ("mutated GDEF differences:\n" + String.concat "\n" failures))
    ]
