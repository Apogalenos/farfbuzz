module Farfbuzz.Diff.ExtentsDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.NoLayoutFonts
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.ShapeGens
open Farfbuzz.Diff.TestFonts

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Whether a face reads extents from the non-variable glyf header only.</summary>
/// <param name="face">The face.</param>
/// <returns>True for a glyf face without bitmap, colour, CFF or CFF2 outlines.</returns>
let private isSupportedGlyf (face: Face) =
    Face.tryFindTable face Tags.Glyf |> ValueOption.isSome
    && Face.tryFindTable face Tags.Sbix |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cbdt |> ValueOption.isNone
    && Face.tryFindTable face Tags.Colr |> ValueOption.isNone
    && Face.tryFindTable face Tags.Varc |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cff2 |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cff |> ValueOption.isNone

/// <summary>Every glyph of every supported glyf face, including one out-of-range probe.</summary>
/// <returns>The test font, face index and glyph triples.</returns>
let private glyphInputs () : struct (TestFont * uint32 * uint32) list =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) -> isSupportedGlyf (faceOf font index))
    |> Seq.collect (fun struct (font, index) ->
        let count = (faceOf font index).GlyphCount
        [ for glyph in 0u .. uint32 count -> struct (font, index, glyph) ])
    |> Seq.toList

/// <summary>Farfbuzz's extents as a comparable tuple; the sentinel marks a not-ported source, which cannot occur here.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private oursExtents (font: Font) (glyph: uint32) : struct (bool * int * int * int * int) =
    match Extents.glyph font glyph with
    | Ok ValueNone -> struct (false, 0, 0, 0, 0)
    | Ok (ValueSome extents) -> struct (true, extents.XBearing, extents.YBearing, extents.Width, extents.Height)
    | Error _ -> struct (false, -1, -1, -1, -1)

/// <summary>The oracle's extents as a comparable tuple.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private oracleExtents (font: nativeint) (glyph: uint32) : struct (bool * int * int * int * int) =
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(font, glyph, &extents)
    struct (present <> 0, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>The stripped fonts whose vertical origin source this phase reads: VORG or the extents fallback.</summary>
/// <returns>The fonts.</returns>
let private verticalFonts () : ImmutableArray<TestFont> =
    NoLayoutFonts.all ()
    |> Seq.filter (fun font ->
        let face = faceOf font (uint32 font.Index)
        match Origins.vOrigin (Font.create face) 0u with
        | Ok _ -> true
        | Error _ -> false)
    |> ImmutableArray.CreateRange

/// <summary>What the vertical comparison looks at: glyph ids, clusters and positions.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparable (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>A vertical request over a supported stripped font, direction TTB or BTT.</summary>
/// <param name="fonts">The stripped faces.</param>
/// <returns>The request.</returns>
let private verticalRequest (fonts: ImmutableArray<TestFont>) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements (fonts |> Seq.toArray)
        let! codepoints = text Vertical
        let! direction = Gen.elements [| ValueSome "ttb"; ValueSome "btt" |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = direction }
    }

/// <summary>
/// The fixed BTT case that once failed on TestAATMorx.ttf: the AAT morx text, direction BTT, ot shaper.
/// The stripped face is used because phase 4 applies no layout, so HarfBuzz's morx table must not be present.
/// </summary>
/// <returns>The request.</returns>
let private fixedVerticalSpec () =
    let font =
        NoLayoutFonts.all ()
        |> Seq.find (fun font -> font.Path.Contains "TestAATMorx.ttf" && font.Index = 0)
    { defaults font.Path
          [| 0x17C69u; 0x866Du; 0x54EBu; 0x1EF5Fu; 0x7E99u; 0x19B56u; 0x1D0D7u; 0x1891Du; 0xA498u; 0x18AE4u; 0x9505u; 0x2189Au; 0x29C92u; 0x42u; 0x25005u |] with
        FaceIndex = font.Index
        Shaper = ValueSome "ot"
        FontFuncs = ValueSome "ot"
        Direction = ValueSome "btt" }

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian 24-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be24 (value: uint32) = [| byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Builds a bare sfnt from a table directory and the tables in order.</summary>
/// <param name="version">The sfnt version.</param>
/// <param name="tables">The tables.</param>
/// <returns>The bytes.</returns>
let private sfnt (version: uint32) (tables: (Tag * byte array) list) : byte array =
    let headerSize = 12 + 16 * tables.Length
    let header = Array.concat [ be32 version; be16 (uint32 tables.Length); be16 0u; be16 0u; be16 0u ]
    let records, _ =
        tables
        |> List.fold (fun (records, offset) (tag, bytes) ->
            let record = Array.concat [ be32 (uint32 tag); be32 0u; be32 (uint32 offset); be32 (uint32 bytes.Length) ]
            record :: records, offset + bytes.Length) ([], headerSize)
    Array.concat (header :: List.rev records @ (tables |> List.map snd))

/// <summary>A valid 54-byte head table.</summary>
/// <returns>The bytes.</returns>
let private head () : byte array =
    Array.concat
        [ be32 0x00010000u; be32 0x00010000u; be32 0u; be32 0x5F0F3CF5u
          be16 0u; be16 1000u; be32 0u; be32 0u; be32 0u; be32 0u
          be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u ]

/// <summary>A version 0.5 maxp table with eight glyphs.</summary>
/// <returns>The bytes.</returns>
let private maxp () : byte array = Array.concat [ be32 0x00005000u; be16 8u ]

/// <summary>A version-1 VORG table with an explicit count, so the count can exceed the records present.</summary>
/// <param name="defaultY">The default origin.</param>
/// <param name="count">The count to write.</param>
/// <param name="records">The glyph and origin pairs.</param>
/// <returns>The bytes.</returns>
let private vorgVersion1 (defaultY: int16) (count: uint32) (records: (uint32 * int16) list) : byte array =
    let recordBytes =
        records
        |> List.toArray
        |> Array.collect (fun (glyph, y) -> Array.concat [ be16 glyph; be16 (uint32 (int y &&& 0xFFFF)) ])
    Array.concat [ be16 1u; be16 0u; be16 (uint32 (int defaultY &&& 0xFFFF)); be16 count; recordBytes ]

/// <summary>A version-2 VORG table with 24-bit glyph ids.</summary>
/// <param name="defaultY">The default origin.</param>
/// <param name="records">The glyph and origin pairs.</param>
/// <returns>The bytes.</returns>
let private vorgVersion2 (defaultY: int16) (records: (uint32 * int16) list) : byte array =
    let recordBytes =
        records
        |> List.toArray
        |> Array.collect (fun (glyph, y) -> Array.concat [ be24 glyph; be16 (uint32 (int y &&& 0xFFFF)) ])
    Array.concat [ be16 2u; be16 0u; be16 (uint32 (int defaultY &&& 0xFFFF)); be24 (uint32 records.Length); recordBytes ]

/// <summary>The vertical origin of one glyph through a face built from bytes, read through the oracle.</summary>
/// <param name="bytes">The sfnt bytes.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The origin, and whether it exists.</returns>
let private oracleVOrigin (bytes: byte array) (glyph: uint32) : struct (bool * int * int) =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let font = Native.hb_font_create face
    Native.hb_ot_font_set_funcs font
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(font, upem, upem)
    let mutable x = 0
    let mutable y = 0
    let present = Native.hb_font_get_glyph_v_origin(font, glyph, &x, &y)
    Native.hb_font_destroy font
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    struct (present <> 0, x, y)

/// <summary>The vertical origin of one glyph through a Farfbuzz face built from bytes.</summary>
/// <param name="bytes">The sfnt bytes.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The origin, and whether it exists.</returns>
let private oursVOrigin (bytes: byte array) (glyph: uint32) : struct (bool * int * int) =
    let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
    match Origins.vOrigin font glyph with
    | Ok (struct (x, y)) -> struct (true, x, y)
    | Error _ -> struct (false, 0, 0)

/// <summary>The changed-VORG differential: version, default, count and an over-large count, through every glyph.</summary>
let private vorgExtents () =
    testCase "P4.S07 changed VORG bytes" (fun () ->
        let mutations =
            [ "version 1, default 880", vorgVersion1 880s 2u [ 2u, 600s; 5u, 700s ]
              "version 1, default 500", vorgVersion1 500s 2u [ 2u, 600s; 5u, 700s ]
              "version 1, count 1", vorgVersion1 880s 1u [ 2u, 600s ]
              "version 1, over-large count", vorgVersion1 880s 100u [ 2u, 600s ]
              "version 2, default 880", vorgVersion2 880s [ 2u, 600s; 5u, 700s ]
              "version 0", Array.concat [ be16 0u; be16 0u; be16 880u; be16 0u ]
              "version 3", Array.concat [ be16 3u; be16 0u; be16 880u; be16 0u ] ]
        let glyphs = [ 0u .. 7u ]
        let mismatches =
            mutations
            |> List.collect (fun (name, vorg) ->
                let bytes = sfnt 0x00010000u [ Tags.Head, head (); Tags.Maxp, maxp (); Tags.Vorg, vorg ]
                glyphs
                |> List.choose (fun glyph ->
                    let ours = oursVOrigin bytes glyph
                    let expected = oracleVOrigin bytes glyph
                    match ours = expected with
                    | true -> None
                    | false -> Some $"  {name} glyph {glyph}: oracle %A{expected}, farfbuzz %A{ours}"))
        Expect.isEmpty mismatches ("first mismatches:\n" + String.concat "\n" mismatches))

/// <summary>Reads the oracle extents of a glyph on a fresh font with synthetic state applied.</summary>
/// <param name="testFont">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="embolden">The synthetic bold strength, or none.</param>
/// <param name="slant">The synthetic slant.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private oracleSyntheticExtents (testFont: TestFont) (index: uint32) (embolden: struct (float32 * float32) voption) (slant: float32) (glyph: uint32) : struct (bool * int * int * int * int) =
    let face = OracleFaces.face testFont index
    let font = Native.hb_font_create face
    Native.hb_ot_font_set_funcs font
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(font, upem, upem)
    match embolden with
    | ValueSome (x, y) -> Native.hb_font_set_synthetic_bold(font, x, y, 0)
    | ValueNone -> ()
    match slant <> 0f with
    | true -> Native.hb_font_set_synthetic_slant(font, slant)
    | false -> ()
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(font, glyph, &extents)
    Native.hb_font_destroy font
    struct (present <> 0, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>Whether two extents answers agree: a Farfbuzz not-ported answer agrees with a present oracle answer, and
/// no-extents agrees with an absent oracle answer.</summary>
/// <param name="ours">Farfbuzz's answer.</param>
/// <param name="expected">The oracle's answer.</param>
/// <returns>True when they agree.</returns>
let private syntheticAgrees (ours: Result<GlyphExtents voption, ShapeError>) (expected: struct (bool * int * int * int * int)) =
    match ours, expected with
    | Ok ValueNone, struct (false, _, _, _, _) -> true
    | Error _, struct (true, _, _, _, _) -> true
    | _ -> false

/// <summary>The synthetic extents differential: emboldened and slanted fonts, through every glyph.</summary>
let private syntheticExtents () =
    testCase "P4.S07 emboldened and slanted glyph extents" (fun () ->
        let faces =
            OracleFaces.all ()
            |> Seq.filter (fun struct (font, index) -> isSupportedGlyf (faceOf font index))
            |> Seq.truncate 3
            |> Seq.toList
        let states = [ "embolden", ValueSome (struct (0.05f, 0.05f)), 0f; "slant", ValueNone, 0.2f ]
        let mismatches =
            faces
            |> List.collect (fun struct (testFont, index) ->
                let face = faceOf testFont index
                states
                |> List.collect (fun (name, embolden, slant) ->
                    let farfbuzzFont =
                        let font = Font.create face
                        match embolden with
                        | ValueSome (x, y) -> Font.withSyntheticBold x y false font
                        | ValueNone -> Font.withSyntheticSlant slant font
                    [ for glyph in 0u .. uint32 face.GlyphCount ->
                        let ours = Extents.glyph farfbuzzFont glyph
                        let expected = oracleSyntheticExtents testFont index embolden slant glyph
                        match syntheticAgrees ours expected with
                        | true -> None
                        | false -> Some $"  {name} glyph {glyph}: oracle %A{expected}, farfbuzz %A{ours}" ]
                    |> List.choose id))
        Expect.isEmpty mismatches ("first mismatches:\n" + String.concat "\n" mismatches))

/// <summary>Farfbuzz and HarfBuzz 15.1.0 agree on glyf extents and on no-layout vertical shaping.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    testList "P4.S07" [
        exhaustive "P4.S07 glyf extents" (glyphInputs ())
            (fun struct (font, index, glyph) -> oursExtents (Font.create (faceOf font index)) glyph)
            (fun struct (font, index, glyph) -> oracleExtents (OracleFaces.font font index) glyph)
        sampledWithOutlines "P4.S07 no-layout vertical shaping"
            (verticalRequest (verticalFonts ()))
            3000
            (fun spec -> comparable (farfbuzz.Shape spec))
            (fun spec -> comparable (oracle.Shape spec))
            outlinesNotPorted
        testCase "P4.S07 no-layout vertical shaping, fixed morx case" (fun () ->
            Expect.equal (comparable (farfbuzz.Shape (fixedVerticalSpec ()))) (comparable (oracle.Shape (fixedVerticalSpec ()))) "agreement")
        vorgExtents ()
        syntheticExtents ()
    ]
