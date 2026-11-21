module Farfbuzz.Diff.GlyphOutlineDiff

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
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.Gens

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Creates a Farfbuzz glyf accelerator from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The accelerator.</returns>
let private glyfFaceOf (font: TestFont) (index: uint32) =
    GlyfFace.load (faceOf font index)

/// <summary>Packs a point's flag and end-point marker the way the oracle shim does.</summary>
/// <param name="point">The point.</param>
/// <returns>The packed flag byte.</returns>
let private packedFlag (point: ContourPoint) =
    match point.IsEndPoint with
    | true -> point.Flag ||| 0x80uy
    | false -> point.Flag

/// <summary>The number of phantom points.</summary>
[<Literal>]
let private PhantomCount = 4

/// <summary>Converts assembled points into the oracle's comparable form, phantoms last.</summary>
/// <param name="points">The points.</param>
/// <param name="phantomOnly">Whether only the last four points are compared.</param>
/// <returns>The comparable tuples.</returns>
let private comparable (points: ContourPoint array) (phantomOnly: bool) : struct (uint32 * uint32 * byte) array =
    let selected = if phantomOnly then points[points.Length - PhantomCount .. points.Length - 1] else points
    selected
    |> Array.map (fun point -> struct (BitConverter.SingleToUInt32Bits point.X, BitConverter.SingleToUInt32Bits point.Y, packedFlag point))

/// <summary>Farfbuzz's assembled points for one glyph, compared as bits.</summary>
/// <param name="glyf">The accelerator.</param>
/// <param name="gvar">The gvar table.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="phantomOnly">Whether only the phantoms are wanted.</param>
/// <returns>The points, or none where the glyph fails.</returns>
let private oursPoints (glyf: GlyfFace) (gvar: Gvar) (coords: NormalizedCoords) (gid: uint32) (phantomOnly: bool) : struct (uint32 * uint32 * byte) array voption =
    match GlyphOutline.points glyf gvar coords gid phantomOnly with
    | ValueNone -> ValueNone
    | ValueSome points -> ValueSome (comparable (points |> Seq.toArray) phantomOnly)

/// <summary>The oracle's assembled points for one glyph, compared as bits.</summary>
/// <param name="font">The oracle font with coordinates set.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="phantomOnly">Whether only the phantoms are wanted.</param>
/// <returns>The points, or none where HarfBuzz fails.</returns>
let private oraclePoints (font: nativeint) (gid: uint32) (phantomOnly: bool) : struct (uint32 * uint32 * byte) array voption =
    let phantom = if phantomOnly then 1u else 0u
    let count = Shims.farfbuzz_oracle_glyf_points(font, gid, phantom, Array.empty<float32>, Array.empty<byte>, 0u)
    match count with
    | 0xFFFFFFFFu -> ValueNone
    | count ->
        let xy = Array.zeroCreate<float32> (int count * 2)
        let flags = Array.zeroCreate<byte> (int count)
        Shims.farfbuzz_oracle_glyf_points(font, gid, phantom, xy, flags, count) |> ignore
        let all = Array.init (int count) (fun i -> struct (BitConverter.SingleToUInt32Bits xy[2 * i], BitConverter.SingleToUInt32Bits xy[2 * i + 1], flags[i]))
        ValueSome (if phantomOnly then all[all.Length - PhantomCount .. all.Length - 1] else all)

/// <summary>Creates an oracle font with the given coordinates, calls the oracle, and releases it.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="phantomOnly">Whether only the phantoms are wanted.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The points.</returns>
let private oracleWithCoords (font: TestFont) (index: uint32) (gid: uint32) (phantomOnly: bool) (coords: ImmutableArray<int>) : struct (uint32 * uint32 * byte) array voption =
    let face = OracleFaces.face font index
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(handle, upem, upem)
    Native.hb_font_set_var_coords_normalized(handle, (coords |> Seq.toArray), uint32 coords.Length)
    let result = oraclePoints handle gid phantomOnly
    Native.hb_font_destroy handle
    result

/// <summary>Every glyph of every gvar face, including composite glyphs.</summary>
/// <returns>The test font, face index and glyph triples.</returns>
let private gvarGlyphInputs () : struct (TestFont * uint32 * uint32) array =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let face = faceOf font index
        let gvar = Gvar.load face
        match gvar.GlyphCount with
        | 0 -> [||]
        | _ -> [| for gid in 0u .. uint32 (face.GlyphCount - 1) -> struct (font, index, gid) |])
    |> Seq.toArray

/// <summary>A random glyph of a random gvar face, a phantom mode and a coordinate draw.</summary>
let private sampledInput : Gen<struct (TestFont * uint32 * uint32 * bool * ImmutableArray<int>)> =
    let glyphs = gvarGlyphInputs ()
    gen {
        let! i = Gen.choose (0, glyphs.Length - 1)
        let struct (font, index, gid) = glyphs[i]
        let! phantomOnly = Gen.choose (0, 1) |> Gen.map (fun n -> n = 1)
        let! coords = Gens.coordsFor font index
        return struct (font, index, gid, phantomOnly, coords)
    }

/// <summary>The gvar faces without an HVAR table, where HarfBuzz falls back to gvar phantoms.</summary>
/// <returns>The test font and index pairs.</returns>
let private gvarNoHvarFaces () : struct (TestFont * uint32) array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = faceOf font index
        (Gvar.load face).GlyphCount <> 0 && not (Hvar.loadHvar face).HasData)
    |> Seq.toArray

/// <summary>Sets normalized coordinates on an oracle font, then reads one horizontal advance.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The horizontal advance.</returns>
let private oracleHAdvance (font: TestFont) (index: uint32) (coords: int array) (glyph: uint32) : int =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
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

/// <summary>Builds a Farfbuzz font at a scale with normalized coordinates.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="scale">The horizontal and vertical scale.</param>
/// <returns>The font.</returns>
let private ourFontAt (font: TestFont) (index: uint32) (coords: ImmutableArray<int>) (scale: int) : Font =
    Font.setVarCoordsNormalized coords (Font.withScale scale scale (Font.create (faceOf font index)))

/// <summary>The vertical-origin-capable faces named by the handoff.</summary>
/// <returns>The test font and index pairs.</returns>
let private verticalMetricFaces () : struct (TestFont * uint32) array =
    let wanted =
        [ "NotoSansCJK-VF.abc.otf"
          "NotoSerifHK-subset.ttf"
          "TestGVAROne.ttf"
          "TestGVARTwo.ttf"
          "TestGVARThree.ttf" ]
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, _) ->
        wanted |> List.exists (fun name -> font.Path.EndsWith(name, StringComparison.Ordinal)))
    |> Seq.toArray

/// <summary>Sets normalized coordinates on an oracle font, then reads one glyph's extents.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The extents.</returns>
let private oracleExtents (font: TestFont) (index: uint32) (coords: int array) (glyph: uint32) : struct (bool * int * int * int * int) =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_var_coords_normalized(handle, coords, uint32 coords.Length)
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(handle, glyph, &extents)
    Native.hb_font_destroy handle
    struct (present <> 0, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>Farfbuzz's glyph extents as a comparable tuple.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The extents.</returns>
let private oursExtents (font: Font) (glyph: uint32) : struct (bool * int * int * int * int) =
    match Extents.glyph font glyph with
    | Ok ValueNone -> struct (false, 0, 0, 0, 0)
    | Ok (ValueSome extents) -> struct (true, extents.XBearing, extents.YBearing, extents.Width, extents.Height)
    | Error _ -> struct (false, -1, -1, -1, -1)

/// <summary>Farfbuzz's vertical origin as an optional pair.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The origin, or none.</returns>
let private ourVOrigin (font: Font) (glyph: uint32) : struct (int * int) voption =
    match Origins.vOrigin font glyph with
    | Ok struct (x, y) -> ValueSome struct (x, y)
    | Error _ -> ValueNone

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

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

/// <summary>A 54-byte head table with long offsets.</summary>
/// <param name="upem">Units per em.</param>
/// <returns>The bytes.</returns>
let private head (upem: int) : byte array =
    Array.concat
        [ be32 0x00010000u; be32 0x00010000u; be32 0u; be32 0x5F0F3CF5u
          be16 0u; be16 (uint32 upem); be32 0u; be32 0u; be32 0u; be32 0u
          be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 1u; be16 0u ]

/// <summary>A maxp table with a small glyph count.</summary>
/// <param name="glyphCount">The glyph count.</param>
/// <returns>The bytes.</returns>
let private maxp (glyphCount: int) : byte array =
    Array.concat [ be32 0x00005000u; be16 (uint32 glyphCount) ]

/// <summary>A long loca table with more entries than the maxp glyph count.</summary>
/// <param name="offsets">The byte offsets.</param>
/// <returns>The bytes.</returns>
let private longLoca (offsets: int array) : byte array =
    offsets |> Array.collect (fun offset -> be32 (uint32 offset))

/// <summary>Builds a face whose loca table names more glyphs than maxp does.</summary>
/// <returns>The face bytes.</returns>
let private overLongLocaFace () : byte array =
    let glyf = Array.concat [ Array.zeroCreate 10; Array.zeroCreate 10; Array.zeroCreate 10; Array.zeroCreate 10; Array.zeroCreate 10; Array.zeroCreate 10 ]
    sfnt 0x00010000u
        [ Tags.Head, head 1000
          Tags.Maxp, maxp 2
          Tags.Loca, longLoca [| 0; 10; 20; 30; 40; 50; 60 |]
          Tags.Glyf, glyf ]

/// <summary>Compares Farfbuzz and the oracle on one glyph-id-range glyph.</summary>
/// <param name="bytes">The face bytes.</param>
/// <param name="gid">The glyph id.</param>
/// <returns>A mismatch description, or none.</returns>
let private compareGlyphIdRange (bytes: byte array) (gid: uint32) : string option =
    let face = Face.create (ReadOnlyMemory bytes) 0u
    let glyf = GlyfFace.load face
    let gvar = Gvar.load face
    let ours = oursPoints glyf gvar NormalizedCoords.empty gid false
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let faceHandle = Native.hb_face_create(blob, 0u)
    let font = Native.hb_font_create faceHandle
    Native.hb_ot_font_set_funcs font
    let upem = int (Native.hb_face_get_upem faceHandle)
    Native.hb_font_set_scale(font, upem, upem)
    let oracle = oraclePoints font gid false
    Native.hb_font_destroy font
    Native.hb_face_destroy faceHandle
    Native.hb_blob_destroy blob
    match ours = oracle with
    | true -> None
    | false -> Some $"gid {gid}: oracle %A{oracle}, farfbuzz %A{ours}"

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Writes a big-endian uint16 into bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="value">The value.</param>
let private writeU16 (bytes: byte array) (at: int) (value: uint16) =
    bytes[at] <- byte (uint32 value >>> 8)
    bytes[at + 1] <- byte value

/// <summary>The absolute byte range of one table in a face's file.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The offset and length.</returns>
let private tableRange (face: Face) (tag: Tag) : struct (int * int) voption =
    match Face.tryFindTable face tag with
    | ValueNone -> ValueNone
    | ValueSome record -> ValueSome (struct (int (face.BaseOffset + record.Offset), int record.Length))

/// <summary>Compares Farfbuzz and the oracle on one mutated composite glyph.</summary>
/// <param name="mutated">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="phantomOnly">Whether only phantoms are wanted.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>A mismatch description, or none.</returns>
let private compareMutated (mutated: byte array) (index: uint32) (gid: uint32) (phantomOnly: bool) (coords: ImmutableArray<int>) : string option =
    let ours =
        let face = Face.create (ReadOnlyMemory mutated) index
        let glyf = GlyfFace.load face
        let gvar = Gvar.load face
        oursPoints glyf gvar (NormalizedCoords.ofSealed coords) gid phantomOnly
    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, index)
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(handle, upem, upem)
    Native.hb_font_set_var_coords_normalized(handle, (coords |> Seq.toArray), uint32 coords.Length)
    let oracle = oraclePoints handle gid phantomOnly
    Native.hb_font_destroy handle
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    match ours = oracle with
    | true -> None
    | false -> Some $"oracle %A{oracle}, farfbuzz %A{ours}"

/// <summary>The P7.S10 glyph-outline differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P7.S10" [
        sampled "P7.S10 points varied" sampledInput 20000
            (fun struct (font, index, gid, phantomOnly, coords) ->
                oursPoints (glyfFaceOf font index) (Gvar.load (faceOf font index)) (NormalizedCoords.ofSealed coords) gid phantomOnly)
            (fun struct (font, index, gid, phantomOnly, coords) ->
                oracleWithCoords font index gid phantomOnly coords)

        sampled "P7.S10 h advances gvar"
            (gen {
                let faces = gvarNoHvarFaces ()
                let! struct (font, index) = Gen.elements faces
                let face = faceOf font index
                let! glyph = Gen.choose (0, face.GlyphCount - 1) |> Gen.map uint32
                let! coords = Gens.coordsFor font index
                return struct (font, index, glyph, coords)
            })
            10000
            (fun struct (font, index, glyph, coords) ->
                Font.hAdvance (ourFontAt font index coords ((faceOf font index).Upem)) glyph)
            (fun struct (font, index, glyph, coords) ->
                oracleHAdvance font index (coords |> Seq.toArray) glyph)

        sampled "P7.S10 v metrics gvar"
            (gen {
                let faces = verticalMetricFaces ()
                let! struct (font, index) = Gen.elements faces
                let face = faceOf font index
                let! glyph = Gen.choose (0, face.GlyphCount - 1) |> Gen.map uint32
                let! coords = Gens.coordsFor font index
                return struct (font, index, glyph, coords)
            })
            5000
            (fun struct (font, index, glyph, coords) ->
                let font = ourFontAt font index coords ((faceOf font index).Upem)
                struct (Font.vAdvance font glyph, ourVOrigin font glyph))
            (fun struct (font, index, glyph, coords) ->
                let arr = coords |> Seq.toArray
                struct (oracleVAdvance font index arr glyph, oracleVOrigin font index arr glyph))

        sampled "P7.S10 extents varied"
            (gen {
                let glyphs = gvarGlyphInputs ()
                let! i = Gen.choose (0, glyphs.Length - 1)
                let struct (font, index, gid) = glyphs[i]
                let! coords = Gens.coordsFor font index
                return struct (font, index, gid, coords)
            })
            10000
            (fun struct (font, index, glyph, coords) ->
                oursExtents (ourFontAt font index coords ((faceOf font index).Upem)) glyph)
            (fun struct (font, index, glyph, coords) ->
                oracleExtents font index (coords |> Seq.toArray) glyph)

        testCase "P7.S10 glyph id range" (fun () ->
            let bytes = overLongLocaFace ()
            let failures =
                [ 2u; 3u; 4u; 5u; 0xFFFFu ]
                |> List.choose (compareGlyphIdRange bytes)
            Expect.isEmpty failures ("glyph-id-range differences:\n" + String.concat "\n" failures))

        testCase "P7.S10 mutated composites" (fun () ->
            let failures = ResizeArray<string>()
            let mutable compared = 0
            let limit = 5000
            let mutable stop = false
            let faces = OracleFaces.all ()
            let mutable fi = 0
            while fi < faces.Length && not stop do
                let struct (font, index) = faces[fi]
                let face = faceOf font index
                let glyf = glyfFaceOf font index
                let gvar = Gvar.load face
                match gvar.GlyphCount with
                | 0 -> ()
                | _ ->
                    let axisCount = gvar.AxisCount
                    match tableRange face Tags.Glyf, tableRange face Tags.Loca with
                    | ValueSome (struct (glyfOffset, _)), ValueSome _ ->
                        let mutable gid = 0u
                        while int gid < face.GlyphCount && not stop do
                            let locaSpan = glyf.Loca.Span
                            let start, finish =
                                match glyf.ShortOffsets with
                                | true -> int (BinaryCalls.u16 locaSpan (int gid * 2)) * 2, int (BinaryCalls.u16 locaSpan (int gid * 2 + 2)) * 2
                                | false -> int (BinaryCalls.u32 locaSpan (int gid * 4)), int (BinaryCalls.u32 locaSpan (int gid * 4 + 4))
                            let bytes = OracleFaces.data font.Path
                            match start <= finish && finish <= glyf.Glyf.Length && finish - start >= 10 with
                            | false -> gid <- gid + 1u
                            | true ->
                                let at = glyfOffset + start
                                match readU16 bytes at with
                                | 0xFFFFus ->
                                    let compositeEnd = glyfOffset + finish
                                    let componentAt = at + 10
                                    let flagsAt = componentAt
                                    let glyphIdAt = componentAt + 2
                                    let argAt = componentAt + 4
                                    let rewrite (label: string) (f: byte array -> unit) =
                                        let mutated = Array.copy bytes
                                        f mutated
                                        label, mutated
                                    let tail =
                                        match compositeEnd > componentAt + 6 with
                                        | false -> []
                                        | true ->
                                            let lastFlagsAt = componentAt
                                            let lastSize = compositeEnd - componentAt
                                            match lastSize >= 4 with
                                            | false -> []
                                            | true ->
                                                [ rewrite "MORE_COMPONENTS on last" (fun b -> writeU16 b lastFlagsAt (readU16 b lastFlagsAt ||| 0x0020us))
                                                  rewrite "truncated tail" (fun b -> b[compositeEnd - 1] <- 0uy; b[compositeEnd - 2] <- 0uy) ]
                                    let mutations =
                                        [ rewrite "flags=0" (fun b -> writeU16 b flagsAt 0us)
                                          rewrite "flags=words" (fun b -> writeU16 b flagsAt (readU16 b flagsAt ||| 0x0001us))
                                          rewrite "flags=xy" (fun b -> writeU16 b flagsAt (readU16 b flagsAt ||| 0x0002us))
                                          rewrite "flags=scale" (fun b -> writeU16 b flagsAt (readU16 b flagsAt ||| 0x0008us))
                                          rewrite "flags=use-my-metrics" (fun b -> writeU16 b flagsAt (readU16 b flagsAt ||| 0x0200us))
                                          rewrite "gid=self" (fun b -> writeU16 b glyphIdAt (uint16 gid))
                                          rewrite "gid=zero" (fun b -> writeU16 b glyphIdAt 0us)
                                          rewrite "gid=back" (fun b -> writeU16 b glyphIdAt (if gid = 0u then 1us else 0us))
                                          rewrite "arg=zero" (fun b -> b[argAt] <- 0uy; b[argAt + 1] <- 0uy)
                                          rewrite "arg=max" (fun b -> b[argAt] <- 0xFFuy; b[argAt + 1] <- 0xFFuy) ]
                                        @ tail
                                    let coords = ImmutableArray.CreateRange (Array.create axisCount 16384)
                                    for label, mutatedBytes in mutations do
                                        if not stop then
                                            for phantomOnly in [ false; true ] do
                                                if not stop then
                                                    compared <- compared + 1
                                                    match compareMutated mutatedBytes index gid phantomOnly coords with
                                                    | None -> ()
                                                    | Some detail -> failures.Add($"{font.Path}#{index} gid {gid} phantom={phantomOnly} {label}: {detail}")
                                                    if compared >= limit then stop <- true
                                    gid <- gid + 1u
                                | _ -> gid <- gid + 1u
                    | _ -> ()
                fi <- fi + 1
            printfn "P7.S10 compared %d mutated composite cases" compared
            Expect.isEmpty failures ("mutated-composite differences:\n" + String.concat "\n" failures))
    ]
