module Farfbuzz.Diff.GvarDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces
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

/// <summary>Farfbuzz's varied points for one simple glyph, compared as bits.</summary>
/// <param name="glyf">The accelerator.</param>
/// <param name="gvar">The gvar table.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="phantomOnly">Whether only the phantoms are wanted.</param>
/// <returns>The points, or none where the glyph fails.</returns>
let private oursPoints (glyf: GlyfFace) (gvar: Gvar) (coords: ImmutableArray<int>) (gid: uint32) (phantomOnly: bool) : struct (uint32 * uint32 * byte) array voption =
    let bytes = GlyphPoints.glyphBytes glyf.Glyf glyf.Loca glyf.ShortOffsets gid
    let span = bytes.Span
    let header =
        { NumberOfContours = BinaryCalls.i16 span 0
          XMin = BinaryCalls.i16 span 2
          YMin = BinaryCalls.i16 span 4
          XMax = BinaryCalls.i16 span 6
          YMax = BinaryCalls.i16 span 8 }
    let contour =
        match GlyphPoints.kind span with
        | CompositeGlyph -> ValueNone
        | EmptyGlyph -> ValueSome ImmutableArray<ContourPoint>.Empty
        | SimpleGlyph _ -> GlyphPoints.simplePoints span false phantomOnly
    match contour with
    | ValueNone -> ValueNone
    | ValueSome contour ->
        let struct (left, right, top, bottom) = GlyphPoints.phantoms glyf gid header
        let allPoints = Array.append (contour |> Seq.toArray) [| left; right; top; bottom |]
        let deltaed =
            match Spans.exists (fun coord -> coord <> 0) (coords.AsSpan()) with
            | false -> ValueSome (ImmutableArray.CreateRange allPoints)
            | true -> Gvar.applyDeltas gvar (coords.AsSpan()) gid phantomOnly (ImmutableArray.CreateRange allPoints)
        match deltaed with
        | ValueNone -> ValueNone
        | ValueSome deltaed ->
            let count = deltaed.Length
            let leftX = deltaed[count - PhantomCount].X
            let shiftStart = if phantomOnly then count - PhantomCount else 0
            let result =
                deltaed
                |> Seq.mapi (fun i point ->
                    let point = if i >= shiftStart && leftX <> 0.0f then { point with X = point.X - leftX } else point
                    struct (BitConverter.SingleToUInt32Bits point.X, BitConverter.SingleToUInt32Bits point.Y, packedFlag point))
                |> Seq.toArray
            ValueSome (if phantomOnly then result[count - PhantomCount .. count - 1] else result)

/// <summary>The oracle's varied points for one glyph, compared as bits.</summary>
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
        let all = Array.init (int count) (fun i ->
            struct (BitConverter.SingleToUInt32Bits xy[2 * i], BitConverter.SingleToUInt32Bits xy[2 * i + 1], flags[i]))
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

/// <summary>Every simple or empty glyph of every gvar face.</summary>
/// <returns>The test font, face index and glyph triples.</returns>
let private gvarGlyphInputs () : struct (TestFont * uint32 * uint32) array =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let face = faceOf font index
        let glyf = GlyfFace.load face
        let gvar = Gvar.load face
        match gvar.GlyphCount with
        | 0 -> [||]
        | _ ->
            [| for gid in 0u .. uint32 (glyf.NumGlyphs - 1) ->
                   let bytes = GlyphPoints.glyphBytes glyf.Glyf glyf.Loca glyf.ShortOffsets gid
                   match GlyphPoints.kind bytes.Span with
                   | CompositeGlyph -> None
                   | EmptyGlyph | SimpleGlyph _ -> Some (struct (font, index, gid)) |]
            |> Array.choose id)
    |> Seq.toArray

/// <summary>A random simple glyph of a random gvar face, a phantom mode and a coordinate draw.</summary>
let private sampledInput : Gen<struct (TestFont * uint32 * uint32 * bool * ImmutableArray<int>)> =
    let glyphs = gvarGlyphInputs ()
    gen {
        let! i = Gen.choose (0, glyphs.Length - 1)
        let struct (font, index, gid) = glyphs[i]
        let! phantomOnly = Gen.choose (0, 1) |> Gen.map (fun n -> n = 1)
        let! coords = Gens.coordsFor font index
        return struct (font, index, gid, phantomOnly, coords)
    }

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

/// <summary>Writes a big-endian uint16 into bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="value">The value.</param>
let private writeU16 (bytes: byte array) (at: int) (value: uint16) =
    bytes[at] <- byte (uint32 value >>> 8)
    bytes[at + 1] <- byte value

/// <summary>Writes a big-endian uint32 into bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="value">The value.</param>
let private writeU32 (bytes: byte array) (at: int) (value: uint32) =
    bytes[at] <- byte (value >>> 24)
    bytes[at + 1] <- byte (value >>> 16)
    bytes[at + 2] <- byte (value >>> 8)
    bytes[at + 3] <- byte value

/// <summary>The data-range of one glyph inside the gvar data region.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="gvarAbs">The gvar table offset.</param>
/// <param name="longOffsets">Whether offsets are 32-bit.</param>
/// <param name="gid">The glyph id.</param>
/// <returns>The start and end offsets.</returns>
let private glyphRange (bytes: byte array) (gvarAbs: int) (longOffsets: bool) (gid: uint32) =
    let headerSize = 20
    match longOffsets with
    | true ->
        readU32 bytes (gvarAbs + headerSize + int gid * 4), readU32 bytes (gvarAbs + headerSize + int (gid + 1u) * 4)
    | false ->
        uint32 (int (readU16 bytes (gvarAbs + headerSize + int gid * 2)) * 2),
        uint32 (int (readU16 bytes (gvarAbs + headerSize + int (gid + 1u) * 2)) * 2)

/// <summary>The mutations of one gvar table for one glyph, one field rewritten at a time.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="gvarAbs">The gvar table offset.</param>
/// <param name="gvarLen">The gvar table length.</param>
/// <param name="axisCount">The gvar axis count.</param>
/// <param name="gid">The glyph id.</param>
/// <returns>The labelled mutated files.</returns>
let private gvarMutations (bytes: byte array) (gvarAbs: int) (gvarLen: int) (axisCount: int) (gid: uint32) : (string * byte array) list =
    let rewrite (label: string) (f: byte array -> unit) =
        let mutated = Array.copy bytes
        f mutated
        label, mutated
    let longOffsets = (readU16 bytes (gvarAbs + 14) &&& 1us) <> 0us
    let dataZ = int (readU32 bytes (gvarAbs + 16))
    let header =
        [ rewrite "version=0" (fun b -> writeU16 b (gvarAbs + 0) 0us)
          rewrite "version=2" (fun b -> writeU16 b (gvarAbs + 0) 2us)
          rewrite "version=0xFFFF" (fun b -> writeU16 b (gvarAbs + 0) 0xFFFFus)
          rewrite "axisCount=0" (fun b -> writeU16 b (gvarAbs + 4) 0us)
          rewrite "axisCount=1" (fun b -> writeU16 b (gvarAbs + 4) 1us)
          rewrite "axisCount=0xFFFF" (fun b -> writeU16 b (gvarAbs + 4) 0xFFFFus)
          rewrite "sharedTupleCount=0" (fun b -> writeU16 b (gvarAbs + 6) 0us)
          rewrite "sharedTupleCount=1" (fun b -> writeU16 b (gvarAbs + 6) 1us)
          rewrite "sharedTupleCount=+1" (fun b -> writeU16 b (gvarAbs + 6) (readU16 bytes (gvarAbs + 6) + 1us))
          rewrite "sharedTupleCount=0xFFFF" (fun b -> writeU16 b (gvarAbs + 6) 0xFFFFus)
          rewrite "sharedTuplesOffset=0" (fun b -> writeU32 b (gvarAbs + 8) 0u)
          rewrite "sharedTuplesOffset=1" (fun b -> writeU32 b (gvarAbs + 8) 1u)
          rewrite "sharedTuplesOffset=0xFFFFFFFF" (fun b -> writeU32 b (gvarAbs + 8) 0xFFFFFFFFu)
          rewrite "glyphCount=0" (fun b -> writeU16 b (gvarAbs + 12) 0us)
          rewrite "glyphCount=1" (fun b -> writeU16 b (gvarAbs + 12) 1us)
          rewrite "glyphCount=0xFFFF" (fun b -> writeU16 b (gvarAbs + 12) 0xFFFFus)
          rewrite "flags=toggle-long" (fun b -> writeU16 b (gvarAbs + 14) (readU16 bytes (gvarAbs + 14) ^^^ 1us)) ]
    let start, finish = glyphRange bytes gvarAbs longOffsets gid
    let hasData = finish >= start + uint32 8 && int start + int dataZ + 8 <= gvarLen && axisCount > 0
    let glyph =
        match hasData with
        | false -> []
        | true ->
            let varDataAbs = gvarAbs + dataZ + int start
            let tupleIndexAbs = varDataAbs + 6
            let offsetAt = gvarAbs + 20 + (if longOffsets then int gid * 4 else int gid * 2)
            [ rewrite "glyphOffset=0" (fun b ->
                  if longOffsets then writeU32 b offsetAt 0u else writeU16 b offsetAt 0us)
              rewrite "glyphOffset=0xFFFF" (fun b ->
                  if longOffsets then writeU32 b offsetAt 0xFFFFFFFFu else writeU16 b offsetAt 0xFFFFus)
              rewrite "tupleCount=0" (fun b -> writeU16 b varDataAbs 0us)
              rewrite "tupleCount=1" (fun b -> writeU16 b varDataAbs 1us)
              rewrite "tupleCount=0xFFFF" (fun b -> writeU16 b varDataAbs 0xFFFFus)
              rewrite "tupleDataSize=0" (fun b -> writeU16 b (varDataAbs + 4) 0us)
              rewrite "tupleDataSize=1" (fun b -> writeU16 b (varDataAbs + 4) 1us)
              rewrite "tupleDataSize=0xFFFF" (fun b -> writeU16 b (varDataAbs + 4) 0xFFFFus)
              rewrite "tupleIndex=0" (fun b -> writeU16 b tupleIndexAbs 0us)
              rewrite "tupleIndex=0xFFFF" (fun b -> writeU16 b tupleIndexAbs 0xFFFFus) ]
    let tail =
        match gvarLen > 0 with
        | false -> []
        | true ->
            [ rewrite "truncated tail" (fun b ->
                  let last = gvarAbs + gvarLen - 1
                  b[last] <- 0uy
                  b[last - 1] <- 0uy
                  b[last - 2] <- 0uy) ]
    header @ glyph @ tail

/// <summary>Compares Farfbuzz and the oracle on one mutated file for one glyph and coordinates.</summary>
/// <param name="mutated">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="phantomOnly">Whether only the phantoms are wanted.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>A mismatch description, or none.</returns>
let private compareMutated (mutated: byte array) (index: uint32) (gid: uint32) (phantomOnly: bool) (coords: ImmutableArray<int>) : string option =
    let ours =
        let face = Face.create (ReadOnlyMemory mutated) index
        let glyf = GlyfFace.load face
        let gvar = Gvar.load face
        oursPoints glyf gvar coords gid phantomOnly
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

/// <summary>The differential tests of P7.S09: sampled varied points and mutated gvar tables.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P7.S09" [
        sampled "P7.S09 simple glyph points varied" sampledInput 20000
            (fun struct (font, index, gid, phantomOnly, coords) ->
                oursPoints (glyfFaceOf font index) (Gvar.load (faceOf font index)) coords gid phantomOnly)
            (fun struct (font, index, gid, phantomOnly, coords) ->
                oracleWithCoords font index gid phantomOnly coords)

        testCase "P7.S09 mutated gvar" (fun () ->
            let failures = ResizeArray<string>()
            let mutable compared = 0
            let mutable mutatedCases = 0
            let limit = 5000
            let mutable stop = false
            let faces = OracleFaces.all ()
            let mutable fi = 0
            while fi < faces.Length && not stop do
                let struct (font, index) = faces[fi]
                let bytes = OracleFaces.data font.Path
                let face = faceOf font index
                let glyf = glyfFaceOf font index
                let gvar = Gvar.load face
                match gvar.GlyphCount, Face.tryFindTable face Tags.Gvar with
                | 0, _ | _, ValueNone -> ()
                | numGlyphs, ValueSome record ->
                    let gvarAbs = int (face.BaseOffset + record.Offset)
                    let gvarLen = min (int record.Length) (bytes.Length - gvarAbs)
                    let axisCount = gvar.AxisCount
                    let coords = ImmutableArray.CreateRange (Array.create axisCount 16384)
                    let mutable gid = 0u
                    while int gid < numGlyphs && not stop do
                        let glyphBytes = GlyphPoints.glyphBytes glyf.Glyf glyf.Loca glyf.ShortOffsets gid
                        match GlyphPoints.kind glyphBytes.Span with
                        | CompositeGlyph -> ()
                        | EmptyGlyph | SimpleGlyph _ ->
                            for phantomOnly in [ false; true ] do
                                for label, mutated in gvarMutations bytes gvarAbs gvarLen axisCount gid do
                                    if not stop then
                                        mutatedCases <- mutatedCases + 1
                                        compared <- compared + 1
                                        match compareMutated mutated index gid phantomOnly coords with
                                        | None -> ()
                                        | Some detail -> failures.Add($"{font.Path}#{index} gid {gid} phantom={phantomOnly} {label}: {detail}")
                                        if compared >= limit then stop <- true
                        gid <- gid + 1u
                fi <- fi + 1
            printfn "P7.S09 compared %d mutated gvar cases" compared
            Expect.isEmpty failures ("mutated-gvar differences:\n" + String.concat "\n" failures))
    ]
