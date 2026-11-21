module Farfbuzz.Diff.GlyphPointsDiff

open System
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

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

/// <summary>Assembles contour points plus the four shifted phantoms, as the oracle reports them.</summary>
/// <param name="contour">The contour points, unshifted.</param>
/// <param name="glyf">The accelerator.</param>
/// <param name="gid">The glyph id.</param>
/// <param name="header">The glyph header.</param>
/// <returns>The points as x bits, y bits and packed flag.</returns>
let private assembled (contour: ContourPoint array) (glyf: GlyfFace) (gid: uint32) (header: GlyphHeader) =
    let struct (left, right, top, bottom) = GlyphPoints.phantoms glyf gid header
    let shift = left.X
    let shifted (point: ContourPoint) =
        match shift = 0.0f with
        | true -> point
        | false -> { point with X = point.X - shift }
    Array.append (Array.map shifted contour) [| shifted left; shifted right; shifted top; shifted bottom |]
    |> Array.map (fun point ->
        struct (BitConverter.SingleToUInt32Bits point.X, BitConverter.SingleToUInt32Bits point.Y, packedFlag point))

/// <summary>Farfbuzz's points for one glyph, phantoms last, or none where the glyph fails to load.</summary>
/// <param name="glyf">The accelerator.</param>
/// <param name="gid">The glyph id.</param>
/// <returns>The points.</returns>
let private oursPoints (glyf: GlyfFace) (gid: uint32) : struct (uint32 * uint32 * byte) array voption =
    let locaSpan = glyf.Loca.Span
    let startU, finishU =
        match glyf.ShortOffsets with
        | true ->
            uint32 (int (BinaryCalls.u16 locaSpan (int gid * 2)) * 2),
            uint32 (int (BinaryCalls.u16 locaSpan (int gid * 2 + 2)) * 2)
        | false ->
            BinaryCalls.u32 locaSpan (int gid * 4), BinaryCalls.u32 locaSpan (int gid * 4 + 4)
    let bad = uint64 gid >= uint64 glyf.NumGlyphs || startU > finishU || uint64 finishU > uint64 glyf.Glyf.Length
    let phantomGid = match bad with | true -> 0xFFFFFFFFu | false -> gid
    let bytes = GlyphPoints.glyphBytes glyf.Glyf glyf.Loca glyf.ShortOffsets gid
    let span = bytes.Span
    let header =
        { NumberOfContours = BinaryCalls.i16 span 0
          XMin = BinaryCalls.i16 span 2
          YMin = BinaryCalls.i16 span 4
          XMax = BinaryCalls.i16 span 6
          YMax = BinaryCalls.i16 span 8 }
    match GlyphPoints.kind span with
    | CompositeGlyph -> ValueNone
    | EmptyGlyph -> ValueSome (assembled [||] glyf phantomGid header)
    | SimpleGlyph _ ->
        match GlyphPoints.simplePoints span false false with
        | ValueNone -> ValueNone
        | ValueSome points -> ValueSome (assembled (points |> Seq.toArray) glyf phantomGid header)

/// <summary>The oracle's points for one glyph, or none where HarfBuzz fails to load it.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="gid">The glyph id.</param>
/// <returns>The points.</returns>
let private oraclePoints (font: nativeint) (gid: uint32) : struct (uint32 * uint32 * byte) array voption =
    let count = Shims.farfbuzz_oracle_glyf_points(font, gid, 0u, Array.empty<float32>, Array.empty<byte>, 0u)
    match count with
    | 0xFFFFFFFFu -> ValueNone
    | count ->
        let xy = Array.zeroCreate<float32> (int count * 2)
        let flags = Array.zeroCreate<byte> (int count)
        Shims.farfbuzz_oracle_glyf_points(font, gid, 0u, xy, flags, count) |> ignore
        ValueSome (Array.init (int count) (fun i ->
            struct (BitConverter.SingleToUInt32Bits xy[2 * i], BitConverter.SingleToUInt32Bits xy[2 * i + 1], flags[i])))

/// <summary>Every simple and empty glyph of every glyf face.</summary>
/// <returns>The test font, face index and glyph triples.</returns>
let private glyphInputs () : struct (TestFont * uint32 * uint32) list =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let glyf = glyfFaceOf font index
        match glyf.NumGlyphs with
        | 0 -> List.empty
        | numGlyphs ->
            [ for gid in 0u .. uint32 (numGlyphs - 1) ->
                  let bytes = GlyphPoints.glyphBytes glyf.Glyf glyf.Loca glyf.ShortOffsets gid
                  match GlyphPoints.kind bytes.Span with
                  | CompositeGlyph -> None
                  | EmptyGlyph | SimpleGlyph _ -> Some (struct (font, index, gid)) ]
            |> List.choose id)
    |> Seq.toList

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Reads a big-endian int16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readI16 (bytes: byte array) (at: int) =
    int16 (readU16 bytes at)

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

/// <summary>The absolute byte range of one table in a face's file.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The offset and length.</returns>
let private tableRange (face: Face) (tag: Tag) : struct (int * int) voption =
    match Face.tryFindTable face tag with
    | ValueNone -> ValueNone
    | ValueSome record -> ValueSome (struct (int (face.BaseOffset + record.Offset), int record.Length))

/// <summary>Rebuilds a Farfbuzz and an oracle answer for one mutated glyph, and compares them.</summary>
/// <param name="mutated">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="gid">The glyph id.</param>
/// <returns>A mismatch description, or none.</returns>
let private compareMutated (mutated: byte array) (index: uint32) (gid: uint32) : string option =
    let ours =
        oursPoints (GlyfFace.load (Face.create (ReadOnlyMemory mutated) index)) gid
    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, index)
    let font = Native.hb_font_create face
    Native.hb_ot_font_set_funcs font
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(font, upem, upem)
    let oracle = oraclePoints font gid
    Native.hb_font_destroy font
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    match ours = oracle with
    | true -> None
    | false -> Some $"oracle %A{oracle}, farfbuzz %A{ours}"

/// <summary>The differential tests of P7.S07: every simple glyph, and mutated glyphs.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P7.S07" [
        exhaustive "P7.S07 points default" (glyphInputs ())
            (fun struct (font, index, gid) -> oursPoints (glyfFaceOf font index) gid)
            (fun struct (font, index, gid) -> oraclePoints (OracleFaces.font font index) gid)

        testCase "P7.S07 mutated glyphs" (fun () ->
            let failures = ResizeArray<string>()
            let mutable compared = 0
            let mutable mutatedGlyphs = 0
            let limit = 5000
            let mutable stop = false
            let faces = OracleFaces.all ()
            let mutable fi = 0
            while fi < faces.Length && not stop do
                let struct (font, index) = faces[fi]
                let face = faceOf font index
                let glyf = glyfFaceOf font index
                match glyf.NumGlyphs with
                | 0 -> ()
                | numGlyphs ->
                    match tableRange face Tags.Glyf, tableRange face Tags.Loca with
                    | ValueSome (struct (glyfOffset, _)), ValueSome (struct (locaOffset, _)) ->
                        let mutable gid = 0u
                        while int gid < numGlyphs && not stop do
                            let locaSpan = glyf.Loca.Span
                            let start, finish =
                                match glyf.ShortOffsets with
                                | true ->
                                    int (BinaryCalls.u16 locaSpan (int gid * 2)) * 2,
                                    int (BinaryCalls.u16 locaSpan (int gid * 2 + 2)) * 2
                                | false ->
                                    int (BinaryCalls.u32 locaSpan (int gid * 4)),
                                    int (BinaryCalls.u32 locaSpan (int gid * 4 + 4))
                            let validGlyph = start <= finish && finish <= glyf.Glyf.Length && finish - start >= 10
                            match validGlyph with
                            | false -> gid <- gid + 1u
                            | true ->
                                let bytes = OracleFaces.data font.Path
                                let at = glyfOffset + start
                                let numContours = int (readI16 bytes at)
                                match numContours <= 0 with
                                | true -> gid <- gid + 1u
                                | false ->
                                    let endPtsBase = at + 10
                                    let numPoints = int (readU16 bytes (endPtsBase + 2 * (numContours - 1))) + 1
                                    let instructionLength = int (readU16 bytes (endPtsBase + 2 * numContours))
                                    let flagsAt = endPtsBase + 2 * (numContours + 1) + instructionLength
                                    let glyphEndAbs = glyfOffset + finish
                                    let rewrite (label: string) (f: byte array -> unit) =
                                        let mutated = Array.copy bytes
                                        f mutated
                                        label, mutated
                                    let locaStartAt, locaEndAt, locaStart, locaFinish =
                                        match glyf.ShortOffsets with
                                        | true ->
                                            let s = locaOffset + int gid * 2
                                            let e = locaOffset + int (gid + 1u) * 2
                                            s, e, int (readU16 bytes s), int (readU16 bytes e)
                                        | false ->
                                            let s = locaOffset + int gid * 4
                                            let e = locaOffset + int (gid + 1u) * 4
                                            s, e, int (readU32 bytes s), int (readU32 bytes e)
                                    let mutations =
                                        [ rewrite "numberOfContours=0" (fun b -> writeU16 b at 0us)
                                          rewrite "endPoint first=0" (fun b -> writeU16 b endPtsBase 0us)
                                          rewrite "endPoint first=last" (fun b -> writeU16 b endPtsBase (uint16 (numPoints - 1)))
                                          rewrite "endPoint last=0" (fun b -> writeU16 b (endPtsBase + 2 * (numContours - 1)) 0us)
                                          rewrite "instructionLength=0" (fun b -> writeU16 b (endPtsBase + 2 * numContours) 0us)
                                          rewrite "instructionLength=1" (fun b -> writeU16 b (endPtsBase + 2 * numContours) 1us)
                                          rewrite "instructionLength=0xFFFF" (fun b -> writeU16 b (endPtsBase + 2 * numContours) 0xFFFFus)
                                          rewrite "truncated tail" (fun b ->
                                              b[glyphEndAbs - 1] <- 0uy
                                              b[glyphEndAbs - 2] <- 0uy) ]
                                        @ (match flagsAt < glyphEndAbs with
                                           | true ->
                                               [ rewrite "flag=0x00" (fun b -> b[flagsAt] <- 0x00uy)
                                                 rewrite "flag=0x01" (fun b -> b[flagsAt] <- 0x01uy)
                                                 rewrite "flag=0x12" (fun b -> b[flagsAt] <- 0x12uy) ]
                                           | false -> [])
                                        @ (match glyf.ShortOffsets with
                                           | true ->
                                               [ rewrite "loca decreasing" (fun b ->
                                                     writeU16 b locaStartAt (uint16 locaFinish)
                                                     writeU16 b locaEndAt (uint16 locaStart))
                                                 rewrite "loca odd" (fun b -> writeU16 b locaStartAt (uint16 (locaStart ||| 1)))
                                                 rewrite "loca past end" (fun b -> writeU16 b locaEndAt 0xFFFFus) ]
                                           | false ->
                                               [ rewrite "loca decreasing" (fun b ->
                                                     writeU32 b locaStartAt (uint32 locaFinish)
                                                     writeU32 b locaEndAt (uint32 locaStart))
                                                 rewrite "loca odd" (fun b -> writeU32 b locaStartAt (uint32 (locaStart ||| 1)))
                                                 rewrite "loca past end" (fun b -> writeU32 b locaEndAt 0xFFFFFFFFu) ])
                                    let mutable m = 0
                                    while m < mutations.Length && not stop do
                                        let label, mutatedBytes = mutations[m]
                                        mutatedGlyphs <- mutatedGlyphs + 1
                                        compared <- compared + 1
                                        match compareMutated mutatedBytes index gid with
                                        | None -> ()
                                        | Some detail -> failures.Add($"{font.Path}#{index} gid {gid} {label}: {detail}")
                                        if compared >= limit then stop <- true
                                        m <- m + 1
                                    gid <- gid + 1u
                    | _ -> ()
                fi <- fi + 1
            printfn "P7.S07 compared %d mutated glyphs across %d cases" mutatedGlyphs compared
            Expect.isEmpty failures ("mutated-glyph differences:\n" + String.concat "\n" failures))
    ]
