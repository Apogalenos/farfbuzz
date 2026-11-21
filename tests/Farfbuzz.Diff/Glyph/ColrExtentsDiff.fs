module Farfbuzz.Diff.ColrExtentsDiff

open System
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.OpenType
open Farfbuzz.Oracle
open Farfbuzz.Var
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.Gens
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>The directories under <c>upstream/harfbuzz/test</c> searched for COLR fonts, besides the shaping suites.</summary>
let private colrDirectories = [ "api/fonts"; "fuzzing/fonts"; "subset/data/fonts" ]

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Every face with a COLR table: the shaping suites' fonts and the COLR fonts of the API, fuzzing and subsetting test folders.</summary>
/// <returns>The test font and face index pairs, distinct.</returns>
let private colrFaces () =
    let upstream = RepositoryPaths.upstream ()
    let fromFolders =
        colrDirectories
        |> List.collect (fun folder ->
            let directory = Path.Combine(upstream, "test", folder)
            Directory.EnumerateFiles directory
            |> Seq.filter (fun path -> (FileInfo path).Length > 12L)
            |> Seq.map (fun path -> struct ({ Path = path; Suite = "extra"; Index = 0 }, 0u))
            |> Seq.toList)
    let fromSuites = OracleFaces.all () |> Seq.toList
    fromSuites @ fromFolders
    |> List.filter (fun struct (font, index) -> Face.tryFindTable (faceOf font index) Tags.Colr |> ValueOption.isSome)
    |> List.distinctBy (fun struct (font, index) -> struct (font.Path, index))
    |> List.toArray

/// <summary>The glyphs a COLR face is compared on: the first few, every glyph its table paints or clips, and two past the end.</summary>
/// <param name="face">The face.</param>
/// <param name="limit">The most glyphs to take from each source, evenly spread.</param>
/// <param name="headCount">The number of leading glyphs to take.</param>
/// <returns>The glyph ids, ascending.</returns>
let private glyphsOf (face: Face) (limit: int) (headCount: int) : uint32 array =
    let colr = Colr.load face
    let table = colr.Table
    let spread (ids: uint32 array) =
        match ids.Length <= limit with
        | true -> ids
        | false -> [| for i in 0 .. limit - 1 -> ids[i * ids.Length / limit] |]
    let records = [| for i in 0 .. colr.NumBaseGlyphs - 1 -> uint32 (Colr.u16 table (colr.BaseGlyphsAt + i * Colr.BaseGlyphRecordSize)) |]
    let paints =
        match colr.Version >= 1 && colr.BaseGlyphListAt <> 0 with
        | false -> [||]
        | true ->
            let count = int (min 100000u (Colr.u32 table colr.BaseGlyphListAt))
            [| for i in 0 .. count - 1 -> uint32 (Colr.u16 table (colr.BaseGlyphListAt + 4 + i * 6)) |]
    let clips =
        match colr.Version >= 1 && colr.ClipListAt <> 0 with
        | false -> [||]
        | true ->
            let count = int (min 100000u (Colr.u32 table (colr.ClipListAt + 1)))
            [| for i in 0 .. count - 1 do
                   yield uint32 (Colr.u16 table (colr.ClipListAt + 5 + i * 7))
                   yield uint32 (Colr.u16 table (colr.ClipListAt + 7 + i * 7)) |]
    let head = [| 0u .. uint32 (min face.GlyphCount headCount) |]
    let tail = [| uint32 face.GlyphCount; uint32 face.GlyphCount + 1u |]
    Array.concat [ head; spread records; spread paints; spread clips; tail ] |> Array.distinct |> Array.sort

/// <summary>Farfbuzz's extents as a comparable tuple; a not-yet-ported source is reported apart.</summary>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers, or none when a source is not ported.</returns>
let private oursExtents (font: Font) (glyph: uint32) =
    match Extents.glyph font glyph with
    | Ok ValueNone -> ValueSome (struct (false, 0, 0, 0, 0))
    | Ok (ValueSome extents) -> ValueSome (struct (true, extents.XBearing, extents.YBearing, extents.Width, extents.Height))
    | Error _ -> ValueNone

/// <summary>The oracle's extents as a comparable tuple.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers.</returns>
let private oracleExtents (handle: nativeint) (glyph: uint32) =
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(handle, glyph, &extents)
    match present with
    | 0 -> struct (false, 0, 0, 0, 0)
    | _ -> struct (true, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>The two fonts of one comparison: Farfbuzz's and the oracle's.</summary>
/// <param name="oursFace">Farfbuzz's face.</param>
/// <param name="oracleFace">The oracle's face handle.</param>
/// <param name="scale">The scale pair.</param>
/// <param name="instance">The named instance, or none.</param>
/// <param name="coords">The design coordinates, or none.</param>
/// <returns>Farfbuzz's font and the oracle's handle.</returns>
let private fontsAt (oursFace: Face) (oracleFace: nativeint) (scale: struct (int * int)) (instance: uint32 voption) (coords: float32 array voption) =
    let struct (sx, sy) = scale
    let ours = Font.withScale sx sy (Font.create oursFace)
    let ours = match instance with | ValueSome i -> Font.setVarNamedInstance i ours | ValueNone -> ours
    let ours = match coords with | ValueSome c -> Font.setVarCoordsDesign (ImmutableArray.CreateRange c) ours | ValueNone -> ours
    let handle = Native.hb_font_create oracleFace
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_scale(handle, sx, sy)
    match instance with
    | ValueSome i -> Native.hb_font_set_var_named_instance(handle, i)
    | ValueNone -> ()
    match coords with
    | ValueSome c -> Native.hb_font_set_var_coords_design(handle, c, uint32 c.Length)
    | ValueNone -> ()
    struct (ours, handle)

/// <summary>Compares the extents of the glyphs of one pair of fonts.</summary>
/// <param name="failures">The collected differences.</param>
/// <param name="label">What the comparison is, for the message.</param>
/// <param name="ours">Farfbuzz's font.</param>
/// <param name="handle">The oracle's font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The number of glyphs compared and the number not ported.</returns>
let private compareGlyphs (failures: ResizeArray<string>) (label: string) (ours: Font) (handle: nativeint) (glyphs: uint32 array) =
    let mutable compared = 0
    let mutable notPorted = 0
    for glyph in glyphs do
        let expected = oracleExtents handle glyph
        match oursExtents ours glyph with
        | ValueNone -> notPorted <- notPorted + 1
        | ValueSome actual ->
            compared <- compared + 1
            match actual = expected with
            | true -> ()
            | false -> failures.Add $"{label} glyph {glyph}: oracle %A{expected}, farfbuzz %A{actual}"
    struct (compared, notPorted)

/// <summary>The scales the differential draws from: the em, a larger odd one, a negative one, unequal axes and a zero one.</summary>
/// <param name="upem">The units per em.</param>
/// <returns>The scale pairs.</returns>
let private scalePairs (upem: int) : struct (int * int) array =
    [| struct (upem, upem)
       struct (3 * upem + 7, 3 * upem + 7)
       struct (-upem, upem)
       struct (upem, 3 * upem / 2 + 1)
       struct (0, upem)
       struct (1, 1) |]

/// <summary>The extents of every COLR face at the default coordinates, at every scale.</summary>
let private defaultExtents () =
    testCase "P7.COLR extents at default coordinates" (fun () ->
        let failures = ResizeArray<string>()
        let mutable compared = 0
        let mutable notPorted = 0
        for struct (font, index) in colrFaces () do
            let face = faceOf font index
            let glyphs = glyphsOf face 600 40
            for scale in scalePairs face.Upem do
                let struct (ours, handle) = fontsAt face (OracleFaces.face font index) scale ValueNone ValueNone
                let struct (c, n) = compareGlyphs failures $"{Path.GetFileName font.Path}#{index} scale %A{scale}" ours handle glyphs
                compared <- compared + c
                notPorted <- notPorted + n
                Native.hb_font_destroy handle
        Console.WriteLine $"COLR extents: {compared} compared, {notPorted} not ported"
        Expect.isGreaterThan compared 1000 "the differential must compare glyphs"
        Expect.isEmpty failures ("first mismatches:\n" + String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>The extents of the variable COLR faces at every named instance and at sampled design coordinates.</summary>
let private variedExtents () =
    testCase "P7.COLR extents at varied coordinates" (fun () ->
        let failures = ResizeArray<string>()
        let mutable compared = 0
        let variable =
            colrFaces () |> Array.filter (fun struct (font, index) -> (Fvar.load (faceOf font index)).Axes.Length > 0)
        for struct (font, index) in variable do
            let face = faceOf font index
            let fvar = Fvar.load face
            let glyphs = glyphsOf face 120 40
            let instances = [ for i in 0 .. fvar.InstanceCount - 1 -> ValueSome (uint32 i) ]
            for instance in instances do
                let struct (ours, handle) = fontsAt face (OracleFaces.face font index) (struct (face.Upem, face.Upem)) instance ValueNone
                let struct (c, _) = compareGlyphs failures $"{Path.GetFileName font.Path}#{index} instance %A{instance}" ours handle glyphs
                compared <- compared + c
                Native.hb_font_destroy handle
            let samples = Gen.sampleWithSeed (Rnd 0xC0102uL) 40 40 (designFor font index)
            for coords in samples do
                let struct (ours, handle) = fontsAt face (OracleFaces.face font index) (struct (face.Upem, face.Upem)) ValueNone (ValueSome (Seq.toArray coords))
                let struct (c, _) = compareGlyphs failures $"{Path.GetFileName font.Path}#{index} coords %A{coords}" ours handle glyphs
                compared <- compared + c
                Native.hb_font_destroy handle
        Console.WriteLine $"COLR extents varied: {compared} compared over {variable.Length} variable faces"
        Expect.isGreaterThan compared 1000 "the differential must compare glyphs"
        Expect.isEmpty failures ("first mismatches:\n" + String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>Encodes an unsigned value big-endian in a number of bytes.</summary>
/// <param name="width">The number of bytes, 1 to 4.</param>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private bigEndian (width: int) (value: uint32) : byte array =
    [| for i in width - 1 .. -1 .. 0 -> byte (value >>> (8 * i)) |]

/// <summary>Creates an oracle font over mutated bytes at scale = upem.</summary>
/// <param name="mutated">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <returns>The font handle, and the face and blob to destroy with it.</returns>
let private mutatedOracle (mutated: byte array) (index: uint32) =
    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, index)
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(handle, upem, upem)
    struct (handle, face, blob)

/// <summary>The mutations of one COLR table: every header field, then seeded random overwrites weighted towards the start of the table
/// where the lists begin.</summary>
/// <param name="table">The table bytes.</param>
/// <param name="count">The number of random overwrites.</param>
/// <param name="seed">The seed.</param>
/// <returns>The offset in the table, the width and the replacement value of each mutation.</returns>
let private mutationsOf (table: byte array) (count: int) (seed: int) : struct (int * int * uint32) array =
    let random = Random seed
    let headerFields = [ 0, 2; 2, 2; 4, 4; 8, 4; 12, 2; 14, 4; 18, 4; 22, 4; 26, 4; 30, 4 ]
    let headerValues = [| 0u; 1u; 2u; 0xFFu; 0xFFFFu; 0x7FFFFFFFu; 0xFFFFFFFFu |]
    let header =
        [| for (at, width) in headerFields do
               for value in headerValues do
                   yield struct (at, width, value) |]
    let randoms =
        [| for _ in 1 .. count do
               let near = random.Next 3 > 0
               let at = match near with | true -> random.Next(0, min table.Length 600) | false -> random.Next(0, max 1 table.Length)
               let width = [| 1; 2; 3; 4 |][random.Next 4]
               let value = [| 0u; 1u; 0xFFu; 0xFFFFu; 0xFFFFFFFFu; uint32 (random.Next()); uint32 (random.Next()) |][random.Next 7]
               yield struct (at, width, value) |]
    Array.append header randoms |> Array.filter (fun struct (at, width, _) -> at + width <= table.Length)

/// <summary>The extents of altered copies of every COLR font: header fields and random bytes of the COLR table rewritten, both sides
/// asked at every sampled glyph.</summary>
let private mutatedExtents () =
    testCase "P7.COLR extents of altered tables" (fun () ->
        let failures = ResizeArray<string>()
        let mutable compared = 0
        let mutable altered = 0
        for struct (font, index) in colrFaces () do
            let bytes = OracleFaces.data font.Path
            let face = Face.create (ReadOnlyMemory bytes) index
            let record = Face.tryFindTable face Tags.Colr |> ValueOption.get
            let tableAt = int (face.BaseOffset + record.Offset)
            let table = Array.sub bytes tableAt (max 0 (min (int record.Length) (bytes.Length - tableAt)))
            let glyphs = glyphsOf face 12 6
            let count = max 20 (min 120 (2_400_000 / max 1 bytes.Length))
            for struct (at, width, value) in mutationsOf table count (font.Path |> Seq.fold (fun hash letter -> (hash * 31 + int letter) &&& 0xFFFF) 7) do
                let mutated = Array.copy bytes
                Array.blit (bigEndian width value) 0 mutated (tableAt + at) width
                let ours = Font.create (Face.create (ReadOnlyMemory mutated) index)
                let struct (handle, oracleFace, blob) = mutatedOracle mutated index
                let struct (c, _) = compareGlyphs failures $"{Path.GetFileName font.Path}#{index} byte {at} width {width} value {value}" ours handle glyphs
                compared <- compared + c
                altered <- altered + 1
                Native.hb_font_destroy handle
                Native.hb_face_destroy oracleFace
                Native.hb_blob_destroy blob
        Console.WriteLine $"COLR extents altered: {compared} compared over {altered} altered tables"
        Expect.isGreaterThan compared 1000 "the differential must compare glyphs"
        Expect.isEmpty failures ("first mismatches:\n" + String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>The COLR extents differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P7.COLR" [
        defaultExtents ()
        variedExtents ()
        mutatedExtents ()
    ]
