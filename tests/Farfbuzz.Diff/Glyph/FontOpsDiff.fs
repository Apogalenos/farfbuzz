module Farfbuzz.Diff.FontOpsDiff

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>The largest font file the mutated differentials copy for each case.</summary>
[<Literal>]
let private SmallestEnough = 400000

/// <summary>A face of a charstring kind: the file, its face index and its bytes.</summary>
type private CffFace =
    { Font: TestFont
      Index: uint32
      Bytes: byte array
      Cff2: bool }

/// <summary>Whether a face has no outlines but CFF or CFF2 charstrings, so that extents read them.</summary>
/// <param name="face">The face.</param>
/// <returns>True for a face whose extents come from its charstrings.</returns>
let private charstringOnly (face: Face) =
    Face.tryFindTable face Tags.Glyf |> ValueOption.isNone
    && Face.tryFindTable face Tags.Sbix |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cbdt |> ValueOption.isNone
    && Face.tryFindTable face Tags.Colr |> ValueOption.isNone
    && Face.tryFindTable face Tags.Varc |> ValueOption.isNone

/// <summary>Every small CFF and CFF2 face of the corpus.</summary>
let private cffFaces =
    lazy (
        OracleFaces.all ()
        |> Seq.choose (fun struct (font, index) ->
            let bytes = OracleFaces.data font.Path
            match bytes.Length < SmallestEnough with
            | false -> None
            | true ->
                let face = Face.create (ReadOnlyMemory bytes) index
                match charstringOnly face, (Cff1.load face).Valid, (Cff2.load face).Valid with
                | true, true, false -> Some { Font = font; Index = index; Bytes = bytes; Cff2 = false }
                | true, false, true -> Some { Font = font; Index = index; Bytes = bytes; Cff2 = true }
                | true, true, true | true, false, false | false, _, _ -> None)
        |> Seq.toArray)

/// <summary>The extents of one glyph as a comparable tuple, Farfbuzz's.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers.</returns>
let private oursExtents (bytes: byte array) (index: uint32) (glyph: uint32) =
    let font = Font.create (Face.create (ReadOnlyMemory bytes) index)
    match Extents.glyph font glyph with
    | Ok ValueNone -> struct (false, 0, 0, 0, 0)
    | Ok (ValueSome extents) -> struct (true, extents.XBearing, extents.YBearing, extents.Width, extents.Height)
    | Error _ -> struct (false, -1, -1, -1, -1)

/// <summary>The extents of one glyph as a comparable tuple, the oracle's.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers.</returns>
let private oracleExtents (bytes: byte array) (index: uint32) (glyph: uint32) =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, index)
    let font = Native.hb_font_create face
    Native.hb_ot_font_set_funcs font
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(font, upem, upem)
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(font, glyph, &extents)
    Native.hb_font_destroy font
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    struct (present <> 0, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>One random charstring token: a number in every encoding, an operator, or a call of a small subroutine.</summary>
/// <param name="random">The generator.</param>
/// <returns>The bytes.</returns>
let private token (random: Random) =
    match random.Next 14 with
    | 0 | 1 | 2 | 3 | 4 | 5 -> [| byte (random.Next(32, 247)) |]
    | 6 -> [| byte (random.Next(247, 255)); byte (random.Next 256) |]
    | 7 -> [| 28uy; byte (random.Next 256); byte (random.Next 256) |]
    | 8 -> [| byte (32 + random.Next 4); (match random.Next 2 with | 0 -> 10uy | _ -> 29uy) |]
    | 9 -> [| 255uy; byte (random.Next 256); byte (random.Next 256); byte (random.Next 256); byte (random.Next 256) |]
    | 10 -> [| 12uy; byte ([| 0; 34; 35; 36; 37 |][random.Next 5]) |]
    | _ -> [| byte ([| 1; 3; 4; 5; 6; 7; 8; 11; 14; 15; 16; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 30; 31 |][random.Next 23]) |]

/// <summary>A random program of tokens, up to a length, ending with an endchar.</summary>
/// <param name="random">The generator.</param>
/// <param name="limit">The most bytes it may take.</param>
/// <returns>The bytes.</returns>
let private program (random: Random) (limit: int) =
    let rec build (soFar: byte list) (length: int) =
        match length >= limit - 1 || random.Next 12 = 0 with
        | true -> List.rev (14uy :: soFar)
        | false ->
            let next = token random
            match length + next.Length > limit - 1 with
            | true -> List.rev (14uy :: soFar)
            | false -> build (List.rev (Array.toList next) @ soFar) (length + next.Length)
    build [] 0 |> List.toArray

/// <summary>Where a face's CFF or CFF2 table sits in its file.</summary>
/// <param name="face">The face.</param>
/// <param name="cff2">Whether the table is CFF2.</param>
/// <returns>The absolute offset.</returns>
let private tableAt (face: Face) (cff2: bool) =
    match Face.tryFindTable face (match cff2 with | true -> Tags.Cff2 | false -> Tags.Cff) with
    | ValueSome record -> int (face.BaseOffset + record.Offset)
    | ValueNone -> 0

/// <summary>Where a glyph's charstring and the global subroutines sit in a face's table.</summary>
/// <param name="face">The face.</param>
/// <param name="cff2">Whether the table is CFF2.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The glyph count, the charstring's offset and length, and the global subroutine INDEX.</returns>
let private layout (face: Face) (cff2: bool) (glyph: uint32) =
    match cff2 with
    | true ->
        let cff = Cff2.load face
        let struct (offset, length) = CffIndex.item cff.Table.Span cff.CharStrings glyph
        struct (cff.NumGlyphs, offset, length, cff.GlobalSubrs, cff.Table)
    | false ->
        let cff = Cff1.load face
        let struct (offset, length) = CffIndex.item cff.Table.Span cff.CharStrings glyph
        struct (int cff.GlyphCount, offset, length, cff.GlobalSubrs, cff.Table)

/// <summary>Compares Farfbuzz and the oracle on one file, counting HarfBuzz's own rejections.</summary>
/// <param name="failures">The collected differences.</param>
/// <param name="rejects">The number of cases HarfBuzz itself rejects.</param>
/// <param name="label">What was changed.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="bytes">The file bytes.</param>
let private compareOne (failures: ResizeArray<string>) (rejects: int ref) (label: string) (index: uint32) (glyph: uint32) (bytes: byte array) =
    let ours = oursExtents bytes index glyph
    let oracle = oracleExtents bytes index glyph
    match oracle with
    | struct (false, _, _, _, _) -> rejects.Value <- rejects.Value + 1
    | _ -> ()
    match ours = oracle with
    | true -> ()
    | false -> failures.Add($"{label} glyph {glyph}: oracle %A{oracle}, farfbuzz %A{ours}")

/// <summary>The random-program differential: a charstring, and sometimes a global subroutine, rewritten with random programs.</summary>
/// <param name="name">The test name.</param>
/// <param name="cff2">Whether to take CFF2 faces.</param>
/// <param name="cases">The number of cases.</param>
/// <param name="seed">The random seed.</param>
let private programs (name: string) (cff2: bool) (cases: int) (seed: int) =
    testCase name (fun () ->
        let failures = ResizeArray<string>()
        let rejects = ref 0
        let random = Random seed
        let faces = cffFaces.Value |> Array.filter (fun face -> face.Cff2 = cff2)
        Expect.isNonEmpty faces "the corpus has faces of the kind"
        for _ in 1 .. cases do
            let cffFace = faces[random.Next faces.Length]
            let face = Face.create (ReadOnlyMemory cffFace.Bytes) cffFace.Index
            let table = tableAt face cff2
            let struct (count, _, _, _, _) = layout face cff2 0u
            let glyph = uint32 (random.Next(0, count + 3))
            let struct (_, offset, length, globals, cffTable) = layout face cff2 glyph
            let changed = Array.copy cffFace.Bytes
            match length > 0 with
            | false -> ()
            | true ->
                let written = program random (min length 40)
                Array.blit written 0 changed (table + offset) written.Length
            match random.Next 4 = 0 && globals.Count > 0u with
            | false -> ()
            | true ->
                let struct (subrOffset, subrLength) = CffIndex.item cffTable.Span globals (uint32 (random.Next(0, int globals.Count)))
                match subrLength > 1 with
                | false -> ()
                | true ->
                    let written = Array.append (program random (min (subrLength - 1) 30)) [| 11uy |] |> Array.truncate subrLength
                    Array.blit written 0 changed (table + subrOffset) written.Length
            compareOne failures rejects "random program" cffFace.Index glyph changed
        printfn "%s compared %d cases, %d rejected by HarfBuzz" name cases rejects.Value
        Expect.isEmpty failures (String.concat "\n" failures))

/// <summary>The limits differential: a subroutine that calls itself and a charstring of 513 and of 514 operands.</summary>
/// <param name="name">The test name.</param>
/// <param name="cff2">Whether to take CFF2 faces.</param>
let private limits (name: string) (cff2: bool) =
    testCase name (fun () ->
        let failures = ResizeArray<string>()
        let rejects = ref 0
        let mutable cases = 0
        for cffFace in cffFaces.Value |> Array.filter (fun face -> face.Cff2 = cff2) do
            let face = Face.create (ReadOnlyMemory cffFace.Bytes) cffFace.Index
            let table = tableAt face cff2
            let struct (count, _, _, globals, cffTable) = layout face cff2 0u
            for glyph in 0 .. min (count - 1) 60 do
                let struct (_, offset, length, _, _) = layout face cff2 (uint32 glyph)
                match globals.Count > 0u with
                | false -> ()
                | true ->
                    let struct (subrOffset, subrLength) = CffIndex.item cffTable.Span globals 0u
                    match subrLength >= 2 && length >= 2 && globals.Count < 1240u with
                    | false -> ()
                    | true ->
                        let changed = Array.copy cffFace.Bytes
                        changed[table + subrOffset] <- 32uy
                        changed[table + subrOffset + 1] <- 29uy
                        changed[table + offset] <- 32uy
                        changed[table + offset + 1] <- 29uy
                        compareOne failures rejects "self-recursive subroutine" cffFace.Index (uint32 glyph) changed
                        cases <- cases + 1
                for operands in [ 513; 514 ] do
                    match length > operands with
                    | false -> ()
                    | true ->
                        let changed = Array.copy cffFace.Bytes
                        Array.fill changed (table + offset) operands 139uy
                        changed[table + offset + operands] <- 14uy
                        compareOne failures rejects $"{operands} operands" cffFace.Index (uint32 glyph) changed
                        cases <- cases + 1
        printfn "%s compared %d cases, %d rejected by HarfBuzz" name cases rejects.Value
        Expect.isGreaterThan cases 0 "the corpus has a case"
        Expect.isEmpty failures (String.concat "\n" failures))

/// <summary>Reads a batch of horizontal and vertical advances through Farfbuzz.</summary>
/// <param name="font">The font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The advances, horizontal then vertical.</returns>
let private oursAdvances (font: Font) (glyphs: uint32 array) =
    let horizontal = Font.hAdvances font (ReadOnlySpan glyphs) |> Farfbuzz.Harness.TailCalls.settle
    let vertical = Font.vAdvances font (ReadOnlySpan glyphs) |> Farfbuzz.Harness.TailCalls.settle
    Array.append (Seq.toArray horizontal) (Seq.toArray vertical)

/// <summary>The advance differential: batches of glyphs at random scales against the oracle's batched advances.</summary>
let private advances () =
    testCase "P9 batched advances at random scales" (fun () ->
        let failures = ResizeArray<string>()
        let random = Random 0xAD5
        let faces = OracleFaces.all () |> Seq.toArray
        let mutable cases = 0
        for _ in 1 .. 6000 do
            let struct (font, index) = faces[random.Next faces.Length]
            let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
            let upem = face.Upem
            let xScale, yScale =
                match random.Next 4 with
                | 0 -> upem, upem
                | 1 -> random.Next(1, 4 * upem), random.Next(1, 4 * upem)
                | 2 -> -random.Next(1, upem), random.Next(1, upem)
                | _ -> 3 * upem + 7, 3 * upem + 7
            let ours = Font.withScale xScale yScale (Font.create face)
            let glyphs = Array.init (random.Next(1, 60)) (fun _ -> match random.Next 10 with | 0 -> uint32 (random.Next(face.GlyphCount, face.GlyphCount + 4)) | _ -> uint32 (random.Next(0, max 1 face.GlyphCount)))
            let handle = Native.hb_font_create (OracleFaces.face font index)
            Native.hb_ot_font_set_funcs handle
            Native.hb_font_set_scale(handle, xScale, yScale)
            let horizontal = Array.zeroCreate glyphs.Length
            let vertical = Array.zeroCreate glyphs.Length
            Native.hb_font_get_glyph_h_advances(handle, uint32 glyphs.Length, glyphs, 4u, horizontal, 4u)
            Native.hb_font_get_glyph_v_advances(handle, uint32 glyphs.Length, glyphs, 4u, vertical, 4u)
            Native.hb_font_destroy handle
            match oursAdvances ours glyphs = Array.append horizontal vertical with
            | true -> ()
            | false -> failures.Add($"{font.Path} index {index} scale {xScale},{yScale}: advances differ for {glyphs}")
            cases <- cases + 1
        printfn "P9 batched advances compared %d cases" cases
        Expect.isEmpty failures (String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>The cmap differential: nominal glyphs of edge and random code points after rewriting bytes of the cmap table.</summary>
let private cmaps () =
    testCase "P9 nominal glyphs after rewriting cmap bytes" (fun () ->
        let failures = ResizeArray<string>()
        let rejects = ref 0
        let random = Random 0xC4A9
        let faces =
            OracleFaces.all ()
            |> Seq.filter (fun struct (font, _) -> (OracleFaces.data font.Path).Length < SmallestEnough)
            |> Seq.toArray
        for _ in 1 .. 6000 do
            let struct (font, index) = faces[random.Next faces.Length]
            let bytes = OracleFaces.data font.Path
            let face = Face.create (ReadOnlyMemory bytes) index
            match Face.tryFindTable face Tags.Cmap with
            | ValueNone -> ()
            | ValueSome record ->
                let changed = Array.copy bytes
                for _ in 1 .. random.Next(0, 4) do
                    changed[int face.BaseOffset + int record.Offset + random.Next(0, max 1 (int record.Length))] <- byte (random.Next 256)
                let mapped =
                    let cmap = (Font.create face).Ot.Cmap.Value
                    Cmap.collectUnicodes cmap face.GlyphCount
                    |> Seq.collect (fun range -> seq { for codepoint in range.First .. min range.Last (range.First + 40u) -> codepoint })
                    |> Seq.toArray
                let codepoints =
                    Array.init 300 (fun step ->
                        match step % 5 with
                        | 0 | 1 -> (match mapped.Length with | 0 -> 0x41u | length -> mapped[random.Next length] + uint32 (random.Next(0, 3)) - 1u)
                        | 2 -> uint32 (random.Next(0, 0x2200))
                        | 3 -> uint32 (random.Next(0x2000, 0x30000))
                        | _ -> [| 0x10FFFFu; 0x110000u; 0xFFFFFFFFu; 0x20FFu; 0x2100u; 0u; 255u; 256u |][random.Next 8])
                let ours = Font.create (Face.create (ReadOnlyMemory changed) index)
                let blob = Native.hb_blob_create(changed, uint32 changed.Length, 0, 0n, 0n)
                let oracleFace = Native.hb_face_create(blob, index)
                let handle = Native.hb_font_create oracleFace
                Native.hb_ot_font_set_funcs handle
                let expected =
                    codepoints
                    |> Array.map (fun codepoint ->
                        let glyph = [| 0u |]
                        Native.hb_font_get_nominal_glyphs(handle, 1u, [| codepoint |], 4u, glyph, 4u)
                        glyph[0])
                Native.hb_font_destroy handle
                Native.hb_face_destroy oracleFace
                Native.hb_blob_destroy blob
                match Array.forall (fun glyph -> glyph = 0u) expected with
                | true -> rejects.Value <- rejects.Value + 1
                | false -> ()
                let actual = codepoints |> Array.map (fun codepoint -> match Font.nominalGlyph ours codepoint with | ValueSome glyph -> glyph | ValueNone -> 0u)
                match expected = actual with
                | true -> ()
                | false -> failures.Add($"{font.Path} index {index}: nominal glyphs differ")
        printfn "P9 nominal glyphs compared 6000 rewritten cmaps, %d answer no glyph at all in HarfBuzz" rejects.Value
        Expect.isEmpty failures (String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>The differentials of the font operations the performance work changed.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P9 font operations" [
        programs "P9 cff1 random charstring programs" false 6000 0xCFF1
        programs "P9 cff2 random charstring programs" true 6000 0xCFF2
        limits "P9 cff1 nesting and operand limits" false
        limits "P9 cff2 nesting and operand limits" true
        advances ()
        cmaps ()
    ]
