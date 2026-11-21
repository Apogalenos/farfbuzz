module Farfbuzz.Diff.Cff1ExtentsDiff

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
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

/// <summary>Whether a face reads extents from the CFF1 charstrings.</summary>
/// <param name="face">The face.</param>
/// <returns>True for a CFF1 face without bitmap, colour, glyf or CFF2 outlines.</returns>
let private isCff1 (face: Face) =
    (Cff1.load face).Valid
    && Face.tryFindTable face Tags.Cff2 |> ValueOption.isNone
    && Face.tryFindTable face Tags.Glyf |> ValueOption.isNone
    && Face.tryFindTable face Tags.Sbix |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cbdt |> ValueOption.isNone
    && Face.tryFindTable face Tags.Colr |> ValueOption.isNone
    && Face.tryFindTable face Tags.Varc |> ValueOption.isNone

/// <summary>The CFF1 test fonts under <c>test/api/fonts/cff1_*.otf</c>.</summary>
/// <returns>The test fonts.</returns>
let private cff1ExtraFonts () : TestFont array =
    let upstream = RepositoryPaths.upstream ()
    let directory = Path.Combine(upstream, "test", "api", "fonts")
    Directory.EnumerateFiles(directory, "cff1_*.otf")
    |> Seq.map (fun path -> { Path = path; Suite = "extra"; Index = 0 })
    |> Seq.toArray

/// <summary>Every CFF1 face of the oracle corpus and of the <c>cff1_*.otf</c> fonts.</summary>
/// <returns>The test font and face index pairs.</returns>
let private cff1Faces () : struct (TestFont * uint32) array =
    let fromAll =
        OracleFaces.all ()
        |> Seq.filter (fun struct (font, index) -> isCff1 (faceOf font index))
        |> Seq.map (fun struct (font, index) -> struct (font, index))
    let fromExtra =
        cff1ExtraFonts ()
        |> Seq.filter (fun font -> isCff1 (faceOf font (uint32 font.Index)))
        |> Seq.map (fun font -> struct (font, uint32 font.Index))
    Seq.append fromAll fromExtra |> Seq.toArray

/// <summary>The three scales the extents differential draws from, <c>upem</c>, <c>3·upem+7</c> and <c>−upem</c>.</summary>
/// <param name="upem">The units per em.</param>
/// <returns>The scale pairs.</returns>
let private scalePairs (upem: int) : struct (int * int) array =
    [| struct (upem, upem); struct (3 * upem + 7, 3 * upem + 7); struct (-upem, -upem) |]

/// <summary>Cached oracle fonts at arbitrary scales.</summary>
let private fontCache = ConcurrentDictionary<struct (string * uint32 * int * int), Lazy<nativeint>>()

/// <summary>Creates a cached OT-func oracle font at a scale.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="xScale">The x scale.</param>
/// <param name="yScale">The y scale.</param>
/// <returns>The font handle.</returns>
let private oracleFontAt (font: TestFont) (index: uint32) (xScale: int) (yScale: int) : nativeint =
    fontCache.GetOrAdd(struct (font.Path, index, xScale, yScale), fun _ -> lazy (
        let face = OracleFaces.face font index
        let handle = Native.hb_font_create face
        Native.hb_ot_font_set_funcs handle
        Native.hb_font_set_scale(handle, xScale, yScale)
        handle)).Value

/// <summary>Farfbuzz's extents as a comparable tuple.</summary>
/// <param name="font">The Farfbuzz font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers.</returns>
let private oursExtents (font: Font) (glyph: uint32) : struct (bool * int * int * int * int) =
    match Extents.glyph font glyph with
    | Ok ValueNone -> struct (false, 0, 0, 0, 0)
    | Ok (ValueSome extents) -> struct (true, extents.XBearing, extents.YBearing, extents.Width, extents.Height)
    | Error _ -> struct (false, -1, -1, -1, -1)

/// <summary>The oracle's extents as a comparable tuple.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers.</returns>
let private oracleExtents (handle: nativeint) (glyph: uint32) : struct (bool * int * int * int * int) =
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(handle, glyph, &extents)
    struct (present <> 0, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>The inputs of the exhaustive extents differential: every glyph of every CFF1 face at three scales.</summary>
/// <returns>The inputs.</returns>
let private cff1Inputs () : struct (TestFont * uint32 * uint32 * struct (int * int)) list =
    cff1Faces ()
    |> Seq.collect (fun struct (font, index) ->
        let face = faceOf font index
        [ for scale in scalePairs face.Upem do
            for glyph in 0u .. uint32 face.GlyphCount -> struct (font, index, glyph, scale) ])
    |> Seq.toList

/// <summary>The synthetic differential inputs: a CFF1 face, glyph, embolden strength and slant.</summary>
/// <returns>The generator.</returns>
let private syntheticInputs () =
    gen {
        let! struct (font, index) = Gen.elements (cff1Faces ())
        let face = faceOf font index
        let! glyph = Gen.choose (0, int face.GlyphCount)
        let! embolden = Gen.elements [| ValueNone; ValueSome (struct (0.05f, 0.05f)) |]
        let! slant = Gen.elements [| 0f; 0.2f; -0.1f |]
        return struct (font, index, uint32 glyph, embolden, slant)
    }

/// <summary>Farfbuzz's synthetic extents, a not-ported answer for an in-range synthetic glyph.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="embolden">The embolden strength.</param>
/// <param name="slant">The slant.</param>
/// <returns>The extents result.</returns>
let private oursSynthetic (font: TestFont) (index: uint32) (glyph: uint32) (embolden: struct (float32 * float32) voption) (slant: float32) =
    let font = Font.create (faceOf font index)
    let font = match embolden with | ValueSome (x, y) -> Font.withSyntheticBold x y false font | ValueNone -> font
    let font = match slant <> 0f with | true -> Font.withSyntheticSlant slant font | false -> font
    Extents.glyph font glyph

/// <summary>The oracle's synthetic extents.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="embolden">The embolden strength.</param>
/// <param name="slant">The slant.</param>
/// <returns>Presence and the four numbers.</returns>
let private oracleSynthetic (font: TestFont) (index: uint32) (glyph: uint32) (embolden: struct (float32 * float32) voption) (slant: float32) : struct (bool * int * int * int * int) =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem (OracleFaces.face font index))
    Native.hb_font_set_scale(handle, upem, upem)
    match embolden with
    | ValueSome (x, y) -> Native.hb_font_set_synthetic_bold(handle, x, y, 0)
    | ValueNone -> ()
    match slant <> 0f with
    | true -> Native.hb_font_set_synthetic_slant(handle, slant)
    | false -> ()
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(handle, glyph, &extents)
    Native.hb_font_destroy handle
    struct (present <> 0, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>Whether a synthetic answer agrees: a not-ported answer agrees with a present oracle answer, no extents
/// agrees with an absent oracle answer, and a computed answer matches exactly.</summary>
/// <param name="ours">Farfbuzz's answer.</param>
/// <param name="expected">The oracle's answer.</param>
/// <returns>True when they agree.</returns>
let private syntheticAgrees (ours: Result<GlyphExtents voption, ShapeError>) (expected: struct (bool * int * int * int * int)) =
    match ours, expected with
    | Ok ValueNone, struct (false, _, _, _, _) -> true
    | Error _, struct (true, _, _, _, _) -> true
    | Ok (ValueSome extents), struct (true, xb, yb, w, h) ->
        extents.XBearing = xb && extents.YBearing = yb && extents.Width = w && extents.Height = h
    | _ -> false

/// <summary>Reads the CFF table's absolute offset in the file.</summary>
/// <param name="face">The face.</param>
/// <returns>The offset, or -1 when the table is absent.</returns>
let private cffTableAbsAt (face: Face) : int =
    match Face.tryFindTable face Tags.Cff with
    | ValueNone -> -1
    | ValueSome record -> int (face.BaseOffset + record.Offset)

/// <summary>Compares Farfbuzz and the oracle on one mutated file, counting HarfBuzz's own rejections.</summary>
/// <param name="failures">The collected differences.</param>
/// <param name="rejects">The number of cases HarfBuzz itself rejects.</param>
/// <param name="label">The mutation label.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="mutated">The mutated file bytes.</param>
let private compareMutated (failures: ResizeArray<string>) (rejects: int ref) (label: string) (index: uint32) (glyph: uint32) (mutated: byte array) =
    let ourFont = Font.create (Face.create (ReadOnlyMemory mutated) index)
    let ours = oursExtents ourFont glyph
    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
    let oracleFace = Native.hb_face_create(blob, index)
    let oracleFont = Native.hb_font_create oracleFace
    Native.hb_ot_font_set_funcs oracleFont
    let upem = int (Native.hb_face_get_upem oracleFace)
    Native.hb_font_set_scale(oracleFont, upem, upem)
    let oracle = oracleExtents oracleFont glyph
    Native.hb_font_destroy oracleFont
    Native.hb_face_destroy oracleFace
    Native.hb_blob_destroy blob
    match oracle with
    | struct (false, _, _, _, _) -> rejects.Value <- rejects.Value + 1
    | _ -> ()
    match ours = oracle with
    | true -> ()
    | false -> failures.Add($"{label} glyph {glyph}: oracle %A{oracle}, farfbuzz %A{ours}")

/// <summary>The mutated-charstring differential: one byte of a real charstring rewritten at a time, sampled over every glyph.</summary>
let private mutatedCharstrings () =
    testCase "P7.S16 mutated charstrings" (fun () ->
        let failures = ResizeArray<string>()
        let rejects = ref 0
        let reservoir = ResizeArray<struct (TestFont * uint32 * uint32 * int * byte * bool)>()
        let rnd = Random 0xCFF116
        let mutable total = 0
        for struct (font, index) in cff1Faces () do
            let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
            let cff = Cff1.load face
            match cff.Valid with
            | false -> ()
            | true ->
                for glyph in 0u .. uint32 (int cff.GlyphCount - 1) do
                    let struct (_, charLength) = CffIndex.item cff.Table.Span cff.CharStrings glyph
                    match charLength with
                    | 0 -> ()
                    | _ ->
                        for byteIndex in 0 .. charLength - 1 do
                            for replacement in [ 0uy; 1uy; 12uy; 14uy; 255uy ] do
                                total <- total + 1
                                let descriptor = struct (font, index, glyph, byteIndex, replacement, false)
                                match total <= 10000 with
                                | true -> reservoir.Add descriptor
                                | false ->
                                    let j = rnd.Next(total)
                                    match j < 10000 with
                                    | true -> reservoir[j] <- descriptor
                                    | false -> ()
                        for cut in [ charLength - 1; charLength / 2 ] do
                            match cut > 0 && cut < charLength with
                            | false -> ()
                            | true ->
                                total <- total + 1
                                let descriptor = struct (font, index, glyph, cut, 0uy, true)
                                match total <= 10000 with
                                | true -> reservoir.Add descriptor
                                | false ->
                                    let j = rnd.Next(total)
                                    match j < 10000 with
                                    | true -> reservoir[j] <- descriptor
                                    | false -> ()
        let mutable cases = 0
        for struct (font, index, glyph, at, replacement, cut) in reservoir do
            let bytes = OracleFaces.data font.Path
            let face = Face.create (ReadOnlyMemory bytes) index
            let tableAbsAt = cffTableAbsAt face
            let cff = Cff1.load face
            let struct (charOffset, _) = CffIndex.item cff.Table.Span cff.CharStrings glyph
            let mutated = Array.copy bytes
            match cut with
            | false ->
                mutated[tableAbsAt + charOffset + at] <- replacement
                compareMutated failures rejects $"byte {at} <- {replacement}" index glyph mutated
            | true ->
                let recordIndex = face.Directory |> Seq.findIndex (fun r -> r.Tag = Tags.Cff)
                let lengthFieldAt = int face.BaseOffset + 12 + recordIndex * 16 + 12
                let shortened = uint32 (tableAbsAt - int face.BaseOffset + charOffset + at)
                mutated[lengthFieldAt..lengthFieldAt + 3] <- [| byte (shortened >>> 24); byte (shortened >>> 16); byte (shortened >>> 8); byte shortened |]
                compareMutated failures rejects $"truncated at {at}" index glyph mutated
            cases <- cases + 1
        printfn "P7.S16 compared %d mutated charstrings, %d rejected by HarfBuzz" cases rejects.Value
        Expect.isEmpty failures ("mutated-charstring differences:\n" + String.concat "\n" failures))

/// <summary>Rewrites one byte of a table and compares Farfbuzz against the oracle.</summary>
/// <param name="failures">The collected differences.</param>
/// <param name="rejects">The number of cases HarfBuzz itself rejects.</param>
/// <param name="bytes">The file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="at">The absolute byte offset.</param>
/// <param name="replacement">The replacement byte.</param>
/// <param name="label">The mutation label.</param>
let private mutateByte (failures: ResizeArray<string>) (rejects: int ref) (bytes: byte array) (index: uint32) (glyph: uint32) (at: int) (replacement: byte) (label: string) =
    match at >= 0 && at < bytes.Length with
    | false -> ()
    | true ->
        let mutated = Array.copy bytes
        mutated[at] <- replacement
        compareMutated failures rejects label index glyph mutated

/// <summary>Rewrites a 16-bit field of a table and compares Farfbuzz against the oracle.</summary>
/// <param name="failures">The collected differences.</param>
/// <param name="rejects">The number of cases HarfBuzz itself rejects.</param>
/// <param name="bytes">The file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="at">The absolute byte offset.</param>
/// <param name="replacement">The replacement value.</param>
/// <param name="label">The mutation label.</param>
let private mutateU16 (failures: ResizeArray<string>) (rejects: int ref) (bytes: byte array) (index: uint32) (glyph: uint32) (at: int) (replacement: uint16) (label: string) =
    match at >= 0 && at + 2 <= bytes.Length with
    | false -> ()
    | true ->
        let mutated = Array.copy bytes
        mutated[at] <- byte (replacement >>> 8)
        mutated[at + 1] <- byte replacement
        compareMutated failures rejects label index glyph mutated

/// <summary>Finds the last occurrence of a byte in a span.</summary>
/// <param name="span">The bytes.</param>
/// <param name="value">The byte.</param>
/// <param name="index">The next byte to check.</param>
/// <returns>The offset, or -1.</returns>
[<TailCall>]
let rec private lastIndexOfLoop (span: ReadOnlySpan<byte>) (value: byte) (index: int) : int =
    match index < 0 with
    | true -> -1
    | false -> match span[index] = value with | true -> index | false -> lastIndexOfLoop span value (index - 1)

/// <summary>Finds the last occurrence of a byte in a span.</summary>
/// <param name="span">The bytes.</param>
/// <param name="value">The byte.</param>
/// <returns>The offset, or -1.</returns>
let private lastIndexOf (span: ReadOnlySpan<byte>) (value: byte) : int =
    lastIndexOfLoop span value (span.Length - 1)

/// <summary>Reads the CFF1 FDArray offset by re-parsing the top dict.</summary>
/// <param name="cff">The accelerator.</param>
/// <returns>The FDArray offset, 0 when absent.</returns>
let private cff1FdArrayAt (cff: Cff1Accel) : int =
    let span = cff.Table.Span
    match span.Length with
    | 0 -> 0
    | _ ->
        let headerSize = int (BinaryCalls.u8 span 2)
        let nameIndex = CffIndex.read span headerSize 2
        let topOffset = headerSize + int (CffIndex.size span nameIndex)
        let topIndex = CffIndex.read span topOffset 2
        let struct (topData, topLength) = CffIndex.item span topIndex 0u
        match CffDict.interpret (span.Slice(topData, topLength)) with
        | ValueNone -> 0
        | ValueSome ops ->
            let mutable fdArray = 0
            for op in ops do
                match op.Op with
                | 1236 when op.Operands.Length > 0 -> fdArray <- int op.Operands[op.Operands.Length - 1]
                | _ -> ()
            fdArray

/// <summary>The mutated-CFF1-table differential: FDSelect, subroutine INDEX and private-dict fields.</summary>
let private mutatedCff1Tables () =
    testCase "P7.S16 mutated cff1 tables" (fun () ->
        let failures = ResizeArray<string>()
        let rejects = ref 0
        let mutable cases = 0
        for struct (font, index) in cff1Faces () do
            let bytes = OracleFaces.data font.Path
            let face = Face.create (ReadOnlyMemory bytes) index
            let tableAbsAt = cffTableAbsAt face
            let cff = Cff1.load face
            match cff.Valid && tableAbsAt >= 0 && cff.GlyphCount > 0u with
            | false -> ()
            | true ->
                let glyph = 1u
                let globalAt = tableAbsAt + cff.GlobalSubrs.Start
                match cff.GlobalSubrs.Count with
                | 0u -> ()
                | _ ->
                    for replacement in [ 0us; 1us; uint16 cff.GlobalSubrs.Count - 1us; uint16 cff.GlobalSubrs.Count + 1us; 0xFFFFus ] do
                        mutateU16 failures rejects bytes index glyph globalAt replacement "global subrs count"
                        cases <- cases + 1
                    for replacement in [ 0uy; 1uy; 5uy; 0xFFuy ] do
                        mutateByte failures rejects bytes index glyph (globalAt + 2) replacement "global subrs offSize"
                        cases <- cases + 1
                for subrs in cff.LocalSubrs do
                    match subrs.Count with
                    | 0u -> ()
                    | _ ->
                        let localAt = tableAbsAt + subrs.Start
                        for replacement in [ 0us; 1us; uint16 subrs.Count - 1us; uint16 subrs.Count + 1us; 0xFFFFus ] do
                            mutateU16 failures rejects bytes index glyph localAt replacement "local subrs count"
                            cases <- cases + 1
                        for replacement in [ 0uy; 1uy; 5uy; 0xFFuy ] do
                            mutateByte failures rejects bytes index glyph (localAt + 2) replacement "local subrs offSize"
                            cases <- cases + 1
                let fdArrayAt = cff1FdArrayAt cff
                match fdArrayAt with
                | 0 -> ()
                | _ ->
                    let fdArray = CffIndex.read cff.Table.Span fdArrayAt 2
                    match fdArray.Count with
                    | 0u -> ()
                    | _ ->
                        let struct (fontDictOffset, fontDictLength) = CffIndex.item cff.Table.Span fdArray 0u
                        match fontDictLength with
                        | 0 -> ()
                        | _ ->
                            let privateAt = lastIndexOf (cff.Table.Span.Slice(fontDictOffset, fontDictLength)) 18uy
                            match privateAt >= 2 with
                            | false -> ()
                            | true ->
                                for replacement in [ 0xFFuy; 0uy ] do
                                    mutateByte failures rejects bytes index glyph (tableAbsAt + fontDictOffset + privateAt - 1) replacement "private dict offset"
                                    cases <- cases + 1
                                    mutateByte failures rejects bytes index glyph (tableAbsAt + fontDictOffset + privateAt - 2) replacement "private dict size"
                                    cases <- cases + 1
                match cff.FdSelect with
                | Cff1FdSelect.NoFdSelect -> ()
                | Cff1FdSelect.FdSelect0 fdsAt ->
                    for replacement in [ 1uy; 3uy; 0xFFuy ] do
                        mutateByte failures rejects bytes index glyph (tableAbsAt + fdsAt - 1) replacement "FDSelect format"
                        cases <- cases + 1
                | Cff1FdSelect.FdSelect3 (rangesAt, rangeCount) ->
                    for replacement in [ 0uy; 1uy; 0xFFuy ] do
                        mutateByte failures rejects bytes index glyph (tableAbsAt + rangesAt - 3) replacement "FDSelect format"
                        cases <- cases + 1
                    for replacement in [ 0us; 1us; uint16 rangeCount + 1us; 0xFFFFus ] do
                        mutateU16 failures rejects bytes index glyph (tableAbsAt + rangesAt - 2) replacement "FDSelect range count"
                        cases <- cases + 1
        printfn "P7.S16 compared %d mutated CFF1 tables, %d rejected by HarfBuzz" cases rejects.Value
        Expect.isEmpty failures ("mutated-CFF1-table differences:\n" + String.concat "\n" failures))

/// <summary>Farfbuzz and HarfBuzz 15.1.0 agree on CFF1 glyph extents and on mutated charstrings.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P7.S16" [
        exhaustive "P7.S16 cff1 extents" (cff1Inputs ())
            (fun struct (font, index, glyph, scale) ->
                let struct (xScale, yScale) = scale
                oursExtents (Font.withScale xScale yScale (Font.create (faceOf font index))) glyph)
            (fun struct (font, index, glyph, scale) ->
                let struct (xScale, yScale) = scale
                oracleExtents (oracleFontAt font index xScale yScale) glyph)
        testPropertyWithConfig (config 5000) "P7.S16 cff1 extents synthetic" (Prop.forAll (Arb.fromGen (syntheticInputs ())) (fun input ->
            let struct (font, index, glyph, embolden, slant) = input
            let expected = oracleSynthetic font index glyph embolden slant
            let actual = oursSynthetic font index glyph embolden slant
            match syntheticAgrees actual expected with
            | true -> Prop.ofTestable true
            | false -> Prop.ofTestable false |> Prop.label $"input: %A{input}\n  oracle:   %A{expected}\n  farfbuzz: %A{actual}"))
        mutatedCharstrings ()
        mutatedCff1Tables ()
    ]
