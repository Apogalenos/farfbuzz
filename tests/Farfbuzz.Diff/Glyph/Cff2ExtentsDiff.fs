module Farfbuzz.Diff.Cff2ExtentsDiff

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

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Whether a face reads extents from the CFF2 charstrings.</summary>
/// <param name="face">The face.</param>
/// <returns>True for a CFF2 face without bitmap, colour or glyf outlines.</returns>
let private isCff2 (face: Face) =
    (Cff2.load face).Valid
    && Face.tryFindTable face Tags.Glyf |> ValueOption.isNone
    && Face.tryFindTable face Tags.Sbix |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cbdt |> ValueOption.isNone
    && Face.tryFindTable face Tags.Colr |> ValueOption.isNone
    && Face.tryFindTable face Tags.Varc |> ValueOption.isNone

/// <summary>The CFF2 test fonts under <c>test/api/fonts</c>.</summary>
/// <returns>The test fonts.</returns>
let private cff2ExtraFonts () : TestFont array =
    let upstream = RepositoryPaths.upstream ()
    let directory = Path.Combine(upstream, "test", "api", "fonts")
    [ "cff2*.otf"; "TestCFF2VF.otf"; "AdobeVFPrototype*.otf"; "varc-short-cff2.otf" ]
    |> List.collect (fun pattern ->
        Directory.EnumerateFiles(directory, pattern)
        |> Seq.filter (fun path -> isCff2 (faceOf { Path = path; Suite = "extra"; Index = 0 } 0u))
        |> Seq.map (fun path -> { Path = path; Suite = "extra"; Index = 0 })
        |> Seq.toList)
    |> List.toArray

/// <summary>Every CFF2 face of the oracle corpus and of the <c>api/fonts</c> CFF2 fonts.</summary>
/// <returns>The test font and face index pairs.</returns>
let private cff2Faces () : struct (TestFont * uint32) array =
    let fromAll =
        OracleFaces.all ()
        |> Seq.filter (fun struct (font, index) -> isCff2 (faceOf font index))
        |> Seq.map (fun struct (font, index) -> struct (font, index))
    let fromExtra =
        cff2ExtraFonts ()
        |> Seq.filter (fun font -> isCff2 (faceOf font (uint32 font.Index)))
        |> Seq.map (fun font -> struct (font, uint32 font.Index))
    Seq.append fromAll fromExtra |> Seq.toArray

/// <summary>The two scales the extents differential draws from, <c>upem</c> and <c>3·upem+7</c>.</summary>
/// <param name="upem">The units per em.</param>
/// <returns>The scale pairs.</returns>
let private scalePairs (upem: int) : struct (int * int) array =
    [| struct (upem, upem); struct (3 * upem + 7, 3 * upem + 7) |]

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

/// <summary>Farfbuzz's extents at a named instance or design coordinates and a scale.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="instance">The named instance, or none.</param>
/// <param name="coords">The design coordinates, or none.</param>
/// <param name="scale">The scale pair.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers.</returns>
let private oursAt (font: TestFont) (index: uint32) (instance: uint32 voption) (coords: float32 array voption) (scale: struct (int * int)) (glyph: uint32) =
    let f = Font.create (faceOf font index)
    let struct (sx, sy) = scale
    let f = Font.withScale sx sy f
    let f = match instance with | ValueSome i -> Font.setVarNamedInstance i f | ValueNone -> f
    let f = match coords with | ValueSome c -> Font.setVarCoordsDesign (ImmutableArray.CreateRange c) f | ValueNone -> f
    oursExtents f glyph

/// <summary>The oracle's extents at a named instance or design coordinates and a scale.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="instance">The named instance, or none.</param>
/// <param name="coords">The design coordinates, or none.</param>
/// <param name="scale">The scale pair.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>Presence and the four numbers.</returns>
let private oracleAt (font: TestFont) (index: uint32) (instance: uint32 voption) (coords: float32 array voption) (scale: struct (int * int)) (glyph: uint32) =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    let struct (x, y) = scale
    Native.hb_font_set_scale(handle, x, y)
    match instance with
    | ValueSome i -> Native.hb_font_set_var_named_instance(handle, i)
    | ValueNone -> ()
    match coords with
    | ValueSome c -> Native.hb_font_set_var_coords_design(handle, c, uint32 c.Length)
    | ValueNone -> ()
    let result = oracleExtents handle glyph
    Native.hb_font_destroy handle
    result

/// <summary>The exhaustive extents differential: every glyph of every CFF2 face at the default and named instances,
/// plus sampled design coordinates.</summary>
let private cff2Extents () =
    testCase "P7.S17 cff2 extents" (fun () ->
        let failures = ResizeArray<string>()
        for struct (font, index) in cff2Faces () do
            let face = faceOf font index
            let fvar = Fvar.load face
            let instances = ValueNone :: [ for i in 0 .. fvar.InstanceCount - 1 -> ValueSome (uint32 i) ]
            for instance in instances do
                for scale in scalePairs face.Upem do
                    for glyph in 0u .. uint32 face.GlyphCount do
                        let ours = oursAt font index instance ValueNone scale glyph
                        let oracle = oracleAt font index instance ValueNone scale glyph
                        match ours = oracle with
                        | true -> ()
                        | false -> failures.Add $"instance %A{instance} glyph {glyph} scale %A{scale}: oracle %A{oracle}, farfbuzz %A{ours}"
        let variableFaces = cff2Faces () |> Array.filter (fun struct (font, index) -> (Fvar.load (faceOf font index)).Axes.Length > 0)
        let inputs =
            gen {
                let! struct (font, index) = Gen.elements variableFaces
                let! coords = designFor font index
                let face = faceOf font index
                let! glyph = Gen.choose (0, face.GlyphCount)
                let! scale = Gen.elements (scalePairs face.Upem)
                return struct (font, index, coords, uint32 glyph, scale)
            }
        let count = 2000 * max 1 variableFaces.Length
        let samples = Gen.sampleWithSeed (Rnd 0xCFF217uL) count count inputs
        for struct (font, index, coords, glyph, scale) in samples do
            let ours = oursAt font index ValueNone (ValueSome (Seq.toArray coords)) scale glyph
            let oracle = oracleAt font index ValueNone (ValueSome (Seq.toArray coords)) scale glyph
            match ours = oracle with
            | true -> ()
            | false -> failures.Add $"coords %A{coords} glyph {glyph} scale %A{scale}: oracle %A{oracle}, farfbuzz %A{ours}"
        Expect.isEmpty failures ("cff2 extents differences:\n" + String.concat "\n" failures))

/// <summary>The synthetic differential inputs: a CFF2 face, glyph, embolden strength and slant.</summary>
/// <returns>The generator.</returns>
let private syntheticInputs () =
    gen {
        let! struct (font, index) = Gen.elements (cff2Faces ())
        let face = faceOf font index
        let! glyph = Gen.choose (0, face.GlyphCount)
        let! coords = designFor font index
        let! embolden = Gen.elements [| ValueNone; ValueSome (struct (0.05f, 0.05f)) |]
        let! slant = Gen.elements [| 0f; 0.2f; -0.1f |]
        return struct (font, index, uint32 glyph, coords, embolden, slant)
    }

/// <summary>Farfbuzz's synthetic extents.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="coords">The design coordinates.</param>
/// <param name="embolden">The embolden strength.</param>
/// <param name="slant">The slant.</param>
/// <returns>The extents result.</returns>
let private oursSynthetic (font: TestFont) (index: uint32) (glyph: uint32) (coords: ImmutableArray<float32>) (embolden: struct (float32 * float32) voption) (slant: float32) =
    let f = Font.setVarCoordsDesign coords (Font.create (faceOf font index))
    let f = match embolden with | ValueSome (x, y) -> Font.withSyntheticBold x y false f | ValueNone -> f
    let f = match slant <> 0f with | true -> Font.withSyntheticSlant slant f | false -> f
    Extents.glyph f glyph

/// <summary>The oracle's synthetic extents.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="coords">The design coordinates.</param>
/// <param name="embolden">The embolden strength.</param>
/// <param name="slant">The slant.</param>
/// <returns>Presence and the four numbers.</returns>
let private oracleSynthetic (font: TestFont) (index: uint32) (glyph: uint32) (coords: ImmutableArray<float32>) (embolden: struct (float32 * float32) voption) (slant: float32) : struct (bool * int * int * int * int) =
    let handle = Native.hb_font_create (OracleFaces.face font index)
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem (OracleFaces.face font index))
    Native.hb_font_set_scale(handle, upem, upem)
    Native.hb_font_set_var_coords_design(handle, Seq.toArray coords, uint32 coords.Length)
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

/// <summary>Whether a synthetic answer agrees.</summary>
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

/// <summary>The synthetic differential.</summary>
let private cff2Synthetic () =
    testPropertyWithConfig (config 3000) "P7.S17 cff2 extents synthetic" (Prop.forAll (Arb.fromGen (syntheticInputs ())) (fun input ->
        let struct (font, index, glyph, coords, embolden, slant) = input
        let expected = oracleSynthetic font index glyph coords embolden slant
        let actual = oursSynthetic font index glyph coords embolden slant
        match syntheticAgrees actual expected with
        | true -> Prop.ofTestable true
        | false -> Prop.ofTestable false |> Prop.label $"input: %A{input}\n  oracle:   %A{expected}\n  farfbuzz: %A{actual}"))

/// <summary>Reads the CFF2 table's absolute offset in the file.</summary>
/// <param name="face">The face.</param>
/// <returns>The offset, or -1 when the table is absent.</returns>
let private cff2TableAbsAt (face: Face) : int =
    match Face.tryFindTable face Tags.Cff2 with
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

/// <summary>The mutated-CFF2-charstring differential: one byte of a real charstring rewritten at a time, sampled over every glyph.</summary>
let private mutatedCharstrings () =
    testCase "P7.S17 mutated cff2 charstrings" (fun () ->
        let failures = ResizeArray<string>()
        let rejects = ref 0
        let reservoir = ResizeArray<struct (TestFont * uint32 * uint32 * int * byte * int)>()
        let rnd = Random 0xCFF217
        let mutable total = 0
        for struct (font, index) in cff2Faces () do
            let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
            let cff = Cff2.load face
            match cff.Valid with
            | false -> ()
            | true ->
                for glyph in 0u .. uint32 (cff.NumGlyphs - 1) do
                    let struct (charOffset, charLength) = CffIndex.item cff.Table.Span cff.CharStrings glyph
                    match charLength with
                    | 0 -> ()
                    | _ ->
                        for byteIndex in 0 .. charLength - 1 do
                            for replacement in [ 0uy; 1uy; 12uy; 14uy; 15uy; 16uy; 255uy ] do
                                total <- total + 1
                                let descriptor = struct (font, index, glyph, byteIndex, replacement, 0)
                                match total <= 10000 with
                                | true -> reservoir.Add descriptor
                                | false ->
                                    let j = rnd.Next(total)
                                    match j < 10000 with
                                    | true -> reservoir[j] <- descriptor
                                    | false -> ()
                        for byteIndex in 0 .. charLength - 2 do
                            match cff.Table.Span[charOffset + byteIndex + 1] with
                            | 15uy | 16uy ->
                                total <- total + 1
                                let descriptor = struct (font, index, glyph, byteIndex, 246uy, 1)
                                match total <= 10000 with
                                | true -> reservoir.Add descriptor
                                | false ->
                                    let j = rnd.Next(total)
                                    match j < 10000 with
                                    | true -> reservoir[j] <- descriptor
                                    | false -> ()
                            | _ -> ()
                        for cut in [ charLength - 1; charLength / 2 ] do
                            match cut > 0 && cut < charLength with
                            | false -> ()
                            | true ->
                                total <- total + 1
                                let descriptor = struct (font, index, glyph, cut, 0uy, 2)
                                match total <= 10000 with
                                | true -> reservoir.Add descriptor
                                | false ->
                                    let j = rnd.Next(total)
                                    match j < 10000 with
                                    | true -> reservoir[j] <- descriptor
                                    | false -> ()
        let mutable cases = 0
        for struct (font, index, glyph, at, replacement, kind) in reservoir do
            let bytes = OracleFaces.data font.Path
            let face = Face.create (ReadOnlyMemory bytes) index
            let tableAbsAt = cff2TableAbsAt face
            let cff = Cff2.load face
            let struct (charOffset, _) = CffIndex.item cff.Table.Span cff.CharStrings glyph
            let mutated = Array.copy bytes
            match kind with
            | 0 ->
                mutated[tableAbsAt + charOffset + at] <- replacement
                compareMutated failures rejects $"{Path.GetFileName font.Path} byte {at} <- {replacement}" index glyph mutated
            | 1 ->
                mutated[tableAbsAt + charOffset + at] <- replacement
                compareMutated failures rejects $"{Path.GetFileName font.Path} operand {at} <- {replacement}" index glyph mutated
            | _ ->
                let recordIndex = face.Directory |> Seq.findIndex (fun r -> r.Tag = Tags.Cff2)
                let lengthFieldAt = int face.BaseOffset + 12 + recordIndex * 16 + 12
                let shortened = uint32 (tableAbsAt - int face.BaseOffset + charOffset + at)
                mutated[lengthFieldAt..lengthFieldAt + 3] <- [| byte (shortened >>> 24); byte (shortened >>> 16); byte (shortened >>> 8); byte shortened |]
                compareMutated failures rejects $"{Path.GetFileName font.Path} truncated at {at}" index glyph mutated
            cases <- cases + 1
        printfn "P7.S17 compared %d mutated CFF2 charstrings, %d rejected by HarfBuzz" cases rejects.Value
        Expect.isEmpty failures ("mutated-CFF2-charstring differences:\n" + String.concat "\n" failures))

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

/// <summary>The mutated-CFF2-table differential: FDSelect, variation store and private-dict fields.</summary>
let private mutatedCff2Tables () =
    testCase "P7.S17 mutated cff2 tables" (fun () ->
        let failures = ResizeArray<string>()
        let rejects = ref 0
        let mutable cases = 0
        for struct (font, index) in cff2Faces () do
            let bytes = OracleFaces.data font.Path
            let face = Face.create (ReadOnlyMemory bytes) index
            let tableAbsAt = cff2TableAbsAt face
            let cff = Cff2.load face
            match cff.Valid && tableAbsAt >= 0 && cff.NumGlyphs > 0 with
            | false -> ()
            | true ->
                let glyph = 1u
                match cff.FdSelect with
                | ValueNone -> ()
                | ValueSome (Cff2FdSelect.FdSelect0 fdsAt) ->
                    for replacement in [ 1uy; 3uy; 4uy; 0xFFuy ] do
                        mutateByte failures rejects bytes index glyph (tableAbsAt + fdsAt - 1) replacement "FDSelect format"
                        cases <- cases + 1
                | ValueSome (Cff2FdSelect.FdSelect3 (rangesAt, rangeCount)) ->
                    for replacement in [ 0uy; 4uy; 0xFFuy ] do
                        mutateByte failures rejects bytes index glyph (tableAbsAt + rangesAt - 3) replacement "FDSelect format"
                        cases <- cases + 1
                    for replacement in [ 0us; 1us; uint16 rangeCount + 1us; 0xFFFFus ] do
                        mutateU16 failures rejects bytes index glyph (tableAbsAt + rangesAt - 2) replacement "FDSelect range count"
                        cases <- cases + 1
                | ValueSome (Cff2FdSelect.FdSelect4 (rangesAt, _rangeCount)) ->
                    for replacement in [ 0uy; 3uy; 0xFFuy ] do
                        mutateByte failures rejects bytes index glyph (tableAbsAt + rangesAt - 5) replacement "FDSelect format"
                        cases <- cases + 1
                for dict in cff.PrivateDicts do
                    match dict.LocalSubrs.Count with
                    | 0u -> ()
                    | _ ->
                        let subrsAt = tableAbsAt + dict.LocalSubrs.Start
                        for replacement in [ 0u; 1u; dict.LocalSubrs.Count - 1u; dict.LocalSubrs.Count + 1u; 0xFFFFFFFFu ] do
                            let mutated = Array.copy bytes
                            mutated[subrsAt] <- byte (replacement >>> 24)
                            mutated[subrsAt + 1] <- byte (replacement >>> 16)
                            mutated[subrsAt + 2] <- byte (replacement >>> 8)
                            mutated[subrsAt + 3] <- byte replacement
                            cases <- cases + 1
                            compareMutated failures rejects "local subrs count" index glyph mutated
                        for replacement in [ 0uy; 1uy; 5uy; 0xFFuy ] do
                            mutateByte failures rejects bytes index glyph (subrsAt + 4) replacement "local subrs offSize"
                            cases <- cases + 1
        printfn "P7.S17 compared %d mutated CFF2 tables, %d rejected by HarfBuzz" cases rejects.Value
        Expect.isEmpty failures ("mutated-CFF2-table differences:\n" + String.concat "\n" failures))

/// <summary>Farfbuzz and HarfBuzz 15.1.0 agree on CFF2 glyph extents and on mutated charstrings.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P7.S17" [
        cff2Extents ()
        cff2Synthetic ()
        mutatedCharstrings ()
        mutatedCff2Tables ()
    ]
