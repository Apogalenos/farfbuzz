module Farfbuzz.HarfBuzzSharp.Tests.VariationTests

open System
open System.Runtime.InteropServices
open Expecto
open Farfbuzz.Diff
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>Every face of every test font, once.</summary>
let private faces = lazy (TestFonts.all ())

/// <summary>The oracle's axes of a face.</summary>
/// <param name="face">The oracle face.</param>
/// <returns>The axes.</returns>
let private oracleAxes (face: nativeint) =
    let total = int (Native.hb_ot_var_get_axis_count face)
    let infos = Array.zeroCreate<Native.OracleVarAxisInfo> total
    let mutable count = uint32 total
    Native.hb_ot_var_get_axis_infos(face, 0u, &count, infos) |> ignore
    infos |> Array.truncate (int count)

/// <summary>The facade's axes as the oracle's tuples.</summary>
/// <param name="axes">The facade axes.</param>
/// <returns>Index, tag, name id, flags, minimum, default, maximum.</returns>
let private facadeTuples (axes: HarfBuzzSharp.OpenTypeVarAxisInfo array) =
    axes |> Array.map (fun axis -> axis.AxisIndex, axis.Tag, uint32 axis.NameId, uint32 axis.Flags, axis.MinValue, axis.DefaultValue, axis.MaxValue)

/// <summary>The oracle's named-instance design coordinates.</summary>
/// <param name="face">The oracle face.</param>
/// <param name="instance">The instance.</param>
/// <returns>The coordinates.</returns>
let private oracleInstanceCoords (face: nativeint) (instance: uint32) =
    let mutable none = 0u
    let total = Native.hb_ot_var_named_instance_get_design_coords(face, instance, &none, Unchecked.defaultof<float32 array>)
    let coords = Array.zeroCreate<float32> (int total)
    let mutable count = total
    Native.hb_ot_var_named_instance_get_design_coords(face, instance, &count, coords) |> ignore
    coords |> Array.truncate (int count)

/// <summary>The oracle's normalized coordinates of a font.</summary>
/// <param name="font">The oracle font.</param>
/// <returns>The coordinates.</returns>
let private oracleCoords (font: nativeint) =
    let mutable length = 0u
    let pointer = Native.hb_font_get_var_coords_normalized(font, &length)
    match length with
    | 0u -> [||]
    | _ ->
        let coords = Array.zeroCreate<int> (int length)
        Marshal.Copy(pointer, coords, 0, coords.Length)
        coords

/// <summary>The oracle's colours of a palette.</summary>
/// <param name="face">The oracle face.</param>
/// <param name="palette">The palette.</param>
/// <returns>The colours in a palette and the colours.</returns>
let private oracleColors (face: nativeint) (palette: uint32) =
    let mutable none = 0u
    let total = Native.hb_ot_color_palette_get_colors(face, palette, 0u, &none, Unchecked.defaultof<uint32 array>)
    let colors = Array.zeroCreate<uint32> (int total)
    let mutable count = total
    Native.hb_ot_color_palette_get_colors(face, palette, 0u, &count, colors) |> ignore
    int total, colors |> Array.truncate (int count)

/// <summary>An oracle font over a face, destroyed by the caller.</summary>
/// <param name="font">The test font.</param>
/// <returns>The oracle face and its new font.</returns>
let private oracleFont (font: TestFonts.TestFont) =
    let face = OracleFaces.face font (uint32 font.Index)
    face, Native.hb_font_create face

/// <summary>The variations as the oracle takes them.</summary>
/// <param name="pairs">The tags and values.</param>
/// <returns>The array of variations.</returns>
let private oracleVariations (pairs: (uint32 * float32) list) =
    pairs |> List.map (fun (tag, value) -> ({ Tag = LanguagePrimitives.UInt32WithMeasure tag; Value = value } : Farfbuzz.Variation)) |> Array.ofList

/// <summary>The faces that have variation axes.</summary>
/// <returns>The test fonts.</returns>
let private variableFaces () =
    faces.Value |> List.filter (fun font -> (FacadeFonts.faceOf font.Path font.Index).VariationAxisCount > 0)

/// <summary>The P10.S16 cases: variations, named instances and palettes.</summary>
[<Tests>]
let tests =
    testList "P10.S16" [
        testCase "axes, named instances and axis lookup equal the oracle on every face of every test font" (fun () ->
            OracleSession.ensure ()
            let mutable compared = 0
            let mutable variable = 0
            let differences =
                [ for font in faces.Value do
                    let facade = FacadeFonts.faceOf font.Path font.Index
                    let oracle = OracleFaces.face font (uint32 font.Index)
                    compared <- compared + 1
                    let expectedAxes = oracleAxes oracle |> Array.map (fun axis -> int axis.AxisIndex, axis.Tag, axis.NameId, axis.Flags, axis.MinValue, axis.DefaultValue, axis.MaxValue)
                    let actualAxes = facadeTuples facade.VariationAxisInfos |> Array.map (fun (index, tag, name, flags, minimum, defaults, maximum) -> int index, tag, name, flags, minimum, defaults, maximum)
                    if expectedAxes.Length > 0 then variable <- variable + 1
                    if facade.HasVariationData <> (Native.hb_ot_var_has_data oracle <> 0) then yield $"{font.Path}:{font.Index} has data"
                    if facade.VariationAxisCount <> int (Native.hb_ot_var_get_axis_count oracle) then yield $"{font.Path}:{font.Index} axis count"
                    if actualAxes <> expectedAxes then yield $"{font.Path}:{font.Index} axes"
                    let span = Array.zeroCreate<HarfBuzzSharp.OpenTypeVarAxisInfo> facade.VariationAxisCount
                    if facade.GetVariationAxisInfos(Span span) <> span.Length then yield $"{font.Path}:{font.Index} span count"
                    for tag in [ for axis in expectedAxes -> let _, tag, _, _, _, _, _ = axis in tag ] @ [ 0x5A5A5A5Au ] do
                        let mutable info = Unchecked.defaultof<Native.OracleVarAxisInfo>
                        let expectedFound = Native.hb_ot_var_find_axis_info(oracle, tag, &info) <> 0
                        let found, axis = facade.TryFindVariationAxis(tag)
                        if found <> expectedFound || (found && (axis.AxisIndex, axis.Tag, axis.MinValue, axis.DefaultValue, axis.MaxValue) <> (info.AxisIndex, info.Tag, info.MinValue, info.DefaultValue, info.MaxValue)) then yield $"{font.Path}:{font.Index} find axis {tag:X8}"
                    let instances = int (Native.hb_ot_var_get_named_instance_count oracle)
                    if facade.NamedInstanceCount <> instances then yield $"{font.Path}:{font.Index} instance count"
                    for instance in 0 .. instances do
                        if uint32 (facade.GetNamedInstanceSubfamilyNameId instance) <> Native.hb_ot_var_named_instance_get_subfamily_name_id(oracle, uint32 instance) then yield $"{font.Path}:{font.Index} subfamily {instance}"
                        if uint32 (facade.GetNamedInstancePostScriptNameId instance) <> Native.hb_ot_var_named_instance_get_postscript_name_id(oracle, uint32 instance) then yield $"{font.Path}:{font.Index} postscript {instance}"
                        let expected = oracleInstanceCoords oracle (uint32 instance)
                        if facade.GetNamedInstanceDesignCoords instance <> expected then yield $"{font.Path}:{font.Index} coords {instance}"
                        if facade.GetNamedInstanceDesignCoordsCount instance <> expected.Length then yield $"{font.Path}:{font.Index} coords count {instance}" ]
            printfn $"P10.S16: {compared} faces compared, {variable} with axes"
            Expect.isGreaterThan variable 10 "variable faces"
            Expect.isEmpty (List.truncate 10 differences) (String.concat "\n" (List.truncate 10 differences)))
        testCase "palettes, colour names and colour glyph kinds equal the oracle on every face of every test font" (fun () ->
            OracleSession.ensure ()
            let mutable withPalettes = 0
            let differences =
                [ for font in faces.Value do
                    let facade = FacadeFonts.faceOf font.Path font.Index
                    let oracle = OracleFaces.face font (uint32 font.Index)
                    let count = int (Native.hb_ot_color_palette_get_count oracle)
                    if count > 0 then withPalettes <- withPalettes + 1
                    if facade.HasPalettes <> (Native.hb_ot_color_has_palettes oracle <> 0) then yield $"{font.Path}:{font.Index} has palettes"
                    if facade.PaletteCount <> count then yield $"{font.Path}:{font.Index} palette count"
                    if facade.HasColorLayers <> (Native.hb_ot_color_has_layers oracle <> 0) then yield $"{font.Path}:{font.Index} layers"
                    if facade.HasColorPng <> (Native.hb_ot_color_has_png oracle <> 0) then yield $"{font.Path}:{font.Index} png"
                    if facade.HasColorSvg <> (Native.hb_ot_color_has_svg oracle <> 0) then yield $"{font.Path}:{font.Index} svg"
                    for palette in 0 .. count do
                        let total, expected = oracleColors oracle (uint32 palette)
                        let actual = facade.GetPaletteColors palette |> Array.map (fun color -> color.Value)
                        if actual <> expected then yield $"{font.Path}:{font.Index} colours of palette {palette}"
                        let into = Array.zeroCreate<HarfBuzzSharp.HBColor> (max 0 (total - 1))
                        if facade.GetPaletteColors(palette, Span into) <> max 0 (min (total - 1) expected.Length) then yield $"{font.Path}:{font.Index} colour span of palette {palette}"
                        if uint32 (facade.GetPaletteFlags palette) <> Native.hb_ot_color_palette_get_flags(oracle, uint32 palette) then yield $"{font.Path}:{font.Index} flags of palette {palette}"
                        if uint32 (facade.GetPaletteNameId palette) <> Native.hb_ot_color_palette_get_name_id(oracle, uint32 palette) then yield $"{font.Path}:{font.Index} name of palette {palette}"
                    let colors = match count with | 0 -> 0 | _ -> fst (oracleColors oracle 0u)
                    for color in 0 .. colors do
                        if uint32 (facade.GetPaletteColorNameId color) <> Native.hb_ot_color_palette_color_get_name_id(oracle, uint32 color) then yield $"{font.Path}:{font.Index} name of colour {color}" ]
            printfn $"P10.S16: {withPalettes} faces with palettes"
            Expect.isGreaterThan withPalettes 0 "colour faces"
            Expect.isEmpty (List.truncate 10 differences) (String.concat "\n" (List.truncate 10 differences)))
        testCase "variation values, design and normalized coordinates and named instances leave the same normalized coordinates as in the oracle" (fun () ->
            OracleSession.ensure ()
            let differences =
                [ for font in variableFaces () do
                    let facade = FacadeFonts.faceOf font.Path font.Index
                    let axes = facade.VariationAxisInfos
                    let oracleFace, _ = oracleFont font
                    let check (name: string) (apply: HarfBuzzSharp.Font -> unit) (applyOracle: nativeint -> unit) =
                        let native = Native.hb_font_create oracleFace
                        use plain = new HarfBuzzSharp.Font(facade)
                        apply plain
                        applyOracle native
                        let expected = oracleCoords native
                        let actual = plain.VariationCoordsNormalized
                        Native.hb_font_destroy native
                        match actual = expected with
                        | true -> None
                        | false -> Some $"{font.Path}:{font.Index} {name}: facade {actual} oracle {expected}"
                    yield! check "untouched" ignore ignore |> Option.toList
                    let tagOf (axis: HarfBuzzSharp.OpenTypeVarAxisInfo) = axis.Tag
                    for value in [ (fun (a: HarfBuzzSharp.OpenTypeVarAxisInfo) -> a.MinValue); (fun a -> a.MaxValue); (fun a -> a.DefaultValue); (fun a -> a.MinValue - 25.0f); (fun a -> a.MaxValue + 25.0f); (fun a -> (a.MinValue + a.MaxValue) / 2.0f) ] do
                        let pairs = [ for axis in axes -> tagOf axis, value axis ]
                        let variations = pairs |> List.map (fun (tag, number) -> HarfBuzzSharp.Variation(Tag = tag, Value = number)) |> Array.ofList
                        yield! check "variations" (fun f -> f.SetVariations(ReadOnlySpan variations)) (fun n -> Native.hb_font_set_variations(n, oracleVariations pairs, uint32 pairs.Length)) |> Option.toList
                        let design = axes |> Array.map value
                        yield! check "design" (fun f -> f.SetVariationCoordsDesign(ReadOnlySpan design)) (fun n -> Native.hb_font_set_var_coords_design(n, design, uint32 design.Length)) |> Option.toList
                    let first = [ HarfBuzzSharp.Variation(Tag = tagOf axes[0], Value = axes[0].MaxValue); HarfBuzzSharp.Variation(Tag = 0x5A5A5A5Au, Value = 1.0f) ] |> Array.ofList
                    yield! check "one axis and an unknown tag" (fun f -> f.SetVariations(ReadOnlySpan first)) (fun n -> Native.hb_font_set_variations(n, oracleVariations [ (first[0].Tag, first[0].Value); (first[1].Tag, first[1].Value) ], 2u)) |> Option.toList
                    yield! check "short design" (fun f -> f.SetVariationCoordsDesign(ReadOnlySpan [| axes[0].MaxValue |])) (fun n -> Native.hb_font_set_var_coords_design(n, [| axes[0].MaxValue |], 1u)) |> Option.toList
                    let normalized = Array.init (axes.Length + 1) (fun index -> [| 16384; -16384; 0; 20000; -20000 |][index % 5])
                    yield! check "normalized" (fun f -> f.SetVariationCoordsNormalized(ReadOnlySpan normalized)) (fun n -> Native.hb_font_set_var_coords_normalized(n, normalized, uint32 normalized.Length)) |> Option.toList
                    for instance in 0 .. facade.NamedInstanceCount do
                        yield! check $"instance {instance}" (fun f -> f.SetVariationNamedInstance instance) (fun n -> Native.hb_font_set_var_named_instance(n, uint32 instance)) |> Option.toList
                        yield! check $"instance {instance} then a variation" (fun f -> f.SetVariationNamedInstance instance; f.SetVariations(ReadOnlySpan first)) (fun n -> Native.hb_font_set_var_named_instance(n, uint32 instance); Native.hb_font_set_variations(n, oracleVariations [ (first[0].Tag, first[0].Value); (first[1].Tag, first[1].Value) ], 2u)) |> Option.toList ]
            Expect.isEmpty (List.truncate 10 differences) (String.concat "\n" (List.truncate 10 differences)))
        testCase "every suite run that sets variations shapes through the facade as it does through the core" (fun () ->
            let requests =
                ShapeTests.suiteRequests ()
                |> List.filter (fun (_, request) -> ValueOption.isSome request.Variations && ValueOption.isNone (ShapeTests.unsupported request))
            printfn $"P10.S16: {requests.Length} suite runs with variations"
            Expect.isGreaterThan requests.Length 100 "runs with variations"
            let differences =
                requests
                |> List.choose (fun (name, request) ->
                    match Farfbuzz.Harness.FarfbuzzShaper.shaper.Shape request with
                    | Error _ -> None
                    | Ok Farfbuzz.Harness.Shaper.ShapingFailed -> None
                    | Ok (Farfbuzz.Harness.Shaper.Shaped core) ->
                        let infos, positions = ShapeTests.shapeThroughFacade FacadeFonts.faceOf request
                        match infos = core.Infos && positions = core.Positions with
                        | true -> None
                        | false -> Some name)
            Expect.isEmpty (List.truncate 10 differences) (String.concat "\n" (List.truncate 10 differences)))
        test "negative indices throw, a new font sits at the default coordinates, sub-fonts inherit them and a span takes what fits" {
            let variable = List.head (variableFaces ())
            let face = FacadeFonts.faceOf variable.Path variable.Index
            use font = new HarfBuzzSharp.Font(face)
            Expect.throwsT<ArgumentOutOfRangeException> (fun () -> font.SetVariationNamedInstance -1) "instance"
            Expect.throwsT<ArgumentOutOfRangeException> (fun () -> face.GetNamedInstanceDesignCoords -1 |> ignore) "design coords"
            Expect.throwsT<ArgumentOutOfRangeException> (fun () -> face.GetPaletteColors -1 |> ignore) "palette"
            Expect.throwsT<ArgumentOutOfRangeException> (fun () -> face.GetPaletteColorNameId -1 |> ignore) "colour name"
            Expect.isGreaterThan face.VariationAxisCount 0 "a variable face"
            Expect.equal font.VariationCoordsNormalized (Array.zeroCreate face.VariationAxisCount) "a new font sits at the defaults, as hb_font_create sets them"
            font.SetVariationCoordsNormalized(ReadOnlySpan (Array.create face.VariationAxisCount 8192))
            use sub = new HarfBuzzSharp.Font(font)
            Expect.equal sub.VariationCoordsNormalized (Array.create face.VariationAxisCount 8192) "the sub-font copies the parent's coordinates"
            let one = Array.zeroCreate<int> 1
            Expect.equal (font.GetVariationCoordsNormalized(Span one)) 1 "a span of one takes one"
            Expect.equal one [| 8192 |] "its value"
            let wide = Array.zeroCreate<int> (face.VariationAxisCount + 3)
            Expect.equal (font.GetVariationCoordsNormalized(Span wide)) face.VariationAxisCount "a wide span takes all"
        }
    ]
