module Farfbuzz.Tests.Glyph.FontOpsTests

open System
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.Allocation

[<Literal>]
let private CaseCount = 8000

/// <summary>The faces of the corpus a property draws from: their bytes and a default font, read once.</summary>
let private corpus =
    lazy (
        let upstream = RepositoryPaths.upstream ()
        let directories =
            Directory.EnumerateDirectories(Path.Combine(upstream, "test", "shape", "data"))
            |> Seq.map (fun suite -> Path.Combine(suite, "fonts"))
            |> Seq.append [ Path.Combine(upstream, "perf", "fonts"); Path.Combine(upstream, "test", "subset", "data", "fonts"); Path.Combine(upstream, "test", "api", "fonts") ]
        [| for directory in directories do
            match Directory.Exists directory with
            | false -> ()
            | true ->
                for path in Directory.EnumerateFiles directory |> Seq.sort do
                    match Path.GetExtension(path) with
                    | ".ttf" | ".otf" ->
                        let bytes = File.ReadAllBytes path
                        match bytes.Length < 2_000_000 with
                        | false -> ()
                        | true ->
                            let face = Face.create (ReadOnlyMemory bytes) 0u
                            match face.GlyphCount with
                            | 0 -> ()
                            | _ -> yield struct (path, Font.create face)
                    | _ -> () |])

/// <summary>How a corpus font is changed before a property reads it: scale, synthetic state, ppem and coordinates.</summary>
type private Shape =
    { XScale: int
      YScale: int
      Embolden: struct (float32 * float32 * bool) voption
      Slant: float32
      Ppem: uint32
      Coords: int list }

/// <summary>The generator of font changes for a font.</summary>
/// <param name="font">The font.</param>
/// <returns>The generator.</returns>
let private shapes (font: Font) =
    gen {
        let upem = font.Face.Upem
        let! xScale = Gen.frequency [ 4, Gen.constant upem; 2, Gen.choose (1, 4 * upem); 1, Gen.choose (-upem, -1) ]
        let! yScale = Gen.frequency [ 4, Gen.constant xScale; 2, Gen.choose (1, 4 * upem); 1, Gen.choose (-upem, -1) ]
        let! embolden = Gen.frequency [ 6, Gen.constant ValueNone; 1, Gen.map2 (fun strength inPlace -> ValueSome (struct (strength, strength, inPlace))) (Gen.elements [| 0.02f; 0.05f; 0.2f |]) (Gen.elements [| true; false |]) ]
        let! slant = Gen.elements [| 0f; 0f; 0f; 0.2f; -0.1f |]
        let! ppem = Gen.elements [| 0u; 0u; 12u; 20u; 40u; 96u |]
        let axes = font.Ot.Variations.Value.Fvar.Axes.Length
        let! coords = Gen.frequency [ 3, Gen.constant []; 4, Gen.listOfLength axes (Gen.frequency [ 1, Gen.constant -16384; 1, Gen.constant 16384; 1, Gen.constant 0; 5, Gen.choose (-16384, 16384) ]) ]
        return { XScale = xScale; YScale = yScale; Embolden = embolden; Slant = slant; Ppem = ppem; Coords = coords }
    }

/// <summary>Applies a change to a font.</summary>
/// <param name="shape">The change.</param>
/// <param name="font">The font.</param>
/// <returns>The changed font.</returns>
let private shaped (shape: Shape) (font: Font) =
    let font = Font.withScale shape.XScale shape.YScale font
    let font =
        match shape.Embolden with
        | ValueSome (struct (x, y, inPlace)) -> Font.withSyntheticBold x y inPlace font
        | ValueNone -> font
    let font = Font.withSyntheticSlant shape.Slant font
    let font = Font.withPpem shape.Ppem shape.Ppem font
    match shape.Coords with
    | [] -> font
    | coords -> Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) font

/// <summary>A case: a corpus font, a change and glyphs, past the glyph count and absurdly high ones included.</summary>
let private cases =
    gen {
        let fonts = corpus.Value
        let! pick = Gen.choose (0, fonts.Length - 1)
        let struct (_, font) = fonts[pick]
        let! shape = shapes font
        let! count = Gen.frequency [ 1, Gen.constant 0; 6, Gen.choose (1, 12); 1, Gen.choose (13, 80) ]
        let top = font.Face.GlyphCount
        let! glyphs = Gen.listOfLength count (Gen.frequency [ 12, Gen.choose (0, top - 1); 1, Gen.choose (top, top + 5); 1, Gen.constant -1 ])
        return struct (pick, shape, glyphs |> List.map uint32 |> List.toArray)
    }

/// <summary>The font of a case, changed.</summary>
/// <param name="pick">The corpus index.</param>
/// <param name="shape">The change.</param>
/// <returns>The font.</returns>
let private fontOf pick shape =
    let struct (_, font) = corpus.Value[pick]
    shaped shape font

/// <summary>Whether the planned glyph extents equal the looked-up ones, and so the not-ported source and the extents value.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>True when they agree.</returns>
let private extentsAgree (font: Font) (glyph: uint32) =
    Extents.glyph font glyph = Extents.glyphReference font glyph
    && Extents.value font glyph = Extents.valueReference font glyph
    && Extents.source font = Extents.sourceReference font

/// <summary>Whether the horizontal advances written into arrays of the caller equal the allocating ones: into an array of exactly the length,
/// into a longer one (the elements after the advances untouched) and into a shorter one (nothing written, the length needed returned).</summary>
/// <param name="font">The font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>True when they agree.</returns>
let private hIntoAgrees (font: Font) (glyphs: uint32 array) =
    let count = glyphs.Length
    let expected = (Font.hAdvances font (ReadOnlySpan glyphs) |> TailCalls.settle).AsSpan().ToArray()
    let exact = Array.create count -1
    let longer = Array.create (count + 3) -7
    let shorter = Array.create (max 0 (count - 1)) -5
    let exactCount = Font.hAdvancesInto font (ReadOnlySpan glyphs) (Span exact) |> TailCalls.settle
    let longerCount = Font.hAdvancesInto font (ReadOnlySpan glyphs) (Span longer) |> TailCalls.settle
    let shorterCount = Font.hAdvancesInto font (ReadOnlySpan glyphs) (Span shorter) |> TailCalls.settle
    exactCount = count && exact = expected
    && longerCount = count && longer[0 .. count - 1] = expected && longer[count ..] = [| -7; -7; -7 |]
    && shorterCount = count && (count = 0 || Array.forall (fun value -> value = -5) shorter)

/// <summary>The same for the vertical advances.</summary>
/// <param name="font">The font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>True when they agree.</returns>
let private vIntoAgrees (font: Font) (glyphs: uint32 array) =
    let count = glyphs.Length
    let expected = (Font.vAdvances font (ReadOnlySpan glyphs) |> TailCalls.settle).AsSpan().ToArray()
    let exact = Array.create count -1
    let longer = Array.create (count + 3) -7
    let shorter = Array.create (max 0 (count - 1)) -5
    let exactCount = Font.vAdvancesInto font (ReadOnlySpan glyphs) (Span exact) |> TailCalls.settle
    let longerCount = Font.vAdvancesInto font (ReadOnlySpan glyphs) (Span longer) |> TailCalls.settle
    let shorterCount = Font.vAdvancesInto font (ReadOnlySpan glyphs) (Span shorter) |> TailCalls.settle
    exactCount = count && exact = expected
    && longerCount = count && longer[0 .. count - 1] = expected && longer[count ..] = [| -7; -7; -7 |]
    && shorterCount = count && (count = 0 || Array.forall (fun value -> value = -5) shorter)

/// <summary>The same for the advances and the origins of an array of glyphs.</summary>
/// <param name="font">The font.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>True when they agree.</returns>
let private metricsAgree (font: Font) (glyphs: uint32 array) =
    let horizontal = Font.hAdvances font (ReadOnlySpan glyphs) |> TailCalls.settle
    let horizontalReference = Font.hAdvancesReference font (ReadOnlySpan glyphs) |> TailCalls.settle
    let vertical = Font.vAdvances font (ReadOnlySpan glyphs) |> TailCalls.settle
    let verticalReference = Font.vAdvancesReference font (ReadOnlySpan glyphs) |> TailCalls.settle
    let origins = Origins.vOrigins font (ReadOnlySpan glyphs) |> TailCalls.settle
    let originsOne = glyphs |> Array.map (fun glyph -> Origins.vOrigin font glyph)
    let originsAgree =
        match origins with
        | Error error -> originsOne |> Array.forall (fun one -> one = Error error)
        | Ok many -> originsOne |> Array.mapi (fun index one -> one = Ok many[index]) |> Array.forall id
    horizontal.AsSpan().SequenceEqual(horizontalReference.AsSpan())
    && vertical.AsSpan().SequenceEqual(verticalReference.AsSpan())
    && originsAgree
    && (glyphs |> Array.forall (fun glyph -> (Font.hAdvances font (ReadOnlySpan [| glyph |]) |> TailCalls.settle)[0] = Font.hAdvance font glyph))
    && (glyphs |> Array.forall (fun glyph -> Font.hAdvance font glyph = Font.hAdvanceReference font glyph && Font.hAdvance font glyph = Font.hAdvanceReference font glyph))
    && (glyphs |> Array.forall (fun glyph -> Font.vAdvance font glyph = Font.vAdvanceReference font glyph && Font.vAdvance font glyph = Font.vAdvanceReference font glyph))
    && (glyphs |> Array.forall (fun glyph -> Origins.vOrigin font glyph = Origins.vOriginReference font glyph && Origins.vOrigin font glyph = Origins.vOriginReference font glyph))
    && hIntoAgrees font glyphs
    && vIntoAgrees font glyphs

/// <summary>The glyph-operation properties: the planned and batched paths answer exactly as the looked-up ones.</summary>
[<Tests>]
let properties =
    testList "Font operations" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "extents: the planned path equals the table-by-table path, at random scales, synthetic state, ppem and coordinates" <|
            Prop.forAll (Arb.fromGen cases) (fun (struct (pick, shape, glyphs)) ->
                let font = fontOf pick shape
                glyphs |> Array.forall (extentsAgree font))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "advances and origins: the batched paths equal glyph by glyph, at random scales, synthetic state and coordinates" <|
            Prop.forAll (Arb.fromGen cases) (fun (struct (pick, shape, glyphs)) -> metricsAgree (fontOf pick shape) glyphs)
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "advances into arrays of the caller: equal to the allocating batch for a font with replaced functions (a sub-font), at random scales" <|
            Prop.forAll (Arb.fromGen cases) (fun (struct (pick, shape, glyphs)) ->
                let font = fontOf pick shape
                let sub = FontFuncsOf.subFont font (font.XScale * 2) (font.YScale / 2 + 1)
                hIntoAgrees sub glyphs && vIntoAgrees sub glyphs)
        test "every glyph of every small corpus font: extents, advances and origins agree at the default font" {
            let mutable glyphsChecked = 0
            for struct (path, font) in corpus.Value do
                match font.Face.GlyphCount <= 3000 with
                | false -> ()
                | true ->
                    let glyphs = Array.init (font.Face.GlyphCount + 3) uint32
                    Expect.isTrue (glyphs |> Array.forall (extentsAgree font)) $"extents of {path}"
                    Expect.isTrue (metricsAgree font glyphs) $"metrics of {path}"
                    glyphsChecked <- glyphsChecked + glyphs.Length
            Expect.isGreaterThan glyphsChecked 50000 "the corpus has glyphs"
        }
    ]

/// <summary>The font that a variable corpus font becomes at some coordinates, for the kernel properties.</summary>
let private variableFonts =
    lazy (
        corpus.Value
        |> Array.filter (fun (struct (_, font)) -> font.Ot.Gvar.Value.GlyphCount > 0 && font.Ot.Glyf.Value.Present && font.Ot.Variations.Value.Fvar.Axes.Length > 0)
        |> Array.map (fun (struct (path, font)) -> struct (path, font)))

/// <summary>The coordinates of a font with a given number of axes, each at an extreme, at zero or anywhere between.</summary>
/// <param name="axes">The axis count.</param>
/// <returns>The generator.</returns>
let private coordinates axes =
    Gen.listOfLength axes (Gen.frequency [ 1, Gen.constant -16384; 1, Gen.constant 16384; 1, Gen.constant 0; 6, Gen.choose (-16384, 16384) ])

/// <summary>The glyf kernel against the reference: the unscaled extents of a glyph at coordinates, and how many of the glyphs it answered.</summary>
/// <param name="font">The font at its coordinates.</param>
/// <param name="glyphs">The glyphs.</param>
/// <returns>Whether every glyph agrees, and the number of glyphs the kernel answered.</returns>
let private kernelAgrees (font: Font) (glyphs: uint32 seq) =
    let glyf = font.Ot.GlyfPoints.Value
    let gvar = font.Ot.Gvar.Value
    let coords = font.Variations.Coords
    let scalars = GlyphOutline.sharedScalars gvar coords
    let mutable answeredCount = 0
    let mutable agree = true
    for glyph in glyphs do
        let struct (answered, kernel) = GlyphOutline.kernelExtents glyf gvar coords scalars glyph
        let reference = GlyphOutline.extentsWithVariations glyf gvar coords glyph
        match answered with
        | false -> ()
        | true ->
            answeredCount <- answeredCount + 1
            agree <- agree && kernel = reference
    struct (agree, answeredCount)

/// <summary>The glyf kernel properties and the cmap pages.</summary>
[<Tests>]
let kernels =
    testList "Font operations kernels" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1500 } "glyf kernel: the unscaled extents of every glyph of a variable font at random coordinates equal the reference" <|
            Prop.forAll (Arb.fromGen (gen {
                let fonts = variableFonts.Value
                let! pick = Gen.choose (0, fonts.Length - 1)
                let struct (_, font) = fonts[pick]
                let! coords = coordinates font.Ot.Variations.Value.Fvar.Axes.Length
                return struct (pick, coords)
            })) (fun (struct (pick, coords)) ->
                let struct (_, font) = variableFonts.Value[pick]
                let varied = Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) font
                let struct (agree, _) = kernelAgrees varied (seq { 0u .. uint32 (min (varied.Face.GlyphCount + 2) 400) })
                agree)
        test "glyf kernel: it answers every glyph of the variable fonts of the corpus, composites included" {
            let mutable total = 0
            let mutable answeredTotal = 0
            for struct (path, font) in variableFonts.Value do
                let axes = font.Ot.Variations.Value.Fvar.Axes.Length
                for coords in [ List.replicate axes 8192; List.init axes (fun axis -> match axis % 2 with | 0 -> 16384 | _ -> -9000); List.replicate axes -16384 ] do
                    let varied = Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) font
                    let glyphs = seq { 0u .. uint32 (min (varied.Face.GlyphCount - 1) 3000) }
                    let struct (agree, answered) = kernelAgrees varied glyphs
                    Expect.isTrue agree $"kernel and reference agree on {path}"
                    total <- total + (min varied.Face.GlyphCount 3001)
                    answeredTotal <- answeredTotal + answered
            Expect.isGreaterThan total 1000 "there are glyphs"
            Expect.equal answeredTotal total "the kernel answers every glyph of the corpus fonts"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 600 } "a font changed after it was used answers for its new state, not from what it held before" <|
            Prop.forAll (Arb.fromGen (gen {
                let fonts = variableFonts.Value
                let! pick = Gen.choose (0, fonts.Length - 1)
                let struct (_, font) = fonts[pick]
                let axes = font.Ot.Variations.Value.Fvar.Axes.Length
                let! first = coordinates axes
                let! second = coordinates axes
                let! shape = shapes font
                let! glyphs = Gen.listOfLength 12 (Gen.choose (0, font.Face.GlyphCount + 1))
                return struct (pick, first, second, shape, glyphs |> List.map uint32 |> List.toArray)
            })) (fun (struct (pick, first, second, shape, glyphs)) ->
                let struct (_, font) = variableFonts.Value[pick]
                let used = shaped shape (Font.setVarCoordsNormalized (ImmutableArray.CreateRange first) font)
                glyphs |> Array.iter (fun glyph ->
                    Font.hAdvance used glyph |> ignore
                    Font.vAdvance used glyph |> ignore
                    Origins.vOrigin used glyph |> ignore
                    Extents.glyph used glyph |> ignore)
                let changed = Font.setVarCoordsNormalized (ImmutableArray.CreateRange second) used
                let rescaled = Font.withScale (used.XScale + 7) (used.YScale + 3) used
                let embolden = Font.withSyntheticBold 0.05f 0.05f false used
                [ changed; rescaled; embolden ]
                |> List.forall (fun other ->
                    glyphs |> Array.forall (fun glyph ->
                        Font.hAdvance other glyph = Font.hAdvanceReference other glyph
                        && Font.vAdvance other glyph = Font.vAdvanceReference other glyph
                        && Origins.vOrigin other glyph = Origins.vOriginReference other glyph
                        && Extents.glyph other glyph = Extents.glyphReference other glyph)))
        test "threads that race for the first requests of one font get the answers of the reference" {
            let upstream = RepositoryPaths.upstream ()
            for relative, coords in [ "test/subset/data/fonts/RobotoFlex-Variable.ttf", true; "test/subset/data/fonts/Roboto-Regular.ttf", false; "perf/fonts/NotoSansDuployan-Regular.otf", false ] do
                let bytes = File.ReadAllBytes(Path.Combine(upstream, relative))
                for round in 1 .. 4 do
                    let made = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let font =
                        match coords with
                        | true -> Font.setVariation (Tag.make 'w' 'g' 'h' 't') 640f (Font.withScale 700 800 made)
                        | false -> Font.withScale 700 800 made
                    let expected = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let expected = match coords with | true -> Font.setVariation (Tag.make 'w' 'g' 'h' 't') 640f (Font.withScale 700 800 expected) | false -> Font.withScale 700 800 expected
                    let glyphs = Array.init (min font.Face.GlyphCount 1500) uint32
                    let unicodes = [| for range in Cmap.collectUnicodes font.Ot.Cmap.Value font.Face.GlyphCount do for codepoint in range.First .. min range.Last (range.First + 600u) -> codepoint |]
                    let failures = System.Collections.Concurrent.ConcurrentBag<string>()
                    System.Threading.Tasks.Parallel.For(0, 12, fun thread ->
                        for index in 0 .. glyphs.Length - 1 do
                            let glyph = glyphs[(index * 7 + thread * 131) % glyphs.Length]
                            match Font.hAdvance font glyph = Font.hAdvanceReference expected glyph && Font.vAdvance font glyph = Font.vAdvanceReference expected glyph with
                            | true -> ()
                            | false -> failures.Add $"advance of {glyph} in {relative}"
                            match Origins.vOrigin font glyph = Origins.vOriginReference expected glyph && Extents.glyph font glyph = Extents.glyphReference expected glyph with
                            | true -> ()
                            | false -> failures.Add $"origin or extents of {glyph} in {relative}"
                        for index in 0 .. unicodes.Length - 1 do
                            let codepoint = unicodes[(index * 13 + thread * 97) % unicodes.Length]
                            match Font.nominalGlyph font codepoint = Cmap.nominalGlyphGeneral font.Ot.Cmap.Value.Table font.Ot.Cmap.Value.Best font.Ot.Cmap.Value.Remap codepoint with
                            | true -> ()
                            | false -> failures.Add $"nominal glyph of U+{codepoint:X} in {relative}") |> ignore
                    Expect.isEmpty failures $"{relative}, round {round}: no thread read a wrong answer"
        }
        test "nominal glyphs above U+20FF equal the subtable lookup, the first time and from the pages" {
            for struct (path, font) in corpus.Value do
                let cmap = font.Ot.Cmap.Value
                let ranges = Cmap.collectUnicodes cmap font.Face.GlyphCount
                let mapped = [| for range in ranges do for codepoint in range.First .. min range.Last (range.First + 400u) -> codepoint |]
                let probes = Array.append mapped [| 0x2100u; 0x20FFu; 0x2101u; 0x10FFFFu; 0x110000u; 0xFFFFFFFFu; 0x1F600u; 0x4E00u |]
                for round in 1 .. 2 do
                    for codepoint in probes do
                        Expect.equal (Font.nominalGlyph font codepoint) (Cmap.nominalGlyphGeneral cmap.Table cmap.Best cmap.Remap codepoint) $"{path}: U+{codepoint:X} on round {round}"
        }
    ]

/// <summary>The warm font operations allocate nothing.</summary>
[<Tests>]
let allocations =
    testList "Font operations allocation" [
        test "extents, advances and origins of one glyph allocate no bytes on warm fonts" {
            let upstream = RepositoryPaths.upstream ()
            for relative in [ "test/subset/data/fonts/Roboto-Regular.ttf"; "test/subset/data/fonts/SourceSansPro-Regular.otf"; "test/subset/data/fonts/AdobeVFPrototype.otf"; "test/subset/data/fonts/SourceSerifVariable-Roman.ttf" ] do
                let bytes = File.ReadAllBytes(Path.Combine(upstream, relative))
                let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
                let allocated =
                    bytesAllocatedAtBest (fun () ->
                        for glyph in 1u .. 40u do
                            Extents.glyph font glyph |> ignore
                            Font.hAdvance font glyph |> ignore
                            Font.nominalGlyph font (0x40u + glyph) |> ignore)
                Expect.equal allocated 0L $"{relative}: extents, advances and nominal glyphs allocate nothing"
        }
        test "a batch of advances allocates its result and nothing else" {
            let upstream = RepositoryPaths.upstream ()
            let bytes = File.ReadAllBytes(Path.Combine(upstream, "test/subset/data/fonts/Roboto-Regular.ttf"))
            let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
            let glyphs = Array.init 500 uint32
            let allocated = bytesAllocatedAtBest (fun () -> Font.hAdvances font (ReadOnlySpan glyphs) |> TailCalls.settle |> ignore)
            Expect.isLessThanOrEqual allocated (500L * 4L + 40L) "the advances array and its header"
        }
    ]

/// <summary>Loading a face builds what the first query needs and nothing more.</summary>
[<Tests>]
let loading =
    testList "Face loading" [
        test "a font loads no table when it is made, and shares its accelerators with every font of the face" {
            let upstream = RepositoryPaths.upstream ()
            let bytes = File.ReadAllBytes(Path.Combine(upstream, "test/subset/data/fonts/RobotoFlex-Variable.ttf"))
            let face = Face.create (ReadOnlyMemory bytes) 0u
            let first = Font.create face
            let second = Font.withScale 500 500 (Font.create face)
            Expect.isTrue (obj.ReferenceEquals(first.Ot, second.Ot)) "fonts of one face share their accelerators"
            Expect.isFalse first.Ot.Cmap.IsValueCreated "the cmap is not loaded by Font.create"
            Expect.isFalse first.Ot.Gsub.IsValueCreated "GSUB is not loaded by Font.create"
            Expect.isFalse first.Ot.Gdef.IsValueCreated "GDEF is not loaded by Font.create"
            Expect.isFalse first.Ot.Variations.Value.Gdef.IsValueCreated "the GDEF variation store is not loaded without coordinates"
            Font.nominalGlyph first 0x41u |> ignore
            Expect.isTrue first.Ot.Cmap.IsValueCreated "a lookup loads the cmap"
            Expect.isFalse first.Ot.Cmap.Value.Low.IsValueCreated "a Latin lookup does not build the low table"
            Font.nominalGlyph first 0x3B1u |> ignore
            Expect.isTrue first.Ot.Cmap.Value.Low.IsValueCreated "a lookup above U+00FF builds the low table"
            Expect.isFalse first.Ot.Cmap.Value.Index.IsValueCreated "a lookup below U+2100 does not build the index"
            let varied = Font.setVariation (Tag.make 'w' 'g' 'h' 't') 700f first
            Expect.isTrue varied.Ot.Variations.Value.Gdef.IsValueCreated "coordinates load the GDEF variation store"
        }
        test "a face with another glyph count has its own accelerators, and faces compare by their tables" {
            let upstream = RepositoryPaths.upstream ()
            let bytes = File.ReadAllBytes(Path.Combine(upstream, "test/subset/data/fonts/Roboto-Regular.ttf"))
            let one = Face.create (ReadOnlyMemory bytes) 0u
            let two = Face.create (ReadOnlyMemory bytes) 0u
            let fewer = Face.withGlyphCount 3 one
            Expect.equal one two "two faces of the same file are equal"
            Expect.isFalse (obj.ReferenceEquals((Font.create one).Ot, (Font.create fewer).Ot)) "another glyph count rebuilds the accelerators"
        }
    ]

