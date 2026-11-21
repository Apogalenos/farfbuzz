module Farfbuzz.Tests.Shape.PlanKeyTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.UpstreamFiles

/// <summary>One pre-built font and the normalized coordinates its key records.</summary>
type private FontCase =
    { Font: Font
      Coords: int array }

/// <summary>The non-variable Roboto and two varied instances of the variable Roboto.</summary>
/// <returns>The font cases.</returns>
let private fontCases () : FontCase array =
    let roboto = Font.create (Face.create (read "test/api/fonts/Roboto-Regular.abc.ttf") 0u)
    let variable = Font.create (Face.create (read "test/subset/data/fonts/Roboto-Variable.ttf") 0u)
    let varied (weight: float32) =
        let font = Font.setVariation (Tag.ofString "wght") weight variable
        let coords = font.Variations.Coords
        { Font = font; Coords = coords.AsSpan().ToArray() }
    [| { Font = roboto; Coords = Array.empty }
       varied 300f
       varied 700f |]

/// <summary>The segment properties of a default-shaper Latin run.</summary>
let private latin =
    { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.NoLanguage }

/// <summary>A global kern feature.</summary>
let private kern =
    { Tag = Tag.ofString "kern"; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }

/// <summary>A kern feature over a range.</summary>
/// <param name="start">The first cluster.</param>
/// <param name="finish">The cluster after the last.</param>
/// <returns>The ranged feature.</returns>
let private kernRanged (start: uint32) (finish: uint32) =
    { kern with Start = start; End = finish }

/// <summary>The static Roboto face, which has no FeatureVariations.</summary>
let private roboto =
    Font.create (Face.create (read "test/api/fonts/Roboto-Regular.abc.ttf") 0u)

/// <summary>The face with FeatureVariations whose coordinate-derived key the upstream cache test pins
/// (<c>test/api/test-shape-plan.c:test_ot_shape_plan_lookup_variations_cache</c>).</summary>
/// <returns>The face.</returns>
let private featureVariationFace () : Face =
    Face.create (read "test/shape/data/in-house/fonts/4e9f0bc6a8f25b5fd3547bbc17423ce8cedb915f.ttf") 0u

/// <summary>The feature-variation font at one normalized TEST-axis coordinate.</summary>
/// <param name="coord">The 2.14 normalized coordinate.</param>
/// <returns>The varied font.</returns>
let private variedFeatureFont (coord: int) : Font =
    Font.setVarCoordsNormalized (ImmutableArray.CreateRange [| coord |]) (Font.create (featureVariationFace ()))

/// <summary>Builds a plan key from a font and a feature list, requiring the key to build.</summary>
/// <param name="font">The font.</param>
/// <param name="features">The features.</param>
/// <returns>The key.</returns>
let private keyOf (font: Font) (features: Feature array) : ShapePlanKey =
    match ShapePlan.key font latin (ReadOnlySpan features) "ot" with
    | ValueSome key -> key
    | ValueNone -> failwith "plan key could not be built"

/// <summary>Maps a shaping result to the glyphs, clusters, positions and glyph flags it produced.</summary>
/// <param name="result">The shaping result.</param>
/// <returns>The comparable tuples, empty when shaping failed.</returns>
let private comparable (result: Result<GlyphRun, ShapeError>) : struct (uint32 * uint32 * int * int * int * int * uint32) array =
    match result with
    | Error _ -> [||]
    | Ok run ->
        Array.init run.Infos.Length (fun index ->
            let info = run.Infos[index]
            let position = run.Positions[index]
            struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined))

/// <summary>The feature tags the property draws from.</summary>
let private featureTags = [| "kern"; "liga"; "smcp"; "dlig"; "calt"; "clig"; "frac"; "onum" |]

/// <summary>One random feature, global or ranged.</summary>
let private featureGen : Gen<Feature> =
    gen {
        let! tag = Gen.elements featureTags
        let! value = Gen.elements [| 0u; 1u; 2u |]
        let! kind = Gen.elements [| 0; 1; 2 |]
        match kind with
        | 0 -> return { Tag = Tag.ofString tag; Value = value; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
        | 1 ->
            let! start = Gen.choose (0, 8)
            return { Tag = Tag.ofString tag; Value = value; Start = uint32 start; End = Feature.GlobalEnd }
        | _ ->
            let! start = Gen.choose (0, 8)
            let! length = Gen.choose (1, 8)
            return { Tag = Tag.ofString tag; Value = value; Start = uint32 start; End = uint32 (start + length) }
    }

/// <summary>The default-shaper code points the property draws from.</summary>
let private codepoints = [| 0x20u; 0x41u; 0x61u; 0x42u; 0x62u; 0x43u; 0x63u; 0x65u |]

/// <summary>One property input: a font, segment properties, user features and a text run.</summary>
let private inputGen : Gen<struct (FontCase * SegmentProperties * Feature array * uint32 array)> =
    gen {
        let! fontCase = Gen.elements (fontCases ())
        let! direction = Gen.elements [| Direction.Ltr; Direction.Rtl; Direction.Ttb; Direction.Btt |]
        let! script = Gen.elements [| Scripts.Latin; Scripts.Greek; Scripts.Cyrillic |]
        let! language = Gen.elements [| Language.NoLanguage; Language.Bcp47 "en"; Language.Bcp47 "fr" |]
        let! count = Gen.choose (0, 4)
        let! features = Gen.arrayOfLength count featureGen
        let! length = Gen.choose (1, 16)
        let! text = Gen.arrayOfLength length (Gen.elements codepoints)
        return struct (fontCase, { Direction = direction; Script = script; Language = language }, features, text)
    }

/// <summary>Shapes one input through the plan-reuse pair and the one-call entry, comparing their outputs.</summary>
/// <param name="input">The font, properties, features and text.</param>
/// <returns>True when both paths produce the same glyphs, clusters, positions and flags.</returns>
let private reuseEqualsFresh (input: struct (FontCase * SegmentProperties * Feature array * uint32 array)) : bool =
    let struct (fontCase, props, features, text) = input
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
        |> GlyphRun.withSegment props
        |> TailCalls.settle
    let featuresArray = ImmutableArray.CreateRange features
    let shapers = ValueSome (ImmutableArray.Create "ot")
    let reused =
        match ShapePlan.create fontCase.Font props featuresArray shapers with
        | Error error -> Error error
        | Ok plan -> Shape.shapeWithPlan fontCase.Font plan featuresArray run
    let fresh = Shape.shapeFull fontCase.Font featuresArray shapers run
    comparable reused = comparable fresh

/// <summary>The P9.S07 plan-key tests and the plan-reuse property.</summary>
[<Tests>]
let tests =
    testList "P9.S07" [
        testCase "equal inputs give equal keys" (fun () ->
            let a = keyOf roboto [| kern |]
            let b = keyOf roboto [| kern |]
            Expect.equal a b "same inputs")
        testCase "features differing only by range give equal keys" (fun () ->
            let a = keyOf roboto [| kernRanged 0u 5u |]
            let b = keyOf roboto [| kernRanged 5u 9u |]
            Expect.equal a b "ranges are ignored")
        testCase "a global and a ranged feature give different keys" (fun () ->
            let a = keyOf roboto [| kern |]
            let b = keyOf roboto [| kernRanged 0u 5u |]
            Expect.notEqual a b "global-ness differs")
        testCase "different feature values give different keys" (fun () ->
            let a = keyOf roboto [| kern |]
            let b = keyOf roboto [| { kern with Value = 0u } |]
            Expect.notEqual a b "value differs")
        testCase "different shapers give different keys" (fun () ->
            let a = keyOf roboto [||]
            let b =
                match ShapePlan.key roboto latin ReadOnlySpan.Empty "fallback" with
                | ValueSome key -> key
                | ValueNone -> failwith "plan key could not be built"
            Expect.notEqual a b "shapers differ")
        testCase "equal non-zero coordinates give equal keys and equal hashes" (fun () ->
            let a = keyOf (variedFeatureFont 4915) [||]
            let b = keyOf (variedFeatureFont 4915) [||]
            Expect.equal a b "same coordinates"
            Expect.equal (a.GetHashCode()) (b.GetHashCode()) "same hash")
        testCase "coordinates with the same lookup set give equal keys" (fun () ->
            let a = keyOf (variedFeatureFont 4915) [||]
            let b = keyOf (variedFeatureFont 6554) [||]
            Expect.equal a b "0.3 and 0.4 select the same lookup set")
        testCase "different coordinates give different keys" (fun () ->
            let a = keyOf (variedFeatureFont 0) [||]
            let b = keyOf (variedFeatureFont 4915) [||]
            Expect.notEqual a b "0 and 0.3 differ")
        testCase "coordinates selecting different records give different keys" (fun () ->
            let a = keyOf (variedFeatureFont 9830) [||]
            let b = keyOf (variedFeatureFont 13107) [||]
            Expect.notEqual a b "0.6 and 0.8 differ")
        testCase "a face without FeatureVariations gives equal keys at any coordinates" (fun () ->
            let variable = Font.create (Face.create (read "test/subset/data/fonts/Roboto-Variable.ttf") 0u)
            let a = keyOf (Font.setVarCoordsNormalized (ImmutableArray.CreateRange [| -100 |]) variable) [||]
            let b = keyOf (Font.setVarCoordsNormalized (ImmutableArray.CreateRange [| 200 |]) variable) [||]
            Expect.equal a b "coordinates do not enter the key")
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "P9.S07 shaping with a reused plan equals a fresh plan"
            (Prop.forAll (Arb.fromGen inputGen) reuseEqualsFresh)
    ]

/// <summary>The line ranges of a text, split on <c>\n</c> as benchmark-shape.cc does.</summary>
/// <param name="text">The UTF-8 text.</param>
/// <returns>The start and length of each <c>\n</c>-terminated line.</returns>
let private lineRanges (text: byte array) : struct (int * int) array =
    let ranges = ResizeArray()
    let mutable start = 0
    for index in 0 .. text.Length - 1 do
        match text[index] with
        | 10uy ->
            ranges.Add (struct (start, index - start))
            start <- index + 1
        | _ -> ()
    ranges.ToArray()

/// <summary>Whether shaping every line of a text through one cache equals <c>Shape.shapeFull</c> line for line.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The UTF-8 text.</param>
/// <returns>True when the cached and fresh paths agree on every line.</returns>
let private cachedTextEqualsFresh (font: Font) (text: byte array) : bool =
    let mutable cache = PlanCache.empty
    let mutable equal = true
    for struct (start, length) in lineRanges text do
        let run =
            GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
            |> GlyphRun.guessSegmentProperties Language.cLocale
            |> TailCalls.settle
        let features = ImmutableArray.Empty
        let struct (cached, next) = Shape.shapeCached cache font features ValueNone run
        cache <- next
        let fresh = Shape.shapeFull font features ValueNone run
        match comparable cached = comparable fresh with
        | true -> ()
        | false -> equal <- false
    equal

/// <summary>Shapes one input through the plan cache and the one-call entry, comparing their outputs.</summary>
/// <param name="input">The font, properties, features and text.</param>
/// <returns>True when both paths produce the same glyphs, clusters, positions and flags.</returns>
let private cacheEqualsFresh (input: struct (FontCase * SegmentProperties * Feature array * uint32 array)) : bool =
    let struct (fontCase, props, features, text) = input
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
        |> GlyphRun.withSegment props
        |> TailCalls.settle
    let featuresArray = ImmutableArray.CreateRange features
    let shapers = ValueSome (ImmutableArray.Create "ot")
    let struct (cached, _) = Shape.shapeCached PlanCache.empty fontCase.Font featuresArray shapers run
    let fresh = Shape.shapeFull fontCase.Font featuresArray shapers run
    comparable cached = comparable fresh

/// <summary>The PERF6 plan-cache tests: the cache equals a fresh plan, and a hit returns the same cache value.</summary>
[<Tests>]
let cacheTests =
    testList "PERF6" [
        testPropertyWithConfig
            { FsCheckConfig.defaultConfig with maxTest = 5000; endSize = 64 }
            "shaping with the plan cache equals a fresh plan"
            (Prop.forAll (Arb.fromGen inputGen) cacheEqualsFresh)

        testCase "the plan cache equals a fresh plan for every line of both Roboto texts" (fun () ->
            let font = Font.create (Face.create (read "perf/fonts/Roboto-Regular.ttf") 0u)
            let littlePrince = (read "perf/texts/en-thelittleprince.txt").ToArray()
            let words = (read "perf/texts/en-words.txt").ToArray()
            Expect.isTrue (cachedTextEqualsFresh font littlePrince) "en-thelittleprince"
            Expect.isTrue (cachedTextEqualsFresh font words) "en-words")

        testCase "the plan cache gains one entry per distinct key and a hit adds none" (fun () ->
            let fontCase = (fontCases ())[0]
            let features = ImmutableArray.Empty
            let shapers = ValueSome (ImmutableArray.Create "ot")
            let runOf props =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x42u |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withSegment props
                |> TailCalls.settle
            let struct (_, c1) = Shape.shapeCached PlanCache.empty fontCase.Font features shapers (runOf latin)
            Expect.equal c1.Entries.Length 1 "the first shape caches one plan"
            let struct (_, c2) = Shape.shapeCached c1 fontCase.Font features shapers (runOf latin)
            Expect.equal c2.Entries.Length 1 "a hit adds no entry"
            let rtl = { Direction = Direction.Rtl; Script = Scripts.Latin; Language = Language.NoLanguage }
            let struct (_, c3) = Shape.shapeCached c2 fontCase.Font features shapers (runOf rtl)
            Expect.equal c3.Entries.Length 2 "a distinct key adds one entry")
    ]
