module Farfbuzz.Tests.AatApiTests

open System
open System.Collections.Immutable
open System.Diagnostics
open Expecto
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.Harness
open Farfbuzz.OpenType
open Farfbuzz.Tests.SfntBuilder

/// <summary>The feature type that makes the design-complexity test's selectors, <c>HB_AAT_LAYOUT_FEATURE_TYPE_DESIGN_COMPLEXITY_TYPE</c>.</summary>
[<Literal>]
let private DesignComplexityType = 18us

/// <summary>The feature type of the typographic extras, <c>HB_AAT_LAYOUT_FEATURE_TYPE_TYPOGRAPHIC_EXTRAS</c>.</summary>
[<Literal>]
let private TypographicExtras = 14us

/// <summary>The invalid feature type, <c>HB_AAT_LAYOUT_FEATURE_TYPE_INVALID</c>.</summary>
[<Literal>]
let private FeatureTypeInvalid = 0xFFFFus

/// <summary>The most subtables a kern or morx table may hold, <c>HB_AAT_MAX_SUBTABLES</c>.</summary>
[<Literal>]
let private MaxSubtables = 1024

/// <summary>The size of one format 3 kern subtable of the limit test, in bytes.</summary>
[<Literal>]
let private KernSubtableLength = 14

/// <summary>The size of one noncontextual morx subtable of the limit test, in bytes.</summary>
[<Literal>]
let private MorxSubtableLength = 20

/// <summary>The number of noncontextual subtables in each chain of the morx limit test.</summary>
[<Literal>]
let private SubtablesPerChain = 5

/// <summary>The size of one morx chain of the limit test, in bytes.</summary>
[<Literal>]
let private MorxChainLength = 16 + SubtablesPerChain * MorxSubtableLength

/// <summary>The maxp table of the two-glyph limit font: version 0.5 and two glyphs.</summary>
let private limitMaxp = [| 0x00uy; 0x00uy; 0x50uy; 0x00uy; 0x00uy; 0x02uy |]

/// <summary>The cmap table of the two-glyph limit font: one format 12 subtable mapping <c>a</c> to glyph 1.</summary>
let private limitCmap =
    [| 0uy; 0uy; 0uy; 1uy; 0uy; 3uy; 0uy; 10uy; 0uy; 0uy; 0uy; 12uy
       0uy; 12uy; 0uy; 0uy; 0uy; 0uy; 0uy; 28uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy
       0uy; 0uy; 0uy; byte 'a'; 0uy; 0uy; 0uy; byte 'a'; 0uy; 0uy; 0uy; 1uy |]

/// <summary>Loads a font file of the upstream API tests.</summary>
/// <param name="name">The file name under <c>test/api/fonts</c>.</param>
/// <returns>The face.</returns>
let private apiFace (name: string) = Face.create (UpstreamFiles.read $"test/api/fonts/{name}") 0u

/// <summary>Builds the face of the limit test: the two-glyph font with one extra table.</summary>
/// <param name="tag">The tag of the table.</param>
/// <param name="table">The table bytes.</param>
/// <returns>The face.</returns>
let private limitFace (tag: Tag) (table: byte array) =
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Maxp, limitMaxp; Tags.Cmap, limitCmap; tag, table ])) 0u

/// <summary>A kern table of format 3 subtables, <c>make_kern_table</c>.</summary>
/// <param name="count">The number of subtables.</param>
/// <returns>The table bytes.</returns>
let private makeKernTable (count: int) =
    let table = Array.zeroCreate (4 + KernSubtableLength * count)
    table[2] <- byte (count >>> 8)
    table[3] <- byte count
    let subtable = Array.init KernSubtableLength (fun index ->
        match index with
        | 3 -> byte KernSubtableLength
        | 4 -> 3uy
        | 5 -> 1uy
        | 7 -> 1uy
        | _ -> 0uy)
    Array.concat [ table[0..3]; Array.concat (Array.replicate count subtable) ]

/// <summary>A morx table of chains of five noncontextual subtables each, <c>make_morx_table</c>.</summary>
/// <param name="chains">The number of chains.</param>
/// <returns>The table bytes.</returns>
let private makeMorxTable (chains: int) =
    let subtable = Array.init MorxSubtableLength (fun index ->
        match index with
        | 3 -> byte MorxSubtableLength
        | 4 -> 0x20uy
        | 7 -> 4uy
        | 11 -> 1uy
        | 13 -> 8uy
        | 15 -> 1uy
        | 17 -> 1uy
        | 19 -> 1uy
        | _ -> 0uy)
    let chain = Array.init 16 (fun index ->
        match index with
        | 3 -> 1uy
        | 7 -> byte MorxChainLength
        | 15 -> byte SubtablesPerChain
        | _ -> 0uy)
    let header = [| 0uy; 2uy; 0uy; 0uy; 0uy; 0uy; byte (chains >>> 8); byte chains |]
    Array.concat [ header; Array.concat (Array.replicate chains (Array.concat (chain :: List.replicate SubtablesPerChain subtable))) ]

/// <summary>Shapes <c>aa</c> with the ot shaper and fails the test unless that succeeds, as the limit test's shaping does.</summary>
/// <param name="face">The face.</param>
/// <returns>The shaped run.</returns>
let private shapeAa (face: Face) =
    let font = Font.create face
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan [| uint32 'a'; uint32 'a' |]) 0 2 GlyphRun.empty
        |> GlyphRun.guessSegmentProperties Language.cLocale
    match Shape.shapeFull font ImmutableArray.Empty (ValueSome (ImmutableArray.Create "ot")) run with
    | Error error -> failtest $"expected Ok, got %A{error}"
    | Ok shaped -> shaped

/// <summary>The number of kern subtables a face applies, <c>count_applied_subtables</c> for kern: a table over the limit is rejected whole.</summary>
/// <param name="count">The number of subtables the table declares.</param>
/// <returns>The number of subtables the loaded table exposes.</returns>
let private kernSubtables (count: int) =
    let face = limitFace Tags.Kern (makeKernTable count)
    shapeAa face |> ignore
    (OtFace.create face).Kern.Value.Subtables.Length

/// <summary>The number of morx subtables a face applies, <c>count_applied_subtables</c> for morx.</summary>
/// <param name="chains">The number of chains the table declares.</param>
/// <returns>The number of subtables the loaded table exposes.</returns>
let private morxSubtables (chains: int) =
    let face = limitFace Tags.Morx (makeMorxTable chains)
    shapeAa face |> ignore
    (OtFace.create face).Morx.Value.Chains.AsSpan().ToArray() |> Array.sumBy (fun chain -> chain.Subtables.Length)

/// <summary>The cmap of the ranged-feature font: <c>a</c> is glyph 1 and <c>b</c> glyph 2.</summary>
let private rangedCmap =
    [| 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x03uy; 0x00uy; 0x0Auy; 0x00uy; 0x00uy; 0x00uy; 0x0Cuy
       0x00uy; 0x0Cuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x1Cuy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy
       0x00uy; 0x00uy; 0x00uy; 0x61uy; 0x00uy; 0x00uy; 0x00uy; 0x62uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy |]

/// <summary>The maxp of the ranged-feature font: three glyphs.</summary>
let private rangedMaxp = [| 0x00uy; 0x00uy; 0x50uy; 0x00uy; 0x00uy; 0x03uy |]

/// <summary>The feat of the ranged-feature font: feature type 1 (ligatures) with the settings 2 and 3, exclusive.</summary>
let private rangedFeat =
    [| 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
       0x00uy; 0x01uy; 0x00uy; 0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x18uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy
       0x00uy; 0x02uy; 0x01uy; 0x00uy; 0x00uy; 0x03uy; 0x01uy; 0x00uy |]

/// <summary>The first chain of the ranged-feature font: glyph 1 becomes glyph 2 where common ligatures are on.</summary>
let private rangedChain =
    [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x3Cuy; 0x00uy; 0x00uy; 0x00uy; 0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy
       0x00uy; 0x01uy; 0x00uy; 0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy
       0x00uy; 0x01uy; 0x00uy; 0x03uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFEuy
       0x00uy; 0x00uy; 0x00uy; 0x14uy; 0x20uy; 0x00uy; 0x00uy; 0x04uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy
       0x00uy; 0x08uy; 0x00uy; 0x01uy; 0x00uy; 0x01uy; 0x00uy; 0x02uy |]

/// <summary>The size of an empty morx chain, in bytes.</summary>
[<Literal>]
let private EmptyChainLength = 16

/// <summary>A font mapping <c>a</c> and <c>b</c> to glyphs 1 and 2 whose first morx chain turns glyph 1 into glyph 2
/// where common ligatures are on, followed by chains that have nothing in them, <c>create_morx_face</c>.</summary>
/// <param name="emptyChains">The number of empty chains after the first chain.</param>
/// <returns>The face.</returns>
let private createMorxFace (emptyChains: int) =
    let chainCount = uint32 (1 + emptyChains)
    let empty = Array.init EmptyChainLength (fun index -> if index = 7 then byte EmptyChainLength else 0uy)
    let morx =
        Array.concat
            [ [| 0uy; 2uy; 0uy; 0uy |]
              be32 chainCount
              rangedChain
              Array.concat (Array.replicate emptyChains empty) ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, rangedCmap; Tags.Maxp, rangedMaxp; Tags.Feat, rangedFeat; Tags.Morx, morx ])) 0u

/// <summary>Shapes a run of <c>a</c> with ligatures on in every character whose index is not a multiple of the period,
/// each character having its own feature range, <c>shape_ranged_features</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="length">The number of characters.</param>
/// <param name="period">The period.</param>
/// <returns>The shaped run.</returns>
let private shapeRangedFeatures (face: Face) (length: int) (period: int) =
    let font = Font.create face
    let features =
        Array.init length (fun index ->
            { Tag = Tags.Liga
              Value = (if index % period <> 0 then 1u else 0u)
              Start = uint32 index
              End = uint32 index + 1u })
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan (Array.create length (uint32 'a'))) 0 length GlyphRun.empty
        |> GlyphRun.guessSegmentProperties Language.cLocale
    let shaped =
        match Shape.shapeFull font (ImmutableArray.CreateRange features) (ValueSome (ImmutableArray.Create "ot")) run with
        | Error error -> failtest $"expected Ok, got %A{error}"
        | Ok shaped -> shaped
    Expect.equal shaped.Infos.Length length "the shaped run keeps its length"
    shaped

/// <summary>The glyphs of a shaped run.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyph indices.</returns>
let private glyphsOf (run: GlyphRun) = run.Infos.AsSpan().ToArray() |> Array.map _.Codepoint

/// <summary>The most time the many-chains test may take, in milliseconds: the number of chains times the number of
/// ranges must not be paid for.</summary>
[<Literal>]
let private ManyChainsBudgetMilliseconds = 60000L

/// <summary>A feat table with one exclusive feature type 5 of two settings, 0 named 300 and 1 named 301.</summary>
/// <param name="flags">The feature flags.</param>
/// <returns>The table bytes.</returns>
let private oneFeatureFeat (flags: uint32) =
    Array.concat
        [ be32 0x00010000u; be16 1u; be16 0u; be32 0u
          be16 5u; be16 2u; be32 24u; be16 flags; be16 257u
          be16 0u; be16 300u; be16 1u; be16 301u ]

/// <summary>The P8.S14 tests of <c>test/api/test-aat-layout.c</c>.</summary>
[<Tests>]
let tests =
    testList "P8.S14" [
        test "test_aat_get_feature_types" {
            let feat = (OtFace.create (apiFace "aat-feat.ttf")).Feat.Value
            Expect.equal (Feat.featureTypeCount feat) 11 "the font has eleven feature types"
            let types = Feat.featureTypes feat 0u 3u
            Expect.equal (types.AsSpan().ToArray()) [| 1us; 3us; 6us |] "the first three feature types"
            Expect.equal (Feat.featureNameId feat 1us) 258u "the name id of type 1"
            Expect.equal (Feat.featureNameId feat 3us) 261u "the name id of type 3"
            Expect.equal (Feat.featureNameId feat 6us) 265u "the name id of type 6"
        }
        test "test_aat_get_feature_selectors" {
            let feat = (OtFace.create (apiFace "aat-feat.ttf")).Feat.Value
            let struct (total, defaultIndex, settings) = Feat.selectorInfos feat DesignComplexityType 0u 3u
            Expect.equal total 4u "four selectors in the type"
            Expect.equal settings.Length 3 "three selectors written"
            Expect.equal defaultIndex 0u "the default index"
            Expect.equal (settings.AsSpan().ToArray() |> Array.map (fun info -> info.Enable, info.NameId)) [| 0u, 294u; 1u, 295u; 2u, 296u |] "the first three selectors"
            let struct (total, defaultIndex, settings) = Feat.selectorInfos feat DesignComplexityType 3u 3u
            Expect.equal total 4u "four selectors in the type"
            Expect.equal settings.Length 1 "one selector left after the offset"
            Expect.equal defaultIndex 0u "the default index"
            Expect.equal (settings[0].Enable, settings[0].NameId) (3u, 297u) "the fourth selector"
            let struct (total, defaultIndex, settings) = Feat.selectorInfos feat TypographicExtras 0u 1u
            Expect.equal total 1u "one selector in the typographic extras"
            Expect.equal settings.Length 1 "one selector written"
            Expect.equal defaultIndex Feat.NoSelectorIndex "a non-exclusive type has no default selector"
            Expect.equal (settings[0].Enable, settings[0].NameId) (8u, 308u) "the selector of the typographic extras"
            let struct (total, _, settings) = Feat.selectorInfos feat FeatureTypeInvalid 0u 100u
            Expect.equal total 0u "no selector in the invalid type"
            Expect.equal settings.Length 0 "nothing written for the invalid type"
        }
        test "a default index past the settings reads the Null setting, whose every byte is 0xFF" {
            let feat = Feat.load (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Feat, oneFeatureFeat 0xC0FFu ])) 0u)
            let struct (total, defaultIndex, infos) = Feat.selectorInfos feat 5us 0u 2u
            Expect.equal (total, defaultIndex) (2u, 255u) "two settings, the default index of the flags"
            let words = infos.AsSpan().ToArray() |> Array.map (fun info -> info.NameId, info.Enable, info.Disable)
            Expect.equal words [| 300u, 0u, 1u; 301u, 1u, 2u |] "the Null default selector is the invalid selector, so each disables with the next selector"
        }
        test "an exclusive feature disables with its default selector" {
            let feat = Feat.load (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Feat, oneFeatureFeat 0xC001u ])) 0u)
            let struct (_, defaultIndex, infos) = Feat.selectorInfos feat 5us 0u 2u
            Expect.equal defaultIndex 1u "the default is the second setting"
            Expect.equal (infos.AsSpan().ToArray() |> Array.map _.Disable) [| 1u; 1u |] "every selector disables with the default selector"
        }
        test "test_aat_has" {
            Expect.isTrue (AatLayout.hasSubstitution (OtFace.create (apiFace "aat-morx.ttf"))) "aat-morx.ttf has substitutions"
            Expect.isTrue (Trak.hasData (OtFace.create (apiFace "aat-trak.ttf")).Trak.Value) "aat-trak.ttf has tracking"
        }
        test "test_aat_kern_subtable_limit" {
            Expect.equal (kernSubtables MaxSubtables) MaxSubtables "1024 kern subtables are all exposed"
            Expect.equal (kernSubtables (MaxSubtables + 1)) 0 "1025 kern subtables are rejected whole"
        }
        test "test_aat_morx_subtable_limit" {
            Expect.equal (morxSubtables 204) 1020 "204 chains of five subtables are 1020 subtables"
            Expect.equal (morxSubtables 205) 0 "205 chains of five subtables are 1025 subtables and rejected whole"
        }
        test "test_aat_ranged_features" {
            let shaped = shapeRangedFeatures (createMorxFace 1) 100 3
            let glyphs = glyphsOf shaped
            for index in 0 .. 99 do
                Expect.equal glyphs[index] (if index % 3 <> 0 then 2u else 1u) $"the glyph of character {index}"
        }
        test "test_aat_ranged_features_many_chains" {
            let face = createMorxFace 20000
            let clock = Stopwatch.StartNew()
            let shaped = shapeRangedFeatures face 5000 2
            clock.Stop()
            let glyphs = glyphsOf shaped
            for index in 0 .. 99 do
                Expect.equal glyphs[index] (if index % 2 <> 0 then 2u else 1u) $"the glyph of character {index}"
            Expect.isLessThan clock.ElapsedMilliseconds ManyChainsBudgetMilliseconds "shaping 5000 ranged characters over 20000 chains stays within the map budget"
        }
    ]
