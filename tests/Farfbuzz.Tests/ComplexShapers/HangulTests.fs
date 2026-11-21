module Farfbuzz.Tests.ComplexShapers.HangulTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>The glyphs of the test fonts: the letter A, a leading consonant, a vowel, two trailing consonants, three
/// syllables, two tone marks (the second with no advance) and the dotted circle.</summary>
let private glyphs : (uint32 * uint32) list =
    [ 0x41u, 1u; 0x1100u, 2u; 0x1161u, 3u; 0x11A8u, 4u; 0x11A9u, 5u; 0xAC00u, 6u; 0xAC01u, 7u; 0xAC02u, 8u
      0x302Eu, 9u; 0x302Fu, 10u; 0x25CCu, 11u ]

/// <summary>The advances of glyphs 0 to 11: every glyph is 500 wide but glyph 10, the second tone mark.</summary>
let private advances : uint32 list =
    [ 500u; 500u; 500u; 500u; 500u; 500u; 500u; 500u; 500u; 500u; 0u; 500u ]

/// <summary>A font with the jamo, the syllables, the tone marks and the dotted circle.</summary>
let private fullFont = fontMappingWithAdvances glyphs advances

/// <summary>A font with the jamo, the tone marks and the dotted circle, and no precomposed syllable.</summary>
let private jamoFont =
    fontMappingWithAdvances (glyphs |> List.filter (fun (codepoint, _) -> codepoint < 0xAC00u || codepoint > 0xD7A3u)) advances

/// <summary>A font with the jamo, the syllables without a trailing consonant, the tone marks and the dotted circle.</summary>
let private lvFont =
    fontMappingWithAdvances (glyphs |> List.filter (fun (codepoint, _) -> codepoint <> 0xAC01u && codepoint <> 0xAC02u)) advances

/// <summary>A font with the syllables and the tone marks and neither the jamo nor the dotted circle.</summary>
let private syllableFont =
    fontMappingWithAdvances (glyphs |> List.filter (fun (codepoint, _) -> codepoint >= 0xAC00u && codepoint <= 0xAC02u || codepoint = 0x302Eu || codepoint = 0x302Fu)) advances

/// <summary>The plan of the Hangul shaper.</summary>
let private hangulPlan = planFor HangulShaper Scripts.Hangul

/// <summary>The masks of the plan data the mask tests use: <c>ljmo</c>, <c>vjmo</c>, <c>tjmo</c> and <c>calt</c>.</summary>
let private masks : HangulPlan = { Ljmo = 0x10u; Vjmo = 0x20u; Tjmo = 0x40u; Calt = 0x80u }

/// <summary>The plan of the Hangul shaper with the masks of <see cref="masks"/>.</summary>
let private maskedPlan = { hangulPlan with Data = HangulData masks }

/// <summary>Prepares a run through the Hangul hook.</summary>
/// <param name="font">The font.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The prepared run.</returns>
let private prepared (font: Font) (codepoints: uint32 array) : GlyphRun =
    ShaperHooks.preprocess hangulPlan font (runOf codepoints)

/// <summary>Reads the jamo feature each glyph of a run was given.</summary>
/// <param name="run">The run.</param>
/// <returns>The features, zero for none, 1 for <c>ljmo</c>, 2 for <c>vjmo</c>, 3 for <c>tjmo</c>.</returns>
let private featuresOf (run: GlyphRun) : byte array =
    [| for props in run.Props -> props.ShaperAuxiliary |]

/// <summary>The P6.S16 Hangul shaper tests: the record, the features, the plan data, the composition and decomposition of
/// syllables by what the font has, the tone marks, the masks and their order, and the upstream conformance lines.</summary>
[<Tests>]
let tests =
    testList "P6.S16" [
        testCase "hangul shaper record" (fun _ ->
            let record = Categorize.profile HangulShaper
            Expect.equal record.Kind HangulShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationNone "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksNone "zero-width marks"
            Expect.isTrue record.FallbackPosition "fallback position")
        testCase "collect_features adds ljmo, vjmo and tjmo, then override_features adds calt after the user features" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Hangul; Language = NoLanguage }
            let user = ImmutableArray.Create { Tag = Tag.ofString "kern"; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            let collected = ShaperHooks.collectFeatures props HangulShaper user
            let tags = [ for info in collected.Features -> Tag.toString info.InfoTag ]
            let expected = [ "ljmo"; "vjmo"; "tjmo" ]
            Expect.equal (tags |> List.filter (fun tag -> List.contains tag expected)) expected "jamo features in order"
            Expect.equal (List.last tags) "calt" "calt last"
            let jamoAt = tags |> List.findIndex (fun tag -> tag = "ljmo")
            let userAt = tags |> List.findIndex (fun tag -> tag = "kern")
            Expect.isLessThan jamoAt userAt "the jamo features come before the user features"
            Expect.isFalse collected.IsSimple "a hook makes the map non-simple")
        testCase "data_create reads the four masks of the compiled map" (fun _ ->
            let mask1 (tag: Tag) =
                match Tag.toString tag with
                | "ljmo" -> 1u
                | "vjmo" -> 2u
                | "tjmo" -> 4u
                | "calt" -> 8u
                | _ -> 0u
            Expect.equal (ShaperHooks.createData HangulShaper mask1 hangulPlan.Props OtMap.empty) (HangulData { Ljmo = 1u; Vjmo = 2u; Tjmo = 4u; Calt = 8u }) "masks"
            Expect.equal (ShaperHooks.createData DefaultShaper mask1 hangulPlan.Props OtMap.empty) NoShaperData "no data for the default shaper")
        testCase "the compiled plan carries the data" (fun _ ->
            match hangulPlan.Data with
            | HangulData data -> Expect.equal data { Ljmo = 0u; Vjmo = 0u; Tjmo = 0u; Calt = 0u } "no feature in the empty map"
            | NoShaperData | KhmerData _ | IndicData _ | UseData _ | ArabicData _ -> failtest "expected Hangul data")
        testCase "an empty run and a run with no jamo, syllable or tone mark are returned as they are" (fun _ ->
            let empty = runOf [||]
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.preprocess hangulPlan fullFont empty, empty)) "empty"
            let latin = runOf [| 0x41u; 0x1161u; 0x11A8u |]
            Expect.isTrue (obj.ReferenceEquals(ShaperHooks.preprocess hangulPlan fullFont latin, latin)) "a vowel and a trailing consonant alone")
        testCase "one leading consonant alone is left as it is" (fun _ ->
            let run = prepared fullFont [| 0x1100u |]
            Expect.equal (codepointsOf run) [| 0x1100u |] "unchanged"
            Expect.equal (featuresOf run) [| 0uy |] "no feature")
        testCase "a leading consonant and a vowel compose into the syllable the font has" (fun _ ->
            let run = prepared fullFont [| 0x1100u; 0x1161u |]
            Expect.equal (codepointsOf run) [| 0xAC00u |] "composed"
            Expect.equal (clustersOf run) [| 0u |] "one cluster")
        testCase "a leading consonant, a vowel and a trailing consonant compose, the trailing one picking the syllable" (fun _ ->
            Expect.equal (codepointsOf (prepared fullFont [| 0x1100u; 0x1161u; 0x11A8u |])) [| 0xAC01u |] "first trailing consonant"
            Expect.equal (codepointsOf (prepared fullFont [| 0x1100u; 0x1161u; 0x11A9u |])) [| 0xAC02u |] "second trailing consonant")
        testCase "jamo that a font cannot compose are given their features and one cluster" (fun _ ->
            let run = prepared jamoFont [| 0x1100u; 0x1161u; 0x11A8u |]
            Expect.equal (codepointsOf run) [| 0x1100u; 0x1161u; 0x11A8u |] "kept"
            Expect.equal (featuresOf run) [| 1uy; 2uy; 3uy |] "features"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u |] "one cluster")
        testCase "two jamo that a font cannot compose are given leading and vowel features" (fun _ ->
            let run = prepared jamoFont [| 0x1100u; 0x1161u |]
            Expect.equal (featuresOf run) [| 1uy; 2uy |] "features"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "old Hangul jamo that Unicode has no syllable for get features and do not compose" (fun _ ->
            let run = prepared fullFont [| 0xA960u; 0x1161u |]
            Expect.equal (codepointsOf run) [| 0xA960u; 0x1161u |] "kept"
            Expect.equal (featuresOf run) [| 1uy; 2uy |] "features"
            let old = prepared fullFont [| 0x1100u; 0x1161u; 0x11C3u |]
            Expect.equal (codepointsOf old) [| 0x1100u; 0x1161u; 0x11C3u |] "a trailing consonant that does not compose is kept"
            Expect.equal (featuresOf old) [| 1uy; 2uy; 3uy |] "it is still a trailing consonant")
        testCase "a precomposed syllable the font has is kept" (fun _ ->
            let run = prepared fullFont [| 0xAC01u |]
            Expect.equal (codepointsOf run) [| 0xAC01u |] "kept"
            Expect.equal (featuresOf run) [| 0uy |] "no feature")
        testCase "a precomposed syllable the font lacks is decomposed when the font has its jamo" (fun _ ->
            let run = prepared jamoFont [| 0xAC01u |]
            Expect.equal (codepointsOf run) [| 0x1100u; 0x1161u; 0x11A8u |] "decomposed"
            Expect.equal (featuresOf run) [| 1uy; 2uy; 3uy |] "features"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u |] "one cluster"
            Expect.equal (codepointsOf (prepared jamoFont [| 0xAC00u |])) [| 0x1100u; 0x1161u |] "a syllable with no trailing consonant")
        testCase "a precomposed syllable the font lacks is kept when the font lacks its jamo too" (fun _ ->
            Expect.equal (codepointsOf (prepared syllableFont [| 0xAC00u; 0xD7A3u |])) [| 0xAC00u; 0xD7A3u |] "kept"
            Expect.equal (codepointsOf (prepared (fontMappingWithAdvances [ 0x1100u, 1u; 0x41u, 2u ] [ 500u; 500u; 500u ]) [| 0xAC01u |])) [| 0xAC01u |] "no vowel glyph")
        testCase "a syllable without a trailing consonant and a trailing consonant after it compose when the font has the result" (fun _ ->
            let run = prepared fullFont [| 0xAC00u; 0x11A8u |]
            Expect.equal (codepointsOf run) [| 0xAC01u |] "composed"
            Expect.equal (clustersOf run) [| 0u |] "one cluster")
        testCase "a syllable and a trailing consonant that the font has no syllable for are decomposed together" (fun _ ->
            let run = prepared lvFont [| 0xAC00u; 0x11A8u |]
            Expect.equal (codepointsOf run) [| 0x1100u; 0x1161u; 0x11A8u |] "decomposed with the trailing consonant"
            Expect.equal (featuresOf run) [| 1uy; 2uy; 3uy |] "features"
            Expect.equal (clustersOf run) [| 0u; 0u; 0u |] "one cluster")
        testCase "a syllable followed by a trailing consonant that does not compose is decomposed with it, even when the font has the syllable" (fun _ ->
            let run = prepared fullFont [| 0xAC00u; 0x11C3u |]
            Expect.equal (codepointsOf run) [| 0x1100u; 0x1161u; 0x11C3u |] "decomposed"
            Expect.equal (featuresOf run) [| 1uy; 2uy; 3uy |] "features")
        testCase "a syllable with a trailing consonant is not combined with a following trailing consonant" (fun _ ->
            let run = prepared fullFont [| 0xAC01u; 0x11A8u |]
            Expect.equal (codepointsOf run) [| 0xAC01u; 0x11A8u |] "kept apart"
            Expect.equal (featuresOf run) [| 0uy; 0uy |] "no feature")
        testCase "a tone mark after a syllable moves in front of it and the clusters merge" (fun _ ->
            let run = prepared fullFont [| 0xAC00u; 0x302Eu |]
            Expect.equal (codepointsOf run) [| 0x302Eu; 0xAC00u |] "moved"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster")
        testCase "a tone mark after composed jamo moves in front of the composed syllable" (fun _ ->
            let run = prepared fullFont [| 0x41u; 0x1100u; 0x1161u; 0x302Eu |]
            Expect.equal (codepointsOf run) [| 0x41u; 0x302Eu; 0xAC00u |] "moved"
            Expect.equal (clustersOf run) [| 0u; 1u; 1u |] "the tone mark joins the syllable's cluster")
        testCase "a tone mark with no advance stays where it is" (fun _ ->
            let run = prepared fullFont [| 0xAC00u; 0x302Fu |]
            Expect.equal (codepointsOf run) [| 0xAC00u; 0x302Fu |] "stays"
            Expect.equal (clustersOf run) [| 0u; 1u |] "clusters apart")
        testCase "a tone mark first in the run gets a dotted circle after it, or before it when it has no advance" (fun _ ->
            let run = prepared fullFont [| 0x302Eu |]
            Expect.equal (codepointsOf run) [| 0x302Eu; 0x25CCu |] "after"
            Expect.equal (clustersOf run) [| 0u; 0u |] "one cluster"
            Expect.equal (codepointsOf (prepared fullFont [| 0x302Fu |])) [| 0x25CCu; 0x302Fu |] "before")
        testCase "a tone mark after a letter that is no syllable gets a dotted circle" (fun _ ->
            Expect.equal (codepointsOf (prepared fullFont [| 0x41u; 0x302Eu |])) [| 0x41u; 0x302Eu; 0x25CCu |] "dotted circle")
        testCase "a second tone mark after a syllable has no syllable to follow and gets a dotted circle" (fun _ ->
            let run = prepared fullFont [| 0xAC00u; 0x302Eu; 0x302Eu |]
            Expect.equal (codepointsOf run) [| 0x302Eu; 0xAC00u; 0x302Eu; 0x25CCu |] "moved, then circled")
        testCase "a tone mark is left alone when the font has no dotted circle or the buffer asks for none" (fun _ ->
            Expect.equal (codepointsOf (prepared syllableFont [| 0x302Eu |])) [| 0x302Eu |] "no glyph"
            let flagged = ShaperHooks.preprocess hangulPlan fullFont (GlyphRun.withFlags BufferFlags.DoNotInsertDottedCircle (runOf [| 0x302Eu |]))
            Expect.equal (codepointsOf flagged) [| 0x302Eu |] "flag")
        testCase "a tone mark moves in front of the last syllable only" (fun _ ->
            let run = prepared fullFont [| 0xAC00u; 0x1100u; 0x1161u; 0x302Eu |]
            Expect.equal (codepointsOf run) [| 0xAC00u; 0x302Eu; 0xAC00u |] "moved in front of the second syllable")
        testCase "the cluster merge of the jamo of a syllable the font cannot compose clears the flags the pair was given" (fun _ ->
            let run = ShaperHooks.preprocess hangulPlan jamoFont (runWith ClusterLevel.MonotoneGraphemes 0x80000000u [| 0x1100u; 0x1161u |])
            Expect.equal (clustersOf run) [| 0u; 0u |] "clusters"
            Expect.equal (masksOf run) [| 0x80000000u; 0x80000000u |] "no flag: the clusters are one")
        testCase "the characters level does not merge the jamo clusters of a syllable it cannot compose" (fun _ ->
            let run = ShaperHooks.preprocess hangulPlan jamoFont (runWith ClusterLevel.Characters 0x80000000u [| 0x1100u; 0x1161u |])
            Expect.equal (clustersOf run) [| 0u; 1u |] "clusters apart")
        testCase "setup_masks gives each jamo the mask of its feature and keeps calt away from every jamo" (fun _ ->
            let run = ShaperHooks.preprocess maskedPlan jamoFont (runWith ClusterLevel.MonotoneGraphemes 0x80u [| 0x41u; 0x1100u; 0x1161u; 0x11A8u |])
            let masked = OtSubstitute.setupMasks maskedPlan run
            Expect.equal (masksOf masked) [| 0x80u; 0x10u; 0x20u; 0x40u |] "feature masks, calt cleared on the jamo only")
        testCase "setup_masks clears calt on a jamo that has no feature too" (fun _ ->
            let run = runWith ClusterLevel.MonotoneGraphemes 0x80u [| 0x1100u; 0x1161u; 0x11A8u; 0xAC00u |]
            let masked = OtSubstitute.setupMasks maskedPlan run
            Expect.equal (masksOf masked) [| 0u; 0u; 0u; 0x80u |] "calt cleared on L, V and T, not on a syllable")
        testCase "setup_masks reads the data of the plan: a plan without data sets nothing" (fun _ ->
            let run = runWith ClusterLevel.MonotoneGraphemes 0x80u [| 0x1100u; 0x41u |]
            Expect.isTrue (masksOf (OtSubstitute.setupMasks { hangulPlan with Data = NoShaperData } run) = [| 0x80u; 0x80u |]) "unchanged")
        testCase "setup_masks runs after the fraction masks and before the user features" (fun _ ->
            let feature (name: string) (mask: uint32) (shift: int) : FeatureMap =
                { Tag = Tag.ofString name
                  GsubIndex = 0u
                  GposIndex = 0u
                  GsubStage = 0
                  GposStage = 0
                  Shift = shift
                  Mask = mask
                  Mask1 = mask
                  NeedsFallback = false
                  AutoZwnj = true
                  AutoZwj = true
                  Random = false
                  PerSyllable = false }
            let map = { OtMap.empty with Features = ImmutableArray.Create (feature "ljmo" 0x10u 4) }
            let user = ImmutableArray.Create { Tag = Tag.ofString "ljmo"; Value = 0u; Start = 0u; End = 1u }
            let plan = { maskedPlan with Map = map; UserFeatures = user }
            let run = ShaperHooks.preprocess plan jamoFont (runWith ClusterLevel.MonotoneGraphemes 0u [| 0x1100u; 0x1161u |])
            let masked = OtSubstitute.setupMasks plan run
            Expect.equal (masksOf masked) [| 0u; 0x20u |] "the user feature turned ljmo off after the hook had set it")
        testCase "the properties of every precomposed syllable, and of the code points on either side of the block, set in one loop, equal the reference" (fun _ ->
            let sameAsReference (codepoints: uint32 array) =
                let entered = GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
                let fused = OtProps.setUnicodePropsEntered 0x1234u entered
                let reference = OtProps.setUnicodeProps 0x1234u entered
                let first = codepoints[0]
                Expect.equal (fused.Props.AsSpan().ToArray()) (reference.Props.AsSpan().ToArray()) $"properties of {codepoints.Length} code points from U+{first:X4}"
                Expect.equal fused.ScratchFlags reference.ScratchFlags "scratch flags"
            sameAsReference [| for codepoint in 0xAC00u .. 0xD7A3u -> codepoint |]
            for neighbour in [ 0xABFFu; 0xD7A4u; 0xD7AFu; 0xD7B0u; 0x11FFu; 0x1100u; 0x302Eu ] do
                sameAsReference [| neighbour; 0xAC00u; 0xD7A3u |]
                sameAsReference [| 0xAC01u; neighbour; 0xD7A3u |]
                sameAsReference [| 0xAC01u; 0xD7A3u; neighbour |])
        testCase "the owned preprocess over every syllable, jamo and tone mark equals the reference when the font maps every third of them" (fun _ ->
            let codepoints =
                [| for codepoint in 0xAC00u .. 0xD7A3u do
                    yield codepoint
                    match codepoint % 28u with
                    | 5u -> yield 0x11A8u + codepoint % 27u
                    | 9u -> yield 0x302Eu
                    | _ -> () |]
            let jamo = [| for index in 0 .. 3000 -> 0x1100u + uint32 (index * 7 % 19) |]
            let mixed = Array.append codepoints (Array.collect (fun (l: uint32) -> [| l; 0x1161u + l % 21u; 0x11A8u + l % 27u |]) jamo)
            let mapped = [ for codepoint in mixed |> Array.distinct do if codepoint < 0x1100u || codepoint >= 0x1200u && codepoint % 3u <> 0u || codepoint >= 0x1100u && codepoint < 0x1200u then yield codepoint ] @ [ 0x25CCu ]
            let font = fontMappingWithAdvances (mapped |> List.mapi (fun index codepoint -> codepoint, uint32 (index + 1))) (0u :: (mapped |> List.mapi (fun index _ -> if index % 17 = 0 then 0u else 500u)))
            let run = runOf mixed
            let reference = ShaperHooks.preprocess hangulPlan font run
            let owned = Hangul.preprocessOwned font (runOf mixed)
            Expect.equal (codepointsOf owned) (codepointsOf reference) "code points"
            Expect.equal [| for info in owned.Infos -> info.Cluster |] [| for info in reference.Infos -> info.Cluster |] "clusters"
            Expect.equal [| for info in owned.Infos -> info.Mask |] [| for info in reference.Infos -> info.Mask |] "masks"
            Expect.equal (featuresOf owned) (featuresOf reference) "features"
            Expect.equal owned.MaxOps reference.MaxOps "operations left"
            Expect.equal owned.Successful reference.Successful "success")
        testCase "the owned preprocess equals the reference at every cluster level, with the operations and the length running out mid-pass" (fun _ ->
            let jamo = [| for index in 0 .. 40 -> [| 0x1100u + uint32 (index * 7 % 19); 0x1161u + uint32 (index * 5 % 21); 0x11A8u + uint32 (index * 3 % 27) |] |]
            let text =
                Array.concat [ for index in 0 .. 40 ->
                                let leading = Array.sub jamo[index] 0 ((index % 3) + 1)
                                let trailing = if index % 4 = 0 then [| 0xAC00u + uint32 (index * 28); 0x302Eu |] else [| 0x41u |]
                                Array.append leading trailing ]
            let mapped = [ for codepoint in text |> Array.distinct -> codepoint ] @ [ 0xAC00u + 28u * 5u; 0x25CCu ]
            let font = fontMappingWithAdvances (mapped |> List.mapi (fun index codepoint -> codepoint, uint32 (index + 1))) (0u :: (mapped |> List.map (fun _ -> 500u)))
            for level in [ ClusterLevel.Characters; ClusterLevel.MonotoneCharacters; ClusterLevel.Graphemes; ClusterLevel.MonotoneGraphemes ] do
                for budget in [ 0; 1; 2; 5; 9; 14; 30; 61; 200; 5000 ] do
                    for limit in [ 3; 10; 60; 1000 ] do
                        let build () =
                            let run = runWith level 0u text
                            let clustered = OtProps.formClusters run
                            { clustered with MaxOps = budget; MaxLen = limit }
                        let reference = ShaperHooks.preprocess hangulPlan font (build ())
                        let owned = Hangul.preprocessOwned font (build ())
                        let label = $"{level}, {budget} operations, {limit} glyphs"
                        Expect.equal (codepointsOf owned) (codepointsOf reference) $"code points, {label}"
                        Expect.equal [| for info in owned.Infos -> struct (info.Cluster, info.Mask) |] [| for info in reference.Infos -> struct (info.Cluster, info.Mask) |] $"clusters and masks, {label}"
                        Expect.equal (featuresOf owned) (featuresOf reference) $"features, {label}"
                        Expect.equal owned.MaxOps reference.MaxOps $"operations left, {label}"
                        Expect.equal owned.Successful reference.Successful $"success, {label}")
        Conformance.files "conformance" [ "in-house", "hangul-jamo.tests"; "in-house", "hangul-calt.tests" ]
    ]
