module Farfbuzz.Tests.ComplexShapers.ShaperHooksTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

/// <summary>The kinds whose hooks are ported, and the script each is planned for.</summary>
let private ported : (string * ShaperKind * Script) list =
    [ "default", DefaultShaper, Scripts.Latin; "dumber", DumberShaper, Scripts.Latin; "hebrew", HebrewShaper, Scripts.Hebrew; "thai", ThaiShaper, Scripts.Thai
      "hangul", HangulShaper, Scripts.Hangul; "khmer", KhmerShaper, Scripts.Khmer; "indic", IndicShaper, Scripts.Devanagari; "use", UseShaper, Scripts.Javanese
      "myanmar", MyanmarShaper, Scripts.Myanmar; "myanmar zawgyi", MyanmarZawgyiShaper, Scripts.Myanmar; "arabic", ArabicShaper, Scripts.Arabic ]

/// <summary>The kinds whose <c>collect_features</c> hook adds nothing.</summary>
let private withoutFeatures : (string * ShaperKind * Script) list =
    ported |> List.filter (fun (_, kind, _) ->
        match kind with
        | HangulShaper | KhmerShaper | IndicShaper | UseShaper | MyanmarShaper | ArabicShaper -> false
        | DefaultShaper | DumberShaper | HebrewShaper | ThaiShaper | MyanmarZawgyiShaper -> true)

/// <summary>Compiles a plan for a kind over the empty map and capabilities.</summary>
/// <param name="kind">The kind.</param>
/// <returns>The plan.</returns>
let private compileKind (kind: ShaperKind) =
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    OtPlan.compile PlanCapabilities.none props ImmutableArray.Empty OtMap.empty kind

/// <summary>The shaper-hook placement tests: which kinds compile, what the hooks do for the shapers that have none, and how
/// the features of a plan are collected.</summary>
[<Tests>]
let tests =
    testList "P6.S14 P6.S15 hooks" [
        yield!
            ported
            |> List.map (fun (name, kind, _) ->
                testCase $"{name} compiles a plan" (fun _ -> Expect.equal (compileKind kind).Shaper.Kind kind "kind"))
        testCase "a shaper without a preprocess, postprocess or reorder hook returns the run it was given" (fun _ ->
            let run = runOf [| 0x05D0u; 0x05BCu; 0x05B7u |]
            for name, kind, script in ported do
                let plan = planFor kind script
                match kind with
                | ThaiShaper | HangulShaper | IndicShaper | UseShaper -> ()
                | DefaultShaper | DumberShaper | HebrewShaper | ArabicShaper | KhmerShaper | MyanmarShaper | MyanmarZawgyiShaper ->
                    Expect.isTrue (obj.ReferenceEquals(ShaperHooks.preprocess plan (fontMapping [ 0x05D0u, 1u ]) run, run)) $"{name} preprocess"
                Expect.isTrue (obj.ReferenceEquals(ShaperHooks.postprocess plan (fontMapping [ 0x05D0u, 1u ]) run, run)) $"{name} postprocess"
                match kind with
                | HebrewShaper -> ()
                | DefaultShaper | DumberShaper | ThaiShaper | ArabicShaper | HangulShaper | IndicShaper | KhmerShaper | MyanmarShaper | MyanmarZawgyiShaper | UseShaper ->
                    Expect.isTrue (obj.ReferenceEquals(ShaperHooks.reorderMarks plan run 1 3, run)) $"{name} reorder marks")
        testCase "decomposition and composition are Unicode's for every ported shaper but Hebrew's composition and Khmer's split vowels" (fun _ ->
            let font = fontMapping [ 0x61u, 1u ]
            for name, kind, script in ported do
                let plan = planFor kind script
                Expect.equal (ShaperHooks.decompose plan font 0xE1u) (Unicode.decompose 0xE1u) $"{name} decompose"
                Expect.equal (ShaperHooks.decompose plan font 0x61u) (Unicode.decompose 0x61u) $"{name} decompose of a base"
                Expect.equal (ShaperHooks.compose plan 0x61u 0x0301u) (Unicode.compose 0x61u 0x0301u) $"{name} compose")
        testCase "the features of the shapers with no collect hook are the features of the default shaper, the map staying simple" (fun _ ->
            let props = { Direction = Direction.Rtl; Script = Scripts.Hebrew; Language = NoLanguage }
            let expected = MapBuilder.collectHead props MapBuilder.empty |> MapBuilder.collectTail props ImmutableArray.Empty
            for name, kind, _ in withoutFeatures do
                let collected = ShaperHooks.collectFeatures props kind ImmutableArray.Empty
                Expect.equal collected.Features expected.Features $"{name} features"
                Expect.isTrue collected.IsSimple $"{name} simple")
        testCase "a user feature makes the map non-simple" (fun _ ->
            let props = { Direction = Direction.Ltr; Script = Scripts.Thai; Language = NoLanguage }
            let user = ImmutableArray.Create { Tag = Tag.ofString "liga"; Value = 0u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            Expect.isFalse (ShaperHooks.collectFeatures props ThaiShaper user).IsSimple "non-simple")
    ]
