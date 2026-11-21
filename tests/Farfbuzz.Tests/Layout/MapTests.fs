module Farfbuzz.Tests.Layout.MapTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Reads a font, creates the face and font and compiles a default-shaper plan.</summary>
/// <param name="path">The upstream font path.</param>
/// <param name="direction">The segment direction.</param>
/// <param name="script">The segment script.</param>
/// <param name="features">The user features.</param>
/// <returns>The plan.</returns>
let private planOf (path: string) (direction: Direction) (script: Script) (features: Feature list) : OtShapePlan =
    let font = Font.create (Face.create (read path) 0u)
    let props = { Direction = direction; Script = script; Language = NoLanguage }
    OtPlan.create font props (ImmutableArray.CreateRange features)

/// <summary>Compiles a plan over a hand-built GSUB table.</summary>
/// <param name="bytes">The GSUB bytes.</param>
/// <param name="props">The segment properties.</param>
/// <param name="features">The user features.</param>
/// <returns>The plan.</returns>
let private planOfGsub (bytes: byte array) (props: SegmentProperties) (features: Feature list) : OtShapePlan =
    let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Gsub, bytes ])) 0u
    let font = Font.create face
    OtPlan.create font props (ImmutableArray.CreateRange features)

/// <summary>Builds a minimal GSUB with one DFLT script, a default language system and one lookup.</summary>
/// <param name="featureTags">The feature tags, each pointing at lookup zero.</param>
/// <param name="requiredFeature">The required feature index, or 0xFFFF for none.</param>
/// <returns>The GSUB bytes.</returns>
let private minimalGsub (featureTags: Tag array) (requiredFeature: uint16) : byte array =
    let n = featureTags.Length
    let langSys =
        Array.concat
            [ be16 0u
              be16 (uint32 requiredFeature)
              be16 (uint32 n)
              yield! Array.map (fun index -> be16 (uint32 index)) [| 0 .. n - 1 |] ]
    let scriptTable = Array.concat [ be16 4u; be16 0u ]
    let scriptList = Array.concat [ be16 1u; be32 (uint32 Tags.Dflt); be16 8u; scriptTable; langSys ]
    let featureRecords =
        Array.mapi (fun index tag ->
            Array.concat [ be32 (uint32 tag); be16 (uint32 (2 + 6 * n + 6 * index)) ]) featureTags
    let featureTables = Array.map (fun _ -> Array.concat [ be16 0u; be16 1u; be16 0u ]) featureTags
    let featureList = Array.concat (Array.append [| be16 (uint32 n) |] (Array.append featureRecords featureTables))
    let lookup = Array.concat [ be16 1u; be16 0u; be16 0u ]
    let lookupList = Array.concat [ be16 1u; be16 4u; lookup ]
    Array.concat
        [ be32 0x00010000u
          be16 10u
          be16 (uint32 (28 + 2 * n))
          be16 (uint32 (30 + 14 * n))
          scriptList
          featureList
          lookupList ]

/// <summary>The latin-horizontal segment properties.</summary>
let private latinLtr = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }

/// <summary>The P5.S04 OpenType map and plan tests.</summary>
[<Tests>]
let tests =
    testList "P5.S04" [
        test "feature tags ltr" {
            let plan = planOf "test/api/fonts/NotoSans-Bold.ttf" Direction.Ltr Scripts.Latin []
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'a' 'b' 'v' 'm'
                  Tag.make 'c' 'c' 'm' 'p'
                  Tag.make 'd' 'i' 's' 't'
                  Tag.make 'd' 'n' 'o' 'm'
                  Tag.make 'f' 'r' 'a' 'c'
                  Tag.make 'k' 'e' 'r' 'n'
                  Tag.make 'l' 'i' 'g' 'a'
                  Tag.make 'm' 'a' 'r' 'k'
                  Tag.make 'm' 'k' 'm' 'k'
                  Tag.make 'n' 'u' 'm' 'r' ]
                "LTR features"
        }
        test "feature tags ttb" {
            let plan = planOf "test/api/fonts/Mplus1p-Regular.ttf" Direction.Ttb Scripts.Han []
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'c' 'c' 'm' 'p'
                  Tag.make 'm' 'a' 'r' 'k'
                  Tag.make 'm' 'k' 'm' 'k'
                  Tag.make 'v' 'e' 'r' 't' ]
                "TTB features"
        }
        test "feature tags userfeatures enable" {
            let feature = { Tag = Tag.make 's' 's' '0' '1'; Value = 1u; Start = 0u; End = 1u }
            let plan = planOf "test/api/fonts/Mada-VF.ttf" Direction.Ltr Scripts.Latin [ feature ]
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'c' 'c' 'm' 'p'
                  Tag.make 'k' 'e' 'r' 'n'
                  Tag.make 'm' 'a' 'r' 'k'
                  Tag.make 'm' 'k' 'm' 'k'
                  Tag.make 'r' 'c' 'l' 't'
                  Tag.make 'r' 'l' 'i' 'g'
                  Tag.make 's' 's' '0' '1' ]
                "enable ss01"
        }
        test "feature tags userfeatures disable" {
            let feature = { Tag = Tag.make 'k' 'e' 'r' 'n'; Value = 0u; Start = 0u; End = Feature.GlobalEnd }
            let plan = planOf "test/api/fonts/Mada-VF.ttf" Direction.Ltr Scripts.Latin [ feature ]
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'c' 'c' 'm' 'p'
                  Tag.make 'm' 'a' 'r' 'k'
                  Tag.make 'm' 'k' 'm' 'k'
                  Tag.make 'r' 'c' 'l' 't'
                  Tag.make 'r' 'l' 'i' 'g' ]
                "disable kern"
        }
        test "feature tags userfeatures disablepartial" {
            let feature = { Tag = Tag.make 'k' 'e' 'r' 'n'; Value = 0u; Start = 0u; End = 1u }
            let plan = planOf "test/api/fonts/Mada-VF.ttf" Direction.Ltr Scripts.Latin [ feature ]
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'c' 'c' 'm' 'p'
                  Tag.make 'k' 'e' 'r' 'n'
                  Tag.make 'm' 'a' 'r' 'k'
                  Tag.make 'm' 'k' 'm' 'k'
                  Tag.make 'r' 'c' 'l' 't'
                  Tag.make 'r' 'l' 'i' 'g' ]
                "disable kern partially"
        }
        test "feature tags userfeatures disablenondefault" {
            let feature = { Tag = Tag.make 's' 's' '0' '1'; Value = 0u; Start = 0u; End = Feature.GlobalEnd }
            let plan = planOf "test/api/fonts/Mada-VF.ttf" Direction.Ltr Scripts.Latin [ feature ]
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'c' 'c' 'm' 'p'
                  Tag.make 'k' 'e' 'r' 'n'
                  Tag.make 'm' 'a' 'r' 'k'
                  Tag.make 'm' 'k' 'm' 'k'
                  Tag.make 'r' 'c' 'l' 't'
                  Tag.make 'r' 'l' 'i' 'g' ]
                "disable ss01"
        }
        test "bit exhaustion" {
            let tags = Array.map (fun c -> Tag.make c c c c) [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j' |]
            let gsub = minimalGsub tags 0xFFFFus
            let features =
                [ for c in 'a' .. 'j' -> { Tag = Tag.make c c c c; Value = 255u; Start = 0u; End = 1u } ]
            let plan = planOfGsub gsub latinLtr features
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'a' 'a' 'a' 'a'
                  Tag.make 'b' 'b' 'b' 'b'
                  Tag.make 'c' 'c' 'c' 'c'
                  Tag.make 'k' 'e' 'r' 'n' ]
                "only the first three value-255 features fit, plus the kern fallback"
        }
        test "duplicate global and ranged feature merge" {
            let gsub = minimalGsub [| Tag.make 'k' 'k' 'k' 'k' |] 0xFFFFus
            let globalK = { Tag = Tag.make 'k' 'k' 'k' 'k'; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            let rangedK = { Tag = Tag.make 'k' 'k' 'k' 'k'; Value = 3u; Start = 0u; End = 2u }
            let plan = planOfGsub gsub latinLtr [ globalK; rangedK ]
            Expect.equal
                (LayoutApi.planFeatureTags plan |> List.ofSeq)
                [ Tag.make 'k' 'e' 'r' 'n'; Tag.make 'k' 'k' 'k' 'k' ]
                "merged tag present"
            Expect.equal (OtMap.mask plan.Map (Tag.make 'k' 'k' 'k' 'k')) (struct (0x30u, 4)) "two-bit mask at shift 4"
            Expect.equal (OtMap.mask1 plan.Map (Tag.make 'k' 'k' 'k' 'k')) 0x10u "value-1 mask"
        }
        test "required feature stage" {
            let gsub = minimalGsub [| Tag.make 'r' 'q' 'd' ' ' |] 0us
            let plan = planOfGsub gsub latinLtr []
            Expect.equal (LayoutApi.planFeatureTags plan |> List.ofSeq) [ Tag.make 'k' 'e' 'r' 'n' ] "only the kern fallback"
            Expect.equal (LayoutApi.planCollectLookups plan Tags.Gsub |> List.ofSeq) [ 0 ] "required lookup collected"
        }
    ]
