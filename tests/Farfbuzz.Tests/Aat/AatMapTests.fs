module Farfbuzz.Tests.AatMapTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Tests.SfntBuilder

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The four bytes.</returns>
let private be32 (value: uint32) = SfntBuilder.be32 value

/// <summary>Builds an ltag table from its tag strings.</summary>
/// <param name="tags">The tag strings.</param>
/// <returns>The table bytes.</returns>
let private ltagOf (tags: string list) : byte array =
    let headerSize = 12
    let rangeSize = 4
    let rangesStart = headerSize + rangeSize * tags.Length
    let stringBytes = tags |> List.map System.Text.Encoding.ASCII.GetBytes
    let stringStart, _ =
        stringBytes
        |> List.mapFold (fun offset bytes -> offset, offset + bytes.Length) rangesStart
    let ranges =
        tags
        |> List.mapi (fun index tag ->
            Array.concat [ be16 (uint32 stringStart[index]); be16 (uint32 tag.Length) ])
        |> Array.concat
    Array.concat
        [ be32 1u
          be32 0u
          be32 (uint32 tags.Length)
          ranges
          stringBytes |> Array.concat ]

/// <summary>Builds a feat table from its feature types.</summary>
/// <param name="features">Each feature type, setting count and whether it is exclusive.</param>
/// <returns>The table bytes.</returns>
let private featOf (features: (uint16 * uint16 * bool) list) : byte array =
    let headerSize = 12
    let nameSize = 12
    let settingSize = 4
    let namesStart = headerSize + nameSize * features.Length
    let settingOffsets =
        features
        |> List.mapFold (fun offset (_, nSettings, _) -> offset, offset + int nSettings * settingSize) namesStart
        |> fst
    let names =
        features
        |> List.mapi (fun index (featureType, nSettings, isExclusive) ->
            Array.concat
                [ be16 (uint32 featureType)
                  be16 (uint32 nSettings)
                  be32 (uint32 settingOffsets[index])
                  be16 (if isExclusive then 0x8000u else 0u)
                  be16 256u ])
        |> Array.concat
    let settings =
        features
        |> List.collect (fun (_, nSettings, _) ->
            [ for setting in 0 .. int nSettings - 1 -> Array.concat [ be16 (uint32 setting); be16 300u ] ])
        |> Array.concat
    Array.concat
        [ be32 0x00010000u
          be16 (uint32 features.Length)
          be16 0u
          be32 0u
          names
          settings ]

/// <summary>Builds a morx or mort table from its chains.</summary>
/// <param name="isMort">True for the mort (16-bit) form.</param>
/// <param name="chains">Each chain's default flags and feature records.</param>
/// <returns>The table bytes.</returns>
let private morxOf (isMort: bool) (chains: (uint32 * (uint16 * uint16 * uint32 * uint32) list) list) : byte array =
    let hbuintSize = if isMort then 2 else 4
    let writeHbuint (value: uint32) = if isMort then be16 value else be32 value
    let chainBytes (defaultFlags, features) =
        let minSize = 8 + 2 * hbuintSize
        let featureBytes =
            features
            |> List.map (fun (kind, setting, enable, disable) ->
                Array.concat [ be16 (uint32 kind); be16 (uint32 setting); be32 enable; be32 disable ])
            |> Array.concat
        let length = minSize + featureBytes.Length
        Array.concat
            [ be32 defaultFlags
              be32 (uint32 length)
              writeHbuint (uint32 features.Length)
              writeHbuint 0u
              featureBytes ]
    Array.concat
        [ be16 1u
          be16 0u
          be32 (uint32 chains.Length)
          chains |> List.map chainBytes |> Array.concat ]

/// <summary>Builds an OtFace from feat, ltag and morx or mort table bytes.</summary>
/// <param name="feat">The feat bytes.</param>
/// <param name="ltag">The ltag bytes.</param>
/// <param name="morx">The morx or mort bytes.</param>
/// <param name="mort">True when the bytes are a mort table.</param>
/// <returns>The face.</returns>
let private faceOf (feat: byte array) (ltag: byte array) (morx: byte array) (mort: bool) : OtFace =
    let tables =
        [ Tags.Feat, feat
          Tags.Ltag, ltag
          (if mort then Tags.Mort else Tags.Morx), morx ]
    let file = sfnt 0x00010000u tables
    OtFace.create (Face.create (ReadOnlyMemory file) 0u)

/// <summary>The P8.S05 feat, ltag and AAT-map unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S05" [
        test "mapping table" {
            let mappings = AatFeatureMappings.mappings
            Expect.equal mappings.Length 78 "HarfBuzz 15.1.0 has 78 mappings"
            let tags = mappings |> Seq.map (fun mapping -> uint32 mapping.OtFeatureTag) |> Seq.toArray
            Expect.sequenceEqual tags (Array.sort tags) "the mappings are sorted by tag"
            Expect.equal (Tag.toString mappings[0].OtFeatureTag) "afrc" "the first mapping"
            Expect.equal (Tag.toString mappings[mappings.Length - 1].OtFeatureTag) "zero" "the last mapping"
            match AatFeatureMappings.find (Tag.make 'a' 'f' 'r' 'c') with
            | ValueNone -> Tests.failtest "afrc should map"
            | ValueSome mapping ->
                Expect.equal mapping.AatFeatureType 11us "afrc's feature type"
                Expect.equal mapping.SelectorToEnable 1us "afrc's enabling selector"
                Expect.equal mapping.SelectorToDisable 0us "afrc's disabling selector"
            match AatFeatureMappings.find (Tag.make 'x' 'x' 'x' 'x') with
            | ValueSome _ -> Tests.failtest "an absent tag should map to nothing"
            | ValueNone -> ()
        }
        test "events" {
            let feat =
                featOf [ (1us, 4us, false) (* ligatures *); (37us, 2us, false) (* lower case *) ]
            let morx =
                morxOf false
                    [ (0u,
                       [ (1us, 2us, 0x1u, 0xFFFFFFFEu) // ligatures, common on
                         (37us, 1us, 0x2u, 0xFFFFFFFDu) ]) ] // lower case, small caps
            let face = faceOf feat (ltagOf []) morx false
            let features =
                [| { Tag = Tag.make 'l' 'i' 'g' 'a'; Value = 1u; Start = 0u; End = 5u }
                   { Tag = Tag.make 's' 'm' 'c' 'p'; Value = 1u; Start = 2u; End = 7u } |]
            let map = AatMap.build face { Direction = Ltr; Script = 0u<script>; Language = NoLanguage } (ReadOnlySpan features)
            Expect.equal map.ChainFlags.Length 1 "one chain"
            let ranges = map.ChainFlags[0]
            Expect.equal
                (ranges |> Seq.map (fun range -> struct (range.Flags, range.ClusterFirst, range.ClusterLast)) |> Seq.toArray)
                [| struct (0x1u, 0u, 1u)
                   struct (0x3u, 2u, 4u)
                   struct (0x2u, 5u, 6u)
                   struct (0x0u, 7u, UInt32.MaxValue) |]
                "the overlapping ranges compile to four flag ranges"
        }
        test "ltag sr" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isTrue (Ltag.matches ltag 0 (Language.ofString "sr")) "sr matches the sr tag"
            Expect.isFalse (Ltag.matches ltag 1 (Language.ofString "sr")) "sr does not match zh-Hant"
        }
        test "ltag SR" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isTrue (Ltag.matches ltag 0 (Language.ofString "SR")) "SR matches the sr tag"
        }
        test "ltag sr-Latn-RS" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isTrue (Ltag.matches ltag 0 (Language.ofString "sr-Latn-RS")) "sr-Latn-RS matches the sr tag"
        }
        test "ltag srb" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isFalse (Ltag.matches ltag 0 (Language.ofString "srb")) "srb does not match the sr tag"
        }
        test "ltag s" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isFalse (Ltag.matches ltag 0 (Language.ofString "s")) "s does not match the sr tag"
        }
        test "ltag en" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isFalse (Ltag.matches ltag 0 (Language.ofString "en")) "en does not match the sr tag"
        }
        test "ltag zh-hant" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isTrue (Ltag.matches ltag 1 (Language.ofString "zh-hant")) "zh-hant matches the zh-Hant tag"
        }
        test "ltag ZH_HANT" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isTrue (Ltag.matches ltag 1 (Language.ofString "ZH_HANT")) "ZH_HANT matches the zh-Hant tag"
        }
        test "ltag zh" {
            let ltag = { Ltag.Table = ReadOnlyMemory (ltagOf [ "sr"; "zh-Hant" ]); Count = 2 }
            Expect.isFalse (Ltag.matches ltag 1 (Language.ofString "zh")) "zh does not match the zh-Hant tag"
        }
    ]
