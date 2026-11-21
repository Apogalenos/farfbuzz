module Farfbuzz.Diff.AatMapDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The four bytes.</returns>
let private be32 (value: uint32) =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Reads a big-endian uint16 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU16 (bytes: byte array) (at: int) =
    uint16 ((int bytes[at] <<< 8) ||| int bytes[at + 1])

/// <summary>Reads a big-endian uint32 from bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <returns>The value.</returns>
let private readU32 (bytes: byte array) (at: int) =
    (uint32 bytes[at] <<< 24) ||| (uint32 bytes[at + 1] <<< 16) ||| (uint32 bytes[at + 2] <<< 8) ||| uint32 bytes[at + 3]

/// <summary>Builds a bare sfnt from its tables.</summary>
/// <param name="tables">The tables.</param>
/// <returns>The file bytes.</returns>
let private sfnt (tables: (Tag * byte array) list) : byte array =
    let headerSize = 12 + 16 * tables.Length
    let header =
        Array.concat
            [ be32 0x00010000u
              be16 (uint32 tables.Length)
              be16 0u
              be16 0u
              be16 0u ]
    let records, _ =
        tables
        |> List.fold (fun (records, offset) (tag, bytes) ->
            let record = Array.concat [ be32 (uint32 tag); be32 0u; be32 (uint32 offset); be32 (uint32 bytes.Length) ]
            record :: records, offset + bytes.Length) ([], headerSize)
    Array.concat (header :: List.rev records @ (tables |> List.map snd))

/// <summary>Builds an ltag table from its tag strings.</summary>
/// <param name="tags">The tag strings.</param>
/// <returns>The table bytes.</returns>
let private ltagOf (tags: string list) : byte array =
    let headerSize = 12
    let rangeSize = 4
    let rangesStart = headerSize + rangeSize * tags.Length
    let stringBytes = tags |> List.map Text.Encoding.ASCII.GetBytes
    let stringStart, _ =
        stringBytes |> List.mapFold (fun offset bytes -> offset, offset + bytes.Length) rangesStart
    let ranges =
        tags
        |> List.mapi (fun index tag -> Array.concat [ be16 (uint32 stringStart[index]); be16 (uint32 tag.Length) ])
        |> Array.concat
    Array.concat [ be32 1u; be32 0u; be32 (uint32 tags.Length); ranges; stringBytes |> Array.concat ]

/// <summary>Builds a feat table from its feature types.</summary>
/// <param name="features">Each feature type, setting count and exclusivity.</param>
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

/// <summary>Builds a morx table from its chains.</summary>
/// <param name="chains">Each chain's default flags and feature records.</param>
/// <returns>The table bytes.</returns>
let private morxOf (chains: (uint32 * (uint16 * uint16 * uint32 * uint32) list) list) : byte array =
    let chainBytes (defaultFlags, features) =
        let minSize = 16
        let featureBytes =
            features
            |> List.map (fun (kind, setting, enable, disable) ->
                Array.concat [ be16 (uint32 kind); be16 (uint32 setting); be32 enable; be32 disable ])
            |> Array.concat
        let length = minSize + featureBytes.Length
        Array.concat [ be32 defaultFlags; be32 (uint32 length); be32 (uint32 features.Length); be32 0u; featureBytes ]
    Array.concat [ be16 1u; be16 0u; be32 (uint32 chains.Length); chains |> List.map chainBytes |> Array.concat ]

/// <summary>A synthetic face with feat, ltag and morx, exposing the types the feature lists use.</summary>
let private syntheticFaceBytes : byte array =
    let feat = featOf [ (1us, 4us, false); (3us, 4us, false); (17us, 2us, true); (37us, 2us, false) ]
    let morx =
        morxOf
            [ (0u,
               [ (1us, 2us, 0x1u, 0xFFFFFFFEu)
                 (1us, 3us, 0x2u, 0xFFFFFFFDu)
                 (17us, 0us, 0x4u, 0xFFFFFFFBu)
                 (17us, 1us, 0x8u, 0xFFFFFFF7u)
                 (37us, 1us, 0x10u, 0xFFFFFFEFu)
                 (3us, 3us, 0x20u, 0xFFFFFFDFu)
                 (39us, 1us, 0x40u, 0xFFFFFFBFu)
                 (39us, 2us, 0x80u, 0xFFFFFF7Fu) ]) ]
    let ltag = ltagOf [ "sr"; "zh-Hant" ]
    sfnt [ Tags.Feat, feat; Tags.Ltag, ltag; Tags.Morx, morx ]

/// <summary>Creates a Farfbuzz face from bytes.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The accelerators.</returns>
let private otFaceOf (bytes: byte array) =
    OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)

/// <summary>The language tags the chain-flags differential draws from.</summary>
let private languages : string array =
    [| "c"; "en"; "sr"; "SR"; "sr-Latn-RS"; "srb"; "s"; "zh-hant"; "ZH_HANT"; "zh"; "" |]

/// <summary>Interns a language through the oracle.</summary>
/// <param name="text">The canonical language string.</param>
/// <returns>The language handle, or zero for an empty string.</returns>
let private oracleLanguage (text: string) : nativeint =
    match text.Length with
    | 0 -> Native.hb_language_from_string(Array.empty, 0)
    | _ -> Native.hb_language_from_string(Text.Encoding.UTF8.GetBytes text, text.Length)

/// <summary>Parses the serialized oracle map back into per-chain flag ranges.</summary>
/// <param name="out">The serialized map.</param>
/// <returns>The chain flags.</returns>
let private parseMap (out: uint32 array) : ImmutableArray<ImmutableArray<RangeFlags>> =
    let chainCount = int out[0]
    let chains = Array.zeroCreate<ImmutableArray<RangeFlags>> chainCount
    let mutable pos = 1
    for chainIndex in 0 .. chainCount - 1 do
        let rangeCount = int out[pos]
        pos <- pos + 1
        let ranges = Array.zeroCreate<RangeFlags> rangeCount
        for rangeIndex in 0 .. rangeCount - 1 do
            ranges[rangeIndex] <- { Flags = out[pos]; ClusterFirst = out[pos + 1]; ClusterLast = out[pos + 2] }
            pos <- pos + 3
        chains[chainIndex] <- ImmutableArray.CreateRange ranges
    ImmutableArray.CreateRange chains

/// <summary>Compiles the map through the oracle shim.</summary>
/// <param name="bytes">The face bytes.</param>
/// <param name="language">The language string.</param>
/// <param name="features">The user features.</param>
/// <returns>The chain flags.</returns>
let private oracleMap (bytes: byte array) (language: string) (features: Feature array) : ImmutableArray<ImmutableArray<RangeFlags>> =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let languageHandle = oracleLanguage language
    let size = Shims.farfbuzz_oracle_aat_map_compile(face, 4u, 0x6C61746Eu, languageHandle, features, uint32 features.Length, Array.empty, 0u)
    let out = Array.zeroCreate (int size)
    Shims.farfbuzz_oracle_aat_map_compile(face, 4u, 0x6C61746Eu, languageHandle, features, uint32 features.Length, out, uint32 out.Length) |> ignore
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    parseMap out

/// <summary>Compiles the map through Farfbuzz.</summary>
/// <param name="bytes">The face bytes.</param>
/// <param name="language">The language string.</param>
/// <param name="features">The user features.</param>
/// <returns>The chain flags.</returns>
let private ourMap (bytes: byte array) (language: string) (features: Feature array) : ImmutableArray<ImmutableArray<RangeFlags>> =
    let segment =
        { Direction = Ltr
          Script = 0x6C61746Eu<script>
          Language = Language.ofString language }
    (AatMap.build (otFaceOf bytes) segment (ReadOnlySpan features)).ChainFlags

/// <summary>One chain-flags case.</summary>
type private ChainFlagsInput =
    { Bytes: byte array
      Features: Feature array
      Language: string }

/// <summary>Every OpenType tag of the mapping table.</summary>
let private mappingTags : uint32 array =
    AatFeatureMappings.mappings |> Seq.map (fun mapping -> uint32 mapping.OtFeatureTag) |> Seq.toArray

/// <summary>Farfbuzz's feature-mapping result.</summary>
/// <param name="tag">The tag.</param>
/// <returns>The found flag and the three values.</returns>
let private ourMapping (tag: uint32) : struct (bool * uint32 * uint32 * uint32) =
    match AatFeatureMappings.find (LanguagePrimitives.UInt32WithMeasure tag) with
    | ValueNone -> struct (false, 0u, 0u, 0u)
    | ValueSome mapping -> struct (true, uint32 mapping.AatFeatureType, uint32 mapping.SelectorToEnable, uint32 mapping.SelectorToDisable)

/// <summary>The oracle's feature-mapping result.</summary>
/// <param name="tag">The tag.</param>
/// <returns>The found flag and the three values.</returns>
let private oracleMapping (tag: uint32) : struct (bool * uint32 * uint32 * uint32) =
    let mutable featureType = 0u
    let mutable enable = 0u
    let mutable disable = 0u
    match Shims.farfbuzz_oracle_aat_find_feature_mapping(tag, &featureType, &enable, &disable) with
    | 0 -> struct (false, 0u, 0u, 0u)
    | _ -> struct (true, featureType, enable, disable)

/// <summary>Generates a full-width uint32 tag.</summary>
/// <returns>The generator.</returns>
let private genRandomTag =
    gen {
        let! high = Gen.choose (0, 0xFFFF)
        let! low = Gen.choose (0, 0xFFFF)
        return (uint32 high <<< 16) ||| uint32 low
    }

/// <summary>Every face with a morx or mort table.</summary>
let private aatFaces : struct (TestFont * uint32) array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        Morx.hasData (Morx.load face false) || Morx.hasData (Morx.load face true))
    |> Seq.toArray

/// <summary>The feature tags the chain-flags differential draws from.</summary>
let private featureTags : Tag array =
    [| Tag.make 'l' 'i' 'g' 'a'
       Tag.make 's' 'm' 'c' 'p'
       Tag.make 'a' 'a' 'l' 't'
       Tag.make 'c' 'l' 'i' 'g'
       Tag.make 'l' 'n' 'u' 'm'
       Tag.make 'f' 'r' 'a' 'c' |]

/// <summary>Generates one user-feature list of the requested kinds.</summary>
/// <returns>The generator.</returns>
let private genFeatures =
    gen {
        let! kind = Gen.choose (0, 6)
        match kind with
        | 0 -> return [||]
        | 1 ->
            let! tag = Gen.elements featureTags
            return [| { Tag = tag; Value = 1u; Start = 0u; End = UInt32.MaxValue } |]
        | 2 ->
            let! tag = Gen.elements featureTags
            return [| { Tag = tag; Value = 1u; Start = 0u; End = 10u }; { Tag = tag; Value = 1u; Start = 5u; End = 15u } |]
        | 3 ->
            let! tag = Gen.elements featureTags
            return [| { Tag = tag; Value = 1u; Start = 0u; End = 10u }; { Tag = tag; Value = 0u; Start = 0u; End = 10u } |]
        | 4 -> return [| { Tag = Tag.make 'a' 'a' 'l' 't'; Value = 2u; Start = 0u; End = UInt32.MaxValue } |]
        | 5 -> return [| { Tag = Tag.make 's' 'm' 'c' 'p'; Value = 1u; Start = 0u; End = UInt32.MaxValue } |]
        | _ ->
            let! tag = Gen.elements featureTags
            return [| { Tag = tag; Value = 1u; Start = 3u; End = 9u } |]
    }

/// <summary>Generates a chain-flags case over a synthetic or a real face.</summary>
let private genChainFlags =
    gen {
        let! synthetic = Gen.choose (0, 1)
        let! features = genFeatures
        let! language = Gen.elements languages
        match synthetic with
        | 0 ->
            let! struct (font, _) = Gen.elements aatFaces
            return { Bytes = OracleFaces.data font.Path; Features = features; Language = language }
        | _ -> return { Bytes = syntheticFaceBytes; Features = features; Language = language }
    }

/// <summary>One field mutation.</summary>
type private Mutation =
    { Name: string
      At: int
      Replacement: byte array }

/// <summary>The feature list and language used by the mutated-table differential.</summary>
let private mutatedFeatures : Feature array =
    [| { Tag = Tag.make 'l' 'i' 'g' 'a'; Value = 1u; Start = 0u; End = UInt32.MaxValue }
       { Tag = Tag.make 's' 'm' 'c' 'p'; Value = 1u; Start = 0u; End = UInt32.MaxValue } |]

/// <summary>Rewrites bytes at an offset, returning a fresh array.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="replacement">The replacement bytes.</param>
/// <returns>The mutated bytes.</returns>
let private mutateAt (bytes: byte array) (at: int) (replacement: byte array) : byte array =
    let mutated = Array.copy bytes
    for i in 0 .. replacement.Length - 1 do
        mutated[at + i] <- replacement[i]
    mutated

/// <summary>Compares one mutated face against the oracle.</summary>
/// <param name="bytes">The mutated file bytes.</param>
/// <param name="name">The mutation name.</param>
/// <param name="failures">The reported differences.</param>
/// <param name="compared">The compared count.</param>
let private compareMutated (bytes: byte array) (name: string) (failures: ResizeArray<string>) (compared: int ref) =
    let ours = ourMap bytes "c" mutatedFeatures
    let oracle = oracleMap bytes "c" mutatedFeatures
    compared.Value <- compared.Value + 1
    match ours = oracle with
    | true -> ()
    | false -> failures.Add($"{name}: oracle %A{oracle}, farfbuzz %A{ours}")

/// <summary>The P8.S05 AAT-map differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P8.S05" [
        exhaustive "mapping table" mappingTags ourMapping oracleMapping
        sampled "random tags" genRandomTag 1000 ourMapping oracleMapping
        sampled "chain flags" genChainFlags 5000
            (fun input -> ourMap input.Bytes input.Language input.Features)
            (fun input -> oracleMap input.Bytes input.Language input.Features)
        testCase "mutated feat and chains" (fun () ->
            let failures = ResizeArray<string>()
            let compared = ref 0
            let bytes = syntheticFaceBytes
            let featAbs = int (readU32 bytes (12 + 8))
            let morxAbs = int (readU32 bytes (12 + 2 * 16 + 8))
            for replacement in [| 0u; 1u; 3u; 5u; 0xFFFFu |] do
                compareMutated (mutateAt bytes (featAbs + 4) (be16 replacement)) "feat feature name count" failures compared
            let nameAt = featAbs + 12
            for replacement in [| 0u; 0x7FFFFFFFu; 0xFFFFFFFFu |] do
                compareMutated (mutateAt bytes (nameAt + 4) (be32 replacement)) "feat setting table offset" failures compared
            for replacement in [| 0u; 0x8000u |] do
                compareMutated (mutateAt bytes (nameAt + 8) (be16 replacement)) "feat exclusive bit" failures compared
            let chainAbs = morxAbs + 8
            for replacement in [| 0u; 1u; 7u; 9u; 0xFFFFFFFFu |] do
                compareMutated (mutateAt bytes (chainAbs + 8) (be32 replacement)) "chain feature count" failures compared
            for replacement in [| 0u; 0xFFFFFFFFu |] do
                compareMutated (mutateAt bytes chainAbs (be32 replacement)) "chain default flags" failures compared
            let featureAt = chainAbs + 16
            for replacement in [| 0u; 0xFFFFu |] do
                compareMutated (mutateAt bytes featureAt (be16 replacement)) "feature record type" failures compared
                compareMutated (mutateAt bytes (featureAt + 2) (be16 replacement)) "feature record setting" failures compared
            let morxLengthField = 12 + 2 * 16 + 12
            for replacement in [| 20u; 21u; 22u; 23u |] do
                compareMutated (mutateAt bytes morxLengthField (be32 replacement)) "truncated morx tail" failures compared
            printfn "P8.S05 compared %d mutated feat and chain fields" compared.Value
            Expect.isEmpty failures ("mutated-map differences:\n" + String.concat "\n" failures))
    ]
