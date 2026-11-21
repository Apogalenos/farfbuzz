module Farfbuzz.Diff.OtTagDiff

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>Converts tags to unsigned values for comparison.</summary>
/// <param name="tags">The tags.</param>
/// <returns>The values.</returns>
let private toU32 (tags: ImmutableArray<Tag>) =
    tags |> Seq.map (fun tag -> uint32 tag) |> Seq.toArray

/// <summary>Reads the oracle script and language tags.</summary>
/// <param name="script">The script.</param>
/// <param name="language">The language handle.</param>
/// <param name="scriptCapacity">The script capacity.</param>
/// <param name="languageCapacity">The language capacity.</param>
/// <returns>The script and language tags.</returns>
let private oracleTags (script: uint32) (language: nativeint) (scriptCapacity: int) (languageCapacity: int) =
    let scriptTags = Array.zeroCreate scriptCapacity
    let languageTags = Array.zeroCreate languageCapacity
    let mutable scriptCount = uint32 scriptCapacity
    let mutable languageCount = uint32 languageCapacity
    Native.hb_ot_tags_from_script_and_language(script, language, &scriptCount, scriptTags, &languageCount, languageTags)
    struct (scriptTags[0 .. int scriptCount - 1], languageTags[0 .. int languageCount - 1])

/// <summary>Reads the oracle language handle for a string.</summary>
/// <param name="text">The input.</param>
/// <returns>The handle.</returns>
let private oracleLanguageHandle (text: string) =
    let bytes = Encoding.UTF8.GetBytes text
    Native.hb_language_from_string(bytes, bytes.Length)

/// <summary>Reads the oracle language string of a handle.</summary>
/// <param name="handle">The handle.</param>
/// <returns>The string, or none.</returns>
let private oracleLanguageString (handle: nativeint) =
    match handle with
    | 0n -> ValueNone
    | _ ->
        match Native.hb_language_to_string handle with
        | 0n -> ValueNone
        | pointer ->
            match Marshal.PtrToStringUTF8 pointer with
            | null -> ValueNone
            | text -> ValueSome text

/// <summary>Every language subtag in the pinned IANA registry.</summary>
let private registryValues =
    let path = Path.Combine(RepositoryPaths.root (), "tools", "data", "vendored", "language-subtag-registry")
    File.ReadAllLines path
    |> Array.choose (fun line ->
        let trimmed = line.Trim()
        match trimmed.StartsWith "Subtag:", trimmed.StartsWith "Tag:" with
        | true, _ -> Some (trimmed.Substring(7).Trim())
        | _, true -> Some (trimmed.Substring(4).Trim())
        | _ -> None)

/// <summary>The language inputs for the tags-from-language differential.</summary>
let private languageInputs =
    seq {
        for value in registryValues do
            yield value
            yield value + "-latn"
            yield value + "-hant-hk"
            yield value + "-x-hbot-41424344"
            yield value + "-x-hbsc-6c61746e"
            yield value + "-polyton"
            yield value + "-fonipa"
    }

/// <summary>A deterministic set of random tags.</summary>
/// <param name="count">The number of tags.</param>
/// <returns>The tags.</returns>
let private randomTags (count: int) =
    let random = Random(0x1234_5678)
    Array.init count (fun _ -> uint32 (random.Next()))

/// <summary>The script inputs for the tags-from-script differential.</summary>
let private scriptInputs =
    seq {
        yield! Scripts.all
        yield Scripts.Invalid
        yield! randomTags 100 |> Seq.map LanguagePrimitives.UInt32WithMeasure
    }

/// <summary>The tag inputs for the tag-to-script differential.</summary>
let private tagToScriptInputs =
    let fromScriptTags =
        scriptInputs
        |> Seq.collect (fun script ->
            let struct (tags, _) = OtTag.tagsFromScriptAndLanguage script Language.NoLanguage 3 0
            tags |> Seq.map uint32)
    let suffixForms =
        Scripts.all
        |> Seq.collect (fun script ->
            let base3 = (uint32 script ||| 0x20202000u) &&& 0xFFFFFF00u
            seq {
                yield base3 ||| 0x32u
                yield base3 ||| 0x33u
            })
    Seq.append fromScriptTags suffixForms |> Seq.distinct

/// <summary>The tag inputs for the tag-to-language differential.</summary>
let private tagToLanguageInputs =
    let generated =
        seq {
            yield! OtTagTable.languages2 |> Seq.map (fun item -> uint32 item.OtTag)
            yield! OtTagTable.languages3 |> Seq.map (fun item -> uint32 item.OtTag)
            yield! OtTagTable.languages3MultiValues |> Seq.map uint32
            yield! OtTagTable.languages3Blocked |> Seq.map uint32
            yield! OtTagTable.ambiguous |> Seq.map (fun struct (tag, _) -> uint32 tag)
            yield! OtTagTable.complexRules |> Seq.collect (fun rule -> rule.Tags |> Seq.map uint32)
        }
    Seq.append generated (randomTags 1000 |> Seq.ofArray) |> Seq.distinct

/// <summary>A random script-and-language tag pair.</summary>
let private tagPair =
    Gen.zip (Gen.choose (0, Int32.MaxValue) |> Gen.map uint32) (Gen.choose (0, Int32.MaxValue) |> Gen.map uint32)

/// <summary>The P3.S12 OpenType tag differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S12" [
        exhaustive "tags from language" languageInputs
            (fun text ->
                let struct (s3, l3) = OtTag.tagsFromScriptAndLanguage Scripts.Latin (Language.ofString text) 3 3
                let struct (s1, l1) = OtTag.tagsFromScriptAndLanguage Scripts.Latin (Language.ofString text) 1 1
                (toU32 s3, toU32 l3, toU32 s1, toU32 l1))
            (fun text ->
                let handle = oracleLanguageHandle text
                let struct (s3, l3) = oracleTags (uint32 Scripts.Latin) handle 3 3
                let struct (s1, l1) = oracleTags (uint32 Scripts.Latin) handle 1 1
                (s3, l3, s1, l1))
        exhaustive "tags from script" scriptInputs
            (fun script ->
                let struct (scriptTags, _) = OtTag.tagsFromScriptAndLanguage script Language.NoLanguage 3 0
                toU32 scriptTags)
            (fun script ->
                let struct (scriptTags, _) = oracleTags (uint32 script) 0n 3 0
                scriptTags)
        exhaustive "tag to script" tagToScriptInputs
            (fun tag -> uint32 (OtTag.tagToScript (LanguagePrimitives.UInt32WithMeasure tag)))
            (fun tag -> Native.hb_ot_tag_to_script tag)
        exhaustive "tag to language" tagToLanguageInputs
            (fun tag -> Language.toString (OtTag.tagToLanguage (LanguagePrimitives.UInt32WithMeasure tag)))
            (fun tag -> oracleLanguageString (Native.hb_ot_tag_to_language tag))
        sampled "tags to script and language" tagPair 10_000
            (fun (scriptTag, languageTag) ->
                let struct (script, language) =
                    OtTag.tagsToScriptAndLanguage
                        (LanguagePrimitives.UInt32WithMeasure scriptTag)
                        (LanguagePrimitives.UInt32WithMeasure languageTag)
                struct (uint32 script, Language.toString language))
            (fun (scriptTag, languageTag) ->
                let mutable script = 0u
                let mutable language = 0n
                Native.hb_ot_tags_to_script_and_language(scriptTag, languageTag, &script, &language)
                struct (script, oracleLanguageString language))
    ]
