module Farfbuzz.Bench.UpstreamOt

open System
open System.Collections.Immutable
open System.Text
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Bench.NativeShaping

/// <summary>The number of OpenType tags a script may expand to, <c>HB_OT_MAX_TAGS_PER_SCRIPT</c>.</summary>
[<Literal>]
let private MaxTagsPerScript = 3

/// <summary>The number of OpenType tags a language may expand to, <c>HB_OT_MAX_TAGS_PER_LANGUAGE</c>.</summary>
[<Literal>]
let private MaxTagsPerLanguage = 3

/// <summary>One script/language pair of perf/benchmark-ot.cc.</summary>
type OtCase =
    { Name: string
      Script: Script
      Language: Language
      NativeLanguage: nativeint
      ScriptOut: uint32 array
      LanguageOut: uint32 array }
    override case.ToString() = case.Name

/// <summary>The 13 cases of benchmark-ot.cc:25-37, in order.</summary>
let private otCaseSpecs =
    [ "COMMON zh_abcd", Scripts.Common, "zh_abcd"
      "COMMON zh_hans", Scripts.Common, "zh_hans"
      "COMMON ab_abcd", Scripts.Common, "ab_abcd"
      "COMMON ab_abc", Scripts.Common, "ab_abc"
      "COMMON abcdef_XY", Scripts.Common, "abcdef_XY"
      "COMMON abcd_XY", Scripts.Common, "abcd_XY"
      "COMMON cxy_CN", Scripts.Common, "cxy_CN"
      "COMMON exy_CN", Scripts.Common, "exy_CN"
      "COMMON zh_CN", Scripts.Common, "zh_CN"
      "COMMON en_US", Scripts.Common, "en_US"
      "LATIN en_US", Scripts.Latin, "en_US"
      "COMMON none", Scripts.Latin, ""
      "LATIN none", Scripts.Latin, "" ]

/// <summary>The Farfbuzz language and the interned oracle language of a tag string.</summary>
/// <param name="text">The tag string, empty for no language.</param>
/// <returns>The language pair.</returns>
let private langOf (text: string) : struct (Language * nativeint) =
    match text.Length with
    | 0 -> struct (Language.NoLanguage, 0n)
    | _ ->
        let bytes = Encoding.UTF8.GetBytes text
        struct (Language.ofString text, Native.hb_language_from_string(bytes, bytes.Length))

/// <summary>Throws naming the case and the difference.</summary>
/// <param name="name">The case name.</param>
/// <param name="detail">The difference.</param>
let private differ (name: string) (detail: string) =
    raise (InvalidOperationException($"case {name}: tags differ: {detail}"))

/// <summary>Compares Farfbuzz tags with the oracle's for one kind of tag.</summary>
/// <param name="name">The case name.</param>
/// <param name="kind">The tag kind, script or language.</param>
/// <param name="ours">The Farfbuzz tags.</param>
/// <param name="theirs">The oracle tags.</param>
let private compareTags (name: string) (kind: string) (ours: ImmutableArray<Tag>) (theirs: uint32 array) : unit =
    match ours.Length = theirs.Length with
    | false -> differ name $"{kind} tag count Farfbuzz {ours.Length} HarfBuzz {theirs.Length}"
    | true ->
        for index in 0 .. ours.Length - 1 do
            match uint32 ours[index] = theirs[index] with
            | true -> ()
            | false -> differ name $"{kind} tag {index} Farfbuzz {uint32 ours[index]} HarfBuzz {theirs[index]}"

/// <summary>Checks the script and language tags of one case agree.</summary>
/// <param name="case">The case.</param>
let private checkOt (case: OtCase) : unit =
    let struct (oursScript, oursLanguage) = OtTag.tagsFromScriptAndLanguage case.Script case.Language MaxTagsPerScript MaxTagsPerLanguage
    let scriptTags = Array.zeroCreate MaxTagsPerScript
    let languageTags = Array.zeroCreate MaxTagsPerLanguage
    let mutable scriptCount = uint32 MaxTagsPerScript
    let mutable languageCount = uint32 MaxTagsPerLanguage
    Native.hb_ot_tags_from_script_and_language(uint32 case.Script, case.NativeLanguage, &scriptCount, scriptTags, &languageCount, languageTags)
    compareTags case.Name "script" oursScript (scriptTags[0 .. int scriptCount - 1])
    compareTags case.Name "language" oursLanguage (languageTags[0 .. int languageCount - 1])

/// <summary>Every case, built once with the equality check already run.</summary>
let private allCases : OtCase list =
    oracle.Force() |> ignore
    [ for (name, script, text) in otCaseSpecs do
        let struct (language, nativeLanguage) = langOf text
        let case =
            { Name = name
              Script = script
              Language = language
              NativeLanguage = nativeLanguage
              ScriptOut = Array.zeroCreate MaxTagsPerScript
              LanguageOut = Array.zeroCreate MaxTagsPerLanguage }
        checkOt case
        yield case ]

/// <summary>Runs the tag conversion once through native HarfBuzz.</summary>
/// <param name="case">The case.</param>
/// <returns>The tag checksum.</returns>
let private runNative (case: OtCase) : int =
    let mutable scriptCount = uint32 MaxTagsPerScript
    let mutable languageCount = uint32 MaxTagsPerLanguage
    Native.hb_ot_tags_from_script_and_language(uint32 case.Script, case.NativeLanguage, &scriptCount, case.ScriptOut, &languageCount, case.LanguageOut)
    let mutable total = 0
    for index in 0 .. int scriptCount - 1 do
        total <- total + int case.ScriptOut[index]
    for index in 0 .. int languageCount - 1 do
        total <- total + int case.LanguageOut[index]
    total

/// <summary>Runs the tag conversion once through Farfbuzz.</summary>
/// <param name="case">The case.</param>
/// <returns>The tag checksum.</returns>
let private runFarfbuzz (case: OtCase) : int =
    let struct (scriptTags, languageTags) = OtTag.tagsFromScriptAndLanguage case.Script case.Language MaxTagsPerScript MaxTagsPerLanguage
    let mutable total = 0
    for tag in scriptTags do
        total <- total + int (uint32 tag)
    for tag in languageTags do
        total <- total + int (uint32 tag)
    total

/// <summary>The 13 script/language tag conversions of perf/benchmark-ot.cc, Farfbuzz against native HarfBuzz 15.1.0.</summary>
[<MemoryDiagnoser>]
type UpstreamOt() =
    /// <summary>The cases.</summary>
    member _.Cases() : seq<obj> = allCases |> Seq.map (fun case -> case :> obj)

    /// <summary>Runs one conversion through native HarfBuzz.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The checksum.</returns>
    [<Benchmark(Baseline = true); ArgumentsSource("Cases")>]
    member _.Native(case: OtCase) : int = runNative case

    /// <summary>Runs one conversion through Farfbuzz.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The checksum.</returns>
    [<Benchmark; ArgumentsSource("Cases")>]
    member _.Farfbuzz(case: OtCase) : int = runFarfbuzz case
