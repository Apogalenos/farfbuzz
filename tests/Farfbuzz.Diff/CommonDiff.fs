module Farfbuzz.Diff.CommonDiff

open System
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

/// <summary>The oracle direction value for a string.</summary>
/// <param name="text">The input.</param>
/// <returns>The direction value.</returns>
let private oracleDirection (text: string) =
    let bytes = Encoding.UTF8.GetBytes text
    Native.hb_direction_from_string(bytes, bytes.Length)

/// <summary>The oracle script tag for an ISO 15924 tag.</summary>
/// <param name="tag">The tag.</param>
/// <returns>The script tag.</returns>
let private oracleScript (tag: uint32) =
    Native.hb_script_from_iso15924_tag tag

/// <summary>The oracle language handle for a string.</summary>
/// <param name="text">The input.</param>
/// <returns>The handle, or zero.</returns>
let private oracleLanguageHandle (text: string) =
    let bytes = Encoding.UTF8.GetBytes text
    Native.hb_language_from_string(bytes, bytes.Length)

/// <summary>The oracle language string for a string.</summary>
/// <param name="text">The input.</param>
/// <returns>The canonical string, or none.</returns>
let private oracleLanguageString (text: string) =
    let handle = oracleLanguageHandle text
    match handle with
    | 0n -> ValueNone
    | _ ->
        let pointer = Native.hb_language_to_string handle
        match pointer with
        | 0n -> ValueNone
        | _ -> ValueSome (Marshal.PtrToStringUTF8 pointer |> nonNull)

/// <summary>The printable ASCII characters.</summary>
let private printable = [ for code in 32 .. 126 -> char code ]

/// <summary>Every string of length zero to two over printable ASCII.</summary>
let private directionInputs =
    seq {
        yield String.Empty
        for first in printable do
            yield string first
        for first in printable do
            for second in printable do
                yield string first + string second
    }

/// <summary>Every four-letter ASCII tag.</summary>
let private scriptTags =
    let letters = Array.append [| 'A' .. 'Z' |] [| 'a' .. 'z' |]
    seq {
        for first in letters do
            for second in letters do
                for third in letters do
                    for fourth in letters do
                        yield (uint32 (int first) <<< 24) ||| (uint32 (int second) <<< 16) ||| (uint32 (int third) <<< 8) ||| uint32 (int fourth)
        yield 0u
        yield 0x5A797979u
        yield 0x7A797979u
        yield 0x31323334u
    }

/// <summary>Every language subtag in the pinned IANA registry.</summary>
let private registryLanguages =
    let path = Path.Combine(RepositoryPaths.root (), "tools", "data", "vendored", "language-subtag-registry")
    File.ReadAllLines path
    |> Array.choose (fun line ->
        let trimmed = line.Trim()
        match trimmed.StartsWith "Subtag:", trimmed.StartsWith "Tag:" with
        | true, _ -> Some (trimmed.Substring(7).Trim())
        | _, true -> Some (trimmed.Substring(4).Trim())
        | _ -> None)

/// <summary>Language strings with the awkward bytes the canonicaliser maps or stops on.</summary>
let private languageText =
    let alphabet = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ] @ [ '_'; '.'; '@'; '-'; 'é'; ' '; '\000'; '\t' ]
    gen {
        let! length = Gen.choose (0, 80)
        let! chars = Gen.listOfLength length (Gen.elements alphabet)
        return String(List.toArray chars)
    }

/// <summary>The P3.S02 common differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P3.S02" [
        exhaustive "direction from string" directionInputs
            (fun text -> int (Direction.value (Direction.ofString text)))
            oracleDirection
        exhaustive "script from iso15924" scriptTags
            (fun tag -> uint32 (Script.ofIso15924 (LanguagePrimitives.UInt32WithMeasure tag)))
            oracleScript
        exhaustive "horizontal direction" (Seq.append Scripts.all [ Scripts.Invalid ])
            (fun script -> int (Direction.value (Script.horizontalDirection script)))
            (fun script -> Native.hb_script_get_horizontal_direction (uint32 script))
        exhaustive "language round trip (registry)" registryLanguages
            (fun text -> Language.toString (Language.ofString text))
            oracleLanguageString
        sampled "language round trip (generated)" languageText 10_000
            (fun text -> Language.toString (Language.ofString text))
            oracleLanguageString
        sampled "language matches" (Gen.zip languageText languageText) 2_000
            (fun (first, second) -> Language.matches (Language.ofString first) (Language.ofString second))
            (fun (first, second) -> Native.hb_language_matches(oracleLanguageHandle first, oracleLanguageHandle second) <> 0)
    ]
