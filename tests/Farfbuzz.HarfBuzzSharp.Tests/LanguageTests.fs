module Farfbuzz.HarfBuzzSharp.Tests.LanguageTests

open System
open System.Globalization
open System.Runtime.InteropServices
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>The oracle's canonical language name for a string, null for the invalid language.</summary>
/// <param name="text">The language string.</param>
/// <returns>The name <c>hb_language_to_string</c> gives.</returns>
let private oracleName (text: string) =
    let bytes = Encoding.UTF8.GetBytes text
    let language = Native.hb_language_from_string(bytes, bytes.Length)
    Option.ofObj (Marshal.PtrToStringAnsi (Native.hb_language_to_string language))

/// <summary>The canonical name the facade gives a language string, none for the invalid language.</summary>
/// <param name="text">The language string.</param>
/// <returns>The name.</returns>
let private facadeName (text: string) =
    use language = new HarfBuzzSharp.Language(text)
    Option.ofObj language.Name

/// <summary>The P10.S03 cases: language names against the oracle.</summary>
[<Tests>]
let tests =
    testList "P10.S03" [
        testCase "Language names equal the oracle for every language string of the shape suites" (fun () ->
            OracleSession.ensure ()
            let strings = SuiteData.optionValues "--language"
            Expect.isGreaterThan strings.Length 5 "the suites carry language strings"
            let mismatches = strings |> List.filter (fun text -> facadeName text <> oracleName text)
            Expect.isEmpty mismatches "language strings that differ")
        testCase "Language names equal the oracle on 1 000 generated strings" (fun () ->
            OracleSession.ensure ()
            let alphabet = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ] @ [ '-'; '_'; ' '; 'é' ]
            let strings = Gens.sample 1000 (Gens.noise alphabet 24)
            let mismatches = strings |> List.filter (fun text -> facadeName text <> oracleName text)
            Expect.isEmpty mismatches "language strings that differ")
        testCase "Language(CultureInfo) equals the oracle for every culture" (fun () ->
            OracleSession.ensure ()
            let cultures = CultureInfo.GetCultures CultureTypes.AllCultures
            Expect.isGreaterThan cultures.Length 100 "the machine has its cultures"
            let mismatches =
                cultures
                |> Array.filter (fun culture -> (use language = new HarfBuzzSharp.Language(culture) in Option.ofObj language.Name) <> oracleName culture.TwoLetterISOLanguageName)
            Expect.isEmpty mismatches "cultures that differ")
        test "the invariant culture gives iv, as natively" {
            OracleSession.ensure ()
            use invariant = new HarfBuzzSharp.Language(CultureInfo.InvariantCulture)
            Expect.equal invariant.Name "iv" "facade"
            Expect.equal (oracleName CultureInfo.InvariantCulture.TwoLetterISOLanguageName) (Some "iv") "oracle"
        }
        test "equality is by canonical name and the invalid language has no name" {
            use upper = new HarfBuzzSharp.Language "EN-us"
            use canonical = new HarfBuzzSharp.Language "en-US"
            use german = new HarfBuzzSharp.Language "de"
            use empty = new HarfBuzzSharp.Language(String.Empty)
            use missing = new HarfBuzzSharp.Language(Unchecked.defaultof<string>)
            Expect.equal upper canonical "case folded"
            Expect.notEqual upper german "different"
            Expect.isNull empty.Name "empty"
            Expect.isNull missing.Name "null"
            Expect.equal (upper.GetHashCode()) (canonical.GetHashCode()) "hash"
        }
        test "Language.Default is the C locale and reads no culture" {
            Expect.equal HarfBuzzSharp.Language.Default.Name "c" "default"
            let before = CultureInfo.CurrentCulture
            CultureInfo.CurrentCulture <- CultureInfo "fr-FR"
            let after = HarfBuzzSharp.Language.Default.Name
            CultureInfo.CurrentCulture <- before
            Expect.equal after "c" "culture ignored"
        }
        test "the oracle's default language is recorded" {
            OracleSession.ensure ()
            let native = Option.ofObj (Marshal.PtrToStringAnsi (Native.hb_language_to_string (Native.hb_language_get_default ())))
            printfn $"P10.S03: oracle hb_language_get_default () = {native}; facade Language.Default.Name = {HarfBuzzSharp.Language.Default.Name}"
            Expect.isSome native "the oracle has a default language"
        }
    ]
