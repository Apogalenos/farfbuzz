module Farfbuzz.Tests.TagRegistryTests

open System.IO
open Expecto
open Farfbuzz.Gen
open Farfbuzz.Harness

/// <summary>The vendored OpenType language-system-tag page.</summary>
/// <returns>The page text.</returns>
let private otHtml () =
    File.ReadAllText(Path.Combine(RepositoryPaths.root(), "tools", "data", "vendored", "languagetags.html"))

/// <summary>The vendored BCP 47 language subtag registry.</summary>
/// <returns>The registry text.</returns>
let private bcp47Registry () =
    File.ReadAllText(Path.Combine(RepositoryPaths.root(), "tools", "data", "vendored", "language-subtag-registry"))

/// <summary>The upstream tag-table generator.</summary>
/// <returns>The path.</returns>
let private generator () =
    Path.Combine(RepositoryPaths.upstream(), "src", "gen-tag-table.py")

/// <summary>Parses the vendored OpenType registry, failing the test on an error.</summary>
/// <returns>The registry.</returns>
let private openTypeRegistry () =
    match TagRegistry.parseOt (otHtml ()) (TagRegistry.iso639_3To1 (generator ())) with
    | Error error -> failwith error
    | Ok ot -> ot

/// <summary>Parses the vendored BCP 47 registry, failing the test on an error.</summary>
/// <returns>The registry.</returns>
let private bcp47 () =
    let keys = (openTypeRegistry ()).FromBcp47 |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    match TagRegistry.parseBcp47 (bcp47Registry ()) keys with
    | Error error -> failwith error
    | Ok bcp -> bcp

/// <summary>The P3.S10 tag-registry parser cases.</summary>
[<Tests>]
let tests =
    testList "P3.S10" [
        test "ISO 639-3 to ISO 639-1 reads the upstream dict literal" {
            let iso = TagRegistry.iso639_3To1 (generator ())
            Expect.equal iso.Count 185 "count"
            Expect.equal iso["abk"] "ab" "Abkhazian"
            Expect.equal iso["zho"] "zh" "Chinese"
        }
        test "the OpenType registry parses tags, names and ISO codes" {
            let ot = openTypeRegistry ()
            Expect.equal ot.Header "<meta name=\"updated_at\" content=\"2024-12-06T06:35:00Z\" />" "header"
            Expect.equal ot.Names.Count 680 "names"
            Expect.equal ot.ToBcp47.Count 672 "to_bcp_47"
            Expect.equal ot.Ranks.Count 672 "ranks"
            Expect.equal ot.FromBcp47.Count 1022 "from_bcp_47"
            Expect.equal ot.ToBcp47["ABK"] (Set.singleton "ab") "ABK"
            Expect.equal ot.ToBcp47["MONT"] (Set.singleton "mnw") "MONT"
        }
        test "the BCP 47 registry parses names, scopes and macrolanguages" {
            let bcp = bcp47 ()
            Expect.equal bcp.Header "File-Date: 2026-09-17" "header"
            Expect.equal bcp.Names.Count 9038 "names"
            Expect.equal bcp.Scopes.Count 440 "scopes"
            Expect.equal bcp.Macrolanguages.Count 239 "macrolanguages"
            Expect.equal bcp.Prefixes.Count 132 "prefixes"
            Expect.equal bcp.Grandfathered.Count 26 "grandfathered"
            Expect.equal bcp.Macrolanguages["zh"].Count 24 "zh macrolanguage"
            Expect.equal bcp.Macrolanguages["ar"].Count 31 "ar macrolanguage"
        }
        test "the HTML tokenizer decodes references" {
            let html = "<table><tr><td>A &amp; B</td><td>'A'</td><td>&#x2019;</td></tr></table>"
            let struct (header, rows) = HtmlRows.parse html
            Expect.equal header ValueNone "no meta header"
            Expect.equal rows.Length 1 "one row"
            Expect.equal (rows[0][0]) "A & B" "ampersand"
            Expect.equal (rows[0][2]) "\u2019" "numeric reference"
        }
        test "an a blanks its cell and br stops collecting it" {
            let html = "<table><tr><td>Name</td><td>'T'</td><td>iso<br>more</td></tr><tr><td>Name2</td><td>'U'</td><td>iso<a href='x'>link</a></td></tr></table>"
            let struct (_, rows) = HtmlRows.parse html
            Expect.equal rows.Length 2 "two rows"
            Expect.equal (rows[0][2]) "iso" "br stops collecting"
            Expect.equal (rows[1][2]) "" "a blanks the cell"
        }
    ]
