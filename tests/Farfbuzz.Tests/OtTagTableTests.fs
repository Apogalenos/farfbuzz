module Farfbuzz.Tests.OtTagTableTests

open Expecto
open Farfbuzz

/// <summary>Finds the first complex rule matching a predicate.</summary>
/// <param name="predicate">The predicate.</param>
/// <returns>The rule.</returns>
let private findRule (predicate: ComplexRule -> bool) : ComplexRule =
    OtTagTable.complexRules |> Seq.find predicate

/// <summary>The P3.S11 tag-table data cases.</summary>
[<Tests>]
let tests =
    testList "P3.S11" [
        test "the generated arrays have the upstream lengths" {
            Expect.equal OtTagTable.languages2.Length 204 "languages2"
            Expect.equal OtTagTable.languages3Blocked.Length 162 "languages3Blocked"
            Expect.equal OtTagTable.languages3.Length 779 "languages3"
            Expect.equal OtTagTable.languages3MultiValues.Length 288 "languages3MultiValues"
            Expect.equal OtTagTable.languages3Multi.Length 137 "languages3Multi"
            Expect.equal OtTagTable.undMinSubtagLength 5 "undMinSubtagLength"
            Expect.equal OtTagTable.complexRules.Length 149 "complexRules"
            Expect.equal OtTagTable.ambiguous.Length 65 "ambiguous"
        }
        test "ot_languages2 starts with aa to AFR" {
            let first = OtTagTable.languages2[0]
            Expect.equal first.Language (Tag.ofString "aa") "language"
            Expect.equal first.OtTag (Tag.ofString "AFR") "ot tag"
        }
        test "the zh-Hant-HK rule is a lang match under z" {
            let rule = findRule (fun candidate -> match candidate.Head with | RuleHead.LangMatches spec -> spec = "h-hant-hk" | _ -> false)
            Expect.equal rule.Group 'z' "group"
            Expect.isEmpty rule.Subtags "subtags"
            Expect.equal rule.Tags.Length 1 "tag count"
            Expect.equal rule.Tags[0] (Tag.ofString "ZHH") "tag"
        }
        test "the und-Syre rule matches the Syre script variant" {
            let rule =
                findRule (fun candidate ->
                    match candidate.Head with
                    | RuleHead.UndOnly -> candidate.Subtags.Length = 1 && candidate.Subtags[0] = "-syre"
                    | _ -> false)
            Expect.equal rule.Group 'u' "group"
            Expect.equal (List.ofSeq rule.Subtags) [ "-syre" ] "subtags"
            Expect.equal rule.Tags.Length 1 "tag count"
            Expect.equal rule.Tags[0] (Tag.ofString "SYRE") "tag"
        }
        test "the el-polyton rule matches the polytonic variant" {
            let rule =
                findRule (fun candidate ->
                    match candidate.Head with
                    | RuleHead.UndOnly -> candidate.Subtags.Length = 1 && candidate.Subtags[0] = "-polyton"
                    | _ -> false)
            Expect.equal rule.Group 'e' "group"
            Expect.equal rule.Tags.Length 1 "tag count"
            Expect.equal rule.Tags[0] (Tag.ofString "PGR") "tag"
        }
    ]
