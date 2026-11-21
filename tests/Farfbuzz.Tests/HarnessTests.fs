module Farfbuzz.Tests.HarnessTests

open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.TestFile
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Runner
open Farfbuzz.Harness.Ratchet

/// <summary>An engine offering what HarfBuzz's amalgamated build offers, for planning only.</summary>
let planner : Shaper.Shaper =
    { Name = "planner"; Shapers = [ "ot"; "fallback" ]; FaceLoaders = [ "ot" ]; FontFuncs = [ "ot" ]; Shape = fun _ -> Error "planning only" }

/// <summary>The machine-independent discovery counts of HarfBuzz 15.1.0's shaping suites, pinned.</summary>
let pinned =
    [ "in-house", (91, 5294, 3, 21, 6, 0)
      "aots", (128, 276, 15, 128, 0, 0)
      "text-rendering-tests", (78, 437, 0, 78, 0, 16) ]

/// <summary>Counts files, cases, commented cases, directives, backend skips and disabled files of a suite.</summary>
/// <param name="suite">The suite.</param>
/// <returns>The counts, in the order of <see cref="pinned"/>.</returns>
let counts (suite: Suites.Suite) =
    let lines = suite.Files |> List.collect (fun file -> TestFile.parse file.Path)
    let count predicate = lines |> List.filter predicate |> List.length
    let backendSkips =
        suite.Files |> List.collect (plan planner) |> List.filter (fun run -> match run.Kind with Skipped (BackendNotProvided _) -> true | Skipped _ | Execute _ | Unrunnable _ -> false) |> List.length
    suite.Files.Length,
    count (function Case _ -> true | CommentedCase _ | Directive _ | Ignored _ | Malformed _ -> false),
    count (function CommentedCase _ -> true | Case _ | Directive _ | Ignored _ | Malformed _ -> false),
    count (function Directive _ -> true | Case _ | CommentedCase _ | Ignored _ | Malformed _ -> false),
    backendSkips,
    suite.Disabled.Length

/// <summary>Harness self-tests and discovery counts, P1.Harness.</summary>
[<Tests>]
let tests =
    testList "P1.Harness" [
        testList "discovery" [
            for suite in Suites.registered (RepositoryPaths.upstream ()) do
                yield test $"{suite.Name} counts are pinned" {
                    Expect.equal (Some (counts suite)) (pinned |> List.tryFind (fst >> (=) suite.Name) |> Option.map snd) "files, cases, commented, directives, backend skips, disabled"
                }
            yield test "no line is one run-tests.py would crash on" {
                let malformed =
                    Suites.registered (RepositoryPaths.upstream ())
                    |> List.collect (fun suite -> suite.Files |> List.collect (fun file -> TestFile.parse file.Path |> List.choose (function Malformed (line, reason) -> Some $"{file.Name}:{line} {reason}" | _ -> None)))
                Expect.isEmpty malformed "malformed lines"
            }
            yield test "every planned run has a unique key" {
                let keys = Suites.registered (RepositoryPaths.upstream ()) |> List.collect (fun suite -> suite.Files |> List.collect (plan planner)) |> List.map _.Id.Key
                Expect.equal (List.distinct keys).Length keys.Length "distinct keys"
            }
        ]
        testList "test lines" [
            test "a '# ' comment is ignored" { Expect.equal (parseLine 1 "# note") (Ignored 1) "comment" }
            test "'#' then a case is a commented-out case" {
                Expect.equal (parseLine 2 "#../fonts/a.ttf;--ned;U+0041;[1]") (CommentedCase (2, { Font = Relative "../fonts/a.ttf"; Options = [ "--ned" ]; Unicodes = "U+0041"; Expected = Glyphs "[1]" })) "commented"
            }
            test "a directive with an exclusion and a trailing comment" {
                Expect.equal (parseLine 3 "@font-funcs-=ft,fontations # why") (Directive (3, { Kind = FontFuncs; Exclude = true; Values = [ "ft"; "fontations" ] })) "directive"
            }
            test "an absolute font with its hash" {
                Expect.equal (parseFont "/System/Library/Fonts/A.ttc@abc") (Absolute ("/System/Library/Fonts/A.ttc", ValueSome "abc")) "font"
            }
            test "three fields is malformed" { Expect.equal (parseLine 4 "a;b;c") (Malformed (4, "3 fields instead of 4")) "malformed" }
        ]
        testList "hb-shape options" [
            test "unicodes use hb-shape's delimiters and hexadecimal" { Expect.equal (decodeUnicodes "U+0041,<u0x42> 43") (Ok [| 0x41u; 0x42u; 0x43u |]) "decoded" }
            test "a lone '*' is the character '*'" { Expect.equal (decodeUnicodes "*") (Ok [| 0x2Au |]) "star" }
            test "--ned means no clusters and no advances" {
                let request = ShapeRequest.build "f" "41" [ "--ned"; "--no-glyph-names" ] []
                Expect.equal (request |> Result.map _.Output) (Ok (SerializeFlags.NoClusters ||| SerializeFlags.NoAdvances ||| SerializeFlags.NoGlyphNames)) "flags"
            }
            test "values may follow the option as the next word" {
                let request = ShapeRequest.build "f" "41" [ "--direction"; "ttb"; "--font-size=1000,1500" ] []
                Expect.equal (request |> Result.map (fun r -> r.Direction, r.FontSize)) (Ok (ValueSome "ttb", ValueSome (struct (1000.0, 1500.0)))) "parsed"
            }
            test "an unknown option is an error, never ignored" { Expect.isError (ShapeRequest.build "f" "41" [ "--frobnicate" ] []) "unknown" }
            test "language defaults to c, as under LC_ALL=C" { Expect.equal (ShapeRequest.build "f" "41" [] [] |> Result.map _.Language) (Ok "c") "language" }
        ]
        testList "run-tests.py loops" [
            let selection = { Shapers = [ "ot"; "fallback" ]; FaceLoaders = [ "ot" ]; FontFuncs = [ "ot" ] }
            let none = { Shaper = ValueNone; FaceLoader = ValueNone; FontFuncs = ValueNone }
            test "the fallback shaper inherits the font-funcs left by the ot pass" {
                Expect.equal
                    (combinations selection none)
                    [ { Shaper = ValueSome "ot"; FaceLoader = ValueSome "ot"; FontFuncs = ValueSome "ot" }
                      { Shaper = ValueSome "fallback"; FaceLoader = ValueSome "ot"; FontFuncs = ValueSome "ot" } ]
                    "combinations"
            }
            test "an unsupported pinned backend is skipped" {
                Expect.equal (stripPins planner [ "--ned"; "--font-funcs=ft" ]) (Error (FontFuncs, "ft")) "pin"
            }
            test "a supported pin is removed from the options" {
                Expect.equal (stripPins planner [ "--shaper"; "ot"; "--ned" ]) (Ok ([ "--ned" ], { none with Shaper = ValueSome "ot" })) "pin"
            }
            test "verification options are appended unless the expectation is '*'" {
                Expect.equal (extraOptions none (Glyphs "[1]")) [ "--verify"; "--unsafe-to-concat"; "--safe-to-insert-tatweel" ] "extra"
                Expect.equal (extraOptions none Any) [] "star"
            }
        ]
        testList "ratchet" [
            test "a baselined run that fails is a regression" { Expect.equal (verdict true (Fail ("a", "b"))) Regressed "verdict" }
            test "a new pass is recorded, a new failure is not a regression" {
                Expect.equal (verdict false Pass, verdict false (Broken "x")) (NewlyPassing, StillFailing) "verdicts"
            }
            test "a planted regression turns a file red" {
                let file = (Suites.registered (RepositoryPaths.upstream ()) |> List.find (fun suite -> suite.Name = "aots")).Files.Head
                let passing = { planner with Shape = fun _ -> Ok Shaper.ShapingFailed }
                let everyKey = plan passing file |> List.map _.Id.Key |> Set.ofList
                let check = checkFile passing everyKey file
                Expect.isNonEmpty check.Regressions "the planted baseline must regress"
            }
        ]
    ]
