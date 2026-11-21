module Farfbuzz.Harness.Suites

open System
open System.IO
open System.Text.RegularExpressions

/// <summary>One registered test file of a suite. A file registered twice appears twice, with different registrations.</summary>
type TestFileEntry =
    { Suite: string         // aots, in-house or text-rendering-tests
      Name: string          // file name, for example arabic-fallback-shaping.tests
      Path: string          // absolute path
      Registration: int }   // 1-based position in the suite's meson list

/// <summary>One suite as test/shape/data/&lt;suite&gt;/meson.build registers it.</summary>
type Suite =
    { Name: string
      Files: TestFileEntry list
      Disabled: string list }  // files listed in the suite's disabled list: reported, never run

/// <summary>The suites in the order test/shape/meson.build declares them.</summary>
let suiteNames = [ "in-house"; "aots"; "text-rendering-tests" ]

/// <summary>Reads the quoted file names of one meson array, <c>name = [ 'a.tests', … ]</c>.</summary>
/// <param name="meson">The meson.build text.</param>
/// <param name="variable">The array variable.</param>
/// <returns>The file names in order, duplicates kept.</returns>
let mesonArray (meson: string) (variable: string) =
    let block = Regex.Match(meson, $@"(?m)^{Regex.Escape variable}\s*=\s*\[(?<items>[^\]]*)\]")
    match block.Success with
    | false -> []
    | true -> [ for item in Regex.Matches(block.Groups["items"].Value, @"'([^']+)'") -> item.Groups[1].Value ]

/// <summary>Discovers the suites and their registered files under the upstream checkout.</summary>
/// <param name="upstreamRoot">The path of upstream/harfbuzz.</param>
/// <returns>The suites, in upstream order.</returns>
let registered (upstreamRoot: string) : Suite list =
    suiteNames
    |> List.map (fun suite ->
        let directory = Path.Combine(upstreamRoot, "test", "shape", "data", suite)
        let meson = File.ReadAllText(Path.Combine(directory, "meson.build"))
        let variable = suite.Replace('-', '_') + "_tests"
        let files =
            mesonArray meson variable
            |> List.mapi (fun index name ->
                { Suite = suite; Name = name; Path = Path.Combine(directory, "tests", name); Registration = index + 1 })
        { Name = suite; Files = files; Disabled = mesonArray meson ("disabled_" + variable) })
