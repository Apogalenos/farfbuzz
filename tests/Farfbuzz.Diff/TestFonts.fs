module Farfbuzz.Diff.TestFonts

open System
open Farfbuzz.Oracle
open System.IO
open Farfbuzz.Harness

/// <summary>One face of a test font.</summary>
type TestFont = { Path: string; Suite: string; Index: int }

/// <summary>The font files of the three shaping suites and of upstream's shaping benchmark.</summary>
/// <returns>The paths, sorted.</returns>
let files () =
    let upstream = RepositoryPaths.upstream ()
    let fontFiles (directory: string) =
        Directory.EnumerateFiles directory
        |> Seq.filter (fun path -> [ ".ttf"; ".otf"; ".ttc"; ".dfont" ] |> List.contains ((Path.GetExtension path |> nonNull).ToLowerInvariant()))
    let suiteFonts = Suites.suiteNames |> List.collect (fun suite -> fontFiles (Path.Combine(upstream, "test", "shape", "data", suite, "fonts")) |> Seq.map (fun path -> suite, path) |> Seq.toList)
    let perfFonts = fontFiles (Path.Combine(upstream, "perf", "fonts")) |> Seq.map (fun path -> "perf", path) |> Seq.toList
    suiteFonts @ perfFonts |> List.sortBy snd

/// <summary>Every face of every test font, the face count read through HarfBuzzSharp.</summary>
/// <returns>The faces.</returns>
let all () : TestFont list =
    files ()
    |> List.collect (fun (suite, path) ->
        use blob = HarfBuzzSharp.Blob.FromFile path
        [ for index in 0 .. max 0 (blob.FaceCount - 1) -> { Path = path; Suite = suite; Index = index } ])

/// <summary>The code points a face maps, read through the oracle.</summary>
/// <param name="font">The face.</param>
/// <returns>The code points, ascending.</returns>
let coverage (font: TestFont) : uint32 array =
    use blob = HarfBuzzSharp.Blob.FromFile font.Path
    use face = new HarfBuzzSharp.Face(blob, font.Index)
    let set = Native.hb_set_create ()
    Native.hb_face_collect_unicodes(face.Handle, set)
    let rec members (current: uint32) (found: uint32 list) =
        let mutable next = current
        match Native.hb_set_next(set, &next) with
        | 0 -> List.rev found
        | _ -> members next (next :: found)
    let result = members UInt32.MaxValue [] |> List.toArray
    Native.hb_set_destroy set
    result

/// <summary>Every face of the named fonts under <c>upstream/harfbuzz/test/</c>, read through HarfBuzzSharp.</summary>
/// <param name="relativePaths">The font paths relative to <c>upstream/harfbuzz/test/</c>.</param>
/// <returns>The faces.</returns>
let extra (relativePaths: string list) : System.Collections.Immutable.ImmutableArray<TestFont> =
    let upstream = RepositoryPaths.upstream ()
    relativePaths
    |> List.collect (fun relative ->
        let path = Path.Combine(upstream, "test", relative)
        use blob = HarfBuzzSharp.Blob.FromFile path
        [ for index in 0 .. max 0 (blob.FaceCount - 1) -> { Path = path; Suite = "extra"; Index = index } ])
    |> List.toArray
    |> System.Collections.Immutable.ImmutableArray.CreateRange

/// <summary>The first face of the test font whose file name matches, searched among every suite and the perf fonts.</summary>
/// <param name="fileName">The file name, for example <c>Roboto-Regular.ttf</c>.</param>
/// <returns>The font.</returns>
let named (fileName: string) : TestFont =
    all () |> List.find (fun font -> Path.GetFileName font.Path = fileName)
