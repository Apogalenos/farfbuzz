module Farfbuzz.Bench.Corpus

open System
open System.IO
open System.Text
open Farfbuzz.Harness

/// <summary>One font/text pair of upstream's perf/benchmark-shape.cc.</summary>
type Pair = { Name: string; Font: string; Text: string }

/// <summary>The ten pairs of perf/benchmark-shape.cc at HarfBuzz 15.1.0, in upstream order.</summary>
let pairs =
    let perf file = Path.Combine("perf", "fonts", file)
    let subset file = Path.Combine("test", "subset", "data", "fonts", file)
    let text file = Path.Combine("perf", "texts", file)
    [ { Name = "Roboto/en-thelittleprince"; Font = perf "Roboto-Regular.ttf"; Text = text "en-thelittleprince.txt" }
      { Name = "Roboto/en-words"; Font = perf "Roboto-Regular.ttf"; Text = text "en-words.txt" }
      { Name = "SourceSerifVariable/react-dom"; Font = subset "SourceSerifVariable-Roman.ttf"; Text = text "react-dom.txt" }
      { Name = "NotoSansDevanagari/hi-words"; Font = subset "NotoSansDevanagari-Regular.ttf"; Text = text "hi-words.txt" }
      { Name = "Amiri/fa-thelittleprince"; Font = perf "Amiri-Regular.ttf"; Text = text "fa-thelittleprince.txt" }
      { Name = "NotoNastaliqUrdu/fa-thelittleprince"; Font = perf "NotoNastaliqUrdu-Regular.ttf"; Text = text "fa-thelittleprince.txt" }
      { Name = "NotoNastaliqUrdu/fa-words"; Font = perf "NotoNastaliqUrdu-Regular.ttf"; Text = text "fa-words.txt" }
      { Name = "Gulzar/fa-thelittleprince"; Font = perf "Gulzar-Regular.ttf"; Text = text "fa-thelittleprince.txt" }
      { Name = "Gulzar/fa-words"; Font = perf "Gulzar-Regular.ttf"; Text = text "fa-words.txt" }
      { Name = "Duployan/duployan"; Font = perf "NotoSansDuployan-Regular.otf"; Text = text "duployan.txt" } ]

/// <summary>The names of the pairs, for BenchmarkDotNet parameters.</summary>
let names = pairs |> List.map _.Name |> List.toArray

/// <summary>A pair's files, resolved under the upstream checkout.</summary>
/// <param name="name">The pair name.</param>
/// <returns>The font path and the UTF-8 text.</returns>
let load (name: string) =
    let pair = pairs |> List.find (fun candidate -> candidate.Name = name)
    let upstream = RepositoryPaths.upstream ()
    Path.Combine(upstream, pair.Font), File.ReadAllBytes(Path.Combine(upstream, pair.Text))

/// <summary>
/// The lines of a text as upstream shapes them: each <c>\n</c>-terminated line, with the rest of the text after it
/// as post-context (a final line without <c>\n</c> is skipped, as in benchmark-shape.cc).
/// </summary>
/// <param name="text">The UTF-8 text.</param>
/// <returns>The start offset and length of each line.</returns>
let lines (text: byte array) : struct (int * int) array =
    let rec collect (start: int) (found: struct (int * int) list) =
        match Array.IndexOf(text, '\n'B, start) with
        | -1 -> List.rev found |> List.toArray
        | newline -> collect (newline + 1) (struct (start, newline - start) :: found)
    collect 0 []
