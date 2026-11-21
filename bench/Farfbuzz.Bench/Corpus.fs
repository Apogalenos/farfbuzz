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

/// <summary>The pairs added to the speed comparison: the Universal Declaration of Human Rights in fourteen languages and scripts, fetched by
/// <c>dotnet fsi build.fsx bench-data</c> into <c>artifacts/bench-data</c>. Their texts and fonts outside the upstream checkout are absolute
/// paths, which <see cref="load"/> and the benchmarks take as they are. Latin, Cyrillic and Greek are shaped with upstream's Roboto, as in
/// its own benchmark; Russian also with Marck Script, a connected Cyrillic handwriting; the other scripts with Noto.</summary>
let scriptPairs =
    let data = Path.Combine(RepositoryPaths.root (), "artifacts", "bench-data")
    let roboto = Path.Combine("perf", "fonts", "Roboto-Regular.ttf")
    let font file = Path.Combine(data, "fonts", file)
    let text file = Path.Combine(data, "texts", file)
    [ { Name = "Roboto/pl-udhr"; Font = roboto; Text = text "pl-udhr.txt" }
      { Name = "Roboto/nb-udhr"; Font = roboto; Text = text "nb-udhr.txt" }
      { Name = "Roboto/hu-udhr"; Font = roboto; Text = text "hu-udhr.txt" }
      { Name = "Roboto/ru-udhr"; Font = roboto; Text = text "ru-udhr.txt" }
      { Name = "MarckScript/ru-udhr"; Font = font "MarckScript-Regular.ttf"; Text = text "ru-udhr.txt" }
      { Name = "Roboto/mn-udhr"; Font = roboto; Text = text "mn-udhr.txt" }
      { Name = "Roboto/el-udhr"; Font = roboto; Text = text "el-udhr.txt" }
      { Name = "Roboto/el-polyton-udhr"; Font = roboto; Text = text "el-polyton-udhr.txt" }
      { Name = "NotoSansGeorgian/ka-udhr"; Font = font "NotoSansGeorgian-Regular.ttf"; Text = text "ka-udhr.txt" }
      { Name = "NotoSansArmenian/hy-udhr"; Font = font "NotoSansArmenian-Regular.ttf"; Text = text "hy-udhr.txt" }
      { Name = "NotoSansSC/zh-udhr"; Font = font "NotoSansSC-Regular.otf"; Text = text "zh-udhr.txt" }
      { Name = "NotoSansJP/ja-udhr"; Font = font "NotoSansJP-Regular.otf"; Text = text "ja-udhr.txt" }
      { Name = "NotoSansKR/ko-udhr"; Font = font "NotoSansKR-Regular.otf"; Text = text "ko-udhr.txt" }
      { Name = "NotoSansThai/th-udhr"; Font = font "NotoSansThai-Regular.ttf"; Text = text "th-udhr.txt" }
      { Name = "NotoNaskhArabic/ar-udhr"; Font = font "NotoNaskhArabic-Regular.ttf"; Text = text "ar-udhr.txt" } ]

/// <summary>Whether every file of the added pairs has been fetched.</summary>
/// <returns>True when <c>dotnet fsi build.fsx bench-data</c> has run.</returns>
let scriptPairsFetched () =
    let upstream = RepositoryPaths.upstream ()
    scriptPairs |> List.forall (fun pair -> File.Exists(Path.Combine(upstream, pair.Font)) && File.Exists(Path.Combine(upstream, pair.Text)))

/// <summary>The names of the pairs, for BenchmarkDotNet parameters.</summary>
let names = pairs |> List.map _.Name |> List.toArray

/// <summary>A pair's files, resolved under the upstream checkout (an absolute path is taken as it is).</summary>
/// <param name="name">The pair name, of the ten or of the added scripts.</param>
/// <returns>The font path and the UTF-8 text.</returns>
let load (name: string) =
    let pair = pairs @ scriptPairs |> List.find (fun candidate -> candidate.Name = name)
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
