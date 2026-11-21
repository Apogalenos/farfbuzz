// Checks every upstream citation `path:line` or `path:start-end` in the given Markdown files:
// the file must exist under upstream/harfbuzz and the lines must exist in it.
// Usage: dotnet fsi tools/check-citations.fsx docs/research/*.md docs/handoff/*.md

open System
open System.IO
open System.Text.RegularExpressions

/// <summary>The upstream checkout citations are relative to.</summary>
let upstream = Path.Combine(__SOURCE_DIRECTORY__, "..", "upstream", "harfbuzz")

/// <summary>A citation: an upstream path under src, util, test, perf or docs, a colon, and a line or range.</summary>
let citation = Regex(@"(?<![\w/.])((?:src|util|test|perf|docs)/[\w./+-]+\.[\w]+):(\d+)(?:-(\d+))?")

/// <summary>Line counts of upstream files, read once per file.</summary>
let lineCount = Collections.Generic.Dictionary<string, int voption>()

/// <summary>The number of lines of an upstream file, or none when it does not exist.</summary>
/// <param name="path">The path relative to upstream/harfbuzz.</param>
/// <returns>The line count.</returns>
let linesOf (path: string) =
    match lineCount.TryGetValue path with
    | true, count -> count
    | false, _ ->
        let full = Path.Combine(upstream, path)
        let count = if File.Exists full then ValueSome (File.ReadAllLines(full).Length) else ValueNone
        lineCount[path] <- count
        count

let documents = fsi.CommandLineArgs |> Array.toList |> List.tail
let problems =
    documents
    |> List.collect (fun document ->
        File.ReadAllLines document
        |> Array.toList
        |> List.mapi (fun index line -> index + 1, line)
        |> List.collect (fun (number, line) ->
            [ for found in citation.Matches line do
                let path = found.Groups[1].Value
                let first = int found.Groups[2].Value
                let last = if found.Groups[3].Success then int found.Groups[3].Value else first
                match linesOf path with
                | ValueNone -> yield $"{document}:{number}: {path} does not exist"
                | ValueSome count when last > count || first < 1 || last < first -> yield $"{document}:{number}: {path}:{first}-{last} outside 1-{count}"
                | ValueSome _ -> () ]))
let total =
    documents |> List.sumBy (fun document -> citation.Matches(File.ReadAllText document).Count)
problems |> List.iter Console.Error.WriteLine
Console.WriteLine $"{total} citations in {documents.Length} files, {problems.Length} problems"
exit (if problems.IsEmpty then 0 else 1)
