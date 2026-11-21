module Farfbuzz.Bench.UpstreamPairs

open System
open Farfbuzz
open Farfbuzz.Harness

/// <summary>A font/text pair of perf/benchmark-shape.cc, paths relative to upstream/harfbuzz.</summary>
type Pair = { FontPath: string; TextPath: string }

/// <summary>The line terminator byte of the corpus texts, the byte benchmark-shape.cc's memchr scans for.</summary>
[<Literal>]
let private Newline = '\n'B

/// <summary>Counts the <c>\n</c> bytes of a text: the number of lines upstream shapes.</summary>
/// <param name="text">The text.</param>
/// <param name="index">The next byte.</param>
/// <param name="count">The newlines seen so far.</param>
/// <returns>The total number of newlines.</returns>
[<TailCall>]
let rec private countNewlines (text: ReadOnlySpan<byte>) (index: int) (count: int) =
    match index < text.Length with
    | false -> count
    | true -> countNewlines text (index + 1) (if text[index] = Newline then count + 1 else count)

/// <summary>Fills one (start, byteLength) pair per <c>\n</c>-terminated line, excluding the <c>\n</c>.</summary>
/// <param name="text">The text.</param>
/// <param name="target">The array being filled, as long as the newline count.</param>
/// <param name="index">The next byte.</param>
/// <param name="start">The first byte of the current line.</param>
/// <param name="slot">The next target slot.</param>
/// <returns>The number of slots filled.</returns>
[<TailCall>]
let rec private fillLines (text: ReadOnlySpan<byte>) (target: struct (int * int) array) (index: int) (start: int) (slot: int) =
    match index < text.Length with
    | false -> slot
    | true ->
        match text[index] = Newline with
        | true ->
            target[slot] <- struct (start, index - start)
            fillLines text target (index + 1) (index + 1) (slot + 1)
        | false -> fillLines text target (index + 1) start slot

/// <summary>The ten pairs of perf/benchmark-shape.cc at HarfBuzz 15.1.0, in upstream order.</summary>
let all =
    Farfbuzz.Bench.Corpus.pairs
    |> List.map (fun pair -> { FontPath = pair.Font; TextPath = pair.Text })
    |> Sealed.ofList

/// <summary>
/// The lines of a text as upstream shapes them: each <c>\n</c>-terminated line, with the bytes after it as
/// post-context, and a final line without a terminator skipped, exactly the <c>memchr</c> loop of benchmark-shape.cc.
/// </summary>
/// <param name="text">The UTF-8 text.</param>
/// <returns>One (start, byteLength) per line, the <c>\n</c> excluded.</returns>
let lineRanges (text: ReadOnlySpan<byte>) =
    let count = countNewlines text 0 0
    let target = Array.zeroCreate count
    fillLines text target 0 0 0 |> ignore
    Sealed.ofSpan (ReadOnlySpan target) |> TailCalls.settle
