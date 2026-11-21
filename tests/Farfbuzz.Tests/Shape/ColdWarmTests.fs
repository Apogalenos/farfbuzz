module Farfbuzz.Tests.Shape.ColdWarmTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests

/// <summary>A corpus pair: a font and a text of the performance corpus, with the font's folder.</summary>
type private CorpusPair = { Font: string; Text: string }

/// <summary>The pairs of the performance corpus that this test shapes: the scripts whose lookups differ most.</summary>
let private pairs =
    [| { Font = "perf/fonts/Roboto-Regular.ttf"; Text = "perf/texts/en-thelittleprince.txt" }
       { Font = "perf/fonts/Amiri-Regular.ttf"; Text = "perf/texts/fa-thelittleprince.txt" }
       { Font = "perf/fonts/NotoNastaliqUrdu-Regular.ttf"; Text = "perf/texts/fa-words.txt" }
       { Font = "perf/fonts/Gulzar-Regular.ttf"; Text = "perf/texts/fa-words.txt" }
       { Font = "perf/fonts/NotoSansDuployan-Regular.otf"; Text = "perf/texts/duployan.txt" }
       { Font = "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf"; Text = "perf/texts/hi-words.txt" } |]

/// <summary>The newline byte that ends a line of the corpus texts.</summary>
[<Literal>]
let private Newline = '\n'B

/// <summary>The most lines of a text one example shapes.</summary>
[<Literal>]
let private LinesPerPair = 6

/// <summary>The newline-terminated lines of a text.</summary>
/// <param name="text">The text.</param>
/// <param name="start">The first byte of the line being found.</param>
/// <param name="found">The lines found so far, last first.</param>
/// <returns>One (start, byteLength) per line.</returns>
[<TailCall>]
let rec private linesFrom (text: byte array) (start: int) (found: struct (int * int) list) =
    match Array.IndexOf(text, Newline, start) with
    | -1 -> found |> List.rev |> List.toArray
    | newline -> linesFrom text (newline + 1) (struct (start, newline - start) :: found)

/// <summary>A fresh font of a corpus pair: a face and font made now, so that every lazily built structure is empty.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The font.</returns>
let private freshFont (pair: CorpusPair) =
    Font.create (Face.create (UpstreamFiles.read pair.Font) 0u)

/// <summary>Shapes one slice of a text on a font and serialises the glyphs.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <param name="start">The first byte.</param>
/// <param name="length">The bytes.</param>
/// <returns>The serialised glyphs, or why shaping failed.</returns>
let private shapedText (font: Font) (text: byte array) (start: int) (length: int) =
    let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    let struct (shaped, _) = Shape.shapeCached PlanCache.empty font ImmutableArray<Feature>.Empty ValueNone run |> TailCalls.settle
    match shaped with
    | Ok run -> Serialize.glyphs SerializeFlags.Default (fun _ -> ValueNone) (fun _ -> ValueNone) (run.Infos.AsSpan()) (run.Positions.AsSpan()) |> TailCalls.settle
    | Error problem -> $"shaping failed: {problem}"

/// <summary>Shapes slices on a font just built, then on the same font again, and finally on another font just built after a full
/// collection: all three must agree.</summary>
/// <param name="pair">The pair.</param>
/// <param name="text">The text.</param>
/// <param name="slices">The slices to shape, in order.</param>
/// <returns>The three serialisations of each slice: cold, warm, cold after a collection.</returns>
let private threeWays (pair: CorpusPair) (text: byte array) (slices: struct (int * int) array) =
    let font = freshFont pair
    let cold = slices |> Array.map (fun (struct (start, length)) -> shapedText font text start length)
    let warm = slices |> Array.map (fun (struct (start, length)) -> shapedText font text start length)
    GC.Collect()
    let other = freshFont pair
    let again = slices |> Array.map (fun (struct (start, length)) -> shapedText other text start length)
    Array.zip3 cold warm again

/// <summary>Cutting a line at code point boundaries: a random slice of it.</summary>
/// <param name="text">The text.</param>
/// <param name="line">The line, as a start and a length.</param>
/// <param name="first">A random number choosing where the slice starts.</param>
/// <param name="size">A random number choosing how long the slice is.</param>
/// <returns>The slice.</returns>
let private sliceOf (text: byte array) (line: struct (int * int)) (first: int) (size: int) =
    let struct (start, length) = line
    let isBoundary (offset: int) = offset = start + length || (text[offset] &&& 0xC0uy) <> 0x80uy
    let boundaries = [| for offset in start .. start + length do if isBoundary offset then yield offset |]
    let from = abs first % boundaries.Length
    let upto = min (boundaries.Length - 1) (from + 1 + abs size % 96)
    struct (boundaries[from], boundaries[upto] - boundaries[from])

/// <summary>Shaping a text on a font whose structures are all empty gives what shaping it on a warm font gives, for every font and text
/// of the performance corpus: the structures a font builds on first use, while a line is being shaped, never change the answer. The
/// browser-wasm build compiled ahead of time once shaped the first Duployan pass differently from the rest (503,804 glyphs against 503,948).</summary>
[<Tests>]
let tests =
    testList "ColdWarm" [
        test "every corpus pair shapes the same cold, warm and cold after a full collection" {
            pairs
            |> Array.iter (fun pair ->
                let text = (UpstreamFiles.read pair.Text).ToArray()
                let slices = linesFrom text 0 [] |> Array.truncate LinesPerPair
                threeWays pair text slices
                |> Array.iteri (fun index (cold, warm, again) ->
                    Expect.equal warm cold $"{pair.Font} line {index}: warm against cold"
                    Expect.equal again cold $"{pair.Font} line {index}: cold after a collection against cold"))
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 150 } "a random slice of a corpus line shapes the same cold and warm" (fun (pairIndex: byte) (lineIndex: uint16) (first: int) (size: int) ->
            let pair = pairs[int pairIndex % pairs.Length]
            let text = (UpstreamFiles.read pair.Text).ToArray()
            let lines = linesFrom text 0 []
            let slice = sliceOf text lines[int lineIndex % lines.Length] first size
            threeWays pair text [| slice |]
            |> Array.forall (fun (cold, warm, again) -> warm = cold && again = cold))
    ]
