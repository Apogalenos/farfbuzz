module Farfbuzz.Tests.Shape.LineShapeAllocationTests

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.UpstreamFiles
open Farfbuzz.Tests.Shape.LineShapeTests

/// <summary>Whether the byte counters are meaningful: CoreCLR only.</summary>
let private isCoreClr =
    not (OperatingSystem.IsBrowser())
    && not (RuntimeInformation.FrameworkDescription.Contains("Mono", StringComparison.Ordinal))

/// <summary>The faces: Roboto without its layout tables and with them.</summary>
let private faces =
    lazy
        (let roboto = read "perf/fonts/Roboto-Regular.ttf"
         struct (Font.create (Face.create (stripped roboto) 0u), Font.create (Face.create roboto 0u)))

/// <summary>The offset and length of each newline-terminated line of a text.</summary>
/// <param name="text">The text.</param>
/// <returns>The lines.</returns>
let private linesOf (text: byte array) =
    let found = ResizeArray<struct (int * int)>()
    let mutable start = 0
    let mutable newline = Array.IndexOf(text, 10uy, start)
    while newline >= 0 do
        found.Add (struct (start, newline - start))
        start <- newline + 1
        newline <- Array.IndexOf(text, 10uy, start)
    ImmutableArray.CreateRange found

/// <summary>A text of identical lines.</summary>
/// <param name="line">The line.</param>
/// <param name="count">How many.</param>
/// <returns>The text and its lines.</returns>
let private repeated (line: string) (count: int) =
    let bytes = Encoding.UTF8.GetBytes line
    let text = Array.concat (Array.init count (fun _ -> Array.append bytes [| 10uy |]))
    struct (text, ImmutableArray.CreateRange (Array.init count (fun index -> struct (index * (bytes.Length + 1), bytes.Length))))

/// <summary>Shapes a document sequentially and gives its glyph count.</summary>
/// <param name="font">The font.</param>
/// <param name="cache">The plan cache.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyphs.</returns>
let private shapeOnce (font: Font) (cache: PlanCache) (text: byte array) (lines: ImmutableArray<struct (int * int)>) =
    let struct (document, _) = LineShape.utf8 1 cache font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory text) lines
    document.ArenaInfos.Length

/// <summary>The bytes per glyph and per line of one sequential pass over a document, the plan cache warm.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count, the bytes per glyph and the bytes per line.</returns>
let private measured (font: Font) (text: byte array) (lines: ImmutableArray<struct (int * int)>) =
    let struct (warm, cache) = LineShape.utf8 1 PlanCache.empty font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory text) lines
    let glyphs = warm.ArenaInfos.Length
    let bytes = bytesAllocatedAtBest (fun () -> shapeOnce font cache text lines |> ignore)
    struct (glyphs, float bytes / float glyphs, float bytes / float lines.Length)

/// <summary>The bytes a shaped document keeps alive, counted from its parts: the two arrays of glyphs and positions, the table of
/// lines, and one object for each line that is not a window of the arrays.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The retained bytes, the lines that are not windows of the document's arrays and the glyphs.</returns>
let private retained (font: Font) (text: byte array) (lines: ImmutableArray<struct (int * int)>) =
    let struct (document, _) = LineShape.utf8 1 PlanCache.empty font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory text) lines
    let detached = document.Lines |> Seq.filter (fun line -> match line with | Arena _ -> false | Alone _ -> true | Refused _ -> true) |> Seq.length
    let glyphs = int64 document.ArenaInfos.Length
    let arrays = 3L * 24L + glyphs * int64 (Unsafe.SizeOf<GlyphInfo>() + Unsafe.SizeOf<GlyphPosition>()) + int64 document.Lines.Length * int64 (Unsafe.SizeOf<LineOutcome>())
    struct (arrays, detached, document.ArenaInfos.Length)

/// <summary>The bytes all threads allocate shaping a document with several threads, the fewest of several passes.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text.</param>
/// <param name="lines">The lines.</param>
/// <returns>The glyph count and the fewest bytes.</returns>
let private parallelBytes (font: Font) (text: byte array) (lines: ImmutableArray<struct (int * int)>) =
    let run () =
        let struct (document, _) = LineShape.utf8From 0 4 PlanCache.empty font ImmutableArray<Feature>.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory text) lines
        document.ArenaInfos.Length
    let glyphs = run ()
    let fewest =
        Seq.init 8 (fun _ ->
            let before = GC.GetTotalAllocatedBytes true
            run () |> ignore
            GC.GetTotalAllocatedBytes true - before)
        |> Seq.min
    struct (glyphs, fewest)

/// <summary>The allocation of the document entry points: bytes per glyph and per line of each path, sequential and on several threads.</summary>
[<Tests>]
let tests =
    testList "line shape allocation" (
        match isCoreClr with
        | false -> []
        | true ->
            [ test "the Little Prince without layout takes at most 45 bytes per glyph and keeps 45" {
                  let struct (plain, _) = faces.Value
                  let text = (read "perf/texts/en-thelittleprince.txt").ToArray()
                  let lines = linesOf text
                  let struct (glyphs, perGlyph, _) = measured plain text lines
                  Expect.isGreaterThan glyphs 80000 "the whole text"
                  Expect.isLessThanOrEqual perGlyph 45.0 $"{perGlyph:F1} bytes per glyph"
                  let struct (kept, detached, arena) = retained plain text lines
                  Expect.equal detached 0 "every line is a window of the two arrays"
                  Expect.isLessThanOrEqual (float kept / float arena) 45.0 $"{float kept / float arena:F1} retained bytes per glyph"
              }
              test "the Little Prince with layout takes at most 45 bytes per glyph and keeps 45" {
                  let struct (_, layout) = faces.Value
                  let text = (read "perf/texts/en-thelittleprince.txt").ToArray()
                  let lines = linesOf text
                  let struct (_, perGlyph, _) = measured layout text lines
                  Expect.isLessThanOrEqual perGlyph 45.0 $"{perGlyph:F1} bytes per glyph"
                  let struct (kept, detached, arena) = retained layout text lines
                  Expect.equal detached 0 "every line is a window of the two arrays"
                  Expect.isLessThanOrEqual (float kept / float arena) 45.0 $"{float kept / float arena:F1} retained bytes per glyph"
              }
              test "the word list takes at most 52 bytes per glyph and 400 bytes per one-word line, with and without layout" {
                  let struct (plain, layout) = faces.Value
                  let text = (read "perf/texts/en-words.txt").ToArray()
                  let lines = linesOf text
                  let struct (_, plainGlyph, plainLine) = measured plain text lines
                  let struct (_, layoutGlyph, layoutLine) = measured layout text lines
                  Expect.isLessThanOrEqual plainGlyph 52.0 $"{plainGlyph:F1} bytes per glyph without layout"
                  Expect.isLessThanOrEqual plainLine 400.0 $"{plainLine:F0} bytes per line without layout"
                  Expect.isLessThanOrEqual layoutGlyph 52.0 $"{layoutGlyph:F1} bytes per glyph with layout"
                  Expect.isLessThanOrEqual layoutLine 400.0 $"{layoutLine:F0} bytes per line with layout"
                  let struct (kept, detached, arena) = retained layout text lines
                  Expect.equal detached 0 "every line is a window of the two arrays"
                  Expect.isLessThanOrEqual (float kept / float arena) 50.0 $"{float kept / float arena:F1} retained bytes per glyph"
              }
              test "a line of 1, 7 and 47 glyphs costs at most 120, 360 and 2200 bytes as one line of a document" {
                  let struct (plain, layout) = faces.Value
                  let bound (glyphs: int) = match glyphs with | 1 -> 120.0 | 7 -> 360.0 | _ -> 2200.0
                  for glyphs in [ 1; 7; 47 ] do
                      let struct (text, lines) = repeated ("the quick brown fox jumps over a lazy dog while seven kernels grow again and again".Substring(0, glyphs)) 200
                      for font in [ plain; layout ] do
                          let struct (_, _, perLine) = measured font text lines
                          Expect.isLessThanOrEqual perLine (bound glyphs) $"{glyphs} glyphs: {perLine:F0} bytes per line"
              }
              test "a line the substitution engine changes takes at most 45 bytes per glyph, and a marked line the general route at most 195" {
                  let struct (_, layout) = faces.Value
                  let struct (ligatures, ligatureLines) = repeated "the first official office staff were fine but the flowers fluffed after a shift and the fox found the field" 100
                  let struct (_, ligaturePerGlyph, _) = measured layout ligatures ligatureLines
                  Expect.isLessThanOrEqual ligaturePerGlyph 45.0 $"{ligaturePerGlyph:F1} bytes per glyph"
                  let struct (marked, markedLines) = repeated "the quick brown fox jumps over a lazy dog and x\u0323 again" 100
                  let struct (_, markedPerGlyph, _) = measured layout marked markedLines
                  Expect.isLessThanOrEqual markedPerGlyph 195.0 $"{markedPerGlyph:F1} bytes per glyph"
              }
              test "Latin lines in a face that maps few Latin letters take at most 50 bytes per glyph and keep 45, as in a face that maps them all" {
                  let devanagari = Font.create (Face.create (read "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf") 0u)
                  let text = (read "perf/texts/en-thelittleprince.txt").ToArray()
                  let lines = linesOf text
                  let struct (glyphs, perGlyph, _) = measured devanagari text lines
                  Expect.isGreaterThan glyphs 80000 "the whole text"
                  Expect.isLessThanOrEqual perGlyph 50.0 $"{perGlyph:F1} bytes per glyph"
                  let struct (kept, detached, arena) = retained devanagari text lines
                  Expect.equal detached 0 "every line is a window of the two arrays"
                  Expect.isLessThanOrEqual (float kept / float arena) 45.0 $"{float kept / float arena:F1} retained bytes per glyph"
              }
              test "Devanagari word lines, on the general route, take at most 270 bytes per glyph as lines of a document" {
                  let devanagari = Font.create (Face.create (read "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf") 0u)
                  let struct (text, lines) = repeated "\u0928\u092E\u0938\u094D\u0924\u0947" 400
                  let struct (glyphs, perGlyph, _) = measured devanagari text lines
                  Expect.isGreaterThan glyphs 1000 "the lines have glyphs"
                  Expect.isLessThanOrEqual perGlyph 270.0 $"{perGlyph:F1} bytes per glyph"
              }
              testSequenced <| test "on four threads the Little Prince and the word list take at most 100 bytes per glyph in all threads" {
                  let struct (_, layout) = faces.Value
                  for name in [ "perf/texts/en-thelittleprince.txt"; "perf/texts/en-words.txt" ] do
                      let text = (read name).ToArray()
                      let struct (glyphs, fewest) = parallelBytes layout text (linesOf text)
                      let perGlyph = float fewest / float glyphs
                      Expect.isLessThanOrEqual perGlyph 100.0 $"{name}: {perGlyph:F1} bytes per glyph"
              } ])
