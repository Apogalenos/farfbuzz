module Farfbuzz.Tests.AllocationTests

open System
open System.Collections.Immutable
open System.Globalization
open System.IO
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Whether the byte counter of <see cref="bytesAllocatedBy"/> is meaningful: CoreCLR only. On Mono the
/// number is reported, never asserted, so the whole list is empty there.</summary>
let private isCoreClr =
    not (OperatingSystem.IsBrowser())
    && not (RuntimeInformation.FrameworkDescription.Contains("Mono", StringComparison.Ordinal))

/// <summary>The line terminator byte the pair texts split on, the byte <c>benchmark-shape.cc</c>'s <c>memchr</c> scans for.</summary>
[<Literal>]
let private Newline = '\n'B

/// <summary>A font/text pair of upstream's perf/benchmark-shape.cc, paths relative to upstream/harfbuzz.</summary>
type private Pair = { Font: string; Text: string }

/// <summary>The pair's name, the font and text file names as the benchmark prints them.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The name.</returns>
let private nameOf (pair: Pair) = $"{Path.GetFileName pair.Font}/{Path.GetFileName pair.Text}"

/// <summary>The ten pairs of perf/benchmark-shape.cc at HarfBuzz 15.1.0, in upstream order.</summary>
let private pairs =
    [ { Font = "perf/fonts/Roboto-Regular.ttf"; Text = "perf/texts/en-thelittleprince.txt" }
      { Font = "perf/fonts/Roboto-Regular.ttf"; Text = "perf/texts/en-words.txt" }
      { Font = "test/subset/data/fonts/SourceSerifVariable-Roman.ttf"; Text = "perf/texts/react-dom.txt" }
      { Font = "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf"; Text = "perf/texts/hi-words.txt" }
      { Font = "perf/fonts/Amiri-Regular.ttf"; Text = "perf/texts/fa-thelittleprince.txt" }
      { Font = "perf/fonts/NotoNastaliqUrdu-Regular.ttf"; Text = "perf/texts/fa-thelittleprince.txt" }
      { Font = "perf/fonts/NotoNastaliqUrdu-Regular.ttf"; Text = "perf/texts/fa-words.txt" }
      { Font = "perf/fonts/Gulzar-Regular.ttf"; Text = "perf/texts/fa-thelittleprince.txt" }
      { Font = "perf/fonts/Gulzar-Regular.ttf"; Text = "perf/texts/fa-words.txt" }
      { Font = "perf/fonts/NotoSansDuployan-Regular.otf"; Text = "perf/texts/duployan.txt" } ]

/// <summary>The Roboto face for the cmap and advance primitives.</summary>
let private roboto = lazy (Font.create (Face.create (read "perf/fonts/Roboto-Regular.ttf") 0u))

/// <summary>The Noto Nastaliq face for the glyph-props primitive; Arabic fonts carry a GDEF with glyph classes.</summary>
let private nastaliq = lazy (Font.create (Face.create (read "test/api/fonts/NotoNastaliqUrdu-Regular.ttf") 0u))

/// <summary>Splits a text into its <c>\n</c>-terminated lines as benchmark-shape.cc does: the bytes after the last
/// newline are not a line.</summary>
/// <param name="text">The text.</param>
/// <returns>The (start, byteLength) of each line, the newline excluded.</returns>
let private lineRanges (text: byte array) =
    let ranges = ResizeArray<int * int>()
    let mutable start = 0
    let mutable newline = Array.IndexOf(text, Newline, start)
    while newline >= 0 do
        ranges.Add(start, newline - start)
        start <- newline + 1
        newline <- Array.IndexOf(text, Newline, start)
    ranges.ToArray()

/// <summary>Shapes every line of a pair once, reusing one plan across lines as the benchmark does, and returns the
/// glyph count.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The whole text; the bytes after each line are its post-context.</param>
/// <param name="lines">The line ranges.</param>
/// <returns>The glyphs shaped.</returns>
let private shapePass (font: Font) (text: byte array) (lines: (int * int) array) =
    let mutable cache = PlanCache.empty
    let mutable glyphs = 0
    for (start, length) in lines do
        let run = GlyphRun.addUtf8 (ReadOnlySpan(text, start, text.Length - start)) 0 length GlyphRun.empty
        let run = GlyphRun.guessSegmentProperties Language.cLocale run
        let struct (shaped, next) = Shape.shapeCached cache font ImmutableArray<Feature>.Empty ValueNone run
        cache <- next
        match shaped with
        | Ok shaped -> glyphs <- glyphs + shaped.Infos.Length
        | Error (OutlinesNotPorted _)
        | Error (NoShaperSucceeded _)
        | Error (VerifyFailed _) -> ()
    glyphs

/// <summary>Loads a pair's font, text bytes and line ranges.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The font, text and lines.</returns>
let private loadPair (pair: Pair) =
    let font = Font.create (Face.create (read pair.Font) 0u)
    let text = (read pair.Text).ToArray()
    font, text, lineRanges text

/// <summary>The committed allocation baseline, one <c>pair&lt;TAB&gt;bytesPerGlyph</c> line per measured pair.</summary>
/// <returns>The file path.</returns>
let private baselinePath () = Path.Combine(RepositoryPaths.root (), "docs", "research", "allocation-baseline.tsv")

/// <summary>Reads the committed allocation baseline.</summary>
/// <returns>The bytes per glyph by pair name.</returns>
let private readBaselines () =
    let path = baselinePath ()
    match File.Exists path with
    | false -> Map.empty
    | true ->
        File.ReadAllLines path
        |> Array.filter (fun line -> line.Length > 0 && not (line.StartsWith "#"))
        |> Array.fold (fun map line ->
            let parts = line.Split '\t'
            match parts.Length = 2 with
            | true -> Map.add parts[0] (Double.Parse(parts[1], CultureInfo.InvariantCulture)) map
            | false -> map) Map.empty

/// <summary>Formats a bytes-per-glyph value for the baseline file, invariant and round-tripping.</summary>
/// <param name="value">The value.</param>
/// <returns>The text.</returns>
let private formatBytesPerGlyph (value: float) = value.ToString("R", CultureInfo.InvariantCulture)

/// <summary>Writes a pair's bytes per glyph into the baseline, keeping the other entries and the header.</summary>
/// <param name="name">The pair name.</param>
/// <param name="bytesPerGlyph">The measured bytes per glyph.</param>
let private writeBaseline (name: string) (bytesPerGlyph: float) =
    let path = baselinePath ()
    let header =
        match File.Exists path with
        | false -> [ "# bytes per glyph of one full pass over the upstream pair, measured on CoreCLR x64 (B/glyph)"
                     "# written by the P9.S03 tests on their first run; a slice that lowers allocation lowers a value in the same commit" ]
        | true -> File.ReadAllLines path |> Array.filter (fun line -> line.StartsWith "#") |> Array.toList
    let entries = readBaselines () |> Map.add name bytesPerGlyph
    let lines =
        header
        @ (entries |> Map.toSeq |> Seq.sortBy fst |> Seq.map (fun (n, v) -> $"{n}\t{formatBytesPerGlyph v}") |> Seq.toList)
    File.WriteAllLines(path, lines)

/// <summary>Measures the bytes one pass allocates: one warm-up pass, then the least of three measured passes, so a
/// single pass spoiled by a lazy value or a tiered recompilation does not set the baseline.</summary>
/// <param name="action">The pass.</param>
/// <returns>The least measured allocation.</returns>
let private bestPassBytes (action: unit -> unit) =
    action ()
    let mutable best = Int64.MaxValue
    for _ in 1 .. 3 do
        let before = GC.GetAllocatedBytesForCurrentThread()
        action ()
        let allocated = GC.GetAllocatedBytesForCurrentThread() - before
        if allocated < best then best <- allocated
    best

/// <summary>The zero-allocation tests of the six primitives, each exact 0 bytes over 100 000 calls.</summary>
let private primitiveTests =
    [ test "digest lookup allocates nothing over 100 000 calls" {
          let digest = Digest.addRange 0u 1023u Digest.empty
          let mutable hits = 0
          let probe () =
              for glyph in 1u .. 100_000u do
                  if Digest.mayHaveCall glyph digest then hits <- hits + 1
          Expect.equal (bytesAllocatedBy probe) 0L "bytes allocated"
          Expect.isGreaterThan hits 0 "the digest answers mayHave"
      }
      test "coverage lookup allocates nothing over 100 000 calls" {
          let bytes =
              [| 0x00uy; 0x01uy; 0x00uy; 0x05uy; 0x00uy; 0x01uy; 0x00uy; 0x02uy; 0x00uy; 0x03uy; 0x00uy; 0x04uy; 0x00uy; 0x05uy |]
          let coverage = Coverage.parse (ReadOnlySpan bytes) 0
          let mutable hits = 0
          let probe () =
              for glyph in 1u .. 100_000u do
                  match Coverage.index coverage (glyph % 8u) with
                  | Covered _ -> hits <- hits + 1
                  | NotCovered -> ()
          Expect.equal (bytesAllocatedBy probe) 0L "bytes allocated"
          Expect.isGreaterThan hits 0 "the coverage answers Covered"
      }
      test "class lookup allocates nothing over 100 000 calls" {
          let bytes = [| 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x03uy; 0x00uy; 0x01uy; 0x00uy; 0x02uy; 0x00uy; 0x03uy |]
          let classDef = ClassDef.parse (ReadOnlySpan bytes) 0
          let mutable nonZero = 0
          let probe () =
              for glyph in 0u .. 100_000u do
                  if ClassDef.classOf classDef (glyph % 4u) <> 0u then nonZero <- nonZero + 1
          Expect.equal (bytesAllocatedBy probe) 0L "bytes allocated"
          Expect.isGreaterThan nonZero 0 "the class definition answers non-zero classes"
      }
      test "cmap lookup allocates nothing over 100 000 calls" {
          let cmap = roboto.Value.Ot.Cmap.Value
          let mutable found = 0
          let probe () =
              for _ in 1 .. 100_000 do
                  match Cmap.nominalGlyph cmap 0x41u with
                  | ValueSome _ -> found <- found + 1
                  | ValueNone -> ()
          Expect.equal (bytesAllocatedBy probe) 0L "bytes allocated"
          Expect.isGreaterThan found 0 "the cmap maps 'A'"
      }
      test "glyph-props lookup allocates nothing over 100 000 calls" {
          let gdef = nastaliq.Value.Ot.Gdef.Value
          let mutable total = 0
          let probe () =
              for glyph in 0u .. 100_000u do
                  total <- total + int (Gdef.glyphProps gdef (glyph % 1000u))
          Expect.equal (bytesAllocatedBy probe) 0L "bytes allocated"
          Expect.isGreaterThan total 0 "the GDEF answers non-zero props"
      }
      test "advance lookup allocates nothing over 100 000 calls" {
          let font = roboto.Value
          let mutable total = 0
          let probe () =
              for glyph in 1u .. 100_000u do
                  total <- total + Font.hAdvance font ((glyph % 100u) + 1u)
          Expect.equal (bytesAllocatedBy probe) 0L "bytes allocated"
          Expect.isGreaterThan total 0 "the advances are positive"
      } ]

/// <summary>One allocation-ratchet test for a pair: shape the pair, then assert the bytes per glyph stay within 10 %
/// of the committed baseline; a pair with no baseline gets one written.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The test.</returns>
let private pairTest (pair: Pair) =
    let name = nameOf pair
    test name {
        let font, text, lines = loadPair pair
        let glyphs = shapePass font text lines
        match glyphs with
        | 0 -> failtest $"{name} shaped no glyphs"
        | _ ->
            let bytes = bestPassBytes (fun () -> shapePass font text lines |> ignore)
            let measured = float bytes / float glyphs
            match readBaselines () |> Map.tryFind name with
            | None -> writeBaseline name measured
            | Some baseline ->
                Expect.isLessThanOrEqual measured (baseline * 1.10) $"{name} allocates {measured:F2} B/glyph, above the baseline {baseline:F2} B/glyph × 1.10"
    }

/// <summary>The P9.S03 allocation ratchet and zero-allocation tests.</summary>
[<Tests>]
let p9s03Allocations =
    testList "P9.S03" (if isCoreClr then primitiveTests @ (pairs |> List.map pairTest) else [])
