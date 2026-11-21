module Farfbuzz.Bench.Runner.Impl

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open System.Text
open Farfbuzz
open Farfbuzz.Bench.Runner.Protocol

/// <summary>The shaped text of a line that failed to shape, so that the hash of a failing run differs from a good one.</summary>
[<Literal>]
let private Failed = "shaping failed"

/// <summary>A Farfbuzz pair: the font and the text, built once.</summary>
type private FarfbuzzCase =
    { Font: Font
      Text: byte array
      Lines: struct (int * int) array }

/// <summary>Shapes one line through Farfbuzz, threading the plan cache, as P9.S01 does.</summary>
/// <param name="cache">The plan cache from the previous line.</param>
/// <param name="case">The pair.</param>
/// <param name="index">The line.</param>
/// <returns>The shaped run and the cache to keep.</returns>
let private farfbuzzLine (cache: PlanCache) (case: FarfbuzzCase) (index: int) =
    let struct (start, length) = case.Lines[index]
    let run = GlyphRun.addUtf8 (ReadOnlySpan(case.Text, start, case.Text.Length - start)) 0 length GlyphRun.empty
    let run = GlyphRun.guessSegmentProperties Language.cLocale run
    Shape.shapeCached cache case.Font ImmutableArray<Feature>.Empty ValueNone run |> settle

/// <summary>Serialises the glyphs of a shaped run as the text of P9.S01.</summary>
/// <param name="run">The run.</param>
/// <returns>The text.</returns>
let private serialised (run: GlyphRun) =
    Serialize.glyphs SerializeFlags.Default (fun _ -> ValueNone) (fun _ -> ValueNone) (run.Infos.AsSpan()) (run.Positions.AsSpan()) |> settle

/// <summary>Shapes every line, threading the plan cache, and counts the glyphs.</summary>
/// <param name="case">The pair.</param>
/// <param name="index">The next line.</param>
/// <param name="cache">The plan cache.</param>
/// <param name="glyphs">The glyphs so far.</param>
/// <returns>The glyphs of the pass.</returns>
[<TailCall>]
let rec private farfbuzzPassFrom (case: FarfbuzzCase) (index: int) (cache: PlanCache) (glyphs: int) =
    match index = case.Lines.Length with
    | true -> glyphs
    | false ->
        let struct (shaped, next) = farfbuzzLine cache case index
        match shaped with
        | Ok run -> farfbuzzPassFrom case (index + 1) next (glyphs + run.Infos.Length)
        | Error _ -> farfbuzzPassFrom case (index + 1) next glyphs

/// <summary>Hashes the serialised output of every line, each followed by a newline.</summary>
/// <param name="case">The pair.</param>
/// <param name="index">The next line.</param>
/// <param name="cache">The plan cache.</param>
/// <param name="hash">The running hash.</param>
/// <param name="glyphs">The glyphs so far.</param>
/// <returns>The hash and the glyphs of the pass.</returns>
[<TailCall>]
let rec private farfbuzzHashFrom (case: FarfbuzzCase) (index: int) (cache: PlanCache) (hash: uint64) (glyphs: int) =
    match index = case.Lines.Length with
    | true -> struct (hash, glyphs)
    | false ->
        let struct (shaped, next) = farfbuzzLine cache case index
        let struct (text, count) =
            match shaped with
            | Ok run -> struct (serialised run, run.Infos.Length)
            | Error _ -> struct (Failed, 0)
        farfbuzzHashFrom case (index + 1) next (foldBytes hash (Encoding.UTF8.GetBytes(text + "\n")) 0) (glyphs + count)

/// <summary>Farfbuzz, with a font built once per pair.</summary>
let farfbuzz =
    { Name = "farfbuzz"
      Prepare = fun pair -> { Font = Font.create (Face.create (ReadOnlyMemory pair.Font) 0u); Text = pair.Text; Lines = pair.Lines } :> obj
      Pass = fun state -> farfbuzzPassFrom (unbox state) 0 PlanCache.empty 0
      Hash = fun state -> farfbuzzHashFrom (unbox state) 0 PlanCache.empty FnvOffsetBasis 0 }

/// <summary>What one line shaped to: its glyph count and its serialised text, or the error.</summary>
type LineOutcome = { Glyphs: int; Text: string }

/// <summary>Shapes every line of a prepared pair once, threading the plan cache, and keeps each line's outcome.</summary>
/// <param name="case">The pair.</param>
/// <param name="index">The next line.</param>
/// <param name="cache">The plan cache.</param>
/// <param name="found">The outcomes so far, last first.</param>
/// <returns>The outcomes in line order.</returns>
[<TailCall>]
let rec private outcomesFrom (case: FarfbuzzCase) (index: int) (cache: PlanCache) (found: LineOutcome list) =
    match index = case.Lines.Length with
    | true -> found |> List.rev |> List.toArray
    | false ->
        let struct (shaped, next) = farfbuzzLine cache case index
        let outcome =
            match shaped with
            | Ok run -> { Glyphs = run.Infos.Length; Text = serialised run }
            | Error problem -> { Glyphs = 0; Text = $"{Failed}: {problem}" }
        outcomesFrom case (index + 1) next (outcome :: found)

/// <summary>Describes the first difference between two passes over the same lines.</summary>
/// <param name="first">The outcomes of the first pass.</param>
/// <param name="second">The outcomes of the second pass.</param>
/// <param name="index">The next line to compare.</param>
/// <param name="report">The report so far.</param>
/// <returns>A report of every differing line, or an empty text when the passes agree.</returns>
[<TailCall>]
let rec private differences (first: LineOutcome array) (second: LineOutcome array) (index: int) (report: string) =
    match index = first.Length with
    | true -> report
    | false ->
        match first[index].Text = second[index].Text with
        | true -> differences first second (index + 1) report
        | false ->
            let left = first[index].Text.Split '|'
            let right = second[index].Text.Split '|'
            let glyph =
                Seq.zip left right
                |> Seq.tryFindIndex (fun (a, b) -> a <> b)
                |> Option.defaultValue (min left.Length right.Length)
            let at (parts: string array) = if glyph < parts.Length then parts[glyph] else "(end)"
            differences first second (index + 1) $"{report}line {index}: {first[index].Glyphs} against {second[index].Glyphs} glyphs, first differing glyph {glyph}: {at left} against {at right}\n"

/// <summary>Shapes a pair cold (a font just built), warm (the same font again) and cold again (another font just built) and reports every differing line.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The report, empty text when all three passes agree.</returns>
let diagnose (pair: Pair) =
    let state = farfbuzz.Prepare pair |> unbox<FarfbuzzCase>
    let cold = outcomesFrom state 0 PlanCache.empty []
    let warm = outcomesFrom state 0 PlanCache.empty []
    let again = farfbuzz.Prepare pair |> unbox<FarfbuzzCase> |> fun fresh -> outcomesFrom fresh 0 PlanCache.empty []
    let totals (outcomes: LineOutcome array) = outcomes |> Array.sumBy _.Glyphs
    let coldWarm = differences cold warm 0 String.Empty
    let warmAgain = differences warm again 0 String.Empty
    $"glyphs cold {totals cold}, warm {totals warm}, fresh font {totals again}\ncold against warm:\n{coldWarm}fresh font against warm:\n{warmAgain}"

/// <summary>Shapes one line of a prepared pair, threading no cache.</summary>
/// <param name="case">The pair.</param>
/// <param name="line">The line.</param>
/// <returns>The outcome of the line.</returns>
let private lineOutcome (case: FarfbuzzCase) (line: int) =
    let struct (shaped, _) = farfbuzzLine PlanCache.empty case line
    match shaped with
    | Ok run -> { Glyphs = run.Infos.Length; Text = serialised run }
    | Error problem -> { Glyphs = 0; Text = $"{Failed}: {problem}" }

/// <summary>Shapes one line under a collection mode.</summary>
/// <param name="mode">plain, or collect (a full collection before the line).</param>
/// <param name="case">The pair.</param>
/// <param name="line">The line.</param>
/// <returns>The outcome and the generation 0 collections that ran during the line.</returns>
let private lineUnder (mode: string) (case: FarfbuzzCase) (line: int) =
    match mode.Contains "collect" with
    | true -> GC.Collect()
    | false -> ()
    let began = GC.CollectionCount 0
    let outcome = lineOutcome case line
    struct (outcome, GC.CollectionCount 0 - began)

/// <summary>Shapes one line of a pair on a font just built, three times, and once more on the first font, under a collection mode.</summary>
/// <param name="pair">The pair.</param>
/// <param name="line">The line.</param>
/// <param name="mode">plain or collect.</param>
/// <returns>The report.</returns>
let diagnoseLine (pair: Pair) (line: int) (mode: string) =
    let first = farfbuzz.Prepare pair |> unbox<FarfbuzzCase>
    let struct (cold, collections) = lineUnder mode first line
    let warm = lineOutcome first line
    let report (label: string) (outcome: LineOutcome) =
        $"{label}: {outcome.Glyphs} glyphs, same text as the warm shaping: {outcome.Text = warm.Text}\n"
    let others =
        [ 1; 2; 3 ]
        |> List.map (fun n ->
            let fresh = farfbuzz.Prepare pair |> unbox<FarfbuzzCase>
            let struct (outcome, collected) = lineUnder mode fresh line
            report $"fresh font {n} ({collected} generation 0 collections)" outcome)
    $"mode {mode}\n" + report $"cold ({collections} generation 0 collections)" cold + report "warm" warm + String.Join(String.Empty, others)

/// <summary>The first and second shaping of a slice of the text on a font just built: cold, then warm.</summary>
/// <param name="pair">The pair.</param>
/// <param name="start">The first byte.</param>
/// <param name="finish">The byte after the last.</param>
/// <returns>The cold outcome and the warm outcome.</returns>
let private coldAndWarm (pair: Pair) (start: int) (finish: int) =
    let case = farfbuzz.Prepare pair |> unbox<FarfbuzzCase>
    let slice = { case with Lines = [| struct (start, finish - start) |] }
    let cold = lineOutcome slice 0
    let warm = lineOutcome slice 0
    struct (cold, warm)

/// <summary>Whether the cold and the warm shaping of a slice differ.</summary>
/// <param name="pair">The pair.</param>
/// <param name="start">The first byte.</param>
/// <param name="finish">The byte after the last.</param>
/// <returns>True when they differ.</returns>
let private differs (pair: Pair) (start: int) (finish: int) =
    let struct (cold, warm) = coldAndWarm pair start finish
    cold.Text <> warm.Text

/// <summary>Narrows two indices of the code point boundaries of a line until the failing one is next to the passing one.</summary>
/// <param name="fails">Whether a pair of boundary indices fails, given the index being moved.</param>
/// <param name="passing">The index that passes.</param>
/// <param name="failing">The index that fails.</param>
/// <returns>The failing index next to a passing one.</returns>
[<TailCall>]
let rec private narrow (fails: int -> bool) (passing: int) (failing: int) =
    match abs (failing - passing) <= 1 with
    | true -> failing
    | false ->
        let middle = (passing + failing) / 2
        match fails middle with
        | true -> narrow fails passing middle
        | false -> narrow fails middle failing

/// <summary>Finds the smallest slice of a line whose cold shaping differs from its warm shaping: the shortest prefix that fails, then the
/// shortest suffix of that prefix that still fails, each found by halving.</summary>
/// <param name="pair">The pair.</param>
/// <param name="line">The line.</param>
/// <returns>The report.</returns>
let minimise (pair: Pair) (line: int) =
    let struct (lineStart, lineLength) = pair.Lines[line]
    let boundaries =
        [| for offset in lineStart .. lineStart + lineLength do
               if offset = lineStart + lineLength || (pair.Text[offset] &&& 0xC0uy) <> 0x80uy then yield offset |]
    let last = boundaries.Length - 1
    match differs pair boundaries[0] boundaries[last] with
    | false -> "the whole line shapes the same cold and warm"
    | true ->
        let finish = narrow (fun index -> differs pair boundaries[0] boundaries[index]) 0 last
        let start = narrow (fun index -> differs pair boundaries[index] boundaries[finish]) finish 0
        let struct (cold, warm) = coldAndWarm pair boundaries[start] boundaries[finish]
        let text = Encoding.UTF8.GetString(pair.Text, boundaries[start], boundaries[finish] - boundaries[start])
        let points = text.EnumerateRunes() |> Seq.map (fun rune -> $"U+{rune.Value:X4}") |> String.concat " "
        $"smallest failing slice: code points {start} to {finish} of line {line}, {finish - start} code points: {points}\ncold {cold.Glyphs} glyphs: {cold.Text}\nwarm {warm.Glyphs} glyphs: {warm.Text}"

#if BENCH_NATIVE
/// <summary>A HarfBuzzSharp pair: the font, one reused buffer, the text.</summary>
type private NativeCase =
    { Font: HarfBuzzSharp.Font
      Buffer: HarfBuzzSharp.Buffer
      Text: byte array
      Lines: struct (int * int) array }

/// <summary>Shapes one line through HarfBuzzSharp, reusing the buffer.</summary>
/// <param name="case">The pair.</param>
/// <param name="index">The line.</param>
let private nativeLine (case: NativeCase) (index: int) =
    let struct (start, length) = case.Lines[index]
    case.Buffer.ClearContents()
    case.Buffer.AddUtf8(ReadOnlySpan(case.Text, start, case.Text.Length - start), 0, length)
    case.Buffer.GuessSegmentProperties()
    case.Font.Shape case.Buffer

/// <summary>Shapes every line and counts the glyphs.</summary>
/// <param name="case">The pair.</param>
/// <param name="index">The next line.</param>
/// <param name="glyphs">The glyphs so far.</param>
/// <returns>The glyphs of the pass.</returns>
[<TailCall>]
let rec private nativePassFrom (case: NativeCase) (index: int) (glyphs: int) =
    match index = case.Lines.Length with
    | true -> glyphs
    | false ->
        nativeLine case index
        nativePassFrom case (index + 1) (glyphs + case.Buffer.Length)

/// <summary>Reads the glyphs of the shaped buffer as Farfbuzz's types and serialises them as P9.S01 does.</summary>
/// <param name="case">The pair, its buffer holding the shaped line.</param>
/// <returns>The text.</returns>
let private nativeSerialised (case: NativeCase) =
    let infos =
        case.Buffer.GlyphInfos
        |> Array.map (fun info -> { Codepoint = info.Codepoint; Mask = info.Mask; Cluster = info.Cluster; Var1 = 0u; Var2 = 0u })
    let positions =
        case.Buffer.GlyphPositions
        |> Array.map (fun position -> { XAdvance = position.XAdvance; YAdvance = position.YAdvance; XOffset = position.XOffset; YOffset = position.YOffset; Var = 0u })
    Serialize.glyphs SerializeFlags.Default (fun _ -> ValueNone) (fun _ -> ValueNone) (ReadOnlySpan infos) (ReadOnlySpan positions) |> settle

/// <summary>Hashes the serialised output of every line, each followed by a newline.</summary>
/// <param name="case">The pair.</param>
/// <param name="index">The next line.</param>
/// <param name="hash">The running hash.</param>
/// <param name="glyphs">The glyphs so far.</param>
/// <returns>The hash and the glyphs of the pass.</returns>
[<TailCall>]
let rec private nativeHashFrom (case: NativeCase) (index: int) (hash: uint64) (glyphs: int) =
    match index = case.Lines.Length with
    | true -> struct (hash, glyphs)
    | false ->
        nativeLine case index
        nativeHashFrom case (index + 1) (foldBytes hash (Encoding.UTF8.GetBytes(nativeSerialised case + "\n")) 0) (glyphs + case.Buffer.Length)

/// <summary>Builds the HarfBuzzSharp font of a pair from its bytes.</summary>
/// <param name="pair">The pair.</param>
/// <returns>The case.</returns>
let private nativePrepare (pair: Pair) =
    let pointer = Marshal.AllocHGlobal pair.Font.Length
    Marshal.Copy(pair.Font, 0, pointer, pair.Font.Length)
    let blob = new HarfBuzzSharp.Blob(pointer, pair.Font.Length, HarfBuzzSharp.MemoryMode.ReadOnly, fun () -> Marshal.FreeHGlobal pointer)
    let font = new HarfBuzzSharp.Font(new HarfBuzzSharp.Face(blob, 0))
    font.SetFunctionsOpenType()
    { Font = font; Buffer = new HarfBuzzSharp.Buffer(); Text = pair.Text; Lines = pair.Lines } :> obj

#if BENCH_FACADE
/// <summary>The Farfbuzz facade, shaping through HarfBuzzSharp's public API as Avalonia does: the code of the native implementation, built against the facade.</summary>
let native =
    { Name = "facade"
#else
/// <summary>HarfBuzzSharp 14.4.0 with its bundled HarfBuzz, shaping with the no-list overload (shaper <c>ot</c>).</summary>
let native =
    { Name = "native"
#endif
      Prepare = nativePrepare
      Pass = fun state -> nativePassFrom (unbox state) 0 0
      Hash = fun state -> nativeHashFrom (unbox state) 0 FnvOffsetBasis 0 }

/// <summary>The implementations this build offers.</summary>
let all = [ farfbuzz; native ]
#else
/// <summary>The implementations this build offers.</summary>
let all = [ farfbuzz ]
#endif
