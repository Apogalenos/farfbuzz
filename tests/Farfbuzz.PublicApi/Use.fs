module Farfbuzz.PublicApi.Use

open System
open System.Collections.Immutable
open System.IO
open Expecto
open Farfbuzz

/// <summary>A font of the test corpus, found from the folder the tests run in.</summary>
/// <returns>The path of Roboto Regular.</returns>
let private robotoPath () =
    let rec up (directory: DirectoryInfo) =
        let candidate = Path.Combine(directory.FullName, "upstream", "harfbuzz", "perf", "fonts", "Roboto-Regular.ttf")
        match File.Exists candidate with
        | true -> candidate
        | false -> up (directory.Parent |> nonNull)
    up (DirectoryInfo AppContext.BaseDirectory)

/// <summary>The font of the tests.</summary>
let private font = lazy (Font.create (Face.create (ReadOnlyMemory (File.ReadAllBytes (robotoPath ()))) 0u))

/// <summary>A line decoded from text, its segment properties guessed.</summary>
/// <param name="text">The text.</param>
/// <returns>The run.</returns>
let private line (text: string) =
    let bytes = Text.Encoding.UTF8.GetBytes text
    GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty
    |> GlyphRun.guessSegmentProperties Language.cLocale

/// <summary>The glyph ids and advances of a shaped run.</summary>
/// <param name="run">The run.</param>
/// <returns>One pair for each glyph.</returns>
let private glyphs (run: GlyphRun) =
    Seq.zip (GlyphRun.infos run) (GlyphRun.positions run) |> Seq.map (fun (info, position) -> info.Codepoint, position.XAdvance) |> List.ofSeq

/// <summary>The cases of the extras of the two-way design (PLAN.md decision 14), written against the public API only.</summary>
[<Tests>]
let tests =
    testList "PublicApi" [
        test "one line shapes to glyphs of the font" {
            match Shape.shape font.Value ImmutableArray.Empty (line "Hello, world") with
            | Ok run ->
                Expect.equal (GlyphRun.infos run).Length 12 "one glyph for each character"
                Expect.isTrue (glyphs run |> List.forall (fun (glyph, advance) -> glyph > 0u && advance > 0)) "each has a glyph and an advance"
            | Error error -> failtest $"shaping failed: {error}"
        }
        test "one font shared by the lines of a document shapes each line as it shapes alone" {
            let lines = [ "Hello, world"; "AVATAR"; "fi fl ffi"; "the quick brown fox" ]
            let text = Text.Encoding.UTF8.GetBytes (String.Join("\n", lines))
            let boundaries =
                lines
                |> List.scan (fun (start, _) item -> start + item.Length + 1, item.Length) (0, 0)
                |> List.tail
                |> List.map (fun (next, length) -> struct (next - length - 1, length))
            let struct (document, _) =
                LineShape.utf8 1 PlanCache.empty font.Value ImmutableArray.Empty ValueNone Language.cLocale GlyphRun.empty (ReadOnlyMemory text) (ImmutableArray.CreateRange boundaries)
            lines
            |> List.iteri (fun index text ->
                match Shape.shape font.Value ImmutableArray.Empty (line text) with
                | Ok alone ->
                    Expect.equal ((ShapedDocument.infosOf document index).ToArray() |> Array.map (fun info -> info.Codepoint)) (Seq.toArray (GlyphRun.infos alone) |> Array.map (fun info -> info.Codepoint)) $"glyphs of line {index}"
                    Expect.equal ((ShapedDocument.positionsOf document index).ToArray()) (Seq.toArray (GlyphRun.positions alone)) $"positions of line {index}"
                | Error error -> failtest $"line {index}: {error}")
        }
        test "a shaping error is a value" {
            match Shape.shapeFull font.Value ImmutableArray.Empty (ValueSome (ImmutableArray.Create "nonexistent")) (line "abc") with
            | Ok _ -> failtest "an unknown shaper cannot shape"
            | Error error -> Expect.isTrue (error.ToString().Length > 0) "the error says what failed"
        }
        test "a kept plan shapes as a fresh one does" {
            let run = line "Office waffles: fluffy 12.5% off"
            match ShapePlan.create font.Value (GlyphRun.segmentProperties run) ImmutableArray.Empty ValueNone with
            | Error error -> failtest $"no plan: {error}"
            | Ok plan ->
                let once = Shape.shapeWithPlan font.Value plan ImmutableArray.Empty run
                let again = Shape.shapeWithPlan font.Value plan ImmutableArray.Empty run
                let fresh = Shape.shape font.Value ImmutableArray.Empty run
                match once, again, fresh with
                | Ok a, Ok b, Ok c ->
                    Expect.equal (glyphs a) (glyphs c) "the plan equals a fresh shape"
                    Expect.equal (glyphs b) (glyphs c) "and again"
                | _ -> failtest "shaping failed"
        }
        test "the public surface is the pinned one" {
            let extra, missing = Surface.differences (Path.Combine(AppContext.BaseDirectory, "public-api.baseline"))
            let describe (title: string) (lines: string list) = match lines with | [] -> "" | _ -> $"{title}:\n  " + String.Join("\n  ", List.truncate 40 lines) + "\n"
            Expect.isTrue (extra.IsEmpty && missing.IsEmpty) (describe "public but not pinned" extra + describe "pinned but not public" missing)
        }
    ]
