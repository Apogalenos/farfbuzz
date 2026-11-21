module Farfbuzz.Tests.Shape.SyllableReuseTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x53594CUL

/// <summary>The lines each font draws in the main property, unless <c>SYLLABLE_REUSE_CASES</c> asks for more.</summary>
let private caseCount =
    match Environment.GetEnvironmentVariable "SYLLABLE_REUSE_CASES" with
    | null | "" -> 6000
    | text -> int text

/// <summary>One font of the properties, with the script its lines are given and the code points they are drawn from.</summary>
type private ReuseFont =
    { Name: string
      Path: string
      Script: Script
      Pools: (int * (int * int) list) list }

/// <summary>The code points every pool shares: the joiners, a space, a Latin letter, a digit, the dotted circle and the dandas.</summary>
let private shared =
    [ (4, [ (0x200C, 0x200D) ]); (4, [ (0x20, 0x20) ]); (1, [ (0x61, 0x61); (0x31, 0x31) ]); (1, [ (0x25CC, 0x25CC) ]); (1, [ (0x964, 0x965) ]) ]

/// <summary>The fonts: an Indic font of each script the upstream tests hold, Noto Sans Devanagari with a <c>calt</c> lookup after the
/// syllable lookups, and a variable Oriya subset with feature variations.</summary>
let private fonts =
    [ { Name = "Devanagari"
        Path = "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf"
        Script = Scripts.Devanagari
        Pools = [ (30, [ (0x915, 0x939) ]); (16, [ (0x93E, 0x94C) ]); (12, [ (0x94D, 0x94D) ]); (4, [ (0x93C, 0x93C) ]); (4, [ (0x901, 0x903) ]); (3, [ (0x905, 0x914) ]); (2, [ (0x958, 0x95F); (0x972, 0x97F) ]) ] @ shared }
      { Name = "Kannada"
        Path = "test/shape/data/text-rendering-tests/fonts/NotoSerifKannada-Regular.ttf"
        Script = Scripts.Kannada
        Pools = [ (30, [ (0xC95, 0xCB9) ]); (16, [ (0xCBE, 0xCCC) ]); (12, [ (0xCCD, 0xCCD) ]); (4, [ (0xC82, 0xC83) ]); (3, [ (0xC85, 0xC94) ]); (2, [ (0xCBC, 0xCBC); (0xCD5, 0xCD6) ]) ] @ shared }
      { Name = "Kannada sans"
        Path = "test/shape/data/text-rendering-tests/fonts/NotoSansKannada-Regular.ttf"
        Script = Scripts.Kannada
        Pools = [ (30, [ (0xC95, 0xCB9) ]); (16, [ (0xCBE, 0xCCC) ]); (12, [ (0xCCD, 0xCCD) ]); (4, [ (0xC82, 0xC83) ]); (3, [ (0xC85, 0xC94) ]); (2, [ (0xCBC, 0xCBC); (0xCD5, 0xCD6) ]) ] @ shared }
      { Name = "Telugu"
        Path = "test/subset/data/fonts/SreeKrushnadevaraya-Regular.ttf"
        Script = Scripts.Telugu
        Pools = [ (30, [ (0xC15, 0xC39) ]); (16, [ (0xC3E, 0xC4C) ]); (12, [ (0xC4D, 0xC4D) ]); (4, [ (0xC01, 0xC03) ]); (3, [ (0xC05, 0xC14) ]); (2, [ (0xC55, 0xC56) ]) ] @ shared }
      { Name = "Oriya"
        Path = "test/subset/data/fonts/NotoSansOriya-subset.ttf"
        Script = Scripts.Oriya
        Pools = [ (30, [ (0xB15, 0xB39) ]); (16, [ (0xB3E, 0xB4C) ]); (12, [ (0xB4D, 0xB4D) ]); (4, [ (0xB3C, 0xB3C); (0xB01, 0xB03) ]); (3, [ (0xB05, 0xB14) ]) ] @ shared } ]

/// <summary>Generates a code point of a font's pools.</summary>
/// <param name="font">The font.</param>
/// <returns>The generator.</returns>
let private genCodepoint (font: ReuseFont) =
    font.Pools
    |> List.map (fun (weight, ranges) -> weight, Gen.oneof (ranges |> List.map (fun (first, last) -> Gen.choose (first, last))))
    |> Gen.frequency
    |> Gen.map uint32

/// <summary>Generates a line: mostly words of a few syllables, now and then a long line or one glyph, with the flags and the cluster level
/// of a buffer.</summary>
/// <param name="font">The font.</param>
/// <returns>The generator of the code points, the flags and the level.</returns>
let private genLine (font: ReuseFont) =
    gen {
        let! length = Gen.frequency [ 3, Gen.choose (1, 3); 10, Gen.choose (4, 12); 4, Gen.choose (13, 40); 1, Gen.choose (80, 160) ]
        let! text = Gen.arrayOfLength length (genCodepoint font)
        let! flags =
            Gen.frequency
                [ 10, Gen.constant BufferFlags.Default
                  3, Gen.constant (BufferFlags.Bot ||| BufferFlags.Eot)
                  2, Gen.constant BufferFlags.DoNotInsertDottedCircle
                  2, Gen.constant BufferFlags.ProduceUnsafeToConcat
                  1, Gen.constant BufferFlags.PreserveDefaultIgnorables
                  1, Gen.constant BufferFlags.RemoveDefaultIgnorables ]
        let! level =
            Gen.frequency
                [ 10, Gen.constant ClusterLevel.MonotoneGraphemes
                  3, Gen.constant ClusterLevel.MonotoneCharacters
                  2, Gen.constant ClusterLevel.Characters
                  1, Gen.constant ClusterLevel.Graphemes ]
        return struct (text, flags, level)
    }

/// <summary>Generates a hostile line: long, mostly marks, viramas and joiners between few consonants, so that broken syllables, dotted
/// circles, syllables sharing clusters and more than fifteen syllables a line, whose serials wrap, are common.</summary>
/// <param name="font">The font.</param>
/// <returns>The generator of the code points, the flags and the level.</returns>
let private genHostile (font: ReuseFont) =
    let marks = font.Pools |> List.skip 1 |> List.collect snd
    let consonants = snd font.Pools.Head
    let pick ranges = Gen.oneof (ranges |> List.map (fun (first, last) -> Gen.choose (first, last)))
    let genCodepoint = Gen.frequency [ 2, pick consonants; 6, pick marks; 2, Gen.elements [ 0x200C; 0x200D ] ] |> Gen.map uint32
    gen {
        let! length = Gen.choose (40, 200)
        let! text = Gen.arrayOfLength length genCodepoint
        let! flags = Gen.elements [ BufferFlags.Default; BufferFlags.Default; BufferFlags.DoNotInsertDottedCircle; BufferFlags.Bot ||| BufferFlags.Eot ]
        return struct (text, flags, ClusterLevel.MonotoneGraphemes)
    }

/// <summary>A run of code points in a direction and script, with flags and a cluster level.</summary>
/// <param name="text">The code points.</param>
/// <param name="direction">The direction.</param>
/// <param name="script">The script.</param>
/// <param name="flags">The flags.</param>
/// <param name="level">The cluster level.</param>
/// <returns>The run.</returns>
let private runOf (text: uint32 array) direction script flags level =
    GlyphRun.addCodepoints (ReadOnlySpan text) 0 text.Length GlyphRun.empty
    |> GlyphRun.withDirection direction
    |> GlyphRun.withScript script
    |> GlyphRun.withFlags flags
    |> GlyphRun.withClusterLevel level

/// <summary>The plans of a font for a direction and script: the plan as made, which reuses syllables, and the same plan without the reuse.</summary>
/// <param name="font">The font.</param>
/// <param name="direction">The direction.</param>
/// <param name="script">The script.</param>
/// <returns>The two plans.</returns>
let private plansOf (font: Font) direction script =
    let props = GlyphRun.segmentProperties (runOf [||] direction script BufferFlags.Default ClusterLevel.MonotoneGraphemes)
    match ShapePlan.create font props ImmutableArray.Empty ValueNone with
    | Ok plan -> struct (plan, Execute.withoutSyllableReuse plan)
    | Error error -> failwith $"no plan: {error}"

/// <summary>The glyphs and positions of a shaped run as text, every field of each.</summary>
/// <param name="shaped">The shaping result.</param>
/// <returns>The text.</returns>
let private described (shaped: Result<GlyphRun, ShapeError>) =
    match shaped with
    | Error error -> $"error {error}"
    | Ok run ->
        let infos = run.Infos |> Seq.map (fun info -> $"{info.Codepoint}:{info.Mask}:{info.Cluster}") |> String.concat " "
        let positions = run.Positions |> Seq.map (fun p -> $"{p.XAdvance},{p.YAdvance},{p.XOffset},{p.YOffset}") |> String.concat " "
        $"{infos} | {positions}"

/// <summary>Shapes one line three ways, with the reuse, without it and through the reference pipeline, and expects one result.</summary>
/// <param name="font">The font.</param>
/// <param name="plans">The plan with the reuse and the plan without.</param>
/// <param name="name">The font's name for the message.</param>
/// <param name="direction">The direction.</param>
/// <param name="script">The script.</param>
/// <param name="line">The code points, the flags and the level.</param>
let private check (font: Font) (plans: struct (ShapePlan * ShapePlan)) name direction script (line: struct (uint32 array * BufferFlags * ClusterLevel)) =
    let struct (reusing, plain) = plans
    let struct (text, flags, level) = line
    let run = runOf text direction script flags level
    let reused = described (Execute.run reusing font ImmutableArray.Empty run)
    let without = described (Execute.run plain font ImmutableArray.Empty run)
    let reference = described (Execute.runGeneral plain font ImmutableArray.Empty run)
    let shown = text |> Array.map (fun c -> $"U+{c:X4}") |> String.concat " "
    Expect.equal reused without $"{name}, {flags}, {level}: the reuse changed the glyphs of {shown}"
    Expect.equal without reference $"{name}, {flags}, {level}: the windowed route differs from the reference on {shown}"

/// <summary>The tests: reusing syllables gives the glyphs, clusters, flags and positions of shaping every line through all its lookups.</summary>
[<Tests>]
let tests =
    testList "syllable reuse" [
        test "the serial rule of the replay is the serial rule of the kernel for every serial" {
            for serial in 0 .. 255 do
                let struct (expectedId, expectedNext) = Matching.allocateLigId (byte serial)
                let struct (actualId, actualNext) = SyllableReuseKernel.allocateLigId (byte serial)
                Expect.equal struct (actualId, actualNext) struct (expectedId, expectedNext) $"serial {serial}"
        }
        test "an Indic plan reuses its syllables and a plan of another shaper does not" {
            let devanagari = Font.create (Face.create (UpstreamFiles.read "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf") 0u)
            let struct (indic, _) = plansOf devanagari Ltr Scripts.Devanagari
            let struct (latin, _) = plansOf devanagari Ltr Scripts.Latin
            Expect.isTrue (ValueOption.isSome indic.Syllables) "the Devanagari plan has a reuse program"
            Expect.isTrue (ValueOption.isNone latin.Syllables) "the Latin plan has none"
            match indic.Syllables with
            | ValueSome program -> Expect.isTrue (ValueOption.isSome program.Tail) "the calt lookup after the syllable lookups runs as a tail window"
            | ValueNone -> ()
        }
        for reuseFont in fonts do
            test $"{reuseFont.Name}: random lines, flags and cluster levels shape the same with and without the reuse, sharing one plan" {
                let font = Font.create (Face.create (UpstreamFiles.read reuseFont.Path) 0u)
                let plans = plansOf font Ltr reuseFont.Script
                let lines = Gen.sampleWithSeed (Rnd (Seed + uint64 (Seq.sumBy int reuseFont.Name))) 30 caseCount (genLine reuseFont)
                lines |> Seq.iter (check font plans reuseFont.Name Ltr reuseFont.Script)
                lines |> Seq.truncate 500 |> Seq.iter (check font plans reuseFont.Name Ltr reuseFont.Script)
                let struct (reusing, _) = plans
                match reusing.Syllables with
                | ValueSome program -> Expect.isNotNull program.Table.Slots "the plan learned syllables"
                | ValueNone -> failtest "the plan has no reuse program"
            }
        for reuseFont in fonts do
            test $"{reuseFont.Name}: hostile lines of marks, joiners and broken syllables shape the same with and without the reuse" {
                let font = Font.create (Face.create (UpstreamFiles.read reuseFont.Path) 0u)
                let plans = plansOf font Ltr reuseFont.Script
                let lines = Gen.sampleWithSeed (Rnd (Seed + 11UL + uint64 (Seq.sumBy int reuseFont.Name))) 30 (caseCount / 6) (genHostile reuseFont)
                lines |> Seq.iter (check font plans reuseFont.Name Ltr reuseFont.Script)
                lines |> Seq.iter (check font plans reuseFont.Name Ltr reuseFont.Script)
            }
        test "a Devanagari line run right to left, which the reuse refuses, shapes as it does without it" {
            let reuseFont = fonts.Head
            let font = Font.create (Face.create (UpstreamFiles.read reuseFont.Path) 0u)
            let plans = plansOf font Rtl reuseFont.Script
            Gen.sampleWithSeed (Rnd (Seed + 7UL)) 30 500 (genLine reuseFont) |> Seq.iter (check font plans reuseFont.Name Rtl reuseFont.Script)
        }
        test "the corpus lines of Hindi words shape the same with and without the reuse, twice over one plan" {
            let font = Font.create (Face.create (UpstreamFiles.read "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf") 0u)
            let plans = plansOf font Ltr Scripts.Devanagari
            let text = UpstreamFiles.read "perf/texts/hi-words.txt"
            let lines =
                Text.Encoding.UTF8.GetString(text.Span).Split('\n', StringSplitOptions.RemoveEmptyEntries)
                |> Array.truncate 3000
                |> Array.map (fun line -> struct (line.EnumerateRunes() |> Seq.map (fun rune -> uint32 rune.Value) |> Seq.toArray, BufferFlags.Default, ClusterLevel.MonotoneGraphemes))
            lines |> Array.iter (check font plans "Hindi words" Ltr Scripts.Devanagari)
            lines |> Array.iter (check font plans "Hindi words again" Ltr Scripts.Devanagari)
        }
    ]
