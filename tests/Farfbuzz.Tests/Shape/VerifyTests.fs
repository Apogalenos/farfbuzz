module Farfbuzz.Tests.Shape.VerifyTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format-12 cmap font mapping each code point to a glyph.</summary>
/// <param name="pairs">The code point and glyph pairs.</param>
/// <returns>The font.</returns>
let private font12 (pairs: (uint32 * uint32) list) : Font =
    let groups = pairs |> List.map (fun (codepoint, glyph) -> codepoint, codepoint, glyph)
    let body = groups |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ]) |> Array.concat
    let subtable = Array.concat [ be16 12u; be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]
    let cmap = Array.concat [ be16 0u; be16 1u; be16 3u; be16 1u; be32 12u; subtable ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmap ])) 0u |> Font.create

/// <summary>Builds a text run from code points, with the given direction and buffer flags.</summary>
/// <param name="direction">The direction.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The text run.</returns>
let private textRun (direction: Direction) (flags: BufferFlags) (codepoints: uint32 array) : GlyphRun =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
    |> GlyphRun.withDirection direction
    |> GlyphRun.withFlags flags
    |> GlyphRun.guessSegmentProperties Language.cLocale
    |> TailCalls.settle

/// <summary>Builds one glyph info.</summary>
/// <param name="codepoint">The glyph id.</param>
/// <param name="cluster">The cluster.</param>
/// <param name="mask">The glyph-flag mask.</param>
/// <returns>The info.</returns>
let private mkInfo (codepoint: uint32) (cluster: uint32) (mask: uint32) : GlyphInfo =
    { Codepoint = codepoint; Mask = mask; Cluster = cluster; Var1 = 0u; Var2 = 0u }

/// <summary>Builds a shaped run with glyph content and zeroed positions.</summary>
/// <param name="direction">The direction.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="infos">The glyph infos.</param>
/// <returns>The run.</returns>
let private glyphRun (direction: Direction) (flags: BufferFlags) (infos: GlyphInfo array) : GlyphRun =
    { GlyphRun.empty with
        Infos = Sealed.ofSpan (ReadOnlySpan infos)
        Props = Sealed.init infos.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
        Positions = Sealed.init infos.Length (fun _ -> { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
        HavePositions = true
        ContentType = GlyphContent
        SegmentProperties = { Direction = direction; Script = Scripts.Invalid; Language = Language.NoLanguage }
        Flags = flags }

/// <summary>Shapes a run with the ot shaper and no verification.</summary>
/// <param name="font">The font.</param>
/// <param name="run">The text run.</param>
/// <returns>The shaped run.</returns>
let private shape (font: Font) (run: GlyphRun) : GlyphRun =
    match Shape.shapeFull font ImmutableArray.Empty ValueNone run with
    | Error error -> failtest $"expected Ok, got %A{error}"
    | Ok shaped -> shaped

/// <summary>The code points of the random texts: Latin letters, digits, spaces, punctuation, the letters of ligatures,
/// Greek, Cyrillic, combining marks, joiners and default ignorables. No Arabic: that shaper is not ported.</summary>
let private alphabet =
    [| 0x41u; 0x62u; 0x20u; 0x39u; 0x2Eu; 0x66u; 0x69u; 0x6Cu; 0x6Fu; 0xE9u; 0x3B1u; 0x3C9u; 0x416u; 0x44Fu
       0x301u; 0x308u; 0x200Du; 0x200Cu; 0xADu; 0x2060u; 0xFEFFu; 0x200Bu |]

/// <summary>The fonts of the equality property: a small format-12 font and three real fonts, two of them with their
/// layout tables.</summary>
let private verifyFonts =
    lazy
        [| font12 (alphabet |> Array.mapi (fun index codepoint -> codepoint, uint32 index + 1u) |> List.ofArray)
           SimpleRunTests.fonts.Value[0]
           SimpleRunTests.fonts.Value[1]
           SimpleRunTests.fonts.Value[2] |]

/// <summary>Changes one field of one glyph of a shaped run, so that the failing verdicts are reached.</summary>
/// <param name="field">Which field: the code point, the cluster, a glyph flag, one of the four position fields, or none.</param>
/// <param name="at">The glyph index.</param>
/// <param name="shaped">The shaped run.</param>
/// <returns>The corrupted run.</returns>
let private corrupted (field: byte) (at: int) (shaped: GlyphRun) : GlyphRun =
    let infos (change: GlyphInfo -> GlyphInfo) =
        { shaped with Infos = Sealed.mapi (fun index (info: GlyphInfo) -> if index = at then change info else info) (shaped.Infos.AsSpan()) }
    let positions (change: GlyphPosition -> GlyphPosition) =
        { shaped with Positions = Sealed.mapi (fun index (position: GlyphPosition) -> if index = at then change position else position) (shaped.Positions.AsSpan()) }
    match shaped.Infos.Length, field % 9uy with
    | 0, _ -> shaped
    | _, 0uy -> infos (fun info -> { info with Codepoint = info.Codepoint + 1u })
    | _, 1uy -> infos (fun info -> { info with Cluster = info.Cluster + 1u })
    | _, 2uy -> infos (fun info -> { info with Mask = info.Mask ^^^ uint32 GlyphFlags.UnsafeToBreak })
    | _, 3uy -> infos (fun info -> { info with Mask = info.Mask ^^^ uint32 GlyphFlags.UnsafeToConcat })
    | _, 4uy -> positions (fun position -> { position with XAdvance = position.XAdvance + 1 })
    | _, 5uy -> positions (fun position -> { position with YAdvance = position.YAdvance + 1 })
    | _, 6uy -> positions (fun position -> { position with XOffset = position.XOffset + 1 })
    | _, 7uy -> positions (fun position -> { position with YOffset = position.YOffset + 1 })
    | _, _ -> shaped

/// <summary>Whether the streamed verification and the reference return the same result, with the shaped run's plan known
/// and not.</summary>
/// <param name="font">The font.</param>
/// <param name="text">The text run.</param>
/// <param name="candidate">The shaped run, possibly corrupted.</param>
/// <returns>The reference result and the two streamed results.</returns>
let private verdicts (font: Font) (text: GlyphRun) (candidate: GlyphRun) =
    match Execute.createPlan font text.SegmentProperties ImmutableArray.Empty ValueNone with
    | Error error -> failtest $"expected a plan, got %A{error}"
    | Ok plan ->
        let reference = Verify.verify font ImmutableArray.Empty ValueNone text candidate
        let known = Verify.verifyStreaming font ImmutableArray.Empty ValueNone (ValueSome (struct (text.SegmentProperties, plan))) text candidate
        let unknown = Verify.verifyStreaming font ImmutableArray.Empty ValueNone ValueNone text candidate
        struct (reference, known, unknown)

/// <summary>The lines of a corpus text, the first ones.</summary>
/// <param name="path">The text's path under the upstream checkout.</param>
/// <param name="count">How many lines.</param>
/// <returns>The lines as code point arrays.</returns>
let private corpusLines (path: string) (count: int) : uint32 array array =
    let text = Text.Encoding.UTF8.GetString ((UpstreamFiles.read path).ToArray())
    text.Split '\n'
    |> Array.truncate count
    |> Array.map (fun line -> line.EnumerateRunes() |> Seq.map (fun rune -> uint32 rune.Value) |> Array.ofSeq)

/// <summary>The bytes allocated by verifying a plain Latin line of a given length, after one warm-up verification.</summary>
/// <param name="font">The font.</param>
/// <param name="flags">The buffer flags.</param>
/// <param name="glyphs">The number of glyphs.</param>
/// <returns>The bytes allocated by one verification.</returns>
let private verifyBytes (font: Font) (flags: BufferFlags) (glyphs: int) : int64 =
    let words = "the quick brown fox jumps over the lazy dog and then rests "
    let codepoints = Array.init glyphs (fun index -> uint32 words[index % words.Length])
    let text = textRun Direction.Ltr flags codepoints
    let shaped = shape font text
    match Execute.createPlan font text.SegmentProperties ImmutableArray.Empty ValueNone with
    | Error error -> failtest $"expected a plan, got %A{error}"
    | Ok plan ->
        let known = ValueSome (struct (text.SegmentProperties, plan))
        Verify.verifyStreaming font ImmutableArray.Empty ValueNone known text shaped |> ignore
        let before = GC.GetAllocatedBytesForCurrentThread()
        let result = Verify.verifyStreaming font ImmutableArray.Empty ValueNone known text shaped
        let after = GC.GetAllocatedBytesForCurrentThread()
        Expect.isOk result "the line verifies"
        after - before

/// <summary>The P4.S09 verify tests.</summary>
[<Tests>]
let tests =
    testList "P4.S09" [
        test "a non-monotone run fails NotMonotone" {
            let shaped =
                GlyphRun.empty
                |> GlyphRun.add 1u 0u
                |> GlyphRun.add 2u 1u
                |> GlyphRun.add 3u 0u
                |> GlyphRun.withDirection Direction.Ltr
            match Verify.verify (Font.create Face.empty) ImmutableArray.Empty ValueNone GlyphRun.empty shaped with
            | Error (VerifyFailed (NotMonotone, _)) -> ()
            | other -> failtest $"expected NotMonotone, got %A{other}"
        }
        test "a wrong glyph inside the reconstruction fails UnsafeToBreakFailed" {
            let font = font12 [ 0x41u, 1u; 0x42u, 2u ]
            let text = textRun Direction.Ltr BufferFlags.Default [| 0x41u; 0x42u |]
            let shaped = shape font text
            let corrupted =
                { shaped with
                    Infos = Sealed.mapi (fun index (info: GlyphInfo) -> if index = 1 then { info with Codepoint = 1u } else info) (shaped.Infos.AsSpan()) }
            match Verify.verify font ImmutableArray.Empty ValueNone text corrupted with
            | Error (VerifyFailed (UnsafeToBreakFailed, _)) -> ()
            | other -> failtest $"expected UnsafeToBreakFailed, got %A{other}"
        }
        test "a wrong unsafe-to-concat flag inside the run fails UnsafeToConcatFailed" {
            let font = font12 [ 0x41u, 1u; 0x42u, 2u; 0x43u, 3u ]
            let text = textRun Direction.Ltr BufferFlags.ProduceUnsafeToConcat [| 0x41u; 0x42u; 0x43u |]
            let shaped = shape font text
            let corrupted =
                { shaped with
                    Infos = Sealed.mapi (fun index (info: GlyphInfo) ->
                        if index = 1 then { info with Mask = uint32 GlyphFlags.UnsafeToConcat } else info) (shaped.Infos.AsSpan()) }
            match Verify.verify font ImmutableArray.Empty ValueNone text corrupted with
            | Error (VerifyFailed (UnsafeToConcatFailed, _)) -> ()
            | other -> failtest $"expected UnsafeToConcatFailed, got %A{other}"
        }
        test "a fragment that fails to shape, for want of a shaper that is known, passes verification" {
            let text = textRun Direction.Ltr BufferFlags.Default [| 0x0627u |]
            let shaped = glyphRun Direction.Ltr BufferFlags.Default [| mkInfo 0u 0u 0u |]
            let shapers = ValueSome (ImmutableArray.Create "no such shaper")
            match Verify.verify (Font.create Face.empty) ImmutableArray.Empty shapers text shaped with
            | Ok result -> Expect.equal result.Infos.Length 1 "the run is returned unchanged"
            | Error error -> failtest $"expected Ok, got %A{error}"
        }
        test "diff reports each kind of difference" {
            let equal = glyphRun Direction.Ltr BufferFlags.Default [| mkInfo 1u 0u 0u; mkInfo 2u 1u 0u |]
            let codepoint = glyphRun Direction.Ltr BufferFlags.Default [| mkInfo 1u 0u 0u; mkInfo 3u 1u 0u |]
            let cluster = glyphRun Direction.Ltr BufferFlags.Default [| mkInfo 1u 0u 0u; mkInfo 2u 2u 0u |]
            let flags = glyphRun Direction.Ltr BufferFlags.Default [| mkInfo 1u 0u 0u; mkInfo 2u 1u (uint32 GlyphFlags.UnsafeToBreak) |]
            let longer = glyphRun Direction.Ltr BufferFlags.Default [| mkInfo 1u 0u 0u |]
            let positionMismatch =
                { equal with
                    Positions = Sealed.init 2 (fun index -> if index = 0 then { XAdvance = 5; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u } else { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }) }
            let unicode = { equal with ContentType = UnicodeContent }
            Expect.equal (Verify.diff equal equal ValueNone 0u) DiffFlags.Equal "equal"
            Expect.equal (Verify.diff codepoint equal ValueNone 0u) DiffFlags.CodepointMismatch "codepoint"
            Expect.equal (Verify.diff cluster equal ValueNone 0u) DiffFlags.ClusterMismatch "cluster"
            Expect.equal (Verify.diff flags equal ValueNone 0u) DiffFlags.GlyphFlagsMismatch "glyph flags"
            Expect.equal (Verify.diff equal longer ValueNone 0u) DiffFlags.LengthMismatch "length"
            Expect.equal (Verify.diff positionMismatch equal ValueNone 0u) DiffFlags.PositionMismatch "position"
            Expect.equal (Verify.diff unicode equal ValueNone 0u) DiffFlags.ContentTypeMismatch "content type"
            Expect.equal (Verify.diff equal equal (ValueSome 99u) 0u) DiffFlags.Equal "dotted-circle not present"
            Expect.equal (Verify.diff equal equal (ValueSome 2u) 0u) DiffFlags.DottedCirclePresent "dotted-circle present"
            let notdef = glyphRun Direction.Ltr BufferFlags.Default [| mkInfo 0u 0u 0u |]
            Expect.equal (Verify.diff notdef notdef (ValueSome 2u) 0u) DiffFlags.NotdefPresent "notdef present"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "verifying with the shaped run's plan equals building a plan per fragment" <| fun (codes: uint16 array) (rtl: bool) (concat: bool) (corrupt: uint16) ->
            let alphabet = [| 0x41u; 0x42u; 0x20u; 0x301u; 0x3B1u; 0x416u; 0x308u; 0x200Du; 0x39u |]
            let font = font12 (alphabet |> Array.mapi (fun index codepoint -> codepoint, uint32 index + 1u) |> List.ofArray)
            let codepoints = codes |> Array.map (fun code -> alphabet[int code % alphabet.Length])
            let direction = if rtl then Direction.Rtl else Direction.Ltr
            let flags = if concat then BufferFlags.ProduceUnsafeToConcat else BufferFlags.Default
            let text = textRun direction flags codepoints
            let shaped = shape font text
            let candidate =
                match corrupt % 3us, shaped.Infos.Length with
                | 0us, _
                | _, 0 -> shaped
                | _ ->
                    let at = int corrupt % shaped.Infos.Length
                    { shaped with
                        Infos = Sealed.mapi (fun index (info: GlyphInfo) -> if index = at then { info with Codepoint = info.Codepoint + 1u } else info) (shaped.Infos.AsSpan()) }
            match Execute.createPlan font text.SegmentProperties ImmutableArray.Empty ValueNone with
            | Error error -> failtest $"expected a plan, got %A{error}"
            | Ok plan ->
                let reference = Verify.verify font ImmutableArray.Empty ValueNone text candidate
                let knowing = Verify.verifyKnowing font ImmutableArray.Empty ValueNone (ValueSome (struct (text.SegmentProperties, plan))) text candidate
                Expect.equal knowing reference "the same verdict and run"
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 6000 } "streamed verification returns the reference's result on random texts" <| fun (codes: uint16 array) (pick: byte) (rtl: bool) (concat: bool) (corrupt: uint16) (field: byte) ->
            let font = verifyFonts.Value[int pick % verifyFonts.Value.Length]
            let codepoints = codes |> Array.truncate 60 |> Array.map (fun code -> alphabet[int code % alphabet.Length])
            let direction = if rtl then Direction.Rtl else Direction.Ltr
            let flags = if concat then BufferFlags.ProduceUnsafeToConcat else BufferFlags.Default
            let text = textRun direction flags codepoints
            let shaped = shape font text
            let candidate = corrupted field (int corrupt % max 1 shaped.Infos.Length) shaped
            let struct (reference, known, unknown) = verdicts font text candidate
            Expect.equal known reference "the same result with the plan known"
            Expect.equal unknown reference "the same result with the plan unknown"
        test "streamed verification returns the reference's result on lines of both Roboto texts" {
            for fontIndex in 1 .. 2 do
                let font = verifyFonts.Value[fontIndex]
                for path in [ "perf/texts/en-thelittleprince.txt"; "perf/texts/en-words.txt" ] do
                    for codepoints in corpusLines path 300 do
                        for flags in [ BufferFlags.Default; BufferFlags.ProduceUnsafeToConcat ] do
                            let text = textRun Direction.Ltr flags codepoints
                            let shaped = shape font text
                            let struct (reference, known, _) = verdicts font text shaped
                            Expect.equal known reference "the same result"
                            Expect.isOk reference "the shaped line verifies"
        }
        test "verifying a plain Latin line allocates a bounded number of bytes per glyph" {
            let stripped = verifyFonts.Value[2]
            let layout = verifyFonts.Value[1]
            for font, flags, perGlyph, fixedBytes in
                [ stripped, BufferFlags.Default, 0L, 1024L
                  stripped, BufferFlags.ProduceUnsafeToConcat, 256L, 4096L
                  layout, BufferFlags.Default, 1536L, 4096L
                  layout, BufferFlags.ProduceUnsafeToConcat, 3072L, 8192L ] do
                let at50 = verifyBytes font flags 50
                let at200 = verifyBytes font flags 200
                printfn $"verify allocation, {flags}: {at50} bytes for 50 glyphs, {at200} bytes for 200 glyphs"
                Expect.isLessThanOrEqual at50 (perGlyph * 50L + fixedBytes) "50 glyphs stay within the budget"
                Expect.isLessThanOrEqual at200 (perGlyph * 200L + fixedBytes) "200 glyphs stay within the budget"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 3000 } "appending ranges in one copy equals appending them one by one" <| fun (codes: uint16 array) (ends: (int8 * int8) array) (seeded: bool) (mixed: bool) ->
            let codepoints = codes |> Array.truncate 40 |> Array.map (fun code -> alphabet[int code % alphabet.Length])
            let source = textRun Direction.Ltr BufferFlags.Default codepoints
            let source = if mixed then shape (verifyFonts.Value[0]) source else source
            let ranges = ends |> Array.truncate 12 |> Array.map (fun (first, last) -> struct (int first, int last))
            let target = if seeded then textRun Direction.Rtl BufferFlags.Default [| 0x41u; 0x42u |] else GlyphRun.empty
            let folded = ranges |> Array.fold (fun run (struct (start, finish)) -> GlyphRun.append source start finish run) target
            Expect.equal (GlyphRun.appendRanges source ranges target) folded "the same run"
        test "every kind of corruption reaches the failing verdict the reference reports" {
            let font = verifyFonts.Value[0]
            let text = textRun Direction.Ltr BufferFlags.ProduceUnsafeToConcat [| 0x41u; 0x62u; 0x20u; 0x66u; 0x69u |]
            let shaped = shape font text
            for field in 0uy .. 8uy do
                let struct (reference, known, unknown) = verdicts font text (corrupted field 2 shaped)
                Expect.equal known reference $"field {field}: the same result with the plan known"
                Expect.equal unknown reference $"field {field}: the same result with the plan unknown"
                let failed = match reference with | Error (VerifyFailed _) -> true | Error _ | Ok _ -> false
                Expect.equal failed (field <> 2uy && field <> 8uy) $"field {field}: whether the run fails verification"
        }
    ]
