module Farfbuzz.Diff.ComplexShapers.ShapeDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Harness.TailCalls
open Farfbuzz.Diff
open Farfbuzz.Diff.HarfBuzzSharpShaper
open Farfbuzz.Diff.TestFonts

/// <summary>Builds a shaping request from a font path, an options field and a text, exactly as a synthetic
/// <c>font;options;U+…;*</c> test line parses into one.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="options">The option field, split on white space.</param>
/// <param name="codepoints">The text.</param>
/// <returns>The request.</returns>
let request (fontFile: string) (options: string) (codepoints: uint32 array) : ShapeRequest =
    let unicodes = codepoints |> Array.map (fun codepoint -> $"U+{codepoint:X4}") |> String.concat ","
    let line = $"{fontFile};{options};{unicodes};*"
    match TestFile.parseLine 1 line with
    | TestFile.Case (_, fields) ->
        match ShapeRequest.build fontFile fields.Unicodes fields.Options [] with
        | Ok request -> request
        | Error reason -> failtest $"synthetic line could not be built: {reason}"
    | TestFile.CommentedCase _ | TestFile.Directive _ | TestFile.Ignored _ | TestFile.Malformed _ ->
        failtest "synthetic line did not parse as a case"

/// <summary>The verification flags the helper adds, as <c>test/shape/run-tests.py</c> does for an expected-output line.</summary>
let helperFlags : BufferFlags =
    BufferFlags.ProduceUnsafeToConcat ||| BufferFlags.ProduceSafeToInsertTatweel

/// <summary>A request with the helper's buffer flags and the glyph-flags output.</summary>
/// <param name="request">The request.</param>
/// <returns>The request with the flags set.</returns>
let withFlags (request: ShapeRequest) : ShapeRequest =
    { request with
        Flags = request.Flags ||| helperFlags
        Output = request.Output ||| SerializeFlags.GlyphFlags }

/// <summary>The glyph serialisation of an engine's outcome, with glyph flags.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The text, or why there is none.</returns>
let serialize (outcome: Result<ShapeOutcome, string>) : Result<string, string> =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Serialize.glyphs SerializeFlags.GlyphFlags run.GlyphName run.Extents (ReadOnlySpan run.Infos) (ReadOnlySpan run.Positions) |> settle)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>A generator of requests over the given alphabet, lengths one to <paramref name="maxLength"/>.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="options">The option field.</param>
/// <param name="alphabet">The code points to draw from.</param>
/// <param name="maxLength">The longest text.</param>
/// <returns>The request generator.</returns>
let cases (fontFile: string) (options: string) (alphabet: ImmutableArray<uint32>) (maxLength: int) : Gen<ShapeRequest> =
    gen {
        let! length = Gen.choose (1, maxLength)
        let! codepoints = Gen.arrayOfLength length (Gen.elements (alphabet |> Seq.toArray))
        return withFlags (request fontFile options codepoints)
    }

/// <summary>
/// Compares two engines on random text over an alphabet: the request is built from a synthetic test line, shaped by
/// each engine and compared as the glyph serialisation with glyph flags.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="options">The option field.</param>
/// <param name="alphabet">The code points to draw from.</param>
/// <param name="maxLength">The longest text.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let sampled (name: string) (fontFile: string) (options: string) (alphabet: ImmutableArray<uint32>) (maxLength: int) (count: int) : Test =
    let ours = create ()
    let oracle = create ()
    Differential.sampled name (cases fontFile options alphabet maxLength) count
        (fun spec -> serialize (ours.Shape spec))
        (fun spec -> serialize (oracle.Shape spec))

/// <summary>The code points the shape self-check draws from: ASCII letters, digits and the space.</summary>
let private asciiAlphabet : ImmutableArray<uint32> =
    ImmutableArray.CreateRange ([ 0x41u .. 0x5Au ] @ [ 0x61u .. 0x7Au ] @ [ 0x30u .. 0x39u ] @ [ 0x20u ])

/// <summary>The P6.S02 shape self-check: the oracle against itself on random ASCII text of Roboto.</summary>
/// <returns>The test.</returns>
let tests () : Test =
    sampled "shape self" (TestFonts.named "Roboto-Regular.ttf").Path String.Empty asciiAlphabet 12 1000

/// <summary>A generator of requests whose option field is drawn from a list for each case, over the given alphabet,
/// lengths one to <paramref name="maxLength"/>.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="optionChoices">The option fields to draw from.</param>
/// <param name="alphabet">The code points to draw from.</param>
/// <param name="maxLength">The longest text.</param>
/// <returns>The request generator.</returns>
let casesWith (fontFile: string) (optionChoices: string array) (alphabet: ImmutableArray<uint32>) (maxLength: int) : Gen<ShapeRequest> =
    gen {
        let! options = Gen.elements optionChoices
        let! length = Gen.choose (1, maxLength)
        let! codepoints = Gen.arrayOfLength length (Gen.elements (alphabet |> Seq.toArray))
        return withFlags (request fontFile options codepoints)
    }

/// <summary>
/// Compares Farfbuzz with HarfBuzz on random text over an alphabet, the option field drawn per case: each request is
/// built from a synthetic test line, shaped by both engines and compared as the glyph serialisation with glyph flags
/// (glyphs, clusters, advances, offsets and flags).
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="optionChoices">The option fields to draw from.</param>
/// <param name="alphabet">The code points to draw from.</param>
/// <param name="maxLength">The longest text.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let against (name: string) (fontFile: string) (optionChoices: string array) (alphabet: ImmutableArray<uint32>) (maxLength: int) (count: int) : Test =
    let ours = FarfbuzzShaper.shaper
    let oracle = create ()
    Differential.sampled name (casesWith fontFile optionChoices alphabet maxLength) count
        (fun spec -> serialize (ours.Shape spec))
        (fun spec -> serialize (oracle.Shape spec))

/// <summary>A generator of requests whose option field is drawn from a list for each case and whose text is the given
/// generator's, for texts built from tokens rather than drawn code point by code point.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="optionChoices">The option fields to draw from.</param>
/// <param name="texts">The generator of texts.</param>
/// <returns>The request generator.</returns>
let casesOfTexts (fontFile: string) (optionChoices: string array) (texts: Gen<uint32 array>) : Gen<ShapeRequest> =
    gen {
        let! options = Gen.elements optionChoices
        let! codepoints = texts
        return withFlags (request fontFile options codepoints)
    }

/// <summary>
/// Compares Farfbuzz with HarfBuzz on random texts from a generator, the option field drawn per case: each request is
/// built from a synthetic test line, shaped by both engines and compared as the glyph serialisation with glyph flags.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="fontFile">The font path.</param>
/// <param name="optionChoices">The option fields to draw from.</param>
/// <param name="texts">The generator of texts.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let againstTexts (name: string) (fontFile: string) (optionChoices: string array) (texts: Gen<uint32 array>) (count: int) : Test =
    let ours = FarfbuzzShaper.shaper
    let oracle = create ()
    Differential.sampled name (casesOfTexts fontFile optionChoices texts) count
        (fun spec -> serialize (ours.Shape spec))
        (fun spec -> serialize (oracle.Shape spec))

/// <summary>
/// Compares Farfbuzz with HarfBuzz on the requests of a generator, each shaped by both engines and compared as the glyph
/// serialisation with glyph flags (glyphs, clusters, advances, offsets and flags), for a generator that sets more of the request
/// than the text, such as an item context.
/// </summary>
/// <param name="name">The test name.</param>
/// <param name="cases">The generator of requests.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let againstCases (name: string) (cases: Gen<ShapeRequest>) (count: int) : Test =
    let ours = FarfbuzzShaper.shaper
    let oracle = create ()
    Differential.sampled name cases count
        (fun spec -> serialize (ours.Shape spec))
        (fun spec -> serialize (oracle.Shape spec))
