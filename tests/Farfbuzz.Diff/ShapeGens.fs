module Farfbuzz.Diff.ShapeGens

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Diff.TestFonts

/// <summary>The kind of text a shaping differential draws; later slices add ignorables and vertical kinds.</summary>
type TextKind =
    | Basic
    | WithMarks
    | Rtl // RTL scripts in their native direction: ensureNativeDirection never reverses anything in it
    | WithIgnorables
    | Vertical

/// <summary>Whether a general category is a mark (spacing, enclosing or non-spacing).</summary>
/// <param name="category">The category.</param>
/// <returns>True for marks.</returns>
let private isMark (category: GeneralCategory) =
    match category with
    | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark | GeneralCategory.NonSpacingMark -> true
    | _ -> false

/// <summary>Whether a code point canonically decomposes into a base and a combining mark; the mark then needs the
/// S06 fallback positioning, so such a code point is not basic yet.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True when its decomposition has two parts.</returns>
let private decomposesWithMark (codepoint: uint32) =
    match Unicode.decompose codepoint with
    | ValueSome (struct (_, second)) -> second <> 0u
    | ValueNone -> false

/// <summary>Whether a code point is eligible for the basic text: a default shaper, no mark, no ignorable, only the
/// ordinary space among the space separators, none of the continuation triggers (emoji modifiers, regional
/// indicators and halfwidth marks) that <c>form_clusters</c> handles in S05, and no decomposition into a mark.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True when it is basic.</returns>
let private isBasic (codepoint: uint32) =
    let category = Unicode.generalCategory codepoint
    Categorize.shaper (Unicode.script codepoint) Direction.Ltr Tags.Empty = DefaultShaper
    && not (isMark category)
    && not (Unicode.isDefaultIgnorable codepoint)
    && (category <> GeneralCategory.SpaceSeparator || codepoint = 0x20u)
    && not (codepoint >= 0x1F3FBu && codepoint <= 0x1F3FFu)
    && not (codepoint >= 0x1F1E6u && codepoint <= 0x1F1FFu)
    && not (codepoint >= 0xFF9Eu && codepoint <= 0xFF9Fu)
    && not (decomposesWithMark codepoint)

/// <summary>The code points the basic text draws from, built once.</summary>
let private basicCodepoints : uint32 array =
    [| for codepoint in 0x20u .. 0x2FFFFu do
        if isBasic codepoint then codepoint |]

/// <summary>Whether a code point is a mark or a precomposed letter of a default-shaper script, the additions the
/// with-marks text makes over the basic set.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True when it is a mark or a decomposable letter of a default shaper.</returns>
let private isMarkOrDecomposing (codepoint: uint32) =
    let category = Unicode.generalCategory codepoint
    Categorize.shaper (Unicode.script codepoint) Direction.Ltr Tags.Empty = DefaultShaper
    && not (Unicode.isDefaultIgnorable codepoint)
    && (isMark category || decomposesWithMark codepoint)

/// <summary>The code points the with-marks text draws from: basic code points plus marks and precomposed letters.</summary>
let private withMarksCodepoints : uint32 array =
    [| for codepoint in 0x20u .. 0x2FFFFu do
        if isBasic codepoint || isMarkOrDecomposing codepoint then codepoint |]

/// <summary>Whether a code point is a base letter of an RTL script the default shaper handles.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for RTL-script letters the default shaper shapes.</returns>
let private isRtlBase (codepoint: uint32) =
    let category = Unicode.generalCategory codepoint
    let script = Unicode.script codepoint
    Categorize.shaper script Direction.Rtl Tags.Empty = DefaultShaper
    && Script.horizontalDirection script = Direction.Rtl
    && not (isMark category)
    && not (Unicode.isDefaultIgnorable codepoint)
    && category <> GeneralCategory.SpaceSeparator

/// <summary>Whether a code point is a mark or precomposed letter of a default-shaper script, under RTL direction.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for RTL-script marks and decomposable letters.</returns>
let private isRtlMarkOrDecomposing (codepoint: uint32) =
    let category = Unicode.generalCategory codepoint
    Categorize.shaper (Unicode.script codepoint) Direction.Rtl Tags.Empty = DefaultShaper
    && not (Unicode.isDefaultIgnorable codepoint)
    && (isMark category || decomposesWithMark codepoint)

/// <summary>The RTL base letters the RTL text draws its first code point from.</summary>
let private rtlBaseCodepoints : uint32 array =
    [| for codepoint in 0x20u .. 0x2FFFFu do
        if isRtlBase codepoint then codepoint |]

/// <summary>The RTL base letters, marks and precomposed letters the RTL text draws from.</summary>
let private rtlCodepoints : uint32 array =
    [| for codepoint in 0x20u .. 0x2FFFFu do
        if isRtlBase codepoint || isRtlMarkOrDecomposing codepoint then codepoint |]

/// <summary>Whether a code point is a default ignorable or a space separator of a default shaper, the additions
/// the with-ignorables text makes over the basic set.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True when it is a default ignorable or a Zs of a default shaper.</returns>
let private isIgnorableOrSpace (codepoint: uint32) =
    let category = Unicode.generalCategory codepoint
    Categorize.shaper (Unicode.script codepoint) Direction.Ltr Tags.Empty = DefaultShaper
    && not (isMark category)
    && (Unicode.isDefaultIgnorable codepoint || category = GeneralCategory.SpaceSeparator)

/// <summary>The code points the with-ignorables text draws from: basic code points plus default ignorables and spaces.</summary>
let private withIgnorablesCodepoints : uint32 array =
    [| for codepoint in 0x20u .. 0x2FFFFu do
        if isBasic codepoint || isIgnorableOrSpace codepoint then codepoint |]

/// <summary>Generates a string of code points of the given kind, one to sixteen of them.</summary>
/// <param name="kind">The text kind.</param>
/// <returns>The code points.</returns>
let text (kind: TextKind) : Gen<uint32 array> =
    match kind with
    | Basic ->
        gen {
            let! length = Gen.choose (1, 16)
            return! Gen.arrayOfLength length (Gen.elements basicCodepoints)
        }
    | WithMarks ->
        gen {
            let! length = Gen.choose (1, 16)
            return! Gen.arrayOfLength length (Gen.elements withMarksCodepoints)
        }
    | Rtl ->
        gen {
            let! length = Gen.choose (1, 16)
            let! first = Gen.elements rtlBaseCodepoints
            let! rest = Gen.arrayOfLength (length - 1) (Gen.elements rtlCodepoints)
            return Array.append [| first |] rest
        }
    | WithIgnorables ->
        gen {
            let! length = Gen.choose (1, 16)
            return! Gen.arrayOfLength length (Gen.elements withIgnorablesCodepoints)
        }
    | Vertical ->
        gen {
            let! length = Gen.choose (1, 16)
            return! Gen.arrayOfLength length (Gen.elements basicCodepoints)
        }

/// <summary>Generates a shaping request over a stripped font and a string of the given kind.</summary>
/// <param name="fonts">The stripped faces.</param>
/// <param name="kind">The text kind.</param>
/// <returns>The request.</returns>
let request (fonts: ImmutableArray<TestFont>) (kind: TextKind) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements (fonts |> Seq.toArray)
        let! codepoints = text kind
        return { defaults font.Path codepoints with FaceIndex = font.Index; Shaper = ValueSome "ot"; FontFuncs = ValueSome "ot" }
    }
