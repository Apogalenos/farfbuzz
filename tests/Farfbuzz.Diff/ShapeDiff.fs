module Farfbuzz.Diff.ShapeDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.NoLayoutFonts
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.ShapeGens
open Farfbuzz.Diff.TestFonts

/// <summary>What the comparison looks at: glyph ids, clusters and positions, as the owner's plan asks.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparable (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>What the S05 comparison looks at: glyph ids, clusters and the defined glyph flags.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparableFlags (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (run.Infos |> Array.map (fun (info: GlyphInfo) -> struct (info.Codepoint, info.Cluster, info.Mask &&& uint32 GlyphFlags.Defined)))
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>
/// A with-marks request whose direction is forced to LTR or RTL and whose cluster level ranges over every level.
/// Forcing RTL exercises the grapheme reversal the native-direction Rtl kind never reaches.
/// </summary>
/// <param name="fonts">The stripped faces.</param>
/// <returns>The request.</returns>
let private forcedDirection (fonts: ImmutableArray<Farfbuzz.Diff.TestFonts.TestFont>) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements (fonts |> Seq.toArray)
        let! codepoints = text WithMarks
        let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
        let! clusterLevel = Gen.elements [| 0; 1; 2; 3 |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = direction
                ClusterLevel = clusterLevel }
    }

/// <summary>
/// A full-horizontal request over stripped fonts whose direction is forced to LTR or RTL and whose text draws the
/// basic set plus default ignorables and space separators, the P4.S06 surface (positions included).
/// </summary>
/// <param name="fonts">The stripped faces.</param>
/// <returns>The request.</returns>
let private fullHorizontal (fonts: ImmutableArray<Farfbuzz.Diff.TestFonts.TestFont>) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements (fonts |> Seq.toArray)
        let! codepoints = text WithIgnorables
        let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = direction }
    }

/// <summary>The code points the backward-runs differential draws from: default-shaper letters of both directions
/// that do not decompose into a mark, default ignorables, variation selectors and space separators.</summary>
let private backwardCodepoints : uint32 array =
    [| for codepoint in 0x20u .. 0x2FFFFu do
        let category = Unicode.generalCategory codepoint
        let script = Unicode.script codepoint
        let defaultShaper =
            Categorize.shaper script Direction.Ltr Tags.Empty = DefaultShaper
            && Categorize.shaper script Direction.Rtl Tags.Empty = DefaultShaper
        let decomposesWithMark =
            match Unicode.decompose codepoint with
            | ValueSome (struct (_, second)) -> second <> 0u
            | ValueNone -> false
        let letter =
            defaultShaper
            && not decomposesWithMark
            && (match category with
                | GeneralCategory.UppercaseLetter | GeneralCategory.LowercaseLetter | GeneralCategory.TitlecaseLetter
                | GeneralCategory.ModifierLetter | GeneralCategory.OtherLetter -> true
                | _ -> false)
        let ignorable = defaultShaper && Unicode.isDefaultIgnorable codepoint
        let selector = defaultShaper && Unicode.isVariationSelector codepoint
        let space = defaultShaper && category = GeneralCategory.SpaceSeparator
        if letter || ignorable || selector || space then codepoint |]

/// <summary>What the backward-runs comparison looks at: glyph ids, clusters, positions and the defined flags.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparableAll (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>
/// A backward-run request: RTL and LTR letters, default ignorables, variation selectors and spaces, direction LTR or
/// RTL, cluster level 0 to 3, and a variation-selector fallback that is unset or a glyph id.
/// </summary>
/// <param name="fonts">The stripped faces.</param>
/// <returns>The request.</returns>
let private backwardIgnorables (fonts: ImmutableArray<Farfbuzz.Diff.TestFonts.TestFont>) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements (fonts |> Seq.toArray)
        let! length = Gen.choose (1, 16)
        let! codepoints = Gen.arrayOfLength length (Gen.elements backwardCodepoints)
        let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
        let! clusterLevel = Gen.elements [| 0; 1; 2; 3 |]
        let! notFoundVs = Gen.elements [| InvalidCodepoint; 5u |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = direction
                ClusterLevel = clusterLevel
                NotFoundVariationSelectorGlyph = notFoundVs }
    }

/// <summary>The code points the fallback differential draws from: letters, spaces, default ignorables and
/// variation selectors across several scripts and planes.</summary>
let private fallbackCodepoints : uint32 array =
    [| 0x20u; 0x41u; 0x61u; 0x00ADu; 0x034Fu; 0x061Cu; 0x200Bu; 0x200Du; 0x2060u; 0xFEFFu; 0xFE0Fu
       0x0627u; 0x0645u; 0x3042u; 0x4E00u; 0xAC00u; 0x1F600u; 0x1F469u; 0xE0000u; 0x10FFFFu |]

/// <summary>A fallback request over every corpus face (not stripped: the fallback shaper reads no layout table).</summary>
/// <param name="fonts">The faces.</param>
/// <returns>The request.</returns>
let private fallback (fonts: TestFont array) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements fonts
        let! length = Gen.choose (1, 16)
        let! codepoints = Gen.arrayOfLength length (Gen.elements fallbackCodepoints)
        let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "fallback"
                FontFuncs = ValueSome "ot"
                Direction = direction }
    }

/// <summary>The three verification flags run-tests.py puts on every expected-output line.</summary>
let private verificationFlags : Farfbuzz.Harness.ShapeRequest.BufferFlags =
    LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.Harness.ShapeRequest.BufferFlags> (
        uint32 Farfbuzz.Harness.ShapeRequest.BufferFlags.Verify
        ||| uint32 Farfbuzz.Harness.ShapeRequest.BufferFlags.ProduceUnsafeToConcat
        ||| uint32 Farfbuzz.Harness.ShapeRequest.BufferFlags.ProduceSafeToInsertTatweel)

/// <summary>The combining acute, the leading mark of the fixed and generated verify cases.</summary>
[<Literal>]
let private LeadingMark = 0x0301u

/// <summary>The verification flags plus BOT, so a leading mark inserts the dotted circle.</summary>
let private verifyBotFlags : Farfbuzz.Harness.ShapeRequest.BufferFlags =
    LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.Harness.ShapeRequest.BufferFlags> (
        uint32 verificationFlags ||| uint32 Farfbuzz.Harness.ShapeRequest.BufferFlags.BeginningOfText)

/// <summary>The verify verdict as a comparable, keeping only the unported-outlines reason.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>Whether the run verified, or the unported-outlines reason it could not shape.</returns>
let private verifiedOutcome (outcome: Result<ShapeOutcome, string>) : Result<bool, string> =
    match outcome with
    | Ok (Shaped _) -> Ok true
    | Ok ShapingFailed -> Ok false
    | Error reason ->
        match outlinesNotPorted (Error reason) with
        | ValueSome _ -> Error reason
        | ValueNone -> Ok false

/// <summary>The verify verdict and, when it shaped, the glyphs, so both the Ok/Error answer and the glyph ids
/// and clusters agree (mark offsets are phase 5 and are not compared here).</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The glyph ids and clusters, or none when <c>hb_shape_full</c> returned false.</returns>
let private verifyOutcome (outcome: Result<ShapeOutcome, string>) : Result<struct (uint32 * uint32) array, unit> =
    match outcome with
    | Ok (Shaped run) ->
        Ok (run.Infos |> Array.map (fun (info: GlyphInfo) -> struct (info.Codepoint, info.Cluster)))
    | Ok ShapingFailed -> Error ()
    | Error _ -> Error ()

/// <summary>The stripped faces whose vertical origin this phase reads: VORG or the extents fallback.</summary>
/// <param name="fonts">The stripped faces.</param>
/// <returns>The vertical-capable faces.</returns>
let private verticalCapable (fonts: ImmutableArray<Farfbuzz.Diff.TestFonts.TestFont>) : ImmutableArray<Farfbuzz.Diff.TestFonts.TestFont> =
    fonts
    |> Seq.filter (fun font ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index)
        match Origins.vOrigin (Font.create face) 0u with
        | Ok _ -> true
        | Error _ -> false)
    |> ImmutableArray.CreateRange

/// <summary>A fixed verify request: a leading combining acute then two letters, BOT set, with or without the
/// do-not-insert-dotted-circle flag.</summary>
/// <param name="font">The stripped face.</param>
/// <param name="direction">The direction.</param>
/// <param name="doNotInsert">Whether to set DoNotInsertDottedCircle.</param>
/// <returns>The request.</returns>
let private leadingMarkSpec (font: Farfbuzz.Diff.TestFonts.TestFont) (direction: string) (doNotInsert: bool) : ShapeRequest =
    let extra =
        uint32 Farfbuzz.Harness.ShapeRequest.BufferFlags.BeginningOfText
        ||| if doNotInsert then uint32 Farfbuzz.Harness.ShapeRequest.BufferFlags.DoNotInsertDottedCircle else 0u
    let flags =
        LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.Harness.ShapeRequest.BufferFlags> (uint32 verificationFlags ||| extra)
    { defaults font.Path [| 0x0301u; 0x0041u; 0x0042u |] with
        FaceIndex = font.Index
        Shaper = ValueSome "ot"
        FontFuncs = ValueSome "ot"
        Direction = ValueSome direction
        Flags = flags }

/// <summary>
/// A verify request over stripped fonts: BOT set, a leading mark in about one case in five, and directions LTR,
/// RTL and BTT (the vertical direction drawn from vertical-capable faces only).
/// </summary>
/// <param name="fonts">The stripped faces.</param>
/// <param name="verticalFonts">The vertical-capable stripped faces.</param>
/// <returns>The request.</returns>
let private verifyRequest (fonts: ImmutableArray<Farfbuzz.Diff.TestFonts.TestFont>) (verticalFonts: ImmutableArray<Farfbuzz.Diff.TestFonts.TestFont>) : Gen<ShapeRequest> =
    gen {
        let! direction = Gen.frequency [ 3, Gen.constant "ltr"; 3, Gen.constant "rtl"; 1, Gen.constant "btt" ]
        let pool = match direction with | "btt" -> verticalFonts | _ -> fonts
        let! font = Gen.elements (pool |> Seq.toArray)
        let! leading = Gen.frequency [ 4, Gen.constant false; 1, Gen.constant true ]
        let! codepoints =
            match leading with
            | false -> text WithIgnorables
            | true -> gen { let! rest = text WithIgnorables in return Array.append [| LeadingMark |] rest }
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = ValueSome direction
                Flags = verifyBotFlags }
    }

/// <summary>
/// Farfbuzz and HarfBuzz 15.1.0 shape the same requests on stripped fonts; agreement on glyph ids, clusters and
/// positions is checked per case.
/// </summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    let fonts = NoLayoutFonts.all ()
    let verticalFonts = verticalCapable fonts
    let allFonts = TestFonts.all () |> List.toArray
    let fixedFont = fonts |> Seq.find (fun font -> font.Path.Contains "gsub3_1_multiple_f1.otf" && font.Index = 0)
    let fixedSpec =
        { defaults fixedFont.Path [| 0x10850u; 0x216ADu; 0x21328u; 0x2061u |] with
            FaceIndex = 0
            Shaper = ValueSome "ot"
            FontFuncs = ValueSome "ot"
            Direction = ValueSome "ltr" }
    let withDottedCircle = fonts |> Seq.find (fun font -> font.Path.Contains "Amiri-Regular.ttf")
    let withoutDottedCircle = fonts |> Seq.find (fun font -> font.Path.Contains "Roboto-Regular.ttf")
    testList "ShapeDiff" [
        testList "P4.S04" [
            sampledWithOutlines "P4.S04 no-layout shaping, basic LTR"
                (request fonts Basic)
                5000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
                outlinesNotPorted
        ]
        testList "P4.S05" [
            sampledWithOutlines "P4.S05 no-layout shaping, graphemes and RTL"
                (request fonts Rtl)
                5000
                (fun spec -> comparableFlags (farfbuzz.Shape spec))
                (fun spec -> comparableFlags (oracle.Shape spec))
                outlinesNotPorted
            sampledWithOutlines "P4.S05 forced direction"
                (forcedDirection fonts)
                5000
                (fun spec -> comparableFlags (farfbuzz.Shape spec))
                (fun spec -> comparableFlags (oracle.Shape spec))
                outlinesNotPorted
        ]
        testList "P4.S06" [
            sampled "P4.S06 no-layout shaping, full horizontal"
                (fullHorizontal fonts)
                5000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            sampled "P4.S06 backward runs with ignorables"
                (backwardIgnorables fonts)
                5000
                (fun spec -> comparableAll (farfbuzz.Shape spec))
                (fun spec -> comparableAll (oracle.Shape spec))
            testCase "P4.S06 backward runs with ignorables, fixed case" (fun () ->
                Expect.equal (comparableAll (farfbuzz.Shape fixedSpec)) (comparableAll (oracle.Shape fixedSpec)) "agreement")
        ]
        testList "P4.S08" [
            sampled "P4.S08 fallback shaper"
                (fallback allFonts)
                2000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
        ]
        testList "P4.S09" [
            sampledWithOutlines "P4.S09 verify agrees"
                (verifyRequest fonts verticalFonts)
                3000
                (fun spec -> verifiedOutcome (farfbuzz.Shape spec))
                (fun spec -> verifiedOutcome (oracle.Shape spec))
                outlinesNotPorted
            testCase "P4.S09 verify agrees, leading-mark fixed cases" (fun () ->
                let cases =
                    [ "RTL, font with U+25CC", leadingMarkSpec withDottedCircle "rtl" false
                      "RTL, font with U+25CC, do-not-insert", leadingMarkSpec withDottedCircle "rtl" true
                      "RTL, font without U+25CC", leadingMarkSpec withoutDottedCircle "rtl" false
                      "RTL, font without U+25CC, do-not-insert", leadingMarkSpec withoutDottedCircle "rtl" true
                      "LTR, font with U+25CC", leadingMarkSpec withDottedCircle "ltr" false
                      "LTR, font with U+25CC, do-not-insert", leadingMarkSpec withDottedCircle "ltr" true
                      "LTR, font without U+25CC", leadingMarkSpec withoutDottedCircle "ltr" false
                      "LTR, font without U+25CC, do-not-insert", leadingMarkSpec withoutDottedCircle "ltr" true ]
                for name, spec in cases do
                    Expect.equal (verifyOutcome (farfbuzz.Shape spec)) (verifyOutcome (oracle.Shape spec)) name)
        ]
    ]
