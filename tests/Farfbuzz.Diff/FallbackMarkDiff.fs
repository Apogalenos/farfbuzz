module Farfbuzz.Diff.FallbackMarkDiff

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

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Whether a face reads extents from the non-variable glyf header only.</summary>
/// <param name="face">The face.</param>
/// <returns>True for a glyf face without bitmap, colour or CFF outlines.</returns>
let private isSupportedGlyf (face: Face) =
    Face.tryFindTable face Tags.Glyf |> ValueOption.isSome
    && Face.tryFindTable face Tags.Sbix |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cbdt |> ValueOption.isNone
    && Face.tryFindTable face Tags.Colr |> ValueOption.isNone
    && Face.tryFindTable face Tags.Varc |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cff2 |> ValueOption.isNone
    && Face.tryFindTable face Tags.Cff |> ValueOption.isNone

/// <summary>The stripped faces whose extents this phase reads: glyf without bitmap, colour or CFF.</summary>
/// <returns>The faces.</returns>
let private glyfFonts () : ImmutableArray<TestFont> =
    NoLayoutFonts.all ()
    |> Seq.filter (fun font -> isSupportedGlyf (faceOf font (uint32 font.Index)))
    |> ImmutableArray.CreateRange

/// <summary>Whether a category is a letter.</summary>
/// <param name="category">The category.</param>
/// <returns>True for Lu, Ll, Lt, Lm and Lo.</returns>
let private isLetter (category: GeneralCategory) =
    match category with
    | GeneralCategory.UppercaseLetter | GeneralCategory.LowercaseLetter | GeneralCategory.TitlecaseLetter
    | GeneralCategory.ModifierLetter | GeneralCategory.OtherLetter -> true
    | _ -> false

/// <summary>What the comparison looks at: glyph ids, clusters and positions.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparable (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>Whether a code point is a base letter of a default-shaper script that names the run's script.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for non-common default-shaper letters.</returns>
let private isBaseCodepoint (codepoint: uint32) =
    let script = Unicode.script codepoint
    isLetter (Unicode.generalCategory codepoint)
    && script <> Scripts.Common
    && script <> Scripts.Inherited
    && script <> Scripts.Unknown
    && Categorize.shaper script Direction.Ltr Tags.Empty = DefaultShaper

/// <summary>Whether a code point is a non-spacing mark, whatever its script.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for Mn.</returns>
let private isCombiningMark (codepoint: uint32) =
    Unicode.generalCategory codepoint = GeneralCategory.NonSpacingMark

/// <summary>
/// Text of one to three bases, each followed by zero to three marks drawn from the marks the face maps, LTR or RTL,
/// with or without a ZERO-WIDTH JOINER between each base and its marks.
/// </summary>
/// <param name="fonts">The stripped glyf faces.</param>
/// <returns>The request.</returns>
let private marksRequest (fonts: ImmutableArray<TestFont>) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements (fonts |> Seq.toArray)
        let covered = TestFonts.coverage font
        let bases = covered |> Array.filter isBaseCodepoint
        let marks = covered |> Array.filter isCombiningMark
        match bases.Length = 0 || marks.Length = 0 with
        | true -> return { defaults font.Path [||] with FaceIndex = font.Index; Shaper = ValueSome "ot"; FontFuncs = ValueSome "ot" }
        | false ->
            let! baseCount = Gen.choose (1, 3)
            let! pieces =
                Gen.arrayOfLength baseCount (gen {
                    let! baseCodepoint = Gen.elements bases
                    let! markCount = Gen.choose (0, 3)
                    let! markCodepoints = Gen.arrayOfLength markCount (Gen.elements marks)
                    return struct (baseCodepoint, markCodepoints)
                })
            let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
            let! withZwj = Gen.elements [| false; true |]
            let text =
                pieces
                |> Array.collect (fun struct (baseCodepoint, markCodepoints) ->
                    match withZwj && markCodepoints.Length > 0 with
                    | true -> Array.append [| baseCodepoint; 0x200Du |] markCodepoints
                    | false -> Array.append [| baseCodepoint |] markCodepoints)
            return
                { defaults font.Path text with
                    FaceIndex = font.Index
                    Shaper = ValueSome "ot"
                    FontFuncs = ValueSome "ot"
                    Direction = direction }
    }

/// <summary>The ZERO-WIDTH JOINER code point.</summary>
[<Literal>]
let private Zwj = 0x200Du

/// <summary>The space code point, an empty glyph in most faces.</summary>
[<Literal>]
let private Space = 0x0020u

/// <summary>Builds a plain ot request over a face.</summary>
/// <param name="font">The face.</param>
/// <param name="text">The text.</param>
/// <param name="direction">The direction, or none to guess it.</param>
/// <returns>The request.</returns>
let private specOf (font: TestFont) (text: uint32 array) (direction: string voption) : ShapeRequest =
    { defaults font.Path text with
        FaceIndex = font.Index
        Shaper = ValueSome "ot"
        FontFuncs = ValueSome "ot"
        Direction = direction }

/// <summary>The fixed fallback-mark cases, each compared with HarfBuzz.</summary>
/// <param name="fonts">The stripped glyf faces.</param>
/// <returns>The named requests.</returns>
let private fixedCases (fonts: ImmutableArray<TestFont>) : (string * ShapeRequest) list =
    let gulzar = fonts |> Seq.find (fun font -> font.Path.Contains "Gulzar")
    let gulzarCovered = TestFonts.coverage gulzar
    let gulzarBase = gulzarCovered |> Array.find isBaseCodepoint
    let gulzarMark = gulzarCovered |> Array.find isCombiningMark
    let roboto = fonts |> Seq.find (fun font -> font.Path.Contains "Roboto")
    let robotoCovered = TestFonts.coverage roboto
    let robotoBase = robotoCovered |> Array.find isBaseCodepoint
    let robotoMark = robotoCovered |> Array.find isCombiningMark
    let class0 =
        fonts
        |> Seq.tryPick (fun font ->
            TestFonts.coverage font
            |> Array.tryPick (fun u ->
                match isCombiningMark u && Unicode.combiningClass u = 0uy && Unicode.modifiedCombiningClass u = 0uy with
                | true -> Some struct (font, u)
                | false -> None)
            |> Option.map (fun struct (font, u) -> struct (font, u)))
    let class0Spec =
        match class0 with
        | Some struct (font, u) ->
            let baseCp = TestFonts.coverage font |> Array.find isBaseCodepoint
            specOf font [| baseCp; u |] (ValueSome "ltr")
        | None -> specOf roboto [| robotoBase; robotoMark |] (ValueSome "ltr")
    [ "Gulzar base and mark", specOf gulzar [| gulzarBase; gulzarMark |] (ValueSome "rtl")
      "mark with class 0", class0Spec
      "mark at the start of the text", specOf roboto [| robotoMark; robotoBase |] (ValueSome "ltr")
      "base without extents (space)", specOf roboto [| Space; robotoMark |] (ValueSome "ltr")
      "cluster longer than 64 glyphs", specOf roboto (Array.append [| robotoBase |] (Array.create 66 robotoMark)) (ValueSome "ltr") ]

/// <summary>A stripped face whose outlines are CFF 1, for the unported-outlines fixed case.</summary>
/// <param name="fonts">The stripped faces.</param>
/// <returns>The face.</returns>
let private cff1Face (fonts: ImmutableArray<TestFont>) : TestFont =
    fonts
    |> Seq.find (fun font ->
        let face = faceOf font (uint32 font.Index)
        Face.tryFindTable face Tags.Glyf |> ValueOption.isNone
        && Face.tryFindTable face Tags.Cff |> ValueOption.isSome)

/// <summary>Farfbuzz and HarfBuzz 15.1.0 agree on fallback mark positioning over stripped faces.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    let glyf = glyfFonts ()
    let all = NoLayoutFonts.all ()
    testList "FallbackMarkDiff" [
        testList "P5.S14" [
            sampled "fallback mark positioning"
                (ShapeGens.request glyf WithMarks)
                3000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            sampledWithOutlines "marks on every stripped face"
                (marksRequest all)
                5000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
                outlinesNotPorted
            testCase "marks on every stripped face, fixed cases" (fun () ->
                for name, spec in fixedCases glyf do
                    Expect.equal (comparable (farfbuzz.Shape spec)) (comparable (oracle.Shape spec)) name)
            testCase "a mark on a stripped CFF face is positioned as HarfBuzz positions it" (fun () ->
                let cff = cff1Face all
                let covered = TestFonts.coverage cff
                let baseCodepoint = covered |> Array.find isBaseCodepoint
                let markCodepoint = covered |> Array.find isCombiningMark
                let spec = specOf cff [| baseCodepoint; markCodepoint |] (ValueSome "ltr")
                Expect.equal (comparable (farfbuzz.Shape spec)) (comparable (oracle.Shape spec)) "the CFF face positions the mark from the CFF1 extents")
        ]
    ]
