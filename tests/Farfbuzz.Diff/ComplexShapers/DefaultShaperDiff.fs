module Farfbuzz.Diff.ComplexShapers.DefaultShaperDiff

open System
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.Shaper
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Diff
open Farfbuzz.Diff.HarfBuzzSharpShaper
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.ComplexShapers.HebrewThaiDiff

/// <summary>The cases the differential reaches over all the faces and variants, the least the work asks for.</summary>
[<Literal>]
let private TotalCases = 24_000

/// <summary>The fewest cases one face variant gets.</summary>
[<Literal>]
let private CasesPerVariantMinimum = 60

/// <summary>The most faces of one outline kind the differential takes besides the faces that have no outlines.</summary>
[<Literal>]
let private GlyfFaces = 40

/// <summary>The most faces with CFF outlines the differential takes.</summary>
[<Literal>]
let private CffFaces = 8

/// <summary>The most faces with bitmap or colour outlines the differential takes.</summary>
[<Literal>]
let private BitmapFaces = 4

/// <summary>The hash prefix of the face on which the difference was found: no outlines, a GDEF with glyph classes for five
/// glyphs, a GSUB and no GPOS.</summary>
[<Literal>]
let private FoundOnFace = "755160dd"

/// <summary>The kinds of outline source a face has, which decide whether the fallback mark positioning can ask for glyph extents; the
/// colour and composite outlines are not ported and the differential leaves them out.</summary>
type private OutlineKind =
    | NoOutlines
    | Glyf
    | Cff
    | Bitmap
    | Unported

/// <summary>The outline kind of a face, read from its table directory.</summary>
/// <param name="font">The face.</param>
/// <returns>The kind: colour or composite outlines, whose extents are not ported, then bitmaps, then glyf, then CFF, else none.</returns>
let private outlineKindOf (font: TestFont) : OutlineKind =
    let face = Face.create (ReadOnlyMemory (File.ReadAllBytes font.Path)) (uint32 font.Index)
    let has (tag: Tag) = Face.tryFindTable face tag |> ValueOption.isSome
    match has Tags.Colr || has Tags.Varc, has Tags.Sbix || has Tags.Cbdt, has Tags.Glyf, has Tags.Cff || has Tags.Cff2 with
    | true, _, _, _ -> Unported
    | false, true, _, _ -> Bitmap
    | false, false, true, _ -> Glyf
    | false, false, false, true -> Cff
    | false, false, false, false -> NoOutlines

/// <summary>Whether a face is a plain single-face font, from which a table can be renamed out.</summary>
/// <param name="font">The face.</param>
/// <returns>True for a face that is not part of a collection.</returns>
let private isPlain (font: TestFont) : bool =
    let bytes = File.ReadAllBytes font.Path
    bytes.Length > 12 && u32 bytes 0 <> uint32 Tags.Collection && font.Index = 0

/// <summary>A sample of at most a number of the elements of a list, spread over all of it.</summary>
/// <param name="count">The number of elements wanted at most.</param>
/// <param name="items">The elements.</param>
/// <returns>The sample.</returns>
let private spread (count: int) (items: 'a list) : 'a list =
    let step = max 1 ((items.Length + count - 1) / max 1 count)
    items |> List.indexed |> List.filter (fun (index, _) -> index % step = 0) |> List.map snd

/// <summary>The faces the differential runs on: every plain face without outlines, a spread of the faces with glyf, CFF and
/// bitmap outlines, and the face the difference was found on.</summary>
let private faces : Lazy<(TestFont * OutlineKind) list> =
    lazy (
        let classified = TestFonts.all () |> List.filter isPlain |> List.map (fun font -> font, outlineKindOf font)
        let ofKind (kind: OutlineKind) = classified |> List.filter (fun (_, candidate) -> candidate = kind)
        let found = classified |> List.filter (fun (font, _) -> (Path.GetFileName font.Path |> nonNull).StartsWith FoundOnFace)
        ofKind NoOutlines @ spread GlyfFaces (ofKind Glyf) @ spread CffFaces (ofKind Cff) @ spread BitmapFaces (ofKind Bitmap) @ found
        |> List.distinctBy (fun (font, _) -> font.Path))

/// <summary>The variants of a face the differential runs: the face as it is, without GPOS (glyph classes and substitution kept),
/// without GDEF (positioning kept, the glyph classes synthesised from the Unicode categories), without GSUB, and with every
/// layout table stripped.</summary>
/// <param name="font">The face.</param>
/// <returns>The variants and their names.</returns>
let private variantsOf (font: TestFont) : (string * TestFont) list =
    let without (tag: string) =
        match withoutTable tag font with
        | ValueSome copy -> [ $"no {tag}", copy ]
        | ValueNone -> []
    [ "layout", font ] @ without "GPOS" @ without "GDEF" @ without "GSUB" @ [ "stripped", NoLayoutFonts.strip font ]

/// <summary>The code points of the Unicode versions the face data covers whose general category is a mark, by combining class:
/// those of class zero (the Gurmukhi bindi among them) and the others.</summary>
let private marks : Lazy<uint32 array * uint32 array> =
    lazy (
        let candidates = [| 0x0300u .. 0x1FFFFu |]
        let isMark (codepoint: uint32) =
            match Unicode.generalCategory codepoint with
            | GeneralCategory.NonSpacingMark | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark -> true
            | GeneralCategory.Control | GeneralCategory.Format | GeneralCategory.Unassigned | GeneralCategory.PrivateUse
            | GeneralCategory.Surrogate | GeneralCategory.LowercaseLetter | GeneralCategory.ModifierLetter | GeneralCategory.OtherLetter
            | GeneralCategory.TitlecaseLetter | GeneralCategory.UppercaseLetter | GeneralCategory.DecimalNumber | GeneralCategory.LetterNumber
            | GeneralCategory.OtherNumber | GeneralCategory.ConnectPunctuation | GeneralCategory.DashPunctuation | GeneralCategory.ClosePunctuation
            | GeneralCategory.FinalPunctuation | GeneralCategory.InitialPunctuation | GeneralCategory.OtherPunctuation | GeneralCategory.OpenPunctuation
            | GeneralCategory.CurrencySymbol | GeneralCategory.ModifierSymbol | GeneralCategory.MathSymbol | GeneralCategory.OtherSymbol
            | GeneralCategory.LineSeparator | GeneralCategory.ParagraphSeparator | GeneralCategory.SpaceSeparator -> false
        let all = candidates |> Array.filter isMark
        all |> Array.filter (fun codepoint -> Unicode.combiningClass codepoint = 0uy), all |> Array.filter (fun codepoint -> Unicode.combiningClass codepoint <> 0uy))

/// <summary>The letters of every script, the code points a default-shaper font rarely maps.</summary>
let private letters : Lazy<uint32 array> =
    lazy (
        Array.append [| 0x00C0u .. 0x2FFFu |] [| 0xA000u .. 0xABFFu |]
        |> Array.filter (fun codepoint ->
            match Unicode.generalCategory codepoint with
            | GeneralCategory.LowercaseLetter | GeneralCategory.OtherLetter | GeneralCategory.UppercaseLetter | GeneralCategory.ModifierLetter -> true
            | GeneralCategory.Control | GeneralCategory.Format | GeneralCategory.Unassigned | GeneralCategory.PrivateUse
            | GeneralCategory.Surrogate | GeneralCategory.TitlecaseLetter | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark
            | GeneralCategory.NonSpacingMark | GeneralCategory.DecimalNumber | GeneralCategory.LetterNumber | GeneralCategory.OtherNumber
            | GeneralCategory.ConnectPunctuation | GeneralCategory.DashPunctuation | GeneralCategory.ClosePunctuation | GeneralCategory.FinalPunctuation
            | GeneralCategory.InitialPunctuation | GeneralCategory.OtherPunctuation | GeneralCategory.OpenPunctuation | GeneralCategory.CurrencySymbol
            | GeneralCategory.ModifierSymbol | GeneralCategory.MathSymbol | GeneralCategory.OtherSymbol | GeneralCategory.LineSeparator
            | GeneralCategory.ParagraphSeparator | GeneralCategory.SpaceSeparator -> false))

/// <summary>The default-ignorable and joining code points, and the spaces.</summary>
let private ignorables : uint32 array =
    [| 0x00ADu; 0x034Fu; 0x061Cu; 0x180Bu; 0x180Cu; 0x180Du; 0x180Eu; 0x200Bu; 0x200Cu; 0x200Du; 0x200Eu; 0x200Fu; 0x2060u; 0x2061u; 0x2064u
       0xFE00u; 0xFE0Fu; 0xFEFFu; 0xE0100u; 0xE01EFu; 0x0020u; 0x00A0u; 0x2002u; 0x2003u; 0x200Au; 0x202Fu |]

/// <summary>The Latin letters and digits.</summary>
let private latinLetters : uint32 array =
    spanning [ 0x41u, 0x5Au; 0x61u, 0x7Au; 0x30u, 0x39u ] |> List.toArray

/// <summary>The Latin letters.</summary>
let private latinOnly : uint32 array =
    spanning [ 0x41u, 0x5Au; 0x61u, 0x7Au ] |> List.toArray

/// <summary>A generator of one code point of an array, from those the face maps three times in five when it maps any.</summary>
/// <param name="covered">The code points the face maps.</param>
/// <param name="candidates">The candidates.</param>
/// <returns>The generator.</returns>
let private drawn (covered: Set<uint32>) (candidates: uint32 array) : Gen<uint32> =
    let mapped = candidates |> Array.filter covered.Contains
    let unmapped = candidates |> Array.filter (fun codepoint -> not (covered.Contains codepoint))
    match mapped.Length, unmapped.Length with
    | 0, _ -> Gen.elements candidates
    | _, 0 -> Gen.elements candidates
    | _ -> Gen.frequency [ 2, Gen.elements mapped; 3, Gen.elements unmapped ]

/// <summary>The generator of default-shaper texts for a face: bases that are Latin letters the face maps, letters of other
/// scripts it does not, spaces and joiners, each followed by no mark or up to three marks of other scripts, the combining class
/// zero ones among them, with a joiner now and then between the base and its marks. Of the marks and the letters the face does
/// not map most are notdef glyphs with a mark general category, which is the case the zeroing of marks acts on.</summary>
/// <param name="covered">The code points the face maps.</param>
/// <param name="latinFirst">Whether the text starts with a Latin letter, so that the script guessed from it is Latin.</param>
/// <returns>The generator of texts.</returns>
let private defaultText (covered: Set<uint32>) (latinFirst: bool) : Gen<uint32 array> =
    let zeroMarks, classedMarks = marks.Value
    let latin = drawn covered latinLetters
    let letter = drawn covered letters.Value
    let ignorable = Gen.elements ignorables
    let mark = Gen.frequency [ 5, drawn covered zeroMarks; 4, drawn covered classedMarks; 1, ignorable ]
    let bases = Gen.frequency [ 6, latin; 5, letter; 2, ignorable ]
    let marksAfter =
        gen {
            let! count = Gen.frequency [ 2, Gen.constant 0; 4, Gen.constant 1; 3, Gen.constant 2; 1, Gen.constant 3 ]
            let! joiner = Gen.frequency [ 5, Gen.constant []; 1, Gen.constant [ 0x200Du ]; 1, Gen.constant [ 0x200Cu ] ]
            let! chosen = Gen.listOfLength count mark
            return joiner @ chosen
        }
    let cluster = gen { let! baseCodepoint = bases in let! following = marksAfter in return baseCodepoint :: following }
    gen {
        let! count = Gen.choose (1, 6)
        let! first = Gen.frequency [ 3, cluster; 1, mark |> Gen.map List.singleton ]
        let! lead = drawn covered latinOnly
        let! rest = Gen.listOfLength count cluster
        let body = (match latinFirst with | true -> [ lead ] | false -> first) :: rest
        return body |> List.concat |> List.toArray
    }

/// <summary>The option fields: the default-shaper scripts, each direction, each cluster level, the buffer flags for the default
/// ignorables, and a vertical direction. The option field without a script is used only with a text that starts with a Latin
/// letter, whose guessed script is Latin.</summary>
let private scriptedOptions : string array =
    let spelled =
        [| for script in [ "Latn"; "Cyrl"; "Grek"; "Zyyy"; "Armn" ] do
            for direction in [ "ltr"; "rtl" ] do
                for level in [ 0; 1; 2; 3 ] do
                    yield $"--script={script} --direction={direction} --cluster-level={level}" |]
    let flagged =
        [| "--script=Latn --direction=ttb"; "--script=Latn --direction=btt --cluster-level=2"; "--script=Latn --bot --eot"
           "--script=Latn --remove-default-ignorables --cluster-level=2"; "--script=Latn --preserve-default-ignorables --cluster-level=3" |]
    Array.append spelled flagged

/// <summary>The option fields with no script given.</summary>
let private guessedOptions : string array =
    [| String.Empty; "--direction=ltr"; "--direction=rtl"; "--cluster-level=1"; "--cluster-level=2 --direction=rtl"; "--cluster-level=3"; "--language=en"
       "--language=pa"; "--direction=rtl --cluster-level=0" |]

/// <summary>A generator of requests: the option field with its text, and for no script a text that starts with a Latin letter.</summary>
/// <param name="fontFile">The font path.</param>
/// <param name="covered">The code points the face maps.</param>
/// <returns>The generator.</returns>
let private requests (fontFile: string) (covered: Set<uint32>) : Gen<ShapeRequest> =
    gen {
        let! guessed = Gen.frequency [ 4, Gen.constant false; 1, Gen.constant true ]
        let! options = Gen.elements (match guessed with | true -> guessedOptions | false -> scriptedOptions)
        let! codepoints = defaultText covered guessed
        return ShapeDiff.withFlags (ShapeDiff.request fontFile options codepoints)
    }

/// <summary>The answer of an engine for a request, as the glyph serialisation with glyph flags: glyphs, clusters, advances,
/// offsets and flags.</summary>
/// <param name="shaper">The engine.</param>
/// <param name="request">The request.</param>
/// <returns>The text, or why there is none.</returns>
let private answer (shaper: Shaper) (request: ShapeRequest) : Result<string, string> =
    ShapeDiff.serialize (shaper.Shape request)

/// <summary>The differential of the default shaper over text that its face does not map: for each variant of each face, Farfbuzz
/// against HarfBuzz on random text of letters and marks of other scripts, so that notdef glyphs stand for marks, with and without
/// GDEF, GPOS, GSUB and every layout table, in each direction and at each cluster level.</summary>
/// <returns>The tests.</returns>
let tests () : Test =
    let all = faces.Value |> List.collect (fun (font, kind) -> variantsOf font |> List.map (fun (variant, copy) -> font, kind, variant, copy))
    let each = max CasesPerVariantMinimum ((TotalCases + all.Length - 1) / max 1 all.Length)
    let kinds = faces.Value |> List.countBy snd |> List.map (fun (kind, count) -> $"{count} {kind}") |> String.concat ", "
    Console.WriteLine $"default shaper over unmapped marks: {faces.Value.Length} faces ({kinds}), {all.Length} variants, {each} cases each, {each * all.Length} cases"
    let ours = FarfbuzzShaper.shaper
    let oracle = create ()
    testList "P6.S21 default shaper over unmapped marks" [
        for font, kind, variant, copy in all ->
            Differential.sampledWithOutlines $"default shaper {label font} {kind} {variant}" (requests copy.Path (covered font)) each
                (answer ours) (answer oracle) Differential.outlinesNotPorted
    ]

/// <summary>The fixed case on which the difference was found: the text of the Indic differential that begins with a Latin letter,
/// with the script given and guessed, shaped without a layout table and with the face's own.</summary>
/// <returns>The tests.</returns>
let foundCases () : Test =
    let font = faces.Value |> List.map fst |> List.find (fun candidate -> (Path.GetFileName candidate.Path |> nonNull).StartsWith FoundOnFace)
    let codepoints = [| 0x4Eu; 0x0A2Du; 0x0A42u; 0x0A02u; 0x0A10u; 0x0A4Du; 0x200Cu; 0x0A02u |]
    let ours = FarfbuzzShaper.shaper
    let oracle = create ()
    testList "P6.S21 default shaper found case" [
        for variant, copy in variantsOf font do
            for options in [ "--script=Latn"; String.Empty; "--script=Latn --direction=rtl"; "--script=Latn --cluster-level=3" ] do
                yield testCase $"{variant} [{options}]" (fun () ->
                    let request = ShapeDiff.withFlags (ShapeDiff.request copy.Path options codepoints)
                    Expect.equal (answer ours request) (answer oracle request) "Farfbuzz shapes the text as HarfBuzz does")
    ]
