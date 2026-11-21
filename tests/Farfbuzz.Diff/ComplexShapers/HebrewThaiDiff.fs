module Farfbuzz.Diff.ComplexShapers.HebrewThaiDiff

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open Expecto
open Farfbuzz
open Farfbuzz.Diff
open Farfbuzz.Diff.ComplexShapers
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Harness

/// <summary>The code points of the inclusive ranges, in order.</summary>
/// <param name="ranges">The first and last code point of each range.</param>
/// <returns>The code points.</returns>
let spanning (ranges: (uint32 * uint32) list) : uint32 list =
    ranges |> List.collect (fun (first, last) -> [ first .. last ])

/// <summary>The cases generated per script, the least the work asks for.</summary>
[<Literal>]
let private CasesPerScript = 20_000

/// <summary>The fewest cases one font gets.</summary>
[<Literal>]
let private CasesPerFontMinimum = 500

/// <summary>The code points a face maps, cached.</summary>
let private coverageCache = System.Collections.Concurrent.ConcurrentDictionary<struct (string * int), Set<uint32>>()

/// <summary>The code points a face maps.</summary>
/// <param name="font">The face.</param>
/// <returns>The set.</returns>
let covered (font: TestFont) : Set<uint32> =
    coverageCache.GetOrAdd(struct (font.Path, font.Index), fun _ -> Set.ofArray (TestFonts.coverage font))

/// <summary>How many code points of a list a face maps.</summary>
/// <param name="font">The face.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The count.</returns>
let private coveredCount (font: TestFont) (codepoints: uint32 list) : int =
    let set = covered font
    codepoints |> List.filter set.Contains |> List.length

let private hebrewLetters = spanning [ 0x05D0u, 0x05EAu ]
let private hebrewPoints = spanning [ 0x05B0u, 0x05C7u ]
let private hebrewCantillation = spanning [ 0x0591u, 0x05AFu ]
let private hebrewOther = spanning [ 0x05F0u, 0x05F2u; 0xFB1Du, 0xFB4Fu ]
let private latin = spanning [ 0x41u, 0x5Au; 0x61u, 0x7Au; 0x30u, 0x39u; 0x20u, 0x20u ]

/// <summary>The alphabet of the Hebrew differential: letters and points twice as likely as cantillation marks and
/// presentation forms, mixed with Latin letters, digits and the space.</summary>
let private hebrewAlphabet : ImmutableArray<uint32> =
    ImmutableArray.CreateRange (hebrewLetters @ hebrewLetters @ hebrewPoints @ hebrewPoints @ hebrewCantillation @ hebrewOther @ latin)

let private thaiRange = spanning [ 0x0E01u, 0x0E5Bu ]
let private thaiBoost = [ 0x0E33u; 0x0E33u; 0x0E33u ] @ spanning [ 0x0E31u, 0x0E31u; 0x0E34u, 0x0E3Au; 0x0E47u, 0x0E4Eu ]

/// <summary>The alphabet of the Thai differential: the Thai block, with the sara am and the marks more likely, mixed with
/// Latin letters, digits and the space.</summary>
let private thaiAlphabet : ImmutableArray<uint32> =
    ImmutableArray.CreateRange (thaiRange @ thaiBoost @ thaiBoost @ latin)

let private laoRange = spanning [ 0x0E81u, 0x0EDFu ]
let private laoBoost = [ 0x0EB3u; 0x0EB3u; 0x0EB3u ] @ spanning [ 0x0EB1u, 0x0EB1u; 0x0EB4u, 0x0EBCu; 0x0EC8u, 0x0ECDu ]

/// <summary>The alphabet of the Lao differential: the Lao block, with the sara am and the marks more likely, mixed with
/// Latin letters, digits and the space.</summary>
let private laoAlphabet : ImmutableArray<uint32> =
    ImmutableArray.CreateRange (laoRange @ laoBoost @ laoBoost @ latin)

/// <summary>The option fields of the Hebrew differential: the script requested, with each direction and without one.</summary>
let private hebrewOptions : string array =
    [| "--script=Hebr --direction=rtl"; "--script=Hebr --direction=ltr"; "--script=Hebr"; "--script=Hebr --language=he"; String.Empty |]

/// <summary>The option fields of the Thai differential.</summary>
let private thaiOptions : string array =
    [| "--script=Thai --direction=ltr"; "--script=Thai --direction=rtl"; "--script=Thai"; "--script=Thai --language=th"; String.Empty |]

/// <summary>The option fields of the Lao differential.</summary>
let private laoOptions : string array =
    [| "--script=Laoo --direction=ltr"; "--script=Laoo --direction=rtl"; "--script=Laoo"; "--script=Laoo --language=lo"; String.Empty |]

/// <summary>Every face of the corpus that covers enough of a script, and the same face with its layout tables stripped.</summary>
/// <param name="letters">The letters whose coverage decides.</param>
/// <param name="atLeast">How many of them a face must map.</param>
/// <returns>The faces.</returns>
let facesCovering (letters: uint32 list) (atLeast: int) : TestFont list =
    TestFonts.all () |> List.filter (fun font -> coveredCount font letters >= atLeast)

/// <summary>The faces the Hebrew differential runs on: those that map at least eight code points of the Hebrew block or
/// of the presentation forms.</summary>
let private hebrewFaces : Lazy<TestFont list> = lazy (facesCovering (spanning [ 0x0590u, 0x05FFu; 0xFB1Du, 0xFB4Fu ]) 8)

/// <summary>The faces the Thai differential runs on.</summary>
let private thaiFaces : Lazy<TestFont list> = lazy (facesCovering (spanning [ 0x0E01u, 0x0E5Bu ]) 8)

/// <summary>The faces the Lao differential runs on.</summary>
let private laoFaces : Lazy<TestFont list> = lazy (facesCovering (spanning [ 0x0E81u, 0x0EDFu ]) 8)

/// <summary>The number of cases each of a number of fonts gets for a script to reach its total.</summary>
/// <param name="fonts">The number of fonts.</param>
/// <returns>The cases per font.</returns>
let private perFont (fonts: int) : int =
    max CasesPerFontMinimum ((CasesPerScript + fonts - 1) / max 1 fonts)

/// <summary>A short name of a face for a test name.</summary>
/// <param name="font">The face.</param>
/// <returns>The file name and face index.</returns>
let label (font: TestFont) : string =
    let name = Path.GetFileName font.Path |> nonNull
    $"{name.Substring(0, min 18 name.Length)}:{font.Index}"

/// <summary>The labels of some faces, space separated.</summary>
/// <param name="faces">The faces.</param>
/// <returns>The labels.</returns>
let names (faces: TestFont list) : string =
    faces |> List.map label |> String.concat " "

/// <summary>Reads a big-endian 16-bit value.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The offset.</param>
/// <returns>The value.</returns>
let u16 (bytes: byte array) (offset: int) : int =
    int (BinaryPrimitives.ReadUInt16BigEndian(ReadOnlySpan(bytes, offset, 2)))

/// <summary>Reads a big-endian 32-bit value.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The offset.</param>
/// <returns>The value.</returns>
let u32 (bytes: byte array) (offset: int) : uint32 =
    BinaryPrimitives.ReadUInt32BigEndian(ReadOnlySpan(bytes, offset, 4))

/// <summary>Writes a big-endian 32-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let be32 (value: uint32) : byte array =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian 16-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let be16 (value: int) : byte array =
    [| byte (value >>> 8); byte value |]

/// <summary>Builds a format 12 cmap table, platform 3 encoding 10, from code point and glyph pairs.</summary>
/// <param name="mapping">The pairs, ascending by code point.</param>
/// <returns>The table.</returns>
let cmapOf (mapping: (uint32 * uint32) list) : byte array =
    let groups = mapping |> List.map (fun (codepoint, glyph) -> Array.concat [ be32 codepoint; be32 codepoint; be32 glyph ]) |> Array.concat
    let subtable = Array.concat [ be16 12; be16 0; be32 (uint32 (16 + groups.Length)); be32 0u; be32 (uint32 mapping.Length); groups ]
    Array.concat [ be16 0; be16 1; be16 3; be16 10; be32 12u; subtable ]

/// <summary>Rebuilds a single-face sfnt with its cmap table replaced.</summary>
/// <param name="bytes">The font bytes.</param>
/// <param name="cmap">The new cmap table.</param>
/// <returns>The new font bytes.</returns>
let withCmap (bytes: byte array) (cmap: byte array) : byte array =
    let count = u16 bytes 4
    let tables =
        [ for index in 0 .. count - 1 ->
            let record = 12 + index * 16
            let tag = u32 bytes record
            let offset = int (u32 bytes (record + 8))
            let length = int (u32 bytes (record + 12))
            match tag = uint32 Tags.Cmap with
            | true -> tag, cmap
            | false -> tag, Array.sub bytes offset length ]
    let padded (data: byte array) = Array.append data (Array.zeroCreate ((4 - data.Length % 4) % 4))
    let start = 12 + count * 16
    let offsets = tables |> List.scan (fun offset (_, data) -> offset + (padded data).Length) start
    let directory =
        List.map2 (fun (tag, data: byte array) offset -> Array.concat [ be32 tag; be32 0u; be32 (uint32 offset); be32 (uint32 data.Length) ]) tables (List.take count offsets)
    Array.concat ([ Array.sub bytes 0 12 ] @ directory @ (tables |> List.map (snd >> padded)))

/// <summary>Writes a copy of a plain single-face font whose table of a tag is renamed out, so the face shapes as if it had none of it
/// and keeps its other tables. The copy is written under <c>artifacts/synthetic</c>.</summary>
/// <param name="tag">The table tag, four letters.</param>
/// <param name="font">The face.</param>
/// <returns>The copy, or none for a face of a collection or a font without the table.</returns>
let withoutTable (tag: string) (font: TestFont) : TestFont voption =
    let bytes = File.ReadAllBytes font.Path
    let plain = bytes.Length > 12 && u32 bytes 0 <> uint32 Tags.Collection && font.Index = 0
    let records = match plain with | true -> [ for index in 0 .. u16 bytes 4 - 1 -> 12 + index * 16 ] | false -> []
    match records |> List.tryFind (fun record -> u32 bytes record = uint32 (Tag.ofString tag)) with
    | None -> ValueNone
    | Some record ->
        let copy = Array.copy bytes
        copy[record] <- byte 'X'
        let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "synthetic")
        Directory.CreateDirectory directory |> ignore
        let hash = Convert.ToHexString(System.Security.Cryptography.SHA1.HashData(System.Text.Encoding.UTF8.GetBytes font.Path)).Substring(0, 12).ToLowerInvariant()
        let target = Path.Combine(directory, $"{hash}-{tag.ToLowerInvariant()}-renamed.ttf")
        File.WriteAllBytes(target, copy)
        ValueSome { font with Path = target }

/// <summary>Writes a copy of a plain single-face font whose GPOS table is renamed out, so it positions without GPOS and
/// keeps its substitution, glyph classes and cmap. The copy is written under <c>artifacts/synthetic</c>.</summary>
/// <param name="font">The face.</param>
/// <returns>The copy, or none for a face of a collection or a font without GPOS.</returns>
let withoutGpos (font: TestFont) : TestFont voption =
    withoutTable "GPOS" font

/// <summary>The variants of a face the differentials run: the face with its layout, the face stripped of every layout
/// table, and the face with only its GPOS renamed out.</summary>
/// <param name="font">The face.</param>
/// <returns>The variants and their names.</returns>
let private variants (font: TestFont) : (string * TestFont) list =
    let gsubOnly =
        match withoutGpos font with
        | ValueSome copy -> [ "no GPOS", copy ]
        | ValueNone -> []
    [ "layout", font; "stripped", NoLayoutFonts.strip font ] @ gsubOnly

/// <summary>One sampled differential for each variant of each face, on one alphabet.</summary>
/// <param name="prefix">The test name prefix.</param>
/// <param name="faces">The faces.</param>
/// <param name="options">The option fields.</param>
/// <param name="alphabet">The alphabet.</param>
/// <param name="maxLength">The longest text.</param>
/// <returns>The tests.</returns>
let private sampledOver (prefix: string) (faces: TestFont list) (options: string array) (alphabet: ImmutableArray<uint32>) (maxLength: int) : Test list =
    let all = faces |> List.collect (fun font -> variants font |> List.map (fun (kind, variant) -> font, kind, variant))
    let each = perFont all.Length
    let focused (font: TestFont) : ImmutableArray<uint32> =
        let set = covered font
        let mapped = alphabet |> Seq.filter set.Contains |> Seq.toList
        ImmutableArray.CreateRange (mapped @ mapped @ mapped @ List.ofSeq alphabet)
    Console.WriteLine $"{prefix}: {faces.Length} faces ({names faces}), {all.Length} variants, {each} cases each, {each * all.Length} cases"
    [ for font, kind, variant in all -> ShapeDiff.against $"{prefix} {label font} {kind}" variant.Path options (focused font) maxLength each ]

/// <summary>The Windows PUA forms of the Thai PUA shaping (src/hb-ot-shaper-thai.cc:98-134).</summary>
let private windowsForms : uint32 list =
    [ 0xF70Au; 0xF70Bu; 0xF70Cu; 0xF70Du; 0xF70Eu; 0xF718u; 0xF719u; 0xF71Au; 0xF705u; 0xF706u; 0xF707u; 0xF708u; 0xF709u
      0xF713u; 0xF714u; 0xF715u; 0xF716u; 0xF717u; 0xF710u; 0xF701u; 0xF702u; 0xF703u; 0xF704u; 0xF712u; 0xF711u; 0xF70Fu; 0xF700u ]

/// <summary>The Mac PUA forms of the same tables.</summary>
let private macForms : uint32 list =
    [ 0xF88Bu; 0xF88Eu; 0xF891u; 0xF894u; 0xF897u; 0xF89Bu; 0xF89Cu; 0xF89Du; 0xF88Cu; 0xF88Fu; 0xF892u; 0xF895u; 0xF898u
      0xF88Au; 0xF88Du; 0xF890u; 0xF893u; 0xF896u; 0xF884u; 0xF885u; 0xF886u; 0xF887u; 0xF888u; 0xF889u; 0xF899u; 0xF89Au; 0xF89Eu ]

/// <summary>Writes a copy of a font whose cmap also maps PUA code points, each to a glyph the font already has, so a
/// shaper that reaches for the PUA forms finds them. The copy is written under <c>artifacts/synthetic</c>.</summary>
/// <param name="source">The face the copy is made from.</param>
/// <param name="name">The copy's name.</param>
/// <param name="forms">The PUA code points to add.</param>
/// <returns>The face of the copy.</returns>
let private withPuaForms (source: TestFont) (name: string) (forms: uint32 list) : TestFont =
    let bytes = File.ReadAllBytes source.Path
    let face = Face.create (ReadOnlyMemory bytes) (uint32 source.Index)
    let font = Font.create face
    let mapped =
        TestFonts.coverage source
        |> Array.toList
        |> List.choose (fun codepoint -> match Font.nominalGlyph font codepoint with | ValueSome glyph -> Some (codepoint, glyph) | ValueNone -> None)
    let glyphs = mapped |> List.map snd |> List.filter (fun glyph -> glyph <> 0u) |> List.distinct |> List.toArray
    let added = forms |> List.mapi (fun index form -> form, glyphs[(index * 7 + 3) % glyphs.Length])
    let merged = (mapped |> List.filter (fun (codepoint, _) -> not (List.contains codepoint forms))) @ added |> List.sortBy fst
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "synthetic")
    Directory.CreateDirectory directory |> ignore
    let target = Path.Combine(directory, name)
    File.WriteAllBytes(target, withCmap bytes (cmapOf merged))
    { source with Path = target; Index = 0 }

/// <summary>The Hebrew shaping differential: Farfbuzz against HarfBuzz on random Hebrew text, on every corpus face that
/// maps the Hebrew letters and on the same face stripped of its layout tables.</summary>
/// <returns>The tests.</returns>
let hebrewTests () : Test =
    testList "P6.S14 hebrew shape" (sampledOver "hebrew" hebrewFaces.Value hebrewOptions hebrewAlphabet 8)

/// <summary>The Thai and Lao shaping differential: Farfbuzz against HarfBuzz on random Thai and Lao text, on every corpus
/// face that maps the script, on the same face stripped of its layout tables, and on copies of a stripped Thai face whose
/// cmap also maps the Windows PUA forms, the Mac forms and both, so the PUA shaping fires.</summary>
/// <returns>The tests.</returns>
let thaiTests () : Test =
    let thai = thaiFaces.Value
    let lao = laoFaces.Value
    let plainSfnt (font: TestFont) = u32 (File.ReadAllBytes font.Path) 0 <> uint32 Tags.Collection && font.Index = 0
    let puaSource = thai |> List.find plainSfnt
    let layoutSource = puaSource
    let strippedSource = NoLayoutFonts.strip puaSource
    let pua =
        [ "windows", windowsForms; "mac", macForms; "both", windowsForms @ macForms ]
        |> List.collect (fun (kind, forms) ->
            [ let stripped = withPuaForms strippedSource $"thai-pua-{kind}-stripped.ttf" forms
              yield ShapeDiff.against $"thai pua {kind} stripped" stripped.Path thaiOptions thaiAlphabet 10 6000
              let layout = withPuaForms layoutSource $"thai-pua-{kind}.ttf" forms
              yield ShapeDiff.against $"thai pua {kind} with layout" layout.Path thaiOptions thaiAlphabet 10 3000
              yield ShapeDiff.against $"lao over pua {kind} stripped" stripped.Path laoOptions laoAlphabet 10 1500 ])
    testList "P6.S15 thai and lao shape" (
        sampledOver "thai" thai thaiOptions thaiAlphabet 10
        @ sampledOver "lao" lao laoOptions laoAlphabet 10
        @ pua)

/// <summary>The font and option pairs of the plan differential.</summary>
/// <param name="faces">The faces.</param>
/// <param name="scripts">The script options.</param>
/// <returns>The pairs.</returns>
let private planCases (faces: TestFont list) (scripts: string list) : (string * string) list =
    [ for font in faces do
        for script in scripts do
            for direction in [ " --direction=ltr"; " --direction=rtl"; " --direction=ttb"; " --direction=btt" ] do
                yield font.Path, $"{script}{direction}"
        let stripped = NoLayoutFonts.strip font
        yield stripped.Path, $"{scripts.Head} --direction=rtl" ]

/// <summary>The plan differential: the feature tags and the GSUB and GPOS lookup sets Farfbuzz and HarfBuzz compile for
/// the Hebrew, Thai and Lao faces, in each direction.</summary>
/// <returns>The tests.</returns>
let planTests () : Test =
    testList "P6.S14 P6.S15 plan" [
        PlanDiff.featuresAgainst "hebrew plan" (planCases hebrewFaces.Value [ "--script=Hebr"; "--script=Hebr --language=he"; "--script=Hebr --language=yi" ])
        PlanDiff.featuresAgainst "thai plan" (planCases thaiFaces.Value [ "--script=Thai"; "--script=Thai --language=th" ])
        PlanDiff.featuresAgainst "lao plan" (planCases laoFaces.Value [ "--script=Laoo"; "--script=Laoo --language=lo" ])
    ]
