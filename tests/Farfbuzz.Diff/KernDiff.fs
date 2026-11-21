module Farfbuzz.Diff.KernDiff

open System
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>What the kern comparison looks at: glyph ids, clusters, positions and the defined flags.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparable (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) : Face =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>The kern table tag.</summary>
let private kernTag = Tag.make 'k' 'e' 'r' 'n'

/// <summary>Whether a face's plan applies the legacy kern table.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>True when the legacy kern applies.</returns>
let private appliesKern (font: TestFont) (index: uint32) : bool =
    let kern = KernTable.load (faceOf font index)
    match KernTable.hasData kern with
    | false -> false
    | true when KernTable.hasStateMachine kern -> false
    | true ->
        let face = faceOf font index
        let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
        (OtPlan.create (Font.create face) props ImmutableArray.Empty).ApplyKern

/// <summary>Every face whose plan applies the legacy kern table.</summary>
/// <returns>The faces.</returns>
let private kernedFonts () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) -> appliesKern font index)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Whether a code point is a kernable default-shaper character.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True when the default shaper shapes it without marks or fallbacks.</returns>
let private kernable (codepoint: uint32) : bool =
    let script = Unicode.script codepoint
    let category = Unicode.generalCategory codepoint
    let isMark =
        match category with
        | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark | GeneralCategory.NonSpacingMark -> true
        | _ -> false
    let decomposesWithMark =
        match Unicode.decompose codepoint with
        | ValueSome (struct (_, second)) -> second <> 0u
        | ValueNone -> false
    Categorize.shaper script Direction.Ltr Tags.Empty = DefaultShaper
    && not isMark
    && not (Unicode.isDefaultIgnorable codepoint)
    && not decomposesWithMark

/// <summary>The kernable code points a face maps.</summary>
/// <param name="font">The face.</param>
/// <returns>The code points, ascending.</returns>
let private kernCoverage (font: TestFont) : uint32 array =
    TestFonts.coverage font |> Array.filter kernable

/// <summary>Generates a kerned shaping request: random text, a direction and a kern feature setting.</summary>
/// <param name="fonts">The kerned faces.</param>
/// <returns>The request.</returns>
let private kernRequest (fonts: TestFont array) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements fonts
        let coverage = kernCoverage font
        let! length = Gen.choose (1, 16)
        let! codepoints = Gen.arrayOfLength length (Gen.elements coverage)
        let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
        let! features = Gen.elements [| ValueSome "kern=1"; ValueSome "kern=0"; ValueSome "kern[2:3]=0" |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = direction
                Features = features }
    }

/// <summary>Reads the kern table offset and length of a file, or none when absent.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The offset and length.</returns>
let private kernOffset (bytes: byte array) : struct (int * int) voption =
    let face = Face.create (ReadOnlyMemory bytes) 0u
    match Face.tryFindTable face kernTag with
    | ValueNone -> ValueNone
    | ValueSome record -> ValueSome (struct (int (face.BaseOffset + record.Offset), int record.Length))

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="value">The value.</param>
let private writeU16 (bytes: byte array) (at: int) (value: uint32) =
    bytes[at] <- byte (value >>> 8)
    bytes[at + 1] <- byte value

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="value">The value.</param>
let private writeU32 (bytes: byte array) (at: int) (value: uint32) =
    bytes[at] <- byte (value >>> 24)
    bytes[at + 1] <- byte (value >>> 16)
    bytes[at + 2] <- byte (value >>> 8)
    bytes[at + 3] <- byte value

/// <summary>Enumerates the fields of a kern table that the mutation differential rewrites.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="baseOffset">The kern table offset.</param>
/// <returns>The name, absolute offset, width and sample values of each field.</returns>
let private kernFields (bytes: byte array) (baseOffset: int) : (string * int * int * uint32 array) array =
    let readU16 at = (uint32 bytes[at] <<< 8) ||| uint32 bytes[at + 1]
    let readU32 at = (uint32 bytes[at] <<< 24) ||| (uint32 bytes[at + 1] <<< 16) ||| (uint32 bytes[at + 2] <<< 8) ||| uint32 bytes[at + 3]
    let isApple = readU16 baseOffset = 1u
    let tableHeaderSize = if isApple then 8 else 4
    let subtableHeaderSize = if isApple then 8 else 6
    let count = if isApple then readU32 (baseOffset + 4) else uint32 (readU16 (baseOffset + 2))
    let fields = ResizeArray<string * int * int * uint32 array>()
    if isApple then
        fields.Add("version", baseOffset, 4, [| 0u; 1u; 0x00010000u; 0xFFFFFFFFu |])
        fields.Add("table count", baseOffset + 4, 4, [| 0u; 1u; 0xFFFFFFFFu |])
    else
        fields.Add("version", baseOffset, 2, [| 0u; 1u; 2u |])
        fields.Add("table count", baseOffset + 2, 2, [| 0u; 1u; 0xFFFFu |])
    let mutable at = baseOffset + tableHeaderSize
    let mutable index = 0
    let mutable remaining = int (min count 1024u)
    while index < remaining do
        let length = if isApple then int (readU32 at) else int (readU16 (at + 2))
        let formatAt = at + (if isApple then 5 else 4)
        let coverageAt = at + (if isApple then 4 else 5)
        let lengthAt = at + (if isApple then 0 else 2)
        let lengthWidth = if isApple then 4 else 2
        match length < subtableHeaderSize with
        | true -> remaining <- 0
        | false ->
            let format = int bytes[formatAt]
            fields.Add($"subtable {index} length", lengthAt, lengthWidth, [| 0u; 1u; 0xFFFFu |])
            fields.Add($"subtable {index} format", formatAt, 1, [| 0u; 1u; 2u; 3u; 0xFFu |])
            fields.Add($"subtable {index} coverage", coverageAt, 1, [| 0u; 0x05u; 0xFFu |])
            match format with
            | 0 ->
                let pairCountAt = at + subtableHeaderSize
                let pairsAt = at + subtableHeaderSize + 8
                fields.Add($"subtable {index} pair count", pairCountAt, 2, [| 0u; 0xFFFFu |])
                fields.Add($"subtable {index} pair key", pairsAt, 2, [| 0xFFFFu |])
            | 2 ->
                fields.Add($"subtable {index} left class offset", at + subtableHeaderSize + 2, 2, [| 0u; 0xFFFFu |])
                fields.Add($"subtable {index} right class offset", at + subtableHeaderSize + 4, 2, [| 0u; 0xFFFFu |])
                fields.Add($"subtable {index} array offset", at + subtableHeaderSize + 6, 2, [| 0u; 0xFFFFu |])
            | 3 ->
                fields.Add($"subtable {index} glyph count", at + subtableHeaderSize, 2, [| 0u; 0xFFFFu |])
                fields.Add($"subtable {index} value count", at + subtableHeaderSize + 2, 1, [| 0u; 0xFFu |])
                fields.Add($"subtable {index} left class count", at + subtableHeaderSize + 3, 1, [| 0u; 0xFFu |])
                fields.Add($"subtable {index} right class count", at + subtableHeaderSize + 4, 1, [| 0u; 0xFFu |])
            | _ -> ()
            at <- at + length
            index <- index + 1
    fields.ToArray()

/// <summary>Shapes one mutated file and compares the two engines.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <param name="path">The mutated file path.</param>
/// <param name="index">The face index.</param>
/// <param name="text">The text.</param>
/// <returns>The mismatch description, or none.</returns>
let private compareShape (oracle: Shaper) (farfbuzz: Shaper) (path: string) (index: int) (text: uint32 array) : string voption =
    let request =
        { defaults path text with
            FaceIndex = index
            Shaper = ValueSome "ot"
            FontFuncs = ValueSome "ot" }
    match comparable (farfbuzz.Shape request), comparable (oracle.Shape request) with
    | ours, oracleResult when ours = oracleResult -> ValueNone
    | ours, oracleResult -> ValueSome $"farfbuzz %A{ours}, oracle %A{oracleResult}"

/// <summary>The P5.S13 kern differentials.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    let fonts = kernedFonts ()
    let mutatedDir = Path.Combine(RepositoryPaths.root (), "artifacts", "kern-mutation")
    testList "P5.S13" [
        testList "has kerning" [
            exhaustive "has kerning" (OracleFaces.all ())
                (fun struct (font, index) ->
                    let kern = KernTable.load (faceOf font index)
                    struct (KernTable.hasData kern, KernTable.hasStateMachine kern, KernTable.hasCrossStream kern))
                (fun struct (font, index) ->
                    let face = OracleFaces.face font index
                    struct (Shims.farfbuzz_oracle_has_kerning face <> 0, Shims.farfbuzz_oracle_has_machine_kerning face <> 0, Shims.farfbuzz_oracle_has_cross_kerning face <> 0))
        ]
        testList "kerned shaping" [
            sampled "P5.S13 kerned shaping"
                (kernRequest fonts)
                5000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
        ]
        testCase "P5.S13 mutated kern tables" (fun () ->
            Directory.CreateDirectory mutatedDir |> ignore
            let failures = ResizeArray<string>()
            let mutable cases = 0
            let mutable fileCounter = 0
            let mutable state = 12345u
            let nextRandom () =
                state <- state * 1103515245u + 12345u
                state
            let fontFields =
                fonts
                |> Array.choose (fun font ->
                    let bytes = OracleFaces.data font.Path
                    match kernOffset bytes with
                    | ValueNone -> None
                    | ValueSome (struct (baseOffset, _)) -> Some (font, kernFields bytes baseOffset))
            let mutable attempt = 0
            while cases < 5000 && attempt < 200000 do
                attempt <- attempt + 1
                let font, fields = fontFields[int (nextRandom () % uint32 fontFields.Length)]
                let name, fieldOffset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                let coverage = kernCoverage font
                let text =
                    match coverage.Length with
                    | 0 -> [| 0x002Eu |]
                    | _ ->
                        let length = int (nextRandom () % 4u) + 1
                        Array.init length (fun _ -> coverage[int (nextRandom () % uint32 coverage.Length)])
                let bytes = OracleFaces.data font.Path
                for value in values do
                    if cases < 5000 then
                        let candidate = Array.copy bytes
                        match width with
                        | 4 -> writeU32 candidate fieldOffset value
                        | 2 -> writeU16 candidate fieldOffset value
                        | _ -> candidate[fieldOffset] <- byte value
                        let path = Path.Combine(mutatedDir, $"mutated-{fileCounter}.ttf")
                        fileCounter <- fileCounter + 1
                        File.WriteAllBytes(path, candidate)
                        match compareShape oracle farfbuzz path font.Index text with
                        | ValueNone -> ()
                        | ValueSome description -> failures.Add $"{font.Suite}/{Path.GetFileName font.Path} face {font.Index}: {name} ({value}): {description}"
                        cases <- cases + 1
            for font in fonts do
                let bytes = OracleFaces.data font.Path
                match kernOffset bytes with
                | ValueNone -> ()
                | ValueSome (struct (kernAt, kernLength)) ->
                    let coverage = kernCoverage font
                    let text = Array.init 2 (fun _ -> coverage[int (nextRandom () % uint32 coverage.Length)])
                    for cutBy in [| 1; 2; 3 |] do
                        let cut = Array.truncate (kernAt + kernLength - cutBy) bytes
                        let path = Path.Combine(mutatedDir, $"mutated-{fileCounter}.ttf")
                        fileCounter <- fileCounter + 1
                        File.WriteAllBytes(path, cut)
                        match compareShape oracle farfbuzz path font.Index text with
                        | ValueNone -> ()
                        | ValueSome description -> failures.Add $"{font.Suite}/{Path.GetFileName font.Path} face {font.Index}: truncated tail: {description}"
                        cases <- cases + 1
            printfn "P5.S13 compared %d mutated kern tables" cases
            Expect.isEmpty failures ("mutated kern differences:\n" + String.concat "\n" failures))
    ]
