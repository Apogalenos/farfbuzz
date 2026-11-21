module Farfbuzz.Diff.GposAttachDiff

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Oracle.NativeText
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.GposDiff
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>The GPOS table of a face, read through Farfbuzz.</summary>
/// <param name="font">The test font.</param>
/// <returns>The table.</returns>
let internal gposOf (font: TestFont) =
    (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gpos.Value

/// <summary>Whether a face has a subtable of one of the attachment types.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when GPOS holds one.</returns>
let private hasAttachment (font: TestFont) =
    (gposOf font).Lookups
    |> Seq.exists (fun lookup -> lookup.Value.Subtables |> Seq.exists (fun accel -> GposAttach.isAttachment accel.Subtable))

/// <summary>Whether a face's GPOS holds a subtable that is not ported and so never applies: one of a format its type does not
/// have, which no corpus face has.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when GPOS has one.</returns>
let hasPendingPositioning (font: TestFont) =
    (gposOf font).Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | PosPending _ -> true
            | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosContext _ | PosChainContext _ -> false))

/// <summary>Whether a face's GSUB still holds a subtable of a format that is not ported.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when GSUB has one.</returns>
let hasPendingSubstitution (font: TestFont) =
    let table = (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gsub.Value
    table.Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | SubstPending _ -> true
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ -> false))

/// <summary>The lookup types of a face's GPOS that are attachment types, counted by type.</summary>
/// <param name="font">The test font.</param>
/// <returns>The counts of cursive, mark-to-base, mark-to-ligature and mark-to-mark subtables.</returns>
let private attachmentCounts (font: TestFont) =
    let subtables = (gposOf font).Lookups |> Seq.collect (fun lookup -> lookup.Value.Subtables |> Seq.map (fun accel -> accel.Subtable))
    let count predicate = subtables |> Seq.filter predicate |> Seq.length
    struct (count (fun s -> match s with | CursivePos _ -> true | _ -> false),
            count (fun s -> match s with | MarkBasePos _ -> true | _ -> false),
            count (fun s -> match s with | MarkLigPos _ -> true | _ -> false),
            count (fun s -> match s with | MarkMarkPos _ -> true | _ -> false))

/// <summary>The faces of the corpus and of the unstripped Latin faces of the API tests that hold an attachment subtable.</summary>
let private attachCandidates =
    lazy (
        let extra =
            TestFonts.extra [ "api/fonts/OpenSans-Regular.ttf"; "api/fonts/NotoSans-Bold.ttf"; "api/fonts/Inconsolata-Regular.abc.ttf"; "api/fonts/Mplus1p-Regular.ttf"; "api/fonts/Cantarell.A.otf"; "api/fonts/notosansitalic.ttf" ]
            |> Seq.toList
        TestFonts.all () @ extra
        |> List.distinctBy (fun font -> font.Path, font.Index)
        |> List.filter hasAttachment
        |> List.toArray)

/// <summary>The candidates whose GSUB subtables are all ported and that have no context or chaining context subtable in GPOS.</summary>
let private attachFonts =
    lazy (attachCandidates.Value |> Array.filter (fun font -> not (hasPendingSubstitution font) && not (hasPendingPositioning font)))

/// <summary>Whether a code point is a mark, a non-spacing, spacing or enclosing one.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for a mark.</returns>
let isMark (codepoint: uint32) =
    match Unicode.generalCategory codepoint with
    | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark | GeneralCategory.NonSpacingMark -> true
    | _ -> false

/// <summary>Whether a code point is one the default shaper takes to its glyph, marks included: no default ignorable, and no
/// decomposition into two code points, which the normalisation would reorder.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for such a code point.</returns>
let usable (codepoint: uint32) =
    let decomposes =
        match Unicode.decompose codepoint with
        | ValueSome (struct (_, second)) -> second <> 0u
        | ValueNone -> false
    Categorize.shaper (Unicode.script codepoint) Direction.Ltr Tags.Empty = DefaultShaper
    && not (Unicode.isDefaultIgnorable codepoint)
    && not decomposes

let private alphabets = ConcurrentDictionary<string, struct (uint32 array * uint32 array)>()
let private probes = ConcurrentDictionary<string, uint32 array array>()

/// <summary>The bases and the marks a face maps among the code points the default shaper takes straight to glyphs.</summary>
/// <param name="font">The face.</param>
/// <returns>The bases and the marks.</returns>
let alphabetOf (font: TestFont) =
    alphabets.GetOrAdd($"{font.Path}#{font.Index}", fun _ ->
        let usableCodepoints = TestFonts.coverage font |> Array.filter usable
        struct (usableCodepoints |> Array.filter (isMark >> not), usableCodepoints |> Array.filter isMark))

/// <summary>Generates a text of a face: groups of a base and some marks, so that marks follow bases, with now and then a
/// run of marks alone or a base alone; the aots faces, whose marks are glyph classes, not code points, get random code points.</summary>
/// <param name="font">The face.</param>
/// <param name="maxLength">The longest text.</param>
/// <returns>The generator of the text.</returns>
let textOf (font: TestFont) (maxLength: int) =
    let struct (bases, marks) = alphabetOf font
    let everything = Array.append bases marks
    gen {
        match marks.Length > 0 && bases.Length > 0 && not (font.Path.Contains "aots") with
        | false ->
            let! length = Gen.choose (1, maxLength)
            return! Gen.arrayOfLength length (Gen.elements everything)
        | true ->
            let! groups = Gen.choose (1, max 1 (maxLength / 3))
            let! pieces =
                Gen.arrayOfLength groups (gen {
                    let! first = Gen.frequency [ 8, Gen.elements bases; 1, Gen.elements marks ]
                    let! count = Gen.frequency [ 2, Gen.constant 0; 4, Gen.constant 1; 3, Gen.constant 2; 1, Gen.constant 3 ]
                    let! following = Gen.arrayOfLength count (Gen.elements marks)
                    return Array.append [| first |] following })
            return Array.concat pieces
    }

/// <summary>The settings of an attachment differential: a direction, buffer flags, a cluster level, a ppem and a feature.</summary>
/// <param name="isAots">Whether the face is one of the aots faces, whose lookups hang on the test feature.</param>
/// <returns>The generator of the settings.</returns>
let settings (isAots: bool) =
    gen {
        let! direction = Gen.frequency [ 5, Gen.constant "ltr"; 2, Gen.constant "rtl"; 1, Gen.constant "ttb"; 1, Gen.constant "btt" ]
        let! flags = Gen.elements [| 0u; 0u; 64u; 64u; 64u ||| 128u; 2u; 1u; 8u |]
        let! level = Gen.elements [| 0; 0; 1; 2; 3 |]
        let! ppem = Gen.frequency [ 3, Gen.constant 0u; 1, Gen.choose (6, 40) |> Gen.map uint32 ]
        let! features =
            match isAots with
            | true -> Gen.elements [| "test"; "test"; "test"; "test=0"; "-test"; "test[1:3]=0"; "" |]
            | false -> Gen.elements [| ""; ""; ""; "-mark"; "-mkmk"; "-kern"; "-curs"; "mark=0"; "mkmk[1:3]=0" |]
        return struct (direction, flags, level, ppem, features)
    }

/// <summary>Generates a request on one of the faces.</summary>
/// <param name="fonts">The faces.</param>
/// <param name="maxLength">The longest text.</param>
/// <returns>The generator of the request.</returns>
let request (fonts: TestFont array) (maxLength: int) =
    gen {
        let! font = Gen.elements fonts
        let! text = textOf font maxLength
        let! struct (direction, flags, level, ppem, features) = settings (font.Path.Contains "aots")
        return
            { Path = font.Path
              Cached = true
              Index = font.Index
              Text = text
              Features = features
              Direction = direction
              Flags = flags
              ClusterLevel = level
              Ppem = ppem }
    }

let private compared = ref 0
let private attached = ref 0

/// <summary>Whether a shaped run has a glyph displaced from its pen position, which a mark or cursive attachment, or a
/// single adjustment, does; the attachment lookups of these faces are the only ones that move a glyph across the line.</summary>
/// <param name="shaped">The shaped glyphs.</param>
/// <returns>True when a glyph has an offset.</returns>
let displaced (shaped: Shaped) =
    shaped |> Array.exists (fun (struct (_, _, _, _, xOffset, yOffset, _)) -> xOffset <> 0 || yOffset <> 0)

/// <summary>Compares the two engines on a request and counts it.</summary>
/// <param name="request">The request.</param>
/// <returns>The two answers.</returns>
let private shapeBoth (request: GposRequest) =
    let ours = ourShape request
    let expected = oracleShape request
    lock compared (fun () ->
        compared.Value <- compared.Value + 1
        match expected with
        | Ok shaped when displaced shaped -> attached.Value <- attached.Value + 1
        | _ -> ())
    struct (ours, expected)

/// <summary>Compares the two engines on generated requests, shaping each once.</summary>
/// <param name="name">The test name.</param>
/// <param name="inputs">The request generator.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let private versus (name: string) (inputs: Gen<GposRequest>) (count: int) =
    testPropertyWithConfig (config count) name (Prop.forAll (Arb.fromGen inputs) (fun input ->
        let struct (ours, expected) = shapeBoth input
        match ours = expected with
        | true -> Prop.ofTestable true
        | false -> Prop.ofTestable false |> Prop.label $"input: %A{input}\n  oracle:   %A{expected}\n  farfbuzz: %A{ours}"))

let private gposTag = Tag.make 'G' 'P' 'O' 'S'

/// <summary>Reads a big-endian value of a width.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="width">Two or four bytes.</param>
/// <returns>The value, zero beyond the bytes.</returns>
let readAt (bytes: byte array) (at: int) (width: int) =
    match at >= 0 && at + width <= bytes.Length with
    | false -> 0
    | true -> Seq.fold (fun acc index -> (acc <<< 8) ||| int bytes[at + index]) 0 (seq { 0 .. width - 1 })

/// <summary>Writes a big-endian value of a width into the bytes.</summary>
/// <param name="bytes">The bytes, changed.</param>
/// <param name="at">The offset.</param>
/// <param name="width">Two or four bytes.</param>
/// <param name="value">The value.</param>
let writeAt (bytes: byte array) (at: int) (width: int) (value: int) =
    for index in 0 .. width - 1 do
        bytes[at + index] <- byte (value >>> (8 * (width - 1 - index)))

/// <summary>Where the GPOS record of a font file's directory is, and where its table starts; none for a collection or a file
/// without GPOS.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The directory record's offset and the table's offset.</returns>
let gposRecord (bytes: byte array) =
    match readAt bytes 0 4 = 0x74746366 with
    | true -> ValueNone
    | false ->
        let count = readAt bytes 4 2
        Seq.tryFind (fun index -> readAt bytes (12 + 16 * index) 4 = int (uint32 gposTag)) (seq { 0 .. count - 1 })
        |> function
            | Some index -> ValueSome (struct (12 + 16 * index, readAt bytes (12 + 16 * index + 8) 4))
            | None -> ValueNone

/// <summary>The fields of the cursive, mark-to-base, mark-to-ligature and mark-to-mark subtables of a font file that the
/// mutation differential rewrites: the name, absolute offset, width and sample values of each, down to the first anchor of
/// each array and matrix, an extension's inner subtable included.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The fields.</returns>
let private attachFields (bytes: byte array) =
    match gposRecord bytes with
    | ValueNone -> [||]
    | ValueSome (struct (_, table)) ->
        let offsets = [| 0; 1; 2; 4; 0xFFFF |]
        let counts = [| 0; 1; 2; 0x7FFF; 0xFFFF |]
        let formats = [| 0; 1; 2; 3; 4; 0xFFFF |]
        let coordinates = [| 0; 1; 0x7FFF; 0x8000; 0xFFFF |]
        let anchorFields (tag: string) (anchor: int) =
            [ $"{tag} anchor format", anchor, 2, formats
              $"{tag} anchor x", anchor + 2, 2, coordinates
              $"{tag} anchor y", anchor + 4, 2, coordinates
              $"{tag} anchor device offset", anchor + 6, 2, offsets ]
        let coverageFields (tag: string) (coverage: int) =
            [ $"{tag} format", coverage, 2, formats
              $"{tag} count", coverage + 2, 2, counts
              $"{tag} first glyph", coverage + 4, 2, [| 0; 1; 2; 0xFFFF |] ]
        let lookupList = table + readAt bytes (table + 8) 2
        let lookups = min 64 (readAt bytes lookupList 2)
        [| for lookupIndex in 0 .. lookups - 1 do
            let lookup = lookupList + readAt bytes (lookupList + 2 + lookupIndex * 2) 2
            let lookupType = readAt bytes lookup 2
            yield $"lookup {lookupIndex} flag", lookup + 2, 2, [| 0; 1; 2; 4; 8; 0x10; 0xFF00 |]
            for subtableIndex in 0 .. min 3 (readAt bytes (lookup + 4) 2) - 1 do
                let outer = lookup + readAt bytes (lookup + 6 + subtableIndex * 2) 2
                let kind, subtable =
                    match lookupType with
                    | 9 -> readAt bytes (outer + 2) 2, outer + readAt bytes (outer + 4) 4
                    | _ -> lookupType, outer
                let tag = $"lookup {lookupIndex} subtable {subtableIndex} type {kind}"
                if lookupType = 9 && kind >= 3 && kind <= 6 then
                    yield $"{tag} extension type", outer + 2, 2, [| 0; 1; 2; 9; 0xFFFF |]
                    yield $"{tag} extension offset", outer + 4, 4, [| 0; 1; 8; 0xFFFF; 0x7FFFFFFF; 0xFFFFFFFF |]
                if kind >= 3 && kind <= 6 then
                    yield $"{tag} format", subtable, 2, [| 0; 1; 2; 3; 0xFFFF |]
                    yield $"{tag} coverage offset", subtable + 2, 2, offsets
                    yield! coverageFields $"{tag} coverage" (subtable + readAt bytes (subtable + 2) 2)
                    match kind with
                    | 3 ->
                        yield $"{tag} record count", subtable + 4, 2, counts
                        yield $"{tag} first entry offset", subtable + 6, 2, offsets
                        yield $"{tag} first exit offset", subtable + 8, 2, offsets
                        yield! anchorFields $"{tag} entry" (subtable + readAt bytes (subtable + 6) 2)
                        yield! anchorFields $"{tag} exit" (subtable + readAt bytes (subtable + 8) 2)
                    | _ ->
                        yield $"{tag} second coverage offset", subtable + 4, 2, offsets
                        yield! coverageFields $"{tag} second coverage" (subtable + readAt bytes (subtable + 4) 2)
                        yield $"{tag} class count", subtable + 6, 2, [| 0; 1; 2; 3; 0xFFFF |]
                        yield $"{tag} mark array offset", subtable + 8, 2, offsets
                        yield $"{tag} second array offset", subtable + 10, 2, offsets
                        let markArray = subtable + readAt bytes (subtable + 8) 2
                        yield $"{tag} mark count", markArray, 2, counts
                        yield $"{tag} first mark class", markArray + 2, 2, [| 0; 1; 2; 0xFFFF |]
                        yield $"{tag} first mark anchor offset", markArray + 4, 2, offsets
                        yield! anchorFields $"{tag} first mark" (markArray + readAt bytes (markArray + 4) 2)
                        let second = subtable + readAt bytes (subtable + 10) 2
                        let matrix = match kind with | 5 -> second + readAt bytes (second + 2) 2 | _ -> second
                        if kind = 5 then
                            yield $"{tag} ligature count", second, 2, counts
                            yield $"{tag} first ligature offset", second + 2, 2, offsets
                        yield $"{tag} row count", matrix, 2, counts
                        yield $"{tag} first matrix anchor offset", matrix + 2, 2, offsets
                        yield $"{tag} second matrix anchor offset", matrix + 4, 2, offsets
                        yield! anchorFields $"{tag} first matrix" (matrix + readAt bytes (matrix + 2) 2) |]

/// <summary>The number of lookups a face's GPOS holds, which HarfBuzz reads as zero when it rejects the table.</summary>
/// <param name="path">The font file.</param>
/// <returns>HarfBuzz's count and Farfbuzz's.</returns>
let lookupCounts (path: string) =
    use blob = HarfBuzzSharp.Blob.FromFile path
    use face = new HarfBuzzSharp.Face(blob, 0)
    let oracle = int (Native.hb_ot_layout_table_get_lookup_count(face.Handle, uint32 gposTag))
    let ours = LayoutTable.lookupCount (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u |> OtFace.create).Gpos.Value
    struct (oracle, ours)

/// <summary>The P5.S11 differentials: GPOS anchors, cursive and mark attachment against HarfBuzz.</summary>
/// <returns>The test list.</returns>
let tests () =
    let fonts = attachFonts.Value |> Array.filter (fun font -> (alphabetOf font |> fun (struct (bases, marks)) -> bases.Length + marks.Length) > 0)
    let aotsOf (marker: string) = fonts |> Array.filter (fun font -> font.Path.Contains marker)
    let aots = fonts |> Array.filter (fun font -> font.Path.Contains "aots")
    let real = fonts |> Array.filter (fun font -> not (font.Path.Contains "aots"))
    let sources = fonts |> Array.filter (fun font -> ValueOption.isSome (gposRecord (OracleFaces.data font.Path)))
    let probeTexts (font: TestFont) = probes.GetOrAdd($"{font.Path}#{font.Index}", fun _ -> Gen.sampleWithSeed (Rnd 77UL) 20 40 (textOf font 12))
    testSequenced <| testList "P5.S11" [
        testCase "P5.S11 the corpus has attachment faces" (fun () ->
            let candidates = attachCandidates.Value
            let pendingSubstitution = candidates |> Array.filter hasPendingSubstitution
            let pendingPositioning = candidates |> Array.filter hasPendingPositioning
            let noText = attachFonts.Value |> Array.filter (fun font -> (alphabetOf font |> fun (struct (bases, marks)) -> bases.Length + marks.Length) = 0)
            let complexOnly =
                fonts
                |> Array.filter (fun font ->
                    let struct (bases, marks) = alphabetOf font
                    let all = TestFonts.coverage font
                    bases.Length + marks.Length < all.Length / 4)
            printfn "P5.S11 faces: %d with an attachment subtable; %d left out for a GSUB subtable not ported, %d for a GPOS context lookup, %d for no code point the default shaper takes; %d used, of which %d have under a quarter of their code points taken by the default shaper (complex scripts used through their ASCII or default-shaper characters)" candidates.Length pendingSubstitution.Length pendingPositioning.Length noText.Length fonts.Length complexOnly.Length
            for font in fonts do
                let struct (cursive, markBase, markLig, markMark) = attachmentCounts font
                let struct (bases, marks) = alphabetOf font
                printfn "P5.S11 face %s#%d: cursive %d, mark-to-base %d, mark-to-ligature %d, mark-to-mark %d; %d base and %d mark code points" (Path.GetFileName font.Path) font.Index cursive markBase markLig markMark bases.Length marks.Length
            Expect.isGreaterThan aots.Length 0 "aots gpos3 to gpos6 faces"
            Expect.isGreaterThan real.Length 0 "real faces with attachment")
        versus "P5.S11 aots gpos3 cursive shaping" (request (aotsOf "gpos3_") 12) 3000
        versus "P5.S11 aots gpos4 mark-to-base shaping" (request (aotsOf "gpos4_") 12) 3000
        versus "P5.S11 aots gpos5 mark-to-ligature shaping" (request (aotsOf "gpos5") 12) 2000
        versus "P5.S11 aots gpos6 mark-to-mark shaping" (request (aotsOf "gpos6") 12) 2000
        versus "P5.S11 real faces, base and mark text" (request real 30) 8000
        testCase "P5.S11 the unchanged faces are accepted and attach glyphs" (fun () ->
            let mutable accepted = 0
            let mutable attaching = 0
            for font in sources do
                let struct (oracleCount, ourCount) = lookupCounts font.Path
                Expect.equal ourCount oracleCount $"{Path.GetFileName font.Path}: the same lookup count"
                Expect.isGreaterThan oracleCount 0 $"{Path.GetFileName font.Path}: accepted"
                accepted <- accepted + 1
                let attached = Seq.exists (fun text -> match oracleShape { Path = font.Path; Cached = true; Index = font.Index; Text = text; Features = (match font.Path.Contains "aots" with | true -> "test" | false -> ""); Direction = "ltr"; Flags = 0u; ClusterLevel = 0; Ppem = 0u } with | Ok shaped -> displaced shaped | Error _ -> false) (probeTexts font)
                if attached then attaching <- attaching + 1
            printfn "P5.S11 unchanged faces: %d accepted by both, %d of them attach a glyph on some probe text" accepted attaching
            Expect.isGreaterThan attaching 20 "many unchanged faces attach glyphs")
        testCase "P5.S11 mutated attachment subtables" (fun () ->
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-attach-mutation")
            Directory.CreateDirectory directory |> ignore
            let candidates = sources |> Array.map (fun font -> font, attachFields (OracleFaces.data font.Path)) |> Array.filter (fun (_, fields) -> fields.Length > 0)
            let failures = ResizeArray<string>()
            let mutable state = 20261114u
            let nextRandom () =
                state <- state * 1103515245u + 12345u
                state >>> 8
            let mutable cases = 0
            let mutable rejected = 0
            let mutable rejectedByUs = 0
            let mutable attachedCases = 0
            let mutable truncated = 0
            let mutable attempt = 0
            while cases < 6000 && attempt < 100000 do
                attempt <- attempt + 1
                let font, fields = candidates[int (nextRandom () % uint32 candidates.Length)]
                let candidate = Array.copy (OracleFaces.data font.Path)
                let name =
                    match nextRandom () % 24u, gposRecord candidate with
                    | 0u, ValueSome (struct (record, _)) ->
                        let length = readAt candidate (record + 12) 4
                        let cut = 1 + int (nextRandom () % 8u)
                        writeAt candidate (record + 12) 4 (max 0 (length - cut))
                        truncated <- truncated + 1
                        $"table length minus {cut}"
                    | _ ->
                        let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                        let value = values[int (nextRandom () % uint32 values.Length)]
                        writeAt candidate offset width value
                        $"{name} = {value}"
                let path = Path.Combine(directory, $"mutated-{cases}.ttf")
                File.WriteAllBytes(path, candidate)
                let text = probeTexts font |> fun texts -> texts[int (nextRandom () % uint32 texts.Length)]
                let features = match font.Path.Contains "aots" with | true -> "test" | false -> ""
                let request = { Path = path; Cached = false; Index = font.Index; Text = text; Features = features; Direction = (match nextRandom () % 5u with | 0u -> "rtl" | _ -> "ltr"); Flags = (match nextRandom () % 3u with | 0u -> 64u | _ -> 0u); ClusterLevel = int (nextRandom () % 4u); Ppem = (match nextRandom () % 4u with | 0u -> 12u | _ -> 0u) }
                let ours = ourShape request
                let expected = oracleShape request
                let struct (oracleCount, ourCount) = lookupCounts path
                if oracleCount = 0 then rejected <- rejected + 1
                if ourCount = 0 then rejectedByUs <- rejectedByUs + 1
                match expected with
                | Ok shaped when oracleCount > 0 && displaced shaped -> attachedCases <- attachedCases + 1
                | _ -> ()
                if oracleCount <> ourCount then failures.Add $"{Path.GetFileName font.Path}: {name}: lookup count oracle {oracleCount}, farfbuzz {ourCount}"
                elif ours <> expected then failures.Add $"{Path.GetFileName font.Path}: {name}: shaped differently: farfbuzz %A{ours}, oracle %A{expected}"
                File.Delete path
                cases <- cases + 1
            printfn "P5.S11 compared %d changed-bytes cases (%d with the GPOS length cut): HarfBuzz rejected the table in %d, Farfbuzz in %d, the same ones; %d accepted cases attach a glyph" cases truncated rejected rejectedByUs attachedCases
            Expect.isGreaterThanOrEqual cases 5000 "the number of cases"
            Expect.isGreaterThan rejected 0 "some changes make HarfBuzz reject the table"
            Expect.equal rejectedByUs rejected "Farfbuzz rejects as many tables as HarfBuzz"
            Expect.isGreaterThan attachedCases 500 "many accepted changed tables still attach glyphs"
            Expect.isEmpty (failures |> Seq.truncate 20 |> Seq.toList) ("changed-bytes differences:\n" + String.concat "\n" (failures |> Seq.truncate 20)))
        testCase "P5.S11 report" (fun () ->
            printfn "P5.S11 shaped %d requests against HarfBuzz, %d with a displaced glyph" compared.Value attached.Value
            Expect.isGreaterThan attached.Value 0 "some requests attached a glyph")
    ]
