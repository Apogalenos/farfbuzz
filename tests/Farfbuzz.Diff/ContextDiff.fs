module Farfbuzz.Diff.ContextDiff

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
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.GposDiff
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>Whether a face's GSUB holds a context, chained-context or reverse-chaining subtable.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when it does.</returns>
let private hasContexts (font: TestFont) =
    let table = (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gsub.Value
    table.Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ -> true
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | SubstPending _ -> false))

/// <summary>Whether a face's GSUB holds a subtable of a format that is not ported.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when it does.</returns>
let private hasPending (font: TestFont) =
    let table = (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gsub.Value
    table.Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | SubstPending _ -> true
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ -> false))

/// <summary>The faces with contextual substitution, those of the corpus and the unstripped Latin faces of the API tests.</summary>
let private contextFonts =
    lazy (
        let extra =
            TestFonts.extra [ "api/fonts/OpenSans-Regular.ttf"; "api/fonts/NotoSans-Bold.ttf"; "api/fonts/Inconsolata-Regular.abc.ttf"; "api/fonts/Mplus1p-Regular.ttf"; "api/fonts/Cantarell.A.otf"; "api/fonts/notosansitalic.ttf" ]
            |> Seq.toList
        TestFonts.all () @ extra
        |> List.distinctBy (fun font -> font.Path, font.Index)
        |> List.filter (fun font -> hasContexts font && not (hasPending font))
        |> List.toArray)

/// <summary>Whether a code point is one the default shaper takes straight to its glyph: no mark, no ignorable.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for a plain code point.</returns>
let private plain (codepoint: uint32) =
    let isMark =
        match Unicode.generalCategory codepoint with
        | GeneralCategory.SpacingMark | GeneralCategory.EnclosingMark | GeneralCategory.NonSpacingMark -> true
        | _ -> false
    let decomposesWithMark =
        match Unicode.decompose codepoint with
        | ValueSome (struct (_, second)) -> second <> 0u
        | ValueNone -> false
    Categorize.shaper (Unicode.script codepoint) Direction.Ltr Tags.Empty = DefaultShaper
    && not isMark
    && not (Unicode.isDefaultIgnorable codepoint)
    && not decomposesWithMark

let private coverages = ConcurrentDictionary<string, uint32 array>()

/// <summary>The plain code points a face maps.</summary>
/// <param name="font">The face.</param>
/// <returns>The code points, ascending.</returns>
let private plainCoverage (font: TestFont) =
    coverages.GetOrAdd($"{font.Path}#{font.Index}", fun _ -> TestFonts.coverage font |> Array.filter plain)

/// <summary>Reads a big-endian value of a width.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="width">Two, three or four bytes.</param>
/// <returns>The value, zero beyond the bytes.</returns>
let private readAt (bytes: byte array) (at: int) (width: int) =
    match at >= 0 && at + width <= bytes.Length with
    | false -> 0
    | true -> Seq.fold (fun acc index -> (acc <<< 8) ||| int bytes[at + index]) 0 (seq { 0 .. width - 1 })

/// <summary>Writes a big-endian value of a width into the bytes.</summary>
/// <param name="bytes">The bytes, changed.</param>
/// <param name="at">The offset.</param>
/// <param name="width">Two, three or four bytes.</param>
/// <param name="value">The value.</param>
let private writeAt (bytes: byte array) (at: int) (width: int) (value: int) =
    for index in 0 .. width - 1 do
        bytes[at + index] <- byte (value >>> (8 * (width - 1 - index)))

/// <summary>The offsets of the sfnt headers of a file, one for a plain font, one per face of a collection.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The header offsets.</returns>
let private headers (bytes: byte array) =
    match readAt bytes 0 4 = 0x74746366 with
    | false -> [| 0 |]
    | true -> Array.init (readAt bytes 8 4) (fun index -> readAt bytes (12 + 4 * index) 4)

/// <summary>A copy of a font file with every GPOS table renamed, so no engine finds one.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The copy.</returns>
let private withoutGpos (bytes: byte array) =
    let copy = Array.copy bytes
    for header in headers copy do
        for record in 0 .. readAt copy (header + 4) 2 - 1 do
            let at = header + 12 + 16 * record
            match readAt copy at 4 = 0x47504F53 with
            | true -> writeAt copy at 4 0x47504F58
            | false -> ()
    copy

let private strippedPaths = ConcurrentDictionary<string, string>()

/// <summary>The path of the stripped copy of a font file, written once.</summary>
/// <param name="path">The font file.</param>
/// <returns>The copy's path.</returns>
let private strippedPath (path: string) =
    strippedPaths.GetOrAdd(path, fun path ->
        let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "context-diff")
        Directory.CreateDirectory directory |> ignore
        let copy = Path.Combine(directory, $"{abs (path.GetHashCode())}-{Path.GetFileName path}")
        File.WriteAllBytes(copy, withoutGpos (OracleFaces.data path))
        copy)

/// <summary>The settings of a context differential: direction, flags with and without the unsafe-to-concat request, a
/// cluster level, and a feature setting.</summary>
/// <param name="isAots">Whether the face is one of the aots fonts, whose lookups hang on the test feature.</param>
/// <returns>The generator of the settings.</returns>
let private settings (isAots: bool) =
    gen {
        let! direction = Gen.frequency [ 6, Gen.constant "ltr"; 1, Gen.constant "rtl" ]
        let! flags = Gen.elements [| 0u; 64u; 64u; 64u ||| 128u; 2u; 64u ||| 2u; 1u ||| 64u |]
        let! level = Gen.elements [| 0; 0; 1; 2; 3 |]
        let! features =
            match isAots with
            | true -> Gen.elements [| "test"; "test"; "test"; "test=0"; "-test"; "test[1:3]=0"; "" |]
            | false -> Gen.elements [| ""; ""; ""; "-calt"; "calt=0"; "-liga"; "-ccmp"; "-clig"; "-rclt" |]
        return struct (direction, flags, level, features)
    }

/// <summary>Generates a request on one of the faces from random plain text of its coverage, GPOS stripped.</summary>
/// <param name="fonts">The faces.</param>
/// <param name="maxLength">The longest text.</param>
/// <returns>The generator of the request.</returns>
let private request (fonts: TestFont array) (maxLength: int) =
    gen {
        let! font = Gen.elements fonts
        let coverage = plainCoverage font
        let! length = Gen.choose (1, maxLength)
        let! text = Gen.arrayOfLength length (Gen.elements coverage)
        let! struct (direction, flags, level, features) = settings (font.Path.Contains "aots")
        return
            { Path = strippedPath font.Path
              Cached = true
              Index = font.Index
              Text = text
              Features = features
              Direction = direction
              Flags = flags
              ClusterLevel = level
              Ppem = 0u }
    }

let private compared = ref 0
let private changed = ref 0

/// <summary>Whether shaping changed the glyphs of the text beyond what the character map says, which a substitution did.</summary>
/// <param name="request">The request.</param>
/// <param name="shaped">The shaped glyphs.</param>
/// <returns>True when the glyph sequence differs from the nominal glyphs.</returns>
let private substituted (request: GposRequest) (shaped: Shaped) =
    let face = Face.create (ReadOnlyMemory (OracleFaces.data request.Path)) (uint32 request.Index)
    let font = Font.create face
    let nominal = request.Text |> Array.choose (fun codepoint -> Font.nominalGlyph font codepoint |> ValueOption.toOption)
    nominal <> (shaped |> Array.map (fun (struct (glyph, _, _, _, _, _, _)) -> glyph))

/// <summary>Compares the two engines on a request.</summary>
/// <param name="request">The request.</param>
/// <returns>The two answers.</returns>
let private shapeBoth (request: GposRequest) =
    let ours = ourShape request
    let expected = oracleShape request
    lock compared (fun () ->
        compared.Value <- compared.Value + 1
        match ours, expected with
        | Ok shaped, Ok _ when substituted request shaped -> changed.Value <- changed.Value + 1
        | _ -> ())
    struct (ours, expected)

/// <summary>Compares the engines on generated requests.</summary>
/// <param name="name">The test name.</param>
/// <param name="inputs">The generator.</param>
/// <param name="count">The number of cases.</param>
/// <returns>The test.</returns>
let private versus (name: string) (inputs: Gen<GposRequest>) (count: int) =
    testPropertyWithConfig (config count) name (Prop.forAll (Arb.fromGen inputs) (fun input ->
        let struct (ours, expected) = shapeBoth input
        match ours = expected with
        | true -> Prop.ofTestable true
        | false -> Prop.ofTestable false |> Prop.label $"input: %A{input}\n  oracle:   %A{expected}\n  farfbuzz: %A{ours}"))

/// <summary>Reads a text file's lines.</summary>
/// <param name="name">The text file under upstream's perf texts.</param>
/// <returns>The non-empty lines.</returns>
let private lines (name: string) =
    File.ReadAllLines (Path.Combine(RepositoryPaths.upstream (), "perf", "texts", name))
    |> Array.filter (fun line -> line.Length > 0)

/// <summary>The glyph sequences the rules of the context lookups of a face match, one more per rule than the lookups hold:
/// the glyph of a coverage, then the glyphs a rule's input asks for or a glyph of each coverage.</summary>
/// <param name="font">The test font.</param>
/// <returns>The lookup and glyph sequence pairs.</returns>
let private ruleSequences (font: TestFont) =
    let gsub = (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gsub.Value
    let first (coverage: Coverage) = Coverage.glyphs coverage |> Seq.tryHead
    let classGlyph (classDef: ClassDef) (klass: uint32) =
        match ClassDef.glyphsInClass classDef klass |> Seq.tryHead with
        | Some range -> Some range.First
        | None -> None
    [| for lookup in 0 .. gsub.Lookups.Length - 1 do
        for accel in gsub.Lookups[lookup].Value.Subtables do
            match accel.Subtable with
            | SubstContext (GlyphRules (coverage, ruleSets)) ->
                let glyphs = Coverage.glyphs coverage
                for setIndex in 0 .. min ruleSets.Length glyphs.Length - 1 do
                    for rule in ruleSets[setIndex] do
                        yield struct (lookup, Array.append [| glyphs[setIndex] |] (rule.Input.AsSpan().ToArray()))
            | SubstContext (ClassRules (coverage, classDef, ruleSets, _)) ->
                match first coverage with
                | Some glyph ->
                    for setIndex in 0 .. ruleSets.Length - 1 do
                        for rule in ruleSets[setIndex] do
                            let rest = rule.Input.AsSpan().ToArray() |> Array.map (classGlyph classDef)
                            match Array.forall Option.isSome rest with
                            | true -> yield struct (lookup, Array.append [| glyph |] (Array.map Option.get rest))
                            | false -> ()
                | None -> ()
            | SubstContext (CoverageRule (coverages, _)) ->
                let glyphs = coverages.AsSpan().ToArray() |> Array.map first
                match Array.forall Option.isSome glyphs with
                | true -> yield struct (lookup, Array.map Option.get glyphs)
                | false -> ()
            | SubstChainContext (ChainGlyphRules (coverage, ruleSets)) ->
                let glyphs = Coverage.glyphs coverage
                for setIndex in 0 .. min ruleSets.Length glyphs.Length - 1 do
                    for rule in ruleSets[setIndex] do
                        yield struct (lookup, Array.append [| glyphs[setIndex] |] (rule.ChainInput.AsSpan().ToArray()))
            | SubstChainContext (ChainClassRules (coverage, _, inputClassDef, _, ruleSets, _)) ->
                match first coverage with
                | Some glyph ->
                    for setIndex in 0 .. ruleSets.Length - 1 do
                        for rule in ruleSets[setIndex] do
                            let rest = rule.ChainInput.AsSpan().ToArray() |> Array.map (classGlyph inputClassDef)
                            match Array.forall Option.isSome rest with
                            | true -> yield struct (lookup, Array.append [| glyph |] (Array.map Option.get rest))
                            | false -> ()
                | None -> ()
            | SubstChainContext (ChainCoverageRule (_, input, _, _)) ->
                let glyphs = input.AsSpan().ToArray() |> Array.map first
                match Array.forall Option.isSome glyphs with
                | true -> yield struct (lookup, Array.map Option.get glyphs)
                | false -> ()
            | ReverseChainSingle (coverage, _, _, _) ->
                match first coverage with
                | Some glyph -> yield struct (lookup, [| glyph |])
                | None -> ()
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | SubstPending _ -> () |]

/// <summary>The would-substitute inputs: the sequences the rules match, each shortened, lengthened with a random glyph, and
/// with its first glyph changed, with and without zero context.</summary>
/// <returns>The font, lookup, glyphs and zero-context tuples.</returns>
let private wouldSubstituteInputs () =
    contextFonts.Value
    |> Seq.collect (fun font ->
        let glyphCount = (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index)).GlyphCount
        ruleSequences font
        |> Seq.truncate 4000
        |> Seq.collect (fun (struct (lookup, glyphs)) ->
            seq {
                let variants =
                    [ glyphs
                      Array.truncate (max 1 (glyphs.Length - 1)) glyphs
                      Array.append glyphs [| uint32 (glyphCount / 2) |]
                      Array.append [| uint32 (glyphCount - 1) |] (Array.tail glyphs) ]
                for variant in variants do
                    for zeroContext in [ 0; 1 ] do
                        yield struct (font, lookup, variant, zeroContext)
            }))
    |> Seq.toArray

/// <summary>Farfbuzz's would-substitute answer.</summary>
/// <param name="input">The font, lookup, glyphs and zero context.</param>
/// <returns>Whether the lookup would substitute.</returns>
let private ourWouldSubstitute (input: struct (TestFont * int * uint32 array * int)) =
    let struct (font, lookup, glyphs, zeroContext) = input
    let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index)
    LayoutApi.wouldSubstitute (OtFace.create face) lookup (ImmutableArray.CreateRange glyphs) (zeroContext <> 0)

/// <summary>The oracle's would-substitute answer.</summary>
/// <param name="input">The font, lookup, glyphs and zero context.</param>
/// <returns>Whether the lookup would substitute.</returns>
let private oracleWouldSubstitute (input: struct (TestFont * int * uint32 array * int)) =
    let struct (font, lookup, glyphs, zeroContext) = input
    Native.hb_ot_layout_lookup_would_substitute (OracleFaces.face font (uint32 font.Index), uint32 lookup, glyphs, uint32 glyphs.Length, zeroContext) <> 0

/// <summary>The fields of the context, chained-context and reverse-chaining subtables of a table of a font file that the
/// mutation differential rewrites, the lookup types of the table given: the name, absolute offset, width and sample values of each. An extension's own fields
/// are included, and a table the walk cannot follow yields nothing past the field it cannot read.</summary>
/// <param name="tableTag">The table, GSUB or GPOS.</param>
/// <param name="extension">The extension lookup type of the table.</param>
/// <param name="context">The context lookup type of the table.</param>
/// <param name="chain">The chained-context lookup type of the table.</param>
/// <param name="reverse">The reverse-chaining lookup type of the table, or a number no lookup has.</param>
/// <param name="bytes">The file bytes.</param>
/// <returns>The fields.</returns>
let contextFieldsOf (tableTag: Tag) (extension: int) (context: int) (chain: int) (reverse: int) (bytes: byte array) =
    let face = Face.create (ReadOnlyMemory bytes) 0u
    match Face.tryFindTable face tableTag with
    | ValueNone -> [||]
    | ValueSome record ->
        let table = int (face.BaseOffset + record.Offset)
        let length = int record.Length
        let lookupList = table + readAt bytes (table + 8) 2
        let offsets = [| 0; 1; 2; 0xFFFF; length |]
        let counts = [| 0; 1; 2; 3; 0x7FFF; 0xFFFF |]
        let indices = [| 0; 1; 2; 3; 0xFFFF |]
        let formats = [| 0; 1; 2; 3; 4; 5; 6; 7; 0xFFFF |]
        let wide (kind: int) (format: int) =
            match kind with
            | 5 | 6 -> format >= 4
            | _ -> format >= 2
        let records (tag: string) (at: int) =
            [ $"{tag} record sequence index", at, 2, [| 0; 1; 2; 64; 0xFFFF |]
              $"{tag} record lookup index", at + 2, 2, indices ]
        let ruleFields (tag: string) (rule: int) (vw: int) =
            let inputCount = readAt bytes rule 2
            [ yield $"{tag} rule input count", rule, 2, counts
              yield $"{tag} rule lookup count", rule + 2, 2, counts
              yield $"{tag} rule first input", rule + 4, vw, [| 0; 1; 2; 0xFFFF |]
              yield! records $"{tag} rule" (rule + 4 + max 0 (inputCount - 1) * vw) ]
        let chainRuleFields (tag: string) (rule: int) (vw: int) =
            let backtrackCount = readAt bytes rule 2
            let inputAt = rule + 2 + backtrackCount * vw
            let inputCount = readAt bytes inputAt 2
            let lookaheadAt = inputAt + 2 + max 0 (inputCount - 1) * vw
            let lookaheadCount = readAt bytes lookaheadAt 2
            let lookupAt = lookaheadAt + 2 + lookaheadCount * vw
            [ yield $"{tag} chain rule backtrack count", rule, 2, counts
              yield $"{tag} chain rule input count", inputAt, 2, counts
              yield $"{tag} chain rule lookahead count", lookaheadAt, 2, counts
              yield $"{tag} chain rule lookup count", lookupAt, 2, counts
              yield $"{tag} chain rule first backtrack", rule + 2, vw, [| 0; 1; 0xFFFF |]
              yield! records $"{tag} chain rule" (lookupAt + 2) ]
        [| for lookupIndex in 0 .. min 64 (readAt bytes lookupList 2) - 1 do
            let lookup = lookupList + readAt bytes (lookupList + 2 + lookupIndex * 2) 2
            let lookupType = readAt bytes lookup 2
            yield $"lookup {lookupIndex} type", lookup, 2, [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 0xFFFF |]
            yield $"lookup {lookupIndex} subtable count", lookup + 4, 2, counts
            for subtableIndex in 0 .. min 6 (readAt bytes (lookup + 4) 2) - 1 do
                let outer = lookup + readAt bytes (lookup + 6 + subtableIndex * 2) 2
                let kind, subtable =
                    match lookupType = extension with
                    | true -> readAt bytes (outer + 2) 2, outer + readAt bytes (outer + 4) 4
                    | false -> lookupType, outer
                let format = readAt bytes subtable 2
                let tag = $"lookup {lookupIndex} subtable {subtableIndex} type {kind} format {format}"
                let canonical = match kind = context, kind = chain, kind = reverse with | true, _, _ -> 5 | _, true, _ -> 6 | _, _, true -> 8 | _ -> kind
                if lookupType = extension then
                    yield $"{tag} extension format", outer, 2, [| 0; 1; 2; 0xFFFF |]
                    yield $"{tag} extension type", outer + 2, 2, [| 0; 1; 5; 6; 7; 8; 9; 0xFFFF |]
                    yield $"{tag} extension offset", outer + 4, 4, [| 0; 1; 8; 0xFFFF; length |]
                if canonical = 5 || canonical = 6 || canonical = 8 then
                    let w = match wide canonical format with | true -> 3 | false -> 2
                    let covW = match wide canonical format with | true -> 4 | false -> 2
                    yield $"{tag} format", subtable, 2, formats
                    match canonical, format with
                    | 5, (1 | 4) ->
                        yield $"{tag} coverage offset", subtable + 2, covW, offsets
                        yield $"{tag} rule set count", subtable + 2 + covW, w, counts
                        yield $"{tag} first rule set offset", subtable + 2 + covW + w, w, offsets
                        let set = subtable + readAt bytes (subtable + 2 + covW + w) w
                        yield $"{tag} first rule set count", set, 2, counts
                        yield $"{tag} first rule offset", set + 2, 2, offsets
                        yield! ruleFields tag (set + readAt bytes (set + 2) 2) w
                    | 5, (2 | 5) ->
                        yield $"{tag} coverage offset", subtable + 2, covW, offsets
                        yield $"{tag} class def offset", subtable + 2 + covW, covW, offsets
                        yield $"{tag} rule set count", subtable + 2 + 2 * covW, w, counts
                        yield $"{tag} first rule set offset", subtable + 2 + 2 * covW + w, w, offsets
                        let set = subtable + readAt bytes (subtable + 2 + 2 * covW + w) w
                        yield $"{tag} first rule set count", set, 2, counts
                        yield $"{tag} first rule offset", set + 2, w, offsets
                        yield! ruleFields tag (set + readAt bytes (set + 2) w) 2
                    | 5, (3 | 6) ->
                        let glyphCount = readAt bytes (subtable + 2) 2
                        yield $"{tag} glyph count", subtable + 2, 2, counts
                        yield $"{tag} lookup count", subtable + 4, 2, counts
                        yield $"{tag} first coverage offset", subtable + 6, covW, offsets
                        yield! records tag (subtable + 6 + glyphCount * covW)
                    | 6, (1 | 4) ->
                        yield $"{tag} coverage offset", subtable + 2, covW, offsets
                        yield $"{tag} rule set count", subtable + 2 + covW, w, counts
                        yield $"{tag} first rule set offset", subtable + 2 + covW + w, w, offsets
                        let set = subtable + readAt bytes (subtable + 2 + covW + w) w
                        yield $"{tag} first rule set count", set, 2, counts
                        yield $"{tag} first rule offset", set + 2, w, offsets
                        yield! chainRuleFields tag (set + readAt bytes (set + 2) w) w
                    | 6, (2 | 5) ->
                        yield $"{tag} coverage offset", subtable + 2, covW, offsets
                        yield $"{tag} backtrack class def offset", subtable + 2 + covW, covW, offsets
                        yield $"{tag} input class def offset", subtable + 2 + 2 * covW, covW, offsets
                        yield $"{tag} lookahead class def offset", subtable + 2 + 3 * covW, covW, offsets
                        yield $"{tag} rule set count", subtable + 2 + 4 * covW, w, counts
                        yield $"{tag} first rule set offset", subtable + 2 + 4 * covW + w, w, offsets
                        let set = subtable + readAt bytes (subtable + 2 + 4 * covW + w) w
                        yield $"{tag} first rule set count", set, 2, counts
                        yield $"{tag} first rule offset", set + 2, w, offsets
                        yield! chainRuleFields tag (set + readAt bytes (set + 2) w) 2
                    | 6, 3 ->
                        let backtrackCount = readAt bytes (subtable + 2) 2
                        let inputAt = subtable + 4 + backtrackCount * 2
                        let inputCount = readAt bytes inputAt 2
                        let lookaheadAt = inputAt + 2 + inputCount * 2
                        let lookaheadCount = readAt bytes lookaheadAt 2
                        let lookupAt = lookaheadAt + 2 + lookaheadCount * 2
                        yield $"{tag} backtrack count", subtable + 2, 2, counts
                        yield $"{tag} input count", inputAt, 2, counts
                        yield $"{tag} lookahead count", lookaheadAt, 2, counts
                        yield $"{tag} lookup count", lookupAt, 2, counts
                        yield $"{tag} first backtrack coverage offset", subtable + 4, 2, offsets
                        yield $"{tag} first input coverage offset", inputAt + 2, 2, offsets
                        yield! records tag (lookupAt + 2)
                    | 8, (1 | 2) ->
                        let backtrackCount = readAt bytes (subtable + 2 + covW) 2
                        let lookaheadAt = subtable + 4 + covW + backtrackCount * w
                        let lookaheadCount = readAt bytes lookaheadAt 2
                        yield $"{tag} coverage offset", subtable + 2, covW, offsets
                        yield $"{tag} backtrack count", subtable + 2 + covW, 2, counts
                        yield $"{tag} lookahead count", lookaheadAt, 2, counts
                        yield $"{tag} glyph count", lookaheadAt + 2 + lookaheadCount * w, w, counts
                        yield $"{tag} first backtrack coverage offset", subtable + 4 + covW, w, offsets
                    | _ -> () |]

/// <summary>The fields of the GSUB context, chained-context and reverse-chaining subtables the mutation differential rewrites.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The fields.</returns>
let private contextFields (bytes: byte array) = contextFieldsOf Tags.Gsub 7 5 6 8 bytes

/// <summary>The number of GSUB lookups a face holds, which HarfBuzz reads as zero when it rejects the table.</summary>
/// <param name="path">The font file.</param>
/// <returns>HarfBuzz's count and Farfbuzz's.</returns>
let private lookupCounts (path: string) =
    use blob = HarfBuzzSharp.Blob.FromFile path
    use face = new HarfBuzzSharp.Face(blob, 0)
    let oracle = int (Native.hb_ot_layout_table_get_lookup_count(face.Handle, uint32 Tags.Gsub))
    let ours = LayoutTable.lookupCount (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u |> OtFace.create).Gsub.Value
    struct (oracle, ours)

/// <summary>Big-endian bytes of a 16-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let be16 (value: int) = [| byte (value >>> 8); byte value |]

/// <summary>Big-endian bytes of a 32-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The four bytes.</returns>
let be32 (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A coverage table of one glyph, format 1.</summary>
/// <param name="glyph">The glyph.</param>
/// <returns>The bytes.</returns>
let coverageOne (glyph: int) = Array.concat [ be16 1; be16 1; be16 glyph ]

/// <summary>A lookup of one subtable.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="subtable">The subtable bytes.</param>
/// <returns>The bytes.</returns>
let lookupOf (lookupType: int) (subtable: byte array) = Array.concat [ be16 lookupType; be16 0; be16 1; be16 8; subtable ]

/// <summary>A single substitution that adds a delta to one glyph, format 1.</summary>
/// <param name="glyph">The glyph.</param>
/// <param name="delta">The delta.</param>
/// <returns>The lookup.</returns>
let private singleLookup (glyph: int) (delta: int) = lookupOf 1 (Array.concat [ be16 1; be16 6; be16 (delta &&& 0xFFFF); coverageOne glyph ])

/// <summary>A context subtable of the third format that matches one glyph and applies lookups at its position.</summary>
/// <param name="glyph">The glyph.</param>
/// <param name="records">The sequence index and lookup index of each record.</param>
/// <returns>The lookup.</returns>
let private contextLookup (glyph: int) (records: (int * int) list) =
    let header = 6 + 2 + 4 * records.Length
    lookupOf 5 (Array.concat [ be16 3; be16 1; be16 records.Length; be16 header; Array.concat (records |> List.map (fun (position, lookup) -> Array.append (be16 position) (be16 lookup))); coverageOne glyph ])

/// <summary>A coverage table of several glyphs, format 1.</summary>
/// <param name="glyphs">The glyphs, ascending.</param>
/// <returns>The bytes.</returns>
let coverageOf (glyphs: int list) = Array.concat [ be16 1; be16 glyphs.Length; Array.concat (glyphs |> List.map be16) ]

/// <summary>A reverse chaining single substitution, lookup type 8, format 1.</summary>
/// <param name="covered">The glyphs the subtable covers, ascending, each replaced by the next entry of the substitutes.</param>
/// <param name="backtrack">The backtrack coverages, nearest first, as glyph lists.</param>
/// <param name="lookahead">The lookahead coverages as glyph lists.</param>
/// <param name="substitutes">The substitute glyphs.</param>
/// <returns>The lookup.</returns>
let private reverseLookup (covered: int list) (backtrack: int list list) (lookahead: int list list) (substitutes: int list) =
    let coverages = coverageOf covered :: (backtrack @ lookahead |> List.map coverageOf)
    let header = 2 + 2 + 2 + 2 * backtrack.Length + 2 + 2 * lookahead.Length + 2 + 2 * substitutes.Length
    let offsets = coverages |> List.scan (fun offset coverage -> offset + coverage.Length) header |> List.take coverages.Length
    let covered = offsets.Head
    let backtrackOffsets = offsets.Tail |> List.take backtrack.Length
    let lookaheadOffsets = offsets.Tail |> List.skip backtrack.Length
    lookupOf 8 (Array.concat [ be16 1; be16 covered; be16 backtrack.Length; Array.concat (backtrackOffsets |> List.map be16); be16 lookahead.Length; Array.concat (lookaheadOffsets |> List.map be16); be16 substitutes.Length; Array.concat (substitutes |> List.map be16); Array.concat coverages ])

/// <summary>A GSUB table whose one feature <c>test</c> applies every lookup.</summary>
/// <param name="lookups">The lookups.</param>
/// <returns>The table bytes.</returns>
let gsubTable (lookups: byte array list) =
    let scriptList = Array.concat [ be16 1; Array.concat [ "DFLT"B; be16 8 ]; be16 4; be16 0; be16 0; be16 0xFFFF; be16 1; be16 0 ]
    let feature = Array.concat [ be16 0; be16 lookups.Length; Array.concat (lookups |> List.mapi (fun index _ -> be16 index)) ]
    let featureList = Array.concat [ be16 1; "test"B; be16 8; feature ]
    let offsets = lookups |> List.scan (fun offset lookup -> offset + lookup.Length) (2 + 2 * lookups.Length) |> List.take lookups.Length
    let lookupList = Array.concat [ be16 lookups.Length; Array.concat (offsets |> List.map be16); Array.concat lookups ]
    let scriptsAt = 10
    let featuresAt = scriptsAt + scriptList.Length
    let lookupsAt = featuresAt + featureList.Length
    Array.concat [ be32 0x00010000; be16 scriptsAt; be16 featuresAt; be16 lookupsAt; scriptList; featureList; lookupList ]

/// <summary>A copy of a font file with one table replaced by a table appended at the end of the file.</summary>
/// <param name="tag">The table's tag as a big-endian number.</param>
/// <param name="bytes">The file bytes.</param>
/// <param name="table">The new table.</param>
/// <returns>The new file.</returns>
let withTable (tag: int) (bytes: byte array) (table: byte array) =
    let copy = Array.append bytes (Array.zeroCreate (((bytes.Length + 3) &&& ~~~3) - bytes.Length))
    let at = copy.Length
    for record in 0 .. readAt copy 4 2 - 1 do
        let entry = 12 + 16 * record
        match readAt copy entry 4 = tag with
        | true ->
            writeAt copy (entry + 8) 4 at
            writeAt copy (entry + 12) 4 table.Length
        | false -> ()
    Array.append copy table

/// <summary>A copy of a font file with its GSUB replaced by a table appended at the end of the file.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="table">The new GSUB table.</param>
/// <returns>The new file.</returns>
let private withGsub (bytes: byte array) (table: byte array) = withTable 0x47535542 bytes table

/// <summary>The differentials of P5.S09: contexts, chained contexts and reverse chaining against HarfBuzz.</summary>
/// <returns>The test list.</returns>
let tests () =
    let wouldSubstituteCases = wouldSubstituteInputs ()
    let fonts = contextFonts.Value |> Array.filter (fun font -> (plainCoverage font).Length > 0)
    let aots = fonts |> Array.filter (fun font -> font.Path.Contains "aots")
    let real = fonts |> Array.filter (fun font -> not (font.Path.Contains "aots"))
    let roboto = fonts |> Array.filter (fun font -> font.Path.EndsWith "perf/fonts/Roboto-Regular.ttf")
    let robotoLines =
        Array.append (lines "en-thelittleprince.txt") (lines "en-words.txt")
        |> Array.map (fun line -> line.EnumerateRunes() |> Seq.map (fun rune -> uint32 rune.Value) |> Seq.toArray)
        |> Array.map (Array.truncate 200)
    testSequenced <| testList "P5.S09" [
        testCase "P5.S09 a Duployan text whose backtrack fails before a reverse lookup's lookahead is read" (fun () ->
            let font = contextFonts.Value |> Array.find (fun font -> font.Path.EndsWith "perf/fonts/NotoSansDuployan-Regular.otf")
            let request =
                { Path = strippedPath font.Path
                  Cached = true
                  Index = font.Index
                  Text = [| 187u; 62u; 113824u; 215u; 60u; 8971u; 8211u; 44u; 51u; 11840u; 187u; 113825u; 8970u; 55u; 113826u; 176u; 8220u; 8220u; 51u; 9676u; 33u; 113826u; 46u; 60u |]
                  Features = ""
                  Direction = "ltr"
                  Flags = 66u
                  ClusterLevel = 2
                  Ppem = 0u }
            let short = { request with Text = [| 62u; 113824u; 215u; 60u; 46u |] }
            for case in [ short; request ] do
                let struct (ours, expected) = shapeBoth case
                Expect.equal ours expected "the glyphs, clusters, positions and flags of the fixed Duployan text")
        testCase "P5.S09 reverse chaining lookups, fixed and random backtracks and lookaheads, against HarfBuzz" (fun () ->
            let font = aots |> Array.find (fun font -> font.Path.Contains "gsub_context1_simple_f1")
            let ourFont = Font.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))
            let codepoints = plainCoverage font |> Array.truncate 4
            let glyphs = codepoints |> Array.map (fun codepoint -> int (Font.nominalGlyph ourFont codepoint |> ValueOption.get))
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "context-limits")
            Directory.CreateDirectory directory |> ignore
            let path = Path.Combine(directory, "reverse.ttf")
            let failures = ResizeArray<string>()
            let mutable compared = 0
            let mutable flagged = 0
            let mutable substituted = 0
            let compare (name: string) (lookups: byte array list) (text: uint32 array) (flags: uint32) (level: int) (direction: string) =
                File.WriteAllBytes(path, withGsub (OracleFaces.data font.Path) (gsubTable lookups))
                let request = { Path = path; Cached = false; Index = font.Index; Text = text; Features = "test"; Direction = direction; Flags = flags; ClusterLevel = level; Ppem = 0u }
                let ours = ourShape request
                let expected = oracleShape request
                compared <- compared + 1
                (match expected with
                 | Ok shaped ->
                    if shaped |> Array.exists (fun (struct (_, _, _, _, _, _, flag)) -> (flag &&& 2u) <> 0u) then flagged <- flagged + 1
                    if shaped |> Array.map (fun (struct (g, _, _, _, _, _, _)) -> g) <> (text |> Array.map (fun c -> uint32 (int (Font.nominalGlyph ourFont c |> ValueOption.get)))) then substituted <- substituted + 1
                 | Error _ -> ())
                match ours = expected with
                | true -> ()
                | false -> failures.Add $"{name}: text %A{text}, flags {flags}, level {level}, {direction}: farfbuzz %A{ours}, oracle %A{expected}"
            let a, b, c, d = glyphs[0], glyphs[1], glyphs[2], glyphs[3]
            let text = codepoints
            compare "the backtrack never matches, the lookahead matches" [ reverseLookup [ a ] [ [ d ] ] [ [ a ] ] [ b ] ] [| text[0]; text[0]; text[0] |] 64u 0 "ltr"
            compare "the backtrack never matches, the lookahead fails" [ reverseLookup [ a ] [ [ d ] ] [ [ c ] ] [ b ] ] [| text[0]; text[0]; text[0] |] 64u 0 "ltr"
            compare "the backtrack never matches, no lookahead" [ reverseLookup [ a ] [ [ d ] ] [] [ b ] ] [| text[0]; text[0]; text[0] |] 64u 0 "ltr"
            compare "the backtrack matches, the lookahead fails" [ reverseLookup [ a ] [ [ a ] ] [ [ c ] ] [ b ] ] [| text[0]; text[0]; text[0] |] 64u 0 "ltr"
            compare "the backtrack runs past the start" [ reverseLookup [ a ] [ [ a ]; [ a ]; [ a ] ] [ [ a ] ] [ b ] ] [| text[0]; text[0]; text[0] |] 64u 0 "ltr"
            compare "the first glyph's backtrack fails" [ reverseLookup [ a ] [ [ a ] ] [ [ a ] ] [ b ] ] [| text[0] |] 64u 0 "ltr"
            let mutable state = 20261114u
            let nextRandom (bound: int) =
                state <- state * 1103515245u + 12345u
                int ((state >>> 8) % uint32 bound)
            let subset () =
                let picked = glyphs |> Array.filter (fun _ -> nextRandom 2 = 0)
                match picked.Length with
                | 0 -> [ glyphs[nextRandom glyphs.Length] ]
                | _ -> picked |> Array.sort |> Array.toList
            let sequence () = List.init (nextRandom 4) (fun _ -> subset ())
            for case in 1 .. 3000 do
                let lookups = List.init (1 + nextRandom 3) (fun _ -> let covered = subset () in reverseLookup covered (sequence ()) (sequence ()) (covered |> List.map (fun _ -> glyphs[nextRandom glyphs.Length])))
                let text = Array.init (1 + nextRandom 10) (fun _ -> codepoints[nextRandom codepoints.Length])
                compare $"random case {case}" lookups text ([| 64u; 66u; 64u; 0u |][nextRandom 4]) (nextRandom 4) ([| "ltr"; "ltr"; "rtl" |][nextRandom 3])
            printfn "P5.S09 compared %d reverse chaining tables with HarfBuzz: %d with an unsafe-to-concat glyph, %d that substitute" compared flagged substituted
            Expect.isEmpty (failures |> Seq.truncate 20 |> Seq.toList) ($"reverse chaining differences ({failures.Count} in all):\n" + String.concat "\n" (failures |> Seq.truncate 20))
            Expect.isGreaterThan flagged 500 "the cases reach the unsafe-to-concat marks"
            Expect.isGreaterThan substituted 500 "the cases substitute")
        testCase "P5.S09 the corpus has faces with contexts" (fun () ->
            printfn "P5.S09 faces with contextual substitution: %d in all, %d aots, %d real, Roboto %d" fonts.Length aots.Length real.Length roboto.Length
            Expect.isGreaterThan aots.Length 20 "aots context and chaining faces"
            Expect.isGreaterThan real.Length 0 "real faces"
            Expect.isGreaterThan roboto.Length 0 "Roboto")
        sampled "P5.S09 would substitute (types 5, 6, 8)" (Gen.elements wouldSubstituteCases) 20_000 ourWouldSubstitute oracleWouldSubstitute
        versus "P5.S09 GSUB shaping, aots context and chaining fonts" (request aots 12) 6000
        versus "P5.S09 GSUB shaping, real fonts, random text" (request real 24) 8000
        versus "P5.S09 GSUB shaping, Roboto benchmark lines"
            (gen {
                let! face = Gen.elements roboto
                let! line = Gen.elements robotoLines
                let! struct (direction, flags, level, features) = settings false
                return
                    { Path = strippedPath face.Path
                      Cached = true
                      Index = face.Index
                      Text = line
                      Features = features
                      Direction = direction
                      Flags = flags
                      ClusterLevel = level
                      Ppem = 0u }
            })
            2000
        testCase "P5.S09 mutated context, chained-context and reverse-chaining subtables" (fun () ->
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "context-mutation")
            Directory.CreateDirectory directory |> ignore
            let sources =
                fonts
                |> Array.map (fun font ->
                    let path = strippedPath font.Path
                    font, path, contextFields (OracleFaces.data path))
                |> Array.filter (fun (_, _, fields) -> fields.Length > 0)
            let failures = ResizeArray<string>()
            let mutable state = 20241111u
            let nextRandom () =
                state <- state * 1103515245u + 12345u
                state >>> 8
            let mutable cases = 0
            let mutable rejected = 0
            let mutable rejectedByUs = 0
            let mutable shapedDifferently = 0
            let mutable sameRejection = 0
            let mutable attempt = 0
            while cases < 5000 && attempt < 100000 do
                attempt <- attempt + 1
                let font, path, fields = sources[int (nextRandom () % uint32 sources.Length)]
                let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                let value = values[int (nextRandom () % uint32 values.Length)]
                let candidate = Array.copy (OracleFaces.data path)
                match offset >= 0 && offset + width <= candidate.Length with
                | false -> ()
                | true ->
                    writeAt candidate offset width value
                    let mutated = Path.Combine(directory, $"mutated-{cases}.ttf")
                    File.WriteAllBytes(mutated, candidate)
                    let coverage = plainCoverage font
                    let text = Array.init (1 + int (nextRandom () % 12u)) (fun _ -> coverage[int (nextRandom () % uint32 coverage.Length)])
                    let features = match font.Path.Contains "aots" with | true -> "test" | false -> ""
                    let flags = match nextRandom () % 2u with | 0u -> 0u | _ -> 64u
                    let request = { Path = mutated; Cached = false; Index = font.Index; Text = text; Features = features; Direction = "ltr"; Flags = flags; ClusterLevel = 0; Ppem = 0u }
                    let struct (ours, expected) = shapeBoth request
                    let struct (oracleCount, ourCount) = lookupCounts mutated
                    let struct (originalOracle, _) = lookupCounts path
                    if oracleCount = 0 && originalOracle > 0 then rejected <- rejected + 1
                    if ourCount = 0 && originalOracle > 0 then rejectedByUs <- rejectedByUs + 1
                    if (oracleCount = 0) = (ourCount = 0) && oracleCount = 0 && originalOracle > 0 then sameRejection <- sameRejection + 1
                    if oracleCount <> ourCount then failures.Add $"{Path.GetFileName font.Path}: {name} = {value}: lookup count oracle {oracleCount}, farfbuzz {ourCount}"
                    elif ours <> expected then
                        shapedDifferently <- shapedDifferently + 1
                        let kept = Path.Combine(directory, $"failure-{shapedDifferently}.ttf")
                        File.Copy(mutated, kept, true)
                        let differences =
                            match ours, expected with
                            | Ok ours, Ok expected ->
                                let shared = min ours.Length expected.Length
                                $"lengths {ours.Length} and {expected.Length}; first differences: %A{[ for index in 0 .. shared - 1 do if ours[index] <> expected[index] then yield index, ours[index], expected[index] ] |> List.truncate 4}"
                            | _ -> $"farfbuzz %A{ours}, oracle %A{expected}"
                        let original = shapeBoth { request with Path = path; Cached = true }
                        let originalAgrees = (let struct (a, b) = original in a = b)
                        failures.Add $"{Path.GetFileName font.Path}: {name} = {value}: shaped differently (kept {kept}, text %A{text}, features {features}, flags {flags}, the unmutated face agrees: {originalAgrees}): {differences}"
                    File.Delete mutated
                    cases <- cases + 1
            printfn "P5.S09 compared %d mutated subtables: HarfBuzz rejected the table in %d, Farfbuzz in %d, both in %d; %d shaped differently" cases rejected rejectedByUs sameRejection shapedDifferently
            Expect.isEmpty (failures |> Seq.truncate 40 |> Seq.toList) ($"mutated subtable differences ({failures.Count} in all):\n" + String.concat "\n" (failures |> Seq.truncate 40))
            Expect.isGreaterThanOrEqual cases 5000 "the number of cases"
            Expect.isGreaterThan rejected 0 "some mutations make HarfBuzz reject the table"
            Expect.equal rejectedByUs rejected "Farfbuzz rejects the tables HarfBuzz rejects")
        testCase "P5.S09 nesting and operation limits against HarfBuzz" (fun () ->
            let font = aots |> Array.find (fun font -> font.Path.Contains "gsub_context1_simple_f1")
            let ourFont = Font.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))
            let codepoints = plainCoverage font |> Array.truncate 3
            let glyph (codepoint: uint32) = int (Font.nominalGlyph ourFont codepoint |> ValueOption.get)
            let first = glyph codepoints[0]
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "context-limits")
            Directory.CreateDirectory directory |> ignore
            let failures = ResizeArray<string>()
            let mutable compared = 0
            let mutable failed = 0
            let compare (name: string) (lookups: byte array list) (text: uint32 array) =
                let path = Path.Combine(directory, "limits.ttf")
                File.WriteAllBytes(path, withGsub (OracleFaces.data font.Path) (gsubTable lookups))
                let request = { Path = path; Cached = false; Index = font.Index; Text = text; Features = "test"; Direction = "ltr"; Flags = 0u; ClusterLevel = 0; Ppem = 0u }
                let ours = ourShape request
                let expected = oracleShape request
                compared <- compared + 1
                (match expected with | Error _ -> failed <- failed + 1 | Ok _ -> ())
                match ours = expected with
                | true -> ()
                | false -> failures.Add $"{name}: farfbuzz %A{ours}, oracle %A{expected}"
                expected
            let changed (result: Result<Shaped, string>) (text: uint32 array) =
                match result with
                | Ok shaped -> shaped |> Array.map (fun (struct (g, _, _, _, _, _, _)) -> g) <> (text |> Array.map (fun c -> uint32 (glyph c)))
                | Error _ -> true
            let text = [| codepoints[0]; codepoints[1] |]
            let outcomes = ResizeArray<string * bool>()
            let run (name: string) (lookups: byte array list) (text: uint32 array) =
                outcomes.Add((name, changed (compare name lookups text) text))
            run "a lookup that recurses into itself" [ contextLookup first [ 0, 0 ] ] text
            for depth in [ 1; 2; 62; 63; 64; 65; 66; 70 ] do
                let chain = [ for index in 0 .. depth - 1 -> contextLookup first [ 0, index + 1 ] ]
                run $"a chain of {depth} nested lookups" (chain @ [ singleLookup first 1 ]) text
            for count in [ 1; 1000; 16000; 16370; 16371; 16372; 16373; 16374; 16375; 16376; 16377; 16378; 16379; 16380; 16383; 16384; 16385; 16386; 16390; 16400; 20000 ] do
                run $"{count} records in one rule" [ contextLookup first (List.replicate count (0, 1)); singleLookup first 1 ] text
            for count in [ 8000; 8190; 8191; 8192; 8193; 8200 ] do
                run $"{count} records in one rule, two glyphs" [ contextLookup first (List.replicate count (0, 1)); singleLookup first 1 ] [| codepoints[0]; codepoints[0]; codepoints[1] |]
            printfn "P5.S09 limits: %d tables compared with HarfBuzz; the ones whose text HarfBuzz changes: %s" compared (String.concat "; " (outcomes |> Seq.filter snd |> Seq.map fst))
            Expect.isEmpty failures ("limit differences:\n" + String.concat "\n" failures)
            let outcome (name: string) = outcomes |> Seq.find (fun (candidate, _) -> candidate = name) |> snd
            Expect.isTrue (outcome "a chain of 64 nested lookups") "a chain of 64 nested lookups still applies"
            Expect.isFalse (outcome "a chain of 65 nested lookups") "a chain of 65 nested lookups reaches the nesting limit")
        testCase "P5.S09 report" (fun () ->
            printfn "P5.S09 shaped %d requests against HarfBuzz, %d with a substitution" compared.Value changed.Value
            Expect.isGreaterThan changed.Value 0 "some requests were substituted")
    ]
