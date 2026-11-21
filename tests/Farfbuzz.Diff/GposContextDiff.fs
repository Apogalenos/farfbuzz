module Farfbuzz.Diff.GposContextDiff

open System
open System.Collections.Concurrent
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.GposDiff
open Farfbuzz.Diff.GposAttachDiff
open Farfbuzz.Diff.ContextDiff
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>The GPOS lookup type of a context lookup, the context subtable and the chained-context subtable.</summary>
[<Literal>]
let private ContextType = 7

/// <summary>The GPOS lookup type of a chained-context lookup.</summary>
[<Literal>]
let private ChainType = 8

/// <summary>The GPOS lookup type of an extension lookup.</summary>
[<Literal>]
let private ExtensionType = 9

/// <summary>Whether a face's GPOS holds a context or chained-context subtable.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when it does.</returns>
let private hasContexts (font: TestFont) =
    (gposOf font).Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | PosContext _ | PosChainContext _ -> true
            | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosPending _ -> false))

/// <summary>The faces of the corpus and of the unstripped Latin faces of the API tests whose GPOS holds a context subtable.</summary>
let private contextCandidates =
    lazy (
        let extra =
            TestFonts.extra [ "api/fonts/OpenSans-Regular.ttf"; "api/fonts/NotoSans-Bold.ttf"; "api/fonts/Inconsolata-Regular.abc.ttf"; "api/fonts/Mplus1p-Regular.ttf"; "api/fonts/Cantarell.A.otf"; "api/fonts/notosansitalic.ttf" ]
            |> Seq.toList
        TestFonts.all () @ extra
        |> List.distinctBy (fun font -> font.Path, font.Index)
        |> List.filter hasContexts
        |> List.toArray)

/// <summary>The number of code points of a face that the default shaper takes straight to glyphs.</summary>
/// <param name="font">The face.</param>
/// <returns>The count of bases and marks.</returns>
let private usableCount (font: TestFont) =
    let struct (bases, marks) = alphabetOf font
    bases.Length + marks.Length

/// <summary>The candidates that can be shaped through code points of the default shaper, the others needing a complex shaper
/// that is not ported (Arabic, Indic, USE, Hangul and the like) to reach their lookups.</summary>
let private contextFonts =
    lazy (contextCandidates.Value |> Array.filter (fun font -> usableCount font > 0))

let private gposBytes = Tag.make 'G' 'P' 'O' 'S'

let private bareFiles = ConcurrentDictionary<string, string>()

/// <summary>A copy of a font file whose context and chained-context lookups, and the extension subtables that wrap them, have
/// the type zero, which neither engine dispatches to a subtable: the font as it shapes without them.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The copy.</returns>
let private withoutContexts (bytes: byte array) =
    let copy = Array.copy bytes
    match gposRecord copy with
    | ValueNone -> copy
    | ValueSome (struct (_, table)) ->
        let lookupList = table + readAt copy (table + 8) 2
        for lookupIndex in 0 .. readAt copy lookupList 2 - 1 do
            let lookup = lookupList + readAt copy (lookupList + 2 + lookupIndex * 2) 2
            match readAt copy lookup 2 with
            | ContextType | ChainType -> writeAt copy lookup 2 0
            | ExtensionType ->
                for subtable in 0 .. readAt copy (lookup + 4) 2 - 1 do
                    let extension = lookup + readAt copy (lookup + 6 + subtable * 2) 2
                    match readAt copy (extension + 2) 2 with
                    | ContextType | ChainType -> writeAt copy (extension + 2) 2 0
                    | _ -> ()
            | _ -> ()
        copy

/// <summary>The path of the copy of a font file without its contexts, written once.</summary>
/// <param name="path">The font file.</param>
/// <returns>The copy's path.</returns>
let private barePath (path: string) =
    bareFiles.GetOrAdd(path, fun path ->
        let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-context-diff")
        Directory.CreateDirectory directory |> ignore
        let copy = Path.Combine(directory, $"{abs (path.GetHashCode())}-{Path.GetFileName path}")
        File.WriteAllBytes(copy, withoutContexts (OracleFaces.data path))
        copy)

let private rulePaths = ConcurrentDictionary<string, uint32 array array>()

/// <summary>The glyph sequences the rules of the context lookups of a face match, mapped back to the code points that produce
/// them: the glyph of a coverage or a class, then the glyphs a rule asks for, with the backtrack before and the lookahead after
/// for a chained rule, so that a text built from them reaches the rules.</summary>
/// <param name="font">The test font.</param>
/// <returns>The code point sequences of the rules whose glyphs the default shaper reaches.</returns>
let private ruleTexts (font: TestFont) =
    rulePaths.GetOrAdd($"{font.Path}#{font.Index}", fun _ ->
        let ourFont = Font.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))
        let struct (bases, marks) = alphabetOf font
        let codepointOf = Collections.Generic.Dictionary<uint32, uint32>()
        for codepoint in Array.append bases marks do
            match Font.nominalGlyph ourFont codepoint with
            | ValueSome glyph -> codepointOf[glyph] <- codepoint
            | ValueNone -> ()
        let first (coverage: Coverage) = Coverage.glyphs coverage |> Seq.tryHead
        let ofClass (classDef: ClassDef) (klass: uint32) =
            match ClassDef.glyphsInClass classDef klass |> Seq.tryHead with
            | Some range -> Some range.First
            | None -> None
        let classMember (coverage: Coverage) (classDef: ClassDef) (klass: int) =
            Coverage.glyphs coverage |> Seq.tryFind (fun glyph -> int (ClassDef.classOf classDef glyph) = klass)
        let sequences = ResizeArray<uint32 option list>()
        for lookup in (gposOf font).Lookups do
            for accel in lookup.Value.Subtables do
                match accel.Subtable with
                | PosContext (GlyphRules (coverage, ruleSets)) ->
                    let glyphs = Coverage.glyphs coverage
                    for setIndex in 0 .. min ruleSets.Length glyphs.Length - 1 do
                        for rule in ruleSets[setIndex] do
                            sequences.Add (Some glyphs[setIndex] :: (rule.Input |> Seq.map Some |> Seq.toList))
                | PosContext (ClassRules (coverage, classDef, ruleSets, _)) ->
                    for setIndex in 0 .. ruleSets.Length - 1 do
                        for rule in ruleSets[setIndex] do
                            sequences.Add (classMember coverage classDef setIndex :: (rule.Input |> Seq.map (ofClass classDef) |> Seq.toList))
                | PosContext (CoverageRule (coverages, _)) -> sequences.Add (coverages |> Seq.map first |> Seq.toList)
                | PosChainContext (ChainGlyphRules (coverage, ruleSets)) ->
                    let glyphs = Coverage.glyphs coverage
                    for setIndex in 0 .. min ruleSets.Length glyphs.Length - 1 do
                        for rule in ruleSets[setIndex] do
                            sequences.Add ((rule.Backtrack |> Seq.rev |> Seq.map Some |> Seq.toList) @ (Some glyphs[setIndex] :: (rule.ChainInput |> Seq.map Some |> Seq.toList)) @ (rule.Lookahead |> Seq.map Some |> Seq.toList))
                | PosChainContext (ChainClassRules (coverage, backtrackClasses, inputClasses, lookaheadClasses, ruleSets, _)) ->
                    for setIndex in 0 .. ruleSets.Length - 1 do
                        for rule in ruleSets[setIndex] do
                            sequences.Add ((rule.Backtrack |> Seq.rev |> Seq.map (ofClass backtrackClasses) |> Seq.toList) @ (classMember coverage inputClasses setIndex :: (rule.ChainInput |> Seq.map (ofClass inputClasses) |> Seq.toList)) @ (rule.Lookahead |> Seq.map (ofClass lookaheadClasses) |> Seq.toList))
                | PosChainContext (ChainCoverageRule (backtrack, input, lookahead, _)) ->
                    sequences.Add ((backtrack |> Seq.rev |> Seq.map first |> Seq.toList) @ (input |> Seq.map first |> Seq.toList) @ (lookahead |> Seq.map first |> Seq.toList))
                | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosPending _ -> ()
        sequences
        |> Seq.truncate 600
        |> Seq.choose (fun glyphs ->
            let codepoints = glyphs |> List.map (fun glyph -> glyph |> Option.bind (fun id -> match codepointOf.TryGetValue id with | true, codepoint -> Some codepoint | false, _ -> None))
            match List.forall Option.isSome codepoints && not codepoints.IsEmpty with
            | true -> Some (codepoints |> List.map Option.get |> List.toArray)
            | false -> None)
        |> Seq.toArray)

/// <summary>Generates a request on one of the faces, most of them with a text built around the sequence of a rule of the face's
/// context lookups, so that the lookups match, and the others plain.</summary>
/// <param name="fonts">The faces.</param>
/// <param name="maxLength">The longest plain stretch of text.</param>
/// <returns>The generator of the request.</returns>
let private biased (fonts: TestFont array) (maxLength: int) =
    gen {
        let! font = Gen.elements fonts
        let sequences = ruleTexts font
        let! text =
            match sequences.Length with
            | 0 -> textOf font maxLength
            | _ ->
                Gen.frequency [
                    1, textOf font maxLength
                    4, gen {
                        let! rule = Gen.elements sequences
                        let! before = textOf font 3
                        let! after = textOf font 3
                        return Array.concat [ before; rule; after ] } ]
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
let private contextual = ref 0

/// <summary>The geometry of a shaped run: its glyphs, clusters, advances and offsets, without the glyph flags.</summary>
/// <param name="shaped">The shaped glyphs.</param>
/// <returns>The fields.</returns>
let private geometry (shaped: Shaped) =
    shaped |> Array.map (fun (struct (glyph, cluster, xAdvance, yAdvance, xOffset, yOffset, _)) -> struct (glyph, cluster, xAdvance, yAdvance, xOffset, yOffset))

/// <summary>Whether a shaped run is placed differently from the run shaped without the contexts of the face.</summary>
/// <param name="expected">HarfBuzz's answer.</param>
/// <param name="without">HarfBuzz's answer on the font without its contexts.</param>
/// <returns>True when the glyphs are placed differently.</returns>
let private placedDifferently (expected: Result<Shaped, string>) (without: Result<Shaped, string>) =
    match expected, without with
    | Ok shaped, Ok unchanged -> geometry shaped <> geometry unchanged
    | _ -> true

/// <summary>Whether the contexts of the face made the shaped run placed differently from the run without them.</summary>
/// <param name="request">The request.</param>
/// <param name="expected">HarfBuzz's answer.</param>
/// <returns>True when the answers differ.</returns>
let private contextChanged (request: GposRequest) (expected: Result<Shaped, string>) =
    match expected with
    | Error _ -> false
    | Ok _ -> placedDifferently expected (oracleShape { request with Path = barePath request.Path; Cached = true })

/// <summary>Compares the two engines on a request and counts it, and the requests the contexts changed.</summary>
/// <param name="request">The request.</param>
/// <returns>The two answers.</returns>
let private shapeBoth (request: GposRequest) =
    let ours = ourShape request
    let expected = oracleShape request
    let changed = contextChanged request expected
    lock compared (fun () ->
        compared.Value <- compared.Value + 1
        match changed with
        | true -> contextual.Value <- contextual.Value + 1
        | false -> ())
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

/// <summary>The records of a rule: the sequence index and lookup index of each.</summary>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private recordBytes (records: (int * int) list) =
    Array.concat (records |> List.map (fun (sequence, lookup) -> Array.append (be16 sequence) (be16 lookup)))

/// <summary>Lays out blocks after a header, giving the offset each starts at.</summary>
/// <param name="header">The header size.</param>
/// <param name="blocks">The blocks.</param>
/// <returns>The offsets, one per block.</returns>
let private offsetsOf (header: int) (blocks: byte array list) =
    blocks |> List.scan (fun at block -> at + block.Length) header |> List.take blocks.Length

/// <summary>A context subtable of the first format: a rule set for each covered glyph.</summary>
/// <param name="covered">The covered glyphs.</param>
/// <param name="ruleSets">For each covered glyph its rules, each the input after the first glyph and the records.</param>
/// <returns>The bytes.</returns>
let private context1 (covered: int list) (ruleSets: (int list * (int * int) list) list list) =
    let rule (input: int list, records: (int * int) list) =
        Array.concat [ be16 (input.Length + 1); be16 records.Length; Array.concat (input |> List.map be16); recordBytes records ]
    let set (rules: (int list * (int * int) list) list) =
        let bodies = rules |> List.map rule
        let header = 2 + 2 * bodies.Length
        Array.concat [ be16 bodies.Length; Array.concat (offsetsOf header bodies |> List.map be16); Array.concat bodies ]
    let sets = ruleSets |> List.map set
    let coverage = coverageOf covered
    let header = 6 + 2 * sets.Length
    let setOffsets = offsetsOf (header + coverage.Length) sets
    Array.concat [ be16 1; be16 header; be16 sets.Length; Array.concat (setOffsets |> List.map be16); coverage; Array.concat sets ]

/// <summary>A context subtable of the third format.</summary>
/// <param name="coverages">The glyphs each input position covers.</param>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private context3 (coverages: int list list) (records: (int * int) list) =
    let bodies = coverages |> List.map coverageOf
    let header = 6 + 2 * bodies.Length + 4 * records.Length
    Array.concat [ be16 3; be16 bodies.Length; be16 records.Length; Array.concat (offsetsOf header bodies |> List.map be16); recordBytes records; Array.concat bodies ]

/// <summary>A chained-context subtable of the first format: a rule set for each covered glyph.</summary>
/// <param name="covered">The covered glyphs.</param>
/// <param name="ruleSets">For each covered glyph its rules, each the backtrack, the input after the first glyph, the lookahead and the records.</param>
/// <returns>The bytes.</returns>
let private chain1 (covered: int list) (ruleSets: (int list * int list * int list * (int * int) list) list list) =
    let rule (backtrack: int list, input: int list, lookahead: int list, records: (int * int) list) =
        Array.concat [ be16 backtrack.Length; Array.concat (backtrack |> List.map be16); be16 (input.Length + 1); Array.concat (input |> List.map be16); be16 lookahead.Length; Array.concat (lookahead |> List.map be16); be16 records.Length; recordBytes records ]
    let set (rules: (int list * int list * int list * (int * int) list) list) =
        let bodies = rules |> List.map rule
        let header = 2 + 2 * bodies.Length
        Array.concat [ be16 bodies.Length; Array.concat (offsetsOf header bodies |> List.map be16); Array.concat bodies ]
    let sets = ruleSets |> List.map set
    let coverage = coverageOf covered
    let header = 6 + 2 * sets.Length
    let setOffsets = offsetsOf (header + coverage.Length) sets
    Array.concat [ be16 1; be16 header; be16 sets.Length; Array.concat (setOffsets |> List.map be16); coverage; Array.concat sets ]

/// <summary>A chained-context subtable of the third format.</summary>
/// <param name="backtrack">The glyphs each backtrack position covers, nearest first.</param>
/// <param name="input">The glyphs each input position covers.</param>
/// <param name="lookahead">The glyphs each lookahead position covers.</param>
/// <param name="records">The records.</param>
/// <returns>The bytes.</returns>
let private chain3 (backtrack: int list list) (input: int list list) (lookahead: int list list) (records: (int * int) list) =
    let bodies = (backtrack @ input @ lookahead) |> List.map coverageOf
    let header = 2 + 2 + 2 * backtrack.Length + 2 + 2 * input.Length + 2 + 2 * lookahead.Length + 2 + 4 * records.Length
    let offsets = offsetsOf header bodies |> List.map be16
    let backtrackOffsets = offsets |> List.take backtrack.Length
    let inputOffsets = offsets |> List.skip backtrack.Length |> List.take input.Length
    let lookaheadOffsets = offsets |> List.skip (backtrack.Length + input.Length)
    Array.concat [ be16 3; be16 backtrack.Length; Array.concat backtrackOffsets; be16 input.Length; Array.concat inputOffsets; be16 lookahead.Length; Array.concat lookaheadOffsets; be16 records.Length; recordBytes records; Array.concat bodies ]

/// <summary>A single adjustment lookup that adds a value to the horizontal advance of the covered glyphs, format 1.</summary>
/// <param name="glyphs">The covered glyphs.</param>
/// <param name="value">The value, in font units.</param>
/// <returns>The lookup.</returns>
let private singleAdvance (glyphs: int list) (value: int) =
    lookupOf 1 (Array.concat [ be16 1; be16 8; be16 4; be16 (value &&& 0xFFFF); coverageOf glyphs ])

/// <summary>A lookup of one subtable wrapped in an extension, lookup type 9.</summary>
/// <param name="innerType">The type of the wrapped subtable.</param>
/// <param name="subtable">The wrapped subtable.</param>
/// <returns>The lookup.</returns>
let private extended (innerType: int) (subtable: byte array) =
    lookupOf ExtensionType (Array.concat [ be16 1; be16 innerType; be32 8; subtable ])

/// <summary>A GPOS table whose one feature <c>test</c> applies every lookup, in the layout of the GSUB table of that name.</summary>
/// <param name="lookups">The lookups.</param>
/// <returns>The table bytes.</returns>
let private gposTable (lookups: byte array list) = gsubTable lookups

/// <summary>The font file with its GPOS replaced.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="lookups">The lookups.</param>
/// <returns>The new file.</returns>
let private withLookups (bytes: byte array) (lookups: byte array list) = withTable (int (uint32 gposBytes)) bytes (gposTable lookups)

/// <summary>The fields of the context, chained-context and extension subtables of a font file's GPOS.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The fields.</returns>
let private gposContextFields (bytes: byte array) = contextFieldsOf gposBytes ExtensionType ContextType ChainType (-1) bytes

/// <summary>The differentials of P5.S12: GPOS contexts, chained contexts and extensions against HarfBuzz.</summary>
/// <returns>The test list.</returns>
let tests () =
    let fonts = contextFonts.Value
    let aotsOf (marker: string) = fonts |> Array.filter (fun font -> font.Path.Contains marker)
    let aots = fonts |> Array.filter (fun font -> font.Path.Contains "aots")
    let real = fonts |> Array.filter (fun font -> not (font.Path.Contains "aots"))
    let probes = ConcurrentDictionary<string, uint32 array array>()
    let probeTexts (font: TestFont) = probes.GetOrAdd($"{font.Path}#{font.Index}", fun _ -> Gen.sampleWithSeed (Rnd 91UL) 20 40 (biased [| font |] 12 |> Gen.map (fun request -> request.Text)))
    let featuresOf (font: TestFont) = match font.Path.Contains "aots" with | true -> "test" | false -> ""
    let sources = fonts |> Array.filter (fun font -> ValueOption.isSome (gposRecord (OracleFaces.data font.Path)))
    testSequenced <| testList "P5.S12" [
        testCase "P5.S12 the corpus has GPOS context faces" (fun () ->
            let candidates = contextCandidates.Value
            let complexOnly = candidates |> Array.filter (fun font -> usableCount font = 0)
            let pendingSubstitution = candidates |> Array.filter hasPendingSubstitution
            let pendingPositioning = candidates |> Array.filter hasPendingPositioning
            printfn "P5.S12 faces: %d with a GPOS context or chained-context subtable; %d left out for having no code point the default shaper takes (they need an unported complex shaper: %s); %d with a GSUB and %d with a GPOS subtable not ported; %d used, %d aots and %d real" candidates.Length complexOnly.Length (String.Join(", ", complexOnly |> Array.map (fun font -> Path.GetFileName font.Path))) pendingSubstitution.Length pendingPositioning.Length fonts.Length aots.Length real.Length
            for font in fonts do
                let subtables = (gposOf font).Lookups |> Seq.collect (fun lookup -> lookup.Value.Subtables |> Seq.map (fun accel -> accel.Subtable)) |> Seq.toArray
                let count predicate = subtables |> Array.filter predicate |> Array.length
                printfn "P5.S12 face %s#%d: %d context, %d chained-context subtables; %d code points the default shaper takes" (Path.GetFileName font.Path) font.Index (count (fun s -> match s with | PosContext _ -> true | _ -> false)) (count (fun s -> match s with | PosChainContext _ -> true | _ -> false)) (usableCount font)
            Expect.isGreaterThan aots.Length 20 "aots context and chaining faces"
            Expect.isGreaterThan real.Length 0 "real faces with GPOS contexts")
        versus "P5.S12 aots gpos7 and gpos9 shaping" (biased (Array.append (aotsOf "gpos7_") (aotsOf "gpos9")) 12) 2000
        versus "P5.S12 aots gpos_context shaping" (biased (aotsOf "gpos_context") 12) 4000
        versus "P5.S12 aots gpos_chaining shaping" (biased (aotsOf "gpos_chaining") 12) 4000
        versus "P5.S12 real faces, base and mark text" (biased real 30) 8000
        testCase "P5.S12 the unchanged faces are accepted and their contexts move glyphs" (fun () ->
            let mutable accepted = 0
            let mutable moving = 0
            let mutable movingReal = 0
            for font in fonts do
                let struct (oracleCount, ourCount) = lookupCounts font.Path
                Expect.equal ourCount oracleCount $"{Path.GetFileName font.Path}: the same lookup count"
                Expect.isGreaterThan oracleCount 0 $"{Path.GetFileName font.Path}: accepted"
                accepted <- accepted + 1
                let moves =
                    probeTexts font
                    |> Seq.exists (fun text ->
                        let request = { Path = font.Path; Cached = true; Index = font.Index; Text = text; Features = featuresOf font; Direction = "ltr"; Flags = 0u; ClusterLevel = 0; Ppem = 0u }
                        contextChanged request (oracleShape request))
                match moves with
                | true ->
                    moving <- moving + 1
                    if not (font.Path.Contains "aots") then movingReal <- movingReal + 1
                | false -> ()
            printfn "P5.S12 unchanged faces: %d accepted by both; the contexts change the shaped run on some probe text of %d of them, %d of the real faces" accepted moving movingReal
            Expect.isGreaterThan moving 20 "the contexts of many faces change the shaped run")
        testCase "P5.S12 mutated context, chained-context and extension subtables" (fun () ->
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-context-mutation")
            Directory.CreateDirectory directory |> ignore
            let candidates =
                sources
                |> Array.map (fun font ->
                    let length = (OracleFaces.data font.Path).Length
                    font, gposContextFields (OracleFaces.data font.Path) |> Array.filter (fun (_, offset, width, _) -> offset >= 0 && offset + width <= length))
                |> Array.filter (fun (_, fields) -> fields.Length > 0)
            let failures = ResizeArray<string>()
            let mutable state = 20261114u
            let nextRandom () =
                state <- state * 1103515245u + 12345u
                state >>> 8
            let mutable cases = 0
            let mutable rejected = 0
            let mutable rejectedByUs = 0
            let mutable accepted = 0
            let mutable moving = 0
            let mutable truncated = 0
            let mutable attempt = 0
            while cases < 5000 && attempt < 100000 do
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
                let request = { Path = path; Cached = false; Index = font.Index; Text = text; Features = featuresOf font; Direction = (match nextRandom () % 5u with | 0u -> "rtl" | _ -> "ltr"); Flags = (match nextRandom () % 3u with | 0u -> 64u | _ -> 0u); ClusterLevel = int (nextRandom () % 4u); Ppem = (match nextRandom () % 4u with | 0u -> 12u | _ -> 0u) }
                let ours = ourShape request
                let expected = oracleShape request
                let struct (oracleCount, ourCount) = lookupCounts path
                if oracleCount = 0 then rejected <- rejected + 1
                if ourCount = 0 then rejectedByUs <- rejectedByUs + 1
                if oracleCount > 0 then
                    accepted <- accepted + 1
                    let bare = Path.Combine(directory, $"bare-{cases}.ttf")
                    File.WriteAllBytes(bare, withoutContexts candidate)
                    if placedDifferently expected (oracleShape { request with Path = bare }) then moving <- moving + 1
                    File.Delete bare
                if oracleCount <> ourCount then failures.Add $"{Path.GetFileName font.Path}: {name}: lookup count oracle {oracleCount}, farfbuzz {ourCount}"
                elif ours <> expected then failures.Add $"{Path.GetFileName font.Path}: {name}: shaped differently: farfbuzz %A{ours}, oracle %A{expected}"
                File.Delete path
                cases <- cases + 1
            printfn "P5.S12 compared %d changed-bytes cases (%d with the GPOS length cut): HarfBuzz rejected the table in %d, Farfbuzz in %d, the same ones; %d accepted, of which %d still place glyphs differently from the font without its contexts" cases truncated rejected rejectedByUs accepted moving
            Expect.isGreaterThanOrEqual cases 5000 "the number of cases"
            Expect.isGreaterThan rejected 0 "some changes make HarfBuzz reject the table"
            Expect.equal rejectedByUs rejected "Farfbuzz rejects as many tables as HarfBuzz"
            Expect.isGreaterThan moving 300 "many accepted changed tables still apply their contexts"
            Expect.isEmpty (failures |> Seq.truncate 20 |> Seq.toList) ("changed-bytes differences:\n" + String.concat "\n" (failures |> Seq.truncate 20)))
        testCase "P5.S12 nesting and operation limits against HarfBuzz" (fun () ->
            let font = aots |> Array.find (fun font -> font.Path.Contains "gpos_context1_simple_f1")
            let ourFont = Font.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))
            let struct (bases, _) = alphabetOf font
            let codepoints = bases |> Array.truncate 3
            let glyph (codepoint: uint32) = int (Font.nominalGlyph ourFont codepoint |> ValueOption.get)
            let first = glyph codepoints[0]
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-context-limits")
            Directory.CreateDirectory directory |> ignore
            let failures = ResizeArray<string>()
            let mutable compared = 0
            let compare (name: string) (lookups: byte array list) (text: uint32 array) =
                let path = Path.Combine(directory, "limits.ttf")
                File.WriteAllBytes(path, withLookups (OracleFaces.data font.Path) lookups)
                let request = { Path = path; Cached = false; Index = font.Index; Text = text; Features = "test"; Direction = "ltr"; Flags = 0u; ClusterLevel = 0; Ppem = 0u }
                let ours = ourShape request
                let expected = oracleShape request
                compared <- compared + 1
                (match ours = expected with
                 | true -> ()
                 | false -> failures.Add $"{name}: farfbuzz %A{ours}, oracle %A{expected}")
                let bare = Path.Combine(directory, "bare.ttf")
                File.WriteAllBytes(bare, withoutContexts (File.ReadAllBytes path))
                struct (placedDifferently expected (oracleShape { request with Path = bare }), expected)
            let advance = singleAdvance [ first ] 100
            let outcomes = ResizeArray<string * bool * bool>()
            let run (name: string) (lookups: byte array list) (text: uint32 array) =
                let struct (placed, expected) = compare name lookups text
                outcomes.Add((name, placed, (match expected with | Ok _ -> true | Error _ -> false)))
            let context (records: (int * int) list) = lookupOf ContextType (context3 [ [ first ] ] records)
            let text = [| codepoints[0]; codepoints[1] |]
            run "a lookup that recurses into itself" [ context [ 0, 0 ]; advance ] text
            for depth in [ 1; 2; 62; 63; 64; 65; 66; 70 ] do
                let chain = [ for index in 0 .. depth - 1 -> context [ 0, index + 1 ] ]
                run $"a chain of {depth} nested lookups" (chain @ [ advance ]) text
            for count in [ 1; 1000; 16000; 16370; 16371; 16372; 16373; 16374; 16375; 16376; 16377; 16378; 16379; 16380; 16383; 16384; 16385; 16386; 16390; 16400; 20000 ] do
                run $"{count} records in one rule" [ context (List.replicate count (0, 1)); advance ] text
            for count in [ 8000; 8190; 8191; 8192; 8193; 8200 ] do
                run $"{count} records in one rule, two glyphs" [ context (List.replicate count (0, 1)); advance ] [| codepoints[0]; codepoints[0]; codepoints[1] |]
            run "a chained-context lookup that recurses into itself" [ lookupOf ChainType (chain3 [] [ [ first ] ] [] [ 0, 0 ]); advance ] text
            run "an extension that wraps a context lookup" [ extended ContextType (context3 [ [ first ] ] [ 0, 1 ]); advance ] text
            run "an extension that wraps a chained-context lookup" [ extended ChainType (chain3 [] [ [ first ] ] [] [ 0, 1 ]); advance ] text
            printfn "P5.S12 limits: %d tables compared with HarfBuzz; the ones whose glyphs HarfBuzz places differently from the font without its contexts: %s" compared (String.concat "; " (outcomes |> Seq.filter (fun (_, placed, shaped) -> placed && shaped) |> Seq.map (fun (name, _, _) -> name))) |> ignore
            printfn "P5.S12 limits: HarfBuzz fails the shaping of: %s" (String.concat "; " (outcomes |> Seq.filter (fun (_, _, shaped) -> not shaped) |> Seq.map (fun (name, _, _) -> name)))
            Expect.isEmpty failures ("limit differences:\n" + String.concat "\n" failures)
            Expect.equal compared 39 "every table of the limits is compared"
            Expect.isGreaterThan (outcomes |> Seq.filter (fun (_, placed, shaped) -> placed && shaped) |> Seq.length) 20 "most tables place glyphs differently from the font without contexts (the lookups run at the top level too, so the nesting limit itself is pinned by the unit test against hand-derived values)")
        testCase "P5.S12 random context tables, glyph, coverage and chained rules, nested lookups and extensions" (fun () ->
            let font = aots |> Array.find (fun font -> font.Path.Contains "gpos_context1_simple_f1")
            let ourFont = Font.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))
            let struct (bases, _) = alphabetOf font
            let codepoints = bases |> Array.truncate 5
            let glyphs = codepoints |> Array.map (fun codepoint -> int (Font.nominalGlyph ourFont codepoint |> ValueOption.get))
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-context-synthetic")
            Directory.CreateDirectory directory |> ignore
            let path = Path.Combine(directory, "synthetic.ttf")
            let failures = ResizeArray<string>()
            let mutable state = 20261115u
            let nextRandom (bound: int) =
                state <- state * 1103515245u + 12345u
                int ((state >>> 8) % uint32 bound)
            let subset () =
                let picked = glyphs |> Array.filter (fun _ -> nextRandom 2 = 0)
                match picked.Length with
                | 0 -> [ glyphs[nextRandom glyphs.Length] ]
                | _ -> picked |> Array.sort |> Array.toList
            let sequence (maximum: int) = List.init (nextRandom (maximum + 1)) (fun _ -> subset ())
            let glyphList (maximum: int) = List.init (nextRandom (maximum + 1)) (fun _ -> glyphs[nextRandom glyphs.Length])
            let mutable compared = 0
            let mutable moving = 0
            let mutable flagged = 0
            for case in 1 .. 3000 do
                let total = 3 + nextRandom 4
                let records () = List.init (1 + nextRandom 3) (fun _ -> nextRandom 4, nextRandom total)
                let nested () =
                    match nextRandom 4 with
                    | 0 | 1 -> singleAdvance (subset ()) (1 + nextRandom 300)
                    | 2 -> lookupOf ContextType (context3 (sequence 3 |> fun rest -> subset () :: rest) (records ()))
                    | _ -> lookupOf ChainType (chain3 (sequence 2) (subset () :: sequence 2) (sequence 2) (records ()))
                let outer () =
                    let wrap kind body =
                        match nextRandom 4 with
                        | 0 -> extended kind body
                        | _ -> lookupOf kind body
                    match nextRandom 5 with
                    | 0 ->
                        let covered = subset ()
                        wrap ContextType (context1 covered (covered |> List.map (fun _ -> List.init (1 + nextRandom 3) (fun _ -> glyphList 3, records ()))))
                    | 1 -> wrap ContextType (context3 (subset () :: sequence 3) (records ()))
                    | 2 ->
                        let covered = subset ()
                        wrap ChainType (chain1 covered (covered |> List.map (fun _ -> List.init (1 + nextRandom 3) (fun _ -> glyphList 2, glyphList 2, glyphList 2, records ()))))
                    | _ -> wrap ChainType (chain3 (sequence 2) (subset () :: sequence 2) (sequence 2) (records ()))
                let lookups = List.init total (fun index -> match index < 2 with | true -> singleAdvance (subset ()) (1 + nextRandom 300) | false -> (match nextRandom 3 with | 0 -> nested () | _ -> outer ()))
                let bytes = withLookups (OracleFaces.data font.Path) lookups
                File.WriteAllBytes(path, bytes)
                let text = Array.init (1 + nextRandom 9) (fun _ -> codepoints[nextRandom codepoints.Length])
                let request = { Path = path; Cached = false; Index = font.Index; Text = text; Features = "test"; Direction = ([| "ltr"; "ltr"; "rtl" |][nextRandom 3]); Flags = ([| 64u; 0u; 64u |][nextRandom 3]); ClusterLevel = nextRandom 4; Ppem = 0u }
                let ours = ourShape request
                let expected = oracleShape request
                compared <- compared + 1
                (match expected with
                 | Ok shaped ->
                    if shaped |> Array.exists (fun (struct (_, _, _, _, _, _, flag)) -> (flag &&& 2u) <> 0u) then flagged <- flagged + 1
                    File.WriteAllBytes(path, withoutContexts bytes)
                    if placedDifferently expected (oracleShape request) then moving <- moving + 1
                 | Error _ -> ())
                File.WriteAllBytes(path, bytes)
                match ours = expected with
                | true -> ()
                | false -> failures.Add $"random case {case}: text %A{text}, direction {request.Direction}, flags {request.Flags}, level {request.ClusterLevel}: farfbuzz %A{ours}, oracle %A{expected}"
            printfn "P5.S12 compared %d random context tables with HarfBuzz: %d with an unsafe-to-concat glyph, %d whose contexts change the shaped run" compared flagged moving
            Expect.isEmpty (failures |> Seq.truncate 20 |> Seq.toList) ($"random table differences ({failures.Count} in all):\n" + String.concat "\n" (failures |> Seq.truncate 20))
            Expect.isGreaterThan moving 300 "many random tables apply their contexts"
            Expect.isGreaterThan flagged 300 "the random tables reach the unsafe-to-concat marks")
        testCase "P5.S12 report" (fun () ->
            printfn "P5.S12 shaped %d requests against HarfBuzz, %d of which the GPOS contexts changed" compared.Value contextual.Value
            Expect.isGreaterThan contextual.Value 0 "some requests were changed by a context")
    ]
