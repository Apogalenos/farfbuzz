module Farfbuzz.Diff.GposDiff

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Oracle.NativeText
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>One shaping request of the GPOS differentials: the face's bytes, the text and the buffer and font settings.</summary>
type GposRequest =
    { Path: string
      Cached: bool // false for a mutated face, which is loaded afresh
      Index: int
      Text: uint32 array
      Features: string
      Direction: string
      Flags: uint32
      ClusterLevel: int
      Ppem: uint32 }

/// <summary>What the comparison looks at: glyph, cluster, advances, offsets and the defined glyph flags.</summary>
type Shaped = (struct (uint32 * uint32 * int * int * int * int * uint32)) array

let private ourFaces = ConcurrentDictionary<struct (string * int), Face>()
let private oracleFaces = ConcurrentDictionary<struct (string * int), HarfBuzzSharp.Face>()

/// <summary>Parses a features argument.</summary>
/// <param name="text">The argument, empty for none.</param>
/// <returns>The features.</returns>
let private ourFeatures (text: string) =
    match text.Length with
    | 0 -> ImmutableArray<Feature>.Empty
    | _ ->
        text.Split([| ','; ' ' |])
        |> Array.choose (fun item -> match Feature.parse item with | Ok feature -> Some feature | Error _ -> None)
        |> ImmutableArray.CreateRange

/// <summary>Loads the face a request names, once for a face that stays as it is.</summary>
/// <param name="request">The request.</param>
/// <returns>The face.</returns>
let private ourFace (request: GposRequest) =
    match request.Cached with
    | false -> Face.create (ReadOnlyMemory (File.ReadAllBytes request.Path)) (uint32 request.Index)
    | true -> ourFaces.GetOrAdd(struct (request.Path, request.Index), fun _ -> Face.create (ReadOnlyMemory (OracleFaces.data request.Path)) (uint32 request.Index))

/// <summary>Shapes a request with Farfbuzz, through the whole pipeline of <c>Shape.shapeFull</c>.</summary>
/// <param name="request">The request.</param>
/// <returns>The shaped glyphs, or the reason shaping failed.</returns>
let ourShape (request: GposRequest) : Result<Shaped, string> =
    let font = Font.create (ourFace request) |> Font.withPpem request.Ppem request.Ppem
    let run = GlyphRun.addCodepoints (ReadOnlySpan request.Text) 0 request.Text.Length GlyphRun.empty
    let flags = LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.BufferFlags> request.Flags
    let level =
        match request.ClusterLevel with
        | 1 -> ClusterLevel.MonotoneCharacters
        | 2 -> ClusterLevel.Characters
        | 3 -> ClusterLevel.Graphemes
        | _ -> ClusterLevel.MonotoneGraphemes
    let run =
        run
        |> GlyphRun.withSegment { Direction = Direction.ofString request.Direction; Script = Scripts.Invalid; Language = Language.ofString "c" }
        |> GlyphRun.withFlags flags
        |> GlyphRun.withClusterLevel level
        |> GlyphRun.guessSegmentProperties Language.cLocale
    match Shape.shapeFull font (ourFeatures request.Features) (ValueSome (ImmutableArray.Create "ot")) run with
    | Error error -> Error $"%A{error}"
    | Ok result ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined))
                (result.Infos.AsSpan().ToArray()) (result.Positions.AsSpan().ToArray()))

/// <summary>Shapes a request with HarfBuzz, configured as hb-shape configures it.</summary>
/// <param name="request">The request.</param>
/// <returns>The shaped glyphs, or the reason shaping failed.</returns>
let oracleShape (request: GposRequest) : Result<Shaped, string> =
    use blob = match request.Cached with | false -> HarfBuzzSharp.Blob.FromFile request.Path | true -> null
    let face =
        match request.Cached with
        | false -> new HarfBuzzSharp.Face(blob, request.Index)
        | true -> oracleFaces.GetOrAdd(struct (request.Path, request.Index), fun _ -> new HarfBuzzSharp.Face(HarfBuzzSharp.Blob.FromFile request.Path, request.Index))
    use font = new HarfBuzzSharp.Font(face)
    let upem = int (Native.hb_face_get_upem face.Handle)
    Native.hb_font_set_scale(font.Handle, upem, upem)
    Native.hb_font_set_ppem(font.Handle, request.Ppem, request.Ppem)
    Native.hb_font_set_funcs_using(font.Handle, "ot") |> ignore
    use buffer = new HarfBuzzSharp.Buffer()
    Native.hb_buffer_add_codepoints(buffer.Handle, request.Text, request.Text.Length, 0u, request.Text.Length)
    let direction = Encoding.UTF8.GetBytes request.Direction
    Native.hb_buffer_set_direction(buffer.Handle, Native.hb_direction_from_string(direction, direction.Length))
    let language = Encoding.UTF8.GetBytes "c"
    Native.hb_buffer_set_language(buffer.Handle, Native.hb_language_from_string(language, language.Length))
    Native.hb_buffer_set_flags(buffer.Handle, request.Flags)
    Native.hb_buffer_set_cluster_level(buffer.Handle, request.ClusterLevel)
    Native.hb_buffer_guess_segment_properties buffer.Handle
    let features =
        match request.Features.Length with
        | 0 -> [||]
        | _ -> request.Features.Split([| ','; ' ' |]) |> Array.choose (fun item -> featureFromString item |> ValueOption.toOption)
    let shapers = [| "ot"; Unchecked.defaultof<string> |]
    match Native.hb_shape_full(font.Handle, buffer.Handle, features, uint32 features.Length, shapers) with
    | 0 -> Error "shaping failed"
    | _ ->
        let mutable length = 0u
        let infos = copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(buffer.Handle, &length)) (int length)
        let positions = copyFrom<GlyphPosition> (Native.hb_buffer_get_glyph_positions(buffer.Handle, &length)) (int length)
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined)) infos positions)

/// <summary>Whether a face has a single or pair adjustment subtable.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when GPOS holds one.</returns>
let private hasPositioning (font: TestFont) =
    let table = (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gpos.Value
    table.Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ -> true
            | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosContext _ | PosChainContext _ | PosPending _ -> false))

/// <summary>Whether a face's GPOS holds a subtable that is not ported and so never applies: one of a format its type does not
/// have, which no corpus face has.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when GPOS has such a subtable.</returns>
let private hasPendingPositioning (font: TestFont) =
    let table = (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gpos.Value
    table.Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | PosPending _ -> true
            | SinglePos _ | PairPosGlyphs _ | PairPosClasses _ | CursivePos _ | MarkBasePos _ | MarkLigPos _ | MarkMarkPos _ | PosContext _ | PosChainContext _ -> false))

/// <summary>Whether a face's GSUB still holds a subtable of a format that is not ported, which would make the two engines
/// differ for reasons of GSUB.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when GSUB has such a subtable.</returns>
let private hasPendingSubstitution (font: TestFont) =
    let table = (OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index))).Gsub.Value
    table.Lookups
    |> Seq.exists (fun lookup ->
        lookup.Value.Subtables
        |> Seq.exists (fun accel ->
            match accel.Subtable with
            | SubstPending _ -> true
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ -> false))

/// <summary>The faces with a single or pair adjustment subtable and every GSUB subtable ported, read once.</summary>
let private positioningFonts = lazy (TestFonts.all () |> List.filter (fun font -> hasPositioning font && not (hasPendingSubstitution font) && not (hasPendingPositioning font)) |> List.toArray)

/// <summary>Whether a code point is one the default shaper takes straight to its glyph: no mark, no ignorable.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>True for a plain code point.</returns>
let private plain (codepoint: uint32) =
    let category = Unicode.generalCategory codepoint
    let isMark =
        match category with
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

/// <summary>The settings shared by every request of a differential: a direction, buffer flags, a cluster level, a ppem
/// and a feature setting.</summary>
/// <param name="isAots">Whether the face is one of the aots fonts, whose lookups hang on the test feature.</param>
/// <returns>The generator of the settings.</returns>
let private settings (isAots: bool) =
    gen {
        let! direction = Gen.frequency [ 6, Gen.constant "ltr"; 1, Gen.constant "rtl"; 1, Gen.constant "ttb" ]
        let! flags = Gen.elements [| 0u; 0u; 2u; 8u; 10u |]
        let! level = Gen.elements [| 0; 0; 1; 2; 3 |]
        let! ppem = Gen.frequency [ 3, Gen.constant 0u; 1, Gen.choose (6, 40) |> Gen.map uint32 ]
        let! features =
            match isAots with
            | true -> Gen.elements [| "test"; "test"; "test=0"; "-test"; "test[1:3]=0"; "" |]
            | false -> Gen.elements [| ""; ""; ""; "-kern"; "kern=0"; "kern[1:3]=0"; "kern[2:]=1"; "+kern" |]
        return struct (direction, flags, level, ppem, features)
    }

/// <summary>Generates a request on one of the faces from random plain text of its coverage.</summary>
/// <param name="fonts">The faces.</param>
/// <returns>The generator of the request.</returns>
let private request (fonts: TestFont array) =
    gen {
        let! font = Gen.elements fonts
        let coverage = plainCoverage font
        let! length = Gen.choose (1, 24)
        let! text = Gen.arrayOfLength length (Gen.elements coverage)
        let isAots = font.Path.Contains "aots"
        let! struct (direction, flags, level, ppem, features) = settings isAots
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

/// <summary>The kerned counters of a differential: the cases compared and those whose advances left the nominal ones.</summary>
let private cases = ref 0
let private kerned = ref 0

/// <summary>Whether a shaped run has an advance or offset that is not its glyph's nominal advance, a pair or a single
/// adjustment having applied.</summary>
/// <param name="request">The request.</param>
/// <param name="shaped">The shaped glyphs.</param>
/// <returns>True when a position was adjusted.</returns>
let private adjusted (request: GposRequest) (shaped: Shaped) =
    let font = Font.create (ourFace request)
    let infos = shaped |> Array.map (fun (struct (glyph, cluster, _, _, _, _, _)) -> { Codepoint = glyph; Mask = 0u; Cluster = cluster; Var1 = 0u; Var2 = 0u })
    let defaults = (Font.hAdvancePositions font (ReadOnlySpan infos)).AsSpan().ToArray()
    shaped
    |> Array.mapi (fun index (struct (_, _, xAdvance, yAdvance, xOffset, yOffset, _)) ->
        request.Direction = "ltr" && (xAdvance <> defaults[index].XAdvance || yAdvance <> 0 || xOffset <> 0 || yOffset <> 0))
    |> Array.exists id

/// <summary>Compares the two engines on a request and counts it.</summary>
/// <param name="request">The request.</param>
/// <returns>The comparison.</returns>
let private shapeBoth (request: GposRequest) =
    let ours = ourShape request
    let expected = oracleShape request
    lock cases (fun () ->
        cases.Value <- cases.Value + 1
        match ours, expected with
        | Ok shaped, Ok _ when adjusted request shaped -> kerned.Value <- kerned.Value + 1
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

/// <summary>Reads a text file's lines.</summary>
/// <param name="name">The text file name under upstream's perf texts.</param>
/// <returns>The non-empty lines.</returns>
let private lines (name: string) =
    File.ReadAllLines (Path.Combine(RepositoryPaths.upstream (), "perf", "texts", name))
    |> Array.filter (fun line -> line.Length > 0)

/// <summary>The GPOS table tag.</summary>
let private gposTag = Tag.make 'G' 'P' 'O' 'S'

/// <summary>Reads a big-endian value of a width.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="at">The offset.</param>
/// <param name="width">Two or four bytes.</param>
/// <returns>The value, zero beyond the bytes.</returns>
let private readAt (bytes: byte array) (at: int) (width: int) =
    match at >= 0 && at + width <= bytes.Length with
    | false -> 0
    | true -> Seq.fold (fun acc index -> (acc <<< 8) ||| int bytes[at + index]) 0 (seq { 0 .. width - 1 })

/// <summary>Where the GPOS table of a font file starts, or none.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The offset.</returns>
let private gposOffset (bytes: byte array) =
    let face = Face.create (ReadOnlyMemory bytes) 0u
    match Face.tryFindTable face gposTag with
    | ValueNone -> ValueNone
    | ValueSome record -> ValueSome (int (face.BaseOffset + record.Offset))

/// <summary>The single and pair adjustment subtables of a font file with the fields the mutation differential rewrites:
/// the name, absolute offset, width and sample values of each, an extension's inner subtable included.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The fields.</returns>
let private gposFields (bytes: byte array) =
    match gposOffset bytes with
    | ValueNone -> [||]
    | ValueSome table ->
        let lookupList = table + readAt bytes (table + 8) 2
        let lookups = min 64 (readAt bytes lookupList 2)
        [| for lookupIndex in 0 .. lookups - 1 do
            let lookup = lookupList + readAt bytes (lookupList + 2 + lookupIndex * 2) 2
            let lookupType = readAt bytes lookup 2
            for subtableIndex in 0 .. min 8 (readAt bytes (lookup + 4) 2) - 1 do
                let outer = lookup + readAt bytes (lookup + 6 + subtableIndex * 2) 2
                let kind, subtable =
                    match lookupType with
                    | 9 -> readAt bytes (outer + 2) 2, outer + readAt bytes (outer + 4) 4
                    | _ -> lookupType, outer
                let format = readAt bytes subtable 2
                let tag = $"lookup {lookupIndex} subtable {subtableIndex} type {kind} format {format}"
                if kind = 9 then yield $"{tag} extension type", outer + 2, 2, [| 0; 1; 2; 9; 0xFFFF |]
                if kind = 1 || kind = 2 then
                    yield $"{tag} format", subtable, 2, [| 0; 1; 2; 3; 4; 5; 0xFFFF |]
                    yield $"{tag} coverage offset", subtable + 2, 2, [| 0; 1; 2; 0xFFFF |]
                    let coverage = subtable + readAt bytes (subtable + 2) 2
                    yield $"{tag} coverage format", coverage, 2, [| 0; 1; 2; 3; 0xFFFF |]
                    yield $"{tag} coverage count", coverage + 2, 2, [| 0; 1; 2; 0x7FFF; 0xFFFF |]
                    yield $"{tag} value format 1", subtable + 4, 2, [| 0; 1; 2; 4; 8; 0x10; 0x44; 0xFF; 0xFFFF |]
                    if kind = 1 && format = 2 then yield $"{tag} value count", subtable + 6, 2, [| 0; 1; 2; 0xFFFF |]
                if kind = 2 then
                    yield $"{tag} value format 2", subtable + 6, 2, [| 0; 1; 4; 0x44; 0xFFFF |]
                    match format with
                    | 1 ->
                        yield $"{tag} pair set count", subtable + 8, 2, [| 0; 1; 0xFFFF |]
                        yield $"{tag} first pair set offset", subtable + 10, 2, [| 0; 1; 0xFFFF |]
                        let pairSet = subtable + readAt bytes (subtable + 10) 2
                        yield $"{tag} first pair set length", pairSet, 2, [| 0; 1; 2; 0xFFFF |]
                        yield $"{tag} first second glyph", pairSet + 2, 2, [| 0; 1; 0xFFFF |]
                    | 2 ->
                        yield $"{tag} class def 1 offset", subtable + 8, 2, [| 0; 1; 0xFFFF |]
                        yield $"{tag} class def 2 offset", subtable + 10, 2, [| 0; 1; 0xFFFF |]
                        yield $"{tag} class 1 count", subtable + 12, 2, [| 0; 1; 2; 0xFFFF |]
                        yield $"{tag} class 2 count", subtable + 14, 2, [| 0; 1; 2; 0xFFFF |]
                    | _ -> () |]

/// <summary>Writes a big-endian value of a width into a copy of the bytes.</summary>
/// <param name="bytes">The bytes, changed.</param>
/// <param name="at">The offset.</param>
/// <param name="width">Two or four bytes.</param>
/// <param name="value">The value.</param>
let private writeAt (bytes: byte array) (at: int) (width: int) (value: int) =
    for index in 0 .. width - 1 do
        bytes[at + index] <- byte (value >>> (8 * (width - 1 - index)))

/// <summary>The number of lookups a face's GPOS holds, which HarfBuzz reads as zero when it rejects the table.</summary>
/// <param name="path">The font file.</param>
/// <returns>HarfBuzz's count and Farfbuzz's.</returns>
let private lookupCounts (path: string) =
    use blob = HarfBuzzSharp.Blob.FromFile path
    use face = new HarfBuzzSharp.Face(blob, 0)
    let oracle = int (Native.hb_ot_layout_table_get_lookup_count(face.Handle, uint32 gposTag))
    let ours = LayoutTable.lookupCount (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u |> OtFace.create).Gpos.Value
    struct (oracle, ours)

/// <summary>The number of copies of its first lookup a widened face gets.</summary>
[<Literal>]
let private WideCopies = 70

/// <summary>Writes a big-endian 16-bit value as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: int) =
    [| byte (value >>> 8); byte value |]

/// <summary>Writes a copy of an aots face whose GPOS table has more than 64 lookups: the lookup list holds the face's lookups
/// and then copies of the first, and every feature that lists the first lookup lists the copies as well, so the map of a run
/// has more lookups than the first 64 positions the pair loops look at, and the lookups applied are the same pair adjustments.</summary>
/// <param name="font">The aots face.</param>
/// <returns>The widened face.</returns>
let private widened (font: TestFont) =
    let bytes = OracleFaces.data font.Path
    let face = Face.create (ReadOnlyMemory bytes) (uint32 font.Index)
    let located = Face.tryFindTable face gposTag
    let record = match located with | ValueSome found -> found | ValueNone -> failwith "the face has no GPOS"
    let table = int (face.BaseOffset + record.Offset)
    let gpos = Array.sub bytes table (int record.Length)
    let u16 (at: int) = readAt gpos at 2
    let scriptOffset, featureOffset, lookupOffset = u16 4, u16 6, u16 8
    let extentOf (offset: int) = [ scriptOffset; featureOffset; lookupOffset ] |> List.filter (fun other -> other > offset) |> List.tryHead |> Option.defaultValue gpos.Length |> fun next -> next - offset
    let scripts = Array.sub gpos scriptOffset (extentOf scriptOffset)
    let lookupCount = u16 lookupOffset
    let starts = Array.init lookupCount (fun index -> u16 (lookupOffset + 2 + 2 * index))
    let block (index: int) =
        let finish = starts |> Array.filter (fun start -> start > starts[index]) |> Array.sortBy id |> Array.tryHead |> Option.defaultValue (lookupOffset + extentOf lookupOffset - lookupOffset)
        Array.sub gpos (lookupOffset + starts[index]) (finish - starts[index])
    let blocks = Array.append (Array.init lookupCount block) (Array.create WideCopies (block 0))
    let offsets = Array.scan (fun total (piece: byte array) -> total + piece.Length) (2 + 2 * blocks.Length) blocks
    let lookups = Array.concat [ be16 blocks.Length; Array.concat (offsets |> Array.truncate blocks.Length |> Array.map be16); Array.concat blocks ]
    let featureCount = u16 featureOffset
    let features =
        Array.init featureCount (fun index ->
            let tag = Array.sub gpos (featureOffset + 2 + 6 * index) 4
            let at = featureOffset + u16 (featureOffset + 2 + 6 * index + 4)
            let indices = Array.init (u16 (at + 2)) (fun entry -> u16 (at + 4 + 2 * entry))
            let copies = Array.init WideCopies (fun copy -> lookupCount + copy)
            let widenedIndices = match Array.contains 0 indices with | true -> Array.append indices copies | false -> indices
            tag, Array.concat [ be16 0; be16 widenedIndices.Length; Array.concat (Array.map be16 widenedIndices) ])
    let featureStarts = Array.scan (fun total (_, bytesOf: byte array) -> total + bytesOf.Length) (2 + 6 * featureCount) features
    let featureList =
        Array.concat
            [ be16 featureCount
              Array.concat (features |> Array.mapi (fun index (tag, _) -> Array.append tag (be16 featureStarts[index])))
              Array.concat (features |> Array.map snd) ]
    let header = Array.concat [ Array.sub gpos 0 4; be16 10; be16 (10 + scripts.Length); be16 (10 + scripts.Length + featureList.Length) ]
    let widenedGpos = Array.concat [ header; scripts; featureList; lookups ]
    let padding = Array.zeroCreate<byte> ((4 - bytes.Length % 4) % 4)
    let file = Array.concat [ bytes; padding; widenedGpos ]
    let tableCount = readAt bytes 4 2
    let recordIndex = Seq.find (fun index -> readAt bytes (12 + 16 * index) 4 = int (uint32 gposTag)) (seq { 0 .. tableCount - 1 })
    let directory = 12 + 16 * recordIndex
    writeAt file (directory + 8) 4 (bytes.Length + padding.Length)
    writeAt file (directory + 12) 4 widenedGpos.Length
    let target = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-wide")
    Directory.CreateDirectory target |> ignore
    let path = Path.Combine(target, font.Path.Substring(font.Path.LastIndexOf '/' + 1))
    File.WriteAllBytes(path, file)
    { font with Path = path }

/// <summary>The P5.S10 differentials: GPOS single and pair adjustment against HarfBuzz.</summary>
/// <returns>The test list.</returns>
let tests () =
    let fonts = positioningFonts.Value |> Array.filter (fun font -> (plainCoverage font).Length > 0)
    let aots = fonts |> Array.filter (fun font -> font.Path.Contains "gpos1_" || font.Path.Contains "gpos2_")
    let real = fonts |> Array.filter (fun font -> not (font.Path.Contains "aots"))
    let roboto = TestFonts.all () |> List.filter (fun font -> font.Path.EndsWith "perf/fonts/Roboto-Regular.ttf" && hasPositioning font) |> List.toArray
    let latinFonts =
        TestFonts.extra [ "api/fonts/OpenSans-Regular.ttf"; "api/fonts/NotoSans-Bold.ttf"; "api/fonts/Inconsolata-Regular.abc.ttf"; "api/fonts/Mplus1p-Regular.ttf"; "api/fonts/Cantarell.A.otf"; "api/fonts/notosansitalic.ttf" ]
        |> Seq.filter hasPositioning
        |> Seq.toArray
    let latinText (font: TestFont) = plainCoverage font |> Array.filter (fun codepoint -> codepoint >= 0x20u && codepoint <= 0x17Fu)
    let wide =
        aots
        |> Array.filter (fun font -> font.Path.Contains "gpos2_")
        |> Array.truncate 6
        |> Array.map widened
    let robotoLines =
        Array.append (lines "en-thelittleprince.txt") (lines "en-words.txt")
        |> Array.map (fun line -> line.EnumerateRunes() |> Seq.map (fun rune -> uint32 rune.Value) |> Seq.toArray)
        |> Array.map (Array.truncate 200)
    testSequenced <| testList "P5.S10" [
        testCase "P5.S10 the corpus has positioning faces" (fun () ->
            printfn "P5.S10 faces: %d positioning faces, %d aots, %d real, %d unstripped Latin, Roboto %d" fonts.Length aots.Length real.Length latinFonts.Length roboto.Length
            Expect.isGreaterThan aots.Length 0 "aots gpos1 and gpos2 faces"
            Expect.isGreaterThan real.Length 0 "real faces with pair or single adjustment"
            printfn "P5.S10 faces widened to more than 64 GPOS lookups: %d" wide.Length
            Expect.isGreaterThan wide.Length 0 "a face with a pair lookup to widen"
            for font in wide do
                let struct (oracle, ours) = lookupCounts font.Path
                Expect.isGreaterThan oracle 64 "HarfBuzz reads more than 64 lookups"
                Expect.equal ours oracle "and so does Farfbuzz"
            Expect.isGreaterThan roboto.Length 0 "Roboto")
        versus "P5.S10 aots gpos1 and gpos2 shaping" (request aots) 6000
        versus "P5.S10 real fonts, random text" (request real) 8000
        versus "P5.S10 unstripped Latin fonts, random text"
            (gen {
                let! font = Gen.elements latinFonts
                let coverage = latinText font
                let! length = Gen.choose (1, 40)
                let! text = Gen.arrayOfLength length (Gen.elements coverage)
                let! struct (direction, flags, level, ppem, features) = settings false
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
            })
            6000
        versus "P5.S10 faces widened to more than 64 GPOS lookups, where the lean loop is off" (request wide) 3000
        versus "P5.S10 Roboto benchmark lines"
            (gen {
                let! face = Gen.elements roboto
                let! line = Gen.elements robotoLines
                let! struct (direction, flags, level, ppem, features) = settings false
                return
                    { Path = face.Path
                      Cached = true
                      Index = face.Index
                      Text = line
                      Features = features
                      Direction = direction
                      Flags = flags
                      ClusterLevel = level
                      Ppem = ppem }
            })
            6000
        testCase "P5.S10 mutated single and pair subtables" (fun () ->
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-mutation")
            Directory.CreateDirectory directory |> ignore
            let sources =
                Array.append fonts roboto
                |> Array.map (fun font -> font, gposFields (OracleFaces.data font.Path))
                |> Array.filter (fun (_, fields) -> fields.Length > 0)
            let failures = ResizeArray<string>()
            let mutable state = 20240501u
            let nextRandom () =
                state <- state * 1103515245u + 12345u
                state >>> 8
            let mutable cases = 0
            let mutable rejected = 0
            let mutable rejectedByUs = 0
            let mutable shapedDifferently = 0
            let mutable attempt = 0
            while cases < 5000 && attempt < 100000 do
                attempt <- attempt + 1
                let font, fields = sources[int (nextRandom () % uint32 sources.Length)]
                let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                let value = values[int (nextRandom () % uint32 values.Length)]
                let candidate = Array.copy (OracleFaces.data font.Path)
                writeAt candidate offset width value
                let path = Path.Combine(directory, $"mutated-{cases}.ttf")
                File.WriteAllBytes(path, candidate)
                let coverage = plainCoverage font
                let text = Array.init (1 + int (nextRandom () % 12u)) (fun _ -> coverage[int (nextRandom () % uint32 coverage.Length)])
                let features = match font.Path.Contains "aots" with | true -> "test" | false -> ""
                let request = { Path = path; Cached = false; Index = font.Index; Text = text; Features = features; Direction = "ltr"; Flags = 0u; ClusterLevel = 0; Ppem = (match nextRandom () % 4u with | 0u -> 12u | _ -> 0u) }
                let struct (ours, expected) = shapeBoth request
                let struct (oracleCount, ourCount) = lookupCounts path
                let struct (originalOracle, _) = lookupCounts font.Path
                if oracleCount = 0 && originalOracle > 0 then rejected <- rejected + 1
                if ourCount = 0 && originalOracle > 0 then rejectedByUs <- rejectedByUs + 1
                if oracleCount <> ourCount then failures.Add $"{Path.GetFileName font.Path}: {name} = {value}: lookup count oracle {oracleCount}, farfbuzz {ourCount}"
                elif ours <> expected then
                    shapedDifferently <- shapedDifferently + 1
                    failures.Add $"{Path.GetFileName font.Path}: {name} = {value}: shaped differently: farfbuzz %A{ours}, oracle %A{expected}"
                File.Delete path
                cases <- cases + 1
            printfn "P5.S10 compared %d mutated subtables, HarfBuzz rejected the table in %d (Farfbuzz in %d)" cases rejected rejectedByUs
            Expect.isGreaterThanOrEqual cases 5000 "the number of cases"
            Expect.isGreaterThan rejected 0 "some mutations make HarfBuzz reject the table"
            Expect.isEmpty (failures |> Seq.truncate 20 |> Seq.toList) ("mutated subtable differences:\n" + String.concat "\n" (failures |> Seq.truncate 20)))
        testCase "P5.S10 report" (fun () ->
            printfn "P5.S10 shaped %d requests against HarfBuzz, %d with an adjusted position" cases.Value kerned.Value
            Expect.isGreaterThan kerned.Value 0 "some requests were kerned")
    ]
