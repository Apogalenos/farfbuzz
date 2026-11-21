module Farfbuzz.Tests.AatSessionTests

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.OpenType
open Farfbuzz.Aat

/// <summary>The number of random runs each corpus font is run over.</summary>
[<Literal>]
let private RunsPerFont = 400

/// <summary>The directories of the upstream corpus that hold AAT test fonts.</summary>
let private corpusDirectories =
    [ "test/shape/data/in-house/fonts"; "test/shape/data/text-rendering-tests/fonts"; "test/api/fonts"; "test/subset/data/fonts" ]

/// <summary>Whether a font file holds one of the AAT tables, read from the table directory.</summary>
/// <param name="bytes">The font file.</param>
/// <param name="tags">The tags that count.</param>
/// <returns>True when the font has one of the tags.</returns>
let private hasTable (bytes: byte array) (tags: string array) =
    match bytes.Length >= 12 && bytes[0] <> byte 't' with
    | false -> false
    | true ->
        let count = int bytes[4] * 256 + int bytes[5]
        Array.init count (fun index -> 12 + 16 * index)
        |> Array.exists (fun at -> at + 4 <= bytes.Length && Array.contains (String [| char bytes[at]; char bytes[at + 1]; char bytes[at + 2]; char bytes[at + 3] |]) tags)

/// <summary>The AAT test fonts of the corpus with their bytes.</summary>
/// <param name="tags">The tables that make a font count.</param>
/// <returns>The names and bytes.</returns>
let private corpusFonts (tags: string array) =
    corpusDirectories
    |> List.collect (fun directory ->
        let path = Path.Combine(RepositoryPaths.upstream (), directory)
        match Directory.Exists path with
        | false -> []
        | true ->
            Directory.GetFiles(path)
            |> Array.filter (fun file -> file.EndsWith ".ttf" || file.EndsWith ".otf")
            |> Array.sort
            |> Array.toList
            |> List.choose (fun file ->
                let bytes = File.ReadAllBytes file
                match hasTable bytes tags with
                | true -> Some (FileInfo(file).Name, bytes)
                | false -> None))

/// <summary>The glyphs the subtables of a table start on, a few of each range, so that random runs reach the state machines.</summary>
/// <param name="table">The morx or mort table.</param>
/// <returns>The glyph pool.</returns>
let private startGlyphs (table: Morx) =
    table.Chains.AsSpan().ToArray()
    |> Array.collect (fun chain ->
        chain.Subtables.AsSpan().ToArray()
        |> Array.collect (fun subtable ->
            subtable.InitialGlyphs.AsSpan().ToArray()
            |> Array.collect (fun range -> [| range.First .. min range.Last (range.First + 20u) |])))
    |> Array.truncate 4000

/// <summary>Describes how two runs differ, or nothing when they agree on every field.</summary>
/// <param name="expected">The run the reference produced.</param>
/// <param name="actual">The run the session produced.</param>
/// <returns>The first difference, or an empty string.</returns>
let private describe (expected: GlyphRun) (actual: GlyphRun) =
    match expected.Infos.AsSpan().ToArray() = actual.Infos.AsSpan().ToArray(),
          expected.Props.AsSpan().ToArray() = actual.Props.AsSpan().ToArray(),
          expected.Positions.AsSpan().ToArray() = actual.Positions.AsSpan().ToArray(),
          struct (expected.HavePositions, expected.MaxOps, expected.Successful, expected.Serial, expected.RandomState, expected.ScratchFlags) =
              struct (actual.HavePositions, actual.MaxOps, actual.Successful, actual.Serial, actual.RandomState, actual.ScratchFlags) with
    | false, _, _, _ -> "infos differ"
    | true, false, _, _ -> "props differ"
    | true, true, false, _ -> "positions differ"
    | true, true, true, false -> $"scalars differ: {expected.MaxOps} {expected.Successful} {expected.ScratchFlags} against {actual.MaxOps} {actual.Successful} {actual.ScratchFlags}"
    | true, true, true, true -> String.Empty

/// <summary>User features of random ranges, so that the range walks run.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="count">The glyphs of the run.</param>
/// <returns>The features.</returns>
let private randomFeatures (rnd: Random) (count: int) =
    let tags = [| "liga"; "clig"; "dlig"; "calt"; "smcp"; "onum"; "lnum"; "hlig"; "rlig"; "ss01"; "salt"; "swsh" |]
    Array.init (rnd.Next(0, 4)) (fun _ ->
        let start = uint32 (rnd.Next(0, max 1 count))
        { Tag = Tag.ofString tags[rnd.Next tags.Length]
          Value = uint32 (rnd.Next(0, 3))
          Start = start
          End = (match rnd.Next 3 with | 0 -> Feature.GlobalEnd | _ -> start + uint32 (rnd.Next(1, 8))) })

/// <summary>A random run over the glyphs of a face, biased to the glyphs the subtables start on.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="glyphCount">The glyph count of the face.</param>
/// <param name="pool">The glyphs the subtables start on.</param>
/// <returns>The run.</returns>
let private randomRun (rnd: Random) (glyphCount: int) (pool: uint32 array) =
    let count = rnd.Next(0, 40)
    let glyph () =
        match pool.Length > 0 && rnd.Next 10 < 7 with
        | true -> pool[rnd.Next pool.Length]
        | false -> uint32 (rnd.Next(0, max 1 glyphCount))
    let mutable cluster = 0u
    let infos =
        Sealed.init count (fun _ ->
            cluster <- cluster + (match rnd.Next 4 with | 0 -> 0u | _ -> 1u)
            { Codepoint = glyph (); Mask = (match rnd.Next 8 with | 0 -> 1u | _ -> 0u); Cluster = cluster; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    let positioned = rnd.Next 12 = 0
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = (match positioned with | true -> Sealed.init count (fun _ -> { XAdvance = 500; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }) | false -> ImmutableArray.Empty)
        HavePositions = positioned
        MaxOps = (match rnd.Next 12 with | 0 -> rnd.Next(0, max 1 (count * 6)) | _ -> max 8192 (count * 64))
        Flags = (match rnd.Next 3 with | 0 -> BufferFlags.ProduceUnsafeToConcat | _ -> BufferFlags.Default)
        SegmentProperties = { Direction = (match rnd.Next 4 with | 0 -> Direction.Rtl | 1 -> Direction.Ttb | _ -> Direction.Ltr); Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>A copy of a run whose arrays share nothing with it.</summary>
/// <param name="run">The run.</param>
/// <returns>The copy.</returns>
let private ownedCopy (run: GlyphRun) =
    { run with
        Infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
        Props = ImmutableArray.CreateRange (run.Props.AsSpan().ToArray()) }

/// <summary>An anchor read straight from the lookup, <c>ankr::get_anchor</c>, the oracle of the per-face anchor table.</summary>
/// <param name="ankr">The table.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="index">The anchor index.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>The anchor.</returns>
let private lookupAnchor (ankr: Ankr) (glyph: uint32) (index: uint32) (numGlyphs: int) =
    match AatLookup.valueAt ankr.Lookup glyph numGlyphs with
    | ValueNone -> struct (0, 0)
    | ValueSome valueAt ->
        let span = ankr.Table.Span
        let target = int (int64 ankr.AnchorsAt + int64 (BinaryCalls.u16 span valueAt))
        match index >= BinaryCalls.u32 span target with
        | true -> struct (0, 0)
        | false ->
            let at = target + 4 + int index * 4
            struct (int (BinaryCalls.i16 span at), int (BinaryCalls.i16 span (at + 2)))

/// <summary>The tests of the working set through the subtables of a table.</summary>
[<Tests>]
let tests =
    testList "AAT session" [
        test "the working set agrees with the interpreter on the morx fonts of the corpus, owned and shared arrays, with user features" {
            let fonts = corpusFonts [| "morx"; "mort" |]
            Expect.isGreaterThan fonts.Length 20 "the corpus holds morx fonts"
            let changed = ConcurrentBag<int>()
            let edited = ConcurrentBag<int>()
            let failures =
                fonts
                |> List.collect (fun (name, bytes) ->
                    let face = OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let table = match Morx.hasData face.Morx.Value with | true -> face.Morx.Value | false -> face.Mort.Value
                    let pool = startGlyphs table
                    let rnd = Random(name.GetHashCode() &&& 0xFFFF)
                    [ for _ in 1 .. RunsPerFont do
                        let run = randomRun rnd face.Face.GlyphCount pool
                        let segment = run.SegmentProperties
                        let features = randomFeatures rnd run.Infos.Length
                        let map = match features.Length with | 0 -> AatMap.defaults face segment | _ -> AatMap.build face segment (ReadOnlySpan<Feature> features)
                        let expected = MorxApply.applyWith Reference table map face segment run
                        let shared = MorxApply.apply table map face segment run
                        let copy = ownedCopy run
                        let struct (owned, wasEdited) = MorxApply.applyOwned table map face segment copy
                        if not (obj.ReferenceEquals (expected, run)) && describe expected run <> String.Empty then changed.Add 1
                        if wasEdited then edited.Add 1
                        match describe expected shared, describe expected owned with
                        | "", "" -> ()
                        | difference, "" -> yield $"{name}: shared {difference}"
                        | _, difference -> yield $"{name}: owned {difference}" ])
            Expect.equal (failures |> List.truncate 10) [] "no morx font shapes differently on the working set"
            Expect.isGreaterThan changed.Count 500 "the subtables of the corpus change many of the random runs"
            Expect.isGreaterThan edited.Count 300 "the working set reports its edits"
        }
        test "the default map equals the chains compiled for the segment's language, and is one map when no chain reads the language" {
            let fonts = corpusFonts [| "morx"; "mort" |]
            let languages = [ NoLanguage; Bcp47 "en"; Bcp47 "ja"; Bcp47 "zh-hant" ]
            let shared = ConcurrentBag<int>()
            let failures =
                fonts
                |> List.collect (fun (name, bytes) ->
                    let face = OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let table = match Morx.hasData face.Morx.Value with | true -> face.Morx.Value | false -> face.Mort.Value
                    let segmentOf language = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = language }
                    let first = AatMap.defaults face (segmentOf NoLanguage)
                    let second = AatMap.defaults face (segmentOf (Bcp47 "ja"))
                    if obj.ReferenceEquals (ImmutableCollectionsMarshal.AsArray first.ChainFlags, ImmutableCollectionsMarshal.AsArray second.ChainFlags) then shared.Add 1
                    languages
                    |> List.collect (fun language ->
                        let map = AatMap.defaults face (segmentOf language)
                        let expected =
                            [| for chain in 0 .. table.Chains.Length - 1 ->
                                [| struct (Morx.compileFlags table chain ImmutableArray<struct (uint16 * uint16)>.Empty face.Ltag.Value language, 0u, UInt32.MaxValue) |] |]
                        let actual =
                            [| for ranges in map.ChainFlags.AsSpan().ToArray() ->
                                [| for range in ranges.AsSpan().ToArray() -> struct (range.Flags, range.ClusterFirst, range.ClusterLast) |] |]
                        match actual = expected with
                        | true -> []
                        | false -> [ $"{name} {language}" ]))
            Expect.equal (failures |> List.truncate 10) [] "the default map is the compile for the language"
            Expect.isGreaterThan shared.Count 15 "most fonts share one default map between languages"
        }
        test "the per-face anchor table equals the lookup read for every glyph and index, out of range ones included" {
            let fonts = corpusFonts [| "ankr" |]
            Expect.isGreaterThan fonts.Length 0 "the corpus holds a font with an ankr table"
            let failures =
                fonts
                |> List.collect (fun (name, bytes) ->
                    let face = OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let ankr = face.Ankr.Value
                    let numGlyphs = face.Face.GlyphCount
                    [ for glyph in 0u .. uint32 numGlyphs + 3u do
                        for index in [ 0u; 1u; 2u; 3u; 7u; 0xFFFFu; UInt32.MaxValue ] do
                            if Ankr.anchor ankr glyph index numGlyphs <> lookupAnchor ankr glyph index numGlyphs then yield $"{name}: glyph {glyph} index {index}" ])
            Expect.equal (failures |> List.truncate 10) [] "the anchor table answers as the lookup does"
            let anchored =
                fonts
                |> List.sumBy (fun (_, bytes) ->
                    let face = OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)
                    [ for glyph in 0u .. uint32 face.Face.GlyphCount - 1u do
                        let struct (x, y) = Ankr.anchor face.Ankr.Value glyph 0u face.Face.GlyphCount
                        if x <> 0 || y <> 0 then yield glyph ]
                    |> List.length)
            Expect.isGreaterThan anchored 20 "the corpus has glyphs with real anchors, so that the comparison is not of nulls"
        }
        test "a run the working set reports as not edited has the glyphs and properties it went in with" {
            let fonts = corpusFonts [| "morx"; "mort" |]
            let wrong =
                fonts
                |> List.collect (fun (name, bytes) ->
                    let face = OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let table = match Morx.hasData face.Morx.Value with | true -> face.Morx.Value | false -> face.Mort.Value
                    let pool = startGlyphs table
                    let rnd = Random(1 + (name.GetHashCode() &&& 0xFFFF))
                    [ for _ in 1 .. RunsPerFont do
                        let run = randomRun rnd face.Face.GlyphCount pool
                        let map = AatMap.defaults face run.SegmentProperties
                        let copy = ownedCopy run
                        let struct (owned, wasEdited) = MorxApply.applyOwned table map face run.SegmentProperties copy
                        let same = owned.Infos.AsSpan().ToArray() = run.Infos.AsSpan().ToArray() && owned.Props.AsSpan().ToArray() = run.Props.AsSpan().ToArray()
                        if (not wasEdited) && not same then yield name ])
            Expect.equal wrong [] "an unedited run keeps its glyphs and properties"
        }
    ]
