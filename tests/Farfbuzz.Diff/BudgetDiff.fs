module Farfbuzz.Diff.BudgetDiff

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Oracle.NativeText
open Farfbuzz.Oracle.OracleLibrary

/// <summary>What one engine left in the buffer after a shaping call whose length limit or operation budget may have failed:
/// whether the buffer's allocations succeeded, and each glyph with its cluster, advances, offsets, and flags.</summary>
type Left =
    { Successful: bool
      Glyphs: (struct (uint32 * uint32 * int * int * int * int * uint32)) array }

/// <summary>One shaping request with the limits the buffer starts from; a negative limit leaves the limit the text length gives.</summary>
type Limited =
    { Path: string
      Script: string
      Text: uint32 array
      Features: string
      Direction: string
      Flags: uint32
      ClusterLevel: int
      MaxLen: int
      MaxOps: int }

let private ourFaces = ConcurrentDictionary<string, Face>()

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

/// <summary>Shapes a request with Farfbuzz through the whole pipeline of <c>Shape.shapeFull</c>, the limits set on the run as a caller
/// of the library may set them.</summary>
/// <param name="request">The request.</param>
/// <returns>What is left, or the reason shaping gave no run.</returns>
let ourShape (request: Limited) : Result<Left, string> =
    let face = ourFaces.GetOrAdd(request.Path, fun path -> Face.create (ReadOnlyMemory (OracleFaces.data path)) 0u)
    let font = Font.create face
    let run = GlyphRun.addCodepoints (ReadOnlySpan request.Text) 0 request.Text.Length GlyphRun.empty
    let level =
        match request.ClusterLevel with
        | 1 -> ClusterLevel.MonotoneCharacters
        | 2 -> ClusterLevel.Characters
        | 3 -> ClusterLevel.Graphemes
        | _ -> ClusterLevel.MonotoneGraphemes
    let run =
        run
        |> GlyphRun.withSegment { Direction = Direction.ofString request.Direction; Script = Script.ofString request.Script; Language = Language.ofString "c" }
        |> GlyphRun.withFlags (LanguagePrimitives.EnumOfValue<uint32, Farfbuzz.BufferFlags> request.Flags)
        |> GlyphRun.withClusterLevel level
    let limited =
        { run with
            MaxLen = (match request.MaxLen < 0 with | true -> run.MaxLen | false -> request.MaxLen)
            MaxOps = (match request.MaxOps < 0 with | true -> run.MaxOps | false -> request.MaxOps) }
    match Shape.shapeFull font (ourFeatures request.Features) (ValueSome (ImmutableArray.Create "ot")) limited with
    | Error error -> Error $"%A{error}"
    | Ok result ->
        let infos = result.Infos.AsSpan().ToArray()
        let positions = result.Positions.AsSpan().ToArray()
        let positionOf (index: int) = match index < positions.Length with | true -> positions[index] | false -> Unchecked.defaultof<GlyphPosition>
        Ok { Successful = result.Successful
             Glyphs =
                infos
                |> Array.mapi (fun index (info: GlyphInfo) ->
                    let position = positionOf index
                    struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined)) }

/// <summary>Shapes a request with HarfBuzz as hb-shape does, after lowering the buffer's limits with the oracle's shim.</summary>
/// <param name="request">The request.</param>
/// <returns>What is left, or the reason shaping failed.</returns>
let oracleShape (request: Limited) : Result<Left, string> =
    use blob = HarfBuzzSharp.Blob.FromFile request.Path
    use face = new HarfBuzzSharp.Face(blob, 0)
    use font = new HarfBuzzSharp.Font(face)
    let upem = int (Native.hb_face_get_upem face.Handle)
    Native.hb_font_set_scale(font.Handle, upem, upem)
    Native.hb_font_set_funcs_using(font.Handle, "ot") |> ignore
    use buffer = new HarfBuzzSharp.Buffer()
    Native.hb_buffer_add_codepoints(buffer.Handle, request.Text, request.Text.Length, 0u, request.Text.Length)
    let direction = Encoding.UTF8.GetBytes request.Direction
    Native.hb_buffer_set_direction(buffer.Handle, Native.hb_direction_from_string(direction, direction.Length))
    let script = Encoding.UTF8.GetBytes request.Script
    Native.hb_buffer_set_script(buffer.Handle, Native.hb_script_from_string(script, script.Length))
    let language = Encoding.UTF8.GetBytes "c"
    Native.hb_buffer_set_language(buffer.Handle, Native.hb_language_from_string(language, language.Length))
    Native.hb_buffer_set_flags(buffer.Handle, request.Flags)
    Native.hb_buffer_set_cluster_level(buffer.Handle, request.ClusterLevel)
    let features =
        match request.Features.Length with
        | 0 -> [||]
        | _ -> request.Features.Split([| ','; ' ' |]) |> Array.choose (fun item -> featureFromString item |> ValueOption.toOption)
    Shims.farfbuzz_oracle_buffer_set_limits(buffer.Handle, request.MaxLen, request.MaxOps)
    let shapers = [| "ot"; Unchecked.defaultof<string> |]
    match Native.hb_shape_full(font.Handle, buffer.Handle, features, uint32 features.Length, shapers) with
    | 0 -> Error "shaping failed"
    | _ ->
        let mutable length = 0u
        let infos = copyFrom<GlyphInfo> (Native.hb_buffer_get_glyph_infos(buffer.Handle, &length)) (int length)
        let positions = copyFrom<GlyphPosition> (Native.hb_buffer_get_glyph_positions(buffer.Handle, &length)) (int length)
        Ok { Successful = Shims.hb_buffer_allocation_successful buffer.Handle <> 0
             Glyphs =
                Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                    struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined)) infos positions }

/// <summary>A script under test: a real font, the script's ISO tag and weighted pools of code points.</summary>
type private Subject =
    { Name: string
      Font: string
      Script: string
      Pools: (int * (int * int) list) list }

let private shared = [ (3, [ (0x41, 0x5A); (0x61, 0x7A) ]); (3, [ (0x20, 0x20); (0x30, 0x39) ]); (2, [ (0x25CC, 0x25CC) ]); (3, [ (0x200C, 0x200D) ]); (1, [ (0x300, 0x30F) ]) ]

let private hangulPools =
    [ (20, [ (0xAC00, 0xD7A3) ]); (10, [ (0x1100, 0x1112) ]); (10, [ (0x1161, 0x1175) ]); (8, [ (0x11A8, 0x11C2) ]); (4, [ (0x1113, 0x115F) ]); (4, [ (0x1176, 0x11A7) ]); (4, [ (0x11C3, 0x11FF) ])
      (3, [ (0xA960, 0xA97C) ]); (3, [ (0xD7B0, 0xD7FB) ]); (4, [ (0x302E, 0x302F) ]); (3, [ (0x3131, 0x318E) ]) ] @ shared

/// <summary>The subjects: one real font for each shaper that has a stage that edits the run.</summary>
let private subjects =
    [ { Name = "Devanagari"; Font = "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf"; Script = "Deva"
        Pools = [ (30, [ (0x915, 0x939) ]); (14, [ (0x93E, 0x94C) ]); (14, [ (0x94D, 0x94D) ]); (5, [ (0x93C, 0x93C) ]); (6, [ (0x904, 0x914) ]); (4, [ (0x900, 0x903) ]) ] @ shared }
      { Name = "Kannada"; Font = "test/shape/data/text-rendering-tests/fonts/NotoSerifKannada-Regular.ttf"; Script = "Knda"
        Pools = [ (30, [ (0xC95, 0xCB9) ]); (14, [ (0xCBE, 0xCCC); (0xCD5, 0xCD6) ]); (14, [ (0xCCD, 0xCCD) ]); (6, [ (0xC85, 0xC94) ]) ] @ shared }
      { Name = "Khmer"; Font = "test/subset/data/fonts/Khmer.ttf"; Script = "Khmr"
        Pools = [ (30, [ (0x1780, 0x17A2) ]); (15, [ (0x17B6, 0x17C5) ]); (8, [ (0x17C6, 0x17D1) ]); (14, [ (0x17D2, 0x17D2) ]) ] @ shared }
      { Name = "Myanmar"; Font = "test/subset/data/fonts/NotoSerifMyanmar-Regular.otf"; Script = "Mymr"
        Pools = [ (30, [ (0x1000, 0x1021) ]); (10, [ (0x102B, 0x1032) ]); (8, [ (0x103B, 0x103E) ]); (5, [ (0x103A, 0x103A) ]); (8, [ (0x1039, 0x1039) ]) ] @ shared }
      { Name = "Balinese (USE)"; Font = "test/shape/data/text-rendering-tests/fonts/NotoSansBalinese-Regular.ttf"; Script = "Bali"
        Pools = [ (30, [ (0x1B13, 0x1B33) ]); (15, [ (0x1B35, 0x1B43) ]); (10, [ (0x1B44, 0x1B44) ]); (4, [ (0x1B00, 0x1B04) ]) ] @ shared }
      { Name = "Duployan (USE)"; Font = "perf/fonts/NotoSansDuployan-Regular.otf"; Script = "Dupl"
        Pools = [ (30, [ (0x1BC00, 0x1BC5B) ]); (14, [ (0x1BC5C, 0x1BC7C) ]); (4, [ (0x1BC80, 0x1BC88) ]); (4, [ (0x1BC9C, 0x1BC9F) ]); (3, [ (0x34F, 0x34F) ]) ] @ shared }
      { Name = "Latin (Roboto)"; Font = "perf/fonts/Roboto-Regular.ttf"; Script = "Latn"
        Pools = [ (30, [ (0x41, 0x5A); (0x61, 0x7A) ]); (10, [ (0xC0, 0x17F) ]); (22, [ (0x300, 0x36F) ]); (2, [ (0x20, 0x20); (0x30, 0x39) ]) ] }
      { Name = "Thai"; Font = "test/fuzzing/fonts/kanit.ttf"; Script = "Thai"
        Pools = [ (30, [ (0xE01, 0xE2E) ]); (10, [ (0xE40, 0xE44) ]); (14, [ (0xE31, 0xE31); (0xE34, 0xE3A) ]); (10, [ (0xE47, 0xE4E) ]); (4, [ (0xE33, 0xE33) ]) ] @ shared }
      { Name = "Hangul (AdobeBlank)"; Font = "test/subset/data/fonts/AdobeBlank-Regular.ttf"; Script = "Hang"; Pools = hangulPools }
      { Name = "Hangul (in-house)"; Font = "test/shape/data/in-house/fonts/600387433d01cd5799e421dad6510a54c862f56b.ttf"; Script = "Hang"; Pools = hangulPools }
      { Name = "Arabic (Amiri)"; Font = "test/subset/data/fonts/Amiri-Regular.ttf"; Script = "Arab"
        Pools = [ (30, [ (0x621, 0x64A) ]); (14, [ (0x64B, 0x65F) ]); (5, [ (0x670, 0x670); (0x6D6, 0x6ED) ]); (4, [ (0x640, 0x640) ]); (6, [ (0x622, 0x626) ]) ] @ shared }
      { Name = "Arabic (Nastaliq)"; Font = "perf/fonts/NotoNastaliqUrdu-Regular.ttf"; Script = "Arab"
        Pools = [ (30, [ (0x621, 0x64A) ]); (14, [ (0x64B, 0x65F) ]); (8, [ (0x66E, 0x6D3) ]) ] @ shared } ]

/// <summary>Draws a request of a subject from a random source: a text of one to fifty glyphs, flags, a cluster level and limits that
/// fail inside the stages.</summary>
/// <param name="subject">The subject.</param>
/// <param name="rnd">The random source.</param>
/// <param name="lengths">Whether the length limit is varied.</param>
/// <param name="budgets">Whether the operation budget is varied.</param>
/// <returns>The request.</returns>
let private draw (subject: Subject) (rnd: Random) (lengths: bool) (budgets: bool) =
    let total = subject.Pools |> List.sumBy fst
    let codepoint () =
        let mutable pick = rnd.Next total
        let (_, ranges) = subject.Pools |> List.find (fun (weight, _) -> pick <- pick - weight; pick < 0)
        let (first, last) = ranges[rnd.Next ranges.Length]
        uint32 (rnd.Next(first, last + 1))
    let length = match rnd.Next 4 with | 0 -> rnd.Next(1, 5) | 1 -> rnd.Next(5, 17) | 2 -> rnd.Next(1, 33) | _ -> rnd.Next(1, 51)
    { Path = Path.Combine(RepositoryPaths.upstream (), subject.Font)
      Script = subject.Script
      Text = Array.init length (fun _ -> codepoint ())
      Features = String.Empty
      Direction = (match subject.Script with | "Arab" -> "rtl" | _ -> "ltr")
      Flags = [| 0u; 0u; 64u; 64u ||| 128u |][rnd.Next 4]
      ClusterLevel = rnd.Next 4
      MaxLen = (match lengths with | true -> rnd.Next(0, 90) | false -> -1)
      MaxOps = (match budgets with | true -> (match rnd.Next 3 with | 0 -> rnd.Next(0, 60) | 1 -> rnd.Next(0, 400) | _ -> rnd.Next(0, 6000)) | false -> -1) }

/// <summary>What differs between what two engines left in a buffer: the success flag, the number of glyphs, the order of the same
/// glyphs, the glyphs themselves, or only their advances, offsets and flags.</summary>
/// <param name="ours">What Farfbuzz left.</param>
/// <param name="theirs">What HarfBuzz left.</param>
/// <returns>The first kind of difference, or none.</returns>
let private kindOfDifference (ours: Result<Left, string>) (theirs: Result<Left, string>) =
    match ours, theirs with
    | Ok a, Ok b when a.Successful <> b.Successful -> "success flag"
    | Ok a, Ok b when a.Glyphs.Length <> b.Glyphs.Length -> "glyph count"
    | Ok a, Ok b when (a.Glyphs |> Array.map (fun (struct (g, c, _, _, _, _, _)) -> struct (g, c)) |> Array.sort) <> (b.Glyphs |> Array.map (fun (struct (g, c, _, _, _, _, _)) -> struct (g, c)) |> Array.sort) -> "glyphs or clusters"
    | Ok a, Ok b when (a.Glyphs |> Array.map (fun (struct (g, c, _, _, _, _, _)) -> struct (g, c))) <> (b.Glyphs |> Array.map (fun (struct (g, c, _, _, _, _, _)) -> struct (g, c))) -> "order"
    | Ok _, Ok _ -> "advances, offsets or flags"
    | Error _, _ | _, Error _ -> "error"

/// <summary>Compares the two engines on the requests of a subject and lists the first differences of each kind.</summary>
/// <param name="subject">The subject.</param>
/// <param name="seed">The seed.</param>
/// <param name="count">The number of requests.</param>
/// <param name="lengths">Whether the length limit is varied.</param>
/// <param name="budgets">Whether the operation budget is varied.</param>
/// <returns>The number of differences, the requests HarfBuzz failed, and a description of the first of each kind.</returns>
let private compareSubject (subject: Subject) (seed: int) (count: int) (lengths: bool) (budgets: bool) =
    let rnd = Random seed
    let differences = ResizeArray<string * string>()
    let mutable failedRuns = 0
    for _ in 1 .. count do
        let request = draw subject rnd lengths budgets
        let ours = ourShape request
        let theirs = oracleShape request
        match theirs with
        | Ok left when not left.Successful -> failedRuns <- failedRuns + 1
        | Ok _ | Error _ -> ()
        match ours = theirs with
        | true -> ()
        | false ->
            let hex = String.Join (" ", request.Text |> Array.map (fun c -> c.ToString "X"))
            let describe (result: Result<Left, string>) =
                match result with
                | Error reason -> reason
                | Ok left -> $"ok {left.Successful} " + String.Join (" ", left.Glyphs |> Array.map (fun (struct (g, c, ax, ay, ox, oy, m)) -> $"{g}@{c}/{ax},{ay},{ox},{oy}/{m:X}"))
            differences.Add (kindOfDifference ours theirs, $"{subject.Name} maxlen {request.MaxLen} maxops {request.MaxOps} flags {request.Flags} level {request.ClusterLevel} text {hex}\n    farfbuzz {describe ours}\n    harfbuzz {describe theirs}")
    let kinds = differences |> Seq.map fst |> Seq.countBy id |> Seq.map (fun (kind, n) -> $"{n} {kind}") |> String.concat ", "
    let firsts = differences |> Seq.groupBy fst |> Seq.map (fun (_, group) -> snd (Seq.head group)) |> String.concat "\n"
    struct (differences.Count, failedRuns, $"[{kinds}]\n{firsts}")

let private u16 (value: int) = [| byte (value >>> 8); byte value |]

let private u32 (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>The offsets at which the pieces of a concatenation start, from a first offset.</summary>
/// <param name="first">The offset of the first piece.</param>
/// <param name="pieces">The pieces.</param>
/// <returns>The offset of each piece.</returns>
let private placed (first: int) (pieces: byte array list) =
    pieces |> List.fold (fun (offsets, at) piece -> offsets @ [ at ], at + piece.Length) ([], first) |> fst

/// <summary>A coverage of format 1.</summary>
/// <param name="glyphs">The sorted glyphs.</param>
/// <returns>The bytes.</returns>
let private coverage (glyphs: int list) =
    Array.concat [ u16 1; u16 glyphs.Length; Array.concat (glyphs |> List.map u16) ]

/// <summary>The glyphs of a synthetic font: glyph zero and twelve letters, U+0041 to U+004C.</summary>
[<Literal>]
let private SyntheticGlyphs = 13

/// <summary>The basic tables of a synthetic font: head, maxp, hhea, hmtx and a cmap that maps U+0041 to U+004C to glyphs 1 to 12.</summary>
/// <returns>The tags and bytes.</returns>
let private basics () =
    let head = Array.concat [ u32 0x00010000; u32 0; u32 0; u32 0x5F0F3CF5; u16 0; u16 1000; Array.zeroCreate 16; Array.zeroCreate 8; u16 0; u16 8; u16 2; u16 0; u16 0 ]
    let maxp = Array.concat [ u32 0x00005000; u16 SyntheticGlyphs ]
    let hhea = Array.concat [ u32 0x00010000; u16 800; u16 0xFF38; u16 0; u16 500; Array.zeroCreate 20; u16 0; u16 SyntheticGlyphs ]
    let hmtx = Array.concat (List.init SyntheticGlyphs (fun _ -> Array.concat [ u16 500; u16 0 ]))
    let subtable = Array.concat [ u16 12; u16 0; u32 28; u32 0; u32 1; u32 0x41; u32 0x4C; u32 1 ]
    let cmap = Array.concat [ u16 0; u16 1; u16 3; u16 10; u32 12; subtable ]
    [ "cmap", cmap; "head", head; "hhea", hhea; "hmtx", hmtx; "maxp", maxp ]

/// <summary>One lookup of a table, its type and its subtables, with no flags.</summary>
/// <param name="kind">The lookup type.</param>
/// <param name="subtables">The subtables.</param>
/// <returns>The bytes.</returns>
let private lookupBytes (kind: int) (subtables: byte array list) =
    let head = 6 + 2 * subtables.Length
    let offsets = placed head subtables
    Array.concat [ u16 kind; u16 0; u16 subtables.Length; Array.concat (offsets |> List.map u16); Array.concat subtables ]

/// <summary>A GSUB or GPOS table of lookups under a default script, one feature for each tag.</summary>
/// <param name="lookups">The lookups, each with its feature tag.</param>
/// <returns>The table bytes.</returns>
let private layoutTable (lookups: (string * byte array) list) =
    let tags = lookups |> List.map fst |> List.distinct
    let langSys = Array.concat [ u16 0; u16 0xFFFF; u16 tags.Length; Array.concat (List.init tags.Length u16) ]
    let script = Array.concat [ u16 4; u16 0; langSys ]
    let scriptList = Array.concat [ u16 1; Text.Encoding.ASCII.GetBytes "DFLT"; u16 8; script ]
    let features =
        tags
        |> List.map (fun tag ->
            let indices = lookups |> List.indexed |> List.filter (fun (_, (other, _)) -> other = tag) |> List.map fst
            Array.concat [ u16 0; u16 indices.Length; Array.concat (indices |> List.map u16) ])
    let featureHead = 2 + 6 * tags.Length
    let featureOffsets = placed featureHead features
    let featureList =
        Array.concat [ u16 tags.Length; Array.concat (List.zip tags featureOffsets |> List.map (fun (tag, offset) -> Array.concat [ Text.Encoding.ASCII.GetBytes tag; u16 offset ])); Array.concat features ]
    let lookupBodies = lookups |> List.map snd
    let lookupHead = 2 + 2 * lookups.Length
    let lookupOffsets = placed lookupHead lookupBodies
    let lookupList = Array.concat [ u16 lookups.Length; Array.concat (lookupOffsets |> List.map u16); Array.concat lookupBodies ]
    let offsets = placed 10 [ scriptList; featureList; lookupList ]
    Array.concat [ u32 0x00010000; u16 offsets[0]; u16 offsets[1]; u16 offsets[2]; scriptList; featureList; lookupList ]

/// <summary>A table directory around tables.</summary>
/// <param name="tables">The tags and bytes.</param>
/// <returns>The file bytes.</returns>
let private sfnt (tables: (string * byte array) list) =
    let tables = tables |> List.sortBy fst
    let head = 12 + 16 * tables.Length
    let padded = tables |> List.map (fun (tag, bytes) -> tag, bytes, Array.zeroCreate<byte> ((4 - bytes.Length % 4) % 4))
    let offsets = placed head (padded |> List.map (fun (_, bytes, padding) -> Array.append bytes padding))
    let records = List.zip padded offsets |> List.map (fun ((tag, bytes, _), offset) -> Array.concat [ Text.Encoding.ASCII.GetBytes tag; u32 0; u32 offset; u32 bytes.Length ])
    Array.concat [ u32 0x00010000; u16 tables.Length; u16 0; u16 0; u16 0; Array.concat records; Array.concat (padded |> List.map (fun (_, bytes, padding) -> Array.append bytes padding)) ]

/// <summary>A multiple substitution of glyph 1 into a sequence of glyph 2 copies.</summary>
/// <param name="length">The length of the sequence.</param>
/// <returns>The lookup type and its subtable.</returns>
let private expansion (length: int) =
    let sequence = Array.concat [ u16 length; Array.concat (List.init length (fun _ -> u16 2)) ]
    let cover = coverage [ 1 ]
    2, Array.concat [ u16 1; u16 8; u16 1; u16 (8 + cover.Length); cover; sequence ]

/// <summary>A context subtable of format 3 that matches glyph 1 and applies a lookup at it for each of a number of records, a
/// substitution context for a GSUB table and a positioning context for a GPOS one: the lookups it calls apply the same way, so that
/// the number of applications is the fan-out raised to the depth.</summary>
/// <param name="called">The index of the lookup every record calls.</param>
/// <param name="records">The number of records.</param>
/// <returns>The subtable.</returns>
let private contextCalling (called: int) (records: int) =
    let cover = coverage [ 1 ]
    let head = 6 + 2 + 4 * records
    Array.concat [ u16 3; u16 1; u16 records; u16 head; Array.concat (List.init records (fun _ -> Array.concat [ u16 0; u16 called ])); cover ]

/// <summary>A chain of context lookups, each calling the next with a number of records, the last one a single substitution of glyph 1 to
/// glyph 3 or a single positioning that widens glyph 1, or a context with no record, so that what the chain does can be seen or not.</summary>
/// <param name="positioning">Whether the table is a GPOS one.</param>
/// <param name="depth">The number of context lookups.</param>
/// <param name="fan">The records of each.</param>
/// <param name="visible">Whether the last lookup changes the glyph.</param>
/// <returns>The lookups, the first with the feature tag that is on by default and the others with a tag that is not.</returns>
let private contextChain (positioning: bool) (depth: int) (fan: int) (visible: bool) =
    let contextKind = match positioning with | true -> 7 | false -> 5
    let cover = coverage [ 1 ]
    let last =
        match positioning, visible with
        | false, true -> lookupBytes 1 [ Array.concat [ u16 1; u16 6; u16 2; cover ] ]
        | true, true -> lookupBytes 1 [ Array.concat [ u16 1; u16 8; u16 4; u16 100; cover ] ]
        | false, false | true, false -> lookupBytes contextKind [ contextCalling 0 0 ]
    let tag index = match index with | 0 -> (match positioning with | true -> "kern" | false -> "ccmp") | _ -> "zzzz"
    [ for index in 0 .. depth - 1 -> tag index, lookupBytes contextKind [ contextCalling (index + 1) fan ] ]
    @ [ tag depth, last ]

/// <summary>The bytes of a synthetic font whose only layout table is the one given.</summary>
/// <param name="positioning">Whether the table is GPOS.</param>
/// <param name="lookups">The lookups.</param>
/// <returns>The font bytes.</returns>
let private syntheticFont (positioning: bool) (lookups: (string * byte array) list) =
    sfnt (basics () @ [ (match positioning with | true -> "GPOS" | false -> "GSUB"), layoutTable lookups ])

/// <summary>Writes a synthetic font to the artifacts folder under a name and returns its path.</summary>
/// <param name="name">The file name.</param>
/// <param name="bytes">The font bytes.</param>
/// <returns>The path.</returns>
let private written (name: string) (bytes: byte array) =
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "budget-synthetic")
    Directory.CreateDirectory directory |> ignore
    let path = Path.Combine(directory, name)
    File.WriteAllBytes(path, bytes)
    path

/// <summary>Compares both engines on one synthetic font and text of glyph 1 letters.</summary>
/// <param name="path">The font.</param>
/// <param name="count">The number of letters.</param>
/// <returns>Whether HarfBuzz failed the buffer, and a description when the engines differ.</returns>
let private compareSynthetic (path: string) (count: int) =
    let request =
        { Path = path; Script = "Latn"; Text = Array.create count 0x41u; Features = String.Empty; Direction = "ltr"
          Flags = 0u; ClusterLevel = 0; MaxLen = -1; MaxOps = -1 }
    let ours = ourShape request
    let theirs = oracleShape request
    let failed = match theirs with | Ok left -> not left.Successful | Error _ -> false
    let summary (result: Result<Left, string>) =
        match result with
        | Error reason -> reason
        | Ok left ->
            let first = String.Join (" ", left.Glyphs |> Array.truncate 4 |> Array.map (fun (struct (g, c, ax, _, _, _, _)) -> string g + "@" + string c + "/" + string ax))
            $"ok {left.Successful} glyphs {left.Glyphs.Length} first {first}"
    struct (failed, (match ours = theirs with | true -> None | false -> Some $"{Path.GetFileName path} x{count}: farfbuzz {summary ours}; harfbuzz {summary theirs}"))

/// <summary>Compares both engines on a synthetic font and a text with the length limit lowered by the oracle's shim.</summary>
/// <param name="path">The font.</param>
/// <param name="text">The code points.</param>
/// <param name="maxLen">The length limit.</param>
/// <returns>Whether HarfBuzz failed the buffer, and a description when the engines differ.</returns>
let private compareLimited (path: string) (text: uint32 array) (maxLen: int) =
    let request =
        { Path = path; Script = "Latn"; Text = text; Features = String.Empty; Direction = "ltr"
          Flags = 0u; ClusterLevel = 0; MaxLen = maxLen; MaxOps = -1 }
    let ours = ourShape request
    let theirs = oracleShape request
    let failed = match theirs with | Ok left -> not left.Successful | Error _ -> false
    let describe (result: Result<Left, string>) =
        match result with
        | Error reason -> reason
        | Ok left -> $"ok {left.Successful} glyphs {left.Glyphs.Length}"
    struct (failed, (match ours = theirs with | true -> None | false -> Some $"{Path.GetFileName path} {text.Length} letters, limit {maxLen}: farfbuzz {describe ours}; harfbuzz {describe theirs}"))

/// <summary>The cases of the length-limit differentials with the limit lowered: a multiple substitution of glyph 1, then a run of letters
/// no lookup covers that the lookup copies glyph by glyph, as <c>apply_forward</c> does, and the same after the run.</summary>
/// <returns>The fonts, texts and limits.</returns>
let private skippedFamilies () =
    [ for length in [ 3; 10; 33; 70 ] do
        let path = written $"expansion-{length}.ttf" (syntheticFont false [ "ccmp", (let (kind, subtable) = expansion length in lookupBytes kind [ subtable ]) ])
        for before in [ 0; 1; 5; 40 ] do
            for after in [ 1; 7; 31; 100 ] do
                let text = Array.concat [ Array.create before 0x43u; [| 0x41u |]; Array.create after 0x43u; [| 0x41u |]; Array.create after 0x44u ]
                for maxLen in [ 20; 32; 33; 34; 60; 79; 80; 81; 120; 151; 152; 153; 200 ] do
                    yield path, text, maxLen ]

/// <summary>The cases of the public-API budget differentials: a font and a number of letters.</summary>
/// <returns>The name of the family, and the fonts with their letter counts.</returns>
let private syntheticFamilies () =
    let expansions =
        [ for length in [ 70; 257; 1000; 7000; 20000 ] do
            let path = written $"expansion-{length}.ttf" (syntheticFont false [ "ccmp", (let (kind, subtable) = expansion length in lookupBytes kind [ subtable ]) ])
            for total in [ 30000; 60000; 63099; 63100; 65535; 65536; 65537; 70000; 80000; 94000; 94709; 94710; 94711; 100000; 150000 ] do
                yield path, max 1 (total / length)
            for count in [ 1; 2; 3; 5; 9; 14 ] do
                yield path, count ]
    let laughs (positioning: bool) =
        [ for (depth, fan) in [ (1, 1000); (1, 5000); (1, 20000); (2, 100); (2, 255); (2, 300); (2, 600); (3, 50); (3, 60); (4, 20); (2, 4); (6, 7); (9, 4); (17, 2) ] do
            let table = match positioning with | true -> "gpos" | false -> "gsub"
            let path = written $"laughs-{table}-{depth}-{fan}.ttf" (syntheticFont positioning (contextChain positioning depth fan true))
            for count in [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 20; 30 ] do
                yield path, count ]
    let nesting (positioning: bool) =
        [ for depth in [ 1; 2; 30; 60; 61; 62; 63; 64; 65; 66; 70 ] do
            let table = match positioning with | true -> "gpos" | false -> "gsub"
            let path = written $"nesting-{table}-{depth}.ttf" (syntheticFont positioning (contextChain positioning depth 1 true))
            for count in [ 1; 3 ] do
                yield path, count ]
    [ "expansion of a glyph by a multiple substitution past the length limit", expansions
      "context recursion of a substitution past the operation budget", laughs false
      "context recursion of a positioning past the operation budget", laughs true
      "the nesting limit of substitution contexts", nesting false
      "the nesting limit of positioning contexts", nesting true ]

/// <summary>The requests of each subject and limit that Farfbuzz still leaves differently from HarfBuzz when the length limit or the
/// operation budget fails, which DIVERGENCES.md D-11 describes: a count may only go down.</summary>
let private knownDifferences =
    Map.ofList
        [ "Arabic (Amiri)|length", 2; "Arabic (Amiri)|budget", 17; "Arabic (Nastaliq)|length", 23; "Arabic (Nastaliq)|budget", 17
          "Balinese (USE)|length", 6; "Balinese (USE)|budget", 34; "Devanagari|length", 1; "Devanagari|budget", 34
          "Duployan (USE)|length", 65; "Duployan (USE)|budget", 32; "Hangul (AdobeBlank)|length", 1; "Hangul (AdobeBlank)|budget", 10
          "Kannada|length", 3; "Kannada|budget", 172; "Khmer|budget", 13; "Latin (Roboto)|length", 3; "Latin (Roboto)|budget", 15
          "Myanmar|budget", 52; "Thai|length", 2; "Thai|budget", 15 ]

/// <summary>The budget differentials: Farfbuzz against HarfBuzz itself when the length limit or the operation budget fails.</summary>
/// <param name="oracle">The oracle in use; the differentials need the shim that lowers the limits, which only a built oracle has.</param>
/// <returns>The tests.</returns>
let tests (oracle: Oracle) =
    match oracle with
    | Bundled -> testList "Budget differentials (needs the built oracle)" []
    | Built _ ->
        testList "Budget differentials" [
            for (family, cases) in syntheticFamilies () do
                testCase $"Public API, {family}: Farfbuzz leaves in the buffer what HarfBuzz leaves" (fun () ->
                    let results = cases |> List.map (fun (path, count) -> compareSynthetic path count)
                    let failed = results |> List.filter (fun (struct (failed, _)) -> failed) |> List.length
                    let differences = results |> List.choose (fun (struct (_, difference)) -> difference)
                    printfn $"{family}: {cases.Length} cases, HarfBuzz failed the buffer in {failed}, {differences.Length} differences"
                    Expect.equal differences.Length 0 (String.concat "\n" (differences |> List.truncate 8)))
            testCase "Limit lowered, glyphs a lookup skips and the tail after its last match: Farfbuzz leaves in the buffer what HarfBuzz leaves" (fun () ->
                let results = skippedFamilies () |> List.map (fun (path, text, maxLen) -> compareLimited path text maxLen)
                let failed = results |> List.filter (fun (struct (failed, _)) -> failed) |> List.length
                let differences = results |> List.choose (fun (struct (_, difference)) -> difference)
                printfn $"skipped glyphs: {results.Length} cases, HarfBuzz failed the buffer in {failed}, {differences.Length} differences"
                Expect.equal differences.Length 0 (String.concat "\n" (differences |> List.truncate 8)))
            for subject in subjects do
                testCase $"{subject.Name}: with the length limit varied, no more requests differ from HarfBuzz than D-11 lists" (fun () ->
                    let struct (count, failed, text) = compareSubject subject 11 400 true false
                    printfn $"{subject.Name} length limit: {failed} of 400 requests failed the buffer in HarfBuzz, {count} differences"
                    let known = knownDifferences |> Map.tryFind $"{subject.Name}|length" |> Option.defaultValue 0
                    Expect.isLessThanOrEqual count known $"more requests differ from HarfBuzz than the {known} that DIVERGENCES.md D-11 lists:\n{text}")
                testCase $"{subject.Name}: with the operation budget varied, no more requests differ from HarfBuzz than D-11 lists" (fun () ->
                    let struct (count, failed, text) = compareSubject subject 12 400 false true
                    printfn $"{subject.Name} operation budget: {failed} of 400 requests failed the buffer in HarfBuzz, {count} differences"
                    let known = knownDifferences |> Map.tryFind $"{subject.Name}|budget" |> Option.defaultValue 0
                    Expect.isLessThanOrEqual count known $"more requests differ from HarfBuzz than the {known} that DIVERGENCES.md D-11 lists:\n{text}")
        ]
