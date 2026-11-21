module Farfbuzz.Diff.KerxDiff

open System
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.NoLayoutFonts
open Farfbuzz.Tests.AnchorTables

/// <summary>Writes a big-endian uint16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32 as four bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian int16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private beI16 (value: int16) = [| byte (uint16 value >>> 8); byte value |]

/// <summary>Builds a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) : OtFace =
    OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index)

/// <summary>What the shaping comparison looks at: glyph ids, clusters, positions and the defined flags.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparable (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>Whether every kerx subtable is one of the pair formats (0 or 2) and at least one exists.</summary>
/// <param name="kerx">The kerx table.</param>
/// <returns>True when only pair subtables are present.</returns>
let private pairsOnly (kerx: Kerx) : bool =
    kerx.Subtables.Length > 0
    && kerx.Subtables |> Seq.forall (fun subtable -> subtable.Format = 0 || subtable.Format = 2)

/// <summary>Every face whose kerx holds only format 0 or 2 subtables, so the oracle output comes only from the
/// ported pair machinery.</summary>
/// <returns>The faces.</returns>
let private pairFaces () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        pairsOnly (Kerx.load face))
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Generates a kerx shaping request over the pair faces.</summary>
/// <param name="fonts">The pair faces.</param>
/// <returns>The request.</returns>
let private aatRequest (fonts: TestFont array) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements fonts
        let coverage = TestFonts.coverage font
        let pool = match coverage.Length with | 0 -> [| 0x20u |] | _ -> coverage
        let! length = Gen.choose (1, 16)
        let! codepoints = Gen.arrayOfLength length (Gen.elements pool)
        let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = direction
                Flags = BufferFlags.ProduceUnsafeToConcat }
    }

/// <summary>Builds a synthetic format 0 kerx table with the given pairs.</summary>
/// <param name="pairs">The left, right and value of each pair.</param>
/// <returns>The kerx table bytes.</returns>
let private format0KerxTable (pairs: (uint16 * uint16 * int16) array) : byte array =
    let binSearch = Array.concat [ be32 (uint32 pairs.Length); be32 0u; be32 0u; be32 0u ]
    let pairBytes = pairs |> Array.collect (fun (l, r, v) -> Array.concat [ be16 (uint32 l); be16 (uint32 r); beI16 v ])
    let subtable = Array.concat [ be32 (uint32 (12 + binSearch.Length + pairBytes.Length)); be32 0u; be32 0u; binSearch; pairBytes ]
    Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]

/// <summary>Builds a single-table sfnt around a table.</summary>
/// <param name="tag">The table tag.</param>
/// <param name="table">The table bytes.</param>
/// <returns>The file bytes.</returns>
let private sfntOf (tag: Tag) (table: byte array) : byte array =
    Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u; be32 (uint32 tag); be32 0u; be32 28u; be32 (uint32 table.Length); table ]

/// <summary>Farfbuzz's sanitize verdict for a kerx table.</summary>
/// <param name="kerx">The kerx table bytes.</param>
/// <returns>True when the table loads.</returns>
let private oursSanitize (kerx: byte array) : bool =
    let face = Face.withGlyphCount 16 (Face.create (ReadOnlyMemory (sfntOf Tags.Kerx kerx)) 0u)
    Kerx.hasData (Kerx.load face)

/// <summary>The fields of the synthetic kerx table that the mutation differential rewrites.</summary>
/// <returns>The name, offset, width and sample values of each field.</returns>
let private kerxFields () : (string * int * int * uint32 array) array =
    [| "version", 0, 2, [| 0u; 1u; 3u; 0xFFFFu |]
       "table count", 4, 4, [| 0u; 1u; 2u; 0xFFFFFFFFu |]
       "subtable length", 8, 4, [| 0u; 1u; 11u; 13u; 0x1000u; 0xFFFFFFFFu |]
       "coverage", 12, 4, [| 0u; 2u; 6u; 0x10000000u; 0x80000000u; 0xFFFFFFFFu |]
       "tuple count", 16, 4, [| 0u; 1u; 2u; 0xFFFFFFFFu |]
       "pair count", 20, 4, [| 0u; 1u; 3u; 0xFFFFFFFFu |]
       "pair left", 36, 2, [| 0u; 1u; 2u; 0xFFFFu |]
       "pair right", 38, 2, [| 0u; 1u; 2u; 0xFFFFu |]
       "pair value", 40, 2, [| 0u; 1u; 0x7FFFu; 0x8000u; 0xFFFFu |] |]

/// <summary>Writes a field value into a byte array.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The field offset.</param>
/// <param name="width">The field width, 1, 2 or 4.</param>
/// <param name="value">The value.</param>
let private writeField (bytes: byte array) (offset: int) (width: int) (value: uint32) =
    match width with
    | 4 ->
        bytes[offset] <- byte (value >>> 24)
        bytes[offset + 1] <- byte (value >>> 16)
        bytes[offset + 2] <- byte (value >>> 8)
        bytes[offset + 3] <- byte value
    | 1 -> bytes[offset] <- byte value
    | _ ->
        bytes[offset] <- byte (value >>> 8)
        bytes[offset + 1] <- byte value

/// <summary>A synthetic table and the fields the mutation differential rewrites one at a time.</summary>
type private Synthetic =
    { Bytes: byte array
      Fields: (string * int * int) array }

/// <summary>Writes a big-endian uint16 list.</summary>
/// <param name="values">The values.</param>
/// <returns>The bytes.</returns>
let private be16s (values: uint32 array) = values |> Array.collect be16

/// <summary>Builds a kerx table with one format 1 subtable: two states over the classes of three glyphs, four entries
/// and five kern action words, every count, offset, flag and index field named.</summary>
/// <param name="glyphs">The glyphs of the three classes 4, 5 and 6.</param>
/// <returns>The table and its fields.</returns>
let private kerxSynthetic (glyphs: uint32 array) : Synthetic =
    let first = Array.min glyphs
    let count = int (Array.max glyphs - first) + 1
    let classOf glyph = match Array.tryFindIndex ((=) glyph) glyphs with | Some index -> uint32 (index + 4) | None -> 1u
    let classes = Array.init count (fun i -> classOf (first + uint32 i))
    let lookupAt = 40
    let stateAt = lookupAt + 6 + 2 * count
    let entryAt = stateAt + 28
    let actionAt = entryAt + 24
    let total = actionAt + 10
    let states = be16s [| 0u; 0u; 0u; 0u; 1u; 0u; 0u; 0u; 0u; 0u; 0u; 2u; 3u; 0u |]
    let entries = be16s [| 0u; 0u; 0xFFFFu; 1u; 0x8000u; 0xFFFFu; 1u; 0x8000u; 0u; 0u; 0u; 3u |]
    let actions = be16s [| 30u; 40u; 0xFFFFu; 40u; 0xFFFFu |]
    let subtable =
        Array.concat
            [ be32 (uint32 (total - 8)); be32 1u; be32 0u
              be32 7u; be32 (uint32 (lookupAt - 20)); be32 (uint32 (stateAt - 20)); be32 (uint32 (entryAt - 20)); be32 (uint32 (actionAt - 20))
              be16 8u; be16 first; be16 (uint32 count); be16s classes; states; entries; actions ]
    let fields =
        [| yield "version", 0, 2
           yield "table count", 4, 4
           yield "subtable length", 8, 4
           yield "coverage", 12, 4
           yield "tuple count", 16, 4
           yield "nClasses", 20, 4
           yield "class table offset", 24, 4
           yield "state array offset", 28, 4
           yield "entry table offset", 32, 4
           yield "kern action offset", 36, 4
           yield "class lookup format", lookupAt, 2
           yield "class lookup first glyph", lookupAt + 2, 2
           yield "class lookup count", lookupAt + 4, 2
           for i in 0 .. count - 1 do yield $"class of glyph {i}", lookupAt + 6 + 2 * i, 2
           for i in 0 .. 13 do yield $"state array slot {i}", stateAt + 2 * i, 2
           for i in 0 .. 3 do
               yield $"entry {i} newState", entryAt + 6 * i, 2
               yield $"entry {i} flags", entryAt + 6 * i + 2, 2
               yield $"entry {i} kern action index", entryAt + 6 * i + 4, 2
           for i in 0 .. 4 do yield $"kern action {i}", actionAt + 2 * i, 2 |]
    { Bytes = Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]; Fields = fields }

/// <summary>Builds an Apple kern table with one format 1 subtable: two states over the classes of three glyphs, four
/// entries and three kern action words, every count, offset, flag and index field named.</summary>
/// <param name="glyphs">The glyphs of the three classes 4, 5 and 6.</param>
/// <returns>The table and its fields.</returns>
let private kernSynthetic (glyphs: uint32 array) : Synthetic =
    let first = Array.min glyphs
    let count = int (Array.max glyphs - first) + 1
    let classOf glyph = match Array.tryFindIndex ((=) glyph) glyphs with | Some index -> byte (index + 4) | None -> 1uy
    let classes = Array.init count (fun i -> classOf (first + uint32 i))
    let classAt = 10
    let stateAt = (classAt + 4 + count + 1) &&& ~~~1
    let entryAt = stateAt + 14
    let actionAt = entryAt + 16
    let total = actionAt + 6
    let row (state: int) = uint32 (stateAt + 7 * state)
    let states = [| 0uy; 0uy; 0uy; 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 2uy; 3uy; 0uy |]
    let entries = be16s [| row 0; 0u; row 1; 0x8000u; row 1; 0x8000u ||| uint32 actionAt; row 0; uint32 (actionAt + 4) |]
    let actions = be16s [| 30u; 41u; 51u |]
    let subtable =
        Array.concat
            [ be32 (uint32 (8 + total)); [| 0uy; 1uy |]; be16 0u
              be16 7u; be16 (uint32 classAt); be16 (uint32 stateAt); be16 (uint32 entryAt); be16 (uint32 actionAt)
              be16 first; be16 (uint32 count); classes
              Array.zeroCreate (stateAt - classAt - 4 - count); states; entries; actions ]
    let fields =
        [| yield "version", 0, 4
           yield "table count", 4, 4
           yield "subtable length", 8, 4
           yield "coverage", 12, 1
           yield "format", 13, 1
           yield "tuple index", 14, 2
           yield "nClasses", 16, 2
           yield "class table offset", 18, 2
           yield "state array offset", 20, 2
           yield "entry table offset", 22, 2
           yield "kern action offset", 24, 2
           yield "class table first glyph", 16 + classAt, 2
           yield "class table count", 16 + classAt + 2, 2
           for i in 0 .. count - 1 do yield $"class of glyph {i}", 16 + classAt + 4 + i, 1
           for i in 0 .. 13 do yield $"state array slot {i}", 16 + stateAt + i, 1
           for i in 0 .. 3 do
               yield $"entry {i} newState", 16 + entryAt + 4 * i, 2
               yield $"entry {i} flags", 16 + entryAt + 4 * i + 2, 2
           for i in 0 .. 2 do yield $"kern action {i}", 16 + actionAt + 2 * i, 2 |]
    { Bytes = Array.concat [ be32 0x00010000u; be32 1u; subtable ]; Fields = fields }

/// <summary>Reads a field's current value.</summary>
/// <param name="bytes">The table.</param>
/// <param name="offset">The offset.</param>
/// <param name="width">The width, 1, 2 or 4.</param>
/// <returns>The value.</returns>
let private readField (bytes: byte array) (offset: int) (width: int) =
    match width with
    | 4 -> (uint32 bytes[offset] <<< 24) ||| (uint32 bytes[offset + 1] <<< 16) ||| (uint32 bytes[offset + 2] <<< 8) ||| uint32 bytes[offset + 3]
    | 1 -> uint32 bytes[offset]
    | _ -> (uint32 bytes[offset] <<< 8) ||| uint32 bytes[offset + 1]

/// <summary>Every mutated table of a synthetic table: each field rewritten to 0, 1, its value minus and plus one, the
/// largest value of its width and an offset past the table, then the table cut short at several lengths.</summary>
/// <param name="synthetic">The table and its fields.</param>
/// <returns>A description and the mutated table for each.</returns>
let private mutations (synthetic: Synthetic) : (string * byte array) array =
    let bytes = synthetic.Bytes
    let largest width = match width with | 4 -> 0xFFFFFFFFu | 1 -> 0xFFu | _ -> 0xFFFFu
    let rewritten =
        synthetic.Fields
        |> Array.collect (fun (name, offset, width) ->
            let current = readField bytes offset width
            [| 0u; 1u; current - 1u; current + 1u; largest width; uint32 bytes.Length + 1u; 0x4000u; 0xC000u |]
            |> Array.map (fun value -> if width = 4 then value else value &&& largest width)
            |> Array.distinct
            |> Array.filter (fun value -> value <> current)
            |> Array.map (fun value ->
                let candidate = Array.copy bytes
                writeField candidate offset width value
                $"{name} = {value}", candidate))
    let cuts =
        [| 0; 4; 8; 12; bytes.Length / 2; bytes.Length - 12; bytes.Length - 6; bytes.Length - 3; bytes.Length - 2; bytes.Length - 1 |]
        |> Array.distinct
        |> Array.map (fun length -> $"truncated to {length} bytes", Array.truncate length bytes)
    let endless =
        let candidate = Array.copy bytes
        synthetic.Fields
        |> Array.filter (fun (name, _, _) -> name.EndsWith " flags" && name.StartsWith "entry")
        |> Array.iter (fun (_, offset, width) -> writeField candidate offset width 0x4000u)
        "every entry DontAdvance", candidate
    Array.append (Array.append rewritten [| endless |]) cuts

/// <summary>Builds a font file from another by adding or replacing one table.</summary>
/// <param name="font">The font file bytes, a single sfnt.</param>
/// <param name="tag">The new table's tag.</param>
/// <param name="table">The table bytes.</param>
/// <returns>The new file bytes.</returns>
let private withTable (font: byte array) (tag: Tag) (table: byte array) : byte array =
    let count = int (readField font 4 2)
    let existing =
        [| for i in 0 .. count - 1 do
            let record = 12 + 16 * i
            let recordTag = readField font record 4
            if recordTag <> uint32 tag then
                let offset = int (readField font (record + 8) 4)
                let length = int (readField font (record + 12) 4)
                yield recordTag, font[offset .. offset + length - 1] |]
    let tables = Array.append existing [| uint32 tag, table |] |> Array.sortBy fst
    let dataStart = 12 + 16 * tables.Length
    let padded (bytes: byte array) = (bytes.Length + 3) &&& ~~~3
    let offsets = tables |> Array.scan (fun offset (_, bytes) -> offset + padded bytes) dataStart
    let directory =
        tables |> Array.mapi (fun i (recordTag, bytes) -> Array.concat [ be32 recordTag; be32 0u; be32 (uint32 offsets[i]); be32 (uint32 bytes.Length) ])
    let data = tables |> Array.map (fun (_, bytes) -> Array.append bytes (Array.zeroCreate (padded bytes - bytes.Length)))
    Array.concat [ be32 0x00010000u; be16 (uint32 tables.Length); be16 0u; be16 0u; be16 0u; Array.concat directory; Array.concat data ]

/// <summary>The smallest corpus face, with its layout tables stripped, that is a single sfnt and maps a, b, c and d.</summary>
/// <returns>The face, the glyphs of a, b, c and d, and its glyph count.</returns>
let private baseFace () : TestFont * uint32 array * uint32 =
    NoLayoutFonts.all ()
    |> Seq.filter (fun font -> font.Index = 0 && (FileInfo font.Path).Length < 400000L)
    |> Seq.sortBy (fun font -> (FileInfo font.Path).Length)
    |> Seq.choose (fun font ->
        let bytes = OracleFaces.data font.Path
        match BinaryCalls.u32 (ReadOnlySpan bytes) 0 = 0x00010000u with
        | false -> None
        | true ->
            let handle = OracleFaces.font font 0u
            let glyphs =
                [| 'a'; 'b'; 'c'; 'd' |]
                |> Array.map (fun letter ->
                    let one = Array.zeroCreate 1
                    Shims.farfbuzz_oracle_nominal_glyph_range(handle, uint32 letter, 1u, one) |> ignore
                    one[0])
            let classed = glyphs[0..2]
            let spread = Array.max classed - Array.min classed
            match Array.forall (fun glyph -> glyph <> 0u && glyph <> 0xFFFFFFFFu) glyphs && Array.distinct glyphs |> Array.length = 4 && spread < 40u with
            | true -> Some (font, glyphs, Native.hb_face_get_glyph_count (OracleFaces.face font 0u))
            | false -> None)
    |> Seq.head

/// <summary>The directory the mutated fonts are written to.</summary>
/// <returns>The path.</returns>
let private mutationDirectory () =
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "kerx-format1-mutation")
    Directory.CreateDirectory directory |> ignore
    directory

/// <summary>Writes a font file once under a name made from its bytes.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The path.</returns>
let private writeMutated (bytes: byte array) =
    let name = Convert.ToHexString(Security.Cryptography.SHA1.HashData bytes).ToLowerInvariant()
    let target = Path.Combine(mutationDirectory (), $"{name}.ttf")
    match File.Exists target with
    | true -> ()
    | false ->
        let temporary = $"{target}.{Guid.NewGuid():N}.tmp"
        File.WriteAllBytes(temporary, bytes)
        File.Move(temporary, target, true)
    target

/// <summary>The shaping requests of one mutated font: random short texts over the three classed letters and one
/// other, in both directions.</summary>
/// <param name="path">The font path.</param>
/// <param name="random">The random source.</param>
/// <param name="count">The number of requests.</param>
/// <returns>The requests.</returns>
let private mutationRequests (path: string) (random: Random) (count: int) =
    let letters = "abcabcd"
    Array.init count (fun _ ->
        let length = random.Next (1, 11)
        let text = Array.init length (fun _ -> uint32 (letters[random.Next 7]))
        { defaults path text with
            Shaper = ValueSome "ot"
            FontFuncs = ValueSome "ot"
            Direction = ValueSome (if random.Next 2 = 0 then "ltr" else "rtl")
            Flags = BufferFlags.ProduceUnsafeToConcat })

/// <summary>Writes a comparable outcome as text.</summary>
/// <param name="outcome">The comparable tuples or the reason there are none.</param>
/// <returns>The text.</returns>
let private outcomeText (outcome: Result<struct (uint32 * uint32 * int * int * int * int * uint32) array, string>) =
    match outcome with
    | Ok glyphs -> glyphs |> Array.map (fun (struct (glyph, cluster, xAdvance, yAdvance, xOffset, yOffset, flags)) -> $"g{glyph}/c{cluster}/{xAdvance},{yAdvance}/{xOffset},{yOffset}/f{flags}") |> String.concat " "
    | Error reason -> reason

/// <summary>The changed-bytes differential: every mutated table put into a real font, shaped by both engines, 5,000
/// shaping cases per format, and the sanitize verdict of both engines on every mutated table.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <param name="name">The test name.</param>
/// <param name="tag">The table tag.</param>
/// <param name="synthetic">Builds the table from the glyphs.</param>
/// <param name="oracleSanitize">The HarfBuzz sanitize verdict.</param>
/// <param name="farfbuzzSanitize">The Farfbuzz verdict on the file.</param>
/// <returns>The test.</returns>
let private changedBytesTest (oracle: Shaper) (farfbuzz: Shaper) (name: string) (tag: Tag) (synthetic: uint32 array -> Synthetic)
    (oracleSanitize: byte array -> uint32 -> bool) (farfbuzzSanitize: byte array -> bool) =
    testCase name (fun () ->
        let font, glyphs, glyphCount = baseFace ()
        let baseBytes = File.ReadAllBytes font.Path
        let tables = Array.append [| "unchanged", (synthetic glyphs[0..2]).Bytes |] (mutations (synthetic glyphs[0..2]))
        let perTable = (5000 + tables.Length - 1) / tables.Length
        let random = Random 20260314
        let failures = ResizeArray<string>()
        let mutable cases = 0
        let mutable rejected = 0
        let mutable disagreeing = 0
        let mutable kerned = 0
        for description, table in tables do
            let file = withTable baseBytes tag table
            let path = writeMutated file
            let theirs = oracleSanitize table glyphCount
            let ours = farfbuzzSanitize file
            if not theirs then rejected <- rejected + 1
            if ours <> theirs then
                disagreeing <- disagreeing + 1
                failures.Add $"{description}: HarfBuzz sanitizes {theirs}, Farfbuzz {ours}"
            for request in mutationRequests path random perTable do
                if cases < 5000 then
                    cases <- cases + 1
                    let expected = comparable (oracle.Shape request)
                    let actual = comparable (farfbuzz.Shape request)
                    if expected <> actual then failures.Add $"{description}, text {String.Join(',', request.Text)}, {request.Direction}: HarfBuzz {outcomeText expected}, Farfbuzz {outcomeText actual}"
                    if description = "unchanged" then
                        let plain = comparable (oracle.Shape { request with FontPath = font.Path })
                        if expected <> plain then kerned <- kerned + 1
        printfn "P8.S11 %s: %d shaping cases over %d mutated tables; HarfBuzz rejects %d tables, Farfbuzz rejects the same %d (%d disagree); the unchanged table changes %d of its cases" name cases tables.Length rejected (rejected - disagreeing) disagreeing kerned
        Expect.isEmpty failures ($"{name} differences:\n" + String.concat "\n" (Seq.truncate 20 failures))
        Expect.isGreaterThan kerned 0 "the unchanged table kerns at least one case, so the comparison is not vacuous")

/// <summary>Whether a face's kerx has at least one format 1 subtable and no format 4.</summary>
/// <param name="kerx">The kerx table.</param>
/// <returns>True when format 1 is present and every subtable is a ported pair or format 1 kind.</returns>
let private format1Only (kerx: Kerx) : bool =
    kerx.Subtables |> Seq.exists (fun subtable -> subtable.Format = 1)
    && kerx.Subtables |> Seq.forall (fun subtable -> subtable.Format = 0 || subtable.Format = 1 || subtable.Format = 2 || subtable.Format = 6)

/// <summary>Every face whose kerx holds a format 1 subtable and no format 4, and that has no GPOS, so the oracle
/// output comes from the ported kerx machinery.</summary>
/// <returns>The faces.</returns>
let private format1Faces () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        let oracleFace = OracleFaces.face font index
        format1Only (Kerx.load face)
        && Native.hb_ot_layout_has_positioning oracleFace = 0)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Every face whose kern table has a format 1 state machine and that has no kerx or GPOS, so the oracle
/// output comes from the ported Apple kern format 1 machinery.</summary>
/// <returns>The faces.</returns>
let private kernFormat1Faces () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        let oracleFace = OracleFaces.face font index
        KernTable.hasStateMachine (KernTable.load face)
        && Native.hb_aat_layout_has_positioning oracleFace = 0
        && Native.hb_ot_layout_has_positioning oracleFace = 0)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Whether a face's kerx has at least one format 4 subtable and only ported kinds.</summary>
/// <param name="kerx">The kerx table.</param>
/// <returns>True when format 4 is present and every subtable is a ported kind.</returns>
let private format4Present (kerx: Kerx) : bool =
    kerx.Subtables |> Seq.exists (fun subtable -> subtable.Format = 4)
    && kerx.Subtables |> Seq.forall (fun subtable -> subtable.Format = 0 || subtable.Format = 1 || subtable.Format = 2 || subtable.Format = 4 || subtable.Format = 6)

/// <summary>Every face whose kerx holds a format 4 subtable and that has no GPOS, so the oracle output comes from the
/// ported kerx machinery.</summary>
/// <returns>The faces.</returns>
let private format4Faces () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        format4Present (Kerx.load face)
        && Native.hb_ot_layout_has_positioning (OracleFaces.face font index) = 0)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Whether a face's kerx has only format 4 subtables.</summary>
/// <param name="font">The font.</param>
/// <returns>True when every kerx subtable of the face is format 4.</returns>
let private onlyFormat4 (font: TestFont) : bool =
    let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index)
    (Kerx.load face).Subtables |> Seq.forall (fun subtable -> subtable.Format = 4)

/// <summary>One real face's ankr table: the Farfbuzz table, the raw bytes HarfBuzz is given and the glyph count.</summary>
type private RealAnkr =
    { Table: Ankr
      Bytes: byte array
      NumGlyphs: int }

/// <summary>Loads the ankr table of a face.</summary>
/// <param name="font">The font.</param>
/// <returns>The table, its raw bytes and the glyph count.</returns>
let private realAnkr (font: TestFont) : RealAnkr =
    let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index)
    { Table = Ankr.load face
      Bytes = (Face.referenceTable face Tags.Ankr).ToArray()
      NumGlyphs = face.GlyphCount }

/// <summary>The ankr tables of the format 4 faces, loaded once.</summary>
let private realAnkrs = lazy (format4Faces () |> Array.map realAnkr)

/// <summary>Random shaping requests over a set of faces, short texts over each face's coverage in both directions.</summary>
/// <param name="fonts">The faces.</param>
/// <param name="random">The random source.</param>
/// <param name="count">The number of requests.</param>
/// <returns>The requests.</returns>
let private randomRequests (fonts: TestFont array) (random: Random) (count: int) =
    Array.init count (fun _ ->
        let font = fonts[random.Next fonts.Length]
        let coverage = TestFonts.coverage font
        let pool = match coverage.Length with | 0 -> [| 0x20u |] | _ -> coverage
        let codepoints = Array.init (random.Next (1, 17)) (fun _ -> pool[random.Next pool.Length])
        { defaults font.Path codepoints with
            FaceIndex = font.Index
            Shaper = ValueSome "ot"
            FontFuncs = ValueSome "ot"
            Direction = ValueSome (if random.Next 2 = 0 then "ltr" else "rtl")
            Flags = BufferFlags.ProduceUnsafeToConcat })

/// <summary>The anchor Farfbuzz gives a glyph.</summary>
/// <param name="table">The ankr table.</param>
/// <param name="numGlyphs">The glyph count.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="index">The anchor index.</param>
/// <returns>The coordinates.</returns>
let private oursAnchor (table: Ankr) (numGlyphs: int) (glyph: uint32) (index: uint32) =
    let struct (x, y) = Ankr.anchor table glyph index numGlyphs
    struct (x, y)

/// <summary>The anchor HarfBuzz gives a glyph from the raw table bytes.</summary>
/// <param name="bytes">The raw table.</param>
/// <param name="numGlyphs">The glyph count.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="index">The anchor index.</param>
/// <returns>The coordinates.</returns>
let private theirsAnchor (bytes: byte array) (numGlyphs: int) (glyph: uint32) (index: uint32) =
    let packed = Shims.farfbuzz_oracle_ankr_anchor(bytes, uint32 bytes.Length, uint32 numGlyphs, glyph, index)
    struct (int (int16 (packed >>> 16)), int (int16 packed))

/// <summary>Whether Farfbuzz accepts an ankr table's bytes: a table that fails to sanitize loads as the empty table.</summary>
/// <param name="bytes">The raw table.</param>
/// <param name="numGlyphs">The glyph count.</param>
/// <returns>The table and whether it was accepted.</returns>
let private oursAnkr (bytes: byte array) (numGlyphs: int) =
    let face = Face.withGlyphCount numGlyphs (Face.create (ReadOnlyMemory (sfntOf Tags.Ankr bytes)) 0u)
    let table = Ankr.load face
    table, table.Table.Length > 0

/// <summary>The anchors of the glyphs and indices a differential asks, as text.</summary>
/// <param name="numGlyphs">The glyph count.</param>
/// <param name="anchor">The anchor function.</param>
/// <returns>One string.</returns>
let private anchorsText (numGlyphs: int) (anchor: uint32 -> uint32 -> struct (int * int)) =
    [ for glyph in 0u .. uint32 numGlyphs + 2u do
        for index in [ 0u; 1u; 2u; 3u; 0xFFFFu ] do
            let struct (x, y) = anchor glyph index
            yield $"{x},{y}" ]
    |> String.concat " "

/// <summary>The glyph models the synthetic ankr tables are built from.</summary>
let private ankrModels : Map<uint32, (int * int) array> array =
    [| Map.ofList [ 1u, [| (100, 200); (-30, 50) |]; 2u, [| (7, 7) |]; 3u, [| (1, 2); (3, 4); (5, 6) |] ]
       Map.ofList [ 2u, [| (32767, -32768) |]; 3u, [||]; 4u, [| (9, 8); (7, 6) |]; 10u, [| (-1, -1) |]; 11u, [| (2, 3); (4, 5) |] ]
       Map.ofList [ 0u, [| (5, -5); (6, -6); (7, -7); (8, -8) |]; 5u, [| (-300, 300) |]; 6u, [| (1, 1); (2, 2) |]; 7u, [||]; 12u, [| (0, 0) |]; 13u, [| (1, -1); (-1, 1) |] ]
       Map.ofList [ 1u, [| (10, 10) |]; 2u, [| (20, 20) |]; 3u, [| (30, 30) |]; 4u, [| (40, 40) |]; 5u, [| (50, 50) |] ]
       Map.empty |]

/// <summary>The glyph count of the synthetic ankr tables.</summary>
[<Literal>]
let private AnkrGlyphs = 14

/// <summary>The lookup formats of the synthetic ankr tables.</summary>
let private ankrKinds = [| Dense; Segments; SegmentArrays; Singles; Trimmed |]

/// <summary>The fields of an ankr table a changed-bytes test rewrites: the header and every 16-bit word after it.</summary>
/// <param name="bytes">The table.</param>
/// <returns>The fields.</returns>
let private ankrFields (bytes: byte array) : (string * int * int) array =
    [| yield "version", 0, 2
       yield "flags", 2, 2
       yield "lookup offset", 4, 4
       yield "anchor data offset", 8, 4
       for position in 12 .. 2 .. bytes.Length - 2 do yield $"word at {position}", position, 2 |]

/// <summary>Every font the shaping changed-bytes differential shapes: the unchanged tables and each mutated table of the
/// format 4 subtable and the ankr table, with the other table kept good.</summary>
/// <param name="glyphs">The glyphs of the letters a, b, c and d.</param>
/// <returns>A family, a description, the kerx table and the ankr table of each.</returns>
let private format4Fonts (glyphs: uint32 array) : (string * string * byte array * byte array) array =
    let model = Map.ofList [ glyphs[0], [| (100, 200); (-30, 50) |]; glyphs[1], [| (7, 9); (1, 1); (4, 4) |]; glyphs[2], [| (-5, 6) |]; glyphs[3], [| (3, 3) |] ]
    let goodAnkr = ankr Segments 0 model
    let words = [| 0; 1; 1; 0; 7; 1; 2; 9 |]
    let kerxFamilies =
        [| for actionType in [ 1; 2; 0; 3 ] do
               let table = kerx4 glyphs[0..2] actionType words false
               yield "kerx4 type " + string actionType, { Bytes = table.Bytes; Fields = table.Fields }
           let trailing = kerx4 glyphs[0..2] 1 words true
           yield "kerx4 followed by a subtable", { Bytes = trailing.Bytes; Fields = trailing.Fields } |]
    let kerxFonts =
        kerxFamilies
        |> Array.collect (fun (family, synthetic) ->
            Array.append [| "unchanged", synthetic.Bytes |] (mutations synthetic)
            |> Array.map (fun (description, table) -> family, description, table, goodAnkr))
    let goodKerx = (kerx4 glyphs[0..2] 1 words false).Bytes
    let ankrFonts =
        [| Segments; SegmentArrays; Singles; Trimmed |]
        |> Array.collect (fun kind ->
            let table = ankr kind 0 model
            mutations { Bytes = table; Fields = ankrFields table }
            |> Array.map (fun (description, mutated) -> $"ankr {kind}", description, goodKerx, mutated))
    Array.append kerxFonts ankrFonts

/// <summary>The count under a key, zero when there is none.</summary>
/// <param name="counts">The counts.</param>
/// <param name="key">The key.</param>
/// <returns>The count.</returns>
let private countOf (counts: Collections.Generic.Dictionary<string, int>) (key: string) =
    match counts.TryGetValue key with
    | true, count -> count
    | false, _ -> 0

/// <summary>The changed-bytes shaping differential of the format 4 subtable and the ankr table: every mutated table put
/// into a real font beside the other, good table, shaped by both engines, 5,000 shaping cases, and the sanitize verdict
/// of both engines on every table.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test.</returns>
let private format4ChangedBytes (oracle: Shaper) (farfbuzz: Shaper) =
    testCase "kerx format 4 and ankr changed bytes" (fun () ->
        let font, glyphs, glyphCount = baseFace ()
        let baseBytes = File.ReadAllBytes font.Path
        let fonts = format4Fonts glyphs
        let unchangedCount = fonts |> Array.filter (fun (_, description, _, _) -> description = "unchanged") |> Array.length
        let unchangedCases = 40
        let extraBudget = max 0 (5000 - unchangedCount * unchangedCases - (fonts.Length - unchangedCount))
        let withoutFlags (outcome: Result<struct (uint32 * uint32 * int * int * int * int * uint32) array, string>) =
            outcome |> Result.map (Array.map (fun (struct (glyph, cluster, xAdvance, yAdvance, xOffset, yOffset, _)) -> struct (glyph, cluster, xAdvance, yAdvance, xOffset, yOffset)))
        let random = Random 20260315
        let failures = ResizeArray<string>()
        let mutable cases = 0
        let mutable extraGiven = 0
        let mutable rejectedKerx = 0
        let mutable rejectedAnkr = 0
        let mutable disagreeing = 0
        let mutable moved = 0
        let attachedBy = Collections.Generic.Dictionary<string, int>()
        let unchangedMoved = Collections.Generic.Dictionary<string, int>()
        let unchangedAccepted = Collections.Generic.Dictionary<string, bool>()
        for family, description, kerx, ankrTable in fonts do
            let theirsKerx = Shims.farfbuzz_oracle_kerx_load_sane(kerx, uint32 kerx.Length, uint32 glyphCount) <> 0
            let theirsAnkr = Shims.farfbuzz_oracle_ankr_sanitize(ankrTable, uint32 ankrTable.Length, glyphCount) <> 0
            let file = withTable (withTable baseBytes Tags.Kerx kerx) Tags.Ankr ankrTable
            let path = writeMutated file
            let face = Face.create (ReadOnlyMemory file) 0u
            let oursKerx = Kerx.hasData (Kerx.load face)
            let oursAnkr = (Ankr.load face).Table.Length > 0
            if not theirsKerx then rejectedKerx <- rejectedKerx + 1
            if not theirsAnkr then rejectedAnkr <- rejectedAnkr + 1
            if oursKerx <> theirsKerx || oursAnkr <> theirsAnkr then
                disagreeing <- disagreeing + 1
                failures.Add $"{family}, {description}: HarfBuzz sanitizes kerx {theirsKerx} ankr {theirsAnkr}, Farfbuzz kerx {oursKerx} ankr {oursAnkr}"
            if description = "unchanged" then unchangedAccepted[family] <- theirsKerx && theirsAnkr && oursKerx && oursAnkr
            let perTable = if description = "unchanged" then unchangedCases elif extraGiven < extraBudget then 2 else 1
            if description <> "unchanged" && extraGiven < extraBudget then extraGiven <- extraGiven + 1
            for request in mutationRequests path random perTable do
                if cases < 5000 then
                    cases <- cases + 1
                    let expected = comparable (oracle.Shape request)
                    let actual = comparable (farfbuzz.Shape request)
                    if expected <> actual then failures.Add $"{family}, {description}, text {String.Join(',', request.Text)}, {request.Direction}: HarfBuzz {outcomeText expected}, Farfbuzz {outcomeText actual}"
                    let plain = comparable (oracle.Shape { request with FontPath = font.Path })
                    if withoutFlags expected <> withoutFlags plain then
                        moved <- moved + 1
                        attachedBy[family] <- countOf attachedBy family + 1
                        if description = "unchanged" then unchangedMoved[family] <- countOf unchangedMoved family + 1
        let report = unchangedMoved |> Seq.map (fun pair -> $"{pair.Key} {pair.Value}") |> String.concat ", "
        printfn "P8.S12 kerx format 4 and ankr changed bytes: %d shaping cases over %d tables; HarfBuzz rejects %d kerx tables and %d ankr tables, Farfbuzz rejects the same ones (%d disagree); %d cases differ from the font without the tables in a glyph, cluster, advance or offset; of the %d cases of the unchanged tables, the ones that differ: %s" cases fonts.Length rejectedKerx rejectedAnkr disagreeing moved (unchangedCount * unchangedCases) report
        Expect.isEmpty failures ("kerx format 4 and ankr differences:\n" + String.concat "\n" (Seq.truncate 20 failures))
        Expect.isTrue (unchangedAccepted.Values |> Seq.forall id) "HarfBuzz and Farfbuzz both accept every unchanged table"
        Expect.isGreaterThan (countOf unchangedMoved "kerx4 type 1") 0 "the unchanged anchor point table moves glyphs"
        Expect.isGreaterThan (countOf unchangedMoved "kerx4 type 2") 0 "the unchanged coordinate table moves glyphs")

/// <summary>The ankr tables the direct differential compares: every lookup format and model, unchanged and with one word
/// rewritten at a time, 5,000 tables, and the table cut short.</summary>
/// <returns>A description and the table bytes of each.</returns>
let private ankrTables () : (string * byte array) array =
    [| for modelIndex in 0 .. ankrModels.Length - 1 do
        for kind in ankrKinds do
            let table = ankr kind AnkrGlyphs ankrModels[modelIndex]
            yield! Array.append [| "unchanged", table |] (mutations { Bytes = table; Fields = ankrFields table })
            |> Array.map (fun (description, bytes) -> $"{kind} model {modelIndex} {description}", bytes) |]

/// <summary>The P8.S10 and P8.S11 kerx and kern differentials.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    testList "P8" [
        testList "P8.S10" [
            exhaustive "P8.S10 has positioning" (OracleFaces.all ())
                (fun struct (font, index) -> AatLayout.hasPositioning (faceOf font index))
                (fun struct (font, index) -> Native.hb_aat_layout_has_positioning (OracleFaces.face font index) <> 0)
            sampled "P8.S10 shape kerx pairs"
                (aatRequest (pairFaces ()))
                2000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            testCase "P8.S10 kerx changed bytes" (fun () ->
                let baseKerx = format0KerxTable [| 1us, 1us, 500s; 2us, 3us, -80s |]
                let fields = kerxFields ()
                let failures = ResizeArray<string>()
                let mutable cases = 0
                let mutable oracleRejects = 0
                let mutable state = 24680u
                let nextRandom () =
                    state <- state * 1103515245u + 12345u
                    state
                let check (kerx: byte array) (description: string) =
                    let ours = oursSanitize kerx
                    let theirs = Shims.farfbuzz_oracle_kerx_sanitize(kerx, uint32 kerx.Length, 16u) <> 0
                    match ours = theirs with
                    | true ->
                        match theirs with
                        | false -> oracleRejects <- oracleRejects + 1
                        | true -> ()
                    | false -> failures.Add $"{description}: oracle {theirs}, farfbuzz {ours}"
                    cases <- cases + 1
                let mutable attempt = 0
                while cases < 5000 && attempt < 200000 do
                    attempt <- attempt + 1
                    let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                    for value in values do
                        if cases < 5000 then
                            let candidate = Array.copy baseKerx
                            writeField candidate offset width value
                            check candidate $"{name} = {value}"
                while cases < 5000 do
                    let cutBy = int (nextRandom () % 30u) + 1
                    let candidate = Array.truncate (max 1 (baseKerx.Length - cutBy)) baseKerx
                    check candidate $"truncated tail by {cutBy}"
                printfn "P8.S10 compared %d mutated kerx tables; HarfBuzz rejects %d" cases oracleRejects
                Expect.isEmpty failures ("mutated kerx differences:\n" + String.concat "\n" failures))
        ]
        testList "P8.S11" [
            testCase "P8.S11 format 1 faces" (fun () ->
                let kerxFaces = format1Faces ()
                let kernFaces = kernFormat1Faces ()
                printfn "P8.S11 real faces: %d with kerx format 1 and no GPOS, %d with Apple kern format 1 and no kerx or GPOS" kerxFaces.Length kernFaces.Length
                Expect.isGreaterThan kerxFaces.Length 0 "there are kerx format 1 faces"
                Expect.isGreaterThan kernFaces.Length 0 "there are kern format 1 faces")
            sampled "P8.S11 shape kerx format 1"
                (aatRequest (format1Faces ()))
                3000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            sampled "P8.S11 shape kern format 1"
                (aatRequest (kernFormat1Faces ()))
                500
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            changedBytesTest oracle farfbuzz "kerx format 1 changed bytes" Tags.Kerx kerxSynthetic
                (fun table glyphCount -> Shims.farfbuzz_oracle_kerx_sanitize(table, uint32 table.Length, glyphCount) <> 0)
                (fun file -> Kerx.hasData (Kerx.load (Face.create (ReadOnlyMemory file) 0u)))
            changedBytesTest oracle farfbuzz "kern format 1 changed bytes" Tags.Kern kernSynthetic
                (fun table glyphCount -> Shims.farfbuzz_oracle_kern_sanitize(table, uint32 table.Length, glyphCount) <> 0 && table.Length >= 4 && readField table 0 4 <> 0u && readField table 0 2 <= 1u)
                (fun file -> KernTable.hasData (KernTable.load (Face.create (ReadOnlyMemory file) 0u)))
        ]
        testList "P8.S12" [
            testCase "P8.S12 format 4 faces" (fun () ->
                let faces = format4Faces ()
                let only = faces |> Array.filter onlyFormat4
                printfn "P8.S12 real faces: %d with kerx format 4, an ankr table and no GPOS, %d of them with only format 4 subtables" faces.Length only.Length
                Expect.isGreaterThan faces.Length 0 "there are kerx format 4 faces"
                Expect.isGreaterThan only.Length 0 "there are faces with only format 4 subtables")
            sampled "P8.S12 shape kerx format 4"
                (aatRequest (format4Faces ()))
                2000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            testCase "P8.S12 real faces attach" (fun () ->
                let faces = format4Faces () |> Array.filter onlyFormat4
                let requests = randomRequests faces (Random 4711) 2000
                let attached =
                    requests
                    |> Array.filter (fun request ->
                        match comparable (oracle.Shape request) with
                        | Ok glyphs -> glyphs |> Array.exists (fun (struct (_, _, _, _, xOffset, yOffset, _)) -> xOffset <> 0 || yOffset <> 0)
                        | Error _ -> false)
                    |> Array.length
                printfn "P8.S12 %d of %d sampled shaping cases over the format 4 only faces leave a glyph offset by an attachment" attached requests.Length
                Expect.isGreaterThan attached 0 "the real faces attach glyphs")
            sampled "P8.S12 ankr anchors of real faces"
                (gen {
                    let! face = Gen.choose (0, realAnkrs.Value.Length - 1)
                    let! glyph = Gen.frequency [ 4, Gen.choose (0, 3000); 1, Gen.choose (0, 70000) ]
                    let! index = Gen.elements [ 0; 1; 2; 3; 4; 7; 0xFFFF ]
                    return face, uint32 glyph, uint32 index })
                3000
                (fun (face, glyph, index) -> oursAnchor realAnkrs.Value[face].Table realAnkrs.Value[face].NumGlyphs glyph index)
                (fun (face, glyph, index) -> theirsAnchor realAnkrs.Value[face].Bytes realAnkrs.Value[face].NumGlyphs glyph index)
            testCase "P8.S12 ankr tables of every lookup format" (fun () ->
                let tables = ankrTables ()
                let failures = ResizeArray<string>()
                let mutable rejected = 0
                let mutable disagreeing = 0
                for description, bytes in tables do
                    let theirs = Shims.farfbuzz_oracle_ankr_sanitize(bytes, uint32 bytes.Length, uint32 AnkrGlyphs) <> 0
                    let table, ours = oursAnkr bytes AnkrGlyphs
                    if not theirs then rejected <- rejected + 1
                    if ours <> theirs then
                        disagreeing <- disagreeing + 1
                        failures.Add $"{description}: HarfBuzz sanitizes {theirs}, Farfbuzz {ours}"
                    let expected = anchorsText AnkrGlyphs (theirsAnchor bytes AnkrGlyphs)
                    let actual = anchorsText AnkrGlyphs (oursAnchor table AnkrGlyphs)
                    if expected <> actual then failures.Add $"{description}: HarfBuzz {expected}, Farfbuzz {actual}"
                printfn "P8.S12 compared %d ankr tables in five lookup formats, unchanged and changed; HarfBuzz rejects %d, Farfbuzz rejects the same %d (%d disagree)" tables.Length rejected (rejected - disagreeing) disagreeing
                Expect.isGreaterThanOrEqual tables.Length 5000 "at least 5,000 tables"
                Expect.isEmpty failures ("ankr differences:\n" + String.concat "\n" (Seq.truncate 20 failures)))
            format4ChangedBytes oracle farfbuzz
        ]
    ]
