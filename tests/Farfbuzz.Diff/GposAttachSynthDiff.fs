module Farfbuzz.Diff.GposAttachSynthDiff

open System
open System.IO
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Diff.GposDiff

/// <summary>A big-endian unsigned 16-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u16 (value: int) = [| byte (value >>> 8); byte value |]

/// <summary>A big-endian unsigned 24-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u24 (value: int) = [| byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A big-endian unsigned 32-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u32 (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A big-endian value of a width.</summary>
/// <param name="width">Two, three or four bytes.</param>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private wide (width: int) (value: int) =
    match width with
    | 2 -> u16 value
    | 3 -> u24 value
    | _ -> u32 value

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

/// <summary>A hinting device of format 1 for 11 to 13 ppem: a delta of one pixel at 11 and 12, none at 13.</summary>
let private hintingDevice = Array.concat [ u16 11; u16 13; u16 1; u16 0x4000 ]

/// <summary>A variation device with the index (0, 0), which no variation store answers.</summary>
let private variationDevice = Array.concat [ u16 0; u16 0; u16 0x8000 ]

/// <summary>A random anchor, each format of anchors of the three, with devices after a format 3 anchor.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The bytes.</returns>
let private randomAnchor (rnd: Random) =
    let coordinate () = match rnd.Next 10 with | 0 -> 32767 | 1 -> -32768 | 2 -> 5 | _ -> rnd.Next(-300, 301)
    let x, y = coordinate (), coordinate ()
    match rnd.Next 8 with
    | 0 -> Array.concat [ u16 2; u16 (x &&& 0xFFFF); u16 (y &&& 0xFFFF); u16 (rnd.Next 8) ]
    | 1 | 2 ->
        let devices = match rnd.Next 3 with | 0 -> [ hintingDevice; hintingDevice ] | 1 -> [ hintingDevice; variationDevice ] | _ -> [ variationDevice; hintingDevice ]
        let first = 10
        let second = first + devices[0].Length
        let offsets = match rnd.Next 5 with | 0 -> [ 0; second ] | 1 -> [ first; 0 ] | _ -> [ first; second ]
        Array.concat [ u16 3; u16 (x &&& 0xFFFF); u16 (y &&& 0xFFFF); u16 offsets[0]; u16 offsets[1]; devices[0]; devices[1] ]
    | 3 -> Array.concat [ u16 (rnd.Next(4, 9)); u16 (x &&& 0xFFFF); u16 (y &&& 0xFFFF) ]
    | _ -> Array.concat [ u16 1; u16 (x &&& 0xFFFF); u16 (y &&& 0xFFFF) ]

/// <summary>A table of anchors reached by offsets from the table's start: a head the caller writes and the anchors after it.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="count">The number of anchors.</param>
/// <param name="allowNull">Whether an entry may be a null offset.</param>
/// <returns>The anchors, and the offset of each from the first, or zero for a null entry.</returns>
let private anchorsOf (rnd: Random) (count: int) (allowNull: bool) =
    let anchors = List.init count (fun _ -> match allowNull && rnd.Next 6 = 0 with | true -> None | false -> Some (randomAnchor rnd))
    let bytes = anchors |> List.choose id
    let offsets = placed 0 bytes
    let at = anchors |> List.mapFold (fun next anchor -> match anchor with | None -> -1, next | Some _ -> offsets[next], next + 1) 0 |> fst
    struct (at, Array.concat bytes)

/// <summary>The offsets of a head and its anchors as the head's fields: the anchors start after the head.</summary>
/// <param name="width">The width of an offset.</param>
/// <param name="headSize">The size of the head the offsets are relative to the start of.</param>
/// <param name="at">The offset of each anchor within the anchors, minus one for a null one.</param>
/// <returns>The bytes of the offset fields.</returns>
let private offsetFields (width: int) (headSize: int) (at: int list) =
    Array.concat (at |> List.map (fun offset -> match offset with | -1 -> wide width 0 | _ -> wide width (headSize + offset)))

/// <summary>A mark array of random marks: the count, then a class and an anchor offset for each record, then the anchors.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="w">The width of the count and of the offsets.</param>
/// <param name="records">The number of marks.</param>
/// <param name="classCount">The class count the classes are drawn from, and one more.</param>
/// <returns>The bytes.</returns>
let private markArray (rnd: Random) (w: int) (records: int) (classCount: int) =
    let struct (at, anchors) = anchorsOf rnd records false
    let head = w + records * (2 + w)
    let recordBytes = at |> List.map (fun offset -> Array.concat [ u16 (rnd.Next(0, classCount + 1)); wide w (head + offset) ])
    Array.concat [ wide w records; Array.concat recordBytes; anchors ]

/// <summary>An anchor matrix: the row count, one offset per entry, then the anchors.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="rowWidth">The width of the row count.</param>
/// <param name="w">The width of the offsets.</param>
/// <param name="rows">The rows.</param>
/// <param name="columns">The columns.</param>
/// <returns>The bytes.</returns>
let private matrix (rnd: Random) (rowWidth: int) (w: int) (rows: int) (columns: int) =
    let struct (at, anchors) = anchorsOf rnd (rows * columns) true
    Array.concat [ wide rowWidth rows; offsetFields w (rowWidth + rows * columns * w) at; anchors ]

/// <summary>A mark-to-base, mark-to-ligature or mark-to-mark subtable of the given format, with random content.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="kind">The lookup type, four to six.</param>
/// <param name="medium">Whether the subtable is of format 2, with 32-bit and 24-bit offsets.</param>
/// <param name="glyphs">The glyphs the coverages are drawn from.</param>
/// <returns>The bytes.</returns>
let private markSubtable (rnd: Random) (kind: int) (medium: bool) (glyphs: int list) =
    let covW = match medium with | true -> 4 | false -> 2
    let w = match medium with | true -> 3 | false -> 2
    let pick () = match glyphs |> List.filter (fun _ -> rnd.Next 2 = 0) with | [] -> [ glyphs[rnd.Next glyphs.Length] ] | some -> some
    let marks = pick ()
    let targets = pick ()
    let classCount = rnd.Next(1, 4)
    let markRecords = match rnd.Next 6 with | 0 -> max 0 (marks.Length - 1) | _ -> marks.Length
    let marksArray = markArray rnd w markRecords classCount
    let secondArray =
        match kind with
        | 5 ->
            let attaches = targets |> List.map (fun _ -> matrix rnd 2 w (rnd.Next(1, 4)) classCount)
            let head = w + targets.Length * w
            let offsets = placed head attaches
            Array.concat [ wide w targets.Length; Array.concat (offsets |> List.map (wide w)); Array.concat attaches ]
        | 4 -> matrix rnd w w targets.Length classCount
        | _ -> matrix rnd 2 w targets.Length classCount
    let markCoverage = coverage marks
    let targetCoverage = coverage targets
    let header = 4 + 4 * covW
    let offsets = placed header [ markCoverage; targetCoverage; marksArray; secondArray ]
    Array.concat
        [ u16 (match medium with | true -> 2 | false -> 1)
          wide covW offsets[0]
          wide covW offsets[1]
          u16 classCount
          wide covW offsets[2]
          wide covW offsets[3]
          markCoverage
          targetCoverage
          marksArray
          secondArray ]

/// <summary>A cursive attachment subtable of the given format, with random content.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="medium">Whether the subtable is of format 2.</param>
/// <param name="glyphs">The glyphs the coverage is drawn from.</param>
/// <returns>The bytes.</returns>
let private cursiveSubtable (rnd: Random) (medium: bool) (glyphs: int list) =
    let covW = match medium with | true -> 4 | false -> 2
    let w = match medium with | true -> 3 | false -> 2
    let covered = match glyphs |> List.filter (fun _ -> rnd.Next 2 = 0) with | [] -> [ glyphs[rnd.Next glyphs.Length] ] | some -> some
    let cover = coverage covered
    let struct (at, anchors) = anchorsOf rnd (covered.Length * 2) true
    let head = 2 + covW + w + covered.Length * 2 * w
    let anchorsAt = head + cover.Length
    let records = at |> List.map (fun offset -> match offset with | -1 -> wide w 0 | _ -> wide w (anchorsAt + offset))
    Array.concat [ u16 (match medium with | true -> 2 | false -> 1); wide covW head; wide w covered.Length; Array.concat records; cover; anchors ]

/// <summary>A class definition of format 2 over glyphs, mapping each glyph to a class.</summary>
/// <param name="classes">The glyph and class pairs, sorted by glyph.</param>
/// <returns>The bytes.</returns>
let private classDef (classes: (int * int) list) =
    Array.concat [ u16 2; u16 classes.Length; Array.concat (classes |> List.map (fun (glyph, klass) -> Array.concat [ u16 glyph; u16 glyph; u16 klass ])) ]

/// <summary>One lookup of a table: a type, flags, subtables, wrapped in extensions when asked, and a mark filtering set.</summary>
/// <param name="kind">The lookup type.</param>
/// <param name="flag">The lookup flag.</param>
/// <param name="subtables">The subtables.</param>
/// <param name="extension">Whether to wrap the subtables in extension subtables.</param>
/// <param name="filteringSet">The mark filtering set, when the flag asks for one.</param>
/// <returns>The bytes.</returns>
let private lookupBytes (kind: int) (flag: int) (subtables: byte array list) (extension: bool) (filteringSet: int) =
    let count = subtables.Length
    let head = 6 + 2 * count + (match flag &&& 0x10 with | 0 -> 0 | _ -> 2)
    let wrapped =
        match extension with
        | false -> subtables
        | true -> subtables |> List.map (fun subtable -> Array.concat [ u16 1; u16 kind; u32 8; subtable ])
    let offsets = placed head wrapped
    Array.concat [ u16 (match extension with | true -> 9 | false -> kind); u16 flag; u16 count; Array.concat (offsets |> List.map u16); (match flag &&& 0x10 with | 0 -> [||] | _ -> u16 filteringSet); Array.concat wrapped ]

/// <summary>A GPOS or GSUB table of lookups, with a default script and one feature for each tag.</summary>
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
    let header = 10
    let offsets = placed header [ scriptList; featureList; lookupList ]
    Array.concat [ u32 0x00010000; u16 offsets[0]; u16 offsets[1]; u16 offsets[2]; scriptList; featureList; lookupList ]

/// <summary>A table directory around tables, for a font that has glyphs but no outlines.</summary>
/// <param name="tables">The tags and bytes, in tag order.</param>
/// <returns>The file bytes.</returns>
let private sfnt (tables: (string * byte array) list) =
    let head = 12 + 16 * tables.Length
    let padded = tables |> List.map (fun (tag, bytes) -> tag, bytes, Array.zeroCreate<byte> ((4 - bytes.Length % 4) % 4))
    let offsets = placed head (padded |> List.map (fun (_, bytes, padding) -> Array.append bytes padding))
    let records = List.zip padded offsets |> List.map (fun ((tag, bytes, _), offset) -> Array.concat [ Text.Encoding.ASCII.GetBytes tag; u32 0; u32 offset; u32 bytes.Length ])
    Array.concat [ u32 0x00010000; u16 tables.Length; u16 0; u16 0; u16 0; Array.concat records; Array.concat (padded |> List.map (fun (_, bytes, padding) -> Array.append bytes padding)) ]

/// <summary>The glyph count of a synthetic font: glyph zero, eight letters and a ligature.</summary>
[<Literal>]
let private GlyphCount = 10

/// <summary>The basic tables of a synthetic font: head, maxp, hhea, hmtx and a cmap that maps U+0041 to U+0048 to glyphs 1 to 8.</summary>
/// <returns>The tags and bytes.</returns>
let private basics () =
    let head = Array.concat [ u32 0x00010000; u32 0; u32 0; u32 0x5F0F3CF5; u16 0; u16 1000; Array.zeroCreate 16; Array.zeroCreate 8; u16 0; u16 8; u16 2; u16 0; u16 0 ]
    let maxp = Array.concat [ u32 0x00005000; u16 GlyphCount ]
    let hhea = Array.concat [ u32 0x00010000; u16 800; u16 0xFF38; u16 0; u16 500; Array.zeroCreate 20; u16 0; u16 GlyphCount ]
    let hmtx = Array.concat (List.init GlyphCount (fun _ -> Array.concat [ u16 500; u16 0 ]))
    let subtable = Array.concat [ u16 12; u16 0; u32 28; u32 0; u32 1; u32 0x41; u32 0x48; u32 1 ]
    let cmap = Array.concat [ u16 0; u16 1; u16 3; u16 10; u32 12; subtable ]
    [ "cmap", cmap; "head", head; "hhea", hhea; "hmtx", hmtx; "maxp", maxp ]

/// <summary>A GDEF table with glyph classes, a mark attachment class definition and two mark glyph sets.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The bytes.</returns>
let private gdef (rnd: Random) =
    let classes = [ for glyph in 1 .. 9 -> glyph, [| 1; 3; 3; 2; 4; 0 |][rnd.Next 6] ] |> List.filter (fun (_, klass) -> klass <> 0)
    let attachClasses = [ for glyph in 1 .. 9 do if rnd.Next 2 = 0 then yield glyph, rnd.Next(1, 3) ]
    let sets = [ coverage [ 2; 3; 4 ]; coverage [ 5; 6 ] ]
    let setsHead = 4 + 4 * sets.Length
    let setOffsets = placed setsHead sets
    let markSets = Array.concat [ u16 1; u16 sets.Length; Array.concat (setOffsets |> List.map u32); Array.concat sets ]
    let glyphClassDef = classDef classes
    let attachClassDef = match attachClasses with | [] -> classDef [ 1, 0 ] | _ -> classDef attachClasses
    let header = 14
    let offsets = placed header [ glyphClassDef; attachClassDef; markSets ]
    Array.concat [ u32 0x00010002; u16 offsets[0]; u16 0; u16 0; u16 offsets[1]; u16 offsets[2]; glyphClassDef; attachClassDef; markSets ]

/// <summary>A GSUB table with a ligature of glyphs 1 and 2 into glyph 9 and a multiple substitution of glyph 3 into glyphs 3 and 4.</summary>
/// <returns>The bytes.</returns>
let private gsub () =
    let ligature = Array.concat [ u16 9; u16 2; u16 2 ]
    let ligatureSet = Array.concat [ u16 1; u16 4; ligature ]
    let ligatureCoverage = coverage [ 1 ]
    let ligatureSubtable = Array.concat [ u16 1; u16 (6 + 2); u16 1; u16 (6 + 2 + ligatureCoverage.Length); ligatureCoverage; ligatureSet ]
    let sequence = Array.concat [ u16 2; u16 3; u16 4 ]
    let sequenceCoverage = coverage [ 3 ]
    let multipleSubtable = Array.concat [ u16 1; u16 (6 + 2); u16 1; u16 (6 + 2 + sequenceCoverage.Length); sequenceCoverage; sequence ]
    layoutTable [ "ccmp", lookupBytes 2 0 [ multipleSubtable ] false 0; "liga", lookupBytes 4 0 [ ligatureSubtable ] false 0 ]

/// <summary>One random GPOS lookup of an attachment type.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The feature tag and the lookup bytes.</returns>
let private gposLookup (rnd: Random) =
    let kind = rnd.Next(3, 7)
    let medium = rnd.Next 4 = 0
    let glyphs = [ 1 .. 9 ]
    let subtable () = match kind with | 3 -> cursiveSubtable rnd medium glyphs | _ -> markSubtable rnd kind medium glyphs
    let flag = [| 0; 0; 0; 1; 1; 2; 4; 8; 9; 0x0A; 0x10; 0x100; 0x200; 0x11 |][rnd.Next 14]
    let tag = match kind with | 3 -> "curs" | 6 -> "mkmk" | _ -> "mark"
    tag, lookupBytes kind flag (List.init (rnd.Next(1, 4)) (fun _ -> subtable ())) (rnd.Next 6 = 0) (rnd.Next 2)

/// <summary>Builds a whole synthetic font from a seed.</summary>
/// <param name="seed">The seed.</param>
/// <returns>The file bytes.</returns>
let private syntheticFont (seed: int) =
    let rnd = Random seed
    let lookups = List.init (rnd.Next(1, 5)) (fun _ -> gposLookup rnd)
    let gposBytes = layoutTable lookups
    let tables = basics () @ [ "GDEF", gdef rnd; "GPOS", gposBytes ] @ (match rnd.Next 2 with | 0 -> [ "GSUB", gsub () ] | _ -> [])
    sfnt (tables |> List.sortBy fst)

/// <summary>Where the GPOS table of a synthetic font starts and how long it is.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The offset and the length.</returns>
let private gposExtent (bytes: byte array) =
    let count = (int bytes[4] <<< 8) ||| int bytes[5]
    let read (at: int) = (int bytes[at] <<< 24) ||| (int bytes[at + 1] <<< 16) ||| (int bytes[at + 2] <<< 8) ||| int bytes[at + 3]
    let record = Seq.find (fun index -> Text.Encoding.ASCII.GetString(bytes, 12 + 16 * index, 4) = "GPOS") (seq { 0 .. count - 1 })
    struct (read (12 + 16 * record + 8), read (12 + 16 * record + 12))

/// <summary>Whether a font file's GPOS has a lookup of the context or chaining context type, which the slice of the contextual
/// subtables ports, or an extension that wraps one.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>True when GPOS has one.</returns>
let private hasContextLookup (bytes: byte array) =
    let struct (start, _) = gposExtent bytes
    let u16 (at: int) = match at >= 0 && at + 1 < bytes.Length with | true -> (int bytes[at] <<< 8) ||| int bytes[at + 1] | false -> 0
    let lookupList = start + u16 (start + 8)
    seq {
        for index in 0 .. min 64 (u16 lookupList) - 1 do
            let lookup = lookupList + u16 (lookupList + 2 + 2 * index)
            yield u16 lookup
            if u16 lookup = 9 then
                for subtable in 0 .. min 8 (u16 (lookup + 4)) - 1 do
                    yield u16 (lookup + u16 (lookup + 6 + 2 * subtable) + 2) }
    |> Seq.exists (fun kind -> kind = 7 || kind = 8)

/// <summary>The number of GPOS lookups HarfBuzz and Farfbuzz read from a file.</summary>
/// <param name="path">The font file.</param>
/// <returns>HarfBuzz's count and Farfbuzz's.</returns>
let private lookupCounts (path: string) =
    use blob = HarfBuzzSharp.Blob.FromFile path
    use face = new HarfBuzzSharp.Face(blob, 0)
    let oracle = int (Native.hb_ot_layout_table_get_lookup_count(face.Handle, uint32 (Tag.make 'G' 'P' 'O' 'S')))
    let ours = LayoutTable.lookupCount (Face.create (ReadOnlyMemory (File.ReadAllBytes path)) 0u |> OtFace.create).Gpos.Value
    struct (oracle, ours)

/// <summary>The P5.S11 differentials over synthetic fonts: anchors of the three formats with devices, the formats with 24-bit and
/// 32-bit offsets, lookup flags with mark filtering sets and mark attachment classes, extensions, ligatures and multiple
/// substitutions, and the same fonts with a byte of GPOS rewritten.</summary>
/// <returns>The test list.</returns>
let tests () =
    testSequenced <| testList "P5.S11 synthetic" [
        testCase "P5.S11 synthetic fonts: well-formed tables are accepted and shape as HarfBuzz does, and random changed bytes are judged as HarfBuzz judges them" (fun () ->
            let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "gpos-attach-synthetic")
            Directory.CreateDirectory directory |> ignore
            let mutable state = 0x5A11u
            let nextRandom () =
                state <- state * 1103515245u + 12345u
                state >>> 8
            let failures = ResizeArray<string>()
            let mutable intact = 0
            let mutable intactAccepted = 0
            let mutable intactAttached = 0
            let mutable changed = 0
            let mutable changedRejected = 0
            let mutable changedRejectedByUs = 0
            let mutable changedAttached = 0
            let mutable skipped = 0
            let shape (path: string) (rnd: Random) =
                let text = Array.init (rnd.Next(1, 10)) (fun _ -> uint32 (0x41 + rnd.Next 8))
                { Path = path
                  Cached = false
                  Index = 0
                  Text = text
                  Features = ""
                  Direction = [| "ltr"; "ltr"; "rtl"; "ttb"; "btt" |][rnd.Next 5]
                  Flags = [| 0u; 64u; 0u; 64u ||| 128u |][rnd.Next 4]
                  ClusterLevel = rnd.Next 4
                  Ppem = [| 0u; 0u; 12u; 20u |][rnd.Next 4] }
            for seed in 0 .. 2999 do
                let bytes = syntheticFont seed
                let path = Path.Combine(directory, $"synthetic-{seed}.ttf")
                File.WriteAllBytes(path, bytes)
                let rnd = Random (seed + 1)
                let struct (oracleCount, ourCount) = lookupCounts path
                intact <- intact + 1
                if oracleCount > 0 then intactAccepted <- intactAccepted + 1
                if oracleCount <> ourCount then failures.Add $"seed {seed}: lookup count oracle {oracleCount}, farfbuzz {ourCount}"
                for attempt in 0 .. 7 do
                    let request = shape path rnd
                    let ours = ourShape request
                    let expected = oracleShape request
                    match expected with
                    | Ok shaped when oracleCount > 0 && shaped |> Array.exists (fun (struct (_, _, _, _, xOffset, yOffset, _)) -> xOffset <> 0 || yOffset <> 0) -> intactAttached <- intactAttached + 1
                    | _ -> ()
                    if ours <> expected then failures.Add $"seed {seed}: shaped differently: {request}\\n  farfbuzz %A{ours}\\n  oracle %A{expected}"
                let struct (start, length) = gposExtent bytes
                for variant in 0 .. 1 do
                    let candidate = Array.copy bytes
                    let at = start + int (nextRandom () % uint32 length)
                    candidate[at] <- (match nextRandom () % 3u with | 0u -> byte (nextRandom ()) | 1u -> candidate[at] ^^^ 0x80uy | _ -> byte (candidate[at] + 1uy))
                    let mutated = Path.Combine(directory, $"mutated-{seed}-{variant}.ttf")
                    match hasContextLookup candidate with
                    | true -> skipped <- skipped + 1
                    | false ->
                        File.WriteAllBytes(mutated, candidate)
                        let struct (mutatedOracle, mutatedOurs) = lookupCounts mutated
                        changed <- changed + 1
                        if mutatedOracle = 0 then changedRejected <- changedRejected + 1
                        if mutatedOurs = 0 then changedRejectedByUs <- changedRejectedByUs + 1
                        if mutatedOracle <> mutatedOurs then
                            File.Copy(mutated, Path.Combine(directory, $"failed-{seed}-{variant}.ttf"), true)
                            File.Copy(path, Path.Combine(directory, $"original-{seed}.ttf"), true)
                            failures.Add $"seed {seed} byte {at - start} of GPOS (gpos at {start}, length {length}): lookup count oracle {mutatedOracle}, farfbuzz {mutatedOurs}"
                        for attempt in 0 .. 3 do
                            let request = shape mutated rnd
                            let ours = ourShape request
                            let expected = oracleShape request
                            match expected with
                            | Ok shaped when mutatedOracle > 0 && shaped |> Array.exists (fun (struct (_, _, _, _, xOffset, yOffset, _)) -> xOffset <> 0 || yOffset <> 0) -> changedAttached <- changedAttached + 1
                            | _ -> ()
                            if ours <> expected then failures.Add $"seed {seed} byte {at - start} of GPOS: shaped differently: {request}\\n  farfbuzz %A{ours}\\n  oracle %A{expected}"
                        File.Delete mutated
                File.Delete path
            printfn "P5.S11 synthetic (%d changed fonts left out for a context lookup): %d fonts, %d accepted by HarfBuzz and by Farfbuzz alike, %d shaped runs with a displaced glyph; %d changed-byte fonts, HarfBuzz rejected %d and Farfbuzz %d, %d shaped runs of the accepted ones with a displaced glyph" skipped intact intactAccepted intactAttached changed changedRejected changedRejectedByUs changedAttached
            Expect.isEmpty (failures |> Seq.truncate 10 |> Seq.toList) ("synthetic differences:\n" + String.concat "\n" (failures |> Seq.truncate 10))
            Expect.isGreaterThan intactAccepted 2500 "almost every synthetic table is well formed"
            Expect.isGreaterThan intactAttached 3000 "many runs attach glyphs"
            Expect.equal changedRejectedByUs changedRejected "Farfbuzz rejects as many changed tables as HarfBuzz"
            Expect.isGreaterThan changedRejected 100 "some changes make HarfBuzz reject the table"
            Expect.isEmpty (failures |> Seq.truncate 10 |> Seq.toList) ("synthetic differences:\\n" + String.concat "\\n" (failures |> Seq.truncate 10)))
    ]
