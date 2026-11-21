module Farfbuzz.Tests.AnchorTables

open System

/// <summary>Writes a big-endian 16-bit value, signed values by their two's complement.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u16 (value: int) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian 32-bit value.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private u32 (value: uint32) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>The lookup formats the ankr builder writes.</summary>
type AnkrLookupKind =
    | Dense
    | Segments
    | SegmentArrays
    | Singles
    | Trimmed

/// <summary>Writes the header of a binary-search lookup: unit size, unit count and the search values.</summary>
/// <param name="unitSize">The unit size.</param>
/// <param name="count">The unit count.</param>
/// <returns>The bytes.</returns>
let private binSearchHeader (unitSize: int) (count: int) =
    let selector = int (Math.Floor (Math.Log2 (float (max 1 count))))
    let range = unitSize * (1 <<< selector)
    Array.concat [ u16 unitSize; u16 count; u16 range; u16 selector; u16 (unitSize * count - range) ]

/// <summary>Writes the anchor array of a glyph: the 32-bit length, then each anchor's x and y.</summary>
/// <param name="anchors">The anchors.</param>
/// <returns>The bytes.</returns>
let private anchorArray (anchors: (int * int) array) =
    Array.concat [ u32 (uint32 anchors.Length); anchors |> Array.collect (fun (x, y) -> Array.append (u16 x) (u16 y)) ]

/// <summary>Groups ascending glyphs into runs of consecutive glyphs.</summary>
/// <param name="glyphs">The glyphs, ascending.</param>
/// <returns>The first and last glyph of each run.</returns>
let private runsOf (glyphs: uint32 list) =
    glyphs
    |> List.fold (fun runs glyph ->
        match runs with
        | (first, last) :: rest when glyph = last + 1u -> (first, glyph) :: rest
        | _ -> (glyph, glyph) :: runs) []
    |> List.rev

/// <summary>Builds an ankr table: a lookup of the given format mapping each glyph of the model to its anchor array, and the
/// anchor arrays. A glyph the lookup covers without a model entry points at an empty array.</summary>
/// <param name="kind">The lookup format.</param>
/// <param name="numGlyphs">The glyph count a dense lookup covers.</param>
/// <param name="model">The anchors of each glyph.</param>
/// <returns>The table bytes.</returns>
let ankr (kind: AnkrLookupKind) (numGlyphs: int) (model: Map<uint32, (int * int) array>) =
    let glyphs = model |> Map.toList |> List.map fst
    let arrays = model |> Map.toList |> List.map snd |> List.map anchorArray
    let offsets =
        arrays
        |> List.fold (fun (placed, next) bytes -> next :: placed, next + bytes.Length) ([], 4)
        |> fst
        |> List.rev
    let offsetOf = List.zip glyphs offsets |> Map.ofList
    let valueOf (glyph: uint32) = match Map.tryFind glyph offsetOf with | Some offset -> offset | None -> 0
    let anchorData = Array.concat (Array.append [| u32 0u |] (List.toArray arrays))
    let lookup =
        match kind with
        | Dense -> Array.concat [ u16 0; Array.init numGlyphs (fun glyph -> u16 (valueOf (uint32 glyph))) |> Array.concat ]
        | Singles ->
            let units = [ for glyph in glyphs -> Array.append (u16 (int glyph)) (u16 (valueOf glyph)) ] @ [ Array.append (u16 0xFFFF) (u16 0) ]
            Array.concat [ u16 6; binSearchHeader 4 units.Length; Array.concat units ]
        | Segments ->
            let units = [ for glyph in glyphs -> Array.concat [ u16 (int glyph); u16 (int glyph); u16 (valueOf glyph) ] ] @ [ Array.concat [ u16 0xFFFF; u16 0xFFFF; u16 0 ] ]
            Array.concat [ u16 2; binSearchHeader 6 units.Length; Array.concat units ]
        | SegmentArrays ->
            let runs = runsOf glyphs
            let unitsEnd = 12 + 6 * (runs.Length + 1)
            let valueArrays = [ for (first, last) in runs -> [ for glyph in first .. last -> u16 (valueOf glyph) ] |> Array.concat ]
            let starts = valueArrays |> List.fold (fun (placed, next) bytes -> next :: placed, next + bytes.Length) ([], unitsEnd) |> fst |> List.rev
            let units = [ for ((first, last), start) in List.zip runs starts -> Array.concat [ u16 (int last); u16 (int first); u16 start ] ] @ [ Array.concat [ u16 0xFFFF; u16 0xFFFF; u16 0 ] ]
            Array.concat [ u16 4; binSearchHeader 6 units.Length; Array.concat units; Array.concat valueArrays ]
        | Trimmed ->
            match glyphs with
            | [] -> Array.concat [ u16 8; u16 0; u16 0 ]
            | _ ->
                let first = List.min glyphs
                let last = List.max glyphs
                Array.concat [ u16 8; u16 (int first); u16 (int (last - first) + 1); [ for glyph in first .. last -> u16 (valueOf glyph) ] |> Array.concat ]
    Array.concat [ u16 0; u16 0; u32 12u; u32 (uint32 (12 + lookup.Length)); lookup; anchorData ]

/// <summary>A kerx table with one format 4 subtable and the fields a changed-bytes test rewrites.</summary>
type Kerx4Table =
    { Bytes: byte array
      Fields: (string * int * int) array
      SubtableAt: int
      ActionDataAt: int }

/// <summary>Builds a kerx table with one format 4 subtable, optionally followed by an empty format 0 subtable so that the
/// format 4 subtable is not the last: seven classes over the three glyphs (classes 4, 5 and 6), two states, four entries
/// and eight words of action data, every count, offset, flag and index field named. In state 0 the first glyph's class
/// marks; in state 1 the second glyph's class runs action 0, the third's runs action 1 and marks, and the first's marks.</summary>
/// <param name="glyphs">The glyphs of the classes 4, 5 and 6.</param>
/// <param name="actionType">The action type, the two top bits of the flags.</param>
/// <param name="actionWords">The eight words of action data.</param>
/// <param name="trailing">Whether an empty format 0 subtable follows.</param>
/// <returns>The table and its fields.</returns>
let kerx4 (glyphs: uint32 array) (actionType: int) (actionWords: int array) (trailing: bool) =
    let first = Array.min glyphs
    let count = int (Array.max glyphs - first) + 1
    let classOf glyph = match Array.tryFindIndex ((=) glyph) glyphs with | Some index -> 4 + index | None -> 1
    let classes = Array.init count (fun i -> classOf (first + uint32 i))
    let subtableAt = 8
    let lookupAt = subtableAt + 32
    let stateAt = lookupAt + 6 + 2 * count
    let entryAt = stateAt + 28
    let dataAt = entryAt + 24
    let total = dataAt + 16
    let machineAt = subtableAt + 12
    let words (values: int array) = values |> Array.collect u16
    let states = words [| 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 2; 3 |]
    let entries = words [| 0; 0; 0xFFFF; 1; 0x8000; 0xFFFF; 1; 0; 0; 1; 0x8000; 1 |]
    let flags = (uint32 actionType <<< 30) ||| uint32 (dataAt - machineAt)
    let flagsBytes = u32 flags
    let subtable =
        Array.concat
            [ u32 (uint32 (total - subtableAt)); u32 4u; u32 0u
              u32 7u; u32 (uint32 (lookupAt - machineAt)); u32 (uint32 (stateAt - machineAt)); u32 (uint32 (entryAt - machineAt)); flagsBytes
              u16 8; u16 (int first); u16 count; words classes; states; entries; words actionWords ]
    let empty0 = Array.concat [ u32 28u; u32 0u; u32 0u; u32 0u; u32 0u; u32 0u; u32 0u ]
    let tableCount = if trailing then 2u else 1u
    let bytes = Array.concat [ u16 2; u16 0; u32 tableCount; subtable; (if trailing then empty0 else [||]) ]
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
           yield "flags", 36, 4
           yield "class lookup format", lookupAt, 2
           yield "class lookup first glyph", lookupAt + 2, 2
           yield "class lookup count", lookupAt + 4, 2
           for i in 0 .. count - 1 do yield $"class of glyph {i}", lookupAt + 6 + 2 * i, 2
           for i in 0 .. 13 do yield $"state array slot {i}", stateAt + 2 * i, 2
           for i in 0 .. 3 do
               yield $"entry {i} newState", entryAt + 6 * i, 2
               yield $"entry {i} flags", entryAt + 6 * i + 2, 2
               yield $"entry {i} ankr action index", entryAt + 6 * i + 4, 2
           for i in 0 .. 7 do yield $"action word {i}", dataAt + 2 * i, 2 |]
    { Bytes = bytes; Fields = fields; SubtableAt = subtableAt; ActionDataAt = dataAt }
