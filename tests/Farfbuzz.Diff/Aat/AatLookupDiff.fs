module Farfbuzz.Diff.AatLookupDiff

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The four bytes.</returns>
let private be32 (value: uint32) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Writes a value of 1, 2 or 4 bytes.</summary>
/// <param name="valueSize">The width.</param>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private writeValue (valueSize: int) (value: uint32) =
    match valueSize with
    | 1 -> [| byte value |]
    | 2 -> be16 value
    | _ -> be32 value

/// <summary>The binary-search header, with the search fields left at zero.</summary>
/// <param name="unitSize">The unit size.</param>
/// <param name="nUnits">The unit count.</param>
/// <returns>The ten bytes.</returns>
let private binSearchHeader (unitSize: int) (nUnits: int) =
    Array.concat [ be16 (uint32 unitSize); be16 (uint32 nUnits); be16 0u; be16 0u; be16 0u ]

/// <summary>A generated lookup and the glyphs to probe.</summary>
type private LookupCase =
    { Bytes: byte array
      ValueSize: int
      NumGlyphs: int
      Glyphs: uint32 array }

/// <summary>A generated obsolete class table and the glyphs to probe.</summary>
type private ClassCase =
    { Bytes: byte array
      FirstGlyph: uint16
      Count: int
      Wide: bool
      Glyphs: uint32 array }

/// <summary>The compared lookup state.</summary>
type private Outcome =
    { Sane: bool
      Found: bool array
      Values: uint32 array
      Classes: uint32 array
      Ranges: (uint32 * uint32) array }

/// <summary>The compared class-table state.</summary>
type private ClassOutcome =
    { Sane: bool
      Classes: uint32 array
      Ranges: (uint32 * uint32) array }

/// <summary>The empty lookup outcome for a table that fails to sanitize.</summary>
/// <param name="count">The number of probe glyphs.</param>
/// <returns>The outcome.</returns>
let private emptyOutcome (count: int) =
    { Sane = false
      Found = Array.zeroCreate count
      Values = Array.zeroCreate count
      Classes = Array.zeroCreate count
      Ranges = [||] }

/// <summary>The empty class-table outcome for a table that fails to sanitize.</summary>
/// <param name="count">The number of probe glyphs.</param>
/// <returns>The outcome.</returns>
let private emptyClassOutcome (count: int) =
    { Sane = false
      Classes = Array.zeroCreate count
      Ranges = [||] }

/// <summary>Farfbuzz's lookup result for a generated case.</summary>
/// <param name="case">The case.</param>
/// <returns>The outcome.</returns>
let private oursLookup (case: LookupCase) =
    let lookup = AatLookup.read (ReadOnlyMemory case.Bytes) 0 case.ValueSize
    let sane = match AatLookup.sanitize (ReadOnlySpan case.Bytes) 0 case.ValueSize case.NumGlyphs true (Sanitize.start case.Bytes.Length) with ValueSome _ -> true | ValueNone -> false
    match sane with
    | false -> emptyOutcome case.Glyphs.Length
    | true ->
        let found = case.Glyphs |> Array.map (fun glyph -> match AatLookup.valueAt lookup glyph case.NumGlyphs with ValueSome _ -> true | ValueNone -> false)
        let values = case.Glyphs |> Array.map (fun glyph -> AatLookup.valueOrNull lookup glyph case.NumGlyphs)
        let classes =
            case.Glyphs
            |> Array.map (fun glyph ->
                match case.ValueSize with
                | 1 -> uint32 (AatLookup.classOf lookup glyph case.NumGlyphs 0xFFus)
                | 2 -> uint32 (AatLookup.classOf lookup glyph case.NumGlyphs 0xFFFFus)
                | _ -> 0u)
        let ranges = AatLookup.collectGlyphs lookup case.NumGlyphs |> Seq.map (fun range -> range.First, range.Last) |> Seq.toArray
        { Sane = true; Found = found; Values = values; Classes = classes; Ranges = ranges }

/// <summary>The oracle's lookup result for a generated case.</summary>
/// <param name="case">The case.</param>
/// <returns>The outcome.</returns>
let private oracleLookup (case: LookupCase) =
    let data = case.Bytes
    let length = uint32 data.Length
    let valueSize = uint32 case.ValueSize
    let numGlyphs = uint32 case.NumGlyphs
    let sane = Shims.farfbuzz_oracle_aat_lookup_sanitize(data, length, valueSize, numGlyphs) <> 0
    match sane with
    | false -> emptyOutcome case.Glyphs.Length
    | true ->
        let found =
            case.Glyphs
            |> Array.map (fun glyph ->
                let mutable foundFlag = 0u
                Shims.farfbuzz_oracle_aat_lookup_get_value(data, length, valueSize, numGlyphs, glyph, &foundFlag) |> ignore
                foundFlag <> 0u)
        let values = case.Glyphs |> Array.map (fun glyph -> Shims.farfbuzz_oracle_aat_lookup_get_value_or_null(data, length, valueSize, numGlyphs, glyph))
        let classes =
            case.Glyphs
            |> Array.map (fun glyph ->
                match case.ValueSize with
                | 1 -> Shims.farfbuzz_oracle_aat_lookup_get_class(data, length, valueSize, numGlyphs, glyph, 0xFFu)
                | 2 -> Shims.farfbuzz_oracle_aat_lookup_get_class(data, length, valueSize, numGlyphs, glyph, 0xFFFFu)
                | _ -> 0u)
        let out = Array.zeroCreate 1024
        let count = int (Shims.farfbuzz_oracle_aat_lookup_collect_glyphs(data, length, valueSize, numGlyphs, out, 512u))
        let ranges = [| for i in 0 .. count - 1 -> out[2 * i], out[2 * i + 1] |]
        { Sane = true; Found = found; Values = values; Classes = classes; Ranges = ranges }

/// <summary>Farfbuzz's class-table result for a generated case.</summary>
/// <param name="case">The case.</param>
/// <returns>The outcome.</returns>
let private oursClass (case: ClassCase) =
    let table = { ClassTable.Table = ReadOnlyMemory case.Bytes; At = 0; FirstGlyph = case.FirstGlyph; Count = case.Count; Wide = case.Wide }
    let sane = match ClassTable.sanitize table (Sanitize.start case.Bytes.Length) with ValueSome _ -> true | ValueNone -> false
    match sane with
    | false -> emptyClassOutcome case.Glyphs.Length
    | true ->
        let classes = case.Glyphs |> Array.map (fun glyph -> uint32 (ClassTable.classOfCall table glyph 0xFFFFus))
        let ranges = ClassTable.collectGlyphs table |> Seq.map (fun range -> range.First, range.Last) |> Seq.toArray
        { Sane = true; Classes = classes; Ranges = ranges }

/// <summary>The oracle's class-table result for a generated case.</summary>
/// <param name="case">The case.</param>
/// <returns>The outcome.</returns>
let private oracleClass (case: ClassCase) =
    let data = case.Bytes
    let length = uint32 data.Length
    let wide = if case.Wide then 1u else 0u
    let sane = Shims.farfbuzz_oracle_aat_class_table_sanitize(data, length, wide) <> 0
    match sane with
    | false -> emptyClassOutcome case.Glyphs.Length
    | true ->
        let classes = case.Glyphs |> Array.map (fun glyph -> Shims.farfbuzz_oracle_aat_class_table_get_class(data, length, wide, glyph, 0xFFFFu))
        let out = Array.zeroCreate 1024
        let count = int (Shims.farfbuzz_oracle_aat_class_table_collect_glyphs(data, length, wide, out, 512u))
        let ranges = [| for i in 0 .. count - 1 -> out[2 * i], out[2 * i + 1] |]
        { Sane = true; Classes = classes; Ranges = ranges }

/// <summary>Generates a format 0 lookup, whole or with its values truncated.</summary>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>The bytes and the boundary glyphs.</returns>
let private genFormat0 (valueSize: int) (numGlyphs: int) =
    gen {
        let! values = Gen.listOfLength numGlyphs (Gen.choose (0, 255) |> Gen.map uint32)
        let valueBytes = values |> List.map (writeValue valueSize) |> Array.concat
        let! broken = Gen.elements [ false; true; true ]
        let bytes =
            match broken with
            | false -> Array.concat [ be16 0u; valueBytes ]
            | true -> Array.concat [ be16 0u; Array.truncate (max 0 (valueBytes.Length - valueSize)) valueBytes ]
        return bytes, [ 0u; uint32 (numGlyphs - 1); uint32 numGlyphs ]
    }

/// <summary>Generates a format 8 trimmed-array lookup, whole or truncated.</summary>
/// <param name="valueSize">The value width.</param>
/// <returns>The bytes and the boundary glyphs.</returns>
let private genFormat8 (valueSize: int) =
    gen {
        let! firstGlyph = Gen.choose (0, 40)
        let! count = Gen.choose (0, 8)
        let! values = Gen.listOfLength count (Gen.choose (0, 255) |> Gen.map uint32)
        let valueBytes = values |> List.map (writeValue valueSize) |> Array.concat
        let! broken = Gen.elements [ false; true ]
        let bytes =
            match broken with
            | false -> Array.concat [ be16 8u; be16 (uint32 firstGlyph); be16 (uint32 count); valueBytes ]
            | true -> Array.concat [ be16 8u; be16 (uint32 firstGlyph); be16 (uint32 count); Array.truncate (max 0 (valueBytes.Length - valueSize)) valueBytes ]
        return bytes, [ uint32 (max 0 (firstGlyph - 1)); uint32 firstGlyph; uint32 (max 0 (firstGlyph + count - 1)); uint32 (firstGlyph + count) ]
    }

/// <summary>Generates a format 10 byte-value lookup, whole or broken.</summary>
/// <param name="valueSize">The outer value width.</param>
/// <returns>The bytes and the boundary glyphs.</returns>
let private genFormat10 (valueSize: int) =
    gen {
        let! firstGlyph = Gen.choose (0, 40)
        let! count = Gen.choose (0, 8)
        let! tableValueSize = Gen.elements [ valueSize; 0; 5 ]
        let! values = Gen.listOfLength count (Gen.choose (0, 255) |> Gen.map uint32)
        let valueBytes = values |> List.map (writeValue valueSize) |> Array.concat
        let! broken = Gen.elements [ false; true ]
        let bytes =
            match broken with
            | false -> Array.concat [ be16 10u; be16 (uint32 tableValueSize); be16 (uint32 firstGlyph); be16 (uint32 count); valueBytes ]
            | true -> Array.concat [ be16 10u; be16 (uint32 tableValueSize); be16 (uint32 firstGlyph); be16 (uint32 count); Array.truncate (max 0 (valueBytes.Length - 1)) valueBytes ]
        return bytes, [ uint32 (max 0 (firstGlyph - 1)); uint32 firstGlyph; uint32 (max 0 (firstGlyph + count - 1)); uint32 (firstGlyph + count) ]
    }

/// <summary>Generates a format 6 single lookup, whole or broken.</summary>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>The bytes and the boundary glyphs.</returns>
let private genFormat6 (valueSize: int) (numGlyphs: int) =
    gen {
        let unitSize = 2 + valueSize
        let! nSingles = Gen.choose (1, 5)
        let! glyphs = Gen.listOfLength nSingles (Gen.choose (0, max 1 (numGlyphs - 1))) |> Gen.map List.sort
        let! values = Gen.listOfLength nSingles (Gen.choose (0, 255) |> Gen.map uint32)
        let unit glyph value = Array.concat [ be16 (uint32 glyph); writeValue valueSize value ]
        let terminator = Array.concat [ be16 0xFFFFu; writeValue valueSize 0u ]
        let units = List.map2 unit glyphs values
        let! broken = Gen.elements [ 0; 0; 1; 2; 3 ]
        let header, body =
            match broken with
            | 1 -> binSearchHeader (unitSize + 1) (nSingles + 1), Array.concat (units @ [ terminator ])
            | 2 ->
                let unsorted = List.rev units
                binSearchHeader unitSize (nSingles + 1), Array.concat (unsorted @ [ terminator ])
            | 3 -> binSearchHeader unitSize (nSingles + 2), Array.concat (units @ [ terminator; terminator ])
            | _ -> binSearchHeader unitSize (nSingles + 1), Array.concat (units @ [ terminator ])
        let bytes = Array.concat [ be16 6u; header; body ]
        let boundaries = glyphs |> List.collect (fun glyph -> [ uint32 (max 0 (glyph - 1)); uint32 glyph; uint32 (glyph + 1) ])
        return bytes, boundaries
    }

/// <summary>Generates a format 2 segment lookup, whole or broken.</summary>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>The bytes and the boundary glyphs.</returns>
let private genFormat2 (valueSize: int) (numGlyphs: int) =
    gen {
        let unitSize = 4 + valueSize
        let! nSegments = Gen.choose (1, 4)
        let! starts = Gen.listOfLength nSegments (Gen.choose (0, max 0 (numGlyphs - 2))) |> Gen.map List.sort
        let! lengths = Gen.listOfLength nSegments (Gen.choose (1, 6))
        let segments = List.map2 (fun start length -> start, min (numGlyphs - 1) (start + length)) starts lengths
        let! values = Gen.listOfLength nSegments (Gen.choose (0, 255) |> Gen.map uint32)
        let unit (first, last) value = Array.concat [ be16 (uint32 last); be16 (uint32 first); writeValue valueSize value ]
        let terminator = Array.concat [ be16 0xFFFFu; be16 0xFFFFu; writeValue valueSize 0u ]
        let units = List.map2 unit segments values
        let! broken = Gen.elements [ 0; 0; 1; 2; 3; 4 ]
        let header, body =
            match broken with
            | 1 ->
                let overlap = List.mapi (fun i (first, last) -> if i = 0 then (first, last) else (max 0 (first - 1), last)) segments
                let units = List.map2 unit overlap values
                binSearchHeader unitSize (nSegments + 1), Array.concat (units @ [ terminator ])
            | 2 -> binSearchHeader unitSize (nSegments + 1), Array.concat (List.rev units @ [ terminator ])
            | 3 -> binSearchHeader (unitSize - 1) (nSegments + 1), Array.concat (units @ [ terminator ])
            | 4 -> binSearchHeader unitSize (nSegments + 4), Array.concat (units @ [ terminator ])
            | _ -> binSearchHeader unitSize (nSegments + 1), Array.concat (units @ [ terminator ])
        let bytes = Array.concat [ be16 2u; header; body ]
        let boundaries = segments |> List.collect (fun (first, last) -> [ uint32 (max 0 (first - 1)); uint32 first; uint32 last; uint32 (last + 1) ])
        return bytes, boundaries
    }

/// <summary>Sequences a list of generators into a generator of a list.</summary>
/// <param name="gens">The generators.</param>
/// <returns>The generator.</returns>
let rec private sequenceGens (gens: Gen<'T> list) =
    match gens with
    | [] -> gen { return [] }
    | first :: rest ->
        gen {
            let! head = first
            let! tail = sequenceGens rest
            return head :: tail
        }

/// <summary>Generates a format 4 segment-array lookup, whole or broken.</summary>
/// <param name="valueSize">The value width.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <returns>The bytes and the boundary glyphs.</returns>
let private genFormat4 (valueSize: int) (numGlyphs: int) =
    gen {
        let! nSegments = Gen.choose (1, 4)
        let! starts = Gen.listOfLength nSegments (Gen.choose (0, max 0 (numGlyphs - 2))) |> Gen.map List.sort
        let! lengths = Gen.listOfLength nSegments (Gen.choose (1, 4))
        let segments = List.map2 (fun start length -> start, min (numGlyphs - 1) (start + length)) starts lengths
        let! valueArrays = segments |> List.map (fun (first, last) -> Gen.listOfLength (last - first + 1) (Gen.choose (0, 255) |> Gen.map uint32)) |> sequenceGens
        let valueBytes = valueArrays |> List.map (fun values -> values |> List.map (writeValue valueSize) |> Array.concat)
        let arraysStart = 12 + 6 * (nSegments + 1)
        let rec offsets (index: int) (offset: int) (acc: int list) =
            match index >= nSegments with
            | true -> List.rev acc
            | false -> offsets (index + 1) (offset + valueBytes[index].Length) (offset :: acc)
        let valuesZ = offsets 0 arraysStart []
        let units =
            List.mapi (fun i (first, last) -> Array.concat [ be16 (uint32 last); be16 (uint32 first); be16 (uint32 valuesZ[i]) ]) segments
            |> Array.concat
        let terminator = Array.concat [ be16 0xFFFFu; be16 0xFFFFu; be16 0u ]
        let arrays = valueBytes |> Array.concat
        let! broken = Gen.elements [ false; true ]
        let bytes =
            match broken with
            | false -> Array.concat [ be16 4u; binSearchHeader 6 (nSegments + 1); units; terminator; arrays ]
            | true -> Array.concat [ be16 4u; binSearchHeader 6 (nSegments + 1); units; terminator; Array.truncate (max 0 (arrays.Length - valueSize)) arrays ]
        let boundaries = segments |> List.collect (fun (first, last) -> [ uint32 (max 0 (first - 1)); uint32 first; uint32 last; uint32 (last + 1) ])
        return bytes, boundaries
    }

/// <summary>Generates an unknown-format lookup, which sanitizes but has no value.</summary>
/// <returns>The bytes and the boundary glyphs.</returns>
let private genUnknown =
    gen {
        let! format = Gen.elements [ 3u; 5u; 7u; 11u; 12u ]
        let! tail = Gen.listOfLength 4 (Gen.choose (0, 255) |> Gen.map byte)
        let bytes = Array.concat [ be16 format; List.toArray tail ]
        return bytes, [ 0u; 1u; 10u; 0xFFFFu ]
    }

/// <summary>Generates a lookup case.</summary>
let private genLookupCase =
    gen {
        let! valueSize = Gen.elements [ 1; 2; 4 ]
        let! numGlyphs = Gen.choose (1, 40)
        let! format = Gen.elements [ 0; 2; 4; 6; 8; 10; 3; 5 ]
        let! bytes, boundaries =
            match format with
            | 0 -> genFormat0 valueSize numGlyphs
            | 2 -> genFormat2 valueSize numGlyphs
            | 4 -> genFormat4 valueSize numGlyphs
            | 6 -> genFormat6 valueSize numGlyphs
            | 8 -> genFormat8 valueSize
            | 10 -> genFormat10 valueSize
            | _ -> genUnknown
        let! randoms = Gen.listOfLength 20 (Gen.choose (0, numGlyphs + 8) |> Gen.map uint32)
        let glyphs = List.distinct ([ 0u; uint32 (numGlyphs - 1); uint32 numGlyphs; 0xFFFFu ] @ boundaries @ randoms)
        return { Bytes = bytes; ValueSize = valueSize; NumGlyphs = numGlyphs; Glyphs = Array.ofList glyphs }
    }

/// <summary>Generates an obsolete class-table case, whole or truncated.</summary>
let private genClassCase =
    gen {
        let! wide = Gen.elements [ false; true ]
        let! count = Gen.choose (1, 12)
        let! firstGlyph = Gen.choose (0, 30)
        let classSize = if wide then 2 else 1
        let! classes = Gen.listOfLength count (Gen.choose (0, 5))
        let classBytes = classes |> List.map (fun value -> writeValue classSize (uint32 value)) |> Array.concat
        let! broken = Gen.elements [ false; true; true ]
        let bytes =
            match broken with
            | false -> Array.concat [ be16 (uint32 firstGlyph); be16 (uint32 count); classBytes ]
            | true -> Array.concat [ be16 (uint32 firstGlyph); be16 (uint32 count); Array.truncate (max 0 (classBytes.Length - classSize)) classBytes ]
        let! randoms = Gen.listOfLength 20 (Gen.choose (0, firstGlyph + count + 4) |> Gen.map uint32)
        let glyphs = List.distinct ([ 0u; uint32 firstGlyph; uint32 (firstGlyph + count - 1); uint32 (firstGlyph + count); 0xFFFFu ] @ randoms)
        return { Bytes = bytes; FirstGlyph = uint16 firstGlyph; Count = count; Wide = wide; Glyphs = Array.ofList glyphs }
    }

/// <summary>The P8.S01 generated-lookup and class-table differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P8.S01" [
        sampled "generated lookups" genLookupCase 5000 oursLookup oracleLookup
        sampled "generated class tables" genClassCase 5000 oursClass oracleClass
    ]
