module Farfbuzz.Diff.Mutate

open System
open Farfbuzz

/// <summary>One deterministic mutated font and its description.</summary>
[<Struct>]
type Mutation =
    { Description: string
      Bytes: byte array }

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Hashes a string without relying on the process-randomised string hash.</summary>
/// <param name="text">The text.</param>
/// <returns>The FNV-1a 32-bit hash.</returns>
let private nameHash (text: string) =
    text
    |> Seq.fold (fun hash byte -> (hash ^^^ uint32 (int byte)) * 16777619u) 2166136261u

/// <summary>Finds the first sfnt offset table in a bare sfnt or collection.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The offset, or -1.</returns>
let private directoryOffset (bytes: byte array) =
    let span = ReadOnlySpan bytes
    match BinaryCalls.u32 span 0 with
    | 0x00010000u | 0x4F54544Fu | 0x74727565u | 0x74797031u -> 0
    | 0x74746366u ->
        let offset = int (BinaryCalls.u32 span 12)
        match offset > 0 && offset + 12 <= bytes.Length with
        | true -> offset
        | false -> -1
    | _ -> -1

/// <summary>Reads the table count at an offset table.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="start">The offset table.</param>
/// <returns>The count.</returns>
let private tableCount (bytes: byte array) (start: int) =
    int (BinaryCalls.u16 (ReadOnlySpan bytes) (start + 4))

/// <summary>Reads a table record's offset.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="start">The offset table.</param>
/// <param name="index">The record index.</param>
/// <returns>The table offset.</returns>
let private tableOffset (bytes: byte array) (start: int) (index: int) =
    int (BinaryCalls.u32 (ReadOnlySpan bytes) (start + 12 + index * 16 + 8))

/// <summary>Reads a table record's length.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="start">The offset table.</param>
/// <param name="index">The record index.</param>
/// <returns>The table length.</returns>
let private tableLength (bytes: byte array) (start: int) (index: int) =
    BinaryCalls.u32 (ReadOnlySpan bytes) (start + 12 + index * 16 + 12) |> Farfbuzz.Harness.TailCalls.settle

/// <summary>Flips one or more bytes.</summary>
/// <param name="bytes">The input bytes.</param>
/// <param name="random">The random source.</param>
/// <param name="count">The number of flips.</param>
/// <returns>The mutated bytes.</returns>
let private flipBytes (bytes: byte array) (random: Random) (count: int) =
    let result = Array.copy bytes
    for _ in 1 .. count do
        match result.Length with
        | 0 -> ()
        | length ->
            let index = random.Next length
            result[index] <- result[index] ^^^ byte (1 <<< random.Next 8)
    result

/// <summary>Mutates a font deterministically from its name and mutation index.</summary>
/// <param name="fileName">The font file name.</param>
/// <param name="k">The mutation index.</param>
/// <returns>The mutated bytes and a one-line description.</returns>
let mutate (fileName: string) (k: int) : Mutation =
    let bytes = System.IO.File.ReadAllBytes fileName
    let random = Random(int (nameHash (fileName + "#" + string k)))
    let start = directoryOffset bytes
    let count = match start >= 0 && start + 12 <= bytes.Length with | true -> tableCount bytes start | false -> 0
    let chooseTable () = match count with | 0 -> -1 | _ -> random.Next count
    match k % 8 with
    | 0 ->
        { Description = "flip one to eight bytes"; Bytes = flipBytes bytes random (1 + random.Next 8) }
    | 1 when start >= 0 && count > 0 ->
        let record = start + 12 + random.Next (count * 16)
        let at = record + random.Next 16
        let result = Array.copy bytes
        result[at] <- result[at] ^^^ 0xFFuy
        { Description = "flip a table directory byte"; Bytes = result }
    | 2 when start >= 0 && count > 0 ->
        let index = chooseTable ()
        let offset = tableOffset bytes start index
        let endIndex = min bytes.Length (offset + 64)
        match offset >= 0 && offset < endIndex with
        | false -> { Description = "flip a table header byte"; Bytes = flipBytes bytes random 1 }
        | true ->
            let result = Array.copy bytes
            let at = offset + random.Next (endIndex - offset)
            result[at] <- result[at] ^^^ 0xFFuy
            { Description = "flip a table header byte"; Bytes = result }
    | 3 ->
        let cut = match bytes.Length with | 0 -> 0 | length -> 1 + random.Next (length - 1)
        { Description = "truncate the file"; Bytes = bytes[0 .. cut - 1] }
    | 4 when start >= 0 && count > 0 ->
        let index = chooseTable ()
        let record = start + 12 + index * 16 + 12
        let result = Array.copy bytes
        let length = tableLength bytes start index
        let shortened = uint32 (random.Next (int (min length 1000u)))
        result[record..record + 3] <- be32 shortened
        { Description = "shorten a table length"; Bytes = result }
    | 5 when start >= 0 && count > 0 ->
        let index = chooseTable ()
        let record = start + 12 + index * 16 + 12
        let extra = 1 + random.Next 16
        let result = Array.append bytes (Array.zeroCreate extra)
        let length = tableLength bytes start index + uint32 extra
        result[record..record + 3] <- be32 length
        { Description = "lengthen a table"; Bytes = result }
    | 6 when start >= 0 && count > 0 ->
        let index = chooseTable ()
        let offset = tableOffset bytes start index
        let values = [ 0u; 0x7FFFu; 0x8000u; 0xFFFFu ]
        let at = offset + random.Next (min 64 (max 1 (bytes.Length - offset)))
        let result = Array.copy bytes
        let value = values[random.Next values.Length]
        if at + 2 <= result.Length then
            result[at..at + 1] <- be16 value
        { Description = "set a 16-bit header field"; Bytes = result }
    | 7 when start >= 0 && count > 0 ->
        let index = chooseTable ()
        let offset = tableOffset bytes start index
        let values = [ 0u; 0x7FFFFFFFu; 0x80000000u; 0xFFFFFFFFu ]
        let at = offset + random.Next (min 64 (max 1 (bytes.Length - offset)))
        let result = Array.copy bytes
        let value = values[random.Next values.Length]
        if at + 4 <= result.Length then
            result[at..at + 3] <- be32 value
        { Description = "set a 32-bit header field"; Bytes = result }
    | _ -> { Description = "flip a byte fallback"; Bytes = flipBytes bytes random 1 }
