module Farfbuzz.Diff.NoLayoutFonts

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Security.Cryptography
open System.Text
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Diff.TestFonts

/// <summary>The layout-table tags renamed out of a stripped font: everything the ot pipeline reads for GSUB/GPOS/GDEF/AAT.</summary>
let private layoutTags =
    [ "GSUB"; "GPOS"; "GDEF"; "kern"; "morx"; "mort"; "kerx"; "trak"; "ankr"; "feat" ]
    |> List.map (fun text -> uint32 (Tag.ofString text))
    |> Set.ofList

/// <summary>Reads a big-endian 16-bit value.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The offset.</param>
/// <returns>The value.</returns>
let private u16 (bytes: byte[]) (offset: int) = BinaryPrimitives.ReadUInt16BigEndian(bytes.AsSpan(offset, 2))

/// <summary>Reads a big-endian 32-bit value.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The offset.</param>
/// <returns>The value.</returns>
let private u32 (bytes: byte[]) (offset: int) = BinaryPrimitives.ReadUInt32BigEndian(bytes.AsSpan(offset, 4))

/// <summary>Reads a big-endian 24-bit value.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The offset.</param>
/// <returns>The value.</returns>
let private u24 (bytes: byte[]) (offset: int) =
    (uint32 bytes[offset] <<< 16) ||| (uint32 bytes[offset + 1] <<< 8) ||| uint32 bytes[offset + 2]

/// <summary>Renames the layout-table tags of one sfnt offset table's directory.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offsetTable">The offset of the offset table.</param>
let private patchDirectory (bytes: byte[]) (offsetTable: int) =
    if offsetTable >= 0 && offsetTable + 6 <= bytes.Length then
        let count = int (u16 bytes (offsetTable + 4))
        for index in 0 .. count - 1 do
            let record = offsetTable + 12 + index * 16
            if record + 4 <= bytes.Length && layoutTags.Contains(u32 bytes record) then
                bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'

/// <summary>The offset-table positions of a collection, one per face offset array entry.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The positions.</returns>
let private collectionOffsetTables (bytes: byte[]) : int list =
    let minor = u16 bytes 6
    let count = int (u32 bytes 8)
    let listOffset =
        match minor >= 1us with
        | false -> 8
        | true -> 8 + 4 + count * 4 + (if u16 bytes 4 = 2us then 12 else 0)
    [ for index in 0 .. count - 1 do
        let entry = listOffset + 4 + index * 4
        if entry + 4 <= bytes.Length then int (u32 bytes entry) else 0 ]

/// <summary>The sfnt offset-table positions of a Mac resource fork.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The positions.</returns>
let private resourceOffsetTables (bytes: byte[]) : int list =
    let dataOffset = int (u32 bytes 0)
    let mapOffset = int (u32 bytes 4)
    if mapOffset + 26 > bytes.Length then
        []
    else
        let typeList = mapOffset + int (u16 bytes (mapOffset + 24))
        let count = int (u16 bytes typeList) + 1
        [ for index in 0 .. count - 1 do
            let record = typeList + 2 + index * 8
            if record + 8 <= bytes.Length && u32 bytes record = uint32 Tags.Sfnt then
                let resources = int (u16 bytes (record + 4)) + 1
                let resourceList = typeList + int (u16 bytes (record + 6))
                for selected in 0 .. resources - 1 do
                    let resource = resourceList + selected * 12
                    if resource + 8 <= bytes.Length then
                        dataOffset + int (u24 bytes (resource + 5)) + 4 ]

/// <summary>Every offset-table position whose directory holds the layout tables to rename.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The positions.</returns>
let private offsetTables (bytes: byte[]) : int list =
    let first = u32 bytes 0
    match first with
    | _ when List.contains first [ uint32 Tags.OpenType; uint32 Tags.TrueType; uint32 Tags.True; uint32 Tags.Typ1 ] -> [ 0 ]
    | _ when first = uint32 Tags.Collection -> collectionOffsetTables bytes
    | _ when first = uint32 Tags.ResourceFork -> resourceOffsetTables bytes
    | _ -> []

/// <summary>Writes a stripped font once, without ever truncating a file another test is reading: an identical file
/// is left alone, a different one is replaced by an atomic rename of a complete temporary file. Tests run in
/// parallel and the oracle maps these files, so a plain rewrite let it read an empty or half-written font (seen as
/// HarfBuzz answering the no-table defaults for a face that has hmtx and hhea).</summary>
/// <param name="target">The stripped file's path.</param>
/// <param name="stripped">The stripped bytes.</param>
let private writeOnce (target: string) (stripped: byte[]) : unit =
    let same = File.Exists target && FileInfo(target).Length = int64 stripped.Length && File.ReadAllBytes(target).AsSpan().SequenceEqual(ReadOnlySpan stripped)
    match same with
    | true -> ()
    | false ->
        let temporary = $"{target}.{Guid.NewGuid():N}.tmp"
        File.WriteAllBytes(temporary, stripped)
        File.Move(temporary, target, true)

/// <summary>Returns a copy of a font whose layout-table tags are renamed out, written beside the originals.</summary>
/// <param name="font">The face.</param>
/// <returns>The stripped face, whose path is the stripped file.</returns>
let strip (font: TestFont) : TestFont =
    let bytes = File.ReadAllBytes font.Path
    let stripped = Array.copy bytes
    offsetTables stripped |> List.iter (patchDirectory stripped)
    let directory = Path.Combine(RepositoryPaths.root (), "artifacts", "no-layout")
    Directory.CreateDirectory directory |> ignore
    let hash = Convert.ToHexString(SHA1.HashData(Encoding.UTF8.GetBytes font.Path)).Substring(0, 12).ToLowerInvariant()
    let target = Path.Combine(directory, $"{hash}-{Path.GetFileName font.Path}#no-layout")
    writeOnce target stripped
    { font with Path = target }

/// <summary>Every stripped face, written once for the whole test run however many tests ask for them.</summary>
let private allStripped : Lazy<ImmutableArray<TestFont>> =
    lazy (TestFonts.all () |> List.map strip |> ImmutableArray.CreateRange)

/// <summary>Every face of every corpus font, with its layout tables stripped so shaping needs none of them.</summary>
/// <returns>The stripped faces.</returns>
let all () : ImmutableArray<TestFont> =
    allStripped.Value
