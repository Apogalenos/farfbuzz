module Farfbuzz.Diff.FeatApiDiff

open System
open System.IO
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Diff.OracleFaces

/// <summary>The number of mutated feat tables of the differential.</summary>
[<Literal>]
let private MutatedTables = 5000

/// <summary>The size of the sfnt header and of one table record, in bytes.</summary>
[<Literal>]
let private SfntHeaderSize = 28

/// <summary>Wraps a feat table in an sfnt holding nothing else.</summary>
/// <param name="table">The feat bytes.</param>
/// <returns>The file bytes.</returns>
let private sfntOf (table: byte array) =
    let be32 (value: uint32) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]
    Array.concat
        [ be32 0x00010000u
          [| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
          be32 (uint32 Tags.Feat)
          be32 0u
          be32 (uint32 SfntHeaderSize)
          be32 (uint32 table.Length)
          table ]

/// <summary>The feature types the queries ask about: those of the table, the invalid type and a few absent ones.</summary>
/// <param name="types">The types the table lists.</param>
/// <returns>The types to ask.</returns>
let private probes (types: uint32 array) = Array.distinct (Array.append types [| 0xFFFFu; 0u; 1u; 14u; 18u; 40u |])

/// <summary>The start offsets and capacities of the window queries.</summary>
let private windows = [| for start in [ 0u; 1u; 2u; 5u; 100u ] do for count in [ 0u; 1u; 3u; 100u ] -> struct (start, count) |]

/// <summary>Everything the three feat functions answer on HarfBuzz's face, as text.</summary>
/// <param name="face">The oracle face.</param>
/// <returns>The answers.</returns>
let private theirs (face: nativeint) =
    let mutable none = 0u
    let total = Native.hb_aat_layout_get_feature_types (face, 0u, &none, Unchecked.defaultof<uint32 array>)
    let mutable all = total
    let everything = Array.zeroCreate (int total)
    Native.hb_aat_layout_get_feature_types (face, 0u, &all, everything) |> ignore
    let lines = ResizeArray<string>()
    lines.Add $"total {total}"
    for struct (start, count) in windows do
        let mutable written = count
        let buffer = Array.zeroCreate (int count)
        let reported = Native.hb_aat_layout_get_feature_types (face, start, &written, buffer)
        lines.Add $"types {start} {count}: {reported} {written} {String.Join(',', Array.take (int written) buffer)}"
    for featureType in probes everything do
        lines.Add $"name {featureType}: {Native.hb_aat_layout_feature_type_get_name_id (face, featureType)}"
        for struct (start, count) in windows do
            let mutable written = count
            let mutable defaultIndex = 0xDEADu
            let buffer = Array.zeroCreate (int count * 4)
            let reported = Native.hb_aat_layout_feature_type_get_selector_infos (face, featureType, start, &written, buffer, &defaultIndex)
            lines.Add $"selectors {featureType} {start} {count}: {reported} {written} {defaultIndex} {String.Join(',', Array.take (int written * 4) buffer)}"
    lines.ToArray()

/// <summary>The same answers from Farfbuzz.</summary>
/// <param name="face">The Farfbuzz face.</param>
/// <returns>The answers.</returns>
let private ours (face: Face) =
    let feat = (OtFace.create face).Feat.Value
    let total = Feat.featureTypeCount feat
    let everything = (Feat.featureTypes feat 0u UInt32.MaxValue).AsSpan().ToArray() |> Array.map uint32
    let lines = ResizeArray<string>()
    lines.Add $"total {total}"
    for struct (start, count) in windows do
        let written = Feat.featureTypes feat start count
        lines.Add $"types {start} {count}: {total} {written.Length} {String.Join(',', written.AsSpan().ToArray())}"
    for featureType in probes everything do
        lines.Add $"name {featureType}: {Feat.featureNameId feat (uint16 featureType)}"
        for struct (start, count) in windows do
            let struct (reported, defaultIndex, infos) = Feat.selectorInfos feat (uint16 featureType) start count
            let words = infos.AsSpan().ToArray() |> Array.collect (fun info -> [| info.NameId; info.Enable; info.Disable; info.Reserved |])
            lines.Add $"selectors {featureType} {start} {count}: {reported} {infos.Length} {defaultIndex} {String.Join(',', words)}"
    lines.ToArray()

/// <summary>Compares both engines on one feat table.</summary>
/// <param name="description">What the table is, for the failure text.</param>
/// <param name="table">The feat bytes.</param>
/// <returns>The first difference, or none, and whether HarfBuzz rejected the table.</returns>
let private compare (description: string) (table: byte array) =
    let file = sfntOf table
    let blob = Native.hb_blob_create (file, uint32 file.Length, 0, 0n, 0n)
    let face = Native.hb_face_create (blob, 0u)
    let expected = theirs face
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    let actual = ours (Face.create (ReadOnlyMemory file) 0u)
    let differing = Array.zip expected actual |> Array.tryFind (fun (a, b) -> a <> b)
    let message = differing |> Option.map (fun (a, b) -> $"{description}: HarfBuzz {a}, Farfbuzz {b}")
    struct (message, expected[0] = "total 0")

/// <summary>Rewrites one field of a table: 0, 1, the value minus and plus one, the largest value, or an offset past the table.</summary>
/// <param name="table">The table.</param>
/// <param name="random">The random source.</param>
/// <returns>The description and the changed copy.</returns>
let private mutated (table: byte array) (random: Random) =
    let copy = Array.copy table
    let width = [| 1; 2; 4 |][random.Next 3]
    let at = random.Next(0, max 1 (table.Length - width))
    let current = Array.fold (fun value index -> value * 256L + int64 copy[at + index]) 0L [| 0 .. min width (table.Length - at) - 1 |]
    let largest = (1L <<< (8 * width)) - 1L
    let kind = random.Next 7
    let value =
        match kind with
        | 0 -> 0L
        | 1 -> 1L
        | 2 -> max 0L (current - 1L)
        | 3 -> min largest (current + 1L)
        | 4 -> largest
        | 5 -> int64 table.Length + 1L
        | _ -> int64 (random.Next(0, table.Length * 2))
    for index in 0 .. min width (table.Length - at) - 1 do
        copy[at + index] <- byte ((value >>> (8 * (width - 1 - index))) &&& 0xFFL)
    let result =
        match random.Next 12 with
        | 0 -> Array.take (random.Next(0, table.Length)) copy
        | _ -> copy
    $"{width}-byte field at {at} set to {value}, table {result.Length} bytes", result

/// <summary>The differential of the three feat functions: the real faces, and 5,000 tables with one field rewritten.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P8.S14" [
        testCase "P8.S14 feat selectors and feature types" (fun () ->
            let tables =
                [| for struct (font, index) in OracleFaces.all () do
                       let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
                       let feat = (OtFace.create face).Feat.Value
                       if Feat.hasData feat then yield feat.Table.ToArray()
                   let apiFont = Path.Combine(RepositoryPaths.upstream (), "test", "api", "fonts", "aat-feat.ttf")
                   let face = Face.create (ReadOnlyMemory (File.ReadAllBytes apiFont)) 0u
                   yield (OtFace.create face).Feat.Value.Table.ToArray() |]
            let failures = ResizeArray<string>()
            for table in tables do
                let struct (message, _) = compare "real feat table" table
                message |> Option.iter failures.Add
            let random = Random 5232
            let mutable rejected = 0
            for _ in 1 .. MutatedTables do
                let description, bytes = mutated tables[random.Next tables.Length] random
                let struct (message, wasRejected) = compare description bytes
                message |> Option.iter failures.Add
                if wasRejected then rejected <- rejected + 1
            printfn "P8.S14 feat: %d real feat tables and %d tables with one field rewritten compared with HarfBuzz; HarfBuzz rejects %d of the rewritten tables" tables.Length MutatedTables rejected
            Expect.isEmpty failures ("feat API differences:\n" + String.concat "\n" (Seq.truncate 20 failures)))
    ]
