module Farfbuzz.Tests.FaceTests

open System
open System.Buffers.Binary
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Builds one 28-byte sfnt offset table with a single table record.</summary>
/// <param name="tag">The table tag.</param>
/// <param name="tableOffset">The table offset.</param>
/// <param name="tableLength">The table length.</param>
/// <returns>The bytes.</returns>
let private sfntFaceWithRecord (tag: Tag) (tableOffset: uint32) (tableLength: uint32) : byte array =
    Array.concat
        [ be32 0x00010000u
          be16 1u
          be16 0u
          be16 0u
          be16 0u
          be32 (uint32 tag)
          be32 0u
          be32 tableOffset
          be32 tableLength ]

/// <summary>Builds the extended TTC of <c>test-be-num-glyphs.c</c>.</summary>
/// <param name="major">The collection major version.</param>
/// <param name="minor">The collection minor version.</param>
/// <param name="malformed">Whether the second list has an impossible count.</param>
/// <param name="oldOffsets">Whether the first list's offsets are all ones.</param>
/// <returns>The file bytes.</returns>
let private extendedTtc (major: uint32) (minor: uint32) (malformed: bool) (oldOffsets: bool) : byte array =
    let extensionOffset = match major with | 2u -> 32 | _ -> 20
    let oldFaceOffset = extensionOffset + 8
    let newFaceOffset = oldFaceOffset + 28
    let maxpOffset = newFaceOffset + 28
    let upperMaxpOffset = maxpOffset + 6
    let firstOffset = match oldOffsets with | true -> 0xFFFFFFFFu | false -> uint32 oldFaceOffset
    let header =
        Array.concat
            [ be32 0x74746366u
              be16 major
              be16 minor
              be32 2u
              be32 firstOffset
              be32 firstOffset ]
    let padding = Array.zeroCreate (extensionOffset - 20)
    let extension =
        Array.concat
            [ be32 (match malformed with | true -> 0xFFFFFFFFu | false -> 1u)
              be32 (uint32 newFaceOffset) ]
    let maxp = Array.concat [ be32 0x00005000u; be16 5u ]
    let upperMaxp = Array.concat [ be32 0x00005000u; be24 0x10001u ]
    Array.concat
        [ header
          padding
          extension
          sfntFaceWithRecord Tags.Maxp (uint32 maxpOffset) 6u
          sfntFaceWithRecord Tags.UpperMaxp (uint32 upperMaxpOffset) 7u
          maxp
          upperMaxp ]

/// <summary>Builds a Mac resource fork with sfnt resources for the face tests.</summary>
/// <param name="types">Each type record's tag and resource payloads.</param>
/// <returns>The dfont bytes.</returns>
let private dfont (types: (Tag * byte array list) list) : byte array =
    let resources = types |> List.collect snd
    let dataParts = resources |> List.map (fun bytes -> Array.concat [ be32 (uint32 bytes.Length); bytes ])
    let dataOffsets =
        dataParts
        |> List.scan (fun offset part -> offset + part.Length) 0
        |> List.take dataParts.Length
    let data = dataParts |> Array.concat
    let typeCount = types.Length
    let typeListOffset = 28
    let resourceArrayOffset = 2 + typeCount * 8
    let typeRecords, _ =
        types
        |> List.mapi (fun index (tag, payloads) ->
            let before =
                types
                |> List.take index
                |> List.sumBy (fun (_, previous) -> previous.Length)
            let resourcesZ = resourceArrayOffset + before * 12
            Array.concat [ be32 (uint32 tag); be16 (uint32 (payloads.Length - 1)); be16 (uint32 resourcesZ) ])
        |> List.fold (fun (records, offset) record -> record :: records, offset + record.Length) ([], 0)
    let typeRecords = typeRecords |> List.rev
    let resourceRecords =
        List.zip resources dataOffsets
        |> List.map (fun (_, offset) ->
            Array.concat [ be16 0u; be16 0xFFFFu; [| 0uy |]; be24 (uint32 offset); be32 0u ])
    let map =
        Array.concat
            [ Array.zeroCreate 24
              be16 (uint32 typeListOffset)
              be16 0u
              be16 (uint32 (typeCount - 1))
              Array.concat typeRecords
              Array.concat resourceRecords ]
    let dataOffset = 256u
    let mapOffset = uint32 (int dataOffset + data.Length)
    Array.concat
        [ be32 dataOffset
          be32 mapOffset
          be32 (uint32 data.Length)
          be32 (uint32 map.Length)
          Array.zeroCreate (int dataOffset - 16)
          data
          map ]

/// <summary>Builds a file with one table record at a chosen offset and length.</summary>
/// <param name="offset">The record offset.</param>
/// <param name="length">The record length.</param>
/// <param name="content">The bytes after the record.</param>
/// <returns>The file bytes.</returns>
let private sfntWithRecord (offset: uint32) (length: uint32) (content: byte array) : byte array =
    Array.concat [ sfntFaceWithRecord Tags.Head offset length; content ]

/// <summary>Linear search model for the small-directory case.</summary>
/// <param name="tags">The directory tags.</param>
/// <param name="key">The query.</param>
/// <param name="index">The next index.</param>
/// <returns>The index, or none.</returns>
let rec private modelLinear (tags: uint32 array) (key: uint32) (index: int) =
    match index < tags.Length with
    | false -> ValueNone
    | true ->
        match key > tags[index], key < tags[index] with
        | false, false -> ValueSome index
        | _ -> modelLinear tags key (index + 1)

/// <summary>Port of <c>hb_bsearch_impl</c> over a tag array, for the unsorted-directory property.</summary>
/// <param name="tags">The directory tags.</param>
/// <param name="key">The query.</param>
/// <param name="lo">The first candidate.</param>
/// <param name="hi">The last candidate.</param>
/// <returns>The index, or none.</returns>
let rec private modelBinary (tags: uint32 array) (key: uint32) (lo: int) (hi: int) =
    match lo > hi with
    | true -> ValueNone
    | false ->
        let mid = int ((uint32 lo + uint32 hi) >>> 1)
        match key > tags[mid], key < tags[mid] with
        | false, false -> ValueSome mid
        | true, _ -> modelBinary tags key (mid + 1) hi
        | false, true -> modelBinary tags key lo (mid - 1)

/// <summary>Chooses the linear or binary model exactly as upstream does.</summary>
/// <param name="tags">The directory tags.</param>
/// <param name="key">The query.</param>
/// <returns>The index, or none.</returns>
let private modelSearch (tags: uint32 array) (key: uint32) =
    match tags.Length < 16 with
    | true -> modelLinear tags key 0
    | false -> modelBinary tags key 0 (tags.Length - 1)

/// <summary>Reads directory tags with a plain byte walk, independent of the library.</summary>
/// <param name="bytes">The file bytes.</param>
/// <param name="index">The next record.</param>
/// <param name="count">The number of records.</param>
/// <param name="tags">The tags found so far, latest first.</param>
/// <returns>The tags in file order.</returns>
[<TailCall>]
let rec private readTags (bytes: ReadOnlySpan<byte>) (index: int) (count: int) (tags: uint32 list) =
    match index < count with
    | false -> List.rev tags
    | true -> readTags bytes (index + 1) count (BinaryPrimitives.ReadUInt32BigEndian(bytes.Slice(12 + index * 16, 4)) :: tags)

/// <summary>Face creation, table lookup and the P2.S01 unit tests.</summary>
[<Tests>]
let tests =
    testList "P2.S01" [
        test "maxp 0.5 gives five glyphs" {
            let maxp = [| 0uy; 0uy; 0x50uy; 0uy; 0uy; 5uy |]
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Maxp, maxp ])) 0u
            Expect.equal face.GlyphCount 5 "glyph count"
        }
        test "an sfnt with no tables has no glyphs" {
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [])) 0u
            Expect.equal face.GlyphCount 0 "empty glyph count"
        }
        testList "extended TTC" [
            for major in [ 1u; 2u ] do
                for minor in [ 1u; 2u ] do
                    yield
                        test $"version {major}.{minor} counts one face and reads the upper MAXP" {
                            let face = Face.create (ReadOnlyMemory (extendedTtc major minor false false)) 0u
                            Expect.equal (Face.count (ReadOnlyMemory (extendedTtc major minor false false))) 1 "face count"
                            Expect.equal face.GlyphCount 0x10001 "glyph count"
                        }
                    yield
                        test $"version {major}.{minor} rejects a malformed second list" {
                            Expect.equal (Face.count (ReadOnlyMemory (extendedTtc major minor true false))) 0 "malformed count"
                        }
                    yield
                        test $"version {major}.{minor} ignores the old offsets" {
                            Expect.equal (Face.count (ReadOnlyMemory (extendedTtc major minor false true))) 1 "old offsets count"
                        }
        ]
        test "createOrFail accepts a named-instance index and rejects an absent face" {
            let file = read "test/api/fonts/Roboto-Regular.abc.ttf"
            let index = 0x10000u
            match Face.createOrFail file index with
            | ValueSome face ->
                Expect.equal face.Index index "index kept"
                Expect.equal (Face.referenceTable face Tags.Head) (Face.referenceTable (Face.create file 0u) Tags.Head) "same head"
            | ValueNone -> Expect.isTrue false "named-instance index must succeed"
            Expect.isTrue (ValueOption.isNone (Face.createOrFail file (uint32 (Face.count file) ||| index))) "absent face"
        }
        test "the empty face has 1000 units and no glyphs or tables" {
            let face = Face.create ReadOnlyMemory.Empty 0u
            Expect.equal face.Upem 1000 "upem"
            Expect.equal face.GlyphCount 0 "glyph count"
            Expect.isEmpty face.Directory "directory"
            Expect.equal (Face.referenceTable face Tags.Head).Length 0 "head"
        }
        test "Roboto's tags are its directory order" {
            let file = read "test/api/fonts/Roboto-Regular.abc.ttf"
            let span = file.Span
            let count = int (BinaryPrimitives.ReadUInt16BigEndian(span.Slice(4, 2)))
            let expected = readTags span 0 count []
            let actual = Face.tableTags (Face.create file 0u) |> Seq.map uint32 |> Seq.toList
            Expect.equal actual expected "table tags"
        }
        test "referenceTable clamps a sub-blob like hb_blob_create_sub_blob" {
            let beyond = Face.create (ReadOnlyMemory (sfntWithRecord 1000u 10u Array.empty)) 0u
            Expect.equal (Face.referenceTable beyond Tags.Head).Length 0 "offset beyond the file"
            let content = [| 1uy; 2uy; 3uy; 4uy |]
            let clamped = Face.create (ReadOnlyMemory (sfntWithRecord 28u 100u content)) 0u
            Expect.equal ((Face.referenceTable clamped Tags.Head).ToArray()) content "length clamped"
            let zero = Face.create (ReadOnlyMemory (sfntWithRecord 28u 0u content)) 0u
            Expect.equal (Face.referenceTable zero Tags.Head).Length 0 "zero length"
            let bytes = sfntWithRecord 32u 4u content
            let atEnd = Face.create (ReadOnlyMemory bytes) 0u
            Expect.equal (Face.referenceTable atEnd Tags.Head).Length 0 "offset at the end"
        }
        testProperty "an unsorted directory finds a tag exactly when hb_bsearch_impl does" <|
            let tagGen =
                gen {
                    let! low = Gen.choose (0, Int32.MaxValue)
                    let! high = Gen.choose (0, 1)
                    return (uint32 low) ||| (uint32 high <<< 31)
                }
            let inputGen = Gen.zip (Gen.listOfLength 20 tagGen) (Gen.listOfLength 10 tagGen)
            Prop.forAll (Arb.fromGen inputGen) (fun (directory, queries) ->
                let tags = directory |> List.map uint32 |> List.toArray
                let tables = directory |> List.map (fun tag -> LanguagePrimitives.UInt32WithMeasure tag, Array.empty<byte>)
                let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u tables)) 0u
                (directory @ queries)
                |> List.forall (fun tag ->
                    let expected = modelSearch tags tag |> ValueOption.isSome
                    let actual = Face.tryFindTable face (LanguagePrimitives.UInt32WithMeasure tag) |> ValueOption.isSome
                    expected = actual))
    ]

/// <summary>The P2.R02 resource-fork and face cases.</summary>
[<Tests>]
let reviewTests =
    let maxp glyphs = Array.concat [ be32 0x00005000u; be16 (uint32 glyphs) ]
    let sfntWith glyphs = sfnt 0x00010000u [ Tags.Maxp, maxp glyphs ]
    testList "P2.R02" [
        test "a dfont reads two sfnt resources from their listed offsets" {
            let file = ReadOnlyMemory (dfont [ Tags.Sfnt, [ sfntWith 5; sfntWith 7 ] ])
            Expect.equal (Face.count file) 2 "face count"
            Expect.equal (Face.create file 0u).GlyphCount 5 "first resource"
            Expect.equal (Face.create file 1u).GlyphCount 7 "second resource"
            Expect.equal (Face.create file 2u).GlyphCount 0 "past the resources"
        }
        test "a dfont continues into a later sfnt type record" {
            let file = ReadOnlyMemory (dfont [ Tags.Sfnt, [ sfntWith 5 ]; Tags.Sfnt, [ sfntWith 7; sfntWith 9 ] ])
            Expect.equal (Face.count file) 1 "face count is the first sfnt type"
            Expect.equal (Face.create file 0u).GlyphCount 5 "first type"
            Expect.equal (Face.create file 1u).GlyphCount 9 "second type's second resource"
        }
        test "a collection with an unknown major version has no face" {
            let font = sfntWith 3
            let file = ReadOnlyMemory (Array.concat [ be32 0x74746366u; be16 3u; be16 0u; be32 1u; be32 20u; font ])
            Expect.equal (Face.count file) 0 "count"
            let face = Face.create file 0u
            Expect.equal face.GlyphCount 0 "glyph count"
            Expect.equal face.Upem 1000 "empty upem"
        }
        test "tag zero references the empty blob" {
            let face = Face.create (read "test/api/fonts/Roboto-Regular.abc.ttf") 0u
            Expect.equal (Face.referenceTable face 0u<tag>).Length 0 "tag zero"
        }
    ]
