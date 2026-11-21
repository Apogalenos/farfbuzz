namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Runtime.InteropServices

/// <summary>An OpenType face: a sanitized container and the directory of one selected face.</summary>
type Face =
    internal
        { Index: uint32
          File: ReadOnlyMemory<byte>
          Kind: ContainerKind
          BaseOffset: uint32
          Directory: ImmutableArray<TableRecord>
          Upem: int
          GlyphCount: int
          Accelerators: AccelSlot }

/// <summary>Opening OpenType files and reading their table directories.</summary>
[<RequireQualifiedAccess>]
module Face =
    /// <summary>The units per em of a face, <c>hb_face_get_upem</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The units per em.</returns>
    let upem (face: Face) =
        face.Upem

    /// <summary>The number of glyphs of a face, <c>hb_face_get_glyph_count</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The glyph count.</returns>
    let glyphCount (face: Face) =
        face.GlyphCount

    /// <summary>The empty face, <c>hb_face_get_empty</c>: 1000 units per em and no glyphs.</summary>
    let empty =
        { Index = 0u
          File = ReadOnlyMemory<byte>.Empty
          Kind = Unrecognized
          BaseOffset = 0u
          Directory = ImmutableArray.Empty
          Upem = 1000
          GlyphCount = 0
          Accelerators = AccelSlot() }

    /// <summary>Compares a key tag with a table directory record stored as bytes.</summary>
    [<Struct>]
    type private RecordComparer =
        { Key: Tag }
        interface IRecordComparer with
            member this.Compare (data, index) =
                let record = MemoryMarshal.Read<TableRecord>(data.Slice(index * 16, 16))
                let value = uint32 record.Tag
                match uint32 this.Key > value, uint32 this.Key < value with
                | true, _ -> 1
                | false, true -> -1
                | false, false -> 0

    /// <summary>Finds a table, using linear search below 16 records and binary search above, as upstream.</summary>
    /// <param name="directory">The records.</param>
    /// <param name="tag">The table tag.</param>
    /// <returns>The matching record, or none.</returns>
    let private tryFindIn (directory: ImmutableArray<TableRecord>) (tag: Tag) : TableRecord voption =
        let records = directory.AsSpan()
        let bytes = MemoryMarshal.AsBytes records
        let comparer = { RecordComparer.Key = tag }
        let found =
            match records.Length < 16 with
            | true -> Binary.lfind comparer bytes 0 records.Length
            | false -> Binary.bsearch comparer bytes 0 (records.Length - 1)
        match found with
        | ValueSome index -> ValueSome records[index]
        | ValueNone -> ValueNone

    /// <summary>Returns a table's sub-blob, clamped as <c>hb_blob_create_sub_blob</c> does.</summary>
    /// <param name="file">The sanitized file.</param>
    /// <param name="baseOffset">The base the record offset is relative to.</param>
    /// <param name="record">The table record.</param>
    /// <returns>The table bytes, or empty.</returns>
    let private referenceTableFrom (file: ReadOnlyMemory<byte>) (baseOffset) (record: TableRecord) : ReadOnlyMemory<byte> =
        let offset = baseOffset + record.Offset
        match record.Length, offset >= uint32 file.Length with
        | 0u, _ -> ReadOnlyMemory<byte>.Empty
        | _, true -> ReadOnlyMemory<byte>.Empty
        | length, false ->
            let start = int offset
            let count = int (min length (uint32 (file.Length - start)))
            file.Slice(start, count)

    /// <summary>Finds a table record by tag, <c>OpenTypeOffsetTable::find_table_index</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="tag">The table tag.</param>
    /// <returns>The record, or none.</returns>
    let internal tryFindTable (face: Face) (tag: Tag) =
        tryFindIn face.Directory tag

    /// <summary>Returns a table's bytes, empty when absent, <c>hb_face_reference_table</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="tag">The table tag.</param>
    /// <returns>The table bytes, or empty.</returns>
    let referenceTable (face: Face) (tag: Tag) : ReadOnlyMemory<byte> =
        match uint32 tag with
        | 0u -> ReadOnlyMemory<byte>.Empty
        | _ ->
            match tryFindIn face.Directory tag with
            | ValueSome record -> referenceTableFrom face.File face.BaseOffset record
            | ValueNone -> ReadOnlyMemory<byte>.Empty

    /// <summary>Returns the directory tags in file order, <c>hb_face_get_table_tags</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The tags.</returns>
    let tableTags (face: Face) : ImmutableArray<Tag> =
        Sealed.init face.Directory.Length (fun index -> face.Directory[index].Tag)

    /// <summary>Returns the number of faces in a file, <c>hb_face_count</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <returns>The face count, or 0 when the file does not sanitize.</returns>
    let count (file: ReadOnlyMemory<byte>) =
        match OpenFile.sanitize file.Span with
        | true -> OpenFile.faceCount file.Span
        | false -> 0

    /// <summary>Creates a face by sanitizing the file and selecting an index, <c>hb_face_create</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="index">The face index; the high 16 bits are kept but only the low 16 select a face.</param>
    /// <returns>The face, empty when the file does not sanitize.</returns>
    let create (file: ReadOnlyMemory<byte>) (index) : Face =
        let sanitized =
            match OpenFile.sanitize file.Span with
            | true -> file
            | false -> ReadOnlyMemory<byte>.Empty
        let kind = (|Container|) (Binary.tag sanitized.Span 0)
        let struct (baseOffset, directory) = OpenFile.face sanitized.Span index
        let head =
            match tryFindIn directory Tags.Head with
            | ValueSome record -> Sanitize.table (Head.Sanitizer()) (referenceTableFrom sanitized baseOffset record)
            | ValueNone -> ReadOnlyMemory<byte>.Empty
        let maxp =
            match tryFindIn directory Tags.Maxp with
            | ValueSome record -> Sanitize.table (Maxp.Sanitizer()) (referenceTableFrom sanitized baseOffset record)
            | ValueNone -> ReadOnlyMemory<byte>.Empty
        let upperMaxp =
            match tryFindIn directory Tags.UpperMaxp with
            | ValueSome record -> Sanitize.table (Maxp.UpperSanitizer()) (referenceTableFrom sanitized baseOffset record)
            | ValueNone -> ReadOnlyMemory<byte>.Empty
        { Index = index
          File = sanitized
          Kind = kind
          BaseOffset = baseOffset
          Directory = directory
          Upem = Head.upem head.Span
          GlyphCount = Maxp.glyphCount maxp upperMaxp
          Accelerators = AccelSlot() }

    /// <summary>Creates a face only when the index exists, <c>hb_face_create_or_fail</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="index">The face index; the high 16 bits are kept but only the low 16 select a face.</param>
    /// <returns>The face, or none when the index is out of range.</returns>
    let createOrFail (file: ReadOnlyMemory<byte>) (index) : Face voption =
        match (index &&& 0xFFFFu) >= uint32 (count file) with
        | true -> ValueNone
        | false -> ValueSome (create file index)

    /// <summary>Replaces the glyph count, <c>hb_face_set_glyph_count</c>; the face keeps its tables and accelerators are
    /// rebuilt from the new count on the next font.</summary>
    /// <param name="glyphCount">The new glyph count.</param>
    /// <param name="face">The face.</param>
    /// <returns>The face with the glyph count replaced.</returns>
    let internal withGlyphCount glyphCount (face: Face) =
        { face with GlyphCount = glyphCount; Accelerators = AccelSlot() }

    /// <summary>Replaces the units per em, <c>hb_face_set_upem</c> (<c>hb-face.cc:836-843</c>); the accelerators are rebuilt from the new
    /// value on the next font.</summary>
    /// <param name="upem">The new units per em.</param>
    /// <param name="face">The face.</param>
    /// <returns>The face with the units per em replaced.</returns>
    let withUpem upem (face: Face) =
        { face with Upem = upem; Accelerators = AccelSlot() }

    /// <summary>Assembles a face from the bytes of its tables, the face <c>hb_face_create_for_tables</c> makes from a table callback
    /// (<c>hb-face.cc:130</c>): the tables become one file under a synthetic directory sorted by tag, a table of no bytes is absent
    /// (the callback answers an empty blob for both), and the units per em and glyph count are read from <c>head</c> and <c>maxp</c>
    /// as <see cref="create"/> reads them.</summary>
    /// <param name="index">The face index to report.</param>
    /// <param name="tables">The tables as pairs of tag and bytes.</param>
    /// <returns>The face; its directory is that of the tables, not of a container.</returns>
    let ofTables index (tables: ImmutableArray<struct (Tag * ReadOnlyMemory<byte>)>) =
        let present =
            tables.AsSpan().ToArray()
            |> Array.filter (fun (struct (_, bytes)) -> bytes.Length > 0)
            |> Array.sortBy (fun (struct (tag, _)) -> uint32 tag)
        let offsets = present |> Array.map (fun (struct (_, bytes)) -> bytes.Length) |> Array.scan (+) 0
        let file = ReadOnlyMemory<byte>(present |> Array.collect (fun (struct (_, bytes)) -> bytes.ToArray()))
        let directory =
            Sealed.init present.Length (fun position ->
                let struct (tag, _) = present[position]
                { Tag = tag; CheckSum = 0u; Offset = uint32 offsets[position]; Length = uint32 (offsets[position + 1] - offsets[position]) })
        let table (tag: Tag) sanitizer =
            match tryFindIn directory tag with
            | ValueSome record -> Sanitize.table sanitizer (referenceTableFrom file 0u record)
            | ValueNone -> ReadOnlyMemory<byte>.Empty
        let head = table Tags.Head (Head.Sanitizer())
        let maxp = table Tags.Maxp (Maxp.Sanitizer())
        let upperMaxp = table Tags.UpperMaxp (Maxp.UpperSanitizer())
        { Index = index
          File = file
          Kind = Sfnt
          BaseOffset = 0u
          Directory = directory
          Upem = Head.upem head.Span
          GlyphCount = Maxp.glyphCount maxp upperMaxp
          Accelerators = AccelSlot() }
