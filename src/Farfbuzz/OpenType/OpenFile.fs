namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The container an OpenType file uses.</summary>
[<Struct>]
type internal ContainerKind =
    | Sfnt
    | Collection
    | ResourceFork
    | Unrecognized

/// <summary>One record of an sfnt table directory.</summary>
[<Struct>]
type internal TableRecord = { Tag: Tag; CheckSum: uint32; Offset: uint32; Length: uint32 }

/// <summary>Container classification shared by the reader and the face.</summary>
[<AutoOpen>]
module internal ContainerPatterns =
    /// <summary>Classifies an sfnt container tag as <c>hb-open-file.hh</c> does.</summary>
    /// <param name="tag">The first four bytes of the file.</param>
    /// <returns>The container kind.</returns>
    let (|Container|) (tag: Tag) : ContainerKind =
        match tag with
        | Tags.OpenType | Tags.TrueType | Tags.True | Tags.Typ1 -> Sfnt
        | Tags.Collection -> Collection
        | Tags.ResourceFork -> ResourceFork
        | _ -> Unrecognized

/// <summary>Reading and sanitizing OpenType containers: bare sfnt, collections and Mac resource forks.</summary>
[<RequireQualifiedAccess>]
module internal OpenFile =
    /// <summary>The size of an sfnt offset table before its records.</summary>
    [<Literal>]
    let private OffsetTableSize = 12

    /// <summary>The size of one table directory record.</summary>
    [<Literal>]
    let private TableRecordSize = 16

    /// <summary>The size of the collection tag and version.</summary>
    [<Literal>]
    let private CollectionHeaderSize = 8

    /// <summary>The size of a 32-bit array length.</summary>
    [<Literal>]
    let private DirectoryCountSize = 4

    /// <summary>The extra bytes in a version 2.0 collection header.</summary>
    [<Literal>]
    let private CollectionExtraV2 = 12

    /// <summary>The size of a resource fork header.</summary>
    [<Literal>]
    let private ResourceForkHeaderSize = 16

    /// <summary>The size of a resource map.</summary>
    [<Literal>]
    let private ResourceMapSize = 28

    /// <summary>The offset of a resource map's type list.</summary>
    [<Literal>]
    let private ResourceMapTypeListOffset = 24

    /// <summary>The size of one resource type record.</summary>
    [<Literal>]
    let private ResourceTypeRecordSize = 8

    /// <summary>The offset of a type record's resource list.</summary>
    [<Literal>]
    let private ResourceTypeResourcesOffset = 6

    /// <summary>The size of one resource record.</summary>
    [<Literal>]
    let private ResourceRecordSize = 12

    /// <summary>The byte holding the 24-bit resource data offset.</summary>
    [<Literal>]
    let private ResourceOffsetByte = 5

    /// <summary>The length prefix before a resource's sfnt data.</summary>
    [<Literal>]
    let private ResourceDataPrefix = 4

    /// <summary>Sanitizes an sfnt offset table and its directory, <c>OpenTypeOffsetTable.sanitize</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="offset">The offset table offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeOffsetTable (file: ReadOnlySpan<byte>) (offset) (ops) : int voption =
        match Sanitize.fits file.Length offset OffsetTableSize with
        | false -> ValueNone
        | true ->
            let start = int offset
            let count = uint32 (Binary.u16 file (start + 4))
            Sanitize.array file.Length (offset + int64 OffsetTableSize) count (uint32 TableRecordSize) ops

    /// <summary>Deep-checks the entries of a 32-bit offset array relative to a base.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="start">The directory start.</param>
    /// <param name="count">The number of entries.</param>
    /// <param name="baseOffset">The base the offsets are relative to.</param>
    /// <param name="index">The next entry.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeDirectoryEntries
            (file: ReadOnlySpan<byte>) (start) (count) (baseOffset) (index) (ops) : int voption =
        match index < int count with
        | false -> ValueSome ops
        | true ->
            let entry = int64 start + int64 DirectoryCountSize + int64 index * 4L
            let relative = Binary.u32 file (int entry)
            let target = baseOffset + uint64 relative
            match Sanitize.fits file.Length entry 4, target > uint64 UInt32.MaxValue, relative with
            | false, _, _ -> ValueNone
            | _, true, _ -> ValueNone
            | _, _, 0u -> sanitizeDirectoryEntries file start count baseOffset (index + 1) ops
            | _, _, _ ->
                match sanitizeOffsetTable file (int64 target) ops with
                | ValueSome remaining -> sanitizeDirectoryEntries file start count baseOffset (index + 1) remaining
                | ValueNone -> ValueNone

    /// <summary>Deep-checks a 32-bit offset array, <c>Array32Of&lt;Offset32To&lt;OpenTypeOffsetTable&gt;&gt;.sanitize</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="offset">The array offset.</param>
    /// <param name="baseOffset">The base the offsets are relative to.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeDirectory (file: ReadOnlySpan<byte>) (offset) (baseOffset) (ops) : int voption =
        match Sanitize.fits file.Length offset DirectoryCountSize with
        | false -> ValueNone
        | true ->
            let start = int offset
            let count = Binary.u32 file start
            match Sanitize.array file.Length (offset + int64 DirectoryCountSize) count 4u ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeDirectoryEntries file start count baseOffset 0 remaining

    /// <summary>Sanitizes a version 1.1 or 2 collection body.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="major">The major version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeExtendedCollection (file: ReadOnlySpan<byte>) (major) (ops) : int voption =
        match Sanitize.fits file.Length (int64 CollectionHeaderSize) DirectoryCountSize with
        | false -> ValueNone
        | true ->
            let count = Binary.u32 file CollectionHeaderSize
            let firstEntries = int64 CollectionHeaderSize + int64 DirectoryCountSize
            match Sanitize.array file.Length firstEntries count 4u ops with
            | ValueNone -> ValueNone
            | ValueSome afterArray ->
                let extra = match major with | 2us -> int64 CollectionExtraV2 | _ -> 0L
                let tableSize = int64 DirectoryCountSize + int64 count * 4L + extra
                match Sanitize.range file.Length (int64 CollectionHeaderSize) (uint32 tableSize) afterArray with
                | ValueNone -> ValueNone
                | ValueSome remaining ->
                    let second = int64 CollectionHeaderSize + tableSize
                    sanitizeDirectory file second 0UL remaining

    /// <summary>Sanitizes a collection, <c>TTCHeader.sanitize</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeCollection (file: ReadOnlySpan<byte>) (ops) : int voption =
        match Sanitize.fits file.Length 0L CollectionHeaderSize with
        | false -> ValueNone
        | true ->
            let major = Binary.u16 file 4
            let minor = Binary.u16 file 6
            match major, minor with
            | (1us | 2us), minor when minor < 1us -> sanitizeDirectory file (int64 CollectionHeaderSize) 0UL ops
            | (1us | 2us), _ -> sanitizeExtendedCollection file major ops
            | _, _ -> ValueSome ops

    /// <summary>Finds the first <c>sfnt</c> type record and its resource count.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="typeList">The type list offset.</param>
    /// <param name="count">The number of type records.</param>
    /// <param name="index">The next record.</param>
    /// <returns>The type list, the record and the resource count, or none.</returns>
    [<TailCall>]
    let rec private findSfntType (file: ReadOnlySpan<byte>) (typeList) (count) (index) =
        match index < count with
        | false -> ValueNone
        | true ->
            let record = typeList + 2 + index * ResourceTypeRecordSize
            match Binary.tag file record with
            | Tags.Sfnt -> ValueSome (struct (typeList, record, uint32 (Binary.u16 file (record + 4)) + 1u))
            | _ -> findSfntType file typeList count (index + 1)

    /// <summary>Sanitizes a resource's data bytes after its bounds check.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="start">The resource data offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeResourceOffset (file: ReadOnlySpan<byte>) (start) (ops) : int voption =
        match Sanitize.fits file.Length start 4 with
        | false -> ValueNone
        | true ->
            let length = Binary.u32 file (int start)
            match Sanitize.range file.Length (start + int64 ResourceDataPrefix) length ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeOffsetTable file (start + int64 ResourceDataPrefix) remaining

    /// <summary>Sanitizes one resource record and its sfnt face.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="dataOffset">The resource data block offset.</param>
    /// <param name="record">The resource record offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeResource (file: ReadOnlySpan<byte>) (dataOffset) (record) (ops) : int voption =
        match Sanitize.fits file.Length (int64 record) ResourceRecordSize with
        | false -> ValueNone
        | true ->
            let relative = Binary.u24 file (record + ResourceOffsetByte)
            let target = dataOffset + uint64 relative
            match target > uint64 UInt32.MaxValue with
            | true -> ValueNone
            | false -> sanitizeResourceOffset file (int64 target) ops

    /// <summary>Sanitizes every resource in a type record's list.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="dataOffset">The resource data block offset.</param>
    /// <param name="resources">The resource list offset.</param>
    /// <param name="count">The number of resources.</param>
    /// <param name="index">The next resource.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeResourceEntries
            (file: ReadOnlySpan<byte>) (dataOffset) (resources) (count) (index) (ops) : int voption =
        match index < int count with
        | false -> ValueSome ops
        | true ->
            match sanitizeResource file dataOffset (resources + index * ResourceRecordSize) ops with
            | ValueSome remaining -> sanitizeResourceEntries file dataOffset resources count (index + 1) remaining
            | ValueNone -> ValueNone

    /// <summary>Sanitizes the resources of one type record, <c>ResourceTypeRecord.sanitize</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="typeList">The type list offset.</param>
    /// <param name="record">The type record offset.</param>
    /// <param name="dataOffset">The resource data block offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeResourceType
            (file: ReadOnlySpan<byte>) (typeList) (record) (dataOffset) (ops) : int voption =
        match Sanitize.fits file.Length (int64 record) ResourceTypeRecordSize with
        | false -> ValueNone
        | true ->
            let count = match Binary.tag file record with | Tags.Sfnt -> uint32 (Binary.u16 file (record + 4)) + 1u | _ -> 0u
            let resources = typeList + int (Binary.u16 file (record + ResourceTypeResourcesOffset))
            match Sanitize.array file.Length (int64 resources) count (uint32 ResourceRecordSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeResourceEntries file dataOffset resources count 0 remaining

    /// <summary>Sanitizes every type record in the resource map.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="typeList">The type list offset.</param>
    /// <param name="count">The number of type records.</param>
    /// <param name="dataOffset">The resource data block offset.</param>
    /// <param name="index">The next record.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeResourceTypes
            (file: ReadOnlySpan<byte>) (typeList) (count) (dataOffset) (index) (ops) : int voption =
        match index < count with
        | false -> ValueSome ops
        | true ->
            match sanitizeResourceType file typeList (typeList + 2 + index * ResourceTypeRecordSize) dataOffset ops with
            | ValueSome remaining -> sanitizeResourceTypes file typeList count dataOffset (index + 1) remaining
            | ValueNone -> ValueNone

    /// <summary>Sanitizes the resource map after the data block.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="mapOffset">The resource map offset.</param>
    /// <param name="dataOffset">The data block offset.</param>
    /// <param name="remaining">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeResourceMap (file: ReadOnlySpan<byte>) (mapOffset) (dataOffset) (remaining) : int voption =
        match Sanitize.fits file.Length (int64 mapOffset) ResourceMapSize with
        | false -> ValueNone
        | true ->
            let typeList = mapOffset + uint32 (Binary.u16 file (int mapOffset + ResourceMapTypeListOffset))
            match Sanitize.fits file.Length (int64 typeList) 2 with
            | false -> ValueNone
            | true ->
                let count = int (Binary.u16 file (int typeList)) + 1
                match Sanitize.array file.Length (int64 typeList + 2L) (uint32 count) (uint32 ResourceTypeRecordSize) remaining with
                | ValueNone -> ValueNone
                | ValueSome afterArray -> sanitizeResourceTypes file (int typeList) count (uint64 dataOffset) 0 afterArray

    /// <summary>Sanitizes a Mac resource fork, <c>ResourceForkHeader.sanitize</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeResourceFork (file: ReadOnlySpan<byte>) (ops) : int voption =
        match Sanitize.fits file.Length 0L ResourceForkHeaderSize with
        | false -> ValueNone
        | true ->
            let dataOffset = Binary.u32 file 0
            let mapOffset = Binary.u32 file 4
            let dataLength = Binary.u32 file 8
            match Sanitize.range file.Length (int64 dataOffset) dataLength ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeResourceMap file mapOffset dataOffset remaining

    /// <summary>Sanitizes an OpenType file, <c>OpenTypeFontFile.sanitize</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <returns>True when the container passes.</returns>
    let sanitize (file: ReadOnlySpan<byte>) =
        match Sanitize.fits file.Length 0L 4 with
        | false -> false
        | true ->
            let ops = Sanitize.start file.Length
            match (|Container|) (Binary.tag file 0) with
            | Sfnt -> ValueOption.isSome (sanitizeOffsetTable file 0L ops)
            | Collection -> ValueOption.isSome (sanitizeCollection file ops)
            | ResourceFork -> ValueOption.isSome (sanitizeResourceFork file ops)
            | Unrecognized -> true

    /// <summary>Returns the directory list offset of a collection.</summary>
    /// <param name="file">The file bytes.</param>
    /// <returns>The offset of the list that selects faces.</returns>
    let private selectedDirectory (file: ReadOnlySpan<byte>) =
        let major = Binary.u16 file 4
        let minor = Binary.u16 file 6
        match minor >= 1us with
        | false -> int64 CollectionHeaderSize
        | true ->
            let count = Binary.u32 file CollectionHeaderSize
            let extra = match major with | 2us -> int64 CollectionExtraV2 | _ -> 0L
            int64 CollectionHeaderSize + int64 DirectoryCountSize + int64 count * 4L + extra

    /// <summary>Returns the number of faces in a sanitized container, <c>OpenTypeFontFile.get_face_count</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <returns>The face count.</returns>
    let faceCount (file: ReadOnlySpan<byte>) =
        match (|Container|) (Binary.tag file 0) with
        | Sfnt -> 1
        | Collection ->
            match Binary.u16 file 4 with
            | 1us | 2us -> int (Binary.u32 file (int (selectedDirectory file)))
            | _ -> 0
        | ResourceFork ->
            let mapOffset = int (Binary.u32 file 4)
            let typeList = mapOffset + int (Binary.u16 file (mapOffset + ResourceMapTypeListOffset))
            let count = int (Binary.u16 file typeList) + 1
            match findSfntType file typeList count 0 with
            | ValueSome (struct (_, _, resources)) -> int resources
            | ValueNone -> 0
        | Unrecognized -> 0

    /// <summary>Reads the directory records in reverse while recursing.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="start">The directory start.</param>
    /// <param name="count">The number of records.</param>
    /// <param name="index">The next record.</param>
    /// <param name="records">The records found so far, latest first.</param>
    /// <returns>The records in file order.</returns>
    [<TailCall>]
    let rec private readRecords
            (file: ReadOnlySpan<byte>) (start) (count) (index) (records: TableRecord list) =
        match index < count with
        | false -> List.rev records
        | true ->
            let record = start + OffsetTableSize + index * TableRecordSize
            let item =
                { Tag = Binary.tag file record
                  CheckSum = Binary.u32 file (record + 4)
                  Offset = Binary.u32 file (record + 8)
                  Length = Binary.u32 file (record + 12) }
            readRecords file start count (index + 1) (item :: records)

    /// <summary>Reads a table directory at an offset.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="offset">The offset table offset.</param>
    /// <returns>The records in file order.</returns>
    let private readDirectory (file: ReadOnlySpan<byte>) (offset) : ImmutableArray<TableRecord> =
        match Sanitize.fits file.Length offset OffsetTableSize with
        | false -> ImmutableArray.Empty
        | true ->
            let start = int offset
            let count = int (Binary.u16 file (start + 4))
            Sealed.ofList (readRecords file start count 0 [])

    /// <summary>Selects a resource-fork face across every sfnt type record.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="typeList">The type list offset.</param>
    /// <param name="count">The number of type records.</param>
    /// <param name="index">The next type record.</param>
    /// <param name="selected">The face index.</param>
    /// <param name="dataOffset">The resource data block offset.</param>
    /// <returns>The base offset and directory.</returns>
    [<TailCall>]
    let rec private findResourceFace
            (file: ReadOnlySpan<byte>) (typeList) (count) (index) (selected) (dataOffset) =
        match index < count with
        | false -> struct (0u, ImmutableArray.Empty)
        | true ->
            let record = typeList + 2 + index * ResourceTypeRecordSize
            match Binary.tag file record with
            | Tags.Sfnt ->
                let resources = uint32 (Binary.u16 file (record + 4)) + 1u
                match selected < resources with
                | false -> findResourceFace file typeList count (index + 1) selected dataOffset
                | true ->
                    let resourceList = typeList + int (Binary.u16 file (record + ResourceTypeResourcesOffset))
                    let resource = resourceList + int selected * ResourceRecordSize
                    let baseOffset = dataOffset + uint64 (Binary.u24 file (resource + ResourceOffsetByte)) + uint64 ResourceDataPrefix
                    let base32 = uint32 baseOffset
                    struct (base32, readDirectory file (int64 base32))
            | _ -> findResourceFace file typeList count (index + 1) selected dataOffset

    /// <summary>Selects a face from a collection's first directory.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="list">The first offset array.</param>
    /// <param name="selected">The face index.</param>
    /// <returns>The base offset and directory.</returns>
    let private collectionFace (file: ReadOnlySpan<byte>) (list) (selected) =
        let count = Binary.u32 file (int list)
        match selected < count with
        | false -> struct (0u, ImmutableArray.Empty)
        | true ->
            let entry = int64 list + int64 DirectoryCountSize + int64 selected * 4L
            match Binary.u32 file (int entry) with
            | 0u -> struct (0u, ImmutableArray.Empty)
            | offset -> struct (0u, readDirectory file (int64 offset))

    /// <summary>Selects a face's base offset and directory, <c>OpenTypeFontFile.get_face</c>.</summary>
    /// <param name="file">The file bytes.</param>
    /// <param name="index">The face index; only the low 16 bits are used.</param>
    /// <returns>The base offset and the directory in file order.</returns>
    let face (file: ReadOnlySpan<byte>) (index) : struct (uint32 * ImmutableArray<TableRecord>) =
        let selected = index &&& 0xFFFFu
        match (|Container|) (Binary.tag file 0) with
        | Sfnt -> struct (0u, readDirectory file 0L)
        | Collection ->
            match Binary.u16 file 4 with
            | 1us | 2us -> collectionFace file (selectedDirectory file) selected
            | _ -> struct (0u, ImmutableArray.Empty)
        | ResourceFork ->
            let dataOffset = uint64 (Binary.u32 file 0)
            let mapOffset = int (Binary.u32 file 4)
            let typeList = mapOffset + int (Binary.u16 file (mapOffset + ResourceMapTypeListOffset))
            let count = int (Binary.u16 file typeList) + 1
            findResourceFace file typeList count 0 selected dataOffset
        | Unrecognized -> struct (0u, ImmutableArray.Empty)
