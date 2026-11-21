namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Text

/// <summary>The post table's glyph names, underline metrics and the lazily sorted name index.</summary>
[<NoEquality; NoComparison>]
type internal PostAccel =
    { Table: ReadOnlyMemory<byte>
      Version: uint32
      UnderlinePosition: int16
      UnderlineThickness: int16
      Pool: int
      IndexToOffset: ImmutableArray<int>
      ByName: Lazy<ImmutableArray<uint16>> }

/// <summary>The post table: standard names, format 2 pools and name lookup.</summary>
[<RequireQualifiedAccess>]
module internal Post =
    /// <summary>The size of the post header.</summary>
    [<Literal>]
    let private HeaderSize = 32

    /// <summary>The offset of underlinePosition.</summary>
    [<Literal>]
    let private UnderlinePositionOffset = 8

    /// <summary>The offset of underlineThickness.</summary>
    [<Literal>]
    let private UnderlineThicknessOffset = 10

    /// <summary>The number of standard Macintosh names.</summary>
    [<Literal>]
    let private StandardNameCount = 258

    /// <summary>The largest number of pool entries upstream records.</summary>
    [<Literal>]
    let private MaxPoolEntries = 65535

    /// <summary>The version of a format 1 table.</summary>
    [<Literal>]
    let private Version1 = 0x00010000u

    /// <summary>The version of a format 2 table.</summary>
    [<Literal>]
    let private Version2 = 0x00020000u

    /// <summary>The version of a format 3 table.</summary>
    [<Literal>]
    let private Version3 = 0x00030000u

    /// <summary>The immutable data a name lookup needs.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private NameSource =
        { Table: ReadOnlyMemory<byte>
          Version: uint32
          Pool: int
          IndexToOffset: ImmutableArray<int>
          Count: int }

    /// <summary>Sanitizes a post table, <c>post::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops) : int voption =
        match Sanitize.fits table.Length 0L HeaderSize with
        | false -> ValueNone
        | true ->
            match Binary.u32 table 0 with
            | version when version = Version1 || version = Version3 -> ValueSome ops
            | version when version = Version2 ->
                match Sanitize.fits table.Length (int64 HeaderSize) 2 with
                | false -> ValueNone
                | true -> Sanitize.array table.Length (int64 HeaderSize + 2L) (uint32 (Binary.u16 table HeaderSize)) 2u ops
            | _ -> ValueNone

    /// <summary>The struct form of the post sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Scans the Pascal-string pool for its offsets, <c>index_to_offset</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="pool">The pool offset.</param>
    /// <param name="data">The next string offset.</param>
    /// <param name="count">The number of offsets found.</param>
    /// <param name="offsets">The offsets so far, latest first.</param>
    /// <returns>The offsets in order.</returns>
    [<TailCall>]
    let rec private scanPool (table: ReadOnlySpan<byte>) (pool) (data) (count) (offsets: int list) =
        let endIndex = table.Length
        let length = int (Binary.u8 table data)
        match count < MaxPoolEntries && data < endIndex && data + length < endIndex with
        | false -> List.rev offsets
        | true -> scanPool table pool (data + 1 + length) (count + 1) ((data - pool) :: offsets)

    /// <summary>Returns the standard name bytes at an index.</summary>
    /// <param name="index">The standard name index.</param>
    /// <returns>The name, or empty beyond the table.</returns>
    let private standardName (index) : ReadOnlyMemory<byte> =
        match index < PostMacRomanNames.names.Length with
        | true -> ReadOnlyMemory(PostMacRomanNames.names[index])
        | false -> ReadOnlyMemory<byte>.Empty

    /// <summary>Returns a version 2 pool name.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="index">The name index.</param>
    /// <returns>The name, or empty when the index is out of range.</returns>
    let private pooledName (source: NameSource) (index) =
        let poolIndex = index - StandardNameCount
        match poolIndex < source.IndexToOffset.Length with
        | false -> ReadOnlyMemory<byte>.Empty
        | true ->
            let data = source.Pool + source.IndexToOffset[poolIndex]
            let length = int (Binary.u8 source.Table.Span data)
            match length with
            | 0 -> ReadOnlyMemory<byte>.Empty
            | _ -> source.Table.Slice(data + 1, length)

    /// <summary>Returns a glyph's name bytes, <c>find_glyph_name</c>.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The name, or empty when absent.</returns>
    let private nameBytes (source: NameSource) (glyph) : ReadOnlyMemory<byte> =
        match source.Version with
        | version when version = Version1 ->
            match uint64 glyph < uint64 StandardNameCount with
            | true -> standardName (int glyph)
            | false -> ReadOnlyMemory<byte>.Empty
        | version when version = Version2 ->
            match uint64 glyph < uint64 source.Count with
            | false -> ReadOnlyMemory<byte>.Empty
            | true ->
                let index = int (Binary.u16 source.Table.Span (HeaderSize + 2 + int glyph * 2))
                match index < StandardNameCount with
                | true -> standardName index
                | false -> pooledName source index
        | _ -> ReadOnlyMemory<byte>.Empty

    /// <summary>Compares two names by length then bytes, <c>hb_array_t::cmp</c>.</summary>
    /// <param name="a">The first name.</param>
    /// <param name="b">The second name.</param>
    /// <returns>The comparison result.</returns>
    let private compareNames (a: ReadOnlyMemory<byte>) (b: ReadOnlyMemory<byte>) =
        match a.Length <> b.Length with
        | true -> a.Length - b.Length
        | false -> a.Span.SequenceCompareTo(b.Span)

    /// <summary>The introsort comparer over glyph ids.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private NameComparer =
        { Source: NameSource }
        interface IIndexComparer with
            member this.Compare (a, b) =
                compareNames (nameBytes this.Source (uint32 a)) (nameBytes this.Source (uint32 b))

    /// <summary>Compares a glyph's name with a key, as <c>cmp_key</c>.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="glyph">The glyph index.</param>
    /// <param name="key">The key bytes.</param>
    /// <returns>The comparison result.</returns>
    let private compareNameToKey (source: NameSource) (glyph) (key: ReadOnlySpan<byte>) =
        let name = nameBytes source (uint32 glyph)
        match name.Length <> key.Length with
        | true -> key.Length - name.Length
        | false -> key.SequenceCompareTo(name.Span)

    /// <summary>Binary-searches the sorted name index.</summary>
    /// <param name="source">The name source.</param>
    /// <param name="byName">The sorted glyph ids.</param>
    /// <param name="key">The key bytes.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The glyph, or none.</returns>
    [<TailCall>]
    let rec private findByName (source: NameSource) (byName: ImmutableArray<uint16>) (key: ReadOnlySpan<byte>) (lo) (hi) =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match compareNameToKey source byName[mid] key with
            | comparison when comparison < 0 -> findByName source byName key lo (mid - 1)
            | comparison when comparison > 0 -> findByName source byName key (mid + 1) hi
            | _ -> ValueSome (uint32 byName[mid])

    /// <summary>Loads the post table and its name index.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The accelerator.</returns>
    let load (face: Face) : PostAccel =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Post)
        let span = table.Span
        let version = Binary.u32 span 0
        let count, pool, offsets =
            match version with
            | value when value = Version1 -> StandardNameCount, 0, ImmutableArray.Empty
            | value when value = Version2 ->
                let glyphCount = int (Binary.u16 span HeaderSize)
                let poolOffset = HeaderSize + 2 + glyphCount * 2
                let scanned = scanPool span poolOffset poolOffset 0 []
                glyphCount, poolOffset, Sealed.ofList scanned
            | _ -> 0, 0, ImmutableArray.Empty
        let source = { Table = table; Version = version; Pool = pool; IndexToOffset = offsets; Count = count }
        { Table = table
          Version = version
          UnderlinePosition = Binary.i16 span UnderlinePositionOffset
          UnderlineThickness = Binary.i16 span UnderlineThicknessOffset
          Pool = pool
          IndexToOffset = offsets
          ByName = lazy (Sealed.sortInPlaceIntro count { Source = source }) }

    /// <summary>Returns the number of named glyphs in the table.</summary>
    /// <param name="post">The accelerator.</param>
    /// <returns>The count.</returns>
    let private glyphCountOf (post: PostAccel) =
        match post.Version with
        | version when version = Version1 -> StandardNameCount
        | version when version = Version2 -> int (Binary.u16 post.Table.Span HeaderSize)
        | _ -> 0

    /// <summary>Returns a glyph's name, <c>get_glyph_name</c>.</summary>
    /// <param name="post">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The name bytes, or none.</returns>
    let glyphName (post: PostAccel) (glyph) : ReadOnlyMemory<byte> voption =
        let source = { Table = post.Table; Version = post.Version; Pool = post.Pool; IndexToOffset = post.IndexToOffset; Count = glyphCountOf post }
        let name = nameBytes source glyph
        match name.Length with
        | 0 -> ValueNone
        | _ -> ValueSome name

    /// <summary>Looks up a glyph by name, <c>get_glyph_from_name</c>.</summary>
    /// <param name="post">The accelerator.</param>
    /// <param name="name">The name bytes.</param>
    /// <returns>The glyph, or none.</returns>
    let glyphFromName (post: PostAccel) (name: ReadOnlySpan<byte>) : uint32 voption =
        match name.Length, post.ByName.Value.Length with
        | 0, _ -> ValueNone
        | _, 0 -> ValueNone
        | _ ->
            let source =
                { Table = post.Table
                  Version = post.Version
                  Pool = post.Pool
                  IndexToOffset = post.IndexToOffset
                  Count = post.ByName.Value.Length }
            findByName source post.ByName.Value name 0 (post.ByName.Value.Length - 1)
