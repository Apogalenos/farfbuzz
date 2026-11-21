namespace Farfbuzz.OpenType

open System
open Farfbuzz
open Farfbuzz.Var

/// <summary>The COLR table after sanitising: the bytes, the header fields and the variation store, <c>OT::COLR</c>.</summary>
[<NoEquality; NoComparison>]
type internal Colr =
    { Table: ReadOnlyMemory<byte>
      Version: int
      NumBaseGlyphs: int
      BaseGlyphsAt: int
      LayersAt: int
      NumLayers: int
      BaseGlyphListAt: int  // absolute offset, 0 for the Null offset
      LayerListAt: int
      ClipListAt: int
      Store: ItemVariationStore
      IndexMap: DeltaSetIndexMap }

/// <summary>The font's variation deltas for a COLR table, <c>ItemVarStoreInstancer</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal ColrInstancer =
    { Active: bool
      Instance: StoreInstance
      Map: DeltaSetIndexMap }

/// <summary>Where a paint, a clip box or a layer is: an absolute offset, or the Null object.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal ColrNode =
    | NullNode
    | NodeAt of offset: int

/// <summary>Reading the COLR table, <c>src/OT/Color/COLR/COLR.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Colr =
    /// <summary>The size of the COLR version 0 header, <c>COLR::min_size</c>.</summary>
    [<Literal>]
    let private HeaderSize = 14

    /// <summary>The offset of the number of base glyph records.</summary>
    [<Literal>]
    let private NumBaseGlyphsOffset = 2

    /// <summary>The offset of the base glyph records offset.</summary>
    [<Literal>]
    let private BaseGlyphsOffset = 4

    /// <summary>The offset of the layer records offset.</summary>
    [<Literal>]
    let private LayersOffset = 8

    /// <summary>The offset of the number of layer records.</summary>
    [<Literal>]
    let private NumLayersOffset = 12

    /// <summary>The offset of the base glyph list offset.</summary>
    [<Literal>]
    let private BaseGlyphListOffset = 14

    /// <summary>The offset of the layer list offset.</summary>
    [<Literal>]
    let private LayerListOffset = 18

    /// <summary>The offset of the clip list offset.</summary>
    [<Literal>]
    let private ClipListOffset = 22

    /// <summary>The offset of the variation index map offset.</summary>
    [<Literal>]
    let private VarIdxMapOffset = 26

    /// <summary>The offset of the variation store offset.</summary>
    [<Literal>]
    let private VarStoreOffset = 30

    /// <summary>The size of a 32-bit offset field.</summary>
    [<Literal>]
    let private Offset32Size = 4

    /// <summary>The size of a 24-bit offset field.</summary>
    [<Literal>]
    let private Offset24Size = 3

    /// <summary>The size of one base glyph record, <c>BaseGlyphRecord::static_size</c>.</summary>
    [<Literal>]
    let BaseGlyphRecordSize = 6

    /// <summary>The size of one layer record, <c>LayerRecord::static_size</c>.</summary>
    [<Literal>]
    let LayerRecordSize = 4

    /// <summary>The size of one base glyph paint record, <c>BaseGlyphPaintRecord::static_size</c>.</summary>
    [<Literal>]
    let private BaseGlyphPaintRecordSize = 6

    /// <summary>The size of one clip record, <c>ClipRecord::static_size</c>.</summary>
    [<Literal>]
    let private ClipRecordSize = 7

    /// <summary>The size of the clip list header, <c>ClipList::min_size</c>.</summary>
    [<Literal>]
    let private ClipListHeaderSize = 5

    /// <summary>The size of a ClipBoxFormat1, <c>ClipBoxFormat1::static_size</c>.</summary>
    [<Literal>]
    let private ClipBoxFormat1Size = 9

    /// <summary>The size of a ClipBoxFormat2, <c>Variable&lt;ClipBoxFormat1&gt;::min_size</c>.</summary>
    [<Literal>]
    let private ClipBoxFormat2Size = 13

    /// <summary>The size of a ColorLine header, <c>ColorLine::min_size</c>.</summary>
    [<Literal>]
    let private ColorLineHeaderSize = 3

    /// <summary>The size of a ColorStop, <c>ColorStop::static_size</c>.</summary>
    [<Literal>]
    let private ColorStopSize = 6

    /// <summary>The size of the variation index that follows a variable value, <c>VarIdx::static_size</c>.</summary>
    [<Literal>]
    let private VarIdxSize = 4

    /// <summary>The size of an Affine2x3, <c>Affine2x3::static_size</c>.</summary>
    [<Literal>]
    let private AffineSize = 24

    /// <summary>The size of a PaintComposite, charged to the operation budget, <c>PaintComposite::min_size</c>.</summary>
    [<Literal>]
    let private PaintCompositeSize = 8

    /// <summary>The offset a paint keeps its source paint at.</summary>
    [<Literal>]
    let private SourceOffset = 1

    /// <summary>The offset a PaintTransform keeps its transform at.</summary>
    [<Literal>]
    let private TransformOffset = 4

    /// <summary>The offset a PaintComposite keeps its backdrop paint at.</summary>
    [<Literal>]
    let private BackdropOffset = 5

    /// <summary>The nesting limit of a paint sanitise, <c>HB_MAX_NESTING_LEVEL</c>.</summary>
    [<Literal>]
    let private MaxNestingLevel = 64

    /// <summary>The variation index meaning no variation, <c>VarIdx::NO_VARIATION</c>.</summary>
    [<Literal>]
    let NoVariation = 0xFFFFFFFFu

    /// <summary>The COLR of a face without usable data.</summary>
    let empty =
        { Table = ReadOnlyMemory<byte>.Empty
          Version = 0
          NumBaseGlyphs = 0
          BaseGlyphsAt = 0
          LayersAt = 0
          NumLayers = 0
          BaseGlyphListAt = 0
          LayerListAt = 0
          ClipListAt = 0
          Store = ItemVariationStore.empty
          IndexMap = DeltaSetIndexMap.identity }

    /// <summary>Whether the table has any data, <c>COLR::has_data</c>: base glyph records or a non-zero version.</summary>
    /// <param name="colr">The table.</param>
    /// <returns>True when it has data.</returns>
    let hasData (colr: Colr) =
        colr.NumBaseGlyphs <> 0 || colr.Version <> 0

    /// <summary>Reads an unsigned byte; 0 outside the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset.</param>
    /// <returns>The value.</returns>
    let u8 (table: ReadOnlyMemory<byte>) at =
        int (Binary.u8 table.Span at)

    /// <summary>Reads an unsigned 16-bit value; 0 outside the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset.</param>
    /// <returns>The value.</returns>
    let u16 (table: ReadOnlyMemory<byte>) at =
        int (Binary.u16 table.Span at)

    /// <summary>Reads a signed 16-bit value; 0 outside the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset.</param>
    /// <returns>The value.</returns>
    let i16 (table: ReadOnlyMemory<byte>) at =
        int (Binary.i16 table.Span at)

    /// <summary>Reads an unsigned 24-bit value; 0 outside the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset.</param>
    /// <returns>The value.</returns>
    let u24 (table: ReadOnlyMemory<byte>) at =
        int (Binary.u24 table.Span at)

    /// <summary>Reads an unsigned 32-bit value; 0 outside the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset.</param>
    /// <returns>The value.</returns>
    let u32 (table: ReadOnlyMemory<byte>) at =
        uint32 (Binary.u32 table.Span at)

    /// <summary>Reads a signed 32-bit value; 0 outside the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset.</param>
    /// <returns>The value.</returns>
    let i32 (table: ReadOnlyMemory<byte>) at =
        int (Binary.u32 table.Span at)

    /// <summary>Clamps an absolute offset into the int range; an offset past it reads as outside the table.</summary>
    /// <param name="value">The offset.</param>
    /// <returns>The offset as an int.</returns>
    let private toAt value =
        match value > int64 Int32.MaxValue with
        | true -> Int32.MaxValue
        | false -> int value

    /// <summary>The node a 32-bit offset field names, <c>OffsetTo::operator()</c>: zero is the Null object.</summary>
    /// <param name="table">The table.</param>
    /// <param name="fieldAt">The offset of the field.</param>
    /// <param name="baseAt">The offset the field is relative to.</param>
    /// <returns>The node.</returns>
    let node32 (table: ReadOnlyMemory<byte>) fieldAt baseAt =
        match u32 table fieldAt with
        | 0u -> NullNode
        | offset -> NodeAt (toAt (int64 baseAt + int64 offset))

    /// <summary>The node a 24-bit offset field names, <c>OffsetTo::operator()</c>: zero is the Null object.</summary>
    /// <param name="table">The table.</param>
    /// <param name="fieldAt">The offset of the field.</param>
    /// <param name="baseAt">The offset the field is relative to.</param>
    /// <returns>The node.</returns>
    let node24 (table: ReadOnlyMemory<byte>) fieldAt baseAt =
        match u24 table fieldAt with
        | 0 -> NullNode
        | offset -> NodeAt (toAt (int64 baseAt + int64 offset))

    /// <summary>Checks an array of a count of items, <c>check_array</c>, threading the operation budget.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The array offset.</param>
    /// <param name="count">The item count.</param>
    /// <param name="itemSize">The item size.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private checkArray (table: ReadOnlyMemory<byte>) at (count: uint32) (itemSize: uint32) ops =
        Sanitize.array table.Length at count itemSize ops

    /// <summary>Continues a sanitise with what is left of the operation budget when the step before it passed.</summary>
    /// <param name="next">The next step, given the remaining operations.</param>
    /// <param name="result">The result of the step before.</param>
    /// <returns>The remaining operations, or none.</returns>
    let inline private andThen ([<InlineIfLambda>] next: int -> int voption) (result: int voption) =
        match result with
        | ValueNone -> ValueNone
        | ValueSome remaining -> next remaining

    /// <summary>Sanitizes a ColorLine, <c>ColorLine::sanitize</c>: the header and the stops array.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the colour line.</param>
    /// <param name="stopSize">The size of one stop, with the variation index when the line is variable.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeColorLine (table: ReadOnlyMemory<byte>) at stopSize ops =
        match Sanitize.fits table.Length at ColorLineHeaderSize with
        | false -> ValueNone
        | true ->
            let count = uint32 (u16 table (toAt at + 1))
            Sanitize.range table.Length (at + int64 ColorLineHeaderSize) (count * uint32 stopSize) ops

    /// <summary>What a paint format holds besides its fixed fields: the children its sanitiser follows.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private PaintShape =
        | Leaf
        | Line of stopSize: int
        | Child
        | TransformChild of affineSize: int
        | CompositeChildren
        | UnknownFormat

    /// <summary>The fixed size and shape of a paint format, <c>Paint::dispatch</c>'s sanitise targets.</summary>
    /// <param name="format">The paint format.</param>
    /// <returns>The size and shape; size 0 and the unknown shape for a format upstream passes without further checks.</returns>
    let private paintShapeOf format =
        match format with
        | 1 -> struct (6, Leaf)
        | 2 -> struct (5, Leaf)
        | 3 -> struct (5 + VarIdxSize, Leaf)
        | 4 -> struct (16, Line ColorStopSize)
        | 5 -> struct (16 + VarIdxSize, Line (ColorStopSize + VarIdxSize))
        | 6 -> struct (16, Line ColorStopSize)
        | 7 -> struct (16 + VarIdxSize, Line (ColorStopSize + VarIdxSize))
        | 8 -> struct (12, Line ColorStopSize)
        | 9 -> struct (12 + VarIdxSize, Line (ColorStopSize + VarIdxSize))
        | 10 -> struct (6, Child)
        | 11 -> struct (3, Leaf)
        | 12 -> struct (7, TransformChild AffineSize)
        | 13 -> struct (7, TransformChild (AffineSize + VarIdxSize))
        | 14 -> struct (8, Child)
        | 15 -> struct (8 + VarIdxSize, Child)
        | 16 -> struct (8, Child)
        | 17 -> struct (8 + VarIdxSize, Child)
        | 18 -> struct (12, Child)
        | 19 -> struct (12 + VarIdxSize, Child)
        | 20 -> struct (6, Child)
        | 21 -> struct (6 + VarIdxSize, Child)
        | 22 -> struct (10, Child)
        | 23 -> struct (10 + VarIdxSize, Child)
        | 24 -> struct (6, Child)
        | 25 -> struct (6 + VarIdxSize, Child)
        | 26 -> struct (10, Child)
        | 27 -> struct (10 + VarIdxSize, Child)
        | 28 -> struct (8, Child)
        | 29 -> struct (8 + VarIdxSize, Child)
        | 30 -> struct (12, Child)
        | 31 -> struct (12 + VarIdxSize, Child)
        | 32 -> struct (PaintCompositeSize, CompositeChildren)
        | 33 -> struct (7, Child)
        | _ -> struct (0, UnknownFormat)

    /// <summary>Sanitizes a paint, <c>Paint::sanitize</c>: the recursion limit, the format byte, the fixed size of the
    /// format and the children it names, threading the operation budget.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the paint.</param>
    /// <param name="depth">The recursion depth so far.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let rec private sanitizePaint (table: ReadOnlyMemory<byte>) at depth ops =
        match depth >= MaxNestingLevel || not (Sanitize.fits table.Length at 1) with
        | true -> ValueNone
        | false -> sanitizePaintShape table at depth ops

    /// <summary>Sanitizes the fixed fields and the children of a paint whose format byte is in the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the paint.</param>
    /// <param name="depth">The recursion depth so far.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    and private sanitizePaintShape (table: ReadOnlyMemory<byte>) at depth ops =
        let atInt = toAt at
        let struct (size, shape) = paintShapeOf (u8 table atInt)
        match Sanitize.fits table.Length at size, shape with
        | false, _ -> ValueNone
        | true, Leaf -> ValueSome ops
        | true, UnknownFormat -> ValueSome ops
        | true, Line stopSize -> sanitizeLineOffset table at atInt stopSize ops
        | true, Child -> sanitizePaintOffset table at atInt SourceOffset depth ops
        | true, TransformChild affineSize ->
            sanitizePaintOffset table at atInt SourceOffset depth ops
            |> andThen (fun remaining -> sanitizeAffineOffset table at atInt affineSize remaining)
        | true, CompositeChildren ->
            match ops - PaintCompositeSize > 0 with
            | false -> ValueNone
            | true ->
                sanitizePaintOffset table at atInt SourceOffset depth (ops - PaintCompositeSize)
                |> andThen (fun remaining -> sanitizePaintOffset table at atInt BackdropOffset depth remaining)

    /// <summary>Sanitizes an offset to a paint, <c>OffsetTo&lt;Paint&gt;::sanitize</c>: the field, then the paint it names
    /// unless it is the Null offset.</summary>
    /// <param name="table">The table.</param>
    /// <param name="baseAt">The offset of the struct holding the field.</param>
    /// <param name="baseInt">The same offset as an int.</param>
    /// <param name="fieldOffset">The offset of the field inside the struct.</param>
    /// <param name="depth">The recursion depth of the struct.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    and private sanitizePaintOffset (table: ReadOnlyMemory<byte>) baseAt baseInt fieldOffset depth ops =
        match Sanitize.fits table.Length (baseAt + int64 fieldOffset) Offset24Size with
        | false -> ValueNone
        | true ->
            match node24 table (baseInt + fieldOffset) baseInt with
            | NullNode -> ValueSome ops
            | NodeAt target -> sanitizePaint table (int64 target) (depth + 1) ops

    /// <summary>Sanitizes the colour line a gradient paint names.</summary>
    /// <param name="table">The table.</param>
    /// <param name="baseAt">The offset of the gradient paint.</param>
    /// <param name="baseInt">The same offset as an int.</param>
    /// <param name="stopSize">The size of one colour stop.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    and private sanitizeLineOffset (table: ReadOnlyMemory<byte>) baseAt baseInt stopSize ops =
        match Sanitize.fits table.Length (baseAt + int64 SourceOffset) Offset24Size with
        | false -> ValueNone
        | true ->
            match node24 table (baseInt + SourceOffset) baseInt with
            | NullNode -> ValueSome ops
            | NodeAt target -> sanitizeColorLine table (int64 target) stopSize ops

    /// <summary>Sanitizes the affine transform a PaintTransform names.</summary>
    /// <param name="table">The table.</param>
    /// <param name="baseAt">The offset of the paint.</param>
    /// <param name="baseInt">The same offset as an int.</param>
    /// <param name="affineSize">The size of the affine with its variation index when variable.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    and private sanitizeAffineOffset (table: ReadOnlyMemory<byte>) baseAt baseInt affineSize ops =
        match Sanitize.fits table.Length (baseAt + int64 TransformOffset) Offset24Size with
        | false -> ValueNone
        | true ->
            match node24 table (baseInt + TransformOffset) baseInt with
            | NullNode -> ValueSome ops
            | NodeAt target ->
                match Sanitize.fits table.Length (int64 target) affineSize with
                | true -> ValueSome ops
                | false -> ValueNone

    /// <summary>Sanitizes a clip box, <c>ClipBox::dispatch</c>: the format byte, then the fixed size of format 1 or 2.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the box.</param>
    /// <returns>True when the box passes.</returns>
    let private sanitizeClipBox (table: ReadOnlyMemory<byte>) at =
        match Sanitize.fits table.Length at 1 with
        | false -> false
        | true ->
            match u8 table (toAt at) with
            | 1 -> Sanitize.fits table.Length at ClipBoxFormat1Size
            | 2 -> Sanitize.fits table.Length at ClipBoxFormat2Size
            | _ -> true

    /// <summary>Sanitizes the clip records one after the other, <c>ClipRecord::sanitize</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="listAt">The offset of the clip list.</param>
    /// <param name="index">The next record.</param>
    /// <param name="count">The number of records.</param>
    /// <returns>True when every record passes.</returns>
    [<TailCall>]
    let rec private sanitizeClipRecords (table: ReadOnlyMemory<byte>) listAt index count =
        match index >= count with
        | true -> true
        | false ->
            let recordAt = listAt + ClipListHeaderSize + int index * ClipRecordSize
            match node24 table (recordAt + 4) listAt with
            | NullNode -> sanitizeClipRecords table listAt (index + 1L) count
            | NodeAt target ->
                match sanitizeClipBox table (int64 target) with
                | false -> false
                | true -> sanitizeClipRecords table listAt (index + 1L) count

    /// <summary>Sanitizes a clip list, <c>ClipList::sanitize</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the list.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeClipList (table: ReadOnlyMemory<byte>) at ops =
        match Sanitize.fits table.Length at ClipListHeaderSize with
        | false -> ValueNone
        | true ->
            let count = u32 table (toAt at + 1)
            match checkArray table (at + int64 ClipListHeaderSize) count (uint32 ClipRecordSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                match sanitizeClipRecords table (toAt at) 0L (int64 count) with
                | true -> ValueSome remaining
                | false -> ValueNone

    /// <summary>Sanitizes the paint offsets of an array of records, one after the other.</summary>
    /// <param name="table">The table.</param>
    /// <param name="listAt">The offset of the list the offsets are relative to.</param>
    /// <param name="firstField">The offset of the first offset field.</param>
    /// <param name="stride">The size of one record.</param>
    /// <param name="index">The next record.</param>
    /// <param name="count">The number of records.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizePaintRecords (table: ReadOnlyMemory<byte>) listAt firstField stride index count ops =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let fieldAt = firstField + int index * stride
            match node32 table fieldAt listAt with
            | NullNode -> sanitizePaintRecords table listAt firstField stride (index + 1L) count ops
            | NodeAt target ->
                match sanitizePaint table (int64 target) 0 ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizePaintRecords table listAt firstField stride (index + 1L) count remaining

    /// <summary>Sanitizes the base glyph list, <c>BaseGlyphList::sanitize</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the list.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeBaseGlyphList (table: ReadOnlyMemory<byte>) at ops =
        match Sanitize.fits table.Length at Offset32Size with
        | false -> ValueNone
        | true ->
            let count = u32 table (toAt at)
            match checkArray table (at + int64 Offset32Size) count (uint32 BaseGlyphPaintRecordSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                sanitizePaintRecords table (toAt at) (toAt at + Offset32Size + 2) BaseGlyphPaintRecordSize 0L (int64 count) remaining

    /// <summary>Sanitizes the layer list, <c>LayerList::sanitize</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the list.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeLayerList (table: ReadOnlyMemory<byte>) at ops =
        match Sanitize.fits table.Length at Offset32Size with
        | false -> ValueNone
        | true ->
            let count = u32 table (toAt at)
            match checkArray table (at + int64 Offset32Size) count (uint32 Offset32Size) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                sanitizePaintRecords table (toAt at) (toAt at + Offset32Size) Offset32Size 0L (int64 count) remaining

    /// <summary>Sanitizes an offset to a sub-table of the header, <c>OffsetTo::sanitize</c>: the field must fit and the
    /// Null offset passes.</summary>
    /// <param name="table">The table.</param>
    /// <param name="fieldAt">The offset of the field in the header.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <param name="sanitizeTarget">Sanitizes the sub-table at an offset.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeHeaderOffset (table: ReadOnlyMemory<byte>) fieldAt ops (sanitizeTarget: ReadOnlyMemory<byte> -> int64 -> int -> int voption) =
        match Sanitize.fits table.Length (int64 fieldAt) Offset32Size with
        | false -> ValueNone
        | true ->
            match u32 table fieldAt with
            | 0u -> ValueSome ops
            | offset -> sanitizeTarget table (int64 offset) ops

    /// <summary>Sanitizes a delta set index map at an offset of the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the map.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeIndexMap (table: ReadOnlyMemory<byte>) at ops =
        match at >= int64 table.Length with
        | true -> ValueNone
        | false ->
            let result = DeltaSetIndexMap.sanitize table.Span (int at) ops
            match result with
            | ValueNone -> ValueNone
            | ValueSome remaining -> ValueSome remaining

    /// <summary>Sanitizes a variation store at an offset of the table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the store.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeStore (table: ReadOnlyMemory<byte>) at ops =
        match at >= int64 table.Length with
        | true -> ValueNone
        | false ->
            let result = ItemVariationStore.sanitize table.Span (int at) ops
            match result with
            | ValueNone -> ValueNone
            | ValueSome remaining -> ValueSome remaining

    /// <summary>Sanitizes the version 1 additions, <c>COLR::sanitize</c> after the version check.</summary>
    /// <param name="table">The table.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>True when they pass.</returns>
    let private sanitizeVersion1 (table: ReadOnlyMemory<byte>) ops =
        sanitizeHeaderOffset table BaseGlyphListOffset ops sanitizeBaseGlyphList
        |> andThen (fun remaining -> sanitizeHeaderOffset table LayerListOffset remaining sanitizeLayerList)
        |> andThen (fun remaining -> sanitizeHeaderOffset table ClipListOffset remaining sanitizeClipList)
        |> andThen (fun remaining -> sanitizeHeaderOffset table VarIdxMapOffset remaining sanitizeIndexMap)
        |> andThen (fun remaining -> sanitizeHeaderOffset table VarStoreOffset remaining sanitizeStore)
        |> ValueOption.isSome

    /// <summary>Sanitizes a COLR table, <c>COLR::sanitize</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="ops">The initial operation budget.</param>
    /// <returns>True when the table passes.</returns>
    let private sanitize (table: ReadOnlyMemory<byte>) ops =
        let baseGlyphs = int64 (u32 table BaseGlyphsOffset)
        let layers = int64 (u32 table LayersOffset)
        let numBase = uint32 (u16 table NumBaseGlyphsOffset)
        let numLayers = uint32 (u16 table NumLayersOffset)
        let afterArrays =
            checkArray table baseGlyphs numBase (uint32 BaseGlyphRecordSize) ops
            |> andThen (fun remaining -> checkArray table layers numLayers (uint32 LayerRecordSize) remaining)
        match Sanitize.fits table.Length 0L HeaderSize, afterArrays with
        | false, _ -> false
        | true, ValueNone -> false
        | true, ValueSome remaining -> u16 table 0 = 0 || sanitizeVersion1 table remaining

    /// <summary>The offset a version 1 header field names, 0 for the Null offset and for a version 0 table.</summary>
    /// <param name="table">The table.</param>
    /// <param name="fieldAt">The offset of the field.</param>
    /// <returns>The absolute offset.</returns>
    let private headerNode (table: ReadOnlyMemory<byte>) fieldAt =
        match u16 table 0 >= 1, u32 table fieldAt with
        | false, _ -> 0
        | true, 0u -> 0
        | true, offset -> toAt (int64 offset)

    /// <summary>Reads a sanitised COLR table.</summary>
    /// <param name="blob">The table bytes.</param>
    /// <returns>The table.</returns>
    let private read (blob: ReadOnlyMemory<byte>) =
        let storeAt = headerNode blob VarStoreOffset
        { Table = blob
          Version = u16 blob 0
          NumBaseGlyphs = u16 blob NumBaseGlyphsOffset
          BaseGlyphsAt = toAt (int64 (u32 blob BaseGlyphsOffset))
          LayersAt = toAt (int64 (u32 blob LayersOffset))
          NumLayers = u16 blob NumLayersOffset
          BaseGlyphListAt = headerNode blob BaseGlyphListOffset
          LayerListAt = headerNode blob LayerListOffset
          ClipListAt = headerNode blob ClipListOffset
          Store =
            match storeAt with
            | 0 -> ItemVariationStore.empty
            | at -> ItemVariationStore.read blob at
          IndexMap = DeltaSetIndexMap.read blob (headerNode blob VarIdxMapOffset) }

    /// <summary>Reads a COLR table of a face: sanitised, or the empty table, <c>hb_sanitize_context_t::reference_table&lt;COLR&gt;</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The table.</returns>
    let load (face: Face) =
        let blob = Face.referenceTable face Tags.Colr
        match blob.Length with
        | 0 -> empty
        | length when sanitize blob (Sanitize.start length) -> read blob
        | _ -> empty

    /// <summary>What kind of record a binary search runs over.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private RecordKind =
        | GlyphRecord
        | ClipRecord

    /// <summary>Compares a glyph with a record, <c>BaseGlyphRecord::cmp</c>, <c>BaseGlyphPaintRecord::cmp</c> and
    /// <c>ClipRecord::cmp</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="kind">The record kind.</param>
    /// <param name="recordAt">The offset of the record.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>Negative before the record, positive after, zero within.</returns>
    let private compareRecord (table: ReadOnlyMemory<byte>) (kind: RecordKind) recordAt glyph =
        let first = u16 table recordAt
        match kind with
        | GlyphRecord -> compare glyph first
        | ClipRecord ->
            match glyph < first with
            | true -> -1
            | false ->
                match glyph <= u16 table (recordAt + 2) with
                | true -> 0
                | false -> 1

    /// <summary>Binary searches sorted records, <c>hb_bsearch_impl</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="kind">The record kind.</param>
    /// <param name="first">The offset of the first record.</param>
    /// <param name="stride">The size of one record.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <returns>The offset of the record found, or none.</returns>
    [<TailCall>]
    let rec private bsearch (table: ReadOnlyMemory<byte>) (kind: RecordKind) first stride glyph lo hi =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) / 2u)
            let recordAt = first + mid * stride
            match compareRecord table kind recordAt glyph with
            | order when order < 0 -> bsearch table kind first stride glyph lo (mid - 1)
            | order when order > 0 -> bsearch table kind first stride glyph (mid + 1) hi
            | _ -> ValueSome recordAt

    /// <summary>Finds a glyph's base glyph record, <c>COLR::get_base_glyph_record</c>.</summary>
    /// <param name="colr">The table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The first layer index and the layer count, or none.</returns>
    let baseGlyphRecord (colr: Colr) (glyph: uint32) =
        match bsearch colr.Table GlyphRecord colr.BaseGlyphsAt BaseGlyphRecordSize (int glyph) 0 (colr.NumBaseGlyphs - 1) with
        | ValueNone -> ValueNone
        | ValueSome recordAt -> ValueSome (struct (u16 colr.Table (recordAt + 2), u16 colr.Table (recordAt + 4)))

    /// <summary>Finds a glyph's paint, <c>COLR::get_base_glyph_paint</c>: a record found with a zero offset is the Null paint.</summary>
    /// <param name="colr">The table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The paint, or none when the glyph has no record.</returns>
    let baseGlyphPaint (colr: Colr) (glyph: uint32) =
        match colr.Version >= 1 && colr.BaseGlyphListAt <> 0 with
        | false -> ValueNone
        | true ->
            let count = int (u32 colr.Table colr.BaseGlyphListAt)
            match bsearch colr.Table GlyphRecord (colr.BaseGlyphListAt + Offset32Size) BaseGlyphPaintRecordSize (int glyph) 0 (count - 1) with
            | ValueNone -> ValueNone
            | ValueSome recordAt -> ValueSome (node32 colr.Table (recordAt + 2) colr.BaseGlyphListAt)

    /// <summary>A layer's paint, <c>LayerList::get_paint</c>: an index past the list is the Null paint.</summary>
    /// <param name="colr">The table.</param>
    /// <param name="index">The layer index.</param>
    /// <returns>The paint.</returns>
    let layerPaint (colr: Colr) (index: uint32) =
        match colr.LayerListAt <> 0 && uint64 index < uint64 (u32 colr.Table colr.LayerListAt) with
        | false -> NullNode
        | true -> node32 colr.Table (colr.LayerListAt + Offset32Size + int index * Offset32Size) colr.LayerListAt

    /// <summary>The instancer of a font's coordinates, <c>ItemVarStoreInstancer</c>: all-zero coordinates apply no deltas.</summary>
    /// <param name="colr">The table.</param>
    /// <param name="coords">The font's normalized coordinates.</param>
    /// <returns>The instancer.</returns>
    let instancer (colr: Colr) (coords: NormalizedCoords) =
        match coords.HasNonZero with
        | false ->
            { Active = false
              Instance = ItemVariationStore.instantiate NormalizedCoords.empty ItemVariationStore.empty
              Map = DeltaSetIndexMap.identity }
        | true ->
            { Active = true
              Instance = ItemVariationStore.instantiate coords colr.Store
              Map = colr.IndexMap }

    /// <summary>A variation delta, <c>ItemVarStoreInstancer::get_delta&lt;float&gt;</c>.</summary>
    /// <param name="instancer">The instancer.</param>
    /// <param name="varIdx">The variation index.</param>
    /// <param name="offset">The offset added to the index.</param>
    /// <returns>The delta, 0 for no variation.</returns>
    let delta (instancer: ColrInstancer) (varIdx: uint32) offset =
        match instancer.Active && varIdx <> NoVariation with
        | false -> 0.0f
        | true ->
            let index = uint64 varIdx + uint64 offset
            match index >= uint64 NoVariation with
            | true -> 0.0f
            | false ->
                let mapped = DeltaSetIndexMap.map instancer.Map (uint32 index)
                ItemVariationStore.delta instancer.Instance mapped

    /// <summary>Adds two ints, saturating, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The first value.</param>
    /// <param name="b">The second value.</param>
    /// <returns>The sum.</returns>
    let private saturateAdd a b =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>Subtracts two ints, saturating, <c>hb_saturate_sub</c>.</summary>
    /// <param name="a">The first value.</param>
    /// <param name="b">The second value.</param>
    /// <returns>The difference.</returns>
    let private saturateSub a b =
        int (Math.Clamp(int64 a - int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>A clip box's corners, <c>ClipBoxFormat1::get_clip_box</c> and <c>ClipBoxFormat2::get_clip_box</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="at">The offset of the box.</param>
    /// <param name="instancer">The instancer.</param>
    /// <returns>The four corners, or none for a format upstream does not know.</returns>
    let private clipBoxCorners (table: ReadOnlyMemory<byte>) at (instancer: ColrInstancer) =
        let xMin = i16 table (at + 1)
        let yMin = i16 table (at + 3)
        let xMax = i16 table (at + 5)
        let yMax = i16 table (at + 7)
        match u8 table at with
        | 1 -> ValueSome (struct (xMin, yMin, xMax, yMax))
        | 2 ->
            match instancer.Active with
            | false -> ValueSome (struct (xMin, yMin, xMax, yMax))
            | true ->
                let varIdx = u32 table (at + ClipBoxFormat1Size)
                let floored index = Fixed.clampToInt32 (float (MathF.Floor (delta instancer varIdx index)))
                let ceiled index = Fixed.clampToInt32 (float (MathF.Ceiling (delta instancer varIdx index)))
                ValueSome (struct (saturateAdd xMin (floored 0), saturateAdd yMin (floored 1), saturateAdd xMax (ceiled 2), saturateAdd yMax (ceiled 3)))
        | _ -> ValueNone

    /// <summary>The extents of the clip box a clip record names, <c>ClipBox::get_extents</c>.</summary>
    /// <param name="colr">The table.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="recordAt">The offset of the clip record.</param>
    /// <returns>The extents, or none for a null box or a format upstream does not know.</returns>
    let private clipBoxOfRecord (colr: Colr) (instancer: ColrInstancer) recordAt =
        match node24 colr.Table (recordAt + 4) colr.ClipListAt with
        | NullNode -> ValueNone
        | NodeAt boxAt ->
            match clipBoxCorners colr.Table boxAt instancer with
            | ValueNone -> ValueNone
            | ValueSome (struct (xMin, yMin, xMax, yMax)) ->
                ValueSome
                    { XBearing = xMin
                      YBearing = yMax
                      Width = saturateSub xMax xMin
                      Height = saturateSub yMin yMax }

    /// <summary>A glyph's clip box in the clip list, <c>ClipList::get_extents</c>.</summary>
    /// <param name="colr">The table.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the glyph has no usable clip box.</returns>
    let private clipBoxInList (colr: Colr) (instancer: ColrInstancer) (glyph: uint32) =
        let count = int (u32 colr.Table (colr.ClipListAt + 1))
        match bsearch colr.Table ClipRecord (colr.ClipListAt + ClipListHeaderSize) ClipRecordSize (int glyph) 0 (count - 1) with
        | ValueNone -> ValueNone
        | ValueSome recordAt -> clipBoxOfRecord colr instancer recordAt

    /// <summary>A glyph's clip box, <c>COLR::get_clip</c>, in font units.</summary>
    /// <param name="colr">The table.</param>
    /// <param name="instancer">The instancer.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the glyph has no usable clip box.</returns>
    let clipBox (colr: Colr) (instancer: ColrInstancer) (glyph: uint32) =
        match colr.Version >= 1 && colr.ClipListAt <> 0 with
        | false -> ValueNone
        | true -> clipBoxInList colr instancer glyph
