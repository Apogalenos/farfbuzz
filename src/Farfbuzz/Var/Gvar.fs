namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.Var

/// <summary>The gvar table: shared tuples, per-glyph offsets and the glyph variation data.</summary>
[<NoEquality; NoComparison>]
type internal Gvar =
    { AxisCount: int
      SharedTuples: ReadOnlyMemory<byte>
      GlyphCount: int
      LongOffsets: bool
      Offsets: ReadOnlyMemory<byte>
      Data: ReadOnlyMemory<byte> }

/// <summary>The gvar table and its delta application, <c>OT::gvar</c> and <c>gvar_accelerator_t</c>.</summary>
[<RequireQualifiedAccess>]
module internal Gvar =
    /// <summary>The offset of the axis count word.</summary>
    [<Literal>]
    let private AxisCountOffset = 4

    /// <summary>The offset of the shared tuple count word.</summary>
    [<Literal>]
    let private SharedTupleCountOffset = 6

    /// <summary>The offset of the shared tuple records offset.</summary>
    [<Literal>]
    let private SharedTuplesOffset = 8

    /// <summary>The header size of the 16-bit <c>gvar</c> table.</summary>
    [<Literal>]
    let private SmallHeaderSize = 20

    /// <summary>The header size of the 32-bit <c>GVAR</c> table.</summary>
    [<Literal>]
    let private WideHeaderSize = 22

    /// <summary>The offset of the flags word in the 16-bit table.</summary>
    [<Literal>]
    let private SmallFlagsOffset = 14

    /// <summary>The offset of the flags word in the 32-bit table.</summary>
    [<Literal>]
    let private WideFlagsOffset = 16

    /// <summary>The offset of the data offset in the 16-bit table.</summary>
    [<Literal>]
    let private SmallDataZOffset = 16

    /// <summary>The offset of the data offset in the 32-bit table.</summary>
    [<Literal>]
    let private WideDataZOffset = 18

    /// <summary>The long-offsets bit of the flags word.</summary>
    [<Literal>]
    let private LongOffsetFlag = 0x0001us

    /// <summary>The embedded-peak flag of a tuple index.</summary>
    [<Literal>]
    let private EmbeddedPeakFlag = 0x8000us

    /// <summary>The intermediate-region flag of a tuple index.</summary>
    [<Literal>]
    let private IntermediateRegionFlag = 0x4000us

    /// <summary>The private-point-numbers flag of a tuple index.</summary>
    [<Literal>]
    let private PrivatePointsFlag = 0x2000us

    /// <summary>The shared-point-numbers flag of the tuple count word.</summary>
    [<Literal>]
    let private SharedPointNumbersFlag = 0x8000us

    /// <summary>The tuple count mask of the tuple count word.</summary>
    [<Literal>]
    let private TupleVarCountMask = 0x0FFFus

    /// <summary>The minimum size of a tuple variation header.</summary>
    [<Literal>]
    let private TupleHeaderMinSize = 4

    /// <summary>The offset of the serialized-data offset inside a glyph's variation data.</summary>
    [<Literal>]
    let private VarDataOffset = 2

    /// <summary>The minimum size of a glyph's variation data.</summary>
    [<Literal>]
    let private GlyphVarDataMinSize = 4

    /// <summary>The number of phantom points.</summary>
    [<Literal>]
    let private PhantomCount = 4

    /// <summary>The empty gvar of a face without a usable gvar table.</summary>
    let empty =
        { AxisCount = 0
          SharedTuples = ReadOnlyMemory<byte>.Empty
          GlyphCount = 0
          LongOffsets = false
          Offsets = ReadOnlyMemory<byte>.Empty
          Data = ReadOnlyMemory<byte>.Empty }

    /// <summary>The byte size of one tuple variation header, <c>TupleVariationHeader::get_size</c>.</summary>
    /// <param name="tupleIndex">The tuple index.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <returns>The header size.</returns>
    let private tupleHeaderSize (tupleIndex: uint16) (axisCountTimes2: int) : int =
        let hasPeak = tupleIndex &&& EmbeddedPeakFlag <> 0us
        let hasIntermediate = tupleIndex &&& IntermediateRegionFlag <> 0us
        match hasPeak || hasIntermediate with
        | false -> TupleHeaderMinSize
        | true ->
            let count = (if hasPeak then 1 else 0) + (if hasIntermediate then 2 else 0)
            TupleHeaderMinSize + count * axisCountTimes2

    /// <summary>The size of the current tuple header at a byte offset.</summary>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="headerAt">The header offset.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <returns>The header size.</returns>
    let private headerSizeAt (varData: ReadOnlySpan<byte>) (headerAt: int) (axisCountTimes2: int) : int =
        tupleHeaderSize (Binary.u16 varData (headerAt + 2)) axisCountTimes2

    /// <summary>Whether a tuple header at an offset is valid, <c>tuple_iterator_t::is_valid</c>.</summary>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="headerAt">The header offset.</param>
    /// <param name="tuplesLeft">The number of tuples still to visit.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <returns>True when the header fits.</returns>
    let private headerValid (varData: ReadOnlySpan<byte>) (headerAt: int) (tuplesLeft: int) (axisCountTimes2: int) : bool =
        tuplesLeft > 0
        && headerAt + TupleHeaderMinSize <= varData.Length
        && headerAt + headerSizeAt varData headerAt axisCountTimes2 <= varData.Length

    /// <summary>Sanitizes a gvar or GVAR table, <c>gvar_GVAR::sanitize_shallow</c>.</summary>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="wide">True for the 32-bit GVAR table.</param>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (numGlyphs: int) (wide: bool) (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        let headerSize = if wide then WideHeaderSize else SmallHeaderSize
        let flagsOffset = if wide then WideFlagsOffset else SmallFlagsOffset
        match Binary.u16 table 0 = 1us && Sanitize.fits table.Length 0L headerSize with
        | false -> ValueNone
        | true ->
            let axisCount = int (Binary.u16 table AxisCountOffset)
            let sharedTupleCount = int (Binary.u16 table SharedTupleCountOffset)
            let sharedTuplesOffset = int (Binary.u32 table SharedTuplesOffset)
            let longOffsets = Binary.u16 table flagsOffset &&& LongOffsetFlag <> 0us
            let offsetWidth = if longOffsets then 4 else 2
            match Sanitize.array table.Length (int64 sharedTuplesOffset) (uint32 (axisCount * sharedTupleCount)) 2u ops with
            | ValueNone -> ValueNone
            | ValueSome afterShared ->
                Sanitize.array table.Length (int64 headerSize) (uint32 (numGlyphs + 1)) (uint32 offsetWidth) afterShared

    /// <summary>The struct form of the gvar sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type Sanitizer =
        val private NumGlyphs: int
        val private Wide: bool

        /// <summary>Creates a gvar sanitizer.</summary>
        /// <param name="numGlyphs">The face glyph count.</param>
        /// <param name="wide">True for GVAR.</param>
        new (numGlyphs, wide) = { NumGlyphs = numGlyphs; Wide = wide }

        interface ITableSanitizer with
            member this.Sanitize (table, ops) = sanitize this.NumGlyphs this.Wide table ops

    /// <summary>Parses a sanitized gvar or GVAR table.</summary>
    /// <param name="table">The sanitized table.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="wide">True for GVAR.</param>
    /// <param name="glyphCount">The accelerator's glyph count.</param>
    /// <returns>The parsed table.</returns>
    let private parse (table: ReadOnlyMemory<byte>) (numGlyphs: int) (wide: bool) (glyphCount: int) : Gvar =
        let span = table.Span
        let headerSize = if wide then WideHeaderSize else SmallHeaderSize
        let flagsOffset = if wide then WideFlagsOffset else SmallFlagsOffset
        let dataZOffset = if wide then WideDataZOffset else SmallDataZOffset
        let axisCount = int (Binary.u16 span AxisCountOffset)
        let sharedTupleCount = int (Binary.u16 span SharedTupleCountOffset)
        let sharedTuplesOffset = int (Binary.u32 span SharedTuplesOffset)
        let longOffsets = Binary.u16 span flagsOffset &&& LongOffsetFlag <> 0us
        let offsetWidth = if longOffsets then 4 else 2
        let dataZ = int (Binary.u32 span dataZOffset)
        { AxisCount = axisCount
          SharedTuples = table.Slice(sharedTuplesOffset, axisCount * sharedTupleCount * 2)
          GlyphCount = glyphCount
          LongOffsets = longOffsets
          Offsets = table.Slice(headerSize, (numGlyphs + 1) * offsetWidth)
          Data = if dataZ >= table.Length then ReadOnlyMemory<byte>.Empty else table.Slice(dataZ) }

    /// <summary>Loads the gvar table, preferring GVAR when it has data, <c>gvar_accelerator_t</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed gvar, or empty.</returns>
    let load (face: Face) : Gvar =
        let numGlyphs = face.GlyphCount
        let wideTable = Sanitize.table (Sanitizer(numGlyphs, true)) (Face.referenceTable face Tags.UpperGvar)
        match Binary.u32 wideTable.Span 0 <> 0u with
        | true -> parse wideTable numGlyphs true numGlyphs
        | false ->
            let table = Sanitize.table (Sanitizer(numGlyphs, false)) (Face.referenceTable face Tags.Gvar)
            match Binary.u32 table.Span 0 <> 0u with
            | true -> parse table numGlyphs false numGlyphs
            | false -> empty

    /// <summary>Reads one glyph's data offset from the offset array, <c>get_offset</c>.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="index">The offset index, 0 through glyph count.</param>
    /// <returns>The data offset.</returns>
    let private offsetAt (gvar: Gvar) (index: uint32) : uint32 =
        match index > uint32 gvar.GlyphCount with
        | true -> 0u
        | false ->
            match gvar.LongOffsets with
            | true -> Binary.u32 gvar.Offsets.Span (int index * 4)
            | false -> uint32 (int (Binary.u16 gvar.Offsets.Span (int index * 2)) * 2)

    /// <summary>The variation data bytes of one glyph, empty when absent or truncated, <c>get_glyph_var_data_bytes</c>.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The data bytes.</returns>
    let private glyphData (gvar: Gvar) (glyph: uint32) : ReadOnlyMemory<byte> =
        let start = offsetAt gvar glyph
        let finish = offsetAt gvar (glyph + 1u)
        match finish < start with
        | true -> ReadOnlyMemory.Empty
        | false ->
            let length = finish - start
            let dataLength = uint32 gvar.Data.Length
            let available = if start > dataLength then 0u else dataLength - start
            let actual = min length available
            match actual < uint32 GlyphVarDataMinSize with
            | true -> ReadOnlyMemory.Empty
            | false -> gvar.Data.Slice(int start, int actual)

    /// <summary>The variation data of one glyph, empty for a glyph past the glyph count or one without usable data, as
    /// <see cref="applyDeltas"/> reads it.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The data bytes.</returns>
    let glyphVarData (gvar: Gvar) (glyph: uint32) =
        match glyph >= uint32 gvar.GlyphCount with
        | true -> ReadOnlyMemory<byte>.Empty
        | false -> glyphData gvar glyph

    /// <summary>The fixed inputs of one delta application, shared by the tuple walk and its stages.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private Context =
        { VarData: ReadOnlyMemory<byte>
          Data: int
          SharedTuples: ReadOnlyMemory<byte>
          SharedIndices: ImmutableArray<uint32>
          AxisCount: int
          PhantomOnly: bool
          Points: ImmutableArray<ContourPoint>
          DeltaStart: int
          DeltaLength: int }

    /// <summary>The mutable state of the tuple walk.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private Walk =
        { Buffer: DeltaBuffer
          Flush: bool
          AnyPrivate: bool
          PrivateChecked: bool
          TuplesLeft: int
          HeaderAt: int
          DataOffset: int }

    /// <summary>Whether any remaining tuple carries private point numbers, <c>apply_deltas_to_points</c>' scan.</summary>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <param name="headerAt">The first header to scan.</param>
    /// <param name="tuplesLeft">The number of tuples to scan.</param>
    /// <returns>True when a tuple has private points.</returns>
    [<TailCall>]
    let rec private anyPrivateScan (varData: ReadOnlySpan<byte>) (axisCountTimes2: int) (headerAt: int) (tuplesLeft: int) : bool =
        match tuplesLeft = 0 with
        | true -> false
        | false ->
            let tupleIndex = Binary.u16 varData (headerAt + 2)
            match tupleIndex &&& PrivatePointsFlag <> 0us with
            | true -> true
            | false ->
                let headerSize = tupleHeaderSize tupleIndex axisCountTimes2
                let nextHeaderAt = headerAt + headerSize
                let nextLeft = tuplesLeft - 1
                match headerValid varData nextHeaderAt nextLeft axisCountTimes2 with
                | false -> false
                | true -> anyPrivateScan varData axisCountTimes2 nextHeaderAt nextLeft

    /// <summary>Fills the private-points scan result into the walk state.</summary>
    /// <param name="varData">The glyph variation data.</param>
    /// <param name="axisCountTimes2">Twice the axis count.</param>
    /// <param name="w">The walk state.</param>
    /// <returns>The updated state.</returns>
    let private ensurePrivate (varData: ReadOnlySpan<byte>) (axisCountTimes2: int) (w: Walk) : Walk =
        match w.PrivateChecked with
        | true -> w
        | false ->
            { w with
                AnyPrivate = anyPrivateScan varData axisCountTimes2 w.HeaderAt w.TuplesLeft
                PrivateChecked = true }

    /// <summary>The next index wrapping within a contour, <c>next_index</c>.</summary>
    /// <param name="i">The index.</param>
    /// <param name="start">The contour start.</param>
    /// <param name="finish">The contour's last index.</param>
    /// <returns>The next index.</returns>
    let private nextIndex (i: int) (start: int) (finish: int) : int =
        if i >= finish then start else i + 1

    /// <summary>Infers one coordinate's delta between two referenced points, <c>infer_delta</c>.</summary>
    /// <param name="pointCoord">The target point's coordinate.</param>
    /// <param name="prevCoord">The previous referenced coordinate.</param>
    /// <param name="nextCoord">The next referenced coordinate.</param>
    /// <param name="prevDelta">The previous referenced delta.</param>
    /// <param name="nextDelta">The next referenced delta.</param>
    /// <returns>The inferred delta.</returns>
    let private inferDelta (pointCoord: float32) (prevCoord: float32) (nextCoord: float32) (prevDelta: float32) (nextDelta: float32) : float32 =
        match prevCoord = nextCoord with
        | true -> (if prevDelta = nextDelta then prevDelta else 0.0f)
        | false ->
            match pointCoord <= Fixed.hbMin prevCoord nextCoord with
            | true -> (if prevCoord < nextCoord then prevDelta else nextDelta)
            | false ->
                match pointCoord >= Fixed.hbMax prevCoord nextCoord with
                | true -> (if prevCoord > nextCoord then prevDelta else nextDelta)
                | false ->
                    let r = (pointCoord - prevCoord) / (nextCoord - prevCoord)
                    prevDelta + r * (nextDelta - prevDelta)

    /// <summary>The first end point at or after an index.</summary>
    /// <param name="points">The points.</param>
    /// <param name="at">The index to scan from.</param>
    /// <param name="count">The point count.</param>
    /// <returns>The end point index, or the count.</returns>
    [<TailCall>]
    let rec private findEndPoint (points: ReadOnlySpan<ContourPoint>) (at: int) (count: int) : int =
        match at < count && not points[at].IsEndPoint with
        | true -> findEndPoint points (at + 1) count
        | false -> at

    /// <summary>Counts the referenced points of a contour.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="at">The first point.</param>
    /// <param name="finish">One past the last point.</param>
    /// <param name="acc">The count so far.</param>
    /// <returns>The referenced count.</returns>
    [<TailCall>]
    let rec private countReferenced (buffer: DeltaBuffer) (at: int) (finish: int) (acc: int) : int =
        match at = finish with
        | true -> acc
        | false -> countReferenced buffer (at + 1) finish (acc + (if buffer.Referenced[at] <> 0uy then 1 else 0))

    /// <summary>Finds a referenced point followed by an unreferenced point, scanning from an index.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="start">The contour start.</param>
    /// <param name="finish">The contour's last index.</param>
    /// <param name="j">The scan position.</param>
    /// <returns>The referenced point.</returns>
    [<TailCall>]
    let rec private findPrev (buffer: DeltaBuffer) (start: int) (finish: int) (j: int) : int =
        let i = j
        let jNext = nextIndex i start finish
        match buffer.Referenced[i] <> 0uy && buffer.Referenced[jNext] = 0uy with
        | true -> i
        | false -> findPrev buffer start finish jNext

    /// <summary>Finds an unreferenced point followed by a referenced point, scanning from an index.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="start">The contour start.</param>
    /// <param name="finish">The contour's last index.</param>
    /// <param name="j">The scan position.</param>
    /// <returns>The referenced point after the gap.</returns>
    [<TailCall>]
    let rec private findNext (buffer: DeltaBuffer) (start: int) (finish: int) (j: int) : int =
        let i = j
        let jNext = nextIndex i start finish
        match buffer.Referenced[i] = 0uy && buffer.Referenced[jNext] <> 0uy with
        | true -> jNext
        | false -> findNext buffer start finish jNext

    /// <summary>Fills the gap between two referenced points with inferred deltas.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="points">The points.</param>
    /// <param name="start">The contour start.</param>
    /// <param name="finish">The contour's last index.</param>
    /// <param name="remaining">The unreferenced points left in the contour.</param>
    /// <param name="prev">The previous referenced point.</param>
    /// <param name="next">The next referenced point.</param>
    /// <param name="i">The current gap position.</param>
    /// <returns>The buffer and the unreferenced count left.</returns>
    [<TailCall>]
    let rec private fillGap (buffer: DeltaBuffer) (points: ReadOnlySpan<ContourPoint>) (start: int) (finish: int) (remaining: int) (prev: int) (next: int) (i: int) : struct (DeltaBuffer * int) =
        let iNext = nextIndex i start finish
        match iNext = next with
        | true -> struct (buffer, remaining)
        | false ->
            let dx = inferDelta points[iNext].X points[prev].X points[next].X buffer.Dx[prev] buffer.Dx[next]
            let dy = inferDelta points[iNext].Y points[prev].Y points[next].Y buffer.Dy[prev] buffer.Dy[next]
            let buffer = DeltaBuffer.setDelta buffer iNext dx dy
            match remaining - 1 = 0 with
            | true -> struct (buffer, 0)
            | false -> fillGap buffer points start finish (remaining - 1) prev next iNext

    /// <summary>Finds and fills every gap of a contour, <c>apply_deltas_to_points</c>' IUP pass.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="points">The points.</param>
    /// <param name="start">The contour start.</param>
    /// <param name="finish">The contour's last index.</param>
    /// <param name="remaining">The unreferenced points left.</param>
    /// <param name="j">The scan position.</param>
    /// <returns>The updated buffer.</returns>
    [<TailCall>]
    let rec private fillGaps (buffer: DeltaBuffer) (points: ReadOnlySpan<ContourPoint>) (start: int) (finish: int) (remaining: int) (j: int) : DeltaBuffer =
        match remaining = 0 with
        | true -> buffer
        | false ->
            let prev = findPrev buffer start finish j
            let next = findNext buffer start finish prev
            let struct (bufferAfter, remainingAfter) = fillGap buffer points start finish remaining prev next prev
            match remainingAfter = 0 with
            | true -> bufferAfter
            | false -> fillGaps bufferAfter points start finish remainingAfter next

    /// <summary>Runs IUP over every contour, inferring deltas for unreferenced points.</summary>
    /// <param name="buffer">The buffer.</param>
    /// <param name="points">The points.</param>
    /// <param name="start">The next contour's start.</param>
    /// <param name="count">The point count.</param>
    /// <returns>The updated buffer.</returns>
    [<TailCall>]
    let rec private iupContours (buffer: DeltaBuffer) (points: ReadOnlySpan<ContourPoint>) (start: int) (count: int) : DeltaBuffer =
        let finish = findEndPoint points start count
        match finish = count with
        | true -> buffer
        | false ->
            let referenced = countReferenced buffer start (finish + 1) 0
            let unref = (finish - start + 1) - referenced
            match unref = 0 || unref > finish - start with
            | true -> iupContours buffer points (finish + 1) count
            | false -> iupContours (fillGaps buffer points start finish unref start) points (finish + 1) count

    /// <summary>The point indices of one tuple and the offset after them, its own or the glyph's shared ones.</summary>
    /// <param name="varDataEnd">The tuple's data region.</param>
    /// <param name="hasPrivate">Whether the tuple has private points.</param>
    /// <param name="at">The serialized data offset.</param>
    /// <param name="sharedIndices">The shared point indices.</param>
    /// <returns>The indices and the offset after them, or none when a private point stream is truncated.</returns>
    let private tupleIndices (varDataEnd: ReadOnlySpan<byte>) (hasPrivate: bool) (at: int) (sharedIndices: ImmutableArray<uint32>) : struct (ImmutableArray<uint32> * int) voption =
        match hasPrivate with
        | false -> ValueSome (struct (sharedIndices, at))
        | true ->
            match TupleVariation.decompilePoints varDataEnd at with
            | ValueNone -> ValueNone
            | ValueSome struct (indices, finish) -> ValueSome (struct (indices, finish))

    /// <summary>Decompiles a tuple's x then y delta streams.</summary>
    /// <param name="varDataEnd">The tuple's data region.</param>
    /// <param name="at">The serialized data offset.</param>
    /// <param name="numDeltas">The number of deltas.</param>
    /// <param name="start">The number of leading deltas left at zero.</param>
    /// <returns>The x and y deltas, or none when a stream is truncated.</returns>
    let private decompileXy (varDataEnd: ReadOnlySpan<byte>) (at: int) (numDeltas: int) (start: int) : struct (ImmutableArray<int> * ImmutableArray<int>) voption =
        match TupleVariation.decompileDeltas varDataEnd at numDeltas start with
        | ValueNone -> ValueNone
        | ValueSome struct (x, afterX) ->
            match TupleVariation.decompileDeltas varDataEnd afterX numDeltas start with
            | ValueNone -> ValueNone
            | ValueSome struct (y, _) -> ValueSome (struct (x, y))

    /// <summary>Accumulates a tuple's deltas into the per-point accumulator and runs IUP.</summary>
    /// <param name="ctx">The fixed inputs.</param>
    /// <param name="w">The walk state.</param>
    /// <param name="indices">The point indices.</param>
    /// <param name="applyToAll">Whether the tuple applies to every point.</param>
    /// <param name="numDeltas">The number of deltas.</param>
    /// <param name="xDeltas">The x deltas.</param>
    /// <param name="yDeltas">The y deltas.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <returns>The updated state.</returns>
    let private applyTupleAccumulate (ctx: Context) (w: Walk) (indices: ImmutableArray<uint32>) (applyToAll: bool) (numDeltas: int) (xDeltas: ImmutableArray<int>) (yDeltas: ImmutableArray<int>) (scalar: float32) : Walk voption =
        let buffer =
            match applyToAll with
            | true -> w.Buffer
            | false ->
                match w.Flush with
                | true -> DeltaBuffer.flush w.Buffer ctx.DeltaStart ctx.DeltaLength
                | false -> DeltaBuffer.clear w.Buffer ctx.DeltaStart ctx.DeltaLength
        let buffer =
            match applyToAll with
            | true -> DeltaBuffer.accumulate buffer scalar (xDeltas.AsSpan()) (yDeltas.AsSpan()) ctx.DeltaStart
            | false -> DeltaBuffer.accumulateIndexed buffer scalar (xDeltas.AsSpan()) (yDeltas.AsSpan()) (indices.AsSpan()) ctx.DeltaStart numDeltas
        let buffer =
            match applyToAll || ctx.PhantomOnly with
            | true -> buffer
            | false -> iupContours buffer (ctx.Points.AsSpan()) 0 ctx.Points.Length
        ValueSome { w with Buffer = buffer; Flush = true }

    /// <summary>Applies a tuple either straight into the points or into the delta accumulator.</summary>
    /// <param name="ctx">The fixed inputs.</param>
    /// <param name="w">The walk state.</param>
    /// <param name="indices">The point indices.</param>
    /// <param name="applyToAll">Whether the tuple applies to every point.</param>
    /// <param name="numDeltas">The number of deltas.</param>
    /// <param name="startDeltas">The number of leading deltas to skip.</param>
    /// <param name="xDeltas">The x deltas.</param>
    /// <param name="yDeltas">The y deltas.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <returns>The updated state.</returns>
    let private applyTupleDispatch (ctx: Context) (w: Walk) (indices: ImmutableArray<uint32>) (applyToAll: bool) (numDeltas: int) (startDeltas: int) (xDeltas: ImmutableArray<int>) (yDeltas: ImmutableArray<int>) (scalar: float32) : Walk voption =
        match applyToAll && not w.AnyPrivate with
        | true ->
            let buffer = DeltaBuffer.addScaledX w.Buffer scalar (xDeltas.AsSpan()) startDeltas
            let buffer = DeltaBuffer.addScaledY buffer scalar (yDeltas.AsSpan()) startDeltas
            ValueSome { w with Buffer = buffer }
        | false -> applyTupleAccumulate ctx w indices applyToAll numDeltas xDeltas yDeltas scalar

    /// <summary>Decompiles a tuple's deltas and dispatches to the fast or accumulating path.</summary>
    /// <param name="ctx">The fixed inputs.</param>
    /// <param name="w">The walk state.</param>
    /// <param name="indices">The point indices.</param>
    /// <param name="deltasAt">The delta stream offset.</param>
    /// <param name="varDataEnd">The tuple's data region.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <returns>The updated state, or none where the deltas are truncated.</returns>
    let private applyTupleDeltas (ctx: Context) (w: Walk) (indices: ImmutableArray<uint32>) (deltasAt: int) (varDataEnd: ReadOnlySpan<byte>) (scalar: float32) : Walk voption =
        let count = ctx.Points.Length
        let applyToAll = indices.Length = 0
        let numDeltas = if applyToAll then count else indices.Length
        let startDeltas = if applyToAll then ctx.DeltaStart else 0
        match decompileXy varDataEnd deltasAt numDeltas startDeltas with
        | ValueNone -> ValueNone
        | ValueSome struct (xDeltas, yDeltas) -> applyTupleDispatch ctx w indices applyToAll numDeltas startDeltas xDeltas yDeltas scalar

    /// <summary>Applies one non-zero tuple: decompiles its deltas and accumulates or adds them.</summary>
    /// <param name="ctx">The fixed inputs.</param>
    /// <param name="w">The walk state.</param>
    /// <param name="tupleIndex">The tuple index.</param>
    /// <param name="varDataSize">The tuple's data size.</param>
    /// <param name="serializedAt">The tuple's serialized data offset.</param>
    /// <param name="scalar">The tuple scalar.</param>
    /// <returns>The updated state, or none where the data is truncated.</returns>
    let private applyTuple (ctx: Context) (w: Walk) (tupleIndex: uint16) (varDataSize: int) (serializedAt: int) (scalar: float32) : Walk voption =
        let varData = ctx.VarData.Span
        let finish = serializedAt + varDataSize
        let varDataEnd = varData.Slice(0, finish)
        let hasPrivate = tupleIndex &&& PrivatePointsFlag <> 0us
        match tupleIndices varDataEnd hasPrivate serializedAt ctx.SharedIndices with
        | ValueNone -> ValueNone
        | ValueSome struct (indices, deltasAt) -> applyTupleDeltas ctx w indices deltasAt varDataEnd scalar

    /// <summary>Processes one tuple, returning the state to advance, or none on a failure.</summary>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="ctx">The fixed inputs.</param>
    /// <param name="w">The walk state.</param>
    /// <returns>The updated state, or none.</returns>
    let private processTuple (coords: ReadOnlySpan<int>) (ctx: Context) (w: Walk) : Walk voption =
        let varData = ctx.VarData.Span
        let tupleIndex = Binary.u16 varData (w.HeaderAt + 2)
        let varDataSize = Binary.u16 varData w.HeaderAt
        let header = { DataSize = varDataSize; TupleIndex = tupleIndex; At = w.HeaderAt }
        let scalar = TupleVariation.calculateScalar varData header coords ctx.AxisCount ctx.SharedTuples.Span
        match scalar = 0.0f with
        | true -> ValueSome w
        | false ->
            let w = ensurePrivate varData (ctx.AxisCount * 2) w
            let serializedAt = ctx.Data + w.DataOffset
            match ctx.Data = 0 || int varDataSize > varData.Length - serializedAt with
            | true -> ValueNone
            | false -> applyTuple ctx w tupleIndex (int varDataSize) serializedAt scalar

    /// <summary>Walks a glyph's tuples, applying deltas, <c>apply_deltas_to_points</c>.</summary>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="ctx">The fixed inputs.</param>
    /// <param name="w">The walk state.</param>
    /// <returns>The final buffer, or none where the data fails.</returns>
    [<TailCall>]
    let rec private walk (coords: ReadOnlySpan<int>) (ctx: Context) (w: Walk) : DeltaBuffer voption =
        match processTuple coords ctx w with
        | ValueNone -> ValueNone
        | ValueSome wAfter ->
            let varData = ctx.VarData.Span
            let tupleIndex = Binary.u16 varData (w.HeaderAt + 2)
            let varDataSize = Binary.u16 varData w.HeaderAt
            let headerSize = tupleHeaderSize tupleIndex (ctx.AxisCount * 2)
            let nextHeaderAt = w.HeaderAt + headerSize
            let nextLeft = w.TuplesLeft - 1
            match headerValid varData nextHeaderAt nextLeft (ctx.AxisCount * 2) with
            | false ->
                match wAfter.Flush with
                | true -> ValueSome (DeltaBuffer.flush wAfter.Buffer ctx.DeltaStart ctx.DeltaLength)
                | false -> ValueSome wAfter.Buffer
            | true ->
                walk coords ctx
                    { wAfter with
                        HeaderAt = nextHeaderAt
                        DataOffset = wAfter.DataOffset + int varDataSize
                        TuplesLeft = nextLeft }

    /// <summary>Builds the walk state and runs the tuple walk, then publishes the points.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="phantomOnly">Whether only the last four points receive deltas.</param>
    /// <param name="points">The points.</param>
    /// <param name="varData">The glyph's variation data.</param>
    /// <param name="data">The serialized-data offset.</param>
    /// <param name="tupleCount">The number of tuples.</param>
    /// <param name="sharedIndices">The shared point indices.</param>
    /// <param name="sharedConsumed">The bytes the shared points consume.</param>
    /// <returns>The points, or none where the data fails.</returns>
    let private applyDeltasRun (gvar: Gvar) (coords: ReadOnlySpan<int>) (phantomOnly: bool) (points: ImmutableArray<ContourPoint>) (varData: ReadOnlyMemory<byte>) (data: int) (tupleCount: int) (sharedIndices: ImmutableArray<uint32>) (sharedConsumed: int) : ImmutableArray<ContourPoint> voption =
        let count = points.Length
        let deltaStart = if phantomOnly then count - PhantomCount else 0
        let deltaLength = if phantomOnly then PhantomCount else count
        let ctx =
            { VarData = varData
              Data = data
              SharedTuples = gvar.SharedTuples
              SharedIndices = sharedIndices
              AxisCount = gvar.AxisCount
              PhantomOnly = phantomOnly
              Points = points
              DeltaStart = deltaStart
              DeltaLength = deltaLength }
        let xs = Sealed.map (fun point -> point.X) (points.AsSpan())
        let ys = Sealed.map (fun point -> point.Y) (points.AsSpan())
        let w =
            { Buffer = DeltaBuffer.create (xs.AsSpan()) (ys.AsSpan())
              Flush = false
              AnyPrivate = false
              PrivateChecked = false
              TuplesLeft = tupleCount
              HeaderAt = TupleHeaderMinSize
              DataOffset = sharedConsumed }
        match walk coords ctx w with
        | ValueNone -> ValueNone
        | ValueSome buffer ->
            let struct (finalXs, finalYs) = DeltaBuffer.publish buffer
            ValueSome (Sealed.init count (fun index ->
                { X = finalXs[index]
                  Y = finalYs[index]
                  Flag = points[index].Flag
                  IsEndPoint = points[index].IsEndPoint }))

    /// <summary>Checks the first tuple header and runs the tuple walk.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="phantomOnly">Whether only the last four points receive deltas.</param>
    /// <param name="points">The points.</param>
    /// <param name="varData">The glyph's variation data.</param>
    /// <param name="data">The serialized-data offset.</param>
    /// <param name="tupleCount">The number of tuples.</param>
    /// <param name="sharedIndices">The shared point indices.</param>
    /// <param name="sharedConsumed">The bytes the shared points consume.</param>
    /// <returns>The points, or none where the data fails.</returns>
    let private applyDeltasHeader (gvar: Gvar) (coords: ReadOnlySpan<int>) (phantomOnly: bool) (points: ImmutableArray<ContourPoint>) (varData: ReadOnlyMemory<byte>) (data: int) (tupleCount: int) (sharedIndices: ImmutableArray<uint32>) (sharedConsumed: int) : ImmutableArray<ContourPoint> voption =
        match headerValid varData.Span TupleHeaderMinSize tupleCount (gvar.AxisCount * 2) with
        | false -> ValueSome points
        | true -> applyDeltasRun gvar coords phantomOnly points varData data tupleCount sharedIndices sharedConsumed

    /// <summary>Decompiles the shared point stream at a non-null offset.</summary>
    /// <param name="varData">The glyph's variation data.</param>
    /// <param name="data">The serialized-data offset.</param>
    /// <returns>The indices and the consumed bytes, or none when the stream is truncated.</returns>
    let private decompileSharedPoints (varData: ReadOnlySpan<byte>) (data: int) : struct (ImmutableArray<uint32> * int) voption =
        match TupleVariation.decompilePoints varData data with
        | ValueNone -> ValueNone
        | ValueSome struct (indices, finish) -> ValueSome (struct (indices, finish - data))

    /// <summary>The shared point indices of a glyph's variation data and the bytes they consume.</summary>
    /// <param name="varData">The glyph's variation data.</param>
    /// <param name="tupleVarCount">The tuple count word.</param>
    /// <param name="data">The serialized-data offset.</param>
    /// <returns>The indices and the consumed bytes, or none when the stream is truncated.</returns>
    let private decompileShared (varData: ReadOnlySpan<byte>) (tupleVarCount: uint16) (data: int) : struct (ImmutableArray<uint32> * int) voption =
        match tupleVarCount &&& SharedPointNumbersFlag <> 0us with
        | false -> ValueSome (struct (ImmutableArray<uint32>.Empty, 0))
        | true ->
            match data = 0 with
            | true -> ValueSome (struct (ImmutableArray<uint32>.Empty, 0))
            | false -> decompileSharedPoints varData data

    /// <summary>Decompiles the shared point numbers and proceeds to the tuple headers.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="phantomOnly">Whether only the last four points receive deltas.</param>
    /// <param name="points">The points.</param>
    /// <param name="varData">The glyph's variation data.</param>
    /// <param name="tupleCount">The number of tuples.</param>
    /// <returns>The points, or none where the data fails.</returns>
    let private applyDeltasShared (gvar: Gvar) (coords: ReadOnlySpan<int>) (phantomOnly: bool) (points: ImmutableArray<ContourPoint>) (varData: ReadOnlyMemory<byte>) (tupleCount: int) : ImmutableArray<ContourPoint> voption =
        let span = varData.Span
        let data = int (Binary.u16 span VarDataOffset)
        match decompileShared span (Binary.u16 span 0) data with
        | ValueNone -> ValueSome points
        | ValueSome struct (sharedIndices, sharedConsumed) ->
            applyDeltasHeader gvar coords phantomOnly points varData data tupleCount sharedIndices sharedConsumed

    /// <summary>Applies deltas after the glyph has variation data, skipping an empty tuple list.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="phantomOnly">Whether only the last four points receive deltas.</param>
    /// <param name="points">The points.</param>
    /// <param name="varData">The glyph's variation data.</param>
    /// <returns>The points, or none where the data fails.</returns>
    let private applyDeltasTuples (gvar: Gvar) (coords: ReadOnlySpan<int>) (phantomOnly: bool) (points: ImmutableArray<ContourPoint>) (varData: ReadOnlyMemory<byte>) : ImmutableArray<ContourPoint> voption =
        let span = varData.Span
        let tupleCount = int (Binary.u16 span 0 &&& TupleVarCountMask)
        match tupleCount = 0 with
        | true -> ValueSome points
        | false -> applyDeltasShared gvar coords phantomOnly points varData tupleCount

    /// <summary>Applies a glyph's gvar deltas to its points, <c>apply_deltas_to_points</c>.</summary>
    /// <param name="gvar">The table.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <param name="phantomOnly">Whether only the last four points receive deltas.</param>
    /// <param name="points">The points, phantoms last.</param>
    /// <returns>The points with deltas applied, unchanged where there is no data, or none where the data fails.</returns>
    let applyDeltas (gvar: Gvar) (coords: ReadOnlySpan<int>) (glyph: uint32) (phantomOnly: bool) (points: ImmutableArray<ContourPoint>) : ImmutableArray<ContourPoint> voption =
        match glyph >= uint32 gvar.GlyphCount with
        | true -> ValueSome points
        | false ->
            let varData = glyphData gvar glyph
            match varData.Length < GlyphVarDataMinSize with
            | true -> ValueSome points
            | false -> applyDeltasTuples gvar coords phantomOnly points varData
