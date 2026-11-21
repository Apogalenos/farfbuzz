namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Var

/// <summary>One CFF2 private DICT: its local subroutine INDEX, the private dict's offset and its data set number.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal Cff2PrivateDict =
    { LocalSubrs: CffIndex
      SubrsBase: int
      Ivs: int }

/// <summary>The CFF2 FDSelect: a per-glyph font-dict index in format 0, 3 or 4.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal Cff2FdSelect =
    | FdSelect0 of fdsAt: int
    | FdSelect3 of rangesAt: int * rangeCount: int
    | FdSelect4 of rangesAt: int * rangeCount: int

/// <summary>The CFF2 accelerator: the charstrings, the subroutines, the font dicts and the variation store.</summary>
[<NoEquality; NoComparison>]
type internal Cff2Accel =
    { Valid: bool
      Table: ReadOnlyMemory<byte>
      NumGlyphs: int
      GlobalSubrs: CffIndex
      CharStrings: CffIndex
      FdSelect: Cff2FdSelect voption
      PrivateDicts: ImmutableArray<Cff2PrivateDict>
      Store: ItemVariationStore
      HasStore: bool }

/// <summary>The CFF2 table: header, dictionaries and accelerator, <c>OT::cff2</c>.</summary>
[<RequireQualifiedAccess>]
module internal Cff2 =
    /// <summary>The CFF2 header size.</summary>
    [<Literal>]
    let private HeaderSize = 5

    /// <summary>The CFF2 INDEX count field size.</summary>
    [<Literal>]
    let private CountSize = 4

    /// <summary>The variation store's 16-bit size field width.</summary>
    [<Literal>]
    let private VarStoreSizeField = 2

    /// <summary>The top dict values the accelerator needs.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private TopDict =
        { CharStrings: int
          FdArray: int
          FdSelect: int
          Vstore: int }

    /// <summary>The Null subroutine INDEX of an absent Subrs operator or a zero offset.</summary>
    let private emptyIndex = { Start = 0; Count = 0u; OffSize = 0; CountSize = CountSize }

    /// <summary>Sanitizes the CFF2 header, <c>cff2::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        match Sanitize.fits table.Length 0L HeaderSize with
        | true when Binary.u8 table 0 = 2uy -> ValueSome ops
        | _ -> ValueNone

    /// <summary>The struct form of the CFF2 sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Reads an operand counted from the end of an operator's operands.</summary>
    /// <param name="fromEnd">Zero for the last operand.</param>
    /// <param name="item">The operator.</param>
    /// <returns>The operand, or none.</returns>
    let private operandAtEnd (fromEnd: int) (item: DictOp) : double voption =
        let index = item.Operands.Length - 1 - fromEnd
        match index >= 0 with
        | true -> ValueSome item.Operands[index]
        | false -> ValueNone

    /// <summary>Reads an operand as a CFF integer, rejecting values outside the int range.</summary>
    /// <param name="fromEnd">Zero for the last operand.</param>
    /// <param name="item">The operator.</param>
    /// <returns>The integer operand, or none.</returns>
    let private intOperand (fromEnd: int) (item: DictOp) : int voption =
        match operandAtEnd fromEnd item with
        | ValueSome value when value >= float Int32.MinValue && value <= float Int32.MaxValue ->
            ValueSome (int value)
        | ValueSome _ -> ValueNone
        | ValueNone -> ValueNone

    /// <summary>Reads an operand as a CFF unsigned integer, rejecting negative or out-of-range values.</summary>
    /// <param name="fromEnd">Zero for the last operand.</param>
    /// <param name="item">The operator.</param>
    /// <returns>The unsigned operand, or none.</returns>
    let private uintOperand (fromEnd: int) (item: DictOp) : uint32 voption =
        match intOperand fromEnd item with
        | ValueSome value when value >= 0 -> ValueSome (uint32 value)
        | ValueSome _ -> ValueNone
        | ValueNone -> ValueNone

    /// <summary>Collects the top dict values from its interpreted operators.</summary>
    /// <param name="ops">The interpreted operators.</param>
    /// <param name="index">The next operator.</param>
    /// <param name="top">The values collected so far.</param>
    /// <returns>The values, or none when an operator is invalid.</returns>
    [<TailCall>]
    let rec private topValues (ops: ImmutableArray<DictOp>) (index: int) (top: TopDict) : TopDict voption =
        match index >= ops.Length with
        | true -> ValueSome top
        | false ->
            let item = ops[index]
            match item.Op with
            | 17 ->
                match intOperand 0 item with
                | ValueSome value -> topValues ops (index + 1) { top with CharStrings = value }
                | ValueNone -> ValueNone
            | 24 ->
                match intOperand 0 item with
                | ValueSome value -> topValues ops (index + 1) { top with Vstore = value }
                | ValueNone -> ValueNone
            | 1236 ->
                match intOperand 0 item with
                | ValueSome value -> topValues ops (index + 1) { top with FdArray = value }
                | ValueNone -> ValueNone
            | 1237 ->
                match intOperand 0 item with
                | ValueSome value -> topValues ops (index + 1) { top with FdSelect = value }
                | ValueNone -> ValueNone
            | _ -> topValues ops (index + 1) top

    /// <summary>Reads the top dict values from its interpreted operators.</summary>
    /// <param name="ops">The interpreted operators.</param>
    /// <returns>The values, or none when an operator is invalid.</returns>
    let private topDictOf (ops: ImmutableArray<DictOp>) : TopDict voption =
        topValues ops 0
            { CharStrings = 0
              FdArray = 0
              FdSelect = 0
              Vstore = 0 }

    /// <summary>Reads a GID of a format-3 or 4 FDSelect range or sentinel.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="at">The GID offset.</param>
    /// <param name="gidSize">2 for format 3, 4 for format 4.</param>
    /// <returns>The glyph id.</returns>
    let private readGid (span: ReadOnlySpan<byte>) (at: int) (gidSize: int) : int =
        match gidSize with
        | 4 -> int (Binary.u32 span at)
        | _ -> int (Binary.u16 span at)

    /// <summary>Reads a font-dict index of a format-3 or 4 FDSelect range.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="at">The font-dict offset.</param>
    /// <param name="gidSize">2 for format 3, 4 for format 4.</param>
    /// <returns>The font-dict index.</returns>
    let private readFd (span: ReadOnlySpan<byte>) (at: int) (gidSize: int) : int =
        match gidSize with
        | 4 -> int (Binary.u16 span at)
        | _ -> int (Binary.u8 span at)

    /// <summary>Walks a format-3 or 4 FDSelect's ranges, checking order and bounds.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="rangesAt">The first range.</param>
    /// <param name="gidSize">2 for format 3, 4 for format 4.</param>
    /// <param name="rangeSize">3 for format 3, 6 for format 4.</param>
    /// <param name="index">The next range.</param>
    /// <param name="rangeCount">The range count.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="previous">The previous range's first glyph.</param>
    /// <returns>True when every range is ordered and in bounds.</returns>
    [<TailCall>]
    let rec private sanitizeRanges
            (span: ReadOnlySpan<byte>) (rangesAt: int) (gidSize: int) (rangeSize: int)
            (index: int) (rangeCount: int) (fdCount: int) (glyphCount: int) (previous: int) : bool =
        match index >= rangeCount with
        | true -> true
        | false ->
            let first = readGid span (rangesAt + index * rangeSize) gidSize
            let fd = readFd span (rangesAt + index * rangeSize + gidSize) gidSize
            match first < glyphCount && fd < fdCount && first > previous with
            | false -> false
            | true -> sanitizeRanges span rangesAt gidSize rangeSize (index + 1) rangeCount fdCount glyphCount first

    /// <summary>Sanitizes a format-3 or 4 FDSelect's sentinel and ranges, <c>FDSelect3_4::sanitize</c>'s tail.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="rangesAt">The first range.</param>
    /// <param name="gidSize">2 for format 3, 4 for format 4.</param>
    /// <param name="rangeSize">3 for format 3, 6 for format 4.</param>
    /// <param name="rangeCount">The range count.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeFdSelectTail (span: ReadOnlySpan<byte>) (rangesAt: int) (gidSize: int) (rangeSize: int) (rangeCount: int) (fdCount: int) (glyphCount: int) (ops: int) : int voption =
        let sentinelAt = rangesAt + rangeCount * rangeSize
        match Sanitize.fits span.Length (int64 sentinelAt) gidSize with
        | false -> ValueNone
        | true ->
            let firstRange = readGid span rangesAt gidSize
            let sentinel = readGid span sentinelAt gidSize
            let ordered = sanitizeRanges span rangesAt gidSize rangeSize 0 rangeCount fdCount glyphCount -1
            match rangeCount > 0 && firstRange = 0 && sentinel = glyphCount && sentinel > readGid span (rangesAt + (rangeCount - 1) * rangeSize) gidSize && ordered with
            | true -> ValueSome ops
            | false -> ValueNone

    /// <summary>Sanitizes a format-3 or 4 FDSelect, <c>FDSelect3_4::sanitize</c>.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="offset">The FDSelect offset.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeFdSelectRanges (span: ReadOnlySpan<byte>) (offset: int) (fdCount: int) (glyphCount: int) (ops: int) : int voption =
        let format = Binary.u8 span offset
        let gidSize = match format with | 4uy -> 4 | _ -> 2
        let rangeSize = match format with | 4uy -> 6 | _ -> 3
        match Sanitize.fits span.Length (int64 offset + 1L) gidSize with
        | false -> ValueNone
        | true ->
            let rangeCount = readGid span (offset + 1) gidSize
            let rangesAt = offset + 1 + gidSize
            match Sanitize.array span.Length (int64 rangesAt) (uint32 rangeCount) (uint32 rangeSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeFdSelectTail span rangesAt gidSize rangeSize rangeCount fdCount glyphCount remaining

    /// <summary>Sanitizes an FDSelect, <c>CFF2FDSelect::sanitize</c>.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="offset">The FDSelect offset.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <param name="glyphCount">The glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeFdSelect (span: ReadOnlySpan<byte>) (offset: int) (fdCount: int) (glyphCount: int) (ops: int) : int voption =
        match Sanitize.fits span.Length (int64 offset) 1 with
        | false -> ValueNone
        | true ->
            match Binary.u8 span offset with
            | 0uy -> Sanitize.array span.Length (int64 offset + 1L) (uint32 glyphCount) 1u ops
            | 3uy | 4uy -> sanitizeFdSelectRanges span offset fdCount glyphCount ops
            | _ -> ValueNone

    /// <summary>Reads an FDSelect, <c>CFF2FDSelect::get_fd</c>'s format dispatch.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="offset">The FDSelect offset.</param>
    /// <returns>The FDSelect.</returns>
    let private readFdSelect (span: ReadOnlySpan<byte>) (offset: int) : Cff2FdSelect =
        match Binary.u8 span offset with
        | 0uy -> FdSelect0 (offset + 1)
        | 3uy -> FdSelect3 (offset + 3, int (Binary.u16 span (offset + 1)))
        | _ -> FdSelect4 (offset + 5, int (Binary.u32 span (offset + 1)))

    /// <summary>Binary-searches the format-3 or 4 FDSelect ranges, <c>FDSelect3_4::get_fd</c>.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="rangesAt">The first range.</param>
    /// <param name="gidSize">2 for format 3, 4 for format 4.</param>
    /// <param name="rangeSize">3 for format 3, 6 for format 4.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="lo">The first candidate.</param>
    /// <param name="hi">The last candidate.</param>
    /// <param name="lastFd">The last range's font dict.</param>
    /// <returns>The glyph's font-dict index.</returns>
    [<TailCall>]
    let rec private fdRange
            (span: ReadOnlySpan<byte>) (rangesAt: int) (gidSize: int) (rangeSize: int)
            (glyph: uint32) (lo: int) (hi: int) (lastFd: int) : int =
        match lo > hi with
        | true -> lastFd
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            let first = uint32 (readGid span (rangesAt + mid * rangeSize) gidSize)
            let next = uint32 (readGid span (rangesAt + (mid + 1) * rangeSize) gidSize)
            match glyph < first, glyph < next with
            | true, _ -> fdRange span rangesAt gidSize rangeSize glyph lo (mid - 1) lastFd
            | false, true -> readFd span (rangesAt + mid * rangeSize + gidSize) gidSize
            | false, false -> fdRange span rangesAt gidSize rangeSize glyph (mid + 1) hi lastFd

    /// <summary>Returns a glyph's font-dict index, <c>CFF2FDSelect::get_fd</c>; no FDSelect is font dict 0.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The font-dict index.</returns>
    let fdOf (cff: Cff2Accel) (glyph: uint32) : int =
        match cff.FdSelect with
        | ValueNone -> 0
        | ValueSome (FdSelect0 fdsAt) -> int (Binary.u8 cff.Table.Span (fdsAt + int glyph))
        | ValueSome (FdSelect3 (rangesAt, rangeCount)) ->
            let span = cff.Table.Span
            let lastFd = readFd span (rangesAt + (rangeCount - 1) * 3 + 2) 2
            fdRange span rangesAt 2 3 glyph 0 (rangeCount - 2) lastFd
        | ValueSome (FdSelect4 (rangesAt, rangeCount)) ->
            let span = cff.Table.Span
            let lastFd = readFd span (rangesAt + (rangeCount - 1) * 6 + 4) 4
            fdRange span rangesAt 4 6 glyph 0 (rangeCount - 2) lastFd

    /// <summary>Reads the Private operator's offset and size of an interpreted font dict.</summary>
    /// <param name="ops">The interpreted operators.</param>
    /// <param name="index">The next operator.</param>
    /// <returns>The private offset and size, zeros when there is no Private operator.</returns>
    [<TailCall>]
    let rec private fontDictPrivate (ops: ImmutableArray<DictOp>) (index: int) : struct (uint32 * uint32) voption =
        match index >= ops.Length with
        | true -> ValueSome (struct (0u, 0u))
        | false ->
            match ops[index].Op with
            | 18 ->
                match uintOperand 0 ops[index], uintOperand 1 ops[index] with
                | ValueSome offset, ValueSome size -> ValueSome (struct (offset, size))
                | _ -> ValueNone
            | _ -> fontDictPrivate ops (index + 1)

    /// <summary>Reads a private dict's vsindex operator's data set number, the first one only.</summary>
    /// <param name="seenVsindex">Whether a vsindex was already seen.</param>
    /// <param name="item">The operator.</param>
    /// <param name="subrs">The Subrs offset so far.</param>
    /// <param name="ivs">The data set number so far.</param>
    /// <returns>The Subrs offset and data set number, or none when the operand is invalid.</returns>
    let private vsindexValues (seenVsindex: bool) (item: DictOp) (subrs: int) (ivs: int) : struct (int * int) voption =
        match seenVsindex with
        | true -> ValueSome (struct (subrs, ivs))
        | false ->
            match uintOperand 0 item with
            | ValueSome value -> ValueSome (struct (subrs, int value))
            | ValueNone -> ValueNone

    /// <summary>Collects a private dict's Subrs offset and data set number from its interpreted operators.</summary>
    /// <param name="ops">The interpreted operators.</param>
    /// <param name="index">The next operator.</param>
    /// <param name="subrs">The Subrs offset so far.</param>
    /// <param name="ivs">The data set number so far.</param>
    /// <param name="seenVsindex">Whether a vsindex was already seen.</param>
    /// <returns>The Subrs offset and data set number, or none when an operator is invalid.</returns>
    [<TailCall>]
    let rec private privateValues (ops: ImmutableArray<DictOp>) (index: int) (subrs: int) (ivs: int) (seenVsindex: bool) : struct (int * int) voption =
        match index >= ops.Length with
        | true -> ValueSome (struct (subrs, ivs))
        | false ->
            let item = ops[index]
            match item.Op with
            | 19 ->
                match intOperand 0 item with
                | ValueSome value -> privateValues ops (index + 1) value ivs seenVsindex
                | ValueNone -> ValueNone
            | 22 ->
                match vsindexValues seenVsindex item subrs ivs with
                | ValueSome (struct (s, i)) -> privateValues ops (index + 1) s i true
                | ValueNone -> ValueNone
            | _ -> privateValues ops (index + 1) subrs ivs seenVsindex

    /// <summary>Reads a private dict's local subroutine INDEX at its Subrs offset, a broken INDEX is empty.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="privateOffset">The private dict offset.</param>
    /// <param name="subrsOffset">The Subrs offset, relative to the private dict.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The local subroutine INDEX.</returns>
    let private localSubrsAt (span: ReadOnlySpan<byte>) (privateOffset: int) (subrsOffset: int) (ops: int) : CffIndex =
        match subrsOffset with
        | 0 -> emptyIndex
        | _ ->
            let at = privateOffset + subrsOffset
            match CffIndex.sanitize span at CountSize ops with
            | ValueSome _ -> CffIndex.read span at CountSize
            | ValueNone -> emptyIndex

    /// <summary>Reads a private dict's values and local subroutines from its interpreted operators.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="privateOffset">The private dict offset.</param>
    /// <param name="privateOps">The interpreted operators.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The private dict and the remaining operations, or none.</returns>
    let private privateDictFromOps (span: ReadOnlySpan<byte>) (privateOffset: int) (privateOps: ImmutableArray<DictOp>) (ops: int) : struct (Cff2PrivateDict * int) voption =
        match privateValues privateOps 0 0 0 false with
        | ValueNone -> ValueNone
        | ValueSome (struct (subrsOffset, ivs)) ->
            ValueSome (struct ({ LocalSubrs = localSubrsAt span privateOffset subrsOffset ops; SubrsBase = privateOffset; Ivs = ivs }, ops))

    /// <summary>Reads a private dict of a given size at an offset.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="privateOffset">The private dict offset.</param>
    /// <param name="privateSize">The private dict size.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The private dict and the remaining operations, or none.</returns>
    let private privateDictFromSize (span: ReadOnlySpan<byte>) (privateOffset: int) (privateSize: uint32) (ops: int) : struct (Cff2PrivateDict * int) voption =
        match privateSize with
        | 0u -> ValueSome (struct ({ LocalSubrs = emptyIndex; SubrsBase = privateOffset; Ivs = 0 }, ops))
        | size ->
            match int64 privateOffset + int64 size <= int64 span.Length with
            | false -> ValueNone
            | true ->
                match CffDict.interpret (span.Slice(privateOffset, int size)) with
                | ValueNone -> ValueNone
                | ValueSome privateOps -> privateDictFromOps span privateOffset privateOps ops

    /// <summary>Reads one font dict's private dict values and its local subroutine INDEX.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="index">The font-dict index.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The private dict and the remaining operations, or none.</returns>
    let private privateDictOf (span: ReadOnlySpan<byte>) (fdArray: CffIndex) (index: int) (ops: int) : struct (Cff2PrivateDict * int) voption =
        let struct (fontOffset, fontLength) = CffIndex.item span fdArray (uint32 index)
        match fontLength with
        | 0 -> ValueNone
        | _ ->
            match CffDict.interpret (span.Slice(fontOffset, fontLength)) with
            | ValueNone -> ValueNone
            | ValueSome fontOps ->
                match fontDictPrivate fontOps 0 with
                | ValueNone -> ValueNone
                | ValueSome (struct (privateOffset, privateSize)) -> privateDictFromSize span (int privateOffset) privateSize ops

    /// <summary>Validates every font dict and its private dict.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="index">The next font dict.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private validatePrivateDicts (span: ReadOnlySpan<byte>) (fdArray: CffIndex) (index: int) (fdCount: int) (ops: int) : int voption =
        match index >= fdCount with
        | true -> ValueSome ops
        | false ->
            match privateDictOf span fdArray index ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (_, remaining)) -> validatePrivateDicts span fdArray (index + 1) fdCount remaining

    /// <summary>Unfolds the private dict of every font dict after validation succeeded.</summary>
    [<Struct>]
    type private PrivateDictUnfolder =
        val private fdArray: CffIndex

        /// <summary>Creates a private-dict unfolder.</summary>
        /// <param name="fdArray">The FDArray INDEX.</param>
        new (fdArray) = { fdArray = fdArray }

        interface ISpanUnfolder<byte, int, Cff2PrivateDict> with
            member this.Next (source, index) =
                match privateDictOf source this.fdArray index (Sanitize.start source.Length) with
                | ValueSome (struct (dict, _)) -> struct (dict, index + 1)
                | ValueNone -> struct ({ LocalSubrs = emptyIndex; SubrsBase = 0; Ivs = 0 }, index + 1)

    /// <summary>Builds the private dict of every font dict, after validation succeeded.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <returns>The private dicts.</returns>
    let private buildPrivateDicts (span: ReadOnlySpan<byte>) (fdArray: CffIndex) (fdCount: int) : ImmutableArray<Cff2PrivateDict> =
        Sealed.unfoldSpan span fdCount 0 (PrivateDictUnfolder(fdArray))

    /// <summary>Reads a variation store after its size field and the store sanitized.</summary>
    /// <param name="table">The table memory.</param>
    /// <param name="storeAt">The store offset.</param>
    /// <param name="size">The store size.</param>
    /// <param name="after">The remaining operations.</param>
    /// <returns>The store, whether it has data, and the remaining operations.</returns>
    let private readVarStoreAfter (table: ReadOnlyMemory<byte>) (storeAt: int) (size: int) (after: int) : struct (ItemVariationStore * bool * int) =
        match size with
        | 0 -> struct (ItemVariationStore.empty, false, after)
        | _ -> struct (ItemVariationStore.read table storeAt, true, after)

    /// <summary>Reads the variation store behind its 16-bit size field, <c>CFF2ItemVariationStore</c>.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="table">The table memory.</param>
    /// <param name="offset">The store offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The store, whether it has data, and the remaining operations, or none.</returns>
    let private readVarStore (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (offset: int) (ops: int) : struct (ItemVariationStore * bool * int) voption =
        match Sanitize.fits span.Length (int64 offset) VarStoreSizeField with
        | false -> ValueNone
        | true ->
            let size = int (Binary.u16 span offset)
            let storeAt = offset + VarStoreSizeField
            match Sanitize.range span.Length (int64 storeAt) (uint32 size) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining ->
                match ItemVariationStore.sanitize span storeAt remaining with
                | ValueNone -> ValueNone
                | ValueSome after -> ValueSome (readVarStoreAfter table storeAt size after)

    /// <summary>Builds the accelerator once the FDSelect and the private dicts have been resolved.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="table">The table memory.</param>
    /// <param name="top">The top dict values.</param>
    /// <param name="charStrings">The CharStrings INDEX.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <param name="numGlyphs">The glyph count.</param>
    /// <param name="fdSelect">The FDSelect, or none.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private buildAccel
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (charStrings: CffIndex)
            (globalSubrs: CffIndex) (fdArray: CffIndex) (fdCount: int) (numGlyphs: int)
            (fdSelect: Cff2FdSelect voption) (remaining: int) (failure: Cff2Accel) : Cff2Accel =
        let struct (store, hasStore) =
            match top.Vstore with
            | 0 -> struct (ItemVariationStore.empty, false)
            | _ ->
                match readVarStore span table top.Vstore remaining with
                | ValueSome (struct (s, h, _)) -> struct (s, h)
                | ValueNone -> struct (ItemVariationStore.empty, false)
        match validatePrivateDicts span fdArray 0 fdCount remaining with
        | ValueNone -> failure
        | ValueSome _ ->
            { Valid = true
              Table = table
              NumGlyphs = numGlyphs
              GlobalSubrs = globalSubrs
              CharStrings = charStrings
              FdSelect = fdSelect
              PrivateDicts = buildPrivateDicts span fdArray fdCount
              Store = store
              HasStore = hasStore }

    /// <summary>Finishes loading after the top dict and the INDEXes have been read.</summary>
    /// <param name="span">The table bytes.</param>
    /// <param name="table">The table memory.</param>
    /// <param name="top">The top dict values.</param>
    /// <param name="charStrings">The CharStrings INDEX.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="fdArray">The FDArray INDEX.</param>
    /// <param name="fdCount">The font-dict count.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private finishLoad
            (span: ReadOnlySpan<byte>) (table: ReadOnlyMemory<byte>) (top: TopDict) (charStrings: CffIndex)
            (globalSubrs: CffIndex) (fdArray: CffIndex) (fdCount: int) (remaining: int) (face: Face) (failure: Cff2Accel) : Cff2Accel =
        let numGlyphs = int charStrings.Count
        match numGlyphs = face.GlyphCount || (numGlyphs < face.GlyphCount && (Face.referenceTable face Tags.Varc).Length > 0) with
        | false -> failure
        | true ->
            match top.FdSelect with
            | 0 -> buildAccel span table top charStrings globalSubrs fdArray fdCount numGlyphs ValueNone remaining failure
            | fdSelectOffset ->
                match sanitizeFdSelect span fdSelectOffset fdCount numGlyphs remaining with
                | ValueNone -> failure
                | ValueSome after ->
                    buildAccel span table top charStrings globalSubrs fdArray fdCount numGlyphs (ValueSome (readFdSelect span fdSelectOffset)) after failure

    /// <summary>Reads the FDArray INDEX and finishes loading.</summary>
    /// <param name="table">The table memory.</param>
    /// <param name="span">The table bytes.</param>
    /// <param name="top">The top dict values.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="charStrings">The CharStrings INDEX.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private loadFdArray (table: ReadOnlyMemory<byte>) (span: ReadOnlySpan<byte>) (top: TopDict) (globalSubrs: CffIndex) (charStrings: CffIndex) (remaining: int) (face: Face) (failure: Cff2Accel) : Cff2Accel =
        match CffIndex.sanitize span top.FdArray CountSize remaining with
        | ValueNone -> failure
        | ValueSome remaining ->
            let fdArray = CffIndex.read span top.FdArray CountSize
            finishLoad span table top charStrings globalSubrs fdArray (int fdArray.Count) remaining face failure

    /// <summary>Reads the CharStrings INDEX.</summary>
    /// <param name="table">The table memory.</param>
    /// <param name="span">The table bytes.</param>
    /// <param name="top">The top dict values.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="remaining">The remaining sanitizer operations.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private loadCharStrings (table: ReadOnlyMemory<byte>) (span: ReadOnlySpan<byte>) (top: TopDict) (globalSubrs: CffIndex) (remaining: int) (face: Face) (failure: Cff2Accel) : Cff2Accel =
        match CffIndex.sanitize span top.CharStrings CountSize remaining with
        | ValueNone -> failure
        | ValueSome remaining -> loadFdArray table span top globalSubrs (CffIndex.read span top.CharStrings CountSize) remaining face failure

    /// <summary>Reads the global subroutine INDEX right after the top dict.</summary>
    /// <param name="table">The table memory.</param>
    /// <param name="span">The table bytes.</param>
    /// <param name="topDictOffset">The top dict offset.</param>
    /// <param name="topDictSize">The top dict size.</param>
    /// <param name="top">The top dict values.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private loadGlobalSubrs (table: ReadOnlyMemory<byte>) (span: ReadOnlySpan<byte>) (topDictOffset: int) (topDictSize: int) (top: TopDict) (face: Face) (failure: Cff2Accel) : Cff2Accel =
        let globalSubrsAt = topDictOffset + topDictSize
        match CffIndex.sanitize span globalSubrsAt CountSize (Sanitize.start span.Length) with
        | ValueNone -> failure
        | ValueSome remaining -> loadCharStrings table span top (CffIndex.read span globalSubrsAt CountSize) remaining face failure

    /// <summary>Interprets the top dict and reads its values.</summary>
    /// <param name="table">The table memory.</param>
    /// <param name="span">The table bytes.</param>
    /// <param name="topDictOffset">The top dict offset.</param>
    /// <param name="topDictSize">The top dict size.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private loadTopDict (table: ReadOnlyMemory<byte>) (span: ReadOnlySpan<byte>) (topDictOffset: int) (topDictSize: int) (face: Face) (failure: Cff2Accel) : Cff2Accel =
        match CffDict.interpret (span.Slice(topDictOffset, topDictSize)) with
        | ValueNone -> failure
        | ValueSome topOps ->
            match topDictOf topOps with
            | ValueNone -> failure
            | ValueSome top -> loadGlobalSubrs table span topDictOffset topDictSize top face failure

    /// <summary>Reads the CFF2 header and sanitizes the top dict bytes.</summary>
    /// <param name="table">The table memory.</param>
    /// <param name="face">The face.</param>
    /// <param name="failure">The invalid accelerator for this table.</param>
    /// <returns>The accelerator.</returns>
    let private loadHeader (table: ReadOnlyMemory<byte>) (face: Face) (failure: Cff2Accel) : Cff2Accel =
        let span = table.Span
        let topDictOffset = int (Binary.u8 span 2)
        let topDictSize = int (Binary.u16 span 3)
        match Sanitize.fits span.Length (int64 topDictOffset) topDictSize with
        | false -> failure
        | true -> loadTopDict table span topDictOffset topDictSize face failure

    /// <summary>Loads the CFF2 accelerator.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The accelerator.</returns>
    let load (face: Face) : Cff2Accel =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Cff2)
        let failure =
            { Valid = false
              Table = table
              NumGlyphs = 0
              GlobalSubrs = emptyIndex
              CharStrings = emptyIndex
              FdSelect = ValueNone
              PrivateDicts = ImmutableArray.Empty
              Store = ItemVariationStore.empty
              HasStore = false }
        match table.Length with
        | 0 -> failure
        | _ -> loadHeader table face failure
