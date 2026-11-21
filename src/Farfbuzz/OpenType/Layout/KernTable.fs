namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Aat
open Farfbuzz.OpenType

/// <summary>One kern pair of a format 0 subtable, <c>KernPair</c>.</summary>
[<Struct>]
type internal KernPair =
    { KernLeft: uint16
      KernRight: uint16
      KernValue: int16 }

/// <summary>A format 2 kern subtable, <c>KerxSubTableFormat2</c>.</summary>
[<Struct>]
type internal KernClasses =
    { ClassesTable: ReadOnlyMemory<byte>
      ClassesAt: int
      ClassesLength: int
      ArrayOffset: int
      LeftClass: ClassTable
      RightClass: ClassTable }

/// <summary>A format 3 kern subtable, <c>KernSubTableFormat3</c>.</summary>
[<Struct>]
type internal KernIndex =
    { IndexTable: ReadOnlyMemory<byte>
      IndexAt: int
      IndexLength: int
      GlyphCount: int
      KernValueCount: int
      LeftClassCount: int
      RightClassCount: int
      KernValueAt: int
      LeftClassAt: int
      RightClassAt: int
      KernIndexAt: int }

/// <summary>The direction and stream flags of one kern subtable, from its coverage byte.</summary>
[<Struct>]
type internal KernSubtableInfo =
    { Horizontal: bool
      CrossStream: bool
      Variation: bool }

/// <summary>The decoded state machine of a format 1 kern subtable and its kern-action addressing.</summary>
[<Struct>]
type internal KernMachineData =
    { Machine: StateMachine
      Table: ReadOnlyMemory<byte>
      RangeEnd: int
      MachineAt: int
      KernActionAt: int
      FirstSet: ImmutableArray<CodepointRange> }

/// <summary>One subtable of the legacy kern table, <c>KernSubTable</c>.</summary>
[<Struct>]
type internal KernSubtable =
    | KernPairs of pairs: ImmutableArray<KernPair> * info: KernSubtableInfo * compiled: Lazy<PairIndex voption>
    | KernMachine of data: KernMachineData * info: KernSubtableInfo
    | KernClasses of classes: KernClasses * info: KernSubtableInfo
    | KernIndex of index: KernIndex * info: KernSubtableInfo
    | KernUnsupported of format: int * info: KernSubtableInfo

/// <summary>The parsed legacy kern table, the immutable <c>OT::kern::accelerator_t</c>.</summary>
type internal KernTable =
    { HasData: bool
      HasStateMachine: bool
      HasCrossStream: bool
      Subtables: ImmutableArray<KernSubtable> }

/// <summary>The legacy kern table: the OT and Apple headers and subtable formats 0, 1, 2 and 3.</summary>
[<RequireQualifiedAccess>]
module internal KernTable =
    /// <summary>The largest subtable count a kern table may declare, <c>HB_AAT_MAX_SUBTABLES</c>.</summary>
    [<Literal>]
    let private MaxSubtables = 1024

    /// <summary>The OT subtable header size, <c>KernOTSubTableHeader::static_size</c>.</summary>
    [<Literal>]
    let private OTHeaderSize = 6

    /// <summary>The Apple subtable header size, <c>KernAATSubTableHeader::static_size</c>.</summary>
    [<Literal>]
    let private AppleHeaderSize = 8

    /// <summary>The OT table header size, <c>KernOT::min_size</c>.</summary>
    [<Literal>]
    let private OTTableHeaderSize = 4

    /// <summary>The Apple table header size, <c>KernAAT::min_size</c>.</summary>
    [<Literal>]
    let private AppleTableHeaderSize = 8

    /// <summary>The Apple kern version, <c>KernAAT::minVersion</c>.</summary>
    [<Literal>]
    let private AppleVersion = 0x00010000u

    /// <summary>The byte width of one kern pair, <c>KernPair::static_size</c>.</summary>
    [<Literal>]
    let private PairSize = 6

    /// <summary>The byte width of one format 2/3 kern value, <c>FWORD::static_size</c>.</summary>
    [<Literal>]
    let private FwordSize = 2

    /// <summary>The format 1 state table and kern-action header size.</summary>
    [<Literal>]
    let private Format1HeaderSize = 10

    /// <summary>The bad index returned when a byte offset precedes its array, <c>INT_MAX / 2</c>.</summary>
    [<Literal>]
    let private BadIndex = 1073741823

    /// <summary>The Apple vertical coverage bit, <c>KernAATSubTableHeader::Vertical</c>.</summary>
    [<Literal>]
    let private AppleVertical = 0x80uy

    /// <summary>The Apple cross-stream coverage bit, <c>KernAATSubTableHeader::CrossStream</c>.</summary>
    [<Literal>]
    let private AppleCrossStream = 0x40uy

    /// <summary>The Apple variation coverage bit, <c>KernAATSubTableHeader::Variation</c>.</summary>
    [<Literal>]
    let private AppleVariation = 0x20uy

    /// <summary>The OT horizontal coverage bit, <c>KernOTSubTableHeader::Horizontal</c>.</summary>
    [<Literal>]
    let private OTHorizontal = 0x01uy

    /// <summary>The OT cross-stream coverage bit, <c>KernOTSubTableHeader::CrossStream</c>.</summary>
    [<Literal>]
    let private OTCrossStream = 0x04uy

    /// <summary>The empty kern table: no kerning data.</summary>
    let empty : KernTable =
        { HasData = false
          HasStateMachine = false
          HasCrossStream = false
          Subtables = ImmutableArray.Empty }

    /// <summary>Whether a face includes kerning data, <c>kern::has_data</c>.</summary>
    /// <param name="kern">The kern table.</param>
    /// <returns>True when the table carries a non-zero version word.</returns>
    let hasData (kern: KernTable) : bool = kern.HasData

    /// <summary>Whether a face includes a state-machine subtable, <c>kern::has_state_machine</c>.</summary>
    /// <param name="kern">The kern table.</param>
    /// <returns>True when a format 1 subtable is present.</returns>
    let hasStateMachine (kern: KernTable) : bool = kern.HasStateMachine

    /// <summary>Whether a face includes a cross-stream subtable, <c>kern::has_cross_stream</c>.</summary>
    /// <param name="kern">The kern table.</param>
    /// <returns>True when a cross-stream coverage bit is set.</returns>
    let hasCrossStream (kern: KernTable) : bool = kern.HasCrossStream

    /// <summary>The coverage-derived flags of a subtable, <c>KernSubTableHeader::is_horizontal</c> and the stream bits.</summary>
    /// <param name="isApple">True for the Apple header.</param>
    /// <param name="coverage">The coverage byte.</param>
    /// <returns>The flags.</returns>
    let private subtableInfo (isApple: bool) (coverage: byte) : KernSubtableInfo =
        { Horizontal = if isApple then coverage &&& AppleVertical = 0uy else coverage &&& OTHorizontal <> 0uy
          CrossStream = if isApple then coverage &&& AppleCrossStream <> 0uy else coverage &&& OTCrossStream <> 0uy
          Variation = isApple && coverage &&& AppleVariation <> 0uy }

    /// <summary>Sanitizes a wide class table within a bound, <c>ClassTable&lt;HBUINT16&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The class-table offset.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeClassTable (table: ReadOnlySpan<byte>) (at: int) (limit: int) (ops: int) : int voption =
        match Sanitize.fits limit (int64 at) 4 with
        | false -> ValueNone
        | true ->
            let count = int (Binary.u16 table (at + 2))
            Sanitize.array limit (int64 (at + 4)) (uint32 count) 2u ops

    /// <summary>Sanitizes a class table, or accepts a null offset, <c>NNOffsetTo::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="offset">The class-table offset from the subtable.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeClassTableOrNull (table: ReadOnlySpan<byte>) (at: int) (offset: int) (limit: int) (ops: int) : int voption =
        match offset with
        | 0 -> ValueSome ops
        | _ -> sanitizeClassTable table (at + offset) limit ops

    /// <summary>Sanitizes a format 0 subtable, <c>KerxSubTableFormat0::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat0 (table: ReadOnlySpan<byte>) (headerSize: int) (at: int) (limit: int) (ops: int) : int voption =
        match Sanitize.fits limit (int64 (at + headerSize)) 8 with
        | false -> ValueNone
        | true -> Sanitize.array limit (int64 (at + headerSize + 8)) (uint32 (Binary.u16 table (at + headerSize))) (uint32 PairSize) ops

    /// <summary>Sanitizes a format 1 subtable, <c>KerxSubTableFormat1::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat1 (table: ReadOnlySpan<byte>) (headerSize: int) (numGlyphs: int) (at: int) (limit: int) (ops: int) : int voption =
        match Sanitize.fits limit (int64 at) (headerSize + Format1HeaderSize) with
        | false -> ValueNone
        | true ->
            match StateTable.sanitize table (at + headerSize) false 0 numGlyphs ops with
            | ValueNone -> ValueNone
            | ValueSome (struct (remaining, _)) -> ValueSome remaining

    /// <summary>Sanitizes the right class table and the array offset of a format 2 subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="rightOffset">The right class-table offset.</param>
    /// <param name="arrayOffset">The kerning-array offset.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat2Right (table: ReadOnlySpan<byte>) (at: int) (rightOffset: int) (arrayOffset: int) (limit: int) (ops: int) : int voption =
        match sanitizeClassTableOrNull table at rightOffset limit ops with
        | ValueNone -> ValueNone
        | ValueSome ops2 ->
            match arrayOffset = 0 || at + arrayOffset <= limit with
            | false -> ValueNone
            | true -> ValueSome ops2

    /// <summary>Sanitizes a format 2 subtable, <c>KerxSubTableFormat2::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat2 (table: ReadOnlySpan<byte>) (headerSize: int) (at: int) (limit: int) (ops: int) : int voption =
        match Sanitize.fits limit (int64 at) (headerSize + 8) with
        | false -> ValueNone
        | true ->
            let leftOffset = int (Binary.u16 table (at + headerSize + 2))
            let rightOffset = int (Binary.u16 table (at + headerSize + 4))
            let arrayOffset = int (Binary.u16 table (at + headerSize + 6))
            match sanitizeClassTableOrNull table at leftOffset limit ops with
            | ValueNone -> ValueNone
            | ValueSome ops1 -> sanitizeFormat2Right table at rightOffset arrayOffset limit ops1

    /// <summary>Sanitizes a format 3 subtable, <c>KernSubTableFormat3::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeFormat3 (table: ReadOnlySpan<byte>) (headerSize: int) (at: int) (limit: int) (ops: int) : int voption =
        match Sanitize.fits limit (int64 at) (headerSize + 6) with
        | false -> ValueNone
        | true ->
            let glyphCount = int (Binary.u16 table (at + headerSize))
            let kernValueCount = int (Binary.u8 table (at + headerSize + 2))
            let leftClassCount = int (Binary.u8 table (at + headerSize + 3))
            let rightClassCount = int (Binary.u8 table (at + headerSize + 4))
            let total = kernValueCount * 2 + glyphCount * 2 + leftClassCount * rightClassCount
            Sanitize.range limit (int64 (at + headerSize + 6)) (uint32 total) ops

    /// <summary>Sanitizes one subtable, <c>KernSubTable::sanitize</c>'s format dispatch.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="limit">The exclusive byte bound.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeSubtable (table: ReadOnlySpan<byte>) (headerSize: int) (numGlyphs: int) (at: int) (format: int) (limit: int) (ops: int) : int voption =
        match format with
        | 0 -> sanitizeFormat0 table headerSize at limit ops
        | 1 -> sanitizeFormat1 table headerSize numGlyphs at limit ops
        | 2 -> sanitizeFormat2 table headerSize at limit ops
        | 3 -> sanitizeFormat3 table headerSize at limit ops
        | _ -> ValueSome ops

    /// <summary>Walks the subtables, threading the operation budget and the per-subtable bounds.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="isApple">True for the Apple header.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="at">The next subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when a subtable fails.</returns>
    [<TailCall>]
    let rec private sanitizeSubtables (table: ReadOnlySpan<byte>) (isApple: bool) (headerSize: int) (numGlyphs: int) (index: int) (count: int) (at: int) (ops: int) : int voption =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let length = if isApple then int (Binary.u32 table at) else int (Binary.u16 table (at + 2))
            let lengthOk = length >= headerSize
            let lengthFits = lengthOk && Sanitize.fits table.Length (int64 at) length
            match Sanitize.fits table.Length (int64 at) headerSize && lengthFits with
            | false -> ValueNone
            | true ->
                let format = if isApple then int (Binary.u8 table (at + 5)) else int (Binary.u8 table (at + 4))
                let limit = if index = count - 1 then table.Length else at + length
                match sanitizeSubtable table headerSize numGlyphs at format limit ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeSubtables table isApple headerSize numGlyphs (index + 1) count (at + length) remaining

    /// <summary>Sanitizes an OT-header kern table, <c>KernOT::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeOt (table: ReadOnlySpan<byte>) (numGlyphs: int) (ops: int) : int voption =
        match Sanitize.fits table.Length 0L OTTableHeaderSize with
        | false -> ValueNone
        | true ->
            let count = int (Binary.u16 table 2)
            match count > MaxSubtables with
            | true -> ValueNone
            | false -> sanitizeSubtables table false OTHeaderSize numGlyphs 0 count OTTableHeaderSize ops

    /// <summary>Sanitizes an Apple-header kern table, <c>KernAAT::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let private sanitizeApple (table: ReadOnlySpan<byte>) (numGlyphs: int) (ops: int) : int voption =
        match Sanitize.fits table.Length 0L AppleTableHeaderSize with
        | false -> ValueNone
        | true ->
            match Binary.u32 table 0 < AppleVersion with
            | true -> ValueNone
            | false ->
                let count = Binary.u32 table 4
                match count > uint32 MaxSubtables with
                | true -> ValueNone
                | false -> sanitizeSubtables table true AppleHeaderSize numGlyphs 0 (int count) AppleTableHeaderSize ops

    /// <summary>Sanitizes the kern table, <c>kern::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="numGlyphs">The face glyph count.</param>
    /// <param name="ops">The initial operation budget.</param>
    /// <returns>The remaining operations, or none when the table fails.</returns>
    let internal sanitize (table: ReadOnlySpan<byte>) (numGlyphs: int) (ops: int) : int voption =
        match Sanitize.fits table.Length 0L 4 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table 0 with
            | 0us -> sanitizeOt table numGlyphs ops
            | 1us -> sanitizeApple table numGlyphs ops
            | _ -> ValueNone

    /// <summary>The unfolder of one decoded pair, <c>KernPair</c> in file order.</summary>
    [<Struct>]
    type private PairUnfolder =
        { Base: int }
        interface ISpanUnfolder<byte, int, KernPair> with
            member this.Next (source, index) =
                let at = this.Base + index * PairSize
                struct ({ KernLeft = Binary.u16 source at
                          KernRight = Binary.u16 source (at + 2)
                          KernValue = Binary.i16 source (at + 4) }, index + 1)

    /// <summary>Decodes a format 0 pair array.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="at">The subtable offset.</param>
    /// <returns>The pairs in file order.</returns>
    let private decodeFormat0 (table: ReadOnlySpan<byte>) (headerSize: int) (at: int) : ImmutableArray<KernPair> =
        Sealed.unfoldSpan table (int (Binary.u16 table (at + headerSize))) 0 { PairUnfolder.Base = at + headerSize + 8 }

    /// <summary>Decodes a format 2 subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="length">The subtable length.</param>
    /// <returns>The class tables and array offset.</returns>
    let private decodeFormat2 (table: ReadOnlyMemory<byte>) (headerSize: int) (at: int) (length: int) : KernClasses =
        let span = table.Span
        let leftOffset = int (Binary.u16 span (at + headerSize + 2))
        let rightOffset = int (Binary.u16 span (at + headerSize + 4))
        let arrayOffset = int (Binary.u16 span (at + headerSize + 6))
        let leftAt = at + leftOffset
        let rightAt = at + rightOffset
        { ClassesTable = table
          ClassesAt = at
          ClassesLength = length
          ArrayOffset = arrayOffset
          LeftClass = { ClassTable.Table = table; At = leftAt; FirstGlyph = Binary.u16 span leftAt; Count = int (Binary.u16 span (leftAt + 2)); Wide = true }
          RightClass = { ClassTable.Table = table; At = rightAt; FirstGlyph = Binary.u16 span rightAt; Count = int (Binary.u16 span (rightAt + 2)); Wide = true } }

    /// <summary>Decodes a format 3 subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="headerSize">The subtable header size.</param>
    /// <param name="at">The subtable offset.</param>
    /// <param name="length">The subtable length.</param>
    /// <returns>The class arrays and their offsets.</returns>
    let private decodeFormat3 (table: ReadOnlyMemory<byte>) (headerSize: int) (at: int) (length: int) : KernIndex =
        let span = table.Span
        let glyphCount = int (Binary.u16 span (at + headerSize))
        let kernValueCount = int (Binary.u8 span (at + headerSize + 2))
        let leftClassCount = int (Binary.u8 span (at + headerSize + 3))
        let rightClassCount = int (Binary.u8 span (at + headerSize + 4))
        let kernValueAt = at + headerSize + 6
        let leftClassAt = kernValueAt + kernValueCount * FwordSize
        let rightClassAt = leftClassAt + glyphCount
        let kernIndexAt = rightClassAt + glyphCount
        { IndexTable = table
          IndexAt = at
          IndexLength = length
          GlyphCount = glyphCount
          KernValueCount = kernValueCount
          LeftClassCount = leftClassCount
          RightClassCount = rightClassCount
          KernValueAt = kernValueAt
          LeftClassAt = leftClassAt
          RightClassAt = rightClassAt
          KernIndexAt = kernIndexAt }

    /// <summary>Compiles the pairs of a format 0 subtable into a hashed index that answers as the binary search over them does.</summary>
    /// <param name="pairs">The pairs.</param>
    /// <returns>The index, or none.</returns>
    let private compilePairs (pairs: ImmutableArray<KernPair>) =
        let keys = Sealed.init pairs.Length (fun index -> (uint32 pairs[index].KernLeft <<< 16) ||| uint32 pairs[index].KernRight)
        let values = Sealed.init pairs.Length (fun index -> pairs[index].KernValue)
        Farfbuzz.Aat.PairLookup.compile (keys.AsSpan()) (values.AsSpan())

    /// <summary>The unfolder of the subtable walk, decoding one subtable per step.</summary>
    [<Struct>]
    type private SubtableUnfolder =
        { IsApple: bool
          HeaderSize: int
          Count: int
          NumGlyphs: int
          Table: ReadOnlyMemory<byte> }
        interface ISpanUnfolder<byte, struct (int * int), KernSubtable> with
            member this.Next (source, state) =
                let struct (index, at) = state
                let span = source
                let format = if this.IsApple then int (Binary.u8 span (at + 5)) else int (Binary.u8 span (at + 4))
                let coverage = if this.IsApple then Binary.u8 span (at + 4) else Binary.u8 span (at + 5)
                let length = if this.IsApple then int (Binary.u32 span at) else int (Binary.u16 span (at + 2))
                let info = subtableInfo this.IsApple coverage
                let subtable =
                    match format with
                    | 0 ->
                        let pairs = decodeFormat0 span this.HeaderSize at
                        KernPairs (pairs, info, lazy (compilePairs pairs))
                    | 1 ->
                        let machineAt = at + this.HeaderSize
                        let machine = StateTable.read this.Table machineAt false 0
                        let classifier: Kerx.Format1EntryClassifier = { Kerx.Format1EntryClassifier.Extended = false }
                        let data =
                            { Machine = machine
                              FirstSet = StateTable.initialGlyphs machine this.NumGlyphs classifier
                              Table = this.Table
                              RangeEnd = if index = this.Count - 1 then this.Table.Length else at + length
                              MachineAt = machineAt
                              KernActionAt = machineAt + int (Binary.u16 span (machineAt + 8)) }
                        KernMachine (data, info)
                    | 2 -> KernClasses (decodeFormat2 this.Table this.HeaderSize at length, info)
                    | 3 -> KernIndex (decodeFormat3 this.Table this.HeaderSize at length, info)
                    | _ -> KernUnsupported (format, info)
                struct (subtable, struct (index + 1, at + length))

    /// <summary>Counts the subtables and reports the state-machine and cross-stream flags.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="isApple">True for the Apple header.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="tableHeaderSize">The table header size.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="at">The next subtable offset.</param>
    /// <param name="hasMachine">Whether a format 1 was seen.</param>
    /// <param name="hasCross">Whether a cross-stream coverage bit was seen.</param>
    /// <returns>The two flags.</returns>
    [<TailCall>]
    let rec private countAndFlags (table: ReadOnlySpan<byte>) (isApple: bool) (count: int) (tableHeaderSize: int) (index: int) (at: int) (hasMachine: bool) (hasCross: bool) : struct (bool * bool) =
        match index >= count with
        | true -> struct (hasMachine, hasCross)
        | false ->
            let format = if isApple then Binary.u8 table (at + 5) else Binary.u8 table (at + 4)
            let coverage = if isApple then Binary.u8 table (at + 4) else Binary.u8 table (at + 5)
            let length = if isApple then int (Binary.u32 table at) else int (Binary.u16 table (at + 2))
            let hasMachine = hasMachine || (format = 1uy)
            let hasCross = hasCross || (if isApple then coverage &&& AppleCrossStream <> 0uy else coverage &&& OTCrossStream <> 0uy)
            countAndFlags table isApple count tableHeaderSize (index + 1) (at + length) hasMachine hasCross

    /// <summary>Loads and sanitizes a face's kern table, <c>OT::kern::accelerator_t</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The kern table, empty when absent or invalid.</returns>
    let load (face: Face) : KernTable =
        let table = Face.referenceTable face Tags.Kern
        match table.IsEmpty with
        | true -> empty
        | false ->
            match sanitize table.Span face.GlyphCount (Sanitize.start table.Length) with
            | ValueNone -> empty
            | ValueSome _ ->
                let span = table.Span
                let major = Binary.u16 span 0
                let isApple = major = 1us
                let headerSize = if isApple then AppleHeaderSize else OTHeaderSize
                let tableHeaderSize = if isApple then AppleTableHeaderSize else OTTableHeaderSize
                let count = if isApple then int (min (Binary.u32 span 4) (uint32 MaxSubtables)) else int (Binary.u16 span 2)
                let version32 = Binary.u32 span 0
                let struct (hasMachine, hasCross) = countAndFlags span isApple count tableHeaderSize 0 tableHeaderSize false false
                { HasData = version32 <> 0u
                  HasStateMachine = hasMachine
                  HasCrossStream = hasCross
                  Subtables = Sealed.unfoldSpan span count (struct (0, tableHeaderSize)) { SubtableUnfolder.IsApple = isApple; HeaderSize = headerSize; Count = count; NumGlyphs = face.GlyphCount; Table = table } }

    /// <summary>The flags of a subtable, read by the apply loop.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The flags.</returns>
    let internal subtableInfoOf (subtable: KernSubtable) : KernSubtableInfo =
        match subtable with
        | KernPairs (_, info, _) -> info
        | KernMachine (_, info) -> info
        | KernClasses (_, info) -> info
        | KernIndex (_, info) -> info
        | KernUnsupported (_, info) -> info

    /// <summary>Binary-searches a pair array, <c>KernPair::cmp</c> over <c>hb_bsearch_impl</c>.</summary>
    /// <param name="pairs">The pairs.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private bsearchPairs (pairs: ReadOnlySpan<KernPair>) (left: uint32) (right: uint32) (lo: int) (hi: int) : int voption =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            let pair = pairs[mid]
            match uint32 pair.KernLeft > left, uint32 pair.KernLeft < left with
            | true, _ -> bsearchPairs pairs left right lo (mid - 1)
            | false, true -> bsearchPairs pairs left right (mid + 1) hi
            | false, false ->
                match uint32 pair.KernRight > right, uint32 pair.KernRight < right with
                | true, _ -> bsearchPairs pairs left right lo (mid - 1)
                | false, true -> bsearchPairs pairs left right (mid + 1) hi
                | false, false -> ValueSome mid

    /// <summary>The kerning of a format 0 pair, <c>KerxSubTableFormat0::get_kerning</c>.</summary>
    /// <param name="pairs">The pairs.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning value, zero when the pair is absent.</returns>
    let private getKerningPairs (pairs: ImmutableArray<KernPair>) (left: uint32) (right: uint32) : int =
        let span = pairs.AsSpan()
        match bsearchPairs span left right 0 (span.Length - 1) with
        | ValueSome index -> int pairs[index].KernValue
        | ValueNone -> 0

    /// <summary>The kerning of a format 2 pair, <c>KerxSubTableFormat2::get_kerning</c>.</summary>
    /// <param name="classes">The subtable.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning value, zero when the pair is absent.</returns>
    let private getKerningClasses (classes: KernClasses) (left: uint32) (right: uint32) : int =
        let l = uint32 (ClassTable.classOf classes.LeftClass left 0us)
        let r = uint32 (ClassTable.classOf classes.RightClass right 0us)
        let kernIdx = l + r
        let span = classes.ClassesTable.Span
        let arrayOffset = classes.ArrayOffset
        match uint64 kernIdx < uint64 arrayOffset with
        | true -> 0
        | false ->
            let index = (uint64 kernIdx - uint64 arrayOffset) / 2UL
            match index > uint64 BadIndex with
            | true -> 0
            | false ->
                let vAt = classes.ClassesAt + arrayOffset + int index * FwordSize
                match Sanitize.fits (classes.ClassesAt + classes.ClassesLength) (int64 vAt) FwordSize with
                | false -> 0
                | true -> int (Binary.i16 span vAt)

    /// <summary>The kerning of a format 3 pair, <c>KernSubTableFormat3::get_kerning</c>.</summary>
    /// <param name="index">The subtable.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning value, zero when the pair is absent.</returns>
    let private getKerningIndex (index: KernIndex) (left: uint32) (right: uint32) : int =
        let span = index.IndexTable.Span
        let leftC = uint32 (Binary.u8 span (index.LeftClassAt + int left))
        let rightC = uint32 (Binary.u8 span (index.RightClassAt + int right))
        match leftC >= uint32 index.LeftClassCount || rightC >= uint32 index.RightClassCount with
        | true -> 0
        | false ->
            let i = int leftC * index.RightClassCount + int rightC
            let kernIndex = int (Binary.u8 span (index.KernIndexAt + i))
            int (Binary.i16 span (index.KernValueAt + kernIndex * FwordSize))

    /// <summary>The kerning of a pair through one subtable, <c>KernSubTable::get_kerning</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="left">The left glyph.</param>
    /// <param name="right">The right glyph.</param>
    /// <returns>The kerning value, zero when the pair is absent.</returns>
    let internal getKerning (subtable: KernSubtable) (left: uint32) (right: uint32) : int =
        match subtable with
        | KernPairs (pairs, _, _) -> getKerningPairs pairs left right
        | KernMachine _ -> 0
        | KernClasses (classes, _) -> getKerningClasses classes left right
        | KernIndex (index, _) -> getKerningIndex index left right
        | KernUnsupported _ -> 0
