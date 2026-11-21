namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Numerics

/// <summary>A table that answers a glyph's coverage index or class directly, built once with the subtable: entry <c>g</c>
/// holds what the coverage or class definition answers for glyph <c>g</c>, the exact answer, so that reading it is the same
/// as asking. A glyph beyond the table has the answer of <see cref="Complete"/>: none for a coverage, class zero for a class
/// definition when the table reaches the last glyph the structure names; when the table was cut short the structure is
/// asked instead.</summary>
[<Struct>]
type internal DirectIndex =
    { Table: ImmutableArray<uint16>
      Complete: bool }

/// <summary>A GPOS anchor as an attachment reads it, <c>Anchor</c>: <see cref="NoAnchor"/> stands for an offset that is
/// null or whose anchor fails the check the apply functions make (<c>offset.sanitize (&amp;c-&gt;sanitizer, this)</c>),
/// where the subtable does not apply; formats 1 and 2 are the design coordinates (a contour point is used only when the
/// font funcs give one, which the OpenType font funcs never do), format 3 adds the two devices, and a format that is none of
/// them is the zero anchor, which is found.</summary>
type internal Anchor =
    | NoAnchor
    | PlainAnchor of plainX: int * plainY: int
    | DeviceAnchor of deviceX: int * deviceY: int * xDevice: Device * yDevice: Device

/// <summary>A mark-to-base or mark-to-mark subtable, <c>MarkBasePosFormat1_2</c> and <c>MarkMarkPosFormat1_2</c>: the mark
/// array, in the order of the mark coverage (an index beyond it is the Null record: class zero and the zero anchor), and
/// the matrix of the anchors of the base glyphs or second marks, row by row, one column for each class.</summary>
type internal MarkAttachment =
    { MarkCoverage: Coverage
      MarkDirect: DirectIndex
      TargetCoverage: Coverage
      TargetDirect: DirectIndex
      ClassCount: int
      MarkClasses: ImmutableArray<uint16>
      MarkAnchors: ImmutableArray<Anchor>
      Rows: int
      Anchors: ImmutableArray<Anchor> }

/// <summary>A mark-to-ligature subtable, <c>MarkLigPosFormat1_2</c>: the mark array as for a base, and for each
/// ligature of the ligature coverage a matrix with a row for each component. The anchors of all ligatures are one array,
/// each ligature's starting where <c>Starts</c> says; a ligature beyond the array, or with a null offset, is the Null
/// matrix of no rows.</summary>
[<NoEquality; NoComparison>]
type internal LigatureAttachment =
    { LigMarkCoverage: Coverage
      LigMarkDirect: DirectIndex
      LigatureCoverage: Coverage
      LigatureDirect: DirectIndex
      LigClassCount: int
      LigMarkClasses: ImmutableArray<uint16>
      LigMarkAnchors: ImmutableArray<Anchor>
      Components: ImmutableArray<int>
      Starts: ImmutableArray<int>
      LigAnchors: ImmutableArray<Anchor> }

/// <summary>A cursive attachment subtable, <c>CursivePosFormat1_2</c>: the entry and exit anchor of each glyph of the
/// coverage, in its order; a record beyond the arrays is the Null record, whose anchors are null.</summary>
[<NoEquality; NoComparison>]
type internal CursiveAttachment =
    { CursiveCoverage: Coverage
      CursiveDirect: DirectIndex
      Entries: ImmutableArray<Anchor>
      Exits: ImmutableArray<Anchor> }

/// <summary>A GPOS subtable. Single and pair adjustment subtables carry their decoded coverage, class definitions and
/// value records: values are the raw 16-bit fields of the value records in file order, and the devices run parallel to
/// them, the null device where a field is no device offset or its device fails the lazy check; the cursive, mark-to-base,
/// mark-to-ligature and mark-to-mark subtables carry their coverages and anchors; the context and chained-context subtables
/// carry the rules GSUB's carry, read by the same code; a subtable of a format none of the types has stays <c>PosPending</c>
/// with its digests and never applies.</summary>
[<NoEquality; NoComparison>]
type internal PosSubtable =
    | SinglePos of singleCoverage: Coverage * singleFormat: uint16 * singleCount: int * singleValues: ImmutableArray<int16> * singleDevices: ImmutableArray<Device> * singleDirect: DirectIndex
    | PairPosGlyphs of pairCoverage: Coverage * pairFormat1: uint16 * pairFormat2: uint16 * setStarts: ImmutableArray<int> * seconds: ImmutableArray<uint32> * pairValues: ImmutableArray<int16> * pairDevices: ImmutableArray<Device> * pairSecondDigest: Digest * pairDirect: DirectIndex
    | PairPosClasses of classCoverage: Coverage * classFormat1: uint16 * classFormat2: uint16 * classDef1: ClassDef * classDef2: ClassDef * class1Count: int * class2Count: int * classValues: ImmutableArray<int16> * classDevices: ImmutableArray<Device> * classSecondDigest: Digest * classDirect: DirectIndex * firstDirect: DirectIndex * secondDirect: DirectIndex
    | CursivePos of cursive: CursiveAttachment
    | MarkBasePos of markBase: MarkAttachment
    | MarkLigPos of markLig: LigatureAttachment
    | MarkMarkPos of markMark: MarkAttachment
    | PosContext of posRules: ContextRules
    | PosChainContext of posChainRules: ChainRules
    | PosPending of posType: int * posFormat: int * coverageDigest: Digest * second: Digest

/// <summary>A mark-to-base or mark-to-mark subtable compiled for the mark loop: the anchors of the marks and of the matrix as raw
/// design coordinates in flat arrays, so that attaching a mark reads a few array cells and scales two numbers. A subtable with a
/// device or variation anchor is not compiled, and the general reader attaches through it.</summary>
type internal MarkTable =
    { Source: MarkAttachment
      MarkX: ImmutableArray<int> // by index in the mark array: the x of the mark's anchor, zero for a null anchor
      MarkY: ImmutableArray<int>
      AnchorX: ImmutableArray<int> // the x of each anchor of the matrix, row by row
      AnchorY: ImmutableArray<int>
      AnchorKnown: ImmutableArray<bool> } // whether the matrix has an anchor there, the subtable not applying where it has none

/// <summary>The pair lookups of a plan compiled for the positioning kernel: the compiled subtables of every lookup of the
/// table, and for the lookups of the first 64 positions of the map the pair loop applies to a run whose glyphs all carry the
/// global mask and none of which is default ignorable, the glyphs each starts at and its tables. Built once with the plan.</summary>
type internal PairProgram =
    { Tables: ImmutableArray<ImmutableArray<PairTable>> // by index in the table, empty for a lookup outside the map or with a subtable that is no pair
      PositionCovered: ImmutableArray<ImmutableArray<uint64>> // by position in the map: the glyphs a lookup starts at
      PositionTables: ImmutableArray<ImmutableArray<PairTable>> // by position in the map: the lookup's compiled subtables
      Glyphs: ImmutableArray<uint64> // for each glyph, the first 64 positions whose lookup covers it, as the plan's table
      Enabled: bool // the map has at most 64 lookups and the table of covering bits is there: the kernel admits a lookup by its bit alone
      Lean: uint64 // the positions whose lookup the lean loop applies
      LeanOnly: bool // every position that covers some glyph is one the lean loop applies or one no run admits, so no look at the glyphs is needed
      Idle: uint64 // the positions whose lookup has no mask, which no run admits
      Marks: ImmutableArray<ImmutableArray<MarkTable voption>> // by index in the table, the compiled mark subtables of a lookup of the map, empty for another lookup
      Targets: ImmutableArray<ImmutableArray<uint64> voption> // by index in the table, the glyphs a lookup of the map of mark attachments attaches to, none for another lookup
      Inert: PairProgram voption // the program for a font whose devices do nothing, compiled where a table's records carry devices; none when no table's do
      Resolved: ResolvedPairs voption } // the programs for fonts that read devices, one for each key, built from the program's own sets; none when no table's records carry devices

/// <summary>The programs of one plan compiled for the fonts that read devices: each is the plan's program with its pair tables resolved for the
/// coordinates and scale of the font, built the first time a font of that key shapes with the plan and kept as one of the newest few keys.</summary>
and [<Sealed; AllowNullLiteral>] internal ResolvedPairs(remake: Lazy<ImmutableArray<Lazy<ImmutableArray<PairTable>>>> -> PairProgram) =
    /// <summary>The programs built so far, by key.</summary>
    let slot = KeyedSlot<ResolveKey, PairProgram>(ResolvedPairs.Capacity)

    /// <summary>The number of keys a plan keeps programs for.</summary>
    static member Capacity = 4

    /// <summary>The program kept for a key, if one was built.</summary>
    /// <param name="key">The coordinates and scale.</param>
    /// <returns>The program, or none.</returns>
    member _.TryProgram (key: ResolveKey) =
        slot.TryFind key

    /// <summary>Builds the program for a key from the pair tables resolved for it, and keeps it.</summary>
    /// <param name="key">The coordinates and scale.</param>
    /// <param name="tables">The pair tables of every lookup of the table, resolved for the key.</param>
    /// <returns>The program.</returns>
    member _.Add (key: ResolveKey, tables: Lazy<ImmutableArray<Lazy<ImmutableArray<PairTable>>>>) =
        slot.Add (key, remake tables)

    /// <summary>Whether another object is a cell of programs; equality ignores the programs, so the plans that hold one compare by their other fields.</summary>
    /// <param name="other">The other object.</param>
    /// <returns>True for any cell.</returns>
    override _.Equals other =
        match other with
        | :? ResolvedPairs -> true
        | _ -> false

    /// <summary>The hash of a cell, the same for every cell.</summary>
    /// <returns>Zero.</returns>
    override _.GetHashCode () = 0

/// <summary>The GPOS subtable parser and digest accessors, <c>PosLookupSubTable</c>.</summary>
[<RequireQualifiedAccess>]
module internal GposTables =
    /// <summary>The single-adjustment lookup type, <c>PosLookupSubTable::Single</c>.</summary>
    [<Literal>]
    let private SingleType = 1

    /// <summary>The pair-adjustment lookup type, <c>PosLookupSubTable::Pair</c>.</summary>
    [<Literal>]
    let private PairType = 2

    /// <summary>The cursive-attachment lookup type, <c>PosLookupSubTable::Cursive</c>.</summary>
    [<Literal>]
    let private CursiveType = 3

    /// <summary>The mark-to-base lookup type, <c>PosLookupSubTable::MarkBase</c>.</summary>
    [<Literal>]
    let private MarkBaseType = 4

    /// <summary>The mark-to-ligature lookup type, <c>PosLookupSubTable::MarkLig</c>.</summary>
    [<Literal>]
    let private MarkLigType = 5

    /// <summary>The mark-to-mark lookup type, <c>PosLookupSubTable::MarkMark</c>.</summary>
    [<Literal>]
    let private MarkMarkType = 6

    /// <summary>The context lookup type, <c>PosLookupSubTable::Context</c>.</summary>
    [<Literal>]
    let private ContextType = 7

    /// <summary>The chained-context lookup type, <c>PosLookupSubTable::ChainContext</c>.</summary>
    [<Literal>]
    let private ChainType = 8

    /// <summary>The extension lookup type, <c>PosLookupSubTable::Extension</c>.</summary>
    [<Literal>]
    let private ExtensionType = 9

    /// <summary>The byte width of a 16-bit offset or glyph id.</summary>
    [<Literal>]
    let private Width16 = 2

    /// <summary>The byte width of a 24-bit offset or glyph id.</summary>
    [<Literal>]
    let private Width24 = 3

    /// <summary>The value-format bits that name a value rather than a device, <c>xPlacement</c> to <c>yAdvance</c>.</summary>
    [<Literal>]
    let private ValueBits = 0x000Fus

    /// <summary>The number of fields of a value record, <c>ValueFormat::get_len</c>: the set bits of the low byte.</summary>
    /// <param name="format">The value format.</param>
    /// <returns>The field count.</returns>
    let valueLength (format: uint16) =
        BitOperations.PopCount (uint32 (format &&& 0x00FFus))

    /// <summary>Reads a coverage reached through a relative offset field, Null for a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetValue">The relative coverage offset.</param>
    /// <returns>The coverage.</returns>
    let private coverageAt (table: ReadOnlySpan<byte>) o offsetValue =
        match offsetValue with
        | 0u -> CoverageNull
        | _ -> Coverage.parse table (o + int offsetValue)

    /// <summary>Reads a class definition reached through a relative offset field, Null for a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetValue">The relative class-definition offset.</param>
    /// <returns>The class definition.</returns>
    let private classDefAt (table: ReadOnlySpan<byte>) o offsetValue =
        match offsetValue with
        | 0u -> ClassDefNull
        | _ -> ClassDef.parse table (o + int offsetValue)

    /// <summary>Reads a coverage-offset field of one width.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The field offset.</param>
    /// <param name="width">Two or four bytes.</param>
    /// <returns>The offset value.</returns>
    let private readLong (table: ReadOnlySpan<byte>) offset width =
        match width with
        | 4 -> Binary.u32 table offset
        | _ -> uint32 (Binary.u16 table offset)

    /// <summary>The number of glyphs a direct table covers at most.</summary>
    [<Literal>]
    let private DirectLimit = 4096

    /// <summary>The answer a direct table stores for a glyph that is not covered.</summary>
    [<Literal>]
    let private NoAnswer = 0xFFFFus

    /// <summary>The last glyph a coverage can cover: the largest glyph of its array, or the largest end of its ranges, so a
    /// glyph beyond it is not covered whatever order the file lists them in.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The glyph.</returns>
    let rec private lastCovered (coverage: Coverage) =
        match coverage with
        | CoverageNull -> 0u
        | CoverageDirect (_, _, source) -> lastCovered source
        | CoverageGlyphs glyphs -> match glyphs.IsEmpty with | true -> 0u | false -> Seq.max glyphs
        | CoverageRanges ranges -> match ranges.IsEmpty with | true -> 0u | false -> Seq.max (Seq.map (fun (range: RangeRecord) -> range.Last) ranges)

    /// <summary>Builds the direct table of a coverage: the covered index of every glyph up to the last the coverage can
    /// cover, or up to the limit. A coverage with an index that does not fit a table entry gets none.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The table.</returns>
    let private directCoverage (coverage: Coverage) =
        let last = lastCovered coverage
        let count = int (min (last + 1u) (uint32 DirectLimit))
        let answers =
            Sealed.init count (fun glyph ->
                match Coverage.index coverage (uint32 glyph) with
                | Covered index -> index
                | NotCovered -> -1)
        match coverage with
        | CoverageNull -> { Table = ImmutableArray<uint16>.Empty; Complete = true }
        | CoverageDirect _
        | CoverageGlyphs _
        | CoverageRanges _ ->
            match Seq.exists (fun answer -> answer >= int NoAnswer) answers with
            | true -> { Table = ImmutableArray<uint16>.Empty; Complete = false }
            | false ->
                let table = Sealed.map (fun answer -> match answer < 0 with | true -> NoAnswer | false -> uint16 answer) (answers.AsSpan())
                { Table = table; Complete = last < uint32 DirectLimit }

    /// <summary>The last glyph a class definition can name, or none for a class definition that names none.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <returns>The glyph.</returns>
    let rec private lastClassified (classDef: ClassDef) =
        match classDef with
        | ClassDefNull -> 0u
        | ClassDefDirect (_, _, source) -> lastClassified source
        | ClassDefArray (start, classes) -> start + uint32 classes.Length
        | ClassDefRanges ranges -> match ranges.IsEmpty with | true -> 0u | false -> Seq.max (Seq.map (fun (range: RangeRecord) -> range.Last) ranges)

    /// <summary>Builds the direct table of a class definition: the class of every glyph up to the last glyph it can name, or
    /// up to the limit; a class beyond what an entry holds is kept as the largest, which no class count admits.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <returns>The table.</returns>
    let private directClasses (classDef: ClassDef) =
        let last = lastClassified classDef
        let count = int (min (last + 1u) (uint32 DirectLimit))
        match classDef with
        | ClassDefNull -> { Table = ImmutableArray<uint16>.Empty; Complete = true }
        | ClassDefDirect _
        | ClassDefArray _
        | ClassDefRanges _ ->
            let table = Sealed.init count (fun glyph -> uint16 (min 0xFFFFu (ClassDef.classOf classDef (uint32 glyph))))
            { Table = table; Complete = last < uint32 DirectLimit }

    /// <summary>Decodes the 16-bit fields of value records.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ValueDecoder =
        { Base: int }
        interface ISpanUnfolder<byte, int, int16> with
            member this.Next (table, index) =
                struct (Binary.i16 table (this.Base + index * 2), index + 1)

    /// <summary>Decodes the devices of value records: the device a field names where the field is a device offset, the
    /// null device elsewhere.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DeviceDecoder =
        { ValuesBase: int
          DeviceBase: int
          Format1: uint16
          Format2: uint16
          Length1: int
          RecordLength: int }
        interface ISpanUnfolder<byte, int, Device> with
            member this.Next (table, index) =
                let slot = index % this.RecordLength
                let format, position =
                    match slot < this.Length1 with
                    | true -> this.Format1, slot
                    | false -> this.Format2, slot - this.Length1
                let raw = int (Binary.u16 table (this.ValuesBase + index * 2))
                let isDevice = position >= BitOperations.PopCount (uint32 (format &&& ValueBits))
                match isDevice && raw <> 0 with
                | true -> struct (DeviceTable.parseSanitized table (this.DeviceBase + raw), index + 1)
                | false -> struct (NoDevice, index + 1)

    /// <summary>Decodes the values of a run of value records.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="start">The offset of the first field.</param>
    /// <param name="count">The number of fields.</param>
    /// <returns>The fields.</returns>
    let private valuesAt (table: ReadOnlySpan<byte>) start count =
        Sealed.unfoldSpan table count 0 { ValueDecoder.Base = start }

    /// <summary>Decodes the devices of a run of value records, nothing when no format names a device.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="start">The offset of the first field.</param>
    /// <param name="deviceBase">The offset the device offsets are relative to.</param>
    /// <param name="format1">The first value format.</param>
    /// <param name="format2">The second value format, zero for a single record.</param>
    /// <param name="count">The number of fields.</param>
    /// <returns>The devices, parallel to the fields.</returns>
    let private devicesAt (table: ReadOnlySpan<byte>) start deviceBase (format1: uint16) (format2: uint16) count =
        match (format1 ||| format2) &&& 0x00F0us with
        | 0us -> ImmutableArray<Device>.Empty
        | _ ->
            let length1 = valueLength format1
            Sealed.unfoldSpan table count 0
                { DeviceDecoder.ValuesBase = start
                  DeviceBase = deviceBase
                  Format1 = format1
                  Format2 = format2
                  Length1 = length1
                  RecordLength = length1 + valueLength format2 }

    /// <summary>Parses a single-adjustment subtable, <c>SinglePos</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parseSingle (table: ReadOnlySpan<byte>) o =
        let format = int (Binary.u16 table o)
        let medium = format >= 3
        let covW = match medium with | true -> 4 | false -> Width16
        let w = match medium with | true -> Width24 | false -> Width16
        match format with
        | 1 | 3 ->
            let coverage = coverageAt table o (readLong table (o + 2) covW)
            let valueFormat = Binary.u16 table (o + 2 + covW)
            let start = o + 4 + covW
            let length = valueLength valueFormat
            SinglePos (coverage, valueFormat, -1, valuesAt table start length, devicesAt table start o valueFormat 0us length, directCoverage coverage)
        | 2 | 4 ->
            let coverage = coverageAt table o (readLong table (o + 2) covW)
            let valueFormat = Binary.u16 table (o + 2 + covW)
            let count = int (Coverage.readWide table (o + 4 + covW) w)
            let start = o + 4 + covW + w
            let length = count * valueLength valueFormat
            SinglePos (coverage, valueFormat, count, valuesAt table start length, devicesAt table start o valueFormat 0us length, directCoverage coverage)
        | _ ->
            let struct (coverage, second) = SubtableDigest.collect table o SingleType false
            PosPending (SingleType, format, coverage, second)

    /// <summary>Decodes the second glyphs of the records of one pair set.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private SecondDecoder =
        { First: int
          Stride: int
          Width: int }
        interface ISpanUnfolder<byte, int, uint32> with
            member this.Next (table, index) =
                struct (Coverage.readWide table (this.First + index * this.Stride) this.Width, index + 1)

    /// <summary>Decodes the value fields of the records of one pair set, which sit after each record's glyph.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private RecordValueDecoder =
        { ValuesFirst: int
          RecordStride: int
          Fields: int }
        interface ISpanUnfolder<byte, int, int16> with
            member this.Next (table, index) =
                let record = index / this.Fields
                let field = index - record * this.Fields
                struct (Binary.i16 table (this.ValuesFirst + record * this.RecordStride + field * 2), index + 1)

    /// <summary>Decodes the devices of the value fields of the records of one pair set.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private RecordDeviceDecoder =
        { DeviceValuesFirst: int
          DeviceStride: int
          DeviceBase: int
          Format1: uint16
          Format2: uint16
          Length1: int
          Fields: int }
        interface ISpanUnfolder<byte, int, Device> with
            member this.Next (table, index) =
                let record = index / this.Fields
                let slot = index - record * this.Fields
                let format, position =
                    match slot < this.Length1 with
                    | true -> this.Format1, slot
                    | false -> this.Format2, slot - this.Length1
                let raw = int (Binary.u16 table (this.DeviceValuesFirst + record * this.DeviceStride + slot * 2))
                let isDevice = position >= BitOperations.PopCount (uint32 (format &&& ValueBits))
                match isDevice && raw <> 0 with
                | true -> struct (DeviceTable.parseSanitized table (this.DeviceBase + raw), index + 1)
                | false -> struct (NoDevice, index + 1)

    /// <summary>One decoded pair set: its second glyphs and the value fields and devices of its records.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DecodedSet =
        { SetSeconds: ImmutableArray<uint32>
          SetValues: ImmutableArray<int16>
          SetDevices: ImmutableArray<Device> }

    /// <summary>Decodes one pair set, <c>PairSet</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="set">The pair set offset.</param>
    /// <param name="glyphWidth">The width of a glyph id and of the record count.</param>
    /// <param name="format1">The first value format.</param>
    /// <param name="format2">The second value format.</param>
    /// <returns>The decoded set.</returns>
    let private decodeSet (table: ReadOnlySpan<byte>) set glyphWidth (format1: uint16) (format2: uint16) =
        let length1 = valueLength format1
        let fields = length1 + valueLength format2
        let count = int (Coverage.readWide table set glyphWidth)
        let stride = glyphWidth + 2 * fields
        let first = set + glyphWidth
        let devices =
            match (format1 ||| format2) &&& 0x00F0us with
            | 0us -> ImmutableArray<Device>.Empty
            | _ ->
                Sealed.unfoldSpan table (count * fields) 0
                    { RecordDeviceDecoder.DeviceValuesFirst = first + glyphWidth
                      DeviceStride = stride
                      DeviceBase = set
                      Format1 = format1
                      Format2 = format2
                      Length1 = length1
                      Fields = fields }
        { SetSeconds = Sealed.unfoldSpan table count 0 { SecondDecoder.First = first; Stride = stride; Width = glyphWidth }
          SetValues = Sealed.unfoldSpan table (count * fields) 0 { RecordValueDecoder.ValuesFirst = first + glyphWidth; RecordStride = stride; Fields = fields }
          SetDevices = devices }

    /// <summary>Decodes the pair sets of a pair-adjustment subtable.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private PairSetDecoder =
        { Subtable: int
          OffsetsBase: int
          OffsetWidth: int
          Format1: uint16
          Format2: uint16 }
        interface ISpanUnfolder<byte, int, DecodedSet> with
            member this.Next (table, index) =
                let offset = Coverage.readWide table (this.OffsetsBase + index * this.OffsetWidth) this.OffsetWidth
                match offset with
                | 0u -> struct ({ SetSeconds = ImmutableArray<uint32>.Empty; SetValues = ImmutableArray<int16>.Empty; SetDevices = ImmutableArray<Device>.Empty }, index + 1)
                | _ -> struct (decodeSet table (this.Subtable + int offset) this.OffsetWidth this.Format1 this.Format2, index + 1)

    /// <summary>Parses a pair-adjustment subtable of format 1 or 3, <c>PairPosFormat1_3</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The format, one or three.</param>
    /// <returns>The subtable.</returns>
    let private parsePairGlyphs (table: ReadOnlySpan<byte>) o format =
        let medium = format = 3
        let covW = match medium with | true -> 4 | false -> Width16
        let w = match medium with | true -> Width24 | false -> Width16
        let coverage = coverageAt table o (readLong table (o + 2) covW)
        let format1 = Binary.u16 table (o + 2 + covW)
        let format2 = Binary.u16 table (o + 4 + covW)
        let count = int (Coverage.readWide table (o + 6 + covW) w)
        let sets = Sealed.unfoldSpan table count 0 { PairSetDecoder.Subtable = o; OffsetsBase = o + 6 + covW + w; OffsetWidth = w; Format1 = format1; Format2 = format2 }
        let struct (_, second) = SubtableDigest.collect table o PairType false
        let seconds = Sealed.map (fun (set: DecodedSet) -> set.SetSeconds) (sets.AsSpan())
        let values = Sealed.map (fun (set: DecodedSet) -> set.SetValues) (sets.AsSpan())
        let devices = Sealed.map (fun (set: DecodedSet) -> set.SetDevices) (sets.AsSpan())
        PairPosGlyphs (coverage, format1, format2, Sealed.prefixLengths seconds, Sealed.flatten seconds, Sealed.flatten values, Sealed.flatten devices, second, directCoverage coverage)

    /// <summary>Parses a pair-adjustment subtable of format 2 or 4, <c>PairPosFormat2_4</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The format, two or four.</param>
    /// <returns>The subtable.</returns>
    let private parsePairClasses (table: ReadOnlySpan<byte>) o format =
        let covW = match format = 4 with | true -> 4 | false -> Width16
        let coverage = coverageAt table o (readLong table (o + 2) covW)
        let format1 = Binary.u16 table (o + 2 + covW)
        let format2 = Binary.u16 table (o + 4 + covW)
        let classDef1 = classDefAt table o (readLong table (o + 6 + covW) covW)
        let classDef2 = classDefAt table o (readLong table (o + 6 + 2 * covW) covW)
        let class1Count = int (Binary.u16 table (o + 6 + 3 * covW))
        let class2Count = int (Binary.u16 table (o + 8 + 3 * covW))
        let start = o + 10 + 3 * covW
        let fields = valueLength format1 + valueLength format2
        let count = class1Count * class2Count * fields
        let struct (_, second) = SubtableDigest.collect table o PairType false
        PairPosClasses (coverage, format1, format2, classDef1, classDef2, class1Count, class2Count, valuesAt table start count, devicesAt table start o format1 format2 count, second, directCoverage coverage, directClasses classDef1, directClasses classDef2)

    /// <summary>Parses a pair-adjustment subtable, <c>PairPos</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <returns>The subtable.</returns>
    let private parsePair (table: ReadOnlySpan<byte>) o =
        match int (Binary.u16 table o) with
        | 1 -> parsePairGlyphs table o 1
        | 3 -> parsePairGlyphs table o 3
        | 2 -> parsePairClasses table o 2
        | 4 -> parsePairClasses table o 4
        | format ->
            let struct (coverage, second) = SubtableDigest.collect table o PairType false
            PosPending (PairType, format, coverage, second)

    /// <summary>The anchor of an unknown format, which the apply functions read as the origin and count as found.</summary>
    let private zeroAnchor = PlainAnchor (0, 0)

    /// <summary>The absolute offset a relative offset field names, or minus one for the Null offset (<c>OffsetTo::operator()</c>
    /// of a zero offset) and for an offset that lies beyond the table, which no sanitized table has.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The offset the field is relative to.</param>
    /// <param name="offset">The offset field.</param>
    /// <returns>The offset, or minus one.</returns>
    let private targetAt (table: ReadOnlySpan<byte>) o offset =
        match offset with
        | 0u -> -1
        | _ ->
            let target = int64 o + int64 offset
            match target <= int64 table.Length with
            | true -> int target
            | false -> -1

    /// <summary>The number of records of an array that lie inside the table, at most the count the array names: a sanitized
    /// table holds them all, and a table that was not checked cannot make a decoder build an array larger than itself.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="first">The offset of the first record.</param>
    /// <param name="stride">The size of a record.</param>
    /// <param name="count">The count the array names.</param>
    /// <returns>The number of records to decode.</returns>
    let private fitting (table: ReadOnlySpan<byte>) first stride count =
        match first >= 0 && first <= table.Length with
        | false -> 0
        | true -> int (min count (int64 (table.Length - first) / int64 stride))

    /// <summary>Reads one device an anchor names through an offset field, <c>OffsetTo&lt;Device&gt;::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="anchor">The anchor's offset, which the field is relative to.</param>
    /// <param name="raw">The offset field.</param>
    /// <returns>Whether the device passes its check, and the device, the null device for a null offset.</returns>
    let private anchorDevice (table: ReadOnlySpan<byte>) anchor raw =
        match raw with
        | 0u -> struct (true, NoDevice)
        | _ ->
            let at = int64 anchor + int64 raw
            match Sanitize.device table at Int32.MaxValue with
            | ValueNone -> struct (false, NoDevice)
            | ValueSome _ -> struct (true, DeviceTable.parse table (int at))

    /// <summary>Reads an anchor of format 3, <c>AnchorFormat3</c>: it is found only when both devices pass their check.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The anchor's offset.</param>
    /// <returns>The anchor.</returns>
    let private deviceAnchor (table: ReadOnlySpan<byte>) at =
        let struct (xPasses, xDevice) = anchorDevice table at (uint32 (Binary.u16 table (at + 6)))
        let struct (yPasses, yDevice) = anchorDevice table at (uint32 (Binary.u16 table (at + 8)))
        match xPasses && yPasses with
        | true -> DeviceAnchor (int (Binary.i16 table (at + 2)), int (Binary.i16 table (at + 4)), xDevice, yDevice)
        | false -> NoAnchor

    /// <summary>Reads an anchor of format 1 or 2, whose coordinates are the first two fields after the format.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The anchor's offset.</param>
    /// <param name="size">The anchor's size.</param>
    /// <returns>The anchor, none when it does not fit the table.</returns>
    let private coordinateAnchor (table: ReadOnlySpan<byte>) at size =
        match Sanitize.fits table.Length (int64 at) size with
        | false -> NoAnchor
        | true -> PlainAnchor (int (Binary.i16 table (at + 2)), int (Binary.i16 table (at + 4)))

    /// <summary>Reads the anchor at an offset as <c>Anchor::sanitize</c> accepts it and <c>Anchor::get_anchor</c> reads it.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The anchor's absolute offset.</param>
    /// <returns>The anchor, none when it fails its check.</returns>
    let private anchorAt (table: ReadOnlySpan<byte>) at =
        match Sanitize.fits table.Length at 2 with
        | false -> NoAnchor
        | true ->
            let o = int at
            match Binary.u16 table o with
            | 1us -> coordinateAnchor table o 6
            | 2us -> coordinateAnchor table o 8
            | 3us ->
                match Sanitize.fits table.Length at 10 with
                | true -> deviceAnchor table o
                | false -> NoAnchor
            | _ -> zeroAnchor

    /// <summary>Reads the anchor an offset field names.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="anchorBase">The offset the field is relative to.</param>
    /// <param name="raw">The offset field.</param>
    /// <param name="ofMark">Whether the field is that of a mark record, where a null offset is the Null anchor, the origin.</param>
    /// <returns>The anchor; for a matrix or a cursive record, none for a null offset.</returns>
    let private anchorAtOffset (table: ReadOnlySpan<byte>) anchorBase raw ofMark =
        match raw, ofMark with
        | 0u, true -> zeroAnchor
        | 0u, false -> NoAnchor
        | _ ->
            match anchorAt table (int64 anchorBase + int64 raw) with
            | NoAnchor when ofMark -> zeroAnchor
            | anchor -> anchor

    /// <summary>Decodes the anchors named by a run of offset fields, one every <c>Stride</c> bytes.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private AnchorDecoder =
        { AnchorBase: int
          AnchorFirst: int
          AnchorStride: int
          AnchorWidth: int
          AnchorSlot: int
          OfMark: bool }
        interface ISpanUnfolder<byte, int, Anchor> with
            member this.Next (table, index) =
                let raw = Coverage.readWide table (this.AnchorFirst + index * this.AnchorStride + this.AnchorSlot) this.AnchorWidth
                struct (anchorAtOffset table this.AnchorBase raw this.OfMark, index + 1)

    /// <summary>Decodes the classes of a mark array's records.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ClassDecoder =
        { ClassFirst: int
          ClassStride: int }
        interface ISpanUnfolder<byte, int, uint16> with
            member this.Next (table, index) =
                struct (Binary.u16 table (this.ClassFirst + index * this.ClassStride), index + 1)

    /// <summary>Decodes a mark array, <c>MarkArray</c>: the class and the anchor of each record; a Null array has none.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="array">The array's offset, minus one for the Null array.</param>
    /// <param name="w">The width of the count and of the offsets.</param>
    /// <returns>The classes and the anchors.</returns>
    let private markRecords (table: ReadOnlySpan<byte>) array w =
        match array with
        | -1 -> struct (ImmutableArray<uint16>.Empty, ImmutableArray<Anchor>.Empty)
        | m ->
            let stride = 2 + w
            let count = fitting table (m + w) stride (int64 (Coverage.readWide table m w))
            let classes = Sealed.unfoldSpan table count 0 { ClassDecoder.ClassFirst = m + w; ClassStride = stride }
            let anchors =
                Sealed.unfoldSpan table count 0
                    { AnchorDecoder.AnchorBase = m
                      AnchorFirst = m + w + 2
                      AnchorStride = stride
                      AnchorWidth = w
                      AnchorSlot = 0
                      OfMark = true }
            struct (classes, anchors)

    /// <summary>Decodes the anchors of a matrix, <c>AnchorMatrix</c>, row by row.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="matrix">The matrix's offset.</param>
    /// <param name="rowWidth">The width of the row count.</param>
    /// <param name="w">The width of the anchor offsets.</param>
    /// <param name="count">The number of anchors the rows and columns name.</param>
    /// <returns>The anchors.</returns>
    let private matrixAnchors (table: ReadOnlySpan<byte>) matrix rowWidth w count =
        Sealed.unfoldSpan table (fitting table (matrix + rowWidth) w count) 0
            { AnchorDecoder.AnchorBase = matrix
              AnchorFirst = matrix + rowWidth
              AnchorStride = w
              AnchorWidth = w
              AnchorSlot = 0
              OfMark = false }

    /// <summary>Parses a mark-to-base or mark-to-mark subtable, <c>MarkBasePosFormat1_2</c> and <c>MarkMarkPosFormat1_2</c>,
    /// whose layouts are the same; the base matrix counts its rows in the width of the subtable, the mark matrix in 16 bits.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The format, one or two.</param>
    /// <param name="wideRows">Whether the row count has the width of the subtable's offsets.</param>
    /// <returns>The attachment.</returns>
    let private parseMarkAttachment (table: ReadOnlySpan<byte>) o format wideRows =
        let medium = format = 2
        let covW = match medium with | true -> 4 | false -> Width16
        let w = match medium with | true -> Width24 | false -> Width16
        let rowWidth = match wideRows with | true -> w | false -> Width16
        let markCoverage = coverageAt table o (readLong table (o + 2) covW)
        let targetCoverage = coverageAt table o (readLong table (o + 2 + covW) covW)
        let classCount = int (Binary.u16 table (o + 2 + 2 * covW))
        let struct (classes, anchors) = markRecords table (targetAt table o (readLong table (o + 4 + 2 * covW) covW)) w
        let matrix = targetAt table o (readLong table (o + 4 + 3 * covW) covW)
        let rows = match matrix with | -1 -> 0 | m -> int (Coverage.readWide table m rowWidth)
        { MarkCoverage = markCoverage
          MarkDirect = directCoverage markCoverage
          TargetCoverage = targetCoverage
          TargetDirect = directCoverage targetCoverage
          ClassCount = classCount
          MarkClasses = classes
          MarkAnchors = anchors
          Rows = rows
          Anchors = (match matrix with | -1 -> ImmutableArray<Anchor>.Empty | m -> matrixAnchors table m rowWidth w (int64 rows * int64 classCount)) }

    /// <summary>One ligature's matrix, decoded: its component count and its anchors.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private DecodedLigature =
        { LigatureRows: int
          LigatureAnchors: ImmutableArray<Anchor> }

    /// <summary>Decodes the ligature attach tables of a ligature array.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LigatureDecoder =
        { ArrayBase: int
          LigaturesFirst: int
          LigatureClasses: int
          LigatureWidth: int }
        interface ISpanUnfolder<byte, int, DecodedLigature> with
            member this.Next (table, index) =
                let w = this.LigatureWidth
                match targetAt table this.ArrayBase (Coverage.readWide table (this.LigaturesFirst + index * w) w) with
                | -1 -> struct ({ LigatureRows = 0; LigatureAnchors = ImmutableArray<Anchor>.Empty }, index + 1)
                | matrix ->
                    let rows = int (Binary.u16 table matrix)
                    struct ({ LigatureRows = rows; LigatureAnchors = matrixAnchors table matrix Width16 w (int64 rows * int64 this.LigatureClasses) }, index + 1)

    /// <summary>Parses a mark-to-ligature subtable, <c>MarkLigPosFormat1_2</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The format, one or two.</param>
    /// <returns>The attachment.</returns>
    let private parseMarkLigature (table: ReadOnlySpan<byte>) o format =
        let medium = format = 2
        let covW = match medium with | true -> 4 | false -> Width16
        let w = match medium with | true -> Width24 | false -> Width16
        let markCoverage = coverageAt table o (readLong table (o + 2) covW)
        let ligatureCoverage = coverageAt table o (readLong table (o + 2 + covW) covW)
        let classCount = int (Binary.u16 table (o + 2 + 2 * covW))
        let struct (classes, anchors) = markRecords table (targetAt table o (readLong table (o + 4 + 2 * covW) covW)) w
        let ligatures =
            match targetAt table o (readLong table (o + 4 + 3 * covW) covW) with
            | -1 -> ImmutableArray<DecodedLigature>.Empty
            | array ->
                let count = fitting table (array + w) w (int64 (Coverage.readWide table array w))
                Sealed.unfoldSpan table count 0 { LigatureDecoder.ArrayBase = array; LigaturesFirst = array + w; LigatureClasses = classCount; LigatureWidth = w }
        let matrices = Sealed.map (fun (ligature: DecodedLigature) -> ligature.LigatureAnchors) (ligatures.AsSpan())
        { LigMarkCoverage = markCoverage
          LigMarkDirect = directCoverage markCoverage
          LigatureCoverage = ligatureCoverage
          LigatureDirect = directCoverage ligatureCoverage
          LigClassCount = classCount
          LigMarkClasses = classes
          LigMarkAnchors = anchors
          Components = Sealed.map (fun (ligature: DecodedLigature) -> ligature.LigatureRows) (ligatures.AsSpan())
          Starts = Sealed.prefixLengths matrices
          LigAnchors = Sealed.flatten matrices }

    /// <summary>Decodes the entry or the exit anchors of the records of a cursive subtable.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="first">The offset of the first record.</param>
    /// <param name="count">The number of records.</param>
    /// <param name="w">The width of the offsets.</param>
    /// <param name="slot">The byte offset of the anchor within a record.</param>
    /// <returns>The anchors.</returns>
    let private cursiveAnchors (table: ReadOnlySpan<byte>) o first count w slot =
        Sealed.unfoldSpan table count 0
            { AnchorDecoder.AnchorBase = o
              AnchorFirst = first
              AnchorStride = 2 * w
              AnchorWidth = w
              AnchorSlot = slot
              OfMark = false }

    /// <summary>Parses a cursive attachment subtable, <c>CursivePosFormat1_2</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The format, one or two.</param>
    /// <returns>The attachment.</returns>
    let private parseCursive (table: ReadOnlySpan<byte>) o format =
        let medium = format = 2
        let covW = match medium with | true -> 4 | false -> Width16
        let w = match medium with | true -> Width24 | false -> Width16
        let coverage = coverageAt table o (readLong table (o + 2) covW)
        let first = o + 2 + covW + w
        let count = fitting table first (2 * w) (int64 (Coverage.readWide table (o + 2 + covW) w))
        { CursiveCoverage = coverage
          CursiveDirect = directCoverage coverage
          Entries = cursiveAnchors table o first count w 0
          Exits = cursiveAnchors table o first count w w }

    /// <summary>Parses a subtable of one of the attachment types, or the pending subtable of a format that is none of theirs.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="lookupType">The lookup type, three to six.</param>
    /// <returns>The subtable.</returns>
    let private parseAttachment (table: ReadOnlySpan<byte>) o lookupType =
        let format = int (Binary.u16 table o)
        match format, lookupType with
        | 1, CursiveType | 2, CursiveType -> CursivePos (parseCursive table o format)
        | 1, MarkBaseType | 2, MarkBaseType -> MarkBasePos (parseMarkAttachment table o format true)
        | 1, MarkLigType | 2, MarkLigType -> MarkLigPos (parseMarkLigature table o format)
        | 1, MarkMarkType | 2, MarkMarkType -> MarkMarkPos (parseMarkAttachment table o format false)
        | _ ->
            let struct (coverage, second) = SubtableDigest.collect table o lookupType false
            PosPending (lookupType, format, coverage, second)

    /// <summary>Parses one GPOS subtable, unwrapping extensions to the extended type.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The subtable offset; negative for the Null subtable of a zero offset.</param>
    /// <param name="lookupType">The lookup's type.</param>
    /// <returns>The subtable.</returns>
    let rec parse (table: ReadOnlySpan<byte>) offset lookupType =
        match lookupType with
        | _ when offset < 0 -> PosPending (lookupType, 0, Digest.empty, Digest.full)
        | ExtensionType ->
            let innerType = int (Binary.u16 table (offset + 2))
            match Binary.u32 table (offset + 4) with
            | 0u -> PosPending (ExtensionType, 1, Digest.empty, Digest.full)
            | innerOffset -> parse table (offset + int innerOffset) innerType
        | SingleType -> parseSingle table offset
        | PairType -> parsePair table offset
        | CursiveType | MarkBaseType | MarkLigType | MarkMarkType -> parseAttachment table offset lookupType
        | ContextType ->
            match GsubTables.contextRules table offset with
            | ValueSome rules -> PosContext rules
            | ValueNone -> PosPending (ContextType, int (Binary.u16 table offset), Digest.empty, Digest.full)
        | ChainType ->
            match GsubTables.chainRules table offset with
            | ValueSome rules -> PosChainContext rules
            | ValueNone -> PosPending (ChainType, int (Binary.u16 table offset), Digest.empty, Digest.full)
        | _ ->
            let format = int (Binary.u16 table offset)
            let struct (coverage, second) = SubtableDigest.collect table offset lookupType false
            PosPending (lookupType, format, coverage, second)

    /// <summary>The coverage digest of a parsed GPOS subtable, <c>get_coverage ().collect_coverage</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The digest.</returns>
    let subtableDigest subtable =
        match subtable with
        | SinglePos (coverage, _, _, _, _, _)
        | PairPosGlyphs (coverage, _, _, _, _, _, _, _, _)
        | PairPosClasses (coverage, _, _, _, _, _, _, _, _, _, _, _, _) -> Coverage.digest coverage
        | CursivePos cursive -> Coverage.digest cursive.CursiveCoverage
        | MarkBasePos attachment
        | MarkMarkPos attachment -> Coverage.digest attachment.MarkCoverage
        | MarkLigPos ligature -> Coverage.digest ligature.LigMarkCoverage
        | PosContext rules -> GsubTables.contextDigest rules
        | PosChainContext rules -> GsubTables.chainDigest rules
        | PosPending (_, _, coverage, _) -> coverage

    /// <summary>The second-glyph digest of a parsed GPOS subtable, <c>collect_second_glyphs</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The digest, full when the subtable collects no second glyph.</returns>
    let secondDigest subtable =
        match subtable with
        | SinglePos _ -> Digest.full
        | PairPosGlyphs (_, _, _, _, _, _, _, second, _)
        | PairPosClasses (_, _, _, _, _, _, _, _, _, second, _, _, _)
        | PosPending (_, _, _, second) -> second
        | CursivePos cursive -> Coverage.digest cursive.CursiveCoverage
        | MarkBasePos attachment
        | MarkMarkPos attachment -> Coverage.digest attachment.TargetCoverage
        | MarkLigPos ligature -> Coverage.digest ligature.LigatureCoverage
        | PosContext rules -> GsubTables.contextSecondDigest rules
        | PosChainContext rules -> GsubTables.chainSecondDigest rules
