namespace Farfbuzz

open System.Collections.Immutable

/// <summary>Compiling pair subtables into the flat arrays the pair loops of <see cref="Sealed"/> read.</summary>
[<RequireQualifiedAccess>]
module internal PairTables =
    /// <summary>The first value format of the records the pair loops decode: a horizontal advance for the first glyph.</summary>
    [<Literal>]
    let private AdvanceOnly = 4us

    /// <summary>The first value format of the records of a table that carries a device for the advance: a horizontal advance and its device.</summary>
    [<Literal>]
    let private AdvanceDevice = 0x44us

    /// <summary>Whether a subtable is a pair table whose records are a horizontal advance and its device, which a font with neither ppem nor
    /// non-zero coordinates reads as the advance alone, <c>ValueFormat::apply_value</c> reading devices only then.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>True for a format 1 or format 2 pair table with first format 0x44 and no second record.</returns>
    let carriesDevices (subtable: PosSubtable) =
        match subtable with
        | PairPosGlyphs (_, format1, format2, _, _, _, _, _, _) -> format1 = AdvanceDevice && format2 = 0us
        | PairPosClasses (_, format1, format2, _, _, _, _, _, _, _, _, _, _) -> format1 = AdvanceDevice && format2 = 0us
        | SinglePos _ -> false
        | CursivePos _ -> false
        | MarkBasePos _ -> false
        | MarkLigPos _ -> false
        | MarkMarkPos _ -> false
        | PosContext _ -> false
        | PosChainContext _ -> false
        | PosPending _ -> false

    /// <summary>The advances of records that are an advance and a device, one field each: every other field of the value array.</summary>
    /// <param name="values">The value fields, two for each record.</param>
    /// <returns>The advance of each record.</returns>
    let private advancesOf (values: ImmutableArray<int16>) =
        Sealed.init (values.Length / 2) (fun record -> values[2 * record])

    /// <summary>The pair lookups of a plan that has none: nothing for the lean loop, no compiled table.</summary>
    let none =
        { Tables = ImmutableArray<ImmutableArray<PairTable>>.Empty
          PositionCovered = ImmutableArray<ImmutableArray<uint64>>.Empty
          PositionTables = ImmutableArray<ImmutableArray<PairTable>>.Empty
          Glyphs = ImmutableArray<uint64>.Empty
          Enabled = false
          Lean = 0UL
          LeanOnly = false
          Idle = 0UL
          Marks = ImmutableArray<ImmutableArray<MarkTable voption>>.Empty
          Targets = ImmutableArray<ImmutableArray<uint64> voption>.Empty
          Inert = ValueNone
          Resolved = ValueNone }

    /// <summary>A table of the general kind, which holds nothing.</summary>
    let private general =
        { Kind = Sealed.PairKindGeneral
          Plain = false
          Cover = ImmutableArray<uint16>.Empty
          Keys = ImmutableArray<uint32>.Empty
          Slots = ImmutableArray<int>.Empty
          Shift = 0
          Filter = ImmutableArray<uint64>.Empty
          FilterShift = 0
          Columns = ImmutableArray<uint16>.Empty
          BeyondColumn = Sealed.PairNoColumn
          Width = 0
          Values = ImmutableArray<int16>.Empty
          Totals = ImmutableArray<int64>.Empty }

    /// <summary>The row of a first glyph in a format 2 table: its class when covered and below the class count.</summary>
    /// <param name="cover">The coverage's direct table.</param>
    /// <param name="first">The first classes' direct table.</param>
    /// <param name="class1Count">The number of first classes.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The row.</returns>
    let private rowOf (cover: ImmutableArray<uint16>) (first: ImmutableArray<uint16>) (class1Count: int) (glyph: int) =
        match cover[glyph] = Sealed.PairNotCovered with
        | true -> Sealed.PairNotCovered
        | false ->
            let klass = match glyph < first.Length with | true -> first[glyph] | false -> 0us
            match int klass < class1Count with
            | true -> klass
            | false -> Sealed.PairNoRow

    /// <summary>The column of a second glyph in a format 2 table: its class when below the class count.</summary>
    /// <param name="second">The second classes' direct table.</param>
    /// <param name="class2Count">The number of second classes.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The column.</returns>
    let private columnOf (second: ImmutableArray<uint16>) (class2Count: int) (glyph: int) =
        match int second[glyph] < class2Count with
        | true -> second[glyph]
        | false -> Sealed.PairNoColumn

    /// <summary>Compiles one pair subtable.</summary>
    /// <param name="inert">Whether the font reading the table has neither ppem nor non-zero coordinates, so that the devices of the
    /// records are not read: a table of records that are an advance and a device is then plain.</param>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The compiled table, of the general kind for a subtable the arrays cannot represent exactly.</returns>
    let private compileFor (inert: bool) (subtable: PosSubtable) =
        match subtable with
        | PairPosGlyphs (coverage, format1, format2, starts, seconds, values, _, _, direct) ->
            match direct.Complete with
            | false -> general
            | true ->
                match Sealed.pairIndexOfSets ((Coverage.glyphs coverage).AsSpan()) (direct.Table.AsSpan()) (starts.AsSpan()) (seconds.AsSpan()) with
                | ValueNone -> general
                | ValueSome (struct (keys, slots, shift, filter, filterShift)) ->
                    { general with
                        Kind = Sealed.PairKindGlyphs
                        Plain = (format1 = AdvanceOnly || (inert && format1 = AdvanceDevice)) && format2 = 0us
                        Cover = direct.Table
                        Keys = keys
                        Slots = slots
                        Shift = shift
                        Filter = filter
                        FilterShift = filterShift
                        Values = (match inert && format1 = AdvanceDevice && format2 = 0us with | true -> advancesOf values | false -> values) }
        | PairPosClasses (_, format1, format2, _, _, class1Count, class2Count, values, _, _, direct, firstDirect, secondDirect) ->
            match direct.Complete && firstDirect.Complete && secondDirect.Complete && class1Count < int Sealed.PairNoRow && class2Count < int Sealed.PairNoColumn with
            | false -> general
            | true ->
                { general with
                    Kind = Sealed.PairKindClasses
                    Plain = (format1 = AdvanceOnly || (inert && format1 = AdvanceDevice)) && format2 = 0us
                    Cover = Sealed.init direct.Table.Length (rowOf direct.Table firstDirect.Table class1Count)
                    Columns = Sealed.init secondDirect.Table.Length (columnOf secondDirect.Table class2Count)
                    BeyondColumn = (match class2Count > 0 with | true -> 0us | false -> Sealed.PairNoColumn)
                    Width = class2Count
                    Values = (match inert && format1 = AdvanceDevice && format2 = 0us with | true -> advancesOf values | false -> values) }
        | SinglePos _ -> general
        | CursivePos _ -> general
        | MarkBasePos _ -> general
        | MarkLigPos _ -> general
        | MarkMarkPos _ -> general
        | PosContext _ -> general
        | PosChainContext _ -> general
        | PosPending _ -> general

    /// <summary>Compiles one pair subtable.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The compiled table, of the general kind for a subtable the arrays cannot represent exactly.</returns>
    let compile (subtable: PosSubtable) =
        compileFor false subtable

    /// <summary>Compiles the subtables of a lookup.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <returns>The compiled tables, in order.</returns>
    let compileAll (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) =
        Sealed.map (fun (accel: SubtableAccel<PosSubtable>) -> compile accel.Subtable) (subtables.AsSpan())

    /// <summary>Whether every table of a lookup is compiled into arrays and has plain records, so that the lean loop never meets
    /// a pair it cannot classify.</summary>
    /// <param name="tables">The tables.</param>
    /// <param name="index">The next table.</param>
    /// <returns>True when no table from the index on is general or has records that are not plain.</returns>
    [<TailCall>]
    let rec allPlain (tables: ImmutableArray<PairTable>) (index: int) =
        match index < tables.Length with
        | false -> true
        | true ->
            match tables[index].Kind <> Sealed.PairKindGeneral && tables[index].Plain with
            | true -> allPlain tables (index + 1)
            | false -> false

    /// <summary>Whether every subtable of a lookup is a pair adjustment, from a subtable on.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when no subtable from the index on is anything else.</returns>
    [<TailCall>]
    let rec onlyPairs (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) (index: int) =
        match index < subtables.Length with
        | false -> true
        | true ->
            match subtables[index].Subtable with
            | PairPosGlyphs _ -> onlyPairs subtables (index + 1)
            | PairPosClasses _ -> onlyPairs subtables (index + 1)
            | SinglePos _ -> false
            | CursivePos _ -> false
            | MarkBasePos _ -> false
            | MarkLigPos _ -> false
            | MarkMarkPos _ -> false
            | PosContext _ -> false
            | PosChainContext _ -> false
            | PosPending _ -> false

    /// <summary>The compiled pair subtables of one GPOS lookup: empty for a lookup that has a subtable that is no pair adjustment.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <returns>The compiled subtables, in order.</returns>
    let ofLookup (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) =
        match onlyPairs subtables 0 with
        | false -> ImmutableArray<PairTable>.Empty
        | true -> compileAll subtables

    /// <summary>Whether a lookup has a pair table whose records carry devices a font may ignore, from a subtable on.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when a subtable from the index on is one.</returns>
    [<TailCall>]
    let rec anyDevices (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) (index: int) =
        match index < subtables.Length with
        | false -> false
        | true ->
            match carriesDevices subtables[index].Subtable with
            | true -> true
            | false -> anyDevices subtables (index + 1)

    /// <summary>The compiled pair subtables of one GPOS lookup for a font whose devices do nothing, the tables of
    /// <see cref="ofLookup"/> with the records that are an advance and a device read as the advance.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <returns>The compiled subtables, in order, empty for a lookup that has a subtable that is no pair adjustment.</returns>
    let ofLookupInert (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) =
        match onlyPairs subtables 0 with
        | false -> ImmutableArray<PairTable>.Empty
        | true -> Sealed.map (fun (accel: SubtableAccel<PosSubtable>) -> compileFor true accel.Subtable) (subtables.AsSpan())

    /// <summary>The total of a record that cannot be resolved: a device that reads the ppem, or an advance or delta too large to add without saturating.</summary>
    [<Literal>]
    let private Unresolved = System.Int64.MinValue

    /// <summary>The largest scaled advance or delta a resolved record holds, so that adding the two never saturates where the general loop adds them one after the other.</summary>
    [<Literal>]
    let private ResolvedLimit = 16777216

    /// <summary>The total of one record that is an advance and its device, for the font it is resolved for.</summary>
    /// <param name="advanceOf">Scales a design advance, <c>em_scale_x</c>.</param>
    /// <param name="deltaOf">The scaled delta of a device, none for a device that depends on the ppem.</param>
    /// <param name="values">The value fields, two for each record: the advance and the device's offset.</param>
    /// <param name="devices">The devices, parallel to the fields.</param>
    /// <param name="record">The record's number.</param>
    /// <returns>The advance, devices included, shifted left once with the lowest bit set when a field read was non-zero, or <see cref="Unresolved"/>.</returns>
    let private totalOf (advanceOf: int -> int) (deltaOf: Device -> int voption) (values: ImmutableArray<int16>) (devices: ImmutableArray<Device>) (record: int) =
        let advance = advanceOf (int values[2 * record])
        let worked = values[2 * record] <> 0s || values[2 * record + 1] <> 0s
        match deltaOf devices[2 * record + 1] with
        | ValueNone -> Unresolved
        | ValueSome delta ->
            match abs (int64 advance) > int64 ResolvedLimit || abs (int64 delta) > int64 ResolvedLimit with
            | true -> Unresolved
            | false -> (int64 (advance + delta) <<< 1) ||| (match worked with | true -> 1L | false -> 0L)

    /// <summary>Resolves the records of a compiled table whose records are an advance and a device: the table of the resolved kind, or the
    /// general table when a record cannot be resolved.</summary>
    /// <param name="advanceOf">Scales a design advance.</param>
    /// <param name="deltaOf">The scaled delta of a device, none for one that depends on the ppem.</param>
    /// <param name="values">The value fields.</param>
    /// <param name="devices">The devices, parallel to the fields.</param>
    /// <param name="compiled">The table compiled with its records raw.</param>
    /// <returns>The resolved table.</returns>
    let private resolve (advanceOf: int -> int) (deltaOf: Device -> int voption) (values: ImmutableArray<int16>) (devices: ImmutableArray<Device>) (compiled: PairTable) =
        let totals = Sealed.init (values.Length / 2) (totalOf advanceOf deltaOf values devices)
        match compiled.Kind, Spans.exists (fun total -> total = Unresolved) (totals.AsSpan()) with
        | _, true -> general
        | Sealed.PairKindGlyphs, false -> { compiled with Kind = Sealed.PairKindGlyphsResolved; Plain = true; Values = ImmutableArray<int16>.Empty; Totals = totals }
        | Sealed.PairKindClasses, false -> { compiled with Kind = Sealed.PairKindClassesResolved; Plain = true; Values = ImmutableArray<int16>.Empty; Totals = totals }
        | _, false -> general

    /// <summary>Compiles one pair subtable for a font that reads devices: a table of records that are an advance and a device has the
    /// device deltas of the font folded into its records, and any other subtable is compiled as <see cref="compile"/> does.</summary>
    /// <param name="advanceOf">Scales a design advance, <c>em_scale_x</c>.</param>
    /// <param name="deltaOf">The scaled delta of a device for the font, none for a device that depends on the ppem.</param>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The compiled table.</returns>
    let private compileResolved (advanceOf: int -> int) (deltaOf: Device -> int voption) (subtable: PosSubtable) =
        match subtable with
        | PairPosGlyphs (_, format1, format2, _, _, values, devices, _, _) when format1 = AdvanceDevice && format2 = 0us && devices.Length = values.Length ->
            resolve advanceOf deltaOf values devices (compile subtable)
        | PairPosClasses (_, format1, format2, _, _, _, _, values, devices, _, _, _, _) when format1 = AdvanceDevice && format2 = 0us && devices.Length = values.Length ->
            resolve advanceOf deltaOf values devices (compile subtable)
        | PairPosGlyphs _ -> compile subtable
        | PairPosClasses _ -> compile subtable
        | SinglePos _ -> general
        | CursivePos _ -> general
        | MarkBasePos _ -> general
        | MarkLigPos _ -> general
        | MarkMarkPos _ -> general
        | PosContext _ -> general
        | PosChainContext _ -> general
        | PosPending _ -> general

    /// <summary>The compiled pair subtables of one GPOS lookup for a font that reads devices (a ppem or non-zero coordinates), the records
    /// that carry a variation device holding the advance with the font's delta added, rounded as <c>apply_value</c> rounds each.</summary>
    /// <param name="advanceOf">Scales a design advance, <c>em_scale_x</c>.</param>
    /// <param name="deltaOf">The scaled delta of a device for the font, none for a device that depends on the ppem.</param>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <returns>The compiled subtables, in order, empty for a lookup that has a subtable that is no pair adjustment.</returns>
    let ofLookupResolved (advanceOf: int -> int) (deltaOf: Device -> int voption) (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) =
        match onlyPairs subtables 0 with
        | false -> ImmutableArray<PairTable>.Empty
        | true -> Sealed.map (fun (accel: SubtableAccel<PosSubtable>) -> compileResolved advanceOf deltaOf accel.Subtable) (subtables.AsSpan())
