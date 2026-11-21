namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Numerics
open System.Runtime.CompilerServices

/// <summary>What the pure decisions of the GPOS lookups read besides the run: the font, the direction, the matcher of the
/// lookup's skipping iterator and the GDEF, held once for the lookup, a struct so that building it allocates nothing.</summary>
[<Struct>]
type internal PosEnv =
    { Font: Font
      Direction: Direction
      Horizontal: bool
      Matcher: Matcher
      Gdef: Gdef }

/// <summary>The value records and the single and pair adjustment lookups of GPOS, <c>ValueFormat::apply_value</c>,
/// <c>SinglePosFormat1_3</c>, <c>SinglePosFormat2_4</c>, <c>PairPosFormat1_3</c>, <c>PairPosFormat2_4</c> and
/// <c>PairSet::apply</c>, as functions of their arguments that return what to write and never write it.</summary>
[<RequireQualifiedAccess>]
module internal GposLookups =
    /// <summary>The value-format bits that name a value rather than a device.</summary>
    [<Literal>]
    let private ValueBits = 0x000F

    /// <summary>The value-format bits that name a device, <c>ValueFormat::devices</c>.</summary>
    [<Literal>]
    let private DeviceBits = 0x00F0

    /// <summary>The answer of a direct table for a glyph that is not covered.</summary>
    [<Literal>]
    let private NoAnswer = 0xFFFFus

    /// <summary>Adds two positions with int saturation, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The augend.</param>
    /// <param name="b">The addend.</param>
    /// <returns>The saturating sum.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let saturateAdd a b =
        let sum = int64 a + int64 b
        match sum > int64 Int32.MaxValue, sum < int64 Int32.MinValue with
        | true, _ -> Int32.MaxValue
        | _, true -> Int32.MinValue
        | _ -> int sum

    /// <summary>Subtracts two positions with int saturation, <c>hb_saturate_sub</c>.</summary>
    /// <param name="a">The minuend.</param>
    /// <param name="b">The subtrahend.</param>
    /// <returns>The saturating difference.</returns>
    let saturateSub a b =
        let difference = int64 a - int64 b
        match difference > int64 Int32.MaxValue, difference < int64 Int32.MinValue with
        | true, _ -> Int32.MaxValue
        | _, true -> Int32.MinValue
        | _ -> int difference

    /// <summary>The index of a field of a value record: the number of fields the format has below its bit.</summary>
    /// <param name="format">The value format.</param>
    /// <param name="bit">The bit of the field.</param>
    /// <param name="at">The index of the record's first field.</param>
    /// <returns>The field's index.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private fieldAt (format: int) (bit: int) (at: int) =
        at + BitOperations.PopCount (uint32 (format &&& (bit - 1)))

    /// <summary>Applies the placement and advance fields of a value record, <c>apply_value</c> before its devices.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="format">The value format.</param>
    /// <param name="values">The value fields.</param>
    /// <param name="at">The index of the record's first field.</param>
    /// <param name="position">The glyph's position.</param>
    /// <returns>The new position and whether a field read was non-zero.</returns>
    let private applyPlain (env: PosEnv) (format: int) (values: ImmutableArray<int16>) (at: int) (position: GlyphPosition) =
        let horizontal = env.Horizontal
        let xPlacement = format &&& 1 <> 0
        let yPlacement = format &&& 2 <> 0
        let xAdvance = horizontal && format &&& 4 <> 0
        let yAdvance = (not horizontal) && format &&& 8 <> 0
        let xValue = match xPlacement with | true -> values[fieldAt format 1 at] | false -> 0s
        let yValue = match yPlacement with | true -> values[fieldAt format 2 at] | false -> 0s
        let xAdvanceValue = match xAdvance with | true -> values[fieldAt format 4 at] | false -> 0s
        let yAdvanceValue = match yAdvance with | true -> values[fieldAt format 8 at] | false -> 0s
        let moved =
            { position with
                XOffset = (match xPlacement with | true -> saturateAdd position.XOffset (Font.emScaleX env.Font (int xValue)) | false -> position.XOffset)
                YOffset = (match yPlacement with | true -> saturateAdd position.YOffset (Font.emScaleY env.Font (int yValue)) | false -> position.YOffset)
                XAdvance = (match xAdvance with | true -> saturateAdd position.XAdvance (Font.emScaleX env.Font (int xAdvanceValue)) | false -> position.XAdvance)
                YAdvance = (match yAdvance with | true -> saturateSub position.YAdvance (Font.emScaleY env.Font (int yAdvanceValue)) | false -> position.YAdvance) }
        struct (moved, xValue <> 0s || yValue <> 0s || xAdvanceValue <> 0s || yAdvanceValue <> 0s)

    /// <summary>Applies the device fields of a value record, <c>apply_value</c> after the placement and advance fields:
    /// only when the font has a ppem or non-zero coordinates, the field counts as read when its offset is non-zero.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="format">The value format.</param>
    /// <param name="values">The value fields.</param>
    /// <param name="devices">The devices, parallel to the fields.</param>
    /// <param name="at">The index of the record's first field.</param>
    /// <param name="position">The glyph's position after the plain fields.</param>
    /// <param name="worked">Whether a plain field was non-zero.</param>
    /// <returns>The new position and whether a field read was non-zero.</returns>
    let private applyDevices (env: PosEnv) (format: int) (values: ImmutableArray<int16>) (devices: ImmutableArray<Device>) (at: int) (position: GlyphPosition) (worked: bool) =
        let font = env.Font
        let useX = font.XPpem <> 0u || font.Variations.Coords.HasNonZero
        let useY = font.YPpem <> 0u || font.Variations.Coords.HasNonZero
        match useX || useY with
        | false -> struct (position, worked)
        | true ->
            let horizontal = env.Horizontal
            let xPla = useX && format &&& 0x10 <> 0
            let yPla = useY && format &&& 0x20 <> 0
            let xAdv = horizontal && useX && format &&& 0x40 <> 0
            let yAdv = (not horizontal) && useY && format &&& 0x80 <> 0
            let iXPla = fieldAt format 0x10 at
            let iYPla = fieldAt format 0x20 at
            let iXAdv = fieldAt format 0x40 at
            let iYAdv = fieldAt format 0x80 at
            let moved =
                { position with
                    XOffset = (match xPla with | true -> saturateAdd position.XOffset (Device.xDelta font devices[iXPla]) | false -> position.XOffset)
                    YOffset = (match yPla with | true -> saturateAdd position.YOffset (Device.yDelta font devices[iYPla]) | false -> position.YOffset)
                    XAdvance = (match xAdv with | true -> saturateAdd position.XAdvance (Device.xDelta font devices[iXAdv]) | false -> position.XAdvance)
                    YAdvance = (match yAdv with | true -> saturateSub position.YAdvance (Device.yDelta font devices[iYAdv]) | false -> position.YAdvance) }
            let read =
                (xPla && values[iXPla] <> 0s) || (yPla && values[iYPla] <> 0s) || (xAdv && values[iXAdv] <> 0s) || (yAdv && values[iYAdv] <> 0s)
            struct (moved, worked || read)

    /// <summary>Applies a value record to a position, <c>ValueFormat::apply_value</c>: placements always, the advance of the
    /// direction (the vertical one negated), then the devices.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="format">The value format.</param>
    /// <param name="values">The value fields.</param>
    /// <param name="devices">The devices, parallel to the fields, empty when no format names a device.</param>
    /// <param name="at">The index of the record's first field.</param>
    /// <param name="position">The glyph's position.</param>
    /// <returns>The new position and whether a field read was non-zero, <c>worked</c>.</returns>
    let applyValue (env: PosEnv) (format: uint16) (values: ImmutableArray<int16>) (devices: ImmutableArray<Device>) (at: int) (position: GlyphPosition) =
        let f = int format
        match f &&& (ValueBits ||| DeviceBits) with
        | 0 -> struct (position, false)
        | _ ->
            let struct (plain, worked) = applyPlain env f values at position
            match f &&& DeviceBits with
            | 0 -> struct (plain, worked)
            | _ -> applyDevices env f values devices at plain worked

    /// <summary>A subtable's answer when it did not match the glyph at all, which records nothing.</summary>
    let private untouched =
        PosOutcome(false, 0, Unchecked.defaultof<GlyphPosition>, false, 0, Unchecked.defaultof<GlyphPosition>, false, 0, 0, 0)

    /// <summary>A pair that did not apply, with the unsafe-to-concat range that records the attempt.</summary>
    /// <param name="concatTo">The end of the range.</param>
    /// <returns>The outcome.</returns>
    let private missing (concatTo: int) =
        PosOutcome(false, 0, Unchecked.defaultof<GlyphPosition>, false, 0, Unchecked.defaultof<GlyphPosition>, false, concatTo, 0, 0)

    /// <summary>The coverage index of a glyph through a direct table, <c>Coverage::get_coverage</c>.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <param name="direct">Its direct table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The index, or -1 when the glyph is not covered.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let coverageIndex (coverage: Coverage) (direct: DirectIndex) glyph =
        match glyph < uint32 direct.Table.Length with
        | true ->
            let answer = direct.Table[int glyph]
            match answer = NoAnswer with
            | true -> -1
            | false -> int answer
        | false ->
            match direct.Complete with
            | true -> -1
            | false ->
                match Coverage.index coverage glyph with
                | Covered index -> index
                | NotCovered -> -1

    /// <summary>The class of a glyph through a direct table, <c>ClassDef::get_class</c>.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <param name="direct">Its direct table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The class.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private classOf (classDef: ClassDef) (direct: DirectIndex) (glyph: uint32) =
        match glyph < uint32 direct.Table.Length with
        | true -> uint32 direct.Table[int glyph]
        | false ->
            match direct.Complete with
            | true -> 0u
            | false -> ClassDef.classOf classDef glyph

    /// <summary>Applies one single-adjustment subtable to the glyph at an index, <c>SinglePosFormat1_3::apply</c> and
    /// <c>SinglePosFormat2_4::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The outcome.</returns>
    let applySingle (env: PosEnv) (subtable: PosSubtable) (infos: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) =
        match subtable with
        | SinglePos (coverage, format, count, values, devices, direct) ->
            let covered = coverageIndex coverage direct infos[index].Codepoint
            match covered < 0 || (count >= 0 && covered >= count) with
            | true -> untouched
            | false ->
                let at = match count < 0 with | true -> 0 | false -> covered * GposTables.valueLength format
                let struct (moved, worked) = applyValue env format values devices at positions[index]
                PosOutcome(true, index + 1, moved, worked, 0, Unchecked.defaultof<GlyphPosition>, false, 0, 0, 0)
        | PairPosGlyphs _ -> untouched
        | PairPosClasses _ -> untouched
        | CursivePos _ -> untouched
        | MarkBasePos _ -> untouched
        | MarkLigPos _ -> untouched
        | MarkMarkPos _ -> untouched
        | PosContext _ -> untouched
        | PosChainContext _ -> untouched
        | PosPending _ -> untouched

    /// <summary>Finds the glyph a pair is made with, <c>skippy_iter.next</c> of the input iterator: the glyph right after
    /// the first when the lookup skips nothing and the glyph is no default ignorable, else the iterator's walk.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The first glyph's index.</param>
    /// <returns>The hit, or the bound of the unsafe-to range of a miss.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private nextGlyph (env: PosEnv) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (index: int) =
        let next = index + 1
        match next < infos.Length && env.Matcher.LookupProps = 0u && not (UnicodeProps.isDefaultIgnorable props[next]) with
        | false -> Matching.next env.Matcher env.Gdef (NoMatchFunc()) 0 infos props index infos.Length
        | true ->
            match infos[next].Mask &&& env.Matcher.Mask <> 0u with
            | true -> Probe.Hit next
            | false -> Probe.Miss (next + 1)

    /// <summary>Applies the value records of a matched pair and builds the outcome in the general case, the tail of <c>PairSet::apply</c> and
    /// <c>PairPosFormat2_4::apply</c>: the first record goes to the glyph, the second to its partner, a pair that changed
    /// nothing is a concat hazard, and a second record consumes the partner.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="format1">The first value format.</param>
    /// <param name="format2">The second value format.</param>
    /// <param name="values">The value fields.</param>
    /// <param name="devices">The devices, parallel to the fields.</param>
    /// <param name="at">The index of the record's first field.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="second">The partner's index.</param>
    /// <returns>The outcome.</returns>
    let private pairOutcomeFull (env: PosEnv) (format1: uint16) (format2: uint16) (values: ImmutableArray<int16>) (devices: ImmutableArray<Device>) (at: int) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (second: int) =
        let length1 = GposTables.valueLength format1
        let length2 = GposTables.valueLength format2
        let struct (first, firstWorked) =
            match length1 with
            | 0 -> struct (positions[index], false)
            | _ -> applyValue env format1 values devices at positions[index]
        let struct (partner, secondWorked) =
            match length2 with
            | 0 -> struct (positions[second], false)
            | _ -> applyValue env format2 values devices (at + length1) positions[second]
        let applied = firstWorked || secondWorked
        let concatTo = match applied with | true -> 0 | false -> second + 1
        let break1To = match applied with | true -> second + 1 | false -> 0
        match length2 with
        | 0 -> PosOutcome(true, second, first, firstWorked, second, partner, secondWorked, concatTo, break1To, 0)
        | _ -> PosOutcome(true, second + 1, first, firstWorked, second, partner, secondWorked, concatTo, break1To, second + 2)

    /// <summary>Builds the outcome of a matched pair: the common pair whose only value is a horizontal advance for the first
    /// glyph is answered by the advance alone, which is exactly what <c>apply_value</c> adds for it, every other pair by
    /// <see cref="pairOutcomeFull"/>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="format1">The first value format.</param>
    /// <param name="format2">The second value format.</param>
    /// <param name="values">The value fields.</param>
    /// <param name="devices">The devices, parallel to the fields.</param>
    /// <param name="at">The index of the record's first field.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="second">The partner's index.</param>
    /// <returns>The outcome.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private pairOutcome (env: PosEnv) (format1: uint16) (format2: uint16) (values: ImmutableArray<int16>) (devices: ImmutableArray<Device>) (at: int) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (second: int) =
        match format1 = 4us && format2 = 0us && env.Horizontal with
        | true ->
            match values[at] with
            | 0s -> PosOutcome(true, second, Unchecked.defaultof<GlyphPosition>, false, second, Unchecked.defaultof<GlyphPosition>, false, second + 1, 0, 0)
            | raw -> PosOutcome(second, second, Font.emScaleX env.Font (int raw), 0, second + 1)
        | false -> pairOutcomeFull env format1 format2 values devices at positions index second

    /// <summary>Finds a second glyph in a pair set, <c>hb_bsearch</c> over the records ordered by second glyph.</summary>
    /// <param name="seconds">The second glyphs of every set.</param>
    /// <param name="glyph">The second glyph looked for.</param>
    /// <param name="low">The first record of the set.</param>
    /// <param name="high">The last record of the set.</param>
    /// <returns>The record's index in the flat arrays, or -1.</returns>
    [<TailCall>]
    let rec private findSecond (seconds: ReadOnlySpan<uint32>) (glyph: uint32) (low: int) (high: int) =
        match low > high with
        | true -> -1
        | false ->
            let mid = int ((uint32 low + uint32 high) >>> 1)
            let probe = seconds[mid]
            match glyph < probe, glyph > probe with
            | true, _ -> findSecond seconds glyph low (mid - 1)
            | _, true -> findSecond seconds glyph (mid + 1) high
            | _ -> mid

    /// <summary>Looks a matched pair up in the pair set of its first glyph and applies it, <c>PairSet::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="covered">The coverage index of the first glyph.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The first glyph's index.</param>
    /// <param name="second">The second glyph's index.</param>
    /// <returns>The outcome.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private applyPairSet (env: PosEnv) (subtable: PosSubtable) (covered: int) (infos: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (second: int) =
        match subtable with
        | PairPosGlyphs (_, format1, format2, starts, seconds, values, devices, _, _) ->
            let found =
                match covered + 1 < starts.Length with
                | false -> -1
                | true -> findSecond (seconds.AsSpan()) infos[second].Codepoint starts[covered] (starts[covered + 1] - 1)
            match found with
            | -1 -> missing (second + 1)
            | record ->
                let length = GposTables.valueLength format1 + GposTables.valueLength format2
                pairOutcome env format1 format2 values devices (record * length) positions index second
        | SinglePos _ -> untouched
        | PairPosClasses _ -> untouched
        | CursivePos _ -> untouched
        | MarkBasePos _ -> untouched
        | MarkLigPos _ -> untouched
        | MarkMarkPos _ -> untouched
        | PosContext _ -> untouched
        | PosChainContext _ -> untouched
        | PosPending _ -> untouched

    /// <summary>Applies one pair-adjustment subtable of format 1 or 3 to the glyph at an index, <c>PairPosFormat1_3::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The outcome.</returns>
    let applyPairGlyphs (env: PosEnv) (subtable: PosSubtable) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) =
        match subtable with
        | PairPosGlyphs (coverage, _, _, _, _, _, _, _, direct) ->
            let covered = coverageIndex coverage direct infos[index].Codepoint
            match covered < 0 with
            | true -> untouched
            | false ->
                match nextGlyph env infos props index with
                | Probe.Miss unsafeTo -> missing unsafeTo
                | Probe.Hit second -> applyPairSet env subtable covered infos positions index second
        | SinglePos _ -> untouched
        | PairPosClasses _ -> untouched
        | CursivePos _ -> untouched
        | MarkBasePos _ -> untouched
        | MarkLigPos _ -> untouched
        | MarkMarkPos _ -> untouched
        | PosContext _ -> untouched
        | PosChainContext _ -> untouched
        | PosPending _ -> untouched

    /// <summary>Applies the class pair of a matched pair, <c>PairPosFormat2_4::apply</c> once the second glyph is found.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The first glyph's index.</param>
    /// <param name="second">The second glyph's index.</param>
    /// <returns>The outcome.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let private applyClassPair (env: PosEnv) (subtable: PosSubtable) (infos: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (second: int) =
        match subtable with
        | PairPosClasses (_, format1, format2, classDef1, classDef2, class1Count, class2Count, values, devices, _, _, first, secondDirect) ->
            let klass1 = classOf classDef1 first infos[index].Codepoint
            let klass2 = classOf classDef2 secondDirect infos[second].Codepoint
            match klass1 >= uint32 class1Count || klass2 >= uint32 class2Count with
            | true -> missing (second + 1)
            | false ->
                let length = GposTables.valueLength format1 + GposTables.valueLength format2
                let at = length * (int klass1 * class2Count + int klass2)
                pairOutcome env format1 format2 values devices at positions index second
        | SinglePos _ -> untouched
        | PairPosGlyphs _ -> untouched
        | CursivePos _ -> untouched
        | MarkBasePos _ -> untouched
        | MarkLigPos _ -> untouched
        | MarkMarkPos _ -> untouched
        | PosContext _ -> untouched
        | PosChainContext _ -> untouched
        | PosPending _ -> untouched

    /// <summary>Applies one pair-adjustment subtable of format 2 or 4 to the glyph at an index, <c>PairPosFormat2_4::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The outcome.</returns>
    let applyPairClasses (env: PosEnv) (subtable: PosSubtable) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) =
        match subtable with
        | PairPosClasses (coverage, _, _, _, _, _, _, _, _, _, direct, _, _) ->
            let covered = coverageIndex coverage direct infos[index].Codepoint
            match covered < 0 with
            | true -> untouched
            | false ->
                match nextGlyph env infos props index with
                | Probe.Miss unsafeTo -> missing unsafeTo
                | Probe.Hit second -> applyClassPair env subtable infos positions index second
        | SinglePos _ -> untouched
        | PairPosGlyphs _ -> untouched
        | CursivePos _ -> untouched
        | MarkBasePos _ -> untouched
        | MarkLigPos _ -> untouched
        | MarkMarkPos _ -> untouched
        | PosContext _ -> untouched
        | PosChainContext _ -> untouched
        | PosPending _ -> untouched

    /// <summary>Applies one GPOS subtable to the glyph at an index.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <returns>The outcome.</returns>
    let applySubtable (env: PosEnv) (subtable: PosSubtable) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) =
        match subtable with
        | SinglePos _ -> applySingle env subtable infos positions index
        | PairPosGlyphs _ -> applyPairGlyphs env subtable infos props positions index
        | PairPosClasses _ -> applyPairClasses env subtable infos props positions index
        | CursivePos _ -> untouched
        | MarkBasePos _ -> untouched
        | MarkLigPos _ -> untouched
        | MarkMarkPos _ -> untouched
        | PosContext _ -> untouched
        | PosChainContext _ -> untouched
        | PosPending _ -> untouched
