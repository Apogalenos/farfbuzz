namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Var

/// <summary>The cursive, mark-to-base, mark-to-ligature and mark-to-mark attachment lookups of GPOS,
/// <c>CursivePosFormat1_2::apply</c>, <c>MarkBasePosFormat1_2::apply</c>, <c>MarkLigPosFormat1_2::apply</c> and
/// <c>MarkMarkPosFormat1_2::apply</c>, as functions of their arguments that return what to write and never write it. What
/// the lookup keeps between glyphs, <c>last_base</c> and <c>last_base_until</c>, comes in and goes out as values.</summary>
[<RequireQualifiedAccess>]
module internal GposAttach =
    /// <summary>The mark attachment type, <c>ATTACH_TYPE_MARK</c>.</summary>
    [<Literal>]
    let private AttachTypeMark = 0x01u

    /// <summary>The cursive attachment type, <c>ATTACH_TYPE_CURSIVE</c>.</summary>
    [<Literal>]
    let private AttachTypeCursive = 0x02u

    /// <summary>The mask of the attach chain and attach type in a position's var.</summary>
    [<Literal>]
    let private ChainAndTypeMask = 0x00FFFFFFu

    /// <summary>The mask that clears the attach chain, the low two bytes of <c>var</c>.</summary>
    [<Literal>]
    let private ChainClearMask = 0xFFFF0000u

    /// <summary>The byte of <c>var</c> that holds the attach type, <c>var.u8[2]</c>.</summary>
    [<Literal>]
    let private TypeShift = 16

    /// <summary>The longest range of glyphs a flag may be set on, <c>_set_glyph_flags</c>'s <c>end - start &gt; 255</c>.</summary>
    [<Literal>]
    let private MaxFlagRange = 255

    /// <summary>The attach chain of a position, <c>var.i16[0]</c>.</summary>
    /// <param name="position">The position.</param>
    /// <returns>The chain.</returns>
    let private attachChain (position: GlyphPosition) =
        int (int16 (uint16 (position.Var &&& 0xFFFFu)))

    /// <summary>The attach type of a position, <c>var.u8[2]</c>.</summary>
    /// <param name="position">The position.</param>
    /// <returns>The type.</returns>
    let private attachType (position: GlyphPosition) =
        (position.Var >>> TypeShift) &&& 0xFFu

    /// <summary>Converts a rounded float to a position, as the C cast saturates it here.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The position.</returns>
    let private toPosition (value: float32) =
        Fixed.clampToInt32 (float value)

    /// <summary>The outcome of a subtable that did not apply: its unsafe-to-concat range and the cache as it stands.</summary>
    /// <param name="concatFrom">The start of the range, negative for none.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let nothing concatFrom lastBase lastBaseUntil =
        AttachOutcome(false, AttachKind.Nothing, 0, Unchecked.defaultof<GlyphPosition>, false, 0, 0, concatFrom, 0, 0f, 0f, 0f, 0f, 0, false, lastBase, lastBaseUntil)

    /// <summary>Adds the unsafe-to-concat range of the subtables that failed before this one to an outcome.</summary>
    /// <param name="outcome">The outcome.</param>
    /// <param name="concatFrom">The start of the range, negative for none.</param>
    /// <returns>The outcome with the range.</returns>
    let withConcat (outcome: AttachOutcome) concatFrom =
        AttachOutcome(outcome.Applied, outcome.Kind, outcome.Next, outcome.Position, outcome.Attached, outcome.BreakFrom, outcome.BreakTo, concatFrom, outcome.Partner, outcome.ExitX, outcome.ExitY, outcome.EntryX, outcome.EntryY, outcome.Direction, outcome.RightToLeft, outcome.LastBase, outcome.LastBaseUntil)

    /// <summary>Merges the unsafe-to-concat range of a subtable that failed into the range so far: every range ends at the glyph
    /// after the one the lookup stopped on, a range longer than the limit is not set at all, and the union of the rest is the
    /// longest.</summary>
    /// <param name="so">The start of the range so far, negative for none.</param>
    /// <param name="index">The glyph the lookup stopped on.</param>
    /// <param name="from">The start of the new range, negative for none.</param>
    /// <returns>The start of the merged range.</returns>
    let mergeConcat so index from =
        match from < 0 || index + 1 - from > MaxFlagRange with
        | true -> so
        | false ->
            match so < 0 with
            | true -> from
            | false -> min so from

    /// <summary>The coordinates of an anchor in user space, <c>Anchor::get_anchor</c>: formats 1 and 2 scale the design
    /// coordinates (the contour point of format 2 is used when the font funcs give one, and the OpenType font funcs do not),
    /// format 3 adds the deltas of its devices when the font has a ppem or variation coordinates, and the Null anchor is the
    /// origin.</summary>
    /// <param name="font">The font.</param>
    /// <param name="anchor">The anchor.</param>
    /// <returns>The x and y coordinates.</returns>
    let private anchorCoordinates (font: Font) anchor =
        match anchor with
        | NoAnchor -> struct (0f, 0f)
        | PlainAnchor (x, y) -> struct (Font.emFscaleX font x, Font.emFscaleY font y)
        | DeviceAnchor (x, y, xDevice, yDevice) ->
            let useX = font.XPpem <> 0u || font.Variations.Coords.HasNonZero
            let useY = font.YPpem <> 0u || font.Variations.Coords.HasNonZero
            let baseX = Font.emFscaleX font x
            let baseY = Font.emFscaleY font y
            struct ((match useX with | true -> baseX + Device.xDeltaF font xDevice | false -> baseX),
                    (match useY with | true -> baseY + Device.yDeltaF font yDevice | false -> baseY))

    /// <summary>The anchor of a record of an array, the Null record's anchor, which is null, beyond the array.</summary>
    /// <param name="anchors">The anchors.</param>
    /// <param name="index">The record's index, negative for a glyph that is not covered.</param>
    /// <returns>The anchor.</returns>
    let private recordAnchor (anchors: ImmutableArray<Anchor>) index =
        match uint32 index < uint32 anchors.Length with
        | true -> anchors[index]
        | false -> NoAnchor

    /// <summary>The anchor of an entry of an anchor matrix, <c>AnchorMatrix::get_anchor</c>: none for a row or column beyond it
    /// and for an offset that is null or fails its check.</summary>
    /// <param name="anchors">The anchors of the matrix and those before it.</param>
    /// <param name="start">The index of the matrix's first anchor.</param>
    /// <param name="rows">The number of rows.</param>
    /// <param name="columns">The number of columns, the class count.</param>
    /// <param name="row">The row.</param>
    /// <param name="column">The column.</param>
    /// <returns>The anchor.</returns>
    let private matrixAnchor (anchors: ImmutableArray<Anchor>) start rows columns row column =
        match row >= rows || column >= columns with
        | true -> NoAnchor
        | false ->
            let at = int64 start + int64 row * int64 columns + int64 column
            match uint64 at < uint64 anchors.Length with
            | true -> anchors[int at]
            | false -> NoAnchor

    /// <summary>The offset of a glyph's chain of cursive attachments from the baseline, <c>resolve_cross_offset</c>: the
    /// glyph's cross-direction offset plus those of the glyphs it is attached to by cursive links, as far as the run is long.</summary>
    /// <param name="positions">The positions.</param>
    /// <param name="horizontal">Whether the direction is horizontal.</param>
    /// <param name="glyphPos">The glyph the walk is at.</param>
    /// <param name="count">The number of links followed, one for the glyph itself.</param>
    /// <param name="offset">The offset so far.</param>
    /// <returns>The offset.</returns>
    [<TailCall>]
    let rec private crossOffset (positions: ReadOnlySpan<GlyphPosition>) horizontal glyphPos count offset =
        let position = positions[glyphPos]
        match count < positions.Length && attachType position &&& AttachTypeCursive <> 0u with
        | false -> offset
        | true ->
            match attachChain position with
            | 0 -> offset
            | chain ->
                let parent = uint32 (glyphPos + chain)
                match parent >= uint32 positions.Length with
                | true -> offset
                | false ->
                    let next = int parent
                    let step = match horizontal with | true -> positions[next].YOffset | false -> positions[next].XOffset
                    crossOffset positions horizontal next (count + 1) (GposLookups.saturateAdd offset step)

    /// <summary>Attaches a mark to a glyph, <c>apply_mark_attachment</c>: the mark's offset is the difference of the two
    /// anchors, rounded, plus the cross offset of the glyph's cursive chain; a chain that does not fit 16 bits leaves the mark
    /// detached, and the lookup goes on all the same.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="positions">The positions.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="markAnchor">The mark's anchor.</param>
    /// <param name="glyphAnchor">The anchor of the glyph the mark attaches to.</param>
    /// <param name="glyphPos">The index of that glyph.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private attachMark (env: PosEnv) (positions: ReadOnlySpan<GlyphPosition>) index markAnchor glyphAnchor glyphPos lastBase lastBaseUntil =
        let struct (markX, markY) = anchorCoordinates env.Font markAnchor
        let struct (baseX, baseY) = anchorCoordinates env.Font glyphAnchor
        let baseOffset = crossOffset positions env.Horizontal glyphPos 1 (match env.Horizontal with | true -> positions[glyphPos].YOffset | false -> positions[glyphPos].XOffset)
        let chain = glyphPos - index
        let mark = positions[index]
        match chain = int (int16 chain) with
        | false ->
            let detached = { mark with Var = mark.Var &&& ChainClearMask }
            AttachOutcome(true, AttachKind.Mark, index + 1, detached, false, glyphPos, index + 1, -1, 0, 0f, 0f, 0f, 0f, 0, false, lastBase, lastBaseUntil)
        | true ->
            let xOffset = toPosition (Fixed.roundf (baseX - markX))
            let yOffset = toPosition (Fixed.roundf (baseY - markY))
            let var = (mark.Var &&& ~~~ChainAndTypeMask) ||| (AttachTypeMark <<< TypeShift) ||| uint32 (uint16 chain)
            let moved =
                match env.Horizontal with
                | true -> { mark with XOffset = xOffset; YOffset = GposLookups.saturateAdd yOffset baseOffset; Var = var }
                | false -> { mark with XOffset = GposLookups.saturateAdd xOffset baseOffset; YOffset = yOffset; Var = var }
            AttachOutcome(true, AttachKind.Mark, index + 1, moved, true, glyphPos, index + 1, -1, 0, 0f, 0f, 0f, 0f, 0, false, lastBase, lastBaseUntil)

    /// <summary>Applies a mark array to a mark and the glyph it found, <c>MarkArray::apply</c>: the anchor of the glyph for the
    /// mark's class, and when the subtable has none the subtable does not apply and the next one gets its chance.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="classes">The mark classes.</param>
    /// <param name="markAnchors">The mark anchors.</param>
    /// <param name="positions">The positions.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <param name="anchors">The anchors of the matrix and those before it.</param>
    /// <param name="start">The index of the matrix's first anchor.</param>
    /// <param name="rows">The number of rows.</param>
    /// <param name="columns">The class count.</param>
    /// <param name="row">The row of the glyph the mark attaches to.</param>
    /// <param name="glyphPos">The index of that glyph.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyMarkArray (env: PosEnv) (classes: ImmutableArray<uint16>) (markAnchors: ImmutableArray<Anchor>) (positions: ReadOnlySpan<GlyphPosition>) index markIndex anchors start rows columns row glyphPos lastBase lastBaseUntil =
        let markClass = match uint32 markIndex < uint32 classes.Length with | true -> int classes[markIndex] | false -> 0
        let markAnchor = match uint32 markIndex < uint32 markAnchors.Length with | true -> markAnchors[markIndex] | false -> PlainAnchor (0, 0)
        match matrixAnchor anchors start rows columns row markClass with
        | NoAnchor -> nothing -1 lastBase lastBaseUntil
        | glyphAnchor -> attachMark env positions index markAnchor glyphAnchor glyphPos lastBase lastBaseUntil

    /// <summary>Whether a glyph may be the base a mark attaches to, <c>MarkBasePosFormat1_2::accept</c>: only the first glyph
    /// of a multiple substitution is a base, unless a mark or another glyph than its predecessor in the sequence stands before it.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="idx">The index of the candidate.</param>
    /// <returns>True when the glyph is accepted.</returns>
    let private acceptBase (props: ReadOnlySpan<GlyphProps>) idx =
        let here = props[idx]
        not (GlyphPropsBits.multiplied here)
        || GlyphPropsBits.ligComp here = 0uy
        || (idx = 0
            || GlyphPropsBits.isMark props[idx - 1]
            || not (GlyphPropsBits.multiplied props[idx - 1])
            || GlyphPropsBits.ligId here <> GlyphPropsBits.ligId props[idx - 1]
            || int (GlyphPropsBits.ligComp here) <> int (GlyphPropsBits.ligComp props[idx - 1]) + 1)

    /// <summary>Whether a glyph may be the ligature a mark attaches to, <c>MarkLigPosFormat1_2::accept</c>: only the first glyph
    /// of a multiple substitution.</summary>
    /// <param name="props">The glyph properties.</param>
    /// <param name="idx">The index of the candidate.</param>
    /// <returns>True when the glyph is accepted.</returns>
    let private acceptLigature (props: ReadOnlySpan<GlyphProps>) idx =
        not (GlyphPropsBits.multiplied props[idx]) || GlyphPropsBits.ligComp props[idx] = 0uy

    /// <summary>Searches backwards for the glyph a mark attaches to, the loop of <c>MarkBasePosFormat1_2::apply</c> and
    /// <c>MarkLigPosFormat1_2::apply</c>: the first glyph the skipping iterator, which ignores marks, matches, that is
    /// accepted or covered by the subtable.</summary>
    /// <param name="matcher">The matcher with the lookup properties set to ignore marks.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="coverage">The coverage of the glyphs the subtable attaches to.</param>
    /// <param name="direct">Its direct table.</param>
    /// <param name="ligature">Whether the subtable is a mark-to-ligature one.</param>
    /// <param name="j">One past the next candidate.</param>
    /// <param name="until">The index the search stops above.</param>
    /// <returns>The index of the glyph, or minus one.</returns>
    [<TailCall>]
    let rec private searchBase (matcher: Matcher) (gdef: Gdef) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (coverage: Coverage) (direct: DirectIndex) ligature j until =
        match j > until with
        | false -> -1
        | true ->
            let candidate = j - 1
            let matched = match Matching.verdict matcher gdef (NoMatchFunc()) 0 infos[candidate] props[candidate] with | Matched -> true | NotMatched -> false | Skipped -> false
            let accepted = match ligature with | true -> acceptLigature props candidate | false -> acceptBase props candidate
            match matched && (accepted || GposLookups.coverageIndex coverage direct infos[candidate].Codepoint >= 0) with
            | true -> candidate
            | false -> searchBase matcher gdef infos props coverage direct ligature (j - 1) until

    /// <summary>The base a mark attaches to: the cached one when the search finds none, <c>c-&gt;last_base</c>, after the cache is
    /// dropped when the lookup has moved back past it.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="coverage">The coverage of the glyphs the subtable attaches to.</param>
    /// <param name="direct">Its direct table.</param>
    /// <param name="ligature">Whether the subtable is a mark-to-ligature one.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The base, minus one for none.</returns>
    let private findBase (env: PosEnv) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (coverage: Coverage) (direct: DirectIndex) ligature index lastBase lastBaseUntil =
        let matcher = { env.Matcher with LookupProps = LookupFlag.IgnoreMarks }
        let reset = lastBaseUntil > index
        let found = searchBase matcher env.Gdef infos props coverage direct ligature index (match reset with | true -> 0 | false -> lastBaseUntil)
        match found, reset with
        | -1, true -> -1
        | -1, false -> lastBase
        | candidate, _ -> candidate

    /// <summary>Applies a mark-to-base subtable to the glyph at an index, <c>MarkBasePosFormat1_2::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="attachment">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyMarkBase (env: PosEnv) (attachment: MarkAttachment) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index lastBase lastBaseUntil =
        let markIndex = GposLookups.coverageIndex attachment.MarkCoverage attachment.MarkDirect infos[index].Codepoint
        match markIndex < 0 with
        | true -> nothing -1 lastBase lastBaseUntil
        | false ->
            let found = findBase env infos props attachment.TargetCoverage attachment.TargetDirect false index lastBase lastBaseUntil
            match found with
            | -1 -> nothing 0 found index
            | baseIndex ->
                let row = GposLookups.coverageIndex attachment.TargetCoverage attachment.TargetDirect infos[baseIndex].Codepoint
                match row < 0 with
                | true -> nothing baseIndex found index
                | false ->
                    applyMarkArray env attachment.MarkClasses attachment.MarkAnchors positions index markIndex attachment.Anchors 0 attachment.Rows attachment.ClassCount row baseIndex found index

    /// <summary>Attaches a mark to the component of a ligature that its ligature id and component name, the last component when
    /// the mark belongs to another ligature, <c>MarkLigPosFormat1_2::apply</c> once the ligature is found and covered.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="attachment">The subtable.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <param name="baseIndex">The index of the ligature.</param>
    /// <param name="ligIndex">The ligature's index in the ligature coverage.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private attachToLigature (env: PosEnv) (attachment: LigatureAttachment) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index markIndex baseIndex ligIndex lastBase lastBaseUntil =
        let components = match uint32 ligIndex < uint32 attachment.Components.Length with | true -> attachment.Components[ligIndex] | false -> 0
        match components with
        | 0 -> nothing baseIndex lastBase lastBaseUntil
        | _ ->
            let ligId = GlyphPropsBits.ligId props[baseIndex]
            let markId = GlyphPropsBits.ligId props[index]
            let markComp = int (GlyphPropsBits.ligComp props[index])
            let row = match ligId <> 0uy && ligId = markId && markComp > 0 with | true -> min components markComp - 1 | false -> components - 1
            applyMarkArray env attachment.LigMarkClasses attachment.LigMarkAnchors positions index markIndex attachment.LigAnchors attachment.Starts[ligIndex] components attachment.LigClassCount row baseIndex lastBase lastBaseUntil

    /// <summary>Applies a mark-to-ligature subtable to the glyph at an index, <c>MarkLigPosFormat1_2::apply</c>.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="attachment">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyMarkLigature (env: PosEnv) (attachment: LigatureAttachment) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index lastBase lastBaseUntil =
        let markIndex = GposLookups.coverageIndex attachment.LigMarkCoverage attachment.LigMarkDirect infos[index].Codepoint
        match markIndex < 0 with
        | true -> nothing -1 lastBase lastBaseUntil
        | false ->
            let found = findBase env infos props attachment.LigatureCoverage attachment.LigatureDirect true index lastBase lastBaseUntil
            match found with
            | -1 -> nothing 0 found index
            | baseIndex ->
                let ligIndex = GposLookups.coverageIndex attachment.LigatureCoverage attachment.LigatureDirect infos[baseIndex].Codepoint
                match ligIndex < 0 with
                | true -> nothing baseIndex found index
                | false -> attachToLigature env attachment props positions index markIndex baseIndex ligIndex found index

    /// <summary>Whether the marks of two glyphs belong to the same base or the same ligature component, the check of
    /// <c>MarkMarkPosFormat1_2::apply</c> on their ligature ids and components.</summary>
    /// <param name="here">The properties of the mark being attached.</param>
    /// <param name="there">The properties of the mark before it.</param>
    /// <returns>True when the marks are on the same base or component.</returns>
    let private sameBase (here: GlyphProps) (there: GlyphProps) =
        let id1 = GlyphPropsBits.ligId here
        let id2 = GlyphPropsBits.ligId there
        let comp1 = GlyphPropsBits.ligComp here
        let comp2 = GlyphPropsBits.ligComp there
        match id1 = id2 with
        | true -> id1 = 0uy || comp1 = comp2
        | false -> (id1 > 0uy && comp1 = 0uy) || (id2 > 0uy && comp2 = 0uy)

    /// <summary>Attaches a mark to the mark before it once that mark is found, covered and on the same base.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="attachment">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="markIndex">The glyph's index in the first mark coverage.</param>
    /// <param name="previous">The index of the mark before it.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private attachToMark (env: PosEnv) (attachment: MarkAttachment) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index markIndex previous lastBase lastBaseUntil =
        match sameBase props[index] props[previous] with
        | false -> nothing previous lastBase lastBaseUntil
        | true ->
            let row = GposLookups.coverageIndex attachment.TargetCoverage attachment.TargetDirect infos[previous].Codepoint
            match row < 0 with
            | true -> nothing previous lastBase lastBaseUntil
            | false -> applyMarkArray env attachment.MarkClasses attachment.MarkAnchors positions index markIndex attachment.Anchors 0 attachment.Rows attachment.ClassCount row previous lastBase lastBaseUntil

    /// <summary>Applies a mark-to-mark subtable to the glyph at an index, <c>MarkMarkPosFormat1_2::apply</c>: the glyph before it
    /// that the lookup's own flags, less the ignore flags, do not skip must be a mark.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="attachment">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyMarkMark (env: PosEnv) (attachment: MarkAttachment) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index lastBase lastBaseUntil =
        let markIndex = GposLookups.coverageIndex attachment.MarkCoverage attachment.MarkDirect infos[index].Codepoint
        match markIndex < 0 with
        | true -> nothing -1 lastBase lastBaseUntil
        | false ->
            let matcher = { env.Matcher with LookupProps = env.Matcher.LookupProps &&& ~~~LookupFlag.IgnoreFlags }
            match Matching.prev matcher env.Gdef (NoMatchFunc()) 0 infos props index with
            | Miss unsafeFrom -> nothing unsafeFrom lastBase lastBaseUntil
            | Hit previous ->
                match GlyphPropsBits.isMark props[previous] with
                | false -> nothing previous lastBase lastBaseUntil
                | true -> attachToMark env attachment infos props positions index markIndex previous lastBase lastBaseUntil

    /// <summary>Applies a cursive attachment subtable to the glyph at an index, <c>CursivePosFormat1_2::apply</c>: the glyph needs an
    /// entry anchor, the glyph the skipping iterator finds before it an exit anchor, and the kernel does the rest.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="cursive">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyCursive (env: PosEnv) (cursive: CursiveAttachment) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) index lastBase lastBaseUntil =
        let entryAnchor = recordAnchor cursive.Entries (GposLookups.coverageIndex cursive.CursiveCoverage cursive.CursiveDirect infos[index].Codepoint)
        match entryAnchor with
        | NoAnchor -> nothing -1 lastBase lastBaseUntil
        | PlainAnchor _
        | DeviceAnchor _ ->
            match Matching.prev env.Matcher env.Gdef (NoMatchFunc()) 0 infos props index with
            | Miss unsafeFrom -> nothing unsafeFrom lastBase lastBaseUntil
            | Hit previous ->
                let exitAnchor = recordAnchor cursive.Exits (GposLookups.coverageIndex cursive.CursiveCoverage cursive.CursiveDirect infos[previous].Codepoint)
                match exitAnchor with
                | NoAnchor -> nothing previous lastBase lastBaseUntil
                | PlainAnchor _
                | DeviceAnchor _ ->
                    let struct (exitX, exitY) = anchorCoordinates env.Font exitAnchor
                    let struct (entryX, entryY) = anchorCoordinates env.Font entryAnchor
                    let rightToLeft = env.Matcher.LookupProps &&& LookupFlag.RightToLeft <> 0u
                    AttachOutcome(true, AttachKind.Cursive, index + 1, Unchecked.defaultof<GlyphPosition>, false, 0, 0, -1, previous, exitX, exitY, entryX, entryY, int (Direction.value env.Direction), rightToLeft, lastBase, lastBaseUntil)

    /// <summary>Applies one GPOS subtable to the glyph at an index, when it is one of the attachment types; any other subtable
    /// does not apply, which records nothing.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let applySubtable (env: PosEnv) (subtable: PosSubtable) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index lastBase lastBaseUntil =
        match subtable with
        | CursivePos cursive -> applyCursive env cursive infos props index lastBase lastBaseUntil
        | MarkBasePos attachment -> applyMarkBase env attachment infos props positions index lastBase lastBaseUntil
        | MarkLigPos attachment -> applyMarkLigature env attachment infos props positions index lastBase lastBaseUntil
        | MarkMarkPos attachment -> applyMarkMark env attachment infos props positions index lastBase lastBaseUntil
        | SinglePos _ -> nothing -1 lastBase lastBaseUntil
        | PairPosGlyphs _ -> nothing -1 lastBase lastBaseUntil
        | PairPosClasses _ -> nothing -1 lastBase lastBaseUntil
        | PosContext _ -> nothing -1 lastBase lastBaseUntil
        | PosChainContext _ -> nothing -1 lastBase lastBaseUntil
        | PosPending _ -> nothing -1 lastBase lastBaseUntil

    /// <summary>Whether a GPOS subtable is of one of the attachment types.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>True for a cursive, mark-to-base, mark-to-ligature or mark-to-mark subtable.</returns>
    let isAttachment subtable =
        match subtable with
        | CursivePos _ -> true
        | MarkBasePos _ -> true
        | MarkLigPos _ -> true
        | MarkMarkPos _ -> true
        | SinglePos _ -> false
        | PairPosGlyphs _ -> false
        | PairPosClasses _ -> false
        | PosContext _ -> false
        | PosChainContext _ -> false
        | PosPending _ -> false

    /// <summary>Attaches a mark to a glyph through a compiled table, <see cref="attachMark"/> with the anchors read as raw coordinates.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="positions">The positions.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="markX">The mark's anchor, x, in design units.</param>
    /// <param name="markY">The mark's anchor, y, in design units.</param>
    /// <param name="glyphX">The anchor of the glyph the mark attaches to, x, in design units.</param>
    /// <param name="glyphY">The anchor of the glyph the mark attaches to, y, in design units.</param>
    /// <param name="glyphPos">The index of that glyph.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private attachMarkRaw (env: PosEnv) (positions: ReadOnlySpan<GlyphPosition>) index markX markY glyphX glyphY glyphPos lastBase lastBaseUntil =
        let baseOffset = crossOffset positions env.Horizontal glyphPos 1 (match env.Horizontal with | true -> positions[glyphPos].YOffset | false -> positions[glyphPos].XOffset)
        let chain = glyphPos - index
        let mark = positions[index]
        match chain = int (int16 chain) with
        | false ->
            let detached = { mark with Var = mark.Var &&& ChainClearMask }
            AttachOutcome(true, AttachKind.Mark, index + 1, detached, false, glyphPos, index + 1, -1, 0, 0f, 0f, 0f, 0f, 0, false, lastBase, lastBaseUntil)
        | true ->
            let xOffset = toPosition (Fixed.roundf (Font.emFscaleX env.Font glyphX - Font.emFscaleX env.Font markX))
            let yOffset = toPosition (Fixed.roundf (Font.emFscaleY env.Font glyphY - Font.emFscaleY env.Font markY))
            let var = (mark.Var &&& ~~~ChainAndTypeMask) ||| (AttachTypeMark <<< TypeShift) ||| uint32 (uint16 chain)
            let moved =
                match env.Horizontal with
                | true -> { mark with XOffset = xOffset; YOffset = GposLookups.saturateAdd yOffset baseOffset; Var = var }
                | false -> { mark with XOffset = GposLookups.saturateAdd xOffset baseOffset; YOffset = yOffset; Var = var }
            AttachOutcome(true, AttachKind.Mark, index + 1, moved, true, glyphPos, index + 1, -1, 0, 0f, 0f, 0f, 0f, 0, false, lastBase, lastBaseUntil)

    /// <summary>Applies a compiled mark array to a mark and the glyph it found, <see cref="applyMarkArray"/> over the compiled anchors.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="table">The compiled subtable.</param>
    /// <param name="positions">The positions.</param>
    /// <param name="index">The mark's index.</param>
    /// <param name="markIndex">The mark's index in the mark coverage.</param>
    /// <param name="row">The row of the glyph the mark attaches to.</param>
    /// <param name="glyphPos">The index of that glyph.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyCompiledMarkArray (env: PosEnv) (table: MarkTable) (positions: ReadOnlySpan<GlyphPosition>) index markIndex row glyphPos lastBase lastBaseUntil =
        let attachment = table.Source
        let markClass = match uint32 markIndex < uint32 attachment.MarkClasses.Length with | true -> int attachment.MarkClasses[markIndex] | false -> 0
        let at = int64 row * int64 attachment.ClassCount + int64 markClass
        match row >= attachment.Rows || markClass >= attachment.ClassCount || uint64 at >= uint64 table.AnchorKnown.Length || not table.AnchorKnown[int at] with
        | true -> nothing -1 lastBase lastBaseUntil
        | false ->
            let known = uint32 markIndex < uint32 table.MarkX.Length
            let markX = match known with | true -> table.MarkX[markIndex] | false -> 0
            let markY = match known with | true -> table.MarkY[markIndex] | false -> 0
            attachMarkRaw env positions index markX markY table.AnchorX[int at] table.AnchorY[int at] glyphPos lastBase lastBaseUntil

    /// <summary>Applies a compiled mark-to-base subtable to the glyph at an index, <see cref="applyMarkBase"/> over the compiled anchors.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="table">The compiled subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyCompiledMarkBase (env: PosEnv) (table: MarkTable) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index lastBase lastBaseUntil =
        let attachment = table.Source
        let markIndex = GposLookups.coverageIndex attachment.MarkCoverage attachment.MarkDirect infos[index].Codepoint
        match markIndex < 0 with
        | true -> nothing -1 lastBase lastBaseUntil
        | false ->
            let found = findBase env infos props attachment.TargetCoverage attachment.TargetDirect false index lastBase lastBaseUntil
            match found with
            | -1 -> nothing 0 found index
            | baseIndex ->
                let row = GposLookups.coverageIndex attachment.TargetCoverage attachment.TargetDirect infos[baseIndex].Codepoint
                match row < 0 with
                | true -> nothing baseIndex found index
                | false -> applyCompiledMarkArray env table positions index markIndex row baseIndex found index

    /// <summary>Attaches a mark to the mark before it through a compiled table once that mark is found, covered and on the same base,
    /// <see cref="attachToMark"/> over the compiled anchors.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="table">The compiled subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="markIndex">The glyph's index in the first mark coverage.</param>
    /// <param name="previous">The index of the mark before it.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private attachToCompiledMark (env: PosEnv) (table: MarkTable) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index markIndex previous lastBase lastBaseUntil =
        match sameBase props[index] props[previous] with
        | false -> nothing previous lastBase lastBaseUntil
        | true ->
            let attachment = table.Source
            let row = GposLookups.coverageIndex attachment.TargetCoverage attachment.TargetDirect infos[previous].Codepoint
            match row < 0 with
            | true -> nothing previous lastBase lastBaseUntil
            | false -> applyCompiledMarkArray env table positions index markIndex row previous lastBase lastBaseUntil

    /// <summary>Applies a compiled mark-to-mark subtable to the glyph at an index, <see cref="applyMarkMark"/> over the compiled anchors.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="table">The compiled subtable.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let private applyCompiledMarkMark (env: PosEnv) (table: MarkTable) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index lastBase lastBaseUntil =
        let attachment = table.Source
        let markIndex = GposLookups.coverageIndex attachment.MarkCoverage attachment.MarkDirect infos[index].Codepoint
        match markIndex < 0 with
        | true -> nothing -1 lastBase lastBaseUntil
        | false ->
            let matcher = { env.Matcher with LookupProps = env.Matcher.LookupProps &&& ~~~LookupFlag.IgnoreFlags }
            match Matching.prev matcher env.Gdef (NoMatchFunc()) 0 infos props index with
            | Miss unsafeFrom -> nothing unsafeFrom lastBase lastBaseUntil
            | Hit previous ->
                match GlyphPropsBits.isMark props[previous] with
                | false -> nothing previous lastBase lastBaseUntil
                | true -> attachToCompiledMark env table infos props positions index markIndex previous lastBase lastBaseUntil

    /// <summary>Applies one subtable to the glyph at an index through its compiled table when it has one: a mark-to-base or
    /// mark-to-mark subtable whose anchors are plain; any other subtable is applied as <see cref="applySubtable"/> applies it.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="compiled">The subtable's compiled table, or none.</param>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="props">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The glyph's index.</param>
    /// <param name="lastBase">The cached base.</param>
    /// <param name="lastBaseUntil">The index the cached base is good until.</param>
    /// <returns>The outcome.</returns>
    let applyCompiled (env: PosEnv) (subtable: PosSubtable) (compiled: MarkTable voption) (infos: ReadOnlySpan<GlyphInfo>) (props: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) index lastBase lastBaseUntil =
        match compiled, subtable with
        | ValueSome table, MarkBasePos _ -> applyCompiledMarkBase env table infos props positions index lastBase lastBaseUntil
        | ValueSome table, MarkMarkPos _ -> applyCompiledMarkMark env table infos props positions index lastBase lastBaseUntil
        | _, CursivePos _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, MarkBasePos _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, MarkLigPos _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, MarkMarkPos _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, SinglePos _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, PairPosGlyphs _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, PairPosClasses _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, PosContext _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, PosChainContext _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil
        | _, PosPending _ -> applySubtable env subtable infos props positions index lastBase lastBaseUntil

    /// <summary>The offset a mark gets along one axis from the anchors of a compiled table, the rounded difference of the scaled
    /// anchor of the glyph and that of the mark, saturated as <see cref="attachMarkRaw"/> does. When the font's scale is its design
    /// size the scaling is exact for coordinates of 16 bits, and the difference of the two is the offset.</summary>
    /// <param name="env">The environment.</param>
    /// <param name="glyphCoordinate">The glyph's anchor, in design units.</param>
    /// <param name="markCoordinate">The mark's anchor, in design units.</param>
    /// <param name="onX">Whether the axis is x.</param>
    /// <returns>The offset.</returns>
    let anchorDelta (env: PosEnv) glyphCoordinate markCoordinate onX =
        let multiplier = match onX with | true -> env.Font.XMultF | false -> env.Font.YMultF
        match multiplier = 1.0f, onX with
        | true, _ -> glyphCoordinate - markCoordinate
        | false, true -> toPosition (Fixed.roundf (Font.emFscaleX env.Font glyphCoordinate - Font.emFscaleX env.Font markCoordinate))
        | false, false -> toPosition (Fixed.roundf (Font.emFscaleY env.Font glyphCoordinate - Font.emFscaleY env.Font markCoordinate))
