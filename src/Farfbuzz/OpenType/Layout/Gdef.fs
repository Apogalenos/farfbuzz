namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Var

/// <summary>A struct sanitizer for one offset target, specialised by the JIT so no span-typed function value is needed.</summary>
type internal IOffsetSanitizer =
    /// <summary>Sanitizes the target of an offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The target offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    abstract Sanitize : table: ReadOnlySpan<byte> * offset: int * ops: int -> int voption

/// <summary>The parsed GDEF table and its precomputed glyph properties, the immutable <c>GDEF::accelerator_t</c>.</summary>
[<NoEquality; NoComparison>]
type internal Gdef =
    { Version: uint32
      GlyphClasses: ClassDef
      MarkAttachClasses: ClassDef
      MarkGlyphSets: ImmutableArray<Coverage>
      LigCarets: ReadOnlyMemory<byte>
      AttachList: ReadOnlyMemory<byte>
      HasVarStore: bool
      HasGlyphClasses: bool
      LigCaretsWide: bool
      GlyphProps: ImmutableArray<uint16> }

/// <summary>The GDEF table: versions 1.0-1.4, the blocklist, glyph props, mark sets, carets and attach points.</summary>
[<RequireQualifiedAccess>]
module internal Gdef =
    /// <summary>The version introducing MarkGlyphSetsDef.</summary>
    [<Literal>]
    let private V1_2 = 0x00010002u

    /// <summary>The version introducing the variation store offset.</summary>
    [<Literal>]
    let private V1_3 = 0x00010003u

    /// <summary>The version introducing the five 32-bit offsets.</summary>
    [<Literal>]
    let private V1_4 = 0x00010004u

    /// <summary>The GDEF header size of version 1.0 and 1.1.</summary>
    [<Literal>]
    let private Header10 = 12

    /// <summary>The GDEF header size of version 1.2.</summary>
    [<Literal>]
    let private Header12 = 14

    /// <summary>The GDEF header size of version 1.3.</summary>
    [<Literal>]
    let private Header13 = 18

    /// <summary>The GDEF header size of version 1.4.</summary>
    [<Literal>]
    let private Header14 = 38

    /// <summary>The lookup flag requesting a mark filtering set, <c>LookupFlag::UseMarkFilteringSet</c>.</summary>
    [<Literal>]
    let private UseMarkFilteringSet = 0x0010u

    /// <summary>The blocklisted font triples of <c>src/hb-ot-layout.cc:181-262</c>.</summary>
    let private blocklistTriples : struct (int * int * int) array =
        [| struct (442, 2874, 42038)
           struct (430, 2874, 40662)
           struct (442, 2874, 39116)
           struct (430, 2874, 39374)
           struct (490, 3046, 41638)
           struct (478, 3046, 41902)
           struct (898, 12554, 46470)
           struct (910, 12566, 47732)
           struct (928, 23298, 59332)
           struct (940, 23310, 60732)
           struct (964, 23836, 60072)
           struct (976, 23832, 61456)
           struct (994, 24474, 60336)
           struct (1006, 24470, 61740)
           struct (1006, 24576, 61346)
           struct (1018, 24572, 62828)
           struct (1006, 24576, 61352)
           struct (1018, 24572, 62834)
           struct (832, 7324, 47162)
           struct (844, 7302, 45474)
           struct (180, 13054, 7254)
           struct (192, 12638, 7254)
           struct (192, 12690, 7254)
           struct (188, 248, 3852)
           struct (188, 264, 3426)
           struct (1058, 47032, 11818)
           struct (1046, 47030, 12600)
           struct (1058, 71796, 16770)
           struct (1046, 71790, 17862)
           struct (1046, 71788, 17112)
           struct (1058, 71794, 17514)
           struct (1330, 109904, 57938)
           struct (1330, 109904, 58972)
           struct (1004, 59092, 14836)
           struct (588, 5078, 14418)
           struct (588, 5078, 14238)
           struct (894, 17162, 33960)
           struct (894, 17154, 34472)
           struct (816, 7868, 17052)
           struct (816, 7868, 17138) |]

    /// <summary>The header size of a version, <c>GDEF::get_size</c>.</summary>
    /// <param name="version">The version.</param>
    /// <returns>The header size in bytes.</returns>
    let private headerSize (version: uint32) : int =
        match version >= V1_4, version >= V1_3, version >= V1_2 with
        | true, _, _ -> Header14
        | false, true, _ -> Header13
        | false, false, true -> Header12
        | false, false, false -> Header10

    /// <summary>Reads a big-endian offset value of a given byte width.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="pos">The field offset.</param>
    /// <param name="width">Two, three or four bytes.</param>
    /// <returns>The value.</returns>
    let private readOffset (table: ReadOnlySpan<byte>) (pos: int) (width: int) : uint32 =
        match width with
        | 2 -> uint32 (Binary.u16 table pos)
        | 3 -> Binary.u24 table pos
        | _ -> Binary.u32 table pos

    /// <summary>Sanitizes the target of a class definition offset, <c>ClassDef::sanitize</c>.</summary>
    [<Struct>]
    type private ClassDefSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = ClassDef.sanitize table offset ops

    /// <summary>Sanitizes the target of a coverage offset, <c>Coverage::sanitize</c>.</summary>
    [<Struct>]
    type private CoverageSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = Coverage.sanitize table offset ops

    /// <summary>Sanitizes an offset field whose target sits at base plus its value.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOff">The base the offset is relative to.</param>
    /// <param name="fieldPos">The field's absolute offset.</param>
    /// <param name="width">The offset width.</param>
    /// <param name="sanitize">The target sanitizer.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeOffsetAt<'S when 'S : struct and 'S :> IOffsetSanitizer>
            (table: ReadOnlySpan<byte>) (baseOff: int) (fieldPos: int) (width: int) (sanitize: 'S) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 fieldPos) width with
        | false -> ValueNone
        | true ->
            match readOffset table fieldPos width with
            | 0u -> ValueSome ops
            | value ->
                match sanitize.Sanitize (table, int (int64 baseOff + int64 value), ops) with
                | ValueNone -> ValueNone
                | ValueSome remaining -> ValueSome remaining

    /// <summary>Sanitizes each offset field of an array, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOff">The base each offset is relative to.</param>
    /// <param name="start">The first field offset.</param>
    /// <param name="width">The offset width.</param>
    /// <param name="count">The number of fields.</param>
    /// <param name="index">The next field.</param>
    /// <param name="sanitize">The target sanitizer.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeOffsets<'S when 'S : struct and 'S :> IOffsetSanitizer>
            (table: ReadOnlySpan<byte>) (baseOff: int) (start: int) (width: int) (count: int) (index: int) (sanitize: 'S) (ops: int) : int voption =
        match index < count with
        | false -> ValueSome ops
        | true ->
            match sanitizeOffsetAt table baseOff (start + index * width) width sanitize ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> sanitizeOffsets table baseOff start width count (index + 1) sanitize ops

    /// <summary>Sanitizes an array of offset fields: the count, the range, then each target.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOff">The base each offset is relative to.</param>
    /// <param name="countPos">The count field offset.</param>
    /// <param name="countWidth">The count width.</param>
    /// <param name="offsetsPos">The first offset field offset.</param>
    /// <param name="offsetWidth">The offset width.</param>
    /// <param name="sanitize">The target sanitizer.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeOffsetArray<'S when 'S : struct and 'S :> IOffsetSanitizer>
            (table: ReadOnlySpan<byte>) (baseOff: int) (countPos: int) (countWidth: int) (offsetsPos: int) (offsetWidth: int) (sanitize: 'S) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 countPos) countWidth with
        | false -> ValueNone
        | true ->
            let count = int (readOffset table countPos countWidth)
            match Sanitize.range table.Length (int64 offsetsPos) (uint32 count * uint32 offsetWidth) ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> sanitizeOffsets table baseOff offsetsPos offsetWidth count 0 sanitize ops

    /// <summary>Sanitizes an AttachPoint table, an <c>Array16Of&lt;HBUINT16&gt;</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ap">The attach-point offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeAttachPoint (table: ReadOnlySpan<byte>) (ap: int) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 ap) 2 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 ap + 2L) (uint32 (Binary.u16 table ap) * 2u) ops

    /// <summary>Sanitizes the target of an attach-point offset.</summary>
    [<Struct>]
    type private AttachPointSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeAttachPoint table offset ops

    /// <summary>Sanitizes a Device table, <c>Device::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The device offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeDevice (table: ReadOnlySpan<byte>) (at: int) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 at + 4L) 2 with
        | false -> ValueNone
        | true ->
            let format = Binary.u16 table (at + 4)
            match format with
            | 0x8000us ->
                match Sanitize.fits table.Length (int64 at) 6 with
                | true -> ValueSome ops
                | false -> ValueNone
            | f when f >= 1us && f <= 3us ->
                let startSize = Binary.u16 table at
                let endSize = Binary.u16 table (at + 2)
                let words =
                    match startSize <= endSize with
                    | false -> 3
                    | true -> 4 + ((int endSize - int startSize) >>> (4 - int f))
                match Sanitize.fits table.Length (int64 at) (2 * words) with
                | true -> ValueSome ops
                | false -> ValueNone
            | _ -> ValueSome ops

    /// <summary>Sanitizes the target of a device offset.</summary>
    [<Struct>]
    type private DeviceSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeDevice table offset ops

    /// <summary>Sanitizes a CaretValue table, <c>CaretValue::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="cv">The caret-value offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeCaretValue (table: ReadOnlySpan<byte>) (cv: int) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 cv) 2 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table cv with
            | 1us | 2us ->
                match Sanitize.fits table.Length (int64 cv) 4 with
                | true -> ValueSome ops
                | false -> ValueNone
            | 3us ->
                match Sanitize.fits table.Length (int64 cv) 6 with
                | false -> ValueNone
                | true -> sanitizeOffsetAt table cv (cv + 4) 2 (DeviceSanitizer()) ops
            | _ -> ValueSome ops

    /// <summary>Sanitizes the target of a caret-value offset.</summary>
    [<Struct>]
    type private CaretValueSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeCaretValue table offset ops

    /// <summary>Sanitizes a LigGlyph table, <c>LigGlyph::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lg">The ligature-glyph offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeLigGlyph (table: ReadOnlySpan<byte>) (lg: int) (ops: int) : int voption =
        sanitizeOffsetArray table lg lg 2 (lg + 2) 2 (CaretValueSanitizer()) ops

    /// <summary>Sanitizes the target of a ligature-glyph offset.</summary>
    [<Struct>]
    type private LigGlyphSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeLigGlyph table offset ops

    /// <summary>Sanitizes a LigCaretList table, <c>LigCaretList&lt;Types&gt;::sanitize</c>.</summary>
    /// <param name="wide">Whether the 24-bit medium form is in use.</param>
    /// <param name="table">The bytes.</param>
    /// <param name="ll">The ligature-caret-list offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeLigCaretList (wide: bool) (table: ReadOnlySpan<byte>) (ll: int) (ops: int) : int voption =
        match wide with
        | false ->
            match sanitizeOffsetAt table ll ll 2 (CoverageSanitizer()) ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> sanitizeOffsetArray table ll (ll + 2) 2 (ll + 4) 2 (LigGlyphSanitizer()) ops
        | true ->
            match sanitizeOffsetAt table ll ll 4 (CoverageSanitizer()) ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> sanitizeOffsetArray table ll (ll + 4) 3 (ll + 7) 3 (LigGlyphSanitizer()) ops

    /// <summary>Sanitizes the target of a 16-bit ligature-caret-list offset.</summary>
    [<Struct>]
    type private LigCaretList16Sanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeLigCaretList false table offset ops

    /// <summary>Sanitizes the target of a 24-bit ligature-caret-list offset.</summary>
    [<Struct>]
    type private LigCaretList24Sanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeLigCaretList true table offset ops

    /// <summary>Sanitizes an AttachList table, <c>AttachList::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="al">The attach-list offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeAttachList (table: ReadOnlySpan<byte>) (al: int) (ops: int) : int voption =
        match sanitizeOffsetAt table al al 2 (CoverageSanitizer()) ops with
        | ValueNone -> ValueNone
        | ValueSome ops -> sanitizeOffsetArray table al (al + 2) 2 (al + 4) 2 (AttachPointSanitizer()) ops

    /// <summary>Sanitizes the target of an attach-list offset.</summary>
    [<Struct>]
    type private AttachListSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeAttachList table offset ops

    /// <summary>Sanitizes a MarkGlyphSets table, <c>MarkGlyphSets::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="mgs">The mark-glyph-sets offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeMarkGlyphSetsTable (table: ReadOnlySpan<byte>) (mgs: int) (ops: int) : int voption =
        match Sanitize.fits table.Length (int64 mgs) 2 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table mgs with
            | 1us -> sanitizeOffsetArray table mgs (mgs + 2) 2 (mgs + 4) 4 (CoverageSanitizer()) ops
            | _ -> ValueSome ops

    /// <summary>Sanitizes the target of a mark-glyph-sets offset.</summary>
    [<Struct>]
    type private MarkGlyphSetsSanitizer =
        interface IOffsetSanitizer with
            member _.Sanitize (table, offset, ops) = sanitizeMarkGlyphSetsTable table offset ops

    /// <summary>Sanitizes a field that has a 16-bit offset and, from 1.4, a winning 32-bit offset.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The GDEF version.</param>
    /// <param name="pos16">The 16-bit offset field.</param>
    /// <param name="pos32">The 32-bit offset field.</param>
    /// <param name="sanitize16">The 16-bit target sanitizer.</param>
    /// <param name="sanitize32">The 32-bit target sanitizer.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeDual<'S16, 'S32 when 'S16 : struct and 'S16 :> IOffsetSanitizer and 'S32 : struct and 'S32 :> IOffsetSanitizer>
            (table: ReadOnlySpan<byte>) (version: uint32) (pos16: int) (pos32: int) (sanitize16: 'S16) (sanitize32: 'S32) (ops: int) : int voption =
        match version >= V1_4 && Binary.u32 table pos32 <> 0u with
        | true -> sanitizeOffsetAt table 0 pos32 4 sanitize32 ops
        | false -> sanitizeOffsetAt table 0 pos16 2 sanitize16 ops

    /// <summary>Sanitizes the variation-store offset, <c>ItemVariationStore::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The GDEF version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeVarStoreField (table: ReadOnlySpan<byte>) (version: uint32) (ops: int) : int voption =
        match version >= V1_3 && Binary.u32 table 14 <> 0u with
        | false -> ValueSome ops
        | true -> ItemVariationStore.sanitize table (int (Binary.u32 table 14)) ops

    /// <summary>Sanitizes the mark-glyph-sets field, gated on version 1.2.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The GDEF version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeMarkGlyphSetsField (table: ReadOnlySpan<byte>) (version: uint32) (ops: int) : int voption =
        let markSets =
            match version >= V1_4 && Binary.u32 table 34 <> 0u with
            | true -> sanitizeOffsetAt table 0 34 4 (MarkGlyphSetsSanitizer()) ops
            | false ->
                match version >= V1_2 with
                | true -> sanitizeOffsetAt table 0 12 2 (MarkGlyphSetsSanitizer()) ops
                | false -> ValueSome ops
        match markSets with
        | ValueNone -> ValueNone
        | ValueSome ops -> sanitizeVarStoreField table version ops

    /// <summary>Sanitizes the mark-attach class definition field.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The GDEF version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeMarkAttachClass (table: ReadOnlySpan<byte>) (version: uint32) (ops: int) : int voption =
        match sanitizeDual table version 10 30 (ClassDefSanitizer()) (ClassDefSanitizer()) ops with
        | ValueNone -> ValueNone
        | ValueSome ops -> sanitizeMarkGlyphSetsField table version ops

    /// <summary>Sanitizes the ligature-caret-list field.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The GDEF version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeLigCaretField (table: ReadOnlySpan<byte>) (version: uint32) (ops: int) : int voption =
        match sanitizeDual table version 8 26 (LigCaretList16Sanitizer()) (LigCaretList24Sanitizer()) ops with
        | ValueNone -> ValueNone
        | ValueSome ops -> sanitizeMarkAttachClass table version ops

    /// <summary>Sanitizes the attach-list field.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The GDEF version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeAttachListField (table: ReadOnlySpan<byte>) (version: uint32) (ops: int) : int voption =
        match sanitizeDual table version 6 22 (AttachListSanitizer()) (AttachListSanitizer()) ops with
        | ValueNone -> ValueNone
        | ValueSome ops -> sanitizeLigCaretField table version ops

    /// <summary>Sanitizes the glyph-class-definition field.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The GDEF version.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeGlyphClassField (table: ReadOnlySpan<byte>) (version: uint32) (ops: int) : int voption =
        match sanitizeDual table version 4 18 (ClassDefSanitizer()) (ClassDefSanitizer()) ops with
        | ValueNone -> ValueNone
        | ValueSome ops -> sanitizeAttachListField table version ops

    /// <summary>Sanitizes the GDEF table, <c>GDEF::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        match Sanitize.fits table.Length 0L 4 with
        | false -> ValueNone
        | true ->
            let major = Binary.u16 table 0
            match major with
            | 1us ->
                let version = (uint32 major <<< 16) ||| uint32 (Binary.u16 table 2)
                match Sanitize.fits table.Length 0L (headerSize version) with
                | false -> ValueNone
                | true -> sanitizeGlyphClassField table version ops
            | _ -> ValueNone

    /// <summary>The winning offset: the 32-bit field when the version is 1.4 and it is non-zero.</summary>
    /// <param name="version">The GDEF version.</param>
    /// <param name="off16">The 16-bit offset.</param>
    /// <param name="off32">The 32-bit offset.</param>
    /// <returns>The winning offset, or zero.</returns>
    let private winningOffset (version: uint32) (off16: uint32) (off32: uint32) : uint32 =
        match version >= V1_4 && off32 <> 0u with
        | true -> off32
        | false -> off16

    /// <summary>Parses a class definition at an offset, null when the offset is zero.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The offset.</param>
    /// <returns>The class definition.</returns>
    let private parseClassDef (table: ReadOnlySpan<byte>) (offset: uint32) : ClassDef =
        match offset with
        | 0u -> ClassDefNull
        | _ -> ClassDef.parse table (int offset)

    /// <summary>Decodes the coverage tables of a MarkGlyphSets table.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private MarkGlyphSetsUnfolder =
        { Base: int }
        interface ISpanUnfolder<byte, int, Coverage> with
            member this.Next (span, index) =
                let coverage =
                    match Binary.u32 span (this.Base + 4 + index * 4) with
                    | 0u -> CoverageNull
                    | value -> Coverage.parse span (this.Base + int value)
                struct (coverage, index + 1)

    /// <summary>Decodes the coverage tables of a MarkGlyphSets table, empty when absent.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The mark-glyph-sets offset, zero when absent.</param>
    /// <returns>The coverages.</returns>
    let private parseMarkGlyphSets (table: ReadOnlySpan<byte>) (offset: uint32) : ImmutableArray<Coverage> =
        match offset with
        | 0u -> ImmutableArray.Empty
        | _ ->
            match Binary.u16 table (int offset) with
            | 1us ->
                let count = int (Binary.u16 table (int offset + 2))
                Sealed.unfoldSpan table count 0 { MarkGlyphSetsUnfolder.Base = int offset }
            | _ -> ImmutableArray.Empty

    /// <summary>Computes the glyph props of a glyph, <c>GDEF::get_glyph_props</c>.</summary>
    /// <param name="glyphClasses">The glyph class definition.</param>
    /// <param name="markAttachClasses">The mark attachment class definition.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The props.</returns>
    let private propsFrom (glyphClasses: ClassDef) (markAttachClasses: ClassDef) (glyph: uint32) : uint16 =
        match ClassDef.classOf glyphClasses glyph with
        | 1u -> GlyphPropsBits.Base
        | 2u -> GlyphPropsBits.Ligature
        | 3u -> GlyphPropsBits.Mark ||| uint16 (ClassDef.classOf markAttachClasses glyph <<< 8)
        | _ -> 0us

    /// <summary>The empty GDEF, the Null object read from a missing or failed table.</summary>
    /// <param name="glyphCount">The face's glyph count.</param>
    /// <returns>The empty GDEF.</returns>
    let private empty (glyphCount: int) : Gdef =
        { Version = 0u
          GlyphClasses = ClassDefNull
          MarkAttachClasses = ClassDefNull
          MarkGlyphSets = ImmutableArray.Empty
          LigCarets = ReadOnlyMemory<byte>.Empty
          AttachList = ReadOnlyMemory<byte>.Empty
          HasVarStore = false
          HasGlyphClasses = false
          LigCaretsWide = false
          GlyphProps = Sealed.init glyphCount (fun _ -> 0us) }

    /// <summary>Finds the blocklisted triple matching the three table lengths, <c>GDEF::is_blocklisted</c>.</summary>
    /// <param name="index">The next triple.</param>
    /// <param name="gdefLength">The GDEF length.</param>
    /// <param name="gsubLength">The GSUB length.</param>
    /// <param name="gposLength">The GPOS length.</param>
    /// <returns>True when the font's GDEF must be emptied.</returns>
    [<TailCall>]
    let rec private blocklistedFrom (index: int) (gdefLength: int) (gsubLength: int) (gposLength: int) : bool =
        match index < blocklistTriples.Length with
        | false -> false
        | true ->
            let struct (g, s, p) = blocklistTriples[index]
            match g = gdefLength && s = gsubLength && p = gposLength with
            | true -> true
            | false -> blocklistedFrom (index + 1) gdefLength gsubLength gposLength

    /// <summary>Whether a face's GDEF is blocklisted by its table lengths.</summary>
    /// <param name="face">The face.</param>
    /// <param name="gdefLength">The GDEF length.</param>
    /// <returns>True when blocklisted.</returns>
    let private blocklisted (face: Face) (gdefLength: int) : bool =
        let gsubLength = LayoutTable.sanitizedLength face Tags.Gsub
        let gposLength = LayoutTable.sanitizedLength face Tags.Gpos
        blocklistedFrom 0 gdefLength gsubLength gposLength

    /// <summary>Parses a sanitized GDEF table.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The sanitized table bytes.</param>
    /// <returns>The GDEF.</returns>
    let private parse (face: Face) (table: ReadOnlyMemory<byte>) : Gdef =
        let span = table.Span
        let version = (uint32 (Binary.u16 span 0) <<< 16) ||| uint32 (Binary.u16 span 2)
        let glyphClassDef = winningOffset version (uint32 (Binary.u16 span 4)) (Binary.u32 span 18)
        let attachList = winningOffset version (uint32 (Binary.u16 span 6)) (Binary.u32 span 22)
        let ligCaretList = winningOffset version (uint32 (Binary.u16 span 8)) (Binary.u32 span 26)
        let markAttachClassDef = winningOffset version (uint32 (Binary.u16 span 10)) (Binary.u32 span 30)
        let markGlyphSetsDef =
            match version >= V1_2 with
            | false -> 0u
            | true -> winningOffset version (uint32 (Binary.u16 span 12)) (Binary.u32 span 34)
        let glyphClasses = parseClassDef span glyphClassDef
        let markAttachClasses = parseClassDef span markAttachClassDef
        { Version = version
          GlyphClasses = glyphClasses
          MarkAttachClasses = markAttachClasses
          MarkGlyphSets = parseMarkGlyphSets span markGlyphSetsDef
          LigCarets = match ligCaretList with | 0u -> ReadOnlyMemory<byte>.Empty | offset -> table.Slice(int offset)
          AttachList = match attachList with | 0u -> ReadOnlyMemory<byte>.Empty | offset -> table.Slice(int offset)
          HasVarStore = version >= V1_3 && Binary.u32 span 14 <> 0u
          HasGlyphClasses = glyphClassDef <> 0u
          LigCaretsWide = version >= V1_4 && Binary.u32 span 26 <> 0u
          GlyphProps = Sealed.init face.GlyphCount (fun glyph -> propsFrom glyphClasses markAttachClasses (uint32 glyph)) }

    /// <summary>Loads and sanitizes a face's GDEF table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The GDEF, empty when absent, invalid or blocklisted.</returns>
    let load (face: Face) : Gdef =
        let table = Face.referenceTable face Tags.Gdef
        match table.IsEmpty with
        | true -> empty face.GlyphCount
        | false ->
            match sanitize table.Span (Sanitize.start table.Length) with
            | ValueNone -> empty face.GlyphCount
            | ValueSome _ ->
                match blocklisted face table.Length with
                | true -> empty face.GlyphCount
                | false -> parse face table

    /// <summary>Reads the variation store of a sanitized, non-blocklisted GDEF table, empty when absent.</summary>
    /// <param name="table">The GDEF table bytes.</param>
    /// <returns>The item variation store.</returns>
    let private readVarStore (table: ReadOnlyMemory<byte>) : ItemVariationStore =
        let version = (uint32 (Binary.u16 table.Span 0) <<< 16) ||| uint32 (Binary.u16 table.Span 2)
        match version >= V1_3 && Binary.u32 table.Span 14 <> 0u with
        | false -> ItemVariationStore.empty
        | true -> ItemVariationStore.read table (int (Binary.u32 table.Span 14))

    /// <summary>Loads the GDEF variation store, empty when absent, invalid or blocklisted, <c>GDEF::get_var_store</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The item variation store.</returns>
    let loadVarStore (face: Face) : ItemVariationStore =
        let table = Face.referenceTable face Tags.Gdef
        match table.IsEmpty with
        | true -> ItemVariationStore.empty
        | false ->
            match sanitize table.Span (Sanitize.start table.Length) with
            | ValueNone -> ItemVariationStore.empty
            | ValueSome _ ->
                match blocklisted face table.Length with
                | true -> ItemVariationStore.empty
                | false -> readVarStore table

    /// <summary>Returns a glyph's props, precomputed for glyphs below the face's glyph count.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The props.</returns>
    let glyphProps (gdef: Gdef) (glyph: uint32) : uint16 =
        match uint64 glyph < uint64 gdef.GlyphProps.Length with
        | true -> gdef.GlyphProps[int glyph]
        | false -> propsFrom gdef.GlyphClasses gdef.MarkAttachClasses glyph

    /// <summary>Returns a glyph's class, <c>get_glyph_class</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The class, zero when unclassified.</returns>
    let glyphClass (gdef: Gdef) (glyph: uint32) : uint32 =
        ClassDef.classOf gdef.GlyphClasses glyph

    /// <summary>Whether a mark set covers a glyph, <c>mark_set_covers</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="set">The mark set index.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>True when covered.</returns>
    let markSetCovers (gdef: Gdef) (set: int) (glyph: uint32) : bool =
        match set >= 0 && set < gdef.MarkGlyphSets.Length with
        | false -> false
        | true ->
            match Coverage.index gdef.MarkGlyphSets[set] glyph with
            | CoverageHit.NotCovered -> false
            | CoverageHit.Covered _ -> true

    /// <summary>Clears an out-of-range mark filtering set index, <c>sanitize_lookup_props</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="lookupProps">The lookup props.</param>
    /// <returns>The sanitized props.</returns>
    let sanitizeLookupProps (gdef: Gdef) (lookupProps: uint32) : uint32 =
        match lookupProps &&& UseMarkFilteringSet <> 0u && (lookupProps >>> 16) >= uint32 gdef.MarkGlyphSets.Length with
        | true -> lookupProps &&& ~~~UseMarkFilteringSet
        | false -> lookupProps

    /// <summary>Whether the glyph class definition is present, <c>has_glyph_classes</c>.</summary>
    /// <param name="gdef">The GDEF.</param>
    /// <returns>True when the class definition offset is non-zero.</returns>
    let hasGlyphClasses (gdef: Gdef) : bool =
        gdef.HasGlyphClasses
