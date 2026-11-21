namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>Parses one subtable of a lookup, <c>SubstLookupSubTable::dispatch</c> to the format parser.</summary>
type internal SubtableParser<'S> =
    /// <summary>Parses the subtable at an offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="lookupType">The lookup's type.</param>
    /// <returns>The parsed subtable.</returns>
    delegate of table: ReadOnlySpan<byte> * offset: int * lookupType: int -> 'S

/// <summary>A language system, the required feature and the feature indices of <c>LangSys</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal LangSys =
    { RequiredFeature: uint32
      FeatureIndices: ImmutableArray<uint16> }

/// <summary>One script of a GSUB/GPOS table with its language systems, <c>Script</c>.</summary>
[<NoEquality; NoComparison>]
type internal ScriptRecord =
    { ScriptTag: Tag
      DefaultLangSys: LangSys voption
      LangSysTags: ImmutableArray<Tag>
      LangSys: ImmutableArray<LangSys> }

/// <summary>One feature of a GSUB/GPOS table with its lookup indices, <c>Feature</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal FeatureRecord =
    { FeatureTag: Tag
      LookupIndices: ImmutableArray<uint16> }

/// <summary>A parsed GSUB or GPOS table, <c>GSUBGPOS</c>.</summary>
[<NoEquality; NoComparison>]
type internal LayoutTable<'S> =
    { Version: uint32
      Scripts: ImmutableArray<ScriptRecord>
      Features: ImmutableArray<FeatureRecord>
      Lookups: ImmutableArray<Lazy<LookupAccel<'S>>>
      FeatureVariations: ReadOnlyMemory<byte> }

/// <summary>The GSUB/GPOS header, script, language, feature and lookup parsing and queries.</summary>
[<RequireQualifiedAccess>]
module internal LayoutTable =
    /// <summary>The version that adds the feature variations offset.</summary>
    [<Literal>]
    let private V1_1 = 0x00010001u

    /// <summary>The version that adds the three 32-bit list offsets.</summary>
    [<Literal>]
    let private V1_2 = 0x00010002u

    /// <summary>The header size of version 1.0.</summary>
    [<Literal>]
    let private Header10 = 10

    /// <summary>The header size of version 1.1.</summary>
    [<Literal>]
    let private Header14 = 14

    /// <summary>The header size of version 1.2 and later.</summary>
    [<Literal>]
    let private Header26 = 26

    /// <summary>The script index meaning none, <c>HB_OT_LAYOUT_NO_SCRIPT_INDEX</c>.</summary>
    [<Literal>]
    let NoScriptIndex = 0xFFFF

    /// <summary>The language index meaning the default language, <c>HB_OT_LAYOUT_DEFAULT_LANGUAGE_INDEX</c>.</summary>
    [<Literal>]
    let DefaultLanguageIndex = 0xFFFF

    /// <summary>The feature index meaning none, <c>HB_OT_LAYOUT_NO_FEATURE_INDEX</c>.</summary>
    [<Literal>]
    let NoFeatureIndex = 0xFFFF

    /// <summary>The lookup flag requesting a mark filtering set, <c>LookupFlag::UseMarkFilteringSet</c>.</summary>
    [<Literal>]
    let private UseMarkFilteringSet = 0x0010us

    /// <summary>The GSUB extension lookup type, <c>SubstLookupSubTable::Extension</c>.</summary>
    [<Literal>]
    let private GsubExtension = 7

    /// <summary>The GPOS extension lookup type, <c>PosLookupSubTable::Extension</c>.</summary>
    [<Literal>]
    let private GposExtension = 9

    /// <summary>The GSUB reverse-chain single lookup type, <c>SubstLookupSubTable::ReverseChainSingle</c>.</summary>
    [<Literal>]
    let private ReverseChainSingle = 8

    /// <summary>The sanitize subtable budget, <c>HB_SANITIZE_MAX_SUBTABLES</c>.</summary>
    [<Literal>]
    let private MaxSubtables = 0x4000

    /// <summary>The <c>size</c> feature tag.</summary>
    [<Literal>]
    let private SizeTag = 0x73697A65u

    /// <summary>The <c>ss</c> feature-tag prefix.</summary>
    [<Literal>]
    let private SsTag = 0x73730000u

    /// <summary>The <c>cv</c> feature-tag prefix.</summary>
    [<Literal>]
    let private CvTag = 0x63760000u

    /// <summary>The width of a 16-bit list offset.</summary>
    [<Literal>]
    let private Width16 = 2

    /// <summary>The width of a 24-bit list offset.</summary>
    [<Literal>]
    let private Width24 = 3

    /// <summary>The sanitize budgets: remaining operations and remaining subtables.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Budget =
        { Ops: int
          Subtables: int }

    /// <summary>The empty GSUB/GPOS table, read from a missing or failed table.</summary>
    let private empty<'S> : LayoutTable<'S> =
        { Version = 0u
          Scripts = ImmutableArray.Empty
          Features = ImmutableArray.Empty
          Lookups = ImmutableArray.Empty
          FeatureVariations = ReadOnlyMemory<byte>.Empty }

    /// <summary>The Null language system: no required feature and no features, <c>Null (LangSys)</c>.</summary>
    let private nullLangSys : LangSys =
        { RequiredFeature = uint32 NoFeatureIndex
          FeatureIndices = ImmutableArray.Empty }

    /// <summary>Compares a key tag with a record tag, as <c>Tag::cmp</c>.</summary>
    /// <param name="key">The search tag.</param>
    /// <param name="value">The record tag.</param>
    /// <returns>Negative when the key sorts first, positive when after, zero when equal.</returns>
    let private compareTags (key: Tag) (value: Tag) : int =
        match uint32 key > uint32 value, uint32 key < uint32 value with
        | true, _ -> 1
        | false, true -> -1
        | false, false -> 0

    /// <summary>Binary-searches the script records, <c>RecordArrayOf&lt;Script&gt;::bfind</c>.</summary>
    /// <param name="scripts">The scripts in file order.</param>
    /// <param name="tag">The key.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private bfindScript (scripts: ReadOnlySpan<ScriptRecord>) (tag: Tag) (lo: int) (hi: int) : int voption =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match compareTags tag scripts[mid].ScriptTag with
            | -1 -> bfindScript scripts tag lo (mid - 1)
            | 1 -> bfindScript scripts tag (mid + 1) hi
            | _ -> ValueSome mid

    /// <summary>Binary-searches a script's language-system tags, <c>RecordArrayOf&lt;LangSys&gt;::bfind</c>.</summary>
    /// <param name="tags">The tags in file order.</param>
    /// <param name="tag">The key.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private bfindLangSys (tags: ReadOnlySpan<Tag>) (tag: Tag) (lo: int) (hi: int) : int voption =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match compareTags tag tags[mid] with
            | -1 -> bfindLangSys tags tag lo (mid - 1)
            | 1 -> bfindLangSys tags tag (mid + 1) hi
            | _ -> ValueSome mid

    /// <summary>Checks that a fixed-size struct fits, charging no operations.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The struct offset.</param>
    /// <param name="size">The struct size.</param>
    /// <returns>True when the struct lies inside the table.</returns>
    let private fitsStruct (table: ReadOnlySpan<byte>) (offset: int64) (size: int) : bool =
        Sanitize.fits table.Length offset size

    /// <summary>Checks a byte range and charges its length, <c>check_range</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The range offset.</param>
    /// <param name="count">The byte count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none when the range or the budget fails.</returns>
    let private fitsRange (table: ReadOnlySpan<byte>) (offset: int64) (count: uint32) (budget: Budget) : Budget voption =
        match Sanitize.range table.Length offset count budget.Ops with
        | ValueNone -> ValueNone
        | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Charges a lookup's subtables against the subtable budget, <c>visit_subtables</c>.</summary>
    /// <param name="budget">The budgets.</param>
    /// <param name="count">The subtable count.</param>
    /// <returns>The remaining budgets, or none when the budget is exhausted.</returns>
    let private chargeSubtables (budget: Budget) (count: int) : Budget voption =
        match budget.Subtables + count < MaxSubtables with
        | true -> ValueSome { budget with Subtables = budget.Subtables + count }
        | false -> ValueNone

    /// <summary>Sanitizes a language system, <c>LangSys::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ls">The language-system offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLangSys (table: ReadOnlySpan<byte>) (ls: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 ls) 6 with
        | false -> ValueNone
        | true -> fitsRange table (int64 ls + 6L) (uint32 (Binary.u16 table (ls + 4)) * 2u) budget

    /// <summary>Sanitizes one language-system record of a script, <c>Record&lt;LangSys&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The script offset the record offset is relative to.</param>
    /// <param name="record">The record offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLangSysRecord (table: ReadOnlySpan<byte>) (baseOffset: int) (record: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 record) 6 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table (record + 4) with
            | 0us -> ValueSome budget
            | offset -> sanitizeLangSys table (baseOffset + int offset) budget

    /// <summary>Sanitizes each language-system record of a script, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The script offset the record offsets are relative to.</param>
    /// <param name="records">The records offset.</param>
    /// <param name="count">The number of records.</param>
    /// <param name="index">The next record.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeLangSysRecords (table: ReadOnlySpan<byte>) (baseOffset: int) (records: int) (count: int) (index: int) (budget: Budget) : Budget voption =
        match index < count with
        | false -> ValueSome budget
        | true ->
            match sanitizeLangSysRecord table baseOffset (records + index * 6) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeLangSysRecords table baseOffset records count (index + 1) budget

    /// <summary>Sanitizes a script's language-system records, <c>Script::sanitize</c> after the default language system.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="s">The script offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeScriptLangSys (table: ReadOnlySpan<byte>) (s: int) (budget: Budget) : Budget voption =
        match fitsRange table (int64 s + 4L) (uint32 (Binary.u16 table (s + 2)) * 6u) budget with
        | ValueNone -> ValueNone
        | ValueSome budget -> sanitizeLangSysRecords table s (s + 4) (int (Binary.u16 table (s + 2))) 0 budget

    /// <summary>Sanitizes a script, <c>Script::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="s">The script offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeScript (table: ReadOnlySpan<byte>) (s: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 s) 4 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table s with
            | 0us -> sanitizeScriptLangSys table s budget
            | defaultLangSys ->
                match sanitizeLangSys table (s + int defaultLangSys) budget with
                | ValueNone -> ValueNone
                | ValueSome budget -> sanitizeScriptLangSys table s budget

    /// <summary>Sanitizes one script record of a script list, <c>Record&lt;Script&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="sl">The script-list offset.</param>
    /// <param name="record">The record offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeScriptRecord (table: ReadOnlySpan<byte>) (sl: int) (record: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 record) 6 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table (record + 4) with
            | 0us -> ValueSome budget
            | offset -> sanitizeScript table (sl + int offset) budget

    /// <summary>Sanitizes each script record of a script list, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="sl">The script-list offset.</param>
    /// <param name="count">The number of records.</param>
    /// <param name="index">The next record.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeScriptRecords (table: ReadOnlySpan<byte>) (sl: int) (count: int) (index: int) (budget: Budget) : Budget voption =
        match index < count with
        | false -> ValueSome budget
        | true ->
            match sanitizeScriptRecord table sl (sl + 2 + index * 6) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeScriptRecords table sl count (index + 1) budget

    /// <summary>Sanitizes a script list, <c>RecordListOfScript::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="sl">The script-list offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeScriptList (table: ReadOnlySpan<byte>) (sl: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 sl) 2 with
        | false -> ValueNone
        | true ->
            match fitsRange table (int64 sl + 2L) (uint32 (Binary.u16 table sl) * 6u) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeScriptRecords table sl (int (Binary.u16 table sl)) 0 budget

    /// <summary>Sanitizes the <c>size</c> feature parameters, <c>FeatureParamsSize::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fp">The parameters offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSizeParams (table: ReadOnlySpan<byte>) (fp: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 fp) 10 with
        | false -> ValueNone
        | true ->
            let designSize = Binary.u16 table fp
            let subfamilyID = Binary.u16 table (fp + 2)
            let subfamilyNameID = Binary.u16 table (fp + 4)
            let rangeStart = Binary.u16 table (fp + 6)
            let rangeEnd = Binary.u16 table (fp + 8)
            match designSize = 0us,
                  subfamilyID = 0us && subfamilyNameID = 0us && rangeStart = 0us && rangeEnd = 0us,
                  designSize < rangeStart || designSize > rangeEnd || subfamilyNameID < 256us || subfamilyNameID > 32767us with
            | true, _, _ -> ValueNone
            | false, true, _ -> ValueSome budget
            | false, false, true -> ValueNone
            | false, false, false -> ValueSome budget

    /// <summary>Sanitizes the <c>ssXX</c> feature parameters, <c>FeatureParamsStylisticSet::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fp">The parameters offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeStylisticSet (table: ReadOnlySpan<byte>) (fp: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 fp) 4 with
        | false -> ValueNone
        | true -> ValueSome budget

    /// <summary>Sanitizes the <c>cvXX</c> feature parameters, <c>FeatureParamsCharacterVariants::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fp">The parameters offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeCharacterVariants (table: ReadOnlySpan<byte>) (fp: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 fp) 14 with
        | false -> ValueNone
        | true -> fitsRange table (int64 fp + 14L) (uint32 (Binary.u16 table (fp + 12)) * 3u) budget

    /// <summary>Sanitizes a feature-parameters table, <c>FeatureParams::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fp">The parameters offset.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeFeatureParams (table: ReadOnlySpan<byte>) (fp: int) (tag: Tag) (budget: Budget) : Budget voption =
        match uint32 tag with
        | SizeTag -> sanitizeSizeParams table fp budget
        | tag when (uint32 tag &&& 0xFFFF0000u) = SsTag -> sanitizeStylisticSet table fp budget
        | tag when (uint32 tag &&& 0xFFFF0000u) = CvTag -> sanitizeCharacterVariants table fp budget
        | _ -> ValueSome budget

    /// <summary>Sanitizes a feature, <c>Feature::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="f">The feature offset.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeFeature (table: ReadOnlySpan<byte>) (f: int) (tag: Tag) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 f) 4 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table f with
            | 0us -> fitsRange table (int64 f + 4L) (uint32 (Binary.u16 table (f + 2)) * 2u) budget
            | offset ->
                match sanitizeFeatureParams table (f + int offset) tag budget with
                | ValueNone -> ValueNone
                | ValueSome budget -> fitsRange table (int64 f + 4L) (uint32 (Binary.u16 table (f + 2)) * 2u) budget

    /// <summary>Sanitizes one feature record of a feature list, <c>Record&lt;Feature&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fl">The feature-list offset.</param>
    /// <param name="record">The record offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeFeatureRecord (table: ReadOnlySpan<byte>) (fl: int) (record: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 record) 6 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table (record + 4) with
            | 0us -> ValueSome budget
            | offset -> sanitizeFeature table (fl + int offset) (Binary.tag table record) budget

    /// <summary>Sanitizes each feature record of a feature list, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fl">The feature-list offset.</param>
    /// <param name="count">The number of records.</param>
    /// <param name="index">The next record.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeFeatureRecords (table: ReadOnlySpan<byte>) (fl: int) (count: int) (index: int) (budget: Budget) : Budget voption =
        match index < count with
        | false -> ValueSome budget
        | true ->
            match sanitizeFeatureRecord table fl (fl + 2 + index * 6) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeFeatureRecords table fl count (index + 1) budget

    /// <summary>Sanitizes a feature list, <c>RecordListOfFeature::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fl">The feature-list offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeFeatureList (table: ReadOnlySpan<byte>) (fl: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 fl) 2 with
        | false -> ValueNone
        | true ->
            match fitsRange table (int64 fl + 2L) (uint32 (Binary.u16 table fl) * 6u) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeFeatureRecords table fl (int (Binary.u16 table fl)) 0 budget

    /// <summary>Reads an extension subtable's extended type, or none when it extends to Extension.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="subtableOffset">The subtable offset, relative to the lookup.</param>
    /// <param name="extensionType">The extension lookup type (7 for GSUB, 9 for GPOS).</param>
    /// <returns>The extended type, or none when the subtable extends to Extension.</returns>
    let private readExtensionType (table: ReadOnlySpan<byte>) (lk: int) (subtableOffset: int) (extensionType: int) : int voption =
        match subtableOffset with
        | 0 -> ValueSome 0
        | offset ->
            let at = lk + offset
            match Binary.u16 table at with
            | 1us ->
                let extended = int (Binary.u16 table (at + 2))
                match extended = extensionType with
                | true -> ValueNone
                | false -> ValueSome extended
            | _ -> ValueSome 0

    /// <summary>Checks that an Extension lookup's subtables share one type, <c>Lookup::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="firstType">The first subtable's extended type.</param>
    /// <param name="haveFirst">Whether a first type is known.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeExtensionSubtables
            (table: ReadOnlySpan<byte>) (lk: int) (count: int) (index: int) (firstType: int) (haveFirst: bool) (extensionType: int) (budget: Budget) : Budget voption =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let subtableOffset = int (Binary.u16 table (lk + 6 + index * 2))
            match readExtensionType table lk subtableOffset extensionType with
            | ValueNone -> ValueNone
            | ValueSome extended ->
                match haveFirst && extended <> firstType with
                | true -> ValueNone
                | false -> sanitizeExtensionSubtables table lk count (index + 1) extended true extensionType budget

    /// <summary>Resolves a relative offset against a base, none for a Null (zero) offset.</summary>
    /// <param name="baseOffset">The base the offset is relative to.</param>
    /// <param name="offsetValue">The offset value.</param>
    /// <returns>The absolute offset, or none.</returns>
    let private resolveOffset baseOffset offsetValue =
        match offsetValue with
        | 0u -> ValueNone
        | _ -> ValueSome (int64 baseOffset + int64 offsetValue)

    /// <summary>Whether a GSUB subtable format uses 24-bit data and 32-bit coverage offsets, <c>MediumTypes</c>.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="format">The subtable format.</param>
    /// <returns>True for a medium-type format.</returns>
    let private mediumFormat lookupType format =
        match lookupType with
        | 1 -> format >= 3
        | 2 | 3 | 4 | 8 -> format >= 2
        | 5 | 6 -> format >= 4
        | _ -> false

    /// <summary>The byte width of a subtable's data (glyphs, counts, subtable offsets) for its format.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="format">The subtable format.</param>
    /// <returns>Two or three bytes.</returns>
    let private dataWidth lookupType format =
        if mediumFormat lookupType format then Width24 else Width16

    /// <summary>The byte width of a subtable's coverage offset, <c>LOffsetTo</c>: 32-bit for medium formats.</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="format">The subtable format.</param>
    /// <returns>Two or four bytes.</returns>
    let private coverageWidth lookupType format =
        if mediumFormat lookupType format then 4 else Width16

    /// <summary>Reads a coverage offset of a subtable, <c>LOffsetTo</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The offset.</param>
    /// <param name="width">Two or four bytes.</param>
    /// <returns>The offset value.</returns>
    let private readCoverageOffset (table: ReadOnlySpan<byte>) offset width =
        match width with
        | 4 -> Binary.u32 table offset
        | _ -> uint32 (Binary.u16 table offset)

    /// <summary>Sanitizes a coverage reached through a relative offset, <c>Coverage::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The subtable offset.</param>
    /// <param name="offsetValue">The relative coverage offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeCoverageAt (table: ReadOnlySpan<byte>) baseOffset offsetValue budget =
        match resolveOffset baseOffset offsetValue with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true ->
                match Coverage.sanitize table (int at) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Charges a coverage's population against the budget, <c>check_ops (population &gt;&gt; 1)</c>.</summary>
    /// <param name="population">The coverage population.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private chargePopulation population budget =
        let charge = population >>> 1
        match charge > uint64 (uint32 budget.Ops) with
        | true -> ValueNone
        | false -> ValueSome { budget with Ops = budget.Ops - int charge }

    /// <summary>The population of a range coverage, <c>Coverage::get_population</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="covOffset">The coverage offset.</param>
    /// <param name="width">The glyph width.</param>
    /// <param name="count">The range count.</param>
    /// <param name="index">The next range.</param>
    /// <param name="acc">The population so far.</param>
    /// <returns>The population.</returns>
    [<TailCall>]
    let rec private rangePopulation (table: ReadOnlySpan<byte>) covOffset width count index acc =
        match index < count with
        | false -> acc
        | true ->
            let at = covOffset + 2 + width + index * 3 * width
            let first = Coverage.readWide table at width
            let last = Coverage.readWide table (at + width) width
            let size = match last >= first with | true -> uint64 last - uint64 first + 1UL | false -> 0UL
            rangePopulation table covOffset width count (index + 1) (acc + size)

    /// <summary>The population of a coverage, <c>Coverage::get_population</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="covOffset">The coverage offset.</param>
    /// <returns>The population.</returns>
    let private coveragePopulation (table: ReadOnlySpan<byte>) covOffset =
        match Binary.u16 table covOffset with
        | 1us -> uint64 (Binary.u16 table (covOffset + 2))
        | 3us -> uint64 (Binary.u24 table (covOffset + 2))
        | 2us -> rangePopulation table covOffset Width16 (int (Binary.u16 table (covOffset + 2))) 0 0UL
        | 4us -> rangePopulation table covOffset Width24 (int (Binary.u24 table (covOffset + 2))) 0 0UL
        | _ -> 0xFFFFFFFFUL

    /// <summary>The population of a single-substitution delta subtable's coverage, <c>get_population</c>: the Null
    /// coverage is <c>NOT_COVERED</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="coverageOffset">The relative coverage offset.</param>
    /// <returns>The population.</returns>
    let private singlePopulation (table: ReadOnlySpan<byte>) o coverageOffset =
        match coverageOffset with
        | 0u -> 0xFFFFFFFFUL
        | _ -> coveragePopulation table (o + int coverageOffset)

    /// <summary>Sanitizes a single-substitution delta subtable, format 1/3, <c>SingleSubstFormat1_3::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The data width.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSingleDelta (table: ReadOnlySpan<byte>) o w covW budget =
        match Sanitize.fits table.Length (int64 o) (2 + covW + w) with
        | false -> ValueNone
        | true ->
            let coverageOffset = readCoverageOffset table (o + 2) covW
            match sanitizeCoverageAt table o coverageOffset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> chargePopulation (singlePopulation table o coverageOffset) budget

    /// <summary>Sanitizes a single-substitution list subtable, format 2/4, <c>SingleSubstFormat2_4::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The data width.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSingleList (table: ReadOnlySpan<byte>) o w covW budget =
        match Sanitize.fits table.Length (int64 o) (2 + covW + w) with
        | false -> ValueNone
        | true ->
            let coverageOffset = readCoverageOffset table (o + 2) covW
            match sanitizeCoverageAt table o coverageOffset budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                match Sanitize.array table.Length (int64 (o + 2 + covW + w)) (Coverage.readWide table (o + 2 + covW) w) (uint32 w) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes a single-substitution subtable, <c>SingleSubst::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSingle (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let w = dataWidth 1 format
            let covW = coverageWidth 1 format
            match format with
            | 1 | 3 -> sanitizeSingleDelta table o w covW budget
            | 2 | 4 -> sanitizeSingleList table o w covW budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes a sequence or alternate set, <c>Array16Of&lt;HBGlyphID&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The table offset.</param>
    /// <param name="w">The glyph width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeGlyphSequence (table: ReadOnlySpan<byte>) o w budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            match Sanitize.array table.Length (int64 (o + 2)) (uint32 (Binary.u16 table o)) (uint32 w) budget.Ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes a glyph sequence reached through a relative offset, <c>OffsetTo&lt;Sequence&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The subtable offset.</param>
    /// <param name="w">The glyph width.</param>
    /// <param name="offset">The relative offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReachedGlyphs (table: ReadOnlySpan<byte>) baseOffset w offset budget =
        match resolveOffset baseOffset offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeGlyphSequence table (int at) w budget

    /// <summary>Sanitizes the sequences of a multiple-substitution subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="count">The sequence count.</param>
    /// <param name="index">The next sequence.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeSequences (table: ReadOnlySpan<byte>) o covW w count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = Coverage.readWide table (o + 2 + covW + w + index * w) w
            match sanitizeReachedGlyphs table o w offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeSequences table o covW w count (index + 1) budget

    /// <summary>Sanitizes the body of a multiple-substitution subtable, <c>MultipleSubstFormat1::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The data width.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMultipleBody (table: ReadOnlySpan<byte>) o w covW budget =
        match Sanitize.fits table.Length (int64 o) (2 + covW + w) with
        | false -> ValueNone
        | true ->
            let coverageOffset = readCoverageOffset table (o + 2) covW
            match sanitizeCoverageAt table o coverageOffset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeSequences table o covW w (int (Coverage.readWide table (o + 2 + covW) w)) 0 budget

    /// <summary>Sanitizes a multiple-substitution subtable, <c>MultipleSubst::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMultiple (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let w = dataWidth 2 format
            let covW = coverageWidth 2 format
            match format with
            | 1 | 2 -> sanitizeMultipleBody table o w covW budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes the alternate sets of an alternate-substitution subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="count">The alternate-set count.</param>
    /// <param name="index">The next set.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeAlternateSets (table: ReadOnlySpan<byte>) o covW w count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = Coverage.readWide table (o + 2 + covW + w + index * w) w
            match sanitizeReachedGlyphs table o w offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeAlternateSets table o covW w count (index + 1) budget

    /// <summary>Sanitizes the body of an alternate-substitution subtable, <c>AlternateSubstFormat1::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The data width.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAlternateBody (table: ReadOnlySpan<byte>) o w covW budget =
        match Sanitize.fits table.Length (int64 o) (2 + covW + w) with
        | false -> ValueNone
        | true ->
            let coverageOffset = readCoverageOffset table (o + 2) covW
            match sanitizeCoverageAt table o coverageOffset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeAlternateSets table o covW w (int (Coverage.readWide table (o + 2 + covW) w)) 0 budget

    /// <summary>Sanitizes an alternate-substitution subtable, <c>AlternateSubst::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAlternate (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let w = dataWidth 3 format
            let covW = coverageWidth 3 format
            match format with
            | 1 | 2 -> sanitizeAlternateBody table o w covW budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes one ligature, <c>Ligature::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lig">The ligature offset.</param>
    /// <param name="w">The glyph width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLigatureGlyph (table: ReadOnlySpan<byte>) lig w budget =
        match fitsStruct table (int64 lig) (w + 2) with
        | false -> ValueNone
        | true ->
            let componentCount = int (Binary.u16 table (lig + w))
            match componentCount with
            | 0 -> ValueSome budget
            | _ ->
                match Sanitize.array table.Length (int64 (lig + w + 2)) (uint32 (componentCount - 1)) (uint32 w) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes a ligature reached through a relative offset, <c>OffsetTo&lt;Ligature&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The base the offset is relative to.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="offset">The relative ligature offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReachedLigatureGlyph (table: ReadOnlySpan<byte>) baseOffset w offset budget =
        match resolveOffset baseOffset offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeLigatureGlyph table (int at) w budget

    /// <summary>Sanitizes each ligature of a ligature set, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="set">The ligature-set offset.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="count">The ligature count.</param>
    /// <param name="index">The next ligature.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeLigatures (table: ReadOnlySpan<byte>) set w count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = Coverage.readWide table (set + 2 + index * w) w
            match sanitizeReachedLigatureGlyph table set w offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeLigatures table set w count (index + 1) budget

    /// <summary>Sanitizes a ligature set, <c>LigatureSet::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="set">The ligature-set offset.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLigatureSet (table: ReadOnlySpan<byte>) set w budget =
        match fitsStruct table (int64 set) 2 with
        | false -> ValueNone
        | true ->
            let count = int (Binary.u16 table set)
            match fitsRange table (int64 (set + 2)) (uint32 count * uint32 w) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeLigatures table set w count 0 budget

    /// <summary>Sanitizes a ligature set reached through a relative offset, <c>OffsetTo&lt;LigatureSet&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The base the offset is relative to.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="offset">The relative set offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReachedLigatureSet (table: ReadOnlySpan<byte>) baseOffset w offset budget =
        match resolveOffset baseOffset offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeLigatureSet table (int at) w budget

    /// <summary>Sanitizes each ligature set of a ligature subtable, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="w">The offset width.</param>
    /// <param name="count">The ligature-set count.</param>
    /// <param name="index">The next set.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeLigatureSets (table: ReadOnlySpan<byte>) o covW w count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = Coverage.readWide table (o + 2 + covW + w + index * w) w
            match sanitizeReachedLigatureSet table o w offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeLigatureSets table o covW w count (index + 1) budget

    /// <summary>Sanitizes the body of a ligature-substitution subtable, <c>LigatureSubstFormat1_2::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The data width.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLigatureBody (table: ReadOnlySpan<byte>) o w covW budget =
        match fitsStruct table (int64 o) (2 + covW + w) with
        | false -> ValueNone
        | true ->
            let coverageOffset = readCoverageOffset table (o + 2) covW
            match sanitizeCoverageAt table o coverageOffset budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                let setCount = int (Coverage.readWide table (o + 2 + covW) w)
                match fitsRange table (int64 (o + 2 + covW + w)) (uint32 setCount * uint32 w) budget with
                | ValueNone -> ValueNone
                | ValueSome budget -> sanitizeLigatureSets table o covW w setCount 0 budget

    /// <summary>Sanitizes a ligature-substitution subtable, <c>LigatureSubst::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLigature (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let w = dataWidth 4 format
            let covW = coverageWidth 4 format
            match format with
            | 1 | 2 -> sanitizeLigatureBody table o w covW budget
            | _ -> ValueSome budget

    /// <summary>The byte size of a value record, <c>ValueFormat::get_size</c>.</summary>
    /// <param name="format">The value format.</param>
    /// <returns>The size in bytes.</returns>
    let private valueSize (format: uint16) =
        2 * System.Numerics.BitOperations.PopCount (uint32 (format &&& 0x00FFus))

    /// <summary>Sanitizes a class definition reached through a relative offset, <c>ClassDef::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The subtable offset.</param>
    /// <param name="offsetValue">The relative class-definition offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeClassDefAt (table: ReadOnlySpan<byte>) baseOffset offsetValue budget =
        match resolveOffset baseOffset offsetValue with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true ->
                match ClassDef.sanitize table (int at) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes a single-adjustment subtable of format 1 or 3, <c>SinglePosFormat1_3::sanitize</c>, with the
    /// devices left to their lazy check as the accelerator's sanitizer does.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSinglePosFixed (table: ReadOnlySpan<byte>) o covW budget =
        match Sanitize.fits table.Length (int64 o) (4 + covW) with
        | false -> ValueNone
        | true ->
            let coverageOffset = readCoverageOffset table (o + 2) covW
            match sanitizeCoverageAt table o coverageOffset budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                match chargePopulation (singlePopulation table o coverageOffset) budget with
                | ValueNone -> ValueNone
                | ValueSome budget ->
                    fitsRange table (int64 o + int64 (4 + covW)) (uint32 (valueSize (Binary.u16 table (o + 2 + covW)))) budget

    /// <summary>Sanitizes a single-adjustment subtable of format 2 or 4, <c>SinglePosFormat2_4::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="w">The width of the value count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSinglePosList (table: ReadOnlySpan<byte>) o covW w budget =
        match Sanitize.fits table.Length (int64 o) (4 + covW + w) with
        | false -> ValueNone
        | true ->
            match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                let count = Coverage.readWide table (o + 4 + covW) w
                match Sanitize.array table.Length (int64 (o + 4 + covW + w)) count (uint32 (valueSize (Binary.u16 table (o + 2 + covW)))) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes a single-adjustment subtable, <c>SinglePos::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSinglePos (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            match int (Binary.u16 table o) with
            | 1 -> sanitizeSinglePosFixed table o Width16 budget
            | 3 -> sanitizeSinglePosFixed table o 4 budget
            | 2 -> sanitizeSinglePosList table o Width16 Width16 budget
            | 4 -> sanitizeSinglePosList table o 4 Width24 budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes one pair set, <c>PairSet::sanitize</c> with the devices left to their lazy check.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="set">The pair set offset.</param>
    /// <param name="w">The width of the glyph ids and of the record count.</param>
    /// <param name="stride">The size of a record in bytes.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizePairSet (table: ReadOnlySpan<byte>) set w stride budget =
        match Sanitize.fits table.Length (int64 set) w with
        | false -> ValueNone
        | true ->
            match Sanitize.array table.Length (int64 (set + w)) (Coverage.readWide table set w) (uint32 stride) budget.Ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes a pair set reached through a relative offset, <c>OffsetTo&lt;PairSet&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The width of the glyph ids and of the record count.</param>
    /// <param name="stride">The size of a record in bytes.</param>
    /// <param name="offset">The relative offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReachedPairSet (table: ReadOnlySpan<byte>) o w stride offset budget =
        match resolveOffset o offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizePairSet table (int at) w stride budget

    /// <summary>Sanitizes the pair sets of a pair-adjustment subtable one after the other.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetsBase">The offset of the first pair-set offset.</param>
    /// <param name="w">The width of the offsets.</param>
    /// <param name="stride">The size of a record in bytes.</param>
    /// <param name="count">The number of pair sets.</param>
    /// <param name="index">The next pair set.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizePairSets (table: ReadOnlySpan<byte>) o offsetsBase w stride count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = Coverage.readWide table (offsetsBase + index * w) w
            match sanitizeReachedPairSet table o w stride offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizePairSets table o offsetsBase w stride count (index + 1) budget

    /// <summary>Sanitizes a pair-adjustment subtable of format 1 or 3, <c>PairPosFormat1_3::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The coverage-offset width.</param>
    /// <param name="w">The width of the glyph ids, offsets and counts.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizePairPosGlyphs (table: ReadOnlySpan<byte>) o covW w budget =
        match Sanitize.fits table.Length (int64 o) (6 + covW + w) with
        | false -> ValueNone
        | true ->
            let stride = w + valueSize (Binary.u16 table (o + 2 + covW)) + valueSize (Binary.u16 table (o + 4 + covW))
            match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                let count = Coverage.readWide table (o + 6 + covW) w
                match Sanitize.array table.Length (int64 (o + 6 + covW + w)) count (uint32 w) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> sanitizePairSets table o (o + 6 + covW + w) w stride (int count) 0 { budget with Ops = ops }

    /// <summary>Sanitizes the class matrix of a pair-adjustment subtable of format 2 or 4.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The width of the coverage and class definition offsets.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeClassMatrix (table: ReadOnlySpan<byte>) o covW budget =
        let stride = valueSize (Binary.u16 table (o + 2 + covW)) + valueSize (Binary.u16 table (o + 4 + covW))
        let count = uint32 (Binary.u16 table (o + 6 + 3 * covW)) * uint32 (Binary.u16 table (o + 8 + 3 * covW))
        match Sanitize.array table.Length (int64 (o + 10 + 3 * covW)) count (uint32 stride) budget.Ops with
        | ValueNone -> ValueNone
        | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes the two class definitions of a pair-adjustment subtable of format 2 or 4, then its matrix.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The width of the coverage and class definition offsets.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeClassDefs (table: ReadOnlySpan<byte>) o covW budget =
        match sanitizeClassDefAt table o (readCoverageOffset table (o + 6 + covW) covW) budget with
        | ValueNone -> ValueNone
        | ValueSome budget ->
            match sanitizeClassDefAt table o (readCoverageOffset table (o + 6 + 2 * covW) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeClassMatrix table o covW budget

    /// <summary>Sanitizes a pair-adjustment subtable of format 2 or 4, <c>PairPosFormat2_4::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The width of the coverage and class definition offsets.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizePairPosClasses (table: ReadOnlySpan<byte>) o covW budget =
        match Sanitize.fits table.Length (int64 o) (10 + 3 * covW) with
        | false -> ValueNone
        | true ->
            match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeClassDefs table o covW budget

    /// <summary>Sanitizes a pair-adjustment subtable, <c>PairPos::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizePairPos (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            match int (Binary.u16 table o) with
            | 1 -> sanitizePairPosGlyphs table o Width16 Width16 budget
            | 3 -> sanitizePairPosGlyphs table o 4 Width24 budget
            | 2 -> sanitizePairPosClasses table o Width16 budget
            | 4 -> sanitizePairPosClasses table o 4 budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes one device an anchor names, <c>OffsetTo&lt;Device&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="anchor">The anchor's offset, which the field is relative to.</param>
    /// <param name="raw">The offset field.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAnchorDevice (table: ReadOnlySpan<byte>) anchor raw budget =
        match raw with
        | 0u -> ValueSome budget
        | _ ->
            match Sanitize.device table (anchor + int64 raw) budget.Ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes an anchor of format 3, <c>AnchorFormat3::sanitize</c>: the struct and then both devices.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The anchor's offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAnchorDevices (table: ReadOnlySpan<byte>) at budget =
        match fitsStruct table at 10 with
        | false -> ValueNone
        | true ->
            match sanitizeAnchorDevice table at (uint32 (Binary.u16 table (int at + 6))) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeAnchorDevice table at (uint32 (Binary.u16 table (int at + 8))) budget

    /// <summary>Sanitizes an anchor, <c>Anchor::sanitize</c>: a format that is none of the three passes.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The anchor's offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAnchor (table: ReadOnlySpan<byte>) at budget =
        match fitsStruct table at 2 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table (int at) with
            | 1us -> (match fitsStruct table at 6 with | true -> ValueSome budget | false -> ValueNone)
            | 2us -> (match fitsStruct table at 8 with | true -> ValueSome budget | false -> ValueNone)
            | 3us -> sanitizeAnchorDevices table at budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes an anchor reached through a relative offset, <c>OffsetTo&lt;Anchor&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The offset the field is relative to.</param>
    /// <param name="offsetValue">The offset field.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAnchorAt (table: ReadOnlySpan<byte>) baseOffset offsetValue budget =
        match resolveOffset baseOffset offsetValue with
        | ValueNone -> ValueSome budget
        | ValueSome at -> sanitizeAnchor table at budget

    /// <summary>Sanitizes the records of a mark array one after the other, <c>MarkRecord::sanitize</c>: the record must fit
    /// and its anchor, a null offset aside, must pass its check, which the accelerator's sanitizer does not leave lazy.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="markArray">The mark array's offset.</param>
    /// <param name="w">The width of the offsets.</param>
    /// <param name="count">The number of records.</param>
    /// <param name="index">The next record.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeMarkRecords (table: ReadOnlySpan<byte>) markArray w count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let record = markArray + w + index * (2 + w)
            match fitsStruct table (int64 record) (2 + w) with
            | false -> ValueNone
            | true ->
                match sanitizeAnchorAt table markArray (Coverage.readWide table (record + 2) w) budget with
                | ValueNone -> ValueNone
                | ValueSome budget -> sanitizeMarkRecords table markArray w count (index + 1) budget

    /// <summary>Sanitizes a mark array, <c>MarkArray::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="markArray">The mark array's offset.</param>
    /// <param name="w">The width of the count and of the offsets.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMarkArray (table: ReadOnlySpan<byte>) markArray w budget =
        match fitsStruct table (int64 markArray) w with
        | false -> ValueNone
        | true ->
            let count = Coverage.readWide table markArray w
            match Sanitize.array table.Length (int64 (markArray + w)) count (uint32 (2 + w)) budget.Ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> sanitizeMarkRecords table markArray w (int count) 0 { budget with Ops = ops }

    /// <summary>Sanitizes a mark array reached through a relative offset, <c>OffsetTo&lt;MarkArray&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetValue">The offset field.</param>
    /// <param name="w">The width of the array's count and offsets.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMarkArrayAt (table: ReadOnlySpan<byte>) o offsetValue w budget =
        match resolveOffset o offsetValue with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeMarkArray table (int at) w budget

    /// <summary>Sanitizes an anchor matrix, <c>AnchorMatrix::sanitize</c> with the anchors left to their lazy check as the
    /// accelerator's sanitizer leaves them: the row count, a product of rows and columns that fits 32 bits, and the array.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="matrix">The matrix's offset.</param>
    /// <param name="rowWidth">The width of the row count.</param>
    /// <param name="w">The width of the anchor offsets.</param>
    /// <param name="columns">The number of columns, the class count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAnchorMatrix (table: ReadOnlySpan<byte>) matrix rowWidth w columns budget =
        match fitsStruct table (int64 matrix) rowWidth with
        | false -> ValueNone
        | true ->
            let rows = Coverage.readWide table matrix rowWidth
            match uint64 rows * uint64 columns > uint64 UInt32.MaxValue with
            | true -> ValueNone
            | false ->
                match Sanitize.array table.Length (int64 (matrix + rowWidth)) (rows * columns) (uint32 w) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes an anchor matrix reached through a relative offset, <c>OffsetTo&lt;AnchorMatrix&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The offset the field is relative to.</param>
    /// <param name="offsetValue">The offset field.</param>
    /// <param name="rowWidth">The width of the row count.</param>
    /// <param name="w">The width of the anchor offsets.</param>
    /// <param name="columns">The number of columns, the class count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeAnchorMatrixAt (table: ReadOnlySpan<byte>) baseOffset offsetValue rowWidth w columns budget =
        match resolveOffset baseOffset offsetValue with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeAnchorMatrix table (int at) rowWidth w columns budget

    /// <summary>Sanitizes the ligature attach tables of a ligature array one after the other.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ligatures">The ligature array's offset.</param>
    /// <param name="w">The width of the offsets.</param>
    /// <param name="columns">The class count.</param>
    /// <param name="count">The number of ligatures.</param>
    /// <param name="index">The next ligature.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeLigatureAttaches (table: ReadOnlySpan<byte>) ligatures w columns count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offsetValue = Coverage.readWide table (ligatures + w + index * w) w
            match sanitizeAnchorMatrixAt table ligatures offsetValue Width16 w columns budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeLigatureAttaches table ligatures w columns count (index + 1) budget

    /// <summary>Sanitizes a ligature array, <c>LigatureArray::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ligatures">The ligature array's offset.</param>
    /// <param name="w">The width of the count and of the offsets.</param>
    /// <param name="columns">The class count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLigatureArray (table: ReadOnlySpan<byte>) ligatures w columns budget =
        match fitsStruct table (int64 ligatures) w with
        | false -> ValueNone
        | true ->
            let count = Coverage.readWide table ligatures w
            match Sanitize.array table.Length (int64 (ligatures + w)) count (uint32 w) budget.Ops with
            | ValueNone -> ValueNone
            | ValueSome ops -> sanitizeLigatureAttaches table ligatures w columns (int count) 0 { budget with Ops = ops }

    /// <summary>Sanitizes a ligature array reached through a relative offset, <c>OffsetTo&lt;LigatureArray&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="offsetValue">The offset field.</param>
    /// <param name="w">The width of the array's count and offsets.</param>
    /// <param name="columns">The class count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLigatureArrayAt (table: ReadOnlySpan<byte>) o offsetValue w columns budget =
        match resolveOffset o offsetValue with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeLigatureArray table (int at) w columns budget

    /// <summary>Sanitizes the two coverages, the mark array and the matrix of a mark-to-base or mark-to-mark subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The width of the coverage and array offsets.</param>
    /// <param name="w">The width of the data.</param>
    /// <param name="rowWidth">The width of the matrix's row count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMarkAttachmentBody (table: ReadOnlySpan<byte>) o covW w rowWidth budget =
        match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
        | ValueNone -> ValueNone
        | ValueSome budget ->
            match sanitizeCoverageAt table o (readCoverageOffset table (o + 2 + covW) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                match sanitizeMarkArrayAt table o (readCoverageOffset table (o + 4 + 2 * covW) covW) w budget with
                | ValueNone -> ValueNone
                | ValueSome budget ->
                    let columns = uint32 (Binary.u16 table (o + 2 + 2 * covW))
                    sanitizeAnchorMatrixAt table o (readCoverageOffset table (o + 4 + 3 * covW) covW) rowWidth w columns budget

    /// <summary>Sanitizes a mark-to-base or mark-to-mark subtable, <c>MarkBasePosFormat1_2::sanitize</c> and
    /// <c>MarkMarkPosFormat1_2::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="wideRows">Whether the matrix counts its rows in the width of the subtable's data, as the base matrix does.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMarkAttachment (table: ReadOnlySpan<byte>) o wideRows budget =
        match fitsStruct table (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let medium = format = 2
            let covW = match medium with | true -> 4 | false -> Width16
            let w = match medium with | true -> Width24 | false -> Width16
            let rowWidth = match wideRows with | true -> w | false -> Width16
            match format, fitsStruct table (int64 o) (4 + 4 * covW) with
            | (1 | 2), true -> sanitizeMarkAttachmentBody table o covW w rowWidth budget
            | (1 | 2), false -> ValueNone
            | _ -> ValueSome budget

    /// <summary>Sanitizes the two coverages, the mark array and the ligature array of a mark-to-ligature subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The width of the coverage and array offsets.</param>
    /// <param name="w">The width of the data.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMarkLigatureBody (table: ReadOnlySpan<byte>) o covW w budget =
        match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
        | ValueNone -> ValueNone
        | ValueSome budget ->
            match sanitizeCoverageAt table o (readCoverageOffset table (o + 2 + covW) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                match sanitizeMarkArrayAt table o (readCoverageOffset table (o + 4 + 2 * covW) covW) w budget with
                | ValueNone -> ValueNone
                | ValueSome budget ->
                    let columns = uint32 (Binary.u16 table (o + 2 + 2 * covW))
                    sanitizeLigatureArrayAt table o (readCoverageOffset table (o + 4 + 3 * covW) covW) w columns budget

    /// <summary>Sanitizes a mark-to-ligature subtable, <c>MarkLigPosFormat1_2::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeMarkLigature (table: ReadOnlySpan<byte>) o budget =
        match fitsStruct table (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let medium = format = 2
            let covW = match medium with | true -> 4 | false -> Width16
            let w = match medium with | true -> Width24 | false -> Width16
            match format, fitsStruct table (int64 o) (4 + 4 * covW) with
            | (1 | 2), true -> sanitizeMarkLigatureBody table o covW w budget
            | (1 | 2), false -> ValueNone
            | _ -> ValueSome budget

    /// <summary>Sanitizes the coverage and the record array of a cursive attachment subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="covW">The width of the coverage offset.</param>
    /// <param name="w">The width of the data.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeCursiveBody (table: ReadOnlySpan<byte>) o covW w budget =
        match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
        | ValueNone -> ValueNone
        | ValueSome budget ->
            match fitsStruct table (int64 (o + 2 + covW)) w with
            | false -> ValueNone
            | true ->
                let count = Coverage.readWide table (o + 2 + covW) w
                match Sanitize.array table.Length (int64 (o + 2 + covW + w)) count (uint32 (2 * w)) budget.Ops with
                | ValueNone -> ValueNone
                | ValueSome ops -> ValueSome { budget with Ops = ops }

    /// <summary>Sanitizes a cursive attachment subtable, <c>CursivePosFormat1_2::sanitize</c> with the anchors of the records
    /// left to their lazy check: the coverage, and the record array's count and range.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeCursive (table: ReadOnlySpan<byte>) o budget =
        match fitsStruct table (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let medium = format = 2
            let covW = match medium with | true -> 4 | false -> Width16
            let w = match medium with | true -> Width24 | false -> Width16
            match format, fitsStruct table (int64 o + 2L) covW with
            | (1 | 2), true -> sanitizeCursiveBody table o covW w budget
            | (1 | 2), false -> ValueNone
            | _ -> ValueSome budget

    /// <summary>Sanitizes one context rule, <c>Rule::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="rule">The rule offset.</param>
    /// <param name="vw">The input value width.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeRule (table: ReadOnlySpan<byte>) rule vw budget =
        match fitsStruct table (int64 rule) 4 with
        | false -> ValueNone
        | true ->
            let inputCount = int (Binary.u16 table rule)
            let lookupCount = int (Binary.u16 table (rule + 2))
            let inputSize = match inputCount with | 0 -> 0 | _ -> (inputCount - 1) * vw
            fitsRange table (int64 (rule + 4)) (uint32 (inputSize + lookupCount * 4)) budget

    /// <summary>Sanitizes a context rule reached through an offset, <c>OffsetTo&lt;Rule&gt;::sanitize</c>.</summary>
    let private sanitizeReachedRule (table: ReadOnlySpan<byte>) baseOffset _rw vw offset budget =
        match resolveOffset baseOffset offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeRule table (int at) vw budget

    /// <summary>Sanitizes each rule of a rule set.</summary>
    [<TailCall>]
    let rec private sanitizeRules (table: ReadOnlySpan<byte>) set rw vw count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            match sanitizeReachedRule table set rw vw (Coverage.readWide table (set + 2 + index * rw) rw) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeRules table set rw vw count (index + 1) budget

    /// <summary>Sanitizes a context rule set, <c>RuleSet::sanitize</c>.</summary>
    let private sanitizeRuleSet (table: ReadOnlySpan<byte>) set rw vw budget =
        match fitsStruct table (int64 set) 2 with
        | false -> ValueNone
        | true ->
            let count = int (Binary.u16 table set)
            match fitsRange table (int64 (set + 2)) (uint32 count * uint32 rw) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeRules table set rw vw count 0 budget

    /// <summary>Sanitizes a context rule set reached through an offset, <c>OffsetTo&lt;RuleSet&gt;::sanitize</c>.</summary>
    let private sanitizeReachedRuleSet (table: ReadOnlySpan<byte>) subtable rw vw offset budget =
        match resolveOffset subtable offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeRuleSet table (int at) rw vw budget

    /// <summary>Sanitizes each rule set of a context or chained subtable; the offsets are relative to the subtable.</summary>
    [<TailCall>]
    let rec private sanitizeRuleSets (table: ReadOnlySpan<byte>) subtable setsAt w rw vw count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = Coverage.readWide table (setsAt + index * w) w
            match sanitizeReachedRuleSet table subtable rw vw offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeRuleSets table subtable setsAt w rw vw count (index + 1) budget

    /// <summary>Sanitizes each coverage of a coverage-based context, <c>Coverage::sanitize</c> per offset, the offsets
    /// relative to the subtable.</summary>
    [<TailCall>]
    let rec private sanitizeCoverageArray (table: ReadOnlySpan<byte>) subtable readBase w count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            match sanitizeCoverageAt table subtable (Coverage.readWide table (readBase + index * w) w) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeCoverageArray table subtable readBase w count (index + 1) budget

    /// <summary>The size of one <c>LookupRecord</c>.</summary>
    [<Literal>]
    let private LookupRecordSize = 4

    /// <summary>Sanitizes a counted array whose items are not checked one by one: its count and its extent.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The array offset.</param>
    /// <param name="countWidth">The width of the count.</param>
    /// <param name="itemSize">The width of an item.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The count, the offset after the array and the remaining budgets, or none.</returns>
    let private fitsArray (table: ReadOnlySpan<byte>) at countWidth itemSize budget =
        match fitsStruct table (int64 at) countWidth with
        | false -> ValueNone
        | true ->
            let count = int (Coverage.readWide table at countWidth)
            match fitsRange table (int64 (at + countWidth)) (uint32 count * uint32 itemSize) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> ValueSome (struct (count, at + countWidth + count * itemSize, budget))

    /// <summary>Sanitizes a headless array, whose count includes the glyph that is not stored.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The array offset.</param>
    /// <param name="itemSize">The width of an item.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The count, the offset after the array and the remaining budgets, or none.</returns>
    let private fitsHeadless (table: ReadOnlySpan<byte>) at itemSize budget =
        match fitsStruct table (int64 at) Width16 with
        | false -> ValueNone
        | true ->
            let count = int (Binary.u16 table at)
            let size = match count with | 0 -> 0 | _ -> (count - 1) * itemSize
            match fitsRange table (int64 (at + Width16)) (uint32 size) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> ValueSome (struct (count, at + Width16 + size, budget))

    /// <summary>Sanitizes a 16-bit counted array of offsets to coverages, <c>Array16Of&lt;OffsetTo&lt;Coverage&gt;&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="subtable">The subtable the offsets are relative to.</param>
    /// <param name="at">The array offset.</param>
    /// <param name="itemSize">The width of an offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The count, the offset after the array and the remaining budgets, or none.</returns>
    let private sanitizeOffsetArray (table: ReadOnlySpan<byte>) subtable at itemSize budget =
        match fitsArray table at Width16 itemSize budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (count, next, budget)) ->
            match sanitizeCoverageArray table subtable (at + Width16) itemSize count 0 budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> ValueSome (struct (count, next, budget))

    /// <summary>Sanitizes the rule sets of a context subtable of formats 1, 2, 4 or 5 after its coverage and class
    /// definition: their count and each set.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="countAt">The offset of the rule-set count.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeContextRuleSets (table: ReadOnlySpan<byte>) o countAt format budget =
        let classRules = format = 2 || format = 5
        let w = dataWidth 5 format
        match fitsArray table countAt w w budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (count, _, budget)) ->
            sanitizeRuleSets table o (countAt + w) w (if classRules then w else Width16) (if classRules then Width16 else w) count 0 budget

    /// <summary>Sanitizes the class definition of a context subtable of format 2 or 5, and the rule sets after it.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeContextClasses (table: ReadOnlySpan<byte>) o format budget =
        let classRules = format = 2 || format = 5
        let covW = coverageWidth 5 format
        match classRules with
        | false -> sanitizeContextRuleSets table o (o + 2 + covW) format budget
        | true ->
            match sanitizeClassDefAt table o (readCoverageOffset table (o + 2 + covW) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeContextRuleSets table o (o + 2 + 2 * covW) format budget

    /// <summary>Sanitizes a context subtable of formats 1, 2, 4 or 5, <c>ContextFormat1_4::sanitize</c> and
    /// <c>ContextFormat2_5::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeContextSets (table: ReadOnlySpan<byte>) o format budget =
        let covW = coverageWidth 5 format
        match fitsStruct table (int64 o) (2 + covW) with
        | false -> ValueNone
        | true ->
            match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeContextClasses table o format budget

    /// <summary>Sanitizes the coverages of a context subtable of format 3 or 6 and its lookup records.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="w">The width of a coverage offset.</param>
    /// <param name="glyphCount">The number of coverages.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeContextCoverages (table: ReadOnlySpan<byte>) o w glyphCount budget =
        match fitsRange table (int64 (o + 6)) (uint32 glyphCount * uint32 w) budget with
        | ValueNone -> ValueNone
        | ValueSome budget ->
            match sanitizeCoverageArray table o (o + 6) w glyphCount 0 budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> fitsRange table (int64 (o + 6 + glyphCount * w)) (uint32 (int (Binary.u16 table (o + 4)) * LookupRecordSize)) budget

    /// <summary>Sanitizes a context subtable of format 3 or 6, <c>ContextFormat3_6::sanitize</c>: a glyph count of zero is
    /// refused so that the first coverage can be read freely.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeContextCoverageRule (table: ReadOnlySpan<byte>) o format budget =
        match fitsStruct table (int64 o) 6 with
        | false -> ValueNone
        | true ->
            match int (Binary.u16 table (o + 2)) with
            | 0 -> ValueNone
            | glyphCount -> sanitizeContextCoverages table o (dataWidth 5 format) glyphCount budget

    /// <summary>Sanitizes a context subtable, formats 1-6, <c>Context::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeContext (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            match format with
            | 1 | 2 | 4 | 5 -> sanitizeContextSets table o format budget
            | 3 | 6 -> sanitizeContextCoverageRule table o format budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes the lookup records of a chained rule or subtable: the array after the lookahead.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The offset of the lookup-record array.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLookupRecords (table: ReadOnlySpan<byte>) at budget =
        match fitsArray table at Width16 LookupRecordSize budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, _, budget)) -> ValueSome budget

    /// <summary>Sanitizes the lookahead of a chained rule, then its lookup records.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The lookahead offset.</param>
    /// <param name="vw">The width of a value.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainRuleLookahead (table: ReadOnlySpan<byte>) at vw budget =
        match fitsArray table at Width16 vw budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, lookupAt, budget)) -> sanitizeLookupRecords table lookupAt budget

    /// <summary>Sanitizes the input of a chained rule, then its lookahead and lookup records.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The input offset.</param>
    /// <param name="vw">The width of a value.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainRuleInput (table: ReadOnlySpan<byte>) at vw budget =
        match fitsHeadless table at vw budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, lookaheadAt, budget)) -> sanitizeChainRuleLookahead table lookaheadAt vw budget

    /// <summary>Sanitizes one chained rule, <c>ChainRule::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="rule">The rule offset.</param>
    /// <param name="vw">The width of a value.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainRule (table: ReadOnlySpan<byte>) rule vw budget =
        match fitsArray table rule Width16 vw budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, inputAt, budget)) -> sanitizeChainRuleInput table inputAt vw budget

    /// <summary>Sanitizes a chained rule reached through an offset.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="baseOffset">The rule set the offset is relative to.</param>
    /// <param name="vw">The width of a value.</param>
    /// <param name="offset">The offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReachedChainRule (table: ReadOnlySpan<byte>) baseOffset vw offset budget =
        match resolveOffset baseOffset offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeChainRule table (int at) vw budget

    /// <summary>Sanitizes each chained rule of a rule set.</summary>
    [<TailCall>]
    let rec private sanitizeChainRules (table: ReadOnlySpan<byte>) set rw vw count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            match sanitizeReachedChainRule table set vw (Coverage.readWide table (set + 2 + index * rw) rw) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeChainRules table set rw vw count (index + 1) budget

    /// <summary>Sanitizes a chained rule set, <c>ChainRuleSet::sanitize</c>.</summary>
    let private sanitizeChainRuleSet (table: ReadOnlySpan<byte>) set rw vw budget =
        match fitsArray table set Width16 rw budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (count, _, budget)) -> sanitizeChainRules table set rw vw count 0 budget

    /// <summary>Sanitizes a chained rule set reached through an offset.</summary>
    let private sanitizeReachedChainRuleSet (table: ReadOnlySpan<byte>) subtable rw vw offset budget =
        match resolveOffset subtable offset with
        | ValueNone -> ValueSome budget
        | ValueSome at ->
            match at <= int64 table.Length with
            | false -> ValueNone
            | true -> sanitizeChainRuleSet table (int at) rw vw budget

    /// <summary>Sanitizes each chained rule set; the offsets are relative to the subtable.</summary>
    [<TailCall>]
    let rec private sanitizeChainRuleSets (table: ReadOnlySpan<byte>) subtable setsAt w rw vw count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = Coverage.readWide table (setsAt + index * w) w
            match sanitizeReachedChainRuleSet table subtable rw vw offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeChainRuleSets table subtable setsAt w rw vw count (index + 1) budget

    /// <summary>Sanitizes each class definition of a chained-context class subtable.</summary>
    [<TailCall>]
    let rec private sanitizeChainClassDefs (table: ReadOnlySpan<byte>) o covW count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            match sanitizeClassDefAt table o (readCoverageOffset table (o + 2 + covW + index * covW) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeChainClassDefs table o covW count (index + 1) budget

    /// <summary>Sanitizes the rule sets of a chained-context subtable of formats 1, 2, 4 or 5, after its class definitions.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="countAt">The offset of the rule-set count.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainSetsArray (table: ReadOnlySpan<byte>) o countAt format budget =
        let w = dataWidth 6 format
        match fitsArray table countAt w w budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (count, _, budget)) ->
            sanitizeChainRuleSets table o (countAt + w) w w (if format = 2 || format = 5 then Width16 else w) count 0 budget

    /// <summary>Sanitizes the class definitions of a chained-context subtable of format 2 or 5, and the rule sets after them.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainClasses (table: ReadOnlySpan<byte>) o format budget =
        let covW = coverageWidth 6 format
        match format = 2 || format = 5 with
        | false -> sanitizeChainSetsArray table o (o + 2 + covW) format budget
        | true ->
            match sanitizeChainClassDefs table o covW 3 0 budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeChainSetsArray table o (o + 2 + 4 * covW) format budget

    /// <summary>Sanitizes a chained-context subtable of formats 1, 2, 4 or 5.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="format">The subtable format.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainSets (table: ReadOnlySpan<byte>) o format budget =
        let covW = coverageWidth 6 format
        match fitsStruct table (int64 o) (2 + covW) with
        | false -> ValueNone
        | true ->
            match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeChainClasses table o format budget

    /// <summary>Sanitizes the lookahead of a chained-context subtable of format 3, then its lookup records.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="at">The lookahead offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainCoverageLookahead (table: ReadOnlySpan<byte>) o at budget =
        match sanitizeOffsetArray table o at Width16 budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, lookupAt, budget)) -> sanitizeLookupRecords table lookupAt budget

    /// <summary>Sanitizes the input of a chained-context subtable of format 3, which must not be empty, then the rest.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="at">The input offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainCoverageInput (table: ReadOnlySpan<byte>) o at budget =
        match sanitizeOffsetArray table o at Width16 budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (0, _, _)) -> ValueNone
        | ValueSome (struct (_, lookaheadAt, budget)) -> sanitizeChainCoverageLookahead table o lookaheadAt budget

    /// <summary>Sanitizes a chained-context subtable of format 3: the backtrack, the input, the lookahead and the lookup
    /// records.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainCoverageRule (table: ReadOnlySpan<byte>) o budget =
        match sanitizeOffsetArray table o (o + 2) Width16 budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, inputAt, budget)) -> sanitizeChainCoverageInput table o inputAt budget

    /// <summary>Sanitizes a chained-context subtable, formats 1-5, <c>ChainContext::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeChainContext (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            match format with
            | 1 | 2 | 4 | 5 -> sanitizeChainSets table o format budget
            | 3 -> sanitizeChainCoverageRule table o budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes the substitutes of a reverse-chaining subtable after its lookahead.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="at">The offset of the substitute count.</param>
    /// <param name="w">The width of a glyph and of the count.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReverseSubstitutes (table: ReadOnlySpan<byte>) at w budget =
        match fitsArray table at w w budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, _, budget)) -> ValueSome budget

    /// <summary>Sanitizes the lookahead of a reverse-chaining subtable, then its substitutes.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="at">The lookahead offset.</param>
    /// <param name="w">The width of a coverage offset and of a glyph.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReverseLookahead (table: ReadOnlySpan<byte>) o at w budget =
        match sanitizeOffsetArray table o at w budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, substitutesAt, budget)) -> sanitizeReverseSubstitutes table substitutesAt w budget

    /// <summary>Sanitizes the backtrack of a reverse-chaining subtable, then the rest.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="at">The backtrack offset.</param>
    /// <param name="w">The width of a coverage offset and of a glyph.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReverseBacktrack (table: ReadOnlySpan<byte>) o at w budget =
        match sanitizeOffsetArray table o at w budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (_, lookaheadAt, budget)) -> sanitizeReverseLookahead table o lookaheadAt w budget

    /// <summary>Sanitizes a reverse-chaining single subtable, formats 1-2,
    /// <c>ReverseChainSingleSubstFormat1_2::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeReverse (table: ReadOnlySpan<byte>) o budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            let format = int (Binary.u16 table o)
            let covW = coverageWidth 8 format
            match format, fitsStruct table (int64 o) (2 + covW) with
            | (1 | 2), true ->
                match sanitizeCoverageAt table o (readCoverageOffset table (o + 2) covW) budget with
                | ValueNone -> ValueNone
                | ValueSome budget -> sanitizeReverseBacktrack table o (o + 2 + covW) (dataWidth 8 format) budget
            | (1 | 2), false -> ValueNone
            | _ -> ValueSome budget

    /// <summary>Whether a lookup type of an extension's inner subtable is one the lookup's table dispatches to a subtable
    /// type; HarfBuzz sanitizes the inner subtable only then, and takes any other type as sanitized
    /// (<c>SubstLookupSubTable::dispatch</c> and <c>PosLookupSubTable::dispatch</c> return the default value).</summary>
    /// <param name="innerType">The inner lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>True when the type has a subtable type.</returns>
    let private dispatchedType innerType isGsub =
        match isGsub with
        | true -> (innerType >= 1 && innerType <= 6) || innerType = ReverseChainSingle
        | false -> innerType >= 1 && innerType <= 8

    /// <summary>Whether a lookup's own type has a subtable type its table dispatches to: the types of <see cref="dispatchedType"/> and
    /// the extension type. The offset of a subtable of any other lookup type is never followed, so it may point anywhere
    /// (<c>PosLookupSubTable::dispatch</c> and <c>SubstLookupSubTable::dispatch</c> return the default value for it).</summary>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>True when the type is dispatched.</returns>
    let private dispatchedLookup lookupType isGsub =
        dispatchedType lookupType isGsub || lookupType = (match isGsub with | true -> GsubExtension | false -> GposExtension)

    /// <summary>Sanitizes an extension subtable's inner type, <c>ExtensionFormat1::dispatch</c>: the inner subtable lies at
    /// the 32-bit offset from the extension, and a zero offset is the Null object, which lies outside the blob, so the
    /// sanitizer refuses it (src/hb-open-type.hh:424-428).</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The extension offset.</param>
    /// <param name="innerType">The inner lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeExtended (table: ReadOnlySpan<byte>) o innerType isGsub budget =
        match dispatchedType innerType isGsub with
        | false -> ValueSome budget
        | true ->
            let at = int64 o + int64 (Binary.u32 table (o + 4))
            match Binary.u32 table (o + 4) = 0u || at > int64 table.Length with
            | true -> ValueNone
            | false ->
                let innerAt = int at
                match isGsub, innerType with
                | true, 1 -> sanitizeSingle table innerAt budget
                | true, 2 -> sanitizeMultiple table innerAt budget
                | true, 3 -> sanitizeAlternate table innerAt budget
                | true, 4 -> sanitizeLigature table innerAt budget
                | true, 5 -> sanitizeContext table innerAt budget
                | true, 6 -> sanitizeChainContext table innerAt budget
                | true, 8 -> sanitizeReverse table innerAt budget
                | false, 1 -> sanitizeSinglePos table innerAt budget
                | false, 2 -> sanitizePairPos table innerAt budget
                | false, 3 -> sanitizeCursive table innerAt budget
                | false, 4 -> sanitizeMarkAttachment table innerAt true budget
                | false, 5 -> sanitizeMarkLigature table innerAt budget
                | false, 6 -> sanitizeMarkAttachment table innerAt false budget
                | false, 7 -> sanitizeContext table innerAt budget
                | false, 8 -> sanitizeChainContext table innerAt budget
                | _ -> ValueSome budget

    /// <summary>Sanitizes an extension subtable's body, <c>ExtensionFormat1::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The extension offset.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeExtensionBody (table: ReadOnlySpan<byte>) o isGsub budget =
        match Sanitize.fits table.Length (int64 o) 8 with
        | false -> ValueNone
        | true ->
            let innerType = int (Binary.u16 table (o + 2))
            match innerType = (match isGsub with | true -> GsubExtension | false -> GposExtension) with
            | true -> ValueNone
            | false -> sanitizeExtended table o innerType isGsub budget

    /// <summary>Sanitizes an extension subtable, <c>Extension::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeExtension (table: ReadOnlySpan<byte>) o isGsub budget =
        match Sanitize.fits table.Length (int64 o) 2 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table o with
            | 1us -> sanitizeExtensionBody table o isGsub budget
            | _ -> ValueSome budget

    /// <summary>Sanitizes one subtable, dispatching on the lookup type; the context and chained-context types are the same code
    /// in GSUB (5, 6) and GPOS (7, 8).</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="o">The subtable offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeSubtable (table: ReadOnlySpan<byte>) o lookupType isGsub budget =
        match isGsub, lookupType with
        | true, 1 -> sanitizeSingle table o budget
        | true, 2 -> sanitizeMultiple table o budget
        | true, 3 -> sanitizeAlternate table o budget
        | true, 4 -> sanitizeLigature table o budget
        | true, 5 -> sanitizeContext table o budget
        | true, 6 -> sanitizeChainContext table o budget
        | true, 8 -> sanitizeReverse table o budget
        | true, 7 -> sanitizeExtension table o true budget
        | false, 1 -> sanitizeSinglePos table o budget
        | false, 2 -> sanitizePairPos table o budget
        | false, 3 -> sanitizeCursive table o budget
        | false, 4 -> sanitizeMarkAttachment table o true budget
        | false, 5 -> sanitizeMarkLigature table o budget
        | false, 6 -> sanitizeMarkAttachment table o false budget
        | false, 7 -> sanitizeContext table o budget
        | false, 8 -> sanitizeChainContext table o budget
        | false, 9 -> sanitizeExtension table o false budget
        | _ -> ValueSome budget

    /// <summary>Sanitizes one subtable of a lookup through its offset, <c>Offset16To&lt;TSubTable&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="offset">The relative subtable offset.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeOneSubtable (table: ReadOnlySpan<byte>) lk lookupType isGsub offset budget =
        match offset with
        | 0u -> ValueSome budget
        | _ ->
            let at = int64 lk + int64 offset
            match at <= int64 table.Length || not (dispatchedLookup lookupType isGsub) with
            | false -> ValueNone
            | true -> sanitizeSubtable table (int at) lookupType isGsub budget

    /// <summary>Sanitizes a lookup's subtables deeply, <c>get_subtables&lt;TSubTable&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeSubtables (table: ReadOnlySpan<byte>) lk lookupType isGsub count index budget =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let offset = uint32 (Binary.u16 table (lk + 6 + index * 2))
            match sanitizeOneSubtable table lk lookupType isGsub offset budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeSubtables table lk lookupType isGsub count (index + 1) budget

    /// <summary>Checks an Extension lookup's subtables, or passes a plain lookup.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLookupExtension (table: ReadOnlySpan<byte>) (lk: int) (count: int) (extensionType: int) (budget: Budget) : Budget voption =
        match int (Binary.u16 table lk) = extensionType with
        | true -> sanitizeExtensionSubtables table lk count 0 0 false extensionType budget
        | false -> ValueSome budget

    /// <summary>Sanitizes a lookup's subtable content, then its extension consistency.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLookupBody (table: ReadOnlySpan<byte>) (lk: int) (count: int) (extensionType: int) (lookupType: int) (isGsub: bool) (budget: Budget) =
        match sanitizeSubtables table lk lookupType isGsub count 0 budget with
        | ValueNone -> ValueNone
        | ValueSome budget -> sanitizeLookupExtension table lk count extensionType budget

    /// <summary>Sanitizes a lookup's mark filtering set, then its subtable content and extension consistency.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLookupTail (table: ReadOnlySpan<byte>) (lk: int) (count: int) (extensionType: int) (lookupType: int) (isGsub: bool) (budget: Budget) : Budget voption =
        match Binary.u16 table (lk + 2) &&& UseMarkFilteringSet with
        | 0us -> sanitizeLookupBody table lk count extensionType lookupType isGsub budget
        | _ ->
            match fitsStruct table (int64 (lk + 6 + count * 2)) 2 with
            | false -> ValueNone
            | true -> sanitizeLookupBody table lk count extensionType lookupType isGsub budget

    /// <summary>Sanitizes a lookup header, <c>Lookup::sanitize</c>, including its subtable content.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLookup (table: ReadOnlySpan<byte>) (lk: int) (extensionType: int) (budget: Budget) : Budget voption =
        match fitsStruct table (int64 lk) 6 with
        | false -> ValueNone
        | true ->
            let lookupType = int (Binary.u16 table lk)
            let isGsub = extensionType = GsubExtension
            let count = int (Binary.u16 table (lk + 4))
            match fitsRange table (int64 lk + 6L) (uint32 count * 2u) budget with
            | ValueNone -> ValueNone
            | ValueSome budget ->
                match chargeSubtables budget count with
                | ValueNone -> ValueNone
                | ValueSome budget -> sanitizeLookupTail table lk count extensionType lookupType isGsub budget

    /// <summary>Sanitizes each lookup offset of a lookup list, in order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ll">The lookup-list offset.</param>
    /// <param name="wide">Whether the offsets are 24-bit.</param>
    /// <param name="count">The number of offsets.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    [<TailCall>]
    let rec private sanitizeLookupOffsets
            (table: ReadOnlySpan<byte>) (ll: int) (wide: bool) (count: int) (index: int) (extensionType: int) (budget: Budget) : Budget voption =
        match index < count with
        | false -> ValueSome budget
        | true ->
            let width = if wide then Width24 else Width16
            let offset = Coverage.readWide table (ll + 2 + index * width) width
            match offset with
            | 0u -> sanitizeLookupOffsets table ll wide count (index + 1) extensionType budget
            | _ ->
                match sanitizeLookup table (ll + int offset) extensionType budget with
                | ValueNone -> ValueNone
                | ValueSome budget -> sanitizeLookupOffsets table ll wide count (index + 1) extensionType budget

    /// <summary>Sanitizes a lookup list, <c>List16OfOffsetTo&lt;Lookup&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ll">The lookup-list offset.</param>
    /// <param name="wide">Whether the offsets are 24-bit.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeLookupList (table: ReadOnlySpan<byte>) (ll: int) (wide: bool) (extensionType: int) (budget: Budget) : Budget voption =
        let width = if wide then Width24 else Width16
        match fitsStruct table (int64 ll) 2 with
        | false -> ValueNone
        | true ->
            match fitsRange table (int64 ll + 2L) (uint32 (Binary.u16 table ll) * uint32 width) budget with
            | ValueNone -> ValueNone
            | ValueSome budget -> sanitizeLookupOffsets table ll wide (int (Binary.u16 table ll)) 0 extensionType budget

    /// <summary>Sanitizes the feature variations field shallowly, deferred fully to phase 7.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The table version.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeFeatureVars (table: ReadOnlySpan<byte>) (version: uint32) (budget: Budget) : Budget voption =
        match version >= V1_1 && Binary.u32 table 10 <> 0u with
        | false -> ValueSome budget
        | true ->
            match fitsStruct table (int64 (Binary.u32 table 10)) 8 with
            | true -> ValueSome budget
            | false -> ValueNone

    /// <summary>The header size of a version, <c>GSUBGPOS::get_size</c>.</summary>
    /// <param name="version">The version.</param>
    /// <returns>The header size in bytes.</returns>
    let private headerSize (version: uint32) : int =
        match version >= V1_2, version >= V1_1 with
        | true, _ -> Header26
        | false, true -> Header14
        | false, false -> Header10

    /// <summary>Sanitizes the feature variations field, deferred fully to phase 7.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The table version.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeHeaderFeatureVars (table: ReadOnlySpan<byte>) (version: uint32) (budget: Budget) : Budget voption =
        match version >= V1_1 with
        | false -> ValueSome budget
        | true -> sanitizeFeatureVars table version budget

    /// <summary>Sanitizes the three list offsets, the 1.2 offsets overriding when non-zero.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="version">The table version.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeHeaderLists (table: ReadOnlySpan<byte>) (version: uint32) (extensionType: int) (budget: Budget) : Budget voption =
        let script16 =
            match uint32 (Binary.u16 table 4) with
            | 0u -> ValueSome budget
            | offset -> sanitizeScriptList table (int offset) budget
        let feature16 =
            match uint32 (Binary.u16 table 6) with
            | 0u -> ValueSome budget
            | offset -> sanitizeFeatureList table (int offset) budget
        let lookup16 =
            match uint32 (Binary.u16 table 8) with
            | 0u -> ValueSome budget
            | offset -> sanitizeLookupList table (int offset) false extensionType budget
        let scriptOk =
            match version >= V1_2 && Binary.u32 table 14 <> 0u with
            | true -> sanitizeScriptList table (int (Binary.u32 table 14)) budget
            | false -> script16
        let featureOk =
            match version >= V1_2 && Binary.u32 table 18 <> 0u with
            | true -> sanitizeFeatureList table (int (Binary.u32 table 18)) budget
            | false -> feature16
        let lookupOk =
            match version >= V1_2 && Binary.u32 table 22 <> 0u with
            | true -> sanitizeLookupList table (int (Binary.u32 table 22)) true extensionType budget
            | false -> lookup16
        match scriptOk, featureOk, lookupOk with
        | ValueSome _, ValueSome _, ValueSome lookupBudget -> sanitizeHeaderFeatureVars table version lookupBudget
        | _ -> ValueNone

    /// <summary>Sanitizes the version, the header range, then the three lists.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeHeaderBody (table: ReadOnlySpan<byte>) (extensionType: int) (budget: Budget) : Budget voption =
        let version = (uint32 (Binary.u16 table 0) <<< 16) ||| uint32 (Binary.u16 table 2)
        match fitsRange table 0L (uint32 (headerSize version)) budget with
        | ValueNone -> ValueNone
        | ValueSome budget -> sanitizeHeaderLists table version extensionType budget

    /// <summary>Sanitizes the GSUB/GPOS header and version, <c>GSUBGPOS::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="extensionType">The extension lookup type.</param>
    /// <param name="budget">The budgets.</param>
    /// <returns>The remaining budgets, or none.</returns>
    let private sanitizeHeader (table: ReadOnlySpan<byte>) (extensionType: int) (budget: Budget) : Budget voption =
        match fitsStruct table 0L 4 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table 0 with
            | 1us -> sanitizeHeaderBody table extensionType budget
            | _ -> ValueNone

    /// <summary>Sanitizes a GSUB/GPOS table blob, <c>sanitize_blob</c>: a failure yields the empty table.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="tag">The table tag, GSUB or GPOS.</param>
    /// <param name="ops">The operation budget.</param>
    /// <returns>True when the table sanitizes.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (tag: Tag) (ops: int) : bool =
        let extensionType = match tag = Tags.Gsub with | true -> GsubExtension | false -> GposExtension
        match sanitizeHeader table extensionType { Ops = ops; Subtables = 0 } with
        | ValueSome _ -> true
        | ValueNone -> false

    /// <summary>Unfolds a big-endian uint16 array.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Uint16Decoder =
        { Base: int }
        interface ISpanUnfolder<byte, int, uint16> with
            member this.Next (table, index) =
                struct (Binary.u16 table (this.Base + index * 2), index + 1)

    /// <summary>Unfolds the tags of a record array.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private TagDecoder =
        { Base: int
          Stride: int }
        interface ISpanUnfolder<byte, int, Tag> with
            member this.Next (table, index) =
                struct (Binary.tag table (this.Base + index * this.Stride), index + 1)

    /// <summary>Parses a language system, <c>LangSys</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="ls">The language-system offset.</param>
    /// <returns>The language system.</returns>
    let private parseLangSys (table: ReadOnlySpan<byte>) (ls: int) : LangSys =
        { RequiredFeature = uint32 (Binary.u16 table (ls + 2))
          FeatureIndices = Sealed.unfoldSpan table (int (Binary.u16 table (ls + 4))) 0 { Uint16Decoder.Base = ls + 6 } }

    /// <summary>Unfolds a script's language systems.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LangSysDecoder =
        { ScriptBase: int }
        interface ISpanUnfolder<byte, int, LangSys> with
            member this.Next (table, index) =
                let record = this.ScriptBase + 4 + index * 6
                let langSys =
                    match Binary.u16 table (record + 4) with
                    | 0us -> nullLangSys
                    | offset -> parseLangSys table (this.ScriptBase + int offset)
                struct (langSys, index + 1)

    /// <summary>Parses one script's language systems, <c>Script</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="s">The script offset.</param>
    /// <returns>The default language system, the tags and the language systems.</returns>
    let private parseScriptLangSys (table: ReadOnlySpan<byte>) (s: int) : struct (LangSys voption * ImmutableArray<Tag> * ImmutableArray<LangSys>) =
        let defaultLangSys =
            match Binary.u16 table s with
            | 0us -> ValueNone
            | offset -> ValueSome (parseLangSys table (s + int offset))
        let count = int (Binary.u16 table (s + 2))
        let tags = Sealed.unfoldSpan table count 0 { TagDecoder.Base = s + 4; Stride = 6 }
        let langSys = Sealed.unfoldSpan table count 0 { LangSysDecoder.ScriptBase = s }
        struct (defaultLangSys, tags, langSys)

    /// <summary>Unfolds a script list's records.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ScriptDecoder =
        { ListBase: int }
        interface ISpanUnfolder<byte, int, ScriptRecord> with
            member this.Next (table, index) =
                let record = this.ListBase + 2 + index * 6
                let offset = uint32 (Binary.u16 table (record + 4))
                let struct (defaultLangSys, tags, langSys) =
                    match offset with
                    | 0u -> struct (ValueNone, ImmutableArray.Empty, ImmutableArray.Empty)
                    | _ -> parseScriptLangSys table (this.ListBase + int offset)
                struct ({ ScriptTag = Binary.tag table record; DefaultLangSys = defaultLangSys; LangSysTags = tags; LangSys = langSys }, index + 1)

    /// <summary>Parses a script list, <c>ScriptList</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="sl">The script-list offset.</param>
    /// <returns>The scripts.</returns>
    let private parseScriptList (table: ReadOnlySpan<byte>) (sl: int) : ImmutableArray<ScriptRecord> =
        Sealed.unfoldSpan table (int (Binary.u16 table sl)) 0 { ScriptDecoder.ListBase = sl }

    /// <summary>Parses a feature's lookup indices, <c>Feature</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="f">The feature offset.</param>
    /// <returns>The lookup indices.</returns>
    let private parseFeatureLookups (table: ReadOnlySpan<byte>) (f: int) : ImmutableArray<uint16> =
        Sealed.unfoldSpan table (int (Binary.u16 table (f + 2))) 0 { Uint16Decoder.Base = f + 4 }

    /// <summary>Unfolds a feature list's records.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FeatureDecoder =
        { ListBase: int }
        interface ISpanUnfolder<byte, int, FeatureRecord> with
            member this.Next (table, index) =
                let record = this.ListBase + 2 + index * 6
                let offset = uint32 (Binary.u16 table (record + 4))
                let lookups =
                    match offset with
                    | 0u -> ImmutableArray.Empty
                    | _ -> parseFeatureLookups table (this.ListBase + int offset)
                struct ({ FeatureTag = Binary.tag table record; LookupIndices = lookups }, index + 1)

    /// <summary>Parses a feature list, <c>FeatureList</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="fl">The feature-list offset.</param>
    /// <returns>The features.</returns>
    let private parseFeatureList (table: ReadOnlySpan<byte>) (fl: int) : ImmutableArray<FeatureRecord> =
        Sealed.unfoldSpan table (int (Binary.u16 table fl)) 0 { FeatureDecoder.ListBase = fl }

    /// <summary>The extended type of an extension subtable, or zero for a Null or non-format-1 subtable.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="subtableOffset">The subtable offset, relative to the lookup.</param>
    /// <returns>The extended type.</returns>
    let private extensionTypeValue (table: ReadOnlySpan<byte>) (lk: int) (subtableOffset: int) : int =
        match subtableOffset with
        | 0 -> 0
        | offset ->
            let at = lk + offset
            match Binary.u16 table at with
            | 1us -> int (Binary.u16 table (at + 2))
            | _ -> 0

    /// <summary>Whether a lookup applies in reverse, <c>SubstLookup::is_reverse</c> and <c>PosLookup::is_reverse</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="lookupType">The lookup type.</param>
    /// <param name="count">The subtable count.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <returns>True for a reverse lookup.</returns>
    let private isReverseLookup (table: ReadOnlySpan<byte>) (lk: int) (lookupType: int) (count: int) (isGsub: bool) : bool =
        match isGsub with
        | false -> false
        | true ->
            match lookupType with
            | ReverseChainSingle -> true
            | GsubExtension ->
                match count with
                | 0 -> false
                | _ -> extensionTypeValue table lk (int (Binary.u16 table (lk + 6))) = ReverseChainSingle
            | _ -> false

    /// <summary>Unfolds a lookup's subtables.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private SubtableDecoder<'S> =
        { LookupBase: int
          LookupType: int
          IsGsub: bool
          Parser: SubtableParser<'S> }
        interface ISpanUnfolder<byte, int, 'S> with
            member this.Next (table, index) =
                let offset = int (Binary.u16 table (this.LookupBase + 6 + index * 2))
                match offset = 0 && not this.IsGsub with
                | true -> struct (this.Parser.Invoke(table, -1, this.LookupType), index + 1)
                | false -> struct (this.Parser.Invoke(table, this.LookupBase + offset, this.LookupType), index + 1)

    /// <summary>Parses a lookup, <c>Lookup</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="parseSubtable">The subtable parser.</param>
    /// <returns>The parsed lookup.</returns>
    let private parseLookup (table: ReadOnlySpan<byte>) (lk: int) (isGsub: bool) (parseSubtable: SubtableParser<'S>) : ParsedLookup<'S> =
        let lookupType = int (Binary.u16 table lk)
        let flag = Binary.u16 table (lk + 2)
        let count = int (Binary.u16 table (lk + 4))
        let props =
            match flag &&& UseMarkFilteringSet with
            | 0us -> uint32 flag
            | _ -> uint32 flag + (uint32 (Binary.u16 table (lk + 6 + count * 2)) <<< 16)
        let subtables =
            Sealed.unfoldSpan table count 0 { SubtableDecoder.LookupBase = lk; LookupType = lookupType; IsGsub = isGsub; Parser = parseSubtable }
        { LookupType = lookupType
          Props = props
          IsReverse = isReverseLookup table lk lookupType count isGsub
          Subtables = subtables }

    /// <summary>Builds a lookup's accelerator on first use, <c>get_accel</c>: the digests are computed when the lookup is first asked for.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="lk">The lookup offset.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="parseSubtable">The subtable parser.</param>
    /// <param name="subtableDigest">The per-subtable coverage digest.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <returns>The lazily built accelerator.</returns>
    let private lazyLookupAccel
            (table: ReadOnlyMemory<byte>)
            (lk: int)
            (isGsub: bool)
            (parseSubtable: SubtableParser<'S>)
            (subtableDigest: 'S -> Digest)
            (secondDigest: 'S -> Digest)
            : Lazy<LookupAccel<'S>> =
        lazy (LayoutAccel.build subtableDigest secondDigest (parseLookup table.Span lk isGsub parseSubtable))

    /// <summary>The Null lookup accelerator: a lookup index whose offset is zero, <c>Null (Lookup)</c>.</summary>
    /// <param name="subtableDigest">The per-subtable coverage digest.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <returns>The lazily built empty accelerator.</returns>
    let private lazyNullLookupAccel (subtableDigest: 'S -> Digest) (secondDigest: 'S -> Digest) : Lazy<LookupAccel<'S>> =
        lazy (LayoutAccel.build subtableDigest secondDigest { LookupType = 0; Props = 0u; IsReverse = false; Subtables = ImmutableArray.Empty })

    /// <summary>Unfolds a lookup list's lookups into lazy accelerators.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LookupDecoder<'S> =
        { Table: ReadOnlyMemory<byte>
          ListBase: int
          Width: int
          IsGsub: bool
          Parser: SubtableParser<'S>
          SubtableDigest: 'S -> Digest
          SecondDigest: 'S -> Digest }
        interface ISpanUnfolder<byte, int, Lazy<LookupAccel<'S>>> with
            member this.Next (table, index) =
                let offset = Coverage.readWide table (this.ListBase + 2 + index * this.Width) this.Width
                let lookup =
                    match offset with
                    | 0u -> lazyNullLookupAccel this.SubtableDigest this.SecondDigest
                    | _ -> lazyLookupAccel this.Table (this.ListBase + int offset) this.IsGsub this.Parser this.SubtableDigest this.SecondDigest
                struct (lookup, index + 1)

    /// <summary>Parses a lookup list into lazily built lookup accelerators, <c>LookupList</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ll">The lookup-list offset.</param>
    /// <param name="wide">Whether the offsets are 24-bit.</param>
    /// <param name="isGsub">Whether the table is GSUB.</param>
    /// <param name="parseSubtable">The subtable parser.</param>
    /// <param name="subtableDigest">The per-subtable coverage digest.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <returns>The lazily built lookup accelerators.</returns>
    let private parseLookupList
            (table: ReadOnlyMemory<byte>)
            (ll: int)
            (wide: bool)
            (isGsub: bool)
            (parseSubtable: SubtableParser<'S>)
            (subtableDigest: 'S -> Digest)
            (secondDigest: 'S -> Digest)
            : ImmutableArray<Lazy<LookupAccel<'S>>> =
        let span = table.Span
        let width = if wide then Width24 else Width16
        Sealed.unfoldSpan span (int (Binary.u16 span ll)) 0
            { LookupDecoder.Table = table; ListBase = ll; Width = width; IsGsub = isGsub; Parser = parseSubtable; SubtableDigest = subtableDigest; SecondDigest = secondDigest }

    /// <summary>The winning list offset: the 32-bit field when the version is 1.2 and it is non-zero.</summary>
    /// <param name="is12">Whether the version is at least 1.2.</param>
    /// <param name="off16">The 16-bit offset.</param>
    /// <param name="off32">The 32-bit offset.</param>
    /// <returns>The winning offset.</returns>
    let private winningOffset (is12: bool) (off16: uint32) (off32: uint32) : uint32 =
        match is12 && off32 <> 0u with
        | true -> off32
        | false -> off16

    /// <summary>Parses a sanitized GSUB/GPOS table.</summary>
    /// <param name="table">The sanitized table bytes.</param>
    /// <param name="tag">The table tag, GSUB or GPOS.</param>
    /// <param name="parseSubtable">The subtable parser.</param>
    /// <param name="subtableDigest">The per-subtable coverage digest.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <returns>The table.</returns>
    let private parse
            (table: ReadOnlyMemory<byte>)
            (tag: Tag)
            (parseSubtable: SubtableParser<'S>)
            (subtableDigest: 'S -> Digest)
            (secondDigest: 'S -> Digest)
            : LayoutTable<'S> =
        let span = table.Span
        let version = (uint32 (Binary.u16 span 0) <<< 16) ||| uint32 (Binary.u16 span 2)
        let is12 = version >= V1_2
        let isGsub = tag = Tags.Gsub
        let scriptListOffset = winningOffset is12 (uint32 (Binary.u16 span 4)) (Binary.u32 span 14)
        let featureListOffset = winningOffset is12 (uint32 (Binary.u16 span 6)) (Binary.u32 span 18)
        let lookupListOffset = winningOffset is12 (uint32 (Binary.u16 span 8)) (Binary.u32 span 22)
        let lookupListWide = is12 && Binary.u32 span 22 <> 0u
        let featureVariations =
            match version >= V1_1 && Binary.u32 span 10 <> 0u with
            | false -> ReadOnlyMemory<byte>.Empty
            | true ->
                let offset = int (Binary.u32 span 10)
                match offset < table.Length with
                | true -> table.Slice offset
                | false -> ReadOnlyMemory<byte>.Empty
        { Version = version
          Scripts =
            match scriptListOffset with
            | 0u -> ImmutableArray.Empty
            | offset -> parseScriptList span (int offset)
          Features =
            match featureListOffset with
            | 0u -> ImmutableArray.Empty
            | offset -> parseFeatureList span (int offset)
          Lookups =
            match lookupListOffset with
            | 0u -> ImmutableArray.Empty
            | offset -> parseLookupList table (int offset) lookupListWide isGsub parseSubtable subtableDigest secondDigest
          FeatureVariations = featureVariations }

    /// <summary>Sanitizes and parses the bytes of a GSUB or GPOS table, the part of loading that does not need a face, for a table
    /// built in memory such as the fallback lookups of a shaper.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="tag">The table tag, GSUB or GPOS.</param>
    /// <param name="parseSubtable">The subtable parser.</param>
    /// <param name="subtableDigest">The per-subtable coverage digest.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <returns>The table, empty when empty or invalid.</returns>
    let ofTable (table: ReadOnlyMemory<byte>) (tag: Tag) (parseSubtable: SubtableParser<'S>) (subtableDigest: 'S -> Digest) (secondDigest: 'S -> Digest) =
        match table.IsEmpty with
        | true -> empty
        | false ->
            match sanitize table.Span tag (Sanitize.start table.Length) with
            | false -> empty
            | true -> parse table tag parseSubtable subtableDigest secondDigest

    /// <summary>Loads and sanitizes a face's GSUB or GPOS table.</summary>
    /// <param name="face">The face.</param>
    /// <param name="tag">The table tag, GSUB or GPOS.</param>
    /// <param name="parseSubtable">The subtable parser.</param>
    /// <param name="subtableDigest">The per-subtable coverage digest.</param>
    /// <param name="secondDigest">The per-subtable second-glyph digest.</param>
    /// <returns>The table, empty when absent or invalid.</returns>
    let load (face: Face) (tag: Tag) (parseSubtable: SubtableParser<'S>) (subtableDigest: 'S -> Digest) (secondDigest: 'S -> Digest) : LayoutTable<'S> =
        ofTable (Face.referenceTable face tag) tag parseSubtable subtableDigest secondDigest

    /// <summary>The length of a GSUB or GPOS table after sanitising, <c>table.get_length</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="tag">The table tag, GSUB or GPOS.</param>
    /// <returns>The sanitised length, zero when absent or invalid.</returns>
    let sanitizedLength (face: Face) (tag: Tag) =
        let table = Face.referenceTable face tag
        match table.IsEmpty with
        | true -> 0
        | false ->
            match sanitize table.Span tag (Sanitize.start table.Length) with
            | false -> 0
            | true -> table.Length

    /// <summary>Finds a script's index, <c>find_script_index</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="script">The script tag.</param>
    /// <returns>The index, or none.</returns>
    let findScriptIndex (table: LayoutTable<'S>) (script: Tag) : int voption =
        let scripts = table.Scripts.AsSpan()
        bfindScript scripts script 0 (scripts.Length - 1)

    /// <summary>Finds the first candidate script present, <c>hb_ot_layout_table_select_script</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="candidates">The candidate script tags.</param>
    /// <param name="index">The next candidate.</param>
    /// <returns>The selection, or none when no candidate matches.</returns>
    [<TailCall>]
    let rec private selectScriptAt (table: LayoutTable<'S>) (candidates: ReadOnlySpan<Tag>) (index: int) : struct (bool * int * Tag) voption =
        match index < candidates.Length with
        | false -> ValueNone
        | true ->
            match findScriptIndex table candidates[index] with
            | ValueSome scriptIndex -> ValueSome (struct (true, scriptIndex, candidates[index]))
            | ValueNone -> selectScriptAt table candidates (index + 1)

    /// <summary>Tries the <c>latn</c> script fallback, else selects none.</summary>
    /// <param name="table">The table.</param>
    /// <returns>The fallback selection.</returns>
    let private selectScriptLatn (table: LayoutTable<'S>) : struct (bool * int * Tag) =
        match findScriptIndex table Tags.Latn with
        | ValueSome scriptIndex -> struct (false, scriptIndex, Tags.Latn)
        | ValueNone -> struct (false, NoScriptIndex, Tags.Empty)

    /// <summary>Tries the <c>dflt</c> script fallback.</summary>
    /// <param name="table">The table.</param>
    /// <returns>The fallback selection.</returns>
    let private selectScriptLowerDflt (table: LayoutTable<'S>) : struct (bool * int * Tag) =
        match findScriptIndex table Tags.dflt with
        | ValueSome scriptIndex -> struct (false, scriptIndex, Tags.dflt)
        | ValueNone -> selectScriptLatn table

    /// <summary>Tries the <c>DFLT</c> script fallback.</summary>
    /// <param name="table">The table.</param>
    /// <returns>The fallback selection.</returns>
    let private selectScriptDflt (table: LayoutTable<'S>) : struct (bool * int * Tag) =
        match findScriptIndex table Tags.Dflt with
        | ValueSome scriptIndex -> struct (false, scriptIndex, Tags.Dflt)
        | ValueNone -> selectScriptLowerDflt table

    /// <summary>Selects a script from the candidates, then the fallbacks, <c>hb_ot_layout_table_select_script</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="candidates">The candidate script tags.</param>
    /// <returns>Whether a candidate matched, the script index and the chosen tag.</returns>
    let selectScript (table: LayoutTable<'S>) (candidates: ImmutableArray<Tag>) : struct (bool * int * Tag) =
        match selectScriptAt table (candidates.AsSpan()) 0 with
        | ValueSome result -> result
        | ValueNone -> selectScriptDflt table

    /// <summary>Finds a language system's index under a script, <c>find_lang_sys_index</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="tag">The language tag.</param>
    /// <returns>The index, or none.</returns>
    let private findLangSysIndex (table: LayoutTable<'S>) (scriptIndex: int) (tag: Tag) : int voption =
        match scriptIndex >= 0 && scriptIndex < table.Scripts.Length with
        | false -> ValueNone
        | true ->
            let tags = table.Scripts[scriptIndex].LangSysTags.AsSpan()
            bfindLangSys tags tag 0 (tags.Length - 1)

    /// <summary>Finds the first candidate language present, <c>hb_ot_layout_script_select_language2</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="candidates">The candidate language tags.</param>
    /// <param name="index">The next candidate.</param>
    /// <returns>The selection, or none when no candidate matches.</returns>
    [<TailCall>]
    let rec private selectLanguageAt (table: LayoutTable<'S>) (scriptIndex: int) (candidates: ReadOnlySpan<Tag>) (index: int) : struct (bool * int * Tag) voption =
        match index < candidates.Length with
        | false -> ValueNone
        | true ->
            match findLangSysIndex table scriptIndex candidates[index] with
            | ValueSome languageIndex -> ValueSome (struct (true, languageIndex, candidates[index]))
            | ValueNone -> selectLanguageAt table scriptIndex candidates (index + 1)

    /// <summary>Selects a language from the candidates, then the fallback, <c>hb_ot_layout_script_select_language2</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="candidates">The candidate language tags.</param>
    /// <returns>Whether a candidate matched, the language index and the chosen tag.</returns>
    let selectLanguage (table: LayoutTable<'S>) (scriptIndex: int) (candidates: ImmutableArray<Tag>) : struct (bool * int * Tag) =
        match selectLanguageAt table scriptIndex (candidates.AsSpan()) 0 with
        | ValueSome result -> result
        | ValueNone ->
            match findLangSysIndex table scriptIndex Tags.dflt with
            | ValueSome languageIndex -> struct (false, languageIndex, Tags.dflt)
            | ValueNone -> struct (false, DefaultLanguageIndex, Tags.Empty)

    /// <summary>Returns a script and language's language system, <c>Script::get_lang_sys</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="languageIndex">The language index, or the default-language index.</param>
    /// <returns>The language system, or the Null language system.</returns>
    let langSys (table: LayoutTable<'S>) (scriptIndex: int) (languageIndex: int) : LangSys =
        match scriptIndex >= 0 && scriptIndex < table.Scripts.Length with
        | false -> nullLangSys
        | true ->
            let script = table.Scripts[scriptIndex]
            match languageIndex = DefaultLanguageIndex with
            | true ->
                match script.DefaultLangSys with
                | ValueSome langSys -> langSys
                | ValueNone -> nullLangSys
            | false ->
                match languageIndex >= 0 && languageIndex < script.LangSys.Length with
                | true -> script.LangSys[languageIndex]
                | false -> nullLangSys

    /// <summary>Finds the first feature of a tag, <c>hb_ot_layout_table_find_feature</c>.</summary>
    /// <param name="features">The features in file order.</param>
    /// <param name="tag">The key.</param>
    /// <param name="index">The next feature.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private findFeatureAt (features: ReadOnlySpan<FeatureRecord>) (tag: Tag) (index: int) : int voption =
        match index < features.Length with
        | false -> ValueNone
        | true ->
            match features[index].FeatureTag = tag with
            | true -> ValueSome index
            | false -> findFeatureAt features tag (index + 1)

    /// <summary>Finds a feature's index, <c>hb_ot_layout_table_find_feature</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The index, or none.</returns>
    let findFeature (table: LayoutTable<'S>) (tag: Tag) : int voption =
        findFeatureAt (table.Features.AsSpan()) tag 0

    /// <summary>The tag of a feature index, <c>get_feature_tag</c>; the empty tag past the end.</summary>
    /// <param name="table">The table.</param>
    /// <param name="index">The feature index.</param>
    /// <returns>The feature tag.</returns>
    let featureTag (table: LayoutTable<'S>) (index: int) : Tag =
        match index >= 0 && index < table.Features.Length with
        | true -> table.Features[index].FeatureTag
        | false -> Tags.Empty

    /// <summary>Builds the feature tag to first index map, <c>hb_ot_layout_collect_features_map</c>.</summary>
    /// <param name="table">The table.</param>
    /// <param name="langSys">The language system.</param>
    /// <returns>The sorted tag and index pairs, the first index winning.</returns>
    let featureIndexMap (table: LayoutTable<'S>) (langSys: LangSys) : ImmutableArray<struct (Tag * int)> =
        let pairs =
            Sealed.init langSys.FeatureIndices.Length (fun i ->
                let position = langSys.FeatureIndices.Length - 1 - i
                let featureIndex = int langSys.FeatureIndices[position]
                struct (uint32 (featureTag table featureIndex), uint32 featureIndex))
        Sealed.sortedMap (pairs.AsSpan())
        |> fun sorted -> Sealed.map (fun (struct (tag, index)) -> struct (LanguagePrimitives.UInt32WithMeasure tag, int index)) (sorted.AsSpan())

    /// <summary>The script tags of a table in file order, <c>get_script_tags</c>.</summary>
    /// <param name="table">The table.</param>
    /// <returns>The tags.</returns>
    let scriptTags (table: LayoutTable<'S>) : ImmutableArray<Tag> =
        Sealed.map (fun script -> script.ScriptTag) (table.Scripts.AsSpan())

    /// <summary>The feature tags of a table in file order, <c>get_feature_tags</c>.</summary>
    /// <param name="table">The table.</param>
    /// <returns>The tags.</returns>
    let featureTags (table: LayoutTable<'S>) : ImmutableArray<Tag> =
        Sealed.map (fun feature -> feature.FeatureTag) (table.Features.AsSpan())

    /// <summary>The number of lookups, <c>get_lookup_count</c>.</summary>
    /// <param name="table">The table.</param>
    /// <returns>The count.</returns>
    let lookupCount (table: LayoutTable<'S>) : int =
        table.Lookups.Length
