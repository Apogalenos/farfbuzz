namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>One feature-name record of the feat table.</summary>
[<Struct; NoComparison; NoEquality>]
type internal FeatureName =
    { Feature: uint16
      NSettings: uint16
      SettingTableOffset: uint32
      FeatureFlags: uint16
      NameIndex: int16 }

/// <summary>Reading one feature-name record, <c>FeatureName</c> (src/hb-aat-layout-feat-table.hh).</summary>
[<RequireQualifiedAccess>]
module internal FeatureName =
    /// <summary>Whether the record names a feature with at least one setting, <c>has_data</c>.</summary>
    /// <param name="name">The record.</param>
    /// <returns>True when <c>NSettings</c> is not zero.</returns>
    let hasData (name: FeatureName) = name.NSettings <> 0us

    /// <summary>Whether the feature's settings are mutually exclusive, <c>is_exclusive</c>.</summary>
    /// <param name="name">The record.</param>
    /// <returns>True when the exclusive bit is set.</returns>
    let isExclusive (name: FeatureName) = name.FeatureFlags &&& 0x8000us <> 0us

/// <summary>The sanitized feat table, holding the feature-name array in source order.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Feat =
    { Version: uint32
      Names: ImmutableArray<FeatureName>
      Table: ReadOnlyMemory<byte> }

/// <summary>One selector of a feature type, <c>hb_aat_layout_feature_selector_info_t</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal FeatureSelectorInfo =
    { NameId: uint32
      Enable: uint32
      Disable: uint32
      Reserved: uint32 }

/// <summary>Reading the feat feature-name table, <c>AAT::feat</c> (src/hb-aat-layout-feat-table.hh).</summary>
[<RequireQualifiedAccess>]
module internal Feat =
    /// <summary>The size of the feat header.</summary>
    [<Literal>]
    let private HeaderSize = 12

    /// <summary>The size of one FeatureName record.</summary>
    [<Literal>]
    let private FeatureNameSize = 12

    /// <summary>The size of one SettingName record.</summary>
    [<Literal>]
    let private SettingNameSize = 4

    /// <summary>The field offsets of a FeatureName record.</summary>
    [<Literal>]
    let private FeatureOffset = 0

    /// <summary>The field offset of the setting count.</summary>
    [<Literal>]
    let private NSettingsOffset = 2

    /// <summary>The field offset of the setting-table offset.</summary>
    [<Literal>]
    let private SettingTableOffset = 4

    /// <summary>The field offset of the feature flags.</summary>
    [<Literal>]
    let private FeatureFlagsOffset = 8

    /// <summary>The field offset of the name index.</summary>
    [<Literal>]
    let private NameIndexOffset = 10

    /// <summary>The empty feat table, returned when the table is absent or fails to sanitize.</summary>
    let private empty : Feat = { Version = 0u; Names = ImmutableArray.Empty; Table = ReadOnlyMemory.Empty }

    /// <summary>Sanitizes the setting tables of every feature name, <c>FeatureName::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="index">The next feature name.</param>
    /// <param name="count">The feature-name count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when a setting table fails.</returns>
    [<TailCall>]
    let rec private sanitizeSettingTables (table: ReadOnlySpan<byte>) (index: int) (count: int) (ops: int) : int voption =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let at = HeaderSize + index * FeatureNameSize
            let nSettings = Binary.u16 table (at + NSettingsOffset)
            let settingTableAt = int64 (Binary.u32 table (at + SettingTableOffset))
            match Sanitize.array table.Length settingTableAt (uint32 nSettings) (uint32 SettingNameSize) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeSettingTables table (index + 1) count remaining

    /// <summary>Sanitizes a feat table, <c>feat::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        match Sanitize.fits table.Length 0 HeaderSize with
        | false -> ValueNone
        | true ->
            match (Binary.u32 table 0) >>> 16 with
            | 1u ->
                let count = int (Binary.u16 table 4)
                match Sanitize.array table.Length HeaderSize (uint32 count) (uint32 FeatureNameSize) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeSettingTables table 0 count remaining
            | _ -> ValueNone

    /// <summary>The struct form of the feat sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Loads and sanitizes the feat table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The feature names, empty when the table is absent or fails.</returns>
    let load (face: Face) : Feat =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Feat)
        match table.Length with
        | 0 -> empty
        | _ ->
            let version = Binary.u32 table.Span 0
            let count = int (Binary.u16 table.Span 4)
            let names =
                Sealed.init count (fun index ->
                    let span = table.Span
                    let at = HeaderSize + index * FeatureNameSize
                    { Feature = Binary.u16 span (at + FeatureOffset)
                      NSettings = Binary.u16 span (at + NSettingsOffset)
                      SettingTableOffset = Binary.u32 span (at + SettingTableOffset)
                      FeatureFlags = Binary.u16 span (at + FeatureFlagsOffset)
                      NameIndex = Binary.i16 span (at + NameIndexOffset) })
            { Version = version
              Names = names
              Table = table }

    /// <summary>Whether the table has data, <c>feat::has_data</c>.</summary>
    /// <param name="feat">The table.</param>
    /// <returns>True when the version is not zero.</returns>
    let hasData (feat: Feat) = feat.Version <> 0u

    /// <summary>Finds a feature name by type, <c>feat::get_feature</c> (a bsearch).</summary>
    /// <param name="feat">The table.</param>
    /// <param name="featureType">The feature type.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The record, or the Null record when absent.</returns>
    [<TailCall>]
    let rec private search (feat: Feat) (featureType: uint16) (lo: int) (hi: int) : FeatureName =
        match lo > hi with
        | true -> { Feature = 0us; NSettings = 0us; SettingTableOffset = 0u; FeatureFlags = 0us; NameIndex = 0s }
        | false ->
            let mid = (lo + hi) / 2
            let name = feat.Names[mid]
            match featureType < name.Feature, featureType > name.Feature with
            | true, _ -> search feat featureType lo (mid - 1)
            | false, true -> search feat featureType (mid + 1) hi
            | false, false -> name

    /// <summary>Finds a feature name by type, <c>feat::get_feature</c>.</summary>
    /// <param name="feat">The table.</param>
    /// <param name="featureType">The feature type.</param>
    /// <returns>The record, or the Null record when absent.</returns>
    let feature (feat: Feat) (featureType: uint16) : FeatureName =
        search feat featureType 0 (feat.Names.Length - 1)

    /// <summary>Whether the table exposes a feature type, <c>feat::exposes_feature</c>.</summary>
    /// <param name="feat">The table.</param>
    /// <param name="featureType">The feature type.</param>
    /// <returns>True when the feature has at least one setting.</returns>
    let exposesFeature (feat: Feat) (featureType: uint16) =
        FeatureName.hasData (feature feat featureType)

    /// <summary>The selector value that means no selector, <c>HB_AAT_LAYOUT_FEATURE_SELECTOR_INVALID</c>.</summary>
    [<Literal>]
    let SelectorInvalid = 0xFFFFu

    /// <summary>The default index of a feature type without a default selector, <c>HB_AAT_LAYOUT_NO_SELECTOR_INDEX</c>.</summary>
    [<Literal>]
    let NoSelectorIndex = 0xFFFFu

    /// <summary>The bit of the feature flags that makes the settings mutually exclusive, <c>FeatureName::Exclusive</c>.</summary>
    [<Literal>]
    let private ExclusiveFlag = 0x8000us

    /// <summary>The bit of the feature flags that says the default is not the first setting, <c>FeatureName::NotDefault</c>.</summary>
    [<Literal>]
    let private NotDefaultFlag = 0x4000us

    /// <summary>The mask of the default setting index in the feature flags, <c>FeatureName::IndexMask</c>.</summary>
    [<Literal>]
    let private IndexMask = 0x00FFus

    /// <summary>The number of feature types of the table, <c>feat::featureNameCount</c>.</summary>
    /// <param name="feat">The table.</param>
    /// <returns>The count.</returns>
    let featureTypeCount (feat: Feat) = feat.Names.Length

    /// <summary>The window of an array a caller asks for, <c>hb_array_t::sub_array</c>.</summary>
    /// <param name="length">The array length.</param>
    /// <param name="startOffset">The first element asked for.</param>
    /// <param name="count">The number of elements asked for.</param>
    /// <returns>The number of elements the window holds.</returns>
    let private windowCount (length: int) (startOffset: uint32) (count: uint32) =
        match int64 startOffset > int64 length with
        | true -> 0
        | false -> int (min (uint32 (int64 length - int64 startOffset)) count)

    /// <summary>The feature types of the table in a window, <c>feat::get_feature_types</c>.</summary>
    /// <param name="feat">The table.</param>
    /// <param name="startOffset">The first feature type asked for.</param>
    /// <param name="count">The most feature types asked for.</param>
    /// <returns>The feature types of the window.</returns>
    let featureTypes (feat: Feat) (startOffset: uint32) (count: uint32) =
        let window = windowCount feat.Names.Length startOffset count
        Sealed.init window (fun index -> feat.Names[int startOffset + index].Feature)

    /// <summary>The name id of a feature type, <c>feat::get_feature_name_id</c>.</summary>
    /// <param name="feat">The table.</param>
    /// <param name="featureType">The feature type.</param>
    /// <returns>The name id, sign-extended from the stored 16-bit value as C does; zero for an absent type.</returns>
    let featureNameId (feat: Feat) (featureType: uint16) =
        uint32 (int (feature feat featureType).NameIndex)

    /// <summary>The Null <c>SettingName</c> an index past the array reads, <c>DEFINE_NULL_NAMESPACE_BYTES (AAT, SettingName)</c>
    /// in <c>hb-static.cc</c>: every byte is 0xFF.</summary>
    [<Literal>]
    let private NullSetting = 0xFFFFus

    /// <summary>Reads one setting name of a feature, <c>SettingName</c>; an index past the array reads the Null object.</summary>
    /// <param name="feat">The table.</param>
    /// <param name="name">The feature name.</param>
    /// <param name="index">The setting index.</param>
    /// <returns>The setting and its name index.</returns>
    let private settingAt (feat: Feat) (name: FeatureName) (index: int) =
        match index < int name.NSettings with
        | false -> struct (NullSetting, NullSetting)
        | true ->
            let at = int name.SettingTableOffset + index * SettingNameSize
            struct (Binary.u16 feat.Table.Span at, Binary.u16 feat.Table.Span (at + 2))

    /// <summary>The default index of a feature, <c>FeatureName::get_selector_infos</c>.</summary>
    /// <param name="name">The feature name.</param>
    /// <returns>The default index, or <c>NoSelectorIndex</c> when the settings are not exclusive.</returns>
    let private defaultIndex (name: FeatureName) =
        match name.FeatureFlags &&& ExclusiveFlag <> 0us, name.FeatureFlags &&& NotDefaultFlag <> 0us with
        | false, _ -> NoSelectorIndex
        | true, false -> 0u
        | true, true -> uint32 (name.FeatureFlags &&& IndexMask)

    /// <summary>The selectors of a feature type in a window, <c>feat::get_selector_infos</c>.</summary>
    /// <param name="feat">The table.</param>
    /// <param name="featureType">The feature type.</param>
    /// <param name="startOffset">The first selector asked for.</param>
    /// <param name="count">The most selectors asked for.</param>
    /// <returns>The number of selectors of the type, the default index, and the selectors of the window.</returns>
    let selectorInfos (feat: Feat) (featureType: uint16) (startOffset: uint32) (count: uint32) =
        let name = feature feat featureType
        let defaultAt = defaultIndex name
        let defaultSelector =
            match defaultAt with
            | NoSelectorIndex -> SelectorInvalid
            | index ->
                let struct (setting, _) = settingAt feat name (int index)
                uint32 setting
        let window = windowCount (int name.NSettings) startOffset count
        let infos =
            Sealed.init window (fun index ->
                let struct (setting, nameIndex) = settingAt feat name (int startOffset + index)
                { NameId = uint32 nameIndex
                  Enable = uint32 setting
                  Disable = (match defaultSelector with SelectorInvalid -> uint32 setting + 1u | selector -> selector)
                  Reserved = 0u })
        struct (uint32 name.NSettings, defaultAt, infos)
