namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The <c>hb_ot_layout_*</c> GDEF query functions.</summary>
[<RequireQualifiedAccess>]
module internal LayoutApi =
    /// <summary>Scales a coordinate by the direction's axis, <c>em_scale_dir</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="coordinate">The design coordinate.</param>
    /// <returns>The scaled coordinate.</returns>
    let private emScaleDir (font: Font) (direction: Direction) (coordinate: int) : int =
        match Direction.isHorizontal direction with
        | true -> Font.emScaleX font coordinate
        | false -> Font.emScaleY font coordinate

    /// <summary>Computes one caret value, <c>CaretValue::get_caret_value</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="span">The ligature-caret-list bytes.</param>
    /// <param name="cvOffset">The caret-value offset.</param>
    /// <returns>The caret position.</returns>
    let private caretValue (font: Font) (direction: Direction) (span: ReadOnlySpan<byte>) (cvOffset: int) : int =
        let horizontal = Direction.isHorizontal direction
        match Binary.u16 span cvOffset with
        | 1us ->
            let coordinate = int (Binary.i16 span (cvOffset + 2))
            emScaleDir font direction coordinate
        | 2us -> 0
        | 3us ->
            let coordinate = int (Binary.i16 span (cvOffset + 2))
            let device =
                match uint32 (Binary.u16 span (cvOffset + 4)) with
                | 0u -> Device.NoDevice
                | offset -> DeviceTable.parse span (cvOffset + int offset)
            let delta =
                match horizontal with
                | true -> Device.xDelta font device
                | false -> Device.yDelta font device
            int (Math.Clamp(int64 (emScaleDir font direction coordinate) + int64 delta, int64 Int32.MinValue, int64 Int32.MaxValue))
        | _ -> 0

    /// <summary>Reads the offset field of a ligature glyph at an index, or none.</summary>
    /// <param name="span">The ligature-caret-list bytes.</param>
    /// <param name="index">The coverage index.</param>
    /// <param name="countPos">The ligature-glyph count offset.</param>
    /// <param name="countWidth">The count width.</param>
    /// <param name="offsetsStart">The first ligature-glyph offset.</param>
    /// <param name="offsetWidth">The offset width.</param>
    /// <returns>The ligature-glyph offset, or none.</returns>
    let private ligGlyphOffset (span: ReadOnlySpan<byte>) (index: int) (countPos: int) (countWidth: int) (offsetsStart: int) (offsetWidth: int) : int voption =
        let count = int (Coverage.readWide span countPos countWidth)
        match index < count with
        | false -> ValueNone
        | true ->
            match Coverage.readWide span (offsetsStart + index * offsetWidth) offsetWidth with
            | 0u -> ValueNone
            | offset -> ValueSome (int offset)

    /// <summary>Unfolds the caret values of one ligature glyph.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LigGlyphCaretsUnfolder =
        { Font: Font
          Direction: Direction
          Base: int }
        interface ISpanUnfolder<byte, int, int> with
            member this.Next (span, index) =
                let at = this.Base + 2 + index * 2
                let caret =
                    match Binary.u16 span at with
                    | 0us -> 0
                    | offset -> caretValue this.Font this.Direction span (this.Base + int offset)
                struct (caret, index + 1)

    /// <summary>Collects the caret positions of one ligature glyph.</summary>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="span">The ligature-caret-list bytes.</param>
    /// <param name="lgOffset">The ligature-glyph offset.</param>
    /// <returns>The carets.</returns>
    let private ligGlyphCarets (font: Font) (direction: Direction) (span: ReadOnlySpan<byte>) (lgOffset: int) : ImmutableArray<int> =
        let count = int (Binary.u16 span lgOffset)
        Sealed.unfoldSpan span count 0 { LigGlyphCaretsUnfolder.Font = font; Direction = direction; Base = lgOffset }

    /// <summary>Collects the caret positions at a coverage index of a ligature-caret list.</summary>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="span">The ligature-caret-list bytes.</param>
    /// <param name="index">The coverage index.</param>
    /// <param name="countPos">The ligature-glyph count offset.</param>
    /// <param name="countWidth">The count width.</param>
    /// <param name="offsetsStart">The first ligature-glyph offset.</param>
    /// <param name="offsetWidth">The offset width.</param>
    /// <returns>The carets.</returns>
    let private ligCaretListCaretsAt (font: Font) (direction: Direction) (span: ReadOnlySpan<byte>) (index: int) (countPos: int) (countWidth: int) (offsetsStart: int) (offsetWidth: int) : ImmutableArray<int> =
        match ligGlyphOffset span index countPos countWidth offsetsStart offsetWidth with
        | ValueNone -> ImmutableArray.Empty
        | ValueSome lgOffset -> ligGlyphCarets font direction span lgOffset

    /// <summary>Collects the caret positions of a glyph in a ligature-caret list.</summary>
    /// <param name="wide">Whether the 24-bit medium form is in use.</param>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="span">The ligature-caret-list bytes.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The carets.</returns>
    let private ligCaretListCarets (wide: bool) (font: Font) (direction: Direction) (span: ReadOnlySpan<byte>) (glyph: uint32) : ImmutableArray<int> =
        match wide with
        | false ->
            match Binary.u16 span 0 with
            | 0us -> ImmutableArray.Empty
            | covOffset ->
                match Coverage.index (Coverage.parse span (int covOffset)) glyph with
                | CoverageHit.NotCovered -> ImmutableArray.Empty
                | CoverageHit.Covered index -> ligCaretListCaretsAt font direction span index 2 2 4 2
        | true ->
            match Binary.u32 span 0 with
            | 0u -> ImmutableArray.Empty
            | covOffset ->
                match Coverage.index (Coverage.parse span (int covOffset)) glyph with
                | CoverageHit.NotCovered -> ImmutableArray.Empty
                | CoverageHit.Covered index -> ligCaretListCaretsAt font direction span index 4 3 7 3

    /// <summary>Unfolds the points of one attach point table.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private AttachPointsUnfolder =
        { Base: int }
        interface ISpanUnfolder<byte, int, uint32> with
            member this.Next (span, index) =
                struct (uint32 (Binary.u16 span (this.Base + 2 + index * 2)), index + 1)

    /// <summary>Collects the points of one attach point table.</summary>
    /// <param name="span">The attach-list bytes.</param>
    /// <param name="apOffset">The attach-point offset.</param>
    /// <returns>The points.</returns>
    let private attachPointsOf (span: ReadOnlySpan<byte>) (apOffset: int) : ImmutableArray<uint32> =
        let count = int (Binary.u16 span apOffset)
        Sealed.unfoldSpan span count 0 { AttachPointsUnfolder.Base = apOffset }

    /// <summary>Collects the attach points of a coverage index, <c>AttachList::get_attach_points</c>.</summary>
    /// <param name="span">The attach-list bytes.</param>
    /// <param name="index">The coverage index.</param>
    /// <returns>The points.</returns>
    let private attachPointAt (span: ReadOnlySpan<byte>) (index: int) : ImmutableArray<uint32> =
        match index < int (Binary.u16 span 2) with
        | false -> ImmutableArray.Empty
        | true ->
            match Binary.u16 span (4 + index * 2) with
            | 0us -> ImmutableArray.Empty
            | offset -> attachPointsOf span (int offset)

    /// <summary>Collects the attach points of a glyph from an attach list.</summary>
    /// <param name="span">The attach-list bytes.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The points.</returns>
    let private attachPoints (span: ReadOnlySpan<byte>) (glyph: uint32) : ImmutableArray<uint32> =
        match Binary.u16 span 0 with
        | 0us -> ImmutableArray.Empty
        | covOffset ->
            match Coverage.index (Coverage.parse span (int covOffset)) glyph with
            | CoverageHit.NotCovered -> ImmutableArray.Empty
            | CoverageHit.Covered index -> attachPointAt span index

    /// <summary>Returns a glyph's class, <c>hb_ot_layout_get_glyph_class</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The class, zero when unclassified.</returns>
    let getGlyphClass (face: OtFace) (glyph: uint32) : uint32 =
        ClassDef.classOf face.Gdef.Value.GlyphClasses glyph

    /// <summary>Returns the glyphs of a class as sorted disjoint ranges, <c>hb_ot_layout_get_glyphs_in_class</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="klass">The class.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let getGlyphsInClass (face: OtFace) (klass: uint32) : ImmutableArray<CodepointRange> =
        ClassDef.glyphsInClass face.Gdef.Value.GlyphClasses klass

    /// <summary>Returns a glyph's attach points, <c>hb_ot_layout_get_attach_points</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The points.</returns>
    let getAttachPoints (face: OtFace) (glyph: uint32) : ImmutableArray<uint32> =
        let attach = face.Gdef.Value.AttachList
        match attach.IsEmpty with
        | true -> ImmutableArray.Empty
        | false -> attachPoints attach.Span glyph

    /// <summary>Returns a glyph's ligature carets, <c>hb_ot_layout_get_ligature_carets</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The caret positions.</returns>
    let getLigatureCarets (font: Font) (direction: Direction) (glyph: uint32) : ImmutableArray<int> =
        let gdef = font.Ot.Gdef.Value
        match gdef.LigCarets.IsEmpty with
        | true -> ImmutableArray.Empty
        | false -> ligCaretListCarets gdef.LigCaretsWide font direction gdef.LigCarets.Span glyph

    /// <summary>The script tags of a GSUB or GPOS table, <c>hb_ot_layout_table_get_script_tags</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <returns>The tags in file order.</returns>
    let scriptTags (face: OtFace) (table: Tag) : ImmutableArray<Tag> =
        match table = Tags.Gsub with
        | true -> LayoutTable.scriptTags face.Gsub.Value
        | false -> LayoutTable.scriptTags face.Gpos.Value

    /// <summary>Selects a script, <c>hb_ot_layout_table_select_script</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="candidates">The candidate script tags.</param>
    /// <returns>Whether a candidate matched, the script index and the chosen tag.</returns>
    let selectScript (face: OtFace) (table: Tag) (candidates: ImmutableArray<Tag>) : struct (bool * int * Tag) =
        match table = Tags.Gsub with
        | true -> LayoutTable.selectScript face.Gsub.Value candidates
        | false -> LayoutTable.selectScript face.Gpos.Value candidates

    /// <summary>The language-system tags of a script, <c>hb_ot_layout_script_get_language_tags</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <returns>The tags in file order.</returns>
    let languageTags (face: OtFace) (table: Tag) (scriptIndex: int) : ImmutableArray<Tag> =
        match table = Tags.Gsub with
        | true ->
            match scriptIndex >= 0 && scriptIndex < face.Gsub.Value.Scripts.Length with
            | true -> face.Gsub.Value.Scripts[scriptIndex].LangSysTags
            | false -> ImmutableArray.Empty
        | false ->
            match scriptIndex >= 0 && scriptIndex < face.Gpos.Value.Scripts.Length with
            | true -> face.Gpos.Value.Scripts[scriptIndex].LangSysTags
            | false -> ImmutableArray.Empty

    /// <summary>Selects a language system, <c>hb_ot_layout_script_select_language2</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="candidates">The candidate language tags.</param>
    /// <returns>Whether a candidate matched, the language index and the chosen tag.</returns>
    let selectLanguage (face: OtFace) (table: Tag) (scriptIndex: int) (candidates: ImmutableArray<Tag>) : struct (bool * int * Tag) =
        match table = Tags.Gsub with
        | true -> LayoutTable.selectLanguage face.Gsub.Value scriptIndex candidates
        | false -> LayoutTable.selectLanguage face.Gpos.Value scriptIndex candidates

    /// <summary>The required feature of a language system, <c>hb_ot_layout_language_get_required_feature</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="languageIndex">The language index.</param>
    /// <returns>Whether a required feature exists, its index and its tag.</returns>
    let requiredFeature (face: OtFace) (table: Tag) (scriptIndex: int) (languageIndex: int) : struct (bool * uint32 * Tag) =
        match table = Tags.Gsub with
        | true ->
            let langSys = LayoutTable.langSys face.Gsub.Value scriptIndex languageIndex
            struct (langSys.RequiredFeature <> uint32 LayoutTable.NoFeatureIndex, langSys.RequiredFeature, LayoutTable.featureTag face.Gsub.Value (int langSys.RequiredFeature))
        | false ->
            let langSys = LayoutTable.langSys face.Gpos.Value scriptIndex languageIndex
            struct (langSys.RequiredFeature <> uint32 LayoutTable.NoFeatureIndex, langSys.RequiredFeature, LayoutTable.featureTag face.Gpos.Value (int langSys.RequiredFeature))

    /// <summary>The feature indices of a language system, <c>hb_ot_layout_language_get_feature_indexes</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="languageIndex">The language index.</param>
    /// <returns>The indices in file order.</returns>
    let featureIndexes (face: OtFace) (table: Tag) (scriptIndex: int) (languageIndex: int) : ImmutableArray<uint16> =
        match table = Tags.Gsub with
        | true -> (LayoutTable.langSys face.Gsub.Value scriptIndex languageIndex).FeatureIndices
        | false -> (LayoutTable.langSys face.Gpos.Value scriptIndex languageIndex).FeatureIndices

    /// <summary>The feature tags of a language system, <c>hb_ot_layout_language_get_feature_tags</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="scriptIndex">The script index.</param>
    /// <param name="languageIndex">The language index.</param>
    /// <returns>The tags in file order.</returns>
    let featureTags (face: OtFace) (table: Tag) (scriptIndex: int) (languageIndex: int) : ImmutableArray<Tag> =
        match table = Tags.Gsub with
        | true ->
            let table = face.Gsub.Value
            let langSys = LayoutTable.langSys table scriptIndex languageIndex
            Sealed.map (fun index -> LayoutTable.featureTag table (int index)) (langSys.FeatureIndices.AsSpan())
        | false ->
            let table = face.Gpos.Value
            let langSys = LayoutTable.langSys table scriptIndex languageIndex
            Sealed.map (fun index -> LayoutTable.featureTag table (int index)) (langSys.FeatureIndices.AsSpan())

    /// <summary>The feature tags of a table, <c>hb_ot_layout_table_get_feature_tags</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <returns>The tags in file order.</returns>
    let tableFeatureTags (face: OtFace) (table: Tag) : ImmutableArray<Tag> =
        match table = Tags.Gsub with
        | true -> LayoutTable.featureTags face.Gsub.Value
        | false -> LayoutTable.featureTags face.Gpos.Value

    /// <summary>Finds a feature's index, <c>hb_ot_layout_table_find_feature</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="featureTag">The feature tag.</param>
    /// <returns>Whether found and the feature index.</returns>
    let findFeature (face: OtFace) (table: Tag) (featureTag: Tag) : struct (bool * int) =
        match table = Tags.Gsub with
        | true ->
            match LayoutTable.findFeature face.Gsub.Value featureTag with
            | ValueSome index -> struct (true, index)
            | ValueNone -> struct (false, LayoutTable.NoFeatureIndex)
        | false ->
            match LayoutTable.findFeature face.Gpos.Value featureTag with
            | ValueSome index -> struct (true, index)
            | ValueNone -> struct (false, LayoutTable.NoFeatureIndex)

    /// <summary>The lookup indices of a feature, <c>hb_ot_layout_feature_get_lookups</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <returns>The indices in file order.</returns>
    let featureLookups (face: OtFace) (table: Tag) (featureIndex: int) : ImmutableArray<uint16> =
        match table = Tags.Gsub with
        | true ->
            match featureIndex >= 0 && featureIndex < face.Gsub.Value.Features.Length with
            | true -> face.Gsub.Value.Features[featureIndex].LookupIndices
            | false -> ImmutableArray.Empty
        | false ->
            match featureIndex >= 0 && featureIndex < face.Gpos.Value.Features.Length with
            | true -> face.Gpos.Value.Features[featureIndex].LookupIndices
            | false -> ImmutableArray.Empty

    /// <summary>The number of lookups, <c>hb_ot_layout_table_get_lookup_count</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <returns>The count.</returns>
    let lookupCount (face: OtFace) (table: Tag) : int =
        match table = Tags.Gsub with
        | true -> LayoutTable.lookupCount face.Gsub.Value
        | false -> LayoutTable.lookupCount face.Gpos.Value

    /// <summary>Whether the GSUB table has data, <c>hb_ot_layout_has_substitution</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>True when the table is present and non-empty.</returns>
    let hasSubstitution (face: OtFace) : bool =
        face.Gsub.Value.Version <> 0u

    /// <summary>Whether the GPOS table has data, <c>hb_ot_layout_has_positioning</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>True when the table is present and non-empty.</returns>
    let hasPositioning (face: OtFace) : bool =
        face.Gpos.Value.Version <> 0u

    /// <summary>Whether a ligature's components match a glyph sequence, <c>Ligature::would_apply</c>.</summary>
    /// <param name="components">The components after the first glyph.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next component, one-based.</param>
    /// <param name="count">The component count, including the first glyph.</param>
    /// <returns>True when every component matches.</returns>
    [<TailCall>]
    let rec private ligatureComponentsMatch (components: ImmutableArray<uint32>) (glyphs: ImmutableArray<uint32>) (index: int) (count: int) =
        match index < count with
        | false -> true
        | true ->
            match glyphs[index] = components[index - 1] with
            | false -> false
            | true -> ligatureComponentsMatch components glyphs (index + 1) count

    /// <summary>Whether a ligature set would apply to a glyph sequence, <c>LigatureSet::would_apply</c>.</summary>
    /// <param name="ligatures">The ligatures.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next ligature.</param>
    /// <returns>True when a ligature would apply.</returns>
    [<TailCall>]
    let rec private ligatureSetWouldApply (ligatures: ImmutableArray<Ligature>) (glyphs: ImmutableArray<uint32>) (index: int) =
        match index < ligatures.Length with
        | false -> false
        | true ->
            let lig = ligatures[index]
            match glyphs.Length = GsubTables.ligCount lig && ligatureComponentsMatch lig.Components glyphs 1 (GsubTables.ligCount lig) with
            | true -> true
            | false -> ligatureSetWouldApply ligatures glyphs (index + 1)

    /// <summary>Whether a ligature subtable would apply to a glyph sequence, <c>LigatureSubstFormat1::would_apply</c>.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <param name="sets">The ligature sets.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <returns>True when a ligature would apply.</returns>
    let private ligatureSubtableWouldApply coverage (sets: ImmutableArray<ImmutableArray<Ligature>>) (glyphs: ImmutableArray<uint32>) =
        match glyphs.IsEmpty with
        | true -> false
        | false ->
            match Coverage.index coverage glyphs[0] with
            | CoverageHit.NotCovered -> false
            | CoverageHit.Covered index ->
                match index < sets.Length with
                | false -> false
                | true -> ligatureSetWouldApply sets[index] glyphs 0

    /// <summary>Whether the input values after the first glyph match a sequence, <c>would_match_input</c>
    /// (src/hb-ot-layout-gsubgpos.hh:1918-1936).</summary>
    /// <param name="classDef">The class definition, none for glyph values.</param>
    /// <param name="input">The input values after the first glyph.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next glyph, one-based.</param>
    /// <returns>True when every value matches.</returns>
    [<TailCall>]
    let rec private wouldMatchInputLoop (classDef: ClassDef voption) (input: ImmutableArray<uint32>) (glyphs: ImmutableArray<uint32>) index =
        match index < glyphs.Length with
        | false -> true
        | true ->
            let matched =
                match classDef with
                | ValueNone -> glyphs[index] = input[index - 1]
                | ValueSome classDef -> ClassDef.classOf classDef glyphs[index] = input[index - 1]
            match matched with
            | false -> false
            | true -> wouldMatchInputLoop classDef input glyphs (index + 1)

    /// <summary>Whether a rule's input matches a glyph sequence, <c>would_match_input</c>.</summary>
    /// <param name="classDef">The class definition, none for glyph values.</param>
    /// <param name="inputCount">The input count as stored, including the first glyph.</param>
    /// <param name="input">The input values after the first glyph.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <returns>True when the sequence matches.</returns>
    let private wouldMatchInput (classDef: ClassDef voption) inputCount (input: ImmutableArray<uint32>) (glyphs: ImmutableArray<uint32>) =
        match glyphs.Length = inputCount with
        | false -> false
        | true -> wouldMatchInputLoop classDef input glyphs 1

    /// <summary>Whether the coverages after the first match a glyph sequence, <c>would_match_input</c> with
    /// <c>match_coverage</c>.</summary>
    [<TailCall>]
    let rec private wouldMatchCoverageLoop (coverages: ImmutableArray<Coverage>) (glyphs: ImmutableArray<uint32>) index =
        match index < glyphs.Length with
        | false -> true
        | true ->
            match Coverage.index coverages[index] glyphs[index] with
            | CoverageHit.NotCovered -> false
            | CoverageHit.Covered _ -> wouldMatchCoverageLoop coverages glyphs (index + 1)

    /// <summary>Whether a coverage-based context rule matches a glyph sequence.</summary>
    /// <param name="coverages">The input coverages, including the first.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <returns>True when the sequence matches.</returns>
    let private wouldMatchCoverage (coverages: ImmutableArray<Coverage>) (glyphs: ImmutableArray<uint32>) =
        match glyphs.Length = coverages.Length with
        | false -> false
        | true -> wouldMatchCoverageLoop coverages glyphs 1

    /// <summary>Whether a context rule set would apply, <c>RuleSet::would_apply</c>.</summary>
    /// <param name="classDef">The class definition, none for glyph rules.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next rule.</param>
    /// <returns>True when a rule would apply.</returns>
    [<TailCall>]
    let rec private contextRuleSetWouldApply (classDef: ClassDef voption) (rules: ImmutableArray<Rule>) (glyphs: ImmutableArray<uint32>) index =
        match index < rules.Length with
        | false -> false
        | true ->
            match wouldMatchInput classDef rules[index].InputCount rules[index].Input glyphs with
            | true -> true
            | false -> contextRuleSetWouldApply classDef rules glyphs (index + 1)

    /// <summary>Whether a chained rule set would apply, <c>ChainRuleSet::would_apply</c>.</summary>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <param name="classDef">The input class definition, none for glyph rules.</param>
    /// <param name="rules">The rules.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next rule.</param>
    /// <returns>True when a rule would apply.</returns>
    [<TailCall>]
    let rec private chainRuleSetWouldApply zeroContext (classDef: ClassDef voption) (rules: ImmutableArray<ChainRule>) (glyphs: ImmutableArray<uint32>) index =
        match index < rules.Length with
        | false -> false
        | true ->
            let rule = rules[index]
            match (not zeroContext || (rule.Backtrack.IsEmpty && rule.Lookahead.IsEmpty)) && wouldMatchInput classDef rule.ChainInputCount rule.ChainInput glyphs with
            | true -> true
            | false -> chainRuleSetWouldApply zeroContext classDef rules glyphs (index + 1)

    /// <summary>Whether a context subtable of the first or fourth format would apply, <c>ContextFormat1_4::would_apply</c>:
    /// the rule set the first glyph's coverage index selects, which is empty for a glyph the coverage does not hold.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <returns>True when a rule would apply.</returns>
    let private glyphRulesWouldApply (coverage: Coverage) (ruleSets: ImmutableArray<ImmutableArray<Rule>>) (glyphs: ImmutableArray<uint32>) =
        match glyphs.IsEmpty with
        | true -> false
        | false ->
            match Coverage.index coverage glyphs[0] with
            | CoverageHit.Covered index when index < ruleSets.Length -> contextRuleSetWouldApply ValueNone ruleSets[index] glyphs 0
            | CoverageHit.Covered _ | CoverageHit.NotCovered -> false

    /// <summary>Whether a chained-context subtable of the first or fourth format would apply.</summary>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <param name="coverage">The coverage.</param>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <returns>True when a rule would apply.</returns>
    let private chainGlyphRulesWouldApply (zeroContext: bool) (coverage: Coverage) (ruleSets: ImmutableArray<ImmutableArray<ChainRule>>) (glyphs: ImmutableArray<uint32>) =
        match glyphs.IsEmpty with
        | true -> false
        | false ->
            match Coverage.index coverage glyphs[0] with
            | CoverageHit.Covered index when index < ruleSets.Length -> chainRuleSetWouldApply zeroContext ValueNone ruleSets[index] glyphs 0
            | CoverageHit.Covered _ | CoverageHit.NotCovered -> false

    /// <summary>Whether one subtable would apply to a glyph sequence, <c>would_apply</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <returns>True when the subtable would substitute the sequence.</returns>
    let private subtableWouldApply subtable zeroContext (glyphs: ImmutableArray<uint32>) =
        match subtable with
        | SingleDelta (coverage, _, _)
        | SingleList (coverage, _)
        | Multiple (coverage, _)
        | Alternate (coverage, _) ->
            match glyphs.Length = 1 with
            | false -> false
            | true ->
                match Coverage.index coverage glyphs[0] with
                | CoverageHit.NotCovered -> false
                | CoverageHit.Covered _ -> true
        | LigatureSubst (coverage, sets, _) -> ligatureSubtableWouldApply coverage sets glyphs
        | SubstContext (GlyphRules (coverage, ruleSets)) -> glyphRulesWouldApply coverage ruleSets glyphs
        | SubstContext (ClassRules (_, classDef, ruleSets, _)) ->
            match glyphs.IsEmpty with
            | true -> false
            | false ->
                let classIndex = int (ClassDef.classOf classDef glyphs[0])
                match classIndex < ruleSets.Length with
                | false -> false
                | true -> contextRuleSetWouldApply (ValueSome classDef) ruleSets[classIndex] glyphs 0
        | SubstContext (CoverageRule (coverages, _)) -> wouldMatchCoverage coverages glyphs
        | SubstChainContext (ChainGlyphRules (coverage, ruleSets)) -> chainGlyphRulesWouldApply zeroContext coverage ruleSets glyphs
        | SubstChainContext (ChainClassRules (_, _, inputClassDef, _, ruleSets, _)) ->
            match glyphs.IsEmpty with
            | true -> false
            | false ->
                let classIndex = int (ClassDef.classOf inputClassDef glyphs[0])
                match classIndex < ruleSets.Length with
                | false -> false
                | true -> chainRuleSetWouldApply zeroContext (ValueSome inputClassDef) ruleSets[classIndex] glyphs 0
        | SubstChainContext (ChainCoverageRule (backtrack, coverages, lookahead, _)) ->
            (not zeroContext || (backtrack.IsEmpty && lookahead.IsEmpty)) && wouldMatchCoverage coverages glyphs
        | ReverseChainSingle (coverage, _, _, _) ->
            match glyphs.Length = 1 with
            | false -> false
            | true ->
                match Coverage.index coverage glyphs[0] with
                | CoverageHit.NotCovered -> false
                | CoverageHit.Covered _ -> true
        | SubstPending _ -> false

    /// <summary>Whether any subtable would apply, <c>SubstLookup::would_apply</c>'s dispatch.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>True when a subtable would substitute the sequence.</returns>
    [<TailCall>]
    let rec private wouldApplyAny (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) zeroContext (glyphs: ImmutableArray<uint32>) index =
        match index < subtables.Length with
        | false -> false
        | true ->
            match subtableWouldApply subtables[index].Subtable zeroContext glyphs with
            | true -> true
            | false -> wouldApplyAny subtables zeroContext glyphs (index + 1)

    /// <summary>Whether a lookup would substitute a glyph sequence, <c>hb_ot_layout_lookup_would_substitute</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="lookup">The lookup index.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <returns>True when the lookup would substitute the sequence.</returns>
    let wouldSubstitute face lookup (glyphs: ImmutableArray<uint32>) (zeroContext: bool) =
        let gsub = face.Gsub.Value
        match lookup < gsub.Lookups.Length && not glyphs.IsEmpty with
        | false -> false
        | true ->
            let accel = gsub.Lookups[lookup].Value
            match Digest.mayHave glyphs[0] accel.LookupDigest with
            | false -> false
            | true -> wouldApplyAny accel.Subtables zeroContext glyphs 0

    /// <summary>The lookups of one feature of a face made ready to answer <c>would_substitute</c>: the accelerators of the lookups the
    /// table holds, in order, and the union of their digests, so a sequence whose first glyph no lookup can start with is refused by
    /// one digest test. It is a pure function of the face's GSUB table, the lookup indices and the zero-context flag, immutable, and
    /// valid for the face whose table is <c>Source</c>.</summary>
    [<NoEquality; NoComparison>]
    type WouldTester =
        { Source: Lazy<LayoutTable<SubstSubtable>>
          Accels: ImmutableArray<LookupAccel<SubstSubtable>>
          Union: Digest
          ZeroContext: bool }

    /// <summary>Folds the digests of lookup accelerators into one.</summary>
    /// <param name="accels">The accelerators.</param>
    /// <param name="index">The next accelerator.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The union of the lookup digests.</returns>
    [<TailCall>]
    let rec private unionOfLookups (accels: ImmutableArray<LookupAccel<SubstSubtable>>) index digest =
        match index < accels.Length with
        | false -> digest
        | true -> unionOfLookups accels (index + 1) (Digest.union digest accels[index].LookupDigest)

    /// <summary>Makes the tester of the lookups of a feature, which <see cref="wouldSubstitute"/> asks one lookup at a time; a lookup
    /// index the table does not hold answers false there and is left out here.</summary>
    /// <param name="face">The face.</param>
    /// <param name="lookups">The lookup indices of the feature, in order.</param>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <returns>The tester.</returns>
    let prepareWould (face: OtFace) (lookups: ImmutableArray<int>) (zeroContext: bool) =
        let gsub = face.Gsub.Value
        let accels =
            Sealed.choose (fun lookup ->
                match lookup < gsub.Lookups.Length with
                | true -> ValueSome gsub.Lookups[lookup].Value
                | false -> ValueNone) (lookups.AsSpan())
        { Source = face.Gsub
          Accels = accels
          Union = unionOfLookups accels 0 Digest.empty
          ZeroContext = zeroContext }

    /// <summary>Whether any lookup of a prepared tester would apply, the lookups in order, each refused by its own digest first.</summary>
    /// <param name="accels">The lookups.</param>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <param name="glyphs">The glyph sequence, not empty.</param>
    /// <param name="index">The next lookup.</param>
    /// <returns>True when a lookup would substitute the sequence.</returns>
    [<TailCall>]
    let rec private wouldApplyLookups (accels: ImmutableArray<LookupAccel<SubstSubtable>>) zeroContext (glyphs: ImmutableArray<uint32>) index =
        match index < accels.Length with
        | false -> false
        | true ->
            let accel = accels[index]
            match Digest.mayHave glyphs[0] accel.LookupDigest && wouldApplyAny accel.Subtables zeroContext glyphs 0 with
            | true -> true
            | false -> wouldApplyLookups accels zeroContext glyphs (index + 1)

    /// <summary>Whether any lookup of a feature would substitute a glyph sequence: <see cref="wouldSubstitute"/> over the lookups,
    /// refused at once when the union of their digests cannot hold the first glyph.</summary>
    /// <param name="tester">The prepared lookups.</param>
    /// <param name="glyphs">The glyph sequence.</param>
    /// <returns>True when a lookup would substitute the sequence.</returns>
    let wouldSubstituteAny (tester: WouldTester) (glyphs: ImmutableArray<uint32>) =
        match glyphs.IsEmpty with
        | true -> false
        | false ->
            match Digest.mayHave glyphs[0] tester.Union with
            | false -> false
            | true -> wouldApplyLookups tester.Accels tester.ZeroContext glyphs 0

    /// <summary>The tester of a feature for a face, kept in the cell of the feature: made the first time the feature is asked about a
    /// face and never changed, so of two racing builders one is kept and both are equal. A cell that holds the tester of another face
    /// (the plan of one map used with a second face) is left alone and the tester is made for the asker alone.</summary>
    /// <param name="face">The face.</param>
    /// <param name="cell">The cell of the feature.</param>
    /// <param name="lookups">The lookup indices of the feature.</param>
    /// <param name="zeroContext">Whether pre- and post-context are disallowed.</param>
    /// <returns>The tester.</returns>
    let testerFor (face: OtFace) (cell: Memo<obj>) (lookups: ImmutableArray<int>) (zeroContext: bool) =
        match cell.TryGet () with
        | ValueSome (:? WouldTester as kept) when obj.ReferenceEquals(kept.Source, face.Gsub) -> kept
        | ValueSome _ -> prepareWould face lookups zeroContext
        | ValueNone ->
            let created = prepareWould face lookups zeroContext
            match cell.Set created with
            | :? WouldTester as held when obj.ReferenceEquals(held.Source, face.Gsub) -> held
            | _ -> created

    /// <summary>One step of the alternates dispatch: continue with a new capacity, or stop with the result.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private AlternatesStep =
        | AlternatesContinue of count: int
        | AlternatesDone of result: struct (int * ImmutableArray<uint32>)

    /// <summary>The written alternates of a single substitution, gated on the remaining capacity.</summary>
    /// <param name="count">The remaining capacity.</param>
    /// <param name="substituted">The substituted glyph.</param>
    /// <returns>The total and the written glyphs.</returns>
    let private singleAlternates count substituted =
        match count > 0 with
        | false -> struct (1, ImmutableArray.Empty)
        | true -> struct (1, Sealed.ofSpan (ReadOnlySpan [| substituted |]))

    /// <summary>The written alternates of an alternate substitution, gated on the remaining capacity.</summary>
    /// <param name="count">The remaining capacity.</param>
    /// <param name="total">The number of alternates.</param>
    /// <param name="startOffset">The first alternate to return.</param>
    /// <param name="alternates">The alternate set.</param>
    /// <returns>The total and the written glyphs.</returns>
    let private alternateResult count total startOffset (alternates: ImmutableArray<uint32>) =
        match count > 0 with
        | false -> struct (total, ImmutableArray.Empty)
        | true ->
            let written = min count (max 0 (total - startOffset))
            struct (total, Sealed.init written (fun k -> alternates[startOffset + k]))

    /// <summary>The alternates one subtable offers for a glyph, threading the shared capacity as
    /// <c>get_glyph_alternates</c> does: a subtable that does not cover the glyph resets the capacity to zero.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="startOffset">The first alternate to return.</param>
    /// <param name="count">The remaining capacity.</param>
    /// <returns>Continue with the new capacity, or the result.</returns>
    let private subtableAlternatesStep subtable glyph startOffset count =
        match subtable with
        | SingleDelta (coverage, delta, glyphMask) ->
            match Coverage.index coverage glyph with
            | CoverageHit.NotCovered -> AlternatesContinue 0
            | CoverageHit.Covered _ -> AlternatesDone (singleAlternates count ((glyph + delta) &&& glyphMask))
        | SingleList (coverage, substitutes) ->
            match Coverage.index coverage glyph with
            | CoverageHit.NotCovered -> AlternatesContinue 0
            | CoverageHit.Covered at ->
                match at < substitutes.Length with
                | false -> AlternatesDone (struct (1, ImmutableArray.Empty))
                | true -> AlternatesDone (singleAlternates count substitutes[at])
        | Alternate (coverage, alternateSets) ->
            match Coverage.index coverage glyph with
            | CoverageHit.NotCovered -> AlternatesContinue 0
            | CoverageHit.Covered at ->
                let alternates = match at < alternateSets.Length with | true -> alternateSets[at] | false -> ImmutableArray<uint32>.Empty
                AlternatesDone (alternateResult count alternates.Length startOffset alternates)
        | Multiple _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> AlternatesContinue count

    /// <summary>Fetches a glyph's alternates from a lookup, threading the shared capacity through the subtables
    /// exactly as <c>hb_ot_layout_lookup_get_glyph_alternates</c>'s dispatch does.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="startOffset">The first alternate to return.</param>
    /// <param name="count">The remaining capacity.</param>
    /// <param name="index">The next subtable.</param>
    /// <returns>The total number of alternates and the returned alternates.</returns>
    [<TailCall>]
    let rec private glyphAlternatesLoop (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) glyph startOffset count index =
        match index < subtables.Length with
        | false -> struct (0, ImmutableArray.Empty)
        | true ->
            match subtableAlternatesStep subtables[index].Subtable glyph startOffset count with
            | AlternatesContinue count -> glyphAlternatesLoop subtables glyph startOffset count (index + 1)
            | AlternatesDone result -> result

    /// <summary>Fetches a glyph's alternates from a lookup, <c>hb_ot_layout_lookup_get_glyph_alternates</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="lookup">The lookup index.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="startOffset">The first alternate to return.</param>
    /// <param name="capacity">The maximum number of alternates to return.</param>
    /// <returns>The total number of alternates and the returned alternates.</returns>
    let glyphAlternates face lookup glyph startOffset capacity =
        let gsub = face.Gsub.Value
        match lookup < gsub.Lookups.Length with
        | false -> struct (0, ImmutableArray.Empty)
        | true -> glyphAlternatesLoop gsub.Lookups[lookup].Value.Subtables glyph startOffset capacity 0

    /// <summary>Collects the alternates one subtable offers, <c>collect_glyph_alternates</c>.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The covered glyph and its alternates, one pair per glyph.</returns>
    let private collectSubtableAlternates subtable =
        match subtable with
        | SingleDelta (coverage, delta, glyphMask) ->
            let glyphs = Coverage.glyphs coverage
            Sealed.map (fun glyph -> struct (glyph, Sealed.ofSpan (ReadOnlySpan [| (glyph + delta) &&& glyphMask |]))) (glyphs.AsSpan())
        | SingleList (coverage, substitutes) ->
            let glyphs = Coverage.glyphs coverage
            let count = min glyphs.Length substitutes.Length
            Sealed.init count (fun i -> struct (glyphs[i], Sealed.ofSpan (ReadOnlySpan [| substitutes[i] |])))
        | Alternate (coverage, alternateSets) ->
            let glyphs = Coverage.glyphs coverage
            let count = min glyphs.Length alternateSets.Length
            Sealed.init count (fun i -> struct (glyphs[i], alternateSets[i]))
        | Multiple _ | LigatureSubst _ | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ | SubstPending _ -> ImmutableArray.Empty

    /// <summary>Collects a lookup's alternates, <c>hb_ot_layout_lookup_collect_glyph_alternates</c>.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="acc">The collected pairs so far.</param>
    /// <returns>The covered glyphs and their alternates.</returns>
    [<TailCall>]
    let rec private collectSubtables (subtables: ImmutableArray<SubtableAccel<SubstSubtable>>) index acc =
        match index < subtables.Length with
        | false -> acc
        | true ->
            let contribution = collectSubtableAlternates subtables[index].Subtable
            collectSubtables subtables (index + 1) (Sealed.appendSpan (acc.AsSpan()) (contribution.AsSpan()))

    /// <summary>Collects every alternate a lookup offers, <c>hb_ot_layout_lookup_collect_glyph_alternates</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="lookup">The lookup index.</param>
    /// <returns>The covered glyphs and their alternates.</returns>
    let collectGlyphAlternates face lookup =
        let gsub = face.Gsub.Value
        match lookup < gsub.Lookups.Length with
        | false -> ImmutableArray.Empty
        | true -> collectSubtables gsub.Lookups[lookup].Value.Subtables 0 ImmutableArray.Empty

    /// <summary>The feature tags enabled by a plan, <c>hb_ot_shape_plan_get_feature_tags</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <returns>The tags in compiled order.</returns>
    let planFeatureTags (plan: OtShapePlan) : ImmutableArray<Tag> =
        Sealed.map (fun feature -> feature.Tag) (plan.Map.Features.AsSpan())

    /// <summary>The sorted unique lookup indices of a plan's table, <c>hb_ot_shape_plan_collect_lookups</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="tableTag">The table tag, GSUB or GPOS.</param>
    /// <returns>The lookup indices.</returns>
    let planCollectLookups (plan: OtShapePlan) (tableTag: Tag) : ImmutableArray<int> =
        let lookups =
            match tableTag = Tags.Gsub with
            | true -> plan.Map.GsubLookups
            | false -> plan.Map.GposLookups
        let pairs = Sealed.map (fun lookup -> struct (uint32 lookup.LookupIndex, uint32 lookup.LookupIndex)) (lookups.AsSpan())
        let sorted = Sealed.sortedMap (pairs.AsSpan())
        Sealed.map (fun (struct (key, _)) -> int key) (sorted.AsSpan())

    /// <summary>The maximum lookup recursion depth, <c>HB_MAX_NESTING_LEVEL</c>.</summary>
    [<Literal>]
    let private MaxNestingLevel = 64

    /// <summary>The four glyph sets a lookup collects, as the not-yet-unioned add operations.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private CollectSets =
        { Before: ImmutableArray<SetOp>
          Input: ImmutableArray<SetOp>
          After: ImmutableArray<SetOp>
          Output: ImmutableArray<SetOp> }

    /// <summary>The empty collected sets.</summary>
    let private emptySets =
        { Before = ImmutableArray.Empty
          Input = ImmutableArray.Empty
          After = ImmutableArray.Empty
          Output = ImmutableArray.Empty }

    /// <summary>One single-point add of a glyph.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The add.</returns>
    let private glyphAdd glyph = SetOp.Add (glyph, glyph)

    /// <summary>A singleton array of one add operation.</summary>
    /// <param name="op">The operation.</param>
    /// <returns>The array.</returns>
    let private singleOp op = Sealed.init 1 (fun _ -> op)

    /// <summary>A value guarded by a condition, for a filter-map.</summary>
    /// <param name="condition">Whether to keep the value.</param>
    /// <param name="value">The value.</param>
    /// <returns>The value, or none.</returns>
    let private optionWhen condition value =
        match condition with
        | true -> ValueSome value
        | false -> ValueNone

    /// <summary>Appends add operations to the input set.</summary>
    /// <param name="sets">The sets.</param>
    /// <param name="adds">The adds.</param>
    /// <returns>The updated sets.</returns>
    let private addInput (sets: CollectSets) (adds: ImmutableArray<SetOp>) =
        { sets with Input = Sealed.appendSpan (sets.Input.AsSpan()) (adds.AsSpan()) }

    /// <summary>Appends add operations to the before set.</summary>
    /// <param name="sets">The sets.</param>
    /// <param name="adds">The adds.</param>
    /// <returns>The updated sets.</returns>
    let private addBefore (sets: CollectSets) (adds: ImmutableArray<SetOp>) =
        { sets with Before = Sealed.appendSpan (sets.Before.AsSpan()) (adds.AsSpan()) }

    /// <summary>Appends add operations to the after set.</summary>
    /// <param name="sets">The sets.</param>
    /// <param name="adds">The adds.</param>
    /// <returns>The updated sets.</returns>
    let private addAfter (sets: CollectSets) (adds: ImmutableArray<SetOp>) =
        { sets with After = Sealed.appendSpan (sets.After.AsSpan()) (adds.AsSpan()) }

    /// <summary>Appends add operations to the output set.</summary>
    /// <param name="sets">The sets.</param>
    /// <param name="adds">The adds.</param>
    /// <returns>The updated sets.</returns>
    let private addOutput (sets: CollectSets) (adds: ImmutableArray<SetOp>) =
        { sets with Output = Sealed.appendSpan (sets.Output.AsSpan()) (adds.AsSpan()) }

    /// <summary>The add operations of a coverage, <c>Coverage::collect_coverage</c>: every glyph for an array form, every
    /// range for a range form, a broken range dropping out as <c>add_range</c> drops it.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The adds.</returns>
    let rec private coverageOps (coverage: Coverage) =
        match coverage with
        | CoverageNull -> ImmutableArray.Empty
        | CoverageDirect (_, _, source) -> coverageOps source
        | CoverageGlyphs glyphs -> Sealed.map glyphAdd (glyphs.AsSpan())
        | CoverageRanges ranges -> Sealed.map (fun (record: RangeRecord) -> SetOp.Add (record.First, record.Last)) (ranges.AsSpan())

    /// <summary>The add operations of a class, <c>ClassDef::collect_class</c>: every entry of the class, no stop at a broken
    /// matching range.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <param name="klass">The class to collect.</param>
    /// <returns>The adds.</returns>
    let rec private classOps (classDef: ClassDef) (klass: uint32) =
        match classDef with
        | ClassDefNull -> ImmutableArray.Empty
        | ClassDefDirect (_, _, source) -> classOps source klass
        | ClassDefArray (startGlyph, classes) ->
            let options = Sealed.mapi (fun index value -> optionWhen (value = klass) (glyphAdd (startGlyph + uint32 index))) (classes.AsSpan())
            Sealed.choose id (options.AsSpan())
        | ClassDefRanges ranges ->
            Sealed.choose (fun (record: RangeRecord) -> optionWhen (record.Value = klass) (SetOp.Add (record.First, record.Last))) (ranges.AsSpan())

    /// <summary>Walks an array class definition's <c>collect_coverage</c> exactly: the non-zero class runs, each extended
    /// to the following class-zero glyph as <c>add_range (startGlyph + start, startGlyph + i)</c> does.</summary>
    /// <param name="classes">The class values.</param>
    /// <param name="startGlyph">The first classified glyph.</param>
    /// <param name="index">The next class value.</param>
    /// <param name="start">The first glyph of the current non-zero run.</param>
    /// <param name="acc">The adds so far.</param>
    /// <returns>The adds.</returns>
    [<TailCall>]
    let rec private arrayCoverageOps (classes: ReadOnlySpan<uint32>) (startGlyph: uint32) (index: int) (start: int) (acc: ImmutableArray<SetOp>) =
        match index < classes.Length with
        | false ->
            match start < classes.Length with
            | true -> Sealed.appendOne (acc.AsSpan()) (SetOp.Add (startGlyph + uint32 start, startGlyph + uint32 classes.Length))
            | false -> acc
        | true ->
            match classes[index] = 0u with
            | false -> arrayCoverageOps classes startGlyph (index + 1) start acc
            | true ->
                let acc =
                    match start < index with
                    | true -> Sealed.appendOne (acc.AsSpan()) (SetOp.Add (startGlyph + uint32 start, startGlyph + uint32 index))
                    | false -> acc
                arrayCoverageOps classes startGlyph (index + 1) (index + 1) acc

    /// <summary>The add operations of a class definition's <c>collect_coverage</c>: the non-zero class glyphs, the array
    /// form extended by its following class-zero glyph as upstream does.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <returns>The adds.</returns>
    let rec private classCoverageOps (classDef: ClassDef) =
        match classDef with
        | ClassDefNull -> ImmutableArray.Empty
        | ClassDefDirect (_, _, source) -> classCoverageOps source
        | ClassDefArray (startGlyph, classes) -> arrayCoverageOps (classes.AsSpan()) startGlyph 0 0 ImmutableArray.Empty
        | ClassDefRanges ranges ->
            Sealed.choose (fun (record: RangeRecord) -> optionWhen (record.Value <> 0u) (SetOp.Add (record.First, record.Last))) (ranges.AsSpan())

    /// <summary>The add operations of an array of glyphs, one single-point add each.</summary>
    /// <param name="glyphs">The glyphs.</param>
    /// <returns>The adds.</returns>
    let private glyphsOps (glyphs: ImmutableArray<uint32>) =
        Sealed.map glyphAdd (glyphs.AsSpan())

    /// <summary>The lookup indices a rule's records name, in design order.</summary>
    /// <param name="records">The records.</param>
    /// <returns>The lookup indices.</returns>
    let private recordLookups (records: ImmutableArray<LookupRecord>) =
        Sealed.map (fun (record: LookupRecord) -> record.LookupListIndex) (records.AsSpan())

    /// <summary>The glyph count a zip with a coverage yields, <c>hb_zip (coverage, array)</c>: the coverage's
    /// <c>iter_t</c> length.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The iterated count.</returns>
    let private zipCount (coverage: Coverage) = (Coverage.glyphs coverage).Length

    /// <summary>Flattens the first <paramref name="count"/> glyph arrays into single-point adds.</summary>
    /// <param name="parts">The glyph arrays.</param>
    /// <param name="count">The number of arrays to flatten.</param>
    /// <param name="index">The next array.</param>
    /// <param name="acc">The adds so far.</param>
    /// <returns>The adds.</returns>
    [<TailCall>]
    let rec private flattenGlyphOps (parts: ImmutableArray<ImmutableArray<uint32>>) (count: int) (index: int) (acc: ImmutableArray<SetOp>) =
        match index < count with
        | false -> acc
        | true ->
            let glyphs = glyphsOps parts[index]
            flattenGlyphOps parts count (index + 1) (Sealed.appendSpan (acc.AsSpan()) (glyphs.AsSpan()))

    /// <summary>Folds the class values of one rule into the accumulator, <c>collect_class</c> per value.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <param name="values">The class values.</param>
    /// <param name="index">The next value.</param>
    /// <param name="acc">The adds so far.</param>
    /// <returns>The adds.</returns>
    [<TailCall>]
    let rec private classValuesOps (classDef: ClassDef) (values: ImmutableArray<uint32>) (index: int) (acc: ImmutableArray<SetOp>) =
        match index < values.Length with
        | false -> acc
        | true ->
            let adds = classOps classDef values[index]
            classValuesOps classDef values (index + 1) (Sealed.appendSpan (acc.AsSpan()) (adds.AsSpan()))

    /// <summary>Folds the glyph rules of every rule set into the collected sets and the nested lookups.</summary>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="setIndex">The next rule set.</param>
    /// <param name="ruleIndex">The next rule of the current set.</param>
    /// <param name="sets">The sets so far.</param>
    /// <param name="nested">The nested lookups so far.</param>
    /// <returns>The sets and the nested lookups.</returns>
    [<TailCall>]
    let rec private glyphRulesOps (ruleSets: ImmutableArray<ImmutableArray<Rule>>) (setIndex: int) (ruleIndex: int) (sets: CollectSets) (nested: ImmutableArray<int>) =
        match setIndex < ruleSets.Length with
        | false -> struct (sets, nested)
        | true ->
            match ruleIndex < ruleSets[setIndex].Length with
            | false -> glyphRulesOps ruleSets (setIndex + 1) 0 sets nested
            | true ->
                let rule = ruleSets[setIndex][ruleIndex]
                let input = glyphsOps rule.Input
                let records = recordLookups rule.Records
                let sets = addInput sets input
                let nested = Sealed.appendSpan (nested.AsSpan()) (records.AsSpan())
                glyphRulesOps ruleSets setIndex (ruleIndex + 1) sets nested

    /// <summary>Folds the class rules of every rule set into the collected sets and the nested lookups.</summary>
    /// <param name="classDef">The input class definition.</param>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="setIndex">The next rule set.</param>
    /// <param name="ruleIndex">The next rule of the current set.</param>
    /// <param name="sets">The sets so far.</param>
    /// <param name="nested">The nested lookups so far.</param>
    /// <returns>The sets and the nested lookups.</returns>
    [<TailCall>]
    let rec private classRulesOps (classDef: ClassDef) (ruleSets: ImmutableArray<ImmutableArray<Rule>>) (setIndex: int) (ruleIndex: int) (sets: CollectSets) (nested: ImmutableArray<int>) =
        match setIndex < ruleSets.Length with
        | false -> struct (sets, nested)
        | true ->
            match ruleIndex < ruleSets[setIndex].Length with
            | false -> classRulesOps classDef ruleSets (setIndex + 1) 0 sets nested
            | true ->
                let rule = ruleSets[setIndex][ruleIndex]
                let input = classValuesOps classDef rule.Input 0 ImmutableArray.Empty
                let records = recordLookups rule.Records
                let sets = addInput sets input
                let nested = Sealed.appendSpan (nested.AsSpan()) (records.AsSpan())
                classRulesOps classDef ruleSets setIndex (ruleIndex + 1) sets nested

    /// <summary>Folds the chained glyph rules of every rule set into the collected sets and the nested lookups.</summary>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="setIndex">The next rule set.</param>
    /// <param name="ruleIndex">The next rule of the current set.</param>
    /// <param name="sets">The sets so far.</param>
    /// <param name="nested">The nested lookups so far.</param>
    /// <returns>The sets and the nested lookups.</returns>
    [<TailCall>]
    let rec private chainGlyphRulesOps (ruleSets: ImmutableArray<ImmutableArray<ChainRule>>) (setIndex: int) (ruleIndex: int) (sets: CollectSets) (nested: ImmutableArray<int>) =
        match setIndex < ruleSets.Length with
        | false -> struct (sets, nested)
        | true ->
            match ruleIndex < ruleSets[setIndex].Length with
            | false -> chainGlyphRulesOps ruleSets (setIndex + 1) 0 sets nested
            | true ->
                let rule = ruleSets[setIndex][ruleIndex]
                let sets = addBefore sets (glyphsOps rule.Backtrack)
                let sets = addInput sets (glyphsOps rule.ChainInput)
                let sets = addAfter sets (glyphsOps rule.Lookahead)
                let records = recordLookups rule.ChainRecords
                let nested = Sealed.appendSpan (nested.AsSpan()) (records.AsSpan())
                chainGlyphRulesOps ruleSets setIndex (ruleIndex + 1) sets nested

    /// <summary>Folds the chained class rules of every rule set into the collected sets and the nested lookups.</summary>
    /// <param name="backtrackClasses">The backtrack class definition.</param>
    /// <param name="inputClasses">The input class definition.</param>
    /// <param name="lookaheadClasses">The lookahead class definition.</param>
    /// <param name="ruleSets">The rule sets.</param>
    /// <param name="setIndex">The next rule set.</param>
    /// <param name="ruleIndex">The next rule of the current set.</param>
    /// <param name="sets">The sets so far.</param>
    /// <param name="nested">The nested lookups so far.</param>
    /// <returns>The sets and the nested lookups.</returns>
    [<TailCall>]
    let rec private chainClassRulesOps (backtrackClasses: ClassDef) (inputClasses: ClassDef) (lookaheadClasses: ClassDef) (ruleSets: ImmutableArray<ImmutableArray<ChainRule>>) (setIndex: int) (ruleIndex: int) (sets: CollectSets) (nested: ImmutableArray<int>) =
        match setIndex < ruleSets.Length with
        | false -> struct (sets, nested)
        | true ->
            match ruleIndex < ruleSets[setIndex].Length with
            | false -> chainClassRulesOps backtrackClasses inputClasses lookaheadClasses ruleSets (setIndex + 1) 0 sets nested
            | true ->
                let rule = ruleSets[setIndex][ruleIndex]
                let sets = addBefore sets (classValuesOps backtrackClasses rule.Backtrack 0 ImmutableArray.Empty)
                let sets = addInput sets (classValuesOps inputClasses rule.ChainInput 0 ImmutableArray.Empty)
                let sets = addAfter sets (classValuesOps lookaheadClasses rule.Lookahead 0 ImmutableArray.Empty)
                let records = recordLookups rule.ChainRecords
                let nested = Sealed.appendSpan (nested.AsSpan()) (records.AsSpan())
                chainClassRulesOps backtrackClasses inputClasses lookaheadClasses ruleSets setIndex (ruleIndex + 1) sets nested

    /// <summary>Folds the coverage arrays of a coverage rule into the input set.</summary>
    /// <param name="coverages">The coverages after the first.</param>
    /// <param name="index">The next coverage.</param>
    /// <param name="acc">The adds so far.</param>
    /// <returns>The adds.</returns>
    [<TailCall>]
    let rec private coveragesOps (coverages: ImmutableArray<Coverage>) (index: int) (acc: ImmutableArray<SetOp>) =
        match index < coverages.Length with
        | false -> acc
        | true ->
            let adds = coverageOps coverages[index]
            coveragesOps coverages (index + 1) (Sealed.appendSpan (acc.AsSpan()) (adds.AsSpan()))

    /// <summary>Folds the ligatures of every ligature set into the collected sets.</summary>
    /// <param name="sets">The ligature sets.</param>
    /// <param name="count">The number of sets to visit.</param>
    /// <param name="setIndex">The next set.</param>
    /// <param name="ligIndex">The next ligature of the current set.</param>
    /// <param name="acc">The sets so far.</param>
    /// <returns>The sets.</returns>
    [<TailCall>]
    let rec private ligaturesOps (sets: ImmutableArray<ImmutableArray<Ligature>>) (count: int) (setIndex: int) (ligIndex: int) (acc: CollectSets) =
        match setIndex < count with
        | false -> acc
        | true ->
            match ligIndex < sets[setIndex].Length with
            | false -> ligaturesOps sets count (setIndex + 1) 0 acc
            | true ->
                let lig = sets[setIndex][ligIndex]
                let acc =
                    match lig.Components.IsDefault with
                    | true -> acc
                    | false -> addInput acc (glyphsOps lig.Components)
                let acc = addOutput acc (singleOp (glyphAdd lig.LigGlyph))
                ligaturesOps sets count setIndex (ligIndex + 1) acc

    /// <summary>The collected sets and nested lookups of one GSUB context subtable.</summary>
    /// <param name="rules">The context rules.</param>
    /// <returns>The sets and the nested lookups.</returns>
    let private contextCollect (rules: ContextRules) =
        match rules with
        | GlyphRules (coverage, ruleSets) ->
            let struct (sets, nested) = glyphRulesOps ruleSets 0 0 emptySets ImmutableArray.Empty
            struct (addInput sets (coverageOps coverage), nested)
        | ClassRules (coverage, classDef, ruleSets, _) ->
            let struct (sets, nested) = classRulesOps classDef ruleSets 0 0 emptySets ImmutableArray.Empty
            struct (addInput sets (coverageOps coverage), nested)
        | CoverageRule (coverages, records) ->
            let sets = addInput emptySets (coveragesOps coverages 0 ImmutableArray.Empty)
            struct (sets, recordLookups records)

    /// <summary>The collected sets and nested lookups of one GSUB chained-context subtable.</summary>
    /// <param name="rules">The chained rules.</param>
    /// <returns>The sets and the nested lookups.</returns>
    let private chainContextCollect (rules: ChainRules) =
        match rules with
        | ChainGlyphRules (coverage, ruleSets) ->
            let struct (sets, nested) = chainGlyphRulesOps ruleSets 0 0 emptySets ImmutableArray.Empty
            struct (addInput sets (coverageOps coverage), nested)
        | ChainClassRules (coverage, backtrackClasses, inputClasses, lookaheadClasses, ruleSets, _) ->
            let struct (sets, nested) = chainClassRulesOps backtrackClasses inputClasses lookaheadClasses ruleSets 0 0 emptySets ImmutableArray.Empty
            struct (addInput sets (coverageOps coverage), nested)
        | ChainCoverageRule (backtrack, input, lookahead, records) ->
            let sets = addBefore emptySets (coveragesOps backtrack 0 ImmutableArray.Empty)
            let sets = addInput sets (coveragesOps input 0 ImmutableArray.Empty)
            let sets = addAfter sets (coveragesOps lookahead 0 ImmutableArray.Empty)
            struct (sets, recordLookups records)

    /// <summary>The collected sets and nested lookups of one GSUB subtable.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The sets and the nested lookups.</returns>
    let private gsubCollect (subtable: SubstSubtable) =
        match subtable with
        | SingleDelta (coverage, delta, glyphMask) ->
            let input = coverageOps coverage
            let output =
                Sealed.map (fun glyph -> glyphAdd ((glyph + delta) &&& glyphMask)) ((Coverage.glyphs coverage).AsSpan())
            struct ({ emptySets with Input = input; Output = output }, ImmutableArray.Empty)
        | SingleList (coverage, substitutes) ->
            let count = min (zipCount coverage) substitutes.Length
            let input = coverageOps coverage
            let output = Sealed.init count (fun index -> glyphAdd substitutes[index])
            struct ({ emptySets with Input = input; Output = output }, ImmutableArray.Empty)
        | Multiple (coverage, sequences) ->
            let count = min (zipCount coverage) sequences.Length
            let input = coverageOps coverage
            let output = flattenGlyphOps sequences count 0 ImmutableArray.Empty
            struct ({ emptySets with Input = input; Output = output }, ImmutableArray.Empty)
        | Alternate (coverage, alternateSets) ->
            let count = min (zipCount coverage) alternateSets.Length
            let input = coverageOps coverage
            let output = flattenGlyphOps alternateSets count 0 ImmutableArray.Empty
            struct ({ emptySets with Input = input; Output = output }, ImmutableArray.Empty)
        | LigatureSubst (coverage, sets, _) ->
            let count = min (zipCount coverage) sets.Length
            let sets = ligaturesOps sets count 0 0 emptySets
            struct (addInput sets (coverageOps coverage), ImmutableArray.Empty)
        | SubstContext rules -> contextCollect rules
        | SubstChainContext rules -> chainContextCollect rules
        | ReverseChainSingle (coverage, backtrack, lookahead, substitutes) ->
            let sets = addBefore emptySets (coveragesOps backtrack 0 ImmutableArray.Empty)
            let sets = addInput sets (coverageOps coverage)
            let sets = addAfter sets (coveragesOps lookahead 0 ImmutableArray.Empty)
            let sets = addOutput sets (glyphsOps substitutes)
            struct (sets, ImmutableArray.Empty)
        | SubstPending _ -> struct (emptySets, ImmutableArray.Empty)

    /// <summary>The collected sets of one GPOS subtable; a GPOS context never recurses, so the nested list is empty.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The sets and the nested lookups.</returns>
    let private gposCollect (subtable: PosSubtable) =
        match subtable with
        | SinglePos (coverage, _, _, _, _, _) -> struct ({ emptySets with Input = coverageOps coverage }, ImmutableArray.Empty)
        | PairPosGlyphs (coverage, _, _, _, seconds, _, _, _, _) ->
            let first = coverageOps coverage
            let second = glyphsOps seconds
            struct ({ emptySets with Input = Sealed.appendSpan (first.AsSpan()) (second.AsSpan()) }, ImmutableArray.Empty)
        | PairPosClasses (coverage, _, _, _, classDef2, _, _, _, _, _, _, _, _) ->
            let first = coverageOps coverage
            let second = classCoverageOps classDef2
            struct ({ emptySets with Input = Sealed.appendSpan (first.AsSpan()) (second.AsSpan()) }, ImmutableArray.Empty)
        | CursivePos cursive -> struct ({ emptySets with Input = coverageOps cursive.CursiveCoverage }, ImmutableArray.Empty)
        | MarkBasePos attachment ->
            let first = coverageOps attachment.MarkCoverage
            let second = coverageOps attachment.TargetCoverage
            struct ({ emptySets with Input = Sealed.appendSpan (first.AsSpan()) (second.AsSpan()) }, ImmutableArray.Empty)
        | MarkLigPos ligature ->
            let first = coverageOps ligature.LigMarkCoverage
            let second = coverageOps ligature.LigatureCoverage
            struct ({ emptySets with Input = Sealed.appendSpan (first.AsSpan()) (second.AsSpan()) }, ImmutableArray.Empty)
        | MarkMarkPos attachment ->
            let first = coverageOps attachment.MarkCoverage
            let second = coverageOps attachment.TargetCoverage
            struct ({ emptySets with Input = Sealed.appendSpan (first.AsSpan()) (second.AsSpan()) }, ImmutableArray.Empty)
        | PosContext rules ->
            let struct (sets, _) = contextCollect rules
            struct (sets, ImmutableArray.Empty)
        | PosChainContext rules ->
            let struct (sets, _) = chainContextCollect rules
            struct (sets, ImmutableArray.Empty)
        | PosPending _ -> struct (emptySets, ImmutableArray.Empty)

    /// <summary>One frame of the collect depth-first walk.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private CollectFrame =
        | CollectSubtable of lookupIndex: int * subtableIndex: int * topLevel: bool * depth: int
        | Recurse of lookupIndex: int * depth: int
        | Complete of lookupIndex: int

    /// <summary>The state of the collect depth-first walk.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private CollectState =
        { Sets: CollectSets
          Recursed: ImmutableArray<int>
          Stack: CollectFrame list }

    /// <summary>Whether an integer array contains a value.</summary>
    /// <param name="values">The values.</param>
    /// <param name="target">The target.</param>
    /// <param name="index">The next value.</param>
    /// <returns>True when the value is present.</returns>
    [<TailCall>]
    let rec private containsInt (values: ReadOnlySpan<int>) (target: int) (index: int) =
        match index < values.Length with
        | false -> false
        | true -> match values[index] = target with | true -> true | false -> containsInt values target (index + 1)

    /// <summary>Pushes one recurse frame per nested lookup, last first so the first pops next.</summary>
    /// <param name="nested">The nested lookups in design order.</param>
    /// <param name="depth">The caller's depth.</param>
    /// <param name="index">The next nested lookup.</param>
    /// <param name="stack">The stack so far.</param>
    /// <returns>The stack.</returns>
    [<TailCall>]
    let rec private pushNestedFrames (nested: ReadOnlySpan<int>) (depth: int) (index: int) (stack: CollectFrame list) =
        match index >= 0 with
        | false -> stack
        | true -> pushNestedFrames nested depth (index - 1) (Recurse (nested[index], depth) :: stack)

    /// <summary>Appends a subtable's contribution to the accumulator, dropping the context sets outside the top level.</summary>
    /// <param name="acc">The accumulator.</param>
    /// <param name="contribution">The contribution.</param>
    /// <param name="topLevel">Whether this is the top-level lookup.</param>
    /// <returns>The updated accumulator.</returns>
    let private addContribution (acc: CollectSets) (contribution: CollectSets) (topLevel: bool) =
        { Before = match topLevel with | true -> Sealed.appendSpan (acc.Before.AsSpan()) (contribution.Before.AsSpan()) | false -> acc.Before
          Input = match topLevel with | true -> Sealed.appendSpan (acc.Input.AsSpan()) (contribution.Input.AsSpan()) | false -> acc.Input
          After = match topLevel with | true -> Sealed.appendSpan (acc.After.AsSpan()) (contribution.After.AsSpan()) | false -> acc.After
          Output = Sealed.appendSpan (acc.Output.AsSpan()) (contribution.Output.AsSpan()) }

    /// <summary>The depth-first collect walk, <c>hb_collect_glyphs_context_t::recurse</c> over an explicit stack.</summary>
    /// <param name="subtableCollect">The per-subtable collector.</param>
    /// <param name="lookups">The table's lookups.</param>
    /// <param name="state">The walk state.</param>
    /// <returns>The collected sets.</returns>
    [<TailCall>]
    let rec private collectLoop (subtableCollect: 'S -> struct (CollectSets * ImmutableArray<int>)) (lookups: ImmutableArray<Lazy<LookupAccel<'S>>>) (state: CollectState) =
        match state.Stack with
        | [] -> state.Sets
        | frame :: rest ->
            match frame with
            | Complete lookupIndex ->
                collectLoop subtableCollect lookups { state with Recursed = Sealed.appendOne (state.Recursed.AsSpan()) lookupIndex; Stack = rest }
            | Recurse (lookupIndex, depth) ->
                match depth = 0 || containsInt (state.Recursed.AsSpan()) lookupIndex 0 with
                | true -> collectLoop subtableCollect lookups { state with Stack = rest }
                | false -> collectLoop subtableCollect lookups { state with Stack = Complete lookupIndex :: CollectSubtable (lookupIndex, 0, false, depth - 1) :: rest }
            | CollectSubtable (lookupIndex, subtableIndex, topLevel, depth) ->
                let accel = lookups[lookupIndex].Value
                match subtableIndex < accel.Subtables.Length with
                | false -> collectLoop subtableCollect lookups { state with Stack = rest }
                | true ->
                    let struct (contribution, nested) = subtableCollect accel.Subtables[subtableIndex].Subtable
                    let sets = addContribution state.Sets contribution topLevel
                    let stack = pushNestedFrames (nested.AsSpan()) depth (nested.Length - 1) (CollectSubtable (lookupIndex, subtableIndex + 1, topLevel, depth) :: rest)
                    collectLoop subtableCollect lookups { state with Sets = sets; Stack = stack }

    /// <summary>Collects a lookup's glyph sets, <c>hb_ot_layout_lookup_collect_glyphs</c>'s body.</summary>
    /// <param name="subtableCollect">The per-subtable collector.</param>
    /// <param name="lookups">The table's lookups.</param>
    /// <param name="lookupIndex">The lookup index.</param>
    /// <returns>The collected sets.</returns>
    let private collectGlyphsOf (subtableCollect: 'S -> struct (CollectSets * ImmutableArray<int>)) (lookups: ImmutableArray<Lazy<LookupAccel<'S>>>) (lookupIndex: int) =
        match lookupIndex < lookups.Length with
        | false -> emptySets
        | true ->
            let state = { Sets = emptySets; Recursed = ImmutableArray.Empty; Stack = [ CollectSubtable (lookupIndex, 0, true, MaxNestingLevel) ] }
            collectLoop subtableCollect lookups state

    /// <summary>Collects a lookup's before, input, after and output glyph sets, <c>hb_ot_layout_lookup_collect_glyphs</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="lookup">The lookup index.</param>
    /// <returns>The four sorted disjoint ranges.</returns>
    let lookupCollectGlyphs (face: OtFace) (table: Tag) (lookup: int) =
        let sets =
            match table = Tags.Gsub with
            | true -> collectGlyphsOf gsubCollect face.Gsub.Value.Lookups lookup
            | false -> collectGlyphsOf gposCollect face.Gpos.Value.Lookups lookup
        struct (Sealed.rangeSet (sets.Before.AsSpan()), Sealed.rangeSet (sets.Input.AsSpan()), Sealed.rangeSet (sets.After.AsSpan()), Sealed.rangeSet (sets.Output.AsSpan()))

    /// <summary>The empty language system, <c>Null (LangSys)</c>.</summary>
    let private nullLangSysOf : LangSys =
        { RequiredFeature = uint32 LayoutTable.NoFeatureIndex
          FeatureIndices = ImmutableArray.Empty }

    /// <summary>Sorts an integer array and drops duplicates, the <c>hb_set_t</c> semantics.</summary>
    /// <param name="values">The values.</param>
    /// <returns>The sorted unique values.</returns>
    let private sortedUniqueInts (values: ImmutableArray<int>) =
        match values.IsEmpty with
        | true -> ImmutableArray.Empty
        | false ->
            let pairs = Sealed.map (fun value -> struct (uint32 value, uint32 value)) (values.AsSpan())
            let sorted = Sealed.sortedMap (pairs.AsSpan())
            Sealed.map (fun (struct (key, _)) -> int key) (sorted.AsSpan())

    /// <summary>Whether a tag array contains a tag.</summary>
    /// <param name="tags">The tags.</param>
    /// <param name="target">The target.</param>
    /// <param name="index">The next tag.</param>
    /// <returns>True when the tag is present.</returns>
    [<TailCall>]
    let rec private containsTag (tags: ImmutableArray<Tag>) (target: Tag) (index: int) =
        match index < tags.Length with
        | false -> false
        | true -> match tags[index] = target with | true -> true | false -> containsTag tags target (index + 1)

    /// <summary>Binary-searches a script's language-system tags, <c>RecordArrayOf&lt;LangSys&gt;::bfind</c>.</summary>
    /// <param name="tags">The tags in file order.</param>
    /// <param name="tag">The key.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private bfindTags (tags: ReadOnlySpan<Tag>) (tag: Tag) (lo: int) (hi: int) =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match uint32 tag > uint32 tags[mid], uint32 tag < uint32 tags[mid] with
            | true, _ -> bfindTags tags tag (mid + 1) hi
            | false, true -> bfindTags tags tag lo (mid - 1)
            | false, false -> ValueSome mid

    /// <summary>Finds a language system of a script by tag, <c>Script::find_lang_sys_index</c>.</summary>
    /// <param name="script">The script.</param>
    /// <param name="tag">The language tag.</param>
    /// <returns>The language index, or none.</returns>
    let private findLangSysIndex (script: ScriptRecord) (tag: Tag) =
        let tags = script.LangSysTags.AsSpan()
        bfindTags tags tag 0 (tags.Length - 1)

    /// <summary>Collects the language indices the requested tags name.</summary>
    /// <param name="script">The script.</param>
    /// <param name="tags">The requested language tags.</param>
    /// <param name="index">The next tag.</param>
    /// <param name="acc">The indices so far.</param>
    /// <returns>The indices.</returns>
    [<TailCall>]
    let rec private langIndices (script: ScriptRecord) (tags: ImmutableArray<Tag>) (index: int) (acc: ImmutableArray<int>) =
        match index < tags.Length with
        | false -> acc
        | true ->
            let acc =
                match findLangSysIndex script tags[index] with
                | ValueNone -> acc
                | ValueSome langIndex -> Sealed.appendOne (acc.AsSpan()) langIndex
            langIndices script tags (index + 1) acc

    /// <summary>The language indices of a script to visit: minus one for the default then every indexed one, or each
    /// requested tag that matches.</summary>
    /// <param name="script">The script.</param>
    /// <param name="languages">The requested languages, none for all.</param>
    /// <returns>The language indices.</returns>
    let private langsOf (script: ScriptRecord) (languages: ImmutableArray<Tag> voption) =
        match languages with
        | ValueNone ->
            match script.DefaultLangSys with
            | ValueNone -> Sealed.init script.LangSys.Length (fun index -> index)
            | ValueSome _ -> Sealed.init (script.LangSys.Length + 1) (fun index -> index - 1)
        | ValueSome tags -> langIndices script tags 0 ImmutableArray.Empty

    /// <summary>The language system of a script at an index, the Null language system when absent.</summary>
    /// <param name="script">The script.</param>
    /// <param name="langIndex">The language index, minus one for the default.</param>
    /// <returns>The language system.</returns>
    let private langSysOf (script: ScriptRecord) (langIndex: int) =
        match langIndex with
        | -1 -> match script.DefaultLangSys with | ValueSome langSys -> langSys | ValueNone -> nullLangSysOf
        | _ -> match langIndex >= 0 && langIndex < script.LangSys.Length with | true -> script.LangSys[langIndex] | false -> nullLangSysOf

    /// <summary>Collects the feature indices of a language system that pass the filter.</summary>
    /// <param name="langSys">The language system.</param>
    /// <param name="filter">The feature-index filter.</param>
    /// <param name="index">The next feature.</param>
    /// <param name="acc">The feature indices so far.</param>
    /// <returns>The feature indices.</returns>
    [<TailCall>]
    let rec private filteredFeatures (langSys: LangSys) (filter: ImmutableArray<int>) (index: int) (acc: ImmutableArray<int>) =
        match index < langSys.FeatureIndices.Length with
        | false -> acc
        | true ->
            let featureIndex = int langSys.FeatureIndices[index]
            match containsInt (filter.AsSpan()) featureIndex 0 with
            | true -> filteredFeatures langSys filter (index + 1) (Sealed.appendOne (acc.AsSpan()) featureIndex)
            | false -> filteredFeatures langSys filter (index + 1) acc

    /// <summary>The feature indices of one language system: the required feature then all, or the filtered ones.</summary>
    /// <param name="langSys">The language system.</param>
    /// <param name="filter">The feature-index filter, none for all features.</param>
    /// <param name="acc">The feature indices so far.</param>
    /// <returns>The feature indices.</returns>
    let private langSysFeatures (langSys: LangSys) (filter: ImmutableArray<int> voption) (acc: ImmutableArray<int>) =
        match filter with
        | ValueNone ->
            let acc =
                match langSys.RequiredFeature <> uint32 LayoutTable.NoFeatureIndex with
                | true -> Sealed.appendOne (acc.AsSpan()) (int langSys.RequiredFeature)
                | false -> acc
            let features = Sealed.map (fun (feature: uint16) -> int feature) (langSys.FeatureIndices.AsSpan())
            Sealed.appendSpan (acc.AsSpan()) (features.AsSpan())
        | ValueSome filter -> filteredFeatures langSys filter 0 acc

    /// <summary>Collects the script indices the requested tags name.</summary>
    /// <param name="layout">The table.</param>
    /// <param name="scripts">The requested script tags.</param>
    /// <param name="index">The next tag.</param>
    /// <param name="acc">The indices so far.</param>
    /// <returns>The indices.</returns>
    [<TailCall>]
    let rec private collectScriptIndices (layout: LayoutTable<'S>) (scripts: ImmutableArray<Tag>) (index: int) (acc: ImmutableArray<int>) =
        match index < scripts.Length with
        | false -> acc
        | true ->
            let acc =
                match LayoutTable.findScriptIndex layout scripts[index] with
                | ValueNone -> acc
                | ValueSome scriptIndex -> Sealed.appendOne (acc.AsSpan()) scriptIndex
            collectScriptIndices layout scripts (index + 1) acc

    /// <summary>Builds the feature-index filter: none for all features, else the feature indices whose tag is requested.</summary>
    /// <param name="layout">The table.</param>
    /// <param name="wanted">The requested feature tags.</param>
    /// <param name="index">The next feature.</param>
    /// <param name="acc">The matching feature indices so far.</param>
    /// <returns>The matching feature indices.</returns>
    [<TailCall>]
    let rec private featureFilterIndices (layout: LayoutTable<'S>) (wanted: ImmutableArray<Tag>) (index: int) (acc: ImmutableArray<int>) =
        match index < layout.Features.Length with
        | false -> acc
        | true ->
            let acc =
                match containsTag wanted layout.Features[index].FeatureTag 0 with
                | true -> Sealed.appendOne (acc.AsSpan()) index
                | false -> acc
            featureFilterIndices layout wanted (index + 1) acc

    /// <summary>Visits scripts and their language systems, collecting the feature indices.</summary>
    /// <param name="layout">The table.</param>
    /// <param name="scripts">The script indices to visit.</param>
    /// <param name="languages">The languages, none for all.</param>
    /// <param name="filter">The feature-index filter, none for all.</param>
    /// <param name="scriptPos">The next script.</param>
    /// <param name="langs">The language indices of the current script.</param>
    /// <param name="langPos">The next language.</param>
    /// <param name="acc">The feature indices so far.</param>
    /// <returns>The feature indices.</returns>
    [<TailCall>]
    let rec private visitScripts (layout: LayoutTable<'S>) (scripts: ImmutableArray<int>) (languages: ImmutableArray<Tag> voption) (filter: ImmutableArray<int> voption) (scriptPos: int) (langs: ImmutableArray<int>) (langPos: int) (acc: ImmutableArray<int>) =
        match scriptPos < scripts.Length with
        | false -> acc
        | true ->
            match langPos < langs.Length with
            | false ->
                let next = scriptPos + 1
                match next < scripts.Length with
                | false -> acc
                | true -> visitScripts layout scripts languages filter next (langsOf layout.Scripts[scripts[next]] languages) 0 acc
            | true ->
                let langSys = langSysOf layout.Scripts[scripts[scriptPos]] langs[langPos]
                match langSys.RequiredFeature = uint32 LayoutTable.NoFeatureIndex && langSys.FeatureIndices.Length = 0 with
                | true -> visitScripts layout scripts languages filter scriptPos langs (langPos + 1) acc
                | false -> visitScripts layout scripts languages filter scriptPos langs (langPos + 1) (langSysFeatures langSys filter acc)

    /// <summary>The feature indices a GSUB or GPOS table collects, <c>hb_ot_layout_collect_features</c>.</summary>
    /// <param name="layout">The table.</param>
    /// <param name="scripts">The scripts, none for all.</param>
    /// <param name="languages">The languages, none for all.</param>
    /// <param name="features">The features, none for all.</param>
    /// <returns>The sorted unique feature indices.</returns>
    let private collectFeaturesOf (layout: LayoutTable<'S>) (scripts: ImmutableArray<Tag> voption) (languages: ImmutableArray<Tag> voption) (features: ImmutableArray<Tag> voption) =
        let filter =
            match features with
            | ValueNone -> ValueNone
            | ValueSome tags -> ValueSome (featureFilterIndices layout tags 0 ImmutableArray.Empty)
        let indices =
            match scripts with
            | ValueNone -> Sealed.init layout.Scripts.Length (fun index -> index)
            | ValueSome tags -> collectScriptIndices layout tags 0 ImmutableArray.Empty
        let langs = match indices.Length with | 0 -> ImmutableArray.Empty | _ -> langsOf layout.Scripts[indices[0]] languages
        sortedUniqueInts (visitScripts layout indices languages filter 0 langs 0 ImmutableArray.Empty)

    /// <summary>Appends every feature's lookup indices.</summary>
    /// <param name="layout">The table.</param>
    /// <param name="featureIndices">The feature indices.</param>
    /// <param name="index">The next feature.</param>
    /// <param name="acc">The lookup indices so far.</param>
    /// <returns>The lookup indices.</returns>
    [<TailCall>]
    let rec private collectFeatureLookups (layout: LayoutTable<'S>) (featureIndices: ImmutableArray<int>) (index: int) (acc: ImmutableArray<int>) =
        match index < featureIndices.Length with
        | false -> acc
        | true ->
            let featureIndex = featureIndices[index]
            let lookups =
                match featureIndex >= 0 && featureIndex < layout.Features.Length with
                | true -> Sealed.map (fun (lookup: uint16) -> int lookup) (layout.Features[featureIndex].LookupIndices.AsSpan())
                | false -> ImmutableArray.Empty
            collectFeatureLookups layout featureIndices (index + 1) (Sealed.appendSpan (acc.AsSpan()) (lookups.AsSpan()))

    /// <summary>Collects the feature lookup indices under scripts, languages and features,
    /// <c>hb_ot_layout_collect_lookups</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="table">The table tag, GSUB or GPOS.</param>
    /// <param name="scripts">The scripts, none for all.</param>
    /// <param name="languages">The languages, none for all.</param>
    /// <param name="features">The features, none for all.</param>
    /// <returns>The sorted unique lookup indices.</returns>
    let collectLookups (face: OtFace) (table: Tag) (scripts: ImmutableArray<Tag> voption) (languages: ImmutableArray<Tag> voption) (features: ImmutableArray<Tag> voption) =
        match table = Tags.Gsub with
        | true ->
            let layout = face.Gsub.Value
            sortedUniqueInts (collectFeatureLookups layout (collectFeaturesOf layout scripts languages features) 0 ImmutableArray.Empty)
        | false ->
            let layout = face.Gpos.Value
            sortedUniqueInts (collectFeatureLookups layout (collectFeaturesOf layout scripts languages features) 0 ImmutableArray.Empty)

