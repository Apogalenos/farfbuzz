namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Var

/// <summary>The feature flags of <c>hb_ot_map_feature_flags_t</c>.</summary>
[<RequireQualifiedAccess>]
module internal FeatureFlags =
    /// <summary>No flags.</summary>
    [<Literal>]
    let None = 0x0000u

    /// <summary>The feature applies to all characters, so it gets no mask of its own.</summary>
    [<Literal>]
    let Global = 0x0001u

    /// <summary>The feature has a fallback implementation, so its mask bit is kept when absent.</summary>
    [<Literal>]
    let HasFallback = 0x0002u

    /// <summary>Do not skip over ZWNJ when matching context.</summary>
    [<Literal>]
    let ManualZwnj = 0x0004u

    /// <summary>Do not skip over ZWJ when matching input.</summary>
    [<Literal>]
    let ManualZwj = 0x0008u

    /// <summary>The two manual-joiner flags together.</summary>
    [<Literal>]
    let ManualJoiners = 0x000Cu

    /// <summary>Global with both manual-joiner flags.</summary>
    [<Literal>]
    let GlobalManualJoiners = 0x000Du

    /// <summary>Global with a fallback implementation.</summary>
    [<Literal>]
    let GlobalHasFallback = 0x0003u

    /// <summary>When absent from the language system, search the whole table for the feature.</summary>
    [<Literal>]
    let GlobalSearch = 0x0010u

    /// <summary>Select an alternate glyph at random.</summary>
    [<Literal>]
    let Random = 0x0020u

    /// <summary>Contain lookup application within one syllable.</summary>
    [<Literal>]
    let PerSyllable = 0x0040u

/// <summary>One pause between two stages of a table, <c>hb_ot_map_builder_t::stage_info_t</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal StageInfo =
    { StageIndex: int
      StagePause: PauseKind }

/// <summary>One feature being added to the map, <c>hb_ot_map_builder_t::feature_info_t</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal FeatureInfo =
    { InfoTag: Tag
      Seq: int
      MaxValue: uint32
      InfoFlags: uint32
      DefaultValue: uint32
      GsubStage: int
      GposStage: int }

/// <summary>The OpenType map builder, <c>hb_ot_map_builder_t</c>.</summary>
[<NoEquality; NoComparison>]
type internal MapBuilder =
    { Features: ImmutableArray<FeatureInfo>
      GsubStage: int
      GposStage: int
      GsubPauses: ImmutableArray<StageInfo>
      GposPauses: ImmutableArray<StageInfo>
      IsSimple: bool }

/// <summary>The feature collection and map compilation of <c>hb_ot_map_builder_t</c>.</summary>
[<RequireQualifiedAccess>]
module internal MapBuilder =
    /// <summary>The bit shift of the single global feature bit, <c>8 * sizeof (hb_mask_t) - 1</c>.</summary>
    [<Literal>]
    let private GlobalBitShift = 31

    /// <summary>The single global feature bit, <c>1u &lt;&lt; global_bit_shift</c>.</summary>
    [<Literal>]
    let private GlobalBitMask = 0x80000000u

    /// <summary>The maximum bits one feature may occupy, <c>HB_OT_MAP_MAX_BITS</c>.</summary>
    [<Literal>]
    let private MaxBits = 8

    /// <summary>The largest feature value, <c>HB_OT_MAP_MAX_VALUE</c>.</summary>
    [<Literal>]
    let private MaxValue = 255u

    /// <summary>The maximum script tags per segment, <c>HB_OT_MAX_TAGS_PER_SCRIPT</c>.</summary>
    [<Literal>]
    let private MaxTagsPerScript = 3

    /// <summary>The maximum language tags per segment, <c>HB_OT_MAX_TAGS_PER_LANGUAGE</c>.</summary>
    [<Literal>]
    let private MaxTagsPerLanguage = 3

    /// <summary>The feature index meaning none, <c>HB_OT_LAYOUT_NO_FEATURE_INDEX</c>.</summary>
    [<Literal>]
    let private NoFeatureIndexValue = 0xFFFFu

    /// <summary>The first feature bit, <c>hb_popcount (HB_GLYPH_FLAG_DEFINED) + 1</c>.</summary>
    [<Literal>]
    let private NextBitStart = 4

    /// <summary>The empty builder: no features, no pauses, one stage per table.</summary>
    let empty : MapBuilder =
        { Features = ImmutableArray.Empty
          GsubStage = 0
          GposStage = 0
          GsubPauses = ImmutableArray.Empty
          GposPauses = ImmutableArray.Empty
          IsSimple = true }

    /// <summary>Adds a feature to the builder, <c>add_feature</c>.</summary>
    /// <param name="tag">The feature tag; the empty tag is ignored.</param>
    /// <param name="flags">The feature flags.</param>
    /// <param name="value">The feature value.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the feature appended.</returns>
    let addFeature (tag: Tag) (flags: uint32) (value: uint32) (builder: MapBuilder) : MapBuilder =
        match tag = Tags.Empty with
        | true -> builder
        | false ->
            let info =
                { InfoTag = tag
                  Seq = builder.Features.Length + 1
                  MaxValue = value
                  InfoFlags = flags
                  DefaultValue =
                    match flags &&& FeatureFlags.Global with
                    | 0u -> 0u
                    | _ -> value
                  GsubStage = builder.GsubStage
                  GposStage = builder.GposStage }
            let features = Sealed.appendOne (builder.Features.AsSpan()) info
            { builder with Features = features }

    /// <summary>Enables a feature globally, <c>enable_feature</c>.</summary>
    /// <param name="tag">The feature tag.</param>
    /// <param name="flags">The extra flags to add to <see cref="FeatureFlags.Global"/>.</param>
    /// <param name="value">The feature value.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the feature appended.</returns>
    let enableFeature (tag: Tag) (flags: uint32) (value: uint32) (builder: MapBuilder) : MapBuilder =
        addFeature tag (FeatureFlags.Global ||| flags) value builder

    /// <summary>Disables a feature, <c>disable_feature</c> (src/hb-ot-map.hh:234-235): it is added globally with value zero, so
    /// that it takes no mask and a user feature of the same tag is the one that turns it on.</summary>
    /// <param name="tag">The feature tag.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the feature appended.</returns>
    let disableFeature tag builder =
        addFeature tag FeatureFlags.Global 0u builder

    /// <summary>Adds a pause to one table and starts a new stage, <c>add_pause</c>.</summary>
    /// <param name="builder">The builder.</param>
    /// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
    /// <param name="pause">The pause.</param>
    /// <returns>The builder with the pause recorded and the stage advanced.</returns>
    let private addPause (builder: MapBuilder) (tableIndex: int) (pause: PauseKind) : MapBuilder =
        match tableIndex with
        | 0 ->
            let stageIndex = builder.GsubStage
            let pauses = Sealed.appendOne (builder.GsubPauses.AsSpan()) { StageIndex = stageIndex; StagePause = pause }
            { builder with GsubStage = builder.GsubStage + 1; GsubPauses = pauses }
        | _ ->
            let stageIndex = builder.GposStage
            let pauses = Sealed.appendOne (builder.GposPauses.AsSpan()) { StageIndex = stageIndex; StagePause = pause }
            { builder with GposStage = builder.GposStage + 1; GposPauses = pauses }

    /// <summary>Adds a GSUB pause, <c>add_gsub_pause</c>.</summary>
    /// <param name="pause">The pause.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the pause recorded.</returns>
    let addGsubPause (pause: PauseKind) (builder: MapBuilder) : MapBuilder =
        addPause builder 0 pause

    /// <summary>Adds a GPOS pause, <c>add_gpos_pause</c>.</summary>
    /// <param name="pause">The pause.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the pause recorded.</returns>
    let addGposPause (pause: PauseKind) (builder: MapBuilder) : MapBuilder =
        addPause builder 1 pause

    /// <summary>Adds the common features of <c>common_features</c>.</summary>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the common features appended.</returns>
    let private addCommonFeatures (builder: MapBuilder) : MapBuilder =
        addFeature Tags.Abvm FeatureFlags.Global 1u builder
        |> addFeature Tags.Blwm FeatureFlags.Global 1u
        |> addFeature Tags.Ccmp FeatureFlags.Global 1u
        |> addFeature Tags.Locl FeatureFlags.Global 1u
        |> addFeature Tags.Mark FeatureFlags.GlobalManualJoiners 1u
        |> addFeature Tags.Mkmk FeatureFlags.GlobalManualJoiners 1u
        |> addFeature Tags.Rlig FeatureFlags.Global 1u

    /// <summary>Adds the horizontal features or the vertical <c>vert</c> feature.</summary>
    /// <param name="direction">The segment direction.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the direction features appended.</returns>
    let private addDirectionFeatures (direction: Direction) (builder: MapBuilder) : MapBuilder =
        match Direction.isHorizontal direction with
        | true ->
            addFeature Tags.Calt FeatureFlags.Global 1u builder
            |> addFeature Tags.Clig FeatureFlags.Global 1u
            |> addFeature Tags.Curs FeatureFlags.Global 1u
            |> addFeature Tags.Dist FeatureFlags.Global 1u
            |> addFeature Tags.Kern FeatureFlags.GlobalHasFallback 1u
            |> addFeature Tags.Liga FeatureFlags.Global 1u
            |> addFeature Tags.Rclt FeatureFlags.Global 1u
        | false -> enableFeature Tags.Vert FeatureFlags.GlobalSearch 1u builder

    /// <summary>Appends the user features, one at a time, <c>collect_features</c>'s user loop.</summary>
    /// <param name="features">The user features.</param>
    /// <param name="index">The next feature.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the user features appended.</returns>
    [<TailCall>]
    let rec private addUserFeaturesFrom (features: ImmutableArray<Feature>) (index: int) (builder: MapBuilder) : MapBuilder =
        match index >= features.Length with
        | true -> builder
        | false ->
            let feature = features[index]
            let flags =
                match feature.Start = Feature.GlobalStart && feature.End = Feature.GlobalEnd with
                | true -> FeatureFlags.Global
                | false -> FeatureFlags.None
            addUserFeaturesFrom features (index + 1) (addFeature feature.Tag flags feature.Value builder)

    /// <summary>Collects the features that precede the shaper's own, <c>hb_ot_shape_collect_features</c> up to the
    /// <c>collect_features</c> hook (src/hb-ot-shape.cc:325-365): the map starts simple.</summary>
    /// <param name="props">The segment properties.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the leading features collected.</returns>
    let collectHead (props: SegmentProperties) (builder: MapBuilder) =
        let builder = { builder with IsSimple = true }
        let builder = enableFeature Tags.Rvrn FeatureFlags.None 1u builder
        let builder = addPause builder 0 NoPause
        let builder =
            match props.Direction with
            | Direction.Ltr ->
                enableFeature Tags.Ltra FeatureFlags.None 1u builder
                |> enableFeature Tags.Ltrm FeatureFlags.None 1u
            | Direction.Rtl ->
                enableFeature Tags.Rtla FeatureFlags.None 1u builder
                |> addFeature Tags.Rtlm FeatureFlags.None 1u
            | Direction.Ttb | Direction.Btt | Direction.Invalid -> builder
        let builder =
            addFeature Tags.Frac FeatureFlags.None 1u builder
            |> addFeature Tags.Numr FeatureFlags.None 1u
            |> addFeature Tags.Dnom FeatureFlags.None 1u
        let builder = enableFeature Tags.Rand FeatureFlags.Random MaxValue builder
        let builder = enableFeature Tags.Harf FeatureFlags.None 1u builder
        enableFeature Tags.HarfUpper FeatureFlags.None 1u builder

    /// <summary>Collects the features that follow the shaper's own, <c>hb_ot_shape_collect_features</c> after the
    /// <c>collect_features</c> hook (src/hb-ot-shape.cc:368-399): the common, direction and user features.</summary>
    /// <param name="props">The segment properties.</param>
    /// <param name="userFeatures">The user features.</param>
    /// <param name="builder">The builder.</param>
    /// <returns>The builder with the trailing features collected.</returns>
    let collectTail (props: SegmentProperties) (userFeatures: ImmutableArray<Feature>) (builder: MapBuilder) =
        let builder = enableFeature Tags.Buzz FeatureFlags.None 1u builder
        let builder = enableFeature Tags.BuzzUpper FeatureFlags.None 1u builder
        let builder = addCommonFeatures builder
        let builder = addDirectionFeatures props.Direction builder
        match userFeatures.IsEmpty with
        | true -> builder
        | false -> addUserFeaturesFrom userFeatures 0 { builder with IsSimple = false }

    /// <summary>Returns the bit length of a value, <c>hb_bit_storage</c>.</summary>
    /// <param name="value">The value.</param>
    /// <param name="acc">The bits counted so far.</param>
    /// <returns>The number of significant bits.</returns>
    [<TailCall>]
    let rec private bitStorage (value: uint32) (acc: int) : int =
        match value <= 0u with
        | true -> acc
        | false -> bitStorage (value >>> 1) (acc + 1)

    /// <summary>Computes the bits a feature needs, <c>hb_min (HB_OT_MAP_MAX_BITS, hb_bit_storage (max_value))</c>.</summary>
    /// <param name="info">The feature.</param>
    /// <returns>The bit count, zero for a global value-1 feature.</returns>
    let private bitNeeded (info: FeatureInfo) : int =
        match (info.InfoFlags &&& FeatureFlags.Global <> 0u) && info.MaxValue = 1u with
        | true -> 0
        | false -> min MaxBits (bitStorage info.MaxValue 0)

    /// <summary>Merges a duplicate feature into the kept one, <c>compile</c>'s duplicate branch.</summary>
    /// <param name="acc">The kept feature.</param>
    /// <param name="x">The duplicate being merged.</param>
    /// <returns>The merged feature.</returns>
    let private mergeInfo (acc: FeatureInfo) (x: FeatureInfo) : FeatureInfo =
        let flags =
            match x.InfoFlags &&& FeatureFlags.Global with
            | 0u -> acc.InfoFlags &&& ~~~FeatureFlags.Global
            | _ -> acc.InfoFlags ||| FeatureFlags.Global
        let maxValue =
            match x.InfoFlags &&& FeatureFlags.Global with
            | 0u -> max acc.MaxValue x.MaxValue
            | _ -> x.MaxValue
        let defaultValue =
            match x.InfoFlags &&& FeatureFlags.Global with
            | 0u -> acc.DefaultValue
            | _ -> x.DefaultValue
        { acc with
            InfoFlags = flags ||| (x.InfoFlags &&& FeatureFlags.HasFallback)
            MaxValue = maxValue
            DefaultValue = defaultValue
            GsubStage = min acc.GsubStage x.GsubStage
            GposStage = min acc.GposStage x.GposStage }

    /// <summary>Folds one group of equal-tag features into its merged representative.</summary>
    /// <param name="features">The sorted features.</param>
    /// <param name="index">The next feature to merge.</param>
    /// <param name="acc">The representative so far.</param>
    /// <returns>The merged feature and the next group's start index.</returns>
    [<TailCall>]
    let rec private mergeFrom (features: ReadOnlySpan<FeatureInfo>) (index: int) (acc: FeatureInfo) : struct (FeatureInfo * int) =
        match index < features.Length && features[index].InfoTag = acc.InfoTag with
        | false -> struct (acc, index)
        | true -> mergeFrom features (index + 1) (mergeInfo acc features[index])

    /// <summary>Counts the distinct feature tags of a sorted feature list.</summary>
    /// <param name="features">The sorted features.</param>
    /// <param name="index">The next feature.</param>
    /// <param name="count">The groups counted so far.</param>
    /// <returns>The number of groups.</returns>
    [<TailCall>]
    let rec private countGroups (features: ReadOnlySpan<FeatureInfo>) (index: int) (count: int) : int =
        match index >= features.Length with
        | true -> count
        | false ->
            let newGroup = index = 0 || features[index].InfoTag <> features[index - 1].InfoTag
            countGroups features (index + 1) (match newGroup with | true -> count + 1 | false -> count)

    /// <summary>The merge folder that folds each equal-tag group into one feature.</summary>
    [<Struct>]
    type private MergeFolder =
        interface ISpanUnfolder<FeatureInfo, int, FeatureInfo> with
            member this.Next (source, state) =
                mergeFrom source (state + 1) source[state]

    /// <summary>Compares two feature indices by tag then sequence, <c>feature_info_t::cmp</c>.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FeatureInfoComparer =
        { Features: ImmutableArray<FeatureInfo> }
        interface IIndexComparer with
            member this.Compare (a, b) =
                let fa = this.Features[a]
                let fb = this.Features[b]
                match uint32 fa.InfoTag = uint32 fb.InfoTag with
                | true ->
                    match fa.Seq < fb.Seq with
                    | true -> -1
                    | false -> 1
                | false ->
                    match uint32 fa.InfoTag < uint32 fb.InfoTag with
                    | true -> -1
                    | false -> 1

    /// <summary>Sorts features by tag then sequence, <c>feature_infos.qsort</c>.</summary>
    /// <param name="features">The features.</param>
    /// <returns>The sorted features.</returns>
    let private sortFeatures (features: ImmutableArray<FeatureInfo>) : ImmutableArray<FeatureInfo> =
        let permutation = Sealed.sortInPlaceIntro features.Length { FeatureInfoComparer.Features = features }
        Sealed.init features.Length (fun index -> features[int permutation[index]])

    /// <summary>Sorts features and merges duplicates, <c>compile</c>'s sort-and-merge step.</summary>
    /// <param name="features">The features.</param>
    /// <param name="isSimple">Whether the features stay in insertion order.</param>
    /// <returns>The merged features.</returns>
    let private mergeFeatures (features: ImmutableArray<FeatureInfo>) (isSimple: bool) : ImmutableArray<FeatureInfo> =
        match features.IsEmpty with
        | true -> features
        | false ->
            let sorted =
                match isSimple with
                | true -> features
                | false -> sortFeatures features
            let span = sorted.AsSpan()
            let groupCount = countGroups span 0 0
            Sealed.unfoldSpan span groupCount 0 (MergeFolder())

    /// <summary>Binary-searches a feature index map, <c>hb_map_t::has</c>.</summary>
    /// <param name="pairs">The sorted tag and index pairs.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The feature index, or none.</returns>
    [<TailCall>]
    let rec private bsearchFeatureIndex (pairs: ReadOnlySpan<struct (Tag * int)>) (tag: Tag) (lo: int) (hi: int) : int voption =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            let struct (key, index) = pairs[mid]
            match uint32 key > uint32 tag, uint32 key < uint32 tag with
            | true, _ -> bsearchFeatureIndex pairs tag lo (mid - 1)
            | false, true -> bsearchFeatureIndex pairs tag (mid + 1) hi
            | false, false -> ValueSome index

    /// <summary>Looks up a feature index in a feature index map, or the no-feature index.</summary>
    /// <param name="pairs">The sorted tag and index pairs.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The feature index.</returns>
    let private mapFeatureIndex (pairs: ImmutableArray<struct (Tag * int)>) (tag: Tag) : uint32 =
        let span = pairs.AsSpan()
        match bsearchFeatureIndex span tag 0 (span.Length - 1) with
        | ValueSome index -> uint32 index
        | ValueNone -> NoFeatureIndexValue

    /// <summary>Finds a feature's indices in both tables, with the global-search fallback.</summary>
    /// <param name="face">The face.</param>
    /// <param name="gsubMap">The GSUB feature index map.</param>
    /// <param name="gposMap">The GPOS feature index map.</param>
    /// <param name="info">The feature.</param>
    /// <returns>Whether found, the GSUB index and the GPOS index.</returns>
    let private featureIndices
            (face: OtFace)
            (gsubMap: ImmutableArray<struct (Tag * int)>)
            (gposMap: ImmutableArray<struct (Tag * int)>)
            (info: FeatureInfo)
            : struct (bool * uint32 * uint32) =
        let gsubIndex = mapFeatureIndex gsubMap info.InfoTag
        let gposIndex = mapFeatureIndex gposMap info.InfoTag
        let found = gsubIndex <> NoFeatureIndexValue || gposIndex <> NoFeatureIndexValue
        match found || (info.InfoFlags &&& FeatureFlags.GlobalSearch = 0u) with
        | true -> struct (found, gsubIndex, gposIndex)
        | false ->
            let struct (foundGsub, gsubIndex) =
                match LayoutTable.findFeature face.Gsub.Value info.InfoTag with
                | ValueSome index -> struct (true, uint32 index)
                | ValueNone -> struct (false, NoFeatureIndexValue)
            let struct (foundGpos, gposIndex) =
                match LayoutTable.findFeature face.Gpos.Value info.InfoTag with
                | ValueSome index -> struct (true, uint32 index)
                | ValueNone -> struct (false, NoFeatureIndexValue)
            struct (foundGsub || foundGpos, gsubIndex, gposIndex)

    /// <summary>The running bit-allocation state.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private BitState =
        { SourceIndex: int
          NextBit: int
          GlobalMask: uint32 }

    /// <summary>One step of the bit-allocation walk.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private BitStep =
        | BitSkip of state: BitState
        | BitEmit of featureMap: FeatureMap * state: BitState

    /// <summary>Builds the feature map of an emitted feature and advances the bit allocation.</summary>
    /// <param name="state">The bit-allocation state.</param>
    /// <param name="info">The feature.</param>
    /// <param name="bitsNeeded">The bit count.</param>
    /// <param name="found">Whether the feature exists in a table.</param>
    /// <param name="gsubIndex">The GSUB feature index.</param>
    /// <param name="gposIndex">The GPOS feature index.</param>
    /// <returns>The feature map and the next state.</returns>
    let private buildFeatureMap
            (state: BitState)
            (info: FeatureInfo)
            (bitsNeeded: int)
            (found: bool)
            (gsubIndex: uint32)
            (gposIndex: uint32)
            : FeatureMap * BitState =
        let globalBit = (info.InfoFlags &&& FeatureFlags.Global <> 0u) && info.MaxValue = 1u
        let shift = match globalBit with | true -> GlobalBitShift | false -> state.NextBit
        let mask =
            match globalBit with
            | true -> GlobalBitMask
            | false -> (1u <<< (state.NextBit + bitsNeeded)) - (1u <<< state.NextBit)
        let featureMap =
            { Tag = info.InfoTag
              GsubIndex = gsubIndex
              GposIndex = gposIndex
              GsubStage = info.GsubStage
              GposStage = info.GposStage
              Shift = shift
              Mask = mask
              Mask1 = (1u <<< shift) &&& mask
              NeedsFallback = not found
              AutoZwnj = (info.InfoFlags &&& FeatureFlags.ManualZwnj) = 0u
              AutoZwj = (info.InfoFlags &&& FeatureFlags.ManualZwj) = 0u
              Random = (info.InfoFlags &&& FeatureFlags.Random) <> 0u
              PerSyllable = (info.InfoFlags &&& FeatureFlags.PerSyllable) <> 0u }
        let nextBit = match globalBit with | true -> state.NextBit | false -> state.NextBit + bitsNeeded
        let globalMask =
            match globalBit with
            | true -> state.GlobalMask
            | false -> state.GlobalMask ||| ((info.DefaultValue <<< state.NextBit) &&& mask)
        (featureMap, { SourceIndex = state.SourceIndex + 1; NextBit = nextBit; GlobalMask = globalMask })

    /// <summary>Classifies the current feature of the bit-allocation walk.</summary>
    /// <param name="features">The merged features.</param>
    /// <param name="gsubMap">The GSUB feature index map.</param>
    /// <param name="gposMap">The GPOS feature index map.</param>
    /// <param name="face">The face.</param>
    /// <param name="state">The bit-allocation state.</param>
    /// <returns>Whether the feature is skipped or emitted.</returns>
    let private bitStep
            (features: ReadOnlySpan<FeatureInfo>)
            (gsubMap: ImmutableArray<struct (Tag * int)>)
            (gposMap: ImmutableArray<struct (Tag * int)>)
            (face: OtFace)
            (state: BitState)
            : BitStep =
        let info = features[state.SourceIndex]
        let bitsNeeded = bitNeeded info
        match info.MaxValue = 0u || state.NextBit + bitsNeeded >= GlobalBitShift with
        | true -> BitSkip { state with SourceIndex = state.SourceIndex + 1 }
        | false ->
            let struct (found, gsubIndex, gposIndex) = featureIndices face gsubMap gposMap info
            match (not found) && (info.InfoFlags &&& FeatureFlags.HasFallback = 0u) with
            | true -> BitSkip { state with SourceIndex = state.SourceIndex + 1 }
            | false -> BitEmit (buildFeatureMap state info bitsNeeded found gsubIndex gposIndex)

    /// <summary>Finds and builds the next emitted feature map, skipping disabled features.</summary>
    /// <param name="features">The merged features.</param>
    /// <param name="gsubMap">The GSUB feature index map.</param>
    /// <param name="gposMap">The GPOS feature index map.</param>
    /// <param name="face">The face.</param>
    /// <param name="state">The bit-allocation state.</param>
    /// <returns>The feature map and the next state.</returns>
    [<TailCall>]
    let rec private advance
            (features: ReadOnlySpan<FeatureInfo>)
            (gsubMap: ImmutableArray<struct (Tag * int)>)
            (gposMap: ImmutableArray<struct (Tag * int)>)
            (face: OtFace)
            (state: BitState)
            : struct (FeatureMap * BitState) =
        match bitStep features gsubMap gposMap face state with
        | BitEmit (featureMap, next) -> struct (featureMap, next)
        | BitSkip next -> advance features gsubMap gposMap face next

    /// <summary>Counts the emitted features and threads the global mask.</summary>
    /// <param name="features">The merged features.</param>
    /// <param name="gsubMap">The GSUB feature index map.</param>
    /// <param name="gposMap">The GPOS feature index map.</param>
    /// <param name="face">The face.</param>
    /// <param name="state">The bit-allocation state.</param>
    /// <param name="count">The emitted count so far.</param>
    /// <returns>The emitted count and the final global mask.</returns>
    [<TailCall>]
    let rec private countEmitted
            (features: ReadOnlySpan<FeatureInfo>)
            (gsubMap: ImmutableArray<struct (Tag * int)>)
            (gposMap: ImmutableArray<struct (Tag * int)>)
            (face: OtFace)
            (state: BitState)
            (count: int)
            : struct (int * uint32) =
        match state.SourceIndex >= features.Length with
        | true -> struct (count, state.GlobalMask)
        | false ->
            match bitStep features gsubMap gposMap face state with
            | BitEmit (_, next) -> countEmitted features gsubMap gposMap face next (count + 1)
            | BitSkip next -> countEmitted features gsubMap gposMap face next count

    /// <summary>The bit-allocation folder that emits one feature map per step.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private BitAllocFolder =
        { GsubMap: ImmutableArray<struct (Tag * int)>
          GposMap: ImmutableArray<struct (Tag * int)>
          Face: OtFace }
        interface ISpanUnfolder<FeatureInfo, BitState, FeatureMap> with
            member this.Next (source, state) =
                advance source this.GsubMap this.GposMap this.Face state

    /// <summary>Compares two feature maps by tag, <c>feature_map_t::cmp</c>.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FeatureMapComparer =
        { Features: ImmutableArray<FeatureMap> }
        interface IIndexComparer with
            member this.Compare (a, b) =
                let ta = uint32 this.Features[a].Tag
                let tb = uint32 this.Features[b].Tag
                match ta < tb, ta > tb with
                | true, _ -> -1
                | false, true -> 1
                | false, false -> 0

    /// <summary>Sorts the compiled feature maps by tag, <c>m.features.qsort</c>.</summary>
    /// <param name="featureMaps">The feature maps.</param>
    /// <returns>The sorted feature maps.</returns>
    let private sortFeatureMaps (featureMaps: ImmutableArray<FeatureMap>) : ImmutableArray<FeatureMap> =
        let permutation = Sealed.sortInPlaceIntro featureMaps.Length { FeatureMapComparer.Features = featureMaps }
        Sealed.init featureMaps.Length (fun index -> featureMaps[int permutation[index]])

    /// <summary>Allocates the feature bits and builds the compiled feature maps.</summary>
    /// <param name="face">The face.</param>
    /// <param name="gsubMap">The GSUB feature index map.</param>
    /// <param name="gposMap">The GPOS feature index map.</param>
    /// <param name="merged">The merged features.</param>
    /// <param name="isSimple">Whether the feature maps are sorted by tag.</param>
    /// <returns>The feature maps and the global mask.</returns>
    let private allocateFeatures
            (face: OtFace)
            (gsubMap: ImmutableArray<struct (Tag * int)>)
            (gposMap: ImmutableArray<struct (Tag * int)>)
            (merged: ImmutableArray<FeatureInfo>)
            (isSimple: bool)
            : struct (ImmutableArray<FeatureMap> * uint32) =
        let span = merged.AsSpan()
        let initial = { SourceIndex = 0; NextBit = NextBitStart; GlobalMask = GlobalBitMask }
        let struct (emittedCount, globalMask) = countEmitted span gsubMap gposMap face initial 0
        let featureMaps = Sealed.unfoldSpan span emittedCount initial { BitAllocFolder.GsubMap = gsubMap; GposMap = gposMap; Face = face }
        let featureMaps =
            match isSimple with
            | true -> sortFeatureMaps featureMaps
            | false -> featureMaps
        struct (featureMaps, globalMask)

    /// <summary>Maps one feature's lookup indices to lookup-map entries, skipping indices past the table.</summary>
    /// <param name="instancer">The feature-variation instancer at the font's coordinates.</param>
    /// <param name="face">The face.</param>
    /// <param name="tableTag">The table tag.</param>
    /// <param name="featureIndex">The feature index.</param>
    /// <param name="mask">The lookup mask.</param>
    /// <param name="autoZwnj">Whether ZWNJ is skipped.</param>
    /// <param name="autoZwj">Whether ZWJ is skipped.</param>
    /// <param name="random">Whether the lookup picks alternates at random.</param>
    /// <param name="perSyllable">Whether the lookup is contained to a syllable.</param>
    /// <param name="featureTag">The feature tag recorded on the lookup.</param>
    /// <returns>The lookup-map entries.</returns>
    let private featureLookupMaps
            (instancer: Instancer)
            (face: OtFace)
            (tableTag: Tag)
            (featureIndex: uint32)
            (mask: uint32)
            (autoZwnj: bool)
            (autoZwj: bool)
            (random: bool)
            (perSyllable: bool)
            (featureTag: Tag)
            : ImmutableArray<LookupMap> =
        let lookupCount = LayoutApi.lookupCount face tableTag
        let indices =
            match tableTag = Tags.Gsub with
            | true -> FeatureVariations.featureLookupsFor face.Gsub.Value instancer (int featureIndex)
            | false -> FeatureVariations.featureLookupsFor face.Gpos.Value instancer (int featureIndex)
        Sealed.choose
            (fun index ->
                match int index < lookupCount with
                | true ->
                    ValueSome
                        { LookupIndex = int index
                          AutoZwnj = autoZwnj
                          AutoZwj = autoZwj
                          Random = random
                          PerSyllable = perSyllable
                          Mask = mask
                          FeatureTag = featureTag }
                | false -> ValueNone)
            (indices.AsSpan())

    /// <summary>Collects the lookup entries of one stage from every feature at that stage.</summary>
    /// <param name="instancer">The feature-variation instancer at the font's coordinates.</param>
    /// <param name="face">The face.</param>
    /// <param name="tableTag">The table tag.</param>
    /// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
    /// <param name="featureMaps">The compiled feature maps.</param>
    /// <param name="index">The next feature map.</param>
    /// <param name="stage">The stage being collected.</param>
    /// <param name="acc">The collected lookups so far.</param>
    /// <returns>The collected lookups.</returns>
    [<TailCall>]
    let rec private collectFeatureLookups
            (instancer: Instancer)
            (face: OtFace)
            (tableTag: Tag)
            (tableIndex: int)
            (featureMaps: ImmutableArray<FeatureMap>)
            (index: int)
            (stage: int)
            (acc: ImmutableArray<LookupMap>)
            : ImmutableArray<LookupMap> =
        match index >= featureMaps.Length with
        | true -> acc
        | false ->
            let feature = featureMaps[index]
            let featureStage = match tableIndex with | 0 -> feature.GsubStage | _ -> feature.GposStage
            match featureStage = stage with
            | false -> collectFeatureLookups instancer face tableTag tableIndex featureMaps (index + 1) stage acc
            | true ->
                let featureIndex = match tableIndex with | 0 -> feature.GsubIndex | _ -> feature.GposIndex
                let maps =
                    featureLookupMaps instancer face tableTag featureIndex feature.Mask feature.AutoZwnj feature.AutoZwj feature.Random feature.PerSyllable feature.Tag
                collectFeatureLookups instancer face tableTag tableIndex featureMaps (index + 1) stage (Sealed.appendSpan (acc.AsSpan()) (maps.AsSpan()))

    /// <summary>Collects the unsorted lookup entries of one stage, required feature first.</summary>
    /// <param name="instancer">The feature-variation instancer at the font's coordinates.</param>
    /// <param name="face">The face.</param>
    /// <param name="tableTag">The table tag.</param>
    /// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
    /// <param name="featureMaps">The compiled feature maps.</param>
    /// <param name="requiredFeatureIndex">The required feature index, or the no-feature index.</param>
    /// <param name="requiredFeatureStage">The stage the required feature belongs to.</param>
    /// <param name="stage">The stage being collected.</param>
    /// <returns>The collected lookups.</returns>
    let private collectStageLookups
            (instancer: Instancer)
            (face: OtFace)
            (tableTag: Tag)
            (tableIndex: int)
            (featureMaps: ImmutableArray<FeatureMap>)
            (requiredFeatureIndex: uint32)
            (requiredFeatureStage: int)
            (stage: int)
            : ImmutableArray<LookupMap> =
        let required =
            match requiredFeatureIndex <> NoFeatureIndexValue && requiredFeatureStage = stage with
            | false -> ImmutableArray.Empty
            | true -> featureLookupMaps instancer face tableTag requiredFeatureIndex GlobalBitMask true true false false Tags.Space
        collectFeatureLookups instancer face tableTag tableIndex featureMaps 0 stage required

    /// <summary>Compares two lookup maps by lookup index, <c>lookup_map_t::cmp</c>.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LookupMapComparer =
        { Lookups: ImmutableArray<LookupMap> }
        interface IIndexComparer with
            member this.Compare (a, b) =
                let la = this.Lookups[a].LookupIndex
                let lb = this.Lookups[b].LookupIndex
                match la < lb, la > lb with
                | true, _ -> -1
                | false, true -> 1
                | false, false -> 0

    /// <summary>Folds one group of equal-index lookups into its merged representative.</summary>
    /// <param name="sorted">The sorted lookups.</param>
    /// <param name="index">The next lookup to merge.</param>
    /// <param name="acc">The representative so far.</param>
    /// <returns>The merged lookup and the next group's start index.</returns>
    [<TailCall>]
    let rec private mergeLookupFrom (sorted: ReadOnlySpan<LookupMap>) (index: int) (acc: LookupMap) : struct (LookupMap * int) =
        match index < sorted.Length && sorted[index].LookupIndex = acc.LookupIndex with
        | false -> struct (acc, index)
        | true ->
            let merged =
                { acc with
                    Mask = acc.Mask ||| sorted[index].Mask
                    AutoZwnj = acc.AutoZwnj && sorted[index].AutoZwnj
                    AutoZwj = acc.AutoZwj && sorted[index].AutoZwj }
            mergeLookupFrom sorted (index + 1) merged

    /// <summary>Counts the distinct lookup indices of a sorted lookup list.</summary>
    /// <param name="sorted">The sorted lookups.</param>
    /// <param name="index">The next lookup.</param>
    /// <param name="count">The groups counted so far.</param>
    /// <returns>The number of groups.</returns>
    [<TailCall>]
    let rec private countLookupGroups (sorted: ReadOnlySpan<LookupMap>) (index: int) (count: int) : int =
        match index >= sorted.Length with
        | true -> count
        | false ->
            let newGroup = index = 0 || sorted[index].LookupIndex <> sorted[index - 1].LookupIndex
            countLookupGroups sorted (index + 1) (match newGroup with | true -> count + 1 | false -> count)

    /// <summary>The merge folder that folds each equal-index group into one lookup.</summary>
    [<Struct>]
    type private LookupMergeFolder =
        interface ISpanUnfolder<LookupMap, int, LookupMap> with
            member this.Next (source, state) =
                mergeLookupFrom source (state + 1) source[state]

    /// <summary>Sorts a stage's lookups by index and merges duplicates, <c>compile</c>'s lookup merge.</summary>
    /// <param name="lookups">The lookups.</param>
    /// <returns>The sorted, merged lookups.</returns>
    let private sortMergeLookups (lookups: ImmutableArray<LookupMap>) : ImmutableArray<LookupMap> =
        match lookups.Length with
        | length when length < 2 -> lookups
        | _ ->
            let permutation = Sealed.sortInPlaceIntro lookups.Length { LookupMapComparer.Lookups = lookups }
            let sorted = Sealed.init lookups.Length (fun index -> lookups[int permutation[index]])
            let span = sorted.AsSpan()
            let groupCount = countLookupGroups span 0 0
            Sealed.unfoldSpan span groupCount 0 (LookupMergeFolder())

    /// <summary>Finds the stage of a feature tag, or stage zero when it is not a feature.</summary>
    /// <param name="features">The merged features.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
    /// <param name="index">The next feature.</param>
    /// <returns>The feature's stage in the table.</returns>
    [<TailCall>]
    let rec private requiredStageAt (features: ReadOnlySpan<FeatureInfo>) (tag: Tag) (tableIndex: int) (index: int) : int =
        match index >= features.Length with
        | true -> 0
        | false ->
            match features[index].InfoTag = tag with
            | true -> match tableIndex with | 0 -> features[index].GsubStage | _ -> features[index].GposStage
            | false -> requiredStageAt features tag tableIndex (index + 1)

    /// <summary>Walks the stages of one table, building its lookups and stage maps.</summary>
    /// <param name="instancer">The feature-variation instancer at the font's coordinates.</param>
    /// <param name="face">The face.</param>
    /// <param name="tableTag">The table tag.</param>
    /// <param name="tableIndex">0 for GSUB, 1 for GPOS.</param>
    /// <param name="featureMaps">The compiled feature maps.</param>
    /// <param name="requiredFeatureIndex">The required feature index, or the no-feature index.</param>
    /// <param name="requiredFeatureStage">The stage the required feature belongs to.</param>
    /// <param name="pauses">The pauses of the table.</param>
    /// <param name="stage">The next stage.</param>
    /// <param name="stageCount">The number of stages.</param>
    /// <param name="lookups">The lookups collected so far.</param>
    /// <param name="stageMaps">The stage maps collected so far.</param>
    /// <returns>The lookups and the stage maps.</returns>
    [<TailCall>]
    let rec private walkStages
            (instancer: Instancer)
            (face: OtFace)
            (tableTag: Tag)
            (tableIndex: int)
            (featureMaps: ImmutableArray<FeatureMap>)
            (requiredFeatureIndex: uint32)
            (requiredFeatureStage: int)
            (pauses: ImmutableArray<StageInfo>)
            (stage: int)
            (stageCount: int)
            (lookups: ImmutableArray<LookupMap>)
            (stageMaps: ImmutableArray<StageMap>)
            : struct (ImmutableArray<LookupMap> * ImmutableArray<StageMap>) =
        match stage >= stageCount with
        | true -> struct (lookups, stageMaps)
        | false ->
            let stageLookups = collectStageLookups instancer face tableTag tableIndex featureMaps requiredFeatureIndex requiredFeatureStage stage |> sortMergeLookups
            let lookups = Sealed.appendSpan (lookups.AsSpan()) (stageLookups.AsSpan())
            let stageMaps = Sealed.appendOne (stageMaps.AsSpan()) { LastLookup = lookups.Length; Pause = pauses[stage].StagePause }
            walkStages instancer face tableTag tableIndex featureMaps requiredFeatureIndex requiredFeatureStage pauses (stage + 1) stageCount lookups stageMaps

    /// <summary>Whether some stage ends in a pause that runs a function.</summary>
    /// <param name="stages">The compiled stages.</param>
    /// <param name="index">The next stage.</param>
    /// <returns>True when a stage has such a pause.</returns>
    [<TailCall>]
    let rec private anyFunctionPause (stages: ImmutableArray<StageMap>) index =
        match index >= stages.Length with
        | true -> false
        | false ->
            match PauseKind.runsFunction stages[index].Pause with
            | true -> true
            | false -> anyFunctionPause stages (index + 1)

    /// <summary>Compiles the map, <c>hb_ot_map_builder_t::compile</c>.</summary>
    /// <param name="coords">The font's normalized variation coordinates.</param>
    /// <param name="gdef">The font's GDEF store instance for those coordinates.</param>
    /// <param name="face">The face.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="builder">The builder with its features collected.</param>
    /// <returns>The compiled OpenType map.</returns>
    let compile (coords: NormalizedCoords) (gdef: StoreInstance) (face: OtFace) (props: SegmentProperties) (builder: MapBuilder) : OtMap =
        let instancer = { Gdef = gdef; Coords = coords }
        let struct (scriptTags, languageTags) =
            OtTag.tagsFromScriptAndLanguage props.Script props.Language MaxTagsPerScript MaxTagsPerLanguage
        let struct (foundGsub, gsubScriptIndex, chosenGsub) = LayoutApi.selectScript face Tags.Gsub scriptTags
        let struct (_, gsubLanguageIndex, _) = LayoutApi.selectLanguage face Tags.Gsub gsubScriptIndex languageTags
        let struct (foundGpos, gposScriptIndex, chosenGpos) = LayoutApi.selectScript face Tags.Gpos scriptTags
        let struct (_, gposLanguageIndex, _) = LayoutApi.selectLanguage face Tags.Gpos gposScriptIndex languageTags
        let struct (_, requiredGsubIndex, requiredGsubTag) = LayoutApi.requiredFeature face Tags.Gsub gsubScriptIndex gsubLanguageIndex
        let struct (_, requiredGposIndex, requiredGposTag) = LayoutApi.requiredFeature face Tags.Gpos gposScriptIndex gposLanguageIndex
        let merged = mergeFeatures builder.Features builder.IsSimple
        let gsubLangSys = LayoutTable.langSys face.Gsub.Value gsubScriptIndex gsubLanguageIndex
        let gposLangSys = LayoutTable.langSys face.Gpos.Value gposScriptIndex gposLanguageIndex
        let gsubFeatureMap = LayoutTable.featureIndexMap face.Gsub.Value gsubLangSys
        let gposFeatureMap = LayoutTable.featureIndexMap face.Gpos.Value gposLangSys
        let struct (featureMaps, globalMask) = allocateFeatures face gsubFeatureMap gposFeatureMap merged builder.IsSimple
        let requiredGsubStage = requiredStageAt (merged.AsSpan()) requiredGsubTag 0 0
        let requiredGposStage = requiredStageAt (merged.AsSpan()) requiredGposTag 1 0
        let builder = addPause builder 0 NoPause
        let builder = addPause builder 1 NoPause
        let struct (gsubLookups, gsubStages) =
            walkStages instancer face Tags.Gsub 0 featureMaps requiredGsubIndex requiredGsubStage builder.GsubPauses 0 builder.GsubStage ImmutableArray.Empty ImmutableArray.Empty
        let struct (gposLookups, gposStages) =
            walkStages instancer face Tags.Gpos 1 featureMaps requiredGposIndex requiredGposStage builder.GposPauses 0 builder.GposStage ImmutableArray.Empty ImmutableArray.Empty
        { GlobalMask = globalMask
          Features = featureMaps
          GsubLookups = gsubLookups
          GposLookups = gposLookups
          GsubStages = gsubStages
          GposStages = gposStages
          HasPauses = anyFunctionPause gsubStages 0
          GsubViews = Memo<obj>()
          ChosenGsubScript = chosenGsub
          ChosenGposScript = chosenGpos
          FoundGsubScript = foundGsub
          FoundGposScript = foundGpos }

    /// <summary>Selects the GSUB script the compiled map will choose, <c>hb_ot_map_builder_t</c>'s constructor
    /// (src/hb-ot-map.cc:46-82): the shaper is categorized from it before any feature is collected,
    /// <c>hb_ot_shape_planner_t</c>'s constructor (src/hb-ot-shape.cc:88).</summary>
    /// <param name="face">The face.</param>
    /// <param name="props">The segment properties.</param>
    /// <returns>The chosen GSUB script tag.</returns>
    let chosenGsubScript (face: OtFace) (props: SegmentProperties) =
        let struct (scriptTags, _) =
            OtTag.tagsFromScriptAndLanguage props.Script props.Language MaxTagsPerScript MaxTagsPerLanguage
        let struct (_, _, chosen) = LayoutApi.selectScript face Tags.Gsub scriptTags
        chosen

    /// <summary>Finds the stage of a feature among the compiled features, <c>features.bsearch</c>.</summary>
    /// <param name="features">The compiled features.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="index">The next feature index.</param>
    /// <returns>The GSUB stage, or none when the map has no such feature.</returns>
    [<TailCall>]
    let rec private stageOfFeature (features: ImmutableArray<FeatureMap>) (tag: Tag) index : int voption =
        match index >= features.Length with
        | true -> ValueNone
        | false ->
            match features[index].Tag = tag with
            | true -> ValueSome features[index].GsubStage
            | false -> stageOfFeature features tag (index + 1)

    /// <summary>Returns the GSUB stage of a feature of the compiled map, <c>get_feature_stage</c>
    /// (src/hb-ot-map.hh:146-150).</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The stage, or none when the map has no such feature.</returns>
    let featureStage (map: OtMap) (tag: Tag) =
        stageOfFeature map.Features tag 0

    /// <summary>Returns the indices of the GSUB lookups of the stage of a feature, <c>get_stage_lookups</c> of
    /// <c>get_feature_stage</c> (src/hb-ot-map.hh:152-161): the lookups the stage holds, the other features of the same stage
    /// included. A feature the map does not have has no lookups.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The lookup indices, in the order the map applies them.</returns>
    let stageLookups (map: OtMap) (tag: Tag) =
        match featureStage map tag with
        | ValueNone -> ImmutableArray.Empty
        | ValueSome stage ->
            let first = match stage with | 0 -> 0 | _ -> map.GsubStages[stage - 1].LastLookup
            let last = match stage < map.GsubStages.Length with | true -> map.GsubStages[stage].LastLookup | false -> map.GsubLookups.Length
            Sealed.init (last - first) (fun index -> map.GsubLookups[first + index].LookupIndex)

    /// <summary>Finds a feature among the compiled features, <c>features.bsearch</c>.</summary>
    /// <param name="features">The compiled features.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="index">The next feature index.</param>
    /// <returns>The feature, or none when the map has no such feature.</returns>
    [<TailCall>]
    let rec private compiledFeature (features: ImmutableArray<FeatureMap>) (tag: Tag) index : FeatureMap voption =
        match index >= features.Length with
        | true -> ValueNone
        | false ->
            match features[index].Tag = tag with
            | true -> ValueSome features[index]
            | false -> compiledFeature features tag (index + 1)

    /// <summary>Returns the value-1 mask of a feature of the compiled map, <c>get_1_mask</c>.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>The mask, zero when the map has no such feature.</returns>
    let featureMask1 (map: OtMap) (tag: Tag) =
        match compiledFeature map.Features tag 0 with
        | ValueSome feature -> feature.Mask1
        | ValueNone -> 0u

    /// <summary>Whether a feature of the compiled map has no lookup in the font and so needs its fallback, <c>needs_fallback</c>
    /// (src/hb-ot-map.hh:140-144).</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>True when the map has the feature and the font does not.</returns>
    let needsFallback (map: OtMap) (tag: Tag) =
        match compiledFeature map.Features tag 0 with
        | ValueSome feature -> feature.NeedsFallback
        | ValueNone -> false

    /// <summary>Whether a language system lists a feature.</summary>
    /// <param name="table">The table.</param>
    /// <param name="langSys">The language system.</param>
    /// <param name="tag">The feature tag.</param>
    /// <param name="index">The next feature of the language system.</param>
    /// <returns>True when the language system lists the feature.</returns>
    [<TailCall>]
    let rec private langSysHasFeature (table: LayoutTable<'S>) (langSys: LangSys) (tag: Tag) index =
        match index >= langSys.FeatureIndices.Length with
        | true -> false
        | false ->
            match LayoutTable.featureTag table (int langSys.FeatureIndices[index]) = tag with
            | true -> true
            | false -> langSysHasFeature table langSys tag (index + 1)

    /// <summary>Whether the font has a feature for the script and language of the segment in either table,
    /// <c>hb_ot_map_builder_t::has_feature</c> (src/hb-ot-map.cc:112-125).</summary>
    /// <param name="face">The face.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="tag">The feature tag.</param>
    /// <returns>True when the chosen language system of GSUB or GPOS lists the feature.</returns>
    let hasFeature (face: OtFace) (props: SegmentProperties) (tag: Tag) =
        let struct (scriptTags, languageTags) =
            OtTag.tagsFromScriptAndLanguage props.Script props.Language MaxTagsPerScript MaxTagsPerLanguage
        let struct (_, gsubScriptIndex, _) = LayoutApi.selectScript face Tags.Gsub scriptTags
        let struct (_, gsubLanguageIndex, _) = LayoutApi.selectLanguage face Tags.Gsub gsubScriptIndex languageTags
        let struct (_, gposScriptIndex, _) = LayoutApi.selectScript face Tags.Gpos scriptTags
        let struct (_, gposLanguageIndex, _) = LayoutApi.selectLanguage face Tags.Gpos gposScriptIndex languageTags
        langSysHasFeature face.Gsub.Value (LayoutTable.langSys face.Gsub.Value gsubScriptIndex gsubLanguageIndex) tag 0
        || langSysHasFeature face.Gpos.Value (LayoutTable.langSys face.Gpos.Value gposScriptIndex gposLanguageIndex) tag 0
