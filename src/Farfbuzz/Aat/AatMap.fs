namespace Farfbuzz.Aat

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>One requested AAT feature: its type, setting, exclusivity and the order it was added.</summary>
[<Struct; NoComparison; NoEquality>]
type internal FeatureInfo =
    { Type: uint16
      Setting: uint16
      IsExclusive: bool
      Seq: int }

/// <summary>Building the AAT map from a face's feat, ltag and morx or mort chains.</summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal AatMap =
    /// <summary>The map-compilation budget, <c>HB_BUDGET_AAT_MAP</c>.</summary>
    [<Literal>]
    let private BudgetAatMap = 1L <<< 24

    /// <summary>The feature type <c>CHARACTER_ALTERNATIVES</c>, <c>aalt</c>'s type.</summary>
    [<Literal>]
    let private FeatureTypeCharacterAlternatives = 17us

    /// <summary>The feature type <c>LETTER_CASE</c>.</summary>
    [<Literal>]
    let private FeatureTypeLetterCase = 3us

    /// <summary>The feature type <c>LOWER_CASE</c>.</summary>
    [<Literal>]
    let private FeatureTypeLowerCase = 37us

    /// <summary>The <c>LOWER_CASE_SMALL_CAPS</c> selector.</summary>
    [<Literal>]
    let private SelectorLowerCaseSmallCaps = 1us

    /// <summary>One requested feature range, <c>feature_range_t</c>.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private FeatureRange =
        { Info: FeatureInfo
          Start: uint32
          End: uint32 }

    /// <summary>One compiled cluster range, <c>RangeStep</c>.</summary>
    [<Struct; NoComparison; NoEquality>]
    type private RangeStep =
        { First: uint32
          Last: uint32
          ActiveCount: int
          Pairs: ImmutableArray<struct (uint16 * uint16)> }

    /// <summary>Builds the requested range from a feature info.</summary>
    /// <param name="info">The feature info.</param>
    /// <param name="feature">The user feature.</param>
    /// <returns>The range.</returns>
    let private rangeOf (info: FeatureInfo) (feature: Feature) : FeatureRange =
        { Info = info
          Start = feature.Start
          End = feature.End }

    /// <summary>The aalt range, or none when the feat table does not expose character alternatives.</summary>
    /// <param name="feat">The feat table.</param>
    /// <param name="feature">The user feature.</param>
    /// <returns>The range.</returns>
    let private aaltFeature (feat: Feat) (feature: Feature) : FeatureRange voption =
        match Feat.exposesFeature feat FeatureTypeCharacterAlternatives with
        | false -> ValueNone
        | true ->
            ValueSome (rangeOf { Type = FeatureTypeCharacterAlternatives; Setting = uint16 feature.Value; IsExclusive = true; Seq = 0 } feature)

    /// <summary>The mapped range, with the small-caps fallback to letter case.</summary>
    /// <param name="feat">The feat table.</param>
    /// <param name="mapping">The feature mapping.</param>
    /// <param name="feature">The user feature.</param>
    /// <returns>The range.</returns>
    let private mappedFeature (feat: Feat) (mapping: FeatureMapping) (feature: Feature) : FeatureRange voption =
        let setting = if feature.Value <> 0u then mapping.SelectorToEnable else mapping.SelectorToDisable
        let name = Feat.feature feat mapping.AatFeatureType
        match FeatureName.hasData name with
        | true -> ValueSome (rangeOf { Type = mapping.AatFeatureType; Setting = setting; IsExclusive = FeatureName.isExclusive name; Seq = 0 } feature)
        | false ->
            match mapping.AatFeatureType = FeatureTypeLowerCase && mapping.SelectorToEnable = SelectorLowerCaseSmallCaps with
            | false -> ValueNone
            | true ->
                let fallback = Feat.feature feat FeatureTypeLetterCase
                match FeatureName.hasData fallback with
                | false -> ValueNone
                | true -> ValueSome (rangeOf { Type = mapping.AatFeatureType; Setting = setting; IsExclusive = FeatureName.isExclusive fallback; Seq = 0 } feature)

    /// <summary>Adds one user feature, <c>hb_aat_map_builder_t::add_feature</c>.</summary>
    /// <param name="feat">The feat table.</param>
    /// <param name="feature">The user feature.</param>
    /// <returns>The requested range, or none when the feature is ignored.</returns>
    let private addFeature (feat: Feat) (feature: Feature) : FeatureRange voption =
        match Feat.hasData feat with
        | false -> ValueNone
        | true ->
            match feature.Tag = Tags.Aalt with
            | true -> aaltFeature feat feature
            | false ->
                match AatFeatureMappings.find feature.Tag with
                | ValueNone -> ValueNone
                | ValueSome mapping -> mappedFeature feat mapping feature

    /// <summary>Adds every user feature, numbering the kept ones in order, <c>add_feature</c> over each.</summary>
    /// <param name="feat">The feat table.</param>
    /// <param name="features">The user features.</param>
    /// <returns>The requested ranges.</returns>
    let private addFeatures (feat: Feat) (features: ReadOnlySpan<Feature>) : ImmutableArray<FeatureRange> =
        let kept = Sealed.choose (addFeature feat) features
        Sealed.mapi (fun index range -> { range with Info = { range.Info with Seq = index + 1 } }) (kept.AsSpan())

    /// <summary>Compares two feature infos, <c>feature_info_t::cmp</c> (the static comparator).</summary>
    /// <param name="a">The first.</param>
    /// <param name="b">The second.</param>
    /// <returns>A negative, zero or positive number.</returns>
    let private compareFeatureInfo (a: FeatureInfo) (b: FeatureInfo) : int =
        match a.Type <> b.Type with
        | true -> if a.Type < b.Type then -1 else 1
        | false ->
            match (not a.IsExclusive) && (a.Setting &&& 0xFFFEus) <> (b.Setting &&& 0xFFFEus) with
            | true -> if a.Setting < b.Setting then -1 else 1
            | false -> compare a.Seq b.Seq

    /// <summary>The index comparer over feature infos.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FeatureComparer =
        { Features: ImmutableArray<FeatureInfo> }
        interface IIndexComparer with
            member this.Compare (a, b) = compareFeatureInfo this.Features[a] this.Features[b]

    /// <summary>The index comparer over uint32 boundary values.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private BoundaryComparer =
        { Values: ImmutableArray<uint32> }
        interface IIndexComparer with
            member this.Compare (a, b) = compare this.Values[a] this.Values[b]

    /// <summary>Sorts the feature infos with the static comparator, <c>qsort</c>.</summary>
    /// <param name="features">The features.</param>
    /// <returns>The sorted features.</returns>
    let private sortFeatures (features: ImmutableArray<FeatureInfo>) : ImmutableArray<FeatureInfo> =
        let order = Sealed.sortInPlaceIntro features.Length { FeatureComparer.Features = features }
        Sealed.init features.Length (fun index -> features[int order[index]])

    /// <summary>Deduplicates the sorted feature infos, keeping the earliest <c>seq</c> per setting.</summary>
    /// <param name="features">The sorted features.</param>
    /// <param name="index">The next candidate.</param>
    /// <param name="kept">The last kept index.</param>
    /// <param name="builder">The kept features.</param>
    /// <returns>The deduplicated features.</returns>
    [<TailCall>]
    let rec private dedupFrom (features: ImmutableArray<FeatureInfo>) (index: int) (kept: int) (builder: PointBuilder<FeatureInfo>) : ImmutableArray<FeatureInfo> =
        match index >= features.Length with
        | true -> PointBuilder.publish builder
        | false ->
            let current = features[index]
            let last = features[kept]
            let duplicate = current.Type = last.Type && (current.IsExclusive || (current.Setting &&& 0xFFFEus) = (last.Setting &&& 0xFFFEus))
            match duplicate with
            | true -> dedupFrom features (index + 1) kept builder
            | false -> dedupFrom features (index + 1) index (PointBuilder.append builder current)

    /// <summary>Deduplicates the sorted feature infos, <c>current_features</c>'s merge loop.</summary>
    /// <param name="features">The sorted features.</param>
    /// <returns>The deduplicated features.</returns>
    let private dedupFeatures (features: ImmutableArray<FeatureInfo>) : ImmutableArray<FeatureInfo> =
        match features.Length with
        | 0 -> features
        | _ -> dedupFrom features 1 0 (PointBuilder.append (PointBuilder.create()) features[0])

    /// <summary>Projects the deduplicated features to their type and setting pairs.</summary>
    /// <param name="features">The features.</param>
    /// <returns>The pairs.</returns>
    let private pairsOf (features: ImmutableArray<FeatureInfo>) : ImmutableArray<struct (uint16 * uint16)> =
        Sealed.map (fun feature -> struct (feature.Type, feature.Setting)) (features.AsSpan())

    /// <summary>Deduplicates sorted boundary values.</summary>
    /// <param name="sorted">The sorted values.</param>
    /// <param name="index">The next candidate.</param>
    /// <param name="last">The last kept value.</param>
    /// <param name="builder">The kept values.</param>
    /// <returns>The deduplicated values.</returns>
    [<TailCall>]
    let rec private dedupBoundariesFrom (sorted: ImmutableArray<uint32>) (index: int) (last: uint32) (builder: PointBuilder<uint32>) : ImmutableArray<uint32> =
        match index >= sorted.Length with
        | true -> PointBuilder.publish builder
        | false ->
            let value = sorted[index]
            match value = last with
            | true -> dedupBoundariesFrom sorted (index + 1) last builder
            | false -> dedupBoundariesFrom sorted (index + 1) value (PointBuilder.append builder value)

    /// <summary>The sorted distinct cluster boundaries of the ranges, 0 and the end included.</summary>
    /// <param name="ranges">The feature ranges.</param>
    /// <returns>The boundaries.</returns>
    let private boundariesOf (ranges: ImmutableArray<FeatureRange>) : ImmutableArray<uint32> =
        let count = ranges.Length
        let values =
            Sealed.init (2 * count + 2) (fun index ->
                match index with
                | 0 -> 0u
                | index when index <= count -> ranges[index - 1].Start
                | index when index <= 2 * count -> ranges[index - count - 1].End
                | _ -> UInt32.MaxValue)
        let order = Sealed.sortInPlaceIntro values.Length { BoundaryComparer.Values = values }
        let sorted = Sealed.init values.Length (fun index -> values[int order[index]])
        dedupBoundariesFrom sorted 1 sorted[0] (PointBuilder.append (PointBuilder.create()) sorted[0])

    /// <summary>The active features covering a range, sorted, deduplicated and projected to pairs.</summary>
    /// <param name="ranges">The feature ranges.</param>
    /// <param name="first">The range start.</param>
    /// <param name="last">The range end.</param>
    /// <returns>The pairs and the covering count.</returns>
    let private activePairsFor (ranges: ImmutableArray<FeatureRange>) (first: uint32) (last: uint32) : struct (ImmutableArray<struct (uint16 * uint16)> * int) =
        let covering =
            Sealed.choose (fun range -> if range.Start <= first && range.End > last then ValueSome range.Info else ValueNone) (ranges.AsSpan())
        let deduped = dedupFeatures (sortFeatures covering)
        struct (pairsOf deduped, covering.Length)

    /// <summary>Builds every range step between the boundaries.</summary>
    /// <param name="ranges">The feature ranges.</param>
    /// <param name="boundaries">The sorted boundaries.</param>
    /// <param name="index">The next boundary.</param>
    /// <param name="builder">The steps.</param>
    /// <returns>The steps.</returns>
    [<TailCall>]
    let rec private buildSteps
        (ranges: ImmutableArray<FeatureRange>) (boundaries: ImmutableArray<uint32>) (index: int) (builder: PointBuilder<RangeStep>) : ImmutableArray<RangeStep> =
        match index >= boundaries.Length - 1 with
        | true -> PointBuilder.publish builder
        | false ->
            let first = boundaries[index]
            let last = boundaries[index + 1] - 1u
            let struct (pairs, activeCount) = activePairsFor ranges first last
            buildSteps ranges boundaries (index + 1)
                (PointBuilder.append builder { First = first; Last = last; ActiveCount = activeCount; Pairs = pairs })

    /// <summary>Merges one chain's new range into its list, extending the last equal range.</summary>
    /// <param name="existing">The chain's ranges.</param>
    /// <param name="flags">The new flags.</param>
    /// <param name="first">The range start.</param>
    /// <param name="last">The range end.</param>
    /// <returns>The ranges and whether a new range was pushed.</returns>
    let private mergeChain (existing: ImmutableArray<RangeFlags>) (flags: uint32) (first: uint32) (last: uint32) : struct (ImmutableArray<RangeFlags> * bool) =
        match existing.Length > 0 && existing[existing.Length - 1].Flags = flags with
        | true -> struct (Sealed.setItem (existing.Length - 1) { existing[existing.Length - 1] with ClusterLast = last } existing, false)
        | false -> struct (Sealed.appendOne (existing.AsSpan()) { Flags = flags; ClusterFirst = first; ClusterLast = last }, true)

    /// <summary>The budget a range charges for visiting every chain and its feature records,
    /// <c>mortmorx::compile_flags</c> through <c>Chain::compile_flags</c>.</summary>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="chainIndex">The next chain.</param>
    /// <param name="charge">The charge so far.</param>
    /// <returns>The charge of all chains.</returns>
    [<TailCall>]
    let rec private chainsCharge (table: Morx) (chainIndex: int) (charge: int64) =
        match chainIndex >= table.Chains.Length with
        | true -> charge
        | false -> chainsCharge table (chainIndex + 1) (charge + int64 (1 + table.Chains[chainIndex].Features.Length))

    /// <summary>The number of chains that stored a new range, each charged <c>HB_BUDGET_16</c>.</summary>
    /// <param name="before">The chains before the range.</param>
    /// <param name="after">The chains after the range.</param>
    /// <param name="chainIndex">The next chain.</param>
    /// <param name="pushed">The count so far.</param>
    /// <returns>The number of chains that grew.</returns>
    [<TailCall>]
    let rec private pushedChains
        (before: ImmutableArray<ImmutableArray<RangeFlags>>) (after: ImmutableArray<ImmutableArray<RangeFlags>>) (chainIndex: int) (pushed: int) =
        match chainIndex >= before.Length with
        | true -> pushed
        | false ->
            match after[chainIndex].Length > before[chainIndex].Length with
            | true -> pushedChains before after (chainIndex + 1) (pushed + 1)
            | false -> pushedChains before after (chainIndex + 1) pushed

    /// <summary>Compiles every chain's flags for one range, merging and spending the budget, <c>mortmorx::compile_flags</c>.
    /// The chains are rebuilt in one pass: upstream pushes in place, and copying the whole chain array per chain would pay
    /// the square of the chain count for every range.</summary>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="ltag">The ltag table.</param>
    /// <param name="language">The segment language.</param>
    /// <param name="chainFlags">The chains compiled so far.</param>
    /// <param name="step">The range.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The chains and the budget after the per-chain charges.</returns>
    let private compileChains
        (table: Morx) (ltag: Ltag) (language: Language) (chainFlags: ImmutableArray<ImmutableArray<RangeFlags>>) (step: RangeStep)
        (budget: int64) =
        let merged =
            Sealed.mapi
                (fun chainIndex existing ->
                    let flags = Morx.compileFlags table chainIndex step.Pairs ltag language
                    let struct (ranges, _) = mergeChain existing flags step.First step.Last
                    ranges)
                (chainFlags.AsSpan())
        let charged = budget - chainsCharge table 0 0L - 16L * int64 (pushedChains chainFlags merged 0 0)
        struct (merged, charged)

    /// <summary>Extends every chain's last range to the global end, the final adjustment of <c>compile</c>.</summary>
    /// <param name="chainFlags">The chains.</param>
    /// <returns>The chains with the last ranges extended.</returns>
    let private finalizeFlags (chainFlags: ImmutableArray<ImmutableArray<RangeFlags>>) : ImmutableArray<ImmutableArray<RangeFlags>> =
        Sealed.map
            (fun (ranges: ImmutableArray<RangeFlags>) ->
                match ranges.Length with
                | 0 -> ranges
                | count -> Sealed.setItem (count - 1) { ranges[count - 1] with ClusterLast = UInt32.MaxValue } ranges)
            (chainFlags.AsSpan())

    /// <summary>Compiles the chains over every range step, stopping when the budget is exhausted.</summary>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="ltag">The ltag table.</param>
    /// <param name="language">The segment language.</param>
    /// <param name="steps">The range steps.</param>
    /// <param name="stepIndex">The next step.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <param name="chainFlags">The chains compiled so far.</param>
    /// <returns>The compiled chains.</returns>
    [<TailCall>]
    let rec private compileRanges
        (table: Morx) (ltag: Ltag) (language: Language) (steps: ImmutableArray<RangeStep>) (stepIndex: int)
        (budget: int64) (chainFlags: ImmutableArray<ImmutableArray<RangeFlags>>) : ImmutableArray<ImmutableArray<RangeFlags>> =
        match stepIndex >= steps.Length with
        | true -> finalizeFlags chainFlags
        | false ->
            let step = steps[stepIndex]
            let struct (nextFlags, budgetAfterCompile) = compileChains table ltag language chainFlags step budget
            let budgetAfter = budgetAfterCompile - int64 step.ActiveCount
            match budgetAfter < 0L with
            | true -> finalizeFlags nextFlags
            | false -> compileRanges table ltag language steps (stepIndex + 1) budgetAfter nextFlags

    /// <summary>Compiles the default map, one global range per chain, <c>compile</c> without features.</summary>
    /// <param name="table">The morx or mort table.</param>
    /// <param name="ltag">The ltag table.</param>
    /// <param name="language">The segment language.</param>
    /// <returns>The map.</returns>
    let private compileDefaults (table: Morx) (ltag: Ltag) (language: Language) : AatMap =
        match Morx.hasData table with
        | false -> { ChainFlags = ImmutableArray.Empty }
        | true ->
            let chainFlags =
                Sealed.init table.Chains.Length (fun chainIndex ->
                    let flags = Morx.compileFlags table chainIndex ImmutableArray<struct (uint16 * uint16)>.Empty ltag language
                    Sealed.init 1 (fun _ -> { Flags = flags; ClusterFirst = 0u; ClusterLast = UInt32.MaxValue }))
            { ChainFlags = chainFlags }

    /// <summary>The morx table when it has data, else the mort table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The table to compile.</returns>
    let private tableOf (face: OtFace) : Morx =
        match Morx.hasData face.Morx.Value with
        | true -> face.Morx.Value
        | false -> face.Mort.Value

    /// <summary>The default AAT map, no user features, <c>hb_aat_map_builder_t::compile</c> with none: the one the table compiled when
    /// no chain reads the language, else compiled for the segment's language.</summary>
    /// <param name="face">The face.</param>
    /// <param name="segment">The segment properties.</param>
    /// <returns>The map.</returns>
    let defaults (face: OtFace) (segment: SegmentProperties) : AatMap =
        let table = tableOf face
        match table.DefaultMap.Value with
        | ValueSome compiled -> compiled
        | ValueNone -> compileDefaults table face.Ltag.Value segment.Language

    /// <summary>Builds the AAT map for user features, <c>hb_aat_map_builder_t::add_feature</c> and <c>compile</c>.</summary>
    /// <param name="face">The face.</param>
    /// <param name="segment">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <returns>The map.</returns>
    let build (face: OtFace) (segment: SegmentProperties) (features: ReadOnlySpan<Feature>) : AatMap =
        let ranges = addFeatures face.Feat.Value features
        match ranges.Length = 0 with
        | true -> defaults face segment
        | false ->
            let table = tableOf face
            match Morx.hasData table with
            | false -> { ChainFlags = ImmutableArray.Empty }
            | true ->
                let boundaries = boundariesOf ranges
                let steps = buildSteps ranges boundaries 0 (PointBuilder.create())
                let chainFlags = Sealed.init table.Chains.Length (fun _ -> ImmutableArray.Empty)
                { ChainFlags = compileRanges table face.Ltag.Value segment.Language steps 0 BudgetAatMap chainFlags }
