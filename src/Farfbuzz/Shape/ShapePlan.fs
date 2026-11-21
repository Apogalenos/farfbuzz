namespace Farfbuzz

open System
open System.Collections.Generic
open System.Collections.Immutable
open Farfbuzz.Var

/// <summary>Element-wise comparison and hashing of a plan key's features, by tag, value and global-ness only.</summary>
module internal ShapePlanKeyImpl =
    /// <summary>Whether two feature lists match, <c>hb_shape_plan_key_t::user_features_match</c>.</summary>
    /// <param name="features">This key's features.</param>
    /// <param name="other">The other key's features.</param>
    /// <param name="index">The next feature.</param>
    /// <returns>True when every feature matches by tag, value and global-ness.</returns>
    [<TailCall>]
    let rec matchFeatures (features: ImmutableArray<Feature>) (other: ImmutableArray<Feature>) (index: int) : bool =
        match index >= features.Length with
        | true -> true
        | false ->
            let a = features[index]
            let b = other[index]
            let aGlobal = a.Start = Feature.GlobalStart && a.End = Feature.GlobalEnd
            let bGlobal = b.Start = Feature.GlobalStart && b.End = Feature.GlobalEnd
            match a.Tag = b.Tag && a.Value = b.Value && aGlobal = bGlobal with
            | true -> matchFeatures features other (index + 1)
            | false -> false

    /// <summary>Mixes one feature into a hash, ignoring its range.</summary>
    /// <param name="features">The features.</param>
    /// <param name="index">The next feature.</param>
    /// <param name="hash">The hash so far.</param>
    /// <returns>The mixed hash.</returns>
    [<TailCall>]
    let rec hashFeatures (features: ImmutableArray<Feature>) (index: int) (hash: int) : int =
        match index >= features.Length with
        | true -> hash
        | false ->
            let feature = features[index]
            let isGlobal = feature.Start = Feature.GlobalStart && feature.End = Feature.GlobalEnd
            hashFeatures features (index + 1) (hash * 31 + int (uint32 feature.Tag) + int feature.Value + (if isGlobal then 1 else 0))

    /// <summary>Whether two condition lists match, <c>hb_vector_t::operator==</c>.</summary>
    /// <param name="conditions">This key's conditions.</param>
    /// <param name="other">The other key's conditions.</param>
    /// <param name="index">The next condition.</param>
    /// <returns>True when every condition is equal.</returns>
    [<TailCall>]
    let rec matchConditions (conditions: ImmutableArray<uint32>) (other: ImmutableArray<uint32>) (index: int) : bool =
        match index >= conditions.Length with
        | true -> true
        | false ->
            match conditions[index] = other[index] with
            | true -> matchConditions conditions other (index + 1)
            | false -> false

    /// <summary>Mixes an unsigned value into a hash, folding its halves so the checked int conversion cannot overflow.</summary>
    /// <param name="hash">The hash so far.</param>
    /// <param name="value">The value.</param>
    /// <returns>The mixed hash.</returns>
    let mixUint32 (hash: int) (value: uint32) : int =
        hash * 31 + int (value >>> 16) + int (value &&& 0xFFFFu)

    /// <summary>Mixes one condition into a hash.</summary>
    /// <param name="conditions">The conditions.</param>
    /// <param name="index">The next condition.</param>
    /// <param name="hash">The hash so far.</param>
    /// <returns>The mixed hash.</returns>
    [<TailCall>]
    let rec hashConditions (conditions: ImmutableArray<uint32>) (index: int) (hash: int) : int =
        match index >= conditions.Length with
        | true -> hash
        | false ->
            let value = conditions[index]
            hashConditions conditions (index + 1) (mixUint32 hash value)

/// <summary>The reusable identity of a shape plan, <c>hb_shape_plan_key_t</c>: the segment properties, the user
/// features compared by tag, value and global-ness only, the ot feature-variation state and the chosen shaper.</summary>
[<Struct; CustomEquality; NoComparison>]
type internal ShapePlanKey =
    { Props: SegmentProperties
      Features: ImmutableArray<Feature>
      GsubVariationsIndex: uint32
      GposVariationsIndex: uint32
      GsubLookupConditions: ImmutableArray<uint32>
      GposLookupConditions: ImmutableArray<uint32>
      Shaper: string }

    /// <summary>Whether this key equals another, <c>hb_shape_plan_key_t::equal</c>.</summary>
    /// <param name="other">The other key.</param>
    /// <returns>True when every compared part is equal.</returns>
    member private this.EqualsKey(other: ShapePlanKey) : bool =
        this.Shaper = other.Shaper
        && this.Props = other.Props
        && this.GsubVariationsIndex = other.GsubVariationsIndex
        && this.GposVariationsIndex = other.GposVariationsIndex
        && this.Features.Length = other.Features.Length
        && this.GsubLookupConditions.Length = other.GsubLookupConditions.Length
        && this.GposLookupConditions.Length = other.GposLookupConditions.Length
        && ShapePlanKeyImpl.matchFeatures this.Features other.Features 0
        && ShapePlanKeyImpl.matchConditions this.GsubLookupConditions other.GsubLookupConditions 0
        && ShapePlanKeyImpl.matchConditions this.GposLookupConditions other.GposLookupConditions 0

    /// <summary>Compares this key with an object, boxing the structural comparison.</summary>
    /// <param name="other">The other object.</param>
    /// <returns>True when the other object is an equal key.</returns>
    override this.Equals(other: obj) : bool =
        match other with
        | :? ShapePlanKey as other -> this.EqualsKey other
        | _ -> false

    /// <summary>Hashes the key, consistent with its equality.</summary>
    /// <returns>The hash.</returns>
    override this.GetHashCode() : int =
        let features = ShapePlanKeyImpl.hashFeatures this.Features 0 17
        let gsub = ShapePlanKeyImpl.hashConditions this.GsubLookupConditions 0 features
        let gpos = ShapePlanKeyImpl.hashConditions this.GposLookupConditions 0 gsub
        let withGsubIndex = ShapePlanKeyImpl.mixUint32 gpos this.GsubVariationsIndex
        let withGposIndex = ShapePlanKeyImpl.mixUint32 withGsubIndex this.GposVariationsIndex
        withGposIndex * 31 + this.Shaper.GetHashCode() * 31 + this.Props.GetHashCode()

    interface IEquatable<ShapePlanKey> with
        member this.Equals(other: ShapePlanKey) = this.EqualsKey other

/// <summary>One cached shape plan and the key that finds it again, <c>hb_face_t::plan_node_t</c>.</summary>
[<Struct>]
type internal PlanEntry =
    { Key: ShapePlanKey
      Plan: ShapePlan }

/// <summary>An immutable cache of shape plans keyed by their reusable identity: the value <c>hb_shape</c> threads
/// through in place of the per-face plan list of <c>hb_shape_plan_create_cached2</c>
/// (src/hb-shape-plan.cc:552-608).</summary>
[<Struct>]
type PlanCache =
    internal
        { Entries: ImmutableArray<PlanEntry> }

/// <summary>Plan-cache lookup and insertion, the value form of <c>hb_shape_plan_create_cached2</c>'s list.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PlanCache =
    /// <summary>The box-free plan-key comparison, the default comparer over <see cref="ShapePlanKey"/>'s
    /// <see cref="IEquatable{ShapePlanKey}"/> implementation.</summary>
    let private keyComparer = EqualityComparer<ShapePlanKey>.Default

    /// <summary>Finds a cached plan by key, scanning the entries in insertion order.</summary>
    /// <param name="entries">The cached entries.</param>
    /// <param name="key">The key to find.</param>
    /// <param name="index">The next entry.</param>
    /// <returns>The plan, or none when no entry matches.</returns>
    [<TailCall>]
    let rec private find (entries: ImmutableArray<PlanEntry>) (key: ShapePlanKey) (index: int) : ShapePlan voption =
        match index >= entries.Length with
        | true -> ValueNone
        | false ->
            match keyComparer.Equals(entries[index].Key, key) with
            | true -> ValueSome entries[index].Plan
            | false -> find entries key (index + 1)

    /// <summary>The empty cache.</summary>
    let empty : PlanCache =
        { Entries = ImmutableArray<PlanEntry>.Empty }

    /// <summary>Returns the plan cached under a key, none when absent.</summary>
    /// <param name="cache">The cache.</param>
    /// <param name="key">The key.</param>
    /// <returns>The plan, or none.</returns>
    let internal tryFind (cache: PlanCache) (key: ShapePlanKey) =
        find cache.Entries key 0

    /// <summary>Adds a plan to the cache; later entries are found before earlier ones.</summary>
    /// <param name="cache">The cache.</param>
    /// <param name="key">The key.</param>
    /// <param name="plan">The plan.</param>
    /// <returns>The cache with the plan appended.</returns>
    let internal add (cache: PlanCache) (key: ShapePlanKey) (plan: ShapePlan) =
        { Entries = Sealed.appendOne (cache.Entries.AsSpan()) { Key = key; Plan = plan } }

/// <summary>Plan keys and construction, <c>hb_shape_plan_create</c> and its key.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShapePlan =
    /// <summary>Builds a plan key from a font and its inputs, <c>hb_shape_plan_key_t::init</c>.</summary>
    /// <param name="font">The font, whose face tables and variation state derive the key.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shaper">The chosen shaper name.</param>
    /// <returns>The key, or none when the feature-variation state cannot be read (unreachable in this port, which has no allocation failure).</returns>
    let internal key (font: Font) (props: SegmentProperties) (features: ReadOnlySpan<Feature>) (shaper: string) =
        let instancer = { Gdef = font.Variations.Gdef; Coords = font.Variations.Coords }
        let gsubFv = FeatureVariations.read font.Ot.Gsub.Value.FeatureVariations
        let gposFv = FeatureVariations.read font.Ot.Gpos.Value.FeatureVariations
        ValueSome
            { Props = props
              Features = Sealed.ofSpan features
              GsubVariationsIndex = FeatureVariations.findIndex gsubFv instancer
              GposVariationsIndex = FeatureVariations.findIndex gposFv instancer
              GsubLookupConditions = FeatureVariations.lookupConditions gsubFv instancer
              GposLookupConditions = FeatureVariations.lookupConditions gposFv instancer
              Shaper = shaper }

    /// <summary>Builds a plan, <c>hb_shape_plan_create</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <returns>The plan, or why no shaper can run.</returns>
    let create (font: Font) (props: SegmentProperties) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) : Result<ShapePlan, ShapeError> =
        Execute.createPlan font props features shapers
