namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The planner: the pure part of <c>hb_ot_shape_planner_t</c> and of its <c>compile</c>.</summary>
[<RequireQualifiedAccess>]
module internal OtPlan =
    /// <summary>Compiles a categorized shaper into a plan, <c>hb_ot_shape_planner_t</c> plus <c>compile</c>.</summary>
    /// <param name="capabilities">The face's layout capabilities.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <param name="map">The compiled OpenType map.</param>
    /// <param name="kind">The categorized shaper kind.</param>
    /// <returns>The plan.</returns>
    let compile
            (capabilities: PlanCapabilities)
            (props: SegmentProperties)
            (features: ImmutableArray<Feature>)
            (map: OtMap)
            (kind: ShaperKind)
            =
        let horizontal = Direction.isHorizontal props.Direction
        let applyMorx = capabilities.HasMorx && (horizontal || not capabilities.HasGsub)
        let scriptZeroMarks = Categorize.zeroWidthMarks kind <> ZeroWidthMarksNone
        let scriptFallbackPosition = Categorize.fallbackPosition kind
        let kind =
            match applyMorx && kind <> DefaultShaper with
            | true -> DumberShaper
            | false -> kind
        let shaper = Categorize.profile kind
        let fracMask = OtMap.mask1 map Tags.Frac
        let numrMask = OtMap.mask1 map Tags.Numr
        let dnomMask = OtMap.mask1 map Tags.Dnom
        let hasFrac = fracMask <> 0u || (numrMask <> 0u && dnomMask <> 0u)
        let rtlmMask = OtMap.mask1 map Tags.Rtlm
        let hasVert = OtMap.mask1 map Tags.Vert <> 0u
        let kernTag = if horizontal then Tags.Kern else Tags.Vkrn
        let struct (kernMask, _) = OtMap.mask map kernTag
        let requestedKerning = kernMask <> 0u
        let hasGposKern = OtMap.featureIndex map 1 kernTag <> OtMap.NoFeatureIndex
        let disableGpos = shaper.GposTag <> Tags.Empty && shaper.GposTag <> map.ChosenGposScript
        let fallbackGlyphClasses = not capabilities.HasGlyphClasses
        let hasKerx = capabilities.HasKerx
        let hasGsub = (not applyMorx) && capabilities.HasGsub
        let hasGpos = (not disableGpos) && capabilities.HasGpos
        let applyKerx1 = hasKerx && not (hasGsub && hasGpos)
        let applyGpos = (not applyKerx1) && hasGpos
        let reposition = (not applyKerx1) && (not hasGposKern || not applyGpos)
        let applyKerx = applyKerx1 || (reposition && hasKerx)
        let applyKern = reposition && (not hasKerx) && capabilities.HasKern && scriptFallbackPosition
        let applyFallbackKern = scriptFallbackPosition && not (applyGpos || applyKerx || applyKern)
        let zeroMarks = scriptZeroMarks && not applyKerx && (not applyKern || not capabilities.HasMachineKern)
        let hasGposMark = OtMap.mask1 map Tags.Mark <> 0u
        let adjustMarkPositioningWhenZeroing0 =
            (not applyGpos) && (not applyKerx) && (not applyKern || not capabilities.HasCrossKern)
        let fallbackMarkPositioning = adjustMarkPositioningWhenZeroing0 && scriptFallbackPosition
        let adjustMarkPositioningWhenZeroing =
            match applyMorx with
            | true -> false
            | false -> adjustMarkPositioningWhenZeroing0
        let applyTrak = capabilities.HasTrak && capabilities.HasStat
        { Props = props
          Shaper = shaper
          Map = map
          Data = ShaperHooks.createData kind (OtMap.mask1 map) props map
          UserFeatures = features
          FracMask = fracMask
          NumrMask = numrMask
          DnomMask = dnomMask
          RtlmMask = rtlmMask
          KernMask = kernMask
          RequestedKerning = requestedKerning
          HasFrac = hasFrac
          HasVert = hasVert
          HasGposMark = hasGposMark
          ZeroMarks = zeroMarks
          FallbackGlyphClasses = fallbackGlyphClasses
          FallbackMarkPositioning = fallbackMarkPositioning
          AdjustMarkPositioningWhenZeroing = adjustMarkPositioningWhenZeroing
          ApplyGpos = applyGpos
          ApplyKern = applyKern
          ApplyFallbackKern = applyFallbackKern
          ApplyKerx = applyKerx
          ApplyMorx = applyMorx
          ApplyTrak = applyTrak
          Unicode = ValueNone }

    /// <summary>Gathers the plan inputs and compiles them, <c>hb_ot_shape_planner_t</c>'s construction and compile.</summary>
    /// <param name="font">The font.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="features">The user features.</param>
    /// <returns>The plan.</returns>
    let create (font: Font) (props: SegmentProperties) (features: ImmutableArray<Feature>) =
        let capabilities = OtLayout.capabilities font
        let struct (map, kind) = OtLayout.compileMap font props features
        compile capabilities props features map kind
