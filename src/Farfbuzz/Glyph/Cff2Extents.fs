namespace Farfbuzz

open System
open Farfbuzz.Var

/// <summary>What CFF2 blending needs from the font; a struct, so the interpreter stays generic and allocation-free.</summary>
[<Struct>]
type internal CffBlend =
    { Blending: bool
      Store: StoreInstance
      Ivs: int }
    interface ICharStringBlend<CffBlend> with
        member _.Blending blend = blend.Blending
        member _.RegionCount (blend, ivs) = ItemVariationStore.regionIndexCount blend.Store.Store ivs
        member _.Scalar (blend, ivs, index) = ItemVariationStore.regionScalar blend.Store ivs index
        member _.Ivs blend = blend.Ivs
    interface ICffBlendSource<CffBlend> with
        member _.Blending blend = blend.Blending
        member _.RegionCount (blend, ivs) = ItemVariationStore.regionIndexCount blend.Store.Store ivs
        member _.Scalar (blend, ivs, index) = ItemVariationStore.regionScalar blend.Store ivs index

/// <summary>The CFF2 glyph extents from the charstring interpreter, blending at the font's coordinates.</summary>
[<RequireQualifiedAccess>]
module internal Cff2Extents =
    /// <summary>The Null subroutine INDEX of an out-of-range font-dict index.</summary>
    let private emptyIndex = { Start = 0; Count = 0u; OffSize = 0; CountSize = 4 }

    /// <summary>Interprets one glyph's charstring at the font's coordinates, through the reference interpreter or the bounds
    /// interpreter.</summary>
    /// <param name="fast">Whether to interpret with the bounds interpreter.</param>
    /// <param name="cff">The accelerator.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="store">The per-font store instance.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The sink state, the ending and the remaining budget, or none.</returns>
    let private interpretOne (fast: bool) (cff: Cff2Accel) (coords: NormalizedCoords) (store: StoreInstance) (glyph: uint32) (budget: int64) =
        match cff.Valid && glyph < uint32 cff.NumGlyphs with
        | false -> ValueNone
        | true ->
            let table = cff.Table.Span
            let struct (charOffset, charLength) = CffIndex.item table cff.CharStrings glyph
            let fd = Cff2.fdOf cff glyph
            let ivs = match fd < cff.PrivateDicts.Length with | true -> cff.PrivateDicts[fd].Ivs | false -> 0
            let localSubrs = match fd < cff.PrivateDicts.Length with | true -> cff.PrivateDicts[fd].LocalSubrs | false -> emptyIndex
            let blend = { Blending = coords.Length > 0 && cff.HasStore; Store = store; Ivs = ivs }
            match fast with
            | false ->
                CffCharString.interpret
                    (Cff1Extents.ExtentsSink())
                    { Cff1Extents.SinkState.PathOpen = false; Bounds = Cff1Extents.empty }
                    (CffFlavour.Cff2 blend)
                    charOffset charLength cff.GlobalSubrs localSubrs table budget
            | true ->
                let result = CffKernel.interpret blend true ivs table charOffset charLength (CffIndex.subrs cff.GlobalSubrs) (CffIndex.subrs localSubrs) budget
                match result.Valid with
                | false -> ValueNone
                | true ->
                    let bounds = { MinX = result.MinX; MinY = result.MinY; MaxX = result.MaxX; MaxY = result.MaxY }
                    ValueSome (struct ({ Cff1Extents.SinkState.PathOpen = result.PathOpen; Bounds = bounds }, Ended, result.Budget))

    /// <summary>Computes one glyph's bounds, <c>get_extents_at</c> before scaling.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="store">The per-font store instance.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The bounds and the remaining budget, or none.</returns>
    let boundsAt (cff: Cff2Accel) (coords: NormalizedCoords) (store: StoreInstance) (glyph: uint32) (budget: int64) =
        match interpretOne false cff coords store glyph budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (sink, _, remaining)) -> ValueSome (struct (sink.Bounds, remaining))

    /// <summary>Computes one glyph's bounds with the bounds interpreter, <c>get_extents_at</c> before scaling.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="store">The per-font store instance.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The bounds and the remaining budget, or none.</returns>
    let boundsAtFast (cff: Cff2Accel) (coords: NormalizedCoords) (store: StoreInstance) (glyph: uint32) (budget: int64) =
        match interpretOne true cff coords store glyph budget with
        | ValueNone -> ValueNone
        | ValueSome (struct (sink, _, remaining)) -> ValueSome (struct (sink.Bounds, remaining))

    /// <summary>The unscaled ink extents of a charstring's bounds, <c>get_extents</c> before scaling.</summary>
    /// <param name="bounds">The bounds and the remaining budget, or none.</param>
    /// <returns>The extents, or none when the charstring has no bounds.</returns>
    let private extentsOf bounds =
        match bounds with
        | ValueNone -> ValueNone
        | ValueSome (struct (bounds: CffBounds, _)) ->
            let xBearing, width =
                match bounds.MinX >= bounds.MaxX with
                | true -> 0, 0
                | false ->
                    let xBearing = Fixed.clampToInt32 (Cff1Extents.roundfHalfUp bounds.MinX)
                    let width = Fixed.clampToInt32 (Cff1Extents.roundfHalfUp bounds.MaxX - float xBearing)
                    xBearing, width
            let yBearing, height =
                match bounds.MinY >= bounds.MaxY with
                | true -> 0, 0
                | false ->
                    let yBearing = Fixed.clampToInt32 (Cff1Extents.roundfHalfUp bounds.MaxY)
                    let height = Fixed.clampToInt32 (Cff1Extents.roundfHalfUp bounds.MinY - float yBearing)
                    yBearing, height
            ValueSome { XBearing = xBearing; YBearing = yBearing; Width = width; Height = height }

    /// <summary>Computes one glyph's unscaled ink extents, <c>get_extents</c> before scaling.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="store">The per-font store instance.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the charstring has no bounds.</returns>
    let glyph (cff: Cff2Accel) (coords: NormalizedCoords) (store: StoreInstance) (glyph: uint32) =
        extentsOf (boundsAt cff coords store glyph Cff1Extents.GlyphBudget)

    /// <summary>Computes one glyph's unscaled ink extents with the bounds interpreter, <c>get_extents</c> before scaling.</summary>
    /// <param name="cff">The accelerator.</param>
    /// <param name="coords">The normalized coordinates.</param>
    /// <param name="store">The per-font store instance.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The extents, or none when the charstring has no bounds.</returns>
    let glyphFast (cff: Cff2Accel) (coords: NormalizedCoords) (store: StoreInstance) (glyph: uint32) =
        extentsOf (boundsAtFast cff coords store glyph Cff1Extents.GlyphBudget)
