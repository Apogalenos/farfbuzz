namespace Farfbuzz.Var

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.OpenType

/// <summary>The variation tables of one face: fvar, avar and the four item-variation stores.</summary>
[<NoEquality; NoComparison>]
type internal FaceVariations =
    { Fvar: Fvar
      Avar: Avar
      Hvar: MetricsVariations
      Vvar: MetricsVariations
      Mvar: Mvar
      Gdef: Lazy<ItemVariationStore>  // read only when a font has non-zero coordinates
      Cff2: Lazy<ItemVariationStore> }  // read from the face's CFF2 accelerator when a glyph's charstring first blends

/// <summary>Building the variation tables of a face.</summary>
[<RequireQualifiedAccess>]
module internal FaceVariations =
    /// <summary>Loads fvar, avar, the HVAR/VVAR metrics variations and the MVAR table, and the GDEF variation store and the CFF2
    /// variation store (the store of the face's CFF2 accelerator, which the accelerator's first use parses) on demand.</summary>
    /// <param name="face">The face.</param>
    /// <param name="cff2">The face's CFF2 accelerator, not yet forced.</param>
    /// <returns>The face's variation tables.</returns>
    let loadWith (face: Face) (cff2: Lazy<Cff2Accel>) =
        { Fvar = Fvar.load face
          Avar = Avar.load face
          Hvar = Hvar.loadHvar face
          Vvar = Hvar.loadVvar face
          Mvar = Mvar.load face
          Gdef = lazy (Gdef.loadVarStore face)
          Cff2 = lazy cff2.Value.Store }

    /// <summary>Loads fvar, avar, the HVAR/VVAR metrics variations, the MVAR table and the GDEF variation store.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The face's variation tables.</returns>
    let load (face: Face) =
        loadWith face (lazy (Cff2.load face))

/// <summary>The variation state of one font instance: the coordinates, the named instance and the store scalars.</summary>
[<NoEquality; NoComparison>]
type internal FontVariations =
    { Coords: NormalizedCoords
      DesignCoords: ImmutableArray<float32>
      NamedInstance: uint32
      Hvar: StoreInstance
      Vvar: StoreInstance
      Mvar: StoreInstance
      Gdef: StoreInstance
      Cff2: Lazy<StoreInstance> }  // worked out when a CFF2 charstring first blends: the face's store at these coordinates

/// <summary>Building the variation state of a font, <c>hb_font_set_variations</c> and its siblings.</summary>
[<RequireQualifiedAccess>]
module internal FontVariations =
    /// <summary>The sentinel meaning no named instance, <c>HB_FONT_NO_VAR_NAMED_INSTANCE</c>.</summary>
    [<Literal>]
    let noNamedInstance = 0xFFFFFFFFu

    /// <summary>The store instance of a font without non-zero coordinates: no scalars.</summary>
    let private noScalars =
        { Store = ItemVariationStore.empty
          Scalars = ImmutableArray<float32>.Empty }

    /// <summary>No variation state: no coordinates, the no-instance sentinel and empty stores.</summary>
    let none =
        { Coords = NormalizedCoords.empty
          DesignCoords = ImmutableArray<float32>.Empty
          NamedInstance = noNamedInstance
          Hvar = noScalars
          Vvar = noScalars
          Mvar = noScalars
          Gdef = noScalars
          Cff2 = lazy noScalars }

    /// <summary>Computes the non-zero flag and, only then, the four store instances, <c>_hb_font_adopt_var_coords</c>.</summary>
    /// <param name="face">The face's variation tables.</param>
    /// <param name="coords">The sealed 2.14 coordinates.</param>
    /// <param name="design">The design coordinates, NaN after a normalized set.</param>
    /// <param name="namedInstance">The named instance, unchanged by adoption.</param>
    /// <returns>The font variation state.</returns>
    let private adopt (face: FaceVariations) (coords: ImmutableArray<int>) (design: ImmutableArray<float32>) (namedInstance: uint32) =
        let normalized = NormalizedCoords.ofSealed coords
        match normalized.HasNonZero with
        | false ->
            { Coords = normalized
              DesignCoords = design
              NamedInstance = namedInstance
              Hvar = noScalars
              Vvar = noScalars
              Mvar = noScalars
              Gdef = noScalars
              Cff2 = lazy { Store = face.Cff2.Value; Scalars = ImmutableArray<float32>.Empty } }
        | true ->
            { Coords = normalized
              DesignCoords = design
              NamedInstance = namedInstance
              Hvar = ItemVariationStore.instantiate normalized face.Hvar.Store
              Vvar = ItemVariationStore.instantiate normalized face.Vvar.Store
              Mvar = ItemVariationStore.instantiate normalized face.Mvar.Store
              Gdef = ItemVariationStore.instantiate normalized face.Gdef.Value
              Cff2 = lazy (ItemVariationStore.instantiate normalized face.Cff2.Value) }

    /// <summary>The design coordinates a named instance selects, or the fvar defaults when none or out of range.</summary>
    /// <param name="face">The face's variation tables.</param>
    /// <param name="namedInstance">The named instance.</param>
    /// <returns>The base design coordinates, one per axis.</returns>
    let private namedInstanceDesign (face: FaceVariations) (namedInstance: uint32) =
        let fvar = face.Fvar
        let defaults = Sealed.init fvar.Axes.Length (fun index -> fvar.Axes[index].DefaultValue)
        match namedInstance < uint32 fvar.InstanceCount with
        | false -> defaults
        | true -> Fvar.instanceCoords (int namedInstance) fvar

    /// <summary>Finds the last variation whose tag equals an axis tag, <c>hb_font_set_variations</c>' inline overwrite.</summary>
    /// <param name="variations">The variations, in order.</param>
    /// <param name="tag">The axis tag.</param>
    /// <param name="index">The next variation.</param>
    /// <param name="found">The last matching value so far.</param>
    /// <returns>The last matching value, or none.</returns>
    [<TailCall>]
    let rec private lastVariationForAxis (variations: ReadOnlySpan<Variation>) (tag: Tag) (index: int) (found: float32 voption) : float32 voption =
        match index >= variations.Length with
        | true -> found
        | false ->
            let next =
                match variations[index].Tag = tag with
                | true -> ValueSome variations[index].Value
                | false -> found
            lastVariationForAxis variations tag (index + 1) next

    /// <summary>Builds the state from a variation list, <c>hb_font_set_variations</c>.</summary>
    /// <param name="face">The face's variation tables.</param>
    /// <param name="namedInstance">The named instance selecting the base coordinates.</param>
    /// <param name="variations">The variations, in order; later entries overwrite earlier ones.</param>
    /// <returns>The font variation state.</returns>
    let ofVariations (face: FaceVariations) (namedInstance: uint32) (variations: ImmutableArray<Variation>) =
        let fvar = face.Fvar
        let axisCount = fvar.Axes.Length
        let baseDesign = namedInstanceDesign face namedInstance
        let design =
            Sealed.init axisCount (fun axis ->
                match lastVariationForAxis (variations.AsSpan()) fvar.Axes[axis].Tag 0 ValueNone with
                | ValueSome value -> value
                | ValueNone -> baseDesign[axis])
        let normalized = NormalizeCoords.normalizeCoords fvar face.Avar (design.AsSpan())
        adopt face normalized design namedInstance

    /// <summary>Builds the state from one variation, starting from the current design coordinates, <c>hb_font_set_variation</c>.</summary>
    /// <param name="face">The face's variation tables.</param>
    /// <param name="namedInstance">The named instance.</param>
    /// <param name="currentDesign">The current design coordinates; reused when their length equals the axis count.</param>
    /// <param name="tag">The axis tag to set.</param>
    /// <param name="value">The value.</param>
    /// <returns>The font variation state.</returns>
    let ofVariation (face: FaceVariations) (namedInstance: uint32) (currentDesign: ImmutableArray<float32>) (tag: Tag) (value: float32) =
        let fvar = face.Fvar
        let axisCount = fvar.Axes.Length
        let baseDesign =
            match currentDesign.Length = axisCount with
            | true -> currentDesign
            | false -> namedInstanceDesign face namedInstance
        let design =
            Sealed.init axisCount (fun axis ->
                match fvar.Axes[axis].Tag = tag with
                | true -> value
                | false -> baseDesign[axis])
        let normalized = NormalizeCoords.normalizeCoords fvar face.Avar (design.AsSpan())
        adopt face normalized design namedInstance

    /// <summary>Builds the state from design coordinates, padding with the defaults, <c>hb_font_set_var_coords_design</c>.</summary>
    /// <param name="face">The face's variation tables.</param>
    /// <param name="namedInstance">The named instance.</param>
    /// <param name="design">The design coordinates; the first axes are taken, the rest defaulted.</param>
    /// <returns>The font variation state.</returns>
    let ofDesign (face: FaceVariations) (namedInstance: uint32) (design: ImmutableArray<float32>) =
        let fvar = face.Fvar
        let axisCount = fvar.Axes.Length
        let input = min design.Length axisCount
        let coords =
            Sealed.init axisCount (fun axis ->
                match axis < input with
                | true -> design[axis]
                | false -> fvar.Axes[axis].DefaultValue)
        let normalized = NormalizeCoords.normalizeCoords fvar face.Avar (coords.AsSpan())
        adopt face normalized coords namedInstance

    /// <summary>Builds the state from normalized coordinates, unclamped, <c>hb_font_set_var_coords_normalized</c>.</summary>
    /// <param name="face">The face's variation tables.</param>
    /// <param name="namedInstance">The named instance.</param>
    /// <param name="normalized">The normalized 2.14 coordinates; the first axes are taken, the rest zeroed.</param>
    /// <returns>The font variation state.</returns>
    let ofNormalized (face: FaceVariations) (namedInstance: uint32) (normalized: ImmutableArray<int>) =
        let axisCount = face.Fvar.Axes.Length
        let input = min normalized.Length axisCount
        let coords =
            Sealed.init axisCount (fun axis ->
                match axis < input with
                | true -> normalized[axis]
                | false -> 0)
        let design = Sealed.init axisCount (fun _ -> Single.NaN)
        adopt face coords design namedInstance
