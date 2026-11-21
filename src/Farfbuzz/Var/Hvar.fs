namespace Farfbuzz.OpenType

open System
open Farfbuzz
open Farfbuzz.Var

/// <summary>The HVAR or VVAR metrics-variation table: the item variation store and the advance and vertical-origin maps.</summary>
[<NoEquality; NoComparison>]
type internal MetricsVariations =
    { HasData: bool
      Store: ItemVariationStore
      AdvMap: DeltaSetIndexMap voption
      VorgMap: DeltaSetIndexMap voption }

/// <summary>The HVAR and VVAR tables, <c>OT::HVAR</c> and <c>OT::VVAR</c>.</summary>
[<RequireQualifiedAccess>]
module internal Hvar =
    /// <summary>The size of the shared HVAR header.</summary>
    [<Literal>]
    let private HeaderSize = 20

    /// <summary>The size of the VVAR table, with its vertical-origin map.</summary>
    [<Literal>]
    let private VvarSize = 24

    /// <summary>The offset of the version word.</summary>
    [<Literal>]
    let private VersionOffset = 0

    /// <summary>The offset of the var-store offset.</summary>
    [<Literal>]
    let private VarStoreOffset = 4

    /// <summary>The offset of the advance-map offset.</summary>
    [<Literal>]
    let private AdvMapOffset = 8

    /// <summary>The offset of the lsb-map offset.</summary>
    [<Literal>]
    let private LsbMapOffset = 12

    /// <summary>The offset of the rsb-map offset.</summary>
    [<Literal>]
    let private RsbMapOffset = 16

    /// <summary>The offset of the vertical-origin-map offset, VVAR only.</summary>
    [<Literal>]
    let private VorgMapOffset = 20

    /// <summary>The no-data metrics variations of a face without a usable HVAR or VVAR table.</summary>
    let private empty =
        { HasData = false
          Store = ItemVariationStore.empty
          AdvMap = ValueNone
          VorgMap = ValueNone }

    /// <summary>Sanitizes the var store at a header offset, the Null store on a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="offset">The store offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeStore (table: ReadOnlySpan<byte>) (offset: int) (ops: int) : int voption =
        match offset with
        | 0 -> ValueSome ops
        | _ -> ItemVariationStore.sanitize table offset ops

    /// <summary>The header offset of one map, in sanitize order.</summary>
    /// <param name="step">The map index.</param>
    /// <returns>The header offset.</returns>
    let private mapOffsetAt (step: int) : int =
        match step with
        | 0 -> AdvMapOffset
        | 1 -> LsbMapOffset
        | 2 -> RsbMapOffset
        | _ -> VorgMapOffset

    /// <summary>Sanitizes the shared maps and, for VVAR, the vertical-origin map, threading the budget.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="step">The next map index.</param>
    /// <param name="count">The number of maps.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    [<TailCall>]
    let rec private sanitizeMaps (table: ReadOnlySpan<byte>) (step: int) (count: int) (ops: int) : int voption =
        match step >= count with
        | true -> ValueSome ops
        | false ->
            match DeltaSetIndexMap.sanitize table (int (Binary.u32 table (mapOffsetAt step))) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeMaps table (step + 1) count remaining

    /// <summary>Sanitizes an HVAR or VVAR table, <c>HVARVVAR::sanitize</c> and <c>VVAR::sanitize</c>.</summary>
    /// <param name="withVorg">True for VVAR, which adds the vertical-origin map.</param>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (withVorg: bool) (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        let headerSize = if withVorg then VvarSize else HeaderSize
        let mapCount = if withVorg then 4 else 3
        match Binary.u32 table VersionOffset >>> 16 = 1u && Sanitize.fits table.Length 0L headerSize with
        | false -> ValueNone
        | true ->
            match sanitizeStore table (int (Binary.u32 table VarStoreOffset)) ops with
            | ValueNone -> ValueNone
            | ValueSome afterStore -> sanitizeMaps table 0 mapCount afterStore

    /// <summary>The struct form of the HVAR or VVAR sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        val private withVorg: bool

        /// <summary>Creates an HVAR or VVAR sanitizer.</summary>
        /// <param name="withVorg">True for VVAR.</param>
        new (withVorg) = { withVorg = withVorg }

        interface ITableSanitizer with
            member this.Sanitize (table, ops) = sanitize this.withVorg table ops

    /// <summary>Reads the var store at a header offset, the Null store on a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The store offset.</param>
    /// <returns>The store.</returns>
    let private readStore (table: ReadOnlyMemory<byte>) (at: int) : ItemVariationStore =
        match at with
        | 0 -> ItemVariationStore.empty
        | _ -> ItemVariationStore.read table at

    /// <summary>Reads a map at a header offset, none on a zero offset.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="at">The map offset.</param>
    /// <returns>The map, or none.</returns>
    let private readMap (table: ReadOnlyMemory<byte>) (at: int) : DeltaSetIndexMap voption =
        match at with
        | 0 -> ValueNone
        | _ -> ValueSome (DeltaSetIndexMap.read table at)

    /// <summary>Loads an HVAR or VVAR table, empty on sanitize failure.</summary>
    /// <param name="face">The face.</param>
    /// <param name="tag">The HVAR or VVAR tag.</param>
    /// <param name="withVorg">True for VVAR.</param>
    /// <returns>The parsed metrics variations.</returns>
    let private loadTable (face: Face) (tag: Tag) (withVorg: bool) : MetricsVariations =
        let table = Sanitize.table (Sanitizer(withVorg)) (Face.referenceTable face tag)
        { HasData = (Binary.u32 table.Span VersionOffset >>> 16) <> 0u
          Store = readStore table (int (Binary.u32 table.Span VarStoreOffset))
          AdvMap = readMap table (int (Binary.u32 table.Span AdvMapOffset))
          VorgMap =
            match withVorg with
            | false -> ValueNone
            | true -> readMap table (int (Binary.u32 table.Span VorgMapOffset)) }

    /// <summary>Loads the HVAR table, empty on sanitize failure, <c>OT::HVAR</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed metrics variations.</returns>
    let loadHvar (face: Face) : MetricsVariations =
        loadTable face Tags.Hvar false

    /// <summary>Loads the VVAR table, empty on sanitize failure, <c>OT::VVAR</c>.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed metrics variations.</returns>
    let loadVvar (face: Face) : MetricsVariations =
        loadTable face Tags.Vvar true

    /// <summary>The unscaled advance delta of a glyph, <c>HVARVVAR::get_advance_delta_unscaled</c>.</summary>
    /// <param name="variations">The metrics variations.</param>
    /// <param name="instance">The store instance holding the region scalars.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The delta.</returns>
    let advanceDelta (variations: MetricsVariations) (instance: StoreInstance) (glyph: uint32) : float32 =
        match variations.AdvMap with
        | ValueNone ->
            match glyph > 0xFFFFu with
            | true -> 0.0f
            | false -> ItemVariationStore.delta instance glyph
        | ValueSome map -> ItemVariationStore.delta instance (DeltaSetIndexMap.map map glyph)

    /// <summary>The unscaled vertical-origin delta of a glyph, <c>VVAR::get_vorg_delta_unscaled</c>.</summary>
    /// <param name="variations">The metrics variations.</param>
    /// <param name="instance">The store instance holding the region scalars.</param>
    /// <param name="glyph">The glyph id.</param>
    /// <returns>The delta, 0 without a vertical-origin map.</returns>
    let vorgDelta (variations: MetricsVariations) (instance: StoreInstance) (glyph: uint32) : float32 =
        match variations.VorgMap with
        | ValueNone -> 0.0f
        | ValueSome map -> ItemVariationStore.delta instance (DeltaSetIndexMap.map map glyph)

    /// <summary>Clamps an advance plus its delta into an int, <c>get_advance_with_var_unscaled</c>.</summary>
    /// <param name="unscaled">The unscaled plain advance.</param>
    /// <param name="delta">The variation delta.</param>
    /// <returns>The advance with the delta, reinterpreted as an int.</returns>
    let advanceWithVariations (unscaled: uint32) (delta: float32) : int =
        int (Fixed.clampToUInt32 (float (float32 unscaled + Fixed.roundf delta)))
