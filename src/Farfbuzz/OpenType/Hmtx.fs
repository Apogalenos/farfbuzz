namespace Farfbuzz

open System

/// <summary>The counts and bytes of an hmtx or vmtx table.</summary>
[<Struct>]
type internal Mtx =
    { Table: ReadOnlyMemory<byte>
      LongMetrics: int
      Bearings: int
      Advances: int
      DefaultAdvance: int }

/// <summary>The hmtx/vmtx tables and their 32-bit HMTX/VMTX twins.</summary>
[<RequireQualifiedAccess>]
module internal Hmtx =
    /// <summary>The size of a long metric record.</summary>
    [<Literal>]
    let private LongMetricSize = 4

    /// <summary>The size of one trailing bearing.</summary>
    [<Literal>]
    let private BearingSize = 2

    /// <summary>Builds one metrics accelerator from a face and one header table.</summary>
    /// <param name="face">The face.</param>
    /// <param name="tableTag">The hmtx or vmtx tag.</param>
    /// <param name="heaTag">The matching hea tag.</param>
    /// <param name="upperHea">Whether the matching hea is the 32-bit uppercase table.</param>
    /// <param name="defaultAdvance">The advance used when the table is absent.</param>
    /// <returns>The metrics.</returns>
    let private build (face: Face) (tableTag: Tag) (heaTag: Tag) (upperHea: bool) (defaultAdvance: int) : Mtx =
        let table = Face.referenceTable face tableTag
        let len0 = table.Length - (table.Length &&& 1)
        let requested = Hhea.longMetricCount face heaTag upperHea
        let longMetrics = int (min requested (uint32 (len0 / LongMetricSize)))
        let len1 = len0 - longMetrics * LongMetricSize
        let bearings0 = face.GlyphCount
        let bearings1 = max bearings0 longMetrics
        let bearings =
            match (bearings1 - longMetrics) * BearingSize > len1 with
            | true -> longMetrics + len1 / BearingSize
            | false -> bearings1
        let len2 = len1 - (bearings - longMetrics) * BearingSize
        let longMetrics, bearings =
            match longMetrics with
            | 0 -> 0, 0
            | _ -> longMetrics, bearings
        { Table = table
          LongMetrics = longMetrics
          Bearings = bearings
          Advances = bearings + len2 / BearingSize
          DefaultAdvance = defaultAdvance }

    /// <summary>Loads hmtx or vmtx, preferring the upper table when it has bearings.</summary>
    /// <param name="face">The face.</param>
    /// <param name="horizontal">True for hmtx, false for vmtx.</param>
    /// <returns>The selected metrics.</returns>
    let load (face: Face) (horizontal: bool) : Mtx =
        let lowerTag, upperTag, lowerHea, upperHea, defaultAdvance =
            match horizontal with
            | true -> Tags.Hmtx, Tags.UpperHmtx, Tags.Hhea, Tags.UpperHhea, face.Upem / 2
            | false -> Tags.Vmtx, Tags.UpperVmtx, Tags.Vhea, Tags.UpperVhea, face.Upem
        let upper = build face upperTag upperHea true defaultAdvance
        let lowerFace =
            match upper.Bearings with
            | 0 -> face
            | _ -> Face.empty
        let lower = build lowerFace lowerTag lowerHea false defaultAdvance
        match upper.Bearings with
        | 0 -> lower
        | _ -> upper

    /// <summary>Whether the metrics table has any bearings, <c>accelerator_t::has_data</c>.</summary>
    /// <param name="mtx">The metrics.</param>
    /// <returns>True when at least one bearing exists.</returns>
    let hasData (mtx: Mtx) : bool =
        mtx.Bearings <> 0

    /// <summary>Returns the unscaled advance, <c>get_advance_without_var_unscaled</c>.</summary>
    /// <param name="mtx">The metrics.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    let advance (mtx: Mtx) (glyph: uint32) : int =
        let index = uint64 glyph
        match index < uint64 mtx.Bearings with
        | true -> int (Binary.u16 mtx.Table.Span (int (min index (uint64 mtx.LongMetrics - 1UL)) * LongMetricSize))
        | false ->
            match mtx.Advances with
            | 0 -> mtx.DefaultAdvance
            | _ -> 0

    /// <summary>Returns the unscaled advance reading the table bytes from a span the caller took once, so a loop over
    /// many glyphs neither copies the metrics nor rebuilds the span, <c>get_advance_without_var_unscaled</c>.</summary>
    /// <param name="mtx">The metrics.</param>
    /// <param name="table">The bytes of the metrics table.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The advance.</returns>
    let advanceIn (mtx: inref<Mtx>) (table: ReadOnlySpan<byte>) (glyph: uint32) =
        let index = uint64 glyph
        match index < uint64 mtx.Bearings with
        | true -> int (Binary.u16 table (int (min index (uint64 mtx.LongMetrics - 1UL)) * LongMetricSize))
        | false ->
            match mtx.Advances with
            | 0 -> mtx.DefaultAdvance
            | _ -> 0

    /// <summary>Returns the unscaled leading bearing, <c>get_leading_bearing_without_var_unscaled</c>.</summary>
    /// <param name="mtx">The metrics.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The bearing, or 0 beyond the table.</returns>
    let leadingBearing (mtx: Mtx) (glyph: uint32) : int =
        let index = uint64 glyph
        match index < uint64 mtx.LongMetrics with
        | true -> int (Binary.i16 mtx.Table.Span (int index * LongMetricSize + 2))
        | false ->
            match index >= uint64 mtx.Bearings with
            | true -> 0
            | false ->
                let at = mtx.LongMetrics * LongMetricSize + int (index - uint64 mtx.LongMetrics) * BearingSize
                int (Binary.i16 mtx.Table.Span at)
