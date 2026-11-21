namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz
open Farfbuzz.Var

/// <summary>One variation axis, <c>hb_ot_var_axis_info_t</c>, with the coordinates already ordered around the default.</summary>
[<Struct>]
type internal AxisInfo =
    { AxisIndex: int
      Tag: Tag
      NameId: uint16
      Flags: uint16
      MinValue: float32
      DefaultValue: float32
      MaxValue: float32 }

/// <summary>The fvar table: axes, the named-instance counts, the raw instance records and whether it has data.</summary>
type internal Fvar =
    { Axes: ImmutableArray<AxisInfo>
      InstanceCount: int
      InstanceSize: int
      Instances: ReadOnlyMemory<byte>
      HasData: bool }

/// <summary>The fvar table of a face, <c>OT::fvar</c>.</summary>
[<RequireQualifiedAccess>]
module internal Fvar =
    /// <summary>The size of the fvar header.</summary>
    [<Literal>]
    let private HeaderSize = 16

    /// <summary>The size of one AxisRecord, the value <c>axisSize</c> must equal.</summary>
    [<Literal>]
    let private AxisRecordSize = 20

    /// <summary>The offset of the axes-array offset.</summary>
    [<Literal>]
    let private FirstAxisOffset = 4

    /// <summary>The offset of the axis count.</summary>
    [<Literal>]
    let private AxisCountOffset = 8

    /// <summary>The offset of the axis size.</summary>
    [<Literal>]
    let private AxisSizeOffset = 10

    /// <summary>The offset of the instance count.</summary>
    [<Literal>]
    let private InstanceCountOffset = 12

    /// <summary>The offset of the instance size.</summary>
    [<Literal>]
    let private InstanceSizeOffset = 14

    /// <summary>The offset of an axis tag.</summary>
    [<Literal>]
    let private AxisTagOffset = 0

    /// <summary>The offset of an axis minimum.</summary>
    [<Literal>]
    let private AxisMinOffset = 4

    /// <summary>The offset of an axis default.</summary>
    [<Literal>]
    let private AxisDefaultOffset = 8

    /// <summary>The offset of an axis maximum.</summary>
    [<Literal>]
    let private AxisMaxOffset = 12

    /// <summary>The offset of an axis flags word.</summary>
    [<Literal>]
    let private AxisFlagsOffset = 16

    /// <summary>The offset of an axis name id.</summary>
    [<Literal>]
    let private AxisNameIdOffset = 18

    /// <summary>The offset of a named instance's subfamily name id.</summary>
    [<Literal>]
    let private InstanceSubfamilyOffset = 0

    /// <summary>The offset of a named instance's coordinates.</summary>
    [<Literal>]
    let private InstanceCoordsOffset = 4

    /// <summary>The invalid name id, <c>HB_OT_NAME_ID_INVALID</c>.</summary>
    [<Literal>]
    let private NameIdInvalid = 0xFFFFus

    /// <summary>The empty fvar of a face without a usable fvar table.</summary>
    let empty =
        { Axes = ImmutableArray.Empty
          InstanceCount = 0
          InstanceSize = 0
          Instances = ReadOnlyMemory<byte>.Empty
          HasData = false }

    /// <summary>Sanitizes an fvar table, <c>fvar::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) ops =
        match Binary.u16 table 0 = 1us && Sanitize.fits table.Length 0L HeaderSize with
        | false -> ValueNone
        | true ->
            let axisCount = int (Binary.u16 table AxisCountOffset)
            let axisSize = int (Binary.u16 table AxisSizeOffset)
            let instanceCount = int (Binary.u16 table InstanceCountOffset)
            let instanceSize = int (Binary.u16 table InstanceSizeOffset)
            let firstAxis = int (Binary.u16 table FirstAxisOffset)
            match axisSize = AxisRecordSize && instanceSize >= axisCount * 4 + 4 && firstAxis <> 0 with
            | false -> ValueNone
            | true ->
                match Sanitize.array table.Length (int64 firstAxis) (uint32 axisCount) (uint32 AxisRecordSize) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining ->
                    let instancesAt = int64 firstAxis + int64 axisCount * int64 AxisRecordSize
                    Sanitize.array table.Length instancesAt (uint32 instanceCount) (uint32 instanceSize) remaining

    /// <summary>The struct form of the fvar sanitizer.</summary>
    [<Struct>]
    type Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Loads the fvar table, empty on sanitize failure, <c>fvar::has_data</c> gating the axes.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The parsed fvar, or empty.</returns>
    let load face =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Fvar)
        let axisCount = int (Binary.u16 table.Span AxisCountOffset)
        let instanceCount = int (Binary.u16 table.Span InstanceCountOffset)
        let instanceSize = int (Binary.u16 table.Span InstanceSizeOffset)
        let firstAxis = int (Binary.u16 table.Span FirstAxisOffset)
        let axes =
            Sealed.init axisCount (fun index ->
                let at = firstAxis + index * AxisRecordSize
                let minValue = Fixed.f16dot16 (int (Binary.u32 table.Span (at + AxisMinOffset)))
                let defaultValue = Fixed.f16dot16 (int (Binary.u32 table.Span (at + AxisDefaultOffset)))
                let maxValue = Fixed.f16dot16 (int (Binary.u32 table.Span (at + AxisMaxOffset)))
                { AxisIndex = index
                  Tag = Binary.tag table.Span (at + AxisTagOffset)
                  NameId = Binary.u16 table.Span (at + AxisNameIdOffset)
                  Flags = Binary.u16 table.Span (at + AxisFlagsOffset)
                  MinValue = Fixed.hbMin defaultValue minValue
                  DefaultValue = defaultValue
                  MaxValue = Fixed.hbMax defaultValue maxValue })
        { Axes = axes
          InstanceCount = instanceCount
          InstanceSize = instanceSize
          Instances = table.Slice(firstAxis + axisCount * AxisRecordSize, instanceCount * instanceSize)
          HasData = Binary.u32 table.Span 0 <> 0u }

    /// <summary>Returns the first axis with a tag, <c>fvar::find_axis_info</c>.</summary>
    /// <param name="tag">The axis tag.</param>
    /// <param name="fvar">The fvar table.</param>
    /// <returns>The axis, or none.</returns>
    let findAxisInfo tag fvar =
        match Spans.tryFindIndex (fun axis -> axis.Tag = tag) (fvar.Axes.AsSpan()) with
        | ValueSome index -> ValueSome fvar.Axes[index]
        | ValueNone -> ValueNone

    /// <summary>Normalizes a design coordinate onto an axis, <c>AxisRecord::normalize_axis_value</c>.</summary>
    /// <param name="axis">The axis index.</param>
    /// <param name="value">The design coordinate.</param>
    /// <param name="fvar">The fvar table.</param>
    /// <returns>The normalized coordinate.</returns>
    let normalizeAxisValue axis value fvar =
        match axis >= 0 && axis < fvar.Axes.Length with
        | false -> 0.0f
        | true ->
            let info = fvar.Axes[axis]
            let clamped = Fixed.hbClamp value info.MinValue info.MaxValue
            match clamped = info.DefaultValue with
            | true -> 0.0f
            | false ->
                match clamped < info.DefaultValue with
                | true -> (clamped - info.DefaultValue) / (info.DefaultValue - info.MinValue)
                | false -> (clamped - info.DefaultValue) / (info.MaxValue - info.DefaultValue)

    /// <summary>A named instance's subfamily name id, <c>fvar::get_instance_subfamily_name_id</c>.</summary>
    /// <param name="instance">The instance index.</param>
    /// <param name="fvar">The fvar table.</param>
    /// <returns>The name id, or <c>HB_OT_NAME_ID_INVALID</c>.</returns>
    let instanceSubfamilyNameId instance fvar =
        match instance >= fvar.InstanceCount with
        | true -> NameIdInvalid
        | false -> Binary.u16 fvar.Instances.Span (instance * fvar.InstanceSize + InstanceSubfamilyOffset)

    /// <summary>A named instance's PostScript name id, <c>fvar::get_instance_postscript_name_id</c>.</summary>
    /// <param name="instance">The instance index.</param>
    /// <param name="fvar">The fvar table.</param>
    /// <returns>The name id, or <c>HB_OT_NAME_ID_INVALID</c>.</returns>
    let instancePostscriptNameId instance fvar =
        match instance >= fvar.InstanceCount with
        | true -> NameIdInvalid
        | false ->
            match fvar.InstanceSize >= fvar.Axes.Length * 4 + 6 with
            | false -> NameIdInvalid
            | true -> Binary.u16 fvar.Instances.Span (instance * fvar.InstanceSize + InstanceCoordsOffset + fvar.Axes.Length * 4)

    /// <summary>A named instance's design coordinates, <c>fvar::get_instance_coords</c>.</summary>
    /// <param name="instance">The instance index.</param>
    /// <param name="fvar">The fvar table.</param>
    /// <returns>The coordinates, or empty when out of range.</returns>
    let instanceCoords instance fvar =
        match instance >= fvar.InstanceCount with
        | true -> ImmutableArray.Empty
        | false ->
            let axisCount = fvar.Axes.Length
            let at = instance * fvar.InstanceSize + InstanceCoordsOffset
            Sealed.init axisCount (fun index -> Fixed.f16dot16 (int (Binary.u32 fvar.Instances.Span (at + index * 4))))
