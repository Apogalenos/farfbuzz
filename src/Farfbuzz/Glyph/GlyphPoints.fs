namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.Var

/// <summary>One glyph point: the accumulated coordinates, the raw simple-glyph flag and whether the point ends a contour.</summary>
[<Struct; NoComparison; NoEquality>]
type internal ContourPoint =
    { X: float32
      Y: float32
      Flag: byte
      IsEndPoint: bool }

/// <summary>The ten-byte header of one glyph, <c>OT::glyf_impl::GlyphHeader</c>.</summary>
[<Struct; NoComparison; NoEquality>]
type internal GlyphHeader =
    { NumberOfContours: int16
      XMin: int16
      YMin: int16
      XMax: int16
      YMax: int16 }

/// <summary>The per-face glyf accelerator, built on phase 4's <c>GlyfHeaders</c> (loca, glyf, index-to-loc format).</summary>
[<NoEquality; NoComparison>]
type internal GlyfFace =
    { Glyf: ReadOnlyMemory<byte>
      Loca: ReadOnlyMemory<byte>
      ShortOffsets: bool
      Extended: bool
      NumGlyphs: int
      Hmtx: Mtx
      Vmtx: Mtx }

/// <summary>What one glyph is, from its <c>numberOfContours</c>.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal GlyphKind =
    | EmptyGlyph
    | SimpleGlyph of contours: int
    | CompositeGlyph

/// <summary>One composite glyph component: its flags, glyph id, transformation matrix and offset.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Component =
    { ComponentFlags: uint16
      GlyphId: uint32
      M0: float32
      M1: float32
      M2: float32
      M3: float32
      Offset: ContourPoint }

/// <summary>Building the per-face glyf accelerator.</summary>
[<RequireQualifiedAccess>]
module internal GlyfFace =
    /// <summary>Loads the accelerator from a face, reusing phase 4's loca and glyf reader.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The accelerator.</returns>
    let load (face: Face) : GlyfFace =
        let headers = GlyfHeader.load face
        { Glyf = headers.Glyf
          Loca = headers.Loca
          ShortOffsets = not headers.LongOffsets
          Extended = false
          NumGlyphs = headers.NumGlyphs
          Hmtx = Hmtx.load face true
          Vmtx = Hmtx.load face false }

/// <summary>Reading the non-variable glyph points, <c>OT/glyf</c>.</summary>
[<RequireQualifiedAccess>]
module internal GlyphPoints =
    /// <summary>The size of one glyph header, <c>GlyphHeader::static_size</c>.</summary>
    [<Literal>]
    let private HeaderSize = 10

    /// <summary>The minimum size of one composite record, flags plus a 16-bit glyph id.</summary>
    [<Literal>]
    let private CompositeMinSize = 4

    /// <summary>The on-curve point flag, <c>FLAG_ON_CURVE</c>.</summary>
    [<Literal>]
    let private FlagOnCurve = 0x01uy

    /// <summary>The x-short flag, <c>FLAG_X_SHORT</c>.</summary>
    [<Literal>]
    let private FlagXShort = 0x02uy

    /// <summary>The y-short flag, <c>FLAG_Y_SHORT</c>.</summary>
    [<Literal>]
    let private FlagYShort = 0x04uy

    /// <summary>The repeat flag, <c>FLAG_REPEAT</c>.</summary>
    [<Literal>]
    let private FlagRepeat = 0x08uy

    /// <summary>The x-same flag, <c>FLAG_X_SAME</c>.</summary>
    [<Literal>]
    let private FlagXSame = 0x10uy

    /// <summary>The y-same flag, <c>FLAG_Y_SAME</c>.</summary>
    [<Literal>]
    let private FlagYSame = 0x20uy

    /// <summary>The cubic flag, <c>FLAG_CUBIC</c>, cleared unless the glyph data is extended.</summary>
    [<Literal>]
    let private FlagCubic = 0x80uy

    /// <summary>The arguments-are-words composite flag, <c>ARG_1_AND_2_ARE_WORDS</c>.</summary>
    [<Literal>]
    let private ArgWords = 0x0001us

    /// <summary>The arguments-are-xy-values composite flag, <c>ARGS_ARE_XY_VALUES</c>.</summary>
    [<Literal>]
    let private ArgsXy = 0x0002us

    /// <summary>The have-a-scale composite flag, <c>WE_HAVE_A_SCALE</c>.</summary>
    [<Literal>]
    let private HaveScale = 0x0008us

    /// <summary>The more-components composite flag, <c>MORE_COMPONENTS</c>.</summary>
    [<Literal>]
    let private MoreComponents = 0x0020us

    /// <summary>The have-an-x-and-y-scale composite flag, <c>WE_HAVE_AN_X_AND_Y_SCALE</c>.</summary>
    [<Literal>]
    let private HaveXYScale = 0x0040us

    /// <summary>The have-a-two-by-two composite flag, <c>WE_HAVE_A_TWO_BY_TWO</c>.</summary>
    [<Literal>]
    let private Have2x2 = 0x0080us

    /// <summary>The 24-bit glyph id composite flag, <c>GID_IS_24BIT</c>.</summary>
    [<Literal>]
    let private Gid24 = 0x2000us

    /// <summary>The data range of one glyph, empty on a bad or out-of-range loca entry, <c>glyph_for_gid</c>.</summary>
    /// <param name="glyf">The glyf table bytes.</param>
    /// <param name="loca">The loca table bytes.</param>
    /// <param name="shortOffsets">Whether loca holds 16-bit offsets.</param>
    /// <param name="gid">The glyph id.</param>
    /// <returns>The glyph bytes, or empty.</returns>
    let glyphBytes (glyf: ReadOnlyMemory<byte>) (loca: ReadOnlyMemory<byte>) (shortOffsets: bool) (gid: uint32) : ReadOnlyMemory<byte> =
        let span = loca.Span
        let start, finish =
            match shortOffsets with
            | true ->
                let at = int gid * 2
                uint32 (int (Binary.u16 span at) * 2), uint32 (int (Binary.u16 span (at + 2)) * 2)
            | false ->
                let at = int gid * 4
                Binary.u32 span at, Binary.u32 span (at + 4)
        match start > finish || uint64 finish > uint64 glyf.Length with
        | true -> ReadOnlyMemory.Empty
        | false -> glyf.Slice(int start, int (finish - start))

    /// <summary>What one glyph is, from its header's <c>numberOfContours</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <returns>The glyph kind.</returns>
    let kind (glyph: ReadOnlySpan<byte>) : GlyphKind =
        match Binary.i16 glyph 0 with
        | 0s -> EmptyGlyph
        | contours when contours > 0s -> SimpleGlyph (int contours)
        | _ -> CompositeGlyph

    /// <summary>Reads one flag run: the flag, the repeat count it carries and the next byte position.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="at">The flag byte position.</param>
    /// <returns>The run, or none when the bytes run out.</returns>
    let private readFlagRun (glyph: ReadOnlySpan<byte>) (at: int) : struct (byte * int * int) voption =
        match at >= glyph.Length with
        | true -> ValueNone
        | false ->
            let flag = glyph[at]
            match flag &&& FlagRepeat <> 0uy with
            | false -> ValueSome (struct (flag, 0, at + 1))
            | true ->
                match at + 1 >= glyph.Length with
                | true -> ValueNone
                | false -> ValueSome (struct (flag, int glyph[at + 1], at + 2))

    /// <summary>Walks the flag stream to its end, checking every byte, <c>read_flags</c>' bounds.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="at">The first flag byte.</param>
    /// <param name="index">The next point index.</param>
    /// <param name="count">The number of points.</param>
    /// <returns>The position after the flags, or none when a byte is out of range.</returns>
    [<TailCall>]
    let rec private flagEnd (glyph: ReadOnlySpan<byte>) (at: int) (index: int) (count: int) : int voption =
        match index >= count with
        | true -> ValueSome at
        | false ->
            match readFlagRun glyph at with
            | ValueNone -> ValueNone
            | ValueSome struct (_, repeat, nextAt) -> flagEnd glyph nextAt (index + 1 + repeat) count

    /// <summary>The running state of the flag unfolder.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FlagState =
        { At: int
          Flag: byte
          Remaining: int }

    /// <summary>Unfolds the run-length flag stream into one flag per point, clearing the cubic bit unless extended.</summary>
    [<Struct>]
    type private FlagUnfolder =
        val private Extended: bool
        new (extended) = { Extended = extended }
        interface ISpanUnfolder<byte, FlagState, byte> with
            member this.Next (source, state) =
                match state.Remaining > 0 with
                | true -> struct (state.Flag, { state with Remaining = state.Remaining - 1 })
                | false ->
                    match readFlagRun source state.At with
                    | ValueSome struct (flag, repeat, nextAt) ->
                        let flag = if this.Extended then flag else flag &&& ~~~FlagCubic
                        struct (flag, { At = nextAt; Flag = flag; Remaining = repeat })
                    | ValueNone -> struct (0uy, { state with At = state.At + 1 })

    /// <summary>Reads the flags of a simple glyph, returning them with the position after them.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="at">The first flag byte.</param>
    /// <param name="count">The number of points.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <returns>The flags and the position after them, or none on a truncated flag run.</returns>
    let private readFlags (glyph: ReadOnlySpan<byte>) (at: int) (count: int) (extended: bool) : struct (ImmutableArray<byte> * int) voption =
        match flagEnd glyph at 0 count with
        | ValueNone -> ValueNone
        | ValueSome flagsEnd ->
            let flags = Sealed.unfoldSpan glyph count { At = at; Flag = 0uy; Remaining = 0 } (FlagUnfolder(extended))
            ValueSome (struct (flags, flagsEnd))

    /// <summary>The number of coordinate bytes one flag consumes on an axis.</summary>
    /// <param name="flag">The point flag.</param>
    /// <param name="shortFlag">The short flag of the axis.</param>
    /// <param name="sameFlag">The same flag of the axis.</param>
    /// <returns>The byte count: 1, 2 or 0.</returns>
    let inline private coordByteCount (flag: byte) (shortFlag: byte) (sameFlag: byte) : int =
        match flag &&& shortFlag <> 0uy with
        | true -> 1
        | false ->
            match flag &&& sameFlag = 0uy with
            | true -> 2
            | false -> 0

    /// <summary>Sums the coordinate bytes of one axis over the flags.</summary>
    /// <param name="flags">The point flags.</param>
    /// <param name="index">The next point index.</param>
    /// <param name="shortFlag">The short flag of the axis.</param>
    /// <param name="sameFlag">The same flag of the axis.</param>
    /// <param name="total">The byte count so far.</param>
    /// <returns>The total byte count.</returns>
    [<TailCall>]
    let rec private coordByteTotal (flags: ReadOnlySpan<byte>) (index: int) (shortFlag: byte) (sameFlag: byte) (total: int) : int =
        match index >= flags.Length with
        | true -> total
        | false -> coordByteTotal flags (index + 1) shortFlag sameFlag (total + coordByteCount flags[index] shortFlag sameFlag)

    /// <summary>The running state of a coordinate unfolder.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private CoordState =
        { Index: int
          V: int
          P: int }

    /// <summary>Unfolds one coordinate axis: deltas accumulated in <c>int</c>, then stored as float32.</summary>
    [<Struct>]
    type private CoordUnfolder =
        val private Flags: ImmutableArray<byte>
        val private ShortFlag: byte
        val private SameFlag: byte
        new (flags, shortFlag, sameFlag) = { Flags = flags; ShortFlag = shortFlag; SameFlag = sameFlag }
        interface ISpanUnfolder<byte, CoordState, float32> with
            member this.Next (source, state) =
                let flag = this.Flags[state.Index]
                let v = state.V
                let p = state.P
                let nextV, nextP =
                    match flag &&& this.ShortFlag <> 0uy with
                    | true ->
                        let delta = int (Binary.u8 source p)
                        v + (if flag &&& this.SameFlag <> 0uy then delta else -delta), p + 1
                    | false ->
                        match flag &&& this.SameFlag = 0uy with
                        | true -> v + int (Binary.i16 source p), p + 2
                        | false -> v, p
                struct (float32 nextV, { Index = state.Index + 1; V = nextV; P = nextP })

    /// <summary>Unfolds the end-point table into one end point per contour.</summary>
    [<Struct>]
    type private EndPointUnfolder =
        interface ISpanUnfolder<byte, int, uint16> with
            member _.Next (source, at) =
                struct (Binary.u16 source at, at + 2)

    /// <summary>Whether a point index ends a contour, scanning the end-point table.</summary>
    /// <param name="endPoints">The end points of the contours.</param>
    /// <param name="i">The point index.</param>
    /// <param name="j">The next end-point index.</param>
    /// <returns>True when the point ends a contour.</returns>
    [<TailCall>]
    let rec private isEndPointOf (endPoints: ImmutableArray<uint16>) (i: int) (j: int) : bool =
        match j >= endPoints.Length with
        | true -> false
        | false ->
            match int endPoints[j] = i with
            | true -> true
            | false -> isEndPointOf endPoints i (j + 1)

    /// <summary>Reads the header structure of a simple glyph, or none when its header does not hold.</summary>
    /// <param name="glyph">The glyph bytes, from the header.</param>
    /// <returns>The contour count, point count and flag offset.</returns>
    let private simpleHead (glyph: ReadOnlySpan<byte>) : struct (int * int * int) voption =
        let numContours = int (Binary.i16 glyph 0)
        let endPtsAt = HeaderSize
        let headerFits = HeaderSize + 2 * (numContours + 1) <= glyph.Length
        let numPoints = int (Binary.u16 glyph (endPtsAt + 2 * (numContours - 1))) + 1
        let instructionLength = int (Binary.u16 glyph (endPtsAt + 2 * numContours))
        let flagsAt = endPtsAt + 2 * (numContours + 1) + instructionLength
        match numContours > 0 && headerFits && numPoints >= numContours with
        | false -> ValueNone
        | true -> ValueSome (struct (numContours, numPoints, flagsAt))

    /// <summary>Reads the flags and coordinates of a simple glyph whose header already holds.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="numContours">The number of contours.</param>
    /// <param name="numPoints">The number of points.</param>
    /// <param name="flagsAt">The byte offset of the flags.</param>
    /// <returns>The points, or none where the flag or coordinate data is truncated.</returns>
    let private readSimplePoints (glyph: ReadOnlySpan<byte>) (extended: bool) (numContours: int) (numPoints: int) (flagsAt: int) : ImmutableArray<ContourPoint> voption =
        match readFlags glyph flagsAt numPoints extended with
        | ValueNone -> ValueNone
        | ValueSome struct (flags, flagsEnd) ->
            let xBytes = coordByteTotal (flags.AsSpan()) 0 FlagXShort FlagXSame 0
            let yBytes = coordByteTotal (flags.AsSpan()) 0 FlagYShort FlagYSame 0
            match flagsEnd + xBytes + yBytes > glyph.Length with
            | true -> ValueNone
            | false ->
                let endPoints = Sealed.unfoldSpan glyph numContours HeaderSize (EndPointUnfolder())
                let xs = Sealed.unfoldSpan glyph numPoints { Index = 0; V = 0; P = flagsEnd } (CoordUnfolder(flags, FlagXShort, FlagXSame))
                let ys = Sealed.unfoldSpan glyph numPoints { Index = 0; V = 0; P = flagsEnd + xBytes } (CoordUnfolder(flags, FlagYShort, FlagYSame))
                ValueSome (Sealed.init numPoints (fun i ->
                    { X = xs[i]
                      Y = ys[i]
                      Flag = flags[i]
                      IsEndPoint = isEndPointOf endPoints i 0 }))

    /// <summary>Reads the contour points of a simple glyph, <c>SimpleGlyph::get_contour_points</c>.</summary>
    /// <param name="glyph">The glyph bytes, from the header.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="phantomOnly">Whether to return zero points without reading flags.</param>
    /// <returns>The points, or none exactly where the C++ returns false.</returns>
    let simplePoints (glyph: ReadOnlySpan<byte>) (extended: bool) (phantomOnly: bool) : ImmutableArray<ContourPoint> voption =
        match simpleHead glyph with
        | ValueNone -> ValueNone
        | ValueSome struct (numContours, numPoints, flagsAt) ->
            match phantomOnly with
            | true -> ValueSome (Sealed.init numPoints (fun _ -> { X = 0.0f; Y = 0.0f; Flag = 0uy; IsEndPoint = false }))
            | false -> readSimplePoints glyph extended numContours numPoints flagsAt

    /// <summary>The byte size of one composite record, <c>CompositeGlyphRecord::get_size</c>.</summary>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="flags">The record flags.</param>
    /// <returns>The record size.</returns>
    let private componentSize (extended: bool) (flags: uint16) : int =
        let glyphIdExtra = match extended && flags &&& Gid24 <> 0us with | true -> 1 | false -> 0
        let argBytes = match flags &&& ArgWords <> 0us with | true -> 4 | false -> 2
        let matrixBytes =
            match flags &&& HaveScale <> 0us, flags &&& HaveXYScale <> 0us, flags &&& Have2x2 <> 0us with
            | true, _, _ -> 2
            | false, true, _ -> 4
            | false, false, true -> 8
            | false, false, false -> 0
        CompositeMinSize + glyphIdExtra + argBytes + matrixBytes

    /// <summary>Reads one composite record, <c>CompositeGlyphRecord::get_transformation</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="at">The record's byte offset.</param>
    /// <param name="flags">The record flags.</param>
    /// <returns>The component.</returns>
    let private readComponent (glyph: ReadOnlySpan<byte>) (extended: bool) (at: int) (flags: uint16) : Component =
        let glyphIdAt = at + 2
        let glyphId, argAt =
            match extended && flags &&& Gid24 <> 0us with
            | true -> Binary.u24 glyph glyphIdAt, glyphIdAt + 3
            | false -> uint32 (Binary.u16 glyph glyphIdAt), glyphIdAt + 2
        let tx, ty, matrixAt =
            match flags &&& ArgWords <> 0us with
            | true -> int (Binary.i16 glyph argAt), int (Binary.i16 glyph (argAt + 2)), argAt + 4
            | false -> int (sbyte (Binary.u8 glyph argAt)), int (sbyte (Binary.u8 glyph (argAt + 1))), argAt + 2
        let tx, ty = match flags &&& ArgsXy <> 0us with | true -> tx, ty | false -> 0, 0
        let m0, m1, m2, m3 =
            match flags &&& HaveScale <> 0us, flags &&& HaveXYScale <> 0us, flags &&& Have2x2 <> 0us with
            | true, _, _ ->
                let scale = Fixed.f2dot14 (Binary.i16 glyph matrixAt)
                scale, 0.0f, 0.0f, scale
            | false, true, _ ->
                Fixed.f2dot14 (Binary.i16 glyph matrixAt), 0.0f, 0.0f, Fixed.f2dot14 (Binary.i16 glyph (matrixAt + 2))
            | false, false, true ->
                Fixed.f2dot14 (Binary.i16 glyph matrixAt),
                Fixed.f2dot14 (Binary.i16 glyph (matrixAt + 2)),
                Fixed.f2dot14 (Binary.i16 glyph (matrixAt + 4)),
                Fixed.f2dot14 (Binary.i16 glyph (matrixAt + 6))
            | false, false, false -> 1.0f, 0.0f, 0.0f, 1.0f
        { ComponentFlags = flags
          GlyphId = glyphId
          M0 = m0
          M1 = m1
          M2 = m2
          M3 = m3
          Offset = { X = float32 tx; Y = float32 ty; Flag = 0uy; IsEndPoint = true } }

    /// <summary>Iterates the composite records, stopping at the first record out of range.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <param name="at">The next record's byte offset.</param>
    /// <param name="builder">The components built so far.</param>
    /// <returns>The completed builder.</returns>
    [<TailCall>]
    let rec private componentsFrom (glyph: ReadOnlySpan<byte>) (extended: bool) (at: int) (builder: PointBuilder<Component>) : PointBuilder<Component> =
        match at + CompositeMinSize > glyph.Length with
        | true -> builder
        | false ->
            let flags = Binary.u16 glyph at
            let size = componentSize extended flags
            match at + size > glyph.Length with
            | true -> builder
            | false ->
                let builder = PointBuilder.append builder (readComponent glyph extended at flags)
                match flags &&& MoreComponents <> 0us with
                | true -> componentsFrom glyph extended (at + size) builder
                | false -> builder

    /// <summary>Reads the component records of a composite glyph, <c>CompositeGlyph::iter</c>.</summary>
    /// <param name="glyph">The glyph bytes.</param>
    /// <param name="extended">Whether the glyph data is extended.</param>
    /// <returns>The components, stopping at the first record out of range.</returns>
    let components (glyph: ReadOnlySpan<byte>) (extended: bool) : ImmutableArray<Component> =
        PointBuilder.publish (componentsFrom glyph extended HeaderSize (PointBuilder.create()))

    /// <summary>Computes the four phantom points, <c>Glyph::get_points</c>'s phantom initialisation.</summary>
    /// <param name="glyf">The accelerator.</param>
    /// <param name="gid">The glyph id.</param>
    /// <param name="header">The glyph header.</param>
    /// <returns>The left, right, top and bottom phantoms.</returns>
    let phantoms (glyf: GlyfFace) (gid: uint32) (header: GlyphHeader) : struct (ContourPoint * ContourPoint * ContourPoint * ContourPoint) =
        let lsb = Hmtx.leadingBearing glyf.Hmtx gid
        let hDelta = int header.XMin - lsb
        let tsb = Hmtx.leadingBearing glyf.Vmtx gid
        let vOrig = int header.YMax + tsb
        let hAdv = Hmtx.advance glyf.Hmtx gid
        let vAdv = Hmtx.advance glyf.Vmtx gid
        struct
            ({ X = float32 hDelta; Y = 0.0f; Flag = 0uy; IsEndPoint = false },
             { X = float32 (hAdv + hDelta); Y = 0.0f; Flag = 0uy; IsEndPoint = false },
             { X = 0.0f; Y = float32 vOrig; Flag = 0uy; IsEndPoint = false },
             { X = 0.0f; Y = float32 (vOrig - vAdv); Flag = 0uy; IsEndPoint = false })

    /// <summary>Transforms a point by a component matrix, <c>contour_point_t::transform</c>.</summary>
    /// <param name="comp">The component.</param>
    /// <param name="point">The point.</param>
    /// <returns>The transformed point.</returns>
    let inline transform (comp: Component) (point: ContourPoint) : ContourPoint =
        let oldX = point.X
        { point with
            X = oldX * comp.M0 + point.Y * comp.M2
            Y = oldX * comp.M1 + point.Y * comp.M3 }


    /// <summary>Transforms a point as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="transform"/> there.</summary>
    /// <param name="comp">The component.</param>
    /// <param name="point">The point.</param>
    /// <returns>The transformed point.</returns>
    let transformCall (comp: Component) (point: ContourPoint) =
        transform comp point