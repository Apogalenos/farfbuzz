namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>A class definition table, decoded once into native arrays in file order.</summary>
[<NoEquality; NoComparison>]
type internal ClassDef =
    | ClassDefNull
    | ClassDefArray of startGlyph: uint32 * classes: ImmutableArray<uint32>
    | ClassDefRanges of classRanges: ImmutableArray<RangeRecord>
    | ClassDefDirect of classSlots: ImmutableArray<uint16> * classComplete: bool * classSource: ClassDef

/// <summary>ClassDef formats 1-4: decoding, class lookup and class collection.</summary>
[<RequireQualifiedAccess>]
module internal ClassDef =
    /// <summary>The byte width of a 16-bit value.</summary>
    [<Literal>]
    let private Width16 = 2

    /// <summary>The byte width of a 24-bit value.</summary>
    [<Literal>]
    let private Width24 = 3

    /// <summary>Sanitizes a 16-bit array ClassDef, <c>ClassDefFormat1_3&lt;SmallTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeArray16 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset) 6 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 6L) (uint32 (Binary.u16 table (offset + 4)) * 2u) ops

    /// <summary>Sanitizes a 16-bit range ClassDef, <c>ClassDefFormat2_4&lt;SmallTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeRanges16 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset) 4 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 4L) (uint32 (Binary.u16 table (offset + 2)) * 6u) ops

    /// <summary>Sanitizes a 24-bit array ClassDef, <c>ClassDefFormat1_3&lt;MediumTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeArray24 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset) 8 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 8L) (Binary.u24 table (offset + 5) * 3u) ops

    /// <summary>Sanitizes a 24-bit range ClassDef, <c>ClassDefFormat2_4&lt;MediumTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeRanges24 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset) 5 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 5L) (Binary.u24 table (offset + 2) * 8u) ops

    /// <summary>Sanitizes a class definition, <c>ClassDef::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset) Width16 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table offset with
            | 1us -> sanitizeArray16 table offset ops
            | 2us -> sanitizeRanges16 table offset ops
            | 3us -> sanitizeArray24 table offset ops
            | 4us -> sanitizeRanges24 table offset ops
            | _ -> ValueSome ops

    /// <summary>Decodes an array ClassDef's class values into native uint32.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ClassDecoder =
        { Base: int
          Width: int }
        interface ISpanUnfolder<byte, int, uint32> with
            member this.Next (table, index) =
                let offset = this.Base + index * this.Width
                struct (Coverage.readWide table offset this.Width, index + 1)

    /// <summary>Decodes a range ClassDef's records into native range records.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ClassRangeDecoder =
        { Base: int
          GlyphWidth: int }
        interface ISpanUnfolder<byte, int, RangeRecord> with
            member this.Next (table, index) =
                let offset = this.Base + index * (2 * this.GlyphWidth + Width16)
                let first = Coverage.readWide table offset this.GlyphWidth
                let last = Coverage.readWide table (offset + this.GlyphWidth) this.GlyphWidth
                let value = uint32 (Binary.u16 table (offset + 2 * this.GlyphWidth))
                struct ({ First = first; Last = last; Value = value }, index + 1)

    /// <summary>Decodes a class definition into native arrays in file order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <returns>The class definition.</returns>
    let parse (table: ReadOnlySpan<byte>) (offset) =
        match Binary.u16 table offset with
        | 1us ->
            let startGlyph = uint32 (Binary.u16 table (offset + 2))
            ClassDefArray (startGlyph, Sealed.unfoldSpan table (int (Binary.u16 table (offset + 4))) 0 { ClassDecoder.Base = offset + 6; Width = Width16 })
        | 2us ->
            ClassDefRanges (Sealed.unfoldSpan table (int (Binary.u16 table (offset + 2))) 0 { ClassRangeDecoder.Base = offset + 4; GlyphWidth = Width16 })
        | 3us ->
            let startGlyph = Coverage.readWide table (offset + 2) Width24
            ClassDefArray (startGlyph, Sealed.unfoldSpan table (int (Binary.u24 table (offset + 5))) 0 { ClassDecoder.Base = offset + 8; Width = Width24 })
        | 4us ->
            ClassDefRanges (Sealed.unfoldSpan table (int (Binary.u24 table (offset + 2))) 0 { ClassRangeDecoder.Base = offset + 5; GlyphWidth = Width24 })
        | _ -> ClassDefNull

    /// <summary>Looks up a glyph's class, <c>ClassDef::get_class</c>.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The class, zero when unclassified or out of range.</returns>
    [<TailCall>]
    let rec classOf (classDef: ClassDef) (glyph: uint32) =
        match classDef with
        | ClassDefNull -> 0u
        | ClassDefDirect (slots, complete, source) ->
            match glyph < uint32 slots.Length with
            | true -> uint32 slots[int glyph]
            | false ->
                match complete with
                | true -> 0u
                | false -> classOf source glyph
        | ClassDefArray (startGlyph, classes) ->
            let offset = glyph - startGlyph
            match offset < uint32 classes.Length with
            | true -> classes[int offset]
            | false -> 0u
        | ClassDefRanges ranges ->
            match Coverage.bsearchRanges (ranges.AsSpan()) glyph 0 (ranges.Length - 1) with
            | ValueNone -> 0u
            | ValueSome i -> ranges[i].Value

    /// <summary>The fewest ranges a range class definition must hold for a direct table to be worth building.</summary>
    [<Literal>]
    let private DirectMinimum = 8

    /// <summary>The most glyph ids a direct table of one class definition holds; a glyph beyond it is answered by the class
    /// definition it was built from.</summary>
    [<Literal>]
    let private DirectLimit = 16384

    /// <summary>The most slots a direct table may hold for each range of its class definition.</summary>
    [<Literal>]
    let private DirectSlotsPerRange = 64

    /// <summary>The largest class a direct table slot holds.</summary>
    [<Literal>]
    let private DirectLargest = 0xFFFFu

    /// <summary>Compiles a range class definition into a direct table, built when a subtable of a lookup is decoded and immutable
    /// afterwards: the class of every glyph up to the last glyph a range ends at (or the limit), each read from the class definition
    /// itself, so that the table answers as the search does whatever order the file lists the ranges in. A class definition that is
    /// not a range form, is small, is sparse for its size, or names a class a slot cannot hold is returned as it is.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <returns>The class definition, with its table.</returns>
    let direct (classDef: ClassDef) =
        match classDef with
        | ClassDefNull
        | ClassDefArray _
        | ClassDefDirect _ -> classDef
        | ClassDefRanges ranges ->
            let last = match ranges.IsEmpty with | true -> 0u | false -> Seq.max (Seq.map (fun (range: RangeRecord) -> range.Last) ranges)
            let count = int (min (last + 1u) (uint32 DirectLimit))
            match ranges.Length >= DirectMinimum && count <= ranges.Length * DirectSlotsPerRange && Seq.forall (fun (range: RangeRecord) -> range.Value <= DirectLargest) ranges with
            | false -> classDef
            | true ->
                let slots = Sealed.init count (fun glyph -> uint16 (classOf classDef (uint32 glyph)))
                ClassDefDirect (slots, last < uint32 DirectLimit, classDef)

    /// <summary>Counts the entries of a class in an array ClassDef.</summary>
    /// <param name="classes">The class values.</param>
    /// <param name="klass">The class to collect.</param>
    /// <param name="index">The next entry.</param>
    /// <param name="total">The members so far.</param>
    /// <returns>The member count.</returns>
    [<TailCall>]
    let rec private countClassArray (classes: ReadOnlySpan<uint32>) (klass: uint32) (index) (total) =
        match index < classes.Length with
        | false -> total
        | true -> countClassArray classes klass (index + 1) (total + (match classes[index] = klass with | true -> 1 | false -> 0))

    /// <summary>Finds the next entry of a class at or after an index, or none.</summary>
    /// <param name="classes">The class values.</param>
    /// <param name="klass">The class to collect.</param>
    /// <param name="index">The next entry.</param>
    /// <returns>The entry index, or none.</returns>
    [<TailCall>]
    let rec private nextClassArray (classes: ReadOnlySpan<uint32>) (klass: uint32) (index) =
        match index < classes.Length with
        | false -> ValueNone
        | true ->
            match classes[index] = klass with
            | true -> ValueSome index
            | false -> nextClassArray classes klass (index + 1)

    /// <summary>Unfolds the entries of a class into add operations.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ClassArrayUnfolder =
        { StartGlyph: uint32
          Klass: uint32 }
        interface ISpanUnfolder<uint32, int, SetOp> with
            member this.Next (classes, index) =
                let next =
                    match nextClassArray classes this.Klass (index + 1) with
                    | ValueSome i -> i
                    | ValueNone -> classes.Length
                struct (SetOp.Add (this.StartGlyph + uint32 index, this.StartGlyph + uint32 index), next)

    /// <summary>Finds the first range of a class whose first is above its last, or the length.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="klass">The class to collect.</param>
    /// <param name="index">The next range.</param>
    /// <returns>The index of the first broken matching range, or the length when none.</returns>
    [<TailCall>]
    let rec private firstBrokenClass (ranges: ReadOnlySpan<RangeRecord>) (klass: uint32) (index) =
        match index < ranges.Length with
        | false -> ranges.Length
        | true ->
            match ranges[index].Value = klass && ranges[index].First > ranges[index].Last with
            | true -> index
            | false -> firstBrokenClass ranges klass (index + 1)

    /// <summary>Collects the glyphs of a class as sorted disjoint ranges, <c>ClassDef::collect_class</c>.</summary>
    /// <param name="classDef">The class definition.</param>
    /// <param name="klass">The class to collect.</param>
    /// <returns>The sorted disjoint ranges.</returns>
    let rec glyphsInClass (classDef: ClassDef) (klass: uint32) =
        match classDef with
        | ClassDefNull -> ImmutableArray.Empty
        | ClassDefDirect (_, _, source) -> glyphsInClass source klass
        | ClassDefArray (startGlyph, classes) ->
            let span = classes.AsSpan()
            match nextClassArray span klass 0 with
            | ValueNone -> ImmutableArray.Empty
            | ValueSome first ->
                let ops = Sealed.unfoldSpan span (countClassArray span klass 0 0) first { ClassArrayUnfolder.StartGlyph = startGlyph; Klass = klass }
                Sealed.rangeSet (ops.AsSpan())
        | ClassDefRanges ranges ->
            let span = ranges.AsSpan()
            let stop = firstBrokenClass span klass 0
            let ops = Sealed.choose (fun record -> match record.Value = klass with | true -> ValueSome (SetOp.Add (record.First, record.Last)) | false -> ValueNone) (span.Slice(0, stop))
            Sealed.rangeSet (ops.AsSpan())
