namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>A glyph range and its coverage value or class, as read from a Coverage or ClassDef table.</summary>
[<Struct>]
type internal RangeRecord =
    { First: uint32
      Last: uint32
      Value: uint32 }

/// <summary>A coverage table, decoded once into native arrays in file order.</summary>
type internal Coverage =
    | CoverageNull
    | CoverageGlyphs of glyphs: ImmutableArray<uint32>
    | CoverageRanges of coverageRanges: ImmutableArray<RangeRecord>
    | CoverageDirect of coverageSlots: ImmutableArray<uint16> * coverageComplete: bool * coverageSource: Coverage

/// <summary>The result of a coverage lookup: the covered index, or not covered.</summary>
[<Struct>]
type internal CoverageHit =
    | Covered of coverageIndex: int
    | NotCovered

/// <summary>Coverage formats 1-4: decoding, binary search, digest and glyph collection.</summary>
[<RequireQualifiedAccess>]
module internal Coverage =
    /// <summary>The byte width of a 16-bit glyph id or value.</summary>
    [<Literal>]
    let private Width16 = 2

    /// <summary>The byte width of a 24-bit glyph id or value.</summary>
    [<Literal>]
    let private Width24 = 3

    /// <summary>Reads a big-endian value of a given byte width.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The byte offset.</param>
    /// <param name="width">The width, two or three bytes.</param>
    /// <returns>The value.</returns>
    let internal readWide (table: ReadOnlySpan<byte>) (offset) (width) =
        match width with
        | 2 -> uint32 (Binary.u16 table offset)
        | _ -> Binary.u24 table offset

    /// <summary>Sanitizes a 16-bit glyph array, <c>CoverageFormat1_3&lt;SmallTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeGlyphs16 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset + 2L) Width16 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 4L) (uint32 (Binary.u16 table (offset + 2)) * 2u) ops

    /// <summary>Sanitizes a 16-bit range array, <c>CoverageFormat2_4&lt;SmallTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeRanges16 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset + 2L) Width16 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 4L) (uint32 (Binary.u16 table (offset + 2)) * 6u) ops

    /// <summary>Sanitizes a 24-bit glyph array, <c>CoverageFormat1_3&lt;MediumTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeGlyphs24 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset + 2L) Width24 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 5L) (Binary.u24 table (offset + 2) * 3u) ops

    /// <summary>Sanitizes a 24-bit range array, <c>CoverageFormat2_4&lt;MediumTypes&gt;::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitizeRanges24 (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset + 2L) Width24 with
        | false -> ValueNone
        | true -> Sanitize.range table.Length (int64 offset + 5L) (Binary.u24 table (offset + 2) * 9u) ops

    /// <summary>Sanitizes a coverage, <c>Coverage::sanitize</c>.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let sanitize (table: ReadOnlySpan<byte>) (offset) (ops) =
        match Sanitize.fits table.Length (int64 offset) Width16 with
        | false -> ValueNone
        | true ->
            match Binary.u16 table offset with
            | 1us -> sanitizeGlyphs16 table offset ops
            | 2us -> sanitizeRanges16 table offset ops
            | 3us -> sanitizeGlyphs24 table offset ops
            | 4us -> sanitizeRanges24 table offset ops
            | _ -> ValueSome ops

    /// <summary>Decodes a glyph array of one width into native glyph ids.</summary>
    [<Struct>]
    type private GlyphDecoder =
        { Base: int
          Width: int }
        interface ISpanUnfolder<byte, int, uint32> with
            member this.Next (table, index) =
                let offset = this.Base + index * this.Width
                struct (readWide table offset this.Width, index + 1)

    /// <summary>Decodes a range array of given widths into native range records.</summary>
    [<Struct>]
    type private RangeDecoder =
        { Base: int
          GlyphWidth: int
          ValueWidth: int }
        interface ISpanUnfolder<byte, int, RangeRecord> with
            member this.Next (table, index) =
                let offset = this.Base + index * (2 * this.GlyphWidth + this.ValueWidth)
                let first = readWide table offset this.GlyphWidth
                let last = readWide table (offset + this.GlyphWidth) this.GlyphWidth
                let value = readWide table (offset + 2 * this.GlyphWidth) this.ValueWidth
                struct ({ First = first; Last = last; Value = value }, index + 1)

    /// <summary>Decodes a coverage into native arrays in file order.</summary>
    /// <param name="table">The bytes.</param>
    /// <param name="offset">The subtable offset.</param>
    /// <returns>The coverage.</returns>
    let parse (table: ReadOnlySpan<byte>) (offset) =
        match Binary.u16 table offset with
        | 1us ->
            CoverageGlyphs (Sealed.unfoldSpan table (int (Binary.u16 table (offset + 2))) 0 { GlyphDecoder.Base = offset + 4; Width = Width16 })
        | 2us ->
            CoverageRanges (Sealed.unfoldSpan table (int (Binary.u16 table (offset + 2))) 0 { RangeDecoder.Base = offset + 4; GlyphWidth = Width16; ValueWidth = Width16 })
        | 3us ->
            CoverageGlyphs (Sealed.unfoldSpan table (int (Binary.u24 table (offset + 2))) 0 { GlyphDecoder.Base = offset + 5; Width = Width24 })
        | 4us ->
            CoverageRanges (Sealed.unfoldSpan table (int (Binary.u24 table (offset + 2))) 0 { RangeDecoder.Base = offset + 5; GlyphWidth = Width24; ValueWidth = Width24 })
        | _ -> CoverageNull

    /// <summary>Binary-searches a decoded glyph array, <c>hb_bsearch_impl</c>.</summary>
    /// <param name="glyphs">The glyphs.</param>
    /// <param name="glyph">The key.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private bsearchGlyphs (glyphs: ReadOnlySpan<uint32>) (glyph: uint32) (lo) (hi) =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match glyph > glyphs[mid], glyph < glyphs[mid] with
            | true, _ -> bsearchGlyphs glyphs glyph (mid + 1) hi
            | false, true -> bsearchGlyphs glyphs glyph lo (mid - 1)
            | false, false -> ValueSome mid

    /// <summary>Binary-searches a decoded range array, <c>RangeRecord::cmp</c> over <c>hb_bsearch_impl</c>.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="glyph">The key.</param>
    /// <param name="lo">The first candidate index.</param>
    /// <param name="hi">The last candidate index.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec internal bsearchRanges (ranges: ReadOnlySpan<RangeRecord>) (glyph: uint32) (lo) (hi) =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match glyph < ranges[mid].First, glyph <= ranges[mid].Last with
            | true, _ -> bsearchRanges ranges glyph lo (mid - 1)
            | false, true -> ValueSome mid
            | false, false -> bsearchRanges ranges glyph (mid + 1) hi

    /// <summary>The slot a direct coverage stores for a glyph that is not covered.</summary>
    [<Literal>]
    let private NoSlot = 0xFFFFus

    /// <summary>Classifies a glyph against a coverage, <c>Coverage::get_coverage</c>.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>The covered index, or not covered.</returns>
    [<TailCall>]
    let rec index (coverage: Coverage) (glyph: uint32) =
        match coverage with
        | CoverageNull -> NotCovered
        | CoverageDirect (slots, complete, source) ->
            match glyph < uint32 slots.Length with
            | true ->
                match slots[int glyph] with
                | NoSlot -> NotCovered
                | slot -> Covered (int slot)
            | false ->
                match complete with
                | true -> NotCovered
                | false -> index source glyph
        | CoverageGlyphs glyphs ->
            match bsearchGlyphs (glyphs.AsSpan()) glyph 0 (glyphs.Length - 1) with
            | ValueSome i -> Covered i
            | ValueNone -> NotCovered
        | CoverageRanges ranges ->
            match bsearchRanges (ranges.AsSpan()) glyph 0 (ranges.Length - 1) with
            | ValueNone -> NotCovered
            | ValueSome i ->
                let record = ranges[i]
                match record.First <= record.Last with
                | true -> Covered (int (record.Value + (glyph - record.First)))
                | false -> NotCovered

    /// <summary>The fewest glyphs or ranges a coverage must hold for a direct table to be worth building: below it the binary
    /// search takes a step or two.</summary>
    [<Literal>]
    let private DirectMinimum = 8

    /// <summary>The most glyph ids a direct table of one coverage holds; a glyph beyond it is answered by the coverage it was built
    /// from.</summary>
    [<Literal>]
    let private DirectLimit = 16384

    /// <summary>The most slots a direct table may hold for each glyph or range of its coverage, so that a sparse coverage does not
    /// get a table many times its own size.</summary>
    [<Literal>]
    let private DirectSlotsPerEntry = 16

    /// <summary>The last glyph a coverage can cover, and its number of entries: the largest glyph of its array, or the largest end of
    /// its ranges, so a glyph beyond it is not covered whatever order the file lists them in.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The glyph, and the number of entries.</returns>
    let private extent (coverage: Coverage) =
        match coverage with
        | CoverageNull
        | CoverageDirect _ -> struct (0u, 0)
        | CoverageGlyphs glyphs -> struct ((match glyphs.IsEmpty with | true -> 0u | false -> Seq.max glyphs), glyphs.Length)
        | CoverageRanges ranges -> struct ((match ranges.IsEmpty with | true -> 0u | false -> Seq.max (Seq.map (fun (range: RangeRecord) -> range.Last) ranges)), ranges.Length)

    /// <summary>Compiles a coverage into a direct table, built when a subtable of a lookup is decoded and immutable afterwards: the
    /// covered index of every glyph up to the last glyph the coverage can cover (or the limit), each read from the coverage itself, so
    /// that the table answers as the search does whatever order the file lists the glyphs in. A coverage that is small, or sparse for its
    /// size, or has an index that does not fit a slot, is returned as it is.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The coverage, with its table.</returns>
    let direct (coverage: Coverage) =
        let struct (last, entries) = extent coverage
        let count = int (min (last + 1u) (uint32 DirectLimit))
        match entries >= DirectMinimum && count <= entries * DirectSlotsPerEntry with
        | false -> coverage
        | true ->
            let answers = Sealed.init count (fun glyph -> match index coverage (uint32 glyph) with | Covered covered -> covered | NotCovered -> -1)
            match Seq.exists (fun answer -> answer >= int NoSlot) answers with
            | true -> coverage
            | false ->
                let slots = Sealed.map (fun answer -> match answer < 0 with | true -> NoSlot | false -> uint16 answer) (answers.AsSpan())
                CoverageDirect (slots, last < uint32 DirectLimit, coverage)

    /// <summary>Adds each glyph of a coverage to a digest, <c>add_array</c>.</summary>
    [<Struct>]
    type private GlyphDigestFolder =
        interface IFolder<Digest, uint32> with
            member _.Step state glyph = Digest.add glyph state

    /// <summary>Adds each range of a coverage to a digest, <c>add_range</c>.</summary>
    [<Struct>]
    type private RangeDigestFolder =
        interface IFolder<Digest, RangeRecord> with
            member _.Step state record = Digest.addRange record.First record.Last state

    /// <summary>Builds the coverage digest the subtable accelerators use, <c>collect_coverage</c>.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The digest.</returns>
    let rec digest (coverage: Coverage) =
        match coverage with
        | CoverageNull -> Digest.empty
        | CoverageDirect (_, _, source) -> digest source
        | CoverageGlyphs glyphs -> Spans.foldWith (GlyphDigestFolder()) Digest.empty (glyphs.AsSpan())
        | CoverageRanges ranges -> Spans.foldWith (RangeDigestFolder()) Digest.empty (ranges.AsSpan())

    /// <summary>The initial iteration state of a range coverage, or none for an empty or broken-first table.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <returns>The range index, the first glyph and the running coverage index, or none.</returns>
    let private initIter (ranges: ReadOnlySpan<RangeRecord>) =
        match ranges.Length = 0 || ranges[0].First > ranges[0].Last with
        | true -> ValueNone
        | false -> ValueSome (struct (0, ranges[0].First, 0u))

    /// <summary>Advances the range iteration by one glyph, <c>iter_t::__next__</c>.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="index">The current range index.</param>
    /// <param name="glyph">The current glyph.</param>
    /// <param name="coverage">The running coverage index.</param>
    /// <returns>The next range index, glyph and coverage index.</returns>
    let private stepIter (ranges: ReadOnlySpan<RangeRecord>) (index) (glyph: uint32) (coverage: uint32) =
        match glyph < ranges[index].Last with
        | true -> struct (index, glyph + 1u, coverage + 1u)
        | false ->
            let next = index + 1
            match next < ranges.Length && ranges[next].Value = coverage + 1u with
            | true -> struct (next, ranges[next].First, ranges[next].Value)
            | false -> struct (ranges.Length, 0u, 0u)

    /// <summary>Counts the glyphs the range iteration yields.</summary>
    /// <param name="ranges">The ranges.</param>
    /// <param name="index">The current range index.</param>
    /// <param name="glyph">The current glyph.</param>
    /// <param name="coverage">The running coverage index.</param>
    /// <param name="total">The yields so far.</param>
    /// <returns>The total yields.</returns>
    [<TailCall>]
    let rec private countIter (ranges: ReadOnlySpan<RangeRecord>) (index) (glyph: uint32) (coverage: uint32) (total) =
        match index < ranges.Length with
        | false -> total
        | true ->
            let struct (nextIndex, nextGlyph, nextCoverage) = stepIter ranges index glyph coverage
            countIter ranges nextIndex nextGlyph nextCoverage (total + 1)

    /// <summary>Unfolds the range iteration into glyphs.</summary>
    [<Struct>]
    type private RangeIterUnfolder =
        interface ISpanUnfolder<RangeRecord, struct (int * uint32 * uint32), uint32> with
            member _.Next (ranges, state) =
                let struct (index, glyph, coverage) = state
                let next = stepIter ranges index glyph coverage
                struct (glyph, next)

    /// <summary>Collects every glyph a coverage covers, in upstream's <c>iter_t</c> order.</summary>
    /// <param name="coverage">The coverage.</param>
    /// <returns>The glyphs.</returns>
    let rec glyphs (coverage: Coverage) =
        match coverage with
        | CoverageNull -> ImmutableArray.Empty
        | CoverageDirect (_, _, source) -> glyphs source
        | CoverageGlyphs glyphs -> glyphs
        | CoverageRanges ranges ->
            let span = ranges.AsSpan()
            match initIter span with
            | ValueNone -> ImmutableArray.Empty
            | ValueSome (struct (index, glyph, coverage)) ->
                Sealed.unfoldSpan span (countIter span index glyph coverage 0) (struct (index, glyph, coverage)) (RangeIterUnfolder())

/// <summary>The coverage active pattern, shared with the layout matching code.</summary>
[<AutoOpen>]
module internal CoveragePatterns =
    /// <summary>Coverage::get_coverage as a total classification.</summary>
    /// <returns>The covered index, or not covered.</returns>
    let (|CoverageOf|) (struct (coverage: Coverage, glyph: uint32)) : CoverageHit =
        Coverage.index coverage glyph
