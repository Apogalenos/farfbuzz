namespace Farfbuzz.OpenType

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>The sanitized ltag table, holding the language-tag strings.</summary>
[<Struct; NoComparison; NoEquality>]
type internal Ltag =
    { Table: ReadOnlyMemory<byte>
      Count: int }

/// <summary>Reading the ltag language-tag table, <c>AAT::ltag</c> (src/hb-aat-ltag-table.hh).</summary>
[<RequireQualifiedAccess>]
module internal Ltag =
    /// <summary>The size of the ltag header.</summary>
    [<Literal>]
    let private HeaderSize = 12

    /// <summary>The size of one FTStringRange record.</summary>
    [<Literal>]
    let private RangeSize = 4

    /// <summary>The longest tag <c>hb_language_from_string</c> keeps, <c>63</c>.</summary>
    [<Literal>]
    let private MaxLanguageLength = 63

    /// <summary>The empty ltag table, returned when the table is absent or fails to sanitize.</summary>
    let private empty : Ltag = { Table = ReadOnlyMemory<byte>.Empty; Count = 0 }

    /// <summary>Sanitizes every language-tag string range, <c>FTStringRange::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="index">The next range.</param>
    /// <param name="count">The range count.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none when a string fails.</returns>
    [<TailCall>]
    let rec private sanitizeRanges (table: ReadOnlySpan<byte>) (index: int) (count: int) (ops: int) : int voption =
        match index >= count with
        | true -> ValueSome ops
        | false ->
            let at = HeaderSize + index * RangeSize
            let tagOffset = int64 (Binary.u16 table at)
            let length = Binary.u16 table (at + 2)
            match Sanitize.range table.Length tagOffset (uint32 length) ops with
            | ValueNone -> ValueNone
            | ValueSome remaining -> sanitizeRanges table (index + 1) count remaining

    /// <summary>Sanitizes an ltag table, <c>ltag::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops: int) : int voption =
        match Sanitize.fits table.Length 0 HeaderSize with
        | false -> ValueNone
        | true ->
            match Binary.u32 table 0 >= 1u with
            | false -> ValueNone
            | true ->
                let count = Binary.u32 table 8
                match Sanitize.array table.Length HeaderSize count (uint32 RangeSize) ops with
                | ValueNone -> ValueNone
                | ValueSome remaining -> sanitizeRanges table 0 (int count) remaining

    /// <summary>The struct form of the ltag sanitizer.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Loads and sanitizes the ltag table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The language tags, empty when the table is absent or fails.</returns>
    let load (face: Face) : Ltag =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Ltag)
        match table.Length with
        | 0 -> empty
        | _ ->
            { Table = table
              Count = int (Binary.u32 table.Span 8) }

    /// <summary>Whether the table has data, <c>ltag has data</c>.</summary>
    /// <param name="ltag">The table.</param>
    /// <returns>True when the table loaded.</returns>
    let hasData (ltag: Ltag) = ltag.Table.Length > 0

    /// <summary>Reads the canonical language string at an index, <c>'\0'</c> past its end.</summary>
    /// <param name="text">The canonical string.</param>
    /// <param name="index">The index.</param>
    /// <returns>The character, or the NUL terminator.</returns>
    let private charAt (text: string) (index: int) : char =
        match index < text.Length with
        | true -> text[index]
        | false -> '\000'

    /// <summary>Compares a language tag against the canonical string, <c>matches_language</c>'s loop.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="tagAt">The string offset.</param>
    /// <param name="length">The bounded string length.</param>
    /// <param name="text">The canonical language string.</param>
    /// <param name="l">The next byte index.</param>
    /// <returns>True when the tag matches the language.</returns>
    [<TailCall>]
    let rec private matchBytes (table: ReadOnlyMemory<byte>) (tagAt: int) (length: int) (text: string) (l: int) : bool =
        match l < length with
        | false -> l > 0 && (charAt text l = '\000' || charAt text l = '-')
        | true ->
            let c = Language.canonicalByte (Binary.u8 table.Span (tagAt + l))
            match c with
            | 0uy -> l > 0 && (charAt text l = '\000' || charAt text l = '-')
            | c ->
                match byte (charAt text l) = c with
                | false -> false
                | true -> matchBytes table tagAt length text (l + 1)

    /// <summary>Whether a language tag matches a specific language, <c>ltag::matches_language</c>.</summary>
    /// <param name="ltag">The table.</param>
    /// <param name="index">The language-tag index.</param>
    /// <param name="specific">The specific language.</param>
    /// <returns>True when the tag matches.</returns>
    let matches (ltag: Ltag) (index: int) (specific: Language) : bool =
        match Language.toString specific with
        | ValueNone -> false
        | ValueSome text ->
            let span = ltag.Table.Span
            let rangeAt = HeaderSize + index * RangeSize
            let tagAt = int (Binary.u16 span rangeAt)
            let length = min (int (Binary.u16 span (rangeAt + 2))) MaxLanguageLength
            matchBytes ltag.Table tagAt length text 0
