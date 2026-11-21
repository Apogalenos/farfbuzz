namespace Farfbuzz

open System

/// <summary>The parts of the OS/2 table the shaper and the metrics need.</summary>
[<Struct>]
type internal Os2 =
    { Present: bool
      Version: uint16
      FsSelection: uint16
      TypoAscender: int16
      TypoDescender: int16
      TypoLineGap: int16
      WinAscent: uint16
      WinDescent: uint16
      SxHeight: int16
      CapHeight: int16
      SubscriptXSize: int16
      SubscriptYSize: int16
      SubscriptXOffset: int16
      SubscriptYOffset: int16
      SuperscriptXSize: int16
      SuperscriptYSize: int16
      SuperscriptXOffset: int16
      SuperscriptYOffset: int16
      StrikeoutSize: int16
      StrikeoutPosition: int16 }

/// <summary>The OS/2 table: typographic metrics, selection flags and font page.</summary>
[<RequireQualifiedAccess>]
module internal Os2 =
    /// <summary>The size of the OS/2 base table.</summary>
    [<Literal>]
    let private BaseSize = 78

    /// <summary>The size of the version 1 tail.</summary>
    [<Literal>]
    let private V1Size = 8

    /// <summary>The size of the version 2 tail.</summary>
    [<Literal>]
    let private V2Size = 10

    /// <summary>The size of the version 5 tail.</summary>
    [<Literal>]
    let private V5Size = 4

    /// <summary>The offset of fsSelection.</summary>
    [<Literal>]
    let private SelectionOffset = 62

    /// <summary>The offset of usFirstCharIndex.</summary>
    [<Literal>]
    let private FirstCharOffset = 64

    /// <summary>The offset of usLastCharIndex.</summary>
    [<Literal>]
    let private LastCharOffset = 66

    /// <summary>The offset of sTypoAscender.</summary>
    [<Literal>]
    let private TypoAscenderOffset = 68

    /// <summary>The offset of usWinAscent.</summary>
    [<Literal>]
    let private WinAscentOffset = 74

    /// <summary>The offset of usWinDescent.</summary>
    [<Literal>]
    let private WinDescentOffset = 76

    /// <summary>The offset of the version 2 sxHeight.</summary>
    [<Literal>]
    let private SxHeightOffset = 86

    /// <summary>The offset of the version 2 sCapHeight.</summary>
    [<Literal>]
    let private CapHeightOffset = 88

    /// <summary>The offset of ySubscriptXSize.</summary>
    [<Literal>]
    let private SubscriptXSizeOffset = 10

    /// <summary>The offset of ySubscriptYSize.</summary>
    [<Literal>]
    let private SubscriptYSizeOffset = 12

    /// <summary>The offset of ySubscriptXOffset.</summary>
    [<Literal>]
    let private SubscriptXOffsetOffset = 14

    /// <summary>The offset of ySubscriptYOffset.</summary>
    [<Literal>]
    let private SubscriptYOffsetOffset = 16

    /// <summary>The offset of ySuperscriptXSize.</summary>
    [<Literal>]
    let private SuperscriptXSizeOffset = 18

    /// <summary>The offset of ySuperscriptYSize.</summary>
    [<Literal>]
    let private SuperscriptYSizeOffset = 20

    /// <summary>The offset of ySuperscriptXOffset.</summary>
    [<Literal>]
    let private SuperscriptXOffsetOffset = 22

    /// <summary>The offset of ySuperscriptYOffset.</summary>
    [<Literal>]
    let private SuperscriptYOffsetOffset = 24

    /// <summary>The offset of yStrikeoutSize.</summary>
    [<Literal>]
    let private StrikeoutSizeOffset = 26

    /// <summary>The offset of yStrikeoutPosition.</summary>
    [<Literal>]
    let private StrikeoutPositionOffset = 28

    /// <summary>The use-typo-metrics bit of fsSelection.</summary>
    [<Literal>]
    let private UseTypoMetricsBit = 0x0080us

    /// <summary>Sanitizes an OS/2 table, <c>OS2::sanitize</c>.</summary>
    /// <param name="table">The table bytes.</param>
    /// <param name="ops">The remaining operations.</param>
    /// <returns>The remaining operations, or none.</returns>
    let private sanitize (table: ReadOnlySpan<byte>) (ops) : int voption =
        let version = Binary.u16 table 0
        let tail =
            match version >= 5us, version >= 2us, version >= 1us with
            | true, _, _ -> int64 (V1Size + V2Size + V5Size)
            | false, true, _ -> int64 (V1Size + V2Size)
            | false, false, true -> int64 V1Size
            | false, false, false -> 0L
        match Sanitize.fits table.Length 0L BaseSize && Sanitize.fits table.Length (int64 BaseSize) (int tail) with
        | true -> ValueSome ops
        | false -> ValueNone

    /// <summary>The struct form of the OS/2 sanitizer.</summary>
    [<Struct>]
    type private Sanitizer =
        interface ITableSanitizer with
            member _.Sanitize (table, ops) = sanitize table ops

    /// <summary>Loads the OS/2 table.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The metrics, absent when the table does not sanitize.</returns>
    let load (face: Face) : Os2 =
        let table = Sanitize.table (Sanitizer()) (Face.referenceTable face Tags.Os2)
        let span = table.Span
        let version = Binary.u16 span 0
        let hasV2 = version >= 2us
        { Present =
            Binary.u16 span 4 <> 0us
            || Binary.u16 span 6 <> 0us
            || Binary.u16 span FirstCharOffset <> 0us
            || Binary.u16 span LastCharOffset <> 0us
          Version = version
          FsSelection = Binary.u16 span SelectionOffset
          TypoAscender = Binary.i16 span TypoAscenderOffset
          TypoDescender = Binary.i16 span (TypoAscenderOffset + 2)
          TypoLineGap = Binary.i16 span (TypoAscenderOffset + 4)
          WinAscent = Binary.u16 span WinAscentOffset
          WinDescent = Binary.u16 span WinDescentOffset
          SxHeight = match hasV2 with | true -> Binary.i16 span SxHeightOffset | false -> 0s
          CapHeight = match hasV2 with | true -> Binary.i16 span CapHeightOffset | false -> 0s
          SubscriptXSize = Binary.i16 span SubscriptXSizeOffset
          SubscriptYSize = Binary.i16 span SubscriptYSizeOffset
          SubscriptXOffset = Binary.i16 span SubscriptXOffsetOffset
          SubscriptYOffset = Binary.i16 span SubscriptYOffsetOffset
          SuperscriptXSize = Binary.i16 span SuperscriptXSizeOffset
          SuperscriptYSize = Binary.i16 span SuperscriptYSizeOffset
          SuperscriptXOffset = Binary.i16 span SuperscriptXOffsetOffset
          SuperscriptYOffset = Binary.i16 span SuperscriptYOffsetOffset
          StrikeoutSize = Binary.i16 span StrikeoutSizeOffset
          StrikeoutPosition = Binary.i16 span StrikeoutPositionOffset }

    /// <summary>Whether horizontal metrics should come from the typo fields, <c>OS2::use_typo_metrics</c>.</summary>
    /// <param name="os2">The OS/2 metrics.</param>
    /// <returns>True when the use-typo-metrics bit is set.</returns>
    let useTypoMetrics (os2: Os2) =
        os2.FsSelection &&& UseTypoMetricsBit <> 0us

    /// <summary>Returns the legacy font page, <c>OS2::get_font_page</c>.</summary>
    /// <param name="os2">The OS/2 metrics.</param>
    /// <returns>The font page, or 0 when the table is not version 0.</returns>
    let fontPage (os2: Os2) =
        match os2.Version with
        | 0us -> os2.FsSelection &&& 0xFF00us
        | _ -> 0us
