namespace Farfbuzz

open System
open System.Globalization

/// <summary>A value read from text and the offset just past it.</summary>
[<Struct>]
type internal Scanned<'T> = { Value: 'T; Next: int }

/// <summary>
/// HarfBuzz's character classes and number parsers (<c>hb-algs.hh</c>, <c>hb-number.cc</c>,
/// <c>hb-number-parser.hh</c>), ported exactly: same accepted inputs, same values, same stopping offsets.
/// </summary>
[<RequireQualifiedAccess>]
module internal Number =
    /// <summary>The white space HarfBuzz skips, <c>ISSPACE</c>.</summary>
    /// <param name="c">The byte.</param>
    /// <returns>True for space, form feed, newline, carriage return, tab and vertical tab.</returns>
    let inline isSpace (c: byte) =
        c = ' 'B || c = '\f'B || c = '\n'B || c = '\r'B || c = '\t'B || c = '\v'B

    /// <summary>ASCII letters, <c>ISALPHA</c>.</summary>
    /// <param name="c">The byte.</param>
    /// <returns>True for a to z and A to Z.</returns>
    let inline isAlpha (c: byte) =
        (c >= 'a'B && c <= 'z'B) || (c >= 'A'B && c <= 'Z'B)

    /// <summary>ASCII lower-casing, <c>TOLOWER</c>.</summary>
    /// <param name="c">The byte.</param>
    /// <returns>The lower-case letter, or the byte unchanged.</returns>
    let inline toLower (c: byte) =
        if c >= 'A'B && c <= 'Z'B then c + 32uy else c

    /// <summary>ASCII decimal digits.</summary>
    /// <param name="c">The byte.</param>
    /// <returns>True for 0 to 9.</returns>
    let inline isDigit (c: byte) =
        c >= '0'B && c <= '9'B

    /// <summary>Skips HarfBuzz white space.</summary>
    /// <param name="text">The input.</param>
    /// <param name="index">The first offset to look at.</param>
    /// <returns>The offset of the first non-space byte, or the length.</returns>
    [<TailCall>]
    let rec skipSpaces (text: ReadOnlySpan<byte>) (index: int) =
        if index < text.Length && isSpace text[index] then skipSpaces text (index + 1) else index

    /// <summary>Skips ASCII letters.</summary>
    /// <param name="text">The input.</param>
    /// <param name="index">The first offset to look at.</param>
    /// <returns>The offset of the first non-letter, or the length.</returns>
    [<TailCall>]
    let rec skipLetters (text: ReadOnlySpan<byte>) (index: int) =
        if index < text.Length && isAlpha text[index] then skipLetters text (index + 1) else index

    /// <summary>The value of a digit in a base, <c>_hb_digit_value</c>.</summary>
    /// <param name="c">The byte.</param>
    /// <param name="numberBase">The base, up to 36.</param>
    /// <returns>The digit value, or -1 when the byte is not a digit of that base.</returns>
    let digitValue (c: byte) (numberBase: int) =
        let value =
            match c with
            | _ when c >= '0'B && c <= '9'B -> int c - int '0'B
            | _ when c >= 'a'B && c <= 'z'B -> int c - int 'a'B + 10
            | _ when c >= 'A'B && c <= 'Z'B -> int c - int 'A'B + 10
            | _ -> -1
        if value >= 0 && value < numberBase then value else -1

    /// <summary>The outcome of scanning digits, <c>_hb_scan_digits</c>.</summary>
    [<Struct>]
    type Digits = { Next: int; Value: uint64; Overflow: bool; Any: bool }

    /// <summary>Accumulates digits with precomputed overflow limits.</summary>
    /// <param name="text">The input.</param>
    /// <param name="numberBase">The base.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="value">The value so far.</param>
    /// <param name="overflow">Whether the value overflowed.</param>
    /// <param name="any">Whether any digit was seen.</param>
    /// <param name="cutoff">The largest value that can be multiplied by the base.</param>
    /// <param name="cutlim">The largest digit allowed at the cutoff.</param>
    /// <returns>The scan result.</returns>
    [<TailCall>]
    let rec private scanDigitsFrom
            (text: ReadOnlySpan<byte>) (numberBase: int) (index: int) (value: uint64) (overflow: bool) (any: bool)
            (cutoff: uint64) (cutlim: uint64) : Digits =
        let digit = if index < text.Length then digitValue text[index] numberBase else -1
        match digit with
        | -1 -> { Next = index; Value = value; Overflow = overflow; Any = any }
        | d ->
            let overflows = value > cutoff || (value = cutoff && uint64 d > cutlim)
            let next = if overflows then value else value * uint64 numberBase + uint64 d
            scanDigitsFrom text numberBase (index + 1) next (overflow || overflows) true cutoff cutlim

    /// <summary>Accumulates digits of a base into a 64-bit value, flagging overflow.</summary>
    /// <param name="text">The input.</param>
    /// <param name="numberBase">The base.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="value">The value so far.</param>
    /// <param name="overflow">Whether the value overflowed.</param>
    /// <param name="any">Whether any digit was seen.</param>
    /// <returns>The scan result.</returns>
    let scanDigits (text: ReadOnlySpan<byte>) (numberBase: int) (index: int) (value: uint64) (overflow: bool) (any: bool) : Digits =
        let cutoff = UInt64.MaxValue / uint64 numberBase
        let cutlim = UInt64.MaxValue % uint64 numberBase
        scanDigitsFrom text numberBase index value overflow any cutoff cutlim

    /// <summary>A sign and magnitude read by <c>_parse_number</c>, before its range check.</summary>
    [<Struct>]
    type RawNumber = { Negative: bool; Magnitude: uint64; End: int }

    /// <summary>Reads optional spaces, an optional sign, an optional 0x prefix in base 16, and digits.</summary>
    /// <param name="text">The input.</param>
    /// <param name="start">The first offset.</param>
    /// <param name="numberBase">The base.</param>
    /// <returns>The raw number, or none when there are no digits or they overflow 64 bits.</returns>
    let scanNumber (text: ReadOnlySpan<byte>) (start: int) (numberBase: int) : RawNumber voption =
        let afterSpaces = skipSpaces text start
        let signed = afterSpaces < text.Length && (text[afterSpaces] = '+'B || text[afterSpaces] = '-'B)
        let negative = signed && text[afterSpaces] = '-'B
        let afterSign = if signed then afterSpaces + 1 else afterSpaces
        let hexPrefix =
            numberBase = 16
            && afterSign + 2 < text.Length
            && text[afterSign] = '0'B
            && (text[afterSign + 1] = 'x'B || text[afterSign + 1] = 'X'B)
            && digitValue text[afterSign + 2] 16 >= 0
        let digits = scanDigits text numberBase (if hexPrefix then afterSign + 2 else afterSign) 0UL false false
        match digits.Any, digits.Overflow with
        | true, false -> ValueSome { Negative = negative; Magnitude = digits.Value; End = digits.Next }
        | true, true -> ValueNone
        | false, _ -> ValueNone

    /// <summary>Parses a decimal <c>int</c>, <c>hb_parse_int</c>.</summary>
    /// <param name="text">The input.</param>
    /// <param name="start">The first offset.</param>
    /// <param name="wholeBuffer">Whether the number must reach the end of the input.</param>
    /// <returns>The value and the offset after it, or none.</returns>
    let parseInt (text: ReadOnlySpan<byte>) (start: int) (wholeBuffer: bool) : Scanned<int> voption =
        match scanNumber text start 10 with
        | ValueNone -> ValueNone
        | ValueSome raw ->
            let limit = if raw.Negative then uint64 Int32.MaxValue + 1UL else uint64 Int32.MaxValue
            match raw.Magnitude > limit, wholeBuffer && raw.End <> text.Length with
            | false, false -> ValueSome { Value = int (if raw.Negative then -(int64 raw.Magnitude) else int64 raw.Magnitude); Next = raw.End }
            | true, _ -> ValueNone
            | false, true -> ValueNone

    /// <summary>Parses an <c>unsigned</c> in a base, <c>hb_parse_uint</c>: a minus sign wraps, as strtoul does.</summary>
    /// <param name="text">The input.</param>
    /// <param name="start">The first offset.</param>
    /// <param name="wholeBuffer">Whether the number must reach the end of the input.</param>
    /// <param name="numberBase">The base.</param>
    /// <returns>The value and the offset after it, or none.</returns>
    let parseUInt (text: ReadOnlySpan<byte>) (start: int) (wholeBuffer: bool) (numberBase: int) : Scanned<uint32> voption =
        match scanNumber text start numberBase with
        | ValueNone -> ValueNone
        | ValueSome raw ->
            match raw.Magnitude > uint64 UInt32.MaxValue, wholeBuffer && raw.End <> text.Length with
            | false, false ->
                let magnitude = uint32 raw.Magnitude
                ValueSome { Value = (if raw.Negative then 0u - magnitude else magnitude); Next = raw.End }
            | true, _ -> ValueNone
            | false, true -> ValueNone

    /// <summary>The states of <c>hb_number_parser_t</c>.</summary>
    [<Struct>]
    type DoubleState =
        | Start
        | Sign
        | IntDigits
        | AfterDotFromInt
        | AfterDotFromStart
        | FracDigits
        | ExpStart
        | ExpSign
        | ExpDigits

    /// <summary>Fraction digits accumulated exactly up to 10^19, then remembered for rounding.</summary>
    [<Struct>]
    type Fraction = { Count: int; Digits: uint64; Drop: int; Sticky: bool }

    /// <summary>The whole state of the double parser.</summary>
    [<Struct>]
    type DoubleParser =
        { State: DoubleState
          PreExpState: DoubleState
          Value: double
          Exp: uint32
          Negative: bool
          ExpNegative: bool
          ExpOverflow: bool
          Speculating: bool
          Position: int
          PreExpPosition: int
          Fraction: Fraction }

    /// <summary>The parser before any byte.</summary>
    let initialParser =
        { State = Start; PreExpState = Start; Value = 0.0; Exp = 0u; Negative = false; ExpNegative = false
          ExpOverflow = false; Speculating = false; Position = 0; PreExpPosition = 0
          Fraction = { Count = 0; Digits = 0UL; Drop = -1; Sticky = false } }

    /// <summary>The largest fraction accumulated exactly, 10^19.</summary>
    [<Literal>]
    let MaxFraction = 10000000000000000000UL

    /// <summary>The largest exponent tracked before flagging overflow, 2^11 - 1.</summary>
    [<Literal>]
    let MaxExponent = 0x7FFu

    /// <summary>Adds one fraction digit, keeping the first dropped digit and whether any later one was non-zero.</summary>
    /// <param name="fraction">The fraction so far.</param>
    /// <param name="digit">The digit.</param>
    /// <returns>The new fraction.</returns>
    let addFraction (fraction: Fraction) (digit: int) =
        match fraction.Digits <= MaxFraction / 10UL, fraction.Drop < 0, digit <> 0 with
        | true, _, _ -> { fraction with Digits = fraction.Digits * 10UL + uint64 digit; Count = fraction.Count + 1 }
        | false, true, _ -> { fraction with Drop = digit }
        | false, false, true -> { fraction with Sticky = true }
        | false, false, false -> fraction

    /// <summary>Enters a speculative exponent, remembering where to roll back to.</summary>
    /// <param name="parser">The parser.</param>
    /// <returns>The parser in <c>ExpStart</c>.</returns>
    let enterExponent (parser: DoubleParser) =
        { parser with PreExpState = parser.State; PreExpPosition = parser.Position; Speculating = true; State = ExpStart; Position = parser.Position + 1 }

    /// <summary>Abandons a speculative exponent that turned out not to be one.</summary>
    /// <param name="parser">The parser.</param>
    /// <returns>The parser as it was before the exponent marker.</returns>
    let rollbackExponent (parser: DoubleParser) =
        match parser.Speculating with
        | false -> parser
        | true ->
            { parser with State = parser.PreExpState; Position = parser.PreExpPosition; Exp = 0u; ExpNegative = false; ExpOverflow = false; Speculating = false }

    /// <summary>Feeds one byte to the parser, <c>hb_number_parser_t::consume</c>.</summary>
    /// <param name="parser">The parser.</param>
    /// <param name="c">The byte.</param>
    /// <returns>Whether the byte was accepted, and the parser afterwards (a rejection may roll back an exponent).</returns>
    let consume (parser: DoubleParser) (c: byte) : struct (bool * DoubleParser) =
        let digit = int c - int '0'B
        let next = parser.Position + 1
        match parser.State with
        | Start when c = '+'B || c = '-'B -> struct (true, { parser with Negative = (c = '-'B); State = Sign; Position = next })
        | Start | Sign when isDigit c -> struct (true, { parser with Value = parser.Value * 10.0 + double digit; State = IntDigits; Position = next })
        | Start | Sign when c = '.'B -> struct (true, { parser with State = AfterDotFromStart; Position = next })
        | Start | Sign -> struct (false, parser)
        | IntDigits when isDigit c -> struct (true, { parser with Value = parser.Value * 10.0 + double digit; Position = next })
        | IntDigits when c = '.'B -> struct (true, { parser with State = AfterDotFromInt; Position = next })
        | IntDigits | AfterDotFromInt | FracDigits when c = 'e'B || c = 'E'B -> struct (true, enterExponent parser)
        | IntDigits -> struct (false, parser)
        | AfterDotFromInt | AfterDotFromStart | FracDigits when isDigit c ->
            struct (true, { parser with Fraction = addFraction parser.Fraction digit; State = FracDigits; Position = next })
        | AfterDotFromInt | AfterDotFromStart | FracDigits -> struct (false, parser)
        | ExpStart when c = '+'B || c = '-'B -> struct (true, { parser with ExpNegative = (c = '-'B); State = ExpSign; Position = next })
        | ExpStart | ExpSign when isDigit c -> struct (true, { parser with Exp = uint32 digit; State = ExpDigits; Position = next })
        | ExpStart | ExpSign -> struct (false, rollbackExponent parser)
        | ExpDigits when isDigit c ->
            let candidate = parser.Exp * 10u + uint32 digit
            match candidate <= MaxExponent with
            | true -> struct (true, { parser with Exp = candidate; Position = next })
            | false -> struct (true, { parser with ExpOverflow = true; Position = next })
        | ExpDigits -> struct (false, parser)

    /// <summary>Feeds bytes until the parser rejects one or the input ends.</summary>
    /// <param name="text">The input.</param>
    /// <param name="index">The next offset.</param>
    /// <param name="parser">The parser.</param>
    /// <returns>The parser when it stopped.</returns>
    [<TailCall>]
    let rec runDouble (text: ReadOnlySpan<byte>) (index: int) (parser: DoubleParser) =
        match index < text.Length with
        | false -> parser
        | true ->
            match consume parser text[index] with
            | struct (true, accepted) -> runDouble text (index + 1) accepted
            | struct (false, stopped) -> stopped

    /// <summary>Small powers of ten, exactly as <c>_small_powers_of_10</c>.</summary>
    let private smallPowers = [| 1e0; 1e1; 1e2; 1e3; 1e4; 1e5; 1e6; 1e7; 1e8; 1e9; 1e10; 1e11; 1e12; 1e13; 1e14; 1e15 |]

    /// <summary>Powers of ten in steps of 16, exactly as <c>_large_powers_of_10</c>.</summary>
    let private largePowers =
        [| 1e0; 1e16; 1e32; 1e48; 1e64; 1e80; 1e96; 1e112; 1e128; 1e144; 1e160; 1e176; 1e192; 1e208; 1e224; 1e240; 1e256; 1e272; 1e288; 1e304 |]

    /// <summary>Ten to a power, as HarfBuzz computes it: infinity above 308.</summary>
    /// <param name="n">The exponent.</param>
    /// <returns>The power.</returns>
    let pow10 (n: uint32) =
        match n with
        | _ when n > 308u -> Double.PositiveInfinity
        | _ when n < 16u -> smallPowers[int n]
        | _ -> largePowers[int (n >>> 4)] * smallPowers[int (n &&& 15u)]

    /// <summary>The value of the accumulated fraction, rounded half-to-even on the first dropped digit.</summary>
    /// <param name="fraction">The fraction.</param>
    /// <returns>Its value.</returns>
    let fractionValue (fraction: Fraction) =
        let roundsUp = fraction.Drop > 5 || (fraction.Drop = 5 && (fraction.Sticky || fraction.Digits &&& 1UL = 1UL))
        let digits = double fraction.Digits + (if roundsUp then 1.0 else 0.0)
        digits / pow10 (uint32 fraction.Count)

    /// <summary>Divides by a negative power of ten, splitting exponents above 308 as HarfBuzz does.</summary>
    /// <param name="value">The mantissa.</param>
    /// <param name="exponent">The exponent magnitude.</param>
    /// <returns>The scaled value.</returns>
    let divideByPower (value: double) (exponent: uint32) =
        let once = if exponent > 308u then struct (value / 1e308, exponent - 308u) else struct (value, exponent)
        let struct (v1, e1) = once
        let struct (v2, e2) = if e1 > 308u then struct (v1 / 1e308, e1 - 308u) else struct (v1, e1)
        v2 / pow10 e2

    /// <summary>The value of a committed parse, <c>hb_number_parser_t::get_value</c>.</summary>
    /// <param name="parser">The committed parser.</param>
    /// <returns>The double.</returns>
    let doubleValue (parser: DoubleParser) =
        let withFraction = if parser.Fraction.Count = 0 then parser.Value else parser.Value + fractionValue parser.Fraction
        let signed = if parser.Negative then withFraction * -1.0 else withFraction
        match signed = 0.0, parser.ExpOverflow, parser.ExpNegative, parser.Exp with
        | true, _, _, _ -> signed
        | false, true, true, _ -> if parser.Negative then -0.0 else 0.0
        | false, true, false, _ -> if parser.Negative then Double.NegativeInfinity else Double.PositiveInfinity
        | false, false, _, 0u -> signed
        | false, false, true, exponent -> divideByPower signed exponent
        | false, false, false, exponent -> signed * pow10 exponent

    /// <summary>Parses a double, <c>hb_parse_double</c>: leading spaces, then the longest valid prefix.</summary>
    /// <param name="text">The input.</param>
    /// <param name="start">The first offset.</param>
    /// <param name="wholeBuffer">Whether the number must reach the end of the input.</param>
    /// <returns>The value and the offset after it, or none.</returns>
    let parseDouble (text: ReadOnlySpan<byte>) (start: int) (wholeBuffer: bool) : Scanned<double> voption =
        let numberStart = skipSpaces text start
        let stopped = runDouble text numberStart initialParser
        let committed =
            match stopped.State with
            | ExpStart | ExpSign -> rollbackExponent stopped
            | Start | Sign | IntDigits | AfterDotFromInt | AfterDotFromStart | FracDigits | ExpDigits -> stopped
        let length =
            match committed.State with
            | Sign | AfterDotFromStart -> 0
            | Start | IntDigits | AfterDotFromInt | FracDigits | ExpStart | ExpSign | ExpDigits -> committed.Position
        match length, wholeBuffer && numberStart + length <> text.Length with
        | 0, _ -> ValueNone
        | _, true -> ValueNone
        | _, false -> ValueSome { Value = doubleValue committed; Next = numberStart + length }

    /// <summary>Removes trailing zeros after a decimal point, and the point itself when nothing follows.</summary>
    /// <param name="digits">A fixed or mantissa representation.</param>
    /// <returns>The trimmed representation.</returns>
    let trimFraction (digits: string) =
        match digits.Contains '.' with
        | false -> digits
        | true -> digits.TrimEnd('0').TrimEnd('.')

    /// <summary>Formats a double as C's <c>printf("%g")</c> in the C locale: six significant digits.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The text, for example <c>0.25</c>, <c>500</c>, <c>1e+06</c> or <c>-inf</c>.</returns>
    let formatG (value: double) =
        match value with
        | _ when Double.IsNaN value -> if Double.IsNegative value then "-nan" else "nan"
        | _ when Double.IsPositiveInfinity value -> "inf"
        | _ when Double.IsNegativeInfinity value -> "-inf"
        | _ when value = 0.0 -> if Double.IsNegative value then "-0" else "0"
        | _ ->
            let scientific = value.ToString("E5", CultureInfo.InvariantCulture)
            let exponentAt = scientific.IndexOf 'E'
            let exponent = Int32.Parse(scientific.AsSpan(exponentAt + 1), NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)
            match exponent >= -4 && exponent < 6 with
            | true -> trimFraction (value.ToString("F" + string (5 - exponent), CultureInfo.InvariantCulture))
            | false ->
                let digits = scientific.Substring(0, exponentAt)
                let mantissa = trimFraction digits
                let sign = if exponent < 0 then "-" else "+"
                mantissa + "e" + sign + (abs exponent).ToString("00", CultureInfo.InvariantCulture)
