module Farfbuzz.Tests.NumberTests

open System
open System.Globalization
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open System.Numerics

/// <summary>Parses a double from a string as HarfBuzz would, returning the value and how much was consumed.</summary>
/// <param name="text">The input.</param>
/// <returns>The value and the consumed length, or none.</returns>
let double (text: string) =
    match Number.parseDouble (ReadOnlySpan(Encoding.UTF8.GetBytes text)) 0 false with
    | ValueSome scanned -> Some (scanned.Value, scanned.Next)
    | ValueNone -> None

/// <summary>Parses an int from a string as HarfBuzz would.</summary>
/// <param name="text">The input.</param>
/// <param name="whole">Whether the number must use the whole input.</param>
/// <returns>The value and the consumed length, or none.</returns>
let integer (text: string) (whole: bool) =
    match Number.parseInt (ReadOnlySpan(Encoding.UTF8.GetBytes text)) 0 whole with
    | ValueSome scanned -> Some (scanned.Value, scanned.Next)
    | ValueNone -> None

/// <summary>Number parsing and %g formatting, P1.Number.</summary>
[<Tests>]
let tests =
    testList "P1.Number" [
        testList "parseDouble" [
            test "integers and fractions" { Expect.equal (double "1.5") (Some (1.5, 3)) "1.5" }
            test "a leading dot" { Expect.equal (double "-.5") (Some (-0.5, 3)) "-.5" }
            test "a trailing dot" { Expect.equal (double "2.") (Some (2.0, 2)) "2." }
            test "an exponent" { Expect.equal (double "1e3") (Some (1000.0, 3)) "1e3" }
            test "a dangling exponent is rolled back" { Expect.equal (double "1e") (Some (1.0, 1)) "1e" }
            test "a dangling exponent sign is rolled back" { Expect.equal (double "7e+x") (Some (7.0, 1)) "7e+x" }
            test "leading spaces are skipped" { Expect.equal (double "  .25 ") (Some (0.25, 5)) "spaces" }
            test "a sign alone is not a number" { Expect.equal (double "-") None "-" }
            test "a dot alone is not a number" { Expect.equal (double ".") None "." }
            test "zero with a huge exponent is zero" { Expect.equal (double "0e441") (Some (0.0, 5)) "0e441" }
            test "a huge exponent overflows to infinity" { Expect.equal (double "1e4000") (Some (Double.PositiveInfinity, 6)) "1e4000" }
            testProperty "short decimals parse within one ULP of the nearest double, as hb-number-parser.hh promises" <| fun (whole: int16) (fraction: byte) ->
                let text = $"{whole}.{fraction}"
                let nearest = Double.Parse(text, Globalization.CultureInfo.InvariantCulture)
                match double text with
                | Some (value, consumed) -> consumed = text.Length && abs (BitConverter.DoubleToInt64Bits value - BitConverter.DoubleToInt64Bits nearest) <= 1L
                | None -> false
        ]
        testList "parseInt" [
            test "a signed decimal" { Expect.equal (integer " -42x" false) (Some (-42, 4)) "-42" }
            test "the int range" { Expect.equal (integer "2147483648" false, integer "-2147483648" false) (None, Some (Int32.MinValue, 11)) "range" }
            test "whole-buffer parsing rejects trailing text" { Expect.equal (integer "12a" true) None "12a" }
        ]
        testList "formatG" [
            for value, expected in
                [ 0.25, "0.25"; 500.0, "500"; 1e6, "1e+06"; 123456789.0, "1.23457e+08"; 0.0001, "0.0001"
                  0.00001, "1e-05"; -7.5, "-7.5"; 100000.0, "100000"; 999999.5, "1e+06"; 0.0, "0"
                  Double.PositiveInfinity, "inf"; Double.NegativeInfinity, "-inf"; 1.0 / 3.0, "0.333333"
                  8599801.0, "8.5998e+06"; 1234500.0, "1.2345e+06"; 1234995.0, "1.235e+06" ] ->
                test $"%%g of {value:R} is {expected}" { Expect.equal (Number.formatG value) expected "%g" }
            yield test "an exact tie always uses the C-standard trailing-zero form" {
                let cases =
                    [ 8599805.0, "8.5998e+06"; 1000005.0, "1e+06"; 12345050.0, "1.2345e+07"
                      -8599805.0, "-8.5998e+06" ]
                for value, expected in cases do
                    Expect.equal (Number.formatG value) expected "%g"
            }
        ]
    ]

/// <summary>The P2.R10 digit-limit cases.</summary>
[<Tests>]
let cursorReviewTests =
    let scan (numberBase: int) (text: string) =
        Number.scanDigits (ReadOnlySpan(Encoding.ASCII.GetBytes text)) numberBase 0 0UL false false
    testList "P2.R10" [
        test "the 64-bit boundary is exact in base 10" {
            let max = scan 10 "18446744073709551615"
            Expect.equal (max.Overflow, max.Value, max.Next, max.Any) (false, UInt64.MaxValue, 20, true) "max"
            let over = scan 10 "18446744073709551616"
            Expect.equal (over.Overflow, over.Next, over.Any) (true, 20, true) "overflow"
        }
        test "the 64-bit boundary is exact in base 16" {
            let max = scan 16 "FFFFFFFFFFFFFFFF"
            Expect.equal (max.Overflow, max.Value, max.Next, max.Any) (false, UInt64.MaxValue, 16, true) "max"
            let over = scan 16 "FFFFFFFFFFFFFFFFF"
            Expect.equal (over.Overflow, over.Next, over.Any) (true, 17, true) "overflow"
        }
        testProperty "scanDigits agrees with BigInteger in bases 10 and 16" <|
            let digit = Gen.elements [ '0' .. '9' ]
            let hexDigit = Gen.elements ([ '0' .. '9' ] @ [ 'A' .. 'F' ])
            let input =
                gen {
                    let! numberBase = Gen.elements [ 10; 16 ]
                    let! length = Gen.choose (0, 40)
                    let! digits = Gen.listOfLength length (if numberBase = 16 then hexDigit else digit)
                    return numberBase, String(List.toArray digits)
                }
            Prop.forAll (Arb.fromGen input) (fun (numberBase, text) ->
                let expected =
                    match text with
                    | "" -> BigInteger.Zero
                    | _ -> BigInteger.Parse((if numberBase = 16 then "0" + text else text), if numberBase = 16 then NumberStyles.HexNumber else NumberStyles.None)
                let actual = scan numberBase text
                let overflow = expected > BigInteger UInt64.MaxValue
                let valueOk = overflow || actual.Value = uint64 expected
                actual.Next = text.Length && actual.Any = (text.Length > 0) && actual.Overflow = overflow && valueOk)
    ]
