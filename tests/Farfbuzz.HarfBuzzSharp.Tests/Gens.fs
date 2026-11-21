module Farfbuzz.HarfBuzzSharp.Tests.Gens

open System
open FsCheck
open FsCheck.FSharp

/// <summary>A string of up to <paramref name="maximum"/> characters drawn from an alphabet.</summary>
/// <param name="alphabet">The characters.</param>
/// <param name="maximum">The longest length.</param>
/// <returns>The generator.</returns>
let noise (alphabet: char list) (maximum: int) =
    gen {
        let! length = Gen.choose (0, maximum)
        let! chars = Gen.listOfLength length (Gen.elements alphabet)
        return String(List.toArray chars)
    }

/// <summary>Letters and digits.</summary>
let alphanumeric = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ]

/// <summary>Generates any 32-bit value.</summary>
let anyUInt32 = Gen.map (fun (value: int) -> uint32 value) (Gen.choose (Int32.MinValue, Int32.MaxValue))

/// <summary>Generates any signed 32-bit value.</summary>
let anyInt32 = Gen.choose (Int32.MinValue, Int32.MaxValue)

/// <summary>Feature strings shaped like the grammar, with frequent near-misses.</summary>
let featureText =
    let structured =
        gen {
            let! sign = Gen.elements [ String.Empty; "-"; "+"; " -"; "--" ]
            let! quote = Gen.elements [ String.Empty; String.Empty; "'"; "\"" ]
            let! tag = noise (alphanumeric @ [ ' '; '_'; '\''; ']'; ':'; '.'; 'é' ]) 5
            let! range = Gen.elements [ String.Empty; String.Empty; "[]"; "[:]"; "[3]"; "[3:5]"; "[:5]"; "[5:]"; "[ 2 ; 4 ]"; "[-1]"; "[x]"; "[1"; "[2147483648]"; "[0:-1]" ]
            let! value = Gen.elements [ String.Empty; String.Empty; "=0"; "=1"; "=2"; "=-1"; " 3"; "=on"; " off"; "=ON"; " xyz"; "="; "=4294967295"; " = 7"; "=1x" ]
            let! tail = Gen.elements [ String.Empty; String.Empty; " "; "x"; " ,"; "\n" ]
            return sign + quote + tag + quote + range + value + tail
        }
    Gen.frequency [ 4, structured; 1, noise (alphanumeric @ [ '-'; '+'; '='; '['; ']'; ':'; ';'; '\''; '"'; ' '; '\t' ]) 12 ]

/// <summary>Draws a fixed number of values from a generator.</summary>
/// <param name="count">The number of values.</param>
/// <param name="generator">The generator.</param>
/// <returns>The values.</returns>
let sample (count: int) (generator: Gen<'T>) : 'T list =
    Gen.sample count generator |> Seq.toList
