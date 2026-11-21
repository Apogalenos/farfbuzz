module Farfbuzz.Tests.UcdTablesTests

open Expecto
open Farfbuzz

/// <summary>The P3.S03 UCD table spot checks.</summary>
[<Tests>]
let tests =
    testList "P3.S03" [
        test "general categories" {
            Expect.equal (UcdTables.gc 0x41u) 9uy "A is Lu"
            Expect.equal (UcdTables.gc 0x61u) 5uy "a is Ll"
            Expect.equal (UcdTables.gc 0x110000u) 2uy "outside is Cn"
        }
        test "combining classes and mirroring" {
            Expect.equal (UcdTables.ccc 0x0301u) 230uy "acute"
            Expect.equal (UcdTables.bmg 0x28u) 1 "left parenthesis mirrors to +1"
            Expect.equal (UcdTables.bmg 0x29u) -1 "right parenthesis mirrors to -1"
        }
        test "scripts use the generated order" {
            let script = UcdTables.scripts[int (UcdTables.sc 0x0627u)]
            Expect.equal script Scripts.Arabic "Arabic"
            Expect.equal (UcdTables.sc 0x110000u) 2uy "outside uses Zzzz index"
        }
    ]

/// <summary>The P3.S03 generator round-trip check over every code point.</summary>
[<Tests>]
let roundTrip =
    testList "P3.S03 round trip" [
        test "packed properties match the grouped XML" {
            let path = System.IO.Path.Combine(Farfbuzz.Harness.RepositoryPaths.root (), "tools", "data", "ucd", "ucd.nounihan.grouped.zip")
            match Farfbuzz.Gen.UcdXml.load path with
            | Error error -> Expect.isTrue false error
            | Ok (struct (description, records)) ->
                Expect.equal description "Unicode 18.0.0" "description"
                let mutable first = None
                for codepoint in 0 .. records.Length - 1 do
                    let record = records[codepoint]
                    let mirror = match record.Bmg with | ValueSome value -> int value - codepoint | ValueNone -> 0
                    if first.IsNone && (UcdTables.gc (uint32 codepoint) <> byte (match record.Gc with | "Cc" -> 0uy | "Cf" -> 1uy | "Cn" -> 2uy | "Co" -> 3uy | "Cs" -> 4uy | "Ll" -> 5uy | "Lm" -> 6uy | "Lo" -> 7uy | "Lt" -> 8uy | "Lu" -> 9uy | "Mc" -> 10uy | "Me" -> 11uy | "Mn" -> 12uy | "Nd" -> 13uy | "Nl" -> 14uy | "No" -> 15uy | "Pc" -> 16uy | "Pd" -> 17uy | "Pe" -> 18uy | "Pf" -> 19uy | "Pi" -> 20uy | "Po" -> 21uy | "Ps" -> 22uy | "Sc" -> 23uy | "Sk" -> 24uy | "Sm" -> 25uy | "So" -> 26uy | "Zl" -> 27uy | "Zp" -> 28uy | _ -> 29uy) || UcdTables.ccc (uint32 codepoint) <> byte record.Ccc || UcdTables.bmg (uint32 codepoint) <> mirror || UcdTables.scripts[int (UcdTables.sc (uint32 codepoint))] <> Script.ofString record.Sc) then
                        first <- Some codepoint
                Expect.equal first None "first differing code point"
        }
    ]
