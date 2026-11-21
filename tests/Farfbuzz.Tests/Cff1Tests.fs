module Farfbuzz.Tests.Cff1Tests

open System
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Loads a CFF1 accelerator from a test font.</summary>
/// <param name="relative">The font path under <c>test/api/fonts</c>.</param>
/// <returns>The accelerator.</returns>
let private cffOf (relative: string) =
    Cff1.load (Face.create (read $"test/api/fonts/{relative}") 0u)

/// <summary>Returns a glyph name as text.</summary>
/// <param name="cff">The accelerator.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The name, or an empty string.</returns>
let private nameOf (cff: Cff1Accel) (glyph: uint32) =
    match Cff1.glyphName cff glyph with
    | ValueSome name -> Encoding.Latin1.GetString(name.Span)
    | ValueNone -> String.Empty

/// <summary>Builds a CFF INDEX with one-byte offsets.</summary>
/// <param name="items">The item bytes.</param>
/// <returns>The INDEX bytes.</returns>
let private index (items: byte array list) : byte array =
    match items with
    | [] -> be16 0u
    | _ ->
        let data = items |> Array.concat
        let offsets =
            items
            |> List.scan (fun offset item -> offset + item.Length) 1
            |> List.map byte
            |> List.toArray
        Array.concat [ be16 (uint32 items.Length); [| 1uy |]; offsets; data ]

/// <summary>Builds a minimal CFF table with a crafted top DICT.</summary>
/// <param name="top">The top DICT bytes.</param>
/// <returns>The CFF table bytes.</returns>
let private cffWithTop (top: byte array) : byte array =
    let header = [| 1uy; 0uy; 4uy; 4uy |]
    let name = Encoding.ASCII.GetBytes "Test"
    let nameIndex = index [ name ]
    let topIndex = index [ top ]
    let stringIndex = index []
    let charStrings = index []
    Array.concat [ header; nameIndex; topIndex; stringIndex; charStrings ]

/// <summary>Builds a minimal CFF with private operands in the top DICT.</summary>
/// <param name="privateOperands">The operands and the Private operator.</param>
/// <returns>The CFF table bytes.</returns>
let private cffWithPrivate (privateOperands: byte array) : byte array =
    let topLength = privateOperands.Length + 4
    let charStringsOffset = 20 + topLength
    let charStrings =
        Array.concat [ [| 28uy; byte (charStringsOffset >>> 8); byte charStringsOffset; 17uy |] ]
    cffWithTop (Array.concat [ privateOperands; charStrings ])

/// <summary>Builds a CFF with a private DICT after the CharStrings INDEX.</summary>
/// <param name="privateDict">The private DICT bytes.</param>
/// <returns>The CFF table bytes.</returns>
let private cffWithPrivateDict (privateDict: byte array) : byte array =
    let privateOffset = 33
    let charStringsOffset = 31
    let top =
        Array.concat
            [ [| 28uy; 0uy; byte privateDict.Length
                 28uy; 0uy; byte privateOffset
                 18uy
                 28uy; 0uy; byte charStringsOffset; 17uy |] ]
    Array.concat [ cffWithTop top; privateDict ]

/// <summary>Builds a CFF with a custom charset offset in the top DICT.</summary>
/// <param name="charsetOffset">The charset offset.</param>
/// <returns>The CFF table bytes.</returns>
let private cffWithCharset (charsetOffset: int) : byte array =
    let charset = [| byte (charsetOffset + 139) |]
    let topLength = charset.Length + 1 + 4
    let charStringsOffset = 20 + topLength
    let top =
        Array.concat
            [ charset
              [| 15uy; 28uy; byte (charStringsOffset >>> 8); byte charStringsOffset; 17uy |] ]
    cffWithTop top

/// <summary>Builds a face around a CFF table with no glyphs.</summary>
/// <param name="cff">The CFF table bytes.</param>
/// <returns>The face.</returns>
let private cffFace (cff: byte array) : Face =
    let maxp = [| 0uy; 0uy; 0x50uy; 0uy; 0uy; 0uy |]
    Face.create (ReadOnlyMemory (sfnt 0x4F54544Fu [ Tags.Cff, cff; Tags.Maxp, maxp ])) 0u

/// <summary>CFF1 charsets and names, P2.S10.</summary>
[<Tests>]
let tests =
    testList "P2.S10" [
        test "standard and custom CFF names" {
            let cff = cffOf "MathTestFontFull.otf"
            Expect.isTrue cff.Valid "valid"
            Expect.isFalse cff.IsCid "non-CID"
            Expect.equal (nameOf cff 0u) ".notdef" "glyph 0"
            Expect.equal (nameOf cff 27u) "Z" "glyph 27"
            Expect.equal (nameOf cff 46u) "arrowdblright" "glyph 46"
            Expect.equal (nameOf cff 138u) "uni21E7_size5" "glyph 138"
        }
        test "the Expert charset uses predefined names" {
            let cff = cffOf "cff1_expert.otf"
            Expect.isTrue cff.Valid "valid"
            Expect.equal (nameOf cff 0u) ".notdef" "glyph 0"
            Expect.equal (nameOf cff 29u) "centsuperior" "glyph 29"
            Expect.equal (nameOf cff 86u) "commainferior" "glyph 86"
        }
        test "CFF names round-trip through the name index" {
            let cff = cffOf "MathTestFontFull.otf"
            for glyph in [ 0u; 27u; 46u; 138u ] do
                let name = nameOf cff glyph
                match Cff1.glyphFromName cff (ReadOnlySpan(Encoding.Latin1.GetBytes name)) with
                | ValueSome found -> Expect.equal (nameOf cff found) name "round trip"
                | ValueNone -> Expect.isTrue false $"missing name for {glyph}"
        }
        test "an absent CFF name misses" {
            let cff = cffOf "MathTestFontFull.otf"
            Expect.equal (Cff1.glyphFromName cff ReadOnlySpan<byte>.Empty) ValueNone "empty"
            Expect.equal (Cff1.glyphFromName cff (ReadOnlySpan(Encoding.ASCII.GetBytes "nosuchname"))) ValueNone "absent"
        }
    ]

/// <summary>The P2.R01 CFF validity cases.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R01" [
        test "a negative private size is invalid" {
            let cff = cffWithPrivate [| 0x8Auy; 0x95uy; 0x12uy |]
            Expect.isFalse (Cff1.load (cffFace cff)).Valid "invalid"
        }
        test "a private offset near Int32.MaxValue is invalid" {
            let cff =
                cffWithPrivate
                    [| 0x95uy; 29uy; 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x12uy |]
            Expect.isFalse (Cff1.load (cffFace cff)).Valid "invalid"
        }
        test "Private with too few operands is invalid" {
            let cff = cffWithPrivate [| 0x12uy |]
            Expect.isFalse (Cff1.load (cffFace cff)).Valid "invalid"
        }
        test "a private Subrs without an operand is invalid" {
            let cff = cffWithPrivateDict [| 19uy |]
            Expect.isFalse (Cff1.load (cffFace cff)).Valid "invalid"
        }
        test "a private size outside the int range is invalid" {
            let cff = cffWithPrivate [| 30uy; 0x1Buy; 0x10uy; 0xFFuy; 0x95uy; 0x12uy |]
            Expect.isFalse (Cff1.load (cffFace cff)).Valid "invalid"
        }
        test "a custom charset that fails sanitising invalidates the accelerator" {
            let cff = cffWithCharset 100
            Expect.isFalse (Cff1.load (cffFace cff)).Valid "invalid"
        }
    ]
