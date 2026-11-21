module Farfbuzz.Diff.TextFormatDiff

open System
open System.Text
open Farfbuzz.Oracle
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Diff.Differential

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

/// <summary>Feature strings shaped like the grammar, with frequent near-misses.</summary>
let featureText =
    let structured =
        gen {
            let! lead = Gen.elements [ String.Empty; " "; "  "; "\t" ]
            let! sign = Gen.elements [ String.Empty; "-"; "+"; " -"; "--" ]
            let! quote = Gen.elements [ String.Empty; String.Empty; "'"; "\"" ]
            let! tag = noise (alphanumeric @ [ ' '; '_'; '\''; ']'; ':'; '.'; 'é' ]) 5
            let! closed = Gen.elements [ true; true; false ]
            let! range = Gen.elements [ String.Empty; String.Empty; "[]"; "[:]"; "[3]"; "[3:5]"; "[:5]"; "[5:]"; "[ 2 ; 4 ]"; "[-1]"; "[x]"; "[1"; "[2147483648]"; "[0:-1]"; "[ 7 ]" ]
            let! value = Gen.elements [ String.Empty; String.Empty; "=0"; "=1"; "=2"; "=-1"; " 3"; "=on"; " off"; "=ON"; " xyz"; "="; "=2147483648"; "=4294967295"; " = 7"; "=1x"; "on" ]
            let! tail = Gen.elements [ String.Empty; String.Empty; " "; "x"; " ,"; "\n" ]
            return lead + sign + quote + tag + (if quote <> String.Empty && closed then quote else String.Empty) + range + value + tail
        }
    Gen.frequency [ 4, structured; 1, noise (alphanumeric @ [ '-'; '+'; '='; '['; ']'; ':'; ';'; '\''; '"'; ' '; '\t' ]) 12 ]

/// <summary>Number spellings, including the precision and exponent edge cases of HarfBuzz's double parser.</summary>
let numberText =
    let digits maximum = noise [ '0' .. '9' ] maximum
    let composed =
        gen {
            let! sign = Gen.elements [ String.Empty; String.Empty; "-"; "+" ]
            let! whole = digits 22
            let! dot = Gen.elements [ String.Empty; "."; "." ]
            let! fraction = digits 24
            let! exponent = Gen.elements [ String.Empty; String.Empty; "e"; "E"; "e+"; "e-"; "e3"; "e-5"; "e308"; "e-320"; "e400"; "e+0012" ]
            return sign + whole + dot + fraction + exponent
        }
    Gen.frequency [ 5, composed; 1, Gen.elements [ String.Empty; "."; "-"; "+"; "-."; "1e"; "0e441"; "-0"; "1e-400"; "4.9e-324"; "1.7976931348623157e308"; "x" ] ]

/// <summary>Variation strings: a tag, an optional separator and a number, with near-misses.</summary>
let variationText =
    gen {
        let! lead = Gen.elements [ String.Empty; " " ]
        let! quote = Gen.elements [ String.Empty; String.Empty; "'" ]
        let! tag = noise alphanumeric 5
        let! separator = Gen.elements [ String.Empty; "="; "="; " "; " = "; "==" ]
        let! number = numberText
        let! tail = Gen.elements [ String.Empty; String.Empty; " "; "x" ]
        return lead + quote + tag + quote + separator + number + tail
    }

/// <summary>Arbitrary strings of up to six characters, NUL and non-ASCII included, for tag conversion.</summary>
let tagText = noise (alphanumeric @ [ ' '; '\000'; 'é'; '€'; '-' ]) 6

/// <summary>Features with tags of printable ASCII and values chosen to hit every branch of the formatter.</summary>
let anyFeature =
    gen {
        let! tag = Gen.listOfLength 4 (Gen.elements (alphanumeric @ [ ' ' ]))
        let! value = Gen.frequency [ 3, Gen.elements [ 0u; 1u; 2u; UInt32.MaxValue ]; 1, Gen.map uint32 (Gen.choose (0, Int32.MaxValue)) ]
        let! start = Gen.frequency [ 2, Gen.elements [ 0u; 1u; 5u; UInt32.MaxValue ]; 1, Gen.map uint32 (Gen.choose (0, Int32.MaxValue)) ]
        let! stop = Gen.frequency [ 2, Gen.elements [ 0u; start + 1u; 5u; UInt32.MaxValue ]; 1, Gen.map uint32 (Gen.choose (0, Int32.MaxValue)) ]
        return { Tag = Tag.ofString (String(List.toArray tag)); Value = value; Start = start; End = stop }
    }

/// <summary>Variations with any non-NaN float value, by bit pattern and by round numbers.</summary>
let anyVariation =
    gen {
        let! tag = Gen.listOfLength 4 (Gen.elements alphanumeric)
        let! bits = Gen.choose (Int32.MinValue, Int32.MaxValue)
        let! round = Gen.choose (-100000, 100000)
        let! useBits = Gen.elements [ true; false ]
        let fromBits = BitConverter.Int32BitsToSingle bits
        let value = if useBits && not (Single.IsNaN fromBits) then fromBits else float32 round / 4.0f
        return { Tag = Tag.ofString (String(List.toArray tag)); Value = value }
    }

/// <summary>Glyph runs with values reaching the int range limits, to exercise saturation.</summary>
let glyphRun =
    gen {
        let extreme = Gen.frequency [ 6, Gen.choose (-3000, 3000); 1, Gen.elements [ Int32.MinValue; Int32.MaxValue; -1; 0 ] ]
        let! count = Gen.choose (0, 10)
        let! items =
            Gen.listOfLength count (
                gen {
                    let! glyph = Gen.frequency [ 5, Gen.choose (0, 70000); 1, Gen.elements [ Int32.MaxValue; 0 ] ]
                    let! cluster = Gen.choose (0, 100000)
                    let! mask = Gen.choose (Int32.MinValue, Int32.MaxValue)
                    let! xAdvance = extreme
                    let! yAdvance = Gen.frequency [ 3, Gen.constant 0; 1, extreme ]
                    let! xOffset = Gen.frequency [ 2, Gen.constant 0; 1, extreme ]
                    let! yOffset = Gen.frequency [ 2, Gen.constant 0; 1, extreme ]
                    return
                        { Codepoint = uint32 glyph; Mask = uint32 mask; Cluster = uint32 cluster; Var1 = 0u; Var2 = 0u },
                        { XAdvance = xAdvance; YAdvance = yAdvance; XOffset = xOffset; YOffset = yOffset; Var = 0u }
                })
        let! flags = Gen.choose (0, 63)
        let infos, positions = List.unzip items
        return LanguagePrimitives.EnumOfValue<uint32, SerializeFlags> (uint32 flags), List.toArray infos, List.toArray positions
    }

/// <summary>Serialised glyph strings: valid ones from the serialiser, and mutations of them.</summary>
let glyphText =
    let valid =
        gen {
            let! flags, infos, positions = glyphRun
            let named = flags &&& ~~~SerializeFlags.GlyphExtents
            return Serialize.glyphs named (fun _ -> ValueNone) (fun _ -> ValueNone) (ReadOnlySpan infos) (ReadOnlySpan positions) |> TailCalls.settle
        }
    let mutated =
        gen {
            let! text = valid
            let! at = Gen.choose (0, max 0 text.Length)
            let! edit = Gen.elements [ 0; 1; 2 ]
            let! inserted = Gen.elements [ '['; ']'; '|'; '='; '@'; '+'; ','; '#'; '<'; '>'; '-'; '0'; '7'; 'g'; 'u'; '\\'; ' '; 'x' ]
            return
                match edit, at < text.Length with
                | 0, _ -> text.Insert(at, string inserted)
                | 1, true -> text.Remove(at, 1)
                | 2, true -> text.Remove(at, 1).Insert(at, string inserted)
                | _ -> text + string inserted
        }
    Gen.frequency [ 2, valid; 3, mutated; 1, Gen.elements [ String.Empty; "["; "[]"; "[gid5]"; "[uni0041]"; "[5"; "[5="; "a\\"; "[1=0<1,2,3,4>]"; "[1+5,0|2]x" ] ]

/// <summary>Farfbuzz's feature parse as an option.</summary>
/// <param name="text">The feature string.</param>
/// <returns>The feature, or none.</returns>
let ourFeature text =
    match Feature.parse text with
    | Ok feature -> ValueSome feature
    | Error _ -> ValueNone

/// <summary>Farfbuzz's variation parse, as bits so that negative zero is told apart.</summary>
/// <param name="text">The variation string.</param>
/// <returns>The tag and value bits, or none.</returns>
let ourVariation text =
    match Variation.parse text with
    | Ok variation -> ValueSome (variation.Tag, BitConverter.SingleToInt32Bits variation.Value)
    | Error _ -> ValueNone

/// <summary>The oracle's variation parse, as bits.</summary>
/// <param name="text">The variation string.</param>
/// <returns>The tag and value bits, or none.</returns>
let oracleVariation text =
    match NativeText.variationFromString text with
    | ValueSome variation -> ValueSome (variation.Tag, BitConverter.SingleToInt32Bits variation.Value)
    | ValueNone -> ValueNone

/// <summary>Farfbuzz's glyph deserialisation as arrays.</summary>
/// <param name="text">The serialised glyphs.</param>
/// <returns>The glyphs and positions, or none.</returns>
let ourGlyphs text =
    match Serialize.parseGlyphs (fun _ -> ValueNone) (fun _ -> ValueNone) text with
    | Ok (struct (infos, positions)) -> ValueSome (infos.AsSpan().ToArray(), positions.AsSpan().ToArray())
    | Error _ -> ValueNone

/// <summary>Awkward feature bytes: single grammar bytes and whole number spellings.</summary>
let private rawFeatureBytes =
    let single =
        [| ' 'B; '\t'B; 0uy; '\''B; '"'B; '-'B; '+'B; '['B; ']'B; ':'B; ';'B; '='B
           '0'B; '1'B; '5'B; '9'B; 'k'B; 'e'B; 'r'B; 'n'B; 'o'B; 'f'B; 'O'B; 'N'B; 'F'B; 'x'B; 0xE9uy; '\v'B |]
    let numbers =
        [ "2147483647"; "2147483648"; "-2147483648"; "-2147483649"; "4294967295"
          "99999999999999999999999"; "0000000000000000000000001"; "+0"; "-0"; "+"; "-" ]
    let piece =
        Gen.frequency
            [ 8, Gen.elements single |> Gen.map (fun value -> [| value |])
              1, Gen.elements numbers |> Gen.map Encoding.ASCII.GetBytes ]
    gen {
        let! count = Gen.choose (0, 8)
        let! pieces = Gen.listOfLength count piece
        return Array.concat pieces
    }

/// <summary>Reads a feature through the cursor stages alone.</summary>
/// <param name="bytes">The UTF-8 input.</param>
/// <returns>The feature, or none.</returns>
let private cursorFeature (bytes: byte array) =
    Feature.Stages.tryParse (ReadOnlySpan bytes) |> TailCalls.settle

/// <summary>Reads a feature through the oracle from raw bytes.</summary>
/// <param name="bytes">The UTF-8 input.</param>
/// <returns>The feature, or none.</returns>
let private oracleFeatureBytes (bytes: byte array) =
    let mutable feature = Unchecked.defaultof<Feature>
    match Native.hb_feature_from_string(bytes, bytes.Length, &feature) with
    | 0 -> ValueNone
    | _ -> ValueSome feature

/// <summary>Removes Darwin's trailing fraction zeros from an oracle %g mantissa.</summary>
/// <param name="text">The oracle text.</param>
/// <returns>The C-standard text.</returns>
let private normalizeOracle (text: string) =
    match OperatingSystem.IsMacOS() with
    | false -> text
    | true ->
        let equalsAt = text.IndexOf '='
        let exponentAt = text.IndexOf('e', equalsAt + 1)
        match exponentAt < 0 with
        | true -> text
        | false ->
            let mantissa = text.Substring(0, exponentAt)
            let trimmed =
                match mantissa.Contains '.' with
                | true -> mantissa.TrimEnd('0').TrimEnd('.')
                | false -> mantissa
            trimmed + text.Substring(exponentAt)

/// <summary>Differential tests of HarfBuzz's text formats, P1.TextFormats.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "TextFormats" [
        testList "P1.TextFormats" [
        sampled "hb_tag_from_string" tagText DefaultCount (fun text -> uint32 (Tag.ofString text)) NativeText.tagFromString
        sampled "hb_feature_from_string" featureText DefaultCount ourFeature NativeText.featureFromString
        sampled "hb_feature_to_string" anyFeature DefaultCount Feature.format NativeText.featureToString
        sampled "hb_variation_from_string" variationText DefaultCount ourVariation oracleVariation
        sampled "hb_variation_to_string" anyVariation DefaultCount Variation.format (NativeText.variationToString >> normalizeOracle)
        testCase "the %g normalisation finds the exponent after the equals sign" (fun () ->
            let variation = { Tag = Tag.ofString "Wqes"; Value = 1861205.0f }
            Expect.equal (normalizeOracle (NativeText.variationToString variation)) (Variation.format variation) "the 'e' of the tag is not the exponent")
        sampled "hb_buffer_serialize_glyphs (text)" glyphRun DefaultCount
            (fun (flags, infos, positions) -> Serialize.glyphs flags (fun _ -> ValueNone) (fun _ -> ValueNone) (ReadOnlySpan infos) (ReadOnlySpan positions) |> TailCalls.settle)
            (fun (flags, infos, positions) -> NativeText.serializeGlyphs flags infos positions)
        sampled "hb_buffer_deserialize_glyphs (text)" glyphText DefaultCount ourGlyphs NativeText.deserializeGlyphs
        testCase "deserialise drops an overflowing number at the end" (fun () ->
            let text = "[1=0|2=1@0,21474836477"
            Expect.equal (ourGlyphs text) (NativeText.deserializeGlyphs text) text)
        ]
        testList "P2.R10" [
            sampled "cursor feature from strings" featureText DefaultCount (fun text -> cursorFeature (Encoding.UTF8.GetBytes text)) NativeText.featureFromString
            sampled "cursor feature from raw bytes" rawFeatureBytes DefaultCount cursorFeature oracleFeatureBytes
            testCase "cursor ranges" (fun () ->
                let ranges =
                    [ "[3]"; "[3:]"; "[:5]"; "[;5]"; "[]"; "[ 3 ; 5 ]"; "[-1]"; "[3" ]
                for text in ranges do
                    Expect.equal (cursorFeature (Encoding.UTF8.GetBytes text)) (NativeText.featureFromString text) text)
        ]
    ]
