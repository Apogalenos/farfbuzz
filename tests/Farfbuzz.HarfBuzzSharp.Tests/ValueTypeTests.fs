module Farfbuzz.HarfBuzzSharp.Tests.ValueTypeTests

open System
open System.Runtime.InteropServices
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Diff.Differential
open Farfbuzz.Oracle
open Farfbuzz.HarfBuzzSharp.Tests

/// <summary>Generates core glyph infos over the whole range of every field.</summary>
let private anyInfo =
    gen {
        let! codepoint = Gens.anyUInt32
        let! mask = Gens.anyUInt32
        let! cluster = Gens.anyUInt32
        let! var1 = Gens.anyUInt32
        let! var2 = Gens.anyUInt32
        return { Codepoint = codepoint; Mask = mask; Cluster = cluster; Var1 = var1; Var2 = var2 }
    }

/// <summary>Generates features with tags of printable ASCII and values over the whole range.</summary>
let private anyFeature =
    gen {
        let! tag = Gen.listOfLength 4 (Gen.elements (Gens.alphanumeric @ [ ' ' ]))
        let! value = Gens.anyUInt32
        let! start = Gens.anyUInt32
        let! stop = Gens.anyUInt32
        return { Tag = Tag.ofString (String(List.toArray tag)); Value = value; Start = start; End = stop }
    }

/// <summary>Generates core glyph positions over the whole range of every field.</summary>
let private anyPosition =
    gen {
        let! xAdvance = Gens.anyInt32
        let! yAdvance = Gens.anyInt32
        let! xOffset = Gens.anyInt32
        let! yOffset = Gens.anyInt32
        let! var = Gens.anyUInt32
        return { XAdvance = xAdvance; YAdvance = yAdvance; XOffset = xOffset; YOffset = yOffset; Var = var }
    }

/// <summary>Reads the feature a facade string parses to, as the core struct.</summary>
/// <param name="text">The feature string.</param>
/// <returns>The feature, or none when the facade rejects the string.</returns>
let private facadeFeature (text: string) : Feature voption =
    match HarfBuzzSharp.Feature.TryParse text with
    | true, feature -> ValueSome (MemoryMarshal.Cast<HarfBuzzSharp.Feature, Feature>(ReadOnlySpan<HarfBuzzSharp.Feature>([| feature |])).ToArray()[0])
    | false, _ -> ValueNone

/// <summary>Hands the core feature to the facade struct by reinterpreting its bytes.</summary>
/// <param name="feature">The core feature.</param>
/// <returns>The facade feature.</returns>
let private asFacadeFeature (feature: Feature) : HarfBuzzSharp.Feature =
    MemoryMarshal.Cast<Feature, HarfBuzzSharp.Feature>(ReadOnlySpan<Feature>([| feature |])).ToArray()[0]

/// <summary>Lists the feature strings on which the facade and the oracle disagree, in parsing and in formatting.</summary>
/// <param name="strings">The feature strings.</param>
/// <returns>One line per disagreement.</returns>
let private featureMismatches (strings: string list) : string list =
    strings
    |> List.choose (fun text ->
        let expected = NativeText.featureFromString text
        let actual = facadeFeature text
        let formatted = actual |> ValueOption.map (fun feature -> let facade = asFacadeFeature feature in facade.ToString())
        let oracleFormatted = expected |> ValueOption.map NativeText.featureToString
        match actual = expected && formatted = oracleFormatted with
        | true -> None
        | false -> Some $"{text}: oracle {expected} {oracleFormatted}, facade {actual} {formatted}")

/// <summary>The P10.S02 cases: value types, enums and the parity of their layout with the core.</summary>
[<Tests>]
let tests =
    testList "P10.S02" [
        test "the facade structs keep the native sizes" {
            Expect.equal (Marshal.SizeOf<HarfBuzzSharp.GlyphInfo>()) 20 "GlyphInfo"
            Expect.equal (Marshal.SizeOf<HarfBuzzSharp.GlyphPosition>()) 20 "GlyphPosition"
            Expect.equal (Marshal.SizeOf<HarfBuzzSharp.Feature>()) 16 "Feature"
        }
        testPropertyWithConfig (config 1000) "glyph infos survive a cast to the facade struct and back" (Prop.forAll (Arb.fromGen (Gen.nonEmptyListOf anyInfo)) (fun infos ->
            let core = List.toArray infos
            let facade = MemoryMarshal.Cast<GlyphInfo, HarfBuzzSharp.GlyphInfo>(ReadOnlySpan<GlyphInfo>(core)).ToArray()
            let fieldsAgree =
                Array.forall2 (fun (c: GlyphInfo) (f: HarfBuzzSharp.GlyphInfo) -> c.Codepoint = f.Codepoint && c.Mask = f.Mask && c.Cluster = f.Cluster) core facade
            let back = MemoryMarshal.Cast<HarfBuzzSharp.GlyphInfo, GlyphInfo>(ReadOnlySpan<HarfBuzzSharp.GlyphInfo>(facade)).ToArray()
            fieldsAgree && back = core))
        testPropertyWithConfig (config 1000) "glyph positions survive a cast to the facade struct and back" (Prop.forAll (Arb.fromGen (Gen.nonEmptyListOf anyPosition)) (fun positions ->
            let core = List.toArray positions
            let facade = MemoryMarshal.Cast<GlyphPosition, HarfBuzzSharp.GlyphPosition>(ReadOnlySpan<GlyphPosition>(core)).ToArray()
            let fieldsAgree =
                Array.forall2 (fun (c: GlyphPosition) (f: HarfBuzzSharp.GlyphPosition) -> c.XAdvance = f.XAdvance && c.YAdvance = f.YAdvance && c.XOffset = f.XOffset && c.YOffset = f.YOffset) core facade
            let back = MemoryMarshal.Cast<HarfBuzzSharp.GlyphPosition, GlyphPosition>(ReadOnlySpan<HarfBuzzSharp.GlyphPosition>(facade)).ToArray()
            fieldsAgree && back = core))
        testPropertyWithConfig (config 1000) "features survive a cast to the facade struct and back" (Prop.forAll (Arb.fromGen anyFeature) (fun feature ->
            let facade = asFacadeFeature feature
            uint32 feature.Tag = uint32 facade.Tag && feature.Value = facade.Value && feature.Start = facade.Start && feature.End = facade.End))
        testCase "Feature.Parse and ToString equal the oracle on 1 000 generated strings" (fun () ->
            OracleSession.ensure ()
            let report = featureMismatches (Gens.sample 1000 Gens.featureText)
            Expect.isEmpty report (String.concat "\n" (List.truncate 10 report)))
        testCase "Feature.Parse and ToString equal the oracle on every feature string of the shape suites" (fun () ->
            OracleSession.ensure ()
            let strings = SuiteData.optionValues "--features" |> List.collect (fun text -> text.Split ',' |> Array.toList) |> List.distinct
            Expect.isGreaterThan strings.Length 20 "the suites carry feature strings"
            let report = featureMismatches strings
            Expect.isEmpty report (String.concat "\n" (List.truncate 10 report)))
        test "Feature.Parse throws on a string the oracle rejects and the constructors set the whole range" {
            OracleSession.ensure ()
            let rejected = Gens.sample 400 Gens.featureText |> List.filter (fun text -> ValueOption.isNone (NativeText.featureFromString text)) |> List.truncate 20
            Expect.isGreaterThan rejected.Length 5 "the generator reaches rejected strings"
            for text in rejected do
                Expect.throwsT<FormatException> (fun () -> HarfBuzzSharp.Feature.Parse text |> ignore) $"FormatException for {text}"
            let feature = HarfBuzzSharp.Feature(HarfBuzzSharp.Tag.Parse "liga")
            Expect.equal (feature.Value, feature.Start, feature.End) (1u, 0u, UInt32.MaxValue) "global"
        }
        test "Script parsing and horizontal direction equal the oracle" {
            OracleSession.ensure ()
            let spellings = [ "Latn"; "latn"; "ARAB"; "Arab"; "Hebr"; "Zyyy"; "Zinh"; "Zzzz"; "Qaai"; "Aran"; "Hans"; "Jamo"; "Xxxx"; "arabic"; String.Empty; "e"; "1234" ]
            for spelling in spellings do
                let bytes = Text.Encoding.UTF8.GetBytes spelling
                let expected = Native.hb_script_from_string(bytes, -1)
                let actual = uint32 (HarfBuzzSharp.Script.Parse spelling)
                Expect.equal actual expected $"Script.Parse {spelling}"
                let script = HarfBuzzSharp.Script.Parse spelling
                let direction = int script.HorizontalDirection
                Expect.equal direction (Native.hb_script_get_horizontal_direction expected) $"HorizontalDirection {spelling}"
        }
        test "GlyphInfo.GlyphFlags is the defined part of the mask" {
            let partial = HarfBuzzSharp.GlyphInfo(Mask = 0xFFFFFFF9u)
            Expect.equal partial.GlyphFlags HarfBuzzSharp.GlyphFlags.UnsafeToBreak "unsafe to break only"
            let all = HarfBuzzSharp.GlyphInfo(Mask = 7u)
            Expect.equal all.GlyphFlags (HarfBuzzSharp.GlyphFlags.UnsafeToBreak ||| HarfBuzzSharp.GlyphFlags.UnsafeToConcat ||| HarfBuzzSharp.GlyphFlags.SafeToInsertTatweel) "all three"
        }
        test "Tag round-trips four characters and keeps its special values" {
            let tag = HarfBuzzSharp.Tag.Parse "liga"
            Expect.equal (uint32 tag) 0x6C696761u "liga"
            Expect.equal (tag.ToString()) "liga" "text"
            Expect.equal (HarfBuzzSharp.Tag.Parse "li") (HarfBuzzSharp.Tag('l', 'i', ' ', ' ')) "padded"
            let none = HarfBuzzSharp.Tag.None
            Expect.equal (none.ToString()) "None" "none"
            Expect.equal (uint32 HarfBuzzSharp.Tag.Max) 0xFFFFFFFFu "max"
        }
    ]
