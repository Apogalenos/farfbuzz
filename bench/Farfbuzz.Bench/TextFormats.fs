module Farfbuzz.Bench.TextFormats

open System
open System.Text
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Oracle

/// <summary>Feature strings of the kinds hb-shape users write.</summary>
let featureTexts = [| "kern"; "-liga"; "aalt=2"; "kern[3:5]"; "'ss01' on"; "+calt[2]"; "salt[:5]=3"; "-dlig" |]

/// <summary>Farfbuzz's text formats against native HarfBuzz 15.1.0 doing the same work.</summary>
[<MemoryDiagnoser>]
type TextFormats() =
    let mutable featureBytes: byte array array = [||]
    let mutable featureStringBytes: byte array = [||]
    let mutable infos: GlyphInfo array = [||]
    let mutable positions: GlyphPosition array = [||]

    /// <summary>Prepares the inputs and loads the oracle.</summary>
    [<GlobalSetup>]
    member this.Setup() =
        NativeShaping.oracle.Force() |> ignore
        featureBytes <- featureTexts |> Array.map Encoding.UTF8.GetBytes
        featureStringBytes <- Encoding.UTF8.GetBytes this.FeatureString
        infos <- Array.init 1000 (fun index -> { Codepoint = uint32 (index * 7 % 3000); Mask = uint32 (index % 4); Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
        positions <- Array.init 1000 (fun index -> { XAdvance = 500 + index % 300; YAdvance = 0; XOffset = (if index % 5 = 0 then -40 else 0); YOffset = 0; Var = 0u })

    /// <summary>The per-string feature benchmark input.</summary>
    [<Params("kern", "-liga", "aalt=2", "kern[3:5]", "salt[:5]=3", "+calt[2]", "'ss01' on", "-dlig")>]
    member val FeatureString: string = "kern" with get, set

    /// <summary>Farfbuzz's staged explainer on one feature string.</summary>
    /// <returns>A checksum of the value.</returns>
    [<Benchmark>]
    member this.FarfbuzzExplainFeature() =
        match Feature.explain featureStringBytes with
        | Ok feature -> int feature.Value
        | Error _ -> -1

    /// <summary>Farfbuzz's cursor parser on one feature string.</summary>
    /// <returns>A checksum of the value.</returns>
    [<Benchmark>]
    member this.FarfbuzzParseFeature() =
        match Feature.parseUtf8 featureStringBytes with
        | Ok feature -> int feature.Value
        | Error _ -> -1

    /// <summary>HarfBuzz parses one feature string.</summary>
    /// <returns>A checksum of the value.</returns>
    [<Benchmark(Baseline = true)>]
    member this.HarfBuzzParseFeature() =
        let mutable feature = Unchecked.defaultof<Feature>
        match Native.hb_feature_from_string(featureStringBytes, featureStringBytes.Length, &feature) with
        | 0 -> -1
        | _ -> int feature.Value

    /// <summary>Farfbuzz parses the feature strings.</summary>
    /// <returns>A checksum of the values.</returns>
    [<Benchmark>]
    member _.FarfbuzzParseFeatures() =
        featureBytes |> Array.sumBy (fun bytes -> match Feature.parseUtf8 bytes with Ok feature -> int feature.Value | Error _ -> -1)

    /// <summary>HarfBuzz parses the feature strings.</summary>
    /// <returns>A checksum of the values.</returns>
    [<Benchmark(Baseline = true)>]
    member _.HarfBuzzParseFeatures() =
        featureBytes |> Array.sumBy (fun bytes ->
            let mutable feature = Unchecked.defaultof<Feature>
            match Native.hb_feature_from_string(bytes, bytes.Length, &feature) with
            | 0 -> -1
            | _ -> int feature.Value)

    /// <summary>Farfbuzz serialises 1000 glyphs.</summary>
    /// <returns>The text length.</returns>
    [<Benchmark>]
    member _.FarfbuzzSerialize() =
        (Serialize.glyphs SerializeFlags.NoGlyphNames (fun _ -> ValueNone) (fun _ -> ValueNone) (ReadOnlySpan infos) (ReadOnlySpan positions)).Length

    /// <summary>HarfBuzz serialises 1000 glyphs (buffer set-up included).</summary>
    /// <returns>The text length.</returns>
    [<Benchmark>]
    member _.HarfBuzzSerialize() =
        (NativeText.serializeGlyphs SerializeFlags.NoGlyphNames infos positions).Length
