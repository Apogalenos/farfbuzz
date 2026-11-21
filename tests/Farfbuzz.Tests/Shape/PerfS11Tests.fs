module Farfbuzz.Tests.Shape.PerfS11Tests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.Allocation

/// <summary>The three fonts the cmap Latin table is checked against, relative to upstream/harfbuzz.</summary>
let private latinFonts =
    [ "perf/fonts/Roboto-Regular.ttf"
      "perf/fonts/Amiri-Regular.ttf"
      "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf" ]

/// <summary>Loads a font's cmap accelerator.</summary>
/// <param name="path">The font path relative to upstream/harfbuzz.</param>
/// <returns>The accelerator.</returns>
let private cmapOf path =
    let face = Face.create (UpstreamFiles.read path) 0u
    (OtFace.create face).Cmap.Value

/// <summary>The cmap Latin table and the setUnicodeProps fusion of P9.S11.</summary>
[<Tests>]
let tests =
    testList "P9.S11" [
        test "nominalGlyph reads the Latin table for U+0000 to U+00FF on three fonts" {
            for path in latinFonts do
                let cmap = cmapOf path
                Expect.equal cmap.Latin.Length 256 $"Latin table length {path}"
                for codepoint in 0 .. 255 do
                    let cp = uint32 codepoint
                    let expected = Cmap.nominalGlyphGeneral cmap.Table cmap.Best cmap.Remap cp
                    let latin =
                        match cmap.Latin[codepoint] with
                        | 0u -> ValueNone
                        | glyph -> ValueSome glyph
                    Expect.equal latin expected $"Latin {path} U+{codepoint:X4}"
                    Expect.equal (Cmap.nominalGlyph cmap cp) expected $"nominalGlyph {path} U+{codepoint:X4}"
        }
        test "nominalGlyph equals the general path for U+0000 to U+10FFFF on three fonts" {
            for path in latinFonts do
                let cmap = cmapOf path
                let mutable firstMismatch : uint32 voption = ValueNone
                let mutable codepoint = 0
                while firstMismatch.IsNone && codepoint <= 0x10FFFF do
                    let cp = uint32 codepoint
                    match Cmap.nominalGlyph cmap cp = Cmap.nominalGlyphGeneral cmap.Table cmap.Best cmap.Remap cp with
                    | true -> codepoint <- codepoint + 1
                    | false -> firstMismatch <- ValueSome cp
                Expect.isTrue firstMismatch.IsNone $"{path} first mismatch U+{firstMismatch:X4}"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } "setUnicodePropsFused equals setUnicodeProps" (
            Prop.forAll (Arb.fromGen (gen {
                let! count = Gen.choose (0, 24)
                let! codepoints = Gen.listOfLength count (Gen.choose (0, 0x10FFFF) |> Gen.map uint32)
                return List.toArray codepoints
            })) (fun (codepoints: uint32 array) ->
                let run = GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty |> TailCalls.settle
                let mask = 0xFFFFFFFFu
                let original = OtProps.setUnicodeProps mask run
                let fused = OtProps.setUnicodePropsFused mask run
                original.Infos.AsSpan().SequenceEqual(fused.Infos.AsSpan())
                && original.Props.AsSpan().SequenceEqual(fused.Props.AsSpan())
                && original.ScratchFlags = fused.ScratchFlags))
        test "setUnicodePropsFused allocates less than the mask map plus unfold" {
            let codepoints = Array.create 1000 0x41u
            let run = GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty |> TailCalls.settle
            let original = bytesAllocatedBy (fun () -> OtProps.setUnicodeProps 0xFFFFFFFFu run |> ignore)
            let fused = bytesAllocatedBy (fun () -> OtProps.setUnicodePropsFused 0xFFFFFFFFu run |> ignore)
            Expect.isLessThan fused original $"fused ({fused}) should allocate less than the original ({original})"
        }
    ]
