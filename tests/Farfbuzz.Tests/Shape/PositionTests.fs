module Farfbuzz.Tests.Shape.PositionTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder

/// <summary>Builds a format-12 cmap font mapping each code point to a glyph.</summary>
/// <param name="pairs">The code point and glyph pairs.</param>
/// <returns>The font.</returns>
let private font12 (pairs: (uint32 * uint32) list) : Font =
    let groups = pairs |> List.map (fun (codepoint, glyph) -> codepoint, codepoint, glyph)
    let body = groups |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ]) |> Array.concat
    let subtable = Array.concat [ be16 12u; be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]
    let cmap = Array.concat [ be16 0u; be16 1u; be16 3u; be16 1u; be32 12u; subtable ]
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmap ])) 0u |> Font.create

/// <summary>Compiles a plan with the fraction masks enabled.</summary>
/// <returns>The plan.</returns>
let private fracPlan () : OtShapePlan =
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    let plan = OtPlan.compile PlanCapabilities.none props ImmutableArray.Empty OtMap.empty DefaultShaper
    { plan with HasFrac = true; FracMask = 0x1000u; NumrMask = 0x2000u; DnomMask = 0x4000u }

/// <summary>Builds a one-glyph space run with a chosen fallback type and advance.</summary>
/// <param name="space">The space fallback type.</param>
/// <param name="invisible">The invisible glyph.</param>
/// <param name="codepoint">The glyph code point.</param>
/// <returns>The run.</returns>
let private spaceRun (space: SpaceFallback) (invisible: uint32) (codepoint: uint32) : GlyphRun =
    let infos = Sealed.init 1 (fun _ -> { Codepoint = codepoint; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init 1 (fun _ -> UnicodeProps.setSpaceFallback space { Unchecked.defaultof<GlyphProps> with UnicodeProps = 29us })
    let positions = Sealed.init 1 (fun _ -> { XAdvance = 1000; YAdvance = 1000; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        Invisible = invisible
        SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
        ScratchFlags = ScratchFlags.HasSpaceFallback }

/// <summary>Reads the masks of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The masks.</returns>
let private masksOf (run: GlyphRun) : uint32 array =
    run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Mask)

/// <summary>Reads the code points of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The code points.</returns>
let private codepointsOf (run: GlyphRun) : uint32 array =
    run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)

/// <summary>The P4.S06 mask, fallback-space, variation-selector and default-ignorable tests.</summary>
[<Tests>]
let tests =
    testList "P4.S06" [
        test "a fraction with digits on both sides gets numerator, slash and denominator masks" {
            let plan = fracPlan ()
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x31u; 0x2044u; 0x32u |]) 0 -1 GlyphRun.empty
                |> OtProps.setUnicodeProps 0u
                |> TailCalls.settle
            let masked = OtSubstitute.setupMasks plan run
            Expect.equal (masksOf masked) [| 0x3000u; 0x1003u; 0x5003u |] "masks"
        }
        test "an isolated fraction slash sets unsafe-to-concat on the empty side" {
            let plan = fracPlan ()
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x31u; 0x2044u |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withFlags BufferFlags.ProduceUnsafeToConcat
                |> OtProps.setUnicodeProps 0u
                |> TailCalls.settle
            let masked = OtSubstitute.setupMasks plan run
            Expect.equal (masksOf masked |> Array.map (fun mask -> mask &&& 7u)) [| 0u; 2u |] "unsafe-to-concat on the slash"
        }
        test "every space fallback type adjusts the advance" {
            let font = font12 [ 0x2Eu, 2u; 0x30u, 1u ]
            let cases =
                [| SpaceFallback.NotSpace, 1000
                   SpaceFallback.SpaceEm, 1000
                   SpaceFallback.SpaceEm2, 500
                   SpaceFallback.SpaceEm3, 333
                   SpaceFallback.SpaceEm4, 250
                   SpaceFallback.SpaceEm5, 200
                   SpaceFallback.SpaceEm6, 167
                   SpaceFallback.SpaceEm16, 63
                   SpaceFallback.Space4Em18, 222
                   SpaceFallback.Space, 1000
                   SpaceFallback.SpaceFigure, 500
                   SpaceFallback.SpacePunctuation, 500
                   SpaceFallback.SpaceNarrow, 500 |]
            for space, expected in cases do
                let positioned = OtPosition.fallbackSpaces font (spaceRun space 0u 0x2002u)
                Expect.equal positioned.Positions[0].XAdvance expected $"space {space}"
        }
        test "an invisible space glyph gets a quarter-em advance" {
            let font = Font.create Face.empty
            let positioned = OtPosition.fallbackSpaces font (spaceRun SpaceFallback.Space 0x2002u 0x2002u)
            Expect.equal positioned.Positions[0].XAdvance 250 "quarter em"
        }
        test "a variation selector is replaced by the not-found variation selector glyph" {
            let struct (baseProps, _) = UnicodeProps.compute 0xFE00u
            let selectorProps = UnicodeProps.setVariationSelector true { Unchecked.defaultof<GlyphProps> with UnicodeProps = baseProps }
            let infos = Sealed.init 2 (fun i -> { Codepoint = (if i = 0 then 0x61u else 0xFE00u); Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 2 (fun i -> if i = 0 then Unchecked.defaultof<GlyphProps> else selectorProps)
            let positions = Sealed.init 2 (fun _ -> { XAdvance = 100; YAdvance = 100; XOffset = 3; YOffset = 4; Var = 9u })
            let run =
                { GlyphRun.empty with
                    Infos = infos
                    Props = props
                    Positions = positions
                    HavePositions = true
                    ScratchFlags = ScratchFlags.HasVariationSelectorFallback
                    NotFoundVariationSelector = 99u }
            let dealt = OtSubstitute.dealWithVariationSelectors run
            Expect.equal dealt.Infos[1].Codepoint 99u "codepoint replaced"
            Expect.equal dealt.Positions[1].XAdvance 0 "x advance zeroed"
            Expect.equal dealt.Positions[1].YAdvance 0 "y advance zeroed"
            Expect.equal dealt.Positions[1].XOffset 0 "x offset zeroed"
            Expect.equal dealt.Positions[1].YOffset 0 "y offset zeroed"
            Expect.equal dealt.Positions[1].Var 9u "var kept"
            Expect.isFalse (UnicodeProps.isVariationSelector dealt.Props[1]) "no longer a variation selector"
            Expect.equal (UnicodeProps.generalCategory dealt.Props[1]) GeneralCategory.NonSpacingMark "category restored to Mn"
        }
        test "default ignorables are replaced by the space glyph" {
            let font = font12 [ 0x20u, 7u ]
            let infos = Sealed.init 3 (fun i -> { Codepoint = uint32 i; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 3 (fun i -> if i = 1 then { Unchecked.defaultof<GlyphProps> with UnicodeProps = UnicodeProps.Ignorable } else Unchecked.defaultof<GlyphProps>)
            let run = { GlyphRun.empty with Infos = infos; Props = props; ScratchFlags = ScratchFlags.HasDefaultIgnorables }
            let hidden = OtSubstitute.hideDefaultIgnorables font run
            Expect.equal (codepointsOf hidden) [| 0u; 7u; 2u |] "middle glyph replaced"
        }
        test "deleting a default ignorable merges its cluster backward" {
            let font = font12 []
            let infos = Sealed.init 2 (fun i -> { Codepoint = uint32 i; Mask = 0u; Cluster = (if i = 0 then 5u else 0u); Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 2 (fun i -> if i = 1 then { Unchecked.defaultof<GlyphProps> with UnicodeProps = UnicodeProps.Ignorable } else Unchecked.defaultof<GlyphProps>)
            let run = { GlyphRun.empty with Infos = infos; Props = props; ScratchFlags = ScratchFlags.HasDefaultIgnorables }
            let hidden = OtSubstitute.hideDefaultIgnorables font run
            Expect.equal hidden.Infos.Length 1 "one glyph remains"
            Expect.equal hidden.Infos[0].Cluster 0u "cluster merged backward"
        }
        test "a zero-width default ignorable in TTB keeps its x offset" {
            let infos = Sealed.init 1 (fun _ -> { Codepoint = 0u; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 1 (fun _ -> { Unchecked.defaultof<GlyphProps> with UnicodeProps = UnicodeProps.Ignorable })
            let positions = Sealed.init 1 (fun _ -> { XAdvance = 100; YAdvance = 200; XOffset = 7; YOffset = 8; Var = 0u })
            let run =
                { GlyphRun.empty with
                    Infos = infos
                    Props = props
                    Positions = positions
                    HavePositions = true
                    ScratchFlags = ScratchFlags.HasDefaultIgnorables
                    SegmentProperties = { Direction = Direction.Ttb; Script = Scripts.Latin; Language = NoLanguage } }
            let positioned = OtPosition.zeroWidthDefaultIgnorables run
            Expect.equal positioned.Positions[0].XAdvance 0 "x advance zeroed"
            Expect.equal positioned.Positions[0].YAdvance 0 "y advance zeroed"
            Expect.equal positioned.Positions[0].XOffset 7 "x offset kept"
            Expect.equal positioned.Positions[0].YOffset 0 "y offset zeroed"
        }
    ]

/// <summary>The PERF6 test: the batched default advances equal the per-glyph advance on a real hmtx font.</summary>
[<Tests>]
let perf6PositionTests =
    testList "PERF6" [
        test "hAdvancePositions equals the per-glyph hAdvance" {
            let font = Font.create (Face.create (Farfbuzz.Tests.UpstreamFiles.read "perf/fonts/Roboto-Regular.ttf") 0u)
            let codepoints = [| 0x41u; 0x42u; 0x61u; 0x20u; 0x30u; 0x2Eu; 0x2Cu; 0x1E9Eu |]
            let infos =
                Sealed.init codepoints.Length (fun i ->
                    { Codepoint = codepoints[i]; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
            let positions = Font.hAdvancePositions font (infos.AsSpan())
            let reference =
                Sealed.init codepoints.Length (fun i ->
                    { XAdvance = Font.hAdvance font infos[i].Codepoint; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
            Expect.equal (positions.AsSpan().ToArray()) (reference.AsSpan().ToArray()) "advances agree"
        }
    ]
