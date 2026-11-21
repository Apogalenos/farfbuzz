module Farfbuzz.Tests.Layout.EngineTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>A minimal font for the finish-offsets tests.</summary>
/// <returns>The font.</returns>
let private emptyFont () : Font =
    Font.create (Face.create (ReadOnlyMemory (sfnt 0x00010000u [])) 0u)

/// <summary>Encodes an attach chain and type into a position's <c>var</c> word.</summary>
/// <param name="chain">The attach chain.</param>
/// <param name="typ">The attach type.</param>
/// <returns>The var value.</returns>
let private attach (chain: int) (typ: byte) : uint32 =
    (uint32 (uint16 chain)) ||| (uint32 typ <<< 16)

/// <summary>Builds a position with an advance, an offset and an attachment.</summary>
/// <param name="xAdvance">The x advance.</param>
/// <param name="xOffset">The x offset.</param>
/// <param name="yOffset">The y offset.</param>
/// <param name="var">The attach var.</param>
/// <returns>The position.</returns>
let private posOf (xAdvance: int) (xOffset: int) (yOffset: int) (var: uint32) : GlyphPosition =
    { XAdvance = xAdvance; YAdvance = 0; XOffset = xOffset; YOffset = yOffset; Var = var }

/// <summary>Builds a positioned run of a given direction and attachment scratch flag.</summary>
/// <param name="direction">The direction.</param>
/// <param name="positions">The positions.</param>
/// <returns>The run.</returns>
let private runOf (direction: Direction) (positions: GlyphPosition array) : GlyphRun =
    let infos = Sealed.init positions.Length (fun i -> { Codepoint = uint32 (0x41 + i); Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init positions.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = Sealed.ofSpan (ReadOnlySpan positions)
        HavePositions = true
        SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = NoLanguage }
        ScratchFlags = ScratchFlags.HasGposAttachment }

/// <summary>Reads the y offsets of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The offsets.</returns>
let private yOffsetsOf (run: GlyphRun) : int array =
    run.Positions.AsSpan().ToArray() |> Array.map (fun position -> position.YOffset)

/// <summary>Reads the x offsets of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The offsets.</returns>
let private xOffsetsOf (run: GlyphRun) : int array =
    run.Positions.AsSpan().ToArray() |> Array.map (fun position -> position.XOffset)

/// <summary>Reads the attach chains of a run, the low 16 bits of each var.</summary>
/// <param name="run">The run.</param>
/// <returns>The chains.</returns>
let private chainsOf (run: GlyphRun) : int array =
    run.Positions.AsSpan().ToArray() |> Array.map (fun position -> int (uint16 (position.Var &&& 0xFFFFu)))

/// <summary>Compiles a default-shaper plan over an upstream font.</summary>
/// <param name="path">The upstream font path.</param>
/// <returns>The plan and the font.</returns>
let private planOf (path: string) : struct (OtShapePlan * Font) =
    let font = Font.create (Face.create (read path) 0u)
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    struct (OtPlan.create font props ImmutableArray.Empty, font)

/// <summary>Generates a random codepoint string for the gating property.</summary>
/// <returns>The generator.</returns>
let private genCodepoints : Gen<uint32 array> =
    gen {
        let! length = Gen.choose (0, 12)
        return! Gen.arrayOfLength length (Gen.elements [| 0x41u; 0x42u; 0x43u; 0x2Eu; 0x20u; 0x34u |])
    }

/// <summary>The P5.S06 engine, accelerator and attachment-offset cases.</summary>
[<Tests>]
let tests =
    testList "P5.S06" [
        test "cursive chain propagation accumulates the far-end offset" {
            let font = emptyFont ()
            let cursive = 0x02uy
            let positions =
                [| posOf 1000 0 1 (attach 1 cursive)
                   posOf 1000 0 2 (attach 1 cursive)
                   posOf 1000 0 5 (attach 0 0uy) |]
            let run = runOf Direction.Ltr positions
            let finished = GposApply.finishOffsets font run
            Expect.equal (yOffsetsOf finished) [| 8; 7; 5 |] "the offsets accumulate toward the attached glyphs"
            Expect.equal (chainsOf finished) [| 0; 0; 0 |] "every attach chain is cleared"
        }
        test "a mark attached after it in RTL subtracts the intervening advances" {
            let font = emptyFont ()
            let mark = 0x01uy
            let positions =
                [| posOf 1000 0 0 0u
                   posOf 100 0 0 (attach 2 mark)
                   posOf 200 0 0 0u
                   posOf 300 10 0 0u |]
            let run = runOf Direction.Rtl positions
            let finished = GposApply.finishOffsets font run
            Expect.equal (xOffsetsOf finished) [| 0; -490; 0; 10 |] "the mark inherits the base offset minus the advances between"
            Expect.equal (chainsOf finished) [| 0; 0; 0; 0 |] "every attach chain is cleared"
        }
        test "a cyclic attachment chain is bounded by the nesting level" {
            let font = emptyFont ()
            let cursive = 0x02uy
            let positions =
                [| posOf 1000 0 1 (attach 1 cursive)
                   posOf 1000 0 1 (attach -1 cursive) |]
            let run = runOf Direction.Ltr positions
            let finished = GposApply.finishOffsets font run
            Expect.equal (finished.Positions.Length) 2 "the run keeps its two glyphs"
            Expect.equal (chainsOf finished) [| 0; 0 |] "the cycle is broken by clearing each chain"
        }
        test "a face whose lookups are all Pending returns the run reference-equal" {
            let struct (plan, font) = planOf "test/api/fonts/NotoSans-Bold.ttf"
            let run = GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x42u; 0x43u |]) 0 -1 GlyphRun.empty
            let substituted = Engine.substitute plan font run
            Expect.isTrue (obj.ReferenceEquals(substituted, run)) "no pending subtable applies, so the run is unchanged"
        }
        test "substitute start reads the real GDEF glyph class" {
            let font = Font.create (Face.create (read "test/api/fonts/NotoNastaliqUrdu-Regular.ttf") 0u)
            let glyph =
                match Font.nominalGlyph font 0x064Bu with
                | ValueSome glyph -> glyph
                | ValueNone -> 0u
            let run = GlyphRun.addCodepoints (ReadOnlySpan [| glyph |]) 0 -1 GlyphRun.empty
            let started = OtSubstitute.substituteStart font run
            Expect.equal started.Props[0].GlyphClass (Gdef.glyphProps font.Ot.Gdef.Value glyph) "the GDEF class lands on the glyph"
            Expect.isTrue (started.Props[0].GlyphClass &&& GlyphPropsBits.Mark <> 0us) "the Arabic mark carries its mark class"
        }
        testProperty "digest gating never changes output" (
            Prop.forAll (Arb.fromGen genCodepoints) (fun codepoints ->
                let struct (plan, font) = planOf "test/api/fonts/NotoSans-Bold.ttf"
                let run = GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
                let gated = Engine.substitute plan font run
                let ungated = Engine.applyTable false (Engine.GsubApplier()) font.Ot.Gsub.Value plan.Map.GsubLookups plan.Map.GsubStages font run
                gated = ungated))
    ]
