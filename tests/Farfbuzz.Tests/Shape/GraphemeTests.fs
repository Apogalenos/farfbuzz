module Farfbuzz.Tests.Shape.GraphemeTests

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

/// <summary>Compiles the default plan for the grapheme tests.</summary>
/// <returns>The plan.</returns>
let private defaultPlan () : OtShapePlan =
    let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
    OtPlan.compile PlanCapabilities.none props ImmutableArray.Empty OtMap.empty DefaultShaper

/// <summary>Builds a run with Unicode properties and the given segment properties.</summary>
/// <param name="script">The script.</param>
/// <param name="direction">The direction.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private proppedRun (script: Script) (direction: Direction) (codepoints: uint32 array) : GlyphRun =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
    |> GlyphRun.withSegment { Direction = direction; Script = script; Language = NoLanguage }
    |> OtProps.setUnicodeProps 0xFFFFFFFFu
    |> TailCalls.settle

/// <summary>Reads the code points of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The code points.</returns>
let private codepointsOf (run: GlyphRun) : uint32 array =
    run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)

/// <summary>A grouping that keeps every glyph in its own group, so <c>reverseGroups</c> reverses the whole run.</summary>
[<Struct>]
type private AllSeparate =
    interface IGlyphGrouping with
        member _.Group (_, _) = false

/// <summary>The P4.S05 grapheme, dotted-circle, native-direction and rotation tests.</summary>
[<Tests>]
let tests =
    testList "P4.S05" [
        test "a dotted circle is inserted before a leading mark with BOT" {
            let font = font12 [ 0x25CCu, 1u ]
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x301u |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withFlags BufferFlags.Bot
                |> OtProps.setUnicodeProps 0xFFFFFFFFu
                |> TailCalls.settle
            let circled = OtProps.insertDottedCircle font run
            Expect.equal (codepointsOf circled) [| 0x25CCu; 0x301u |] "dotted circle first"
            Expect.equal circled.Infos[0].Cluster circled.Infos[1].Cluster "circle takes the mark's cluster"
        }
        test "no dotted circle without BOT, with context, or with DoNotInsertDottedCircle" {
            let font = font12 [ 0x25CCu, 1u ]
            let mark =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x301u |]) 0 -1 GlyphRun.empty
                |> OtProps.setUnicodeProps 0xFFFFFFFFu
                |> TailCalls.settle
            let noBot = OtProps.insertDottedCircle font mark
            let withContext = OtProps.insertDottedCircle font { mark with ContextBefore = Sealed.ofSpan (ReadOnlySpan [| 0x61u |]) }
            let flagged =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x301u |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withFlags (BufferFlags.Bot ||| BufferFlags.DoNotInsertDottedCircle)
                |> OtProps.setUnicodeProps 0xFFFFFFFFu
                |> TailCalls.settle
            let flagged = OtProps.insertDottedCircle font flagged
            Expect.equal (codepointsOf noBot) [| 0x301u |] "no BOT"
            Expect.equal (codepointsOf withContext) [| 0x301u |] "context"
            Expect.equal (codepointsOf flagged) [| 0x301u |] "DoNotInsertDottedCircle"
        }
        test "an emoji ZWJ sequence forms one cluster" {
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x1F469u; 0x200Du; 0x1F4BBu |]) 0 -1 GlyphRun.empty
                |> OtProps.setUnicodeProps 0xFFFFFFFFu
                |> TailCalls.settle
            let clustered = OtProps.formClusters run
            Expect.equal clustered.Infos.Length 3 "three glyphs"
            Expect.equal clustered.Infos[0].Cluster clustered.Infos[1].Cluster "base and ZWJ share a cluster"
            Expect.equal clustered.Infos[1].Cluster clustered.Infos[2].Cluster "ZWJ and emoji share a cluster"
        }
        test "Arabic digits requested LTR stay LTR" {
            let run = proppedRun Scripts.Arabic Direction.Ltr [| 0x660u; 0x661u |]
            let native = OtProps.ensureNativeDirection run
            Expect.equal native.SegmentProperties.Direction Direction.Ltr "direction stays LTR"
            Expect.equal (codepointsOf native) [| 0x660u; 0x661u |] "digits not reversed"
        }
        test "Old Italic requested RTL is not reversed" {
            let run = proppedRun Scripts.OldItalic Direction.Rtl [| 0x10300u; 0x10301u |]
            let native = OtProps.ensureNativeDirection run
            Expect.equal native.SegmentProperties.Direction Direction.Rtl "direction stays RTL"
            Expect.equal (codepointsOf native) [| 0x10300u; 0x10301u |] "glyphs not reversed"
        }
        test "a parenthesis is mirrored in RTL" {
            let font = font12 [ 0x28u, 1u; 0x29u, 2u ]
            let plan = defaultPlan ()
            let run = GlyphRun.addCodepoints (ReadOnlySpan [| 0x28u |]) 0 -1 GlyphRun.empty |> TailCalls.settle
            let rotated = OtSubstitute.rotateChars plan font Direction.Rtl run
            Expect.equal rotated.Infos[0].Codepoint 0x29u "mirrored to )"
        }
        test "a corner bracket is rotated in TTB" {
            let font = font12 [ 0x3008u, 1u; 0xFE3Fu, 2u ]
            let plan = defaultPlan ()
            let run = GlyphRun.addCodepoints (ReadOnlySpan [| 0x3008u |]) 0 -1 GlyphRun.empty |> TailCalls.settle
            let rotated = OtSubstitute.rotateChars plan font Direction.Ttb run
            Expect.equal rotated.Infos[0].Codepoint 0xFE3Fu "rotated to the vertical form"
        }
        test "reverseGroups moves the props with the glyphs" {
            let infos = Sealed.init 3 (fun index -> { Codepoint = uint32 (0x61 + index); Mask = 0u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 3 (fun index -> { Unchecked.defaultof<GlyphProps> with UnicodeProps = uint16 (index + 1) })
            let run = { GlyphRun.empty with Infos = infos; Props = props }
            let reversed = GlyphRun.reverseGroups (AllSeparate()) false run
            Expect.equal reversed.Infos[2].Codepoint 0x61u "first glyph moves last"
            Expect.equal reversed.Props[0].UnicodeProps 3us "first prop comes from the last glyph"
            Expect.equal reversed.Props[1].UnicodeProps 2us "middle prop stays"
            Expect.equal reversed.Props[2].UnicodeProps 1us "last prop comes from the first glyph"
        }
    ]
