module Farfbuzz.Tests.ComplexShapers.DefaultShaperTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.UpstreamFiles

/// <summary>The face of the upstream corpus on which the Indic differential found the difference: no outlines, a GDEF that gives glyph
/// classes to five glyphs, a GSUB and no GPOS.</summary>
[<Literal>]
let private FoundFont = "test/shape/data/in-house/fonts/755160ddba002332349fda3eb999e629d63dccf6.ttf"

/// <summary>Shapes code points with the face, left to right, in the Latin script, as <c>hb-shape --script=Latn</c> does.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The shaped run.</returns>
let private shapedLatin (codepoints: uint32 array) : GlyphRun =
    let font = Face.create (read FoundFont) 0u |> Font.create
    let run =
        GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
        |> GlyphRun.withSegment { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
        |> GlyphRun.withClusterLevel ClusterLevel.MonotoneGraphemes
        |> TailCalls.settle
    match Shape.shape font ImmutableArray<Feature>.Empty run with
    | Ok result -> result
    | Error error -> failtest $"expected a shaped run, got {error}"

/// <summary>The glyph, cluster, advance and offsets of each glyph of a run.</summary>
/// <param name="run">The run.</param>
/// <returns>The tuples.</returns>
let private summary (run: GlyphRun) : (uint32 * uint32 * int * int * int) array =
    Array.init run.Infos.Length (fun index ->
        run.Infos[index].Codepoint, run.Infos[index].Cluster, run.Positions[index].XAdvance, run.Positions[index].XOffset, run.Positions[index].YOffset)

/// <summary>The default shaper's mark zeroing tests.</summary>
[<Tests>]
let tests =
    testList "P6.S21 default shaper" [
        test "a notdef glyph that stands for a mark is zeroed and shifted back by its advance when the face has no outlines" {
            let run = shapedLatin [| 0x4Eu; 0x0A2Du; 0x0A42u; 0x0A02u; 0x0A10u; 0x0A4Du; 0x200Cu; 0x0A02u |]
            Expect.equal (summary run) [| 0u, 0u, 580, 0, 0; 1u, 1u, 594, 0, 0; 3u, 1u, 0, 0, 0; 0u, 1u, 0, -580, 0; 0u, 4u, 580, 0, 0; 4u, 4u, 0, 0, 0; 0u, 6u, 0, -580, 0 |]
                "hb-shape --script=Latn gives [gid0=0+580|gid1=1+594|gid3=1+0|gid0=1@-580,0+0|gid0=4+580|gid4=4+0|gid0=6@-580,0+0]"
        }
        test "a bindi after a base is zeroed and shifted back" {
            let run = shapedLatin [| 0x4Eu; 0x0A02u |]
            Expect.equal (summary run) [| 0u, 0u, 580, 0, 0; 0u, 0u, 0, -580, 0 |] "hb-shape --script=Latn gives [gid0=0+580|gid0=0@-580,0+0]"
        }
        test "a bindi alone is not zeroed" {
            let run = shapedLatin [| 0x0A02u |]
            Expect.equal (summary run) [| 0u, 0u, 580, 0, 0 |] "hb-shape --script=Latn gives [gid0=0+580]"
        }
        test "two marks after a base" {
            let run = shapedLatin [| 0x4Eu; 0x0A02u; 0x0A02u |]
            Expect.equal (summary run) [| 0u, 0u, 580, 0, 0; 0u, 0u, 0, -580, 0; 0u, 0u, 0, -580, 0 |] "hb-shape --script=Latn gives [gid0=0+580|gid0=0@-580,0+0|gid0=0@-580,0+0]"
        }
    ]
