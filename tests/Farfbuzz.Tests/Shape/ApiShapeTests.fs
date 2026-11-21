module Farfbuzz.Tests.Shape.ApiShapeTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.UpstreamFiles

/// <summary>The API shape tests ported from <c>test/api/test-shape.c</c>.</summary>
[<Tests>]
let tests =
    testList "P4.S06" [
        test "test_shape_clusters" {
            let font = Font.create Face.empty
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0xFFF1u; 0xF0B6u |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withDirection Direction.Ltr
                |> TailCalls.settle
            match Shape.shape font ImmutableArray.Empty run with
            | Error error -> failtest $"expected Ok, got {error}"
            | Ok shaped ->
                Expect.equal shaped.Infos.Length 1 "one glyph"
                Expect.equal shaped.Infos[0].Codepoint 0u "the not-found glyph"
                Expect.equal shaped.Infos[0].Cluster 0u "cluster 0"
        }
        test "test_shape_overlong_cluster_remerge" {
            let font = Face.create (read "test/api/fonts/Roboto-Regular.abc.ttf") 0u |> Font.create
            let text = Array.concat [| Array.create 20000 0x00ADu; [| 0x61u |]; Array.create 20000 0x0301u |]
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
                |> GlyphRun.withDirection Direction.Rtl
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            match Shape.shapeFull font ImmutableArray.Empty (ValueSome (ImmutableArray.Create "ot")) run with
            | Error error -> failtest $"expected Ok, got {error}"
            | Ok shaped -> Expect.isFalse shaped.Successful "the re-merge walk exhausts the op budget"
        }
    ]
