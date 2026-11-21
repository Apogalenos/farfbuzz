module Farfbuzz.Tests.GlyphRunTests

open System
open System.Collections.Immutable
open System.Text
open Expecto
open Farfbuzz
open Farfbuzz.Harness

/// <summary>Builds a run from code points and clusters.</summary>
/// <param name="items">The code point and cluster pairs.</param>
/// <returns>The run.</returns>
let private runOf (items: (uint32 * uint32) array) =
    items |> Array.fold (fun run (codepoint, cluster) -> GlyphRun.add codepoint cluster run) GlyphRun.empty

/// <summary>The P3.S08 buffer-state cases.</summary>
[<Tests>]
let tests =
    testList "P3.S08" [
        test "empty state and property setters" {
            Expect.equal GlyphRun.empty.MaxLen 0x3FFFFFFF "max length" 
            Expect.equal GlyphRun.empty.MaxOps 0x1FFFFFFF "max operations"
            Expect.isTrue GlyphRun.empty.Successful "successful"
            let run =
                GlyphRun.empty
                |> GlyphRun.withDirection Direction.Rtl
                |> GlyphRun.withScript Scripts.Arabic
                |> GlyphRun.withLanguage (Language.Bcp47 "fa")
                |> GlyphRun.withFlags BufferFlags.Bot
                |> GlyphRun.withClusterLevel ClusterLevel.Characters
                |> GlyphRun.withReplacement 0x25CCu
                |> GlyphRun.withInvisible 3u
                |> GlyphRun.withNotFound 4u
                |> GlyphRun.withNotFoundVariationSelector 5u
                |> GlyphRun.withRandomState 23u
            Expect.equal run.SegmentProperties { Direction = Direction.Rtl; Script = Scripts.Arabic; Language = Language.Bcp47 "fa" } "segment"
            Expect.equal run.Flags BufferFlags.Bot "flags"
            Expect.equal run.ClusterLevel ClusterLevel.Characters "cluster level"
            Expect.equal (run.Replacement, run.Invisible, run.NotFound, run.NotFoundVariationSelector, run.RandomState) (0x25CCu, 3u, 4u, 5u, 23u) "glyph values"
        }
        test "segment property guessing" {
            let arabic = runOf [| 0x627u, 1u |] |> GlyphRun.guessSegmentProperties (Language.Bcp47 "fa")
            Expect.equal arabic.SegmentProperties.Script Scripts.Arabic "Arabic script"
            Expect.equal arabic.SegmentProperties.Direction Direction.Rtl "Arabic direction"
            Expect.equal arabic.SegmentProperties.Language (Language.Bcp47 "fa") "default language"
            let common = runOf [| 0x20u, 0u |] |> GlyphRun.guessSegmentProperties (Language.Bcp47 "fa")
            Expect.equal common.SegmentProperties.Script Scripts.Invalid "common leaves script invalid"
            Expect.equal common.SegmentProperties.Direction Direction.Ltr "fallback direction"
            let preset = GlyphRun.withDirection Direction.Ttb arabic |> GlyphRun.guessSegmentProperties (Language.Bcp47 "fa")
            Expect.equal preset.SegmentProperties.Direction Direction.Ttb "preset kept"
        }
        test "enter narrows limits and resets scratch state" {
            let small = GlyphRun.enter GlyphRun.empty
            Expect.equal (small.MaxLen, small.MaxOps, small.Serial, small.ScratchFlags) (65536, 65536, 0u, ScratchFlags.Default) "empty limits"
            let large = runOf (Array.init 1000 (fun i -> uint32 i, uint32 i)) |> GlyphRun.enter
            Expect.equal large.MaxLen 256000 "length limit"
            Expect.equal large.MaxOps 4096000 "operation limit"
            let dirty = { large with Serial = 9u; ScratchFlags = ScratchFlags.HasCgj } |> GlyphRun.enter
            Expect.equal (dirty.Serial, dirty.ScratchFlags) (0u, ScratchFlags.Default) "scratch reset"
        }
        test "set length extends, shrinks and resets contexts" {
            let start = runOf [| 0x41u, 0u; 0x42u, 1u |] |> fun run -> { run with ContentType = UnicodeContent; ContextBefore = ImmutableArray.Create 0x61u; ContextAfter = ImmutableArray.Create 0x63u }
            let grown = GlyphRun.setLength 4 start
            Expect.equal (grown.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 0x41u; 0x42u; 0u; 0u |] "extended"
            Expect.equal grown.Infos[3].Cluster 0u "zero cluster"
            Expect.equal grown.ContextBefore (ImmutableArray.Create 0x61u) "pre-context kept"
            Expect.isEmpty grown.ContextAfter "post-context cleared"
            let shrunk = GlyphRun.setLength 1 grown
            Expect.equal shrunk.Infos.Length 1 "shrunk"
            let empty = GlyphRun.setLength 0 shrunk
            Expect.equal empty.ContentType ContentType.NoContent "content reset"
            Expect.isEmpty empty.ContextBefore "pre-context reset"
            let failed = GlyphRun.setLength 0x40000000 start
            Expect.isFalse failed.Successful "oversized length fails"
            Expect.equal failed.Infos.Length 2 "oversized length keeps infos"
        }
        test "reverse range and clusters move positions" {
            let run = runOf [| 0x41u, 0u; 0x42u, 1u; 0x43u, 1u; 0x44u, 3u |]
            let positioned =
                { run with
                    HavePositions = true
                    Positions = ImmutableArray.Create(
                        { XAdvance = 1; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u },
                        { XAdvance = 2; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u },
                        { XAdvance = 3; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u },
                        { XAdvance = 4; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }) }
            let reversed = GlyphRun.reverseRange 1 3 positioned
            Expect.equal (reversed.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 0x41u; 0x43u; 0x42u; 0x44u |] "range infos"
            Expect.equal (reversed.Positions |> Seq.map (fun position -> position.XAdvance) |> Seq.toArray) [| 1; 3; 2; 4 |] "range positions"
            let clusters = GlyphRun.reverseClusters positioned
            Expect.equal (clusters.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 0x44u; 0x42u; 0x43u; 0x41u |] "cluster order"
            Expect.equal (clusters.Positions |> Seq.map (fun position -> position.XAdvance) |> Seq.toArray) [| 4; 2; 3; 1 |] "cluster positions"
            let full = GlyphRun.reverse positioned
            Expect.equal (full.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 0x44u; 0x43u; 0x42u; 0x41u |] "full reverse"
        }
        test "append copies content, contexts, positions and overlays properties" {
            let source =
                runOf [| 0x41u, 0u; 0x42u, 1u; 0x43u, 2u |]
                |> fun run ->
                    { run with
                        ContentType = UnicodeContent
                        ContextBefore = ImmutableArray.Create 0x61u
                        ContextAfter = ImmutableArray.Create 0x7Au
                        HavePositions = true
                        Positions = ImmutableArray.Create(
                            { XAdvance = 1; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u },
                            { XAdvance = 2; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u },
                            { XAdvance = 3; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
                        SegmentProperties = { Direction = Direction.Rtl; Script = Scripts.Arabic; Language = Language.Bcp47 "ar" } }
            let target = GlyphRun.withScript Scripts.Latin GlyphRun.empty
            let appended = GlyphRun.append source 1 3 target
            Expect.equal (appended.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 0x42u; 0x43u |] "infos"
            Expect.equal (appended.Positions |> Seq.map (fun position -> position.XAdvance) |> Seq.toArray) [| 2; 3 |] "positions"
            Expect.equal appended.ContentType ContentType.UnicodeContent "content"
            Expect.equal appended.ContextBefore (ImmutableArray.Create(0x41u, 0x61u)) "pre-context from infos"
            Expect.equal appended.ContextAfter (ImmutableArray.Create 0x7Au) "post-context"
            Expect.equal appended.SegmentProperties { Direction = Direction.Rtl; Script = Scripts.Latin; Language = Language.NoLanguage } "overlay"
            let second = GlyphRun.append source 0 2 (GlyphRun.append source 1 3 target)
            Expect.equal (second.Infos.Length, second.ContextBefore, second.ContextAfter) (4, ImmutableArray.Create(0x41u, 0x61u), ImmutableArray.Create(0x43u, 0x7Au)) "second append contexts"
            let cleared = GlyphRun.clearPositions appended
            Expect.isTrue cleared.HavePositions "positions allocated"
            Expect.isTrue (cleared.Positions |> Seq.forall (fun position -> position.XAdvance = 0 && position.YAdvance = 0 && position.XOffset = 0 && position.YOffset = 0)) "positions zeroed"
        }
    ]

/// <summary>The P3.R02 allocation and regression cases.</summary>
[<Tests>]
let r02Tests =
    let positionOf value = { XAdvance = value; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }
    let runOfLength length clusterOf =
        let infos =
            Sealed.init length (fun index ->
                { Codepoint = uint32 index; Mask = 0u; Cluster = clusterOf index; Var1 = 0u; Var2 = 0u })
        { GlyphRun.empty with
            Infos = infos
            Positions = Sealed.init length positionOf
            HavePositions = true }
    let allocated action =
        let before = GC.GetAllocatedBytesForCurrentThread()
        let result = action ()
        let after = GC.GetAllocatedBytesForCurrentThread()
        struct (after - before, result)
    testList "P3.R02" [
        test "addUtf8 fills one info and one property array" {
            let text = Array.create 1000 'a'B
            GlyphRun.addUtf8 (ReadOnlySpan text) 0 text.Length GlyphRun.empty |> ignore
            let struct (bytes, run) = allocated (fun () -> GlyphRun.addUtf8 (ReadOnlySpan text) 0 text.Length GlyphRun.empty |> TailCalls.settle)
            Expect.equal run.Infos.Length 1000 "length"
            Expect.equal run.Props.Length 1000 "properties"
            Expect.isLessThanOrEqual bytes 34000 "allocation"
        }
        test "reverseClusters writes one info, one position and one property array" {
            let run = runOfLength 1000 (fun index -> uint32 (index / 3))
            GlyphRun.reverseClusters run |> ignore
            let struct (bytes, reversed) = allocated (fun () -> GlyphRun.reverseClusters run)
            Expect.equal reversed.Infos.Length 1000 "length"
            Expect.isLessThanOrEqual bytes 56000 "allocation"
        }
        test "reverseRange writes one info, one position and one property array" {
            let run = runOfLength 1000 (fun index -> uint32 index)
            GlyphRun.reverseRange 100 900 run |> ignore
            let struct (bytes, reversed) = allocated (fun () -> GlyphRun.reverseRange 100 900 run)
            Expect.equal reversed.Infos[100].Codepoint 899u "middle"
            Expect.isLessThanOrEqual bytes 56000 "allocation"
        }
        test "append writes one info, one position and one property array" {
            let target = runOfLength 500 (fun index -> uint32 index)
            let source = runOfLength 500 (fun index -> uint32 (index + 500))
            GlyphRun.append source 0 500 target |> ignore
            let struct (bytes, appended) = allocated (fun () -> GlyphRun.append source 0 500 target)
            Expect.equal appended.Infos.Length 1000 "length"
            Expect.equal appended.Infos[999].Codepoint 499u "tail"
            Expect.isLessThanOrEqual bytes 56000 "allocation"
        }
        test "addUtf8 of valid text equals the decoded text, ASCII fast path included, over random items" {
            let random = Random 0x7A5C11
            let pick (low: int) (high: int) = Rune(low + random.Next(high - low + 1))
            for _ in 1 .. 3000 do
                let ascii = random.Next 10 < 7
                let runes =
                    Array.init (random.Next 40) (fun _ ->
                        match ascii, random.Next 6 with
                        | true, _ -> pick 0x00 0x7F
                        | false, 0 -> pick 0x80 0x7FF
                        | false, 1 -> pick 0x800 0xD7FF
                        | false, 2 -> pick 0xE000 0xFFFF
                        | false, 3 -> pick 0x10000 0x10FFFF
                        | false, _ -> pick 0x20 0x7E)
                let widths = runes |> Array.map (fun rune -> rune.Utf8SequenceLength)
                let starts = Array.scan (+) 0 widths
                let bytes = runes |> Array.collect (fun rune -> let buffer = Array.zeroCreate<byte> 4 in rune.EncodeToUtf8 (Span buffer) |> ignore; Array.take rune.Utf8SequenceLength buffer)
                let first = random.Next (runes.Length + 1)
                let last = first + random.Next (runes.Length - first + 1)
                let run = GlyphRun.addUtf8 (ReadOnlySpan bytes) starts[first] (starts[last] - starts[first]) GlyphRun.empty
                let expectedItem = runes[first .. last - 1] |> Array.map (fun rune -> uint32 rune.Value)
                Expect.sequenceEqual (run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)) expectedItem "code points"
                Expect.sequenceEqual (run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Cluster)) (Array.init (last - first) (fun index -> uint32 starts[first + index])) "clusters"
                Expect.equal run.Props.Length (last - first) "properties"
                let after = runes[last .. min (runes.Length - 1) (last + 4)] |> Array.map (fun rune -> uint32 rune.Value)
                Expect.sequenceEqual (run.ContextAfter.AsSpan().ToArray()) after "context after"
                let before = runes[max 0 (first - 5) .. first - 1] |> Array.rev |> Array.map (fun rune -> uint32 rune.Value)
                Expect.sequenceEqual (run.ContextBefore.AsSpan().ToArray()) before "context before"
        }
        test "text entry keeps contexts nearest first" {
            let run = GlyphRun.addUtf8 (ReadOnlySpan [| 0x61uy; 0x62uy; 0x63uy |]) 1 1 GlyphRun.empty
            Expect.equal (run.ContextBefore |> Seq.toArray) [| 0x61u |] "before"
            Expect.equal (run.ContextAfter |> Seq.toArray) [| 0x63u |] "after"
        }
    ]
