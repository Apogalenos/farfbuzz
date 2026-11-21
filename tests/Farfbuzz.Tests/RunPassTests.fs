module Farfbuzz.Tests.RunPassTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open Farfbuzz

/// <summary>Builds a run from code point and cluster pairs.</summary>
/// <param name="items">The pairs.</param>
/// <returns>The run.</returns>
let private runOf (items: (uint32 * uint32) array) =
    items |> Array.fold (fun run (codepoint, cluster) -> GlyphRun.add codepoint cluster run) GlyphRun.empty

/// <summary>Builds a run of a given length with the given cluster function.</summary>
/// <param name="length">The length.</param>
/// <param name="clusterOf">The cluster of each index.</param>
/// <returns>The run.</returns>
let private runOfLength (length: int) (clusterOf: int -> uint32) =
    let infos =
        Sealed.init length (fun index ->
            { Codepoint = uint32 index; Mask = 0u; Cluster = clusterOf index; Var1 = 0u; Var2 = 0u })
    { GlyphRun.empty with Infos = infos }

/// <summary>Converts an op-script end argument to the -1 sentinel.</summary>
/// <param name="value">The raw value.</param>
/// <returns>-1 for 0xFFFFFFFF, the value otherwise.</returns>
let private endOf (value: uint32) =
    match value = UInt32.MaxValue with
    | true -> -1
    | false -> int value

/// <summary>Decodes one op of the P3.S09 op script into an edit.</summary>
/// <param name="ops">The op stream.</param>
/// <param name="index">The next stream index.</param>
/// <returns>The edit and the next index.</returns>
let rec private decodeOp (ops: uint32 array) (index: int) : struct (RunEdit * int) =
    match ops[index] with
    | 1u -> struct (RunEdit.ClearOutput, index + 1)
    | 2u -> struct (RunEdit.NextGlyph, index + 1)
    | 3u -> struct (RunEdit.ReplaceGlyph ops[index + 1], index + 2)
    | 4u -> struct (RunEdit.OutputGlyph ops[index + 1], index + 2)
    | 5u -> struct (RunEdit.SkipGlyph, index + 1)
    | 6u -> struct (RunEdit.DeleteGlyph, index + 1)
    | 7u -> struct (RunEdit.CopyGlyph, index + 1)
    | 8u -> struct (RunEdit.MoveTo (int ops[index + 1]), index + 2)
    | 9u -> struct (RunEdit.MergeClusters (int ops[index + 1], int ops[index + 2]), index + 3)
    | 10u -> struct (RunEdit.MergeOutClusters (int ops[index + 1], int ops[index + 2]), index + 3)
    | 11u -> struct (RunEdit.UnsafeToBreak (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 12u -> struct (RunEdit.UnsafeToConcat (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 13u -> struct (RunEdit.UnsafeToBreakFromOutbuffer (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 14u -> struct (RunEdit.UnsafeToConcatFromOutbuffer (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | 15u -> struct (RunEdit.Sync, index + 1)
    | 16u ->
        let produced = Sealed.init (int ops[index + 2]) (fun i -> ops[index + 3 + i])
        struct (RunEdit.ReplaceGlyphs (int ops[index + 1], produced), index + 3 + produced.Length)
    | 17u -> struct (RunEdit.SetMasks (ops[index + 1], ops[index + 2], ops[index + 3], ops[index + 4]), index + 5)
    | 18u -> struct (RunEdit.NextGlyphs (int ops[index + 1]), index + 2)
    | 19u -> struct (RunEdit.SafeToInsertTatweel (int ops[index + 1], endOf ops[index + 2]), index + 3)
    | _ -> struct (RunEdit.Finish, index + 1)

/// <summary>A pass that replays a P3.S09 op script.</summary>
[<Struct>]
type ScriptPass =
    val private Ops: uint32 array
    /// <summary>Creates a script pass.</summary>
    /// <param name="ops">The op stream.</param>
    new (ops: uint32 array) = { Ops = ops }
    interface IRunPass<int> with
        member this.Step (_, index) =
            match index >= this.Ops.Length with
            | true -> struct (RunEdit.Finish, index)
            | false -> decodeOp this.Ops index

/// <summary>Runs an op script over a run.</summary>
/// <param name="ops">The op stream.</param>
/// <param name="run">The run.</param>
/// <returns>The run after the script.</returns>
let private runScript (ops: uint32 array) (run: GlyphRun) =
    GlyphRun.runPass (ScriptPass(ops)) 0 run

/// <summary>A pass that advances every glyph without opening an output buffer.</summary>
[<Struct>]
type IdentityPass =
    interface IRunPass<unit> with
        member _.Step (cursor, _) =
            match cursor.Index < cursor.Input.Length with
            | true -> struct (RunEdit.NextGlyph, ())
            | false -> struct (RunEdit.Finish, ())

/// <summary>A pass that deletes every glyph.</summary>
[<Struct>]
type DeleteAllPass =
    interface IRunPass<unit> with
        member _.Step (cursor, _) =
            match cursor.HaveOutput, cursor.Index < cursor.Input.Length with
            | false, _ -> struct (RunEdit.ClearOutput, ())
            | true, true -> struct (RunEdit.DeleteGlyph, ())
            | true, false -> struct (RunEdit.Finish, ())

/// <summary>A comparer ordering glyphs by descending code point.</summary>
[<Struct>]
type DescendingComparer =
    interface IGlyphComparer with
        member _.Compare (a, b) = int b.Codepoint - int a.Codepoint

/// <summary>The P3.S09 out-buffer tests.</summary>
[<Tests>]
let tests =
    testList "P3.S09" [
        test "an identity pass returns the same instance" {
            let run = runOfLength 32 (fun index -> uint32 index)
            let result = GlyphRun.runPass (IdentityPass()) () run
            Expect.isTrue (Object.ReferenceEquals(result, run)) "reference-equal"
            Expect.equal result.Infos.Length 32 "length preserved"
        }
        test "a pass deleting every glyph empties the buffer" {
            let run = runOf [| 0x41u, 0u; 0x42u, 1u; 0x43u, 2u; 0x44u, 3u |]
            let result = GlyphRun.runPass (DeleteAllPass()) () run
            Expect.equal result.Infos.Length 0 "all glyphs deleted"
        }
        test "replace glyph rewrites the code point" {
            let run = runOf [| 0x41u, 0u; 0x42u, 1u |]
            let result = runScript [| 1u; 3u; 100u; 2u; 15u |] run
            Expect.equal (result.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 100u; 0x42u |] "first replaced"
        }
        test "merge clusters assigns the smallest cluster" {
            let run = runOf [| 0x41u, 5u; 0x42u, 3u; 0x43u, 7u |]
            let result = runScript [| 9u; 0u; 3u |] run
            Expect.equal (result.Infos |> Seq.map (fun info -> info.Cluster) |> Seq.toArray) [| 3u; 3u; 3u |] "merged"
        }
        test "unsafe to break flags every glyph but the minimum cluster" {
            let run = runOf [| 0x41u, 0u; 0x42u, 1u; 0x43u, 2u |]
            let result = runScript [| 11u; 0u; 3u |] run
            Expect.equal (result.Infos |> Seq.map (fun info -> info.Mask &&& 7u) |> Seq.toArray) [| 0u; 3u; 3u |] "interior cluster kept clean"
        }
        test "sort clusters orders infos by code point" {
            let run = runOf [| 0x43u, 0u; 0x41u, 1u; 0x42u, 2u |]
            let sorted = GlyphRun.sortClusters 0 3 (DescendingComparer()) run
            Expect.equal (sorted.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray) [| 0x43u; 0x42u; 0x41u |] "descending"
        }
        testProperty "merge-only scripts keep sorted clusters monotone under MonotoneCharacters" (fun (clusters: int array) ->
            let values = clusters |> Array.map (fun value -> uint32 (abs (value % 5))) |> Array.sort
            let length = values.Length
            let run = runOfLength length (fun index -> values[index]) |> GlyphRun.withClusterLevel ClusterLevel.MonotoneCharacters
            let merges = [| for i in 0 .. length - 2 do yield 9u; yield uint32 i; yield uint32 (i + 2) |]
            let ops = Array.concat [| [| 1u |]; merges; [| 15u |] |]
            let result = runScript ops run
            let output = result.Infos |> Seq.map (fun info -> info.Cluster) |> Seq.toArray
            Expect.isTrue (output |> Seq.pairwise |> Seq.forall (fun (a, b) -> a <= b)) "clusters monotone")
    ]

/// <summary>The P3.S09 allocation test.</summary>
[<Tests>]
let allocationTests =
    testList "P3.S09" [
        test "runPass on an identity pass allocates once, not per edit" {
            let run = runOfLength 1000 (fun index -> uint32 index)
            GlyphRun.runPass (IdentityPass()) () run |> ignore
            let before = GC.GetAllocatedBytesForCurrentThread()
            let result = GlyphRun.runPass (IdentityPass()) () run
            let after = GC.GetAllocatedBytesForCurrentThread()
            Expect.isTrue (Object.ReferenceEquals(result, run)) "reference-equal"
            Expect.isLessThanOrEqual (after - before) 70000 "allocation"
        }
    ]