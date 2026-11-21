module Farfbuzz.Tests.ComplexShapers.SyllableEditTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness

/// <summary>A rewriter that applies a per-chunk list of local cluster merges, addressed by a mutable chunk counter.</summary>
[<Struct>]
type private MergeRewriter =
    val Level: ClusterLevel
    val Merges: struct (int * int)[][] // one merge list per chunk, each [start, finish) chunk-relative
    val Counter: int[]                 // the next chunk index, shared mutable cell
    new (level, merges, counter) = { Level = level; Merges = merges; Counter = counter }

    interface ISegmentRewriter with
        member this.Rewrite (rows) =
            let chunkIndex = this.Counter[0]
            this.Counter[0] <- this.Counter[0] + 1
            let level = this.Level
            let merges = this.Merges[chunkIndex]
            let initial : SyllableState =
                { Rows = Sealed.ofSpan rows
                  Left = ImmutableArray.Empty
                  Right = ImmutableArray.Empty }
            let final =
                Array.fold
                    (fun state (struct (s, e)) -> SyllableEdit.mergeClusters level s e state)
                    initial
                    merges
            { Rows = final.Rows; Left = final.Left; Right = final.Right } : SegmentRewrite

/// <summary>A pass that emits one <c>MergeClusters</c> edit per merge, in order, for the whole-run reference.</summary>
[<Struct>]
type private MergePass =
    val Merges: struct (int * int)[] // absolute merges, in application order
    new (merges) = { Merges = merges }

    interface IRunPass<int> with
        member this.Step (_, state) =
            match state < this.Merges.Length with
            | false -> struct (RunEdit.Finish, state)
            | true ->
                let struct (s, e) = this.Merges[state]
                struct (RunEdit.MergeClusters (s, e), state + 1)

/// <summary>Builds tagged infos and properties for a random run.</summary>
/// <param name="clusters">The clusters.</param>
/// <param name="masks">The masks.</param>
/// <param name="syllables">The syllable bytes.</param>
/// <returns>The infos and properties.</returns>
let private buildRun (clusters: uint32 array) (masks: uint32 array) (syllables: byte array) : struct (GlyphInfo array * GlyphProps array) =
    let n = clusters.Length
    let infos =
        Array.init n (fun i ->
            { Codepoint = uint32 (i + 1); Mask = masks[i]; Cluster = clusters[i]; Var1 = 0u; Var2 = 0u })
    let props =
        Array.init n (fun i ->
            { UnicodeProps = 0us
              GlyphClass = 0us
              LigProps = 0uy
              Syllable = syllables[i]
              ShaperCategory = 0uy
              ShaperAuxiliary = 0uy
              GlyphIndex = 0u })
    struct (infos, props)

/// <summary>Flattens the per-chunk merge lists into absolute merge ranges.</summary>
/// <param name="chunks">The chunks.</param>
/// <param name="merges">The per-chunk merges.</param>
/// <returns>The absolute merges.</returns>
let private absoluteMerges (chunks: struct (int * int) array) (merges: struct (int * int)[] array) : struct (int * int)[] =
    merges
    |> Array.mapi (fun chunkIndex mergeList ->
        let struct (chunkStart, _) = chunks[chunkIndex]
        mergeList |> Array.map (fun struct (s, e) -> struct (chunkStart + s, chunkStart + e)))
    |> Array.concat

/// <summary>Sequences a list of generators into a generator of a list.</summary>
/// <param name="gens">The generators.</param>
/// <returns>The generator.</returns>
let rec private sequenceGens (gens: Gen<'T> list) : Gen<'T list> =
    match gens with
    | [] -> gen { return [] }
    | first :: rest ->
        gen {
            let! head = first
            let! tail = sequenceGens rest
            return head :: tail
        }

/// <summary>A random run plus one merge per chunk, drawn within that chunk.</summary>
let private genCase : Gen<struct (byte array * uint32 array * uint32 array * ClusterLevel * struct (int * int)[] array)> =
    gen {
        let! n = Gen.choose (0, 20)
        let! level =
            Gen.elements
                [ ClusterLevel.MonotoneGraphemes
                  ClusterLevel.MonotoneCharacters
                  ClusterLevel.Characters
                  ClusterLevel.Graphemes ]
        let! syllables = Gen.arrayOfLength n (Gen.choose (0, 3) |> Gen.map byte)
        let! clusters = Gen.arrayOfLength n (Gen.choose (0, 4) |> Gen.map uint32)
        let! masks = Gen.arrayOfLength n (Gen.choose (0, 7) |> Gen.map uint32)
        let props =
            Sealed.init n (fun i ->
                { Unchecked.defaultof<GlyphProps> with Syllable = syllables[i] })
        let chunkList =
            SyllableEdit.chunks props
            |> Seq.map (fun (struct (s, e)) -> struct (s, e))
            |> Seq.toArray
        let! merges =
            chunkList
            |> Array.map (fun struct (chunkStart, chunkEnd) ->
                gen {
                    let! count = Gen.choose (0, 2)
                    return! Gen.arrayOfLength count (gen {
                        let! a = Gen.choose (0, chunkEnd - chunkStart)
                        let! b = Gen.choose (a, chunkEnd - chunkStart)
                        return struct (a, b)
                    })
                })
            |> Array.toList
            |> sequenceGens
            |> Gen.map List.toArray
        return struct (syllables, clusters, masks, level, merges)
    }

/// <summary>Farfbuzz's rewriteSegments applied with the per-chunk merges.</summary>
/// <param name="infos">The infos.</param>
/// <param name="props">The properties.</param>
/// <param name="level">The cluster level.</param>
/// <param name="merges">The per-chunk merges.</param>
/// <returns>The rewritten infos and properties.</returns>
let private rewrite (infos: GlyphInfo array) (props: GlyphProps array) (level: ClusterLevel) (merges: struct (int * int)[] array) : struct (ImmutableArray<GlyphInfo> * ImmutableArray<GlyphProps>) =
    let counter = [| 0 |]
    Sealed.rewriteSegments (MergeRewriter(level, merges, counter)) (ReadOnlySpan infos) (ReadOnlySpan props) |> TailCalls.settle

/// <summary>The whole-run reference: the same merges as <c>MergeClusters</c> edits through <c>Sealed.runPass</c>.</summary>
/// <param name="infos">The infos.</param>
/// <param name="props">The properties.</param>
/// <param name="level">The cluster level.</param>
/// <param name="merges">The absolute merges.</param>
/// <returns>The reference infos and properties.</returns>
let private reference (infos: GlyphInfo array) (props: GlyphProps array) (level: ClusterLevel) (merges: struct (int * int) array) : struct (ImmutableArray<GlyphInfo> * ImmutableArray<GlyphProps>) =
    let input =
        { Infos = Sealed.ofSpan (ReadOnlySpan infos)
          Props = Sealed.ofSpan (ReadOnlySpan props)
          Positions = ImmutableArray.Empty
          HavePositions = false
          MaxLen = Int32.MaxValue
          MaxOps = Int32.MaxValue
          Successful = true
          ClusterLevel = level
          Flags = BufferFlags.Default
          Serial = 0uy
          RandomState = 1u
          ScratchFlags = ScratchFlags.Default }
    let result = Sealed.runPass input (MergePass merges) 0
    match result.Unchanged with
    | true -> struct (input.Infos, input.Props)
    | false -> struct (result.Infos, result.Props)

/// <summary>The reference syllable chunking, the loop of <c>_hb_next_syllable</c>.</summary>
/// <param name="props">The properties.</param>
/// <returns>The chunks.</returns>
let private referenceChunks (props: GlyphProps array) : struct (int * int) array =
    let n = props.Length
    let mutable result = []
    let mutable start = 0
    while start < n do
        let count = start + min (n - start) 64
        let syllable = props[start].Syllable
        let mutable finish = start + 1
        while finish < count && props[finish].Syllable = syllable do
            finish <- finish + 1
        result <- struct (start, finish) :: result
        start <- finish
    List.toArray (List.rev result)

/// <summary>The reference stable insertion sort, <c>hb_stable_sort</c>.</summary>
/// <param name="items">The items.</param>
/// <returns>The stably sorted items.</returns>
let private referenceStableSort (items: int array) : int array =
    let result = Array.copy items
    for i in 1 .. result.Length - 1 do
        let mutable j = i
        while j > 0 && result[j - 1] > result[i] do
            j <- j - 1
        if j <> i then
            let t = result[i]
            Array.blit result j result (j + 1) (i - j)
            result[j] <- t
    result

/// <summary>A random integer array with duplicates, for the stable-sort property.</summary>
let private genItems : Gen<int array> =
    gen {
        let! n = Gen.choose (0, 20)
        return! Gen.arrayOfLength n (Gen.choose (0, 5))
    }

/// <summary>The P6.S10 syllable-edit property tests and their fixed edge cases.</summary>
[<Tests>]
let tests =
    testList "P6.S10" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 }
            "rewriteSegments with random local merges equals the whole-run merge_clusters"
            (Prop.forAll (Arb.fromGen genCase) (fun struct (syllables, clusters, masks, level, merges) ->
                let struct (infos, props) = buildRun clusters masks syllables
                let chunkList = referenceChunks props
                let absolute = absoluteMerges chunkList merges
                let struct (oursInfos, oursProps) = rewrite infos props level merges
                let struct (refInfos, refProps) = reference infos props level absolute
                oursInfos.AsSpan().ToArray() = refInfos.AsSpan().ToArray() && oursProps.AsSpan().ToArray() = refProps.AsSpan().ToArray()))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 }
            "chunks equals the reference _hb_next_syllable"
            (Prop.forAll (Arb.fromGen (gen {
                let! n = Gen.choose (0, 40)
                return! Gen.arrayOfLength n (Gen.choose (0, 255) |> Gen.map byte)
            }))
                (fun syllables ->
                    let props = Array.map (fun s -> { Unchecked.defaultof<GlyphProps> with Syllable = s }) syllables
                    let ours =
                        SyllableEdit.chunks (Sealed.ofSpan (ReadOnlySpan props))
                        |> Seq.map (fun (struct (s, e)) -> struct (s, e))
                        |> Seq.toArray
                    let expected = referenceChunks props
                    ours = expected))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 }
            "stableSortBy equals the reference insertion sort"
            (Prop.forAll (Arb.fromGen genItems) (fun items ->
                let ours = Sealed.stableSortBy id (Sealed.ofSpan (ReadOnlySpan items))
                ours.AsSpan().ToArray() = referenceStableSort items))
        testCase "empty and one-glyph runs chunk and rewrite identically" (fun () ->
            for n in [ 0; 1 ] do
                let syllables = Array.create n 7uy
                let clusters = Array.init n (fun i -> uint32 i)
                let masks = Array.zeroCreate<uint32> n
                let struct (infos, props) = buildRun clusters masks syllables
                let merges = Array.create (if n = 0 then 0 else 1) [||]
                let struct (oursInfos, oursProps) = rewrite infos props ClusterLevel.MonotoneGraphemes merges
                Expect.equal oursInfos.Length n "rewrite keeps the glyph count"
                Expect.equal oursProps.Length n "rewrite keeps the property count")
        testCase "a syllable of 64 glyphs and one of 65 chunk at the cap" (fun () ->
            for n in [ 64; 65 ] do
                let syllables = Array.create n 1uy
                let props = Array.map (fun s -> { Unchecked.defaultof<GlyphProps> with Syllable = s }) syllables
                let chunks =
                    SyllableEdit.chunks (Sealed.ofSpan (ReadOnlySpan props))
                    |> Seq.map (fun (struct (s, e)) -> struct (s, e))
                    |> Seq.toArray
                let expected = referenceChunks props
                Expect.equal chunks expected $"chunks split a {n}-glyph syllable at 64")
        testCase "move to the same index is the identity" (fun () ->
            let items = Sealed.init 5 id
            let moved = (Sealed.move 2 2 items).AsSpan().ToArray()
            let original = items.AsSpan().ToArray()
            Expect.equal moved original "move to the same index")
        testCase "move shifts both directions" (fun () ->
            let items = Sealed.init 5 id
            let right = (Sealed.move 0 4 items).AsSpan().ToArray()
            let left = (Sealed.move 4 0 items).AsSpan().ToArray()
            Expect.equal right [| 1; 2; 3; 4; 0 |] "move left to right"
            Expect.equal left [| 4; 0; 1; 2; 3 |] "move right to left")
        testCase "setItem replaces one element" (fun () ->
            let items = Sealed.init 5 id
            let replaced = (Sealed.setItem 2 99 items).AsSpan().ToArray()
            Expect.equal replaced [| 0; 1; 99; 3; 4 |] "setItem")
    ]
