module Farfbuzz.Tests.ComplexShapers.SyllableTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.Allocation

/// <summary>Generates a list of disjoint ascending runs within [0, n).</summary>
/// <param name="pos">The first allowed start index.</param>
/// <param name="n">The buffer length.</param>
/// <returns>The runs.</returns>
let rec private genRuns (pos: int) (n: int) : Gen<struct (int * int * byte) array> =
    gen {
        let! more = Gen.elements [ false; true ]
        match pos < n && more with
        | false -> return [||]
        | true ->
            let! start = Gen.choose (pos, n)
            let! stop = Gen.choose (start, n)
            let! value = Gen.choose (0, 255) |> Gen.map byte
            let! rest = genRuns stop n
            return Array.append [| struct (start, stop, value) |] rest
    }

/// <summary>A case: a byte array and a list of disjoint ascending runs to paint over it.</summary>
let private genCase : Gen<struct (byte array * struct (int * int * byte) array)> =
    gen {
        let! n = Gen.choose (0, 20)
        let! previous = Gen.arrayOfLength n (Gen.choose (0, 255) |> Gen.map byte)
        let! runs = genRuns 0 n
        return struct (previous, runs)
    }

/// <summary>The reference painting: paint each run's [start, stop) with its value over a copy of previous.</summary>
/// <param name="previous">The previous bytes.</param>
/// <param name="runs">The runs.</param>
/// <returns>The painted bytes.</returns>
let private reference (previous: byte array) (runs: struct (int * int * byte) array) : byte array =
    let target = Array.copy previous
    for struct (start, stop, value) in runs do
        for i in start .. stop - 1 do
            target[i] <- value
    target

/// <summary>Farfbuzz's painting through Sealed.paintRuns.</summary>
/// <param name="previous">The previous bytes.</param>
/// <param name="runs">The runs.</param>
/// <returns>The painted bytes.</returns>
let private ours (previous: byte array) (runs: struct (int * int * byte) array) : byte array =
    let source = RunQueueSource runs
    let struct (painted, _) = Sealed.paintRuns (Sealed.ofSpan (ReadOnlySpan previous)) source 0
    painted.AsSpan().ToArray()

/// <summary>The P6.S04 paintRuns property case.</summary>
[<Tests>]
let tests =
    testList "P6.S04" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 } "paintRuns paints exactly the emitted runs and keeps the rest"
            (Prop.forAll (Arb.fromGen genCase) (fun struct (previous, runs) -> ours previous runs = reference previous runs))
    ]

/// <summary>Builds a glyph from one number, every field of it different.</summary>
/// <param name="seed">The number.</param>
/// <param name="position">The position.</param>
/// <returns>The glyph and its properties.</returns>
let private glyphOf (seed: int) (position: byte) =
    let info : GlyphInfo = { Codepoint = uint32 seed; Mask = uint32 (seed * 7919); Cluster = uint32 seed; Var1 = uint32 (seed + 1); Var2 = uint32 (seed + 2) }
    let props : GlyphProps =
        { UnicodeProps = uint16 (seed * 31)
          GlyphClass = uint16 (seed * 17)
          LigProps = byte (seed * 3)
          Syllable = byte (seed * 5)
          ShaperCategory = byte (seed * 11)
          ShaperAuxiliary = position
          GlyphIndex = uint32 (seed * 101) }
    struct (info, props)

/// <summary>A case: the positions of the glyphs of a run and a range inside it.</summary>
let private genSortCase : Gen<struct (byte array * int * int)> =
    gen {
        let! n = Gen.choose (0, 20)
        let! positions = Gen.arrayOfLength n (Gen.choose (0, 14) |> Gen.map byte)
        let! start = Gen.choose (0, n)
        let! finish = Gen.choose (start, n)
        return struct (positions, start, finish)
    }

/// <summary>The reference of the sort with offsets: the glyphs of the range take their offsets as syllable bytes and are sorted stably by position.</summary>
/// <param name="glyphs">The glyphs and their properties.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The glyphs with the range sorted.</returns>
let private sortReference (glyphs: struct (GlyphInfo * GlyphProps) array) (start: int) (finish: int) =
    let range =
        glyphs[start .. finish - 1]
        |> Array.mapi (fun offset (struct (info, props)) -> struct (info, { props with Syllable = byte offset }))
        |> List.ofArray
        |> List.sortBy (fun (struct (_, props)) -> props.ShaperAuxiliary)
        |> Array.ofList
    Array.concat [ glyphs[.. start - 1]; range; glyphs[finish ..] ]

/// <summary>A case: the clusters and masks of the glyphs of a run, a range inside it and a monotone cluster level.</summary>
let private genMergeCase : Gen<struct (struct (uint32 * uint32) array * int * int * bool)> =
    gen {
        let! n = Gen.choose (0, 12)
        let! glyphs = Gen.arrayOfLength n (Gen.zip (Gen.choose (0, 5) |> Gen.map uint32) (Gen.choose (0, 63) |> Gen.map uint32))
        let! start = Gen.choose (0, n)
        let! finish = Gen.choose (start, n)
        let! graphemes = Gen.elements [ false; true ]
        return struct (glyphs |> Array.map (fun (cluster, mask) -> struct (cluster, mask)), start, finish, graphemes)
    }

/// <summary>The reference of the cluster merge at a monotone level, <c>merge_clusters_impl</c>: the smallest cluster of the range, the range grown
/// over the glyphs of the clusters it touches, and a glyph whose cluster changes loses the low three bits of its mask.</summary>
/// <param name="glyphs">The clusters and masks.</param>
/// <param name="start">The first index.</param>
/// <param name="finish">The end index.</param>
/// <returns>The clusters and masks after the merge.</returns>
let private mergeReference (glyphs: struct (uint32 * uint32) array) (start: int) (finish: int) =
    match finish - start < 2 with
    | true -> glyphs
    | false ->
        let cluster = glyphs[start .. finish - 1] |> Array.map (fun (struct (c, _)) -> c) |> Array.min
        let clusterAt index = let struct (c, _) = glyphs[index] in c
        let last = if cluster <> clusterAt (finish - 1) then (Seq.initInfinite (fun step -> finish + step) |> Seq.find (fun at -> at >= glyphs.Length || clusterAt (at - 1) <> clusterAt at)) else finish
        let first = if cluster <> clusterAt start then (Seq.initInfinite (fun step -> start - step) |> Seq.find (fun at -> at <= 0 || clusterAt (at - 1) <> clusterAt at)) else start
        glyphs |> Array.mapi (fun index (struct (c, m)) -> match index >= first && index < last && c <> cluster with | true -> struct (cluster, m &&& ~~~7u) | false -> struct (c, m))

/// <summary>The property tests of the in-place writers of a syllable's properties and masks.</summary>
[<Tests>]
let writerTests =
    testList "syllable in-place writers" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 } "the sort with offsets is the offsets then the stable sort by position"
            (Prop.forAll (Arb.fromGen genSortCase) (fun struct (positions, start, finish) ->
                let glyphs = positions |> Array.mapi (fun index position -> glyphOf (index + 1) position)
                let infos = Sealed.ofSpan (ReadOnlySpan (glyphs |> Array.map (fun (struct (info, _)) -> info)))
                let props = Sealed.ofSpan (ReadOnlySpan (glyphs |> Array.map (fun (struct (_, props)) -> props)))
                SyllableOwned.sortByAuxiliaryIndexing infos props start finish
                let expected = sortReference glyphs start finish
                infos.AsSpan().ToArray() = (expected |> Array.map (fun (struct (info, _)) -> info))
                && props.AsSpan().ToArray() = (expected |> Array.map (fun (struct (_, props)) -> props))))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 } "the byte writers change their own field and no other"
            (Prop.forAll (Arb.fromGen genSortCase) (fun struct (positions, start, finish) ->
                let glyphs = positions |> Array.mapi (fun index position -> glyphOf (index + 1) position)
                let infos = Sealed.ofSpan (ReadOnlySpan (glyphs |> Array.map (fun (struct (info, _)) -> info)))
                let props = Sealed.ofSpan (ReadOnlySpan (glyphs |> Array.map (fun (struct (_, props)) -> props)))
                SyllableOwned.fillSyllable props start finish 201uy
                for index in start .. finish - 1 do
                    SyllableOwned.setAuxiliary props index 9uy
                    SyllableOwned.orMask infos index 0x110u
                    SyllableOwned.clearMask infos index 0x3u
                let expected =
                    glyphs
                    |> Array.mapi (fun index (struct (info, props)) ->
                        match index >= start && index < finish with
                        | true -> struct ({ info with Mask = (info.Mask ||| 0x110u) &&& ~~~0x3u }, { props with Syllable = 201uy; ShaperAuxiliary = 9uy })
                        | false -> struct (info, props))
                infos.AsSpan().ToArray() = (expected |> Array.map (fun (struct (info, _)) -> info))
                && props.AsSpan().ToArray() = (expected |> Array.map (fun (struct (_, props)) -> props))))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 1000 } "the cluster merge in place is merge_clusters at the monotone levels"
            (Prop.forAll (Arb.fromGen genMergeCase) (fun struct (glyphs, start, finish, graphemes) ->
                let level = match graphemes with | true -> ClusterLevel.MonotoneGraphemes | false -> ClusterLevel.MonotoneCharacters
                let infos =
                    Sealed.ofSpan (ReadOnlySpan (glyphs |> Array.mapi (fun index (struct (cluster, mask)) -> { Codepoint = uint32 index; Mask = mask; Cluster = cluster; Var1 = 0u; Var2 = 0u })))
                SyllableOwned.mergeClusters level infos start finish
                let expected = mergeReference glyphs start finish
                (infos.AsSpan().ToArray() |> Array.map (fun info -> struct (info.Cluster, info.Mask))) = expected))
    ]
