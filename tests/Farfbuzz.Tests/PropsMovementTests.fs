module Farfbuzz.Tests.PropsMovementTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz

/// <summary>Builds a run whose glyphs are tagged: code point and GlyphIndex both equal index + 1.</summary>
/// <param name="length">The glyph count.</param>
/// <returns>The tagged run.</returns>
let private taggedRun (length: int) : GlyphRun =
    let infos =
        Sealed.init length (fun index ->
            { Codepoint = uint32 (index + 1); Mask = 0u; Cluster = uint32 (index % 3); Var1 = 0u; Var2 = 0u })
    let props =
        Sealed.init length (fun index -> { Unchecked.defaultof<GlyphProps> with GlyphIndex = uint32 (index + 1) })
    let positions =
        Sealed.init length (fun index -> { XAdvance = index; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with Infos = infos; Props = props; Positions = positions; HavePositions = true }

/// <summary>One buffer operation that reorders, deletes, inserts or appends glyphs.</summary>
[<Struct>]
type private ReorderOp =
    | OpReverse
    | OpReverseRange of start: int * finish: int
    | OpReverseClusters
    | OpReverseGroups
    | OpDeleteInplace
    | OpSortClusters
    | OpSetLength of length: int
    | OpAppend of sourceLength: int * start: int * finish: int

/// <summary>A grouping that keeps every glyph its own group.</summary>
[<Struct>]
type private NeverGroup =
    interface IGlyphGrouping with
        member _.Group (_, _) = false

/// <summary>A predicate that deletes glyphs with an even code point.</summary>
[<Struct>]
type private EvenDelete =
    interface IGlyphPredicate with
        member _.Delete (info, _) = info.Codepoint % 2u = 0u

/// <summary>A comparer ordering glyphs by descending code point.</summary>
[<Struct>]
type private PropsDescendingComparer =
    interface IGlyphComparer with
        member _.Compare (a, b) = int b.Codepoint - int a.Codepoint

/// <summary>Generates a bounded reorder operation for a run length.</summary>
/// <param name="length">The run length.</param>
/// <returns>The operation.</returns>
let private reorderOp (length: int) : Gen<ReorderOp> =
    gen {
        let! kind = Gen.choose (0, 7)
        match kind with
        | 0 -> return OpReverse
        | 1 ->
            let! start = Gen.choose (0, length + 1)
            let! finish = Gen.choose (0, length + 1)
            return OpReverseRange (start, finish)
        | 2 -> return OpReverseClusters
        | 3 -> return OpReverseGroups
        | 4 -> return OpDeleteInplace
        | 5 -> return OpSortClusters
        | 6 ->
            let! newLength = Gen.choose (0, length + 3)
            return OpSetLength newLength
        | _ ->
            let! sourceLength = Gen.choose (0, 8)
            let! start = Gen.choose (0, sourceLength + 1)
            let! finish = Gen.choose (0, sourceLength + 1)
            return OpAppend (sourceLength, start, finish)
    }

/// <summary>Applies one reorder operation to a run.</summary>
/// <param name="op">The operation.</param>
/// <param name="run">The run.</param>
/// <returns>The run after the operation.</returns>
let private apply (op: ReorderOp) (run: GlyphRun) : GlyphRun =
    match op with
    | OpReverse -> GlyphRun.reverse run
    | OpReverseRange (start, finish) -> GlyphRun.reverseRange start finish run
    | OpReverseClusters -> GlyphRun.reverseClusters run
    | OpReverseGroups -> GlyphRun.reverseGroups (NeverGroup()) false run
    | OpDeleteInplace -> GlyphRun.deleteGlyphsInplace (EvenDelete()) run
    | OpSortClusters -> GlyphRun.sortClusters 0 run.Infos.Length (PropsDescendingComparer()) run
    | OpSetLength length -> GlyphRun.setLength length run
    | OpAppend (sourceLength, start, finish) -> GlyphRun.append (taggedRun sourceLength) start finish run

/// <summary>Whether the properties and positions are always as long as the glyphs.</summary>
/// <param name="run">The run.</param>
/// <returns>True when the invariant holds.</returns>
let private invariant (run: GlyphRun) : bool =
    match run.Props.Length = run.Infos.Length with
    | false -> false
    | true ->
        match run.HavePositions with
        | false -> true
        | true -> run.Positions.Length = run.Infos.Length

/// <summary>Whether every surviving glyph still carries the properties tagged to its code point.</summary>
/// <param name="run">The run.</param>
/// <returns>True when the properties are parallel to the infos.</returns>
let private tagsFollow (run: GlyphRun) : bool =
    let infos = run.Infos.AsSpan()
    let props = run.Props.AsSpan()
    let mutable index = 0
    let mutable follows = true
    while index < infos.Length && follows do
        follows <- props[index].GlyphIndex = infos[index].Codepoint
        index <- index + 1
    follows

/// <summary>Whether a run keeps the invariant and, for tagged runs, the tags with their glyphs.</summary>
/// <param name="run">The run.</param>
/// <returns>True when both checks hold.</returns>
let private invariantAndTags (run: GlyphRun) : bool =
    invariant run && tagsFollow run

/// <summary>The random input for the property: a run length and one reorder operation.</summary>
let private reorderInput =
    gen {
        let! length = Gen.choose (0, 16)
        let! op = reorderOp length
        return struct (length, op)
    }

/// <summary>The property that every reorder moves properties with the glyphs, and the fixed edge cases.</summary>
[<Tests>]
let tests =
    testList "Props movement" [
        testProperty "every reorder, delete, insert and append moves properties with the glyphs"
            (Prop.forAll (Arb.fromGen reorderInput) (fun struct (length, op) ->
                apply op (taggedRun length) |> invariantAndTags))
        testCase "the empty run grows and shrinks with properties as long as the glyphs" (fun () ->
            let run = taggedRun 0
            Expect.isTrue (invariantAndTags run) "empty"
            let grown = GlyphRun.setLength 3 run
            Expect.isTrue (invariantAndTags grown) "grown from empty"
            Expect.equal grown.Props.Length 3 "three zeroed properties"
            let shrunk = GlyphRun.setLength 0 grown
            Expect.isTrue (invariantAndTags shrunk) "shrunk to empty"
            Expect.equal shrunk.Props.Length 0 "no properties")
        testCase "one glyph keeps its properties through every reorder" (fun () ->
            let run = taggedRun 1
            Expect.isTrue (invariantAndTags run) "seed"
            Expect.isTrue (apply OpReverse run |> invariantAndTags) "reverse"
            Expect.isTrue (apply OpReverseClusters run |> invariantAndTags) "reverse clusters"
            Expect.isTrue (apply OpReverseGroups run |> invariantAndTags) "reverse groups"
            Expect.isTrue (apply OpSortClusters run |> invariantAndTags) "sort clusters"
            Expect.isTrue (apply OpDeleteInplace run |> invariantAndTags) "delete in place")
        testCase "a run with empty properties gains zeroed properties as it grows" (fun () ->
            let infos =
                Sealed.init 2 (fun index ->
                    { Codepoint = uint32 (index + 1); Mask = 0u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
            let run = { GlyphRun.empty with Infos = infos }
            let grown = GlyphRun.setLength 4 run
            Expect.isTrue (invariant grown) "growing zeroes the new properties"
            Expect.equal grown.Props.Length 4 "four properties"
            Expect.isTrue (invariant (GlyphRun.reverse run)) "reverse fills the properties"
            Expect.isTrue (invariant (GlyphRun.reverseClusters run)) "reverse clusters fills the properties"
            Expect.isTrue (invariant (GlyphRun.append (taggedRun 1) 0 1 run)) "append fills the properties")
        testCase "the reported seed passes" (fun () ->
            let replay = { Rnd = Rnd(2909071192197832099UL, 17379836621394946507UL); Size = None }
            let config = FsCheck.Config.QuickThrowOnFailure.WithReplay(Some replay).WithQuietOnSuccess(true)
            FsCheck.Check.One(config, Prop.forAll (Arb.fromGen reorderInput) (fun struct (length, op) ->
                apply op (taggedRun length) |> invariantAndTags)))
    ]
