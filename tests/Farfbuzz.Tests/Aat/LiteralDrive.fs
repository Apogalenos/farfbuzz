module Farfbuzz.Tests.LiteralDrive

open System
open Farfbuzz
open Farfbuzz.Aat

/// <summary>The kern mask the literal sets flags against.</summary>
[<Literal>]
let KernMaskBit = 0x80000000u

/// <summary>The inputs of one literal state machine run.</summary>
type internal LiteralInput =
    { Machine: StateMachine
      Codepoints: uint32 array
      Clusters: uint32 array
      Masks: uint32 array
      Positions: GlyphPosition array
      Level: ClusterLevel
      MaxOps: int
      NumGlyphs: int }

/// <summary>A literal transliteration of <c>StateTableDriver::drive</c> (src/hb-aat-layout-common.hh:1282-1442) over an
/// in-place buffer, with the buffer flag setting of src/hb-buffer.hh:456-520 and 640-702, one glyph per iteration, mutable
/// as the C is. The subtable's transition is a callback that edits the buffer.</summary>
type internal LiteralDrive(input: LiteralInput) =
    let len = input.Codepoints.Length
    let masks = Array.copy input.Masks
    let positions = Array.copy input.Positions
    let mutable idx = 0
    let mutable maxOps = input.MaxOps
    let mutable successful = true
    let mutable scratch = 0u

    member _.Len = len
    member _.Masks = masks
    member _.Positions = positions
    member _.Codepoints = input.Codepoints

    member _.Idx = idx

    member _.MaxOps
        with get () = maxOps
        and set value = maxOps <- value

    member _.Successful = successful

    member _.Scratch
        with get () = scratch
        and set value = scratch <- value

    member private _.InfosSetFlags (start: int, finish: int, cluster: uint32, mask: uint32) =
        match start = finish with
        | true -> ()
        | false ->
            maxOps <- maxOps - (finish - start)
            if maxOps < 0 then successful <- false
            let clusterFirst = input.Clusters[start]
            let clusterLast = input.Clusters[finish - 1]
            match input.Level = ClusterLevel.Characters || (cluster <> clusterFirst && cluster <> clusterLast) with
            | true ->
                for i in start .. finish - 1 do
                    if cluster <> input.Clusters[i] then masks[i] <- masks[i] ||| mask
            | false ->
                match cluster = clusterFirst with
                | true ->
                    let mutable i = finish
                    while start < i && input.Clusters[i - 1] <> clusterFirst do
                        masks[i - 1] <- masks[i - 1] ||| mask
                        i <- i - 1
                | false ->
                    let mutable i = start
                    while i < finish && input.Clusters[i] <> clusterLast do
                        masks[i] <- masks[i] ||| mask
                        i <- i + 1

    member private _.FindMinCluster (start: int, finish: int) =
        match start = finish with
        | true -> UInt32.MaxValue
        | false ->
            match input.Level = ClusterLevel.Characters with
            | true ->
                let mutable cluster = UInt32.MaxValue
                for i in start .. finish - 1 do
                    cluster <- min cluster input.Clusters[i]
                cluster
            | false -> min UInt32.MaxValue (min input.Clusters[start] input.Clusters[finish - 1])

    member this.UnsafeToBreak (start: int, finish: int, fromOutBuffer: bool) =
        let mask = uint32 GlyphFlags.UnsafeToBreak ||| uint32 GlyphFlags.UnsafeToConcat
        match uint32 (finish - start) > 255u with
        | true -> ()
        | false ->
            let finish = min finish len
            match (not fromOutBuffer) && finish - start < 2 with
            | true -> ()
            | false -> this.InfosSetFlags (start, finish, this.FindMinCluster (start, finish), mask)

    member internal this.Drive (isActionable: Entry -> bool, transition: Entry -> unit) =
        let classOf glyph = StateTable.classOfCall input.Machine glyph input.NumGlyphs
        let getEntry state klass = StateTable.entryCall input.Machine state klass
        let startSafeEot = not (isActionable (getEntry StateTable.StartOfText StateTable.ClassEndOfText))
        let mutable state = 0
        let mutable finished = false
        while successful && not finished do
            let mutable klass = if idx < len then classOf input.Codepoints[idx] else StateTable.ClassEndOfText
            let mutable resume = true
            while resume && successful && not finished do
                resume <- false
                let entry = getEntry state klass
                let nextState = entry.NewState
                let notEpsilon = entry.Flags &&& 0x4000us = 0us
                let notActionable = not (isActionable entry)
                let isNull = state = StateTable.StartOfText && nextState = StateTable.StartOfText && startSafeEot && notActionable && notEpsilon
                match isNull with
                | true ->
                    let oldKlass = klass
                    let mutable go = true
                    while go do
                        transition entry
                        match idx = len || not successful with
                        | true -> go <- false
                        | false ->
                            idx <- idx + 1
                            klass <- if idx < len then classOf input.Codepoints[idx] else StateTable.ClassEndOfText
                            go <- klass = oldKlass
                    match idx = len || not successful with
                    | true -> finished <- true
                    | false -> resume <- true
                | false ->
                    let wouldBe = getEntry StateTable.StartOfText klass
                    let safe =
                        (not (isActionable entry))
                        && (state = StateTable.StartOfText
                            || (entry.Flags &&& 0x4000us <> 0us && nextState = StateTable.StartOfText)
                            || ((not (isActionable wouldBe)) && nextState = wouldBe.NewState && (entry.Flags &&& 0x4000us) = (wouldBe.Flags &&& 0x4000us)))
                        && not (isActionable (getEntry state StateTable.ClassEndOfText))
                    if (not safe) && idx > 0 && idx < len then this.UnsafeToBreak (idx - 1, idx + 1, true)
                    transition entry
                    state <- nextState
                    match idx = len with
                    | true -> finished <- true
                    | false ->
                        let before = maxOps
                        match notEpsilon with
                        | true -> idx <- idx + 1
                        | false ->
                            maxOps <- maxOps - 1
                            if before <= 0 then idx <- idx + 1
