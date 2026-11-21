module Farfbuzz.Diff.AatStateTableDiff

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The four bytes.</returns>
let private be32 (value: uint32) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A generated state table and the glyphs to probe.</summary>
type private Case =
    { Bytes: byte array
      Extended: bool
      EntryDataWords: int
      NumGlyphs: int
      NumClasses: int
      Glyphs: uint32 array }

/// <summary>The compared state-table result.</summary>
type private Outcome =
    { Sane: bool
      NumEntries: int
      MinState: int
      MaxState: int
      Classes: uint32 array
      Entries: (int * uint32 * uint32 * uint32) array }

/// <summary>The empty outcome for a table that fails to sanitize.</summary>
/// <param name="glyphCount">The number of probe glyphs.</param>
/// <returns>The outcome.</returns>
let private emptyOutcome (glyphCount: int) =
    { Sane = false
      NumEntries = 0
      MinState = 0
      MaxState = 0
      Classes = Array.zeroCreate glyphCount
      Entries = [||] }

/// <summary>Farfbuzz's state-table result for a generated case.</summary>
/// <param name="case">The case.</param>
/// <returns>The outcome.</returns>
let private ours (case: Case) =
    match StateTable.sanitize (ReadOnlySpan case.Bytes) 0 case.Extended case.EntryDataWords case.NumGlyphs (Sanitize.start case.Bytes.Length) with
    | ValueNone -> emptyOutcome case.Glyphs.Length
    | ValueSome (struct (_, numEntries)) ->
        let machine = StateTable.read (ReadOnlyMemory case.Bytes) 0 case.Extended case.EntryDataWords
        let minState = -machine.StateBias
        let maxState = minState + machine.RowCount - 1
        let classes = case.Glyphs |> Array.map (fun glyph -> uint32 (StateTable.classOfCall machine glyph case.NumGlyphs))
        let entries =
            [| for state in minState .. maxState do
                 for klass in 0 .. case.NumClasses - 1 do
                   let e = StateTable.entryCall machine state (uint16 klass)
                   yield (e.NewState, uint32 e.Flags, uint32 e.Data0, uint32 e.Data1) |]
        { Sane = true
          NumEntries = numEntries
          MinState = minState
          MaxState = maxState
          Classes = classes
          Entries = entries }

/// <summary>The oracle's state-table result for a generated case.</summary>
/// <param name="case">The case.</param>
/// <returns>The outcome.</returns>
let private oracle (case: Case) =
    let data = case.Bytes
    let length = uint32 data.Length
    let extended = if case.Extended then 1u else 0u
    let entryDataWords = uint32 case.EntryDataWords
    let numGlyphs = uint32 case.NumGlyphs
    let mutable numEntries = 0u
    let mutable minState = 0
    let mutable maxState = 0
    let sane = Shims.farfbuzz_oracle_aat_state_table_sanitize(data, length, extended, entryDataWords, numGlyphs, &numEntries, &minState, &maxState) <> 0
    match sane with
    | false -> emptyOutcome case.Glyphs.Length
    | true ->
        let classes = case.Glyphs |> Array.map (fun glyph -> Shims.farfbuzz_oracle_aat_state_table_get_class(data, length, extended, entryDataWords, numGlyphs, glyph))
        let entries =
            [| for state in minState .. maxState do
                 for klass in 0 .. case.NumClasses - 1 do
                   let mutable newState = 0
                   let mutable flags = 0u
                   let mutable data0 = 0u
                   let mutable data1 = 0u
                   Shims.farfbuzz_oracle_aat_state_table_get_entry(data, length, extended, entryDataWords, numGlyphs, state, uint32 klass, &newState, &flags, &data0, &data1) |> ignore
                   yield (newState, flags, data0, data1) |]
        { Sane = true
          NumEntries = int numEntries
          MinState = minState
          MaxState = maxState
          Classes = classes
          Entries = entries }

/// <summary>Builds a well-formed state table from its parts.</summary>
/// <param name="extended">True for the 32-bit form.</param>
/// <param name="entryDataWords">The entry data words.</param>
/// <param name="nClasses">The class count.</param>
/// <param name="minState">The smallest state row.</param>
/// <param name="maxState">The largest state row.</param>
/// <param name="stateArray">The entry index of each state and class.</param>
/// <param name="entries">Each entry's state, flags and first data word.</param>
/// <param name="firstGlyph">The obsolete class table's first glyph.</param>
/// <param name="classValues">The class values.</param>
/// <returns>The table bytes.</returns>
let private buildStateTable
    (extended: bool) (entryDataWords: int) (nClasses: int) (minState: int) (maxState: int)
    (stateArray: int list) (entries: (int * int * int) list) (firstGlyph: int) (classValues: int list) =
    let hbuintSize = if extended then 4 else 2
    let stateUnitSize = if extended then 2 else 1
    let headerSize = 4 * hbuintSize
    let numNeg = -minState
    let stateArrayOffset = headerSize + numNeg * nClasses * stateUnitSize
    let stateArrayLen = (maxState - minState + 1) * nClasses * stateUnitSize
    let classTable =
        match extended with
        | true -> Array.concat [ be16 0u; classValues |> List.map (fun value -> be16 (uint32 value)) |> Array.concat ]
        | false -> Array.concat [ be16 (uint32 firstGlyph); be16 (uint32 classValues.Length); classValues |> List.map byte |> List.toArray ]
    let classTableOffset = headerSize + stateArrayLen
    let entryTableOffset = classTableOffset + classTable.Length
    let writeOffset (value: int) = if extended then be32 (uint32 value) else be16 (uint32 value)
    let header = Array.concat [ writeOffset nClasses; writeOffset classTableOffset; writeOffset stateArrayOffset; writeOffset entryTableOffset ]
    let stateArrayBytes =
        match extended with
        | true -> stateArray |> List.map (fun value -> be16 (uint32 value)) |> Array.concat
        | false -> stateArray |> List.map byte |> List.toArray
    let entryBytes =
        [| for (state, flags, data0) in entries do
             let rawNewState = if extended then state else stateArrayOffset + state * nClasses * stateUnitSize
             yield! be16 (uint32 rawNewState)
             yield! be16 (uint32 flags)
             if entryDataWords >= 1 then yield! be16 (uint32 data0) |]
    Array.concat [ header; stateArrayBytes; classTable; entryBytes ]

/// <summary>Generates a state-table case, well-formed or broken.</summary>
let private genStateCase =
    gen {
        let! extended = Gen.elements [ false; true ]
        let! entryDataWords = Gen.elements [ 0; 1 ]
        let hbuintSize = if extended then 4 else 2
        let stateUnitSize = if extended then 2 else 1
        let entrySize = 4 + 2 * entryDataWords
        let headerSize = 4 * hbuintSize
        let! nClasses = Gen.choose (4, 8)
        let! minState = Gen.choose (-2, 0)
        let! maxState = Gen.choose (0, 3)
        let! numGlyphs = Gen.choose (4, 20)
        let rowCount = maxState - minState + 1
        let! numEntries = Gen.choose (1, 6)
        let! stateArray = Gen.listOfLength (rowCount * nClasses) (Gen.choose (0, numEntries - 1))
        let! entries = Gen.listOfLength numEntries (gen {
            let! state = Gen.choose (minState, maxState)
            let! flags = Gen.choose (0, 0xFFFF)
            let! data0 = Gen.choose (0, 0xFFFF)
            return (state, flags, data0) })
        let! firstGlyph = Gen.choose (0, 5)
        let! classCount = Gen.choose (1, 6)
        let! classValues = Gen.listOfLength (if extended then numGlyphs else classCount) (Gen.choose (0, 4))
        let numNeg = -minState
        let stateArrayOffset = headerSize + numNeg * nClasses * stateUnitSize
        let stateArrayLen = (maxState - minState + 1) * nClasses * stateUnitSize
        let full = buildStateTable extended entryDataWords nClasses minState maxState stateArray entries firstGlyph classValues
        let! broken = Gen.elements [ 0; 0; 1; 2; 3; 4 ]
        let bytes =
            match broken with
            | 0 -> full
            | 1 -> Array.truncate (max 0 (full.Length - entrySize)) full
            | 2 ->
                let classTableOffset = headerSize + stateArrayLen
                let entryTableOffset = classTableOffset + (if extended then 2 + 2 * numGlyphs else 4 + classCount)
                let writeOffset (value: int) = if extended then be32 (uint32 value) else be16 (uint32 value)
                let header = Array.concat [ writeOffset 3; writeOffset classTableOffset; writeOffset stateArrayOffset; writeOffset entryTableOffset ]
                Array.concat [ header; Array.skip headerSize full ]
            | 3 ->
                let classTableOffset = headerSize + stateArrayLen
                let entryTableOffset = classTableOffset + (if extended then 2 + 2 * numGlyphs else 4 + classCount)
                let writeOffset (value: int) = if extended then be32 (uint32 value) else be16 (uint32 value)
                let header = Array.concat [ writeOffset nClasses; writeOffset classTableOffset; writeOffset (full.Length + 100); writeOffset entryTableOffset ]
                Array.concat [ header; Array.skip headerSize full ]
            | _ ->
                let classTableOffset = headerSize + stateArrayLen
                let entryTableOffset = classTableOffset + (if extended then 2 + 2 * numGlyphs else 4 + classCount)
                let writeOffset (value: int) = if extended then be32 (uint32 value) else be16 (uint32 value)
                let header = Array.concat [ writeOffset nClasses; writeOffset (full.Length + 100); writeOffset stateArrayOffset; writeOffset entryTableOffset ]
                Array.concat [ header; Array.skip headerSize full ]
        let! randoms = Gen.listOfLength 20 (Gen.choose (0, numGlyphs + 6) |> Gen.map uint32)
        let boundaries =
            [ 0xFFFFu; 0u; uint32 (numGlyphs - 1); uint32 numGlyphs
              uint32 (max 0 (firstGlyph - 1)); uint32 firstGlyph; uint32 (firstGlyph + classCount - 1); uint32 (firstGlyph + classCount) ]
        let glyphs = List.distinct (boundaries @ randoms)
        return { Bytes = bytes; Extended = extended; EntryDataWords = entryDataWords; NumGlyphs = numGlyphs; NumClasses = nClasses; Glyphs = Array.ofList glyphs }
    }

/// <summary>The fixed edge cases: empty, minimal and large-count tables.</summary>
/// <returns>The cases.</returns>
let private fixedCases =
    let probes = [| 0xFFFFu; 0u; 1u; 5u; 10u |]
    let empty (extended: bool) =
        { Bytes = [||]; Extended = extended; EntryDataWords = 0; NumGlyphs = 10; NumClasses = 4; Glyphs = probes }
    let minimal (extended: bool) (entryDataWords: int) =
        let nClasses = 4
        let numGlyphs = 10
        let classValues = if extended then List.replicate numGlyphs 0 else [ 0; 1; 2; 3 ]
        let bytes = buildStateTable extended entryDataWords nClasses 0 0 [ 0; 0; 0; 0 ] [ (0, 0x1111, 0x2222) ] 0 classValues
        { Bytes = bytes; Extended = extended; EntryDataWords = entryDataWords; NumGlyphs = numGlyphs; NumClasses = nClasses; Glyphs = probes }
    let large =
        let nClasses = 513
        let bytes = buildStateTable false 0 nClasses 0 0 (List.replicate nClasses 0) [ (0, 0, 0) ] 0 [ 0; 1; 2; 3 ]
        { Bytes = bytes; Extended = false; EntryDataWords = 0; NumGlyphs = 10; NumClasses = nClasses; Glyphs = probes }
    [ empty false; empty true; minimal false 0; minimal false 1; minimal true 0; minimal true 1; large ]

/// <summary>The P8.S02 generated state-table differential.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P8.S02" [
        sampled "generated state tables" genStateCase 5000 ours oracle
        exhaustive "fixed edge cases" fixedCases ours oracle
    ]
