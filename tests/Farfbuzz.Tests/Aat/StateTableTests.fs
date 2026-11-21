module Farfbuzz.Tests.AatStateTableTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Aat

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The two bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Builds an obsolete (16-bit) state table from its state rows and entries.</summary>
/// <param name="minState">The smallest state row laid out.</param>
/// <param name="maxState">The largest state row laid out.</param>
/// <param name="nClasses">The class count.</param>
/// <param name="stateValue">The entry index of a state and class.</param>
/// <param name="entries">Each entry's raw <c>newState</c> byte offset and flags.</param>
/// <param name="firstGlyph">The class table's first glyph.</param>
/// <param name="classes">The class bytes.</param>
/// <returns>The table bytes.</returns>
let private buildObsolete
    (minState: int) (maxState: int) (nClasses: int) (stateValue: int -> int -> int)
    (entries: (uint16 * uint16) list) (firstGlyph: uint16) (classes: byte array) =
    let numNeg = -minState
    let stateArrayOffset = 8 + numNeg * nClasses
    let stateArrayLen = (maxState - minState + 1) * nClasses
    let classTableOffset = 8 + stateArrayLen
    let entryTableOffset = classTableOffset + 4 + classes.Length
    let stateArray =
        [| for s in minState .. maxState do
             for k in 0 .. nClasses - 1 do
                yield byte (stateValue s k) |]
    let entryBytes =
        [| for (newState, flags) in entries do
             yield! be16 (uint32 newState)
             yield! be16 (uint32 flags) |]
    Array.concat
        [ be16 (uint32 nClasses); be16 (uint32 classTableOffset); be16 (uint32 stateArrayOffset); be16 (uint32 entryTableOffset)
          stateArray
          be16 (uint32 firstGlyph); be16 (uint32 classes.Length); classes
          entryBytes ]

/// <summary>The entry count a sanitizer derives, or -1 when the table fails.</summary>
/// <param name="table">The bytes.</param>
/// <param name="extended">True for the extended form.</param>
/// <param name="entryDataWords">The entry data words.</param>
/// <returns>The entry count.</returns>
let private entryCount (table: byte array) (extended: bool) (entryDataWords: int) =
    match StateTable.sanitize (ReadOnlySpan table) 0 extended entryDataWords 100 (Sanitize.start table.Length) with
    | ValueSome (struct (_, count)) -> count
    | ValueNone -> -1

/// <summary>The expected range list.</summary>
/// <param name="pairs">The first and last glyphs.</param>
/// <returns>The ranges.</returns>
let private ranges (pairs: (uint32 * uint32) list) =
    ImmutableArray.CreateRange (List.toArray (List.map (fun (first, last) -> { First = first; Last = last }) pairs))

/// <summary>The classifier that is never actionable or initiable.</summary>
[<Struct>]
type private NeverActionable =
    interface IEntryClassifier with
        member _.IsActionable _ = false
        member _.IsActionInitiable _ = false

/// <summary>The classifier that acts when an entry carries the 0xBBBB flag.</summary>
[<Struct>]
type private ActionableWhenFlags =
    interface IEntryClassifier with
        member _.IsActionable entry = entry.Flags = 0xBBBBus
        member _.IsActionInitiable _ = false

/// <summary>The P8.S02 state-table unit tests, on hand-built bytes.</summary>
[<Tests>]
let tests =
    testList "P8.S02" [
        test "negative states are admitted and decodable" {
            let table =
                buildObsolete
                    -1 0 4
                    (fun s k ->
                        match s, k with
                        | 0, 2 -> 1
                        | -1, 1 -> 1
                        | _ -> 0)
                    [ 12us, 0x1111us; 8us, 0x2222us ]
                    0us [| 0uy; 1uy; 2uy; 3uy |]
            Expect.equal (entryCount table false 0) 2 "the sanitizer admits two entries"
            let machine = StateTable.read (ReadOnlyMemory table) 0 false 0
            Expect.equal machine.RowCount 2 "two reachable states"
            Expect.equal machine.StateBias 1 "the negative state is biased"
            Expect.equal machine.Entries.Length 2 "two decoded entries"
            Expect.equal (StateTable.entryCall machine 0 2us).NewState -1 "state 0 class 2 leads to state -1"
            Expect.equal (StateTable.entryCall machine 0 2us).Flags 0x2222us "the negative state's entry"
            Expect.equal (StateTable.entryCall machine (-1) 1us).NewState -1 "the negative state is readable"
        }
        test "a negative new_state truncates toward zero" {
            let table =
                buildObsolete
                    -1 0 4
                    (fun _ _ -> 0)
                    [ 9us, 0xAAAAus ]
                    0us [| 0uy; 1uy; 2uy; 3uy |]
            let machine = StateTable.read (ReadOnlyMemory table) 0 false 0
            Expect.equal (StateTable.entryCall machine 0 0us).NewState 0 "(9 - 12) / 4 truncates to zero, not minus one"
            Expect.equal (StateTable.entryCall machine 0 0us).Flags 0xAAAAus "the entry flags survive"
        }
        test "a class at or above the class count is out-of-bounds" {
            let table =
                buildObsolete
                    0 0 4
                    (fun s k ->
                        match s, k with
                        | 0, 1 -> 1
                        | _ -> 0)
                    [ 8us, 0x1111us; 8us, 0x2222us ]
                    0us [| 0uy; 1uy; 2uy; 3uy |]
            let machine = StateTable.read (ReadOnlyMemory table) 0 false 0
            let outOfBounds = StateTable.entryCall machine 0 4us
            let classOne = StateTable.entryCall machine 0 1us
            Expect.equal outOfBounds.NewState classOne.NewState "class 4 reads as the out-of-bounds class 1"
            Expect.equal outOfBounds.Flags classOne.Flags "the out-of-bounds entry is entry 1"
            Expect.equal outOfBounds.Flags 0x2222us "the out-of-bounds class maps to entry 1"
        }
        test "a deleted glyph is class 2 before any lookup" {
            let table =
                buildObsolete
                    0 0 4
                    (fun _ _ -> 0)
                    [ 8us, 0us ]
                    10us [| 0uy; 2uy; 1uy; 3uy |]
            let machine = StateTable.read (ReadOnlyMemory table) 0 false 0
            Expect.equal (StateTable.classOfCall machine 0xFFFFu 100) 2us "the deleted glyph"
            Expect.equal (StateTable.classOfCall machine 11u 100) 2us "the class table's class 2"
            Expect.equal (StateTable.classOfCall machine 10u 100) 0us "the class table's class 0"
        }
        test "initial glyphs include the deleted glyph when class 2 is reachable" {
            let table =
                buildObsolete
                    0 0 4
                    (fun s k ->
                        match s, k with
                        | 0, 1 -> 1
                        | 0, 2 -> 1
                        | _ -> 0)
                    [ 8us, 0x1111us; 8us, 0xBBBBus ]
                    10us [| 0uy; 2uy; 1uy; 3uy |]
            let machine = StateTable.read (ReadOnlyMemory table) 0 false 0
            Expect.equal (StateTable.initialGlyphs machine 100 (ActionableWhenFlags())) (ranges [ 11u, 12u; 0xFFFFu, 0xFFFFu ]) "classes 1 and 2 plus the deleted glyph"
        }
        test "more than 512 classes collect the whole class table" {
            let classTableBytes = Array.concat [ be16 0u; be16 3u; [| 1uy; 2uy; 3uy |] ]
            let table = { ClassTable.Table = ReadOnlyMemory classTableBytes; At = 0; FirstGlyph = 0us; Count = 3; Wide = false }
            let machine =
                { ClassCount = 600
                  Classes = Narrow table
                  States = ImmutableArray.Empty
                  StateBias = 0
                  RowCount = 0
                  Entries = ImmutableArray.Empty
                  IsExtended = false }
            Expect.equal (StateTable.initialGlyphs machine 100 (NeverActionable())) (ranges [ 1u, 2u ]) "every non-out-of-bounds glyph"
        }
    ]
