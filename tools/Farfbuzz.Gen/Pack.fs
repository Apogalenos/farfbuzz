#nowarn "3261"

module Farfbuzz.Gen.Pack

open System
open System.Collections.Generic

/// <summary>A packed two-level table.</summary>
type PackedTable =
    { Shift: int
      Index: uint16 array
      Values: int array
      Default: int }

/// <summary>Compares integer blocks structurally.</summary>
type private BlockComparer() =
    interface IEqualityComparer<int array> with
        member _.Equals (a: int array, b: int array) =
            match a with
            | null -> false
            | _ ->
                match b with
                | null -> false
                | _ -> a.Length = b.Length && Array.forall2 (=) a b
        member _.GetHashCode (block: int array) =
            block |> Array.fold (fun hash value -> (hash * 31 + value) &&& 0x7FFFFFFF) 17

/// <summary>Packs a codepoint-indexed table into deduplicated fixed-size blocks.</summary>
/// <param name="values">The values for 0..0x10FFFF.</param>
/// <param name="defaultValue">The value outside the range.</param>
/// <returns>The packed table.</returns>
let trie (values: int array) (defaultValue: int) : PackedTable =
    let comparer = BlockComparer()
    let mutable best = None
    for shift in 6 .. 10 do
        let blockSize = 1 <<< shift
        let blockCount = (values.Length + blockSize - 1) / blockSize
        let blocks = Dictionary<int array, int>(comparer)
        let index = Array.zeroCreate<uint16> blockCount
        let mutable next = 0
        for blockIndex in 0 .. blockCount - 1 do
            let start = blockIndex * blockSize
            let stop = min values.Length (start + blockSize)
            let block = Array.init blockSize (fun offset -> if start + offset < stop then values[start + offset] else defaultValue)
            match blocks.TryGetValue block with
            | true, existing -> index[blockIndex] <- uint16 existing
            | false, _ ->
                blocks.Add(block, next)
                index[blockIndex] <- uint16 next
                next <- next + 1
        let valuesArray = Array.zeroCreate (next * blockSize)
        for pair in blocks do
            Array.blit pair.Key 0 valuesArray (pair.Value * blockSize) blockSize
        let cost = blockCount * 2 + valuesArray.Length * 4
        match best with
        | Some (bestCost, _) when bestCost <= cost -> ()
        | _ -> best <- Some (cost, { Shift = shift; Index = index; Values = valuesArray; Default = defaultValue })
    match best with
    | Some (_, table) -> table
    | None -> failwith "no packing"
