module Farfbuzz.Bench.Toolkit

open System
open System.Linq
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Harness

/// <summary>A struct folder summing integers.</summary>
[<Struct>]
type Sum =
    interface IFolder<int64, int> with
        member _.Step state item = state + int64 item

/// <summary>The functional toolkit's folds against FSharp.Core and the BCL's vectorised sum, on 100 000 integers.</summary>
[<MemoryDiagnoser>]
type Folds() =
    let mutable items: int array = [||]

    /// <summary>Creates the integers.</summary>
    [<GlobalSetup>]
    member _.Setup() = items <- Array.init 100_000 (fun index -> index % 977)

    /// <summary>Spans.foldWith with a struct folder: specialised, no closure.</summary>
    /// <returns>The sum.</returns>
    [<Benchmark>]
    member _.FoldWithStructFolder() = Spans.foldWith (Sum()) 0L (ReadOnlySpan items) |> TailCalls.settle

    /// <summary>Spans.fold with an F# closure.</summary>
    /// <returns>The sum.</returns>
    [<Benchmark>]
    member _.FoldWithClosure() = Spans.fold (fun state item -> state + int64 item) 0L (ReadOnlySpan items) |> TailCalls.settle

    /// <summary>FSharp.Core's Array.fold.</summary>
    /// <returns>The sum.</returns>
    [<Benchmark(Baseline = true)>]
    member _.ArrayFold() = Array.fold (fun state item -> state + int64 item) 0L items

    /// <summary>The BCL's vectorised Enumerable.Sum over the array (int32 accumulation; the inputs cannot overflow it).</summary>
    /// <returns>The sum.</returns>
    [<Benchmark>]
    member _.VectorisedLinqSum() = int64 (items.Sum())
