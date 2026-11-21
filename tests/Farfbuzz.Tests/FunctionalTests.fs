module Farfbuzz.Tests.FunctionalTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.Allocation

/// <summary>A struct folder summing integers, for the zero-allocation fold.</summary>
[<Struct>]
type SumFolder =
    interface IFolder<int64, int> with
        member _.Step state item = state + int64 item

/// <summary>Sums a span through the struct folder.</summary>
/// <param name="items">The items.</param>
/// <returns>The sum.</returns>
let sumWith (items: int array) =
    Spans.foldWith (SumFolder()) 0L (ReadOnlySpan items) |> TailCalls.settle

/// <summary>Toolkit tests, P1.Functional.</summary>
[<Tests>]
let tests =
    testList "P1.Functional" [
        testProperty "Spans.fold equals Array.fold" <| fun (items: int array) ->
            Spans.fold (fun state item -> state * 31 + item) 17 (ReadOnlySpan items) = Array.fold (fun state item -> state * 31 + item) 17 items
        testProperty "Spans.foldi passes indices in order" <| fun (items: int array) ->
            Spans.foldi (fun state index item -> state + index * item) 0 (ReadOnlySpan items) = (items |> Array.mapi (fun index item -> index * item) |> Array.sum)
        testProperty "Spans.foldWith equals Array.sum" <| fun (items: int array) ->
            sumWith items = (items |> Array.sumBy int64)
        testProperty "Spans.tryFindIndex equals Array.tryFindIndex" <| fun (items: int array) (target: int) ->
            Spans.tryFindIndex ((=) target) (ReadOnlySpan items) |> ValueOption.toOption = Array.tryFindIndex ((=) target) items
        testProperty "Spans.exists and forall agree with Array" <| fun (items: int array) ->
            Spans.exists (fun item -> item % 3 = 0) (ReadOnlySpan items) = Array.exists (fun item -> item % 3 = 0) items
            && Spans.forall (fun item -> item % 3 = 0) (ReadOnlySpan items) = Array.forall (fun item -> item % 3 = 0) items
        testProperty "Ranges.fold covers the half-open range" <| fun (lower: int8) (count: byte) ->
            let first = int lower
            Ranges.fold first (first + int count) (fun state index -> state + int64 index) 0L = (seq { first .. first + int count - 1 } |> Seq.sumBy int64)
        testProperty "Sealed.init equals Array.init" <| fun (count: byte) ->
            (Sealed.init (int count) (fun index -> index * index)).AsSpan().ToArray() = Array.init (int count) (fun index -> index * index)
        testProperty "Sealed.mapi equals Array.mapi" <| fun (items: int array) ->
            (Sealed.mapi (fun index item -> index + item) (ReadOnlySpan items)).AsSpan().ToArray() = Array.mapi (fun index item -> index + item) items
        testProperty "Sealed.choose equals Array.choose" <| fun (items: int array) ->
            let chooser item = if item % 2 = 0 then ValueSome (item / 2) else ValueNone
            (Sealed.choose chooser (ReadOnlySpan items)).AsSpan().ToArray() = Array.choose (chooser >> ValueOption.toOption) items
        testProperty "Sealed.ofList keeps order" <| fun (items: int list) ->
            (Sealed.ofList items).AsSpan().ToArray() = List.toArray items
        test "a struct-folder fold allocates nothing" {
            let items = Array.init 10_000 id
            Expect.equal (bytesAllocatedBy (fun () -> sumWith items |> ignore)) 0L "bytes allocated"
        }
        test "Sealed.init allocates only its array" {
            let arrayBytes = int64 (sizeof<int> * 10_000)
            let allocated = bytesAllocatedBy (fun () -> Sealed.init 10_000 id |> ignore)
            Expect.isLessThanOrEqual allocated (arrayBytes + 256L) "bytes allocated beyond the array"
        }
    ]
