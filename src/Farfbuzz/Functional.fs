namespace Farfbuzz

open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.OptimizedClosures

/// <summary>Keeps the call of a replaced function from being the last call of its caller. With tail calls on, F# marks the callers of a function whose
/// body ends in a call it cannot see through, a closure of the caller's font or buffer, <c>tail.</c>; the runtime then compiles each of them without
/// profile data and never inlines it, so the lookup of a font that has no replaced function would slow down for the sake of one that has.</summary>
module internal Settled =
    /// <summary>Returns its argument.</summary>
    /// <param name="value">The answer of the replaced function.</param>
    /// <returns>The same value.</returns>
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let value (value: 'T) = value

/// <summary>
/// A fold step as an interface, implemented by structs so that a generic loop over it is specialised by the
/// JIT for each folder type and the step inlined: the zero-allocation way to fold in a hot path.
/// </summary>
type internal IFolder<'State, 'Item> =
    /// <summary>Combines the state with one item.</summary>
    /// <param name="state">The state so far.</param>
    /// <param name="item">The next item.</param>
    /// <returns>The new state.</returns>
    abstract Step: state: 'State -> item: 'Item -> 'State

/// <summary>Folds, searches and tests over spans, written as module-level self tail recursion (compiled to loops).</summary>
[<RequireQualifiedAccess>]
module internal Spans =
    /// <summary>Folds from an index to the end of a span.</summary>
    /// <param name="span">The items.</param>
    /// <param name="folder">The adapted fold step.</param>
    /// <param name="index">The next index.</param>
    /// <param name="state">The state so far.</param>
    /// <returns>The final state.</returns>
    [<TailCall>]
    let rec private foldFrom (span: ReadOnlySpan<'T>) (folder: FSharpFunc<'S, 'T, 'S>) (index) (state: 'S) : 'S =
        if index = span.Length then state else foldFrom span folder (index + 1) (folder.Invoke(state, span[index]))

    /// <summary>Folds from an index to the end of a span, passing each index to the step.</summary>
    /// <param name="span">The items.</param>
    /// <param name="folder">The adapted fold step.</param>
    /// <param name="index">The next index.</param>
    /// <param name="state">The state so far.</param>
    /// <returns>The final state.</returns>
    [<TailCall>]
    let rec private foldiFrom (span: ReadOnlySpan<'T>) (folder: FSharpFunc<'S, int, 'T, 'S>) (index) (state: 'S) : 'S =
        if index = span.Length then state else foldiFrom span folder (index + 1) (folder.Invoke(state, index, span[index]))

    /// <summary>Folds from an index to the end of a span with a struct folder, without allocation.</summary>
    /// <param name="span">The items.</param>
    /// <param name="folder">The struct folder.</param>
    /// <param name="index">The next index.</param>
    /// <param name="state">The state so far.</param>
    /// <returns>The final state.</returns>
    [<TailCall>]
    let rec private foldWithFrom<'F, 'S, 'T when 'F :> IFolder<'S, 'T> and 'F: struct>
        (span: ReadOnlySpan<'T>) (folder: 'F) (index) (state: 'S) : 'S =
        if index = span.Length then state else foldWithFrom span folder (index + 1) (folder.Step state span[index])

    /// <summary>Finds the first index at or after a position whose item satisfies a predicate.</summary>
    /// <param name="span">The items.</param>
    /// <param name="predicate">The test.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The index, or none.</returns>
    [<TailCall>]
    let rec private findFrom (span: ReadOnlySpan<'T>) (predicate: 'T -> bool) (index) : int voption =
        match index = span.Length with
        | true -> ValueNone
        | false when predicate span[index] -> ValueSome index
        | false -> findFrom span predicate (index + 1)

    /// <summary>Folds a span from the first item to the last.</summary>
    /// <param name="folder">The fold step.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="span">The items.</param>
    /// <returns>The final state.</returns>
    let fold (folder: 'S -> 'T -> 'S) (state: 'S) (span: ReadOnlySpan<'T>) : 'S =
        foldFrom span (FSharpFunc<'S, 'T, 'S>.Adapt folder) 0 state

    /// <summary>Folds a span from the first item to the last, passing each index to the step.</summary>
    /// <param name="folder">The fold step, given the state, the index and the item.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="span">The items.</param>
    /// <returns>The final state.</returns>
    let foldi (folder: 'S -> int -> 'T -> 'S) (state: 'S) (span: ReadOnlySpan<'T>) : 'S =
        foldiFrom span (FSharpFunc<'S, int, 'T, 'S>.Adapt folder) 0 state

    /// <summary>Folds a span with a struct folder: no closure, no allocation, the step inlined by the JIT.</summary>
    /// <param name="folder">The struct folder.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="span">The items.</param>
    /// <returns>The final state.</returns>
    let foldWith<'F, 'S, 'T when 'F :> IFolder<'S, 'T> and 'F: struct> (folder: 'F) (state: 'S) (span: ReadOnlySpan<'T>) : 'S =
        foldWithFrom span folder 0 state

    /// <summary>Finds the index of the first item satisfying a predicate.</summary>
    /// <param name="predicate">The test.</param>
    /// <param name="span">The items.</param>
    /// <returns>The index, or none.</returns>
    let tryFindIndex (predicate: 'T -> bool) (span: ReadOnlySpan<'T>) : int voption =
        findFrom span predicate 0

    /// <summary>Tests whether some item satisfies a predicate.</summary>
    /// <param name="predicate">The test.</param>
    /// <param name="span">The items.</param>
    /// <returns>True when at least one item does.</returns>
    let exists (predicate: 'T -> bool) (span: ReadOnlySpan<'T>) =
        ValueOption.isSome (findFrom span predicate 0)

    /// <summary>Tests whether every item satisfies a predicate.</summary>
    /// <param name="predicate">The test.</param>
    /// <param name="span">The items.</param>
    /// <returns>True when all items do, including for an empty span.</returns>
    let forall (predicate: 'T -> bool) (span: ReadOnlySpan<'T>) =
        ValueOption.isNone (findFrom span (fun item -> not (predicate item)) 0)

/// <summary>Folds over integer ranges.</summary>
[<RequireQualifiedAccess>]
module internal Ranges =
    /// <summary>Folds the half-open range from <paramref name="index"/> to <paramref name="upper"/>.</summary>
    /// <param name="upper">The exclusive upper bound.</param>
    /// <param name="folder">The adapted fold step.</param>
    /// <param name="index">The next index.</param>
    /// <param name="state">The state so far.</param>
    /// <returns>The final state.</returns>
    [<TailCall>]
    let rec private foldFrom (upper) (folder: FSharpFunc<'S, int, 'S>) (index) (state: 'S) : 'S =
        if index >= upper then state else foldFrom upper folder (index + 1) (folder.Invoke(state, index))

    /// <summary>Folds the half-open range [lower, upper).</summary>
    /// <param name="lower">The first index.</param>
    /// <param name="upper">The exclusive upper bound.</param>
    /// <param name="folder">The fold step.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state.</returns>
    let fold (lower) (upper) (folder: 'S -> int -> 'S) (state: 'S) : 'S =
        foldFrom upper (FSharpFunc<'S, int, 'S>.Adapt folder) lower state

/// <summary>A computation expression for <see cref="Result{T,TError}"/>: binds stop at the first error.</summary>
type internal ResultBuilder() =
    /// <summary>Continues with the value of a success, or stops with its error.</summary>
    /// <param name="result">The result to bind.</param>
    /// <param name="continuation">What to do with the value.</param>
    /// <returns>The continuation's result, or the error.</returns>
    member inline _.Bind(result: Result<'T, 'E>, ([<InlineIfLambda>] continuation: 'T -> Result<'U, 'E>)) : Result<'U, 'E> =
        match result with
        | Ok value -> continuation value
        | Error error -> Error error

    /// <summary>Wraps a value as a success.</summary>
    /// <param name="value">The value.</param>
    /// <returns>The success.</returns>
    member inline _.Return(value: 'T) : Result<'T, 'E> = Ok value

    /// <summary>Returns a result unchanged.</summary>
    /// <param name="result">The result.</param>
    /// <returns>The same result.</returns>
    member _.ReturnFrom(result: Result<'T, 'E>) : Result<'T, 'E> = result

/// <summary>The <c>result { }</c> expression.</summary>
[<AutoOpen>]
module internal ResultExpression =
    /// <summary>Builds a <see cref="Result{T,TError}"/> computation.</summary>
    let result = ResultBuilder()
