module Farfbuzz.Smoke.Probes

open System
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
open System.Threading.Tasks
open Farfbuzz

/// <summary>The outcome of one smoke check: a description of what was observed, or why it failed.</summary>
type Check = { Name: string; Outcome: Result<string, string> }

/// <summary>Builds a check from a condition and the observation that explains it.</summary>
/// <param name="name">The check name.</param>
/// <param name="passed">Whether the check passed.</param>
/// <param name="observed">What was observed.</param>
/// <returns>The check.</returns>
let check name passed observed =
    { Name = name; Outcome = if passed then Ok observed else Error observed }

/// <summary>Counts down by self tail recursion, which the F# compiler turns into a loop.</summary>
/// <param name="remaining">Steps left.</param>
/// <param name="total">Steps taken so far.</param>
/// <returns>The number of steps taken.</returns>
[<TailCall>]
let rec countDown (remaining: int64) (total: int64) =
    if remaining = 0L then total else countDown (remaining - 1L) (total + 1L)

/// <summary>Decides parity by mutual tail recursion: stack-safe only where the runtime honours <c>tail.</c>.</summary>
/// <param name="n">The number to classify.</param>
/// <returns>Whether the number is even.</returns>
[<TailCall>]
let rec isEven (n: int64) =
    if n = 0L then true else isOdd (n - 1L)

/// <summary>The other half of <see cref="isEven"/>.</summary>
/// <param name="n">The number to classify.</param>
/// <returns>Whether the number is odd.</returns>
and [<TailCall>] isOdd (n: int64) =
    if n = 0L then false else isEven (n - 1L)

/// <summary>The depth used by the recursion probes.</summary>
[<Literal>]
let RecursionDepth = 10_000_000L

/// <summary>A pure function of an index, used to compare parallel and sequential maps.</summary>
/// <param name="index">The index.</param>
/// <returns>A value mixing the bits of the index.</returns>
let mix (index: int) =
    (uint32 index * 2654435761u) ^^^ (uint32 index >>> 7)

/// <summary>Describes the runtime the smoke runs on.</summary>
/// <returns>Framework, operating system, architecture and code-generation facts.</returns>
let runtimeDescription () =
    let dynamicCode = if Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeCompiled then "JIT" else "no JIT"
    $"{RuntimeInformation.FrameworkDescription}; {RuntimeInformation.RuntimeIdentifier}; {RuntimeInformation.ProcessArchitecture}; {dynamicCode}; browser={OperatingSystem.IsBrowser()}; processors={Environment.ProcessorCount}"

/// <summary>The checks every platform must pass.</summary>
/// <returns>The checks, already evaluated.</returns>
let gate () =
    let tag = Tag.ofString "GSUB"
    let indices = Array.init 200_000 id
    let sequential = Array.map mix indices
    let concurrent = Array.Parallel.map mix indices
    [
        check "tag round trip" (Tag.toString tag = "GSUB" && uint32 tag = 0x47535542u) (Tag.toString tag)
        check "self tail recursion, 10^7 deep" (countDown RecursionDepth 0L = RecursionDepth) "no stack overflow"
        check "parallel map equals sequential map" (concurrent = sequential) $"{indices.Length} elements"
        check "Vector128 acceleration reported" true $"IsHardwareAccelerated={Vector128.IsHardwareAccelerated}"
    ]

/// <summary>Runs the mutual-recursion probe, which may overflow the stack on runtimes that drop <c>tail.</c>.</summary>
/// <returns>The check; a stack overflow ends the process instead.</returns>
let mutualRecursionProbe () =
    check "mutual tail recursion, 10^7 deep" (isEven RecursionDepth) "no stack overflow"

/// <summary>A check that always fails, to prove that failures reach the exit code.</summary>
/// <returns>The failing check.</returns>
let plantedFailure () =
    check "planted failure" false "planted on request"
