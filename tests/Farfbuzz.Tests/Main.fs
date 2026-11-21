module Farfbuzz.Tests.Main

open System.Collections.Concurrent
open Expecto
open Farfbuzz.Harness.Ratchet

/// <summary>Runs the unit tests and the conformance ratchet, then writes the conformance reports.</summary>
/// <param name="argv">Expecto arguments, for example <c>--filter</c> or <c>--summary</c>.</param>
/// <returns>The process exit code: zero when every test passed.</returns>
[<EntryPoint>]
let main argv =
    let sink = ConcurrentQueue<FileCheck>()
    let tests = testList "Farfbuzz.Tests" [ yield! Option.toList (Impl.testFromThisAssembly ()); ConformanceTests.tests sink ]
    let code = runTestsWithCLIArgs [] argv tests
    ConformanceTests.writeReports sink
    code
