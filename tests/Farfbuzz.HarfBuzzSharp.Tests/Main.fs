module Farfbuzz.HarfBuzzSharp.Tests.Main

open Expecto

/// <summary>Runs the facade tests.</summary>
/// <param name="argv">Expecto arguments, for example <c>--filter-test-list</c> or <c>--summary</c>.</param>
/// <returns>The process exit code: zero when every test passed.</returns>
[<EntryPoint>]
let main argv =
    runTestsInAssemblyWithCLIArgs [] argv
