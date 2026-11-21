module Farfbuzz.Harness.TailCalls

open System.Runtime.CompilerServices

/// <summary>
/// Returns its argument. It is written after a call that passes a span, so that this call, and not that one, is
/// the last call of the function. With tail calls on, F# marks the last call of a function <c>tail.</c>, also
/// when an argument is a span, and the runtime then let the callee read where the span's array had been before a
/// collection moved it: 15 wrong results in 1,200 process starts on .NET 10.0.12 (BLOCKED.md, 2026-11-10).
/// </summary>
/// <param name="value">The result of the call that passed a span.</param>
/// <returns>The same value.</returns>
[<MethodImpl(MethodImplOptions.NoInlining)>]
let settle (value: 'T) : 'T = value
