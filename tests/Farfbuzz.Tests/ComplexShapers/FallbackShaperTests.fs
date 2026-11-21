module Farfbuzz.Tests.ComplexShapers.FallbackShaperTests

open Expecto
open Farfbuzz.Tests.ComplexShapers.Conformance

/// <summary>The P6.S24 fallback-shaper conformance lines: the two cases of <c>simple.tests</c>, one run by the default
/// shaper and one by <c>--shaper=fallback</c>.</summary>
[<Tests>]
let tests = Conformance.files "P6.S24" [ "in-house", "simple.tests" ]
