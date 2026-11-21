module Farfbuzz.Diff.ComplexShapers.FallbackDiff

open System.Collections.Immutable
open Expecto
open Farfbuzz.Diff
open Farfbuzz.Diff.ComplexShapers
open Farfbuzz.Diff.TestFonts

/// <summary>The code points the fallback differential draws from: the Latin letters and the three default ignorables
/// the handoff names.</summary>
let private alphabet =
    ImmutableArray.CreateRange ([ 0x41u .. 0x5Au ] @ [ 0x61u .. 0x7Au ] @ [ 0x00ADu; 0x200Bu; 0x034Fu ])

/// <summary>The option fields of the fallback differential: the fallback shaper in each direction.</summary>
let private options =
    [| "--shaper=fallback --direction=ltr"; "--shaper=fallback --direction=rtl" |]

/// <summary>The P6.S24 fallback-shaping differential: Farfbuzz against HarfBuzz on random Latin text with default
/// ignorables, shaped by the fallback shaper in each direction.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P6.S24" [
        ShapeDiff.against "fallback shaper" (TestFonts.named "49c9f7485c1392fa09a1b801bc2ffea79275f22e.ttf").Path options alphabet 8 2000
    ]
