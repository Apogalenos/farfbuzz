module Farfbuzz.Tests.VarCoordsTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Tests.UpstreamFiles

/// <summary>The P7.S15 port of <c>test/api/test-var-coords.c</c>: the variation-coordinate getters and the axis-info
/// queries.</summary>
[<Tests>]
let tests =
    testList "P7.S15" [
        test "get var coords" {
            let face = Face.create (read "test/api/fonts/TestCFF2VF.otf") 0u
            let font = Font.create face
            let normalized = Font.setVarCoordsNormalized (ImmutableArray.CreateRange [| 100; 0 |]) font
            let designAfterNormalized = Font.varCoordsDesign normalized
            Expect.isTrue (Single.IsNaN designAfterNormalized[0]) "design coords are NaN after a normalized set" // test/api/test-var-coords.c:42
            let normalizedAfterNormalized = Font.varCoordsNormalized normalized
            Expect.equal normalizedAfterNormalized[0] 100 "the normalized coordinate round-trips" // test/api/test-var-coords.c:43
            let design = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 206.0f; 0.0f |]) font
            let normalizedAfterDesign = Font.varCoordsNormalized design
            Expect.equal normalizedAfterDesign[0] -16117 "design 206 normalizes to -16117" // test/api/test-var-coords.c:48
            let designAfterDesign = Font.varCoordsDesign design
            Expect.equal (int designAfterDesign[0]) 206 "the design coordinate round-trips" // test/api/test-var-coords.c:49
        }
        test "get var get axis infos" {
            let face = Face.create (read "test/api/fonts/Estedad-VF.ttf") 0u
            let fvar = Fvar.load face
            Expect.equal fvar.Axes.Length 2 "two variation axes" // test/api/test-var-coords.c:60
            Expect.equal fvar.Axes[0].Tag (Tag.make 'w' 'g' 'h' 't') "the first axis is wght" // test/api/test-var-coords.c:66
            Expect.equal fvar.Axes[1].Tag (Tag.make 'w' 'd' 't' 'h') "the second axis is wdth" // test/api/test-var-coords.c:70
        }
    ]
