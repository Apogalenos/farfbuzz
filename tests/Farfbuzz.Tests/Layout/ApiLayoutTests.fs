module Farfbuzz.Tests.Layout.ApiLayoutTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.UpstreamFiles

/// <summary>The glyphs of a sorted range set, expanded.</summary>
/// <param name="ranges">The ranges.</param>
/// <returns>The glyphs.</returns>
let private members (ranges: ImmutableArray<CodepointRange>) : uint32 array =
    ranges |> Seq.collect (fun range -> seq { for glyph in int range.First .. int range.Last -> uint32 glyph }) |> Seq.toArray

/// <summary>Asserts a collected set's population and a few members.</summary>
/// <param name="label">The set name.</param>
/// <param name="ranges">The set.</param>
/// <param name="population">The expected population.</param>
/// <param name="checks">The (index, glyph) pairs to check.</param>
let private expectSet (label: string) (ranges: ImmutableArray<CodepointRange>) (population: int) (checks: (int * uint32) list) =
    let values = members ranges
    Expect.equal values.Length population $"{label} population"
    for (index, glyph) in checks do
        Expect.equal values[index] glyph $"{label}[{index}]"

/// <summary>Collects a lookup's four sets from a test font.</summary>
/// <param name="path">The font path.</param>
/// <param name="table">The table tag.</param>
/// <param name="lookup">The lookup index.</param>
/// <returns>The four sets.</returns>
let private collect (path: string) (table: Tag) (lookup: int) =
    let face = Face.create (read path) 0u
    LayoutApi.lookupCollectGlyphs (OtFace.create face) table lookup

/// <summary>Asserts the four sets of one lookup.</summary>
/// <param name="path">The font path.</param>
/// <param name="table">The table tag.</param>
/// <param name="lookup">The lookup index.</param>
/// <param name="before">The expected before population and checks.</param>
/// <param name="input">The expected input population and checks.</param>
/// <param name="after">The expected after population and checks.</param>
/// <param name="output">The expected output population and checks.</param>
let private check (path: string) (table: Tag) (lookup: int) before input after output =
    let struct (b, i, a, o) = collect path table lookup
    expectSet "before" b (fst before) (snd before)
    expectSet "input" i (fst input) (snd input)
    expectSet "after" a (fst after) (snd after)
    expectSet "output" o (fst output) (snd output)

/// <summary>The P5.S15 ports of <c>test/api/test-ot-collect-glyphs.c</c> and <c>test-shape.c</c>'s overlong mark cluster.</summary>
[<Tests>]
let tests =
    testList "P5.S15" [
        test "collect glyphs source sans" {
            let path = "test/api/fonts/SourceSansPro-Regular.otf"
            check path Tags.Gsub 0 (0, []) (684, [ 0, 54u ]) (0, []) (372, [ 0, 574u ]) // test-ot-collect-glyphs.c:51-62
            check path Tags.Gsub 1 (0, []) (143, [ 0, 2u ]) (0, []) (319, [ 0, 519u ]) // :65-79
            check path Tags.Gsub 7 (0, []) (10, [ 0, 92u ]) (0, []) (9, [ 0, 6u ]) // :82-96
            check path Tags.Gsub 10 (0, []) (14, [ 0, 1823u ]) (0, []) (22, [ 0, 1897u ]) // :99-113
            check path Tags.Gsub 8 (0, []) (10, [ 0, 92u ]) (2, [ 0, 1826u; 1, 1837u ]) (9, [ 0, 6u ]) // :116-135
            check path Tags.Gsub 13 (771, [ 0, 2u ]) (28, [ 0, 1823u ]) (0, []) (48, [ 0, 325u ]) // :138-155
            check path Tags.Gpos 0 (0, []) (179, [ 0, 28u ]) (0, []) (0, []) // :158-168
            check path Tags.Gpos 9 (0, []) (48, [ 0, 1823u ]) (0, []) (0, []) // :171-181
            check path Tags.Gpos 10 (0, []) (1426, [ 0, 2u ]) (0, []) (0, []) // :184-194
            let han = "test/api/fonts/SourceHanSans-Regular.41,3041,4C2E.otf"
            check han Tags.Gpos 1 (0, []) (1, [ 0, 4u ]) (0, []) (0, []) // :200-210
        }
        test "collect glyphs noto nastaliq" {
            let path = "test/api/fonts/NotoNastaliqUrdu-Regular.ttf"
            check path Tags.Gsub 9 (0, []) (3, [ 0, 228u; 1, 416u; 2, 441u ]) (0, []) (3, [ 0, 267u; 1, 268u; 2, 279u ]) // :231-253
            check path Tags.Gsub 10 (0, []) (3, [ 0, 228u; 1, 416u; 2, 441u ]) (0, []) (3, [ 0, 267u; 1, 268u; 2, 279u ]) // :256-278
            check path Tags.Gsub 16 (16, [ 0, 74u ]) (27, [ 0, 276u ]) (14, [ 0, 252u ]) (43, [ 0, 74u ]) // :281-301
            check path Tags.Gsub 39 (0, []) (246, [ 0, 252u ]) (0, []) (258, [ 0, 74u ]) // :304-318
            check path Tags.Gpos 0 (0, []) (616, [ 0, 228u ]) (0, []) (0, []) // :322-332
            check path Tags.Gpos 13 (0, []) (46, [ 0, 1004u ]) (0, []) (0, []) // :335-345
            check path Tags.Gpos 17 (0, []) (242, [ 0, 257u ]) (0, []) (0, []) // :348-358
        }
        test "collect glyphs qahiri" {
            let path = "test/api/fonts/Qahiri-Regular.ttf"
            check path Tags.Gsub 0 (0, []) (4, [ 0, 52u; 1, 60u; 2, 62u; 3, 159u ]) (0, []) (4, [ 0, 53u; 1, 61u; 2, 63u; 3, 160u ]) // :379-405
            check path Tags.Gsub 11 (1, [ 0, 39u ]) (2, [ 0, 154u; 1, 159u ]) (1, [ 0, 179u ]) (2, [ 0, 155u; 1, 162u ]) // :408-432
            check path Tags.Gsub 31 (0, []) (4, [ 0, 53u ]) (0, []) (4, [ 0, 52u; 1, 60u; 2, 62u; 3, 159u ]) // :435-455
            check path Tags.Gsub 32 (0, []) (42, [ 0, 47u ]) (46, [ 0, 61u ]) (42, [ 0, 463u ]) // :458-475
        }
        test "shape overlong mark cluster" {
            let font = Face.create (read "test/api/fonts/NotoSans-Bold.ttf") 0u |> Font.create |> Font.withScale 786432 786432
            let text = Array.concat [| [| 0x65u |]; Array.create 35000 0x0301u; [| 0x58u |] |]
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan text) 0 -1 GlyphRun.empty
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            match Shape.shape font ImmutableArray.Empty run with
            | Error error -> failtest $"expected Ok, got {error}"
            | Ok shaped ->
                Expect.equal shaped.Infos.Length 35001 "one glyph is deleted" // test-shape.c:201
                Expect.equal shaped.Positions[34999].YOffset Int32.MaxValue "the last mark's y offset saturates" // test-shape.c:202
        }
    ]
