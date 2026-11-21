module Farfbuzz.Tests.CollectTests

open System
open System.Collections.Generic
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Loads a face's cmap from a test font.</summary>
/// <param name="relative">The font path under <c>test/api/fonts</c>.</param>
/// <returns>The cmap accelerator.</returns>
let private cmapOf (relative: string) =
    let face = Face.create (UpstreamFiles.read $"test/api/fonts/{relative}") 0u
    Cmap.load face (Os2.load face)

/// <summary>Expands ranges into a simple list.</summary>
/// <param name="ranges">The ranges.</param>
/// <returns>The odd pairs.</returns>
let private rangePairs (ranges: System.Collections.Immutable.ImmutableArray<CodepointRange>) =
    ranges |> Seq.map (fun range -> range.First, range.Last) |> Seq.toList

/// <summary>Expands mapping pairs into a simple list.</summary>
/// <param name="pairs">The pairs.</param>
/// <returns>The pairs.</returns>
let private mappingPairs (pairs: System.Collections.Immutable.ImmutableArray<struct (uint32 * uint32)>) =
    pairs |> Seq.map (fun struct (key, value) -> key, value) |> Seq.toList

/// <summary>The hand-built format-4 cmap of <c>test-collect-unicodes.c</c>.</summary>
let private rangeOffsetCmap =
    [| 0uy; 0uy; 0uy; 1uy
       0uy; 0uy; 0uy; 3uy; 0uy; 0uy; 0uy; 12uy
       0uy; 4uy; 0uy; 44uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy
       0uy; 73uy; 0uy; 0uy; 0uy; 70uy; 0uy; 5uy; 0uy; 2uy
       0uy; 8uy; 0uy; 5uy; 0uy; 0uy; 0uy; 12uy |]

/// <summary>Points used by the range-set model: small values and the top of the 32-bit space.</summary>
let private rangePoints =
    [| for value in 0 .. 4096 -> uint32 value
       for value in 0xFFFFFFF0u .. 0xFFFFFFFFu -> value |]

/// <summary>Generates range operations over the model points.</summary>
let private rangeOpGen =
    gen {
        let! lo = Gen.choose (0, rangePoints.Length - 1) |> Gen.map (fun index -> rangePoints[index])
        let! hi = Gen.choose (0, rangePoints.Length - 1) |> Gen.map (fun index -> rangePoints[index])
        let! add = Gen.elements [ true; false ]
        return match add with | true -> Add (lo, hi) | false -> Delete (lo, hi)
    }

/// <summary>Generates map pairs with repeated keys.</summary>
let private mapPairGen =
    gen {
        let! key = Gen.choose (0, 100) |> Gen.map uint32
        let! value = Gen.choose (0, 1000) |> Gen.map uint32
        return struct (key, value)
    }

/// <summary>Builds a format 4 subtable.</summary>
/// <param name="segments">Start, end, delta and range offset per segment.</param>
/// <param name="glyphIds">The glyph id array.</param>
/// <returns>The bytes.</returns>
let private format4 (segments: (int * int * int * int) list) (glyphIds: uint16 list) : byte array =
    let count = segments.Length
    let header = Array.concat [ be16 4u; be16 0u; be16 0u; be16 (uint32 (2 * count)); be16 0u; be16 0u; be16 0u ]
    let ends = segments |> List.map (fun (_, last, _, _) -> be16 (uint32 last)) |> Array.concat
    let starts = segments |> List.map (fun (first, _, _, _) -> be16 (uint32 first)) |> Array.concat
    let deltas = segments |> List.map (fun (_, _, delta, _) -> be16 (uint32 (int16 delta))) |> Array.concat
    let offsets = segments |> List.map (fun (_, _, _, offset) -> be16 (uint32 offset)) |> Array.concat
    let glyphs = glyphIds |> List.map (fun glyph -> be16 (uint32 glyph)) |> Array.concat
    Array.concat [ header; ends; be16 0u; starts; deltas; offsets; glyphs ]

/// <summary>Builds a format 12 or 13 subtable.</summary>
/// <param name="format">12 or 13.</param>
/// <param name="groups">Start, end and glyph per group.</param>
/// <returns>The bytes.</returns>
let private longSegmented (format: uint16) (groups: (uint32 * uint32 * uint32) list) : byte array =
    let body =
        groups
        |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ])
        |> Array.concat
    Array.concat [ be16 (uint32 format); be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]

/// <summary>Builds a cmap table with one subtable.</summary>
/// <param name="platform">The platform id.</param>
/// <param name="encoding">The encoding id.</param>
/// <param name="subtable">The subtable bytes.</param>
/// <returns>The cmap bytes.</returns>
let private cmapWith (platform: uint16) (encoding: uint16) (subtable: byte array) : byte array =
    Array.concat
        [ be16 0u; be16 1u; be16 (uint32 platform); be16 (uint32 encoding); be32 12u; subtable ]

/// <summary>Builds a face with one cmap subtable.</summary>
/// <param name="platform">The platform id.</param>
/// <param name="encoding">The encoding id.</param>
/// <param name="subtable">The subtable bytes.</param>
/// <returns>The cmap accelerator.</returns>
let private cmapFromSubtable (platform: uint16) (encoding: uint16) (subtable: byte array) : CmapAccel =
    let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmapWith platform encoding subtable ])) 0u
    Cmap.load face (Os2.load face)

/// <summary>Collect functions and the Sealed range builders, P2.S07.</summary>
[<Tests>]
let tests =
    testList "P2.S07" [
        test "format 4 collects abc" {
            let cmap = cmapOf "Roboto-Regular.abc.format4.ttf"
            Expect.equal (rangePairs (Cmap.collectUnicodes cmap 4)) [ 0x61u, 0x63u ] "ranges"
        }
        test "format 4 range offsets collect 70, 71 and 73" {
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, rangeOffsetCmap ])) 0u
            let cmap = Cmap.load face (Os2.load face)
            Expect.equal (rangePairs (Cmap.collectUnicodes cmap 4)) [ 70u, 71u; 73u, 73u ] "ranges"
            Expect.equal (mappingPairs (Cmap.collectMapping cmap 4)) [ 70u, 13u; 71u, 10u; 73u, 17u ] "mapping"
        }
        test "format 12 skips the notdef group start" {
            let cmap = cmapOf "cmunrm.otf"
            let first =
                Cmap.collectUnicodes cmap 1000
                |> Seq.collect (fun range -> [ range.First .. range.Last ])
                |> Seq.truncate 3
                |> Seq.toList
            Expect.equal first [ 0x20u; 0x21u; 0x22u ] "first codepoints"
        }
        test "format 12 collects abc" {
            let cmap = cmapOf "Roboto-Regular.abc.format12.ttf"
            Expect.equal (rangePairs (Cmap.collectUnicodes cmap 4)) [ 0x61u, 0x63u ] "ranges"
        }
        test "collect unicodes and mapping agree" {
            let cmap = cmapOf "Roboto-Regular.abc.ttf"
            let ranges = Cmap.collectUnicodes cmap 4
            let mapping = Cmap.collectMapping cmap 4
            let expected = [ 0x61u; 0x62u; 0x63u ]
            Expect.equal (ranges |> Seq.collect (fun range -> [ range.First .. range.Last ]) |> Seq.toList) expected "unicodes"
            Expect.equal (mappingPairs mapping |> List.map fst) expected "mapping keys"
        }
        testProperty "rangeSet matches a point-by-point model" <|
            Prop.forAll (Arb.fromGen (Gen.listOf rangeOpGen)) (fun ops ->
                let mutable model = Array.create rangePoints.Length false
                for op in ops do
                    match op with
                    | Add (lo, hi) when lo <= hi && lo <> UInt32.MaxValue && hi <> UInt32.MaxValue ->
                        for index in 0 .. rangePoints.Length - 1 do
                            if lo <= rangePoints[index] && rangePoints[index] <= hi then model[index] <- true
                    | Add _ -> ()
                    | Delete (lo, hi) when lo <= hi && lo <> UInt32.MaxValue ->
                        let last = match hi with | UInt32.MaxValue -> UInt32.MaxValue - 1u | _ -> hi
                        for index in 0 .. rangePoints.Length - 1 do
                            if lo <= rangePoints[index] && rangePoints[index] <= last then model[index] <- false
                    | Delete _ -> ()
                let ranges = Sealed.rangeSet (ReadOnlySpan(List.toArray ops))
                rangePoints |> Array.mapi (fun index point ->
                    let actual = ranges |> Seq.exists (fun range -> range.First <= point && point <= range.Last)
                    actual = model[index]) |> Array.forall id)
        testProperty "sortedMap keeps the last value per key" <|
            Prop.forAll (Arb.fromGen (Gen.listOf mapPairGen)) (fun pairs ->
                let model = Dictionary<uint32, uint32>()
                pairs |> List.iter (fun struct (key, value) -> model[key] <- value)
                let actual = mappingPairs (Sealed.sortedMap (ReadOnlySpan(List.toArray pairs)))
                let expected = model |> Seq.map (fun pair -> pair.Key, pair.Value) |> Seq.sortBy fst |> Seq.toList
                actual = expected)
        test "the range builders stay within their allocation budget" {
            let ops = Array.init 100 (fun index -> if index % 2 = 0 then Add (uint32 index, uint32 (index + 10)) else Delete (uint32 index, uint32 index))
            let sorted = Array.init 100 (fun index -> struct (uint32 index, uint32 index))
            let allocated =
                Allocation.bytesAllocatedBy (fun () ->
                    Sealed.rangeSet (ReadOnlySpan ops) |> ignore
                    Sealed.sortedMap (ReadOnlySpan sorted) |> ignore)
            Expect.isLessThan allocated 1_000_000 "allocated bytes"
        }
    ]

/// <summary>The P2.R04 cmap collection cases.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R04" [
        test "mapping uses the group end before clipping for ordering" {
            let cmap = cmapFromSubtable 3us 10us (longSegmented 12us [ 0x100u, 0x1FFu, 5u; 0x150u, 0x150u, 3u ])
            let pairs = Cmap.collectMapping cmap 10 |> mappingPairs
            Expect.isFalse (pairs |> List.exists (fun (codepoint, _) -> codepoint = 0x150u)) "second group is out of order"
        }
        test "format 13 groups with glyph zero are skipped" {
            let cmap = cmapFromSubtable 3us 10us (longSegmented 13us [ 0x41u, 0x5Au, 0u ])
            Expect.equal (rangePairs (Cmap.collectUnicodes cmap 100)) [] "none"
        }
        test "reversed format 12 glyph-zero groups are skipped" {
            let cmap = cmapFromSubtable 3us 10us (longSegmented 12us [ 0x41u, 0x40u, 0u ])
            Expect.equal (rangePairs (Cmap.collectUnicodes cmap 100)) [] "none"
        }
        test "clipped long ends clamp to the Unicode maximum" {
            let cmap = cmapFromSubtable 3us 10us (longSegmented 12us [ 0x10FFF0u, 0x110010u, 1u ])
            let ranges = Cmap.collectUnicodes cmap 20 |> rangePairs
            Expect.equal ranges [ 0x10FFF0u, 0x10FFFFu ] "clamped"
        }
        test "a backwards format-4 idRangeOffset ends the segment" {
            let subtable = format4 [ 0x41, 0x42, 0, 2; 0xFFFF, 0xFFFF, 0, 0 ] [ 7us ]
            let cmap = cmapFromSubtable 3us 1us subtable
            Expect.equal (rangePairs (Cmap.collectUnicodes cmap 10)) [] "none"
        }
        test "a format-4 delta result of zero is still mapped" {
            let subtable = format4 [ 0x41, 0x41, -5, 4; 0xFFFF, 0xFFFF, 0, 0 ] [ 5us ]
            let cmap = cmapFromSubtable 3us 1us subtable
            let pairs = Cmap.collectMapping cmap 10 |> mappingPairs
            Expect.equal pairs [ 0x41u, 0u ] "zero glyph"
        }
        test "rangeSet allocation grows linearly with the input" {
            let make count =
                Array.init count (fun index -> Add (uint32 index * 2u, uint32 index * 2u))
            let small = Allocation.bytesAllocatedBy (fun () -> Sealed.rangeSet (ReadOnlySpan (make 100)) |> ignore)
            let large = Allocation.bytesAllocatedBy (fun () -> Sealed.rangeSet (ReadOnlySpan (make 200)) |> ignore)
            Expect.isLessThan large (small * 4L) "allocation grows sub-quadratically"
        }
        test "nominalGlyph does not allocate per lookup" {
            let cmap = cmapFromSubtable 3us 10us (longSegmented 12us [ 0x41u, 0x5Au, 1u ])
            let allocated =
                Allocation.bytesAllocatedBy (fun () ->
                    for _ in 1 .. 1000 do
                        Cmap.nominalGlyph cmap 0x41u |> ignore)
            Expect.isLessThan allocated 1_000 "per-lookup allocations"
        }
    ]
