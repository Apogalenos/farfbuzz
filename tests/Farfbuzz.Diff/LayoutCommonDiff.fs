module Farfbuzz.Diff.LayoutCommonDiff

open System
open System.Collections.Concurrent
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>The GDEF table tag.</summary>
let private gdefTag = Tag.make 'G' 'D' 'E' 'F'

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Reads the glyph class definition of a face's GDEF table.</summary>
/// <param name="face">The face.</param>
/// <returns>The class definition, null when absent.</returns>
let private gdefClassDef (face: Face) : ClassDef =
    let gdef = Face.referenceTable face gdefTag
    match gdef.IsEmpty with
    | true -> ClassDefNull
    | false ->
        let span = gdef.Span
        let offset =
            match BinaryCalls.u16 span 2 >= 4us && BinaryCalls.u32 span 18 <> 0u with
            | true -> BinaryCalls.u32 span 18
            | false -> uint32 (BinaryCalls.u16 span 4)
        match offset with
        | 0u -> ClassDefNull
        | _ -> ClassDef.parse span (int offset) |> TailCalls.settle

/// <summary>The glyph class definition of each face, cached by path and index.</summary>
let private classDefs = ConcurrentDictionary<struct (string * uint32), ClassDef>()

/// <summary>Returns a face's glyph class definition, built once.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The class definition.</returns>
let private classDefOf (font: TestFont) (index: uint32) : ClassDef =
    classDefs.GetOrAdd(struct (font.Path, index), fun _ -> gdefClassDef (faceOf font index))

/// <summary>Applies an add/add_range op stream to a digest.</summary>
/// <param name="ops">The op stream: per op, kind (0 add, 1 add_range), a, b.</param>
/// <param name="index">The next stream element.</param>
/// <param name="digest">The digest so far.</param>
/// <returns>The digest of every complete op.</returns>
[<TailCall>]
let rec private applyOps (ops: uint32 array) (index: int) (digest: Digest) : Digest =
    match index + 3 <= ops.Length with
    | false -> digest
    | true ->
        let kind = ops[index]
        let a = ops[index + 1]
        let b = ops[index + 2]
        let next =
            match kind with
            | 0u -> Digest.add a digest
            | _ -> Digest.addRange a b digest
        applyOps ops (index + 3) next

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint24.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be24 (value: uint32) = [| byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>A random glyph id over the full uint32 range.</summary>
let private randomGlyph =
    gen {
        let! high = Gen.choose (0, 0xFFFF)
        let! low = Gen.choose (0, 0xFFFF)
        return (uint32 high <<< 16) ||| uint32 low
    }

/// <summary>A random 16-bit glyph id.</summary>
let private randomGlyph16 = Gen.choose (0, 0xFFFF) |> Gen.map uint32

/// <summary>A random 24-bit glyph id.</summary>
let private randomGlyph24 = Gen.choose (0, 0xFFFFFF) |> Gen.map uint32

/// <summary>A random 16-bit range record, possibly first above last and with a non-consecutive value.</summary>
let private randomRange16 =
    gen {
        let! first = Gen.choose (0, 0xFFFF)
        let! width = Gen.choose (0, 200)
        let! inverted = Gen.choose (0, 3)
        let! value = Gen.choose (0, 15)
        let last = if inverted = 0 then max 0 (first - width) else min 0xFFFF (first + width)
        return (uint32 first, uint32 last, uint32 value)
    }

/// <summary>A random 24-bit range record, possibly first above last and with a non-consecutive value.</summary>
let private randomRange24 =
    gen {
        let! first = Gen.choose (0, 0xFFFFFF)
        let! width = Gen.choose (0, 200)
        let! inverted = Gen.choose (0, 3)
        let! value = Gen.choose (0, 15)
        let last = if inverted = 0 then max 0 (first - width) else min 0xFFFFFF (first + width)
        return (uint32 first, uint32 last, uint32 value)
    }

/// <summary>A digest case: an add/add_range op stream and one query glyph near a range end.</summary>
let private digestCase =
    gen {
        let! opCount = Gen.choose (0, 16)
        let kinds = ResizeArray<uint32>()
        let aValues = ResizeArray<uint32>()
        let bValues = ResizeArray<uint32>()
        for _ in 0 .. opCount - 1 do
            let! kind = Gen.choose (0, 1)
            let! a = randomGlyph
            match kind with
            | 0 ->
                kinds.Add 0u
                aValues.Add a
                bValues.Add 0u
            | _ ->
                let! width = Gen.choose (0, 5000)
                kinds.Add 1u
                aValues.Add a
                bValues.Add (a + uint32 width)
        let ops =
            [ 0 .. opCount - 1 ]
            |> List.collect (fun i -> [ kinds[i]; aValues[i]; bValues[i] ])
            |> List.toArray
        let! query =
            gen {
                let! fromMember = Gen.choose (0, 2)
                match fromMember, opCount with
                | 0, count when count > 0 ->
                    let! pick = Gen.choose (0, count - 1)
                    let! which = Gen.choose (0, 1)
                    let! delta = Gen.choose (-200, 200)
                    let endpoint = if which = 0 then aValues[pick] else bValues[pick]
                    return (if delta >= 0 then endpoint + uint32 delta else endpoint - uint32 (-delta))
                | _ -> return! randomGlyph
            }
        return struct (ops, query)
    }

/// <summary>The boundary glyphs of a glyph array, plus neighbours.</summary>
/// <param name="glyphs">The glyphs.</param>
/// <returns>The boundaries.</returns>
let private glyphBoundaries (glyphs: uint32 list) =
    [ for g in glyphs do yield g - 1u; yield g; yield g + 1u ]

/// <summary>The boundary glyphs of a range array, plus neighbours.</summary>
/// <param name="ranges">First, last and value per range.</param>
/// <returns>The boundaries.</returns>
let private rangeBoundaries (ranges: (uint32 * uint32 * uint32) list) =
    [ for (f, l, _) in ranges do yield f - 1u; yield f; yield l; yield l + 1u ]

/// <summary>Truncates bytes to a prefix a fifth of the time, leaving a count larger than the bytes.</summary>
/// <param name="bytes">The bytes.</param>
/// <returns>The possibly truncated bytes.</returns>
let private maybeTruncate (bytes: byte array) =
    gen {
        let! cut = Gen.choose (0, 4)
        match cut with
        | 0 when bytes.Length > 2 ->
            let! at = Gen.choose (2, bytes.Length - 1)
            return bytes[0 .. at - 1]
        | _ -> return bytes
    }

/// <summary>A generated coverage table and the glyphs to sample.</summary>
let private coverageCase =
    gen {
        let! format = Gen.frequency [ 4, Gen.constant 1; 4, Gen.constant 2; 2, Gen.constant 3; 2, Gen.constant 4; 1, Gen.choose (5, 15) ]
        let! random = Gen.listOfLength 20 randomGlyph24
        match format with
        | 1 ->
            let! count = Gen.choose (1, 8)
            let! glyphs = Gen.listOfLength count randomGlyph16
            let bytes = Array.concat [ be16 1u; be16 (uint32 count); glyphs |> List.map be16 |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = glyphBoundaries glyphs @ random |> List.distinct |> List.toArray
            return struct (bytes, sample)
        | 3 ->
            let! count = Gen.choose (1, 8)
            let! glyphs = Gen.listOfLength count randomGlyph24
            let bytes = Array.concat [ be16 3u; be24 (uint32 count); glyphs |> List.map be24 |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = glyphBoundaries glyphs @ random |> List.distinct |> List.toArray
            return struct (bytes, sample)
        | 2 ->
            let! count = Gen.choose (1, 6)
            let! ranges = Gen.listOfLength count randomRange16
            let bytes = Array.concat [ be16 2u; be16 (uint32 count); ranges |> List.map (fun (f, l, v) -> Array.concat [ be16 f; be16 l; be16 v ]) |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = rangeBoundaries ranges @ random |> List.distinct |> List.toArray
            return struct (bytes, sample)
        | 4 ->
            let! count = Gen.choose (1, 6)
            let! ranges = Gen.listOfLength count randomRange24
            let bytes = Array.concat [ be16 4u; be24 (uint32 count); ranges |> List.map (fun (f, l, v) -> Array.concat [ be24 f; be24 l; be24 v ]) |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = rangeBoundaries ranges @ random |> List.distinct |> List.toArray
            return struct (bytes, sample)
        | unknown ->
            let bytes = be16 (uint32 unknown)
            let! bytes = maybeTruncate bytes
            return struct (bytes, random |> List.toArray)
    }

/// <summary>Farfbuzz's answer for a generated coverage: sanitize verdict, coverages, iteration and may-haves.</summary>
let private ourCoverage (struct (bytes: byte array, sample: uint32 array)) =
    let span = ReadOnlySpan bytes
    let sanitized = Coverage.sanitize span 0 (Sanitize.start bytes.Length) |> ValueOption.isSome
    let coverage =
        match sanitized with
        | true -> Coverage.parse span 0
        | false -> CoverageNull
    let coverages =
        sample
        |> Array.map (fun glyph -> match Coverage.index coverage glyph with | Covered index -> index | NotCovered -> -1)
    let iteration = Coverage.glyphs coverage |> Array.ofSeq
    let digest = Coverage.digest coverage
    let mayHave = sample |> Array.map (fun glyph -> Digest.mayHaveCall glyph digest)
    struct (sanitized, coverages, iteration, mayHave)

/// <summary>HarfBuzz's answer for a generated coverage, through the shim.</summary>
let private oracleCoverage (struct (bytes: byte array, sample: uint32 array)) =
    let mutable sanitized = 0u
    let coverages = Array.zeroCreate sample.Length
    let iteration = Array.zeroCreate 4096
    let mutable iterCount = 0u
    let mayHave = Array.zeroCreate sample.Length
    Shims.farfbuzz_oracle_coverage (bytes, uint32 bytes.Length, sample, uint32 sample.Length, &sanitized, coverages, iteration, uint32 iteration.Length, &iterCount, mayHave)
    struct (sanitized <> 0u, coverages, iteration[0 .. int iterCount - 1], mayHave |> Array.map (fun b -> b <> 0uy))

/// <summary>A generated class definition, its sample glyphs and the classes to collect.</summary>
let private classdefCase =
    gen {
        let! format = Gen.frequency [ 4, Gen.constant 1; 4, Gen.constant 2; 2, Gen.constant 3; 2, Gen.constant 4; 1, Gen.choose (5, 15) ]
        let! random = Gen.listOfLength 20 randomGlyph24
        match format with
        | 1 ->
            let! count = Gen.choose (1, 8)
            let! start = Gen.choose (0, 0x1000)
            let! classes = Gen.listOfLength count (Gen.choose (0, 0xFFFF) |> Gen.map uint32)
            let bytes = Array.concat [ be16 1u; be16 (uint32 start); be16 (uint32 count); classes |> List.map be16 |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = glyphBoundaries [ for i in 0 .. count - 1 -> uint32 start + uint32 i ] @ random |> List.distinct |> List.toArray
            let drawn = classes[(abs (int (List.sum classes)) % count)]
            return struct (bytes, sample, [| 0u; 1u; 2u; 3u; 4u; drawn |])
        | 3 ->
            let! count = Gen.choose (1, 8)
            let! start = Gen.choose (0, 0xFFFFF)
            let! classes = Gen.listOfLength count (Gen.choose (0, 0xFFFFFF) |> Gen.map uint32)
            let bytes = Array.concat [ be16 3u; be24 (uint32 start); be24 (uint32 count); classes |> List.map be24 |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = glyphBoundaries [ for i in 0 .. count - 1 -> uint32 start + uint32 i ] @ random |> List.distinct |> List.toArray
            let drawn = classes[(abs (int (List.sum classes)) % count)]
            return struct (bytes, sample, [| 0u; 1u; 2u; 3u; 4u; drawn |])
        | 2 ->
            let! count = Gen.choose (1, 6)
            let! ranges = Gen.listOfLength count randomRange16
            let bytes = Array.concat [ be16 2u; be16 (uint32 count); ranges |> List.map (fun (f, l, v) -> Array.concat [ be16 f; be16 l; be16 v ]) |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = rangeBoundaries ranges @ random |> List.distinct |> List.toArray
            let drawn = ranges[(abs (int (List.sum (ranges |> List.map (fun (_, _, v) -> int v)))) % count)] |> fun (_, _, v) -> v
            return struct (bytes, sample, [| 0u; 1u; 2u; 3u; 4u; drawn |])
        | 4 ->
            let! count = Gen.choose (1, 6)
            let! ranges = Gen.listOfLength count randomRange24
            let bytes = Array.concat [ be16 4u; be24 (uint32 count); ranges |> List.map (fun (f, l, v) -> Array.concat [ be24 f; be24 l; be16 v ]) |> Array.concat ]
            let! bytes = maybeTruncate bytes
            let sample = rangeBoundaries ranges @ random |> List.distinct |> List.toArray
            let drawn = ranges[(abs (int (List.sum (ranges |> List.map (fun (_, _, v) -> int v)))) % count)] |> fun (_, _, v) -> v
            return struct (bytes, sample, [| 0u; 1u; 2u; 3u; 4u; drawn |])
        | unknown ->
            let bytes = be16 (uint32 unknown)
            let! bytes = maybeTruncate bytes
            return struct (bytes, random |> List.toArray, [| 0u; 1u; 2u; 3u; 4u; 0xFFFFFFFFu |])
    }

/// <summary>Farfbuzz's answer for a generated class definition: sanitize verdict, classes and collected ranges.</summary>
let private ourClassdef (struct (bytes: byte array, sample: uint32 array, collect: uint32 array)) =
    let span = ReadOnlySpan bytes
    let sanitized = ClassDef.sanitize span 0 (Sanitize.start bytes.Length) |> ValueOption.isSome
    let classDef =
        match sanitized with
        | true -> ClassDef.parse span 0
        | false -> ClassDefNull
    let classes = sample |> Array.map (fun glyph -> ClassDef.classOf classDef glyph)
    let ranges =
        collect
        |> Array.map (fun klass -> ClassDef.glyphsInClass classDef klass |> Seq.map (fun r -> struct (r.First, r.Last)) |> Seq.toArray)
    struct (sanitized, classes, ranges)

/// <summary>HarfBuzz's answer for a generated class definition, through the shim.</summary>
let private oracleClassdef (struct (bytes: byte array, sample: uint32 array, collect: uint32 array)) =
    let mutable sanitized = 0u
    let classes = Array.zeroCreate sample.Length
    let rangeFirsts = Array.zeroCreate 1024
    let rangeLasts = Array.zeroCreate 1024
    let rangeCounts = Array.zeroCreate collect.Length
    Shims.farfbuzz_oracle_classdef (bytes, uint32 bytes.Length, sample, uint32 sample.Length, collect, uint32 collect.Length, &sanitized, classes, rangeFirsts, rangeLasts, uint32 rangeFirsts.Length, rangeCounts)
    let ranges =
        let mutable offset = 0
        [| for k in 0 .. collect.Length - 1 ->
            let count = int rangeCounts[k]
            let result = [| for j in 0 .. count - 1 -> struct (rangeFirsts[offset + j], rangeLasts[offset + j]) |]
            offset <- offset + count
            result |]
    struct (sanitized <> 0u, classes, ranges)

/// <summary>The P5.S01 layout-common differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let classInputs =
        OracleFaces.all ()
        |> Seq.collect (fun struct (font, index) ->
            let face = faceOf font index
            match (Face.referenceTable face gdefTag).IsEmpty with
            | true -> Seq.empty
            | false -> seq { for glyph in 0 .. face.GlyphCount -> struct (font, index, uint32 glyph) })
    testList "P5.S01" [
        exhaustive "glyph classes" classInputs
            (fun struct (font, index, glyph) -> int (ClassDef.classOf (classDefOf font index) glyph))
            (fun struct (font, index, glyph) -> Native.hb_ot_layout_get_glyph_class (OracleFaces.face font index, glyph))
        sampled "digest" digestCase 5000
            (fun struct (ops, query) -> Digest.mayHaveCall query (applyOps ops 0 Digest.empty))
            (fun struct (ops, query) -> Shims.farfbuzz_oracle_set_digest_may_have (ops, uint32 (ops.Length / 3), query) <> 0)
        sampled "generated coverage" coverageCase 5000
            ourCoverage
            oracleCoverage
        sampled "generated class definitions" classdefCase 5000
            ourClassdef
            oracleClassdef
    ]
