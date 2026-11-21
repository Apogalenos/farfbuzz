module Farfbuzz.Diff.GsubDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.LayoutGens
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf font index =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Generates a single, multiple or alternate lookup and glyphs over every face.</summary>
/// <returns>The generator.</returns>
let private wouldSubstituteCase =
    gen {
        let faces = OracleFaces.all ()
        let! index = Gen.choose (0, faces.Length - 1)
        let struct (font, _) = faces[index]
        let! struct (lookup, glyphs) = lookupInputs font
        return struct (font, lookup, glyphs)
    }

/// <summary>Farfbuzz's would-substitute answer.</summary>
/// <param name="input">The font, lookup and glyphs.</param>
/// <returns>Whether the lookup would substitute.</returns>
let private ourWouldSubstitute (input: struct (TestFont * int * uint32 array)) =
    let struct (font, lookup, glyphs) = input
    let face = faceOf font (uint32 font.Index)
    LayoutApi.wouldSubstitute (OtFace.create face) lookup (ImmutableArray.CreateRange glyphs) false

/// <summary>The oracle's would-substitute answer.</summary>
/// <param name="input">The font, lookup and glyphs.</param>
/// <returns>Whether the lookup would substitute.</returns>
let private oracleWouldSubstitute (input: struct (TestFont * int * uint32 array)) =
    let struct (font, lookup, glyphs) = input
    Native.hb_ot_layout_lookup_would_substitute (OracleFaces.face font (uint32 font.Index), uint32 lookup, glyphs, uint32 glyphs.Length, 0) <> 0

/// <summary>Every single or alternate lookup's covered glyph of every GSUB face.</summary>
/// <returns>The font, index, lookup and glyph tuples.</returns>
let private alternateInputs () =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let gsub = (OtFace.create (faceOf font index)).Gsub.Value
        seq {
            for lookup in 0 .. gsub.Lookups.Length - 1 do
                for glyph in lookupCoverage gsub.Lookups[lookup].Value.Subtables 0 ImmutableArray.Empty do
                    yield struct (font, index, lookup, glyph)
        })

/// <summary>The alternates a lookup offers for a glyph, <c>hb_ot_layout_lookup_get_glyph_alternates</c>.</summary>
/// <param name="face">The face.</param>
/// <param name="lookup">The lookup index.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The total and the returned alternates.</returns>
let private oracleGlyphAlternates face lookup glyph =
    let mutable count = 100u
    let glyphs = Array.zeroCreate 100
    let total = Native.hb_ot_layout_lookup_get_glyph_alternates (face, uint32 lookup, glyph, 0u, &count, glyphs)
    struct (int total, glyphs |> Array.take (int count))

/// <summary>Whether a GSUB subtable format uses 24-bit offsets.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="format">The subtable format.</param>
/// <returns>True for a medium-type format.</returns>
let private gsubMedium lookupType format =
    match lookupType with
    | 1 -> format >= 3
    | 2 | 3 | 4 | 8 -> format >= 2
    | 5 | 6 -> format >= 4
    | _ -> false

/// <summary>Reads a big-endian field of a given byte width.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The offset.</param>
/// <param name="width">Two or three bytes.</param>
/// <returns>The value.</returns>
let private readField (bytes: byte array) offset width =
    match width with
    | 3 -> (uint32 bytes[offset] <<< 16) ||| (uint32 bytes[offset + 1] <<< 8) ||| uint32 bytes[offset + 2]
    | _ -> (uint32 bytes[offset] <<< 8) ||| uint32 bytes[offset + 1]

/// <summary>Writes a big-endian field of a given byte width.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The offset.</param>
/// <param name="width">Two or three bytes.</param>
/// <param name="value">The value.</param>
let private writeField (bytes: byte array) offset width value =
    match width with
    | 3 ->
        bytes[offset] <- byte (value >>> 16)
        bytes[offset + 1] <- byte (value >>> 8)
        bytes[offset + 2] <- byte value
    | _ ->
        bytes[offset] <- byte (value >>> 8)
        bytes[offset + 1] <- byte value

/// <summary>Collects the single, multiple and alternate subtable locations of a lookup.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <param name="lk">The lookup offset.</param>
/// <param name="lookupIndex">The lookup index.</param>
/// <param name="lookupType">The lookup type.</param>
/// <param name="count">The subtable count.</param>
/// <param name="index">The next subtable.</param>
/// <param name="acc">The locations so far.</param>
/// <returns>The locations.</returns>
let rec private collectLocs gsub lk lookupIndex lookupType count index acc =
    match index < count with
    | false -> acc
    | true ->
        let subOff = int (BinaryCalls.u16 gsub (lk + 6 + index * 2))
        let at = lk + subOff
        let newAcc =
            match subOff with
            | 0 -> acc
            | _ ->
                match lookupType with
                | 7 ->
                    match int (BinaryCalls.u16 gsub (at + 2)), BinaryCalls.u32 gsub (at + 4) with
                    | 7, _ | _, 0u -> acc
                    | innerType, innerOff ->
                        let innerAt = at + int innerOff
                        let format = int (BinaryCalls.u16 gsub innerAt)
                        let w = if gsubMedium innerType format then 3 else 2
                        match innerType, innerAt + 2 + 2 * w <= gsub.Length with
                        | 1, true | 2, true | 3, true -> struct (lookupIndex, innerAt, innerType, w) :: acc
                        | _ -> acc
                | 1 | 2 | 3 ->
                    let format = int (BinaryCalls.u16 gsub at)
                    let w = if gsubMedium lookupType format then 3 else 2
                    match at + 2 + 2 * w <= gsub.Length with
                    | false -> acc
                    | true -> struct (lookupIndex, at, lookupType, w) :: acc
                | _ -> acc
        collectLocs gsub lk lookupIndex lookupType count (index + 1) newAcc

/// <summary>Collects the single, multiple and alternate subtable locations of a GSUB table.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <param name="ll">The lookup-list offset.</param>
/// <param name="wide">Whether the lookup offsets are 24-bit.</param>
/// <param name="lookupIndex">The next lookup.</param>
/// <param name="lookupCount">The lookup count.</param>
/// <param name="acc">The locations so far.</param>
/// <returns>The locations.</returns>
let rec private collectLookups gsub ll wide lookupIndex lookupCount acc =
    match lookupIndex < lookupCount with
    | false -> acc
    | true ->
        let w = if wide then 3 else 2
        let off = Coverage.readWide gsub (ll + 2 + lookupIndex * w) w
        let newAcc =
            match off with
            | 0u -> acc
            | _ ->
                let lk = ll + int off
                let lookupType = int (BinaryCalls.u16 gsub lk)
                let count = int (BinaryCalls.u16 gsub (lk + 4))
                collectLocs gsub lk lookupIndex lookupType count 0 acc
        collectLookups gsub ll wide (lookupIndex + 1) lookupCount newAcc

/// <summary>All single, multiple and alternate subtable locations of a GSUB table.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <returns>The locations.</returns>
let private gsubLocs (gsub: ReadOnlySpan<byte>) =
    let version = (uint32 (BinaryCalls.u16 gsub 0) <<< 16) ||| uint32 (BinaryCalls.u16 gsub 2)
    let is12 = version >= 0x00010002u
    let lookupListOffset =
        match is12 && BinaryCalls.u32 gsub 22 <> 0u with
        | true -> BinaryCalls.u32 gsub 22
        | false -> uint32 (BinaryCalls.u16 gsub 8)
    match lookupListOffset with
    | 0u -> []
    | _ ->
        let ll = int lookupListOffset
        let wide = is12 && BinaryCalls.u32 gsub 22 <> 0u
        collectLookups gsub ll wide 0 (int (BinaryCalls.u16 gsub ll)) []

/// <summary>The first glyph a subtable's coverage holds, none for an empty coverage.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <param name="subOff">The subtable offset.</param>
/// <param name="w">The offset width.</param>
/// <returns>The first covered glyph, or none.</returns>
let private firstCovered (gsub: ReadOnlySpan<byte>) subOff w =
    match Coverage.readWide gsub (subOff + 2) w with
    | 0u -> ValueNone
    | covOff ->
        let glyphs = Coverage.glyphs (Coverage.parse gsub (subOff + int covOff))
        match glyphs.Length with
        | 0 -> ValueNone
        | _ -> ValueSome glyphs[0]

/// <summary>Compares Farfbuzz and HarfBuzz on mutated file bytes, returning the first mismatch and the oracle verdict.</summary>
/// <param name="bytes">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="lookup">The lookup index.</param>
/// <param name="glyph">The covered glyph.</param>
/// <returns>The mismatch description, or none, and whether HarfBuzz rejects the table.</returns>
let private compareMutated bytes index lookup glyph =
    let blob = Native.hb_blob_create (bytes, uint32 bytes.Length, 0, 0n, 0n)
    let oracleFace = Native.hb_face_create (blob, index)
    let oracleAccepted = Native.hb_ot_layout_has_substitution oracleFace <> 0
    let face = Face.create (ReadOnlyMemory bytes) index
    let ourAccepted = (OtFace.create face).Gsub.Value.Version <> 0u
    let result =
        match ourAccepted = oracleAccepted with
        | false -> ValueSome $"accepted differs (ours {ourAccepted}, oracle {oracleAccepted})"
        | true ->
            match oracleAccepted with
            | false -> ValueNone
            | true ->
                let ours = LayoutApi.wouldSubstitute (OtFace.create face) lookup (ImmutableArray.CreateRange [| glyph |]) false
                let oracle = Native.hb_ot_layout_lookup_would_substitute (oracleFace, uint32 lookup, [| glyph |], 1u, 0) <> 0
                match ours = oracle with
                | true -> ValueNone
                | false -> ValueSome $"would substitute of glyph {glyph} differs (ours {ours}, oracle {oracle})"
    Native.hb_face_destroy oracleFace
    Native.hb_blob_destroy blob
    struct (result, oracleAccepted)

/// <summary>The P5.S07 mutated-subtable differential: one field rewritten at a time, compared after upstream's sanitize.</summary>
/// <returns>The test list.</returns>
let private mutatedSubtables () =
    testCase "mutated subtables" (fun () ->
        let failures = ResizeArray<string>()
        let mutable compared = 0
        let mutable rejected = 0
        for struct (font, index) in OracleFaces.all () do
            let face = faceOf font index
            match Face.tryFindTable face Tags.Gsub with
            | ValueNone -> ()
            | ValueSome record ->
                let bytes = OracleFaces.data font.Path
                let baseOffset = int (face.BaseOffset + record.Offset)
                let length = int record.Length
                match length > 0 with
                | false -> ()
                | true ->
                    let gsub = ReadOnlySpan<byte>(bytes, baseOffset, length)
                    for struct (lookup, subOff, lookupType, w) in gsubLocs gsub do
                        let glyph = match firstCovered gsub subOff w with | ValueSome g -> g | ValueNone -> 0u
                        let fileSub = baseOffset + subOff
                        let coverageOff = fileSub + 2
                        let countOff = fileSub + 2 + w
                        let maxValue = if w = 3 then 0xFFFFFFu else 0xFFFFu
                        let format = readField bytes fileSub 2
                        let coverage = readField bytes coverageOff w
                        let count = readField bytes countOff w
                        let mutations =
                            [ fileSub, 2, 0u; fileSub, 2, 1u; fileSub, 2, format - 1u; fileSub, 2, format + 1u; fileSub, 2, maxValue
                              coverageOff, w, 0u; coverageOff, w, 1u; coverageOff, w, coverage - 1u; coverageOff, w, coverage + 1u; coverageOff, w, maxValue; coverageOff, w, uint32 length
                              countOff, w, 0u; countOff, w, 1u; countOff, w, count - 1u; countOff, w, count + 1u; countOff, w, maxValue ]
                        for offset, width, value in mutations do
                            let candidate = Array.copy bytes
                            writeField candidate offset width value
                            let struct (mismatch, oracleAccepted) = compareMutated candidate index lookup glyph
                            match mismatch with
                            | ValueNone -> ()
                            | ValueSome description -> failures.Add $"{font.Suite}/{System.IO.Path.GetFileName font.Path} face {index} lookup {lookup} type {lookupType} field {offset - baseOffset} ({width}-byte, {readField bytes offset width} -> {value}): {description}"
                            compared <- compared + 1
                            match oracleAccepted with
                            | false -> rejected <- rejected + 1
                            | true -> ()
        printfn "P5.S07 compared %d mutated subtables; HarfBuzz rejects %d" compared rejected
        Expect.isEmpty failures ("mutated subtable differences:\n" + String.concat "\n" failures))

/// <summary>Every ligature sequence a GSUB face's ligature lookups match.</summary>
/// <returns>The font, lookup and glyph-sequence tuples.</returns>
let private ligatureInputs () =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let gsub = (OtFace.create (faceOf font index)).Gsub.Value
        seq {
            for lookup in 0 .. gsub.Lookups.Length - 1 do
                if gsub.Lookups[lookup].Value.LookupType = 4 then
                    for subtable in gsub.Lookups[lookup].Value.Subtables do
                        match subtable.Subtable with
                        | LigatureSubst (coverage, sets, _) ->
                            for glyph in Coverage.glyphs coverage do
                                match Coverage.index coverage glyph with
                                | CoverageHit.Covered ci when ci < sets.Length ->
                                    for lig in sets[ci] do
                                        match GsubTables.ligCount lig with
                                        | 0 -> ()
                                        | _ -> yield struct (font, lookup, Array.append [| glyph |] (lig.Components.AsSpan().ToArray()))
                                | _ -> ()
                        | _ -> ()
        })
    |> Seq.toArray

/// <summary>The ligature sequences, collected once at module load.</summary>
let private ligatureCases = ligatureInputs ()

/// <summary>Generates a ligature lookup and the glyph sequence one of its ligatures matches.</summary>
/// <returns>The generator.</returns>
let private wouldSubstituteLigatureCase =
    gen {
        match ligatureCases.Length with
        | 0 -> return struct (Unchecked.defaultof<TestFont>, -1, [||])
        | _ -> return! Gen.elements ligatureCases
    }

/// <summary>The Farfbuzz would-substitute answer for a ligature sequence.</summary>
/// <param name="input">The font, lookup and glyphs.</param>
/// <returns>Whether the lookup would substitute.</returns>
let private ourWouldSubstituteLigature (input: struct (TestFont * int * uint32 array)) =
    let struct (font, lookup, glyphs) = input
    let face = faceOf font (uint32 font.Index)
    LayoutApi.wouldSubstitute (OtFace.create face) lookup (ImmutableArray.CreateRange glyphs) false

/// <summary>The oracle's would-substitute answer for a ligature sequence.</summary>
/// <param name="input">The font, lookup and glyphs.</param>
/// <returns>Whether the lookup would substitute.</returns>
let private oracleWouldSubstituteLigature (input: struct (TestFont * int * uint32 array)) =
    let struct (font, lookup, glyphs) = input
    Native.hb_ot_layout_lookup_would_substitute (OracleFaces.face font (uint32 font.Index), uint32 lookup, glyphs, uint32 glyphs.Length, 0) <> 0

/// <summary>Compares Farfbuzz and HarfBuzz on mutated file bytes with a ligature glyph sequence.</summary>
/// <param name="bytes">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="lookup">The lookup index.</param>
/// <param name="glyphs">The representative glyph sequence.</param>
/// <returns>The mismatch description, or none, and whether HarfBuzz rejects the table.</returns>
let private compareMutatedLigature bytes index lookup (glyphs: uint32 array) =
    let blob = Native.hb_blob_create (bytes, uint32 bytes.Length, 0, 0n, 0n)
    let oracleFace = Native.hb_face_create (blob, index)
    let oracleAccepted = Native.hb_ot_layout_has_substitution oracleFace <> 0
    let face = Face.create (ReadOnlyMemory bytes) index
    let ourAccepted = (OtFace.create face).Gsub.Value.Version <> 0u
    let result =
        match ourAccepted = oracleAccepted with
        | false -> ValueSome $"accepted differs (ours {ourAccepted}, oracle {oracleAccepted})"
        | true ->
            match oracleAccepted with
            | false -> ValueNone
            | true ->
                let ours = LayoutApi.wouldSubstitute (OtFace.create face) lookup (ImmutableArray.CreateRange glyphs) false
                let oracle = Native.hb_ot_layout_lookup_would_substitute (oracleFace, uint32 lookup, glyphs, uint32 glyphs.Length, 0) <> 0
                match ours = oracle with
                | true -> ValueNone
                | false -> ValueSome $"would substitute differs (ours {ours}, oracle {oracle})"
    Native.hb_face_destroy oracleFace
    Native.hb_blob_destroy blob
    struct (result, oracleAccepted)

/// <summary>Collects the ligature subtable locations of a GSUB table.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <param name="lk">The lookup offset.</param>
/// <param name="lookupIndex">The lookup index.</param>
/// <param name="lookupType">The lookup type.</param>
/// <param name="count">The subtable count.</param>
/// <param name="index">The next subtable.</param>
/// <param name="acc">The locations so far.</param>
/// <returns>The locations.</returns>
let rec private collectLigatureLocs gsub lk lookupIndex lookupType count index acc =
    match index < count with
    | false -> acc
    | true ->
        let subOff = int (BinaryCalls.u16 gsub (lk + 6 + index * 2))
        let at = lk + subOff
        let newAcc =
            match subOff with
            | 0 -> acc
            | _ ->
                match lookupType with
                | 7 ->
                    match int (BinaryCalls.u16 gsub (at + 2)), BinaryCalls.u32 gsub (at + 4) with
                    | 7, _ | _, 0u -> acc
                    | innerType, innerOff ->
                        let innerAt = at + int innerOff
                        let format = int (BinaryCalls.u16 gsub innerAt)
                        let w = if gsubMedium innerType format then 3 else 2
                        match innerType, innerAt + 2 + 2 * w <= gsub.Length with
                        | 4, true -> struct (lookupIndex, innerAt, innerType, w) :: acc
                        | _ -> acc
                | 4 ->
                    let format = int (BinaryCalls.u16 gsub at)
                    let w = if gsubMedium lookupType format then 3 else 2
                    match at + 2 + 2 * w <= gsub.Length with
                    | false -> acc
                    | true -> struct (lookupIndex, at, lookupType, w) :: acc
                | _ -> acc
        collectLigatureLocs gsub lk lookupIndex lookupType count (index + 1) newAcc

/// <summary>Collects the ligature subtable locations of a GSUB table.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <param name="ll">The lookup-list offset.</param>
/// <param name="wide">Whether the lookup offsets are 24-bit.</param>
/// <param name="lookupIndex">The next lookup.</param>
/// <param name="lookupCount">The lookup count.</param>
/// <param name="acc">The locations so far.</param>
/// <returns>The locations.</returns>
let rec private collectLigatureLookups gsub ll wide lookupIndex lookupCount acc =
    match lookupIndex < lookupCount with
    | false -> acc
    | true ->
        let w = if wide then 3 else 2
        let off = Coverage.readWide gsub (ll + 2 + lookupIndex * w) w
        let newAcc =
            match off with
            | 0u -> acc
            | _ ->
                let lk = ll + int off
                let lookupType = int (BinaryCalls.u16 gsub lk)
                let count = int (BinaryCalls.u16 gsub (lk + 4))
                collectLigatureLocs gsub lk lookupIndex lookupType count 0 acc
        collectLigatureLookups gsub ll wide (lookupIndex + 1) lookupCount newAcc

/// <summary>All ligature subtable locations of a GSUB table.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <returns>The locations.</returns>
let private gsubLigatureLocs (gsub: ReadOnlySpan<byte>) =
    let version = (uint32 (BinaryCalls.u16 gsub 0) <<< 16) ||| uint32 (BinaryCalls.u16 gsub 2)
    let is12 = version >= 0x00010002u
    let lookupListOffset =
        match is12 && BinaryCalls.u32 gsub 22 <> 0u with
        | true -> BinaryCalls.u32 gsub 22
        | false -> uint32 (BinaryCalls.u16 gsub 8)
    match lookupListOffset with
    | 0u -> []
    | _ ->
        let ll = int lookupListOffset
        let wide = is12 && BinaryCalls.u32 gsub 22 <> 0u
        collectLigatureLookups gsub ll wide 0 (int (BinaryCalls.u16 gsub ll)) []

/// <summary>The first ligature's glyph sequence of a ligature subtable, none when it has no components.</summary>
/// <param name="gsub">The GSUB bytes.</param>
/// <param name="subOff">The subtable offset.</param>
/// <param name="w">The offset width.</param>
/// <returns>The sequence, or none.</returns>
let private firstLigatureSequence (gsub: ReadOnlySpan<byte>) subOff w =
    let covW = if w = 3 then 4 else 2
    match Coverage.readWide gsub (subOff + 2) covW with
    | 0u -> ValueNone
    | covOff ->
        let covered = Coverage.glyphs (Coverage.parse gsub (subOff + int covOff))
        match covered.Length with
        | 0 -> ValueNone
        | _ ->
            let first = covered[0]
            let setCount = int (Coverage.readWide gsub (subOff + 2 + covW) w)
            match setCount with
            | 0 -> ValueNone
            | _ ->
                match Coverage.readWide gsub (subOff + 2 + covW + w) w with
                | 0u -> ValueNone
                | setOff ->
                    let set = subOff + int setOff
                    match int (BinaryCalls.u16 gsub set) with
                    | 0 -> ValueNone
                    | _ ->
                        match Coverage.readWide gsub (set + 2) w with
                        | 0u -> ValueNone
                        | ligOff ->
                            let lig = set + int ligOff
                            let compCount = int (BinaryCalls.u16 gsub (lig + w))
                            let components = Array.zeroCreate (max 0 (compCount - 1))
                            let mutable k = 0
                            while k < components.Length do
                                components[k] <- Coverage.readWide gsub (lig + w + 2 + k * w) w
                                k <- k + 1
                            ValueSome (Array.append [| first |] components)

/// <summary>The P5.S08 mutated-ligature differential: one field rewritten at a time, compared after upstream's sanitize.</summary>
/// <returns>The test list.</returns>
let private mutatedLigatureSubtables () =
    testCase "mutated ligature subtables" (fun () ->
        let failures = ResizeArray<string>()
        let mutable compared = 0
        let mutable rejected = 0
        for struct (font, index) in OracleFaces.all () do
            let face = faceOf font index
            match Face.tryFindTable face Tags.Gsub with
            | ValueNone -> ()
            | ValueSome record ->
                let bytes = OracleFaces.data font.Path
                let baseOffset = int (face.BaseOffset + record.Offset)
                let length = int record.Length
                match length > 0 with
                | false -> ()
                | true ->
                    let gsub = ReadOnlySpan<byte>(bytes, baseOffset, length)
                    for struct (lookup, subOff, lookupType, w) in gsubLigatureLocs gsub do
                        let glyphs = match firstLigatureSequence gsub subOff w with | ValueSome g -> g | ValueNone -> [| 0u |]
                        let covW = if w = 3 then 4 else 2
                        let fileSub = baseOffset + subOff
                        let coverageOff = fileSub + 2
                        let setCountOff = fileSub + 2 + covW
                        let setOffOff = fileSub + 2 + covW + w
                        let maxValue = if w = 3 then 0xFFFFFFu else 0xFFFFu
                        let format = readField bytes fileSub 2
                        let coverage = readField bytes coverageOff covW
                        let setCount = readField bytes setCountOff w
                        let setOff = readField bytes setOffOff w
                        let setBase = fileSub + int setOff
                        let ligatureMutations =
                            match setOff = 0u || setBase + 2 + w > bytes.Length with
                            | true -> []
                            | false ->
                                let ligOff = readField bytes (setBase + 2) w
                                let ligBase = setBase + int ligOff
                                match ligOff = 0u || ligBase + w + 2 > bytes.Length with
                                | true -> []
                                | false ->
                                    let ligGlyph = readField bytes ligBase w
                                    let compCount = readField bytes (ligBase + w) 2
                                    [ ligBase, w, 0u; ligBase, w, 1u; ligBase, w, ligGlyph - 1u; ligBase, w, ligGlyph + 1u; ligBase, w, maxValue
                                      ligBase + w, 2, 0u; ligBase + w, 2, 1u; ligBase + w, 2, compCount - 1u; ligBase + w, 2, compCount + 1u; ligBase + w, 2, 0xFFFFu ]
                        let mutations =
                            [ fileSub, 2, 0u; fileSub, 2, 1u; fileSub, 2, format - 1u; fileSub, 2, format + 1u; fileSub, 2, maxValue
                              coverageOff, covW, 0u; coverageOff, covW, 1u; coverageOff, covW, coverage - 1u; coverageOff, covW, coverage + 1u; coverageOff, covW, maxValue; coverageOff, covW, uint32 length
                              setCountOff, w, 0u; setCountOff, w, 1u; setCountOff, w, setCount - 1u; setCountOff, w, setCount + 1u; setCountOff, w, maxValue
                              setOffOff, w, 0u; setOffOff, w, 1u; setOffOff, w, setOff - 1u; setOffOff, w, setOff + 1u; setOffOff, w, maxValue; setOffOff, w, uint32 length ] @ ligatureMutations
                        for offset, width, value in mutations do
                            let candidate = Array.copy bytes
                            writeField candidate offset width value
                            let struct (mismatch, oracleAccepted) = compareMutatedLigature candidate index lookup glyphs
                            match mismatch with
                            | ValueNone -> ()
                            | ValueSome description -> failures.Add $"{font.Suite}/{System.IO.Path.GetFileName font.Path} face {index} lookup {lookup} type {lookupType} field {offset - baseOffset} ({width}-byte, {readField bytes offset width} -> {value}): {description}"
                            compared <- compared + 1
                            match oracleAccepted with
                            | false -> rejected <- rejected + 1
                            | true -> ()
        printfn "P5.S08 compared %d mutated ligature subtables; HarfBuzz rejects %d" compared rejected
        Expect.isGreaterThan compared 4999 "the differential covers at least 5,000 mutations"
        Expect.isEmpty failures ("mutated ligature subtable differences:\n" + String.concat "\n" failures))

/// <summary>The P5.S07 and P5.S08 GSUB differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "GSUB" [
        testList "P5.S07" [
            sampled "would substitute (types 1-3)" wouldSubstituteCase 20_000 ourWouldSubstitute oracleWouldSubstitute
            exhaustive "glyph alternates" (alternateInputs ())
                (fun struct (font, index, lookup, glyph) ->
                    let struct (total, alternates) = LayoutApi.glyphAlternates (OtFace.create (faceOf font index)) lookup glyph 0 100
                    struct (total, alternates.AsSpan().ToArray()))
                (fun struct (font, index, lookup, glyph) -> oracleGlyphAlternates (OracleFaces.face font index) lookup glyph)
            mutatedSubtables ()
        ]
        testList "P5.S08" [
            sampled "would substitute (type 4)" wouldSubstituteLigatureCase 20_000 ourWouldSubstituteLigature oracleWouldSubstituteLigature
            mutatedLigatureSubtables ()
        ]
    ]
