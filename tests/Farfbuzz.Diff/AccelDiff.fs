module Farfbuzz.Diff.AccelDiff

open System
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>The GSUB table tag.</summary>
let private gsubTag = Tag.make 'G' 'S' 'U' 'B'

/// <summary>The GPOS table tag.</summary>
let private gposTag = Tag.make 'G' 'P' 'O' 'S'

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Builds a Farfbuzz OtFace for a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The OpenType face.</returns>
let private otFaceOf (font: TestFont) (index: uint32) =
    OtFace.create (faceOf font index)

/// <summary>Whether the oracle accepts a table as present, <c>has_substitution</c> or <c>has_positioning</c>.</summary>
/// <param name="oracleFace">The oracle face.</param>
/// <param name="table">The table tag.</param>
/// <returns>True when the table has data.</returns>
let private oracleValidTable (oracleFace: nativeint) (table: Tag) : bool =
    match table = gsubTag with
    | true -> Native.hb_ot_layout_has_substitution oracleFace <> 0
    | false -> Native.hb_ot_layout_has_positioning oracleFace <> 0

/// <summary>The six digest masks of a lookup: the lookup digest then the second digest.</summary>
/// <param name="table">The table.</param>
/// <param name="lookup">The lookup index.</param>
/// <returns>The masks.</returns>
let private lookupDigestOf (table: LayoutTable<'S>) (lookup: int) : uint64 array =
    match lookup < table.Lookups.Length with
    | false -> [| 0UL; 0UL; 0UL; 0UL; 0UL; 0UL |]
    | true ->
        let accel = table.Lookups[lookup].Value
        [| accel.LookupDigest.M0; accel.LookupDigest.M1; accel.LookupDigest.M2
           accel.SecondDigest.M0; accel.SecondDigest.M1; accel.SecondDigest.M2 |]

/// <summary>The six digest masks of a Farfbuzz lookup: the lookup digest then the second digest.</summary>
/// <param name="otFace">The OpenType face.</param>
/// <param name="table">The table tag.</param>
/// <param name="lookup">The lookup index.</param>
/// <returns>The masks.</returns>
let private ourDigest (otFace: OtFace) (table: Tag) (lookup: int) : uint64 array =
    match table = gsubTag with
    | true -> lookupDigestOf otFace.Gsub.Value lookup
    | false -> lookupDigestOf otFace.Gpos.Value lookup

/// <summary>The six digest masks of an oracle lookup.</summary>
/// <param name="oracleFace">The oracle face.</param>
/// <param name="table">The table tag.</param>
/// <param name="lookup">The lookup index.</param>
/// <returns>The masks.</returns>
let private oracleDigest (oracleFace: nativeint) (table: Tag) (lookup: int) : uint64 array =
    let out = Array.zeroCreate 6
    Shims.farfbuzz_oracle_lookup_digest (oracleFace, uint32 table, uint32 lookup, out)
    out

/// <summary>The P5.S06 lookup-digest differential.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P5.S06" [
        testCase "lookup digests" (fun () ->
            let failures = ResizeArray<string>()
            let mutable compared = 0
            for struct (font, index) in OracleFaces.all () do
                let oracleFace = OracleFaces.face font index
                for table in [ gsubTag; gposTag ] do
                    match oracleValidTable oracleFace table with
                    | false -> ()
                    | true ->
                        let otFace = otFaceOf font index
                        let lookupCount = LayoutApi.lookupCount otFace table
                        for lookup in 0 .. lookupCount - 1 do
                            let ours = ourDigest otFace table lookup
                            let oracle = oracleDigest oracleFace table lookup
                            compared <- compared + 1
                            match ours = oracle with
                            | true -> ()
                            | false ->
                                let lookupType = match table = gsubTag with | true -> otFace.Gsub.Value.Lookups[lookup].Value.LookupType | false -> otFace.Gpos.Value.Lookups[lookup].Value.LookupType
                                failures.Add $"{font.Suite}/{System.IO.Path.GetFileName font.Path} face {index} {Tag.toString table} lookup {lookup} type {lookupType}: ours {ours[0]}/{ours[1]}/{ours[2]} {ours[3]}/{ours[4]}/{ours[5]}, oracle {oracle[0]}/{oracle[1]}/{oracle[2]} {oracle[3]}/{oracle[4]}/{oracle[5]}"
            printfn "P5.S06 compared %d lookup digests" compared
            Expect.isEmpty failures ("lookup digest differences:\n" + String.concat "\n" failures))
    ]
