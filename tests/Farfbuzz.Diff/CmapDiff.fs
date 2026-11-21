module Farfbuzz.Diff.CmapDiff

open System
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.OracleFaces

/// <summary>One contiguous plane or a short list of isolated code points.</summary>
[<Struct>]
type private Block =
    { First: uint32
      Count: int
      Points: uint32 array }

/// <summary>The digest of one block's nominal glyphs.</summary>
[<Struct>]
type private BlockDigest =
    { Mapped: int
      Hash: uint64
      FirstMapped: uint32 }

/// <summary>The FNV-1a 64 offset basis.</summary>
[<Literal>]
let private FnvOffset = 14695981039346656037UL

/// <summary>The FNV-1a 64 prime.</summary>
[<Literal>]
let private FnvPrime = 1099511628211UL

/// <summary>Mixes one code point and glyph into the digest.</summary>
/// <param name="hash">The hash so far.</param>
/// <param name="codepoint">The code point.</param>
/// <param name="glyph">The glyph, or 0xFFFFFFFF for a miss.</param>
/// <returns>The new hash.</returns>
let private mix (hash: uint64) (codepoint: uint32) (glyph: uint32) =
    ((hash ^^^ uint64 codepoint) * FnvPrime ^^^ uint64 glyph) * FnvPrime

/// <summary>Folds one code point into the digest.</summary>
/// <param name="cmap">The cmap accelerator.</param>
/// <param name="codepoint">The code point.</param>
/// <param name="digest">The digest so far.</param>
/// <returns>The new digest.</returns>
let private step (cmap: CmapAccel) (codepoint: uint32) (digest: BlockDigest) =
    match Cmap.nominalGlyph cmap codepoint with
    | ValueSome glyph ->
        { Mapped = digest.Mapped + 1
          Hash = mix digest.Hash codepoint glyph
          FirstMapped = match digest.FirstMapped with | 0xFFFFFFFFu -> codepoint | _ -> digest.FirstMapped }
    | ValueNone -> { digest with Hash = mix digest.Hash codepoint 0xFFFFFFFFu }

/// <summary>Walks a contiguous range.</summary>
/// <param name="cmap">The cmap accelerator.</param>
/// <param name="first">The first code point.</param>
/// <param name="count">The number of code points.</param>
/// <param name="index">The next index.</param>
/// <param name="digest">The digest so far.</param>
/// <returns>The digest.</returns>
[<TailCall>]
let rec private walk (cmap: CmapAccel) (first: uint32) (count: int) (index: int) (digest: BlockDigest) =
    match index < count with
    | false -> digest
    | true -> walk cmap first count (index + 1) (step cmap (first + uint32 index) digest)

/// <summary>Computes a Farfbuzz block digest.</summary>
/// <param name="cmap">The cmap accelerator.</param>
/// <param name="block">The block.</param>
/// <returns>The digest.</returns>
let private ours (cmap: CmapAccel) (block: Block) =
    match block.Points.Length with
    | 0 -> walk cmap block.First block.Count 0 { Mapped = 0; Hash = FnvOffset; FirstMapped = 0xFFFFFFFFu }
    | _ ->
        block.Points
        |> Array.fold (fun digest codepoint -> step cmap codepoint digest) { Mapped = 0; Hash = FnvOffset; FirstMapped = 0xFFFFFFFFu }

/// <summary>Builds a digest from an oracle glyph array.</summary>
/// <param name="first">The first code point.</param>
/// <param name="glyphs">The glyphs, 0xFFFFFFFF for misses.</param>
/// <returns>The digest.</returns>
let private digestOf (first: uint32) (glyphs: uint32 array) =
    let mutable digest = { Mapped = 0; Hash = FnvOffset; FirstMapped = 0xFFFFFFFFu }
    for index in 0 .. glyphs.Length - 1 do
        let glyph = glyphs[index]
        digest <-
            match glyph with
            | 0xFFFFFFFFu -> { digest with Hash = mix digest.Hash (first + uint32 index) glyph }
            | _ ->
                { Mapped = digest.Mapped + 1
                  Hash = mix digest.Hash (first + uint32 index) glyph
                  FirstMapped = match digest.FirstMapped with | 0xFFFFFFFFu -> first + uint32 index | _ -> digest.FirstMapped }
    digest

/// <summary>Computes an oracle block digest.</summary>
/// <param name="font">The oracle font.</param>
/// <param name="block">The block.</param>
/// <returns>The digest.</returns>
let private oracle (font: nativeint) (block: Block) =
    match block.Points.Length with
    | 0 ->
        let glyphs = Array.zeroCreate block.Count
        Shims.farfbuzz_oracle_nominal_glyph_range(font, block.First, uint32 block.Count, glyphs) |> ignore
        digestOf block.First glyphs
    | _ ->
        block.Points
        |> Array.fold (fun digest codepoint ->
            let glyphs = Array.zeroCreate 1
            Shims.farfbuzz_oracle_nominal_glyph_range(font, codepoint, 1u, glyphs) |> ignore
            match glyphs[0] with
            | 0xFFFFFFFFu -> { digest with Hash = mix digest.Hash codepoint 0xFFFFFFFFu }
            | glyph ->
                { Mapped = digest.Mapped + 1
                  Hash = mix digest.Hash codepoint glyph
                  FirstMapped = match digest.FirstMapped with | 0xFFFFFFFFu -> codepoint | _ -> digest.FirstMapped })
            { Mapped = 0; Hash = FnvOffset; FirstMapped = 0xFFFFFFFFu }

/// <summary>Finds the first code point whose nominal glyph differs.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="block">The block.</param>
/// <returns>The first differing code point and both answers, or none.</returns>
let private firstDifference (font: TestFont) (index: uint32) (block: Block) =
    let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
    let cmap = (OtFace.create face).Cmap.Value
    let oracleFont = OracleFaces.font font index
    let codepoints =
        match block.Points.Length with
        | 0 -> Seq.init block.Count (fun offset -> block.First + uint32 offset)
        | _ -> Seq.ofArray block.Points
    codepoints
    |> Seq.tryPick (fun codepoint ->
        let glyphs = Array.zeroCreate 1
        Shims.farfbuzz_oracle_nominal_glyph_range(oracleFont, codepoint, 1u, glyphs) |> ignore
        let actual =
            match Cmap.nominalGlyph cmap codepoint with
            | ValueSome glyph -> glyph
            | ValueNone -> 0xFFFFFFFFu
        match actual = glyphs[0] with
        | true -> None
        | false -> Some (codepoint, glyphs[0], actual))

/// <summary>The P2.S06 nominal glyph differential.</summary>
/// <returns>The test list.</returns>
let tests () =
    let faces = OracleFaces.all ()
    let blocks =
        [ for plane in 0 .. 16 do
              yield { First = uint32 (plane <<< 16); Count = 65536; Points = Array.empty }
          yield { First = 0x110000u; Count = 3; Points = [| 0x110000u; 0x7FFFFFFFu; 0xFFFFFFFFu |] } ]
    let inputs =
        faces
        |> Seq.collect (fun struct (font, index) ->
            blocks |> Seq.map (fun block -> struct (font, index, block)))
    testList "P2.S06" [
        testCase "P2.S06 nominal glyphs" (fun () ->
            let failures =
                inputs
                |> Seq.choose (fun struct (font, index, block) ->
                    let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
                    let expected = oracle (OracleFaces.font font index) block
                    let actual = ours (OtFace.create face).Cmap.Value block
                    match actual = expected with
                    | true -> None
                    | false -> Some (font, index, block, firstDifference font index block))
                |> Seq.truncate 20
                |> Seq.toList
            let report =
                failures
                |> List.map (fun (font, index, block, difference) ->
                    $"  {font.Path} face {index}, block {block.First}+{block.Count}: first difference {difference}")
            Expect.isEmpty failures ("first mismatches:\n" + String.concat "\n" report))
    ]
