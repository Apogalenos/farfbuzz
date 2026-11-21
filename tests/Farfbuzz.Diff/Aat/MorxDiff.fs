module Farfbuzz.Diff.MorxDiff

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Harness
open Farfbuzz.Harness.ShapeRequest
open Farfbuzz.Harness.Shaper
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Builds a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) : OtFace =
    OtFace.create (Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index)

/// <summary>What the shaping comparison looks at: glyph ids, clusters, positions and the defined flags.</summary>
/// <param name="outcome">An engine's outcome.</param>
/// <returns>The comparable tuples, or the reason there are none.</returns>
let private comparable (outcome: Result<ShapeOutcome, string>) =
    match outcome with
    | Ok (Shaped run) ->
        Ok (Array.map2 (fun (info: GlyphInfo) (position: GlyphPosition) ->
                struct (info.Codepoint, info.Cluster, position.XAdvance, position.YAdvance, position.XOffset, position.YOffset, info.Mask &&& uint32 GlyphFlags.Defined)) run.Infos run.Positions)
    | Ok ShapingFailed -> Error "shaping failed"
    | Error reason -> Error reason

/// <summary>Counts noncontextual subtables in one chain, stopping at the first other kind.</summary>
/// <param name="subtables">The chain's subtables.</param>
/// <param name="index">The next subtable.</param>
/// <param name="count">The count so far.</param>
/// <returns>The count, or none when a non-noncontextual subtable appears.</returns>
[<TailCall>]
let rec private countSubtables (subtables: ImmutableArray<ChainSubtable>) (index: int) (count: int) : int voption =
    match index >= subtables.Length with
    | true -> ValueSome count
    | false ->
        match subtables[index].Kind with
        | Noncontextual -> countSubtables subtables (index + 1) (count + 1)
        | Rearrangement | Contextual | Ligature | Insertion | UnknownKind _ -> ValueNone

/// <summary>Walks every chain counting noncontextual subtables, stopping at the first other kind.</summary>
/// <param name="chains">The chains.</param>
/// <param name="chainIndex">The next chain.</param>
/// <param name="count">The noncontextual subtables counted so far.</param>
/// <returns>True when every subtable is noncontextual and at least one exists.</returns>
[<TailCall>]
let rec private onlyNoncontextual (chains: ImmutableArray<MorxChain>) (chainIndex: int) (count: int) : bool =
    match chainIndex >= chains.Length with
    | true -> count > 0
    | false ->
        match countSubtables chains[chainIndex].Subtables 0 count with
        | ValueSome next -> onlyNoncontextual chains (chainIndex + 1) next
        | ValueNone -> false

/// <summary>Whether a face's morx or mort holds only noncontextual subtables and at least one.</summary>
/// <param name="face">The face.</param>
/// <returns>True when only noncontextual subtables are present.</returns>
let private noncontextualOnly (face: Face) : bool =
    let morx = Morx.load face false
    let mort = Morx.load face true
    let table = if Morx.hasData morx then morx else mort
    match Morx.hasData table with
    | false -> false
    | true -> onlyNoncontextual table.Chains 0 0

/// <summary>Every face whose morx or mort holds only noncontextual subtables and that has no kerx or Apple kern
/// format 1, so the positions and flags the oracle produces come only from the ported machinery.</summary>
/// <returns>The faces.</returns>
let private noncontextualFaces () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        let oracleFace = OracleFaces.face font index
        noncontextualOnly face
        && Native.hb_aat_layout_has_positioning oracleFace = 0
        && Shims.farfbuzz_oracle_has_machine_kerning oracleFace = 0)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Generates an AAT shaping request over the noncontextual faces, with the unsafe-to-concat flag set so
/// the whole-run unsafe-to-concat marking is compared.</summary>
/// <param name="fonts">The noncontextual faces.</param>
/// <returns>The request.</returns>
let private aatRequest (fonts: TestFont array) : Gen<ShapeRequest> =
    gen {
        let! font = Gen.elements fonts
        let coverage = TestFonts.coverage font
        let pool = match coverage.Length with | 0 -> [| 0x20u |] | _ -> coverage
        let! length = Gen.choose (1, 16)
        let! codepoints = Gen.arrayOfLength length (Gen.elements pool)
        let! direction = Gen.elements [| ValueSome "ltr"; ValueSome "rtl" |]
        return
            { defaults font.Path codepoints with
                FaceIndex = font.Index
                Shaper = ValueSome "ot"
                FontFuncs = ValueSome "ot"
                Direction = direction
                Flags = BufferFlags.ProduceUnsafeToConcat }
    }

/// <summary>Builds a morx table with one chain and one noncontextual subtable wrapping the given lookup.</summary>
/// <param name="lookup">The substitute lookup bytes.</param>
/// <returns>The morx table bytes.</returns>
let private morxWithLookup (lookup: byte array) : byte array =
    let subtable = Array.concat [ be32 (uint32 (12 + lookup.Length)); be32 4u; be32 1u; lookup ]
    let chain = Array.concat [ be32 1u; be32 (uint32 (16 + subtable.Length)); be32 0u; be32 1u; subtable ]
    Array.concat [ be16 1u; be16 0u; be32 1u; chain ]

/// <summary>Builds a format-6 single lookup from glyph and value pairs, with the terminator.</summary>
/// <param name="entries">The glyph and value pairs.</param>
/// <returns>The lookup bytes.</returns>
let private singleLookup (entries: (uint32 * uint32) array) : byte array =
    let nUnits = uint32 entries.Length + 1u
    let header = Array.concat [ be16 6u; be16 4u; be16 nUnits; be16 0u; be16 0u; be16 0u ]
    let units = entries |> Array.collect (fun (glyph, value) -> Array.concat [ be16 glyph; be16 value ])
    let terminator = Array.concat [ be16 0xFFFFu; be16 0u ]
    Array.concat [ header; units; terminator ]

/// <summary>The fields of the synthetic noncontextual morx table that the mutation differential rewrites. The subtable
/// coverage type is left alone: rewriting it dispatches to the later slices' subtable kinds.</summary>
/// <returns>The name, offset, width and sample values of each field.</returns>
let private morxFields () : (string * int * int * uint32 array) array =
    [| "subtable length", 24, 4, [| 0u; 1u; 39u; 41u; 0x10000u; 0xFFFFFFFFu |]
       "sub feature flags", 32, 4, [| 0u; 1u; 0xFFFFFFFFu |]
       "lookup format", 36, 2, [| 0u; 1u; 5u; 7u; 0xFFFFu |]
       "lookup unit size", 38, 2, [| 0u; 3u; 5u; 0xFFFFu |]
       "lookup unit count", 40, 2, [| 0u; 1u; 3u; 0xFFFFu |]
       "lookup search range", 42, 2, [| 0u; 1u; 0xFFFFu |]
       "lookup entry selector", 44, 2, [| 0u; 1u; 0xFFFFu |]
       "lookup range shift", 46, 2, [| 0u; 1u; 0xFFFFu |] |]

/// <summary>Writes a field value into a byte array.</summary>
/// <param name="bytes">The bytes.</param>
/// <param name="offset">The field offset.</param>
/// <param name="width">The field width, 2 or 4.</param>
/// <param name="value">The value.</param>
let private writeField (bytes: byte array) (offset: int) (width: int) (value: uint32) =
    match width with
    | 4 ->
        bytes[offset] <- byte (value >>> 24)
        bytes[offset + 1] <- byte (value >>> 16)
        bytes[offset + 2] <- byte (value >>> 8)
        bytes[offset + 3] <- byte value
    | _ ->
        bytes[offset] <- byte (value >>> 8)
        bytes[offset + 1] <- byte value

/// <summary>Builds a single-table sfnt around a table.</summary>
/// <param name="tag">The table tag.</param>
/// <param name="table">The table bytes.</param>
/// <returns>The file bytes.</returns>
let private sfntOf (tag: Tag) (table: byte array) : byte array =
    Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u; be32 (uint32 tag); be32 0u; be32 28u; be32 (uint32 table.Length); table ]

/// <summary>Farfbuzz's sanitize verdict for a morx table.</summary>
/// <param name="morx">The morx table bytes.</param>
/// <returns>True when the table loads.</returns>
let private oursSanitize (morx: byte array) : bool =
    let face = Face.withGlyphCount 16 (Face.create (ReadOnlyMemory (sfntOf Tags.Morx morx)) 0u)
    Morx.hasData (Morx.load face false)

/// <summary>Whether every subtable of every chain is one of the ported substitution kinds.</summary>
/// <param name="chains">The chains.</param>
/// <returns>True when only rearrangement, contextual and noncontextual subtables appear.</returns>
let private allPorted (chains: ImmutableArray<MorxChain>) : bool =
    chains
    |> Seq.collect (fun chain -> chain.Subtables)
    |> Seq.forall (fun subtable ->
        match subtable.Kind with
        | Rearrangement | Contextual | Noncontextual | Ligature -> true
        | Insertion | UnknownKind _ -> false)

/// <summary>Whether any subtable of any chain has the wanted kind.</summary>
/// <param name="chains">The chains.</param>
/// <param name="wanted">The wanted kind.</param>
/// <returns>True when the kind appears.</returns>
let private hasKind (chains: ImmutableArray<MorxChain>) (wanted: SubtableKind) : bool =
    chains |> Seq.collect (fun chain -> chain.Subtables) |> Seq.exists (fun subtable -> subtable.Kind = wanted)

/// <summary>Every face whose morx or mort holds only the ported substitution kinds, at least one of the wanted kind, and
/// that has no kerx or Apple kern format 1, so the oracle output comes only from the ported machinery.</summary>
/// <param name="wanted">The subtable kind the face must contain.</param>
/// <returns>The faces.</returns>
let private substitutionFaces (wanted: SubtableKind) : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        let oracleFace = OracleFaces.face font index
        let morx = Morx.load face false
        let mort = Morx.load face true
        let table = if Morx.hasData morx then morx else mort
        Morx.hasData table
        && allPorted table.Chains
        && hasKind table.Chains wanted
        && Native.hb_aat_layout_has_positioning oracleFace = 0
        && Shims.farfbuzz_oracle_has_machine_kerning oracleFace = 0)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Every face whose morx or mort has data and that has no kerx, Apple kern format 1 or GPOS, so the oracle
/// output comes only from the ported substitution machinery (unknown subtable kinds are skipped, and the GPOS
/// interaction on the morx faces is not ported yet).</summary>
/// <returns>The faces.</returns>
let private allMorxFaces () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        let oracleFace = OracleFaces.face font index
        let morx = Morx.load face false
        let mort = Morx.load face true
        let table = if Morx.hasData morx then morx else mort
        Morx.hasData table
        && Native.hb_aat_layout_has_positioning oracleFace = 0
        && Native.hb_ot_layout_has_positioning oracleFace = 0
        && Shims.farfbuzz_oracle_has_machine_kerning oracleFace = 0)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Whether every subtable of every chain is one of the ported substitution kinds, insertion included.</summary>
/// <param name="chains">The chains.</param>
/// <returns>True when only the five ported kinds appear.</returns>
let private allPortedWithInsertion (chains: ImmutableArray<MorxChain>) : bool =
    chains
    |> Seq.collect (fun chain -> chain.Subtables)
    |> Seq.forall (fun subtable ->
        match subtable.Kind with
        | Rearrangement | Contextual | Noncontextual | Ligature | Insertion -> true
        | UnknownKind _ -> false)

/// <summary>Every face whose morx or mort holds only the ported substitution kinds, at least one insertion subtable, and
/// that has no kerx, Apple kern format 1 or GPOS, so the oracle output comes only from the ported machinery.</summary>
/// <returns>The faces.</returns>
let private insertionFaces () : TestFont array =
    OracleFaces.all ()
    |> Seq.filter (fun struct (font, index) ->
        let face = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index
        let oracleFace = OracleFaces.face font index
        let morx = Morx.load face false
        let mort = Morx.load face true
        let table = if Morx.hasData morx then morx else mort
        Morx.hasData table
        && allPortedWithInsertion table.Chains
        && hasKind table.Chains Insertion
        && Native.hb_aat_layout_has_positioning oracleFace = 0
        && Native.hb_ot_layout_has_positioning oracleFace = 0
        && Shims.farfbuzz_oracle_has_machine_kerning oracleFace = 0)
    |> Seq.map (fun struct (font, _) -> font)
    |> Seq.toArray

/// <summary>Builds a mort table with one chain and one contextual subtable, for the changed-bytes differential.</summary>
/// <returns>The mort table bytes.</returns>
let private mortContextualTable () : byte array =
    let classBytes = Array.concat [ be16 0u; be16 4u; [| 0uy; 1uy; 2uy; 3uy |] ]
    let entry flags data0 data1 = Array.concat [ be16 18u; be16 flags; be16 data0; be16 data1 ]
    let entries = Array.concat [ entry 0u 0u 0u; entry 0u 1u 2u; entry 0u 0u 0u; entry 0u 0u 0u ]
    let subs = Array.concat [ be16 10u; be16 20u; be16 30u ]
    let body =
        Array.concat [ be16 4u; be16 10u; be16 18u; be16 22u; be16 54u; classBytes; [| 0uy; 1uy; 2uy; 3uy |]; entries; subs ]
    let subtable = Array.concat [ be16 (uint32 (8 + body.Length)); be16 1u; be32 1u; body ]
    let chain = Array.concat [ be32 1u; be32 (uint32 (12 + subtable.Length)); be16 0u; be16 1u; subtable ]
    Array.concat [ be16 1u; be16 0u; be32 1u; chain ]

/// <summary>The fields of the synthetic mort contextual table that the mutation differential rewrites.</summary>
/// <returns>The name, offset, width and sample values of each field.</returns>
let private mortFields () : (string * int * int * uint32 array) array =
    [| "nClasses", 28, 2, [| 0u; 1u; 3u; 5u; 0xFFFFu |]
       "class table offset", 30, 2, [| 0u; 1u; 9u; 11u; 0xFFFFu |]
       "state array offset", 32, 2, [| 0u; 1u; 17u; 19u; 0xFFFFu |]
       "entry table offset", 34, 2, [| 0u; 1u; 21u; 23u; 0xFFFFu |]
       "substitution tables offset", 36, 2, [| 0u; 1u; 53u; 55u; 0xFFFFu |]
       "entry flags", 52, 2, [| 0u; 1u; 0x4000u; 0x8000u; 0xFFFFu |]
       "entry mark index", 54, 2, [| 0u; 1u; 2u; 0xFFFFu |]
       "entry current index", 56, 2, [| 0u; 1u; 2u; 0xFFFFu |] |]

/// <summary>Builds a mort table with one chain and one ligature subtable whose class 1 entry is action-initiable, so
/// its initial glyph set is exactly glyph 1.</summary>
/// <returns>The mort table bytes.</returns>
let private mortLigatureTable () : byte array =
    let classBytes = Array.concat [ be16 0u; be16 4u; [| 0uy; 1uy; 2uy; 3uy |] ]
    let entry (newState: uint32) (flags: uint32) = Array.concat [ be16 newState; be16 flags ]
    let entries = Array.concat [ entry 22u 0u; entry 22u 0x8000u; entry 22u 0u; entry 22u 0u ]
    let body =
        Array.concat
            [ be16 4u; be16 14u; be16 22u; be16 26u; be16 42u; be16 46u; be16 48u
              classBytes
              [| 0uy; 1uy; 2uy; 3uy |]
              entries
              be32 0u
              be16 0u
              be16 0u ]
    let subtable = Array.concat [ be16 (uint32 (8 + body.Length)); be16 2u; be32 1u; body ]
    let chain = Array.concat [ be32 1u; be32 (uint32 (12 + subtable.Length)); be16 0u; be16 1u; subtable ]
    Array.concat [ be16 1u; be16 0u; be32 1u; chain ]

/// <summary>The fields of the synthetic mort ligature table that the mutation differential rewrites.</summary>
/// <returns>The name, offset, width and sample values of each field.</returns>
let private mortLigatureFields () : (string * int * int * uint32 array) array =
    [| "nClasses", 28, 2, [| 0u; 1u; 3u; 5u; 0xFFFFu |]
       "class table offset", 30, 2, [| 0u; 1u; 13u; 15u; 0xFFFFu |]
       "state array offset", 32, 2, [| 0u; 1u; 21u; 23u; 0xFFFFu |]
       "entry table offset", 34, 2, [| 0u; 1u; 25u; 27u; 0xFFFFu |]
       "ligAction offset", 36, 2, [| 0u; 1u; 41u; 43u; 0xFFFFu |]
       "component offset", 38, 2, [| 0u; 1u; 45u; 47u; 0xFFFFu |]
       "ligature offset", 40, 2, [| 0u; 1u; 47u; 49u; 0xFFFFu |]
       "class table count", 44, 2, [| 0u; 1u; 3u; 5u; 0xFFFFu |]
       "entry newState", 58, 2, [| 0u; 1u; 21u; 23u; 0xFFFFu |]
       "entry flags", 60, 2, [| 0u; 1u; 0x7FFFu; 0x8001u; 0xFFFFu |]
       "ligAction word", 70, 4, [| 0u; 1u; 0x3FFFFFFFu; 0x40000000u; 0x80000000u; 0xFFFFFFFFu |]
       "component value", 74, 2, [| 0u; 1u; 0xFFFFu |]
       "ligature value", 76, 2, [| 0u; 1u; 0xFFFFu |] |]

/// <summary>Farfbuzz's sanitize verdict for a mort table.</summary>
/// <param name="mort">The mort table bytes.</param>
/// <returns>True when the table loads.</returns>
let private oursMortSanitize (mort: byte array) : bool =
    let face = Face.withGlyphCount 16 (Face.create (ReadOnlyMemory (sfntOf Tags.Mort mort)) 0u)
    Morx.hasData (Morx.load face true)

/// <summary>Builds a mort table with one chain and one insertion subtable whose class 1 entry is action-initiable, so
/// its initial glyph set is exactly glyph 1.</summary>
/// <returns>The mort table bytes.</returns>
let private mortInsertionTable () : byte array =
    let classBytes = Array.concat [ be16 0u; be16 4u; [| 0uy; 1uy; 2uy; 3uy |] ]
    let entry (newState: uint32) (flags: uint32) (data0: uint32) (data1: uint32) =
        Array.concat [ be16 newState; be16 flags; be16 data0; be16 data1 ]
    let entries =
        Array.concat
            [ entry 18u 0u 0xFFFFu 0xFFFFu
              entry 18u 0x8000u 0xFFFFu 0xFFFFu
              entry 18u 0u 0xFFFFu 0xFFFFu
              entry 18u 0u 0xFFFFu 0xFFFFu ]
    let body =
        Array.concat
            [ be16 4u; be16 10u; be16 18u; be16 22u; be16 54u
              classBytes
              [| 0uy; 1uy; 2uy; 3uy |]
              entries
              be16 10u ]
    let subtable = Array.concat [ be16 (uint32 (8 + body.Length)); be16 5u; be32 1u; body ]
    let chain = Array.concat [ be32 1u; be32 (uint32 (12 + subtable.Length)); be16 0u; be16 1u; subtable ]
    Array.concat [ be16 1u; be16 0u; be32 1u; chain ]

/// <summary>The fields of the synthetic mort insertion table that the mutation differential rewrites.</summary>
/// <returns>The name, offset, width and sample values of each field.</returns>
let private mortInsertionFields () : (string * int * int * uint32 array) array =
    [| "nClasses", 28, 2, [| 0u; 1u; 3u; 5u; 0xFFFFu |]
       "class table offset", 30, 2, [| 0u; 1u; 9u; 11u; 0xFFFFu |]
       "state array offset", 32, 2, [| 0u; 1u; 17u; 19u; 0xFFFFu |]
       "entry table offset", 34, 2, [| 0u; 1u; 21u; 23u; 0xFFFFu |]
       "insertion action offset", 36, 2, [| 0u; 1u; 53u; 55u; 0xFFFFu |]
       "class table count", 40, 2, [| 0u; 1u; 3u; 5u; 0xFFFFu |]
       "entry flags", 60, 2, [| 0u; 1u; 0x0400u; 0x0800u; 0x4000u; 0x8000u; 0xFFFFu |]
       "entry current index", 62, 2, [| 0u; 1u; 2u; 0xFFFFu |]
       "entry marked index", 64, 2, [| 0u; 1u; 2u; 0xFFFFu |] |]

/// <summary>The P8.S06 morx differentials.</summary>
/// <param name="oracle">The HarfBuzz engine.</param>
/// <param name="farfbuzz">The Farfbuzz engine.</param>
/// <returns>The test list.</returns>
let tests (oracle: Shaper) (farfbuzz: Shaper) =
    testList "P8" [
        testList "P8.S06" [
            exhaustive "P8.S06 has substitution" (OracleFaces.all ())
                (fun struct (font, index) -> AatLayout.hasSubstitution (faceOf font index))
                (fun struct (font, index) -> Native.hb_aat_layout_has_substitution (OracleFaces.face font index) <> 0)
            sampled "P8.S06 shape noncontextual"
                (aatRequest (noncontextualFaces ()))
                3000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            testCase "P8.S06 morx changed bytes" (fun () ->
                let baseMorx = morxWithLookup (singleLookup [| (1u, 101u); (2u, 102u); (3u, 103u) |])
                let fields = morxFields ()
                let failures = ResizeArray<string>()
                let mutable cases = 0
                let mutable oracleRejects = 0
                let mutable state = 12345u
                let nextRandom () =
                    state <- state * 1103515245u + 12345u
                    state
                let check (morx: byte array) (description: string) =
                    let ours = oursSanitize morx
                    let theirs = Shims.farfbuzz_oracle_morx_sanitize(morx, uint32 morx.Length, 0u, 16u) <> 0
                    match ours = theirs with
                    | true ->
                        match theirs with
                        | false -> oracleRejects <- oracleRejects + 1
                        | true -> ()
                    | false -> failures.Add $"{description}: oracle {theirs}, farfbuzz {ours}"
                    cases <- cases + 1
                let mutable attempt = 0
                while cases < 5000 && attempt < 200000 do
                    attempt <- attempt + 1
                    let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                    for value in values do
                        if cases < 5000 then
                            let candidate = Array.copy baseMorx
                            writeField candidate offset width value
                            check candidate $"{name} = {value}"
                while cases < 5000 do
                    let cutBy = int (nextRandom () % 30u) + 1
                    let candidate = Array.truncate (max 1 (baseMorx.Length - cutBy)) baseMorx
                    check candidate $"truncated tail by {cutBy}"
                printfn "P8.S06 compared %d mutated morx tables; HarfBuzz rejects %d" cases oracleRejects
                Expect.isEmpty failures ("mutated morx differences:\n" + String.concat "\n" failures))
        ]
        testList "P8.S07" [
            sampled "P8.S07 shape rearrangement"
                (aatRequest (substitutionFaces Rearrangement))
                3000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            sampled "P8.S07 shape contextual"
                (aatRequest (substitutionFaces Contextual))
                3000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            testCase "P8.S07 mort contextual changed bytes" (fun () ->
                let baseMort = mortContextualTable ()
                let fields = mortFields ()
                let failures = ResizeArray<string>()
                let mutable cases = 0
                let mutable oracleRejects = 0
                let mutable state = 67890u
                let nextRandom () =
                    state <- state * 1103515245u + 12345u
                    state
                let check (mort: byte array) (description: string) =
                    let ours = oursMortSanitize mort
                    let theirs = Shims.farfbuzz_oracle_morx_sanitize(mort, uint32 mort.Length, 1u, 16u) <> 0
                    match ours = theirs with
                    | true ->
                        match theirs with
                        | false -> oracleRejects <- oracleRejects + 1
                        | true -> ()
                    | false -> failures.Add $"{description}: oracle {theirs}, farfbuzz {ours}"
                    cases <- cases + 1
                let mutable attempt = 0
                while cases < 5000 && attempt < 200000 do
                    attempt <- attempt + 1
                    let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                    for value in values do
                        if cases < 5000 then
                            let candidate = Array.copy baseMort
                            writeField candidate offset width value
                            check candidate $"{name} = {value}"
                while cases < 5000 do
                    let cutBy = int (nextRandom () % 30u) + 1
                    let candidate = Array.truncate (max 1 (baseMort.Length - cutBy)) baseMort
                    check candidate $"truncated tail by {cutBy}"
                printfn "P8.S07 compared %d mutated mort tables; HarfBuzz rejects %d" cases oracleRejects
                Expect.isEmpty failures ("mutated mort differences:\n" + String.concat "\n" failures))
        ]
        testList "P8.S08" [
            sampled "P8.S08 shape ligature"
                (aatRequest (substitutionFaces Ligature))
                3000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            testCase "P8.S08 mort ligature changed bytes" (fun () ->
                let baseMort = mortLigatureTable ()
                let fields = mortLigatureFields ()
                let failures = ResizeArray<string>()
                let mutable cases = 0
                let mutable oracleRejects = 0
                let mutable state = 34567u
                let nextRandom () =
                    state <- state * 1103515245u + 12345u
                    state
                let check (mort: byte array) (description: string) =
                    let ours = oursMortSanitize mort
                    let theirs = Shims.farfbuzz_oracle_morx_sanitize(mort, uint32 mort.Length, 1u, 16u) <> 0
                    match ours = theirs with
                    | true ->
                        match theirs with
                        | false -> oracleRejects <- oracleRejects + 1
                        | true -> ()
                    | false -> failures.Add $"{description}: oracle {theirs}, farfbuzz {ours}"
                    cases <- cases + 1
                let mutable attempt = 0
                while cases < 5000 && attempt < 200000 do
                    attempt <- attempt + 1
                    let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                    for value in values do
                        if cases < 5000 then
                            let candidate = Array.copy baseMort
                            writeField candidate offset width value
                            check candidate $"{name} = {value}"
                while cases < 5000 do
                    let cutBy = int (nextRandom () % 30u) + 1
                    let candidate = Array.truncate (max 1 (baseMort.Length - cutBy)) baseMort
                    check candidate $"truncated tail by {cutBy}"
                printfn "P8.S08 compared %d mutated mort ligature tables; HarfBuzz rejects %d" cases oracleRejects
                Expect.isEmpty failures ("mutated mort ligature differences:\n" + String.concat "\n" failures))
        ]
        testList "P8.S09" [
            sampled "P8.S09 shape insertion"
                (aatRequest (insertionFaces ()))
                3000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            sampled "P8.S09 shape all morx"
                (aatRequest (allMorxFaces ()))
                5000
                (fun spec -> comparable (farfbuzz.Shape spec))
                (fun spec -> comparable (oracle.Shape spec))
            testCase "P8.S09 mort insertion changed bytes" (fun () ->
                let baseMort = mortInsertionTable ()
                let fields = mortInsertionFields ()
                let failures = ResizeArray<string>()
                let mutable cases = 0
                let mutable oracleRejects = 0
                let mutable state = 45678u
                let nextRandom () =
                    state <- state * 1103515245u + 12345u
                    state
                let check (mort: byte array) (description: string) =
                    let ours = oursMortSanitize mort
                    let theirs = Shims.farfbuzz_oracle_morx_sanitize(mort, uint32 mort.Length, 1u, 16u) <> 0
                    match ours = theirs with
                    | true ->
                        match theirs with
                        | false -> oracleRejects <- oracleRejects + 1
                        | true -> ()
                    | false -> failures.Add $"{description}: oracle {theirs}, farfbuzz {ours}"
                    cases <- cases + 1
                let mutable attempt = 0
                while cases < 5000 && attempt < 200000 do
                    attempt <- attempt + 1
                    let name, offset, width, values = fields[int (nextRandom () % uint32 fields.Length)]
                    for value in values do
                        if cases < 5000 then
                            let candidate = Array.copy baseMort
                            writeField candidate offset width value
                            check candidate $"{name} = {value}"
                while cases < 5000 do
                    let cutBy = int (nextRandom () % 30u) + 1
                    let candidate = Array.truncate (max 1 (baseMort.Length - cutBy)) baseMort
                    check candidate $"truncated tail by {cutBy}"
                printfn "P8.S09 compared %d mutated mort insertion tables; HarfBuzz rejects %d" cases oracleRejects
                Expect.isEmpty failures ("mutated mort insertion differences:\n" + String.concat "\n" failures))
        ]
    ]
