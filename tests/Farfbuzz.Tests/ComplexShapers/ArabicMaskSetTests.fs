module Farfbuzz.Tests.ComplexShapers.ArabicMaskSetTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport
open Farfbuzz.Tests.Shape.SimpleRunTests

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x41524DUL

/// <summary>The cases each property draws.</summary>
[<Literal>]
let private CaseCount = 20000

/// <summary>The plan data of the properties: one bit for each action above the low bits, which hold the glyph flags.</summary>
let private data : ArabicPlan =
    { MaskArray = ImmutableArray.CreateRange [ 0x10u; 0x20u; 0x40u; 0x80u; 0x100u; 0x200u; 0x400u; 0u ]
      HasStch = true
      DoFallback = true }

/// <summary>The code points of every joining kind: dual joining, right joining, non-joining and transparent Arabic, the joiners, Syriac
/// with its Alaph and Dalath rules, Mongolian with its variation selectors, N'Ko, Adlam, Mandaic, Manichaean, Psalter Pahlavi, Hanifi
/// Rohingya and Sogdian letters, and Latin and digits that join nothing.</summary>
let private pools =
    [ (24, [ (0x628, 0x628); (0x62A, 0x62E); (0x633, 0x63A); (0x641, 0x64A) ]); (14, [ (0x627, 0x627); (0x62F, 0x632); (0x648, 0x648) ]); (10, [ (0x64B, 0x65F); (0x670, 0x670); (0x6D6, 0x6ED) ])
      (5, [ (0x200C, 0x200D) ]); (3, [ (0x640, 0x640) ]); (4, [ (0x20, 0x20); (0x41, 0x5A); (0x30, 0x39); (0x660, 0x669) ])
      (10, [ (0x710, 0x72C) ]); (5, [ (0x710, 0x710); (0x715, 0x716); (0x72A, 0x72A) ]); (4, [ (0x730, 0x74A) ]); (3, [ (0x70F, 0x70F) ])
      (6, [ (0x1820, 0x1842); (0x1843, 0x1878) ]); (6, [ (0x180A, 0x180F); (0x180B, 0x180D); (0x180F, 0x180F) ]); (4, [ (0x7CA, 0x7EA); (0x7EB, 0x7F3) ])
      (4, [ (0x1E900, 0x1E943); (0x1E944, 0x1E94A) ]); (3, [ (0x840, 0x858); (0x859, 0x85B) ]); (2, [ (0x10AC0, 0x10AE4); (0x10B80, 0x10BAF); (0x10D00, 0x10D3F); (0x10F30, 0x10F59) ]) ]

/// <summary>Generates one code point of the pools.</summary>
let private genCodepoint =
    pools
    |> List.map (fun (weight, ranges) -> weight, Gen.oneof (ranges |> List.map (fun (first, last) -> Gen.choose (first, last))))
    |> Gen.frequency
    |> Gen.map uint32

/// <summary>Generates a case: the text, the context on both sides, the script, the flags, the level, the clusters and the budgets.</summary>
let private genCase =
    gen {
        let! length = Gen.frequency [ 4, Gen.choose (0, 4); 8, Gen.choose (4, 16); 3, Gen.choose (17, 80); 1, Gen.choose (240, 300) ]
        let! text = Gen.arrayOfLength length genCodepoint
        let! beforeLength = Gen.choose (0, 5)
        let! before = Gen.arrayOfLength beforeLength genCodepoint
        let! afterLength = Gen.choose (0, 5)
        let! after = Gen.arrayOfLength afterLength genCodepoint
        let! script = Gen.elements [ Scripts.Arabic; Scripts.Syriac; Scripts.Mongolian; Scripts.Nko; Scripts.Adlam; Scripts.Mandaic; Scripts.Sogdian; Scripts.HanifiRohingya ]
        let! flags = Gen.elements [ BufferFlags.Default; BufferFlags.ProduceUnsafeToConcat; BufferFlags.ProduceSafeToInsertTatweel; BufferFlags.ProduceUnsafeToConcat ||| BufferFlags.ProduceSafeToInsertTatweel; BufferFlags.Bot ||| BufferFlags.Eot ]
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! repeated = Gen.frequency [ 3, Gen.constant false; 1, Gen.constant true ]
        let! steps = Gen.arrayOfLength length (Gen.choose (-1, 1))
        let! successful = Gen.frequency [ 12, Gen.constant true; 1, Gen.constant false ]
        let! ops = Gen.frequency [ 6, Gen.constant GlyphRun.MaxOpsDefault; 4, Gen.choose (0, 40); 2, Gen.choose (0, 400) ]
        let! mask = Gen.elements [ 0u; 0x80000000u; 0xFFFFFF00u ]
        return struct (text, before, after, script, flags, level, repeated, steps, successful, ops, mask)
    }

/// <summary>Builds the run of a case with its Unicode properties set, the context around it and the clusters, repeated or not.</summary>
/// <param name="case">The case.</param>
/// <returns>The script and the run.</returns>
let private runOfCase (case: struct (uint32 array * uint32 array * uint32 array * Script * BufferFlags * ClusterLevel * bool * int array * bool * int * uint32)) =
    let struct (text, before, after, script, flags, level, repeated, steps, successful, ops, mask) = case
    let run = runWith level mask text
    let clusters = Array.scan (fun total step -> match repeated with | true -> total + step | false -> total + 1) 1000 steps |> Array.truncate text.Length |> Array.map uint32
    let infos = Sealed.init text.Length (fun index -> { run.Infos[index] with Cluster = clusters[index] })
    struct (script, { run with Infos = infos; ContextBefore = ImmutableArray.CreateRange before; ContextAfter = ImmutableArray.CreateRange after; Flags = flags; Successful = successful; MaxOps = ops })

/// <summary>Copies the arrays of a run into a working set of their own.</summary>
/// <param name="run">The run.</param>
/// <returns>The working set.</returns>
let private copiedSet (run: GlyphRun) =
    GlyphRun.setOf { run with Infos = Sealed.ofSpan (run.Infos.AsSpan()); Props = Sealed.ofSpan (run.Props.AsSpan()) }

/// <summary>Checks one case: the working-set mask setup equals the reference in every field of the run.</summary>
/// <param name="case">The case.</param>
let private check case =
    let struct (script, run) = runOfCase case
    let expected = Arabic.setupMasksPlan data script run
    let actual = GlyphRun.runOf (Arabic.setupMasksSet data script (copiedSet run))
    expectSameRunAll "arabic mask setup" expected actual

/// <summary>The tests: the in-place Arabic mask setup equals the reference over random joining sequences.</summary>
[<Tests>]
let tests =
    testList "arabic mask setup in place" [
        test "the in-place joining pass equals the reference over random joining sequences with context, flags, budgets and every script that joins" {
            let cases = Gen.sampleWithSeed (Rnd Seed) 30 CaseCount genCase
            cases |> Seq.iter check
        }
        test "the in-place joining pass flags the same glyphs when the buffer asks for the flags and the budget is tight" {
            let cases =
                Gen.sampleWithSeed (Rnd (Seed + 1UL)) 30 CaseCount genCase
                |> Seq.map (fun (struct (text, before, after, script, _, level, repeated, steps, successful, _, mask)) ->
                    struct (text, before, after, script, BufferFlags.ProduceUnsafeToConcat ||| BufferFlags.ProduceSafeToInsertTatweel, level, repeated, steps, successful, 12, mask))
            cases |> Seq.iter check
        }
        test "the in-place joining pass allocates nothing beyond the arrays of the set it is given, where the reference allocates several arrays a glyph" {
            let runs =
                Gen.sampleWithSeed (Rnd (Seed + 2UL)) 30 800 genCase
                |> Seq.map (fun case -> runOfCase case)
                |> Seq.toArray
            let glyphs = runs |> Array.sumBy (fun (struct (_, run)) -> run.Infos.Length)
            let copying = Allocation.bytesAllocatedAtBest (fun () -> for struct (_, run) in runs do copiedSet run |> ignore)
            let applying = Allocation.bytesAllocatedAtBest (fun () -> for struct (script, run) in runs do Arabic.setupMasksSet data script (copiedSet run) |> ignore)
            let reference = Allocation.bytesAllocatedAtBest (fun () -> for struct (script, run) in runs do Arabic.setupMasksPlan data script run |> ignore)
            printfn $"arabic mask setup, {glyphs} glyphs: in place {float (applying - copying) / float glyphs:F2} B/glyph, reference {float reference / float glyphs:F1} B/glyph"
            Expect.equal applying copying "the pass allocates nothing"
        }
        test "a Mongolian free variation selector takes the action of the letter before it, as the reference does" {
            let text = [| 0x1820u; 0x180Bu; 0x1821u; 0x180Cu; 0x180Du; 0x180Fu |]
            check (struct (text, [||], [||], Scripts.Mongolian, BufferFlags.Default, ClusterLevel.MonotoneGraphemes, false, Array.zeroCreate text.Length, true, GlyphRun.MaxOpsDefault, 0u))
        }
    ]
