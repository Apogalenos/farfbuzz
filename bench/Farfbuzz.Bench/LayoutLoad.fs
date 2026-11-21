module Farfbuzz.Bench.LayoutLoad

open System
open System.IO
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Bench.NativeShaping

/// <summary>The GSUB table tag.</summary>
let private gsubTag = Tag.make 'G' 'S' 'U' 'B'

/// <summary>The GPOS table tag.</summary>
let private gposTag = Tag.make 'G' 'P' 'O' 'S'

/// <summary>The three fonts whose GSUB and GPOS load is measured.</summary>
let private fontFiles =
    [ "Roboto-Regular.ttf"
      "NotoNastaliqUrdu-Regular.ttf"
      "Amiri-Regular.ttf" ]

/// <summary>Resolves one perf font under the upstream checkout.</summary>
/// <param name="file">The file name.</param>
/// <returns>The path.</returns>
let private perfPath (file: string) =
    Path.Combine(RepositoryPaths.upstream (), "perf", "fonts", file)

/// <summary>One font and one layout table to load.</summary>
type LayoutLoadCase =
    { Font: string
      Bytes: byte array
      TableTag: Tag }
    override this.ToString() = $"{Tag.toString this.TableTag}/{this.Font}"

/// <summary>The six cases, one per font and table.</summary>
let private cases : LayoutLoadCase list =
    oracle.Force() |> ignore
    [ for file in fontFiles do
        let bytes = File.ReadAllBytes (perfPath file)
        yield { Font = file; Bytes = bytes; TableTag = gsubTag }
        yield { Font = file; Bytes = bytes; TableTag = gposTag } ]

/// <summary>Loads a table through native HarfBuzz: a fresh face, then the query that forces the load.</summary>
/// <param name="case">The case.</param>
/// <returns>Whether the table is present, so the work cannot be elided.</returns>
let private nativeLoad (case: LayoutLoadCase) : int =
    let blob = Native.hb_blob_create(case.Bytes, uint32 case.Bytes.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, 0u)
    let present =
        match case.TableTag = gsubTag with
        | true -> Native.hb_ot_layout_has_substitution face
        | false -> Native.hb_ot_layout_has_positioning face
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    present

/// <summary>Loads a table through Farfbuzz: a fresh face, then the layout table value.</summary>
/// <param name="case">The case.</param>
/// <returns>The lookup count, so the work cannot be elided.</returns>
let private farfbuzzLoad (case: LayoutLoadCase) : int =
    let face = Face.create (ReadOnlyMemory case.Bytes) 0u
    let otFace = OtFace.create face
    match case.TableTag = gsubTag with
    | true -> LayoutTable.lookupCount otFace.Gsub.Value
    | false -> LayoutTable.lookupCount otFace.Gpos.Value

/// <summary>Loading GSUB and GPOS, Farfbuzz against native HarfBuzz 15.1.0.</summary>
[<MemoryDiagnoser>]
type LayoutLoad() =
    /// <summary>The cases, one per font and table.</summary>
    member _.Cases() : seq<obj> = cases |> Seq.map (fun case -> case :> obj)

    /// <summary>Loads through native HarfBuzz.</summary>
    /// <param name="case">The case.</param>
    /// <returns>Whether the table is present.</returns>
    [<Benchmark(Baseline = true); ArgumentsSource("Cases")>]
    member _.Native(case: LayoutLoadCase) : int = nativeLoad case

    /// <summary>Loads through Farfbuzz.</summary>
    /// <param name="case">The case.</param>
    /// <returns>The lookup count.</returns>
    [<Benchmark; ArgumentsSource("Cases")>]
    member _.Farfbuzz(case: LayoutLoadCase) : int = farfbuzzLoad case
