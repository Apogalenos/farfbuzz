module Farfbuzz.Tests.CmapLowTableTests

open System
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness

/// <summary>The font files of the upstream checkout the property draws from.</summary>
let private fontFiles =
    lazy (
        let upstream = RepositoryPaths.upstream ()
        let directories =
            Directory.EnumerateDirectories(Path.Combine(upstream, "test", "shape", "data"))
            |> Seq.map (fun suite -> Path.Combine(suite, "fonts"))
            |> Seq.append [ Path.Combine(upstream, "perf", "fonts"); Path.Combine(upstream, "test", "subset", "data", "fonts"); Path.Combine(upstream, "test", "api", "fonts") ]
        [| for directory in directories do
            match Directory.Exists directory with
            | false -> ()
            | true ->
                for path in Directory.EnumerateFiles directory |> Seq.sort do
                    match Path.GetExtension(path) with
                    | ".ttf" | ".otf" -> yield path
                    | _ -> () |])

/// <summary>Whether the lookup of code points above the low table, through the index, equals the lookup in the subtable: the
/// edges of every range and a spread of other code points, up to the largest 32-bit one.</summary>
/// <param name="cmap">The accelerator.</param>
/// <returns>True when every code point agrees.</returns>
let private wideAgrees (cmap: CmapAccel) =
    let index = cmap.Index.Value
    let edges =
        [ for range in 0 .. index.Firsts.Length - 1 do
            for edge in [ index.Firsts[range]; index.Lasts[range] ] do
                yield! [ edge - 1u; edge; edge + 1u ] ]
    let spread = [ for step in 0 .. 3000 -> uint32 ((int64 step * 7919L * 53L) % 0x110000L) ] @ [ 0x10FFFFu; 0x110000u; 0x110100u; 0xFFFFFFFFu; 0x20FFu; 0x2100u ]
    Seq.forall (fun codepoint -> Cmap.nominalGlyph cmap codepoint = Cmap.nominalGlyphGeneral cmap.Table cmap.Best cmap.Remap codepoint) (Seq.append edges spread)

/// <summary>Whether the low and Latin tables of a cmap are what the lookup of each code point alone gives.</summary>
/// <param name="cmap">The accelerator.</param>
/// <returns>True when every entry agrees.</returns>
let private tablesAgree (cmap: CmapAccel) =
    let expected codepoint =
        match Cmap.nominalGlyphGeneral cmap.Table cmap.Best cmap.Remap (uint32 codepoint) with
        | ValueSome glyph -> glyph
        | ValueNone -> 0u
    cmap.Low.Value.Length = 0x2100
    && cmap.Latin.Length = 256
    && Seq.forall (fun codepoint -> cmap.Low.Value[codepoint] = expected codepoint) (seq { 0 .. 0x20FF })
    && Seq.forall (fun codepoint -> cmap.Latin[codepoint] = expected codepoint) (seq { 0 .. 255 })
    && wideAgrees cmap

/// <summary>The cmap of a file's first face, with the bytes of its cmap table and where they sit in the file.</summary>
/// <param name="bytes">The file bytes.</param>
/// <returns>The accelerator and the table's absolute offset and length, or none when the font has no cmap.</returns>
let private cmapOf (bytes: byte array) =
    let face = Face.create (ReadOnlyMemory bytes) 0u
    match Face.tryFindTable face Tags.Cmap with
    | ValueNone -> ValueNone
    | ValueSome record ->
        let otFace = OtFace.create face
        ValueSome (struct (otFace.Cmap.Value, int (face.BaseOffset + record.Offset), int record.Length))

/// <summary>The fonts whose cmap is small enough to mutate quickly, as their bytes and cmap place.</summary>
let private smallCmaps =
    lazy (
        fontFiles.Value
        |> Array.choose (fun path ->
            let bytes = File.ReadAllBytes path
            match bytes.Length < 600000 with
            | false -> None
            | true ->
                match cmapOf bytes with
                | ValueSome (struct (cmap, at, length)) when cmap.Present && length <= 6000 -> Some (struct (bytes, at, length))
                | ValueSome _ | ValueNone -> None))

/// <summary>Whether the presence table answers, for every code point below U+10000 and for the edges and a spread above it, what the
/// lookup of the code point alone does, the code points asked in an order that builds the pages as they are met.</summary>
/// <param name="cmap">The accelerator.</param>
/// <returns>True when every code point agrees.</returns>
let private presenceAgrees (cmap: CmapAccel) =
    let presence = Cmap.presenceOf cmap
    let expected codepoint =
        match Cmap.nominalGlyph cmap codepoint with
        | ValueSome _ -> true
        | ValueNone -> false
    let order = Seq.append (seq { 0xAC00u .. 0xD7FFu }) (Seq.append (seq { 0x1100u .. 0x11FFu }) (Seq.append (seq { 0u .. 0xABFFu }) (seq { 0xD800u .. 0x10200u })))
    Seq.forall (fun codepoint -> Cmap.hasGlyph cmap presence codepoint = expected codepoint) order
    && Cmap.presenceOf cmap = presence

/// <summary>The low and Latin cmap tables equal the per-code-point lookup.</summary>
[<Tests>]
let tests =
    testList "Cmap low table" [
        test "every corpus font: the presence table equals the lookup of each code point" {
            let mutable checkedFonts = 0
            for path in fontFiles.Value do
                match cmapOf (File.ReadAllBytes path) with
                | ValueNone -> ()
                | ValueSome (struct (cmap, _, _)) ->
                    checkedFonts <- checkedFonts + 1
                    Expect.isTrue (presenceAgrees cmap) path
            Expect.isGreaterThan checkedFonts 100 "the corpus has fonts with a cmap"
        }
        test "every corpus font: the low and Latin tables and the index equal the lookup of each code point" {
            let mutable checkedFonts = 0
            for path in fontFiles.Value do
                match cmapOf (File.ReadAllBytes path) with
                | ValueNone -> ()
                | ValueSome (struct (cmap, _, _)) ->
                    match cmap.Present with
                    | false -> ()
                    | true ->
                        checkedFonts <- checkedFonts + 1
                        Expect.isTrue (tablesAgree cmap) path
            Expect.isGreaterThan checkedFonts 100 "the corpus has fonts with a cmap"
        }
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 6000 } "mutated cmap tables: the low and Latin tables and the index equal the lookup of each code point" <|
            Prop.forAll
                (Arb.fromGen (gen {
                    let fonts = smallCmaps.Value
                    let! pick = Gen.choose (0, fonts.Length - 1)
                    let! changes = Gen.choose (1, 3)
                    let! edits = Gen.listOfLength changes (Gen.zip (Gen.choose (0, 5999)) (Gen.frequency [ 1, Gen.constant 0; 1, Gen.constant 255; 3, Gen.choose (0, 255) ]))
                    return struct (pick, edits)
                }))
                (fun (struct (pick, edits)) ->
                    let struct (bytes, at, length) = smallCmaps.Value[pick]
                    let changed = Array.copy bytes
                    for (position, value) in edits do
                        changed[at + position % length] <- byte value
                    match cmapOf changed with
                    | ValueNone -> true
                    | ValueSome (struct (cmap, _, _)) -> not cmap.Present || tablesAgree cmap)
    ]
