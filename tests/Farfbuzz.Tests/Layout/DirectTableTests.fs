module Farfbuzz.Tests.Layout.DirectTableTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz

/// <summary>The seed of the generated cases.</summary>
[<Literal>]
let private Seed = 160UL

/// <summary>The number of cases of each property.</summary>
[<Literal>]
let private CaseCount = 4000

/// <summary>A glyph id bound for a case: small, medium, and large enough to pass the table's limit.</summary>
let private genSpan = Gen.elements [ 60; 300; 2000; 20000 ]

/// <summary>A glyph array: listed in order without repeats, or in any order with repeats as a broken font may list it.</summary>
let private genGlyphs =
    gen {
        let! span = genSpan
        let! length = Gen.frequency [ 1, Gen.choose (0, 7); 6, Gen.choose (8, 70) ]
        let! ordered = Gen.frequency [ 3, Gen.constant true; 1, Gen.constant false ]
        let! glyphs = Gen.arrayOfLength length (Gen.choose (0, span))
        return
            match ordered with
            | true -> glyphs |> Array.distinct |> Array.sort |> Array.map uint32
            | false -> glyphs |> Array.map uint32
    }

/// <summary>A coverage: an array, or ranges that may overlap, be backwards or be listed out of order.</summary>
let private genCoverage =
    gen {
        let! isRanges = Gen.elements [ false; true ]
        match isRanges with
        | false ->
            let! glyphs = genGlyphs
            return CoverageGlyphs (ImmutableArray.CreateRange glyphs)
        | true ->
            let! span = genSpan
            let! length = Gen.frequency [ 1, Gen.choose (0, 7); 6, Gen.choose (8, 40) ]
            let! ordered = Gen.frequency [ 3, Gen.constant true; 1, Gen.constant false ]
            let! raw = Gen.arrayOfLength length (Gen.zip3 (Gen.choose (0, span)) (Gen.choose (-2, 40)) (Gen.frequency [ 8, Gen.choose (0, 3000); 1, Gen.choose (60000, 70000) ]))
            let records = raw |> Array.map (fun (first, width, value) -> { First = uint32 first; Last = uint32 (max 0 (first + width)); Value = uint32 value })
            let kept =
                match ordered with
                | true -> records |> Array.sortBy (fun record -> record.First) |> Array.fold (fun (kept: RangeRecord list) record -> match kept with | previous :: _ when record.First <= previous.Last -> kept | _ -> record :: kept) [] |> List.rev |> List.toArray
                | false -> records
            return CoverageRanges (ImmutableArray.CreateRange kept)
    }

/// <summary>A range class definition, with classes that fit a slot and, rarely, classes that do not.</summary>
let private genClassDef =
    gen {
        let! span = genSpan
        let! length = Gen.frequency [ 1, Gen.choose (0, 7); 6, Gen.choose (8, 40) ]
        let! ordered = Gen.frequency [ 3, Gen.constant true; 1, Gen.constant false ]
        let! largest = Gen.frequency [ 9, Gen.constant 300; 1, Gen.constant 70000 ]
        let! raw = Gen.arrayOfLength length (Gen.zip3 (Gen.choose (0, span)) (Gen.choose (-2, 60)) (Gen.choose (0, largest)))
        let records = raw |> Array.map (fun (first, width, value) -> { First = uint32 first; Last = uint32 (max 0 (first + width)); Value = uint32 value })
        let kept =
            match ordered with
            | true -> records |> Array.sortBy (fun record -> record.First) |> Array.fold (fun (kept: RangeRecord list) record -> match kept with | previous :: _ when record.First <= previous.Last -> kept | _ -> record :: kept) [] |> List.rev |> List.toArray
            | false -> records
        return ClassDefRanges (ImmutableArray.CreateRange kept)
    }

/// <summary>The glyph ids a comparison asks: every id up to a bound past the table's limit's neighbourhood, and ids far out of range.</summary>
let private probes =
    Array.concat [ Array.init 2100 uint32; Array.init 2000 (fun step -> uint32 (2100 + step * 9)); Array.init 40 (fun step -> uint32 (16370 + step)); [| 0xFFFFu; 0x10000u; 0xFFFFFFu; 0x1000000u; UInt32.MaxValue - 1u; UInt32.MaxValue |] ]

/// <summary>Whether a coverage carries a direct table.</summary>
/// <param name="coverage">The coverage.</param>
/// <returns>True when it does.</returns>
let private isDirectCoverage coverage =
    match coverage with
    | CoverageDirect _ -> true
    | CoverageNull | CoverageGlyphs _ | CoverageRanges _ -> false

/// <summary>Whether a class definition carries a direct table.</summary>
/// <param name="classDef">The class definition.</param>
/// <returns>True when it does.</returns>
let private isDirectClassDef classDef =
    match classDef with
    | ClassDefDirect _ -> true
    | ClassDefNull | ClassDefArray _ | ClassDefRanges _ -> false

/// <summary>The properties of the direct tables of coverages and class definitions.</summary>
[<Tests>]
let tests =
    testList "direct tables" [
        test "the direct coverage answers every glyph id as the coverage it was built from does" {
            let cases = Gen.sampleWithSeed (Rnd Seed) 30 CaseCount genCoverage
            let built = cases |> Seq.filter (fun coverage -> isDirectCoverage (Coverage.direct coverage)) |> Seq.length
            for coverage in cases do
                let direct = Coverage.direct coverage
                for glyph in probes do
                    Expect.equal (Coverage.index direct glyph) (Coverage.index coverage glyph) $"glyph {glyph}"
                Expect.equal (Coverage.digest direct) (Coverage.digest coverage) "digest"
                Expect.equal (Coverage.glyphs direct) (Coverage.glyphs coverage) "glyphs"
            printfn $"direct coverage property: {cases.Length} cases, {built} with a table"
            Expect.isGreaterThan built 400 "many cases build a table"
        }

        test "the direct class definition answers every glyph id as the class definition it was built from does" {
            let cases = Gen.sampleWithSeed (Rnd (Seed + 1UL)) 30 CaseCount genClassDef
            let built = cases |> Seq.filter (fun classDef -> isDirectClassDef (ClassDef.direct classDef)) |> Seq.length
            for classDef in cases do
                let direct = ClassDef.direct classDef
                for glyph in probes do
                    Expect.equal (ClassDef.classOf direct glyph) (ClassDef.classOf classDef glyph) $"glyph {glyph}"
                for klass in [ 0u; 1u; 2u; 7u; 299u ] do
                    Expect.equal (ClassDef.glyphsInClass direct klass) (ClassDef.glyphsInClass classDef klass) $"class {klass}"
            printfn $"direct class definition property: {cases.Length} cases, {built} with a table"
            Expect.isGreaterThan built 400 "many cases build a table"
        }

        test "a coverage or class definition that is small, sparse, or holds a value a slot cannot is left as it is" {
            let small = CoverageGlyphs (ImmutableArray.Create (1u, 2u, 3u))
            Expect.equal (isDirectCoverage (Coverage.direct small)) false "small coverage"
            let sparse = CoverageGlyphs (ImmutableArray.CreateRange (Array.init 8 (fun index -> uint32 (index * 3000))))
            Expect.equal (isDirectCoverage (Coverage.direct sparse)) false "sparse coverage"
            let dense = CoverageGlyphs (ImmutableArray.CreateRange (Array.init 8 uint32))
            Expect.equal (isDirectCoverage (Coverage.direct dense)) true "dense coverage"
            let wide = ClassDefRanges (ImmutableArray.CreateRange (Array.init 8 (fun index -> { First = uint32 (index * 4); Last = uint32 (index * 4 + 3); Value = 70000u })))
            Expect.equal (isDirectClassDef (ClassDef.direct wide)) false "class beyond a slot"
            let array = ClassDefArray (0u, ImmutableArray.CreateRange (Array.init 20 uint32))
            Expect.equal (isDirectClassDef (ClassDef.direct array)) false "array form"
        }

        test "a coverage and a class definition that reach beyond the table's limit answer past it from their source" {
            let coverage = CoverageGlyphs (ImmutableArray.CreateRange (Array.init 20000 uint32))
            let direct = Coverage.direct coverage
            Expect.equal (isDirectCoverage direct) true "table built"
            let ranges = ImmutableArray.CreateRange (Array.init 300 (fun index -> { First = uint32 (index * 67); Last = uint32 (index * 67 + 40); Value = uint32 (index % 9) }))
            let classDef = ClassDefRanges ranges
            let directClasses = ClassDef.direct classDef
            Expect.equal (isDirectClassDef directClasses) true "class table built"
            for glyph in Array.append (Array.init 20200 uint32) [| UInt32.MaxValue |] do
                Expect.equal (Coverage.index direct glyph) (Coverage.index coverage glyph) $"coverage glyph {glyph}"
                Expect.equal (ClassDef.classOf directClasses glyph) (ClassDef.classOf classDef glyph) $"class glyph {glyph}"
        }
    ]
