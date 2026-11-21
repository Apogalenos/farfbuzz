module Farfbuzz.Tests.ComplexShapers.WouldSubstituteTests

open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests


/// <summary>An Indic font under test, the script and the code point ranges whose glyphs the sequences are drawn from.</summary>
type private Subject =
    { Name: string
      Font: string
      Script: Script
      Ranges: (int * int) list }

/// <summary>The Indic fonts the properties run over.</summary>
let private subjects =
    [ { Name = "Devanagari"; Font = "test/subset/data/fonts/NotoSansDevanagari-Regular.ttf"; Script = Scripts.Devanagari; Ranges = [ (0x900, 0x97F) ] }
      { Name = "Telugu"; Font = "test/subset/data/fonts/SreeKrushnadevaraya-Regular.ttf"; Script = Scripts.Telugu; Ranges = [ (0xC00, 0xC7F) ] }
      { Name = "Kannada"; Font = "test/shape/data/text-rendering-tests/fonts/NotoSerifKannada-Regular.ttf"; Script = Scripts.Kannada; Ranges = [ (0xC80, 0xCFF) ] }
      { Name = "Bengali"; Font = "test/subset/data/fonts/IndicTestHowrah-Regular.ttf"; Script = Scripts.Bengali; Ranges = [ (0x980, 0x9FF) ] }
      { Name = "Gurmukhi"; Font = "test/subset/data/fonts/IndicTestJalandhar-Regular.ttf"; Script = Scripts.Gurmukhi; Ranges = [ (0xA00, 0xA7F) ] }
      { Name = "Malayalam"; Font = "test/shape/data/in-house/fonts/55e2910dbc9ef5dd89f4e146e7e0152169545b6a.ttf"; Script = Scripts.Malayalam; Ranges = [ (0xD00, 0xD7F) ] }
      { Name = "Oriya"; Font = "test/subset/data/fonts/NotoSansOriya-subset.ttf"; Script = Scripts.Oriya; Ranges = [ (0xB00, 0xB7F) ] } ]

/// <summary>The font, the plan data and the glyph pool of a subject, made once.</summary>
let private prepared =
    lazy
        (subjects
         |> List.map (fun subject ->
             let font = Font.create (Face.create (UpstreamFiles.read subject.Font) 0u)
             let props = { Direction = Direction.Ltr; Script = subject.Script; Language = Language.cLocale }
             let plan =
                 match Execute.createPlan font props ImmutableArray<Feature>.Empty ValueNone with
                 | Error _ -> failwith "no plan"
                 | Ok shapePlan ->
                     match shapePlan.Ot with
                     | ValueNone -> failwith "no ot plan"
                     | ValueSome ot ->
                         match ot.Data with
                         | IndicData data -> data
                         | NoShaperData | HangulData _ | KhmerData _ | UseData _ | ArabicData _ -> failwith "no indic data"
             let pool =
                 subject.Ranges
                 |> List.collect (fun (first, last) -> [ first .. last ])
                 |> List.choose (fun cp -> match Font.nominalGlyph font (uint32 cp) with | ValueSome glyph -> Some glyph | ValueNone -> None)
                 |> List.toArray
             struct (subject, font, plan, pool))
         |> List.toArray)

/// <summary>The features of a plan, with the pair that names them.</summary>
/// <param name="plan">The plan data.</param>
/// <returns>The five features.</returns>
let private featuresOf (plan: IndicPlan) = [ plan.RphfFeature; plan.PrefFeature; plan.BlwfFeature; plan.PstfFeature; plan.VatuFeature ]

/// <summary>The answer of asking each lookup of a feature in turn, as the code did before the prepared tester.</summary>
/// <param name="face">The face.</param>
/// <param name="feature">The feature.</param>
/// <param name="glyphs">The sequence.</param>
/// <returns>True when a lookup would substitute it.</returns>
let private reference (face: OtFace) (feature: IndicWouldSubstitute) (glyphs: ImmutableArray<uint32>) =
    feature.Lookups.AsSpan().ToArray() |> Array.exists (fun lookup -> LayoutApi.wouldSubstitute face lookup glyphs feature.ZeroContext)

/// <summary>Generates a glyph sequence of one to four glyphs from the pool, with an occasional glyph of the whole face.</summary>
/// <param name="pool">The pool.</param>
/// <param name="count">The glyph count of the face.</param>
/// <returns>The generator.</returns>
let private genSequence (pool: uint32[]) (count: int) =
    let glyph = Gen.frequency [ 12, Gen.elements pool; 1, Gen.map uint32 (Gen.choose (0, count)) ]
    gen {
        let! length = Gen.choose (1, 4)
        let! glyphs = Gen.arrayOfLength length glyph
        return ImmutableArray.CreateRange glyphs
    }

/// <summary>Runs a property with a fixed seed.</summary>
/// <param name="name">The name.</param>
/// <param name="generator">The generator.</param>
/// <param name="check">The property.</param>
/// <returns>The test.</returns>
let private property name generator check =
    testCase name (fun _ ->
        let config = Config.QuickThrowOnFailure.WithMaxTest(4000).WithQuietOnSuccess(true)
        Check.One(config, Prop.forAll (Arb.fromGen generator) check))

[<Tests>]
let tests =
    testList "would substitute prepared" [
        for index in 0 .. subjects.Length - 1 do
            let name = subjects[index].Name
            yield property $"the prepared tester equals asking each lookup, with and without zero context ({name})"
                (gen {
                    let struct (_, font, _, pool) = prepared.Value[index]
                    return! genSequence pool font.Face.GlyphCount })
                (fun glyphs ->
                    let struct (_, font, plan, _) = prepared.Value[index]
                    featuresOf plan
                    |> List.forall (fun feature ->
                        [ feature.ZeroContext; not feature.ZeroContext ]
                        |> List.forall (fun zero ->
                            let fresh = { feature with ZeroContext = zero; Prepared = Memo<obj>() }
                            IndicInitial.anyWouldSubstitute font.Ot fresh glyphs 0 = reference font.Ot fresh glyphs
                            && IndicInitial.anyWouldSubstitute font.Ot fresh glyphs 0 = reference font.Ot fresh glyphs)))

            yield property $"the kept consonant position equals the one worked out from the features ({name})"
                (gen {
                    let struct (_, font, _, pool) = prepared.Value[index]
                    return! genSequence pool font.Face.GlyphCount })
                (fun glyphs ->
                    let struct (_, font, plan, _) = prepared.Value[index]
                    match Indic.viramaGlyph plan font with
                    | ValueNone -> true
                    | ValueSome virama ->
                        let consonant = glyphs[0]
                        let expected =
                            let either feature = reference font.Ot feature (ImmutableArray.Create(virama, consonant)) || reference font.Ot feature (ImmutableArray.Create(consonant, virama))
                            match either plan.BlwfFeature || either plan.VatuFeature with
                            | true -> IndicPosition.PosBelowC
                            | false ->
                                match either plan.PstfFeature || either plan.PrefFeature with
                                | true -> IndicPosition.PosPostC
                                | false -> IndicPosition.PosBaseC
                        let ask () = IndicInitial.consonantPositionOfPairs plan font.Ot (ImmutableArray.Create(virama, consonant)) (ImmutableArray.Create(consonant, virama))
                        ask () = expected && ask () = expected)
    ]
