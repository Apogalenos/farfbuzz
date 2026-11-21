module Farfbuzz.Diff.ShapePlanDiff

open System
open System.Collections.Immutable
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Var
open Farfbuzz.Harness
open Farfbuzz.Oracle
open Farfbuzz.Oracle.Native
open Farfbuzz.Oracle.Shims
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.Gens

/// <summary>The feature tags the differential draws from.</summary>
let private featureTags = [| "kern"; "liga"; "smcp"; "dlig"; "calt"; "clig"; "frac"; "onum"; "pnum"; "salt" |]

/// <summary>One random feature, global or ranged.</summary>
let private featureGen : Gen<Feature> =
    gen {
        let! tag = Gen.elements featureTags
        let! value = Gen.elements [| 0u; 1u; 2u |]
        let! kind = Gen.elements [| 0; 1; 2 |]
        match kind with
        | 0 -> return { Tag = Tag.ofString tag; Value = value; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
        | 1 ->
            let! start = Gen.choose (0, 8)
            return { Tag = Tag.ofString tag; Value = value; Start = uint32 start; End = Feature.GlobalEnd }
        | _ ->
            let! start = Gen.choose (0, 8)
            let! length = Gen.choose (1, 8)
            return { Tag = Tag.ofString tag; Value = value; Start = uint32 start; End = uint32 (start + length) }
    }

/// <summary>One coordinate pair over a face: shared properties, features and shapers, two sampled coordinate sets.</summary>
type private CoordCase =
    { Props: SegmentProperties
      Features: Feature array
      Shapers: string array
      CoordsA: int array
      CoordsB: int array }

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) (uint32 font.Index)

/// <summary>Whether a face is variable: it has at least one fvar axis.</summary>
/// <param name="font">The test font.</param>
/// <returns>True when the face has an fvar axis.</returns>
let private isVariable (font: TestFont) : bool =
    (Fvar.load (faceOf font)).Axes.Length > 0

/// <summary>Every variable face of the shape suites, the perf fonts and the subset fonts.</summary>
/// <returns>The faces.</returns>
let private variableFaces () : TestFont array =
    let suites = TestFonts.all ()
    let extra = TestFonts.extra [ "subset/data/fonts/Roboto-Variable.ttf" ] |> Seq.toList
    List.append suites extra
    |> List.filter isVariable
    |> List.toArray

/// <summary>One coordinate pair: shared properties, features and shapers, two independent coordinate sets.</summary>
let private caseGen : Gen<struct (TestFont * CoordCase)> =
    gen {
        let! font = Gen.elements (variableFaces ())
        let! direction = Gen.elements [| Direction.Ltr; Direction.Rtl; Direction.Ttb; Direction.Btt |]
        let! script = Gen.elements [| Scripts.Latin; Scripts.Greek; Scripts.Cyrillic; Scripts.Arabic; Scripts.Hebrew; Scripts.Devanagari; Scripts.Thai |]
        let! language = Gen.elements [| Language.NoLanguage; Language.Bcp47 "en"; Language.Bcp47 "ar"; Language.Bcp47 "fa"; Language.Bcp47 "de" |]
        let! count = Gen.choose (0, 4)
        let! features = Gen.arrayOfLength count featureGen
        let! shapers = Gen.elements [| [| "ot" |]; [| "fallback" |]; [| "ot"; "fallback" |]; [| "fallback"; "ot" |] |]
        let! coordsA = coordsFor font (uint32 font.Index)
        let! coordsB = coordsFor font (uint32 font.Index)
        let props = { Direction = direction; Script = script; Language = language }
        let case =
            { Props = props
              Features = features
              Shapers = shapers
              CoordsA = coordsA |> Seq.toArray
              CoordsB = coordsB |> Seq.toArray }
        return struct (font, case)
    }

/// <summary>The interned oracle language of a Farfbuzz language.</summary>
/// <param name="language">The language.</param>
/// <returns>The language handle, or zero.</returns>
let private languageHandle (language: Language) : nativeint =
    match language with
    | Language.NoLanguage -> 0n
    | Language.Bcp47 text -> Native.hb_language_from_string(Encoding.ASCII.GetBytes text, text.Length)

/// <summary>The oracle plan pointer of a plan input, identifying the plan its cache returned.</summary>
/// <param name="face">The oracle face handle.</param>
/// <param name="props">The segment properties.</param>
/// <param name="features">The user features.</param>
/// <param name="shapers">The requested shapers.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The plan pointer, equal for equal keys.</returns>
let private oraclePtr (face: nativeint) (props: SegmentProperties) (features: Feature array) (shapers: string array) (coords: int array) : nativeint =
    Shims.farfbuzz_oracle_shape_plan_create_cached2(
        face,
        Direction.value props.Direction,
        uint32 props.Script,
        languageHandle props.Language,
        features,
        uint32 features.Length,
        coords,
        uint32 coords.Length,
        Array.append shapers [| Unchecked.defaultof<string> |])

/// <summary>The Farfbuzz plan key of a plan input.</summary>
/// <param name="font">The test font.</param>
/// <param name="props">The segment properties.</param>
/// <param name="features">The user features.</param>
/// <param name="shapers">The requested shapers.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <returns>The key.</returns>
let private portKey (font: TestFont) (props: SegmentProperties) (features: Feature array) (shapers: string array) (coords: int array) : ShapePlanKey =
    let name =
        match Execute.shaperName (ValueSome (ImmutableArray.CreateRange shapers)) with
        | ValueSome name -> name
        | ValueNone -> String.Empty
    let ourFace = faceOf font
    let ourFont = Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) (Font.create ourFace)
    match ShapePlan.key ourFont props (ReadOnlySpan features) name with
    | ValueSome key -> key
    | ValueNone -> failwith "plan key could not be built"

/// <summary>
/// The plan-key equality differential: two cached plans on the same face are the same object exactly when the two
/// Farfbuzz keys are equal, over sampled coordinates on variable faces with and without FeatureVariations.
/// </summary>
let private planKeyEquality () =
    let samples = Gen.sampleWithSeed (Rnd 0x5EEDUL) 5000 5000 caseGen
    let mutable equal = 0
    let mismatches = ResizeArray<string>()
    for struct (font, case) in samples do
        let face = OracleFaces.face font (uint32 font.Index)
        let oracleA = oraclePtr face case.Props case.Features case.Shapers case.CoordsA
        let oracleB = oraclePtr face case.Props case.Features case.Shapers case.CoordsB
        let oracleEqual = oracleA = oracleB
        let portA = portKey font case.Props case.Features case.Shapers case.CoordsA
        let portB = portKey font case.Props case.Features case.Shapers case.CoordsB
        let portEqual = portA = portB
        if oracleEqual then equal <- equal + 1
        if oracleEqual <> portEqual then
            mismatches.Add $"{font.Path}#{font.Index}: oracle equal {oracleEqual}, farfbuzz equal {portEqual}, coords %A{case.CoordsA} vs %A{case.CoordsB}"
    Console.WriteLine $"P9.S07 plan key equality: {equal} of 5000 pairs equal"
    Expect.isEmpty mismatches ("plan-key equality mismatches:\n" + String.concat "\n" mismatches)

/// <summary>The P9.S07 plan-key equality differential against the oracle's plan cache.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P9.S07" [
        testCase "P9.S07 plan key equality" planKeyEquality
    ]
