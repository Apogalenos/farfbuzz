module Farfbuzz.Tests.Shape.Perf3Tests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.Allocation

/// <summary>Whether two immutable arrays hold the same elements, the content comparison of the run checks.</summary>
/// <param name="a">The first array.</param>
/// <param name="b">The second array.</param>
/// <returns>True when their elements match.</returns>
let private spanEq (a: ImmutableArray<'T>) (b: ImmutableArray<'T>) =
    a.AsSpan().SequenceEqual(b.AsSpan())

/// <summary>Whether two runs agree on every field a shaping pass reads or writes.</summary>
/// <param name="a">The first run.</param>
/// <param name="b">The second run.</param>
/// <returns>True when the runs agree.</returns>
let private runEq (a: GlyphRun) (b: GlyphRun) : bool =
    spanEq a.Infos b.Infos
    && spanEq a.Props b.Props
    && spanEq a.Positions b.Positions
    && a.HavePositions = b.HavePositions
    && a.MaxOps = b.MaxOps
    && a.Successful = b.Successful
    && a.Serial = b.Serial
    && a.RandomState = b.RandomState
    && a.ScratchFlags = b.ScratchFlags

/// <summary>Generates one glyph's properties with a random category and the hidden and ignorable bits.</summary>
/// <returns>The generator.</returns>
let private genProps : Gen<GlyphProps> =
    gen {
        let! category = Gen.elements [ GeneralCategory.LowercaseLetter; GeneralCategory.NonSpacingMark; GeneralCategory.DecimalNumber; GeneralCategory.SpacingMark; GeneralCategory.EnclosingMark ]
        let! hidden = Gen.elements [ false; true ]
        let! ignorable = Gen.elements [ false; true ]
        let! glyphIndex = Gen.choose (0, 10) |> Gen.map uint32
        let baseProps = { Unchecked.defaultof<GlyphProps> with GlyphIndex = glyphIndex; GlyphClass = 0us }
        let baseProps = UnicodeProps.setGeneralCategory category baseProps
        let baseProps = match hidden with | true -> { baseProps with UnicodeProps = baseProps.UnicodeProps ||| UnicodeProps.Hidden } | false -> baseProps
        let baseProps = match ignorable with | true -> { baseProps with UnicodeProps = baseProps.UnicodeProps ||| UnicodeProps.Ignorable } | false -> baseProps
        return baseProps
    }

/// <summary>Generates a run of up to six glyphs with the given code points and random properties and clusters.</summary>
/// <param name="codepoints">The code-point generator.</param>
/// <returns>The generator.</returns>
let private genRun (codepoints: Gen<uint32>) : Gen<GlyphRun> =
    gen {
        let! count = Gen.choose (0, 6)
        let! cps = Gen.listOfLength count codepoints
        let! clusters = Gen.listOfLength count (Gen.choose (0, 4) |> Gen.map uint32)
        let! props = Gen.listOfLength count genProps
        let! hasFractionSlash = Gen.elements [ false; true ]
        let infos = List.map3 (fun cp cluster _ -> { Codepoint = cp; Mask = 0u; Cluster = cluster; Var1 = 0u; Var2 = 0u }) cps clusters props
        let scratch = if hasFractionSlash then ScratchFlags.HasFractionSlash else ScratchFlags.Default
        return { GlyphRun.empty with Infos = Sealed.ofList infos; Props = Sealed.ofList props; ScratchFlags = scratch }
    }

/// <summary>Builds one compiled feature-map entry.</summary>
/// <param name="tag">The feature tag.</param>
/// <param name="mask">The feature mask.</param>
/// <param name="shift">The feature shift.</param>
/// <returns>The entry.</returns>
let private featureMap (tag: Tag) (mask: uint32) (shift: int) : FeatureMap =
    { Tag = tag; GsubIndex = 0xFFFFu; GposIndex = 0xFFFFu; GsubStage = 0; GposStage = 0; Shift = shift; Mask = mask; Mask1 = mask; NeedsFallback = false; AutoZwnj = false; AutoZwj = false; Random = false; PerSyllable = false }

/// <summary>Builds a compiled map from tag, mask and shift triples.</summary>
/// <param name="entries">The triples.</param>
/// <returns>The map.</returns>
let private mkMap (entries: (Tag * uint32 * int) list) : OtMap =
    { OtMap.empty with Features = entries |> List.map (fun (t, m, s) -> featureMap t m s) |> Sealed.ofList }

/// <summary>Builds an ot plan with explicit fraction, fallback and normalization flags.</summary>
/// <param name="hasFrac">Whether fractions fire.</param>
/// <param name="numr">The numerator mask.</param>
/// <param name="frac">The fraction mask.</param>
/// <param name="dnom">The denominator mask.</param>
/// <param name="fallback">Whether fallback mark positioning fires.</param>
/// <param name="normalization">The normalization mode.</param>
/// <param name="map">The compiled map.</param>
/// <param name="features">The user features.</param>
/// <returns>The plan.</returns>
let private mkPlan (hasFrac: bool) (numr: uint32) (frac: uint32) (dnom: uint32) (fallback: bool) (normalization: NormalizationMode) (map: OtMap) (features: ImmutableArray<Feature>) : OtShapePlan =
    { Props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
      Shaper = { Kind = DefaultShaper; GposTag = Tags.Empty; Normalization = normalization; ZeroWidthMarks = ZeroWidthMarksByGdefEarly; FallbackPosition = fallback }
      Map = map
      Data = NoShaperData
      UserFeatures = features
      FracMask = frac
      NumrMask = numr
      DnomMask = dnom
      RtlmMask = 0u
      KernMask = 0u
      RequestedKerning = false
      HasFrac = hasFrac
      HasVert = false
      HasGposMark = false
      ZeroMarks = false
      FallbackGlyphClasses = false
      FallbackMarkPositioning = fallback
      AdjustMarkPositioningWhenZeroing = false
      ApplyGpos = false
      ApplyKern = false
      ApplyFallbackKern = false
      ApplyKerx = false
      ApplyMorx = false
      ApplyTrak = false; Unicode = ValueNone }

/// <summary>The feature tags the setup-masks generator draws from.</summary>
let private featureTags = [| Tags.Liga; Tags.Kern; Tags.Calt; Tags.Dflt |]

/// <summary>Generates one user feature with a random range, value and tag.</summary>
/// <returns>The generator.</returns>
let private genFeature : Gen<Feature> =
    gen {
        let! tag = Gen.elements featureTags
        let! value = Gen.choose (0, 4) |> Gen.map uint32
        let! start = Gen.elements [ 0u; 1u; 2u; 3u ]
        let! stop = Gen.elements [ 1u; 2u; 3u; 4u; 0xFFFFFFFFu ]
        return { Tag = tag; Value = value; Start = start; End = stop }
    }

/// <summary>Generates a plan and run for the setup-masks early exit.</summary>
/// <returns>The generator.</returns>
let private genSetup : Gen<OtShapePlan * GlyphRun> =
    gen {
        let! hasFrac = Gen.elements [ false; true ]
        let! numr = Gen.choose (1, 8) |> Gen.map uint32
        let! frac = Gen.choose (1, 8) |> Gen.map uint32
        let! dnom = Gen.choose (1, 8) |> Gen.map uint32
        let! featureCount = Gen.choose (0, 3)
        let! features = Gen.listOfLength featureCount genFeature
        let! run = genRun (Gen.elements [ 0x41u; 0x42u; 0x2044u; 0x31u; 0x32u ])
        let entries = features |> List.distinctBy (fun f -> f.Tag) |> List.mapi (fun i f -> f.Tag, uint32 (0x10 <<< (i + 2)), i + 2)
        let plan = mkPlan hasFrac numr frac dnom true NormalizationComposedDiacritics (mkMap entries) (Sealed.ofList features)
        return plan, run
    }

/// <summary>Builds a format 12 cmap subtable.</summary>
/// <param name="groups">Start, end and glyph per group.</param>
/// <returns>The bytes.</returns>
let private format12 (groups: (uint32 * uint32 * uint32) list) : byte array =
    let body = groups |> List.map (fun (first, last, glyph) -> Array.concat [ be32 first; be32 last; be32 glyph ]) |> Array.concat
    Array.concat [ be16 12u; be16 0u; be32 (uint32 (16 + body.Length)); be32 0u; be32 (uint32 groups.Length); body ]

/// <summary>Builds a cmap table from sorted encoding records.</summary>
/// <param name="records">Platform, encoding and subtable bytes.</param>
/// <returns>The cmap bytes.</returns>
let private cmapTable (records: (uint16 * uint16 * byte array) list) : byte array =
    let sorted = records |> List.sortBy (fun (platform, encoding, _) -> platform, encoding)
    let headerSize = 4 + 8 * sorted.Length
    let offsets =
        sorted |> List.fold (fun (offsets, offset) (_, _, bytes) -> (offset :: offsets, offset + bytes.Length)) ([], headerSize) |> fst |> List.rev
    let recordBytes =
        List.map2 (fun (platform, encoding, _) offset -> Array.concat [ be16 (uint32 platform); be16 (uint32 encoding); be32 (uint32 offset) ]) sorted offsets |> Array.concat
    let subtables = sorted |> List.map (fun (_, _, bytes) -> bytes) |> Array.concat
    Array.concat [ be16 0u; be16 (uint32 sorted.Length); recordBytes; subtables ]

/// <summary>Builds a font from one cmap table.</summary>
/// <param name="records">The encoding records.</param>
/// <returns>The font.</returns>
let private fontWith (records: (uint16 * uint16 * byte array) list) : Font =
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Cmap, cmapTable records ])) 0u |> Font.create

/// <summary>The normalize-test font: A, B and C map to glyphs 1, 2 and 3; nothing else maps.</summary>
let private normalizeFontValue : Font =
    fontWith [ 3us, 10us, format12 [ 0x41u, 0x41u, 1u; 0x42u, 0x42u, 2u; 0x43u, 0x43u, 3u ] ]

/// <summary>Generates a plan and run for the normalize early exit.</summary>
/// <returns>The generator.</returns>
let private genNormalize : Gen<OtShapePlan * GlyphRun> =
    gen {
        let! normalization = Gen.elements [ NormalizationNone; NormalizationDecomposed; NormalizationComposedDiacritics; NormalizationComposedDiacriticsNoShortCircuit ]
        let! run = genRun (Gen.elements [ 0x41u; 0x42u; 0x43u; 0xE000u ])
        let plan = mkPlan false 0u 0u 0u false normalization OtMap.empty ImmutableArray.Empty
        return plan, run
    }

/// <summary>The PERF3 tests: the early exits match the passes they replace, and allocate nothing in their no-op cases.</summary>
[<Tests>]
let tests =
    testList "PERF3" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } "tryMapAll maps all or none" (
            Prop.forAll (Arb.fromGen (Gen.arrayOf (Gen.choose (0, 9)))) (fun (items: int array) ->
                let mapping i = if i % 2 = 0 then ValueSome (i + 1) else ValueNone
                match Sealed.tryMapAll mapping (ReadOnlySpan items) with
                | ValueSome mapped -> Array.forall (fun i -> i % 2 = 0) items && mapped.AsSpan().SequenceEqual(ReadOnlySpan (Array.map (fun i -> i + 1) items))
                | ValueNone -> not (Array.forall (fun i -> i % 2 = 0) items)))

        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } "setupMasks equals the pass" (
            Prop.forAll (Arb.fromGen genSetup) (fun (plan, run) ->
                runEq (OtSubstitute.setupMasks plan run) (OtSubstitute.setupMasksPass plan run)))

        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } "fallbackMarkPosition equals the pass" (
            Prop.forAll (Arb.fromGen (genRun (Gen.choose (0x40, 0x47) |> Gen.map uint32))) (fun run ->
                let plan = mkPlan false 0u 0u 0u true NormalizationComposedDiacritics OtMap.empty ImmutableArray.Empty
                match OtLayout.fallbackMarkPosition plan (Font.create Face.empty) false run, OtLayout.fallbackMarkPositionPass plan (Font.create Face.empty) false run with
                | Ok actual, Ok expected -> runEq actual expected
                | _ -> false))

        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } "normalize equals the pass" (
            Prop.forAll (Arb.fromGen genNormalize) (fun (plan, run) ->
                runEq (Normalize.run plan normalizeFontValue run) (Normalize.runPass plan normalizeFontValue run)))

        testCase "a fraction slash first or last keeps the pass" (fun _ ->
            for codepoint in [ 0x2044u ] do
                for position in [ 0; 4 ] do
                    let codepoints = Array.create 5 0x41u
                    codepoints[position] <- codepoint
                    let run =
                        GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
                        |> OtProps.setUnicodeProps 0xFFFFFFFFu
                        |> TailCalls.settle
                    let plan = mkPlan true 4u 8u 16u true NormalizationComposedDiacritics OtMap.empty ImmutableArray.Empty
                    Expect.isTrue (runEq (OtSubstitute.setupMasks plan run) (OtSubstitute.setupMasksPass plan run)) "setupMasks agrees")

        testCase "a mark first or last keeps the fallback pass" (fun _ ->
            for position in [ 0; 4 ] do
                let codepoints = Array.create 5 0x41u
                codepoints[position] <- 0x301u
                let run =
                    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
                    |> OtProps.setUnicodeProps 0xFFFFFFFFu
                    |> TailCalls.settle
                let plan = mkPlan false 0u 0u 0u true NormalizationComposedDiacritics OtMap.empty ImmutableArray.Empty
                match OtLayout.fallbackMarkPosition plan (Font.create Face.empty) false run, OtLayout.fallbackMarkPositionPass plan (Font.create Face.empty) false run with
                | Ok actual, Ok expected -> Expect.isTrue (runEq actual expected) "fallbackMarkPosition agrees"
                | _ -> failtest "expected Ok")
    ]

/// <summary>The PERF3 allocation cases.</summary>
[<Tests>]
let allocationTests =
    testList "PERF3" [
        test "setupMasks without a fraction slash or user features allocates a constant" {
            let plan = mkPlan false 0u 0u 0u true NormalizationComposedDiacritics OtMap.empty ImmutableArray.Empty
            let measure (length: int) =
                let codepoints = Array.create length 0x41u
                let run = GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty |> TailCalls.settle
                bytesAllocatedBy (fun () -> OtSubstitute.setupMasks plan run |> ignore)
            let thousand = measure 1000
            let twoThousand = measure 2000
            Expect.isLessThanOrEqual (twoThousand - thousand) 256L $"setupMasks grew per glyph (1 000 = {thousand}, 2 000 = {twoThousand})"
        }
        test "fallbackMarkPosition with no mark allocates nothing" {
            let font = Font.create Face.empty
            let run = GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x42u |]) 0 -1 GlyphRun.empty |> TailCalls.settle
            let plan = mkPlan false 0u 0u 0u true NormalizationComposedDiacritics OtMap.empty ImmutableArray.Empty
            match OtLayout.fallbackMarkPosition plan font false run with
            | Ok _ -> Expect.equal (bytesAllocatedBy (fun () -> OtLayout.fallbackMarkPosition plan font false run |> ignore)) 0L "bytes allocated"
            | Error _ -> failtest "expected Ok"
        }
        test "normalize of simple text allocates a constant number of arrays" {
            let plan = mkPlan false 0u 0u 0u false NormalizationComposedDiacritics OtMap.empty ImmutableArray.Empty
            let measure (length: int) =
                let codepoints = Array.create length 0x41u
                let run = GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty
                bytesAllocatedBy (fun () -> Normalize.run plan normalizeFontValue run |> ignore)
            let thousand = measure 1000
            let twoThousand = measure 2000
            Expect.isLessThanOrEqual (twoThousand - thousand) (int64 (2000 * (sizeof<uint32> + sizeof<GlyphProps>) * 2)) $"normalize grew per glyph (1 000 = {thousand}, 2 000 = {twoThousand})"
        }
    ]
