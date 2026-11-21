module Farfbuzz.Tests.Shape.SimpleRunTests

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Threading
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests

/// <summary>The layout-table tags a stripped copy of a font renames away.</summary>
let private layoutTags =
    [ "GSUB"; "GPOS"; "GDEF"; "kern"; "morx"; "mort"; "kerx"; "trak"; "ankr"; "feat" ]
    |> List.map (fun text -> uint32 (Tag.ofString text))
    |> Set.ofList

/// <summary>Renames the layout tables of a font so shaping reads none of them, as the benchmarks do.</summary>
/// <param name="source">The font bytes.</param>
/// <returns>The stripped copy.</returns>
let private stripped (source: ReadOnlyMemory<byte>) =
    let bytes = source.ToArray()
    let count = int (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan(bytes, 4, 2)))
    for index in 0 .. count - 1 do
        let record = 12 + index * 16
        if record + 4 <= bytes.Length && layoutTags.Contains (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan(bytes, record, 4))) then
            bytes[record] <- if bytes[record] = byte 'X' then byte 'Y' else byte 'X'
    ReadOnlyMemory bytes

/// <summary>The fonts of the property: names, and whether the layout tables are stripped first.</summary>
let internal fontSources =
    [| "perf/fonts/Roboto-Regular.ttf", false
       "perf/fonts/Roboto-Regular.ttf", true
       "test/api/fonts/OpenSans-Regular.ttf", false
       "test/api/fonts/OpenSans-Regular.ttf", true
       "test/api/fonts/NotoSans-Bold.ttf", false
       "test/api/fonts/Inconsolata-Regular.abc.ttf", false
       "test/api/fonts/Mplus1p-Regular.ttf", false
       "test/api/fonts/Estedad-VF.ttf", false
       "test/api/fonts/Mada-VF.ttf", false
       "test/api/fonts/Cantarell.A.otf", false
       "test/api/fonts/NotoNastaliqUrdu-Regular.ttf", false
       "test/shape/data/text-rendering-tests/fonts/Selawik-variable.ttf", false
       "test/shape/data/aots/fonts/gsub1_1_simple_f1.otf", false
       "test/shape/data/aots/fonts/gsub1_1_modulo_f1.otf", false
       "test/shape/data/aots/fonts/gsub1_2_simple_f1.otf", false
       "test/shape/data/aots/fonts/gsub2_1_simple_f1.otf", false
       "test/shape/data/aots/fonts/gsub2_1_multiple_sequences_f1.otf", false
       "test/shape/data/aots/fonts/gsub3_1_simple_f1.otf", false
       "test/shape/data/aots/fonts/gsub_chaining1_simple_f1.otf", false
       "test/shape/data/aots/fonts/gsub1_1_lookupflag_f1.otf", false
       "test/shape/data/in-house/fonts/bef923f4ccb474f961c43b63a9c74b7d9b7a023f.ttf", false
       "test/shape/data/aots/fonts/gpos2_1_simple_f1.otf", false
       "test/shape/data/aots/fonts/gpos4_simple_1.otf", false
       "test/shape/data/aots/fonts/gsub4_1_simple_f1.otf", false
       "test/shape/data/aots/fonts/gsub4_1_multiple_ligatures_f1.otf", false
       "test/shape/data/aots/fonts/gsub4_1_multiple_ligatures_f2.otf", false
       "test/shape/data/aots/fonts/gsub4_1_multiple_ligsets_f1.otf", false
       "test/shape/data/aots/fonts/gsub4_1_lookupflag_f1.otf", false
       "test/shape/data/aots/fonts/gsub_context1_simple_f1.otf", false |]

/// <summary>The fonts whose text is drawn from words rich in ligatures: unstripped Roboto, Open Sans, Noto Sans.</summary>
let internal ligatureWordFonts = [| 0; 2; 4 |]

/// <summary>The words of the ligature-rich text of a run.</summary>
let private ligatureWords =
    [| "fi"; "ffi"; "fl"; "ff"; "ffl"; "fj"; "office"; "affluent"; "fish"; "fluffy"; "efficient"; "flow"; "life"; "stuff"; "Fi"; "fif"; "fff"; "ft"; "fb"; "fh" |]

/// <summary>The index of the first font that carries real lookups for the property, a font of the upstream test data
/// whose lookups the test feature or the default features turn on.</summary>
[<Literal>]
let internal FirstLookupFont = 12

/// <summary>The fonts, loaded once.</summary>
let internal fonts =
    lazy
        (fontSources
         |> Array.map (fun (path, strip) ->
             let bytes = UpstreamFiles.read path
             Font.create (Face.create (if strip then stripped bytes else bytes) 0u)))

/// <summary>The code points of U+0000 to U+024F that each font maps to a glyph, so a run of a test font hits its
/// lookups.</summary>
let internal mappedCodepoints =
    lazy
        (fonts.Value
         |> Array.map (fun font ->
             [| 0u .. 0x24Fu |]
             |> Array.filter (fun codepoint -> Font.nominalGlyph font codepoint |> ValueOption.isSome)))

/// <summary>Compares two shaped runs on every field.</summary>
/// <param name="what">What the runs are, for the failure message.</param>
/// <param name="expected">The reference run.</param>
/// <param name="actual">The candidate run.</param>
let internal expectSameRunAll (what: string) (expected: GlyphRun) (actual: GlyphRun) =
    Expect.sequenceEqual (actual.Infos.AsSpan().ToArray()) (expected.Infos.AsSpan().ToArray()) $"{what}: infos"
    Expect.sequenceEqual (actual.Props.AsSpan().ToArray()) (expected.Props.AsSpan().ToArray()) $"{what}: props"
    Expect.sequenceEqual (actual.Positions.AsSpan().ToArray()) (expected.Positions.AsSpan().ToArray()) $"{what}: positions"
    Expect.equal actual.ContextBefore expected.ContextBefore $"{what}: context before"
    Expect.equal actual.ContextAfter expected.ContextAfter $"{what}: context after"
    Expect.equal actual.ContentType expected.ContentType $"{what}: content type"
    Expect.equal actual.SegmentProperties expected.SegmentProperties $"{what}: segment properties"
    Expect.equal actual.Flags expected.Flags $"{what}: flags"
    Expect.equal actual.ClusterLevel expected.ClusterLevel $"{what}: cluster level"
    Expect.equal actual.Replacement expected.Replacement $"{what}: replacement"
    Expect.equal actual.Invisible expected.Invisible $"{what}: invisible"
    Expect.equal actual.NotFound expected.NotFound $"{what}: not found"
    Expect.equal actual.NotFoundVariationSelector expected.NotFoundVariationSelector $"{what}: not found variation selector"
    Expect.equal actual.RandomState expected.RandomState $"{what}: random state"
    Expect.equal actual.HavePositions expected.HavePositions $"{what}: have positions"
    Expect.equal actual.Successful expected.Successful $"{what}: successful"
    Expect.equal actual.Serial expected.Serial $"{what}: serial"
    Expect.equal actual.ScratchFlags expected.ScratchFlags $"{what}: scratch flags"
    Expect.equal actual.MaxLen expected.MaxLen $"{what}: max len"
    Expect.equal actual.MaxOps expected.MaxOps $"{what}: max ops"

/// <summary>The number of runs checked whose shaped positions left the nominal advances, a kerning pair or another position
/// adjustment having applied.</summary>
let internal kernedRuns = ref 0

/// <summary>Whether a shaped horizontal run has a position that is not the nominal advance of its glyph.</summary>
/// <param name="font">The font.</param>
/// <param name="shaped">The shaped run.</param>
/// <returns>True when a position was adjusted.</returns>
let private adjusted (font: Font) (shaped: GlyphRun) =
    let nominal = Font.hAdvancePositions font (shaped.Infos.AsSpan())
    Direction.isHorizontal shaped.SegmentProperties.Direction
    && Seq.exists2 (fun (position: GlyphPosition) (plain: GlyphPosition) -> position <> plain) shaped.Positions nominal

/// <summary>The outcome code of a run the general pipeline shaped.</summary>
[<Literal>]
let private GeneralOutcome = 0

/// <summary>The outcome code of a run that has no plan.</summary>
[<Literal>]
let private NoPlanOutcome = 7

/// <summary>The outcome code of a path of the kernel entry.</summary>
/// <param name="path">The path.</param>
/// <returns>1 for lookup-free, 2 for no lookup applies, 3 for substitution unchanged, 4 for substitution fast, 5 for
/// substitution general, 6 for positioning general, 8 for positioning fast, 9 for positioning tail, 10 for the AAT stages.</returns>
let private outcomeOf (path: SimplePath) =
    match path with
    | LookupFree -> 1
    | NoLookupApplies -> 2
    | SubstitutionUnchanged -> 3
    | SubstitutionFast -> 4
    | SubstitutionGeneral -> 5
    | PositioningGeneral -> 6
    | PositioningFast -> 8
    | PositioningTail -> 9
    | AatSimple -> 10

/// <summary>Checks one run: the entry that may take the kernel equals the general pipeline in every field.</summary>
/// <param name="font">The font.</param>
/// <param name="features">The user features.</param>
/// <param name="run">The run.</param>
/// <returns>The outcome code: 0 when the general pipeline shaped the run, 1 to 6, 8 and 9 for the path of the kernel, 7 when
/// the run has no plan; and whether the shaped run holds a glyph formed by a ligature that took glyphs away.</returns>
let private checkRun (font: Font) (features: ImmutableArray<Feature>) (run: GlyphRun) =
    match Execute.createPlan font run.SegmentProperties features ValueNone with
    | Error _ -> struct (NoPlanOutcome, false)
    | Ok plan ->
        let general = Execute.runGeneral plan font features run
        let entry = Execute.run plan font features run
        let ligated =
            match general, entry with
            | Ok expected, Ok actual ->
                expectSameRunAll "run" expected actual
                if adjusted font expected then Interlocked.Increment kernedRuns |> ignore
                expected.Infos.Length < run.Infos.Length && expected.Props.AsSpan().ToArray() |> Array.exists (fun props -> props.GlyphClass &&& GlyphPropsBits.Ligated <> 0us)
            | Error expected, Error actual ->
                Expect.equal actual expected "errors"
                false
            | _ -> failtest "one entry shaped the run and the other refused it"
        match SimpleRun.tryShapePath plan font run with
        | ValueSome (struct (path, _)) -> struct (outcomeOf path, ligated)
        | ValueNone -> struct (GeneralOutcome, ligated)

/// <summary>Counts the cases of each outcome.</summary>
/// <param name="results">The outcome codes and whether a ligature formed.</param>
/// <returns>The report line.</returns>
let private outcomeReport (results: struct (int * bool) array) =
    let outcomes = results |> Array.map (fun (struct (code, _)) -> code)
    let ligated = results |> Array.filter (fun (struct (_, formed)) -> formed)
    let ligatedAt code = ligated |> Array.filter (fun (struct (outcome, _)) -> outcome = code) |> Array.length
    let count code = outcomes |> Array.filter (fun outcome -> outcome = code) |> Array.length
    $"{outcomes.Length} cases, {ligated.Length} formed a ligature (of which {ligatedAt 3} substitution unchanged, {ligatedAt 4} fast, {ligatedAt 5} general tail, {ligatedAt 6} positioning general, {ligatedAt 8} positioning fast, {ligatedAt 9} positioning tail, {ligatedAt GeneralOutcome} general pipeline): {count 1} lookup-free, {count 2} no lookup applies, {count 3} substitution unchanged, {count 4} substitution fast, {count 5} substitution general, {count 6} positioning general, {count 8} positioning fast, {count 9} positioning tail, {count GeneralOutcome} general pipeline, {count NoPlanOutcome} no plan"

/// <summary>Generates a code point weighted towards Latin, with marks, ignorables, right-to-left letters, spaces and
/// the code points the kernel refuses.</summary>
let private genAnyCodepoint =
    Gen.frequency
        [ 60, Gen.choose (0x20, 0x7E)
          4, Gen.choose (0, 0x1F)
          8, Gen.choose (0x80, 0xFF)
          4, Gen.choose (0x100, 0x24F)
          3, Gen.choose (0x370, 0x4FF)
          4, Gen.choose (0x300, 0x36F)
          3, Gen.elements [ 0xAD; 0x200B; 0x200C; 0x200D; 0x2060; 0xFE0F; 0xFE00; 0x034F; 0x180B; 0xE0020; 0xFEFF; 0x061C ]
          2, Gen.choose (0x5D0, 0x5EA)
          2, Gen.choose (0x627, 0x64A)
          2, Gen.elements [ 0xA0; 0x2000; 0x2001; 0x2002; 0x2003; 0x2004; 0x2005; 0x2006; 0x2007; 0x2008; 0x2009; 0x200A; 0x3000; 0x202F; 0x205F; 0x1680 ]
          2, Gen.elements [ 0x2044; 0x1F1E6; 0x1F3FB; 0xFF9E; 0x1F600; 0x25CC; 0x2011; 0x0E01; 0x0915; 0xAC00; 0x4E00; 0x00E9; 0x1E9B ]
          2, Gen.choose (0x2000, 0x2BFF)
          1, Gen.choose (0, 0x2FFFF) ]
    |> Gen.map uint32

/// <summary>Generates a code point of plain Latin text: mostly printable ASCII, some Latin-1 letters.</summary>
let private genLatinCodepoint =
    Gen.frequency
        [ 88, Gen.choose (0x20, 0x7E)
          9, Gen.choose (0xA1, 0xFF)
          2, Gen.choose (0x100, 0x17F)
          1, Gen.elements [ 0x09; 0x0A; 0xA0 ] ]
    |> Gen.map uint32

/// <summary>Generates the text of a run: mostly plain Latin, a quarter of the runs from the wild distribution.</summary>
let internal genText (fontIndex: int) =
    gen {
        let! length = Gen.frequency [ 5, Gen.choose (1, 3); 6, Gen.choose (4, 24); 1, Gen.choose (25, 120) ]
        let! kind = Gen.frequency [ 3, Gen.constant 0; 1, Gen.constant 1; (if fontIndex >= FirstLookupFont then 8 else 0), Gen.constant 2; (if Array.contains fontIndex ligatureWordFonts then 8 else 0), Gen.constant 3 ]
        let mapped = mappedCodepoints.Value[fontIndex]
        match kind with
        | 0 -> return! Gen.arrayOfLength length genLatinCodepoint
        | 1 -> return! Gen.arrayOfLength length genAnyCodepoint
        | 3 ->
            let! words = Gen.listOfLength (1 + length / 4) (Gen.frequency [ 8, Gen.elements (List.ofArray ligatureWords); 2, Gen.arrayOfLength 3 genLatinCodepoint |> Gen.map (fun codepoints -> String (Array.map char codepoints)) ])
            let! separator = Gen.elements [ " "; ""; "" ]
            return (String.Join (separator, words)).ToCharArray() |> Array.map uint32
        | _ -> return! Gen.arrayOfLength length (Gen.frequency [ 9, Gen.elements mapped; 1, genLatinCodepoint ])
    }

/// <summary>Generates the direction and script of a run.</summary>
let internal genSegment =
    Gen.frequency
        [ 60, Gen.constant (Direction.Ltr, Scripts.Latin)
          8, Gen.constant (Direction.Ltr, Scripts.Common)
          6, Gen.elements [ Direction.Ltr, Scripts.Cyrillic; Direction.Ltr, Scripts.Greek ]
          6, Gen.constant (Direction.Rtl, Scripts.Latin)
          4, Gen.constant (Direction.Rtl, Scripts.Arabic)
          3, Gen.constant (Direction.Rtl, Scripts.Hebrew)
          3, Gen.constant (Direction.Ttb, Scripts.Latin)
          2, Gen.constant (Direction.Btt, Scripts.Latin)
          4, Gen.constant (Direction.Ltr, Scripts.Arabic)
          4, Gen.constant (Direction.Invalid, Scripts.Invalid) ]

/// <summary>Generates the user features of a run: none, or one to three, global or ranged.</summary>
let internal genFeatures (fontIndex: int) =
    let genFeature =
        gen {
            let! tag = Gen.elements [ "kern"; "liga"; "clig"; "smcp"; "frac"; "numr"; "dnom"; "ccmp"; "locl"; "calt"; "mark"; "mkmk"; "vert"; "rtlm"; "ss01"; "zero"; "aalt"; "test" ]
            let! value = Gen.choose (0, 2)
            let! ranged = Gen.frequency [ 7, Gen.constant false; 3, Gen.constant true ]
            let! start = Gen.choose (0, 6)
            let! length = Gen.choose (0, 6)
            return
                { Tag = Tag.ofString tag
                  Value = uint32 value
                  Start = if ranged then uint32 start else Feature.GlobalStart
                  End = if ranged then uint32 (start + length) else Feature.GlobalEnd }
        }
    let genTest =
        gen {
            let! value = Gen.choose (0, 3)
            return { Tag = Tag.ofString "test"; Value = uint32 value; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
        }
    gen {
        let! count = Gen.frequency [ 13, Gen.constant 0; 5, Gen.constant 1; 1, Gen.choose (2, 3) ]
        let! list = Gen.listOfLength count genFeature
        let! withTest = Gen.frequency [ 4, Gen.constant (fontIndex >= FirstLookupFont); 1, Gen.constant false ]
        let! test = genTest
        return ImmutableArray.CreateRange (if withTest then test :: list else list)
    }

/// <summary>Generates a flag set, mostly the defaults.</summary>
let internal genFlags =
    Gen.frequency [ 5, Gen.constant BufferFlags.Default; 4, Gen.choose (0, 0xFF) |> Gen.map (fun bits -> LanguagePrimitives.EnumOfValue<uint32, BufferFlags> (uint32 bits)) ]

/// <summary>Generates the clusters of a run: the indices, or a non-decreasing sequence with repeats.</summary>
/// <param name="length">The run length.</param>
let internal genClusters (length: int) =
    gen {
        let! repeated = Gen.frequency [ 17, Gen.constant false; 3, Gen.constant true ]
        match repeated with
        | false -> return Array.init length uint32
        | true ->
            let! steps = Gen.arrayOfLength length (Gen.choose (0, 2))
            return Array.scan (fun total step -> total + uint32 step) 0u steps |> Array.truncate length
    }

/// <summary>Generates a case: a font index, user features and a run, with every field of the run varied.</summary>
let private genCase =
    gen {
        let! fontIndex = Gen.choose (0, fontSources.Length - 1)
        let! text = genText fontIndex
        let! (direction, script) = genSegment
        let! features = genFeatures fontIndex
        let! flags = genFlags
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! clusters = genClusters text.Length
        let! wildMasks = Gen.frequency [ 4, Gen.constant false; 1, Gen.constant true ]
        let! wildVars = Gen.frequency [ 9, Gen.constant false; 1, Gen.constant true ]
        let! garbage = Gen.arrayOfLength text.Length (Gen.choose (0, Int32.MaxValue))
        let! successful = Gen.frequency [ 24, Gen.constant true; 1, Gen.constant false ]
        let! smallOps = Gen.frequency [ 17, Gen.constant false; 3, Gen.constant true ]
        let! ops = Gen.choose (0, 5000)
        let! randomState = Gen.choose (0, Int32.MaxValue)
        let! serial = Gen.choose (0, 200)
        let! scratch = Gen.choose (0, 0xFF)
        let! invisible = Gen.elements [ 0u; 0u; 3u; 77u ]
        let! notFound = Gen.elements [ 0u; 0u; 5u ]
        let garbagePositions = false
        let! language = Gen.elements [ Language.cLocale; NoLanguage ]
        let infos =
            Sealed.init text.Length (fun index ->
                { Codepoint = text[index]
                  Mask = if wildMasks then uint32 garbage[index] else 0u
                  Cluster = clusters[index]
                  Var1 = if wildVars then uint32 garbage[index] else 0u
                  Var2 = if wildVars then uint32 (garbage[index] >>> 3) else 0u })
        let props =
            Sealed.init text.Length (fun index ->
                if wildMasks then { Unchecked.defaultof<GlyphProps> with GlyphClass = uint16 garbage[index]; GlyphIndex = uint32 garbage[index] }
                else Unchecked.defaultof<GlyphProps>)
        let positions =
            match garbagePositions with
            | true -> Sealed.init text.Length (fun index -> { XAdvance = garbage[index] % 900; YAdvance = 0; XOffset = index; YOffset = 0; Var = 0u })
            | false -> ImmutableArray.Empty
        let run =
            { GlyphRun.empty with
                Infos = infos
                Props = props
                Positions = positions
                HavePositions = garbagePositions
                ContentType = UnicodeContent
                SegmentProperties = { Direction = direction; Script = script; Language = language }
                Flags = flags
                ClusterLevel = level
                Invisible = invisible
                NotFound = notFound
                RandomState = uint32 randomState
                Successful = successful
                Serial = uint32 serial
                ScratchFlags = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> (uint32 scratch)
                MaxOps = if smallOps then ops else GlyphRun.MaxOpsDefault }
        let guessed =
            match direction with
            | Direction.Invalid -> GlyphRun.guessSegmentProperties Language.cLocale run
            | _ -> run
        return struct (fontIndex, features, guessed)
    }

/// <summary>Whether <c>GsubApply.trySubtable</c> can let a subtable of this type match today. The match names every case:
/// a new subtable type fails the build here, and in <c>Engine.substSubtableGlyphs</c>, until both say what it does.</summary>
/// <param name="subtable">The subtable.</param>
/// <returns>True for the types that apply.</returns>
let private appliesToday (subtable: SubstSubtable) =
    match subtable with
    | SingleDelta _ -> true
    | SingleList _ -> true
    | Multiple _ -> true
    | Alternate _ -> true
    | LigatureSubst _ -> true
    | SubstContext _ -> true
    | SubstChainContext _ -> true
    | ReverseChainSingle _ -> true
    | SubstPending _ -> false

/// <summary>The first components a ligature subtable can start a match with.</summary>
/// <param name="subtable">The subtable.</param>
/// <returns>The glyphs of its coverage, none for another type.</returns>
let private ligatureStarts (subtable: SubstSubtable) =
    match subtable with
    | LigatureSubst (coverage, _, _) -> Coverage.glyphs coverage
    | SingleDelta _ -> ImmutableArray.Empty
    | SingleList _ -> ImmutableArray.Empty
    | Multiple _ -> ImmutableArray.Empty
    | Alternate _ -> ImmutableArray.Empty
    | SubstContext _ -> ImmutableArray.Empty
    | SubstChainContext _ -> ImmutableArray.Empty
    | ReverseChainSingle _ -> ImmutableArray.Empty
    | SubstPending _ -> ImmutableArray.Empty

/// <summary>The number of random cases of the equality property.</summary>
[<Literal>]
let private CaseCount = 24000

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x51D1E5UL

/// <summary>Shapes every line of a text through both entries and compares them.</summary>
/// <param name="font">The font.</param>
/// <param name="path">The text, relative to the upstream submodule.</param>
/// <returns>The outcome code of every line and whether a ligature formed.</returns>
let private checkLines (font: Font) (path: string) =
    let bytes = (UpstreamFiles.read path).ToArray()
    let outcomes = ResizeArray<struct (int * bool)>()
    let mutable start = 0
    for index in 0 .. bytes.Length - 1 do
        if bytes[index] = 0x0Auy then
            let run =
                GlyphRun.addUtf8 (ReadOnlySpan(bytes, start, bytes.Length - start)) 0 (index - start) GlyphRun.empty
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            outcomes.Add (checkRun font ImmutableArray.Empty run)
            start <- index + 1
    outcomes.ToArray()

/// <summary>The equality of the simple-run kernel and the general pipeline.</summary>
[<Tests>]
let simpleRunTests =
    testList "simple run kernel" [
        test "the entry equals the general pipeline in every field over random runs" {
            let cases = Gen.sampleWithSeed (Rnd Seed) 30 CaseCount genCase
            let check (struct (fontIndex, features, run: GlyphRun)) =
                try
                    checkRun fonts.Value[fontIndex] features run
                with error ->
                    printfn $"FAILED font {fontIndex} features {features.Length} ops {run.MaxOps} flags {run.Flags} level {run.ClusterLevel} props {run.SegmentProperties} text {run.Infos.AsSpan().ToArray() |> Array.map (fun info -> info.Codepoint)} {error.Message}"
                    reraise ()
            let results = cases |> Seq.map check |> Seq.toArray
            let fastWith predicate =
                Seq.zip cases results
                |> Seq.filter (fun (struct (_, _, run: GlyphRun), struct (code, _)) -> (code = 4 || code = 9) && predicate run.Flags)
                |> Seq.length
            let fusedFast = fastWith (fun flags -> flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default)
            let tatweelFast = fastWith (fun flags -> flags &&& BufferFlags.ProduceSafeToInsertTatweel <> BufferFlags.Default)
            printfn $"simple run property, fast tail: {fusedFast} runs finished in one pass over their clusters, {tatweelFast} with the tatweel flag through the shared tail"
            Expect.isGreaterThan fusedFast 300 "many substituted runs take the one-pass finish"
            Expect.isGreaterThan tatweelFast 20 "substituted runs with the tatweel flag take the shared tail"
            let outcomes = results |> Array.map (fun (struct (code, _)) -> code)
            let kernel = outcomes |> Array.filter (fun outcome -> outcome <> GeneralOutcome && outcome <> NoPlanOutcome) |> Array.length
            let general = outcomes |> Array.filter (fun outcome -> outcome = GeneralOutcome) |> Array.length
            let count code = outcomes |> Array.filter (fun outcome -> outcome = code) |> Array.length
            let formed = results |> Array.filter (fun (struct (_, ligated)) -> ligated) |> Array.length
            let formedByKernel = results |> Array.filter (fun (struct (code, ligated)) -> ligated && code <> GeneralOutcome && code <> NoPlanOutcome) |> Array.length
            printfn $"simple run property: {outcomeReport results}"
            printfn $"simple run property: {kernedRuns.Value} of the runs checked so far had a position adjusted by GPOS, {outcomes |> Array.filter (fun outcome -> outcome = 8) |> Array.length} took the fast tail after the positioning kernel and {outcomes |> Array.filter (fun outcome -> outcome = 9) |> Array.length} the shared tail"
            Expect.isGreaterThan kernedRuns.Value 2000 "many runs are kerned"
            Expect.isGreaterThan (count 8) 500 "many runs are positioned by the kernel and finished by the fast tail"
            Expect.isGreaterThan formed 300 "several hundred cases form a ligature"
            Expect.isGreaterThan formedByKernel 100 "the kernel handles runs that form a ligature"
            Expect.equal outcomes.Length CaseCount "every case is compared"
            Expect.isGreaterThan kernel 4000 "the kernel is taken often"
            Expect.isGreaterThan general 4000 "the general pipeline is taken often"
            Expect.isGreaterThan (count 4 + count 9) 100 "substituted runs take the fast tail"
        }

        test "the gate counts every subtable type that applies, and a ligature subtable by its first component" {
            let mutable ligatureSubtables = 0
            let mutable contextSubtables = 0
            for font in fonts.Value do
                let table = font.Ot.Gsub.Value
                for lookup in table.Lookups do
                    for accel in lookup.Value.Subtables do
                        let glyphs = Engine.substSubtableGlyphs accel.Subtable
                        Expect.equal (appliesToday accel.Subtable) (not glyphs.IsEmpty) "a subtable that applies is counted, a pending one is not"
                        Expect.sequenceEqual glyphs (ligatureStarts accel.Subtable |> fun starts -> if starts.IsEmpty then glyphs else starts) "a ligature subtable counts the glyphs of its coverage"
                        match accel.Subtable with
                        | LigatureSubst _ -> ligatureSubtables <- ligatureSubtables + 1
                        | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ -> contextSubtables <- contextSubtables + 1
                        | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | SubstPending _ -> ()
            Expect.isGreaterThan ligatureSubtables 0 "the fonts hold ligature subtables"
            Expect.isGreaterThan contextSubtables 0 "the fonts hold context, chain and reverse subtables"
        }

        test "a run a ligature lookup can change is not skipped by the gate" {
            let font = fonts.Value[0]
            let run =
                GlyphRun.addUtf8 (ReadOnlySpan("office fi fl".ToCharArray() |> Array.map byte)) 0 12 GlyphRun.empty
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            let struct (code, ligated) = checkRun font ImmutableArray.Empty run
            Expect.isTrue ligated "unstripped Roboto forms ligatures in the run"
            Expect.notEqual code GeneralOutcome "the kernel takes it"
            Expect.notEqual code 1 "the plan is not lookup-free"
            Expect.notEqual code 2 "the gate does not skip the ligature lookup"
            Expect.notEqual code 3 "the substitution changed the run"
        }

        test "a run a ligature lookup changes is finished by the fast tail alone, which positions it once" {
            let font = fonts.Value[0]
            let bytes = Text.Encoding.UTF8.GetBytes (String.replicate 100 "fi office ")
            let run =
                GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            match Execute.createPlan font run.SegmentProperties ImmutableArray.Empty ValueNone with
            | Error _ -> failtest "the plan of unstripped Roboto is built"
            | Ok plan ->
                let measure () =
                    Execute.run plan font ImmutableArray.Empty run |> ignore
                    let before = GC.GetAllocatedBytesForCurrentThread()
                    Execute.run plan font ImmutableArray.Empty run |> ignore
                    GC.GetAllocatedBytesForCurrentThread() - before
                let bytesAllocated = measure ()
                match SimpleRun.tryShapePath plan font run with
                | ValueNone -> failtest "the run qualifies for the kernel"
                | ValueSome (struct (path, _)) -> Expect.equal path SimplePath.PositioningTail "a hundred ligatures are kerned and take the shared tail"
                let glyphs = int64 run.Infos.Length
                Expect.isLessThanOrEqual bytesAllocated (glyphs * 170L + 16384L) $"{bytesAllocated} B for {glyphs} glyphs: the kernel's arrays, the run the lookups read, their published copy, the positions and the propagated infos, and no general positioning on top"
        }

        test "every short run of the ligature fonts equals the general pipeline, matches that fail midway included" {
            let flagSets = [| BufferFlags.Default; BufferFlags.ProduceUnsafeToConcat |]
            let test = ImmutableArray.Create { Tag = Tag.ofString "test"; Value = 1u; Start = Feature.GlobalStart; End = Feature.GlobalEnd }
            let rec strings (alphabet: uint32 array) length =
                match length with
                | 0 -> [ [||] ]
                | _ -> [ for tail in strings alphabet (length - 1) do for head in alphabet -> Array.append [| head |] tail ]
            let results =
                [ for fontIndex in 23 .. 27 do
                    let alphabet = mappedCodepoints.Value[fontIndex] |> Array.filter (fun codepoint -> codepoint >= 0x11u && codepoint <= 0x18u)
                    for length in 1 .. 4 do
                        for text in strings alphabet length do
                            for flags in flagSets do
                                let bytes = Text.Encoding.UTF8.GetBytes (String (Array.map char text))
                                let run =
                                    { GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty with Flags = flags }
                                    |> GlyphRun.guessSegmentProperties Language.cLocale
                                    |> TailCalls.settle
                                yield checkRun fonts.Value[fontIndex] test run ]
                |> List.toArray
            printfn $"ligature fonts, every short run: {outcomeReport results}"
            Expect.isGreaterThan (results |> Array.filter (fun (struct (_, formed)) -> formed) |> Array.length) 1000 "ligatures form"
            Expect.isGreaterThan (results |> Array.filter (fun (struct (code, formed)) -> not formed && (code = 3 || code = 4)) |> Array.length) 1000 "lookups run on runs that form no ligature, matches that fail midway among them"
        }

        test "a run that is not successful comes back with its glyphs when a lookup applies" {
            let font = fonts.Value[4]
            let run =
                { GlyphRun.addUtf8 (ReadOnlySpan("office affluent fi ffl".ToCharArray() |> Array.map byte)) 0 22 GlyphRun.empty with
                    Successful = false
                    MaxOps = 4540 }
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            match Execute.createPlan font run.SegmentProperties ImmutableArray.Empty ValueNone with
            | Error _ -> failtest "the plan"
            | Ok plan ->
                match Execute.runGeneral plan font ImmutableArray.Empty run, Execute.run plan font ImmutableArray.Empty run with
                | Ok general, Ok entry ->
                    Expect.isFalse general.Successful "still not successful"
                    Expect.equal general.Infos.Length 22 "no glyph is lost"
                    expectSameRunAll "run" general entry
                | _ -> failtest "the pipeline refused the run"
        }

        test "a run that is not successful and grows while normalizing ends" {
            let codepoints =
                [| 960u; 111u; 234u; 118u; 236u; 65024u; 13u; 117u; 167u; 71u; 32u; 35u; 790u; 235u; 102u; 12u; 201u; 112u; 121u; 68u; 90u; 103u; 48u; 108u; 93u; 177u; 78u; 52u; 531u; 40u; 54u
                   8196u; 72u; 771u; 100u; 849u; 40u; 38u; 8199u; 85u; 8202u; 63u; 39u; 56u; 90u; 47u; 69u; 201u; 836u; 297u; 39u; 111u; 64u; 299u; 38u; 73u; 119u; 61u; 117u; 101u; 37u; 38u; 35u; 67u
                   32u; 8200u; 100u; 103u; 88u; 107u; 87u; 105u; 244u; 41u; 117u; 65279u; 2325u; 93u |]
            let infos = Sealed.init codepoints.Length (fun index -> { Codepoint = codepoints[index]; Mask = 0u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
            let run =
                { GlyphRun.empty with
                    Infos = infos
                    Props = Sealed.init codepoints.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
                    ContentType = UnicodeContent
                    SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
                    Successful = false }
            let outcome = Threading.Tasks.Task.Run(fun () -> checkRun fonts.Value[11] ImmutableArray.Empty run)
            Expect.isTrue (outcome.Wait 20000) "the shaping of a failed run ends"
        }

        test "no run throws or hangs under a small operation budget or length limit" {
            let operations = [| 0; 1; 2; 5; 17; 100; 1000; 4540 |]
            let lengths = [| 0; 1; 3; 8; 20; 65536; GlyphRun.MaxLenDefault |]
            let cases = Gen.sampleWithSeed (Rnd (Seed + 1UL)) 30 CaseCount genCase
            let outcomes =
                cases
                |> Seq.mapi (fun index (struct (fontIndex, features, run: GlyphRun)) ->
                    let limited = { run with MaxOps = operations[index % operations.Length]; MaxLen = lengths[index / operations.Length % lengths.Length]; Successful = index % 3 <> 0 }
                    checkRun fonts.Value[fontIndex] features limited)
                |> Seq.toArray
            Expect.equal outcomes.Length CaseCount "every case ran to the end"
        }

        test "the fast tail after a substitution equals the general positioning, and refuses what it cannot prove" {
            let font = fonts.Value[0]
            let run =
                GlyphRun.addUtf8 (ReadOnlySpan("Hello, world 42".ToCharArray() |> Array.map byte)) 0 15 GlyphRun.empty
                |> GlyphRun.guessSegmentProperties Language.cLocale
                |> TailCalls.settle
            let plan = match Execute.createPlan font run.SegmentProperties ImmutableArray.Empty ValueNone with | Ok plan -> plan | Error _ -> failtest "the plan"
            let otPlan = match plan.Ot with | ValueSome otPlan -> otPlan | ValueNone -> failtest "the ot plan"
            let entered = GlyphRun.enter run
            let native =
                OtProps.setUnicodePropsFused otPlan.Map.GlobalMask entered
                |> OtProps.insertDottedCircle font
                |> OtProps.formClusters
                |> OtProps.ensureNativeDirection
            let substituted = match OtSubstitute.substitutePre otPlan font native with | Ok substituted -> substituted | Error _ -> failtest "the substitution"
            let reference (state: GlyphRun) =
                match OtShaper.fromSubstituted otPlan font run.SegmentProperties.Direction state with
                | Ok shaped -> { shaped with MaxLen = GlyphRun.MaxLenDefault; MaxOps = GlyphRun.MaxOpsDefault; Serial = 0u }
                | Error _ -> failtest "the positioning"
            let mark = { substituted.Props[3] with GlyphClass = GlyphPropsBits.Mark }
            let states =
                [ "untouched", substituted, true
                  "a mark-class glyph", { substituted with Props = Sealed.mapi (fun index (props: GlyphProps) -> if index = 3 then mark else props) (substituted.Props.AsSpan()) }, false
                  "a continuation glyph", { substituted with Props = Sealed.mapi (fun index (props: GlyphProps) -> if index = 3 then { props with UnicodeProps = props.UnicodeProps ||| UnicodeProps.Continuation } else props) (substituted.Props.AsSpan()) }, false
                  "a scratch flag", { substituted with ScratchFlags = ScratchFlags.HasSpaceFallback }, false
                  "a failed run", { substituted with Successful = false }, false ]
            for (name, state, fast) in states do
                let struct (path, result) = SimpleRun.afterSubstitution otPlan font run state
                match result with
                | Ok actual -> expectSameRunAll name (reference state) actual
                | Error _ -> failtest "the tail refused the run"
                Expect.equal path (if fast then SubstitutionFast else SubstitutionGeneral) $"{name}: path"
        }

        test "the entry equals the general pipeline on every line of both Roboto texts" {
            for index in 0 .. 1 do
                let font = fonts.Value[index]
                let results = Array.append (checkLines font "perf/texts/en-thelittleprince.txt") (checkLines font "perf/texts/en-words.txt")
                let outcomes = results |> Array.map (fun (struct (code, _)) -> code)
                let label = if index = 0 then "with layout" else "stripped"
                printfn $"roboto {label}: {outcomeReport results}"
                Expect.isGreaterThan outcomes.Length 10000 "both texts are read"
                Expect.equal (outcomes |> Array.filter (fun outcome -> outcome <> GeneralOutcome && outcome <> NoPlanOutcome) |> Array.length) outcomes.Length "every line takes the kernel"
        }

        test "the entry equals the general pipeline on lines of the AAT fonts of the corpus and of this machine" {
            let corpus =
                [ "test/shape/data/in-house/fonts/a8b4a746c2d1a1fd5d5d6108e366e79c0ae74ef5.ttf"
                  "test/shape/data/text-rendering-tests/fonts/TestAATMorx.ttf"
                  "test/shape/data/text-rendering-tests/fonts/TestMORXFourtyone.ttf"
                  "test/shape/data/text-rendering-tests/fonts/TestAATMorx.ttf" ]
            let system =
                [ "/System/Library/Fonts/Helvetica.ttc"
                  "/System/Library/Fonts/Supplemental/Hoefler Text.ttc"
                  "/System/Library/Fonts/Supplemental/Zapfino.ttf" ]
                |> List.filter System.IO.File.Exists
            let fontOf (bytes: byte array) = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
            let cmapLines (font: Font) =
                let rnd = Random 11
                let mapped = [| 0x20u .. 0x24Fu |] |> Array.filter (fun codepoint -> match Font.nominalGlyph font codepoint with | ValueSome _ -> true | ValueNone -> false)
                let marks = [| 0x300u; 0x301u; 0x200Du; 0xADu; 0x2009u |]
                let pool = Array.append mapped marks
                Array.init 150 (fun _ ->
                    let text = String(Array.init (rnd.Next(1, 40)) (fun _ -> char pool[rnd.Next pool.Length]))
                    let bytes = Text.Encoding.UTF8.GetBytes text
                    GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty
                    |> GlyphRun.guessSegmentProperties Language.cLocale
                    |> TailCalls.settle)
            let corpusOutcomes =
                corpus
                |> List.collect (fun path ->
                    let font = fontOf ((UpstreamFiles.read path).ToArray())
                    cmapLines font |> Array.map (fun run -> checkRun font ImmutableArray.Empty run) |> Array.toList)
            let systemOutcomes =
                system
                |> List.collect (fun path ->
                    let font = fontOf (System.IO.File.ReadAllBytes path)
                    Array.append (checkLines font "perf/texts/en-thelittleprince.txt") (cmapLines font |> Array.map (fun run -> checkRun font ImmutableArray.Empty run))
                    |> Array.toList)
            let count code (outcomes: struct (int * bool) list) = outcomes |> List.filter (fun (struct (outcome, _)) -> outcome = code) |> List.length
            printfn $"AAT fonts of the corpus: {corpusOutcomes.Length} lines, {count 10 corpusOutcomes} on the AAT path, {count GeneralOutcome corpusOutcomes} general; this machine: {systemOutcomes.Length} lines, {count 10 systemOutcomes} on the AAT path, {count GeneralOutcome systemOutcomes} general"
            Expect.isGreaterThan (count 10 corpusOutcomes) 0 "some line of the corpus fonts takes the AAT path"
            Expect.isGreaterThan (count GeneralOutcome corpusOutcomes) 0 "some line of the corpus fonts takes the general pipeline: the gate refuses marks and ignorables"
        }
    ]
