module Farfbuzz.Tests.Shape.LookupKernelTests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.Shape.SimpleRunTests

/// <summary>One random case of the equality property: a font, a plan and a run of glyphs as the substitution sees it.</summary>
[<Struct>]
type private KernelCase =
    { FontIndex: int
      Features: ImmutableArray<Feature>
      Segment: SegmentProperties
      Run: GlyphRun
      Wild: bool }

/// <summary>The number of random cases of the equality property.</summary>
[<Literal>]
let private CaseCount = 40000

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x4B3E21UL

/// <summary>Generates the user features of a case, the features of the simple-run property with the lookup-bearing test
/// font's own feature turned on more often.</summary>
let private genKernelSegment =
    Gen.frequency
        [ 80, Gen.constant (Direction.Ltr, Scripts.Latin)
          8, Gen.constant (Direction.Rtl, Scripts.Latin)
          4, Gen.constant (Direction.Ltr, Scripts.Common)
          4, Gen.constant (Direction.Rtl, Scripts.Arabic)
          4, Gen.constant (Direction.Ltr, Scripts.Cyrillic) ]

/// <summary>Generates a case: the glyphs, masks, clusters, properties and every buffer field of a run, varied.</summary>
let private genCase =
    gen {
        let! fontIndex = Gen.frequency [ 3, Gen.choose (0, FirstLookupFont - 1); 9, Gen.choose (FirstLookupFont, fontSources.Length - 1) ]
        let! text = genText fontIndex
        let! (direction, script) = genKernelSegment
        let! features = genFeatures fontIndex
        let! flags = genFlags
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! clusters = genClusters text.Length
        let! maskMode = Gen.frequency [ 9, Gen.constant 0; 7, Gen.constant 1; 2, Gen.constant 2 ]
        let! garbage = Gen.arrayOfLength text.Length (Gen.choose (0, Int32.MaxValue))
        let! glyphNoise = Gen.arrayOfLength text.Length (Gen.frequency [ 15, Gen.constant -1; 1, Gen.choose (0, 63) ])
        let! propsMode = Gen.frequency [ 12, Gen.constant 0; 5, Gen.constant 1; 1, Gen.constant 2 ]
        let! successful = Gen.frequency [ 24, Gen.constant true; 1, Gen.constant false ]
        let! smallOps = Gen.frequency [ 17, Gen.constant false; 3, Gen.constant true ]
        let! ops = Gen.choose (0, 3000)
        let! smallLen = Gen.frequency [ 9, Gen.constant 0; 2, Gen.constant 1; 1, Gen.constant 2 ]
        let! extraLen = Gen.choose (0, 12)
        let! randomState = Gen.choose (0, Int32.MaxValue)
        let! serial = Gen.choose (0, 255)
        let! scratch = Gen.choose (0, 0xFF)
        let! withPositions = Gen.frequency [ 24, Gen.constant false; 1, Gen.constant true ]
        let segment = { Direction = direction; Script = script; Language = Language.cLocale }
        return struct (fontIndex, features, segment, text, flags, level, clusters, maskMode, garbage, glyphNoise, propsMode, successful, smallOps, ops, smallLen, extraLen, randomState, serial, scratch, withPositions)
    }

/// <summary>Builds the run of a case once its plan exists: the glyphs the font maps, the masks the plan's lookups test.</summary>
/// <param name="font">The font.</param>
/// <param name="otPlan">The plan the run is substituted by.</param>
/// <param name="parts">The generated fields.</param>
/// <returns>The run.</returns>
let private buildRun (font: Font) (otPlan: OtShapePlan) (parts: struct (int * ImmutableArray<Feature> * SegmentProperties * uint32 array * BufferFlags * ClusterLevel * uint32 array * int * int array * int array * int * bool * bool * int * int * int * int * int * int * bool)) =
    let struct (_, _, segment, text, flags, level, clusters, maskMode, garbage, glyphNoise, propsMode, successful, smallOps, ops, smallLen, extraLen, randomState, serial, scratch, withPositions) = parts
    let globalMask = otPlan.Map.GlobalMask
    let lookupBits = otPlan.Map.GsubLookups.AsSpan().ToArray() |> Array.fold (fun bits lookup -> bits ||| lookup.Mask) 0u
    let gdef = font.Ot.Gdef.Value
    let glyphs =
        Array.init text.Length (fun index ->
            match glyphNoise[index] >= 0 with
            | true -> uint32 glyphNoise[index]
            | false ->
                match Font.nominalGlyph font text[index] with
                | ValueSome glyph -> glyph
                | ValueNone -> text[index] % 40u)
    let infos =
        Sealed.init text.Length (fun index ->
            let mask =
                match maskMode with
                | 0 -> globalMask
                | 1 -> globalMask ||| (uint32 garbage[index] &&& lookupBits &&& ~~~7u)
                | _ -> uint32 garbage[index] ^^^ (uint32 garbage[index] <<< 7)
            { Codepoint = glyphs[index]; Mask = mask; Cluster = clusters[index]; Var1 = 0u; Var2 = 0u })
    let props =
        Sealed.init text.Length (fun index ->
            let struct (unicode, _) = UnicodeProps.compute text[index]
            let glyphClass = if Gdef.hasGlyphClasses gdef then Gdef.glyphProps gdef glyphs[index] else 0us
            match propsMode with
            | 0 -> { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode; GlyphClass = glyphClass }
            | 1 ->
                { Unchecked.defaultof<GlyphProps> with
                    UnicodeProps = unicode
                    GlyphClass = glyphClass ||| (uint16 garbage[index] &&& 0x7Eus)
                    LigProps = byte (garbage[index] >>> 4)
                    Syllable = byte (garbage[index] >>> 9) }
            | _ -> { Unchecked.defaultof<GlyphProps> with UnicodeProps = unicode; GlyphClass = glyphClass })
    let positions =
        match withPositions with
        | true -> Sealed.init text.Length (fun index -> { XAdvance = garbage[index] % 900; YAdvance = 0; XOffset = index; YOffset = 0; Var = 0u })
        | false -> ImmutableArray.Empty
    let maxLen =
        match smallLen with
        | 0 -> GlyphRun.MaxLenDefault
        | 1 -> text.Length + extraLen
        | _ -> extraLen
    { GlyphRun.empty with
        Infos = infos
        Props = (match propsMode with | 2 -> ImmutableArray.Empty | _ -> props)
        Positions = positions
        HavePositions = withPositions
        ContentType = GlyphContent
        SegmentProperties = segment
        Flags = flags
        ClusterLevel = level
        RandomState = uint32 randomState
        Successful = successful
        Serial = uint32 serial
        ScratchFlags = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> (uint32 scratch)
        MaxLen = maxLen
        MaxOps = if smallOps then ops else GlyphRun.MaxOpsDefault }

/// <summary>What one case did to its run.</summary>
[<Struct>]
type private Outcome =
    { Changed: bool
      Ligated: bool
      Deleted: bool
      Grew: bool
      HitBudget: bool
      Admitted: bool }

/// <summary>The table with its context, chained-context and reverse-chaining subtables made inert, the subtable types
/// the interpreter the kernel replaced never applied: the table that interpreter is the reference for.</summary>
/// <param name="table">The table.</param>
/// <returns>The table.</returns>
let private withoutContexts (table: LayoutTable<SubstSubtable>) =
    let strip (accel: LookupAccel<SubstSubtable>) =
        let inert subtable =
            match subtable with
            | SubstContext _ | SubstChainContext _ | ReverseChainSingle _ -> SubstPending (accel.LookupType, 1, Digest.empty, Digest.full)
            | SingleDelta _ | SingleList _ | Multiple _ | Alternate _ | LigatureSubst _ | SubstPending _ -> subtable
        lazy (
            LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest
                { LookupType = accel.LookupType
                  Props = accel.Props
                  IsReverse = accel.IsReverse
                  Subtables = accel.Subtables |> Seq.map (fun entry -> inert entry.Subtable) |> ImmutableArray.CreateRange })
    { table with Lookups = table.Lookups |> Seq.map (fun lookup -> strip lookup.Value) |> ImmutableArray.CreateRange }

/// <summary>Compares the kernel against the interpreter on one case, gated and ungated, applicable and admitted, over the
/// font's table without its context subtables, which the interpreter does not apply; and compares the kernel's paths
/// against each other over the whole table, contexts included: the plan's sets and the digest gates must not change a
/// run any lookup could have changed.</summary>
/// <param name="parts">The generated fields.</param>
/// <returns>What the case did, or none when the font has no plan.</returns>
let private check (parts: struct (int * ImmutableArray<Feature> * SegmentProperties * uint32 array * BufferFlags * ClusterLevel * uint32 array * int * int array * int array * int * bool * bool * int * int * int * int * int * int * bool)) =
    let struct (fontIndex, features, segment, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = parts
    let font = fonts.Value[fontIndex]
    match Execute.createPlan font segment features ValueNone with
    | Error _ -> ValueNone
    | Ok plan ->
        match plan.Ot with
        | ValueNone -> ValueNone
        | ValueSome otPlan ->
            let run = buildRun font otPlan parts
            let maps = otPlan.Map.GsubLookups
            let stages = otPlan.Map.GsubStages
            let inert = withoutContexts font.Ot.Gsub.Value
            let reference = Engine.applyTable true (Engine.GsubApplier()) inert maps stages font run
            let kernel = Engine.applyGsub true inert maps stages font run
            expectSameRunAll "gated" reference kernel
            let referenceUngated = Engine.applyTable false (Engine.GsubApplier()) inert maps stages font run
            let kernelUngated = Engine.applyGsub false inert maps stages font run
            expectSameRunAll "ungated" referenceUngated kernelUngated
            let inertCovered = Engine.lookupSetsOf inert maps
            let inertGlyphLookups = Engine.glyphLookupsOf inert maps
            let referenceCovered = Engine.applyTable true (Engine.GsubApplicableApplier(inertCovered)) inert maps stages font run
            let kernelCovered = Engine.applyGsubCovered true inertCovered inertGlyphLookups inert maps stages font run
            expectSameRunAll "applicable" referenceCovered kernelCovered
            let inertAdmission = Engine.lookAt inertGlyphLookups (run.Infos.AsSpan())
            expectSameRunAll "admitted" referenceCovered (Engine.applyGsubCoveredAdmitted inertCovered inertGlyphLookups inert maps stages font inertAdmission run)
            let positionless = { run with Positions = ImmutableArray.Empty; HavePositions = false }
            let whole = Engine.applyGsub false font.Ot.Gsub.Value maps stages font positionless
            let wholeCovered = Engine.substituteApplicable otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups positionless
            let admission = Engine.lookAt plan.GsubGlyphLookups (positionless.Infos.AsSpan())
            match run.Props.Length = run.Infos.Length with
            | false -> ()
            | true ->
                expectSameRunAll "whole table, ungated and applicable" whole wholeCovered
                expectSameRunAll "whole table, applicable and admitted" wholeCovered (Engine.substituteAdmitted otPlan font plan.GsubLookupCovered plan.GsubGlyphLookups admission positionless)
            let ligated =
                wholeCovered.Infos.Length < run.Infos.Length
                && wholeCovered.Props.AsSpan().ToArray() |> Array.exists (fun props -> props.GlyphClass &&& GlyphPropsBits.Ligated <> 0us)
            ValueSome
                { Changed = wholeCovered.Infos.AsSpan().ToArray() <> run.Infos.AsSpan().ToArray() || wholeCovered.Props.AsSpan().ToArray() <> run.Props.AsSpan().ToArray()
                  Ligated = ligated
                  Deleted = wholeCovered.Infos.Length < run.Infos.Length && not ligated
                  Grew = wholeCovered.Infos.Length > run.Infos.Length
                  HitBudget = run.Successful && not wholeCovered.Successful
                  Admitted = admission.Known && run.Successful && run.Props.Length = run.Infos.Length }

/// <summary>Checks one case and, when it fails, prints the fields that replay it.</summary>
/// <param name="case">The generated fields.</param>
let private checkReporting (case: struct (int * ImmutableArray<Feature> * SegmentProperties * uint32 array * BufferFlags * ClusterLevel * uint32 array * int * int array * int array * int * bool * bool * int * int * int * int * int * int * bool)) =
    try
        check case
    with error ->
        let struct (fontIndex, features, _, text, flags, level, _, maskMode, _, _, propsMode, successful, smallOps, ops, smallLen, extraLen, _, _, _, withPositions) = case
        printfn $"FAILED font {fontIndex} features {features.Length} flags {flags} level {level} masks {maskMode} props {propsMode} successful {successful} ops {smallOps} {ops} len {smallLen} {extraLen} positions {withPositions} text {text} {error.Message}"
        reraise ()

/// <summary>The glyphs of the synthetic tables are the first few ids, so lookups and runs meet often.</summary>
[<Literal>]
let private SyntheticCaseCount = 30000

/// <summary>The properties a synthetic glyph may carry: unmarked, base, ligature, mark and combinations.</summary>
let private syntheticClasses = [| 0us; GlyphPropsBits.Base; GlyphPropsBits.Ligature; GlyphPropsBits.Mark; GlyphPropsBits.Base ||| GlyphPropsBits.Substituted; GlyphPropsBits.Mark ||| GlyphPropsBits.Ligated |]

/// <summary>The code points whose Unicode properties a synthetic glyph may carry.</summary>
let private syntheticUnicode = [| 0x41u; 0x300u; 0x200Du; 0x200Cu; 0xADu; 0x20u; 0x61u; 0x1F3FBu |]

/// <summary>The lookup flags a synthetic lookup may carry.</summary>
let private syntheticLookupProps = [| 0u; 0u; 0u; 0x0002u; 0x0004u; 0x0008u; 0x000Au; 0x000Eu; 0x0100u; 0x0200u |]

/// <summary>The masks a synthetic lookup may test: single bits, a pair, and the byte whose value 255 randomises.</summary>
let private syntheticMasks = [| 0x08u; 0x08u; 0x08u; 0x10u; 0x30u; 0x300u; 0xFF00u; 0x18u |]

/// <summary>Builds a coverage over some glyphs.</summary>
/// <param name="glyphs">The covered glyphs, sorted and distinct.</param>
/// <param name="ranges">Whether to write it as ranges.</param>
let private syntheticCoverage (glyphs: uint32 array) (ranges: bool) =
    match ranges with
    | false -> CoverageGlyphs (ImmutableArray.CreateRange glyphs)
    | true ->
        let records = ImmutableArray.CreateBuilder<RangeRecord>()
        let mutable coverageIndex = 0u
        for glyph in glyphs do
            records.Add { First = glyph; Last = glyph; Value = coverageIndex }
            coverageIndex <- coverageIndex + 1u
        CoverageRanges (records.ToImmutable())

/// <summary>Builds the lookup records of a random rule: nested lookups at the first positions of the match, any lookup of
/// the table, the rule's own lookup included.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="lookupCount">The number of lookups of the table.</param>
let private syntheticRecords (rnd: Random) (lookupCount: int) =
    ImmutableArray.CreateRange (Array.init (match rnd.Next 6 with | 0 -> 0 | 1 | 2 -> 1 | 3 | 4 -> 2 | _ -> 3) (fun _ -> { SequenceIndex = rnd.Next(0, 4); LookupListIndex = rnd.Next lookupCount }))

/// <summary>The first-value digests of the rule sets of a class subtable, the bit of each rule's first class, all bits for
/// a rule with no class after the first glyph.</summary>
/// <param name="counts">The input count of each rule of each set.</param>
/// <param name="firsts">The first value after the first glyph of each rule of each set.</param>
let private syntheticDigests (counts: int array array) (firsts: uint32 array array) =
    Sealed.init counts.Length (fun set ->
        match counts[set] |> Array.exists (fun count -> count <= 1) with
        | true -> UInt64.MaxValue
        | false -> firsts[set] |> Array.fold (fun mask first -> mask ||| (1UL <<< int (first &&& 63u))) 0UL)

/// <summary>Builds one random context subtable of format 1, 2 or 3, rule sets of up to six rules so the fast path of
/// five rules or more is met as often as the slow one.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The largest glyph id.</param>
/// <param name="lookupCount">The number of lookups of the table.</param>
/// <param name="covered">The glyphs of the subtable's coverage.</param>
/// <param name="coverage">The coverage.</param>
let private syntheticContext (rnd: Random) (alphabet: int) (lookupCount: int) (covered: uint32 array) (coverage: Coverage) =
    let glyph () = uint32 (rnd.Next(0, alphabet + 1))
    let inputCount () = match rnd.Next 14 with | 0 -> 0 | 1 | 2 -> 1 | 3 | 4 | 5 -> 2 | 6 | 7 | 8 -> 3 | _ -> rnd.Next(2, 5)
    match rnd.Next 3 with
    | 0 ->
        let counts = covered |> Array.map (fun _ -> Array.init (match rnd.Next 4 with | 0 -> 0 | 1 -> rnd.Next(1, 4) | _ -> rnd.Next(4, 8)) (fun _ -> inputCount ()))
        let sets = counts |> Array.map (fun setCounts -> ImmutableArray.CreateRange (setCounts |> Array.map (fun count -> { InputCount = count; Input = ImmutableArray.CreateRange (Array.init (max 0 (count - 1)) (fun _ -> glyph ())); Records = syntheticRecords rnd lookupCount })))
        SubstContext (GlyphRules (coverage, ImmutableArray.CreateRange sets))
    | 1 ->
        let classes = Array.init (alphabet + 1) (fun _ -> uint32 (rnd.Next(0, 4)))
        let classDef = ClassDefArray (0u, ImmutableArray.CreateRange classes)
        let counts = Array.init (rnd.Next(1, 5)) (fun _ -> Array.init (match rnd.Next 4 with | 0 -> 0 | 1 -> rnd.Next(1, 4) | _ -> rnd.Next(4, 8)) (fun _ -> inputCount ()))
        let firsts = counts |> Array.map (fun setCounts -> setCounts |> Array.map (fun _ -> uint32 (rnd.Next(0, 4))))
        let rules = Array.map2 (fun (setCounts: int array) (setFirsts: uint32 array) -> Array.map2 (fun count first -> { InputCount = count; Input = ImmutableArray.CreateRange (Array.init (max 0 (count - 1)) (fun position -> match position with | 0 -> first | _ -> uint32 (rnd.Next(0, 4)))); Records = syntheticRecords rnd lookupCount }) setCounts setFirsts) counts firsts
        let digests = syntheticDigests (rules |> Array.map (Array.map (fun (rule: Rule) -> rule.InputCount))) (rules |> Array.map (Array.map (fun (rule: Rule) -> match rule.Input.IsEmpty with | true -> 0u | false -> rule.Input[0])))
        SubstContext (ClassRules (coverage, classDef, ImmutableArray.CreateRange (rules |> Array.map ImmutableArray.CreateRange), digests))
    | _ ->
        let coverages = ImmutableArray.CreateRange (Array.init (rnd.Next(1, 4)) (fun position -> match position with | 0 -> coverage | _ -> syntheticCoverage (Array.init (rnd.Next(1, alphabet + 1)) (fun _ -> glyph ()) |> Array.distinct |> Array.sort) false))
        SubstContext (CoverageRule (coverages, syntheticRecords rnd lookupCount))

/// <summary>Builds one random chained-context subtable of format 1, 2 or 3.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The largest glyph id.</param>
/// <param name="lookupCount">The number of lookups of the table.</param>
/// <param name="covered">The glyphs of the subtable's coverage.</param>
/// <param name="coverage">The coverage.</param>
let private syntheticChain (rnd: Random) (alphabet: int) (lookupCount: int) (covered: uint32 array) (coverage: Coverage) =
    let glyph () = uint32 (rnd.Next(0, alphabet + 1))
    let inputCount () = match rnd.Next 14 with | 0 -> 0 | 1 | 2 -> 1 | 3 | 4 | 5 -> 2 | 6 | 7 | 8 -> 3 | _ -> rnd.Next(2, 5)
    let values (limit: int) = ImmutableArray.CreateRange (Array.init (rnd.Next(0, 3)) (fun _ -> uint32 (rnd.Next(0, limit))))
    let rule (limit: int) (count: int) (first: uint32 voption) =
        { Backtrack = values limit
          ChainInputCount = count
          ChainInput = ImmutableArray.CreateRange (Array.init (max 0 (count - 1)) (fun position -> match position, first with | 0, ValueSome value -> value | _ -> uint32 (rnd.Next(0, limit))))
          Lookahead = values limit
          ChainRecords = syntheticRecords rnd lookupCount }
    match rnd.Next 3 with
    | 0 ->
        let sets = covered |> Array.map (fun _ -> ImmutableArray.CreateRange (Array.init (match rnd.Next 4 with | 0 -> 0 | 1 -> rnd.Next(1, 4) | _ -> rnd.Next(4, 8)) (fun _ -> rule (alphabet + 1) (inputCount ()) ValueNone)))
        SubstChainContext (ChainGlyphRules (coverage, ImmutableArray.CreateRange sets))
    | 1 ->
        let classDef () = ClassDefArray (0u, ImmutableArray.CreateRange (Array.init (alphabet + 1) (fun _ -> uint32 (rnd.Next(0, 4)))))
        let sets = Array.init (rnd.Next(1, 5)) (fun _ -> Array.init (match rnd.Next 4 with | 0 -> 0 | 1 -> rnd.Next(1, 4) | _ -> rnd.Next(4, 8)) (fun _ -> rule 4 (inputCount ()) (ValueSome (uint32 (rnd.Next(0, 4))))))
        let digests = Sealed.init sets.Length (fun set -> match sets[set] |> Array.exists (fun (r: ChainRule) -> r.ChainInputCount <= 1) with | true -> UInt64.MaxValue | false -> sets[set] |> Array.fold (fun mask (r: ChainRule) -> mask ||| (1UL <<< int (r.ChainInput[0] &&& 63u))) 0UL)
        SubstChainContext (ChainClassRules (coverage, classDef (), classDef (), classDef (), ImmutableArray.CreateRange (sets |> Array.map ImmutableArray.CreateRange), digests))
    | _ ->
        let coverages (count: int) = ImmutableArray.CreateRange (Array.init count (fun _ -> syntheticCoverage (Array.init (rnd.Next(1, alphabet + 1)) (fun _ -> glyph ()) |> Array.distinct |> Array.sort) false))
        let input = ImmutableArray.CreateRange (Array.append [| coverage |] (coverages (rnd.Next(0, 3)) |> Seq.toArray))
        SubstChainContext (ChainCoverageRule (coverages (rnd.Next(0, 3)), input, coverages (rnd.Next(0, 3)), syntheticRecords rnd lookupCount))

/// <summary>Builds one random subtable over the glyphs 0 to <paramref name="alphabet"/>.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The largest glyph id.</param>
/// <param name="lookupCount">The number of lookups of the table.</param>
/// <param name="kind">One for single, two for multiple, three for alternate, four for ligature, five for context, six for
/// chained context, eight for reverse chaining.</param>
let private syntheticSubtable (rnd: Random) (alphabet: int) (lookupCount: int) (kind: int) =
    let glyph () = uint32 (rnd.Next(0, alphabet + 1))
    let covered = Array.init (rnd.Next(1, alphabet + 1)) (fun _ -> glyph ()) |> Array.distinct |> Array.sort
    let coverage = syntheticCoverage covered (rnd.Next 4 = 0)
    match kind with
    | 1 ->
        match rnd.Next 2 with
        | 0 -> SingleDelta (coverage, uint32 (rnd.Next(0, alphabet + 1)), (if rnd.Next 4 = 0 then 0x7u else 0xFFFFu))
        | _ -> SingleList (coverage, ImmutableArray.CreateRange (covered |> Array.map (fun _ -> glyph ())))
    | 2 ->
        Multiple (coverage, ImmutableArray.CreateRange (covered |> Array.map (fun _ -> ImmutableArray.CreateRange (Array.init (match rnd.Next 6 with | 0 -> 0 | 1 | 2 -> 1 | 3 -> 2 | _ -> rnd.Next(2, 5)) (fun _ -> glyph ())))))
    | 3 ->
        Alternate (coverage, ImmutableArray.CreateRange (covered |> Array.map (fun _ -> ImmutableArray.CreateRange (Array.init (rnd.Next(1, 4)) (fun _ -> glyph ())))))
    | 4 ->
        let sets =
            covered
            |> Array.map (fun _ ->
                ImmutableArray.CreateRange (
                    Array.init (rnd.Next(1, 4)) (fun _ ->
                        match rnd.Next 12 with
                        | 0 -> { LigGlyph = glyph (); Components = Unchecked.defaultof<ImmutableArray<uint32>> }
                        | _ -> { LigGlyph = glyph (); Components = ImmutableArray.CreateRange (Array.init (match rnd.Next 8 with | 0 -> 0 | 1 | 2 | 3 -> 1 | 4 | 5 -> 2 | _ -> rnd.Next(1, 5)) (fun _ -> glyph ())) })))
        let second =
            sets
            |> Array.fold (fun digest set ->
                set.AsSpan().ToArray()
                |> Array.fold (fun inner lig -> if lig.Components.IsDefault || lig.Components.Length = 0 then Digest.full else Digest.add lig.Components[0] inner) digest) Digest.empty
        LigatureSubst (coverage, ImmutableArray.CreateRange sets, second)
    | 5 -> syntheticContext rnd alphabet lookupCount covered coverage
    | 6 -> syntheticChain rnd alphabet lookupCount covered coverage
    | _ ->
        let coverages (count: int) = ImmutableArray.CreateRange (Array.init count (fun _ -> syntheticCoverage (Array.init (rnd.Next(1, alphabet + 1)) (fun _ -> glyph ()) |> Array.distinct |> Array.sort) false))
        ReverseChainSingle (coverage, coverages (rnd.Next(0, 3)), coverages (rnd.Next(0, 3)), ImmutableArray.CreateRange (covered |> Array.map (fun _ -> glyph ())))

/// <summary>Builds one random lookup of one to three subtables of one kind.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="alphabet">The largest glyph id.</param>
/// <param name="lookupCount">The number of lookups of the table.</param>
/// <param name="kinds">The kinds a lookup may have.</param>
let private syntheticLookup (rnd: Random) (alphabet: int) (lookupCount: int) (kinds: int array) =
    let kind = kinds[rnd.Next kinds.Length]
    let subtables = ImmutableArray.CreateRange (Array.init (rnd.Next(1, 4)) (fun _ -> syntheticSubtable rnd alphabet lookupCount kind))
    LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest
        { LookupType = kind
          Props = syntheticLookupProps[rnd.Next syntheticLookupProps.Length]
          IsReverse = (match kind with | 8 -> true | _ -> kinds.Length = 4 && rnd.Next 24 = 0)
          Subtables = subtables }

/// <summary>The lookup kinds of the tables of the interpreter's property: single, multiple, alternate and ligature.</summary>
let private oldKinds = [| 1; 2; 3; 4 |]

/// <summary>The lookup kinds of the tables of the kernel's own property: every kind, contexts and chained contexts the
/// most often so nested lookups meet.</summary>
let private everyKind = [| 1; 2; 3; 4; 5; 5; 5; 6; 6; 6; 8 |]

/// <summary>Builds a whole synthetic case from one seed: a table of random lookups, the lookup maps and stages that apply
/// them, and a run of glyphs the lookups meet often.</summary>
/// <param name="kinds">The kinds a lookup may have.</param>
/// <param name="seed">The seed.</param>
let private syntheticCase (kinds: int array) (seed: int) =
    let rnd = Random seed
    let alphabet = rnd.Next(3, 14)
    let lookupCount = rnd.Next(1, 7)
    let lookups = Array.init lookupCount (fun _ -> syntheticLookup rnd alphabet lookupCount kinds)
    let table =
        { Version = 0x00010000u
          Scripts = ImmutableArray.Empty
          Features = ImmutableArray.Empty
          Lookups = lookups |> Array.map (fun lookup -> lazy lookup) |> ImmutableArray.CreateRange
          FeatureVariations = ReadOnlyMemory.Empty }
    let maps =
        Array.init (rnd.Next(1, 8)) (fun _ ->
            { LookupIndex = rnd.Next lookupCount
              AutoZwnj = rnd.Next 3 = 0
              AutoZwj = rnd.Next 3 = 0
              Random = rnd.Next 3 = 0
              PerSyllable = rnd.Next 4 = 0
              Mask = syntheticMasks[rnd.Next syntheticMasks.Length] ||| (if rnd.Next 3 = 0 then 0x08u else 0u)
              FeatureTag = Tag.make 'c' 'c' 'm' 'p' })
    let stages =
        let cuts = Array.init (rnd.Next(1, 4)) (fun _ -> rnd.Next(0, maps.Length + 1)) |> Array.sort
        Array.append cuts [| maps.Length |] |> Array.map (fun last -> { LastLookup = last; Pause = PauseKind.NoPause })
    let allMasks = maps |> Array.fold (fun bits map -> bits ||| map.Mask) 0u
    let length = match rnd.Next 6 with | 0 -> rnd.Next(0, 3) | 1 -> rnd.Next(20, 40) | _ -> rnd.Next(3, 20)
    let clusters = Array.scan (fun total _ -> total + uint32 (rnd.Next(0, 3))) 0u (Array.zeroCreate length) |> Array.truncate length
    let infos =
        Sealed.init length (fun index ->
            { Codepoint = uint32 (rnd.Next(0, alphabet + 1))
              Mask = (if rnd.Next 3 = 0 then uint32 (rnd.Next()) else uint32 (rnd.Next()) &&& allMasks) ||| (if rnd.Next 8 = 0 then 0u else 0x08u)
              Cluster = if rnd.Next 4 = 0 then uint32 index else clusters[index]
              Var1 = 0u
              Var2 = 0u })
    let props =
        Sealed.init length (fun _ ->
            let struct (unicode, _) = UnicodeProps.compute syntheticUnicode[rnd.Next syntheticUnicode.Length]
            { Unchecked.defaultof<GlyphProps> with
                UnicodeProps = unicode
                GlyphClass = syntheticClasses[rnd.Next syntheticClasses.Length]
                LigProps = (if rnd.Next 3 = 0 then byte (rnd.Next 256) else 0uy)
                Syllable = byte (rnd.Next 3) })
    let withPositions = rnd.Next 20 = 0
    let positions = if withPositions then Sealed.init length (fun index -> { XAdvance = index * 7; YAdvance = 0; XOffset = index; YOffset = 0; Var = 0u }) else ImmutableArray.Empty
    let run =
        { GlyphRun.empty with
            Infos = infos
            Props = (if rnd.Next 30 = 0 then ImmutableArray.Empty else props)
            Positions = positions
            HavePositions = withPositions
            ContentType = GlyphContent
            Flags = LanguagePrimitives.EnumOfValue<uint32, BufferFlags> (uint32 (if rnd.Next 2 = 0 then 0 else rnd.Next 0x100))
            ClusterLevel = [| ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes |][rnd.Next 4]
            RandomState = uint32 (rnd.Next())
            Successful = rnd.Next 30 <> 0
            Serial = uint32 (rnd.Next 256)
            ScratchFlags = LanguagePrimitives.EnumOfValue<uint32, ScratchFlags> (uint32 (rnd.Next 0x100))
            MaxLen = (match rnd.Next 8 with | 0 -> length + rnd.Next(0, 10) | 1 -> rnd.Next(0, 6) | _ -> GlyphRun.MaxLenDefault)
            MaxOps = (match rnd.Next 8 with | 0 -> rnd.Next(0, 30) | 1 -> rnd.Next(0, 300) | _ -> GlyphRun.MaxOpsDefault) }
    struct (table, ImmutableArray.CreateRange maps, ImmutableArray.CreateRange stages, run)

/// <summary>The summary of what a synthetic case did to its run.</summary>
/// <param name="run">The run before.</param>
/// <param name="result">The run after.</param>
/// <param name="admission">The look at the run.</param>
let private outcomeOf (run: GlyphRun) (result: GlyphRun) (admission: RunAdmission) =
    let ligatedCount (glyphs: GlyphRun) = glyphs.Props.AsSpan().ToArray() |> Array.filter (fun props -> props.GlyphClass &&& GlyphPropsBits.Ligated <> 0us) |> Array.length
    let ligated = ligatedCount result > ligatedCount run
    { Changed = result.Infos.AsSpan().ToArray() <> run.Infos.AsSpan().ToArray() || result.Props.AsSpan().ToArray() <> run.Props.AsSpan().ToArray()
      Ligated = ligated
      Deleted = result.Infos.Length < run.Infos.Length && not ligated
      Grew = result.Infos.Length > run.Infos.Length
      HitBudget = run.Successful && not result.Successful
      Admitted = admission.Known && run.Successful && not run.HavePositions && run.Positions.IsEmpty && run.Props.Length = run.Infos.Length }

/// <summary>Compares the kernel against the interpreter on one synthetic case over the table's old kinds, gated and
/// ungated, applicable and admitted: the interpreter applies no context, so the context subtables of a table are made
/// inert for it and for the kernel alike.</summary>
/// <param name="seed">The seed.</param>
/// <returns>What the case did.</returns>
let private checkSynthetic (seed: int) =
    let struct (full, maps, stages, run) = syntheticCase oldKinds seed
    let table = withoutContexts full
    let font = Font.create Face.empty
    expectSameRunAll "synthetic gated" (Engine.applyTable true (Engine.GsubApplier()) table maps stages font run) (Engine.applyGsub true table maps stages font run)
    let ungated = Engine.applyGsub false table maps stages font run
    expectSameRunAll "synthetic ungated" (Engine.applyTable false (Engine.GsubApplier()) table maps stages font run) ungated
    let covered = Engine.lookupSetsOf table maps
    let glyphLookups = Engine.glyphLookupsOf table maps
    let referenceCovered = Engine.applyTable true (Engine.GsubApplicableApplier(covered)) table maps stages font run
    let coveredRun = Engine.applyGsubCovered true covered glyphLookups table maps stages font run
    expectSameRunAll "synthetic applicable" referenceCovered coveredRun
    let admission = Engine.lookAt glyphLookups (run.Infos.AsSpan())
    expectSameRunAll "synthetic admitted" referenceCovered (Engine.applyGsubCoveredAdmitted covered glyphLookups table maps stages font admission run)
    outcomeOf run ungated admission

/// <summary>Compares the kernel's paths against each other on one synthetic case over every lookup kind, nested lookups
/// and reverse chaining included: the digest gates, the plan's sets and the one look at the run must not change a run
/// that no gate would have kept the lookups from changing. The runs have no positions, as no run has while GSUB applies: a
/// run that carries them, or lacks its properties, is returned as it is by the gated path when no lookup changes it and loses
/// them in the output buffer of the ungated path, which touches it.</summary>
/// <param name="seed">The seed.</param>
/// <returns>What the case did.</returns>
let private checkEveryKind (seed: int) =
    let struct (table, maps, stages, generated) = syntheticCase everyKind seed
    let run = { generated with Positions = ImmutableArray.Empty; HavePositions = false; Props = (match generated.Props.Length = generated.Infos.Length with | true -> generated.Props | false -> Sealed.init generated.Infos.Length (fun _ -> Unchecked.defaultof<GlyphProps>)) }
    let font = Font.create Face.empty
    let ungated = Engine.applyGsub false table maps stages font run
    let gated = Engine.applyGsub true table maps stages font run
    expectSameRunAll "every kind, gated and ungated" ungated gated
    let covered = Engine.lookupSetsOf table maps
    let glyphLookups = Engine.glyphLookupsOf table maps
    let coveredRun = Engine.applyGsubCovered true covered glyphLookups table maps stages font run
    expectSameRunAll "every kind, applicable" ungated coveredRun
    let admission = Engine.lookAt glyphLookups (run.Infos.AsSpan())
    expectSameRunAll "every kind, admitted" ungated (Engine.applyGsubCoveredAdmitted covered glyphLookups table maps stages font admission run)
    match run.Props.Length = run.Infos.Length with
    | true -> Expect.equal ungated.Props.Length ungated.Infos.Length "the properties still run beside the glyphs after every lookup kind"
    | false -> ()
    outcomeOf run ungated admission

/// <summary>Checks one synthetic case and, when it fails, prints the seed that replays it.</summary>
/// <param name="check">The check to run.</param>
/// <param name="seed">The seed.</param>
let private checkSyntheticReporting check (seed: int) =
    try
        check seed
    with error ->
        printfn $"FAILED synthetic seed {seed} {error.Message}"
        reraise ()

/// <summary>The bytes one call allocates, after a warm-up call.</summary>
/// <param name="action">The call.</param>
let private measureAlloc (action: unit -> GlyphRun) =
    action () |> ignore
    let before = GC.GetAllocatedBytesForCurrentThread()
    action () |> ignore
    GC.GetAllocatedBytesForCurrentThread() - before

/// <summary>The equality of the lookup kernel and the edit interpreter it replaced.</summary>
[<Tests>]
let lookupKernelTests =
    testList "lookup kernel" [
        test "the kernel equals the interpreter, and its paths agree, in every field over random runs" {
            let cases = Gen.sampleWithSeed (Rnd Seed) 30 CaseCount genCase
            let results = cases |> Seq.map checkReporting |> Seq.toArray
            let planned = results |> Seq.choose ValueOption.toOption |> Seq.toArray
            let count predicate = planned |> Array.filter predicate |> Array.length
            printfn $"lookup kernel property: {CaseCount} cases, {planned.Length} with a plan, {count (fun outcome -> outcome.Changed)} changed the run, {count (fun outcome -> outcome.Ligated)} formed a ligature, {count (fun outcome -> outcome.Deleted)} deleted a glyph, {count (fun outcome -> outcome.Grew)} grew the run, {count (fun outcome -> outcome.HitBudget)} hit a budget, {count (fun outcome -> outcome.Admitted)} used the look the caller made"
            Expect.isGreaterThan (count (fun outcome -> outcome.Admitted && outcome.Changed)) 1000 "many cases that change the run take the look the caller made"
            Expect.isGreaterThanOrEqual planned.Length 30000 "at least thirty thousand cases have a plan and are compared"
            Expect.isGreaterThan (count (fun outcome -> outcome.Changed)) 2000 "many cases change the run"
            Expect.isGreaterThan (count (fun outcome -> outcome.Ligated)) 300 "many cases form a ligature"
            Expect.isGreaterThan (count (fun outcome -> outcome.Grew)) 100 "some cases grow the run"
        }
        test "the kernel equals the interpreter over random tables of the interpreter's lookup kinds" {
            let seeds = Gen.sampleWithSeed (Rnd Seed) 30 SyntheticCaseCount (Gen.choose (0, Int32.MaxValue))
            let results = seeds |> Seq.map (checkSyntheticReporting checkSynthetic) |> Seq.toArray
            let count predicate = results |> Array.filter predicate |> Array.length
            printfn $"lookup kernel synthetic property: {results.Length} cases, {count (fun outcome -> outcome.Changed)} changed the run, {count (fun outcome -> outcome.Ligated)} formed a ligature, {count (fun outcome -> outcome.Deleted)} deleted a glyph, {count (fun outcome -> outcome.Grew)} grew the run, {count (fun outcome -> outcome.HitBudget)} hit a budget, {count (fun outcome -> outcome.Admitted)} used the look the caller made"
            Expect.isGreaterThan (count (fun outcome -> outcome.Admitted && outcome.Changed)) 3000 "many cases that change the run take the look the caller made"
            Expect.isGreaterThan (count (fun outcome -> outcome.Changed)) 6000 "many cases change the run"
            Expect.isGreaterThan (count (fun outcome -> outcome.Ligated)) 600 "many cases form a ligature"
            Expect.isGreaterThan (count (fun outcome -> outcome.Deleted)) 300 "many cases delete a glyph"
            Expect.isGreaterThan (count (fun outcome -> outcome.Grew)) 600 "many cases grow the run"
            Expect.isGreaterThan (count (fun outcome -> outcome.HitBudget)) 100 "many cases hit a budget"
        }
        test "the kernel's gated, covered and admitted paths agree over random tables of every kind, contexts and nested lookups included" {
            let seeds = Gen.sampleWithSeed (Rnd Seed) 30 SyntheticCaseCount (Gen.choose (0, Int32.MaxValue))
            let results = seeds |> Seq.map (checkSyntheticReporting checkEveryKind) |> Seq.toArray
            let count predicate = results |> Array.filter predicate |> Array.length
            printfn $"lookup kernel every-kind property: {results.Length} cases, {count (fun outcome -> outcome.Changed)} changed the run, {count (fun outcome -> outcome.Deleted)} deleted a glyph, {count (fun outcome -> outcome.Grew)} grew the run, {count (fun outcome -> outcome.HitBudget)} hit a budget, {count (fun outcome -> outcome.Admitted)} used the look the caller made"
            Expect.isGreaterThan (count (fun outcome -> outcome.Changed)) 5000 "many cases change the run"
            Expect.isGreaterThan (count (fun outcome -> outcome.Grew)) 500 "many cases grow the run"
            Expect.isGreaterThan (count (fun outcome -> outcome.Deleted)) 200 "many cases delete a glyph"
        }
    ]

/// <summary>The one-pass look at a run's glyphs.</summary>
[<Tests>]
let lookAtGlyphsTests =
    testList "look at glyphs" [
        test "the one pass gives what the covering pass and the digest pass gave, for every table and run" {
            let rnd = Random 0x51ED
            for _ in 1 .. 3000 do
                let positions = ImmutableArray.CreateRange (Array.init (rnd.Next(0, 70)) (fun _ -> ImmutableArray.CreateRange (Array.init (rnd.Next(0, 12)) (fun _ -> uint32 (rnd.Next(0, 300))))))
                let table = Sealed.glyphLookupTable positions
                let infos = Sealed.init (rnd.Next(0, 90)) (fun _ -> { Codepoint = uint32 (rnd.Next(0, 400)); Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
                let struct (bits, digest) = Sealed.lookupsAndDigest table (infos.AsSpan())
                Expect.equal bits (Sealed.lookupsCovering table (infos.AsSpan())) "the lookups that cover a glyph"
                Expect.equal digest (Digest.ofSpan (infos.AsSpan())) "the digest of the glyphs"
        }
    ]

/// <summary>The fast path of a ligature set.</summary>
[<Tests>]
let ligatureNextTests =
    testList "ligature fast path" [
        test "the next ligature the fast path tries is the first from the index with no component after the first or the probed second glyph" {
            let rnd = Random 0x1A7E
            let mutable passed = 0
            let mutable skipped = 0
            for _ in 1 .. 20000 do
                let ligatures =
                    ImmutableArray.CreateRange (
                        Array.init (rnd.Next(0, 14)) (fun _ ->
                            let components =
                                match rnd.Next(0, 8) with
                                | 0 -> Unchecked.defaultof<ImmutableArray<uint32>>
                                | 1 -> ImmutableArray<uint32>.Empty
                                | _ -> ImmutableArray.CreateRange (Array.init (rnd.Next(1, 5)) (fun _ -> uint32 (rnd.Next(1, 5))))
                            { LigGlyph = uint32 (rnd.Next(1, 500)); Components = components }))
                let second = uint32 (rnd.Next(1, 5))
                let first = rnd.Next(0, ligatures.Length + 2)
                let admitted (lig: Ligature) = GsubTables.ligCount lig <= 1 || lig.Components[0] = second
                let expected =
                    seq { first .. ligatures.Length - 1 }
                    |> Seq.tryFind (fun index -> admitted ligatures[index])
                    |> Option.defaultValue (max first ligatures.Length)
                Expect.equal (Engine.ligatureNext ligatures first second) expected "the first admitted ligature, or the count when none is"
                match expected > first with
                | true -> skipped <- skipped + 1
                | false -> passed <- passed + 1
            Expect.isGreaterThan skipped 2000 "many cases pass over a ligature"
            Expect.isGreaterThan passed 2000 "many cases stop at the first"
        }
    ]

/// <summary>Builds a GSUB table of one lookup over some subtables.</summary>
/// <param name="lookupType">The lookup type.</param>
/// <param name="subtables">The subtables.</param>
let private oneLookupTable (lookupType: int) (subtables: SubstSubtable list) =
    let lookup =
        LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest
            { LookupType = lookupType
              Props = 0u
              IsReverse = false
              Subtables = ImmutableArray.CreateRange subtables }
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = ImmutableArray.Create (lazy lookup)
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>The compiled lookups of a map that applies the first <paramref name="count"/> lookups of a table, each under mask 1.</summary>
/// <param name="count">The number of lookups.</param>
let private mapOf (count: int) =
    Sealed.init count (fun index ->
        { LookupIndex = index
          AutoZwnj = false
          AutoZwj = false
          Random = false
          PerSyllable = false
          Mask = 1u
          FeatureTag = Tag.make 'c' 'c' 'm' 'p' })

/// <summary>A run of glyphs under mask 1, one property record per glyph, the run the substitution receives.</summary>
/// <param name="count">The number of glyphs.</param>
/// <param name="glyphOf">The glyph at an index.</param>
let private glyphRun (count: int) (glyphOf: int -> uint32) =
    let infos = Sealed.init count (fun index -> { Codepoint = glyphOf index; Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with Infos = infos; Props = props; ContentType = GlyphContent }

/// <summary>The bound, in bytes per glyph, on what a stage that changes the run without changing its length allocates: one
/// published copy of the infos (20 bytes) and the properties (12 bytes), which is also the working copy.</summary>
[<Literal>]
let private PublishedCopyBytesPerGlyph = 32L

/// <summary>The bytes of the objects around the arrays that a changing stage allocates besides them.</summary>
[<Literal>]
let private ChangedRunOverhead = 1024L

/// <summary>Builds the application of a table through the kernel with the plan's sets, as the shaping entry applies it: the
/// sets are built once, here.</summary>
/// <param name="table">The table.</param>
/// <param name="count">The number of lookups.</param>
let private kernelFor (table: LayoutTable<SubstSubtable>) (count: int) =
    let maps = mapOf count
    let stages = ImmutableArray.Create { LastLookup = count; Pause = PauseKind.NoPause }
    let covered = Engine.lookupSetsOf table maps
    let glyphLookups = Engine.glyphLookupsOf table maps
    let font = Font.create Face.empty
    fun (run: GlyphRun) -> Engine.applyGsubCovered true covered glyphLookups table maps stages font run

/// <summary>The allocation bounds of the kernel.</summary>
[<Tests>]
let lookupKernelAllocationTests =
    testList "lookup kernel allocation" [
        test "a lookup that matches nothing allocates nothing, with the digest gate, without it and with the plan's sets" {
            let covers = CoverageGlyphs (ImmutableArray.Create 1000u)
            let single = oneLookupTable 1 [ SingleDelta (covers, 1u, 0xFFFFu) ]
            let ligature = oneLookupTable 4 [ LigatureSubst (covers, ImmutableArray.Create (ImmutableArray.Create { LigGlyph = 7u; Components = ImmutableArray.Create 1u }), Digest.add 1u Digest.empty) ]
            let run = glyphRun 1000 (fun index -> uint32 (index % 128))
            let font = Font.create Face.empty
            for table in [ single; ligature ] do
                let maps = mapOf 1
                let stages = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
                Expect.equal (measureAlloc (fun () -> Engine.applyGsub true table maps stages font run)) 0L "gated"
                Expect.equal (measureAlloc (fun () -> Engine.applyGsub false table maps stages font run)) 0L "ungated"
                let withSets = kernelFor table 1
                Expect.equal (measureAlloc (fun () -> withSets run)) 0L "with the plan's sets, once the sets are built"
        }

        test "a context, class, coverage and chained-context lookup that covers glyphs and matches nothing allocates nothing" {
            let covers = CoverageGlyphs (ImmutableArray.Create 1u)
            let never = CoverageGlyphs (ImmutableArray.Create 999u)
            let records = ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 }
            let rule = { InputCount = 2; Input = ImmutableArray.Create 999u; Records = records }
            let manyRules = ImmutableArray.CreateRange (List.replicate 6 rule)
            let chainRule = { Backtrack = ImmutableArray.Create 998u; ChainInputCount = 2; ChainInput = ImmutableArray.Create 999u; Lookahead = ImmutableArray.Empty; ChainRecords = records }
            let classDef = ClassDefArray (0u, ImmutableArray.CreateRange (Array.init 128 (fun glyph -> if glyph = 1 then 1u else 0u)))
            let tables =
                [ oneLookupTable 5 [ SubstContext (GlyphRules (covers, ImmutableArray.Create (ImmutableArray.Create rule))) ]
                  oneLookupTable 5 [ SubstContext (GlyphRules (covers, ImmutableArray.Create manyRules)) ]
                  oneLookupTable 5 [ SubstContext (ClassRules (covers, classDef, ImmutableArray.Create (ImmutableArray.Empty, manyRules), Sealed.init 2 (fun _ -> UInt64.MaxValue))) ]
                  oneLookupTable 5 [ SubstContext (CoverageRule (ImmutableArray.Create (covers, never), records)) ]
                  oneLookupTable 6 [ SubstChainContext (ChainGlyphRules (covers, ImmutableArray.Create (ImmutableArray.Create chainRule))) ]
                  oneLookupTable 6 [ SubstChainContext (ChainCoverageRule (ImmutableArray.Create never, ImmutableArray.Create (covers, never), ImmutableArray.Empty, records)) ] ]
            let run = glyphRun 1000 (fun index -> uint32 (index % 128))
            let font = Font.create Face.empty
            for table in tables do
                let maps = mapOf 1
                let stages = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
                Expect.equal (measureAlloc (fun () -> Engine.applyGsub true table maps stages font run)) 0L "gated"
                Expect.equal (measureAlloc (fun () -> Engine.applyGsub false table maps stages font run)) 0L "ungated"
                let withSets = kernelFor table 1
                Expect.equal (measureAlloc (fun () -> withSets run)) 0L "with the plan's sets, once the sets are built"
                Expect.isTrue (obj.ReferenceEquals (withSets run, run)) "the run it was given"
        }

        test "a context lookup that matches at every third glyph allocates the working copy and one published copy, not an array per match" {
            let count = 3000
            let covers = CoverageGlyphs (ImmutableArray.Create 0u)
            let single = SingleDelta (CoverageGlyphs (ImmutableArray.Create 1u), 4u, 0xFFFFu)
            let context = SubstContext (CoverageRule (ImmutableArray.Create (covers, CoverageGlyphs (ImmutableArray.Create 1u)), ImmutableArray.Create { SequenceIndex = 1; LookupListIndex = 0 }))
            let lookup (lookupType: int) (subtable: SubstSubtable) =
                lazy (LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest { LookupType = lookupType; Props = 0u; IsReverse = false; Subtables = ImmutableArray.Create subtable })
            let table = { (oneLookupTable 1 [ single ]) with Lookups = ImmutableArray.Create (lookup 1 single, lookup 5 context) }
            let maps = ImmutableArray.Create { (mapOf 1)[0] with LookupIndex = 1 }
            let stages = ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
            let covered = Engine.lookupSetsOf table maps
            let glyphLookups = Engine.glyphLookupsOf table maps
            let font = Font.create Face.empty
            let run = glyphRun count (fun index -> uint32 (index % 3))
            let apply () = Engine.applyGsubCovered true covered glyphLookups table maps stages font run
            let result = apply ()
            Expect.equal result.Infos[1].Codepoint 5u "the nested single substitution applied at the first match"
            let bytes = measureAlloc apply
            Expect.isLessThanOrEqual bytes (int64 count * 2L * PublishedCopyBytesPerGlyph + ChangedRunOverhead) $"{count / 3} matches allocated {bytes} B: the working copy and the published copy of {count} glyphs, nothing for the matches themselves"
        }

        test "a lookup that matches nothing returns the run it was given" {
            let table = oneLookupTable 1 [ SingleDelta (CoverageGlyphs (ImmutableArray.Create 1000u), 1u, 0xFFFFu) ]
            let run = glyphRun 100 (fun index -> uint32 index)
            Expect.isTrue (obj.ReferenceEquals (kernelFor table 1 run, run)) "same instance"
        }

        test "a stage that replaces glyphs allocates one published copy, however many lookups it holds" {
            let count = 1000
            let covers = CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u })
            let run = glyphRun count (fun index -> uint32 (index % 128))
            let one = oneLookupTable 1 [ SingleDelta (covers, 1u, 0xFFFFu) ]
            let applyOne = kernelFor one 1
            let bytes = measureAlloc (fun () -> applyOne run)
            Expect.isLessThanOrEqual bytes (int64 count * PublishedCopyBytesPerGlyph + ChangedRunOverhead) $"one lookup allocated {bytes} B for {count} glyphs: one published copy, {PublishedCopyBytesPerGlyph} B per glyph"
            let tenLookups =
                { one with Lookups = ImmutableArray.CreateRange (Array.init 10 (fun _ -> one.Lookups[0])) }
            let applyTen = kernelFor tenLookups 10
            let tenBytes = measureAlloc (fun () -> applyTen run)
            Expect.isLessThanOrEqual tenBytes (int64 count * PublishedCopyBytesPerGlyph + ChangedRunOverhead) $"ten lookups allocated {tenBytes} B for {count} glyphs: the same one copy"
        }

        test "a stage that forms ligatures allocates a working copy of the input and a published copy of the output" {
            let count = 1000
            let lig = { LigGlyph = 9u; Components = ImmutableArray.Create 1u }
            let table = oneLookupTable 4 [ LigatureSubst (CoverageGlyphs (ImmutableArray.Create 0u), ImmutableArray.Create (ImmutableArray.Create lig), Digest.add 1u Digest.empty) ]
            let run = glyphRun count (fun index -> uint32 (index % 2))
            let applyLigature = kernelFor table 1
            let bytes = measureAlloc (fun () -> applyLigature run)
            Expect.isLessThanOrEqual bytes (int64 count * 2L * PublishedCopyBytesPerGlyph + ChangedRunOverhead) $"forming {count / 2} ligatures allocated {bytes} B: a working copy of {count} glyphs and a published copy of {count / 2}"
        }

        test "a run of two hundred thousand glyphs is substituted without running out of stack" {
            let count = 200_000
            let covers = CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u })
            let table = oneLookupTable 1 [ SingleDelta (covers, 1u, 0xFFFFu) ]
            let result = kernelFor table 1 (glyphRun count (fun index -> uint32 (index % 128)))
            Expect.equal result.Infos.Length count "every glyph survives"
            Expect.equal result.Infos[5].Codepoint 6u "and is replaced"
        }
    ]
