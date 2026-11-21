module Farfbuzz.Bench.RunPassCost

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.OpenType
open Farfbuzz.Harness
open Farfbuzz.Tests.AnchorTables

/// <summary>The pair provider over the compiled index of a kerx format 0 subtable.</summary>
/// <param name="subtable">The subtable, whose pairs compile.</param>
/// <returns>The provider.</returns>
let private indexedPairsOf (subtable: KerxSubtable) = Kern.IndexedPairProvider(ValueOption.get subtable.Pairs.Value)

/// <summary>A pass whose every step advances one glyph and changes nothing.</summary>
[<Struct>]
type NextGlyphPass =
    interface IRunPass<unit> with
        member _.Step (cursor, _) =
            match cursor.Index < cursor.Input.Length with
            | true -> struct (RunEdit.NextGlyph, ())
            | false -> struct (RunEdit.Finish, ())

/// <summary>A pass that sets one mask bit on every glyph, one glyph per step.</summary>
[<Struct>]
type SetMaskPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state < cursor.Input.Length with
            | true -> struct (RunEdit.OrMask (1u, state, state + 1), state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>A pass that sets the position of every glyph, one glyph per step.</summary>
[<Struct>]
type SetPositionPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state < cursor.Input.Length with
            | true ->
                struct (RunEdit.SetPosition (state, { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }), state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>A pass that outputs every glyph through <c>RunEdit.OutputInfo</c>, one glyph per step.</summary>
[<Struct>]
type OutputInfoPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state < cursor.Input.Length with
            | true ->
                struct (RunEdit.OutputInfo (cursor.Input[state], Unchecked.defaultof<GlyphProps>), state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>A pass that skips every glyph, one <c>SkipGlyph</c> edit per glyph.</summary>
[<Struct>]
type SkipGlyphPass =
    interface IRunPass<unit> with
        member _.Step (cursor, _) =
            match cursor.Index < cursor.Input.Length with
            | true -> struct (RunEdit.SkipGlyph, ())
            | false -> struct (RunEdit.Finish, ())

/// <summary>A pass that replays the post-match hot path: <c>OutputInfo</c> then <c>SkipGlyph</c> per glyph.</summary>
[<Struct>]
type OutputSkipPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state / 2 < cursor.Input.Length with
            | true ->
                match state % 2 with
                | 0 -> struct (RunEdit.OutputInfo (cursor.Input[state / 2], Unchecked.defaultof<GlyphProps>), state + 1)
                | _ -> struct (RunEdit.SkipGlyph, state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>A pass that replaces every glyph in place with one <c>ReplaceGlyphProps</c> edit per glyph.</summary>
[<Struct>]
type ReplaceGlyphPropsPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state < cursor.Input.Length with
            | true -> struct (RunEdit.ReplaceGlyphProps (cursor.Input[state].Codepoint + 1u, Unchecked.defaultof<GlyphProps>), state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>Builds a single-substitution lookup accelerator over a coverage and delta, <c>SingleSubstFormat1</c>.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="delta">The delta.</param>
/// <returns>The lookup accelerator.</returns>
let private singleLookup (coverage: Coverage) (delta: uint32) : LookupAccel<SubstSubtable> =
    let subtable = SingleDelta (coverage, delta, 0xFFFFu)
    let digest = Coverage.digest coverage
    let accel = { Subtable = subtable; SubtableDigest = digest }
    { LookupType = 1
      Props = 0u
      IsReverse = false
      Subtables = ImmutableArray.Create accel
      LookupDigest = digest
      SecondDigest = Digest.full }

/// <summary>Builds a synthetic GSUB table of one single-substitution lookup.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="delta">The delta.</param>
/// <returns>The table.</returns>
let private singleTable (coverage: Coverage) (delta: uint32) : LayoutTable<SubstSubtable> =
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = ImmutableArray.Create (lazy (singleLookup coverage delta))
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>Builds a synthetic GSUB table of one single-substitution lookup per coverage.</summary>
/// <param name="coverages">The coverages.</param>
/// <param name="delta">The delta.</param>
/// <returns>The table.</returns>
let private multiTable (coverages: Coverage array) (delta: uint32) : LayoutTable<SubstSubtable> =
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = coverages |> Array.map (fun coverage -> lazy (singleLookup coverage delta)) |> ImmutableArray.CreateRange
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>Builds a synthetic GSUB table of one ligature lookup over a coverage.</summary>
/// <param name="coverage">The coverage.</param>
/// <param name="ligGlyph">The ligature glyph.</param>
/// <param name="componentGlyph">The component glyph.</param>
/// <returns>The table.</returns>
let private ligatureTable (coverage: Coverage) (ligGlyph: uint32) (componentGlyph: uint32) =
    let lig = { LigGlyph = ligGlyph; Components = ImmutableArray.Create componentGlyph }
    let second = Digest.add componentGlyph Digest.empty
    let subtable = LigatureSubst (coverage, ImmutableArray.Create (ImmutableArray.Create lig), second)
    let accel = { Subtable = subtable; SubtableDigest = Coverage.digest coverage }
    let lookup =
        { LookupType = 4
          Props = 0u
          IsReverse = false
          Subtables = ImmutableArray.Create accel
          LookupDigest = Coverage.digest coverage
          SecondDigest = second }
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = ImmutableArray.Create (lazy lookup)
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>Builds a synthetic GSUB table of a single-substitution lookup that adds four to glyph one, and a lookup that
/// holds one context subtable, the lookup the benchmarks apply.</summary>
/// <param name="subtable">The context subtable.</param>
/// <returns>The table.</returns>
let private contextTable (subtable: SubstSubtable) =
    let built lookupType subtables =
        LayoutAccel.build GsubTables.subtableDigest GsubTables.secondDigest
            { LookupType = lookupType
              Props = 0u
              IsReverse = false
              Subtables = ImmutableArray.CreateRange (subtables: SubstSubtable list) }
    let single = built 1 [ SingleDelta (CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| 1u |])), 4u, 0xFFFFu) ]
    let context = built 5 [ subtable ]
    { Version = 0x00010000u
      Scripts = ImmutableArray.Empty
      Features = ImmutableArray.Empty
      Lookups = ImmutableArray.Create (lazy single, lazy context)
      FeatureVariations = ReadOnlyMemory.Empty }

/// <summary>A run of glyphs under mask one that repeat with a period.</summary>
/// <param name="period">The period.</param>
/// <returns>The run.</returns>
let private periodicRun period =
    let infos = Sealed.init 10_000 (fun index -> { Codepoint = uint32 (index % period); Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init 10_000 (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with Infos = infos; Props = props }

/// <summary>A lookup map with a non-zero mask, <c>hb_ot_map_t::lookup_map_t</c>.</summary>
/// <param name="index">The lookup index.</param>
/// <returns>The lookup map.</returns>
let private lookupMapOf (index: int) : LookupMap =
    { LookupIndex = index
      AutoZwnj = false
      AutoZwj = false
      Random = false
      PerSyllable = false
      Mask = 1u
      FeatureTag = Tag.make 'c' 'c' 'm' 'p' }

/// <summary>The lookup maps of a stage, one per lookup.</summary>
/// <param name="count">The lookup count.</param>
/// <returns>The lookup maps.</returns>
let private lookupMapsOf (count: int) : ImmutableArray<LookupMap> =
    Sealed.init count (fun index -> lookupMapOf index)

/// <summary>Builds a run of glyphs with a non-zero mask, for the substitution benchmarks.</summary>
/// <param name="count">The glyph count.</param>
/// <returns>The run.</returns>
let private maskedRun (count: int) : GlyphRun =
    let infos = Sealed.init count (fun index -> { Codepoint = uint32 (index % 128); Mask = 1u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with Infos = infos; Props = props }

/// <summary>An in-place AAT kind whose transition never emits an edit and whose entries are never actionable.</summary>
[<Struct>]
type AatNoopKind =
    interface IStateMachineKind<int> with
        member _.InPlace = true
        member _.IsActionable _ = false
        member _.IsActionInitiable _ = false
        member _.Transition (_, _, context) = struct (Emission.Complete, context)
        member _.NullTransition (_, _, context) = context

/// <summary>Builds a state machine whose class table maps every glyph to one class, with one entry per state-and-class
/// pair in row-major order.</summary>
/// <param name="nClasses">The class count.</param>
/// <param name="classValue">The class every glyph below <paramref name="glyphCount"/> maps to.</param>
/// <param name="glyphCount">The number of glyphs the class table covers.</param>
/// <param name="entries">The entries, <c>nStates * nClasses</c> of them.</param>
/// <returns>The machine.</returns>
let private uniformMachine (nClasses: int) (classValue: byte) (glyphCount: int) (entries: Entry array) =
    let classBytes = Array.init (4 + glyphCount) (fun i -> if i < 4 then 0uy else classValue)
    { ClassCount = nClasses
      Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = glyphCount; Wide = false }
      States = Sealed.init entries.Length (fun i -> uint16 i)
      StateBias = 0
      RowCount = entries.Length / nClasses
      Entries = Sealed.init entries.Length (fun i -> entries[i])
      IsExtended = false }

/// <summary>The driver config and seed for a synthetic machine.</summary>
type private DriverRun =
    { Config: DriverConfig
      Seed: DriverState<int> }

/// <summary>The steps and edits a counted pass has taken, by edit kind.</summary>
type EditTally() =
    static member val Steps = 0L with get, set
    static member val Kinds: int64 array = Array.zeroCreate 64 with get
    static member Reset() =
        EditTally.Steps <- 0L
        Array.Clear(EditTally.Kinds)

/// <summary>A pass that counts the steps and the edit kinds of the pass it wraps.</summary>
[<Struct>]
type internal CountingPass<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> =
    { Inner: 'P }
    interface IRunPass<'S> with
        member this.Step (cursor, state) =
            let struct (edit, next) = this.Inner.Step(&cursor, state)
            EditTally.Steps <- EditTally.Steps + 1L
            EditTally.Kinds[int edit.Kind] <- EditTally.Kinds[int edit.Kind] + 1L
            struct (edit, next)

/// <summary>A pass that applies only the edits that move the cursor or charge the budget, dropping every other edit.</summary>
[<Struct>]
type internal DroppingPass<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> =
    { Inner: 'P }
    interface IRunPass<'S> with
        member this.Step (cursor, state) =
            let struct (edit, next) = this.Inner.Step(&cursor, state)
            match edit.Kind with
            | EditKind.NextGlyph | EditKind.NextGlyphs | EditKind.Finish | EditKind.ChargeOps | EditKind.ClearOutput -> struct (edit, next)
            | _ -> struct (RunEdit.NextGlyphs 0, next)

/// <summary>The cost of one shaping pass over a 10,000-glyph run: the out-buffer machine against a plain array map,
/// so the per-step overhead of <c>Sealed.runPass</c> is visible in nanoseconds per glyph.</summary>
[<MemoryDiagnoser>]
type RunPassCost() =
    let mutable run: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable substFont: Font = Unchecked.defaultof<Font>
    let mutable substRun: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable stageRun: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable nonMatchingTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable replaceAllTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable replaceOneTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable nonMatching10Table: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable ligatureNonMatchingTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable ligatureEverySecondTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable ligatureEverySecondRun: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable nonMatchingSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable replaceAllSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable replaceOneSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable nonMatching10Sets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable ligatureNonMatchingSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable ligatureEverySecondSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable contextGatedTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable contextCoveredTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable contextEveryThirdTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable chainCoveredTable: LayoutTable<SubstSubtable> = Unchecked.defaultof<LayoutTable<SubstSubtable>>
    let mutable contextTwoRun: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable contextThreeRun: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable contextGatedSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable contextCoveredSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable contextEveryThirdSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable chainCoveredSets: struct (ImmutableArray<ImmutableArray<uint64>> * ImmutableArray<uint64>) = Unchecked.defaultof<_>
    let mutable contextMaps: ImmutableArray<LookupMap> = Unchecked.defaultof<ImmutableArray<LookupMap>>
    let mutable singleLookupMaps: ImmutableArray<LookupMap> = Unchecked.defaultof<ImmutableArray<LookupMap>>
    let mutable tenLookupMaps: ImmutableArray<LookupMap> = Unchecked.defaultof<ImmutableArray<LookupMap>>
    let mutable singleStage: ImmutableArray<StageMap> = Unchecked.defaultof<ImmutableArray<StageMap>>
    let mutable tenStage: ImmutableArray<StageMap> = Unchecked.defaultof<ImmutableArray<StageMap>>

    let mutable nullTransition: DriverRun = Unchecked.defaultof<DriverRun>
    let mutable otherState: DriverRun = Unchecked.defaultof<DriverRun>
    let mutable alternating: DriverRun = Unchecked.defaultof<DriverRun>
    let mutable contextualMatchNothing: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable contextualChangeAll: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable contextualChangeAllBody: byte array = Unchecked.defaultof<byte array>
    let mutable rearrangementMatchNothing: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable rearrangementChangeAll: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable ligatureMatchNothing: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable ligatureChangeAll: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable ligatureKind: LigatureKind = Unchecked.defaultof<LigatureKind>
    let mutable insertionMatchNothing: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable insertionChangeAll: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable insertionKind: InsertionKind = Unchecked.defaultof<InsertionKind>
    let mutable matchNothing: Lookup = Unchecked.defaultof<Lookup>
    let mutable changeAll: Lookup = Unchecked.defaultof<Lookup>
    let mutable emptyContext: AatContext = Unchecked.defaultof<AatContext>
    let mutable kerxFont: Font = Unchecked.defaultof<Font>
    let mutable kerxPlan: OtShapePlan = Unchecked.defaultof<OtShapePlan>
    let mutable kerxMatchNothingRun: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable kerxChangeAllRun: GlyphRun = Unchecked.defaultof<GlyphRun>
    let mutable kerxFormat0: KerxSubtable = Unchecked.defaultof<KerxSubtable>
    let mutable kerxFormat2: KerxSubtable = Unchecked.defaultof<KerxSubtable>
    let mutable kerxFormat6: KerxSubtable = Unchecked.defaultof<KerxSubtable>
    let mutable kerxFormat1MatchNothing: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable kerxFormat1ChangeAll: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable kerxFormat1ChangeAllBody: byte array = Unchecked.defaultof<byte array>
    let mutable kernFormat1MatchNothing: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable kernFormat1ChangeAll: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable kernFormat1ChangeAllBody: byte array = Unchecked.defaultof<byte array>
    let mutable kerxFormat4MatchNothing: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable kerxFormat4AttachAll: DriverConfig = Unchecked.defaultof<DriverConfig>
    let mutable kerxFormat4AttachAllBody: byte array = Unchecked.defaultof<byte array>
    let mutable kerxFormat4AnchorFont: Font = Unchecked.defaultof<Font>

    /// <summary>Builds the 10,000-glyph run and reports the size of the machine's value types.</summary>
    [<GlobalSetup>]
    member _.Setup() =
        let infos =
            Sealed.init 10_000 (fun index ->
                { Codepoint = uint32 (index % 128); Mask = 0u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
        let props = Sealed.init 10_000 (fun _ -> Unchecked.defaultof<GlyphProps>)
        let positions = Sealed.init 10_000 (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
        run <- { GlyphRun.empty with Infos = infos; Props = props; Positions = positions; HavePositions = true }
        substFont <- Font.create Face.empty
        substRun <- maskedRun 10_000
        stageRun <- maskedRun 50
        nonMatchingTable <- singleTable (CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| 0xFFFFu |]))) 1u
        replaceAllTable <- singleTable (CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u })) 1u
        replaceOneTable <- multiTable (Array.init 10 (fun index -> CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| uint32 index |])))) 1u
        nonMatching10Table <- multiTable (Array.init 10 (fun index -> CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| 1000u + uint32 index |])))) 1u
        ligatureNonMatchingTable <- ligatureTable (CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| 0xFFFFu |]))) 100u 1u
        ligatureEverySecondTable <- ligatureTable (CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| 0u |]))) 100u 1u
        let ligInfos = Sealed.init 10_000 (fun i -> { Codepoint = uint32 (i % 2); Mask = 1u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
        let ligProps = Sealed.init 10_000 (fun _ -> Unchecked.defaultof<GlyphProps>)
        ligatureEverySecondRun <- { GlyphRun.empty with Infos = ligInfos; Props = ligProps }
        singleLookupMaps <- lookupMapsOf 1
        contextMaps <- Sealed.init 1 (fun _ -> lookupMapOf 1)
        let coverageOf (glyph: uint32) = CoverageGlyphs (Sealed.ofSpan (ReadOnlySpan [| glyph |]))
        let neverMatching = ImmutableArray.Create { InputCount = 2; Input = ImmutableArray.Create 99u; Records = ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 } }
        contextGatedTable <- contextTable (SubstContext (GlyphRules (coverageOf 0xFFFFu, ImmutableArray.Create neverMatching)))
        contextCoveredTable <- contextTable (SubstContext (GlyphRules (coverageOf 0u, ImmutableArray.Create neverMatching)))
        contextEveryThirdTable <- contextTable (SubstContext (CoverageRule (ImmutableArray.Create (coverageOf 0u, coverageOf 1u), ImmutableArray.Create { SequenceIndex = 1; LookupListIndex = 0 })))
        chainCoveredTable <- contextTable (SubstChainContext (ChainCoverageRule (ImmutableArray.Empty, ImmutableArray.Create (coverageOf 0u, coverageOf 99u), ImmutableArray.Empty, ImmutableArray.Create { SequenceIndex = 0; LookupListIndex = 0 })))
        contextTwoRun <- periodicRun 2
        contextThreeRun <- periodicRun 3
        let contextSetsOf table = struct (Engine.lookupSetsOf table contextMaps, Engine.glyphLookupsOf table contextMaps)
        contextGatedSets <- contextSetsOf contextGatedTable
        contextCoveredSets <- contextSetsOf contextCoveredTable
        contextEveryThirdSets <- contextSetsOf contextEveryThirdTable
        chainCoveredSets <- contextSetsOf chainCoveredTable
        tenLookupMaps <- lookupMapsOf 10
        singleStage <- ImmutableArray.Create { LastLookup = 1; Pause = PauseKind.NoPause }
        tenStage <- ImmutableArray.Create { LastLookup = 10; Pause = PauseKind.NoPause }
        let setsOf (table: LayoutTable<SubstSubtable>) (maps: ImmutableArray<LookupMap>) = struct (Engine.lookupSetsOf table maps, Engine.glyphLookupsOf table maps)
        nonMatchingSets <- setsOf nonMatchingTable singleLookupMaps
        replaceAllSets <- setsOf replaceAllTable singleLookupMaps
        replaceOneSets <- setsOf replaceOneTable tenLookupMaps
        nonMatching10Sets <- setsOf nonMatching10Table tenLookupMaps
        ligatureNonMatchingSets <- setsOf ligatureNonMatchingTable singleLookupMaps
        ligatureEverySecondSets <- setsOf ligatureEverySecondTable singleLookupMaps

        let nullEntries = Array.init 5 (fun _ -> { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us })
        let nullMachine = uniformMachine 5 4uy 128 nullEntries
        nullTransition <-
            { Config = { Machine = nullMachine; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
              Seed = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = 0 } }
        let otherEntries = Array.init 10 (fun _ -> { NewState = 1; Flags = 0us; Data0 = 0us; Data1 = 0us })
        let otherMachine = uniformMachine 5 4uy 128 otherEntries
        otherState <-
            { Config = { Machine = otherMachine; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
              Seed = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = 0 } }
        let alternatingClassBytes = Array.init (4 + 128) (fun i -> if i < 4 then 0uy else byte (2 + (i - 4) % 2))
        let alternatingEntries =
            [| { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 1; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 1; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 2; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 2; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 1; Flags = 0us; Data0 = 0us; Data1 = 0us }
               { NewState = 1; Flags = 0us; Data0 = 0us; Data1 = 0us } |]
        let alternatingMachine =
            { ClassCount = 4
              Classes = Narrow { ClassTable.Table = ReadOnlyMemory alternatingClassBytes; At = 0; FirstGlyph = 0us; Count = 128; Wide = false }
              States = Sealed.init 12 (fun i -> uint16 i)
              StateBias = 0
              RowCount = 3
              Entries = Sealed.init 12 (fun i -> alternatingEntries[i])
              IsExtended = false }
        alternating <-
            { Config = { Machine = alternatingMachine; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
              Seed = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = 0 } }
        let be16 (value: uint32) = [| byte (value >>> 8); byte value |]
        let be32 (value: uint32) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]
        let single = Array.concat [ be16 6u; be16 4u; be16 2u; be16 0u; be16 0u; be16 0u; be16 1000u; be16 2000u; be16 0xFFFFu; be16 0u ]
        matchNothing <- AatLookup.read (ReadOnlyMemory single) 0 2
        let format0 = Array.concat [ be16 0u; Array.init 128 (fun index -> be16 (uint32 ((index + 1) % 128))) |> Array.concat ]
        changeAll <- AatLookup.read (ReadOnlyMemory format0) 0 2
        emptyContext <- { Gdef = Gdef.load Face.empty }
        let nullEntry = { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }
        let contextualEntries nClasses actionable =
            Array.init nClasses (fun i ->
                match i = 4 with
                | true -> if actionable then { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 1us } else nullEntry
                | false -> nullEntry)
        contextualMatchNothing <- { Machine = uniformMachine 5 4uy 128 (contextualEntries 5 false); Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        contextualChangeAll <- { Machine = uniformMachine 5 4uy 128 (contextualEntries 5 true); Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        contextualChangeAllBody <- Array.concat [ be16 0u; Array.init 128 (fun i -> be16 (uint32 (i + 1))) |> Array.concat ]
        let rearrangementEntries nClasses actionable =
            Array.init nClasses (fun i ->
                match i = 4 with
                | true -> if actionable then { NewState = 0; Flags = 0x0001us; Data0 = 0us; Data1 = 0us } else nullEntry
                | false -> nullEntry)
        rearrangementMatchNothing <- { Machine = uniformMachine 5 4uy 128 (rearrangementEntries 5 false); Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        rearrangementChangeAll <- { Machine = uniformMachine 5 4uy 128 (rearrangementEntries 5 true); Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        let ligatureNullEntries = Array.init 5 (fun _ -> nullEntry)
        ligatureMatchNothing <- { Machine = uniformMachine 5 4uy 128 ligatureNullEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        let ligatureActionEntries = Array.init 5 (fun i -> if i = 4 then { NewState = 0; Flags = 0xA000us; Data0 = 0us; Data1 = 0us } else nullEntry)
        ligatureChangeAll <- { Machine = uniformMachine 5 4uy 128 ligatureActionEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        let ligatureBody = Array.concat [ Array.zeroCreate 16; be32 28u; be32 32u; be32 288u; be32 0xC0000000u; Array.zeroCreate 256; be16 100u ]
        ligatureKind <- { Extended = true; Body = ReadOnlyMemory ligatureBody; LigActionAt = 28L; ComponentAt = 32L; LigatureAt = 288L; Context = emptyContext }
        let insertionNullEntry = { NewState = 0; Flags = 0us; Data0 = 0xFFFFus; Data1 = 0xFFFFus }
        let insertionNullEntries = Array.init 5 (fun _ -> insertionNullEntry)
        insertionMatchNothing <- { Machine = uniformMachine 5 4uy 128 insertionNullEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        let insertionActionEntries =
            Array.init 5 (fun i -> if i = 4 then { NewState = 0; Flags = 0x0820us; Data0 = 0us; Data1 = 0xFFFFus } else insertionNullEntry)
        insertionChangeAll <- { Machine = uniformMachine 5 4uy 128 insertionActionEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        let insertionBody = Array.concat [ be16 10u ]
        insertionKind <- { Body = ReadOnlyMemory insertionBody; InsertionActionAt = 0L; Context = emptyContext }

        let beI16 (value: int16) = [| byte (uint16 value >>> 8); byte value |]
        kerxFont <- Font.create Face.empty
        kerxPlan <-
            { Props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
              Shaper = Categorize.profile DefaultShaper
              Map = OtMap.empty
              Data = NoShaperData
              UserFeatures = ImmutableArray.Empty
              FracMask = 0u
              NumrMask = 0u
              DnomMask = 0u
              RtlmMask = 0u
              KernMask = 0x80000000u
              RequestedKerning = true
              HasFrac = false
              HasVert = false
              HasGposMark = false
              ZeroMarks = false
              FallbackGlyphClasses = true
              FallbackMarkPositioning = false
              AdjustMarkPositioningWhenZeroing = false
              ApplyGpos = false
              ApplyKern = false
              ApplyFallbackKern = false
              ApplyKerx = true
              ApplyMorx = false
              ApplyTrak = false; Unicode = ValueNone }
        let mkKerxRun (glyph: uint32) =
            let infos = Sealed.init 10_000 (fun i -> { Codepoint = glyph; Mask = 0x80000000u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 10_000 (fun _ -> Unchecked.defaultof<GlyphProps>)
            let positions = Sealed.init 10_000 (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
            { GlyphRun.empty with
                Infos = infos
                Props = props
                Positions = positions
                HavePositions = true
                SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }
        kerxMatchNothingRun <- mkKerxRun 2u
        kerxChangeAllRun <- mkKerxRun 1u
        let kerxTableOf (subtable: byte array) =
            let kerxBytes = Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]
            let file = Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u; be32 (uint32 Tags.Kerx); be32 0u; be32 28u; be32 (uint32 kerxBytes.Length); kerxBytes ]
            (Kerx.load (Face.withGlyphCount 16 (Face.create (ReadOnlyMemory file) 0u))).Subtables[0]
        let format0Bytes =
            let binSearch = Array.concat [ be32 1u; be32 0u; be32 0u; be32 0u ]
            let pair = Array.concat [ be16 1u; be16 1u; beI16 500s ]
            Array.concat [ be32 (uint32 (12 + binSearch.Length + pair.Length)); be32 0u; be32 0u; binSearch; pair ]
        let format8 (first: uint32) (values: uint16 array) = Array.concat [ be16 8u; be16 first; be16 (uint32 values.Length); values |> Array.collect (fun v -> be16 (uint32 v)) ]
        let format2Bytes =
            let left = format8 1u [| 0us |]
            let right = format8 1u [| 0us |]
            let structBytes = Array.concat [ be32 2u; be32 28u; be32 (28u + uint32 left.Length); be32 (28u + uint32 left.Length + uint32 right.Length) ]
            let body = Array.concat [ structBytes; left; right; beI16 500s ]
            Array.concat [ be32 (uint32 (12 + body.Length)); be32 2u; be32 0u; body ]
        let format6Bytes =
            let row = format8 1u [| 0us |]
            let column = format8 1u [| 0us |]
            let structBytes = Array.concat [ be32 0u; be16 16u; be16 16u; be32 36u; be32 (36u + uint32 row.Length); be32 (36u + uint32 row.Length + uint32 column.Length); be32 (36u + uint32 row.Length + uint32 column.Length + 2u) ]
            let body = Array.concat [ structBytes; row; column; beI16 500s ]
            Array.concat [ be32 (uint32 (12 + body.Length)); be32 6u; be32 0u; body ]
        kerxFormat0 <- kerxTableOf format0Bytes
        kerxFormat2 <- kerxTableOf format2Bytes
        kerxFormat6 <- kerxTableOf format6Bytes
        let format1NullEntries = Array.init 5 (fun _ -> { NewState = 0; Flags = 0us; Data0 = 0xFFFFus; Data1 = 0us })
        kerxFormat1MatchNothing <- { Machine = uniformMachine 5 4uy 128 format1NullEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        let kerxFormat1ActionEntries =
            Array.init 5 (fun i -> if i = 4 then { NewState = 0; Flags = 0x8000us; Data0 = 0us; Data1 = 0us } else { NewState = 0; Flags = 0us; Data0 = 0xFFFFus; Data1 = 0us })
        kerxFormat1ChangeAll <- { Machine = uniformMachine 5 4uy 128 kerxFormat1ActionEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        kerxFormat1ChangeAllBody <- Array.concat [ be16 500u; be16 0xFFFFu ]
        kernFormat1MatchNothing <- { Machine = uniformMachine 5 4uy 128 format1NullEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        let kernFormat1ActionEntries =
            Array.init 5 (fun i -> if i = 4 then { NewState = 0; Flags = 0x8002us; Data0 = 0us; Data1 = 0us } else { NewState = 0; Flags = 0us; Data0 = 0xFFFFus; Data1 = 0us })
        kernFormat1ChangeAll <- { Machine = uniformMachine 5 4uy 128 kernFormat1ActionEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        kernFormat1ChangeAllBody <- Array.concat [ be16 0u; be16 501u ]
        kerxFormat4MatchNothing <- { Machine = uniformMachine 5 4uy 128 format1NullEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        kerxFormat4AttachAll <- { Machine = uniformMachine 5 4uy 128 kerxFormat1ActionEntries; Ranges = ImmutableArray.Empty; SubtableFlags = 0u; NumGlyphs = 128; StartSafeToBreakAtEndOfText = true }
        kerxFormat4AttachAllBody <- Array.concat [ be16 0u; be16 1u; be16 0u; be16 1u ]
        let ankrBytes = ankr Dense 16 (Map.ofList [ 1u, [| (100, 200); (30, 50) |] ])
        let ankrFile = Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u; be32 (uint32 Tags.Ankr); be32 0u; be32 28u; be32 (uint32 ankrBytes.Length); ankrBytes ]
        kerxFormat4AnchorFont <- Font.create (Face.withGlyphCount 16 (Face.create (ReadOnlyMemory ankrFile) 0u))
        Console.WriteLine($"RunEdit = {Unsafe.SizeOf<RunEdit>()} bytes, RunCursor = {Unsafe.SizeOf<RunCursor>()} bytes")

    /// <summary>Runs the NextGlyph pass over the whole run, changing nothing.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark(Baseline = true)>]
    member _.NextGlyphPassRun() = GlyphRun.runPass (NextGlyphPass()) () run

    /// <summary>Runs the mask pass over the whole run.</summary>
    /// <returns>The masked run.</returns>
    [<Benchmark>]
    member _.SetMaskPassRun() = GlyphRun.runPass (SetMaskPass()) 0 run

    /// <summary>Runs the position pass over the whole run.</summary>
    /// <returns>The positioned run.</returns>
    [<Benchmark>]
    member _.SetPositionPassRun() = GlyphRun.runPass (SetPositionPass()) 0 run

    /// <summary>Runs the output pass over the whole run.</summary>
    /// <returns>The output run.</returns>
    [<Benchmark>]
    member _.OutputInfoPassRun() = GlyphRun.runPass (OutputInfoPass()) 0 run

    /// <summary>Runs the skip pass over the whole run, one SkipGlyph per glyph.</summary>
    /// <returns>The skipped run.</returns>
    [<Benchmark>]
    member _.SkipGlyphPassRun() = GlyphRun.runPass (SkipGlyphPass()) () run

    /// <summary>Runs the output-then-skip pass over the whole run, two edits per glyph.</summary>
    /// <returns>The output run.</returns>
    [<Benchmark>]
    member _.OutputSkipPassRun() = GlyphRun.runPass (OutputSkipPass()) 0 run

    /// <summary>Runs the in-place replace pass over the whole run, one ReplaceGlyphProps per glyph.</summary>
    /// <returns>The replaced run.</returns>
    [<Benchmark>]
    member _.ReplaceGlyphPropsPassRun() = GlyphRun.runPass (ReplaceGlyphPropsPass()) 0 run

    /// <summary>Recomputes one glyph's properties through <c>Matching.setGlyphClass</c> 10,000 times, with no GDEF
    /// classes, so the pure props cost of a single substitution is visible.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.SetGlyphClassOnly() =
        let gdef = substFont.Ot.Gdef.Value
        let mutable index = 0
        while index < 10_000 do
            Matching.setGlyphClass gdef false (uint32 (index % 128)) 0us false false Unchecked.defaultof<GlyphProps> |> ignore
            index <- index + 1
        index

    /// <summary>Reads one glyph's class through <c>Gdef.glyphProps</c> 10,000 times, the GDEF read a single
    /// substitution pays when glyph classes are present.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.GdefGlyphPropsOnly() =
        let gdef = substFont.Ot.Gdef.Value
        let mutable index = 0
        while index < 10_000 do
            Gdef.glyphProps gdef (uint32 (index % 128)) |> ignore
            index <- index + 1
        index

    /// <summary>Tries the single-delta subtable on one glyph 10,000 times, <c>GsubApply.trySubtable</c>.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.TrySubtableOnly() =
        let gdef = substFont.Ot.Gdef.Value
        let subtable = SingleDelta (CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u }), 1u, 0xFFFFu)
        let mutable index = 0
        let infos = [| { Codepoint = 0u; Mask = 1u; Cluster = 0u; Var1 = 0u; Var2 = 0u } |]
        let props = [| Unchecked.defaultof<GlyphProps> |]
        let mutable cursor = RunCursor(ReadOnlySpan infos, ReadOnlySpan props, 0, ReadOnlySpan.Empty, ReadOnlySpan.Empty, ReadOnlySpan.Empty, false, ClusterLevel.MonotoneGraphemes, BufferFlags.Default, 0uy, 0u, 1000, true)
        while index < 10_000 do
            GsubApply.trySubtable gdef false 1u false subtable &cursor |> ignore
            index <- index + 1
        index

    /// <summary>Looks one glyph up in the range coverage 10,000 times, <c>Coverage.index</c>.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.CoverageIndexOnly() =
        let coverage = CoverageRanges (ImmutableArray.Create { First = 0u; Last = 127u; Value = 0u })
        let mutable index = 0
        while index < 10_000 do
            Coverage.index coverage (uint32 (index % 128)) |> ignore
            index <- index + 1
        index

    /// <summary>Builds the digest of the 10,000-glyph run once, the per-lookup digest refresh of
    /// <c>hb_ot_map_t::apply</c>.</summary>
    /// <returns>The first word of the digest.</returns>
    [<Benchmark>]
    member _.DigestOfSpanOnly() = (Digest.ofSpan (run.Infos.AsSpan()) |> TailCalls.settle).M0


    /// <summary>Runs the driver over a machine whose every glyph takes the null transition, so the fast path batches
    /// the whole 10,000-glyph stretch. Reported in nanoseconds per glyph.</summary>
    /// <returns>The shaped glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.NullTransitionRun() =
        let result = GlyphRun.runPass { Config = nullTransition.Config; Kind = AatNoopKind() } nullTransition.Seed run
        result.Infos.Length

    /// <summary>Runs the driver over a machine whose every entry is non-actionable and leads to a non-start state, so
    /// the null-transition fast path never applies and the general path advances one glyph per step. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The shaped glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.NonActionableTransitionRun() =
        let result = GlyphRun.runPass { Config = otherState.Config; Kind = AatNoopKind() } otherState.Seed run
        result.Infos.Length

    /// <summary>Runs the driver over a machine whose glyphs alternate between two classes and whose transitions
    /// alternate between two states, so no stretch can form and the general path advances one glyph per step with a
    /// safe-to-break lookup each time. Reported in nanoseconds per glyph.</summary>
    /// <returns>The shaped glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.NonActionableAlternatingRun() =
        let result = GlyphRun.runPass { Config = alternating.Config; Kind = AatNoopKind() } alternating.Seed run
        result.Infos.Length

    /// <summary>Runs the noncontextual subtable over a run no glyph in the lookup covers; the pass changes nothing and
    /// returns the input run. Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.NoncontextualMatchNothing() =
        let struct (result, _) = MorxNoncontextual.apply matchNothing 128 ImmutableArray.Empty 1u emptyContext false run
        result.Infos.Length

    /// <summary>Runs the noncontextual subtable over a run whose every glyph the lookup maps; the pass replaces every
    /// glyph. Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.NoncontextualChangeAll() =
        let struct (result, _) = MorxNoncontextual.apply changeAll 128 ImmutableArray.Empty 1u emptyContext false run
        result.Infos.Length

    /// <summary>Runs the contextual driver over a run no glyph is actionable on; the pass batches the whole run.
    /// Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.ContextualMatchNothing() =
        let seed: DriverState<ContextualContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxContextual.seed false }
        let kind =
            { ContextualKind.Extended = false; Body = ReadOnlyMemory.Empty; SubstitutionTablesAt = 0; NumGlyphs = 128; Context = emptyContext }
        let result = GlyphRun.runPass { Config = contextualMatchNothing; Kind = kind } seed run
        result.Infos.Length

    /// <summary>Runs the contextual driver over a run whose every glyph the current substitution replaces. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.ContextualChangeAll() =
        let seed: DriverState<ContextualContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxContextual.seed false }
        let kind =
            { ContextualKind.Extended = false; Body = ReadOnlyMemory contextualChangeAllBody; SubstitutionTablesAt = 2; NumGlyphs = 128; Context = emptyContext }
        let result = GlyphRun.runPass { Config = contextualChangeAll; Kind = kind } seed run
        result.Infos.Length

    /// <summary>Runs the rearrangement driver over a run no glyph carries a verb; the pass batches the whole run.
    /// Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.RearrangementMatchNothing() =
        let seed: DriverState<RearrangementContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxRearrangement.seed }
        let result = GlyphRun.runPass { Config = rearrangementMatchNothing; Kind = RearrangementKind() } seed run
        result.Infos.Length

    /// <summary>Runs the rearrangement driver over a run whose every glyph carries a verb; the pass rearranges every
    /// glyph. Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.RearrangementChangeAll() =
        let seed: DriverState<RearrangementContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxRearrangement.seed }
        let result = GlyphRun.runPass { Config = rearrangementChangeAll; Kind = RearrangementKind() } seed run
        result.Infos.Length

    /// <summary>Runs the ligature driver over a run no glyph is actionable on; the pass batches the whole run.
    /// Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.LigatureMatchNothing() =
        let seed: DriverState<LigatureContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxLigature.seed false }
        let result = GlyphRun.runPass { Config = ligatureMatchNothing; Kind = ligatureKind } seed run
        result.Infos.Length

    /// <summary>Runs the ligature driver over a run whose every glyph forms a single-component ligature. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.LigatureChangeAll() =
        let seed: DriverState<LigatureContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxLigature.seed false }
        let result = GlyphRun.runPass { Config = ligatureChangeAll; Kind = ligatureKind } seed run
        result.Infos.Length

    /// <summary>Runs the insertion driver over a run no glyph inserts on; the pass batches the whole run. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.InsertionMatchNothing() =
        let seed: DriverState<InsertionContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxInsertion.seed false }
        let result = GlyphRun.runPass { Config = insertionMatchNothing; Kind = insertionKind } seed run
        result.Infos.Length

    /// <summary>Runs the insertion driver over a run whose every glyph inserts one glyph before it. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.InsertionChangeAll() =
        let seed: DriverState<InsertionContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxInsertion.seed false }
        let result = GlyphRun.runPass { Config = insertionChangeAll; Kind = insertionKind } seed run
        result.Infos.Length

    /// <summary>Maps the glyphs to themselves with a plain array map.</summary>
    /// <returns>The identity array.</returns>
    [<Benchmark>]
    member _.MapiIdentity() = Sealed.mapi (fun _ (info: GlyphInfo) -> info) (run.Infos.AsSpan()) |> TailCalls.settle

    /// <summary>Sets one mask bit on every glyph with a plain array map.</summary>
    /// <returns>The masked array.</returns>
    [<Benchmark>]
    member _.MapiSetMask() =
        Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = info.Mask ||| 1u }) (run.Infos.AsSpan()) |> TailCalls.settle

    /// <summary>Applies one non-matching single-substitution lookup without digest gating: the pass scans every
    /// glyph and finds no match.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.SingleSubstNonGatedNonMatching() =
        Engine.applyTable false (Engine.GsubApplier()) nonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one non-matching single-substitution lookup stopped by its digest, the gate of
    /// <c>hb_ot_map_t::apply</c>.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.SingleSubstGated() =
        Engine.applyTable true (Engine.GsubApplier()) nonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one single-substitution lookup that replaces every glyph.</summary>
    /// <returns>The substituted run.</returns>
    [<Benchmark>]
    member _.SingleSubstReplaceAll() =
        Engine.applyTable false (Engine.GsubApplier()) replaceAllTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies a stage of ten lookups that each replace one glyph of a 50-glyph run.</summary>
    /// <returns>The substituted run.</returns>
    [<Benchmark>]
    member _.Stage10ReplaceOne() =
        Engine.applyTable true (Engine.GsubApplier()) replaceOneTable tenLookupMaps tenStage substFont stageRun

    /// <summary>Applies a stage of ten lookups of which none matches a 50-glyph run.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.Stage10NonMatching() =
        Engine.applyTable true (Engine.GsubApplier()) nonMatching10Table tenLookupMaps tenStage substFont stageRun

    /// <summary>Applies one non-matching ligature lookup without digest gating: the pass scans every glyph.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.LigatureNonGatedNonMatching() =
        Engine.applyTable false (Engine.GsubApplier()) ligatureNonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one non-matching ligature lookup stopped by its digest.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.LigatureGated() =
        Engine.applyTable true (Engine.GsubApplier()) ligatureNonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one ligature lookup that forms a ligature at every second glyph of a 10,000-glyph run.</summary>
    /// <returns>The substituted run.</returns>
    [<Benchmark>]
    member _.LigatureFormsEverySecond() =
        Engine.applyTable false (Engine.GsubApplier()) ligatureEverySecondTable singleLookupMaps singleStage substFont ligatureEverySecondRun

    /// <summary>Applies one non-matching single-substitution lookup through the lookup kernel, without digest gating.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.KernelSingleSubstNonGatedNonMatching() =
        let struct (covered, glyphLookups) = nonMatchingSets
        Engine.applyGsubCovered false covered glyphLookups nonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one non-matching single-substitution lookup stopped by its digest, through the lookup kernel.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.KernelSingleSubstGated() =
        let struct (covered, glyphLookups) = nonMatchingSets
        Engine.applyGsubCovered true covered glyphLookups nonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one single-substitution lookup that replaces every glyph, through the lookup kernel.</summary>
    /// <returns>The substituted run.</returns>
    [<Benchmark>]
    member _.KernelSingleSubstReplaceAll() =
        let struct (covered, glyphLookups) = replaceAllSets
        Engine.applyGsubCovered false covered glyphLookups replaceAllTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies a stage of ten lookups that each replace one glyph of a 50-glyph run, through the lookup kernel.</summary>
    /// <returns>The substituted run.</returns>
    [<Benchmark>]
    member _.KernelStage10ReplaceOne() =
        let struct (covered, glyphLookups) = replaceOneSets
        Engine.applyGsubCovered true covered glyphLookups replaceOneTable tenLookupMaps tenStage substFont stageRun

    /// <summary>Applies a stage of ten lookups of which none matches a 50-glyph run, through the lookup kernel.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.KernelStage10NonMatching() =
        let struct (covered, glyphLookups) = nonMatching10Sets
        Engine.applyGsubCovered true covered glyphLookups nonMatching10Table tenLookupMaps tenStage substFont stageRun

    /// <summary>Applies one non-matching ligature lookup through the lookup kernel, without digest gating.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.KernelLigatureNonGatedNonMatching() =
        let struct (covered, glyphLookups) = ligatureNonMatchingSets
        Engine.applyGsubCovered false covered glyphLookups ligatureNonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one non-matching ligature lookup stopped by its digest, through the lookup kernel.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark>]
    member _.KernelLigatureGated() =
        let struct (covered, glyphLookups) = ligatureNonMatchingSets
        Engine.applyGsubCovered true covered glyphLookups ligatureNonMatchingTable singleLookupMaps singleStage substFont substRun

    /// <summary>Applies one ligature lookup that forms a ligature at every second glyph of a 10,000-glyph run, through
    /// the lookup kernel.</summary>
    /// <returns>The substituted run.</returns>
    [<Benchmark>]
    member _.KernelLigatureFormsEverySecond() =
        let struct (covered, glyphLookups) = ligatureEverySecondSets
        Engine.applyGsubCovered false covered glyphLookups ligatureEverySecondTable singleLookupMaps singleStage substFont ligatureEverySecondRun

    /// <summary>Applies one context lookup whose coverage no glyph of a 10,000-glyph run is in, through the lookup kernel
    /// with the plan's sets, which stop it before it runs. Reported in nanoseconds per glyph.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KernelContextGated() =
        let struct (covered, glyphLookups) = contextGatedSets
        Engine.applyGsubCovered true covered glyphLookups contextGatedTable contextMaps singleStage substFont substRun

    /// <summary>Applies one context lookup that covers every second glyph of a 10,000-glyph run and matches nowhere, through
    /// the lookup kernel without digest gating, so every covered glyph tries its rules. Reported in nanoseconds per glyph.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KernelContextCoveredMatchesNothing() =
        let struct (covered, glyphLookups) = contextCoveredSets
        Engine.applyGsubCovered false covered glyphLookups contextCoveredTable contextMaps singleStage substFont contextTwoRun

    /// <summary>The same for a chained-context lookup of the third format. Reported in nanoseconds per glyph.</summary>
    /// <returns>The unchanged run.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KernelChainCoveredMatchesNothing() =
        let struct (covered, glyphLookups) = chainCoveredSets
        Engine.applyGsubCovered false covered glyphLookups chainCoveredTable contextMaps singleStage substFont contextTwoRun

    /// <summary>Applies one context lookup that matches at every third glyph of a 10,000-glyph run and applies one nested
    /// single substitution there, through the lookup kernel. Reported in nanoseconds per glyph.</summary>
    /// <returns>The substituted run.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KernelContextMatchesEveryThird() =
        let struct (covered, glyphLookups) = contextEveryThirdSets
        Engine.applyGsubCovered false covered glyphLookups contextEveryThirdTable contextMaps singleStage substFont contextThreeRun

    /// <summary>Applies one kerx format 0 subtable over a run of unmapped glyphs; the kern machine finds no pair.
    /// Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat0MatchNothing() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat0, 16)
        Kern.applyProvider kerxPlan kerxFont true false provider kerxMatchNothingRun |> ignore
        kerxMatchNothingRun.Infos.Length

    /// <summary>Applies one kerx format 0 subtable over a run whose every adjacent pair kerns. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat0ChangeAll() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat0, 16)
        Kern.applyProvider kerxPlan kerxFont true false provider kerxChangeAllRun |> ignore
        kerxChangeAllRun.Infos.Length

    /// <summary>Applies one kerx format 2 subtable over a run of unmapped glyphs. Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat2MatchNothing() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat2, 16)
        Kern.applyProvider kerxPlan kerxFont true false provider kerxMatchNothingRun |> ignore
        kerxMatchNothingRun.Infos.Length

    /// <summary>Applies one kerx format 2 subtable over a run whose every adjacent pair kerns. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat2ChangeAll() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat2, 16)
        Kern.applyProvider kerxPlan kerxFont true false provider kerxChangeAllRun |> ignore
        kerxChangeAllRun.Infos.Length

    /// <summary>Applies one kerx format 6 subtable over a run of unmapped glyphs. Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat6MatchNothing() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat6, 16)
        Kern.applyProvider kerxPlan kerxFont true false provider kerxMatchNothingRun |> ignore
        kerxMatchNothingRun.Infos.Length

    /// <summary>Applies one kerx format 6 subtable over a run whose every adjacent pair kerns. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat6ChangeAll() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat6, 16)
        Kern.applyProvider kerxPlan kerxFont true false provider kerxChangeAllRun |> ignore
        kerxChangeAllRun.Infos.Length

    /// <summary>Runs the kerx format 1 driver over a run no glyph is actionable on; the pass batches the whole run.
    /// Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat1MatchNothing() =
        let spec = { Extended = true; TableBytes = ReadOnlyMemory.Empty; RangeEnd = 0; MachineAt = 0; KernActionAt = 0; TupleCount = 0u }
        let kind = { Spec = spec; Font = kerxFont; Horizontal = true; CrossStream = false; KernMask = 0x80000000u }
        let seed: DriverState<KerxFormat1Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxStateMachines.seed () }
        let result = GlyphRun.runPass { StateMachinePass.Config = kerxFormat1MatchNothing; Kind = kind } seed kerxMatchNothingRun
        result.Infos.Length

    /// <summary>Runs the kerx format 1 driver over a run whose every glyph is pushed and kerned. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat1ChangeAll() =
        let spec = { Extended = true; TableBytes = ReadOnlyMemory kerxFormat1ChangeAllBody; RangeEnd = kerxFormat1ChangeAllBody.Length; MachineAt = 0; KernActionAt = 0; TupleCount = 0u }
        let kind = { Spec = spec; Font = kerxFont; Horizontal = true; CrossStream = false; KernMask = 0x80000000u }
        let seed: DriverState<KerxFormat1Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxStateMachines.seed () }
        let result = GlyphRun.runPass { StateMachinePass.Config = kerxFormat1ChangeAll; Kind = kind } seed kerxChangeAllRun
        result.Infos.Length

    /// <summary>Runs the Apple kern format 1 driver over a run no glyph is actionable on. Reported in nanoseconds per
    /// glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KernFormat1MatchNothing() =
        let spec = { Extended = false; TableBytes = ReadOnlyMemory.Empty; RangeEnd = 0; MachineAt = 0; KernActionAt = 0; TupleCount = 0u }
        let kind = { Spec = spec; Font = kerxFont; Horizontal = true; CrossStream = false; KernMask = 0x80000000u }
        let seed: DriverState<KerxFormat1Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxStateMachines.seed () }
        let result = GlyphRun.runPass { StateMachinePass.Config = kernFormat1MatchNothing; Kind = kind } seed kerxMatchNothingRun
        result.Infos.Length

    /// <summary>Runs the Apple kern format 1 driver over a run whose every glyph is pushed and kerned. Reported in
    /// nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KernFormat1ChangeAll() =
        let spec = { Extended = false; TableBytes = ReadOnlyMemory kernFormat1ChangeAllBody; RangeEnd = kernFormat1ChangeAllBody.Length; MachineAt = 0; KernActionAt = 2; TupleCount = 0u }
        let kind = { Spec = spec; Font = kerxFont; Horizontal = true; CrossStream = false; KernMask = 0x80000000u }
        let seed: DriverState<KerxFormat1Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxStateMachines.seed () }
        let result = GlyphRun.runPass { StateMachinePass.Config = kernFormat1ChangeAll; Kind = kind } seed kerxChangeAllRun
        result.Infos.Length

    /// <summary>Runs the kerx format 4 driver over a run no glyph is actionable on; the pass batches the whole run.
    /// Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat4MatchNothing() =
        let spec = { TableBytes = ReadOnlyMemory.Empty; RangeEnd = 0; ActionsAt = 0L; ActionType = 2 }
        let kind: KerxFormat4Kind = { Spec = spec; Ankr = kerxFont.Ot.Ankr.Value; Font = kerxFont; Horizontal = true; Reversed = false }
        let seed: DriverState<KerxFormat4Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxFormat4Machine.seed () }
        let result = GlyphRun.runPass { StateMachinePass.Config = kerxFormat4MatchNothing; Kind = kind } seed kerxMatchNothingRun
        result.Infos.Length

    /// <summary>Runs the kerx format 4 driver with control point coordinate actions over a run whose every glyph after the
    /// first is attached to the glyph before it. Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat4AttachAll() =
        let spec = { TableBytes = ReadOnlyMemory kerxFormat4AttachAllBody; RangeEnd = kerxFormat4AttachAllBody.Length; ActionsAt = 0L; ActionType = 2 }
        let kind: KerxFormat4Kind = { Spec = spec; Ankr = kerxFont.Ot.Ankr.Value; Font = kerxFont; Horizontal = true; Reversed = false }
        let seed: DriverState<KerxFormat4Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxFormat4Machine.seed () }
        let result = GlyphRun.runPass { StateMachinePass.Config = kerxFormat4AttachAll; Kind = kind } seed kerxChangeAllRun
        result.Infos.Length

    /// <summary>Runs the kerx format 4 driver with anchor point actions, each reading two anchors from an ankr table, over a
    /// run whose every glyph after the first is attached to the glyph before it. Reported in nanoseconds per glyph.</summary>
    /// <returns>The glyph count.</returns>
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.KerxFormat4AnchorAll() =
        let spec = { TableBytes = ReadOnlyMemory kerxFormat4AttachAllBody; RangeEnd = kerxFormat4AttachAllBody.Length; ActionsAt = 0L; ActionType = 1 }
        let kind: KerxFormat4Kind = { Spec = spec; Ankr = kerxFormat4AnchorFont.Ot.Ankr.Value; Font = kerxFormat4AnchorFont; Horizontal = true; Reversed = false }
        let seed: DriverState<KerxFormat4Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxFormat4Machine.seed () }
        let result = GlyphRun.runPass { StateMachinePass.Config = kerxFormat4AttachAll; Kind = kind } seed kerxChangeAllRun
        result.Infos.Length

    /// <summary>Runs a pass under the counting wrapper and returns the tally as text.</summary>
    /// <returns>The steps and the edits per glyph.</returns>
    member _.Tally (run: Func<unit, int>) =
        EditTally.Reset()
        run.Invoke() |> ignore
        let kinds =
            EditTally.Kinds
            |> Array.mapi (fun index count -> struct (index, count))
            |> Array.filter (fun (struct (_, count)) -> count > 0L)
            |> Array.map (fun (struct (index, count)) -> $"{enum<EditKind> index}={float count / 10000.0:N2}")
        $"steps/glyph={float EditTally.Steps / 10000.0:N2}; " + String.Join(", ", kinds)

    member internal _.PassAlternating() = struct ({ Config = alternating.Config; Kind = AatNoopKind() }, alternating.Seed, run)

    member internal _.PassContextual() =
        let seed: DriverState<ContextualContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxContextual.seed false }
        let kind =
            { ContextualKind.Extended = false; Body = ReadOnlyMemory contextualChangeAllBody; SubstitutionTablesAt = 2; NumGlyphs = 128; Context = emptyContext }
        struct ({ StateMachinePass.Config = contextualChangeAll; Kind = kind }, seed, run)

    member internal _.PassRearrangement() =
        let seed: DriverState<RearrangementContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxRearrangement.seed }
        struct ({ StateMachinePass.Config = rearrangementChangeAll; Kind = RearrangementKind() }, seed, run)

    member internal _.PassLigature() =
        let seed: DriverState<LigatureContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxLigature.seed false }
        struct ({ StateMachinePass.Config = ligatureChangeAll; Kind = ligatureKind }, seed, run)

    member internal _.PassInsertion() =
        let seed: DriverState<InsertionContext> =
            { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxInsertion.seed false }
        struct ({ StateMachinePass.Config = insertionChangeAll; Kind = insertionKind }, seed, run)

    member internal _.PassKerxFormat1() =
        let spec = { Extended = true; TableBytes = ReadOnlyMemory kerxFormat1ChangeAllBody; RangeEnd = kerxFormat1ChangeAllBody.Length; MachineAt = 0; KernActionAt = 0; TupleCount = 0u }
        let kind = { Spec = spec; Font = kerxFont; Horizontal = true; CrossStream = false; KernMask = 0x80000000u }
        let seed: DriverState<KerxFormat1Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxStateMachines.seed () }
        struct ({ StateMachinePass.Config = kerxFormat1ChangeAll; Kind = kind }, seed, kerxChangeAllRun)

    member internal _.PassKerxFormat4() =
        let spec = { TableBytes = ReadOnlyMemory kerxFormat4AttachAllBody; RangeEnd = kerxFormat4AttachAllBody.Length; ActionsAt = 0L; ActionType = 2 }
        let kind: KerxFormat4Kind = { Spec = spec; Ankr = kerxFont.Ot.Ankr.Value; Font = kerxFont; Horizontal = true; Reversed = false }
        let seed: DriverState<KerxFormat4Context> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = KerxFormat4Machine.seed () }
        struct ({ StateMachinePass.Config = kerxFormat4AttachAll; Kind = kind }, seed, kerxChangeAllRun)

    member _.KerxFormat0ChangeAllReference() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat0, 16)
        Kern.applyProviderWith Reference kerxPlan kerxFont true false provider kerxChangeAllRun |> ignore
        kerxChangeAllRun.Infos.Length

    member _.KerxFormat0IndexedChangeAll() =
        let provider = indexedPairsOf kerxFormat0
        Kern.applyProvider kerxPlan kerxFont true false provider kerxChangeAllRun |> ignore
        kerxChangeAllRun.Infos.Length

    member _.KerxFormat0IndexedMatchNothing() =
        let provider = indexedPairsOf kerxFormat0
        Kern.applyProvider kerxPlan kerxFont true false provider kerxMatchNothingRun |> ignore
        kerxMatchNothingRun.Infos.Length

    member _.KerxFormat0MatchNothingGeneral() =
        let provider = KerxPairs.KerxPairProvider(kerxFormat0, 16)
        Kern.applyProvider kerxPlan kerxFont true false provider kerxMatchNothingRun |> ignore
        kerxMatchNothingRun.Infos.Length
