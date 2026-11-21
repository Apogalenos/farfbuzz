module Farfbuzz.Tests.ComplexShapers.RagelTableTests

open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Gen
open Farfbuzz.Harness

/// <summary>The path of one machine header.</summary>
/// <param name="name">The machine name.</param>
/// <returns>The upstream header path.</returns>
let private machineHh (name: string) =
    Path.Combine(RepositoryPaths.upstream (), "src", $"hb-ot-shaper-{name}-machine.hh")

/// <summary>Parses one machine header, failing the test on an error.</summary>
/// <param name="name">The machine name.</param>
/// <returns>The flat machine.</returns>
let private parseOrFail (name: string) : RagelFlat.FlatMachine =
    match RagelFlat.parse (machineHh name) name with
    | Error error -> failwith error
    | Ok flat -> flat

/// <summary>Collects every syllable type a machine's actions emit.</summary>
/// <param name="actions">The machine actions.</param>
/// <returns>The found syllable types.</returns>
let private foundTypes (actions: ImmutableArray<RagelAction>) : int list =
    let collect (ops: ImmutableArray<RagelOp>) =
        ops
        |> Seq.choose (fun op -> match op with | RagelOp.FoundSyllable (t, _) -> Some (int t) | _ -> None)
    actions
    |> Seq.collect (fun action ->
        match action with
        | RagelAction.Ops ops -> collect ops
        | RagelAction.ActSwitch byAct -> byAct |> Seq.collect collect
        | RagelAction.NoAction -> Seq.empty)
    |> Seq.toList

/// <summary>Asserts a machine's shape against the numbers read from upstream.</summary>
/// <param name="label">The test label.</param>
/// <param name="name">The machine name.</param>
/// <param name="states">The state count.</param>
/// <param name="transitions">The transition count.</param>
/// <param name="start">The start state.</param>
/// <param name="exportCount">The category export count.</param>
/// <param name="machine">The generated machine.</param>
/// <param name="spot">A check of two category literals.</param>
let private shapeTest label name states transitions start exportCount (machine: RagelMachine) (spot: unit -> unit) =
    test label {
        let flat = parseOrFail name
        Expect.equal flat.Exports.Length exportCount "export count"
        Expect.equal flat.Constants["start"] start "start constant"
        Expect.equal flat.Constants["first_final"] start "first_final constant"
        Expect.equal flat.Constants["en_main"] start "en_main constant"
        Expect.equal flat.Constants["error"] -1 "error constant"
        let struct (_, keySpans) = flat.Arrays["key_spans"]
        Expect.equal keySpans.Length states "states"
        let struct (_, transTargs) = flat.Arrays["trans_targs"]
        Expect.equal transTargs.Length transitions "transitions"
        Expect.equal machine.Start start "generated start"
        Expect.equal (machine.TransKeys.Length / 2) states "generated states"
        Expect.equal machine.TransTargs.Length transitions "generated transitions"
        Expect.equal machine.TransActions.Length transitions "generated transition actions"
        spot ()
    }

/// <summary>Asserts a machine's transitions and actions close over their states and types.</summary>
/// <param name="label">The test label.</param>
/// <param name="machine">The generated machine.</param>
/// <param name="states">The state count.</param>
/// <param name="typeCount">The syllable type count.</param>
let private closureTest label (machine: RagelMachine) states typeCount =
    test label {
        let targetsInRange = machine.TransTargs |> Seq.forall (fun t -> int t < states)
        Expect.isTrue targetsInRange "every transition target is a state"
        let actionsInRange = machine.TransActions |> Seq.forall (fun a -> int a < machine.Actions.Length)
        Expect.isTrue actionsInRange "every transition action id decodes"
        let founds = foundTypes machine.Actions
        Expect.isTrue (founds |> List.forall (fun t -> t < typeCount)) "every found syllable type is valid"
        Expect.isGreaterThan founds.Length 0 "some syllable is found"
    }

/// <summary>The P6.S03 Ragel table shape and closure cases.</summary>
[<Tests>]
let tests =
    testList "P6.S03" [
        shapeTest "the indic machine has the upstream shape" "indic" 138 151 31 21 IndicMachine.machine (fun () ->
            Expect.equal IndicMachine.A 9uy "indic A"
            Expect.equal IndicMachine.VD 9uy "indic VD alias")
        shapeTest "the khmer machine has the upstream shape" "khmer" 43 49 21 15 KhmerMachine.machine (fun () ->
            Expect.equal KhmerMachine.C 1uy "khmer C"
            Expect.equal KhmerMachine.Ra 15uy "khmer Ra")
        shapeTest "the myanmar machine has the upstream shape" "myanmar" 53 61 0 25 MyanmarMachine.machine (fun () ->
            Expect.equal MyanmarMachine.As 32uy "myanmar As"
            Expect.equal MyanmarMachine.IV 2uy "myanmar IV alias")
        shapeTest "the use machine has the upstream shape" "use" 127 147 1 44 UseMachine.machine (fun () ->
            Expect.equal UseMachine.B 1uy "use B"
            Expect.equal UseMachine.O 0uy "use O")
        closureTest "the indic machine actions and transitions close" IndicMachine.machine 138 6
        closureTest "the khmer machine actions and transitions close" KhmerMachine.machine 43 3
        closureTest "the myanmar machine actions and transitions close" MyanmarMachine.machine 53 3
        closureTest "the use machine actions and transitions close" UseMachine.machine 127 9
    ]

/// <summary>The category alphabet each machine's random inputs are drawn from: every category the machine names and a few beyond its
/// keys, which take the error transition.</summary>
[<Literal>]
let private CategoryLimit = 48

/// <summary>The longest category run of a random input.</summary>
[<Literal>]
let private LongestRun = 90

/// <summary>A random category run.</summary>
let private genCategories =
    Gen.choose (0, LongestRun) |> Gen.bind (fun length -> Gen.arrayOfLength length (Gen.choose (0, CategoryLimit) |> Gen.map byte))

/// <summary>The syllable bytes and the broken flag the interpreter paints for categories, through the identity mapping or a mapping
/// that spaces the glyphs of the buffer.</summary>
/// <param name="machine">The machine.</param>
/// <param name="compiled">The same machine, flattened.</param>
/// <param name="spaced">Whether the buffer has a glyph between every two machine indices.</param>
/// <param name="categories">The categories.</param>
/// <returns>The painted bytes and the flag of the interpreter, then those of the flat scan.</returns>
let private bothScans (machine: RagelMachine) (compiled: RagelCompiled) (spaced: bool) (categories: byte array) =
    let positions =
        match spaced with
        | true -> ImmutableArray.CreateRange (Seq.init categories.Length (fun index -> 2 * index))
        | false -> Unchecked.defaultof<ImmutableArray<int>>
    let glyphs = match spaced with | true -> 2 * categories.Length + 1 | false -> categories.Length
    let propsOf () = ImmutableArray.CreateRange (Seq.init glyphs (fun _ -> Unchecked.defaultof<GlyphProps>))
    let categoryArray = ImmutableArray.CreateRange categories
    let steppedProps = propsOf ()
    let flatProps = propsOf ()
    let stepped = Ragel.findSyllablesOwnedStepped machine categoryArray positions steppedProps
    let flat = Ragel.findSyllablesOwned compiled categoryArray positions flatProps
    struct ((Seq.map (fun (props: GlyphProps) -> props.Syllable) steppedProps |> Seq.toArray, stepped), (Seq.map (fun (props: GlyphProps) -> props.Syllable) flatProps |> Seq.toArray, flat))

/// <summary>A property that the flat scan paints what the interpreter paints, for random categories.</summary>
/// <param name="label">The label.</param>
/// <param name="machine">The machine.</param>
/// <param name="compiled">The same machine, flattened.</param>
/// <param name="spaced">Whether the buffer has a glyph between every two machine indices.</param>
let private scanProperty label machine compiled spaced =
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } label
        (Prop.forAll (Arb.fromGen genCategories) (fun categories ->
            let struct (stepped, flat) = bothScans machine compiled spaced categories
            stepped = flat))

/// <summary>The run of a category array with glyphs that carry the categories, clusters that repeat or grow, and no syllable yet.</summary>
/// <param name="categories">The categories.</param>
/// <param name="steps">How far each cluster is from the one before.</param>
/// <returns>The glyphs and their properties.</returns>
let private glyphsOf (categories: byte array) (steps: int array) =
    let clusters = Array.scan (fun total step -> total + uint32 (step % 2)) 0u steps
    let infos = ImmutableArray.CreateRange (Seq.init categories.Length (fun index -> { Codepoint = 0x41u; Mask = 0u; Cluster = clusters[index]; Var1 = 0u; Var2 = 0u }))
    let props = ImmutableArray.CreateRange (Seq.init categories.Length (fun index -> { Unchecked.defaultof<GlyphProps> with ShaperCategory = categories[index] }))
    struct (infos, props)

/// <summary>A property that the scan which marks every syllable unsafe to break as it finds it equals the scan followed by the pass over the
/// chunks of the syllable bytes, at every cluster level.</summary>
/// <param name="label">The label.</param>
/// <param name="compiled">The machine, flattened.</param>
let private breakingProperty label (compiled: RagelCompiled) =
    let generated = Gen.zip (Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]) (Gen.zip genCategories (Gen.arrayOfLength 100 (Gen.choose (0, 3))))
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } label
        (Prop.forAll (Arb.fromGen generated) (fun (level, (categories, steps)) ->
            let struct (referenceInfos, referenceProps) = glyphsOf categories steps
            let struct (fusedInfos, fusedProps) = glyphsOf categories steps
            let referenceBroken = Ragel.findSyllablesOwned compiled (ImmutableArray.CreateRange categories) Unchecked.defaultof<ImmutableArray<int>> referenceProps
            SyllableOwned.unsafeToBreakSyllables level referenceInfos referenceProps
            let fusedBroken = SyllableOwned.paintFlatBreaking compiled level fusedInfos fusedProps
            referenceBroken = fusedBroken
            && Seq.toArray referenceInfos = Seq.toArray fusedInfos
            && Seq.toArray referenceProps = Seq.toArray fusedProps))

/// <summary>The flat scan of each machine equals the interpreter it replaces.</summary>
[<Tests>]
let flatScanTests =
    testList "P6.S03 flat scan" [
        scanProperty "the indic scan paints what the interpreter paints" IndicMachine.machine Ragel.indic false
        scanProperty "the khmer scan paints what the interpreter paints" KhmerMachine.machine Ragel.khmer false
        scanProperty "the myanmar scan paints what the interpreter paints" MyanmarMachine.machine Ragel.myanmar false
        scanProperty "the use scan paints what the interpreter paints" UseMachine.machine Ragel.universal false
        scanProperty "the use scan paints what the interpreter paints through a mapping" UseMachine.machine Ragel.universal true
        scanProperty "the indic scan paints what the interpreter paints through a mapping" IndicMachine.machine Ragel.indic true
        breakingProperty "the indic scan that marks syllables unsafe to break equals the scan and the pass after it" Ragel.indic
        breakingProperty "the khmer scan that marks syllables unsafe to break equals the scan and the pass after it" Ragel.khmer
        breakingProperty "the myanmar scan that marks syllables unsafe to break equals the scan and the pass after it" Ragel.myanmar
        breakingProperty "the use scan that marks syllables unsafe to break equals the scan and the pass after it" Ragel.universal
        testCase "every action body of the four machines is held by the packed effects the scan applies without running its statements" (fun _ ->
            for compiled in [ Ragel.indic; Ragel.khmer; Ragel.myanmar; Ragel.universal ] do
                Expect.isFalse (compiled.ListEffects |> Seq.exists (fun effect -> effect = RagelCodes.EffectSlow)) "no action body takes the slow path")
    ]
