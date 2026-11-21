module Farfbuzz.Tests.AatKernelTests

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.AnchorTables

/// <summary>The number of random cases of each machine kind.</summary>
[<Literal>]
let private CaseCount = 5000

/// <summary>The kern mask bit the kerx machines set flags against.</summary>
[<Literal>]
let private KernMaskBit = 0x80000000u

/// <summary>The marker for a state machine of an AAT kind, with the run it works on.</summary>
type private Case<'Kind, 'Context when 'Kind : struct and 'Kind :> IStateMachineKind<'Context> and 'Context : struct> =
    { Config: DriverConfig
      Kind: 'Kind
      Seed: 'Context
      Mask: uint16
      Quiet: bool
      Run: GlyphRun }

/// <summary>Every field of a run a pass can change, in a comparable form.</summary>
/// <param name="run">The run.</param>
/// <returns>The infos, properties, positions and the scalar fields.</returns>
let private fieldsOf (run: GlyphRun) =
    struct (run.Infos.AsSpan().ToArray(),
            run.Props.AsSpan().ToArray(),
            run.Positions.AsSpan().ToArray(),
            struct (run.HavePositions, run.MaxOps, run.Successful, run.Serial, run.RandomState, run.ScratchFlags, run.Infos.Length))

/// <summary>Describes how two runs differ, or nothing when they agree on every field.</summary>
/// <param name="what">Which kernel produced the second run.</param>
/// <param name="expected">The run the interpreter produced.</param>
/// <param name="actual">The run the kernel produced.</param>
/// <returns>The first difference, or an empty string.</returns>
let private describe (what: string) (expected: GlyphRun) (actual: GlyphRun) =
    let struct (expectedInfos, expectedProps, expectedPositions, expectedScalars) = fieldsOf expected
    let struct (actualInfos, actualProps, actualPositions, actualScalars) = fieldsOf actual
    let firstDifference (show: 'T -> string) (left: 'T array) (right: 'T array) =
        match left.Length = right.Length with
        | false -> Some $"length {left.Length} against {right.Length}"
        | true ->
            Array.zip left right
            |> Array.tryFindIndex (fun (a, b) -> a <> b)
            |> Option.map (fun index -> $"index {index}: {show left[index]} against {show right[index]}")
    let showInfo (info: GlyphInfo) = $"glyph {info.Codepoint} mask {info.Mask} cluster {info.Cluster} var {info.Var1},{info.Var2}"
    let showProps (props: GlyphProps) = $"unicode {props.UnicodeProps} class {props.GlyphClass} lig {props.LigProps} index {props.GlyphIndex}"
    let showPosition (position: GlyphPosition) = $"advance {position.XAdvance},{position.YAdvance} offset {position.XOffset},{position.YOffset} var {position.Var}"
    match firstDifference showInfo expectedInfos actualInfos, firstDifference showProps expectedProps actualProps, firstDifference showPosition expectedPositions actualPositions with
    | Some difference, _, _ -> $"{what}: infos differ at {difference}"
    | None, Some difference, _ -> $"{what}: props differ at {difference}"
    | None, None, Some difference -> $"{what}: positions differ at {difference}"
    | None, None, None ->
        match expectedScalars = actualScalars with
        | true -> String.Empty
        | false -> $"{what}: scalars {expectedScalars} against {actualScalars}"

/// <summary>Runs a case on the reference interpreter, on the kernel over the table as it is and on the kernel over the
/// dense table, and compares the runs and what the context observed.</summary>
/// <param name="observe">The observable part of the final context.</param>
/// <param name="case">The case.</param>
/// <returns>The first difference, or an empty string when the three agree on every field.</returns>
let private diagnose (observe: 'Context -> 'Observed) (case: Case<'Kind, 'Context>) =
    let struct (expected, expectedContext) = AatKernel.run Reference ValueNone case.Config case.Kind case.Mask false case.Seed case.Run
    let dense = DenseMachine.build case.Config.Machine case.Config.NumGlyphs case.Kind
    let struct (general, generalContext) = AatKernel.run Kernel ValueNone case.Config case.Kind case.Mask case.Quiet case.Seed case.Run
    let struct (compiled, compiledContext) = AatKernel.run Kernel dense case.Config case.Kind case.Mask case.Quiet case.Seed case.Run
    match dense.IsSome, describe "general" expected general, describe "dense" expected compiled with
    | false, _, _ -> "the table did not compile"
    | true, difference, _ when difference.Length > 0 -> difference
    | true, _, difference when difference.Length > 0 -> difference
    | true, _, _ ->
        match observe generalContext = observe expectedContext, observe compiledContext = observe expectedContext with
        | true, true -> String.Empty
        | false, _ -> $"general context {observe generalContext} against {observe expectedContext}"
        | _, false -> $"dense context {observe compiledContext} against {observe expectedContext}"

/// <summary>The class and glyph numbers of the random tables.</summary>
[<Literal>]
let private GlyphCount = 10

/// <summary>Generates one random run over the glyphs of a table.</summary>
/// <param name="positions">Whether the run carries positions.</param>
/// <param name="longest">The longest run.</param>
/// <returns>The generator.</returns>
let private runGen (positions: bool) (longest: int) =
    gen {
        let! count = Gen.frequency [ 1, Gen.constant 0; 1, Gen.constant 1; 12, Gen.choose (2, longest) ]
        let! codepoints =
            Gen.arrayOfLength count (Gen.frequency [ 12, Gen.choose (0, GlyphCount - 1) |> Gen.map uint32; 1, Gen.constant 0xFFFFu; 1, Gen.choose (GlyphCount, GlyphCount + 3) |> Gen.map uint32 ])
        let! steps = Gen.arrayOfLength count (Gen.elements [ 0u; 1u; 1u; 1u; 2u ])
        let ascending = Array.scan (+) 0u steps |> Array.take count
        let! shape = Gen.frequency [ 6, Gen.constant 0; 2, Gen.constant 1; 1, Gen.constant 2 ]
        let clusters =
            match shape with
            | 0 -> ascending
            | 1 -> ascending |> Array.rev
            | _ -> ascending |> Array.mapi (fun index cluster -> match index % 3 with | 0 -> cluster | 1 -> cluster / 2u | _ -> cluster + 1u)
        let! masks = Gen.arrayOfLength count (Gen.elements [ 0u; 0u; 1u; 2u; 3u; KernMaskBit; KernMaskBit ||| 3u ])
        let! unicode = Gen.arrayOfLength count (Gen.elements [ 0us; 0us; 0us; 0x20us; 0x0400us; 0x0420us ])
        let! classes = Gen.arrayOfLength count (Gen.elements [ 0us; 0us; 1us; 2us; 3us; 4us ])
        let! advances = Gen.arrayOfLength count (Gen.choose (-50, 50))
        let! offsets = Gen.arrayOfLength count (Gen.zip (Gen.choose (-30, 30)) (Gen.choose (-30, 30)))
        let! attach = Gen.arrayOfLength count (Gen.elements [ 0u; 0x00010001u; 0x00020001u; 0x0002FFFFu; 0x00020000u ])
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters; ClusterLevel.Graphemes ]
        let! maxOps = Gen.frequency [ 4, Gen.elements [ 1; 2; 3; 5; 8; 13; 21; 40; 100 ]; 6, Gen.constant (max 8192 (count * 64)) ]
        let! flags = Gen.elements [ BufferFlags.Default; BufferFlags.ProduceUnsafeToConcat ]
        let! randomState = Gen.elements [ 1u; 7u; 0xFFFFFFFFu ]
        let! serial = Gen.elements [ 0u; 1u; 250u ]
        let! scratch = Gen.elements [ ScratchFlags.Default; ScratchFlags.HasGposAttachment ]
        return
            { GlyphRun.empty with
                Infos = Sealed.init count (fun i -> { Codepoint = codepoints[i]; Mask = masks[i]; Cluster = clusters[i]; Var1 = uint32 i; Var2 = 7u })
                Props = Sealed.init count (fun i -> { UnicodeProps = unicode[i]; GlyphClass = classes[i]; LigProps = byte i; Syllable = 0uy; ShaperCategory = 0uy; ShaperAuxiliary = 0uy; GlyphIndex = uint32 i })
                Positions =
                    match positions with
                    | true -> Sealed.init count (fun i -> { XAdvance = advances[i]; YAdvance = 0; XOffset = fst offsets[i]; YOffset = snd offsets[i]; Var = attach[i] })
                    | false -> ImmutableArray.Empty
                HavePositions = positions
                ClusterLevel = level
                MaxOps = maxOps
                Flags = flags
                RandomState = randomState
                Serial = serial
                ScratchFlags = scratch
                SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }
    }

/// <summary>Generates a state machine: a class for every glyph, rows of entries with the state array's indirection, and
/// the state bias.</summary>
/// <param name="flagsGen">The entry flags.</param>
/// <param name="data0Gen">The first entry datum.</param>
/// <param name="data1Gen">The second entry datum.</param>
/// <param name="extended">Whether the table is an extended one.</param>
/// <returns>The generator.</returns>
let private machineGen (flagsGen: Gen<uint16>) (data0Gen: Gen<uint16>) (data1Gen: Gen<uint16>) (extended: bool) =
    gen {
        let! nClasses = Gen.elements [ 4; 5; 6; 8 ]
        let! nStates = Gen.choose (1, 6)
        let! bias = Gen.frequency [ 5, Gen.constant 0; 1, Gen.constant 1 ]
        let rows = nStates + bias
        let! first = Gen.elements [ 0us; 0us; 1us; 2us ]
        let! glyphClasses = Gen.arrayOfLength GlyphCount (Gen.frequency [ 12, Gen.choose (0, nClasses - 1); 1, Gen.choose (nClasses, nClasses + 2) ] |> Gen.map byte)
        let entryGen =
            gen {
                let! newState = Gen.frequency [ 3, Gen.choose (-bias, nStates - 1 - bias); 1, Gen.constant 0 ]
                let! flags = flagsGen
                let! data0 = data0Gen
                let! data1 = data1Gen
                return { NewState = newState; Flags = flags; Data0 = data0; Data1 = data1 }
            }
        let! poolSize = Gen.choose (1, rows * nClasses)
        let! pool = Gen.arrayOfLength poolSize entryGen
        let! cells = Gen.arrayOfLength (rows * nClasses) (Gen.choose (0, poolSize - 1))
        let classBytes = Array.concat [ be16 (uint32 first); be16 (uint32 GlyphCount); glyphClasses ]
        return
            { ClassCount = nClasses
              Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = first; Count = GlyphCount; Wide = false }
              States = Sealed.init cells.Length (fun i -> uint16 cells[i])
              StateBias = bias
              RowCount = rows
              Entries = Sealed.init pool.Length (fun i -> pool[i])
              IsExtended = extended }
    }

/// <summary>Generates the compiled ranges of a chain and the subtable flags: mostly one range, else several covering
/// every cluster.</summary>
/// <returns>The generator.</returns>
let private rangesGen =
    gen {
        let! several = Gen.frequency [ 4, Gen.constant false; 1, Gen.constant true ]
        match several with
        | false -> return struct (ImmutableArray<RangeFlags>.Empty, 1u)
        | true ->
            let! count = Gen.choose (2, 4)
            let! bounds = Gen.arrayOfLength (count - 1) (Gen.choose (0, 12))
            let sorted = Array.sort bounds
            let! flags = Gen.arrayOfLength count (Gen.elements [ 0u; 1u; 2u; 3u ])
            let! subtable = Gen.elements [ 1u; 2u; 3u ]
            let ranges =
                Array.init count (fun index ->
                    { Flags = flags[index]
                      ClusterFirst = (match index with | 0 -> 0u | _ -> uint32 sorted[index - 1] + 1u)
                      ClusterLast = (match index = count - 1 with | true -> UInt32.MaxValue | false -> uint32 sorted[index]) })
            return struct (ImmutableArray.CreateRange ranges, subtable)
    }

/// <summary>Builds the case of a kind from its generated parts.</summary>
/// <param name="kind">The kind.</param>
/// <param name="seed">The initial context.</param>
/// <param name="mask">The null-transition mask of the kind.</param>
/// <param name="quiet">Whether the null transition stands in for the transition of an entry that is not actionable.</param>
/// <param name="machine">The machine.</param>
/// <param name="ranges">The ranges and the subtable flags.</param>
/// <param name="run">The run.</param>
/// <returns>The case.</returns>
let private caseOf (kind: 'Kind) (seed: 'Context) (mask: uint16) (quiet: bool) (machine: StateMachine) (ranges: struct (ImmutableArray<RangeFlags> * uint32)) (run: GlyphRun) =
    let struct (compiled, subtable) = ranges
    { Config = { Machine = machine; Ranges = compiled; SubtableFlags = subtable; NumGlyphs = GlyphCount; StartSafeToBreakAtEndOfText = true }
      Kind = kind
      Seed = seed
      Mask = mask
      Quiet = quiet
      Run = run }

/// <summary>The gate of the contextual, ligature and insertion kinds' apply context.</summary>
let private applyContext: AatContext = { Gdef = Gdef.load Face.empty }

/// <summary>The property of one kind.</summary>
/// <param name="name">The test name.</param>
/// <param name="gen">The case generator.</param>
/// <param name="observe">The observable part of the context.</param>
/// <returns>The test.</returns>
let private kindProperty (name: string) (gen: Gen<Case<'Kind, 'Context>>) (observe: 'Context -> 'Observed) =
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } name <|
        Prop.forAll (Arb.fromGen gen) (fun case ->
            let difference = diagnose observe case
            Prop.label difference (difference.Length = 0))

/// <summary>The flags of an entry with the verb of a rearrangement, the marks and the epsilon bit.</summary>
let private rearrangementFlags =
    gen {
        let! verb = Gen.choose (0, 15)
        let! bits = Gen.elements [ 0us; 0us; 0x8000us; 0x2000us; 0xA000us; 0x4000us; 0x8000us; 0xE000us ]
        return bits ||| uint16 verb
    }

/// <summary>The generator of the rearrangement cases.</summary>
let private rearrangementGen =
    gen {
        let! machine = machineGen rearrangementFlags (Gen.constant 0us) (Gen.constant 0us) false
        let! ranges = rangesGen
        let! run = runGen false 12
        return caseOf (RearrangementKind()) MorxRearrangement.seed 0xA000us true machine ranges run
    }

/// <summary>The rearrangement property.</summary>
let private rearrangementTest =
    kindProperty "the kernel agrees with the interpreter on every field, rearrangement" rearrangementGen (fun _ -> 0)

/// <summary>The rearrangement property on the kernel that runs the transition directly.</summary>
let private rearrangementDirectTest =
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "the direct rearrangement kernel agrees with the interpreter on every field" <|
        Prop.forAll (Arb.fromGen rearrangementGen) (fun case ->
            let expected = MorxRearrangementDirect.run Reference ValueNone case.Config case.Run
            let dense = DenseMachine.build case.Config.Machine case.Config.NumGlyphs case.Kind
            let difference =
                match dense.IsSome, describe "general" expected (MorxRearrangementDirect.run Kernel ValueNone case.Config case.Run), describe "dense" expected (MorxRearrangementDirect.run Kernel dense case.Config case.Run) with
                | false, _, _ -> "the table did not compile"
                | true, difference, _ when difference.Length > 0 -> difference
                | true, _, difference when difference.Length > 0 -> difference
                | true, _, _ -> String.Empty
            Prop.label difference (difference.Length = 0))

/// <summary>The bytes of the contextual substitution tables of the obsolete form: 16-bit glyph offsets.</summary>
let private mortBodyGen = Gen.arrayOfLength 40 (Gen.elements [ 0us; 1us; 3us; 7us; 9us; 0xFFFFus; 12us; 0x8000us ]) |> Gen.map (Array.collect (fun value -> be16 (uint32 value)))

/// <summary>The bytes of the extended contextual substitution tables: an offset array and four format 0 lookups.</summary>
let private morxBodyGen =
    gen {
        let tableSize = 2 + 2 * GlyphCount
        let! tables = Gen.arrayOfLength 4 (Gen.arrayOfLength GlyphCount (Gen.elements [ 0us; 1us; 3us; 7us; 9us; 0xFFFFus; 5us ]))
        let offsets = Array.init 4 (fun index -> be32 (uint32 (16 + index * tableSize)))
        let lookups = tables |> Array.map (fun values -> Array.concat [ be16 0u; values |> Array.collect (fun value -> be16 (uint32 value)) ])
        return Array.concat [ Array.concat offsets; Array.concat lookups ]
    }

/// <summary>The generator of contextual cases, obsolete or extended.</summary>
/// <param name="extended">True for the extended form.</param>
/// <returns>The generator.</returns>
let private contextualGen (extended: bool) =
    let flags = Gen.elements [ 0us; 0us; 0x8000us; 0x4000us; 0xC000us ]
    let data = match extended with | true -> Gen.elements [ 0xFFFFus; 0xFFFFus; 0us; 1us; 2us; 3us ] | false -> Gen.elements [ 0us; 0us; 1us; 2us; 5us; 0xFFFFus; 30us ]
    gen {
        let! machine = machineGen flags data data extended
        let! ranges = rangesGen
        let! body = match extended with | true -> morxBodyGen | false -> mortBodyGen
        let! subsAt = match extended with | true -> Gen.constant 0 | false -> Gen.elements [ 0; 0; 8 ]
        let! collect = Gen.elements [ false; true ]
        let! run = runGen false 14
        let kind = { ContextualKind.Extended = extended; Body = ReadOnlyMemory body; SubstitutionTablesAt = subsAt; NumGlyphs = GlyphCount; Context = applyContext }
        return caseOf kind (MorxContextual.seed collect) 0x8000us extended machine ranges run
    }

/// <summary>The contextual property on the micro-step kernel, obsolete or extended.</summary>
/// <param name="extended">True for the extended form.</param>
/// <returns>The test.</returns>
let private contextualTest (extended: bool) =
    let name = match extended with | true -> "the kernel agrees with the interpreter on every field, contextual extended" | false -> "the kernel agrees with the interpreter on every field, contextual obsolete"
    kindProperty name (contextualGen extended) (fun (context: ContextualContext) -> struct (PointBuilder.publish context.Written |> Seq.toArray, context.HasDeleted))

/// <summary>The contextual property on the kernel that writes the substitutions directly, obsolete or extended.</summary>
/// <param name="extended">True for the extended form.</param>
/// <returns>The test.</returns>
let private contextualDirectTest (extended: bool) =
    let name = match extended with | true -> "the direct contextual kernel agrees with the interpreter on every field, extended" | false -> "the direct contextual kernel agrees with the interpreter on every field, obsolete"
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } name <|
        Prop.forAll (Arb.fromGen (contextualGen extended)) (fun case ->
            let collect = case.Seed.CollectWritten
            let struct (expected, expectedWritten, expectedDeleted) = MorxContextualDirect.run Reference ValueNone case.Config case.Kind collect case.Run
            let dense = DenseMachine.build case.Config.Machine case.Config.NumGlyphs case.Kind
            let struct (general, generalWritten, generalDeleted) = MorxContextualDirect.run Kernel ValueNone case.Config case.Kind collect case.Run
            let struct (compiled, compiledWritten, compiledDeleted) = MorxContextualDirect.run Kernel dense case.Config case.Kind collect case.Run
            let difference =
                match dense.IsSome, describe "general" expected general, describe "dense" expected compiled with
                | false, _, _ -> "the table did not compile"
                | true, difference, _ when difference.Length > 0 -> difference
                | true, _, difference when difference.Length > 0 -> difference
                | true, _, _ ->
                    match generalWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray() && compiledWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray(), generalDeleted = expectedDeleted && compiledDeleted = expectedDeleted with
                    | true, true -> String.Empty
                    | false, _ -> "the glyphs written differ"
                    | _, false -> "the deleted flag differs"
            Prop.label difference (difference.Length = 0))

/// <summary>The ligature action words, the component and ligature arrays, laid out after an 8-byte lead.</summary>
let private ligatureBodyGen =
    gen {
        let! actions = Gen.arrayOfLength 20 (gen {
            let! last = Gen.elements [ 0u; 0x80000000u; 0x80000000u ]
            let! store = Gen.elements [ 0u; 0x40000000u; 0x40000000u ]
            let! offset = Gen.elements [ 0u; 1u; 2u; 3u; 4u; 0x3FFFFFFFu; 0x3FFFFFFEu; 0x20000001u; 5u ]
            return last ||| store ||| offset })
        let! components = Gen.arrayOfLength 32 (Gen.elements [ 0us; 1us; 2us; 3us; 0x8000us; 0x4000us; 5us ])
        let! ligatures = Gen.arrayOfLength 32 (Gen.elements [ 0us; 1us; 4us; 9us; 0xFFFFus; 0x1234us ])
        return Array.concat [ Array.zeroCreate 8; actions |> Array.collect be32; components |> Array.collect (fun value -> be16 (uint32 value)); ligatures |> Array.collect (fun value -> be16 (uint32 value)) ]
    }

/// <summary>The generator of ligature cases, obsolete or extended.</summary>
/// <param name="extended">True for the extended form.</param>
/// <returns>The generator.</returns>
let private ligatureGen (extended: bool) =
    let flags = Gen.frequency [ 4, Gen.elements [ 0us; 0x8000us; 0xA000us; 0x2000us; 0x4000us; 0xE000us ]; 2, Gen.elements [ 0x8000us; 0x8000us ] ]
    let flagsFor =
        match extended with
        | true -> flags
        | false ->
            gen {
                let! bits = Gen.elements [ 0us; 0x8000us; 0x4000us; 0xC000us ]
                let! offset = Gen.elements [ 0us; 8us; 12us; 16us; 20us; 24us; 60us; 0x3FFFus ]
                return bits ||| offset
            }
    let data0 = match extended with | true -> Gen.elements [ 0us; 1us; 2us; 5us; 19us; 25us ] | false -> Gen.constant 0us
    let gen =
        gen {
            let! machine = machineGen flagsFor data0 (Gen.constant 0us) extended
            let! ranges = rangesGen
            let! body = ligatureBodyGen
            let! collect = Gen.elements [ false; true ]
            let! longRun = Gen.frequency [ 5, Gen.constant false; 1, Gen.constant true ]
            let! run = runGen false (match longRun with | true -> 90 | false -> 14)
            let kind =
                { LigatureKind.Extended = extended
                  Body = ReadOnlyMemory body
                  LigActionAt = 8L
                  ComponentAt = 88L
                  LigatureAt = 152L
                  Context = applyContext }
            return caseOf kind (MorxLigature.seed collect) 0x8000us true machine ranges run
        }
    gen

/// <summary>The ligature property on the micro-step kernel, obsolete or extended.</summary>
/// <param name="extended">True for the extended form.</param>
/// <returns>The test.</returns>
let private ligatureTest (extended: bool) =
    let name = match extended with | true -> "the kernel agrees with the interpreter on every field, ligature extended" | false -> "the kernel agrees with the interpreter on every field, ligature obsolete"
    kindProperty name (ligatureGen extended) (fun (context: LigatureContext) -> struct (PointBuilder.publish context.Written |> Seq.toArray, context.HasDeleted))

/// <summary>The ligature property on the kernel that runs the action list directly, obsolete or extended.</summary>
/// <param name="extended">True for the extended form.</param>
/// <returns>The test.</returns>
let private ligatureDirectTest (extended: bool) =
    let name = match extended with | true -> "the direct ligature kernel agrees with the interpreter on every field, extended" | false -> "the direct ligature kernel agrees with the interpreter on every field, obsolete"
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } name <|
        Prop.forAll (Arb.fromGen (ligatureGen extended)) (fun case ->
            let collect = case.Seed.CollectWritten
            let struct (expected, expectedWritten, expectedDeleted) = MorxLigatureDirect.run Reference ValueNone case.Config case.Kind collect case.Run
            let dense = DenseMachine.build case.Config.Machine case.Config.NumGlyphs case.Kind
            let struct (general, generalWritten, generalDeleted) = MorxLigatureDirect.run Kernel ValueNone case.Config case.Kind collect case.Run
            let struct (compiled, compiledWritten, compiledDeleted) = MorxLigatureDirect.run Kernel dense case.Config case.Kind collect case.Run
            let difference =
                match dense.IsSome, describe "general" expected general, describe "dense" expected compiled with
                | false, _, _ -> "the table did not compile"
                | true, difference, _ when difference.Length > 0 -> difference
                | true, _, difference when difference.Length > 0 -> difference
                | true, _, _ ->
                    match generalWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray() && compiledWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray(), generalDeleted = expectedDeleted && compiledDeleted = expectedDeleted with
                    | true, true -> String.Empty
                    | false, _ -> "the glyphs written differ"
                    | _, false -> "the deleted flag differs"
            Prop.label difference (difference.Length = 0))

/// <summary>The generator of the insertion cases: random machines whose entries insert before and after the mark and the current
/// glyph, with counts that run past the glyph list.</summary>
let private insertionGen =
    let flags =
        gen {
            let! current = Gen.frequency [ 6, Gen.choose (0, 3); 1, Gen.choose (0, 31) ]
            let! marked = Gen.frequency [ 6, Gen.choose (0, 3); 1, Gen.choose (0, 31) ]
            let! bits = Gen.elements [ 0us; 0x8000us; 0x4000us; 0x0800us; 0x0400us; 0x0C00us; 0xC000us; 0x8800us ]
            return bits ||| uint16 (current <<< 5) ||| uint16 marked
        }
    let data = Gen.elements [ 0xFFFFus; 0xFFFFus; 0us; 1us; 3us; 10us; 20us; 22us; 23us; 24us ]
    gen {
        let! machine = machineGen flags data data true
        let! ranges = rangesGen
        let! glyphs = Gen.arrayOfLength 24 (Gen.elements [ 1us; 2us; 3us; 0xFFFFus; 9us ])
        let! collect = Gen.elements [ false; true ]
        let! run = runGen false 12
        let kind = { InsertionKind.Body = ReadOnlyMemory (glyphs |> Array.collect (fun value -> be16 (uint32 value))); InsertionActionAt = 0L; Context = applyContext }
        return caseOf kind (MorxInsertion.seed collect) 0x8000us false machine ranges run
    }

/// <summary>The insertion property.</summary>
let private insertionTest =
    kindProperty "the kernel agrees with the interpreter on every field, insertion" insertionGen (fun (context: InsertionContext) -> struct (PointBuilder.publish context.Written |> Seq.toArray, context.HasDeleted))

/// <summary>The insertion property on the kernel that runs the transition directly.</summary>
let private insertionDirectTest =
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "the direct insertion kernel agrees with the interpreter on every field" <|
        Prop.forAll (Arb.fromGen insertionGen) (fun case ->
            let collect = case.Seed.CollectWritten
            let struct (expected, expectedWritten, expectedDeleted) = MorxInsertionDirect.run Reference ValueNone case.Config case.Kind collect case.Run
            let dense = DenseMachine.build case.Config.Machine case.Config.NumGlyphs case.Kind
            let struct (general, generalWritten, generalDeleted) = MorxInsertionDirect.run Kernel ValueNone case.Config case.Kind collect case.Run
            let struct (compiled, compiledWritten, compiledDeleted) = MorxInsertionDirect.run Kernel dense case.Config case.Kind collect case.Run
            let difference =
                match dense.IsSome, describe "general" expected general, describe "dense" expected compiled with
                | false, _, _ -> "the table did not compile"
                | true, difference, _ when difference.Length > 0 -> difference
                | true, _, difference when difference.Length > 0 -> difference
                | true, _, _ ->
                    match generalWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray() && compiledWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray(), generalDeleted = expectedDeleted && compiledDeleted = expectedDeleted with
                    | true, true -> String.Empty
                    | false, _ -> "the glyphs written differ"
                    | _, false -> "the deleted flag differs"
            Prop.label difference (difference.Length = 0))

/// <summary>The bytes of a lookup over the glyphs of a table: a format 0 array, a format 8 trimmed array or a format 6 list of
/// single values.</summary>
let private lookupBytesGen =
    let value = Gen.elements [ 0us; 1us; 3us; 7us; 9us; 0xFFFFus; 5us; 4us ]
    gen {
        let! format = Gen.elements [ 0; 6; 8 ]
        match format with
        | 0 ->
            let! values = Gen.arrayOfLength GlyphCount value
            return Array.concat [ be16 0u; values |> Array.collect (fun v -> be16 (uint32 v)) ]
        | 8 ->
            let! first = Gen.choose (0, GlyphCount - 1)
            let! count = Gen.choose (0, GlyphCount + 2)
            let! values = Gen.arrayOfLength count value
            return Array.concat [ be16 8u; be16 (uint32 first); be16 (uint32 count); values |> Array.collect (fun v -> be16 (uint32 v)) ]
        | _ ->
            let! glyphs = Gen.arrayOfLength 6 (Gen.choose (0, GlyphCount + 2)) |> Gen.map (Array.distinct >> Array.sort)
            let! values = Gen.arrayOfLength glyphs.Length value
            let units = Array.map2 (fun glyph v -> Array.concat [ be16 (uint32 glyph); be16 (uint32 v) ]) glyphs values
            return Array.concat [ be16 6u; be16 4u; be16 (uint32 glyphs.Length); be16 0u; be16 0u; be16 0u; Array.concat units ]
    }

/// <summary>The noncontextual property: the kernel that writes the replacements in one loop, over the lookup as it is and over the
/// replacements compiled with the face, equals the reference that builds the run again, on every field and on the glyphs
/// written.</summary>
let private noncontextualTest =
    let gen =
        gen {
            let! bytes = lookupBytesGen
            let! ranges = rangesGen
            let! usingSet = Gen.elements [ false; true ]
            let! run = runGen true 14
            return struct (bytes, ranges, usingSet, run)
        }
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "the noncontextual kernel agrees with the reference on every field" <|
        Prop.forAll (Arb.fromGen gen) (fun (struct (bytes, struct (ranges, subFlags), usingSet, run)) ->
            let lookup = AatLookup.read (ReadOnlyMemory bytes) 0 2
            let compiled = Morx.compileReplacements (ReadOnlyMemory bytes) GlyphCount
            let struct (expected, expectedWritten) = MorxNoncontextualDirect.run Reference ValueNone lookup GlyphCount ranges subFlags applyContext usingSet run
            let struct (general, generalWritten) = MorxNoncontextualDirect.run Kernel ValueNone lookup GlyphCount ranges subFlags applyContext usingSet run
            let struct (dense, denseWritten) = MorxNoncontextualDirect.run Kernel compiled lookup GlyphCount ranges subFlags applyContext usingSet run
            let difference =
                match compiled.IsSome, describe "general" expected general, describe "dense" expected dense with
                | false, _, _ -> "the replacements did not compile"
                | true, difference, _ when difference.Length > 0 -> difference
                | true, _, difference when difference.Length > 0 -> difference
                | true, _, _ ->
                    match generalWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray() && denseWritten.AsSpan().ToArray() = expectedWritten.AsSpan().ToArray() with
                    | true -> String.Empty
                    | false -> "the glyphs written differ"
            Prop.label difference (difference.Length = 0))

/// <summary>The font the kerx machines read their scale from.</summary>
let private kerxFont = Font.create Face.empty

/// <summary>The kerx and kern format 1 property.</summary>
/// <param name="extended">True for kerx, false for the obsolete kern.</param>
/// <returns>The test.</returns>
let private format1Test (extended: bool) =
    let flags = Gen.elements [ 0us; 0us; 0x8000us; 0x4000us; 0xC000us; 0x2000us ]
    let flagsFor =
        match extended with
        | true -> flags
        | false ->
            gen {
                let! bits = Gen.elements [ 0us; 0x8000us; 0x4000us; 0xC000us ]
                let! offset = Gen.elements [ 0us; 2us; 4us; 6us; 0x3FFFus ]
                return bits ||| offset
            }
    let data0 = match extended with | true -> Gen.elements [ 0xFFFFus; 0xFFFFus; 0us; 1us; 2us; 3us; 40us ] | false -> Gen.constant 0us
    let gen =
        gen {
            let! machine = machineGen flagsFor data0 (Gen.constant 0us) extended
            let! ranges = Gen.constant (struct (ImmutableArray<RangeFlags>.Empty, 1u))
            let! values = Gen.arrayOfLength 16 (Gen.elements [ -1; -1; 0; 1; 2; 3; 40; -40; 501; -0x8000; 0x7FFF; 255 ])
            let body = values |> Array.collect (fun value -> be16 (uint32 (uint16 value)))
            let! rangeEnd = Gen.elements [ body.Length; body.Length; body.Length; body.Length - 2; body.Length - 5; 8; 3; 0 ]
            let! kernActionAt = Gen.elements [ 0; 2; 4; 6 ]
            let! tupleCount = match extended with | true -> Gen.elements [ 0u; 0u; 1u; 2u; 3u ] | false -> Gen.constant 0u
            let! horizontal = Gen.elements [ true; false ]
            let! crossStream = Gen.elements [ false; false; true ]
            let! run = runGen true 14
            let spec = { Extended = extended; TableBytes = ReadOnlyMemory body; RangeEnd = rangeEnd; MachineAt = 0; KernActionAt = kernActionAt; TupleCount = tupleCount }
            let kind: KerxFormat1Kind = { Spec = spec; Font = kerxFont; Horizontal = horizontal; CrossStream = crossStream; KernMask = KernMaskBit }
            return caseOf kind (KerxStateMachines.seed ()) 0x8000us true machine ranges run
        }
    let name = match extended with | true -> "the kernel agrees with the interpreter on every field, kerx format 1" | false -> "the kernel agrees with the interpreter on every field, kern format 1"
    kindProperty name gen (fun (_: KerxFormat1Context) -> 0)

/// <summary>The kerx format 4 property for one action type.</summary>
/// <param name="actionType">The action type.</param>
/// <returns>The test.</returns>
let private format4Gen (actionType: int) =
    let flags = Gen.elements [ 0us; 0us; 0x8000us; 0x8000us; 0x4000us; 0xC000us; 0x2000us ]
    let data0 = Gen.elements [ 0xFFFFus; 0xFFFFus; 0us; 1us; 2us; 3us; 40us ]
    let model = Map.ofList [ 1u, [| (100, 200); (30, 50) |]; 3u, [| (7, -9) |]; 5u, [| (-4, 12); (0, 0); (9, 9) |] ]
    let ankrFont =
        let file = sfnt 0x00010000u [ Tags.Ankr, ankr Dense 16 model ]
        Font.withScale 1000 1000 (Font.create (Face.withGlyphCount 16 (Face.create (ReadOnlyMemory file) 0u)))
    let gen =
        gen {
            let! machine = machineGen flags data0 (Gen.constant 0us) true
            let! ranges = Gen.constant (struct (ImmutableArray<RangeFlags>.Empty, 1u))
            let! words = Gen.arrayOfLength 16 (Gen.elements [ 0; 1; 2; 3; 7; 40; 0xFFFF; 0x7FFF; 0x8000; 501; 0xFFFB ])
            let body = words |> Array.collect (fun value -> be16 (uint32 value))
            let! rangeEnd = Gen.elements [ body.Length; body.Length; body.Length; body.Length - 2; body.Length - 5; 8; 3; 0 ]
            let! actionsAt = Gen.elements [ 0L; 0L; 2L; 4L; 6L; 8L ]
            let! horizontal = Gen.elements [ true; false ]
            let! reversed = Gen.elements [ true; false ]
            let! run = runGen true 14
            let spec = { TableBytes = ReadOnlyMemory body; RangeEnd = rangeEnd; ActionsAt = actionsAt; ActionType = actionType }
            let font = match actionType with | 1 -> ankrFont | _ -> kerxFont
            let kind: KerxFormat4Kind = { Spec = spec; Ankr = font.Ot.Ankr.Value; Font = font; Horizontal = horizontal; Reversed = reversed }
            return caseOf kind (KerxFormat4Machine.seed ()) KerxFormat4Machine.MarkFlag true machine ranges run
        }
    gen

/// <summary>The format 4 property on the kernel that runs the transition straight on the working copy, for one action type.</summary>
/// <param name="actionType">The action type.</param>
/// <returns>The test.</returns>
let private format4DirectTest (actionType: int) =
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } $"the direct kerx format 4 kernel agrees with the interpreter on every field, action type {actionType}" <|
        Prop.forAll (Arb.fromGen (format4Gen actionType)) (fun case ->
            let kind: KerxFormat4Kind = case.Kind
            let machine = case.Config.Machine
            let apply runner dense = KerxFormat4Apply.applyWith runner dense kind.Spec machine kind.Font kind.Horizontal kind.Reversed case.Run
            let dense = DenseMachine.build machine case.Config.NumGlyphs kind
            let expected = apply Reference ValueNone
            let difference =
                match dense.IsSome, describe "general" expected (apply Kernel ValueNone), describe "dense" expected (apply Kernel dense) with
                | false, _, _ -> "the table did not compile"
                | true, difference, _ when difference.Length > 0 -> difference
                | true, _, difference when difference.Length > 0 -> difference
                | true, _, _ -> String.Empty
            Prop.label difference (difference.Length = 0))

/// <summary>The format 4 property, for one action type.</summary>
/// <param name="actionType">The action type.</param>
/// <returns>The test.</returns>
let private format4Test (actionType: int) =
    kindProperty $"the kernel agrees with the interpreter on every field, kerx format 4 action type {actionType}" (format4Gen actionType) (fun (_: KerxFormat4Context) -> 0)

/// <summary>A table whose entries never act and whose states all loop on themselves, with a long stretch of one class.</summary>
/// <returns>The case.</returns>
let private nullCase (count: int) (maxOps: int) =
    let entries = Array.init 5 (fun _ -> { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us })
    let classBytes = Array.init (4 + 128) (fun i -> if i < 4 then 0uy else 4uy)
    let machine =
        { ClassCount = 5
          Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = 128; Wide = false }
          States = Sealed.init 5 (fun i -> uint16 i)
          StateBias = 0
          RowCount = 1
          Entries = Sealed.init 5 (fun i -> entries[i])
          IsExtended = false }
    let infos = Sealed.init count (fun i -> { Codepoint = uint32 (i % 128); Mask = 3u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    let run = { GlyphRun.empty with Infos = infos; Props = props; MaxOps = maxOps }
    caseOf (RearrangementKind()) MorxRearrangement.seed 0xA000us true machine (struct (ImmutableArray<RangeFlags>.Empty, 1u)) run

/// <summary>The property that the sorted union of adds builds the same ranges as the walk over a list: a delete of a value
/// no add reaches sends the same operations down the list walk.</summary>
let private rangeSetTest =
    let opGen =
        gen {
            let! lo = Gen.frequency [ 8, Gen.choose (0, 40); 1, Gen.elements [ 0xFFFFFFFEu |> int; 0x10FFFF; 0x7FFFFFF0 ] ]
            let! span = Gen.frequency [ 6, Gen.constant 0; 3, Gen.choose (1, 6); 1, Gen.choose (-2, 0) ]
            let! invalid = Gen.frequency [ 30, Gen.constant false; 1, Gen.constant true ]
            let first = uint32 lo
            let last = match invalid with | true -> UInt32.MaxValue | false -> uint32 (max 0 (lo + span))
            return SetOp.Add (first, last)
        }
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "the union of adds equals the list walk" <|
        Prop.forAll (Arb.fromGen (Gen.listOf opGen)) (fun ops ->
            let adds = List.toArray ops
            let withDelete = Array.append adds [| SetOp.Delete (0x7FFFFFFEu, 0x7FFFFFFEu) |]
            let fast = Sealed.rangeSet (ReadOnlySpan adds)
            let slow = Sealed.rangeSet (ReadOnlySpan withDelete)
            let overlapped = adds |> Array.exists (fun op -> match op with | SetOp.Add (lo, hi) -> lo <= 0x7FFFFFFEu && 0x7FFFFFFEu <= hi && lo <= hi | SetOp.Delete _ -> false)
            overlapped || fast.AsSpan().ToArray() = slow.AsSpan().ToArray())

/// <summary>The property that the compiled pair index finds what a map of the pairs finds, for every pair a table lists and
/// for pairs it does not, and declines a table whose pairs do not strictly increase.</summary>
let private pairIndexTest =
    let pairGen =
        gen {
            let! left = Gen.frequency [ 8, Gen.choose (0, 40); 1, Gen.elements [ 0xFFFF; 0x10000; 70000 ] ]
            let! right = Gen.frequency [ 8, Gen.choose (0, 40); 1, Gen.elements [ 0xFFFF; 0x10000; 70000 ] ]
            let! value = Gen.elements [ 0s; 1s; -1s; 40s; -40s; Int16.MinValue; Int16.MaxValue ]
            return struct (left, right, value)
        }
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "the pair index finds what a map of the pairs finds" <|
        Prop.forAll (Arb.fromGen (Gen.zip (Gen.listOf pairGen) (Gen.elements [ true; true; false ]))) (fun (pairs, sorted) ->
            let valid = pairs |> List.filter (fun (struct (left, right, _)) -> left <= 0xFFFF && right <= 0xFFFF)
            let ordered =
                match sorted with
                | true -> valid |> List.distinctBy (fun (struct (left, right, _)) -> left, right) |> List.sortBy (fun (struct (left, right, _)) -> left, right)
                | false -> valid
            let keys = ordered |> List.map (fun (struct (left, right, _)) -> (uint32 left <<< 16) ||| uint32 right) |> List.toArray
            let values = ordered |> List.map (fun (struct (_, _, value)) -> value) |> List.toArray
            let increasing = keys |> Array.pairwise |> Array.forall (fun (a, b) -> b > a)
            match PairLookup.build (ReadOnlySpan keys) (ReadOnlySpan values), increasing with
            | ValueNone, false -> Prop.ofTestable true
            | ValueNone, true -> Prop.label "a sorted table did not compile" false
            | ValueSome _, false -> Prop.label "an unsorted table compiled" false
            | ValueSome index, true ->
                let expected = Array.zip keys values |> Map.ofArray
                let probes = [ for left in 0 .. 42 do for right in 0 .. 42 -> (uint32 left, uint32 right) ] @ [ 0xFFFFu, 0xFFFFu; 0x10000u, 1u; 1u, 0x10000u; 70000u, 70000u ]
                let wrong =
                    probes
                    |> List.filter (fun (left, right) ->
                        let want = match Map.tryFind ((left <<< 16) ||| right) expected with | Some value -> int value | None -> 0
                        left > 0xFFFFu || right > 0xFFFFu || PairLookup.find index left right <> want)
                    |> List.filter (fun (left, right) -> not (left > 0xFFFFu || right > 0xFFFFu) || PairLookup.find index left right <> 0)
                let leftsWrong =
                    [ 0u .. 45u ] |> List.filter (fun glyph -> PairLookup.hasLeft index glyph <> (keys |> Array.exists (fun key -> key >>> 16 = glyph)))
                Prop.label $"{wrong.Length} pairs and {leftsWrong.Length} left glyphs differ" (wrong.IsEmpty && leftsWrong.IsEmpty))

/// <summary>The binary search of a pair table as <c>hb_bsearch</c> runs it, the middle of the candidates first.</summary>
/// <param name="keys">The keys in the table's order.</param>
/// <param name="values">The values.</param>
/// <param name="key">The key.</param>
/// <returns>The value of the entry the search ends on, zero when it finds none.</returns>
let private searchedValue (keys: uint32 array) (values: int16 array) (key: uint32) =
    let rec search lo hi =
        match lo > hi with
        | true -> 0
        | false ->
            let mid = int ((uint32 lo + uint32 hi) >>> 1)
            match keys[mid] > key, keys[mid] < key with
            | true, _ -> search lo (mid - 1)
            | false, true -> search (mid + 1) hi
            | false, false -> int values[mid]
    search 0 (keys.Length - 1)

/// <summary>The compiled pair index of a table in any order, with duplicate keys, finds what the binary search over the table finds.</summary>
let private searchedPairIndexTest =
    let pairGen =
        gen {
            let! left = Gen.choose (0, 30)
            let! right = Gen.choose (0, 30)
            let! value = Gen.elements [ 0s; 1s; -1s; 40s; -40s; Int16.MinValue; Int16.MaxValue ]
            return struct (left, right, value)
        }
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "the searched pair index finds what the binary search over the table finds" <|
        Prop.forAll (Arb.fromGen (Gen.zip (Gen.listOf pairGen) (Gen.elements [ 0; 1; 2 ]))) (fun (pairs, order) ->
            let ordered =
                match order with
                | 0 -> pairs
                | 1 -> pairs |> List.sortBy (fun (struct (left, right, _)) -> left, right)
                | _ -> pairs |> List.sortBy (fun (struct (left, right, _)) -> left, right) |> List.collect (fun pair -> [ pair; pair ])
            let keys = ordered |> List.map (fun (struct (left, right, _)) -> (uint32 left <<< 16) ||| uint32 right) |> List.toArray
            let values = ordered |> List.map (fun (struct (_, _, value)) -> value) |> List.toArray
            match PairLookup.compile (ReadOnlySpan keys) (ReadOnlySpan values) with
            | ValueNone -> Prop.label "the table did not compile" false
            | ValueSome index ->
                let probes = [ for left in 0 .. 32 do for right in 0 .. 32 -> (uint32 left, uint32 right) ]
                let wrong = probes |> List.filter (fun (left, right) -> PairLookup.find index left right <> searchedValue keys values ((left <<< 16) ||| right))
                let leftsWrong = probes |> List.filter (fun (left, right) -> not (PairLookup.hasLeft index left) && PairLookup.find index left right <> 0)
                Prop.label $"{wrong.Length} pairs and {leftsWrong.Length} left glyphs differ" (wrong.IsEmpty && leftsWrong.IsEmpty))

/// <summary>The property that the glyph set of a run, the glyphs added to it and the bit table of a chain agree with the set
/// operations they stand for.</summary>
let private glyphSetTest =
    let glyphGen = Gen.frequency [ 8, Gen.choose (0, 30); 1, Gen.elements [ 0xFFFF; 0x10000; 0x7FFFFFFF ] ] |> Gen.map uint32
    let infoOf (glyph: uint32) = { Codepoint = glyph; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u }
    let setsGen =
        gen {
            let! count = Gen.choose (1, 70)
            let! sets = Gen.listOfLength count (Gen.listOf (Gen.zip (Gen.choose (0, 34)) (Gen.choose (0, 3))))
            return sets |> List.map (fun ranges -> ranges |> List.map (fun (first, span) -> { First = uint32 first; Last = uint32 (first + span) }))
        }
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "the glyph ranges, the added glyphs and the bit table agree with the set operations" <|
        Prop.forAll (Arb.fromGen (Gen.zip3 (Gen.listOf glyphGen) (Gen.listOf glyphGen) setsGen)) (fun (originals, added, sets) ->
            let infos = originals |> List.map infoOf |> List.toArray
            let adds (glyphs: uint32 list) = glyphs |> List.map (fun glyph -> SetOp.Add (glyph, glyph)) |> List.toArray
            let ranges = Sealed.glyphRanges (ReadOnlySpan infos)
            let rangesExpected = Sealed.rangeSet (ReadOnlySpan (adds originals))
            let grown = Sealed.rangesWithGlyphs (ranges.AsSpan()) (ReadOnlySpan (List.toArray added))
            let grownExpected = Sealed.rangeSet (ReadOnlySpan (adds (originals @ added)))
            let setArrays = sets |> List.map (fun set -> set |> List.sortBy (fun range -> range.First) |> List.fold (fun (acc: CodepointRange list) range -> match acc with | previous :: rest when range.First <= previous.Last + 1u -> { previous with Last = max previous.Last range.Last } :: rest | _ -> range :: acc) [] |> List.rev |> ImmutableArray.CreateRange) |> ImmutableArray.CreateRange
            let words = (setArrays.Length + 63) / 64
            let top = 40
            let bits = Sealed.subtableBits setArrays words top
            let zero = ImmutableArray.CreateRange (List.replicate words 0UL)
            let candidates = Sealed.candidateBitsOfGlyphs (Sealed.candidateBits zero bits top (ReadOnlySpan infos)) bits top (ReadOnlySpan (List.toArray added))
            let present = originals @ added
            let bitsWrong =
                [ 0 .. setArrays.Length - 1 ]
                |> List.filter (fun subtable ->
                    let wanted = present |> List.exists (fun glyph -> setArrays[subtable] |> Seq.exists (fun range -> range.First <= glyph && glyph <= range.Last))
                    let found = (candidates[subtable / 64] >>> (subtable % 64)) &&& 1UL <> 0UL
                    wanted <> found)
            let valid (glyphs: uint32 list) = glyphs |> List.filter (fun glyph -> glyph <> UInt32.MaxValue)
            match ranges.AsSpan().ToArray() = rangesExpected.AsSpan().ToArray(), grown.AsSpan().ToArray() = grownExpected.AsSpan().ToArray(), bitsWrong.IsEmpty, (valid originals).Length >= 0 with
            | true, true, true, _ -> Prop.ofTestable true
            | false, _, _, _ -> Prop.label "the glyph ranges differ" false
            | _, false, _, _ -> Prop.label "the grown ranges differ" false
            | _, _, false, _ -> Prop.label $"the bits differ at subtables {bitsWrong}" false)

/// <summary>The machine tests that are not random.</summary>
let private allocationTests =
    testList "allocation" [
        test "a kernel pass that changes nothing allocates nothing and returns the same instance" {
            let case = nullCase 1000 65536
            let dense = DenseMachine.build case.Config.Machine case.Config.NumGlyphs case.Kind
            let run () = AatKernel.run Kernel dense case.Config case.Kind case.Mask case.Quiet case.Seed case.Run |> TailCalls.settle
            let struct (result, _) = run ()
            Expect.isTrue (result.Infos = case.Run.Infos) "the same infos array comes back"
            let measured () = run () |> ignore
            measured ()
            Expect.equal (bytesAllocatedBy measured) 0L "a pass that changes nothing allocates no bytes"
        }
        test "a ligature pass that forms nothing allocates nothing and returns the same instance" {
            let case = nullCase 1000 65536
            let kind = { LigatureKind.Extended = true; Body = ReadOnlyMemory Array.empty; LigActionAt = 0L; ComponentAt = 0L; LigatureAt = 0L; Context = applyContext }
            let dense = DenseMachine.build case.Config.Machine case.Config.NumGlyphs kind
            let run () = MorxLigatureDirect.run Kernel dense case.Config kind true case.Run |> TailCalls.settle
            let struct (result, written, deleted) = run ()
            Expect.isTrue (result.Infos = case.Run.Infos) "the same infos array comes back"
            Expect.isTrue (written.IsEmpty && not deleted) "nothing is written"
            let measured () = run () |> ignore
            measured ()
            Expect.equal (bytesAllocatedBy measured) 0L "a ligature pass that forms nothing allocates no bytes"
        }
        test "a noncontextual pass that replaces nothing allocates nothing, and one that replaces every glyph allocates the output arrays" {
            let count = 1000
            let infos = Sealed.init count (fun index -> { Codepoint = uint32 (index % 8); Mask = 3u; Cluster = uint32 index; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
            let run = { GlyphRun.empty with Infos = infos; Props = props; MaxOps = 65536 }
            let none = Array.concat [ be16 8u; be16 0u; be16 0u ]
            let noneLookup = AatLookup.read (ReadOnlyMemory none) 0 2
            let noneReplacements = Morx.compileReplacements (ReadOnlyMemory none) 8
            let every = Array.concat [ be16 0u; Array.concat (Array.init 8 (fun glyph -> be16 (uint32 (glyph + 1)))) ]
            let everyLookup = AatLookup.read (ReadOnlyMemory every) 0 2
            let everyReplacements = Morx.compileReplacements (ReadOnlyMemory every) 8
            let nothing () = MorxNoncontextualDirect.run Kernel noneReplacements noneLookup 8 ImmutableArray.Empty 1u applyContext true run |> TailCalls.settle
            let struct (same, written) = nothing ()
            Expect.isTrue (obj.ReferenceEquals (same, run)) "the same run comes back"
            Expect.isTrue written.IsEmpty "nothing is written"
            let measuredNothing () = nothing () |> ignore
            measuredNothing ()
            Expect.equal (bytesAllocatedBy measuredNothing) 0L "a pass that replaces nothing allocates no bytes"
            let everything () = MorxNoncontextualDirect.run Kernel everyReplacements everyLookup 8 ImmutableArray.Empty 1u applyContext false run |> TailCalls.settle
            let struct (replaced, _) = everything ()
            Expect.equal (replaced.Infos[3].Codepoint) 4u "a glyph is replaced"
            let measuredEvery () = everything () |> ignore
            measuredEvery ()
            let bytes = bytesAllocatedBy measuredEvery
            Expect.isLessThan bytes (int64 (count * (Runtime.CompilerServices.Unsafe.SizeOf<GlyphInfo>() + Runtime.CompilerServices.Unsafe.SizeOf<GlyphProps>())) + 400L) "a pass that replaces every glyph allocates the two output arrays and a constant"
        }
    ]

/// <summary>The directories of the upstream corpus that hold AAT test fonts.</summary>
let private corpusDirectories =
    [ "test/shape/data/in-house/fonts"; "test/shape/data/text-rendering-tests/fonts"; "test/api/fonts"; "test/subset/data/fonts" ]

/// <summary>Whether a font file holds one of the AAT tables, read from the table directory.</summary>
/// <param name="bytes">The font file.</param>
/// <param name="tags">The tags that count.</param>
/// <returns>True when the font has one of the tags.</returns>
let private hasTable (bytes: byte array) (tags: string array) =
    match bytes.Length >= 12 && bytes[0] <> byte 't' with
    | false -> false
    | true ->
        let count = int bytes[4] * 256 + int bytes[5]
        Array.init count (fun index -> 12 + 16 * index)
        |> Array.exists (fun at -> at + 4 <= bytes.Length && Array.contains (String [| char bytes[at]; char bytes[at + 1]; char bytes[at + 2]; char bytes[at + 3] |]) tags)

/// <summary>The AAT test fonts of the corpus with their bytes.</summary>
let private corpusFonts (tags: string array) =
    corpusDirectories
    |> List.collect (fun directory ->
        let path = Path.Combine(RepositoryPaths.upstream (), directory)
        match Directory.Exists path with
        | false -> []
        | true ->
            Directory.GetFiles(path)
            |> Array.filter (fun file -> file.EndsWith ".ttf" || file.EndsWith ".otf")
            |> Array.sort
            |> Array.toList
            |> List.choose (fun file ->
                let bytes = File.ReadAllBytes file
                match hasTable bytes tags with
                | true -> Some (FileInfo(file).Name, bytes)
                | false -> None))

/// <summary>Random glyph runs over the glyphs of a face.</summary>
/// <param name="rnd">The random source.</param>
/// <param name="glyphCount">The glyph count.</param>
/// <param name="kerning">Whether the run carries positions and the kern mask.</param>
/// <returns>The run.</returns>
let private corpusRun (rnd: Random) (glyphCount: int) (kerning: bool) =
    let count = rnd.Next(0, 30)
    let glyphs = Array.init count (fun _ -> uint32 (rnd.Next(0, max 1 glyphCount)))
    let infos = Sealed.init count (fun i -> { Codepoint = glyphs[i]; Mask = (match kerning with | true -> KernMaskBit | false -> 0u); Cluster = uint32 (i - i % (1 + rnd.Next 2)); Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    let positions = Sealed.init count (fun _ -> { XAdvance = 100 + rnd.Next 600; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = kerning
        MaxOps = max 8192 (count * 64)
        SegmentProperties = { Direction = (match rnd.Next 4 with | 0 -> Direction.Rtl | _ -> Direction.Ltr); Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>The plan that applies the kerx table.</summary>
let private kerxPlan: OtShapePlan =
    { Props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
      Shaper = Categorize.profile DefaultShaper
      Map = OtMap.empty
      Data = NoShaperData
      UserFeatures = ImmutableArray.Empty
      FracMask = 0u
      NumrMask = 0u
      DnomMask = 0u
      RtlmMask = 0u
      KernMask = KernMaskBit
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

/// <summary>The number of random runs each corpus font is run over.</summary>
[<Literal>]
let private RunsPerFont = 150

/// <summary>The tests on the real AAT fonts of the corpus.</summary>
let private corpusTests =
    testList "corpus" [
        test "the kernel agrees with the interpreter on the morx fonts of the corpus" {
            let fonts = corpusFonts [| "morx"; "mort" |]
            let changed = ConcurrentBag<int>()
            Expect.isGreaterThan fonts.Length 20 "the corpus holds morx fonts"
            let applied =
                fonts
                |> List.map (fun (name, bytes) ->
                    let face = OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let table = match Morx.hasData face.Morx.Value with | true -> face.Morx.Value | false -> face.Mort.Value
                    let rnd = Random(name.GetHashCode() &&& 0xFFFF)
                    let mutable differing = 0
                    for _ in 1 .. RunsPerFont do
                        let run = corpusRun rnd face.Face.GlyphCount false
                        let segment = run.SegmentProperties
                        let map = AatMap.defaults face segment
                        let expected = MorxApply.applyWith Reference table map face segment run
                        let actual = MorxApply.applyWith Kernel table map face segment run
                        if fieldsOf expected <> fieldsOf actual then differing <- differing + 1
                        if fieldsOf expected <> fieldsOf run then changed.Add 1
                    struct (name, differing))
            Expect.equal (applied |> List.filter (fun (struct (_, differing)) -> differing > 0) |> List.map (fun (struct (name, differing)) -> $"{name}: {differing} runs differ")) [] "no morx font shapes differently"
            Expect.isGreaterThan changed.Count 200 "the morx subtables of the corpus change many of the random runs"
        }
        test "the kernel agrees with the interpreter on the kerx fonts of the corpus" {
            let fonts = corpusFonts [| "kerx" |]
            let changed = ConcurrentBag<int>()
            Expect.isGreaterThan fonts.Length 10 "the corpus holds kerx fonts"
            let applied =
                fonts
                |> List.map (fun (name, bytes) ->
                    let face = OtFace.create (Face.create (ReadOnlyMemory bytes) 0u)
                    let font = Font.create face.Face
                    let rnd = Random(name.GetHashCode() &&& 0xFFFF)
                    let mutable differing = 0
                    for _ in 1 .. RunsPerFont do
                        let run = corpusRun rnd face.Face.GlyphCount true
                        let expected = KerxPairs.applyWith Reference kerxPlan font run
                        let actual = KerxPairs.applyWith Kernel kerxPlan font run
                        if fieldsOf expected <> fieldsOf actual then differing <- differing + 1
                        if fieldsOf expected <> fieldsOf run then changed.Add 1
                    struct (name, differing))
            Expect.equal (applied |> List.filter (fun (struct (_, differing)) -> differing > 0) |> List.map (fun (struct (name, differing)) -> $"{name}: {differing} runs differ")) [] "no kerx font shapes differently"
            Expect.isGreaterThan changed.Count 50 "the kerx subtables of the corpus change some of the random runs"
        }
    ]

/// <summary>The AAT kernel tests.</summary>
[<Tests>]
let tests =
    testList "AAT kernel" [
        rearrangementTest
        rearrangementDirectTest
        contextualTest false
        contextualTest true
        contextualDirectTest false
        contextualDirectTest true
        ligatureTest false
        ligatureTest true
        ligatureDirectTest false
        ligatureDirectTest true
        insertionTest
        insertionDirectTest
        noncontextualTest
        format1Test true
        format1Test false
        format4Test 0
        format4Test 1
        format4Test 2
        format4DirectTest 0
        format4DirectTest 1
        format4DirectTest 2
        format4DirectTest 3
        rangeSetTest
        pairIndexTest
        searchedPairIndexTest
        glyphSetTest
        allocationTests
        corpusTests
    ]
