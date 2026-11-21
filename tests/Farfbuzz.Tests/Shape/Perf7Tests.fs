module Farfbuzz.Tests.Shape.Perf7Tests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open Farfbuzz
open Farfbuzz.Harness

/// <summary>Builds a run's glyph infos from an int array: code point, cluster and mask derived from each value.</summary>
/// <param name="values">The source values.</param>
/// <returns>The infos.</returns>
let private infosOf (values: int array) =
    Sealed.init values.Length (fun index ->
        { Codepoint = uint32 (abs (values[index] % 0x110000))
          Mask = uint32 (values[index] % 16)
          Cluster = uint32 (max 0 (index - (values[index] % 3)))
          Var1 = 0u
          Var2 = 0u })

/// <summary>Builds a run's glyph properties from an int array.</summary>
/// <param name="values">The source values.</param>
/// <returns>The properties.</returns>
let private propsOf (values: int array) =
    Sealed.init values.Length (fun index ->
        { Unchecked.defaultof<GlyphProps> with GlyphClass = uint16 (values[index] % 4); GlyphIndex = uint32 index })

/// <summary>Builds the run fields a program reads from an int array.</summary>
/// <param name="values">The source values.</param>
/// <returns>The run fields.</returns>
let private runInputOf (values: int array) : RunInput =
    { Infos = infosOf values
      Props = propsOf values
      Positions = ImmutableArray.Empty
      HavePositions = false
      MaxLen = GlyphRun.MaxLenDefault
      MaxOps = GlyphRun.MaxOpsDefault
      Successful = true
      ClusterLevel = ClusterLevel.MonotoneGraphemes
      Flags = BufferFlags.Default
      Serial = 0uy
      RandomState = 1u
      ScratchFlags = ScratchFlags.Default }

/// <summary>Adds a constant to every glyph's mask, one pure info-props map step.</summary>
[<Struct>]
type MaskAddMap =
    val Amount: uint32
    new (amount) = { Amount = amount }
    interface IInfoPropsMap with
        member this.Map (info, props) = struct ({ info with Mask = info.Mask + this.Amount }, props)
    interface Sealed.IStage with
        member this.Run (sink: byref<Sealed.StageSink>) = Sealed.mapInfoProps &sink this

/// <summary>A per-glyph position map that adds a constant to every X advance.</summary>
[<Struct>]
type AdvanceAddMap =
    val Amount: int
    new (amount) = { Amount = amount }
    interface IPositionMap with
        member this.Map (_, _, position) = { position with XAdvance = position.XAdvance + this.Amount }
    interface Sealed.IStage with
        member this.Run (sink: byref<Sealed.StageSink>) = Sealed.mapPositions &sink this

/// <summary>A pass that sets one glyph's mask without touching positions.</summary>
[<Struct>]
type MaskPass =
    interface IRunPass<bool> with
        member _.Step (_, done_) =
            match done_ with
            | true -> struct (RunEdit.Finish, true)
            | false -> struct (RunEdit.OrMask (1u, 0, 1), true)

/// <summary>Wraps a run pass and its seed as one program stage.</summary>
[<Struct>]
type internal PassStage<'P, 'S when 'P : struct and 'P :> IRunPass<'S>> =
    val Pass: 'P
    val Seed: 'S
    new (pass, seed) = { Pass = pass; Seed = seed }
    interface Sealed.IStage with
        member this.Run (sink: byref<Sealed.StageSink>) = Sealed.runPassStage &sink this.Pass this.Seed

/// <summary>An identity info-props map: every glyph and property comes back unchanged.</summary>
[<Struct>]
type IdentityInfoPropsStage =
    interface IInfoPropsMap with
        member _.Map (info, props) = struct (info, props)
    interface Sealed.IStage with
        member this.Run (sink: byref<Sealed.StageSink>) = Sealed.mapInfoProps &sink this

/// <summary>Rebuilds the run fields for a second pass from the result of the first, the way
/// <c>GlyphRun.runPass</c> threads one pass into the next.</summary>
/// <param name="original">The original input.</param>
/// <param name="result">The first pass's result.</param>
/// <returns>The input for the second pass.</returns>
let private nextInput (original: RunInput) (result: RunResult) : RunInput =
    match result.Unchanged with
    | true -> original
    | false ->
        { Infos = result.Infos
          Props = result.Props
          Positions = result.Positions
          HavePositions = result.HavePositions
          MaxLen = original.MaxLen
          MaxOps = result.MaxOps
          Successful = result.Successful
          ClusterLevel = original.ClusterLevel
          Flags = original.Flags
          Serial = result.Serial
          RandomState = result.RandomState
          ScratchFlags = result.ScratchFlags }

/// <summary>Runs three mask maps as one program.</summary>
/// <param name="input">The run fields.</param>
/// <param name="a">The first amount.</param>
/// <param name="b">The second amount.</param>
/// <param name="c">The third amount.</param>
/// <returns>The program's result.</returns>
let private threeMapProgram (input: RunInput) (a: uint32) (b: uint32) (c: uint32) : RunResult =
    let program =
        Sealed.Then(Sealed.Then(MaskAddMap(a), MaskAddMap(b)), MaskAddMap(c))
    Sealed.runStages input program

/// <summary>Maps the infos through three <see cref="Sealed.mapi"/> calls, the reference composition.</summary>
/// <param name="input">The run fields.</param>
/// <param name="a">The first amount.</param>
/// <param name="b">The second amount.</param>
/// <param name="c">The third amount.</param>
/// <returns>The composed infos.</returns>
let private threeMapReference (input: RunInput) (a: uint32) (b: uint32) (c: uint32) : ImmutableArray<GlyphInfo> =
    let add (amount: uint32) (info: GlyphInfo) = { info with Mask = info.Mask + amount }
    let once = Sealed.mapi (fun _ info -> add a info) (input.Infos.AsSpan())
    let twice = Sealed.mapi (fun _ info -> add b info) (once.AsSpan())
    Sealed.mapi (fun _ info -> add c info) (twice.AsSpan()) |> TailCalls.settle

/// <summary>Runs two passes as one program.</summary>
/// <param name="input">The run fields.</param>
/// <returns>The program's result.</returns>
let private twoPassProgram (input: RunInput) : RunResult =
    let program = Sealed.Then(PassStage(MaskPass(), false), PassStage(MaskPass(), false))
    Sealed.runStages input program

/// <summary>Runs two passes through <see cref="Sealed.runPass"/> twice, the reference composition.</summary>
/// <param name="input">The run fields.</param>
/// <returns>The second pass's result.</returns>
let private twoPassReference (input: RunInput) : RunResult =
    let first = Sealed.runPass input (MaskPass()) false
    Sealed.runPass (nextInput input first) (MaskPass()) false

/// <summary>Compares the infos and props of two results.</summary>
/// <param name="name">The label.</param>
/// <param name="expected">The reference result.</param>
/// <param name="actual">The program result.</param>
let private expectSameArrays name (expected: RunResult) (actual: RunResult) =
    Expect.sequenceEqual (expected.Infos.AsSpan().ToArray()) (actual.Infos.AsSpan().ToArray()) $"{name}: infos"
    Expect.sequenceEqual (expected.Props.AsSpan().ToArray()) (actual.Props.AsSpan().ToArray()) $"{name}: props"

/// <summary>The PERF7 program primitives: one working copy per group of stages.</summary>
[<Tests>]
let tests =
    testList "PERF7" [
        testProperty "a program of info-props maps equals the Sealed.mapi composition" (fun (values: int array) (a: int) (b: int) (c: int) ->
            let values = values |> Array.truncate 128
            let input = runInputOf values
            let expected = threeMapReference input (uint32 a) (uint32 b) (uint32 c)
            let actual = threeMapProgram input (uint32 a) (uint32 b) (uint32 c)
            let actualInfos = match actual.Unchanged with | true -> input.Infos | false -> actual.Infos
            Expect.sequenceEqual (expected.AsSpan().ToArray()) (actualInfos.AsSpan().ToArray()) "infos")

        test "a program whose maps add nothing reports itself unchanged, and the run keeps its input" {
            let input = runInputOf [| 1; 2; 3; 4; 5 |]
            let actual = threeMapProgram input 0u 0u 0u
            Expect.isTrue actual.Unchanged "an unchanged program says so"
            Expect.equal actual.Infos.Length 0 "and hands back no array, as every RunResult of an unchanged pass does"
            Expect.sequenceEqual (threeMapReference input 0u 0u 0u |> fun infos -> infos.AsSpan().ToArray()) (input.Infos.AsSpan().ToArray()) "the input is already the result"
        }

        testProperty "a program of passes equals runPass after runPass" (fun (values: int array) ->
            let values = if values.Length = 0 then [| 1 |] else values |> Array.truncate 128
            let input = runInputOf values
            let expected = twoPassReference input
            let actual = twoPassProgram input
            expectSameArrays "passes" expected actual)

        test "an identity program allocates nothing" {
            let input = runInputOf [| for i in 0 .. 999 -> i |]
            let program = Sealed.Then(IdentityInfoPropsStage(), IdentityInfoPropsStage())
            Sealed.runStages input program |> ignore
            let before = GC.GetAllocatedBytesForCurrentThread()
            let result = Sealed.runStages input program
            let after = GC.GetAllocatedBytesForCurrentThread()
            Expect.isTrue result.Unchanged "the identity program changed nothing"
            Expect.equal (after - before) 0L "bytes allocated"
        }

        test "three map stages over 10,000 glyphs allocate one published copy, not three" {
            let values = [| for i in 0 .. 9999 -> i |]
            let input = runInputOf values
            let program = Sealed.Then(Sealed.Then(MaskAddMap(1u), MaskAddMap(2u)), MaskAddMap(3u))
            let oneCopy = int64 (10000 * (sizeof<GlyphInfo> + sizeof<GlyphProps>))
            let threeInfos = int64 (10000 * sizeof<GlyphInfo> * 3)
            Sealed.runStages input program |> ignore
            let before = GC.GetAllocatedBytesForCurrentThread()
            Sealed.runStages input program |> ignore
            let after = GC.GetAllocatedBytesForCurrentThread()
            let programBytes = after - before
            let threeMaps () =
                let once = Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = info.Mask + 1u }) (input.Infos.AsSpan())
                let twice = Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = info.Mask + 2u }) (once.AsSpan())
                Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = info.Mask + 3u }) (twice.AsSpan()) |> TailCalls.settle
            threeMaps () |> ignore
            let beforeRef = GC.GetAllocatedBytesForCurrentThread()
            threeMaps () |> ignore
            let afterRef = GC.GetAllocatedBytesForCurrentThread()
            let referenceBytes = afterRef - beforeRef
            Expect.isLessThanOrEqual programBytes (oneCopy + 256L) "the program allocated beyond one published copy"
            Expect.isGreaterThanOrEqual referenceBytes (threeInfos - 256L) "three separate maps did not allocate three copies"
        }
    ]

/// <summary>Compares two shaped runs field by field, for the positioning equivalence property.</summary>
/// <param name="expected">The composition result.</param>
/// <param name="actual">The program result.</param>
let private expectSameRun (expected: GlyphRun) (actual: GlyphRun) =
    Expect.sequenceEqual (expected.Infos.AsSpan().ToArray()) (actual.Infos.AsSpan().ToArray()) "infos"
    Expect.sequenceEqual (expected.Props.AsSpan().ToArray()) (actual.Props.AsSpan().ToArray()) "props"
    Expect.sequenceEqual (expected.Positions.AsSpan().ToArray()) (actual.Positions.AsSpan().ToArray()) "positions"
    Expect.equal expected.HavePositions actual.HavePositions "have positions"
    Expect.equal expected.Successful actual.Successful "successful"
    Expect.equal expected.MaxOps actual.MaxOps "max ops"
    Expect.equal expected.ScratchFlags actual.ScratchFlags "scratch flags"
    Expect.equal expected.RandomState actual.RandomState "random state"

/// <summary>The Roboto face, loaded once for the positioning equivalence tests.</summary>
let private roboto =
    lazy (Font.create (Face.create (Farfbuzz.Tests.UpstreamFiles.read "perf/fonts/Roboto-Regular.ttf") 0u))

/// <summary>Builds a plan for Roboto in the given direction.</summary>
/// <param name="direction">The direction.</param>
/// <returns>The plan.</returns>
let private robotoPlan (direction: Direction) =
    match Execute.createPlan roboto.Value { Direction = direction; Script = Scripts.Latin; Language = Language.cLocale } ImmutableArray.Empty ValueNone with
    | Ok plan ->
        match plan.Ot with
        | ValueSome ot -> ot
        | ValueNone -> failwith "plan creation failed"
    | Error _ -> failwith "plan creation failed"

/// <summary>Builds a synthetic horizontal run of glyph ids with a few marks and default ignorables.</summary>
/// <param name="values">The source values.</param>
/// <param name="direction">The direction.</param>
/// <returns>The run.</returns>
let private syntheticRun (values: int array) (direction: Direction) =
    let len = max 1 values.Length
    let struct (markUnicode, _) = UnicodeProps.compute 0x301u
    let infos =
        Sealed.init len (fun i ->
            { Codepoint = uint32 (abs (values[i % values.Length] % 400))
              Mask = 0u
              Cluster = uint32 (i % 4)
              Var1 = 0u
              Var2 = 0u })
    let props =
        Sealed.init len (fun i ->
            match i % 7 with
            | 0 -> { Unchecked.defaultof<GlyphProps> with UnicodeProps = markUnicode; GlyphClass = GlyphPropsBits.Mark }
            | 1 -> { Unchecked.defaultof<GlyphProps> with UnicodeProps = UnicodeProps.Ignorable }
            | _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = GlyphPropsBits.Base })
    let positions =
        Sealed.init len (fun i -> { XAdvance = 200 + (values[i % values.Length] % 100); YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = NoLanguage }
        ScratchFlags = ScratchFlags.HasDefaultIgnorables ||| ScratchFlags.HasSpaceFallback }

/// <summary>Runs the positioning pipeline up to the substitute round, the run both positioning paths read.</summary>
/// <param name="font">The font.</param>
/// <param name="plan">The plan.</param>
/// <param name="run">The entered run.</param>
/// <returns>The substituted run.</returns>
let private substituteToPosition (font: Font) (plan: OtShapePlan) (run: GlyphRun) =
    let propped = OtProps.setUnicodeProps plan.Map.GlobalMask run
    let circled = OtProps.insertDottedCircle font propped
    let clustered = OtProps.formClusters circled
    let native = OtProps.ensureNativeDirection clustered
    OtSubstitute.substitutePre plan font native

/// <summary>The PERF7 positioning equivalence: the program equals the composition.</summary>
[<Tests>]
let positionTests =
    testList "PERF7 position" [
        testProperty "the positioning program equals the composition over random runs" (fun (values: int array) (backward: bool) ->
            let values = if values.Length = 0 then [| 1 |] else values |> Array.truncate 64
            let direction = if backward then Direction.Rtl else Direction.Ltr
            let run = syntheticRun values direction
            let plan = robotoPlan direction
            match OtPosition.positionComposed plan roboto.Value run, OtPosition.positionProgram plan roboto.Value run with
            | Ok expected, Ok actual -> expectSameRun expected actual
            | Error expected, Error actual -> Expect.equal expected actual "errors agree"
            | _ -> failtest "one path errored, the other did not")

        test "the positioning program equals the composition on every line of both Roboto texts" {
            let font = roboto.Value
            let texts =
                [ "perf/texts/en-thelittleprince.txt"; "perf/texts/en-words.txt" ]
                |> List.map (fun path -> Farfbuzz.Tests.UpstreamFiles.read path)
            for text in texts do
                let bytes = text.ToArray()
                let plan = robotoPlan Direction.Ltr
                let mutable start = 0
                let mutable count = 0
                let mutable i = 0
                while i < bytes.Length do
                    match bytes[i] with
                    | 0x0Auy ->
                        let run =
                            GlyphRun.addUtf8 (ReadOnlySpan(bytes, start, bytes.Length - start)) 0 (i - start) GlyphRun.empty
                            |> GlyphRun.guessSegmentProperties Language.cLocale
                            |> GlyphRun.enter
                            |> TailCalls.settle
                        match substituteToPosition font plan run with
                        | Ok substituted ->
                            match OtPosition.positionComposed plan font substituted, OtPosition.positionProgram plan font substituted with
                            | Ok expected, Ok actual -> expectSameRun expected actual
                            | Error expected, Error actual -> Expect.equal expected actual "errors agree"
                            | _ -> failtest "one path errored, the other did not"
                        | Error _ -> ()
                        start <- i + 1
                        count <- count + 1
                    | _ -> ()
                    i <- i + 1
                Expect.isGreaterThan count 100 $"expected many lines, shaped {count}"
        }

        test "the positioning program allocates less than the composition" {
            let font = roboto.Value
            let plan = robotoPlan Direction.Ltr
            let values = [| for i in 0 .. 9999 -> i |]
            let run = syntheticRun values Direction.Ltr
            let composed () = OtPosition.positionComposed plan font run |> ignore
            let program () = OtPosition.positionProgram plan font run |> ignore
            composed ()
            program ()
            let before = GC.GetAllocatedBytesForCurrentThread()
            composed ()
            let composedBytes = GC.GetAllocatedBytesForCurrentThread() - before
            let before2 = GC.GetAllocatedBytesForCurrentThread()
            program ()
            let programBytes = GC.GetAllocatedBytesForCurrentThread() - before2
            Expect.isLessThan programBytes (composedBytes / 2L) "the program should allocate well under half of the composition"
        }
    ]
