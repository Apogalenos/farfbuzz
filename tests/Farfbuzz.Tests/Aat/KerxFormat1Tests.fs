module Farfbuzz.Tests.AatKerxFormat1Tests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Aat
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.SfntBuilder

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = SfntBuilder.be16 value

/// <summary>Writes a big-endian uint32.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) = SfntBuilder.be32 value

/// <summary>A null format 1 entry: no flags and no action index.</summary>
let private nullEntry = { NewState = 0; Flags = 0us; Data0 = 0xFFFFus; Data1 = 0us }

/// <summary>Builds a one-state machine whose narrow class table maps glyph 0 to the user class 4 and whose five
/// entries are in class order.</summary>
/// <param name="actionEntry">The class 4 entry; the other four are null.</param>
/// <returns>The machine.</returns>
let private actionMachine (actionEntry: Entry) =
    let nClasses = 5
    let classBytes = Array.concat [ be16 0u; be16 1u; [| 4uy |] ]
    { ClassCount = nClasses
      Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = 1; Wide = false }
      States = Sealed.init nClasses (fun i -> uint16 i)
      StateBias = 0
      RowCount = 1
      Entries = Sealed.init nClasses (fun i -> if i = 4 then actionEntry else nullEntry)
      IsExtended = false }

/// <summary>Builds a horizontal run of one glyph with a 100-unit advance and the kern mask set.</summary>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The run.</returns>
let private format1Run (maxOps: int) =
    let infos = Sealed.init 1 (fun _ -> { Codepoint = 0u; Mask = 0x80000000u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init 1 (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = 0us })
    let positions = Sealed.init 1 (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        MaxOps = maxOps
        SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>Runs the format 1 driver over the run and returns the finished run.</summary>
/// <param name="font">The font.</param>
/// <param name="spec">The subtable spec.</param>
/// <param name="machine">The state machine.</param>
/// <param name="run">The run.</param>
/// <returns>The run after the subtable.</returns>
let private runFormat1 (font: Font) (spec: Format1Spec) (machine: StateMachine) (run: GlyphRun) =
    let kind =
        { Spec = spec
          Font = font
          Horizontal = true
          CrossStream = false
          KernMask = 0x80000000u }
    let config =
        { Machine = machine
          Ranges = ImmutableArray.Empty
          SubtableFlags = 0u
          NumGlyphs = 16
          StartSafeToBreakAtEndOfText = not (KerxStateMachines.performAction spec.Extended (StateTable.entryCall machine StateTable.StartOfText StateTable.ClassEndOfText)) }
    let seed: DriverState<KerxFormat1Context> =
        { Phase = Start
          State = 0
          Range = 0
          SkipEndOfText = false
          Context = KerxStateMachines.seed () }
    GlyphRun.runPass { StateMachinePass.Config = config; Kind = kind } seed run

/// <summary>A default font for the format 1 tests, upem = scale.</summary>
let private format1Font = Font.create Face.empty

/// <summary>Builds a face whose only table is a kerx table with the given glyph count.</summary>
/// <param name="kerx">The kerx table bytes.</param>
/// <param name="glyphCount">The glyph count.</param>
/// <returns>The face.</returns>
let private kerxFace (kerx: byte array) (glyphCount: int) =
    let file = sfnt 0x00010000u [ Tags.Kerx, kerx ]
    OtFace.create (Face.withGlyphCount glyphCount (Face.create (ReadOnlyMemory file) 0u))

/// <summary>Builds a plan that applies the kerx table.</summary>
/// <returns>The plan.</returns>
let private kerxPlan () =
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

/// <summary>Builds a kerx format 1 table whose initial glyph set is empty, so the subtable is always skipped.</summary>
/// <returns>The kerx table bytes.</returns>
let private emptySetKerx () =
    let classLookup = Array.concat [ be16 8u; be16 0u; be16 1u; be16 0u ]
    let stateArray = Array.concat [ be16 0u; be16 0u; be16 0u; be16 0u ]
    let entries = Array.concat [ be16 0u; be16 0u; be16 0xFFFFu ]
    let subtable =
        Array.concat
            [ be32 56u; be32 1u; be32 0u
              be32 4u; be32 20u; be32 28u; be32 36u; be32 42u
              classLookup; stateArray; entries; be16 0xFFFFu ]
    Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]

/// <summary>The kern mask the literal and the driver both use.</summary>
[<Literal>]
let private KernMaskBit = 0x80000000u

/// <summary>The inputs of one literal format 1 run.</summary>
type private LiteralInput =
    { Spec: Format1Spec
      Machine: StateMachine
      Horizontal: bool
      CrossStream: bool
      Codepoints: uint32 array
      Clusters: uint32 array
      Masks: uint32 array
      Positions: GlyphPosition array
      Level: ClusterLevel
      MaxOps: int }

/// <summary>A literal transliteration of <c>StateTableDriver::drive</c> (src/hb-aat-layout-common.hh:1282-1442) with
/// <c>KerxSubTableFormat1::driver_context_t::transition</c> (src/hb-aat-layout-kerx-table.hh:263-405) and the buffer
/// flag setting of src/hb-buffer.hh:456-520 and 640-702, one glyph per iteration, mutable as the C is.</summary>
type private LiteralFormat1(input: LiteralInput, font: Font) =
    let spec = input.Spec
    let len = input.Codepoints.Length
    let masks = Array.copy input.Masks
    let positions = Array.copy input.Positions
    let stack: int array = Array.zeroCreate 8
    let mutable idx = 0
    let mutable maxOps = input.MaxOps
    let mutable successful = true
    let mutable scratch = 0u
    let mutable depth = 0
    let mutable stackPos = 0

    member _.Masks = masks
    member _.Positions = positions
    member _.MaxOps = maxOps
    member _.Successful = successful
    member _.Scratch = scratch

    member private _.InfosSetFlags (start: int, finish: int, cluster: uint32, mask: uint32) =
        match start = finish with
        | true -> ()
        | false ->
            maxOps <- maxOps - (finish - start)
            if maxOps < 0 then successful <- false
            let clusterFirst = input.Clusters[start]
            let clusterLast = input.Clusters[finish - 1]
            match input.Level = ClusterLevel.Characters || (cluster <> clusterFirst && cluster <> clusterLast) with
            | true ->
                for i in start .. finish - 1 do
                    if cluster <> input.Clusters[i] then masks[i] <- masks[i] ||| mask
            | false ->
                match cluster = clusterFirst with
                | true ->
                    let mutable i = finish
                    while start < i && input.Clusters[i - 1] <> clusterFirst do
                        masks[i - 1] <- masks[i - 1] ||| mask
                        i <- i - 1
                | false ->
                    let mutable i = start
                    while i < finish && input.Clusters[i] <> clusterLast do
                        masks[i] <- masks[i] ||| mask
                        i <- i + 1

    member private _.FindMinCluster (start: int, finish: int) =
        match start = finish with
        | true -> UInt32.MaxValue
        | false ->
            match input.Level = ClusterLevel.Characters with
            | true ->
                let mutable cluster = UInt32.MaxValue
                for i in start .. finish - 1 do
                    cluster <- min cluster input.Clusters[i]
                cluster
            | false -> min UInt32.MaxValue (min input.Clusters[start] input.Clusters[finish - 1])

    member this.UnsafeToBreak (start: int, finish: int, fromOutBuffer: bool) =
        let mask = uint32 GlyphFlags.UnsafeToBreak ||| uint32 GlyphFlags.UnsafeToConcat
        match uint32 (finish - start) > 255u with
        | true -> ()
        | false ->
            let finish = min finish len
            match (not fromOutBuffer) && finish - start < 2 with
            | true -> ()
            | false -> this.InfosSetFlags (start, finish, this.FindMinCluster (start, finish), mask)

    member private _.SaturateAdd (a: int, b: int) =
        int (Math.Clamp (int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    member private this.ApplyValue (index: int, v: int) =
        let o = positions[index]
        let attachType = byte ((o.Var >>> 16) &&& 0xFFu)
        match input.Horizontal, input.CrossStream with
        | true, true ->
            match v = -0x8000 with
            | true -> positions[index] <- { o with Var = o.Var &&& 0xFF000000u; YOffset = 0 }
            | false ->
                match attachType <> 0uy with
                | true ->
                    positions[index] <- { o with YOffset = this.SaturateAdd (o.YOffset, Font.emScaleY font v) }
                    scratch <- scratch ||| uint32 ScratchFlags.HasGposAttachment
                | false -> ()
        | false, true ->
            match v = -0x8000 with
            | true -> positions[index] <- { o with Var = o.Var &&& 0xFF000000u; XOffset = 0 }
            | false ->
                match attachType <> 0uy with
                | true ->
                    positions[index] <- { o with XOffset = this.SaturateAdd (o.XOffset, Font.emScaleX font v) }
                    scratch <- scratch ||| uint32 ScratchFlags.HasGposAttachment
                | false -> ()
        | true, false ->
            match masks[index] &&& KernMaskBit <> 0u with
            | true ->
                let scaled = Font.emScaleX font v
                positions[index] <- { o with XAdvance = this.SaturateAdd (o.XAdvance, scaled); XOffset = this.SaturateAdd (o.XOffset, scaled) }
            | false -> ()
        | false, false ->
            match masks[index] &&& KernMaskBit <> 0u with
            | true ->
                let scaled = Font.emScaleY font v
                positions[index] <- { o with YAdvance = this.SaturateAdd (o.YAdvance, scaled); YOffset = this.SaturateAdd (o.YOffset, scaled) }
            | false -> ()

    member private this.Transition (entry: Entry) =
        let span = spec.TableBytes.Span
        let extended = spec.Extended
        let offsetMask = 0x3FFFus
        if entry.Flags &&& 0x8000us <> 0us then
            if extended then
                stack[stackPos] <- idx
                stackPos <- (stackPos + 1) % 8
                depth <- min (depth + 1) 8
            elif depth < 8 then
                stack[depth] <- idx
                depth <- depth + 1
            else
                depth <- 0
        let performAction = if extended then entry.Data0 <> 0xFFFFus else entry.Flags &&& offsetMask <> 0us
        if performAction && depth <> 0 then
            let tupleCount = int64 (max 1u spec.TupleCount)
            let kernIdx =
                match extended with
                | true -> int64 entry.Data0
                | false ->
                    let arrayOffset = int64 (spec.KernActionAt - spec.MachineAt)
                    let offset = int64 (entry.Flags &&& offsetMask)
                    match offset < arrayOffset with
                    | true -> int64 (Int32.MaxValue / 2)
                    | false -> (offset - arrayOffset) / 2L
            let mutable actions = int64 spec.KernActionAt + kernIdx * 2L
            let limit = int64 spec.RangeEnd
            let mutable stop = false
            if (not extended) && not (actions >= 0L && actions + int64 depth * tupleCount * 2L <= limit) then
                depth <- 0
                stop <- true
            let mutable cursor = stackPos
            let mutable last = false
            while not stop && not last && depth <> 0 do
                let mutable skip = false
                if extended then
                    let before = maxOps
                    maxOps <- maxOps - 1
                    if before <= 0 then
                        successful <- false
                        stop <- true
                    elif not (actions >= 0L && actions + 2L <= limit) then
                        depth <- 0
                        stop <- true
                    elif BinaryCalls.i16 span (int actions) = -1s then
                        stop <- true
                    elif tupleCount > 1L && not (actions + tupleCount * 2L <= limit) then
                        depth <- 0
                        stop <- true
                if not stop then
                    let index =
                        match extended with
                        | true ->
                            cursor <- (cursor + 8 - 1) % 8
                            stack[cursor]
                        | false ->
                            depth <- depth - 1
                            stack[depth]
                    let mutable v = int (BinaryCalls.i16 span (int actions))
                    actions <- actions + tupleCount * 2L
                    if index >= len then skip <- true
                    if not skip then
                        if extended then this.UnsafeToBreak (index, idx + 1, false)
                        if not extended then
                            last <- v &&& 1 <> 0
                            v <- v &&& ~~~1
                        this.ApplyValue (index, v)

    member this.Drive () =
        let classOf glyph = StateTable.classOfCall input.Machine glyph 100
        let getEntry state klass = StateTable.entryCall input.Machine state klass
        let isActionable (entry: Entry) = if spec.Extended then entry.Data0 <> 0xFFFFus else entry.Flags &&& 0x3FFFus <> 0us
        let startSafeEot = not (isActionable (getEntry StateTable.StartOfText StateTable.ClassEndOfText))
        let mutable state = 0
        let mutable finished = false
        while successful && not finished do
            let mutable klass = if idx < len then classOf input.Codepoints[idx] else StateTable.ClassEndOfText
            let mutable resume = true
            while resume && successful && not finished do
                resume <- false
                let entry = getEntry state klass
                let nextState = entry.NewState
                let notEpsilon = entry.Flags &&& 0x4000us = 0us
                let notActionable = not (isActionable entry)
                let isNull = state = StateTable.StartOfText && nextState = StateTable.StartOfText && startSafeEot && notActionable && notEpsilon
                match isNull with
                | true ->
                    let oldKlass = klass
                    let mutable go = true
                    while go do
                        this.Transition entry
                        match idx = len || not successful with
                        | true -> go <- false
                        | false ->
                            idx <- idx + 1
                            klass <- if idx < len then classOf input.Codepoints[idx] else StateTable.ClassEndOfText
                            go <- klass = oldKlass
                    match idx = len || not successful with
                    | true -> finished <- true
                    | false -> resume <- true
                | false ->
                    let wouldBe = getEntry StateTable.StartOfText klass
                    let safe =
                        (not (isActionable entry))
                        && (state = StateTable.StartOfText
                            || (entry.Flags &&& 0x4000us <> 0us && nextState = StateTable.StartOfText)
                            || ((not (isActionable wouldBe)) && nextState = wouldBe.NewState && (entry.Flags &&& 0x4000us) = (wouldBe.Flags &&& 0x4000us)))
                        && not (isActionable (getEntry state StateTable.ClassEndOfText))
                    if (not safe) && idx > 0 && idx < len then this.UnsafeToBreak (idx - 1, idx + 1, true)
                    this.Transition entry
                    state <- nextState
                    match idx = len with
                    | true -> finished <- true
                    | false ->
                        let before = maxOps
                        match notEpsilon with
                        | true -> idx <- idx + 1
                        | false ->
                            maxOps <- maxOps - 1
                            if before <= 0 then idx <- idx + 1


/// <summary>Builds a machine whose class table maps glyph g to class g, one state row per entry row.</summary>
/// <param name="nClasses">The class count.</param>
/// <param name="entries">The entries, nStates times nClasses of them.</param>
/// <returns>The machine.</returns>
let private classMachine (nClasses: int) (entries: Entry array) =
    let classBytes = Array.init (4 + nClasses) (fun i -> if i < 4 then 0uy else byte (i - 4))
    { ClassCount = nClasses
      Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = nClasses; Wide = false }
      States = Sealed.init entries.Length (fun i -> uint16 i)
      StateBias = 0
      RowCount = entries.Length / nClasses
      Entries = Sealed.init entries.Length (fun i -> entries[i])
      IsExtended = false }

/// <summary>Generates a random format 1 input: the machine, the action bytes, the run and the direction.</summary>
/// <param name="extended">True for kerx, false for the obsolete kern.</param>
/// <returns>The generator.</returns>
let private inputGen (extended: bool) =
    gen {
        let! nClasses = Gen.elements [ 5; 6; 7 ]
        let! nStates = Gen.choose (1, 4)
        let! kernActionAt = Gen.elements [ 0; 2; 4; 6 ]
        let arrayOffset = match extended with | true -> 0 | false -> kernActionAt
        let offsets = [ 0; arrayOffset; arrayOffset + 1; arrayOffset + 2; arrayOffset + 4; arrayOffset + 6; max 0 (arrayOffset - 2); 0x3FFF ]
        let entryGen =
            gen {
                let! newState = Gen.choose (0, nStates - 1)
                let! flagBits = Gen.elements [ 0us; 0us; 0x8000us; 0x4000us; 0xC000us; 0x2000us ]
                let! data0 = Gen.elements [ 0xFFFFus; 0xFFFFus; 0us; 1us; 2us; 3us; 40us ]
                let! offset = Gen.elements offsets
                match extended with
                | true -> return { NewState = newState; Flags = flagBits; Data0 = data0; Data1 = 0us }
                | false -> return { NewState = newState; Flags = (flagBits &&& 0xC000us) ||| uint16 offset; Data0 = 0us; Data1 = 0us }
            }
        let! entries = Gen.arrayOfLength (nStates * nClasses) entryGen
        let! values = Gen.arrayOfLength 16 (Gen.elements [ -1; -1; 0; 1; 2; 3; 40; -40; 501; -0x8000; 0x7FFF; 255 ])
        let body = values |> Array.collect (fun value -> be16 (uint32 (uint16 value)))
        let! rangeEnd = Gen.elements [ body.Length; body.Length; body.Length; body.Length - 2; body.Length - 5; 8; 3; 0 ]
        let! tupleCount = match extended with | true -> Gen.elements [ 0u; 0u; 1u; 2u; 3u ] | false -> Gen.constant 0u
        let! count = Gen.choose (0, 12)
        let! codepoints = Gen.arrayOfLength count (Gen.choose (0, nClasses - 1) |> Gen.map uint32)
        let! steps = Gen.arrayOfLength count (Gen.elements [ 0u; 1u; 1u; 2u ])
        let clusters = Array.scan (+) 0u steps |> Array.take count
        let! masks = Gen.arrayOfLength count (Gen.elements [ 0u; KernMaskBit; KernMaskBit ])
        let! attach = Gen.arrayOfLength count (Gen.elements [ 0u; 2u ])
        let! advances = Gen.arrayOfLength count (Gen.choose (-50, 50))
        let! horizontal = Gen.elements [ true; false ]
        let! crossStream = Gen.elements [ false; false; true ]
        let! level = Gen.elements [ ClusterLevel.MonotoneGraphemes; ClusterLevel.MonotoneCharacters; ClusterLevel.Characters ]
        let! maxOps = Gen.elements [ 1; 2; 3; 5; 8; 13; 40; 1000 ]
        let positions = Array.init count (fun i -> { XAdvance = advances[i]; YAdvance = 0; XOffset = 0; YOffset = 0; Var = attach[i] <<< 16 })
        let spec =
            { Extended = extended
              TableBytes = ReadOnlyMemory body
              RangeEnd = rangeEnd
              MachineAt = 0
              KernActionAt = kernActionAt
              TupleCount = tupleCount }
        return
            { Spec = spec
              Machine = classMachine nClasses entries
              Horizontal = horizontal
              CrossStream = crossStream
              Codepoints = codepoints
              Clusters = clusters
              Masks = masks
              Positions = positions
              Level = level
              MaxOps = maxOps }
    }

/// <summary>Runs the driver over a literal input and returns every field of the run the pass can change.</summary>
/// <param name="input">The input.</param>
/// <returns>The masks, positions, budget, success flag and scratch flags.</returns>
let private driverFields (input: LiteralInput) =
    let count = input.Codepoints.Length
    let infos = Sealed.init count (fun i -> { Codepoint = input.Codepoints[i]; Mask = input.Masks[i]; Cluster = input.Clusters[i]; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    let positions = Sealed.init count (fun i -> input.Positions[i])
    let run =
        { GlyphRun.empty with
            Infos = infos
            Props = props
            Positions = positions
            HavePositions = true
            ClusterLevel = input.Level
            MaxOps = input.MaxOps }
    let kind: KerxFormat1Kind = { Spec = input.Spec; Font = format1Font; Horizontal = input.Horizontal; CrossStream = input.CrossStream; KernMask = KernMaskBit }
    let config: DriverConfig =
        { Machine = input.Machine
          Ranges = ImmutableArray.Empty
          SubtableFlags = 0u
          NumGlyphs = 100
          StartSafeToBreakAtEndOfText = not (KerxStateMachines.performAction input.Spec.Extended (StateTable.entryCall input.Machine StateTable.StartOfText StateTable.ClassEndOfText)) }
    let seed: DriverState<KerxFormat1Context> =
        { Phase = Start
          State = 0
          Range = 0
          SkipEndOfText = false
          Context = KerxStateMachines.seed () }
    let result = GlyphRun.runPass { StateMachinePass.Config = config; Kind = kind } seed run
    struct (result.Infos |> Seq.map (fun info -> info.Mask) |> Seq.toArray,
            result.Positions |> Seq.toArray,
            result.MaxOps,
            result.Successful,
            uint32 result.ScratchFlags)

/// <summary>Runs the literal transliteration and returns the same fields.</summary>
/// <param name="input">The input.</param>
/// <returns>The masks, positions, budget, success flag and scratch flags.</returns>
let private literalFields (input: LiteralInput) =
    let literal = LiteralFormat1 (input, format1Font)
    literal.Drive ()
    struct (literal.Masks, literal.Positions, literal.MaxOps, literal.Successful, literal.Scratch)

/// <summary>The property: the driver and the literal agree on every field, for one format.</summary>
/// <param name="name">The test name.</param>
/// <param name="extended">True for kerx.</param>
/// <returns>The test.</returns>
let private literalProperty (name: string) (extended: bool) =
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 5000 } name <|
        Prop.forAll (Arb.fromGen (inputGen extended)) (fun input ->
            let struct (masks, positions, maxOps, successful, scratch) = driverFields input
            let struct (literalMasks, literalPositions, literalMaxOps, literalSuccessful, literalScratch) = literalFields input
            let show (positions: GlyphPosition array) = positions |> Array.map (fun p -> $"{p.XAdvance}/{p.YAdvance}/{p.XOffset}/{p.YOffset}/{p.Var}") |> String.concat " "
            let machineText = input.Machine.Entries |> Seq.map (fun e -> $"[{e.NewState},{e.Flags:X},{e.Data0:X}]") |> String.concat ""
            let context = $"ext={input.Spec.Extended} tuple={input.Spec.TupleCount} end={input.Spec.RangeEnd} kaa={input.Spec.KernActionAt} horiz={input.Horizontal} cross={input.CrossStream} level={input.Level} ops={input.MaxOps} text={String.Join(',', input.Codepoints)} clusters={String.Join(',', input.Clusters)} masks={String.Join(',', input.Masks)} pos={show input.Positions} entries={machineText} classes={input.Machine.ClassCount} body={Convert.ToHexString(input.Spec.TableBytes.ToArray())}"
            Expect.equal (String.concat "," (Array.map string masks)) (String.concat "," (Array.map string literalMasks)) $"glyph flags {context}"
            Expect.equal (show positions) (show literalPositions) $"positions ours={show positions} literal={show literalPositions} {context}"
            Expect.equal maxOps literalMaxOps $"budget ours={maxOps} literal={literalMaxOps} {context}"
            Expect.equal successful literalSuccessful $"success flag {context}"
            Expect.equal scratch literalScratch $"scratch flags {context}"
            true)

/// <summary>The P8.S11 kerx and kern format 1 unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S11" [
        test "the eight-slot history wraps and saturates" {
            let extended = KerxStateMachines.seed ()
            let mutable context = extended
            for index in 0 .. 8 do
                context <- KerxStateMachines.push true index context
            Expect.equal context.Depth 8 "the extended depth is capped at eight"
            Expect.equal context.StackPos 1 "the extended stack position wrapped to slot 1"
            Expect.equal (Stack8.get context.Stack 0) 8 "the ninth push overwrote slot 0"
            Expect.equal (Stack8.get context.Stack 1) 1 "slot 1 keeps the second push"
            let obsolete = KerxStateMachines.seed ()
            let mutable legacy = obsolete
            for index in 0 .. 8 do
                legacy <- KerxStateMachines.push false index legacy
            Expect.equal legacy.Depth 0 "the ninth legacy push clears the depth"
        }
        test "the legacy terminator low bit ends the action list" {
            let action = { NewState = 0; Flags = 0x8002us; Data0 = 0us; Data1 = 0us }
            let machine = actionMachine action
            let body = Array.concat [ be16 0u; be16 501u ]
            let spec =
                { Extended = false
                  TableBytes = ReadOnlyMemory body
                  RangeEnd = body.Length
                  MachineAt = 0
                  KernActionAt = 2
                  TupleCount = 0u }
            let result = runFormat1 format1Font spec machine (format1Run GlyphRun.MaxOpsDefault)
            Expect.equal result.Positions[0].XAdvance 600 "the cleared 500 value is added to the advance"
            Expect.equal result.Positions[0].XOffset 500 "the cleared 500 value is added to the offset"
        }
        test "the extended action loop ends when the budget is spent" {
            let action = { NewState = 0; Flags = 0x8000us; Data0 = 0us; Data1 = 0us }
            let machine = actionMachine action
            let body = Array.create 200 0uy
            let spec =
                { Extended = true
                  TableBytes = ReadOnlyMemory body
                  RangeEnd = body.Length
                  MachineAt = 0
                  KernActionAt = 0
                  TupleCount = 0u }
            let result = runFormat1 format1Font spec machine (format1Run 3)
            Expect.isFalse result.Successful "the budget is exhausted"
            Expect.equal result.MaxOps (-1) "the failing check charges too, as max_ops-- <= 0 does"
        }
        literalProperty "the kerx format 1 driver equals a literal transliteration of drive and transition" true
        literalProperty "the kern format 1 driver equals a literal transliteration of drive and transition" false
        test "a kern format 1 pass whose actions change nothing allocates nothing" {
            let body = Array.concat [ be16 500u; be16 0xFFFFu ]
            let run = { (format1Run GlyphRun.MaxOpsDefault) with Infos = Sealed.init 1 (fun _ -> { Codepoint = 0u; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u }) }
            let obsoleteSpec = { Extended = false; TableBytes = ReadOnlyMemory body; RangeEnd = body.Length; MachineAt = 0; KernActionAt = 0; TupleCount = 0u }
            let obsolete = actionMachine { NewState = 0; Flags = 0x8000us ||| 2us; Data0 = 0us; Data1 = 0us }
            runFormat1 format1Font obsoleteSpec obsolete run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> runFormat1 format1Font obsoleteSpec obsolete run |> ignore)) 0L "a kern pass the kern mask refuses allocates nothing, as no kern action charges the budget"
        }
        test "an endless DontAdvance machine ends when the budget is spent" {
            let machine = actionMachine { NewState = 0; Flags = 0x4000us; Data0 = 0xFFFFus; Data1 = 0us }
            let spec = { Extended = true; TableBytes = ReadOnlyMemory.Empty; RangeEnd = 0; MachineAt = 0; KernActionAt = 0; TupleCount = 0u }
            let result = runFormat1 format1Font spec machine (format1Run 50)
            Expect.isFalse (result.MaxOps > 0) "the budget is spent by the epsilon transitions"
            Expect.equal result.Positions[0].XAdvance 100 "nothing was kerned"
        }
        test "a skipped kerx format 1 subtable allocates nothing" {
            let face = kerxFace (emptySetKerx ()) 16
            let plan = kerxPlan ()
            let font = Font.create face.Face
            let run = format1Run GlyphRun.MaxOpsDefault
            AatLayout.position plan font run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> AatLayout.position plan font run |> ignore)) 0L "a subtable with no initial glyphs allocates nothing"
        }
        test "a non-actionable format 1 transition allocates nothing" {
            let machine = actionMachine nullEntry
            let spec =
                { Extended = true
                  TableBytes = ReadOnlyMemory.Empty
                  RangeEnd = 0
                  MachineAt = 0
                  KernActionAt = 0
                  TupleCount = 0u }
            let run = format1Run GlyphRun.MaxOpsDefault
            runFormat1 format1Font spec machine run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> runFormat1 format1Font spec machine run |> ignore)) 0L "a null transition batches without allocating"
        }
    ]
