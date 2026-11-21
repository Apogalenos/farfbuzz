module Farfbuzz.Tests.Shape.Perf10Tests

open System
open System.Collections.Immutable
open Expecto
open FsCheck
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests
open Farfbuzz.Tests.Allocation

/// <summary>Compares two shaped runs on every field, for the every-field equivalence properties.</summary>
/// <param name="expected">The reference run.</param>
/// <param name="actual">The candidate run.</param>
let private expectSameRunAll (expected: GlyphRun) (actual: GlyphRun) =
    Expect.sequenceEqual (expected.Infos.AsSpan().ToArray()) (actual.Infos.AsSpan().ToArray()) "infos"
    Expect.sequenceEqual (expected.Props.AsSpan().ToArray()) (actual.Props.AsSpan().ToArray()) "props"
    Expect.sequenceEqual (expected.Positions.AsSpan().ToArray()) (actual.Positions.AsSpan().ToArray()) "positions"
    Expect.equal expected.ContextBefore actual.ContextBefore "context before"
    Expect.equal expected.ContextAfter actual.ContextAfter "context after"
    Expect.equal expected.ContentType actual.ContentType "content type"
    Expect.equal expected.SegmentProperties actual.SegmentProperties "segment properties"
    Expect.equal expected.Flags actual.Flags "flags"
    Expect.equal expected.ClusterLevel actual.ClusterLevel "cluster level"
    Expect.equal expected.Replacement actual.Replacement "replacement"
    Expect.equal expected.Invisible actual.Invisible "invisible"
    Expect.equal expected.NotFound actual.NotFound "not found"
    Expect.equal expected.NotFoundVariationSelector actual.NotFoundVariationSelector "not found variation selector"
    Expect.equal expected.RandomState actual.RandomState "random state"
    Expect.equal expected.HavePositions actual.HavePositions "have positions"
    Expect.equal expected.Successful actual.Successful "successful"
    Expect.equal expected.Serial actual.Serial "serial"
    Expect.equal expected.ScratchFlags actual.ScratchFlags "scratch flags"
    Expect.equal expected.MaxLen actual.MaxLen "max len"
    Expect.equal expected.MaxOps actual.MaxOps "max ops"

/// <summary>Builds a run of the given code points, one glyph each, with zeroed properties.</summary>
/// <param name="codepoints">The code points.</param>
/// <param name="direction">The direction.</param>
/// <returns>The run.</returns>
let private runOfCodepoints (codepoints: uint32 array) (direction: Direction) =
    let infos =
        Sealed.init codepoints.Length (fun index ->
            { Codepoint = codepoints[index]
              Mask = 0u
              Cluster = uint32 index
              Var1 = 0u
              Var2 = 0u })
    let props = Sealed.init codepoints.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with
        Infos = infos
        Props = props
        SegmentProperties = { Direction = direction; Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>The Roboto face, loaded once.</summary>
let private roboto =
    lazy (Font.create (Face.create (UpstreamFiles.read "perf/fonts/Roboto-Regular.ttf") 0u))

/// <summary>Builds a Roboto plan in the given direction.</summary>
/// <param name="direction">The direction.</param>
/// <returns>The ot plan.</returns>
let private robotoPlan (direction: Direction) =
    match Execute.createPlan roboto.Value { Direction = direction; Script = Scripts.Latin; Language = Language.cLocale } ImmutableArray.Empty ValueNone with
    | Ok plan ->
        match plan.Ot with
        | ValueSome ot -> ot
        | ValueNone -> failwith "plan creation failed"
    | Error _ -> failwith "plan creation failed"

/// <summary>The reference rotateChars: always maps every glyph, the path the early exit replaces.</summary>
/// <param name="plan">The plan.</param>
/// <param name="font">The font.</param>
/// <param name="targetDirection">The direction.</param>
/// <param name="run">The run.</param>
/// <returns>The rotated run.</returns>
let private rotateCharsReference (plan: OtShapePlan) (font: Font) (targetDirection: Direction) (run: GlyphRun) =
    let hasGlyph (codepoint: uint32) =
        match Font.nominalGlyph font codepoint with
        | ValueSome _ -> true
        | ValueNone -> false
    let isVertical (direction: Direction) =
        match direction with
        | Direction.Ttb | Direction.Btt -> true
        | Direction.Invalid | Direction.Ltr | Direction.Rtl -> false
    let mapping (info: GlyphInfo) =
        let info =
            match Direction.isBackward targetDirection with
            | true ->
                let mirrored = Unicode.mirroring info.Codepoint
                match mirrored <> info.Codepoint && hasGlyph mirrored with
                | true -> { info with Codepoint = mirrored }
                | false -> { info with Mask = info.Mask ||| plan.RtlmMask }
            | false -> info
        match isVertical targetDirection && not plan.HasVert with
        | true ->
            let vertical = Unicode.verticalCharFor info.Codepoint
            match vertical <> info.Codepoint && hasGlyph vertical with
            | true -> { info with Codepoint = vertical }
            | false -> info
        | false -> info
    { run with Infos = Sealed.mapIfChanged mapping run.Infos }

/// <summary>The PERF10.3 early exits of rotateChars and recategorizeMarks.</summary>
[<Tests>]
let tests =
    testList "PERF10" [
        test "isNonSpacingMark equals the general-category check for every property value" {
            for value in 0 .. 0xFFFF do
                let props = { Unchecked.defaultof<GlyphProps> with UnicodeProps = uint16 value }
                let viaGeneral = UnicodeProps.generalCategory props = GeneralCategory.NonSpacingMark
                Expect.equal (UnicodeProps.isNonSpacingMark props) viaGeneral $"value {value:X4}"
        }

        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 2000 } "rotateChars equals the reference for every direction" (
            fun (values: int array) (directionIndex: int) ->
                let values = if values.Length = 0 then [| 1 |] else values |> Array.truncate 64
                let codepoints = values |> Array.map (fun value -> uint32 (abs (value % 0x110000)))
                let direction = [| Direction.Ltr; Direction.Rtl; Direction.Ttb; Direction.Btt |][abs directionIndex % 4]
                let run = runOfCodepoints codepoints direction
                let plan = robotoPlan direction
                let expected = rotateCharsReference plan roboto.Value direction run
                let actual = OtSubstitute.rotateChars plan roboto.Value direction run
                expectSameRunAll expected actual)

        test "rotateChars returns its input unchanged and allocates nothing for LTR horizontal" {
            let plan = robotoPlan Direction.Ltr
            let run = runOfCodepoints [| 0x41u; 0x42u; 0x43u |] Direction.Ltr
            let result = OtSubstitute.rotateChars plan roboto.Value Direction.Ltr run
            Expect.isTrue (Object.ReferenceEquals(run, result)) "reference-equal"
            Expect.equal (bytesAllocatedBy (fun () -> OtSubstitute.rotateChars plan roboto.Value Direction.Ltr run |> ignore)) 0L "bytes allocated"
        }

        test "recategorizeMarks returns its input unchanged for mark-free text" {
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x42u; 0x43u; 0x20u |]) 0 -1 GlyphRun.empty
                |> OtProps.setUnicodeProps 0xFFFFFFFFu
                |> TailCalls.settle
            let result = OtLayout.recategorizeMarks run
            Expect.isTrue (Object.ReferenceEquals(run, result)) "reference-equal"
            Expect.equal (bytesAllocatedBy (fun () -> OtLayout.recategorizeMarks run |> ignore)) 0L "bytes allocated"
        }

        test "substituteStart equals the always-mapping reference on a face without and with glyph classes" {
            let substituteStartReference font (run: GlyphRun) =
                let props =
                    Sealed.mapi (fun index (info: GlyphInfo) ->
                        { run.Props[index] with
                            GlyphClass = OtLayout.glyphProps font info.Codepoint
                            LigProps = 0uy }) (run.Infos.AsSpan())
                { run with Props = props }
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| 0x41u; 0x301u; 0x42u |]) 0 -1 GlyphRun.empty
                |> OtProps.setUnicodeProps 0xFFFFFFFFu
                |> TailCalls.settle
            let noGdef = Font.create Face.empty
            let expected = substituteStartReference noGdef run
            let actual = OtSubstitute.substituteStart noGdef run
            expectSameRunAll expected actual
            Expect.isTrue (Object.ReferenceEquals(run, actual)) "no-GDEF input reused"
            Expect.equal (bytesAllocatedBy (fun () -> OtSubstitute.substituteStart noGdef run |> ignore)) 0L "no-GDEF bytes"
            let withGdef = roboto.Value
            expectSameRunAll (substituteStartReference withGdef run) (OtSubstitute.substituteStart withGdef run)
        }
    ]
