module Farfbuzz.Tests.VariableKernelTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.Shape.SimpleRunTests

/// <summary>The number of random cases of each property.</summary>
[<Literal>]
let private CaseCount = 6000

/// <summary>The seed of the random cases, fixed so a failure replays.</summary>
[<Literal>]
let private Seed = 0x5E7A11UL

/// <summary>The variable font of the perf corpus, whose kern lookup has records of an advance and a variation device.</summary>
let private variable =
    lazy (Font.create (Face.create (UpstreamFiles.read "test/subset/data/fonts/SourceSerifVariable-Roman.ttf") 0u))

/// <summary>The number of axes of the variable font.</summary>
let private axisCount = lazy (Font.varCoordsNormalized variable.Value).Length

/// <summary>A normalized coordinate: the default, the extremes, one inside the range, one beyond it.</summary>
/// <param name="rnd">The random source.</param>
let private coordinate (rnd: Random) =
    match rnd.Next 8 with
    | 0 | 1 -> 0
    | 2 -> 16384
    | 3 -> -16384
    | 4 -> 20000
    | 5 -> -20000
    | _ -> rnd.Next(-16384, 16385)

/// <summary>A text of Latin letters, kerned pairs, digits, punctuation and a few characters that leave the kernel.</summary>
/// <param name="rnd">The random source.</param>
let private text (rnd: Random) =
    let pairs = [| "AV"; "To"; "Ty"; "WA"; "Yo"; "r."; "LT"; "P,"; "AT"; "fi"; "ff"; "AVAV" |]
    let length = rnd.Next(1, 40)
    String.Join(String.Empty, Array.init length (fun _ ->
        match rnd.Next 12 with
        | 0 -> " "
        | 1 -> pairs[rnd.Next pairs.Length]
        | 2 -> string (char (rnd.Next(0x30, 0x3A)))
        | 3 -> string (char (rnd.Next(0x21, 0x30)))
        | 4 -> string (char (rnd.Next(0xC0, 0x250)))
        | 5 -> (if rnd.Next 8 = 0 then "́" else "e")
        | _ -> string (char (rnd.Next(0x41, 0x7B)))))

/// <summary>One case: a font at some coordinates and ppem, a text and a direction.</summary>
/// <param name="seed">The seed.</param>
/// <returns>The font, the run, and whether the font had non-zero coordinates.</returns>
let private build (seed: int) =
    let rnd = Random seed
    let coords = Array.init axisCount.Value (fun _ -> coordinate rnd)
    let withCoords = variable.Value |> Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords)
    let ppem = [| 0u; 0u; 0u; 12u |][rnd.Next 4]
    let upem = withCoords.Face.Upem
    let scale = [| upem; upem; upem / 2; upem * 3 + 7; 17; 2 * upem - 1 |][rnd.Next 6]
    let font = withCoords |> Font.withScale scale scale |> Font.withPpem ppem ppem
    let bytes = Text.Encoding.UTF8.GetBytes (text rnd)
    let run =
        { GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty with Flags = (match rnd.Next 3 with | 0 -> BufferFlags.ProduceUnsafeToConcat | _ -> BufferFlags.Default) }
        |> GlyphRun.guessSegmentProperties Language.cLocale
        |> TailCalls.settle
    struct (font, run, Array.exists (fun coord -> coord <> 0) coords, ppem <> 0u)

/// <summary>Checks one case: the entry that may take the kernel equals the general pipeline in every field.</summary>
/// <param name="seed">The seed.</param>
/// <returns>Whether the font had non-zero coordinates, a ppem, and whether the run's positions differ from its advances.</returns>
let private check (seed: int) =
    let struct (font, run, coords, ppem) = build seed
    match Execute.createPlan font run.SegmentProperties ImmutableArray<Feature>.Empty ValueNone with
    | Error _ -> failtest "no plan"
    | Ok plan ->
        match Execute.runGeneral plan font ImmutableArray<Feature>.Empty run, Execute.run plan font ImmutableArray<Feature>.Empty run with
        | Ok expected, Ok actual ->
            expectSameRunAll $"seed {seed}" expected actual
            Expect.equal actual.Successful expected.Successful $"seed {seed}: successful"
            Expect.equal actual.MaxOps expected.MaxOps $"seed {seed}: operation budget"
            Expect.equal actual.ScratchFlags expected.ScratchFlags $"seed {seed}: scratch flags"
            let kerned = expected.Positions.AsSpan().ToArray() |> Array.exists (fun position -> position.XOffset <> 0 || position.Var <> 0u)
            struct (coords, ppem, kerned)
        | _ -> failtest $"seed {seed}: one entry shaped the run and the other refused it"

/// <summary>The kernel on a variable font, held to the general pipeline.</summary>
[<Tests>]
let variableKernelTests =
    testList "variable font kernel" [
        test "the entry equals the general pipeline in every field on a variable font at random coordinates and ppem" {
            let seeds = FsCheck.FSharp.Gen.sampleWithSeed (FsCheck.Rnd Seed) 30 CaseCount (FsCheck.FSharp.Gen.choose (0, Int32.MaxValue))
            let results = seeds |> Seq.map check |> Seq.toArray
            let count predicate = results |> Array.filter predicate |> Array.length
            printfn $"variable font kernel: {results.Length} cases, {count (fun (struct (coords, _, _)) -> coords)} with non-zero coordinates, {count (fun (struct (_, ppem, _)) -> ppem)} with a ppem"
            Expect.isGreaterThan (count (fun (struct (coords, _, _)) -> coords)) 2000 "many cases have non-zero coordinates"
            Expect.isGreaterThan (count (fun (struct (coords, ppem, _)) -> not coords && not ppem)) 800 "many cases are at the default coordinates, where the devices do nothing"
            Expect.isGreaterThan (count (fun (struct (_, ppem, _)) -> ppem)) 1000 "many cases have a ppem"
            Expect.isGreaterThan (count (fun (struct (coords, ppem, _)) -> coords && not ppem)) 1500 "many cases have non-zero coordinates and no ppem, where the resolved tables serve the line"
        }

        test "the lean loop kerns a line of the variable font at its default coordinates in place, allocating nothing, as the general loop does" {
            let font = variable.Value
            let segment = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
            match Execute.createPlan font segment ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> failtest "the plan"
            | Ok plan ->
                let otPlan = match plan.Ot with | ValueSome otPlan -> otPlan | ValueNone -> failtest "the ot plan"
                let withoutGpos = { plan with GposCovered = ImmutableArray.Empty }
                let bytes = Text.Encoding.UTF8.GetBytes (String.replicate 500 "AV")
                let source = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale |> TailCalls.settle
                let run =
                    match Execute.run withoutGpos font ImmutableArray<Feature>.Empty source with
                    | Ok shaped -> { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                    | Error _ -> failtest "the run"
                let reference = Engine.positionPlannedReference otPlan font plan.GposLookupCovered plan.GposGlyphLookups run
                let infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
                let positions = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray())
                let lean () = Engine.positionOwned plan.GposPairs font infos positions run.Flags run.ClusterLevel run.MaxOps |> TailCalls.settle
                lean () |> ignore
                let fresh = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray())
                let freshInfos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
                let before = GC.GetAllocatedBytesForCurrentThread()
                let left = Engine.positionOwned plan.GposPairs font freshInfos fresh run.Flags run.ClusterLevel run.MaxOps |> TailCalls.settle
                let allocated = GC.GetAllocatedBytesForCurrentThread() - before
                Expect.isTrue (ValueOption.isSome left) "the lean loop applies to the variable font's kern lookup at default coordinates"
                Expect.equal allocated 0L "a kerned line allocates nothing in the lean loop"
                Expect.sequenceEqual (fresh.AsSpan().ToArray()) (reference.Positions.AsSpan().ToArray()) "the positions are the general loop's"
                Expect.sequenceEqual (freshInfos.AsSpan().ToArray()) (reference.Infos.AsSpan().ToArray()) "the infos are the general loop's"
                Expect.notEqual (fresh.AsSpan().ToArray()) (run.Positions.AsSpan().ToArray()) "the line was kerned"
        }

        test "the lean loop kerns a line of the variable font at non-default coordinates in place, allocating nothing, as the general loop does" {
            let coords = ImmutableArray.CreateRange (Array.create axisCount.Value 9000)
            let font = variable.Value |> Font.setVarCoordsNormalized coords |> Font.withScale 1500 1500
            let segment = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = Language.cLocale }
            match Execute.createPlan font segment ImmutableArray<Feature>.Empty ValueNone with
            | Error _ -> failtest "the plan"
            | Ok plan ->
                let otPlan = match plan.Ot with | ValueSome otPlan -> otPlan | ValueNone -> failtest "the ot plan"
                let withoutGpos = { plan with GposCovered = ImmutableArray.Empty }
                let bytes = Text.Encoding.UTF8.GetBytes (String.replicate 500 "AV")
                let source = GlyphRun.addUtf8 (ReadOnlySpan bytes) 0 bytes.Length GlyphRun.empty |> GlyphRun.guessSegmentProperties Language.cLocale |> TailCalls.settle
                let run =
                    match Execute.run withoutGpos font ImmutableArray<Feature>.Empty source with
                    | Ok shaped -> { shaped with Infos = Sealed.withMask otPlan.Map.GlobalMask (shaped.Infos.AsSpan()) }
                    | Error _ -> failtest "the run"
                let reference = Engine.positionPlannedReference otPlan font plan.GposLookupCovered plan.GposGlyphLookups run
                let infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
                let positions = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray())
                Engine.positionOwned plan.GposPairs font infos positions run.Flags run.ClusterLevel run.MaxOps |> TailCalls.settle |> ignore
                let fresh = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray())
                let freshInfos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
                let before = GC.GetAllocatedBytesForCurrentThread()
                let left = Engine.positionOwned plan.GposPairs font freshInfos fresh run.Flags run.ClusterLevel run.MaxOps |> TailCalls.settle
                let allocated = GC.GetAllocatedBytesForCurrentThread() - before
                Expect.isTrue (ValueOption.isSome left) "the lean loop applies to the variable font's kern lookup at non-default coordinates"
                Expect.equal allocated 0L "a kerned line allocates nothing in the lean loop"
                Expect.sequenceEqual (fresh.AsSpan().ToArray()) (reference.Positions.AsSpan().ToArray()) "the positions are the general loop's"
                Expect.sequenceEqual (freshInfos.AsSpan().ToArray()) (reference.Infos.AsSpan().ToArray()) "the infos are the general loop's"
                Expect.notEqual (fresh.AsSpan().ToArray()) (run.Positions.AsSpan().ToArray()) "the line was kerned"
        }

        test "the advances read from the HVAR table for a font equal the advances the general reader gives, at every glyph" {
            let rnd = Random 11
            let font = variable.Value
            let upem = font.Face.Upem
            let checks =
                [ 1 .. 12 ]
                |> List.map (fun _ ->
                    let coords = Array.init axisCount.Value (fun _ -> coordinate rnd)
                    let varied = font |> Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) |> Font.withScale (upem + rnd.Next 900) upem
                    let table = Font.hvarAdvances varied
                    let nonZero = Array.exists (fun coord -> coord <> 0) coords
                    Expect.equal table.IsEmpty (not nonZero) "the table exists exactly when the coordinates are not all zero"
                    table.AsSpan().ToArray() |> Array.iteri (fun glyph advance ->
                        Expect.equal (Font.emScaleBy varied.XMult advance) (Font.hAdvance varied (uint32 glyph)) $"glyph {glyph}")
                    table.Length)
            Expect.isGreaterThan (List.max checks) 0 "a table was built"
        }
    ]
