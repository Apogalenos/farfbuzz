module Farfbuzz.Tests.RunPassAatTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz

/// <summary>Builds a run whose props and positions are marked by their index, so a reorder is visible.</summary>
/// <param name="cps">The code points.</param>
/// <returns>The marked run.</returns>
let private markedRun (cps: uint32 array) : GlyphRun =
    let infos = Sealed.init cps.Length (fun i -> { Codepoint = cps[i]; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init cps.Length (fun i -> { Unchecked.defaultof<GlyphProps> with GlyphIndex = uint32 i })
    let positions = Sealed.init cps.Length (fun i -> { XAdvance = i; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with Infos = infos; Props = props; Positions = positions; HavePositions = true }

/// <summary>Builds a run with empty props and no positions.</summary>
/// <param name="cps">The code points.</param>
/// <returns>The plain run.</returns>
let private plainRun (cps: uint32 array) : GlyphRun =
    let infos = Sealed.init cps.Length (fun i -> { Codepoint = cps[i]; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init cps.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
    { GlyphRun.empty with Infos = infos; Props = props }

/// <summary>A pass that replays a fixed script of edits, then finishes.</summary>
[<Struct>]
type private ScriptPass =
    val Edits: RunEdit array
    new (edits) = { Edits = edits }
    interface IRunPass<int> with
        member this.Step (_, index) =
            match index < this.Edits.Length with
            | true -> struct (this.Edits[index], index + 1)
            | false -> struct (RunEdit.Finish, index)

/// <summary>Runs a script of edits over a run.</summary>
/// <param name="run">The run.</param>
/// <param name="edits">The edits.</param>
/// <returns>The shaped run.</returns>
let private runScript (run: GlyphRun) (edits: RunEdit array) =
    GlyphRun.runPass (ScriptPass(edits)) 0 run

/// <summary>The code points of a run, in order.</summary>
/// <param name="run">The run.</param>
/// <returns>The code points.</returns>
let private codepoints (run: GlyphRun) =
    run.Infos |> Seq.map (fun info -> info.Codepoint) |> Seq.toArray

/// <summary>The glyph indices of a run's props, in order.</summary>
/// <param name="run">The run.</param>
/// <returns>The glyph indices.</returns>
let private glyphIndices (run: GlyphRun) =
    run.Props |> Seq.map (fun props -> props.GlyphIndex) |> Seq.toArray

/// <summary>The x advances of a run's positions, in order.</summary>
/// <param name="run">The run.</param>
/// <returns>The x advances.</returns>
let private xAdvances (run: GlyphRun) =
    run.Positions |> Seq.map (fun position -> position.XAdvance) |> Seq.toArray

/// <summary>A pass that advances one glyph per step, changing nothing.</summary>
[<Struct>]
type private IdentityPass =
    interface IRunPass<int> with
        member _.Step (cursor, state) =
            match state < cursor.Input.Length with
            | true -> struct (RunEdit.NextGlyph, state + 1)
            | false -> struct (RunEdit.Finish, state)

/// <summary>Builds the run fields <see cref="Sealed.runPassWithState"/> reads.</summary>
/// <param name="count">The number of glyphs.</param>
/// <returns>The run fields.</returns>
let private runInput (count: int) : RunInput =
    let infos = Sealed.init count (fun i -> { Codepoint = uint32 (i % 128); Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init count (fun _ -> Unchecked.defaultof<GlyphProps>)
    { Infos = infos
      Props = props
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

/// <summary>The P8.S03 transducer-case unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S03" [
        test "ChargeOps reduces the budget" {
            let run = plainRun [| 1u; 2u; 3u |]
            let result = runScript run [| RunEdit.ChargeOps 2 |]
            Expect.equal result.MaxOps (GlyphRun.MaxOpsDefault - 2) "charged two operations"
            Expect.equal result.Successful true "still successful"
        }
        test "Fail marks the run unsuccessful" {
            let run = plainRun [| 1u; 2u |]
            let result = runScript run [| RunEdit.Fail |]
            Expect.equal result.Successful false "failed"
        }
        test "SetInputGlyph rewrites the code point and props" {
            let run = plainRun [| 1u; 2u; 3u |]
            let props = { Unchecked.defaultof<GlyphProps> with GlyphIndex = 42u }
            let result = runScript run [| RunEdit.SetInputGlyph (1, 99u, props) |]
            Expect.equal result.Infos[1].Codepoint 99u "codepoint"
            Expect.equal result.Props[1] props "props"
            Expect.equal result.Infos[0].Codepoint 1u "first glyph untouched"
            Expect.equal result.Infos[2].Codepoint 3u "last glyph untouched"
        }
        test "Rearrange moves infos and props but not positions" {
            let run = markedRun [| 0u; 1u; 2u; 3u; 4u; 5u |]
            let result = runScript run [| RunEdit.Rearrange (1, 5, 1, 2, false, true) |]
            Expect.equal (codepoints result) [| 0u; 4u; 3u; 2u; 1u; 5u |] "rearranged code points"
            Expect.equal (glyphIndices result) [| 0u; 4u; 3u; 2u; 1u; 5u |] "properties moved with the glyphs"
            Expect.equal (xAdvances result) [| 0; 1; 2; 3; 4; 5 |] "positions did not move"
        }
        test "P8.S03 runPassWithState allocation" {
            let input = runInput 1000
            let measure count =
                let before = GC.GetAllocatedBytesForCurrentThread()
                let mutable index = 0
                while index < count do
                    Sealed.runPassWithState input (IdentityPass()) 0 |> ignore
                    index <- index + 1
                GC.GetAllocatedBytesForCurrentThread() - before
            let mutable warm = 0
            while warm < 1000 do
                Sealed.runPassWithState input (IdentityPass()) 0 |> ignore
                warm <- warm + 1
            Expect.equal (measure 1000) 0L "an identity pass allocates nothing beyond the returned state"
        }
    ]
