module Farfbuzz.Tests.AatLigatureTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Harness
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

/// <summary>A null entry leading back to the start state.</summary>
let private nullEntry = { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us }

/// <summary>Builds a decoded state machine whose class table maps every glyph below 5 to class 4, with one entry per
/// state-and-class pair in row-major order.</summary>
/// <param name="entries">The entries.</param>
/// <returns>The machine.</returns>
let private singleClassMachine (entries: Entry array) =
    let nClasses = 5
    let classBytes = Array.init (4 + nClasses) (fun i -> if i < 4 then 0uy else 4uy)
    { ClassCount = nClasses
      Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = nClasses; Wide = false }
      States = Sealed.init entries.Length (fun i -> uint16 i)
      StateBias = 0
      RowCount = entries.Length / nClasses
      Entries = Sealed.init entries.Length (fun i -> entries[i])
      IsExtended = false }

/// <summary>Builds a morx ligature body: an unused state-table header, the three offsets, one action, one component
/// and one ligature.</summary>
/// <param name="action">The action word.</param>
/// <param name="componentValue">The component value.</param>
/// <param name="ligatureValue">The ligature glyph.</param>
/// <returns>The body bytes.</returns>
let private ligatureBody (action: uint32) (componentValue: uint16) (ligatureValue: uint16) =
    Array.concat
        [ Array.zeroCreate 16
          be32 28u
          be32 32u
          be32 34u
          be32 action
          be16 (uint32 componentValue)
          be16 (uint32 ligatureValue) ]

/// <summary>Builds the ligature kind over a body.</summary>
/// <param name="body">The body bytes.</param>
/// <returns>The kind.</returns>
let private kindOf (body: byte array) =
    { Extended = true
      Body = ReadOnlyMemory body
      LigActionAt = 28L
      ComponentAt = 32L
      LigatureAt = 34L
      Context = { Gdef = Gdef.load Face.empty } }

/// <summary>Runs the ligature driver over a text and returns the finished run and driver state.</summary>
/// <param name="machine">The machine.</param>
/// <param name="kind">The kind.</param>
/// <param name="codepoints">The code points.</param>
/// <returns>The run result and the driver state.</returns>
let private runLigature (machine: StateMachine) (kind: LigatureKind) (codepoints: uint32 array) =
    let infos = Sealed.init codepoints.Length (fun i -> { Codepoint = codepoints[i]; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init codepoints.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
    let input: RunInput =
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
    let config =
        { Machine = machine
          Ranges = ImmutableArray.Empty
          SubtableFlags = 0u
          NumGlyphs = 128
          StartSafeToBreakAtEndOfText = true }
    let seed: DriverState<LigatureContext> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxLigature.seed false }
    Sealed.runPassWithState input { StateMachinePass.Config = config; Kind = kind } seed

/// <summary>Builds a single-table sfnt around a table.</summary>
/// <param name="tag">The table tag.</param>
/// <param name="table">The table bytes.</param>
/// <returns>The file bytes.</returns>
let private sfntOf (tag: Tag) (table: byte array) =
    Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u; be32 (uint32 tag); be32 0u; be32 28u; be32 (uint32 table.Length); table ]

/// <summary>A mort table with one chain holding one ligature subtable whose class 1 entry is action-initiable, so its
/// initial glyph set is exactly glyph 1.</summary>
/// <returns>The mort table bytes.</returns>
let private ligatureMortTable () =
    let classBytes = Array.concat [ be16 0u; be16 4u; [| 0uy; 1uy; 2uy; 3uy |] ]
    let entry (newState: uint32) (flags: uint32) = Array.concat [ be16 newState; be16 flags ]
    let entries = Array.concat [ entry 22u 0u; entry 22u 0x8000u; entry 22u 0u; entry 22u 0u ]
    let body =
        Array.concat
            [ be16 4u; be16 14u; be16 22u; be16 26u; be16 42u; be16 46u; be16 48u
              classBytes
              [| 0uy; 1uy; 2uy; 3uy |]
              entries
              be32 0u
              be16 0u
              be16 0u ]
    let subtable = Array.concat [ be16 (uint32 (8 + body.Length)); be16 2u; be32 1u; body ]
    let chain = Array.concat [ be32 1u; be32 (uint32 (12 + subtable.Length)); be16 0u; be16 1u; subtable ]
    Array.concat [ be16 1u; be16 0u; be32 1u; chain ]

/// <summary>Builds a face from a mort table, with the given glyph count.</summary>
/// <param name="mort">The mort table bytes.</param>
/// <param name="glyphCount">The face glyph count.</param>
/// <returns>The face.</returns>
let private mortFaceOf (mort: byte array) (glyphCount: int) =
    OtFace.create (Face.withGlyphCount glyphCount (Face.create (ReadOnlyMemory (sfntOf Tags.Mort mort)) 0u))

/// <summary>Builds a run of the given code points, one cluster per glyph.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private runOf (codepoints: uint32 array) =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty |> TailCalls.settle

/// <summary>The P8.S08 ligature unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S08" [
        test "the ring wraps after 64 pushes" {
            let input = Sealed.init 65 (fun i -> { Codepoint = uint32 i; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 65 (fun _ -> Unchecked.defaultof<GlyphProps>)
            let mutable context = MorxLigature.seed false
            for i in 0 .. 64 do
                let mutable cursor =
                    RunCursor(input.AsSpan(), props.AsSpan(), i, ReadOnlySpan.Empty, ReadOnlySpan.Empty,
                              ReadOnlySpan.Empty, true, ClusterLevel.MonotoneGraphemes, BufferFlags.Default, 0uy, 0u,
                              GlyphRun.MaxOpsDefault, true)
                context <- MorxLigature.pushComponent &cursor context
            Expect.equal context.MatchLength 65 "the 65th push keeps counting past the ring"
            Expect.equal context.Ring[0] 64u "ring slot 0 is overwritten by the 65th push"
            Expect.equal context.Ring[1] 1u "ring slot 1 keeps the second push"
        }
        test "a stack underflow clears the match" {
            let entries = [| nullEntry; nullEntry; nullEntry; nullEntry; { NewState = 0; Flags = 0xA000us; Data0 = 0us; Data1 = 0us } |]
            let machine = singleClassMachine entries
            let kind = kindOf (ligatureBody 0u 0us 0us)
            let struct (_, finalState) = runLigature machine kind [| 0u |]
            Expect.equal finalState.Context.MatchLength 0 "the underflow clears the component stack"
        }
        test "a single-component ligature forms" {
            let entries = [| nullEntry; nullEntry; nullEntry; nullEntry; { NewState = 0; Flags = 0xA000us; Data0 = 0us; Data1 = 0us } |]
            let machine = singleClassMachine entries
            let kind = kindOf (ligatureBody 0xC0000000u 0us 100us)
            let struct (result, _) = runLigature machine kind [| 0u |]
            Expect.equal result.Infos.Length 1 "one glyph remains"
            Expect.equal result.Infos[0].Codepoint 100u "the glyph is the ligature"
        }
        test "a skipped ligature subtable allocates nothing" {
            let face = mortFaceOf (ligatureMortTable ()) 16
            let map = AatMap.defaults face { Direction = Direction.Ltr; Script = 0u<script>; Language = Language.NoLanguage }
            let run = runOf [| 0u |]
            let table = Morx.load face.Face true
            MorxApply.apply table map face run.SegmentProperties run |> ignore
            let measured () = MorxApply.apply table map face run.SegmentProperties run |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a non-intersecting ligature subtable costs no allocation"
        }
        test "the ligature transition allocates nothing for a non-actionable entry" {
            let kind = kindOf (ligatureBody 0u 0us 0us)
            let context = MorxLigature.seed false
            let measured () =
                let mutable cursor =
                    RunCursor(ReadOnlySpan.Empty, ReadOnlySpan.Empty, 0, ReadOnlySpan.Empty, ReadOnlySpan.Empty,
                              ReadOnlySpan.Empty, true, ClusterLevel.MonotoneGraphemes, BufferFlags.Default, 0uy, 0u,
                              GlyphRun.MaxOpsDefault, true)
                MorxLigature.transition true kind.Body kind.LigActionAt kind.ComponentAt kind.LigatureAt kind.Context &cursor nullEntry context |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a non-actionable entry runs without allocating"
        }
        test "a DontAdvance ligature machine ends when the budget is spent" {
            let entries = [| nullEntry; nullEntry; nullEntry; nullEntry; { NewState = 0; Flags = 0x4000us; Data0 = 0us; Data1 = 0us } |]
            let machine = singleClassMachine entries
            let kind = kindOf (ligatureBody 0u 0us 0us)
            let config =
                { Machine = machine
                  Ranges = ImmutableArray.Empty
                  SubtableFlags = 0u
                  NumGlyphs = 128
                  StartSafeToBreakAtEndOfText = true }
            let infos = Sealed.init 1 (fun _ -> { Codepoint = 0u; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 1 (fun _ -> Unchecked.defaultof<GlyphProps>)
            let run = { GlyphRun.empty with Infos = infos; Props = props; MaxOps = 3 }
            let result = GlyphRun.runPass { StateMachinePass.Config = config; Kind = kind } ({ Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxLigature.seed false }) run
            Expect.isLessThan result.MaxOps 0 "the budget is spent"
            Expect.isTrue result.Successful "the epsilon loop ends by force-advancing, not by failing the buffer"
        }
    ]
