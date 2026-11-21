module Farfbuzz.Tests.AatInsertionTests

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

/// <summary>A null insertion entry: no insertion and no flags.</summary>
let private nullEntry = { NewState = 0; Flags = 0us; Data0 = 0xFFFFus; Data1 = 0xFFFFus }

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

/// <summary>Builds the insertion kind over a glyph list, the list at the start of the body.</summary>
/// <param name="glyphs">The insertion glyphs.</param>
/// <returns>The kind.</returns>
let private kindOf (glyphs: uint16 array) =
    let body = glyphs |> Array.collect (fun g -> be16 (uint32 g))
    { InsertionKind.Body = ReadOnlyMemory body
      InsertionActionAt = 0L
      Context = { Gdef = Gdef.load Face.empty } }

/// <summary>Runs the insertion driver over a text with a length limit and returns the finished run and driver state.</summary>
/// <param name="maxLen">The length limit.</param>
/// <param name="machine">The machine.</param>
/// <param name="kind">The kind.</param>
/// <param name="codepoints">The code points.</param>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The run result and the driver state.</returns>
let private runInsertionLimited (maxLen: int) (machine: StateMachine) (kind: InsertionKind) (codepoints: uint32 array) (maxOps: int) =
    let infos = Sealed.init codepoints.Length (fun i -> { Codepoint = codepoints[i]; Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init codepoints.Length (fun _ -> Unchecked.defaultof<GlyphProps>)
    let input: RunInput =
        { Infos = infos
          Props = props
          Positions = ImmutableArray.Empty
          HavePositions = false
          MaxLen = maxLen
          MaxOps = maxOps
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
    let seed: DriverState<InsertionContext> = { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxInsertion.seed false }
    Sealed.runPassWithState input { StateMachinePass.Config = config; Kind = kind } seed

/// <summary>Runs the insertion driver over a text and returns the finished run and driver state.</summary>
/// <param name="machine">The machine.</param>
/// <param name="kind">The kind.</param>
/// <param name="codepoints">The code points.</param>
/// <param name="maxOps">The operation budget.</param>
/// <returns>The run result and the driver state.</returns>
let private runInsertion (machine: StateMachine) (kind: InsertionKind) (codepoints: uint32 array) (maxOps: int) =
    runInsertionLimited GlyphRun.MaxLenDefault machine kind codepoints maxOps

/// <summary>Builds a single-table sfnt around a table.</summary>
/// <param name="tag">The table tag.</param>
/// <param name="table">The table bytes.</param>
/// <returns>The file bytes.</returns>
let private sfntOf (tag: Tag) (table: byte array) =
    Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u; be32 (uint32 tag); be32 0u; be32 28u; be32 (uint32 table.Length); table ]

/// <summary>A mort table with one chain holding one insertion subtable whose class 1 entry is action-initiable, so its
/// initial glyph set is exactly glyph 1.</summary>
/// <returns>The mort table bytes.</returns>
let private insertionMortTable () =
    let classBytes = Array.concat [ be16 0u; be16 4u; [| 0uy; 1uy; 2uy; 3uy |] ]
    let entry (newState: uint32) (flags: uint32) (data0: uint32) (data1: uint32) =
        Array.concat [ be16 newState; be16 flags; be16 data0; be16 data1 ]
    let entries =
        Array.concat
            [ entry 18u 0u 0xFFFFu 0xFFFFu
              entry 18u 0x8000u 0xFFFFu 0xFFFFu
              entry 18u 0u 0xFFFFu 0xFFFFu
              entry 18u 0u 0xFFFFu 0xFFFFu ]
    let body =
        Array.concat
            [ be16 4u; be16 10u; be16 18u; be16 22u; be16 54u
              classBytes
              [| 0uy; 1uy; 2uy; 3uy |]
              entries
              be16 10u ]
    let subtable = Array.concat [ be16 (uint32 (8 + body.Length)); be16 5u; be32 1u; body ]
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

/// <summary>The P8.S09 insertion unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S09" [
        test "insertion before the current glyph leaves the inserted glyph's props on it" {
            let entries =
                [| nullEntry
                   nullEntry
                   nullEntry
                   nullEntry
                   { NewState = 0; Flags = 0x0820us; Data0 = 0us; Data1 = 0xFFFFus } |]
            let machine = singleClassMachine entries
            let kind = kindOf [| 0xFFFFus |]
            let struct (result, _) = runInsertion machine kind [| 0u |] GlyphRun.MaxOpsDefault
            Expect.equal result.Infos.Length 2 "the inserted glyph and the current glyph remain"
            Expect.equal result.Infos[0].Codepoint 0xFFFFu "the inserted deleted glyph comes first"
            Expect.equal result.Infos[1].Codepoint 0u "the current glyph follows"
            Expect.equal (UnicodeProps.generalCategory result.Props[1]) GeneralCategory.Format "the current glyph took the inserted glyph's deleted category"
            Expect.equal (result.Props[1].UnicodeProps &&& UnicodeProps.CfAatDeleted) UnicodeProps.CfAatDeleted "the current glyph is marked AAT-deleted"
        }
        test "insertion at the end of the text, before and after a glyph and at a mark, raises nothing with every length limit from 0 to the length plus 12" {
            let endAfter = { nullEntry with Flags = 0x0060us; Data0 = 0us }
            let endBefore = { nullEntry with Flags = 0x0860us; Data0 = 0us }
            let glyphBefore = { nullEntry with Flags = 0x0860us; Data0 = 0us }
            let marked = { nullEntry with Flags = 0x8002us; Data1 = 0us }
            let variants =
                [ [| endAfter; nullEntry; nullEntry; nullEntry; nullEntry |]
                  [| endBefore; nullEntry; nullEntry; nullEntry; nullEntry |]
                  [| nullEntry; nullEntry; nullEntry; nullEntry; glyphBefore |]
                  [| endAfter; nullEntry; nullEntry; nullEntry; marked |]
                  [| endBefore; nullEntry; nullEntry; nullEntry; { marked with Flags = 0x8402us } |] ]
            for entries in variants do
                let machine = singleClassMachine entries
                let kind = kindOf [| 10us; 11us; 12us |]
                for length in 1 .. 4 do
                    for limit in 0 .. length + 12 do
                        runInsertionLimited limit machine kind (Array.zeroCreate length) GlyphRun.MaxOpsDefault |> ignore
        }
        test "budget exhaustion skips the mark update" {
            let entries =
                [| nullEntry
                   nullEntry
                   nullEntry
                   nullEntry
                   { NewState = 0; Flags = 0x8001us; Data0 = 0xFFFFus; Data1 = 0us } |]
            let machine = singleClassMachine entries
            let kind = kindOf [| 10us |]
            let struct (_, finalState) = runInsertion machine kind [| 0u |] 0
            Expect.equal finalState.Context.Mark 0 "the SetMark update is skipped when the budget is spent"
        }
        test "a skipped insertion subtable allocates nothing" {
            let face = mortFaceOf (insertionMortTable ()) 16
            let map = AatMap.defaults face { Direction = Direction.Ltr; Script = 0u<script>; Language = Language.NoLanguage }
            let run = runOf [| 0u |]
            let table = Morx.load face.Face true
            MorxApply.apply table map face run.SegmentProperties run |> ignore
            let measured () = MorxApply.apply table map face run.SegmentProperties run |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a non-intersecting insertion subtable costs no allocation"
        }
        test "a non-actionable insertion transition allocates nothing" {
            let kind = kindOf [||]
            let context = MorxInsertion.seed false
            let measured () =
                let mutable cursor =
                    RunCursor(ReadOnlySpan.Empty, ReadOnlySpan.Empty, 0, ReadOnlySpan.Empty, ReadOnlySpan.Empty,
                              ReadOnlySpan.Empty, true, ClusterLevel.MonotoneGraphemes, BufferFlags.Default, 0uy, 0u,
                              GlyphRun.MaxOpsDefault, true)
                MorxInsertion.transition kind.Body kind.InsertionActionAt kind.Context &cursor nullEntry context |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a non-actionable entry runs without allocating"
        }
        test "a DontAdvance insertion machine ends when the budget is spent" {
            let entries =
                [| nullEntry
                   nullEntry
                   nullEntry
                   nullEntry
                   { NewState = 0; Flags = 0x4000us; Data0 = 0xFFFFus; Data1 = 0xFFFFus } |]
            let machine = singleClassMachine entries
            let kind = kindOf [||]
            let config =
                { Machine = machine
                  Ranges = ImmutableArray.Empty
                  SubtableFlags = 0u
                  NumGlyphs = 128
                  StartSafeToBreakAtEndOfText = true }
            let infos = Sealed.init 1 (fun _ -> { Codepoint = 0u; Mask = 0u; Cluster = 0u; Var1 = 0u; Var2 = 0u })
            let props = Sealed.init 1 (fun _ -> Unchecked.defaultof<GlyphProps>)
            let run = { GlyphRun.empty with Infos = infos; Props = props; MaxOps = 3 }
            let result =
                GlyphRun.runPass { StateMachinePass.Config = config; Kind = kind }
                    ({ Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxInsertion.seed false }) run
            Expect.isLessThan result.MaxOps 0 "the budget is spent"
            Expect.isTrue result.Successful "the epsilon loop ends by force-advancing, not by failing the buffer"
        }
    ]
