module Farfbuzz.Tests.MorxStateMachineTests

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

/// <summary>Builds a single-table sfnt around a table.</summary>
/// <param name="tag">The table tag.</param>
/// <param name="table">The table bytes.</param>
/// <returns>The file bytes.</returns>
let private sfntOf (tag: Tag) (table: byte array) =
    Array.concat [ be32 0x00010000u; be16 1u; be16 0u; be16 0u; be16 0u; be32 (uint32 tag); be32 0u; be32 28u; be32 (uint32 table.Length); table ]

/// <summary>Builds a face from a mort table, with the given glyph count.</summary>
/// <param name="mort">The mort table bytes.</param>
/// <param name="glyphCount">The face glyph count.</param>
/// <returns>The face.</returns>
let private mortFaceOf (mort: byte array) (glyphCount: int) : OtFace =
    OtFace.create (Face.withGlyphCount glyphCount (Face.create (ReadOnlyMemory (sfntOf Tags.Mort mort)) 0u))

/// <summary>Builds one obsolete entry: byte-offset new state, flags, mark index and current index.</summary>
/// <param name="flags">The entry flags.</param>
/// <param name="data0">The first data word.</param>
/// <param name="data1">The second data word.</param>
/// <returns>The entry bytes.</returns>
let private entry (flags: uint16) (data0: uint16) (data1: uint16) =
    Array.concat [ be16 18u; be16 (uint32 flags); be16 (uint32 data0); be16 (uint32 data1) ]

/// <summary>A mort contextual table whose class 1 entry is actionable, so its initial glyph set is exactly glyph 1.</summary>
/// <returns>The mort table bytes.</returns>
let private contextualMortTable () : byte array =
    let classBytes = Array.concat [ be16 0u; be16 4u; [| 0uy; 1uy; 2uy; 3uy |] ]
    let entries = Array.concat [ entry 0us 0us 0us; entry 0us 1us 2us; entry 0us 0us 0us; entry 0us 0us 0us ]
    let subs = Array.concat [ be16 10u; be16 20u; be16 30u ]
    let body = Array.concat [ be16 4u; be16 10u; be16 18u; be16 22u; be16 54u; classBytes; [| 0uy; 1uy; 2uy; 3uy |]; entries; subs ]
    let subtable = Array.concat [ be16 (uint32 (8 + body.Length)); be16 1u; be32 1u; body ]
    let chain = Array.concat [ be32 1u; be32 (uint32 (12 + subtable.Length)); be16 0u; be16 1u; subtable ]
    Array.concat [ be16 1u; be16 0u; be32 1u; chain ]

/// <summary>A mort rearrangement table whose class 1 entry carries a verb, so its initial glyph set is exactly glyph 1.</summary>
/// <returns>The mort table bytes.</returns>
let private rearrangementMortTable () : byte array =
    let classBytes = Array.concat [ be16 0u; be16 4u; [| 0uy; 1uy; 2uy; 3uy |] ]
    let entries = Array.concat [ entry 0us 0us 0us; entry 0x0001us 0us 0us; entry 0us 0us 0us; entry 0us 0us 0us ]
    let body = Array.concat [ be16 4u; be16 8u; be16 16u; be16 20u; classBytes; [| 0uy; 1uy; 2uy; 3uy |]; entries ]
    let subtable = Array.concat [ be16 (uint32 (8 + body.Length)); be16 0u; be32 1u; body ]
    let chain = Array.concat [ be32 1u; be32 (uint32 (12 + subtable.Length)); be16 0u; be16 1u; subtable ]
    Array.concat [ be16 1u; be16 0u; be32 1u; chain ]

/// <summary>A run of the given code points, one cluster per glyph, with no flags.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private runOf (codepoints: uint32 array) =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty |> TailCalls.settle

/// <summary>Builds a null-transition machine: one state, every entry non-actionable, every glyph maps to its own class.</summary>
/// <param name="nClasses">The class count, at least 4.</param>
/// <returns>The machine.</returns>
let private nullMachine (nClasses: int) : StateMachine =
    let classBytes = Array.init (4 + nClasses) (fun i -> if i < 4 then 0uy else byte (i - 4))
    { ClassCount = nClasses
      Classes = Narrow { ClassTable.Table = ReadOnlyMemory classBytes; At = 0; FirstGlyph = 0us; Count = nClasses; Wide = false }
      States = Sealed.init nClasses (fun i -> uint16 i)
      StateBias = 0
      RowCount = 1
      Entries = Sealed.init nClasses (fun _ -> { NewState = 0; Flags = 0us; Data0 = 0us; Data1 = 0us })
      IsExtended = false }

/// <summary>The run fields <see cref="Sealed.runPassWithState"/> reads for a null-transition pass.</summary>
/// <param name="count">The number of glyphs.</param>
/// <returns>The run fields.</returns>
let private nullInput (count: int) : RunInput =
    let infos = Sealed.init count (fun i -> { Codepoint = uint32 (i % 4); Mask = 0u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
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

/// <summary>The P8.S07 rearrangement and contextual allocation tests.</summary>
[<Tests>]
let tests =
    testList "P8.S07" [
        test "a skipped contextual subtable allocates nothing" {
            let face = mortFaceOf (contextualMortTable ()) 16
            let map = AatMap.defaults face { Direction = Direction.Ltr; Script = 0u<script>; Language = Language.NoLanguage }
            let run = runOf [| 0u |]
            let table = Morx.load face.Face true
            MorxApply.apply table map face run.SegmentProperties run |> ignore
            let measured () = MorxApply.apply table map face run.SegmentProperties run |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a non-intersecting contextual subtable costs no allocation"
        }
        test "a skipped rearrangement subtable allocates nothing" {
            let face = mortFaceOf (rearrangementMortTable ()) 16
            let map = AatMap.defaults face { Direction = Direction.Ltr; Script = 0u<script>; Language = Language.NoLanguage }
            let run = runOf [| 0u |]
            let table = Morx.load face.Face true
            MorxApply.apply table map face run.SegmentProperties run |> ignore
            let measured () = MorxApply.apply table map face run.SegmentProperties run |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a non-intersecting rearrangement subtable costs no allocation"
        }
        test "a contextual driver that changes nothing allocates nothing" {
            let machine = nullMachine 5
            let config =
                { Machine = machine
                  Ranges = ImmutableArray.Empty
                  SubtableFlags = 0u
                  NumGlyphs = 128
                  StartSafeToBreakAtEndOfText = true }
            let kind =
                { ContextualKind.Extended = false
                  Body = ReadOnlyMemory.Empty
                  SubstitutionTablesAt = 0
                  NumGlyphs = 128
                  Context = { Gdef = Gdef.load Face.empty } }
            let seed: DriverState<ContextualContext> =
                { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxContextual.seed false }
            let input = nullInput 1000
            let pass = { StateMachinePass.Config = config; Kind = kind }
            let measured () = Sealed.runPassWithState input pass seed |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a null-transition contextual pass allocates nothing"
        }
        test "a rearrangement driver that changes nothing allocates nothing" {
            let machine = nullMachine 5
            let config =
                { Machine = machine
                  Ranges = ImmutableArray.Empty
                  SubtableFlags = 0u
                  NumGlyphs = 128
                  StartSafeToBreakAtEndOfText = true }
            let seed: DriverState<RearrangementContext> =
                { Phase = Start; State = 0; Range = 0; SkipEndOfText = false; Context = MorxRearrangement.seed }
            let input = nullInput 1000
            let pass = { StateMachinePass.Config = config; Kind = RearrangementKind() }
            let measured () = Sealed.runPassWithState input pass seed |> ignore
            Expect.equal (bytesAllocatedBy measured) 0L "a null-transition rearrangement pass allocates nothing"
        }
    ]
