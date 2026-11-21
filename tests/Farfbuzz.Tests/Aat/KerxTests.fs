module Farfbuzz.Tests.AatKerxTests

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Aat
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.Allocation

/// <summary>Writes a big-endian int16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private beI16 (value: int16) : byte array =
    [| byte (uint16 value >>> 8); byte value |]

/// <summary>Writes a big-endian uint16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) : byte array =
    SfntBuilder.be16 value

/// <summary>Writes a big-endian uint32 as four bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) : byte array =
    SfntBuilder.be32 value

/// <summary>Builds a face whose only table is a kerx table, with the given glyph count.</summary>
/// <param name="kerx">The kerx table bytes.</param>
/// <param name="glyphCount">The glyph count.</param>
/// <returns>The face.</returns>
let private kerxFace (kerx: byte array) (glyphCount: int) : OtFace =
    let file = sfnt 0x00010000u [ Tags.Kerx, kerx ]
    OtFace.create (Face.withGlyphCount glyphCount (Face.create (ReadOnlyMemory file) 0u))

/// <summary>Builds a plan that applies the kerx table.</summary>
/// <returns>The plan.</returns>
let private kerxPlan () : OtShapePlan =
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

/// <summary>Builds a horizontal run of the given glyph ids with 100-unit advances.</summary>
/// <param name="glyphs">The glyph ids.</param>
/// <returns>The run.</returns>
let private kerxRun (glyphs: uint32 array) : GlyphRun =
    let infos = Sealed.init glyphs.Length (fun i -> { Codepoint = glyphs[i]; Mask = 0x80000000u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init glyphs.Length (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = 0us })
    let positions = Sealed.init glyphs.Length (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>Builds a format 0 kerx subtable.</summary>
/// <param name="coverage">The coverage field.</param>
/// <param name="pairs">The left, right and value of each pair.</param>
/// <returns>The subtable bytes.</returns>
let private format0KerxSubtable (coverage: uint32) (pairs: (uint16 * uint16 * int16) list) : byte array =
    let binSearch = Array.concat [ be32 (uint32 pairs.Length); be32 0u; be32 0u; be32 0u ]
    let pairBytes = pairs |> List.map (fun (l, r, v) -> Array.concat [ be16 (uint32 l); be16 (uint32 r); beI16 v ]) |> Array.concat
    let body = Array.concat [ binSearch; pairBytes ]
    Array.concat [ be32 (uint32 (12 + body.Length)); be32 coverage; be32 0u; body ]

/// <summary>Builds a format 8 lookup of 16-bit values, the extended class table.</summary>
/// <param name="firstGlyph">The first glyph the lookup covers.</param>
/// <param name="values">The lookup values.</param>
/// <returns>The lookup bytes.</returns>
let private format8Lookup (firstGlyph: uint32) (values: uint16 array) : byte array =
    Array.concat [ be16 8u; be16 firstGlyph; be16 (uint32 values.Length); values |> Array.collect (fun c -> be16 (uint32 c)) ]

/// <summary>Builds a format 2 kerx subtable.</summary>
/// <param name="leftFirstGlyph">The left class lookup's first glyph.</param>
/// <param name="leftClasses">The left class values.</param>
/// <param name="rightFirstGlyph">The right class lookup's first glyph.</param>
/// <param name="rightClasses">The right class values.</param>
/// <param name="arrayValues">The kerning array.</param>
/// <returns>The subtable bytes.</returns>
let private format2KerxSubtable (leftFirstGlyph: uint32) (leftClasses: uint16 array) (rightFirstGlyph: uint32) (rightClasses: uint16 array) (arrayValues: int16 array) : byte array =
    let left = format8Lookup leftFirstGlyph leftClasses
    let right = format8Lookup rightFirstGlyph rightClasses
    let leftOffset = 28u
    let rightOffset = leftOffset + uint32 left.Length
    let arrayOffset = rightOffset + uint32 right.Length
    let structBytes = Array.concat [ be32 2u; be32 leftOffset; be32 rightOffset; be32 arrayOffset ]
    let body = Array.concat [ structBytes; left; right; arrayValues |> Array.collect beI16 ]
    Array.concat [ be32 (uint32 (12 + body.Length)); be32 2u; be32 0u; body ]

/// <summary>Builds a format 0 lookup of 32-bit values.</summary>
/// <param name="values">The values.</param>
/// <returns>The lookup bytes.</returns>
let private format0Lookup32 (values: uint32 array) : byte array =
    Array.concat [ be16 0u; values |> Array.collect be32 ]

/// <summary>Builds a long format 6 kerx subtable.</summary>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="rowValues">The row index-table values, one per glyph.</param>
/// <param name="columnValues">The column index-table values, one per glyph.</param>
/// <param name="arrayValues">The kerning array of 32-bit values.</param>
/// <returns>The subtable bytes.</returns>
let private format6LongKerxSubtable (numGlyphs: int) (rowValues: uint32 array) (columnValues: uint32 array) (arrayValues: uint32 array) : byte array =
    let row = format0Lookup32 rowValues
    let column = format0Lookup32 columnValues
    let rowOffset = 36u
    let columnOffset = rowOffset + uint32 row.Length
    let arrayOffset = columnOffset + uint32 column.Length
    let vectorOffset = arrayOffset + uint32 (arrayValues.Length * 4)
    let structBytes = Array.concat [ be32 1u; be16 (uint32 numGlyphs); be16 (uint32 numGlyphs); be32 rowOffset; be32 columnOffset; be32 arrayOffset; be32 vectorOffset ]
    let body = Array.concat [ structBytes; row; column; arrayValues |> Array.collect be32 ]
    Array.concat [ be32 (uint32 (12 + body.Length)); be32 6u; be32 0u; body ]

/// <summary>Reads one subtable's kerning through its pair provider.</summary>
/// <param name="subtable">The subtable.</param>
/// <param name="numGlyphs">The face glyph count.</param>
/// <param name="left">The left glyph.</param>
/// <param name="right">The right glyph.</param>
/// <returns>The kerning.</returns>
let private pairKerning (subtable: KerxSubtable) (numGlyphs: int) (left: uint32) (right: uint32) =
    (KerxPairs.KerxPairProvider(subtable, numGlyphs) :> Kern.IKernProvider).GetKerning (left, right)

/// <summary>A copy of a run whose infos, properties and positions share nothing with it.</summary>
/// <param name="run">The run.</param>
/// <returns>The copy.</returns>
let private separateCopy (run: GlyphRun) =
    { run with
        Infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
        Props = ImmutableArray.CreateRange (run.Props.AsSpan().ToArray())
        Positions = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray()) }

/// <summary>Whether two runs agree on every field the kerx pass writes.</summary>
/// <param name="expected">The run the copying route produced.</param>
/// <param name="actual">The run the in-place route produced.</param>
/// <returns>True when they agree.</returns>
let private sameKerxed (expected: GlyphRun) (actual: GlyphRun) =
    expected.Infos.AsSpan().ToArray() = actual.Infos.AsSpan().ToArray()
    && expected.Props.AsSpan().ToArray() = actual.Props.AsSpan().ToArray()
    && expected.Positions.AsSpan().ToArray() = actual.Positions.AsSpan().ToArray()
    && struct (expected.MaxOps, expected.Successful, expected.ScratchFlags, expected.Serial, expected.HavePositions)
       = struct (actual.MaxOps, actual.Successful, actual.ScratchFlags, actual.Serial, actual.HavePositions)

/// <summary>A random run for the kerx pass: glyphs below eight, masks, marks, positions, the flags and the direction.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The run.</returns>
let private randomKerxRun (rnd: Random) : GlyphRun =
    let count = rnd.Next(0, 14)
    let run = kerxRun (Array.init count (fun _ -> uint32 (rnd.Next(0, 8))))
    { run with
        Infos = Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = (match rnd.Next 6 with | 0 -> 0u | _ -> info.Mask) }) (run.Infos.AsSpan())
        Positions = Sealed.mapi (fun _ (position: GlyphPosition) -> { position with XAdvance = rnd.Next(50, 700) }) (run.Positions.AsSpan())
        MaxOps = 1000
        Flags = (match rnd.Next 3 with | 0 -> BufferFlags.ProduceUnsafeToConcat | _ -> BufferFlags.Default)
        SegmentProperties = { Direction = (match rnd.Next 4 with | 0 -> Direction.Rtl | _ -> Direction.Ltr); Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>The P8.S10 kerx unit tests.</summary>
[<Tests>]
let tests =
    testList "P8.S10" [
        test "format 2 gates an unmapped left glyph out of the matrix" {
            let subtable = format2KerxSubtable 0u [| 0us |] 0u [| 0us |] [| 500s |]
            let face = kerxFace (Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]) 16
            let sub = (Kerx.load face.Face).Subtables[0]
            Expect.equal (pairKerning sub 16 0u 0u) 500 "a mapped pair reads the array"
            Expect.equal (pairKerning sub 16 1u 0u) 0 "an unmapped left glyph is gated out"
        }
        test "format 6 long overflow returns zero" {
            let rowValues = Array.init 16 (fun i -> if i = 0 then 0x40000000u else 0u)
            let columnValues = Array.init 16 (fun _ -> 0u)
            let subtable = format6LongKerxSubtable 16 rowValues columnValues [| 500u |]
            let face = kerxFace (Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]) 16
            let sub = (Kerx.load face.Face).Subtables[0]
            Expect.equal (pairKerning sub 16 1u 0u) 500 "a small offset reads the array"
            Expect.equal (pairKerning sub 16 0u 0u) 0 "the long offset overflow returns zero"
        }
        test "the last subtable reads to the end of the table" {
            let binSearch = Array.concat [ be32 1u; be32 0u; be32 0u; be32 0u ]
            let pair = Array.concat [ be16 1u; be16 1u; beI16 500s ]
            let subtable = Array.concat [ be32 12u; be32 0u; be32 0u; binSearch; pair ]
            let face = kerxFace (Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]) 16
            Expect.equal (Kerx.hasData (Kerx.load face.Face)) true "a truncated last subtable still loads"
            let result = AatLayout.position (kerxPlan ()) (Font.create face.Face) (kerxRun [| 1u; 1u |])
            Expect.equal result.Positions[0].XAdvance 350 "the left advance is kerned"
            Expect.equal result.Positions[1].XAdvance 350 "the right advance is kerned"
        }
        test "kerx pair subtables on arrays the caller owns equal the copying route on every field, in any subtable order and coverage" {
            let rnd = Random 8101
            let failures =
                [ for round in 1 .. 1500 do
                    let subtables =
                        [ for _ in 1 .. rnd.Next(1, 4) ->
                            let pairs = [ for _ in 1 .. rnd.Next(1, 10) -> uint16 (rnd.Next(1, 6)), uint16 (rnd.Next(1, 6)), int16 (rnd.Next(-300, 300)) ]
                            let coverage = match rnd.Next 5 with | 0 -> 0x40000000u | 1 -> 0x10000000u | _ -> 0u
                            format0KerxSubtable coverage (match rnd.Next 2 with | 0 -> pairs |> List.distinctBy (fun (left, right, _) -> left, right) |> List.sortBy (fun (left, right, _) -> left, right) | _ -> pairs) ]
                    let table = Array.concat ([ be16 2u; be16 0u; be32 (uint32 subtables.Length) ] @ subtables)
                    let font = Font.create (kerxFace table 16).Face
                    let run = randomKerxRun rnd
                    let shared = AatLayout.position (kerxPlan ()) font run
                    let owned = AatLayout.positionOwned (kerxPlan ()) font (separateCopy run)
                    if not (sameKerxed shared owned) then yield round ]
            Expect.equal (failures |> List.truncate 5) [] "the in-place kerx pass equals the copying one"
        }
        test "a kerx pair pass on arrays the caller owns writes those arrays" {
            let subtable = format0KerxSubtable 0u [ 1us, 1us, 500s ]
            let font = Font.create (kerxFace (Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]) 16).Face
            let run = separateCopy (kerxRun [| 1u; 1u; 2u; 3u |])
            let result = AatLayout.positionOwned (kerxPlan ()) font run
            Expect.equal result.Positions[0].XAdvance 350 "the left advance is kerned"
            Expect.isTrue (obj.ReferenceEquals (ImmutableCollectionsMarshal.AsArray run.Positions, ImmutableCollectionsMarshal.AsArray result.Positions)) "the positions are the caller's array"
            Expect.isTrue (obj.ReferenceEquals (ImmutableCollectionsMarshal.AsArray run.Infos, ImmutableCollectionsMarshal.AsArray result.Infos)) "the infos are the caller's array"
        }
        test "a skipped subtable allocates nothing" {
            let subtable = format0KerxSubtable 0u [ 1us, 1us, 500s ]
            let face = kerxFace (Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]) 16
            let plan = kerxPlan ()
            let font = Font.create face.Face
            let run = kerxRun [| 0u |]
            AatLayout.position plan font run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> AatLayout.position plan font run |> ignore)) 0L "a non-intersecting subtable allocates nothing"
        }
        test "a pair pass that changes nothing allocates nothing" {
            let subtable = format0KerxSubtable 0u [ 1us, 1us, 500s ]
            let face = kerxFace (Array.concat [ be16 2u; be16 0u; be32 1u; subtable ]) 16
            let plan = kerxPlan ()
            let font = Font.create face.Face
            let sub = (Kerx.load face.Face).Subtables[0]
            let provider = KerxPairs.KerxPairProvider(sub, 16)
            let run = kerxRun [| 1u; 2u |]
            Kern.applyProvider plan font true false provider run |> ignore
            Expect.equal (bytesAllocatedBy (fun () -> Kern.applyProvider plan font true false provider run |> ignore)) 0L "a run without a kern pair allocates nothing"
        }
    ]
