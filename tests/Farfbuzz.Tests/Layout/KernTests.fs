module Farfbuzz.Tests.Layout.KernTests

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder

/// <summary>Writes a big-endian int16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private beI16 (value: int16) : byte array =
    [| byte (uint16 value >>> 8); byte value |]

/// <summary>Builds a face whose only table is the kern table.</summary>
/// <param name="kern">The kern table bytes.</param>
/// <returns>The face.</returns>
let private kernFace (kern: byte array) : Face =
    Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Kern, kern ])) 0u

/// <summary>Loads the kern table of a face whose only table is the given kern bytes.</summary>
/// <param name="kern">The kern table bytes.</param>
/// <returns>The kern table.</returns>
let private kernTableOf (kern: byte array) : KernTable =
    KernTable.load (kernFace kern)

/// <summary>Builds an OT format 0 kern table from its coverage byte and pairs.</summary>
/// <param name="coverage">The coverage byte.</param>
/// <param name="pairs">The left, right and value of each pair.</param>
/// <returns>The table bytes.</returns>
let private format0Kern (coverage: byte) (pairs: (uint16 * uint16 * int16) list) : byte array =
    let body = pairs |> List.map (fun (l, r, v) -> Array.concat [ be16 (uint32 l); be16 (uint32 r); beI16 v ]) |> Array.concat
    let binSearch = Array.concat [ be16 (uint32 pairs.Length); be16 0u; be16 0u; be16 0u ]
    let subtable = Array.concat [ be16 0u; be16 (uint32 (6 + 8 + body.Length)); [| 0uy; coverage |]; binSearch; body ]
    Array.concat [ be16 0u; be16 1u; subtable ]

/// <summary>Builds a wide class table, first glyph zero.</summary>
/// <param name="classes">The class values.</param>
/// <returns>The class-table bytes.</returns>
let private classTable (classes: uint16 array) : byte array =
    Array.concat [ be16 0u; be16 (uint32 classes.Length); (classes |> Array.collect (fun c -> be16 (uint32 c))) ]

/// <summary>Builds an OT format 2 kern table from its class tables and array.</summary>
/// <param name="leftClasses">The left-hand classes.</param>
/// <param name="rightClasses">The right-hand classes.</param>
/// <param name="arrayValues">The kerning values.</param>
/// <returns>The table bytes.</returns>
let private format2Kern (leftClasses: uint16 array) (rightClasses: uint16 array) (arrayValues: int16 array) : byte array =
    let left = classTable leftClasses
    let right = classTable rightClasses
    let leftOffset = 6 + 8
    let rightOffset = leftOffset + left.Length
    let arrayOffset = rightOffset + right.Length
    let body = Array.concat [ be16 0u; be16 (uint32 leftOffset); be16 (uint32 rightOffset); be16 (uint32 arrayOffset) ]
    let subtable = Array.concat [ be16 0u; be16 (uint32 (6 + 8 + left.Length + right.Length + arrayValues.Length * 2)); [| 2uy; 0x01uy |]; body; left; right; (arrayValues |> Array.collect (fun v -> beI16 v)) ]
    Array.concat [ be16 0u; be16 1u; subtable ]

/// <summary>Builds an OT format 3 kern table from its class arrays.</summary>
/// <param name="glyphCount">The glyph count.</param>
/// <param name="kernValues">The kerning values.</param>
/// <param name="leftClassCount">The left-hand class count.</param>
/// <param name="rightClassCount">The right-hand class count.</param>
/// <param name="leftClass">The left-hand classes, one per glyph.</param>
/// <param name="rightClass">The right-hand classes, one per glyph.</param>
/// <param name="kernIndex">The kerning-value indices.</param>
/// <returns>The table bytes.</returns>
let private format3Kern (glyphCount: int) (kernValues: int16 array) (leftClassCount: int) (rightClassCount: int) (leftClass: byte array) (rightClass: byte array) (kernIndex: byte array) : byte array =
    let body =
        Array.concat
            [ be16 (uint32 glyphCount)
              [| byte kernValues.Length; byte leftClassCount; byte rightClassCount; 0uy |]
              (kernValues |> Array.collect (fun v -> beI16 v))
              leftClass
              rightClass
              kernIndex ]
    let subtable = Array.concat [ be16 0u; be16 (uint32 (6 + body.Length)); [| 3uy; 0x01uy |]; body ]
    Array.concat [ be16 0u; be16 1u; subtable ]

/// <summary>Builds a plan that applies the kern table.</summary>
/// <returns>The plan.</returns>
let private kernPlan () : OtShapePlan =
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
      ApplyKern = true
      ApplyFallbackKern = false
      ApplyKerx = false
      ApplyMorx = false
      ApplyTrak = false; Unicode = ValueNone }

/// <summary>Builds a horizontal run of the given glyph ids with 100-unit advances.</summary>
/// <param name="glyphs">The glyph ids.</param>
/// <returns>The run.</returns>
let private kernRun (glyphs: uint32 array) : GlyphRun =
    let infos = Sealed.init glyphs.Length (fun i -> { Codepoint = glyphs[i]; Mask = 0x80000000u; Cluster = uint32 i; Var1 = 0u; Var2 = 0u })
    let props = Sealed.init glyphs.Length (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = 0us })
    let positions = Sealed.init glyphs.Length (fun _ -> { XAdvance = 100; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        SegmentProperties = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>A run of random glyphs, masks, properties, positions, flags and direction, for the kern machine.</summary>
/// <param name="rnd">The random source.</param>
/// <returns>The run.</returns>
let private randomKernRun (rnd: Random) : GlyphRun =
    let count = rnd.Next(0, 24)
    let infos =
        Sealed.init count (fun index ->
            { Codepoint = uint32 (rnd.Next(0, 8))
              Mask = (match rnd.Next 6 with | 0 -> 0u | _ -> 0x80000000u)
              Cluster = uint32 (index / 2)
              Var1 = 0u
              Var2 = 0u })
    let props = Sealed.init count (fun _ -> { Unchecked.defaultof<GlyphProps> with GlyphClass = (match rnd.Next 6 with | 0 -> GlyphPropsBits.Mark | _ -> 0us) })
    let positions = Sealed.init count (fun _ -> { XAdvance = rnd.Next(50, 700); YAdvance = rnd.Next(-5, 5); XOffset = rnd.Next(-3, 3); YOffset = 0; Var = 0u })
    { GlyphRun.empty with
        Infos = infos
        Props = props
        Positions = positions
        HavePositions = true
        MaxOps = 1000
        Flags = (match rnd.Next 3 with | 0 -> BufferFlags.ProduceUnsafeToConcat | _ -> BufferFlags.Default)
        SegmentProperties = { Direction = (match rnd.Next 5 with | 0 -> Direction.Rtl | _ -> Direction.Ltr); Script = Scripts.Latin; Language = NoLanguage } }

/// <summary>A copy of a run whose infos, properties and positions share nothing with it.</summary>
/// <param name="run">The run.</param>
/// <returns>The copy.</returns>
let private separateCopy (run: GlyphRun) =
    { run with
        Infos = ImmutableArray.CreateRange (run.Infos.AsSpan().ToArray())
        Props = ImmutableArray.CreateRange (run.Props.AsSpan().ToArray())
        Positions = ImmutableArray.CreateRange (run.Positions.AsSpan().ToArray()) }

/// <summary>Whether two runs agree on every field the kern machine writes.</summary>
/// <param name="expected">The run the copying route produced.</param>
/// <param name="actual">The run the in-place route produced.</param>
/// <returns>True when they agree.</returns>
let private sameKerned (expected: GlyphRun) (actual: GlyphRun) =
    expected.Infos.AsSpan().ToArray() = actual.Infos.AsSpan().ToArray()
    && expected.Props.AsSpan().ToArray() = actual.Props.AsSpan().ToArray()
    && expected.Positions.AsSpan().ToArray() = actual.Positions.AsSpan().ToArray()
    && struct (expected.MaxOps, expected.Successful, expected.ScratchFlags, expected.Serial, expected.HavePositions)
       = struct (actual.MaxOps, actual.Successful, actual.ScratchFlags, actual.Serial, actual.HavePositions)

/// <summary>The P5.S13 kern-table unit tests.</summary>
[<Tests>]
let tests =
    testList "P5.S13" [
        test "format 0 searches the pair array in file order" {
            let kern = kernTableOf (format0Kern 0x01uy [ 1us, 1us, 500s; 2us, 3us, -80s ])
            let subtable = kern.Subtables[0]
            Expect.equal (KernTable.getKerning subtable 1u 1u) 500 "found pair"
            Expect.equal (KernTable.getKerning subtable 2u 3u) -80 "second pair"
            Expect.equal (KernTable.getKerning subtable 1u 2u) 0 "missing pair"
        }
        test "format 2 sums the class byte offsets into the array" {
            let kern = kernTableOf (format2Kern [| 0us; 10us |] [| 20us; 0us |] [| 500s |])
            let subtable = kern.Subtables[0]
            Expect.equal (KernTable.getKerning subtable 1u 0u) 500 "the summed offset"
            Expect.equal (KernTable.getKerning subtable 0u 0u) 0 "offset before the array"
            Expect.equal (KernTable.getKerning subtable 1u 1u) 0 "offset before the array"
        }
        test "format 3 indexes the class values into the kern array" {
            let kern = kernTableOf (format3Kern 3 [| 100s; 200s |] 2 2 [| 0uy; 1uy; 1uy |] [| 0uy; 1uy; 0uy |] [| 0uy; 1uy; 1uy; 0uy |])
            let subtable = kern.Subtables[0]
            Expect.equal (KernTable.getKerning subtable 0u 0u) 100 "class pair (0,0)"
            Expect.equal (KernTable.getKerning subtable 0u 1u) 200 "class pair (0,1)"
            Expect.equal (KernTable.getKerning subtable 1u 0u) 200 "class pair (1,0)"
            Expect.equal (KernTable.getKerning subtable 1u 1u) 100 "class pair (1,1)"
        }
        test "an odd kern value splits into floor and remainder halves" {
            let font = Font.create (kernFace (format0Kern 0x01uy [ 1us, 1us, -41s ]))
            let result = Kern.applyKern (kernPlan ()) font (kernRun [| 1u; 1u |])
            Expect.equal result.Positions[0].XAdvance 79 "left advance 100 - 21"
            Expect.equal result.Positions[1].XAdvance 80 "right advance 100 - 20"
            Expect.equal result.Positions[1].XOffset -20 "right offset -20"
        }
        test "a cross-stream subtable attaches a cursive chain and sets the y offset" {
            let font = Font.create (kernFace (format0Kern 0x05uy [ 1us, 1us, 500s ]))
            let result = Kern.applyKern (kernPlan ()) font (kernRun [| 1u; 1u |])
            Expect.equal result.Positions[0].Var 0x0002FFFFu "left glyph chain"
            Expect.equal result.Positions[1].Var 0x0002FFFFu "right glyph chain"
            Expect.equal result.Positions[1].YOffset 500 "cross-stream y offset"
            Expect.equal result.Positions[0].YOffset 0 "left glyph y offset unchanged"
        }
        test "kerning arrays the caller owns equals kerning shared arrays on every field, for every pair table order and coverage" {
            let rnd = Random 4101
            let failures =
                [ for round in 1 .. 1500 do
                    let pairs =
                        [ for _ in 1 .. rnd.Next(1, 12) -> uint16 (rnd.Next(1, 6)), uint16 (rnd.Next(1, 6)), int16 (rnd.Next(-300, 300)) ]
                    let ordered = match rnd.Next 2 with | 0 -> pairs |> List.distinctBy (fun (left, right, _) -> left, right) |> List.sortBy (fun (left, right, _) -> left, right) | _ -> pairs
                    let coverage = match rnd.Next 3 with | 0 -> 0x05uy | _ -> 0x01uy
                    let font = Font.create (kernFace (format0Kern coverage ordered))
                    let run = randomKernRun rnd
                    let shared = Kern.applyKern (kernPlan ()) font run
                    let owned = Kern.applyKernOwned (kernPlan ()) font (separateCopy run)
                    if not (sameKerned shared owned) then yield round ]
            Expect.equal (failures |> List.truncate 5) [] "the in-place kern equals the copying kern"
        }
        test "a kerned pair on arrays the caller owns is written into those arrays" {
            let font = Font.create (kernFace (format0Kern 0x01uy [ 1us, 1us, 500s ]))
            let run = separateCopy (kernRun [| 1u; 1u; 2u |])
            let result = Kern.applyKernOwned (kernPlan ()) font run
            Expect.equal result.Positions[0].XAdvance 350 "the left advance took half the kerning"
            Expect.isTrue (obj.ReferenceEquals (ImmutableCollectionsMarshal.AsArray run.Positions, ImmutableCollectionsMarshal.AsArray result.Positions)) "the positions are the caller's array"
            Expect.isTrue (obj.ReferenceEquals (ImmutableCollectionsMarshal.AsArray run.Infos, ImmutableCollectionsMarshal.AsArray result.Infos)) "the infos are the caller's array"
            let shared = kernRun [| 1u; 1u; 2u |]
            let copied = Kern.applyKern (kernPlan ()) font shared
            Expect.equal shared.Positions[0].XAdvance 100 "the copying route leaves the input alone"
            Expect.isFalse (obj.ReferenceEquals (ImmutableCollectionsMarshal.AsArray shared.Positions, ImmutableCollectionsMarshal.AsArray copied.Positions)) "the copying route made new positions"
        }
        test "plan decisions follow the kern capabilities" {
            let props = { Direction = Direction.Ltr; Script = Scripts.Latin; Language = NoLanguage }
            let compile caps =
                OtPlan.compile caps props ImmutableArray.Empty OtMap.empty DefaultShaper
            let noKern = compile PlanCapabilities.none
            Expect.isFalse noKern.ApplyKern "no kern table"
            let kernOnly = compile { PlanCapabilities.none with HasKern = true }
            Expect.isTrue kernOnly.ApplyKern "a kern table applies kern"
            Expect.isTrue kernOnly.ZeroMarks "zero marks with plain kern"
            Expect.isTrue kernOnly.AdjustMarkPositioningWhenZeroing "adjust offsets with plain kern"
            let machineKern = compile { PlanCapabilities.none with HasKern = true; HasMachineKern = true }
            Expect.isTrue machineKern.ApplyKern "machine kern applies"
            Expect.isFalse machineKern.ZeroMarks "no zero marks with machine kern"
            let crossKern = compile { PlanCapabilities.none with HasKern = true; HasCrossKern = true }
            Expect.isTrue crossKern.ApplyKern "cross kern applies"
            Expect.isFalse crossKern.AdjustMarkPositioningWhenZeroing "no adjust with cross kern"
        }
    ]
